/// Asynchronous Network Layer for Event-Driven Clients or Servers
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.async;

{
  *****************************************************************************

   Event-Driven Network Classes and Functions
   - Low-Level Non-blocking Connections
   - Client or Server Asynchronous Process
   - THttpAsyncServer Event-Driven HTTP Server
   - THttpProxyServer HTTP Server With Proxy and Cache

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef NO_ASYNC_WINIOCP}
  {$undef USE_WINIOCP}
{$endif NO_ASYNC_WINIOCP}
// you may define NO_ASYNC_WINIOCP conditional to force regular select() instead
// of TWinIocp - but the later seems faster and should scale much better

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.threads,
  mormot.core.search,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.perf,
  mormot.core.zip,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server; // for multi-threaded process


{ ******************** Low-Level Non-blocking Connections }

type
  {$M+}
  TPollAsyncSockets = class;
  {$M-}

  /// define the TPollAsyncSockets.OnRead/AfterWrite method result
  // - soContinue should continue reading/writing content from/to the socket
  // - soDone should unsubscribe for the current read/write phase, but should
  // not shutdown the socket yet
  // - soWaitWrite (for AfterWrite) should wait a little then retry writing
  // - soClose would shutdown the socket
  TPollAsyncSocketOnReadWrite = (
    soContinue,
    soWaitWrite,
    soDone,
    soClose
  );

  /// low-level flags used by the state machine about one TPollAsyncConnection
  // - fWasActive is set by TAsyncConnections.IdleEverySecond to purge rd/wr
  // unused buffers, to avoid calling GetTickCount64 for every activity
  // - fClosed is set by OnClose virtual method
  // - fFirstRead is set once TPollAsyncSockets.OnFirstRead is called
  // - fSubRead/fSubWrite flags are set when Subscribe() has been called
  // - fInList indicates that ConnectionAdd() did register the connection
  // - fReadPending states that there is a pending event for this connection
  // - note: better keep it up to 8 items to fit in a byte (faster access)
  TPollAsyncConnectionFlags = set of (
    fWasActive,
    fClosed,
    fFirstRead,
    {$ifndef USE_WINIOCP}
    fSubRead,
    fSubWrite,
    {$endif USE_WINIOCP}
    fInList,
    fReadPending
  );

  /// abstract parent to store information about one TPollAsyncSockets connection
  TPollAsyncConnection = class(TSynPersistent)
  protected
    /// the associated TCP connection
    // - equals nil after TPollAsyncSockets.Stop
    fSocket: TNetSocket;
    /// the associated 32-bit sequence number
    // - equals 0 after TPollAsyncSockets.Stop
    fHandle: TConnectionAsyncHandle;
    /// low-level 8-bit flags used by the state machine about this connection
    fFlags: TPollAsyncConnectionFlags;
    /// internal 8-bit flags e.g. for fRW[] or IOCP or to mark AddGC()
    fInternalFlags: set of (
      ifWriteWait, ifFromGC, ifInGC, ifSeparateWLock, ifProcessing);
    /// the current (reusable) receiving data buffer of this connection
    fRd: TRawByteStringBuffer;
    /// the current (reusable) sending data buffer of this connection
    fWr: TRawByteStringBuffer;
    /// re-entrant TryLock/Unlock R/W thread acquisition
    // - by default, a single lock is used for all connection access, but
    // ifSeparateWLock may be set to separate Recv() and Send()
    fRWSafe: array[0..1] of TMultiLightLock;
    /// low-level TLS context
    fSecure: INetTls;
    /// the thread currently set by ProcessRead - maybe nil e.g. on write
    fReadThread: TSynThread;
    // how many bytes have been transmitted via Send() and Recv() methods
    fBytesRecv, fBytesSend: Int64;
    {$ifdef USE_WINIOCP}
    // opaque Windows IOCP instances returned by TWinIocp.Subscribe()
    fIocpSub: PWinIocpSubscription; // from the main fIocpRecvSend IOCP queue
    function IocpPrepareNextWrite(queue: TWinIocp): boolean;
    {$endif USE_WINIOCP}
    /// called when the instance is connected to a poll, after Create or Recycle
    // - i.e. at the end of TAsyncConnections.ConnectionNew(), when Handle is set
    // - overriding this method is cheaper than its plain Create constructor
    // - default implementation does nothing
    procedure AfterCreate; virtual;
    /// called when the instance is about to be deleted from a poll
    // - overriding this method is cheaper than the plain Destroy destructor
    // - default implementation does nothing
    procedure BeforeDestroy; virtual;
    /// called when fFirstRead flag is set, i.e. once just after connection
    // - should return true on success, or false to close the connection
    // - this default implementation will just call aOwner.fOnFirstRead()
    // and return false on any exception (typically a TLS error)
    function OnFirstRead(aOwner: TPollAsyncSockets): boolean; virtual;
    /// called just before ProcessRead/OnRead are done
    // - is overriden e.g. in THttpAsyncConnection to wait for background Write
    procedure BeforeProcessRead; virtual;
    /// this method is called when the some input data is pending on the socket
    // - should extract frames or requests from Connection.rd, and handle them
    // - this is where the input should be parsed and extracted according to
    // the implemented procotol; Connection.rd could be kept as temporary
    // buffer during the parsing, and rd.Reset called once processed
    // - Sender.Write() could be used for asynchronous answer sending
    // - Sender.LogVerbose() allows logging of escaped data
    // - could return sorClose to shutdown the socket, e.g. on parsing error
    function OnRead: TPollAsyncSocketOnReadWrite; virtual; abstract;
    /// called by TPollAsyncSockets.SubscribeConnection([pseWrite]
    // just after fWrite.Subscribe()
    procedure OnAfterWriteSubscribe; virtual;
    /// this method is called when some data has been written to the socket
    // - default implementation will do nothing - see e.g. TRtspConnection
    // - you may send data asynchronously using Connection.wr.Append()
    function AfterWrite: TPollAsyncSocketOnReadWrite; virtual;
    /// this method is called when the sockets is closing
    // - overriden e.g. by TWebSocketAsyncConnection to notify the closing
    procedure OnClose; virtual;
    /// called by ReleaseMemoryOnIdle within the read lock: clean fRd here
    function ReleaseReadMemoryOnIdle: PtrInt; virtual;
    function ReleaseWriteMemoryOnIdle: PtrInt; virtual;
  public
    /// inherited classes should never call it, but reintroduce their own Create
    // and override AfterCreate if needed
    constructor Create; override;
    /// finalize the instance
    destructor Destroy; override;
    /// quick check if this instance seems still active, i.e. its Handle <> 0
    function IsDangling: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// quick check if this instance is still open
    function IsClosed: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// acquire an exclusive R/W access to this connection
    // - returns true if connection has been acquired, setting the wasactive flag
    // - returns false if it is used by another thread
    function TryLock(writer: boolean): boolean;
    /// try to acquire an exclusive R/W access to this connection
    // - returns true if connection has been acquired
    // - returns false if it is used by another thread, after the timeoutMS period
    // - only with writer=true after a locked read + process, so unlikely to sleep
    function WaitLock(writer: boolean; timeoutMS: cardinal): boolean;
    /// release exclusive R/W access to this connection
    procedure UnLock(writer: boolean);
      {$ifdef HASINLINE} inline; {$endif}
    /// called after TAsyncConnections.LastOperationReleaseMemorySeconds
    function ReleaseMemoryOnIdle: PtrInt;
    /// send some buffer to the connection, using TLS if possible
    // - you should have acquired the instance lock via TryLock/WaitLock(true)
    function Send(buf: pointer; var len: integer): TNetResult;
    /// receive some buffer from the connection, using TLS if possible
    // - you should have acquired the instance lock via TryLock/WaitLock(false)
    function Recv(buf: pointer; var len: integer): TNetResult;
    /// read-only access to the socket number associated with this connection
    property Socket: TNetSocket
      read fSocket;
    /// read-only access to the low-level TLS context
    property Secure: INetTls
      read fSecure;
  published
    /// read-only access to the handle number associated with this connection
    property Handle: TConnectionAsyncHandle
      read fHandle;
  end;

  /// thread-safe storage of several connections
  // - use e.g. by TPollAsyncSockets.ProcessWaitingWrite or to implement
  // generational garbage collector of TAsyncConnection instances
  TPollAsyncConnections = record
    Safe: TLightLock;
    Count: integer; // should be integer, not PtrInt
    Items: array of TPollAsyncConnection;
  end;
  PPollAsyncConnections = ^TPollAsyncConnections;

  /// possible options for low-level TPollAsyncSockets process
  // - as translated from homonymous high-level acoWritePollOnly
  // TAsyncConnectionsOptions item
  TPollAsyncSocketsOptions = set of (
    paoWritePollOnly
  );

  /// callback prototype for TPollAsyncSockets.OnStart events
  // - should return true if Start() should not subscribe for this connection
  TOnPollAsyncFunc = function(Sender: TPollAsyncConnection): boolean of object;

  /// callback prototype for TPollAsyncSockets.OnStop events
  TOnPollAsyncProc = procedure(Sender: TPollAsyncConnection) of object;

  {$ifndef USE_WINIOCP}
  TPollReadSockets = class(TPollSockets)
  protected
    function EnsurePending(tag: TPollSocketTag): boolean; override;
    procedure SetPending(tag: TPollSocketTag); override;
    function UnsetPending(tag: TPollSocketTag): boolean; override;
  end;
  TPollWriteSockets = TPollSockets;
  {$endif USE_WINIOCP}

  {$M+}
  /// read/write buffer-oriented process of multiple non-blocking connections
  // - to be used e.g. for stream protocols (e.g. WebSockets or IoT communication)
  // - assigned sockets will be set in non-blocking mode, so that polling will
  // work as expected: you should then never use direclty the socket (e.g. via
  // blocking TCrtSocket), but rely on this class for asynchronous process:
  // TPollAsyncConnection.OnRead() overriden method will receive all incoming
  // data from input buffer, and Write() should be called to add send some data,
  // potentially asynchronous with an internal buffer
  // - ProcessRead/ProcessWrite methods are to be run for actual communication:
  // either you call those methods from multiple threads, or you run them in
  // loop from a single thread, then define a TSynThreadPool for running any
  // blocking process (e.g. computing requests answers) from OnRead callbacks
  TPollAsyncSockets = class
  protected
    {$ifdef USE_WINIOCP}
    fIocpRecvSend: TWinIocp; // process both wieRecv and wieSend notifications
    {$else}
    fRead: TPollReadSockets;
    fWrite: TPollWriteSockets; // separated fWrite
    {$endif USE_WINIOCP}
    fProcessingRead, fProcessingWrite: integer;
    fSendBufferSize: integer; // retrieved at first connection Start()
    fReadCount: Int64;
    fWriteCount: Int64;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fDebugLog: TSynLogClass;
    fOptions: TPollAsyncSocketsOptions;
    fTerminated: boolean;
    fReadWaitMs: integer;
    fOnStart: TOnPollAsyncFunc;
    fOnFirstRead, fOnStop: TOnPollAsyncProc;
    fWaitingWrite: TPollAsyncConnections; // to implement soWaitWrite
    function GetCount: integer;
    procedure DoLog(TextFmt: PUtf8Char; const TextArgs: array of const;
      Level: TSynLogLevel = sllTrace);
    // pseError: return false to close socket and connection
    function OnError(connection: TPollAsyncConnection;
      events: TPollSocketEvents): boolean; virtual; abstract;
    procedure OnClosed(connection: TPollAsyncConnection); virtual; abstract;
    procedure RegisterConnection(connection: TPollAsyncConnection); virtual; abstract;
    function SubscribeConnection(const caller: ShortString;
      connection: TPollAsyncConnection; sub: TPollSocketEvent): boolean;
    procedure CloseConnection(var connection: TPollAsyncConnection;
      const caller: ShortString); // set connection:=nil and close+GC
    function RawWrite(connection: TPollAsyncConnection;
      var data: PByte; var datalen: integer): boolean;
    function DoAfterWrite(const caller: ShortString;
      connection: TPollAsyncConnection): TPollAsyncSocketOnReadWrite;
    procedure ProcessWaitingWrite; // pending soWaitWrite
  public
    /// initialize the read/write sockets polling
    // - fRead and fWrite TPollSocketsBuffer instances will track pseRead or
    // pseWrite events, and maintain input and output data buffers
    constructor Create(aOptions: TPollAsyncSocketsOptions; aThreadCount: integer); virtual;
    /// finalize buffer-oriented sockets polling, and release all used memory
    destructor Destroy; override;
    /// assign a new connection to the internal reading poll
    // - the TSocket handle will be set in non-blocking mode from now on - it
    // is not recommended to access it directly any more, but use Write() and
    // handle OnRead() callback
    // - fRead will poll incoming packets, then call OnRead to handle them,
    // or Unsubscribe and delete the socket when pseClosed is notified
    // - fWrite will poll for outgoing packets as specified by Write(), then
    // send any pending data once the socket is ready
    // - any manual call of Start() should ensure the connection is non-blocking
    function Start(connection: TPollAsyncConnection): boolean; virtual;
    /// remove a connection from the internal poll, and shutdown its socket
    // - most of the time, the connection is released by OnClosed when the other
    // end shutdown the socket; but you can explicitly call this method when
    // the connection (and its socket) is to be shutdown
    // - this method won't call OnClosed, since it is initiated by the class
    function Stop(connection: TPollAsyncConnection;
      const caller: ShortString): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    // - this method may block if the connection is currently writing from
    // another thread (which is not possible from TPollAsyncSockets.Write),
    // up to timeout milliseconds
    function Write(connection: TPollAsyncConnection;
      data: pointer; datalen: integer; timeout: integer = 5000): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    function WriteString(connection: TPollAsyncConnection;
      const data: RawByteString; timeout: integer = 5000): boolean;
    /// one or several threads should execute this method
    // - thread-safe handle of any notified incoming packet
    // - return true if something has been read or closed, false to retry later
    function ProcessRead(Sender: TSynThread;
      const notif: TPollSocketResult): boolean;
    /// one thread should execute this method with the proper pseWrite notif
    // - thread-safe handle of any outgoing packets
    // - sent  is the number of bytes already sent from connection.fWr buffer,
    // e.g. via TWinIocp.PrepareNext(wieSend)
    procedure ProcessWrite(const notif: TPollSocketResult; sent: integer);
    /// notify internal socket polls to stop their polling loop ASAP
    procedure Terminate(waitforMS: integer);
    /// some processing options
    property Options: TPollAsyncSocketsOptions
      read fOptions write fOptions;
    /// event called on Start() method success
    // - warning: this callback should be very quick because it is blocking
    property OnStart: TOnPollAsyncFunc
      read fOnStart write fOnStart;
    /// event called on first ProcessRead() on a given connection
    // - is assigned e.g. to TAsyncServer.OnFirstReadDoTls to setup the TLS
    // in one sub-thread of the thread pool
    property OnFirstRead: TOnPollAsyncProc
      read fOnFirstRead write fOnFirstRead;
    /// event called on Stop() method success
    // - warning: this callback should be very quick because it is blocking
    property OnStop: TOnPollAsyncProc
      read fOnStop write fOnStop;
  published
    /// how many connections are currently managed by this instance
    property Count: integer
      read GetCount;
    /// how many times data has been received by this instance
    property ReadCount: Int64
      read fReadCount;
    /// how many times data has been sent by this instance
    property WriteCount: Int64
      read fWriteCount;
    /// how many data bytes have been received by this instance
    property ReadBytes: Int64
      read fReadBytes;
    /// how many data bytes have been sent by this instance
    property WriteBytes: Int64
      read fWriteBytes;
    // enable WaitFor() during recv() in ProcessRead
    // - may enhance responsiveness especially on HTTP/1.0 connections
    // - equals 0 ms by default, but could be tuned e.g. to 50 or 100 if needed
    // - use with care: performance degrades with highly concurrent HTTP/1.1
    property ReadWaitMs: integer
      read fReadWaitMs write fReadWaitMs;
    {$ifdef USE_WINIOCP}
    /// low-level access to the IOCP polling class used for all events
    property IocpRecvSend: TWinIocp
      read fIocpRecvSend;
    {$else}
    /// low-level access to the polling class used for recv() data
    property PollRead: TPollReadSockets
      read fRead;
    /// low-level access to the polling class used for send() data
    property PollWrite: TPollWriteSockets
      write fWrite;
    {$endif USE_WINIOCP}
  end;

  {$M-}

function ToText(so: TPollAsyncSocketOnReadWrite): PShortString; overload;



{ ******************** Client or Server Asynchronous Process }

type
  /// exception associated with TAsyncConnection / TAsyncConnections process
  EAsyncConnections = class(ESynException);

  TAsyncConnections = class;

  /// 32-bit type used to store GetTickCount64 div 1000 values
  // - as used e.g. by TAsyncConnection.fLastOperation
  TAsyncConnectionSec = type cardinal;

  /// abstract class to store one TAsyncConnections connection
  // - may implement e.g. WebSockets frames, or IoT binary protocol
  // - each connection will be identified by a TConnectionAsyncHandle integer
  // - idea is to minimize the resources used per connection, and allow full
  // customization of the process by overriding the OnRead virtual method (and,
  // if needed, AfterCreate/AfterWrite/BeforeDestroy/OnLastOperationIdle)
  TAsyncConnection = class(TPollAsyncConnection)
  protected
    fLastOperation: TAsyncConnectionSec; // as 32-bit monotonic seconds
    fRemoteIP4: cardinal; // may contain cLocalhost32 = 127.0.0.1
    fRemoteIP: RawUtf8;   // never contains '127.0.0.1'
    fOwner: TAsyncConnections;
    // called after TAsyncConnections.LastOperationIdleSeconds of no activity
    // - Sender.Write() could be used to send e.g. a hearbeat frame
    // - should finish quickly and be non-blocking
    // - returns true to log notified events, false if nothing happened
    function OnLastOperationIdle(nowsec: TAsyncConnectionSec): boolean; virtual;
  public
    /// initialize this instance
    constructor Create(aOwner: TAsyncConnections;
      const aRemoteIP: TNetAddr); reintroduce; virtual;
    /// reuse this instance for a new incoming connection
    procedure Recycle(const aRemoteIP: TNetAddr); virtual;
    /// read-only access to the associated connections list
    property Owner: TAsyncConnections
      read fOwner;
  published
    /// the associated remote IPv4/IPv6, as text
    property RemoteIP: RawUtf8
      read fRemoteIP;
  end;
  PAsyncConnection = ^TAsyncConnection;

  /// meta-class of one TAsyncConnections connection
  TAsyncConnectionClass = class of TAsyncConnection;

  /// used to store a dynamic array of TAsyncConnection
  TAsyncConnectionDynArray = array of TAsyncConnection;

  /// handle multiple non-blocking connections using TAsyncConnection instances
  TAsyncConnectionsSockets = class(TPollAsyncSockets)
  protected
    fOwner: TAsyncConnections;
    function GetTotal: integer;
      {$ifdef HASINLINE} inline; {$endif}
    procedure RegisterConnection(connection: TPollAsyncConnection); override;
    // just log the error, and close connection if acoOnErrorContinue is not set
    function OnError(connection: TPollAsyncConnection;
      events: TPollSocketEvents): boolean; override;
    procedure OnClosed(connection: TPollAsyncConnection); override;
  public
    /// add some data to the asynchronous output buffer of a given connection
    // - this overriden method will also log the write operation if needed
    // - can be executed from an TAsyncConnection.OnRead method
    function Write(connection: TPollAsyncConnection;
      data: pointer; datalen: integer; timeout: integer = 5000): boolean; override;
  published
    /// how many connections have been handled by the poll, from the beginning
    property Total: integer
      read GetTotal;
  end;

  {$ifdef USE_WINIOCP}
  /// TAsyncConnectionsThread.Execute will directly call TWinIocp.GetNext()
  TAsyncConnectionsThreadProcess = (
    atpReadPending);
  {$else}
  /// define what TAsyncConnectionsThread.Execute should actually do
  TAsyncConnectionsThreadProcess = (
    atpReadSingle,
    atpReadPoll,
    atpReadPending
  );
  TAsyncConnectionsThreadProcesses = set of TAsyncConnectionsThreadProcess;
  {$endif USE_WINIOCP}

  /// used to implement a thread poll to process TAsyncConnection instances
  TAsyncConnectionsThread = class(TLoggedThread)
  protected
    fOwner: TAsyncConnections;
    fProcess: TAsyncConnectionsThreadProcess;
    fWaitForReadPending: boolean;
    fWakeUpFromSlowProcess: boolean;
    fExecuteState: THttpServerExecuteState;
    fIndex: integer;
    fCustomObject: TObject;
    {$ifndef USE_WINIOCP}
    fEvent: TSynEvent;
    fThreadPollingLastWakeUpTix: integer;
    fThreadPollingLastWakeUpCount: integer;
    function GetNextRead(out notif: TPollSocketResult): boolean;
    procedure ReleaseEvent; {$ifdef HASINLINE} inline; {$endif}
    {$endif USE_WINIOCP}
    procedure DoExecute; override;
  public
    /// initialize the thread
    constructor Create(aOwner: TAsyncConnections;
      aProcess: TAsyncConnectionsThreadProcess; aIndex: integer); reintroduce;
    /// finalize the thread resources
    destructor Destroy; override;
    /// a TObject instance which will be owned by this thread once assigned
    // - Destroy will delete it when needed
    // - could be used to maintain some thread-speficic resource, e.g. a raw 
    // DB connection or a (set of) COM object(s)
    property CustomObject: TObject
      read fCustomObject write fCustomObject;
  published
    /// which kind of ProcessRead or ProcessWrite this thread is doing
    property Process: TAsyncConnectionsThreadProcess
      read fProcess;
    /// when used as a thread pool, the number of this thread
    property Index: integer
      read fIndex;
  end;
  PAsyncConnectionsThread = ^TAsyncConnectionsThread;

  /// low-level options for TAsyncConnections processing
  // - TAsyncConnectionsSockets.OnError will shutdown the connection on any error,
  // unless acoOnErrorContinue is defined
  // - acoNoLogRead and acoNoLogWrite could reduce the log verbosity
  // - acoVerboseLog will log transmitted frames content, for debugging purposes
  // - acoWritePollOnly will be translated into paoWritePollOnly on server
  // - acoDebugReadWriteLog would make low-level send/receive logging
  // - acoNoConnectionTrack would force to by-pass the internal Connections list
  // if it is not needed - not used by now
  // - acoEnableTls flag for TLS support, via Windows SChannel or OpenSSL 1.1/3.x
  // - either acoThreadCpuAffinity or acoThreadSocketAffinity could be set: the
  // first for thread affinity to one CPU logic core, the 2nd for affinity to
  // all logical cores of each CPU HW socket (both exclusive)
  // - acoReusePort will set SO_REUSEPORT on POSIX, allowing to bind several
  // TAsyncConnections on the same port, either within the same process, or as
  // separated processes (e.g. to set process affinity to one CPU HW socket)
  // - acoThreadSmooting will change the ThreadPollingWakeup() algorithm to
  // focus the process on the first threads of the pool - by design, this
  // setting will disable both acoThreadCpuAffinity and acoThreadSocketAffinity
  TAsyncConnectionsOptions = set of (
    acoOnErrorContinue,
    acoNoLogRead,
    acoNoLogWrite,
    acoVerboseLog,
    acoWritePollOnly,
    acoDebugReadWriteLog,
    acoNoConnectionTrack,
    acoEnableTls,
    acoThreadCpuAffinity,
    acoThreadSocketAffinity,
    acoReusePort,
    acoThreadSmooting
  );

  /// dynamic array of TAsyncConnectionsThread instances
  TAsyncConnectionsThreads = array of TAsyncConnectionsThread;

  /// implements an abstract thread-pooled high-performance TCP clients or server
  // - internal TAsyncConnectionsSockets will handle high-performance process
  // of a high number of long-living simultaneous connections
  // - will use a TAsyncConnection inherited class to maintain connection state
  // - don't use this abstract class but either TAsyncServer or TAsyncClient
  // - under Linux/POSIX, check your "ulimit -H -n" value: one socket consumes
  // two file descriptors: you may better add the following line to your
  // /etc/limits.conf or /etc/security/limits.conf system file:
  // $ * hard nofile 65535
  TAsyncConnections = class(TLoggedThread)
  protected
    fConnectionClass: TAsyncConnectionClass;
    fConnection: TAsyncConnectionDynArray; // sorted by TAsyncConnection.Handle
    fSockets: TAsyncConnectionsSockets;
    fThreads: TAsyncConnectionsThreads;
    fConnectionLock: TRWLock; // write lock/block only on connection add/remove
    fConnectionCount: integer; // only subscribed - not just after accept()
    fConnectionHigh: integer;
    fThreadPoolCount: integer;
    fLastConnectionFind: integer;
    fThreadPollingWakeupSafe: TLightLock; // topmost to ensure aarch64 alignment
    fLastHandle: integer;
    fOptions: TAsyncConnectionsOptions;
    fLastOperationSec: TAsyncConnectionSec;
    fLastOperationReleaseMemorySeconds: cardinal;
    fLastOperationIdleSeconds: cardinal;
    fKeepConnectionInstanceMS: cardinal;
    fLastOperationMS: Int64; // = GetTickCount64 as set by ProcessIdleTix()
    {$ifdef USE_WINIOCP}
    // in IOCP mode, Execute does wieSend (and wieAccept for TAsyncServer)
    fIocpAccept: TWinIocp; // wieAccept in their own IOCP queue
    fIocpAcceptSub: PWinIocpSubscription;
    {$else}
    fThreadReadPoll: TAsyncConnectionsThread;
    fThreadPollingWakeupLoad: integer;
    fThreadPollingLastWakeUpTix: integer;
    fThreadPollingAwakeCount: integer;
    fSocketsEpoll: boolean; // = PollSocketClass.FollowEpoll
    {$endif USE_WINIOCP}
    /// implement generational garbage collector of TAsyncConnection instances
    // - we define two generations: GC #1 has a TTL of KeepConnectionInstanceMS
    // (100ms) and are used to avoid GPF or confusion on still active connections;
    // GC #2 has a TTL of 10 seconds and will be used by ConnectionCreate to
    // recycle e.g. THttpAsyncConnection instances between HTTP/1.0 calls
    fGC1, fGC2: TPollAsyncConnections;
    fGCLast, fGCTix1, fGCTix2: integer;
    fOnIdle: array of TOnPollSocketsIdle;
    fThreadClients: record // used by TAsyncClient
      Count, Timeout: integer;
      Address, Port: RawUtf8;
    end;
    function AllThreadsStarted: boolean; virtual;
    procedure AddGC(aConnection: TPollAsyncConnection; const aContext: ShortString);
    procedure DoGC;
    procedure FreeGC(var conn: TPollAsyncConnections);
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: TNetAddr;
      out aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionNew(aSocket: TNetSocket; aConnection: TAsyncConnection;
      aAddAndSubscribe: boolean = true): boolean; virtual;
    function ConnectionDelete(
      aConnection: TPollAsyncConnection): boolean; overload; virtual;
    function LockedConnectionDelete(
      aConnection: TAsyncConnection; aIndex: integer): boolean;
    procedure ConnectionAdd(conn: TAsyncConnection);
    procedure DoLog(Level: TSynLogLevel; TextFmt: PUtf8Char;
      const TextArgs: array of const; Instance: TObject);
    procedure ProcessIdleTix(Sender: TObject; NowTix: Int64); virtual;
    function ProcessClientStart(Sender: TPollAsyncConnection): boolean;
    procedure IdleEverySecond; virtual;
    {$ifndef USE_WINIOCP}
    function ThreadPollingWakeup(Events: integer): PtrInt;
    {$endif USE_WINIOCP}
  public
    /// initialize the multiple connections
    // - don't use this constructor but inherited client/server classes
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the instance, releasing all associated threads and sockets
    procedure Shutdown; virtual;
    /// shut down and finalize the instance, calling Shutdown
    destructor Destroy; override;
    /// ensure all threads of the pool is bound to a given CPU core
    // - may lower performance, but reduce global consumption
    procedure SetCpuAffinity(CpuIndex: integer);
    /// ensure all threads of the pool is bound to a given CPU HW socket
    // - may enhance performance on multi-socket systems
    procedure SetSocketAffinity(SocketIndex: integer);
    /// add or remove a callback run from ProcessIdleTix() internal method
    // - all callbacks will be triggered once with Sender=nil at shutdown
    procedure SetOnIdle(const aOnIdle: TOnPollSocketsIdle; Remove: boolean = false);
    /// high-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    // - raise an exception if acoNoConnectionTrack option was defined
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the main lock as:
    // ! try ... finally UnLock(aLock); end;
    function ConnectionFindAndLock(aHandle: TConnectionAsyncHandle;
      aLock: TRWLockContext; aIndex: PInteger = nil): TAsyncConnection;
    /// high-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search
    // - this method won't keep the main Lock, but this class will ensure that
    // the returned pointer will last for at least 100ms until Free is called
    function ConnectionFind(Handle: TConnectionAsyncHandle): TAsyncConnection;
    /// high-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search of a TAsyncConnection instance
    // - will also thread-safely attempt to acquire one of the connection's lock
    // - returns nil if the handle was not found and acquired within WaitTimeoutMS
    function ConnectionFindAndWaitLock(Handle: TConnectionAsyncHandle;
      LockWriter: boolean; WaitTimeoutMS: cardinal): pointer;
    /// low-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search, since handles are increasing
    // - caller should have called Lock before this method is done
    function LockedConnectionSearch(Handle: TConnectionAsyncHandle): TAsyncConnection;
    /// remove an handle from the internal list, and close its connection
    // - raise an exception if acoNoConnectionTrack option was defined
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function ConnectionRemove(Handle: TConnectionAsyncHandle): boolean;
    /// call ConnectionRemove unless acoNoConnectionTrack is set
    procedure EndConnection(connection: TAsyncConnection);
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function Write(connection: TAsyncConnection; data: pointer; datalen: integer;
      timeout: integer = 5000): boolean;
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function WriteString(connection: TAsyncConnection; const data: RawByteString;
      timeout: integer = 5000): boolean;
    /// low-level method to connect a client to this server
    // - is called e.g. from fThreadClients
    function ThreadClientsConnect: TAsyncConnection;
    /// log some binary data with proper escape as sllTrace
    // - can be executed e.g. from an TAsyncConnection.OnRead method as such:
    // $ if acoVerboseLog in fOwner.Options then
    // $   fOwner.LogVerbose(self, ..., fRd);
    procedure LogVerbose(connection: TPollAsyncConnection; const ident: RawUtf8;
      const identargs: array of const; const data: TRawByteStringBuffer);
    /// the current monotonic time elapsed, evaluated in seconds
    // - GetTickSecs value set at most every 500ms by ProcessIdleTix()
    property LastOperationSec: TAsyncConnectionSec
      read fLastOperationSec;
    /// the current monotonic time elapsed, evaluated in milliseconds
    // - GetTickCount64 value set at most every 500ms by ProcessIdleTix()
    property LastOperationMS: Int64
      read fLastOperationMS;
    /// allow idle connection to release its internal Connection.rd/wr buffers
    // - default is 60 seconds, which is pretty conservative
    // - could be tuned in case of high numbers of concurrent connections and
    // constrained memory, e.g. with a lower value like 2 seconds
    property LastOperationReleaseMemorySeconds: cardinal
      read fLastOperationReleaseMemorySeconds write fLastOperationReleaseMemorySeconds;
    /// will execute TAsyncConnection.OnLastOperationIdle after an idle period
    // - could be used to send heartbeats after read/write inactivity
    // - equals 0 (i.e. disabled) by default
    property LastOperationIdleSeconds: cardinal
      read fLastOperationIdleSeconds write fLastOperationIdleSeconds;
    /// how many milliseconds a TAsyncConnection instance is kept alive after closing
    // - default is 100 ms before the internal GC calls Free on this instance
    property KeepConnectionInstanceMS: cardinal
      read fKeepConnectionInstanceMS write fKeepConnectionInstanceMS;
    /// allow to customize low-level options for processing
    property Options: TAsyncConnectionsOptions
      read fOptions write fOptions;
    {$ifndef USE_WINIOCP}
    // how many events a fast active thread is supposed to handle in its loop
    // for the acoThreadSmooting option in ThreadPollingWakeup()
    // - will wake up the threads only if the previous seem to be somewhat idle
    // - default value is (ThreadPoolCount/CpuCount)*8, with a minimum of 4
    property ThreadPollingWakeupLoad: integer
      read fThreadPollingWakeupLoad write fThreadPollingWakeupLoad;
    {$endif USE_WINIOCP}
    /// low-level unsafe direct access to the connection instances
    // - ensure this property is used in a thread-safe manner, i.e. calling
    // ConnectionFindAndLock() high-level function, ot via manual
    // ! ConnectionLock.ReadOnlyLock;
    // ! try ... finally ConnectionLock.ReadOnlyUnLock; end;
    property Connection: TAsyncConnectionDynArray
      read fConnection;
    /// access to the R/W lock protecting the Connection[] array
    // - will WriteLock/block only on connection add/remove
    property ConnectionLock: TRWLock
      read fConnectionLock;
    /// direct access to the class instantiated for each connection
    // - as supplied to the constructor, but may be overriden just after startup
    property ConnectionClass: TAsyncConnectionClass
      read fConnectionClass write fConnectionClass;
    /// direct access to the internal AsyncConnectionsThread's
    property Threads: TAsyncConnectionsThreads
      read fThreads;
  published
    /// how many read threads there are in this thread pool
    property ThreadPoolCount: integer
      read fThreadPoolCount;
    /// current HTTP/1.1 / WebSockets connections count
    // - this is the number of long-living connections - may not appear just
    // after accept, so never for a HTTP/1.0 short-living request
    property ConnectionCount: integer
      read fConnectionCount;
    /// maximum number of concurrent long-living connections since started
    property ConnectionHigh: integer
      read fConnectionHigh;
    /// access to the TCP client sockets poll
    // - TAsyncConnection.OnRead should rather use Write() and LogVerbose()
    // methods of this TAsyncConnections class instead of using Clients
    property Sockets: TAsyncConnectionsSockets
      read fSockets;
  end;

  /// implements a thread-pooled high-performance TCP server
  // - will use a TAsyncConnection inherited class to maintain connection
  // state for server process
  TAsyncServer = class(TAsyncConnections)
  protected
    fServer: TCrtSocket; // for proper complex binding (including TLS)
    fMaxPending: integer;
    fMaxConnections: integer;
    fAccepted: Int64;
    fExecuteState: THttpServerExecuteState;
    fExecuteAcceptOnly: boolean; // W in other thread (POSIX THttpAsyncServer)
    fExecuteMessage: RawUtf8;
    fSockPort: RawUtf8;
    fBanned: THttpAcceptBan; // for hsoBan40xIP or BlackList
    procedure OnFirstReadDoTls(Sender: TPollAsyncConnection);
    procedure SetExecuteState(State: THttpServerExecuteState); virtual;
    procedure DoExecute; override;
  public
    /// run the TCP server, listening on a supplied IP port
    // - aThreadPoolCount = 1 is fine if the process is almost non-blocking,
    // like our mormot.net.rtsphttp relay - but not e.g. for a REST/SOA server
    // - with aThreadPoolCount > 1, a thread will do atpReadPoll, and all other
    // threads will do atpReadPending for socket reading and processing the data
    // - there will always be two other threads, one for Accept() and another
    // for asynchronous data writing (i.e. sending to the socket)
    // - warning: should call WaitStarted() to let Execute bind and run
    // - for TLS support, set acoEnableTls, and once WaitStarted() returned,
    // set Server.TLS.CertificateFile/PrivateKeyFile/PrivatePassword properties
    // and call Server.DoTlsAfter(cstaBind)
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// to be called just after Create to wait for Execute to Bind
    // - will raise an exception on timeout, or if the binding failed
    // - needed only for raw protocol implementation: THttpServerGeneric will
    // have its own WaitStarted method
    procedure WaitStarted(seconds: integer);
    /// prepare the server finalization
    procedure Shutdown; override;
    /// shut down the server, releasing all associated threads and sockets
    destructor Destroy; override;
  published
    /// access to the TCP server socket
    property Server: TCrtSocket
      read fServer;
    /// how many connections have been accepted since server startup
    // - ConnectionCount is the number of long-living connections, this
    // counter is the absolute number of successful accept() calls,
    // including short-living (e.g. HTTP/1.0) connections
    property Accepted: Int64
      read fAccepted;
    /// above how many active connections accept() would reject
    // - MaxPending applies to the actual thread-pool processing activity,
    // whereas MaxConnections tracks the number of connections even in idle state
    property MaxConnections: integer
      read fMaxConnections write fMaxConnections;
    /// above how many fSockets.fRead.PendingCount accept() would reject
    // - is mapped by the high-level THttpAsyncServer.HttpQueueLength property
    // - default is 10000, but could be a lower value e.g. for a load-balancer
    // - MaxConnections regulates the absolute number of (idle) connections,
    // whereas this property tracks the actual REST/HTTP requests pending for
    // the internal thread pool
    property MaxPending: integer
      read fMaxPending write fMaxPending;
  end;

  /// implements thread-pooled high-performance TCP multiple clients
  // - e.g. to run some load stress tests with optimized resource use
  // - will use a TAsyncConnection inherited class to maintain connection state
  // of each connected client
  TAsyncClient = class(TAsyncConnections)
  protected
    procedure DoExecute; override;
  public
    /// start the TCP client connections, connecting to the supplied IP server
    constructor Create(const aServer, aPort: RawUtf8;
      aClientsCount, aClientsTimeoutSecs: integer;
      const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer = 1); reintroduce; virtual;
  published
    /// server IP address
    property Server: RawUtf8
      read fThreadClients.Address;
    /// server IP port
    property Port: RawUtf8
      read fThreadClients.Port;
  end;


const
  /// the TAsyncConnectionsOptions for THttpAsyncServer running on production
  // - with low verbosity of the logs - similar to a plain THttpServer
  ASYNC_OPTION_PROD = [
    acoNoLogRead,
    acoNoLogWrite];

  /// the TAsyncConnectionsOptions for debugging THttpAsyncServer
  // - with high-level receive/send block information
  ASYNC_OPTION_DEBUG = [
    ];

  /// the TAsyncConnectionsOptions for fully detailed debug of THttpAsyncServer
  // - with all possible - and very verbose - log information
  // - could be used to track performance or heisenbug issues
  ASYNC_OPTION_VERBOSE = [
    acoVerboseLog,
    acoDebugReadWriteLog];


{ ******************** THttpAsyncServer Event-Driven HTTP Server }

type
  /// exception associated with Event-Driven HTTP Server process
  EHttpAsyncConnections = class(EAsyncConnections);

  THttpAsyncClientConnection = class;
  THttpAsyncServer = class;
  THttpAsyncConnections = class;

  /// abstract HTTP server or client connection to our non-blocking THttpAsyncServer
  THttpAsyncConnection = class(TAsyncConnection)
  protected
    fHttp: THttpRequestContext; // non-blocking HTTP state machine
    fServer: THttpAsyncServer;
    function ReleaseReadMemoryOnIdle: PtrInt; override;
    procedure OnAfterWriteSubscribe; override;
  public
    /// low-level access to the associated HTTP async server instance
    property Server: THttpAsyncServer
      read fServer;
  end;

  /// define the TOnHttpClientAsync callback state machine steps
  // - hcsBeforeTlsHandshake allows to change connection.Tls parameters
  // - hcsAfterTlsHandshake can validate the connection.Tls information
  // - hcsBeforeSendHeaders allows to change emitted connection.Http.Head
  // - hcsAfterSendHeaders is called just before read subscription
  // - hcsReadStateChanged is called by connection.OnRead when Http.State changed
  // - hcsHeadersReceived is called when response has set connection.ResponseStatus
  // and connection.Http.Headers have been set
  // - hcsFinished is called when a response was received, maybe with a body
  // - hcsFailed is set on eventual error
  TOnHttpClientState = (
    hcsBeforeTlsHandshake,
    hcsAfterTlsHandshake,
    hcsBeforeSendHeaders,
    hcsAfterSendHeaders,
    hcsReadStateChanged,
    hcsHeadersReceived,
    hcsFinished,
    hcsFailed);
  /// define when the TOnHttpClientAsync callback is to be executed
  TOnHttpClientStates = set of TOnHttpClientState;

  /// callback used e.g. by THttpAsyncClientConnection.OnStateChanged
  // - should return soContinue on success, or anything else to abort/close
  // - eventually hrsResponseDone or one hrsError* will mark the end of process
  TOnHttpClientAsync = function(state: TOnHttpClientState;
    connection: THttpAsyncClientConnection): TPollAsyncSocketOnReadWrite of object;

  /// handle one HTTP client connection handled by our non-blocking THttpAsyncServer
  // - used e.g. for efficient reverse proxy support with another server
  THttpAsyncClientConnection = class(THttpAsyncConnection)
  protected
    fOnStateChanged: TOnHttpClientAsync;
    fResponseStatus: integer;
    fOnStateChange: TOnHttpClientStates;
    fTls: TNetTlsContext;
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    function NotifyStateChange(state: TOnHttpClientState): TPollAsyncSocketOnReadWrite;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// access to the associated progress event callback
    property OnStateChanged: TOnHttpClientAsync
      read fOnStateChanged;
    /// server response HTTP status code (e.g. 200)
    property ResponseStatus: integer
      read fResponseStatus;
    /// associated TLS options and informations
    property Tls: TNetTlsContext
      read fTls write fTls;
  end;

  /// handle one HTTP server connection to our non-blocking THttpAsyncServer
  THttpAsyncServerConnection = class(THttpAsyncConnection)
  protected
    fKeepAliveMaxSec: TAsyncConnectionSec; // 0 for no keep-alive (force close)
    fHeadersSec: TAsyncConnectionSec;
    fRequestFlags: THttpServerRequestFlags;
    fPipelineState: set of (pEnabled, pWrite);
    fRespStatus: cardinal;
    fRequest: THttpServerRequest; // recycled between calls
    fConnectionOpaque: THttpServerConnectionOpaque; // two PtrUInt tags
    fConnectionID: THttpServerConnectionID; // may be <> fHandle behind nginx
    fAfterResponseStart: Int64;
    fAuthRejectSec: cardinal;
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    procedure HttpInit;
      {$ifdef HASINLINE} inline; {$endif}
    // overriden to wait for background Write to finish
    procedure BeforeProcessRead; override;
    // redirect to fHttp.ProcessRead()
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    // redirect to fHttp.ProcessWrite()
    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    // DoRequest gathered all output in fWr buffer to be sent at once
    function FlushPipelinedWrite: TPollAsyncSocketOnReadWrite;
    // handle ifProcessing flag
    procedure OnClose; override;
    // quickly reject incorrect requests (payload/timeout/OnBeforeBody)
    function DoReject(status: integer): TPollAsyncSocketOnReadWrite;
    function DecodeHeaders: integer; virtual; // e.g. hfConnectionUpgrade override
    function DoHeaders: TPollAsyncSocketOnReadWrite;
    function DoRequest: TPollAsyncSocketOnReadWrite;
    function DoResponse(res: TPollAsyncSocketOnReadWrite): TPollAsyncSocketOnReadWrite;
    procedure DoAfterResponse;
  public
    /// reuse this instance for a new incoming connection
    procedure Recycle(const aRemoteIP: TNetAddr); override;
    /// access to the internal two PtrUInt tags of this connection
    // - may return nil e.g. behind a nginx proxy
    function GetConnectionOpaque: PHttpServerConnectionOpaque;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// event-driven process of HTTP/WebSockets connections
  THttpAsyncConnections = class(TAsyncServer)
  protected
    fAsyncServer: THttpAsyncServer;
    procedure IdleEverySecond; override;
    procedure SetExecuteState(State: THttpServerExecuteState); override;
    procedure DoExecute; override;
  published
    /// used for hsoBan40xIP has been defined or via Banned.BlackList
    // - indicates e.g. how many accept() have been rejected from their IP
    // - you can customize its behavior once the server is started by resetting
    // its Seconds/Max/WhiteIP properties, before any connections are made
    property Banned: THttpAcceptBan
      read fBanned;
  end;

  /// implement HTTP async client requests
  // - reusing the threads pool and sockets polling of an associated
  // TAsyncConnections instance (typically a THttpAsyncServer)
  THttpAsyncClientConnections = class(TSynPersistent)
  protected
    fLock: TLightLock;
    fOwner: TAsyncConnections;
    fConnectionTimeoutMS: integer;
    fUserAgent: RawUtf8;
  public
    /// initialize the instance for a given TAsyncConnections
    // - connections will be kept alive up to aConnectionTimeoutSec seconds,
    // ready to be reused
    constructor Create(aOwner: TAsyncConnections;
      aConnectionTimeoutSec: integer); reintroduce;
    /// start an async connection to a remote HTTP server using a callback
    // - the aOnStateChanged event will be called after each http.State change
    function StartRequest(var aUrl: TUri; const aMethod, aHeaders: RawUtf8;
      const aOnStateChanged: TOnHttpClientAsync;
      aTls: PNetTlsContext; const aDestFileName: TFileName;
      out aConnection: THttpAsyncClientConnection;
      aOnStateChange: TOnHttpClientStates =
        [low(TOnHttpClientState) .. high(TOnHttpClientState)]): TNetResult;
    /// called to notify that the main process is about to finish
    procedure Shutdown;
    /// allow to customize the User-Agent used by each client connection
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
  end;

  /// meta-class of THttpAsyncConnections type
  THttpAsyncConnectionsClass = class of THttpAsyncConnections;

  /// HTTP server using non-blocking sockets
  THttpAsyncServer = class(THttpServerSocketGeneric)
  protected
    fAsync: THttpAsyncConnections;
    fHeadersDefaultBufferSize: integer;
    fHeadersMaximumSize: integer;
    fConnectionClass: TAsyncConnectionClass;
    fConnectionsClass: THttpAsyncConnectionsClass;
    fRequestClass: THttpServerRequestClass;
    fInterning: PRawUtf8InterningSlot;
    fInterningTix: cardinal;
    fExecuteEvent: TSynEvent;
    fClientSockets: THttpAsyncClientConnections; // allocated when needed
    fHttpDateNowUtc: THttpDateNowUtc;
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetConnectionsActive: cardinal; override;
    function GetExecuteState: THttpServerExecuteState; override;
    function GetBanned: THttpAcceptBan; override;
    procedure IdleEverySecond; virtual;
    procedure AppendHttpDate(var Dest: TRawByteStringBuffer); override;
    // the main thread will Send output packets in the background
    procedure DoExecute; override;
    {$ifdef OSWINDOWS}
    function GetApiVersion: RawUtf8; override; // 'WinIocp'
    {$endif OSWINDOWS}
  public
    /// create an event-driven HTTP Server
    constructor Create(const aPort: RawUtf8; const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; ProcessOptions: THttpServerOptions = [];
      aLog: TSynLogClass = nil); override;
    /// finalize the HTTP Server
    destructor Destroy; override;
    /// send an asynchronous response to the client, e.g. after slow DB process
    procedure AsyncResponse(Handle: TConnectionAsyncHandle;
      const Content, ContentType: RawUtf8; Status: cardinal = HTTP_SUCCESS); override;
    /// access async connections to any remote HTTP server
    // - will reuse the threads pool and sockets polling of this instance
    function Clients: THttpAsyncClientConnections;
    /// the class used for each THttpServerRequest instances
    property RequestClass: THttpServerRequestClass
      read fRequestClass write fRequestClass;
  published
    /// initial capacity of internal per-connection Headers buffer
    // - the 2KB default is within the mormot.core.fpcx64mm SMALL blocks limit
    // so will try up to 3 locks before contention
    property HeadersDefaultBufferSize: integer
      read fHeadersDefaultBufferSize write fHeadersDefaultBufferSize;
    /// maximum allowed size in bytes of incoming headers
    // - is set to 64KB by default, which seems conservative enough
    property HeadersMaximumSize: integer
      read fHeadersMaximumSize write fHeadersMaximumSize;
    /// direct access to the internal high-performance TCP server
    // - you could set e.g. Async.MaxConnections or Async.Banned properties
    property Async: THttpAsyncConnections
      read fAsync;
  end;


{ ******************** THttpProxyServer HTTP Server With Proxy and Cache }

type
  /// the result of THttpProxyMem.FromUri
  THttpProxyCacheKind = set of (
    pckIgnore,
    pckForce);

  /// define the caching settings of content for THttpProxyServer
  // - set the memory cache settings if used as exact THttpProxyMem class
  THttpProxyMem = class(TSynPersistent)
  protected
    fMaxSize: Int64;
    fTimeoutSec: integer;
    fIgnoreCsv, fForceCsv: RawUtf8;
    fIgnore, fForce: TUriMatch; // parsed fIgnoreCsv, fForceCsv
  public
    /// setup the default values of this cache
    constructor Create; override;
    /// check the IgnoreCsv and ForceCSv properties against a given URI
    function FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
  published
    /// size (in bytes) below which the file should be included in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property MaxSize: Int64
      read fMaxSize write fMaxSize;
    /// how many seconds this file should remain in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property TimeoutSec: integer
      read fTimeoutSec write fTimeoutSec;
    /// CSV list of GLOB file names to be excluded to this cache
    // - e.g. '*.changelog,*.tmp'
    property IgnoreCsv: RawUtf8
      read fIgnoreCsv write fIgnoreCsv;
    /// CSV list of GLOB file names to be included in this cache even if
    // its size does not match MaxSize
    // - e.g. '*.xml'
    property ForceCsv: RawUtf8
      read fForceCsv write fForceCsv;
  end;

  /// define disk cache settings of content for THttpProxyServer
  THttpProxyDisk = class(THttpProxyMem)
  protected
    fPath: TFileName;
  published
    /// default '' will use the main THttpProxyServerSettings value
    property Path: TFileName
      read fPath write fPath;
  end;

  /// set of THttpProxyUrl.Options items, used to refine a specific URI process
  // - hpoNoSubFolder disable access to any sub-folder within this URI
  // - hpoNoFolderHtmlIndex disable the HTML index generation at folder level
  // - hpoPublishMd5/psoPublishSha1/psoPublishSha256 enable hash content
  // generation on server side with .md5/.sha1/.sha256 extension on a resource
  // - hpoDisable304 disable "if-none-match:" / "if-modified-since:" headers
  // default support as efficient 304 HTTP_NOTMODIFIED response
  // - hpoClientOnlySocket will be used for THttpProxyUrl.RemoteClientHead()
  THttpProxyUrlOption = (
    hpoNoSubFolder,
    hpoNoFolderHtmlIndex,
    hpoDisableFolderHtmlIndexCache,
    hpoPublishMd5,
    hpoPublishSha1,
    hpoPublishSha256,
    hpoDisable304,
    hpoClientOnlySocket);
  /// store THttpProxyUrl options for a given URI
  THttpProxyUrlOptions = set of THttpProxyUrlOption;

  /// define one URL content setting for THttpProxyServer
  THttpProxyUrl = class(TSynPersistent)
  protected
    fUrl, fSource: RawUtf8;
    fDisabled: boolean;
    fOptions: THttpProxyUrlOptions;
    fMethods: TUriRouterMethods;
    fSourced: (sUndefined, sLocalFolder, sRemoteUri);
    fAlgos: THashAlgos; // hfMD5,hfSha1,hfSha256
    fCacheControlMaxAgeSec: integer;
    fMemCache: THttpProxyMem;
    fDiskCache: THttpProxyDisk;
    fRejectCsv: RawUtf8;
    fLocalFolder: TFileName;
    fRemoteUri: TUri;
    fMemCached: TSynDictionary;  // Uri:RawUtf8 / Content:RawByteString
    fHashCached: TSynDictionary; // Uri: RawUtf8 / hash[fAlgos]: TRawUtf8DynArray
    fReject: TUriMatch;
    fRemoteClient: IHttpClient;
    fRemoteClientSafe: TOSLightLock;
    function ReturnHash(ctxt: THttpServerRequestAbstract; h: THashAlgo;
      const name: RawUtf8; var fn: TFileName): integer;
    function RemoteClientHead(const uri: TUri; var header: RawUtf8): cardinal;
  public
    /// setup the default values of this URL
    constructor Create; override;
    /// finalize this instance
    destructor Destroy; override;
  published
    /// this source won't be processed if this property is set to true
    property Disabled: boolean
      read fDisabled write fDisabled;
    /// the local URI prefix to use with the main HTTP(S) server of this instance
    // - a typical value is e.g. 'debian' for 'http://ftp.debian.org/debian'
    property Url: RawUtf8
      read fUrl write fUrl;
    /// CSV list of GLOB file or directly names to be rejected as not found
    // - e.g. '*.secret'
    property RejectCsv: RawUtf8
      read fRejectCsv write fRejectCsv;
    /// a local folder name or remote origin URL to ask
    // - if Source is a local folder (e.g. 'd:/mysite' or '/var/www/mysite'),
    // the Url prefix chars will be removed from the client request, then used
    // to locate the file to be served within this local folder
    // - if Source is a remote URI (like http://....), the Url prefix chars will
    // be removed from the client request, then appended to this remote URI for
    // remote proxy with caching, which is e.g. 'http://ftp.debian.org/debian'
    // 'or http://security.debian.org/debian-security' matching Local 'debian' or
    // 'debian-security' prefixes, to compute a source remote URI
    property Source: RawUtf8
      read fSource write fSource;
    /// which methods are applied to the local Source folder or relayed to
    // the Remote server
    // - equals by default [urmGet, urmHead]
    property Methods: TUriRouterMethods
      read fMethods write fMethods;
    /// refined the process of this URI definition
    property Options: THttpProxyUrlOptions
      read fOptions write fOptions;
    /// support optional "Cache-Control: max-age=..." header timeout value
    // - default 0 value will disable this header transmission
    // - to be used in conjunction with the hpoIfModifiedSince option
    property CacheControlMaxAgeSec: integer
      read fCacheControlMaxAgeSec write fCacheControlMaxAgeSec;
    /// overwrite the main MemCache setting to tune in-memory caching
    // - can be used for both local file or remote URI lookups
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// overwrite the main DiskCache setting to tune on-disk caching
    // - disk cache is available only for remote URI lookups, i.e. if Source
    // is defined as 'http://...' and not as a local file
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
  end;

  /// define one or several remote content source(s) for THttpProxyServer
  THttpProxyUrlObjArray = array of THttpProxyUrl;

  /// the available high-level options for THttpProxyServerMainSettings
  // - psoLogVerbose could be used to debug a server in production
  // - psoExcludeDateHeader won't include the default "Date: ..." HTTP header
  // - psoHttpsSelfSigned will enable HTTPS with a self-signed certificate
  // - psoReusePort will set SO_REUSEPORT on POSIX, to bind several servers
  // - psoEnableLogging enable an associated THttpServerGeneric.Logger instance
  // - psoRejectBotUserAgent identifies and rejects Bots via IsHttpUserAgentBot()
  // - psoBan40xIP will reject any IP for a few seconds after a 4xx error code
  // - psoDisableMemCache will globally disable all MemCache settings
  THttpProxyServerOption = (
    psoLogVerbose,
    psoExcludeDateHeader,
    psoHttpsSelfSigned,
    psoReusePort,
    psoEnableLogging,
    psoRejectBotUserAgent,
    psoBan40xIP,
    psoDisableMemCache);

  /// a set of available options for THttpProxyServerMainSettings
  THttpProxyServerOptions = set of THttpProxyServerOption;

  /// define the THttpProxyServer HTTP(S) server settings
  THttpProxyServerMainSettings = class(TSynAutoCreateFields)
  protected
    fPort: RawUtf8;
    fOptions: THttpProxyServerOptions;
    fThreadCount: integer;
    fCertificateFile: TFileName;
    fCACertificatesFile: TFileName;
    fPrivateKeyFile: TFileName;
    fPrivateKeyPassword: SpiUtf8;
    fServerName: RawUtf8;
    fLog: THttpLoggerSettings;
    fFaviconFile: TFileName;
  public
    /// initialize the default settings
    constructor Create; override;
    /// assign the HTTPS/TLS settings to a context
    function SetupTls(var tls: TNetTlsContext): boolean;
  published
    /// the local port used for HTTP/HTTPS content delivery
    // - is '8098' by default (THttpPeerCache uses 8099), unassigned by IANA
    // - you can bind to a specific 'IP:port' if needed
    property Port: RawUtf8
      read fPort write fPort;
    /// customize this proxy cache HTTP/HTTPS process
    property Options: THttpProxyServerOptions
      read fOptions write fOptions;
    /// the number of threads of the HTTP/HTTPS content delivery
    // - default value is 4 sub-threads which is enough to scale well if no
    // content is to be generated
    property ThreadCount: integer
      read fThreadCount write fThreadCount;
    /// custom log settings for the psoEnableLogging option
    // - e.g. to override default LOGFORMAT_COMBINED output, or rotation settings
    property Log: THttpLoggerSettings
      read fLog write fLog;
    /// optional HTTPS certificate file name
    // - should also set PrivateKeyFile and PrivateKeyPassword
    property CertificateFile: TFileName
      read fCertificateFile write fCertificateFile;
    /// optional HTTPS private key file name
    property PrivateKeyFile: TFileName
      read fPrivateKeyFile write fPrivateKeyFile;
    /// optional HTTPS private key file password
    property PrivateKeyPassword: SpiUtf8
      read fPrivateKeyPassword write fPrivateKeyPassword;
    /// optional HTTPS certificates authorities file
    property CACertificatesFile: TFileName
      read fCACertificatesFile write fCACertificatesFile;
    /// optional alternate favicon.ico file name
    property FaviconFile: TFileName
      read fFaviconFile write fFaviconFile;
    /// optional Server name for HTTP/HTTPS
    // - to overwrite the default value set by the framework e.g. 'mORMot2 (Linux)'
    property ServerName: RawUtf8
      read fServerName write fServerName;
  end;

  /// define the THttpProxyServer forward proxy process
  THttpProxyServerSettings = class(TSynAutoCreateFields)
  protected
    fServer: THttpProxyServerMainSettings;
    fMemCache: THttpProxyMem;
    fDiskCache: THttpProxyDisk;
    fUrl: THttpProxyUrlObjArray;
  public
    /// initialize the default settings
    constructor Create; override;
    /// append and own a given THttpProxyUrl definition at runtime
    // - this instance will be stored and owned in Url[] array
    function AddUrl(one: THttpProxyUrl): THttpProxyUrl;
    /// create a THttpProxyUrl definition to serve a local static folder
    // - if optional ExceptionClass is supplied, the local folder should exist
    function AddFolder(const folder: TFileName; const uri: RawUtf8 = '';
      RaiseExceptionOnNonExistingFolder: ExceptionClass = nil): THttpProxyUrl;
  published
    /// define the HTTP/HTTPS server configuration
    property Server: THttpProxyServerMainSettings
      read fServer write fServer;
    /// default in-memory cache settings
    // - is set to MaxSize = 4KB and TimeoutSec = 5 minutes
    // - can be overriden by Url[].MemCache property
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// default on-disk local cache settings
    // - can be overriden by Url[].DiskCache property
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
    /// define the remote content sources
    property Url: THttpProxyUrlObjArray
      read fUrl;
  end;

  EHttpProxyServer = class(ESynException);

  /// implements a HTTP server with forward proxy and caching
  THttpProxyServer = class(TSynAutoCreateFields)
  protected
    fSettings: THttpProxyServerSettings;
    fLog: TSynLogClass;
    fSettingsOwned, fVerboseLog: boolean;
    fServer: THttpAsyncServer;
    fGC: TObjectDynArray;
    function SetupTls(var tls: TNetTlsContext): boolean; virtual;
    procedure AfterServerStarted; virtual;
    function OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
    function OnGetHead(Ctxt: THttpServerRequestAbstract; Def: THttpProxyUrl;
      Met: TUriRouterMethod; const Uri: TUriMatchName): cardinal;
  public
    /// initialize this forward proxy instance
    // - the supplied aSettings should be owned by the caller (e.g from a main
    // settings class instance) - if nil is supplied, this instance will setup
    // and own its own instance
    constructor Create(aSettings: THttpProxyServerSettings); reintroduce; virtual;
    /// finalize this class instance
    destructor Destroy; override;
    /// actually start the HTTP/HTTPS server
    // - you can specify a private key password if not already in Settings
    // - may raise some exceptions if the HTTP server cannot be started
    procedure Start(const aPrivateKeyPassword: SpiUtf8 = '');
    /// finalize the HTTP/HTTPS server
    procedure Stop;
    /// the local HTTP(S) asynchronous server
    property Server: THttpAsyncServer
      read fServer;
    /// access to the used settings
    property Settings: THttpProxyServerSettings
      read fSettings;
  end;



implementation


{ ******************** Low-Level Non-blocking Connections }

{ TPollAsyncConnection }

constructor TPollAsyncConnection.Create;
begin
  EAsyncConnections.RaiseUtf8('Unexpected %.Create: use AfterCreate', [self]);
end;

destructor TPollAsyncConnection.Destroy;
begin
  // note: our light locks do not need any specific release
  try
    if not (fClosed in fFlags) then
      try
        OnClose;
      except
      end;
    BeforeDestroy;
    // finalize the instance
    fHandle := 0; // to detect any dangling pointer
  except
    // ignore any exception at this stage
  end;
  inherited Destroy;
end;

procedure TPollAsyncConnection.AfterCreate;
begin // default implementation does nothing
end;

procedure TPollAsyncConnection.BeforeDestroy;
begin // default implementation does nothing
end;

function TPollAsyncConnection.OnFirstRead(aOwner: TPollAsyncSockets): boolean;
begin
  result := true; // continue
  if Assigned(aOwner) and
     Assigned(aOwner.fOnFirstRead) then
    try
      aOwner.fOnFirstRead(self); // typically TAsyncServer.OnFirstReadDoTls
    except
      result := false; // notify error within callback
    end;
end;

procedure TPollAsyncConnection.BeforeProcessRead;
begin // default implementation does nothing
end;

procedure TPollAsyncConnection.OnAfterWriteSubscribe;
begin // default implementation does nothing
end;

function TPollAsyncConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
begin // default implementation does nothing
  result := soContinue;
end;

function TPollAsyncConnection.IsDangling: boolean;
begin
  result := (self = nil) or
            (fHandle = 0) or
            not InheritsFrom(TPollAsyncConnection); // detect reused mem block
end;

function TPollAsyncConnection.IsClosed: boolean;
begin
  result := (self = nil) or
            (fHandle = 0) or
            (fSocket = nil) or
            (fClosed in fFlags);
end;

function TPollAsyncConnection.TryLock(writer: boolean): boolean;
begin
  if (fSocket <> nil) and
     fRWSafe[ord(writer and (ifSeparateWLock in fInternalFlags))].TryLock then
  begin
    include(fFlags, fWasActive);
    result := true;
  end
  else
    result := false;
end;

procedure TPollAsyncConnection.UnLock(writer: boolean);
begin
  if self <> nil then
    fRWSafe[ord(writer and (ifSeparateWLock in fInternalFlags))].UnLock;
end;

procedure TPollAsyncConnection.OnClose;
begin
  include(fFlags, fClosed);
end;

function TPollAsyncConnection.ReleaseReadMemoryOnIdle: PtrInt;
begin
  // caller made fRWSafe[0].TryLock
  if fRd.Len <> 0 then
    result := 0
  else
    result := fRd.Clear; // returns number of bytes released
end;

function TPollAsyncConnection.ReleaseWriteMemoryOnIdle: PtrInt;
begin
  // caller made fRWSafe[0/1].TryLock
  if fWr.Len <> 0 then
    result := 0 // the buffer is still in use - wait until fWr.Reset
  else
    result := fWr.Clear;
end;

function TPollAsyncConnection.ReleaseMemoryOnIdle: PtrInt;
begin
  // called now and then to reduce temp memory consumption on Idle connections
  result := 0;
  if (fRd.Buffer <> nil) and
     (fRd.Len = 0) and
     fRWSafe[0].TryLock then // direct call to leave fWasActive flag untouched
  begin
    inc(result, ReleaseReadMemoryOnIdle);
    if (fWr.Buffer <> nil) and
       (fWr.Len = 0) and
       not (ifSeparateWLock in fInternalFlags) then
      inc(result, ReleaseWriteMemoryOnIdle); // do it within the same lock
    fRWSafe[0].UnLock;
  end;
  if (ifSeparateWLock in fInternalFlags) and
     (fWr.Buffer <> nil) and
     (fWr.Len = 0) and
     fRWSafe[1].TryLock then
  begin
    inc(result, ReleaseWriteMemoryOnIdle);
    fRWSafe[1].UnLock;
  end;
end;

function TPollAsyncConnection.WaitLock(writer: boolean; timeoutMS: cardinal): boolean;
var
  endtix: Int64;
  ms: integer;
begin
  result := false;
  if IsClosed then
    exit;
  result := TryLock(writer);
  if result or
     (timeoutMS = 0) then
    // we acquired the Connection for this direction, or we don't want to wait
    exit;
  // loop to wait for the lock release
  endtix := mormot.core.os.GetTickCount64 + timeoutMS; // never wait forever
  ms := 0;
  repeat
    SleepHiRes(ms);
    ms := ms xor 1; // 0,1,0,1,0,1...
    if IsClosed  then
      break; // no socket to lock any more
    result := TryLock(writer);
    if result then
    begin
      // the lock has been acquired
      result := not IsClosed; // check it again
      if not result then
        UnLock(writer);
      break; // acquired or socket closed
    end;
  until mormot.core.os.GetTickCount64 >= endtix;
end;

function TPollAsyncConnection.Send(buf: pointer; var len: integer): TNetResult;
begin
  if fSecure <> nil then
    result := fSecure.Send(buf, len)
  else
    result := fSocket.Send(buf, len);
  if result = nrOK then
    inc(fBytesSend, len);
end;

function TPollAsyncConnection.Recv(buf: pointer; var len: integer): TNetResult;
begin
  if fSecure <> nil then
    result := fSecure.Receive(buf, len)
  else
    result := fSocket.Recv(buf, len);
  if result = nrOK then
    inc(fBytesRecv, len);
end;

{$ifdef USE_WINIOCP}

function TPollAsyncConnection.IocpPrepareNextWrite(queue: TWinIocp): boolean;
begin
  if fWr.Len = 0 then
    // nothing more to send
    result := true
  else if fSecure = nil then
    // try to send some plain data asynchronously
    // otherwise GetNext() would return with no delay
    result := queue.PrepareNext('next', fIocpSub, wieSend, fWr.Buffer, fWr.len)
  else
    // on TLS, don't send any plain buffer but let INetTls handle the socket
    result := queue.PrepareNext('tls', fIocpSub, wieSend);
end;

{$else}

{ TPollReadSockets }

function TPollReadSockets.EnsurePending(tag: TPollSocketTag): boolean;
begin
  // fast O(1) flag access
  if tag = 0 then // tag = 0 at shutdown
    result := false
  else
  begin
    result := fReadPending in TPollAsyncConnection(tag).fFlags;
    include(TPollAsyncConnection(tag).fFlags, fReadPending); // always set
  end;
end;

procedure TPollReadSockets.SetPending(tag: TPollSocketTag);
begin
  if tag <> 0 then // tag = 0 at shutdown
    include(TPollAsyncConnection(tag).fFlags, fReadPending);
end;

function TPollReadSockets.UnsetPending(tag: TPollSocketTag): boolean;
begin
  result := false;
  if tag <> 0 then
    // same paranoid logic than TPollAsyncConnection IsDangling() + TryLock()
    if // avoid dangling pointer
       (TPollAsyncConnection(tag).fHandle <> 0) and
       // another atpReadPending thread may currently own this connection
       // (occurs if PollForPendingEvents was called in between)
       (not TPollAsyncConnection(tag).fRWSafe[0].IsLocked) then
    begin
      exclude(TPollAsyncConnection(tag).fFlags, fReadPending);
      result := true;
    end;
end;

{$endif USE_WINIOCP}


{ TPollAsyncSockets }

constructor TPollAsyncSockets.Create(aOptions: TPollAsyncSocketsOptions;
  aThreadCount: integer);
begin
  fOptions := aOptions;
  inherited Create;
  {$ifdef USE_WINIOCP}
  fIocpRecvSend := TWinIocp.Create(aThreadCount, [wioUnsubscribeShutdownSocket]);
  {$else}
  fRead := TPollReadSockets.Create;
  fRead.UnsubscribeShouldShutdownSocket := true;
  fWrite := TPollWriteSockets.Create;
  {$endif USE_WINIOCP}
end;

destructor TPollAsyncSockets.Destroy;
begin
  if not fTerminated then
    Terminate(5000);
  {$ifdef USE_WINIOCP}
  fIocpRecvSend.Free;
  {$else}
  fRead.Free;
  fWrite.Free;
  {$endif USE_WINIOCP}
  inherited Destroy;
end;

const
  // maximum/optimum socket output buffer is 128KB on 32-bit and 256KB on 64-bit
  SENDBUF_MEM = (256 shl 10) {$ifdef CPU32} shr 1 {$endif CPU32};

function TPollAsyncSockets.Start(connection: TPollAsyncConnection): boolean;
begin
  result := false;
  if fTerminated or
     connection.IsDangling then
    exit;
  LockedInc32(@fProcessingRead);
  try
    {if fDebugLog <> nil then
      DoLog('Start sock=% handle=%',
        [pointer(connection.fSocket), connection.Handle]);}
    // get sending buffer size from OS (once - if not already retrieved)
    if fSendBufferSize = 0 then
    begin
      {$ifdef OSWINDOWS}
      // on Windows, default buffer is reported as 8KB by fSocket.SendBufferSize
      // but the actual value is much bigger and modified at runtime
      fSendBufferSize := SENDBUF_MEM; // just assume optimal 128/256KB on Windows
      // if direct Write() doesn't succeed, it will subscribe to ProcessWrite
      {$else}
      // on Linux/POSIX, typical values are 2MB for TCP, 200KB on Unix Sockets
      fSendBufferSize := connection.fSocket.SendBufferSize;
      if fSendBufferSize > SENDBUF_MEM then
         fSendBufferSize := SENDBUF_MEM; // no benefit with anything bigger
      {$endif OSWINDOWS}
    end;
    // subscribe for incoming data (async for select/poll, immediate for epoll)
    if Assigned(fOnStart) then
      result := fOnStart(connection); // e.g. TAsyncConnections.ProcessClientStart
    // result=true here means that fRead.Subscribe() is delayed in atpReadPending
    // warning: result=true may actually make connection.Free before it returns
    // result=false is returned e.g. to call TWinIocp.Subscribe()
    if not result then
      // let ProcessRead handle pseRead+pseError/pseClosed on this socket
      result := SubscribeConnection('start', connection, pseRead);
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

{$ifndef USE_WINIOCP}
const
  _SUB: array[boolean] of AnsiChar = '-+';
{$endif USE_WINIOCP}

function TPollAsyncSockets.Stop(connection: TPollAsyncConnection;
  const caller: ShortString): boolean;
var
  sock: TNetSocket;
begin
  result := false;
  if fTerminated or
     connection.IsDangling then
    exit;
  LockedInc32(@fProcessingRead);
  try
    // retrieve the raw information of this abstract connection
    sock := connection.fSocket;
    if fDebugLog <> nil then
      {$ifdef USE_WINIOCP}
      fDebugLog.Add.Log(sllDebug, 'Stop(%) sock=% handle=% r=% w=%',
        [caller, pointer(sock), connection.Handle,
         connection.fRWSafe[0].IsLocked, connection.fRWSafe[1].IsLocked], self);
      {$else}
      fDebugLog.Add.Log(sllDebug, 'Stop(%) sock=% handle=% r=%% w=%%',
        [caller, pointer(sock), connection.Handle,
         connection.fRWSafe[0].IsLocked, _SUB[fSubRead in connection.fFlags],
         connection.fRWSafe[1].IsLocked, _SUB[fSubWrite in connection.fFlags]], self);
      {$endif USE_WINIOCP}
    if sock <> nil then
    begin
      // notify ProcessRead/ProcessWrite to abort
      connection.fSocket := nil;
      // clean the TLS state
      if connection.fSecure <> nil then
        try
          connection.fSecure := nil; // perform TLS shutdown and release context
        except
          pointer(connection.fSecure) := nil; // leak better than propagated GPF
        end;
      // unsubscribe and close the socket
      {$ifdef USE_WINIOCP}
      if connection.fIocpSub <> nil then
        fIocpRecvSend.Unsubscribe(connection.fIocpSub)
        // with wioUnsubscribeShutdownSocket for final wieRecv + closesocket()
      else
        // close the socket even if not subscribed (e.g. HTTP/1.0)
        sock.ShutdownAndClose({rdwr=}true, {waitms=}100); // ensure sent
      {$else}
      if fSubWrite in connection.fFlags then
        // write first because of fRead.UnsubscribeShouldShutdownSocket=true
        fWrite.Unsubscribe(sock, TPollSocketTag(connection));
      if fSubRead in connection.fFlags then
        // note: fRead.UnsubscribeShouldShutdownSocket=true, so ShutdownAndClose
        // is done now on Epoll/TWinIocp, or at next PollForPendingEvents()
        fRead.Unsubscribe(sock, TPollSocketTag(connection))
      else
        // close the socket even if not subscribed (e.g. HTTP/1.0)
        sock.ShutdownAndClose({rdwr=}false);
      {$endif USE_WINIOCP}
      result := true;
    end
    {$ifdef USE_WINIOCP}
    else if connection.fIocpSub <> nil then
      fIocpRecvSend.Unsubscribe(connection.fIocpSub); // paranoid
    {$endif USE_WINIOCP}
  finally
    LockedDec32(@fProcessingRead);
  end;
  if result and
     Assigned(fOnStop) then
    fOnStop(connection);
end;

function TPollAsyncSockets.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := {$ifdef USE_WINIOCP} fIocpRecvSend {$else} fRead {$endif}.Count;
end;

procedure TPollAsyncSockets.DoLog(TextFmt: PUtf8Char;
  const TextArgs: array of const; Level: TSynLogLevel);
begin
  fDebugLog.Add.Log(Level, TextFmt, TextArgs, self);
end;

procedure TPollAsyncSockets.Terminate(waitforMS: integer);
var
  start, elapsed: Int64;
begin
  if fDebugLog <> nil then
    DoLog('Terminate(%) processing fRd=% fWr=%',
      [waitforMS, fProcessingRead, fProcessingWrite], sllDebug);
  fTerminated := true;
  // abort receive/send polling engines
  {$ifdef USE_WINIOCP}
  fIocpRecvSend.Terminate; // will notify all pending threads
  {$else}
  fRead.Terminate;
  fWrite.Terminate;
  {$endif USE_WINIOCP}
  // wait for actual termination
  if (waitforMS <= 0) or
     ((fProcessingRead = 0) and
      (fProcessingWrite = 0)) then
    exit;
  start := mormot.core.os.GetTickCount64;
  repeat
    SleepHiRes(1);
    elapsed := mormot.core.os.GetTickCount64 - start;
  until ((fProcessingRead = 0) and
         (fProcessingWrite = 0)) or
         (elapsed > waitforMS);
  if fDebugLog <> nil then
    DoLog('Terminate processing fRd=% fWr=% after %ms',
      [fProcessingRead, fProcessingWrite, elapsed], sllDebug);
end;

function TPollAsyncSockets.WriteString(connection: TPollAsyncConnection;
  const data: RawByteString; timeout: integer): boolean;
begin
  if self = nil then
    result := false
  else
    result := Write(connection, pointer(data), length(data), timeout);
end;

function TPollAsyncSockets.RawWrite(connection: TPollAsyncConnection;
  var data: PByte; var datalen: integer): boolean;
var
  res: TNetResult;
  sent: integer;
begin
  result := false; // closed or failed
  repeat
    if fTerminated or
       (connection.fSocket = nil) then
      exit;
    sent := datalen;
    res := connection.Send(data, sent);
    if connection.fSocket = nil then
      exit;  // Stop() called
    if res = nrRetry then
      break; // fails now -> return true and retry later in ProcessWrite
    if res <> nrOK then
    begin
      if fDebugLog <> nil then
        DoLog('Write: connection.Send(%)=% len=% handle=%',
          [pointer(connection.Socket), _NR[res],
           sent, connection.Handle], sllLastError);
      exit;  // connection closed or broken -> abort
    end;
    inc(data, sent);
    inc(fWriteCount);
    inc(fWriteBytes, sent);
    dec(datalen, sent);
  until datalen = 0;
  result := true; // retry later (perhaps with pending data/datalen)
end;

function TPollAsyncSockets.Write(connection: TPollAsyncConnection;
  data: pointer; datalen: integer; timeout: integer): boolean;
var // warning: datalen parameter should be defined as integer, not PtrInt
  P: PByte;
  previous: integer;
  res: TPollAsyncSocketOnReadWrite;
begin
  result := false;
  if (datalen <= 0) or
     fTerminated or
     connection.IsClosed then
    exit;
  // try and wait for another ProcessWrite
  LockedInc32(@fProcessingWrite);
  if connection.WaitLock({writer=}true, timeout) then
  try
    // we acquired the write lock: immediate or delayed/buffered sending
    //DoLog('Write: WaitLock fProcessingWrite=%', [fProcessingWrite]);
    P := data;
    // try to send now in non-blocking mode (works most of the time)
    previous := connection.fWr.Len;
    if (previous = 0) and
       not (paoWritePollOnly in fOptions) then
      if not RawWrite(connection, P, datalen) then // use OS buffers
        exit; // aborted
    if connection.fSocket = nil then
      exit;
    if datalen <> 0 then
    begin
      // use fWrite output polling for the remaining data in ProcessWrite
      connection.fWr.Append(P, datalen);
      res := soContinue;
    end
    else
      // notify everything written - and maybe call slot.fWr.Append
      res := DoAfterWrite('Write', connection);
    result := true; // soContinue or soWaitWrite
    case res of
      soContinue:
        if connection.fWr.Len > 0 then
         // there is still some pending output bytes
         if previous = 0 then
           // register for ProcessWrite() if not already
           result := SubscribeConnection('write', connection, pseWrite);
      soDone:
        result := true;  // continue with no subscribe
      soClose:
        result := false; // abort
    end;
  finally
    //DoLog('Write: finally fProcessingWrite=%', [fProcessingWrite]);
    if result then
      connection.UnLock({writer=}true)
    else
      // sending or subscription error -> abort
      CloseConnection(connection, 'Write() failure');
    LockedDec32(@fProcessingWrite);
  end
  else
  begin
    // WaitLock() should always work - unless the connection is closing or
    // several write operations collide (e.g. websockets broadcast + process)
    if fDebugLog <> nil then
      DoLog('Write: WaitLock failed %', [pointer(connection)]);
    LockedDec32(@fProcessingWrite);
  end;
  //DoLog('Write: done fProcessingWrite=%', [fProcessingWrite]);
end;

function TPollAsyncSockets.SubscribeConnection(const caller: ShortString;
  connection: TPollAsyncConnection; sub: TPollSocketEvent): boolean;
var
  tag: TPollSocketTag absolute connection;
begin
  if not (fInList in connection.fFlags) then // not already registered
    RegisterConnection(connection);
  result := false;
  if not (sub in [pseRead, pseWrite]) then
    exit;
  {$ifdef USE_WINIOCP}
  if connection.fIocpSub = nil then
    connection.fIocpSub := fIocpRecvSend.Subscribe(connection.fSocket, tag);
  result := connection.fIocpSub <> nil;
  if result then
    case sub of
      pseRead:
        result := fIocpRecvSend.PrepareNext('sub', connection.fIocpSub, wieRecv);
      pseWrite:
        result := fIocpRecvSend.PrepareNext('sub', connection.fIocpSub, wieSend);
    end;
  {$else}
  if sub = pseRead then
    if fSubRead in connection.fFlags then
      exit
    else
      result := fRead.Subscribe(connection.fSocket, [pseRead], tag)
  else if fSubWrite in connection.fFlags then
      exit
    else
      result := fWrite.Subscribe(connection.fSocket, [pseWrite], tag);
  if result then
     if sub = pseRead then
       include(connection.fFlags, fSubRead)
     else
     begin
       include(connection.fFlags, fSubWrite);
       connection.OnAfterWriteSubscribe; // overriden in THttpAsyncConnection
     end;
  {$endif USE_WINIOCP}
  if fDebugLog <> nil then
    fDebugLog.Add.Log(sllDebug, 'Subscribe(%,%)=% % handle=%',
      [pointer(connection.fSocket), POLL_SOCKET_EVENT[sub], BOOL_STR[result],
       caller, connection.Handle], self);
end;

procedure TPollAsyncSockets.CloseConnection(
  var connection: TPollAsyncConnection; const caller: ShortString);
var
  c: TPollAsyncConnection;
begin
  try
    // wipe supplied instance reference ASAP to own it from now on
    c := connection;
    connection := nil;
    // check for wrong reference (paranoid)
    if c.IsDangling then
      exit;
    // final acquisition of connection's read+write locks for this thread
    if (not c.WaitLock({writer=}false, 500)) or
       ((ifSeparateWLock in c.fInternalFlags) and
        (not c.WaitLock({writer=}true, 500))) then
    begin
      c.fRWSafe[0].ForceLock; // locks should be available within 0.5 second
      c.fRWSafe[1].ForceLock;
    end;
    // call OnClose virtual method once
    if not (fClosed in c.fFlags) then
      // before slot/socket closing: needed for TWebSocketAsyncConnection
      c.OnClose; // set fClosed flag
    // set socket := nil and async unsubscribe for next PollForPendingEvents()
    Stop(c, caller);
    // now safe to perform fOwner.ConnectionDelete() for async instance GC
    OnClosed(c);
  except
    // just ignore any exception here (may leak memory at the cost of stability)
  end;
end;

function TPollAsyncSockets.ProcessRead(Sender: TSynThread;
  const notif: TPollSocketResult): boolean;
var
  connection: TPollAsyncConnection;
  recved: integer; // should be integer
  added, retryms: integer;
  pse: TPollSocketEvents;
  res: TNetResult;
  start: Int64;
  wf: TShort3;
  temp: array[0..$7fff] of byte; // up to 32KB moved to small reusable fRd.Buffer
begin
  result := true; // if closed or properly read: don't retry
  connection := TPollAsyncConnection(ResToTag(notif));
  if (self = nil) or
     fTerminated or
     connection.IsClosed then
    exit;
  connection.fReadThread := Sender;
  LockedInc32(@fProcessingRead);
  try
    pse := ResToEvents(notif);
    if pseClosed in pse then
    begin
      // - properly triggered from EPOLLRDHUP on Linux in ET mode
      // - on Windows, select() doesn't return any "close" flag
      // and checking for pending bytes for closed connection is not correct
      // on multi-thread -> Recv() below will properly detect disconnection
      CloseConnection(connection, 'ProcessRead pseClosed');
      exit;
    end;
    if pseError in pse then
      // check for pseError after pseClosed, because closing is no fatal error
      if not OnError(connection, pse) then
      begin
        CloseConnection(connection, 'ProcessRead pseError');
        exit;
      end;
    if pseRead in pse then
    begin
      // we were notified that there may be some pending input data
      added := 0;
      retryms := fReadWaitMs;
      if connection.TryLock({writer=}false) then
      // GetOnePending may be from several threads -> ensure locked
      begin
        // implement TLS handshake at socket level before any user-level process
        if not (fFirstRead in connection.fFlags) then
        begin
          include(connection.fFlags, fFirstRead);
          // calls e.g. TAsyncServer.OnFirstReadDoTls
          if not connection.OnFirstRead(self) then
          begin
            // TLS error -> abort
            CloseConnection(connection, 'ProcessRead OnFirstRead');
            exit;
          end;
          // waiting a little just after accept() helps a idle server to respond
          if (retryms = 0) and
             (fProcessingRead < 4) then // < 4 for "wrk -c 10000" not fail
            retryms := 50;
        end;
        // receive as much data as possible into connection.fRd buffer
        repeat
          if fTerminated or
             (connection.fSocket = nil) then
            exit;
          if fDebugLog <> nil then
            QueryPerformanceMicroSeconds(start);
          recved := SizeOf(temp);
          res := connection.Recv(@temp, recved); // no need of RecvPending()
          if (res = nrRetry) and
             (retryms <> 0) then
          begin
            // seen after accept() or from ab -> leverage this thread
            recved := SizeOf(temp);
            if neRead in connection.fSocket.WaitFor(retryms, [neRead, neError]) then
              res := connection.Recv(@temp, recved);
            wf := 'wf ';
          end
          else
            wf[0] := #0;
          if fDebugLog <> nil then
            fDebugLog.Add.Log(LOG_TRACEWARNING[not (res in [nrOk, nrRetry, nrClosed])],
              'ProcessRead recv(%)=% len=% %in %',
              [pointer(connection.Socket), _NR[res], recved, wf,
               MicroSecFrom(start)], self);
          if connection.fSocket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break; // may block, try later
          if res <> nrOk then
          begin
            // socket closed gracefully or unrecoverable error -> abort
            CloseConnection(connection, _NR[res]);
            exit;
          end;
          if (recved < 1024) and
             (connection.fRd.Len = 0) then
            connection.fRd.Reserve(1024); // minimal reusable fRd.Buffer
          connection.fRd.Append(@temp, recved);
          inc(added, recved);
        until recved < SizeOf(temp);
        // process the received data by calling connection.OnRead
        if added > 0 then
        begin
          inc(fReadCount); // no lock since informative only
          inc(fReadBytes, added);
          try
            // process connection.fRd incoming data (outside of the lock)
            if connection.OnRead = soClose then
              CloseConnection(connection, 'ProcessRead OnRead');
          except
            CloseConnection(connection, 'ProcessRead Exception');
          end;
        end
        else
          result := false; // retry later
        // ensure this connection will be tracked for next recv()
        if connection <> nil then // CloseConnection() set connection := nil
        begin
          {$ifdef USE_WINIOCP}
          // IOCP requires per-notification subscription
          if not fIocpRecvSend.PrepareNext('next', connection.fIocpSub, wieRecv) then
          {$else}
          if not (fSubRead in connection.fFlags) then
            // it is time to subscribe for any future read on this connection
            if not SubscribeConnection('read', connection, pseRead) then
          {$endif USE_WINIOCP}
          begin
            if fDebugLog <> nil then
              DoLog('ProcessRead: Subscribe failed %', [connection]);
            CloseConnection(connection, 'ProcessRead Subscribe failure');
          end;
          connection.UnLock({writer=}false); // eventual lock release
        end;
      end
      else // TryLock(read) failed
      begin
        if fDebugLog <> nil then
          // happens on thread contention
          DoLog('ProcessRead: TryLock failed %', [connection]);
        SleepHiRes(0); // avoid switch threads for nothing
        {$ifdef USE_WINIOCP}
        // IOCP requires per-notification subscription
        if not fIocpRecvSend.PrepareNext('next', connection.fIocpSub, wieRecv) then
          CloseConnection(connection, 'ProcessRead waitlock iocp');
        {$endif USE_WINIOCP}
        result := false; // retry later
      end;
    end;
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

procedure TPollAsyncSockets.ProcessWrite(
  const notif: TPollSocketResult; sent: integer);
var
  connection: TPollAsyncConnection;
  buf: PByte;
  buflen, w: integer; // warning: buflen should be integer, not PtrInt
  res: TPollAsyncSocketOnReadWrite;
  start: Int64;
begin
  connection := TPollAsyncConnection(ResToTag(notif));
  if (self = nil) or
     fTerminated or
     (ResToEvents(notif) <> [pseWrite]) or
     connection.IsClosed then
    exit;
  // we are now sure that the socket is writable and safe
  //DoLog('ProcessWrite: fProcessingWrite=%', [fProcessingWrite]);
  LockedInc32(@fProcessingWrite);
  try
    res := soContinue;
    {$ifdef USE_WINIOCP}
    if ((connection.fSecure = nil) or // ensure TLS won't actually block
        (ifWriteWait in connection.fInternalFlags) or
        (neWrite in connection.Socket.WaitFor(0, [neWrite, neError]))) and
       connection.WaitLock({writer=}true, {timeout=}20) then
       // allow to wait a little since we are in a single W thread
    {$else}
    if connection.TryLock({writer=}true) then // no need to wait
    {$endif USE_WINIOCP}
    try
      buflen := connection.fWr.Len;
      if buflen = 0 then
        exit;
      buf := connection.fWr.Buffer;
      if sent > 0 then // e.g. after IOCP wieSend
      begin
        inc(connection.fBytesSend, sent);
        inc(buf, sent);
        dec(buflen, sent);
      end;
      if buflen > 0 then
      begin
        w := buflen;
        if fDebugLog <> nil then
          QueryPerformanceMicroSeconds(start);
        if not RawWrite(connection, buf, buflen) then
        begin
          {$ifndef USE_WINIOCP} // no TWinIocp.PrepareNext() call is enough
          fWrite.Unsubscribe(connection.fSocket, TPollSocketTag(connection));
          exclude(connection.fFlags, fSubWrite);
          {$endif USE_WINIOCP}
          res := soClose;
          exit; // socket closed gracefully or unrecoverable error -> abort
        end;
        dec(w, buflen); // buflen = remaining data to send
        inc(sent, w);   // beforewrite = actually sent by RawWrite()
        if fDebugLog <> nil then
          fDebugLog.Add.Log(sllTrace, 'ProcessWrite RawWrite(%)=% sent=% remain=% in % pw=%',
            [pointer(connection.fSocket), w, sent, buflen,
             MicroSecFrom(start), fProcessingWrite], self);
      end
      else if fDebugLog <> nil then
        fDebugLog.Add.Log(sllTrace, 'ProcessWrite sent(%)=% pw=%',
          [pointer(connection.fSocket), sent, fProcessingWrite], self);
      connection.fWr.Remove(sent); // is very likely to just set fWr.Len := 0
      if connection.fWr.Len = 0 then
        // no more data in output buffer - AfterWrite may refill connection.fWr
        res := DoAfterWrite('ProcessWrite', connection);
      {$ifdef USE_WINIOCP}
      if res = soContinue then
        if not connection.IocpPrepareNextWrite(fIocpRecvSend) then
          res := soClose;
      {$else}
      if connection.fWr.Len = 0 then
      begin
        // no further ProcessWrite unless slot.fWr contains pending data
        fWrite.Unsubscribe(connection.fSocket, TPollSocketTag(connection));
        exclude(connection.fFlags, fSubWrite);
        if fDebugLog <> nil then
          fDebugLog.Add.Log(sllTrace, 'ProcessWrite Unsubscribe(sock=%,handle=%)=%',
           [pointer(connection.fSocket), connection.Handle], self);
      end;
      {$endif USE_WINIOCP}
    finally
      if res = soClose then // sending error or AfterWrite abort
        CloseConnection(connection, 'ProcessWrite')
      else
        connection.UnLock({writer=}true);
    end
    else
    begin
      // if already locked (unlikely) -> retry later
      if fDebugLog <> nil then
        DoLog('ProcessWrite: WaitLock failed % -> will retry later',
          [pointer(connection)]);
      SleepHiRes(0); // avoid switch threads for nothing
      {$ifdef USE_WINIOCP} // add to main IOCP queue, but no PrepareNextWrite
      fIocpRecvSend.Enqueue(connection.fIocpSub, wieSend, sent);
      {$endif USE_WINIOCP}
    end;
  finally
    LockedDec32(@fProcessingWrite);
  end;
end;

function TPollAsyncSockets.DoAfterWrite(const caller: ShortString;
  connection: TPollAsyncConnection): TPollAsyncSocketOnReadWrite;
begin
  try
    result := connection.AfterWrite;
    case result of
      soContinue,
      soDone:
        ; // just send connection.fWr content
      soClose:
        begin
          if fDebugLog <> nil then
            fDebugLog.Add.Log(sllTrace, '% % closed by AfterWrite handle=%',
              [caller, pointer(connection.fSocket), connection.Handle], self);
          connection.fWr.Clear;
        end;
      soWaitWrite:
        ObjArrayAdd(fWaitingWrite.Items, connection,
          fWaitingWrite.Safe, @fWaitingWrite.Count);
    end;
  except
    connection.fWr.Clear;
    result := soClose; // intercept any exception in AfterWrite overriden method
  end;
end;

procedure TPollAsyncSockets.ProcessWaitingWrite;
var
  queue: array of TPollAsyncConnection;
  connection: TPollAsyncConnection;
  res: TPollAsyncSocketOnReadWrite;
  i, n: PtrInt;
  {%H-}log: ISynLog;
begin
  if fWaitingWrite.Count = 0 then
    exit; // no connection in pending rfProgressiveStatic mode
  fDebugLog.EnterLocal(log, 'ProcessWaitingWrite %', [fWaitingWrite.Count], self);
  with fWaitingWrite do
  begin
    Safe.Lock;
    n := Count;
    Count := 0;
    pointer(queue) := pointer(Items); // no refcount
    pointer(Items) := nil;
    Safe.UnLock;
  end;
  for i := 0 to n - 1 do
  begin
    if fTerminated then
      exit;
    connection := queue[i];
    res := soClose;
    if not connection.IsClosed then
      if connection.WaitLock({writer=}true, 0) then
      try
        res := DoAfterWrite('ProcessWaitingWrite', connection);
        if (res = soContinue) and
           (connection.fWr.Len > 0) then // async sending of the new data
          SubscribeConnection('waitwrite', connection, pseWrite);
      finally
        if res = soClose then
          CloseConnection(connection, 'ProcessWaitingWrite')
        else
          connection.UnLock({writer=}true);
      end
      else // retry if locked (unlikely)
        ObjArrayAdd(fWaitingWrite.Items, connection,
          fWaitingWrite.Safe, @fWaitingWrite.Count);
  end;
end;


function ToText(so: TPollAsyncSocketOnReadWrite): PShortString;
begin
  result := GetEnumName(TypeInfo(TPollAsyncSocketOnReadWrite), ord(so));
end;


{ ******************** Client or Server Asynchronous Process }

{ TAsyncConnection }

constructor TAsyncConnection.Create(aOwner: TAsyncConnections;
  const aRemoteIP: TNetAddr);
begin
  fOwner := aOwner;
  // inherited Create; use AfterCreate instead
  fFlags := [fWasActive]; // by definition
  fRemoteIP4 := aRemoteIP.IP4;
  aRemoteIP.IP(fRemoteIP, {localasvoid=}true);
end;

procedure TAsyncConnection.Recycle(const aRemoteIP: TNetAddr);
begin
  fFlags := [fWasActive];
  fInternalFlags := [ifFromGC];
  fRd.Reset;
  fWr.Reset;
  fRWSafe[0].Init;
  fRWSafe[1].Init;
  fSecure := nil;
  fLastOperation := 0;
  {$ifdef USE_WINIOCP}
  fIocpSub := nil;
  {$endif USE_WINIOCP}
  fRemoteIP4 := aRemoteIP.IP4;
  aRemoteIP.IP(fRemoteIP, {localasvoid=}true);
end;

function TAsyncConnection.OnLastOperationIdle(nowsec: TAsyncConnectionSec): boolean;
begin
  result := false; // nothing happened
end;


{ TAsyncConnectionsSockets }

procedure TAsyncConnectionsSockets.OnClosed(connection: TPollAsyncConnection);
begin
  if connection.IsDangling then
    exit;
  // caller did call Stop() before calling OnClose (socket=0)
  {if acoVerboseLog in fOwner.fOptions then
    fOwner.DoLog(sllTrace, 'OnClose %', [connection], self);}
  // unregister from fOwner list and do connection.Free
  fOwner.EndConnection(connection as TAsyncConnection);
end;

function TAsyncConnectionsSockets.OnError(connection: TPollAsyncConnection;
  events: TPollSocketEvents): boolean;
var
  err: ShortString;
begin
  GetSetNameShort(TypeInfo(TPollSocketEvents), events, err);
  fOwner.DoLog(sllDebug, 'OnError% events=[%] -> free socket and instance',
    [connection, err], self);
  result := acoOnErrorContinue in fOwner.Options; // false=close by default
end;

function TAsyncConnectionsSockets.Write(connection: TPollAsyncConnection;
  data: pointer; datalen: integer; timeout: integer): boolean;
begin
  if (acoVerboseLog in fOwner.Options) and
     not (acoNoLogWrite in fOwner.Options) then
   fOwner.fLogClass.Add.LogEscape(sllTrace, 'Write', [], data, datalen, connection);
  result := inherited Write(connection, data, datalen, timeout);
end;

function TAsyncConnectionsSockets.GetTotal: integer;
begin
  result := fOwner.fLastHandle; // handles are a plain integer sequence
end;

procedure TAsyncConnectionsSockets.RegisterConnection(connection: TPollAsyncConnection);
begin
  fOwner.ConnectionAdd(TAsyncConnection(connection));
end;


{ TAsyncConnectionsThread }

constructor TAsyncConnectionsThread.Create(aOwner: TAsyncConnections;
  aProcess: TAsyncConnectionsThreadProcess; aIndex: integer);
begin
  fOwner := aOwner;
  fProcess := aProcess;
  fIndex := aIndex;
  {$ifndef USE_WINIOCP}
  fEvent := TSynEvent.Create;
  {$endif USE_WINIOCP}
  fOnThreadTerminate := fOwner.fOnThreadTerminate;
  inherited Create({suspended=}false, fOwner.fOnThreadStart,
    fOwner.fOnThreadTerminate, fOwner.fLogClass, FormatUtf8('R%:%',
    [fIndex, SplitRight(fOwner.fProcessName, '=')]));
end;

destructor TAsyncConnectionsThread.Destroy;
begin
  inherited Destroy;
  {$ifndef USE_WINIOCP}
  fEvent.Free;
  {$endif USE_WINIOCP}
  FreeAndNil(fCustomObject);
end;

{$ifndef USE_WINIOCP}

procedure TAsyncConnectionsThread.ReleaseEvent;
begin
  if fWaitForReadPending then
  begin
    fWaitForReadPending := false; // set event once
    fEvent.SetEvent;
  end;
end;

function TAsyncConnectionsThread.GetNextRead(
  out notif: TPollSocketResult): boolean;
begin
  result := fOwner.fSockets.fRead.GetOnePending(notif, fProcessName) and
            not Terminated;
  if result then
    if (acoThreadSmooting in fOwner.Options) and
       (fThreadPollingLastWakeUpTix <> fOwner.fThreadPollingLastWakeUpTix) and
       not fWakeUpFromSlowProcess then
    begin
      // ProcessRead() did take some time: wake up another thread
      // - slow down a little bit the wrk RPS
      // - but seems to reduce the wrk max latency
      fWakeUpFromSlowProcess := true; // do it once per Execute loop
      fOwner.ThreadPollingWakeup(1); // one thread is enough
    end;
end;

{$endif USE_WINIOCP}

procedure TAsyncConnectionsThread.DoExecute;
var
  {$ifdef USE_WINIOCP}
  e: TWinIocpEvent;
  sub: PWinIocpSubscription;
  bytes: cardinal;
  {$else}
  new, ms: integer;
  {$endif USE_WINIOCP}
  notif: TPollSocketResult;
begin
  try
    fExecuteState := esRunning;
    // implement parallel client connections for TAsyncClient
    while not Terminated and
          (fOwner.fThreadClients.Count > 0) and
          (InterlockedDecrement(fOwner.fThreadClients.Count) >= 0) do
      fOwner.ThreadClientsConnect;
    {$ifdef USE_WINIOCP} // TWinIocp needs only atpReadPending threads
    while not Terminated and
          (fOwner.fSockets <> nil) and
          (fOwner.fSockets.fIocpRecvSend <> nil) do
    begin
      sub := fOwner.fSockets.fIocpRecvSend.GetNext(INFINITE, e, bytes);
      if sub = nil then
        break; // Terminated
      case e of
        wieRecv:
          begin
            if sub^.Tag <> 0 then // not needed outside IOCP
              TPollAsyncConnection(sub^.Tag).BeforeProcessRead;
            SetRes(notif, sub^.Tag, [pseRead]);
            fOwner.fSockets.ProcessRead(self, notif);
          end;
        wieSend:
          // writes are done in the single (and main) fOwner.Execute thread
          // -> just relay this event to the IOCP queue handling acceptex()
          // process, i.e. TAsyncServer.DoExecute
          fOwner.fIocpAccept.Enqueue(sub, e, bytes);
        wieConnect: // from THttpAsyncClientConnections.StartRequest
          begin
            SetRes(notif, sub^.Tag, [pseWrite]);
            fOwner.fSockets.ProcessWrite(notif, 0);
          end;
      end;
    end;
    {$else}
    // compute the best delay depending on the socket layer
    ms := 1000; // epoll is asynchronous
    case fProcess of
      atpReadSingle:
        if fOwner.fSocketsEpoll then
          ms := 100; // for quick shutdown
      atpReadPoll:
        if not fOwner.fSocketsEpoll then
          ms := 10; // let WaitForModified(ms) quickly react to subscriptions
    end;
    // main TAsyncConnections read/write process
    while not Terminated and
          (fOwner.fSockets <> nil) and
          (fOwner.fSockets.fRead <> nil) do
      case fProcess of
        atpReadSingle:
          // a single thread to rule them all: polling, reading and processing
          if fOwner.fSockets.fRead.GetOne(ms, fProcessName, notif) then
            if not Terminated then
              fOwner.fSockets.ProcessRead(self, notif);
        atpReadPoll:
          // main thread will just fill pending events from socket polls
          // (no process because a faulty service would delay all reading)
          begin
            fWaitForReadPending := false;
            new := fOwner.fSockets.fRead.PollForPendingEvents(ms);
            if Terminated then
              break;
            if (new = 0) and
               (fOwner.fSockets.fRead.fPending.Count <> 0) then
              new := 1; // wake up one thread if some reads are still pending
            fEvent.ResetEvent;
            fWaitForReadPending := true; // to be set before wakeup
            if new <> 0 then
              fOwner.ThreadPollingWakeup(new);
            // wait for the sub-threads to wake up this one
            if Terminated then
              break;
            if (fEvent.IsEventFD and
                (fOwner.fThreadPollingAwakeCount > 2)) or
               ((fOwner.fSockets.fRead.fPending.Count = 0) and
                (fOwner.fSockets.fRead.Count = 0)) then
              // 1) avoid poll(eventfd) syscall on heavy loaded server
              // 2) there is no connection any more: wait for next accept
            begin
              fWaitForReadPending := true; // better safe than sorry
              fEvent.WaitForEver;
            end
            else
            begin
              // always release current thread to avoid CPU burning
              // (any condition makes stability and performance worse)
              fWaitForReadPending := true;
              fEvent.WaitFor(1);
            end;
          end;
        atpReadPending:
          // secondary threads wait, then read and process pending events
          begin
            fEvent.ResetEvent;
            fWaitForReadPending := true; // to be set just before WaitForEver
            fEvent.WaitForEver;
            if Terminated then
              break;
            LockedInc32(@fOwner.fThreadPollingAwakeCount);
            fWakeUpFromSlowProcess := false;
            while GetNextRead(notif) do
              fOwner.fSockets.ProcessRead(self, notif);
            fThreadPollingLastWakeUpTix := 0; // will now need to wakeup
            LockedDec32(@fOwner.fThreadPollingAwakeCount);
            fOwner.fThreadReadPoll.ReleaseEvent; // atpReadPoll lock above
          end;
      else
        EAsyncConnections.RaiseUtf8('%.Execute: unexpected fProcess=%',
          [self, ord(fProcess)]);
      end;
    {$endif USE_WINIOCP}
    fOwner.DoLog(sllInfo, 'Execute: done %', [fProcessName], self);
  except
    on E: Exception do
      if fOwner <> nil then
        fOwner.DoLog(sllWarning, 'Execute raised a % -> terminate % thread %',
          [PClass(E)^, fOwner.fConnectionClass, fProcessName], self);
  end;
  fExecuteState := esFinished;
end;


{ TAsyncConnections }

constructor TAsyncConnections.Create(const OnStart, OnStop: TOnNotifyThread;
  aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
  aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
var
  i: PtrInt;
  tix32: cardinal;
  opt: TPollAsyncSocketsOptions;
  {%H-}log: ISynLog;
begin
  aLog.EnterLocal(log, 'Create(%,%,%)',
    [aConnectionClass, ProcessName, aThreadPoolCount], self);
  // setup connection class
  if (aConnectionClass = TAsyncConnection) or
     (aConnectionClass = nil) then
    EAsyncConnections.RaiseUtf8('Unexpected %.Create(%)',
      [self, aConnectionClass]);
  fConnectionClass := aConnectionClass;
  // setup thread pool parameters
  if aThreadPoolCount <= 0 then
    aThreadPoolCount := 1;
  {$ifndef USE_WINIOCP}
  fThreadPollingWakeupLoad :=
    (cardinal(aThreadPoolCount) div SystemInfo.dwNumberOfProcessors) * 8;
  if fThreadPollingWakeupLoad < 4 then
    fThreadPollingWakeupLoad := 4; // below 4, the whole algorithm seems pointless
  {$endif USE_WINIOCP}
  // setup internal variables
  fLastOperationReleaseMemorySeconds := 60;
  fLastOperationMS := mormot.core.os.GetTickCount64;
  fLastOperationSec := Qword(fLastOperationMS) div 1000; // ASAP
  fKeepConnectionInstanceMS := 100;
  SetLength(fGC1.Items, 512);
  SetLength(fGC2.Items, 512);
  // setup associated event-driven sockets polling
  opt := [];
  if acoWritePollOnly in aOptions then
    include(opt, paoWritePollOnly);
  fSockets := TAsyncConnectionsSockets.Create(opt, aThreadPoolCount);
  fSockets.fOwner := self;
  fSockets.OnStart := ProcessClientStart;
  // setup logs
  if acoVerboseLog in aOptions then
    if (aLog = nil) or
       not (sllTrace in aLog.Family.Level) then
      exclude(aOptions, acoVerboseLog); // no need to check verbose
  if Assigned(aLog) and
     (acoDebugReadWriteLog in aOptions) then
  begin
    fSockets.fDebugLog := aLog;
  {$ifdef USE_WINIOCP}
    fSockets.fIocpRecvSend.OnLog := aLog.DoLog;
  end;
  {$else}
    fSockets.fRead.OnLog := aLog.DoLog;
    fSockets.fWrite.OnLog := aLog.DoLog;
  end;
  fSockets.fWrite.OnGetOneIdle := ProcessIdleTix;
  {$endif USE_WINIOCP}
  // prepare this main thread: fThreads[] requires proper fOwner.OnStart/OnStop
  fOptions := aOptions;
  inherited Create({suspended=}false, OnStart, OnStop, aLog, ProcessName);
  // initiate the read/receive thread(s)
  fThreadPoolCount := aThreadPoolCount;
  SetLength(fThreads, fThreadPoolCount);
  {$ifdef USE_WINIOCP}
  for i := 0 to aThreadPoolCount - 1 do
    fThreads[i] := TAsyncConnectionsThread.Create(self, atpReadPending, i);
  // prepare an IOCP queue only for wieAccept of fServer.Sock
  fIocpAccept := TWinIocp.Create({processing=}1);
  fIocpAccept.OnLog := fSockets.fIocpRecvSend.OnLog;
  {$else}
  fSocketsEpoll := fSockets.fRead.PollClass.FollowEpoll;
  if aThreadPoolCount = 1 then
    fThreads[0] := TAsyncConnectionsThread.Create(self, atpReadSingle, 0)
  else
  begin
    fThreadReadPoll := TAsyncConnectionsThread.Create(self, atpReadPoll, 0);
    fThreads[0] := fThreadReadPoll;
    for i := 1 to aThreadPoolCount - 1 do
      fThreads[i] := TAsyncConnectionsThread.Create(self, atpReadPending, i);
  end;
  {$endif USE_WINIOCP}
  // wait for all threads to be started
  tix32 := GetTickSec + 7;
  repeat
     if AllThreadsStarted then
       break;
     SleepHiRes(1);
  until GetTickSec > tix32;
  // setup custom threads affinity
  if acoThreadCpuAffinity in aOptions then
    SetServerThreadsAffinityPerCpu(log, TThreadDynArray(fThreads))
  else if acoThreadSocketAffinity in aOptions then
    SetServerThreadsAffinityPerSocket(log, TThreadDynArray(fThreads));
  // caller will start the main thread
  if Assigned(log) then
    log.Log(sllTrace, 'Create: started % threads', [fThreadPoolCount + 1], self);
end;

function TAsyncConnections.AllThreadsStarted: boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to high(fThreads) do
    if fThreads[i].fExecuteState = esNotStarted then
      exit;
  result := true;
end;

{.$define GCVERBOSE} // help debugging

procedure TAsyncConnections.AddGC(aConnection: TPollAsyncConnection; const aContext: ShortString);
begin
  if Terminated or
     (aConnection = nil) or
     (ifInGC in aConnection.fInternalFlags) then
    exit;
  include(aConnection.fInternalFlags, ifInGC); // ensure AddGC() done once
  {$ifdef GCVERBOSE}
  if Assigned(fLogClass) then
    fLogClass.Add.Log(sllTrace, 'AddGC %', [aContext], aConnection);
  {$endif GCVERBOSE}
  (aConnection as TAsyncConnection).fLastOperation := fLastOperationMS; // in ms
  with fSockets.fWaitingWrite do
    if Count <> 0 then
      PtrArrayDelete(Items, aConnection, Safe, @Count);
  ObjArrayAdd(fGC1.Items, aConnection, fGC1.Safe, @fGC1.Count); // to 1st gen
end;

function OneGC(var gen, dst: TPollAsyncConnections; lastms, oldms: cardinal): PtrInt;
var
  c: PAsyncConnection;
  ms, n: cardinal;
  d: PtrInt;
begin
  result := 0;
  n := gen.Count;
  if n = 0 then
    exit;
  d := dst.Count;
  if oldms < 50 then
    oldms := 50; // bare minimum for GC#1
  oldms := lastms - oldms;
  c := pointer(gen.Items);
  repeat
    ms := c^.fLastOperation;
    if ms <= oldms then // AddGC() set c.fLastOperation as ms
    begin
      // move to next generation list after timeout
      if d = length(dst.Items) then
        SetLength(dst.Items, NextGrow(d));
      dst.Items[d] := c^;
      inc(d);
    end
    else
    begin
      if ms > lastms then
        // need to reset time flag every 49.7 days (32-bit unlikely overflow)
        c^.fLastOperation := lastms; // will wait twice the delay
      gen.Items[result] := c^; // keep
      inc(result);
    end;
    inc(c);
    dec(n);
  until n = 0;
  gen.Count := result; // don't resize gen.Items[] to avoid realloc
  dst.Count := d;
end;

procedure TAsyncConnections.DoGC;
var
  tofree: TPollAsyncConnections;
  n1, n2, h, tix1, tix2: integer;
  lastms: cardinal;
begin
  // refresh ticks markers
  tix1 := fLastOperationMS shr 5; // ProcessIdleTix() to call every 32 ms
  tix2 := tix1 shr 5;             // check GC#2 only every second
  lastms := fLastOperationMS;     // to force 32-bit unsigned value
  // retrieve the connection instances to be released
  if fGC2.Safe.TryLock then
  try
    n1 := 0;
    if fGC1.Count <> 0 then
      if fGC1.Safe.TryLock then
        // if we reached here, both GC#1 and GC#2 have been acquired
        try
          // keep just-closed-connections in GC#1 list for 100 ms by default
          n1 := OneGC(fGC1, fGC2, lastms, fKeepConnectionInstanceMS);
        finally
          fGC1.Safe.UnLock;
        end
      else
        exit; // won't block and retry if another thread is accessing GC#1
    fGCTix1 := tix1;
    if (fGCTix2 = tix2) or
       (fGC2.Count = 0) then
      exit;
    fGCTix2 := tix2;
    // keep instances 10 seconds in GC#2 until no pending accept() needs them
    tofree.Count := 0;
    SetLength(tofree.Items, NextGrow(fGC2.Count shr 1));
    n2 := OneGC(fGC2, tofree, lastms, 10000);
  finally
    fGC2.Safe.UnLock;
  end
  else
    exit; // won't block if another thread is accessing GC#2
  // notify the result of GC#1 and GC#2 collection
  h := n1 xor (n2 shl 16); // compute the current state of both GC
  if (h = fGCLast) and
     (tofree.Count = 0) then
    exit; // nothing new to report
  fGCLast := h;
  if Assigned(fLogClass) then
    fLogClass.Add.Log(sllTrace, 'DoGC #1=% #2=% free=% client=%',
      [n1, n2, tofree.Count, fSockets.Count], self);
//writeln('DoGC n1=', n1, ' n2=',n2, ' tofree=',tofree.Count);
  // actually release the deprecated connection instances
  if tofree.Count <> 0 then
    FreeGC(tofree);
end;

procedure TAsyncConnections.FreeGC(var conn: TPollAsyncConnections);
var
  i: PtrInt;
  c: TPollAsyncConnection;
begin
  c := nil; // make compiler happy
  i := conn.Count - 1;
  while i >= 0 do // don't use ObjArrayClear() to have verbose debug logging
    try
      while i >= 0 do
      begin
        c := conn.Items[i];
        {$ifdef GCVERBOSE}
        if Assigned(fLogClass) then
          fLogClass.Add.Log(sllTrace, 'DoGC #% %', [i, pointer(c)], self);
        {$endif GCVERBOSE}
        c.Free;
        dec(i);
      end;
    except
      on E: Exception do
      begin
        if Assigned(fLogClass) then
          fLogClass.Add.Log(sllWarning, 'FreeGC: %.Free failed as %',
            [pointer(c), PClass(E)^], self);
        dec(i); // just ignore this entry
      end;
    end;
  conn.Count := 0;
  conn.Items := nil;
end;

procedure TAsyncConnections.Shutdown;
var
  i, n: PtrInt;
  endtix: Int64;
begin
  {$ifdef USE_WINIOCP}
  fIocpAccept.Unsubscribe(fIocpAcceptSub);
  fIocpAccept.Terminate;
  {$endif USE_WINIOCP}
  Terminate;
  // terminate the main clients asynchronous logic
  if fSockets <> nil then
  begin
    DoLog(sllDebug,
      'Shutdown threads=% total=% reads=%/% writes=%/% now=% hi=% pending=%',
      [length(fThreads), fSockets.Total,
       fSockets.ReadCount, KB(fSockets.ReadBytes),
       fSockets.WriteCount, KB(fSockets.WriteBytes),
       fConnectionCount, fConnectionHigh, fSockets.Count], self);
    fSockets.Terminate(5000);
  end;
  // notify the SetOnIdle() registered events
  endtix := mormot.core.os.GetTickCount64;
  for i := 0 to high(fOnIdle) do
    fOnIdle[i]({Sender=}nil, endtix);
  // terminate and unlock background ProcessRead/ProcessWrite polling threads
  if fThreads <> nil then
  begin
    for i := 0 to high(fThreads) do
    begin
      fThreads[i].Terminate; // set the Terminated flag
      {$ifndef USE_WINIOCP}
      if fThreads[i].fEvent <> nil then
        fThreads[i].fEvent.SetEvent; // release any lock (e.g. atpReadPoll)
      {$endif USE_WINIOCP}
    end;
    endtix := endtix + 10000; // wait up to 10 seconds
    repeat
      SleepHiRes(1);
      n := 0;
      for i := 0 to high(fThreads) do
        with fThreads[i] do
          if fExecuteState = esRunning then
          begin
            DoLog(sllTrace, 'Shutdown unfinished=%', [fProcessName], self);
            inc(n);
          end;
    until (n = 0) or
          (mormot.core.os.GetTickCount64 > endtix);
    FreeAndNilSafe(fSockets); // FreeAndNil() sets nil before which is incorrect
    ObjArrayClear(fThreads, {continueonexception=}true);
  end;
  // there may be some trailing connection instances to be released
  FreeGC(fGC1);
  FreeGC(fGC2);
end;

destructor TAsyncConnections.Destroy;
begin
  Shutdown;
  inherited Destroy;
  {$ifdef USE_WINIOCP}
  FreeAndNil(fIocpAccept);
  {$endif USE_WINIOCP}
  if (acoNoConnectionTrack in fOptions) or
     (fConnectionCount = 0) then
    exit;
  // they are normally no working connection anymore: time to free memory
  if Assigned(fLogClass) then
    fLogClass.Add.Log(sllTrace, 'Destroy: connections=%', [fConnectionCount], self);
  ObjArrayClear(fConnection, {continueonexception=}true, @fConnectionCount);
end;

procedure TAsyncConnections.SetCpuAffinity(CpuIndex: integer);
var
  i: PtrInt;
begin
  SetThreadCpuAffinity(self, CpuIndex);
  for i := 0 to high(Threads) do
    SetThreadCpuAffinity(Threads[i], CpuIndex);
end;

procedure TAsyncConnections.SetSocketAffinity(SocketIndex: integer);
var
  i: PtrInt;
begin
  SetThreadSocketAffinity(self, SocketIndex);
  for i := 0 to high(Threads) do
    SetThreadSocketAffinity(Threads[i], SocketIndex);
end;

function TAsyncConnections.ThreadClientsConnect: TAsyncConnection;
var
  res: TNetResult;
  client: TNetSocket;
  addr: TNetAddr;
begin
  result := nil;
  if Terminated then
    exit;
  with fThreadClients do
    res := NewSocket(Address, Port, nlTcp, {bind=}false, Timeout, Timeout,
      Timeout, {retry=}0, client, @addr);
  if res = nrOk then
    res := client.MakeAsync;
  if res <> nrOK then
    EAsyncConnections.RaiseUtf8('%: %:% connection failure (%)',
      [self, fThreadClients.Address, fThreadClients.Port, _NR[res]]);
  // create and register the async connection as in TAsyncServer.Execute
  if not ConnectionCreate(client, addr, result) then
    client.ShutdownAndClose({rdwr=}false)
  else if not fSockets.Start(result) then
    FreeAndNil(result);
end;

// NOTICE on the acoThreadSmooting scheduling algorithm (genuine AFAICT)
// - in TAsyncConnectionsThread.Execute, the R0/atpReadPoll main thread calls
// PollForPendingEvents (e.g. the epoll API on Linux) then ThreadPollingWakeup()
// to process the socket reads in the R1..Rn/atpReadPending threads of the pool
// - the naive/standard/well-used algorithm of waking up the threads on need
// does not perform well, especially with a high number of threads: the
// global CPU usage remains idle, because most of the time is spent between
// the threads, and not processing actual data
// - acoThreadSmooting wake up the sub-threads only if it did not become idle
// within GetTickCount64 resolution (i.e. 16ms on Windows, 4ms on Linux)
// - on small load or quick response, only the R1 thread is involved
// - on slow process (e.g. DB access) or in case of high traffic, R1 is
// identified as blocking, and R2..Rmax threads are awaken in order
// - it seems to leverage the CPU performance especially when the number of
// threads is higher than the number of cores
// - this algorithm seems efficient, and simple enough to implement and debug,
// in respect to what I have seen in high-performance thread pools (e.g. in
// MariaDB), which have much bigger complexity (like a dynamic thread pool)
// - ThreadPollingWakeupLoad property defines how many fast processing events a
// thread is supposed to handle in its loop - default value is computed as
// (ThreadPoolCount / CpuCount) * 8 so should scale depending on the actual HW
// - on Linux, waking up threads is done via efficient blocking eventfd()
// - on Windows, TWinIocp will directly handle atpReadPending thread wakening

{$ifndef USE_WINIOCP}
function TAsyncConnections.ThreadPollingWakeup(Events: integer): PtrInt;
var
  i: PtrInt;
  th: PAsyncConnectionsThread;
  t: TAsyncConnectionsThread;
  c, tix: integer; // 32-bit is enough to check for
  ndx: TByteToByte; // wake up to 256 threads at once
begin
  if Events > high(ndx) then
    Events := high(ndx); // avoid ndx[] buffer overflow (parnoid)
  result := 0;
  tix := 0; // default is one thread per event (legacy algorithm)
  if acoThreadSmooting in fOptions then
  begin
    fThreadPollingLastWakeUpTix := mormot.core.os.GetTickCount64; // 16ms / 4ms
    if Events > 1 then
      // after accept() or on idle server, we always wake up one thread
      tix := fThreadPollingLastWakeUpTix;
  end;
  fThreadPollingWakeupSafe.Lock;
  try
    th := @fThreads[1]; // [0]=fThreadReadPoll and should not be set from here
    for i := 1 to length(fThreads) - 1 do
    begin
      t := th^;
      if tix = 0 then
      begin
        // exactly wake up one thread per needed event
        if t.fWaitForReadPending then
        begin
          // this thread is currently idle and can be activated
          t.fThreadPollingLastWakeUpCount := 0;
          t.fThreadPollingLastWakeUpTix := 0;
          t.fWaitForReadPending := false; // acquire this thread
          ndx[result] := i; // notify outside of fThreadPollingWakeupSafe lock
          inc(result);
          dec(Events);
        end;
      end
      // fast working threads handle up to fThreadPollingLastWakeUpCount events
      else if not t.fWaitForReadPending and
              (t.fThreadPollingLastWakeUpCount > 0) and
              (t.fThreadPollingLastWakeUpTix = tix) then
      begin
        // this thread is likely to be available very soon: consider it done
        c := t.fThreadPollingLastWakeUpCount;
        dec(t.fThreadPollingLastWakeUpCount, Events);
        dec(Events, c);
      end
      else if t.fWaitForReadPending then
      begin
        // we need to wake up a thread, since some slow work is going on
        t.fThreadPollingLastWakeUpTix := tix;
        t.fThreadPollingLastWakeUpCount := fThreadPollingWakeupLoad - Events;
        t.fWaitForReadPending := false; // acquire this thread
        ndx[result] := i;
        inc(result);
        dec(Events, fThreadPollingWakeupLoad);
      end;
      if Events <= 0 then
        break;
      inc(th);
    end;
  finally
    fThreadPollingWakeupSafe.UnLock;
  end;
  // notify threads outside fThreadPollingWakeupSafe
  for i := 0 to result - 1 do
    fThreads[ndx[i]].fEvent.SetEvent; // on Linux, uses eventfd()
end;
{$endif USE_WINIOCP}

procedure TAsyncConnections.DoLog(Level: TSynLogLevel; TextFmt: PUtf8Char;
  const TextArgs: array of const; Instance: TObject);
begin
  if (self <> nil) and
     Assigned(fLogClass) then
    fLogClass.Add.Log(Level, TextFmt, TextArgs, Instance);
end;

function TAsyncConnections.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: TNetAddr; out aConnection: TAsyncConnection): boolean;
var
  pool: PPollAsyncConnections;
begin
  // you can override this class then call ConnectionNew
  if Terminated then
    result := false
  else
  begin
    aConnection := nil;
    // first try to recycle 2nd gen instances e.g. for short-living HTTP/1.0
    pool := @fGC2;
    if (pool^.Count > 0) and
       pool^.Safe.TryLock then
    begin
      if pool^.Count > 0 then
      begin
        dec(pool^.Count);
        aConnection := TAsyncConnection(pool^.Items[pool^.Count]);
      end;
      pool^.Safe.UnLock;
    end;
    if aConnection = nil then
      // need to allocate and initialize a new instance
      aConnection := fConnectionClass.Create(self, aRemoteIp)
    else
      // reuse the existing instance of a closed connection
      aConnection.Recycle(aRemoteIP);
    result := ConnectionNew(aSocket, aConnection, {add=}false);
  end;
end;

function TAsyncConnections.ConnectionNew(aSocket: TNetSocket;
  aConnection: TAsyncConnection; aAddAndSubscribe: boolean): boolean;
begin
  result := false; // caller should release aSocket
  if Terminated then
    exit;
  aConnection.fSocket := aSocket;
  aConnection.fHandle := InterlockedIncrement(fLastHandle);
  while aConnection.fHandle < 0 do
  begin
    // paranoid sequence overflow (may appear after 4 years at 1000 conn/sec)
    LockedAdd32(PCardinal(@fLastHandle)^, $80000000); // thread-safe reset to 0
    aConnection.fHandle := InterlockedIncrement(fLastHandle);
  end;
  if acoNoConnectionTrack in fOptions then
  begin
    include(aConnection.fFlags, fInList);
    LockedInc32(@fConnectionCount);
  end
  else if {$ifndef USE_WINIOCP} (fThreadReadPoll = nil) or {$endif}
          aAddAndSubscribe then
    // ProcessClientStart() won't delay SuscribeConnection + RegisterConnection
    ConnectionAdd(aConnection);
  aConnection.AfterCreate; // Handle has been computed
  if acoVerboseLog in fOptions then
    DoLog(sllTrace, 'ConnectionNew % sock=% count=% gc=%',
      [aConnection, pointer(aSocket), fConnectionCount,
       ifFromGC in aConnection.fInternalFlags], self);
  result := true; // indicates aSocket owned by the pool
end;

function TAsyncConnections.LockedConnectionDelete(aConnection: TAsyncConnection;
  aIndex: integer): boolean;
var
  n: PtrInt;
  start: Int64;
begin
  // caller should have done fConnectionLock.WriteLock
  try
    if acoVerboseLog in fOptions then
      QueryPerformanceMicroSeconds(start);
    PtrArrayDelete(fConnection, aIndex, @fConnectionCount);
    n := fConnectionCount;
    if (n > 256) and
       (length(fConnection) > n shl 1) then
      SetLength(fConnection, n + n shr 3); // reduce 50% free into 12.5%
    if acoVerboseLog in fOptions then
      DoLog(sllTrace, 'ConnectionDelete % ndx=% count=% %',
        [aConnection, aIndex, n, MicroSecFrom(start)], self);
    aConnection.fSocket := nil;   // ensure is known as disabled
    AddGC(aConnection, 'LockedConnectionDelete'); // delayed released
    result := true;
  except
    result := false;
  end;
end;

procedure TAsyncConnections.ConnectionAdd(conn: TAsyncConnection);
var
  c: ^TPollAsyncConnection;
  i, n: PtrInt;
begin
  include(conn.fFlags, fInList); // mark as registered
  fConnectionLock.WriteLock;
  try
    n := fConnectionCount;
    if n = length(fConnection) then
      SetLength(fConnection, NextGrow(n));
    i := n - 1;
    c := @fConnection[i];
    while (i >= 0) and
          (c^.Handle >= conn.Handle) do
    begin
      dec(i); // not sorted Handle (on rare thread contention)
      dec(c); // the order problem is always at the end (no binary search need)
    end;
    inc(i);   // the index where to insert
    inc(c);
    if i < n then
      MoveFast(c^, PAnsiChar(c)[SizeOf(conn)], (n - i) * SizeOf(conn));
    c^ := conn;
    inc(n);
    if n > fConnectionHigh then
      fConnectionHigh := n;
    fConnectionCount := n;
  finally
    fConnectionLock.WriteUnLock;
  end;
end;

function TAsyncConnections.ConnectionDelete(
  aConnection: TPollAsyncConnection): boolean;
var
  i: integer; // integer, not PtrInt for ConnectionFindAndLock(@i)
  conn: TAsyncConnection;
begin
  // don't call fSockets.Stop() here - see ConnectionRemove()
  result := false;
  if Terminated or
     (aConnection = nil) or
     (aConnection.Handle <= 0) then
    exit;
  if not (fInList in aConnection.fFlags) then
  begin
    // this connection was not part of fConnection[] list nor subscribed
    // e.g. HTTP/1.0 short request
    // -> explicit GC - Free is unstable here
    AddGC(aConnection, 'ConnectionDelete');
    result := true;
    exit;
  end;
  exclude(aConnection.fFlags, fInList);
  conn := ConnectionFindAndLock(aConnection.Handle, cWrite, @i);
  if conn <> nil then
    try
      result := LockedConnectionDelete(conn, i);
    finally
      fConnectionLock.WriteUnLock;
    end;
  if not result then
    DoLog(sllWarning, 'ConnectionDelete(%)=false count=%',
      [aConnection.Handle, fConnectionCount], self); // should never happen
end;

function FastFindConnection(Conn: PPointerArray; R: PtrInt; H: integer): PtrInt;
var
  L, RR: PtrInt;
  C: integer;
begin
  // fast O(log(n)) binary search
  L := 0;
  if R >= 0 then
    repeat
      {$ifdef CPUX64}
      result := L + R;
      result := result shr 1;
      {$else}
      result := (L + R) shr 1;
      {$endif CPUX64}
      C := TPollAsyncConnection(Conn[result]).Handle;
      if C = H then
        exit;
      RR := result + 1; // compile as 2 branchless cmovg/cmovle on x86_64 FPC
      dec(result);
      if C < H then
        L := RR
      else
        R := result;
    until L > R;
  result := -1
end;

function SlowFindConnection(Conn: PPointerArray; R, H: integer): PtrInt;
begin
  // brute force variant to debug ConnectionDelete()=false
  if R >= 0 then
  begin
    inc(R);
    result := 0;
    repeat
      if TPollAsyncConnection(Conn[result]).Handle = H then
        exit;
      inc(result);
      dec(R);
    until R = 0;
  end;
  result := -1;
end;

function TAsyncConnections.ConnectionFindAndLock(
  aHandle: TConnectionAsyncHandle; aLock: TRWLockContext;
  aIndex: PInteger): TAsyncConnection;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     Terminated or
     (aHandle <= 0) then
    exit;
  if acoNoConnectionTrack in fOptions then
    EAsyncConnections.RaiseUtf8(
      'Unexpected %.ConnectionFindAndLock(%)', [self, aHandle]);
  fConnectionLock.Lock(aLock);
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    i := fConnectionCount - 1;
    if i >= 0 then
    begin
      if fConnection[i].Handle <> aHandle then // very short-living connection
      begin
        i := FastFindConnection(pointer(fConnection), i, aHandle); // O(log(n))
        {if i < 0 then
        begin
          i := SlowFindConnection(pointer(fConnection), i, aHandle); // O(n)
          if i >= 0 then
            DoLog(sllError, 'ConnectionFindAndLock(%) Slow<>Fast count=%',
              [aHandle, fConnectionCount], self); // should never happen
        end;}
      end;
      if i >= 0 then
      begin
        result := fConnection[i];
        if aIndex <> nil then
          aIndex^ := i;
      end;
      {if acoVerboseLog in fOptions then
        DoLog(sllTrace, 'ConnectionFindAndLock(%)=%', [aHandle, result], self);}
    end;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    if result = nil then
      fConnectionLock.UnLock(aLock);
  end;
end;

function TAsyncConnections.LockedConnectionSearch(
  Handle: TConnectionAsyncHandle): TAsyncConnection;
var
  i, n: PtrInt;
begin
  // caller should have made fConnectionLock.Lock()
  result := nil;
  if Handle <= 0 then
    exit;
  i := fLastConnectionFind;
  n := fConnectionCount;
  if (i >= n) or
     (fConnection[i].Handle <> Handle) then
    i := FastFindConnection(pointer(fConnection), n - 1, Handle); // O(log(n))
  if i >= 0 then
  begin
    fLastConnectionFind := i;
    result := fConnection[i];
    if result.IsClosed then
      result := nil; // too late
  end;
end;

function TAsyncConnections.ConnectionFind(
  Handle: TConnectionAsyncHandle): TAsyncConnection;
begin
  result := nil;
  if (self = nil) or
     Terminated or
     (Handle <= 0) or
     (acoNoConnectionTrack in fOptions) then
    exit;
  fConnectionLock.ReadOnlyLock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    result := LockedConnectionSearch(Handle); // O(log(n)) binary search
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fConnectionLock.ReadOnlyUnLock;
  end;
end;

function TAsyncConnections.ConnectionFindAndWaitLock(Handle: TConnectionAsyncHandle;
  LockWriter: boolean; WaitTimeoutMS: cardinal): pointer;
var
  endtix, tix: Int64;
  ms: integer;
begin
  result := nil;
  if (self = nil) or
     (Handle <= 0) or
     (acoNoConnectionTrack in fOptions) then
    exit;
  ms := 0;
  endtix := 0;
  while not Terminated do
  begin
    fConnectionLock.ReadOnlyLock;
    try
      result := LockedConnectionSearch(Handle); // O(log(n)) binary search
      if result = nil then
        exit; // too late, or invalid handle
      if TAsyncConnection(result).TryLock(LockWriter) then
        exit; // found and locked within main ReadOnlyLock
    finally
      fConnectionLock.ReadOnlyUnLock;
    end;
    // impossible to lock this connection: retry ASAP (same algo than WaitLock)
    result := nil;
    tix := mormot.core.os.GetTickCount64;
    if endtix = 0 then
      endtix := tix + WaitTimeoutMS // never wait forever
    else if tix >= endtix then
      exit;
    SleepHiRes(ms);
    ms := ms xor 1; // 0,1,0,1,0,1...
  end;
end;

function TAsyncConnections.ConnectionRemove(
  Handle: TConnectionAsyncHandle): boolean;
var
  i: integer; // integer, not PtrInt for ConnectionFindAndLock(@i)
  conn: TAsyncConnection;
begin
  result := false;
  if (self = nil) or
     Terminated or
     (Handle <= 0) then
    exit;
  conn := ConnectionFindAndLock(Handle, cWrite, @i);
  if conn <> nil then
    try
      if not fSockets.Stop(conn, 'ConnectionRemove') then
        DoLog(sllDebug, 'ConnectionRemove: Stop=false for %', [conn], self);
      result := LockedConnectionDelete(conn, i);
    finally
      fConnectionLock.WriteUnLock;
    end;
  if not result then
    DoLog(sllTrace, 'ConnectionRemove(%)=false', [Handle], self);
end;

procedure TAsyncConnections.EndConnection(connection: TAsyncConnection);
begin
  if acoNoConnectionTrack in fOptions then
  begin
    connection.fSocket := nil;
    AddGC(connection, 'EndConnection'); // delayed released
    InterlockedDecrement(fConnectionCount);
  end
  else
    ConnectionDelete(connection);
end;

function TAsyncConnections.Write(connection: TAsyncConnection; data: pointer;
  datalen: integer; timeout: integer): boolean;
begin
  if Terminated then
    result := false
  else
    result := fSockets.Write(connection, data, datalen, timeout);
end;

function TAsyncConnections.WriteString(connection: TAsyncConnection;
  const data: RawByteString; timeout: integer): boolean;
begin
  if Terminated then
    result := false
  else
    result := fSockets.WriteString(connection, data, timeout);
end;

procedure TAsyncConnections.LogVerbose(connection: TPollAsyncConnection;
  const ident: RawUtf8; const identargs: array of const;
  const data: TRawByteStringBuffer);
begin
  if acoVerboseLog in Options then
    fLogClass.Add.LogEscape(
      sllTrace, ident, identargs, data.Buffer, data.Len, connection);
end;

procedure TAsyncConnections.IdleEverySecond;
var
  i, notified, gced: PtrInt;
  idle: array of TAsyncConnection;
  idles: integer;
  sec, allowed, gc: cardinal;
  c: TAsyncConnection;
  start: Int64;
begin
  if Terminated or
     (fConnectionCount = 0) or
     (acoNoConnectionTrack in fOptions) then
    exit;
  // call TAsyncConnection.ReleaseMemoryOnIdle and OnLastOperationIdle events
  // and update TAsyncConnection.fLastOperation when needed
  if acoVerboseLog in fOptions then
    QueryPerformanceMicroSeconds(start);
  idles := 0;
  notified := 0;
  gced := 0;
  sec := fLastOperationSec; // 32-bit second resolution is fine
  allowed := fLastOperationIdleSeconds;
  if allowed <> 0 then
    allowed := sec - allowed;
  gc := fLastOperationReleaseMemorySeconds;
  if gc <> 0 then
    gc := sec - gc;
  fConnectionLock.ReadOnlyLock; // non-blocking quick process
  try
    for i := 0 to fConnectionCount - 1 do
    begin
      c := fConnection[i];
      if fWasActive in c.fFlags then
      begin
        // update fLastOperation flag once per second is good enough
        exclude(c.fFlags, fWasActive);
        c.fLastOperation := sec;
      end
      else // inactive connection
      begin
        // check if some working memory could be released
        if (gc <> 0) and
           (c.fLastOperation < gc) then
          inc(gced, c.ReleaseMemoryOnIdle); // quick non virtual method
        // check if some events should be triggerred
        // e.g. TWebSocketAsyncConnection would send ping/pong heartbeats
        if (allowed <> 0) and
           (c.fLastOperation < allowed) then
          ObjArrayAddCount(idle, c, idles); // calls below, outside the lock
        if Terminated then
          break;
      end;
    end;
  finally
    fConnectionLock.ReadOnlyUnLock;
  end;
  // OnLastOperationIdle should be called outside of fConnectionLock because if
  // Write() fails, it calls ConnectionDelete() and its WriteLock
  for i := 0 to idles - 1 do
    try
      c := idle[i];
      if c.OnLastOperationIdle(sec) then
        inc(notified); // e.g. a TWebSocketAsyncConnection ping was sent
      if Terminated then
        break;
    except
      // this overriden method should fail silently
    end;
  if (acoVerboseLog in fOptions) and
     ((notified <> 0) or
      (gced <> 0)) then
    DoLog(sllTrace, 'IdleEverySecond % idle=% notif=% gc=% %',
      [fConnectionClass, idles, notified, KBNoSpace(gced),
       MicroSecFrom(start)], self);
end;

procedure TAsyncConnections.ProcessIdleTix(Sender: TObject; NowTix: Int64);
var
  ms32: integer; // change at most every 32ms
  sec: TAsyncConnectionSec;
  i: PtrInt;
begin
  // called from fSockets.fWrite.OnGetOneIdle callback at most every 500ms
  if Terminated or
     (fLastOperationMS = NowTix) then
    exit;
  try
    ms32 := NowTix shr 5;
    // process pending soWaitWrite
    if (fSockets <> nil) and
       (fSockets.fWaitingWrite.Count <> 0) and
       (fLastOperationMS shr 5 <> ms32) then
    begin
      fSockets.ProcessWaitingWrite;
      if Terminated then
        exit;
      NowTix := mormot.core.os.GetTickCount64; // may have changed
    end;
    // update internal cache to avoid GetTickCount64 syscall
    fLastOperationMS := NowTix;
    // perform connection GC
    if (fGCTix1 <> ms32) and
       (fGC1.Count + fGC2.Count <> 0) then
      DoGC;
    if Terminated then
      exit;
    // notify IdleEverySecond
    sec := Qword(NowTix) div 1000; // when 32-bit second resolution is fine
    if sec <> fLastOperationSec then
    begin
      if sec < fLastOperationSec then // should append once every 136 years :)
        DoLog(sllError, 'ProcessIdleTix 32-bit overflow: %<%',
          [sec, fLastOperationSec], self);
      fLastOperationSec := sec;
      IdleEverySecond;
    end;
    // notify the SetOnIdle() registered events
    if fOnIdle <> nil then
      for i := 0 to length(fOnIdle) - 1 do
        if Terminated then
          exit
        else
          fOnIdle[i](Sender, NowTix); // any exception is cathed below
  except // any exception from here is fatal for the whole server process
    on E: Exception do
      DoLog(sllWarning, 'ProcessIdleTix catched %', [E], self);
  end;
  // note: this method should be non-blocking and return quickly
  // e.g. overriden in TWebSocketAsyncConnections to send pending frames, or
  // start a TLoggedWorkThread in THttpServerSocketGeneric.RefreshBlackListUri
end;

procedure TAsyncConnections.SetOnIdle(
  const aOnIdle: TOnPollSocketsIdle; Remove: boolean);
begin
  MultiEventSet(fOnIdle, TMethod(aOnIdle), Remove);
end;

{$ifdef USE_WINIOCP}

function TAsyncConnections.ProcessClientStart(Sender: TPollAsyncConnection): boolean;
begin
  result := false; // Subscribe() is done by TPollAsyncSockets.Start caller
end;

{$else}

function TAsyncConnections.ProcessClientStart(Sender: TPollAsyncConnection): boolean;
begin
  if fThreadReadPoll <> nil then
  begin
    // initial accept() will be directly redirected to atpReadPending threads
    // with no initial fRead.SubScribe() to speed up e.g. HTTP/1.0
    fSockets.fRead.AddOnePending(TPollSocketTag(Sender), [pseRead],
      {aSearchExisting=} false{ifFromGC in Sender.fInternalFlags});
    ThreadPollingWakeup(1);
    result := true; // no Subscribe() -> delayed in atpReadPending if needed
  end
  else
    result := false; // Subscribe() is done by TPollAsyncSockets.Start caller
end;

{$endif USE_WINIOCP}


{ TAsyncServer }

constructor TAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; aConnectionClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fSockPort := aPort;
  fMaxConnections := 7777777; // huge number for sure
  fMaxPending := 10000;       // fair enough for pending requests
  fProcessName := Join(['=AW:', ProcessName]); // for DoExecute main thread
  inherited Create(OnStart, OnStop, aConnectionClass, ProcessName, aLog,
    aOptions, aThreadPoolCount);
  // binding will be done in Execute
end;

procedure TAsyncServer.WaitStarted(seconds: integer);
var
  tix: Int64;
begin
  if self = nil then
    EAsyncConnections.RaiseUtf8(
      'TAsyncServer.WaitStarted(%) with self=nil', [seconds]);
  tix := mormot.core.os.GetTickCount64 + seconds * 1000; // never wait forever
  repeat
    if Terminated then
      exit;
    case fExecuteState of
      esRunning:
        exit;
      esFinished:
        EAsyncConnections.RaiseUtf8('%.Execute aborted as %',
          [self, fExecuteMessage]);
    end; // loop during esNotStarted / esBinding states
    SleepHiRes(1); // warning: waits typically 1-15 ms on Windows
    if mormot.core.os.GetTickCount64 > tix then
      EAsyncConnections.RaiseUtf8(
        '%.WaitStarted % timeout after % seconds',
          [self, ToText(fExecuteState)^, seconds]);
  until false;
end;

{$ifdef USE_WINIOCP}
procedure TAsyncServer.Shutdown;
begin
  inherited Shutdown;
  if fServer <> nil then
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
end;
{$else}
procedure TAsyncServer.Shutdown;
var
  i: PtrInt;
  len: integer; // should be integer
  touchandgo: TNetSocket; // paranoid ensure Accept() is released
  ev: TNetEvents;
  host, port: RawUtf8;
begin
  Terminate;
  for i := 0 to high(fThreads) do
    with fThreads[i] do
      if not Terminated then
      begin
        Terminate;
        if fEvent <> nil then
          fEvent.SetEvent;
      end;
  if fServer.SockIsDefined then
  begin
    host := fServer.Server; // will also work for nlUnix
    if fServer.SocketLayer <> nlUnix then
    begin
      if host = '0.0.0.0' then
        host := '127.0.0.1';
      port := fSockPort;
    end;
    DoLog(sllDebug, 'Shutdown %:% accept release request', [host, port], self);
    if NewSocket(host, port{%H-}, fServer.SocketLayer, false,
         10, 0, 0, 0, touchandgo) = nrOk then
    begin
      if fSocketsEpoll then
      begin
        len := 1;
        touchandgo.Send(@len, len);    // release epoll_wait() in R0 thread
        ev := touchandgo.WaitFor(100, [neRead, neError]);
        DoLog(sllTrace, 'Shutdown epoll WaitFor=%', [byte(ev)], self);
        SleepHiRes(1);
      end;
      touchandgo.ShutdownAndClose(false);  // release the AW thread
    end;
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
  end;
  inherited Shutdown;
end;
{$endif USE_WINIOCP}

destructor TAsyncServer.Destroy;
var
  endtix: Int64;
begin
  endtix := mormot.core.os.GetTickCount64 + 10000; // never wait forever
  Shutdown;
  //DoLog(sllTrace, 'Destroy before inherited', [], self);
  inherited Destroy;
  if fExecuteState = esRunning then
  begin
    DoLog(sllTrace, 'Destroy before sleep', [], self);
    repeat
      SleepHiRes(1); // wait for Execute to be finalized (unlikely)
    until (fExecuteState <> esRunning) or
          (mormot.core.os.GetTickCount64 > endtix);
    DoLog(sllTrace, 'Destroy waited until %', [ToText(fExecuteState)^], self);
  end;
  FreeAndNilSafe(fServer);
  FreeAndNil(fBanned);
  DoLog(sllTrace, 'Destroy finished', [], self);
end;

procedure TAsyncServer.OnFirstReadDoTls(Sender: TPollAsyncConnection);
begin
  // slow TLS processs is done from ProcessRead in a sub-thread
  if (fServer = nil) or
     (Sender.fSecure <> nil) then // paranoid
    EAsyncConnections.RaiseUtf8('Unexpected %.OnFirstReadDoTls', [self]);
  if not fServer.TLS.Enabled then  // if not already done in WaitStarted()
  begin
    fGC1.Safe.Lock; // load certificates once from first connected thread
    try
      fServer.DoTlsAfter(cstaBind);  // validate certificates now
    finally
      fGC1.Safe.UnLock;
    end;
  end;
  // TAsyncServer.Execute made Accept(async=false) from acoEnableTls
  Sender.fSecure := NewNetTls;  // should work since DoTlsAfter() was fine
  Sender.fSecure.AfterAccept(Sender.fSocket, fServer.TLS, nil, nil);
  Sender.fSocket.MakeAsync;     // as expected by our asynchronous code
  if acoVerboseLog in fOptions then
    DoLog(sllTrace, 'AfterAccept % %',
      [Sender.fSocket, Sender.fSecure.GetCipherName], Sender);
end;

procedure TAsyncServer.SetExecuteState(State: THttpServerExecuteState);
begin
  fExecuteState := State;
  DoLog(sllInfo, 'Execute: State=%', [ToText(State)^], self);
end;

{$ifdef USE_WINIOCP}
  {.$define IOCP_ACCEPT_PREALLOCATE_SOCKETS}
  // was reported to be more stable/scaling by some experts, but not our tests
{$endif USE_WINIOCP}

procedure TAsyncServer.DoExecute;
var
  {$ifdef USE_WINIOCP}
  sub: PWinIocpSubscription;
  s: TNetSocket;
  e: TWinIocpEvent;
  {$ifdef IOCP_ACCEPT_PREALLOCATE_SOCKETS}
  sockets: TNetSocketDynArray;
  socketsalloc: PtrInt;
  {$endif IOCP_ACCEPT_PREALLOCATE_SOCKETS}
  {$else}
  // in select/poll/epoll mode, this thread may do accept or accept+write
  async: boolean;
  {$endif USE_WINIOCP}
  res: TNetResult;
  notif: TPollSocketResult;
  client: TNetSocket;
  connection: TAsyncConnection;
  start: Int64;
  bytes: cardinal;
  len: integer; // should be integer
  sin: TNetAddr;
begin
  // Accept() incoming connections
  // and Send() output packets in the background if fExecuteAcceptOnly=false
  try
    // create and bind fServer to the expected TCP port
    SetExecuteState(esBinding);
    if acoEnableTls in fOptions then
      fSockets.OnFirstRead := OnFirstReadDoTls;
    // BIND + LISTEN (TLS is done later)
    fServer := TCrtSocket.Create(5000);
    if fLogClass <> nil  then
      fServer.OnLog := fLogClass.DoLog;
    fServer.BindPort(fSockPort, nlTcp, acoReusePort in fOptions);
    if not fServer.SockIsDefined then // paranoid check
      EAsyncConnections.RaiseUtf8('%.Execute: bind % failed', [self, fSockPort]);
    {$ifdef USE_WINIOCP}
    fIocpAcceptSub := fIocpAccept.Subscribe(fServer.Sock, 0);
    if not fIocpAccept.PrepareNext('first', fIocpAcceptSub, wieAccept) then
      RaiseLastError('TAsyncServer.Execute: acceptex', EWinIocp);
    {$ifdef IOCP_ACCEPT_PREALLOCATE_SOCKETS}
    sockets := NewRawSockets(fServer.SocketFamily, nlTcp, 10000);
    socketsalloc := 0; // we have pre-allocated 10000 sockets
    {$endif IOCP_ACCEPT_PREALLOCATE_SOCKETS}
    {$else}
    async := false; // at least first Accept() will be blocking
    if not fExecuteAcceptOnly then
      // setup the main bound connection to be polling together with the writes
      if fSockets.fWrite.Subscribe(fServer.Sock, [pseRead], {tag=}0) then
        fSockets.fWrite.PollForPendingEvents(0) // actually subscribe
      else
        EAsyncConnections.RaiseUtf8('%.Execute: accept subscribe', [self]);
    {$endif USE_WINIOCP}
    // main socket accept/send processing loop
    SetExecuteState(esRunning);
    start := 0;
    while not Terminated do
    begin
      PQWord(@notif)^ := 0; // direct blocking accept() by default
      {$ifdef USE_WINIOCP}
      sub := fIocpAccept.GetNext(INFINITE, e, bytes);
      if sub = nil then
        break; // terminated
      res := nrFatalError;
      case e of
        wieAccept:
          if fIocpAccept.GetNextAccept(sub, client, sin) then
          begin
            if acoEnableTls in fOptions then
              res := nrOk
            else
              res := client.MakeAsync;
            if res = nrOk then
            begin
              s := nil; // allocate in PrepareNext()
              {$ifdef IOCP_ACCEPT_PREALLOCATE_SOCKETS}
              if socketsalloc <= high(sockets) then
              begin
                s := sockets[socketsalloc]; // provide one pre-allocated socket
                inc(socketsalloc);
              end;
              {$endif IOCP_ACCEPT_PREALLOCATE_SOCKETS}
              if not fIocpAccept.PrepareNext('next', sub, wieAccept, nil, 0, s) then
                res := nrFatalError;
            end;
          end;
        wieSend:
          begin
            // not from a socket, but relayed from TAsyncConnectionsThread.Execute
            SetRes(notif, sub^.Tag, [pseWrite]); // ProcessWrite() in this thread
            res := nrOk;
          end;
      end;
      if e = wieAccept then
      begin
      {$else}
      bytes := 0; // only set with IOCP (wieSend)
      if async and
         not fSockets.fWrite.GetOne(1000, 'AW', notif) then
        continue;
      if ResToTag(notif) = 0 then // no tag = main accept()
      begin
        // could we Accept one or several incoming connection(s)?
        {DoLog(sllCustom1, 'Execute: before accepted=%', [fAccepted], self);}
        res := fServer.Sock.Accept(client, sin, // = accept4() on Linux
          {async=}not (acoEnableTls in fOptions)); // see OnFirstReadDoTls
      {$endif USE_WINIOCP}
        {DoLog(sllTrace, 'Execute: Accept(%)=% sock=% #% hi=%', [fServer.Port,
          _NR[res], pointer(client), fAccepted, fConnectionHigh], self);}
        // first check if the server was shut down
        if Terminated then
        begin
          {$ifndef USE_WINIOCP}
          // specific behavior from Shutdown method
          if fSocketsEpoll and
             (res = nrOK) then
          begin
            DoLog(sllDebug, 'Execute: Accept(%) release', [fServer.Port], self);
            // background subscribe to release epoll_wait() in R0 thread
            fSockets.fRead.Subscribe(client, [pseRead], {tag=}0);
            len := 1;
            client.Send(@len, len); // release touchandgo.WaitFor
          end;
          {$endif USE_WINIOCP}
          break;
        end;
        // check if fServer.Sock.Accept() did return with a socket, or a timeout
        {if res = nrRetry then
          DoLog(sllCustom1, 'Execute: Accept(%) retry', [fServer.Port], self);}
        if res = nrRetry then // timeout
          continue;
        // check if the remote IP is banned
        if fBanned.IsBanned(sin) then // IP filtering from blacklist
        begin
          if acoVerboseLog in fOptions then
            DoLog(sllTrace, 'Execute: ban=%', [CardinalToHexShort(sin.IP4)], self);
          len := ord(HTTP_BANIP_RESPONSE[0]);
          client.Send(@HTTP_BANIP_RESPONSE[1], len); // 418 I'm a teapot
          client.ShutdownAndClose({rdwr=}false);    // reject before TLS setup
          continue;
        end;
        // ensure we don't have too many connections on this server instance
        {$ifdef USE_WINIOCP}
        if fSockets.fIocpRecvSend.Count > fMaxConnections then
        {$else}
        if (fSockets.fRead.Count > fMaxConnections) or
           (fSockets.fRead.PendingCount > fMaxPending) then
           // map THttpAsyncServer.HttpQueueLength property value
        {$endif USE_WINIOCP}
        begin
          client.ShutdownAndClose({rdwr=}false); // e.g. for load balancing
          res := nrTooManyConnections;
        end;
        // handle any socket error in fServer.Sock.Accept()
        if Terminated then
          break;
        if res <> nrOK then
        begin
          // failure (too many clients?) -> wait and retry
          DoLog(sllDebug, 'Execute: Accept(%) failed as %',
            [fServer.Port, _NR[res]], self);
          // progressive wait on socket error, including nrTooManyConnections
          SleepStep(start);
          continue;
        end;
        // if we reached here, we have accepted a connection -> process
        inc(fAccepted);
        start := 0; // reset sleep pace if no error
        if ConnectionCreate(client, sin, connection) then
        begin
          // no log here, because already done in ConnectionNew and Start()
          // may do connection.Free in atpReadPending background -> log before
          if fSockets.Start(connection) then
          begin
            {$ifndef USE_WINIOCP}
            if (not async) and
               not fExecuteAcceptOnly then
            begin
              fServer.Sock.MakeAsync; // share thread with Writes
              async := true;
            end;
            {$endif USE_WINIOCP}
          end
          else if connection <> nil then // connection=nil for custom list
            ConnectionDelete(connection);
        end
        else
          client.ShutdownAndClose({rdwr=}false);
      end
      else
        // this was a pseWrite (wieSend) notification -> try send pending data
        // here connection = TObject(notif.tag)
        // - never executed if fExecuteAcceptOnly=true (POSIX THttpAsyncServer)
        fSockets.ProcessWrite(notif, bytes);
    end;
  except
    on E: Exception do
    begin
      // callback exceptions should all be catched: so we assume that any
      // exception in mORMot code should be considered as fatal
      FormatUtf8('% [%]', [E, E.Message], fExecuteMessage);
      DoLog(sllWarning, 'Execute raised uncatched % -> terminate %',
        [PClass(E)^, fProcessName], self);
    end;
  end;
  DoLog(sllInfo, 'Execute: done AW %', [fProcessName], self);
  SetExecuteState(esFinished);
end;



{ TAsyncClient }

constructor TAsyncClient.Create(const aServer, aPort: RawUtf8;
  aClientsCount, aClientsTimeoutSecs: integer;
  const OnStart, OnStop: TOnNotifyThread; aConnectionClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fThreadClients.Count := aClientsCount;
  fThreadClients.Timeout := aClientsTimeoutSecs * 1000;
  fThreadClients.Address := aServer;
  fThreadClients.Port := aPort;
  fProcessName := Join(['=W:', ProcessName]); // for DoExecute main thread
  inherited Create(OnStart, OnStop, aConnectionClass, ProcessName,
    aLog, aOptions, aThreadPoolCount);
end;

procedure TAsyncClient.DoExecute;
var
  notif: TPollSocketResult;
  bytes: cardinal;
  {$ifdef USE_WINIOCP}
  e: TWinIocpEvent;
  sub: PWinIocpSubscription;
  {$endif USE_WINIOCP}
begin
  try
    if fThreadClients.Count > 0 then
      while InterlockedDecrement(fThreadClients.Count) >= 0 do
        // will first connect some clients in this main thread
        ThreadClientsConnect;
    while not Terminated do
    begin
      {$ifdef USE_WINIOCP}
      sub := fIocpAccept.GetNext(INFINITE, e, bytes);
      if sub = nil then
        break; // terminated
      if e <> wieSend then
        continue;
      SetRes(notif, sub^.Tag, [pseWrite]);
      {$else}
      bytes := 0;
      if fSockets.fWrite.GetOne(1000, 'W', notif) then
      {$endif USE_WINIOCP}
        fSockets.ProcessWrite(notif, bytes);
    end;
    DoLog(sllInfo, 'Execute: done % C', [fProcessName], self);
  except
    on E: Exception do
      DoLog(sllWarning, 'Execute raised % -> terminate %',
        [PClass(E)^, fProcessName], self);
  end;
end;


{ ******************** THttpAsyncServer Event-Driven HTTP Server }

{ THttpAsyncConnection }

function THttpAsyncConnection.ReleaseReadMemoryOnIdle: PtrInt;
begin
  // caller made fRWSafe[0].TryLock
  result := inherited ReleaseReadMemoryOnIdle + // clean fRd memory
            fHttp.Head.Clear +
            fHttp.Process.Clear;
end;

procedure THttpAsyncConnection.OnAfterWriteSubscribe;
begin
  {$ifndef USE_WINIOCP}
  if fServer <> nil then
    if fOwner.fSockets.fWrite.SubscribeCount +
       fOwner.fSockets.fWrite.Count <= 1  then
     // release fExecuteEvent.WaitFor(msidle) in THttpAsyncServer
     fServer.fExecuteEvent.SetEvent;
  {$endif USE_WINIOCP}
end;


{ THttpAsyncClientConnection }

procedure THttpAsyncClientConnection.AfterCreate;
begin
  if fOwner.InheritsFrom(THttpAsyncConnections) then
    fServer := THttpAsyncConnections(fOwner).fAsyncServer;
  if (fServer <> nil) and
     (fServer.fCompressList.Algo <> nil) then
    fHttp.CompressList := @fServer.fCompressList;
  fHttp.ProcessInit; // ready to process this HTTP request
  fHttp.State := hrsConnect;
  // inherited AfterCreate; // void parent method
end;

procedure THttpAsyncClientConnection.BeforeDestroy;
begin
  fHttp.ProcessDone; // ContentStream.Free
  // inherited BeforeDestroy; // void parent method
end;

function THttpAsyncClientConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  st: TProcessParseLine;
  previous: THttpRequestState;
begin
  // cut-down version of THttpAsyncServerConnection.OnRead
  if (acoVerboseLog in fOwner.Options) and
     not (acoNoLogRead in fOwner.Options) then
   fOwner.fLogClass.Add.LogEscape(sllTrace,
     'OnRead %', [HTTP_STATE[fHttp.State]], fRd.Buffer, fRd.Len, self);
  result := soContinue;
  st.P := fRd.Buffer;
  st.Len := fRd.Len;
  repeat
    previous := fHttp.State;
    if not fHttp.ProcessRead(st, {returnOnStateChange=}true) then
      break; // not enough input
    if previous <> fHttp.State then
    begin
      result := NotifyStateChange(hcsReadStateChanged);
      if result <> soContinue then
        break;
    end;
    case fHttp.State of
      hrsGetBodyChunkedHexFirst,
      hrsGetBodyContentLengthFirst,
      hrsWaitProcessing:
        if not (nfHeadersParsed in fHttp.HeaderFlags) then
        begin
          // we just received command + all headers
          result := soClose;
          if not fHttp.ParseResponse(fResponseStatus) then
            break;
          fHttp.ParseHeaderFinalize;
          result := NotifyStateChange(hcsHeadersReceived);
          if result <> soContinue then
            break;
          if fHttp.State = hrsWaitProcessing then
            break;
          if HttpMethodWithNoBody(fHttp.CommandMethod) then
          begin
            // no body to wait for (e.g. HEAD request)
            fHttp.State := hrsWaitProcessing;
            break;
          end;
        end;
    end;
  until (fHttp.State = hrsWaitProcessing) or
        (fHttp.State >= hrsErrorPayloadTooLarge);
  if (fHttp.State = hrsWaitProcessing) and
     (result = soContinue) then
    result := NotifyStateChange(hcsFinished);
  if result = soContinue then
    exit;
  fOwner.DoLog(sllWarning, 'OnRead=%: state=%',
    [ToText(result)^, HTTP_STATE[fHttp.State]], self);
  NotifyStateChange(hcsFailed);
end;

function THttpAsyncClientConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
var
  data: PByte;
  datalen: integer; // warning: datalen should be defined as integer, not PtrInt
begin
  repeat // just to avoid a goto
    result := soClose; // e.g. unexpected state
    case fHttp.State of
      hrsConnect:
        begin
          exclude(fInternalFlags, ifWriteWait);
          // setup any TLS communication once connected (if needed)
          if Assigned(fSecure) then
            try
              result := NotifyStateChange(hcsBeforeTlsHandshake);
              if result <> soContinue then
                break;
              fSocket.MakeBlocking;
              fSecure.AfterConnection(fSocket, fTls, fHttp.Host);
              fSocket.MakeAsync;
              result := NotifyStateChange(hcsAfterTlsHandshake);
              if result <> soContinue then
                break;
            except
              break; // e.g. TLS handshake failure
            end;
          // compute the request command and headers
          fHttp.Head.Reset;
          fHttp.Head.Append([
            fHttp.CommandMethod, ' /', fHttp.CommandUri, ' HTTP/1.1'#13#10 +
            'Host: ', fHttp.Host, #13#10 +
            'Connection: Keep-Alive'#13#10]);
          if fHttp.Headers <> '' then // after PurgeHeaders(trim=true)
            fHttp.Head.Append([fHttp.Headers, #13#10]);
          fHttp.Head.Append(['User-Agent: ', fHttp.UserAgent, #13#10]);
          // no Content-Encoding Content-Length Content-Type support yet
          fHttp.Head.AppendCRLF; // end of header
          // allow customization
          fHttp.State := hrsSendHeaders;
          result := NotifyStateChange(hcsBeforeSendHeaders);
          if result <> soContinue then
            break;
          // synchronous sending of headers
          data := fHttp.Head.Buffer;
          datalen := fHttp.Head.Len;
          fHttp.Head.Reset;
          if fOwner.fSockets.RawWrite(self, data, datalen) and
             (datalen = 0) then
          begin
            // prepare for incoming response
            fHttp.Headers := '';
            fHttp.Options := [hroHeadersUnfiltered];
            fHttp.State := hrsGetCommand;
            result := NotifyStateChange(hcsAfterSendHeaders);
            if (result = soContinue) and
               fOwner.fSockets.SubscribeConnection('connect', self, pseRead) then
              result := soDone; // register read + caller will unregister write
          end;
        end;
    end;
    break;
  until true;
  if result = soDone then
    exit;
  fOwner.DoLog(sllWarning, 'AfterWrite=%: state=%',
    [ToText(result)^, HTTP_STATE[fHttp.State]], self);
  NotifyStateChange(hcsFailed);
end;

function THttpAsyncClientConnection.NotifyStateChange(
  state: TOnHttpClientState): TPollAsyncSocketOnReadWrite;
begin
  result := soContinue;
  if Assigned(fOnStateChanged) and
     (state in fOnStateChange) then
    try
      result := fOnStateChanged(state, self);
    except
      result := soClose;
    end;
end;


{ THttpAsyncClientConnections }

constructor THttpAsyncClientConnections.Create(aOwner: TAsyncConnections;
  aConnectionTimeoutSec: integer);
begin
  fOwner := aOwner;
  fConnectionTimeoutMS := aConnectionTimeoutSec * 1000;
  fUserAgent := DefaultUserAgent(self);
end;

function THttpAsyncClientConnections.StartRequest(var aUrl: TUri;
  const aMethod, aHeaders: RawUtf8; const aOnStateChanged: TOnHttpClientAsync;
  aTls: PNetTlsContext; const aDestFileName: TFileName;
  out aConnection: THttpAsyncClientConnection;
  aOnStateChange: TOnHttpClientStates): TNetResult;
var
  addr: TNetAddr;
  sock: TNetSocket;
  h: THandle;
  tag: TPollSocketTag absolute aConnection;
begin
  aConnection := nil;
  // validate the input parameters
  if (fOwner = nil) or
     not Assigned(aOnStateChanged) then
    result := nrNotImplemented
  else if (aMethod = '') or
          (aUrl.Server = '') then
    result := nrNotFound
  else
    result := addr.SetFrom(aUrl.Server, aUrl.Port, nlTcp);
  if result <> nrOk then
  begin
    fOwner.DoLog(sllDebug, 'StartRequest(% %/%)=%',
      [aMethod, aUrl.Server, aUrl.Address, ToText(result)^], self);
    exit;
  end;
  // create a new HttpAsyncClientConnection instance (and its socket)
  aConnection := THttpAsyncClientConnection.Create(fOwner, addr){%H-};
  try
    result := nrNoSocket;
    sock := addr.NewSocket(nlTcp);
    if sock = nil then
      exit;
    sock.MakeAsync;
    result := nrRefused;
    if not fOwner.ConnectionNew(sock, aConnection, {add=}true) then
      exit;
    if aDestFileName <> '' then
    begin
      fOwner.DoLog(sllTrace, 'StartRequest(% %/%) %',
        [aMethod, aUrl.Server, aUrl.Address, aDestFileName], self);
      h := FileCreate(aDestFileName);
      if not ValidHandle(h) then
      begin
        result := nrInvalidParameter;
        exit;
      end;
      aConnection.fHttp.ContentStream := // raise EOSException on invalid h
        TFileStreamEx.CreateFromHandle(h, aDestFileName);
      include(aConnection.fHttp.ResponseFlags, rfContentStreamNeedFree);
    end;
    aConnection.fHttp.CommandMethod := aMethod;
    aConnection.fHttp.CommandUri := aUrl.Address;
    aConnection.fHttp.UserAgent := fUserAgent;
    if aHeaders <> '' then
      aConnection.fHttp.Headers := PurgeHeaders(aHeaders, {trim=}true);
    aConnection.fHttp.Head.Reserve(2048); // prepare for 2KB headers
    if aUrl.Port = DEFAULT_PORT[aTls <> nil] then
      aConnection.fHttp.Host := aUrl.Server
    else
      Append(aConnection.fHttp.Host, [aUrl.Server, ':', aUrl.Port]);
    aConnection.fOnStateChange := aOnStateChange;
    aConnection.fOnStateChanged := aOnStateChanged;
    // optionally prepare for TLS
    result := nrNotImplemented;
    if (aTls <> nil) or
       aUrl.Https then
    begin
      if aTls <> nil then
        aConnection.fTls := aTls^;
      aConnection.fSecure := NewNetTls;
      if aConnection.fSecure = nil then
        exit;
    end;
    // start async events subscription and connection
    {$ifdef USE_WINIOCP}
    include(aConnection.fInternalFlags, ifWriteWait);
    if aConnection.fIocpSub = nil then
      aConnection.fIocpSub := fOwner.fIocpAccept.Subscribe(aConnection.fSocket, tag);
    if fOwner.fIocpAccept.PrepareNext('client', aConnection.fIocpSub, wieConnect) then
      result := nrOk;
    {$else}
    result := addr.SocketConnect(aConnection.fSocket, -1);
    if result <> nrOk then
      exit;
    if fOwner.fSockets.fWrite.Subscribe(aConnection.fSocket, [pseWrite], tag) then
      result := nrOk;
    {$endif USE_WINIOCP}
  finally
    if result <> nrOk then
    try
      aConnection.NotifyStateChange(hcsFailed);
      fOwner.DoLog(sllDebug, 'StartRequest(% %/%)=%',
        [aMethod, aUrl.Server, aUrl.Address, ToText(result)^], self);
    finally
      FreeAndNil(aConnection);
    end;
  end;
end;

procedure THttpAsyncClientConnections.Shutdown;
begin
end;


{ THttpAsyncServerConnection }

procedure THttpAsyncServerConnection.HttpInit;
begin
  fHttp.ProcessInit; // ready to process a new HTTP request
  fHeadersSec := 0;
  fBytesRecv := 0; // reset stats
  fBytesSend := 0;
end;

procedure THttpAsyncServerConnection.AfterCreate;
begin
  fServer := (fOwner as THttpAsyncConnections).fAsyncServer;
  fHttp.Interning := fServer.fInterning;
  if fServer.fCompressList.Algo <> nil then
    fHttp.CompressList := @fServer.fCompressList;
  fHttp.Options := fServer.fDefaultRequestOptions;
  if fServer.fServerKeepAliveTimeOutSec <> 0 then // 0 = no keep alive
    fKeepAliveMaxSec := fServer.Async.fLastOperationSec +
                        fServer.fServerKeepAliveTimeOutSec;
  if hsoEnablePipelining in fServer.Options then
    fPipelineState := [pEnabled];
  HttpInit;
  // inherited AfterCreate; // void parent method
end;

procedure THttpAsyncServerConnection.Recycle(const aRemoteIP: TNetAddr);
begin
  inherited Recycle(aRemoteIP);
  fHttp.Reset;
  if fServer <> nil then
  begin
    if fServer.fServerKeepAliveTimeOutSec <> 0 then
      fKeepAliveMaxSec := fServer.Async.fLastOperationSec +
                          fServer.fServerKeepAliveTimeOutSec;
    fPipelineState := fPipelineState * [pEnabled];
  end;
end;

function THttpAsyncServerConnection.GetConnectionOpaque: PHttpServerConnectionOpaque;
begin
  if fConnectionID = fHandle then
    result := @fConnectionOpaque
  else
    result := nil; // local fConnectionOpaque is clearly invalid
end;

procedure THttpAsyncServerConnection.BeforeDestroy;
begin
  if Assigned(fServer) and
     Assigned(fServer.fProgressiveRequests) and
     (rfProgressiveStatic in fHttp.ResponseFlags) then
    fServer.DoProgressiveRequestFree(fHttp);
  fHttp.ProcessDone; // ContentStream.Free
  FreeAndNil(fRequest);
  // inherited BeforeDestroy; // void parent method
end;

function THttpAsyncServerConnection.FlushPipelinedWrite: TPollAsyncSocketOnReadWrite;
var
  P: PByte;
  PLen: integer; // warning: PLen should be defined as integer, not PtrInt
begin
  result := soContinue;
  exclude(fPipelineState, pWrite);
  PLen := fWr.Len;
  if PLen = 0 then
    exit;
  P := fWr.Buffer;
  if not fOwner.fSockets.RawWrite(self, P, PLen) or
     (PLen <> 0) then // PLen<>0 if OS sending buffer is full
  begin
    fOwner.DoLog(sllWarning, 'OnRead: pipelined send error', [], self);
    result := soClose;
  end;
  fWr.Reset; // we could reuse the buffer
end;

procedure THttpAsyncServerConnection.OnClose;
begin
  inherited OnClose; // set fClosed flag
  if ifProcessing in fInternalFlags then
  begin
    exclude(fInternalFlags, ifProcessing); // if not properly done in AfterWrite
    if Assigned(fServer) then
      LockedDec32(@fServer.fCurrentProcess);
  end;
end;

procedure THttpAsyncServerConnection.BeforeProcessRead;
var
  endtix: Int64;
begin
  // ensure any previous request is actually finished (possible with IOCP only)
  if fHttp.State <> hrsSendBody then
    exit;
  // possible race condition of ProcessWrite() in a background thread
  // - when the client is making a lot of requests on the loopback (i.e. only
  // tests, not production) - should not appear with normal network latency
  endtix := mormot.core.os.GetTickCount64 + 50; // on Windows Sleep() maybe too quick
  repeat
    fOwner.DoLog(sllWarning, 'OnRead(%): wait for background W', [Socket], self);
    SleepHiRes(5); // may wait any time < 16ms
  until (fHttp.State <> hrsSendBody) or
        (mormot.core.os.GetTickCount64 > endtix);
end;

function THttpAsyncServerConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  st: TProcessParseLine;
  previous: THttpRequestState;
begin
  if (acoVerboseLog in fOwner.Options) and
     not (acoNoLogRead in fOwner.Options) then
    fOwner.fLogClass.Add.LogEscape(sllTrace,
      'OnRead %', [HTTP_STATE[fHttp.State]], fRd.Buffer, fRd.Len, self);
  result := soClose;
  if (fServer = nil) or
     (fOwner.fSockets = nil) then
    fHttp.State := hrsErrorMisuse
  else if fServer.fShutdownInProgress then
    fHttp.State := hrsErrorShutdownInProgress
  else
  begin
    previous := fHttp.State;
    // use the HTTP state machine to asynchronously parse fRd input
    result := soContinue;
    if fHttp.Head.Buffer = nil then
     fHttp.Head.Reserve(fServer.HeadersDefaultBufferSize); // preallocate once
    st.P := fRd.Buffer;
    st.Len := fRd.Len;
    // process one request (or several in case of pipelined input/output)
    while fHttp.ProcessRead(st, {returnOnStateChange=}false) do
    begin
      // handle main steps change
      case fHttp.State of
        hrsGetBodyChunkedHexFirst,
        hrsGetBodyContentLengthFirst:
          // received command + all headers
          if not (nfHeadersParsed in fHttp.HeaderFlags) then
            result := DoHeaders;
        hrsWaitProcessing:
          // received command + all headers + body (if any)
          begin
            // detect pipelined GET input
            if st.Len <> 0 then // there are still data in the input read buffer
              if (fPipelineState = [pEnabled]) and // no pWrite yet
                 (fKeepAliveMaxSec > 0) and
                 not (hfConnectionClose in fHttp.HeaderFlags) then
                // DoRequest should gather output in fWr
                include(fPipelineState, pWrite);
                // note: if hsoEnablePipelining is not set, will continue
            // = DoHeader (if needed) + call fServer.DoRequest() callback
            result := DoRequest;
          end
      else
        begin
          fOwner.DoLog(sllWarning, 'OnRead: close connection after % (before=%)',
            [HTTP_STATE[fHttp.State], HTTP_STATE[previous]], self);
          DoReject(HTTP_BADREQUEST);
          result := soClose;
        end;
      end;
      if pWrite in fPipelineState then
      begin
        if fWr.Len > 128 shl 10 then
          // flush when got more than 128KB of pending output
          if FlushPipelinedWrite <> soContinue then
            result := soClose;
        if (result <> soContinue) or
           (fHttp.State in [hrsWaitAsyncProcessing, hrsUpgraded]) then
          break; // rejected, async or upgraded
      end
      else if (result <> soContinue) or
              (fHttp.State in [hrsGetCommand, hrsWaitAsyncProcessing, hrsUpgraded]) then
        break; // rejected, authenticated, async or upgraded
      previous := fHttp.State;
    end;
    // no more available input
    if pWrite in fPipelineState then // time to flush the pipelined responses
       if FlushPipelinedWrite <> soContinue then
         result := soClose;
    if fHttp.State = hrsGetHeaders then
      if (fHeadersSec = 0) and
         (fServer.HeaderRetrieveAbortDelay >= 1000) then
        // start measuring time for receiving the headers
        fHeadersSec := fServer.Async.fLastOperationSec +
                       fServer.HeaderRetrieveAbortDelay div 1000
      else if (fHeadersSec > 0) and
              (fServer.Async.fLastOperationSec > fHeadersSec) then
      begin
        // 408 HTTP error after Server.HeaderRetrieveAbortDelay ms
        fOwner.DoLog(sllWarning, 'OnRead: Header TimeOut', [], self);
        DoReject(HTTP_TIMEOUT);
        fServer.IncStat(grTimeout);
        result := soClose;
      end
      else if fHttp.Head.Len > fServer.HeadersMaximumSize then
      begin
        // 413 HTTP error when headers > Server.HeadersMaximumSize
        fOwner.DoLog(sllWarning, 'OnRead: Head.Len=%', [fHttp.Head.Len], self);
        DoReject(HTTP_PAYLOADTOOLARGE);
        result := soClose;
      end;
    {fOwner.DoLog(sllCustom2, 'OnRead % result=%', [HTTP_STATE[fHttp.State], ord(result)], self);}
    // finalize the memory buffers
    if (st.Len = 0) or
       (result = soClose) then
      fRd.Reset // all input processed (usual and nominal case)
    else
      fRd.Remove(st.P - fRd.Buffer);  // keep remaining input for next time
  end;
end;

function THttpAsyncServerConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
var
  hrp: THttpRequestProcessBody;
begin
  result := soClose; // on error, shutdown and clear the connection
  if fOwner.fSockets = nil then
    fHttp.State := hrsErrorMisuse;
  // compute next step
  if fHttp.State = hrsSendBody then
  begin
    // use the HTTP state machine to fill fWr with outgoing body chunk
    hrp := fServer.DoProcessBody(fHttp, fWr, fOwner.fSockets.fSendBufferSize);
    if acoVerboseLog in fOwner.fOptions then
      fOwner.DoLog(sllTrace, 'AfterWrite ProcessBody=% ContentLength=% Wr=%',
        [ToText(hrp)^, fHttp.ContentLength, fWr.Len], self);
    case hrp of
      hrpSend: // background sending
        begin
          result := soContinue;
          exit;
        end;
      hrpWait: // not yet available (rfProgressiveStatic mode)
        begin
          result := soWaitWrite;
          exit;
        end;
    end; // hrpAbort, hrpDone will check hrsResponseDone
  end;
  // if we reached here, we are either finished or failed
  if Assigned(fServer.fProgressiveRequests) and
     (rfProgressiveStatic in fHttp.ResponseFlags) then
    fServer.DoProgressiveRequestFree(fHttp);
  fHttp.ProcessDone;   // ContentStream.Free
  fHttp.Process.Clear; // CompressContentAndFinalizeHead may have allocated it
  if ifProcessing in fInternalFlags then
  begin
    exclude(fInternalFlags, ifProcessing);
    LockedDec32(@fServer.fCurrentProcess);
  end;
  if Assigned(fServer.fOnAfterResponse) then
    DoAfterResponse;
  if fHttp.State <> hrsResponseDone then
  begin
    fOwner.DoLog(sllWarning, 'AfterWrite: unexpected %',
      [HTTP_STATE[fHttp.State]], self);
    exit; // return soClose
  end;
  // state = hrsResponseDone: whole headers (+ body) outgoing content were sent
  if acoVerboseLog in fOwner.fOptions then
    fOwner.DoLog(sllTrace, 'AfterWrite Done ContentLength=% Wr=% Flags=%',
      [fHttp.ContentLength, fWr.Len, ToText(fHttp.HeaderFlags)], self);
  if hfConnectionClose in fHttp.HeaderFlags then
    exit; // return soClose
  // kept alive connection -> reset the HTTP parser and continue
  fHttp.Reset;
  HttpInit;
  result := soContinue;
end;

function THttpAsyncServerConnection.DecodeHeaders: integer;
begin
  // compute the flags corresponding to this request
  fRequestFlags := HTTP_TLS_FLAGS[Assigned(fSecure)] +
                   HTTP_UPG_FLAGS[hfConnectionUpgrade in fHttp.HeaderFlags] +
                   HTTP_10_FLAGS[rfHttp10 in fHttp.ResponseFlags];
  // support optional Basic/Digest authentication
  if (hfHasAuthorization in fHttp.HeaderFlags) and
     (fServer.Authorize <> hraNone) then
  begin
    if fServer.Authorization(fHttp, fConnectionID) = asrMatch then
      include(fRequestFlags, hsrAuthorized)
    else if fServer.Async.fLastOperationSec shr 2 = fAuthRejectSec then
    begin
      // 403 HTTP error (and close connection) on wrong attemps within 4 seconds
      result := HTTP_FORBIDDEN;
      exit;
    end
    else
      // 401 HTTP_UNAUTHORIZED to ask for credentials and renew after 4 seconds
      // (ConnectionID may have changed in-between)
      fAuthRejectSec := fServer.Async.fLastOperationSec shr 2;
  end;
  // by default, continue the request process
  result := HTTP_SUCCESS;
  if (fServer.MaximumAllowedContentLength > 0) and
     (fHttp.ContentLength > fServer.MaximumAllowedContentLength) then
  begin
    // 413 HTTP error if requested payload is too big (default is 0 = no limit)
    result := HTTP_PAYLOADTOOLARGE;
    fServer.IncStat(grOversizedPayload);
  end
  else if (hsoRejectBotUserAgent in fServer.Options) and
          (fHttp.UserAgent <> '') and
          IsHttpUserAgentBot(fHttp.UserAgent) then
  begin
    // implement early hsoRejectBotUserAgent detection as 418 I'm a teapot
    result := HTTP_TEAPOT;
    fServer.IncStat(grRejected);
  end
  else if Assigned(fServer.OnBeforeBody) then
    // custom validation (e.g. missing/invalid URL or BearerToken)
    result := fServer.OnBeforeBody(
      fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers, fHttp.ContentType,
      fRemoteIP, fHttp.BearerToken, fHttp.ContentLength, fRequestFlags);
end;

function THttpAsyncServerConnection.DoReject(
  status: integer): TPollAsyncSocketOnReadWrite;
var
  len: integer; // should not be PtrInt
begin
  result := soClose;
  if status = HTTP_TEAPOT then // from hsoRejectBotUserAgent
  begin
    len := ord(HTTP_BANIP_RESPONSE[0]);
    Send(@HTTP_BANIP_RESPONSE[1], len); // 418 I'm a teapot
  end
  else
  begin
    if fServer.ComputeRejectBody(fHttp.Content, fConnectionID, status) then
      result := soContinue; // for grWwwAuthenticate
    len := length(fHttp.Content);
    Send(pointer(fHttp.Content), len); // no polling nor ProcessWrite
    if result = soContinue then
    begin
      fServer.IncStat(grWwwAuthenticate);
      fHttp.State := hrsResponseDone;
      result := AfterWrite;
      exit;
    end;
  end;
  fServer.IncStat(grRejected);
  fHttp.State := hrsErrorRejected;
  if (hsoBan40xIP in fServer.Options) and
     not IsUrlFavIcon(pointer(fHttp.CommandUri)) and
     fServer.Async.Banned.ShouldBan(status, fRemoteIP4) then
  begin
    if acoVerboseLog in fOwner.fOptions then
      fOwner.DoLog(sllTrace, 'DoReject(%): BanIP(%) %',
        [status, fRemoteIP, fServer.Async.Banned], self);
    fServer.IncStat(grBanned);
  end;
end;

function THttpAsyncServerConnection.DoHeaders: TPollAsyncSocketOnReadWrite;
var
  status: integer;
begin
  // finalize the headers
  result := soClose;
  if (nfHeadersParsed in fHttp.HeaderFlags) or
     not fHttp.ParseCommand then
    exit;
  fHttp.ParseHeaderFinalize;
  fConnectionID := fHandle; // ID is local socket handle by default
  fServer.ParseRemoteIPConnID(fHttp.Headers, fRemoteIP, fConnectionID);
  // immediate reject of clearly invalid requests
  status := DecodeHeaders; // may handle hfConnectionUpgrade when overriden
  if status <> HTTP_SUCCESS then
  begin
    // on fatal error (e.g. OnBeforeBody) direct reject and close the connection
    result := DoReject(status);
    exit;
  end;
  // now THttpAsyncServerConnection.OnRead can get the body
  fServer.IncStat(grHeaderReceived);
  if not (fHttp.State in [hrsWaitProcessing, hrsWaitAsyncProcessing, hrsUpgraded]) and
     // HEAD and OPTIONS are requests with Content-Length header but no body
     HttpMethodWithNoBody(fHttp.CommandMethod) then
  begin
    // implement Expect: 100-Continue Header
    if hfExpect100 in fHttp.HeaderFlags then
      // client waits for the server to parse the headers and return 100
      // before sending the request body
      fServer.fAsync.fSockets.WriteString(self, 'HTTP/1.1 100 Continue'#13#10#13#10);
    result := DoRequest;
  end
  else
    result := soContinue;
end;

function THttpAsyncServerConnection.DoRequest: TPollAsyncSocketOnReadWrite;
begin
  // check the status
  if nfHeadersParsed in fHttp.HeaderFlags then
    fServer.IncStat(grBodyReceived)
  else
    begin
      // content-length was 0, so hrsGetBody* and DoHeaders() were not called
      result := DoHeaders;
      if (result <> soContinue) or
         (fHttp.State in [hrsGetCommand, hrsUpgraded]) then
        exit; // rejected or upgraded to WebSockets
    end;
  // optionaly uncompress content
  if fHttp.ContentEncoding <> nil then
    fHttp.UncompressData;
  // prepare the HTTP/REST process reusing the THttpServerRequest instance
  if Assigned(fServer.OnAfterResponse) then
    QueryPerformanceMicroSeconds(fAfterResponseStart);
  result := soClose;
  if fRequest = nil then
    // created once, if not rejected by OnBeforeBody
    fRequest := fServer.fRequestClass.Create(fServer,
      fConnectionID, fReadThread, fHandle, fRequestFlags, GetConnectionOpaque)
  else
    fRequest.Recycle(
      fConnectionID, fReadThread, fHandle, fRequestFlags, GetConnectionOpaque);
  include(fInternalFlags, ifProcessing);
  LockedInc32(@fServer.fCurrentProcess);
  fRequest.Prepare(fHttp, fRemoteIP, fServer.fAuthorize);
  // let the associated THttpAsyncServer execute the request
  if fServer.DoRequest(fRequest) then
  begin
    result := soContinue;
    if fRequest.RespStatus = HTTP_ASYNCRESPONSE then
    begin
      // delayed response using fServer.AsyncResponse()
      include(fHttp.ResponseFlags, rfAsynchronous);
      fHttp.State := hrsWaitAsyncProcessing;
      exit;
    end;
  end;
  // handle HTTP/1.1 keep alive timeout
  if not (hfConnectionClose in fHttp.HeaderFlags) then
    if fKeepAliveMaxSec = 0 then // fServer.ServerKeepAliveTimeOut = 0
      include(fHttp.HeaderFlags, hfConnectionClose) // disable Keep-Alive
    else if fServer.Async.fLastOperationSec > fKeepAliveMaxSec then
    begin
      if acoVerboseLog in fOwner.fOptions then
        fOwner.DoLog(sllTrace, 'DoRequest KeepAlive=%ms timeout: close connnection',
          [fServer.ServerKeepAliveTimeOut], self);
      include(fHttp.HeaderFlags, hfConnectionClose); // before SetupResponse
    end;
  // trigger optional hsoBan40xIP temporary IP4 bans on unexpected request
  if (hsoBan40xIP in fServer.Options) and
     fServer.fAsync.Banned.ShouldBan(fRequest.RespStatus, fRemoteIP4) then
  begin
    fOwner.DoLog(sllTrace, 'DoRequest=%: BanIP(%) %',
      [fRequest.RespStatus, fRemoteIP, fServer.fAsync.Banned], self);
    fServer.IncStat(grBanned);
    include(fHttp.HeaderFlags, hfConnectionClose); // before SetupResponse
  end;
  // finalize the response and send it back to the client
  result := DoResponse(result);
end;

function THttpAsyncServerConnection.DoResponse(
  res: TPollAsyncSocketOnReadWrite): TPollAsyncSocketOnReadWrite;
var
  output: PRawByteStringBuffer;
  sent: integer; // warning: sent should be defined as integer, not PtrInt
  p: PByte;
begin
  result := res;
  // compute the response for the HTTP state machine
  output := fRequest.SetupResponse(fHttp, fServer.fCompressGz,
    fServer.fAsync.fSockets.fSendBufferSize);
  // SetupResponse() set fHttp.State as hrsSendBody or hrsResponseDone
  fRespStatus := fRequest.RespStatus;
  if pWrite in fPipelineState then
    // we are in HTTP pipelined mode: input stream had several requests
    if fHttp.State <> hrsResponseDone then
    begin
      fOwner.DoLog(sllWarning, 'DoRequest: pipelining with % streaming',
        [fHttp.CommandUri], self);
      if FlushPipelinedWrite = soContinue then // back to regular process
        fServer.fAsync.fSockets.Write(self, output.Buffer, output.Len, 1000)
      else
        result := soClose;
    end
    else
    begin
      fWr.Append(output.Buffer, output.Len);
      result := AfterWrite; // be ready for the next pipelined request
    end
  else // regular non-pipelined mode
    // now try socket send() with headers (and small body if hrsResponseDone)
    if fHttp.State = hrsResponseDone then
    begin
      // we can send the response in a single syscall
      p := output.Buffer;
      sent := output.Len;
      if not fServer.fAsync.fSockets.RawWrite(self, p, sent) then
        result := soClose
      else if sent <> 0 then // OS buffer was not big enough (paranoid)
      begin
        output.Remove(output.Len - sent);
        fServer.fAsync.fSockets.Write(self, output.Buffer, output.Len, 1000);
      end
      else
        result := AfterWrite; // be ready for the next request
    end
    else
      // let TPollAsyncSockets.ProcessWrite/subscribe process hrsSendBody
      // in the background, then call AfterWrite once finished
      fServer.fAsync.fSockets.Write(self, output.Buffer, output.Len, {timeout=}1000);
      // see THttpServer.Process() for the blocking equivalency of this async code
end;

procedure THttpAsyncServerConnection.DoAfterResponse;
var
  ctx: TOnHttpServerAfterResponseContext;
begin
  QueryPerformanceMicroSeconds(ctx.ElapsedMicroSec);
  dec(ctx.ElapsedMicroSec, fAfterResponseStart);
  ctx.Connection := fConnectionID;
  ctx.Method := pointer(fHttp.CommandMethod);
  ctx.Host := pointer(fHttp.Host);
  ctx.Url := pointer(fHttp.CommandUri);
  ctx.User := nil;
  if hsrAuthorized in fRequestFlags then
    ctx.User := pointer(fHttp.BearerToken); // see fServer.Authorization()
  ctx.Referer := pointer(fHttp.Referer);
  ctx.UserAgent := pointer(fHttp.UserAgent);
  ctx.RemoteIP := pointer(fRemoteIP);
  ctx.Flags := fRequestFlags;
  ctx.State := fHttp.State;
  ctx.StatusCode := fRespStatus;
  ctx.Received := fBytesRecv;
  ctx.Sent := fBytesSend;
  ctx.Tix64 := fServer.fAsync.fLastOperationMS; // ProcessIdleTix() GetTickCount64
  try
    fServer.fOnAfterResponse(ctx); // e.g. THttpLogger or THttpAnalyzer
  except
    on E: Exception do // paranoid
    begin
      fServer.fOnAfterResponse := nil; // won't try again
      fOwner.DoLog(sllWarning,
        'AfterWrite: OnAfterResponse raised % -> disabled', [PClass(E)^], self);
    end;
  end;
end;


{ THttpAsyncConnections }

procedure THttpAsyncConnections.DoExecute;
begin
  fExecuteAcceptOnly := true; // THttpAsyncServer.Execute will do POSIX writes
  inherited DoExecute;
end;

procedure THttpAsyncConnections.IdleEverySecond;
begin
  {ConsoleWrite('conn=% pending=% awake=%', [
    fConnectionCount, fSockets.fRead.fPending.Count, fThreadPollingAwakeCount]);}
  // GC of connection memory
  inherited IdleEverySecond;
  // high-level THttpAsyncServer process
  if fAsyncServer <> nil then
    fAsyncServer.IdleEverySecond;
  // reset the hsoBan40xIP items of the oldest list
  if (fBanned <> nil) and
     (fBanned.Count <> 0) then
    fBanned.DoRotate;
end;

procedure THttpAsyncConnections.SetExecuteState(State: THttpServerExecuteState);
begin
  if (State = esRunning) and
     (fAsyncServer <> nil) and
     (fServer <> nil) then
    fAsyncServer.fSock := fServer;
  inherited SetExecuteState(State);
end;


{ THttpAsyncServer }

constructor THttpAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions; aLog: TSynLogClass);
var
  aco: TAsyncConnectionsOptions;
begin
  fProcessName := ProcessName;
  if fProcessName = '' then
    fProcessName := aPort;
  fExecuteEvent := TSynEvent.Create;
  // initialize HTTP parsing
  fHeadersDefaultBufferSize := 2048; // one fpcx64mm small block
  fHeadersMaximumSize := 65535;
  // setup connections
  if hsoLogVerbose in ProcessOptions then
    aco := ASYNC_OPTION_VERBOSE // for server debugging
  else
    aco := ASYNC_OPTION_PROD;   // default is to log only errors/warnings
  if hsoHeadersInterning in ProcessOptions then
  begin
    fInterning := AllocMem(SizeOf(fInterning^));
    fInterning.Init;
  end;
  //include(aco, acoNoConnectionTrack);
  //include(aco, acoWritePollOnly);
  if hsoEnableTls in ProcessOptions then
    include(aco, acoEnableTls);
  if hsoThreadSmooting in ProcessOptions then
    include(aco, acoThreadSmooting)
  else // our thread smooting algorithm excludes CPU affinity
  begin 
    if hsoThreadCpuAffinity in ProcessOptions then
      include(aco, acoThreadCpuAffinity);
    if hsoThreadSocketAffinity in ProcessOptions then
      include(aco, acoThreadSocketAffinity);
  end;
  if hsoReusePort in ProcessOptions then
    include(aco, acoReusePort);
  if fConnectionClass = nil then
    fConnectionClass := THttpAsyncServerConnection;
  if fConnectionsClass = nil then
    fConnectionsClass := THttpAsyncConnections;
  if fRequestClass = nil then
    fRequestClass := THttpServerRequest; // may be overriden later
  // bind and start the actual thread-pooled connections async server
  fAsync := fConnectionsClass.Create(aPort, OnStart, OnStop,
    fConnectionClass, fProcessName, TSynLog, aco, ServerThreadPoolCount);
  fAsync.fAsyncServer := self;
  fAsync.fBanned := THttpAcceptBan.Create; // for hsoBan40xIP and BlackList
  // launch this TThread instance
  inherited Create(aPort, OnStart, OnStop, fProcessName,
    ServerThreadPoolCount, KeepAliveTimeOut, ProcessOptions, aLog);
end;

destructor THttpAsyncServer.Destroy;
begin
  // no more incoming request
  Shutdown;
  // abort pending async processes
  if fAsync <> nil then
    fAsync.Shutdown;
  if fClientSockets<> nil then
    fClientSockets.Shutdown;
  // terminate the Execute thread
  if fExecuteEvent <> nil then
    fExecuteEvent.SetEvent;
  inherited Destroy;
  // finalize all thread-pooled connections
  FreeAndNilSafe(fAsync);
  FreeAndNilSafe(fClientSockets);
  // release associated context
  if fInterning <> nil then
  begin
    Dispose(fInterning);
    fInterning := nil;
  end;
  FreeAndNil(fExecuteEvent);
end;

procedure THttpAsyncServer.AsyncResponse(Handle: TConnectionAsyncHandle;
  const Content, ContentType: RawUtf8; Status: cardinal);
var
  c: THttpAsyncServerConnection;
  res: TPollAsyncSocketOnReadWrite;
begin
  // thread-safe locate the connection using O(log(n)) Handle binary search
  c := fAsync.ConnectionFindAndWaitLock(Handle, {LockWrite=}false, {ms=}40);
  if c <> nil then
  // process within the read lock, since may respond before state is set
  try
    // verify expected connection state
    if (c.fHttp.State <> hrsWaitAsyncProcessing) or
       not (rfAsynchronous in c.fHttp.ResponseFlags) then // paranoid
    begin
      fAsync.DoLog(sllWarning, 'AsyncResponse(#%) failed state=%',
        [Handle, ToText(c.fHttp.State)^], self);
      exit; // will call c.UnLock()
    end;
    // finalize and send the response back to the client
    c.fRequest.RespStatus := Status;
    c.fRequest.OutContent := Content;
    c.fRequest.OutContentType := ContentType;
    if hfConnectionClose in c.fHttp.HeaderFlags then
      res := soClose
    else
      res := soContinue;
    if c.DoResponse(res) = soClose then
    begin
      if acoVerboseLog in fAsync.fOptions then
        fAsync.DoLog(sllTrace, 'final AsyncResponse: closing #%', [Handle], self);
      fAsync.fSockets.CloseConnection(TPollAsyncConnection(c), 'AsyncResponse');
    end;
  finally
    c.UnLock({LockWrite=}false); // unless CloseConnection set c := nil
  end
  else if acoVerboseLog in fAsync.fOptions then
    fAsync.DoLog(sllTrace, 'late AsyncResponse on closed #%', [Handle], self);
end;

function THttpAsyncServer.Clients: THttpAsyncClientConnections;
begin
  result := fClientSockets;
  if result <> nil then
    exit;
  fSafe.Lock;
  if fClientSockets = nil then
    fClientSockets := THttpAsyncClientConnections.Create(fAsync, {timeoutsec=}0);
  fSafe.UnLock;
  result := fClientSockets;
end;

function THttpAsyncServer.GetExecuteState: THttpServerExecuteState;
begin
  result := fAsync.fExecuteState; // state comes from THttpAsyncConnections
  fExecuteMessage := fAsync.fExecuteMessage; // copy message
end;

function THttpAsyncServer.GetBanned: THttpAcceptBan;
begin
  result := fAsync.fBanned;
end;

{$ifdef OSWINDOWS}
function THttpAsyncServer.GetApiVersion: RawUtf8;
begin
  {$ifdef USE_WINIOCP}
  result := 'WinIocp';
  {$else}
  result := 'WinSock';
  {$endif USE_WINIOCP}
end;
{$endif OSWINDOWS}

procedure THttpAsyncServer.IdleEverySecond;
var
  tix, cleaned: cardinal;
  T: TSynSystemTime;
  tmp: ShortString;
begin
  // no need to use the global HttpDateNowUtc and its GetTickCount64 API call
  if hsoIncludeDateHeader in fOptions then
  begin
    T.FromNowUtc;
    T.ToHttpDateShort(tmp, 'GMT'#13#10, 'Date: ');
    fHttpDateNowUtc := tmp; // (almost) atomic set within CPU L1 cache line
  end;
  // ensure log file(s) are flushed/consolidated if needed
  if fLogger <> nil then
    fLogger.OnIdle(fAsync.fLastOperationMS) // = ProcessIdleTix() GetTickCount64
  else if fAnalyzer <> nil then
    fAnalyzer.OnIdle(fAsync.fLastOperationMS);
  // clean interned HTTP headers at least every 16 secs
  if (fInterning <> nil) and
     (fAsync <> nil) then
  begin
    tix := fAsync.LastOperationSec shr 4;
    if (fInterning^.Count > 1000) or // is the slot highly used (DDos?)
       (fInterningTix <> tix) then
    begin
      cleaned := fInterning^.Clean(1);
      if (cleaned > 500) or
         ((cleaned <> 0) and
          (hsoLogVerbose in Options)) then
        fAsync.DoLog(sllTrace,
          'IdleEverySecond: cleaned % interned headers', [cleaned], self);
      fInterningTix := tix;
    end;
  end;
  // BlackListUri regular refresh support
  if (fBlackListUriNextTix <> 0) and
     (fAsync.LastOperationSec >= fBlackListUriNextTix) then
    RefreshBlackListUri(fAsync.LastOperationSec);
  {$ifdef OSPOSIX}
  if Assigned(fSspiKeyTab) and
     fSspiKeyTab.TryRefresh(fAsync.fLastOperationSec) then
    fAsync.DoLog(sllDebug, 'IdleEverySecond: refreshed %', [fSspiKeyTab], self);
  {$endif OSPOSIX}
end;

procedure THttpAsyncServer.AppendHttpDate(var Dest: TRawByteStringBuffer);
begin
  Dest.AppendShort(fHttpDateNowUtc); // set by IdleEverySecond
end;

function THttpAsyncServer.GetHttpQueueLength: cardinal;
begin
  result := fAsync.fMaxPending;
end;

procedure THttpAsyncServer.SetHttpQueueLength(aValue: cardinal);
begin
  fAsync.fMaxPending := aValue;
end;

function THttpAsyncServer.GetConnectionsActive: cardinal;
begin
  result := fAsync.ConnectionCount;
end;

procedure THttpAsyncServer.DoExecute;
var
  {$ifndef USE_WINIOCP}
  notif: TPollSocketResult;
  ms: integer;
  {$endif USE_WINIOCP}
  tix64: Int64;
  tix, lasttix: cardinal;
  msidle, mscallbacks: integer;
begin
  // call ProcessIdleTix - and POSIX Send() output packets in the background
  //SetCurrentThreadName('=M:%', [fAsync.fProcessName]);
  WaitStarted(10); // wait for fAsync.Execute to bind and start
  if fAsync <> nil then
    try
      fSock := fAsync.fServer;
      fAsync.DoLog(sllTrace, 'Execute: main loop', [], self);
      IdleEverySecond; // initialize idle process (e.g. fHttpDateNowUtc)
      tix := mormot.core.os.GetTickCount64 shr 16; // delay=500 after 1 min idle
      lasttix := tix;
      mscallbacks := 0;
      if fCallbackSendDelay <> nil then
        mscallbacks := fCallbackSendDelay^;
      {$ifndef USE_WINIOCP}
      ms := 1000; // fine if OnGetOneIdle is called in-between
      if fAsync.fSocketsEpoll then
        if mscallbacks <> 0 then
          ms := mscallbacks; // for WebSockets frame gathering
      {$endif USE_WINIOCP}
      while not Terminated and
            not fAsync.Terminated do
        {$ifndef USE_WINIOCP}
        if fAsync.fSockets.fWrite.SubscribeCount +
           fAsync.fSockets.fWrite.Count = 0  then
        {$endif USE_WINIOCP}
        begin
          // no socket/poll/epoll API nedeed (most common case)
          if (mscallbacks <> 0) and // typically = 10ms
             (tix = lasttix) then
            msidle := mscallbacks   // delayed SendFrames gathering
          else if (fAsync.fGC1.Count = 0) or
                  (fAsync.fKeepConnectionInstanceMS > 500 * 2) then
            msidle := 500 // idle server
          else // default fKeepConnectionInstanceMS is 100ms
            msidle := fAsync.fKeepConnectionInstanceMS shr 1; // follow GC pace
          fExecuteEvent.WaitFor(msidle);
          if fShutdownInProgress or
             Terminated or
             fAsync.Terminated then
            break;
          // periodic trigger of IdleEverySecond and ProcessIdleTixSendFrames
          tix64 := mormot.core.os.GetTickCount64;
          tix := tix64 shr 16; // check SendFrame idle after 1 minute (64K ms)
          fAsync.ProcessIdleTix(self, tix64);
          if (mscallbacks <> 0) and
             //TODO: set and check fCallbackOutgoingCount>0 instead?
             (fAsync.fConnectionCount <> 0) then
            lasttix := tix; // need mscallbacks for upgraded connections
        {$ifndef USE_WINIOCP}
        end
        else
        begin
          // some huge packets queued for async sending (less common)
          // note: fWrite.GetOne() calls ProcessIdleTix() while looping
          if fAsync.fSockets.fWrite.GetOne(ms, 'W', notif) then
            fAsync.fSockets.ProcessWrite(notif, 0);
          if mscallbacks <> 0 then
          begin
            tix := mormot.core.os.GetTickCount64 shr 16;
            lasttix := tix;
          end;
        {$endif USE_WINIOCP}
        end;
    except
      on E: Exception do
        // callback exceptions should all be catched: so we assume that any
        // exception in mORMot code should be considered as fatal
        fAsync.DoLog(sllWarning, 'Execute raised uncatched % -> terminate %',
          [PClass(E)^, fAsync.fProcessName], self);
    end;
  if fAsync = nil then
    exit;
  fAsync.DoLog(sllInfo, 'Execute: done W %', [fAsync.fProcessName], self);
end;


{ ******************** THttpProxyServer HTTP Proxy with Cache }

{ THttpProxyMem }

constructor THttpProxyMem.Create;
begin
  inherited Create;
  fMaxSize := -1; // use main THttpProxyServerSettings value
  fTimeoutSec := -1;
end;

function THttpProxyMem.FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
begin
  result := [];
  if (fForceCsv <> '') and
     fForce.Check(fForceCsv, uri, PathCaseInsensitive) then
    include(result, pckForce);
  if (fIgnoreCsv <> '') and
     fIgnore.Check(fIgnoreCsv, uri, PathCaseInsensitive) then
    include(result, pckIgnore);
end;


{ THttpProxyUrl }

constructor THttpProxyUrl.Create;
begin
  inherited Create;
  fMethods := [urmGet, urmHead];
  fOptions := [];
  fRemoteClientSafe.Init;
end;

destructor THttpProxyUrl.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fMemCached);
  FreeAndNil(fHashCached);
  fRemoteClientSafe.Done;
end;

function THttpProxyUrl.ReturnHash(ctxt: THttpServerRequestAbstract; h: THashAlgo;
  const name: RawUtf8; var fn: TFileName): integer;
var
  i: PtrInt;
  a: THashAlgo;
  hashes: TRawUtf8DynArray;
begin
  result := HTTP_NOTFOUND;
  if not (h in fAlgos) then
    exit;
  if not fHashCached.FindAndCopy(name, hashes) then
  begin
    // first time this hash is request: try to load and compute now
    i := length(fn);
    while i > 0 do
      if fn[i] = '.' then
        break // remove the .md5/.sha1/.sha256 file extension
      else
        dec(i);
    if i = 0 then
      exit;
    SetLength(fn, i - 1); // xxx.ext.md5 -> xxx.ext
    hashes := HashFileRaw(fn, fAlgos);
    if hashes = nil then
      exit; // no such file
    fHashCached.Add(name, hashes);
  end;
  // return the pre-computed hash of this file
  i := 0;
  for a := low(a) to high(a) do
    if a in fAlgos then
      if a = h then
      begin
        ctxt.OutContent := hashes[i];
        ctxt.OutContentType := TEXT_CONTENT_TYPE;
        result := HTTP_SUCCESS;
        exit;
      end
      else
        inc(i);
end;

function THttpProxyUrl.RemoteClientHead(const uri: TUri;
  var header: RawUtf8): cardinal;
begin
  fRemoteClientSafe.Lock;
  try
    if fRemoteClient = nil then
      fRemoteClient := TSimpleHttpClient.Create(hpoClientOnlySocket in fOptions);
    result := fRemoteClient.Request(uri, 'HEAD', '', '', '', {keepalive=}30000);
  finally
    fRemoteClientSafe.UnLock;
  end;
end;


{ THttpProxyServerMainSettings }

constructor THttpProxyServerMainSettings.Create;
begin
  inherited Create;
  fThreadCount := SystemInfo.dwNumberOfProcessors + 1;
  fPort := '8098';
end;

function THttpProxyServerMainSettings.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := (self <> nil) and
            ((fCertificateFile <> '') or
             (fCACertificatesFile <> ''));
  if not result then
    exit;
  if tls.PrivatePassword = '' then
    tls.PrivatePassword := fPrivateKeyPassword;
  InitNetTlsContext(tls, {server=}true, fCertificateFile,
    fPrivateKeyFile, tls.PrivatePassword, fCACertificatesFile);
end;


{ THttpProxyServerSettings }

constructor THttpProxyServerSettings.Create;
begin
  inherited Create;
  // set default values in this main instance
  fDiskCache.Path := Executable.ProgramFilePath + 'proxycache';
  fMemCache.MaxSize := 4096;
  fMemCache.TimeoutSec := 15 * SecsPerMin;
end;

function THttpProxyServerSettings.AddUrl(one: THttpProxyUrl): THttpProxyUrl;
begin
  result := one;
  if result <> nil then
    if result.Source = '' then
      FreeAndNil(result)
    else
      ObjArrayAdd(fUrl, result); // will be owned as fUri[]
end;

function THttpProxyServerSettings.AddFolder(const folder: TFileName;
  const uri: RawUtf8; RaiseExceptionOnNonExistingFolder: ExceptionClass): THttpProxyUrl;
begin
  if RaiseExceptionOnNonExistingFolder <> nil then
    if not DirectoryExists(folder) then
      raise RaiseExceptionOnNonExistingFolder.CreateFmt(
        '%s.AddFolder: %s does not exist', [ClassNameShort(self)^, folder]);
  result := THttpProxyUrl.Create;
  result.Url := uri;
  if RaiseExceptionOnNonExistingFolder = nil then
    RaiseExceptionOnNonExistingFolder := EHttpProxyServer;
  result.Source := StringToUtf8(EnsureDirectoryExists(
    folder, RaiseExceptionOnNonExistingFolder));
  result := AddUrl(result);
end;


{ THttpProxyServer }

constructor THttpProxyServer.Create(aSettings: THttpProxyServerSettings);
begin
  inherited Create; // call TSynAutoCreateFields
  fLog := TSynLog;
  if aSettings = nil then
  begin
    fSettings := THttpProxyServerSettings.Create;
    fSettingsOwned := true;
  end
  else
    fSettings := aSettings;
end;

destructor THttpProxyServer.Destroy;
begin
  if fServer <> nil then
    fServer.Shutdown; // set flag ASAP
  inherited Destroy;
  Stop;
  if fSettingsOwned then
    fSettings.Free;
  fSettings := nil; // notify background threads and event callbacks
  ObjArrayClear(fGC);
end;

procedure THttpProxyServer.Start(const aPrivateKeyPassword: SpiUtf8);
var
  {%H-}log: ISynLog;
  hso: THttpServerOptions;
  tls: TNetTlsContext;
  fav: RawByteString;
begin
  fLog.EnterLocal(log, 'Start %', [fSettings], self);
  if fServer <> nil then
    EHttpProxyServer.RaiseUtf8('Duplicated %.Start', [self]);
  // compute THttpAsyncServer options from settings
  hso := [hsoNoXPoweredHeader,
          hsoIncludeDateHeader,
          hsoThreadSmooting];
  if Assigned(log) and
     (psoLogVerbose in fSettings.Server.Options) then
    include(hso, hsoLogVerbose);
  if psoExcludeDateHeader in fSettings.Server.Options then
    exclude(hso, hsoIncludeDateHeader);
  if psoReusePort in fSettings.Server.Options then
    include(hso, hsoReusePort);
  if psoEnableLogging in fSettings.Server.Options then
    include(hso, hsoEnableLogging);
  if psoRejectBotUserAgent in fSettings.Server.Options then
    include(hso, hsoRejectBotUserAgent);
  if psoBan40xIP in fSettings.Server.Options then
    include(hso, hsoBan40xIP);
  tls.PrivatePassword := aPrivateKeyPassword; // if not in fSettings
  if (psoHttpsSelfSigned in fSettings.Server.Options) or
     SetupTls(tls) then
    include(hso, hsoEnableTls);
  // launch the HTTP(S) server
  fServer := THttpAsyncServer.Create(fSettings.Server.Port, nil, nil, '',
    fSettings.Server.ThreadCount, 30000, hso, fLog);
  if fSettings.Server.ServerName <> '' then
    fServer.ServerName := fSettings.Server.ServerName; // override 'mORMot (OS)'
  if fServer.Logger <> nil then
    fServer.Logger.Settings := fSettings.Server.Log; // override default
  fav := StringFromFile(fSettings.Server.FaviconFile);
  if fav = '' then
    fav := 'default';
  fServer.SetFavIcon(fav); // do once
  // setup the URI routes
  AfterServerStarted;
  // wait for actual server availability
  if hsoEnableTls in hso then
    if psoHttpsSelfSigned in fSettings.Server.Options then
      fServer.WaitStartedHttps
    else
      fServer.WaitStarted(30, @tls)
  else
    fServer.WaitStarted;
  if Assigned(log) then
    log.Log(sllDebug, 'Start: %', [fServer], self);
end;

procedure THttpProxyServer.Stop;
begin
  if fServer <> nil then
    with fLog.Enter('Stop %', [fServer], self) do
    begin
      fServer.Shutdown;
      FreeAndNil(fServer);
    end;
end;

function THttpProxyServer.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := fSettings.Server.SetupTls(tls); // can be overriden if needed
end;

procedure THttpProxyServer.AfterServerStarted;
var
  uri: RawUtf8;
  new, old: TUriRouter;
  one: THttpProxyUrl;
  i: PtrInt;
begin
  new := TUriRouter.Create(TUriTreeNode);
  try
    // compute all new routes from Settings[]
    for i := 0 to high(fSettings.Url) do
    begin
      one := fSettings.Url[i];
      FreeAndNil(one.fMemCached);
      FreeAndNil(one.fHashCached);
      if one.Disabled or
         (one.Source = '') then
        continue;
      // validate source as local file folder or remote http(s) server
      one.fSourced := sUndefined;
      if IsHttp(one.Source) then
      begin // detect also 'http://unix:/path/to/socket.sock:/url/path'
        if one.fRemoteUri.From(one.Source) then
          one.fSourced := sRemoteUri;
      end
      else
      begin
        Utf8ToFileName(one.Source, one.fLocalFolder);
        if DirectoryExists(one.fLocalFolder) then
        begin
          one.fLocalFolder := IncludeTrailingPathDelimiter(one.fLocalFolder);
          one.fSourced := sLocalFolder;
        end;
      end;
      if one.fSourced = sUndefined then
      begin
        fLog.Add.Log(sllWarning, 'AfterServerStarted: unexpected %', [one], self);
        continue;
      end;
      // normalize cache settings
      if not (psoDisableMemCache in fSettings.Server.Options) then
      begin
        if one.MemCache.MaxSize < 0 then
          one.MemCache.MaxSize := fSettings.MemCache.MaxSize;
        if one.MemCache.TimeoutSec < 0 then
          one.MemCache.TimeoutSec := fSettings.MemCache.TimeoutSec;
        if (one.MemCache.MaxSize > 0) and
           (one.MemCache.TimeoutSec > 0) then
          one.fMemCached := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
            TypeInfo(TRawByteStringDynArray), PathCaseInsensitive,
            one.MemCache.TimeoutSec);
        if one.fSourced = sRemoteUri then
        begin
          if one.DiskCache.MaxSize < 0 then
            one.DiskCache.MaxSize := fSettings.DiskCache.MaxSize;
          if one.DiskCache.Path = '' then
            one.DiskCache.Path := fSettings.DiskCache.Path;
          if one.DiskCache.TimeoutSec <= 0 then
            one.DiskCache.TimeoutSec := fSettings.DiskCache.TimeoutSec;
        end;
      end;
      // prepare optional hash cache
      one.fAlgos := [];
      if hpoPublishMd5 in one.Options then
        include(one.fAlgos, hfMd5);
      if hpoPublishSha1 in one.Options then
        include(one.fAlgos, hfSha1);
      if hpoPublishSha256 in one.Options then
        include(one.fAlgos, hfSha256);
      if one.fAlgos <> [] then
        one.fHashCached := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
          TypeInfo(TRawUtf8DynArrayDynArray), PathCaseInsensitive, SecsPerHour);
      // compute and register this URI
      uri := one.fUrl;
      while (uri <> '') and
            (uri[length(uri)] = '/') do
         SetLength(uri, length(uri) - 1);
      if (uri <> '') and
         (uri[1] <> '/') then
        insert('/', uri, 1);
      if uri <> '' then
        new.Run(one.Methods, uri, OnExecute, one);
      new.Run(one.Methods, uri + '/', OnExecute, one);
      new.Run(one.Methods, uri + '/*', OnExecute, one);
      fLog.Add.Log(sllDebug, 'AfterServerStarted: register % URI from %%',
        [uri, one.fLocalFolder, one.fRemoteUri.URI], self);
    end;
    // replace existing routes at once
    old := fServer.ReplaceRoute(new);
    new := nil; // is owned by fServer from now on
    if old <> nil then
      ObjArrayAdd(fGC, old); // late release at shutdown
  finally
    new.Free;
  end;
end;

function THttpProxyServer.OnGetHead(Ctxt: THttpServerRequestAbstract;
  Def: THttpProxyUrl; Met: TUriRouterMethod; const Uri: TUriMatchName): cardinal;
var
  fn: TFileName;
  name: RawUtf8;
  cached: RawByteString;
  tix64, siz: Int64;
  ext: PUtf8Char;
  pck: THttpProxyCacheKind;
begin
  // delete any deprecated cached content
  tix64 := fServer.Async.LastOperationMS; // set by ProcessIdleTix()
  Def.fMemCached.DeleteDeprecated(tix64);
  Def.fHashCached.DeleteDeprecated(tix64);
  // supplied URI should be a safe resource reference
  result := HTTP_FORBIDDEN; // 403
  // local the resource from its source
  if not SafePathNameU(name) then
    exit;
  case Def.fSourced of
    sLocalFolder:
      begin
        // try to assign a local file to the output Ctxt
        UrlDecodeVar(Uri.Path.Text, Uri.Path.Len, name, {space=}'+');
        NormalizeFileNameU(name);
        if hpoNoSubFolder in Def.Options then
          if PosExChar(PathDelim, name) <> 0 then
            exit;
        fn := MakeString([Def.fLocalFolder, name]);
        result := Ctxt.SetOutFile(fn, not(hpoDisable304 in Def.Options), '',
          Def.CacheControlMaxAgeSec, @siz); // to be streamed from file
      end;
    sRemoteUri:
      begin

      end;
  else
    exit;
  end;
  // complete the actual URI process
  case result of
    HTTP_SUCCESS:
      // this local file does exist: try if we could use Def.MemCache
      if Assigned(Def.fMemCached) then
      begin
        pck := Def.MemCache.FromUri(Uri);
        if not (pckIgnore in pck) then
          if (pckForce in pck) or
             (siz <= Def.MemCache.MaxSize) then
          begin
            // use a memory cache
            if not Def.fMemCached.FindAndCopy(name, cached) then
            begin
              cached := StringFromFile(fn);
              Def.fMemCached.Add(name, cached);
            end;
            Ctxt.ExtractOutContentType; // reverse Ctxt.SetOutFile(fn)
            Ctxt.OutContent := cached;
          end;
      end;
    HTTP_NOTFOUND:
      // this URI is no file, but may be a folder
      if (siz < 0) and // siz=-1 for folder
         not (hpoNoFolderHtmlIndex in Def.Options) then
      begin
        // return the folder files info as cached HTML
        if (hpoDisableFolderHtmlIndexCache in Def.Options) or
           not Def.fMemCached.FindAndCopy(name, cached) then
        begin
          FolderHtmlIndex(fn, Ctxt.Url,
            StringReplaceChars(name, PathDelim, '/'),
            RawUtf8(cached), hpoNoSubFolder in Def.Options);
          if Assigned(Def.fMemCached) and
             not (hpoDisableFolderHtmlIndexCache in Def.Options) then
            Def.fMemCached.Add(name, cached);
        end;
        result := Ctxt.SetOutContent(cached,
                    not(hpoDisable304 in Def.Options), HTML_CONTENT_TYPE);
      end
      else if siz = 0 then
        // check URI for any .md5/.sha1/.sha256 hash extension
        if Assigned(Def.fHashCached) then
        begin
          ext := ExtractExtP(name, {withoutdot:}true);
          if ext <> nil then
            case PCardinal(ext)^ of
              ord('m') + ord('d') shl 8 + ord('5') shl 16:
                result := Def.ReturnHash(Ctxt, hfMd5, name, fn);
              ord('s') + ord('h') shl 8 + ord('a') shl 16 + ord('1') shl 24:
                result := Def.ReturnHash(Ctxt, hfSHA1, name, fn);
              ord('s') + ord('h') shl 8 + ord('a') shl 16 + ord('2') shl 24:
                result := Def.ReturnHash(Ctxt, hfSHA256, name, fn);
            end;
        end;
  end; // may be e.g. HTTP_NOTMODIFIED (304)
  fLog.Add.Log(sllTrace, 'OnExecute: % % fn=% status=% size=% cached=%',
    [Ctxt.Method, Ctxt.Url, fn, result, siz, (cached <> '')], self);
end;

function THttpProxyServer.OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
var
  one: THttpProxyUrl;
  uri: TUriMatchName;
  met: TUriRouterMethod;
begin
  result := HTTP_NOTFOUND; // 404 by default
  // retrieve O(1) execution context
  one := Ctxt.RouteOpaque;
  if (one = nil) or
     one.Disabled or
     (one.fSourced = sUndefined) then
    exit;
  // validate the request method
  if not (UriMethod(Ctxt.Method, met) and
          (met in one.Methods)) then
  begin
    result := HTTP_NOTALLOWED; // 405 Method Not Allowed
    exit;
  end;
  // retrieve path and resource/file name from URI
  Ctxt.RouteAt(0, uri.Path);
  if uri.Path.Len > 512 then // obviously invalid
    exit;
  uri.ParsePath; // compute uri.Name for file-level TUriMatch
  // ensure was not marked as rejected
  if (one.RejectCsv <> '') and
     one.fReject.Check(one.RejectCsv, uri, PathCaseInsensitive) then
  begin
    result := HTTP_FORBIDDEN; // 403
    exit;
  end;
  // actual request processing
  case met of
    urmGet,
    urmHead:
      result := OnGetHead(Ctxt, one, met, uri);
    urmPost,
    urmPut,
    urmDelete:
      if one.fSourced <> sRemoteUri then
        result := HTTP_NOTALLOWED // 405 Method Not Allowed
      else
      { TODO: implement proxy with POST/PUT/DELETE }
        ;
  end;
end;



initialization
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(
    TypeInfo(THttpProxyUrlObjArray), THttpProxyUrl);
  {$endif HASDYNARRAYTYPE}

end.

