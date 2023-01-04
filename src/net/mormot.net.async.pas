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

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.zip,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.server; // for multi-threaded process


{ ******************** Low-Level Non-blocking Connections }

type
  /// 32-bit integer value used to identify an asynchronous connection
  // - will start from 1, and increase during the TAsyncConnections live-time
  TPollAsyncConnectionHandle = type integer;

  /// a dynamic array of TPollAsyncConnectionHandle identifiers
  TPollAsyncConnectionHandleDynArray = array of TPollAsyncConnectionHandle;

  /// let TPollAsyncSockets.OnRead/AfterWrite shutdown the socket if needed
  TPollAsyncSocketOnReadWrite = (
    soContinue,
    soClose
  );

  /// low-level flags used by the state machine about one TPollAsyncConnection
  // - fWasActive is set by TAsyncConnections.IdleEverySecond to purge rd/wr
  // unused buffers, to avoid calling GetTickCount64 for every activity
  // - fClosed is set by OnClose virtual method
  // - fFirstRead is set once TPollAsyncSockets.OnFirstRead is called
  // - fSubRead/fSubWrite flags are set when Subscribe() has been called
  // - fInList indicates that the connection was Added to the list
  // - fReadPending states that there is a pending event for this connection
  // - fFromGC is set when the connection has been recycled from the GC list
  TPollAsyncConnectionFlags = set of (
    fWasActive,
    fClosed,
    fFirstRead,
    fSubRead,
    fSubWrite,
    fInList,
    fReadPending,
    fFromGC
  );

  /// abstract parent to store information aboout one TPollAsyncSockets connection
  TPollAsyncConnection = class(TSynPersistent)
  protected
    /// the associated TCP connection
    // - equals nil after TPollAsyncSockets.Stop
    fSocket: TNetSocket;
    /// the associated 32-bit sequence number
    // - equals 0 after TPollAsyncSockets.Stop
    fHandle: TPollAsyncConnectionHandle;
    /// false for a single lock (default), true to separate read/write locks
    fLockMax: boolean;
    /// low-level flags used by the state machine about this connection
    fFlags: TPollAsyncConnectionFlags;
    /// the current (reusable) read data buffer of this connection
    fRd: TRawByteStringBuffer;
    /// the current (reusable) write data buffer of this connection
    fWr: TRawByteStringBuffer;
    /// TryLock/Unlock R/W thread acquisition
    // - uses its own rentrant implementation, faster/lighter than TOSLock
    fRW: array[boolean] of record
      Lock: PtrUInt;
      ThreadID: TThreadID;
      RentrantCount: integer;
    end;
    /// low-level TLS context
    fSecure: INetTls;
    /// called when the instance is connected to a poll
    // - i.e. at the end of TAsyncConnections.ConnectionNew(), when Handle is set
    // - overriding this method is cheaper than the plain Create destructor
    // - default implementation does nothing
    procedure AfterCreate; virtual;
    /// called when the instance is about to be deleted from a poll
    // - overriding this method is cheaper than the plain Destroy destructor
    // - default implementation does nothing
    procedure BeforeDestroy; virtual;
    /// this method is called when the some input data is pending on the socket
    // - should extract frames or requests from Connection.rd, and handle them
    // - this is where the input should be parsed and extracted according to
    // the implemented procotol; Connection.rd could be kept as temporary
    // buffer during the parsing, and rd.Reset called once processed
    // - Sender.Write() could be used for asynchronous answer sending
    // - Sender.LogVerbose() allows logging of escaped data
    // - could return sorClose to shutdown the socket, e.g. on parsing error
    function OnRead: TPollAsyncSocketOnReadWrite; virtual; abstract;
    /// this method is called when some data has been written to the socket
    // - default implementation will do nothing - see e.g. TRtspConnection
    // - you may send data asynchronously using Connection.wr.Append()
    function AfterWrite: TPollAsyncSocketOnReadWrite; virtual;
    /// this method is called when the sockets is closing
    procedure OnClose; virtual;
  public
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
    /// release all R/W nested locks
    // - used when the connection is closed and inactive
    procedure UnLockFinal(writer: boolean);
      {$ifdef HASINLINE} inline; {$endif}
    /// called after TAsyncConnections.LastOperationReleaseMemorySeconds
    function ReleaseMemoryOnIdle: PtrInt;
    /// send some buffer to the connection, using TLS if possible
    function Send(buf: pointer; var len: integer): TNetResult;
    /// receive some buffer from the connection, using TLS if possible
    function Recv(buf: pointer; var len: integer): TNetResult;
    /// read-only access to the socket number associated with this connection
    property Socket: TNetSocket
      read fSocket;
    /// read-only access to the low-level TLS context
    property Secure: INetTls
      read fSecure;
  published
    /// read-only access to the handle number associated with this connection
    property Handle: TPollAsyncConnectionHandle
      read fHandle;
  end;

  /// possible options for low-level TPollAsyncSockets process
  // - as translated from homonymous high-level acoWritePollOnly
  // TAsyncConnectionsOptions item
  TPollAsyncSocketsOptions = set of (
    paoWritePollOnly
  );

  TPollConnectionSockets = class(TPollSockets)
  protected
    function EnsurePending(tag: TPollSocketTag): boolean; override;
    procedure SetPending(tag: TPollSocketTag); override;
    function UnsetPending(tag: TPollSocketTag): boolean; override;
  end;

  /// callback prototype for TPollAsyncSockets.OnStart events
  // - should return true if Start() should not subscribe for this connection
  TOnPollAsyncFunc = function(Sender: TPollAsyncConnection): boolean of object;

  /// callback prototype for TPollAsyncSockets.OnStop events
  TOnPollAsyncProc = procedure(Sender: TPollAsyncConnection) of object;

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
    fRead: TPollConnectionSockets;
    fWrite: TPollSockets; // separated fWrite (short-term subscribe)
    fProcessingRead, fProcessingWrite: integer;
    fSendBufferSize: integer; // retrieved at first connection Start()
    fReadCount: Int64;
    fWriteCount: Int64;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fDebugLog: TSynLogClass;
    fOptions: TPollAsyncSocketsOptions;
    fProcessReadCheckPending: boolean;
    fReadWaitMs: integer;
    fOnStart: TOnPollAsyncFunc;
    fOnFirstRead, fOnStop: TOnPollAsyncProc;
    function GetCount: integer;
    procedure DoLog(const TextFmt: RawUtf8; const TextArgs: array of const);
    // pseError: return false to close socket and connection
    function OnError(connection: TPollAsyncConnection;
      events: TPollSocketEvents): boolean; virtual; abstract;
    procedure OnClosed(connection: TPollAsyncConnection); virtual; abstract;
    procedure UnlockAndCloseConnection(writer: boolean;
      var connection: TPollAsyncConnection; const caller: ShortString);
    procedure RegisterConnection(connection: TPollAsyncConnection); virtual; abstract;
    function SubscribeConnection(const caller: shortstring;
      connection: TPollAsyncConnection; sub: TPollSocketEvent): boolean;
    procedure CloseConnection(var connection: TPollAsyncConnection);
  public
    /// initialize the read/write sockets polling
    // - fRead and fWrite TPollSocketsBuffer instances will track pseRead or
    // pseWrite events, and maintain input and output data buffers
    constructor Create(aOptions: TPollAsyncSocketsOptions); virtual;
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
    function Stop(connection: TPollAsyncConnection): boolean; virtual;
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
    function ProcessRead(const notif: TPollSocketResult): boolean;
    /// one thread should execute this method with the proper pseWrite notif
    // - thread-safe handle of any outgoing packets
    procedure ProcessWrite(const notif: TPollSocketResult);
    /// notify internal socket polls to stop their polling loop ASAP
    procedure Terminate(waitforMS: integer);
    /// low-level access to the polling class used for incoming data
    property PollRead: TPollConnectionSockets
      read fRead;
    /// low-level access to the polling class used for outgoind data
    property PollWrite: TPollSockets
      write fWrite;
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
  end;

  {$M-}

function ToText(ev: TPollSocketEvent): PShortString; overload;


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
  // - each connection will be identified by a TPollAsyncConnectionHandle integer
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
    /// the associated remote IP4/IP6, as text
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
    // log the closing
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

  /// define what TAsyncConnectionsThread.Execute should actually do
  TAsyncConnectionsThreadProcess = (
    atpReadSingle,
    atpReadPoll,
    atpReadPending
  );

  TAsyncConnectionsThreadProcesses = set of TAsyncConnectionsThreadProcess;

  /// used to implement a thread poll to process TAsyncConnection instances
  TAsyncConnectionsThread = class(TSynThread)
  protected
    fOwner: TAsyncConnections;
    fProcess: TAsyncConnectionsThreadProcess;
    fWaitForReadPending: boolean;
    fExecuteState: THttpServerExecuteState;
    fIndex: integer;
    fEvent: TSynEvent;
    fName: RawUtf8;
    fThreadPollingLastWakeUpTix: integer;
    fThreadPollingLastWakeUpCount: integer;
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(aOwner: TAsyncConnections;
      aProcess: TAsyncConnectionsThreadProcess; aIndex: integer); reintroduce;
    /// finalize the thread resources
    destructor Destroy; override;
  published
    /// which kind of ProcessRead or ProcessWrite this thread is doing
    property Process: TAsyncConnectionsThreadProcess
      read fProcess;
    /// when used as a thread pool, the number of this thread
    property Index: integer
      read fIndex;
    /// the low-level thread name
    property Name: RawUtf8
      read fName;
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

  /// to implement generational garbage collector of asynchronous connections
  TAsyncConnectionsGC = record
    Safe: TLightLock;
    Count: integer;
    Items: TAsyncConnectionDynArray;
  end;

  /// implements an abstract thread-pooled high-performance TCP clients or server
  // - internal TAsyncConnectionsSockets will handle high-performance process
  // of a high number of long-living simultaneous connections
  // - will use a TAsyncConnection inherited class to maintain connection state
  // - don't use this abstract class but either TAsyncServer or TAsyncClient
  // - under Linux/POSIX, check your "ulimit -H -n" value: one socket consumes
  // two file descriptors: you may better add the following line to your
  // /etc/limits.conf or /etc/security/limits.conf system file:
  // $ * hard nofile 65535
  TAsyncConnections = class(TNotifiedThread)
  protected
    fConnectionClass: TAsyncConnectionClass;
    fConnection: TAsyncConnectionDynArray; // sorted by TAsyncConnection.Handle
    fClients: TAsyncConnectionsSockets;
    fThreads: array of TAsyncConnectionsThread;
    fThreadReadPoll: TAsyncConnectionsThread;
    fConnectionCount: integer; // only subscribed - not just after accept()
    fConnectionHigh: integer;
    fThreadPoolCount: integer;
    fLastConnectionFind: integer;
    fLastHandle: integer;
    fLog: TSynLogClass;
    fConnectionLock: TRWLock; // would block only on connection add/remove
    fOptions: TAsyncConnectionsOptions;
    fClientsEpoll: boolean; // = PollSocketClass.FollowEpoll
    fLastOperationSec: TAsyncConnectionSec;
    fLastOperationMS: cardinal; // stored by AddGC in connection.LastOperation
    fLastOperationReleaseMemorySeconds: cardinal;
    fLastOperationIdleSeconds: cardinal;
    fKeepConnectionInstanceMS: cardinal;
    fThreadClients: record // used by TAsyncClient
      Count, Timeout: integer;
      Address, Port: RawUtf8;
    end;
    fThreadPollingWakeupSafe: TLightLock;
    fThreadPollingWakeupLoad: integer;
    fThreadPollingLastWakeUpTix: integer;
    fGC: array[1..2] of TAsyncConnectionsGC;
    function AllThreadsStarted: boolean; virtual;
    procedure AddGC(aConnection: TPollAsyncConnection);
    procedure DoGC;
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: TNetAddr;
      out aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionNew(aSocket: TNetSocket; aConnection: TAsyncConnection;
      aAddAndSubscribe: boolean = true): boolean; virtual;
    function ConnectionDelete(
      aConnection: TPollAsyncConnection): boolean; overload; virtual;
    function LockedConnectionDelete(
      aConnection: TAsyncConnection; aIndex: integer): boolean;
    procedure ConnectionAdd(conn: TAsyncConnection);
    function ThreadPollingWakeup(Events: PtrInt): PtrInt;
    procedure DoLog(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; Instance: TObject);
    procedure ProcessIdleTix(Sender: TObject; NowTix: Int64); virtual;
    function ProcessClientStart(Sender: TPollAsyncConnection): boolean;
    procedure IdleEverySecond; virtual;
  public
    /// initialize the multiple connections
    // - don't use this constructor but inherited client/server classes
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the instance, releasing all associated threads and sockets
    procedure Shutdown;
    /// shut down and finalize the instance, calling Shutdown
    destructor Destroy; override;
    /// high-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    // - raise an exception if acoNoConnectionTrack option was defined
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the lock as:
    // ! try ... finally UnLock(aLock); end;
    function ConnectionFindAndLock(aHandle: TPollAsyncConnectionHandle;
      aLock: TRWLockContext; aIndex: PInteger = nil): TAsyncConnection;
    /// high-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search
    // - this method won't keep the main Lock, but this class will ensure that
    // the returned pointer will last for at least 100ms until Free is called
    function ConnectionFind(aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
    /// low-level access to a connection instance, from its handle
    // - use efficient O(log(n)) binary search, since handles are increasing
    // - caller should have called Lock before this method is done
    function LockedConnectionSearch(aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
    /// just a wrapper around fConnectionLock.Lock
    // - raise an exception if acoNoConnectionTrack option was defined
    procedure Lock(aLock: TRWLockContext);
    /// just a wrapper around fConnectionLock.UnLock
    // - raise an exception if acoNoConnectionTrack option was defined
    // - to be called e.g. after a successfull ConnectionFindAndLock(aLock)
    procedure Unlock(aLock: TRWLockContext);
    /// remove an handle from the internal list, and close its connection
    // - raise an exception if acoNoConnectionTrack option was defined
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function ConnectionRemove(aHandle: TPollAsyncConnectionHandle): boolean;
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
    /// log some binary data with proper escape
    // - can be executed from an TAsyncConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(self,...);
    procedure LogVerbose(connection: TPollAsyncConnection; const ident: RawUtf8;
      const identargs: array of const; frame: pointer; framelen: integer); overload;
    /// log some binary data with proper escape
    // - can be executed from an TAsyncConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(...);
    procedure LogVerbose(connection: TPollAsyncConnection; const ident: RawUtf8;
      const identargs: array of const; const frame: TRawByteStringBuffer); overload;
    /// the current monotonic time elapsed, evaluated in seconds
    // - IdleEverySecond will set GetTickCount64 div 1000
    property LastOperationSec: TAsyncConnectionSec
      read fLastOperationSec;
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
    // - default is 100 ms before the internal GC call Free on this instance
    property KeepConnectionInstanceMS: cardinal
      read fKeepConnectionInstanceMS write fKeepConnectionInstanceMS;
    /// allow to customize low-level options for processing
    property Options: TAsyncConnectionsOptions
      read fOptions write fOptions;
    // how many events a fast active thread is supposed to handle in its loop
    // for the acoThreadSmooting option in ThreadPollingWakeup()
    // - will wake up the threads only if the previous seem to be somewhat idle
    // - default value of 32 has been set from trials over wrk benchmarks
    property ThreadPollingWakeupLoad: integer
      read fThreadPollingWakeupLoad write fThreadPollingWakeupLoad;
    /// access to the associated log class
    property Log: TSynLogClass
      read fLog;
    /// low-level unsafe direct access to the connection instances
    // - ensure this property is used in a thread-safe manner, i.e. calling
    // ConnectionFindAndLock() high-level function, ot via manual
    // ! Lock; try ... finally UnLock; end;
    property Connection: TAsyncConnectionDynArray
      read fConnection;
  published
    /// how many read threads there are in this thread pool
    property ThreadPoolCount: integer
      read fThreadPoolCount;
    /// current HTTP/1.1 / WebSockets connections count
    // - this is the number of long-living connections - may not appear just
    // after accept, so never for a HTTP/1.0 short-living request
    property ConnectionCount: integer
      read fConnectionCount;
    /// maximum number of concurrent long-living connections
    property ConnectionHigh: integer
      read fConnectionHigh;
    /// access to the TCP client sockets poll
    // - TAsyncConnection.OnRead should rather use Write() and LogVerbose()
    // methods of this TAsyncConnections class instead of using Clients
    property Clients: TAsyncConnectionsSockets
      read fClients;
  end;

  /// implements a thread-pooled high-performance TCP server
  // - will use a TAsyncConnection inherited class to maintain connection
  // state for server process
  TAsyncServer = class(TAsyncConnections)
  protected
    fServer: TCrtSocket; // for proper complex binding
    fMaxPending: integer;
    fMaxConnections: integer;
    fAccepted: Int64;
    fExecuteState: THttpServerExecuteState;
    fExecuteAcceptOnly: boolean; // writes in another thread (THttpAsyncServer)
    fExecuteMessage: RawUtf8;
    fSockPort: RawUtf8;
    fBanned: THttpAcceptBan; // for hsoBan40xIP
    procedure OnFirstReadDoTls(Sender: TPollAsyncConnection);
    procedure SetExecuteState(State: THttpServerExecuteState); virtual;
    procedure Execute; override;
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
    procedure Shutdown;
    /// shut down the server, releasing all associated threads and sockets
    destructor Destroy; override;
  published
    /// access to the TCP server socket
    property Server: TCrtSocket
      read fServer;
    /// how many connections have been accepted since server startup
    // - ConnectionCount is the number of long-living connections, this
    // counter is the absolute number of successfull accept() calls,
    // including short-living (e.g. HTTP/1.0) connections
    property Accepted: Int64
      read fAccepted;
    /// above how many active connections accept() would reject
    // - MaxPending applies to the actual thread-pool processing activity,
    // whereas MaxConnections tracks the number of connections even in idle state
    property MaxConnections: integer
      read fMaxConnections write fMaxConnections;
    /// above how many fClients.fRead.PendingCount accept() would reject
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
    procedure Execute; override;
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

  THttpAsyncServer = class;
  THttpAsyncConnections = class;

  /// handle one HTTP client connection to our non-blocking THttpAsyncServer
  THttpAsyncConnection = class(TAsyncConnection)
  protected
    fHttp: THttpRequestContext;
    fServer: THttpAsyncServer;
    fKeepAliveSec: TAsyncConnectionSec;
    fHeadersSec: TAsyncConnectionSec;
    fRespStatus: integer;
    fRequest: THttpServerRequest;
    fConnectionOpaque: THttpServerConnectionOpaque; // two PtrUInt tags
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    procedure HttpInit;
    // redirect to fHttp.ProcessRead()
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    // redirect to fHttp.ProcessWrite()
    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    // quickly reject incorrect requests (payload/timeout/OnBeforeBody)
    procedure DoReject(status: integer);
    function DecodeHeaders: integer; virtual;
    function DoHeaders: TPollAsyncSocketOnReadWrite;
    function DoRequest: TPollAsyncSocketOnReadWrite;
  public
    /// reuse this instance for a new incoming connection
    procedure Recycle(const aRemoteIP: TNetAddr); override;
  end;

  /// event-driven process of HTTP/WebSockets connections
  THttpAsyncConnections = class(TAsyncServer)
  protected
    fAsyncServer: THttpAsyncServer;
    procedure IdleEverySecond; override;
    procedure SetExecuteState(State: THttpServerExecuteState); override;
    procedure Execute; override;
  published
    /// set if hsoBan40xIP has been defined
    // - indicates e.g. how many accept() have been rejected from their IP
    // - you can customize its behavior once the server is started by resetting
    // its Seconds/Max/WhiteIP properties, before any connections are made
    property Banned: THttpAcceptBan
      read fBanned;
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
    fInterning: PRawUtf8InterningSlot;
    fInterningTix: cardinal;
    fHttpDateNowUtc: string[39]; // consume 37 chars
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetExecuteState: THttpServerExecuteState; override;
    procedure IdleEverySecond; virtual;
    procedure AppendHttpDate(var Dest: TRawByteStringBuffer); override;
    // the main thread will Send output packets in the background
    procedure Execute; override;
  public
    /// create an event-driven HTTP Server
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
      ServerThreadPoolCount: integer = 32; KeepAliveTimeOut: integer = 30000;
      ProcessOptions: THttpServerOptions = []); override;
    /// finalize the HTTP Server
    destructor Destroy; override;
  published
    /// initial capacity of internal per-connection Headers buffer
    // - 2 KB by default is within the mormot.core.fpcx64mm SMALL blocks limit
    // so will use up to 3 locks before contention
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




implementation


{ ******************** Low-Level Non-blocking Connections }

function ToText(ev: TPollSocketEvent): PShortString;
begin
  result := GetEnumName(TypeInfo(TPollSocketEvent), ord(ev));
end;



{ TPollAsyncConnection }

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
begin
end;

procedure TPollAsyncConnection.BeforeDestroy;
begin
end;

function TPollAsyncConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
begin
  result := soContinue;
end;

function TPollAsyncConnection.IsDangling: boolean;
begin
  result := (self = nil) or
            (fHandle = 0);
end;

function TPollAsyncConnection.IsClosed: boolean;
begin
  result := (self = nil) or
            (fHandle = 0) or
            (fSocket = nil) or
            (fClosed in fFlags);
end;

function TPollAsyncConnection.TryLock(writer: boolean): boolean;
var
  tid: TThreadID;
begin
  result := false;
  if (self = nil) or
     (fSocket = nil) then
    exit;
  tid := GetCurrentThreadId;
  with fRW[writer and fLockMax] do
    if Lock <> 0 then
      if ThreadID = tid then
      begin
        inc(RentrantCount);
        result := true;
      end
      else
        exit
    else if LockedExc(Lock, 1, 0) then
    begin
      include(fFlags, fWasActive);
      ThreadID := tid;
      RentrantCount := 1;
      result := true;
    end;
end;

procedure TPollAsyncConnection.UnLock(writer: boolean);
begin
  if self <> nil then
    with fRW[writer and fLockMax] do
    begin
      dec(RentrantCount);
      if RentrantCount <> 0 then
        exit;
      Lock := 0;
      ThreadID := TThreadID(0);
    end;
end;

procedure TPollAsyncConnection.UnLockFinal(writer: boolean);
begin
  fRW[writer and fLockMax].Lock:= 0;
end;

procedure TPollAsyncConnection.OnClose;
begin
  include(fFlags, fClosed);
end;

function TPollAsyncConnection.ReleaseMemoryOnIdle: PtrInt;
begin
  // called now and then to reduce temp memory consumption on Idle connections
  result := 0;
  if (fRd.Buffer <> nil) and
     TryLock({wr=}false) then
  begin
    inc(result, fRd.Capacity); // returns number of bytes released
    fRd.Clear;
    UnLock(false);
  end;
  if (fWr.Buffer <> nil) and
     TryLock({fWr=}true) then
  begin
    inc(result, fWr.Capacity);
    fWr.Clear;
    UnLock(true);
  end;
  exclude(fFlags, fWasActive); // TryLock() was with no true activity here
end;

function TPollAsyncConnection.WaitLock(writer: boolean; timeoutMS: cardinal): boolean;
var
  endtix: Int64;
  ms: integer;
begin
  result := (@self <> nil) and
            (fSocket <> nil);
  if not result then
    exit; // socket closed
  result := TryLock(writer);
  if result or
     (timeoutMS = 0) then
    // we acquired the Connection for this direction, or we don't want to wait
    exit;
  // loop to wait for the lock release
  endtix := GetTickCount64 + timeoutMS; // never wait forever
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
  until GetTickCount64 >= endtix;
end;

function TPollAsyncConnection.Send(buf: pointer; var len: integer): TNetResult;
begin
  if fSecure <> nil then
    result := fSecure.Send(buf, len)
  else
    result := fSocket.Send(buf, len);
end;

function TPollAsyncConnection.Recv(buf: pointer; var len: integer): TNetResult;
begin
  if fSecure <> nil then
    result := fSecure.Receive(buf, len)
  else
    result := fSocket.Recv(buf, len);
end;


{ TPollConnectionSockets }

function TPollConnectionSockets.EnsurePending(tag: TPollSocketTag): boolean;
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

procedure TPollConnectionSockets.SetPending(tag: TPollSocketTag);
begin
  if tag <> 0 then // tag = 0 at shutdown
    include(TPollAsyncConnection(tag).fFlags, fReadPending);
end;

function TPollConnectionSockets.UnsetPending(tag: TPollSocketTag): boolean;
begin
  result := false;
  if tag <> 0 then
    // same paranoid logic than TPollAsyncConnection IsDangling() + TryLock()
    if // avoid dangling pointer
       (TPollAsyncConnection(tag).fHandle <> 0) and
       // another atpReadPending thread may currently own this connection
       // (occurs if PollForPendingEvents was called in between)
       (TPollAsyncConnection(tag).fRW[{write=}false].RentrantCount = 0) then
    begin
      exclude(TPollAsyncConnection(tag).fFlags, fReadPending);
      result := true;
    end;
end;


{ TPollAsyncSockets }

constructor TPollAsyncSockets.Create(aOptions: TPollAsyncSocketsOptions);
begin
  fOptions := aOptions;
  inherited Create;
  fRead := TPollConnectionSockets.Create;
  fRead.UnsubscribeShouldShutdownSocket := true;
  fWrite := TPollSockets.Create;
end;

destructor TPollAsyncSockets.Destroy;
begin
  if not fRead.Terminated then
    Terminate(5000);
  fRead.Free;
  fWrite.Free;
  inherited Destroy;
end;

function TPollAsyncSockets.Start(connection: TPollAsyncConnection): boolean;
begin
  result := false;
  if fRead.Terminated or
     connection.IsDangling then
    exit;
  LockedInc32(@fProcessingRead);
  try
    {if fDebugLog <> nil then
      DoLog('Start sock=% handle=%',
        [pointer(connection.fSocket), connection.Handle]);}
    // get sending buffer size from OS (if not already retrieved)
    if fSendBufferSize = 0 then
      {$ifdef OSWINDOWS}
      // on Windows, default buffer is 8KB and we upgrade it to 64KB
      // but for normal process, this could be a bit low -> force 256KB
      fSendBufferSize := 256 shl 10;
      // if direct Write() doesn't succeed, it will subscribe to ProcessWrite
      {$else}
      // on Linux/POSIX, typical values are 2MB for TCP, 200KB on Unix Sockets
      fSendBufferSize := connection.fSocket.SendBufferSize;
      {$endif OSWINDOWS}
    // subscribe for incoming data (async for select/poll, immediate for epoll)
    if Assigned(fOnStart) then
      result := fOnStart(connection); // e.g. TAsyncConnections.ProcessClientStart
    // result=true here means that fRead.Subscribe() is delayed in atpReadPending
    // warning: result=true may actually make connection.Free before it returns
    if not result then
      // let ProcessRead handle pseRead+pseError/pseClosed on this socket
      result := SubscribeConnection('start', connection, pseRead);
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

const
  _SUB: array[boolean] of AnsiChar = '-+';

function TPollAsyncSockets.Stop(connection: TPollAsyncConnection): boolean;
var
  sock: TNetSocket;
begin
  result := false;
  if fRead.Terminated or
     connection.IsDangling then
    exit;
  LockedInc32(@fProcessingRead);
  try
    // retrieve the raw information of this abstract connection
    sock := connection.fSocket;
    if fDebugLog <> nil then
      DoLog('Stop sock=% handle=% r=%% w=%%',
        [pointer(sock), connection.Handle, connection.fRW[false].RentrantCount,
         _SUB[fSubRead in connection.fFlags], connection.fRW[true].RentrantCount,
         _SUB[fSubWrite in connection.fFlags]]);
    if sock <> nil then
    begin
      // notify ProcessRead/ProcessWrite to abort
      connection.fSocket := nil;
      // unsubscribe and close the socket
      if fSubWrite in connection.fFlags then
        // write first because of fRead.UnsubscribeShouldShutdownSocket
        fWrite.Unsubscribe(sock, TPollSocketTag(connection));
      if connection.fSecure <> nil then
        try
          connection.fSecure := nil; // perform TLS shutdown and release context
        except
          pointer(connection.fSecure) := nil; // leak is better than GPF
        end;
      if fSubRead in connection.fFlags then
        // note: fRead.UnsubscribeShouldShutdownSocket=true, so ShutdownAndClose
        // is done now on Epoll, or at next PollForPendingEvents()
        fRead.Unsubscribe(sock, TPollSocketTag(connection))
      else
        // close the socket even if not subscribed (e.g. HTTP/1.0)
        sock.ShutdownAndClose({rdwr=}false);
      result := true;
    end;
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
    result := fRead.Count;
end;

procedure TPollAsyncSockets.DoLog(const TextFmt: RawUtf8;
  const TextArgs: array of const);
begin
  fDebugLog.Add.Log(sllTrace, TextFmt, TextArgs, self);
end;

procedure TPollAsyncSockets.Terminate(waitforMS: integer);
var
  start, elapsed: Int64;
begin
  if fDebugLog <> nil then
    DoLog('Terminate(%) processing fRd=% fWr=%',
      [waitforMS, fProcessingRead, fProcessingWrite]);
  // abort receive/send polling engines
  fRead.Terminate;
  fWrite.Terminate;
  // wait for actual termination
  if (waitforMS <= 0) or
     ((fProcessingRead = 0) and
      (fProcessingWrite = 0)) then
    exit;
  start := GetTickCount64;
  repeat
    SleepHiRes(1);
    elapsed := GetTickCount64 - start;
  until ((fProcessingRead = 0) and
         (fProcessingWrite = 0)) or
         (elapsed > waitforMS);
  if fDebugLog <> nil then
    DoLog('Terminate processing fRd=% fWr=% after %ms',
      [fProcessingRead, fProcessingWrite, elapsed]);
end;

function TPollAsyncSockets.WriteString(connection: TPollAsyncConnection;
  const data: RawByteString; timeout: integer): boolean;
begin
  if self = nil then
    result := false
  else
    result := Write(connection, pointer(data), length(data), timeout);
end;

function TPollAsyncSockets.Write(connection: TPollAsyncConnection;
  data: pointer; datalen: integer; timeout: integer): boolean;
var
  P: PByte;
  res: TNetResult;
  sent, previous: integer;
begin
  result := false;
  if (datalen <= 0) or
     fWrite.Terminated or
     connection.IsClosed then
    exit;
  // try and wait for another ProcessWrite
  LockedInc32(@fProcessingWrite);
  if connection.WaitLock({writer=}true, timeout) then
  try
    // we acquired the write lock: immediate or delayed/buffered sending
    //DoLog('Write: WaitLock fProcessingWrite=%', [fProcessingWrite]);
    P := data;
    previous := connection.fWr.Len;
    if (previous = 0) and
       not (paoWritePollOnly in fOptions) then
      repeat
        // try to send now in non-blocking mode (works most of the time)
        if fWrite.Terminated or
           (connection.fSocket = nil) then
          exit;
        sent := datalen;
        res := connection.Send(P, sent);
        if connection.fSocket = nil then
          exit;  // Stop() called
        if res = nrRetry then
          break; // fails now -> retry later in ProcessWrite
        if res <> nrOK then
        begin
          if fDebugLog <> nil then
            DoLog('Write: Send(%)=% len=% handle=%', [pointer(connection.Socket),
              ToText(res)^, sent, connection.Handle]);
          exit;  // connection closed or broken -> abort
        end;
        inc(P, sent);
        inc(fWriteCount);
        inc(fWriteBytes, sent);
        dec(datalen, sent);
      until datalen = 0;
    if connection.fSocket = nil then
      exit;
    result := true;
    if datalen <> 0 then
      // use fWrite output polling for the remaining data in ProcessWrite
      connection.fWr.Append(P, datalen)
    else
      // notify everything written - maybe call slot.fWr.Append
      try
        result := connection.AfterWrite = soContinue;
        if (not result) and
           (fDebugLog <> nil) then
          DoLog('Write % closed by AfterWrite handle=%',
            [pointer(connection.Socket), connection.Handle]);
      except
        result := false;
      end;
    if result and
       (connection.fWr.Len > 0) then
      // there is still some pending output bytes
      if previous = 0 then
        // register for ProcessWrite() if not already
        result := SubscribeConnection('write', connection, pseWrite);
  finally
    //DoLog('Write: finally fProcessingWrite=%', [fProcessingWrite]);
    if result then
      connection.UnLock({writer=}true)
    else
      // sending or subscription error -> abort
      UnlockAndCloseConnection(true, connection, 'Write() finished');
    LockedDec32(@fProcessingWrite);
  end
  else
  begin
    // WaitLock() should always work - unless the connection is closing or
    // several write operations collide (e.g. websockets broadcast + process)
    if fDebugLog <> nil then
      DoLog('Write: WaitLock failed % %', [pointer(connection), fWrite]);
    LockedDec32(@fProcessingWrite);
  end;
  //DoLog('Write: done fProcessingWrite=%', [fProcessingWrite]);
end;

procedure TPollAsyncSockets.UnlockAndCloseConnection(writer: boolean;
  var connection: TPollAsyncConnection; const caller: ShortString);
var
  c: TPollAsyncConnection;
begin
  c := connection;
  connection := nil;
  if (c = nil) or
     (fClosed in c.fFlags) then
    exit;
  {if fDebugLog <> nil then
    DoLog('UnlockSlotAndCloseConnection: % on handle=%',
      [caller, connection.Handle]);}
  // first unlock (if needed)
  c.UnLockFinal(writer);
  // optional process - e.g. TWebSocketAsyncConnection = focConnectionClose
  c.OnClose; // called before slot/socket closing - set fClosed flag
  // Stop() will try to acquire this lock -> notify no need to wait
  CloseConnection(c);
end;

function TPollAsyncSockets.SubscribeConnection(const caller: shortstring;
  connection: TPollAsyncConnection; sub: TPollSocketEvent): boolean;
var
  poll: TPollSockets;
begin
  if not (fInList in connection.fFlags) then // not already registered
    RegisterConnection(connection);
  result := false;
  if sub = pseRead then
    if fSubRead in connection.fFlags then
      exit
    else
      poll := fRead
  else if fSubWrite in connection.fFlags then
      exit
    else
      poll := fWrite;
  result := poll.Subscribe(connection.fSocket, [sub], TPollSocketTag(connection));
  if result then
     if sub = pseRead then
       include(connection.fFlags, fSubRead)
     else
       include(connection.fFlags, fSubWrite);
  if fDebugLog <> nil then
    DoLog('Subscribe(%)=% % handle=% % cnt=%', [pointer(connection.fSocket),
      BOOL_STR[result], caller, connection.Handle, ToText(sub)^, poll.Count]);
end;

procedure TPollAsyncSockets.CloseConnection(var connection: TPollAsyncConnection);
begin
  if connection.IsDangling then
    exit;
  try
    if not (fClosed in connection.fFlags) then
      // if not already done in UnlockAndCloseConnection
      connection.OnClose; // called before slot/socket closing
    // set socket := nil and async unsubscribe for next PollForPendingEvents()
    Stop(connection);
    // now safe to perform fOwner.ConnectionDelete() for async instance GC
    OnClosed(connection);
  except
    connection := nil;   // user code may be unstable
  end;
end;

function TPollAsyncSockets.ProcessRead(const notif: TPollSocketResult): boolean;
var
  connection: TPollAsyncConnection;
  recved, added, retryms: integer;
  pse: TPollSocketEvents;
  res: TNetResult;
  start, stop: Int64;
  wf: string[3];
  temp: array[0..$7fff] of byte; // up to 32KB moved to small reusable fRd.Buffer
begin
  result := true; // if closed or properly read: don't retry
  connection := TPollAsyncConnection(ResToTag(notif));
  if (self = nil) or
     fRead.Terminated or
     connection.IsClosed then
    exit;
  LockedInc32(@fProcessingRead);
  try
    pse := ResToEvents(notif);
    if pseClosed in pse then
    begin
      // - properly triggered from EPOLLRDHUP on Linux in ET mode
      // - never notified on Windows: select() doesn't return any "close" flag
      // and checking for pending bytes for closed connection is not correct
      // on multi-thread -> Recv() below will properly detect disconnection
      CloseConnection(connection);
      exit;
    end;
    if pseError in pse then
      // check for pseError after pseClosed, because closing is no fatal error
      if not OnError(connection, pse) then
      begin
        CloseConnection(connection);
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
        if not (fFirstRead in connection.fFlags) then
        begin
          include(connection.fFlags, fFirstRead);
          if Assigned(fOnFirstRead) then
          try
            fOnFirstRead(connection); // e.g. TAsyncServer.OnFirstReadDoTls
          except
            // TLS error -> abort
            UnlockAndCloseConnection(false, connection, 'ProcessRead OnFirstRead');
            exit;
          end
          else if (retryms = 0) and
                  (fProcessingRead < 4) then // < 4 for "wrk -c 10000" not fail
            retryms := 50; // just after accept() on a idle server
        end;
        repeat
          if fRead.Terminated or
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
            if connection.fSocket.WaitFor(retryms, [neRead]) = [neRead] then
              res := connection.Recv(@temp, recved);
            wf := 'wf ';
          end
          else
            wf[0] := #0;
          if fDebugLog <> nil then
          begin
            QueryPerformanceMicroSeconds(stop);
            DoLog('ProcessRead recv(%)=% len=% %in % %', [pointer(connection.Socket),
              ToText(res)^, recved, wf, MicroSecToString(stop - start), fRead]);
          end;
          if connection.fSocket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break; // may block, try later
          if res <> nrOk then
          begin
            // socket closed gracefully or unrecoverable error -> abort
            UnlockAndCloseConnection(false, connection, 'ProcessRead recv');
            exit;
          end;
          connection.fRd.Append(@temp, recved); // will mostly reuse fRd.Buffer
          inc(added, recved);
        until recved < SizeOf(temp);
        if added > 0 then
        begin
          inc(fReadCount); // no lock since informationaly only
          inc(fReadBytes, added);
          try
            // process connection.fRd incoming data (outside of the lock)
            if connection.OnRead = soClose then
              UnlockAndCloseConnection(false, connection, 'ProcessRead OnRead');
          except
            UnlockAndCloseConnection(false, connection, 'ProcessRead Exception');
          end;
        end
        else
          result := false; // retry later
        if connection <> nil then // UnlockAndCloseConnection() may set Free+nil
        begin
          connection.UnLock(false); // UnlockSlotAndCloseConnection set slot=nil
          if (connection.fSocket <> nil) and
             not (fClosed in connection.fFlags) and
             not (fSubRead in connection.fFlags) then
            // it is time to subscribe for any future read on this connection
            if not SubscribeConnection('read', connection, pseRead) then
              if fDebugLog <> nil then
                DoLog('ProcessRead: Subscribe failed % %', [connection, fRead]);
        end;
      end
      else
      begin
        if fDebugLog <> nil then
          // happens on thread contention
          DoLog('ProcessRead: TryLock failed % %', [connection, fRead]);
        result := false; // retry later
      end;
    end;
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

procedure TPollAsyncSockets.ProcessWrite(const notif: TPollSocketResult);
var
  connection: TPollAsyncConnection;
  buf: PByte;
  buflen, bufsent, sent: integer;
  res: TNetResult;
  start: Int64;
begin
  connection := TPollAsyncConnection(ResToTag(notif));
  if (self = nil) or
     fWrite.Terminated or
     (ResToEvents(notif) <> [pseWrite]) or
     connection.IsClosed then
    exit;
  // we are now sure that the socket is writable and safe
  sent := 0;
  //DoLog('ProcessWrite: fProcessingWrite=%', [fProcessingWrite]);
  LockedInc32(@fProcessingWrite);
  try
    res := nrOK;
    if connection.WaitLock({writer=}true, {timeout=}0) then // no need to wait
    try
      //DoLog('ProcessWrite: Locked fProcessingWrite=%', [fProcessingWrite]);
      buflen := connection.fWr.Len;
      if buflen = 0 then
        exit;
      buf := connection.fWr.Buffer;
      repeat
        if fDebugLog <> nil then
          QueryPerformanceMicroSeconds(start);
        if fWrite.Terminated or
           (connection.fSocket = nil) then
          exit;
        bufsent := buflen;
        //DoLog('ProcessWrite: before Send buflen=%', [bufsent]);
        res := connection.Send(buf, bufsent);
        if fDebugLog <> nil then
        begin
          DoLog('ProcessWrite send(%)=% %/%B in % % fProcessingWrite=%',
            [pointer(connection.fSocket), ToText(res)^, bufsent, buflen,
             MicroSecFrom(start), fWrite, fProcessingWrite]);
        end;
        if connection.fSocket = nil then
          exit; // Stop() called
        if res = nrRetry then
          break // may block, try later
        else if res <> nrOk then
        begin
          fWrite.Unsubscribe(connection.fSocket, ResToTag(notif));
          exclude(connection.fFlags, fSubWrite);
          if fDebugLog <> nil then
            DoLog('Write failed as % -> Unsubscribe(%,%) %', [ToText(res)^,
              pointer(connection.fSocket), connection.Handle, fWrite]);
          exit; // socket closed gracefully or unrecoverable error -> abort
        end;
        inc(fWriteCount);
        inc(sent, bufsent);
        inc(buf, bufsent);
        dec(buflen, bufsent);
      until buflen = 0;
      inc(fWriteBytes, sent);
      connection.fWr.Remove(sent); // is very likely to just set fWr.Len := 0
      if connection.fWr.Len = 0 then
      begin
        // no data any more to be sent - maybe call slot.fWr.Append
        try
          if connection.AfterWrite <> soContinue then
          begin
            if fDebugLog <> nil then
              DoLog('ProcessWrite % closed by AfterWrite handle=% sent=%',
                [pointer(connection.fSocket), connection.Handle, sent]);
            connection.fWr.Clear;
            res := nrClosed;
          end;
        except
          connection.fWr.Reset;
        end;
        if connection.fWr.Len = 0 then
        begin
          // no further ProcessWrite unless slot.fWr contains pending data
          fWrite.Unsubscribe(connection.fSocket, ResToTag(notif));
          exclude(connection.fFlags, fSubWrite);
          if fDebugLog <> nil then
            DoLog('Write Unsubscribe(sock=%,handle=%)=% %',
             [pointer(connection.fSocket), connection.Handle, fWrite]);
        end;
      end;
    finally
      if res in [nrOk, nrRetry] then
        connection.UnLock(true)
      else
        // sending error or AfterWrite abort
        UnlockAndCloseConnection(true, connection, 'ProcessWrite');
    end
    // if already locked (unlikely) -> will try next time
    else if fDebugLog <> nil then
      DoLog('ProcessWrite: WaitLock failed % % -> will retry later',
        [pointer(connection), fWrite]);
  finally
    LockedDec32(@fProcessingWrite);
  end;
end;


{ ******************** Client or Server Asynchronous Process }

{ TAsyncConnection }

constructor TAsyncConnection.Create(aOwner: TAsyncConnections;
  const aRemoteIP: TNetAddr);
begin
  fOwner := aOwner;
  inherited Create;
  fFlags := [fWasActive]; // by definition
  fRemoteIP4 := aRemoteIP.IP4;
  aRemoteIP.IP(fRemoteIP, {localasvoid=}true);
end;

procedure TAsyncConnection.Recycle(const aRemoteIP: TNetAddr);
begin
  fLockMax := false;
  fFlags := [fFromGC, fWasActive];
  fRd.Reset;
  fWr.Reset;
  FillCharFast(fRW, SizeOf(fRW), 0);
  fSecure := nil;
  fLastOperation := 0;
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
  if (fOwner.fLog <> nil) and
     (acoVerboseLog in fOwner.Options) and
     not (acoNoLogWrite in fOwner.Options) then
    fOwner.LogVerbose(TAsyncConnection(connection), 'Write', [], data, datalen);
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
  fEvent := TSynEvent.Create;
  fOnThreadTerminate := fOwner.fOnThreadTerminate;
  inherited Create({suspended=}false);
end;

destructor TAsyncConnectionsThread.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TAsyncConnectionsThread.Execute;
var
  new, pending, ms: integer;
  didwakeupduetoslowprocess: boolean;
  start: Int64;
  notif: TPollSocketResult;
begin
  FormatUtf8('R% %', [fIndex, fOwner.fProcessName], fName);
  SetCurrentThreadName(fName);
  fOwner.NotifyThreadStart(self);
  try
    fExecuteState := esRunning;
    // implement parallel client connections for TAsyncClient
    while not Terminated and
          (fOwner.fThreadClients.Count > 0) and
          (InterlockedDecrement(fOwner.fThreadClients.Count) >= 0) do
      fOwner.ThreadClientsConnect;
    ms := 0;
    case fProcess of
      atpReadSingle:
        if fOwner.fClientsEpoll then
          ms := 100 // for quick shutdown
        else
          ms := 1000;
      atpReadPoll:
        if fOwner.fClientsEpoll then
          ms := 1100; // efficient epoll_wait(ms) API call
    end;
    // main TAsyncConnections read/write process
    while not Terminated and
          (fOwner.fClients <> nil) and
          (fOwner.fClients.fRead <> nil) do
    begin
      case fProcess of
        atpReadSingle:
          // a single thread to rule them all: polling, reading and processing
          if fOwner.fClients.fRead.GetOne(ms, fName, notif) then
            if not Terminated then
              fOwner.fClients.ProcessRead(notif);
        atpReadPoll:
          // main thread will just fill pending events from socket polls
          // (no process because a faulty service would delay all reading)
          begin
            start := 0; // back to SleepStep(0)
            while not Terminated do
            begin
              fWaitForReadPending := false;
              new := fOwner.fClients.fRead.PollForPendingEvents(ms);
              if Terminated then
                break;
              fWaitForReadPending := true;
              pending := fOwner.fClients.fRead.fPending.Count;
              if new = 0 then // fRead has nothing new
                if (pending = 0) and
                   (fOwner.fClients.fRead.Count = 0) then
                begin
                  // avoid void PollForPendingEvents/SleepStep loop
                  fOwner.DoGC;
                  fEvent.ResetEvent;
                  fWaitForReadPending := true;
                  //fOwner.DoLog(sllInfo, 'Execute: % sleep', [fName], self);
                  fEvent.WaitForEver; // blocking until next accept()
                  //fOwner.DoLog(sllInfo, 'Execute: % wakeup', [fName], self);
                  continue;
                end
                else
                begin
                  // 0/1/10/50/150 ms steps, checking fThreadReadPoll.fEvent
                  fEvent.SleepStep(start, @Terminated);
                  continue;
                end;
              if pending > 0 then
              begin
                // process fOwner.fClients.fPending in atpReadPending threads
                //fOwner.fClients.fRead.PendingLogDebug('Wakeup');
                fEvent.ResetEvent;
                fWaitForReadPending := true; // should be set before wakeup
                fOwner.ThreadPollingWakeup(pending);
                //fOwner.DoLog(sllCustom1, 'Execute: WaitFor ReadPending', [], self);
                if not Terminated then
                  fEvent.WaitFor(20);
                //fOwner.DoLog(sllCustom1, 'Execute: WaitFor out', [], self);
                break; // back to SleepStep(0) if new=0
              end;
            end;
          end;
        atpReadPending:
          begin
            // secondary threads wait, then read and process pending events
            fWaitForReadPending := true;
            fEvent.WaitForEver;
            if Terminated then
              break;
            didwakeupduetoslowprocess := false;
            //{$I-}system.writeln(Name,' start loop ',fThreadPollingLastWakeUpCount);
            while fOwner.fClients.fRead.GetOnePending(notif, fName) and
                  not Terminated do
            begin
              if (acoThreadSmooting in fOwner.Options) and
                 (fThreadPollingLastWakeUpTix <> fOwner.fThreadPollingLastWakeUpTix) and
                 not didwakeupduetoslowprocess then
              begin
                // ProcessRead() did take some time: wake up another thread
                // - slow down a little bit the wrk RPS
                // - but seems to reduce the wrk max latency
                didwakeupduetoslowprocess := true; // do it once per loop
                //{$I-}system.writeln(Name,' didwakeupduetoslowprocess');
                fOwner.ThreadPollingWakeup(1); // one thread is enough
              end;
              fOwner.fClients.ProcessRead(notif);
            end;
            if acoThreadSmooting in fOwner.Options then
            begin
              fOwner.fThreadPollingWakeupSafe.Lock;
              fThreadPollingLastWakeUpTix := 0; // this thread will now need to wakeup
              fOwner.fThreadPollingWakeupSafe.UnLock;
            end;
            //{$I-}system.writeln(Name,' stop loop ',fThreadPollingLastWakeUpCount);
            // release atpReadPoll lock above
            with fOwner.fThreadReadPoll do
              if fWaitForReadPending then
              begin
                //{$I-}system.writeln(Name,' set event ',fThreadPollingLastWakeUpCount);
                fWaitForReadPending := false; // set event once
                fEvent.SetEvent;
              end;
          end;
      else
        raise EAsyncConnections.CreateUtf8('%.Execute: unexpected fProcess=%',
          [self, ord(fProcess)]);
      end;
    end;
    fOwner.DoLog(sllInfo, 'Execute: done %', [fName], self);
  except
    on E: Exception do
      if fOwner <> nil then
        fOwner.DoLog(sllWarning, 'Execute raised a % -> terminate % thread %',
          [E.ClassType, fOwner.fConnectionClass, fName], self);
  end;
  fExecuteState := esFinished;
end;


{ TAsyncConnections }

constructor TAsyncConnections.Create(const OnStart, OnStop: TOnNotifyThread;
  aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
  aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
var
  i: PtrInt;
  tix: Int64;
  opt: TPollAsyncSocketsOptions;
  {%H-}log: ISynLog;
begin
  log := aLog.Enter('Create(%,%,%)',
    [aConnectionClass, ProcessName, aThreadPoolCount], self);
  if (aConnectionClass = TAsyncConnection) or
     (aConnectionClass = nil) then
    raise EAsyncConnections.CreateUtf8('Unexpected %.Create(%)',
      [self, aConnectionClass]);
  if aThreadPoolCount <= 0 then
    aThreadPoolCount := 1;
  fLastOperationReleaseMemorySeconds := 60;
  fLastOperationSec := Qword(mormot.core.os.GetTickCount64) div 1000; // ASAP
  fKeepConnectionInstanceMS := 100;
  fThreadPollingWakeupLoad := 32; // see ThreadPollingWakeup() below
  fLog := aLog;
  fConnectionClass := aConnectionClass;
  if not (acoNoConnectionTrack in aOptions) then
    fConnectionLock.Init;
  opt := [];
  if acoWritePollOnly in aOptions then
    include(opt, paoWritePollOnly);
  fClients := TAsyncConnectionsSockets.Create(opt);
  fClients.fOwner := self;
  fClientsEpoll := fClients.fRead.PollClass.FollowEpoll;
  fClients.OnStart := ProcessClientStart;
  fClients.fWrite.OnGetOneIdle := ProcessIdleTix;
  if Assigned(fLog) and
     (acoDebugReadWriteLog in aOptions) then
  begin
    fClients.fDebugLog := fLog;
    fClients.fRead.OnLog := fLog.DoLog;
    fClients.fWrite.OnLog := fLog.DoLog;
  end;
  fOptions := aOptions;
  // prepare this main thread: fThreads[] requires proper fOwner.OnStart/OnStop
  inherited Create({suspended=}false, OnStart, OnStop, ProcessName);
  // initiate the read/receive thread(s)
  fThreadPoolCount := aThreadPoolCount;
  SetLength(fThreads, fThreadPoolCount);
  if aThreadPoolCount = 1 then
    fThreads[0] := TAsyncConnectionsThread.Create(self, atpReadSingle, 0)
  else
  begin
    fThreadReadPoll := TAsyncConnectionsThread.Create(self, atpReadPoll, 0);
    fThreads[0] := fThreadReadPoll;
    for i := 1 to aThreadPoolCount - 1 do
      fThreads[i] := TAsyncConnectionsThread.Create(self, atpReadPending, i);
  end;
  tix := mormot.core.os.GetTickCount64 + 7000;
  repeat
     if AllThreadsStarted then
       break;
     SleepHiRes(1);
  until mormot.core.os.GetTickCount64 > tix;
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

procedure TAsyncConnections.AddGC(aConnection: TPollAsyncConnection);
begin
  if Terminated then
    exit;
  (aConnection as TAsyncConnection).fLastOperation := fLastOperationMS; // in ms
  with fGC[1] do // add to 1st generation
  begin
    Safe.Lock;
    ObjArrayAddCount(Items, aConnection, Count);
    Safe.UnLock;
  end;
end;

function OneGC(var gen, dst: TAsyncConnectionsGC;
  lastop, oldenough: TAsyncConnectionSec): PtrInt;
var
  c: TAsyncConnection;
  i: PtrInt;
begin
  result := 0;
  if gen.Count = 0 then
    exit;
  oldenough := lastop - oldenough;
  for i := 0 to gen.Count - 1 do
  begin
    c := gen.Items[i];
    if c.fLastOperation <= oldenough then
    begin
      // release after timeout
      if dst.Count >= length(dst.Items) then
        SetLength(dst.Items, dst.Count + gen.Count - i);
      dst.Items[dst.Count] := c;
      inc(dst.Count);
    end
    else
    begin
      if c.fLastOperation > lastop then
        // reset time flag after 42 days / 32-bit overflow
        c.fLastOperation := lastop;
      gen.Items[result] := c; // keep if not fKeepConnectionInstanceMS old
      inc(result);
    end;
  end;
  gen.Count := result; // don't resize gen.Items[] to avoid realloc
end;

procedure TAsyncConnections.DoGC;
var
  tofree: TAsyncConnectionsGC;
  n1, n2: integer;
begin
  if Terminated or
     (fGC[1].Count + fGC[2].Count = 0) then
    exit;
  fGC[2].Safe.Lock;
  try
    fGC[1].Safe.Lock;
    try
      // keep in first generation GC for 100 ms by default
      n1 := OneGC(fGC[1], fGC[2], fLastOperationMS, fKeepConnectionInstanceMS);
    finally
      fGC[1].Safe.UnLock;
    end;
    // wait 2 seconds until no pending event is in queue and free instances
    tofree.Count := 0;
    n2 := OneGC(fGC[2], tofree, fLastOperationMS, 2000);
  finally
    fGC[2].Safe.UnLock;
  end;
  if n1 + n2 + tofree.Count = 0 then
    exit;
  // np := fClients.fRead.DeleteSeveralPending(pointer(gc), ngc); always 0
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace,  'DoGC #1=% #2=% free=%', [n1, n2, tofree.Count], self);
  if tofree.Count <> 0 then
    ObjArrayClear(tofree.Items, {continueonexc=}true, @tofree.Count);
end;

procedure TAsyncConnections.Shutdown;
var
  i, n, p: PtrInt;
  endtix: Int64;
begin
  Terminate;
  // terminate the main clients asynchronous logic
  if fClients <> nil then
  begin
    with fClients do
      DoLog('Shutdown threads=% total=% reads=%/% writes=%/% now=% hi=%',
        [length(fThreads), Total, ReadCount, KB(ReadBytes),
         WriteCount, KB(WriteBytes), fConnectionCount, fConnectionHigh]);
    fClients.Terminate(5000);
  end;
  // terminate and unlock background ProcessRead/ProcessWrite polling threads
  if fThreads <> nil then
  begin
    for i := 0 to high(fThreads) do
      fThreads[i].Terminate;
    p := 0;
    endtix := mormot.core.os.GetTickCount64 + 10000;
    repeat
      n := 0;
      for i := 0 to high(fThreads) do
        with fThreads[i] do
          if fExecuteState = esRunning then
          begin
            if fEvent <> nil then
              fEvent.SetEvent; // release any (e.g. atpReadPoll) lock
            if p and 15 = 0 then
              DoLog(sllTrace, 'Shutdown unfinished=%', [fThreads[i]], self);
            inc(n);
          end;
      if n = 0 then
        break;
      SleepHiRes(1);
      inc(p);
    until mormot.core.os.GetTickCount64 > endtix;
    FreeAndNilSafe(fClients); // FreeAndNil() sets nil before which is incorrect
    ObjArrayClear(fThreads, {continueonexception=}true);
  end;
  // there may be some trailing connection instances to be released
  for i := low(fGC) to high(fGC) do
    with fGC[i] do
      if Count <> 0 then
      begin
        DoLog(sllTrace, 'Shutdown GC#%=%', [i, Count], self);
        ObjArrayClear(Items, {continueonexception=}true, @Count);
      end;
end;

destructor TAsyncConnections.Destroy;
begin
  Shutdown;
  inherited Destroy;
  if not (acoNoConnectionTrack in fOptions) then
  begin
    if fConnectionCount <> 0 then
    begin
      // they are normally no pending connection anymore
      if Assigned(fLog) then
        fLog.Add.Log(sllTrace, 'Destroy: GC connections=%', [fConnectionCount], self);
      ObjArrayClear(fConnection, {continueonexception=}true, @fConnectionCount);
    end;
  end;
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
    res := NewSocket(Address, Port, nlTcp, {bind=}false, timeout, timeout,
      timeout, {retry=}0, client, @addr);
  if res = nrOk then
    res := client.MakeAsync;
  if res <> nrOK then
    raise EAsyncConnections.CreateUtf8('%: %:% connection failure (%)',
      [self, fThreadClients.Address, fThreadClients.Port, ToText(res)^]);
  // create and register the async connection as in TAsyncServer.Execute
  if not ConnectionCreate(client, addr, result) then
    client.ShutdownAndClose({rdwr=}false)
  else if not fClients.Start(result) then
    FreeAndNil(result);
end;

  // NOTICE on ThreadPollingWakeupLoad / acoThreadSmooting option (i.e.
  // how many events a fast active thread is supposed to handle in its loop)
  // - the naive/standard/well-used algorithm of waking up the threads on need
  // does not perform well, especially with a high number of CPU cores: the
  // global CPU usage remains idle, because most of the time is spent between
  // the threads, and not processing actual data
  // - we actually wake up the sub-threads only if it did not become idle within
  // GetTickCount64 resolution (i.e. 16ms on Windows, 4ms on Linux)
  // - on small load or quick response, only R1 thread is involved
  // - on slow process (e.g. DB access), R1 is identified as blocking, and
  // R2..Rmax threads are awaken once WAKEUP_LOAD events have been assigned
  // to processing threads
  // - it seems to leverage the CPU performance especially when the number of
  // threads is higher than the number of cores, or on high number of cores CPU
  // - this algorithm seems efficient, and simple enough to implement and debug,
  // in respect to what I have seen in high-performance thread pools (e.g. in
  // MariaDB), which have much bigger complexity (like a dynamic thread pool)
  // - current default value of 32 has been set from trials of wrk benchmarks

function TAsyncConnections.ThreadPollingWakeup(Events: PtrInt): PtrInt;
var
  i: PtrInt;
  th: PAsyncConnectionsThread;
  t: TAsyncConnectionsThread;
  tix: integer; // 32-bit is enough to check for
  ndx: array[byte] of byte; // wake up to 256 threads at once
begin
  // simple thread-safe fair round-robin over fThreads[]
  if Events > high(ndx) then
    Events := high(ndx); // paranoid avoid ndx[] buffer overflow
  result := 0;
  //{$I-}system.writeln('wakeup=',Events);
  fThreadPollingLastWakeUpTix := mormot.core.os.GetTickCount64; // 16ms Windows, 4ms Linux
  if (acoThreadSmooting in fOptions) and
     (Events > 1) then
    tix := fThreadPollingLastWakeUpTix
  else
    tix := 0; // after accept() or on idle server, we can always wake threads
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
          // this thread is currently idle and can be used
          t.fWaitForReadPending := false; // acquire this thread
          ndx[result] := i; // notify outside of fThreadPollingWakeupSafe lock
          inc(result);
          dec(Events);
        end;
      end
      // fast working threads are available for up to WAKEUP_LOAD events
      else if not t.fWaitForReadPending and
              (t.fThreadPollingLastWakeUpCount > 0) and
              (t.fThreadPollingLastWakeUpTix = tix) then
      begin
        // this thread is likely to be available very soon: consider it done
        //{$I-}system.writeln(t.Name,' cnt=',t.fThreadPollingLastWakeUpCount,'-',Events);
        dec(t.fThreadPollingLastWakeUpCount, Events);
        dec(Events, t.fThreadPollingLastWakeUpCount);
      end
      else if t.fWaitForReadPending then
      begin
        // we need to wake up a thread, since some slow work is going on
        t.fThreadPollingLastWakeUpTix := tix;
        t.fThreadPollingLastWakeUpCount := fThreadPollingWakeupLoad - Events;
        //{$I-}system.writeln('wakeup #', t.Name,' cnt=',t.fThreadPollingLastWakeUpCount);
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
  for i := 0 to result - 1 do
    fThreads[ndx[i]].fEvent.SetEvent; // notify outside fThreadPollingWakeupSafe
end;

procedure TAsyncConnections.DoLog(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArgs: array of const; Instance: TObject);
begin
  if (self <> nil) and
     Assigned(fLog) then
    fLog.Add.Log(Level, TextFmt, TextArgs, Instance);
end;

function TAsyncConnections.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: TNetAddr; out aConnection: TAsyncConnection): boolean;
begin
  // you can override this class then call ConnectionNew
  if Terminated then
    result := false
  else
  begin
    aConnection := nil;
    with fGC[2] do // recycle 2nd gen instances e.g. for short-living HTTP/1.0
      if (Count > 0) and
         Safe.TryLock then
      begin
        if Count > 0 then
        begin
          dec(Count);
          aConnection := Items[Count];
        end;
        Safe.UnLock;
      end;
    if aConnection = nil then
      aConnection := fConnectionClass.Create(self, aRemoteIp)
    else
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
  if fLastHandle < 0 then // paranoid check
    raise EAsyncConnections.CreateUtf8(
      '%.ConnectionNew: %.Handle overflow', [self, aConnection]);
  aConnection.fSocket := aSocket;
  aConnection.fHandle := InterlockedIncrement(fLastHandle);
  if acoNoConnectionTrack in fOptions then
  begin
    include(aConnection.fFlags, fInList);
    LockedInc32(@fConnectionCount);
  end
  else if (fThreadReadPoll = nil) or
          aAddAndSubscribe then
    // ProcessClientStart() won't delay SuscribeConnection + RegisterConnection
    ConnectionAdd(aConnection);
  aConnection.AfterCreate; // Handle has been computed
  if acoVerboseLog in fOptions then
    DoLog(sllTrace, 'ConnectionNew % sock=% count=% gc=%',
      [aConnection, pointer(aSocket), fConnectionCount,
       fFromGC in aConnection.fFlags], self);
  result := true; // indicates aSocket owned by the pool
end;

function TAsyncConnections.LockedConnectionDelete(aConnection: TAsyncConnection;
  aIndex: integer): boolean;
var
  n: PtrInt;
  start, stop: Int64;
begin
  // caller should have done fConnectionLock.Lock(cWrite)
  try
    if acoVerboseLog in fOptions then
      QueryPerformanceMicroSeconds(start);
    PtrArrayDelete(fConnection, aIndex, @fConnectionCount);
    n := fConnectionCount;
    if (n > 256) and
       (length(fConnection) > n shl 1) then
      SetLength(fConnection, n + n shr 3); // reduce 50% free into 12.5%
    if acoVerboseLog in fOptions then
    begin
      QueryPerformanceMicroSeconds(stop); // a few us at most
      DoLog(sllTrace, 'ConnectionDelete % ndx=% count=% %us',
        [aConnection, aIndex, n, stop - start], self);
    end;
    aConnection.fSocket := nil;   // ensure is known as disabled
    AddGC(aConnection); // will be released once processed
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
  i: integer;
  conn: TAsyncConnection;
begin
  // don't call fClients.Stop() here - see ConnectionRemove()
  result := false;
  if Terminated or
     (aConnection = nil) or
     (aConnection.Handle <= 0) then
    exit;
  if not (fInList in aConnection.fFlags) then
  begin
    // this connection was not part of fConnection[] list nor subscribed
    // e.g. HTTP/1.0 short request -> explicit GC - Free is unstable here
    AddGC(aConnection);
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
  aHandle: TPollAsyncConnectionHandle; aLock: TRWLockContext;
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
    raise EAsyncConnections.CreateUtf8(
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
  aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
var
  i, n: PtrInt;
begin
  // caller should have made fConnectionLock.Lock()
  result := nil;
  if aHandle <= 0 then
    exit;
  i := fLastConnectionFind;
  n := fConnectionCount;
  if (i >= n) or
     (fConnection[i].Handle <> aHandle) then
    i := FastFindConnection(pointer(fConnection), n - 1, aHandle); // O(log(n))
  if i >= 0 then
  begin
    fLastConnectionFind := i;
    result := fConnection[i];
    if result.IsClosed then
      result := nil; // too late
  end;
end;

function TAsyncConnections.ConnectionFind(
  aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
begin
  result := nil;
  if (self = nil) or
     Terminated or
     (aHandle <= 0) or
     (acoNoConnectionTrack in fOptions) then
    exit;
  fConnectionLock.ReadOnlyLock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    result := LockedConnectionSearch(aHandle); // O(log(n)) binary search
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fConnectionLock.ReadOnlyUnLock;
  end;
end;

function TAsyncConnections.ConnectionRemove(
  aHandle: TPollAsyncConnectionHandle): boolean;
var
  i: integer;
  conn: TAsyncConnection;
begin
  result := false;
  if (self = nil) or
     Terminated or
     (aHandle <= 0) then
    exit;
  conn := ConnectionFindAndLock(aHandle, cWrite, @i);
  if conn <> nil then
    try
      if not fClients.Stop(conn) then
        DoLog(sllDebug, 'ConnectionRemove: Stop=false for %', [conn], self);
      result := LockedConnectionDelete(conn, i);
    finally
      fConnectionLock.WriteUnLock;
    end;
  if not result then
    DoLog(sllTrace, 'ConnectionRemove(%)=false', [aHandle], self);
end;

procedure TAsyncConnections.EndConnection(connection: TAsyncConnection);
begin
  if acoNoConnectionTrack in fOptions then
  begin
    connection.fSocket := nil;
    AddGC(connection); // will be released once processed
    InterlockedDecrement(fConnectionCount);
  end
  else
    ConnectionDelete(connection);
end;

procedure TAsyncConnections.Lock(aLock: TRWLockContext);
begin
  if acoNoConnectionTrack in fOptions then
    raise EAsyncConnections.CreateUtf8('Unexpected %.Lock', [self]);
  fConnectionLock.Lock(aLock);
end;

procedure TAsyncConnections.Unlock(aLock: TRWLockContext);
begin
  if acoNoConnectionTrack in fOptions then
    raise EAsyncConnections.CreateUtf8('Unexpected %.UnLock', [self]);
  fConnectionLock.UnLock(aLock);
end;

function TAsyncConnections.Write(connection: TAsyncConnection; data: pointer;
  datalen: integer; timeout: integer): boolean;
begin
  if Terminated then
    result := false
  else
    result := fClients.Write(connection, data, datalen, timeout);
end;

function TAsyncConnections.WriteString(connection: TAsyncConnection;
  const data: RawByteString; timeout: integer): boolean;
begin
  if Terminated then
    result := false
  else
    result := fClients.WriteString(connection, data, timeout);
end;

procedure TAsyncConnections.LogVerbose(connection: TPollAsyncConnection;
  const ident: RawUtf8; const identargs: array of const;
  frame: pointer; framelen: integer);
var
  tmp: TLogEscape; // 512 bytes of temp buffer
begin
  if (acoVerboseLog in Options) and
     (fLog <> nil) then
    DoLog(sllTrace, '% len=%%',
      [FormatToShort(ident, identargs), framelen,
       LogEscape(frame, framelen, tmp{%H-})], connection);
end;

procedure TAsyncConnections.LogVerbose(connection: TPollAsyncConnection;
  const ident: RawUtf8; const identargs: array of const; const frame: TRawByteStringBuffer);
begin
  LogVerbose(connection, ident, identargs, frame.Buffer, frame.Len)
end;

procedure TAsyncConnections.IdleEverySecond;
var
  i: PtrInt;
  notified, gced: PtrInt;
  sec, allowed, gc: cardinal;
  c: TAsyncConnection;
begin
  if Terminated or
     (fConnectionCount = 0) or
     (acoNoConnectionTrack in fOptions) then
    exit;
  notified := 0;
  gced := 0;
  sec := fLastOperationSec; // 32-bit second resolution is fine
  allowed := fLastOperationIdleSeconds;
  if allowed <> 0 then
    allowed := sec - allowed;
  gc := fLastOperationReleaseMemorySeconds;
  if gc <> 0 then
    gc := sec - gc;
  fConnectionLock.ReadOnlyLock;
  try
    for i := 0 to fConnectionCount - 1 do
    begin
      c := fConnection[i];
      if fWasActive in c.fFlags then
      begin
        exclude(c.fFlags, fWasActive); // update once per second is good enough
        c.fLastOperation := sec;
      end
      else
      begin
        // check if some events should be triggerred on this inactive connection
        // e.g. TWebSocketAsyncConnection would send ping/pong heartbeats
        if (gc <> 0) and
           (c.fLastOperation < gc) then
          inc(gced, c.ReleaseMemoryOnIdle);
        if (allowed <> 0) and
           (c.fLastOperation < allowed) then
          try
            if c.OnLastOperationIdle(sec) then
              inc(notified);
            if Terminated then
              break;
          except
          end;
      end;
    end;
    if (acoVerboseLog in fOptions) and
       ((notified <> 0) or
        (gced <> 0)) then
      DoLog(sllTrace, 'IdleEverySecond % notif=% GC=%',
        [fConnectionClass, notified, KBNoSpace(gced)], self);
  finally
    fConnectionLock.ReadOnlyUnLock;
  end;
end;

procedure TAsyncConnections.ProcessIdleTix(Sender: TObject; NowTix: Int64);
var
  sec: TAsyncConnectionSec;
begin
  // called from fClients.fWrite.OnGetOneIdle callback
  if Terminated then
    exit;
  fLastOperationMS := NowTix;
  DoGC;
  sec := Qword(NowTix) div 1000; // 32-bit second resolution is fine
  if sec = fLastOperationSec then
    exit; // not a new second tick yet
  fLastOperationSec := sec;
  IdleEverySecond;
  // note: this method should be non-blocking and return quickly
  // e.g. overriden in TWebSocketAsyncConnections to send pending frames
 end;

function TAsyncConnections.ProcessClientStart(Sender: TPollAsyncConnection): boolean;
begin
  if fThreadReadPoll <> nil then
  begin
    // initial accept() will be directly redirected to atpReadPending threads
    // with no initial fRead.SubScribe() to speed up e.g. HTTP/1.0
    fClients.fRead.AddOnePending(TPollSocketTag(Sender), [pseRead],
      {aSearchExisting=} false{fFromGC in Sender.fFlags});
    ThreadPollingWakeup(1);
    result := true; // no Subscribe() -> delayed in atpReadPending if needed
  end
  else
    result := false; // Subscribe() is done by TPollAsyncSockets.Start caller
end;


{ TAsyncServer }

constructor TAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; aConnectionClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fSockPort := aPort;
  fMaxConnections := 7777777; // huge number for sure
  fMaxPending := 10000;       // fair enough for pending requests
  inherited Create(OnStart, OnStop, aConnectionClass, ProcessName, aLog,
    aOptions, aThreadPoolCount);
  if acoEnableTls in aOptions then
    fClients.OnFirstRead := OnFirstReadDoTls;
  // binding will be done in Execute
end;

procedure TAsyncServer.WaitStarted(seconds: integer);
var
  tix: Int64;
begin
  if self = nil then
    raise EAsyncConnections.CreateUtf8(
      'TAsyncServer.WaitStarted(%) with self=nil', [seconds]);
  tix := mormot.core.os.GetTickCount64 + seconds * 1000; // never wait forever
  repeat
    if Terminated then
      exit;
    case fExecuteState of
      esRunning:
        exit;
      esFinished:
        raise EAsyncConnections.CreateUtf8('%.Execute aborted as %',
          [self, fExecuteMessage]);
    end;
    SleepHiRes(1); // warning: waits typically 1-15 ms on Windows
    if mormot.core.os.GetTickCount64 > tix then
      raise EAsyncConnections.CreateUtf8(
        '%.WaitStarted timeout after % seconds', [self, seconds]);
  until false;
end;

procedure TAsyncServer.Shutdown;
var
  i: PtrInt;
  len: integer;
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
    DoLog(sllTrace, 'Shutdown %:% release request', [host, port], self);
    if NewSocket(host, port{%H-}, fServer.SocketLayer, false,
         10, 0, 0, 0, touchandgo) = nrOk then
    begin
      if fClientsEpoll then
      begin
        len := 1;
        touchandgo.Send(@len, len);    // release epoll_wait() in R0 thread
        ev := touchandgo.WaitFor(100, [neRead]);
        DoLog(sllTrace, 'Shutdown epoll WaitFor=%', [byte(ev)], self);
        SleepHiRes(1);
      end;
      touchandgo.ShutdownAndClose(false);  // release the AW thread
    end;
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
  end;
end;

destructor TAsyncServer.Destroy;
var
  endtix: Int64;
begin
  endtix := mormot.core.os.GetTickCount64 + 10000;
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
    raise EAsyncConnections.CreateUtf8('Unexpected %.OnFirstReadDoTls', [self]);
  if not fServer.TLS.Enabled then  // if not already done in WaitStarted()
  begin
    fGC[1].Safe.Lock; // load certificates once from first connected thread
    try
      fServer.DoTlsAfter(cstaBind);  // validate certificates now
    finally
      fGC[1].Safe.UnLock;
    end;
  end;
  // TAsyncServer.Execute made Accept(async=false) from acoEnableTls
  Sender.fSecure := NewNetTls;  // should work since DoTlsAfter() was fine
  Sender.fSecure.AfterAccept(Sender.fSocket, fServer.TLS, nil, nil);
  Sender.fSocket.MakeAsync;     // as expected by our asynchronous code
end;

procedure TAsyncServer.SetExecuteState(State: THttpServerExecuteState);
begin
  fExecuteState := State;
  DoLog(sllInfo, 'Execute: State=%',
    [GetEnumName(TypeInfo(THttpServerExecuteState), ord(State))^], self);
end;

procedure TAsyncServer.Execute;
var
  notif: TPollSocketResult;
  client: TNetSocket;
  connection: TAsyncConnection;
  res: TNetResult;
  async: boolean;
  start: Int64;
  len: integer;
  sin: TNetAddr;
const
  AW: array[boolean] of string[1] = ('W', '');
begin
  // Accept() incoming connections
  // and Send() output packets in the background if fExecuteAcceptOnly=false
  SetCurrentThreadName('A% %', [AW[fExecuteAcceptOnly], fProcessName]);
  NotifyThreadStart(self);
  try
    // create and bind fServer to the expected TCP port
    SetExecuteState(esBinding);
    // BIND + LISTEN (TLS is done later)
    fServer := TCrtSocket.Bind(fSockPort, nlTcp, 5000, acoReusePort in Options);
    if not fServer.SockIsDefined then // paranoid check
      raise EAsyncConnections.CreateUtf8('%.Execute: bind failed', [self]);
    SetExecuteState(esRunning);
    if not fExecuteAcceptOnly then
      // setup the main bound connection to be polling together with the writes
      if fClients.fWrite.Subscribe(fServer.Sock, [pseRead], {tag=}0) then
        fClients.fWrite.PollForPendingEvents(0) // actually subscribe
      else
        raise EAsyncConnections.CreateUtf8('%.Execute: no accept sub', [self]);
    // main socket accept/send processing loop
    async := false; // first Accept() will be blocking
    start := 0;
    while not Terminated do
    begin
      if not async then
        PQWord(@notif)^ := 0 // blocking initial accept()
      else if not fClients.fWrite.GetOne(900, 'AW', notif) then
        continue;
      if Terminated then
        break;
      if ResToTag(notif) = 0 then // no tag = main accept()
      begin
        repeat
          // could we Accept one or several incoming connection(s)?
          // async=true to expect client in non-blocking mode from now on
          // will use accept4() single syscall on Linux
          {DoLog(sllCustom1, 'Execute: before accepted=%', [fAccepted], self);}
          res := fServer.Sock.Accept(client, sin,
            {async=}not (acoEnableTls in fOptions)); // see OnFirstReadDoTls
          {DoLog(sllTrace, 'Execute: Accept(%)=% sock=% #% hi=%', [fServer.Port,
            ToText(res)^, pointer(client), fAccepted, fConnectionHigh], self);}
          if Terminated then
          begin
            // specific behavior from Shutdown method
            if fClientsEpoll and
               (res = nrOK) then
            begin
              DoLog(sllTrace, 'Execute: Accept(%) release', [fServer.Port], self);
              // background subscribe to release epoll_wait() in R0 thread
              fClients.fRead.Subscribe(client, [pseRead], {tag=}0);
              len := 1;
              client.Send(@len, len); // release touchandgo.WaitFor
            end;
            break;
          end;
          {if res = nrRetry then
            DoLog(sllCustom1, 'Execute: Accept(%) retry', [fServer.Port], self);}
          if res = nrRetry then
            break; // 10 seconds timeout
          if (fBanned <> nil) and
             (fBanned.Count <> 0) and
             fBanned.IsBanned(sin) then // IP filtering from blacklist
          begin
            if acoVerboseLog in fOptions then
              DoLog(sllTrace, 'Execute: ban=%', [CardinalToHexShort(sin.IP4)], self);
            len := ord(HTTP_BANIP_RESPONSE[0]);
            client.Send(@HTTP_BANIP_RESPONSE[1], len); // 418 I'm a teapot
            client.ShutdownAndClose({rdwr=}false);    // reject before TLS setup
            continue;
          end;
          inc(fAccepted);
          if (fClients.fRead.Count > fMaxConnections) or
             (fClients.fRead.PendingCount > fMaxPending) then
          begin
            // map THttpAsyncServer.HttpQueueLength property value
            DoLog(sllWarning,
              'Execute: Accept connections=%>% pending=%>% overflow',
              [fClients.fRead.Count, fMaxConnections,
               fClients.fRead.PendingCount, fMaxPending], self);
            client.ShutdownAndClose({rdwr=}false); // e.g. for load balancing
            res := nrTooManyConnections;
          end;
          if res <> nrOK then
          begin
            // failure (too many clients?) -> wait and retry
            DoLog(sllWarning, 'Execute: Accept(%) failed as %',
              [fServer.Port, ToText(res)^], self);
            // progressive wait on socket error, including nrTooManyConnections
            SleepStep(start);
            break;
          end;
          if Terminated then
            break;
          // if we reached here, we have accepted a connection -> process
          start := 0;
          if ConnectionCreate(client, sin, connection) then
          begin
            // no log here, because already done in ConnectionNew and Start()
            // may do connection.Free in atpReadPending background -> log before
            if fClients.Start(connection) then
            begin
              if (not async) and
                 not fExecuteAcceptOnly then
              begin
                fServer.Sock.MakeAsync; // share thread with Writes
                async := true;
              end;
            end
            else if connection <> nil then // connection=nil for custom list
              ConnectionDelete(connection);
          end
          else
            client.ShutdownAndClose({rdwr=}false);
        until Terminated;
      end
      else
        // this was a pseWrite notification -> try to send pending data
        // here connection = TObject(notif.tag)
        // - never executed if fExecuteAcceptOnly=true (THttpAsyncServer)
        fClients.ProcessWrite(notif);
    end;
  except
    on E: Exception do
    begin
      // callback exceptions should all be catched: so we assume that any
      // exception in mORMot code should be considered as fatal
      FormatUtf8('% [%]', [E, E.Message], fExecuteMessage);
      DoLog(sllWarning, 'Execute raised uncatched % -> terminate %',
        [E.ClassType, fProcessName], self);
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
  inherited Create(OnStart, OnStop, aConnectionClass, ProcessName,
    aLog, aOptions, aThreadPoolCount);
end;

procedure TAsyncClient.Execute;
var
  notif: TPollSocketResult;
begin
  SetCurrentThreadName('C % %', [fProcessName, self]);
  NotifyThreadStart(self);
  try
    if fThreadClients.Count > 0 then
      while InterlockedDecrement(fThreadClients.Count) >= 0 do
        // will first connect some clients in this main thread
        ThreadClientsConnect;
    while not Terminated do
      if fClients.fWrite.GetOne(1000, 'C', notif) then
        fClients.ProcessWrite(notif);
    DoLog(sllInfo, 'Execute: done % C', [fProcessName], self);
  except
    on E: Exception do
      DoLog(sllWarning, 'Execute raised % -> terminate %',
        [E.ClassType, fProcessName], self);
  end;
end;


{ ******************** THttpAsyncServer Event-Driven HTTP Server }

{ THttpAsyncConnection }

procedure THttpAsyncConnection.AfterCreate;
begin
  fServer := (fOwner as THttpAsyncConnections).fAsyncServer;
  if fServer <> nil then
  begin
    HttpInit;
    fHttp.Interning := fServer.fInterning;
    fHttp.Compress := fServer.fCompress;
    fHttp.CompressAcceptEncoding := fServer.fCompressAcceptEncoding;
    if fServer.fServerKeepAliveTimeOutSec <> 0 then
      fKeepAliveSec := fServer.Async.fLastOperationSec +
                       fServer.fServerKeepAliveTimeOutSec;
  end;
  // inherited AfterCreate; // void parent method
end;

procedure THttpAsyncConnection.Recycle(const aRemoteIP: TNetAddr);
begin
  inherited Recycle(aRemoteIP);
  fConnectionOpaque.ValueInternal := 0;
  fConnectionOpaque.ValueExternal := 0;
  if fServer <> nil then
  begin
    if fServer.fServerKeepAliveTimeOutSec <> 0 then
      fKeepAliveSec := fServer.Async.fLastOperationSec +
                       fServer.fServerKeepAliveTimeOutSec;
  end;
end;

procedure THttpAsyncConnection.BeforeDestroy;
begin
  fHttp.ProcessDone;
  FreeAndNil(fRequest);
  inherited BeforeDestroy;
end;

procedure THttpAsyncConnection.HttpInit;
begin
  fHttp.ProcessInit({instream=}nil); // ready to process this HTTP request
  if hsoHeadersUnfiltered in fServer.Options then
    include(fHttp.Options, hroHeadersUnfiltered);
  fHttp.Head.Reserve(fServer.HeadersDefaultBufferSize); // 2KB by default
  fHeadersSec := 0;
end;

function THttpAsyncConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  st: TProcessParseLine;
begin
  if (fOwner.fLog <> nil) and
     (acoVerboseLog in fOwner.Options) and
     not (acoNoLogRead in fOwner.Options) then
    fOwner.LogVerbose(self, 'OnRead %', [ToText(fHttp.State)^], fRd);
  result := soClose;
  if fOwner.fClients = nil then
    fHttp.State := hrsErrorMisuse
  else if fServer.fShutdownInProgress then
    fHttp.State := hrsErrorShutdownInProgress
  else
  begin
    // use the HTTP state machine to asynchronously parse fRd input
    result := soContinue;
    if fHttp.Process.Len = 0 then
    begin
      st.P := fRd.Buffer;
      st.Len := fRd.Len;
    end
    else
    begin
      fHttp.Process.Append(fRd.Buffer, fRd.Len);
      st.P := fHttp.Process.Buffer;
      st.Len := fHttp.Process.Len;
    end;
    while fHttp.ProcessRead(st) do
    begin
      {fOwner.DoLog(sllCustom2, 'OnRead % st.len=%', [ToText(fHttp.State)^, st.Len], self);}
      // handle main steps change
      case fHttp.State of
        hrsGetBodyChunkedHexFirst,
        hrsGetBodyContentLength:
          // we just received command + all headers
          result := DoHeaders;
        hrsWaitProcessing:
          // calls the (blocking) HTTP request processing callback
          result := DoRequest;
      else
        begin
          fOwner.DoLog(sllWarning, 'OnRead: close connection after %',
            [ToText(fHttp.State)^], self);
          result := soClose;
        end;
      end;
      if (result <> soContinue) or
         (fHttp.State = hrsUpgraded) then
        break; // rejected or upgraded
    end;
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
    {fOwner.DoLog(sllCustom2, 'OnRead % result=%', [ToText(fHttp.State)^, ord(result)], self);}
    // finalize the memory buffers
    if st.Len = 0 then
      fHttp.Process.Reset // all input processed (usual and nominal case)
    else if fHttp.Process.Len <> 0 then
      fHttp.Process.Remove(st.P - fHttp.Process.Buffer) // some input processed
    else
    begin
      fHttp.Process.Reset;
      fHttp.Process.Append(st.P, st.Len); // keep remaining input for next time
    end;
    fRd.Reset;
  end;
end;

function THttpAsyncConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
begin
  result := soContinue;
  if fOwner.fClients = nil then
    fHttp.State := hrsErrorMisuse;
  // compute next step
  if fHttp.State = hrsSendBody then
  begin
    // use the HTTP state machine to fill fWr with outgoing body chunk
    fHttp.ProcessBody(fWr, fOwner.fClients.fSendBufferSize);
    if acoVerboseLog in fOwner.fOptions then
      fOwner.DoLog(sllTrace, 'AfterWrite SendBody ContentLength=% Wr=%',
        [fHttp.ContentLength, fWr.Len], self);
    if fWr.Len <> 0 then
      // need to continue background sending
      exit;
  end
  else if fHttp.State <> hrsResponseDone then
  begin
    fOwner.DoLog(sllWarning, 'AfterWrite: unexpected %',
      [ToText(fHttp.State)^], self);
    result := soClose;
    exit;
  end;
  // whole headers (+ body) outgoing content was sent
  if acoVerboseLog in fOwner.fOptions then
    fOwner.DoLog(sllTrace, 'AfterWrite Done ContentLength=% Wr=% Flags=%',
      [fHttp.ContentLength, fWr.Len, ToText(fHttp.HeaderFlags)], self);
  if Assigned(fServer.fOnAfterResponse) then
    try
      fServer.fOnAfterResponse(
        fHttp.CommandMethod, fHttp.CommandUri, fRemoteIP, fRespStatus);
    except
      on E: Exception do
      begin
        include(fHttp.HeaderFlags, hfConnectionClose);
        if acoVerboseLog in fOwner.fOptions then
          fOwner.DoLog(sllTrace, 'AfterWrite OnAfterResponse raised %',
            [E], self);
      end;
    end;
  fHttp.ProcessDone;   // ContentStream.Free
  fHttp.Process.Clear; // CompressContentAndFinalizeHead may have set it
  if hfConnectionClose in fHttp.HeaderFlags then
    // connection: close -> shutdown and clear the connection
    result := soClose
  else
  begin
    // kept alive connection -> reset the HTTP parser and continue
    HttpInit;
    result := soContinue;
  end;
end;

function THttpAsyncConnection.DecodeHeaders: integer;
begin
  result := HTTP_SUCCESS; // indicates we can continue the request process
  if (fServer.MaximumAllowedContentLength > 0) and
     (fHttp.ContentLength > fServer.MaximumAllowedContentLength) then
  begin
    // 413 HTTP error if requested payload is too big (default is 0 = no limit)
    result := HTTP_PAYLOADTOOLARGE;
    fServer.IncStat(grOversizedPayload);
  end
  else if Assigned(fServer.OnBeforeBody) then
    // custom validation (e.g. missing/invalid URL or BearerToken)
    result := fServer.OnBeforeBody(
      fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers, fHttp.ContentType,
      fRemoteIP, fHttp.BearerToken, fHttp.ContentLength,
      HTTP_TLS_FLAGS[Assigned(fSecure)] +
      HTTP_UPG_FLAGS[hfConnectionUpgrade in fHttp.HeaderFlags]);
end;

procedure THttpAsyncConnection.DoReject(status: integer);
var
  len: integer;
begin
  StatusCodeToReason(status, fHttp.UserAgent);
  // (use fHttp fields as temp var to avoid local RawUtf8 allocation)
  FormatUtf8('HTTP/1.0 % %'#13#10 + TEXT_CONTENT_TYPE_HEADER + #13#10#13#10 +
    '% Server rejected % request as % %',
      [status, fHttp.UserAgent, fHttp.Host, fHttp.CommandUri,
       status, fHttp.UserAgent], fHttp.CommandResp);
  len := length(fHttp.CommandResp);
  Send(pointer(fHttp.CommandResp), len); // no polling nor ProcessWrite
  fServer.IncStat(grRejected);
  fHttp.State := hrsErrorRejected;
  if (fServer.Async.Banned <> nil) and
     not IsUrlFavIcon(pointer(fHttp.CommandUri)) and
     fServer.Async.Banned.BanIP(fRemoteIP4) then
  begin
    fOwner.DoLog(sllTrace, 'DoReject(%): BanIP(%) %',
      [status, fRemoteIP, fServer.Async.Banned], self);
    fServer.IncStat(grBanned);
  end;
end;

function THttpAsyncConnection.DoHeaders: TPollAsyncSocketOnReadWrite;
var
  status: integer;
begin
  // finalize the headers
  result := soClose;
  if (nfHeadersParsed in fHttp.HeaderFlags) or
     not fHttp.ParseCommand then
    exit;
  fHttp.ParseHeaderFinalize;
  // immediate reject of clearly invalid requests
  status := DecodeHeaders; // may handle hfConnectionUpgrade when overriden
  if status <> HTTP_SUCCESS then
  begin
    // on fatal error (e.g. OnBeforeBody) direct reject and close the connection
    DoReject(status);
    exit;
  end;
  // now THttpAsyncConnection.OnRead can get the body
  fServer.IncStat(grHeaderReceived);
  if not (fHttp.State in [hrsWaitProcessing, hrsUpgraded]) and
     // HEAD and OPTIONS are requests with Content-Length header but no body
     HttpMethodWithNoBody(fHttp.CommandMethod) then
  begin
    // implement Expect: 100-Continue Header
    if hfExpect100 in fHttp.HeaderFlags then
      // client waits for the server to parse the headers and return 100
      // before sending the request body
      fServer.fAsync.fClients.WriteString(self, 'HTTP/1.1 100 Continue'#13#10#13#10);
    result := DoRequest;
  end
  else
    result := soContinue;
end;

function THttpAsyncConnection.DoRequest: TPollAsyncSocketOnReadWrite;
var
  output: PRawByteStringBuffer;
  remoteID: THttpServerConnectionID;
  flags: THttpServerRequestFlags;
begin
  // check the status
  if nfHeadersParsed in fHttp.HeaderFlags then
    fServer.IncStat(grBodyReceived)
  else
    begin
      // content-length was 0, so hrsGetBody* and DoHeaders() were not called
      result := DoHeaders;
      if (result <> soContinue) or
         (fHttp.State = hrsUpgraded) then
        exit; // rejected or upgraded
    end;
  // optionaly uncompress content
  if fHttp.CompressContentEncoding >= 0 then
    fHttp.UncompressData;
  // prepare the HTTP/REST process reusing the THttpServerRequest instance
  result := soClose;
  remoteid := fHandle;
  fServer.ParseRemoteIPConnID(fHttp.Headers, fRemoteIP, remoteid);
  flags := HTTP_TLS_FLAGS[Assigned(fSecure)] +
           HTTP_UPG_FLAGS[hfConnectionUpgrade in fHttp.HeaderFlags];
  if fRequest = nil then // only create if not rejected by OnBeforeBody
    fRequest := THttpServerRequest.Create(
      fServer, remoteid, nil, flags, @fConnectionOpaque)
  else
    fRequest.Recycle(remoteid, flags);
  fRequest.Prepare(fHttp, fRemoteIP);
  // let the associated THttpAsyncServer execute the request
  if fServer.DoRequest(fRequest) then
    result := soContinue;
  // handle HTTP/1.1 keep alive timeout
  if (fKeepAliveSec > 0) and
     not (hfConnectionClose in fHttp.HeaderFlags) and
     (fServer.Async.fLastOperationSec > fKeepAliveSec) then
  begin
    fOwner.DoLog(sllTrace, 'DoRequest KeepAlive=% timeout: close connnection',
      [fKeepAliveSec], self);
    include(fHttp.HeaderFlags, hfConnectionClose); // before SetupResponse
  end
  // trigger optional hsoBan40xIP temporary IP4 bans on unexpected request
  else if fServer.fAsync.Banned.ShouldBan(fRequest.RespStatus, fRemoteIP4) then
  begin
    fOwner.DoLog(sllTrace, 'DoRequest=%: BanIP(%) %',
      [fRequest.RespStatus, fRemoteIP, fServer.fAsync.Banned], self);
    fServer.IncStat(grBanned);
    include(fHttp.HeaderFlags, hfConnectionClose); // before SetupResponse
  end;
  // compute the response for the HTTP state machine
  output := fRequest.SetupResponse(fHttp, fServer.fCompressGz,
    fServer.fAsync.fClients.fSendBufferSize);
  // now fHttp.State is final as hrsSendBody or hrsResponseDone
  fRespStatus := fRequest.RespStatus;
  // release memory of all COW fields ASAP if HTTP header was interned
  if fHttp.Interning = nil then
    FreeAndNil(fRequest) // more efficient to create a new instance
  else
    fRequest.CleanupInstance; // let all headers have refcount=1
  // now try socket send() with headers (and small body if hrsResponseDone)
  // then TPollAsyncSockets.ProcessWrite/subscribe would process hrsSendBody
  fServer.fAsync.fClients.Write(self, output.Buffer, output.Len, {timeout=}1000);
  // will call THttpAsyncConnection.AfterWrite once sent to finish/continue
  // see THttpServer.Process() for the blocking equivalency of this async code
end;


{ THttpAsyncConnections }

procedure THttpAsyncConnections.Execute;
begin
  fExecuteAcceptOnly := true; // THttpAsyncServer.Execute will do the writes
  inherited Execute;
end;

procedure THttpAsyncConnections.IdleEverySecond;
begin
  // GC of connection memory
  inherited IdleEverySecond;
  // high-level THttpAsyncServer process
  if fAsyncServer <> nil then
    fAsyncServer.IdleEverySecond;
  // reset the hsoBan40xIP items of the oldest list
  if fBanned <> nil then
    fBanned.IdleEverySecond;
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
  ServerThreadPoolCount, KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions);
var
  aco: TAsyncConnectionsOptions;
begin
  fProcessName := ProcessName;
  if fProcessName = '' then
    fProcessName := aPort;
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
    include(aco, acoThreadSmooting) // and exclude any thread affinity
  else
  begin
    if hsoThreadCpuAffinity in ProcessOptions then
      include(aco, acoThreadCpuAffinity);
    if hsoThreadSocketAffinity in ProcessOptions then
      include(aco, acoThreadSocketAffinity);
  end;
  if hsoReusePort in ProcessOptions then
    include(aco, acoReusePort);
  if fConnectionClass = nil then
    fConnectionClass := THttpAsyncConnection;
  if fConnectionsClass = nil then
    fConnectionsClass := THttpAsyncConnections;
  // bind and start the actual thread-pooled connections async server
  fAsync := fConnectionsClass.Create(aPort, OnStart, OnStop,
    fConnectionClass, fProcessName, TSynLog, aco, ServerThreadPoolCount);
  fAsync.fAsyncServer := self;
  if hsoBan40xIP in ProcessOptions then
    fAsync.fBanned := THttpAcceptBan.Create;
  // launch this TThread instance
  inherited Create(aPort, OnStart, OnStop, fProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, ProcessOptions);
end;

destructor THttpAsyncServer.Destroy;
begin
  // no more incoming request
  Shutdown;
  // abort pending async process
  if fAsync <> nil then
    fAsync.Shutdown;
  // terminate the (void) Execute (suspended) thread
  inherited Destroy;
  // finalize all thread-pooled connections
  FreeAndNilSafe(fAsync);
  if fInterning <> nil then
  begin
    Dispose(fInterning);
    fInterning := nil;
  end;
end;

function THttpAsyncServer.GetExecuteState: THttpServerExecuteState;
begin
  result := fAsync.fExecuteState; // state comes from THttpAsyncConnections
  fExecuteMessage := fAsync.fExecuteMessage;
end;

procedure THttpAsyncServer.IdleEverySecond;
var
  tix, cleaned: cardinal;
  T: TSynSystemTime;
  tmp: shortstring;
begin
  // no need to use the global HttpDateNowUtc and its GetTickCount64 API call
  if hsoIncludeDateHeader in fOptions then
  begin
    T.FromNowUtc;
    T.ToHttpDateShort(tmp, 'GMT'#13#10, 'Date: ');
    fHttpDateNowUtc := tmp; // (almost) atomic set
  end;
  // clean interned HTTP headers every 16 secs
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

procedure THttpAsyncServer.Execute;
var
  notif: TPollSocketResult;
  tix64: Int64;
  tix, lasttix: cardinal;
  ms, msidle: integer;
begin
  // Send() output packets in the background
  SetCurrentThreadName('W %', [fAsync.fProcessName]);
  NotifyThreadStart(self);
  WaitStarted(10); // wait for fAsync.Execute to bind and start
  if fAsync <> nil then
    try
      fSock := fAsync.fServer;
      fAsync.DoLog(sllTrace, 'Execute: main W loop', [], self);
      IdleEverySecond; // initialize idle process (e.g. fHttpDateNowUtc)
      tix := mormot.core.os.GetTickCount64 shr 16; // delay=500 after 1 min idle
      lasttix := tix;
      ms := 1000; // fine if OnGetOneIdle is called in-between
      if fAsync.fClientsEpoll then
        if fCallbackSendDelay <> nil then
          ms := fCallbackSendDelay^; // for WebSockets frame gathering
      while not Terminated and
            not fAsync.Terminated do
        if fAsync.fClients.fWrite.Count + fAsync.fClients.fWrite.SubscribeCount = 0 then
        begin
          // no socket/poll/epoll API nedeed (most common case)
          if (fCallbackSendDelay <> nil) and
             (tix = lasttix) then
            msidle := fCallbackSendDelay^ // delayed SendFrames gathering
          else if (fAsync.fGC[1].Count = 0) or
                  (fAsync.fKeepConnectionInstanceMS > 500 * 2) then
            msidle := 500 // idle server
          else
            msidle := fAsync.fKeepConnectionInstanceMS shr 1; // follow GC pace
          SleepHiRes(msidle);
          // periodic trigger of IdleEverySecond and ProcessIdleTixSendFrames
          tix64 := mormot.core.os.GetTickCount64;
          tix := tix64 shr 16;
          fAsync.ProcessIdleTix(self, tix64);
          if (fCallbackSendDelay <> nil) and
             (fAsync.fClients.fRead.Count <> 0) then
            lasttix := tix; // need fCallbackSendDelay^ for upgraded connections
        end
        else
        begin
          // some huge packets queued for async sending (seldom)
          // note: fWrite.GetOne() calls ProcessIdleTix() while looping
          if fAsync.fClients.fWrite.GetOne(ms, 'W', notif) then
            fAsync.fClients.ProcessWrite(notif);
          if fCallbackSendDelay <> nil then
          begin
            tix := mormot.core.os.GetTickCount64 shr 16;
            lasttix := tix;
          end;
        end;
    except
      on E: Exception do
        // callback exceptions should all be catched: so we assume that any
        // exception in mORMot code should be considered as fatal
        fAsync.DoLog(sllWarning, 'Execute raised uncatched % -> terminate %',
          [E.ClassType, fAsync.fProcessName], self);
    end;
  fAsync.DoLog(sllInfo, 'Execute: done W %', [fAsync.fProcessName], self);
end;


end.

