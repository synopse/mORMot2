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

  /// let TPollAsyncSockets.OnRead/AfterWrite shutdown the socket if needed
  TPollAsyncSocketOnReadWrite = (
    soContinue,
    soClose);

  /// abstract parent to store information aboout one TPollAsyncSockets connection
  TPollAsyncConnection = class(TSynPersistent)
  protected
    /// the associated TCP connection
    // - equals nil after TPollAsyncSockets.Stop
    fSocket: TNetSocket;
    /// the associated sequence number
    // - equals 0 after TPollAsyncSockets.Stop
    fHandle: TPollAsyncConnectionHandle;
    /// Lock/Unlock R/W thread acquisition
    // - our first implementation attempted a simple atomic counter, but failed
    // on AARCH64 CPU so now TRTLCriticalSection is used - and is fast enough
    fLockRW: array[boolean] of TRTLCriticalSection;
    /// current number of nested TryLock/WaitLock calls
    fLockCounter: array[boolean] of integer;
    /// atomically incremented during WaitLock()
    fWaitCounter: integer;
    /// false for a single lock (default), true to separate read/write locks
    fLockMax: boolean;
    /// flag set by TAsyncConnections.IdleEverySecond to purge rd/wr unused buffers
    // - avoid to call the GetTickCount64 system API for every activity
    fWasActive: boolean;
    /// flag set when Subscribe() has been called
    fWriteSubscribed: boolean;
    /// flag set by UnlockAndCloseConnection
    fClosing: boolean;
    /// the current (reusable) read data buffer of this connection
    fRd: TRawByteStringBuffer;
    /// the current (reusable) write data buffer of this connection
    fWr: TRawByteStringBuffer;
    /// this method is called when the instance is connected to a poll
    // - overriding this method is cheaper than the plain Create destructor
    // - default implementation initializes the locker[] mutexes
    // - you may inherit and set fLockMax := true before if two locks are needed
    procedure AfterCreate; virtual;
    /// this method is called when the instance is about to be deleted from a poll
    // - overriding this method is cheaper than the plain Destroy destructor
    // - default implementation resets its Handle to 0 for IsDangling detection
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
    /// initialize the connection instnace
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
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// try to acquire an exclusive R/W access to this connection
    // - returns true if connection has been acquired
    // - returns false if it is used by another thread, after the timeoutMS period
    function WaitLock(writer: boolean; timeoutMS: cardinal): boolean;
    /// release exclusive R/W access to this connection
    procedure UnLock(writer: boolean);
      {$ifdef HASINLINEWINAPI} inline; {$endif}
    /// release all R/W nested locks
    // - used when the connection is closed and inactive
    procedure UnLockFinal(writer: boolean);
    // called after TAsyncConnections.LastOperationReleaseMemorySeconds
    function ReleaseMemoryOnIdle: PtrInt;
    /// read-only access to the socket number associated with this connection
    property Socket: TNetSocket
      read fSocket;
  published
    /// read-only access to the handle number associated with this connection
    property Handle: TPollAsyncConnectionHandle
      read fHandle;
  end;

  /// possible options for TPollAsyncSockets process
  // - by default, TPollAsyncSockets.Write will first try to send the data
  // using Send() in non-blocking mode, unless paoWritePollOnly is defined,
  // and fWrite will be used to poll output state and send it asynchronously
  TPollAsyncSocketsOptions = set of (
    paoWritePollOnly);

  /// TPollAsyncSockets.Read allows to asynchronously delete connection instances
  TPollAsyncReadSockets = class(TPollSockets)
  protected
    fGC, fGC2: TObjectDynArray;
    fGCCount, fGCCount2, fGCPass: integer;
    function IsValidPending(tag: TPollSocketTag): boolean; override;
  public
    /// finalize this instance and the associated remaining AddGC connections
    destructor Destroy; override;
    /// thread-safe registration of a to-be-freed TObject instance
    // - typically TAsyncConnection instances released by PollForPendingEvents
    procedure AddGC(tobefree: TObject);
    /// overriden to eventually free the connections as registered by AddGC()
    function PollForPendingEvents(timeoutMS: integer): integer; override;
  end;

  /// callback prototype for TPollAsyncSockets.OnStart/OnStop events
  TOnPollAsyncEvent = procedure(Sender: TPollAsyncConnection) of object;

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
    fRead: TPollAsyncReadSockets;
    fWrite: TPollSockets; // separated fWrite (short-term subscriptions)
    fProcessingRead, fProcessingWrite: integer;
    fSendBufferSize: integer; // retrieved at first connection Start()
    fReadCount: integer;
    fWriteCount: integer;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fOptions: TPollAsyncSocketsOptions;
    fDebugLog: TSynLogClass;
    fProcessReadCheckPending: boolean;
    fOnStart: TOnPollAsyncEvent;
    fOnStop: TOnPollAsyncEvent;
    function GetCount: integer;
    procedure DoLog(const TextFmt: RawUtf8; const TextArgs: array of const);
    // pseError: return false to close socket and connection
    function OnError(connection: TPollAsyncConnection;
      events: TPollSocketEvents): boolean; virtual; abstract;
    procedure OnClosed(connection: TPollAsyncConnection); virtual; abstract;
    procedure UnlockAndCloseConnection(writer: boolean;
      var connection: TPollAsyncConnection; const caller: shortstring);
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
    function ProcessRead(const notif: TPollSocketResult): boolean;
    /// one thread should execute this method with the proper pseWrite notif
    // - thread-safe handle of any outgoing packets
    procedure ProcessWrite(const notif: TPollSocketResult);
    /// notify internal socket polls to stop their polling loop ASAP
    procedure Terminate(waitforMS: integer);
    /// low-level access to the polling class used for incoming data
    property PollRead: TPollAsyncReadSockets
      read fRead;
    /// low-level access to the polling class used for outgoind data
    property PollWrite: TPollSockets
      write fWrite;
    /// some processing options
    property Options: TPollAsyncSocketsOptions
      read fOptions write fOptions;
    /// event called on Start() method success
    property OnStart: TOnPollAsyncEvent
      read fOnStart write fOnStart;
    /// event called on Stop() method success
    property OnStop: TOnPollAsyncEvent
      read fOnStop write fOnStop;
  published
    /// how many connections are currently managed by this instance
    property Count: integer
      read GetCount;
    /// how many times data has been received by this instance
    property ReadCount: integer
      read fReadCount;
    /// how many times data has been sent by this instance
    property WriteCount: integer
      read fWriteCount;
    /// how many data bytes have been received by this instance
    property ReadBytes: Int64
      read fReadBytes;
    /// how many data bytes have been sent by this instance
    property WriteBytes: Int64
      read fWriteBytes;
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
    fLastOperation: TAsyncConnectionSec;
    fOwner: TAsyncConnections;
    fRemoteIP: RawUtf8;
    fRemoteConnID: THttpServerConnectionID;
    // called after TAsyncConnections.LastOperationIdleSeconds of no activity
    // - Sender.Write() could be used to send e.g. a hearbeat frame
    // - should finish quickly and be non-blocking
    // - returns true to log notified events, false if nothing happened
    function OnLastOperationIdle(nowsec: TAsyncConnectionSec): boolean; virtual;
  public
    /// initialize this instance
    constructor Create(aOwner: TAsyncConnections;
      const aRemoteIP: RawUtf8); reintroduce; virtual;
    /// read-only access to the associated connections list
    property Owner: TAsyncConnections
      read fOwner;
  published
    /// the associated remote IP4/IP6, as text
    property RemoteIP: RawUtf8
      read fRemoteIP;
  end;

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
    /// how many clients have been handled by the poll, from the beginning
    property Total: integer
      read GetTotal;
  end;

  /// define what TAsyncConnectionsThread.Execute should actually do
  TAsyncConnectionsThreadProcess = (
    atpReadSingle,
    atpReadPoll,
    atpReadPending);

  TAsyncConnectionsThreadProcesses = set of TAsyncConnectionsThreadProcess;

  /// used to implement a thread poll to process TAsyncConnection instances
  TAsyncConnectionsThread = class(TSynThread)
  protected
    fOwner: TAsyncConnections;
    fProcess: TAsyncConnectionsThreadProcess;
    fWaitForReadPending: boolean;
    fExecuteStarted: boolean;
    fExecuteFinished: boolean;
    fIndex: integer;
    fEvent: TEvent;
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
  end;

  /// low-level options for TAsyncConnections processing
  // - TAsyncConnectionsSockets.OnError will shutdown the connection on any error,
  // unless acoOnErrorContinue is defined
  // - acoNoLogRead and acoNoLogWrite could reduce the log verbosity
  // - acoVerboseLog will log transmitted frames content, for debugging purposes
  // - acoWritePollOnly will be translated into paoWritePollOnly on server
  // - acoDebugReadWriteLog would make low-level send/receive logging
  // - acoNoConnectionTrack would force to by-pass the internal Connections list
  // if it is not needed - not used by now
  TAsyncConnectionsOptions = set of (
    acoOnErrorContinue,
    acoNoLogRead,
    acoNoLogWrite,
    acoVerboseLog,
    acoWritePollOnly,
    acoDebugReadWriteLog,
    acoNoConnectionTrack);

  /// implements an abstract thread-pooled high-performance TCP clients or server
  // - internal TAsyncConnectionsSockets will handle high-performance process
  // of a high number of long-living simultaneous connections
  // - will use a TAsyncConnection inherited class to maintain connection state
  // - don't use this abstract class but either TAsyncServer or TAsyncClient
  // - under Linux/POSIX, check your "ulimit -H -n" value: one socket consumes
  // two file descriptors: you may better add the following line to your
  // /etc/limits.conf or /etc/security/limits.conf system file:
  // $ * hard nofile 65535
  TAsyncConnections = class(TServerGeneric)
  protected
    fConnectionClass: TAsyncConnectionClass;
    fConnection: TAsyncConnectionDynArray; // sorted by TAsyncConnection.Handle
    fClients: TAsyncConnectionsSockets;
    fThreads: array of TAsyncConnectionsThread;
    fThreadReadPoll: TAsyncConnectionsThread;
    fConnectionCount: integer;
    fThreadPoolCount: integer;
    fLastConnectionFind: integer;
    fLog: TSynLogClass;
    fLastHandle: integer;
    fOptions: TAsyncConnectionsOptions;
    fLastOperationSec: TAsyncConnectionSec;
    fLastOperationReleaseMemorySeconds: cardinal;
    fLastOperationIdleSeconds: cardinal;
    fThreadClients: record // used by TAsyncClient
      Count, Timeout: integer;
      Address, Port: RawUtf8;
    end;
    fConnectionLock: TSynLocker;
    fIdleTix: Int64;
    function AllThreadsStarted: boolean; virtual;
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: RawUtf8;
      out aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionAdd(
      aSocket: TNetSocket; aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionDelete(
      aHandle: TPollAsyncConnectionHandle): boolean; overload; virtual;
    function ConnectionDelete(
      aConnection: TAsyncConnection; aIndex: integer): boolean; overload;
    procedure ThreadPollingWakeup(Events: integer);
    procedure DoLog(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; Instance: TObject);
    procedure ProcessIdleTix(Sender: TObject; NowTix: Int64); virtual;
    procedure ProcessClientStart(Sender: TPollAsyncConnection);
    procedure IdleEverySecond(tix: Int64);
  public
    /// initialize the multiple connections
    // - don't use this constructor but inherited client/server classes
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the instance, releasing all associated threads and sockets
    destructor Destroy; override;
    /// high-level access to a connection instance, from its handle
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    // - raise an exception if acoNoConnectionTrack option was defined
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the lock as:
    // ! try ... finally UnLock; end;
    function ConnectionFindLocked(aHandle: TPollAsyncConnectionHandle;
      aIndex: PInteger = nil): TAsyncConnection;
    /// high-level access to a connection instance, from its handle
    // - this method won't keep the main Lock so its handler should delay its
    // destruction by a proper mechanism (flag/refcounting)
    function ConnectionFind(aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
    /// low-level access to a connection instance, from its handle
    // - caller should have called Lock before this method is done
    function ConnectionSearch(aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
    /// just a wrapper around fConnectionLock.Lock
    // - raise an exception if acoNoConnectionTrack option was defined
    procedure Lock;
    /// just a wrapper around fConnectionLock.UnLock
    // - raise an exception if acoNoConnectionTrack option was defined
    procedure Unlock;
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
    /// allow to customize low-level options for processing
    property Options: TAsyncConnectionsOptions
      read fOptions write fOptions;
    /// access to the associated log class
    property Log: TSynLogClass
      read fLog;
    /// low-level unsafe direct access to the connection instances
    // - ensure this property is used in a thread-safe manner, i.e. via
    // ! Lock; try ... finally UnLock; end;
    property Connection: TAsyncConnectionDynArray
      read fConnection;
    /// low-level unsafe direct access to the connection count
    // - ensure this property is used in a thread-safe manner, i.e. via
    // ! Lock; try ... finally UnLock; end;
    property ConnectionCount: integer
      read fConnectionCount;
    /// how many read threads there are in this thread pool
    property ThreadPoolCount: integer
      read fThreadPoolCount;
  published
    /// access to the TCP client sockets poll
    // - TAsyncConnection.OnRead should rather use Write() and LogVerbose()
    // methods of this TAsyncConnections class instead of using Clients
    property Clients: TAsyncConnectionsSockets
      read fClients;
  end;

  /// implements a thread-pooled high-performance TCP server
  // - will use a TAsyncConnection inherited class to maintain connection state
  // for server process
  TAsyncServer = class(TAsyncConnections)
  protected
    fServer: TCrtSocket; // for proper complex binding
    fMaxPending: integer;
    fMaxConnections: integer;
    fExecuteState: THttpServerExecuteState;
    fExecuteAcceptOnly: boolean; // writes in another thread (THttpAsyncServer)
    fSockPort: RawUtf8;
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
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the server, releasing all associated threads and sockets
    destructor Destroy; override;
  published
    /// access to the TCP server socket
    property Server: TCrtSocket
      read fServer;
    /// above how many active connections accept() would reject
    // - MaxPending applies to the actual thread-pool processing activity,
    // whereas MaxConnections tracks the number of connections even in idle state
    property MaxConnections: integer
      read fMaxConnections write fMaxConnections;
    /// above how many fClients.fRead.PendingCount accept() would reject
    // - is mapped by the high-level THttpAsyncServer.HttpQueueLength property
    // - default is 1000, but could be a lower value e.g. for a load-balancer
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
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
    procedure HttpInit;
    // redirect to fHttp.ProcessRead()
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    // redirect to fHttp.ProcessWrite()
    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    // quickly reject incorrect requests (payload/timeout/OnBeforeBody)
    function DecodeHeaders: integer; virtual;
    function DoHeaders: TPollAsyncSocketOnReadWrite;
    function DoRequest: TPollAsyncSocketOnReadWrite;
  end;

  /// event-driven process of HTTP/WebSockets connections
  THttpAsyncConnections = class(TAsyncServer)
  protected
    fAsyncServer: THttpAsyncServer;
    procedure SetExecuteState(State: THttpServerExecuteState); override;
  end;

  /// meta-class of THttpAsyncConnections type
  THttpAsyncConnectionsClass = class of THttpAsyncConnections;

  /// HTTP server using non-blocking sockets
  THttpAsyncServer = class(THttpServerSocketGeneric)
  protected
    fAsync: THttpAsyncConnections;
    fCompressGz: integer;
    fHeadersDefaultBufferSize: integer;
    fConnectionClass: TAsyncConnectionClass;
    fConnectionsClass: THttpAsyncConnectionsClass;
    function GetRegisterCompressGzStatic: boolean;
    procedure SetRegisterCompressGzStatic(Value: boolean);
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetExecuteState: THttpServerExecuteState; override;
    // the main thread will Send output packets in the background
    procedure Execute; override;
  public
    /// create an event-driven HTTP Server
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; aHeadersUnFiltered: boolean = false;
      CreateSuspended: boolean = false; aLogVerbose: boolean = false); override;
    /// finalize the HTTP Server
    destructor Destroy; override;
    /// if we should search for local .gz cached file when serving static files
    property RegisterCompressGzStatic: boolean
      read GetRegisterCompressGzStatic write SetRegisterCompressGzStatic;
    /// initial capacity of internal per-connection Headers buffer
    // - 2 KB by default is within the mormot.core.fpcx64mm SMALL blocks limit
    // so will use up to 3 locks before contention
    property HeadersDefaultBufferSize: integer
      read fHeadersDefaultBufferSize write fHeadersDefaultBufferSize;
    /// direct access to the internal high-performance TCP server
    // - you could set e.g. Async.MaxConnections
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

constructor TPollAsyncConnection.Create;
begin
  inherited Create; // RTTI ininialization
  AfterCreate;
end;

destructor TPollAsyncConnection.Destroy;
var
  b: boolean;
begin
  try
    BeforeDestroy;
    // ensure all locks are properly released
    for b := false to fLockMax do
      if fLockCounter[b] >= 0 then // set to -1 if already deleted
      begin
        while fLockCounter[b] <> 0 do
        begin
          // paranoid check: should have been properly unlocked
          {TSynLog.DoLog(sllWarning, 'TPollAsyncConnection(%) Done locked: r=% w=%',
            [pointer(@self), fLockCounter[false], fLockCounter[true]], nil);}
          mormot.core.os.LeaveCriticalSection(fLockRW[b]);
          dec(fLockCounter[b]);
        end;
        // on some OS, free the mutex in the same thread which created it
        DeleteCriticalSection(fLockRW[b]);
        fLockCounter[b] := -1; // call DeleteCriticalSection() only once
      end;
    // finalize the instance
    fHandle := 0; // to detect any dangling pointer
  except
    // ignore any exception at this stage
  end;
  inherited Destroy;
end;

procedure TPollAsyncConnection.AfterCreate;
var
  b: boolean;
begin
  for b := false to fLockMax do
    InitializeCriticalSection(fLockRW[b]);
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
            fClosing;
end;

function TPollAsyncConnection.TryLock(writer: boolean): boolean;
begin
  if not fLockMax then
    writer := false; // there is a single lock
  if (fSocket <> nil) and
     (mormot.core.os.TryEnterCriticalSection(fLockRW[writer]) <> 0) then
  begin
    fWasActive := true;
    inc(fLockCounter[writer]);
    result := true;
  end
  else
    result := false;
end;

procedure TPollAsyncConnection.UnLock(writer: boolean);
begin
  if not fLockMax then
    writer := false; // there is a single lock
  if (@self <> nil) and
     (fLockCounter[writer] > 0) then
  begin
    dec(fLockCounter[writer]);
    mormot.core.os.LeaveCriticalSection(fLockRW[writer]);
  end;
end;

procedure TPollAsyncConnection.UnLockFinal(writer: boolean);
begin
  if not fLockMax then
    writer := false; // there is a single lock
  while fLockCounter[writer] > 0 do
  begin
    dec(fLockCounter[writer]);
    mormot.core.os.LeaveCriticalSection(fLockRW[writer]);
  end;
end;

procedure TPollAsyncConnection.OnClose;
begin
  // nothing to do by default
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
  fWasActive := false; // TryLock() was with no true activity here
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
  InterlockedIncrement(fWaitCounter);
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
  InterlockedDecrement(fWaitCounter);
end;


{ TPollAsyncReadSockets }

destructor TPollAsyncReadSockets.Destroy;
begin
  inherited Destroy;
  // release any connection which are not been GCed yet
  ObjArrayClear(fGC,  {continueonexception=}true, @fGCCount);
  ObjArrayClear(fGC2, {continueonexception=}true, @fGCCount2);
end;

function TPollAsyncReadSockets.IsValidPending(tag: TPollSocketTag): boolean;
begin
  // same logic than TPollAsyncConnection IsDangling() + TryLock()
  result := (tag <> 0) and
            // avoid dangling pointer
            (TPollAsyncConnection(tag).fHandle <> 0) and
            // another atpReadPending thread may currently own this connection
            // (occurs if PollForPendingEvents was called in between)
            (TPollAsyncConnection(tag).fLockCounter[{write=}false] = 0);
end;

function TPollAsyncReadSockets.PollForPendingEvents(timeoutMS: integer): integer;
begin
  result := inherited PollForPendingEvents(timeoutMS);
  if fPending.Count <> 0 then
    // don't free any connection while we have pending events
    exit;
  if fGCCount2 > 0 then
  begin
    if fGCCount2 < 1000 then
    begin
      // delayed safe/paranoid Free at the 8th idle pass of atpReadPoll polling
      inc(fGCPass);
      if fGCPass and 7 <> 0 then
        exit;
    end;
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'PollForPendingEvents: GC=%', [fGCCount2], self);
    ObjArrayClear(fGC2, {continueonexception=}true, @fGCCount2);
    // here fGC2=nil and fGCCount2=0
  end;
  if fGCCount > 0 then
  begin
    // atomic copy of the shared AddGC() instances list
    mormot.core.os.EnterCriticalSection(fPendingLock); // keep lock short
    fGC2 := fGC; // immediate ref-counting assignment
    fGCCount2 := fGCCount;
    fGC := nil;
    fGCCount := 0;
    mormot.core.os.LeaveCriticalSection(fPendingLock);
  end;
end;

procedure TPollAsyncReadSockets.AddGC(tobefree: TObject);
begin
  if Terminated then
    exit;
  mormot.core.os.EnterCriticalSection(fPendingLock);
  ObjArrayAddCount(fGC, tobefree, fGCCount);
  mormot.core.os.LeaveCriticalSection(fPendingLock);
end;


{ TPollAsyncSockets }

constructor TPollAsyncSockets.Create(aOptions: TPollAsyncSocketsOptions);
begin
  fOptions := aOptions;
  inherited Create;
  fRead := TPollAsyncReadSockets.Create;
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
var
  res: TNetResult;
begin
  result := false;
  if fRead.Terminated or
     connection.IsDangling then
    exit;
  LockedInc32(@fProcessingRead);
  try
    // we expect non-blocking mode on a real working socket
    res := connection.fSocket.MakeAsync;
    if res <> nrOK then
    begin
      if fDebugLog <> nil then
        DoLog('Start: MakeAsync(%) failed as % %',
          [pointer(connection.fSocket), ToText(res)^, connection]);
    end
    else
    begin
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
      // subscribe for any incoming packets
      result := fRead.Subscribe(
        connection.fSocket, [pseRead], TPollSocketTag(connection));
      // now, ProcessRead will handle pseRead+pseError/pseClosed on this socket
      if fDebugLog <> nil then
        DoLog('Start sock=% handle=%',
          [pointer(connection.fSocket), connection.Handle]);
    end;
  finally
    LockedDec32(@fProcessingRead);
  end;
  if result and
     Assigned(fOnStart) then
    fOnStart(connection);
end;

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
      DoLog('Stop sock=% handle=% r=% w=%',
        [pointer(sock), connection.Handle,
         connection.fLockCounter[false], connection.fLockCounter[true]]);
    if sock <> nil then
    begin
      // notify ProcessRead/ProcessWrite to abort
      connection.fSocket := nil;
      // register to unsubscribe for the next PollForPendingEvents() call
      fRead.Unsubscribe(sock, TPollSocketTag(connection));
      if connection.fWriteSubscribed then
        fWrite.Unsubscribe(sock, TPollSocketTag(connection));
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
  if waitforMS <= 0 then
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
        res := connection.fSocket.Send(P, sent);
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
      begin
        // register for ProcessWrite() if not already
        result := fWrite.Subscribe(
          connection.fSocket, [pseWrite], TPollSocketTag(connection));
        if result then
          connection.fWriteSubscribed := true;
        if fDebugLog <> nil then
          DoLog('Write Subscribe(sock=%,handle=%)=% %',
            [pointer(connection.Socket), connection.Handle, result, fWrite]);
      end;
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
  var connection: TPollAsyncConnection; const caller: shortstring);
begin
  if connection = nil then
    exit;
  if fDebugLog <> nil then
    DoLog('UnlockSlotAndCloseConnection: % on handle=%',
      [caller, connection.Handle]);
  if connection.fClosing then
    exit;
  // first unlock (if needed)
  connection.UnLockFinal(writer);
  // optional process - e.g. TWebSocketAsyncConnection = focConnectionClose
  connection.OnClose; // called before slot/socket closing
  // Stop() will try to acquire this lock -> notify no need to wait
  connection.fClosing := true;
  CloseConnection(connection);
end;

procedure TPollAsyncSockets.CloseConnection(var connection: TPollAsyncConnection);
begin
  if connection.IsDangling then
    exit;
  try
    if not connection.fClosing then
    begin
      // if not already done in UnlockAndCloseConnection
      connection.OnClose; // called before slot/socket closing
      connection.fClosing := true;
    end;
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
  recved, added: integer;
  res: TNetResult;
  timer: TPrecisionTimer;
  temp: array[0..$7fff] of byte; // up to 32KB moved to small reusable fRd.Buffer
begin
  result := false;
  connection := TPollAsyncConnection(notif.tag);
  if (self = nil) or
     fRead.Terminated or
     connection.IsClosed then
    exit;
  LockedInc32(@fProcessingRead);
  try
    if pseError in notif.events then
      if not OnError(connection, notif.events) then
      begin
        CloseConnection(connection);
        exit;
      end;
    if pseClosed in notif.events then
    begin
      // never notified on Windows: select() doesn't return any "close" flag
      // and checking for pending bytes for closed connection is not correct
      // on multi-thread -> Recv() below will properly detect disconnection
      CloseConnection(connection);
      exit;
    end
    else if pseRead in notif.events then
    begin
      // we were notified that there may be some pending input data
      added := 0;
      if connection.TryLock({writer=}false) then
      // GetOnePending may be from several threads -> ensure locked
      begin
        repeat
          if fRead.Terminated or
             (connection.fSocket = nil) then
            exit;
          recved := SizeOf(temp);
          if fDebugLog <> nil then
            timer.Start;
          res := connection.fSocket.Recv(@temp, recved); // no need of RecvPending()
          if fDebugLog <> nil then
            DoLog('ProcessRead recv(%)=% len=% in % %',
              [pointer(connection.Socket), ToText(res)^, recved, timer.Stop, fRead]);
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
          inc(fReadCount);
          inc(fReadBytes, added);
          try
            // process connection.fRd incoming data (outside of the lock)
            if connection.OnRead = soClose then
              UnlockAndCloseConnection(false, connection, 'ProcessRead OnRead');
            result := true;
          except
            UnlockAndCloseConnection(false, connection, 'ProcessRead OnRead/E');
          end;
        end;
        connection.UnLock(false); // UnlockSlotAndCloseConnection set slot=nil
      end
      else if fDebugLog <> nil then
        DoLog('ProcessRead: TryLock failed % %', [connection, fRead]);
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
  timer: TPrecisionTimer;
begin
  connection := TPollAsyncConnection(notif.tag);
  if (self = nil) or
     fWrite.Terminated or
     (notif.events <> [pseWrite]) or
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
        if fWrite.Terminated or
           (connection.fSocket = nil) then
          exit;
        bufsent := buflen;
        if fDebugLog <> nil then
          timer.Start;
        //DoLog('ProcessWrite: before Send buflen=%', [bufsent]);
        res := connection.fSocket.Send(buf, bufsent);
        if fDebugLog <> nil then
          DoLog('ProcessWrite send(%)=% %/%B in % % fProcessingWrite=%',
            [pointer(connection.fSocket), ToText(res)^,
             bufsent, buflen, timer.Stop, fWrite, fProcessingWrite]);
        if connection.fSocket = nil then
          exit; // Stop() called
        if res = nrRetry then
          break // may block, try later
        else if res <> nrOk then
        begin
          fWrite.Unsubscribe(connection.fSocket, notif.tag);
          connection.fWriteSubscribed := false;
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
          fWrite.Unsubscribe(connection.fSocket, notif.tag);
          connection.fWriteSubscribed := false;
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
  const aRemoteIP: RawUtf8);
begin
  fOwner := aOwner;
  inherited Create;
  if aRemoteIP <> IP4local then
    fRemoteIP := aRemoteIP;
  fWasActive := true; // by definition
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
  if acoVerboseLog in fOwner.fOptions then
    fOwner.DoLog(sllTrace, 'OnClose %', [connection], self);
  // unregister from fOwner list and do connection.Free
  fOwner.EndConnection(connection as TAsyncConnection);
end;

function TAsyncConnectionsSockets.OnError(connection: TPollAsyncConnection;
  events: TPollSocketEvents): boolean;
var
  err: shortstring;
begin
  GetSetNameShort(TypeInfo(TPollSocketEvents), events, err);
  fOwner.DoLog(sllDebug, 'OnError% events=[%] -> free socket and instance',
    [connection, err], self);
  result := acoOnErrorContinue in fOwner.Options; // false=close by default
end;

function TAsyncConnectionsSockets.Write(connection: TPollAsyncConnection;
  data: pointer; datalen, timeout: integer): boolean;
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


{ TAsyncConnectionsThread }

constructor TAsyncConnectionsThread.Create(aOwner: TAsyncConnections;
  aProcess: TAsyncConnectionsThreadProcess; aIndex: integer);
begin
  fOwner := aOwner;
  fProcess := aProcess;
  fIndex := aIndex;
  fEvent := TEvent.Create(nil, false, false, '');
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
  n: RawUtf8;
  pending: integer;
  start: Int64;
  notif: TPollSocketResult;
begin
  FormatUtf8('R% %', [fIndex, fOwner.fProcessName], n);
  SetCurrentThreadName(n);
  fOwner.NotifyThreadStart(self);
  try
    fExecuteStarted := true;
    // implement parallel client connections for TAsyncClient
    while not Terminated and
          (fOwner.fThreadClients.Count > 0) and
          (InterlockedDecrement(fOwner.fThreadClients.Count) >= 0) do
      fOwner.ThreadClientsConnect;
    // main TAsyncConnections read/write process
    while not Terminated and
          (fOwner.fClients <> nil) and
          (fOwner.fClients.fRead <> nil) do
    begin
      case fProcess of
        atpReadSingle:
          // a single thread to rule them all: polling, reading and processing
          if fOwner.fClients.fRead.GetOne(1000, n, notif) then
            if not Terminated then
              fOwner.fClients.ProcessRead(notif);
        atpReadPoll:
          // main thread will just fill pending events from socket polls
          // (no process because a faulty service would delay all reading)
          begin
            if fOwner.fClients.fRead.Count = 0 then
              fEvent.WaitFor(INFINITE); // blocking until next accept()
            start := 0; // back to SleepHiRes(0)
            while not Terminated do
            begin
              pending := fOwner.fClients.fRead.PollForPendingEvents(0);
              if Terminated then
                break
              else if pending = 0 then
                // 0/1/10/50/150 ms steps, checking fThreadReadPoll from accept
                SleepStep(start, @Terminated, fEvent)
              else
              begin
                // process fOwner.fClients.fPending in atpReadPending threads
                fOwner.ThreadPollingWakeup(pending);
                // atpReadPending notifies fThreadReadPoll.fEvent when done
                if Terminated or
                   (fEvent.WaitFor(10) = wrSignaled) then
                  break;
              end;
            end;
          end;
        atpReadPending:
          begin
            // secondary threads wait, then read and process pending events
            fWaitForReadPending := true;
            if fEvent.WaitFor(INFINITE) = wrSignaled then
            begin
              if Terminated then
                break;
              fWaitForReadPending := false;
              while fOwner.fClients.fRead.GetOnePending(notif, n) and
                    not Terminated do
                fOwner.fClients.ProcessRead(notif);
              // release atpReadPoll lock above
              fOwner.fThreadReadPoll.fEvent.SetEvent;
            end;
          end;
      else
        raise EAsyncConnections.CreateUtf8('%.Execute: unexpected fProcess=%',
          [self, ord(fProcess)]);
      end;
    end;
    fOwner.DoLog(sllInfo, 'Execute: done %', [n], self);
  except
    on E: Exception do
      if fOwner <> nil then
        fOwner.DoLog(sllWarning, 'Execute raised a % -> terminate % thread %',
          [E.ClassType, fOwner.fConnectionClass, n], self);
  end;
  fExecuteFinished := true;
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
  fLastOperationSec := Qword(GetTickCount64) div 1000; // need something ASAP
  fLog := aLog;
  fConnectionClass := aConnectionClass;
  if not (acoNoConnectionTrack in aOptions) then
    fConnectionLock.Init;
  opt := [];
  if acoWritePollOnly in aOptions then
    include(opt, paoWritePollOnly);
  fClients := TAsyncConnectionsSockets.Create(opt);
  fClients.fOwner := self;
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
  // caller will start the main thread
  log.Log(sllTrace, 'Create: started % threads', [fThreadPoolCount + 1], self);
end;

function TAsyncConnections.AllThreadsStarted: boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to high(fThreads) do
    if not fThreads[i].fExecuteStarted then
      exit;
  result := true;
end;

destructor TAsyncConnections.Destroy;
var
  i, n: PtrInt;
  endtix: Int64;
begin
  Terminate;
  if fClients <> nil then
  begin
    with fClients do
      DoLog('Destroy threads=% total=% reads=%/% writes=%/% count=%',
        [length(fThreads), Total, ReadCount, KB(ReadBytes),
         WriteCount, KB(WriteBytes), fConnectionCount]);
    fClients.Terminate(5000);
  end;
  // unlock and stop ProcessRead/ProcessWrite polling
  for i := 0 to high(fThreads) do
  begin
    fThreads[i].Terminate;
    if (fThreads[i].fEvent <> nil) and
       not fThreads[i].fExecuteFinished then
      fThreads[i].fEvent.SetEvent;
  end;
  // stop polling and refuse further Write/ConnectionRemove
  endtix := mormot.core.os.GetTickCount64 + 1000;
  repeat
    n := 0;
    for i := 0 to high(fThreads) do
      if not fThreads[i].fExecuteFinished then
        inc(n);
    if n = 0 then
      break;
    DoLog(sllTrace, 'Destroy unfinished=%', [n], self);
    SleepHiRes(1);
  until mormot.core.os.GetTickCount64 > endtix;
  FreeAndNilSafe(fClients); // FreeAndNil() sets nil before which is incorrect
  ObjArrayClear(fThreads);
  inherited Destroy;
  if not (acoNoConnectionTrack in fOptions) then
  begin
    for i := 0 to fConnectionCount - 1 do
      fConnection[i].Free;
    fConnectionLock.Done;
  end;
end;

function TAsyncConnections.ThreadClientsConnect: TAsyncConnection;
var
  res: TNetResult;
  client: TNetSocket;
begin
  result := nil;
  if Terminated then
    exit;
  with fThreadClients do
    res := NewSocket(Address, Port, nlTcp, {bind=}false, timeout, timeout,
      timeout, {retry=}0, client);
  if res <> nrOK then
    raise EAsyncConnections.CreateUtf8('%: %:% connection failure (%)',
      [self, fThreadClients.Address, fThreadClients.Port, ToText(res)^]);
  // create and register the async connection as in TAsyncServer.Execute
  if not ConnectionCreate(client, {ip=}'', result) then
    client.ShutdownAndClose({rdwr=}false)
  else if not fClients.Start(result) then
    FreeAndNil(result);
end;

procedure TAsyncConnections.ThreadPollingWakeup(Events: integer);
var
  i: PtrInt;
begin
  for i := 0 to length(fThreads) - 1 do
    if fThreads[i].fWaitForReadPending then
    begin
      fThreads[i].fEvent.SetEvent;
      dec(Events);
      if Events = 0 then
        exit;
    end;
end;

procedure TAsyncConnections.DoLog(Level: TSynLogInfo; const TextFmt: RawUtf8;
  const TextArgs: array of const; Instance: TObject);
begin
  if (self <> nil) and
     Assigned(fLog) then
    fLog.Add.Log(Level, TextFmt, TextArgs, Instance);
end;

function TAsyncConnections.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: RawUtf8; out aConnection: TAsyncConnection): boolean;
begin
  // you can override this class then call ConnectionAdd
  if Terminated then
    result := false
  else
  begin
    aConnection := fConnectionClass.Create(self, aRemoteIp);
    result := ConnectionAdd(aSocket, aConnection);
  end;
end;

function TAsyncConnections.ConnectionAdd(aSocket: TNetSocket;
  aConnection: TAsyncConnection): boolean;
begin
  result := false; // caller should release aSocket
  if Terminated then
    exit;
  if fLastHandle < 0 then // paranoid check
    raise EAsyncConnections.CreateUtf8(
      '%.ConnectionAdd: %.Handle overflow', [self, aConnection]);
  aConnection.fSocket := aSocket;
  if acoNoConnectionTrack in fOptions then
  begin
    aConnection.fHandle := InterlockedIncrement(fLastHandle);
    InterlockedIncrement(fConnectionCount);
  end
  else
  begin
    fConnectionLock.Lock;
    try
      inc(fLastHandle);
      aConnection.fHandle := fLastHandle;
      ObjArrayAddCount(fConnection, aConnection, fConnectionCount);
    finally
      fConnectionLock.UnLock;
    end;
  end;
  aConnection.AfterCreate;
  if acoVerboseLog in fOptions then
    DoLog(sllTrace, 'ConnectionAdd% socket=% count=%',
      [aConnection, pointer(aSocket), fConnectionCount], self);
  result := true; // indicates aSocket owned by the pool
end;

function TAsyncConnections.ConnectionDelete(aConnection: TAsyncConnection;
  aIndex: integer): boolean;
begin
  // caller should have done fConnectionLock.Lock
  try
    PtrArrayDelete(fConnection, aIndex, @fConnectionCount);
    if acoVerboseLog in fOptions then
      DoLog(sllTrace, 'ConnectionDelete % count=%',
        [aConnection, fConnectionCount], self);
    aConnection.fSocket := nil;   // ensure is known as disabled
    fClients.fRead.AddGC(aConnection); // will be released after processed
    result := true;
  except
    result := false;
  end;
end;

function TAsyncConnections.ConnectionDelete(
  aHandle: TPollAsyncConnectionHandle): boolean;
var
  i: integer;
  conn: TAsyncConnection;
begin
  // don't call fClients.Stop() here - see ConnectionRemove()
  result := false;
  if Terminated or
     (aHandle <= 0) then
    exit;
  conn := ConnectionFindLocked(aHandle, @i);
  if conn <> nil then
    try
      result := ConnectionDelete(conn, i);
    finally
      fConnectionLock.UnLock;
    end;
  if not result then
    DoLog(sllTrace, 'ConnectionDelete(%)=false count=%',
      [aHandle, fConnectionCount], self);
end;

function FastFindConnection(Conn: PPointerArray; R: PtrInt; H: integer): PtrInt;
var
  L, RR: PtrInt;
  C: integer;
begin
  // fast O(log(n)) binary search
  L := 0;
  if 0 <= R then
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
      RR := result + 1; // compile as 2 branchless cmovc/cmovnc on FPC
      dec(result);
      if C < H then
        L := RR
      else
        R := result;
    until L > R;
  result := -1
end;

function TAsyncConnections.ConnectionFindLocked(
  aHandle: TPollAsyncConnectionHandle; aIndex: PInteger): TAsyncConnection;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     Terminated or
     (aHandle <= 0) then
    exit;
  if acoNoConnectionTrack in fOptions then
    raise EAsyncConnections.CreateUtf8('Unexpected %.ConnectionFindLocked', [self]);
  fConnectionLock.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$endif HASFASTTRYFINALLY}
    i := fConnectionCount - 1;
    if i >= 0 then
    begin
      if fConnection[i].Handle <> aHandle then // very short-living connection
        i := FastFindConnection(pointer(fConnection), i, aHandle);
      if i >= 0 then
      begin
        result := fConnection[i];
        if aIndex <> nil then
          aIndex^ := i;
      end;
      {if acoVerboseLog in fOptions then
        DoLog(sllTrace, 'ConnectionFindLocked(%)=%', [aHandle, result], self);}
    end;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    if result = nil then
      fConnectionLock.UnLock;
  {$ifdef HASFASTTRYFINALLY}
  end;
  {$endif HASFASTTRYFINALLY}
end;

function TAsyncConnections.ConnectionSearch(
  aHandle: TPollAsyncConnectionHandle): TAsyncConnection;
var
  i, n: PtrInt;
begin
  // caller should have made fConnectionLock.Lock
  result := nil;
  if aHandle <= 0 then
    exit;
  i := fLastConnectionFind;
  n := fConnectionCount;
  if (i >= n) or
     (fConnection[i].Handle <> aHandle) then
    i := FastFindConnection(pointer(fConnection), n - 1, aHandle);
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
  fConnectionLock.Lock;
  {$ifdef HASFASTTRYFINALLY}
  try
  {$endif HASFASTTRYFINALLY}
    result := ConnectionSearch(aHandle);
  {$ifdef HASFASTTRYFINALLY}
  finally
    fConnectionLock.UnLock;
  end;
  {$else}
  fConnectionLock.UnLock;
  {$endif HASFASTTRYFINALLY}
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
  conn := ConnectionFindLocked(aHandle, @i);
  if conn <> nil then
    try
      if not fClients.Stop(conn) then
        DoLog(sllDebug, 'ConnectionRemove: Stop=false for %', [conn], self);
      result := ConnectionDelete(conn, i);
    finally
      fConnectionLock.UnLock;
    end;
  if not result then
    DoLog(sllTrace, 'ConnectionRemove(%)=false', [aHandle], self);
end;

procedure TAsyncConnections.EndConnection(connection: TAsyncConnection);
begin
  if acoNoConnectionTrack in fOptions then
  begin
    connection.fSocket := nil;
    fClients.fRead.AddGC(connection); // will be released after processed
    InterlockedDecrement(fConnectionCount);
  end
  else
    ConnectionDelete(connection.Handle);
end;

procedure TAsyncConnections.Lock;
begin
  if acoNoConnectionTrack in fOptions then
    raise EAsyncConnections.CreateUtf8('Unexpected %.Lock', [self]);
  fConnectionLock.Lock;
end;

procedure TAsyncConnections.Unlock;
begin
  if acoNoConnectionTrack in fOptions then
    raise EAsyncConnections.CreateUtf8('Unexpected %.UnLock', [self]);
  fConnectionLock.UnLock;
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

procedure TAsyncConnections.IdleEverySecond(tix: Int64);
var
  i: PtrInt;
  notified, gced: PtrInt;
  allowed, gc: cardinal;
  c: TAsyncConnection;
begin
  if Terminated or
     (fConnectionCount = 0) then
    exit;
  if acoNoConnectionTrack in fOptions then
    exit;
  notified := 0;
  gced := 0;
  fLastOperationSec := Qword(tix) div 1000; // 32-bit second resolution is fine
  allowed := fLastOperationIdleSeconds;
  if allowed <> 0 then
    allowed := fLastOperationSec - allowed;
  gc := fLastOperationReleaseMemorySeconds;
  if gc <> 0 then
    gc := fLastOperationSec - gc;
  fConnectionLock.Lock;
  try
    for i := 0 to fConnectionCount - 1 do
    begin
      c := fConnection[i];
      if c.fWasActive then
      begin
        c.fWasActive := false; // update once per second is good enough
        c.fLastOperation := fLastOperationSec;
      end
      else
      begin
        // check if some events should be triggerred on this inactive connection
        if (gc <> 0) and
           (c.fLastOperation < gc) then
          inc(gced, c.ReleaseMemoryOnIdle);
        if (allowed <> 0) and
           (c.fLastOperation < allowed) then
          try
            if c.OnLastOperationIdle(fLastOperationSec) then
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
    fConnectionLock.UnLock;
  end;
end;

procedure TAsyncConnections.ProcessIdleTix(Sender: TObject; NowTix: Int64);
begin
  // called from fClients.fWrite.OnGetOneIdle callback
  if not Terminated then
    if NowTix >= fIdleTix then
    begin
      IdleEverySecond(NowTix);
      fIdleTix := NowTix + 1000;
    end;
  // note: this method should be non-blocking and return quickly
 end;

procedure TAsyncConnections.ProcessClientStart(Sender: TPollAsyncConnection);
begin
  if fThreadReadPoll <> nil then
    // release atpReadPoll lock to handle new subscription ASAP
    fThreadReadPoll.fEvent.SetEvent;
end;


{ TAsyncServer }

constructor TAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; aConnectionClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fSockPort := aPort;
  fServer := TCrtSocket.Bind(aPort);
  fMaxConnections := 7777777; // huge number for sure
  fMaxPending := 1000; // fair enough for pending requests
  inherited Create(OnStart, OnStop, aConnectionClass, ProcessName, aLog,
    aOptions, aThreadPoolCount);
end;

destructor TAsyncServer.Destroy;
var
  endtix: Int64;
  touchandgo: TNetSocket; // paranoid ensure Accept() is released
  host, port: RawUtf8;
begin
  Terminate;
  endtix := mormot.core.os.GetTickCount64 + 10000;
  if fServer <> nil then
  begin
    host := fServer.Server; // will also work for nlUnix
    if fServer.SocketLayer <> nlUnix then
    begin
      if host = '0.0.0.0' then
        host := '127.0.0.1';
      port := fSockPort;
    end;
    if NewSocket(host, port{%H-}, fServer.SocketLayer, false,
         10, 0, 0, 0, touchandgo) = nrOk then
      touchandgo.ShutdownAndClose(false);
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
  end;
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
  DoLog(sllTrace, 'Destroy finished', [], self);
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
  sin: TNetAddr;
  ip: RawUtf8;
  start: Int64;
  async: boolean;
begin
  // Accept() incoming connections [and Send() output packets in the background]
  SetCurrentThreadName('AW %', [fProcessName]);
  NotifyThreadStart(self);
  // constructor did bind fServer to the expected TCP port
  if fServer.Sock <> nil then
  try
    SetExecuteState(esRunning);
    if not fExecuteAcceptOnly then
      // setup the main bound connection to be polling together with the writes
      if fClients.fWrite.Subscribe(fServer.Sock, [pseRead], {notif.tag=} 0) then
        fClients.fWrite.PollForPendingEvents(0) // actually subscribe
      else
        raise EAsyncConnections.CreateUtf8('%.Execute: subscribe accept?', [self]);
    // main socket accept/send processing loop
    async := false; // first Accept() will be blocking
    start := 0;
    while not Terminated do
    begin
      if not async then
        notif.tag := 0 // blocking accept()
      else if not fClients.fWrite.GetOne(1000, 'AW', notif) then
        continue;
      if Terminated then
        break;
      if notif.tag = 0 then // no tag = main accept()
      begin
        repeat
          // could we Accept one or several incoming connection(s)?
          res := fServer.Sock.Accept(client, sin);
          if Terminated or
             (res = nrRetry) then
            break;
          if (fClients.fRead.Count > fMaxConnections) or
             (fClients.fRead.PendingCount > fMaxPending) then
          begin
            // map THttpAsyncServer.HttpQueueLength property value
            client.ShutdownAndClose({rdwr=}false); // e.g. for load balancing
            res := nrTooManyConnections;
          end;
          if res <> nrOK then
          begin
            // failure (too many clients?) -> wait and retry
            DoLog(sllWarning, 'Execute: Accept(%) failed as %',
              [fServer.Port, ToText(res)^], self);
            if res <> nrTooManyConnections then
              // progressive wait (if not load-balancing, but socket error)
              SleepStep(start, @Terminated);
            break;
          end;
          if Terminated then
            break;
          // if we reached here, we have accepted a connection -> process
          start := 0;
          ip := sin.IP;
          if not ConnectionCreate(client, ip, connection) then
            client.ShutdownAndClose({rdwr=}false)
          else if fClients.Start(connection) then
          begin
            if acoVerboseLog in fOptions then
              DoLog(sllTrace, 'Execute: Accept(%)=%',
                [fServer.Port, connection], self);
            if (not async) and
               not fExecuteAcceptOnly then
            begin
              fServer.Sock.MakeAsync; // share thread with Writes
              async := true;
            end;
          end
          else if connection <> nil then // connection=nil for custom list
            ConnectionDelete(connection.Handle);
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
      // callback exceptions should all be catched: so we assume that any
      // exception in mORMot code should be considered as fatal
      DoLog(sllWarning, 'Execute raised uncatched % -> terminate %',
        [E.ClassType, fProcessName], self);
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
    fHttp.Compress := fServer.fCompress;
    fHttp.CompressAcceptEncoding := fServer.fCompressAcceptEncoding;
    if fServer.ServerKeepAliveTimeOut >= 1000 then
      fKeepAliveSec := fServer.Async.fLastOperationSec +
                       fServer.ServerKeepAliveTimeOut div 1000;
  end;
  inherited AfterCreate;
end;

procedure THttpAsyncConnection.BeforeDestroy;
begin
  fHttp.ProcessDone;
  inherited BeforeDestroy;
end;

procedure THttpAsyncConnection.HttpInit;
begin
  fHttp.ProcessInit({instream=}nil); // ready to process this HTTP request
  if fServer.HeadersUnFiltered then
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
    // use the HTTP machine state to parse fRd input
    result := soContinue;
    st.P := fRd.Buffer;
    st.Len := fRd.Len;
    if fHttp.Process.Len <> 0 then
    begin
      fHttp.Process.Append(st.P, st.Len);
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
    if (fHttp.State = hrsGetHeaders) and
       (fHeadersSec = 0) and
       (fServer.HeaderRetrieveAbortDelay >= 1000) then
      // start measuring time for receiving the headers
      fHeadersSec := fServer.Async.fLastOperationSec +
                     fServer.HeaderRetrieveAbortDelay div 1000;
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
    // use the HTTP machine state to fill fWr with outgoing body chunk
    fHttp.ProcessBody(fWr, fOwner.fClients.fSendBufferSize);
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
  // whole headers + body outgoing content was sent
  fOwner.DoLog(sllTrace, 'AfterWrite Done ContentLength=% Wr=% Flags=%',
    [fHttp.ContentLength, fWr.Len, ToText(fHttp.HeaderFlags)], self);
  if Assigned(fServer.fOnAfterResponse) then
    try
      fServer.fOnAfterResponse(
        fHttp.CommandMethod, fHttp.CommandUri, fRemoteIP, fRespStatus);
    except
      include(fHttp.HeaderFlags, hfConnectionClose);
    end;
  if hfConnectionClose in fHttp.HeaderFlags then
    // connection: close -> shutdown and clear the connection
    result := soClose
  else
  begin
    // kept alive connection -> reset the HTTP parser and continue
    fHttp.ProcessDone;   // ContentStream.Free
    fHttp.Process.Clear; // CompressContentAndFinalizeHead may have set it
    HttpInit;
    result := soContinue;
  end;
end;

function THttpAsyncConnection.DecodeHeaders: integer;
begin
  result := HTTP_SUCCESS;
  if (fServer.MaximumAllowedContentLength > 0) and
     (fHttp.ContentLength > fServer.MaximumAllowedContentLength) then
  begin
    // 413 HTTP error if requested payload is too big (default is 0 = no limit)
    result := HTTP_PAYLOADTOOLARGE;
    fServer.IncStat(grOversizedPayload);
  end else if (fHeadersSec > 0) and
              (fServer.Async.fLastOperationSec > fHeadersSec) then
  begin
    // 408 HTTP error after Server.HeaderRetrieveAbortDelay ms
    result := HTTP_TIMEOUT;
    fServer.IncStat(grTimeout);
  end
  else if Assigned(fServer.OnBeforeBody) then
    // custom validation (e.g. banned IP or missing/invalid BearerToken)
    result := fServer.OnBeforeBody(
      fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers, fHttp.ContentType,
      fRemoteIP, fHttp.BearerToken, fHttp.ContentLength, {notls=}[]);
end;

function THttpAsyncConnection.DoHeaders: TPollAsyncSocketOnReadWrite;
var
  status: integer;
begin
  // finalize the headers
  result := soClose;
  if (nfHeadersParsed in fHttp.HeaderFlags) or
     not fHttp.ParseCommandAndHeader then
    exit;
  fServer.ParseRemoteIPConnID(fHttp.Headers, fRemoteIP, fRemoteConnID);
  // immediate reject of clearly invalid requests
  status := DecodeHeaders; // may handle hfConnectionUpgrade when overriden
  if status <> HTTP_SUCCESS then
  begin
    // on fatal error direct reject and close the connection
    StatusCodeToReason(status, fHttp.Command);
    FormatUtf8('HTTP/1.0 % %'#13#10#13#10'Server Rejected Request as % %',
      [status, fHttp.Command, status, fHttp.Command],
      // (use fHttp.CommandUri as temp var to avoid local RawUtf8 allocation)
      fHttp.CommandUri);
    fServer.fAsync.fClients.WriteString(self, fHttp.CommandUri); // no polling
    fServer.IncStat(grRejected);
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
  req: THttpServerRequest;
  cod: integer;
  output: PRawByteStringBuffer;
  err: string;
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
  fHttp.UncompressData;
  // compute the HTTP/REST process
  result := soClose;
  req := THttpServerRequest.Create(fServer, fRemoteConnID, {thread=}nil, []);
  try
    req.Prepare(fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers,
      fHttp.Content, fHttp.ContentType, fRemoteIP, fHttp.BearerToken, fHttp.UserAgent);
    try
      req.RespStatus := fServer.DoBeforeRequest(req);
      if req.RespStatus > 0 then
        FormatString('Rejected % Request', [fHttp.CommandUri], err)
      else
      begin
        // execute the main processing callback
        req.RespStatus := fServer.Request(req);
        cod := fServer.DoAfterRequest(req);
        if cod > 0 then
          req.RespStatus := cod;
      end;
      result := soContinue;
    except
      on E: Exception do
        begin
          // intercept and return Internal Server Error 500
          req.RespStatus := HTTP_SERVERERROR;
          FormatString('%: %', [E, E.Message], err);
          fServer.IncStat(grException);
          // will keep soClose as result to shutdown the connection
        end;
    end;
    // prepare the response for the HTTP state machine
    if (fKeepAliveSec > 0) and
       not (hfConnectionClose in fHttp.HeaderFlags) and
       (fServer.Async.fLastOperationSec > fKeepAliveSec) then
    begin
      fOwner.DoLog(sllTrace, 'DoRequest KeepAlive %>%: close connnection',
        [fServer.Async.fLastOperationSec, fKeepAliveSec], self);
      include(fHttp.HeaderFlags, hfConnectionClose); // before SetupResponse
    end;
    req.SetupResponse(fHttp, fServer.ServerName, err,
      fServer.OnSendFile, fServer.fCompressGz,
      fServer.fAsync.fClients.fSendBufferSize);
    fRespStatus := req.RespStatus;
  finally
    req.Free;
  end;
  // now try socket send() with headers (and small body if hrsResponseDone)
  // then TPollAsyncSockets.ProcessWrite/subscribe if needed if hrsSendBody
  if fHttp.Head.Len <> 0 then
    output := @fHttp.Head
  else
    output := @fHttp.Process;
  fServer.fAsync.fClients.Write(self, output.Buffer, output.Len, {timeout=}1000);
  // will call THttpAsyncConnection.AfterWrite once sent to finish/continue
end;


{ THttpAsyncConnections }

procedure THttpAsyncConnections.SetExecuteState(State: THttpServerExecuteState);
begin
  if State = esRunning then
    // SetExecuteState(esRunning) is done by TAsyncServer.Execute once started
    fExecuteAcceptOnly := true; // THttpAsyncServer.Execute will do the writes
  inherited SetExecuteState(state);
end;


{ THttpAsyncServer }

constructor THttpAsyncServer.Create(const aPort: RawUtf8; const OnStart,
  OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  aHeadersUnFiltered: boolean; CreateSuspended: boolean; aLogVerbose: boolean);
var
  aco: TAsyncConnectionsOptions;
begin
  // initialize HTTP parsing
  fCompressGz := -1;
  fHeadersDefaultBufferSize := 2048; // one fpcx64mm small block
  // setup connections
  if aLogVerbose then
    aco := ASYNC_OPTION_VERBOSE // for server debugging
  else
    aco := ASYNC_OPTION_PROD;   // default is to log only errors/warnings
  //include(aco, acoNoConnectionTrack);
  //include(aco, acoWritePollOnly);
  if fConnectionClass = nil then
    fConnectionClass := THttpAsyncConnection;
  if fConnectionsClass = nil then
    fConnectionsClass := THttpAsyncConnections;
  // bind and start the actual thread-pooled connections async server
  fAsync := fConnectionsClass.Create(aPort, OnStart, OnStop,
    fConnectionClass, ProcessName, TSynLog, aco, ServerThreadPoolCount);
  fAsync.fAsyncServer := self;
  // launch this TThread instance, but as suspended since Execute is void
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, aHeadersUnFiltered, CreateSuspended);
end;

destructor THttpAsyncServer.Destroy;
begin
  // no more incoming request
  Shutdown;
  // abort pending async process
  if fAsync <> nil then
    fAsync.Terminate;
  // terminate the (void) Execute (suspended) thread
  inherited Destroy;
  // finalize all thread-pooled connections
  FreeAndNilSafe(fAsync);
end;

function THttpAsyncServer.GetExecuteState: THttpServerExecuteState;
begin
  result := fAsync.fExecuteState; // state comes from THttpAsyncConnections
end;

function THttpAsyncServer.GetRegisterCompressGzStatic: boolean;
begin
  result := fCompressGz >= 0;
end;

procedure THttpAsyncServer.SetRegisterCompressGzStatic(Value: boolean);
begin
  if Value then
    fCompressGz := CompressIndex(fCompress, @CompressGzip)
  else
    fCompressGz := -1;
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
begin
  // Send() output packets in the background
  SetCurrentThreadName('W %', [fAsync.fProcessName]);
  NotifyThreadStart(self);
  WaitStarted(10);
  if fAsync = nil then
    exit;
  try
    fAsync.DoLog(sllTrace, 'Execute: main W loop', [], self);
    while not Terminated and
          not fAsync.Terminated do
      if fAsync.fClients.fWrite.GetOne(1000, 'W', notif) then
        fAsync.fClients.ProcessWrite(notif);
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

