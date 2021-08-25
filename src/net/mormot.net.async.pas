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
  /// store information of one TPollAsyncSockets connection
  {$ifdef USERECORDWITHMETHODS}
  TPollSocketsSlot = record
  {$else}
  TPollSocketsSlot = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the associated TCP connection
    // - equals 0 after TPollAsyncSockets.Stop
    socket: TNetSocket;
    /// Lock/Unlock R/W thread acquisition (lighter than a TRTLCriticalSection)
    lockcounter: array[boolean] of integer;
    /// the last error reported before the connection ends
    lastWSAError: TNetResult;
    /// flag set by TAsyncConnections.IdleEverySecond to purge rd/wr unused buffers
    // - avoid to call the GetTickCount64 system API for every activity
    wasactive: boolean;
    /// flag set when Subscribe() has been called
    writesubscribed: boolean;
    /// the current (reusable) read data buffer of this slot
    rd: TRawByteStringBuffer;
    /// the current (reusable) write data buffer of this slot
    wr: TRawByteStringBuffer;
    /// acquire an exclusive R/W access to this connection
    // - returns true if slot has been acquired, setting the wasactive flag
    // - returns false if it is used by another thread
    // - warning: this method is not re-entrant
    function Lock(writer: boolean): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// try to acquire an exclusive R/W access to this connection
    // - returns true if slot has been acquired
    // - returns false if it is used by another thread, after the timeoutMS period
    // - warning: this method is not re-entrant
    function TryLock(writer: boolean; timeoutMS: cardinal): boolean;
    /// release exclusive R/W access to this connection
    procedure UnLock(writer: boolean);
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// points to thread-safe information of one TPollAsyncSockets connection
  PPollSocketsSlot = ^TPollSocketsSlot;
  PPPollSocketsSlot = ^PPollSocketsSlot;

  /// possible options for TPollAsyncSockets process
  // - by default, TPollAsyncSockets.Write will first try to send the data
  // using Send() in non-blocking mode, unless paoWritePollOnly is defined,
  // and fWrite will be used to poll output state and send it asynchronously
  TPollAsyncSocketsOptions = set of (
    paoWritePollOnly);

  /// let TPollAsyncSockets.OnRead/AfterWrite shutdown the socket if needed
  TPollAsyncSocketOnReadWrite = (
    soContinue,
    soClose);

  {$M+}
  /// read/write buffer-oriented process of multiple non-blocking connections
  // - to be used e.g. for stream protocols (e.g. WebSockets or IoT communication)
  // - assigned sockets will be set in non-blocking mode, so that polling will
  // work as expected: you should then never use direclty the socket (e.g. via
  // blocking TCrtSocket), but rely on this class for asynchronous process:
  // OnRead() overriden method will receive all incoming data from input buffer,
  // and Write() should be called to add some data to asynchronous output buffer
  // - connections are identified as TObject instances, which should hold a
  // TPollSocketsSlot record as private values for the polling process
  // - ProcessRead/ProcessWrite methods are to be run for actual communication:
  // either you call those methods from multiple threads, or you run them in
  // loop from a single thread, then define a TSynThreadPool for running any
  // blocking process (e.g. computing requests answers) from OnRead callbacks
  // - inherited classes should override abstract OnRead, OnClose, OnError and
  // SlotFromConnection methods according to the actual connection class
  TPollAsyncSockets = class
  protected
    fRead: TPollSockets;
    // separated fWrite because its subscriptions are short-term
    fWrite: TPollSockets;
    fProcessingRead, fProcessingWrite: integer;
    fSendBufferSize: integer; // retrieved at first connection Start()
    fReadCount: integer;
    fWriteCount: integer;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fOptions: TPollAsyncSocketsOptions;
    fDebugLog: TSynLogClass;
    fProcessReadCheckPending: boolean;
    function GetCount: integer;
    procedure DoLog(const TextFmt: RawUtf8; const TextArgs: array of const);
    // (warning: abstract methods below should be properly overriden)
    // return low-level socket information from connection instance
    function SlotFromConnection(connection: TObject): PPollSocketsSlot;
      virtual; abstract;
    // extract frames from slot.rd, and handle them
    function OnRead(connection: TObject): TPollAsyncSocketOnReadWrite;
      virtual; abstract;
    // called when slot.wr content has been sent through the socket
    function AfterWrite(connection: TObject): TPollAsyncSocketOnReadWrite;
      virtual; abstract;
    // pseClosed: should do connection.free - Stop() has been called (socket=0)
    procedure OnClose(connection: TObject); virtual; abstract;
    // pseError: return false to close socket and connection (calling OnClose)
    function OnError(connection: TObject; events: TPollSocketEvents): boolean;
      virtual; abstract;
    procedure UnlockSlotAndCloseConnection(slot: PPPollSocketsSlot;
      var connection: TObject; const caller: shortstring);
  public
    /// initialize the read/write sockets polling
    // - fRead and fWrite TPollSocketsBuffer instances will track pseRead or
    // pseWrite events, and maintain input and output data buffers
    constructor Create(aOptions: TPollAsyncSocketsOptions); virtual;
    /// finalize buffer-oriented sockets polling, and release all used memory
    destructor Destroy; override;
    /// assign a new connection to the internal reading poll
    // - the TSocket handle will be retrieved via SlotFromConnection, and
    // set in non-blocking mode from now on - it is not recommended to access
    // it directly any more, but use Write() and handle OnRead() callback
    // - fRead will poll incoming packets, then call OnRead to handle them,
    // or Unsubscribe and delete the socket when pseClosed is notified
    // - fWrite will poll for outgoing packets as specified by Write(), then
    // send any pending data once the socket is ready
    function Start(connection: TObject): boolean; virtual;
    /// remove a connection from the internal poll, and shutdown its socket
    // - most of the time, the connection is released by OnClose when the other
    // end shutdown the socket; but you can explicitly call this method when
    // the connection (and its socket) is to be shutdown
    // - this method won't call OnClose, since it is initiated by the class
    function Stop(connection: TObject): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    // - this method may block if the connection is currently writing from
    // another thread (which is not possible from TPollAsyncSockets.Write),
    // up to timeout milliseconds
    function Write(connection: TObject; const data; datalen: integer;
      timeout: integer = 5000): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    function WriteString(connection: TObject; const data: RawByteString): boolean;
    /// one or several threads should execute this method
    // - thread-safe handle of any notified incoming packet
    function ProcessRead(const notif: TPollSocketResult): boolean;
    /// one thread should execute this method with the proper pseWrite notif
    // - thread-safe handle of any outgoing packets
    procedure ProcessWrite(const notif: TPollSocketResult);
    /// notify internal socket polls to stop their polling loop ASAP
    procedure Terminate(waitforMS: integer);
    /// low-level access to the polling class used for incoming data
    property PollRead: TPollSockets
      read fRead;
    /// low-level access to the polling class used for outgoind data
    property PollWrite: TPollSockets
      write fWrite;
    /// some processing options
    property Options: TPollAsyncSocketsOptions
      read fOptions write fOptions;
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

  /// 32-bit integer value used to identify an asynchronous connection
  // - will start from 1, and increase during the TAsyncConnections live-time
  TAsyncConnectionHandle = type integer;

  TAsyncConnections = class;

  /// abstract class to store one TAsyncConnections connection
  // - may implement e.g. WebSockets frames, or IoT binary protocol
  // - each connection will be identified by a TAsyncConnectionHandle integer
  // - idea is to minimize the resources used per connection, and allow full
  // customization of the process by overriding the OnRead virtual method (and,
  // if needed, AfterCreate/AfterWrite/BeforeDestroy/OnLastOperationIdle)
  TAsyncConnection = class(TSynPersistent)
  protected
    fMagic: cardinal; // for TAsyncConnectionsSockets.SlotFromConnection
    fHandle: TAsyncConnectionHandle;
    fLastOperation: cardinal;
    fSlot: TPollSocketsSlot;
    fRemoteIP: RawUtf8;
    fRemoteConnID: THttpServerConnectionID;
    /// this method is called when the instance is connected to a poll
    // - overriding this method is cheaper than the plain Create destructor
    procedure AfterCreate(Sender: TAsyncConnections); virtual;
    /// this method is called when the instance is about to be deleted from a poll
    // - default implementation will reset fHandle to 0
    // - overriding this method is cheaper than the plain Destroy destructor
    procedure BeforeDestroy(Sender: TAsyncConnections); virtual;
    /// this method is called when the some input data is pending on the socket
    // - should extract frames or requests from fSlot.rd, and handle them
    // - this is where the input should be parsed and extracted according to
    // the implemented procotol; fSlot.rd could be kept as temporary
    // buffer during the parsing, and rd.Reset called once processed
    // - Sender.Write() could be used for asynchronous answer sending
    // - Sender.LogVerbose() allows logging of escaped data
    // - could return sorClose to shutdown the socket, e.g. on parsing error
    function OnRead(Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
      virtual; abstract;
    /// this method is called when some data has been written to the socket
    // - default implementation will do nothing
    // - you may continue sending data asynchronously using fSlot.wr.Append()
    function AfterWrite(Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
      virtual;
    // called after TAsyncConnections.LastOperationIdleSeconds of no activity
    // - Sender.Write() could be used to send e.g. a hearbeat frame
    procedure OnLastOperationIdle(Sender: TAsyncConnections;
      IdleSeconds: cardinal); virtual;
    // called after TAsyncConnections.LastOperationReleaseMemorySeconds
    function ReleaseMemoryOnIdle: PtrInt; virtual;
  public
    /// initialize this instance
    constructor Create(const aRemoteIP: RawUtf8); reintroduce; virtual;
    /// read-only access to the socket number associated with this connection
    property Socket: TNetSocket
      read fSlot.socket;
  published
    /// the associated remote IP4/IP6, as text
    property RemoteIP: RawUtf8
      read fRemoteIP;
    /// read-only access to the handle number associated with this connection
    property Handle: TAsyncConnectionHandle
      read fHandle;
  end;

  /// meta-class of one TAsyncConnections connection
  TAsyncConnectionClass = class of TAsyncConnection;
  /// used to store a dynamic array of TAsyncConnection

  TAsyncConnectionObjArray = array of TAsyncConnection;

  /// handle multiple non-blocking connections using TAsyncConnection instances
  TAsyncConnectionsSockets = class(TPollAsyncSockets)
  protected
    fOwner: TAsyncConnections;
    function GetTotal: integer;
      {$ifdef HASINLINE} inline; {$endif}
    // safely but efficiently return TAsyncConnection(connection).fSlot
    function SlotFromConnection(connection: TObject): PPollSocketsSlot;
      override;
    // redirect to TAsyncConnection.OnRead
    function OnRead(connection: TObject): TPollAsyncSocketOnReadWrite;
      override;
    // redirect to TAsyncConnection.AfterWrite
    function AfterWrite(connection: TObject): TPollAsyncSocketOnReadWrite;
      override;
    // call fOwner.ConnectionDelete() to unregister from the connections list
    procedure OnClose(connection: TObject); override;
    // just log the error, and close connection if acoOnErrorContinue is not set
    function OnError(connection: TObject; events: TPollSocketEvents): boolean;
      override;
  public
    /// add some data to the asynchronous output buffer of a given connection
    // - this overriden method will also log the write operation if needed
    // - can be executed from an TAsyncConnection.OnRead method
    function Write(connection: TObject; const data; datalen: integer;
      timeout: integer = 5000): boolean; override;
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
  TAsyncConnectionsOptions = set of (
    acoOnErrorContinue,
    acoNoLogRead,
    acoNoLogWrite,
    acoVerboseLog,
    acoWritePollOnly,
    acoDebugReadWriteLog);

  /// implements an abstract thread-pooled high-performance TCP clients or server
  // - internal TAsyncConnectionsSockets will handle high-performance process
  // of a high number of long-living simultaneous connections
  // - will use a TAsyncConnection inherited class to maintain connection state
  // - don't use this abstract class but either TAsyncServer or TAsyncClients
  // - under Linux/POSIX, check your "ulimit -H -n" value: one socket consumes
  // two file descriptors: you may better add the following line to your
  // /etc/limits.conf or /etc/security/limits.conf system file:
  // $ * hard nofile 65535
  TAsyncConnections = class(TServerGeneric)
  protected
    fConnectionClass: TAsyncConnectionClass;
    fConnection: TAsyncConnectionObjArray;
    fConnectionCount: integer;
    fConnections: TDynArray; // fConnection[] sorted by TAsyncConnection.Handle
    fClients: TAsyncConnectionsSockets;
    fThreads: array of TAsyncConnectionsThread;
    fThreadReadPoll: TAsyncConnectionsThread;
    fThreadPoolCount: integer;
    fThreadPollingCount: integer;
    fLastHandle: integer;
    fLog: TSynLogClass;
    fTempConnectionForSearchPerHandle: TAsyncConnection;
    fOptions: TAsyncConnectionsOptions;
    fLastOperationReleaseMemorySeconds,
    fLastOperationIdleSeconds: cardinal;
    fThreadClients: record // used by TAsyncClient
      Count, Timeout: integer;
      Address, Port: RawUtf8;
    end;
    fConnectionLock: TSynLocker;
    fIdleTix: Int64;
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: RawUtf8;
      out aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionAdd(aSocket: TNetSocket; aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionDelete(aHandle: TAsyncConnectionHandle): boolean; overload; virtual;
    function ConnectionDelete(aConnection: TAsyncConnection; aIndex: integer): boolean; overload;
    procedure ThreadClientsConnect; // from fThreadClients
    procedure ThreadPollingWakeup(
      Events: integer; Kind: TAsyncConnectionsThreadProcesses);
    procedure DoLog(Level: TSynLogInfo; const TextFmt: RawUtf8;
      const TextArgs: array of const; Instance: TObject);
    procedure ProcessIdleTix(Sender: TObject; NowTix: Int64);
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
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the lock as:
    // ! try ... finally UnLock; end;
    function ConnectionFindLocked(aHandle: TAsyncConnectionHandle;
      aIndex: PInteger = nil): TAsyncConnection;
    /// just a wrapper around fConnectionLock.Lock
    procedure Lock;
    /// just a wrapper around fConnectionLock.UnLock
    procedure Unlock;
    /// remove an handle from the internal list, and close its connection
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function ConnectionRemove(aHandle: TAsyncConnectionHandle): boolean;
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function Write(connection: TAsyncConnection; const data; datalen: integer): boolean; overload;
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    function Write(connection: TAsyncConnection; const data: RawByteString): boolean; overload;
    /// log some binary data with proper escape
    // - can be executed from an TAsyncConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(self,...);
    procedure LogVerbose(connection: TAsyncConnection; const ident: RawUtf8;
      frame: pointer; framelen: integer); overload;
    /// log some binary data with proper escape
    // - can be executed from an TAsyncConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(...);
    procedure LogVerbose(connection: TAsyncConnection; const ident: RawUtf8;
      const frame: TRawByteStringBuffer); overload;
    /// allow idle connection to release its internal fSlot.rd/wr memory buffers
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
    property Connection: TAsyncConnectionObjArray
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
    fExecuteFinished: boolean;
    fMaxPending: integer;
    fMaxConnections: integer;
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
    fKeepAliveTix: Int64;
    fHeadersTix: Int64;
    fRespStatus: integer;
    procedure AfterCreate(Sender: TAsyncConnections); override;
    procedure BeforeDestroy(Sender: TAsyncConnections); override;
    procedure HttpInit;
    // redirect to fHttp.ProcessRead()
    function OnRead(Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
      override;
    // redirect to fHttp.ProcessWrite()
    function AfterWrite(Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
      override;
    function DoHeaders: TPollAsyncSocketOnReadWrite;
    function DoRequest: TPollAsyncSocketOnReadWrite;
  end;

  /// event-driven process of HTTP/WebSockets connections
  THttpAsyncConnections = class(TAsyncServer)
  protected
    fAsyncServer: THttpAsyncServer;
  end;

  /// HTTP server using non-blocking sockets
  THttpAsyncServer = class(THttpServerSocketGeneric)
  protected
    fAsync: THttpAsyncConnections;
    fCompressGz: integer;
    function GetRegisterCompressGzStatic: boolean;
    procedure SetRegisterCompressGzStatic(Value: boolean);
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
  public
    /// create an event-driven HTTP Server
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; aHeadersUnFiltered: boolean = false;
      AsyncOptions: TAsyncConnectionsOptions = []); reintroduce;
    /// finalize the HTTP Server
    destructor Destroy; override;
    /// if we should search for local .gz cached file when serving static files
    property RegisterCompressGzStatic: boolean
      read GetRegisterCompressGzStatic write SetRegisterCompressGzStatic;
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


{ TPollSocketsSlot }

function TPollSocketsSlot.Lock(writer: boolean): boolean;
begin
  result := InterlockedIncrement(lockcounter[writer]) = 1;
  if result then
    wasactive := true
  else
    LockedDec32(@lockcounter[writer]);
end;

procedure TPollSocketsSlot.Unlock(writer: boolean);
begin
  if @self <> nil then
    LockedDec32(@lockcounter[writer]);
end;

function TPollSocketsSlot.TryLock(writer: boolean; timeoutMS: cardinal): boolean;
var
  endtix: Int64;
  ms: integer;
begin
  result := (@self <> nil) and
            (socket <> nil);
  if not result then
    exit; // socket closed
  result := Lock(writer);
  if result or
     (timeoutMS = 0) then
    exit; // we acquired the slot, or we don't want to wait
  endtix := GetTickCount64 + timeoutMS; // never wait forever
  ms := 0;
  repeat
    SleepHiRes(ms);
    ms := ms xor 1; // 0,1,0,1,0,1...
    if socket = nil then
      exit; // no socket to lock for
    result := Lock(writer);
    if result then
    begin
      result := socket <> nil;
      if not result then
        UnLock(writer);
      exit; // acquired or socket closed
    end;
  until GetTickCount64 >= endtix;
end;


{ TPollAsyncSockets }

constructor TPollAsyncSockets.Create(aOptions: TPollAsyncSocketsOptions);
begin
  fOptions := aOptions;
  inherited Create;
  fRead := TPollSockets.Create;
  fWrite := TPollSockets.Create;
end;

destructor TPollAsyncSockets.Destroy;
begin
  if not fRead.Terminated then
    Terminate(5000);
  inherited Destroy;
  fRead.Free;
  fWrite.Free;
end;

function TPollAsyncSockets.Start(connection: TObject): boolean;
var
  slot: PPollSocketsSlot;
  res: TNetResult;
begin
  result := false;
  if fRead.Terminated or
     (connection = nil) then
    exit;
  LockedInc32(@fProcessingRead);
  try
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    res := slot.socket.MakeAsync;
    if res <> nrOK then
    begin
      if fDebugLog <> nil then
        DoLog('Start: MakeAsync(%)=% %',
          [PtrUInt(slot.socket), ToText(res)^, connection]);
      exit; // we expect non-blocking mode on a real working socket
    end;
    if fSendBufferSize = 0 then
      fSendBufferSize := slot.socket.SendBufferSize;
    result := fRead.Subscribe(slot.socket, [pseRead], TPollSocketTag(connection));
    // now, ProcessRead will handle pseRead + pseError/pseClosed on this socket
    if fDebugLog <> nil then
      DoLog('Start % %', [PtrUInt(slot.socket), connection]);
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

function TPollAsyncSockets.Stop(connection: TObject): boolean;
var
  slot: PPollSocketsSlot;
  sock: TNetSocket;
  endtix: Int64;
  lock: set of (r, w);
  dummy: byte;
  dummylen: integer;
begin
  result := false;
  if fRead.Terminated or
     (connection = nil) then
    exit;
  LockedInc32(@fProcessingRead);
  try
    slot := SlotFromConnection(connection);
    if slot = nil then
      exit;
    sock := slot.socket;
    if fDebugLog <> nil then
      DoLog('Stop % %', [PtrUInt(sock), connection]);
    if sock <> nil then
    try
      slot.socket := nil; // notify ProcessRead/ProcessWrite to abort
      dummylen := SizeOf(dummy);
      slot.lastWSAError := sock.Recv(@dummy, dummylen);
      if slot.lastWSAError = nrClosed then
        slot.lastWSAError := nrOK;
      fRead.Unsubscribe(sock, TPollSocketTag(connection));
      if slot.writesubscribed then
        fWrite.Unsubscribe(sock, TPollSocketTag(connection));
      result := true;
    finally
      sock.ShutdownAndClose({rdwr=}false);
      endtix := GetTickCount64 + 10000;
      lock := [];
      repeat
        if fRead.Terminated then
          break;
        // acquire r+w locks to avoid OnClose -> Connection.Free -> GPF
        if not (r in lock) and
           slot.Lock(false) then
          include(lock, r);
        if not (w in lock) and
           slot.Lock(true) then
          include(lock, w);
        if lock = [r, w] then
          break;
        SleepHiRes(0); // 10 microsecs on POSIX
      until GetTickCount64 >= endtix;
      if fDebugLog <> nil then
        DoLog('Stop lock=% slot=%', [byte(lock), ToText(slot.lastWSAError)^]);
    end;
  finally
    LockedDec32(@fProcessingRead);
  end;
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
    DoLog('Terminate(%) processing rd=% wr=%',
      [waitforMS, fProcessingRead, fProcessingWrite]);
  fRead.Terminate;
  fWrite.Terminate;
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
    DoLog('Terminate processing rd=% wr=% after %ms',
      [fProcessingRead, fProcessingWrite, elapsed]);
end;

function TPollAsyncSockets.WriteString(connection: TObject;
  const data: RawByteString): boolean;
begin
  if self = nil then
    result := false
  else
    result := Write(connection, pointer(data)^, length(data));
end;

function TPollAsyncSockets.Write(connection: TObject; const data;
  datalen: integer; timeout: integer): boolean;
var
  slot: PPollSocketsSlot;
  P: PByte;
  res: TNetResult;
  sent, previous: integer;
begin
  result := false;
  if (datalen <= 0) or
     (connection = nil) or
     fWrite.Terminated then
    exit;
  slot := SlotFromConnection(connection);
  if (slot = nil) or
     (slot.socket = nil) then
    exit;
  LockedInc32(@fProcessingWrite);
  if slot.TryLock(true, timeout) then // try and wait for another ProcessWrite
  try
    P := @data;
    previous := slot.wr.Len;
    if (previous = 0) and
       not (paoWritePollOnly in fOptions) then
      repeat
        // try to send now in non-blocking mode (works most of the time)
        if fWrite.Terminated or
           (slot.socket = nil) then
          exit;
        sent := datalen;
        res := slot.socket.Send(P, sent);
        if slot.socket = nil then
          exit;  // Stop() called
        if res = nrRetry then
          break; // fails now -> retry later in ProcessWrite
        if res <> nrOK then
        begin
          if fDebugLog <> nil then
            DoLog('Write: Send(%)=% %',
              [PtrUInt(slot.socket), ToText(res)^, connection]);
          exit;  // connection closed or broken -> abort
        end;
        inc(P, sent);
        inc(fWriteCount);
        inc(fWriteBytes, sent);
        dec(datalen, sent);
      until datalen = 0;
    if slot.socket = nil then
      exit;
    result := true;
    if datalen <> 0 then
      // use fWrite output polling for the remaining data in ProcessWrite
      slot.wr.Append(P, datalen)
    else
      // notify everything written - maybe call slot.wr.Append
      try
        result := AfterWrite(connection) = soContinue;
        if (not result) and
           (fDebugLog <> nil) then
          DoLog('Write % closed by AfterWrite callback %',
            [PtrUInt(slot.socket), connection]);
      except
        result := false;
      end;
    if result and
       (slot.wr.Len > 0) then
      // there is still some pending output bytes
      if previous = 0 then
      begin
        // register for ProcessWrite() if not already
        result := fWrite.Subscribe(
          slot.socket, [pseWrite], TPollSocketTag(connection));
        if result then
          slot.writesubscribed := true;
        if fDebugLog <> nil then
          DoLog('Write Subscribe(%,%)=% %',
            [PtrUInt(slot.socket), connection, result, fWrite]);
      end;
  finally
    if result then
      slot.UnLock({writer=}true)
    else
      // sending or subscription error -> abort
      UnlockSlotAndCloseConnection(@slot, connection, 'Write() finished');
    LockedDec32(@fProcessingWrite);
  end
  else
  begin
    if fDebugLog <> nil then
      DoLog('Write: TryLock failed % %', [connection, fWrite]);
    LockedDec32(@fProcessingWrite);
  end;
end;

procedure TPollAsyncSockets.UnlockSlotAndCloseConnection(
  slot: PPPollSocketsSlot; var connection: TObject; const caller: shortstring);
begin
  if fDebugLog <> nil then
    DoLog('UnlockSlotAndCloseConnection: % on % %', [caller, slot, connection]);
  if slot <> nil then
    slot^.UnLock({writer=}false); // Stop() will try to acquire this lock
  try
    Stop(connection); // shutdown and set socket:=0 + acquire locks
    OnClose(connection); // now safe to perform connection.Free
  except
    connection := nil;   // user code may be unstable
  end;
  if slot <> nil then
    slot^ := nil; // ignore pseClosed and slot.Unlock(false)
end;

function TPollAsyncSockets.ProcessRead(const notif: TPollSocketResult): boolean;
var
  connection: TObject;
  slot: PPollSocketsSlot;
  recved, added: integer;
  res: TNetResult;
  timer: TPrecisionTimer;
  temp: array[0..$7fff] of byte; // read up to 32KB per chunk
begin
  result := false;
  if (self = nil) or
     fRead.Terminated then
    exit;
  LockedInc32(@fProcessingRead);
  try
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    if pseError in notif.events then
      if not OnError(connection, notif.events) then
      begin
        UnlockSlotAndCloseConnection(nil, connection, 'ProcessRead error');
        exit;
      end;
    if pseRead in notif.events then
    begin
      added := 0;
      if slot.Lock({writer=}false) then // paranoid thread-safe read
      begin
        repeat
          if fRead.Terminated or
             (slot.socket = nil) then
            exit;
          recved := SizeOf(temp);
          if fDebugLog <> nil then
            timer.Start;
          res := slot.socket.Recv(@temp, recved);
          if fDebugLog <> nil then
            DoLog('ProcessRead recv(%)=% %B in % %',
              [PtrUInt(slot.socket), ToText(res)^, recved, timer.Stop, fRead]);
          if slot.socket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break; // may block, try later
          if res <> nrOk then
          begin
            UnlockSlotAndCloseConnection(@slot, connection, 'ProcessRead Recv');
            exit; // socket closed gracefully or unrecoverable error -> abort
          end;
          slot.rd.Append(@temp, recved);
          inc(added, recved);
        until recved < SizeOf(temp);
        if added > 0 then
          try
            result := true;
            inc(fReadCount);
            inc(fReadBytes, added);
            if OnRead(connection) = soClose then
              UnlockSlotAndCloseConnection(@slot, connection, 'OnRead abort');
          except
            UnlockSlotAndCloseConnection(@slot, connection, 'ProcessRead except');
          end;
        slot.UnLock(false); // UnlockSlotAndCloseConnection may set slot=nil
      end
      else if fDebugLog <> nil then
        DoLog('ProcessRead: Lock failed % %', [connection, fRead]);
    end;
    if (slot <> nil) and
       (slot.socket <> nil) and
       (pseClosed in notif.events) then
    begin
      UnlockSlotAndCloseConnection(nil, connection, 'ProcessRead terminated');
      exit;
    end;
  finally
    LockedDec32(@fProcessingRead);
  end;
end;

procedure TPollAsyncSockets.ProcessWrite(const notif: TPollSocketResult);
var
  connection: TObject;
  slot: PPollSocketsSlot;
  buf: PByte;
  buflen, bufsent, sent: integer;
  res: TNetResult;
  b: boolean;
  timer: TPrecisionTimer;
begin
  if (self = nil) or
     fWrite.Terminated or
     (notif.events <> [pseWrite]) then
    exit;
  // we are now sure that the socket is writable and safe
  LockedInc32(@fProcessingWrite);
  try
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    res := nrOK;
    if slot.Lock({writer=}true) then // paranoid check
    try
      buflen := slot.wr.Len;
      if buflen <> 0 then
      begin
        buf := slot.wr.Buffer;
        sent := 0;
        repeat
          if fWrite.Terminated or
             (slot.socket = nil) then
            exit;
          bufsent := buflen;
          if fDebugLog <> nil then
            timer.Start;
          res := slot.socket.Send(buf, bufsent);
          if fDebugLog <> nil then
            DoLog('ProcessWrite send(%)=% %B in % %',
              [PtrUInt(slot.socket), ToText(res)^, bufsent, timer.Stop, fWrite]);
          if slot.socket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break // may block, try later
          else if res <> nrOk then
          begin
            b := fWrite.Unsubscribe(slot.socket, notif.tag);
            slot.writesubscribed := false;
            if fDebugLog <> nil then
              DoLog('Write % Unsubscribe(%,%)=% %',
                [ToText(res)^, PtrUInt(slot.socket), connection, b, fWrite]);
            exit; // socket closed gracefully or unrecoverable error -> abort
          end;
          inc(fWriteCount);
          inc(sent, bufsent);
          inc(buf, bufsent);
          dec(buflen, bufsent);
        until buflen = 0;
        inc(fWriteBytes, sent);
        slot.wr.Remove(sent); // is very likely to just set wr.Len := 0
      end;
      if slot.wr.Len = 0 then
      begin
        // no data any more to be sent - maybe call slot.wr.Append
        try
          if AfterWrite(connection) <> soContinue then
          begin
            if fDebugLog <> nil then
              DoLog('ProcessWrite % closed by AfterWrite callback %',
                [PtrUInt(slot.socket), connection]);
            slot.wr.Clear;
            res := nrClosed;
          end;
        except
          slot.wr.Reset;
        end;
        if slot.wr.Len = 0 then
        begin
          // no further ProcessWrite unless slot.wr contains pending data
          b := fWrite.Unsubscribe(slot.socket, notif.tag);
          slot.writesubscribed := false;
          if fDebugLog <> nil then
            DoLog('Write Unsubscribe(%,%)=% %',
              [PtrUInt(slot.socket), connection, b, fWrite]);
        end;
      end;
    finally
      if res in [nrOk, nrRetry] then
        slot.UnLock(true)
      else
        // sending error or AfterWrite abort
        UnlockSlotAndCloseConnection(@slot, connection, 'ProcessWrite');
    end
    // if already locked (unlikely) -> will try next time
    else if fDebugLog <> nil then
      DoLog('ProcessWrite: Lock failed % %', [connection, fWrite]);
  finally
    LockedDec32(@fProcessingWrite);
  end;
end;


{ ******************** Client or Server Asynchronous Process }

{ TAsyncConnection }

const
  ASYNCCONNECTION_MAGIC = $3eedc0f3;

constructor TAsyncConnection.Create(const aRemoteIP: RawUtf8);
begin
  inherited Create;
  if aRemoteIP <> IP4local then
    fRemoteIP := aRemoteIP;
  fSlot.wasactive := true; // by definition
  fMagic := ASYNCCONNECTION_MAGIC; // TAsyncConnectionsSockets.SlotFromConnection
end;

procedure TAsyncConnection.AfterCreate(Sender: TAsyncConnections);
begin
end;

procedure TAsyncConnection.BeforeDestroy(Sender: TAsyncConnections);
begin
  fHandle := 0; // to detect any dangling pointer
  fMagic := 0; // for TAsyncConnectionsSockets.SlotFromConnection
end;

procedure TAsyncConnection.OnLastOperationIdle(
  Sender: TAsyncConnections; IdleSeconds: cardinal);
begin
end;

function TAsyncConnection.ReleaseMemoryOnIdle: PtrInt;
begin
  // after some inactivity, we can safely flush fSlot.rd/wr temporary buffers
  result := 0;
  if (fSlot.rd.Buffer <> nil) and
     fSlot.Lock({wr=}false) then
  begin
    inc(result, fSlot.rd.Capacity); // returns number of bytes released
    fSlot.rd.Clear;
    fSlot.UnLock(false);
  end;
  if (fSlot.wr.Buffer <> nil) and
     fSlot.Lock({wr=}true) then
  begin
    inc(result, fSlot.wr.Capacity);
    fSlot.wr.Clear;
    fSlot.UnLock(true);
  end;
  fSlot.wasactive := false; // fSlot.Lock() was with no true activity here
end;

function TAsyncConnection.AfterWrite(
  Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
begin
  result := soContinue; // nothing to do by default
end;


{ TAsyncConnectionsSockets }

procedure TAsyncConnectionsSockets.OnClose(connection: TObject);
begin
  // caller did call Stop() before calling OnClose (socket=0)
  if acoVerboseLog in fOwner.fOptions then
    fOwner.DoLog(sllTrace, 'OnClose%', [connection], self);
  // unregister from fOwner list and do connection.Free
  fOwner.ConnectionDelete((connection as TAsyncConnection).Handle);
end;

function TAsyncConnectionsSockets.OnError(connection: TObject; events:
  TPollSocketEvents): boolean;
var
  err: shortstring;
begin
  GetSetNameShort(TypeInfo(TPollSocketEvents), events, err);
  fOwner.DoLog(sllDebug, 'OnError% events=[%] -> free socket and instance',
    [connection, err], self);
  result := acoOnErrorContinue in fOwner.Options; // false=close by default
end;

function TAsyncConnectionsSockets.OnRead(
  connection: TObject): TPollAsyncSocketOnReadWrite;
var
  ac: TAsyncConnection;
begin
  ac := connection as TAsyncConnection;
  if not (acoNoLogRead in fOwner.Options) then
    fOwner.DoLog(sllTrace, 'OnRead% len=%', [ac, ac.fSlot.rd.Len], self);
  result := ac.OnRead(fOwner);
end;

function TAsyncConnectionsSockets.SlotFromConnection(
  connection: TObject): PPollSocketsSlot;
begin
  {$ifdef HASFASTTRYFINALLY}
  try
  {$endif HASFASTTRYFINALLY}
    if (connection = nil) or
       (TAsyncConnection(connection).fMagic <> ASYNCCONNECTION_MAGIC) or
       (TAsyncConnection(connection).Handle = 0) then
    begin
      fOwner.DoLog(sllError,
        'SlotFromConnection() with dangling pointer %', [pointer(connection)], self);
      result := nil;
    end
    else
      result := @TAsyncConnection(connection).fSlot;
  {$ifdef HASFASTTRYFINALLY}
  except
    fOwner.DoLog(sllError, 'SlotFromConnection() with dangling pointer %',
      [pointer(connection)], self);
    result := nil;
  end;
  {$endif HASFASTTRYFINALLY}
end;

function TAsyncConnectionsSockets.Write(connection: TObject; const data;
  datalen, timeout: integer): boolean;
var
  tmp: TLogEscape;
begin
  result := inherited Write(connection, data, datalen, timeout);
  if (fOwner.fLog <> nil) and
     not (acoNoLogWrite in fOwner.Options) then
    fOwner.DoLog(sllTrace, 'Write%=% len=%%',
      [connection, BOOL_STR[result], datalen,
       LogEscape(@data, datalen, tmp{%H-}, acoVerboseLog in fOwner.Options)],
      self);
end;

function TAsyncConnectionsSockets.AfterWrite(
  connection: TObject): TPollAsyncSocketOnReadWrite;
begin
  result := (connection as TAsyncConnection).AfterWrite(fOwner);
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
  inherited Create(false);
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
    start := 0;
    while not Terminated and
          (fOwner.fClients <> nil) do
    begin
      // implement parallel client connections for TAsyncClient
      if (fOwner.fThreadClients.Count > 0) and
         (InterlockedDecrement(fOwner.fThreadClients.Count) >= 0) then
        fOwner.ThreadClientsConnect
      else
        // main TAsyncConnections read/write process
        case fProcess of
          atpReadSingle:
            // a single thread to rule them all: polling, reading and processing
            if fOwner.fClients.fRead.GetOne(30000, notif) then
              fOwner.fClients.ProcessRead(notif);
          atpReadPoll:
            // main thread will just fill pending events from socket polls
            // (no process because a faulty service would delay all reading)
            begin
              start := 0; // back to SleepHiRes(0)
              while not Terminated do
              begin
                pending := fOwner.fClients.fRead.PollForPendingEvents(10);
                if pending = 0 then
                  SleepStep(start, @Terminated) // 0/1/10/50/150 ms steps
                else
                begin
                  // process fOwner.fClients.fPending in atpReadPending threads
                  fOwner.ThreadPollingWakeup(pending, [atpReadPending]);
                  // last atpReadPending does fThreadReadPoll.fEvent.SetEvent
                  if not Terminated then
                    fEvent.WaitFor(INFINITE);
                  break;
                end;
              end;
            end;
          atpReadPending:
            // secondary threads wait, then read and process pending events
            if fEvent.WaitFor(INFINITE) = wrSignaled then
              if Terminated then
                break
              else
              begin
                LockedInc32(@fOwner.fThreadPollingCount);
                while fOwner.fClients.fRead.GetOneWithinPending(notif) and
                      not Terminated do
                  fOwner.fClients.ProcessRead(notif);
                if InterlockedDecrement(fOwner.fThreadPollingCount) = 0 then
                  // release atpReadPoll lock above
                  fOwner.fThreadReadPoll.fEvent.SetEvent;
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
end;


{ TAsyncConnections }

function TAsyncConnectionCompareByHandle(const A, B): integer;
begin
  // for fast binary search from the connection handle (31-bit resolution is ok)
  result := TAsyncConnection(A).Handle - TAsyncConnection(B).Handle;
end;

constructor TAsyncConnections.Create(const OnStart, OnStop: TOnNotifyThread;
  aConnectionClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
  aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
var
  i: PtrInt;
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
  fLog := aLog;
  fConnectionClass := aConnectionClass;
  fConnectionLock.Init;
  fConnections.Init(TypeInfo(TPointerDynArray), fConnection, @fConnectionCount);
  // don't use TAsyncConnectionObjArray to manually call TAsyncConnection.BeforeDestroy
  fConnections.Compare := TAsyncConnectionCompareByHandle;
  opt := [];
  if acoWritePollOnly in aOptions then
    include(opt, paoWritePollOnly);
  fClients := TAsyncConnectionsSockets.Create(opt);
  fClients.fOwner := self;
  fClients.fWrite.OnGetOneIdle := ProcessIdleTix;
  if Assigned(fLog) and
     (acoDebugReadWriteLog in aOptions) then
  begin
    fClients.fDebugLog := fLog;
    fClients.fRead.OnLog := fLog.DoLog;
    fClients.fWrite.OnLog := fLog.DoLog;
  end;
  fTempConnectionForSearchPerHandle := fConnectionClass.Create('');
  fOptions := aOptions;
  inherited Create(false, OnStart, OnStop, ProcessName);
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
end;

destructor TAsyncConnections.Destroy;
var
  i: PtrInt;
begin
  Terminate;
  if fClients <> nil then
  begin
    with fClients do
      DoLog('Destroy threads=% total=% reads=%/% writes=%/%',
        [length(fThreads), Total, ReadCount, KB(ReadBytes), WriteCount, KB(WriteBytes)]);
    fClients.Terminate(5000);
  end;
  // stop ProcessRead/ProcessWrite when polling stops
  for i := 0 to high(fThreads) do
    fThreads[i].Terminate;
  // unlock fEvent to end all fThreads[].Execute
  ThreadPollingWakeup(maxInt, [atpReadPoll, atpReadPending]);
  // stop polling and refuse further Write/ConnectionRemove
  FreeAndNil(fClients);
  ObjArrayClear(fThreads);
  inherited Destroy;
  for i := 0 to fConnectionCount - 1 do
    try
      fConnection[i].BeforeDestroy(self);
      fConnection[i].Free;
    except
    end;
  fConnectionLock.Done;
  fTempConnectionForSearchPerHandle.Free;
end;

procedure TAsyncConnections.ThreadClientsConnect;
var
  res: TNetResult;
  client: TNetSocket;
  connection: TAsyncConnection;
begin
  if Terminated then
    exit;
  with fThreadClients do
    res := NewSocket(Address, Port, nlTcp, {bind=}false, timeout, timeout,
      timeout, {retry=}0, client);
  if res <> nrOK then
    raise EAsyncConnections.CreateUtf8('%: %:% connection failure (%)',
      [self, fThreadClients.Address, fThreadClients.Port, ToText(res)^]);
  connection := nil;
  if not ConnectionCreate(client, {ip=}'', connection) then
    client.ShutdownAndClose({rdwr=}false);
end;

procedure TAsyncConnections.ThreadPollingWakeup(
  Events: integer; Kind: TAsyncConnectionsThreadProcesses);
var
  i: PtrInt;
begin
  for i := 0 to high(fThreads) do
    if (fThreads[i].fEvent <> nil) and
       (fThreads[i].fProcess in Kind) then
    begin
      fThreads[i].fEvent.SetEvent;
      dec(Events);
      if Events = 0 then
        break;
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
    aConnection := fConnectionClass.Create(aRemoteIp);
    result := ConnectionAdd(aSocket, aConnection);
  end;
end;

function TAsyncConnections.ConnectionAdd(aSocket: TNetSocket;
  aConnection: TAsyncConnection): boolean;
begin
  result := false; // caller should release aSocket
  if Terminated then
    exit;
  aConnection.fSlot.socket := aSocket;
  fConnectionLock.Lock;
  try
    inc(fLastHandle);
    if fLastHandle <= 0 then // paranoid check
      raise EAsyncConnections.CreateUtf8(
        '%.ConnectionAdd: %.Handle overflow', [self, aConnection]);
    aConnection.fHandle := fLastHandle;
    fConnections.Add(aConnection);
    if acoVerboseLog in fOptions then
      DoLog(sllTrace, 'ConnectionAdd% count=%',
        [aConnection, fConnectionCount], self);
    fConnections.Sorted := true; // handles are increasing -> fast binary search
  finally
    fConnectionLock.UnLock;
  end;
  aConnection.AfterCreate(self);
  result := true; // indicates aSocket owned by the pool
end;

function TAsyncConnections.ConnectionDelete(aConnection: TAsyncConnection;
  aIndex: integer): boolean;
var
  t: TClass;
  h: TAsyncConnectionHandle;
begin
  // caller should have done fConnectionLock.Lock
  try
    t := PClass(aConnection)^;
    h := aConnection.Handle;
    aConnection.BeforeDestroy(self);
    aConnection.Free;
  finally
    fConnections.FastDeleteSorted(aIndex);
  end;
  if acoVerboseLog in fOptions then
    DoLog(sllTrace, 'ConnectionDelete %.Handle=% count=%',
      [t, h, fConnectionCount], self);
  result := true;
end;

function TAsyncConnections.ConnectionDelete(aHandle: TAsyncConnectionHandle): boolean;
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

function TAsyncConnections.ConnectionFindLocked(aHandle: TAsyncConnectionHandle;
  aIndex: PInteger): TAsyncConnection;
var
  i: integer;
begin
  result := nil;
  if (self = nil) or
     Terminated or
     (aHandle <= 0) then
    exit;
  fConnectionLock.Lock;
  try
    // fast O(log(n)) binary search
    fTempConnectionForSearchPerHandle.fHandle := aHandle;
    i := fConnections.Find(fTempConnectionForSearchPerHandle);
    if i >= 0 then
    begin
      result := fConnection[i];
      if aIndex <> nil then
        aIndex^ := i;
    end;
    if acoVerboseLog in fOptions then
      DoLog(sllTrace, 'ConnectionFindLocked(%)=%', [aHandle, result], self);
  finally
    if result = nil then
      fConnectionLock.UnLock;
  end;
end;

function TAsyncConnections.ConnectionRemove(aHandle: TAsyncConnectionHandle): boolean;
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

procedure TAsyncConnections.Lock;
begin
  fConnectionLock.Lock;
end;

procedure TAsyncConnections.Unlock;
begin
  fConnectionLock.UnLock;
end;

function TAsyncConnections.Write(connection: TAsyncConnection; const data;
  datalen: integer): boolean;
begin
  if Terminated then
    result := false
  else
    result := fClients.Write(connection, data, datalen);
end;

function TAsyncConnections.Write(connection: TAsyncConnection;
  const data: RawByteString): boolean;
begin
  if Terminated then
    result := false
  else
    result := fClients.WriteString(connection, data);
end;

procedure TAsyncConnections.LogVerbose(connection: TAsyncConnection;
  const ident: RawUtf8; frame: pointer; framelen: integer);
var
  tmp: TLogEscape;
begin
  if not (acoNoLogRead in Options) and
     (acoVerboseLog in Options) and
     (fLog <> nil) then
    DoLog(sllTrace, '% len=%%',
      [ident, framelen, LogEscape(frame, framelen, tmp{%H-})], connection);
end;

procedure TAsyncConnections.LogVerbose(connection: TAsyncConnection;
  const ident: RawUtf8; const frame: TRawByteStringBuffer);
begin
  LogVerbose(connection, ident, frame.Buffer, frame.Len)
end;

procedure TAsyncConnections.IdleEverySecond(tix: Int64);
var
  i: PtrInt;
  notified, gced: PtrInt;
  now, allowed, gc: cardinal;
  c: TAsyncConnection;
  log: ISynLog;
begin
  if Terminated or
     (fConnectionCount = 0) then
    exit;
  notified := 0;
  gced := 0;
  now := Qword(tix) div 1000; // we work at 32-bit second resolution here
  allowed := LastOperationIdleSeconds;
  if allowed <> 0 then
    allowed := now - allowed;
  gc := LastOperationReleaseMemorySeconds;
  if gc <> 0 then
    gc := now - gc;
  fConnectionLock.Lock;
  try
    for i := 0 to fConnectionCount - 1 do
    begin
      c := fConnection[i];
      if c.fSlot.wasactive then
      begin
        c.fSlot.wasactive := false;
        c.fLastOperation := now; // update once per second is good enough
      end
      else
      begin
        if (gc <> 0) and
           (c.fLastOperation < gc) then
          inc(gced, c.ReleaseMemoryOnIdle);
        if (allowed <> 0) and
           (c.fLastOperation < allowed) then
          try
            if {%H-}log = nil then
              if acoVerboseLog in fOptions then
                log := fLog.Enter(self, 'IdleEverySecond');
            c.OnLastOperationIdle(self, now - c.fLastOperation);
            inc(notified);
            if Terminated then
              break;
          except
          end;
      end;
    end;
    if log <> nil then
      log.Log(sllTrace, 'IdleEverySecond % notified=% GC=%',
        [fConnectionClass, notified, KBNoSpace(gced)], self)
    else if gced <> 0 then
      if acoVerboseLog in fOptions then
        DoLog(sllTrace, 'IdleEverySecond % GC=%',
          [fConnectionClass, KBNoSpace(gced)], self);
  finally
    fConnectionLock.UnLock;
  end;
end;

procedure TAsyncConnections.ProcessIdleTix(Sender: TObject; NowTix: Int64);
begin
  if not Terminated then
    if NowTix >= fIdleTix then
    begin
      IdleEverySecond(NowTix);
      // IdleEverySecond may take some time -> retrieve ticks again
      fIdleTix := mormot.core.os.GetTickCount64 + 1000;
    end;
end;


{ TAsyncServer }

constructor TAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; aConnectionClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
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
begin
  Terminate;
  endtix := mormot.core.os.GetTickCount64 + 10000;
  if fServer <> nil then
  begin
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
    if NewSocket('127.0.0.1', fServer.Port, nlTcp, false,
         10, 0, 0, 0, touchandgo) = nrOk then
      touchandgo.ShutdownAndClose(false);
  end;
  inherited Destroy;
  while not fExecuteFinished and
        (mormot.core.os.GetTickCount64 < endtix) do
    SleepHiRes(1); // wait for Execute to be finalized (unlikely)
  fServer.Free;
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
begin
  // Accept() incoming connections and Send() output packets in the background
  SetCurrentThreadName('AW %', [fProcessName]);
  NotifyThreadStart(self);
  // constructor did bind fServer to the expected TCP port
  if fServer.Sock <> nil then
  try
    // make Accept() non-blocking (and therefore also systemd-ready)
    if not fServer.SockIsDefined or // paranoid (Bind would have raise an exception)
       (fServer.Sock.MakeAsync <> nrOK) then
      raise EAsyncConnections.CreateUtf8(
        '%.Execute: %.Bind failed', [self, fServer]);
    // setup the main bound connection to be polling together with the writes
    fClients.fWrite.Subscribe(fServer.Sock, [pseRead], {notif.tag=}0);
    // main socket accept/send processing loop
    start := 0;
    while not Terminated do
    begin
      if fClients.fWrite.GetOne(1000, notif) then
        if Terminated then
          break
        else if notif.tag = 0 then
        begin
          // could we Accept an incoming connection?
          res := fServer.Sock.Accept(client, sin);
          if Terminated then
            break;
          if (fClients.fRead.Count > fMaxConnections) or
             (fClients.fRead.PendingCount > fMaxPending) then
            // map THttpAsyncServer.HttpQueueLength property value
            res := nrTooManyConnections;
          if res <> nrOK then
            if res = nrRetry then
              continue // we reached ReceiveTimeout
            else
            begin
              // failure (too many clients?) -> wait and retry
              DoLog(sllWarning, 'Execute: Accept(%) failed as %',
                [fServer.Port, ToText(res)^], self);
              if res <> nrTooManyConnections then
                // progressive wait (if not load-balancing, but socket error)
                SleepStep(start, @Terminated);
              continue;
            end;
          if Terminated then
          begin
            client.ShutdownAndClose({rdwr=}false);
            break;
          end;
          // if we reached here, we have accepted a connection -> process
          start := 0;
          ip := sin.IP;
          if ConnectionCreate(client, ip, connection) then
            if fClients.Start(connection) then
            begin
              if acoVerboseLog in fOptions then
                DoLog(sllTrace, 'Execute: Accept(%)=%',
                  [fServer.Port, connection], self);
            end
            else
              connection.Free
          else
            client.ShutdownAndClose({rdwr=}false);
        end
        else
          // this was a pseWrite notification -> try to send pending data
          // here connection = TObject(notif.tag)
          fClients.ProcessWrite(notif);
    end;
  except
    on E: Exception do
      DoLog(sllWarning, 'Execute raised % -> terminate %',
        [E.ClassType, fProcessName], self);
  end;
  DoLog(sllInfo, 'Execute: done AW %', [fProcessName], self);
  fExecuteFinished := true;
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
      if fClients.fWrite.GetOne(1000, notif) then
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

procedure THttpAsyncConnection.AfterCreate(Sender: TAsyncConnections);
begin
  fServer := (Sender as THttpAsyncConnections).fAsyncServer;
  HttpInit;
  if fServer.ServerKeepAliveTimeOut > 0 then
    fKeepAliveTix := GetTickCount64 + fServer.ServerKeepAliveTimeOut;
end;

procedure THttpAsyncConnection.BeforeDestroy(Sender: TAsyncConnections);
begin
  fHttp.ProcessDone;
  inherited BeforeDestroy(Sender);
end;

procedure THttpAsyncConnection.HttpInit;
begin
  fHttp.ProcessInit({instream=}nil); // ready to process this HTTP request
  if fServer.HeadersUnFiltered then
    include(fHttp.Options, hroHeadersUnfiltered);
  fHeadersTix := 0;
end;

function THttpAsyncConnection.OnRead(Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
begin
  if Sender.fClients = nil then
    fHttp.State := hrsErrorMisuse
  else if fServer.fShutdownInProgress then
    fHttp.State := hrsErrorShutdownInProgress
  else
  begin
    // use the HTTP machine state to parse fSlot.rd input
    fHttp.ProcessRead(fSlot.rd.Buffer, fSlot.rd.Len);
    fSlot.rd.Reset;
  end;
  // compute next step
  case fHttp.State of
    hrsGetCommand:
      begin
        if fServer.HeaderRetrieveAbortDelay > 0 then
          // start measuring time for receiving the headers
          fHeadersTix := GetTickCount64 + fServer.HeaderRetrieveAbortDelay;
        result := soContinue;
      end;
    hrsGetHeaders,
    hrsGetBodyChunkedHexNext,
    hrsGetBodyChunkedData,
    hrsGetBodyChunkedDataVoidLine,
    hrsGetBodyChunkedDataLastLine:
      // async receiving phase
      result := soContinue;
    hrsGetBodyChunkedHexFirst,
    hrsGetBodyContentLength:
      // we just received all command + headers
      result := DoHeaders;
    hrsWaitProcessing:
      // calls the (blocking) HTTP request processing callback
      result := DoRequest;
  else
    begin
      Sender.DoLog(sllWarning, 'OnRead: close connection after %',
        [ToText(fHttp.State)^], self);
      result := soClose
    end;
  end;
end;

function THttpAsyncConnection.AfterWrite(
  Sender: TAsyncConnections): TPollAsyncSocketOnReadWrite;
begin
  if Sender.fClients = nil then
    fHttp.State := hrsErrorMisuse;
  // compute next step
  case fHttp.State of
    hrsSendBody:
      begin
        // use the HTTP machine state to fill fSlot.wr with outgoing body data
        fHttp.ProcessBody(fSlot.wr, Sender.fClients.fSendBufferSize);
        result := soContinue;
      end;
    hrsResponseDone:
      begin
        // all headers + body outgoing content was sent
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
          fHttp.ProcessDone; // ContentStream.Free
          HttpInit;
          result := soContinue;
          LockedInc32(@fServer.fStats[grOwned]);
        end;
      end
  else
    begin
      Sender.DoLog(sllWarning, 'AfterWrite: unexpected %',
        [ToText(fHttp.State)^], self);
      result := soClose;
    end;
  end;
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
  fServer.ParseRemoteIPConnID(fHttp.Headers,
    fRemoteIP, fRemoteConnID);
  // immediate reject of clearly invalid requests
  status := HTTP_SUCCESS;
  if (fServer.MaximumAllowedContentLength > 0) and
     (fHttp.ContentLength > fServer.MaximumAllowedContentLength) then
  begin
    status := HTTP_PAYLOADTOOLARGE; // 413
    LockedInc32(@fServer.fStats[grOversizedPayload]);
  end else if (fHeadersTix > 0) and
              (GetTickCount64 > fHeadersTix) then
  begin
    status := HTTP_TIMEOUT; // 408
    LockedInc32(@fServer.fStats[grTimeout]);
  end
  else if Assigned(fServer.OnBeforeBody) then
    status := fServer.OnBeforeBody(
      fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers,
      fHttp.ContentType, fRemoteIP, fHttp.BearerToken,
      fHttp.ContentLength, {notls=}[]);
  if status <> HTTP_SUCCESS then
  begin
    // on fatal error direct reject and close the connection
    // (use fHttp.Command* as temp variables to avoid local RawUtf8 allocation)
    StatusCodeToReason(status, fHttp.Command);
    FormatUtf8('HTTP/1.0 % %'#13#10#13#10'Server Rejected Request as % %',
      [status, fHttp.Command, status, fHttp.Command],
      fHttp.CommandUri);
    fServer.fAsync.fClients.WriteString(self, fHttp.CommandUri); // no polling
    LockedInc32(@fServer.fStats[grRejected]);
    exit;
  end;
  // now THttpAsyncConnection.OnRead can get the body
  LockedInc32(@fServer.fStats[grHeaderReceived]);
  if (fHttp.State <> hrsWaitProcessing) and
     not HttpMethodWithNoBody(fHttp.CommandMethod) then
    // HEAD and OPTIONS are requests with Content-Length header but no body
    result := DoRequest
  else
    result := soContinue;
end;

function THttpAsyncConnection.DoRequest: TPollAsyncSocketOnReadWrite;
var
  req: THttpServerRequest;
  cod: integer;
  err: string;
begin
  // check the status
  if nfHeadersParsed in fHttp.HeaderFlags then
    LockedInc32(@fServer.fStats[grBodyReceived])
  else
    begin
      // content-length was 0, so hrsGetBody* and DoHeaders() were not called
      result := DoHeaders;
      if result <> soContinue then
        exit; // rejected
    end;
  // compute the HTTP/REST process
  result := soClose;
  req := THttpServerRequest.Create(fServer, fRemoteConnID, {thread=}nil, []);
  try
    req.Prepare(fHttp.CommandUri, fHttp.CommandMethod, fHttp.Headers,
      fHttp.Content, fHttp.ContentType, fRemoteIP);
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
          LockedInc32(@fServer.fStats[grException]);
          // will keep soClose as result to shutdown the connection
        end;
    end;
    // prepare the response for the HTTP state machine
    req.SetupResponse(fHttp, fServer.ServerName, err,
      fServer.OnSendFile, fServer.fCompressGz);
    fRespStatus := req.RespStatus;
    if (fKeepAliveTix > 0) and
       not (hfConnectionClose in fHttp.HeaderFlags) and
       (GetTickCount64 > fKeepAliveTix) then
      include(fHttp.HeaderFlags, hfConnectionClose);
  finally
    req.Free;
  end;
  // now try socket send() with headers (and small body as hrsResponseDone)
  // then TPollAsyncSockets.ProcessWrite/subscribe if needed as hrsSendBody
  fServer.fAsync.fClients.Write(self, fHttp.Head.Buffer^, fHttp.Head.Len);
end;


{ THttpAsyncServer }

constructor THttpAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  aHeadersUnFiltered: boolean; AsyncOptions: TAsyncConnectionsOptions);
begin
  fCompressGz := -1;
  fAsync := THttpAsyncConnections.Create(aPort, OnStart, OnStop,
    THttpAsyncConnection, ProcessName, TSynLog, AsyncOptions, ServerThreadPoolCount);
  fAsync.fAsyncServer := self;
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, aHeadersUnFiltered, {suspended=}true);
end;

destructor THttpAsyncServer.Destroy;
begin
  if fAsync <> nil then
    fAsync.Terminate;
  inherited Destroy;
  FreeAndNil(fAsync);
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


end.

