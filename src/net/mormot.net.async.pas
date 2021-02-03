/// Asynchronous Network Layer for Event-Driven Clients or Servers
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.async;

{
  *****************************************************************************

   Event-Driven Network Classes and Functions
   - Low-Level Non-blocking Connections
   - Client or Server Asynchronous Process

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
  mormot.core.buffers,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.rtti,
  mormot.net.sock,
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
    /// the current read data buffer of this slot
    readbuf: RawByteString;
    /// the current write data buffer of this slot
    writebuf: RawByteString;
    /// acquire an exclusive R/W access to this connection
    // - returns true if slot has been acquired
    // - returns false if it is used by another thread
    // - warning: this method is not re-entrant
    function Lock(writer: boolean): boolean;
    /// try to acquire an exclusive R/W access to this connection
    // - returns true if slot has been acquired
    // - returns false if it is used by another thread, after the timeoutMS period
    // - warning: this method is not re-entrant
    function TryLock(writer: boolean; timeoutMS: cardinal): boolean;
    /// release exclusive R/W access to this connection
    procedure UnLock(writer: boolean);
  end;

  /// points to thread-safe information of one TPollAsyncSockets connection
  PPollSocketsSlot = ^TPollSocketsSlot;

  /// possible options for TPollAsyncSockets process
  // - by default, TPollAsyncSockets.Write will first try to send the data
  // using Send() in non-blocking mode, unless paoWritePollOnly is defined,
  // and fWrite will be used to poll output state and send it asynchronously
  TPollAsyncSocketsOptions = set of (
    paoWritePollOnly);

  /// let TPollAsyncSockets.OnRead shutdown the socket if needed
  TPollAsyncSocketOnRead = (
    sorContinue,
    sorClose);

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
    fWrite: TPollSockets;
    fReadCount: integer;
    fWriteCount: integer;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fProcessing: integer;
    fOptions: TPollAsyncSocketsOptions;
    function GetCount: integer;
    // warning: abstract methods below should be properly overriden
    // return low-level socket information from connection instance
    function SlotFromConnection(connection: TObject): PPollSocketsSlot; virtual; abstract;
    // extract frames from slot.readbuf, and handle them
    function OnRead(connection: TObject): TPollAsyncSocketOnRead; virtual; abstract;
    // called when slot.writebuf has been sent through the socket
    procedure AfterWrite(connection: TObject); virtual; abstract;
    // pseClosed: should do connection.free - Stop() has been called (socket=0)
    procedure OnClose(connection: TObject); virtual; abstract;
    // pseError: return false to close socket and connection (calling OnClose)
    function OnError(connection: TObject; events: TPollSocketEvents): boolean;
      virtual; abstract;
  public
    /// initialize the read/write sockets polling
    // - fRead and fWrite TPollSocketsBuffer instances will track pseRead or
    // pseWrite events, and maintain input and output data buffers
    constructor Create; virtual;
    /// finalize buffer-oriented sockets polling, and release all used memory
    destructor Destroy; override;
    /// assign a new connection to the internal poll
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
    // end shutdown the socket; but you can explicitely call this method when
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
    // - thread-safe handle of any incoming packets
    // - if this method is called from a single thread, you should use
    // a TSynThreadPool for any blocking process of OnRead events
    // - otherwise, this method is thread-safe, and incoming packets may be
    // consumed from a set of threads, and call OnRead with newly received data
    procedure ProcessRead(timeoutMS: integer);
    /// one or several threads should execute this method
    // - thread-safe handle of any outgoing packets
    procedure ProcessWrite(timeoutMS: integer);
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
    fSlot: TPollSocketsSlot;
    fHandle: TAsyncConnectionHandle;
    fLastOperation: cardinal;
    fRemoteIP: RawUtf8;
    /// this method is called when the instance is connected to a poll
    // - default implementation will set fLastOperation content
    procedure AfterCreate(Sender: TAsyncConnections); virtual;
    /// this method is called when the some input data is pending on the socket
    // - should extract frames or requests from fSlot.readbuf, and handle them
    // - this is where the input should be parsed and extracted according to
    // the implemented procotol; fSlot.readbuf could be kept as temporary
    // buffer during the parsing, and voided by the caller once processed
    // - Sender.Write() could be used for asynchronous answer sending
    // - Sender.LogVerbose() allows logging of escaped data
    // - could return sorClose to shutdown the socket, e.g. on parsing error
    function OnRead(Sender: TAsyncConnections): TPollAsyncSocketOnRead;
      virtual; abstract;
    /// this method is called when some data has been written to the socket
    // - default implementation will do nothing
    procedure AfterWrite(Sender: TAsyncConnections); virtual;
    /// this method is called when the instance is about to be deleted from a poll
    // - default implementation will reset fHandle to 0
    procedure BeforeDestroy(Sender: TAsyncConnections); virtual;
    // called after TAsyncConnections.LastOperationIdleSeconds of no activity
    // - reset fLastOperation by default - overriden code should be fast
    // - Sender.Write() could be used to send e.g. a hearbeat frame
    procedure OnLastOperationIdle(Sender: TAsyncConnections); virtual;
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
  // - OnRead will redirect to TAsyncConnection.OnRead virtual method
  // - OnClose will remove the instance from TAsyncConnections.fConnections[]
  // - OnError will return false to shutdown the connection (unless
  // acoOnErrorContinue is defined in TAsyncConnections.Options)
  TAsyncConnectionsSockets = class(TPollAsyncSockets)
  protected
    fOwner: TAsyncConnections;
    function SlotFromConnection(connection: TObject): PPollSocketsSlot; override;
    function OnRead(connection: TObject): TPollAsyncSocketOnRead; override;
    procedure AfterWrite(connection: TObject); override;
    procedure OnClose(connection: TObject); override;
    function OnError(connection: TObject; events: TPollSocketEvents): boolean; override;
    function GetTotal: integer;
  public
    /// add some data to the asynchronous output buffer of a given connection
    // - this overriden method will refresh TAsyncConnection.LastOperation
    // - can be executed from an TAsyncConnection.OnRead method
    function Write(connection: TObject; const data; datalen: integer;
      timeout: integer = 5000): boolean; override;
  published
    /// how many clients have been handled by the poll, from the beginning
    property Total: integer
      read GetTotal;
  end;

  /// used to implement a thread poll to process TAsyncConnection instances
  TAsyncConnectionsThread = class(TSynThread)
  protected
    fOwner: TAsyncConnections;
    fProcess: TPollSocketEvent; // pseRead or pseWrite
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(aOwner: TAsyncConnections; aProcess: TPollSocketEvent);
      reintroduce;
  end;

  /// low-level options for TAsyncConnections processing
  // - TAsyncConnectionsSockets.OnError will shutdown the connection on any error,
  // unless acoOnErrorContinue is defined
  // - acoOnAcceptFailureStop will let failed Accept() finalize the process
  // - acoNoLogRead and acoNoLogWrite could reduce the log verbosity
  // - acoVerboseLog will log transmitted frames content, for debugging purposes
  // - acoLastOperationNoRead and acoLastOperationNoWrite could be used to
  // avoid TAsyncConnection.fLastOperation reset at read or write
  TAsyncConnectionsOptions = set of (
    acoOnErrorContinue,
    acoOnAcceptFailureStop,
    acoNoLogRead,
    acoNoLogWrite,
    acoVerboseLog,
    acoLastOperationNoRead,
    acoLastOperationNoWrite);

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
    fStreamClass: TAsyncConnectionClass;
    fConnection: TAsyncConnectionObjArray;
    fConnectionCount: integer;
    fConnections: TDynArray; // fConnection[] sorted by TAsyncConnection.Handle
    fClients: TAsyncConnectionsSockets;
    fThreads: array of TAsyncConnectionsThread;
    fLastHandle: integer;
    fLog: TSynLogClass;
    fTempConnectionForSearchPerHandle: TAsyncConnection;
    fOptions: TAsyncConnectionsOptions;
    fLastOperationIdleSeconds: cardinal;
    fThreadClients: record // used by TAsyncClient
      Count, Timeout: integer;
      Address, Port: RawUtf8;
    end;
    fConnectionLock: TSynLocker;
    procedure IdleEverySecond;
    function ConnectionCreate(aSocket: TNetSocket; const aRemoteIp: RawUtf8;
      out aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionAdd(aSocket: TNetSocket; aConnection: TAsyncConnection): boolean; virtual;
    function ConnectionDelete(aHandle: TAsyncConnectionHandle): boolean; overload; virtual;
    function ConnectionDelete(aConnection: TAsyncConnection; aIndex: integer): boolean; overload;
    procedure ThreadClientsConnect; // from fThreadClients
  public
    /// initialize the multiple connections
    // - warning: currently reliable only with aThreadPoolCount=1
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      aStreamClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the instance, releasing all associated threads and sockets
    destructor Destroy; override;
    /// high-level access to a connection instance, from its handle
    // - could be executed e.g. from a TAsyncConnection.OnRead method
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the lock as:
    // ! try ... finally UnLock; end;
    function ConnectionFindLocked(aHandle: TAsyncConnectionHandle; aIndex:
      PInteger = nil): TAsyncConnection;
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
      const frame: RawByteString); overload;
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
    fServer: TCrtSocket;
    fExecuteFinished: boolean;
    procedure Execute; override;
  public
    /// run the TCP server, listening on a supplied IP port
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      aStreamClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
      aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions;
      aThreadPoolCount: integer = 1); reintroduce; virtual;
    /// shut down the server, releasing all associated threads and sockets
    destructor Destroy; override;
  published
    /// access to the TCP server socket
    property Server: TCrtSocket
      read fServer;
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
      aStreamClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
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
  if not result then
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

constructor TPollAsyncSockets.Create;
var
  c: TPollSocketClass;
begin
  inherited Create;
  c := PollSocketClass;
  fRead := TPollSockets.Create(c);
  { TODO : try TPollSocketEPoll for fWrite on LINUXNOTBSD ? }
  fWrite := TPollSockets.Create(c);
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
begin
  result := false;
  if (fRead.Terminated) or
     (connection = nil) then
    exit;
  LockedInc32(@fProcessing);
  try
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    if slot.socket.MakeAsync <> nrOK then
      exit; // we expect non-blocking mode on a real working socket
    result := fRead.Subscribe(slot.socket, [pseRead], TPollSocketTag(connection));
    // now, ProcessRead will handle pseRead + pseError/pseClosed on this socket
  finally
    LockedDec32(@fProcessing);
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
  LockedInc32(@fProcessing);
  try
    slot := SlotFromConnection(connection);
    if slot = nil then
      exit;
    sock := slot.socket;
    if sock <> nil then
    try
      slot.socket := nil; // notify ProcessRead/ProcessWrite to abort
      dummylen := SizeOf(dummy);
      slot.lastWSAError := sock.Recv(@dummy, dummylen);
      if slot.lastWSAError = nrClosed then
        slot.lastWSAError := nrOK;
      fRead.Unsubscribe(sock, TPollSocketTag(connection));
      fWrite.Unsubscribe(sock, TPollSocketTag(connection));
      result := true;
    finally
      sock.ShutdownAndClose({rdwr=}false);
      endtix := GetTickCount64 + 10000;
      lock := [];
      repeat // acquire locks to avoid OnClose -> Connection.Free -> GPF
        if not (r in lock) and slot.Lock(false) then
          include(lock, r);
        if not (w in lock) and slot.Lock(true) then
          include(lock, w);
        if lock = [r, w] then
          break;
        SleepHiRes(0); // 10 microsecs on POSIX
      until GetTickCount64 >= endtix;
    end;
  finally
    LockedDec32(@fProcessing);
  end;
end;

function TPollAsyncSockets.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fRead.Count;
end;

procedure TPollAsyncSockets.Terminate(waitforMS: integer);
var
  endtix: Int64;
begin
  fRead.Terminate;
  fWrite.Terminate;
  if waitforMS <= 0 then
    exit;
  endtix := GetTickCount64 + waitforMS;
  repeat
    SleepHiRes(1);
    if fProcessing = 0 then
      break;
  until GetTickCount64 > endtix;
end;

function TPollAsyncSockets.WriteString(connection: TObject;
  const data: RawByteString): boolean;
begin
  if self = nil then
    result := false
  else
    result := Write(connection, pointer(data)^, length(data));
end;

procedure AppendData(var buf: RawByteString; const data; datalen: PtrInt);
var
  buflen: PtrInt;
begin
  if datalen > 0 then
  begin
    buflen := length(buf);
    SetLength(buf, buflen + datalen);
    MoveFast(data, PByteArray(buf)^[buflen], datalen);
  end;
end;

function TPollAsyncSockets.Write(connection: TObject; const data;
  datalen, timeout: integer): boolean;
var
  tag: TPollSocketTag;
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
  LockedInc32(@fProcessing);
  try
    tag := TPollSocketTag(connection);
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    if slot.TryLock(true, timeout) then // try and wait for another ProcessWrite
    try
      P := @data;
      previous := length(slot.writebuf);
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
            exit;  // connection closed or broken -> abort
          inc(fWriteCount);
          inc(fWriteBytes, sent);
          dec(datalen, sent);
          if datalen = 0 then
          begin
            try
              // notify everything written
              AfterWrite(connection);
              result := true;
            except
              result := false;
            end;
            exit;
          end;
          inc(P, sent);
        until false;
        // use fWrite output polling for the remaining data in ProcessWrite
      AppendData(slot.writebuf, P^, datalen);
      if previous > 0 then // already subscribed
        result := slot.socket <> nil
      else if fWrite.Subscribe(slot.socket, [pseWrite], tag) then
        result := slot.socket <> nil
      else
        slot.writebuf := ''; // subscription error -> abort
    finally
      slot.UnLock({writer=}true);
    end;
  finally
    LockedDec32(@fProcessing);
  end;
end;

procedure TPollAsyncSockets.ProcessRead(timeoutMS: integer);
var
  notif: TPollSocketResult;
  connection: TObject;
  slot: PPollSocketsSlot;
  recved, added: integer;
  res: TNetResult;
  temp: array[0..$7fff] of byte; // read up to 32KB per chunk

  procedure CloseConnection(withinreadlock: boolean);
  begin
    if withinreadlock then
      slot.UnLock({writer=}false); // Stop() will try to acquire this lock
    Stop(connection); // shutdown and set socket:=0 + acquire locks
    try
      OnClose(connection); // now safe to perform connection.Free
    except
      connection := nil;   // user code may be unstable
    end;
    slot := nil; // ignore pseClosed and slot.Unlock(false)
  end;

begin
  if (self = nil) or
     fRead.Terminated then
    exit;
  LockedInc32(@fProcessing);
  try
    if not fRead.GetOne(timeoutMS, notif) then
      exit;
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    if pseError in notif.events then
      if not OnError(connection, notif.events) then
      begin
        // false = shutdown
        CloseConnection({withinlock=}false);
        exit;
      end;
    if pseRead in notif.events then
    begin
      if slot.Lock({writer=}false) then // paranoid thread-safe read
      try
        added := 0;
        repeat
          if fRead.Terminated or
             (slot.socket = nil) then
            exit;
          recved := SizeOf(temp);
          res := slot.socket.Recv(@temp, recved);
          if slot.socket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break; // may block, try later
          if res <> nrOk then
          begin
            CloseConnection(true);
            exit; // socket closed gracefully or unrecoverable error -> abort
          end;
          AppendData(slot.readbuf, temp, recved);
          inc(added, recved);
        until false;
        if added > 0 then
        try
          inc(fReadCount);
          inc(fReadBytes, added);
          if OnRead(connection) = sorClose then
            CloseConnection(true);
        except
          CloseConnection(true); // force socket shutdown
        end;
      finally
        slot.UnLock(false); // CloseConnection may set slot=nil
      end;
    end;
    if (slot <> nil) and
       (slot.socket <> nil) and
       (pseClosed in notif.events) then
    begin
      CloseConnection(false);
      exit;
    end;
  finally
    LockedDec32(@fProcessing);
  end;
end;

procedure TPollAsyncSockets.ProcessWrite(timeoutMS: integer);
var
  notif: TPollSocketResult;
  connection: TObject;
  slot: PPollSocketsSlot;
  buf: PByte;
  buflen, bufsent, sent: integer;
  res: TNetResult;
begin
  if (self = nil) or
     fWrite.Terminated then
    exit;
  LockedInc32(@fProcessing);
  try
    if not fWrite.GetOne(timeoutMS, notif) then
      exit;
    if notif.events <> [pseWrite] then
      exit; // only try if we are sure the socket is writable and safe
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot = nil) or
       (slot.socket = nil) then
      exit;
    if slot.Lock({writer=}true) then // paranoid check
    try
      buflen := length(slot.writebuf);
      if buflen <> 0 then
      begin
        buf := pointer(slot.writebuf);
        sent := 0;
        repeat
          if fWrite.Terminated or
             (slot.socket = nil) then
            exit;
          bufsent := buflen;
          res := slot.socket.Send(buf, bufsent);
          if slot.socket = nil then
            exit; // Stop() called
          if res = nrRetry then
            break; // may block, try later
          if res <> nrOk then
            exit; // socket closed gracefully or unrecoverable error -> abort
          inc(fWriteCount);
          inc(sent, bufsent);
          inc(buf, bufsent);
          dec(buflen, bufsent);
        until buflen = 0;
        inc(fWriteBytes, sent);
        delete(slot.writebuf, 1, sent);
      end;
      if slot.writebuf = '' then
      begin
        // no data any more to be sent
        fWrite.Unsubscribe(slot.socket, notif.tag);
        try
          AfterWrite(connection);
        except
        end;
      end;
    finally
      slot.UnLock(true);
    end;
  finally
    LockedDec32(@fProcessing);
  end;
end;


{ ******************** Client or Server Asynchronous Process }

{ TAsyncConnection }

constructor TAsyncConnection.Create(const aRemoteIP: RawUtf8);
begin
  inherited Create;
  fRemoteIP := aRemoteIP;
end;

procedure TAsyncConnection.AfterCreate(Sender: TAsyncConnections);
begin
  fLastOperation := UnixTimeUtc;
end;

procedure TAsyncConnection.OnLastOperationIdle(Sender: TAsyncConnections);
begin
  fLastOperation := UnixTimeUtc;
end;

procedure TAsyncConnection.AfterWrite(Sender: TAsyncConnections);
begin
  // do nothing
end;

procedure TAsyncConnection.BeforeDestroy(Sender: TAsyncConnections);
begin
  fHandle := 0; // to detect any dangling pointer
end;


{ TAsyncConnectionsSockets }

procedure TAsyncConnectionsSockets.OnClose(connection: TObject);
begin
  // caller did call Stop() before calling OnClose (socket=0)
  fOwner.fLog.Add.Log(sllTrace, 'OnClose%', [connection], self);
  fOwner.ConnectionDelete((connection as TAsyncConnection).Handle); // do connection.Free
end;

function TAsyncConnectionsSockets.OnError(connection: TObject; events:
  TPollSocketEvents): boolean;
begin
  fOwner.fLog.Add.Log(sllDebug,
    'OnError% events=[%] -> free socket and instance', [connection,
    GetSetName(TypeInfo(TPollSocketEvents), events)], self);
  result := acoOnErrorContinue in fOwner.Options; // false=close by default
end;

function TAsyncConnectionsSockets.OnRead(connection: TObject): TPollAsyncSocketOnRead;
var
  ac: TAsyncConnection;
begin
  ac := connection as TAsyncConnection;
  if not (acoNoLogRead in fOwner.Options) then
    fOwner.fLog.Add.Log(sllTrace, 'OnRead% len=%', [ac, length(ac.fSlot.readbuf)], self);
  result := ac.OnRead(fOwner);
  if not (acoLastOperationNoRead in fOwner.Options) then
    ac.fLastOperation := UnixTimeUtc;
end;

function TAsyncConnectionsSockets.SlotFromConnection(connection: TObject):
  PPollSocketsSlot;
begin
  try
    if (connection = nil) or
       not connection.InheritsFrom(TAsyncConnection) or
       (TAsyncConnection(connection).Handle = 0) then
    begin
      fOwner.fLog.Add.Log(sllStackTrace,
        'SlotFromConnection() with dangling pointer %', [connection], self);
      result := nil;
    end
    else
      result := @TAsyncConnection(connection).fSlot;
  except
    fOwner.fLog.Add.Log(sllError, 'SlotFromConnection() with dangling pointer %',
      [pointer(connection)], self);
    result := nil;
  end;
end;

function TAsyncConnectionsSockets.Write(connection: TObject; const data;
  datalen, timeout: integer): boolean;
var
  tmp: TLogEscape;
begin
  result := inherited Write(connection, data, datalen, timeout);
  if result and
     not (acoLastOperationNoWrite in fOwner.Options) then
    (connection as TAsyncConnection).fLastOperation := UnixTimeUtc;
  if (fOwner.fLog <> nil) and
     not (acoNoLogWrite in fOwner.Options) then
    fOwner.fLog.Add.Log(sllTrace, 'Write%=% len=%%', [connection,
      BOOL_STR[result], datalen, LogEscape(@data, datalen, tmp{%H-},
      acoVerboseLog in fOwner.Options)], self);
end;

procedure TAsyncConnectionsSockets.AfterWrite(connection: TObject);
begin
  (connection as TAsyncConnection).AfterWrite(fOwner);
end;

function TAsyncConnectionsSockets.GetTotal: integer;
begin
  result := fOwner.fLastHandle; // by definition
end;


{ TAsyncConnectionsThread }

constructor TAsyncConnectionsThread.Create(aOwner: TAsyncConnections; aProcess:
  TPollSocketEvent);
begin
  fOwner := aOwner;
  fProcess := aProcess;
  fOnThreadTerminate := fOwner.fOnThreadTerminate;
  inherited Create(false);
end;

procedure TAsyncConnectionsThread.Execute;
var
  idletix: Int64;
begin
  SetCurrentThreadName('% % %', [fOwner.fProcessName, self, ToText(fProcess)^]);
  fOwner.NotifyThreadStart(self);
  try
    idletix := mormot.core.os.GetTickCount64 + 1000;
    while not Terminated and
          (fOwner.fClients <> nil) do
    begin
      // implement parallel client connections for TAsyncClient
      if (fOwner.fThreadClients.Count > 0) and
         (InterlockedDecrement(fOwner.fThreadClients.Count) >= 0) then
        fOwner.ThreadClientsConnect
      else      // generic TAsyncConnections read/write process
        case fProcess of
          pseRead:
            fOwner.fClients.ProcessRead(30000);
          pseWrite:
            begin
              fOwner.fClients.ProcessWrite(30000);
              if mormot.core.os.GetTickCount64 >= idletix then
              begin
                fOwner.IdleEverySecond; // may take some time -> retrieve ticks again
                idletix := mormot.core.os.GetTickCount64 + 1000;
              end;
            end;
        else
          raise EAsyncConnections.CreateUtf8('%.Execute: unexpected fProcess=%',
            [self, ToText(fProcess)^]);
        end;
    end;
  except
    on E: Exception do
      fOwner.fLog.Add.Log(sllWarning, 'Execute raised a % -> terminate % thread',
        [E.ClassType, fOwner.fStreamClass], self);
  end;
  fOwner.fLog.Add.Log(sllDebug, 'Execute: done', self);
end;


{ TAsyncConnections }

function TAsyncConnectionCompareByHandle(const A, B): integer;
begin
  // for fast binary search from the connection handle
  result := TAsyncConnection(A).Handle - TAsyncConnection(B).Handle;
end;

constructor TAsyncConnections.Create(const OnStart, OnStop: TOnNotifyThread;
  aStreamClass: TAsyncConnectionClass; const ProcessName: RawUtf8;
  aLog: TSynLogClass; aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
var
  i: PtrInt;
  log: ISynLog;
begin
  log := aLog.Enter('Create(%,%,%)', [aStreamClass, ProcessName, aThreadPoolCount], self);
  if (aStreamClass = TAsyncConnection) or
     (aStreamClass = nil) then
    raise EAsyncConnections.CreateUtf8('%.Create(%)', [self, aStreamClass]);
  if aThreadPoolCount <= 0 then
    aThreadPoolCount := 1;
  fLog := aLog;
  fStreamClass := aStreamClass;
  fConnectionLock.Init;
  fConnections.Init(TypeInfo(TPointerDynArray), fConnection, @fConnectionCount);
  // don't use TAsyncConnectionObjArray to manually call TAsyncConnection.BeforeDestroy
  fConnections.Compare := TAsyncConnectionCompareByHandle;
  fClients := TAsyncConnectionsSockets.Create;
  fClients.fOwner := self;
  fTempConnectionForSearchPerHandle := fStreamClass.Create('');
  fOptions := aOptions;
  inherited Create(false, OnStart, OnStop, ProcessName);
  SetLength(fThreads, aThreadPoolCount + 1);
  fThreads[0] := TAsyncConnectionsThread.Create(self, pseWrite);
  for i := 1 to aThreadPoolCount do
    fThreads[i] := TAsyncConnectionsThread.Create(self, pseRead);
end;

destructor TAsyncConnections.Destroy;
var
  i: PtrInt;
begin
  if fClients <> nil then
    with fClients do
      fLog.Add.Log(sllDebug, 'Destroy total=% reads=%/% writes=%/%',
        [Total, ReadCount, KB(ReadBytes), WriteCount, KB(WriteBytes)], self);
  Terminate;
  for i := 0 to high(fThreads) do
    fThreads[i].Terminate; // stop ProcessRead/ProcessWrite when polling stops
  FreeAndNil(fClients); // stop polling and refuse further Write/ConnectionRemove
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
  with fThreadClients do
    res := NewSocket(Address, Port, nlTCP, {bind=}false, timeout, timeout,
      timeout, {retry=}0, client);
  if res <> nrOK then
    raise EAsyncConnections.CreateUtf8('%: %:% connection failure (%)',
      [self, fThreadClients.Address, fThreadClients.Port, ToText(res)^]);
  connection := nil;
  if not ConnectionCreate(client, {ip=}'', connection) then
    client.ShutdownAndClose({rdwr=}false);
end;

function TAsyncConnections.ConnectionCreate(aSocket: TNetSocket;
  const aRemoteIp: RawUtf8; out aConnection: TAsyncConnection): boolean;
begin
  // you can override this class then call ConnectionAdd
  if Terminated then
    result := false
  else
  begin
    aConnection := fStreamClass.Create(aRemoteIp);
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
    aConnection.fHandle := fLastHandle;
    fConnections.Add(aConnection);
    fLog.Add.Log(sllTrace, 'ConnectionAdd% count=%',
      [aConnection, fConnectionCount], self);
    fConnections.Sorted := true; // handles are increasing
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
    h := aConnection.Handle;
    t := aConnection.ClassType;
    aConnection.BeforeDestroy(self);
    aConnection.Free;
  finally
    fConnections.FastDeleteSorted(aIndex);
  end;
  fLog.Add.Log(sllTrace, 'ConnectionDelete %.Handle=% count=%',
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
    fLog.Add.Log(sllTrace, 'ConnectionDelete(%)=false count=%',
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
    fTempConnectionForSearchPerHandle.fHandle := aHandle;
    // fast O(log(n)) binary search
    i := fConnections.Find(fTempConnectionForSearchPerHandle);
    if i >= 0 then
    begin
      result := fConnection[i];
      if aIndex <> nil then
        aIndex^ := i;
    end;
    fLog.Add.Log(sllTrace, 'ConnectionFindLocked(%)=%', [aHandle, result], self);
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
      fLog.Add.Log(sllDebug, 'ConnectionRemove: Stop=false for %', [conn], self);
    result := ConnectionDelete(conn, i);
  finally
    fConnectionLock.UnLock;
  end;
  if not result then
    fLog.Add.Log(sllTrace, 'ConnectionRemove(%)=false', [aHandle], self);
end;

procedure TAsyncConnections.lock;
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
    fLog.Add.Log(sllTrace, '% len=%%', [ident, framelen, LogEscape(frame,
      framelen, tmp{%H-})], connection);
end;

procedure TAsyncConnections.LogVerbose(connection: TAsyncConnection;
  const ident: RawUtf8; const frame: RawByteString);
begin
  LogVerbose(connection, ident, pointer(frame), length(frame));
end;

procedure TAsyncConnections.IdleEverySecond;
var
  i, n: integer;
  allowed: cardinal;
  log: ISynLog;
begin
  if Terminated or
     (LastOperationIdleSeconds <= 0) then
    exit;
  fConnectionLock.Lock;
  try
    n := 0;
    allowed := UnixTimeUtc - LastOperationIdleSeconds;
    for i := 0 to fConnectionCount - 1 do
      if fConnection[i].fLastOperation < allowed then
      try
        if {%H-}log = nil then
          log := fLog.Enter(self, 'IdleEverySecond');
        fConnection[i].OnLastOperationIdle(self);
        inc(n);
      except
      end;
    if log <> nil then
      log.Log(sllTrace, 'IdleEverySecond notified % %', [n, fStreamClass], self);
  finally
    fConnectionLock.UnLock;
  end;
end;


{ TAsyncServer }

constructor TAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; aStreamClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fServer := TCrtSocket.Bind(aPort);
  inherited Create(OnStart, OnStop, aStreamClass, ProcessName, aLog,
    aOptions, aThreadPoolCount);
end;

destructor TAsyncServer.Destroy;
var
  endtix: Int64;
  touchandgo: TNetSocket; // paranoid ensure Accept() is released
begin
  Terminate;
  if fServer <> nil then
  begin
    fServer.Close; // shutdown the socket to unlock Accept() in Execute
    if NewSocket('127.0.0.1', fServer.Port, nlTCP, false, 1000, 0, 0, 0, touchandgo) = nrOk then
      touchandgo.ShutdownAndClose(false);
  end;
  endtix := mormot.core.os.GetTickCount64 + 10000;
  inherited Destroy;
  while not fExecuteFinished and
        (mormot.core.os.GetTickCount64 < endtix) do
    SleepHiRes(1); // wait for Execute to be finalized (unlikely)
  fServer.Free;
end;

procedure TAsyncServer.Execute;
var
  client: TNetSocket;
  connection: TAsyncConnection;
  res: TNetResult;
  sin: TNetAddr;
  ip: RawUtf8;
begin
  SetCurrentThreadName('% % Accept', [fProcessName, self]);
  NotifyThreadStart(self);
  if fServer.Sock <> nil then
  try
    while not Terminated do
    begin
      res := fServer.Sock.Accept(client, sin);
      if res <> nrOk then
        if Terminated then
          break
        else
        begin
          fLog.Add.Log(sllWarning, 'Execute: Accept()=%', [ToText(res)^], self);
          raise EAsyncConnections.CreateUtf8('%.Execute: Accept failed as %',
            [self, ToText(res)^]);
          SleepHiRes(1);
          continue;
        end;
      if Terminated then
      begin
        client.ShutdownAndClose({rdwr=}false);
        break;
      end;
      ip := sin.IP;
      if ConnectionCreate(client, ip, connection) then
        if fClients.Start(connection) then
          fLog.Add.Log(sllTrace, 'Execute: Accept()=%', [connection], self)
        else
          connection.Free
      else
        client.ShutdownAndClose({rdwr=}false);
    end;
  except
    on E: Exception do
      fLog.Add.Log(sllWarning, 'Execute raised % -> terminate %',
        [E.ClassType, fProcessName], self);
  end;
  fLog.Add.Log(sllDebug, 'Execute: % done', [fProcessName], self);
  fExecuteFinished := true;
end;



{ TAsyncClient }

constructor TAsyncClient.Create(const aServer, aPort: RawUtf8;
  aClientsCount, aClientsTimeoutSecs: integer;
  const OnStart, OnStop: TOnNotifyThread; aStreamClass: TAsyncConnectionClass;
  const ProcessName: RawUtf8; aLog: TSynLogClass;
  aOptions: TAsyncConnectionsOptions; aThreadPoolCount: integer);
begin
  fThreadClients.Count := aClientsCount;
  fThreadClients.Timeout := aClientsTimeoutSecs * 1000;
  fThreadClients.Address := aServer;
  fThreadClients.Port := aPort;
  inherited Create(OnStart, OnStop, aStreamClass, ProcessName, aLog, aOptions, aThreadPoolCount);
end;

procedure TAsyncClient.Execute;
begin
  SetCurrentThreadName('% %', [fProcessName, self]);
  NotifyThreadStart(self);
  try
    while InterlockedDecrement(fThreadClients.Count) >= 0 do
      ThreadClientsConnect; // will connect some clients in this main thread
  except
    on E: Exception do
      fLog.Add.Log(sllWarning, 'Execute raised % -> terminate %', [E.ClassType,
        fProcessName], self);
  end;
  fLog.Add.Log(sllDebug, 'Execute: % done', [fProcessName], self);
end;


end.

