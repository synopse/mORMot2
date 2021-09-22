/// Event-Driven WebSockets Server-Side Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ws.async;

{
  *****************************************************************************

    Asynchronous WebSockets Bidirectional Server
    - TWebSocketAsyncServer Event-Driven HTTP/WebSockets Server
    - TWebSocketAsyncServerRest Bidirectional REST Server

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.text,
  mormot.core.data,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.core.perf,
  mormot.crypt.core,
  mormot.crypt.ecc,
  mormot.crypt.secure, // IProtocol definition
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async,
  mormot.net.ws.core;


{ ******************** TWebSocketAsyncServer Event-Driven HTTP/WebSockets Server }

type
  TWebSocketAsyncConnection = class;

  /// WebSockets process based on non-blocking TWebSocketAsyncConnection
  TWebSocketAsyncProcess = class(TWebSocketProcess)
  protected
    fConnection: TWebSocketAsyncConnection;
    fProcessPos: PtrInt;   // index in fConnection.fHttp.Process.Buffer/Len
    fReadPos: PtrInt;      // index in fConnection.fSlot.rd.Buffer/Len
    fOnRead: TWebProcessInFrame;
    fOnReadFrame: TWebSocketFrame;
    function OnRead: TPollAsyncSocketOnReadWrite;
    function ComputeContext(
      out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract; override;
  public
    /// initialize the WebSockets process on a given TCrtSocket connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aConnection: TWebSocketAsyncConnection;
                       aProtocol: TWebSocketProtocol); reintroduce;
    /// first step of the low level incoming WebSockets framing protocol over TCrtSocket
    // - in practice, just call fSocket.SockInPending to check for pending data
    function CanGetFrame(TimeOut: cardinal;
                         ErrorWithoutException: PInteger): boolean; override;
    /// low level receive incoming WebSockets frame data over TCrtSocket
    // - in practice, just call fSocket.SockInRead to check for pending data
    function ReceiveBytes(P: PAnsiChar; count: PtrInt): integer; override;
    /// low level receive incoming WebSockets frame data over TCrtSocket
    // - in practice, just call fSocket.TrySndLow to send pending data
    function SendBytes(P: pointer; Len: PtrInt): boolean; override;
    /// delayed process of outgoing WebSockets framing protocol
    // - will notify TWebSocketAsyncConnections.ProcessIdleTix sending
    procedure SendFrameAsync(const Frame: TWebSocketFrame); override;
  end;

  /// one HTTP/WebSockets server connection using non-blocking sockets
  TWebSocketAsyncConnection = class(THttpAsyncConnection)
  protected
    fProcess: TWebSocketAsyncProcess; // set once upgraded
    fNextPingSec: TAsyncConnectionSec;
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    function AfterWrite: TPollAsyncSocketOnReadWrite; override;
    procedure OnClose; override;
    function DecodeHeaders: integer; override;
    procedure BeforeDestroy; override;
    // called every 10 seconds
    function OnLastOperationIdle(nowsec: TAsyncConnectionSec): boolean; override;
    // used e.g. by TWebSocketAsyncServer.WebSocketBroadcast
    function SendDirect(const tmp: TSynTempBuffer;
      opcode: TWebSocketFrameOpCode): boolean;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// handle HTTP/WebSockets server connections using non-blocking sockets
  TWebSocketAsyncConnections = class(THttpAsyncConnections)
  protected
    // maintain a thread-safe list to minimize ProcessIdleTix time
    fOutgoingLock: TRTLCriticalSection;
    fOutgoingCount: integer;
    fOutgoing: array of TWebSocketAsyncConnection;
    procedure NotifyOutgoing(Connection: TWebSocketAsyncConnection);
    procedure ProcessIdleTix(Sender: TObject; NowTix: Int64); override;
  public
    /// finalize the HTTP/WebSockets Connections
    destructor Destroy; override;
  end;

  /// HTTP/WebSockets server using non-blocking sockets
  TWebSocketAsyncServer = class(THttpAsyncServer)
  protected
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
  public
    /// create an event-driven HTTP/WebSockets Server
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; aHeadersUnFiltered: boolean = false;
      CreateSuspended: boolean = false; aLogVerbose: boolean = false); override;
    /// finalize the HTTP/WebSockets Server
    destructor Destroy; override;
    /// allow to customize the WebSockets processing
    // - apply to all protocols on this server instance
    // - those parameters are accessed by reference from existing connections,
    // so you should better not modify them once the server started
    function Settings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
    /// will send a given frame to clients matching the supplied connection IDs
    // - expect aFrame.opcode to be either focText or focBinary
    // - WebSocketBroadcast(nil) will broadcast to all running websockets
    // - returns the number of sent frames
    // - warning: the raw frame will be directly sent with no encoding (i.e.
    // no encryption nor compression) so is to be used with raw protocols
    // (e.g. to efficiently notify AJAX browsers)
    function WebSocketBroadcast(const aFrame: TWebSocketFrame;
      const aClientsConnectionID: THttpServerConnectionIDDynArray): integer;
    /// access to the protocol list handled by this server
    property WebSocketProtocols: TWebSocketProtocolList
      read fProtocols;
  end;


{ ******************** TWebSocketAsyncServerRest Bidirectional REST Server }

type
  /// main HTTP/WebSockets server Thread using non-blocking sockets
  TWebSocketAsyncServerRest = class(TWebSocketAsyncServer)
  public
    /// create a Server instance, binded and listening on a port, with our
    // 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - aWebSocketsEncryptionKey format follows TWebSocketProtocol.SetEncryptKey,
    // so could be e.g. 'password#xxxxxx.private' or 'a=mutual;e=aesctc128;p=34a2..'
    // to use TEcdheProtocol, or a plain password for TProtocolAes
    // - if aWebSocketsAjax is TRUE, it will also register TWebSocketProtocolJson
    // so that AJAX applications would be able to connect to this server
    // - warning: WaitStarted should be called after Create() to check for
    // for actual port binding in the background thread
    constructor Create(const aPort: RawUtf8; const OnStart, OnStop: TOnNotifyThread;
      const aProcessName: RawUtf8; ServerThreadPoolCount: integer;
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax, aLogVerbose: boolean); reintroduce; overload;
    /// defines the WebSockets protocols to be used for this Server
    // - i.e. 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - aWebSocketsEncryptionKey format follows TWebSocketProtocol.SetEncryptKey
    // - if aWebSocketsAjax is TRUE, it will also register TWebSocketProtocolJson
    // so that AJAX applications would be able to connect to this server
    procedure WebSocketsEnable(
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions = [pboSynLzCompress]);
    /// server can send a request back to the client, when the connection has
    // been upgraded to WebSocket
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return HTTP_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal;
      override;
  end;



implementation


{ ******************** TWebSocketAsyncServer Event-Driven HTTP/WebSockets Server }

{ TWebSocketAsyncConnection }

function TWebSocketAsyncConnection.OnRead: TPollAsyncSocketOnReadWrite;
begin
  if fProcess = nil then
  begin
    result := inherited OnRead; // regular HTTP requests, including upgrade
    if (result <> soContinue) or
       (fProcess = nil) or
       (fHttp.Process.Len = 0) then
      exit;
  end;
  // process fSlot.rd incoming bytes into the current WebSockets protocol
  result := fProcess.OnRead;
end;

function TWebSocketAsyncConnection.AfterWrite: TPollAsyncSocketOnReadWrite;
begin
  if fHttp.State <> hrsUpgraded then
    result := inherited AfterWrite
  else
    result := soContinue; // frames are always provided as a single buffer
end;

function TWebSocketAsyncConnection.OnLastOperationIdle(
  nowsec: TAsyncConnectionSec): boolean;
var
  delay: TAsyncConnectionSec; // HeartbeatDelay may be changed on the fly
begin
  // this code is not blocking and very quick most of the time
  result := false;
  delay := TWebSocketAsyncServer(fServer).fSettings.HeartbeatDelay shr 10;
  if TAsyncConnectionSec(nowsec - fLastOperation) < delay then
    exit;
  fProcess.SendPing; // Write will change fLastOperation
  result := true;
end;

function TWebSocketAsyncConnection.DecodeHeaders: integer;
var
  proto: TWebSocketProtocol;
  resp: RawUtf8;
begin
  result := inherited DecodeHeaders; // e.g. HTTP_TIMEOUT or OnBeforeBody()
  if (result = HTTP_SUCCESS) and
     (fHttp.Upgrade <> '') and
     (hfConnectionUpgrade in fHttp.HeaderFlags) then
  begin
    // try to upgrade to one of the registered WebSockets protocol
    result := (fServer as TWebSocketAsyncServer).fProtocols.
      ServerUpgrade(fHttp, fRemoteIP, fHandle, proto, resp);
    if result = HTTP_SUCCESS then
    begin
      fHttp.State := hrsUpgraded;
      if fOwner.Write(self, resp) then
      begin
        // if we reached here, we switched/upgraded to WebSockets bidir frames
        fProcess := TWebSocketAsyncProcess.Create(self, proto);
        TWebSocketAsyncServer(fServer).IncStat(grUpgraded);
        fProcess.ProcessStart; // OnClientConnected + focContinuation event
        fProcess.fState := wpsRun;
      end
      else
        raise EWebSockets.CreateUtf8('%.DecodeHeaders: upgrade failed', [self]);
    end;
  end;
end;

procedure TWebSocketAsyncConnection.OnClose;
begin
  // inherited OnClose; // do nothing
  if fProcess = nil then
    exit;
  fProcess.Shutdown({waitforpong=}true); // send focConnectionClose
  if not fProcess.fProcessEnded then
    fProcess.ProcessStop; // there is no separated thread loop to wait for
end;

procedure TWebSocketAsyncConnection.BeforeDestroy;
begin
  if fProcess <> nil then
  begin
    fProcess.fConnectionCloseWasSent := true; // too late for focConnectionClose
    if not fProcess.fProcessEnded then
      fProcess.ProcessStop; // there is no separated thread loop to wait for
  end;
  inherited BeforeDestroy;
  FreeAndNilSafe(fProcess);
end;

function TWebSocketAsyncConnection.SendDirect(const tmp: TSynTempBuffer;
  opcode: TWebSocketFrameOpCode): boolean;
begin
  if self = nil then
    result := false
  else
  begin
    result := fOwner.Write(self, tmp.buf^, tmp.len, {timeout=}0);
    if result and
       (opcode = focConnectionClose) then
      fProcess.fConnectionCloseWasSent := true;
  end;
end;


{ TWebSocketAsyncConnections }

procedure TWebSocketAsyncConnections.NotifyOutgoing(
  Connection: TWebSocketAsyncConnection);
begin
  EnterCriticalSection(fOutgoingLock);
  ObjArrayAddOnce(fOutgoing, Connection, fOutgoingCount);
  LeaveCriticalSection(fOutgoingLock);
end;

procedure TWebSocketAsyncConnections.ProcessIdleTix(Sender: TObject;
  NowTix: Int64);
var
  i, conn, valid, sent, invalid: PtrInt;
  timer: TPrecisionTimer;
begin
  if Terminated then
    exit;
  // inlined TAsyncConnections.ProcessIdleTix logic
  if NowTix >= fIdleTix then
  begin
    IdleEverySecond(NowTix);
    fIdleTix := NowTix + 1000;
  end;
  // send pending outgoing frames, with optional JumboFrame gathering
  if fOutgoingCount = 0 then
    exit;
  timer.Start; // we monitor this loop, which should not be blocking
  valid := 0;
  invalid := 0;
  EnterCriticalSection(fOutgoingLock);
  try
    conn := fOutgoingCount;
    for i := 0 to conn - 1 do
    begin
      sent := fOutgoing[i].fProcess.SendPendingOutgoingFrames;
      if sent < 0 then
        inc(invalid)
      else
        inc(valid, sent);
    end;
    fOutgoingCount := 0;
  finally
    LeaveCriticalSection(fOutgoingLock);
  end;
  timer.Pause; // BeforeSendFrame encrypt/compress may have taken some time
  if (invalid <> 0) or
     (timer.TimeInMicroSec > 500) then // 0.5 ms seems responsive enough
    DoLog(sllTrace, 'ProcessIdleTix conn=% valid=% invalid=% in %',
      [conn, valid, invalid, timer.Time], self);
end;

destructor TWebSocketAsyncConnections.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(fOutgoingLock);
end;


{ TWebSocketAsyncProcess }

constructor TWebSocketAsyncProcess.Create(aConnection: TWebSocketAsyncConnection;
  aProtocol: TWebSocketProtocol);
var
  serv: TWebSocketAsyncServer;
begin
  serv := aConnection.fServer as TWebSocketAsyncServer;
  fNoLastSocketTicks := true; // aConnection.OnLastOperationIdle handles pings
  inherited Create(
    aProtocol, aConnection.Handle, nil, @serv.fSettings, serv.ProcessName);
  fConnection := aConnection;
  fOnRead.Init(self, @fOnReadFrame);
end;

function TWebSocketAsyncProcess.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
begin
  result := THttpServerRequest.Create(
    fConnection.fServer, fOwnerConnectionID, nil, fProtocol.ConnectionFlags);
  RequestProcess :=  fConnection.fServer.Request;
end;

function TWebSocketAsyncProcess.CanGetFrame(TimeOut: cardinal;
  ErrorWithoutException: PInteger): boolean;
begin
  // first read from fHttp.Process, then fSlot.rd
  if (fConnection.Socket = nil) or
     fConnection.fSlot.closing then
  begin
    result := false;
    if ErrorWithoutException <> nil then
      ErrorWithoutException^ := -2;
  end
  else
  begin
    // TimeOut is ignored with our non-blocking sockets
    result := ((fConnection.fHttp.Process.Len - fProcessPos) +
               (fConnection.fSlot.rd.Len - fReadPos)) <> 0;
    if ErrorWithoutException <> nil then
      ErrorWithoutException^ := 0; // no error
  end;
end;

function TWebSocketAsyncProcess.ReceiveBytes(P: PAnsiChar; count: PtrInt): integer;
begin
  // first read from fHttp.Process / fProcessPos (remaining from previous read)
  result := fConnection.fHttp.Process.ExtractAt(P, count, fProcessPos);
  if count <> 0 then
    // try if we can get some more directly from fSlot.rd / fReadPos
    inc(result, fConnection.fSlot.rd.ExtractAt(P, count, fReadPos));
end;

function TWebSocketAsyncProcess.OnRead: TPollAsyncSocketOnReadWrite;
var
  processed: boolean;
  len: PtrInt;
begin
  if fState <> wpsRun then
    result := soClose
  else
    try
      result := soContinue;
      LockedInc32(@fProcessCount); // flag currently processing
      try
        // asynchronous ProcessLoopStepReceive() logic
        // - ProcessLoopStepReceive/GetFrame can't be resumed so they fail
        // when OS read buffers are full (which is common on Windows)
        repeat
          processed := false;
          if fState = wpsRun then
            if CanGetFrame({timeout=}0, nil) then
              if fOnRead.Step(nil) = pfsDone then
              begin
                // we received a full frame
                processed := true;
                ProcessLoopReceived(fOnReadFrame); // SendFrame() if needed
                fOnRead.data := '';
                fOnRead.Init(self, @fOnReadFrame); // reset for next frame
              end;
          if (fOwnerThread <> nil) and
             fOwnerThread.Terminated then
            fState := wpsClose;
          if fState <> wpsRun then
          begin
            result := soClose;
            break;
          end;
        until not processed;
        // manage remainging data in Process+rd buffers
        if result = soContinue then
        begin
          if fProcessPos <> 0 then
          begin
            fConnection.fHttp.Process.Remove(fProcessPos); // remove processed
            fProcessPos := 0;
          end;
          len := fConnection.fSlot.rd.Len - fReadPos;
          if len <> 0 then
          begin
            fConnection.fHttp.Process.Append(
              PAnsiChar(fConnection.fSlot.rd.Buffer) + fReadPos, len);
            fConnection.fSlot.rd.Reset; // fSlot.rd was moved to fHttp.Process
            fReadPos := 0;
          end;
        end;
      finally
        LockedDec32(@fProcessCount); // release flag
      end;
    except
      result := soClose;
    end;
  if result = soClose then
    ProcessStop; // OnClientDisconnected - called in read thread pool
end;


function TWebSocketAsyncProcess.SendBytes(P: pointer; Len: PtrInt): boolean;
begin
  // try to send all in non-blocking mode, or subscribe for biggest writes
  result := fConnection.Owner.Write(fConnection, P^, Len);
end;

procedure TWebSocketAsyncProcess.SendFrameAsync(const Frame: TWebSocketFrame);
begin
  inherited SendFrameAsync(Frame); // calls Outgoing.Push()
  TWebSocketAsyncConnections(fConnection.fOwner).NotifyOutgoing(fConnection);
end;


{ TWebSocketAsyncServer }

constructor TWebSocketAsyncServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  aHeadersUnFiltered: boolean; CreateSuspended: boolean; aLogVerbose: boolean);
begin
  // initialize protocols and connections
  fConnectionClass := TWebSocketAsyncConnection;
  fConnectionsClass := TWebSocketAsyncConnections;
  fCanNotifyCallback := true;
  fProtocols := TWebSocketProtocolList.Create;
  fSettings.SetDefaults;
  fSettings.HeartbeatDelay := 20000;
  if aLogVerbose then
    fSettings.SetFullLog;
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, aHeadersUnFiltered, CreateSuspended, aLogVerbose);
  fAsync.LastOperationIdleSeconds := 10; // 10 secs is good enough for ping/pong
  InitializeCriticalSection((fAsync as TWebSocketAsyncConnections).fOutgoingLock);
end;

destructor TWebSocketAsyncServer.Destroy;
var
  closing: TWebSocketFrame;
  log: ISynLog;
begin
  log := TSynLog.Enter(self, 'Destroy');
  // no more incoming request
  Shutdown;
  // notify at once all client connections - don't wait for answer
  closing.opcode := focConnectionClose;
  closing.content := [];
  closing.tix := 0;
  WebSocketBroadcast(closing, nil);
  log.Log(sllTrace, 'Destroy: WebSocketBroadcast(focConnectionClose) done', self);
  // close any pending connection
  inherited Destroy;
  log.Log(sllTrace, 'Destroy: inherited THttpAsyncServer done', self);
  // release internal protocols list
  fProtocols.Free;
end;

function TWebSocketAsyncServer.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketAsyncServer.WebSocketBroadcast(const aFrame: TWebSocketFrame;
  const aClientsConnectionID: THttpServerConnectionIDDynArray): integer;
var
  i: PtrInt;
  tmp: TSynTempBuffer;
begin
  result := 0;
  if Terminated or
     (fAsync = nil) or
     not (aFrame.opcode in [focText, focBinary, focConnectionClose]) then
    exit;
  FrameSendEncode(aFrame, {mask=}0, tmp);
  fAsync.Lock;
  try
    // use TWebSocketAsyncConnection.SendDirect for non-blocking socket sending
    if aClientsConnectionID = nil then
      // broadcast to all connected clients
      for i := 0 to fAsync.ConnectionCount - 1 do
        inc(result, ord(TWebSocketAsyncConnection(fAsync.Connection[i]).
           SendDirect(tmp, aFrame.opcode)))
    else
      // broadcast to some specified connected clients
      for i := 0 to length(aClientsConnectionID) - 1 do
        inc(result, ord(TWebSocketAsyncConnection(
          fAsync.ConnectionSearch(aClientsConnectionID[i])). // O(log(n)) search
            SendDirect(tmp, aFrame.opcode)));
  finally
    fAsync.UnLock;
    tmp.Done;
  end;
end;


{ ******************** TWebSocketAsyncServerRest Bidirectional REST Server }

{ TWebSocketAsyncServerRest }

constructor TWebSocketAsyncServerRest.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName: RawUtf8;
  ServerThreadPoolCount: integer;
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax, aLogVerbose: boolean);
begin
  inherited Create(aPort, OnStart, OnStop, aProcessName, ServerThreadPoolCount,
    {alive=}30000, {headersunfiltered=}false, {suspended=}false, aLogVerbose);
  WebSocketsEnable(aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAjax);
end;

procedure TWebSocketAsyncServerRest.WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions);
begin
  if self = nil then
    exit;
  fProtocols.AddOnce(TWebSocketProtocolBinary.Create(
    aWebSocketsURI, {server=}true, aWebSocketsEncryptionKey,
    @fSettings, aWebSocketsBinaryOptions));
  if aWebSocketsAjax then
    fProtocols.AddOnce(TWebSocketProtocolJson.Create(aWebSocketsURI));
end;

function TWebSocketAsyncServerRest.Callback(Ctxt: THttpServerRequest;
  aNonBlocking: boolean): cardinal;
var
  connection: pointer; // TWebSocketAsyncConnection
  mode: TWebSocketProcessNotifyCallback;
begin
  if aNonBlocking then // see TInterfacedObjectFakeServer.CallbackInvoke
    mode := wscNonBlockWithoutAnswer
  else
    mode := wscBlockWithAnswer;
  if Ctxt = nil then
    connection := nil
  else
  begin
    connection := fAsync.ConnectionFind(Ctxt.ConnectionID); // O(log(n)) lookup
    WebSocketLog.Add.Log(LOG_TRACEERROR[connection = nil],
      'Callback(%) % on ConnectionID=% -> %',
      [Ctxt.Url, ToText(mode)^, Ctxt.ConnectionID, connection], self);
  end;
  if connection <> nil then
  begin
    // this request is a websocket, on a non broken connection
    result := TWebSocketAsyncConnection(connection).fProcess.
                NotifyCallback(Ctxt, mode);
    // if connection is released on a background thread, NotifyCallback should
    // check fProcess.fState and abort any waiting loop
  end
  else
    result := HTTP_NOTFOUND;
end;


end.

