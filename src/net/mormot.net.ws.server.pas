/// WebSockets Server-Side Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ws.server;

{
  *****************************************************************************

    WebSockets Bidirectional Server
    - TWebSocketProtocolChat Simple Protocol
    - TWebSocketProcessServer Processing Class
    - TWebSocketServerSocket Bidirectional REST Server

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
  mormot.crypt.core,
  mormot.crypt.ecc,
  mormot.crypt.secure, // IProtocol definition
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.ws.core;


{ ******************** TWebSocketProtocolChat Simple Protocol }

type
  /// callback event triggered by TWebSocketProtocolChat for any incoming message
  // - a first call with frame.opcode=focContinuation will take place when
  // the connection will be upgraded to WebSockets
  // - then any incoming focText/focBinary events will trigger this callback
  // - eventually, a focConnectionClose will notify the connection ending
  TOnWebSocketProtocolChatIncomingFrame = procedure(
    Sender: TWebCrtSocketProcess;
    const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. with AJAX applications
  // - see TWebSocketProtocolRest for bi-directional events expecting answers
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUtf8); override;
  public
    /// initialize the chat protocol with an incoming frame callback
    constructor Create(const aName, aUri: RawUtf8;
       const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame); overload;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
    /// on the server side, allows to send a message over the wire to a
    // specified client connection
    // - a temporary copy of the Frame content will be made for safety
    function SendFrame(Sender: TWebCrtSocketProcess;
       const Frame: TWebSocketFrame): boolean;
    /// on the server side, allows to send a JSON message over the wire to a
    // specified client connection
    // - the supplied JSON content is supplied as "var", since it may be
    // modified during execution, e.g. XORed for frame masking
    function SendFrameJson(Sender: TWebCrtSocketProcess;
       var Json: RawUtf8): boolean;
    /// you can assign an event to this property to be notified of incoming messages
    property OnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame
      read fOnIncomingFrame write fOnIncomingFrame;
  end;



{ ******************** TWebSocketProcessServer Processing Class }

type
  {$M+}
  TWebSocketServerResp = class;
  {$M-}

  /// implements WebSockets process as used on server side
  TWebSocketProcessServer = class(TWebCrtSocketProcess)
  protected
    fServerResp: TWebSocketServerResp;
    function ComputeContext(
      out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract; override;
  public
    /// low-level access to the associated HTTP/WebSockets connection
    property ServerResp: TWebSocketServerResp
      read fServerResp;
  end;

  /// meta-class of WebSockets process as used on server side
  TWebSocketProcessServerClass = class of TWebSocketProcessServer;



{ ******************** TWebSocketServerSocket Bidirectional REST Server }

  /// an enhanced input/output structure used for HTTP and WebSockets requests
  // - this class will contain additional parameters used to maintain the
  // WebSockets execution context in overriden TWebSocketServer.Process method
  TWebSocketServerResp = class(THttpServerResp)
  protected
    fProcess: TWebSocketProcessServer;
  public
    /// initialize the context, associated to a HTTP/WebSockets server instance
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer); override;
    /// called by THttpServer.Destroy on existing connections
    // - send focConnectionClose before killing the socket
    procedure Shutdown; override;
    /// push a notification to the client
    function _NotifyCallback(Ctxt: THttpServerRequest;
      aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJson or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    function WebSocketProtocol: TWebSocketProtocol;
      {$ifdef HASINLINE} inline; {$endif}
    /// low-level WebSocket protocol processing instance
    property WebSocketProcess: TWebSocketProcessServer
      read fProcess;
  end;

  /// callback signature to notify TWebSocketServer connections
  TOnWebSocketServerEvent = procedure(Sender: TWebSocketServerResp) of object;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fWebSocketConnections: TSynObjectListLocked;
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
    fProcessClass: TWebSocketProcessServerClass;
    fOnWSConnect: TOnWebSocketServerEvent;
    fOnWSDisconnect: TOnWebSocketServerEvent;
    procedure DoConnect(Context: TWebSocketServerResp); virtual;
    procedure DoDisconnect(Context: TWebSocketServerResp); virtual;
    /// validate the WebSockets handshake, then call Context.fProcess.ProcessLoop()
    function WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
      Context: TWebSocketServerResp): integer; virtual;
    /// overriden method which will recognize the WebSocket protocol handshake,
    // then run the whole bidirectional communication in its calling thread
    // - here ConnectionThread is a THttpServerResp, and ClientSock.Headers
    // and ConnectionUpgrade properties should be checked for the handshake
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread); override;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - due to the way how WebSockets works, one thread will be created
    // for any incoming connection
    // - note that this constructor will not register any protocol, so is
    // useless until you execute Protocols.Add()
    // - in the current implementation, the ServerThreadPoolCount parameter will
    // use two threads by default to handle shortliving HTTP/1.0 "connection: close"
    // requests, and one thread will be maintained per keep-alive/websockets client
    // - by design, the KeepAliveTimeOut value is ignored with this server
    // once it has been upgraded to WebSockets
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
      ServerThreadPoolCount: integer = 2; KeepAliveTimeOut: integer = 30000;
      HeadersUnFiltered: boolean = false; CreateSuspended: boolean = false); override;
    /// close the server
    destructor Destroy; override;
    /// will send a given frame to all connected clients
    // - expect aFrame.opcode to be either focText or focBinary
    // - will call TWebSocketProcess.Outgoing.Push for asynchronous sending
    procedure WebSocketBroadcast(const aFrame: TWebSocketFrame); overload;
    /// will send a given frame to clients matching the supplied connection IDs
    // - expect aFrame.opcode to be either focText or focBinary
    // - WebSocketBroadcast(nil) will broadcast to all running websockets
    // - will call TWebSocketProcess.Outgoing.Push for asynchronous sending
    procedure WebSocketBroadcast(const aFrame: TWebSocketFrame;
      const aClientsConnectionID: THttpServerConnectionIDDynArray); overload;
    /// give access to the underlying WebSockets connection from its ID
    function IsActiveWebSocket(
      ConnectionID: THttpServerConnectionID): TWebSocketProcessServer;
    /// allow to customize the WebSockets processing
    // - apply to all protocols on this server instance
    // - those parameters are accessed by reference from existing connections,
    // so you should better not modify them once the server started
    function Settings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
    /// allow to customize the WebSockets processing classes
    property ProcessClass: TWebSocketProcessServerClass
      read fProcessClass write fProcessClass;
    /// how many WebSockets connections are currently maintained
    function WebSocketConnections: integer;
    /// access to the protocol list handled by this server
    property WebSocketProtocols: TWebSocketProtocolList
      read fProtocols;
    /// event triggerred when a new connection upgrade has been done
    // - just before the main processing WebSockets frames processing loop
    property OnWebSocketConnect: TOnWebSocketServerEvent
      read fOnWSConnect write fOnWSConnect;
    /// event triggerred when a connection is about to be closed
    // - when the main processing WebSockets frames processing loop finishes
    property OnWebSocketDisconnect: TOnWebSocketServerEvent
      read fOnWSDisconnect write fOnWSDisconnect;
  end;


  TWebSocketServerSocket = class(THttpServerSocket)
  protected
    fProcess: TWebCrtSocketProcess; // set once upgraded
    /// overriden to detect upgrade, or process WebSockets in the thread pool
    procedure TaskProcess(aCaller: TSynThreadPoolWorkThread;
      aTimeOutTix: Int64); override;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // our proprietary Sec-WebSocket-Protocol: 'synopsejson' or 'synopsebin'
  // application content, managing regular REST client-side requests and
  // also server-side push notifications
  // - once in 'synopse*' mode, the Request() method will be trigerred from
  // any incoming WebSocket frame from the client, and the OnCallback event
  // will be available to push a WebSocket frame from the server to the client
  TWebSocketServerRest = class(TWebSocketServer)
  public
    /// create a Server Thread, binded and listening on a port, with our
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
      const aProcessName, aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false); reintroduce; overload;
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


{ ******************** TWebSocketProtocolChat Simple Protocol }

{ TWebSocketProtocolChat }

constructor TWebSocketProtocolChat.Create(const aName, aUri: RawUtf8;
  const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame);
begin
  inherited Create(aName, aUri);
  fOnIncomingFrame := aOnIncomingFrame;
end;

function TWebSocketProtocolChat.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName, fUri);
  if fEncryption <> nil then
    TWebSocketProtocolChat(result).fEncryption := fEncryption.Clone;
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

procedure TWebSocketProtocolChat.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUtf8);
begin
  if Assigned(OnInComingFrame) then
  try
    if Sender.InheritsFrom(TWebCrtSocketProcess) then
      OnIncomingFrame(TWebCrtSocketProcess(Sender), request)
    else
      OnIncomingFrame(nil, request);
  except
    // ignore any exception in the callback
  end;
end;

function TWebSocketProtocolChat.SendFrame(Sender: TWebCrtSocketProcess;
  const frame: TWebSocketFrame): boolean;
var
  tmp: TWebSocketFrame; // SendFrame() may change frame content (e.g. mask)
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     (Sender.State <> wpsRun) or
     not (frame.opcode in [focText, focBinary])  then
    exit;
  tmp.opcode := frame.opcode;
  tmp.content := frame.content;
  SetString(tmp.payload, PAnsiChar(Pointer(frame.payload)), length(frame.payload));
  result := Sender.SendFrame(tmp)
end;

function TWebSocketProtocolChat.SendFrameJson(Sender: TWebCrtSocketProcess;
  var Json: RawUtf8): boolean;
var
  frame: TWebSocketFrame;
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     (Sender.State <> wpsRun) then
    exit;
  frame.opcode := focText;
  frame.content := [];
  frame.payload := Json;
  result := Sender.SendFrame(frame)
end;


{ ******************** TWebSocketProcessServer Processing Class }

{ TWebSocketProcessServer }

function TWebSocketProcessServer.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
begin
  result := THttpServerRequest.Create(
    (fOwnerThread as TWebSocketServerResp).fServer, fOwnerConnection,
    fOwnerThread, fProtocol.ConnectionFlags);
  RequestProcess := TWebSocketServerResp(fOwnerThread).Server.Request;
end;


{ ******************** TWebSocketProcessServer Processing Class }

function HttpServerWebSocketUpgrade(ClientSock: THttpServerSocket;
  Protocols: TWebSocketProtocolList; out Protocol: TWebSocketProtocol): integer;
var
  uri, version, prot, subprot, key, extin, extout, header: RawUtf8;
  extins: TRawUtf8DynArray;
  P: PUtf8Char;
  Digest: TSha1Digest;
begin
  result := HTTP_BADREQUEST;
  try
    if not IdemPropNameU(ClientSock.Upgrade, 'websocket') then
      exit;
    version := ClientSock.HeaderGetValue('SEC-WEBSOCKET-VERSION');
    if GetInteger(pointer(version)) < 13 then
      exit; // we expect WebSockets protocol version 13 at least
    uri := TrimU(ClientSock.URL);
    if (uri <> '') and
       (uri[1] = '/') then
      Delete(uri, 1, 1);
    prot := ClientSock.HeaderGetValue('SEC-WEBSOCKET-PROTOCOL');
    P := pointer(prot);
    if P <> nil then
    begin
      repeat
        GetNextItemTrimed(P, ',', subprot);
        Protocol := Protocols.CloneByName(subprot, uri);
      until (P = nil) or
            (Protocol <> nil);
      if (Protocol <> nil) and
         (Protocol.Uri = '') and
         not Protocol.ProcessHandshakeUri(uri) then
      begin
        Protocol.Free;
        result := HTTP_NOTFOUND;
        exit;
      end;
    end
    else
      // if no protocol is specified, try to match by URI
      Protocol := Protocols.CloneByUri(uri);
    if Protocol = nil then
      exit;
    Protocol.UpgradeUri := uri;
    Protocol.RemoteIP := ClientSock.HeaderGetValue('SEC-WEBSOCKET-REMOTEIP');
    if Protocol.RemoteIP = '' then
    begin
      Protocol.RemoteIP := ClientSock.RemoteIP;
      Protocol.RemoteLocalhost := (ClientSock.RemoteIP = '127.0.0.1') or
                                   (RemoteIPLocalHostAsVoidInServers and
                                    (ClientSock.RemoteIP = ''));
    end
    else
      Protocol.RemoteLocalhost := Protocol.RemoteIP = '127.0.0.1';
    extin := ClientSock.HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
    if extin <> '' then
    begin
      CsvToRawUtf8DynArray(pointer(extin), extins, ';', true);
      if not Protocol.ProcessHandshake(extins, extout, nil) then
      begin
        Protocol.Free;
        result := HTTP_NOTACCEPTABLE;
        exit;
      end;
    end;
    key := ClientSock.HeaderGetValue('SEC-WEBSOCKET-KEY');
    if Base64ToBinLengthSafe(pointer(key), length(key)) <> 16 then
    begin
      Protocol.Free;
      exit; // this nonce must be a Base64-encoded value of 16 bytes
    end;
    ComputeChallenge(key, Digest);
    if extout <> '' then
      extout := 'Sec-WebSocket-Extensions: ' + extout + #13#10;
    FormatUtf8('HTTP/1.1 101 Switching Protocols'#13#10 +
               'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10 +
               'Sec-WebSocket-Protocol: %'#13#10 +
               '%Sec-WebSocket-Accept: %'#13#10#13#10,
      [Protocol.Name, extout, BinToBase64Short(@Digest, sizeof(Digest))], header);
    if not ClientSock.TrySndLow(pointer(header), length(header)) then
    begin
      Protocol.Free;
      result := HTTP_BADREQUEST;
      exit;
    end;
    result := HTTP_SUCCESS; // connection upgraded: never back to HTTP/1.1
  finally
    if result <> HTTP_SUCCESS then
    begin
      // notify upgrade failure to client and close connection
      FormatUtf8('HTTP/1.0 % WebSocket Upgrade Error'#13#10 +
                 'Connection: Close'#13#10#13#10, [result], header);
      ClientSock.TrySndLow(pointer(header), length(header));
      ClientSock.KeepAliveClient := false;
    end;
  end;
end;


{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount, KeepAliveTimeOut: integer; HeadersUnFiltered, CreateSuspended: boolean);
begin
  // override with custom processing classes
  fSocketClass := TWebSocketServerSocket;
  fThreadRespClass := TWebSocketServerResp;
  fProcessClass := TWebSocketProcessServer;
  // initialize protocols and connections
  fWebSocketConnections := TSynObjectListLocked.Create({owned=}false);
  fProtocols := TWebSocketProtocolList.Create;
  fSettings.SetDefaults;
  fSettings.HeartbeatDelay := 20000;
  fCanNotifyCallback := true;
  // start the server
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, HeadersUnFiltered, CreateSuspended);
end;

function TWebSocketServer.WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
  Context: TWebSocketServerResp): integer;
var
  protocol: TWebSocketProtocol;
begin
  result := HttpServerWebSocketUpgrade(ClientSock, fProtocols, protocol);
  if result <> HTTP_SUCCESS then
    exit;
  // if we reached here, we switched/upgraded to WebSockets bidir frames
  ClientSock.KeepAliveClient := false; // close connection with WebSockets
  Context.fProcess := fProcessClass.Create(ClientSock, protocol,
    Context.ConnectionID, Context, @fSettings, fProcessName);
  Context.fProcess.fServerResp := Context;
  fWebSocketConnections.Add(Context);
  try
    DoConnect(Context);
    Context.fProcess.ProcessLoop;  // run main blocking loop
  finally
    DoDisconnect(Context);
    FreeAndNil(Context.fProcess); // notify end of WebSockets
    fWebSocketConnections.Remove(Context);
  end;
end;

procedure TWebSocketServer.DoConnect(Context: TWebSocketServerResp);
begin
  if Assigned(fOnWSConnect) then
    fOnWSConnect(Context);
end;

procedure TWebSocketServer.DoDisconnect(Context: TWebSocketServerResp);
begin
  if Assigned(fOnWSDisconnect) then
  try
    fOnWSDisconnect(Context);
  except // ignore any external callback error during shutdown
  end;
end;

procedure TWebSocketServer.Process(ClientSock: THttpServerSocket; ConnectionID:
  THttpServerConnectionID; ConnectionThread: TSynThread);
var
  err: integer;
begin
  if (hfConnectionUpgrade in ClientSock.HeaderFlags) and
     ClientSock.KeepAliveClient and
     IdemPropNameU('GET', ClientSock.Method) and
     IdemPropNameU(ClientSock.Upgrade, 'websocket') and
     ConnectionThread.InheritsFrom(TWebSocketServerResp) then
  begin
    err := WebSocketProcessUpgrade(ClientSock, TWebSocketServerResp(ConnectionThread));
    if err <> HTTP_SUCCESS then
      WebSocketLog.Add.Log(sllTrace,
        'Process: WebSocketProcessUpgrade failed as %', [err], self);
  end
  else
    inherited Process(ClientSock, ConnectionID, ConnectionThread);
end;

destructor TWebSocketServer.Destroy;
begin
  inherited Destroy; // close any pending connection
  fWebSocketConnections.Free;
  fProtocols.Free;
end;

function TWebSocketServer.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketServer.WebSocketConnections: integer;
begin
  result := fWebSocketConnections.Count;
end;

type
  PWebSocketServerResp = ^TWebSocketServerResp;

function FastFindConnection(c: PWebSocketServerResp; n: integer;
  id: THttpServerConnectionID): TWebSocketServerResp;
begin
  // speedup brute force check in case of high number of connections
  if n > 0 then
    repeat
      result := c^;
      if result.ConnectionID = id then
        exit;
      inc(c);
      dec(n);
    until n = 0;
  result := nil;
end;

function TWebSocketServer.IsActiveWebSocket(
  ConnectionID: THttpServerConnectionID): TWebSocketProcessServer;
var
  thread: TWebSocketServerResp;
begin
  result := nil;
  if Terminated or
     (ConnectionID = 0) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    thread := FastFindConnection(pointer(fWebSocketConnections.List),
      fWebSocketConnections.Count, ConnectionID);
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
  if (thread <> nil) and
     (thread.fProcess <> nil) and
     (thread.fProcess.State = wpsRun) then
    result := thread.fProcess;
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame);
begin
  WebSocketBroadcast(aFrame, nil);
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame;
  const aClientsConnectionID: THttpServerConnectionIDDynArray);
var
  i, len, ids: integer;
  c: ^TWebSocketServerResp;
  temp: TWebSocketFrame; // local copy since SendFrame() modifies the payload
  sorted: TSynTempBuffer;
begin
  if Terminated or
     not (aFrame.opcode in [focText, focBinary]) then
    exit;
  ids := length(aClientsConnectionID);
  if ids > 0 then
  begin
    sorted.Init(pointer(aClientsConnectionID), ids * 8);
    QuickSortInt64(sorted.buf, 0, ids - 1); // branchless O(log(n)) asm on x86_64
  end;
  dec(ids); // WebSocketBroadcast(nil) -> ids<0 -> broadcast all
  temp.opcode := aFrame.opcode;
  temp.content := aFrame.content;
  len := length(aFrame.payload);
  fWebSocketConnections.Safe.Lock;
  try
    c := pointer(fWebSocketConnections.List);
    for i := 1 to fWebSocketConnections.Count do
    begin
      if (c^.fProcess.State = wpsRun) and
         ((ids < 0) or
          (FastFindInt64Sorted(sorted.buf, ids, c^.ConnectionID) >= 0)) then
      begin
        SetString(temp.payload, PAnsiChar(pointer(aFrame.payload)), len);
        c^.fProcess.Outgoing.Push(temp); // non blocking asynchronous sending
      end;
      inc(c);
    end;
  finally
    fWebSocketConnections.Safe.UnLock;
    if ids >= 0 then
      sorted.Done;
  end;
end;


{ TWebSocketServerResp }

constructor TWebSocketServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  if not aServer.InheritsFrom(TWebSocketServer) then
    raise EWebSockets.CreateUtf8('%.Create(%: TWebSocketServer?)', [self, aServer]);
  inherited Create(aServerSock, aServer);
end;

procedure TWebSocketServerResp.Shutdown;
begin
  if not fProcess.ConnectionCloseWasSent then
  begin
    WebSocketLog.Add.Log(sllTrace, 'Shutdown: send focConnectionClose', self);
    fProcess.Shutdown; // notify client with focConnectionClose
  end;
  inherited Shutdown;
end;

function TWebSocketServerResp._NotifyCallback(Ctxt: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
begin
  if fProcess = nil then
    result := HTTP_NOTFOUND
  else
    result := fProcess.NotifyCallback(Ctxt, aMode);
end;

function TWebSocketServerResp.WebSocketProtocol: TWebSocketProtocol;
begin
  if (Self = nil) or
     (fProcess = nil) then
    result := nil
  else
    result := fProcess.Protocol;
end;


{ TWebSocketServerSocket }

procedure TWebSocketServerSocket.TaskProcess(aCaller: TSynThreadPoolWorkThread;
  aTimeOutTix: Int64);
var
  freeme: boolean;
  res: THttpServerSocketGetRequestResult;
begin
  // from TSynThreadPoolTHttpServer.Task
  freeme := true;
  try
    if Assigned(fProcess) then
    begin
      freeme := false;
      exit;
    end;
    res := GetRequest({withbody=}false, aTimeOutTix);
    if (res = grHeaderReceived) and
       (hfConnectionUpgrade in HeaderFlags) and
       KeepAliveClient and
       IdemPropNameU(Method, 'GET') and
       IdemPropNameU(Upgrade, 'websocket') then
    begin
      // perform a WebSockets upgrade
      res := grHeaderReceived;
    end;
    // regular HTTP request process
    freeme := TaskProcessBody(aCaller, res);
  finally
    if freeme then
      Free;
  end;
end;


{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName, aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean);
begin
  Create(aPort, OnStart, OnStop, aProcessName);
  WebSocketsEnable(aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAjax);
end;

procedure TWebSocketServerRest.WebSocketsEnable(
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax: boolean;
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

function TWebSocketServerRest.Callback(Ctxt: THttpServerRequest;
  aNonBlocking: boolean): cardinal;
var
  connection: TWebSocketProcessServer;
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
    connection := IsActiveWebSocket(Ctxt.ConnectionID);
    WebSocketLog.Add.Log(LOG_TRACEERROR[connection = nil],
      'Callback(%) % on ConnectionID=%',
      [Ctxt.Url, ToText(mode)^, Ctxt.ConnectionID], self);
  end;
  if connection <> nil then
    // this request is a websocket, on a non broken connection
    result := connection.NotifyCallback(Ctxt, mode)
  else
    result := HTTP_NOTFOUND;
end;



end.

