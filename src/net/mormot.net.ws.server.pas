/// WebSockets Server-Side Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ws.server;

{
  *****************************************************************************

    WebSockets Bidirectional Server
    - TWebSocketProtocolChat Simple Protocol
    - TWebSocketProcessServer Processing Class
    - TWebSocketServerRest Bidirectional REST Server

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
  /// implements WebSockets process as used on server side
  TWebSocketProcessServer = class(TWebCrtSocketProcess)
  protected
    function ComputeContext(
      out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract; override;
  end;

  /// meta-class of WebSockets process as used on server side
  TWebSocketProcessServerClass = class of TWebSocketProcessServer;



{ ******************** TWebSocketServerRest Bidirectional REST Server }

  /// socket maintained for each connection to the HTTP/WebSockets server
  // - this class contains additional parameters used to maintain the
  // WebSockets execution context in overriden TWebSocketServer.Process method
  TWebSocketServerSocket = class(THttpServerSocket)
  protected
    fProcess: TWebSocketProcessServer; // set once upgraded
  public
    /// ensure focConnectionClose is done before closing the connection
    procedure Close; override;
    /// finalize this socket and its associated TWebSocketProcessServer
    destructor Destroy; override;
    /// push a notification to the client
    function NotifyCallback(Ctxt: THttpServerRequest;
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
  TOnWebSocketServerEvent = procedure(Sender: TWebSocketServerSocket) of object;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fWebSocketConnections: TSynObjectListLocked; // of TWebSocketProcessServer
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
    fProcessClass: TWebSocketProcessServerClass;
    fOnWSConnect: TOnWebSocketServerEvent;
    fOnWSDisconnect: TOnWebSocketServerEvent;
    procedure DoConnect(Context: TWebSocketServerSocket); virtual;
    procedure DoDisconnect(Context: TWebSocketServerSocket); virtual;
    /// validate the WebSockets handshake, then call Context.fProcess.ProcessLoop()
    function WebSocketProcessUpgrade(ClientSock: THttpServerSocket): integer; virtual;
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
      HeadersUnFiltered: boolean = false; CreateSuspended: boolean = false;
      aLogVerbose: boolean = false); override;
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
    function WebSocketsEnable(const aWebSocketsURI,
      aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions =
        [pboSynLzCompress]): pointer; override;
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
  frame.tix := 0;
  frame.payload := Json;
  result := Sender.SendFrame(frame)
end;


{ ******************** TWebSocketProcessServer Processing Class }

{ TWebSocketProcessServer }

function TWebSocketProcessServer.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
var
  server: THttpServer;
begin
  server := (fSocket as TWebSocketServerSocket).Server;
  result := THttpServerRequest.Create(
    server, fOwnerConnectionID, fOwnerThread, fProtocol.ConnectionFlags);
  RequestProcess :=  server.Request;
end;


{ ******************** TWebSocketProcessServer Processing Class }

{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount, KeepAliveTimeOut: integer;
  HeadersUnFiltered, CreateSuspended, aLogVerbose: boolean);
begin
  // override with custom processing classes
  fSocketClass := TWebSocketServerSocket;
  fProcessClass := TWebSocketProcessServer;
  // initialize protocols and connections
  fCallbackSendDelay := @fSettings.SendDelay;
  fWebSocketConnections := TSynObjectListLocked.Create({owned=}false);
  fProtocols := TWebSocketProtocolList.Create;
  fSettings.SetDefaults;
  fSettings.HeartbeatDelay := 20000;
  if aLogVerbose then
    fSettings.SetFullLog;
  if ServerThreadPoolCount > 4 then
    ServerThreadPoolCount := 4; // don't loose threads for nothing
  // start the server
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, HeadersUnFiltered, CreateSuspended);
end;

function TWebSocketServer.WebSocketProcessUpgrade(
  ClientSock: THttpServerSocket): integer;
var
  protocol: TWebSocketProtocol;
  resp: RawUtf8;
  sock: TWebSocketServerSocket;
begin
  sock := ClientSock as TWebSocketServerSocket;
  // validate the WebSockets upgrade handshake
  sock.KeepAliveClient := false; // close connection with WebSockets
  result := fProtocols.ServerUpgrade(sock.Http, sock.RemoteIP,
    sock.RemoteConnectionID, protocol, resp);
  if result = HTTP_SUCCESS then
    if not sock.TrySndLow(pointer(resp), length(resp)) then
    begin
      protocol.Free;
      result := HTTP_BADREQUEST;
    end;
  if result <> HTTP_SUCCESS then
  begin
    // notify upgrade failure to client and close connection
    FormatUtf8('HTTP/1.0 % WebSocket Upgrade Error'#13#10 +
               'Connection: Close'#13#10#13#10, [result], resp);
    sock.TrySndLow(pointer(resp), length(resp));
    exit;
  end;
  // if we reached here, we switched/upgraded to WebSockets bidir frames
  sock.fProcess := fProcessClass.Create(ClientSock, protocol,
    sock.RemoteConnectionID, {ownerthread=}nil, @fSettings, fProcessName);
  fWebSocketConnections.Add(sock.fProcess);
  try
    DoConnect(sock);
    sock.fProcess.ProcessLoop;  // run main blocking loop
  finally
    DoDisconnect(sock);
    fWebSocketConnections.Remove(sock.fProcess);
    FreeAndNilSafe(sock.fProcess); // notify end of WebSockets
  end;
end;

procedure TWebSocketServer.DoConnect(Context: TWebSocketServerSocket);
begin
  if Assigned(fOnWSConnect) then
    fOnWSConnect(Context);
end;

procedure TWebSocketServer.DoDisconnect(Context: TWebSocketServerSocket);
begin
  if Assigned(fOnWSDisconnect) then
    try
      fOnWSDisconnect(Context);
    except // ignore any external callback error during shutdown
    end;
end;

procedure TWebSocketServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var
  err: integer;
begin
  if (hfConnectionUpgrade in ClientSock.Http.HeaderFlags) and
     ClientSock.KeepAliveClient and
     IdemPropNameU('GET', ClientSock.Method) and
     IdemPropNameU(ClientSock.Http.Upgrade, 'websocket') then
  begin
    // upgrade and run fProcess.ProcessLoop
    err := WebSocketProcessUpgrade(ClientSock);
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
  PWebSocketProcessServer = ^TWebSocketProcessServer;

function FastFindConnection(c: PWebSocketProcessServer; n: integer;
  id: THttpServerConnectionID): TWebSocketProcessServer;
begin
  // speedup brute force check in case of high number of connections
  if n > 0 then
    repeat
      result := c^;
      if result.OwnerConnectionID = id then
        exit;
      inc(c);
      dec(n);
    until n = 0;
  result := nil;
end;

function TWebSocketServer.IsActiveWebSocket(
  ConnectionID: THttpServerConnectionID): TWebSocketProcessServer;
begin
  result := nil;
  if Terminated or
     (ConnectionID = 0) then
    exit;
  fWebSocketConnections.Safe.ReadOnlyLock;
  try
    result := FastFindConnection(pointer(fWebSocketConnections.List),
      fWebSocketConnections.Count, ConnectionID);
  finally
    fWebSocketConnections.Safe.ReadOnlyUnLock;
  end;
  if (result <> nil) and
     (result.State <> wpsRun) then
    result := nil;
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame);
begin
  WebSocketBroadcast(aFrame, nil);
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame;
  const aClientsConnectionID: THttpServerConnectionIDDynArray);
var
  i, len, ids: integer;
  ws: PWebSocketProcessServer;
  tix: cardinal;
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
    QuickSortInt64(sorted.buf, 0, ids - 1);
  end;
  dec(ids); // WebSocketBroadcast(nil) -> ids<0 -> broadcast all
  temp.opcode := aFrame.opcode;
  temp.content := aFrame.content;
  len := length(aFrame.payload);
  tix := GetTickCount64 shr 10;
  fWebSocketConnections.Safe.ReadOnlyLock;
  try
    ws := pointer(fWebSocketConnections.List);
    for i := 1 to fWebSocketConnections.Count do
    begin
      if (ws^.State = wpsRun) and
          // broadcast all
         ((ids < 0) or
          // branchless O(log(n)) asm on x86_64
          (FastFindInt64Sorted(sorted.buf, ids, ws^.OwnerConnectionID) >= 0)) then
      begin
        SetString(temp.payload, PAnsiChar(pointer(aFrame.payload)), len);
        ws^.Outgoing.Push(temp, tix); // non blocking asynchronous sending
      end;
      inc(ws);
    end;
  finally
    fWebSocketConnections.Safe.ReadOnlyUnLock;
    if ids >= 0 then
      sorted.Done;
  end;
end;


{ TWebSocketServerSocket }

procedure TWebSocketServerSocket.Close;
begin
  if (fProcess <> nil) and
     not fProcess.ConnectionCloseWasSent then
  begin
    WebSocketLog.Add.Log(sllTrace, 'Close: send focConnectionClose', self);
    fProcess.Shutdown({waitforpong=}true); // notify client with focConnectionClose
  end;
  inherited Close;
end;

destructor TWebSocketServerSocket.Destroy;
begin
  inherited Destroy;
  FreeAndNilSafe(fProcess);
end;

function TWebSocketServerSocket.NotifyCallback(Ctxt: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
begin
  if fProcess = nil then
    result := HTTP_NOTFOUND
  else
    result := fProcess.NotifyCallback(Ctxt, aMode);
end;

function TWebSocketServerSocket.WebSocketProtocol: TWebSocketProtocol;
begin
  if (Self = nil) or
     (fProcess = nil) then
    result := nil
  else
    result := fProcess.Protocol;
end;


{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName, aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean);
begin
  Create(aPort, OnStart, OnStop, aProcessName);
  WebSocketsEnable(aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAjax);
end;

function TWebSocketServerRest.WebSocketsEnable(
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions): pointer;
begin
  fProtocols.AddOnce(TWebSocketProtocolBinary.Create(
    aWebSocketsURI, {server=}true, aWebSocketsEncryptionKey,
    @fSettings, aWebSocketsBinaryOptions));
  if aWebSocketsAjax then
    fProtocols.AddOnce(TWebSocketProtocolJson.Create(aWebSocketsURI));
  result := @fSettings;
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

