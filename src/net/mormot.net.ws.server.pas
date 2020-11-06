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
  mormot.core.crypto,
  mormot.core.ecc,
  mormot.core.secure, // IProtocol definition
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
  TOnWebSocketProtocolChatIncomingFrame = procedure(Sender: THttpServerResp;
    const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. with AJAX applications
  // - see TWebSocketProtocolRest for bi-directional events expecting answers
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// initialize the chat protocol with an incoming frame callback
    constructor Create(const aName, aURI: RawUTF8;
       const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame); overload;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
    /// on the server side, allows to send a message over the wire to a
    // specified client connection
    // - a temporary copy of the Frame content will be made for safety
    function SendFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame): boolean;
    /// on the server side, allows to send a JSON message over the wire to a
    // specified client connection
    // - the supplied JSON content is supplied as "var", since it may be
    // modified during execution, e.g. XORed for frame masking
    function SendFrameJson(Sender: THttpServerResp; var JSON: RawUTF8): boolean;
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
  end;



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
    /// push a notification to the client
    function NotifyCallback(Ctxt: THttpServerRequest;
      aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    function WebSocketProtocol: TWebSocketProtocol;
    /// low-level WebSocket protocol processing instance
    property WebSocketProcess: TWebSocketProcessServer read fProcess;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fWebSocketConnections: TSynObjectListLocked;
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
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
    constructor Create(const aPort: RawUTF8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUTF8;
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
    // - will call TWebSocketProcess.Outgoing.Push for asynchronous sending
    procedure WebSocketBroadcast(const aFrame: TWebSocketFrame;
      const aClientsConnectionID: THttpServerConnectionIDDynArray); overload;
    /// give access to the underlying connection from its ID
    // - also identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocket(
      ConnectionID: THttpServerConnectionID): TWebSocketServerResp;
    /// give access to the underlying connection from its connection thread
    // - also identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocketThread(
      ConnectionThread: TSynThread): TWebSocketServerResp;
    /// the settings to be used for WebSockets process
    // - note that those parameters won't be propagated to existing connections
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
    /// how many WebSockets connections are currently maintained
    function WebSocketConnections: integer;
    /// access to the protocol list handled by this server
    property WebSocketProtocols: TWebSocketProtocolList
      read fProtocols;
  end;


  TWebSocketServerSocket = class(THttpServerSocket)
  public
    /// overriden to detect upgrade: websocket header and return grOwned
    function GetRequest(withBody: boolean;
      headerMaxTix: Int64): THttpServerSocketGetRequestResult; override;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // our proprietary Sec-WebSocket-Protocol: 'synopsejson' or 'synopsebin'
  // application content, managing regular REST client-side requests and
  // also server-side push notifications
  // - once in 'synopse*' mode, the Request() method will be trigerred from
  // any incoming REST request from the client, and the OnCallback event
  // will be available to push a request from the server to the client
  TWebSocketServerRest = class(TWebSocketServer)
  public
    /// create a Server Thread, binded and listening on a port, with our
    // 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    // - warning: WaitStarted should be called after Create() to check for
    // for actual port binding in the background thread
    constructor Create(const aPort: RawUTF8; const OnStart, OnStop: TOnNotifyThread;
      const aProcessName, aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean = false); reintroduce; overload;
    /// defines the WebSockets protocols to be used for this Server
    // - i.e. 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    procedure WebSocketsEnable(const aWebSocketsURI, aWebSocketsEncryptionKey:
      RawUTF8; aWebSocketsAJAX: boolean = false; aWebSocketsCompressed: boolean = true);
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

constructor TWebSocketProtocolChat.Create(const aName, aURI: RawUTF8;
  const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame);
begin
  inherited Create(aName, aURI);
  fOnIncomingFrame := aOnIncomingFrame;
end;

function TWebSocketProtocolChat.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName, fURI);
  if fEncryption <> nil then
    TWebSocketProtocolChat(result).fEncryption := fEncryption.Clone;
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

procedure TWebSocketProtocolChat.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
begin
  if Assigned(OnInComingFrame) then
  try
    if Sender.InheritsFrom(TWebSocketProcessServer) then
      OnIncomingFrame(TWebSocketProcessServer(Sender).fServerResp, request)
    else
      OnIncomingFrame(nil, request);
  except
    // ignore any exception in the callback
  end;
end;

function TWebSocketProtocolChat.SendFrame(Sender: THttpServerResp;
  const frame: TWebSocketFrame): boolean;
var
  tmp: TWebSocketFrame; // SendFrame() may change frame content (e.g. mask)
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     Sender.Terminated or
     not (frame.opcode in [focText, focBinary]) or
     ((Sender.Server as TWebSocketServer).
       IsActiveWebSocketThread(Sender) <> Sender) then
    exit;
  tmp.opcode := frame.opcode;
  tmp.content := frame.content;
  SetString(tmp.payload, PAnsiChar(Pointer(frame.payload)), length(frame.payload));
  result := (Sender as TWebSocketServerResp).fProcess.SendFrame(tmp)
end;

function TWebSocketProtocolChat.SendFrameJson(Sender: THttpServerResp;
  var JSON: RawUTF8): boolean;
var
  frame: TWebSocketFrame;
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     Sender.Terminated or
     ((Sender.Server as TWebSocketServer).
       IsActiveWebSocketThread(Sender) <> Sender) then
    exit;
  frame.opcode := focText;
  frame.content := [];
  frame.payload := JSON;
  result := (Sender as TWebSocketServerResp).fProcess.SendFrame(frame)
end;


{ ******************** TWebSocketProcessServer Processing Class }

{ TWebSocketProcessServer }

function TWebSocketProcessServer.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
begin
  result := THttpServerRequest.Create(
    (fOwnerThread as TWebSocketServerResp).fServer, fOwnerConnection, fOwnerThread);
  RequestProcess := TWebSocketServerResp(fOwnerThread).Server.Request;
end;


{ ******************** TWebSocketProcessServer Processing Class }

function HttpServerWebSocketUpgrade(ClientSock: THttpServerSocket;
  Protocols: TWebSocketProtocolList; out Protocol: TWebSocketProtocol): integer;
var
  uri, version, prot, subprot, key, extin, extout, header: RawUTF8;
  extins: TRawUTF8DynArray;
  P: PUTF8Char;
  Digest: TSHA1Digest;
begin
  result := HTTP_BADREQUEST;
  try
    if not IdemPropNameU(ClientSock.Upgrade, 'websocket') then
      exit;
    version := ClientSock.HeaderGetValue('SEC-WEBSOCKET-VERSION');
    if GetInteger(pointer(version)) < 13 then
      exit; // we expect WebSockets protocol version 13 at least
    uri := Trim(RawUTF8(ClientSock.URL));
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
         (Protocol.URI = '') and
         not Protocol.ProcessHandshakeURI(uri) then
      begin
        Protocol.Free;
        result := HTTP_UNAUTHORIZED;
        exit;
      end;
    end
    else
      // if no protocol is specified, try to match by URI
      Protocol := Protocols.CloneByURI(uri);
    if Protocol = nil then
      exit;
    Protocol.UpgradeURI := uri;
    Protocol.RemoteIP := ClientSock.HeaderGetValue('SEC-WEBSOCKET-REMOTEIP');
    if Protocol.RemoteIP = '' then
      Protocol.RemoteIP := ClientSock.RemoteIP;
    Protocol.RemoteLocalhost := Protocol.RemoteIP = '127.0.0.1';
    extin := ClientSock.HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
    if extin <> '' then
    begin
      CSVToRawUTF8DynArray(pointer(extin), extins, ';', true);
      if not Protocol.ProcessHandshake(extins, extout, nil) then
      begin
        Protocol.Free;
        result := HTTP_UNAUTHORIZED;
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
    FormatUTF8('HTTP/1.1 101 Switching Protocols'#13#10 +
      'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10 +
      'Sec-WebSocket-Protocol: %'#13#10'%Sec-WebSocket-Accept: %'#13#10#13#10,
      [Protocol.Name, extout, BinToBase64Short(@Digest, sizeof(Digest))], header);
    if not ClientSock.TrySndLow(pointer(header), length(header)) then
    begin
      Protocol.Free;
      result := HTTP_WEBSOCKETCLOSED;
      exit;
    end;
    result := HTTP_SUCCESS; // connection upgraded: never back to HTTP/1.1
  finally
    if result <> HTTP_SUCCESS then
    begin // notify upgrade failure to client
      FormatUTF8('HTTP/1.0 % WebSocket Upgrade Error'#13#10 +
        'Connection: Close'#13#10#13#10, [result], header);
      ClientSock.TrySndLow(pointer(header), length(header));
      ClientSock.KeepAliveClient := false;
    end;
  end;
end;


{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: RawUTF8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUTF8;
  ServerThreadPoolCount, KeepAliveTimeOut: integer; HeadersUnFiltered, CreateSuspended: boolean);
begin
  // override with custom processing classes
  fSocketClass := TWebSocketServerSocket;
  fThreadRespClass := TWebSocketServerResp;
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
  ClientSock.KeepAliveClient := false; // close connection with WebSockets
  Context.fProcess := TWebSocketProcessServer.Create(ClientSock, protocol,
    Context.ConnectionID, Context, fSettings, fProcessName);
  Context.fProcess.fServerResp := Context;
  fWebSocketConnections. Add(Context);
  try
    Context.fProcess.ProcessLoop;  // run main blocking loop
  finally
    FreeAndNil(Context.fProcess); // notify end of WebSockets
    fWebSocketConnections.Remove(Context);
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

function TWebSocketServer.IsActiveWebSocketThread(
  ConnectionThread: TSynThread): TWebSocketServerResp;
var
  i: integer;
  c: PWebSocketServerResp;
begin // no need to optimize (not called often)
  result := nil;
  if Terminated or
     (ConnectionThread = nil) or
     not ConnectionThread.InheritsFrom(TWebSocketServerResp) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    c := pointer(fWebSocketConnections.List);
    for i := 1 to fWebSocketConnections.Count do
      if c^ = ConnectionThread then
      begin
        if c^.fProcess.State = wpsRun then
          result := c^;
        exit;
      end
      else
        inc(c);
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
end;

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
      if n = 0 then
        break;
      result := c^;
      if result.ConnectionID = id then
        exit;
      inc(c);
      dec(n);
    until n = 0;
  result := nil;
end;

function TWebSocketServer.IsActiveWebSocket(
  ConnectionID: THttpServerConnectionID): TWebSocketServerResp;
begin
  result := nil;
  if Terminated or
     (ConnectionID = 0) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    result := FastFindConnection(pointer(fWebSocketConnections.List),
      fWebSocketConnections.Count, ConnectionID);
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
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
    QuickSortInt64(sorted.buf, 0, ids - 1); // faster O(log(n)) binary search
  end;
  dec(ids); // WebSocketBroadcast(nil) -> ids<0
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

{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: RawUTF8;
  const OnStart, OnStop: TOnNotifyThread; const aProcessName, aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean);
begin
  Create(aPort, OnStart, OnStop, aProcessName);
  WebSocketsEnable(aWebSocketsURI, aWebSocketsEncryptionKey, aWebSocketsAJAX);
end;

procedure TWebSocketServerRest.WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX, aWebSocketsCompressed: boolean);
begin
  if self = nil then
    exit;
  fProtocols.AddOnce(TWebSocketProtocolBinary.Create(aWebSocketsURI, true,
    aWebSocketsEncryptionKey, aWebSocketsCompressed));
  if aWebSocketsAJAX then
    fProtocols.AddOnce(TWebSocketProtocolJSON.Create(aWebSocketsURI));
end;

function TWebSocketServerRest.Callback(Ctxt: THttpServerRequest;
  aNonBlocking: boolean): cardinal;
var
  connection: TWebSocketServerResp;
  mode: TWebSocketProcessNotifyCallback;
begin
  if Ctxt = nil then
    connection := nil
  else
  begin
    WebSocketLog.Add.Log(sllTrace, 'Callback(%) on socket=%',
      [Ctxt.URL, Ctxt.ConnectionID], self);
    connection := IsActiveWebSocket(Ctxt.ConnectionID);
  end;
  if connection <> nil then
  begin
    //  this request is a websocket, on a non broken connection
    if aNonBlocking then // see TInterfacedObjectFakeServer.CallbackInvoke
      mode := wscNonBlockWithoutAnswer
    else
      mode := wscBlockWithAnswer;
    result := connection.NotifyCallback(Ctxt, mode);
  end
  else
  begin
    WebSocketLog.Add.Log(sllError, 'Callback() on inactive socket', self);
    result := HTTP_NOTFOUND;
  end;
end;


{ TWebSocketServerResp }

constructor TWebSocketServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  if not aServer.InheritsFrom(TWebSocketServer) then
    raise EWebSockets.CreateUTF8('%.Create(%: TWebSocketServer?)', [self, aServer]);
  inherited Create(aServerSock, aServer);
end;

function TWebSocketServerResp.NotifyCallback(Ctxt: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
begin
  if fProcess = nil then
    result := HTTP_NOTFOUND
  else
  begin
    result := fProcess.NotifyCallback(Ctxt, aMode);
    if result = HTTP_WEBSOCKETCLOSED then
    begin
      WebSocketLog.Add.Log(sllError, 'NotifyCallback on closed connection', self);
      ServerSock.KeepAliveClient := false; // force close the connection
      result := HTTP_NOTFOUND;
    end;
  end;
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

function TWebSocketServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
begin
  result := inherited GetRequest(withBody, headerMaxTix);
  if (result = grHeaderReceived) and
     (hfConnectionUpgrade in HeaderFlags) and
    KeepAliveClient and IdemPropNameU(Method, 'GET') and
    IdemPropNameU(Upgrade, 'websocket') then
    //writeln('!!');
end;




end.

