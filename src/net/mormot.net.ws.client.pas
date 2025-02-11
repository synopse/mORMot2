/// WebSockets Client-Side Process 
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ws.client;

{
  *****************************************************************************

    WebSockets Bidirectional Client
    - TWebSocketProcessClient Processing Class
    - THttpClientWebSockets Bidirectional REST Client
    - Socket.IO / Engine.IO Client Protocol over WebSockets

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
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
  mormot.net.server,  // THttpServerRequest for callbacks
  mormot.net.ws.core;



{ ******************** TWebSocketProcessClient Processing Class }

type
  {$M+}
  THttpClientWebSockets = class;
  TWebSocketProcessClient = class;
  TWebSocketProcessClientThread = class;
  {$M-}

  /// TWebSocketProcessClient.OnReconnect callback definition
  // - is called with aProcess=nil if the socket was not reestablished
  // - otherwise, aProcess could be used to perform any needed registration
  // - should return '' to continue, or an error message to wait and retry
  TOnWebSocketReconnect = function(aProcess: TWebSocketProcessClient): string of object;

  /// implements WebSockets process as used on client side
  TWebSocketProcessClient = class(TWebCrtSocketProcess)
  protected
    fConnectionID: THttpServerConnectionID;
    fOnReconnect: TOnWebSocketReconnect;
    function ComputeContext(
      out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract; override;
  public
    /// initialize the client process for a given THttpClientWebSockets
    constructor Create(aSender: THttpClientWebSockets;
      aConnectionID: THttpServerConnectionID;
      aProtocol: TWebSocketProtocol; const aProcessName: RawUtf8); reintroduce; virtual;
    /// finalize the process
    destructor Destroy; override;
    /// if defined, the background thread will try to reconnect after any
    // server disconnection and call this callback
    property OnReconnect: TOnWebSocketReconnect
      read fOnReconnect write fOnReconnect;
  published
    /// the server-side connection ID, as returned during 101 connection
    // upgrade in 'Sec-WebSocket-Connection-ID' response header
    property ConnectionID: THttpServerConnectionID
      read fConnectionID;
  end;

  /// the current state of the client side processing thread
  TWebSocketProcessClientThreadState = (
    sCreate,
    sRun,
    sFinished,
    sClosed);

  /// WebSockets processing thread used on client side
  // - will handle any incoming callback
  TWebSocketProcessClientThread = class(TSynThread)
  protected
    fThreadState: TWebSocketProcessClientThreadState;
    fProcess: TWebSocketProcessClient;
    procedure Execute; override;
  public
    constructor Create(aProcess: TWebSocketProcessClient); reintroduce;
  end;


{ ******************** THttpClientWebSockets Bidirectional REST Client }

  /// Socket API based REST and HTTP/1.1 client, able to upgrade to WebSockets
  // - will implement regular HTTP/1.1 until WebSocketsUpgrade() is called
  THttpClientWebSockets = class(THttpClientSocket)
  protected
    fProcess: TWebSocketProcessClient;
    fSettings: TWebSocketProcessSettings;
    fOnCallbackRequestProcess: TOnHttpServerRequest;
    fOnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame;
    fOnWebSocketsClosed: TNotifyEvent;
    procedure SetReceiveTimeout(aReceiveTimeout: integer); override;
  public
    /// low-level client WebSockets connection factory for host and port
    // - calls Open() then WebSocketsUpgrade() for a given protocol
    // - with error interception and optional logging, returning nil on error
    class function WebSocketsConnect(const aHost, aPort: RawUtf8;
      aProtocol: TWebSocketProtocol; aLog: TSynLogClass = nil;
      const aLogContext: RawUtf8 = ''; const aUri: RawUtf8 = '';
      const aCustomHeaders: RawUtf8 = ''; aTls: boolean = false;
      aTLSContext: PNetTlsContext = nil): THttpClientWebSockets; overload;
    /// low-level client WebSockets connection factory for a given URI
    // - would recognize ws://host:port/uri or wss://host:port/uri (over TLS)
    // - calls Open() then WebSocketsUpgrade() for a given protocol
    // - with error interception and optional logging, returning nil on error
    class function WebSocketsConnect(const aUri: RawUtf8;
      aProtocol: TWebSocketProtocol; aLog: TSynLogClass = nil;
      const aLogContext: RawUtf8 = ''; const aCustomHeaders: RawUtf8 = '';
      aTLSContext: PNetTlsContext = nil): THttpClientWebSockets; overload;
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    constructor Create(aTimeOut: PtrInt = 10000); override;
    /// finalize the connection
    destructor Destroy; override;
    /// process low-level REST request, either on HTTP/1.1 or via WebSockets
    // - after WebSocketsUpgrade() call, will use WebSockets for the communication
    function Request(const url, method: RawUtf8; KeepAlive: cardinal;
      const header: RawUtf8; const Data: RawByteString; const DataType: RawUtf8;
      retry: boolean; InStream: TStream = nil; OutStream: TStream = nil): integer; override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - i.e. 'synopsebin' and optionally 'synopsejson' modes
    // - you may specify an URI to as expected by the server for upgrade
    // - if aWebSocketsAjax equals default FALSE, it will register the
    // TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAjax is TRUE, it will register the slower and less secure
    // TWebSocketProtocolJson (to be used for AJAX debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - aWebSocketsEncryptionKey format follows TWebSocketProtocol.SetEncryptKey,
    // so could be e.g. 'password#xxxxxx.private' or 'a=mutual;e=aesctc128;p=34a2..'
    // to use TEcdheProtocol, or a plain password for TProtocolAes
    // - alternatively, you can specify your own custom TWebSocketProtocol
    // instance (owned by this method and immediately released on error)
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions = [pboSynLzCompress];
      aProtocol: TWebSocketProtocol = nil; const aCustomHeaders: RawUtf8 = '';
      aReconnect: boolean = false): RawUtf8;
    /// allow to customize the WebSockets processing
    // - those parameters are accessed by reference to the existing connections
    // so you should better not modify them once the client is upgraded
    function Settings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
    /// this event handler will be executed for any incoming push notification
    property OnCallbackRequestProcess: TOnHttpServerRequest
      read fOnCallbackRequestProcess write fOnCallbackRequestProcess;
    /// event handler trigerred when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
    /// allow low-level interception before
    // TWebSocketProcessClient.ProcessIncomingFrame is executed
    property OnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame
      read fOnBeforeIncomingFrame write fOnBeforeIncomingFrame;
  published
    /// the current WebSockets processing class
    // - equals nil for plain HTTP/1.1 mode
    // - points to the current WebSockets process instance, after a successful
    // WebSocketsUpgrade() call, so that you could use e.g. WebSockets.Protocol
    // to retrieve the protocol currently used
    property WebSockets: TWebSocketProcessClient
      read fProcess;
  end;


function ToText(st: TWebSocketProcessClientThreadState): PShortString; overload;


{ ******************** Socket.IO / Engine.IO Client Protocol over WebSockets }

type
  /// allow to customize TSocketsIOClient process
  // - sciIgnoreUnknownEvent won't raise an ESocketIO when an unregistered event
  // is received
  // - sciIgnoreUnknownAck won't raise an ESocketIO on an expected sioAck message
  // - sciEmitAutoConnect will let Emit() call Connect() if needed
  // - sciAutoReconnect will track any lost connection, and retry to reconnect
  // to the server, eventually re-registering any pre-connected namespaces
  TSocketsIOClientOption = (
    sciIgnoreUnknownEvent,
    sciIgnoreUnknownAck,
    sciEmitAutoConnect,
    sciAutoReconnect);
  /// options to customize TSocketsIOClient process
  TSocketsIOClientOptions = set of TSocketsIOClientOption;

const
  SCI_DEFAULT = [sciEmitAutoConnect, sciAutoReconnect];

type
  TSocketsIOClient = class;
  TWebSocketSocketIOClientProtocol = class;

  TSocketsIOWaitEventPacket = (
    wepNone,
    wepConnect,
    wepEmit);

  TOnSocketsIOClient = procedure(Sender: TSocketsIOClient) of object;

  /// a HTTP/HTTPS client, upgraded to Socket.IO over WebSockets
  // - no polling mode is supported by this class
  // - use Open() class factories to connect to a Socket.IO server and not Create
  // - warning: this class is not thread-safe, and should be used from a single
  // thread, e.g. the main thread of the application, or protected via a Lock
  TSocketsIOClient = class(TEngineIOAbstract)
  protected
    fOwnedClient: THttpClientWebSockets;
    fRemotes: TSocketIORemoteNamespaces;
    fLocals: TSocketIOLocalNamespaces;
    fWaitEvent: TSynEvent;
    fOptions: TSocketsIOClientOptions;
    fDefaultWaitTimeoutSec: cardinal;
    fWaitEventPrepared: TSocketsIOWaitEventPacket;
    fWaitEventAckID: TSocketIOAckID;
    fWaitEventData: PDocVariantData;
    fRemoteNames, fLocalNames: TRawUtf8DynArray;
    fWaitEventParam: RawUtf8;
    fLocalNamespaceClass: TSocketIOLocalNamespaceClass; // customizable
    procedure WaitEventPrepare(Event: TSocketsIOWaitEventPacket);
    procedure WaitEventDone(Event: TSocketsIOWaitEventPacket);
    procedure WaitEventAck(const Message: TSocketIOMessage);
    function GetRemote(NameSpace: PUtf8Char; NameSpaceLen: PtrInt): pointer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    function GetLocal(NameSpace: PUtf8Char; NameSpaceLen: PtrInt;
      const FallbackOnDefault: boolean = false): pointer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    function GetRemote(const NameSpace: RawUtf8): pointer; overload;
    function GetLocal(const NameSpace: RawUtf8; FallbackOnDefault: boolean): pointer; overload;
    // in-place-decode one Engine.IO OPEN payload on client side
    procedure AfterOpen(OpenPayload: PUtf8Char);
    // handle server response after namespace connection request
    procedure AfterNamespaceConnect(const Response: TSocketIOMessage);
    procedure OnEvent(const aMessage: TSocketIOMessage); virtual;
    procedure OnAck(const Message: TSocketIOMessage); virtual;
    function OnReconnect(Process: TWebSocketProcessClient): string; virtual;
    procedure RegisterAgainAfterReconnect; virtual;
  public
    /// callback called just after WebSocketsUpgrade()'s server response
    OnAfterOpen: TOnSocketsIOClient;
    /// callback called when Connect() did receive a successfull response
    // - is also triggered during automated re-connection
    OnNamespaceConnected: procedure(Sender: TSocketsIOClient;
      NameSpace: TSocketIORemoteNamespace) of object;
    /// callback called when an event has been received
    // - should return false to process the event as usual (i.e. from registration)
    // - could return true if this event requires no further processing
    OnReceivedEvent: function(Sender: TSocketsIOClient;
      const Message: TSocketIOMessage): boolean of object;
    /// callback called each time a re-connection attempt failed
    OnLostConnection: TOnSocketsIOClient;
    /// callback called during re-connection, just before WebSocketsUpgrade()
    // - to change e.g. Protocol.UpgradeUri/UpgradeBearerToken
    OnBeforeReconnectUpgrade: procedure (Sender: TSocketsIOClient;
      Protocol: TWebSocketSocketIOClientProtocol) of object;
    /// callback called during re-connection, just before Connect()
    // - to change e.g. Connection.HandhsakeData with a refreshed token
    OnBeforeReconnectConnect: procedure (Sender: TSocketsIOClient;
      var Connection: TSocketIORemoteNamespace) of object;
    /// low-level client WebSockets connection factory for host and port
    // - calls THttpClientWebSockets.WebSocketsConnect for the Socket.IO protocol
    // - with error interception and optional logging, returning nil on error,
    // or a new TSocketsIOClient instance on success
    // - never call the Create constructor, but one of the Open() factory methods
    class function Open(const aHost, aPort: RawUtf8;
      aOptions: TSocketsIOClientOptions = SCI_DEFAULT;
      aLog: TSynLogClass = nil; const aLogContext: RawUtf8 = '';
      const aRoot: RawUtf8 = ''; const aCustomHeaders: RawUtf8 = '';
      aTls: boolean = false; aTLSContext: PNetTlsContext = nil): pointer; overload;
    /// low-level client WebSockets connection factory for host and port
    // - calls THttpClientWebSockets.WebSocketsConnect for the Socket.IO protocol
    // - with error interception and optional logging, returning nil on error,
    // or a new TSocketsIOClient instance on success
    // - would recognize ws://host:port/uri or wss://host:port/uri (over TLS)
    // - if no root UI is supplied, default /socket.io/ will be used
    // - never call the Create constructor, but one of the Open() factory methods
    class function Open(const aUri: RawUtf8; aLog: TSynLogClass = nil;
      aOptions: TSocketsIOClientOptions = SCI_DEFAULT;
      const aLogContext: RawUtf8 = ''; const aCustomHeaders: RawUtf8 = '';
      aTls: boolean = false; aTLSContext: PNetTlsContext = nil): pointer; overload;
    /// finalize this instance and release its associated Client instance
    destructor Destroy; override;
    /// return the array of connected remote namespaces as text
    function RemoteNames: TRawUtf8DynArray;
    /// return the array of registered local namespaces as text
    function LocalNames: TRawUtf8DynArray;
    /// retrieve or register a local event handler class associated with a namespace
    function Local(const NameSpace: RawUtf8 = '/';
      DoNotAddIfNone: boolean = false): TSocketIOLocalNamespace;
    /// register an event with an associated callback for a local namespace
    procedure LocalEvent(const NameSpace, EventName: RawUtf8;
      const Callback: TOnSocketIOEvent);
    /// register all published methods of a class as local namespace handlers
    // - published method names are case-sensitive Socket.IO event names
    // - if no Instance is supplied, will register self published methods
    // - the methods should follow the TOnSocketIOMethod exact signature, i.e.
    // ! function eventname(const Data: TDocVariantData): RawJson;
    procedure LocalPublishedMethods(const Namespace: RawUtf8;
      Instance: TObject = nil);
    /// access to a given Socket.IO namespace
    // - sends a connect message if needed (for first-time registration)
    // - with an optional Data JSON array
    function Connect(const NameSpace: RawUtf8; const Data: RawUtf8 = '';
      WaitTimeoutMS: integer = 0): TSocketIORemoteNamespace;
    /// disconnect from a given Socket.IO namespace
    procedure Disconnect(const NameSpace: RawUtf8);
    /// sends an event to a given remote namespace
    // - with an optional asynchronous acknowledgment callback
    function Emit(const EventName: RawUtf8; const Data: RawUtf8 = '';
      const NameSpace: RawUtf8 = ''; const OnAck: TOnSocketIOAck = nil): TSocketIOAckID; overload;
    /// sends an event to a given remote namespace and wait for its answer
    // - the acknowledged callback JSON array is parsed and used to initialize
    // a TDocVariant array with its content in a synchronous/blocking way
    function Emit(out Dest: TDocVariantData; const EventName: RawUtf8;
      const Data: RawUtf8 = ''; const NameSpace: RawUtf8 = '';
      WaitTimeoutMS: integer = 0): boolean; overload;
    /// refine the TSocketsIOClient process
    property Options: TSocketsIOClientOptions
      read fOptions write fOptions;
    /// how many seconds blocking Connect() and Emit() should wait for its ACK
    // - default is 2 seconds
    property DefaultWaitTimeoutSec: cardinal
      read fDefaultWaitTimeoutSec write fDefaultWaitTimeoutSec;
    /// raw access to the owned associated WebSockets connection
    // - warning: all Request() method are forbidden on this upgraded connection
    property OwnedClient: THttpClientWebSockets
      read fOwnedClient;
    /// raw access to the associated remote name spaces
    property Remotes: TSocketIORemoteNamespaces
      read fRemotes;
    /// raw access to the associated local name spaces and its events
    property Locals: TSocketIOLocalNamespaces
      read fLocals;
  end;

  TSocketsIOClientClass = class of TSocketsIOClient;

  TWebSocketSocketIOClientProtocol = class(TWebSocketSocketIOProtocol)
  protected
    fClient: TSocketsIOClient; // weak reference (owned by TSocketsIOClient)
    procedure AfterUpgrade(aProcess: TWebSocketProcess); override;
    procedure EnginePacketReceived(Sender: TWebSocketProcess; PacketType: TEngineIOPacket;
      PayLoad: PUtf8Char; PayLoadLen: PtrInt; PayLoadBinary: boolean); override;
    // this is the main entry point for incoming Socket.IO messages
    procedure SocketPacketReceived(const Message: TSocketIOMessage); override;
  public
    /// finalize this instance
    destructor Destroy; override;
    /// reuse an existing protocol instance on a new connection
    // - called e.g. by THttpClientWebSockets.WebSocketsUpgrade(aReconnect=true)
    procedure Reset; override;
  end;


function ToText(wep: TSocketsIOWaitEventPacket): PShortString; overload;


implementation


{ ******************** TWebSocketProcessClient Processing Class }

function ToText(st: TWebSocketProcessClientThreadState): PShortString;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessClientThreadState), ord(st));
end;


{ TWebSocketProcessClient }

constructor TWebSocketProcessClient.Create(aSender: THttpClientWebSockets;
  aConnectionID: THttpServerConnectionID; aProtocol: TWebSocketProtocol;
  const aProcessName: RawUtf8);
begin
  // https://tools.ietf.org/html/rfc6455#section-10.3
  // client-to-server masking is mandatory (but not from server to client)
  fMaskSentFrames := FRAME_LEN_MASK;
  fConnectionID := aConnectionID;
  inherited Create(aSender, aProtocol, nil, @aSender.fSettings, aProcessName);
  // initialize the thread after everything is set (Execute may be instant)
  TWebSocketProcessClientThread.Create(self);
  // wait for TWebSocketProcess.ProcessLoop to initiate
  WaitThreadStarted;
end;

destructor TWebSocketProcessClient.Destroy;
var
  t: TWebSocketProcessClientThread;
  tix: Int64;
  {%H-}log: ISynLog;
begin
  t := fOwnerThread as TWebSocketProcessClientThread;
  log := WebSocketLog.Enter('Destroy: ThreadState=%', [ToText(t.fThreadState)^], self);
  try
    // focConnectionClose would be handled in this thread -> close client thread
    t.Terminate;
    tix := GetTickCount64 + 7000; // never wait forever
    while (t.fThreadState = sRun) and
          (GetTickCount64 < tix) do
      SleepHiRes(1);
    t.fProcess := nil;
  finally
    // SendPendingOutgoingFrames + SendFrame/GetFrame(focConnectionClose)
    inherited Destroy;
    t.Free;
  end;
end;

function TWebSocketProcessClient.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
var
  ws: THttpClientWebSockets;
begin
  ws := fSocket as THttpClientWebSockets;
  RequestProcess := ws.fOnCallbackRequestProcess;
  if Assigned(RequestProcess) then
    result := THttpServerRequest.Create(
      nil, 0, fOwnerThread, 0, ws.fProcess.Protocol.ConnectionFlags, nil)
  else
    result := nil;
end;


{ TWebSocketProcessClientThread }

constructor TWebSocketProcessClientThread.Create(aProcess: TWebSocketProcessClient);
begin
  fProcess := aProcess;
  fProcess.fOwnerThread := self;
  inherited Create({suspended=}false); // eventually launch the thread
end;

procedure TWebSocketProcessClientThread.Execute;
var
  log: TSynLog;
  retry: string;
  waitms, maxwaitms: integer;
begin
  if fProcess <> nil then
  try
    fThreadState := sRun;
    maxwaitms := 10000 + Random32(5000); // wait up to 10-15 seconds pace
    SetCurrentThreadName(
      '% % %', [fProcess.fProcessName, self, fProcess.Protocol.Name]);
    repeat
      // main processing loop
      log := WebSocketLog.Add;
      log.Log(sllDebug, 'Execute: before ProcessLoop %', [fProcess], self);
      if not Terminated then
        fProcess.ProcessLoop;
      log.Log(sllDebug, 'Execute: after ProcessLoop % (%)',
        [fProcess, ToText(fProcess.fState)^], self);
      // this connection is finished
      if (fProcess.Socket <> nil) and
         fProcess.Socket.InheritsFrom(THttpClientWebSockets) then
        with THttpClientWebSockets(fProcess.Socket) do
          if Assigned(OnWebSocketsClosed) then
            OnWebSocketsClosed(self);
      if Terminated or
         (fProcess.Socket = nil) or
         not Assigned(fProcess.fOnReconnect) then
        break;
      // try to auto-reconnect to the server
      waitms := 0;
      repeat
        if waitms < maxwaitms then // wait with random increases
          inc(waitms, Random32(200));
        if SleepOrTerminated(waitms) then
          break;
        log.Log(sllDebug,
          'Execute: try reconnect after %ms', [waitms], self);
        retry := fProcess.Socket.ReOpen;
        if Terminated then
          break;
        if retry = '' then
        begin
          log.Log(sllTrace, 'Execute: call OnReconnect', self);
          retry := fProcess.fOnReconnect(fProcess);
          if retry = '' then
            break // successfully reconnected (and also on remote namespaces)
          else
            fProcess.Socket.Close;
        end
        else
          fProcess.fOnReconnect(nil); // notify socket re-open failure
        log.Log(sllTrace, 'Execute: reconnect failed [%]', [retry], self);
      until Terminated;
    until Terminated;
  except
    // ignore any exception in the thread, but end this unsafe client
  end;
  fThreadState := sFinished; // safely set final state
  if (fProcess <> nil) and
     (fProcess.fState = wpsClose) then
    fThreadState := sClosed;
  WebSocketLog.Add.Log(sllDebug, 'Execute: done (%)', [ToText(fThreadState)^], self);
end;


{ ******************** THttpClientWebSockets Bidirectional REST Client }

{ THttpClientWebSockets }

constructor THttpClientWebSockets.Create(aTimeOut: PtrInt);
begin
  inherited;
  fSettings.SetDefaults;
  fSettings.CallbackAnswerTimeOutMS := aTimeOut;
end;

class function THttpClientWebSockets.WebSocketsConnect(
  const aHost, aPort: RawUtf8; aProtocol: TWebSocketProtocol; aLog: TSynLogClass;
  const aLogContext, aUri, aCustomHeaders: RawUtf8;
  aTls: boolean; aTLSContext: PNetTlsContext): THttpClientWebSockets;
var
  error: RawUtf8;
begin
  result := nil;
  if (aProtocol = nil) or
     (aHost = '') then
    EWebSockets.RaiseUtf8('%.WebSocketsConnect(nil)', [self]);
  try
    // call socket constructor
    result := Open(aHost, aPort, nlTcp, 10000, aTls, aTLSContext);
    error := result.WebSocketsUpgrade(
      aUri, '', false, [], aProtocol, aCustomHeaders);
    if error <> '' then
      FreeAndNil(result)
    else if Assigned(aLog) then
      result.OnLog := aLog.DoLog;
  except
    on E: Exception do
    begin
      aProtocol.Free; // as done in WebSocketsUpgrade()
      FreeAndNil(result);
      FormatUtf8('% %', [E, E.Message], error);
    end;
  end;
  if aLog <> nil then
    if result <> nil then
      aLog.Add.Log(sllDebug, '%: WebSocketsConnect %', [aLogContext, result])
    else
      aLog.Add.Log(sllWarning, '%: WebSocketsConnect %:% failed - %',
        [aLogContext, aHost, aPort, error]);
end;

class function THttpClientWebSockets.WebSocketsConnect(const aUri: RawUtf8;
  aProtocol: TWebSocketProtocol; aLog: TSynLogClass;
  const aLogContext, aCustomHeaders: RawUtf8;
  aTLSContext: PNetTlsContext): THttpClientWebSockets;
var
  uri: TUri;
begin
  if (aProtocol = nil) or
     not uri.From(aUri) then
    EWebSockets.RaiseUtf8('%.WebSocketsConnect(nil)', [self]);
  result := WebSocketsConnect(uri.Server, uri.Port, aProtocol,
    aLog, aLogContext, uri.Address, aCustomHeaders, uri.Https, aTLSContext);
end;

destructor THttpClientWebSockets.Destroy;
begin
  FreeAndNil(fProcess);
  inherited;
end;

function THttpClientWebSockets.Request(const url, method: RawUtf8;
  KeepAlive: cardinal; const header: RawUtf8; const Data: RawByteString;
  const DataType: RawUtf8; retry: boolean; InStream, OutStream: TStream): integer;
var
  t: TWebSocketProcessClientThread;
  Ctxt: THttpServerRequest;
  block: TWebSocketProcessNotifyCallback;
  body, resthead: RawUtf8;
begin
  if fProcess = nil then
  begin
    // standard HTTP/1.1 REST request (before WebSocketsUpgrade call)
    result := inherited Request(url, method, KeepAlive, header, Data, DataType,
      retry, InStream, OutStream);
    exit;
  end;
  // WebSocketsUpgrade() did succeed: use the upgraded connection
  t := fProcess.fOwnerThread as TWebSocketProcessClientThread;
  if t.fThreadState = sCreate then
    sleep(10); // paranoid warmup of TWebSocketProcessClientThread.Execute
  if (t.fThreadState <> sRun) or // WebSockets closed by server side
     not fProcess.Protocol.InheritsFrom(TWebSocketProtocolRest) then
  begin
    result := HTTP_CLIENTERROR; // notify client-side error
    exit;
  end;
  // send the REST request over WebSockets - both ends use NotifyCallback()
  Ctxt := THttpServerRequest.Create(nil, fProcess.Protocol.ConnectionID,
    t, 0, fProcess.Protocol.ConnectionFlags, fProcess.Protocol.ConnectionOpaque);
  try
    body := Data;
    if InStream <> nil then
      body := body + StreamToRawByteString(InStream);
    Ctxt.PrepareDirect(url, method, header, body, DataType, '');
    FindNameValue(header, 'SEC-WEBSOCKET-REST:', resthead);
    if resthead = 'NonBlocking' then
      block := wscNonBlockWithoutAnswer
    else
      block := wscBlockWithAnswer;
    result := fProcess.NotifyCallback(Ctxt, block);
    if IdemPChar(pointer(Ctxt.OutContentType), JSON_CONTENT_TYPE_UPPER) then
      HeaderSetText(Ctxt.OutCustomHeaders)
    else
      HeaderSetText(Ctxt.OutCustomHeaders, Ctxt.OutContentType);
    Http.ContentLength := length(Ctxt.OutContent);
    if OutStream <> nil then
      OutStream.WriteBuffer(pointer(Ctxt.OutContent)^, Http.ContentLength)
    else
      Http.Content := Ctxt.OutContent;
    Http.ContentType := Ctxt.OutContentType;
  finally
    Ctxt.Free;
  end;
end;

procedure THttpClientWebSockets.SetReceiveTimeout(aReceiveTimeout: integer);
begin
  inherited SetReceiveTimeout(aReceiveTimeout);
  fSettings.CallbackAnswerTimeOutMS := aReceiveTimeout;
end;

function THttpClientWebSockets.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

{$ifdef ISDELPHI20062007}
  {$warnings off} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function THttpClientWebSockets.WebSocketsUpgrade(
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions;
  aProtocol: TWebSocketProtocol; const aCustomHeaders: RawUtf8;
  aReconnect: boolean): RawUtf8;
var
  key: TAESBlock;
  bin1, bin2: RawByteString;
  extin, extout, expectedprot, supportedprot: RawUtf8;
  extins: TRawUtf8DynArray;
  cmd: RawUtf8;
  id: Int64;
  digest1, digest2: TSha1Digest;
begin
  try
    result := 'Not Connected';
    if not SockConnected then
      exit;
    if not aReconnect then
      if fProcess <> nil then
      begin
        result := 'Already upgraded to WebSockets';
        if PropNameEquals(fProcess.Protocol.Uri, aWebSocketsURI) then
          result := result + ' on this URI'
        else
          result := FormatUtf8('% with URI=[%] but requested [%]',
            [result, fProcess.Protocol.Uri, aWebSocketsURI]);
        exit;
      end;
    try
      // setup the new protocol instance
      if aProtocol = nil then
        if aWebSocketsAjax then
          aProtocol := TWebSocketProtocolJson.Create(aWebSocketsURI)
        else
          aProtocol := TWebSocketProtocolBinary.Create(aWebSocketsURI, false,
            aWebSocketsEncryptionKey, @fSettings, aWebSocketsBinaryOptions);
      aProtocol.OnBeforeIncomingFrame := fOnBeforeIncomingFrame;
      // send initial upgrade request
      RequestSendHeader(aWebSocketsURI, 'GET');
      RandomBytes(@key, SizeOf(key)); // Lecuyer is enough for public random
      bin1 := BinToBase64(@key, SizeOf(key));
      SockSendLine(['Content-Length: 0'#13#10 +
                    'Connection: Upgrade'#13#10 +
                    'Upgrade: websocket'#13#10 +
                    'Sec-WebSocket-Key: ', bin1, #13#10 +
                    'Sec-WebSocket-Version: 13']);
      expectedprot := aProtocol.GetSubprotocols;
      if expectedprot <> '' then
        // this header may be omitted, e.g. by TWebSocketEngineIOProtocol
        SockSendLine(['Sec-WebSocket-Protocol: ', expectedprot]);
      if aProtocol.ProcessHandshake(nil, extout, nil) and
         (extout <> '') then // e.g. for TEcdheProtocol
        SockSendLine(['Sec-WebSocket-Extensions: ', extout]);
      if aCustomHeaders <> '' then
        SockSendHeaders(pointer(aCustomHeaders)); // normalizing CRLF
      SockSendCRLF;
      SockSendFlush('');
      // validate the response as WebSockets upgrade
      SockRecvLn(cmd);
      GetHeader(false);
      if not IdemPChar(pointer(cmd), 'HTTP/1.1 101') then
      begin
        result := cmd;
        if result = '' then
          result := 'No server response';
        exit; // return the unexpected command line as error message
      end;
      result := 'Invalid HTTP Upgrade Header';
      if not (hfConnectionUpgrade in Http.HeaderFlags) or
         (Http.ContentLength > 0) or
         not PropNameEquals(Http.Upgrade, 'websocket') then
        exit;
      result := 'Invalid HTTP Upgrade Accept Challenge';
      ComputeChallenge(bin1, digest1);
      bin2 := HeaderGetValue('SEC-WEBSOCKET-ACCEPT');
      if not Base64ToBin(pointer(bin2), @digest2, length(bin2), SizeOf(digest2)) or
         not IsEqual(digest1, digest2) then
        exit;
      // optionally handle sub-protocols and extensions
      result := 'Invalid HTTP Upgrade Sub-Protocol';
      supportedprot := HeaderGetValue('SEC-WEBSOCKET-PROTOCOL');
      if supportedprot <> '' then // this header may be omitted
        if aProtocol.SetSubprotocol(supportedprot) then
          aProtocol.Name := supportedprot
        else
          exit // unsupported sub-protocol
      else if PosExChar(',', expectedprot) <> 0 then
        exit; // requires to select one given sub-protocol
      if extout <> '' then
      begin
        // process protocol extension (e.g. TEcdheProtocol handshake)
        result := 'Invalid HTTP Upgrade ProcessHandshake';
        extin := HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
        CsvToRawUtf8DynArray(pointer(extin), extins, ';', true);
        if (extins = nil) or
           not aProtocol.ProcessHandshake(extins, extout, @result) then
          exit;
      end;
      // if we reached here, connection is successfully upgraded to WebSockets
      if (Server = 'localhost') or
         (Server = '127.0.0.1') then
      begin
        aProtocol.RemoteIP := '127.0.0.1';
        aProtocol.RemoteLocalhost := true;
      end
      else
        aProtocol.RemoteIP := Server;
      // initialize the TWebSocketProcess
      result := ''; // no error message = success
      SetInt64(pointer(HeaderGetValue('SEC-WEBSOCKET-CONNECTION-ID')), id);
      if fProcess = nil then
        fProcess := TWebSocketProcessClient.Create(self, id, aProtocol, fProcessName)
      else
        fProcess.Reset(id); // from aReconnect
      aProtocol.UpgradeUri := aWebSocketsURI;
      aProtocol.UpgradeBearerToken := aCustomHeaders;
      aProtocol := nil; // protocol instance is owned by fProcess now
    except
      on E: Exception do
      begin
        FreeAndNil(fProcess);
        FormatUtf8('%: %', [E, E.Message], result);
      end;
    end;
  finally
    if not aReconnect then
      aProtocol.Free;
  end;
end;

{$ifdef ISDELPHI20062007}
  {$warnings on}
{$endif ISDELPHI20062007}


{ ******************** Socket.IO / Engine.IO Client Protocol over WebSockets }

function ToText(wep: TSocketsIOWaitEventPacket): PShortString;
begin
  result := GetEnumName(TypeInfo(TSocketsIOWaitEventPacket), ord(wep));
end;


{ TSocketsIOClient }

class function TSocketsIOClient.Open(const aHost, aPort: RawUtf8;
  aOptions: TSocketsIOClientOptions; aLog: TSynLogClass; const aLogContext,
  aRoot, aCustomHeaders: RawUtf8; aTls: boolean; aTLSContext: PNetTlsContext): pointer;
var
  c: THttpClientWebSockets;
  proto: TWebSocketSocketIOClientProtocol;
begin
  result := nil;
  proto := TWebSocketSocketIOClientProtocol.Create('Socket.IO', '');
  proto.fClient := Create;
  proto.fClient.fDefaultWaitTimeoutSec := 2;
  proto.fClient.fOptions := aOptions;
  c := THttpClientWebSockets.WebSocketsConnect(
    aHost, aPort, proto, aLog, aLogContext, EngineIOHandshakeUri(aRoot),
    aCustomHeaders, aTls, aTLSContext);
  if c = nil then
    exit; // WebSocketsConnect() made proto.Free on Open() failure
  proto.fClient.fOwnedClient := c;
  if sciAutoReconnect in aOptions then
    c.fProcess.OnReconnect := proto.fClient.OnReconnect;
  result := proto.fClient;
end;

class function TSocketsIOClient.Open(const aUri: RawUtf8;
  aLog: TSynLogClass; aOptions: TSocketsIOClientOptions; const aLogContext,
  aCustomHeaders: RawUtf8; aTls: boolean; aTLSContext: PNetTlsContext): pointer;
var
  uri: TUri;
begin
  if uri.From(aUri) then // detect both https:// and wss:// schemes
    result := Open(uri.Server, uri.Port, aOptions, aLog, aLogContext,
      uri.Address, aCustomHeaders, aTls, aTLSContext)
  else
    result := nil;
end;

destructor TSocketsIOClient.Destroy;
begin
  ObjArrayClear(fRemotes);
  ObjArrayClear(fLocals);
  fOwnedClient.Free;
  fWaitEvent.Free;
  inherited Destroy;
end;

function TSocketsIOClient.GetRemote(NameSpace: PUtf8Char;
  NameSpaceLen: PtrInt): pointer;
begin
  result := SocketIOGetNameSpace(
    pointer(fRemotes), length(fRemotes), NameSpace, NameSpaceLen);
end;

function TSocketsIOClient.GetLocal(NameSpace: PUtf8Char;
  NameSpaceLen: PtrInt; const FallbackOnDefault: boolean): pointer;
begin
  result := SocketIOGetNameSpace(
    pointer(fLocals), length(fLocals), NameSpace, NameSpaceLen);
  if FallbackOnDefault and
     (result = nil) then
    result := SocketIOGetNameSpace(pointer(fLocals), length(fLocals), '*', 1);
end;

function TSocketsIOClient.GetRemote(const NameSpace: RawUtf8): pointer;
begin
  result := GetRemote(pointer(NameSpace), length(NameSpace));
end;

function TSocketsIOClient.GetLocal(const NameSpace: RawUtf8;
  FallbackOnDefault: boolean): pointer;
begin
  result := GetLocal(pointer(NameSpace), length(NameSpace), FallbackOnDefault);
end;

procedure TSocketsIOClient.AfterOpen(OpenPayload: PUtf8Char);
var
  V: array[0..4] of TValuePUtf8Char;
begin
  JsonDecode(OpenPayload,
    ['sid', 'upgrades', 'pingInterval', 'pingTimeout', 'maxPayload'], @V);
  if V[0].Text = nil then
    EEngineIO.RaiseUtf8('%.Create: missing "sid" in %', [self, OpenPayload]);
  V[0].ToUtf8(fEngineSid);
  if V[1].Text <> nil then
    EEngineIO.RaiseUtf8('%.Create: unsupported "upgrades" in %', [self, OpenPayload]);
  fPingInterval := V[2].ToCardinal(fPingInterval);
  fPingTimeout  := V[3].ToCardinal(fPingTimeout);
  fMaxPayload   := V[4].ToCardinal;
  if Assigned(OnAfterOpen) then
    OnAfterOpen(self);
end;

procedure TSocketsIOClient.AfterNamespaceConnect(const Response: TSocketIOMessage);
var
  ns: TSocketIORemoteNamespace;
begin
  fRemoteNames := nil; // to be reallocated on need
  if Response.PacketType = sioConnect then
  begin
    ns := TSocketIORemoteNamespace.CreateFromConnectMessage(
      Response, fWaitEventParam, self);
    ObjArrayAdd(fRemotes, ns);
    if Assigned(OnNamespaceConnected) then
      OnNamespaceConnected(self, ns);
  end;
  WaitEventDone(wepConnect); // notify any waiting acknowledgement
  if Response.PacketType = sioConnectError then
    Response.RaiseESockIO('Connect() failed with');
end;

procedure TSocketsIOClient.OnEvent(const aMessage: TSocketIOMessage);
var
  ns: TSocketIOLocalNamespace;
begin
  if Assigned(OnReceivedEvent) and
     OnReceivedEvent(self, aMessage) then
    exit; // no further processing
  ns := GetLocal(aMessage.NameSpace, aMessage.NameSpaceLen, {fallback=}true);
  if not Assigned(ns) then
    if sciIgnoreUnknownEvent in fOptions then
      exit
    else
      aMessage.RaiseESockIO('Unknown namespace');
  ns.HandleEvent(aMessage, (sciIgnoreUnknownEvent in fOptions));
end;

procedure TSocketsIOClient.OnAck(const Message: TSocketIOMessage);
var
  ns: TSocketIORemoteNamespace;
begin
  ns := GetRemote(Message.NameSpace, Message.NameSpaceLen);
  if not Assigned(ns) then
    if sciIgnoreUnknownAck in fOptions then
      exit
    else
      Message.RaiseESockIO('ACK on disconnected namespace');
  ns.Acknowledge(Message);
end;

procedure TSocketsIOClient.RegisterAgainAfterReconnect;
var
  i: PtrInt;
  rem: TSocketIORemoteNamespaces;
  r: PSocketIORemoteNamespace;
begin
  try
    // re-Connect() to all remote name spaces
    rem := fRemotes;
    fRemotes := nil;
    r := pointer(rem);
    for i := 1 to length(rem) do
    begin
      // optional user callback
      if Assigned(OnBeforeReconnectConnect) then
        OnBeforeReconnectConnect(self, r^);
      // actual re-connection to this namespace
      Connect(r^.NameSpace, r^.HandshakeData);
      inc(r);
    end;
  except
    if rem <> nil then
      ExchgPointer(@fRemotes, @rem); // restore callbacks on error
  end;
  // remove unneeded registration
  ObjArrayClear(rem);
end;

type
  TWebSocketReconnectClientThread = class(TLoggedThread)
  protected
    fClient: TSocketsIOClient;
    procedure DoExecute; override;
  public
    constructor Create(aClient: TSocketsIOClient); reintroduce;
  end;

constructor TWebSocketReconnectClientThread.Create(aClient: TSocketsIOClient);
begin
  fClient := aClient;
  FreeOnTerminate := true;
  inherited Create({suspended=}false, TSynLog, 'Reconnect');
end;

procedure TWebSocketReconnectClientThread.DoExecute;
begin
  // let the main ProcessLoop be reached
  fClient.fOwnedClient.fProcess.WaitThreadStarted;
  // re-Connect() to all previous remote namespaces
  fClient.RegisterAgainAfterReconnect;
end;

function TSocketsIOClient.OnReconnect(Process: TWebSocketProcessClient): string;
begin
  result := '';
  if Process = nil then // was a reconnection failure
  begin
    // optional user callback
    if Assigned(OnLostConnection) then
      OnLostConnection(self);
    exit;
  end;
  if Process = fOwnedClient.fProcess then // paranoid
  try
    // optional user callback
    if Assigned(OnBeforeReconnectUpgrade) then
      OnBeforeReconnectUpgrade(
        self, Process.Protocol as TWebSocketSocketIOClientProtocol);
    // upgrade the new connection to WebSockets with the previous parameters
    result := string(fOwnedClient.WebSocketsUpgrade(
      Process.Protocol.UpgradeUri, '', {ajax=}false, [],
      Process.Protocol, Process.Protocol.UpgradeBearerToken, {reconnect=}true));
    if result = '' then
      // all the registration should take place in another thread
      TWebSocketReconnectClientThread.Create(self);
  except
    on E: Exception do
      result := E.Message;
  end;
end;

function TSocketsIOClient.Local(const Namespace: RawUtf8;
  DoNotAddIfNone: boolean): TSocketIOLocalNamespace;
begin
  result := GetLocal(NameSpace, {fallback=}false);
  if DoNotAddIfNone or
     (result <> nil) then
    exit;
  if fLocalNamespaceClass = nil then
    fLocalNamespaceClass := TSocketIOLocalNamespace;
  result := fLocalNamespaceClass.Create(self, NameSpace);
  ObjArrayAdd(fLocals, result);
end;

procedure TSocketsIOClient.LocalEvent(
  const NameSpace, EventName: RawUtf8; const Callback: TOnSocketIOEvent);
begin
  if Assigned(Callback) then
    Local(NameSpace).RegisterEvent(EventName, Callback);
end;

procedure TSocketsIOClient.LocalPublishedMethods(
  const Namespace: RawUtf8; Instance: TObject);
begin
  if Instance = nil then
    Instance := self;
  Local(NameSpace).RegisterPublishedMethods(Instance);
end;

function TSocketsIOClient.RemoteNames: TRawUtf8DynArray;
begin
  if fRemoteNames = nil then
    SocketIOGetNameSpaces(pointer(fRemotes), length(fRemotes), fRemoteNames);
  result := fRemoteNames;
end;

function TSocketsIOClient.LocalNames: TRawUtf8DynArray;
begin
  if fLocalNames = nil then
    SocketIOGetNameSpaces(pointer(fLocals), length(fLocals), fLocalNames);
  result := fLocalNames;
end;

procedure TSocketsIOClient.WaitEventDone(Event: TSocketsIOWaitEventPacket);
begin
  if fWaitEventPrepared <> Event then
    exit;
  fWaitEvent.SetEvent;
  fWaitEventPrepared := wepNone;
end;

procedure TSocketsIOClient.WaitEventPrepare(Event: TSocketsIOWaitEventPacket);
begin
  if fWaitEventPrepared <> wepNone then
    ESocketIO.RaiseUtf8('%.WaitEventPrepare(%) with pending %',
      [self, ToText(Event)^, ToText(fWaitEventPrepared)^]);
  fWaitEventPrepared := Event;
  if Assigned(fWaitEvent) then
    fWaitEvent.ResetEvent
  else
    fWaitEvent := TSynEvent.Create;
end;

function TSocketsIOClient.Connect(const NameSpace, Data: RawUtf8;
  WaitTimeoutMS: integer): TSocketIORemoteNamespace;
begin
  result := GetRemote(NameSpace);
  if result <> nil then
    exit; // already connected to this name space
  if fWebSockets = nil then
    ESocketIO.RaiseUtf8('Unexpected %.Connect with no WS connection', [self]);
  if WaitTimeOutMS = 0 then
    WaitTimeoutMS := fDefaultWaitTimeoutSec shl 10;
  if WaitTimeoutMS > 0 then
    WaitEventPrepare(wepConnect);
  fWaitEventParam := Data;
  SocketIOSendPacket(fWebSockets, sioConnect, NameSpace, pointer(Data), length(Data));
  if WaitTimeoutMS <= 0 then
    exit;
  fWaitEvent.WaitFor(WaitTimeoutMS);
  fWaitEventPrepared := wepNone;
  result := GetRemote(NameSpace);
  if result = nil then
    ESocketIO.RaiseUtf8('%.Connect(%,%) failed', [self, NameSpace, WaitTimeoutMS]);
end;

procedure TSocketsIOClient.Disconnect(const NameSpace: RawUtf8);
var
  ns: TSocketIORemoteNamespace;
begin
  ns := GetRemote(NameSpace);
  if ns = nil then
    exit;
  fRemoteNames := nil; // to be reallocated on need
  SocketIOSendPacket(fWebSockets, sioDisconnect, NameSpace);
  ObjArrayDelete(fRemotes, ns);
end;

function TSocketsIOClient.Emit(const EventName, Data, NameSpace: RawUtf8;
  const OnAck: TOnSocketIOAck): TSocketIOAckID;
var
  ns: TSocketIORemoteNamespace;
begin
  if sciEmitAutoConnect in fOptions then
    ns := Connect(NameSpace) // no call needed if already connected
  else
    ns := GetRemote(NameSpace);
  if ns = nil then
    ESocketIO.RaiseUtf8('Unexpected %.Emit(%,%)', [self, EventName, NameSpace]);
  result := ns.SendEvent(EventName, data, OnAck);
end;

procedure TSocketsIOClient.WaitEventAck(const Message: TSocketIOMessage);
var
  dest: PDocVariantData;
begin
  if (fWaitEventPrepared <> wepEmit) or
     (Message.ID <> fWaitEventAckID) then
    exit;
  dest := fWaitEventData;
  if dest <> nil then
    Message.DataGet(dest^);
  WaitEventDone(wepEmit);
  fWaitEventAckID := SIO_NO_ACK;
end;

function TSocketsIOClient.Emit(out Dest: TDocVariantData;
  const EventName, Data, NameSpace: RawUtf8; WaitTimeoutMS: integer): boolean;
begin
  Dest.InitFast;
  if WaitTimeOutMS = 0 then
    WaitTimeoutMS := fDefaultWaitTimeoutSec shl 10;
  if WaitTimeoutMS > 0 then
  begin
    WaitEventPrepare(wepEmit);
    fWaitEventData := @Dest;
  end;
  fWaitEventAckID := Emit(EventName, Data, Namespace, WaitEventAck);
  result := fWaitEventAckID <> SIO_NO_ACK;
  if (WaitTimeoutMS <= 0) or
     not result then
    exit;
  fWaitEvent.WaitFor(WaitTimeoutMS);
  fWaitEventData := nil;
  fWaitEventPrepared := wepNone;
  result := fWaitEventAckID = SIO_NO_ACK;
  fWaitEventAckID := SIO_NO_ACK
end;



{ TWebSocketEngineIOClientProtocol }

procedure TWebSocketSocketIOClientProtocol.AfterUpgrade(aProcess: TWebSocketProcess);
begin
  // need a Socket.IO handler ASAP to handle any incoming frame
  fClient.fWebSockets := aProcess as TWebCrtSocketProcess;
end;

procedure TWebSocketSocketIOClientProtocol.Reset;
begin
  inherited Reset;
  fOpened := false;
end;

destructor TWebSocketSocketIOClientProtocol.Destroy;
begin
  inherited Destroy;
  if fClient.fWebSockets = nil then // not yet owned
    fClient.Free; // may happen during handshake issue
end;

procedure TWebSocketSocketIOClientProtocol.EnginePacketReceived(
  Sender: TWebSocketProcess; PacketType: TEngineIOPacket;
  PayLoad: PUtf8Char; PayLoadLen: PtrInt; PayLoadBinary: boolean);
var
  msg: TSocketIOMessage;
begin
  if fClient = nil then
    ESocketIO.RaiseUtf8('Unexpected %.EnginePacketReceived', [self]);
  case PacketType of
    eioOpen:
      fClient.AfterOpen(PayLoad);
    eioMessage:
      // decode the raw Engine.IO packet into a TSocketIOMessage
      if msg.InitBuffer(PayLoad, PayLoadLen, PayLoadBinary, Sender) then
        SocketPacketReceived(msg)
      else
        ESocketIO.RaiseUtf8('%.EnginePacketReceived: invalid Payload', [self]);
  end;
end;

procedure TWebSocketSocketIOClientProtocol.SocketPacketReceived(
  const Message: TSocketIOMessage);
begin
  if fClient = nil then
    ESocketIO.RaiseUtf8('Unexpected %.SocketPacketReceived', [self]);
  case Message.PacketType of
    sioConnect,
    sioConnectError:
      fClient.AfterNamespaceConnect(Message);
    sioEvent:
      fClient.OnEvent(Message);
    sioAck:
      fClient.OnAck(Message);
  else
    ESocketIO.RaiseUtf8('%.SocketPacketReceived: not supported packet type: %',
      [self, ToText(Message.PacketType)^]);
  end;
end;

end.

