/// REpresentation State Tranfer (REST) HTTP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.http.client;

{
  *****************************************************************************

   Client-Side REST Process over HTTP/WebSockets
    - TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes
    - TRestHttpClientSocket REST Client Class over Sockets
    - TRestHttpClientWebsockets REST Client Class over WebSockets
    - TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes
    - TRestHttpClientCurl REST Client Class over LibCurl
    - TRestHttpClient/TRestHttpClients Main Usable Classes

  *****************************************************************************
}

interface

{.$define NOHTTPCLIENTWEBSOCKETS}
{ if defined, TRestHttpClientWebSockets won't be declared
  - will avoid to link mormot.net.ws.* units }

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.zip,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.soa.core,
  mormot.soa.client,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client,
  mormot.net.sock,
  mormot.net.http,
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  mormot.net.ws.core,
  mormot.net.ws.client,
  {$endif NOHTTPCLIENTWEBSOCKETS}
  mormot.net.client;


{ ************ TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes }

type
  ERestHttpClient = class(ERestException);

  /// available compression algorithms for transmission
  // - SynLZ is faster then Deflate, but not standard: use hcSynLZ for Delphi
  // clients, but hcDeflate for AJAX or any HTTP clients
  // - with hcSynLZ, the 440 KB JSON for TTestClientServerAccess._TRestHttpClient
  // is compressed into 106 KB with no speed penalty (it's even a bit faster)
  // whereas hcDeflate with its level set to 1 (fastest), is 25 % slower
  // - here hcDeflate will use in fact gzip content encoding, since deflate
  // is inconsistent between browsers: http://stackoverflow.com/a/9186091
  // - TRestHttpClientGeneric.Compression default property is [hcSynLZ]
  // - deprecated hcSynShaAes used SHA-256/AES-256-CFB to encrypt the content
  // (after SynLZ compression), but has been audited as weak so HTTPS is to
  // be used instead
  TRestHttpCompression = (
    hcSynLZ,
    hcDeflate
    {$ifndef PUREMORMOT2} , hcSynShaAes {$endif PUREMORMOT2} );

  /// set of available compressions schemes
  TRestHttpCompressions = set of TRestHttpCompression;

  /// abstract HTTP/1.1 RESTful JSON mORMot Client class
  // - this class, and other inherited classes defined in this unit, are
  // thread-safe, since each of their Uri() method is protected by a giant lock
  TRestHttpClientGeneric = class(TRestClientUri)
  protected
    fKeepAliveMS: cardinal;
    fCompression: TRestHttpCompressions;
    /// connection parameters as set by Create()
    fServer, fPort: RawUtf8;
    fHttps: boolean;
    fProxyName, fProxyByPass: RawUtf8;
    fSendTimeout, fReceiveTimeout, fConnectTimeout: cardinal;
    fConnectRetrySeconds: integer; // used by InternalCheckOpen
    fExtendedOptions: THttpRequestExtendedOptions;
    procedure SetCompression(Value: TRestHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
    /// process low-level HTTP/1.1 request
    // - called by InternalUri(), therefore by Uri() public method
    // - returns 200,202,204 if OK, http status error otherwise in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    procedure InternalUri(var Call: TRestUriParams); override;
  public
    /// connect to TRestHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy - note
    // that proxy parameters are currently not available for TRestHttpClientSocket
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms) - if
    // you left the 0 default parameters, it would use global
    // HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT and
    // HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(const aServer, aPort: RawUtf8; aModel: TOrmModel;
      aHttps: boolean = false; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; aSendTimeout: cardinal = 0;
      aReceiveTimeout: cardinal = 0; aConnectTimeout: cardinal = 0);
       reintroduce; overload; virtual;
    /// connect to TRestHttpServer via 'address:port/root' URI format
    // - if port is not specified, aDefaultPort is used
    // - if root is not specified, aModel.Root is used
    constructor Create(const aServer: TRestServerUriString; aModel: TOrmModel;
      aDefaultPort: integer; aHttps: boolean = false); reintroduce; overload;
    /// initialize REST server instance from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// connnect to a LogView HTTP Server for remote logging
    // - will associate the EchoCustom callback of the log class to this server
    // - the aLogClass.Family will manage this TRestHttpClientGeneric instance
    // life time, until application is closed or Family.EchoRemoteStop is called
    constructor CreateForRemoteLogging(const aServer: RawUtf8;
      aLogClass: TSynLogClass; aPort: Integer = 8091;
      const aRoot: RawUtf8 = 'LogService');
    /// save the TRestHttpClientGeneric properties into a persistent storage object
    // - CreateFrom() will expect Definition.ServerName to store the URI as
    // 'server:port' or 'https://server:port', Definition.User/Password to store
    // the TRestClientUri.SetUser() information, and Definition.DatabaseName
    // to store the extended options as an URL-encoded string
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;

    /// returns 'Server:Port' current value
    function HostName: RawUtf8;
    /// optional custom HTTP "User Agent:" header value
    property UserAgent: RawUtf8
      read fExtendedOptions.UserAgent write fExtendedOptions.UserAgent;
  published
    /// the Server IP address
    property Server: RawUtf8
      read fServer;
    /// the Server IP port
    property Port: RawUtf8
      read fPort;
    /// the time (in milliseconds) to keep the connection alive with the
    // TRestHttpServer
    // - default is 20000, i.e. 20 seconds
    property KeepAliveMS: cardinal
      read fKeepAliveMS write SetKeepAliveMS;
    /// the compression algorithms usable with this client
    // - equals [hcSynLZ] by default, since our SynLZ algorithm provides a good
    // compression, with very low CPU use on server side
    // - you may include hcDeflate, which will have a better compression ratio,
    // be recognized by all browsers and libraries, but would consume much
    // more CPU resources than hcSynLZ
    // - if you include hcSynShaAes, it will use SHA-256/AES-256-CFB to encrypt
    // the content (after SynLZ compression), if it is enabled on the server side:
    // ! MyServer := TRestHttpServer.Create('888',[DataBase],'+',useHttpApi,32,secSynShaAes);
    // - for fast and safe communication between stable mORMot nodes, consider
    // using TRestHttpClientWebSockets, leaving hcDeflate for AJAX or non mORMot
    // clients, and hcSynLZ if you expect to have mORMot client(s)
    property Compression: TRestHttpCompressions
      read fCompression write SetCompression;
    /// how many seconds the client may try to connect after open socket failure
    // - is disabled to 0 by default, but you may set some seconds here e.g. to
    // let the server start properly, and let the client handle exceptions to
    // wait and retry until the specified timeout is reached
    // - this property is used only once at startup, then flushed to 0 once connected
    property ConnectRetrySeconds: integer
      read fConnectRetrySeconds write fConnectRetrySeconds;
  end;

  TRestHttpClientGenericClass = class of TRestHttpClientGeneric;


  /// HTTP/1.1 RESTful JSON mORMot Client abstract class using either WinINet,
  // WinHttp or libcurl API
  // - not to be called directly, but via TRestHttpClientWinINet or (even
  // better) TRestHttpClientWinHttp overridden classes under Windows
  TRestHttpClientRequest = class(TRestHttpClientGeneric)
  protected
    fRequest: THttpRequest;
    fRequestClass: THttpRequestClass;
    /// call fWinAPI.Request()
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// set the fWinAPI class
    // - the overridden implementation should set the expected fWinAPIClass
    procedure InternalSetClass; virtual; abstract;
  public
    /// internal class instance used for the connection
    // - will return either a TWinINet, a TWinHttp or a TCurlHttp class instance
    property Request: THttpRequest
      read fRequest;
    /// allows to ignore untrusted SSL certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreSSLCertificateErrors: boolean
      read fExtendedOptions.IgnoreSSLCertificateErrors
      write fExtendedOptions.IgnoreSSLCertificateErrors;
    /// optional Authentication Scheme
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme
      write fExtendedOptions.Auth.Scheme;
    /// optional User Name for Authentication
    property AuthUserName: SynUnicode
      read fExtendedOptions.Auth.UserName
      write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SynUnicode
      read fExtendedOptions.Auth.Password
      write fExtendedOptions.Auth.Password;
  end;

  /// meta-class of TRestHttpClientRequest types
  TRestHttpClientRequestClass = class of TRestHttpClientRequest;


{ ************ TRestHttpClientSocket REST Client Class over Sockets }

  /// HTTP/1.1 RESTful JSON mORMot Client class using SynCrtSock's Sockets
  // - will give the best performance on a local computer, but has been found
  // out to be slower over a network
  // - is not able to use secure HTTPS protocol
  // - note that, in its current implementation, this class is not thread-safe:
  // you need either to lock its access via a critical section, or initialize
  // one client instance per thread
  TRestHttpClientSocket = class(TRestHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocketClass: THttpClientSocketClass;
    /// either THttpClientSocket or THttpClientWebSockets
    fSocket: THttpClientSocket;
    /// call fSocket.Request()
    function InternalRequest(const url, method: RawUtf8;
      var Header, Data, DataType: RawUtf8): Int64Rec; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
  published
    /// internal HTTP/1.1 compatible client
    // - can be used e.g. to access SendTimeout and ReceiveTimeout properties
    property Socket: THttpClientSocket
      read fSocket;
  end;



{ ************ TRestHttpClientWebsockets REST Client Class over WebSockets }

{$ifndef NOHTTPCLIENTWEBSOCKETS}

  /// HTTP/1.1 RESTful JSON mORMot Client able to upgrade to WebSockets
  // - in addition to TRestHttpClientSocket, this client class is able
  // to upgrade its HTTP connection to the WebSockets protocol, so that the
  // server may be able to notify the client via a callback
  // - the internal Socket class will be in fact a THttpClientWebSockets
  // instance, as defined in the mormot.net.ws.client unit
  TRestHttpClientWebsockets = class(TRestHttpClientSocket)
  protected
    fWebSocketParams: record
      AutoUpgrade: boolean;
      Key: RawUtf8;
      Compression: boolean;
      Ajax: boolean;
    end;
    fOnWebSocketsUpgraded: TOnClientNotify;
    fOnWebSocketsClosed: TNotifyEvent;
    fWebSocketLoopDelay: integer;
    fDefaultWebSocketProcessSettings: TWebSocketProcessSettings;
    function InternalCheckOpen: boolean; override;
    function CallbackRequest(
      Ctxt: THttpServerRequestAbstract): cardinal; virtual;
  public
    /// connect to TRestHttpServer on aServer:aPort
    // - this overriden method will handle properly WebSockets settings
    constructor Create(const aServer, aPort: RawUtf8; aModel: TOrmModel;
      aHttps: boolean = false; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; aSendTimeout: cardinal = 0;
      aReceiveTimeout: cardinal = 0; aConnectTimeout: cardinal = 0); override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - the Model.Root URI will be used for upgrade
    // - if aWebSocketsAjax equals default FALSE, it will use 'synopsebinary'
    // i.e. TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAjax is TRUE, it will register the slower and less secure
    // 'synopsejson' mode, i.e. TWebSocketProtocolJson (to be used for AJAX
    // debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - once upgraded, the client would automatically re-upgrade any new
    // HTTP client link on automatic reconnection, so that use of this class
    // should be not tied to a particular TCP/IP socket - use OnWebsocketsUpgraded
    // event to perform any needed initialization set, e.g. SOA real-time
    // callbacks registration
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsCompression: boolean = true): RawUtf8;
    /// connect using a specified WebSockets protocol
    // - this method would call WebSocketsUpgrade, then ServerTimestampSynchronize
    // - it therefore expects SetUser() to have been previously called
    function WebSocketsConnect(const aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsCompression: boolean = true): RawUtf8;
    /// internal HTTP/1.1 and WebSockets compatible client
    // - you could use its properties after upgrading the connection to WebSockets
    function WebSockets: THttpClientWebSockets;
    /// returns true if the connection is a running WebSockets
    // - may be false even if fSocket<>nil, e.g. when gracefully disconnected
    function WebSocketsConnected: boolean;
    /// will set the HTTP header as expected by THttpClientWebSockets.Request to
    // perform the Callback() query in wscNonBlockWithoutAnswer mode
    procedure CallbackNonBlockingSetHeader(out Header: RawUtf8); override;
    /// used to handle an interface parameter as SOA callback
    function FakeCallbackRegister(Sender: TServiceFactory;
      const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
      ParamValue: Pointer): integer; override;
    /// used to finalize an interface parameter as SOA callback
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: integer; Instance: pointer): boolean; override;
    /// this event will be executed just after the HTTP client has been
    // upgraded to the expected WebSockets protocol
    // - supplied Sender parameter will be this TRestHttpClientWebsockets instance
    // - it will be executed the first time, and also on each reconnection
    // occuring when the HTTP-TCP/IP link is re-created, and user re-authenticated
    // - this event handler is the right place to setup link-driven connection,
    // e.g. SOA real-time callbacks registration (using Sender.Services)
    property OnWebSocketsUpgraded: TOnClientNotify
      read fOnWebSocketsUpgraded write fOnWebSocketsUpgraded;
    /// this event handler will be executed when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
    /// customize the internal REST loop delay
    // - to be defined before WebSocketsUpdate/WebSocketsConnect
    // - will set TWebSocketProcessSettings.LoopDelay value at WebSocketsUpgrade
    // - will override LoopDelay from DefaultWebSocketProcessSettings
    property WebSocketLoopDelay: integer
      read fWebSocketLoopDelay write fWebSocketLoopDelay;
    /// returns a reference to default settings for every new WebSocket process
    function DefaultWebSocketProcessSettings: PWebSocketProcessSettings;
      {$ifdef HASINLINE}inline;{$endif}
  end;

{$endif NOHTTPCLIENTWEBSOCKETS}


{ ************ TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes }

{$ifdef USEWININET}

  /// HTTP/1.1 RESTful JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TRestHttpClient using SynCrtSock
  // on a local machine, but was found to be faster throughout local networks
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TRestHttpClientWinINet = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

  /// HTTP/1.1 RESTful JSON Client class using WinHttp API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure HTTPS connection - so it seems to be used
  // in your client programs: TRestHttpClient will therefore map to this class
  // - WinHttp does not share directly any proxy settings with Internet Explorer.
  // The default WinHttp proxy configuration is set by either
  // proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
  // netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
  // the current user's proxy settings for Internet Explorer (under 64 bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinHttp API can be used from a service or a server
  // - is implemented by creating a TWinHttp internal class instance
  TRestHttpClientWinHttp = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

{$endif USEWININET}


{ ************ TRestHttpClientCurl REST Client Class over LibCurl }

  {$ifdef USELIBCURL}
  /// HTTP/1.1 RESTful JSON Client class using libculr
  // - will handle HTTP and HTTPS, if OpenSSL or similar libray is available
  TRestHttpClientCurl = class(TRestHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USELIBCURL}


  { ************ TRestHttpClient/TRestHttpClients Main Usable Classes }

type
  {$ifdef ONLYUSEHTTPSOCKET}

  /// HTTP/1.1 RESTful JSON default mORMot Client class
  // -  maps the raw socket implementation class
  TRestHttpClient = TRestHttpClientSocket;

  {$ifdef USELIBCURL}
  /// HTTP/HTTPS RESTful JSON default mORMot Client class
  TRestHttpsClient = TRestHttpClientCurl;
  {$else}
  {$ifdef USEWININET}
  TRestHttpsClient = TRestHttpClientWinHttp;
  {$else}
  TRestHttpsClient = TRestHttpClientSocket; // (Android) fallback to non-TLS class
  {$endif USEWININET}
  {$endif USELIBCURL}

  {$else ONLYUSEHTTPSOCKET}

  {$ifdef USEWININET}
  /// HTTP/1.1 RESTful JSON default mORMot Client class
  // - under Windows, maps the TRestHttpClientWinHttp class
  TRestHttpClient = TRestHttpClientWinHttp;

  /// HTTP/HTTPS RESTful JSON default mORMot Client class
  // - under Windows, maps the TRestHttpClientWinHttp class, or TRestHttpClientCurl
  // under Linux
  TRestHttpsClient = TRestHttpClientWinHttp;
  {$else}
  TRestHttpClient = TRestHttpClientSocket;
  TRestHttpsClient = TRestHttpClientSocket; // wouls use SChannel if available
  {$endif USEWININET}

  {$endif ONLYUSEHTTPSOCKET}

var
  /// a global hook variable, able to set WebSockets logging to full verbose
  // - checked by TRestHttpClientWebsockets.WebSocketsConnect()
  HttpClientFullWebSocketsLog: boolean;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSqlRestHttpClientWinSock = TRestHttpClientSocket;
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  TSqlRestHttpClientWebsockets = TRestHttpClientWebsockets;
  {$endif NOHTTPCLIENTWEBSOCKETS}
  {$ifdef USEWININET}
  TSqlRestHttpClientWinINet = TRestHttpClientWinINet;
  TSqlRestHttpClientWinHttp = TRestHttpClientWinHttp;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TSqlRestHttpClientCurl = TRestHttpClientCurl;
  {$endif USELIBCURL}

{$endif PUREMORMOT2}


implementation


{ ************ TRestHttpClientGeneric and TRestHttpClientRequest Parent Classes }

{ TRestHttpClientGeneric }

procedure TRestHttpClientGeneric.InternalUri(var Call: TRestUriParams);
var
  Head, Content, ContentType: RawUtf8;
  P, PBeg: PUtf8Char;
  res: Int64Rec;
  log: ISynLog;
begin
  log := fLogClass.Enter('InternalUri %', [Call.Method], self);
  if InternalCheckOpen then
  begin
    Head := Call.InHead;
    Content := Call.InBody;
    ContentType := JSON_CONTENT_TYPE_VAR; // consider JSON by default
    P := pointer(Head);
    while P <> nil do
    begin
      PBeg := P;
      if IdemPChar(PBeg, 'CONTENT-TYPE:') then
      begin
        ContentType := GetNextLine(PBeg + 14, P); // retrieve customized type
        if P = nil then
          // last entry in header
          SetLength(Head, PBeg - pointer(Head))
        else
          system.delete(Head, PBeg - pointer(Head) + 1, P - PBeg);
        Head := TrimU(Head);
        break;
      end;
      P := GotoNextLine(P);
    end;
    if Content <> '' then // always favor content type from binary
      ContentType := GetMimeContentTypeFromBuffer(
        pointer(Content), Length(Content), ContentType);
    fSafe.Enter;
    try
      res := InternalRequest(Call.Url, Call.Method, Head, Content, ContentType);
    finally
      fSafe.Leave;
    end;
    Call.OutStatus := res.Lo;
    Call.OutInternalState := res.Hi;
    Call.OutHead := Head;
    Call.OutBody := Content;
  end
  else
    Call.OutStatus := HTTP_NOTIMPLEMENTED; // 501 indicates not socket closed
  if log <> nil then
    with Call do
      log.Log(sllClient, '% % status=% len=% state=%',
        [method, url, OutStatus, length(OutBody), OutInternalState], self);
end;

procedure TRestHttpClientGeneric.SetCompression(Value: TRestHttpCompressions);
begin
  fCompression := Value;
  InternalClose; // force re-create connection at next request
end;

procedure TRestHttpClientGeneric.SetKeepAliveMS(Value: cardinal);
begin
  fKeepAliveMS := Value;
  InternalClose; // force re-create connection at next request
end;

constructor TRestHttpClientGeneric.Create(const aServer, aPort: RawUtf8;
  aModel: TOrmModel; aHttps: boolean; const aProxyName, aProxyByPass: RawUtf8;
  aSendTimeout, aReceiveTimeout, aConnectTimeout: cardinal);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fHttps := aHttps;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
  fCompression := []; // may add hcSynLZ or hcDeflate for AJAX clients
  if aConnectTimeout = 0 then
    fConnectTimeout := HTTP_DEFAULT_CONNECTTIMEOUT
  else
    fConnectTimeout := aConnectTimeout;
  if aSendTimeout = 0 then
    fSendTimeout := HTTP_DEFAULT_SENDTIMEOUT
  else
    fSendTimeout := aSendTimeout;
  if aReceiveTimeout = 0 then
    fReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT
  else
    fReceiveTimeout := aReceiveTimeout;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
end;

constructor TRestHttpClientGeneric.CreateForRemoteLogging(const aServer: RawUtf8;
  aLogClass: TSynLogClass; aPort: Integer; const aRoot: RawUtf8);
var
  aModel: TOrmModel;
begin
  if not Assigned(aLogClass) then
    raise ERestHttpClient.CreateUtf8(
      '%.CreateForRemoteLogging(LogClass=nil)', [self]);
  aModel := TOrmModel.Create([], aRoot);
  Create(aServer, UInt32ToUtf8(aPort), aModel, aPort = 443);
  aModel.Owner := self;
  ServerRemoteLogStart(aLogClass, true);
  fRemoteLogClass.Log(sllTrace,
    'Echoing to remote server http://%/%/RemoteLog:%', [aServer, aRoot, aPort]);
end;

procedure TRestHttpClientGeneric.DefinitionTo(
  Definition: TSynConnectionDefinition);
begin
  if Definition = nil then
    exit;
  inherited DefinitionTo(Definition); // save Kind + User/Password
  if fHttps then
    Definition.ServerName := 'https://';
  Definition.ServerName := FormatUtf8('%%:%',
    [Definition.ServerName, fServer, fPort]);
  Definition.DatabaseName := UrlEncode([
    'IgnoreSSLCertificateErrors', ord(fExtendedOptions.IgnoreSSLCertificateErrors),
    'ConnectTimeout', fConnectTimeout,
    'SendTimeout', fSendTimeout,
    'ReceiveTimeout', fReceiveTimeout,
    'ProxyName', fProxyName,
    'ProxyByPass', fProxyByPass]);
  Definition.DatabaseName :=
    copy(Definition.DatabaseName, 2, MaxInt); // trim leading '?'
end;

constructor TRestHttpClientGeneric.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
var
  URI: TUri;
  P: PUtf8Char;
  V: cardinal;
  tmp: RawUtf8;
begin
  URI.From(aDefinition.ServerName);
  Create(URI.Server, URI.Port, aModel, URI.Https);
  P := Pointer(aDefinition.DataBaseName);
  while P <> nil do
  begin
    if UrlDecodeCardinal(P, 'CONNECTTIMEOUT', V) then
      fConnectTimeout := V
    else if UrlDecodeCardinal(P, 'SENDTIMEOUT', V) then
      fSendTimeout := V
    else if UrlDecodeCardinal(P, 'RECEIVETIMEOUT', V) then
      fReceiveTimeout := V
    else if UrlDecodeValue(P, 'PROXYNAME', tmp) then
      fProxyName := CurrentAnsiConvert.Utf8ToAnsi(tmp)
    else if UrlDecodeValue(P, 'PROXYBYPASS', tmp) then
      fProxyByPass := CurrentAnsiConvert.Utf8ToAnsi(tmp);
    if UrlDecodeCardinal(P, 'IGNORESSLCERTIFICATEERRORS', V, @P) then
      fExtendedOptions.IgnoreSSLCertificateErrors := boolean(V);
  end;
  inherited RegisteredClassCreateFrom(aModel, aDefinition, false); // call SetUser()
end;

constructor TRestHttpClientGeneric.Create(const aServer: TRestServerUriString;
  aModel: TOrmModel; aDefaultPort: integer; aHttps: boolean);
var
  URI: TRestServerUri;
begin
  URI.Uri := aServer;
  if URI.Root <> '' then
    aModel.Root := URI.Root;
  if (URI.Port = '') and
     (aDefaultPort <> 0) then
    URI.Port := Int32ToUtf8(aDefaultPort);
  Create(URI.Address, URI.Port, aModel, aHttps);
end;

function TRestHttpClientGeneric.HostName: RawUtf8;
begin
  if fServer <> '' then
    if fPort <> '' then
      result := fServer + ':' + fPort
    else
      result := fServer
  else
    result := '';
end;


{ ************ TRestHttpClientSocket REST Client Class over Sockets }


{ TRestHttpClientSocket }

function TRestHttpClientSocket.InternalCheckOpen: boolean;
var
  started, elapsed: Int64;
  wait, retry: integer;
begin
  result := fSocket <> nil;
  if result or
     (isDestroying in fInternalState) then
    exit;
  fSafe.Enter;
  try
    if fSocket = nil then
    begin
      if fSocketClass = nil then
        fSocketClass := THttpClientSocket;
      retry := 0;
      if fConnectRetrySeconds = 0 then
        started := 0
      else
        started := GetTickCount64;
      repeat
        try
          fSocket := fSocketClass.Open(
            fServer, fPort, nlTCP, fConnectTimeout, fHttps);
        except
          on E: Exception do
          begin
            FreeAndNil(fSocket);
            if started = 0 then
              exit;
            elapsed := GetTickCount64 - started;
            if elapsed >= fConnectRetrySeconds shl 10 then
              exit;
            inc(retry);
            if elapsed < 500 then
              wait := 100
            else
              wait := 1000; // checking every second is enough
            fLogClass.Add.Log(sllTrace, 'InternalCheckOpen: % on %:% after %' +
              ' -> wait % and retry #% up to % seconds',
              [E.ClassType, fServer, fPort, MicroSecToString(elapsed * 1000),
               MicroSecToString(wait * 1000), retry, fConnectRetrySeconds],
              self);
            SleepHiRes(wait);
          end;
        end;
      until fSocket <> nil;
      fConnectRetrySeconds := 0; // retry done once at startup
      if fExtendedOptions.UserAgent <> '' then
        fSocket.UserAgent := fExtendedOptions.UserAgent;
      if fModel <> nil then
        fSocket.ProcessName := FormatUtf8('%/%', [fPort, fModel.Root]);
      if fSendTimeout > 0 then
        fSocket.SendTimeout := fSendTimeout;
      if fReceiveTimeout > 0 then
        fSocket.ReceiveTimeout := fReceiveTimeout;
      // note that first registered algo will be the prefered one
      {$ifndef PUREMORMOT2}
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CFB encryption + SynLZ compression
        fSocket.RegisterCompress(CompressShaAes, {CompressMinSize=}0);
      {$endif PUREMORMOT2}
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fSocket.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP gzip compression
        fSocket.RegisterCompress(CompressGZip);
    end;
    result := true;
  finally
    fSafe.Leave;
  end;
end;

procedure TRestHttpClientSocket.InternalClose;
begin
  if fSocket <> nil then
  try
    InternalLog('InternalClose: fSocket.Free', sllTrace);
    FreeAndNil(fSocket);
  except
    ; // ignore any error here
  end;
end;

function TRestHttpClientSocket.InternalRequest(const url, method: RawUtf8;
  var Header, Data, DataType: RawUtf8): Int64Rec;
begin
  fLogFamily.SynLog.Log(sllTrace, 'InternalRequest % calling %(%).Request',
    [method, fSocket.ClassType, pointer(fSocket)], self);
  result.Lo := fSocket.Request(url, method, KeepAliveMS, Header,
    RawByteString(Data), DataType, false);
  result.Hi := fSocket.ServerInternalState;
  Header := fSocket.HeaderGetText;
  Data := fSocket.Content;
  fSocket.Content := ''; // ensure RefCnt=1 to avoid body alloc+copy
end;




{ TRestHttpClientRequest }

function TRestHttpClientRequest.InternalCheckOpen: boolean;
var
  timeout: Int64;
begin
  result := fRequest <> nil;
  if result or
     (isDestroying in fInternalState) then
    exit;
  fSafe.Enter;
  try
    if fRequest = nil then
    begin
      InternalSetClass;
      if fRequestClass = nil then
        raise ERestHttpClient.CreateUtf8('fRequestClass=nil for %', [self]);
      timeout := GetTickCount64 + fConnectRetrySeconds shl 10;
      repeat
        try
          fRequest := fRequestClass.Create(fServer, fPort, fHttps, fProxyName,
            fProxyByPass, fConnectTimeout, fSendTimeout, fReceiveTimeout);
        except
          on E: Exception do
          begin
            FreeAndNil(fRequest);
            if GetTickCount64 >= timeout then
              exit;
            fLogClass.Add.Log(sllTrace,
              'InternalCheckOpen: % on %:% -> wait and retry up to % seconds',
              [E.ClassType, fServer, fPort, fConnectRetrySeconds], self);
            SleepHiRes(250);
          end;
        end;
      until fRequest <> nil;
      fRequest.ExtendedOptions := fExtendedOptions;
      // note that first registered algo will be the prefered one
      {$ifndef PUREMORMOT2}
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CFB encryption + SynLZ compression
        fRequest.RegisterCompress(CompressShaAes, 0); // CompressMinSize=0
      {$endif PUREMORMOT2}
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fRequest.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP zip/deflate compression
        fRequest.RegisterCompress(CompressGZip);
    end;
    result := true;
  finally
    fSafe.Leave;
  end;
end;

procedure TRestHttpClientRequest.InternalClose;
begin
  FreeAndNil(fRequest);
end;

function TRestHttpClientRequest.InternalRequest(const url, method: RawUtf8;
  var Header, Data, DataType: RawUtf8): Int64Rec;
var
  OutHeader: RawUtf8;
  OutData: RawByteString;
begin
  if fRequest = nil then
    result.Lo := HTTP_NOTIMPLEMENTED
  else
  begin
    result.Lo := fRequest.Request(url, method, KeepAliveMS, Header,
      RawByteString(Data), DataType, OutHeader, OutData);
    result.Hi := GetCardinal(
      FindNameValue(pointer(OutHeader), 'SERVER-INTERNALSTATE:'));
    Header := OutHeader;
    Data := OutData;
  end;
end;



{ ************ TRestHttpClientWebsockets REST Client Class over WebSockets }

{$ifndef NOHTTPCLIENTWEBSOCKETS}

{ TRestHttpClientWebsockets }

function TRestHttpClientWebsockets.InternalCheckOpen: boolean;
begin
  result := WebSocketsConnected;
  if result or
     (isDestroying in fInternalState) then
    exit; // already connected
  fSafe.Enter;
  try
    if fSocket = nil then
    try
      if fSocketClass = nil then
        fSocketClass := THttpClientWebSockets;
      InternalLog('InternalCheckOpen: calling %.Open', [fSocketClass]);
      result := inherited InternalCheckOpen;
      if result then
      begin
        include(fInternalState, isOpened);
        with fWebSocketParams do
          if AutoUpgrade then
            result := WebSocketsUpgrade(Key, Ajax, Compression) = '';
      end;
    except
      result := false;
    end;
  finally
    fSafe.Leave;
  end;
end;

function TRestHttpClientWebsockets.FakeCallbackRegister(Sender: TServiceFactory;
  const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
  ParamValue: Pointer): integer;
begin
  if WebSockets = nil then
    raise EServiceException.CreateUtf8('Missing %.WebSocketsUpgrade() call ' +
      'to enable interface parameter callbacks for %.%(%: %)',
      [self, Sender.InterfaceTypeInfo ^.Name, Method.Uri,
       ParamInfo.ParamName^, ParamInfo.ArgTypeName^]);
  if ParamValue = nil then
    result := 0
  else
    result := fFakeCallbacks.DoRegister(ParamValue,
      TInterfaceFactory.Get(ParamInfo.ArgRtti.Info));
end;

function TRestHttpClientWebsockets.FakeCallbackUnregister(
  Factory: TInterfaceFactory; FakeCallbackID: integer;
  Instance: pointer): boolean;
var
  body, head, resp: RawUtf8;
begin
  if (FakeCallbackID = 0) or
     not WebSocketsConnected then
  begin
    result := true; // nothing to notify
    exit;
  end;
  if WebSockets = nil then
    raise EServiceException.CreateUtf8('Missing %.WebSocketsUpgrade() call', [self]);
  FormatUtf8('{"%":%}', [Factory.InterfaceTypeInfo^.RawName, FakeCallbackID], body);
  CallbackNonBlockingSetHeader(head); // frames gathering + no wait
  result := CallBack(mPOST, 'CacheFlush/_callback_', body, resp, nil, 0, @head)
    in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestHttpClientWebsockets.CallbackRequest(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  params: TRestUriParams;
begin
  if (Ctxt = nil) or
     ((Ctxt.InContentType <> '') and
      not IdemPropNameU(Ctxt.InContentType, JSON_CONTENT_TYPE)) then
  begin
    result := HTTP_BADREQUEST;
    exit;
  end;
  params.Init(Ctxt.Url, Ctxt.Method, Ctxt.InHeaders, Ctxt.InContent);
  InternalNotificationMethodExecute(params);
  Ctxt.OutContent := params.OutBody;
  Ctxt.OutCustomHeaders := params.OutHead;
  Ctxt.OutContentType := params.OutBodyType;
  result := params.OutStatus;
end;

constructor TRestHttpClientWebsockets.Create(const aServer, aPort: RawUtf8;
  aModel: TOrmModel; aHttps: boolean; const aProxyName, aProxyByPass: RawUtf8;
  aSendTimeout, aReceiveTimeout, aConnectTimeout: cardinal);
begin
  inherited;
  fDefaultWebSocketProcessSettings.SetDefaults;
end;

function TRestHttpClientWebsockets.
  DefaultWebSocketProcessSettings: PWebSocketProcessSettings;
begin
  if self = nil then
    result := nil
  else
    result := @fDefaultWebSocketProcessSettings;
end;

function TRestHttpClientWebsockets.WebSocketsConnected: boolean;
begin
  result := (self <> nil) and
            (fSocket <> nil) and
            fSocket.InheritsFrom(THttpClientWebSockets) and
            (THttpClientWebSockets(fSocket).WebSockets.State <= wpsRun);
end;

procedure TRestHttpClientWebsockets.CallbackNonBlockingSetHeader(
  out Header: RawUtf8);
begin
  Header := 'Sec-WebSocket-REST: NonBlocking'; // frames gathering + no wait
end;

function TRestHttpClientWebsockets.WebSockets: THttpClientWebSockets;
begin
  if fSocket = nil then
    if not InternalCheckOpen then
    begin
      result := nil;
      exit;
    end;
  result := fSocket as THttpClientWebSockets;
  if not Assigned(result.OnCallbackRequestProcess) then
    result.OnCallbackRequestProcess := CallbackRequest;
  if not Assigned(result.OnWebSocketsClosed) then
    result.OnWebSocketsClosed := OnWebSocketsClosed;
  result.Settings^ := fDefaultWebSocketProcessSettings;
end;

function TRestHttpClientWebsockets.WebSocketsUpgrade(
  const aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsCompression: boolean): RawUtf8;
var
  sockets: THttpClientWebSockets;
  log: ISynLog;
begin
  log := fLogFamily.SynLog.Enter(self, 'WebSocketsUpgrade');
  sockets := WebSockets;
  if sockets = nil then
    result := 'Impossible to connect to the Server'
  else
  begin
    if fWebSocketLoopDelay > 0 then
      sockets.Settings^.LoopDelay := fWebSocketLoopDelay;
    result := sockets.WebSocketsUpgrade(Model.Root, aWebSocketsEncryptionKey,
      aWebSocketsAjax, aWebSocketsCompression);
    if result = '' then
      // no error message = success
      with fWebSocketParams do
      begin
        // store parameters for auto-reconnection
        AutoUpgrade := true;
        Key := aWebSocketsEncryptionKey;
        Compression := aWebSocketsCompression;
        Ajax := aWebSocketsAjax;
        if Assigned(fOnWebSocketsUpgraded) then
          fOnWebSocketsUpgraded(self);
      end;
  end;
  if log <> nil then
    if result <> '' then
      log.Log(sllWarning, '[%] error upgrading %', [result, sockets], self)
    else
      log.Log(sllHTTP, 'HTTP link upgraded to WebSockets using %',
        [sockets], self);
end;

function TRestHttpClientWebsockets.WebSocketsConnect(
  const aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsCompression: boolean): RawUtf8;
begin
  if WebSockets = nil then
    result := 'WebSockets=nil'
  else
  begin
    if HttpClientFullWebSocketsLog then
      WebSockets.Settings.SetFullLog;
    result := WebSocketsUpgrade(aWebSocketsEncryptionKey, aWebSocketsAjax,
      aWebSocketsCompression);
    if result = '' then
      if not ServerTimestampSynchronize then
        result := 'ServerTimestampSynchronize';
  end;
  if result <> '' then
    raise ERestHttpClient.CreateUtf8('%.WebSocketsConnect failed on %:%/% -> %',
      [self, Server, Port, Model.Root, result]);
end;

{$endif NOHTTPCLIENTWEBSOCKETS}



{ ************ TRestHttpClientWinINet TRestHttpClientWinHttp Windows REST Client Classes }

{$ifdef USEWININET}

{ TRestHttpClientWinINet }

procedure TRestHttpClientWinINet.InternalSetClass;
begin
  fRequestClass := TWinINet;
  inherited;
end;


{ TRestHttpClientWinHttp }

procedure TRestHttpClientWinHttp.InternalSetClass;
begin
  fRequestClass := TWinHttp;
  inherited;
end;

{$endif USEWININET}



{ ************ TRestHttpClientCurl REST Client Class over LibCurl }

{$ifdef USELIBCURL}

{ TRestHttpClientCurl}

procedure TRestHttpClientCurl.InternalSetClass;
begin
  fRequestClass := TCurlHttp;
  inherited;
end;

{$endif USELIBCURL}

initialization
  TRestHttpClientSocket.RegisterClassNameForDefinition;
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  TRestHttpClientWebsockets.RegisterClassNameForDefinition;
  {$endif NOHTTPCLIENTWEBSOCKETS}
  {$ifdef USEWININET}
  TRestHttpClientWinINet.RegisterClassNameForDefinition;
  TRestHttpClientWinHttp.RegisterClassNameForDefinition;
  {$endif USEWININET}
  {$ifdef USELIBCURL}
  TRestHttpClientCurl.RegisterClassNameForDefinition;
  {$endif USELIBCURL}

end.

