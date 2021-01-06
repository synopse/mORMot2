/// REpresentation State Tranfer (REST) HTTP Server
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.http.server;

{
  *****************************************************************************

   Server-Side REST Process over HTTP/WebSockets
    - TRestHttpServer RESTful Server
    - TRestHttpRemoteLogServer to Receive Remote Log Stream

  *****************************************************************************
}

interface

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
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.core.zip,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.soa.core,
  mormot.soa.Server,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.net.http,
  mormot.net.sock,
  mormot.net.server,
  mormot.net.ws.core,
  mormot.net.ws.server;


{ ************ TRestHttpServer RESTful Server }

type
  /// exception raised in case of a HTTP Server error
  ERestHttpServer = class(ERestException);

  /// available running options for TRestHttpServer.Create() constructor
  // - useHttpApi to run kernel-mode HTTP.SYS server (THttpApiServer) with an
  // already registered URI (default way, similar to IIS/WCF security policy
  // as specified by Microsoft) - you would need to register the URI by hand,
  // e.g. in the Setup program, via code similar to this one:
  // ! THttpApiServer.AddUrlAuthorize('root','888',false,'+'))
  // - useHttpApiRegisteringURI will first registry the given URI, then run
  // kernel-mode HTTP.SYS server (THttpApiServer) - will need Administrator
  // execution rights at least one time (e.g. during setup); note that if
  // the URI is already registered, the server will still be launched, even if
  // the program does not run as Administrator - it is therefore sufficient
  // to run such a program once as Administrator to register the URI, when this
  // useHttpApiRegisteringURI option is set
  // - useHttpSocket will run the standard Sockets library (i.e. socket-based
  // THttpServer) - it will trigger the Windows firewall popup UAC window at
  // first execution
  // - useBidirSocket will use the standard Sockets library but via the
  // TWebSocketServerRest class, allowing HTTP connection upgrade to the
  // WebSockets protocol, to enable immediate event callbacks in addition to
  // the standard request/answer RESTful mode
  // - the first item should be the preferred one (see HTTP_DEFAULT_MODE)
  TRestHttpServerUse = (
    {$ifndef ONLYUSEHTTPSOCKET}
    useHttpApi,
    useHttpApiRegisteringURI,
    {$endif ONLYUSEHTTPSOCKET}
    useHttpSocket,
    useBidirSocket);

  /// available security options for TRestHttpServer.Create() constructor
  // - default secNone will use plain HTTP connection
  // - secSSL will use HTTPS secure connection
  // - secSynShaAes will use our proprietary SHA-256 / AES-256-CTR encryption
  // identified as 'synshaaes' as ACCEPT-ENCODING: header parameter
  TRestHttpServerSecurity = (
    secNone,
    secSSL,
    secSynShaAes);

const
  /// the default access rights used by the HTTP server if none is specified
  HTTP_DEFAULT_ACCESS_RIGHTS: POrmAccessRights = @SUPERVISOR_ACCESS_RIGHTS;

  /// the kind of HTTP server to be used by default
  // - will define the best available server class, depending on the platform
  HTTP_DEFAULT_MODE =
    {$ifdef ONLYUSEHTTPSOCKET}
    useHttpSocket
    {$else}
    useHttpApiRegisteringURI
    {$endif ONLYUSEHTTPSOCKET};


type
  /// HTTP/1.1 RESTFUL JSON mORMot Server class
  // - this server is multi-threaded and not blocking
  // - under Windows, it will first try to use fastest http.sys kernel-mode
  // server (i.e. create a THttpApiServer instance); it should work OK under XP
  // or WS 2K3 - but you need to have administrator rights under Vista or Seven:
  // if http.sys fails to initialize, it will use the socket-based THttpServer;
  // a solution is to call the THttpApiServer.AddUrlAuthorize class method during
  // program setup for the desired port, or define a useHttpApiRegisteringURI
  // kind of server, in order to allow it for every user
  // - under Linux, only THttpServer is available
  // - you can specify useBidirSocket kind of server (i.e. TWebSocketServerRest)
  // if you want the HTTP protocol connection to be upgraded to a WebSockets
  // mode, to allow immediate callbacks from the server to the client
  // - just create it and it will serve SQL statements as UTF-8 JSON
  // - for a true AJAX server, expanded data is prefered - your code may contain:
  // ! DBServer.NoAjaxJson := false;
  TRestHttpServer = class(TSynPersistentLock)
  protected
    fOnlyJsonRequests: boolean;
    fShutdownInProgress: boolean;
    fHttpServer: THttpServerGeneric;
    fPort, fDomainName: RawUtf8;
    fPublicAddress, fPublicPort: RawUtf8;
    /// internal servers to compute responses (protected by inherited fSafe)
    fDBServers: array of record
      Server: TRestServer;
      RestAccessRights: POrmAccessRights;
      Security: TRestHttpServerSecurity;
    end;
    fHosts: TSynNameValue;
    fAccessControlAllowOrigin: RawUtf8;
    fAccessControlAllowOriginsMatch: TMatchs;
    fAccessControlAllowCredential: boolean;
    fRootRedirectToURI: array[boolean] of RawUtf8;
    fRedirectServerRootUriForExactCase: boolean;
    fUse: TRestHttpServerUse;
    fLog: TSynLogClass;
    procedure SetAccessControlAllowOrigin(const Value: RawUtf8);
    procedure ComputeAccessControlHeader(Ctxt: THttpServerRequestAbstract);
    // assigned to fHttpServer.OnHttpThreadStart/Terminate e.g. to handle connections
    procedure HttpThreadStart(Sender: TThread); virtual;
    procedure HttpThreadTerminate(Sender: TThread); virtual;
    // implement the server response - must be thread-safe
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    function GetDBServerCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
    function GetDBServer(Index: Integer): TRestServer;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetDBServerAccessRight(Index: integer; Value: POrmAccessRights);
    procedure SetDBServer(aIndex: integer; aServer: TRestServer;
      aSecurity: TRestHttpServerSecurity; aRestAccessRights: POrmAccessRights);
    function GetDBServerNames: RawUtf8;
    function HttpApiAddUri(const aRoot, aDomainName: RawByteString;
      aSecurity: TRestHttpServerSecurity;
      aRegisterUri, aRaiseExceptionOnError: boolean): RawUtf8;
    function NotifyCallback(aSender: TRestServer;
      const aInterfaceDotMethodName, aParams: RawUtf8;
      aConnectionID: THttpServerConnectionID;
      aFakeCallID: integer; aResult, aErrorMsg: PRawUtf8): boolean;
  public
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a ERestHttpServer exception if binding failed
    // - specify one or more TRestServer server class to be used: each
    // class must have an unique Model.Root value, to identify which TRestServer
    // instance must handle a particular request from its URI
    // - port is an RawUtf8/AnsiString, as expected by the WinSock API - in case
    // of useHttpSocket or useBidirSocket kind of server, you should specify the
    // public server address to bind to: e.g. '1.2.3.4:1234' - even for http.sys,
    // the public address could be used e.g. for TRestServer.SetPublicUri()
    // - aDomainName is the Urlprefix to be used for HttpAddUrl API call:
    // it could be either a fully qualified case-insensitive domain name
    // an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port) - this
    // parameter is ignored by the TRestHttpApiServer instance
    // - aHttpServerKind defines how the HTTP server itself will be implemented:
    // it will use by default optimized kernel-based http.sys server (useHttpApi),
    // optionally registering the URI (useHttpApiRegisteringURI) if needed,
    // or using the standard Sockets library (useHttpSocket), possibly in its
    // WebSockets-friendly version (useBidirSocket - you shoud call the
    // WebSocketsEnable method to initialize the available protocols)
    // - by default, the POrmAccessRights will be set to nil
    // - the aThreadPoolCount parameter will set the number of threads
    // to be initialized to handle incoming connections (default is 32, which
    // may be sufficient for most cases, maximum is 256)
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalUrl parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TRestHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    // - for THttpServer, you can force aHeadersUnFiltered flag
    constructor Create(const aPort: RawUtf8;
      const aServers: array of TRestServer; const aDomainName: RawUtf8 = '+';
      aUse: TRestHttpServerUse = HTTP_DEFAULT_MODE;
      aThreadPoolCount: Integer = 32;
      aSecurity: TRestHttpServerSecurity = secNone;
      const aAdditionalUrl: RawUtf8 = '';
      const aQueueName: SynUnicode = '';
      aHeadersUnFiltered: boolean = false); reintroduce; overload;
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a ERestHttpServer exception if binding failed
    // - specify one TRestServer server class to be used
    // - port is an RawUtf8, as expected by the WinSock API - in case of
    // useHttpSocket or useBidirSocket kind of server, you can specify the
    // public server address to bind to: e.g. '1.2.3.4:1234' - even for http.sys,
    // the public address could be used e.g. for TRestServer.SetPublicUri()
    // - aDomainName is the Urlprefix to be used for HttpAddUrl API call
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalUrl parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TRestHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    constructor Create(const aPort: RawUtf8; aServer: TRestServer;
      const aDomainName: RawUtf8 = '+';
      aUse: TRestHttpServerUse = HTTP_DEFAULT_MODE;
      aRestAccessRights: POrmAccessRights = nil;
      aThreadPoolCount: Integer = 32;
      aSecurity: TRestHttpServerSecurity = secNone;
      const aAdditionalUrl: RawUtf8 = '';
      const aQueueName: SynUnicode = ''); reintroduce; overload;
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a ERestHttpServer exception if binding failed
    // - specify one TRestServer instance to be published, and the associated
    // transmission definition; other parameters would be the standard one
    // - only the supplied aDefinition.Authentication will be defined
    // - under Windows, will use http.sys with automatic URI registration, unless
    // aDefinition.WebSocketPassword is set and binary WebSockets would be
    // expected with the corresponding encryption, or aForcedKind is overriden
    // - optional aWebSocketsLoopDelay parameter could be set for tuning
    // WebSockets responsiveness
    constructor Create(aServer: TRestServer;
      aDefinition: TRestHttpServerDefinition;
      aForcedUse: TRestHttpServerUse = HTTP_DEFAULT_MODE;
      aWebSocketsLoopDelay: integer = 0); reintroduce; overload;
    /// release all memory, internal mORMot server and HTTP handlers
    destructor Destroy; override;
    /// you can call this method to prepare the HTTP server for shutting down
    // - it will call all associated TRestServer.Shutdown methods, unless
    // noRestServerShutdown is true
    // - note that Destroy won't call this method on its own, since the
    // TRestServer instances may have a life-time uncoupled from HTTP process
    procedure Shutdown(noRestServerShutdown: boolean = false);
    /// try to register another TRestServer instance to the HTTP server
    // - each TRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - an optional aRestAccessRights parameter is available to override the
    // default HTTP_DEFAULT_ACCESS_RIGHTS access right setting - but you shall
    // better rely on the authentication feature included in the framework
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - return true on success, false on error (e.g. duplicated Root value)
    function AddServer(aServer: TRestServer;
      aRestAccessRights: POrmAccessRights = nil;
      aSecurity: TRestHttpServerSecurity = secNone): boolean;
    /// un-register a TRestServer from the HTTP server
    // - each TRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - return true on success, false on error (e.g. specified server not found)
    function RemoveServer(aServer: TRestServer): boolean;
    /// register a domain name to be redirected to a given Model.Root
    // - i.e. can be used to support some kind of virtual hosting
    // - by default, the URI would be used to identify which TRestServer
    // instance to use, and the incoming HOST value would just be ignored
    // - you can specify here domain names which would be checked against
    // the incoming HOST header, to redirect to a given URI, as such:
    // ! DomainHostRedirect('project1.com','root1');
    // ! DomainHostRedirect('project2.com','root2');
    // ! DomainHostRedirect('blog.project2.com','root2/blog');
    // for the last entry, you may have for instance initialized a MVC web
    // server on the 'blog' sub-URI of the 'root2' TRestServer via:
    // !constructor TMyMvcApplication.Create(aRestModel: TRest; aInterface: PTypeInfo);
    // ! ...
    // ! fMainRunner := TMvcRunOnRestServer.Create(self,nil,'blog');
    // ! ...
    // - if aUri='' is given, the corresponding host redirection will be disabled
    // - note: by design, 'something.localhost' is likely to be not recognized
    // as aDomain, since 'localhost' can not be part of proper DNS resolution
    procedure DomainHostRedirect(const aDomain, aUri: RawUtf8);
    /// allow to temporarly redirect ip:port root URI to a given sub-URI
    // - by default, only sub-URI, as defined by TRestServer.Model.Root, are
    // registered - you can define here a sub-URI to reach when the main server
    // is directly accessed from a browser, e.g. localhost:port will redirect to
    // localhost:port/RedirectedUri
    // - for http.sys server, would try to register '/' if aRegisterUri is TRUE
    // - by default, will redirect http://localhost:port unless you set
    // aHttpServerSecurity=secSSL so that it would redirect https://localhost:port
    procedure RootRedirectToUri(const aRedirectedUri: RawUtf8;
      aRegisterUri: boolean = true; aHttps: boolean = false);
    /// defines the WebSockets protocols to be used for useBidirSocket
    // - i.e. 'synopsebinary' and optionally 'synopsejson' protocols
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single REST server
    // - TWebSocketProtocolBinary will always be registered by this method
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAjax is TRUE, it will also register TWebSocketProtocolJson
    // so that AJAX applications would be able to connect to this server
    // - this method does nothing if the associated HttpServer class is not a
    // TWebSocketServerRest (i.e. this instance was not created as useBidirSocket)
    function WebSocketsEnable(
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsCompressed: boolean = true): TWebSocketServerRest; overload;
    /// defines the useBidirSocket WebSockets protocol to be used for a REST server
    // - same as the overloaded WebSocketsEnable() method, but the URI will be
    // forced to match the aServer.Model.Root value, as expected on the client
    // side by TRestHttpClientWebsockets.WebSocketsUpgrade()
    function WebSocketsEnable(aServer: TRestServer;
      const aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean = false;
      aWebSocketsCompressed: boolean = true): TWebSocketServerRest; overload;
    /// the associated running HTTP server instance
    // - either THttpApiServer (available only under Windows), THttpServer or
    // TWebSocketServerRest (on any system)
    property HttpServer: THttpServerGeneric
      read fHttpServer;
    /// the TCP/IP (address and) port on which this server is listening to
    // - may contain the public server address to bind to: e.g. '1.2.3.4:1234'
    // - see PublicAddress and PublicPort properties if you want to get the
    // true IP port or address
    property Port: RawUtf8
      read fPort;
    /// the TCP/IP public address on which this server is listening to
    // - equals e.g. '1.2.3.4' if Port = '1.2.3.4:1234'
    // - if Port does not contain an explicit address (e.g. '1234'), the current
    // computer host name would be assigned as PublicAddress
    property PublicAddress: RawUtf8
      read fPublicAddress;
    /// the TCP/IP public port on which this server is listening to
    // - equals e.g. '1234' if Port = '1.2.3.4:1234'
    property PublicPort: RawUtf8
      read fPublicPort;
    /// the Urlprefix used for internal HttpAddUrl API call
    property DomainName: RawUtf8
      read fDomainName;
    /// read-only access to the number of registered internal servers
    property DBServerCount: integer
      read GetDBServerCount;
    /// read-only access to all internal servers
    property DBServer[Index: integer]: TRestServer
      read GetDBServer;
    /// write-only access to all internal servers access right
    // - can be used to override the default HTTP_DEFAULT_ACCESS_RIGHTS setting
    property DBServerAccessRight[Index: integer]: POrmAccessRights
      write SetDBServerAccessRight;
    /// find the first instance of a registered REST server
    // - note that the same REST server may appear several times in this HTTP
    // server instance, e.g. with diverse security options
    function DBServerFind(aServer: TRestServer): integer;
    /// set this property to TRUE if the server must only respond to
    // request of MIME type APPLICATION/JSON
    // - the default is false, in order to allow direct view of JSON from
    // any browser
    property OnlyJsonRequests: boolean
      read fOnlyJsonRequests write fOnlyJsonRequests;
    /// enable cross-origin resource sharing (CORS) for proper AJAX process
    // - see @https://developer.mozilla.org/en-US/docs/HTTP/Access_control_CORS
    // - can be set e.g. to '*' to allow requests from any site/domain; or
    // specify an Csv white-list of URI to be allowed as origin e.g. as
    // 'https://foo.example1,https://foo.example2' or 'https://*.foo.example' or
    // (faster) '*.foo.example1,*.foo.example2' following the TMatch syntax
    // - see also AccessControlAllowCredential property
    property AccessControlAllowOrigin: RawUtf8
      read fAccessControlAllowOrigin write SetAccessControlAllowOrigin;
    /// enable cookies, authorization headers or TLS client certificates CORS exposition
    // - this option works with the AJAX XMLHttpRequest.withCredentials property
    // on client/JavaScript side, as stated by
    // @https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials
    // - see @https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials
    property AccessControlAllowCredential: boolean
      read fAccessControlAllowCredential write fAccessControlAllowCredential;
    /// enable redirectoin to fix any URI for a case-sensitive match of Model.Root
    // - by default, TRestServer.Model.Root would be accepted with case
    // insensitivity; but it may induce errors for HTTP cookies, since they
    // are bound with '; Path=/ModelRoot', which is case-sensitive on the
    // browser side
    // - set this property to TRUE so that only exact case URI would be handled
    // by TRestServer.Uri(), and any case-sensitive URIs (e.g. /Root/... or
    // /ROOT/...) would be temporary redirected to Model.Root (e.g. /root/...)
    // via a HTTP 307 command
    property RedirectServerRootUriForExactCase: boolean
      read fRedirectServerRootUriForExactCase
      write fRedirectServerRootUriForExactCase;
  end;

var
  /// a global hook variable, able to enhance WebSockets logging
  // - when a TRestHttpServer is created from a TRestHttpServerDefinition
  HttpServerFullWebSocketsLog: boolean;


function ToText(use: TRestHttpServerUse): PShortString; overload;
function ToText(sec: TRestHttpServerSecurity): PShortString; overload;



{ ************ TRestHttpRemoteLogServer to Receive Remote Log Stream }

type
  /// callback expected by TRestHttpRemoteLogServer to notify about a received log
  TRemoteLogReceivedOne = procedure(const Text: RawUtf8) of object;

  {$M+}
  /// limited HTTP server which is will receive remote log notifications
  // - this will create a simple in-memory mORMot server, which will trigger
  // a supplied callback when a remote log is received
  // - see TRestHttpClientWinGeneric.CreateForRemoteLogging() for the client side
  // - used e.g. by the LogView tool
  TRestHttpRemoteLogServer = class(TRestHttpServer)
  protected
    fServer: TRestServerFullMemory;
    fEvent: TRemoteLogReceivedOne;
  public
    /// initialize the HTTP server and an internal mORMot server
    // - you can share several HTTP log servers on the same port, if you use
    // a dedicated root URI and use the http.sys server (which is the default)
    constructor Create(const aRoot: RawUtf8; aPort: integer;
      const aEvent: TRemoteLogReceivedOne); reintroduce;
    /// release the HTTP server and its internal mORMot server
    destructor Destroy; override;
    /// the associated mORMot server instance running with this HTTP server
    property Server: TRestServerFullMemory
      read fServer;
  published
    /// this HTTP server will publish a 'RemoteLog' method-based service
    // - expecting PUT with text as body, at http://server/root/RemoteLog
    procedure RemoteLog(Ctxt: TRestServerUriContext);
  end;
  {$M-}


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

  TSQLHTTPServerOptions = TRestHttpServerUse;
  TSQLHTTPServerSecurity = TRestHttpServerSecurity;
  TSQLHTTPServer = TRestHttpServer;
  TSQLHTTPRemoteLogServer = TRestHttpRemoteLogServer;

{$endif PUREMORMOT2}


implementation


{ ************ TRestHttpServer RESTful Server }

function ToText(use: TRestHttpServerUse): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestHttpServerUse), ord(use));
end;

function ToText(sec: TRestHttpServerSecurity): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestHttpServerSecurity), ord(sec));
end;

{ TRestHttpServer }

function TRestHttpServer.AddServer(aServer: TRestServer;
  aRestAccessRights: POrmAccessRights;
  aSecurity: TRestHttpServerSecurity): boolean;
var
  i, n: PtrInt;
  log: ISynLog;
begin
  result := False;
  if (self = nil) or
     (aServer = nil) or
     (aServer.Model = nil) then
    exit;
  log := fLog.Enter(self, 'AddServer');
  fSafe.Lock; // protect fDBServers[]
  try
    n := length(fDBServers);
    for i := 0 to n - 1 do
      if (fDBServers[i].Server.Model.UriMatch(aServer.Model.Root) <> rmNoMatch) and
        (fDBServers[i].Security = aSecurity) then
        exit; // register only once per URI Root address and per protocol
    {$ifndef ONLYUSEHTTPSOCKET}
    if fUse in [useHttpApi, useHttpApiRegisteringURI] then
      if HttpApiAddUri(aServer.Model.Root, fDomainName, aSecurity,
          fUse = useHttpApiRegisteringURI, false) <> '' then
        exit;
    {$endif ONLYUSEHTTPSOCKET}
    SetLength(fDBServers, n + 1);
    SetDBServer(n, aServer, aSecurity, aRestAccessRights);
    fHttpServer.ProcessName := GetDBServerNames;
    result := true;
  finally
    fSafe.UnLock;
    if log <> nil then
      log.Log(sllHttp, 'AddServer(%,Root=%,Port=%,Public=%:%)=%',
        [aServer, aServer.Model.Root, fPort, fPublicAddress, fPublicPort,
         BOOL_STR[result]], self);
  end;
end;

function TRestHttpServer.DBServerFind(aServer: TRestServer): integer;
begin
  fSafe.Lock; // protect fDBServers[]
  try
    for result := 0 to Length(fDBServers) - 1 do
      if fDBServers[result].Server = aServer then
        exit;
    result := -1;
  finally
    fSafe.UnLock;
  end;
end;

function TRestHttpServer.RemoveServer(aServer: TRestServer): boolean;
var
  i, j, n: PtrInt;
  log: ISynLog;
begin
  result := False;
  if (self = nil) or
     (aServer = nil) or
     (aServer.Model = nil) then
    exit;
  log := fLog.Enter(self, 'RemoveServer');
  fSafe.Lock; // protect fDBServers[]
  try
    n := high(fDBServers);
    for i := n downto 0 do // may appear several times, with another Security
      if fDBServers[i].Server = aServer then
      begin
      {$ifndef ONLYUSEHTTPSOCKET}
        if fHttpServer.InheritsFrom(THttpApiServer) then
          if THttpApiServer(fHttpServer).RemoveUrl(aServer.Model.Root,
             fPublicPort, fDBServers[i].Security = secSSL,
             fDomainName) <> NO_ERROR then
            log.Log(sllLastError, '%.RemoveUrl(%)',
              [self, aServer.Model.Root], self);
      {$endif ONLYUSEHTTPSOCKET}
        for j := i to n - 1 do
          fDBServers[j] := fDBServers[j + 1];
        SetLength(fDBServers, n);
        dec(n);
        aServer.OnNotifyCallback := nil;
        aServer.SetPublicUri('', '');
        result := true; // don't break here: may appear with another Security
      end;
  finally
    fSafe.UnLock;
    if log <> nil then
      log.Log(sllHttp, '%.RemoveServer(Root=%)=%',
        [self, aServer.Model.Root, BOOL_STR[result]], self);
  end;
end;

procedure TRestHttpServer.DomainHostRedirect(const aDomain, aUri: RawUtf8);
var
  uri: TUri;
begin
  if uri.From(aDomain) and
     EndWith(uri.Server, '.LOCALHOST') then
    fLog.Add.Log(sllWarning, 'DomainHostRedirect(%) is very likely to be ' +
      'unresolved: consider using a real host name instead of the loopback',
      [aDomain], self);
  if aUri = '' then
    fHosts.Delete(aDomain)
  else
    // e.g. Add('project1.com','root1')
    fHosts.Add(aDomain, aUri);
end;

constructor TRestHttpServer.Create(const aPort: RawUtf8;
  const aServers: array of TRestServer; const aDomainName: RawUtf8;
  aUse: TRestHttpServerUse; aThreadPoolCount: Integer;
  aSecurity: TRestHttpServerSecurity; const aAdditionalUrl: RawUtf8;
  const aQueueName: SynUnicode; aHeadersUnFiltered: boolean);
var
  i, j: PtrInt;
  ServersRoot: RawUtf8;
  ErrMsg: RawUtf8;
  log: ISynLog;
begin
  if high(aServers) < 0 then
    fLog := TSynLog
  else
    fLog := aServers[0].LogClass;
  log := fLog.Enter('Create % (%) on port %',
    [ToText(aUse)^, ToText(aSecurity)^, aPort], self);
  inherited Create;
  SetAccessControlAllowOrigin(''); // deny CORS by default
  fHosts.Init(false);
  fDomainName := aDomainName;
  fPort := aPort;
  Split(RawUtf8(fPort), ':', fPublicAddress, fPublicPort);
  if fPublicPort = '' then
  begin
    // you should better set aPort='publicip:port'
    fPublicPort := fPublicAddress;
    fPublicAddress := ExeVersion.Host;
  end;
  fUse := aUse;
  if high(aServers) >= 0 then
  begin
    for i := 0 to high(aServers) do
      if (aServers[i] = nil) or
         (aServers[i].Model = nil) then
        ErrMsg := 'Invalid TRestServer';
    if {%H-}ErrMsg = '' then
      for i := 0 to high(aServers) do
        with aServers[i].Model do
        begin
          ServersRoot := {%H-}ServersRoot + ' ' + Root;
          for j := i + 1 to high(aServers) do
            if aServers[j].Model.UriMatch(Root) <> rmNoMatch then
              FormatUtf8('Duplicated Root URI: % and %',
                [Root, aServers[j].Model.Root], ErrMsg);
        end;
    if ErrMsg <> '' then
      raise ERestHttpServer.CreateUtf8('%.Create(% ): %', [self, ServersRoot, ErrMsg]);
    // associate before HTTP server is started, for TRestServer.BeginCurrentThread
    SetLength(fDBServers, length(aServers));
    for i := 0 to high(aServers) do
      SetDBServer(i, aServers[i], aSecurity, HTTP_DEFAULT_ACCESS_RIGHTS);
  end;
  {$ifndef ONLYUSEHTTPSOCKET}
  if aUse in [useHttpApi, useHttpApiRegisteringURI] then
  try
    if PosEx('Wine', OSVersionInfoEx) > 0 then
      log.Log(sllWarning,
        '%: httpapi probably not supported on % -> try useHttpSocket',
        [ToText(aUse)^, OSVersionInfoEx]);
    // first try to use fastest http.sys
    fHttpServer := THttpApiServer.Create(false, aQueueName, HttpThreadStart,
      HttpThreadTerminate, GetDBServerNames);
    for i := 0 to high(aServers) do
      HttpApiAddUri(aServers[i].Model.Root, fDomainName, aSecurity,
        fUse = useHttpApiRegisteringURI, true);
    if aAdditionalUrl <> '' then
      HttpApiAddUri(aAdditionalUrl, fDomainName, aSecurity,
        fUse = useHttpApiRegisteringURI, true);
  except
    on E: Exception do
    begin
      log.Log(sllError, '% for % % at%  -> fallback to socket-based server',
        [E, ToText(aUse)^, fHttpServer, ServersRoot], self);
      FreeAndNil(fHttpServer); // if http.sys initialization failed
    end;
  end;
  {$endif ONLYUSEHTTPSOCKET}
  if fHttpServer = nil then
  begin
    // http.sys not running -> create one instance of our pure socket server
    if aUse = useBidirSocket then
      fHttpServer := TWebSocketServerRest.Create(fPort, HttpThreadStart,
        HttpThreadTerminate, GetDBServerNames)
    else
      fHttpServer := THttpServer.Create(fPort, HttpThreadStart,
        HttpThreadTerminate, GetDBServerNames, aThreadPoolCount, 30000,
        aHeadersUnFiltered);
    THttpServer(fHttpServer).WaitStarted;
  end;
  fHttpServer.OnRequest := Request;
{$ifndef PUREMORMOT2}
  if aSecurity = secSynShaAes then
    fHttpServer.RegisterCompress(CompressShaAes, 0); // CompressMinSize=0
{$endif PUREMORMOT2}
  {$ifdef COMPRESSSYNLZ} // SynLZ registered first, since will be prefered
  fHttpServer.RegisterCompress(CompressSynLZ);
  {$endif COMPRESSSYNLZ}
  {$ifdef COMPRESSDEFLATE}
  fHttpServer.RegisterCompress(CompressGZip);
  {$endif COMPRESSDEFLATE}
  {$ifndef ONLYUSEHTTPSOCKET}
  if fHttpServer.InheritsFrom(THttpApiServer) then
    // allow fast multi-threaded requests
    if aThreadPoolCount > 1 then
      THttpApiServer(fHttpServer).Clone(aThreadPoolCount - 1);
  {$endif ONLYUSEHTTPSOCKET}
  // last HTTP server handling callbacks would be set for the TRestServer(s)
  if fHttpServer.CanNotifyCallback then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.OnNotifyCallback := NotifyCallback;
  log.Log(sllHttp, '% initialized for%', [fHttpServer, ServersRoot], self);
end;

constructor TRestHttpServer.Create(const aPort: RawUtf8; aServer: TRestServer;
  const aDomainName: RawUtf8; aUse: TRestHttpServerUse;
  aRestAccessRights: POrmAccessRights; aThreadPoolCount: Integer;
  aSecurity: TRestHttpServerSecurity; const aAdditionalUrl: RawUtf8;
  const aQueueName: SynUnicode);
begin
  Create(aPort, [aServer], aDomainName, aUse, aThreadPoolCount,
    aSecurity, aAdditionalUrl, aQueueName);
  if aRestAccessRights <> nil then
    DBServerAccessRight[0] := aRestAccessRights;
end;

destructor TRestHttpServer.Destroy;
var
  log: ISynLog;
begin
  log := fLog.Enter(self, 'Destroy');
  if log <> nil then
    log.Log(sllHttp, '% finalized for %',
      [fHttpServer, Plural('server', length(fDBServers))], self);
  Shutdown(true); // but don't call fDBServers[i].Server.Shutdown
  FreeAndNil(fHttpServer);
  inherited Destroy;
  fAccessControlAllowOriginsMatch.Free;
end;

procedure TRestHttpServer.Shutdown(noRestServerShutdown: boolean);
var
  i: PtrInt;
  log: ISynLog;
begin
  if (self <> nil) and
     not fShutdownInProgress then
  begin
    log := fLog.Enter('Shutdown(%)', [BOOL_STR[noRestServerShutdown]], self);
    fShutdownInProgress := true;
    fHttpServer.Shutdown;
    fSafe.Lock; // protect fDBServers[]
    try
      for i := 0 to high(fDBServers) do
      begin
        if not noRestServerShutdown then
          fDBServers[i].Server.Shutdown;
        if TMethod(fDBServers[i].Server.OnNotifyCallback).Data = self then
          // avoid unexpected GPF
          fDBServers[i].Server.OnNotifyCallback := nil;
      end;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TRestHttpServer.GetDBServer(Index: Integer): TRestServer;
begin
  result := nil;
  if self = nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    if cardinal(Index) < cardinal(length(fDBServers)) then
      result := fDBServers[Index].Server;
  finally
    fSafe.UnLock;
  end;
end;

function TRestHttpServer.GetDBServerCount: integer;
begin
  result := length(fDBServers);
end;

function TRestHttpServer.GetDBServerNames: RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  if self = nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      result := result + fDBServers[i].Server.Model.Root + ' ';
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestHttpServer.SetDBServerAccessRight(Index: integer;
  Value: POrmAccessRights);
begin
  if self = nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    if Value = nil then
      Value := HTTP_DEFAULT_ACCESS_RIGHTS;
    if cardinal(Index) < cardinal(length(fDBServers)) then
      fDBServers[Index].RestAccessRights := Value;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestHttpServer.SetDBServer(aIndex: integer; aServer: TRestServer;
  aSecurity: TRestHttpServerSecurity; aRestAccessRights: POrmAccessRights);
begin
  // note: caller should have made fSafe.Lock
  if (self <> nil) and
     (cardinal(aIndex) < cardinal(length(fDBServers))) then
    with fDBServers[aIndex] do
    begin
      Server := aServer;
      if (fHttpServer <> nil) and
         fHttpServer.CanNotifyCallback then
        Server.OnNotifyCallback := NotifyCallback;
      Server.SetPublicUri(fPublicAddress, fPublicPort);
      Security := aSecurity;
      if aRestAccessRights = nil then
        RestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS
      else
        RestAccessRights := aRestAccessRights;
    end;
end;

const
  HTTPS_TEXT: array[boolean] of string[1] = (
    '', 's');
  HTTPS_SECURITY: array[boolean] of TRestHttpServerSecurity = (
    secNone, secSSL);

procedure TRestHttpServer.RootRedirectToUri(const aRedirectedUri: RawUtf8;
  aRegisterUri: boolean; aHttps: boolean);
begin
  if fRootRedirectToUri[aHttps] = aRedirectedUri then
    exit;
  fLog.Add.Log(sllHttp, 'Redirect http%://localhost:% to http%://localhost:%/%',
    [HTTPS_TEXT[aHttps], fPublicPort, HTTPS_TEXT[aHttps], fPublicPort,
     aRedirectedUri], self);
  fRootRedirectToUri[aHttps] := aRedirectedUri;
  if aRedirectedUri <> '' then
    HttpApiAddUri('/', '+', HTTPS_SECURITY[aHttps], aRegisterUri, true);
end;

function TRestHttpServer.HttpApiAddUri(const aRoot, aDomainName: RawByteString;
  aSecurity: TRestHttpServerSecurity; aRegisterUri,
  aRaiseExceptionOnError: boolean): RawUtf8;
{$ifndef ONLYUSEHTTPSOCKET}
var
  err: integer;
  https: boolean;
{$endif ONLYUSEHTTPSOCKET}
begin
  result := ''; // no error
  {$ifndef ONLYUSEHTTPSOCKET}
  if not fHttpServer.InheritsFrom(THttpApiServer) then
    exit;
  https := aSecurity = secSSL;
  fLog.Add.Log(sllHttp, 'http.sys registration of http%://%:%/%',
    [HTTPS_TEXT[https], aDomainName, fPublicPort, aRoot], self);
  // try to register the URL to http.sys
  err := THttpApiServer(fHttpServer).AddUrl(aRoot, fPublicPort, https,
    aDomainName, aRegisterUri);
  if err = NO_ERROR then
    exit;
  FormatUtf8('http.sys URI registration error #% for http%://%:%/%',
    [err, HTTPS_TEXT[https], aDomainName, fPublicPort, aRoot], result);
  if err = ERROR_ACCESS_DENIED then
    if aRegisterUri then
      result := result +
        ' (administrator rights needed, at least once to register the URI)'
    else
      result := result +
        ' (you need to register the URI - try to use useHttpApiRegisteringURI)';
  fLog.Add.Log(sllLastError, result, self);
  if aRaiseExceptionOnError then
    raise ERestHttpServer.CreateUtf8('%: %', [self, result]);
  {$endif ONLYUSEHTTPSOCKET}
end;

function TRestHttpServer.Request(Ctxt: THttpServerRequestAbstract): cardinal;
var
  call: TRestUriParams;
  i, hostlen: PtrInt;
  P: PUtf8Char;
  headers, hostroot, redirect: RawUtf8;
  match: TRestModelMatch;
  serv: TRestServer;
  c: THttpServerRequest absolute Ctxt;
begin
  if (self = nil) or
     fShutdownInProgress then
    result := HTTP_NOTFOUND
  else if ((Ctxt.Url = '') or
           (Ctxt.Url = '/')) and
          (Ctxt.Method = 'GET') then
    // RootRedirectToUri() to redirect ip:port root URI to a given sub-URI
    if fRootRedirectToUri[Ctxt.UseSSL] <> '' then
    begin
      Ctxt.OutCustomHeaders := 'Location: ' + fRootRedirectToUri[Ctxt.UseSSL];
      result := HTTP_TEMPORARYREDIRECT;
    end
    else
      result := HTTP_BADREQUEST
  else if Ctxt.Method = 'OPTIONS' then
  begin
    // handle CORS
    if fAccessControlAllowOrigin = '' then
      Ctxt.OutCustomHeaders := 'Access-Control-Allow-Origin:'
    else
    begin
      FindNameValue(Ctxt.InHeaders, 'ACCESS-CONTROL-REQUEST-HEADERS:', headers);
      Ctxt.OutCustomHeaders := 'Access-Control-Allow-Headers: ' + headers;
      ComputeAccessControlHeader(Ctxt);
    end;
    result := HTTP_NOCONTENT;
  end
  else if (Ctxt.Method = '') or
          (OnlyJsonRequests and
           not IdemPChar(pointer(Ctxt.InContentType), JSON_CONTENT_TYPE_UPPER)) then
    // wrong Input parameters or not JSON request: 400 BAD REQUEST
    result := HTTP_BADREQUEST
  else
  begin
    // compute URI, handling any virtual host domain
    call.Init;
    call.LowLevelConnectionID := Ctxt.ConnectionID;
    if Ctxt.UseSSL then
      call.LowLevelFlags := call.LowLevelFlags + [llfHttps, llfSecured];
    if c.ConnectionThread <> nil then
      if PClass(c.ConnectionThread)^ = TWebSocketServerResp then
      begin
        include(call.LowLevelFlags, llfWebsockets);
        if TWebSocketServerResp(c.ConnectionThread).WebSocketProtocol.Encrypted then
          include(call.LowLevelFlags, llfSecured);
      end;
    if fHosts.Count > 0 then
    begin
      FindNameValue(Ctxt.InHeaders, 'HOST: ', hostroot);
      i := PosExChar(':', hostroot);
      if i > 0 then
        SetLength(hostroot, i - 1); // trim any port
      if hostroot <> '' then
        // e.g. 'Host: project1.com' -> 'root1'
        hostroot := fHosts.Value(hostroot);
    end;
    if hostroot <> '' then
      if (Ctxt.Url = '') or
         (Ctxt.Url = '/') then
        call.Url := hostroot
      else if Ctxt.Url[1] = '/' then
        call.Url := hostroot + Ctxt.Url
      else
        call.Url := hostroot + '/' + Ctxt.Url
    else if (Ctxt.Url <> '') and
            (Ctxt.Url[1] = '/') then
      call.Url := copy(Ctxt.Url, 2, maxInt)
    else
      call.Url := Ctxt.Url;
    // search and call any matching TRestServer instance
    result := HTTP_NOTFOUND; // page not found by default (in case of wrong URL)
    serv := nil;
    match := rmNoMatch;
    fSafe.Lock; // protect fDBServers[]
    try
      for i := 0 to length(fDBServers) - 1 do
        with fDBServers[i] do
          if Ctxt.UseSSL = (Security = secSSL) then
          begin
            // registered for http or https
            match := Server.Model.UriMatch(call.Url);
            if match = rmNoMatch then
              continue;
            call.RestAccessRights := RestAccessRights;
            serv := Server;
            break;
          end;
    finally
      fSafe.UnLock;
    end;
    if (match = rmNoMatch) or
       (serv = nil) then
      exit;
    if fRedirectServerRootUriForExactCase and
       (match = rmMatchWithCaseChange) then
    begin
      // force redirection to exact Server.Model.Root case sensitivity
      call.OutStatus := HTTP_TEMPORARYREDIRECT;
      call.OutHead := 'Location: ' + serv.Model.Root +
        copy(call.Url, length(serv.Model.Root)  + 1, maxInt);
    end
    else
    begin
      // call matching TRestServer.Uri()
      call.Method := Ctxt.Method;
      call.InHead := Ctxt.InHeaders;
      call.InBody := Ctxt.InContent;
      serv.Uri(call);
    end;
    // set output content
    result := call.OutStatus;
    Ctxt.OutContent := call.OutBody;
    Ctxt.OutContentType := call.OutBodyType;
    P := pointer(call.OutHead);
    if IdemPChar(P, 'CONTENT-TYPE: ') then
    begin
      // change mime type if modified in HTTP header (e.g. GET blob fields)
      Ctxt.OutContentType := GetNextLine(P + 14, P);
      call.OutHead := P;
    end
    else
      // default content type is JSON
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    // handle HTTP redirection and cookies over virtual hosts
    if hostroot <> '' then
    begin
      if (result = HTTP_MOVEDPERMANENTLY) or
         (result = HTTP_TEMPORARYREDIRECT) then
      begin
        redirect := FindIniNameValue(P, 'LOCATION: ');
        hostlen := length(hostroot);
        if (length(redirect) > hostlen) and
           (redirect[hostlen + 1] = '/') and
           IdemPropNameU(hostroot, pointer(redirect), hostlen) then
          // hostroot/method -> method on same domain
          call.OutHead := 'Location: ' + copy(redirect, hostlen + 1, maxInt);
      end
      else if ExistsIniName(P, 'SET-COOKIE:') then
        call.OutHead := StringReplaceAll(call.OutHead, '; Path=/' +
          serv.Model.Root, '; Path=/')
    end;
    Ctxt.OutCustomHeaders := TrimU(call.OutHead);
    if call.OutInternalState <> 0 then
      Ctxt.OutCustomHeaders := FormatUtf8('%'#13#10'Server-InternalState: %',
        [Ctxt.OutCustomHeaders, call.OutInternalState]);
    // handle optional CORS origin
    if fAccessControlAllowOrigin <> '' then
      ComputeAccessControlHeader(Ctxt);
    Ctxt.OutCustomHeaders := TrimU(Ctxt.OutCustomHeaders);
  end;
end;

procedure TRestHttpServer.HttpThreadTerminate(Sender: TThread);
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.Run.EndCurrentThread(Sender);
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestHttpServer.HttpThreadStart(Sender: TThread);
var
  i: PtrInt;
begin
  if self = nil then
    exit;
  SetCurrentThreadName('% %/%%', [self, fPort, GetDBServerNames, Sender]);
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.Run.BeginCurrentThread(Sender);
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestHttpServer.SetAccessControlAllowOrigin(const Value: RawUtf8);
var
  patterns: TRawUtf8DynArray;
begin
  fAccessControlAllowOrigin := Value;
  FreeAndNil(fAccessControlAllowOriginsMatch);
  if (Value = '') or
     (Value = '*') then
    exit;
  CsvToRawUtf8DynArray(pointer(Value), patterns);
  if patterns = nil then
    exit;
  fAccessControlAllowOriginsMatch :=
    TMatchs.Create(patterns, {caseinsensitive=}true);
end;

procedure TRestHttpServer.ComputeAccessControlHeader(
  Ctxt: THttpServerRequestAbstract);
var
  origin: RawUtf8;
begin
  // note: caller did ensure that fAccessControlAllowOrigin<>''
  FindNameValue(Ctxt.InHeaders, 'ORIGIN: ', origin);
  if origin = '' then
    exit;
  if fAccessControlAllowOrigin = '*' then
    origin := fAccessControlAllowOrigin
  else if fAccessControlAllowOriginsMatch.Match(origin) < 0 then
    exit;
  Ctxt.OutCustomHeaders := Ctxt.OutCustomHeaders +
    #13#10'Access-Control-Allow-Methods: POST, PUT, GET, DELETE, LOCK, OPTIONS' +
    #13#10'Access-Control-Max-Age: 1728000' +
    // see http://blog.import.io/tech-blog/exposing-headers-over-cors-with-access-control-expose-headers
    #13#10'Access-Control-Expose-Headers: content-length,location,server-internalstate' +
    #13#10'Access-Control-Allow-Origin: ' + origin;
  if fAccessControlAllowCredential then
    Ctxt.OutCustomHeaders := Ctxt.OutCustomHeaders +
      #13#10'Access-Control-Allow-Credentials: true';
end;

function TRestHttpServer.WebSocketsEnable(
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax, aWebSocketsCompressed: boolean): TWebSocketServerRest;
begin
  if fHttpServer.InheritsFrom(TWebSocketServerRest) then
  begin
    result := TWebSocketServerRest(fHttpServer);
    result.WebSocketsEnable(aWebSocketsURI, aWebSocketsEncryptionKey,
      aWebSocketsAjax, aWebSocketsCompressed);
  end
  else
    raise EWebSockets.CreateUtf8(
      '%.WebSocketEnable(%): expected useBidirSocket',
      [self, GetEnumName(TypeInfo(TRestHttpServerUse),
       ord(fUse))^]);
end;

function TRestHttpServer.WebSocketsEnable(aServer: TRestServer;
  const aWebSocketsEncryptionKey: RawUtf8;
  aWebSocketsAjax, aWebSocketsCompressed: boolean): TWebSocketServerRest;
begin
  if (aServer = nil) or
     (DBServerFind(aServer) < 0) then
    raise EWebSockets.CreateUtf8('%.WebSocketEnable(aServer=%?)', [self, aServer]);
  result := WebSocketsEnable(aServer.Model.Root, aWebSocketsEncryptionKey,
    aWebSocketsAjax, aWebSocketsCompressed);
end;

function TRestHttpServer.NotifyCallback(aSender: TRestServer;
  const aInterfaceDotMethodName, aParams: RawUtf8;
  aConnectionID: THttpServerConnectionID; aFakeCallID: integer;
  aResult, aErrorMsg: PRawUtf8): boolean;
var
  ctxt: THttpServerRequest;
  status: cardinal;
begin
  result := false;
  if (self <> nil) and
     not fShutdownInProgress then
  try
    if fHttpServer <> nil then
    begin
      // aConnection.InheritsFrom(TSynThread) may raise an exception
      // -> checked in WebSocketsCallback/IsActiveWebSocket
      ctxt := THttpServerRequest.Create(nil, aConnectionID, nil);
      try
        ctxt.Prepare(FormatUtf8('%/%/%', [aSender.Model.Root,
          aInterfaceDotMethodName, aFakeCallID]), 'POST', '',
            '[' + aParams + ']', '', '', {ssl=}false);
        status := fHttpServer.Callback(ctxt, aResult = nil);
        if status = HTTP_SUCCESS then
        begin
          if aResult <> nil then
            if IdemPChar(pointer(ctxt.OutContent), '{"RESULT":') then
              aResult^ := copy(ctxt.OutContent, 11, maxInt)
            else
              aResult^ := ctxt.OutContent;
          result := true;
        end
        else if aErrorMsg <> nil then
          FormatUtf8('%.Callback(%) received status=% from %',
            [fHttpServer, aConnectionID, status, ctxt.Url], aErrorMsg^);
      finally
        ctxt.Free;
      end;
    end
    else if aErrorMsg <> nil then
      FormatUtf8('%.NotifyCallback with fHttpServer=nil', [self], aErrorMsg^);
  except
    on E: Exception do
      if aErrorMsg <> nil then
        aErrorMsg^ := ObjectToJsonDebug(E);
  end;
end;

constructor TRestHttpServer.Create(aServer: TRestServer;
  aDefinition: TRestHttpServerDefinition; aForcedUse: TRestHttpServerUse;
  aWebSocketsLoopDelay: integer);
const
  AUTH: array[TRestHttpServerRestAuthentication] of
    TRestServerAuthenticationClass = (
    // adDefault, adHttpBasic, adWeak, adSSPI
    TRestServerAuthenticationDefault,
    TRestServerAuthenticationHttpBasic,
    TRestServerAuthenticationNone,
    {$ifdef DOMAINRESTAUTH}
    TRestServerAuthenticationSspi
    {$else}
    nil
    {$endif DOMAINRESTAUTH});
var
  a: TRestHttpServerRestAuthentication;
  thrdcnt: integer;
  websock: TWebSocketServerRest;
begin
  if aDefinition = nil then
    raise ERestHttpServer.CreateUtf8('%.Create(aDefinition=nil)', [self]);
  if aDefinition.WebSocketPassword <> '' then
    aForcedUse := useBidirSocket;
  if aDefinition.ThreadCount = 0 then
    thrdcnt := 32
  else
    thrdcnt := aDefinition.ThreadCount;
  Create(aDefinition.BindPort, aServer, '+', aForcedUse, nil, thrdcnt,
    HTTPS_SECURITY[aDefinition.Https], '', aDefinition.HttpSysQueueName);
  if aDefinition.EnableCors <> '' then
  begin
    AccessControlAllowOrigin := aDefinition.EnableCors;
    AccessControlAllowCredential := true;
  end;
  if fHttpServer <> nil then
    fHttpServer.RemoteIPHeader := aDefinition.RemoteIPHeader;
  a := aDefinition.Authentication;
  if aServer.HandleAuthentication then
    if AUTH[a] = nil then
      fLog.Add.Log(sllWarning, 'Ignored',
        TypeInfo(TRestHttpServerRestAuthentication), a, self)
    else
    begin
      aServer.AuthenticationUnregisterAll;
      aServer.AuthenticationRegister(AUTH[a]);
    end;
  if aDefinition.WebSocketPassword <> '' then
  begin
    websock := WebSocketsEnable(aServer, aDefinition.PasswordPlain);
    if HttpServerFullWebSocketsLog then
      websock.Settings.SetFullLog;
    websock.Settings^.LoopDelay := aWebSocketsLoopDelay;
  end;
end;



{ ************ TRestHttpRemoteLogServer to Receive Remote Log Stream }

{ TRestHttpRemoteLogServer }

constructor TRestHttpRemoteLogServer.Create(const aRoot: RawUtf8;
  aPort: integer; const aEvent: TRemoteLogReceivedOne);
var
  aModel: TOrmModel;
begin
  aModel := TOrmModel.Create([], aRoot);
  fServer := TRestServerFullMemory.Create(aModel);
  aModel.Owner := fServer;
  fServer.ServiceMethodRegisterPublishedMethods('', self);
  fServer.AcquireExecutionMode[execSoaByMethod] := amLocked; // protect aEvent
  inherited Create(UInt32ToUtf8(aPort), fServer, '+', HTTP_DEFAULT_MODE, nil, 1);
  fEvent := aEvent;
  SetAccessControlAllowOrigin('*'); // e.g. when called from AJAX/SMS
end;

destructor TRestHttpRemoteLogServer.Destroy;
begin
  try
    inherited Destroy;
  finally
    fServer.Free;
  end;
end;

procedure TRestHttpRemoteLogServer.RemoteLog(Ctxt: TRestServerUriContext);
begin
  if Assigned(fEvent) and
     (Ctxt.Method = mPUT) then
  begin
    fEvent(Ctxt.Call^.InBody);
    Ctxt.Success;
  end;
end;


end.

