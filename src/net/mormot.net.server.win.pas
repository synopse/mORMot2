/// HTTP Server Classes Over Windows http.sys Module
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.server.win;

{
  *****************************************************************************

   HTTP Server Classes Over Windows http.sys Module
   - THttpApiServer HTTP/1.1 Server Over Windows http.sys Module
   - THttpApiWebSocketServer Over Windows http.sys Module

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef USEHTTPSYS}

// compile as a void unit if USEHTTPSYS is not defined

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.fmt,
  mormot.core.variants,
  mormot.core.zip,
  mormot.core.log,
  mormot.core.search,
  mormot.net.sock,
  mormot.net.http,
  mormot.lib.winhttp,
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  mormot.net.client,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.factory,
  mormot.net.server;


{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

type
  THttpApiServer = class;

  /// processing sub-thread of THttpApiServer
  THttpApiServerThread = class(TLoggedThread)
  protected
    fOwner: THttpApiServer;
    fStarted: boolean;
    // just redirect to fOwner.DoExecute
    procedure DoExecute; override;
  public
    /// initialize this processing thread
    constructor Create(aOwner: THttpApiServer); reintroduce;
  end;
  THttpApiServerThreads = array of THttpApiServerThread;

  /// HTTP server using fast http.sys kernel-mode server
  // - The HTTP Server API enables applications to communicate over HTTP without
  // using Microsoft Internet Information Server (IIS). Applications can register
  // to receive HTTP requests for particular URLs, receive HTTP requests, and send
  // HTTP responses. The HTTP Server API includes TLS support so that applications
  // can exchange data over secure HTTP connections without IIS. It is also
  // designed to work with I/O completion ports.
  // - The HTTP Server API is supported on Windows Server 2003 operating systems
  // and on Windows XP with Service Pack 2 (SP2). Be aware that Microsoft IIS 5
  // running on Windows XP with SP2 is not able to share port 80 with other HTTP
  // applications running simultaneously.
  THttpApiServer = class(THttpServerGeneric)
  protected
    fReqQueue: THandle;                         // the internal request queue
    fThreads: THttpApiServerThreads;            // additional processing threads
    fRegisteredUnicodeUrl: TSynUnicodeDynArray; // all registered URLs
    fServerSessionID: HTTP_SERVER_SESSION_ID;
    fUrlGroupID: HTTP_URL_GROUP_ID;
    fLoggingServiceName: RawUtf8;
    fRegisteredUrl: RawUtf8;
    fReceiveBufferSize: cardinal;
    fAuthenticationSchemes: THttpApiRequestAuthentications; // 8-bit
    fLogging: boolean;                                      // 8-bit
    fIdleTix64: Int64; // set every second by OnIdleProcess
    fIdleThread: TSynBackgroundThreadProcess;
    function GetRegisteredUrl: RawUtf8;
    function GetHttpQueueLength: cardinal; override;
    function GetConnectionsActive: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetProperty(dest: PHTTP_QOS_SETTING_INFO; destlen: cardinal;
      qos: HTTP_QOS_SETTING_TYPE): boolean;
    procedure SetProperty(value: PHTTP_QOS_SETTING_INFO; valuelen: cardinal;
      qos: HTTP_QOS_SETTING_TYPE; alsoForSession: boolean = false);
    function GetMaxBandwidth: cardinal;
    procedure SetMaxBandwidth(aValue: cardinal);
    function GetMaxConnections: cardinal;
    procedure SetMaxConnections(aValue: cardinal);
    function GetApiVersion: RawUtf8; override;
    function CheckAndLog(Api: THttpApiFunction; Error: integer;
      Level: TSynLogLevel = sllWarning; ErrMsg: PRawUtf8 = nil): integer;
    procedure Ensure(Api: THttpApiFunction; Error: integer);
    procedure DoAfterResponse(Ctxt: THttpServerRequest; const Referer: RawUtf8;
      StatusCode: cardinal; Elapsed, Received, Sent: QWord); virtual;
    procedure OnIdleProcess(Sender: TSynBackgroundThreadProcess);
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrieve the content
    procedure DoExecute; override;
    /// retrieve flags for SendHttpResponse
   // - if response content type is not STATICFILE_CONTENT_TYPE
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; virtual;
    procedure DestroyMainThread; virtual;
  public
    /// initialize the HTTP Service
    // - will raise an exception if http.sys is not available e.g. before
    // Windows XP SP2) or if the request queue creation failed
    // - if you override this contructor, put the AddUrl() methods within,
    // and you can set CreateSuspended to FALSE
    // - if you will call AddUrl() methods later, set CreateSuspended to TRUE,
    // then call explicitly the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(QueueName: SynUnicode = '';
      const OnStart: TOnNotifyThread = nil; const OnStop: TOnNotifyThread = nil;
      const ProcessName: RawUtf8 = ''; ProcessOptions: THttpServerOptions = [];
      aLog: TSynLogClass = nil; ServerThreadPoolCount: integer = 32); reintroduce;
    /// ensure the HTTP server threads are actually started
    // - note that http.sys requires global system-wide registration for its
    // TLS/https settings
    function WaitStarted(Seconds: cardinal = 30): boolean;
    /// release all associated memory and handles
    destructor Destroy; override;
    /// register the URLs to Listen On
    // - e.g. AddUrl('root','888')
    // - aDomainName could be either a fully qualified case-insensitive domain
    // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port)
    // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
    // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
    // running with enough rights (by default, UAC requires administrator rights
    // for adding an URL to http.sys registration list) - solution is to call
    // the THttpApiServer.AddUrlAuthorize class method during program setup
    // - if this method is not used within an overridden constructor, default
    // Create must have be called with CreateSuspended = TRUE and then call the
    // Resume method after all Url have been added
    // - if aRegisterUri is TRUE, the URI will be registered (need adminitrator
    // rights) - default is FALSE, as defined by Windows security policy
    function AddUrl(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; aRegisterUri: boolean = false;
      aContext: Int64 = 0): integer;
    /// un-register the URLs to Listen On
    // - this method expect the same parameters as specified to AddUrl()
    // - return 0 (NO_ERROR) on success, an error code if failed (e.g.
    // -1 if the corresponding parameters do not match any previous AddUrl)
    function RemoveUrl(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'): integer;
    /// will authorize a specified URL prefix
    // - will allow to call AddUrl() later for any user on the computer
    // - if aRoot is left '', it will authorize any root for this port
    // - must be called with Administrator rights: this class function is to be
    // used in a Setup program for instance, especially under Vista or Seven,
    // to reserve the Url for the server
    // - add a new record to the http.sys URL reservation store
    // - return '' on success, an error message otherwise
    // - will first delete any matching rule for this URL prefix
    // - if OnlyDelete is true, will delete but won't add the new authorization;
    // in this case, any error message at deletion will be returned
    class function AddUrlAuthorize(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; OnlyDelete: boolean = false): RawUtf8;
    /// access to the additional processing threads
    property Threads: THttpApiServerThreads
      read fThreads;
  public
    { HTTP API 2.0 methods and properties }
    /// can be used to check if the HTTP API 2.0 is available
    class function HasApi2: boolean;
      {$ifdef HASINLINE} inline; static; {$endif}
    /// enable HTTP API 2.0 advanced timeout settings
    // - all those settings are set for the current URL group
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   SetTimeOutLimits(....);
    // - aEntityBody is the time, in seconds, allowed for the request entity
    // body to arrive - default value is 2 minutes
    // - aDrainEntityBody is the time, in seconds, allowed for the HTTP Server
    // API to drain the entity body on a Keep-Alive connection - default value
    // is 2 minutes
    // - aRequestQueue is the time, in seconds, allowed for the request to
    // remain in the request queue before the application picks it up - default
    // value is 2 minutes
    // - aIdleConnection is the time, in seconds, allowed for an idle connection;
    // is similar to THttpServer.ServerKeepAliveTimeOut - default value is
    // 2 minutes
    // - aHeaderWait is the time, in seconds, allowed for the HTTP Server API
    // to parse the request header - default value is 2 minutes
    // - aMinSendRate is the minimum send rate, in bytes-per-second, for the
    // response - default value is 150 bytes-per-second
    // - any value set to 0 will set the HTTP Server API default value
    procedure SetTimeOutLimits(aEntityBody, aDrainEntityBody,
      aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
    /// enable HTTP API 2.0 logging
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   LogStart(....);
    // - you can select the output folder and the expected logging layout
    // - aSoftwareName will set the optional W3C-only software name string
    // - aRolloverSize will be used only when aRolloverType is hlrSize
    procedure LogStart(const aLogFolder: TFileName;
      aType: THttpApiLoggingType = hltW3C;
      const aSoftwareName: TFileName = '';
      aRolloverType: THttpApiLoggingRollOver = hlrDaily;
      aRolloverSize: cardinal = 0;
      aLogFields: THttpApiLogFields = [hlfDate..hlfSubStatus];
      aFlags: THttpApiLoggingFlags = [hlfUseUtf8Conversion]);
    /// disable HTTP API 2.0 logging
    procedure LogStop;
    /// enable HTTP API 2.0 server-side authentication
    // - once enabled, the client sends an unauthenticated request: it is up to
    // the server application to generate the initial 401 challenge with proper
    // WWW-Authenticate headers; any further authentication steps will be
    // perform in kernel mode, until the authentication handshake is finalized;
    // later on, the application can check the AuthenticationStatus property
    // of THttpServerRequest and its associated AuthenticatedUser value
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasApi2 then
    // !   SetAuthenticationSchemes(....);
    // - this method will work on the current group, for all instances
    // - see HTTPAPI_AUTH_ENABLE_ALL constant to set all available schemes
    // - optional Realm parameters can be used when haBasic scheme is defined
    // - optional DomainName and Realm parameters can be used for haDigest
    procedure SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
      const DomainName: SynUnicode = ''; const Realm: SynUnicode = '');
    /// read-only access to HTTP API 2.0 server-side enabled authentication schemes
    property AuthenticationSchemes: THttpApiRequestAuthentications
      read fAuthenticationSchemes;
    /// read-only access to check if the HTTP API 2.0 logging is enabled
    // - use LogStart/LogStop methods to change this property value
    property Logging: boolean
      read fLogging;
    /// the current HTTP API 2.0 logging Service name
    // - should be UTF-8 encoded, if LogStart(aFlags=[hlfUseUtf8Conversion])
    property LoggingServiceName: RawUtf8
      read fLoggingServiceName write fLoggingServiceName;
    /// read-only access to the low-level HTTP API 2.0 Session ID
    property ServerSessionID: HTTP_SERVER_SESSION_ID
      read fServerSessionID;
    /// read-only access to the low-level HTTP API 2.0 URI Group ID
    property UrlGroupID: HTTP_URL_GROUP_ID
      read fUrlGroupID;
    /// how many bytes are retrieved in a single call to ReceiveRequestEntityBody
    // - set by default to 1048576, i.e. 1 MB - practical limit is around 20 MB
    // - you may customize this value if you encounter HTTP error HTTP_NOTACCEPTABLE
    // (406) from client, corresponding to an ERROR_NO_SYSTEM_RESOURCES (1450)
    // exception on server side, when uploading huge data content
    property ReceiveBufferSize: cardinal
      read fReceiveBufferSize write fReceiveBufferSize;
  published
    /// return the list of registered URL on this server instance as a CSV text
    property RegisteredUrl: RawUtf8
      read GetRegisteredUrl;
    /// the maximum allowed bandwidth rate in bytes per second (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited bandwidth
    // - by default Windows not limit bandwidth (actually limited to 4 Gbit/sec).
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxBandwidth: cardinal
      read GetMaxBandwidth write SetMaxBandwidth;
    /// the maximum number of HTTP connections allowed (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited number of connections
    // - by default Windows does not limit number of allowed connections
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxConnections: cardinal
      read GetMaxConnections write SetMaxConnections;
  end;


{ ****************** THttpApiWebSocketServer Over Windows http.sys Module }

type
  TSynThreadPoolHttpApiWebSocketServer = class;
  THttpApiWebSocketServer = class;
  THttpApiWebSocketServerProtocol = class;

  /// current state of a THttpApiWebSocketConnection
  TWebSocketState = (
    wsConnecting,
    wsOpen,
    wsClosing,
    wsClosedByClient,
    wsClosedByServer,
    wsClosedByGuard,
    wsClosedByShutdown);

  /// Identify one THttpApiWebSocketConnection instance
  THttpApiWebSocketConnectionID = TLockedListID;

  PHttpApiWebSocketConnection = ^THttpApiWebSocketConnection;

  THttpApiWsIocpKind = (
    wiReceive,
    wiWake);

  PHttpApiWsIocp = ^THttpApiWsIocp;
  THttpApiWsIocp = record
    Overlapped: TOverlapped; // MUST be first
    Owner: PHttpApiWebSocketConnection;
    Kind: THttpApiWsIocpKind;
  end;

  /// TOverlapped-rooted structure representing a single WebSocket connection
  {$ifdef USERECORDWITHMETHODS}
  THttpApiWebSocketConnection = record
  {$else}
  THttpApiWebSocketConnection = object
  {$endif USERECORDWITHMETHODS}
  private
    fList: TLockedListOne;    // MUST be first for TLockedList
    fReadIo: THttpApiWsIocp;
    fWakeIo: THttpApiWsIocp;
    fReadPending: boolean;
    fWakePending: boolean;
    fUpgradePending: boolean;
    fConnected: boolean;
    fState: TWebSocketState;
    fCloseStatus: WEB_SOCKET_CLOSE_STATUS;
    fProtocol: THttpApiWebSocketServerProtocol;
    fOpaqueHTTPRequestId: HTTP_REQUEST_ID;
    fWSHandle: WEB_SOCKET_HANDLE;
    fLastActionContext: pointer;
    fLastActiveTix64, fLastPingTix64: Int64;
    fPrivateData: pointer;
    fBuffer: RawByteString;
    function ProcessActions(ActionQueue: cardinal): boolean;
    function ReadData(const WebsocketBufferData): integer;
    function WriteData(const WebsocketBufferData; out Written: cardinal): boolean;
    procedure BeforeRead;
    procedure DoOnMessage(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: pointer; aBufferSize: ULONG);
    procedure DoOnConnect;
    procedure DoOnDisconnect;
    procedure DoClose(Reason: TWebSocketState; const Context: RawUtf8 = '');
    procedure InternalSend(aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
    procedure Ping;
    procedure Disconnect;
    procedure CheckIsActive(Tix64: Int64);
    // call onAccept Method of protocol, and if protocol not accept connection or
    // can not be accepted from other reasons return false else return true
    function TryAcceptConnection(aProtocol: THttpApiWebSocketServerProtocol;
      Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
  public
    /// Send data to client
    procedure Send(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: pointer; aBufferSize: ULONG);
    /// Close connection
    procedure Close(aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: pointer; aBufferSize: ULONG);
    /// Protocol of connection
    property Protocol: THttpApiWebSocketServerProtocol
       read fProtocol;
    /// Custom user data
    property PrivateData: pointer
      read fPrivateData write fPrivateData;
    /// Access to the current state of this connection
    property State: TWebSocketState
      read fState;
    /// Genuine Sequence Number of this connection, from TLockedList
    property ID: THttpApiWebSocketConnectionID
      read fList.sequence;
  end;

  /// Event handler on THttpApiWebSocketServerProtocol Accepted connection
  TOnHttpApiWebSocketServerAcceptEvent = function(Ctxt: THttpServerRequest;
    var Conn: THttpApiWebSocketConnection): boolean of object;
  /// Event handler on THttpApiWebSocketServerProtocol Message received
  TOnHttpApiWebSocketServerMessageEvent = procedure(var Conn: THttpApiWebSocketConnection;
    aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG) of object;
  /// Event handler on THttpApiWebSocketServerProtocol connection
  TOnHttpApiWebSocketServerConnectEvent = procedure(
    var Conn: THttpApiWebSocketConnection) of object;
  /// Event handler on THttpApiWebSocketServerProtocol disconnection
  TOnHttpApiWebSocketServerDisconnectEvent = procedure(var Conn: THttpApiWebSocketConnection;
    aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: pointer; aBufferSize: ULONG) of object;

  /// Protocol Handler of websocket endpoints events
  // - maintains a list of all WebSockets clients for a given protocol
  THttpApiWebSocketServerProtocol = class
  private
    fSafe: TOSLock;
    fName: RawUtf8;
    fServer: THttpApiWebSocketServer;
    fConnections: TLockedList;
    fOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
    fOnMessage: TOnHttpApiWebSocketServerMessageEvent;
    fOnFragment: TOnHttpApiWebSocketServerMessageEvent;
    fOnConnect: TOnHttpApiWebSocketServerConnectEvent;
    fOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
    fManualFragmentManagement: boolean;
    procedure OnConnectionFree(one: PLockedListOne);
    procedure PostWake(conn: PHttpApiWebSocketConnection);
    procedure BeginClose(conn: PHttpApiWebSocketConnection; Reason: TWebSocketState);
    procedure DoShutdown;
  public
    /// initialize the WebSockets process
    // - if aManualFragmentManagement is true, onMessage will appear only for whole
    // received messages, otherwise OnFragment handler must be passed (for video
    // broadcast, for example)
    constructor Create(const aName: RawUtf8; aManualFragmentManagement: boolean;
      aServer: THttpApiWebSocketServer;
      const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
      const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
      const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
      const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
      const aOnFragment: TOnHttpApiWebSocketServerMessageEvent = nil);
    /// finalize the process
    destructor Destroy; override;
    /// text identifier
    property Name: RawUtf8
      read fName;
    /// OnFragment event will be called for each fragment
    property ManualFragmentManagement: boolean
      read fManualFragmentManagement;
    /// event triggerred when a WebSockets client is initiated
    property OnAccept: TOnHttpApiWebSocketServerAcceptEvent
      read fOnAccept;
    /// event triggerred when a WebSockets message is received
    property OnMessage: TOnHttpApiWebSocketServerMessageEvent
      read fOnMessage;
    /// event triggerred when a WebSockets client is connected
    property OnConnect: TOnHttpApiWebSocketServerConnectEvent
      read fOnConnect;
    /// event triggerred when a WebSockets client is gracefully disconnected
    property OnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent
      read fOnDisconnect;
    /// event triggerred when a non complete frame is received
    // - required if ManualFragmentManagement is true
    property OnFragment: TOnHttpApiWebSocketServerMessageEvent
      read fOnFragment;
    /// Send message to the WebSocket connection identified by its ID
    function Send(aID: THttpApiWebSocketConnectionID; aBufferType: ULONG;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
    /// Send message to all connections of this protocol
    function Broadcast(aBufferType: ULONG;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
    /// Close WebSocket connection identified by its ID
    function Close(aID: THttpApiWebSocketConnectionID;
      aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: pointer; aBufferSize: ULONG): boolean;
  end;

  THttpApiWebSocketServerProtocolDynArray = array of THttpApiWebSocketServerProtocol;

  /// HTTP & WebSocket server using fast http.sys kernel-mode server
  // - can be used like simple THttpApiServer
  // - when AddUrlWebSocket() is called, WebSocket support is added
  // and the frames will be received in asynchronous mode
  THttpApiWebSocketServer = class(THttpApiServer)
  protected
    fOwnedProtocolsSafe: TLightLock;
    fPingTimeout: integer;
    fThreadPoolServer: TSynThreadPoolHttpApiWebSocketServer;
    fOnWSThreadStart: TOnNotifyThread;
    fOnWSThreadTerminate: TOnNotifyThread;
    fServiceOverlaped: TOverlapped;
    fOnServiceMessage: TThreadMethod;
    fOwnedProtocols: THttpApiWebSocketServerProtocolDynArray;
    fWsStopping: boolean;
    function UpgradeToWebSocket(Ctxt: THttpServerRequestAbstract): cardinal;
    procedure FinishUpgrade(conn: PHttpApiWebSocketConnection;
      Success: boolean);
    procedure DoAfterResponse(Ctxt: THttpServerRequest; const Referer: RawUtf8;
      StatusCode: cardinal; Elapsed, Received, Sent: QWord); override;
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; override;
    procedure DoIdle(tix64: Int64; sec32: cardinal); override;
    procedure DestroyMainThread; override;
  public
    /// initialize the HTTPAPI based Server with WebSocket support
    // - will raise an exception if http.sys or websocket.dll is not available
    // (e.g. before Windows 8) or if the request queue creation failed
    // - for aPingTimeout explanation see PingTimeout property documentation
    constructor Create(aSocketThreadsCount: integer = 1;
      aPingTimeout: integer = 0; const QueueName: SynUnicode = '';
      const aOnWSThreadStart: TOnNotifyThread = nil;
      const aOnWSThreadTerminate: TOnNotifyThread = nil;
      ProcessOptions: THttpServerOptions = []); reintroduce;
    /// prepare the process for a given THttpApiWebSocketServerProtocol
    procedure RegisterProtocol(const aName: RawUtf8; aManualFragmentManagement: boolean;
      const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
      const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
      const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
      const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
      const aOnFragment: TOnHttpApiWebSocketServerMessageEvent = nil);
    /// register the URLs to Listen on using WebSocket
    // - aProtocols is an array of a recond with callbacks, server call during
    // WebSocket activity
    function AddUrlWebSocket(const aRoot, aPort: RawUtf8; Https: boolean = false;
      const aDomainName: RawUtf8 = '*'; aRegisterUri: boolean = false): integer;
    /// handle the HTTP request
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
    /// Ping timeout in seconds. 0 mean no ping.
    // - send ping frames on connection idle more than this timeout
    // - any connection idle longer than double of this timeout will be closed
    property PingTimeout: integer
      read fPingTimeout write fPingTimeout;
    /// access to the associated endpoints as a thread-safe array copy
    function GetRegisteredProtocols: THttpApiWebSocketServerProtocolDynArray;
    /// event called when the processing thread starts
    property OnWSThreadStart: TOnNotifyThread
      read fOnWSThreadStart write fOnWSThreadStart;
    /// event called when the processing thread termintes
    property OnWSThreadTerminate: TOnNotifyThread
      read fOnWSThreadTerminate write fOnWSThreadTerminate;
    /// send a "service" message to a WebSocketServer to wake up a WebSocket thread
    // - can be called from any thread
    // - when a webSocket thread receives such a message it will call onServiceMessage
    // in the thread context
    procedure SendServiceMessage;
    /// event called when a service message is raised
    property OnServiceMessage: TThreadMethod
      read fOnServiceMessage write fOnServiceMessage;
  end;

  /// a Thread Pool, used for fast handling of WebSocket requests
  TSynThreadPoolHttpApiWebSocketServer = class(TSynThreadPool)
  protected
    fServer: THttpApiWebSocketServer;
    procedure OnThreadStart(Sender: TThreadAbstract);
    procedure OnThreadTerminate(Sender: TThreadAbstract);
    function NeedStopOnIOError: boolean; override;
    // aContext is a PHttpApiWebSocketConnection, or fServer.fServiceOverlaped
    // (SendServiceMessage) or fServer.fSendOverlaped (WriteData)
    procedure Task(aCaller: TSynThreadPoolWorkThread;
      aContext: pointer); override;
  public
    /// initialize the thread pool
    constructor Create(Server: THttpApiWebSocketServer;
      NumberOfThreads: integer = 1); reintroduce;
  end;


implementation


{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

{ THttpApiServerRequest }

type
  // private per-thread sub-class to hold the response logic and reusable variables
  THttpApiServerRequest = class(THttpServerRequest)
  protected
    function GetFullUrl: RawUtf8; override;
  public
    api: THttpApiServer;
    req: PHTTP_REQUEST; // 16KB first, may be upsized by THttpApiServer.DoExecute
    ws: PHttpApiWebSocketConnection;
    reqid: HTTP_REQUEST_ID;
    resp: HTTP_RESPONSE;
    bytesread, bytessent, flags: cardinal;
    compressset: THttpSocketCompressSet;
    heads: HTTP_UNKNOWN_HEADERS;
    chunkbuf: TBytes;
    incontenc, inaccept, srvhost, referer, errmsg, outmsg: RawUtf8;
    fn: TFileName;
    datachunkmem: HTTP_DATA_CHUNK_INMEMORY;
    logbuf: HTTP_LOG_FIELDS_DATA;
    tmp: ShortString;
    procedure FromReq(var Value: RawUtf8; Head: THttpApiHeader);
    procedure ReqToLog(var log: HTTP_LOG_FIELDS_DATA);
    function HttpSendResponse(flags: cardinal): boolean;
    procedure SendError(StatusCode: cardinal; const ErrorMsg: RawUtf8;
      Disconnect: boolean = false; E: Exception = nil);
    function SendResponse: boolean;
    procedure ProcessRequest;
  end;

function THttpApiServerRequest.GetFullUrl: RawUtf8;
begin
  if req = nil then
    result := ''
  else
    RawUnicodeToUtf8(req^.CookedUrl.pFullUrl,
      req^.CookedUrl.FullUrlLength shr 1, result); // FullUrlLength in bytes
end;

procedure THttpApiServerRequest.ReqToLog(var log: HTTP_LOG_FIELDS_DATA);
var
  hdr: PHTTP_KNOWN_HEADER;
begin
  log.ServerNameLength   := length(api.fServerName);
  log.ServerName         := pointer(api.fServerName);
  log.ServiceNameLength  := length(api.fLoggingServiceName);
  log.ServiceName        := pointer(api.fLoggingServiceName);
  log.ProtocolStatus     := resp.StatusCode;
  log.MethodNum          := req^.Verb;
  log.UriStemLength      := req^.CookedUrl.AbsPathLength;
  log.UriStem            := req^.CookedUrl.pAbsPath;
  hdr := @req^.headers.KnownHeaders[reqUserAgent];
  log.UserAgentLength    := hdr^.RawValueLength;
  log.UserAgent          := hdr^.pRawValue;
  hdr := @req^.headers.KnownHeaders[reqHost];
  log.HostLength         := hdr^.RawValueLength;
  log.Host               := hdr^.pRawValue;
  hdr := @req^.headers.KnownHeaders[reqReferrer];
  log.ReferrerLength     := hdr^.RawValueLength;
  log.Referrer           := hdr^.pRawValue;
  log.ClientIp           := pointer(fRemoteIP);
  log.ClientIpLength     := length(fRemoteIP);
  log.Method             := pointer(fMethod);
  log.MethodLength       := length(fMethod);
  log.UserName           := pointer(fAuthenticatedUser);
  log.UserNameLength     := Length(fAuthenticatedUser);
end;

procedure THttpApiServerRequest.FromReq(var Value: RawUtf8; Head: THttpApiHeader);
var
  hdr: PHTTP_KNOWN_HEADER;
begin
  hdr := @req^.Headers.KnownHeaders[Head];
  FastSetString(Value, hdr^.pRawValue, hdr^.RawValueLength);
end;

function THttpApiServerRequest.HttpSendResponse(flags: cardinal): boolean;
var
  log: PHTTP_LOG_FIELDS_DATA;
  hdr: PHTTP_KNOWN_HEADER;
  err: HRESULT;
begin
  // update log information
  if api.fLogging then // after LogStart (and http.sys API v2)
  begin
    ReqToLog(logbuf);
    log := @logbuf;
  end
  else
    log := nil;
  // send the resp HTTP response to the req^ HTTP request
  resp.Version := req^.Version;
  hdr := @resp.headers.KnownHeaders[respServer];
  hdr^.pRawValue      := pointer(api.fServerName);
  hdr^.RawValueLength := length(api.fServerName);
  include(fInternalFlags, ifRespSent); // response submission attempted
  err := Http.SendHttpResponse(api.fReqQueue, req^.RequestId, flags, resp, nil,
      bytessent, nil, 0, nil, log);
  result := err = NO_ERROR;
  if not result then
    api.CheckAndLog(hSendHttpResponse, err);
  fRespStatus := resp.StatusCode;
  FillcharFast(resp, SizeOf(resp), 0); // reset
end;

procedure THttpApiServerRequest.SendError(StatusCode: cardinal;
  const ErrorMsg: RawUtf8; Disconnect: boolean; E: Exception);
var
  flags: cardinal;
  hdr: PHTTP_KNOWN_HEADER;
begin
  try
    FormatUtf8('<!DOCTYPE html><html><body style="font-family:verdana;">' +
      '<h1>Server Error %: %</h1><p>',
      [StatusCode, resp.SetStatus(StatusCode)^], outmsg);
    if E <> nil then
      if E.InheritsFrom(EHttpApiServer) and // ensure still connected
         (EHttpApiServer(E).LastApiError = HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
        exit // nothing to send back on a broken connection
      else
        FormatShort('% raised', [E], tmp) // don't include E.Message in the HTML
    else
      HtmlEscapeShortVar(ErrorMsg, tmp);
    Append(outmsg, [tmp, '</p><p><small>' + XPOWEREDVALUE]);
    resp.SetContent(datachunkmem, outmsg, HTML_CONTENT_TYPE);
    flags := 0;
    if Disconnect then
    begin
      hdr := @resp.headers.KnownHeaders[reqConnection];
      hdr^.pRawValue := 'close';
      hdr^.RawValueLength := 5;
      flags := HTTP_SEND_RESPONSE_FLAG_DISCONNECT;
    end;
    HttpSendResponse(flags);
    if hsoLogVerbose in api.fOptions then
      api.fLogClass.Add.Log(sllTrace, 'SendError(%)=% %',
        [srvhost, StatusCode, fRemoteIP], self);
  except
    on Exception do
      ; // ignore any HttpApi level errors here (client may have crashed)
  end;
end;

function THttpApiServerRequest.SendResponse: boolean;
var
  R: PUtf8Char;
  flags: cardinal;
  filehandle: THandle;
  comprec: PHttpSocketCompressRec;
  size, start, rangelen: Qword;
  hdr: PHTTP_KNOWN_HEADER;
  chunk: HTTP_DATA_CHUNK_FILEHANDLE;
begin
  result := false; // true=success
  if fServer.Terminated then
    exit;
  if not OutContentStreamToBuffer then // kernel http.sys needs buffer
    fRespStatus := HTTP_SERVERERROR; // failed to read that stream
  resp.SetStatus(fRespStatus);
  // associate response headers
  resp.SetHeaders(pointer(OutCustomHeaders), heads,
    hsoNoXPoweredHeader in api.fOptions);
  if OutContentType = STATICFILE_CONTENT_TYPE then
  begin
    // response is file -> OutContent is UTF-8 file name to be served
    Utf8ToFileName(OutContent, fn);
    filehandle := FileOpen(fn, fmOpenReadShared);
    if not ValidHandle(filehandle)  then
    begin
      SetErrorText(errmsg); // text message from OS
      SendError(HTTP_NOTFOUND, errmsg);
      exit;
    end;
    try
      // http.sys will serve then close filehandle from kernel
      chunk.DataChunkType := hctFromFileHandle;
      chunk.FileHandle := filehandle;
      flags := 0;
      chunk.ByteRange.StartingOffset.QuadPart := 0;
      PInt64(@chunk.ByteRange.Length.QuadPart)^ := -1; // to eof
      hdr := @req^.headers.KnownHeaders[reqRange];
      if (hdr^.RawValueLength > 6) and
         (hdr^.RawValueLength < 255) and
         IdemPChar(pointer(hdr^.pRawValue), 'BYTES=') and
         (hdr^.pRawValue[6] in ['0'..'9']) then
      begin
        SetString(tmp, hdr^.pRawValue + 6, hdr^.RawValueLength - 6);
        AppendShortChar(#0, @tmp); // GetNextRange(R) requires #0 ended
        R := @tmp[1];
        start := GetNextRange(R);
        if R^ = '-' then
        begin
          inc(R);
          size := FileSize(filehandle);
          if start >= size then
          begin
            SendError(HTTP_RANGENOTSATISFIABLE, 'Invalid Range');
            exit;
          end;
          chunk.ByteRange.Length.QuadPart := size - start;
          flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
          chunk.ByteRange.StartingOffset.QuadPart := start;
          if R^ in ['0'..'9'] then
          begin
            rangelen := GetNextRange(R) - start + 1;
            if Int64(rangelen) < 0 then
              rangelen := 0;
            if rangelen < chunk.ByteRange.Length.QuadPart then
               // "bytes=0-499" -> start=0, len=500
              chunk.ByteRange.Length.QuadPart := rangelen;
          end; // "bytes=1000-" -> start=1000, to eof
          FormatShort('bytes %-%/%',
            [start, start + chunk.ByteRange.Length.QuadPart - 1, size], tmp);
          hdr := @resp.headers.KnownHeaders[reqContentRange];
          hdr^.pRawValue := @tmp[1];
          hdr^.RawValueLength := length(tmp);
          resp.SetStatus(HTTP_PARTIALCONTENT);
        end;
      end;
      hdr := @resp.headers.KnownHeaders[respAcceptRanges];
      hdr^.pRawValue := 'bytes';
      hdr^.RawValueLength := 5;
      resp.EntityChunkCount := 1;
      resp.pEntityChunks := @chunk;
      result := HttpSendResponse(flags);
    finally
      FileClose(filehandle);
    end;
  end
  else
  begin
    // response is in OutContent -> send it from memory
    if OutContentType = NORESPONSE_CONTENT_TYPE then
      OutContentType := ''; // true HTTP always expects a response
    if (integer(compressset) <> 0) and
       (api.fCompressList.Algo <> nil) then
    begin
      hdr := @resp.headers.KnownHeaders[reqContentEncoding];
      if hdr^.RawValueLength = 0 then // no previous encoding -> try compression
      begin
        comprec := api.fCompressList.CompressContent(
          compressset, OutContentType, fOutContent);
        if comprec <> nil then
        begin
          hdr^.pRawValue      := pointer(comprec^.Name);
          hdr^.RawValueLength := length(comprec^.Name);
        end;
      end;
    end;
    resp.SetContent(datachunkmem, OutContent, OutContentType);
    result := HttpSendResponse(api.GetSendResponseFlags(self));
  end;
end;

procedure THttpApiServerRequest.ProcessRequest;
var
  err: HRESULT;
  bodyknown: boolean;
  afterstatcode, chunk: cardinal;
  hdr: PHTTP_KNOWN_HEADER;
  bufread, V: PUtf8Char;
  i, incontlenread: PtrInt;
  incontlen, remain: Qword;
  started, elapsed: Int64;
begin
  // parse method and main headers as ctxt.Prepare() does
  bytessent := 0;
  ws := nil;
  Recycle(req^.ConnectionID, fServer, {asynchandle=}0,
    HTTP_TLS_FLAGS[req^.pSslInfo <> nil] +
    // no HTTP_UPG_FLAGS[]: plain THttpApiServer don't support upgrade
    HTTP_10_FLAGS[(req^.Version.MajorVersion = 1) and
                  (req^.Version.MinorVersion = 0)],
    // ctxt.fConnectionOpaque is not supported by http.sys
    nil);
  FastSetString(fUrl, req^.pRawUrl, req^.RawUrlLength);
  if req^.Verb in [low(VERB_TEXT)..high(VERB_TEXT)] then
    fMethod := VERB_TEXT[req^.Verb]
  else
    FastSetString(fMethod, req^.pUnknownVerb, req^.UnknownVerbLength);
  FromReq(fInContentType, reqContentType);
  FromReq(fUserAgent, reqUserAgent);
  FromReq(fHost, reqHost);
  srvhost := fHost; // may be reset during Request()
  hdr := @req^.Headers.KnownHeaders[reqAuthorization];
  if (hdr^.RawValueLength > 7) and
     IdemPChar(pointer(hdr^.pRawValue), 'BEARER ') then
    FastSetString(fAuthBearer, hdr^.pRawValue + 7, hdr^.RawValueLength - 7);
  FromReq(inaccept, reqAcceptEncoding);
  FromReq(referer, reqReferrer);
  api.fCompressList.DecodeAcceptEncoding(pointer(inaccept), compressset);
  RetrieveHeadersAndGetRemoteIPConnectionID(req^,
    api.fRemoteIPHeaderUpper, api.fRemoteConnIDHeaderUpper, {out}fRemoteIP,
    PQWord(@fConnectionID)^, fInHeaders);
  // retrieve any SetAuthenticationSchemes() information
  if byte(api.fAuthenticationSchemes) <> 0 then // set only with HTTP API 2.0
    // https://docs.microsoft.com/en-us/windows/win32/http/authentication-in-http-version-2-0
    for i := 0 to req^.RequestInfoCount - 1 do
      with req^.pRequestInfo^[i] do
      if InfoType = HttpRequestInfoTypeAuth then
        case pInfo^.AuthStatus of
          HttpAuthStatusSuccess:
            if pInfo^.AuthType > HttpRequestAuthTypeNone then
            begin
              byte(fAuthenticationStatus) := ord(pInfo^.AuthType) + 1;
              if pInfo^.AccessToken <> 0 then
              begin
                LookupTokenVar(pInfo^.AccessToken, '', fAuthenticatedUser);
                // AccessToken lifecycle is application responsibility
                CloseHandle(pInfo^.AccessToken);
                fAuthBearer := fAuthenticatedUser;
                include(fConnectionFlags, hsrAuthorized);
              end;
            end;
          HttpAuthStatusFailure:
            fAuthenticationStatus := hraFailed;
        end;
  // abort request if > MaximumAllowedContentLength or OnBeforeBody
  hdr := @req^.headers.KnownHeaders[reqContentLength];
  V := pointer(hdr^.pRawValue);
  SetQWord(V, V + hdr^.RawValueLength, incontlen);
  if (incontlen > 0) and
     (api.fMaximumAllowedContentLength > 0) and
     (incontlen > QWord(api.fMaximumAllowedContentLength)) then
  begin
    SendError(HTTP_PAYLOADTOOLARGE, 'Rejected', {disconnect=}true);
    exit;
  end;
  if (hsoRejectBotUserAgent in api.fOptions) and
     (fUserAgent <> '') and
     IsHttpUserAgentBot(fUserAgent) then
  begin
    SendError(HTTP_TEAPOT, BOTBUSTER_RESPONSE, {disconnect=}true);
    exit;
  end;
  if Assigned(fServer.OnBeforeBody) then
  begin
    try
      err := fServer.OnBeforeBody(fUrl, fMethod, fInHeaders, fInContentType,
        fRemoteIP, fAuthBearer, incontlen, ConnectionFlags);
    except
      err := HTTP_SERVERERROR;
    end;
    if err <> HTTP_SUCCESS then
    begin
      SendError(err, 'Rejected', {disconnect=}true);
      exit;
    end;
  end;
  // retrieve body
  incontlenread := 0; // actual number of entity-body bytes received
  if (HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and req^.flags) <> 0 then
  begin
    errmsg := '';
    FromReq(incontenc, reqContentEncoding);
    bodyknown := (incontlen <> 0) and // transfer-encoding has precedence
      (req^.headers.KnownHeaders[reqTransferEncoding].RawValueLength = 0);
    if bodyknown then
    begin
      chunk := MaxPtrUInt(64 * 1024, api.fReceiveBufferSize); // 1MB default
      if chunk > incontlen then
        chunk := incontlen;
      bufread := FastNewRawByteString(fInContent, incontlen);
    end
    else
    begin
      chunk := 128 shl 10; // chunked doesn't need fReceiveBufferSize
      if chunkbuf = nil then
        SetLength(chunkbuf, chunk); // allocate once 128KB
      bufread := pointer(chunkbuf);
    end;
    if api.HasApi2 then // speed optimization for Vista+ = whole chunk
      flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER
    else
      flags := 0;
    repeat
      bytesread := 0;
      err := Http.ReceiveRequestEntityBody(api.fReqQueue,
        req^.RequestId, flags, bufread, chunk, @bytesread);
      if fServer.Terminated then
        exit;
      if err = ERROR_HANDLE_EOF then
      begin
        if incontlenread < length(fInContent) then
          SetLength(fInContent, incontlenread);
        err := NO_ERROR;
        break; // we reached the end of this body
      end;
      if err <> NO_ERROR then
      begin
        api.CheckAndLog(hReceiveRequestEntityBody, err, sllDebug, @errmsg);
        err := HTTP_NOTACCEPTABLE;
        break;
      end;
      inc(incontlenread, bytesread);
      if bodyknown then
      begin
        remain := incontlen - QWord(incontlenread);
        if remain = 0 then
          break; // reached the end of Content-Length
        inc(bufread, bytesread);
        if remain < chunk then
          chunk := remain; // avoid buffer overflow
      end
      else if (api.fMaximumAllowedContentLength > 0) and
              (incontlenread > api.fMaximumAllowedContentLength) then
        err := HTTP_PAYLOADTOOLARGE
      else if fInContent = '' then // initial maybe-single chunk
        FastSetRawByteString(fInContent, bufread, bytesread)
      else
      begin
        if length(fInContent) < incontlenread then
          SetLength(fInContent, NextGrow(incontlenread));
        MoveFast(bufread^, PByteArray(fInContent)[
          incontlenread - PtrInt(bytesread)], bytesread);
      end;
    until err <> NO_ERROR;
    if err <> NO_ERROR then
    begin
      SendError(err, errmsg, {disconnect=}true);
      exit;
    end;
    if (incontenc <> '') and // optionally uncompress input body
       (api.fCompressList.UncompressContent(incontenc, fInContent) = nil) and
       (api.fCompressList.CompressIndex(incontenc) >= 0) then
    begin
      SendError(HTTP_BADREQUEST, incontenc); // keep the connection
      exit;
    end;
  end;
  exclude(fInternalFlags, ifRespSent);
  QueryPerformanceMicroSeconds(started);
  try
    // compute response
    fRespStatus := 0;
    if api.fRoute <> nil then
      // URI rewrite or event callback execution
      fRespStatus := api.fRoute.Process(self);
    if fRespStatus = 0 then // no router callback was executed
    begin
      // regular server-side OnRequest execution
      fRespStatus := api.DoBeforeRequest(self);
      if fRespStatus > 0 then
        if not SendResponse or
           (fRespStatus <> HTTP_ACCEPTED) then
          exit;
      fRespStatus := fServer.Request(self); // call OnRequest for main process
      afterstatcode := api.DoAfterRequest(self);
      if afterstatcode > 0 then
        fRespStatus := afterstatcode;
    end;
    // send response - SendResponse does buffer any SetOutStream()
    if ifRespSent in fInternalFlags then // e.g. 202 already sent
      OutContentStreamDiscard            // we can release SetOutStream()
    else if not SendResponse then
    begin
      if ws <> nil then
      try
        (fServer as THttpApiWebSocketServer).FinishUpgrade(ws, false);
      finally
        ws := nil;
      end;
      exit;
    end;
    QueryPerformanceMicroSeconds(elapsed);
    dec(elapsed, started);
    fHost := srvhost; // may have been reset during Request()
    api.DoAfterResponse(self, referer, fRespStatus, elapsed, incontlenread, bytessent);
    if hsoLogVerbose in api.fOptions then
      api.fLogClass.Add.Log(sllTrace, 'ProcessRequest(%)=% % %us',
        [srvhost, fRespStatus, fRemoteIP, elapsed], self);
  except
    // handle any exception raised during process
    on E: Exception do
    begin
      if ws <> nil then
      try
        (fServer as THttpApiWebSocketServer).FinishUpgrade(ws, false);
      finally
        ws := nil;
      end;
      OutContentStreamDiscard; // eventually release SetOutStream()
      if not (ifRespSent in fInternalFlags) then
        SendError(HTTP_SERVERERROR, 'Exception raised', false, E);
    end;
  end;
end;


{ THttpApiServerThread }

procedure THttpApiServerThread.DoExecute;
begin
  if fOwner = nil then
    exit;
  fStarted := true;
  fOwner.DoExecute; // this main method is re-entrant by design
end;

constructor THttpApiServerThread.Create(aOwner: THttpApiServer);
begin
  fOwner := aOwner;
  inherited Create({suspended=}false, aOwner.fOnThreadStart,
    aOwner.fOnThreadTerminate, aOwner.fLogClass,
    Make([aOwner.ProcessName, ' #', length(aOwner.fThreads) + 1]));
end;


{ THttpApiServer }

class function THttpApiServer.HasApi2: boolean;
begin
  result := Http.Version.MajorVersion >= 2;
end;

function THttpApiServer.AddUrl(const aRoot, aPort: RawUtf8; Https: boolean;
  const aDomainName: RawUtf8; aRegisterUri: boolean; aContext: Int64): integer;
var
  uri: SynUnicode;
  n: PtrInt;
begin
  result := -1;
  if (Self = nil) or
     (fReqQueue = 0) or
     (Http.Module = 0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri = '' then
    exit; // invalid parameters
  if aRegisterUri then
    AddUrlAuthorize(aRoot, aPort, Https, aDomainName);
  if HasApi2 then
    result := CheckAndLog(hAddUrlToUrlGroup,
      Http.AddUrlToUrlGroup(fUrlGroupID, pointer(uri), aContext))
  else
    result := CheckAndLog(hAddUrl,
      Http.AddUrl(fReqQueue, pointer(uri)));
  if result <> NO_ERROR then
    exit;
  n := length(fRegisteredUnicodeUrl);
  SetLength(fRegisteredUnicodeUrl, n + 1);
  fRegisteredUnicodeUrl[n] := uri;
end;

function THttpApiServer.RemoveUrl(const aRoot, aPort: RawUtf8; Https: boolean;
  const aDomainName: RawUtf8): integer;
var
  uri: SynUnicode;
  i, j, n: PtrInt;
begin
  result := -1;
  if (Self = nil) or
     (fReqQueue = 0) or
     (Http.Module = 0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri = '' then
    exit; // invalid parameters
  n := High(fRegisteredUnicodeUrl);
  for i := 0 to n do
    if fRegisteredUnicodeUrl[i] = uri then
    begin
      if HasApi2 then
        result := CheckAndLog(hRemoveUrlFromUrlGroup,
          Http.RemoveUrlFromUrlGroup(fUrlGroupID, pointer(uri), 0))
      else
        result := CheckAndLog(hRemoveUrl,
          Http.RemoveUrl(fReqQueue, pointer(uri)));
      if result <> NO_ERROR then
        exit; // shall be handled by caller
      for j := i to n - 1 do
        fRegisteredUnicodeUrl[j] := fRegisteredUnicodeUrl[j + 1];
      SetLength(fRegisteredUnicodeUrl, n);
      exit;
    end;
end;

class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: RawUtf8;
  Https: boolean; const aDomainName: RawUtf8; OnlyDelete: boolean): RawUtf8;
begin // just redirect to our wrapper in mormot.lib.winhttp
  result := HttpApiAuthorize(aRoot, aPort, Https, aDomainName, OnlyDelete);
end;

function THttpApiServer.GetApiVersion: RawUtf8;
begin
  FormatUtf8('http.sys %.%',
    [Http.Version.MajorVersion, Http.Version.MinorVersion], result);
end;

function THttpApiServer.CheckAndLog(Api: THttpApiFunction; Error: integer;
  Level: TSynLogLevel; ErrMsg: PRawUtf8): integer;
var
  msg: RawUtf8;
begin
  result := Error;
  if HttpApiSucceed(Api, msg, Error) then
    exit;
  if Assigned(fLogClass) then
    fLogClass.Add.Log(Level, msg, self);
  if ErrMsg <> nil then
    ErrMsg^ := msg;
end;

procedure THttpApiServer.Ensure(Api: THttpApiFunction; Error: integer);
begin
  if Assigned(fLogClass) then
    fLogClass.Add.Log(sllTrace, '%=%', [HttpApiFunction[Api], Error], self);
  EHttpApiServer.RaiseOnError(Api, Error);
end;

constructor THttpApiServer.Create(QueueName: SynUnicode;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ProcessOptions: THttpServerOptions; aLog: TSynLogClass;
  ServerThreadPoolCount: integer);
var
  log: ISynLog;
  binding: HTTP_BINDING_INFO;
  i: PtrInt;
begin
  aLog.EnterLocal(log, 'Create(%) processname=% threads=%',
    [QueueName, ProcessName, ServerThreadPoolCount], self);
  ProcessOptions := ProcessOptions - [hsoThreadCpuAffinity,
    hsoThreadSocketAffinity, hsoReusePort, hsoThreadSmooting];
  inherited Create(OnStart, OnStop, ProcessName, ProcessOptions, aLog);
  // create the Request Queue
  HttpApiInitialize; // will raise an exception in case of failure
  if Assigned(log) then
    log.Log(sllHttp, 'Create: new % handle', [GetApiVersion], self);
  Ensure(hInitialize,
    Http.Initialize(Http.Version, HTTP_INITIALIZE_SERVER));
  if HasApi2 then
  begin
    Ensure(hCreateServerSession,
      Http.CreateServerSession(Http.Version, fServerSessionID));
    Ensure(hCreateUrlGroup,
      Http.CreateUrlGroup(fServerSessionID, fUrlGroupID));
    if QueueName = '' then
      Utf8ToSynUnicode(Int64ToUtf8(fServerSessionID), QueueName);
    Ensure(hCreateRequestQueue,
      Http.CreateRequestQueue(Http.Version, pointer(QueueName), nil, 0, fReqQueue));
    binding.Flags := 1;
    binding.RequestQueueHandle := fReqQueue;
    Ensure(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID,
        HttpServerBindingProperty, @binding, SizeOf(binding)));
  end
  else
    Ensure(hCreateHttpHandle,
      Http.CreateHttpHandle(fReqQueue));
  // DoIdle() processing thread once per second
  fIdleThread := TSynBackgroundThreadProcess.Create(
    'HttpApiIdle', OnIdleProcess, 1000);
  // start the other processing threads
  fReceiveBufferSize := 1 shl 20; // i.e. 1 MB
  if Assigned(log) then
    log.Log(sllTrace, 'Create: start threads', self);
  for i := 2 to ServerThreadPoolCount do
    PtrArrayAdd(fThreads, THttpApiServerThread.Create(self));
  // eventually start the main thread
  Append(fProcessName, [' #', ServerThreadPoolCount]);
end;

function THttpApiServer.WaitStarted(Seconds: cardinal): boolean;
var
  tix32: cardinal;
  i: PtrInt;
begin
  result := false;
  if fReceiveBufferSize = 0 then
    exit; // Create never reached actual activation
  tix32 := GetTickSec + Seconds; // never wait forever
  repeat
    SleepHiRes(1);  // warning: waits typically 1-15 ms on Windows
    result := true; // fProcessing may be false if main thread did abort
    for i := 0 to high(fThreads) do
      result := result and ((fThreads[i] = nil) or fThreads[i].fStarted);
  until result or
        (GetTickSec > tix32);
  fLogClass.Add.Log(sllTrace, 'WaitStarted(%)=%', [Seconds, BOOL_STR[result]], self);
end;

procedure THttpApiServer.DestroyMainThread;
var
  i: PtrInt;
begin
  if fReqQueue <> 0 then
  begin
    fLogClass.Add.Log(sllTrace, 'DestroyMainThread=%', [length(fThreads)], self);
    for i := 0 to high(fThreads) do
      if fThreads[i] <> nil then
        fThreads[i].Terminate; // for CloseHandle() below to finish Execute
    if HasApi2 then
    begin
      if fUrlGroupID <> 0 then
      begin
        CheckAndLog(hRemoveUrlFromUrlGroup,
          Http.RemoveUrlFromUrlGroup(fUrlGroupID, nil, HTTP_URL_FLAG_REMOVE_ALL));
        CheckAndLog(hCloseUrlGroup,
          Http.CloseUrlGroup(fUrlGroupID));
        fUrlGroupID := 0;
      end;
      CloseHandle(fReqQueue);
      if fServerSessionID <> 0 then
      begin
        CheckAndLog(hCloseServerSession,
          Http.CloseServerSession(fServerSessionID));
        fServerSessionID := 0;
      end;
    end
    else
    begin
      for i := 0 to high(fRegisteredUnicodeUrl) do
        CheckAndLog(hRemoveUrl,
          Http.RemoveUrl(fReqQueue, pointer(fRegisteredUnicodeUrl[i])));
      CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
    end;
    fReqQueue := 0;
    {$ifdef FPC}
    for i := 0 to high(fThreads) do
      if fThreads[i] <> nil then
        WaitForSingleObject(fThreads[i].Handle, 30000); // maybe needed on FPC
    {$endif FPC}
    ObjArrayClear(fThreads, {continueOnException:}true);
    CheckAndLog(hTerminate,
      Http.Terminate(HTTP_INITIALIZE_SERVER));
  end;
end;

destructor THttpApiServer.Destroy;
begin
  Terminate; // for Execute to be notified about end of process
  try
    FreeAndNilSafe(fIdleThread);
    if Http.Module <> 0 then
      DestroyMainThread;
    {$ifdef FPC}
    WaitForSingleObject(Handle, 30000); // sometimes needed on FPC
    {$endif FPC}
  finally
    inherited Destroy;
  end;
end;

procedure THttpApiServer.OnIdleProcess(Sender: TSynBackgroundThreadProcess);
begin
  // executed by fIdleThread every second
  fIdleTix64 := mormot.core.os.GetTickCount64;
  DoIdle(fIdleTix64, cardinal(fIdleTix64 div 1000)); // virtual method
end;

function THttpApiServer.GetSendResponseFlags(Ctxt: THttpServerRequest): integer;
begin
  result := 0;
end;

procedure THttpApiServer.DoExecute;
var
  reqbuf: TBytes;
  ctx: THttpApiServerRequest; // used as per-thread recycled state machine
  err: HRESULT;
begin
  if Terminated then
    exit;
  ctx := THttpApiServerRequest.Create(self, 0, self, 0, [], nil);
  try
    // reserve working buffers
    SetLength(ctx.heads, 64);
    SetLength(reqbuf, 16384 + SizeOf(HTTP_REQUEST)); // req^ + 16 KB of headers
    ctx.req  := pointer(reqbuf);
    ctx.fServer := self;
    ctx.api := self; // as THttpApiServer
    ctx.reqid := 0;
    // main loop reusing a single ctxt instance for this thread
    repeat
      // release input/output body buffers ASAP
      ctx.fInContent := '';
      ctx.fOutContent := '';
      // retrieve next pending request, and read its headers
      FillcharFast(ctx.req^, SizeOf(HTTP_REQUEST), 0);
      err := Http.ReceiveHttpRequest(fReqQueue, ctx.reqid, 0,
        ctx.req^, length(reqbuf), ctx.bytesread); // blocking until received something
      if Terminated then
        break;
      case err of
        NO_ERROR:
          begin
            LockedInc32(@fCurrentProcess);
            try
              ctx.ProcessRequest;
            except
              on E: Exception do
                fLogClass.Add.Log(sllWarning, 'DoExecute: uncatched %', [E], self);
            end;
            LockedDec32(@fCurrentProcess);
            ctx.reqid := 0; // reset Request ID to handle the next pending request
          end;
        ERROR_MORE_DATA:
          begin
            // input buffer was too small to hold the request headers
            fLogClass.Add.Log(sllDebug,
              'DoExecute: increase buffer size to %', [ctx.bytesread], self);
            ctx.reqid := ctx.req^.RequestId;
            SetLength(reqbuf, ctx.bytesread);
            ctx.req := pointer(reqbuf); // will try again
          end;
        ERROR_CONNECTION_INVALID,
        ERROR_INVALID_PARAMETER,
        ERROR_HANDLE_EOF:
          begin
            CheckAndLog(hReceiveHttpRequest, err);
            if ctx.reqid = 0 then
              break;        // fatal error (e.g. API misuse)
            ctx.reqid := 0; // error was specific to this TCP connection
          end;
      else
        begin
          CheckAndLog(hReceiveHttpRequest, err);
          break; // unhandled err value
        end;
      end;
    until Terminated;
  finally
    ctx.Free;
  end;
end;

function THttpApiServer.GetHttpQueueLength: cardinal;
var
  n: cardinal; // safer with an explicit local variable
begin
  result := 0;
  if (self = nil) or
     (fReqQueue = 0) or
     not HasApi2 then
    exit;
  EHttpApiServer.RaiseOnError(hQueryRequestQueueProperty,
    Http.QueryRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
      @n, SizeOf(n)));
  result := n;
end;

procedure THttpApiServer.SetHttpQueueLength(aValue: cardinal);
begin
  EHttpApiServer.RaiseCheckApi2(hSetRequestQueueProperty);
  if (self <> nil) and
     (fReqQueue <> 0) then
    Ensure(hSetRequestQueueProperty,
      Http.SetRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
        @aValue, SizeOf(aValue)));
end;

function THttpApiServer.GetConnectionsActive: cardinal;
begin
  result := 0; // unsupported
end;

function THttpApiServer.GetRegisteredUrl: RawUtf8;
var
  i: PtrInt;
begin
  result := fRegisteredUrl;
  if (result <> '') or
     (fRegisteredUnicodeUrl = nil) then
    exit;
  fRegisteredUrl := SynUnicodeToUtf8(fRegisteredUnicodeUrl[0]);
  for i := 1 to high(fRegisteredUnicodeUrl) do
    Append(fRegisteredUrl, [',', fRegisteredUnicodeUrl[i]]);
  result := fRegisteredUrl;
end;

function THttpApiServer.GetProperty(dest: PHTTP_QOS_SETTING_INFO;
  destlen: cardinal; qos: HTTP_QOS_SETTING_TYPE): boolean;
begin
  result := false;
  if (self = nil) or
     not HasApi2 then
    exit;
  if fUrlGroupID = 0 then
    exit;
  dest.QosType := qos;
  dest.QosSetting := PAnsiChar(dest) + SizeOf(dest^); // should be after header
  FillCharFast(dest.QosSetting^, destlen - SizeOf(dest^), 0); // for safety
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty, dest, destlen));
  result := true;
end;

procedure THttpApiServer.SetProperty(value: PHTTP_QOS_SETTING_INFO;
  valuelen: cardinal; qos: HTTP_QOS_SETTING_TYPE; alsoForSession: boolean);
begin
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  if fUrlGroupID = 0 then
    exit;
  value.QosType := qos;
  value.QosSetting := PAnsiChar(value) + SizeOf(value^); // just after header
  if alsoForSession then
    Ensure(hSetServerSessionProperty,
      Http.SetServerSessionProperty(fServerSessionID, HttpServerQosProperty,
        value, valuelen));
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty, value, valuelen));
end;

procedure THttpApiServer.SetMaxBandwidth(aValue: cardinal);
var
  limit: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  limit.Flags := 1;
  if aValue = 0 then
    limit.MaxBandwidth := HTTP_LIMIT_INFINITE
  else if aValue < HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
    limit.MaxBandwidth := HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE
  else
    limit.MaxBandwidth := aValue;
  SetProperty(@limit, SizeOf(limit),
    HttpQosSettingTypeBandwidth, {alsoSession=}true);
end;

function THttpApiServer.GetMaxBandwidth: cardinal;
var
  limit: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  result := 0;
  if GetProperty(@limit, SizeOf(limit), HttpQosSettingTypeBandwidth) then
    result := limit.MaxBandwidth;
  if result = HTTP_LIMIT_INFINITE then
    result := 0; // cleaner for end-user
end;

function THttpApiServer.GetMaxConnections: cardinal;
var
  limit: HTTP_CONNECTION_LIMIT_INFO;
begin
  result := 0;
  if GetProperty(@limit, SizeOf(limit), HttpQosSettingTypeConnectionLimit) then
    result := limit.MaxConnections;
  if result = HTTP_LIMIT_INFINITE then
    result := 0; // cleaner for end-user
end;

procedure THttpApiServer.SetMaxConnections(aValue: cardinal);
var
  limit: HTTP_CONNECTION_LIMIT_INFO;
begin
  limit.Flags := 1;
  if aValue = 0 then
    limit.MaxConnections := HTTP_LIMIT_INFINITE
  else
    limit.MaxConnections := aValue;
  SetProperty(@limit, SizeOf(limit), HttpQosSettingTypeConnectionLimit);
end;

procedure THttpApiServer.LogStart(const aLogFolder: TFileName;
  aType: THttpApiLoggingType; const aSoftwareName: TFileName;
  aRolloverType: THttpApiLoggingRollOver; aRolloverSize: cardinal;
  aLogFields: THttpApiLogFields; aFlags: THttpApiLoggingFlags);
var
  log: HTTP_LOGGING_INFO;
  folder, software: SynUnicode;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  // disable any previous logging
  fLogging := false;
  // setup log parameters
  FillcharFast(log, SizeOf(log), 0);
  log.Flags := 1;
  log.LoggingFlags := byte(aFlags);
  if aLogFolder = '' then
    raise EHttpApiServer.CreateFmt('LogStart(aLogFolder="")', []);
  if length(aLogFolder) > 212 then
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532
    raise EHttpApiServer.CreateFmt('LogStart(%s): too long path', [aLogFolder]);
  folder   := SynUnicode(aLogFolder);
  software := SynUnicode(aSoftwareName);
  log.SoftwareNameLength  := length(software) * 2; // in bytes
  log.SoftwareName        := pointer(software);
  log.DirectoryNameLength := length(folder) * 2;
  log.DirectoryName       := pointer(folder);      // in bytes
  log.Format := HTTP_LOGGING_TYPE(aType);
  if aType = hltNCSA then
    aLogFields := [hlfDate .. hlfSubStatus];
  log.Fields := integer(aLogFields);
  log.RolloverType := HTTP_LOGGING_ROLLOVER_TYPE(aRolloverType);
  if aRolloverType = hlrSize then
    log.RolloverSize := aRolloverSize;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerLoggingProperty, @log, SizeOf(log)));
  // on success, enable the use of these HTTP_LOGGING_INFO settings
  fLogging := true;
end;

procedure THttpApiServer.LogStop;
begin
  if self <> nil then
    fLogging := false;
end;

procedure THttpApiServer.SetAuthenticationSchemes(
  schemes: THttpApiRequestAuthentications; const DomainName, Realm: SynUnicode);
var
  auth: HTTP_SERVER_AUTHENTICATION_INFO;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  fAuthenticationSchemes := schemes;
  FillcharFast(auth, SizeOf(auth), 0);
  auth.Flags := 1;
  auth.AuthSchemes := byte(schemes);
  auth.ReceiveMutualAuth := true;
  if haBasic in schemes then
  begin
    auth.BasicParams.RealmLength := Length(Realm) * 2; // in bytes
    auth.BasicParams.Realm       := pointer(Realm);
  end;
  if haDigest in schemes then
  begin
    auth.DigestParams.DomainNameLength := Length(DomainName) * 2; // in bytes
    auth.DigestParams.DomainName       := pointer(DomainName);
    auth.DigestParams.RealmLength      := Length(Realm) * 2;      // in bytes
    auth.DigestParams.Realm            := pointer(Realm);
  end;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerAuthenticationProperty, @auth, SizeOf(auth)));
end;

procedure THttpApiServer.SetTimeOutLimits(aEntityBody, aDrainEntityBody,
  aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
var
  timeout: HTTP_TIMEOUT_LIMIT_INFO;
begin
  if self = nil then
    exit;
  EHttpApiServer.RaiseCheckApi2(hSetUrlGroupProperty);
  FillcharFast(timeout, SizeOf(timeout), 0);
  timeout.Flags           := 1;
  timeout.EntityBody      := aEntityBody;
  timeout.DrainEntityBody := aDrainEntityBody;
  timeout.RequestQueue    := aRequestQueue;
  timeout.IdleConnection  := aIdleConnection;
  timeout.HeaderWait      := aHeaderWait;
  timeout.MinSendRate     := aMinSendRate;
  Ensure(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID,
      HttpServerTimeoutsProperty, @timeout, SizeOf(timeout)));
end;

procedure THttpApiServer.DoAfterResponse(Ctxt: THttpServerRequest;
  const Referer: RawUtf8; StatusCode: cardinal; Elapsed, Received, Sent: QWord);
var
  ctx: TOnHttpServerAfterResponseContext;
begin
  if Assigned(fOnAfterResponse) then
  try
    ctx.Connection      := Ctxt.ConnectionID;
    ctx.User            := pointer(Ctxt.AuthenticatedUser);
    ctx.Method          := pointer(Ctxt.Method);
    ctx.Host            := pointer(Ctxt.Host);
    ctx.Url             := pointer(Ctxt.Url);
    ctx.Referer         := pointer(Referer);
    ctx.UserAgent       := pointer(Ctxt.UserAgent);
    ctx.RemoteIP        := pointer(Ctxt.RemoteIP);
    ctx.Flags           := Ctxt.ConnectionFlags;
    ctx.State           := hrsResponseDone;
    ctx.StatusCode      := StatusCode;
    ctx.ElapsedMicroSec := Elapsed;
    ctx.Received        := Received;
    ctx.Sent            := Sent;
    ctx.Tix64           := 0;
    fOnAfterResponse(ctx); // e.g. THttpLogger or THttpAnalyzer
  except
    on E: Exception do // paranoid
      fOnAfterResponse := nil; // won't try again
  end;
end;


{ ****************** THttpApiWebSocketServer Over Windows http.sys Module }

{ THttpApiWebSocketServerProtocol }

constructor THttpApiWebSocketServerProtocol.Create(const aName: RawUtf8;
  aManualFragmentManagement: boolean; aServer: THttpApiWebSocketServer;
  const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
  const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
  const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
  const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
  const aOnFragment: TOnHttpApiWebSocketServerMessageEvent);
begin
  if aManualFragmentManagement and
     (not Assigned(aOnFragment)) then
    raise EWebSocketApi.CreateFmt(
      'Error register WebSocket protocol. Protocol %s does not use buffer, ' +
      'but OnFragment handler is not assigned', [aName]);
  fSafe.Init;
  fConnections.Init(SizeOf(THttpApiWebSocketConnection), OnConnectionFree);
  fName := aName;
  fManualFragmentManagement := aManualFragmentManagement;
  fServer := aServer;
  fOnAccept := aOnAccept;
  fOnMessage := aOnMessage;
  fOnConnect := aOnConnect;
  fOnDisconnect := aOnDisconnect;
  fOnFragment := aOnFragment;
end;

procedure THttpApiWebSocketServerProtocol.OnConnectionFree(
  one: PLockedListOne);
begin
  Finalize(PHttpApiWebSocketConnection(one)^); // I/O should have been finalized
end;

destructor THttpApiWebSocketServerProtocol.Destroy;
begin
  fConnections.Done;
  fSafe.Done;
  inherited;
end;

procedure THttpApiWebSocketServerProtocol.PostWake(
  conn: PHttpApiWebSocketConnection);
begin
  if conn.fWakePending then
    exit;
  conn.fWakePending := true;
  if IocpPostQueuedStatus(fServer.fThreadPoolServer.FRequestQueue,
       0, nil, @conn.fWakeIo.Overlapped) then
    exit;
  conn.fWakePending := false;
  raise EWebSocketApi.Create('Cannot post WebSocket IOCP wakeup');
end;

const
  WSAPI_CLOSED = [
    wsClosedByClient, wsClosedByServer, wsClosedByGuard, wsClosedByShutdown];
  WSAPI_ACTIVE = [
    wsOpen, wsClosing];

procedure THttpApiWebSocketServerProtocol.BeginClose(
  conn: PHttpApiWebSocketConnection; Reason: TWebSocketState);
begin
  if not (conn.fState in WSAPI_CLOSED) then
  begin
    // no CLOSE frame was received: use an explicit reason,
    // not any partially accumulated application message
    if Reason in [wsClosedByClient, wsClosedByServer] then
    begin
      conn.fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
      conn.fBuffer := '';
    end;
    conn.fState := Reason;
    if conn.fOpaqueHTTPRequestId <> 0 then
      Http.CancelHttpRequest(fServer.fReqQueue, conn.fOpaqueHTTPRequestId);
  end;
  PostWake(conn);
end;

function THttpApiWebSocketServerProtocol.Broadcast(
  aBufferType: WEB_SOCKET_BUFFER_TYPE;
  aBuffer: pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := true;
  fSafe.Lock;
  try
    conn := fConnections.Head;
    while conn <> nil do
    begin
      if conn.fState = wsOpen then
        try
          conn.Send(aBufferType, aBuffer, aBufferSize);
          if conn.fState <> wsOpen then
            result := false;
        except
          on E: Exception do
          begin
            result := false;
            BeginClose(conn, wsClosedByServer);
          end;
        end;
      conn := conn.fList.next;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function THttpApiWebSocketServerProtocol.Send(aID: THttpApiWebSocketConnectionID;
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  fSafe.Lock;
  try
    conn := fConnections.Find(aID);
    if (conn = nil) or
       (conn.fState <> wsOpen) then
      exit;
    conn.Send(aBufferType, aBuffer, aBufferSize);
    result := conn.fState = wsOpen;
  finally
    fSafe.UnLock;
  end;
end;

function THttpApiWebSocketServerProtocol.Close(aID: THttpApiWebSocketConnectionID;
  aStatus: WEB_SOCKET_CLOSE_STATUS;
  aBuffer: pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  fSafe.Lock;
  try
    conn := fConnections.Find(aID);
    if (conn = nil) or
       (conn.fState <> wsOpen) then
      exit;
    conn.Close(aStatus, aBuffer, aBufferSize);
    result := conn.fState = wsClosing;
  finally
    fSafe.UnLock;
  end;
end;

procedure THttpApiWebSocketServerProtocol.DoShutdown;
var
  conn: PHttpApiWebSocketConnection;
begin
  fSafe.Lock;
  try
    conn := fConnections.Head;
    while conn <> nil do
    begin
      if not (conn.fState in WSAPI_CLOSED) then
        conn.DoClose(wsClosedByShutdown, 'Server shutdown');
      conn := conn.fList.next;
    end;
  finally
    fSafe.UnLock;
  end;
end;


 { THttpApiWebSocketConnection }

function THttpApiWebSocketConnection.TryAcceptConnection(
  aProtocol: THttpApiWebSocketServerProtocol;
  Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
var
  reqhead: WEB_SOCKET_HTTP_HEADER_ARR;
  srvhead: PWEB_SOCKET_HTTP_HEADER;
  srvheadcount: ULONG;
  ctx: THttpApiServerRequest absolute Ctxt;
begin
  fState := wsConnecting;
  fBuffer := '';
  fWSHandle := nil;
  fLastActionContext := nil;
  FillcharFast(fReadIo, SizeOf(fReadIo), 0);
  fReadIo.Owner := @self;
  fReadIo.Kind := wiReceive;
  FillcharFast(fWakeIo, SizeOf(fWakeIo), 0);
  fWakeIo.Owner := @self;
  fWakeIo.Kind := wiWake;
  fProtocol := aProtocol;
  fOpaqueHTTPRequestId := ctx.req^.RequestId;
  if (fProtocol = nil) or
     (Assigned(fProtocol.OnAccept) and
      not fProtocol.OnAccept(ctx, Self)) then
  begin
    result := false;
    exit;
  end;
  EWebSocketApi.RaiseOnError(hCreateServerHandle,
    WebSocketApi.CreateServerHandle(nil, 0, fWSHandle));
  reqhead := HttpSys2ToWebSocketHeaders(ctx.req^.headers);
  if aNeedHeader then
    result := WebSocketApi.BeginServerHandshake(fWSHandle,
      pointer(fProtocol.name), nil, 0, @reqhead[0], Length(reqhead), srvhead,
      srvheadcount) = S_OK
  else
    result := WebSocketApi.BeginServerHandshake(fWSHandle, nil, nil, 0,
      pointer(reqhead), Length(reqhead),
      srvhead, srvheadcount) = S_OK;
  if result then
  try
    Ctxt.OutCustomHeaders := WebSocketHeadersToText(srvhead, srvheadcount);
  finally
    result := WebSocketApi.EndServerHandshake(fWSHandle) = S_OK;
  end;
  if result then
    fLastActiveTix64 := 0
  else if fWSHandle <> nil then
  begin
    WebSocketApi.AbortHandle(fWSHandle);
    WebSocketApi.DeleteHandle(fWSHandle);
    fWSHandle := nil;
  end;
end;

procedure THttpApiWebSocketConnection.DoOnMessage(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferSize: ULONG);
begin
  if fProtocol = nil then
    exit;
  if (aBufferType = WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE) or
     (aBufferType = WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) then
  begin
    // Fragment
    if not fProtocol.ManualFragmentManagement then
      Append(fBuffer, aBuffer, aBufferSize);
    if Assigned(fProtocol.OnFragment) then
      fProtocol.OnFragment(self, aBufferType, aBuffer, aBufferSize);
  end
  else
  try
    // last Fragment
    if Assigned(fProtocol.OnMessage) then
      if fProtocol.ManualFragmentManagement then
        fProtocol.OnMessage(self, aBufferType, aBuffer, aBufferSize)
      else
      begin
        Append(fBuffer, aBuffer, aBufferSize);
        fProtocol.OnMessage(self, aBufferType, pointer(fBuffer), Length(fBuffer));
      end;
  finally
    fBuffer := '';
  end;
end;

procedure THttpApiWebSocketConnection.DoOnConnect;
begin
  if (fProtocol <> nil) and
     Assigned(fProtocol.OnConnect) then
    fProtocol.OnConnect(self);
end;

procedure THttpApiWebSocketConnection.DoOnDisconnect;
begin
  if (fProtocol <> nil) and
     Assigned(fProtocol.OnDisconnect) then
    fProtocol.OnDisconnect(self, fCloseStatus, pointer(fBuffer), length(fBuffer));
end;

function THttpApiWebSocketConnection.ReadData(const WebsocketBufferData): integer;
var
  err: HRESULT;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  result := 0;
  if fWSHandle = nil then
    exit;
  FillcharFast(fReadIo.Overlapped, SizeOf(TOverlapped), 0);
  fReadPending := true;
  err := Http.ReceiveRequestEntityBody(fProtocol.fServer.fReqQueue,
    fOpaqueHTTPRequestId, 0, buf.pbBuffer, buf.ulBufferLength, nil,
    @fReadIo.Overlapped);
  case err of
    NO_ERROR,
    ERROR_IO_PENDING:
      result := 0;
  else
    begin
      fReadPending := false;
      result := -1;
    end;
  end;
end;

function THttpApiWebSocketConnection.WriteData(
  const WebsocketBufferData; out Written: cardinal): boolean;
var
  inmem: HTTP_DATA_CHUNK_INMEMORY;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  result := false;
  Written := 0;
  if fWSHandle = nil then
    exit;
  inmem.DataChunkType := hctFromMemory;
  inmem.pBuffer := buf.pbBuffer;
  inmem.BufferLength := buf.ulBufferLength;
  if Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue, fOpaqueHTTPRequestId,
       HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA or HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
       1, @inmem, Written, nil, nil, nil) = NO_ERROR then
    result := Written = buf.ulBufferLength
  else
    Written := 0;
end;

procedure THttpApiWebSocketConnection.CheckIsActive(Tix64: Int64);
var
  elapsed, delay: Int64;
begin
  if (@self = nil) or
     (not (fState in WSAPI_ACTIVE)) or
     (fLastActiveTix64 <= 0) or
     (fProtocol.fServer.fPingTimeout <= 0) then
    exit;
  elapsed := Tix64 - fLastActiveTix64;
  delay := fProtocol.fServer.PingTimeout * 1000;
  if elapsed > 2 * delay then
    DoClose(wsClosedByGuard, 'Closed after ping timeout')
  else if (elapsed >= delay) and
          (fState = wsOpen) and
          (fLastPingTix64 <= fLastActiveTix64) then
  try
    fLastPingTix64 := Tix64;
    Ping;
  except
    // a failed ping should close this connection, not terminate the thread
    DoClose(wsClosedByGuard, 'Ping failed');
  end;
end;

procedure THttpApiWebSocketConnection.Disconnect;
var
  chunk: HTTP_DATA_CHUNK_INMEMORY;
  writ: cardinal;
begin
  WebSocketApi.AbortHandle(fWSHandle);
  WebSocketApi.DeleteHandle(fWSHandle);
  fWSHandle := nil;
  chunk.DataChunkType := hctFromMemory;
  chunk.pBuffer := nil;
  chunk.BufferLength := 0;
  Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue, fOpaqueHTTPRequestId,
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT, 1, @chunk, writ, nil, nil, nil);
end;

procedure THttpApiWebSocketConnection.BeforeRead;
begin
  // if reading is in progress then try read messages else try receive new messages
  if fState in WSAPI_ACTIVE then
  begin
    if Assigned(fLastActionContext) then
    begin
      WebSocketApi.CompleteAction(fWSHandle,
        fLastActionContext, fReadIo.Overlapped.InternalHigh);
      fLastActionContext := nil;
    end
    else
      EWebSocketApi.RaiseOnError(hReceive,
        WebSocketApi.Receive(fWSHandle, nil, nil));
  end
  else
    raise EWebSocketApi.CreateFmt(
      'THttpApiWebSocketConnection.BeforeRead state is not wsOpen (%s)',
      [GetEnumName(TypeInfo(TWebSocketState), ord(fState))^]);
end;

function THttpApiWebSocketConnection.ProcessActions(
  ActionQueue: WEB_SOCKET_ACTION_QUEUE): boolean;
var
  buf: array[0 .. 1] of WEB_SOCKET_BUFFER_DATA;
  bufcount: ULONG;
  buftyp: WEB_SOCKET_BUFFER_TYPE;
  action: WEB_SOCKET_ACTION;
  appctxt: pointer;
  actctxt: pointer;
  i: PtrInt;
  written, total: cardinal;
begin
  result := true;
  repeat
    bufcount := Length(buf);
    EWebSocketApi.RaiseOnError(hGetAction,
      WebSocketApi.GetAction(fWSHandle, ActionQueue, @buf[0], bufcount,
        action, buftyp, appctxt, actctxt));
    case action of
      WEB_SOCKET_NO_ACTION:
        ;
      WEB_SOCKET_SEND_TO_NETWORK_ACTION:
        begin
          result := false;
          total := 0;
          for i := 0 to bufcount - 1 do
          begin
            if not WriteData(buf[i], written) then
            begin
              inc(total, written); // partial count would cancel this action
              WebSocketApi.CompleteAction(fWSHandle, actctxt, total);
              fProtocol.BeginClose(@self, wsClosedByServer);
              exit;
            end;
            inc(total, written);
          end;
          WebSocketApi.CompleteAction(fWSHandle, actctxt, total);
          result := true;
          continue; // fetch next action, without completing actctxt twice
        end;
      WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION:
         if buftyp <> WEB_SOCKET_PING_PONG_BUFFER_TYPE then
          fLastActiveTix64 := fProtocol.fServer.fIdleTix64; // from OnIdleProcess
      WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION:
        begin
          result := false;
          // One OVERLAPPED can track only one outstanding receive
          if (bufcount <> 1) or
             fReadPending then
          begin
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            fProtocol.BeginClose(@self, wsClosedByServer);
            exit;
          end;
          fLastActionContext := actctxt;
          if ReadData(buf[0]) < 0 then
          begin
            fLastActionContext := nil;
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            DoClose(wsClosedByClient);
          end;
          exit;
        end;
      WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION:
        begin
          fLastActiveTix64 := fProtocol.fServer.fIdleTix64; // from OnIdleProcess
          if buftyp = WEB_SOCKET_CLOSE_BUFFER_TYPE then
          begin
            if fState = wsOpen then
              fState := wsClosedByClient
            else
              fState := wsClosedByServer;
            FastSetRawByteString(fBuffer, buf[0].pbBuffer, buf[0].ulBufferLength);
            fCloseStatus := buf[0].Reserved1;
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            if fState = wsClosedByClient then
              Close(fCloseStatus, pointer(fBuffer), length(fBuffer));
            result := false;
            exit;
          end
          else if buftyp = WEB_SOCKET_PING_PONG_BUFFER_TYPE then
          begin
            // todo: may be answer to client's ping
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            exit;
          end
          else if buftyp = WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE then
          begin
            // todo: may be handle this situation
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            exit;
          end
          else
          begin
            DoOnMessage(buftyp, buf[0].pbBuffer, buf[0].ulBufferLength);
            WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            exit;
          end;
        end
    else
      raise EWebSocketApi.CreateFmt('Invalid WebSocket action %d', [byte(action)]);
    end;
    if actctxt <> nil then
      WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
  until {%H-}action = WEB_SOCKET_NO_ACTION;
end;

procedure THttpApiWebSocketConnection.InternalSend(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
begin
  EWebSocketApi.RaiseOnError(hSend,
    WebSocketApi.Send(fWSHandle, aBufferType, WebsocketBufferData, nil));
  ProcessActions(WEB_SOCKET_SEND_ACTION_QUEUE);
end;

procedure THttpApiWebSocketConnection.Send(aBufferType: WEB_SOCKET_BUFFER_TYPE;
  aBuffer: pointer; aBufferSize: ULONG);
var
  buf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState <> wsOpen then
    exit;
  buf.pbBuffer := aBuffer;
  buf.ulBufferLength := aBufferSize;
  InternalSend(aBufferType, @buf);
end;

procedure THttpApiWebSocketConnection.Close(aStatus: WEB_SOCKET_CLOSE_STATUS;
  aBuffer: pointer; aBufferSize: ULONG);
var
  buf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState = wsOpen then
    fState := wsClosing;
  buf.pbBuffer := aBuffer;
  buf.ulBufferLength := aBufferSize;
  buf.Reserved1 := aStatus;
  InternalSend(WEB_SOCKET_CLOSE_BUFFER_TYPE, @buf);
end;

procedure THttpApiWebSocketConnection.Ping;
begin
  InternalSend(WEB_SOCKET_PING_PONG_BUFFER_TYPE, nil);
end;

procedure THttpApiWebSocketConnection.DoClose(Reason: TWebSocketState;
  const Context: RawUtf8);
begin
  fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
  fBuffer := Context;
  fProtocol.BeginClose(@self, Reason);
end;


{ THttpApiWebSocketServer }

constructor THttpApiWebSocketServer.Create(
  aSocketThreadsCount, aPingTimeout: integer; const QueueName: SynUnicode;
  const aOnWSThreadStart, aOnWSThreadTerminate: TOnNotifyThread;
  ProcessOptions: THttpServerOptions);
begin
  WebSocketApiInitialize;
  if not (WebSocketApi.WebSocketEnabled) then
    raise EWebSocketApi.Create('WebSocket API not supported');
  inherited Create(QueueName, nil, nil, '', ProcessOptions);
  fPingTimeout := aPingTimeout;
  fOnWSThreadStart := aOnWSThreadStart;
  fOnWSThreadTerminate := aOnWSThreadTerminate;
  fThreadPoolServer := TSynThreadPoolHttpApiWebSocketServer.Create(Self,
    aSocketThreadsCount);
end;

function THttpApiWebSocketServer.GetRegisteredProtocols: THttpApiWebSocketServerProtocolDynArray;
begin
  fOwnedProtocolsSafe.Lock; // thread-safe copy
  result := copy(fOwnedProtocols);
  fOwnedProtocolsSafe.UnLock;
end;

procedure THttpApiWebSocketServer.DestroyMainThread;
var
  i: PtrInt;
  running, tix, endtix, lasttix: cardinal;
begin
  fWsStopping := true;
  for i := 0 to High(fOwnedProtocols) do
    fOwnedProtocols[i].DoShutdown;
  endtix := GetTickSec + 5; // never wait forever
  lasttix := 0;
  repeat
    running := 0;
    for i := 0 to High(fOwnedProtocols) do
      inc(running, fOwnedProtocols[i].fConnections.Count);
    if running = 0 then
      break;
    tix := GetTickSec;
    if tix > endtix then
      break;
    if tix <> lasttix then // log every second
      fLogClass.Add.Log(sllDebug, 'DestroyMainThread running=%', [running], self);
    lasttix := tix;
    SleepHiRes(10);
  until false;
  FreeAndNilSafe(fThreadPoolServer);
  ObjArrayClear(fOwnedProtocols);
  inherited DestroyMainThread;
end;

function THttpApiWebSocketServer.UpgradeToWebSocket(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  proto: THttpApiWebSocketServerProtocol;
  protos: THttpApiWebSocketServerProtocolDynArray;
  i, j: PtrInt;
  hdr: PHTTP_UNKNOWN_HEADER;
  ch, chB: PUtf8Char;
  protoname: RawUtf8;
  specified: boolean;
  ctx: THttpApiServerRequest absolute Ctxt;
begin
  result := HTTP_NOTFOUND;
  // search for explicit name specified in 'SEC-WEBSOCKET-PROTOCOL:' header
  proto := nil;
  protos := GetRegisteredProtocols; // local copy
  specified := false;
  hdr := ctx.req^.headers.pUnknownHeaders;
  for j := 1 to ctx.req^.headers.UnknownHeaderCount do
  begin
    if (hdr^.NameLength = Length(sProtocolHeader)) and
       IdemPChar(hdr^.pName, pointer(sProtocolHeader)) then
    begin
      specified := true;
      ch := hdr^.pRawValue;
      while (proto = nil) and
            ((ch - hdr^.pRawValue) < hdr^.RawValueLength) do
      begin
        while ((ch - hdr^.pRawValue) < hdr^.RawValueLength) and
              (ch^ in [',', ' ']) do
          inc(ch);
        chB := ch;
        while ((ch - hdr^.pRawValue) < hdr^.RawValueLength) and
              not (ch^ in [',']) do
          inc(ch);
        FastSetString(protoname, chB, ch);
        TrimSelf(protoname);
        for i := 0 to Length(protos) - 1 do
          if protos[i].name = protoname then
          begin
            proto := protos[i];
            break;
          end;
      end;
      if proto <> nil then
        break;
    end;
    inc(hdr);
  end;
  if not specified and
     (proto = nil) and
     (Length(protos) = 1) then
    proto := protos[0]; // fallback to our single known protocol
  if proto = nil then
    exit;
  // add the connection for this protocol
  proto.fSafe.Lock;
  try
    if fWsStopping then
      exit;
    ctx.ws := proto.fConnections.New; // PHttpApiWebSocketConnection
    try
      ctx.ws.fUpgradePending := true;
      if ctx.ws.TryAcceptConnection(proto, Ctxt, specified) then
        result := HTTP_SWITCHINGPROTOCOLS
      else
      begin
        proto.fConnections.Free(ctx.ws);
        ctx.ws := nil;
        result := HTTP_NOTALLOWED;
      end;
    except
      // the failure path must release any partially created
      // websocket.dll handle before recycling the record
      if ctx.ws <> nil then
      begin
        if ctx.ws.fWSHandle <> nil then
        begin
          WebSocketApi.AbortHandle(ctx.ws.fWSHandle);
          WebSocketApi.DeleteHandle(ctx.ws.fWSHandle);
          ctx.ws.fWSHandle := nil;
        end;
        proto.fConnections.Free(ctx.ws);
        ctx.ws := nil;
      end;
      raise;
    end;
  finally
    proto.fSafe.UnLock;
  end;
end;

procedure THttpApiWebSocketServer.FinishUpgrade(
  conn: PHttpApiWebSocketConnection; Success: boolean);
var
  proto: THttpApiWebSocketServerProtocol;
begin
  if conn = nil then
    exit;
  proto := conn.fProtocol;
  proto.fSafe.Lock;
  try
    if not conn.fUpgradePending then
      exit;
    conn.fUpgradePending := false;
    if not Success or fWsStopping then
      proto.BeginClose(conn, wsClosedByServer)
    else
      proto.PostWake(conn);
  finally
    proto.fSafe.UnLock;
  end;
end;

procedure THttpApiWebSocketServer.DoAfterResponse(
  Ctxt: THttpServerRequest; const Referer: RawUtf8;
  StatusCode: cardinal; Elapsed, Received, Sent: QWord);
var
  ctx: THttpApiServerRequest absolute Ctxt;
begin
  if ctx.ws <> nil then
  begin
    FinishUpgrade(ctx.ws, StatusCode = HTTP_SWITCHINGPROTOCOLS);
    ctx.ws := nil;
  end;
  inherited DoAfterResponse(
    Ctxt, Referer, StatusCode, Elapsed, Received, Sent);
end;

function THttpApiWebSocketServer.GetSendResponseFlags(Ctxt: THttpServerRequest): integer;
var
  ctx: THttpApiServerRequest absolute Ctxt;
begin
  if (ctx.RespStatus = HTTP_SWITCHINGPROTOCOLS) and
     (ctx.ws <> nil) then
    result := HTTP_SEND_RESPONSE_FLAG_OPAQUE or
              HTTP_SEND_RESPONSE_FLAG_MORE_DATA or
              HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA
  else
    result := inherited GetSendResponseFlags(Ctxt);
end;

function THttpApiWebSocketServer.AddUrlWebSocket(const aRoot, aPort: RawUtf8;
  Https: boolean; const aDomainName: RawUtf8; aRegisterUri: boolean): integer;
begin
  result := AddUrl(
    aRoot, aPort, Https, aDomainName, aRegisterUri, WEB_SOCKET_URL_CONTEXT);
end;

procedure THttpApiWebSocketServer.RegisterProtocol(const aName: RawUtf8;
  aManualFragmentManagement: boolean;
  const aOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
  const aOnMessage: TOnHttpApiWebSocketServerMessageEvent;
  const aOnConnect: TOnHttpApiWebSocketServerConnectEvent;
  const aOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
  const aOnFragment: TOnHttpApiWebSocketServerMessageEvent);
var
  proto: THttpApiWebSocketServerProtocol;
begin
  if self = nil then
    exit;
  proto := THttpApiWebSocketServerProtocol.Create(aName,
    aManualFragmentManagement, self, aOnAccept, aOnMessage, aOnConnect,
    aOnDisconnect, aOnFragment);
  ObjArrayAdd(fOwnedProtocols, proto, fOwnedProtocolsSafe);
end;

function THttpApiWebSocketServer.Request(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  ctx: THttpApiServerRequest absolute Ctxt;
begin
  if ctx.req.UrlContext = WEB_SOCKET_URL_CONTEXT then
    result := UpgradeToWebSocket(Ctxt)
  else
    result := inherited Request(Ctxt);
end;

procedure THttpApiWebSocketServer.SendServiceMessage;
begin
  IocpPostQueuedStatus(fThreadPoolServer.FRequestQueue, 0, nil, @fServiceOverlaped);
end;

procedure THttpApiWebSocketServer.DoIdle(tix64: Int64; sec32: cardinal);
var
  i: PtrInt;
  conn: PHttpApiWebSocketConnection;
  proto: THttpApiWebSocketServerProtocol;
  protos: THttpApiWebSocketServerProtocolDynArray;
begin
  // logger + telemetry + OnIdle()
  inherited DoIdle(tix64, sec32);
  // PING support
  if (fPingTimeout > 0) and
     not fWsStopping and
     (fThreadPoolServer <> nil) then
  begin
    protos := GetRegisteredProtocols;
    for i := 0 to High(protos) do
    begin
      proto := protos[i];
      proto.fSafe.Lock;
      try
        conn := proto.fConnections.Head;
        while conn <> nil do
        begin
          conn.CheckIsActive(tix64);
          conn := conn.fList.next;
        end;
      finally
        proto.fSafe.UnLock;
      end;
    end;
  end;
end;


{ TSynThreadPoolHttpApiWebSocketServer }

function TSynThreadPoolHttpApiWebSocketServer.NeedStopOnIOError: boolean;
begin
  // If connection closed by guard than ERROR_HANDLE_EOF or ERROR_OPERATION_ABORTED
  // can be returned - Other connections must work normally
  result := false;
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadStart(Sender: TThreadAbstract);
begin
  if Assigned(fServer.OnWSThreadStart) then
    fServer.OnWSThreadStart(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadTerminate(Sender: TThreadAbstract);
begin
  if Assigned(fServer.OnWSThreadTerminate) then
    fServer.OnWSThreadTerminate(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.Task(
  aCaller: TSynThreadPoolWorkThread; aContext: pointer);
var
  io: PHttpApiWsIocp absolute aContext;
  conn: PHttpApiWebSocketConnection;
  status: cardinal;
  proto: THttpApiWebSocketServerProtocol;
begin
  if aContext = @fServer.fServiceOverlaped then
  begin
    if Assigned(fServer.OnServiceMessage) then
      fServer.OnServiceMessage;
    exit;
  end;
  if aContext = nil then
    exit;
  conn := io.Owner;
  if conn = nil then
    exit;
  proto := conn.fProtocol;
  proto.fSafe.Lock; // recursive TOSLock
  try
    case io.Kind of
      wiReceive:
        begin
          conn.fReadPending := false;
          status := cardinal(conn.fReadIo.Overlapped.Internal); // is NTSTATUS
          if (conn.fState in WSAPI_ACTIVE) and
             (((status and $80000000) <> 0) or
              (conn.fReadIo.Overlapped.InternalHigh = 0)) then
          begin
            // Failed read or connection closed by HTTP.sys
            // Complete the outstanding WebSocket receive action
            // with zero bytes before initiating closure
            if conn.fLastActionContext <> nil then
            begin
              WebSocketApi.CompleteAction(conn.fWSHandle, conn.fLastActionContext, 0);
              conn.fLastActionContext := nil;
            end;
            conn.DoClose(wsClosedByClient);
          end;
        end;
      wiWake:
        conn.fWakePending := false;
    end;
    try
      if (conn.fState = wsConnecting) and
         not conn.fUpgradePending then
      begin
        conn.fState := wsOpen;
        conn.fLastActiveTix64 := fServer.fIdleTix64; // set by OnIdleProcess
        conn.fConnected := true;
        conn.DoOnConnect;
      end;
      if (conn.fState in WSAPI_ACTIVE) and
         not conn.fReadPending then
        repeat
          conn.BeforeRead;
        until not conn.ProcessActions(WEB_SOCKET_RECEIVE_ACTION_QUEUE);
    except
      on E: Exception do
        proto.BeginClose(conn, wsClosedByServer);
    end;
    // Recycle only when:
    // - the upgrade handoff is finished;
    // - no HTTP.sys receive completion is outstanding;
    // - no manually posted wakeup is outstanding.
    if (conn.fState in WSAPI_CLOSED) and
       not conn.fUpgradePending and
       not conn.fReadPending and
       not conn.fWakePending then
    begin
      if conn.fConnected then
        try
          conn.DoOnDisconnect;
        except
          // A user callback must not prevent resource cleanup.
        end;
      if conn.fWSHandle <> nil then
        conn.Disconnect;
      // eventuall remove from TLockedList
      proto.fConnections.Free(conn); // calls OnConnectionFree
    end;
  finally
    proto.fSafe.UnLock;
  end;
end;

constructor TSynThreadPoolHttpApiWebSocketServer.Create(
  Server: THttpApiWebSocketServer; NumberOfThreads: integer);
begin
  fServer := Server;
  fOnThreadStart := OnThreadStart;
  fOnThreadTerminate := OnThreadTerminate;
  inherited Create(NumberOfThreads, Server.fReqQueue);
end;

{$else}
implementation
{$endif USEHTTPSYS}


end.
