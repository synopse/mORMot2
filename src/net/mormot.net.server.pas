/// HTTP/HTTPS Server Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.server;

{
  *****************************************************************************

   HTTP Server Classes
   - Shared Server-Side HTTP Process
   - THttpServerSocket/THttpServer HTTP/1.1 Server
   - THttpApiServer HTTP/1.1 Server Over Windows http.sys Module
   - THttpApiWebSocketServer Over Windows http.sys Module

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
  mormot.core.threads,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.datetime,
  mormot.core.zip,
  mormot.net.sock,
  mormot.net.http,
  {$ifdef USEWININET}
  mormot.lib.winhttp,
  {$endif USEWININET}
  mormot.net.client,
  mormot.crypt.secure;


{ ******************** Shared Server-Side HTTP Process }

type
  /// exception raised during HTTP process
  EHttpServer = class(ESynException);

  {$M+} // to have existing RTTI for published properties
  THttpServerGeneric = class;
  {$M-}

  /// a generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  THttpServerRequest = class(THttpServerRequestAbstract)
  protected
    fServer: THttpServerGeneric;
    fErrorMessage: string;
    {$ifdef USEWININET}
    fHttpApiRequest: PHTTP_REQUEST;
    function GetFullURL: SynUnicode;
    {$endif USEWININET}
  public
    /// initialize the context, associated to a HTTP server instance
    constructor Create(aServer: THttpServerGeneric;
      aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
      aConnectionFlags: THttpServerRequestFlags;
      aConnectionOpaque: PHttpServerConnectionOpaque); virtual;
    /// could be called before Prepare() to reuse an existing instance
    procedure Recycle(aConnectionID: THttpServerConnectionID;
      aConnectionFlags: THttpServerRequestFlags);
    /// prepare one reusable HTTP State Machine for sending the response
    function SetupResponse(var Context: THttpRequestContext;
      CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;
    /// just a wrapper around fErrorMessage := FormatString()
    procedure SetErrorMessage(const Fmt: RawUtf8; const Args: array of const);
    /// the associated server instance
    // - may be a THttpServer or a THttpApiServer class
    property Server: THttpServerGeneric
      read fServer;
    /// optional error message which will be used by SetupResponse
    property ErrorMessage: string
      read fErrorMessage write fErrorMessage;
    {$ifdef USEWININET}
    /// for THttpApiServer, input parameter containing the caller full URL
    property FullURL: SynUnicode
      read GetFullURL;
    /// for THttpApiServer, points to a PHTTP_REQUEST structure
    property HttpApiRequest: PHTTP_REQUEST
      read fHttpApiRequest;
    {$endif USEWININET}
  end;

  /// available HTTP server options
  // - some THttpServerGeneric classes may have only partial support of them
  // - hsoHeadersUnfiltered will store all headers, not only relevant (i.e.
  // include raw Content-Length, Content-Type and Content-Encoding entries)
  // - hsoHeadersInterning triggers TRawUtf8Interning to reduce memory usage
  // - hsoNoStats will disable low-level statistic counters
  // - hsoNoXPoweredHeader excludes 'X-Powered-By: mORMot 2 synopse.info' header
  // - hsoCreateSuspended won't start the server thread immediately
  // - hsoLogVerbose could be used to debug a server in production
  // - hsoIncludeDateHeader will let all answers include a Date: ... HTTP header
  // - hsoEnableTls enables TLS support for THttpServer socket server, using
  // Windows SChannel API or OpenSSL - call WaitStarted() to set the certificates
  THttpServerOption = (
    hsoHeadersUnfiltered,
    hsoHeadersInterning,
    hsoNoXPoweredHeader,
    hsoNoStats,
    hsoCreateSuspended,
    hsoLogVerbose,
    hsoIncludeDateHeader,
    hsoEnableTls);

  /// how a THttpServerGeneric class is expected to process incoming requests
  THttpServerOptions = set of THttpServerOption;

  /// abstract parent class to implement a HTTP server
  // - do not use it, but rather THttpServer/THttpAsyncServer or THttpApiServer
  THttpServerGeneric = class(TNotifiedThread)
  protected
    fShutdownInProgress: boolean;
    fOptions: THttpServerOptions;
    /// optional event handlers for process interception
    fOnRequest: TOnHttpServerRequest;
    fOnBeforeBody: TOnHttpServerBeforeBody;
    fOnBeforeRequest: TOnHttpServerRequest;
    fOnAfterRequest: TOnHttpServerRequest;
    fOnAfterResponse: TOnHttpServerAfterResponse;
    fMaximumAllowedContentLength: cardinal;
    /// set by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    fCompressAcceptEncoding: RawUtf8;
    fServerName: RawUtf8;
    fCurrentConnectionID: integer; // 31-bit NextConnectionID sequence
    fCurrentRequestID: integer;
    fCallbackSendDelay: PCardinal;
    fRemoteIPHeader, fRemoteIPHeaderUpper: RawUtf8;
    fRemoteConnIDHeader, fRemoteConnIDHeaderUpper: RawUtf8;
    fOnSendFile: TOnHttpServerSendFile;
    function GetApiVersion: RawUtf8; virtual; abstract;
    procedure SetServerName(const aName: RawUtf8); virtual;
    procedure SetOnRequest(const aRequest: TOnHttpServerRequest); virtual;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); virtual;
    procedure SetOnBeforeRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterRequest(const aEvent: TOnHttpServerRequest); virtual;
    procedure SetOnAfterResponse(const aEvent: TOnHttpServerAfterResponse); virtual;
    procedure SetMaximumAllowedContentLength(aMax: cardinal); virtual;
    procedure SetRemoteIPHeader(const aHeader: RawUtf8); virtual;
    procedure SetRemoteConnIDHeader(const aHeader: RawUtf8); virtual;
    function GetHttpQueueLength: cardinal; virtual; abstract;
    procedure SetHttpQueueLength(aValue: cardinal); virtual; abstract;
    function DoBeforeRequest(Ctxt: THttpServerRequest): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    function DoAfterRequest(Ctxt: THttpServerRequest): cardinal;
      {$ifdef HASINLINE}inline;{$endif}
    procedure DoAfterResponse(Ctxt: THttpServerRequest; const Code: cardinal); virtual;
    function NextConnectionID: integer; // 31-bit internal sequence
    procedure ParseRemoteIPConnID(const Headers: RawUtf8;
      var RemoteIP: RawUtf8; var RemoteConnID: THttpServerConnectionID);
    procedure AppendHttpDate(var Dest: TRawByteStringBuffer); virtual;
  public
    /// initialize the server instance
    constructor Create(const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ProcessOptions: THttpServerOptions); reintroduce; virtual;
    /// override this function to customize your http server
    // - InURL/InMethod/InContent properties are input parameters
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - result of the function is the HTTP error code (200 if OK, e.g.),
    // - OutCustomHeader is available to handle Content-Type/Location
    // - if OutContentType is STATICFILE_CONTENT_TYPE (i.e. '!STATICFILE'),
    // then OutContent is the UTF-8 filename of a file to be sent directly
    // to the client via http.sys or NGINX's X-Accel-Redirect; the
    // OutCustomHeader should contain the eventual 'Content-type: ....' value
    // - default implementation is to call the OnRequest event (if existing),
    // and will return HTTP_NOTFOUND if OnRequest was not set
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously, but with a given Ctxt instance for each)
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    /// server can send a request back to the client, when the connection has
    // been upgraded e.g. to WebSockets
    // - InURL/InMethod/InContent properties are input parameters
    // (InContentType is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - Ctxt.ConnectionID should be set, so that the method could know
    // which connnection is to be used - returns HTTP_NOTFOUND (404) if unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    // - warning: this void implementation will raise an EHttpServer exception -
    // inherited classes should override it, e.g. as in TWebSocketServerRest
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; virtual;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlz) protocols
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024); virtual;
    /// you can call this method to prepare the HTTP server for shutting down
    procedure Shutdown;
    /// event handler called by the default implementation of the
    // virtual Request method
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    property OnRequest: TOnHttpServerRequest
      read fOnRequest write SetOnRequest;
    /// event handler called just before the body is retrieved from the client
    // - should return HTTP_SUCCESS=200 to continue the process, or an HTTP
    // error code to reject the request immediatly, and close the connection
    property OnBeforeBody: TOnHttpServerBeforeBody
      read fOnBeforeBody write SetOnBeforeBody;
    /// event handler called after HTTP body has been retrieved, before OnProcess
    // - may be used e.g. to return a HTTP_ACCEPTED (202) status to client and
    // continue a long-term job inside the OnProcess handler in the same thread;
    // or to modify incoming information before passing it to main businnes logic,
    // (header preprocessor, body encoding etc...)
    // - if the handler returns > 0 server will send a response immediately,
    // unless return code is HTTP_ACCEPTED (202), then OnRequest will be called
    // - warning: this handler must be thread-safe (can be called by several
    // threads simultaneously)
    property OnBeforeRequest: TOnHttpServerRequest
      read fOnBeforeRequest write SetOnBeforeRequest;
    /// event handler called after request is processed but before response
    // is sent back to client
    // - main purpose is to apply post-processor, not part of request logic
    // - if handler returns value > 0 it will override the OnProcess response code
    // - warning: this handler must be thread-safe (can be called by several
    // threads simultaneously)
    property OnAfterRequest: TOnHttpServerRequest
      read fOnAfterRequest write SetOnAfterRequest;
    /// event handler called after response is sent back to client
    // - main purpose is to apply post-response analysis, logging, etc.
    // - warning: this handler must be thread-safe (can be called by several
    // threads simultaneously)
    property OnAfterResponse: TOnHttpServerAfterResponse
      read fOnAfterResponse write SetOnAfterResponse;
    /// event handler called after each working Thread is just initiated
    // - called in the thread context at first place in THttpServerGeneric.Execute
    property OnHttpThreadStart: TOnNotifyThread
      read fOnThreadStart write fOnThreadStart;
    /// event handler called when a working Thread is terminating
    // - called in the corresponding thread context
    // - the TThread.OnTerminate event will be called within a Synchronize()
    // wrapper, so it won't fit our purpose
    // - to be used e.g. to call CoUnInitialize from thread in which CoInitialize
    // was made, for instance via a method defined as such:
    // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
    // ! begin // TSqlDBConnectionPropertiesThreadSafe
    // !   fMyConnectionProps.EndCurrentThread;
    // ! end;
    // - is used e.g. by TRest.EndCurrentThread for proper multi-threading
    property OnHttpThreadTerminate: TOnNotifyThread
      read fOnThreadTerminate write SetOnTerminate;
    /// reject any incoming request with a body size bigger than this value
    // - default to 0, meaning any input size is allowed
    // - returns HTTP_PAYLOADTOOLARGE = 413 error if "Content-Length" incoming
    // header overflow the supplied number of bytes
    property MaximumAllowedContentLength: cardinal
      read fMaximumAllowedContentLength write SetMaximumAllowedContentLength;
    /// custom event handler used to send a local file for STATICFILE_CONTENT_TYPE
    // - see also NginxSendFileFrom() method
    property OnSendFile: TOnHttpServerSendFile
      read fOnSendFile write fOnSendFile;
    /// defines request/response internal queue length
    // - default value if 1000, which sounds fine for most use cases
    // - for THttpApiServer, will return 0 if the system does not support HTTP
    // API 2.0 (i.e. under Windows XP or Server 2003)
    // - for THttpServer or THttpAsyncServer, will shutdown any incoming accepted
    // socket if the internal number of pending requests exceed this limit
    // - increase this value if you don't have any load-balancing in place, and
    // in case of e.g. many 503 HTTP answers or if many "QueueFull" messages
    // appear in HTTP.sys log files (normally in
    // C:\Windows\System32\LogFiles\HTTPERR\httperr*.log) - may appear with
    // thousands of concurrent clients accessing at once the same server -
    // see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa364501
    // - you can use this property with a reverse-proxy as load balancer, e.g.
    // with nginx configured as such:
    // $ location / {
    // $       proxy_pass              http://balancing_upstream;
    // $       proxy_next_upstream     error timeout invalid_header http_500 http_503;
    // $       proxy_connect_timeout   2;
    // $       proxy_set_header        Host            $host;
    // $       proxy_set_header        X-Real-IP       $remote_addr;
    // $       proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
    // $       proxy_set_header        X-Conn-ID       $connection
    // $ }
    // see https://synopse.info/forum/viewtopic.php?pid=28174#p28174
    property HttpQueueLength: cardinal
      read GetHttpQueueLength write SetHttpQueueLength;
    /// TRUE if the inherited class is able to handle callbacks
    // - only TWebSocketServer/TWebSocketAsyncServer have this ability by now
    function CanNotifyCallback: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// the value of a custom HTTP header containing the real client IP
    // - by default, the RemoteIP information will be retrieved from the socket
    // layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the true remote client
    // IP value, mostly as 'X-Real-IP' or 'X-Forwarded-For'
    property RemoteIPHeader: RawUtf8
      read fRemoteIPHeader write SetRemoteIPHeader;
    /// the value of a custom HTTP header containing the real client connection ID
    // - by default, Ctxt.ConnectionID information will be retrieved from our
    // socket layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the real remote connection,
    // for example as 'X-Conn-ID', setting in nginx config:
    //  $ proxy_set_header      X-Conn-ID       $connection
    property RemoteConnIDHeader: RawUtf8
      read fRemoteConnIDHeader write SetRemoteConnIDHeader;
    /// allow to customize this HTTP server instance
    // - some inherited classes may have only partial support of those options
    property Options: THttpServerOptions
      read fOptions write fOptions;
  published
    /// returns the API version used by the inherited implementation
    property ApiVersion: RawUtf8
      read GetApiVersion;
    /// the Server name, UTF-8 encoded, e.g. 'mORMot2 (Linux)'
    // - will be served as "Server: ..." HTTP header
    // - for THttpApiServer, when called from the main instance, will propagate
    // the change to all cloned instances, and included in any HTTP API 2.0 log
    property ServerName: RawUtf8
      read fServerName write SetServerName;
    /// the associated process name
    property ProcessName: RawUtf8
      read fProcessName write fProcessName;
  end;


const
  /// used to compute the request ConnectionFlags from the socket TLS state
  HTTPREMOTEFLAGS: array[{tls=}boolean] of THttpServerRequestFlags = (
    [],
    [hsrHttps, hsrSecured]);

/// some pre-computed CryptCertAlgoOpenSsl[caaRS256].New key for Windows
// - the associated password is 'pass'
// - as used e.g. by secTLSSelfSigned with the SChannel API on server side
function PrivKeyCertPfx: RawByteString;

/// initialize a server-side TLS structure with a self-signed algorithm
// - as used e.g. by secTLSSelfSigned with the SChannel API on server side
// - if OpenSSL is available, will generate a temporary pair of key files via
// Generate(CU_TLS_SERVER, '127.0.0.1', nil, 3650) with a random password
// - on pure SChannel, will use the PrivKeyCertPfx pre-computed constant
// - you should eventually call DeleteFile(Utf8ToString(TLS.CertificateFile))
// and DeleteFile(Utf8ToString(TLS.PrivateKeyFile)) to delete the two temp files
procedure InitNetTlsContextSelfSignedServer(var TLS: TNetTlsContext;
  Algo: TCryptAsymAlgo = caaRS256);


{ ******************** THttpServerSocket/THttpServer HTTP/1.1 Server }

type
  /// results of THttpServerSocket.GetRequest virtual method
  // - return grError if the socket was not connected any more, or grException
  // if any exception occured during the process
  // - grOversizedPayload is returned when MaximumAllowedContentLength is reached
  // - grRejected is returned when OnBeforeBody returned not 200
  // - grTimeout is returned when HeaderRetrieveAbortDelay is reached
  // - grHeaderReceived is returned for GetRequest({withbody=}false)
  // - grBodyReceived is returned for GetRequest({withbody=}true)
  // - grUpgraded indicates that this connection was upgraded e.g. as WebSockets
  THttpServerSocketGetRequestResult = (
    grError,
    grException,
    grOversizedPayload,
    grRejected,
    grTimeout,
    grHeaderReceived,
    grBodyReceived,
    grUpgraded);

  {$M+} // to have existing RTTI for published properties
  THttpServer = class;
  {$M-}

  /// Socket API based HTTP/1.1 server class used by THttpServer Threads
  THttpServerSocket = class(THttpSocket)
  protected
    fRemoteConnectionID: THttpServerConnectionID;
    fServer: THttpServer;
    fKeepAliveClient: boolean;
    fConnectionOpaque: THttpServerConnectionOpaque;
    // from TSynThreadPoolTHttpServer.Task
    procedure TaskProcess(aCaller: TSynThreadPoolWorkThread); virtual;
    function TaskProcessBody(aCaller: TSynThreadPoolWorkThread;
      aHeaderResult: THttpServerSocketGetRequestResult): boolean;
  public
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    // - once created, caller should call AcceptRequest() to accept the socket
    // - if TLS is enabled, ensure server certificates are initialized once
    constructor Create(aServer: THttpServer); reintroduce;
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if withBody=true and ContentLength<>0)
    // - returned enumeration will indicates the processing state
    function GetRequest(withBody: boolean;
      headerMaxTix: Int64): THttpServerSocketGetRequestResult; virtual;
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    property Method: RawUtf8
      read Http.CommandMethod;
    /// contains the URL ('/' e.g.) after GetRequest()
    property URL: RawUtf8
      read Http.CommandUri;
    /// true if the client is HTTP/1.1 and 'Connection: Close' is not set
    // - default HTTP/1.1 behavior is "keep alive", unless 'Connection: Close'
    // is specified, cf. RFC 2068 page 108: "HTTP/1.1 applications that do not
    // support persistent connections MUST include the "close" connection option
    // in every message"
    property KeepAliveClient: boolean
      read fKeepAliveClient write fKeepAliveClient;
    /// the recognized connection ID, after a call to GetRequest()
    // - identifies either the raw connection on the current server, or is
    // a custom header value set by a local proxy, e.g.
    // THttpServerGeneric.RemoteConnIDHeader='X-Conn-ID' for nginx
    property RemoteConnectionID: THttpServerConnectionID
      read fRemoteConnectionID;
    /// the associated HTTP Server instance - may be nil
    property Server: THttpServer
      read fServer;
  end;

  /// HTTP response Thread as used by THttpServer Socket API based class
  // - Execute procedure get the request and calculate the answer, using
  // the thread for a single client connection, until it is closed
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TSynThread)
  protected
    fConnectionID: THttpServerConnectionID;
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    fClientSock: TNetSocket;
    fClientSin: TNetAddr;
    /// main thread loop: read request from socket, send back answer
    procedure Execute; override;
  public
    /// initialize the response thread for the corresponding incoming socket
    // - this version will get the request directly from an incoming socket
    constructor Create(aSock: TNetSocket; const aSin: TNetAddr;
      aServer: THttpServer); reintroduce; overload;
    /// initialize the response thread for the corresponding incoming socket
    // - this version will handle KeepAlive, for such an incoming request
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer);
      reintroduce; overload; virtual;
    /// called by THttpServer.Destroy on existing connections
    // - set Terminate and close the socket
    procedure Shutdown; virtual;
    /// the associated socket to communicate with the client
    property ServerSock: THttpServerSocket
      read fServerSock;
    /// the associated main HTTP server instance
    property Server: THttpServer
      read fServer;
    /// the unique identifier of this connection
    property ConnectionID: THttpServerConnectionID
      read fConnectionID;
  end;

  /// metaclass of HTTP response Thread
  THttpServerRespClass = class of THttpServerResp;

  /// a simple Thread Pool, used for fast handling HTTP requests of a THttpServer
  // - will handle multi-connection with less overhead than creating a thread
  // for each incoming request
  // - will create a THttpServerResp response thread, if the incoming request is
  // identified as HTTP/1.1 keep alive, or HTTP body length is bigger than 1 MB
  TSynThreadPoolTHttpServer = class(TSynThreadPool)
  protected
    fServer: THttpServer;
    fBigBodySize: integer;
    fMaxBodyThreadCount: integer;
    {$ifndef USE_WINIOCP}
    function QueueLength: integer; override;
    {$endif USE_WINIOCP}
    // here aContext is a THttpServerSocket instance
    procedure Task(aCaller: TSynThreadPoolWorkThread;
      aContext: Pointer); override;
    procedure TaskAbort(aContext: Pointer); override;
  public
    /// initialize a thread pool with the supplied number of threads
    // - Task() overridden method processs the HTTP request set by Push()
    // - up to 256 threads can be associated to a Thread Pool
    constructor Create(Server: THttpServer;
      NumberOfThreads: integer = 32); reintroduce;
    /// when Content-Length is bigger than this value, by-pass the threadpool
    // and creates a dedicated THttpServerResp thread
    // - default is THREADPOOL_BIGBODYSIZE = 16 MB, but can set a bigger value
    // e.g. behind a buffering proxy if you trust the client not to make DOD
    property BigBodySize: integer
      read fBigBodySize write fBigBodySize;
    /// how many stand-alone THttpServerResp threads can be initialized when a
    // HTTP request comes in
    // - default is THREADPOOL_MAXWORKTHREADS = 512, but since each thread
    // consume system memory, you should not go so high
    // - above this value, the thread pool will be used
    property MaxBodyThreadCount: integer
      read fMaxBodyThreadCount write fMaxBodyThreadCount;
  end;

  /// meta-class of the THttpServerSocket process
  // - used to override THttpServerSocket.GetRequest for instance
  THttpServerSocketClass = class of THttpServerSocket;

  /// THttpServerSocketGeneric current state
  THttpServerExecuteState = (
    esNotStarted,
    esBinding,
    esRunning,
    esFinished);

  /// abstract parent class for both THttpServer and THttpAsyncServer
  THttpServerSocketGeneric = class(THttpServerGeneric)
  protected
    fServerKeepAliveTimeOut: cardinal;
    fServerKeepAliveTimeOutSec: cardinal;
    fHeaderRetrieveAbortDelay: cardinal;
    fCompressGz: integer;
    fSockPort: RawUtf8;
    fSock: TCrtSocket;
    fSafe: TLightLock;
    fExecuteMessage: RawUtf8;
    fNginxSendFileFrom: array of TFileName;
    fStats: array[THttpServerSocketGetRequestResult] of integer;
    function DoRequest(Ctxt: THttpServerRequest): boolean;
    procedure SetServerKeepAliveTimeOut(Value: cardinal);
    function GetStat(one: THttpServerSocketGetRequestResult): integer;
    procedure IncStat(one: THttpServerSocketGetRequestResult);
      {$ifdef HASINLINE} inline; {$endif}
    function OnNginxAllowSend(Context: THttpServerRequestAbstract;
      const LocalFileName: TFileName): boolean;
    // this overridden version will return e.g. 'Winsock 2.514'
    function GetApiVersion: RawUtf8; override;
    function GetExecuteState: THttpServerExecuteState; virtual; abstract;
    function GetRegisterCompressGzStatic: boolean;
    procedure SetRegisterCompressGzStatic(Value: boolean);
  public
    /// create a Server Thread, ready to be bound and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - can listed to local Unix Domain Sockets file in case port is prefixed
    // with 'unix:', e.g. 'unix:/run/myapp.sock' - faster and safer than TCP
    // - on Linux in case aPort is empty string will check if external fd
    // is passed by systemd and use it (so called systemd socked activation)
    // - you can specify a number of threads to be initialized to handle
    // incoming connections. Default is 32, which may be sufficient for most
    // cases, maximum is 256. If you set 0, the thread pool will be disabled
    // and one thread will be created for any incoming connection
    // - you can also tune (or disable with 0) HTTP/1.1 keep alive delay and
    // how incoming request Headers[] are pushed to the processing method
    // - this constructor won't actually do the port binding, which occurs in
    // the background thread: caller should therefore call WaitStarted after
    // THttpServer.Create()
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
      ServerThreadPoolCount: integer = 32; KeepAliveTimeOut: integer = 30000;
      ProcessOptions: THttpServerOptions = []); reintroduce; virtual;
    /// defines the WebSockets protocols to be used for this Server
    // - this default implementation will raise an exception
    // - returns the associated PWebSocketProcessSettings reference on success
    function WebSocketsEnable(
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUtf8;
      aWebSocketsAjax: boolean = false;
      aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions =
        [pboSynLzCompress]): pointer; virtual;
    /// ensure the HTTP server thread is actually bound to the specified port
    // - TCrtSocket.Bind() occurs in the background in the Execute method: you
    // should call and check this method result just after THttpServer.Create
    // - initial THttpServer design was to call Bind() within Create, which
    // works fine on Delphi + Windows, but fails with a EThreadError on FPC/Linux
    // - raise a EHttpServer if binding failed within the specified period (if
    // port is free, it would be almost immediate)
    // - calling this method is optional, but if the background thread didn't
    // actually bind the port, the server will be stopped and unresponsive with
    // no explicit error message, until it is terminated
    // - for hsoEnableTls support, you should either specify the private key
    // and certificate here, or set TLS.PrivateKeyFile/CertificateFile fields -
    // the benefit of this method parameters is that the certificates are
    // loaded and checked now by calling InitializeTlsAfterBind, not at the
    // first client connection (which may be too late)
    procedure WaitStarted(Seconds: integer; const CertificateFile: TFileName;
      const PrivateKeyFile: TFileName = ''; const PrivateKeyPassword: RawUtf8 = '';
      const CACertificatesFile: TFileName = ''); overload;
    /// ensure the HTTP server thread is actually bound to the specified port
    // - for hsoEnableTls support, allow to specify all server-side TLS
    // events, including callbacks, as supported by OpenSSL
    procedure WaitStarted(Seconds: integer = 30; TLS: PNetTlsContext = nil);
      overload;
    /// could be called after WaitStarted(seconds,'','','') to setup TLS
    // - use Sock.TLS.CertificateFile/PrivateKeyFile/PrivatePassword
    procedure InitializeTlsAfterBind;
    /// enable NGINX X-Accel internal redirection for STATICFILE_CONTENT_TYPE
    // - will define internally a matching OnSendFile event handler
    // - generating "X-Accel-Redirect: " header, trimming any supplied left
    // case-sensitive file name prefix, e.g. with NginxSendFileFrom('/var/www'):
    // $ # Will serve /var/www/protected_files/myfile.tar.gz
    // $ # When passed URI /protected_files/myfile.tar.gz
    // $ location /protected_files {
    // $  internal;
    // $  root /var/www;
    // $ }
    // - call this method several times to register several folders
    procedure NginxSendFileFrom(const FileNameLeftTrim: TFileName);
    /// milliseconds delay to reject a connection due to too long header retrieval
    // - default is 0, i.e. not checked (typically not needed behind a reverse proxy)
    property HeaderRetrieveAbortDelay: cardinal
      read fHeaderRetrieveAbortDelay write fHeaderRetrieveAbortDelay;
    /// the low-level thread execution thread
    property ExecuteState: THttpServerExecuteState
      read GetExecuteState;
    /// access to the main server low-level Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - for THttpServer inherited class, will own its own instance, then
    // THttpServerSocket/THttpServerResp are created for every connection
    // - for THttpAsyncServer inherited class, redirect to TAsyncServer.fServer
    property Sock: TCrtSocket
      read fSock;
  published
    /// the bound TCP port, as specified to Create() constructor
    property SockPort: RawUtf8
      read fSockPort;
    /// time, in milliseconds, for the HTTP/1.1 connections to be kept alive
    // - default is 30000 ms, i.e. 30 seconds
    // - setting 0 here (or in KeepAliveTimeOut constructor parameter) will
    // disable keep-alive, and fallback to HTTP.1/0 for all incoming requests
    // (may be a good idea e.g. behind a NGINX reverse proxy)
    // - see THttpApiServer.SetTimeOutLimits(aIdleConnection) parameter
    property ServerKeepAliveTimeOut: cardinal
      read fServerKeepAliveTimeOut write fServerKeepAliveTimeOut;
    /// if we should search for local .gz cached file when serving static files
    property RegisterCompressGzStatic: boolean
      read GetRegisterCompressGzStatic write SetRegisterCompressGzStatic;
    /// how many invalid HTTP headers have been rejected
    property StatHeaderErrors: integer
      index grError read GetStat;
    /// how many invalid HTTP headers raised an exception
    property StatHeaderException: integer
      index grException read GetStat;
    /// how many HTTP requests pushed more than MaximumAllowedContentLength bytes
    property StatOversizedPayloads: integer
      index grOversizedPayload read GetStat;
    /// how many HTTP requests were rejected by the OnBeforeBody event handler
    property StatRejected: integer
      index grRejected read GetStat;
    /// how many HTTP requests were rejected after HeaderRetrieveAbortDelay timeout
    property StatHeaderTimeout: integer
      index grTimeout read GetStat;
    /// how many HTTP headers have been processed
    property StatHeaderProcessed: integer
      index grHeaderReceived read GetStat;
    /// how many HTTP bodies have been processed
    property StatBodyProcessed: integer
      index grBodyReceived read GetStat;
    /// how many HTTP connections were upgraded e.g. to WebSockets
    property StatUpgraded: integer
      index grUpgraded read GetStat;
  end;

  /// meta-class of our THttpServerSocketGeneric classes
  // - typically implemented by THttpServer, TWebSocketServer,
  // TWebSocketServerRest or THttpAsyncServer classes
  THttpServerSocketGenericClass = class of THttpServerSocketGeneric;

  /// main HTTP server Thread using the standard Sockets API (e.g. WinSock)
  // - bind to a port and listen to incoming requests
  // - assign this requests to THttpServerResp threads from a ThreadPool
  // - it implements a HTTP/1.1 compatible server, according to RFC 2068 specifications
  // - if the client is also HTTP/1.1 compatible, KeepAlive connection is handled:
  //  multiple requests will use the existing connection and thread;
  //  this is faster and uses less resources, especialy under Windows
  // - a Thread Pool is used internally to speed up HTTP/1.0 connections - a
  // typical use, under Linux, is to run this class behind a NGINX frontend,
  // configured as https reverse proxy, leaving default "proxy_http_version 1.0"
  // and "proxy_request_buffering on" options for best performance, and
  // setting KeepAliveTimeOut=0 in the THttpServer.Create constructor
  // - under windows, will trigger the firewall UAC popup at first run
  // - don't forget to use Free method when you are finished
  // - a typical HTTPS server usecase could be:
  // $ fHttpServer := THttpServer.Create('443', nil, nil, '', 32, 30000, [hsoEnableTls]);
  // $ fHttpServer.WaitStarted('cert.pem', 'privkey.pem', '');  // cert.pfx for SSPI
  // $ // now certificates will be initialized and used
  THttpServer = class(THttpServerSocketGeneric)
  protected
    fThreadPool: TSynThreadPoolTHttpServer;
    fInternalHttpServerRespList: TSynObjectListLocked;
    fHttpQueueLength: cardinal;
    fSocketClass: THttpServerSocketClass;
    fThreadRespClass: THttpServerRespClass;
    fServerConnectionCount: integer;
    fServerConnectionActive: integer;
    fServerSendBufferSize: integer;
    fExecuteState: THttpServerExecuteState;
    function GetExecuteState: THttpServerExecuteState; override;
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    /// server main loop - don't change directly
    procedure Execute; override;
    /// this method is called on every new client connection, i.e. every time
    // a THttpServerResp thread is created with a new incoming socket
    procedure OnConnect; virtual;
    /// this method is called on every client disconnection to update stats
    procedure OnDisconnect; virtual;
    /// override this function in order to low-level process the request;
    // default process is to get headers, and call public function Request
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread); virtual;
  public
    /// create a socket-based HTTP Server, ready to be bound and listening on a port
    constructor Create(const aPort: RawUtf8;
      const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
      ServerThreadPoolCount: integer = 32; KeepAliveTimeOut: integer = 30000;
      ProcessOptions: THttpServerOptions = []); override;
    /// release all memory and handlers
    destructor Destroy; override;
  published
    /// will contain the current number of connections to the server
    property ServerConnectionActive: integer
      read fServerConnectionActive write fServerConnectionActive;
    /// will contain the total number of connections to the server
    // - it's the global count since the server started
    property ServerConnectionCount: integer
      read fServerConnectionCount write fServerConnectionCount;
    /// the associated thread pool
    // - may be nil if ServerThreadPoolCount was 0 on constructor
    property ThreadPool: TSynThreadPoolTHttpServer
      read fThreadPool;
  end;


const
  // kept-alive or big HTTP requests will create a dedicated THttpServerResp
  // - each thread reserves 2 MB of memory so it may break the server
  // - keep the value to a decent number, to let resources be constrained up to 1GB
  // - is the default value to TSynThreadPoolTHttpServer.MaxBodyThreadCount
  THREADPOOL_MAXWORKTHREADS = 512;

  /// if HTTP body length is bigger than 16 MB, creates a dedicated THttpServerResp
  // - is the default value to TSynThreadPoolTHttpServer.BigBodySize
  THREADPOOL_BIGBODYSIZE = 16 * 1024 * 1024;

function ToText(res: THttpServerSocketGetRequestResult): PShortString; overload;


{$ifdef USEWININET}

{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

type
  THttpApiServer = class;

  THttpApiServers = array of THttpApiServer;

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
    /// the internal request queue
    fReqQueue: THandle;
    /// contain list of THttpApiServer cloned instances
    fClones: THttpApiServers;
    // if cloned, fOwner contains the main THttpApiServer instance
    fOwner: THttpApiServer;
    /// list of all registered URL
    fRegisteredUnicodeUrl: TSynUnicodeDynArray;
    fServerSessionID: HTTP_SERVER_SESSION_ID;
    fUrlGroupID: HTTP_URL_GROUP_ID;
    fLogData: pointer;
    fLogDataStorage: TBytes;
    fLoggingServiceName: RawUtf8;
    fAuthenticationSchemes: THttpApiRequestAuthentications;
    fReceiveBufferSize: cardinal;
    procedure SetReceiveBufferSize(Value: cardinal);
    function GetRegisteredUrl: SynUnicode;
    function GetCloned: boolean;
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    function GetMaxBandwidth: cardinal;
    procedure SetMaxBandwidth(aValue: cardinal);
    function GetMaxConnections: cardinal;
    procedure SetMaxConnections(aValue: cardinal);
    procedure SetOnTerminate(const Event: TOnNotifyThread); override;
    function GetApiVersion: RawUtf8; override;
    function GetLogging: boolean;
    procedure SetServerName(const aName: RawUtf8); override;
    procedure SetOnRequest(const aRequest: TOnHttpServerRequest); override;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); override;
    procedure SetOnBeforeRequest(const aEvent: TOnHttpServerRequest); override;
    procedure SetOnAfterRequest(const aEvent: TOnHttpServerRequest); override;
    procedure SetOnAfterResponse(const aEvent: TOnHttpServerAfterResponse); override;
    procedure SetMaximumAllowedContentLength(aMax: cardinal); override;
    procedure SetRemoteIPHeader(const aHeader: RawUtf8); override;
    procedure SetRemoteConnIDHeader(const aHeader: RawUtf8); override;
    procedure SetLoggingServiceName(const aName: RawUtf8);
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrive the content
    procedure Execute; override;
    /// retrieve flags for SendHttpResponse
   // - if response content type is not STATICFILE_CONTENT_TYPE
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; virtual;
    /// free resources (for not cloned server)
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
      const ProcessName: RawUtf8 = ''; ProcessOptions: THttpServerOptions = []);
        reintroduce;
    /// create a HTTP/1.1 processing clone from the main thread
    // - do not use directly - is called during thread pool creation
    constructor CreateClone(From: THttpApiServer); virtual;
    /// release all associated memory and handles
    destructor Destroy; override;
    /// will clone this thread into multiple other threads
    // - could speed up the process on multi-core CPU
    // - will work only if the OnProcess property was set (this is the case
    // e.g. in TRestHttpServer.Create() constructor)
    // - maximum value is 256 - higher should not be worth it
    procedure Clone(ChildThreadCount: integer);
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
      const aDomainName: RawUtf8 = '*'; OnlyDelete: boolean = false): string;
    /// will register a compression algorithm
    // - overridden method which will handle any cloned instances
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024); override;
    /// access to the internal THttpApiServer list cloned by this main instance
    // - as created by Clone() method
    property Clones: THttpApiServers
      read fClones;
  public { HTTP API 2.0 methods and properties }
    /// can be used to check if the HTTP API 2.0 is available
    function HasApi2: boolean;
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
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
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
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
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
      read GetLogging;
    /// the current HTTP API 2.0 logging Service name
    // - should be UTF-8 encoded, if LogStart(aFlags=[hlfUseUtf8Conversion])
    // - this value is dedicated to one instance, so the main instance won't
    // propagate the change to all cloned instances
    property LoggingServiceName: RawUtf8
      read fLoggingServiceName write SetLoggingServiceName;
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
      read fReceiveBufferSize write SetReceiveBufferSize;
  published
    /// TRUE if this instance is in fact a cloned instance for the thread pool
    property Cloned: boolean
      read GetCloned;
    /// return the list of registered URL on this server instance
    property RegisteredUrl: SynUnicode
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
  TSynWebSocketGuard = class;
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

  /// structure representing a single WebSocket connection
  {$ifdef USERECORDWITHMETHODS}
  THttpApiWebSocketConnection = record
  {$else}
  THttpApiWebSocketConnection = object
  {$endif USERECORDWITHMETHODS}
  private
    fOverlapped: TOverlapped;
    fState: TWebSocketState;
    fProtocol: THttpApiWebSocketServerProtocol;
    fOpaqueHTTPRequestId: HTTP_REQUEST_ID;
    fWSHandle: WEB_SOCKET_HANDLE;
    fLastActionContext: Pointer;
    fLastReceiveTickCount: Int64;
    fPrivateData: pointer;
    fBuffer: RawByteString;
    fCloseStatus: WEB_SOCKET_CLOSE_STATUS;
    fIndex: integer;
    function ProcessActions(ActionQueue: cardinal): boolean;
    function ReadData(const WebsocketBufferData): integer;
    procedure WriteData(const WebsocketBufferData);
    procedure BeforeRead;
    procedure DoOnMessage(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: Pointer; aBufferSize: ULONG);
    procedure DoOnConnect;
    procedure DoOnDisconnect;
    procedure InternalSend(aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
    procedure Ping;
    procedure Disconnect;
    procedure CheckIsActive;
    // call onAccept Method of protocol, and if protocol not accept connection or
    // can not be accepted from other reasons return false else return true
    function TryAcceptConnection(aProtocol: THttpApiWebSocketServerProtocol;
      Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
  public
    /// Send data to client
    procedure Send(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: Pointer; aBufferSize: ULONG);
    /// Close connection
    procedure Close(aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: Pointer; aBufferSize: ULONG);
    /// Index of connection in protocol's connection list
    property Index: integer
      read fIndex;
    /// Protocol of connection
    property Protocol: THttpApiWebSocketServerProtocol
       read fProtocol;
    /// Custom user data
    property PrivateData: pointer
      read fPrivateData write fPrivateData;
    /// Access to the current state of this connection
    property State: TWebSocketState
      read fState;
  end;

  PHttpApiWebSocketConnection = ^THttpApiWebSocketConnection;

  THttpApiWebSocketConnectionVector =
    array[0..MaxInt div SizeOf(PHttpApiWebSocketConnection) - 1] of
    PHttpApiWebSocketConnection;

  PHttpApiWebSocketConnectionVector = ^THttpApiWebSocketConnectionVector;

  /// Event handlers for WebSocket
  TOnHttpApiWebSocketServerAcceptEvent = function(Ctxt: THttpServerRequest;
    var Conn: THttpApiWebSocketConnection): boolean of object;
  TOnHttpApiWebSocketServerMessageEvent = procedure(const Conn: THttpApiWebSocketConnection;
    aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG) of object;
  TOnHttpApiWebSocketServerConnectEvent = procedure(const Conn: THttpApiWebSocketConnection) of object;
  TOnHttpApiWebSocketServerDisconnectEvent = procedure(const Conn: THttpApiWebSocketConnection;
    aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG) of object;

  /// Protocol Handler of websocket endpoints events
  // - maintains a list of all WebSockets clients for a given protocol
  THttpApiWebSocketServerProtocol = class
  private
    fName: RawUtf8;
    fManualFragmentManagement: boolean;
    fOnAccept: TOnHttpApiWebSocketServerAcceptEvent;
    fOnMessage: TOnHttpApiWebSocketServerMessageEvent;
    fOnFragment: TOnHttpApiWebSocketServerMessageEvent;
    fOnConnect: TOnHttpApiWebSocketServerConnectEvent;
    fOnDisconnect: TOnHttpApiWebSocketServerDisconnectEvent;
    fConnections: PHttpApiWebSocketConnectionVector;
    fConnectionsCapacity: integer;
    //Count of used connections. Some of them can be nil(if not used more)
    fConnectionsCount: integer;
    fFirstEmptyConnectionIndex: integer;
    fServer: THttpApiWebSocketServer;
    fSafe: TRTLCriticalSection;
    fPendingForClose: TSynList;
    fIndex: integer;
    function AddConnection(aConn: PHttpApiWebSocketConnection): integer;
    procedure RemoveConnection(index: integer);
    procedure doShutdown;
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
    /// identify the endpoint instance
    property Index: integer
      read fIndex;
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

    /// Send message to the WebSocket connection identified by its index
    function Send(index: integer; aBufferType: ULONG;
      aBuffer: Pointer; aBufferSize: ULONG): boolean;
    /// Send message to all connections of this protocol
    function Broadcast(aBufferType: ULONG;
      aBuffer: Pointer; aBufferSize: ULONG): boolean;
    /// Close WebSocket connection identified by its index
    function Close(index: integer; aStatus: WEB_SOCKET_CLOSE_STATUS;
      aBuffer: Pointer; aBufferSize: ULONG): boolean;
  end;

  THttpApiWebSocketServerProtocolDynArray =
    array of THttpApiWebSocketServerProtocol;
  PHttpApiWebSocketServerProtocolDynArray =
    ^THttpApiWebSocketServerProtocolDynArray;

  /// HTTP & WebSocket server using fast http.sys kernel-mode server
  // - can be used like simple THttpApiServer
  // - when AddUrlWebSocket is called WebSocket support are added
  // in this case WebSocket will receiving the frames in asynchronous
  THttpApiWebSocketServer = class(THttpApiServer)
  private
    fThreadPoolServer: TSynThreadPoolHttpApiWebSocketServer;
    fGuard: TSynWebSocketGuard;
    fLastConnection: PHttpApiWebSocketConnection;
    fPingTimeout: integer;
    fRegisteredProtocols: PHttpApiWebSocketServerProtocolDynArray;
    fOnWSThreadStart: TOnNotifyThread;
    fOnWSThreadTerminate: TOnNotifyThread;
    fSendOverlaped: TOverlapped;
    fServiceOverlaped: TOverlapped;
    fOnServiceMessage: TThreadMethod;
    procedure SetOnWSThreadTerminate(const Value: TOnNotifyThread);
    function GetProtocol(index: integer): THttpApiWebSocketServerProtocol;
    function getProtocolsCount: integer;
    procedure SetOnWSThreadStart(const Value: TOnNotifyThread);
  protected
    function UpgradeToWebSocket(Ctxt: THttpServerRequestAbstract): cardinal;
    procedure DoAfterResponse(Ctxt: THttpServerRequest;
      const Code: cardinal); override;
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; override;
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
    /// create a WebSockets processing clone from the main thread
    // - do not use directly - is called during thread pool creation
    constructor CreateClone(From: THttpApiServer); override;
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
    // - if connection not receive messages longer than this timeout
    // TSynWebSocketGuard will send ping frame
    // - if connection not receive any messages longer than double of
    // this timeout it will be closed
    property PingTimeout: integer
      read fPingTimeout;
    /// access to the associated endpoints
    property Protocols[index: integer]: THttpApiWebSocketServerProtocol
      read GetProtocol;
    /// access to the associated endpoints count
    property ProtocolsCount: integer
      read getProtocolsCount;
    /// event called when the processing thread starts
    property OnWSThreadStart: TOnNotifyThread
      read FOnWSThreadStart write SetOnWSThreadStart;
    /// event called when the processing thread termintes
    property OnWSThreadTerminate: TOnNotifyThread
      read FOnWSThreadTerminate write SetOnWSThreadTerminate;
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
    procedure OnThreadStart(Sender: TThread);
    procedure OnThreadTerminate(Sender: TThread);
    function NeedStopOnIOError: boolean; override;
    // aContext is a PHttpApiWebSocketConnection, or fServer.fServiceOverlaped
    // (SendServiceMessage) or fServer.fSendOverlaped (WriteData)
    procedure Task(aCaller: TSynThreadPoolWorkThread;
      aContext: Pointer); override;
  public
    /// initialize the thread pool
    constructor Create(Server: THttpApiWebSocketServer;
      NumberOfThreads: integer = 1); reintroduce;
  end;

  /// Thread for closing deprecated WebSocket connections
  // - i.e. which have not responsed after PingTimeout interval
  TSynWebSocketGuard = class(TThread)
  protected
    fServer: THttpApiWebSocketServer;
    fSmallWait, fWaitCount: integer;
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(Server: THttpApiWebSocketServer); reintroduce;
  end;

{$endif USEWININET}


implementation


{ ******************** Shared Server-Side HTTP Process }

{ THttpServerRequest }

var
  // global request counter if no THttpServer is defined
  GlobalRequestID: integer;

constructor THttpServerRequest.Create(aServer: THttpServerGeneric;
  aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
  aConnectionFlags: THttpServerRequestFlags;
  aConnectionOpaque: PHttpServerConnectionOpaque);
var
  id: PInteger;
begin
  inherited Create;
  fServer := aServer;
  fConnectionID := aConnectionID;
  fConnectionThread := aConnectionThread;
  fConnectionOpaque := aConnectionOpaque;
  fConnectionFlags := aConnectionFlags;
  if fServer = nil then
    id := @GlobalRequestID
  else
    id := @fServer.fCurrentRequestID;
  fRequestID := InterLockedIncrement(id^);
  if fRequestID = maxInt - 2048 then
    id^ := 0; // ensure no overflow (31-bit range)
end;

procedure THttpServerRequest.Recycle(aConnectionID: THttpServerConnectionID;
  aConnectionFlags: THttpServerRequestFlags);
begin
  fConnectionID := aConnectionID;
  fConnectionFlags := aConnectionFlags;
  fErrorMessage := '';
end;

const
  _CMD_200: array[boolean] of string[17] = (
    'HTTP/1.1 200 OK'#13#10,
    'HTTP/1.0 200 OK'#13#10);
  _CMD_ERR: array[boolean] of string[9] = (
    'HTTP/1.1 ',
    'HTTP/1.0 ');

function THttpServerRequest.SetupResponse(var Context: THttpRequestContext;
  CompressGz, MaxSizeAtOnce: integer): PRawByteStringBuffer;

  procedure ProcessStaticFile;
  var
    fn: TFileName;
  begin
    ExtractHeader(fOutCustomHeaders, 'CONTENT-TYPE:', fOutContentType);
    Utf8ToFileName(OutContent, fn);
    if (not Assigned(fServer.OnSendFile)) or
       (not fServer.OnSendFile(self, fn)) then
      if Context.ContentFromFile(fn, CompressGz) then
        OutContent := Context.Content
      else
      begin
        FormatString('Impossible to find %', [fn], fErrorMessage);
        fRespStatus := HTTP_NOTFOUND;
      end;
  end;

  procedure ProcessErrorMessage;
  begin
    OutCustomHeaders := '';
    OutContentType := 'text/html; charset=utf-8'; // create message to display
    StatusCodeToReason(fRespStatus, fRespReason);
    FormatUtf8('<body style="font-family:verdana">'#10 +
      '<h1>% Server Error %</h1><hr><p>HTTP % %<p>%<p><small>' + XPOWEREDVALUE,
      [fServer.ServerName, fRespStatus, fRespStatus, fRespReason,
       HtmlEscapeString(fErrorMessage)], RawUtf8(fOutContent));
  end;

var
  P, PEnd: PUtf8Char;
  len: PtrInt;
  h: PRawByteStringBuffer;
begin
  // note: caller should have set hfConnectionClose in Context.HeaderFlags
  // process content
  Context.ContentLength := 0;
  if OutContentType = NORESPONSE_CONTENT_TYPE then
    OutContentType := '' // true HTTP always expects a response
  else if (OutContent <> '') and
          (OutContentType = STATICFILE_CONTENT_TYPE) then
    ProcessStaticFile;
  if fErrorMessage <> '' then
    ProcessErrorMessage;
  // append Command
  h := @Context.Head;
  h^.Reset;
  if fRespStatus = HTTP_SUCCESS then // optimistic approach
    h^.AppendShort(_CMD_200[hfConnectionClose in Context.HeaderFlags])
  else
  begin
    h^.AppendShort(_CMD_ERR[hfConnectionClose in Context.HeaderFlags]);
    StatusCodeToReason(fRespStatus, fRespReason);
    h^.Append(fRespStatus);
    h^.Append(' ');
    h^.Append(fRespReason);
    h^.AppendCRLF;
  end;
  // append (and sanitize) custom headers from Request() method
  P := pointer(OutCustomHeaders);
  if P <> nil then
  begin
    PEnd := P + length(OutCustomHeaders);
    repeat
      len := BufferLineLength(P, PEnd);
      if len > 0 then // no void line (means headers ending)
      begin
        if (PCardinal(P)^ or $20202020 =
             ord('c') + ord('o') shl 8 + ord('n') shl 16 + ord('t') shl 24) and
           (PCardinal(P + 4)^ or $20202020 =
             ord('e') + ord('n') shl 8 + ord('t') shl 16 + ord('-') shl 24) and
           (PCardinal(P + 8)^ or $20202020 =
             ord('e') + ord('n') shl 8 + ord('c') shl 16 + ord('o') shl 24) and
           (PCardinal(P + 12)^ or $20202020 =
             ord('d') + ord('i') shl 8 + ord('n') shl 16 + ord('g') shl 24) and
           (P[16] = ':') then
          // custom CONTENT-ENCODING: don't compress
          integer(Context.CompressAcceptHeader) := 0;
        h^.Append(P, len); // normalize CR/LF endings
        h^.AppendCRLF;
        inc(P, len);
      end;
      while P^ in [#10, #13] do
        inc(P);
    until P^ = #0;
  end;
  // generic headers
  h^.AppendShort('Server: ');
  h^.Append(fServer.ServerName);
  h^.AppendCRLF;
  if hsoIncludeDateHeader in fServer.Options then
    fServer.AppendHttpDate(h^);
  if not (hsoNoXPoweredHeader in fServer.Options) then
    h^.AppendShort(XPOWEREDNAME + ': ' + XPOWEREDVALUE + #13#10);
  Context.Content := OutContent;
  Context.ContentType := OutContentType;
  OutContent := ''; // release body memory ASAP
  result := Context.CompressContentAndFinalizeHead(MaxSizeAtOnce); // also set State
  // now TAsyncConnectionsSockets.Write(result) should be called
end;

procedure THttpServerRequest.SetErrorMessage(const Fmt: RawUtf8;
  const Args: array of const);
begin
  FormatString(Fmt, Args, fErrorMessage);
end;

{$ifdef USEWININET}

function THttpServerRequest.GetFullURL: SynUnicode;
begin
  if fHttpApiRequest = nil then
    result := ''
  else
    // fHttpApiRequest^.CookedUrl.FullUrlLength is in bytes -> use ending #0
    result := fHttpApiRequest^.CookedUrl.pFullUrl;
end;

{$endif USEWININET}


{ THttpServerGeneric }

constructor THttpServerGeneric.Create(const OnStart, OnStop: TOnNotifyThread;
  const ProcessName: RawUtf8; ProcessOptions: THttpServerOptions);
begin
  SetServerName('mORMot2 (' + OS_TEXT + ')');
  fOptions := ProcessOptions;
  inherited Create(hsoCreateSuspended in fOptions, OnStart, OnStop, ProcessName);
end;

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer);
begin
  RegisterCompressFunc(
    fCompress, aFunction, fCompressAcceptEncoding, aCompressMinSize);
end;

procedure THttpServerGeneric.Shutdown;
begin
  if self <> nil then
    fShutdownInProgress := true;
end;

function THttpServerGeneric.Request(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if (self = nil) or
     fShutdownInProgress then
    result := HTTP_NOTFOUND
  else
  begin
    if Assigned(Ctxt.ConnectionThread) and
       (not Assigned(Ctxt.ConnectionThread.StartNotified)) then
      NotifyThreadStart(Ctxt.ConnectionThread);
    if Assigned(OnRequest) then
      result := OnRequest(Ctxt)
    else
      result := HTTP_NOTFOUND;
  end;
end;

function THttpServerGeneric.{%H-}Callback(Ctxt: THttpServerRequest;
  aNonBlocking: boolean): cardinal;
begin
  raise EHttpServer.CreateUtf8('%.Callback is not implemented: try to use ' +
    'another communication protocol, e.g. WebSockets', [self]);
end;

procedure THttpServerGeneric.ParseRemoteIPConnID(const Headers: RawUtf8;
  var RemoteIP: RawUtf8; var RemoteConnID: THttpServerConnectionID);
var
  P: PUtf8Char;
begin
  if self = nil then // = nil e.g. from TRtspOverHttpServer
    exit;
  // real Internet IP (replace RemoteIP='127.0.0.1' from a proxy)
  if fRemoteIPHeaderUpper <> '' then
    FindNameValue(Headers, pointer(fRemoteIPHeaderUpper),
      RemoteIP, {keepnotfound=}true);
  // real proxy connection ID
  if fRemoteConnIDHeaderUpper <> '' then
  begin
    P := FindNameValue(pointer(Headers), pointer(fRemoteConnIDHeaderUpper));
    if P <> nil then
      SetQWord(P, PQWord(@RemoteConnID)^);
  end;
  if RemoteConnID = 0 then
    // fallback to 31-bit sequence
    RemoteConnID := NextConnectionID;
end;

procedure THttpServerGeneric.AppendHttpDate(var Dest: TRawByteStringBuffer);
begin
  Dest.AppendShort(HttpDateNowUtc);
end;

function THttpServerGeneric.CanNotifyCallback: boolean;
begin
  result := (self <> nil) and
            (fCallbackSendDelay <> nil);
end;

procedure THttpServerGeneric.SetServerName(const aName: RawUtf8);
begin
  fServerName := aName;
end;

procedure THttpServerGeneric.SetOnRequest(
  const aRequest: TOnHttpServerRequest);
begin
  fOnRequest := aRequest;
end;

procedure THttpServerGeneric.SetOnBeforeBody(
  const aEvent: TOnHttpServerBeforeBody);
begin
  fOnBeforeBody := aEvent;
end;

procedure THttpServerGeneric.SetOnBeforeRequest(
  const aEvent: TOnHttpServerRequest);
begin
  fOnBeforeRequest := aEvent;
end;

procedure THttpServerGeneric.SetOnAfterRequest(
  const aEvent: TOnHttpServerRequest);
begin
  fOnAfterRequest := aEvent;
end;

procedure THttpServerGeneric.SetOnAfterResponse(
  const aEvent: TOnHttpServerAfterResponse);
begin
  fOnAfterResponse := aEvent;
end;

function THttpServerGeneric.DoBeforeRequest(Ctxt: THttpServerRequest): cardinal;
begin
  if Assigned(fOnBeforeRequest) then
    result := fOnBeforeRequest(Ctxt)
  else
    result := 0;
end;

function THttpServerGeneric.DoAfterRequest(Ctxt: THttpServerRequest): cardinal;
begin
  if Assigned(fOnAfterRequest) then
    result := fOnAfterRequest(Ctxt)
  else
    result := 0;
end;

procedure THttpServerGeneric.DoAfterResponse(Ctxt: THttpServerRequest;
  const Code: cardinal);
begin
  if Assigned(fOnAfterResponse) then
    fOnAfterResponse(Ctxt.Method, Ctxt.Url, Ctxt.RemoteIP, Code);
end;

procedure THttpServerGeneric.SetMaximumAllowedContentLength(aMax: cardinal);
begin
  fMaximumAllowedContentLength := aMax;
end;

procedure THttpServerGeneric.SetRemoteIPHeader(const aHeader: RawUtf8);
begin
  fRemoteIPHeader := aHeader;
  fRemoteIPHeaderUpper := UpperCase(aHeader);
end;

procedure THttpServerGeneric.SetRemoteConnIDHeader(const aHeader: RawUtf8);
begin
  fRemoteConnIDHeader := aHeader;
  fRemoteConnIDHeaderUpper := UpperCase(aHeader);
end;

function THttpServerGeneric.NextConnectionID: integer;
begin
  result := InterlockedIncrement(fCurrentConnectionID);
  if result = maxInt - 2048 then // paranoid 31-bit counter reset to ensure >0
    fCurrentConnectionID := 0;
end;


const
  // was generated from InitNetTlsContextSelfSignedServer commented lines
  PRIVKEY_PFX: array[0..2400] of byte = (
    $30, $82, $09, $5D, $02, $01, $03, $30, $82, $09, $27, $06, $09, $2A, $86, $48,
    $86, $F7, $0D, $01, $07, $01, $A0, $82, $09, $18, $04, $82, $09, $14, $30, $82,
    $09, $10, $30, $82, $03, $C7, $06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $07,
    $06, $A0, $82, $03, $B8, $30, $82, $03, $B4, $02, $01, $00, $30, $82, $03, $AD,
    $06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $07, $01, $30, $1C, $06, $0A, $2A,
    $86, $48, $86, $F7, $0D, $01, $0C, $01, $06, $30, $0E, $04, $08, $D4, $F9, $E6,
    $DE, $12, $70, $DD, $EE, $02, $02, $08, $00, $80, $82, $03, $80, $3A, $91, $73,
    $2F, $46, $F9, $49, $00, $B6, $90, $5B, $59, $8F, $37, $6F, $19, $6F, $85, $EF,
    $01, $97, $1D, $CD, $A6, $C5, $04, $DF, $0A, $0F, $87, $28, $59, $80, $9A, $88,
    $5F, $7F, $8B, $B2, $97, $A5, $13, $6E, $3E, $AB, $04, $B2, $5F, $62, $12, $0B,
    $30, $A5, $A7, $CC, $54, $9A, $8A, $6B, $6B, $8A, $7F, $0C, $CD, $AF, $BB, $EA,
    $78, $A5, $7F, $11, $85, $13, $6F, $DB, $61, $40, $D2, $26, $7C, $EB, $99, $A2,
    $6F, $1B, $A4, $71, $77, $44, $7A, $10, $EC, $02, $3D, $26, $48, $72, $77, $10,
    $07, $9E, $FE, $75, $20, $7A, $3B, $F2, $D8, $74, $74, $E8, $5C, $FF, $12, $DF,
    $6C, $ED, $54, $C1, $76, $29, $D7, $2D, $DD, $FA, $3A, $32, $26, $7D, $F0, $31,
    $CF, $2D, $06, $37, $83, $9B, $39, $92, $2B, $78, $1D, $17, $1A, $D3, $4B, $24,
    $70, $00, $9F, $66, $8D, $3D, $BE, $05, $E3, $63, $7C, $2E, $58, $F7, $DB, $6D,
    $4F, $3E, $36, $CF, $0B, $C5, $5F, $B1, $AE, $6D, $E2, $61, $63, $12, $4C, $99,
    $24, $3E, $C9, $CF, $B9, $97, $20, $4A, $55, $41, $35, $F1, $6C, $43, $9F, $67,
    $63, $DA, $14, $31, $57, $D2, $13, $B2, $AB, $59, $6B, $30, $D7, $1D, $2C, $54,
    $ED, $73, $0C, $2D, $AA, $F9, $11, $13, $64, $88, $56, $D8, $B6, $16, $F9, $E7,
    $9C, $03, $DA, $87, $2F, $7B, $4B, $C2, $EE, $1B, $2C, $53, $06, $74, $D2, $11,
    $7F, $81, $31, $E8, $EE, $84, $40, $27, $1C, $18, $FA, $66, $02, $B1, $67, $42,
    $4A, $B9, $4D, $8B, $96, $95, $6B, $AB, $1A, $48, $47, $44, $0E, $63, $2C, $26,
    $27, $7C, $C1, $C8, $7C, $74, $B8, $1C, $F5, $9D, $6F, $09, $0F, $27, $F0, $B0,
    $46, $68, $0C, $99, $03, $80, $E5, $81, $2B, $74, $E6, $B4, $02, $12, $AD, $EF,
    $A8, $E6, $BE, $36, $BF, $24, $2B, $AB, $B5, $4D, $33, $7D, $CD, $A0, $DB, $6D,
    $19, $68, $C9, $00, $DB, $A3, $D7, $02, $A8, $8A, $FB, $2F, $71, $4A, $A7, $82,
    $06, $CD, $BC, $E3, $88, $12, $CA, $35, $66, $66, $36, $CF, $2D, $E9, $97, $F8,
    $C1, $03, $48, $9C, $7A, $F4, $5F, $F5, $BC, $FD, $67, $62, $90, $19, $25, $62,
    $03, $B2, $B1, $AE, $27, $FF, $A0, $D5, $47, $0E, $A1, $21, $29, $C8, $A5, $19,
    $D3, $D5, $F1, $0C, $51, $5B, $4A, $DB, $FB, $D8, $A6, $49, $DB, $3A, $8E, $9D,
    $64, $BE, $24, $01, $80, $F0, $35, $4E, $DA, $83, $5A, $DB, $83, $D7, $7C, $01,
    $1B, $5C, $8F, $B3, $D7, $B7, $49, $9F, $AF, $C7, $29, $87, $4D, $73, $EF, $D0,
    $D7, $BE, $BF, $C2, $09, $60, $BB, $FC, $5B, $64, $24, $04, $E6, $09, $9A, $19,
    $68, $61, $9C, $DA, $62, $5E, $A4, $8A, $38, $5D, $DE, $BD, $4F, $BF, $78, $04,
    $6D, $CE, $9A, $E2, $E4, $E7, $93, $A1, $E9, $CA, $F1, $3D, $9B, $E5, $14, $C8,
    $98, $FB, $29, $B0, $1F, $01, $48, $40, $80, $67, $2B, $F2, $30, $21, $1E, $A9,
    $4A, $B4, $8C, $BE, $DD, $9B, $3E, $2D, $82, $37, $63, $51, $24, $17, $AC, $9A,
    $49, $BD, $AF, $DF, $2C, $CE, $BC, $D5, $A9, $43, $1F, $7A, $9A, $BF, $7B, $5A,
    $3E, $F3, $12, $55, $67, $7D, $97, $9B, $B6, $35, $4F, $D4, $97, $DF, $2C, $D9,
    $40, $32, $1B, $92, $8E, $25, $6E, $F0, $7A, $48, $41, $2B, $9F, $55, $7E, $D2,
    $E5, $58, $85, $BA, $73, $51, $5C, $3F, $95, $18, $F6, $9B, $6A, $8D, $85, $25,
    $A2, $5E, $F0, $4F, $F7, $96, $51, $CA, $AC, $FF, $C9, $CC, $96, $4F, $C6, $B0,
    $63, $60, $C1, $50, $9A, $5B, $0D, $CA, $8F, $19, $CC, $87, $89, $6A, $31, $0F,
    $10, $DF, $C8, $26, $64, $09, $2E, $59, $94, $22, $24, $E7, $5B, $59, $EB, $86,
    $F9, $99, $EE, $39, $28, $14, $0C, $A7, $C4, $1F, $B5, $69, $93, $C1, $CC, $DC,
    $14, $35, $DE, $A8, $EA, $14, $6F, $C0, $D3, $13, $98, $2A, $A9, $55, $D6, $B6,
    $D4, $84, $0C, $92, $B2, $64, $28, $B5, $0F, $89, $A4, $F2, $7F, $3B, $3C, $35,
    $5D, $0B, $4A, $42, $6B, $CF, $B4, $70, $78, $B3, $5E, $3E, $3D, $6E, $86, $29,
    $5F, $F0, $27, $9A, $31, $A5, $6F, $94, $AB, $22, $8D, $E7, $FB, $21, $72, $DA,
    $5A, $CF, $7B, $6A, $23, $F7, $6C, $05, $6D, $E1, $17, $24, $36, $7C, $3F, $56,
    $A7, $F4, $96, $8D, $B1, $9E, $D1, $90, $F0, $9D, $F8, $32, $4B, $24, $B5, $5B,
    $30, $B6, $B1, $3E, $9D, $D0, $FC, $56, $19, $41, $0A, $90, $CB, $E2, $BF, $E4,
    $55, $D1, $F1, $14, $AF, $90, $B2, $13, $4E, $16, $2A, $1B, $43, $D9, $34, $14,
    $17, $C8, $8A, $FE, $1C, $A0, $66, $40, $5E, $6B, $9F, $EE, $15, $BF, $90, $D7,
    $6D, $87, $E2, $03, $10, $2A, $FF, $18, $E5, $A1, $DA, $00, $9B, $B7, $E6, $1E,
    $3C, $5C, $8A, $36, $1E, $33, $E9, $4D, $89, $DA, $6C, $49, $2F, $0D, $7B, $54,
    $68, $30, $B3, $AC, $AF, $5F, $6F, $FF, $CB, $EE, $D7, $21, $28, $73, $7D, $32,
    $32, $D5, $C2, $74, $08, $C3, $01, $7E, $80, $C1, $F4, $CB, $AC, $91, $05, $5D,
    $B3, $D2, $B6, $95, $D4, $D0, $19, $B8, $25, $46, $D2, $EA, $17, $3A, $BF, $D3,
    $FF, $DC, $A1, $85, $A8, $56, $01, $1C, $24, $55, $BB, $2D, $6D, $7A, $07, $AC,
    $C3, $1A, $DC, $93, $97, $60, $9B, $6F, $AA, $4C, $2E, $61, $86, $30, $82, $05,
    $41, $06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $07, $01, $A0, $82, $05, $32,
    $04, $82, $05, $2E, $30, $82, $05, $2A, $30, $82, $05, $26, $06, $0B, $2A, $86,
    $48, $86, $F7, $0D, $01, $0C, $0A, $01, $02, $A0, $82, $04, $EE, $30, $82, $04,
    $EA, $30, $1C, $06, $0A, $2A, $86, $48, $86, $F7, $0D, $01, $0C, $01, $03, $30,
    $0E, $04, $08, $04, $E0, $0A, $B0, $D6, $79, $A5, $44, $02, $02, $08, $00, $04,
    $82, $04, $C8, $7F, $48, $8D, $D1, $AB, $5E, $A1, $D8, $D0, $63, $62, $6A, $D2,
    $AF, $DD, $20, $DE, $91, $4D, $9A, $2F, $78, $20, $0C, $84, $A2, $C9, $38, $69,
    $FE, $8A, $AA, $8E, $B6, $3E, $4E, $D7, $CA, $F4, $2E, $6B, $D6, $9D, $C0, $3B,
    $5A, $4E, $7B, $89, $B8, $86, $38, $29, $87, $08, $A4, $B0, $2A, $ED, $CA, $13,
    $B2, $FE, $15, $3E, $87, $BD, $1D, $AD, $43, $1F, $62, $93, $C1, $B8, $9F, $93,
    $46, $74, $B3, $F4, $34, $D3, $9C, $97, $E1, $38, $09, $4C, $F4, $19, $35, $81,
    $34, $27, $93, $C7, $B3, $FA, $AF, $58, $46, $73, $CC, $56, $91, $9F, $C8, $DC,
    $6B, $04, $AF, $F1, $67, $65, $3D, $2C, $8E, $D1, $CC, $AC, $B7, $94, $41, $EA,
    $56, $C4, $45, $ED, $C9, $2C, $BB, $C1, $0F, $05, $06, $73, $03, $33, $D1, $C2,
    $BC, $34, $B2, $D5, $EA, $78, $5A, $22, $CA, $C3, $B4, $31, $43, $47, $92, $E8,
    $B4, $21, $F2, $70, $0E, $B5, $1B, $9A, $07, $86, $45, $66, $8F, $DD, $90, $2E,
    $9B, $AF, $9F, $D4, $04, $42, $EC, $07, $78, $C8, $66, $0F, $19, $AE, $64, $F6,
    $99, $11, $6C, $71, $DB, $58, $F2, $CE, $13, $29, $FF, $C2, $4A, $C7, $4A, $02,
    $D8, $28, $F7, $54, $DC, $A8, $FB, $30, $DF, $53, $98, $85, $6D, $3C, $CF, $16,
    $93, $B9, $8B, $F5, $39, $80, $CD, $84, $36, $0A, $0F, $2F, $A2, $9E, $CB, $9B,
    $83, $F0, $49, $C5, $34, $B9, $4B, $1D, $5A, $46, $56, $8F, $A8, $05, $E0, $4C,
    $51, $41, $A4, $6B, $07, $38, $AF, $F4, $43, $81, $8D, $7D, $54, $DD, $85, $DA,
    $39, $2B, $0E, $EF, $44, $90, $E8, $99, $67, $65, $32, $5B, $F1, $CA, $1F, $CD,
    $58, $2D, $B3, $1E, $10, $4F, $B5, $6E, $23, $A0, $26, $D3, $22, $A7, $D9, $BD,
    $CC, $E6, $25, $52, $FE, $00, $70, $B3, $A8, $E6, $BE, $42, $AE, $09, $7A, $AD,
    $46, $EC, $03, $A5, $12, $D4, $07, $23, $A7, $9E, $7E, $42, $00, $48, $13, $96,
    $E5, $3B, $55, $13, $2B, $A6, $E6, $6C, $9A, $25, $E0, $53, $27, $B5, $E7, $5F,
    $2B, $96, $B3, $7C, $77, $A9, $D7, $F7, $14, $C7, $A8, $E1, $19, $0F, $5C, $88,
    $E4, $F2, $1C, $AD, $71, $E8, $8F, $B2, $F6, $88, $B9, $2A, $57, $63, $EF, $B5,
    $D7, $CA, $7C, $95, $14, $5E, $9D, $21, $6C, $6F, $87, $37, $88, $B5, $5E, $F1,
    $8E, $0C, $33, $4B, $32, $A5, $AD, $3C, $B8, $E1, $BC, $1C, $74, $C2, $36, $D4,
    $14, $37, $96, $1F, $3D, $93, $EF, $23, $5A, $59, $B5, $13, $CD, $34, $C7, $D6,
    $78, $F5, $DE, $1B, $38, $EC, $70, $D3, $9E, $D4, $08, $EF, $B7, $9C, $34, $14,
    $12, $9A, $7D, $D0, $7A, $09, $74, $16, $5F, $0E, $88, $CF, $F4, $D7, $F7, $30,
    $97, $D7, $D2, $18, $FF, $C7, $62, $8D, $37, $D0, $77, $66, $FD, $B3, $EE, $86,
    $D9, $1B, $9E, $7C, $D0, $D5, $B8, $D7, $F1, $3C, $57, $BE, $51, $07, $A5, $25,
    $37, $E4, $73, $5E, $60, $B7, $98, $99, $6A, $C1, $F0, $35, $FF, $F6, $D7, $12,
    $44, $7B, $1E, $70, $BF, $32, $E2, $49, $58, $78, $41, $22, $EE, $B5, $99, $2B,
    $08, $C6, $A3, $E2, $C6, $65, $06, $8E, $D1, $FB, $CB, $2D, $D9, $0B, $92, $D2,
    $05, $AB, $91, $EA, $43, $62, $16, $B3, $4B, $73, $7A, $BD, $C5, $41, $A0, $2D,
    $6D, $28, $44, $A2, $93, $62, $2E, $67, $6B, $4A, $A0, $AB, $5E, $20, $A2, $F3,
    $00, $56, $B4, $A8, $E8, $A3, $DA, $08, $99, $83, $C2, $AD, $8A, $7F, $85, $70,
    $3E, $CE, $2F, $39, $06, $77, $A8, $77, $3E, $BF, $E5, $C8, $38, $DC, $68, $28,
    $35, $49, $C8, $A8, $E3, $FD, $9D, $05, $DC, $70, $4C, $A2, $0D, $2C, $44, $37,
    $F4, $F3, $B8, $0A, $99, $3C, $97, $10, $92, $77, $58, $B2, $E3, $00, $A2, $0E,
    $34, $AF, $5F, $C6, $1D, $22, $DD, $34, $57, $DC, $5B, $F1, $F1, $6E, $03, $12,
    $C2, $6C, $AD, $75, $03, $BF, $CD, $7A, $CD, $52, $0A, $75, $A1, $31, $B5, $19,
    $DF, $52, $09, $3B, $94, $76, $EE, $1A, $5A, $A8, $8D, $3B, $EE, $B7, $86, $C6,
    $65, $C7, $E8, $0B, $3C, $B9, $EE, $7D, $80, $22, $89, $3D, $F8, $6C, $9E, $4F,
    $6E, $C8, $F8, $3A, $54, $76, $B5, $89, $6B, $05, $A5, $C9, $68, $68, $0B, $33,
    $E5, $55, $E8, $B2, $F9, $39, $DC, $C8, $0A, $13, $94, $01, $D2, $A1, $0A, $42,
    $F5, $37, $A4, $18, $C9, $97, $BB, $A4, $93, $4C, $49, $BB, $FB, $B0, $F5, $4E,
    $C5, $D3, $3B, $BD, $A0, $37, $10, $9F, $8F, $E7, $BB, $8A, $6D, $FE, $C3, $6C,
    $36, $A6, $3D, $C6, $ED, $D0, $7D, $68, $37, $11, $22, $16, $82, $AB, $C4, $02,
    $EC, $EB, $A0, $7D, $0E, $22, $79, $CE, $6A, $39, $45, $31, $5C, $99, $75, $C3,
    $6A, $B9, $A1, $00, $2D, $4D, $4D, $F5, $AC, $CC, $1E, $0D, $36, $A7, $36, $40,
    $53, $6C, $A8, $6C, $B0, $F8, $27, $30, $68, $AE, $06, $39, $A5, $89, $86, $CC,
    $BB, $B0, $CA, $43, $62, $1D, $71, $6A, $30, $62, $B9, $BC, $DC, $8A, $D1, $23,
    $04, $6F, $35, $4B, $6F, $81, $B8, $31, $91, $26, $83, $28, $E6, $2E, $D3, $84,
    $FB, $53, $F9, $6F, $B0, $0E, $37, $E1, $CE, $4D, $6F, $35, $14, $37, $4B, $EE,
    $31, $46, $EE, $85, $DF, $04, $0D, $3D, $F0, $AC, $D2, $B7, $EF, $AE, $87, $7A,
    $A8, $C0, $9F, $98, $4E, $E9, $C0, $A6, $7C, $E9, $FF, $D7, $76, $72, $82, $CA,
    $89, $FB, $94, $9C, $67, $7A, $47, $47, $5C, $2C, $17, $61, $96, $15, $D6, $26,
    $BB, $0F, $EF, $F0, $C7, $23, $BA, $39, $8A, $08, $B5, $F3, $68, $DE, $54, $80,
    $15, $A3, $43, $A5, $DA, $0B, $60, $FE, $F9, $BF, $54, $FE, $21, $34, $08, $AB,
    $0D, $59, $A8, $DC, $8E, $7B, $54, $46, $4D, $F7, $B6, $AC, $DF, $1D, $6F, $50,
    $9C, $3C, $17, $5D, $19, $4C, $48, $21, $D2, $5B, $F0, $6F, $A7, $2B, $D4, $B0,
    $87, $FD, $42, $D0, $87, $D3, $BE, $7A, $01, $61, $16, $8A, $A3, $BC, $83, $1D,
    $BB, $6A, $FB, $51, $EB, $6B, $37, $F9, $1E, $E8, $FF, $0A, $4F, $46, $14, $1C,
    $04, $EE, $CD, $8D, $4A, $33, $CD, $8D, $4F, $0B, $24, $2C, $E1, $25, $48, $42,
    $A2, $EB, $04, $F4, $7E, $30, $62, $AE, $CC, $20, $1A, $A6, $38, $5C, $D5, $F3,
    $27, $07, $81, $75, $9C, $F4, $D0, $87, $79, $6F, $0A, $28, $3D, $A5, $22, $B8,
    $EC, $C7, $B3, $C0, $F5, $DE, $77, $6C, $7F, $C3, $01, $1E, $FA, $88, $83, $BB,
    $D0, $9C, $29, $82, $11, $DB, $D0, $99, $C7, $D8, $E0, $2F, $E0, $22, $22, $0D,
    $2A, $E7, $29, $64, $B3, $72, $A2, $08, $5A, $FA, $08, $86, $D4, $E5, $FE, $05,
    $08, $64, $CC, $C3, $53, $7F, $9A, $2E, $93, $21, $C2, $FA, $16, $37, $3E, $28,
    $CF, $CA, $57, $DA, $BB, $15, $1A, $C6, $41, $39, $BE, $D7, $F9, $9E, $78, $1B,
    $83, $A7, $6D, $1E, $22, $BE, $49, $7F, $64, $41, $5D, $A8, $11, $40, $D7, $AD,
    $43, $F6, $C3, $9E, $7E, $3A, $95, $2D, $27, $04, $80, $95, $02, $60, $A6, $A6,
    $55, $25, $BD, $64, $E2, $D0, $99, $B5, $D9, $4B, $42, $F5, $69, $CE, $9A, $FE,
    $26, $D1, $C4, $9E, $29, $3D, $AF, $85, $2F, $8E, $E0, $0A, $69, $F2, $69, $EE,
    $66, $C2, $F7, $AB, $81, $BC, $82, $01, $22, $B6, $45, $31, $25, $30, $23, $06,
    $09, $2A, $86, $48, $86, $F7, $0D, $01, $09, $15, $31, $16, $04, $14, $11, $9C,
    $AB, $D1, $44, $93, $91, $54, $3C, $52, $A0, $66, $4C, $A5, $99, $DB, $42, $62,
    $D2, $43, $30, $2D, $30, $21, $30, $09, $06, $05, $2B, $0E, $03, $02, $1A, $05,
    $00, $04, $14, $E0, $D8, $41, $1F, $76, $85, $94, $B5, $64, $2D, $FD, $59, $27,
    $CE, $EA, $3B, $B1, $E2, $25, $11, $04, $08, $01, $3E, $2B, $1B, $94, $CF, $41,
    $11);

function PrivKeyCertPfx: RawByteString;
begin
  FastSetRawByteString(result, @PRIVKEY_PFX, SizeOf(PRIVKEY_PFX));
end;

procedure InitNetTlsContextSelfSignedServer(var TLS: TNetTlsContext;
  Algo: TCryptAsymAlgo);
var
  cert: ICryptCert;
  certfile, keyfile: TFileName;
  keypass: RawUtf8;
begin
  certfile := TemporaryFileName;
  if CryptCertAlgoOpenSsl[Algo] = nil then
  begin
    FileFromString(PrivKeyCertPfx, certfile); // use pre-computed key
    keypass := 'pass';
  end
  else
  begin
    keyfile := TemporaryFileName;
    keypass := CardinalToHexLower(Random32);
    cert := CryptCertAlgoOpenSsl[Algo].
              Generate(CU_TLS_SERVER, '127.0.0.1', nil, 3650);
    cert.SaveToFile(certfile, cccCertOnly, '', ccfPem);
    cert.SaveToFile(keyfile, cccPrivateKeyOnly, keypass, ccfPem);
    //writeln(BinToSource('PRIVKEY_PFX', '',
    //  cert.Save(cccCertWithPrivateKey, 'pass', ccfBinary)));
  end;
  InitNetTlsContext(TLS, {server=}true, certfile, keyfile, keypass);
end;


{ ******************** THttpServerSocket/THttpServer HTTP/1.1 Server }

{ THttpServerSocketGeneric }

constructor THttpServerSocketGeneric.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions);
begin
  fSockPort := aPort;
  fCompressGz := -1;
  SetServerKeepAliveTimeOut(KeepAliveTimeOut); // 30 seconds by default
  // event handlers set before inherited Create to be visible in childs
  fOnThreadStart := OnStart;
  SetOnTerminate(OnStop);
  fProcessName := ProcessName; // TSynThreadPoolTHttpServer needs it now
  inherited Create(OnStart, OnStop, ProcessName, ProcessOptions);
end;

function THttpServerSocketGeneric.GetApiVersion: RawUtf8;
begin
  result := SocketApiVersion;
end;

function THttpServerSocketGeneric.GetRegisterCompressGzStatic: boolean;
begin
  result := fCompressGz >= 0;
end;

procedure THttpServerSocketGeneric.SetRegisterCompressGzStatic(Value: boolean);
begin
  if Value then
    fCompressGz := CompressIndex(fCompress, @CompressGzip)
  else
    fCompressGz := -1;
end;

function THttpServerSocketGeneric.{%H-}WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUtf8; aWebSocketsAjax: boolean;
  aWebSocketsBinaryOptions: TWebSocketProtocolBinaryOptions): pointer;
begin
  raise EHttpServer.CreateUtf8('Unexpected %.WebSocketEnable: requires ' +
    'HTTP_BIDIR (useBidirSocket or useBidirAsync) kind of server', [self]);
end;

procedure THttpServerSocketGeneric.WaitStarted(Seconds: integer;
  const CertificateFile, PrivateKeyFile: TFileName;
  const PrivateKeyPassword: RawUtf8; const CACertificatesFile: TFileName);
var
  tls: TNetTlsContext;
begin
  InitNetTlsContext(tls, {server=}true,
    CertificateFile, PrivateKeyFile, PrivateKeyPassword, CACertificatesFile);
  WaitStarted(Seconds, @tls);
end;

procedure THttpServerSocketGeneric.WaitStarted(
  Seconds: integer; TLS: PNetTlsContext);
var
  tix: Int64;
begin
  tix := GetTickCount64 + Seconds * 1000; // never wait forever
  repeat
    if Terminated then
      exit;
    case GetExecuteState of
      esRunning:
        break;
      esFinished:
        raise EHttpServer.CreateUtf8('%.Execute aborted due to %',
          [self, fExecuteMessage]);
    end;
    Sleep(1); // warning: waits typically 1-15 ms on Windows
    if GetTickCount64 > tix then
      raise EHttpServer.CreateUtf8('%.WaitStarted timeout after % seconds [%]',
        [self, Seconds, fExecuteMessage]);
  until false;
  // now the server socket has been bound, and is ready to accept connections
  if (hsoEnableTls in fOptions) and
     (TLS <> nil) and
     (TLS^.CertificateFile <> '') and
     ((fSock = nil) or
      not fSock.TLS.Enabled) then
  begin
    if fSock = nil then
      Sleep(5); // paranoid on some servers which propagate the pointer
    if (fSock <> nil) and
       not fSock.TLS.Enabled then // call InitializeTlsAfterBind once
    begin
      fSock.TLS := TLS^;
      InitializeTlsAfterBind; // validate TLS certificate(s) now
      Sleep(1); // let some warmup happen
    end;
  end;
end;

function THttpServerSocketGeneric.GetStat(
  one: THttpServerSocketGetRequestResult): integer;
begin
  result := fStats[one];
end;

procedure THttpServerSocketGeneric.IncStat(
  one: THttpServerSocketGetRequestResult);
begin
  if not (hsoNoStats in fOptions) then
    LockedInc32(@fStats[one]);
end;

function THttpServerSocketGeneric.DoRequest(Ctxt: THttpServerRequest): boolean;
var
  cod: integer;
begin
  result := false; // error
  try
    Ctxt.RespStatus := DoBeforeRequest(Ctxt);
    if Ctxt.RespStatus > 0 then
    begin
      Ctxt.SetErrorMessage('Rejected % Request', [Ctxt.Url]);
      IncStat(grRejected);
    end
    else
    begin
      // execute the main processing callback
      Ctxt.RespStatus := Request(Ctxt);
      Ctxt.InContent := ''; // release memory ASAP
      cod := DoAfterRequest(Ctxt);
      if cod > 0 then
        Ctxt.RespStatus := cod;
    end;
    result := true; // success
  except
    on E: Exception do
      begin
        // intercept and return Internal Server Error 500
        Ctxt.RespStatus := HTTP_SERVERERROR;
        Ctxt.SetErrorMessage('%: %', [E, E.Message]);
        IncStat(grException);
        // will keep soClose as result to shutdown the connection
      end;
  end;
end;

procedure THttpServerSocketGeneric.SetServerKeepAliveTimeOut(Value: cardinal);
begin
  fServerKeepAliveTimeOut := Value;
  fServerKeepAliveTimeOutSec := Value div 1000;
end;

function THttpServerSocketGeneric.OnNginxAllowSend(
  Context: THttpServerRequestAbstract; const LocalFileName: TFileName): boolean;
var
  match, i, f: PtrInt;
  folderlefttrim: ^TFileName;
begin
  match := 0;
  folderlefttrim := pointer(fNginxSendFileFrom);
  if LocalFileName <> '' then
    for f := 1 to length(fNginxSendFileFrom) do
    begin
      match := length(folderlefttrim^);
      for i := 1 to match do // case sensitive left search
        if LocalFileName[i] <> folderlefttrim^[i] then
        begin
          match := 0;
          break;
        end;
      if match <> 0 then
        break; // found matching folderlefttrim
      inc(folderlefttrim);
    end;
  result := match <> 0;
  if not result then
    exit; // no match -> manual send
  Context.OutContent :=
    copy(Context.OutContent, match + 1, 1024); // remove '/var/www'
  Context.OutCustomHeaders := TrimU(Context.OutCustomHeaders + #13#10 +
    'X-Accel-Redirect: ' + Context.OutContent);
  Context.OutContent := '';
end;

procedure THttpServerSocketGeneric.NginxSendFileFrom(
  const FileNameLeftTrim: TFileName);
var
  n: PtrInt;
begin
  n := length(fNginxSendFileFrom);
  SetLength(fNginxSendFileFrom, n + 1);
  fNginxSendFileFrom[n] := FileNameLeftTrim;
  fOnSendFile := OnNginxAllowSend;
end;

procedure THttpServerSocketGeneric.InitializeTlsAfterBind;
begin
  if fSock.TLS.Enabled then
    exit;
  fSafe.Lock; // load certificates once from first connected thread
  try
    fSock.DoTlsAfter(cstaBind);  // validate certificates now
  finally
    fSafe.UnLock;
  end;
end;


{ THttpServer }

constructor THttpServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount: integer; KeepAliveTimeOut: integer;
  ProcessOptions: THttpServerOptions);
begin
  fInternalHttpServerRespList := TSynObjectListLocked.Create({ownobject=}false);
  if fThreadPool <> nil then
    fThreadPool.ContentionAbortDelay := 5000; // 5 seconds default
  if fThreadRespClass = nil then
    fThreadRespClass := THttpServerResp;
  if fSocketClass = nil then
    fSocketClass := THttpServerSocket;
  fServerSendBufferSize := 256 shl 10; // 256KB of buffers seems good enough
  inherited Create(aPort, OnStart, OnStop, ProcessName, ServerThreadPoolCount,
    KeepAliveTimeOut, ProcessOptions);
  if ServerThreadPoolCount > 0 then
  begin
    fThreadPool := TSynThreadPoolTHttpServer.Create(self, ServerThreadPoolCount);
    fHttpQueueLength := 1000;
  end;
end;

destructor THttpServer.Destroy;
var
  endtix: Int64;
  i: PtrInt;
  dummy: TNetSocket; // touch-and-go to the server to release main Accept()
begin
  Terminate; // set Terminated := true for THttpServerResp.Execute
  if fThreadPool <> nil then
    fThreadPool.fTerminated := true; // notify background process
  if (fExecuteState = esRunning) and
     (Sock <> nil) then
  begin
    if Sock.SocketLayer <> nlUnix then
      Sock.Close; // shutdown TCP/UDP socket to unlock Accept() in Execute
    if NewSocket(Sock.Server, Sock.Port, Sock.SocketLayer,
       {dobind=}false, 10, 10, 10, 0, dummy) = nrOK then
      // Windows TCP/UDP socket may not release Accept() until connected
      dummy.ShutdownAndClose({rdwr=}false);
    if Sock.SockIsDefined then
      Sock.Close; // nlUnix expects shutdown after accept() returned
  end;
  endtix := GetTickCount64 + 20000;
  try
    if fInternalHttpServerRespList <> nil then // HTTP/1.1 long running threads
    begin
      fInternalHttpServerRespList.Safe.ReadOnlyLock; // notify
      for i := 0 to fInternalHttpServerRespList.Count - 1 do
        THttpServerResp(fInternalHttpServerRespList.List[i]).Shutdown;
      fInternalHttpServerRespList.Safe.ReadOnlyUnLock;
      repeat
        // wait for all THttpServerResp.Execute to be finished
        fInternalHttpServerRespList.Safe.ReadOnlyLock;
        try
          if (fInternalHttpServerRespList.Count = 0) and
             (fExecuteState <> esRunning) then
            break;
        finally
          fInternalHttpServerRespList.Safe.ReadOnlyUnLock;
        end;
        SleepHiRes(10);
      until GetTickCount64 > endtix;
      FreeAndNilSafe(fInternalHttpServerRespList);
    end;
  finally
    FreeAndNilSafe(fThreadPool); // release all associated threads
    FreeAndNilSafe(fSock);
    inherited Destroy;       // direct Thread abort, no wait till ended
  end;
end;

function THttpServer.GetExecuteState: THttpServerExecuteState;
begin
  result := fExecuteState;
end;

function THttpServer.GetHttpQueueLength: cardinal;
begin
  result := fHttpQueueLength;
end;

procedure THttpServer.SetHttpQueueLength(aValue: cardinal);
begin
  fHttpQueueLength := aValue;
end;

{ $define MONOTHREAD}
// define this not to create a thread at every connection (not recommended)

procedure THttpServer.Execute;
var
  cltsock: TNetSocket;
  cltaddr: TNetAddr;
  cltservsock: THttpServerSocket;
  res: TNetResult;
  {$ifdef MONOTHREAD}
  endtix: Int64;
  {$endif MONOTHREAD}
begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  fExecuteState := esBinding;
  NotifyThreadStart(self);
  // main server process loop
  try
    fSock := TCrtSocket.Bind(fSockPort); // BIND + LISTEN (TLS is done later)
    fExecuteState := esRunning;
    if not fSock.SockIsDefined then // paranoid check
      raise EHttpServer.CreateUtf8('%.Execute: %.Bind failed', [self, fSock]);
    while not Terminated do
    begin
      res := Sock.Sock.Accept(cltsock, cltaddr, {async=}false);
      if not (res in [nrOK, nrRetry]) then
        if Terminated then
          break
        else
        begin
          SleepHiRes(1); // failure (too many clients?) -> wait and retry
          continue;
        end;
      if Terminated or
         (Sock = nil) then
      begin
        cltsock.ShutdownAndClose({rdwr=}true);
        break; // don't accept input if server is down
      end;
      OnConnect;
      {$ifdef MONOTHREAD}
      try
        cltservsock := fSocketClass.Create(self);
        try
          cltservsock.AcceptRequest(cltsock, @cltaddr);
          if hsoEnableTls in fOptions then
            cltservsock.DoTlsAfter(cstaAccept);
          endtix := fHeaderRetrieveAbortDelay;
          if endtix > 0 then
            inc(endtix, GetTickCount64);
          if cltservsock.GetRequest({withbody=}true, endtix)
              in [grBodyReceived, grHeaderReceived] then
            Process(cltservsock, 0, self);
          OnDisconnect;
        finally
          cltservsock.Free;
        end;
      except
        on E: Exception do
          // do not stop thread on TLS or socket error
          fSock.OnLog(sllTrace, 'Execute: % [%]', [E, E.Message], self);
      end;
      {$else}
      if Assigned(fThreadPool) then
      begin
        // use thread pool to process the request header, and probably its body
        cltservsock := fSocketClass.Create(self);
        // note: we tried to reuse the fSocketClass instance -> no perf benefit
        cltservsock.AcceptRequest(cltsock, @cltaddr);
        if not fThreadPool.Push(pointer(PtrUInt(cltservsock)),
            {waitoncontention=}true) then
        begin
          // was false if there is no idle thread in the pool, and queue is full
          cltservsock.Free; // will call DirectShutdown(cltsock)
        end;
      end
      else
        // default implementation creates one thread for each incoming socket
        fThreadRespClass.Create(cltsock, cltaddr, self);
      {$endif MONOTHREAD}
    end;
  except
    on E: Exception do
      // any exception would break and release the thread
      FormatUtf8('% [%]', [E, E.Message], fExecuteMessage);
  end;
  fSafe.Lock;
  fExecuteState := esFinished;
  fSafe.UnLock;
end;

procedure THttpServer.OnConnect;
begin
  LockedInc32(@fServerConnectionCount);
  LockedInc32(@fServerConnectionActive);
end;

procedure THttpServer.OnDisconnect;
begin
  LockedDec32(@fServerConnectionActive);
end;

procedure THttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var
  req: THttpServerRequest;
  output: PRawByteStringBuffer;
  dest: TRawByteStringBuffer;
begin
  if (ClientSock = nil) or
     (ClientSock.Http.Headers = '') or
     Terminated then
    // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  // compute the response
  req := THttpServerRequest.Create(self, ConnectionID, ConnectionThread,
    HTTPREMOTEFLAGS[ClientSock.TLS.Enabled], @ClientSock.fConnectionOpaque);
  try
    req.Prepare(ClientSock.Http, ClientSock.fRemoteIP);
    DoRequest(req);
    output := req.SetupResponse(
      ClientSock.Http, fCompressGz, fServerSendBufferSize);
  finally
    req.Free;
  end;
  // send back the response
  if Terminated then
    exit;
  if hfConnectionClose in ClientSock.Http.HeaderFlags then
    ClientSock.fKeepAliveClient := false;
  if ClientSock.TrySndLow(output.Buffer, output.Len) then // header[+body]
    while not Terminated do
    begin
      case ClientSock.Http.State of
        hrsResponseDone:
          break; // finished
        hrsSendBody:
          begin
            dest.Clear; // body is retrieved from Content/ContentStream
            ClientSock.Http.ProcessBody(dest, fServerSendBufferSize);
            if ClientSock.TrySndLow(dest.Buffer, dest.Len) then
              continue; // send body by fServerSendBufferSize chunks
          end;
      end;
      ClientSock.fKeepAliveClient := false; // force socket close on error
      break;
    end
  else
    ClientSock.fKeepAliveClient := false;
  ClientSock.Http.ProcessDone;   // ContentStream.Free
  // add transfert stats to main socket
  if Sock <> nil then
  begin
    fSafe.Lock;
    Sock.BytesIn := Sock.BytesIn + ClientSock.BytesIn;
    Sock.BytesOut := Sock.BytesOut + ClientSock.BytesOut;
    fSafe.UnLock;
    ClientSock.fBytesIn := 0;
    ClientSock.fBytesOut := 0;
  end;
end;


{ THttpServerSocket }

procedure THttpServerSocket.TaskProcess(aCaller: TSynThreadPoolWorkThread);
var
  freeme: boolean;
  headertix: Int64;
begin
  // process this THttpServerSocket in the thread pool
  freeme := true;
  try
    if hsoEnableTls in fServer.Options then
      DoTlsAfter(cstaAccept); // slow TLS handshake done in this sub-thread
    headertix := fServer.HeaderRetrieveAbortDelay;
    if headertix > 0 then
      headertix := headertix + GetTickCount64;
    freeme := TaskProcessBody(aCaller, GetRequest({withbody=}false, headertix));
  finally
    if freeme then
      Free;
  end;
end;

function THttpServerSocket.TaskProcessBody(aCaller: TSynThreadPoolWorkThread;
  aHeaderResult: THttpServerSocketGetRequestResult): boolean;
var
  pool: TSynThreadPoolTHttpServer;
begin
  result := true;
  if (fServer = nil) or
     fServer.Terminated  then
    exit;
  // properly get the incoming body and process the request
  fServer.IncStat(aHeaderResult);
  case aHeaderResult of
    grHeaderReceived:
      begin
        pool := TSynThreadPoolTHttpServer(aCaller.Owner);
        // connection and header seem valid -> process request further
        if (fServer.ServerKeepAliveTimeOut > 0) and
           (fServer.fInternalHttpServerRespList.Count < pool.MaxBodyThreadCount) and
           (KeepAliveClient or
            (Http.ContentLength > pool.BigBodySize)) then
        begin
          // HTTP/1.1 Keep Alive (including WebSockets) or posted data > 16 MB
          // -> process in dedicated background thread
          fServer.fThreadRespClass.Create(self, fServer);
          result := false; // THttpServerResp will own and free srvsock
        end
        else
        begin
          // no Keep Alive = multi-connection -> process in the Thread Pool
          if not (hfConnectionUpgrade in Http.HeaderFlags) and
             not HttpMethodWithNoBody(Method) then
          begin
            GetBody; // we need to get it now
            fServer.IncStat(grBodyReceived);
          end;
          // multi-connection -> process now
          fServer.Process(self, RemoteConnectionID, aCaller);
          fServer.OnDisconnect;
          // no Shutdown here: will be done client-side
        end;
      end;
  else if Assigned(fServer.Sock.OnLog) then
    fServer.Sock.OnLog(sllTrace, 'Task: close after GetRequest=% from %',
        [ToText(aHeaderResult)^, RemoteIP], self);
  end;
end;

constructor THttpServerSocket.Create(aServer: THttpServer);
begin
  inherited Create(5000);
  if aServer <> nil then // nil e.g. from TRtspOverHttpServer
  begin
    fServer := aServer;
    Http.Compress := aServer.fCompress;
    Http.CompressAcceptEncoding := aServer.fCompressAcceptEncoding;
    fSocketLayer := aServer.Sock.SocketLayer;
    if hsoEnableTls in aServer.fOptions then
    begin
      if not aServer.fSock.TLS.Enabled then // if not already in WaitStarted()
        aServer.InitializeTlsAfterBind;     // load certificate(s) once
      TLS.AcceptCert := aServer.Sock.TLS.AcceptCert; // TaskProcess cstaAccept
    end;
    OnLog := aServer.Sock.OnLog;
  end;
end;

function THttpServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
var
  P: PUtf8Char;
  status: cardinal;
  pending: integer;
  reason: RawUtf8;
  noheaderfilter: boolean;
begin
  result := grError;
  try
    // use SockIn with 1KB buffer if not already initialized: 2x faster
    CreateSockIn;
    // abort now with no exception if socket is obviously broken
    if fServer <> nil then
    begin
      pending := SockInPending(100, {alsosocket=}true);
      if (pending < 0) or
         (fServer = nil) or
         fServer.Terminated then
        exit;
      noheaderfilter := hsoHeadersUnfiltered in fServer.Options;
    end
    else
      noheaderfilter := false;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Http.CommandResp);
    P := pointer(Http.CommandResp);
    if P = nil then
      exit; // broken
    GetNextItem(P, ' ', Http.CommandMethod); // 'GET'
    GetNextItem(P, ' ', Http.CommandUri);    // '/path'
    fKeepAliveClient := ((fServer = nil) or
                         (fServer.ServerKeepAliveTimeOut > 0)) and
                        IdemPChar(P, 'HTTP/1.1');
    Http.Content := '';
    // get headers
    GetHeader(noheaderfilter);
    fServer.ParseRemoteIPConnID(Http.Headers, fRemoteIP, fRemoteConnectionID);
    if hfConnectionClose in Http.HeaderFlags then
      fKeepAliveClient := false;
    if (Http.ContentLength < 0) and
       (KeepAliveClient or
       (Http.CommandMethod = 'GET')) then
      Http.ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    if (headerMaxTix > 0) and
       (GetTickCount64 > headerMaxTix) then
    begin
      result := grTimeout;
      exit; // allow 10 sec for header -> DOS/TCPSYN Flood
    end;
    if fServer <> nil then
    begin
      // validate allowed PayLoad size and OnBeforeBody callback
      if (Http.ContentLength > 0) and
         (fServer.MaximumAllowedContentLength > 0) and
         (Http.ContentLength > fServer.MaximumAllowedContentLength) then
      begin
        SockSendFlush('HTTP/1.0 413 Payload Too Large'#13#10#13#10'Rejected');
        result := grOversizedPayload;
        exit;
      end;
      if Assigned(fServer.OnBeforeBody) then
      begin
        HeadersPrepare(fRemoteIP); // will include remote IP to Http.Headers
        status := fServer.OnBeforeBody(Http.CommandUri, Http.CommandMethod,
          Http.Headers, Http.ContentType, fRemoteIP, Http.BearerToken,
          Http.ContentLength, HTTPREMOTEFLAGS[TLS.Enabled]);
        {$ifdef SYNCRTDEBUGLOW}
        TSynLog.Add.Log(sllCustom2,
          'GetRequest sock=% OnBeforeBody=% Command=% Headers=%', [fSock, status,
          LogEscapeFull(Command), LogEscapeFull(allheaders)], self);
        {$endif SYNCRTDEBUGLOW}
        if status <> HTTP_SUCCESS then
        begin
          StatusCodeToReason(status, reason);
          if Assigned(OnLog) then
            OnLog(sllTrace, 'GetRequest: rejected by OnBeforeBody=% %',
              [status, reason], self);
          SockSend(['HTTP/1.0 ', status, ' ', reason, #13#10#13#10,
            reason, ' ', status]);
          SockSendFlush('');
          result := grRejected;
          exit;
        end;
      end;
    end;
    // implement 'Expect: 100-Continue' Header
    if hfExpect100 in Http.HeaderFlags then
      // client waits for the server to parse the headers and return 100
      // before sending the request body
      SockSendFlush('HTTP/1.1 100 Continue'#13#10#13#10);
    // now the server could retrieve the request body
    if withBody and
       not (hfConnectionUpgrade in Http.HeaderFlags) then
    begin
      if not HttpMethodWithNoBody(Http.CommandMethod) then
        GetBody;
      result := grBodyReceived;
    end
    else
      result := grHeaderReceived;
  except
    on E: Exception do
      result := grException;
  end;
end;


{ THttpServerResp }

constructor THttpServerResp.Create(aSock: TNetSocket; const aSin: TNetAddr;
  aServer: THttpServer);
var
  c: THttpServerSocketClass;
begin
  fClientSock := aSock;
  fClientSin := aSin;
  if aServer = nil then
    c := THttpServerSocket
  else
    c := aServer.fSocketClass;
  Create(c.Create(aServer), aServer); // on Linux, Execute raises during Create
end;

constructor THttpServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  fServer := aServer;
  fServerSock := aServerSock;
  fOnThreadTerminate := fServer.fOnThreadTerminate;
  fServer.fInternalHttpServerRespList.Add(self);
  fConnectionID := aServerSock.fRemoteConnectionID;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure THttpServerResp.Shutdown;
begin
  Terminate;
  if fServerSock <> nil then
    fServerSock.Close;
end;

procedure THttpServerResp.Execute;

  procedure HandleRequestsProcess;
  var
    keepaliveendtix, beforetix, headertix, tix: Int64;
    pending: TCrtSocketPending;
    res: THttpServerSocketGetRequestResult;
  begin
    {$ifdef SYNCRTDEBUGLOW}
    try
    {$endif SYNCRTDEBUGLOW}
    try
      repeat
        beforetix := GetTickCount64;
        keepaliveendtix := beforetix + fServer.ServerKeepAliveTimeOut;
        repeat
          // within this loop, break=wait for next command, exit=quit
          if (fServer = nil) or
             fServer.Terminated or
             (fServerSock = nil) then
            // server is down -> close connection
            exit;
          pending := fServerSock.SockReceivePending(50); // 50 ms timeout
          if (fServer = nil) or
             fServer.Terminated then
            // server is down -> disconnect the client
            exit;
          {$ifdef SYNCRTDEBUGLOW}
          TSynLog.Add.Log(sllCustom2, 'HandleRequestsProcess: sock=% pending=%',
            [fServerSock.fSock, _CSP[pending]], self);
          {$endif SYNCRTDEBUGLOW}
          case pending of
            cspSocketError:
              begin
                if Assigned(fServer.Sock.OnLog) then
                  fServer.Sock.OnLog(sllTrace, 'Execute: Socket error from %',
                    [fServerSock.RemoteIP], self);
                exit; // disconnect the client
              end;
            cspNoData:
              begin
                tix := GetTickCount64;
                if tix >= keepaliveendtix then
                begin
                  if Assigned(fServer.Sock.OnLog) then
                    fServer.Sock.OnLog(sllTrace, 'Execute: % KeepAlive=% timeout',
                      [fServerSock.RemoteIP, keepaliveendtix - tix], self);
                  exit; // reached keep alive time out -> close connection
                end;
                if tix - beforetix < 40 then
                begin
                  {$ifdef SYNCRTDEBUGLOW}
                  // getsockopt(fServerSock.fSock,SOL_SOCKET,SO_ERROR,@error,errorlen) returns 0 :(
                  TSynLog.Add.Log(sllCustom2,
                    'HandleRequestsProcess: sock=% LOWDELAY=%',
                    [fServerSock.fSock, tix - beforetix], self);
                  {$endif SYNCRTDEBUGLOW}
                  SleepHiRes(1); // seen only on Windows in practice
                  if (fServer = nil) or
                     fServer.Terminated then
                    // server is down -> disconnect the client
                    exit;
                end;
                beforetix := tix;
              end;
            cspDataAvailable:
              begin
                // get request and headers
                headertix := fServer.HeaderRetrieveAbortDelay;
                if headertix > 0 then
                  inc(headertix, beforetix);
                res := fServerSock.GetRequest({withbody=}true, headertix);
                if (fServer = nil) or
                   fServer.Terminated then
                  // server is down -> disconnect the client
                  exit;
                fServer.IncStat(res);
                case res of
                  grBodyReceived,
                  grHeaderReceived:
                    begin
                      if res = grBodyReceived then
                        fServer.IncStat(grHeaderReceived);
                      // calc answer and send response
                      fServer.Process(fServerSock, ConnectionID, self);
                      // keep connection only if necessary
                      if fServerSock.KeepAliveClient then
                        break
                      else
                        exit;
                    end;
                else
                  begin
                    if Assigned(fServer.Sock.OnLog) then
                      fServer.Sock.OnLog(sllTrace,
                        'Execute: close after GetRequest=% from %',
                        [ToText(res)^, fServerSock.RemoteIP], self);
                    exit;
                  end;
                end;
              end;
          end;
        until false;
      until false;
    except
      on E: Exception do
        ; // any exception will silently disconnect the client
    end;
    {$ifdef SYNCRTDEBUGLOW}
    finally
      TSynLog.Add.Log(sllCustom2, 'HandleRequestsProcess: close sock=%',
        [fServerSock.fSock], self);
    end;
    {$endif SYNCRTDEBUGLOW}
  end;

var
  netsock: TNetSocket;
begin
  fServer.NotifyThreadStart(self);
  try
    try
      if fClientSock.Socket <> 0 then
      begin
        // direct call from incoming socket
        netsock := fClientSock;
        fClientSock := nil; // fServerSock owns fClientSock
        fServerSock.AcceptRequest(netsock, @fClientSin);
        if fServer <> nil then
          HandleRequestsProcess;
      end
      else
      begin
        // call from TSynThreadPoolTHttpServer -> handle first request
        if not fServerSock.fBodyRetrieved and
           not HttpMethodWithNoBody(fServerSock.Http.CommandMethod) then
          fServerSock.GetBody;
        fServer.Process(fServerSock, ConnectionID, self);
        if (fServer <> nil) and
           fServerSock.KeepAliveClient then
          HandleRequestsProcess; // process further kept alive requests
      end;
    finally
      try
        if fServer <> nil then
          try
            fServer.OnDisconnect;
            if Assigned(fOnThreadTerminate) then
              fOnThreadTerminate(self);
          finally
            fServer.fInternalHttpServerRespList.Remove(self);
            fServer := nil;
            fOnThreadTerminate := nil;
          end;
      finally
        FreeAndNilSafe(fServerSock);
        // if Destroy happens before fServerSock.GetRequest() in Execute below
        fClientSock.ShutdownAndClose({rdwr=}false);
      end;
    end;
  except
    on Exception do
      ; // just ignore unexpected exceptions here, especially during clean-up
  end;
end;


{ TSynThreadPoolTHttpServer }

constructor TSynThreadPoolTHttpServer.Create(Server: THttpServer;
  NumberOfThreads: integer);
begin
  fServer := Server;
  fOnThreadTerminate := fServer.fOnThreadTerminate;
  fBigBodySize := THREADPOOL_BIGBODYSIZE;
  fMaxBodyThreadCount := THREADPOOL_MAXWORKTHREADS;
  inherited Create(NumberOfThreads,
    {$ifdef USE_WINIOCP} INVALID_HANDLE_VALUE {$else} {queuepending=}true{$endif},
    Server.ProcessName);
end;

{$ifndef USE_WINIOCP}
function TSynThreadPoolTHttpServer.QueueLength: integer;
begin
  if fServer = nil then
    result := 10000
  else
    result := fServer.fHttpQueueLength;
end;
{$endif USE_WINIOCP}

procedure TSynThreadPoolTHttpServer.Task(
  aCaller: TSynThreadPoolWorkThread; aContext: Pointer);
begin
  // process this THttpServerSocket in the thread pool
  if (fServer = nil) or
     fServer.Terminated then
    THttpServerSocket(aContext).Free
  else
    THttpServerSocket(aContext).TaskProcess(aCaller);
end;

procedure TSynThreadPoolTHttpServer.TaskAbort(aContext: Pointer);
begin
  THttpServerSocket(aContext).Free;
end;


function ToText(res: THttpServerSocketGetRequestResult): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpServerSocketGetRequestResult), ord(res));
end;


{$ifdef USEWININET}

{ **************** THttpApiServer HTTP/1.1 Server Over Windows http.sys Module }

{ THttpApiServer }

function THttpApiServer.AddUrl(const aRoot, aPort: RawUtf8; Https: boolean;
  const aDomainName: RawUtf8; aRegisterUri: boolean; aContext: Int64): integer;
var
  uri: SynUnicode;
  n: integer;
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
  if Http.Version.MajorVersion > 1 then
    result := Http.AddUrlToUrlGroup(fUrlGroupID, pointer(uri), aContext)
  else
    result := Http.AddUrl(fReqQueue, pointer(uri));
  if result = NO_ERROR then
  begin
    n := length(fRegisteredUnicodeUrl);
    SetLength(fRegisteredUnicodeUrl, n + 1);
    fRegisteredUnicodeUrl[n] := uri;
  end;
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
      if Http.Version.MajorVersion > 1 then
        result := Http.RemoveUrlFromUrlGroup(fUrlGroupID, pointer(uri), 0)
      else
        result := Http.RemoveUrl(fReqQueue, pointer(uri));
      if result <> 0 then
        exit; // shall be handled by caller
      for j := i to n - 1 do
        fRegisteredUnicodeUrl[j] := fRegisteredUnicodeUrl[j + 1];
      SetLength(fRegisteredUnicodeUrl, n);
      exit;
    end;
end;

class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: RawUtf8;
  Https: boolean; const aDomainName: RawUtf8; OnlyDelete: boolean): string;
const
  /// will allow AddUrl() registration to everyone
  // - 'GA' (GENERIC_ALL) to grant all access
  // - 'S-1-1-0'	defines a group that includes all users
  HTTPADDURLSECDESC: PWideChar = 'D:(A;;GA;;;S-1-1-0)';
var
  prefix: SynUnicode;
  err: HRESULT;
  cfg: HTTP_SERVICE_CONFIG_URLACL_SET;
begin
  try
    HttpApiInitialize;
    prefix := RegURL(aRoot, aPort, Https, aDomainName);
    if prefix = '' then
      result := 'Invalid parameters'
    else
    begin
      EHttpApiServer.RaiseOnError(hInitialize,
        Http.Initialize(Http.Version, HTTP_INITIALIZE_CONFIG));
      try
        FillcharFast(cfg, SizeOf(cfg), 0);
        cfg.KeyDesc.pUrlPrefix := pointer(prefix);
        // first delete any existing information
        err := Http.DeleteServiceConfiguration(
          0, hscUrlAclInfo, @cfg, SizeOf(cfg));
        // then add authorization rule
        if not OnlyDelete then
        begin
          cfg.KeyDesc.pUrlPrefix := pointer(prefix);
          cfg.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
          err := Http.SetServiceConfiguration(
            0, hscUrlAclInfo, @cfg, SizeOf(cfg));
        end;
        if (err <> NO_ERROR) and
           (err <> ERROR_ALREADY_EXISTS) then
          raise EHttpApiServer.Create(hSetServiceConfiguration, err);
        result := ''; // success
      finally
        Http.Terminate(HTTP_INITIALIZE_CONFIG);
      end;
    end;
  except
    on E: Exception do
      result := E.Message;
  end;
end;

type
  THttpApiServerClass = class of THttpApiServer;

procedure THttpApiServer.Clone(ChildThreadCount: integer);
var
  i: PtrInt;
begin
  if (fReqQueue = 0) or
     (not Assigned(OnRequest)) or
     (ChildThreadCount <= 0) or
     (fClones <> nil) then
    exit; // nothing to clone (need a queue and a process event)
  if ChildThreadCount > 256 then
    ChildThreadCount := 256; // not worth adding
  SetLength(fClones, ChildThreadCount);
  for i := 0 to ChildThreadCount - 1 do
    fClones[i] := THttpApiServerClass(Self.ClassType).CreateClone(self);
end;

function THttpApiServer.GetApiVersion: RawUtf8;
begin
  FormatUtf8('HTTP API %.%',
    [Http.Version.MajorVersion, Http.Version.MinorVersion], result);
end;

constructor THttpApiServer.Create(QueueName: SynUnicode;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ProcessOptions: THttpServerOptions);
var
  binding: HTTP_BINDING_INFO;
begin
  SetLength(fLogDataStorage, SizeOf(HTTP_LOG_FIELDS_DATA)); // should be done 1st
  inherited Create(OnStart, OnStop, ProcessName, ProcessOptions + [hsoCreateSuspended]);
  fOptions := ProcessOptions;
  HttpApiInitialize; // will raise an exception in case of failure
  EHttpApiServer.RaiseOnError(hInitialize,
    Http.Initialize(Http.Version, HTTP_INITIALIZE_SERVER));
  if Http.Version.MajorVersion > 1 then
  begin
    EHttpApiServer.RaiseOnError(hCreateServerSession,
      Http.CreateServerSession(Http.Version, fServerSessionID));
    EHttpApiServer.RaiseOnError(hCreateUrlGroup,
      Http.CreateUrlGroup(fServerSessionID, fUrlGroupID));
    if QueueName = '' then
      Utf8ToSynUnicode(Int64ToUtf8(fServerSessionID), QueueName);
    EHttpApiServer.RaiseOnError(hCreateRequestQueue,
      Http.CreateRequestQueue(Http.Version, pointer(QueueName), nil, 0, fReqQueue));
    binding.Flags := 1;
    binding.RequestQueueHandle := fReqQueue;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerBindingProperty,
        @binding, SizeOf(binding)));
  end
  else
    EHttpApiServer.RaiseOnError(hCreateHttpHandle,
      Http.CreateHttpHandle(fReqQueue));
  fReceiveBufferSize := 1 shl 20; // i.e. 1 MB
  if Suspended then
    Suspended := False;
end;

constructor THttpApiServer.CreateClone(From: THttpApiServer);
begin
  SetLength(fLogDataStorage, SizeOf(HTTP_LOG_FIELDS_DATA));
  fOwner := From;
  fReqQueue := From.fReqQueue;
  fOnRequest := From.fOnRequest;
  fOnBeforeBody := From.fOnBeforeBody;
  fOnBeforeRequest := From.fOnBeforeRequest;
  fOnAfterRequest := From.fOnAfterRequest;
  fCallbackSendDelay := From.fCallbackSendDelay;
  fCompress := From.fCompress;
  fCompressAcceptEncoding := From.fCompressAcceptEncoding;
  fReceiveBufferSize := From.fReceiveBufferSize;
  if From.fLogData <> nil then
    fLogData := pointer(fLogDataStorage);
  SetServerName(From.fServerName);
  SetRemoteIPHeader(From.RemoteIPHeader);
  SetRemoteConnIDHeader(From.RemoteConnIDHeader);
  fLoggingServiceName := From.fLoggingServiceName;
  inherited Create(From.fOnThreadStart, From.fOnThreadTerminate,
   From.ProcessName, From.Options - [hsoCreateSuspended]);
end;

procedure THttpApiServer.DestroyMainThread;
var
  i: PtrInt;
begin
  if fReqQueue <> 0 then
  begin
    for i := 0 to length(fClones) - 1 do
      fClones[i].Terminate; // for CloseHandle() below to finish Execute
    if Http.Version.MajorVersion > 1 then
    begin
      if fUrlGroupID <> 0 then
      begin
        Http.RemoveUrlFromUrlGroup(fUrlGroupID, nil, HTTP_URL_FLAG_REMOVE_ALL);
        Http.CloseUrlGroup(fUrlGroupID);
        fUrlGroupID := 0;
      end;
      CloseHandle(fReqQueue);
      if fServerSessionID <> 0 then
      begin
        Http.CloseServerSession(fServerSessionID);
        fServerSessionID := 0;
      end;
    end
    else
    begin
      for i := 0 to high(fRegisteredUnicodeUrl) do
        Http.RemoveUrl(fReqQueue, pointer(fRegisteredUnicodeUrl[i]));
      CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
    end;
    fReqQueue := 0;
    {$ifdef FPC}
    for i := 0 to length(fClones) - 1 do
      WaitForSingleObject(fClones[i].Handle, 30000); // sometimes needed on FPC
    {$endif FPC}
    for i := 0 to length(fClones) - 1 do
      fClones[i].Free;
    fClones := nil;
    Http.Terminate(HTTP_INITIALIZE_SERVER);
  end;
end;

destructor THttpApiServer.Destroy;
begin
  Terminate; // for Execute to be notified about end of process
  try
    if (fOwner = nil) and
       (Http.Module <> 0) then // fOwner<>nil for cloned threads
      DestroyMainThread;
    {$ifdef FPC}
    WaitForSingleObject(Handle, 30000); // sometimes needed on FPC
    {$endif FPC}
  finally
    inherited Destroy;
  end;
end;

function THttpApiServer.GetSendResponseFlags(Ctxt: THttpServerRequest): integer;
begin
  result := 0;
end;

// returned P^ points to the first non digit char - not as GetNextItemQWord()
function GetNextNumber(var P: PUtf8Char): Qword;
var
  c: PtrUInt;
begin
  result := 0;
  if P <> nil then
    repeat
      c := byte(P^) - 48;
      if c > 9 then
        break
      else
        result := result * 10 + Qword(c);
      inc(P);
    until false;
end;

type
  TVerbText = array[hvOPTIONS..pred(hvMaximum)] of RawUtf8;

const
  VERB_TEXT: TVerbText = (
    'OPTIONS',
    'GET',
    'HEAD',
    'POST',
    'PUT',
    'DELETE',
    'TRACE',
    'CONNECT',
    'TRACK',
    'MOVE',
    'COPY',
    'PROPFIND',
    'PROPPATCH',
    'MKCOL',
    'LOCK',
    'UNLOCK',
    'SEARCH');

var
  global_verbs: TVerbText; // to avoid memory allocation on Delphi

procedure THttpApiServer.Execute;
var
  req: PHTTP_REQUEST;
  reqid: HTTP_REQUEST_ID;
  reqbuf, respbuf: RawByteString;
  i: PtrInt;
  bytesread, bytessent, flags: cardinal;
  err: HRESULT;
  compressset: THttpSocketCompressSet;
  incontlen: Qword;
  incontlenchunk, incontlenread: cardinal;
  incontenc, inaccept, range: RawUtf8;
  outcontenc, outstat: RawUtf8;
  outstatcode, afterstatcode: cardinal;
  respsent: boolean;
  ctxt: THttpServerRequest;
  filehandle: THandle;
  reps: PHTTP_RESPONSE;
  bufread, V: PUtf8Char;
  heads: HTTP_UNKNOWN_HEADERs;
  rangestart, rangelen: ULONGLONG;
  outcontlen: ULARGE_INTEGER;
  datachunkmem: HTTP_DATA_CHUNK_INMEMORY;
  datachunkfile: HTTP_DATA_CHUNK_FILEHANDLE;
  logdata: PHTTP_LOG_FIELDS_DATA;
  contrange: ShortString;

  procedure SendError(StatusCode: cardinal; const ErrorMsg: string; E: Exception = nil);
  var
    msg: RawUtf8;
  begin
    try
      reps^.SetStatus(StatusCode, outstat);
      logdata^.ProtocolStatus := StatusCode;
      FormatUtf8('<html><body style="font-family:verdana;"><h1>Server Error %: %</h1><p>',
        [StatusCode, outstat], msg);
      if E <> nil then
        msg := FormatUtf8('%% Exception raised:<br>', [msg, E]);
      msg := msg + HtmlEscapeString(ErrorMsg) + ('</p><p><small>' + XPOWEREDVALUE);
      reps^.SetContent(datachunkmem, msg, 'text/html; charset=utf-8');
      Http.SendHttpResponse(fReqQueue, req^.RequestId, 0, reps^, nil,
        bytessent, nil, 0, nil, fLogData);
    except
      on Exception do
        ; // ignore any HttpApi level errors here (client may crashed)
    end;
  end;

  function SendResponse: boolean;
  var
    R: PUtf8Char;
    flags: cardinal;
  begin
    result := not Terminated; // true=success
    if not result then
      exit;
    respsent := true;
    reps^.SetStatus(outstatcode, outstat);
    if Terminated then
      exit;
    // update log information
    if Http.Version.MajorVersion >= 2 then
      with req^, logdata^ do
      begin
        MethodNum := Verb;
        UriStemLength := CookedUrl.AbsPathLength;
        UriStem := CookedUrl.pAbsPath;
        with headers.KnownHeaders[reqUserAgent] do
        begin
          UserAgentLength := RawValueLength;
          UserAgent := pRawValue;
        end;
        with headers.KnownHeaders[reqHost] do
        begin
          HostLength := RawValueLength;
          Host := pRawValue;
        end;
        with headers.KnownHeaders[reqReferrer] do
        begin
          ReferrerLength := RawValueLength;
          Referrer := pRawValue;
        end;
        ProtocolStatus := reps^.StatusCode;
        ClientIp := pointer(ctxt.fRemoteIP);
        ClientIpLength := length(ctxt.fRemoteip);
        Method := pointer(ctxt.fMethod);
        MethodLength := length(ctxt.fMethod);
        UserName := pointer(ctxt.fAuthenticatedUser);
        UserNameLength := Length(ctxt.fAuthenticatedUser);
      end;
    // send response
    reps^.Version := req^.Version;
    reps^.SetHeaders(pointer(ctxt.OutCustomHeaders),
      heads, hsoNoXPoweredHeader in fOptions);
    if fCompressAcceptEncoding <> '' then
      reps^.AddCustomHeader(pointer(fCompressAcceptEncoding), heads, false);
    with reps^.headers.KnownHeaders[respServer] do
    begin
      pRawValue := pointer(fServerName);
      RawValueLength := length(fServerName);
    end;
    if ctxt.OutContentType = STATICFILE_CONTENT_TYPE then
    begin
      // response is file -> OutContent is UTF-8 file name to be served
      filehandle := FileOpen(Utf8ToString(ctxt.OutContent),
        fmOpenRead or fmShareDenyNone);
      if not ValidHandle(filehandle)  then
      begin
        SendError(HTTP_NOTFOUND, SysErrorMessage(GetLastError));
        result := false; // notify fatal error
      end;
      try // http.sys will serve then close the file from kernel
        datachunkfile.DataChunkType := hctFromFileHandle;
        datachunkfile.filehandle := filehandle;
        flags := 0;
        datachunkfile.ByteRange.StartingOffset.QuadPart := 0;
        Int64(datachunkfile.ByteRange.Length.QuadPart) := -1; // to eof
        with req^.headers.KnownHeaders[reqRange] do
        begin
          if (RawValueLength > 6) and
             IdemPChar(pointer(pRawValue), 'BYTES=') and
             (pRawValue[6] in ['0'..'9']) then
          begin
            FastSetString(range, pRawValue + 6, RawValueLength - 6); // need #0 end
            R := pointer(range);
            rangestart := GetNextNumber(R);
            if R^ = '-' then
            begin
              outcontlen.QuadPart := FileSize(filehandle);
              datachunkfile.ByteRange.Length.QuadPart :=
                outcontlen.QuadPart - rangestart;
              inc(R);
              flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
              datachunkfile.ByteRange.StartingOffset.QuadPart := rangestart;
              if R^ in ['0'..'9'] then
              begin
                rangelen := GetNextNumber(R) - rangestart + 1;
                if rangelen < datachunkfile.ByteRange.Length.QuadPart then
                  // "bytes=0-499" -> start=0, len=500
                  datachunkfile.ByteRange.Length.QuadPart := rangelen;
              end; // "bytes=1000-" -> start=1000, to eof)
              FormatShort('Content-range: bytes %-%/%'#0, [rangestart,
                rangestart + datachunkfile.ByteRange.Length.QuadPart - 1,
                outcontlen.QuadPart], contrange);
              reps^.AddCustomHeader(@contrange[1], heads, false);
              reps^.SetStatus(HTTP_PARTIALCONTENT, outstat);
            end;
          end;
          with reps^.headers.KnownHeaders[respAcceptRanges] do
          begin
            pRawValue := 'bytes';
            RawValueLength := 5;
          end;
        end;
        reps^.EntityChunkCount := 1;
        reps^.pEntityChunks := @datachunkfile;
        Http.SendHttpResponse(fReqQueue, req^.RequestId, flags, reps^, nil,
          bytessent, nil, 0, nil, fLogData);
      finally
        FileClose(filehandle);
      end;
    end
    else
    begin
      // response is in OutContent -> send it from memory
      if ctxt.OutContentType = NORESPONSE_CONTENT_TYPE then
        ctxt.OutContentType := ''; // true HTTP always expects a response
      if fCompress <> nil then
      begin
        with reps^.headers.KnownHeaders[reqContentEncoding] do
          if RawValueLength = 0 then
          begin
            // no previous encoding -> try if any compression
            CompressContent(compressset, fCompress, ctxt.OutContentType,
              ctxt.fOutContent, outcontenc);
            pRawValue := pointer(outcontenc);
            RawValueLength := length(outcontenc);
          end;
      end;
      reps^.SetContent(datachunkmem, ctxt.OutContent, ctxt.OutContentType);
      flags := GetSendResponseFlags(ctxt);
      EHttpApiServer.RaiseOnError(hSendHttpResponse,
        Http.SendHttpResponse(fReqQueue, req^.RequestId, flags, reps^, nil,
          bytessent, nil, 0, nil, fLogData));
    end;
  end;

begin
  if Terminated then
    exit;
  ctxt := nil;
  try
    // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
    NotifyThreadStart(self);
    // reserve working buffers
    SetLength(heads, 64);
    SetLength(respbuf, SizeOf(HTTP_RESPONSE));
    reps := pointer(respbuf);
    SetLength(reqbuf, 16384 + SizeOf(HTTP_REQUEST)); // req^ + 16 KB of headers
    req := pointer(reqbuf);
    logdata := pointer(fLogDataStorage);
    if global_verbs[hvOPTIONS] = '' then
      global_verbs := VERB_TEXT;
    ctxt := THttpServerRequest.Create(self, 0, self, [], nil);
    // main loop reusing a single ctxt instance for this thread
    reqid := 0;
    ctxt.fServer := self;
    repeat
      // release input/output body buffers ASAP
      ctxt.fInContent := '';
      ctxt.fOutContent := '';
      ctxt.fOutContentType := '';
      ctxt.fOutCustomHeaders := '';
      // reset authentication status & user between requests
      ctxt.fAuthenticationStatus := hraNone;
      ctxt.fAuthenticatedUser := '';
      ctxt.fAuthBearer := '';
      byte(ctxt.fConnectionFlags) := 0;
      // retrieve next pending request, and read its headers
      FillcharFast(req^, SizeOf(HTTP_REQUEST), 0);
      err := Http.ReceiveHttpRequest(fReqQueue, reqid, 0,
        req^, length(reqbuf), bytesread);
      if Terminated then
        break;
      case err of
        NO_ERROR:
          try
            // parse method and main headers as ctxt.Prepare() does
            ctxt.fHttpApiRequest := req;
            FastSetString(ctxt.fUrl, req^.pRawUrl, req^.RawUrlLength);
            if req^.Verb in [low(global_verbs)..high(global_verbs)] then
              ctxt.fMethod := global_verbs[req^.Verb]
            else
              FastSetString(ctxt.fMethod, req^.pUnknownVerb, req^.UnknownVerbLength);
            with req^.headers.KnownHeaders[reqContentType] do
              FastSetString(ctxt.fInContentType, pRawValue, RawValueLength);
            with req^.headers.KnownHeaders[reqUserAgent] do
              FastSetString(ctxt.fUserAgent, pRawValue, RawValueLength);
            with req^.Headers.KnownHeaders[reqAuthorization] do
              if (RawValueLength > 7) and
                 IdemPChar(pointer(pRawValue), 'BEARER ') then
                FastSetString(ctxt.fAuthBearer, pRawValue + 7, RawValueLength - 7);
            with req^.headers.KnownHeaders[reqAcceptEncoding] do
              FastSetString(inaccept, pRawValue, RawValueLength);
            compressset := ComputeContentEncoding(fCompress, pointer(inaccept));
            ctxt.ConnectionFlags := HTTPREMOTEFLAGS[req^.pSslInfo <> nil];
            ctxt.fConnectionID := req^.ConnectionID;
            // ctxt.fConnectionOpaque is not supported by http.sys
            ctxt.fInHeaders := RetrieveHeadersAndGetRemoteIPConnectionID(
              req^, fRemoteIPHeaderUpper, fRemoteConnIDHeaderUpper,
              {out} ctxt.fRemoteIP, PQword(@ctxt.fConnectionID)^);
            // retrieve any SetAuthenticationSchemes() information
            if byte(fAuthenticationSchemes) <> 0 then // set only with HTTP API 2.0
              for i := 0 to req^.RequestInfoCount - 1 do
                if req^.pRequestInfo^[i].InfoType = HttpRequestInfoTypeAuth then
                  with PHTTP_REQUEST_AUTH_INFO(req^.pRequestInfo^[i].pInfo)^ do
                    case AuthStatus of
                      HttpAuthStatusSuccess:
                        if AuthType > HttpRequestAuthTypeNone then
                        begin
                          byte(ctxt.fAuthenticationStatus) := ord(AuthType) + 1;
                          if AccessToken <> 0 then
                          begin
                            // Per spec https://docs.microsoft.com/en-us/windows/win32/http/authentication-in-http-version-2-0
                            GetDomainUserNameFromToken(AccessToken, ctxt.fAuthenticatedUser);
                            // AccessToken lifecycle is application responsability and should be closed after use
                            CloseHandle(AccessToken);
                          end;
                        end;
                      HttpAuthStatusFailure:
                        ctxt.fAuthenticationStatus := hraFailed;
                    end;
            // abort request if > MaximumAllowedContentLength or OnBeforeBody
            with req^.headers.KnownHeaders[reqContentLength] do
            begin
              V := pointer(pRawValue);
              SetQWord(V, V + RawValueLength, incontlen);
            end;
            if (incontlen > 0) and
               (MaximumAllowedContentLength > 0) and
               (incontlen > MaximumAllowedContentLength) then
            begin
              SendError(HTTP_PAYLOADTOOLARGE, 'Rejected');
              continue;
            end;
            if Assigned(OnBeforeBody) then
            begin
              err := OnBeforeBody(ctxt.fUrl, ctxt.fMethod, ctxt.fInHeaders,
                ctxt.fInContentType, ctxt.fRemoteIP, ctxt.fAuthBearer, incontlen,
                ctxt.ConnectionFlags);
              if err <> HTTP_SUCCESS then
              begin
                SendError(err, 'Rejected');
                continue;
              end;
            end;
            // retrieve body
            if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and req^.flags <> 0 then
            begin
              with req^.headers.KnownHeaders[reqContentEncoding] do
                FastSetString(incontenc, pRawValue, RawValueLength);
              if incontlen <> 0 then
              begin
                // receive body chunks
                SetLength(ctxt.fInContent, incontlen);
                bufread := pointer(ctxt.InContent);
                incontlenread := 0;
                repeat
                  bytesread := 0;
                  if Http.Version.MajorVersion > 1 then
                    // speed optimization for Vista+
                    flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER
                  else
                    flags := 0;
                  incontlenchunk := incontlen - incontlenread;
                  if (fReceiveBufferSize >= 1024) and
                     (incontlenchunk > fReceiveBufferSize) then
                    incontlenchunk := fReceiveBufferSize;
                  err := Http.ReceiveRequestEntityBody(fReqQueue,
                    req^.RequestId, flags, bufread, incontlenchunk, bytesread);
                  if Terminated then
                    exit;
                  inc(incontlenread, bytesread);
                  if err = ERROR_HANDLE_EOF then
                  begin
                    if incontlenread < incontlen then
                      SetLength(ctxt.fInContent, incontlenread);
                    err := NO_ERROR;
                    break; // should loop until returns ERROR_HANDLE_EOF
                  end;
                  if err <> NO_ERROR then
                    break;
                  inc(bufread, bytesread);
                until incontlenread = incontlen;
                if err <> NO_ERROR then
                begin
                  SendError(HTTP_NOTACCEPTABLE, SysErrorMessagePerModule(err, HTTPAPI_DLL));
                  continue;
                end;
                // optionally uncompress input body
                if incontenc <> '' then
                  for i := 0 to high(fCompress) do
                    if fCompress[i].Name = incontenc then
                    begin
                      fCompress[i].Func(ctxt.fInContent, false); // uncompress
                      break;
                    end;
              end;
            end;
            try
              // compute response
              FillcharFast(reps^, SizeOf(reps^), 0);
              respsent := false;
              outstatcode := DoBeforeRequest(ctxt);
              if outstatcode > 0 then
                if not SendResponse or
                   (outstatcode <> HTTP_ACCEPTED) then
                  continue;
              outstatcode := Request(ctxt); // call OnRequest for main process
              afterstatcode := DoAfterRequest(ctxt);
              if afterstatcode > 0 then
                outstatcode := afterstatcode;
              // send response
              if not respsent then
                if not SendResponse then
                  continue;
              DoAfterResponse(ctxt, outstatcode);
            except
              on E: Exception do
                // handle any exception raised during process: show must go on!
                if not respsent then
                  if not E.InheritsFrom(EHttpApiServer) or // ensure still connected
                    (EHttpApiServer(E).LastApiError <> HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
                    SendError(HTTP_SERVERERROR, E.Message, E);
            end;
          finally
            reqid := 0; // reset Request ID to handle the next pending request
          end;
        ERROR_MORE_DATA:
          begin
            // input buffer was too small to hold the request headers
            // -> increase buffer size and call the API again
            reqid := req^.RequestId;
            SetLength(reqbuf, bytesread);
            req := pointer(reqbuf);
          end;
        ERROR_CONNECTION_INVALID:
          if reqid = 0 then
            break
          else
            // TCP connection was corrupted by the peer -> ignore + next request
            reqid := 0;
      else
        break; // unhandled err value
      end;
    until Terminated;
  finally
    ctxt.Free;
  end;
end;

function THttpApiServer.GetHttpQueueLength: cardinal;
var
  len: ULONG;
begin
  if (Http.Version.MajorVersion < 2) or
     (self = nil) then
    result := 0
  else
  begin
    if fOwner <> nil then
      self := fOwner;
    if fReqQueue = 0 then
      result := 0
    else
      EHttpApiServer.RaiseOnError(hQueryRequestQueueProperty,
        Http.QueryRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
          @result, SizeOf(result), 0, @len, nil));
  end;
end;

procedure THttpApiServer.SetHttpQueueLength(aValue: cardinal);
begin
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetRequestQueueProperty, ERROR_OLD_WIN_VERSION);
  if (self <> nil) and
     (fReqQueue <> 0) then
    EHttpApiServer.RaiseOnError(hSetRequestQueueProperty,
      Http.SetRequestQueueProperty(fReqQueue, HttpServerQueueLengthProperty,
        @aValue, SizeOf(aValue), 0, nil));
end;

function THttpApiServer.GetRegisteredUrl: SynUnicode;
var
  i: PtrInt;
begin
  if fRegisteredUnicodeUrl = nil then
    result := ''
  else
    result := fRegisteredUnicodeUrl[0];
  for i := 1 to high(fRegisteredUnicodeUrl) do
    result := result + ',' + fRegisteredUnicodeUrl[i];
end;

function THttpApiServer.GetCloned: boolean;
begin
  result := (fOwner <> nil);
end;

procedure THttpApiServer.SetMaxBandwidth(aValue: cardinal);
var
  qos: HTTP_QOS_SETTING_INFO;
  limit: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  if (self <> nil) and
     (fUrlGroupID <> 0) then
  begin
    if aValue = 0 then
      limit.MaxBandwidth := HTTP_LIMIT_INFINITE
    else if aValue < HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
      limit.MaxBandwidth := HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE
    else
      limit.MaxBandwidth := aValue;
    limit.Flags := 1;
    qos.QosType := HttpQosSettingTypeBandwidth;
    qos.QosSetting := @limit;
    EHttpApiServer.RaiseOnError(hSetServerSessionProperty,
      Http.SetServerSessionProperty(fServerSessionID, HttpServerQosProperty,
        @qos, SizeOf(qos)));
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qos, SizeOf(qos)));
  end;
end;

function THttpApiServer.GetMaxBandwidth: cardinal;
var
  info: record
    qos: HTTP_QOS_SETTING_INFO;
    limit: HTTP_BANDWIDTH_LIMIT_INFO;
  end;
begin
  if (Http.Version.MajorVersion < 2) or
     (self = nil) then
  begin
    result := 0;
    exit;
  end;
  if fOwner <> nil then
    self := fOwner;
  if fUrlGroupID = 0 then
  begin
    result := 0;
    exit;
  end;
  info.qos.QosType := HttpQosSettingTypeBandwidth;
  info.qos.QosSetting := @info.limit;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @info, SizeOf(info)));
  result := info.limit.MaxBandwidth;
end;

function THttpApiServer.GetMaxConnections: cardinal;
var
  info: record
    qos: HTTP_QOS_SETTING_INFO;
    limit: HTTP_CONNECTION_LIMIT_INFO;
  end;
  len: ULONG;
begin
  if (Http.Version.MajorVersion < 2) or
     (self = nil) then
  begin
    result := 0;
    exit;
  end;
  if fOwner <> nil then
    self := fOwner;
  if fUrlGroupID = 0 then
  begin
    result := 0;
    exit;
  end;
  info.qos.QosType := HttpQosSettingTypeConnectionLimit;
  info.qos.QosSetting := @info.limit;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @info, SizeOf(info), @len));
  result := info.limit.MaxConnections;
end;

procedure THttpApiServer.SetMaxConnections(aValue: cardinal);
var
  qos: HTTP_QOS_SETTING_INFO;
  limit: HTTP_CONNECTION_LIMIT_INFO;
begin
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  if (self <> nil) and
     (fUrlGroupID <> 0) then
  begin
    if aValue = 0 then
      limit.MaxConnections := HTTP_LIMIT_INFINITE
    else
      limit.MaxConnections := aValue;
    limit.Flags := 1;
    qos.QosType := HttpQosSettingTypeConnectionLimit;
    qos.QosSetting := @limit;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qos, SizeOf(qos)));
  end;
end;

function THttpApiServer.HasApi2: boolean;
begin
  result := Http.Version.MajorVersion >= 2;
end;

function THttpApiServer.GetLogging: boolean;
begin
  result := (fLogData <> nil);
end;

procedure THttpApiServer.LogStart(const aLogFolder: TFileName;
  aType: THttpApiLoggingType; const aSoftwareName: TFileName;
  aRolloverType: THttpApiLoggingRollOver; aRolloverSize: cardinal;
  aLogFields: THttpApiLogFields; aFlags: THttpApiLoggingFlags);
var
  log: HTTP_LOGGING_INFO;
  folder, software: SynUnicode;
begin
  if (self = nil) or
     (fOwner <> nil) then
    exit;
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  fLogData := nil; // disable any previous logging
  FillcharFast(log, SizeOf(log), 0);
  log.Flags := 1;
  log.LoggingFlags := byte(aFlags);
  if aLogFolder = '' then
    raise EHttpApiServer.CreateFmt('LogStart(aLogFolder="")', []);
  if length(aLogFolder) > 212 then
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532
    raise EHttpApiServer.CreateFmt('aLogFolder is too long for LogStart(%s)', [aLogFolder]);
  folder := SynUnicode(aLogFolder);
  software := SynUnicode(aSoftwareName);
  log.SoftwareNameLength := length(software) * 2;
  log.SoftwareName := pointer(software);
  log.DirectoryNameLength := length(folder) * 2;
  log.DirectoryName := pointer(folder);
  log.Format := HTTP_LOGGING_TYPE(aType);
  if aType = hltNCSA then
    aLogFields := [hlfDate..hlfSubStatus];
  log.Fields := integer(aLogFields);
  log.RolloverType := HTTP_LOGGING_ROLLOVER_TYPE(aRolloverType);
  if aRolloverType = hlrSize then
    log.RolloverSize := aRolloverSize;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerLoggingProperty,
      @log, SizeOf(log)));
  // on success, update the actual log memory structure
  fLogData := pointer(fLogDataStorage);
end;

procedure THttpApiServer.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer);
var
  i: PtrInt;
begin
  inherited;
  for i := 0 to length(fClones) - 1 do
    fClones[i].RegisterCompress(aFunction, aCompressMinSize);
end;

procedure THttpApiServer.SetOnTerminate(const Event: TOnNotifyThread);
var
  i: PtrInt;
begin
  inherited SetOnTerminate(Event);
  if fOwner = nil then
    for i := 0 to length(fClones) - 1 do
      fClones[i].OnHttpThreadTerminate := Event;
end;

procedure THttpApiServer.LogStop;
var
  i: PtrInt;
begin
  if (self = nil) or
     (fClones = nil) or
     (fLogData = nil) then
    exit;
  fLogData := nil;
  for i := 0 to length(fClones) - 1 do
    fClones[i].fLogData := nil;
end;

procedure THttpApiServer.SetReceiveBufferSize(Value: cardinal);
var
  i: PtrInt;
begin
  fReceiveBufferSize := Value;
  for i := 0 to length(fClones) - 1 do
    fClones[i].fReceiveBufferSize := Value;
end;

procedure THttpApiServer.SetServerName(const aName: RawUtf8);
var
  i: PtrInt;
begin
  inherited SetServerName(aName);
  with PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^ do
  begin
    ServerName := pointer(aName);
    ServerNameLength := Length(aName);
  end;
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetServerName(aName);
end;

procedure THttpApiServer.SetOnRequest(const aRequest: TOnHttpServerRequest);
var
  i: PtrInt;
begin
  inherited SetOnRequest(aRequest);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetOnRequest(aRequest);
end;

procedure THttpApiServer.SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody);
var
  i: PtrInt;
begin
  inherited SetOnBeforeBody(aEvent);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetOnBeforeBody(aEvent);
end;

procedure THttpApiServer.SetOnBeforeRequest(const aEvent: TOnHttpServerRequest);
var
  i: PtrInt;
begin
  inherited SetOnBeforeRequest(aEvent);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetOnBeforeRequest(aEvent);
end;

procedure THttpApiServer.SetOnAfterRequest(const aEvent: TOnHttpServerRequest);
var
  i: PtrInt;
begin
  inherited SetOnAfterRequest(aEvent);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetOnAfterRequest(aEvent);
end;

procedure THttpApiServer.SetOnAfterResponse(const aEvent: TOnHttpServerAfterResponse);
var
  i: PtrInt;
begin
  inherited SetOnAfterResponse(aEvent);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetOnAfterResponse(aEvent);
end;

procedure THttpApiServer.SetMaximumAllowedContentLength(aMax: cardinal);
var
  i: PtrInt;
begin
  inherited SetMaximumAllowedContentLength(aMax);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetMaximumAllowedContentLength(aMax);
end;

procedure THttpApiServer.SetRemoteIPHeader(const aHeader: RawUtf8);
var
  i: PtrInt;
begin
  inherited SetRemoteIPHeader(aHeader);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetRemoteIPHeader(aHeader);
end;

procedure THttpApiServer.SetRemoteConnIDHeader(const aHeader: RawUtf8);
var
  i: PtrInt;
begin
  inherited SetRemoteConnIDHeader(aHeader);
  for i := 0 to length(fClones) - 1 do
    fClones[i].SetRemoteConnIDHeader(aHeader);
end;

procedure THttpApiServer.SetLoggingServiceName(const aName: RawUtf8);
begin
  if self = nil then
    exit;
  fLoggingServiceName := aName;
  with PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^ do
  begin
    ServiceName := pointer(fLoggingServiceName);
    ServiceNameLength := Length(fLoggingServiceName);
  end;
end;

procedure THttpApiServer.SetAuthenticationSchemes(schemes:
  THttpApiRequestAuthentications; const DomainName, Realm: SynUnicode);
var
  auth: HTTP_SERVER_AUTHENTICATION_INFO;
begin
  if (self = nil) or
     (fOwner <> nil) then
    exit;
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  fAuthenticationSchemes := schemes;
  FillcharFast(auth, SizeOf(auth), 0);
  auth.Flags := 1;
  auth.AuthSchemes := byte(schemes);
  auth.ReceiveMutualAuth := true;
  if haBasic in schemes then
    with auth.BasicParams do
    begin
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  if haDigest in schemes then
    with auth.DigestParams do
    begin
      DomainNameLength := Length(DomainName);
      DomainName := pointer(DomainName);
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerAuthenticationProperty,
      @auth, SizeOf(auth)));
end;

procedure THttpApiServer.SetTimeOutLimits(aEntityBody, aDrainEntityBody,
  aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
var
  timeout: HTTP_TIMEOUT_LIMIT_INFO;
begin
  if (self = nil) or
     (fOwner <> nil) then
    exit;
  if Http.Version.MajorVersion < 2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  FillcharFast(timeout, SizeOf(timeout), 0);
  timeout.Flags := 1;
  timeout.EntityBody := aEntityBody;
  timeout.DrainEntityBody := aDrainEntityBody;
  timeout.RequestQueue := aRequestQueue;
  timeout.IdleConnection := aIdleConnection;
  timeout.HeaderWait := aHeaderWait;
  timeout.MinSendRate := aMinSendRate;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerTimeoutsProperty,
      @timeout, SizeOf(timeout)));
end;



{ ****************** THttpApiWebSocketServer Over Windows http.sys Module }

{ THttpApiWebSocketServerProtocol }

const
  WebSocketConnectionCapacity = 1000;

function THttpApiWebSocketServerProtocol.AddConnection(
  aConn: PHttpApiWebSocketConnection): integer;
var
  i: PtrInt;
begin
  if fFirstEmptyConnectionIndex >= fConnectionsCapacity - 1 then
  begin
    inc(fConnectionsCapacity, WebSocketConnectionCapacity);
    ReallocMem(fConnections, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
    FillcharFast(fConnections^[fConnectionsCapacity - WebSocketConnectionCapacity],
      WebSocketConnectionCapacity * SizeOf(PHttpApiWebSocketConnection), 0);
  end;
  if fFirstEmptyConnectionIndex >= fConnectionsCount then
    fConnectionsCount := fFirstEmptyConnectionIndex + 1;
  fConnections[fFirstEmptyConnectionIndex] := aConn;
  result := fFirstEmptyConnectionIndex;
  for i := fFirstEmptyConnectionIndex + 1 to fConnectionsCount do
  begin
    if fConnections[i] = nil then
    begin
      fFirstEmptyConnectionIndex := i;
      Break;
    end;
  end;
end;

function THttpApiWebSocketServerProtocol.Broadcast(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer;
  aBufferSize: ULONG): boolean;
var
  i: PtrInt;
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fConnectionsCount - 1 do
      if Assigned(fConnections[i]) then
        fConnections[i].Send(aBufferType, aBuffer, aBufferSize);
  finally
    LeaveCriticalSection(fSafe);
  end;
  result := True;
end;

function THttpApiWebSocketServerProtocol.Close(index: integer;
  aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  if cardinal(index) < cardinal(fConnectionsCount) then
  begin
    conn := fConnections^[index];
    if (conn <> nil) and
       (conn.fState = wsOpen) then
    begin
      conn.Close(aStatus, aBuffer, aBufferSize);
      result := True;
    end;
  end;
end;

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
  InitializeCriticalSection(fSafe);
  fPendingForClose := TSynList.Create;
  fName := aName;
  fManualFragmentManagement := aManualFragmentManagement;
  fServer := aServer;
  fOnAccept := aOnAccept;
  fOnMessage := aOnMessage;
  fOnConnect := aOnConnect;
  fOnDisconnect := aOnDisconnect;
  fOnFragment := aOnFragment;
  fConnectionsCapacity := WebSocketConnectionCapacity;
  fConnectionsCount := 0;
  fFirstEmptyConnectionIndex := 0;
  fConnections := AllocMem(fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
end;

destructor THttpApiWebSocketServerProtocol.Destroy;
var
  i: PtrInt;
  conn: PHttpApiWebSocketConnection;
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fPendingForClose.Count - 1 do
    begin
      conn := fPendingForClose[i];
      if Assigned(conn) then
      begin
        conn.DoOnDisconnect();
        conn.Disconnect();
        Dispose(conn);
      end;
    end;
    fPendingForClose.Free;
  finally
    LeaveCriticalSection(fSafe);
  end;
  DeleteCriticalSection(fSafe);
  FreeMem(fConnections);
  fConnections := nil;
  inherited;
end;

procedure THttpApiWebSocketServerProtocol.doShutdown;
var
  i: PtrInt;
  conn: PHttpApiWebSocketConnection;
const
  sReason = 'Server shutdown';
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fConnectionsCount - 1 do
    begin
      conn := fConnections[i];
      if Assigned(conn) then
      begin
        RemoveConnection(i);
        conn.fState := wsClosedByShutdown;
        conn.fBuffer := sReason;
        conn.fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
        conn.Close(WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS,
          Pointer(conn.fBuffer), Length(conn.fBuffer));
// PostQueuedCompletionStatus(fServer.fThreadPoolServer.FRequestQueue, 0, 0, @conn.fOverlapped);
      end;
    end;
  finally
    LeaveCriticalSection(fSafe);
  end;
end;

procedure THttpApiWebSocketServerProtocol.RemoveConnection(index: integer);
begin
  fPendingForClose.Add(fConnections[index]);
  fConnections[index] := nil;
  if fFirstEmptyConnectionIndex > index then
    fFirstEmptyConnectionIndex := index;
end;

function THttpApiWebSocketServerProtocol.Send(index: integer;
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG): boolean;
var
  conn: PHttpApiWebSocketConnection;
begin
  result := false;
  if (index >= 0) and
     (index < fConnectionsCount) then
  begin
    conn := fConnections^[index];
    if (conn <> nil) and
       (conn.fState = wsOpen) then
    begin
      conn.Send(aBufferType, aBuffer, aBufferSize);
      result := True;
    end;
  end;
end;


 { THttpApiWebSocketConnection }

function THttpApiWebSocketConnection.TryAcceptConnection(
  aProtocol: THttpApiWebSocketServerProtocol;
  Ctxt: THttpServerRequestAbstract; aNeedHeader: boolean): boolean;
var
  req: PHTTP_REQUEST;
  reqhead: WEB_SOCKET_HTTP_HEADER_ARR;
  srvhead: PWEB_SOCKET_HTTP_HEADER;
  srvheadcount: ULONG;
begin
  fState := wsConnecting;
  fBuffer := '';
  fWSHandle := nil;
  fLastActionContext := nil;
  FillcharFast(fOverlapped, SizeOf(fOverlapped), 0);
  fProtocol := aProtocol;
  req := PHTTP_REQUEST((Ctxt as THttpServerRequest).HttpApiRequest);
  fIndex := fProtocol.fFirstEmptyConnectionIndex;
  fOpaqueHTTPRequestId := req^.RequestId;
  if (fProtocol = nil) or
     (Assigned(fProtocol.OnAccept) and
      not fProtocol.OnAccept(Ctxt as THttpServerRequest, Self)) then
  begin
    result := False;
    exit;
  end;
  EWebSocketApi.RaiseOnError(hCreateServerHandle,
    WebSocketApi.CreateServerHandle(nil, 0, fWSHandle));
  reqhead := HttpSys2ToWebSocketHeaders(req^.headers);
  if aNeedHeader then
    result := WebSocketApi.BeginServerHandshake(fWSHandle,
      Pointer(fProtocol.name), nil, 0, @reqhead[0], Length(reqhead), srvhead,
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
  if not result then
    Disconnect
  else
    fLastReceiveTickCount := 0;
end;

procedure THttpApiWebSocketConnection.DoOnMessage(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);

  procedure PushFragmentIntoBuffer;
  var
    l: integer;
  begin
    l := Length(fBuffer);
    SetLength(fBuffer, l + integer(aBufferSize));
    MoveFast(aBuffer^, fBuffer[l + 1], aBufferSize);
  end;

begin
  if fProtocol = nil then
    exit;
  if (aBufferType = WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE) or
     (aBufferType = WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) then
  begin
    // Fragment
    if not fProtocol.ManualFragmentManagement then
      PushFragmentIntoBuffer;
    if Assigned(fProtocol.OnFragment) then
      fProtocol.OnFragment(self, aBufferType, aBuffer, aBufferSize);
  end
  else
  begin
    // last Fragment
    if Assigned(fProtocol.OnMessage) then
    begin
      if fProtocol.ManualFragmentManagement then
        fProtocol.OnMessage(self, aBufferType, aBuffer, aBufferSize)
      else
      begin
        PushFragmentIntoBuffer;
        fProtocol.OnMessage(self, aBufferType, Pointer(fBuffer), Length(fBuffer));
        fBuffer := '';
      end;
    end;
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
    fProtocol.OnDisconnect(self, fCloseStatus, Pointer(fBuffer), length(fBuffer));
end;

function THttpApiWebSocketConnection.ReadData(const WebsocketBufferData): integer;
var
  err: HRESULT;
  read: cardinal;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  result := 0;
  if fWSHandle = nil then
    exit;
  err := Http.ReceiveRequestEntityBody(fProtocol.fServer.fReqQueue,
    fOpaqueHTTPRequestId, 0, buf.pbBuffer, buf.ulBufferLength, read,
    @self.fOverlapped);
  case err of
    // On page reload Safari do not send a WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION
    // with BufferType = WEB_SOCKET_CLOSE_BUFFER_TYPE, instead it send a dummy packet
    // (WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION) and terminate socket
    // see forum discussion https://synopse.info/forum/viewtopic.php?pid=27125
    ERROR_HANDLE_EOF:
      result := -1;
    ERROR_IO_PENDING:
      ; //
    NO_ERROR:
      ; //
  else
    // todo: close connection?
  end;
end;

procedure THttpApiWebSocketConnection.WriteData(const WebsocketBufferData);
var
  err: HRESULT;
  inmem: HTTP_DATA_CHUNK_INMEMORY;
  writ: cardinal;
  buf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  if fWSHandle = nil then
    exit;
  writ := 0;
  inmem.DataChunkType := hctFromMemory;
  inmem.pBuffer := buf.pbBuffer;
  inmem.BufferLength := buf.ulBufferLength;
  err := Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue,
    fOpaqueHTTPRequestId, HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA or
    HTTP_SEND_RESPONSE_FLAG_MORE_DATA, 1, @inmem, writ, nil, nil,
    @fProtocol.fServer.fSendOverlaped);
  case err of
    ERROR_HANDLE_EOF:
      Disconnect;
    ERROR_IO_PENDING:
      ; //
    NO_ERROR:
      ; //
  else
    // todo: close connection?
  end;
end;

procedure THttpApiWebSocketConnection.CheckIsActive;
var
  elapsed: Int64;
begin
  if (fLastReceiveTickCount > 0) and
     (fProtocol.fServer.fPingTimeout > 0) then
  begin
    elapsed := GetTickCount64 - fLastReceiveTickCount;
    if elapsed > 2 * fProtocol.fServer.PingTimeout * 1000 then
    begin
      fProtocol.RemoveConnection(fIndex);
      fState := wsClosedByGuard;
      fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
      fBuffer := 'Closed after ping timeout';
      PostQueuedCompletionStatus(
        fProtocol.fServer.fThreadPoolServer.FRequestQueue, 0, nil, @fOverlapped);
    end
    else if elapsed >= fProtocol.fServer.PingTimeout * 1000 then
      Ping;
  end;
end;

procedure THttpApiWebSocketConnection.Disconnect;
var //Err: HRESULT; //todo: handle error
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
  if fState in [wsOpen, wsClosing] then
  begin
    if Assigned(fLastActionContext) then
    begin
      EWebSocketApi.RaiseOnError(hCompleteAction,
        WebSocketApi.CompleteAction(fWSHandle, fLastActionContext,
        fOverlapped.InternalHigh));
      fLastActionContext := nil;
    end
    else
      EWebSocketApi.RaiseOnError(hReceive,
        WebSocketApi.Receive(fWSHandle, nil, nil));
  end
  else
    raise EWebSocketApi.CreateFmt(
      'THttpApiWebSocketConnection.BeforeRead state is not wsOpen (%d)',
      [ord(fState)]);
end;

const
  C_WEB_SOCKET_BUFFER_SIZE = 2;

type
  TWebSocketBufferDataArr = array[0..C_WEB_SOCKET_BUFFER_SIZE - 1] of WEB_SOCKET_BUFFER_DATA;

function THttpApiWebSocketConnection.ProcessActions(
  ActionQueue: WEB_SOCKET_ACTION_QUEUE): boolean;
var
  buf: TWebSocketBufferDataArr;
  bufcount: ULONG;
  buftyp: WEB_SOCKET_BUFFER_TYPE;
  action: WEB_SOCKET_ACTION;
  appctxt: Pointer;
  actctxt: Pointer;
  i: PtrInt;
  err: HRESULT;

  procedure CloseConnection;
  begin
    EnterCriticalSection(fProtocol.fSafe);
    try
      fProtocol.RemoveConnection(fIndex);
    finally
      LeaveCriticalSection(fProtocol.fSafe);
    end;
    EWebSocketApi.RaiseOnError(hCompleteAction,
      WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
  end;

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
          for i := 0 to bufcount - 1 do
            WriteData(buf[i]);
          if fWSHandle <> nil then
          begin
            err := WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
            EWebSocketApi.RaiseOnError(hCompleteAction, err);
          end;
          result := False;
          exit;
        end;
      WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION:
        ;
      WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION:
        begin
          for i := 0 to bufcount - 1 do
            if ReadData(buf[i]) = -1 then
            begin
              fState := wsClosedByClient;
              fBuffer := '';
              fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
              CloseConnection;
            end;
          fLastActionContext := actctxt;
          result := False;
          exit;
        end;
      WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION:
        begin
          fLastReceiveTickCount := GetTickCount64;
          if buftyp = WEB_SOCKET_CLOSE_BUFFER_TYPE then
          begin
            if fState = wsOpen then
              fState := wsClosedByClient
            else
              fState := wsClosedByServer;
            FastSetRawByteString(fBuffer, buf[0].pbBuffer, buf[0].ulBufferLength);
            fCloseStatus := buf[0].Reserved1;
            CloseConnection;
            result := False;
            exit;
          end
          else if buftyp = WEB_SOCKET_PING_PONG_BUFFER_TYPE then
          begin
            // todo: may be answer to client's ping
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end
          else if buftyp = WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE then
          begin
            // todo: may be handle this situation
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end
          else
          begin
            DoOnMessage(buftyp, buf[0].pbBuffer, buf[0].ulBufferLength);
            EWebSocketApi.RaiseOnError(hCompleteAction,
              WebSocketApi.CompleteAction(fWSHandle, actctxt, 0));
            exit;
          end;
        end
    else
      raise EWebSocketApi.CreateFmt('Invalid WebSocket action %d', [byte(action)]);
    end;
    err := WebSocketApi.CompleteAction(fWSHandle, actctxt, 0);
    if actctxt <> nil then
      EWebSocketApi.RaiseOnError(hCompleteAction, err);
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
  aBuffer: Pointer; aBufferSize: ULONG);
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
  aBuffer: Pointer; aBufferSize: ULONG);
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


{ THttpApiWebSocketServer }

constructor THttpApiWebSocketServer.Create(
  aSocketThreadsCount, aPingTimeout: integer; const QueueName: SynUnicode;
  const aOnWSThreadStart, aOnWSThreadTerminate: TOnNotifyThread;
  ProcessOptions: THttpServerOptions);
begin
  inherited Create(QueueName, nil, nil, '', ProcessOptions);
  if not (WebSocketApi.WebSocketEnabled) then
    raise EWebSocketApi.Create('WebSocket API not supported');
  fPingTimeout := aPingTimeout;
  if fPingTimeout > 0 then
    fGuard := TSynWebSocketGuard.Create(Self);
  New(fRegisteredProtocols);
  SetLength(fRegisteredProtocols^, 0);
  FOnWSThreadStart := aOnWSThreadStart;
  FOnWSThreadTerminate := aOnWSThreadTerminate;
  fThreadPoolServer := TSynThreadPoolHttpApiWebSocketServer.Create(Self,
    aSocketThreadsCount);
end;

constructor THttpApiWebSocketServer.CreateClone(From: THttpApiServer);
var
  serv: THttpApiWebSocketServer absolute From;
begin
  inherited CreateClone(From);
  fThreadPoolServer := serv.fThreadPoolServer;
  fPingTimeout := serv.fPingTimeout;
  fRegisteredProtocols := serv.fRegisteredProtocols
end;

procedure THttpApiWebSocketServer.DestroyMainThread;
var
  i: PtrInt;
begin
  fGuard.Free;
  for i := 0 to Length(fRegisteredProtocols^) - 1 do
    fRegisteredProtocols^[i].doShutdown;
  FreeAndNilSafe(fThreadPoolServer);
  for i := 0 to Length(fRegisteredProtocols^) - 1 do
    fRegisteredProtocols^[i].Free;
  fRegisteredProtocols^ := nil;
  Dispose(fRegisteredProtocols);
  fRegisteredProtocols := nil;
  inherited;
end;

procedure THttpApiWebSocketServer.DoAfterResponse(Ctxt: THttpServerRequest;
  const Code: cardinal);
begin
  if Assigned(fLastConnection) then
    PostQueuedCompletionStatus(fThreadPoolServer.FRequestQueue, 0, nil,
      @fLastConnection.fOverlapped);
  inherited DoAfterResponse(Ctxt, Code);
end;

function THttpApiWebSocketServer.GetProtocol(index: integer):
  THttpApiWebSocketServerProtocol;
begin
  if cardinal(index) < cardinal(Length(fRegisteredProtocols^)) then
    result := fRegisteredProtocols^[index]
  else
    result := nil;
end;

function THttpApiWebSocketServer.getProtocolsCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := Length(fRegisteredProtocols^);
end;

function THttpApiWebSocketServer.getSendResponseFlags(Ctxt: THttpServerRequest): integer;
begin
  if (PHTTP_REQUEST(Ctxt.HttpApiRequest)^.UrlContext = WEB_SOCKET_URL_CONTEXT) and
     (fLastConnection <> nil) then
    result := HTTP_SEND_RESPONSE_FLAG_OPAQUE or
      HTTP_SEND_RESPONSE_FLAG_MORE_DATA or HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA
  else
    result := inherited getSendResponseFlags(Ctxt);
end;

function THttpApiWebSocketServer.UpgradeToWebSocket(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  proto: THttpApiWebSocketServerProtocol;
  i, j: PtrInt;
  req: PHTTP_REQUEST;
  p: PHTTP_UNKNOWN_HEADER;
  ch, chB: PUtf8Char;
  protoname: RawUtf8;
  protofound: boolean;
label
  fnd;
begin
  result := 404;
  proto := nil;
  protofound := false;
  req := PHTTP_REQUEST((Ctxt as THttpServerRequest).HttpApiRequest);
  p := req^.headers.pUnknownHeaders;
  for j := 1 to req^.headers.UnknownHeaderCount do
  begin
    if (p.NameLength = Length(sProtocolHeader)) and
       IdemPChar(p.pName, pointer(sProtocolHeader)) then
    begin
      protofound := true;
      for i := 0 to Length(fRegisteredProtocols^) - 1 do
      begin
        ch := p.pRawValue;
        while (ch - p.pRawValue) < p.RawValueLength do
        begin
          while ((ch - p.pRawValue) < p.RawValueLength) and
                (ch^ in [',', ' ']) do
            inc(ch);
          chB := ch;
          while ((ch - p.pRawValue) < p.RawValueLength) and
                not (ch^ in [',']) do
            inc(ch);
          FastSetString(protoname, chB, ch - chB);
          if protoname = fRegisteredProtocols^[i].name then
          begin
            proto := fRegisteredProtocols^[i];
            goto fnd;
          end;
        end;
      end;
    end;
    inc(p);
  end;
  if not protofound and
     (proto = nil) and
     (Length(fRegisteredProtocols^) = 1) then
    proto := fRegisteredProtocols^[0];
fnd:
  if proto <> nil then
  begin
    EnterCriticalSection(proto.fSafe);
    try
      New(fLastConnection);
      if fLastConnection.TryAcceptConnection(
          proto, Ctxt, protofound) then
      begin
        proto.AddConnection(fLastConnection);
        result := 101
      end
      else
      begin
        Dispose(fLastConnection);
        fLastConnection := nil;
        result := HTTP_NOTALLOWED;
      end;
    finally
      LeaveCriticalSection(proto.fSafe);
    end;
  end;
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
  protocol: THttpApiWebSocketServerProtocol;
begin
  if self = nil then
    exit;
  protocol := THttpApiWebSocketServerProtocol.Create(aName,
    aManualFragmentManagement, Self, aOnAccept, aOnMessage, aOnConnect,
    aOnDisconnect, aOnFragment);
  protocol.fIndex := length(fRegisteredProtocols^);
  SetLength(fRegisteredProtocols^, protocol.fIndex + 1);
  fRegisteredProtocols^[protocol.fIndex] := protocol;
end;

function THttpApiWebSocketServer.Request(
  Ctxt: THttpServerRequestAbstract): cardinal;
begin
  if PHTTP_REQUEST(THttpServerRequest(Ctxt).HttpApiRequest).
       UrlContext = WEB_SOCKET_URL_CONTEXT then
    result := UpgradeToWebSocket(Ctxt)
  else
  begin
    result := inherited Request(Ctxt);
    fLastConnection := nil;
  end;
end;

procedure THttpApiWebSocketServer.SendServiceMessage;
begin
  PostQueuedCompletionStatus(
    fThreadPoolServer.FRequestQueue, 0, nil, @fServiceOverlaped);
end;

procedure THttpApiWebSocketServer.SetOnWSThreadStart(const Value: TOnNotifyThread);
begin
  FOnWSThreadStart := Value;
end;

procedure THttpApiWebSocketServer.SetOnWSThreadTerminate(const Value: TOnNotifyThread);
begin
  FOnWSThreadTerminate := Value;
end;


{ TSynThreadPoolHttpApiWebSocketServer }

function TSynThreadPoolHttpApiWebSocketServer.NeedStopOnIOError: boolean;
begin
  // If connection closed by guard than ERROR_HANDLE_EOF or ERROR_OPERATION_ABORTED
  // can be returned - Other connections must work normally
  result := False;
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadStart(Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadStart) then
    fServer.OnWSThreadStart(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.OnThreadTerminate(Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadTerminate) then
    fServer.OnWSThreadTerminate(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.Task(
  aCaller: TSynThreadPoolWorkThread; aContext: Pointer);
var
  conn: PHttpApiWebSocketConnection;
begin
  if aContext = @fServer.fSendOverlaped then
    exit;
  if aContext = @fServer.fServiceOverlaped then
  begin
    if Assigned(fServer.OnServiceMessage) then
      fServer.OnServiceMessage;
    exit;
  end;
  conn := PHttpApiWebSocketConnection(aContext);
  if conn.fState = wsConnecting then
  begin
    conn.fState := wsOpen;
    conn.fLastReceiveTickCount := GetTickCount64;
    conn.DoOnConnect();
  end;
  if conn.fState in [wsOpen, wsClosing] then
    repeat
      conn.BeforeRead;
    until not conn.ProcessActions(WEB_SOCKET_RECEIVE_ACTION_QUEUE);
  if conn.fState in [wsClosedByGuard] then
    EWebSocketApi.RaiseOnError(hCompleteAction,
      WebSocketApi.CompleteAction(conn.fWSHandle, conn.fLastActionContext, 0));
  if conn.fState in
       [wsClosedByClient, wsClosedByServer, wsClosedByGuard, wsClosedByShutdown] then
  begin
    conn.DoOnDisconnect;
    if conn.fState = wsClosedByClient then
      conn.Close(conn.fCloseStatus, Pointer(conn.fBuffer), length(conn.fBuffer));
    conn.Disconnect;
    EnterCriticalSection(conn.Protocol.fSafe);
    try
      conn.Protocol.fPendingForClose.Remove(conn);
    finally
      LeaveCriticalSection(conn.Protocol.fSafe);
    end;
    Dispose(conn);
  end;
end;

constructor TSynThreadPoolHttpApiWebSocketServer.Create(Server:
  THttpApiWebSocketServer; NumberOfThreads: integer);
begin
  fServer := Server;
  fOnThreadStart := OnThreadStart;
  fOnThreadTerminate := OnThreadTerminate;
  inherited Create(NumberOfThreads, Server.fReqQueue);
end;


{ TSynWebSocketGuard }

procedure TSynWebSocketGuard.Execute;
var
  i, j: PtrInt;
  prot: THttpApiWebSocketServerProtocol;
begin
  if fServer.fPingTimeout > 0 then
    while not Terminated do
    begin
      if fServer <> nil then
        for i := 0 to Length(fServer.fRegisteredProtocols^) - 1 do
        begin
          prot := fServer.fRegisteredProtocols^[i];
          EnterCriticalSection(prot.fSafe);
          try
            for j := 0 to prot.fConnectionsCount - 1 do
              if Assigned(prot.fConnections[j]) then
                prot.fConnections[j].CheckIsActive;
          finally
            LeaveCriticalSection(prot.fSafe);
          end;
        end;
      i := 0;
      while not Terminated and
            (i < fServer.fPingTimeout) do
      begin
        Sleep(1000);
        inc(i);
      end;
    end
  else
    Terminate;
end;

constructor TSynWebSocketGuard.Create(Server: THttpApiWebSocketServer);
begin
  fServer := Server;
  inherited Create(false);
end;

{$endif USEWININET}

end.

