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
  mormot.core.rtti,
  mormot.net.sock,
  mormot.net.http,
  {$ifdef USEWININET}
  mormot.lib.winhttp,
  {$endif USEWININET}
  mormot.net.client;


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
    fConnectionThread: TSynThread;
    {$ifdef OSWINDOWS}
    fHttpApiRequest: Pointer;
    fFullURL: SynUnicode;
    {$endif OSWINDOWS}
  public
    /// initialize the context, associated to a HTTP server instance
    constructor Create(aServer: THttpServerGeneric;
      aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
      aConnectionFlags: THttpServerRequestFlags); virtual;
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aURL, aMethod, aInHeaders: RawUtf8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8);
        override;
    {$ifdef OSWINDOWS}
    /// input parameter containing the caller Full URL
    property FullURL: SynUnicode
      read fFullURL;
    {$endif OSWINDOWS}
    /// the associated server instance
    // - may be a THttpServer or a THttpApiServer class
    property Server: THttpServerGeneric
      read fServer;
    /// the thread which owns the connection of this execution context
    // - depending on the HTTP server used, may not follow ConnectionID
    property ConnectionThread: TSynThread
      read fConnectionThread;
    {$ifdef OSWINDOWS}
    /// for THttpApiServer, points to a PHTTP_REQUEST structure
    // - not used by now for other kind of servers
    property HttpApiRequest: Pointer
      read fHttpApiRequest;
    {$endif OSWINDOWS}
  end;

  /// abstract class to implement a server thread
  // - do not use this class, but rather the THttpServer, THttpApiServer
  // or TWebSocketServer (as defined in mormot.net.websock)
  TServerGeneric = class(TSynThread)
  protected
    fProcessName: RawUtf8;
    fOnHttpThreadStart: TOnNotifyThread;
    procedure SetOnTerminate(const Event: TOnNotifyThread); virtual;
    procedure NotifyThreadStart(Sender: TSynThread);
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: boolean;
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8); reintroduce; virtual;
  end;

  /// abstract class to implement a HTTP server
  // - do not use this class, but rather the THttpServer or THttpApiServer
  THttpServerGeneric = class(TServerGeneric)
  protected
    fShutdownInProgress: boolean;
    /// optional event handlers for process interception
    fOnRequest: TOnHttpServerRequest;
    fOnBeforeBody: TOnHttpServerBeforeBody;
    fOnBeforeRequest: TOnHttpServerRequest;
    fOnAfterRequest: TOnHttpServerRequest;
    fOnAfterResponse: TOnHttpServerAfterResponse;
    fMaximumAllowedContentLength: cardinal;
    /// list of all registered compression algorithms
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawUtf8;
    fServerName: RawUtf8;
    fCurrentConnectionID: integer; // 31-bit NextConnectionID sequence
    fCurrentRequestID: integer;
    fCanNotifyCallback: boolean;
    fRemoteIPHeader, fRemoteIPHeaderUpper: RawUtf8;
    fRemoteConnIDHeader, fRemoteConnIDHeaderUpper: RawUtf8;
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
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: boolean; 
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8); reintroduce; virtual;
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
    // or custom (synlzo/synlz) protocols
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
      read fOnHttpThreadStart write fOnHttpThreadStart;
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
    /// defines request/response internal queue length
    // - default value if 1000, which sounds fine for most use cases
    // - for THttpApiServer, will return 0 if the system does not support HTTP
    // API 2.0 (i.e. under Windows XP or Server 2003)
    // - for THttpServer, will shutdown any incoming accepted socket if the
    // internal TSynThreadPool.PendingContextCount+ThreadCount exceeds this limit;
    // each pending connection is a THttpServerSocket instance in the queue
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
    // - only TWebSocketServer has this ability by now
    property CanNotifyCallback: boolean
      read fCanNotifyCallback;
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
  published
    /// returns the API version used by the inherited implementation
    property ApiVersion: RawUtf8
      read GetApiVersion;
    /// the Server name, UTF-8 encoded, e.g. 'mORMot/1.18 (Linux)'
    // - will be served as "Server: ..." HTTP header
    // - for THttpApiServer, when called from the main instance, will propagate
    // the change to all cloned instances, and included in any HTTP API 2.0 log
    property ServerName: RawUtf8
      read fServerName write SetServerName;
    /// the associated process name
    property ProcessName: RawUtf8
      read fProcessName write fProcessName;
  end;



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
  // - grOwned indicates that this connection is now handled by another thread,
  // e.g. asynchronous WebSockets
  THttpServerSocketGetRequestResult = (
    grError,
    grException,
    grOversizedPayload,
    grRejected,
    grTimeout,
    grHeaderReceived,
    grBodyReceived,
    grOwned);

  {$M+} // to have existing RTTI for published properties
  THttpServer = class;
  {$M-}

  /// Socket API based HTTP/1.1 server class used by THttpServer Threads
  THttpServerSocket = class(THttpSocket)
  protected
    fMethod: RawUtf8;
    fURL: RawUtf8;
    fKeepAliveClient: boolean;
    fRemoteConnectionID: THttpServerConnectionID;
    fServer: THttpServer;
  public
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    // - once created, caller should call AcceptRequest() to accept the socket
    constructor Create(aServer: THttpServer); reintroduce;
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if withBody=true and ContentLength<>0)
    // - returned enumeration will indicates the processing state
    function GetRequest(withBody: boolean;
      headerMaxTix: Int64): THttpServerSocketGetRequestResult; virtual;
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    property Method: RawUtf8
      read fMethod;
    /// contains the URL ('/' e.g.) after GetRequest()
    property URL: RawUtf8
      read fURL;
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
  end;

  /// HTTP response Thread as used by THttpServer Socket API based class
  // - Execute procedure get the request and calculate the answer, using
  // the thread for a single client connection, until it is closed
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TSynThread)
  protected
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    fClientSock: TNetSocket;
    fClientSin: TNetAddr;
    fConnectionID: THttpServerConnectionID;
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
    procedure Task(aCaller: TSynThread; aContext: Pointer); override;
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

  /// event handler used by THttpServer.Process to send a local file
  // when STATICFILE_CONTENT_TYPE content-type is returned by the service
  // - can be defined e.g. to use NGINX X-Accel-Redirect header
  // - should return true if the Context has been modified to serve the file, or
  // false so that the file will be manually read and sent from memory
  // - any exception during process will be returned as a HTTP_NOTFOUND page
  TOnHttpServerSendFile = function(Context: THttpServerRequest;
    const LocalFileName: TFileName): boolean of object;

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
  THttpServer = class(THttpServerGeneric)
  protected
    /// used to protect Process() call, e.g. fInternalHttpServerRespList
    fProcessCS: TRTLCriticalSection;
    fHeaderRetrieveAbortDelay: integer;
    fThreadPool: TSynThreadPoolTHttpServer;
    fInternalHttpServerRespList: TSynList;
    fServerConnectionCount: integer;
    fServerConnectionActive: integer;
    fServerKeepAliveTimeOut: cardinal;
    fSockPort: RawUtf8;
    fSock: TCrtSocket;
    fOnSendFile: TOnHttpServerSendFile;
    fNginxSendFileFrom: array of TFileName;
    fHttpQueueLength: cardinal;
    fExecuteState: (esNotStarted, esBinding, esRunning, esFinished);
    fStats: array[THttpServerSocketGetRequestResult] of integer;
    fSocketClass: THttpServerSocketClass;
    fThreadRespClass: THttpServerRespClass;
    fHeadersUnFiltered: boolean;
    fExecuteMessage: string;
    function GetStat(one: THttpServerSocketGetRequestResult): integer;
    function GetHttpQueueLength: cardinal; override;
    procedure SetHttpQueueLength(aValue: cardinal); override;
    procedure InternalHttpServerRespListAdd(resp: THttpServerResp);
    procedure InternalHttpServerRespListRemove(resp: THttpServerResp);
    function OnNginxAllowSend(Context: THttpServerRequest;
      const LocalFileName: TFileName): boolean;
    // this overridden version will return e.g. 'Winsock 2.514'
    function GetApiVersion: RawUtf8; override;
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
      const OnStart, OnStop: TOnNotifyThread;
      const ProcessName: RawUtf8; ServerThreadPoolCount: integer = 32;
      KeepAliveTimeOut: integer = 30000; aHeadersUnFiltered: boolean = false;
      CreateSuspended: boolean = false); reintroduce; virtual;
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
    procedure WaitStarted(Seconds: integer = 30); virtual;
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
    /// release all memory and handlers
    destructor Destroy; override;
    /// by default, only relevant headers are added to internal headers list
    // - for instance, Content-Length, Content-Type and Content-Encoding are
    // stored as fields in this THttpSocket, but not included in its Headers[]
    // - set this property to true to include all incoming headers
    property HeadersUnFiltered: boolean
      read fHeadersUnFiltered;
    /// access to the main server low-level Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - THttpServerSocket are created on the fly for every request, then
    // a THttpServerResp thread is created for handling this THttpServerSocket
    property Sock: TCrtSocket
      read fSock;
    /// custom event handler used to send a local file for STATICFILE_CONTENT_TYPE
    // - see also NginxSendFileFrom() method
    property OnSendFile: TOnHttpServerSendFile
      read fOnSendFile write fOnSendFile;
  published
    /// will contain the current number of connections to the server
    property ServerConnectionActive: integer
      read fServerConnectionActive write fServerConnectionActive;
    /// will contain the total number of connections to the server
    // - it's the global count since the server started
    property ServerConnectionCount: integer
      read fServerConnectionCount write fServerConnectionCount;
    /// time, in milliseconds, for the HTTP/1.1 connections to be kept alive
    // - default is 30000 ms, i.e. 30 seconds
    // - setting 0 here (or in KeepAliveTimeOut constructor parameter) will
    // disable keep-alive, and fallback to HTTP.1/0 for all incoming requests
    // (may be a good idea e.g. behind a NGINX reverse proxy)
    // - see THttpApiServer.SetTimeOutLimits(aIdleConnection) parameter
    property ServerKeepAliveTimeOut: cardinal
      read fServerKeepAliveTimeOut write fServerKeepAliveTimeOut;
    /// the bound TCP port, as specified to Create() constructor
    // - TCrtSocket.Bind() occurs in the Execute method
    property SockPort: RawUtf8
      read fSockPort;
    /// the associated thread pool
    // - may be nil if ServerThreadPoolCount was 0 on constructor
    property ThreadPool: TSynThreadPoolTHttpServer
      read fThreadPool;
    /// milliseconds delay to reject a connection due to too long header retrieval
    // - default is 0, i.e. not checked (typically not needed behind a reverse proxy)
    property HeaderRetrieveAbortDelay: integer
      read fHeaderRetrieveAbortDelay write fHeaderRetrieveAbortDelay;
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
    /// how many HTTP connections were passed to an asynchronous handler
    // - e.g. for background WebSockets processing after proper upgrade
    property StatOwnedConnections: integer
      index grOwned read GetStat;
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
  // HTTP responses. The HTTP Server API includes SSL support so that applications
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
    fLogDataStorage: array of byte;
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
    // then call explicitely the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(CreateSuspended: boolean; QueueName: SynUnicode = '';
      const OnStart: TOnNotifyThread = nil; const OnStop: TOnNotifyThread = nil;
      const ProcessName: RawUtf8 = ''); reintroduce;
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
    constructor Create(CreateSuspended: boolean; aSocketThreadsCount: integer = 1;
      aPingTimeout: integer = 0; const QueueName: SynUnicode = '';
      const aOnWSThreadStart: TOnNotifyThread=nil;
      const aOnWSThreadTerminate: TOnNotifyThread=nil); reintroduce;
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
    procedure Task(aCaller: TSynThread; aContext: Pointer); override;
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

constructor THttpServerRequest.Create(aServer: THttpServerGeneric;
  aConnectionID: THttpServerConnectionID; aConnectionThread: TSynThread;
  aConnectionFlags: THttpServerRequestFlags);
begin
  inherited Create;
  fServer := aServer;
  fConnectionID := aConnectionID;
  fConnectionThread := aConnectionThread;
  fConnectionFlags := aConnectionFlags;
end;

var
  // global request counter if no THttpServer is defined
  GlobalRequestID: integer;

procedure THttpServerRequest.Prepare(const aURL, aMethod, aInHeaders: RawUtf8;
  const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8);
var
  id: PInteger;
begin
  if fServer = nil then
    id := @GlobalRequestID
  else
    id := @fServer.fCurrentRequestID;
  fRequestID := InterLockedIncrement(id^);
  if fRequestID = maxInt - 2048 then // ensure no overflow (31-bit range)
    id^ := 0;
  fURL := aURL;
  fMethod := aMethod;
  fRemoteIP := aRemoteIP;
  if aRemoteIP <> '' then
    if aInHeaders = '' then
      fInHeaders := 'RemoteIP: ' + aRemoteIP
    else
      fInHeaders := aInHeaders + #13#10'RemoteIP: ' + aRemoteIP
  else
    fInHeaders := aInHeaders;
  fInContent := aInContent;
  fInContentType := aInContentType;
  fOutContent := '';
  fOutContentType := '';
  fOutCustomHeaders := '';
end;



{ TServerGeneric }

constructor TServerGeneric.Create(CreateSuspended: boolean;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8);
begin
  fProcessName := ProcessName;
  fOnHttpThreadStart := OnStart;
  SetOnTerminate(OnStop);
  inherited Create(CreateSuspended);
end;

procedure TServerGeneric.NotifyThreadStart(Sender: TSynThread);
begin
  if Sender = nil then
    raise EHttpServer.CreateUtf8('%.NotifyThreadStart(nil)', [self]);
  if Assigned(fOnHttpThreadStart) and
     not Assigned(Sender.StartNotified) then
  begin
    fOnHttpThreadStart(Sender);
    Sender.StartNotified := self;
  end;
end;

procedure TServerGeneric.SetOnTerminate(const Event: TOnNotifyThread);
begin
  fOnThreadTerminate := Event;
end;


{ THttpServerGeneric }

constructor THttpServerGeneric.Create(CreateSuspended: boolean;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8);
begin
  SetServerName('mORMot2 (' + OS_TEXT + ')');
  inherited Create(CreateSuspended, OnStart, OnStop, ProcessName);
end;

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer);
begin
  RegisterCompressFunc(fCompress, aFunction, fCompressAcceptEncoding, aCompressMinSize);
end;

procedure THttpServerGeneric.Shutdown;
begin
  if self <> nil then
    fShutdownInProgress := true;
end;

function THttpServerGeneric.Request(Ctxt: THttpServerRequestAbstract): cardinal;
var
  thrd: TSynThread;
begin
  if (self = nil) or
     fShutdownInProgress then
    result := HTTP_NOTFOUND
  else
  begin
    thrd := THttpServerRequest(Ctxt).ConnectionThread;
    if not Assigned(thrd.StartNotified) then
      NotifyThreadStart(thrd);
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
    fOnAfterResponse(Ctxt, Code);
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


{ ******************** THttpServerSocket/THttpServer HTTP/1.1 Server }

{ THttpServer }

constructor THttpServer.Create(const aPort: RawUtf8;
  const OnStart, OnStop: TOnNotifyThread; const ProcessName: RawUtf8;
  ServerThreadPoolCount, KeepAliveTimeOut: integer;
  aHeadersUnFiltered, CreateSuspended: boolean);
begin
  fSockPort := aPort;
  fInternalHttpServerRespList := TSynList.Create;
  InitializeCriticalSection(fProcessCS);
  fServerKeepAliveTimeOut := KeepAliveTimeOut; // 30 seconds by default
  if fThreadPool <> nil then
    fThreadPool.ContentionAbortDelay := 5000; // 5 seconds default
  // event handlers set before inherited Create to be visible in childs
  fOnHttpThreadStart := OnStart;
  SetOnTerminate(OnStop);
  if fThreadRespClass = nil then
    fThreadRespClass := THttpServerResp;
  if fSocketClass = nil then
    fSocketClass := THttpServerSocket;
  fProcessName := ProcessName; // TSynThreadPoolTHttpServer needs it now
  if ServerThreadPoolCount > 0 then
  begin
    fThreadPool := TSynThreadPoolTHttpServer.Create(self, ServerThreadPoolCount);
    fHttpQueueLength := 1000;
  end;
  fHeadersUnFiltered := aHeadersUnFiltered;
  inherited Create(CreateSuspended, OnStart, OnStop, ProcessName);
end;

function THttpServer.GetApiVersion: RawUtf8;
begin
  result := SocketApiVersion;
end;

destructor THttpServer.Destroy;
var
  endtix: Int64;
  i: PtrInt;
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
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
       {dobind=}false, 10, 10, 10, 0, callback) = nrOK then
      // Windows TCP/UDP socket may not release Accept() until connected
      callback.ShutdownAndClose({rdwr=}false);
    if Sock.SockIsDefined then
      Sock.Close; // nlUnix expects shutdown after accept() returned
  end;
  endtix := mormot.core.os.GetTickCount64 + 20000;
  EnterCriticalSection(fProcessCS);
  try
    if fInternalHttpServerRespList <> nil then
    begin
      for i := 0 to fInternalHttpServerRespList.Count - 1 do
        THttpServerResp(fInternalHttpServerRespList.List[i]).Shutdown;
      repeat
        // wait for all THttpServerResp.Execute to be finished
        if (fInternalHttpServerRespList.Count = 0) and
           (fExecuteState <> esRunning) then
          break;
        LeaveCriticalSection(fProcessCS);
        SleepHiRes(10);
        EnterCriticalSection(fProcessCS);
      until mormot.core.os.GetTickCount64 > endtix;
      FreeAndNil(fInternalHttpServerRespList);
    end;
  finally
    LeaveCriticalSection(fProcessCS);
    FreeAndNil(fThreadPool); // release all associated threads and I/O completion
    FreeAndNil(fSock);
    inherited Destroy;       // direct Thread abort, no wait till ended
    DeleteCriticalSection(fProcessCS);
  end;
end;

function THttpServer.GetStat(one: THttpServerSocketGetRequestResult): integer;
begin
  result := fStats[one];
end;

function THttpServer.GetHttpQueueLength: cardinal;
begin
  result := fHttpQueueLength;
end;

procedure THttpServer.SetHttpQueueLength(aValue: cardinal);
begin
  fHttpQueueLength := aValue;
end;

procedure THttpServer.InternalHttpServerRespListAdd(resp: THttpServerResp);
begin
  if (self = nil) or
     (fInternalHttpServerRespList = nil) or
     (resp = nil) then
    exit;
  EnterCriticalSection(fProcessCS);
  try
    fInternalHttpServerRespList.Add(resp);
  finally
    LeaveCriticalSection(fProcessCS);
  end;
end;

procedure THttpServer.InternalHttpServerRespListRemove(resp: THttpServerResp);
var
  i: integer;
begin
  if (self = nil) or
     (fInternalHttpServerRespList = nil) then
    exit;
  EnterCriticalSection(fProcessCS);
  try
    i := fInternalHttpServerRespList.IndexOf(resp);
    if i >= 0 then
      fInternalHttpServerRespList.Delete(i);
  finally
    LeaveCriticalSection(fProcessCS);
  end;
end;

function THttpServer.OnNginxAllowSend(Context: THttpServerRequest;
  const LocalFileName: TFileName): boolean;
var
  match, i, f: PtrInt;
  folder: ^TFileName;
begin
  match := 0;
  folder := pointer(fNginxSendFileFrom);
  if LocalFileName <> '' then
    for f := 1 to length(fNginxSendFileFrom) do
    begin
      match := length(folder^);
      for i := 1 to match do // case sensitive left search
        if LocalFileName[i] <> folder^[i] then
        begin
          match := 0;
          break;
        end;
      if match <> 0 then
        break; // found matching folder
      inc(folder);
    end;
  result := match <> 0;
  if not result then
    exit; // no match -> manual send
  delete(Context.fOutContent, 1, match); // remove e.g. '/var/www'
  Context.OutCustomHeaders := TrimU(Context.OutCustomHeaders + #13#10 +
    'X-Accel-Redirect: ' + Context.OutContent);
  Context.OutContent := '';
end;

procedure THttpServer.NginxSendFileFrom(const FileNameLeftTrim: TFileName);
var
  n: PtrInt;
begin
  n := length(fNginxSendFileFrom);
  SetLength(fNginxSendFileFrom, n + 1);
  fNginxSendFileFrom[n] := FileNameLeftTrim;
  fOnSendFile := OnNginxAllowSend;
end;

procedure THttpServer.WaitStarted(Seconds: integer);
var
  tix: Int64;
  ok: boolean;
begin
  tix := mormot.core.os.GetTickCount64 + Seconds * 1000; // never wait forever
  repeat
    EnterCriticalSection(fProcessCS);
    ok := Terminated or
          (fExecuteState in [esRunning, esFinished]);
    LeaveCriticalSection(fProcessCS);
    if ok then
      exit;
    Sleep(1); // warning: waits typically 1-15 ms on Windows
    if mormot.core.os.GetTickCount64 > tix then
      raise EHttpServer.CreateUtf8('%.WaitStarted timeout after % seconds [%]',
        [self, Seconds, fExecuteMessage]);
  until false;
end;

{.$define MONOTHREAD}
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
    fSock := TCrtSocket.Bind(fSockPort); // BIND + LISTEN
    {$ifdef OSLINUX}
    // in case was started by systemd, listening socket is created by another
    // process and do not interrupt while process got a signal. So we need to
    // set a timeout to unlock accept() periodically and check for termination
    if fSockPort = '' then // external socket
      fSock.ReceiveTimeout := 1000; // unblock accept every second
    {$endif OSLINUX}
    fExecuteState := esRunning;
    if not fSock.SockIsDefined then // paranoid (Bind would have raise an exception)
      raise EHttpServer.CreateUtf8('%.Execute: %.Bind failed', [self, fSock]);
    while not Terminated do
    begin
      res := Sock.Sock.Accept(cltsock, cltaddr);
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
      cltservsock := fSocketClass.Create(self);
      try
        cltservsock.InitRequest(cltsock);
        endtix := fHeaderRetrieveAbortDelay;
        if endtix > 0 then
          inc(endtix, mormot.core.os.GetTickCount64);
        if cltservsock.GetRequest({withbody=}true, endtix)
            in [grBodyReceived, grHeaderReceived] then
          Process(cltservsock, 0, self);
        OnDisconnect;
        DirectShutdown(cltsock);
      finally
        cltservsock.Free;
      end;
      {$else}
      if Assigned(fThreadPool) then
      begin
        // use thread pool to process the request header, and probably its body
        cltservsock := fSocketClass.Create(self);
        // we tried to reuse the fSocketClass instance -> no performance change
        cltservsock.AcceptRequest(cltsock, @cltaddr);
        if not fThreadPool.Push(pointer(PtrUInt(cltservsock)),
            {waitoncontention=}true) then
        begin
          // returned false if there is no idle thread in the pool, and queue is full
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
      fExecuteMessage := E.ClassName + ' [' + E.Message + ']';
  end;
  EnterCriticalSection(fProcessCS);
  fExecuteState := esFinished;
  LeaveCriticalSection(fProcessCS);
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

function IdemPCharNotVoid(p: PByteArray; up: PByte; toup: PByteArray): boolean;
  {$ifdef HASINLINE}inline;{$endif}
var
  u: byte;
begin
  // slightly more efficient than plain IdemPChar() - we don't check p/up=nil
  result := false;
  dec(PtrUInt(p), PtrUInt(up));
  repeat
    u := up^;
    if u = 0 then
      break;
    if toup[p[PtrUInt(up)]] <> u then
      exit;
    inc(up);
  until false;
  result := true;
end;

procedure ExtractNameValue(var headers: RawUtf8; const upname: RawUtf8;
  out res: RawUtf8);
var
  i, j, k: PtrInt;
begin
  if (headers = '') or
      (upname = '') then
    exit;
  i := 1;
  repeat
    k := length(headers) + 1;
    for j := i to k - 1 do
      if headers[j] < ' ' then
      begin
        k := j;
        break;
      end;
    if IdemPCharNotVoid(@PByteArray(headers)[i - 1], pointer(upname), @NormToUpper) then
    begin
      j := i;
      inc(i, length(upname));
      TrimCopy(headers, i, k - i, res);
      while true do // delete also ending #13#10
        if (headers[k] = #0) or
           (headers[k] >= ' ') then
          break
        else
          inc(k);
      delete(headers, j, k - j);
      exit;
    end;
    i := k;
    while headers[i] < ' ' do
      if headers[i] = #0 then
        exit
      else
        inc(i);
  until false;
end;

const
  // accessed only in the HTTP context over Sockets, not WebSockets
  // - currently, THttpServerSocket.TLS.Enabled is never set
  // - Windows http.sys will directly set the flags
  HTTPREMOTEFLAGS: array[{tls=}boolean] of THttpServerRequestFlags = (
    [], [hsrHttps, hsrSecured]);

procedure THttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var
  ctxt: THttpServerRequest;
  respsent: boolean;
  cod, aftercode: cardinal;
  reason: RawUtf8;
  errmsg: string;

  function SendFileAsResponse: boolean;
  var
    fn: TFileName;
  begin
    result := true;
    ExtractNameValue(ctxt.fOutCustomHeaders, 'CONTENT-TYPE:', ctxt.fOutContentType);
    fn := Utf8ToString(ctxt.OutContent);
    if not Assigned(fOnSendFile) or
       not fOnSendFile(ctxt, fn) then
    begin
       ctxt.OutContent := StringFromFile(fn);
       if ctxt.OutContent = '' then
       begin
         FormatString('Impossible to send void file: %', [fn], errmsg);
         cod := HTTP_NOTFOUND;
         result := false; // fatal error
       end;
    end;
  end;

  function SendResponse: boolean;
  var
    P, PEnd: PUtf8Char;
    len: PtrInt;
  begin
    result := not Terminated; // true=success
    if not result then
      exit;
    {$ifdef SYNCRTDEBUGLOW}
    TSynLog.Add.Log(sllCustom2, 'SendResponse respsent=% cod=%',
      [respsent, cod], self);
    {$endif SYNCRTDEBUGLOW}
    respsent := true;
    // handle case of direct sending of static file (as with http.sys)
    if (ctxt.OutContent <> '') and
       (ctxt.OutContentType = STATICFILE_CONTENT_TYPE) then
      result := SendFileAsResponse
    else if ctxt.OutContentType = NORESPONSE_CONTENT_TYPE then
      ctxt.OutContentType := ''; // true HTTP always expects a response
    // send response (multi-thread OK) at once
    if (cod < HTTP_SUCCESS) or
       (ClientSock.Headers = '') then
      cod := HTTP_NOTFOUND;
    StatusCodeToReason(cod, reason);
    if errmsg <> '' then
    begin
      ctxt.OutCustomHeaders := '';
      ctxt.OutContentType := 'text/html; charset=utf-8'; // create message to display
      ctxt.OutContent := FormatUtf8('<body style="font-family:verdana">'#10 +
        '<h1>% Server Error %</h1><hr><p>HTTP % %<p>%<p><small>%',
        [self, cod, cod, reason, HtmlEscapeString(errmsg), fServerName]);
    end;
    // 1. send HTTP status command
    if ClientSock.KeepAliveClient then
      ClientSock.SockSend(['HTTP/1.1 ', cod, ' ', reason])
    else
      ClientSock.SockSend(['HTTP/1.0 ', cod, ' ', reason]);
    // 2. send headers
    // 2.1. custom headers from Request() method
    P := pointer(ctxt.fOutCustomHeaders);
    if P <> nil then
    begin
      PEnd := P + length(ctxt.fOutCustomHeaders);
      repeat
        len := BufferLineLength(P, PEnd);
        if len > 0 then
        begin
          // no void line (means headers ending)
          if IdemPChar(P, 'CONTENT-ENCODING:') then
            // custom encoding: don't compress
            integer(ClientSock.fCompressAcceptHeader) := 0;
          ClientSock.SockSend(P, len);
          ClientSock.SockSendCRLF;
          inc(P, len);
        end;
        while P^ in [#10, #13] do
          inc(P);
      until P^ = #0;
    end;
    // 2.2. generic headers
    ClientSock.SockSend([
      {$ifndef NOXPOWEREDNAME}
      XPOWEREDNAME + ': ' + XPOWEREDVALUE + #13#10 +
      {$endif NOXPOWEREDNAME}
      'Server: ', fServerName]);
    ClientSock.CompressDataAndWriteHeaders(
      ctxt.OutContentType, ctxt.fOutContent, nil);
    if ClientSock.KeepAliveClient then
    begin
      if ClientSock.fCompressAcceptEncoding <> '' then
        ClientSock.SockSend(ClientSock.fCompressAcceptEncoding);
      ClientSock.SockSend('Connection: Keep-Alive'#13#10); // #13#10 -> end headers
    end
    else
      ClientSock.SockSendCRLF; // headers must end with a void line
    // 3. sent HTTP body content (if any)
    ClientSock.SockSendFlush(ctxt.OutContent); // flush all data to network
  end;

begin
  if (ClientSock = nil) or
     (ClientSock.Headers = '') then
    // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  if Terminated then
    exit;
  ctxt := THttpServerRequest.Create(
    self, ConnectionID, ConnectionThread, HTTPREMOTEFLAGS[ClientSock.TLS.Enabled]);
  try
    respsent := false;
    with ClientSock do
      ctxt.Prepare(URL, Method, HeaderGetText(fRemoteIP), Content, ContentType, '');
    try
      cod := DoBeforeRequest(ctxt);
      if cod > 0 then
      begin
        {$ifdef SYNCRTDEBUGLOW}
        TSynLog.Add.Log(sllCustom2, 'DoBeforeRequest=%', [cod], self);
        {$endif SYNCRTDEBUGLOW}
        if not SendResponse or
           (cod <> HTTP_ACCEPTED) then
          exit;
      end;
      cod := Request(ctxt); // this is the main processing callback
      aftercode := DoAfterRequest(ctxt);
      {$ifdef SYNCRTDEBUGLOW}
      TSynLog.Add.Log(sllCustom2, 'Request=% DoAfterRequest=%', [cod, aftercode], self);
      {$endif SYNCRTDEBUGLOW}
      if aftercode > 0 then
        cod := aftercode;
      if respsent or
         SendResponse then
        DoAfterResponse(ctxt, cod);
      {$ifdef SYNCRTDEBUGLOW}
      TSynLog.Add.Log(sllCustom2, 'DoAfterResponse respsent=% errmsg=%',
        [respsent, errmsg], self);
      {$endif SYNCRTDEBUGLOW}
    except
      on E: Exception do
        if not respsent then
        begin
          // notify the exception as server response
          FormatString('%: %', [E, E.Message], errmsg);
          cod := HTTP_SERVERERROR;
          SendResponse;
        end;
    end;
  finally
    // add transfert stats to main socket
    if Sock <> nil then
    begin
      EnterCriticalSection(fProcessCS);
      Sock.BytesIn := Sock.BytesIn + ClientSock.BytesIn;
      Sock.BytesOut := Sock.BytesOut + ClientSock.BytesOut;
      LeaveCriticalSection(fProcessCS);
      ClientSock.fBytesIn := 0;
      ClientSock.fBytesOut := 0;
    end;
    ctxt.Free;
  end;
end;



{ THttpServerSocket }

constructor THttpServerSocket.Create(aServer: THttpServer);
begin
  inherited Create(5000);
  if aServer <> nil then // nil e.g. from TRtspOverHttpServer
  begin
    fServer := aServer;
    fCompress := aServer.fCompress;
    fCompressAcceptEncoding := aServer.fCompressAcceptEncoding;
    fSocketLayer := aServer.Sock.SocketLayer;
    TLS.Enabled := aServer.Sock.TLS.Enabled; // not implemented yet
    OnLog := aServer.Sock.OnLog;
  end;
end;

function THttpServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
var
  P: PUtf8Char;
  status: cardinal;
  pending: integer;
  reason, allheaders: RawUtf8;
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
      noheaderfilter := fServer.HeadersUnFiltered;
    end
    else
      noheaderfilter := false;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Command);
    P := pointer(Command);
    if P = nil then
      exit; // broken
    GetNextItem(P, ' ', fMethod); // 'GET'
    GetNextItem(P, ' ', fURL);    // '/path'
    fKeepAliveClient := ((fServer = nil) or
                         (fServer.ServerKeepAliveTimeOut > 0)) and
                        IdemPChar(P, 'HTTP/1.1');
    Content := '';
    // get headers and content
    GetHeader(noheaderfilter);
    if fServer <> nil then // = nil e.g. from TRtspOverHttpServer
    begin
      if fServer.fRemoteIPHeaderUpper <> '' then
        // real Internet IP (replace 127.0.0.1 from a proxy)
        FindNameValue(headers, pointer(fServer.fRemoteIPHeaderUpper),
          fRemoteIP, {keepnotfound=}true);
      if fServer.fRemoteConnIDHeaderUpper <> '' then
      begin
        P := FindNameValue(pointer(headers), pointer(fServer.fRemoteConnIDHeaderUpper));
        if P <> nil then
          SetQWord(P, PQWord(@fRemoteConnectionID)^);
      end;
    end;
    if hfConnectionClose in HeaderFlags then
      fKeepAliveClient := false;
    if (ContentLength < 0) and
       (KeepAliveClient or
       (fMethod = 'GET')) then
      ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    if (headerMaxTix > 0) and
       (GetTickCount64 > headerMaxTix) then
    begin
      result := grTimeout;
      exit; // allow 10 sec for header -> DOS/TCPSYN Flood
    end;
    if fServer <> nil then
    begin
      if (ContentLength > 0) and
         (fServer.MaximumAllowedContentLength > 0) and
         (cardinal(ContentLength) > fServer.MaximumAllowedContentLength) then
      begin
        SockSend('HTTP/1.0 413 Payload Too Large'#13#10#13#10'Rejected');
        SockSendFlush('');
        result := grOversizedPayload;
        exit;
      end;
      if Assigned(fServer.OnBeforeBody) then
      begin
        allheaders := HeaderGetText(fRemoteIP);
        status := fServer.OnBeforeBody(fURL, fMethod, allheaders, ContentType,
          fRemoteIP, BearerToken, ContentLength, HTTPREMOTEFLAGS[TLS.Enabled]);
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
    if withBody and
       not (hfConnectionUpgrade in HeaderFlags) then
    begin
      if IdemPCharArray(pointer(fMethod), ['HEAD', 'OPTIONS']) < 0 then
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
  fServer.InternalHttpServerRespListAdd(self);
  fConnectionID := aServerSock.RemoteConnectionID;
  if fConnectionID = 0 then
    fConnectionID := fServer.NextConnectionID; // fallback to 31-bit sequence
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure THttpServerResp.Shutdown;
begin
  Terminate;
  fServerSock.Sock.ShutdownAndClose({rdwr=}true);
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
        beforetix := mormot.core.os.GetTickCount64;
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
                tix := mormot.core.os.GetTickCount64;
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
                LockedInc32(@fServer.fStats[res]);
                case res of
                  grBodyReceived, grHeaderReceived:
                    begin
                      if res = grBodyReceived then
                        LockedInc32(@fServer.fStats[grHeaderReceived]);
                      // calc answer and send response
                      fServer.Process(fServerSock, ConnectionID, self);
                      // keep connection only if necessary
                      if fServerSock.KeepAliveClient then
                        break
                      else
                        exit;
                    end;
                  grOwned:
                    begin
                      fServerSock := nil; // will be freed by new owner
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
           (IdemPCharArray(pointer(fServerSock.fMethod), ['HEAD', 'OPTIONS']) < 0) then
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
        finally
          fServer.InternalHttpServerRespListRemove(self);
          fServer := nil;
        end;
      finally
        FreeAndNil(fServerSock);
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

procedure TSynThreadPoolTHttpServer.Task(aCaller: TSynThread; aContext: Pointer);
var
  srvsock: THttpServerSocket;
  headertix: Int64;
  res: THttpServerSocketGetRequestResult;
begin
  srvsock := aContext;
  try
    if fServer.Terminated then
      exit;
    // get Header of incoming request in the thread pool
    headertix := fServer.HeaderRetrieveAbortDelay;
    if headertix > 0 then
      headertix := headertix + GetTickCount64;
    res := srvsock.GetRequest({withbody=}false, headertix);
    if (fServer = nil) or
       fServer.Terminated then
      exit;
    // properly get the incoming body and process the request
    LockedInc32(@fServer.fStats[res]);
    case res of
      grHeaderReceived:
        begin
          // connection and header seem valid -> process request further
          if (fServer.ServerKeepAliveTimeOut > 0) and
             (fServer.fInternalHttpServerRespList.Count < fMaxBodyThreadCount) and
             (srvsock.KeepAliveClient or
              (srvsock.ContentLength > fBigBodySize)) then
          begin
            // HTTP/1.1 Keep Alive (including WebSockets) or posted data > 16 MB
            // -> process in dedicated background thread
            fServer.fThreadRespClass.Create(srvsock, fServer);
            srvsock := nil; // THttpServerResp will own and free srvsock
          end
          else
          begin
            // no Keep Alive = multi-connection -> process in the Thread Pool
            if not (hfConnectionUpgrade in srvsock.HeaderFlags) and
               (IdemPCharArray(pointer(srvsock.fMethod), ['HEAD', 'OPTIONS']) < 0) then
            begin
              srvsock.GetBody; // we need to get it now
              LockedInc32(@fServer.fStats[grBodyReceived]);
            end;
            // multi-connection -> process now
            fServer.Process(srvsock, srvsock.RemoteConnectionID, aCaller);
            fServer.OnDisconnect;
            // no Shutdown here: will be done client-side
          end;
        end;
      grOwned:
        // e.g. for asynchrounous WebSockets
        srvsock := nil; // to ignore FreeAndNil(srvsock) below
    else if Assigned(fServer.Sock.OnLog) then
      fServer.Sock.OnLog(sllTrace, 'Task: close after GetRequest=% from %',
          [ToText(res)^, srvsock.RemoteIP], self);
    end;
  finally
    srvsock.Free;
  end;
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
        FillcharFast(cfg, sizeof(cfg), 0);
        cfg.KeyDesc.pUrlPrefix := pointer(prefix);
        // first delete any existing information
        err := Http.DeleteServiceConfiguration(
          0, hscUrlAclInfo, @cfg, Sizeof(cfg));
        // then add authorization rule
        if not OnlyDelete then
        begin
          cfg.KeyDesc.pUrlPrefix := pointer(prefix);
          cfg.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
          err := Http.SetServiceConfiguration(
            0, hscUrlAclInfo, @cfg, Sizeof(cfg));
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
     not Assigned(OnRequest) or
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

constructor THttpApiServer.Create(CreateSuspended: boolean;
  QueueName: SynUnicode; const OnStart, OnStop: TOnNotifyThread;
  const ProcessName: RawUtf8);
var
  binding: HTTP_BINDING_INFO;
begin
  SetLength(fLogDataStorage, sizeof(HTTP_LOG_FIELDS_DATA)); // should be done 1st
  inherited Create({suspended=}true, OnStart, OnStop, ProcessName);
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
  fReceiveBufferSize := 1048576; // i.e. 1 MB
  if not CreateSuspended then
    Suspended := False;
end;

constructor THttpApiServer.CreateClone(From: THttpApiServer);
begin
  SetLength(fLogDataStorage, sizeof(HTTP_LOG_FIELDS_DATA));
  fOwner := From;
  fReqQueue := From.fReqQueue;
  fOnRequest := From.fOnRequest;
  fOnBeforeBody := From.fOnBeforeBody;
  fOnBeforeRequest := From.fOnBeforeRequest;
  fOnAfterRequest := From.fOnAfterRequest;
  fCanNotifyCallback := From.fCanNotifyCallback;
  fCompress := From.fCompress;
  fCompressAcceptEncoding := From.fCompressAcceptEncoding;
  fReceiveBufferSize := From.fReceiveBufferSize;
  if From.fLogData <> nil then
    fLogData := pointer(fLogDataStorage);
  SetServerName(From.fServerName);
  SetRemoteIPHeader(From.RemoteIPHeader);
  SetRemoteConnIDHeader(From.RemoteConnIDHeader);
  fLoggingServiceName := From.fLoggingServiceName;
  inherited Create(false, From.fOnHttpThreadStart, From.fOnThreadTerminate, From.ProcessName);
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

procedure THttpApiServer.Execute;
type
  TVerbText = array[hvOPTIONS..pred(hvMaximum)] of RawUtf8;
const
  VERB_TEXT: TVerbText = (
    'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE',
    'TRACE', 'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
    'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH');
var
  req: PHTTP_REQUEST;
  reqid: HTTP_REQUEST_ID;
  reqbuf, respbuf: RawByteString;
  remoteip, remoteconn, token: RawUtf8;
  i, L: PtrInt;
  P: PHTTP_UNKNOWN_HEADER;
  flags, bytesread, bytessent: cardinal;
  err: HRESULT;
  compressset: THttpSocketCompressSet;
  incontlen, incontlenchunk, incontlenread: cardinal;
  incontenc, inaccept, range: RawUtf8;
  outcontenc, outstat: RawUtf8;
  outstatcode, afterstatcode: cardinal;
  respsent: boolean;
  ctxt: THttpServerRequest;
  filehandle: THandle;
  reps: PHTTP_RESPONSE;
  bufread, R: PUtf8Char;
  heads: HTTP_UNKNOWN_HEADERs;
  rangestart, rangelen: ULONGLONG;
  outcontlen: ULARGE_INTEGER;
  datachunkmem: HTTP_DATA_CHUNK_INMEMORY;
  datachunkfile: HTTP_DATA_CHUNK_FILEHANDLE;
  logdata: PHTTP_LOG_FIELDS_DATA;
  contrange: ShortString;
  vervs: TVerbText; // to avoid memory allocation

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
      reps^.SetContent(datachunkmem, msg + HtmlEscapeString(ErrorMsg)
        {$ifndef NOXPOWEREDNAME} + '</p><p><small>' + XPOWEREDVALUE {$endif},
        'text/html; charset=utf-8');
      Http.SendHttpResponse(fReqQueue, req^.RequestId, 0, reps^, nil,
        bytessent, nil, 0, nil, fLogData);
    except
      on Exception do
        ; // ignore any HttpApi level errors here (client may crashed)
    end;
  end;

  function SendResponse: boolean;
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
        ClientIp := pointer(remoteip);
        ClientIpLength := length(remoteip);
        Method := pointer(ctxt.fMethod);
        MethodLength := length(ctxt.fMethod);
        UserName := pointer(ctxt.fAuthenticatedUser);
        UserNameLength := Length(ctxt.fAuthenticatedUser);
      end;
    // send response
    reps^.Version := req^.Version;
    reps^.SetHeaders(pointer(ctxt.OutCustomHeaders), heads);
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
            outcontenc := CompressDataAndGetHeaders(compressset,
              fCompress, ctxt.OutContentType, ctxt.fOutContent);
            pRawValue := pointer(outcontenc);
            RawValueLength := length(outcontenc);
          end;
      end;
      reps^.SetContent(datachunkmem, ctxt.OutContent, ctxt.OutContentType);
      EHttpApiServer.RaiseOnError(hSendHttpResponse,
        Http.SendHttpResponse(fReqQueue, req^.RequestId,
          getSendResponseFlags(ctxt), reps^, nil,
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
    SetLength(respbuf, sizeof(HTTP_RESPONSE));
    reps := pointer(respbuf);
    SetLength(reqbuf, 16384 + sizeof(HTTP_REQUEST)); // req^ + 16 KB of headers
    req := pointer(reqbuf);
    logdata := pointer(fLogDataStorage);
    vervs := VERB_TEXT;
    ctxt := THttpServerRequest.Create(self, 0, self, []);
    // main loop reusing a single ctxt instance for this thread
    reqid := 0;
    ctxt.fServer := self;
    repeat
      // release input/output body buffers ASAP
      ctxt.fInContent := '';
      ctxt.fOutContent := '';
      // reset authentication status & user between requests
      ctxt.fAuthenticationStatus := hraNone;
      ctxt.fAuthenticatedUser := '';
      // retrieve next pending request, and read its headers
      FillcharFast(req^, sizeof(HTTP_REQUEST), 0);
      err := Http.ReceiveHttpRequest(fReqQueue, reqid, 0,
        req^, length(reqbuf), bytesread);
      if Terminated then
        break;
      case err of
        NO_ERROR:
          try
            // parse method and headers
            ctxt.fConnectionID := req^.ConnectionID;
            ctxt.fHttpApiRequest := req;
            SetString(ctxt.fFullURL, req^.CookedUrl.pFullUrl, req^.CookedUrl.FullUrlLength);
            FastSetString(ctxt.fURL, req^.pRawUrl, req^.RawUrlLength);
            if req^.Verb in [low(vervs)..high(vervs)] then
              ctxt.fMethod := vervs[req^.Verb]
            else
              FastSetString(ctxt.fMethod, req^.pUnknownVerb, req^.UnknownVerbLength);
            with req^.headers.KnownHeaders[reqContentType] do
              FastSetString(ctxt.fInContentType, pRawValue, RawValueLength);
            with req^.headers.KnownHeaders[reqAcceptEncoding] do
              FastSetString(inaccept, pRawValue, RawValueLength);
            compressset := ComputeContentEncoding(fCompress, pointer(inaccept));
            if req^.pSslInfo <> nil then
              ctxt.ConnectionFlags := [hsrHttps, hsrSecured];
            ctxt.fInHeaders := RetrieveHeaders(req^, fRemoteIPHeaderUpper, remoteip);
            // compute remote connection ID
            L := length(fRemoteConnIDHeaderUpper);
            if L <> 0 then
            begin
              P := req^.headers.pUnknownHeaders;
              if P <> nil then
                for i := 1 to req^.headers.UnknownHeaderCount do
                  if (P^.NameLength = L) and
                     IdemPChar(P^.pName, Pointer(fRemoteConnIDHeaderUpper)) then
                  begin
                    FastSetString(remoteconn, P^.pRawValue, P^.RawValueLength); // need #0 end
                    R := pointer(remoteconn);
                    ctxt.fConnectionID := GetNextNumber(R);
                    break;
                  end
                  else
                    inc(P);
            end;
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
                            GetDomainUserNameFromToken(AccessToken, ctxt.fAuthenticatedUser);
                            // Per spec https://docs.microsoft.com/en-us/windows/win32/http/authentication-in-http-version-2-0
                            // AccessToken lifecycle is application responsability and should be closed after use
                            CloseHandle(AccessToken);
                          end;
                        end;
                      HttpAuthStatusFailure:
                        ctxt.fAuthenticationStatus := hraFailed;
                    end;
            with req^.headers.KnownHeaders[reqContentLength] do
              incontlen := GetCardinal(
                pointer(pRawValue), pointer(pRawValue + RawValueLength));
            if (incontlen > 0) and
               (MaximumAllowedContentLength > 0) and
               (incontlen > MaximumAllowedContentLength) then
            begin
              SendError(HTTP_PAYLOADTOOLARGE, 'Rejected');
              continue;
            end;
            if Assigned(OnBeforeBody) then
            begin
              with req^.Headers.KnownHeaders[reqAuthorization] do
                if (RawValueLength > 7) and
                   IdemPChar(pointer(pRawValue), 'BEARER ') then
                  FastSetString(token, pRawValue + 7, RawValueLength - 7)
                else
                  token := '';
              err := OnBeforeBody(ctxt.fURL, ctxt.fMethod, ctxt.fInHeaders,
                ctxt.fInContentType, remoteip, token, incontlen, ctxt.ConnectionFlags);
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
                SetLength(ctxt.fInContent, incontlen);
                bufread := pointer(ctxt.InContent);
                incontlenread := 0;
                repeat
                  bytesread := 0;
                  if Http.Version.MajorVersion > 1 then // speed optimization for Vista+
                    flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER
                  else
                    flags := 0;
                  incontlenchunk := incontlen - incontlenread;
                  if (fReceiveBufferSize >= 1024) and
                     (incontlenchunk > fReceiveBufferSize) then
                    incontlenchunk := fReceiveBufferSize;
                  err := Http.ReceiveRequestEntityBody(fReqQueue, req^.RequestId,
                    flags, bufread, incontlenchunk, bytesread);
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
              ctxt.OutContent := '';
              ctxt.OutContentType := '';
              ctxt.OutCustomHeaders := '';
              FillcharFast(reps^, sizeof(reps^), 0);
              respsent := false;
              outstatcode := DoBeforeRequest(ctxt);
              if outstatcode > 0 then
                if not SendResponse or
                   (outstatcode <> HTTP_ACCEPTED) then
                  continue;
              outstatcode := Request(ctxt);
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
                    (EHttpApiServer(E).LastError <> HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
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
          @result, sizeof(result), 0, @len, nil));
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
        @aValue, sizeof(aValue), 0, nil));
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
     not Assigned(aOnFragment) then
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
    raise EWebSocketApi.Create('THttpApiWebSocketConnection.BeforeRead state ' +
      'is not wsOpen (%d)', [ord(fState)]);
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
            SetString(fBuffer, PAnsiChar(buf[0].pbBuffer), buf[0].ulBufferLength);
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

constructor THttpApiWebSocketServer.Create(CreateSuspended: boolean;
  aSocketThreadsCount, aPingTimeout: integer; const QueueName: SynUnicode;
  const aOnWSThreadStart, aOnWSThreadTerminate: TOnNotifyThread);
begin
  inherited Create(CreateSuspended, QueueName);
  if not (WebSocketApi.WebSocketEnabled) then
    raise EWebSocketApi.Create('WebSocket API not supported', []);
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
  FreeAndNil(fThreadPoolServer);
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

procedure TSynThreadPoolHttpApiWebSocketServer.Task(aCaller: TSynThread;
  aContext: Pointer);
var
  conn: PHttpApiWebSocketConnection;
begin
  if aContext = @fServer.fSendOverlaped then
    exit;
  if aContext = @fServer.fServiceOverlaped then
  begin
    if Assigned(fServer.onServiceMessage) then
      fServer.onServiceMessage;
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

