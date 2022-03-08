/// HTTP/HTTPS Client Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.client;

{
  *****************************************************************************

   HTTP Client Classes
   - THttpMultiPartStream for multipart/formdata HTTP POST
   - THttpClientSocket Implementing HTTP client over plain sockets
   - THttpRequest Abstract HTTP client class
   - TWinHttp TWinINet TWinHttpWebSocketClient TCurlHttp
   - TSimpleHttpClient Wrapper Class
   - Cached HTTP Connection to a Remote Server
   - Send Email using the SMTP Protocol
   - DNS Resolution Cache for mormot.net.sock NewSocket()

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.net.sock,
  mormot.net.http,
  {$ifdef USEWININET}  // as set in mormot.defines.inc
  WinINet,
  mormot.lib.winhttp,
  {$ifdef FORCE_OPENSSL}
  mormot.lib.openssl11, // bypass SChannel for a given project
  {$endif FORCE_OPENSSL}
  {$endif USEWININET}
  {$ifdef USELIBCURL}  // as set in mormot.defines.inc
  mormot.lib.curl,
  {$endif USELIBCURL}
  {$ifdef DOMAINRESTAUTH}
  mormot.lib.sspi, // do-nothing units on non compliant system
  mormot.lib.gssapi,
  {$endif DOMAINRESTAUTH}
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json, // TSynDictionary for THttpRequestCached
  mormot.core.perf;


{ ******************** THttpMultiPartStream for multipart/formdata HTTP POST }

type
  /// low-level section information as stored by THttpMultiPartStream
  THttpMultiPartStreamSection =  record
    Name: RawUtf8;
    FileName: RawUtf8;
    Content: RawByteString;
    ContentType: RawUtf8;
    ContentFile: TFileName;
  end;
  PHttpMultiPartStreamSection = ^THttpMultiPartStreamSection;
  THttpMultiPartStreamSections = array of THttpMultiPartStreamSection;

  /// a TStream descendant implementing client multipart/formdata HTTP POST
  // - AddContent/AddFileContent/AddFile will append name/contents sections
  // to this instance, then after Flush, send Read() data via TCP to include the
  // proper multipart formatting as defined by RFC 2488 / RFC 1341
  // - AddFile() won't load the file content into memory so it is more
  // efficient than MultiPartFormDataEncode() from mormot.core.buffers
  THttpMultiPartStream = class(TNestedStreamReader)
  protected
    fSections: THttpMultiPartStreamSections;
    fBounds: TRawUtf8DynArray;
    fBound: RawUtf8;
    fMultipartContentType: RawUtf8;
    fFilesCount: integer;
    fRfc2388NestedFiles: boolean;
    function Add(const name, content, contenttype,
      filename, encoding: RawUtf8): PHttpMultiPartStreamSection;
  public
    /// append a content section from a binary/text buffer
    // - warning: should be called before AddFile/AddFileContent
    procedure AddContent(const name: RawUtf8; const content: RawByteString;
      const contenttype: RawUtf8 = ''; const encoding: RawUtf8 = '');
    /// append a file upload section from a binary/text buffer
    // - warning: should be called after AddContent
    procedure AddFileContent(const name, filename: RawUtf8;
      const content: RawByteString; const contenttype: RawUtf8 = '';
      const encoding: RawUtf8 = '');
    /// append a file upload section from a local file
    // - the supplied file won't be loaded into memory, but created as an
    // internal TFileStream to be retrieved by successive Read() calls
    // - warning: should be called after AddContent
    procedure AddFile(const name: RawUtf8; const filename: TFileName;
      const contenttype: RawUtf8 = '');
    /// call this method before any Read() call to sent data to HTTP server
    // - it is called also when Seek(0, soBeginning) is called
    procedure Flush; override;
    /// the content-type header value for this multipart content
    // - equals '' if no section has been added
    // - includes a random boundary field
    property MultipartContentType: RawUtf8
      read fMultipartContentType;
    /// will force the deprecated nested "multipart/mixed" format
    property Rfc2388NestedFiles: boolean
      read fRfc2388NestedFiles write fRfc2388NestedFiles;
    /// high-level sections parameters as provided to Add* methods
    // - can be used e.g. by libcurl which makes its own encoding
    property Sections: THttpMultiPartStreamSections
      read fSections;
    /// how many AddFile/AddFileContent have been appended
    property FilesCount: integer
      read fFilesCount;
  end;


{ ************** THttpClientSocket Implementing HTTP client over plain sockets }

var
  /// THttpRequest timeout default value for DNS resolution
  // - only used by TWinHttp class - other clients will ignore it
  // - leaving to 0 will let system default value be used
  HTTP_DEFAULT_RESOLVETIMEOUT: integer = 0;
  /// THttpRequest timeout default value for remote connection
  // - default is 30 seconds
  // - used e.g. by THttpClientSocket, THttpRequest, TRestHttpClientRequest and
  // TRestHttpClientGeneric
  HTTP_DEFAULT_CONNECTTIMEOUT: integer = 30000;
  /// THttpRequest timeout default value for data sending
  // - default is 30 seconds
  // - used e.g. by THttpClientSocket, THttpRequest, TRestHttpClientRequest and
  // TRestHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // the class constructor
  HTTP_DEFAULT_SENDTIMEOUT: integer = 30000;
  /// THttpRequest timeout default value for data receiving
  // - default is 30 seconds
  // - used e.g. by THttpClientSocket, THttpRequest, TRestHttpClientRequest and
  // TRestHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // the class constructor
  HTTP_DEFAULT_RECEIVETIMEOUT: integer = 30000;

const
  /// standard text used to identify the WebSockets protocol
  HTTP_WEBSOCKET_PROTOCOL: RawUtf8 = 'SEC-WEBSOCKET-PROTOCOL';


type
  /// parameters set for THttpClientSocket.WGet() process
  // - some parameters are optional, and you should call Clear by default
  // - you could call redirectly the WGet method after having called Clear
  // and set the appropriated variables
  {$ifdef USERECORDWITHMETHODS}
  THttpClientSocketWGet = record
  {$else}
  THttpClientSocketWGet = object
  {$endif USERECORDWITHMETHODS}
    /// how much time this connection should be kept alive
    // - as redirected to the internal Request() parameter
    KeepAlive: cardinal;
    /// callback event called during download process
    // - typical usage is to assign e.g. TStreamRedirect.ProgressToConsole
    // - note that by default, THttpClientSocket.OnLog will always be called
    OnProgress: TOnStreamProgress;
    /// optional callback if TFileStream.Create(FileName, Mode) is not good enough
    OnStreamCreate: TOnStreamCreate;
    /// allow to continue an existing .part file download
    // - during the download phase, url + '.part' is used locally to avoid
    // confusion in case of process shutdown - you can use this parameter to
    // continue the download from the existing content (useful for huge files)
    Resume: boolean;
    /// try to download the Hash value from the server, e.g. from url + '.md5'
    // - the hash URI extension is retrieved from TStreamRedirect.GetHashFileExt
    HashFromServer: boolean;
    /// allow custom hashing of the content
    // - if not set, a plain TStreamRedirect with no hashing instance will be
    // used for correct streaming to the destination file
    // - typical classes are TStreamRedirectMd5 or TStreamRedirectSha256 from
    // mormot.crypt.secure
    Hasher: TStreamRedirectClass;
    /// the expected hash value, to be compared with Hasher.GetHash return
    // - if supplied, the downloaded content will be checked against this value
    // - see also HashFromServer and HashCacheDir parameters
    Hash: RawUtf8;
    /// an optional folder to lookup for existing content
    // - the Hash parameter will be used to validate the content
    HashCacheDir: TFileName;
    /// allow to customize request headers, e.g. a cookie or Auth-Bearer
    Header: RawUtf8;
    /// can be used to reduce the download speed into supplied bytes per second
    LimitBandwith: integer;
    /// will raise ESynException after TimeOutSec seconds are elapsed
    // - WGet(sockettimeout) is the TCP connect/receive/send raw timeout for
    // each packet, whereas this property is about the global time elapsed
    TimeOutSec: integer;
    /// initialize the default parameters
    procedure Clear;
    /// after Clear, instantiate and wrap THttpClientSocket.WGet
    function WGet(const url: RawUtf8; const destfile: TFileName;
      const tunnel: RawUtf8 = ''; tls: PNetTlsContext = nil;
      sockettimeout: cardinal = 10000; redirectmax: integer = 0): TFileName;
  end;

  /// THttpClientSocket.Request low-level execution context
  TTHttpClientSocketRequestParams = record
    url, method, header: RawUtf8;
    Data: RawByteString;
    DataType: RawUtf8;
    status, redirected: integer;
    InStream, OutStream: TStream;
    KeepAlive: cardinal;
    OutStreamInitialPos: Int64;
    retry: set of (rMain, rAuth, rAuthProxy);
  end;

  THttpClientSocket = class;

  /// callback used by THttpClientSocket.Request on HTTP_UNAUTHORIZED (401)
  // or HTTP_PROXYAUTHREQUIRED (407) errors
  // - as set to OnAuthorize/OnProxyAuthorize event properties
  // - Authenticate contains the "WWW-Authenticate" response header value
  // - could e.g. set Sender.AuthBearer/BasicAuthUserPassword then return
  // true to let the caller try again with the new headers
  // - if you return false, nothing happens and the 401 is reported back
  // - more complex schemes (like SSPI) could be implemented within the
  // callback - see e.g. THttpClientSocket.AuthorizeSspi class method
  TOnHttpClientSocketAuthorize = function(Sender: THttpClientSocket;
    var Context: TTHttpClientSocketRequestParams;
    const Authenticate: RawUtf8): boolean of object;

  /// callback used by THttpClientSocket.Request before/after every request
  // - return true to continue execution, false to abort normal process
  TOnHttpClientSocketRequest = function(Sender: THttpClientSocket;
    var Context: TTHttpClientSocketRequestParams): boolean of object;

  /// Socket API based REST and HTTP/1.1 compatible client class
  // - this component is HTTP/1.1 compatible, according to RFC 2068 document
  // - the REST commands (GET/POST/PUT/DELETE) are directly available
  // - open connection with the server with inherited Open(server,port) function
  // - if KeepAlive>0, the connection is not broken: a further request (within
  // KeepAlive milliseconds) will use the existing connection if available,
  // or recreate a new one if the former is outdated or reset by server
  // (will retry only once); this is faster, uses less resources (especialy
  // under Windows), and is the recommended way to implement a HTTP/1.1 server
  // - on any error (timeout, connection closed) will retry once to get the value
  // - note that this client is not thread-safe: either use a critical section
  // (as we do in TRestClientUri), or create one instance per thread
  // - don't forget to use Free procedure when you are finished
  THttpClientSocket = class(THttpSocket)
  protected
    fUserAgent: RawUtf8;
    fReferer: RawUtf8;
    fAccept: RawUtf8;
    fProcessName: RawUtf8;
    fRedirected: RawUtf8;
    fRangeStart, fRangeEnd: Int64;
    fBasicAuthUserPassword, fAuthBearer: RawUtf8;
    fOnAuthorize, fOnProxyAuthorize: TOnHttpClientSocketAuthorize;
    fOnBeforeRequest: TOnHttpClientSocketRequest;
    fOnAfterRequest: TOnHttpClientSocketRequest;
    fRedirectMax: integer;
    {$ifdef DOMAINRESTAUTH}
    fAuthorizeSspiSpn: RawUtf8;
    {$endif DOMAINRESTAUTH}
    procedure RequestSendHeader(const url, method: RawUtf8); virtual;
    procedure RequestClear; virtual;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    // - you can customize the default client timeouts by setting appropriate
    // aTimeout parameters (in ms) if you left the 0 default parameters,
    // it would use global HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(aTimeOut: PtrInt = 0); override;
    /// low-level HTTP/1.1 request
    // - called by all Get/Head/Post/Put/Delete REST methods
    // - after an Open(server,port), return 200,202,204 if OK, or an http
    // status error otherwise
    // - retry is usually false, but could be recursively recalled as true
    // - use either Data or InStream for sending its body request
    // - response body will be either in Content or in OutStream
    // - wrapper around RequestInternal() with OnBeforeRequest/OnAfterRequest
    // and RedirectMax handling
    function Request(const url, method: RawUtf8; KeepAlive: cardinal;
      const header: RawUtf8; const Data: RawByteString = '';
      const DataType: RawUtf8 = ''; retry: boolean = false;
      InStream: TStream = nil; OutStream: TStream = nil): integer; virtual;
    /// low-level processing method called from Request()
    // - can be used e.g. when implementing callbacks like OnAuthorize or
    // OnBeforeRequest/OnAfterRequest
    procedure RequestInternal(var ctxt: TTHttpClientSocketRequestParams); virtual;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    function Get(const url: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    // - if AuthToken<>'', will add an header with 'Authorization: Bearer '+AuthToken
    function GetAuth(const url, AuthToken: RawUtf8; KeepAlive: cardinal = 0): integer;
    /// download a (huge) file with proper resume and optional caching
    // - DestFile is the file name to use to put the downloaded content - if
    // left void, will compute and return a file name from the url value
    // - fine tuning of the process could be done using params
    function WGet(const url: RawUtf8; const destfile: TFileName;
      var params: THttpClientSocketWGet): TFileName;
    /// after an Open(server,port), return 200 if OK, http status error otherwise - only
    // header is read from server: Content is always '', but Headers are set
    function Head(const url: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Post(const url: RawUtf8; const Data: RawByteString;
      const DataType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer; overload;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    // - this overloaded method accepts a TStream for its output body content
    // - you could use a THttpMultiPartStream for multipart/formdata HTTP POST
    function Post(const url: RawUtf8; Data: TStream;
      const DataType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer; overload;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Put(const url: RawUtf8; const Data: RawByteString;
      const DataType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    function Delete(const url: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    {$ifdef DOMAINRESTAUTH}
    /// web authentication of the current logged user using Windows Security
    // Support Provider Interface (SSPI) or GSSAPI library on Linux
    // - match the OnAuthorize: TOnHttpClientSocketAuthorize callback signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function AuthorizeSspi(Sender: THttpClientSocket;
      var Context: TTHttpClientSocketRequestParams; const Authenticate: RawUtf8): boolean;
    /// web authentication of the current logged user using Windows Security
    // Support Provider Interface (SSPI) or GSSAPI library on Linux
    // - match the OnProxyAuthorize: TOnHttpClientSocketAuthorize signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function ProxyAuthorizeSspi(Sender: THttpClientSocket;
      var Context: TTHttpClientSocketRequestParams; const Authenticate: RawUtf8): boolean;
    /// the Kerberos Service Principal Name, as registered in domain
    // - e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
    // - used by class procedure AuthorizeSspi/ProxyAuthorizeSspi callbacks
    // - on Linux/GSSAPI either this property or ClientForceSpn() is mandatory
    property AuthorizeSspiSpn: RawUtf8
      read fAuthorizeSspiSpn write fAuthorizeSspiSpn;
    {$endif DOMAINRESTAUTH}

    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
    /// the optional 'Accept: ' header value
    property Accept: RawUtf8
      read fAccept write fAccept;
    /// the optional 'Referer: ' header value
    property Referer: RawUtf8
      read fReferer write fReferer;
    /// the associated process name, to be set and used by the external context
    property ProcessName: RawUtf8
      read fProcessName write fProcessName;
    /// optional begining position of a request
    // - is reset once the Request has been sent
    property RangeStart: Int64
      read fRangeStart write fRangeStart;
    /// optional ending position of a request
    // - is reset once the Request has been sent
    property RangeEnd: Int64
      read fRangeEnd write fRangeEnd;
    /// how many 3xx status code redirections are allowed
    // - default is 0 - i.e. no redirection
    property RedirectMax: integer
      read fRedirectMax write fRedirectMax;
    /// the effective 'Location:' URI after 3xx redirection(s) of Request()
    property Redirected: RawUtf8
      read fRedirected;
    /// optional Authorization: Basic header, encoded as 'User:Password' text
    property BasicAuthUserPassword: RawUtf8
      read fBasicAuthUserPassword write fBasicAuthUserPassword;
    /// optional Authorization: Bearer header value
    property AuthBearer: RawUtf8
      read fAuthBearer write fAuthBearer;
    /// contain the body data retrieved from the server - from inherited Http
    property Content: RawByteString
      read Http.Content;
    /// contain the body data length retrieved from the server - from inherited Http
    property ContentLength: Int64
      read Http.ContentLength;
    /// contain the response headers retrieved from the server - from inherited Http
    property Headers: RawUtf8
      read Http.Headers;
    /// optional authorization callback
    // - is triggered by Request() on HTTP_UNAUTHORIZED (401) status
    // - see e.g. THttpClientSocket.AuthorizeSspi class method for SSPI auth
    property OnAuthorize: TOnHttpClientSocketAuthorize
      read fOnAuthorize write fOnAuthorize;
    /// optional proxy authorization callback
    // - is triggered by Request() on HTTP_PROXYAUTHREQUIRED (407) status
    // - see e.g. THttpClientSocket.ProxyAuthorizeSspi class method for SSPI auth
    property OnProxyAuthorize: TOnHttpClientSocketAuthorize
      read fOnProxyAuthorize write fOnProxyAuthorize;
    /// optional callback called before each Request()
    property OnBeforeRequest: TOnHttpClientSocketRequest
      read fOnBeforeRequest write fOnBeforeRequest;
    /// optional callback called after each Request()
    property OnAfterRequest: TOnHttpClientSocketRequest
      read fOnAfterRequest write fOnAfterRequest;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // mormot.net.websock unit)
  THttpClientSocketClass = class of THttpClientSocket;

/// returns the HTTP User-Agent header value of a mORMot client including
// the Instance class name in its minified/uppercase-only translation
function DefaultUserAgent(Instance: TObject): RawUtf8;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ENetSock
function OpenHttp(const aServer, aPort: RawUtf8; aTLS: boolean = false;
  aLayer: TNetLayer = nlTcp; const aUrlForProxy: RawUtf8 = ''): THttpClientSocket; overload;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ENetSock
function OpenHttp(const aUri: RawUtf8;
  aAddress: PRawUtf8 = nil): THttpClientSocket; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket: if you want
// something able to use your computer proxy, take a look at TWinINet.Get()
// and the overloaded HttpGet() functions
function OpenHttpGet(const server, port, url, inHeaders: RawUtf8;
  outHeaders: PRawUtf8 = nil; aLayer: TNetLayer = nlTcp;
  aTLS: boolean = false; outStatus: PInteger = nil): RawByteString; overload;

/// download some potentially huge file, with proper resume
// - is a wrapper around THttpClientSocket.WGet() method
// - returns '' on success, or an error message otherwise
function WGet(const url: RawUtf8; const destfile: TFileName;
  const tunnel: RawUtf8 = ''; hasher: TStreamRedirectClass = nil;
  const hash: RawUtf8 = ''; tls: PNetTlsContext = nil;
  sockettimeout: cardinal = 10000; redirectmax: integer = 0;
  consoledisplay: boolean = false): string;


var
  /// global overriden value for the GetSystemProxyUri() function
  // - as used by OpenHttp/OpenHttpGet and TSimpleHttpClient
  // - can be set manually to a forced global value
  DefaultHttpClientSocketProxy: TUri;

  /// force GetProxyForUri(fromSystem=true) in GetSystemProxyUri() function
  DefaultHttpClientSocketProxyAuto: boolean;


/// ask the Operating System to return the Tunnel/Proxy settings for a given URI
// - as used by OpenHttp/OpenHttpGet and TSimpleHttpClient
// - if proxy is set, will return its value from @temp, otherwise return
// @DefaultHttpClientSocketProxy or call
// GetProxyForUri(DefaultHttpClientSocketProxyAuto) to fill and return @temp
// - return nil if no proxy is to be used for this URI
function GetSystemProxyUri(const uri, proxy: RawUtf8; var temp: TUri): PUri;

/// ask the Operating System to return the Tunnel/Proxy setting for a given URI
// - will always use or HTTP_PROXY/HTTPS_PROXY environment variables
// - if no environment variable is set, on Windows fromSystem=true will call
// WinHttpGetProxyInfo from mormot.lib.winhttp to use the Internet Explorer
// settings or system PAC file
// - return '' if no proxy is defined
function GetProxyForUri(const uri: RawUtf8;
  fromSystem: boolean = true): RawUtf8;


{ ******************** THttpRequest Abstract HTTP client class }

type
  /// the supported authentication schemes which may be used by HTTP clients
  // - supported only by TWinHttp class yet
  THttpRequestAuthentication = (
    wraNone,
    wraBasic,
    wraDigest,
    wraNegotiate);

  /// a record to set some extended options for HTTP clients
  // - allow easy propagation e.g. from a TRestHttpClient* wrapper class to
  // the actual mormot.net.http's THttpRequest implementation class
  THttpRequestExtendedOptions = record
    /// let HTTPS be less paranoid about SSL certificates
    // - IgnoreSSLCertificateErrors is handled by TWinHttp and TCurlHttp
    IgnoreSSLCertificateErrors: boolean;
    /// allow HTTP authentication to take place at connection
    // - Auth.Scheme and UserName/Password properties are handled
    // by the TWinHttp class only by now
    Auth: record
      UserName: SynUnicode;
      Password: SynUnicode;
      Scheme: THttpRequestAuthentication;
    end;
    /// allow to customize the User-Agent header
    UserAgent: RawUtf8;
  end;

  {$M+} // to have existing RTTI for published properties
  /// abstract class to handle HTTP/1.1 request
  // - never instantiate this class, but inherited TWinHttp, TWinINet or TCurlHttp
  THttpRequest = class
  protected
    fServer: RawUtf8;
    fProxyName: RawUtf8;
    fProxyByPass: RawUtf8;
    fPort: TNetPort;
    fLayer: TNetLayer;
    fHttps: boolean;
    fKeepAlive: cardinal;
    fExtendedOptions: THttpRequestExtendedOptions;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawUtf8;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressAcceptHeader: THttpSocketCompressSet;
    fTag: PtrInt;
    class function InternalREST(const url, method: RawUtf8; const data:
      RawByteString; const header: RawUtf8; aIgnoreSSLCertificateErrors: boolean;
      outHeaders: PRawUtf8 = nil; outStatus: PInteger = nil): RawByteString;
    // inherited class should override those abstract methods
    procedure InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal); virtual; abstract;
    procedure InternalCreateRequest(const aMethod, aUrl: RawUtf8); virtual; abstract;
    procedure InternalSendRequest(const aMethod: RawUtf8; const aData:
      RawByteString); virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUtf8;
      var Data: RawByteString): integer; virtual; abstract;
    procedure InternalCloseRequest; virtual; abstract;
    procedure InternalAddHeader(const hdr: RawUtf8); virtual; abstract;
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; virtual; abstract;
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy:
    // aProxyName/aProxyByPass will be recognized by TWinHttp and TWinINet,
    // and aProxyName will set the CURLOPT_PROXY option to TCurlHttp
    // (see https://curl.haxx.se/libcurl/c/CURLOPT_PROXY.html as reference)
    // - you can customize the default client timeouts by setting appropriate
    // SendTimeout and ReceiveTimeout parameters (in ms) - note that after
    // creation of this instance, the connection is tied to the initial
    // parameters, so we won't publish any properties to change those
    // initial values once created - if you left the 0 default parameters, it
    // would use global HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT
    // and HTTP_DEFAULT_RECEIVETIMEOUT variable values
    // - *TimeOut parameters are currently ignored by TCurlHttp
    constructor Create(const aServer, aPort: RawUtf8; aHttps: boolean;
      const aProxyName: RawUtf8 = ''; const aProxyByPass: RawUtf8 = '';
      ConnectionTimeOut: cardinal = 0; SendTimeout: cardinal = 0;
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTcp); overload; virtual;
    /// connect to the supplied URI
    // - is just a wrapper around TUri and the overloaded Create() constructor
    constructor Create(const aUri: RawUtf8; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; ConnectionTimeOut: cardinal = 0;
      SendTimeout: cardinal = 0; ReceiveTimeout: cardinal = 0;
      aIgnoreSSLCertificateErrors: boolean = false); overload;

    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwise
    // - KeepAlive is in milliseconds, 0 for "Connection: Close" HTTP/1.0 requests
    function Request(const url, method: RawUtf8; KeepAlive: cardinal;
      const InHeader: RawUtf8; const InData: RawByteString; const InDataType: RawUtf8;
      out OutHeader: RawUtf8; out OutData: RawByteString): integer; virtual;

    /// wrapper method to retrieve a resource via an HTTP GET
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Get() but either TWinHttp.Get(), TWinINet.Get() or
    // TCurlHttp.Get() methods
    class function Get(const aUri: RawUtf8; const aHeader: RawUtf8 = '';
      aIgnoreSSLCertificateErrors: boolean = false; outHeaders: PRawUtf8 = nil;
      outStatus: PInteger = nil): RawByteString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Post() but either TWinHttp.Post(), TWinINet.Post() or
    // TCurlHttp.Post() methods
    class function Post(const aUri: RawUtf8; const aData: RawByteString;
      const aHeader: RawUtf8 = ''; aIgnoreSSLCertificateErrors: boolean = false;
      outHeaders: PRawUtf8 = nil; outStatus: PInteger = nil): RawByteString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Put() but either TWinHttp.Put(), TWinINet.Put() or
    // TCurlHttp.Put() methods
    class function Put(const aUri: RawUtf8; const aData: RawByteString;
      const aHeader: RawUtf8 = ''; aIgnoreSSLCertificateErrors: boolean = false;
      outHeaders: PRawUtf8 = nil; outStatus: PInteger = nil): RawByteString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Delete() but either TWinHttp.Delete(), TWinINet.Delete() or
    // TCurlHttp.Delete() methods
    class function Delete(const aUri: RawUtf8; const aHeader: RawUtf8 = '';
      aIgnoreSSLCertificateErrors: boolean = false; outHeaders: PRawUtf8 = nil;
      outStatus: PInteger = nil): RawByteString;

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) below which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024): boolean;

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
    /// custom HTTP "User Agent:" header value
    property UserAgent: RawUtf8
      read fExtendedOptions.UserAgent
      write fExtendedOptions.UserAgent;
    /// internal structure used to store extended options
    // - will be replicated by IgnoreSSLCertificateErrors and Auth* properties
    property ExtendedOptions: THttpRequestExtendedOptions
      read fExtendedOptions
      write fExtendedOptions;
    /// some internal field, which may be used by end-user code
    property Tag: PtrInt
      read fTag write fTag;
  published
    /// the remote server host name, as stated specified to the class constructor
    property Server: RawUtf8
      read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: TNetPort
      read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean
      read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: RawUtf8
      read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: RawUtf8
      read fProxyByPass;
  end;
  {$M-}

  /// store the actual class of a HTTP/1.1 client instance
  // - may be used to define at runtime which API to be used (e.g. WinHttp,
  // WinINet or LibCurl), following the Liskov substitution principle

  THttpRequestClass = class of THttpRequest;


{$ifdef USEWININET}

{ ******************** TWinHttp TWinINet TWinHttpWebSocketClient }

type
  TWinHttpApi = class;

  /// event callback to track download progress, e.g. in the UI
  // - used in TWinHttpApi.OnProgress property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  TOnWinHttpProgress = procedure(Sender: TWinHttpApi;
    CurrentSize, ContentLength: cardinal) of object;

  /// event callback to process the download by chunks, not in memory
  // - used in TWinHttpApi.OnDownload property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  // - ChunkSize is the size of the latest downloaded chunk, available in
  // the untyped ChunkData memory buffer
  // - implementation should return TRUE to continue the download, or FALSE
  // to abort the download process
  TWinHttpDownload = function(Sender: TWinHttpApi; CurrentSize, ContentLength,
    ChunkSize: cardinal; const ChunkData): boolean of object;

  /// event callback to track upload progress, e.g. in the UI
  // - used in TWinHttpApi.OnUpload property
  // - CurrentSize is the current total number of uploaded bytes
  // - ContentLength is the size of content
  // - implementation should return TRUE to continue the upload, or FALSE
  // to abort the upload process
  TWinHttpUpload = function(Sender: TWinHttpApi;
    CurrentSize, ContentLength: cardinal): boolean of object;

  /// a class to handle HTTP/1.1 request using either WinINet or WinHttp API
  // - both APIs have a common logic, which is encapsulated by this parent class
  // - this abstract class defined some abstract methods which will be
  // implemented by TWinINet or TWinHttp with the proper API calls
  TWinHttpApi = class(THttpRequest)
  protected
    fOnProgress: TOnWinHttpProgress;
    fOnDownload: TWinHttpDownload;
    fOnUpload: TWinHttpUpload;
    fOnDownloadChunkSize: cardinal;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    /// do not add "Accept: */*" HTTP header by default
    fNoAllAccept: boolean;
    function InternalGetInfo(Info: cardinal): RawUtf8; virtual; abstract;
    function InternalGetInfo32(Info: cardinal): cardinal; virtual; abstract;
    function InternalQueryDataAvailable: cardinal; virtual; abstract;
    function InternalReadData(var Data: RawByteString; Read: integer;
      Size: cardinal): cardinal; virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUtf8;
      var Data: RawByteString): integer; override;
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; override;
    /// do not add "Accept: */*" HTTP header by default
    property NoAllAccept: boolean
      read fNoAllAccept write fNoAllAccept;
    /// download would call this method to notify progress of incoming data
    property OnProgress: TOnWinHttpProgress
      read fOnProgress write fOnProgress;
    /// download would call this method instead of filling Data: RawByteString value
    // - may be used e.g. when downloading huge content, and saving directly
    // the incoming data on disk or database
    // - if this property is set, raw TCP/IP incoming data would be supplied:
    // compression and encoding won't be handled by the class
    property OnDownload: TWinHttpDownload
      read fOnDownload write fOnDownload;
    /// upload would call this method to notify progress of outgoing data
    // - and optionally abort sending the data by returning FALSE
    property OnUpload: TWinHttpUpload
      read fOnUpload write fOnUpload;
    /// how many bytes should be retrieved for each OnDownload event chunk
    // - if default 0 value is left, would use 65536, i.e. 64KB
    property OnDownloadChunkSize: cardinal
      read fOnDownloadChunkSize
      write fOnDownloadChunkSize;
  end;

  /// a class to handle HTTP/1.1 request using the WinINet API
  // - The Microsoft Windows Internet (WinINet) application programming interface
  // (API) enables applications to access standard Internet protocols, such as
  // FTP and HTTP/HTTPS, similar to what IE offers
  // - by design, the WinINet API should not be used from a service, since this
  // API may require end-user GUI interaction
  // - note: WinINet is MUCH slower than THttpClientSocket or TWinHttp: do not
  // use this, only if you find some configuration benefit on some old networks
  // (e.g. to diaplay the dialup popup window for a GUI client application)
  TWinINet = class(TWinHttpApi)
  protected
    // those internal methods will raise an EWinINet exception on error
    procedure InternalConnect(ConnectionTimeOut, SendTimeout,
      ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aUrl: RawUtf8); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUtf8); override;
    procedure InternalSendRequest(const aMethod: RawUtf8;
      const aData: RawByteString); override;
    function InternalGetInfo(Info: cardinal): RawUtf8; override;
    function InternalGetInfo32(Info: cardinal): cardinal; override;
    function InternalQueryDataAvailable: cardinal; override;
    function InternalReadData(var Data: RawByteString; Read: integer;
      Size: cardinal): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinINet exception type
  EWinINet = class(EHttpSocket)
  protected
    fLastError: integer;
  public
    /// create and raise a WinINet exception, with the error message as text
    class procedure RaiseFromLastError;
  published
    /// the associated WSAGetLastError value
    property LastError: integer
      read fLastError;
  end;

  /// a class to handle HTTP/1.1 request using the WinHttp API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure https connection - so it seems to be the
  // class to use in your client programs
  // - WinHttp does not share any proxy settings with Internet Explorer.
  // The WinHttp proxy configuration is set by either
  // $ proxycfg.exe
  // on Windows XP and Windows Server 2003 or earlier, either
  // $ netsh.exe
  // on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run either:
  // $ proxycfg -u
  // $ netsh winhttp import proxy source=ie
  // to use the current user's proxy settings for Internet Explorer (under 64-bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitly)
  // - Microsoft Windows HTTP Services (WinHttp) is targeted at middle-tier and
  // back-end server applications that require access to an HTTP client stack
  TWinHttp = class(TWinHttpApi)
  protected
    // you can override this method e.g. to disable/enable some protocols
    function InternalGetProtocols: cardinal; virtual;
    // those internal methods will raise an EOSError exception on error
    procedure InternalConnect(ConnectionTimeOut, SendTimeout,
      ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aUrl: RawUtf8); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUtf8); override;
    procedure InternalSendRequest(const aMethod: RawUtf8;
      const aData: RawByteString); override;
    function InternalGetInfo(Info: cardinal): RawUtf8; override;
    function InternalGetInfo32(Info: cardinal): cardinal; override;
    function InternalQueryDataAvailable: cardinal; override;
    function InternalReadData(var Data: RawByteString; Read: integer;
      Size: cardinal): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinHttp exception type
  EWinHttp = class(ESynException);

  /// establish a client connection to a WebSocket server using the Windows API
  // - used by TWinWebSocketClient class
  TWinHttpUpgradeable = class(TWinHttp)
  private
    fSocket: HINTERNET;
  protected
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding:
      RawUtf8; var Data: RawByteString): integer; override;
    procedure InternalSendRequest(const aMethod: RawUtf8;
      const aData: RawByteString); override;
  public
    /// initialize the instance
    constructor Create(const aServer, aPort: RawUtf8; aHttps: boolean;
      const aProxyName: RawUtf8 = ''; const aProxyByPass: RawUtf8 = '';
      ConnectionTimeOut: cardinal = 0; SendTimeout: cardinal = 0;
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTcp); override;
  end;

  /// WebSocket client implementation
  TWinHttpWebSocketClient = class
  protected
    fSocket: HINTERNET;
    function CheckSocket: boolean;
  public
    /// initialize the instance
    // - all parameters do match TWinHttp.Create except url: address of WebSocketServer
    // for sending upgrade request
    constructor Create(const aServer, aPort: RawUtf8; aHttps: boolean;
      const url: RawUtf8; const aSubProtocol: RawUtf8 = ''; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; ConnectionTimeOut: cardinal = 0;
      SendTimeout: cardinal = 0; ReceiveTimeout: cardinal = 0);
    /// send buffer
    function Send(aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer;
      aBufferLength: cardinal): cardinal;
    /// receive buffer
    function Receive(aBuffer: pointer; aBufferLength: cardinal;
      out aBytesRead: cardinal; out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): cardinal;
    /// close current connection
    function CloseConnection(const aCloseReason: RawUtf8): cardinal;
    /// finalize the instance
    destructor Destroy; override;
  end;

{$endif USEWININET}

{$ifdef USELIBCURL}

type
  /// libcurl exception type
  ECurlHttp = class(ExceptionWithProps);

  /// a class to handle HTTP/1.1 request using the libcurl library
  // - libcurl is a free and easy-to-use cross-platform URL transfer library,
  // able to directly connect via HTTP or HTTPS on most Linux systems
  // - under a 32 bit Linux system, the libcurl library (and its dependencies,
  // like OpenSSL) may not be installed - you can add it via your package
  // manager, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3
  // - under a 64-bit Linux system, if compiled with Kylix, you should install
  // the 32-bit flavor of libcurl, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3:i386
  // - will use in fact libcurl.so, so either libcurl.so.3 or libcurl.so.4,
  // depending on the default version available on the system
  TCurlHttp = class(THttpRequest)
  protected
    fHandle: pointer;
    fRootURL: RawUtf8;
    fIn: record
      Headers: pointer;
      DataOffset: integer;
      URL, Method: RawUtf8;
      Data: RawByteString;
    end;
    fOut: record
      Header, Encoding, AcceptEncoding: RawUtf8;
      Data: RawByteString;
    end;
    fSSL: record
      CertFile, CACertFile, KeyName, PassPhrase: RawUtf8;
    end;
    procedure InternalConnect(
      ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aUrl: RawUtf8); override;
    procedure InternalSendRequest(const aMethod: RawUtf8;
      const aData: RawByteString); override;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUtf8;
      var Data: RawByteString): integer; override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUtf8); override;
    function GetCACertFile: RawUtf8;
    procedure SetCACertFile(const aCertFile: RawUtf8);
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; override;
    /// release the connection
    destructor Destroy; override;
    /// allow to set a CA certification file without touching the client certification
    property CACertFile: RawUtf8
      read GetCACertFile write SetCACertFile;
    /// set the client SSL certification details
    // - see CACertFile if you don't want to change the whole client cert info
    // - used e.g. as
    // ! UseClientCertificate('testcert.pem','cacert.pem','testkey.pem','pass');
    procedure UseClientCertificate(
      const aCertFile, aCACertFile, aKeyName, aPassPhrase: RawUtf8);
  end;

{$endif USELIBCURL}


{ ******************** TSimpleHttpClient Wrapper Class }

type
  /// simple wrapper around THttpClientSocket/THttpRequest instances
  // - this class will reuse the previous connection if possible, and select the
  // best connection class available on this platform for a given URI
  TSimpleHttpClient = class
  protected
    fHttp: THttpClientSocket;
    fHttps: THttpRequest;
    fProxy, fHeaders, fUserAgent: RawUtf8;
    fBody: RawByteString;
    fSocketTLS: TNetTlsContext;
    fOnlyUseClientSocket: boolean;
    fTimeOut: integer;
  public
    /// initialize the instance
    // - aOnlyUseClientSocket=true will use THttpClientSocket even for HTTPS
    constructor Create(aOnlyUseClientSocket: boolean = false); reintroduce;
    /// finalize the connection
    destructor Destroy; override;
    /// low-level entry point of this instance, using an TUri as input
    // - rather use the Request() more usable method
    function RawRequest(const Uri: TUri; const Method, Header: RawUtf8;
      const Data: RawByteString; const DataType: RawUtf8;
      KeepAlive: cardinal): integer; overload;
    /// simple-to-use entry point of this instance
    // - use Body and Headers properties to retrieve the HTTP body and headers
    function Request(const uri: RawUtf8; const method: RawUtf8 = 'GET';
      const header: RawUtf8 = ''; const data: RawByteString = '';
      const datatype: RawUtf8 = ''; keepalive: cardinal = 10000): integer; overload;
    /// access to the raw TLS settings for THttpClientSocket
    function SocketTLS: PNetTlsContext;
      {$ifdef HASINLINE} inline; {$endif}
    /// returns the HTTP body as returned by a previous call to Request()
    property Body: RawByteString
      read fBody;
    /// returns the HTTP headers as returned by a previous call to Request()
    property Headers: RawUtf8
      read fHeaders;
    /// allows to customize the user-agent header
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
    /// allows to customize HTTPS connection and allow weak certificates
    property IgnoreSSLCertificateErrors: boolean
      read fSocketTLS.IgnoreCertificateErrors write fSocketTLS.IgnoreCertificateErrors;
    /// set the timeout value for RawRequest/Request, in milliseconds
    property TimeOut: integer
      read fTimeOut write fTimeOut;
    /// alows to customize the connection using a proxy
    property Proxy: RawUtf8
      read fProxy write fProxy;
  end;

/// returns the best THttpRequest class, depending on the system it runs on
// - e.g. TWinHttp or TCurlHttp
// - consider using TSimpleHttpClient if you just need a simple connection
function MainHttpClass: THttpRequestClass;

/// low-level forcing of another THttpRequest class
// - could be used if we found out that the current MainHttpClass failed (which
// could easily happen with TCurlHttp if the library is missing or deprecated)
procedure ReplaceMainHttpClass(aClass: THttpRequestClass);


{ ************** Cached HTTP Connection to a Remote Server }

type
  /// in-memory storage of one THttpRequestCached entry
  THttpRequestCache = record
    Tag: RawUtf8;
    Content: RawByteString;
  end;
  /// in-memory storage of all THttpRequestCached entries
  THttpRequestCacheDynArray = array of THttpRequestCache;

  /// handles cached HTTP connection to a remote server
  // - use in-memory cached content when HTTP_NOTMODIFIED (304) is returned
  // for an already known ETAG header value
  THttpRequestCached = class(TSynPersistent)
  protected
    fUri: TUri;
    fClient: TSimpleHttpClient;
    fKeepAlive: integer;
    fTokenHeader: RawUtf8;
    fCache: TSynDictionary;
  public
    /// initialize the cache for a given server
    // - once set, you can change the request URI using the Address property
    // - aKeepAliveSeconds = 0 will force "Connection: Close" HTTP/1.0 requests
    // - an internal cache will be maintained, and entries will be flushed after
    // aTimeoutSeconds - i.e. 15 minutes per default - setting 0 will disable
    // the client-side cache content
    // - aToken is an optional token which will be transmitted as HTTP header:
    // $ Authorization: Bearer <aToken>
    // - TWinHttp will be used by default under Windows, unless you specify
    // another class
    constructor Create(const aUri: RawUtf8; aKeepAliveSeconds: integer = 30;
      aTimeoutSeconds: integer = 15*60; const aToken: RawUtf8 = '';
      aOnlyUseClientSocket: boolean = false); reintroduce;
    /// finalize the current connnection and flush its in-memory cache
    // - you may use LoadFromUri() to connect to a new server
    procedure Clear;
    /// connect to a new server
    // - aToken is an optional token which will be transmitted as HTTP header:
    // $ Authorization: Bearer <aToken>
    function LoadFromUri(const aUri: RawUtf8; const aToken: RawUtf8 = ''): boolean;
    /// finalize the cache
    destructor Destroy; override;
    /// retrieve a resource from the server, or internal cache
    // - aModified^ = true if server returned a HTTP_SUCCESS (200) with some new
    // content, or aModified^ = false if HTTP_NOTMODIFIED (304) was returned
    function Get(const aAddress: RawUtf8; aModified: PBoolean = nil;
      aStatus: PInteger = nil): RawByteString;
    /// erase one resource from internal cache
    function Flush(const aAddress: RawUtf8): boolean;
    /// read-only access to the connected server
    property URI: TUri
      read fUri;
    /// access to the underlying HTTP client connection class
    property Client: TSimpleHttpClient
      read fClient;
  end;


/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHttp/TCurlHttp for any https URI, or if forceNotSocket is set to true
// - see also OpenHttpGet() for direct THttpClientSock call
function HttpGet(const aUri: RawUtf8; outHeaders: PRawUtf8 = nil;
  forceNotSocket: boolean = false; outStatus: PInteger = nil): RawByteString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHttp/TCurlHttp for any https URI
function HttpGet(const aUri: RawUtf8; const inHeaders: RawUtf8;
  outHeaders: PRawUtf8 = nil; forceNotSocket: boolean = false;
  outStatus: PInteger = nil): RawByteString; overload;



{ ************** Send Email using the SMTP Protocol }

const
  /// the layout of TSMTPConnection.FromText method
  SMTP_DEFAULT = 'user:password@smtpserver:port';

type
  /// may be used to store a connection to a SMTP server
  // - see SendEmail() overloaded function
  {$ifdef USERECORDWITHMETHODS}
  TSMTPConnection = record
  {$else}
  TSMTPConnection = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the SMTP server IP or host name
    Host: RawUtf8;
    /// the SMTP server port (25 by default)
    Port: RawUtf8;
    /// the SMTP user login (if any)
    User: RawUtf8;
    /// the SMTP user password (if any)
    Pass: RawUtf8;
    /// fill the STMP server information from a single text field
    // - expects 'user:password@smtpserver:port' format
    // - if aText equals SMTP_DEFAULT ('user:password@smtpserver:port'),
    // does nothing
    function FromText(const aText: RawUtf8): boolean;
  end;

  /// exception class raised by SendEmail() on raw SMTP process
  ESendEmail = class(ESynException);

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7-bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server, From, CsvDest, Subject, Text: RawUtf8;
  const Headers: RawUtf8 = ''; const User: RawUtf8 = ''; const Pass: RawUtf8 = '';
  const Port: RawUtf8 = '25'; const TextCharSet: RawUtf8  =  'ISO-8859-1';
  TLS: boolean = false): boolean; overload;

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7-bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server: TSMTPConnection;
  const From, CsvDest, Subject, Text: RawUtf8; const Headers: RawUtf8 = '';
  const TextCharSet: RawUtf8  = 'ISO-8859-1'; TLS: boolean = false): boolean; overload;

/// convert a supplied subject text into an Unicode encoding
// - will convert the text into UTF-8 and append '=?UTF-8?B?'
// - for pre-Unicode versions of Delphi, Text is expected to be already UTF-8
// encoded - since Delphi 2010, it will be converted from UnicodeString
function SendEmailSubject(const Text: string): RawUtf8;



implementation


{ ******************** THttpMultiPartStream for multipart/formdata HTTP POST }

{ THttpMultiPartStream }

function THttpMultiPartStream.Add(const name, content, contenttype,
  filename, encoding: RawUtf8): PHttpMultiPartStreamSection;
var
  ns: PtrInt;
  s: RawUtf8;
begin
  // same logic than MultiPartFormDataEncode() from mormot.core.buffers
  ns := length(fSections);
  SetLength(fSections, ns + 1);
  result := @fSections[ns];
  if name = '' then
    if filename = '' then
      FormatUtf8('field%', [ns], result^.Name)
    else
      FormatUtf8('file%', [fFilesCount], result^.Name)
  else
    result^.Name := MimeHeaderEncode(name);
  result^.Content := content;
  result^.ContentType := contenttype;
  if result^.ContentType = '' then
    result^.ContentType := GetMimeContentType(
      pointer(content), length(content), Ansi7ToString(filename));
  if result^.ContentType = '' then
    if filename = '' then
      if (content <> '') and
         (GotoNextNotSpace(pointer(content))^ in ['{', '[', '"']) and
         IsValidJson(content) then
        result^.ContentType := JSON_CONTENT_TYPE
      else
        result^.ContentType := TEXT_CONTENT_TYPE
    else
      result^.ContentType := BINARY_CONTENT_TYPE;
  result^.FileName := filename;
  if fNested = nil then
  begin
    // compute multipart content type with the main random boundary
    fBound := MultiPartFormDataNewBound(fBounds);
    fMultipartContentType  := 'multipart/form-data; boundary=' + fBound;
  end;
  if filename = '' then
    // simple form field
    FormatUtf8('--%'#13#10 +
      'Content-Disposition: form-data; name="%"'#13#10 +
      'Content-Type: %'#13#10#13#10'%'#13#10,
      [fBound, result^.Name, result^.ContentType, content], s)
  else
  begin
    // file field
    if fRfc2388NestedFiles and
       (fFilesCount = 0) then
    begin
      // if this is the first file, create the RFC 2388 nested "files"
      FormatUtf8('--%'#13#10, [fBound], s);
      fBound := MultiPartFormDataNewBound(fBounds);
      s := FormatUtf8(
        '%Content-Disposition: form-data; name="files"'#13#10 +
        'Content-Type: multipart/mixed; boundary=%'#13#10#13#10'--%'#13#10 +
        'Content-Disposition: file; name="%"; filename="%"'#13#10 +
        'Content-Type: %'#13#10, [{%H-}s, fBound, fBound,
         result^.Name, MimeHeaderEncode(filename), result^.ContentType]);
    end
    else
      // see https://tools.ietf.org/html/rfc7578#appendix-A
      FormatUtf8('--%'#13#10 +
        'Content-Disposition: form-data; name="%"; filename="%"'#13#10 +
        'Content-Type: %'#13#10,
        [fBound, result^.Name, filename, result^.ContentType], s);
    if encoding <> '' then
      s := FormatUtf8('%Content-Transfer-Encoding: %'#13#10, [s, encoding]);
    if content <> '' then
      s := s + #13#10 + content + #13#10
    else
      s := s + #13#10; // a TFileStream content will be appended
    inc(fFilesCount);
  end;
  Append(s);
end;

procedure THttpMultiPartStream.AddContent(const name: RawUtf8;
  const content: RawByteString; const contenttype: RawUtf8;
  const encoding: RawUtf8);
begin
  Add(name, content, contenttype, '', encoding);
end;

procedure THttpMultiPartStream.AddFileContent(const name, filename: RawUtf8;
  const content: RawByteString; const contenttype: RawUtf8;
  const encoding: RawUtf8);
begin
  Add(name, content, contenttype, filename, encoding);
end;

procedure THttpMultiPartStream.AddFile(const name: RawUtf8;
  const filename: TFileName; const contenttype: RawUtf8);
var
  fs: TFileStream;
  fn: RawUtf8;
begin
  fs := TFileStream.Create(filename, fmShareDenyNone or fmOpenRead);
  // an exception is raised in above line if filename is incorrect
  fn := StringToUtf8(ExtractFileName(filename));
  Add(name, '', contenttype, fn, 'binary')^.ContentFile := filename;
  NewStream(fs);
  Append(#13#10);
end;

procedure THttpMultiPartStream.Flush;
var
  i: PtrInt;
  s: RawUtf8;
begin
  if fBounds = nil then
    exit;
  for i := length(fBounds) - 1 downto 0 do
    s := {%H-}s + '--' + fBounds[i] + '--'#13#10;
  Append(s);
  inherited Flush; // compute fSize
end;




{ ************** THttpClientSocket Implementing HTTP client over plain sockets }

function DefaultUserAgent(Instance: TObject): RawUtf8;
var
  i: PtrInt;
  P: PShortString;
  name: ShortString;
begin
  // instance class THttpClientSocket translated into 'HCS'
  P := ClassNameShort(Instance);
  name[0] := #0;
  for i := 2 to ord(P^[0]) do
    if P^[i] in ['A'..'Z'] then
      {%H-}AppendShortChar(P^[i], name);
  if name[0] <> #0 then
    P := @name;
  // note: the framework would identify 'mORMot' pattern in the user-agent
  // header to enable advanced behavior e.g. about JSON transmission
  FormatUtf8('Mozilla/5.0 (' + OS_TEXT + '; mORMot ' +
    SYNOPSE_FRAMEWORK_VERSION + ' %)', [P^], result);
end;


{ THttpClientSocketWGet }

procedure THttpClientSocketWGet.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

function THttpClientSocketWGet.WGet(const url: RawUtf8;
  const destfile: TFileName; const tunnel: RawUtf8; tls: PNetTlsContext;
  sockettimeout: cardinal; redirectmax: integer): TFileName;
var
  s: THttpClientSocket;
  u: RawUtf8;
begin
  s := THttpClientSocket.OpenUri(url, u, tunnel, sockettimeout, tls);
  try
    s.RedirectMax := redirectmax;
    result := s.WGet(u, destfile, self);
  finally
    s.Free;
  end;
end;

function WGet(const url: RawUtf8; const destfile: TFileName;
  const tunnel: RawUtf8; hasher: TStreamRedirectClass; const hash: RawUtf8;
  tls: PNetTlsContext; sockettimeout: cardinal; redirectmax: integer;
  consoledisplay: boolean): string;
var
  params: THttpClientSocketWGet;
begin
  try
    if destfile = '' then
      raise EHttpSocket.CreateUtf8('WGet(destfile='''') for %', [url]);
    params.Clear;
    params.Resume := true;
    params.Hasher := hasher;
    params.Hash := hash;
    if consoledisplay then
      params.OnProgress := TStreamRedirect.ProgressStreamToConsole;
    if params.WGet(url, destfile,
         tunnel, tls, sockettimeout, redirectmax) = destfile then
      result := ''
    else
      result := 'WGet: unexpected destfile'; // paranoid
  except
    on E: Exception do
      FormatString('[%] %', [E, E.Message], result);
  end;
end;


var
  _PROXYSET: boolean; // retrieve environment variables only once
  _PROXYSAFE: TLightLock;
  _PROXY: array[{https:}boolean] of RawUtf8;

function GetProxyForUri(const uri: RawUtf8; fromSystem: boolean): RawUtf8;
{$ifdef USEWININET}
var
  pi: TProxyInfo;
{$endif USEWININET}
begin
  if not _PROXYSET then
  begin
    _PROXYSAFE.Lock;
    StringToUtf8(GetEnvironmentVariable('HTTP_PROXY'),  _PROXY[false]);
    StringToUtf8(GetEnvironmentVariable('HTTPS_PROXY'), _PROXY[true]);
    if _PROXY[true] = '' then
      _PROXY[true] := _PROXY[false];
    _PROXYSET := true;
    _PROXYSAFE.UnLock;
  end;
  result := _PROXY[IdemPChar(pointer(uri), 'HTTPS://')];
  {$ifdef USEWININET}
  if (result = '') and
     fromsystem then
    // no environment variable was set -> try using mormot.lib.winhttp
    if WinHttpGetProxyInfo(Utf8ToSynUnicode(uri), pi) = 0 then
      result := SynUnicodeToUtf8(pi.URL);
  {$endif USEWININET}
end;

function GetSystemProxyUri(const uri, proxy: RawUtf8; var temp: TUri): PUri;
begin
  if (proxy <> '') and
     temp.From(proxy) then
    result := @temp
  else if DefaultHttpClientSocketProxy.Server <> '' then
    result := @DefaultHttpClientSocketProxy
  else if temp.From(GetProxyForUri(uri, DefaultHttpClientSocketProxyAuto)) then
    result := @temp
  else
    result := nil;
end;


{ THttpClientSocket }

constructor THttpClientSocket.Create(aTimeOut: PtrInt);
begin
  if aTimeOut = 0 then
    aTimeOut := HTTP_DEFAULT_RECEIVETIMEOUT;
  inherited Create(aTimeOut);
  fUserAgent := DefaultUserAgent(self);
  fAccept := '*/*';
end;

procedure THttpClientSocket.RequestInternal(
  var ctxt: TTHttpClientSocketRequestParams);

  procedure DoRetry(FatalError: integer;
    const Fmt: RawUtf8; const Args: array of const);
  var
    msg: RawUtf8;
  begin
    FormatUtf8(Fmt, Args, msg);
    //writeln('DoRetry ',retry, ' ', Error, ' / ', msg);
    if Assigned(OnLog) then
       OnLog(sllTrace, 'DoRetry % socket=% fatal=% retry=%',
         [msg, fSock.Socket, FatalError, BOOL_STR[rMain in ctxt.retry]], self);
    if rMain in ctxt.retry then
      // we should retry once -> return error only if failed twice
      ctxt.status := FatalError
    else
    begin
      // recreate the connection and try again
      Close;
      //if Assigned(OnLog) then
      //   OnLog(sllTrace, 'DoRetry after close', [], self);
      try
        OpenBind(fServer, fPort, {bind=}false, TLS.Enabled);
        HttpStateReset;
        include(ctxt.retry, rMain);
        RequestInternal(ctxt);
      except
        on Exception do
          ctxt.status := FatalError;
      end;
    end;
  end;

var
  P: PUtf8Char;
  pending: TCrtSocketPending;
  loerr: integer;
  dat: RawByteString;
  timer: TPrecisionTimer;
begin
  if Assigned(OnLog) then
  begin
    timer.Start;
    OnLog(sllTrace, 'RequestInternal % %:%/% flags=% retry=%', [ctxt.method,
      fServer, fPort, ctxt.url, ToText(Http.HeaderFlags), byte(ctxt.retry)], self);
  end;
  if SockIn = nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  Http.Content := '';
  if (hfConnectionClose in Http.HeaderFlags) or
     not SockIsDefined then
    DoRetry(HTTP_NOTFOUND, 'connection closed (keepalive or maxrequest)', [])
  else if SockReceivePending(0, @loerr) = cspSocketError then
    DoRetry(HTTP_NOTFOUND, 'connection broken (socketerror=%)', [loerr])
  else
    try
      try
        // send request - we use SockSend because writeln() is calling flush()
        // prepare headers
        RequestSendHeader(ctxt.url, ctxt.method);
        if ctxt.KeepAlive > 0 then
          SockSend(['Keep-Alive: ', ctxt.KeepAlive,
              #13#10'Connection: Keep-Alive'])
        else
          SockSend('Connection: Close');
        dat := ctxt.Data; // local var copy for Data to be compressed in-place
        if (dat <> '') or
           ((ctxt.method <> 'GET') and // no message body len/type for GET/HEAD
            (ctxt.method <> 'HEAD')) then
          CompressDataAndWriteHeaders(ctxt.DataType, dat, ctxt.InStream);
        if ctxt.header <> '' then
          SockSend(ctxt.header);
        if Http.CompressAcceptEncoding <> '' then
          SockSend(Http.CompressAcceptEncoding);
        SockSendCRLF;
        // flush headers and Data/InStream body
        SockSendFlush(dat);
        if ctxt.InStream <> nil then
        begin
          // InStream may be a THttpMultiPartStream -> Seek(0) calls Flush
          ctxt.InStream.Seek(0, soBeginning);
          SockSendStream(ctxt.InStream);
        end;
        // retrieve HTTP command line response
        pending := SockReceivePending(Timeout); // wait using select/poll
        if pending <> cspDataAvailable then
        begin
          DoRetry(HTTP_TIMEOUT, '% waiting %ms for headers',
            [GetEnumName(TypeInfo(TCrtSocketPending), ord(pending))^, TimeOut]);
          exit;
        end;
        SockRecvLn(Http.Command); // will raise ENetSock on any error
        P := pointer(Http.Command);
        if IdemPChar(P, 'HTTP/1.') then
        begin
          // get http numeric status code (200,404...) from 'HTTP/1.x ######'
          ctxt.status := GetCardinal(P + 9);
          if ctxt.status = 0 then
          begin
            ctxt.status := HTTP_HTTPVERSIONNONSUPPORTED;
            exit;
          end;
          if P[7] = '0' then
            // HTTP/1.0 -> force connection close
            ctxt.KeepAlive := 0;
        end
        else
        begin
          // error on reading answer -> 505=wrong format
          if Http.Command = '' then
            DoRetry(HTTP_TIMEOUT, 'Broken Link - timeout=%ms', [TimeOut])
          else
            DoRetry(HTTP_HTTPVERSIONNONSUPPORTED, 'Command=%', [Http.Command]);
          exit;
        end;
        // retrieve all HTTP headers
        GetHeader({unfiltered=}false);
        // retrieve Body content (if any)
        if (ctxt.status >= HTTP_SUCCESS) and
           (ctxt.status <> HTTP_NOCONTENT) and
           (ctxt.status <> HTTP_NOTMODIFIED) and
           not HttpMethodWithNoBody(ctxt.method) then
           // HEAD or status 100..109,204,304 -> no body (RFC 2616 section 4.3)
        begin
          if ctxt.OutStream <> nil then
          begin
            if (Http.ContentLength > 0) and
               (ctxt.Status in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) then
            begin
              if ctxt.OutStream.InheritsFrom(TStreamRedirect) then
                TStreamRedirect(ctxt.OutStream).ExpectedSize :=
                  fRangeStart + Http.ContentLength;
              GetBody(ctxt.OutStream)
            end;
          end
          else
            GetBody(nil);
        end;
        // successfully sent -> reset some fields for the next request
        if ctxt.Status in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then
          RequestClear;
      except
        on E: Exception do
          if E.InheritsFrom(ENetSock) or
             E.InheritsFrom(EHttpSocket) then
            // network layer problem - typically EHttpSocket
            DoRetry(HTTP_NOTFOUND, '% raised after % [%]',
              [E, ToText(ENetSock(E).LastError)^, E.Message])
          else
            // propagate custom exceptions to the caller (e.g. from progression)
            raise;
      end;
    finally
      if Assigned(OnLog) then
         OnLog(sllTrace, 'RequestInternal status=% keepalive=% flags=% in %',
           [ctxt.Status, ctxt.KeepAlive, ToText(Http.HeaderFlags), timer.Stop], self);
      if ctxt.KeepAlive = 0 then
        Close;
    end;
end;

procedure THttpClientSocket.RequestSendHeader(const url, method: RawUtf8);
begin
  if not SockIsDefined then
    exit;
  if SockIn = nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  if (url = '') or
     (url[1] <> '/') then
    SockSend([method, ' /', url, ' HTTP/1.1'])
  else
    SockSend([method, ' ', url, ' HTTP/1.1']);
  {$ifdef OSPOSIX}
  if SocketLayer = nlUnix then
    SockSend('Host: unix')
  else
  {$endif OSPOSIX}
  if Port = DEFAULT_PORT[TLS.Enabled] then
    SockSend(['Host: ', Server])
  else
    SockSend(['Host: ', Server, ':', Port]);
  if (fRangeStart > 0) or
     (fRangeEnd > 0) then
    if fRangeEnd > fRangeStart then
      SockSend(['Range: bytes=', fRangeStart, '-', fRangeEnd])
    else
      SockSend(['Range: bytes=', fRangeStart, '-']);
  if fAuthBearer <> '' then
    SockSend(['Authorization: Bearer ', fAuthBearer]);
  if fBasicAuthUserPassword <> '' then
    SockSend(['Authorization: Basic ', BinToBase64(fBasicAuthUserPassword)]);
  if fReferer <> '' then
    SockSend(['Referer: ', fReferer]);
  if fAccept <> '' then
    SockSend(['Accept: ', fAccept]);
  SockSend(['User-Agent: ', UserAgent]);
end;

procedure THttpClientSocket.RequestClear;
begin
  fRangeStart := 0;
  fRangeEnd := 0;
end;

function THttpClientSocket.Request(const url, method: RawUtf8;
  KeepAlive: cardinal; const header: RawUtf8; const Data: RawByteString;
  const DataType: RawUtf8; retry: boolean; InStream, OutStream: TStream): integer;
var
  ctxt: TTHttpClientSocketRequestParams;
  newuri: TUri;
begin
  ctxt.url := url;
  ctxt.method := method;
  ctxt.KeepAlive := KeepAlive;
  ctxt.header := header;
  ctxt.Data := Data;
  ctxt.DataType := DataType;
  ctxt.InStream := InStream;
  ctxt.OutStream := OutStream;
  if OutStream <> nil then
    ctxt.OutStreamInitialPos := OutStream.Position;
  ctxt.status := 0;
  ctxt.redirected := 0;
  if retry then
    ctxt.retry := [rMain]
  else
    ctxt.retry := [];
  if (not Assigned(fOnBeforeRequest)) or
     fOnBeforeRequest(self, ctxt) then
  begin
    fRedirected := '';
    repeat
      // this will handle the actual request, with proper retrial
      RequestInternal(ctxt);
      // handle optional (proxy) authentication callbacks
      if (ctxt.status = HTTP_UNAUTHORIZED) and
          Assigned(fOnAuthorize) then
      begin
        if assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [method, url, ctxt.status], self);
        if rAuth in ctxt.retry then
          break;
        include(ctxt.retry, rAuth);
        if fOnAuthorize(self, ctxt, Http.HeaderGetValue('WWW-AUTHENTICATE')) then
          continue;
      end
      else if (ctxt.status = HTTP_PROXYAUTHREQUIRED) and
          Assigned(fOnProxyAuthorize) then
      begin
        if assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [method, url, ctxt.status], self);
        if rAuthProxy in ctxt.retry then
          break;
        include(ctxt.retry, rAuthProxy);
        if fOnProxyAuthorize(self, ctxt, Http.HeaderGetValue('PROXY-AUTHENTICATE')) then
          continue;
      end;
      // handle redirection from returned headers
      if (ctxt.status < 300) or              // 300..399 are redirections
         (ctxt.status = HTTP_NOTMODIFIED) or // but 304 is not
         (ctxt.status > 399) or              // 400.. are errors
         (ctxt.redirected >= fRedirectMax) then
        break;
      if retry then
        ctxt.retry := [rMain]
      else
        ctxt.retry := [];
      ctxt.url := Http.HeaderGetValue('LOCATION');
      if assigned(OnLog) then
        OnLog(sllTrace, 'Request % % redirected to %', [method, url, ctxt.url], self);
      if IdemPChar(pointer(ctxt.url), 'HTTP') and
         newuri.From(ctxt.url) then
        if (hfConnectionClose in Http.HeaderFlags) or
           (newuri.Server <> Server) or
           (newuri.Port <> Port) or
           (newuri.Https <> TLS.Enabled) then
      begin
        Close; // relocated to another server -> reset the TCP connection
        try
          OpenBind(newuri.Server, newuri.Port, false, newuri.Https);
          fRedirected := newuri.Address;
        except
          ctxt.status := HTTP_NOTFOUND;
        end;
        HttpStateReset;
        ctxt.url := newuri.Address;
        if (OutStream <> nil) and
           (OutStream.Position <> ctxt.OutStreamInitialPos) then
        begin
          OutStream.Size := ctxt.OutStreamInitialPos; // truncate
          OutStream.Position := ctxt.OutStreamInitialPos; // reset position
        end;
      end;
      inc(ctxt.redirected);
    until false;
    if Assigned(fOnAfterRequest) then
      fOnAfterRequest(self, ctxt);
  end;
  result := ctxt.status;
end;

function THttpClientSocket.Get(const url: RawUtf8; KeepAlive: cardinal;
  const header: RawUtf8): integer;
begin
  result := Request(url, 'GET', KeepAlive, header);
end;

function THttpClientSocket.GetAuth(const url, AuthToken: RawUtf8;
  KeepAlive: cardinal): integer;
begin
  result := Get(url, KeepAlive, AuthorizationBearer(AuthToken));
end;

{$ifdef ISDELPHI20062007}
  {$warnings off} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function THttpClientSocket.WGet(const url: RawUtf8; const destfile: TFileName;
  var params: THttpClientSocketWGet): TFileName;
var
  size: Int64;
  cached, part: TFileName;
  requrl, parthash, urlfile: RawUtf8;
  res: integer;
  partstream: TStreamRedirect;
  resumed: boolean;
  ExpectedSize: Int64;

  procedure DoRequestAndFreePartStream;
  var
    modif: TDateTime;
    lastmod: RawUtf8;
  begin
    partstream.Context := urlfile;
    partstream.OnProgress := params.OnProgress;
    partstream.OnLog := OnLog;
    partstream.TimeOut := params.TimeOutSec * 1000;
    partstream.LimitPerSecond := params.LimitBandwith;
    res := Request(requrl, 'GET', params.KeepAlive, params.Header, '', '',
      {retry=}false, {instream=}nil, partstream);
    if not (res in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) then
    begin
      if (res = HTTP_NOTACCEPTABLE) or
         (res =  HTTP_RANGENOTSATISFIABLE) then
        DeleteFile(part); // force delete (maybe) incorrect partial file
      raise EHttpSocket.CreateUtf8('%.WGet: %:%/% failed as %',
        [self, fServer, fPort, requrl, StatusCodeToErrorMsg(res)]);
    end;
    partstream.Ended; // notify finished
    parthash := partstream.GetHash; // hash updated on each partstream.Write()
    FreeAndNil(partstream);
    lastmod := Http.HeaderGetValue('LAST-MODIFIED');
    if HttpDateToDateTime(lastmod, modif, {local=}true) then
      FileSetDate(part, DateTimeToFileDate(modif));
  end;

  function GetExpectedTargetSize(out Size: Int64): boolean;
  begin
    res := Head(requrl, params.KeepAlive, params.Header);
    if not (res in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) then
      raise EHttpSocket.CreateUtf8('%.WGet: %:%/% failed as %',
        [self, fServer, fPort, requrl, StatusCodeToErrorMsg(res)]);
    Size := Http.ContentLength;
    result := Size > 0;
    if result and
       (fRedirected <> '') and
       (fRedirected <> requrl) then
      requrl := fRedirected; // don't perform 302 twice
  end;

  procedure NewPartStream(Mode: cardinal);
  var
    redirected: TStream;
  begin
    if Assigned(params.OnStreamCreate) then
      redirected := params.OnStreamCreate(part, Mode)
    else
      redirected := TFileStream.Create(part, Mode);
    partstream := params.Hasher.Create(redirected);
  end;

begin
  result := destfile;
  requrl := url;
  urlfile := SplitRight(url, '/');
  if urlfile = '' then
    urlfile := 'index';
  if result = '' then
    result := GetSystemPath(spTempFolder) + Utf8ToString(urlfile);
  TrimSelf(params.Hash);
  if params.HashFromServer and
     Assigned(params.Hasher) and
     (params.Hash = '') then
    begin
      // try to retrieve the hash from the HTTP server
      parthash := params.Hasher.GetHashFileExt;
      if parthash <> '' then
      begin
        parthash := url + parthash; // e.g. 'files/somefile.zip.md5'
        if Get(parthash, 5000) = 200 then
          // handle 'c7d8e61e82a14404169af3fa5a72be85 *file.name' format
          params.Hash := Split(TrimU(Http.Content), ' ');
        if Assigned(OnLog) then
          OnLog(sllTrace, 'WGet: hash from % = %', [parthash, params.Hash], self);
      end;
    end;
  if (params.HashCacheDir <> '') and
     DirectoryExists(params.HashCacheDir) then
    cached := IncludeTrailingPathDelimiter(params.HashCacheDir) +
              ExtractFileName(result);
  if (destfile <> '') and
     Assigned(params.Hasher) and
     (params.Hash <> '') then
    // check if we already got the file from its md5/sha* hash
    if FileExists(destfile) and
       IdemPropNameU(params.Hasher.HashFile(result), params.Hash) then
    begin
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: % already available', [url, result], self);
      exit;
    end
    else if cached <> '' then
    begin
      // check from local cache folder
      if IdemPropNameU(params.Hasher.HashFile(cached), params.Hash) then
      begin
        if Assigned(OnLog) then
          OnLog(sllTrace, 'WGet %: copy from cached %', [url, cached], self);
        if not CopyFile(cached, result, {failexists=}false) then
          raise EHttpSocket.CreateUtf8('%.WGet: copy from % cache failed',
            [self, cached]);
        exit;
      end;
    end;
  // we need to download the file
  if not Assigned(params.Hasher) then
    params.Hasher := TStreamRedirect; // no hash by default
  if FileExists(result) then
    if not DeleteFile(result) or
       FileExists(result) then
      raise EHttpSocket.CreateUtf8(
        '%.WGet: impossible to delete deprecated %', [self, result]);
  part := result + '.part';
  size := FileSize(part);
  resumed := params.Resume;
  if (size > 0) and
     resumed then
  begin
    if Assigned(OnLog) then
      OnLog(sllTrace, 'WGet %: resume % (%)', [url, part, KB(size)], self);
    // try to get expected target size with a HEAD request
    if GetExpectedTargetSize(ExpectedSize) and
       (Size < ExpectedSize) then
    begin // seems good enough
      NewPartStream(fmOpenReadWrite);
      partstream.Append; // hash partial content
      fRangeStart := size;
    end
    else
    begin
      resumed := false;
      DeleteFile(part); // this .part is too big, so should be avoided
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: got Size=% Expected=% -> reset %',
          [url, Size, ExpectedSize, part], self);
      NewPartStream(fmCreate);
    end;
  end
  else
  begin
    resumed := false;
    if Assigned(OnLog) then
      OnLog(sllTrace, 'WGet %: start downloading %', [url, part], self);
    NewPartStream(fmCreate);
  end;
  try
    DoRequestAndFreePartStream;
    if (params.Hash <> '') and
       (parthash <> '') then
    begin
      // check the hash
      if resumed and
         not IdemPropNameU(parthash, params.Hash) then
      begin
        if Assigned(OnLog) then
          OnLog(sllDebug,
            'WGet %: wrong hash after resume -> reset and retry', [url]);
        NewPartStream(fmCreate);
        requrl := url; // try again including initial redirection steps
        DoRequestAndFreePartStream;
      end;
      if not IdemPropNameU(parthash, params.Hash) then
      begin
        DeleteFile(part); // this .part was clearly incorrect
        raise EHttpSocket.CreateUtf8('%.WGet: %:%/% hash failure (% vs %)',
          [self, fServer, fPort, url, parthash, params.Hash]);
      end;
    end;
    if cached <> '' then
    begin
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: copy into cached %', [url, cached]);
      CopyFile(part, cached, {failsexist=}false);
    end;
    if not RenameFile(part, result) then
      raise EHttpSocket.CreateUtf8(
        '%.WGet: impossible to rename % as %', [self, part, result]);
    part := '';
  finally
    partstream.Free;  // close file on unexpected error
    if (part <> '') and
       not params.Resume then
      DeleteFile(part); // force next attempt from scratch if resume is not set
  end;
end;

{$ifdef ISDELPHI20062007}
  {$warnings on} // avoid paranoid Delphi 2007 warning
{$endif ISDELPHI20062007}

function THttpClientSocket.Head(const url: RawUtf8; KeepAlive: cardinal;
  const header: RawUtf8): integer;
begin
  result := Request(url, 'HEAD', KeepAlive, header);
end;

function THttpClientSocket.Post(const url: RawUtf8; const Data: RawByteString;
  const DataType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'POST', KeepAlive, header, Data, DataType);
end;

function THttpClientSocket.Post(const url: RawUtf8; Data: TStream;
  const DataType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'POST', KeepAlive, header, '', DataType, false, Data);
end;

function THttpClientSocket.Put(const url: RawUtf8; const Data: RawByteString;
  const DataType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'PUT', KeepAlive, header, Data, DataType);
end;

function THttpClientSocket.Delete(const url: RawUtf8; KeepAlive: cardinal;
  const header: RawUtf8): integer;
begin
  result := Request(url, 'DELETE', KeepAlive, header);
end;

{$ifdef DOMAINRESTAUTH}

// see https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication

procedure DoSspi(Sender: THttpClientSocket;
  var Context: TTHttpClientSocketRequestParams;
  const Authenticate, InHeaderUp, OutHeader: RawUtf8);
var
  sc: TSecContext;
  bak: RawUtf8;
  unauthstatus: integer;
  datain, dataout: RawByteString;
begin
  if (Sender = nil) or
     not IdemPChar(pointer(Authenticate), pointer(SECPKGNAMEHTTP_UPPER)) then
    exit;
  unauthstatus := Context.status;
  bak := Context.header;
  InvalidateSecContext(sc, 0);
  try
    repeat
      FindNameValue(Sender.Http.Headers, pointer(InHeaderUp), RawUtf8(datain));
      datain := Base64ToBin(TrimU(datain));
      ClientSspiAuth(sc, datain, Sender.AuthorizeSspiSpn, dataout);
      if dataout = '' then
        break;
      Context.header := OutHeader + BinToBase64(dataout);
      if bak <> '' then
        Context.header := Context.header + #13#10 + bak;
      Sender.RequestInternal(Context);
    until Context.status <> unauthstatus;
    // here Context is the final answer (sucess or auth error) from the server
  finally
    FreeSecContext(sc);
    if Assigned(Sender.OnLog) then
      Sender.OnLog(sllDebug, 'DoSspi %%', [OutHeader, Context.status], Sender);
    Context.header := bak;
  end;
end;

class function THttpClientSocket.AuthorizeSspi(Sender: THttpClientSocket;
  var Context: TTHttpClientSocketRequestParams; const Authenticate: RawUtf8): boolean;
begin
  if InitializeDomainAuth then
    // try to setup sspi/gssapi -> SECPKGNAMEHTTP
    DoSspi(Sender, Context, Authenticate,
      'WWW-AUTHENTICATE: ' + SECPKGNAMEHTTP_UPPER + ' ',
      'Authorization: ' + SECPKGNAMEHTTP + ' ');
  result := false; // final RequestInternal() was done within DoSspi()
end;

class function THttpClientSocket.ProxyAuthorizeSspi(Sender: THttpClientSocket;
  var Context: TTHttpClientSocketRequestParams; const Authenticate: RawUtf8): boolean;
begin
  if InitializeDomainAuth then
    // try to setup sspi/gssapi -> SECPKGNAMEHTTP
    DoSspi(Sender, Context, Authenticate,
      'PROXY-AUTHENTICATE: ' + SECPKGNAMEHTTP_UPPER + ' ',
      'Proxy-Authorization: ' + SECPKGNAMEHTTP + ' ');
  result := false; // final RequestInternal() was done within DoSspi()
end;

{$endif DOMAINRESTAUTH}

function OpenHttp(const aServer, aPort: RawUtf8; aTLS: boolean;
  aLayer: TNetLayer; const aUrlForProxy: RawUtf8): THttpClientSocket;
var
  temp: TUri;
begin
  try
    result := THttpClientSocket.Open(aServer, aPort, aLayer, 0, aTLS, nil,
      GetSystemProxyUri(aUrlForProxy, '', temp));
  except
    on ENetSock do
      result := nil;
  end;
end;

function OpenHttp(const aUri: RawUtf8; aAddress: PRawUtf8): THttpClientSocket;
var
  URI: TUri;
begin
  result := nil;
  if URI.From(aUri) then
  begin
    result := OpenHttp(URI.Server, URI.Port, URI.Https, URI.Layer);
    if aAddress <> nil then
      aAddress^ := URI.Address;
  end;
end;

function OpenHttpGet(const server, port, url, inHeaders: RawUtf8;
  outHeaders: PRawUtf8; aLayer: TNetLayer; aTLS: boolean;
  outStatus: PInteger): RawByteString;
var
  Http: THttpClientSocket;
  status: integer;
begin
  result := '';
  Http := OpenHttp(server, port, aTLS, aLayer);
  if Http <> nil then
  try
    Http.RedirectMax := 5;
    status := Http.Get(url, 0, inHeaders);
    if outStatus <> nil then
      outStatus^ := status;
    if status in [HTTP_SUCCESS..HTTP_PARTIALCONTENT] then
    begin
      result := Http.Http.Content;
      if outHeaders <> nil then
        outHeaders^ := Http.Http.Headers;
    end;
  finally
    Http.Free;
  end;
end;


{ ******************** THttpRequest Abstract HTTP client class }

{ THttpRequest }

class function THttpRequest.InternalREST(const url, method: RawUtf8;
  const data: RawByteString; const header: RawUtf8; aIgnoreSSLCertificateErrors: boolean;
  outHeaders: PRawUtf8; outStatus: PInteger): RawByteString;
var
  uri: TUri;
  outh: RawUtf8;
  status: integer;
begin
  result := '';
  with uri do
    if From(url) then
    try
      with self.Create(Server, Port, Https, '', '', 0, 0, 0, Layer) do
      try
        IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
        status := Request(Address, method, 0, header, data, '', outh, result);
        if outStatus <> nil then
          outStatus^ := status;
        if outHeaders <> nil then
          outHeaders^ := outh;
      finally
        Free;
      end;
    except
      result := '';
    end;
end;

constructor THttpRequest.Create(const aServer, aPort: RawUtf8; aHttps: boolean;
  const aProxyName: RawUtf8; const aProxyByPass: RawUtf8; ConnectionTimeOut,
  SendTimeout, ReceiveTimeout: cardinal; aLayer: TNetLayer);
begin
  fLayer := aLayer;
  if fLayer <> nlUnix then
  begin
    fPort := GetCardinal(pointer(aPort));
    if fPort = 0 then
      if aHttps then
        fPort := 443
      else
        fPort := 80;
  end;
  fServer := aServer;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fExtendedOptions.UserAgent := DefaultUserAgent(self);
  if ConnectionTimeOut = 0 then
    ConnectionTimeOut := HTTP_DEFAULT_CONNECTTIMEOUT;
  if SendTimeout = 0 then
    SendTimeout := HTTP_DEFAULT_SENDTIMEOUT;
  if ReceiveTimeout = 0 then
    ReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT;
  InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout); // raise an exception on error
end;

constructor THttpRequest.Create(const aUri: RawUtf8; const aProxyName: RawUtf8;
  const aProxyByPass: RawUtf8; ConnectionTimeOut: cardinal; SendTimeout: cardinal;
  ReceiveTimeout: cardinal; aIgnoreSSLCertificateErrors: boolean);
var
  uri: TUri;
begin
  if not uri.From(aUri) then
    raise EHttpSocket.CreateUtf8('%.Create: invalid url=%', [self, aUri]);
  IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
  Create(uri.Server, uri.Port, uri.Https, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, uri.Layer);
end;

function THttpRequest.Request(const url, method: RawUtf8; KeepAlive: cardinal;
  const InHeader: RawUtf8; const InData: RawByteString; const InDataType: RawUtf8;
  out OutHeader: RawUtf8; out OutData: RawByteString): integer;
var
  aData: RawByteString;
  aDataEncoding, aAcceptEncoding, aUrl: RawUtf8;
  i: integer;
begin
  if (url = '') or
     (url[1] <> '/') then
    aUrl := '/' + url
  else // need valid url according to the HTTP/1.1 RFC
    aUrl := url;
  fKeepAlive := KeepAlive;
  InternalCreateRequest(method, aUrl); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType <> '' then
      InternalAddHeader(RawUtf8('Content-Type: ') + InDataType);
    // handle custom compression
    aData := InData;
    if integer(fCompressAcceptHeader) <> 0 then
    begin
      aDataEncoding := CompressContent(fCompressAcceptHeader,
        fCompress, InDataType, aData);
      if aDataEncoding <> '' then
        InternalAddHeader(RawUtf8('Content-Encoding: ') + aDataEncoding);
    end;
    if fCompressAcceptEncoding <> '' then
      InternalAddHeader(fCompressAcceptEncoding);
    // send request to remote server
    InternalSendRequest(method, aData);
    // retrieve status and headers
    result := InternalRetrieveAnswer(OutHeader, aDataEncoding, aAcceptEncoding, OutData);
    // handle incoming answer compression
    if OutData <> '' then
    begin
      if aDataEncoding <> '' then
        for i := 0 to high(fCompress) do
          with fCompress[i] do
            if Name = aDataEncoding then
              if Func(OutData, false) = '' then
                raise EHttpSocket.CreateUtf8(
                  '%.Request: % uncompress error', [self, Name])
              else
                break; // successfully uncompressed content
      if aAcceptEncoding <> '' then
        fCompressAcceptHeader := ComputeContentEncoding(
          fCompress, pointer(aAcceptEncoding));
    end;
  finally
    InternalCloseRequest;
  end;
end;

class function THttpRequest.Get(const aUri: RawUtf8; const aHeader: RawUtf8;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUtf8; outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aUri, 'GET', '', aHeader, aIgnoreSSLCertificateErrors,
    outHeaders, outStatus);
end;

class function THttpRequest.Post(const aUri: RawUtf8; const aData: RawByteString;
  const aHeader: RawUtf8; aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUtf8;
  outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aUri, 'POST', aData, aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

class function THttpRequest.Put(const aUri: RawUtf8; const aData: RawByteString;
  const aHeader: RawUtf8; aIgnoreSSLCertificateErrors: boolean; outHeaders:
  PRawUtf8; outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aUri, 'PUT', aData, aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

class function THttpRequest.Delete(const aUri: RawUtf8; const aHeader: RawUtf8;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUtf8;
  outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aUri, 'DELETE', '', aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

function THttpRequest.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress, aFunction,
    fCompressAcceptEncoding, aCompressMinSize) <> '';
end;



{$ifdef USEWININET}

{ ******************** TWinHttp TWinINet TWinHttpWebSocketClient }

{ TWinHttpApi }

function TWinHttpApi.InternalRetrieveAnswer(var Header, Encoding,
  AcceptEncoding: RawUtf8; var Data: RawByteString): integer;
var
  ChunkSize, Bytes, ContentLength, Read: cardinal;
  tmp: RawByteString;
begin
  // HTTP_QUERY* and WINHTTP_QUERY* do match -> common to TWinINet + TWinHttp
  result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
  Header := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
  Encoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
  AcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
  // retrieve received content (if any)
  Read := 0;
  ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
  if Assigned(fOnProgress) then
    fOnProgress(self, 0, ContentLength); // initial notification
  if Assigned(fOnDownload) then
    // download per-chunk using calback event
    repeat
      Bytes := InternalQueryDataAvailable;
      if Bytes = 0 then
        break;
      if integer(Bytes) > Length({%H-}tmp) then
      begin
        ChunkSize := fOnDownloadChunkSize;
        if ChunkSize <= 0 then
          ChunkSize := 65536; // 64KB seems fair enough by default
        if Bytes > ChunkSize then
          ChunkSize := Bytes;
        SetLength(tmp, ChunkSize);
      end;
      Bytes := InternalReadData(tmp, 0, Bytes);
      if Bytes = 0 then
        break;
      inc(Read, Bytes);
      if not fOnDownload(self, Read, ContentLength, Bytes, pointer(tmp)^) then
        break; // returned false = aborted
      if Assigned(fOnProgress) then
        fOnProgress(self, Read, ContentLength);
    until false
  else if ContentLength <> 0 then
  begin
    // optimized version reading "Content-Length: xxx" bytes
    SetLength(Data, ContentLength);
    repeat
      Bytes := InternalQueryDataAvailable;
      if Bytes = 0 then
      begin
        SetLength(Data, Read); // truncated content
        break;
      end;
      Bytes := InternalReadData(Data, Read, Bytes);
      if Bytes = 0 then
      begin
        SetLength(Data, Read); // truncated content
        break;
      end;
      inc(Read, Bytes);
      if Assigned(fOnProgress) then
        fOnProgress(self, Read, ContentLength);
    until Read = ContentLength;
  end
  else
  begin
    // Content-Length not set: read response in blocks of HTTP_RESP_BLOCK_SIZE
    repeat
      Bytes := InternalQueryDataAvailable;
      if Bytes = 0 then
        break;
      SetLength(Data, Read + Bytes{HTTP_RESP_BLOCK_SIZE});
      Bytes := InternalReadData(Data, Read, Bytes);
      if Bytes = 0 then
        break;
      inc(Read, Bytes);
      if Assigned(fOnProgress) then
        fOnProgress(self, Read, ContentLength);
    until false;
    SetLength(Data, Read);
  end;
  if Assigned(fOnProgress) then
    fOnProgress(self, Read, Read); // final notification
end;

class function TWinHttpApi.IsAvailable: boolean;
begin
  result := true; // both WinINet and WinHttp are statically linked
end;




{ TWinHttp }

function TWinHttp.InternalGetProtocols: cardinal;
begin
  // WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 and WINHTTP_FLAG_SECURE_PROTOCOL_SSL3
  // are unsafe, disabled at Windows level, therefore never supplied
  result := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
  // Windows 7 and newer support TLS 1.1 & 1.2
  if OSVersion >= wSeven then
    result := result or
              WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or
              WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
end;

procedure TWinHttp.InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
var
  Callback: WINHTTP_STATUS_CALLBACK;
  CallbackRes: PtrInt absolute Callback; // for FPC compatibility
  access, protocols: cardinal;
begin
  if fProxyName = '' then
    if OSVersion >= wEightOne then
      access := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY // Windows 8.1 and newer
    else
      access := WINHTTP_ACCESS_TYPE_NO_PROXY
  else
    access := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  fSession := WinHttpApi.Open(
    pointer(Utf8ToSynUnicode(fExtendedOptions.UserAgent)),
    access,
    pointer(Utf8ToSynUnicode(fProxyName)),
    pointer(Utf8ToSynUnicode(fProxyByPass)), 0);
  if fSession = nil then
    RaiseLastModuleError(winhttpdll, EWinHttp);
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpApi.SetTimeouts(fSession, HTTP_DEFAULT_RESOLVETIMEOUT,
     ConnectionTimeOut, SendTimeout, ReceiveTimeout) then
    RaiseLastModuleError(winhttpdll, EWinHttp);
  if fHTTPS then
  begin
    protocols := InternalGetProtocols;
    if not WinHttpApi.SetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
        @protocols, SizeOf(protocols)) then
      RaiseLastModuleError(winhttpdll, EWinHttp);
    Callback := WinHttpApi.SetStatusCallback(fSession,
      WinHttpSecurityErrorCallback, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
      RaiseLastModuleError(winhttpdll, EWinHttp);
  end;
  fConnection := WinHttpApi.Connect(
    fSession, pointer(Utf8ToSynUnicode(fServer)), fPort, 0);
  if fConnection = nil then
    RaiseLastModuleError(winhttpdll, EWinHttp);
end;

procedure TWinHttp.InternalCreateRequest(const aMethod, aUrl: RawUtf8);
const
  ALL_ACCEPT: array[0..1] of PWideChar = (
    '*/*', nil);
  ACCEPT_TYPES: array[boolean] of pointer = (
    @ALL_ACCEPT, nil);
var
  Flags: cardinal;
begin
  Flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
  if fHttps then
    Flags := Flags or WINHTTP_FLAG_SECURE;
  fRequest := WinHttpApi.OpenRequest(fConnection, pointer(Utf8ToSynUnicode(aMethod)),
    pointer(Utf8ToSynUnicode(aUrl)), nil, nil, ACCEPT_TYPES[fNoAllAccept], Flags);
  if fRequest = nil then
    RaiseLastModuleError(winhttpdll, EWinHttp);
  if fKeepAlive = 0 then
  begin
    Flags := WINHTTP_DISABLE_KEEP_ALIVE;
    if not WinHttpApi.SetOption(
       fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @Flags, SizeOf(Flags)) then
      RaiseLastModuleError(winhttpdll, EWinHttp);
  end;
end;

procedure TWinHttp.InternalCloseRequest;
begin
  if fRequest <> nil then
  begin
    WinHttpApi.CloseHandle(fRequest);
    FRequest := nil;
  end;
end;

procedure TWinHttp.InternalAddHeader(const hdr: RawUtf8);
begin
  if (hdr <> '') and
     not WinHttpApi.AddRequestHeaders(FRequest,
     Pointer(Utf8ToSynUnicode(hdr)), length(hdr), WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastModuleError(winhttpdll, EWinHttp);
end;

procedure TWinHttp.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);

  function _SendRequest(L: cardinal): boolean;
  var
    Bytes, Current, Max, BytesWritten: cardinal;
  begin
    if Assigned(fOnUpload) and
       (IdemPropNameU(aMethod, 'POST') or
        IdemPropNameU(aMethod, 'PUT')) then
    begin
      result := WinHttpApi.SendRequest(
        fRequest, nil, 0, nil, 0, L, 0);
      if result then
      begin
        Current := 0;
        while Current < L do
        begin
          Bytes := fOnDownloadChunkSize;
          if Bytes <= 0 then
            Bytes := 65536; // 64KB seems fair enough by default
          Max := L - Current;
          if Bytes > Max then
            Bytes := Max;
          if not WinHttpApi.WriteData(fRequest, @PByteArray(aData)[Current],
             Bytes, BytesWritten) then
            RaiseLastModuleError(winhttpdll, EWinHttp);
          inc(Current, BytesWritten);
          if not fOnUpload(Self, Current, L) then
            raise EWinHttp.CreateUtf8('%: OnUpload canceled %', [self, aMethod]);
        end;
      end;
    end
    else
      result := WinHttpApi.SendRequest(
        fRequest, nil, 0, pointer(aData), L, L, 0);
  end;

var
  L: integer;
  winAuth: cardinal;
begin
  with fExtendedOptions do
    if AuthScheme <> wraNone then
    begin
      case AuthScheme of
        wraBasic:
          winAuth := WINHTTP_AUTH_SCHEME_BASIC;
        wraDigest:
          winAuth := WINHTTP_AUTH_SCHEME_DIGEST;
        wraNegotiate:
          winAuth := WINHTTP_AUTH_SCHEME_NEGOTIATE;
      else
        raise EWinHttp.CreateUtf8(
          '%: unsupported AuthScheme=%', [self, ord(AuthScheme)]);
      end;
      if not WinHttpApi.SetCredentials(fRequest, WINHTTP_AUTH_TARGET_SERVER,
         winAuth, pointer(AuthUserName), pointer(AuthPassword), nil) then
        RaiseLastModuleError(winhttpdll, EWinHttp);
    end;
  if fHTTPS and
     IgnoreSSLCertificateErrors then
    if not WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
      RaiseLastModuleError(winhttpdll, EWinHttp);
  L := length(aData);
  if _SendRequest(L) and
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success
  if fHTTPS and
     (GetLastError = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) and
     IgnoreSSLCertificateErrors and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
       pointer(WINHTTP_NO_CLIENT_CERT_CONTEXT), 0) and
     _SendRequest(L) and
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success with no certificate validation
  // if we reached here, an error occured
  RaiseLastModuleError(winhttpdll, EWinHttp);
end;

function TWinHttp.InternalGetInfo(Info: cardinal): RawUtf8;
var
  dwSize, dwIndex: cardinal;
  tmp: TSynTempBuffer;
  i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpApi.QueryHeaders(fRequest, Info, nil, nil, dwSize, dwIndex) and
     (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    tmp.Init(dwSize);
    if WinHttpApi.QueryHeaders(fRequest, Info, nil, tmp.buf, dwSize, dwIndex) then
    begin
      dwSize := dwSize shr 1;
      SetLength(result, dwSize);
      for i := 0 to dwSize - 1 do // fast ANSI 7-bit conversion
        PByteArray(result)^[i] := PWordArray(tmp.buf)^[i];
    end;
    tmp.Done;
  end;
end;

function TWinHttp.InternalGetInfo32(Info: cardinal): cardinal;
var
  dwSize, dwIndex: cardinal;
begin
  dwSize := SizeOf(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpApi.QueryHeaders(fRequest, Info, nil, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinHttp.InternalQueryDataAvailable: cardinal;
begin
  if not WinHttpApi.QueryDataAvailable(fRequest, result) then
    RaiseLastModuleError(winhttpdll, EWinHttp);
end;

function TWinHttp.InternalReadData(var Data: RawByteString; Read: integer;
  Size: cardinal): cardinal;
begin
  if not WinHttpApi.ReadData(fRequest, @PByteArray(Data)[Read], Size, result) then
    RaiseLastModuleError(winhttpdll, EWinHttp);
end;

destructor TWinHttp.Destroy;
begin
  if fConnection <> nil then
    WinHttpApi.CloseHandle(fConnection);
  if fSession <> nil then
    WinHttpApi.CloseHandle(fSession);
  inherited Destroy;
end;


{ EWinINet }

class procedure EWinINet.RaiseFromLastError;
var
  err: integer;
  E: EWinINet;
begin
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  err := GetLastError;
  E := CreateFmt('%s (%x)', [SysErrorMessageWinInet(err), err]);
  E.fLastError := err;
  raise E;
end;


{ TWinINet }

procedure TWinINet.InternalConnect(
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
var
  OpenType: integer;
begin
  if fProxyName = '' then
    OpenType := INTERNET_OPEN_TYPE_PRECONFIG
  else
    OpenType := INTERNET_OPEN_TYPE_PROXY;
  fSession := InternetOpenA(Pointer(fExtendedOptions.UserAgent), OpenType,
    pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession = nil then
    EWinINet.RaiseFromLastError;
  InternetSetOption(fConnection, INTERNET_OPTION_CONNECT_TIMEOUT,
    @ConnectionTimeOut, SizeOf(ConnectionTimeOut));
  InternetSetOption(fConnection, INTERNET_OPTION_SEND_TIMEOUT,
    @SendTimeout, SizeOf(SendTimeout));
  InternetSetOption(fConnection, INTERNET_OPTION_RECEIVE_TIMEOUT,
    @ReceiveTimeout, SizeOf(ReceiveTimeout));
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort,
    nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection = nil then
    EWinINet.RaiseFromLastError;
end;

procedure TWinINet.InternalCreateRequest(const aMethod, aUrl: RawUtf8);
const
  ALL_ACCEPT: array[0..1] of PAnsiChar = (
    '*/*', nil);
  ACCEPT_TYPES: array[boolean] of pointer = (
    @ALL_ACCEPT, nil);
var
  Flags: cardinal;
begin
  Flags := INTERNET_FLAG_HYPERLINK or INTERNET_FLAG_PRAGMA_NOCACHE or
    INTERNET_FLAG_RESYNCHRONIZE; // options for a true RESTful request
  if fKeepAlive <> 0 then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
  if fHttps then
    Flags := Flags or INTERNET_FLAG_SECURE;
  FRequest := HttpOpenRequestA(FConnection, Pointer(aMethod), Pointer(aUrl),
    nil, nil, ACCEPT_TYPES[fNoAllAccept], Flags, 0);
  if FRequest = nil then
    EWinINet.RaiseFromLastError;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest <> nil then
  begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;

procedure TWinINet.InternalAddHeader(const hdr: RawUtf8);
begin
  if (hdr <> '') and
     not HttpAddRequestHeadersA(fRequest, Pointer(hdr), length(hdr),
       HTTP_ADDREQ_FLAG_COALESCE) then
    EWinINet.RaiseFromLastError;
end;

procedure TWinINet.InternalSendRequest(const aMethod: RawUtf8; const aData:
  RawByteString);
var
  buff: TInternetBuffersA;
  datapos, datalen, max, Bytes, BytesWritten: cardinal;
begin
  datalen := length(aData);
  if (datalen > 0) and
     Assigned(fOnUpload) then
  begin
    FillCharFast(buff, SizeOf(buff), 0);
    buff.dwStructSize := SizeOf(buff);
    buff.dwBufferTotal := Length(aData);
    if not HttpSendRequestExA(fRequest, @buff, nil, 0, 0) then
      EWinINet.RaiseFromLastError;
    datapos := 0;
    while datapos < datalen do
    begin
      Bytes := fOnDownloadChunkSize;
      if Bytes <= 0 then
        Bytes := 65536; // 64KB seems fair enough by default
      max := datalen - datapos;
      if Bytes > max then
        Bytes := max;
      if not InternetWriteFile(fRequest,
         @PByteArray(aData)[datapos], Bytes, BytesWritten) then
        EWinINet.RaiseFromLastError;
      inc(datapos, BytesWritten);
      if not fOnUpload(Self, datapos, datalen) then
        raise EWinINet.CreateFmt('OnUpload Canceled %s', [aMethod]);
    end;
    if not HttpEndRequest(fRequest, nil, 0, 0) then
      EWinINet.RaiseFromLastError;
  end
  else
  // blocking send with no callback
  if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    EWinINet.RaiseFromLastError;
end;

function TWinINet.InternalGetInfo(Info: cardinal): RawUtf8;
var
  dwSize, dwIndex: cardinal;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not HttpQueryInfoA(fRequest, Info, nil, dwSize, dwIndex) and
     (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    SetLength(result, dwSize - 1);
    if not HttpQueryInfoA(fRequest, Info, pointer(result), dwSize, dwIndex) then
      result := '';
  end;
end;

function TWinINet.InternalGetInfo32(Info: cardinal): cardinal;
var
  dwSize, dwIndex: cardinal;
begin
  dwSize := SizeOf(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalQueryDataAvailable: cardinal;
begin
  if not InternetQueryDataAvailable(fRequest, result, 0, 0) then
    EWinINet.RaiseFromLastError;
end;

function TWinINet.InternalReadData(var Data: RawByteString; Read: integer; Size:
  cardinal): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], Size, result) then
    EWinINet.RaiseFromLastError;
end;

destructor TWinINet.Destroy;
begin
  if fConnection <> nil then
    InternetCloseHandle(FConnection);
  if fSession <> nil then
    InternetCloseHandle(FSession);
  inherited Destroy;
end;


{ TWinHttpUpgradeable }

function TWinHttpUpgradeable.InternalRetrieveAnswer(
  var Header, Encoding, AcceptEncoding: RawUtf8;
  var Data: RawByteString): integer;
begin
  result := inherited InternalRetrieveAnswer(Header, Encoding, AcceptEncoding, Data);
end;

procedure TWinHttpUpgradeable.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);
begin
  inherited InternalSendRequest(aMethod, aData);
end;

constructor TWinHttpUpgradeable.Create(const aServer, aPort: RawUtf8;
  aHttps: boolean; const aProxyName, aProxyByPass: RawUtf8;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal; aLayer: TNetLayer);
begin
  inherited Create(aServer, aPort, aHttps, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, aLayer);
end;


{ TWinHttpWebSocketClient }

function TWinHttpWebSocketClient.CheckSocket: boolean;
begin
  result := fSocket <> nil;
end;

constructor TWinHttpWebSocketClient.Create(const aServer, aPort: RawUtf8;
  aHttps: boolean; const url, aSubProtocol, aProxyName, aProxyByPass: RawUtf8;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
var
  _http: TWinHttpUpgradeable;
  inH, outH: RawUtf8;
  outD: RawByteString;
begin
  fSocket := nil;
  _http := TWinHttpUpgradeable.Create(aServer, aPort, aHttps, aProxyName,
    aProxyByPass, ConnectionTimeOut, SendTimeout, ReceiveTimeout);
  try
    // WebSocketApi.BeginClientHandshake()
    if aSubProtocol <> '' then
      inH := HTTP_WEBSOCKET_PROTOCOL + ': ' + aSubProtocol
    else
      inH := '';
    if _http.Request(url, 'GET', 0, inH, '', '', outH, outD) = 101 then
      fSocket := _http.fSocket
    else
      raise EWinHttp.CreateUtf8('%.Create: % handshake failed', [self, _http]);
  finally
    _http.Free;
  end;
end;

function TWinHttpWebSocketClient.Send(aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
  aBuffer: pointer; aBufferLength: cardinal): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpApi.WebSocketSend(
      fSocket, aBufferType, aBuffer, aBufferLength);
end;

function TWinHttpWebSocketClient.Receive(
  aBuffer: pointer; aBufferLength: cardinal;
  out aBytesRead: cardinal;
  out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpApi.WebSocketReceive(fSocket, aBuffer, aBufferLength,
      aBytesRead, aBufferType);
end;

function TWinHttpWebSocketClient.CloseConnection(
  const aCloseReason: RawUtf8): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpApi.WebSocketClose(fSocket, WEB_SOCKET_SUCCESS_CLOSE_STATUS,
      Pointer(aCloseReason), Length(aCloseReason));
  if result = 0 then
    fSocket := nil;
end;

destructor TWinHttpWebSocketClient.Destroy;
const
  CloseReason: PAnsiChar = 'object is destroyed';
var
  status: Word;
  reason: RawUtf8;
  reasonLength: cardinal;
begin
  if CheckSocket then
  begin
    // todo: check result
    WinHttpApi.WebSocketClose(fSocket, WEB_SOCKET_ABORTED_CLOSE_STATUS,
      Pointer(CloseReason), Length(CloseReason));
    SetLength(reason, WEB_SOCKET_MAX_CLOSE_REASON_LENGTH);
    WinHttpApi.WebSocketQueryCloseStatus(fSocket, status, Pointer(reason),
      WEB_SOCKET_MAX_CLOSE_REASON_LENGTH, reasonLength);
    WinHttpApi.CloseHandle(fSocket);
  end;
  inherited Destroy;
end;

{$endif USEWININET}


{$ifdef USELIBCURL}

{ TCurlHttp }

procedure TCurlHttp.InternalConnect(ConnectionTimeOut, SendTimeout,
  ReceiveTimeout: cardinal);
const
  HTTPS: array[boolean] of string[1] = (
    '',
    's');
begin
  if not IsAvailable then
    raise ECurlHttp.CreateFmt('No available %s', [LIBCURL_DLL]);
  fHandle := curl.easy_init;
  if curl.globalShare <> nil then
    curl.easy_setopt(fHandle, coShare, curl.globalShare);
  curl.easy_setopt(fHandle, coConnectTimeoutMs, ConnectionTimeOut); // default=300 !
  if SendTimeout < ReceiveTimeout then
    SendTimeout := ReceiveTimeout;
  if SendTimeout <> 0 then // prevent send+receive forever
    curl.easy_setopt(fHandle, coTimeoutMs, SendTimeout);
  // coTimeout=CURLOPT_TIMEOUT is global for the transfer, so shouldn't be used
  if fLayer = nlUnix then
    // see CURLOPT_UNIX_SOCKET_PATH doc
    fRootURL := 'http://localhost'
  else
    FormatUtf8('http%://%:%', [HTTPS[fHttps], fServer, fPort], fRootURL);
end;

destructor TCurlHttp.Destroy;
begin
  if fHandle <> nil then
    curl.easy_cleanup(fHandle);
  inherited;
end;

function TCurlHttp.GetCACertFile: RawUtf8;
begin
  result := fSSL.CACertFile;
end;

procedure TCurlHttp.SetCACertFile(const aCertFile: RawUtf8);
begin
  fSSL.CACertFile := aCertFile;
end;

procedure TCurlHttp.UseClientCertificate(const aCertFile, aCACertFile, aKeyName,
  aPassPhrase: RawUtf8);
begin
  fSSL.CertFile := aCertFile;
  fSSL.CACertFile := aCACertFile;
  fSSL.KeyName := aKeyName;
  fSSL.PassPhrase := aPassPhrase;
end;

procedure TCurlHttp.InternalCreateRequest(const aMethod, aUrl: RawUtf8);
const
  CERT_PEM: RawUtf8 = 'PEM';
begin
  fIn.URL := fRootURL + aUrl;
  curl.easy_setopt(fHandle, coFollowLocation, 1); // url redirection (as TWinHttp)
  //curl.easy_setopt(fHandle,coTCPNoDelay,0); // disable Nagle
  if fLayer = nlUnix then
    curl.easy_setopt(fHandle, coUnixSocketPath, pointer(fServer));
  curl.easy_setopt(fHandle, coURL, pointer(fIn.URL));
  if fProxyName <> '' then
    curl.easy_setopt(fHandle, coProxy, pointer(fProxyName));
  if fHttps then
    if IgnoreSSLCertificateErrors then
    begin
      curl.easy_setopt(fHandle, coSSLVerifyPeer, 0);
      curl.easy_setopt(fHandle, coSSLVerifyHost, 0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyPeer,0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyHost,0);
    end
    else
    begin
      // see https://curl.haxx.se/libcurl/c/simplessl.html
      if fSSL.CertFile <> '' then
      begin
        curl.easy_setopt(fHandle, coSSLCertType, pointer(CERT_PEM));
        curl.easy_setopt(fHandle, coSSLCert, pointer(fSSL.CertFile));
        if fSSL.PassPhrase <> '' then
          curl.easy_setopt(fHandle, coSSLCertPasswd, pointer(fSSL.PassPhrase));
        curl.easy_setopt(fHandle, coSSLKeyType, nil);
        curl.easy_setopt(fHandle, coSSLKey, pointer(fSSL.KeyName));
        curl.easy_setopt(fHandle, coCAInfo, pointer(fSSL.CACertFile));
        curl.easy_setopt(fHandle, coSSLVerifyPeer, 1);
      end
      else if fSSL.CACertFile <> '' then
        curl.easy_setopt(fHandle, coCAInfo, pointer(fSSL.CACertFile));
    end;
  curl.easy_setopt(fHandle, coUserAgent, pointer(fExtendedOptions.UserAgent));
  curl.easy_setopt(fHandle, coWriteFunction, @CurlWriteRawByteString);
  curl.easy_setopt(fHandle, coHeaderFunction, @CurlWriteRawByteString);
  fIn.Method := UpperCase(aMethod);
  if fIn.Method = '' then
    fIn.Method := 'GET';
  if fIn.Method = 'GET' then
    fIn.Headers := nil
  else // disable Expect 100 continue in libcurl
    fIn.Headers := curl.slist_append(nil, 'Expect:');
  Finalize(fOut);
end;

procedure TCurlHttp.InternalAddHeader(const hdr: RawUtf8);
var
  P: PUtf8Char;
  s: RawUtf8;
begin
  P := pointer(hdr);
  while P <> nil do
  begin
    s := GetNextLine(P, P);
    if s <> '' then // nil would reset the whole list
      fIn.Headers := curl.slist_append(fIn.Headers, pointer(s));
  end;
end;

class function TCurlHttp.IsAvailable: boolean;
begin
  result := CurlIsAvailable;
end;

procedure TCurlHttp.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);
begin
  // see http://curl.haxx.se/libcurl/c/CURLOPT_CUSTOMREQUEST.html
  if HttpMethodWithNoBody(fIn.Method) then
    // the only verbs which do not expect body in answer are HEAD and OPTIONS
    curl.easy_setopt(fHandle, coNoBody, 1)
  else
    curl.easy_setopt(fHandle, coNoBody, 0);
  curl.easy_setopt(fHandle, coCustomRequest, pointer(fIn.Method));
  curl.easy_setopt(fHandle, coPostFields, pointer(aData));
  curl.easy_setopt(fHandle, coPostFieldSize, length(aData));
  curl.easy_setopt(fHandle, coHttpHeader, fIn.Headers);
  curl.easy_setopt(fHandle, coFile, @fOut.Data);
  curl.easy_setopt(fHandle, coWriteHeader, @fOut.Header);
end;

function TCurlHttp.InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding:
  RawUtf8; var Data: RawByteString): integer;
var
  res: TCurlResult;
  P: PUtf8Char;
  s: RawUtf8;
  i: integer;
  rc: PtrInt; // needed on Linux x86-64
begin
  res := curl.easy_perform(fHandle);
  if res <> crOK then
    raise ECurlHttp.CreateFmt('libcurl error %d (%s) on %s %s',
      [ord(res), curl.easy_strerror(res), fIn.Method, fIn.URL]);
  rc := 0;
  curl.easy_getinfo(fHandle, ciResponseCode, rc);
  result := rc;
  Header := TrimU(fOut.Header);
  if IdemPChar(pointer(Header), 'HTTP/') then
  begin
    i := 6;
    while Header[i] >= ' ' do
      inc(i);
    while ord(Header[i]) in [10, 13] do
      inc(i);
    system.Delete(Header, 1, i - 1); // trim leading 'HTTP/1.1 200 OK'#$D#$A
  end;
  P := pointer(Header);
  while P <> nil do
  begin
    s := GetNextLine(P, P);
    if IdemPChar(pointer(s), 'ACCEPT-ENCODING:') then
      trimcopy(s, 17, 100, AcceptEncoding)
    else if IdemPChar(pointer(s), 'CONTENT-ENCODING:') then
      trimcopy(s, 18, 100, Encoding);
  end;
  Data := fOut.Data;
end;

procedure TCurlHttp.InternalCloseRequest;
begin
  if fIn.Headers <> nil then
  begin
    curl.slist_free_all(fIn.Headers);
    fIn.Headers := nil;
  end;
  Finalize(fIn);
  fIn.DataOffset := 0;
  Finalize(fOut);
end;

{$endif USELIBCURL}


{ ******************** TSimpleHttpClient Wrapper Class }


{ TSimpleHttpClient }

constructor TSimpleHttpClient.Create(aOnlyUseClientSocket: boolean);
begin
  fOnlyUseClientSocket := aOnlyUseClientSocket;
  fTimeOut := 5000;
  inherited Create;
end;

destructor TSimpleHttpClient.Destroy;
begin
  FreeAndNil(fHttp);
  FreeAndNil(fHttps);
  inherited Destroy;
end;

function TSimpleHttpClient.RawRequest(const Uri: TUri;
  const Method, Header: RawUtf8; const Data: RawByteString;
  const DataType: RawUtf8; KeepAlive: cardinal): integer;
var
  temp: TUri;
begin
  result := 0;
  if (Uri.Https or
      (Proxy <> '')) and
     not fOnlyUseClientSocket then
  try
    if (fHttps = nil) or
       (fHttps.Server <> Uri.Server) or
       (fHttps.Port <> Uri.PortInt) then
    begin
      FreeAndNil(fHttp);
      FreeAndNil(fHttps); // need a new HTTPS connection
      fHttps := MainHttpClass.Create(
        Uri.Server, Uri.Port, Uri.Https, Proxy, '', fTimeOut, fTimeOut, fTimeOut);
      fHttps.IgnoreSSLCertificateErrors := fSocketTLS.IgnoreCertificateErrors;
      if fUserAgent <> '' then
        fHttps.UserAgent := fUserAgent;
    end;
    result := fHttps.Request(
      Uri.Address, Method, KeepAlive, Header, Data, DataType, fHeaders, fBody);
    if KeepAlive = 0 then
      FreeAndNil(fHttps);
    exit;
  except
    FreeAndNil(fHttps);
  end;
  // if we reached here, plain http or fOnlyUseClientSocket or fHttps failed
  try
    if (fHttp = nil) or
       (fHttp.Server <> Uri.Server) or
       (fHttp.Port <> Uri.Port) then
    begin
      FreeAndNil(fHttps);
      FreeAndNil(fHttp); // need a new HTTP connection
      fHttp := THttpClientSocket.Open(
        Uri.Server, Uri.Port, nlTcp, fTimeOut, Uri.Https, @fSocketTLS,
        GetSystemProxyUri(Uri.Address, Proxy, temp));
      if fUserAgent <> '' then
        fHttp.UserAgent := fUserAgent;
    end;
    if not fHttp.SockConnected then
      exit
    else
      result := fHttp.Request(
        Uri.Address, Method, KeepAlive, Header, Data, DataType, true);
    fBody := fHttp.Http.Content;
    fHeaders := fHttp.Http.Headers;
    if KeepAlive = 0 then
      FreeAndNil(fHttp);
  except
    FreeAndNil(fHttp);
  end;
end;

function TSimpleHttpClient.Request(const uri: RawUtf8; const method: RawUtf8;
  const header: RawUtf8; const data: RawByteString; const datatype: RawUtf8;
  keepalive: cardinal): integer;
var
  u: TUri;
begin
  if u.From(uri) then
    result := RawRequest(u, method, header, data, datatype, keepalive)
  else
    result := HTTP_NOTFOUND;
end;

function TSimpleHttpClient.SocketTLS: PNetTlsContext;
begin
  if self = nil then
    result := nil
  else
    result := @fSocketTLS;
end;


var
  _MainHttpClass: THttpRequestClass;

function MainHttpClass: THttpRequestClass;
begin
  if _MainHttpClass = nil then
  begin
    {$ifdef USEWININET}
    _MainHttpClass := TWinHttp;
    {$else}
    {$ifdef USELIBCURL}
    _MainHttpClass := TCurlHttp
    {$else}
    raise EHttpSocket.Create('MainHttpClass: No THttpRequest class known!');
    {$endif USELIBCURL}
    {$endif USEWININET}
  end;
  result := _MainHttpClass;
end;

procedure ReplaceMainHttpClass(aClass: THttpRequestClass);
begin
  _MainHttpClass := aClass;
end;



{ ************** Cached HTTP Connection to a Remote Server }

{ THttpRequestCached }

constructor THttpRequestCached.Create(const aUri: RawUtf8; aKeepAliveSeconds,
  aTimeOutSeconds: integer; const aToken: RawUtf8; aOnlyUseClientSocket: boolean);
begin
  inherited Create; // may have been overriden
  fKeepAlive := aKeepAliveSeconds * 1000;
  if aTimeOutSeconds > 0 then // 0 means no cache
    fCache := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
      TypeInfo(THttpRequestCacheDynArray), true, aTimeOutSeconds);
  fClient := fClient.Create(aOnlyUseClientSocket);
  if aUri <> '' then
    if not LoadFromUri(aUri, aToken) then
      raise ESynException.CreateUtf8('%.Create: invalid aUri=%', [self, aUri]);
end;

procedure THttpRequestCached.Clear;
begin
  FreeAndNil(fClient);
  if fCache <> nil then
    fCache.DeleteAll;
  fUri.Clear;
  fTokenHeader := '';
end;

destructor THttpRequestCached.Destroy;
begin
  fCache.Free;
  fClient.Free;
  inherited Destroy;
end;

function THttpRequestCached.Get(const aAddress: RawUtf8; aModified: PBoolean;
  aStatus: PInteger): RawByteString;
var
  cache: THttpRequestCache;
  headin: RawUtf8;
  status: integer;
  modified: boolean;
begin
  result := '';
  if fClient = nil then // either fHttp or fSocket is used
    exit;
  if (fCache <> nil) and
     fCache.FindAndCopy(aAddress, cache) then
    FormatUtf8('If-None-Match: %', [cache.Tag], headin);
  if fTokenHeader <> '' then
  begin
    if {%H-}headin <> '' then
      headin := headin + #13#10;
    headin := headin + fTokenHeader;
  end;
  status := fClient.RawRequest(fUri, 'GET', headin, '', '', fKeepAlive);
  modified := true;
  case status of
    HTTP_SUCCESS:
      if fCache <> nil then
      begin
        FindNameValue(fClient.Headers, 'ETAG:', cache.Tag);
        if cache.Tag <> '' then
        begin
          cache.Content := result;
          fCache.AddOrUpdate(aAddress, cache);
        end;
      end;
    HTTP_NOTMODIFIED:
      begin
        result := cache.Content;
        modified := false;
      end;
  end;
  if aModified <> nil then
    aModified^ := modified;
  if aStatus <> nil then
    aStatus^ := status;
end;

function THttpRequestCached.LoadFromUri(const aUri, aToken: RawUtf8): boolean;
begin
  result := false;
  if (self = nil) or
     (fClient = nil) or
     not fUri.From(aUri) then
    exit;
  fTokenHeader := AuthorizationBearer(aToken);
  result := true;
end;

function THttpRequestCached.Flush(const aAddress: RawUtf8): boolean;
begin
  if fCache <> nil then
    result := fCache.Delete(aAddress) >= 0
  else
    result := true;
end;



function HttpGet(const aUri: RawUtf8; outHeaders: PRawUtf8;
  forceNotSocket: boolean; outStatus: PInteger): RawByteString;
begin
  result := HttpGet(aUri, '', outHeaders, forceNotSocket, outStatus);
end;

function HttpGet(const aUri: RawUtf8; const inHeaders: RawUtf8;
  outHeaders: PRawUtf8; forceNotSocket: boolean;
  outStatus: PInteger): RawByteString;
var
  uri: TUri;
begin
  if uri.From(aUri) then
    if uri.Https or
       forceNotSocket then
      {$ifdef USEWININET}
      result := TWinHttp.Get(
        aUri, inHeaders, {weakCA=}true, outHeaders, outStatus)
      {$else}
      {$ifdef USELIBCURL2}
      if TCurlHttp.IsAvailable then
        result := TCurlHttp.Get(
          aUri, inHeaders, {weakCA=}true, outHeaders, outStatus)
      else
      {$endif USELIBCURL}
        // fallback to SChannel/OpenSSL if libcurl is not installed
        result := OpenHttpGet(uri.Server, uri.Port, uri.Address,
          inHeaders, outHeaders, uri.Layer, uri.Https, outStatus)
      {$endif USEWININET}
    else
      result := OpenHttpGet(uri.Server, uri.Port, uri.Address,
        inHeaders, outHeaders, uri.Layer, uri.Https, outStatus)
    else
      result := '';
  {$ifdef LINUX_RAWDEBUGVOIDHTTPGET}
  if result = '' then
    writeln('HttpGet returned VOID for ',uri.server,':',uri.Port,' ',uri.Address);
  {$endif LINUX_RAWDEBUGVOIDHTTPGET}
end;



{ ************** Send Email using the SMTP Protocol }

function TSMTPConnection.FromText(const aText: RawUtf8): boolean;
var
  u, h: RawUtf8;
begin
  if aText = SMTP_DEFAULT then
  begin
    result := false;
    exit;
  end;
  if Split(aText, '@', u, h) then
  begin
    if not Split(u, ':', User, Pass) then
      User := u;
  end
  else
    h := aText;
  if not Split(h, ':', Host, Port) then
  begin
    Host := h;
    Port := '25';
  end;
  if (Host <> '') and
     (Host[1] = '?') then
    Host := '';
  result := Host <> '';
end;

function SendEmail(const Server: TSMTPConnection; const From, CsvDest, Subject,
  Text, Headers, TextCharSet: RawUtf8; TLS: boolean): boolean;
begin
  result := SendEmail(
    Server.Host, From, CsvDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet,
    TLS or (Server.Port = '465') or (Server.Port = '587'));
end;

{$I-}

function SendEmail(const Server, From, CsvDest, Subject, Text, Headers, User,
  Pass, Port, TextCharSet: RawUtf8; TLS: boolean): boolean;
var
  TCP: TCrtSocket;

  procedure Expect(const Answer: RawUtf8);
  var
    Res: RawUtf8;
  begin
    repeat
      readln(TCP.SockIn^, Res);
      if ioresult <> 0 then
        raise ESendEmail.CreateUtf8('read error for %', [Res]);
    until (Length(Res) < 4) or
          (Res[4] <> '-');
    if not IdemPChar(pointer(Res), pointer(Answer)) then
      raise ESendEmail.CreateUtf8('%', [Res]);
  end;

  procedure Exec(const Command, Answer: RawUtf8);
  begin
    writeln(TCP.SockOut^, Command);
    if ioresult <> 0 then
      raise ESendEmail.CreateUtf8('write error for %s', [Command]);
    Expect(Answer)
  end;

var
  P: PUtf8Char;
  rec, ToList, head: RawUtf8;
begin
  result := false;
  P := pointer(CsvDest);
  if P = nil then
    exit;
  TCP := SocketOpen(Server, Port, TLS);
  if TCP <> nil then
  try
    TCP.CreateSockIn; // we use SockIn and SockOut here
    TCP.CreateSockOut;
    Expect('220');
    if (User <> '') and
       (Pass <> '') then
    begin
      Exec('EHLO ' + Server, '25');
      Exec('AUTH LOGIN', '334');
      Exec(BinToBase64(User), '334');
      Exec(BinToBase64(Pass), '235');
    end
    else
      Exec('HELO ' + Server, '25');
    writeln(TCP.SockOut^, 'MAIL FROM:<', From, '>');
    Expect('250');
    repeat
      GetNextItem(P, ',', rec);
      TrimSelf(rec);
      if rec = '' then
        continue;
      if PosExChar('<', rec) = 0 then
        rec := '<' + rec + '>';
      Exec('RCPT TO:' + rec, '25');
      if {%H-}ToList = '' then
        ToList := #13#10'To: ' + rec
      else
        ToList := ToList + ', ' + rec;
    until P = nil;
    Exec('DATA', '354');
    head := trimU(Headers);
    if head <> '' then
      head := head + #13#10;
    writeln(TCP.SockOut^,
      'Subject: ', Subject,
      #13#10'From: ', From, ToList,
      #13#10'Content-Type: text/plain; charset=', TextCharSet,
      #13#10'Content-Transfer-Encoding: 8bit'#13#10, head,
      #13#10, Text);
    Exec('.', '25');
    writeln(TCP.SockOut^, 'QUIT');
    result := ioresult = 0;
  finally
    TCP.Free;
  end;
end;

{$I+}

function SendEmailSubject(const Text: string): RawUtf8;
begin
  StringToUtf8(Text, result);
  result := MimeHeaderEncode(result);
end;


{ ************** DNS Resolution Cache for mormot.net.sock NewSocket() }

{ TNewSocketAddressCache }

type
  /// thread-safe TSynDictionary-based cache of DNS names for NewSocket()
  TNewSocketAddressCache = class(TInterfacedObject, INewSocketAddressCache)
  protected
    fData: TSynDictionary; // RawUtf8/TNetAddr pairs
  public
    constructor Create(aTimeOutSeconds: integer);
    destructor Destroy; override;
    // INewSocketAddressCache methods
    function Search(const Host: RawUtf8; out NetAddr: TNetAddr): boolean;
    procedure Add(const Host: RawUtf8; const NetAddr: TNetAddr);
    procedure Flush(const Host: RawUtf8);
    procedure SetTimeOut(aSeconds: integer);
  end;

constructor TNewSocketAddressCache.Create(aTimeOutSeconds: integer);
begin
  fData := TSynDictionary.Create(
    TypeInfo(TRawUtf8DynArray), TypeInfo(TNetAddrDynArray),
    {caseinsens=}true, aTimeOutSeconds);
end;

destructor TNewSocketAddressCache.Destroy;
begin
  fData.Free;
  inherited Destroy;
end;

function TNewSocketAddressCache.Search(const Host: RawUtf8;
  out NetAddr: TNetAddr): boolean;
begin
  result := fData.FindAndCopy(Host, NetAddr);
end;

procedure TNewSocketAddressCache.Add(const Host: RawUtf8;
  const NetAddr: TNetAddr);
begin
  fData.DeleteDeprecated;   // flush cache only when we may need some new space
  fData.Add(Host, NetAddr); // do nothing if already added in another thread
end;

procedure TNewSocketAddressCache.Flush(const Host: RawUtf8);
begin
  fData.Delete(Host);
end;

procedure TNewSocketAddressCache.SetTimeOut(aSeconds: integer);
begin
  fData.TimeOutSeconds := aSeconds; // warning: will clear the cache
end;



initialization
  NewSocketAddressCache := TNewSocketAddressCache.Create(600); // 10min timeout

finalization
  NewSocketAddressCache := nil;

end.

