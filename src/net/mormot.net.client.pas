/// HTTP/HTTPS Client Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.client;

{
  *****************************************************************************

   HTTP Client Classes
   - THttpMultiPartStream for multipart/formdata HTTP POST
   - THttpClientSocket Implementing HTTP client over plain sockets
   - Additional Client Protocols Support
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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json, // TSynDictionary for THttpRequestCached
  mormot.core.perf,
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
  mormot.lib.sspi, // do-nothing units on non compliant systems
  mormot.lib.gssapi,
  {$endif DOMAINRESTAUTH}
  mormot.crypt.secure;


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
    // internal TFileStreamEx to be retrieved by successive Read() calls
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
  THttpClientSocket = class;
  IWGetAlternate = interface;

  /// available THttpClientSocketWGet.Alternate file operations
  // - by default, cached file is temporary but could be kept forever on disk
  // if waoPermanentCache is defined
  // - waoNoHeadFirst will call OnDownload() first then fallback to GET so
  // may be preferred e.g. if the main server has a huge latency
  // - waoNoMinimalSize should let OnDownload() accept even the smallest files
  // - waoTryLastPeer/waoBroadcastNotAlone will force homonymous
  // pcoTryLastPeer/pcoBroadcastNotAlone THttpPeerCacheOption
  // - waoNoProgressiveDownloading will disable pcfResponsePartial requests
  TWGetAlternateOption = (
    waoPermanentCache,
    waoNoHeadFirst,
    waoNoMinimalSize,
    waoTryLastPeer,
    waoBroadcastNotAlone,
    waoNoProgressiveDownloading);

  /// define how THttpClientSocketWGet.Alternate should operate this file
  TWGetAlternateOptions = set of TWGetAlternateOption;

  /// the various steps of THttpClientSocket.WGet() process
  TWGetStep = (
    wgsUrlFile,
    wgsFileName,
    wgsHashed,
    wgsAlreadExisting,
    wgsFromCache,
    wgsResume,
    wgsHead,
    wgsAbortResume,
    wgsFromScratch,
    wgsNewStream,
    wgsDeleteFile,
    wgsAlternateFromCache,
    wgsAlternateLastPeer,
    wgsAlternateBroadcast,
    wgsAlternateGet,
    wgsAlternateSuccess,
    wgsAlternateFailed,
    wgsAlternateReset,
    wgsAlternateAlreadyInCache,
    wgsAlternateWrongSizeInCache,
    wgsAlternateCopiedInCache,
    wgsProgressive,
    wgsProgressiveFailed,
    wgsGet,
    wgsSetDate,
    wgsLastMod);
  /// which steps have been performed during THttpClientSocket.WGet() process
  TWGetSteps = set of TWGetStep;

  /// callback event for THttpClientSocket.WGet() process
  // - as set to THttpClientSocketWGet.OnStep
  // - an additional text information, depending on each step, is supplied
  TOnWGetStep = procedure(Step: TWGetStep; const Context: RawUtf8) of object;

  /// parameters set for THttpClientSocket.WGet() process
  // - some parameters are optional, and you should call Clear by default
  // - you could call redirectly the WGet method after having called Clear
  // and set the appropriated variables
  {$ifdef USERECORDWITHMETHODS}
  THttpClientSocketWGet = record
  {$else}
  THttpClientSocketWGet = object
  {$endif USERECORDWITHMETHODS}
    /// optional callback event called during download process
    // - typical usage is to assign e.g. TStreamRedirect.ProgressStreamToConsole
    // - note that by default, THttpClientSocket.OnLog will always be called
    OnProgress: TOnStreamProgress;
    /// optional callback if TFileStreamEx.Create(FileName, Mode) is not good enough
    OnStreamCreate: TOnStreamCreate;
    /// optional callback event raised during WGet() process
    // - alternative for business logic tracking: the OnProgress callback is
    // more about human interaction in GUI or console
    OnStep: TOnWGetStep;
    /// optional callback to allow an alternate download method
    // - can be used for a local peer-to-peer download cache via THttpPeerCache
    // - if defined, it will make a HEAD on the server to ensure the file still
    // exist (and that the client has the right to access it), then try to call
    // OnDownload() to get it from THttpPeerCache instances in the vicinity,
    // with a fallback to a plain GET if this file is not known by the peers
    // - OnDownloaded() will be called once a file has been received
    Alternate: IWGetAlternate;
    /// how Alternate should operate this file
    AlternateOptions: TWGetAlternateOptions;
    /// how much time this connection should be kept alive
    // - as redirected to the internal Request() parameter
    KeepAlive: cardinal;
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
    // mormot.crypt.secure - you may use e.g. HASH_STREAMREDIRECT[] constants
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
    LimitBandwidth: integer;
    /// will raise ESynException after TimeOutSec seconds are elapsed
    // - WGet(sockettimeout) is the TCP connect/receive/send raw timeout for
    // each packet, whereas this property is about the global time elapsed
    // during the whole download process
    TimeOutSec: integer;
    /// when WGet() has been called, contains all the steps involed during the
    // process
    OutSteps: TWGetSteps;
    /// initialize the default parameters - reset all fields to 0 / nil / ''
    procedure Clear;
    /// method used internally during process to notify Steps and OnStep()
    procedure SetStep(Step: TWGetStep; const Context: array of const);
    /// after Clear, instantiate and wrap THttpClientSocket.WGet
    // - would return the downloaded file name on success
    function WGet(const url: RawUtf8; const destfile: TFileName;
      const tunnel: RawUtf8 = ''; tls: PNetTlsContext = nil;
      sockettimeout: cardinal = 10000; redirectmax: integer = 0): TFileName;
  end;

  /// interface called by THttpClientSocket.WGet() for alternate download
  // - THttpPeerCache implements e.g. a local peer-to-peer download cache
  // - as set to THttpClientSocketWGet.Alternate optional parameter
  IWGetAlternate = interface
    /// try to download a resource from the alternative source
    // - e.g. THttpPeerCache will broadcast and retrieve the file from its peers
    // - this method is called after a HEAD on the server to retrieve the file
    // size and ensure the client is authorized to get the resource
    // - Params.Hasher/Hash are expected to be populated
    // - should behave the same (including RangeStart/RangeEnd support) as
    // ! Sender.Request(Url, 'GET', Params^.KeepAlive, Params^.Header, '', '',
    // !      {retry=}false, {instream=}nil, OutStream)
    // - OutStream.LimitPerSecond may be overriden during the call, e.g. if
    // operating on a local network
    // - should return HTTP_SUCCESS or HTTP_PARTIALCONTENT on success
    // - should return 0 if hash was not found, to fallback to a regular GET
    function OnDownload(Sender: THttpClientSocket;
      var Params: THttpClientSocketWGet; const Url: RawUtf8;
      ExpectedFullSize: Int64; OutStream: TStreamRedirect): integer;
    /// notify the alternate download implementation that there is a file
    // currently downloading into a .partial local file content
    // - e.g. THttpPeerCache will make this file available as pcfResponsePartial
    // - Params.Hasher/Hash are expected to be populated
    // - returns an integer OnDownloadingID > 0 to be supplied to OnDowloaded()
    // or OnDownloadingFailed()
    function OnDownloading(const Params: THttpClientSocketWGet;
      const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
    /// put a downloaded file into the alternative source cache
    // - e.g. THttpPeerCache will add this file to its cache, and resume any
    // pcfResponsePartial with the new file name
    // - this method is called after any file has been successfully downloaded
    // - Params.Hasher/Hash are expected to be populated
    procedure OnDowloaded(var Params: THttpClientSocketWGet;
      const Partial: TFileName; OnDownloadingID: THttpPartialID);
    /// notify the alternate download implementation that the data supplied
    // by OnDownload() was incorrect
    // - e.g. THttpPeerCache will delete this file from its cache
    // - mainly if the resulting hash does not match
    procedure OnDownloadFailed(const Params: THttpClientSocketWGet);
    /// notify the alternate download implementation that OnDownloading() failed
    // - e.g. THttpPeerCache will abort publishing this partial file
    procedure OnDownloadingFailed(OnDownloadingID: THttpPartialID);
  end;

  /// internal low-level execution context for THttpClientSocket.Request
  THttpClientRequest = record
    Url, Method, Header: RawUtf8;
    Data: RawByteString;
    DataMimeType: RawUtf8;
    Status, Redirected: integer;
    InStream, OutStream: TStream;
    KeepAliveSec: cardinal;
    Retry: set of (rMain, rAuth, rAuthProxy); // auth + retry state machine
    OutStreamInitialPos: Int64;
  end;

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
    var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean of object;

  /// callback used by THttpClientSocket.Request before/after every request
  // - return true to continue execution, false to abort normal process
  TOnHttpClientSocketRequest = function(Sender: THttpClientSocket;
    var Context: THttpClientRequest): boolean of object;

  /// callback used e.g. by THttpClientSocket.Request to process any custom protocol
  TOnHttpClientRequest = function(
    var http: THttpRequestContext): integer of object;

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
    fDigestAuthUserName: RawUtf8;
    fDigestAuthPassword: SpiUtf8;
    fDigestAuthAlgo: TDigestAlgo;
    fRedirectMax: integer;
    fOnAuthorize, fOnProxyAuthorize: TOnHttpClientSocketAuthorize;
    fOnBeforeRequest: TOnHttpClientSocketRequest;
    fOnProtocolRequest: TOnHttpClientRequest;
    fOnAfterRequest: TOnHttpClientSocketRequest;
    fOnRedirect: TOnHttpClientSocketRequest;
    {$ifdef DOMAINRESTAUTH}
    fAuthorizeSspiSpn: RawUtf8;
    {$endif DOMAINRESTAUTH}
    procedure RequestSendHeader(const url, method: RawUtf8); virtual;
    procedure RequestClear; virtual;
    function OnAuthorizeDigest(Sender: THttpClientSocket;
      var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    // - you can customize the default client timeouts by setting appropriate
    // aTimeout parameters (in ms) if you left the 0 default parameters,
    // it would use global HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(aTimeOut: PtrInt = 0); override;
    /// finalize this instance
    destructor Destroy; override;
    /// constructor to create a client connection to a given URI
    // - returns TUri.Address as parsed from aUri
    // - overriden to support custom RegisterNetClientProtocol()
    constructor OpenUri(const aUri: RawUtf8; out aAddress: RawUtf8;
      const aTunnel: RawUtf8 = ''; aTimeOut: cardinal = 10000;
      aTLSContext: PNetTlsContext = nil); override;
    /// low-level HTTP/1.1 request
    // - called by all Get/Head/Post/Put/Delete REST methods
    // - after an Open(server,port), return 200,202,204 if OK, or an http
    // status error otherwise
    // - retry is usually false, but could be recursively recalled as true
    // - use either Data or InStream for sending its body request (with its MIME
    // content type as DataMimeType e.g. JSON_CONTENT_TYPE)
    // - response body will be either in Content or in OutStream
    // - wrapper around RequestInternal() with OnBeforeRequest/OnAfterRequest
    // and RedirectMax handling
    function Request(const url, method: RawUtf8; KeepAlive: cardinal;
      const Header: RawUtf8; const Data: RawByteString = '';
      const DataMimeType: RawUtf8 = ''; retry: boolean = false;
      InStream: TStream = nil; OutStream: TStream = nil): integer; virtual;
    /// low-level processing method called from Request()
    // - can be used e.g. when implementing callbacks like OnAuthorize or
    // OnBeforeRequest/OnAfterRequest
    procedure RequestInternal(var ctxt: THttpClientRequest); virtual;
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
      const DataMimeType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer; overload;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    // - this overloaded method accepts a TStream for its output body content
    // - you could use a THttpMultiPartStream for multipart/formdata HTTP POST
    function Post(const url: RawUtf8; Data: TStream;
      const DataMimeType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer; overload;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Put(const url: RawUtf8; const Data: RawByteString;
      const DataMimeType: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    function Delete(const url: RawUtf8; KeepAlive: cardinal = 0;
      const header: RawUtf8 = ''): integer;
    /// setup web authentication using the Digest access algorithm
    procedure AuthorizeDigest(const UserName: RawUtf8; const Password: SpiUtf8;
      Algo: TDigestAlgo = daMD5_Sess);
    {$ifdef DOMAINRESTAUTH}
    /// web authentication of the current logged user using Windows Security
    // Support Provider Interface (SSPI) or GSSAPI library on Linux
    // - match the OnAuthorize: TOnHttpClientSocketAuthorize callback signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function AuthorizeSspi(Sender: THttpClientSocket;
      var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
    /// web authentication of the current logged user using Windows Security
    // Support Provider Interface (SSPI) or GSSAPI library on Linux
    // - match the OnProxyAuthorize: TOnHttpClientSocketAuthorize signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function ProxyAuthorizeSspi(Sender: THttpClientSocket;
      var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
    /// the Kerberos Service Principal Name, as registered in domain
    // - e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
    // - used by class procedure AuthorizeSspi/ProxyAuthorizeSspi callbacks
    // - on Linux/GSSAPI either this property or ClientForceSpn() is mandatory
    property AuthorizeSspiSpn: RawUtf8
      read fAuthorizeSspiSpn write fAuthorizeSspiSpn;
    {$endif DOMAINRESTAUTH}

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
    /// optional callback called before a Request() internal redirection
    // - ctxt.Status contains e.g. 301 (HTTP_MOVEDPERMANENTLY)
    // - ctxt.Url contains the redirected URI retrieved from 'Location:' header
    property OnRedirect: TOnHttpClientSocketRequest
      read fOnRedirect write fOnRedirect;
  published
    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
    /// contain the body data length retrieved from the server
    property ContentLength: Int64
      read Http.ContentLength;
    /// contain the body type retrieved from the server
    property ContentType: RawUtf8
      read Http.ContentType;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // mormot.net.websock unit)
  THttpClientSocketClass = class of THttpClientSocket;

/// returns the HTTP User-Agent header value of a mORMot client including
// the Instance class name in its minified/uppercase-only translation
// - typical value is "Mozilla/5.0 (Linux x64; mORMot) HCS/2 Tests/3"
// for THttpClientSocket from a Tests.exe application in version 3.x
// - framework is identified as '/2' with no release number, for security
// - note: the framework would identify the 'mORMot' pattern in the user-agent
// header to enable advanced behavior e.g. about JSON transmission
function DefaultUserAgent(Instance: TObject): RawUtf8;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ENetSock
function OpenHttp(const aServer, aPort: RawUtf8; aTLS: boolean = false;
  aLayer: TNetLayer = nlTcp; const aUrlForProxy: RawUtf8 = '';
  aTimeout: integer = 0; aTLSContext: PNetTlsContext = nil): THttpClientSocket; overload;

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
  aTLS: boolean = false; outStatus: PInteger = nil;
  aTimeout: integer = 0; ignoreTlsCertError: boolean = false): RawByteString; overload;

/// download some potentially huge file, with proper resume
// - is a wrapper around THttpClientSocket.WGet() method
// - returns '' on success, or an error message otherwise
function WGet(const url: RawUtf8; const destfile: TFileName;
  const tunnel: RawUtf8 = ''; hasher: TStreamRedirectClass = nil;
  const hash: RawUtf8 = ''; tls: PNetTlsContext = nil;
  sockettimeout: cardinal = 10000; redirectmax: integer = 0;
  consoledisplay: boolean = false): string;

function ToText(wgs: TWGetStep): PShortString; overload;
function ToText(wgs: TWGetSteps; trimmed: boolean = true): RawUtf8; overload;

var
  /// global overriden value for the GetSystemProxyUri() function
  // - as used by OpenHttp/OpenHttpGet and TSimpleHttpClient
  // - can be set manually to a forced global value
  DefaultHttpClientSocketProxy: TUri;

  /// force GetProxyForUri(fromSystem=true) in GetSystemProxyUri() function
  DefaultHttpClientSocketProxyAuto: boolean;


/// ask the Operating System to return the Tunnel/Proxy settings for a given URI
// - as used internally by OpenHttp/OpenHttpGet and TSimpleHttpClient to call
// THttpClientSocket.Open() constructor
// - if proxy is set, will return its value from @temp, otherwise return
// @DefaultHttpClientSocketProxy or call GetProxyForUri() to fill and return @temp
// - return nil if no proxy is to be used for this URI
function GetSystemProxyUri(const uri, proxy: RawUtf8; var temp: TUri): PUri;

/// ask the Operating System to return the Tunnel/Proxy settings for a given URI
// - return DefaultHttpClientSocketProxy.URI or call GetProxyForUri()
// - return '' if no proxy is to be used for this URI
function GetSystemProxy(const uri: RawUtf8): RawUtf8;

/// ask the Operating System to return the Tunnel/Proxy setting for a given URI
// - will always use or HTTP_PROXY/HTTPS_PROXY environment variables
// - if no environment variable is set, on Windows fromSystem=true will call
// WinHttpGetProxyInfo from mormot.lib.winhttp to use the Internet Explorer
// settings or system PAC file
// - return '' if no proxy is defined
function GetProxyForUri(const uri: RawUtf8;
  fromSystem: boolean = true): RawUtf8;

/// parse a URI into its final resource name
// - optionally sanitize the output to be filename-compatible
// - note that it returns an UTF-8 string as resource URI, not TFileName
function ExtractResourceName(const uri: RawUtf8; sanitize: boolean = true): RawUtf8;


{ ******************** Additional Client Protocols Support }

var
  /// raw thread-safe access to <Name:RawUtf8,TOnHttpClientRequest> pairs
  NetClientProtocols: TSynDictionary;

/// register a INetClientProtocol
// - you can unregister a protocol by setting OnRequest = nil
// - note that the class instance used by OnRequest will be owned by thit unit
procedure RegisterNetClientProtocol(
  const Name: RawUtf8; const OnRequest: TOnHttpClientRequest);


{ ******************** THttpRequest Abstract HTTP client class }

type
  {$M+} // to have existing RTTI for published properties
  THttpRequest = class;
  {$M-}

  /// the supported authentication schemes which may be used by HTTP clients
  // - supported only by TWinHttp class yet, and TCurlHttp
  // - wraBearer is only supported by TCurlHttp
  THttpRequestAuthentication = (
    wraNone,
    wraBasic,
    wraDigest,
    wraNegotiate,
    wraBearer);

  /// a record to set some extended options for HTTP clients
  // - allow easy propagation e.g. from a TRestHttpClient* wrapper class to
  // the actual mormot.net.http's THttpRequest implementation class
  THttpRequestExtendedOptions = record
    /// customize HTTPS process
    TLS: TNetTlsContext;
    /// allow HTTP authentication to take place at connection
    // - Auth.Scheme and UserName/Password properties are handled
    // by the TWinHttp class only by now
    // - Auth.Token is only handled by TCurlHttp
    Auth: record
      UserName: SynUnicode;
      Password: SynUnicode;
      Token: SpiUtf8;
      Scheme: THttpRequestAuthentication;
    end;
    /// allow to customize the User-Agent header
    // - for TWinHttp, should be set at constructor level
    UserAgent: RawUtf8;
  end;

  /// event callback to track up/download process
  TOnHttpRequest = procedure(Sender: THttpRequest; Done: Boolean) of object;
  /// event callback to track up/download progress
  TOnHttpRequestProgress = procedure(Sender: THttpRequest;
    Current, Total: Int64) of object;

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
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawUtf8;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressAcceptHeader: THttpSocketCompressSet;
    fExtendedOptions: THttpRequestExtendedOptions;
    fTag: PtrInt;
    fOnUpload: TOnHttpRequest;
    fOnUploadProgress: TOnHttpRequestProgress;
    fOnDownload: TOnHttpRequest;
    fOnDownloadProgress: TOnHttpRequestProgress;
    class function InternalREST(const url, method: RawUtf8;
      const data: RawByteString; const header: RawUtf8;
      aIgnoreTlsCertificateErrors: boolean; timeout: integer;
      outHeaders: PRawUtf8; outStatus: PInteger): RawByteString;
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
    // - aUserAgent is needed for TWinHttp, which requires this info at connection 
    constructor Create(const aServer, aPort: RawUtf8; aHttps: boolean;
      const aProxyName: RawUtf8 = ''; const aProxyByPass: RawUtf8 = '';
      ConnectionTimeOut: cardinal = 0; SendTimeout: cardinal = 0;
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTcp;
      const aUserAgent: RawUtf8 = ''); overload; virtual;
    /// connect to the supplied URI
    // - is just a wrapper around TUri and the overloaded Create() constructor
    constructor Create(const aUri: RawUtf8; const aProxyName: RawUtf8 = '';
      const aProxyByPass: RawUtf8 = ''; ConnectionTimeOut: cardinal = 0;
      SendTimeout: cardinal = 0; ReceiveTimeout: cardinal = 0;
      aIgnoreTlsCertificateErrors: boolean = false); overload;
    /// finalize this instance
    destructor Destroy; override;

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
    // - aIgnoreTlsCertificateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Get() but either TWinHttp.Get(), TWinINet.Get() or
    // TCurlHttp.Get() methods
    class function Get(const aUri: RawUtf8; const aHeader: RawUtf8 = '';
      aIgnoreTlsCertificateErrors: boolean = false; outHeaders: PRawUtf8 = nil;
      outStatus: PInteger = nil; timeout: integer = 0): RawByteString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreTlsCertificateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Post() but either TWinHttp.Post(), TWinINet.Post() or
    // TCurlHttp.Post() methods
    class function Post(const aUri: RawUtf8; const aData: RawByteString;
      const aHeader: RawUtf8 = ''; aIgnoreTlsCertificateErrors: boolean = false;
      outHeaders: PRawUtf8 = nil; outStatus: PInteger = nil;
      timeout: integer = 0): RawByteString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreTlsCertificateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Put() but either TWinHttp.Put(), TWinINet.Put() or
    // TCurlHttp.Put() methods
    class function Put(const aUri: RawUtf8; const aData: RawByteString;
      const aHeader: RawUtf8 = ''; aIgnoreTlsCertificateErrors: boolean = false;
      outHeaders: PRawUtf8 = nil; outStatus: PInteger = nil;
      timeout: integer = 0): RawByteString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreTlsCertificateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Delete() but either TWinHttp.Delete(), TWinINet.Delete() or
    // TCurlHttp.Delete() methods
    class function Delete(const aUri: RawUtf8; const aHeader: RawUtf8 = '';
      aIgnoreTlsCertificateErrors: boolean = false; outHeaders: PRawUtf8 = nil;
      outStatus: PInteger = nil; timeout: integer = 0): RawByteString;

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) below which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    // within each priority level (the lower aPriority first)
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024; aPriority: integer = 10): boolean;

    /// allows to ignore untrusted TLS certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreTlsCertificateErrors: boolean
      read fExtendedOptions.TLS.IgnoreCertificateErrors
      write fExtendedOptions.TLS.IgnoreCertificateErrors;
    {$ifndef PUREMORMOT2}
    property IgnoreSslCertificateErrors: boolean
      read fExtendedOptions.TLS.IgnoreCertificateErrors
      write fExtendedOptions.TLS.IgnoreCertificateErrors;
    {$endif PUREMORMOT2}
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
    /// optional Token for TCurlHttp Authentication
    property AuthToken: SpiUtf8
      read fExtendedOptions.Auth.Token
      write fExtendedOptions.Auth.Token;
    /// custom HTTP "User Agent:" header value
    property UserAgent: RawUtf8
      read fExtendedOptions.UserAgent
      write fExtendedOptions.UserAgent;
    /// internal structure used to store extended options
    // - will be replicated by TLS.IgnoreCertificateErrors and Auth* properties
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
    /// called before and after Upload process
    property OnUpload: TOnHttpRequest
      read fOnUpload write fOnUpload;
    /// called during Upload progression
    // - only implemented by TCurlHttp yet
    property OnUploadProgress: TOnHttpRequestProgress
      read fOnUploadProgress write fOnUploadProgress;
    /// called before and after Download process
    property OnDownload: TOnHttpRequest
      read fOnDownload write fOnDownload;
    /// called during Download progression
    // - only implemented by TCurlHttp yet
    property OnDownloadProgress: TOnHttpRequestProgress
      read fOnDownloadProgress write fOnDownloadProgress;
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
  EWinHttp = class(ESynException)
  public
    /// create and raise a EWinHttp exception, with the error message as text
    class procedure RaiseFromLastError;
  end;

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
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTcp;
      const aUserAgent: RawUtf8 = ''); override;
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

var
  /// global flag to enable HTTP proxy detection at OS level for TWinHttp
  // - as if Windows 8.1 and newer is detected
  // - may be useful if {$R Vista.res} manifest is not available in the project,
  // so only Windows 7 is detected by the executable
  WinHttpForceProxyDetection: boolean;

{$endif USEWININET}

{$ifdef USELIBCURL}

type
  /// TCurlHttp exception type
  ECurlHttp = class(ExceptionWithProps)
  protected
    fError: TCurlResult;
  public
    constructor Create(error: TCurlResult; const Msg: string;
      const Args: array of const); overload;
  published
    property Error: TCurlResult
      read fError;
  end;

  /// a class to handle HTTP/1.1 request using the libcurl library
  // - libcurl is a free and easy-to-use cross-platform URL transfer library,
  // able to directly connect via HTTP or HTTPS on most Linux systems
  // - under a 32 bit Linux system, the libcurl library (and its dependencies,
  // like OpenSSL) may not be installed - you can add it via your package
  // manager, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3
  // - under a 64-bit Linux system, if compiled into linux-i386 target, you
  // should install the 32-bit flavor of libcurl, e.g. on Ubuntu:
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
    fTls: record
      CertFile, CACertFile, KeyName, PassPhrase: RawUtf8;
    end;
    fLast: record
      dlTotal, dlNow, ulTotal, ulNow: Int64;
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
    /// set the client TLS certification details
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
    fUri, fProxy, fHeaders, fUserAgent: RawUtf8;
    fBody: RawByteString;
    fSocketTLS: TNetTlsContext;
    fOnlyUseClientSocket: boolean;
    fTimeOut, fStatus: integer;
  public
    /// initialize the instance
    // - aOnlyUseClientSocket=true will use THttpClientSocket even for HTTPS
    constructor Create(aOnlyUseClientSocket: boolean = false); reintroduce;
    /// finalize the connection
    destructor Destroy; override;
    /// low-level entry point of this instance, using an TUri as input
    // - rather use the Request() more usable method
    function RawRequest(const Uri: TUri; const Method, Header: RawUtf8;
      const Data: RawByteString; const DataMimeType: RawUtf8;
      KeepAlive: cardinal): integer; overload;
    /// simple-to-use entry point of this instance
    // - use Body and Headers properties to retrieve the HTTP body and headers
    function Request(const Uri: RawUtf8; const Method: RawUtf8 = 'GET';
      const Header: RawUtf8 = ''; const Data: RawByteString = '';
      const DataMimeType: RawUtf8 = ''; keepalive: cardinal = 10000): integer; overload;
    /// access to the raw TLS settings for THttpClientSocket
    function SocketTLS: PNetTlsContext;
      {$ifdef HASINLINE} inline; {$endif}
    /// returns the HTTP body as returned by a previous call to Request()
    property Body: RawByteString
      read fBody;
    /// returns the HTTP status code after a Request() call
    property Status: integer
      read fStatus;
    /// returns the HTTP headers as returned by a previous call to Request()
    property Headers: RawUtf8
      read fHeaders;
    /// allows to customize the user-agent header
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
    /// allows to customize HTTPS connection and allow weak certificates
    property IgnoreTlsCertificateErrors: boolean
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
  forceNotSocket: boolean = false; outStatus: PInteger = nil;
  timeout: integer = 0; forceSocket: boolean = false;
  ignoreTlsCertError: boolean = false): RawByteString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHttp/TCurlHttp for any https URI
function HttpGet(const aUri: RawUtf8; const inHeaders: RawUtf8;
  outHeaders: PRawUtf8 = nil; forceNotSocket: boolean = false;
  outStatus: PInteger = nil; timeout: integer = 0; forceSocket: boolean = false;
  ignoreTlsCertError: boolean = false): RawByteString; overload;



{ ************** Send Email using the SMTP Protocol }

const
  /// the layout of TSmtpConnection.FromText method
  SMTP_DEFAULT = 'user:password@smtpserver:port';

type
  /// may be used to store a connection to a SMTP server
  // - see SendEmail() overloaded function
  {$ifdef USERECORDWITHMETHODS}
  TSmtpConnection = record
  {$else}
  TSmtpConnection = object
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
// - by default, the text body is expected to be in ISO-8859-1, i.e. Utf8ToWinAnsi
// - you can optionally set another encoding charset or force TextCharSet='' to
// expect the 'Content-Type:' to be set in Headers and Text to be the raw body
// (e.g. a multi-part encoded message)
function SendEmail(const Server, From, CsvDest, Subject: RawUtf8;
  const Text: RawByteString; const Headers: RawUtf8 = ''; const User: RawUtf8 = '';
  const Pass: RawUtf8 = ''; const Port: RawUtf8 = '25';
  const TextCharSet: RawUtf8  =  'ISO-8859-1'; TLS: boolean = false): boolean; overload;

/// send an email using the SMTP protocol via a TSmtpConnection definition
// - retry true on success
// - the Subject is expected to be in plain 7-bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - by default, the text body is expected to be in ISO-8859-1, i.e. Utf8ToWinAnsi
// - you can optionally set another encoding charset or force TextCharSet='' to
// expect the 'Content-Type:' to be set in Headers and Text to be the raw body
// (e.g. a multi-part encoded message)
// - TLS will be forced if the port is either 465 or 587
function SendEmail(const Server: TSmtpConnection;
  const From, CsvDest, Subject: RawUtf8; const Text: RawByteString;
  const Headers: RawUtf8 = ''; const TextCharSet: RawUtf8  = 'ISO-8859-1';
  TLS: boolean = false): boolean; overload;

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
        result^.ContentType := JSON_CONTENT_TYPE_VAR
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
      s := s + #13#10; // a TFileStreamEx content will be appended
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
  fs: TStream;
  fn: RawUtf8;
begin
  fs := TFileStreamEx.Create(filename, fmOpenReadShared);
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


{ ******************** Additional Client Protocols Support }

procedure RegisterNetClientProtocol(
  const Name: RawUtf8; const OnRequest: TOnHttpClientRequest);
var
  m: TMethod;
begin
  if NetClientProtocols.FindAndExtract(Name, m) then
    TObject(m.Data).Free; // was owned by this unit
  if Assigned(OnRequest) then
    NetClientProtocols.Add(Name, OnRequest);
end;


{ TNetClientProtocolFile }

type
  TNetClientProtocolFile = class
  public
    function OnRequest(var http: THttpRequestContext): integer;
  end;

function TNetClientProtocolFile.OnRequest(var http: THttpRequestContext): integer;
var
  fn: TFileName;
begin
  // try PathCreateFromUrl() API on Windows, or parse using TUri on POSIX
  fn := GetFileNameFromUrl(Utf8ToString(http.CommandResp));
  result := http.ContentFromFile(fn, -1); // into http.Content/ContentStream
  http.Headers := ''; // no custom headers
end;


{ ************** THttpClientSocket Implementing HTTP client over plain sockets }

function DefaultUserAgent(Instance: TObject): RawUtf8;
var
  i: PtrInt;
  P: PShortString;
  name, vers: TShort16;
begin
  // instance class name reduced to uppercase e.g. THttpClientSocket into 'HCS'
  name[0] := #0;
  P := ClassNameShort(Instance);
  for i := 2 to ord(P^[0]) do
    if P^[i] in ['A'..'Z'] then // append uppercase alphabetic chars
      begin
        inc(name[0]);
        name[ord(name[0])] := P^[i];
        if name[0] = #16 then
          break;
      end;
  // note: the framework would identify 'mORMot' pattern in the user-agent
  // header to enable advanced behavior e.g. about JSON transmission
  vers[0] := #0;
  if Executable.Version.Major <> 0 then
    FormatShort16('/%', [Executable.Version.Major], vers);
  FormatUtf8('Mozilla/5.0 (' + OS_TEXT + ' ' + CPU_ARCH_TEXT + '; mORMot) %/2 %%',
    [name, Executable.ProgramName, vers], result);
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
    if tls <> nil then
      tls^ := s.TLS; // copy peer info to the TLS context (may be redirected)
  finally
    s.Free;
  end;
end;

procedure THttpClientSocketWGet.SetStep(
  Step: TWGetStep; const Context: array of const);
begin
  include(OutSteps, Step);
  if Assigned(OnStep) then
    OnStep(Step, Make(Context));
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
      EHttpSocket.RaiseUtf8('WGet(destfile='''') for %', [url]);
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

function ToText(wgs: TWGetStep): PShortString;
begin
  result := GetEnumName(TypeInfo(TWGetStep), ord(wgs));
end;

function ToText(wgs: TWGetSteps; trimmed: boolean): RawUtf8;
begin
  result := GetSetName(TypeInfo(TWGetSteps), wgs, trimmed);
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

function GetSystemProxy(const uri: RawUtf8): RawUtf8;
begin
  if DefaultHttpClientSocketProxy.Server <> '' then
    result := DefaultHttpClientSocketProxy.URI
  else
    result := GetProxyForUri(uri, DefaultHttpClientSocketProxyAuto);
end;

function ExtractResourceName(const uri: RawUtf8; sanitize: boolean): RawUtf8;
var
  u: TUri;
begin
  result := '';
  if (uri = '') or
     not u.From(uri) then
    exit;
  result := UrlDecode(u.ResourceName);
  if sanitize and
     not SafeFileNameU(result) then
    result := OnlyChar(result, ['0'..'9', 'A'..'Z', 'a'..'z', '_', '.']);
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

destructor THttpClientSocket.Destroy;
begin
  FillZero(fDigestAuthPassword);
  inherited Destroy;
end;

constructor THttpClientSocket.OpenUri(const aUri: RawUtf8;
  out aAddress: RawUtf8; const aTunnel: RawUtf8; aTimeOut: cardinal;
  aTLSContext: PNetTlsContext);
var
  u: TUri;
begin
  if (u.From(aUri) or // e.g. 'file:///path/to' returns false but is valid
      (u.Address <> '')) and
     not IdemPChar(pointer(u.Scheme), 'HTTP') and
     NetClientProtocols.FindAndCopy(u.Scheme, fOnProtocolRequest) then
    begin
      Create(aTimeOut); // no socket involved
      fOpenUriFull := aUri; // e.g. to call PatchCreateFromUrl() Windows API
      aAddress := u.Address;
    end
  else
    inherited OpenUri(aUri, aAddress, aTunnel, aTimeOut, aTLSContext);
end;

procedure THttpClientSocket.RequestInternal(var ctxt: THttpClientRequest);

  procedure DoRetry(FatalError: integer;
    const Fmt: RawUtf8; const Args: array of const);
  var
    msg: RawUtf8;
  begin
    FormatUtf8(Fmt, Args, msg);
    //writeln('DoRetry ',byte(ctxt.Retry), ' ', FatalError, ' / ', msg);
    if Assigned(OnLog) then
       OnLog(sllTrace, 'DoRetry % socket=% fatal=% retry=%',
         [msg, fSock.Socket, FatalError, BOOL_STR[rMain in ctxt.Retry]], self);
    if fAborted then
      ctxt.Status := HTTP_NOTFOUND
    else if rMain in ctxt.Retry then
      // we should retry once -> return error only if failed twice
      ctxt.Status := FatalError
    else
      try
        // recreate the connection and try again
        Close;
        OpenBind(fServer, fPort, {bind=}false, TLS.Enabled);
        HttpStateReset;
        include(ctxt.Retry, rMain);
        RequestInternal(ctxt);
      except
        on Exception do
          ctxt.Status := FatalError;
      end;
  end;

var
  cmd: PUtf8Char;
  pending: TCrtSocketPending;
  bodystream: TStream;
  loerr: integer;
  dat: RawByteString;
  start: Int64;
begin
  if Assigned(OnLog) then
  begin
    QueryPerformanceMicroSeconds(start);
    OnLog(sllTrace, 'RequestInternal % %:%/% flags=% retry=%', [ctxt.Method,
      fServer, fPort, ctxt.Url, ToText(Http.HeaderFlags), byte(ctxt.Retry)], self);
  end;
  if SockIn = nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  Http.Content := '';
  if fAborted then
    ctxt.Status := HTTP_NOTFOUND
  else if (hfConnectionClose in Http.HeaderFlags) or
          not SockIsDefined then
    DoRetry(HTTP_NOTFOUND, 'connection closed (keepalive timeout or max)', [])
  else if not fSock.Available(@loerr) then
    DoRetry(HTTP_NOTFOUND, 'connection broken (socketerror=%)', [loerr])
  else
  try
    // send request - we use SockSend because writeln() is calling flush()
    try
      // prepare headers
      RequestSendHeader(ctxt.Url, ctxt.Method);
      if ctxt.KeepAliveSec <> 0 then
        SockSend(['Connection: Keep-Alive'#13#10 +
                  'Keep-Alive: timeout=', ctxt.KeepAliveSec]) // as seconds
      else
        SockSend('Connection: Close');
      dat := ctxt.Data; // local var copy for Data to be compressed in-place
      if (dat <> '') or
         (not IsGet(ctxt.Method) and // no message body len/type for GET/HEAD
          not IsHead(ctxt.Method)) then
        CompressDataAndWriteHeaders(ctxt.DataMimeType, dat, ctxt.InStream);
      if ctxt.Header <> '' then
        SockSend(ctxt.Header);
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
      // wait and retrieve HTTP command line response
      pending := SockReceivePending(Timeout, @loerr); // select/poll
      case pending of
        cspDataAvailable:
          ; // ok
        cspDataAvailableOnClosedSocket:
          include(Http.HeaderFlags, hfConnectionClose); // socket is closed
        cspNoData:
          begin
            ctxt.Status := HTTP_TIMEOUT; // no retry on real timeout
            exit;
          end;
      else // cspSocketError, cspSocketClosed
        begin
          DoRetry(HTTP_NOTFOUND, '% % waiting %ms for headers',
            [ToText(pending)^, CardinalToHexShort(loerr), TimeOut]);
          exit;
        end;
      end;
      SockRecvLn(Http.CommandResp); // will raise ENetSock on any error
      cmd := pointer(Http.CommandResp);
      if IdemPChar(cmd, 'HTTP/1.') and
         (cmd[7] in ['0', '1']) then
      begin
        // get http numeric status code (200,404...) from 'HTTP/1.x ######'
        ctxt.Status := GetCardinal(cmd + 9);
        if (ctxt.Status < 200) or
           (ctxt.Status > 599) then
        begin
          ctxt.Status := HTTP_HTTPVERSIONNONSUPPORTED;
          exit;
        end;
      end
      else
      begin
        // error on reading answer -> 505=wrong format
        if Http.CommandResp = '' then
          DoRetry(HTTP_NOTFOUND, 'Broken Link - timeout=%ms', [TimeOut])
        else
          DoRetry(HTTP_HTTPVERSIONNONSUPPORTED, 'Command=%', [Http.CommandResp]);
        exit;
      end;
      // retrieve all HTTP headers
      GetHeader({unfiltered=}false);
      if (cmd[7] = '0') and  // plain HTTP/1.0 should force connection close
         not (hfConnectionKeepAlive in Http.HeaderFlags) then
        include(Http.HeaderFlags, hfConnectionClose);
      // retrieve Body content (if any)
      if (ctxt.Status >= HTTP_SUCCESS) and
         (ctxt.Status <> HTTP_NOCONTENT) and
         (ctxt.Status <> HTTP_NOTMODIFIED) and
         not HttpMethodWithNoBody(ctxt.Method) then
         // HEAD or status 100..109,204,304 -> no body (RFC 2616 section 4.3)
      begin
        // specific TStreamRedirect expectations
        bodystream := ctxt.OutStream;
        if (bodystream <> nil) and
           bodystream.InheritsFrom(TStreamRedirect) then
          if ctxt.Status in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then
          begin
            if Http.ContentLength > 0 then
              TStreamRedirect(bodystream).ExpectedSize :=
                fRangeStart + Http.ContentLength // we know the size
          end
          else
            bodystream := nil; // don't append any HTML server error message
        // retrieve whole response body
        GetBody(bodystream);
      end;
      // successfully sent -> reset some fields for the next request
      if ctxt.Status in HTTP_GET_OK then
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
       OnLog(sllTrace, 'RequestInternal status=% flags=% in %',
         [ctxt.Status, ToText(Http.HeaderFlags), MicroSecFrom(start)], self);
    if hfConnectionClose in Http.HeaderFlags then
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
  KeepAlive: cardinal; const Header: RawUtf8; const Data: RawByteString;
  const DataMimeType: RawUtf8; retry: boolean; InStream, OutStream: TStream): integer;
var
  ctxt: THttpClientRequest;
  newuri: TUri;
begin
  ctxt.Url := url;
  if (url = '') or
     (url[1] <> '/') then
    insert('/', ctxt.Url, 1); // normalize URI as in RFC (e.g. for Digest auth)
  ctxt.Method := method;
  if KeepAlive = 0 then
    ctxt.KeepAliveSec := 0
  else if KeepAlive <= 1000 then
    ctxt.KeepAliveSec := 1 // "Keep-Alive: timeout=xx" header unit is in seconds
  else
    ctxt.KeepAliveSec := KeepAlive div 1000;
  ctxt.Header := TrimU(header);
  ctxt.Data := Data;
  ctxt.DataMimeType := DataMimeType;
  ctxt.InStream := InStream;
  ctxt.OutStream := OutStream;
  if OutStream <> nil then
    ctxt.OutStreamInitialPos := OutStream.Position;
  ctxt.Status := 0;
  ctxt.Redirected := 0;
  if retry then
    ctxt.Retry := [rMain]
  else
    ctxt.Retry := [];
  if (not Assigned(fOnBeforeRequest)) or
     fOnBeforeRequest(self, ctxt) then
  begin
    fRedirected := '';
    if Assigned(fOnProtocolRequest) then
    begin
      // emulate a custom protocol (e.g. 'file://') into a HTTP request
      if Http.ParseAll(ctxt.InStream, ctxt.Data,
          FormatUtf8('% % HTTP/1.0', [method, ctxt.Url]), ctxt.Header) then
      begin
        Http.CommandResp := fOpenUriFull;
        ctxt.Status := fOnProtocolRequest(Http);
        if StatusCodeIsSuccess(ctxt.Status) then
          ctxt.Status := Http.ContentToOutput(ctxt.Status, ctxt.OutStream);
        if assigned(OnLog) then
          OnLog(sllTrace, 'Request(%)=% via %.OnRequest',
            [fOpenUriFull, ctxt.Status,
             TObject(TMethod(fOnProtocolRequest).Data)], self);
      end;
    end
    else
    repeat
      // sub-method to handle the actual request, with proper retrial
      RequestInternal(ctxt);
      if fAborted then
        break;
      // handle optional (proxy) authentication callbacks
      if (ctxt.Status = HTTP_UNAUTHORIZED) and
          Assigned(fOnAuthorize) then
      begin
        if assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [ctxt.Method, url, ctxt.Status], self);
        if rAuth in ctxt.Retry then
          break; // avoid infinite recursion
        include(ctxt.Retry, rAuth);
        if fOnAuthorize(self, ctxt, Http.HeaderGetValue('WWW-AUTHENTICATE')) then
          continue;
      end
      else if (ctxt.Status = HTTP_PROXYAUTHREQUIRED) and
          Assigned(fOnProxyAuthorize) then
      begin
        if assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [ctxt.Method, url, ctxt.Status], self);
        if rAuthProxy in ctxt.Retry then
          break;
        include(ctxt.Retry, rAuthProxy);
        if fOnProxyAuthorize(self, ctxt, Http.HeaderGetValue('PROXY-AUTHENTICATE')) then
          continue;
      end;
      // handle redirection from returned headers
      if (ctxt.Status < 300) or              // 300..399 are redirections
         (ctxt.Status = HTTP_NOTMODIFIED) or // but 304 is not
         (ctxt.Status > 399) or              // 400.. are errors
         (ctxt.Redirected >= fRedirectMax) then
        break;
      if retry then
        ctxt.Retry := [rMain]
      else
        ctxt.Retry := [];
      ctxt.Url := Http.HeaderGetValue('LOCATION');
      case ctxt.Status of
        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections
        HTTP_MOVEDPERMANENTLY,
        HTTP_SEEOTHER:
          ctxt.Method := 'GET';
        // HTTP_TEMPORARYREDIRECT HTTP_PERMANENTREDIRECT should keep the method
      end;
      if (OutStream <> nil) and
         // TStreamRedirect would have set bodystream := nil in RequestInternal
         not OutStream.InheritsFrom(TStreamRedirect) and
         (OutStream.Position <> ctxt.OutStreamInitialPos) then
      begin
        OutStream.Size := ctxt.OutStreamInitialPos;     // truncate
        OutStream.Position := ctxt.OutStreamInitialPos; // reset position
      end;
      if assigned(OnLog) then
        OnLog(sllTrace, 'Request % % redirected to %', [ctxt.Method, url, ctxt.Url], self);
      if Assigned(fOnRedirect) then
        if not fOnRedirect(self, ctxt) then
          break;
      if IdemPChar(pointer(ctxt.Url), 'HTTP') and
         newuri.From(ctxt.Url) then
      begin
        fRedirected := newuri.Address;
        if (hfConnectionClose in Http.HeaderFlags) or
           (newuri.Server <> Server) or
           (newuri.Port <> Port) or
           (newuri.Https <> TLS.Enabled) then
        begin
          Close; // relocated to another server -> reset the TCP connection
          try
            OpenBind(newuri.Server, newuri.Port, {bind=}false, newuri.Https);
          except
            ctxt.Status := HTTP_NOTFOUND;
          end;
          HttpStateReset;
          ctxt.Url := newuri.Address;
        end;
      end
      else
        fRedirected := ctxt.Url;
      inc(ctxt.Redirected);
    until fAborted;
    if Assigned(fOnAfterRequest) then
      fOnAfterRequest(self, ctxt);
  end;
  result := ctxt.Status;
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

function THttpClientSocket.WGet(const url: RawUtf8; const destfile: TFileName;
  var params: THttpClientSocketWGet): TFileName;
var
  size, expsize, start: Int64;
  res, altdownloading: integer;
  cached, part: TFileName;
  requrl, parthash, urlfile: RawUtf8;
  stream: TStreamRedirect;
  resumed, altdownload: boolean;

  function GetExpectedSizeAndRedirection: boolean;
  begin
    // try to get expected target size with a HEAD request
    res := Head(requrl, params.KeepAlive, params.Header);
    params.SetStep(wgsHead, [requrl, '=', res]);
    if not (res in HTTP_GET_OK) then
      EHttpSocket.RaiseUtf8('%.WGet: %:%/% failed as %',
        [self, fServer, fPort, requrl, StatusCodeToShort(res)]);
    expsize := Http.ContentLength;
    result := expsize > 0;
    if result and
       (fRedirected <> '') then
      // don't perform 3xx again - especially needed if server:port changed
      requrl := fRedirected;
  end;

  procedure NewStream(Mode: cardinal);
  var
    redirected: TStream;
  begin
    Mode := Mode or fmShareRead; // e.g. to allow partial PeerCache reading
    if Assigned(params.OnStreamCreate) then
      redirected := params.OnStreamCreate(part, Mode)
    else
      redirected := TFileStreamEx.Create(part, Mode);
    stream := params.Hasher.Create(redirected);
    params.SetStep(wgsNewStream, [redirected]);
  end;

  procedure DoRequestAndFreeStream;
  begin
    // prepare TStreamRedirect context
    stream.Context := urlfile;
    stream.OnProgress := params.OnProgress;
    stream.OnLog := OnLog;
    stream.TimeOut := params.TimeOutSec * 1000;
    stream.LimitPerSecond := params.LimitBandwidth;
    // perform the actual request
    res := 0;
    if Assigned(params.Alternate) and
       (params.Hasher <> nil) and
       (params.Hash <> '') and
       ((waoNoHeadFirst in params.AlternateOptions) or
        (expsize <> 0) or // ensure we made HEAD once for auth and size
        GetExpectedSizeAndRedirection) then
    begin
      // alternate download (e.g. local peer-to-peer cache) from file hash
      res := params.Alternate.OnDownload(self, params, requrl, expsize, stream);
      altdownload := res <> 0; // to notify OnDownloadFailed
      if altdownload then
        params.SetStep(wgsAlternateSuccess, [res])
      else
        params.SetStep(wgsAlternateFailed, []);
    end;
    if res = 0 then
    begin
      // notify that parallel rfProgressiveStatic mode is possible on this file
      if Assigned(params.Alternate) and
         (params.Hasher <> nil) and
         (params.Hash <> '') and
         not (waoNoProgressiveDownloading in params.AlternateOptions) and
         ((expsize <> 0) or
          GetExpectedSizeAndRedirection) then
      begin
        altdownloading := params.Alternate.OnDownloading(params, part, expsize);
        params.SetStep(wgsProgressive, [altdownloading]);
      end;
      // regular direct GET, if not done via Alternate.OnDownload()
      res := Request(requrl, 'GET', params.KeepAlive, params.Header, '', '',
        {retry=}false, {instream=}nil, stream);
      params.SetStep(wgsGet, [res]);
    end;
    // verify (partial) response
    if not (res in HTTP_GET_OK) then
      EHttpSocket.RaiseUtf8('%.WGet: %:%/% failed as %',
        [self, fServer, fPort, requrl, StatusCodeToShort(res)]);
    // finalize the successful request
    stream.Ended; // notify finished
    parthash := stream.GetHash; // hash updated on each stream.Write()
    FreeAndNil(stream);
    if Http.ContentLastModified > 0 then
    begin
      FileSetDateFromUnixUtc(part, Http.ContentLastModified);
      params.SetStep(wgsLastMod, [Http.ContentLastModified]);
    end;
  end;

  procedure AbortAlternateDownloading;
  begin
    try
      params.Alternate.OnDownloadingFailed(altdownloading);
    except
      // ignore any fatal error in callbacks
    end;
    params.SetStep(wgsProgressiveFailed, [altdownloading]);
    altdownloading := 0;
  end;

  procedure DeletePartAndResetDownload(const Context: RawUtf8);
  begin
    if altdownload then // something went wrong with OnDownload()
    try
      params.SetStep(wgsAlternateReset, [Context]);
      params.Alternate.OnDownloadFailed(params); // notify
    except
      // intercept any fatal error in callbacks
    end;
    altdownload := false;
    if altdownloading <> 0 then
      AbortAlternateDownloading;
    params.SetStep(wgsDeleteFile, [Context]);
    DeleteFile(part); // this .part was clearly incorrect
  end;

begin
  QueryPerformanceMicroSeconds(start); // for NotifyEnded() from cache
  expsize := 0;
  altdownload := false;
  altdownloading := 0;
  // check requested url and result file name
  requrl := url;
  urlfile := ExtractResourceName(url); // TUri + UrlDecode() + sanitize filename
  if urlfile = '' then
    urlfile := 'index';
  params.SetStep(wgsUrlFile, [urlfile]);
  result := destfile;
  if result = '' then
    result := GetSystemPath(spTemp) + Utf8ToString(urlfile)
  else if DirectoryExists(result) then // not a file, but a folder
    result := MakePath([result, urlfile]);
  params.SetStep(wgsFileName, [result]);
  // retrieve the .hash of this file
  TrimSelf(params.Hash);
  if params.HashFromServer and
     Assigned(params.Hasher) then
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
    params.SetStep(wgsHashed, [params.Hash]);
  end;
  // try to get from local HashCacheDir
  if (params.HashCacheDir <> '') and
     DirectoryExists(params.HashCacheDir) then
    cached := IncludeTrailingPathDelimiter(params.HashCacheDir) +
              ExtractFileName(result);
  if (destfile <> '') and
     Assigned(params.Hasher) and
     (params.Hash <> '') then
  begin
    // check if we already got the file from its md5/sha* hash
    size := FileSize(destfile);
    if (size <> 0) and
       PropNameEquals(params.Hasher.HashFile(result), params.Hash) then
    begin
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: % already available size=%',
          [url, result, size], self);
      if Assigned(params.OnProgress) then
        TStreamRedirect.NotifyEnded(params.OnProgress, nil,
          '% already available - % of',
          [urlfile, params.Hasher.GetHashName], size, start);
      params.SetStep(wgsAlreadExisting, [result]);
      exit;
    end
    else if cached <> '' then
    begin
      // check from local cache folder
      size := FileSize(cached);
      if (size <> 0) and
         PropNameEquals(params.Hasher.HashFile(cached), params.Hash) then
      begin
        if Assigned(OnLog) then
          OnLog(sllTrace, 'WGet %: copy from cached %', [url, cached], self);
        if not CopyFile(cached, result, {failexists=}false) then
          EHttpSocket.RaiseUtf8('%.WGet: copy from % cache failed',
            [self, cached]);
        if Assigned(params.OnProgress) then
          TStreamRedirect.NotifyEnded(params.OnProgress, nil,
            '% from local cache - % and copy of',
            [urlfile, params.Hasher.GetHashName], size, start);
        params.SetStep(wgsFromCache, [cached]);
        exit;
      end;
    end;
  end;
  // we need to download the file
  if not Assigned(params.Hasher) then
    params.Hasher := TStreamRedirect; // no hash by default
  if FileExists(result) then
    if not DeleteFile(result) or
       FileExists(result) then
      EHttpSocket.RaiseUtf8(
        '%.WGet: impossible to delete deprecated %', [self, result]);
  part := result + '.part';
  size := FileSize(part);
  resumed := params.Resume;
  if (size > 0) and
     resumed then
  begin
    if Assigned(OnLog) then
      OnLog(sllTrace, 'WGet %: resume % (%)', [url, part, KB(size)], self);
    params.SetStep(wgsResume, [url]);
    // try to get expected target size with a HEAD request
    if GetExpectedSizeAndRedirection and
       (size < expsize) then
    begin // seems good enough
      NewStream(fmOpenReadWrite);
      stream.Append; // hash partial content
      fRangeStart := size;
    end
    else
    begin
      resumed := false;
      params.SetStep(wgsAbortResume, [part]);
      DeleteFile(part); // reject this .part if unknown or too big
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: size=% expected=% -> reset %',
          [url, size, expsize, part], self);
      NewStream(fmCreate);
    end;
  end
  else
  begin
    resumed := false;
    if Assigned(OnLog) then
      OnLog(sllTrace, 'WGet %: start downloading %', [url, part], self);
    params.SetStep(wgsFromScratch, [part]);
    NewStream(fmCreate);
  end;
  // actual file retrieval
  try
    DoRequestAndFreeStream;
    if (params.Hash <> '') and
       (parthash <> '') then
    begin
      // check the hash after resume
      if resumed and
         not PropNameEquals(parthash, params.Hash) then
      begin
        if Assigned(OnLog) then
          OnLog(sllDebug,
            'WGet %: wrong hash after resume -> reset and retry', [url], self);
        DeletePartAndResetDownload('resume'); // get rid of wrong file
        NewStream(fmCreate);                  // setup a new output stream
        requrl := url;                        // reset any redirection
        DoRequestAndFreeStream;               // try again without any resume
      end;
      // now the hash should be correct
      if not PropNameEquals(parthash, params.Hash) then
      begin
        DeletePartAndResetDownload('hash');
        EHttpSocket.RaiseUtf8('%.WGet: %:%/% hash failure (% vs %)',
          [self, fServer, fPort, url, parthash, params.Hash]);
      end;
    end;
    // update local HashCacheDir
    if cached <> '' then
    begin
      if Assigned(OnLog) then
        OnLog(sllTrace, 'WGet %: copy into cached %', [url, cached], self);
      CopyFile(part, cached, {failsexist=}false);
    end;
    // notify e.g. THttpPeerCache of the newly downloaded file
    if Assigned(params.Alternate) and // alternate is not relevant here
       (params.Hasher <> nil) and
       (params.Hash <> '') then
      try
        params.Alternate.OnDowloaded(params, part, altdownloading);
        altdownloading := 0;
      except
        // ignore any fatal error in callbacks
      end;
    // valid .part file can now be converted into the result file
    if not RenameFile(part, result) then
      EHttpSocket.RaiseUtf8(
        '%.WGet: impossible to rename % as %', [self, part, result]);
    // set part='' to notify fully downloaded into result file name
    part := '';
  finally
    stream.Free;  // close .part file on unexpected error (if not already)
    if altdownloading <> 0 then
      AbortAlternateDownloading;
    if part <> '' then // is there still some interuppted .part content?
      if (FileSize(part) < 32768) or // not worth it, and maybe HTML error msg
         not params.Resume then      // resume is not enabled
        DeleteFile(part); // force next attempt from scratch
    if Assigned(OnLog) and
       (params.OutSteps <> []) then
      OnLog(sllTrace, 'WGet %: %', [url, ToText(params.OutSteps)], self);
  end;
end;

function THttpClientSocket.Head(const url: RawUtf8; KeepAlive: cardinal;
  const header: RawUtf8): integer;
begin
  result := Request(url, 'HEAD', KeepAlive, header);
end;

function THttpClientSocket.Post(const url: RawUtf8; const Data: RawByteString;
  const DataMimeType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'POST', KeepAlive, header, Data, DataMimeType);
end;

function THttpClientSocket.Post(const url: RawUtf8; Data: TStream;
  const DataMimeType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'POST', KeepAlive, header, '', DataMimeType, false, Data);
end;

function THttpClientSocket.Put(const url: RawUtf8; const Data: RawByteString;
  const DataMimeType: RawUtf8; KeepAlive: cardinal; const header: RawUtf8): integer;
begin
  result := Request(url, 'PUT', KeepAlive, header, Data, DataMimeType);
end;

function THttpClientSocket.Delete(const url: RawUtf8; KeepAlive: cardinal;
  const header: RawUtf8): integer;
begin
  result := Request(url, 'DELETE', KeepAlive, header);
end;

procedure THttpClientSocket.AuthorizeDigest(const UserName: RawUtf8;
  const Password: SpiUtf8; Algo: TDigestAlgo);
begin
  fDigestAuthUserName := UserName;
  fDigestAuthPassword := Password;
  fDigestAuthAlgo := Algo;
  if (UserName = '') or
     (Algo = daUndefined) then
    OnAuthorize := nil
  else
    OnAuthorize := OnAuthorizeDigest;
end;

function THttpClientSocket.OnAuthorizeDigest(Sender: THttpClientSocket;
  var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
var
  auth: RawUtf8;
  p: PUtf8Char;
begin
  p := pointer(Authenticate);
  if IdemPChar(p, 'DIGEST ') then
  begin
    auth := DigestClient(fDigestAuthAlgo, p + 7, Context.method, Context.url,
      fDigestAuthUserName, fDigestAuthPassword);
    if auth <> '' then
    begin
      auth := 'Authorization: Digest ' + auth;
      if Context.header <> '' then
        Context.header := auth + #13#10 + Context.header
      else
        Context.header := auth;
    end;
  end;
  result := true;
end;

{$ifdef DOMAINRESTAUTH}

// see https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication

procedure DoSspi(Sender: THttpClientSocket; var Context: THttpClientRequest;
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
  unauthstatus := Context.status; // either 401 or 407
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
    // here Context is the final answer (success or auth error) from the server
  finally
    FreeSecContext(sc);
    if Assigned(Sender.OnLog) then
      Sender.OnLog(sllDebug, 'DoSspi %%', [OutHeader, Context.status], Sender);
    Context.header := bak;
  end;
end;

class function THttpClientSocket.AuthorizeSspi(Sender: THttpClientSocket;
  var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
begin
  if InitializeDomainAuth then
    // try to setup sspi/gssapi -> SECPKGNAMEHTTP
    DoSspi(Sender, Context, Authenticate,
      'WWW-AUTHENTICATE: ' + SECPKGNAMEHTTP_UPPER + ' ',
      'Authorization: ' + SECPKGNAMEHTTP + ' ');
  result := false; // final RequestInternal() was done within DoSspi()
end;

class function THttpClientSocket.ProxyAuthorizeSspi(Sender: THttpClientSocket;
  var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
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
  aLayer: TNetLayer; const aUrlForProxy: RawUtf8;
  aTimeout: integer; aTLSContext: PNetTlsContext): THttpClientSocket;
var
  temp: TUri;
begin
  try
    result := THttpClientSocket.Open(aServer, aPort, aLayer, aTimeout,
      aTLS, aTLSContext, GetSystemProxyUri(aUrlForProxy, '', temp));
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
  outStatus: PInteger; aTimeout: integer; ignoreTlsCertError: boolean): RawByteString;
var
  Http: THttpClientSocket;
  status: integer;
  tls: TNetTlsContext;
begin
  result := '';
  InitNetTlsContext(tls);
  tls.IgnoreCertificateErrors := ignoreTlsCertError;
  Http := OpenHttp(server, port, aTLS, aLayer, '', aTimeout, @tls);
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
  const data: RawByteString; const header: RawUtf8; aIgnoreTlsCertificateErrors: boolean;
  timeout: integer; outHeaders: PRawUtf8; outStatus: PInteger): RawByteString;
var
  uri: TUri;
  outh: RawUtf8;
  status: integer;
begin
  result := '';
  with uri do
    if From(url) then
    try
      with self.Create(
        Server, Port, Https, '', '', timeout, timeout, timeout, Layer) do
      try
        IgnoreTlsCertificateErrors := aIgnoreTlsCertificateErrors;
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
  SendTimeout, ReceiveTimeout: cardinal; aLayer: TNetLayer;
  const aUserAgent: RawUtf8);
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
  if aUserAgent <> '' then
    fExtendedOptions.UserAgent := aUserAgent
  else
    fExtendedOptions.UserAgent := DefaultUserAgent(self);
  if ConnectionTimeOut = 0 then
    ConnectionTimeOut := HTTP_DEFAULT_CONNECTTIMEOUT;
  if SendTimeout = 0 then
    SendTimeout := HTTP_DEFAULT_SENDTIMEOUT;
  if ReceiveTimeout = 0 then
    ReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT;
  InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout); // raise exception on error
end;

constructor THttpRequest.Create(const aUri: RawUtf8; const aProxyName: RawUtf8;
  const aProxyByPass: RawUtf8; ConnectionTimeOut: cardinal; SendTimeout: cardinal;
  ReceiveTimeout: cardinal; aIgnoreTlsCertificateErrors: boolean);
var
  uri: TUri;
begin
  if not uri.From(aUri) then
    EHttpSocket.RaiseUtf8('%.Create: invalid url=%', [self, aUri]);
  IgnoreTlsCertificateErrors := aIgnoreTlsCertificateErrors;
  Create(uri.Server, uri.Port, uri.Https, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, uri.Layer);
end;

destructor THttpRequest.Destroy;
begin
  inherited Destroy;
  FillZero(fExtendedOptions.Auth.Password);
  FillZero(fExtendedOptions.Auth.Token);
end;

function THttpRequest.Request(const url, method: RawUtf8; KeepAlive: cardinal;
  const InHeader: RawUtf8; const InData: RawByteString; const InDataType: RawUtf8;
  out OutHeader: RawUtf8; out OutData: RawByteString): integer;
var
  aData: RawByteString;
  aDataEncoding, aAcceptEncoding, aUrl: RawUtf8;
  i: integer;
  upload: boolean;
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
      CompressContent(fCompressAcceptHeader, fCompress, InDataType,
        aData, aDataEncoding);
      if aDataEncoding <> '' then
        InternalAddHeader(RawUtf8('Content-Encoding: ') + aDataEncoding);
    end;
    if fCompressAcceptEncoding <> '' then
      InternalAddHeader(fCompressAcceptEncoding);
    upload:= IsPost(method) or IsPut(method);
    // send request to remote server
    if assigned(fOnUpload) and
       upload then
      fOnUpload(self, false)
    else if assigned(fOnDownload) and
            not upload then
      fOnDownload(self, false);
    InternalSendRequest(method, aData);
    // retrieve status and headers
    result := InternalRetrieveAnswer(OutHeader, aDataEncoding, aAcceptEncoding, OutData);
    if assigned(fOnUpload) and
       upload then
      fOnUpload(self, true)
    else if assigned(fOnDownload) and
            not upload then
      fOnDownload(self, true);
    // handle incoming answer compression
    if OutData <> '' then
    begin
      if aDataEncoding <> '' then
        for i := 0 to high(fCompress) do
          with fCompress[i] do
            if Name = aDataEncoding then
              if Func(OutData, false) = '' then
                EHttpSocket.RaiseUtf8('%.Request: % uncompress error', [self, Name])
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
  aIgnoreTlsCertificateErrors: boolean; outHeaders: PRawUtf8; outStatus: PInteger;
  timeout: integer): RawByteString;
begin
  result := InternalREST(aUri, 'GET', '', aHeader, aIgnoreTlsCertificateErrors,
    timeout, outHeaders, outStatus);
end;

class function THttpRequest.Post(const aUri: RawUtf8; const aData: RawByteString;
  const aHeader: RawUtf8; aIgnoreTlsCertificateErrors: boolean; outHeaders: PRawUtf8;
  outStatus: PInteger; timeout: integer): RawByteString;
begin
  result := InternalREST(aUri, 'POST', aData, aHeader,
    aIgnoreTlsCertificateErrors, timeout, outHeaders, outStatus);
end;

class function THttpRequest.Put(const aUri: RawUtf8; const aData: RawByteString;
  const aHeader: RawUtf8; aIgnoreTlsCertificateErrors: boolean;
  outHeaders: PRawUtf8; outStatus: PInteger; timeout: integer): RawByteString;
begin
  result := InternalREST(aUri, 'PUT', aData, aHeader,
    aIgnoreTlsCertificateErrors, timeout, outHeaders, outStatus);
end;

class function THttpRequest.Delete(const aUri: RawUtf8; const aHeader: RawUtf8;
  aIgnoreTlsCertificateErrors: boolean; outHeaders: PRawUtf8;
  outStatus: PInteger; timeout: integer): RawByteString;
begin
  result := InternalREST(aUri, 'DELETE', '', aHeader,
    aIgnoreTlsCertificateErrors, timeout, outHeaders, outStatus);
end;

function THttpRequest.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize, aPriority: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress, aFunction,
    fCompressAcceptEncoding, aCompressMinSize, aPriority) <> '';
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
  ua, pn, pb: SynUnicode;
begin
  WinHttpApiInitialize;
  Utf8ToSynUnicode(fExtendedOptions.UserAgent, ua);
  if fProxyName = '' then
    if (OSVersion >= wEightOne) or
       WinHttpForceProxyDetection then
      access := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY // Windows 8.1 and newer
    else
      access := WINHTTP_ACCESS_TYPE_NO_PROXY
  else
    access := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  Utf8ToSynUnicode(fProxyName, pn);
  Utf8ToSynUnicode(fProxyByPass, pb);
  fSession := WinHttpApi.Open(pointer(ua), access, pointer(pn), pointer(pb), 0);
  if (fSession = nil) and
     WinHttpForceProxyDetection and
     (access = WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY) and
     (OSVersion < wEightOne) then
  begin
    // WinHttpForceProxyDetection flag may be too optimistic: try without it
    access := WINHTTP_ACCESS_TYPE_NO_PROXY;
    fSession := WinHttpApi.Open(pointer(ua), access, pointer(pn), pointer(pb), 0);
    if fSession <> nil then
      WinHttpForceProxyDetection := false; // flag was the culprit
  end;
  if fSession = nil then
    EWinHttp.RaiseFromLastError;
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpApi.SetTimeouts(fSession, HTTP_DEFAULT_RESOLVETIMEOUT,
     ConnectionTimeOut, SendTimeout, ReceiveTimeout) then
    EWinHttp.RaiseFromLastError;
  if fHTTPS then
  begin
    protocols := InternalGetProtocols;
    if not WinHttpApi.SetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
        @protocols, SizeOf(protocols)) then
      EWinHttp.RaiseFromLastError;
    Callback := WinHttpApi.SetStatusCallback(fSession,
      WinHttpSecurityErrorCallback, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
      EWinHttp.RaiseFromLastError;
  end;
  fConnection := WinHttpApi.Connect(
    fSession, pointer(Utf8ToSynUnicode(fServer)), fPort, 0);
  if fConnection = nil then
    EWinHttp.RaiseFromLastError;
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
    EWinHttp.RaiseFromLastError;
  if fKeepAlive = 0 then
  begin
    Flags := WINHTTP_DISABLE_KEEP_ALIVE;
    if not WinHttpApi.SetOption(
       fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @Flags, SizeOf(Flags)) then
      EWinHttp.RaiseFromLastError;
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
     pointer(Utf8ToSynUnicode(hdr)), length(hdr), WINHTTP_ADDREQ_FLAG_COALESCE) then
    EWinHttp.RaiseFromLastError;
end;

procedure TWinHttp.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);

  function _SendRequest(L: cardinal): boolean;
  var
    Bytes, Current, Max, BytesWritten: cardinal;
  begin
    if Assigned(fOnUpload) and
       (IsPost(aMethod) or
        IsPut(aMethod)) then
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
            EWinHttp.RaiseFromLastError;
          inc(Current, BytesWritten);
          if not fOnUpload(Self, Current, L) then
            EWinHttp.RaiseUtf8('%: OnUpload canceled %', [self, aMethod]);
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
      EWinHttp.RaiseFromLastError;
  end;
  if fHTTPS and
     IgnoreTlsCertificateErrors then
    if not WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
      EWinHttp.RaiseFromLastError;
  L := length(aData);
  if _SendRequest(L) and
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success
  if fHTTPS and
     (GetLastError = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) and
     IgnoreTlsCertificateErrors and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
       pointer(WINHTTP_NO_CLIENT_CERT_CONTEXT), 0) and
     _SendRequest(L) and
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success with no certificate validation
  // if we reached here, an error occurred
  EWinHttp.RaiseFromLastError;
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
    EWinHttp.RaiseFromLastError;
end;

function TWinHttp.InternalReadData(var Data: RawByteString; Read: integer;
  Size: cardinal): cardinal;
begin
  if not WinHttpApi.ReadData(fRequest, @PByteArray(Data)[Read], Size, result) then
    EWinHttp.RaiseFromLastError;
end;

destructor TWinHttp.Destroy;
begin
  if fConnection <> nil then
    WinHttpApi.CloseHandle(fConnection);
  if fSession <> nil then
    WinHttpApi.CloseHandle(fSession);
  inherited Destroy;
end;


{ EWinHttp }

class procedure EWinHttp.RaiseFromLastError;
begin
  RaiseLastModuleError(winhttpdll, EWinHttp);
end;


{ EWinINet }

class procedure EWinINet.RaiseFromLastError;
var
  err: integer;
  E: EWinINet;
begin
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  err := GetLastError;
  E := CreateUtf8('% (%)', [SysErrorMessageWinInet(err), err]);
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
  fSession := InternetOpenA(pointer(fExtendedOptions.UserAgent), OpenType,
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
  FRequest := HttpOpenRequestA(FConnection, pointer(aMethod), pointer(aUrl),
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
     not HttpAddRequestHeadersA(fRequest, pointer(hdr), length(hdr),
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
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal; aLayer: TNetLayer;
  const aUserAgent: RawUtf8);
begin
  inherited Create(aServer, aPort, aHttps, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, aLayer, aUserAgent);
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
      EWinHttp.RaiseUtf8('%.Create: % handshake failed', [self, _http]);
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
      pointer(aCloseReason), Length(aCloseReason));
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
      pointer(CloseReason), Length(CloseReason));
    SetLength(reason, WEB_SOCKET_MAX_CLOSE_REASON_LENGTH);
    WinHttpApi.WebSocketQueryCloseStatus(fSocket, status, pointer(reason),
      WEB_SOCKET_MAX_CLOSE_REASON_LENGTH, reasonLength);
    WinHttpApi.CloseHandle(fSocket);
  end;
  inherited Destroy;
end;

{$endif USEWININET}


{$ifdef USELIBCURL}

{ ECurlHttp }

constructor ECurlHttp.Create(error: TCurlResult;
  const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  fError:= error;
end;


{ TCurlHttp }

procedure TCurlHttp.InternalConnect(ConnectionTimeOut, SendTimeout,
  ReceiveTimeout: cardinal);
const
  HTTPS: array[boolean] of string[1] = (
    '',
    's');
begin
  if not IsAvailable then
    raise EOSException.CreateFmt('No available %s', [LIBCURL_DLL]);
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
  result := fTls.CACertFile;
end;

procedure TCurlHttp.SetCACertFile(const aCertFile: RawUtf8);
begin
  fTls.CACertFile := aCertFile;
end;

procedure TCurlHttp.UseClientCertificate(const aCertFile, aCACertFile, aKeyName,
  aPassPhrase: RawUtf8);
begin
  fTls.CertFile := aCertFile;
  fTls.CACertFile := aCACertFile;
  fTls.KeyName := aKeyName;
  fTls.PassPhrase := aPassPhrase;
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
    if IgnoreTlsCertificateErrors then
    begin
      curl.easy_setopt(fHandle, coSSLVerifyPeer, 0);
      curl.easy_setopt(fHandle, coSSLVerifyHost, 0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyPeer,0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyHost,0);
    end
    else
    begin
      // see https://curl.haxx.se/libcurl/c/simplessl.html
      if fTls.CertFile <> '' then
      begin
        curl.easy_setopt(fHandle, coSSLCertType, pointer(CERT_PEM));
        curl.easy_setopt(fHandle, coSSLCert, pointer(fTls.CertFile));
        if fTls.PassPhrase <> '' then
          curl.easy_setopt(fHandle, coSSLCertPasswd, pointer(fTls.PassPhrase));
        curl.easy_setopt(fHandle, coSSLKeyType, nil);
        curl.easy_setopt(fHandle, coSSLKey, pointer(fTls.KeyName));
        curl.easy_setopt(fHandle, coCAInfo, pointer(fTls.CACertFile));
        curl.easy_setopt(fHandle, coSSLVerifyPeer, 1);
      end
      else if fTls.CACertFile <> '' then
        curl.easy_setopt(fHandle, coCAInfo, pointer(fTls.CACertFile));
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

const
  WRA2CAU: array[THttpRequestAuthentication] of TCurlAuths = (
    [],             // wraNone
    [cauBasic],     // wraBasic
    [cauDigest],    // wraDigest
    [cauNegotiate], // wraNegotiate
    [cauBearer]);   // wraBearer

procedure TCurlHttp.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);
var
  username: RawUtf8;
  password, tok: SpiUtf8;
begin
  // 1. handle authentication
  case AuthScheme of
    wraBasic,
    wraDigest,
    wraNegotiate:
      begin
        // expect user/password credentials
        username := SynUnicodeToUtf8(AuthUserName);
        password := SynUnicodeToUtf8(AuthPassword);
      end;
    wraBearer:
      tok := AuthToken;
  end;
  curl.easy_setopt(fHandle, coUserName, pointer(username));
  curl.easy_setopt(fHandle, coPassword, pointer(password));
  curl.easy_setopt(fHandle, coXOAuth2Bearer, pointer(tok));
  curl.easy_setopt(fHandle, coHttpAuth, integer(WRA2CAU[AuthScheme]));
  FillZero(password);
  // 2. main request options
  // the only verbs which do not expect body in answer are HEAD and OPTIONS
  curl.easy_setopt(fHandle, coNoBody, ord(HttpMethodWithNoBody(fIn.Method)));
  // see http://curl.haxx.se/libcurl/c/CURLOPT_CUSTOMREQUEST.html
  curl.easy_setopt(fHandle, coCustomRequest, pointer(fIn.Method));
  curl.easy_setopt(fHandle, coPostFields, pointer(aData));
  curl.easy_setopt(fHandle, coPostFieldSize, length(aData));
  curl.easy_setopt(fHandle, coHttpHeader, fIn.Headers);
  curl.easy_setopt(fHandle, coWriteData, @fOut.Data);
  curl.easy_setopt(fHandle, coWriteHeader, @fOut.Header);
end;

function xfer_info(clientp: pointer;
  dltotal, dlnow, ultotal, ulnow: Int64): integer; cdecl;
// should be a separated function for FPC
var
  s: TCurlHttp;
begin
  s := TCurlHttp(clientp);
  if Assigned(s.OnUploadProgress) and
     ((ulnow <> s.fLast.ulNow) or (ultotal <> s.fLast.ulTotal)) then
  begin
    s.OnUploadProgress(s, ulnow, ultotal);
    s.fLast.ulTotal := ultotal;
    s.fLast.ulNow := ulnow;
  end;
  if Assigned(s.OnDownloadProgress) and
     ((dlnow <> s.fLast.dlNow) or (dltotal <> s.fLast.dlTotal)) then
  begin
    s.OnDownloadProgress(s, dlnow, dltotal);
    s.fLast.dlTotal := dltotal;
    s.fLast.dlNow := dlnow;
  end;
  result := 0;
end;

function TCurlHttp.InternalRetrieveAnswer(
  var Header, Encoding, AcceptEncoding: RawUtf8; var Data: RawByteString): integer;
var
  res: TCurlResult;
  P: PUtf8Char;
  s: RawUtf8;
  i: integer;
  rc: PtrInt; // needed on Linux x86-64
begin
  if Assigned(OnUploadProgress) or
     Assigned(OnDownloadProgress) then
  begin
    fLast.dlTotal := -1;
    fLast.dlNow := -1;
    fLast.ulTotal := -1;
    fLast.ulNow := -1;
    curl.easy_setopt(fHandle, coXferInfoData, pointer(self));
    curl.easy_setopt(fHandle, coXferInfoFunction, @xfer_info);
    curl.easy_setopt(fHandle, coNoProgress, 0);
  end
  else
    curl.easy_setopt(fHandle, coNoProgress, 1);
  res := curl.easy_perform(fHandle);
  curl.easy_setopt(fHandle, coNoProgress, 1);
  curl.easy_setopt(fHandle, coXferInfoFunction, nil);
  curl.easy_setopt(fHandle, coXferInfoData, nil);
  if res <> crOK then
    raise ECurlHttp.Create(res, 'libcurl error %d (%s) on %s %s',
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
      TrimCopy(s, 17, 100, AcceptEncoding)
    else if IdemPChar(pointer(s), 'CONTENT-ENCODING:') then
      TrimCopy(s, 18, 100, Encoding);
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
  const DataMimeType: RawUtf8; KeepAlive: cardinal): integer;
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
        Uri.Server, Uri.Port, Uri.Https, Proxy, '',
        fTimeOut, fTimeOut, fTimeOut, nlTcp, fUserAgent);
      fHttps.IgnoreTlsCertificateErrors := fSocketTLS.IgnoreCertificateErrors;
    end;
    result := fHttps.Request(
      Uri.Address, Method, KeepAlive, Header, Data, DataMimeType, fHeaders, fBody);
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
        Uri.Address, Method, KeepAlive, Header, Data, DataMimeType, true);
    fBody := fHttp.Http.Content;
    fHeaders := fHttp.Http.Headers;
    if KeepAlive = 0 then
      FreeAndNil(fHttp);
  except
    FreeAndNil(fHttp);
  end;
end;

function TSimpleHttpClient.Request(const Uri: RawUtf8; const Method: RawUtf8;
  const Header: RawUtf8; const Data: RawByteString; const DataMimeType: RawUtf8;
  KeepAlive: cardinal): integer;
var
  u: TUri;
begin
  fUri := Uri;
  if u.From(Uri) then
    result := RawRequest(u, Method, Header, Data, DataMimeType, KeepAlive)
  else
    result := HTTP_NOTFOUND;
  fStatus := result;
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
    _MainHttpClass := TCurlHttp;
    {$endif USELIBCURL}
    {$endif USEWININET}
    if _MainHttpClass = nil then
      raise EHttpSocket.Create('MainHttpClass: No THttpRequest class known!');
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
  fClient := TSimpleHttpClient.Create(aOnlyUseClientSocket);
  if aUri <> '' then
    if not LoadFromUri(aUri, aToken) then
      ESynException.RaiseUtf8('%.Create: invalid aUri=%', [self, aUri]);
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
    AppendLine(headin, [fTokenHeader]);
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
  forceNotSocket: boolean; outStatus: PInteger; timeout: integer;
  forceSocket, ignoreTlsCertError: boolean): RawByteString;
begin
  result := HttpGet(aUri, '', outHeaders,
    forceNotSocket, outStatus, timeout, forceSocket, ignoreTlsCertError);
end;

function HttpGet(const aUri: RawUtf8; const inHeaders: RawUtf8;
  outHeaders: PRawUtf8; forceNotSocket: boolean; outStatus: PInteger;
  timeout: integer; forceSocket, ignoreTlsCertError: boolean): RawByteString;
var
  uri: TUri;
begin
  if uri.From(aUri) then
    if (uri.Https or
        forceNotSocket) and
       not forceSocket then
      {$ifdef USEWININET}
      result := TWinHttp.Get(
        aUri, inHeaders, ignoreTlsCertError, outHeaders, outStatus, timeout)
      {$else}
      {$ifdef USELIBCURL}
      if TCurlHttp.IsAvailable then
        result := TCurlHttp.Get(
          aUri, inHeaders, ignoreTlsCertError, outHeaders, outStatus)
      else
      {$endif USELIBCURL}
        // fallback to SChannel/OpenSSL if libcurl is not installed
        result := OpenHttpGet(uri.Server, uri.Port, uri.Address,
          inHeaders, outHeaders, uri.Layer, uri.Https, outStatus,
          timeout, ignoreTlsCertError)
      {$endif USEWININET}
    else
      result := OpenHttpGet(uri.Server, uri.Port, uri.Address,
        inHeaders, outHeaders, uri.Layer, uri.Https, outStatus,
        timeout, ignoreTlsCertError)
    else
      result := '';
  {$ifdef LINUX_RAWDEBUGVOIDHTTPGET}
  if result = '' then
    writeln('HttpGet returned VOID for ',uri.server,':',uri.Port,' ',uri.Address);
  {$endif LINUX_RAWDEBUGVOIDHTTPGET}
end;



{ ************** Send Email using the SMTP Protocol }

function TSmtpConnection.FromText(const aText: RawUtf8): boolean;
var
  u, h: RawUtf8;
begin
  if aText = SMTP_DEFAULT then
  begin
    result := false;
    exit;
  end;
  h := SplitRight(aText, '@', @u);
  if (u <> '') and
     not Split(u, ':', User, Pass) then
    User := u;
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

function SendEmail(const Server: TSmtpConnection;
  const From, CsvDest, Subject: RawUtf8; const Text: RawByteString;
  const Headers, TextCharSet: RawUtf8; TLS: boolean): boolean;
begin
  result := SendEmail(
    Server.Host, From, CsvDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet,
    TLS or (Server.Port = '465') or (Server.Port = '587'));
end;

{$I-}

function SendEmail(const Server, From, CsvDest, Subject: RawUtf8;
  const Text: RawByteString; const Headers, User, Pass, Port, TextCharSet: RawUtf8;
  TLS: boolean): boolean;
var
  sock: TCrtSocket;

  procedure Expect(const answer: RawUtf8);
  var
    res: RawUtf8;
  begin
    repeat
      readln(sock.SockIn^, res);
      if ioresult <> 0 then
        ESendEmail.RaiseUtf8('read error for %', [res]);
    until (Length(res) < 4) or
          (res[4] <> '-'); // - indicates there are other headers following
    if IdemPChar(pointer(res), pointer(answer)) then
      exit;
    if res = '' then
      res := 'Undefined Error';
    ESendEmail.RaiseUtf8('Command failed for % at %:% [%]',
      [User, Server, Port, res]);
  end;

  procedure Exec(const Command, answer: RawUtf8);
  begin
    sock.SockSendFlush(Command + #13#10);
    if ioresult <> 0 then
      ESendEmail.RaiseUtf8('Write error for %', [Command]);
    Expect(answer)
  end;

var
  P: PUtf8Char;
  rec, ToList, head: RawUtf8;
begin
  result := false;
  P := pointer(CsvDest);
  if P = nil then
    exit;
  sock := SocketOpen(Server, Port, TLS);
  if sock <> nil then
  try
    sock.CreateSockIn; // we use SockIn for readln in Expect()
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
    sock.SockSend(['MAIL FROM:<', From, '>']);
    sock.SockSendFlush;
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
    sock.SockSend([
      'Subject: ', Subject, #13#10 +
      'From: ', From, ToList]);
    head := trimU(Headers);
    if (TextCharSet <> '') or
       (head = '') then
      sock.SockSend([
        'Content-Type: text/plain; charset=', TextCharSet, #13#10 +
        'Content-Transfer-Encoding: 8bit']);
    if head <> '' then
      sock.SockSend(head);
    sock.SockSendCRLF; // end of headers
    sock.SockSend(Text);
    Exec('.', '25');
    Exec('QUIT', '22');
    result := ioresult = 0;
  finally
    sock.Free;
  end;
end;

{$I+}

function SendEmailSubject(const Text: string): RawUtf8;
begin
  StringToUtf8(Text, result{%H-});
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


procedure InitializeUnit;
begin
  NewSocketAddressCache := TNewSocketAddressCache.Create(600); // 10 min timeout
  NetClientProtocols := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
    TypeInfo(TMethodDynArray), {caseinsensitive=}true);
  RegisterNetClientProtocol('file', TNetClientProtocolFile.Create.OnRequest);
end;

procedure FinalizeUnit;
var
  i: integer;
  m: PMethod;
begin
  NewSocketAddressCache := nil;
  m := NetClientProtocols.Values.Value^;
  for i := 1 to NetClientProtocols.Values.Count do
  begin
    TObject(m^.Data).Free; // was owned by this unit
    inc(m);
  end;
  NetClientProtocols.Free;
end;


initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

