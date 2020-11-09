/// HTTP/HTTPS Client Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.client;

{
  *****************************************************************************

   HTTP Client Classes
   - THttpClientSocket Implementing HTTP client over plain sockets
   - THttpRequest Abstract HTTP client class
   - TWinHttp TWinINet TWinHttpWebSocketClient TCurlHTTP
   - Cached HTTP Connection to a Remote Server

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
  mormot.lib.winhttp,
  {$endif USEWININET}
  {$ifdef USELIBCURL}  // as set in mormot.defines.inc
  mormot.lib.curl,
  {$endif USELIBCURL}
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.text,
  mormot.core.data,
  mormot.core.json; // TSynDictionary for THttpRequestCached



{ ************** THttpClientSocket Implementing HTTP client over plain sockets }

type
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
  // - don't forget to use Free procedure when you are finished
  THttpClientSocket = class(THttpSocket)
  protected
    fUserAgent: RawUTF8;
    fProcessName: RawUTF8;
    procedure RequestSendHeader(const url, method: RawUTF8); virtual;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    // - you can customize the default client timeouts by setting appropriate
    // aTimeout parameters (in ms) if you left the 0 default parameters,
    // it would use global HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(aTimeOut: PtrInt = 0); override;
    /// low-level HTTP/1.1 request
    // - called by all Get/Head/Post/Put/Delete REST methods
    // - after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    // - retry is false by caller, and will be recursively called with true to retry once
    function Request(const url, method: RawUTF8; KeepAlive: cardinal;
      const header: RawUTF8; const Data: RawByteString; const DataType: RawUTF8;
      retry: boolean): integer; virtual;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    function Get(const url: RawUTF8; KeepAlive: cardinal = 0;
      const header: RawUTF8 = ''): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    // - if AuthToken<>'', will add an header with 'Authorization: Bearer '+AuthToken
    function GetAuth(const url, AuthToken: RawUTF8; KeepAlive: cardinal = 0): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise - only
    // header is read from server: Content is always '', but Headers are set
    function Head(const url: RawUTF8; KeepAlive: cardinal = 0;
      const header: RawUTF8 = ''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Post(const url: RawUTF8; const Data: RawByteString;
      const DataType: RawUTF8; KeepAlive: cardinal = 0;
      const header: RawUTF8 = ''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Put(const url: RawUTF8; const Data: RawByteString;
      const DataType: RawUTF8; KeepAlive: cardinal = 0;
      const header: RawUTF8 = ''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    function Delete(const url: RawUTF8; KeepAlive: cardinal = 0;
      const header: RawUTF8 = ''): integer;

    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    property UserAgent: RawUTF8 read fUserAgent write fUserAgent;
    /// the associated process name
    property ProcessName: RawUTF8 read fProcessName write fProcessName;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // mormot.net.websock unit)
  THttpClientSocketClass = class of THttpClientSocket;


{ ******************** THttpRequest Abstract HTTP client class }

type
  /// the supported authentication schemes which may be used by HTTP clients
  // - supported only by TWinHTTP class yet
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
    // - IgnoreSSLCertificateErrors is handled by TWinHttp and TCurlHTTP
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
    UserAgent: RawUTF8;
  end;

  {$M+} // to have existing RTTI for published properties
  /// abstract class to handle HTTP/1.1 request
  // - never instantiate this class, but inherited TWinHTTP, TWinINet or TCurlHTTP
  THttpRequest = class
  protected
    fServer: RawUTF8;
    fProxyName: RawUTF8;
    fProxyByPass: RawUTF8;
    fPort: cardinal;
    fHttps: boolean;
    fLayer: TNetLayer;
    fKeepAlive: cardinal;
    fExtendedOptions: THttpRequestExtendedOptions;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawUTF8;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressAcceptHeader: THttpSocketCompressSet;
    fTag: PtrInt;
    class function InternalREST(const url, method: RawUTF8; const data:
      RawByteString; const header: RawUTF8; aIgnoreSSLCertificateErrors: boolean;
      outHeaders: PRawUTF8 = nil; outStatus: PInteger = nil): RawByteString;
    // inherited class should override those abstract methods
    procedure InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal); virtual; abstract;
    procedure InternalCreateRequest(const aMethod, aURL: RawUTF8); virtual; abstract;
    procedure InternalSendRequest(const aMethod: RawUTF8; const aData:
      RawByteString); virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUTF8;
      var Data: RawByteString): integer; virtual; abstract;
    procedure InternalCloseRequest; virtual; abstract;
    procedure InternalAddHeader(const hdr: RawUTF8); virtual; abstract;
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; virtual; abstract;
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy:
    // aProxyName/aProxyByPass will be recognized by TWinHTTP and TWinINet,
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
    constructor Create(const aServer, aPort: RawUTF8; aHttps: boolean;
      const aProxyName: RawUTF8 = ''; const aProxyByPass: RawUTF8 = '';
      ConnectionTimeOut: cardinal = 0; SendTimeout: cardinal = 0;
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTCP); overload; virtual;
    /// connect to the supplied URI
    // - is just a wrapper around TURI and the overloaded Create() constructor
    constructor Create(const aURI: RawUTF8; const aProxyName: RawUTF8 = '';
      const aProxyByPass: RawUTF8 = ''; ConnectionTimeOut: cardinal = 0;
      SendTimeout: cardinal = 0; ReceiveTimeout: cardinal = 0;
      aIgnoreSSLCertificateErrors: boolean = false); overload;

    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwise
    // - KeepAlive is in milliseconds, 0 for "Connection: Close" HTTP/1.0 requests
    function Request(const url, method: RawUTF8; KeepAlive: cardinal;
      const InHeader: RawUTF8; const InData: RawByteString; const InDataType: RawUTF8;
      out OutHeader: RawUTF8; out OutData: RawByteString): integer; virtual;

    /// wrapper method to retrieve a resource via an HTTP GET
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Get() but either TWinHTTP.Get(), TWinINet.Get() or
    // TCurlHTTP.Get() methods
    class function Get(const aURI: RawUTF8; const aHeader: RawUTF8 = '';
      aIgnoreSSLCertificateErrors: boolean = true; outHeaders: PRawUTF8 = nil;
      outStatus: PInteger = nil): RawByteString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Post() but either TWinHTTP.Post(), TWinINet.Post() or
    // TCurlHTTP.Post() methods
    class function Post(const aURI: RawUTF8; const aData: RawByteString;
      const aHeader: RawUTF8 = ''; aIgnoreSSLCertificateErrors: boolean = true;
      outHeaders: PRawUTF8 = nil; outStatus: PInteger = nil): RawByteString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Put() but either TWinHTTP.Put(), TWinINet.Put() or
    // TCurlHTTP.Put() methods
    class function Put(const aURI: RawUTF8; const aData: RawByteString;
      const aHeader: RawUTF8 = ''; aIgnoreSSLCertificateErrors: boolean = true;
      outHeaders: PRawUTF8 = nil; outStatus: PInteger = nil): RawByteString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Delete() but either TWinHTTP.Delete(), TWinINet.Delete() or
    // TCurlHTTP.Delete() methods
    class function Delete(const aURI: RawUTF8; const aHeader: RawUTF8 = '';
      aIgnoreSSLCertificateErrors: boolean = true; outHeaders: PRawUTF8 = nil;
      outStatus: PInteger = nil): RawByteString;

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress; aCompressMinSize: integer = 1024): boolean;

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
    property UserAgent: RawUTF8
      read fExtendedOptions.UserAgent
      write fExtendedOptions.UserAgent;
    /// internal structure used to store extended options
    // - will be replicated by IgnoreSSLCertificateErrors and Auth* properties
    property ExtendedOptions: THttpRequestExtendedOptions
      read fExtendedOptions
      write fExtendedOptions;
    /// some internal field, which may be used by end-user code
    property Tag: PtrInt read fTag write fTag;
  published
    /// the remote server host name, as stated specified to the class constructor
    property Server: RawUTF8 read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: cardinal read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: RawUTF8 read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: RawUTF8 read fProxyByPass;
  end;
  {$M-}

  /// store the actual class of a HTTP/1.1 client instance
  // - may be used to define at runtime which API to be used (e.g. WinHTTP,
  // WinINet or LibCurl), following the Liskov substitution principle

  THttpRequestClass = class of THttpRequest;


{$ifdef USEWININET}

{ ******************** TWinHttp TWinINet TWinHttpWebSocketClient }

type
  TWinHttpAPI = class;

  /// event callback to track download progress, e.g. in the UI
  // - used in TWinHttpAPI.OnProgress property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  TOnWinHttpProgress = procedure(Sender: TWinHttpAPI;
    CurrentSize, ContentLength: cardinal) of object;

  /// event callback to process the download by chunks, not in memory
  // - used in TWinHttpAPI.OnDownload property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  // - ChunkSize is the size of the latest downloaded chunk, available in
  // the untyped ChunkData memory buffer
  // - implementation should return TRUE to continue the download, or FALSE
  // to abort the download process
  TWinHttpDownload = function(Sender: TWinHttpAPI; CurrentSize, ContentLength,
    ChunkSize: cardinal; const ChunkData): boolean of object;

  /// event callback to track upload progress, e.g. in the UI
  // - used in TWinHttpAPI.OnUpload property
  // - CurrentSize is the current total number of uploaded bytes
  // - ContentLength is the size of content
  // - implementation should return TRUE to continue the upload, or FALSE
  // to abort the upload process
  TWinHttpUpload = function(Sender: TWinHttpAPI;
    CurrentSize, ContentLength: cardinal): boolean of object;

  /// a class to handle HTTP/1.1 request using either WinINet or WinHTTP API
  // - both APIs have a common logic, which is encapsulated by this parent class
  // - this abstract class defined some abstract methods which will be
  // implemented by TWinINet or TWinHttp with the proper API calls
  TWinHttpAPI = class(THttpRequest)
  protected
    fOnProgress: TOnWinHttpProgress;
    fOnDownload: TWinHttpDownload;
    fOnUpload: TWinHttpUpload;
    fOnDownloadChunkSize: cardinal;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    /// do not add "Accept: */*" HTTP header by default
    fNoAllAccept: boolean;
    function InternalGetInfo(Info: cardinal): RawUTF8; virtual; abstract;
    function InternalGetInfo32(Info: cardinal): cardinal; virtual; abstract;
    function InternalQueryDataAvailable: cardinal; virtual; abstract;
    function InternalReadData(var Data: RawByteString; Read: integer;
      Size: cardinal): cardinal; virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUTF8;
      var Data: RawByteString): integer; override;
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; override;
    /// do not add "Accept: */*" HTTP header by default
    property NoAllAccept: boolean read fNoAllAccept write fNoAllAccept;
    /// download would call this method to notify progress of incoming data
    property OnProgress: TOnWinHttpProgress read fOnProgress write fOnProgress;
    /// download would call this method instead of filling Data: RawByteString value
    // - may be used e.g. when downloading huge content, and saving directly
    // the incoming data on disk or database
    // - if this property is set, raw TCP/IP incoming data would be supplied:
    // compression and encoding won't be handled by the class
    property OnDownload: TWinHttpDownload read fOnDownload write fOnDownload;
    /// upload would call this method to notify progress of outgoing data
    // - and optionally abort sending the data by returning FALSE
    property OnUpload: TWinHttpUpload read fOnUpload write fOnUpload;
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
  TWinINet = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EWinINet exception on error
    procedure InternalConnect(ConnectionTimeOut, SendTimeout,
      ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aURL: RawUTF8); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUTF8); override;
    procedure InternalSendRequest(const aMethod: RawUTF8;
      const aData: RawByteString); override;
    function InternalGetInfo(Info: cardinal): RawUTF8; override;
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
    /// create a WinINet exception, with the error message as text
    constructor Create;
  published
    /// the associated WSAGetLastError value
    property LastError: integer read fLastError;
  end;

  /// a class to handle HTTP/1.1 request using the WinHTTP API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure https connection - so it seems to be the
  // class to use in your client programs
  // - WinHTTP does not share any proxy settings with Internet Explorer.
  // The WinHTTP proxy configuration is set by either
  // $ proxycfg.exe
  // on Windows XP and Windows Server 2003 or earlier, either
  // $ netsh.exe
  // on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run either:
  // $ proxycfg -u
  // $ netsh winhttp import proxy source=ie
  // to use the current user's proxy settings for Internet Explorer (under 64-bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - Microsoft Windows HTTP Services (WinHTTP) is targeted at middle-tier and
  // back-end server applications that require access to an HTTP client stack
  TWinHTTP = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EOSError exception on error
    procedure InternalConnect(ConnectionTimeOut, SendTimeout,
      ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aURL: RawUTF8); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUTF8); override;
    procedure InternalSendRequest(const aMethod: RawUTF8;
      const aData: RawByteString); override;
    function InternalGetInfo(Info: cardinal): RawUTF8; override;
    function InternalGetInfo32(Info: cardinal): cardinal; override;
    function InternalQueryDataAvailable: cardinal; override;
    function InternalReadData(var Data: RawByteString; Read: integer;
      Size: cardinal): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinHTTP exception type
  EWinHTTP = class(Exception);

  /// establish a client connection to a WebSocket server using the Windows API
  // - used by TWinWebSocketClient class
  TWinHTTPUpgradeable = class(TWinHTTP)
  private
    fSocket: HINTERNET;
  protected
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding:
      RawUTF8; var Data: RawByteString): integer; override;
    procedure InternalSendRequest(const aMethod: RawUTF8;
      const aData: RawByteString); override;
  public
    /// initialize the instance
    constructor Create(const aServer, aPort: RawUTF8; aHttps: boolean;
      const aProxyName: RawUTF8 = ''; const aProxyByPass: RawUTF8 = '';
      ConnectionTimeOut: cardinal = 0; SendTimeout: cardinal = 0;
      ReceiveTimeout: cardinal = 0; aLayer: TNetLayer = nlTCP); override;
  end;

  /// WebSocket client implementation
  TWinHTTPWebSocketClient = class
  protected
    fSocket: HINTERNET;
    function CheckSocket: boolean;
  public
    /// initialize the instance
    // - all parameters do match TWinHTTP.Create except url: address of WebSocketServer
    // for sending upgrade request
    constructor Create(const aServer, aPort: RawUTF8; aHttps: boolean;
      const url: RawUTF8; const aSubProtocol: RawUTF8 = ''; const aProxyName: RawUTF8 = '';
      const aProxyByPass: RawUTF8 = ''; ConnectionTimeOut: cardinal = 0;
      SendTimeout: cardinal = 0; ReceiveTimeout: cardinal = 0);
    /// send buffer
    function Send(aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer;
      aBufferLength: cardinal): cardinal;
    /// receive buffer
    function Receive(aBuffer: pointer; aBufferLength: cardinal;
      out aBytesRead: cardinal; out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): cardinal;
    /// close current connection
    function CloseConnection(const aCloseReason: RawUTF8): cardinal;
    /// finalize the instance
    destructor Destroy; override;
  end;

{$endif USEWININET}

{$ifdef USELIBCURL}

type
  /// libcurl exception type
  ECurlHTTP = class(Exception);

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
  TCurlHTTP = class(THttpRequest)
  protected
    fHandle: pointer;
    fRootURL: RawUTF8;
    fIn: record
      Headers: pointer;
      DataOffset: integer;
      URL, Method: RawUTF8;
      Data: RawByteString;
    end;
    fOut: record
      Header, Encoding, AcceptEncoding: RawUTF8;
      Data: RawByteString;
    end;
    fSSL: record
      CertFile, CACertFile, KeyName, PassPhrase: RawUTF8;
    end;
    procedure InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal); override;
    procedure InternalCreateRequest(const aMethod, aURL: RawUTF8); override;
    procedure InternalSendRequest(const aMethod: RawUTF8;
      const aData: RawByteString); override;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding: RawUTF8;
      var Data: RawByteString): integer; override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: RawUTF8); override;
    function GetCACertFile: RawUTF8;
    procedure SetCACertFile(const aCertFile: RawUTF8);
  public
    /// returns TRUE if the class is actually supported on this system
    class function IsAvailable: boolean; override;
    /// release the connection
    destructor Destroy; override;
    /// allow to set a CA certification file without touching the client certification
    property CACertFile: RawUTF8 read GetCACertFile write SetCACertFile;
    /// set the client SSL certification details
    // - see CACertFile if you don't want to change the whole client cert info
    // - used e.g. as
    // ! UseClientCertificate('testcert.pem','cacert.pem','testkey.pem','pass');
    procedure UseClientCertificate(
      const aCertFile, aCACertFile, aKeyName, aPassPhrase: RawUTF8);
  end;

{$endif USELIBCURL}


{ ************** Cached HTTP Connection to a Remote Server }

type
  /// in-memory storage of one THttpRequestCached entry
  THttpRequestCache = record
    Tag: RawUTF8;
    Content: RawByteString;
  end;
  /// in-memory storage of all THttpRequestCached entries
  THttpRequestCacheDynArray = array of THttpRequestCache;

  /// handles cached HTTP connection to a remote server
  // - use in-memory cached content when HTTP_NOTMODIFIED (304) is returned
  // for an already known ETAG header value
  THttpRequestCached = class(TSynPersistent)
  protected
    fURI: TURI;
    fHttp: THttpRequest; // either fHttp or fSocket is used
    fSocket: THttpClientSocket;
    fKeepAlive: integer;
    fTokenHeader: RawUTF8;
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
    constructor Create(const aURI: RawUTF8; aKeepAliveSeconds: integer = 30;
      aTimeoutSeconds: integer = 15*60; const aToken: RawUTF8 = '';
      aHttpClass: THttpRequestClass = nil); reintroduce;
    /// finalize the current connnection and flush its in-memory cache
    // - you may use LoadFromURI() to connect to a new server
    procedure Clear;
    /// connect to a new server
    // - aToken is an optional token which will be transmitted as HTTP header:
    // $ Authorization: Bearer <aToken>
    // - TWinHttp will be used by default under Windows, unless you specify
    // another class
    function LoadFromURI(const aURI: RawUTF8; const aToken: RawUTF8 = '';
      aHttpClass: THttpRequestClass = nil): boolean;
    /// finalize the cache
    destructor Destroy; override;
    /// retrieve a resource from the server, or internal cache
    // - aModified^ = true if server returned a HTTP_SUCCESS (200) with some new
    // content, or aModified^ = false if HTTP_NOTMODIFIED (304) was returned
    function Get(const aAddress: RawUTF8; aModified: PBoolean = nil;
      aStatus: PInteger = nil): RawByteString;
    /// erase one resource from internal cache
    function Flush(const aAddress: RawUTF8): boolean;
    /// read-only access to the connected server
    property URI: TURI read fURI;
  end;


implementation

{$ifdef USEWININET}
uses
  WinINet;
{$endif USEWININET}



{ ************** THttpClientSocket Implementing HTTP client over plain sockets }

function DefaultUserAgent(Instance: TObject): RawUTF8;
begin
  // note: the framework would identify 'mORMot' pattern in the user-agent
  // header to enable advanced behavior e.g. about JSON transmission
  FormatUTF8('Mozilla/5.0 (' + OS_TEXT + '; mORMot ' +
    SYNOPSE_FRAMEWORK_VERSION + ' %)', [Instance], result);
end;

{ THttpClientSocket }

procedure THttpClientSocket.RequestSendHeader(const url, method: RawUTF8);
begin
  if not SockIsDefined then
    exit;
  if SockIn = nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  if TCPPrefix <> '' then
    SockSend(TCPPrefix);
  if (url = '') or
     (url[1] <> '/') then
    SockSend([method, ' /', url, ' HTTP/1.1'])
  else
    SockSend([method, ' ', url, ' HTTP/1.1']);
  if Port = DEFAULT_PORT[fTLS] then
    SockSend(['Host: ', Server])
  else
    SockSend(['Host: ', Server, ':', Port]);
  SockSend(['Accept: */*'#13#10'User-Agent: ', UserAgent]);
end;

constructor THttpClientSocket.Create(aTimeOut: PtrInt);
begin
  if aTimeOut = 0 then
    aTimeOut := HTTP_DEFAULT_RECEIVETIMEOUT;
  inherited Create(aTimeOut);
  fUserAgent := DefaultUserAgent(self);
end;

function THttpClientSocket.Request(const url, method: RawUTF8; KeepAlive:
  cardinal; const header: RawUTF8; const Data: RawByteString; const DataType:
  RawUTF8; retry: boolean): integer;

  procedure DoRetry(Error: integer; const msg: RawUTF8);
  begin
    {$ifdef SYNCRTDEBUGLOW}
    TSynLog.Add.Log(sllCustom2,
      'Request: % socket=% DoRetry(%) retry=%',
      [msg, Sock, Error, BOOL_STR[retry]], self);
    {$endif SYNCRTDEBUGLOW}
    if retry then // retry once -> return error only if failed after retrial
      result := Error
    else
    begin
      Close; // close this connection
      try
        OpenBind(Server, Port, false); // retry this request with a new socket
        result := Request(url, method, KeepAlive, header, Data, DataType, true);
      except
        on Exception do
          result := Error;
      end;
    end;
  end;

var
  P: PUTF8Char;
  aData: RawByteString;
begin
  if SockIn = nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  Content := '';
  if SockReceivePending(0) = cspSocketError then
  begin
    DoRetry(HTTP_NOTFOUND, 'connection broken (keepalive timeout?)');
    exit;
  end;
  try
    try
      // send request - we use SockSend because writeln() is calling flush()
      // -> all headers will be sent at once
      RequestSendHeader(url, method);
      if KeepAlive > 0 then
        SockSend(['Keep-Alive: ', KeepAlive, #13#10'Connection: Keep-Alive'])
      else
        SockSend('Connection: Close');
      aData := Data; // local var copy for Data to be compressed in-place
      CompressDataAndWriteHeaders(DataType, aData);
      if header <> '' then
        SockSend(header);
      if fCompressAcceptEncoding <> '' then
        SockSend(fCompressAcceptEncoding);
      SockSend; // send CRLF
      SockSendFlush(aData); // flush all pending data to network
      // get headers
      if SockReceivePending(1000) = cspSocketError then
      begin
        DoRetry(HTTP_NOTFOUND, 'cspSocketError waiting for headers');
        exit;
      end;
      SockRecvLn(Command); // will raise ECrtSocket on any error
      if TCPPrefix <> '' then
        if Command <> TCPPrefix then
        begin
          result := HTTP_HTTPVERSIONNONSUPPORTED; // 505
          exit;
        end
        else
          SockRecvLn(Command);
      P := pointer(Command);
      if IdemPChar(P, 'HTTP/1.') then
      begin
        result := GetCardinal(P + 9); // get http numeric status code (200,404...)
        if result = 0 then
        begin
          result := HTTP_HTTPVERSIONNONSUPPORTED;
          exit;
        end;
        while result = 100 do
        begin
          repeat // 100 CONTINUE is just to be ignored client side
            SockRecvLn(Command);
            P := pointer(Command);
          until IdemPChar(P, 'HTTP/1.');  // ignore up to next command
          result := GetCardinal(P + 9);
        end;
        if P[7] = '0' then
          KeepAlive := 0; // HTTP/1.0 -> force connection close
      end
      else
      begin // error on reading answer
        DoRetry(HTTP_HTTPVERSIONNONSUPPORTED, Command); // 505=wrong format
        exit;
      end;
      GetHeader(false); // read all other headers
      if (result <> HTTP_NOCONTENT) and
         (IdemPCharArray(pointer(method), ['HEAD', 'OPTIONS']) < 0) then
        GetBody; // get content if necessary (not HEAD/OPTIONS methods)
    except
      on Exception do
        DoRetry(HTTP_NOTFOUND, 'Exception');
    end;
  finally
    if KeepAlive = 0 then
      Close;
  end;
end;

function THttpClientSocket.Get(const url: RawUTF8; KeepAlive: cardinal;
  const header: RawUTF8): integer;
begin
  result := Request(url, 'GET', KeepAlive, header, '', '', false);
end;

function THttpClientSocket.GetAuth(const url, AuthToken: RawUTF8;
  KeepAlive: cardinal): integer;
begin
  result := Get(url, KeepAlive, AuthorizationBearer(AuthToken));
end;

function THttpClientSocket.Head(const url: RawUTF8; KeepAlive: cardinal;
  const header: RawUTF8): integer;
begin
  result := Request(url, 'HEAD', KeepAlive, header, '', '', false);
end;

function THttpClientSocket.Post(const url: RawUTF8; const Data: RawByteString;
  const DataType: RawUTF8; KeepAlive: cardinal; const header: RawUTF8): integer;
begin
  result := Request(url, 'POST', KeepAlive, header, Data, DataType, false);
end;

function THttpClientSocket.Put(const url: RawUTF8; const Data: RawByteString;
  const DataType: RawUTF8; KeepAlive: cardinal; const header: RawUTF8): integer;
begin
  result := Request(url, 'PUT', KeepAlive, header, Data, DataType, false);
end;

function THttpClientSocket.Delete(const url: RawUTF8; KeepAlive: cardinal;
  const header: RawUTF8): integer;
begin
  result := Request(url, 'DELETE', KeepAlive, header, '', '', false);
end;


{ THttpRequest }

class function THttpRequest.InternalREST(const url, method: RawUTF8;
  const data: RawByteString; const header: RawUTF8; aIgnoreSSLCertificateErrors: boolean;
  outHeaders: PRawUTF8; outStatus: PInteger): RawByteString;
var
  URI: TURI;
  outh: RawUTF8;
  status: integer;
begin
  result := '';
  with URI do
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

constructor THttpRequest.Create(const aServer, aPort: RawUTF8; aHttps: boolean;
  const aProxyName: RawUTF8; const aProxyByPass: RawUTF8; ConnectionTimeOut,
  SendTimeout, ReceiveTimeout: cardinal; aLayer: TNetLayer);
begin
  fLayer := aLayer;
  if fLayer <> nlUNIX then
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

constructor THttpRequest.Create(const aURI: RawUTF8; const aProxyName: RawUTF8;
  const aProxyByPass: RawUTF8; ConnectionTimeOut: cardinal; SendTimeout: cardinal;
  ReceiveTimeout: cardinal; aIgnoreSSLCertificateErrors: boolean);
var
  URI: TURI;
begin
  if not URI.From(aURI) then
    raise EHttpSocket.CreateFmt('%.Create: invalid url=%',
      [ClassNameShort(self)^, aURI]);
  IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
  Create(URI.Server, URI.Port, URI.Https, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, URI.Layer);
end;

function THttpRequest.Request(const url, method: RawUTF8; KeepAlive: cardinal;
  const InHeader: RawUTF8; const InData: RawByteString; const InDataType: RawUTF8;
  out OutHeader: RawUTF8; out OutData: RawByteString): integer;
var
  aData: RawByteString;
  aDataEncoding, aAcceptEncoding, aURL: RawUTF8;
  i: integer;
begin
  if (url = '') or
     (url[1] <> '/') then
    aURL := '/' + url
  else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  fKeepAlive := KeepAlive;
  InternalCreateRequest(method, aURL); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType <> '' then
      InternalAddHeader(RawUTF8('Content-Type: ') + InDataType);
    // handle custom compression
    aData := InData;
    if integer(fCompressAcceptHeader) <> 0 then
    begin
      aDataEncoding := CompressDataAndGetHeaders(fCompressAcceptHeader,
        fCompress, InDataType, aData);
      if aDataEncoding <> '' then
        InternalAddHeader(RawUTF8('Content-Encoding: ') + aDataEncoding);
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
                raise EHttpSocket.CreateFmt('%s uncompress', [Name])
              else
                break; // successfully uncompressed content
      if aAcceptEncoding <> '' then
        fCompressAcceptHeader := ComputeContentEncoding(fCompress, pointer(aAcceptEncoding));
    end;
  finally
    InternalCloseRequest;
  end;
end;

class function THttpRequest.Get(const aURI: RawUTF8; const aHeader: RawUTF8;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUTF8; outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aURI, 'GET', '', aHeader, aIgnoreSSLCertificateErrors,
    outHeaders, outStatus);
end;

class function THttpRequest.Post(const aURI: RawUTF8; const aData: RawByteString;
  const aHeader: RawUTF8; aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUTF8;
  outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aURI, 'POST', aData, aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

class function THttpRequest.Put(const aURI: RawUTF8; const aData: RawByteString;
  const aHeader: RawUTF8; aIgnoreSSLCertificateErrors: boolean; outHeaders:
  PRawUTF8; outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aURI, 'PUT', aData, aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

class function THttpRequest.Delete(const aURI: RawUTF8; const aHeader: RawUTF8;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PRawUTF8;
  outStatus: PInteger): RawByteString;
begin
  result := InternalREST(aURI, 'DELETE', '', aHeader,
    aIgnoreSSLCertificateErrors, outHeaders, outStatus);
end;

function THttpRequest.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress, aFunction, fCompressAcceptEncoding,
    aCompressMinSize) <> '';
end;



{$ifdef USEWININET}

{ ******************** TWinHttp TWinINet TWinHttpWebSocketClient }

{ TWinHttpAPI }

function TWinHttpAPI.InternalRetrieveAnswer(var Header, Encoding,
  AcceptEncoding: RawUTF8; var Data: RawByteString): integer;
var
  ChunkSize, Bytes, ContentLength, Read: cardinal;
  tmp: RawByteString;
begin // HTTP_QUERY* and WINHTTP_QUERY* do match -> common to TWinINet + TWinHTTP
  result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
  Header := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
  Encoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
  AcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
  // retrieve received content (if any)
  Read := 0;
  ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
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
end;

class function TWinHttpAPI.IsAvailable: boolean;
begin
  result := true; // both WinINet and WinHTTP are statically linked
end;




{ TWinHTTP }

procedure TWinHTTP.InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
var
  OpenType: integer;
  Callback: WINHTTP_STATUS_CALLBACK;
  CallbackRes: PtrInt absolute Callback; // for FPC compatibility
  // MPV - don't know why, but if I pass WINHTTP_FLAG_SECURE_PROTOCOL_SSL2
  // flag also, TLS1.2 does not work
  protocols: cardinal;
begin
  if fProxyName = '' then
    if OSVersion >= wEightOne then
      // Windows 8.1 and newer
      OpenType := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY
    else
      OpenType := WINHTTP_ACCESS_TYPE_NO_PROXY
  else
    OpenType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  fSession := WinHttpAPI.Open(pointer(UTF8ToSynUnicode(fExtendedOptions.UserAgent)),
    OpenType, pointer(UTF8ToSynUnicode(fProxyName)), pointer(UTF8ToSynUnicode(fProxyByPass)), 0);
  if fSession = nil then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpAPI.SetTimeouts(fSession, HTTP_DEFAULT_RESOLVETIMEOUT,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout) then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
  if fHTTPS then
  begin
    protocols := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
    // Windows 7 and newer supports TLS 1.1 & 1.2
    if OSVersion >= wSeven then
      protocols := protocols or
        (WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2);
    if not WinHttpAPI.SetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
        @protocols, SizeOf(protocols)) then
      RaiseLastModuleError(winhttpdll, EWinHTTP);
    Callback := WinHttpAPI.SetStatusCallback(fSession,
      WinHTTPSecurityErrorCallback, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
      RaiseLastModuleError(winhttpdll, EWinHTTP);
  end;
  fConnection := WinHttpAPI.Connect(fSession, pointer(UTF8ToSynUnicode(fServer)),
    fPort, 0);
  if fConnection = nil then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
end;

procedure TWinHTTP.InternalCreateRequest(const aMethod, aURL: RawUTF8);
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
  fRequest := WinHttpAPI.OpenRequest(fConnection, pointer(UTF8ToSynUnicode(aMethod)),
    pointer(UTF8ToSynUnicode(aURL)), nil, nil, ACCEPT_TYPES[fNoAllAccept], Flags);
  if fRequest = nil then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
  if fKeepAlive = 0 then
  begin
    Flags := WINHTTP_DISABLE_KEEP_ALIVE;
    if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @Flags,
      sizeOf(Flags)) then
      RaiseLastModuleError(winhttpdll, EWinHTTP);
  end;
end;

procedure TWinHTTP.InternalCloseRequest;
begin
  if fRequest <> nil then
  begin
    WinHttpAPI.CloseHandle(fRequest);
    FRequest := nil;
  end;
end;

procedure TWinHTTP.InternalAddHeader(const hdr: RawUTF8);
begin
  if (hdr <> '') and
     not WinHttpAPI.AddRequestHeaders(FRequest,
    Pointer(UTF8ToSynUnicode(hdr)), length(hdr), WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
end;

procedure TWinHTTP.InternalSendRequest(const aMethod: RawUTF8;
  const aData: RawByteString);

  function _SendRequest(L: cardinal): boolean;
  var
    Bytes, Current, Max, BytesWritten: cardinal;
  begin
    if Assigned(fOnUpload) and
       (IdemPropNameU(aMethod, 'POST') or IdemPropNameU(aMethod, 'PUT')) then
    begin
      result := WinHttpAPI.SendRequest(fRequest, nil, 0, nil, 0, L, 0);
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
          if not WinHttpAPI.WriteData(fRequest, @PByteArray(aData)[Current],
             Bytes, BytesWritten) then
            RaiseLastModuleError(winhttpdll, EWinHTTP);
          inc(Current, BytesWritten);
          if not fOnUpload(Self, Current, L) then
            raise EWinHTTP.CreateFmt('OnUpload Canceled %s', [aMethod]);
        end;
      end;
    end
    else
      result := WinHttpAPI.SendRequest(fRequest, nil, 0, pointer(aData), L, L, 0);
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
        raise EWinHTTP.CreateFmt('Unsupported AuthScheme=%d', [ord(AuthScheme)]);
      end;
      if not WinHttpAPI.SetCredentials(fRequest, WINHTTP_AUTH_TARGET_SERVER,
         winAuth, pointer(AuthUserName), pointer(AuthPassword), nil) then
        RaiseLastModuleError(winhttpdll, EWinHTTP);
    end;
  if fHTTPS and IgnoreSSLCertificateErrors then
    if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
      RaiseLastModuleError(winhttpdll, EWinHTTP);
  L := length(aData);
  if not _SendRequest(L) or not WinHttpAPI.ReceiveResponse(fRequest, nil) then
  begin
    if not fHTTPS then
      RaiseLastModuleError(winhttpdll, EWinHTTP);
    if (GetLastError = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) and
      IgnoreSSLCertificateErrors then
    begin
      if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
         @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
        RaiseLastModuleError(winhttpdll, EWinHTTP);
      if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
         pointer(WINHTTP_NO_CLIENT_CERT_CONTEXT), 0) then
        RaiseLastModuleError(winhttpdll, EWinHTTP);
      if not _SendRequest(L) or not WinHttpAPI.ReceiveResponse(fRequest, nil) then
        RaiseLastModuleError(winhttpdll, EWinHTTP);
    end;
  end;
end;

function TWinHTTP.InternalGetInfo(Info: cardinal): RawUTF8;
var
  dwSize, dwIndex: cardinal;
  tmp: TSynTempBuffer;
  i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpAPI.QueryHeaders(fRequest, Info, nil, nil, dwSize, dwIndex) and
     (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
  begin
    tmp.Init(dwSize);
    if WinHttpAPI.QueryHeaders(fRequest, Info, nil, tmp.buf, dwSize, dwIndex) then
    begin
      dwSize := dwSize shr 1;
      SetLength(result, dwSize);
      for i := 0 to dwSize - 1 do // fast ANSI 7 bit conversion
        PByteArray(result)^[i] := PWordArray(tmp.buf)^[i];
    end;
    tmp.Done;
  end;
end;

function TWinHTTP.InternalGetInfo32(Info: cardinal): cardinal;
var
  dwSize, dwIndex: cardinal;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpAPI.QueryHeaders(fRequest, Info, nil, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinHTTP.InternalQueryDataAvailable: cardinal;
begin
  if not WinHttpAPI.QueryDataAvailable(fRequest, result) then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
end;

function TWinHTTP.InternalReadData(var Data: RawByteString; Read: integer;
  Size: cardinal): cardinal;
begin
  if not WinHttpAPI.ReadData(fRequest, @PByteArray(Data)[Read], Size, result) then
    RaiseLastModuleError(winhttpdll, EWinHTTP);
end;

destructor TWinHTTP.Destroy;
begin
  if fConnection <> nil then
    WinHttpAPI.CloseHandle(fConnection);
  if fSession <> nil then
    WinHttpAPI.CloseHandle(fSession);
  inherited Destroy;
end;


{ EWinINet }

constructor EWinINet.Create;
begin // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  fLastError := GetLastError;
  inherited CreateFmt('%s (%d)', [SysErrorMessageWinInet(fLastError), fLastError]);
end;


{ TWinINet }

procedure TWinINet.InternalConnect(ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
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
    raise EWinINet.Create;
  InternetSetOption(fConnection, INTERNET_OPTION_CONNECT_TIMEOUT, @ConnectionTimeOut,
    SizeOf(ConnectionTimeOut));
  InternetSetOption(fConnection, INTERNET_OPTION_SEND_TIMEOUT, @SendTimeout,
    SizeOf(SendTimeout));
  InternetSetOption(fConnection, INTERNET_OPTION_RECEIVE_TIMEOUT, @ReceiveTimeout,
    SizeOf(ReceiveTimeout));
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort, nil, nil,
    INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection = nil then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCreateRequest(const aMethod, aURL: RawUTF8);
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
  FRequest := HttpOpenRequestA(FConnection, Pointer(aMethod), Pointer(aURL), nil,
    nil, ACCEPT_TYPES[fNoAllAccept], Flags, 0);
  if FRequest = nil then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest <> nil then
  begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;

procedure TWinINet.InternalAddHeader(const hdr: RawUTF8);
begin
  if (hdr <> '') and
     not HttpAddRequestHeadersA(fRequest, Pointer(hdr), length(hdr),
       HTTP_ADDREQ_FLAG_COALESCE) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalSendRequest(const aMethod: RawUTF8; const aData:
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
      raise EWinINet.Create;
    datapos := 0;
    while datapos < datalen do
    begin
      Bytes := fOnDownloadChunkSize;
      if Bytes <= 0 then
        Bytes := 65536; // 64KB seems fair enough by default
      max := datalen - datapos;
      if Bytes > max then
        Bytes := max;
      if not InternetWriteFile(fRequest, @PByteArray(aData)[datapos], Bytes,
        BytesWritten) then
        raise EWinINet.Create;
      inc(datapos, BytesWritten);
      if not fOnUpload(Self, datapos, datalen) then
        raise EWinINet.CreateFmt('OnUpload Canceled %s', [aMethod]);
    end;
    if not HttpEndRequest(fRequest, nil, 0, 0) then
      raise EWinINet.Create;
  end
  else // blocking send with no callback
if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    raise EWinINet.Create;
end;

function TWinINet.InternalGetInfo(Info: cardinal): RawUTF8;
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
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalQueryDataAvailable: cardinal;
begin
  if not InternetQueryDataAvailable(fRequest, result, 0, 0) then
    raise EWinINet.Create;
end;

function TWinINet.InternalReadData(var Data: RawByteString; Read: integer; Size:
  cardinal): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], Size, result) then
    raise EWinINet.Create;
end;

destructor TWinINet.Destroy;
begin
  if fConnection <> nil then
    InternetCloseHandle(FConnection);
  if fSession <> nil then
    InternetCloseHandle(FSession);
  inherited Destroy;
end;


{ TWinHTTPUpgradeable }

function TWinHTTPUpgradeable.InternalRetrieveAnswer(var Header, Encoding,
  AcceptEncoding: RawUTF8; var Data: RawByteString): integer;
begin
  result := inherited InternalRetrieveAnswer(Header, Encoding, AcceptEncoding, Data);
end;

procedure TWinHTTPUpgradeable.InternalSendRequest(const aMethod: RawUTF8; const
  aData: RawByteString);
begin
  inherited InternalSendRequest(aMethod, aData);
end;

constructor TWinHTTPUpgradeable.Create(const aServer, aPort: RawUTF8; aHttps:
  boolean; const aProxyName: RawUTF8; const aProxyByPass: RawUTF8;
  ConnectionTimeOut: cardinal; SendTimeout: cardinal; ReceiveTimeout: cardinal; aLayer: TNetLayer);
begin
  inherited Create(aServer, aPort, aHttps, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, aLayer);
end;


{ TWinHTTPWebSocketClient }

function TWinHTTPWebSocketClient.CheckSocket: boolean;
begin
  result := fSocket <> nil;
end;

constructor TWinHTTPWebSocketClient.Create(const aServer, aPort: RawUTF8; aHttps:
  boolean; const url: RawUTF8; const aSubProtocol: RawUTF8; const aProxyName:
  RawUTF8; const aProxyByPass: RawUTF8; ConnectionTimeOut: cardinal; SendTimeout:
  cardinal; ReceiveTimeout: cardinal);
var
  _http: TWinHTTPUpgradeable;
  inH, outH: RawUTF8;
  outD: RawByteString;
begin
  fSocket := nil;
  _http := TWinHTTPUpgradeable.Create(aServer, aPort, aHttps, aProxyName,
    aProxyByPass, ConnectionTimeOut, SendTimeout, ReceiveTimeout);
  try
    // WebSocketAPI.BeginClientHandshake()
    if aSubProtocol <> '' then
      inH := HTTP_WEBSOCKET_PROTOCOL + ': ' + aSubProtocol
    else
      inH := '';
    if _http.Request(url, 'GET', 0, inH, '', '', outH, outD) = 101 then
      fSocket := _http.fSocket
    else
      raise EWinHTTP.Create('WebSocketClient creation fail');
  finally
    _http.Free;
  end;
end;

function TWinHTTPWebSocketClient.Send(aBufferType:
  WINHTTP_WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer; aBufferLength: cardinal): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpAPI.WebSocketSend(fSocket, aBufferType, aBuffer, aBufferLength);
end;

function TWinHTTPWebSocketClient.Receive(aBuffer: pointer; aBufferLength: cardinal;
  out aBytesRead: cardinal; out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpAPI.WebSocketReceive(fSocket, aBuffer, aBufferLength,
      aBytesRead, aBufferType);
end;

function TWinHTTPWebSocketClient.CloseConnection(const aCloseReason: RawUTF8): cardinal;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE
  else
    result := WinHttpAPI.WebSocketClose(fSocket, WEB_SOCKET_SUCCESS_CLOSE_STATUS,
      Pointer(aCloseReason), Length(aCloseReason));
  if result = 0 then
    fSocket := nil;
end;

destructor TWinHTTPWebSocketClient.Destroy;
const
  CloseReason: PAnsiChar = 'object is destroyed';
var
  status: Word;
  reason: RawUTF8;
  reasonLength: cardinal;
begin
  if CheckSocket then
  begin // todo: check result
    WinHttpAPI.WebSocketClose(fSocket, WEB_SOCKET_ABORTED_CLOSE_STATUS, Pointer(CloseReason),
      Length(CloseReason));
    SetLength(reason, WEB_SOCKET_MAX_CLOSE_REASON_LENGTH);
    WinHttpAPI.WebSocketQueryCloseStatus(fSocket, status, Pointer(reason),
      WEB_SOCKET_MAX_CLOSE_REASON_LENGTH, reasonLength);
    WinHttpAPI.CloseHandle(fSocket);
  end;
  inherited Destroy;
end;

{$endif USEWININET}


{$ifdef USELIBCURL}

{ TCurlHTTP }

procedure TCurlHTTP.InternalConnect(ConnectionTimeOut, SendTimeout,
  ReceiveTimeout: cardinal);
const
  HTTPS: array[boolean] of string[1] = (
    '', 's');
begin
  if not IsAvailable then
    raise ENetSock.CreateFmt('No available %s', [LIBCURL_DLL]);
  fHandle := curl.easy_init;
  ConnectionTimeOut := ConnectionTimeOut div 1000; // curl expects seconds
  if ConnectionTimeOut = 0 then
    ConnectionTimeOut := 1;
  curl.easy_setopt(fHandle, coConnectTimeout, ConnectionTimeOut); // default=300 !
  // coTimeout=CURLOPT_TIMEOUT is global for the transfer, so shouldn't be used
  if fLayer = nlUNIX then
    // see CURLOPT_UNIX_SOCKET_PATH doc
    fRootURL := 'http://localhost'
  else
    FormatUTF8('http%://%:%', [HTTPS[fHttps], fServer, fPort], fRootURL);
end;

destructor TCurlHTTP.Destroy;
begin
  if fHandle <> nil then
    curl.easy_cleanup(fHandle);
  inherited;
end;

function TCurlHTTP.GetCACertFile: RawUTF8;
begin
  result := fSSL.CACertFile;
end;

procedure TCurlHTTP.SetCACertFile(const aCertFile: RawUTF8);
begin
  fSSL.CACertFile := aCertFile;
end;

procedure TCurlHTTP.UseClientCertificate(const aCertFile, aCACertFile, aKeyName,
  aPassPhrase: RawUTF8);
begin
  fSSL.CertFile := aCertFile;
  fSSL.CACertFile := aCACertFile;
  fSSL.KeyName := aKeyName;
  fSSL.PassPhrase := aPassPhrase;
end;

procedure TCurlHTTP.InternalCreateRequest(const aMethod, aURL: RawUTF8);
const
  CERT_PEM: RawUTF8 = 'PEM';
begin
  fIn.URL := fRootURL + aURL;
  curl.easy_setopt(fHandle, coFollowLocation, 1); // url redirection (as TWinHTTP)
  //curl.easy_setopt(fHandle,coTCPNoDelay,0); // disable Nagle
  if fLayer = nlUNIX then
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

procedure TCurlHTTP.InternalAddHeader(const hdr: RawUTF8);
var
  P: PUTF8Char;
  s: RawUTF8;
begin
  P := pointer(hdr);
  while P <> nil do
  begin
    s := GetNextLine(P, P);
    if s <> '' then // nil would reset the whole list
      fIn.Headers := curl.slist_append(fIn.Headers, pointer(s));
  end;
end;

class function TCurlHTTP.IsAvailable: boolean;
begin
  result := CurlIsAvailable;
end;

procedure TCurlHTTP.InternalSendRequest(const aMethod: RawUTF8; const aData:
  RawByteString);
begin // see http://curl.haxx.se/libcurl/c/CURLOPT_CUSTOMREQUEST.html
  if fIn.Method = 'HEAD' then // the only verb what do not expect body in answer is HEAD
    curl.easy_setopt(fHandle, coNoBody, 1)
  else
    curl.easy_setopt(fHandle, coNoBody, 0);
  curl.easy_setopt(fHandle, coCustomRequest, pointer(fIn.Method));
  curl.easy_setopt(fHandle, coPostFields, pointer(aData));
  curl.easy_setopt(fHandle, coPostFieldSize, length(aData));
  curl.easy_setopt(fHandle, coHTTPHeader, fIn.Headers);
  curl.easy_setopt(fHandle, coFile, @fOut.Data);
  curl.easy_setopt(fHandle, coWriteHeader, @fOut.Header);
end;

function TCurlHTTP.InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding:
  RawUTF8; var Data: RawByteString): integer;
var
  res: TCurlResult;
  P: PUTF8Char;
  s: RawUTF8;
  i: integer;
  rc: longint; // needed on Linux x86-64
begin
  res := curl.easy_perform(fHandle);
  if res <> crOK then
    raise ECurlHTTP.CreateFmt('libcurl error %d (%s) on %s %s', [ord(res), curl.easy_strerror
      (res), fIn.Method, fIn.URL]);
  rc := 0;
  curl.easy_getinfo(fHandle, ciResponseCode, rc);
  result := rc;
  Header := Trim(fOut.Header);
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

procedure TCurlHTTP.InternalCloseRequest;
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


{ ************** Cached HTTP Connection to a Remote Server }

{ THttpRequestCached }

constructor THttpRequestCached.Create(const aURI: RawUTF8; aKeepAliveSeconds,
  aTimeoutSeconds: integer; const aToken: RawUTF8; aHttpClass: THttpRequestClass);
begin
  inherited Create;
  fKeepAlive := aKeepAliveSeconds * 1000;
  if aTimeoutSeconds > 0 then // 0 means no cache
    fCache := TSynDictionary.Create(TypeInfo(TRawUTF8DynArray),
      TypeInfo(THttpRequestCacheDynArray), true, aTimeoutSeconds);
  if not LoadFromURI(aURI, aToken, aHttpClass) then
    raise ESynException.CreateUTF8('%.Create: invalid aURI=%', [self, aURI]);
end;

procedure THttpRequestCached.Clear;
begin
  FreeAndNil(fHttp); // either fHttp or fSocket is used
  FreeAndNil(fSocket);
  if fCache <> nil then
    fCache.DeleteAll;
  fURI.Clear;
  fTokenHeader := '';
end;

destructor THttpRequestCached.Destroy;
begin
  fCache.Free;
  fHttp.Free;
  fSocket.Free;
  inherited Destroy;
end;

function THttpRequestCached.Get(const aAddress: RawUTF8; aModified: PBoolean;
  aStatus: PInteger): RawByteString;
var
  cache: THttpRequestCache;
  headin, headout: RawUTF8;
  status: integer;
  modified: boolean;
begin
  result := '';
  if (fHttp = nil) and
     (fSocket = nil) then // either fHttp or fSocket is used
    exit;
  if (fCache <> nil) and
     fCache.FindAndCopy(aAddress, cache) then
    FormatUTF8('If-None-Match: %', [cache.Tag], headin);
  if fTokenHeader <> '' then
  begin
    if {%H-}headin <> '' then
      headin := headin + #13#10;
    headin := headin + fTokenHeader;
  end;
  if fSocket <> nil then
  begin
    status := fSocket.Get(aAddress, fKeepAlive, headin);
    result := fSocket.Content;
  end
  else
    status := fHttp.Request(aAddress, 'GET', fKeepAlive, headin, '', '', headout, result);
  modified := true;
  case status of
    HTTP_SUCCESS:
      if fCache <> nil then
      begin
        if fHttp <> nil then
          FindNameValue(headout{%H-}, 'ETAG:', cache.Tag)
        else
          cache.Tag := fSocket.HeaderGetValue('ETAG');
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

function THttpRequestCached.LoadFromURI(const aURI, aToken: RawUTF8;
  aHttpClass: THttpRequestClass): boolean;
begin
  result := false;
  if (self = nil) or
     (fHttp <> nil) or
     (fSocket <> nil) or
     not fURI.From(aURI) then
    exit;
  fTokenHeader := AuthorizationBearer(aToken);
  if aHttpClass = nil then
  begin
    {$ifdef USEWININET}
    aHttpClass := TWinHTTP;
    {$else}
    {$ifdef USELIBCURL}
    if fURI.Https then
      aHttpClass := TCurlHTTP;
    {$endif USELIBCURL}
    {$endif USEWININET}
  end;
  if aHttpClass = nil then
    fSocket := THttpClientSocket.Open(fURI.Server, fURI.Port)
  else
    fHttp := aHttpClass.Create(fURI.Server, fURI.Port, fURI.Https);
  result := true;
end;

function THttpRequestCached.Flush(const aAddress: RawUTF8): boolean;
begin
  if fCache <> nil then
    result := fCache.Delete(aAddress) >= 0
  else
    result := true;
end;

initialization

finalization

end.

