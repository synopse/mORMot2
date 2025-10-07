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
   - TWinHttp TWinINet TCurlHttp classes
   - IHttpClient / TSimpleHttpClient Wrappers
   - TJsonClient JSON requests over HTTP
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
  mormot.core.variants,
  mormot.net.sock,
  mormot.net.http,
  {$ifdef USEWININET}  // as set in mormot.defines.inc
  wininet,
  mormot.lib.winhttp,
  {$ifdef FORCE_OPENSSL}
  mormot.lib.openssl11, // ensure bypass SChannel for a given project
  {$endif FORCE_OPENSSL}
  {$endif USEWININET}
  {$ifdef USELIBCURL}  // as set in mormot.defines.inc
  mormot.lib.curl,
  {$endif USELIBCURL}
  {$ifdef DOMAINRESTAUTH}
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  {$endif DOMAINRESTAUTH}
  mormot.crypt.core,
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

{ high-level definitions, shared with both THttpRequest and THttpClientSocket }

type
  /// the supported authentication schemes which may be used by HTTP clients
  // - not supported by all classes (e.g. TWinINet won't support all schemes)
  THttpRequestAuthentication = (
    wraNone,
    wraBasic,
    wraDigest,
    wraNegotiate,
    wraNegotiateChannelBinding,
    wraBearer);

  /// pointer to some extended options for HTTP clients
  PHttpRequestExtendedOptions = ^THttpRequestExtendedOptions;

  /// a record to set some extended options for HTTP clients
  // - allow easy propagation e.g. from a TRestHttpClient* wrapper class to
  // the actual mormot.net.http's THttpRequest implementation class
  {$ifdef USERECORDWITHMETHODS}
  THttpRequestExtendedOptions = record
  {$else}
  THttpRequestExtendedOptions = object
  {$endif USERECORDWITHMETHODS}
  private
    procedure AuthorizeUserPassword(const UserName, Password: RawUtf8;
      Scheme: THttpRequestAuthentication);
  public
    /// allow to customize the HTTPS process
    // - used only during initial connection
    TLS: TNetTlsContext;
    /// optional Proxy URI
    // - if kept to its default '', will try to use the system PROXY
    // - if set to 'none', won't use any proxy
    // - otherwise, will use this value as explicit proxy server name
    // - used only during initial connection
    Proxy: RawUtf8;
    /// the timeout to be used for the whole connection, as supplied to Create()
    CreateTimeoutMS: integer;
    /// allow HTTP/HTTPS authentication to take place at server request
    Auth: record
      Scheme: THttpRequestAuthentication;
      UserName: RawUtf8;
      Password: SpiUtf8;
      Token: SpiUtf8;
    end;
    /// how many times THttpClientSocket/TWinHttp should redirect 30x responses
    // - TCurlHttp would only check for RedirectMax > 0 with no exact count
    // - TWinINet won't support this parameter
    RedirectMax: integer;
    /// allow to customize the User-Agent header
    // - for TWinHttp, should be set at constructor level
    UserAgent: RawUtf8;
    /// may be used to initialize this record on stack with zeroed values
    procedure Init;
    /// reset this record, calling FillZero() on Password/Token SpiUtf8 values
    procedure Clear;
    /// setup web authentication using the Basic access algorithm
    procedure AuthorizeBasic(const UserName: RawUtf8; const Password: SpiUtf8);
    /// setup web authentication using the Digest access algorithm
    procedure AuthorizeDigest(const UserName: RawUtf8; const Password: SpiUtf8);
    /// setup web authentication using Kerberos via SSPI/GSSAPI and credentials
    // - if you want to authenticate with the current logged user, just set
    // ! Auth.Scheme := wraNegotiate;
    procedure AuthorizeSspiUser(const UserName: RawUtf8; const Password: SpiUtf8);
    /// setup web authentication using a given Bearer in the request headers
    procedure AuthorizeBearer(const Value: SpiUtf8);
    /// compare the Auth fields, depending on their scheme
    function SameAuth(Another: PHttpRequestExtendedOptions): boolean;
    /// persist all fields of this record as a TDocVariant
    // - returns e.g. {"ti":1,"as":3} for TLS.IgnoreCertificateErrors = true
    // and Auth.Scheme = wraNegotiate
    function ToDocVariant(const Secret: RawByteString = ''): variant;
    /// persist all fields of this record as a URI-encoded TDocVariant
    // - returns e.g. '/root?ti=1&as=3' for TLS.IgnoreCertificateErrors = true
    // and Auth.Scheme = wraNegotiate and UriRoot = '/root'
    function ToUrlEncode(const UriRoot: RawUtf8;
      const Secret: RawByteString = ''): RawUtf8;
    /// reset this record, then set all fields from a ToDocVariant() value
    function InitFromDocVariant(const Value: variant;
      const Secret: RawByteString = ''): boolean;
    /// reset this record, then set all fields from a URI-encoded ToDocVariant()
    // - expects UrlParams to be just after the '?', e.g.  'ti=1&as=3' for
    // TLS.IgnoreCertificateErrors = true and Auth.Scheme = wraNegotiate
    function InitFromUrl(const UrlParams: RawUtf8;
      const Secret: RawByteString = ''): boolean;
  end;

function ToText(wra: THttpRequestAuthentication): PShortString; overload;

/// persist main TNetTlsContext input fields into a TDocVariant
function SaveNetTlsContext(const TLS: TNetTlsContext;
  const Secret: RawByteString = ''): variant;

/// fill TNetTlsContext input fields from a SaveNetTlsContext() TDocVariant
procedure LoadNetTlsContext(var TLS: TNetTlsContext; const V: TDocVariantData;
  const Secret: RawByteString = '');


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
  /// maintain one partial download for THttpPartials
  THttpPartial = record
    /// genuine 31-bit positive identifier, 0 if empty/recyclable
    ID: THttpPartialID;
    /// the expected full size of this download
    FullSize: Int64;
    /// the partial file name currently downloaded
    PartFile: TFileName;
    /// up to 512-bit of raw binary hash, precessed by hash algo byte
    Digest: THashDigest;
    /// background HTTP requests which are waiting for data on this download
    HttpContext: array of PHttpRequestContext;
  end;
  PHttpPartial = ^THttpPartial;

  /// maintain a list of partial downloads
  // - used e.g. during progressive download in THttpPeerCache
  THttpPartials = class
  protected
    /// 31-bit monotonic counter sequence to populate THttpPartial.ID
    fLastID: cardinal;
    /// how many fDownload[] are actually non void (ID <> 0)
    fUsed: cardinal;
    /// store (a few) partial download states
    fDownload: array of THttpPartial;
    procedure ReleaseSlot(p: PHttpPartial);
    procedure DoLog(const Fmt: RawUtf8; const Args: array of const);
    /// retrieve a Partial[] for a given sequence ID, hash or filename
    function FromID(aID: THttpPartialID): PHttpPartial;
    function FromHash(const Hash: THashDigest): PHttpPartial;
    function FromFile(const FileName: TFileName): PHttpPartial;
  public
    /// thread-safe access to the list of partial downloads
    // - concurrent ReadLock is used during background rfProgressiveStatic process
    // - blocking WriteLock is for Add/Associate/ChangeFile/Abort/Remove methods
    Safe: TRWLightLock;
    /// can be assigned to TSynLog.DoLog class method for low-level logging
    OnLog: TSynLogProc;
    /// return true if self is nil or fDownload is void
    function IsVoid: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// thread-safe register a new partial download and its associated HTTP request
    function Add(const Partial: TFileName; ExpectedFullSize: Int64;
      const Hash: THashDigest; Http: PHttpRequestContext = nil): THttpPartialID;
    /// search for given partial file name and size, from its hash
    function Find(const Hash: THashDigest; out Size: Int64;
      aID: PHttpPartialID = nil): TFileName;
    /// search for given partial file name from its ID, returning its file name
    // - caller should eventually run Safe.ReadUnLock
    function FindReadLocked(ID: THttpPartialID): TFileName;
    /// search for a given partial file name
    function HasFile(const FileName: TFileName): boolean;
    /// register a HTTP request to an existing partial
    function Associate(const Hash: THashDigest; Http: PHttpRequestContext): boolean;
    /// fill Dest buffer from up to MaxSize bytes from Ctxt.ProgressiveID
    function ProcessBody(var Ctxt: THttpRequestContext;
      var Dest: TRawByteStringBuffer; MaxSize: PtrInt): THttpRequestProcessBody;
    /// notify a partial file name change, when download is complete
    // - should be nested by caller within Safe.WriteLock / Safe.WriteUnLock
    function DoneLocked(const OldFile, NewFile: TFileName): boolean; overload;
    /// notify a partial file download finalization from its ID
    // - should be nested by caller within Safe.WriteLock / Safe.WriteUnLock
    function DoneLocked(ID: THttpPartialID): boolean; overload;
    /// notify a partial file download failure, e.g. on invalid hash
    // - returns the number of removed HTTP requests
    function Abort(ID: THttpPartialID): integer;
    /// unregister a HTTP request to a given partial
    // - called when the request is finished e.g. via
    // THttpServerSocketGeneric.DoProgressiveRequestFree private method
    procedure Remove(Sender: PHttpRequestContext);
  end;


type
  THttpClientSocket = class;
  IWGetAlternate = interface;

  /// available THttpClientSocketWGet.Alternate file operations
  // - by default, cached file is temporary but could be kept forever on disk
  // if waoPermanentCache is defined
  // - waoNoHeadFirst will call OnDownload() first then fallback to GET so
  // may be preferred e.g. if the main server has a huge latency
  // - waoNoMinimalSize should let OnDownload() accept even the smallest files
  // - waoTryLastPeer/waoTryAllPeers/waoBroadcastNotAlone will force homonymous
  // pcoTryLastPeer/pcoTryAllPeers/pcoBroadcastNotAlone THttpPeerCacheOption
  // - waoNoProgressiveDownloading will disable pcfResponsePartial requests
  TWGetAlternateOption = (
    waoPermanentCache,
    waoNoHeadFirst,
    waoNoMinimalSize,
    waoTryLastPeer,
    waoTryAllPeers,
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
    wgsAlternateGetNext,
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
    wgsLastMod,
    wgsAlternateRename,
    wgsAlternateFailedRename,
    wgsAlternateFailedCopyInCache);
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
    // - if OutSteps: TWGetSteps field is not enough
    // - alternative for business logic tracking: the OnProgress callback is
    // more about periodic human interaction in GUI or console
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
    /// when WGet() has been called, contains all the steps involved
    OutSteps: TWGetSteps;
    /// to optionally log all SetStep() content during process
    LogSteps: TSynLogProc;
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

  /// the flags as returned by IWGetAlternate.State
  // - gasProcessing will be set e.g. if THttpServer.CurrentProcess > 0
  // - gasPartials will be set e.g. if THttpPartials.IsVoid = false
  TWGetAlternateState = set of (
    gasProcessing,
    gasPartials);

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
    // - returns an integer OnDownloadingID > 0 sequence to be eventually
    // supplied to OnDownloaded() or OnDownloadingFailed()
    function OnDownloading(const Params: THttpClientSocketWGet;
      const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
    /// put a downloaded file into the alternative source cache
    // - e.g. THttpPeerCache will add this file to its cache, and resume any
    // pcfResponsePartial with the new file name
    // - this method is called after any file has been successfully downloaded
    // - Params.Hasher/Hash are expected to be populated
    // - can Rename(Partial, ToRename) with proper progressive support
    procedure OnDownloaded(var Params: THttpClientSocketWGet;
      const Partial, ToRename: TFileName; OnDownloadingID: THttpPartialID);
    /// notify the alternate download implementation that the data supplied
    // by OnDownload() was incorrect
    // - e.g. THttpPeerCache will delete this file from its cache
    // - mainly if the resulting hash does not match
    procedure OnDownloadFailed(const Params: THttpClientSocketWGet);
    /// notify the alternate download implementation that OnDownloading() failed
    // - e.g. THttpPeerCache will abort publishing this partial file
    procedure OnDownloadingFailed(OnDownloadingID: THttpPartialID);
    /// check if the network interface defined in Settings did actually change
    // - you may want to recreate the alternate downloading instance
    function NetworkInterfaceChanged: boolean;
    /// returns the current state of this instance
    function State: TWGetAlternateState;
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
  // - more complex schemes (like SSPI/Kerberos) could be implemented within the
  // callback - see e.g. THttpClientSocket.OnAuthorizeSspi class method
  TOnHttpClientSocketAuthorize = function(Sender: THttpClientSocket;
    var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean of object;

  /// callback used by THttpClientSocket.Request before/after every request
  // - return true to continue execution, false to abort normal process
  TOnHttpClientSocketRequest = function(Sender: THttpClientSocket;
    var Context: THttpClientRequest): boolean of object;

  /// callback used e.g. by THttpClientSocket.Request to process any custom protocol
  // - http.CommandResp contains the full URI, e.g. 'file:///C:/folder/file.txt'
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
  // - don't forget to use the Free method when you are finished, or consider
  // using IJsonClient/TJsonClient for a higher level REST client
  THttpClientSocket = class(THttpSocket)
  protected
    fExtendedOptions: THttpRequestExtendedOptions;
    fReferer: RawUtf8;
    fAccept: RawUtf8;
    fProcessName: RawUtf8;
    fRedirected: RawUtf8;
    fProxyAuthHeader: RawUtf8;
    fRequestContext: RawUtf8;
    fRangeStart, fRangeEnd: Int64;
    fAuthDigestAlgo: TDigestAlgo;
    fOnAuthorize, fOnProxyAuthorize: TOnHttpClientSocketAuthorize;
    fOnBeforeRequest: TOnHttpClientSocketRequest;
    fOnProtocolRequest: TOnHttpClientRequest;
    fOnAfterRequest: TOnHttpClientSocketRequest;
    fOnRedirect: TOnHttpClientSocketRequest;
    {$ifdef DOMAINRESTAUTH}
    fAuthorizeSspiSpn: RawUtf8;
    {$endif DOMAINRESTAUTH}
    procedure SetAuthBearer(const Value: SpiUtf8);
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
    constructor Create(aTimeOut: integer = 0); override;
    /// finalize this instance
    destructor Destroy; override;
    /// constructor to create a client connection to a given URI
    // - returns TUri.Address as parsed from aUri
    // - overriden to support custom RegisterNetClientProtocol()
    constructor OpenUri(const aUri: TUri; const aUriFull, aTunnel: RawUtf8;
      aTimeOut: cardinal; aTLSContext: PNetTlsContext); override;
    /// constructor to create a client connection to a given TUri and options
    // - will use specified options, including TLS and Auth members, just like
    // the overloaded THttpRequest.Create(TUri,PHttpRequestExtendedOptions)
    // - raise an exception on connection error
    // - as used e.g. by TSimpleHttpClient
    constructor OpenOptions(const aUri: TUri;
      var aOptions: THttpRequestExtendedOptions; const aOnLog: TSynLogProc = nil);
    /// after Create(), open or bind to a given server port
    // - overriden to support HTTP proxy without CONNECT
    procedure OpenBind(const aServer, aPort: RawUtf8; doBind: boolean;
      aTLS: boolean = false; aLayer: TNetLayer = nlTcp;
      aSock: TNetSocket = TNetSocket(-1); aReusePort: boolean = false); override;
    /// compare TUri and its options with the actual connection
    // - returns true if no new instance - i.e. Free + OpenOptions() - is needed
    // - only supports HTTP/HTTPS, not any custom RegisterNetClientProtocol()
    function SameOpenOptions(const aUri: TUri;
      const aOptions: THttpRequestExtendedOptions): boolean; virtual;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    // within each priority level (the lower aPriority first)
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024; aPriority: integer = 10): boolean;
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
    /// low-level method which could be used after Close to reset options
    procedure ResetExtendedOptions;
    /// setup web authentication using the Basic access algorithm
    procedure AuthorizeBasic(const UserName: RawUtf8; const Password: SpiUtf8);
    /// setup web authentication using the Digest access algorithm
    procedure AuthorizeDigest(const UserName: RawUtf8; const Password: SpiUtf8;
      Algo: TDigestAlgo = daMD5_Sess);
    {$ifdef DOMAINRESTAUTH}
    /// setup web authentication using Kerberos via SSPI/GSSAPI for this instance
    // - will store the user/paswword credentials, and set OnAuthorizeSspi callback
    // - if Password is '', will search for an existing Kerberos token on UserName
    // - an in-memory token will be used to authenticate the connection
    // - WARNING: on MacOS, the default system GSSAPI stack seems to create a
    // session-wide token (like kinit), not a transient token in memory - you
    // may prefer to load a proper libgssapi_krb5.dylib instead
    procedure AuthorizeSspiUser(const UserName: RawUtf8; const Password: SpiUtf8;
      const KerberosSpn: RawUtf8 = '');
    /// web authentication callback of the current logged user using Kerberos
    // - calling the Security Support Provider Interface (SSPI) API on Windows,
    // or GSSAPI on Linux (only Kerboros)
    // - match the OnAuthorize: TOnHttpClientSocketAuthorize callback signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function OnAuthorizeSspi(Sender: THttpClientSocket;
      var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
    /// proxy authentication callback of the current logged user using Kerberos
    // - calling the Security Support Provider Interface (SSPI) API on Windows,
    // or GSSAPI on Linux (only Kerboros)
    // - match the OnProxyAuthorize: TOnHttpClientSocketAuthorize signature
    // - see also ClientForceSpn() and AuthorizeSspiSpn property
    class function OnProxyAuthorizeSspi(Sender: THttpClientSocket;
      var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
    /// the Kerberos Service Principal Name, as registered in domain
    // - e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
    // - used by class procedure OnAuthorizeSspi/OnProxyAuthorizeSspi callbacks
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
      read fExtendedOptions.RedirectMax write fExtendedOptions.RedirectMax;
    /// the effective 'Location:' URI after 3xx redirection(s) of Request()
    property Redirected: RawUtf8
      read fRedirected;
    /// optional Authentication Scheme
    // - may still be wraNone if OnAuthorize has been manually set
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme;
    /// optional Authorization: Bearer header value
    property AuthBearer: SpiUtf8
      read fExtendedOptions.Auth.Token write SetAuthBearer;
    /// contain the body data retrieved from the server - from inherited Http
    property Content: RawByteString
      read Http.Content;
    /// contain the response headers retrieved from the server - from inherited Http
    property Headers: RawUtf8
      read Http.Headers;
    /// optional authorization callback
    // - is triggered by Request() on HTTP_UNAUTHORIZED (401) status
    // - as set by AuthorizeDigest() AuthorizeSspiUser()
    // - as reset by AuthorizeBasic()
    // - see e.g. THttpClientSocket.OnAuthorizeSspi class method for SSPI auth
    property OnAuthorize: TOnHttpClientSocketAuthorize
      read fOnAuthorize write fOnAuthorize;
    /// optional proxy authorization callback
    // - is triggered by Request() on HTTP_PROXYAUTHREQUIRED (407) status
    // - see e.g. THttpClientSocket.OnProxyAuthorizeSspi class method for SSPI auth
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
      read fExtendedOptions.UserAgent write fExtendedOptions.UserAgent;
    /// contain the body data length retrieved from the server
    property ContentLength: Int64
      read Http.ContentLength;
    /// contain the body type retrieved from the server
    property ContentType: RawUtf8
      read Http.ContentType;
    /// human-readable text information filled during the last HTTP request
    // - typically one or several lines of 'DoRetry ...' context and/or
    // some redirection / exception information
    property RequestContext: RawUtf8
      read fRequestContext;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // mormot.net.websock unit)
  THttpClientSocketClass = class of THttpClientSocket;

/// returns the HTTP User-Agent header value of a mORMot client including
// the Instance class name in its minified/uppercase-only translation
// - typical value is "Mozilla/5.0 (Linux x64; mORMot) HCS/3 Tests/1" for
// THttpClientSocket 2.3 from a Tests.exe application in version 1.x
// - framework branch is identified as '/3' with no build number, for security
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
function ToText(st: TWGetAlternateState; trimmed: boolean = true): RawUtf8; overload;

var
  /// global overriden value for the GetSystemProxyUri() function
  // - as used by OpenHttp/OpenHttpGet and TSimpleHttpClient
  // - can be set manually to a forced global value
  DefaultHttpClientSocketProxy: TUri;

  /// force GetProxyForUri(fromSystem=true) in GetSystemProxyUri() function
  DefaultHttpClientSocketProxyAuto: boolean;

  /// disable proxy for any IPv4 '1.2.3.4' address in GetSystemProxyUri() function
  DefaultHttpClientSocketProxyNotForIp4: boolean;


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

{$ifdef DOMAINRESTAUTH}
/// setup Kerberos tls-server-end-point channel binding on a given TLS connection
procedure KerberosChannelBinding(const Tls: INetTls; var SecContext: TSecContext;
  var Temp: THash512Rec);
{$endif DOMAINRESTAUTH}


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

{$ifdef USEHTTPREQUEST} // as set in mormot.defines.inc

type
  {$M+} // to have existing RTTI for published properties
  THttpRequest = class;
  {$M-}

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
    fKeepAlive: cardinal;
    fHttps: boolean;
    fCompressList: THttpSocketCompressList; // used by RegisterCompress method
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
    /// connect to the supplied URI, supplied as TUri
    // - overloaded constructor to accept optional THttpRequestExtendedOptions
    constructor Create(const aUri: TUri;
      aOptions: PHttpRequestExtendedOptions = nil); overload;
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
    property AuthUserName: RawUtf8
      read fExtendedOptions.Auth.UserName
      write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SpiUtf8
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
    /// how many 3xx status code redirections are allowed
    // - default is 0 - i.e. no redirection
    // - recognized by TWinHttp and TCurlHttp, but not by TWinINet
    property RedirectMax: integer
      read fExtendedOptions.RedirectMax write fExtendedOptions.RedirectMax;
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
    // - you may set 'none' to disable any Proxy, or keep '' to use the OS proxy
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

/// returns the best THttpRequest class, depending on the system it runs on
// - i.e. TWinHttp or TCurlHttp, or raise an EHttpSocket exception
// - consider using TSimpleHttpClient if you just need a simple connection
function MainHttpClass: THttpRequestClass;

/// low-level forcing of another THttpRequest class
// - could be used if we found out that the current MainHttpClass failed (which
// could easily happen with TCurlHttp if the library is missing or deprecated)
procedure ReplaceMainHttpClass(aClass: THttpRequestClass);

{$endif USEHTTPREQUEST}


{ ******************** TWinHttp TWinINet classes }

{$ifdef USEWININET}

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
    /// do not add "Accept: */*" HTTP header by default
    fNoAllAccept: boolean;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    function InternalGetInfo(Info: cardinal): RawUtf8; virtual; abstract;
    function InternalGetInfo32(Info: cardinal): cardinal; virtual; abstract;
    function InternalQueryDataAvailable: cardinal; virtual; abstract;
    function InternalReadData(var Data: RawByteString;
      Read: PtrInt; Size: cardinal): cardinal; virtual; abstract;
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
    procedure RaiseFromLastError(const ctxt: ShortString);
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
    function InternalReadData(var Data: RawByteString;
      Read: PtrInt; Size: cardinal): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinINet exception type
  EWinINet = class(EHttpSocket)
  protected
    fLastError: integer;
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
    // those internal methods will raise an EWinHttp exception on error
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
    function InternalReadData(var Data: RawByteString;
      Read: PtrInt; Size: cardinal): cardinal; override;
    procedure RaiseFromLastError(const ctxt: ShortString);
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinHttp exception type
  EWinHttp = class(ESynException);

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


const
  /// true if neither TWinHttp nor TCurlHttp are not available on this system
  // - i.e. if MainHttpClass would raise an EHttpSocket
  ONLY_CLIENT_SOCKET = {$ifdef USEHTTPREQUEST} false {$else} true {$endif};


{ ******************** IHttpClient / TSimpleHttpClient Wrappers }

type
  /// an interface to make HTTP client requests
  // - is implemented e.g. by our TSimpleHttpClient class in this unit
  // - may eventually be implemented by any class, even non-mORMot classes from
  // the RTL or Indy
  // - instances of this interface could be used to make HTTP requests on one
  // or several servers
  IHttpClient = interface
    /// simple-to-use entry point of this instance
    // - will adapt to the full Uri (e.g. 'https://synopse.info/forum') and
    // (re)open the proper connection if needed
    // - use Body and Headers properties to retrieve the HTTP body and headers
    function Request(const Uri: RawUtf8; const Method: RawUtf8 = 'GET';
      const Header: RawUtf8 = ''; const Data: RawByteString = '';
      const DataMimeType: RawUtf8 = ''; keepalive: cardinal = 10000): integer; overload;
    /// main entry point of this instance
    // - the overloaded Request() method will call TUri.From() then this method
    function Request(const Uri: TUri; const Method, Header: RawUtf8;
      const Data: RawByteString; const DataMimeType: RawUtf8;
      KeepAlive: cardinal): integer; overload;
    /// quickly check if we can connect to the corresponding server
    // - Server.Address is not used by this method
    // - return '' on success, or any raised Exception error message
    function Connected(const Server: TUri): string;
    /// finalize any existing HTTP/HTTPS connection
    // - the connection will be reestablished on the next Request()
    procedure Close;
    /// access to the raw connection options
    // - we define a pointer to the record and not directly a record property
    // to allow direct modification of any property of the record, e.g. as
    // ! client.connectOptions^.UserAgent := 'MyOwnClient 1.2';
    // ! client.connectOptions^.Tls.IgnoreTlsCertificateErrors := false;
    function Options: PHttpRequestExtendedOptions;
    /// access to the raw TLS settings, i.e. @Options^.TLS
    // - we define a pointer to the record and not directly a record property
    // to allow direct modification of any property of the record, e.g. as:
    // ! client.Tls^.IgnoreTlsCertificateErrors := false
    function Tls: PNetTlsContext;
    /// returns the HTTP body as returned by a previous call to Request()
    function Body: RawByteString;
    /// returns the HTTP status code after the last Request() call
    function Status: integer;
    /// returns an additional error message after the last Request() call
    // - is '' on success, or is typically an exception text with its message
    function LastError: string;
    /// returns the HTTP headers as returned by a previous call to Request()
    function Headers: RawUtf8;
    /// retrieve a HTTP header text value after the last Request() call
    function Header(const Name: RawUtf8; out Value: RawUtf8): boolean; overload;
    /// retrieve a HTTP header 64-bit integer value after the last Request() call
    function Header(const Name: RawUtf8; out Value: Int64): boolean; overload;
  end;

  /// abstract implementation class of IHttpClient
  THttpClientAbstract = class(TInterfacedObject, IHttpClient)
  protected
    // the options to be used for connection and authentication
    fConnectOptions: THttpRequestExtendedOptions;
    // last request values
    fUri, fHeaders: RawUtf8;
    fBody: RawByteString;
    fLastError: string;
    fStatus: integer;
  public
    /// finalize the connection
    destructor Destroy; override;
    /// abstract low-level connection method of this class, using an TUri as input
    // - should raise an Exception on issue
    procedure RawConnect(const Server: TUri); virtual; abstract;
    // IHttpClient methods, redirecting to the internal properties or methods
    function Request(const Uri: TUri; const Method, Header: RawUtf8;
      const Data: RawByteString; const DataMimeType: RawUtf8;
      KeepAlive: cardinal): integer; overload; virtual; abstract;
    function Request(const Uri: RawUtf8; const Method: RawUtf8 = 'GET';
      const Header: RawUtf8 = ''; const Data: RawByteString = '';
      const DataMimeType: RawUtf8 = ''; keepalive: cardinal = 10000): integer; overload;
    function Connected(const Server: TUri): string; virtual;
    procedure Close; virtual; abstract;
    function Options: PHttpRequestExtendedOptions; virtual; abstract;
    function Tls: PNetTlsContext;
    function Body: RawByteString;
    function Status: integer;
    function Headers: RawUtf8;
    function LastError: string;
    function Header(const Name: RawUtf8; out Value: RawUtf8): boolean; overload;
    function Header(const Name: RawUtf8; out Value: Int64): boolean; overload;
  end;

  /// simple wrapper around THttpClientSocket/THttpRequest instances
  // - this class will reuse the previous connection if possible, and select the
  // best connection class available on this platform for a given URI
  TSimpleHttpClient = class(THttpClientAbstract)
  protected
    fHttp: THttpClientSocket;
    {$ifdef USEHTTPREQUEST}
    fHttps: THttpRequest;
    fOnlyUseClientSocket: boolean;
    {$endif USEHTTPREQUEST}
  public
    /// initialize the instance
    // - aOnlyUseClientSocket=true will use THttpClientSocket even for HTTPS
    constructor Create(aOnlyUseClientSocket: boolean = ONLY_CLIENT_SOCKET); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// low-level entry point of this instance, using an TUri as input
    // - rather use the Request() more usable method
    function Request(const Uri: TUri; const Method, Header: RawUtf8;
      const Data: RawByteString; const DataMimeType: RawUtf8;
      KeepAlive: cardinal): integer; override;
    /// low-level connection point of this instance, using an TUri as input
    // - rather use the Connected() more usable method
    procedure RawConnect(const Server: TUri); override;
    // IHttpClient methods
    procedure Close; override;
    function Options: PHttpRequestExtendedOptions; override;
  end;


{ ******************** TJsonClient JSON requests over HTTP }

type
  /// transient structure used to store a raw IJsonClient response
  // - using such a structure allow the response to be thread-safe processed
  {$ifdef USERECORDWITHMETHODS}
  TJsonResponse = record
  {$else}
  TJsonResponse = object
  {$endif USERECORDWITHMETHODS}
    /// the raw HTTP response status code
    Status: integer;
    /// the queried HTTP verb, e.g. 'GET' or 'POST'
    Method: RawUtf8;
    /// the queried URL, e.g. '/api/hypervisors'
    // - i.e. without its server part, and excluding any ?parameters=...
    Url: RawUtf8;
    /// the raw HTTP response headers (if any)
    Headers: RawUtf8;
    /// the raw HTTP response body
    Content: RawByteString;
    /// reset the content of this response structure
    procedure Init;
    /// set the content of this response structure from previous aClient.Request
    procedure InitFrom(const aMethod, aUrl: RawUtf8; const aClient: IHttpClient);
    /// retrieve a HTTP header text value
    function Header(const Name: RawUtf8; out Value: RawUtf8): boolean; overload;
    /// retrieve a HTTP header 64-bit integer value
    function Header(const Name: RawUtf8; out Value: Int64): boolean; overload;
  end;
  PJsonResponse = ^TJsonResponse;

  /// exception class raised by TJsonClient instances
  EJsonClient = class(EHttpSocket)
  protected
    fResponse: TJsonResponse;
  public
    /// dedicated constructor, setting the associated TJsonResponse
    constructor CreateResp(const Format: RawUtf8; const Args: array of const;
      const Resp: TJsonResponse); virtual;
    /// low-level access to the raw response context
    property Response: TJsonResponse
      read fResponse;
  end;
  /// meta-class of the exceptions raised by TJsonClient instances
  EJsonClientClass = class of EJsonClient;

  IJsonClient = interface;

  /// customize how IJsonClient handle its process, e.g. its parsing
  TJsonClientOptions = set of (
    jcoLogFullRequest,
    jcoResultNoClear,
    jcoHttpExceptionIntercept,
    jcoHttpErrorRaise,
    jcoPayloadWithoutVoid,
    jcoPayloadDateTimeWithZ,
    jcoParseTolerant,
    jcoParseErrorClear,
    jcoParseErrorRaise);

  /// event signature for error callbacks during IJsonClient.Request()
  TOnJsonClientError = procedure(const Sender: IJsonClient;
    const Response: TJsonResponse; const ErrorMsg: ShortString) of object;

  /// event signature for a callback run before each IJsonClient.Request()
  TOnJsonClientBefore = procedure(const Sender: IJsonClient;
    const Method, Action, Body: RawUtf8) of object;

  /// event signature for a callback run after each IJsonClient.Request()
  TOnJsonClientAfter = procedure(const Sender: IJsonClient;
    const Response: TJsonResponse) of object;

  /// an interface to make thread-safe JSON client requests
  // - is implemented e.g. by our TJsonClient class in this unit
  // - don't know anything about the HTTP connection itself but it is connected,
  // so could be actually not using HTTP at all (even run in-process)
  // - can use RTTI for automated input/output JSON serialization of records
  // or dynamic arrays - Request() methods are just wrapper to RttiRequest()
  // - errors are handled as TOnJsonClientError callbacks, either for each
  // Request(), or globally via the OnError property
  // - all those methods are thread-safe, protected by an OS mutex/lock
  IJsonClient = interface
    // some property helpers
    function GetOnError: TOnJsonClientError;
    procedure SetOnError(const Event: TOnJsonClientError);
    function GetOnBefore: TOnJsonClientBefore;
    procedure SetOnBefore(const Event: TOnJsonClientBefore);
    function GetOnAfter: TOnJsonClientAfter;
    procedure SetOnAfter(const Event: TOnJsonClientAfter);
    function GetOptions: TJsonClientOptions;
    procedure SetOptions(Value: TJsonClientOptions);
    function GetUrlEncoder: TUrlEncoder;
    procedure SetUrlEncoder(Value: TUrlEncoder);
    function GetCookies: RawUtf8;
    procedure SetCookies(const Value: RawUtf8);
    function GetDefaultHeaders: RawUtf8;
    procedure SetDefaultHeaders(const Value: RawUtf8);
    /// raw access to the HTTP client itself
    // - Http.Options^ could be used e.g. to tune the connection (proxy or TLS),
    // or to change the authentication method
    // - direct access to this interface methods is not thread-safe
    function Http: IHttpClient;
    /// check if the client is actually connected to the server
    // - return '' on success, or a text error (typically an Exception.Message)
    function Connected: string;
    /// set Http.Options^.Auth.Token/Scheme with a given wraBearer token
    // - will disable authentication if Token = ''
    procedure SetBearer(const Token: SpiUtf8);
    /// can specify an additional default header to the HTTP request
    // - could be used e.g. as
    // ! Client.AddDefaultHeader('Authorization', 'Wawi '+ GetApiKey);
    procedure AddDefaultHeader(const Name, Value: RawUtf8);
    /// Request execution, with no JSON parsing using RTTI
    procedure Request(const Method, Action: RawUtf8;
      const CustomError: TOnJsonClientError = nil); overload;
    /// Request execution, with output only JSON parsing using RTTI
    procedure Request(const Method, Action: RawUtf8;
      var Res; ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    /// parameterized Request execution, with no JSON parsing using RTTI
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      const CustomError: TOnJsonClientError = nil); overload;
    /// parameterized Request execution, with output only JSON parsing using RTTI
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      var Res; ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    /// parameterized Request execution, with input/output JSON parsing using RTTI
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      const Payload; var Res; PayloadInfo, ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    /// main Request execution, with optional input/output JSON parsing using RTTI
    // - if Payload/PayloadInfo or Res/ResInfo is nil, corresponding input
    // Payload or output Res variable will be ignored
    // - will optionally call CustomError, to thread-safely handle errors
    // dedicated to this actual request - with fallback to OnError global property
    procedure RttiRequest(const Method, Action, Headers: RawUtf8;
      Payload, Res: pointer; PayloadInfo, ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError);
    /// low-level HTTP request execution
    // - won't raise any exception on HTTP error, nor do any parsing - so could
    // be used manually if you need to send a raw request to the server
    // - is called by RttiRequest(), and implemented e.g. in TJsonClient
    procedure RawRequest(const Method, Action, InType, InBody, InHeaders: RawUtf8;
      var Response: TJsonResponse);
    /// can specify a cookie value to the HTTP request
    // - is void by default
    // - specify a fully constructed 'cookiename: cookievalue; path=...' content
    property Cookies: RawUtf8
      read GetCookies write SetCookies;
    /// can specify a default header to the HTTP request
    // - contains 'Accept: application/json' by default
    property DefaultHeaders: RawUtf8
      read GetDefaultHeaders write SetDefaultHeaders;
    /// event optionally called before any HTTP request
    property OnBefore: TOnJsonClientBefore
      read GetOnBefore write SetOnBefore;
    /// event optionally called after any HTTP request
    // - is called even in case of error, i.e. just before OnError
    property OnAfter: TOnJsonClientAfter
      read GetOnAfter write SetOnAfter;
    /// allow to customize globally any HTTP error
    // - used if CustomError parameter is not set in Request() overloads
    // - if no event is set, TJsonResponse.RaiseForStatus will be called
    property OnError: TOnJsonClientError
      read GetOnError write SetOnError;
    /// allow to customize the process
    // - by default, contains [jcoParseTolerant, jcoHttpErrorRaise]
    property Options: TJsonClientOptions
      read GetOptions write SetOptions;
    /// allow to customize the URL encoding of parameters
    // - by default, contains [ueEncodeNames, ueSkipVoidString]
    property UrlEncoder: TUrlEncoder
      read GetUrlEncoder write SetUrlEncoder;
  end;

  /// abstract thread-safe generic JSON client class, implementing IJsonClient
  // - will implement all JSON and RTTI featured methods, without any actual
  // HTTP connection, which is abstracted to Connected and RawRequest() methods
  TJsonClientAbstract = class(TInterfacedObjectLocked, IJsonClient)
  protected
    fUrlEncoder: TUrlEncoder;
    fOptions: TJsonClientOptions;
    fOnError: TOnJsonClientError;
    fOnBefore: TOnJsonClientBefore;
    fOnAfter: TOnJsonClientAfter;
    fOnLog: TSynLogProc;
    function CheckRequestError(const Response: TJsonResponse;
      const CustomError: TOnJsonClientError): boolean;
  public
    // IJsonClient methods
    function GetOnError: TOnJsonClientError;
    procedure SetOnError(const Event: TOnJsonClientError); virtual;
    function GetOnBefore: TOnJsonClientBefore;
    procedure SetOnBefore(const Event: TOnJsonClientBefore); virtual;
    function GetOnAfter: TOnJsonClientAfter;
    procedure SetOnAfter(const Event: TOnJsonClientAfter); virtual;
    function GetOptions: TJsonClientOptions;
    procedure SetOptions(Value: TJsonClientOptions);
    function GetUrlEncoder: TUrlEncoder;
    procedure SetUrlEncoder(Value: TUrlEncoder);
    function GetCookies: RawUtf8; virtual; abstract;
    procedure SetCookies(const Value: RawUtf8); virtual; abstract;
    function GetDefaultHeaders: RawUtf8; virtual; abstract;
    procedure SetDefaultHeaders(const Value: RawUtf8); virtual; abstract;
    function Http: IHttpClient; virtual; abstract;
    procedure SetBearer(const Token: SpiUtf8); virtual;
    procedure AddDefaultHeader(const Name, Value: RawUtf8); virtual; abstract;
    function Connected: string; virtual; abstract;
    procedure RawRequest(const Method, Action, InType, InBody, InHeaders: RawUtf8;
      var Response: TJsonResponse); virtual; abstract;
    procedure RttiRequest(const Method, Action, Headers: RawUtf8;
      Payload, Res: pointer; PayloadInfo, ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError); overload;
    procedure Request(const Method, Action: RawUtf8;
      const CustomError: TOnJsonClientError = nil); overload;
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      const CustomError: TOnJsonClientError = nil); overload;
    procedure Request(const Method, Action: RawUtf8;
      var Res; ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      var Res; ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    procedure Request(const Method, ActionFmt: RawUtf8;
      const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
      const Payload; var Res; PayloadInfo, ResInfo: PRttiInfo;
      const CustomError: TOnJsonClientError = nil); overload;
    /// event optionally called before any HTTP request
    property OnBefore: TOnJsonClientBefore
      read GetOnBefore write SetOnBefore;
    /// event optionally called after any HTTP request
    property OnAfter: TOnJsonClientAfter
      read GetOnAfter write SetOnAfter;
    /// allow to globally customize any HTTP error
    // - used if CustomError parameter is not set in Request() overloads
    // - if no event is set, TJsonResponse.RaiseForStatus will be called
    property OnError: TOnJsonClientError
      read GetOnError write SetOnError;
    /// allow to customize the process
    // - by default, contains [jcoParseTolerant, jcoHttpErrorRaise]
    property Options: TJsonClientOptions
      read GetOptions write SetOptions;
    /// allow to customize the URL encoding of parameters
    // - by default, contains [ueEncodeNames, ueSkipVoidString]
    property UrlEncoder: TUrlEncoder
      read GetUrlEncoder write SetUrlEncoder;
    /// optional log of the client side process
    // - could be assigned to the TSynLog.DoLog class method
    property OnLog: TSynLogProc
      read fOnLog write fOnLog;
  end;

  /// thread-safe generic JSON client class over HTTP, implementing IJsonClient
  TJsonClient = class(TJsonClientAbstract)
  protected
    fHttp: IHttpClient;
    fServerUri: TUri;
    fBaseUri, fDefaultHeaders, fCookies, fInHeaders: RawUtf8;
    fKeepAlive: integer;
    procedure SetInHeaders;
  public
    /// initialize the instance, over a HTTP server
    // - you can then set the needed HttpOptions^, then call the Connected method
    // to check if it is actually connected
    constructor Create(const aServerAddress: RawUtf8; const aBaseUri: RawUtf8 = '';
      aKeepAlive: integer = 5000); reintroduce; virtual;
    /// finalize the instance, and its associated lock
    destructor Destroy; override;
    /// raw access to the HTTP options for the connection, e.g. TLS or Auth
    function HttpOptions: PHttpRequestExtendedOptions;
    // IJsonClient methods
    function GetCookies: RawUtf8; override;
    procedure SetCookies(const Value: RawUtf8); override;
    function GetDefaultHeaders: RawUtf8; override;
    procedure SetDefaultHeaders(const Value: RawUtf8); override;
    procedure AddDefaultHeader(const Name, Value: RawUtf8); override;
    function Http: IHttpClient; override;
    function Connected: string; override;
    procedure RawRequest(const Method, Action, InType, InBody, InHeaders: RawUtf8;
      var Response: TJsonResponse); override;
    /// raw access to the base URI text prepended to each request
    property BaseUri: RawUtf8
      read fBaseUri write fBaseUri;
  end;

/// a simple wrapper to FindRawUtf8() which converts -1 into 0
// - so could be used for the enums as generated by mormot.net.openapi,
// which have the first item always being ##None as '', e.g. defined as
// ! TEnum1 = (e1None, e1, e2);
// ! const ENUM1_TXT: array[TEnum1] of RawUtf8 = ('', 'one', 'and 2');
function FindCustomEnum(const CustomText: array of RawUtf8;
  const Value: RawUtf8): integer;


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
    fOnlyUseClientSocket: boolean;
    fCache: TSynDictionary;
  public
    /// initialize the cache for a given server
    // - will call LoadFromUri(aUri, aToken) if aUri <> ''
    // - aKeepAliveSeconds = 0 will force "Connection: Close" HTTP/1.0 requests
    // - an internal cache will be maintained, and entries will be flushed after
    // aTimeoutSeconds - i.e. 15 minutes per default - setting 0 will disable
    // the client-side cache content
    constructor Create(const aUri: RawUtf8; aKeepAliveSeconds: integer = 30;
      aTimeoutSeconds: integer = 15 * 60; const aToken: RawUtf8 = '';
      aOnlyUseClientSocket: boolean = ONLY_CLIENT_SOCKET); reintroduce;
    /// finalize the current connnection and flush its in-memory cache
    // - you may use LoadFromUri() to connect to a new server
    procedure Clear;
    /// specify a new server and an optional authentication token
    // - won't actually connect until Get() is called
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
    // - you can change e.g. Client.Options^ or Client.Tls^ members before Get()
    // - warning: don't assign this instance to a IHttpClient because it is NOT
    // reference counted, and is local to this THttpRequestCached
    property Client: TSimpleHttpClient
      read fClient;
  end;


/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHttp/TCurlHttp for any https URI, or if forceNotSocket is set to true
// - see also OpenHttpGet() for direct THttpClientSock call
// - try INetClientProtocol if aUri does not start with http:// or https://
function HttpGet(const aUri: RawUtf8; outHeaders: PRawUtf8 = nil;
  forceNotSocket: boolean = false; outStatus: PInteger = nil;
  timeout: integer = 0; forceSocket: boolean = false;
  ignoreTlsCertError: boolean = false): RawByteString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHttp/TCurlHttp for any https URI
// - try INetClientProtocol if aUri does not start with http:// or https://
function HttpGet(const aUri: RawUtf8; const inHeaders: RawUtf8;
  outHeaders: PRawUtf8 = nil; forceNotSocket: boolean = false;
  outStatus: PInteger = nil; timeout: integer = 0; forceSocket: boolean = false;
  ignoreTlsCertError: boolean = false): RawByteString; overload;

/// retrieve the content of a web page, with ignoreTlsCertError=true for https
// - typically used to retrieve reference material online for testing
// - can optionally use a local file as convenient offline cache
// - try INetClientProtocol if aUri does not start with http:// or https://
function HttpGetWeak(const aUri: RawUtf8; const aLocalFile: TFileName = '';
  outStatus: PInteger = nil): RawByteString;


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
  const TextCharSet: RawUtf8  =  'ISO-8859-1'; TLS: boolean = false;
  TLSIgnoreCertError: boolean = false): boolean; overload;

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
  TLS: boolean = false; TLSIgnoreCertError: boolean = false): boolean; overload;

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
    result^.ContentType := GetMimeContentType(content, Ansi7ToString(filename));
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
    Join(['multipart/form-data; boundary=', fBound], fMultipartContentType);
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
  StringToUtf8(ExtractFileName(filename), fn);
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
    mormot.core.text.Append(s, ['--', fBounds[i], '--'#13#10]);
  Append(s);
  inherited Flush; // compute fSize
end;


{ ******************** Additional Client Protocols Support }

procedure RegisterNetClientProtocol(
  const Name: RawUtf8; const OnRequest: TOnHttpClientRequest);
var
  m: TMethod;
begin
  if NetClientProtocols.FindAndExtract(Name, m) then // remove any existing
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
  fn := GetFileNameFromUrl(http.CommandResp);
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
  FormatUtf8('Mozilla/5.0 (' + OS_TEXT + ' ' + CPU_ARCH_TEXT + '; mORMot) %/' +
    SYNOPSE_FRAMEWORK_BRANCH + ' %%',
    [name, Executable.ProgramName, vers], result);
end;


{ THttpPartials }

function THttpPartials.FromID(aID: THttpPartialID): PHttpPartial;
var
  i: PtrInt;
begin
  result := pointer(fDownload);
  if cardinal(aID) <= fLastID then
    for i := 1 to length(fDownload) do
      if result^.ID = aID then // fast enough with a few slots
        exit
      else
        inc(result);
  result := nil;
end;

function THttpPartials.FromHash(const Hash: THashDigest): PHttpPartial;
var
  i: PtrInt;
begin
  result := pointer(fDownload);
  for i := 1 to length(fDownload) do
    if (result^.ID <> 0) and // not a recycled slot
       HashDigestEqual(result^.Digest, Hash) then
      exit
    else
      inc(result);
  result := nil;
end;

function THttpPartials.FromFile(const FileName: TFileName): PHttpPartial;
var
  i: PtrInt;
begin
  result := pointer(fDownload);
  for i := 1 to length(fDownload) do
    if (result^.ID <> 0) and // not a recycled slot
       (result^.PartFile = FileName) then
      exit
    else
      inc(result);
  result := nil;
end;

function THttpPartials.IsVoid: boolean;
begin
  result := (self = nil) or
            (fUsed = 0);
end;

procedure THttpPartials.DoLog(const Fmt: RawUtf8; const Args: array of const);
var
  txt: ShortString;
begin
  if not Assigned(OnLog) then
    exit;
  FormatShort(Fmt, Args, txt);
  OnLog(sllTrace, '% used=%/%', [txt, fUsed, length(fDownload)], self);
end;

function THttpPartials.Add(const Partial: TFileName; ExpectedFullSize: Int64;
  const Hash: THashDigest; Http: PHttpRequestContext): THttpPartialID;
var
  n: PtrInt;
  p: PHttpPartial;
begin
  result := 0; // unsupported
  if (self = nil) or
     (ExpectedFullSize = 0) then
    exit;
  Safe.WriteLock;
  try
    inc(fLastID);
    inc(fUsed);
    result := fLastID; // returns 1,2,3... THttpPartialID (process specific)
    p := FromID(0); // try to reuse an empty slot
    if p = nil then
    begin
      n := length(fDownload);
      SetLength(fDownload, n + 1); // need a new slot
      p := @fDownload[n];
    end;
    p^.ID := result;
    p^.Digest := Hash;
    p^.FullSize := ExpectedFullSize;
    p^.PartFile := Partial;
    p^.HttpContext := nil;
    if Http <> nil then
    begin
      PtrArrayAdd(p^.HttpContext, Http);
      Http^.ProgressiveID := p^.ID;
    end;
  finally
    Safe.WriteUnLock;
  end;
  DoLog('Add(%,%)=%', [Partial, ExpectedFullSize, result]);
end;

function THttpPartials.Find(const Hash: THashDigest; out Size: Int64;
  aID: PHttpPartialID): TFileName;
var
  p: PHttpPartial;
begin
  Size := 0;
  result := '';
  if aID <> nil then
    aID^ := 0;
  if IsVoid then
    exit;
  Safe.ReadLock;
  try
    p := FromHash(Hash);
    if p = nil then
      exit;
    Size := p^.FullSize;
    result := p^.PartFile;
    if aID <> nil then
      aID^ := p^.ID;
  finally
    Safe.ReadUnLock;
  end;
end;

function THttpPartials.FindReadLocked(ID: THttpPartialID): TFileName;
var
  p: PHttpPartial;
begin
  result := '';
  if IsVoid then
    exit;
  Safe.ReadLock;
  try
    p := FromID(ID);
    if p <> nil then
      result := p^.PartFile;
  finally
    if result = '' then
      Safe.ReadUnLock; // keep ReadLock if a file name was found
  end;
end;

function THttpPartials.HasFile(const FileName: TFileName): boolean;
begin
  result := false;
  if IsVoid or
     (FileName = '') then
    exit;
  Safe.ReadLock;
  try
    result := FromFile(FileName) <> nil;
  finally
    Safe.ReadUnLock; // keep ReadLock if a file name was found
  end;
end;

function THttpPartials.Associate(const Hash: THashDigest; Http: PHttpRequestContext): boolean;
var
  p: PHttpPartial;
begin
  result := false;
  if IsVoid or
     (Http = nil) then
    exit;
  Safe.WriteLock;
  try
    p := FromHash(Hash);
    if p = nil then
      exit;
    PtrArrayAdd(p^.HttpContext, Http);
    Http^.ProgressiveID := p^.ID;
    result := true;
  finally
    Safe.WriteUnLock;
  end;
end;

function THttpPartials.ProcessBody(var Ctxt: THttpRequestContext;
  var Dest: TRawByteStringBuffer; MaxSize: PtrInt): THttpRequestProcessBody;
var
  tix: cardinal;
  fn: TFileName;
  src: THandle;
begin
  result := hrpAbort;
  if IsVoid or
     (Ctxt.ProgressiveID = 0) or // e.g. after Abort()
     not (rfProgressiveStatic in Ctxt.ResponseFlags) then
    exit;
  // prepare to wait for the data to be available
  tix := GetTickSec;
  if Ctxt.ProgressiveTix = 0 then
    Ctxt.ProgressiveTix := tix + STATICFILE_PROGTIMEOUTSEC; // first seen
  // retrieve the file name to be processed
  fn := FindReadLocked(Ctxt.ProgressiveID);
  if fn = '' then // e.g. after THttpPartials.DoneLocked()
    exit;
  // process this file within the read lock
  try
    src := FileOpen(fn, fmOpenReadShared); // partial file access
    if ValidHandle(src) then
    try
      // fill up to MaxSize bytes of src file into Dest buffer
      result := Ctxt.ProcessBody(src, Dest, MaxSize);
      case result of
        hrpSend:
          Ctxt.ProgressiveTix := tix + STATICFILE_PROGTIMEOUTSEC; // reset
        hrpWait:
          if tix > Ctxt.ProgressiveTix then
          begin
            if Assigned(OnLog) then
              OnLog(sllWarning, 'ProcessBody: ProgressiveID=% timeout % at %/%',
                [Ctxt.ProgressiveID, fn, FileSize(src), Ctxt.ContentLength], self);
            result := hrpAbort; // never wait forever: abort after 10 seconds
          end;
      else // hrpAbort (hrpDone in THttpServerSocketGeneric.DoProcessBody)
        if Assigned(OnLog) then
          OnLog(sllTrace, 'ProcessBody=% id=% fn=%',
            [ToText(result)^, Ctxt.ProgressiveID], self);
      end;
    finally
      FileClose(src); // the lock protects the file itself
    end
    else if Assigned(OnLog) then
      OnLog(sllLastError, 'ProcessBody: ProgressiveID=% FileOpen % failed',
        [Ctxt.ProgressiveID, fn], self);
  finally
    Safe.ReadUnLock;
  end;
end;

procedure THttpPartials.ReleaseSlot(p: PHttpPartial);
begin
  p^.ID := 0; // reuse this slot at next Add()
  p^.PartFile := '';
  p^.HttpContext := nil;
  dec(fUsed);
  if (fUsed = 0) and
     (length(fDownload) > 16) then
    fDownload := nil; // worth releasing the memory
end;

function THttpPartials.DoneLocked(const OldFile, NewFile: TFileName): boolean;
var
  p: PHttpPartial;
begin
  result := false;
  if IsVoid or
     (OldFile = '') then
    exit;
  p := FromFile(OldFile);
  if p <> nil then
  begin
    result := true;
    if p^.HttpContext = nil then
      ReleaseSlot(p)
    else
      p^.PartFile := NewFile; // notify any pending background process
  end;
  DoLog('Done(%,%)=%', [OldFile, NewFile, result]);
end;

function THttpPartials.DoneLocked(ID: THttpPartialID): boolean;
var
  p: PHttpPartial;
begin
  result := false;
  if IsVoid or
     (ID = 0) or
     (cardinal(ID) > fLastID) then
    exit;
  p := FromID(ID);
  if p <> nil then
  begin
    result := true;
    if p^.HttpContext = nil then
      ReleaseSlot(p); // associated to no background download
    // keep p^.PartFile which may still be available
  end;
  DoLog('Done(%)=%', [ID, result]);
end;

function THttpPartials.Abort(ID: THttpPartialID): integer;
var
  i: PtrInt;
  p: PHttpPartial;
begin
  // called on aborted partial retrieval
  result := 0; // returns the number of changed entries
  if IsVoid or
     (ID = 0) or
     (cardinal(ID) > fLastID) then
    exit;
  Safe.WriteLock;
  try
    p := FromID(ID);
    if p <> nil then
    begin
      if p^.HttpContext <> nil then
      begin
        for i := 0 to length(p^.HttpContext) - 1 do
          try
            p^.HttpContext[i].ProgressiveID := 0; // abort THttpServer.Process
            inc(result);
          except
            on E: Exception do // paranoid
              if Assigned(OnLog) then
                OnLog(sllWarning, 'Abort: HttpContext[%] raised %', [i, E], self);
          end;
        p^.HttpContext := nil;
      end;
      ReleaseSlot(p);
    end;
  finally
    Safe.WriteUnLock;
  end;
  DoLog('Abort(%)=%', [ID, result]);
end;

procedure THttpPartials.Remove(Sender: PHttpRequestContext);
var
  p: PHttpPartial;
begin
  // nominal case, when the partial retrieval has eventually successed
  if IsVoid or
     (Sender = nil) or
     (Sender.ProgressiveID = 0) then
    exit;
  Safe.WriteLock;
  try
    p := FromID(Sender.ProgressiveID);
    if p <> nil then
    begin
      PtrArrayDelete(p^.HttpContext, Sender);
      if p^.HttpContext = nil then
        ReleaseSlot(p);
    end;
  finally
    Safe.WriteUnLock;
  end;
  DoLog('Remove(%)=%', [Sender.ProgressiveID, (p <> nil)]);
end;


{ THttpClientSocketWGet }

procedure THttpClientSocketWGet.Clear;
begin
  RecordZero(@self, TypeInfo(THttpClientSocketWGet));
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
var
  txt: RawUtf8;
begin
  include(OutSteps, Step);
  if Assigned(OnStep) or
     Assigned(LogSteps) then
  begin
    txt := Make(Context);
    if Assigned(OnStep) then
      OnStep(Step, txt);
    if Assigned(LogSteps) then
      LogSteps(sllCustom1, 'WGet %: %', [ToText(Step)^, txt]);
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

function ToText(st: TWGetAlternateState; trimmed: boolean): RawUtf8;
begin
  result := GetSetName(TypeInfo(TWGetAlternateState), st, trimmed);
end;

var
  _PROXYSETFROMENV: boolean; // retrieve environment variables only once
  _PROXYSAFE: TLightLock;
  _PROXY: array[{https:}boolean] of RawUtf8;

function GetProxyForUri(const uri: RawUtf8; fromSystem: boolean): RawUtf8;
{$ifdef USEWININET}
var
  pi: TProxyInfo;
{$endif USEWININET}
begin
  if not _PROXYSETFROMENV then
  begin
    _PROXYSAFE.Lock;
    StringToUtf8(GetEnvironmentVariable('HTTP_PROXY'),  _PROXY[false]);
    StringToUtf8(GetEnvironmentVariable('HTTPS_PROXY'), _PROXY[true]);
    if _PROXY[true] = '' then
      _PROXY[true] := _PROXY[false];
    _PROXYSETFROMENV := true;
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
  if IsNone(proxy) or
     (not temp.From(uri)) or
     (temp.Address = '') or
     (not (temp.UriScheme in [usHttp, usHttps])) or
     IsLocalHost(pointer(temp.Address)) or // no proxy for "127.x.x.x"
     (DefaultHttpClientSocketProxyNotForIp4 and
      NetIsIP4(pointer(temp.Address))) then  // plain "1.2.3.4" IP has no proxy
    result := nil
  else if (proxy <> '') and
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
    result := OnlyChar(result, ['0'..'9', 'A'..'Z', 'a'..'z', '_', '.', '-']);
end;


{ THttpClientSocket }

constructor THttpClientSocket.Create(aTimeOut: integer);
begin
  if aTimeOut = 0 then
    aTimeOut := HTTP_DEFAULT_RECEIVETIMEOUT;
  inherited Create(aTimeOut);
  if fExtendedOptions.UserAgent = '' then
    fExtendedOptions.UserAgent := DefaultUserAgent(self);
  fAccept := '*/*';
end;

destructor THttpClientSocket.Destroy;
begin
  fExtendedOptions.Clear;
  inherited Destroy;
end;

constructor THttpClientSocket.OpenUri(const aUri: TUri; const aUriFull,
  aTunnel: RawUtf8; aTimeOut: cardinal; aTLSContext: PNetTlsContext);
begin
  if not (aUri.UriScheme in [usHttp .. usUdp]) and
     NetClientProtocols.FindAndCopy(aUri.Scheme, fOnProtocolRequest) then
  begin
    Create(aTimeOut); // no socket involved - but keep Request() logic
    fOpenUriFull := aUriFull;  // e.g. to call PatchCreateFromUrl() WinAPI
  end
  else
    inherited OpenUri(aUri, aUriFull, aTunnel, aTimeOut, aTLSContext);
end;

constructor THttpClientSocket.OpenOptions(const aUri: TUri;
  var aOptions: THttpRequestExtendedOptions; const aOnLog: TSynLogProc);
var
  temp: TUri;
  pu: PUri;
begin
  // setup the proper options before any connection
  fExtendedOptions := aOptions;
  Create(fExtendedOptions.CreateTimeoutMS);
  if Assigned(aOnLog) then
    OnLog := aOnLog; // allow to debug ASAP
  case fExtendedOptions.Auth.Scheme of
    wraDigest:
      begin
        fOnAuthorize := OnAuthorizeDigest; // as AuthorizeDigest()
        fAuthDigestAlgo := daMD5_Sess;
      end;
    wraNegotiate,
    wraNegotiateChannelBinding:
      {$ifdef DOMAINRESTAUTH}
      fOnAuthorize := OnAuthorizeSspi;     // as AuthorizeSspiUser()
      {$else}
      EHttpSocket.RaiseUtf8('%.Open: unsupported wraNegotiate', [self]);
      {$endif DOMAINRESTAUTH}
  end;
  TLS := fExtendedOptions.TLS;
  pu := GetSystemProxyUri(aUri.URI, fExtendedOptions.Proxy, temp);
  if pu <> nil then
    Tunnel := pu^;
  // actually connect to the server (inlined TCrtSock.Open)
  OpenBind(aUri.Server, aUri.Port, {bind=}false, aUri.Https, aUri.Layer);
  aOptions.TLS := TLS; // copy back Peer information after connection
end;

procedure THttpClientSocket.OpenBind(const aServer, aPort: RawUtf8; doBind,
  aTLS: boolean; aLayer: TNetLayer; aSock: TNetSocket; aReusePort: boolean);
var
  bak: TUri;
begin
  if doBind then
    EHttpSocket.RaiseUtf8('%.OpenBind with doBind=true', [self]);
  fProxyAuthHeader := '';
  if (not aTLS) and // proxy to https:// destination requires CONNECT
     (Tunnel.Server <> '') and
     (Tunnel.Server <> aServer) then
  begin
    // plain http:// proxy is implemented in RequestSendHeader not via CONNECT
    bak := Tunnel;
    try
      Tunnel.Clear; // no CONNECT
      inherited OpenBind(bak.Server, bak.Port, false, bak.Https, bak.Layer);
      fProxyUrl := bak.URI;
      if bak.User <> '' then
        Join(['Proxy-Authorization: Basic ', bak.UserPasswordBase64], fProxyAuthHeader);
      fSocketLayer := aLayer;
      include(fFlags, fProxyHttp);
      if Assigned(OnLog) then
        OnLog(sllTrace, 'Open(%:%) via proxy %', [aServer, aPort, fProxyUrl], self);
    finally
      // always restore server and tunnel params for proper retry
      fServer := aServer;
      fPort := aPort; // good enough to keep '' for default port 80
      exclude(fFlags, fServerTlsEnabled); // any TLS was about the proxy
      Tunnel := bak;
    end;
  end
  else
    // regular socket creation if no proxy or toward https://
    inherited OpenBind(aServer, aPort, {doBind=}false, aTLS, aLayer);
end;

function THttpClientSocket.SameOpenOptions(const aUri: TUri;
  const aOptions: THttpRequestExtendedOptions): boolean;
var
  tun: TUri;
begin
  result := (aUri.UriScheme in HTTP_SCHEME) and
            aUri.Same(Server, Port, ServerTls) and
            SameNetTlsContext(TLS, aOptions.TLS) and
            fExtendedOptions.SameAuth(@aOptions.Auth);
  if result then
    if tun.From(aOptions.Proxy) then
      result := tun.Same(Tunnel.Server, Tunnel.Port, Tunnel.Https)
    else
      result := (Tunnel.Server = '');
end;

function THttpClientSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize, aPriority: integer): boolean;
begin
  result := fCompressList.RegisterFunc(aFunction, aCompressMinSize, aPriority) <> nil;
  if (fCompressList.Algo <> nil) and
     (Http.CompressList = nil) then
    Http.CompressList := @fCompressList; // enable compression for the requests
end;

procedure THttpClientSocket.RequestInternal(var ctxt: THttpClientRequest);

  procedure DoRetry(const Fmt: RawUtf8; const Args: array of const;
    FatalErrorCode: integer = HTTP_CLIENTERROR);
  var
    msg: ShortString;
  begin
    FormatShort(Fmt, Args, msg);
    AppendLine(fRequestContext, ['DoRetry ',  msg]);
    //writeln('DoRetry ',byte(ctxt.Retry), ' ', FatalErrorCode, ' / ', msg);
    if Assigned(OnLog) then
       OnLog(sllTrace, 'DoRetry % socket=% fatal=% retry=%',
         [msg, fSock.Socket, FatalErrorCode, BOOL_STR[rMain in ctxt.Retry]], self);
    if Aborted then
      ctxt.Status := HTTP_CLIENTERROR
    else if rMain in ctxt.Retry then
      // we should retry once -> return error only if failed twice
      ctxt.Status := FatalErrorCode
    else
      try
        // recreate the connection and try again - like TCrtSocket.ReOpen()
        Close;
        OpenBind(fServer, fPort, {bind=}false, ServerTls);
        HttpStateReset;
        include(ctxt.Retry, rMain);
        RequestInternal(ctxt); // retry once
      except
        on E: Exception do
        begin
          AppendLine(fRequestContext, [E, ':', E.Message]);
          ctxt.Status := FatalErrorCode;
        end;
      end;
  end;

var
  cmd: PUtf8Char;
  pending: TCrtSocketPending;
  res: TNetResult;
  bodystream: TStream;
  loerr, buflen: integer;
  dat: RawByteString;
  start: Int64;
begin
  if Assigned(OnLog) then
  begin
    QueryPerformanceMicroSeconds(start);
    OnLog(sllTrace, 'RequestInternal % %:%/% flags=% retry=%', [ctxt.Method,
      fServer, fPort, ctxt.Url, ToText(Http.HeaderFlags), byte(ctxt.Retry)], self);
  end;
  Http.Content := '';
  if Aborted then
    ctxt.Status := HTTP_CLIENTERROR
  else if (hfConnectionClose in Http.HeaderFlags) or
          not SockIsDefined then
    DoRetry('connection closed (keepalive timeout or max)', [])
  else if not fSock.Available(@loerr) then
    DoRetry('connection broken (socketerror=%)', [loerr])
  else if not SockConnected then
    DoRetry('getpeername() failed', [])
  else
  try
    // send request - we use SockSend because writeln() is calling flush()
    try
      // prepare headers
      RequestSendHeader(ctxt.Url, ctxt.Method);
      buflen := fSndBufLen;
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
        SockSendHeaders(ctxt.Header); // normalizing CRLF
      if Http.CompressList <> nil then
        SockSendHeaders(Http.CompressList^.AcceptEncoding);
      SockSendCRLF;
      // flush headers and Data/InStream body
      SockSendFlush(dat);
      if fExtendedOptions.Auth.Scheme in [wraBasic, wraBearer] then
        FillCharFast(pointer(fSndBuf)^, buflen, 0); // hide SPI bearer
      if ctxt.InStream <> nil then
      begin
        // InStream may be a THttpMultiPartStream -> Seek(0) calls Flush
        ctxt.InStream.Seek(0, soBeginning);
        res := SockSendStream(ctxt.InStream, 1 shl 20,
             {noraise=}false, {checkrecv=}true);
        AppendLine(fRequestContext, [ctxt.InStream, ' = ', _NR[res]]);
        if res = nrRetry then
        begin
          // the server interrupted the upload by sending something (e.g. 413)
          if Assigned(OnLog) then
             OnLog(sllTrace, 'RequestInternal: response during SockSendStream %',
               [ctxt.InStream], self);
          include(Http.HeaderFlags, hfConnectionClose); // socket state is wrong
        end;
      end;
      // wait and retrieve HTTP command line response
      pending := SockReceivePending(Timeout, @loerr); // select/poll
      case pending of
        cspDataAvailable:
          ; // ok
        cspDataAvailableOnClosedSocket:
          include(Http.HeaderFlags, hfConnectionClose); // socket is closed
        cspNoData:
          if SockConnected then // getpeername()=nrOK
          begin
            // timeout may happen not because the server took its time, but
            // because the network is down: sadly, the socket is still reported
            // as OK by the OS (on both Windows and POSIX)
            AppendLine(fRequestContext, ['NoData ms=', Timeout]);
            // -> no need to retry
            ctxt.Status := HTTP_TIMEOUT;
            // -> close the socket, since this HTTP request is clearly aborted
            include(Http.HeaderFlags, hfConnectionClose);
            exit;
          end
          else
          begin
            DoRetry('NoData waiting %ms for headers', [TimeOut]);
            exit;
          end;
      else // cspSocketError, cspSocketClosed
        begin
          DoRetry('% % waiting %ms for headers',
            [ToText(pending)^, CardinalToHexShort(loerr), TimeOut]);
          exit;
        end;
      end;
      SockRecvLn(Http.CommandResp); // will raise ENetSock on any error
      cmd := pointer(Http.CommandResp);
      if IdemPChar(cmd, 'HTTP/1.') and
         (cmd[7] in ['0', '1']) then
      begin
        if cmd[7] = '0' then
          include(Http.ResponseFlags, rfHttp10);
        // get http numeric status code (200,404...) from 'HTTP/1.x ###'
        ctxt.Status := GetCardinal(cmd + 9);
        if (ctxt.Status < 200) or
           (ctxt.Status > 599) then // the HTTP standard requires three digits
        begin
          AppendLine(fRequestContext, ['Invalid ', Http.CommandResp]);
          exit; // abort but returns the received number (may be 0)
        end;
      end
      else
      begin
        // error on reading answer -> 505=wrong format
        if Http.CommandResp = '' then
          DoRetry('Broken Link - timeout=%ms', [TimeOut])
        else
          DoRetry('Command=%', [Http.CommandResp], HTTP_HTTPVERSIONNONSUPPORTED);
        exit;
      end;
      // retrieve all HTTP headers
      GetHeader(hroHeadersUnfiltered in Http.Options);
      if (rfHttp10 in Http.ResponseFlags) and // implicit keepalive in HTTP/1.1
         not (hfConnectionKeepAlive in Http.HeaderFlags) then
        include(Http.HeaderFlags, hfConnectionClose);
      // retrieve Body content (if any) - see RFC 7230 #3.3.3
      if (ctxt.Status >= HTTP_SUCCESS) and
         // status 100..109,204,304 -> no body (RFC 2616 #4.3)
         (((ctxt.Status <> HTTP_NOCONTENT) and
           (ctxt.Status <> HTTP_NOTMODIFIED)) or
          (Http.ContentLength > 0) or // server bug of 204,304 with body
          (hfTransferChunked in Http.HeaderFlags)) and
         // HEAD/OPTIONS
         not HttpMethodWithNoBody(ctxt.Method) then
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
        if bodystream <> nil then
          AppendLine(fRequestContext, [ctxt.Status, ' over ', bodystream]);
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
          DoRetry('% raised after % [%]',
            [E, ToText(ENetSock(E).LastError)^, E.Message])
        else
        begin
          // propagate custom exceptions to the caller (e.g. from progression)
          AppendLine(fRequestContext, [E, ':', E.Message]);
          raise;
        end;
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
var
  secret: SpiUtf8;
begin
  if not SockIsDefined then
    exit;
  if SockIn = nil then
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  fSndBufLen := 0;
  if (url = '') or
     (url[1] <> '/') then
    SockSendLine([method, ' /', url, ' HTTP/1.1']) // should always start with /
  else
    SockSendLine([method, ' ', url, ' HTTP/1.1']);
  {$ifdef OSPOSIX}
  if SocketLayer = nlUnix then
    SockSend('Host: unix') // not part of the HTTP standard anyway
  else
  {$endif OSPOSIX}
  if (fPort = '') or // = '' for fProxyHttp on port 80
     (fPort = DEFAULT_PORT[ServerTls]) then
    SockSendLine(['Host: ', fServer])
  else
    SockSendLine(['Host: ', fServer, ':', fPort]);
  if fProxyAuthHeader <> '' then
    SockSend(fProxyAuthHeader);
  if (fRangeStart > 0) or
     (fRangeEnd > 0) then
    if fRangeEnd > fRangeStart then
      SockSend(['Range: bytes=', fRangeStart, '-', fRangeEnd])
    else
      SockSend(['Range: bytes=', fRangeStart, '-']);
  with fExtendedOptions.Auth do
    case Scheme of
      wraBasic:
        begin
          BasicClient(UserName, Password, secret);
          SockSend(secret);
          FillZero(secret);
        end;
      wraBearer:
        SockSendLine(['Authorization: Bearer ', Token]);
    end; // other Scheme values would have set OnAuthorize
  if fReferer <> '' then
    SockSendLine(['Referer: ', fReferer]);
  if fAccept <> '' then
    SockSendLine(['Accept: ', fAccept]);
  SockSendLine(['User-Agent: ', fExtendedOptions.UserAgent]);
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
  // prepare the execution
  fRequestContext := '';
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
  // execute the request
  if (not Assigned(fOnBeforeRequest)) or
     fOnBeforeRequest(self, ctxt) then
  begin
    fRedirected := '';
    if Assigned(fOnProtocolRequest) then
    begin
      // emulate a custom protocol (e.g. 'file://') into a HTTP request
      if Http.ParseAll(ctxt.InStream, ctxt.Data,
        Join([method, ' ', ctxt.Url, ' HTTP/1.0']), ctxt.Header) then
      begin
        Http.CommandResp := fOpenUriFull; // e.g. 'file:///C:/folder/file.txt'
        ctxt.Status := fOnProtocolRequest(Http);
        if StatusCodeIsSuccess(ctxt.Status) then
          ctxt.Status := Http.ContentToOutput(ctxt.Status, ctxt.OutStream);
        if Assigned(OnLog) then
          OnLog(sllTrace, 'Request(%)=% via %.OnRequest',
            [fOpenUriFull, ctxt.Status,
             TObject(TMethod(fOnProtocolRequest).Data)], self);
      end;
    end
    else
    repeat
      // sub-method to handle the actual request, with proper retry
      RequestInternal(ctxt);
      if Aborted then
        break;
      // handle optional (proxy) authentication callbacks
      if (ctxt.Status = HTTP_UNAUTHORIZED) and
          Assigned(fOnAuthorize) then
      begin
        if Assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [ctxt.Method, url, ctxt.Status], self);
        AppendLine(fRequestContext, [ctxt.Status]);
        if rAuth in ctxt.Retry then
          break; // avoid infinite recursion
        include(ctxt.Retry, rAuth);
        if fOnAuthorize(self, ctxt, Http.HeaderGetValue('WWW-AUTHENTICATE')) then
          continue;
      end
      else if (ctxt.Status = HTTP_PROXYAUTHREQUIRED) and
          Assigned(fOnProxyAuthorize) then
      begin
        if Assigned(OnLog) then
          OnLog(sllTrace, 'Request(% %)=%', [ctxt.Method, url, ctxt.Status], self);
        AppendLine(fRequestContext, [ctxt.Status]);
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
         (ctxt.Redirected >= fExtendedOptions.RedirectMax) then
        break;
      if retry then
        ctxt.Retry := [rMain]
      else
        ctxt.Retry := [];
      ctxt.Url := Http.HeaderGetValue('LOCATION');
      AppendLine(fRequestContext, [ctxt.Status, ' into ', ctxt.Url]);
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
      if Assigned(OnLog) then
        OnLog(sllTrace, 'Request % % redirected to %', [ctxt.Method, url, ctxt.Url], self);
      if Assigned(fOnRedirect) then
        if not fOnRedirect(self, ctxt) then
          break;
      if IsHttp(ctxt.Url) and
         newuri.From(ctxt.Url) then // relocated to another server
      begin
        fRedirected := newuri.Address;
        if (hfConnectionClose in Http.HeaderFlags) or
           (newuri.Server <> Server) or
           (newuri.Port <> Port) or
           (newuri.Https <> ServerTls) then
        begin
          Close; // relocated to another server -> reset the TCP connection
          try
            AppendLine(fRequestContext, ['ReOpen ', newuri.URI]);
            OpenBind(newuri.Server, newuri.Port, {bind=}false, newuri.Https);
          except
            on E: Exception do
            begin
              AppendLine(fRequestContext, [E, ': ', E.Message]);
              ctxt.Status := HTTP_CLIENTERROR; // more explicit than 404 or 501
            end;
          end;
          HttpStateReset;
          ctxt.Url := newuri.Address;
        end;
      end
      else
        fRedirected := ctxt.Url;
      inc(ctxt.Redirected);
    until Aborted;
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
      EHttpSocket.RaiseUtf8('%.WGet: HEAD %:%/% failed as %',
        [self, fServer, fPort, requrl, StatusCodeToShort(res)]);
    expsize := Http.ContentLength;
    result := expsize > 0;
    if result and
       (fRedirected <> '') then
      // don't perform 3xx again - especially needed if server:port did change
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

  procedure RttiRequestAndFreeStream;
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
    cached := MakePath([params.HashCacheDir, ExtractFileName(result)]);
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
  RequestClear; // reset Range from any previous failed request
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
    RttiRequestAndFreeStream;
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
        RttiRequestAndFreeStream;             // try again without any resume
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
        // notify peercache and also make RenameFile(part, result)
        params.Alternate.OnDownloaded(params, part, result, altdownloading);
        altdownloading := 0;
      except
        // ignore any fatal error in callbacks
      end;
    // valid .part file can now be converted into the result file
    if FileExists(part) then // if not already done in Alternate.OnDownloaded()
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

procedure THttpClientSocket.ResetExtendedOptions;
begin
  fExtendedOptions.Init;
end;

procedure THttpClientSocket.SetAuthBearer(const Value: SpiUtf8);
begin
  fOnAuthorize := nil;
  fExtendedOptions.AuthorizeBearer(Value);
end;

procedure THttpClientSocket.AuthorizeBasic(const UserName: RawUtf8;
  const Password: SpiUtf8);
begin
  fOnAuthorize := nil;
  fExtendedOptions.AuthorizeBasic(UserName, Password);
end;

procedure THttpClientSocket.AuthorizeDigest(const UserName: RawUtf8;
  const Password: SpiUtf8; Algo: TDigestAlgo);
begin
  fOnAuthorize := nil;
  fExtendedOptions.AuthorizeDigest(UserName, Password);
  if UserName = '' then
    exit;
  fOnAuthorize := OnAuthorizeDigest;
  fAuthDigestAlgo := Algo;
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
    auth := DigestClient(fAuthDigestAlgo, p + 7, Context.method, Context.url,
      fExtendedOptions.Auth.UserName, fExtendedOptions.Auth.Password);
    if auth <> '' then
      AppendLine(Context.header, ['Authorization: Digest ', auth]);
  end;
  result := true;
end;

{$ifdef DOMAINRESTAUTH}

procedure KerberosChannelBinding(const Tls: INetTls; var SecContext: TSecContext;
  var Temp: THash512Rec);
var
  hasher: RawUtf8;
  cert: RawUtf8;
begin
  if not Assigned(Tls) then
    exit;
  cert := Tls.GetRawCert(@hasher);
  if cert = '' then
    exit;
  SecContext.ChannelBindingsHashLen := HashForChannelBinding(cert, hasher, Temp);
  if SecContext.ChannelBindingsHashLen <> 0 then
      SecContext.ChannelBindingsHash := @Temp;
end;

// see https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication

procedure DoSspi(Sender: THttpClientSocket; var Context: THttpClientRequest;
  const Authenticate, InHeaderUp, OutHeader: RawUtf8);
var
  sc: TSecContext;
  bak: RawUtf8;
  unauthstatus: integer;
  datain, dataout: RawByteString;
  channelbindingtemp: THash512Rec;
begin
  if (Sender = nil) or
     not IdemPChar(pointer(Authenticate), SECPKGNAMEHTTP_UPPER) then
    exit;
  unauthstatus := Context.status; // either 401 (http auth) or 407 (proxy auth)
  bak := Context.header;
  InvalidateSecContext(sc);
  try
    // Kerberos + TLS may require tls-server-end-point channel binding
    if Assigned(Sender.Secure) and
       (Sender.AuthScheme = wraNegotiateChannelBinding) then
      KerberosChannelBinding(Sender.Secure, sc, channelbindingtemp);
    // main Kerberos loop
    repeat
      FindNameValue(Sender.Http.Headers, pointer(InHeaderUp), RawUtf8(datain));
      datain := Base64ToBin(TrimU(datain));
      if Sender.fExtendedOptions.Auth.UserName <> '' then // from AuthorizeSspiUser()
        ClientSspiAuthWithPassword(sc, datain, Sender.fExtendedOptions.Auth.UserName,
          Sender.fExtendedOptions.Auth.Password, Sender.AuthorizeSspiSpn, dataout)
      else                               // use current logged user
        ClientSspiAuth(sc, datain, Sender.AuthorizeSspiSpn, dataout);
      if dataout = '' then
        break;
      Context.header := OutHeader + BinToBase64(dataout);
      if bak <> '' then
        Append(Context.header, #13#10, bak);
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

class function THttpClientSocket.OnAuthorizeSspi(Sender: THttpClientSocket;
  var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
begin
  if InitializeDomainAuth then
    // try to setup sspi/gssapi -> SECPKGNAMEHTTP
    DoSspi(Sender, Context, Authenticate,
      'WWW-AUTHENTICATE: ' + SECPKGNAMEHTTP_UPPER + ' ',
      'Authorization: '    + SECPKGNAMEHTTP + ' ');
  result := false; // final RequestInternal() was done within DoSspi()
end;

procedure THttpClientSocket.AuthorizeSspiUser(const UserName: RawUtf8;
  const Password: SpiUtf8; const KerberosSpn: RawUtf8);
begin
  if not InitializeDomainAuth then
    EHttpSocket.RaiseUtf8('%.AuthorizeSspiUser: no % available on this system',
      [self, SECPKGNAMEAPI]);
  fOnAuthorize := nil;
  fExtendedOptions.AuthorizeSspiUser(UserName, Password);
  if UserName = '' then
    exit;
  fOnAuthorize := OnAuthorizeSspi;
  if KerberosSpn <> '' then
    fAuthorizeSspiSpn := KerberosSpn;
end;

class function THttpClientSocket.OnProxyAuthorizeSspi(Sender: THttpClientSocket;
  var Context: THttpClientRequest; const Authenticate: RawUtf8): boolean;
begin
  if InitializeDomainAuth then
    // try to setup sspi/gssapi -> SECPKGNAMEHTTP
    DoSspi(Sender, Context, Authenticate,
      'PROXY-AUTHENTICATE: '  + SECPKGNAMEHTTP_UPPER + ' ',
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

function DoHttpGet(http: THttpClientSocket; const url, inHeaders: RawUtf8;
  outHeaders: PRawUtf8; outStatus: PInteger): RawByteString;
var
  status: integer;
begin
  if Http <> nil then
  try
    Http.RedirectMax := 5; // fair enough
    status := Http.Get(url, 0, inHeaders);
    if outStatus <> nil then
      outStatus^ := status;
    if status in [HTTP_SUCCESS .. HTTP_PARTIALCONTENT] then
    begin
      result := Http.Http.Content;
      if outHeaders <> nil then
        outHeaders^ := Http.Http.Headers;
    end;
  finally
    Http.Free;
  end;
end;

function OpenHttpGet(const server, port, url, inHeaders: RawUtf8;
  outHeaders: PRawUtf8; aLayer: TNetLayer; aTLS: boolean;
  outStatus: PInteger; aTimeout: integer; ignoreTlsCertError: boolean): RawByteString;
var
  tmp: TNetTlsContext;
begin
  result := DoHttpGet(OpenHttp(server, port, aTLS, aLayer, '', aTimeout,
                        GetTlsContext(aTLS, ignoreTlsCertError, tmp)),
              url, inHeaders, outHeaders, outStatus);
end;


{ ******************** THttpRequest Abstract HTTP client class }

{ THttpRequestExtendedOptions }

procedure THttpRequestExtendedOptions.Init;
begin
  RecordZero(@self, TypeInfo(THttpRequestExtendedOptions));
end;

procedure THttpRequestExtendedOptions.Clear;
begin
  FillZero(Auth.Password);
  FillZero(Auth.Token);
  Init;
end;

procedure THttpRequestExtendedOptions.AuthorizeUserPassword(
  const UserName, Password: RawUtf8; Scheme: THttpRequestAuthentication);
begin
  Auth.UserName := UserName;
  Auth.Password := Password;
  Auth.Token := '';
  if UserName = '' then
    Scheme := wraNone;
  Auth.Scheme := Scheme;
end;

procedure THttpRequestExtendedOptions.AuthorizeBasic(const UserName: RawUtf8;
  const Password: SpiUtf8);
begin
  AuthorizeUserPassword(UserName, Password, wraBasic);
end;

procedure THttpRequestExtendedOptions.AuthorizeDigest(const UserName: RawUtf8;
  const Password: SpiUtf8);
begin
  AuthorizeUserPassword(UserName, Password, wraDigest);
end;

procedure THttpRequestExtendedOptions.AuthorizeSspiUser(
  const UserName: RawUtf8; const Password: SpiUtf8);
begin
  AuthorizeUserPassword(UserName, Password, wraNegotiate);
end;

procedure THttpRequestExtendedOptions.AuthorizeBearer(const Value: SpiUtf8);
begin
  Auth.UserName := '';
  Auth.Password := '';
  Auth.Token := Value;
  if Value = '' then
    Auth.Scheme := wraNone
  else
    Auth.Scheme := wraBearer;
end;

function THttpRequestExtendedOptions.SameAuth(
  Another: PHttpRequestExtendedOptions): boolean;
begin
  result := (Another <> nil) and
            (Auth.Scheme = Another^.Auth.Scheme);
  if result then
    case Auth.Scheme of
      wraBasic,
      wraDigest,
      wraNegotiate,
      wraNegotiateChannelBinding:
        result := (Auth.UserName = Another^.Auth.UserName) and
                  (Auth.Password = Another^.Auth.Password);
      wraBearer:
        result := (Auth.Token = Another^.Auth.Token);
    end;
end;

function THttpRequestExtendedOptions.ToDocVariant(const Secret: RawByteString): variant;
var
  v: TDocVariantData absolute result;
begin
  result := SaveNetTlsContext(TLS, Secret);
  v.AddNameValuesToObject([
    'p',  Proxy,
    'as', ord(Auth.Scheme),
    'au', Auth.UserName,
    'ap', Auth.Password,
    'at', Auth.Token], {dontAddDefault=}true);
  if v.Count = 0 then
    v.Clear;
end;

function THttpRequestExtendedOptions.ToUrlEncode(const UriRoot: RawUtf8;
  const Secret: RawByteString): RawUtf8;
begin
  result := _Safe(ToDocVariant(Secret))^.ToUrlEncode(UriRoot);
end;

function THttpRequestExtendedOptions.InitFromDocVariant(const Value: variant;
  const Secret: RawByteString): boolean;
var
  v: PDocVariantData;
  s: integer;
begin
  Init;
  result := _SafeObject(Value, v);
  if not result or
     (v^.Count = 0) then
    exit;
  LoadNetTlsContext(TLS, v^, Secret);
  v^.GetAsRawUtf8('p', Proxy);
  if v^.GetAsInteger('as', s) and
     (cardinal(s) <= cardinal(high(Auth.Scheme))) then
    Auth.Scheme := THttpRequestAuthentication(s);
  v^.GetAsRawUtf8('au', Auth.UserName);
  v^.GetAsRawUtf8('ap', RawUtf8(Auth.Password));
  v^.GetAsRawUtf8('at', RawUtf8(Auth.Token));
end;

function THttpRequestExtendedOptions.InitFromUrl(const UrlParams: RawUtf8;
  const Secret: RawByteString): boolean;
var
  v: TDocVariantData;
begin
  v.InitFromUrl(pointer(UrlParams), JSON_FAST);
  result := InitFromDocVariant(variant(v), Secret);
end;


function ToText(wra: THttpRequestAuthentication): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpRequestAuthentication), ord(wra));
end;

const
  TLS_ROUNDS = 1000;
  TLS_SALT = 'a41c0c2447821c01afdcdc75f7ab8a0a';

function SaveNetTlsContext(const TLS: TNetTlsContext;
  const Secret: RawByteString): variant;
begin
  VarClear(result{%H-});
  TDocVariantData(result).InitObject([
    'te', TLS.Enabled,
    'ti', TLS.IgnoreCertificateErrors,
    'ta', TLS.AllowDeprecatedTls,
    'tu', TLS.ClientAllowUnsafeRenegotation,
    'cf', TLS.CertificateFile,
    'ca', TLS.CACertificatesFile,
    'pf', TLS.PrivateKeyFile], JSON_FAST, {dontAddDefault=}true);
  if (TLS.PrivateKeyFile <> '') and
     (TLS.PrivatePassword <> '') then
    TDocVariantData(result).AddValueText('pp',
      BinToBase64uri(CryptDataWithSecret(TLS.PrivatePassword,
        [TLS.PrivateKeyFile, TLS.CertificateFile, Secret], TLS_ROUNDS, TLS_SALT)));
end;

procedure LoadNetTlsContext(var TLS: TNetTlsContext; const V: TDocVariantData;
  const Secret: RawByteString);
begin
  V.GetAsBoolean('te', TLS.Enabled);
  V.GetAsBoolean('ti', TLS.IgnoreCertificateErrors);
  V.GetAsBoolean('ta', TLS.AllowDeprecatedTls);
  V.GetAsBoolean('tu', TLS.ClientAllowUnsafeRenegotation);
  V.GetAsRawUtf8('cf', TLS.CertificateFile);
  V.GetAsRawUtf8('ca', TLS.CACertificatesFile);
  V.GetAsRawUtf8('pf', TLS.PrivateKeyFile);
  if (TLS.PrivateKeyFile <> '') and
     V.GetAsRawUtf8('pp', TLS.PrivatePassword) then
    TLS.PrivatePassword := CryptDataWithSecret(Base64uriToBin(TLS.PrivatePassword),
      [TLS.PrivateKeyFile, TLS.CertificateFile, Secret], TLS_ROUNDS, TLS_SALT);
end;


{$ifdef USEHTTPREQUEST}

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
  const aProxyName, aProxyByPass: RawUtf8;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal;
  aLayer: TNetLayer; const aUserAgent: RawUtf8);
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
  if fExtendedOptions.UserAgent = '' then
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

constructor THttpRequest.Create(const aUri, aProxyName, aProxyByPass: RawUtf8;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal;
  aIgnoreTlsCertificateErrors: boolean);
var
  uri: TUri;
begin
  if not uri.From(aUri) then
    EHttpSocket.RaiseUtf8('%.Create: invalid url=%', [self, aUri]);
  IgnoreTlsCertificateErrors := aIgnoreTlsCertificateErrors;
  Create(uri.Server, uri.Port, uri.Https, aProxyName, aProxyByPass,
    ConnectionTimeOut, SendTimeout, ReceiveTimeout, uri.Layer);
end;

constructor THttpRequest.Create(
  const aUri: TUri; aOptions: PHttpRequestExtendedOptions);
begin
  if aOptions <> nil then
    fExtendedOptions := aOptions^; // to be set before Create=InternalConnect
  Create(aUri.Server, aUri.Port, aUri.Https, fExtendedOptions.Proxy, {bypass=}'',
    fExtendedOptions.CreateTimeoutMS, fExtendedOptions.CreateTimeoutMS,
    fExtendedOptions.CreateTimeoutMS, aUri.Layer);
end;

destructor THttpRequest.Destroy;
begin
  inherited Destroy;
  fExtendedOptions.Clear;
end;

function THttpRequest.Request(const url, method: RawUtf8; KeepAlive: cardinal;
  const InHeader: RawUtf8; const InData: RawByteString; const InDataType: RawUtf8;
  out OutHeader: RawUtf8; out OutData: RawByteString): integer;
var
  data: RawByteString;
  acceptEnc, contentEnc, aUrl: RawUtf8;
  comp: PHttpSocketCompressRec;
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
      InternalAddHeader(Join(['Content-Type: ', InDataType]));
    // handle custom compression
    data := InData;
    if integer(fCompressAcceptHeader) <> 0 then
    begin
      comp := fCompressList.CompressContent(
        fCompressAcceptHeader, InDataType, data);
      if comp <> nil then
        InternalAddHeader(Join(['Content-Encoding: ', comp^.Name]));
    end;
    if fCompressList.AcceptEncoding <> '' then
      InternalAddHeader(fCompressList.AcceptEncoding);
    upload:= IsPost(method) or IsPut(method);
    // send request to remote server
    if Assigned(fOnUpload) and
       upload then
      fOnUpload(self, false)
    else if Assigned(fOnDownload) and
            not upload then
      fOnDownload(self, false);
    InternalSendRequest(method, data);
    // retrieve status and headers
    result := InternalRetrieveAnswer(OutHeader, contentEnc, acceptEnc, OutData);
    if Assigned(fOnUpload) and
       upload then
      fOnUpload(self, true)
    else if Assigned(fOnDownload) and
            not upload then
      fOnDownload(self, true);
    // handle incoming answer compression
    if OutData <> '' then
    begin
      if contentEnc <> '' then
        if fCompressList.UncompressContent(contentEnc, OutData) = nil then
          EHttpSocket.RaiseUtf8('%.Request: % uncompress error', [self, contentEnc]);
      if acceptEnc <> '' then
        fCompressList.DecodeAcceptEncoding(pointer(acceptEnc), fCompressAcceptHeader);
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
  result := fCompressList.RegisterFunc(aFunction, aCompressMinSize, aPriority) <> nil;
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
      EHttpSocket.RaiseU('MainHttpClass: No THttpRequest class known!');
  end;
  result := _MainHttpClass;
end;

procedure ReplaceMainHttpClass(aClass: THttpRequestClass);
begin
  _MainHttpClass := aClass;
end;

{$endif USEHTTPREQUEST}


{ ******************** TWinHttp TWinINet classes }

{$ifdef USEWININET}

{ TWinHttpApi }

function TWinHttpApi.InternalRetrieveAnswer(var Header, Encoding,
  AcceptEncoding: RawUtf8; var Data: RawByteString): integer;
var
  ChunkSize, Bytes, ContentLength, Read: cardinal;
  tmp: RawByteString;
begin
  // HTTP_QUERY* and WINHTTP_QUERY* do match -> common to TWinINet + TWinHttp
  result         := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
  Header         := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
  Encoding       := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
  AcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
  // retrieve received content (if any)
  Read := 0;
  ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
  if Assigned(fOnProgress) then
    fOnProgress(self, 0, ContentLength); // initial notification
  if Assigned(fOnDownload) then
    // download per-chunk using callback event
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
  if IsNone(fProxyName) then
    access := WINHTTP_ACCESS_TYPE_NO_PROXY
  else if fProxyName = '' then
    if (OSVersion >= wEightOne) or
       WinHttpForceProxyDetection then
      access := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY // Windows 8.1 and newer
    else
      access := WINHTTP_ACCESS_TYPE_NO_PROXY
  else
    access := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  if access <> WINHTTP_ACCESS_TYPE_NO_PROXY then
  begin
    Utf8ToSynUnicode(fProxyName, pn);
    Utf8ToSynUnicode(fProxyByPass, pb);
  end;
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
    RaiseFromLastError('Open');
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpApi.SetTimeouts(fSession, HTTP_DEFAULT_RESOLVETIMEOUT,
     ConnectionTimeOut, SendTimeout, ReceiveTimeout) then
    RaiseFromLastError('SetTimeouts');
  if fHttps or
     (fExtendedOptions.RedirectMax > 0) then // may redirect from http to https
  begin
    protocols := InternalGetProtocols;
    if not WinHttpApi.SetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
        @protocols, SizeOf(protocols)) then
      RaiseFromLastError('SetOption(tls)');
    Callback := WinHttpApi.SetStatusCallback(fSession,
      WinHttpSecurityErrorCallback, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes = WINHTTP_INVALID_STATUS_CALLBACK then
      RaiseFromLastError('SetStatusCallback');
  end;
  fConnection := WinHttpApi.Connect(
    fSession, pointer(Utf8ToSynUnicode(fServer)), fPort, 0);
  if fConnection = nil then
    RaiseFromLastError('Connect');
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
    RaiseFromLastError('OpenRequest');
  if fKeepAlive = 0 then
  begin
    Flags := WINHTTP_DISABLE_KEEP_ALIVE;
    if not WinHttpApi.SetOption(
       fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @Flags, SizeOf(Flags)) then
      RaiseFromLastError('SetOption(keepalive)');
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
    RaiseFromLastError('AddRequestHeaders');
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
            RaiseFromLastError('WriteData');
          inc(Current, BytesWritten);
          if not fOnUpload(Self, Current, L) then
            EWinHttp.RaiseUtf8('%: OnUpload cancel % on %:%',
              [self, aMethod, fServer, fPort]);
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
  usr, pwd: SynUnicode;
begin
  if AuthScheme <> wraNone then
    if AuthScheme = wraBearer then
      InternalAddHeader(AuthorizationBearer(AuthToken))
    else
    begin
      case AuthScheme of
        wraBasic:
          winAuth := WINHTTP_AUTH_SCHEME_BASIC;
        wraDigest:
          winAuth := WINHTTP_AUTH_SCHEME_DIGEST;
        wraNegotiate,
        wraNegotiateChannelBinding:
          winAuth := WINHTTP_AUTH_SCHEME_NEGOTIATE;
      else // no RaiseUtf8 to avoid "winAuth not initialized" error on Delphi
        raise EWinHttp.CreateUtf8('%: unsupported AuthScheme=% on % %:%',
          [self, ToText(AuthScheme)^, aMethod, fServer, fPort]);
      end;
      Utf8ToSynUnicode(AuthUserName, usr);
      Utf8ToSynUnicode(AuthPassword, pwd);
      try
        if not WinHttpApi.SetCredentials(fRequest, WINHTTP_AUTH_TARGET_SERVER,
           winAuth, pointer(usr), pointer(pwd), nil) then
          RaiseFromLastError('SetCredentials');
      finally
        FillZero(pwd);
      end;
    end;
  if IgnoreTlsCertificateErrors then
    if fHttps or
       (fExtendedOptions.RedirectMax > 0) then // may redirect from http to https
      if not WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
         @SECURITY_FLAG_IGNORE_CERTIFICATES, SizeOf(cardinal)) then
        RaiseFromLastError('SetOption(ignorecert)');
  if fExtendedOptions.RedirectMax > 0 then
    if WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_REDIRECT_POLICY,
         @REDIRECT_POLICY_ALWAYS, SizeOf(cardinal)) then
      WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_MAX_HTTP_AUTOMATIC_REDIRECTS,
        @fExtendedOptions.RedirectMax, SizeOf(cardinal)); // ignore errors
  L := length(aData);
  if _SendRequest(L) and
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success
  if (fHttps or
      (fExtendedOptions.RedirectMax > 0)) and
     (GetLastError = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) and
     IgnoreTlsCertificateErrors and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAG_IGNORE_CERTIFICATES, SizeOf(cardinal)) and
     WinHttpApi.SetOption(fRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
       pointer(WINHTTP_NO_CLIENT_CERT_CONTEXT), 0) and
     _SendRequest(L) and // retry
     WinHttpApi.ReceiveResponse(fRequest, nil) then
    exit; // success with no certificate validation
  // if we reached here, an error occurred
  RaiseFromLastError('SendRequest');
end;

function TWinHttp.InternalGetInfo(Info: cardinal): RawUtf8;
var
  dwSize, dwIndex: cardinal;
  tmp: TSynTempBuffer;
begin
  result := '';
  dwIndex := 0;
  dwSize := tmp.Init; // first try with stack buffer (in bytes)
  try
    if not WinHttpApi.QueryHeaders(fRequest, Info, nil, tmp.buf, dwSize, dwIndex) then
    begin
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        exit;
      dwIndex := 0;
      tmp.Init(dwSize); // need more space (seldom needed)
      if not WinHttpApi.QueryHeaders(fRequest, Info, nil, tmp.buf, dwSize, dwIndex) then
        exit;
    end;
    Win32PWideCharToUtf8(tmp.buf, dwSize shr 1, result);
  finally
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
    if GetLastError = ERROR_WINHTTP_OPERATION_CANCELLED then
      result := 0 // connection may be closed by the server e.g. on 30x redirect
    else
      RaiseFromLastError('QueryDataAvailable');
end;

function TWinHttp.InternalReadData(var Data: RawByteString;
  Read: PtrInt; Size: cardinal): cardinal;
begin
  if not WinHttpApi.ReadData(fRequest, @PByteArray(Data)[Read], Size, result) then
    RaiseFromLastError('ReadData');
end;

destructor TWinHttp.Destroy;
begin
  if fConnection <> nil then
    WinHttpApi.CloseHandle(fConnection);
  if fSession <> nil then
    WinHttpApi.CloseHandle(fSession);
  inherited Destroy;
end;

procedure TWinHttp.RaiseFromLastError(const ctxt: ShortString);
var
  err: integer;
begin
  err := GetLastError;
  EWinHttp.RaiseUtf8('%: % error [%] (%) on %:%',
    [self, ctxt, WinApiErrorShort(err, WinHttpApi.LibraryHandle),
     err, fServer, fPort]);
end;


{ TWinINet }

procedure TWinINet.RaiseFromLastError(const ctxt: ShortString);
var
  err: integer;
  E: EWinINet;
begin
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  err := GetLastError;
  E := EWinINet.CreateUtf8('%: % error [%] (%) on %:%',
    [self, ctxt, SysErrorMessageWinInet(err), err, fServer, fPort]);
  E.fLastError := err;
  raise E;
end;

procedure TWinINet.InternalConnect(
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
var
  OpenType: integer;
begin
  if IsNone(fProxyName) then
    OpenType := INTERNET_OPEN_TYPE_DIRECT
  else if fProxyName = '' then
    OpenType := INTERNET_OPEN_TYPE_PRECONFIG
  else
    OpenType := INTERNET_OPEN_TYPE_PROXY;
  fSession := InternetOpenA(pointer(fExtendedOptions.UserAgent), OpenType,
    pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession = nil then
    RaiseFromLastError('Open');
  InternetSetOption(fConnection, INTERNET_OPTION_CONNECT_TIMEOUT,
    @ConnectionTimeOut, SizeOf(ConnectionTimeOut));
  InternetSetOption(fConnection, INTERNET_OPTION_SEND_TIMEOUT,
    @SendTimeout, SizeOf(SendTimeout));
  InternetSetOption(fConnection, INTERNET_OPTION_RECEIVE_TIMEOUT,
    @ReceiveTimeout, SizeOf(ReceiveTimeout));
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort,
    nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection = nil then
    RaiseFromLastError('Connect');
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
    RaiseFromLastError('OpenRequest');
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
    RaiseFromLastError('AddHeader');
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
      RaiseFromLastError('SendRequest');
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
        RaiseFromLastError('WriteFile');
      inc(datapos, BytesWritten);
      if not fOnUpload(Self, datapos, datalen) then
        raise EWinINet.CreateFmt('OnUpload Canceled %s', [aMethod]);
    end;
    if not HttpEndRequest(fRequest, nil, 0, 0) then
      RaiseFromLastError('EndRequest');
  end
  else
    // blocking send with no callback
    if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
      RaiseFromLastError('SendRequest');
end;

function TWinINet.InternalGetInfo(Info: cardinal): RawUtf8;
var
  dwSize, dwIndex: cardinal;
  tmp: TSynTempBuffer;
begin
  result := '';
  dwIndex := 0;
  dwSize := tmp.Init; // first try with stack buffer (in bytes)
  try
    if not HttpQueryInfoW(fRequest, Info, tmp.buf, dwSize, dwIndex) then
    begin
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        exit;
      dwIndex := 0;
      tmp.Init(dwSize); // need more space (seldom needed)
      if not HttpQueryInfoW(fRequest, Info, tmp.buf, dwSize, dwIndex) then
        exit;
    end;
    Win32PWideCharToUtf8(tmp.buf, dwSize shr 1, result);
  finally
    tmp.Done;
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
    RaiseFromLastError('QueryDataAvailable');
end;

function TWinINet.InternalReadData(var Data: RawByteString;
  Read: PtrInt; Size: cardinal): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], Size, result) then
    RaiseFromLastError('ReadData');
end;

destructor TWinINet.Destroy;
begin
  if fConnection <> nil then
    InternetCloseHandle(FConnection);
  if fSession <> nil then
    InternetCloseHandle(FSession);
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

procedure TCurlHttp.InternalConnect(
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: cardinal);
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
    FormatUtf8('http%://%:%', [TLS_TEXT[fHttps], fServer, fPort], fRootURL);
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
  if fExtendedOptions.RedirectMax > 0 then // url redirection (as TWinHttp)
    curl.easy_setopt(fHandle, coFollowLocation, 1);
  //curl.easy_setopt(fHandle,coTCPNoDelay,0); // disable Nagle
  if fLayer = nlUnix then
    curl.easy_setopt(fHandle, coUnixSocketPath, pointer(fServer));
  curl.easy_setopt(fHandle, coURL, pointer(fIn.URL));
  if (fProxyName <> '') and
     not IsNone(fProxyName) then
    curl.easy_setopt(fHandle, coProxy, pointer(fProxyName));
  if fHttps or
     (fExtendedOptions.RedirectMax > 0) then // may redirect from http to https
    // see https://curl.haxx.se/libcurl/c/simplessl.html
    if IgnoreTlsCertificateErrors then
    begin
      curl.easy_setopt(fHandle, coSSLVerifyPeer, 0);
      curl.easy_setopt(fHandle, coSSLVerifyHost, 0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyPeer,0);
      //curl.easy_setopt(fHandle,coProxySSLVerifyHost,0);
    end
    else if fTls.CertFile <> '' then
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
    [cauNegotiate], // wraNegotiateChannelBinding
    [cauBearer]);   // wraBearer

procedure TCurlHttp.InternalSendRequest(const aMethod: RawUtf8;
  const aData: RawByteString);
begin
  // 1. handle authentication
  curl.easy_setopt(fHandle, coUserName, pointer(AuthUserName));
  curl.easy_setopt(fHandle, coPassword, pointer(AuthPassword));
  curl.easy_setopt(fHandle, coXOAuth2Bearer, pointer(AuthToken));
  curl.easy_setopt(fHandle, coHttpAuth, integer(WRA2CAU[AuthScheme]));
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


{ ******************** IHttpClient / TSimpleHttpClient Wrappers }

{ THttpClientAbstract }

destructor THttpClientAbstract.Destroy;
begin
  Close;
  inherited Destroy;
end;

function THttpClientAbstract.Body: RawByteString;
begin
  result := fBody;
end;

function THttpClientAbstract.Status: integer;
begin
  result := fStatus;
end;

function THttpClientAbstract.LastError: string;
begin
  result := fLastError;
end;

function THttpClientAbstract.Headers: RawUtf8;
begin
  result := fHeaders;
end;

function THttpClientAbstract.Header(
  const Name: RawUtf8; out Value: RawUtf8): boolean;
begin
  result := GetHeader(fHeaders, Name, Value);
end;

function THttpClientAbstract.Header(
  const Name: RawUtf8; out Value: Int64): boolean;
begin
  result := GetHeader(fHeaders, Name, Value);
end;

function THttpClientAbstract.Tls: PNetTlsContext;
begin
  result := @fConnectOptions.TLS;
end;

function THttpClientAbstract.Request(const Uri, Method, Header: RawUtf8;
  const Data: RawByteString; const DataMimeType: RawUtf8;
  KeepAlive: cardinal): integer;
var
  u: TUri;
begin
  fUri := Uri;
  if u.From(Uri) then
    result := Request(u, Method, Header, Data, DataMimeType, KeepAlive)
  else
    result := HTTP_CLIENTERROR; // not 404 because no server was involved
  fStatus := result;
end;

function THttpClientAbstract.Connected(const Server: TUri): string;
begin
  try
    RawConnect(Server);
    result := ''; // success
  except
    on E: Exception do
    begin
      result := E.Message;
      Close;
    end;
  end;
end;


{ TSimpleHttpClient }

constructor TSimpleHttpClient.Create(aOnlyUseClientSocket: boolean);
begin
  fConnectOptions.RedirectMax := 4; // seems fair enough
  {$ifdef USEHTTPREQUEST}
  fOnlyUseClientSocket := aOnlyUseClientSocket or
                          not MainHttpClass.IsAvailable;
  {$endif USEHTTPREQUEST}
  inherited Create;
end;

destructor TSimpleHttpClient.Destroy;
begin
  fConnectOptions.Clear;
  inherited Destroy;
end;

procedure TSimpleHttpClient.RawConnect(const Server: TUri);
begin
  {$ifdef USEHTTPREQUEST}
  if (Server.Https or
      (fConnectOptions.Proxy <> '')) and
     not fOnlyUseClientSocket then
  begin
    if (fHttps = nil) or
       (fHttps.Server <> Server.Server) or
       (fHttps.Port <> Server.PortInt) then
    begin
      Close; // need a new https connection
      fHttps := MainHttpClass.Create(Server, @fConnectOptions); // connect
    end;
  end
  else
  {$endif USEHTTPREQUEST}
  // if we reached here, plain http or fOnlyUseClientSocket or fHttps failed
  if (fHttp = nil) or
     (fHttp.Server <> Server.Server) or
     (fHttp.Port <> Server.Port) or
     not fHttp.SockConnected then
  begin
    Close;
    fHttp := THttpClientSocket.OpenOptions(Server, fConnectOptions); // connect
  end;
end;

procedure TSimpleHttpClient.Close;
begin
  if fHttp <> nil then
    fConnectOptions := fHttp.fExtendedOptions; // for the next RawConnect()
  FreeAndNil(fHttp);
  {$ifdef USEHTTPREQUEST}
  if fHttps <> nil then
    fConnectOptions := fHttps.fExtendedOptions;
  FreeAndNil(fHttps);
  {$endif USEHTTPREQUEST}
end;

function TSimpleHttpClient.Options: PHttpRequestExtendedOptions;
begin
  {$ifdef USEHTTPREQUEST}
  if fHttps <> nil then
    result := @fHttps.fExtendedOptions
  else
  {$endif USEHTTPREQUEST}
  if fHttp <> nil then
    result := @fHttp.fExtendedOptions
  else
    result := @fConnectOptions; // used outside an actual connection
end;

function TSimpleHttpClient.Request(const Uri: TUri;
  const Method, Header: RawUtf8; const Data: RawByteString;
  const DataMimeType: RawUtf8; KeepAlive: cardinal): integer;
begin
  // reset status
  fLastError := '';
  fStatus := 0;
  // do the request
  result := 0;
  try
    RawConnect(Uri); // raise an exception on connection issue
    {$ifdef USEHTTPREQUEST}
    if fHttps <> nil then
      result := fHttps.Request(
        Uri.Address, Method, KeepAlive, Header, Data, DataMimeType, fHeaders, fBody)
    else
    {$endif USEHTTPREQUEST}
    if fHttp <> nil then // paranoid
    begin
      // if we reached here, plain http or fOnlyUseClientSocket or fHttps failed
      result := fHttp.Request(
        Uri.Address, Method, KeepAlive, Header, Data, DataMimeType, {retry=}true);
      fBody := fHttp.Http.Content;
      fHeaders := fHttp.Http.Headers;
    end;
    if KeepAlive = 0 then
      Close; // force HTTP/1.0 scheme
  except
    on E: Exception do
    begin
      FormatString('% % raised % [%]',
        [Method, Uri.URI, E, E.Message], fLastError);
      Close; // keeping result = 0
    end;
  end;
  fStatus := result;
end;


{ ******************** TJsonClient JSON requests over HTTP }

function FindCustomEnum(const CustomText: array of RawUtf8;
  const Value: RawUtf8): integer;
begin
  if (Value <> '') and
     (high(CustomText) > 0) then
    // we can just ignore CustomText[0] which is supposed to be ''
    result := FindRawUtf8(@CustomText[1], Value, high(CustomText), {casesens=}true) + 1
  else
    result := 0;
end;


{ EJsonClient }

constructor EJsonClient.CreateResp(const Format: RawUtf8;
  const Args: array of const; const Resp: TJsonResponse);
begin
  CreateUtf8(Format, Args);
  fResponse := Resp;
end;


{ TJsonResponse }

procedure TJsonResponse.Init;
begin
  RecordZero(@self, TypeInfo(TJsonResponse));
end;

procedure TJsonResponse.InitFrom(const aMethod, aUrl: RawUtf8;
  const aClient: IHttpClient);
begin
  Url := aUrl;
  Method := aMethod;
  Status := aClient.Status;
  Headers := aClient.Headers;
  Content := aClient.Body;
end;

function TJsonResponse.Header(const Name: RawUtf8; out Value: RawUtf8): boolean;
begin
  result := GetHeader(Headers, Name, Value);
end;

function TJsonResponse.Header(const Name: RawUtf8; out Value: Int64): boolean;
begin
  result := GetHeader(Headers, Name, Value);
end;


{ TJsonClientAbstract }

function TJsonClientAbstract.CheckRequestError(const Response: TJsonResponse;
  const CustomError: TOnJsonClientError): boolean;
var
  err: ShortString;
begin
  result := (Response.Status <> 0) and
            not StatusCodeIsSuccess(Response.Status);
  if not result then
    exit;
  // HTTP error
  FormatShort('Request: % on % % - %', [StatusCodeToShort(Response.Status),
    Response.Method, Response.Url, ContentToShort(Response.Content)], err);
  if Assigned(fOnLog) then
    fOnLog(sllHTTP, '%', [err], self);
  if Assigned(CustomError) then
    CustomError(self, Response, err)
  else if Assigned(fOnError) then
    fOnError(self, Response, err)
  else if jcoHttpErrorRaise in fOptions then
    raise EJsonClient.CreateResp('%.%', [self, err], Response);
end;

function TJsonClientAbstract.GetOnError: TOnJsonClientError;
begin
  result := fOnError;
end;

procedure TJsonClientAbstract.SetOnError(const Event: TOnJsonClientError);
begin
  fOnError := Event;
end;

function TJsonClientAbstract.GetOnBefore: TOnJsonClientBefore;
begin
  result := fOnBefore;
end;

procedure TJsonClientAbstract.SetOnBefore(const Event: TOnJsonClientBefore);
begin
  fOnBefore := Event;
end;

function TJsonClientAbstract.GetOnAfter: TOnJsonClientAfter;
begin
  result := fOnAfter;
end;

procedure TJsonClientAbstract.SetOnAfter(const Event: TOnJsonClientAfter);
begin
  fOnAfter := Event;
end;

function TJsonClientAbstract.GetOptions: TJsonClientOptions;
begin
  result := fOptions;
end;

procedure TJsonClientAbstract.SetOptions(Value: TJsonClientOptions);
begin
  fOptions := Value;
end;

function TJsonClientAbstract.GetUrlEncoder: TUrlEncoder;
begin
  result := fUrlEncoder;
end;

procedure TJsonClientAbstract.SetUrlEncoder(Value: TUrlEncoder);
begin
  fUrlEncoder := Value;
end;

procedure TJsonClientAbstract.SetBearer(const Token: SpiUtf8);
var
  h: IHttpClient;
  o: PHttpRequestExtendedOptions;
begin
  h := Http;
  if not Assigned(h) then
    exit;
  o := h.Options;
  if not Assigned(o) then
    exit;
  o^.Auth.Scheme := wraBearer;
  if Token = '' then
    o^.Auth.Scheme := wraNone; // disable any previous token
  o^.Auth.Token := Token;
end;

const
  FMT_REQ: array[{full=}boolean] of PUtf8Char = (
    'Request % %', 'Request % % %');

procedure TJsonClientAbstract.RttiRequest(const Method, Action, Headers: RawUtf8;
  Payload, Res: pointer; PayloadInfo, ResInfo: PRttiInfo;
  const CustomError: TOnJsonClientError);
var
  r: TJsonResponse;
  b: RawUtf8;
  j: TRttiJson;
  u: PUtf8Char;
  two: TTextWriterOptions;
  err: ShortString;
begin
  if (Payload <> nil) and
     (PayloadInfo <> nil) then
  begin
    two := [];
    if jcoPayloadWithoutVoid in fOptions then
      include(two, twoIgnoreDefaultInRecord);
    if jcoPayloadDateTimeWithZ in fOptions then
      include(two, twoDateTimeWithZ);
    SaveJson(Payload^, PayloadInfo, two, b);
  end;
  if Assigned(fOnLog) then
    fOnLog(sllServiceCall,
      FMT_REQ[((jcoLogFullRequest in fOptions) or (length(b) < 1024))],
      [Method, Action, b], self);
  if Assigned(fOnBefore) then
    fOnBefore(self, Method, Action, b);
  j := nil;
  if Res <> nil then
    j := pointer(Rtti.RegisterType(ResInfo));
  if (j <> nil) and
     not (jcoResultNoClear in fOptions) then
    j.ValueFinalizeAndClear(Res); // void result before request or parsing
  try
    RawRequest(Method, Action, '', b, Headers, r); // blocking thread-safe request
  except
    on E: Exception do
    begin
      if Assigned(fOnLog) then
        fOnLog(sllFail, FMT_REQ[true], [Method, Action, E], self);
      if not (jcoHttpExceptionIntercept in fOptions) then
        raise; // propagate HTTP low-level exception
      r.Status := HTTP_CLIENTERROR; // 666 = fake error
      r.Content := ObjectToJsonDebug(E);
    end;
  end;
  if Assigned(fOnAfter) then
    fOnAfter(self, r);
  if CheckRequestError(r, CustomError) or // HTTP status error
     (j = nil) then                       // no JSON response to parse
    exit;
  u := pointer(r.Content); // parse in-place the returned body
  j.ValueLoadJson(Res, u, nil,
    JSONPARSER_DEFAULTORTOLERANTOPTIONS[jcoParseTolerant in fOptions],
    @JSON_[mFastFloat], nil, nil);
  if u <> nil then
    exit; // JSON parsing success
  FormatShort('Request: % % failure parsing %', [Method, r.Url, j.Name], err);
  if Assigned(fOnLog) then
    fOnLog(sllServiceReturn, '%', [err], self);
  if jcoParseErrorClear in fOptions then
    j.ValueFinalizeAndClear(Res);
  if jcoParseErrorRaise in fOptions then
    EJsonClient.RaiseUtf8('%.%', [self, err]);
end;

procedure TJsonClientAbstract.Request(const Method, Action: RawUtf8;
  const CustomError: TOnJsonClientError);
begin
  RttiRequest(Method, Action, {Headers=}'',
    nil, nil, nil, nil, CustomError); // nil = no RTTI
end;

procedure TJsonClientAbstract.Request(const Method, Action: RawUtf8;
  var Res; ResInfo: PRttiInfo; const CustomError: TOnJsonClientError);
begin
  RttiRequest(Method, Action, {Headers=}'',
    {Payload=}nil, @Res, {PayloadInfo=}nil, ResInfo, CustomError);
end;

procedure DoHeadersEncode(const NameValuePairs: array of const;
  var OutHeaders: RawUtf8);
var
  a: PtrInt;
  name, value: RawUtf8;
  p: PVarRec;
  tmp: TSynTempAdder;
begin
  {%H-}tmp.Init;
  p := @NameValuePairs[0];
  for a := 0 to high(NameValuePairs) shr 1 do
  begin
    VarRecToUtf8(p, name);
    inc(p);
    VarRecToUtf8(p, value);
    if (name = '') or
       (value = '') then
      continue;
    // append name='X-MyHeader'/value='5' as 'X-MyHeader: 5'#13#10
    tmp.Add(name);
    if PosExChar(':', name) = 0 then // if name is e.g. 'Cookie: id='
      tmp.AddDirect(':', ' ');
    tmp.Add(value); // OpenAPI "simple" style is just a CSV
    tmp.AddDirect(#13, #10); // use CR+LF in HTTP headers
    inc(p);
  end;
  if tmp.Size <> 0 then
    tmp.Done(OutHeaders);
end;

function HeadersEncode(const NameValuePairs: array of const): RawUtf8;
begin
  result := '';
  if (high(NameValuePairs) >= 0) and
     (high(NameValuePairs) and 1 = 1) then // n should be = 1,3,5,7,..
    DoHeadersEncode(NameValuePairs, result);
end;

procedure TJsonClientAbstract.Request(const Method, ActionFmt: RawUtf8;
  const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
  const CustomError: TOnJsonClientError);
begin
  RttiRequest(Method,
    UrlEncodeFull(ActionFmt, ActionArgs, QueryNameValueParams, fUrlEncoder),
    HeadersEncode(HeaderNameValueParams),
    nil, nil, nil, nil, CustomError);
end;

procedure TJsonClientAbstract.Request(const Method, ActionFmt: RawUtf8;
  const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
  var Res; ResInfo: PRttiInfo; const CustomError: TOnJsonClientError);
begin
  RttiRequest(Method,
    UrlEncodeFull(ActionFmt, ActionArgs, QueryNameValueParams, fUrlEncoder),
    HeadersEncode(HeaderNameValueParams),
    nil, @Res, nil, ResInfo, CustomError);
end;

procedure TJsonClientAbstract.Request(const Method, ActionFmt: RawUtf8;
  const ActionArgs, QueryNameValueParams, HeaderNameValueParams: array of const;
  const Payload; var Res; PayloadInfo, ResInfo: PRttiInfo;
  const CustomError: TOnJsonClientError);
begin
  RttiRequest(Method,
    UrlEncodeFull(ActionFmt, ActionArgs, QueryNameValueParams, fUrlEncoder),
    HeadersEncode(HeaderNameValueParams),
    @Payload, @Res, PayloadInfo, ResInfo, CustomError);
end;


{ TJsonClient }

constructor TJsonClient.Create(const aServerAddress, aBaseUri: RawUtf8;
  aKeepAlive: integer);
begin
  inherited Create;
  if not fServerUri.From(aServerAddress) then
    EJsonClient.RaiseUtf8('Unexpected %.Create(%)', [self, aServerAddress]);
  fBaseUri := IncludeTrailingUriDelimiter(aBaseUri);
  fKeepAlive := aKeepAlive;
  fHttp := TSimpleHttpClient.Create;
  fDefaultHeaders := ('Accept: ' + JSON_CONTENT_TYPE);
  fOptions := [jcoParseTolerant, jcoHttpErrorRaise];
  fUrlEncoder := [ueEncodeNames, ueSkipVoidString];
end;

destructor TJsonClient.Destroy;
begin
  inherited Destroy;
end;

procedure TJsonClient.SetInHeaders;
begin
  fInHeaders := fDefaultHeaders;
  AppendLine(fInHeaders, ['Cookie: ', fCookies]);
end;

function TJsonClient.GetCookies: RawUtf8;
begin
  result := fCookies;
end;

procedure TJsonClient.SetCookies(const Value: RawUtf8);
begin
  fCookies := Value;
  SetInHeaders;
end;

procedure TJsonClient.SetDefaultHeaders(const Value: RawUtf8);
begin
  fDefaultHeaders := Value;
  SetInHeaders;
end;

function TJsonClient.GetDefaultHeaders: RawUtf8;
begin
  result := fDefaultHeaders;
end;

procedure TJsonClient.AddDefaultHeader(const Name, Value: RawUtf8);
begin
  AppendLine(fDefaultHeaders, [Name, ': ', Value]);
  SetInHeaders;
end;

function TJsonClient.HttpOptions: PHttpRequestExtendedOptions;
begin
  result := fHttp.Options;
end;

function TJsonClient.Http: IHttpClient;
begin
  result := fHttp;
end;

function TJsonClient.Connected: string;
begin
  fSafe.Lock;
  try
    result := fHttp.Connected(fServerUri);
  finally
    fSafe.UnLock;
  end;
end;

procedure TJsonClient.RawRequest(const Method, Action,
  InType, InBody, InHeaders: RawUtf8; var Response: TJsonResponse);
var
  a, t, b, h: RawUtf8;
begin
  // prepare input paramteters
  h := fInHeaders; // pre-computed from Cookies and DefaultHeaders properties
  if InHeaders <> '' then
    AppendLine(h, [InHeaders]);
  if (InBody <> '') and
     not HttpMethodWithNoBody(Method) then
  begin
    b := InBody;
    t := InType;
    if t = '' then
      t := JSON_CONTENT_TYPE_VAR;
  end;
  a := Action;
  while (a <> '') and
        (a[1] = '/') do
    delete(a, 1, 1); // avoid dual 'base//action' URI
  Response.Init;
  Response.Method := Method;
  fSafe.Lock; // blocking thread-safe HTTP request
  try
    fServerUri.Address := fBaseUri + a;
    Response.Url := fServerUri.Root; // excluding ?parameters=...
    fHttp.Request(fServerUri, Method, h, b, t, fKeepAlive);
    Response.Status := fHttp.Status;
    Response.Content := fHttp.Body;
  finally
    fServerUri.Address := ''; // reset
    fSafe.UnLock;
  end;
end;


{ ************** Cached HTTP Connection to a Remote Server }

{ THttpRequestCached }

constructor THttpRequestCached.Create(const aUri: RawUtf8; aKeepAliveSeconds,
  aTimeOutSeconds: integer; const aToken: RawUtf8; aOnlyUseClientSocket: boolean);
begin
  inherited Create; // may have been overriden
  fKeepAlive := aKeepAliveSeconds * 1000;
  fOnlyUseClientSocket := aOnlyUseClientSocket;
  if aTimeOutSeconds > 0 then // 0 means no cache
    fCache := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
      TypeInfo(THttpRequestCacheDynArray), true, aTimeOutSeconds);
  fClient := TSimpleHttpClient.Create(fOnlyUseClientSocket);
  if aUri <> '' then
    if not LoadFromUri(aUri, aToken) then
      ESynException.RaiseUtf8('%.Create: invalid aUri=%', [self, aUri]);
end;

procedure THttpRequestCached.Clear;
begin
  fClient.Close;
  if fCache <> nil then
    fCache.DeleteAll;
  fUri.Clear;
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
  if (fCache <> nil) and
     fCache.FindAndCopy(aAddress, cache) then
    FormatUtf8('If-None-Match: %', [cache.Tag], headin);
  fUri.Address := aAddress;
  status := fClient.Request(fUri, 'GET', headin{%H-}, '', '', fKeepAlive);
  modified := true;
  case status of
    HTTP_SUCCESS:
      if (fCache <> nil) and
         FindNameValue(fClient.Headers, 'ETAG:', cache.Tag) and
         (cache.Tag <> '')  then
      begin
        cache.Content := result;
        fCache.AddOrUpdate(aAddress, cache);
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
     not fUri.From(aUri) then
    exit;
  Clear;
  if aToken <> '' then
    fClient.Options^.AuthorizeBearer(aToken);
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
  if uri.From(aUri) and // has a valid uri.Server field
     (uri.UriScheme in HTTP_SCHEME) then
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
  else if uri.Scheme = '' then
    result := '' // a clearly invalid URI
  else // try custom RegisterNetClientProtocol()
    result := DoHttpGet(THttpClientSocket.OpenUri(uri, aUri, '', timeout, nil),
      uri.Address, inHeaders, outHeaders, outStatus);
  {$ifdef LINUX_RAWDEBUGVOIDHTTPGET}
  if result = '' then
    writeln('HttpGet returned VOID for ',uri.server,':',uri.Port,' ',uri.Address);
  {$endif LINUX_RAWDEBUGVOIDHTTPGET}
end;

function HttpGetWeak(const aUri: RawUtf8; const aLocalFile: TFileName;
  outStatus: PInteger): RawByteString;
var
  status: integer;
begin
  if aLocalFile <> '' then // try from local cache
  begin
    result := StringFromFile(aLocalFile); // useful e.g. during regression tests
    if result <> '' then
    begin
      if outStatus <> nil then
        outStatus^ := HTTP_SUCCESS; // emulates proper download
      exit;
    end;
  end;
  result := HttpGet(aUri, {inhead=}'', {outhead=}nil, {notsock=}false,
    @status, {timeout=}0, {forcesocket=}false, {ignorecerterror=}true);
  if outStatus <> nil then
    outStatus^ := status;
  if (status = HTTP_SUCCESS) and
     (aLocalFile <> '') and
     (result <> '') then
    FileFromString(result, aLocalFile);
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
  const Headers, TextCharSet: RawUtf8; TLS, TLSIgnoreCertError: boolean): boolean;
begin
  result := SendEmail(
    Server.Host, From, CsvDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet,
    TLS or (Server.Port = '465') or (Server.Port = '587'), TLSIgnoreCertError);
end;

function SendEmail(const Server, From, CsvDest, Subject: RawUtf8;
  const Text: RawByteString; const Headers, User, Pass, Port, TextCharSet: RawUtf8;
  TLS, TLSIgnoreCertError: boolean): boolean;
var
  sock: TCrtSocket;

  procedure Expect(const answer: RawUtf8);
  var
    res: RawUtf8;
  begin
    repeat
      sock.SockRecvLn(res);
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
    sock.SockSend(Command);
    sock.SockSendFlush;
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
  sock := SocketOpen(Server, Port, TLS, nil, nil, TLSIgnoreCertError);
  if sock <> nil then
  try
    sock.CreateSockIn; // we use SockIn for buffered SockRecvLn() in Expect()
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
    sock.SockSendLine(['MAIL FROM:<', From, '>']);
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
        Join([#13#10'To: ', rec], ToList)
      else
        Append(ToList, ', ', rec);
    until P = nil;
    Exec('DATA', '354');
    sock.SockSendLine([
      'Subject: ', Subject, #13#10 +
      'From: ', From, ToList]);
    head := trimU(Headers);
    if (TextCharSet <> '') or
       (head = '') then
      sock.SockSend([
        'Content-Type: text/plain;charset=', TextCharSet, #13#10 +
        'Content-Transfer-Encoding: 8bit']);
    if head <> '' then
      sock.SockSendHeaders(head); // normalizing CRLF
    sock.SockSendCRLF;            // end of headers
    sock.SockSend(Text);
    Exec('.', '25');
    Exec('QUIT', '22');
    result := true;
  finally
    sock.Free;
  end;
end;

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
    procedure Force(const Host, IP: RawUtf8);
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

procedure TNewSocketAddressCache.Force(const Host, IP: RawUtf8);
var
  addr: TNetAddr;
begin
  if not NetIsIP4(pointer(IP)) or
     not addr.SetFromIP4(IP, true) then
    exit;
  fData.DeleteDeprecated;   // flush cache only when we may need some new space
  fData.AddOrUpdate(Host, addr); // force change
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

