/// HTTP Server With Proxy and Cache
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.proxy;

{
  *****************************************************************************

   HTTP Server With Proxy and Cache
   - THttpProxyServer HTTP Server With Proxy and Cache

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.threads,
  mormot.core.search,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.zip,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server,
  mormot.net.async;


{ ******************** THttpProxyServer HTTP Server With Proxy and Cache }

type
  /// the result of THttpProxyMem.FromUri
  THttpProxyCacheKind = set of (
    pckIgnore,
    pckForce);

  /// define the caching settings of content for THttpProxyServer
  // - set the memory cache settings if used as exact THttpProxyMem class
  THttpProxyMem = class(TSynPersistent)
  protected
    fMaxSizeKB: integer;
    fTimeoutSec: integer;
    fIgnoreCsv, fForceCsv: RawUtf8;
    fIgnore, fForce: TUriMatch; // parsed fIgnoreCsv, fForceCsv
  public
    /// setup the default values of this cache
    constructor Create; override;
    /// check the IgnoreCsv and ForceCSv properties against a given URI
    function FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
  published
    /// size (in kilobytes) below which the file should be included in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property MaxSizeKB: integer
      read fMaxSizeKB write fMaxSizeKB default -1;
    /// how many seconds this file should remain in this cache
    // - default -1 will use the main THttpProxyServerSettings value
    property TimeoutSec: integer
      read fTimeoutSec write fTimeoutSec default -1;
    /// CSV list of GLOB file names to be excluded to this cache
    // - e.g. '*.changelog,*.tmp'
    property IgnoreCsv: RawUtf8
      read fIgnoreCsv write fIgnoreCsv;
    /// CSV list of GLOB file names to be included in this cache even if
    // its size does not match MaxSize
    // - e.g. '*.xml'
    property ForceCsv: RawUtf8
      read fForceCsv write fForceCsv;
  end;

  /// define disk cache settings of content for THttpProxyServer
  THttpProxyDisk = class(THttpProxyMem)
  protected
    fPath: TFileName;
  published
    /// default '' will use the main THttpProxyServerSettings value
    property Path: TFileName
      read fPath write fPath;
  end;

  THttpProxyUrl = class;

  /// THttpProxyUrl.OnRemoteClient callback signature
  // - could be used e.g. for interaction, or to customize TLS or proxy options
  TOnHttpProxyUrlClient = procedure(Sender: THttpProxyUrl; const Uri: TUri;
    var Options: THttpRequestExtendedOptions) of object;

  /// set of THttpProxyUrl.Settings.Options items, to customize a specific URI
  // - hpoNoSubFolder disable access to any sub-folder within this URI
  // - hpoNoFolderHtmlIndex disable the HTML index generation at folder level
  // - hpoPublishMd5/psoPublishSha1/psoPublishSha256 enable hash content
  // generation on server side with .md5/.sha1/.sha256 extension on a resource
  // - hpoDisable304 disable "if-none-match:" / "if-modified-since:" headers
  // default support as efficient 304 HTTP_NOTMODIFIED response
  // - hpoClientCacheSubFolder could be used with a lot of cached files, to
  // generate sub-folders following the first hash nibble (0..9/a..z)
  // - hpoClientHeadNoRefresh won't refresh the HEAD timeout when a resource
  // is accessed, so that the cache is always flushed after HttpHeadCacheSec
  // - hpoClientIgnoreTlsError will ignore any HTTPS issue
  // - hpoClientAlllowWinApi will be used for THttpProxyUrl.RemoteClientHead()
  // - hpoClientAllowRedirect enable 30x HTTP redirections (disabled by default)
  // - hpoClientNoCacheDirect disable caching of dynamic pages < HttpDirectGetKB
  // - hpoClientLowerCaseUri will force remote http://... to be in lowercase
  // - hpoClientNormalizeCaseHash to compute the local hash from uppercase URI
  // - hpoNoXProxyName will disable our custom 'X-Proxy-Name: xxxx' header
  // - hpNoXCache will purge any X-Cache: X-Served-By: Via: Age: headers
  THttpProxyUrlOption = (
    hpoNoSubFolder,
    hpoNoFolderHtmlIndex,
    hpoDisableFolderHtmlIndexCache,
    hpoPublishMd5,
    hpoPublishSha1,
    hpoPublishSha256,
    hpoDisable304,
    hpoClientCacheSubFolder,
    hpoClientHeadNoRefresh,
    hpoClientIgnoreTlsError,
    hpoClientAlllowWinApi,
    hpoClientAllowRedirect,
    hpoClientNoCacheDirect,
    hpoClientLowerCaseUri,
    hpoClientNormalizeCaseHash,
    hpoNoXProxyName,
    hpNoXCache);
  /// store THttpProxyUrl.Settings options for a given URI
  THttpProxyUrlOptions = set of THttpProxyUrlOption;

  THttpProxyUrlSettings = class;

  /// callback function prototype for THttpProxyServerSettings.AddEvent()
  TOnHttpProxyServerRequest = function(Ctxt: THttpServerRequestAbstract;
    const Uri: TUriMatchName; Met: TUriRouterMethod;
    ProxyUrl: THttpProxyUrl): cardinal of object;

  /// define one URL content / THttpProxyUrl setting for THttpProxyServer
  THttpProxyUrlSettings = class(TSynAutoCreateFields)
  protected
    fUrl, fSource: RawUtf8;
    fDisabled: boolean;
    fOptions: THttpProxyUrlOptions;
    fMethods: TUriRouterMethods;
    fCacheControlMaxAgeSec: integer;
    fHttpKeepAlive, fHttpHeadCacheSec, fHttpDirectGetKB, fHttpLimitPerSecond: integer;
    fMemCache: THttpProxyMem; // owned as TSynAutoCreateFields
    fDiskCache: THttpProxyDisk;
    fRejectCsv: RawUtf8;
    fLocalFolder: TFileName;
    fOnRemoteClient: TOnHttpProxyUrlClient;
    fOnRequest: TOnHttpProxyServerRequest;
  public
    /// setup the default values of this URL
    constructor Create; override;
    /// allow to customize the processing options in a fluent calling interface
    function SetOptions(aOptions: THttpProxyUrlOptions): THttpProxyUrlSettings;
    /// optional event handler when a remote URI connection is instantiated
    property OnRemoteClient: TOnHttpProxyUrlClient
      read fOnRemoteClient write fOnRemoteClient;
    /// optional event handler when a callback is to be called
    property OnRequest: TOnHttpProxyServerRequest
      read fOnRequest write fOnRequest;
  published
    /// this source won't be processed if this property is set to true
    property Disabled: boolean
      read fDisabled write fDisabled;
    /// which methods are applied to the local Source folder or relayed to
    // the Remote server
    // - equals by default [urmGet, urmHead]
    property Methods: TUriRouterMethods
      read fMethods write fMethods;
    /// refined the process of this URI definition
    property Options: THttpProxyUrlOptions
      read fOptions write fOptions;
    /// the local URI prefix to route the main HTTP(S) server of this instance
    // - a typical value is e.g. 'debian' for 'http://ftp.debian.org/debian'
    property Url: RawUtf8
      read fUrl write fUrl;
    /// a local folder name or remote origin URL to ask
    // - if Source is a local folder (e.g. 'd:/mysite' or '/var/www/mysite'),
    // the Url prefix chars will be removed from the client request, then used
    // to locate the file to be served within this local folder
    // - if Source is a remote URI (like http://....), the Url prefix chars will
    // be removed from the client request, then appended to this remote URI for
    // remote proxy with caching, which is e.g. 'http://ftp.debian.org/debian'
    // 'or http://security.debian.org/debian-security' matching Local 'debian' or
    // 'debian-security' prefixes, to compute a source remote URI
    property Source: RawUtf8
      read fSource write fSource;
    /// optional bandwidth limitation of Source http:// remote URIs download
    // - would trigger a TStreamRedirect.LimitPerSecond additional process
    property HttpLimitPerSecond: integer
      read fHttpLimitPerSecond write fHttpLimitPerSecond;
    /// how many seconds HEAD requests could be cached in memory
    // - 0 would disable head caching
    // - default is 60, i.e. cache HEAD response for 1 minute
    property HttpHeadCacheSec: integer
      read fHttpHeadCacheSec write fHttpHeadCacheSec;
    /// seconds of HTTP keep alive source - default to 30
    property HttpKeepAlive: integer
      read fHttpKeepAlive write fHttpKeepAlive;
    /// below this KB size, will make a blocking GET and no background proxy
    // - default is 16 KB
    property HttpDirectGetKB: integer
      read fHttpDirectGetKB write fHttpDirectGetKB;
    /// CSV list of GLOB file or directly names to be rejected as not found
    // - e.g. '*.secret'
    property RejectCsv: RawUtf8
      read fRejectCsv write fRejectCsv;
    /// support optional "Cache-Control: max-age=..." header timeout value
    // - default 0 value will disable this header transmission
    // - to be used in conjunction with the hpoIfModifiedSince option
    property CacheControlMaxAgeSec: integer
      read fCacheControlMaxAgeSec write fCacheControlMaxAgeSec;
    /// overwrite the main MemCache setting to tune in-memory caching
    // - can be used for both local file or remote URI lookups
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// overwrite the main DiskCache setting to tune on-disk caching
    // - disk cache is available only for remote URI lookups, i.e. if Source
    // is defined as 'http://...' and not as a local file
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
  end;
  /// store a dynamic array of THttpProxyUrlSettings instances
  THttpProxyUrlSettingsObjArray = array of THttpProxyUrlSettings;

  /// the available high-level options for THttpProxyServerMainSettings
  // - psoLogVerbose could be used to debug a server in production
  // - psoExcludeDateHeader won't include the default "Date: ..." HTTP header
  // - psoHttpsSelfSigned will enable HTTPS with a self-signed certificate
  // - psoReusePort will set SO_REUSEPORT on POSIX, to bind several servers
  // - psoEnableLogging enable an associated THttpServerGeneric.Logger instance
  // - psoRejectBotUserAgent identifies and rejects Bots via IsHttpUserAgentBot()
  // - psoBan40xIP will reject any IP for a few seconds after a 4xx error code
  // - psoDisableMemCache will globally disable all MemCache settings
  // - psoDisableGzip won't use runtime gzip content compression
  THttpProxyServerOption = (
    psoLogVerbose,
    psoExcludeDateHeader,
    psoHttpsSelfSigned,
    psoReusePort,
    psoEnableLogging,
    psoRejectBotUserAgent,
    psoBan40xIP,
    psoDisableMemCache,
    psoDisableGzip);

  /// a set of available options for THttpProxyServerMainSettings
  THttpProxyServerOptions = set of THttpProxyServerOption;

  /// define the THttpProxyServer HTTP(S) server settings
  THttpProxyServerMainSettings = class(TSynAutoCreateFields)
  protected
    fPort: RawUtf8;
    fOptions: THttpProxyServerOptions;
    fThreadCount: integer;
    fCertificateFile: TFileName;
    fCACertificatesFile: TFileName;
    fPrivateKeyFile: TFileName;
    fPrivateKeyPassword: SpiUtf8;
    fServerName: RawUtf8;
    fLog: THttpLoggerSettings;
    fFaviconFile: TFileName;
  public
    /// initialize the default settings
    constructor Create; override;
    /// assign the HTTPS/TLS settings to a context
    function SetupTls(var tls: TNetTlsContext): boolean;
  published
    /// the local port used for HTTP/HTTPS content delivery
    // - is '8098' by default (THttpPeerCache uses 8099), unassigned by IANA
    // - you can bind to a specific 'IP:port' if needed
    property Port: RawUtf8
      read fPort write fPort;
    /// customize this proxy cache HTTP/HTTPS process
    property Options: THttpProxyServerOptions
      read fOptions write fOptions;
    /// the number of threads of the HTTP/HTTPS content delivery
    // - default value is 4 sub-threads which is enough to scale well if no
    // content is to be generated
    property ThreadCount: integer
      read fThreadCount write fThreadCount;
    /// custom log settings for the psoEnableLogging option
    // - e.g. to override default LOGFORMAT_COMBINED output, or rotation settings
    property Log: THttpLoggerSettings
      read fLog write fLog;
    /// optional HTTPS certificate file name
    // - should also set PrivateKeyFile and PrivateKeyPassword
    property CertificateFile: TFileName
      read fCertificateFile write fCertificateFile;
    /// optional HTTPS private key file name
    property PrivateKeyFile: TFileName
      read fPrivateKeyFile write fPrivateKeyFile;
    /// optional HTTPS private key file password
    property PrivateKeyPassword: SpiUtf8
      read fPrivateKeyPassword write fPrivateKeyPassword;
    /// optional HTTPS certificates authorities file
    property CACertificatesFile: TFileName
      read fCACertificatesFile write fCACertificatesFile;
    /// optional alternate favicon.ico file name
    property FaviconFile: TFileName
      read fFaviconFile write fFaviconFile;
    /// optional Server name for HTTP/HTTPS
    // - to overwrite the default value set by the framework e.g. 'mORMot2 (Linux)'
    property ServerName: RawUtf8
      read fServerName write fServerName;
  end;

  /// hold one or several remote content source(s) process for THttpProxyServer
  THttpProxyUrlObjArray = array of THttpProxyUrl;

  THttpProxyServer = class;

  /// define the THttpProxyServer forward proxy process
  THttpProxyServerSettings = class(TSynAutoCreateFields)
  protected
    fServer: THttpProxyServerMainSettings;
    fMemCache: THttpProxyMem;
    fDiskCache: THttpProxyDisk;
    fUrl: THttpProxyUrlSettingsObjArray;
  public
    /// initialize the default settings
    constructor Create; override;
    /// append and own a given THttpProxyUrlSettings definition at runtime
    // - this instance will be stored and owned in Url[].Settings array
    function AddUrl(one: THttpProxyUrlSettings): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition to serve a local static folder
    // - if optional ExceptionClass is supplied, the local folder should exist
    // - this instance will be stored and owned in Url[].Settings array
    function AddFolder(const folder: TFileName; const uri: RawUtf8 = '';
      RaiseExceptionOnNonExistingFolder: ESynExceptionClass = nil): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition as remote URI proxy
    function AddProxy(const remoteuri: RawUtf8; const localuri: RawUtf8 = ''): THttpProxyUrlSettings;
    /// create a THttpProxyUrlSettings definition for using a callback
    function AddEvent(const request: TOnHttpProxyServerRequest;
      const uri: RawUtf8 = ''): THttpProxyUrlSettings;
    /// load URI definitions from local *.json files (also supports JSON5 or HJson)
    // - returns the number of added THttpProxyUrl instances into Url[]
    function AddFromFiles(const settingsfolder: TFileName;
      const mask: TFileName = '*.json'): integer;
  published
    /// define the HTTP/HTTPS server configuration
    property Server: THttpProxyServerMainSettings
      read fServer write fServer;
    /// default in-memory cache settings
    // - is set to MaxSize = 4KB and TimeoutSec = 5 minutes
    // - can be overriden by Url[].MemCache property
    property MemCache: THttpProxyMem
      read fMemCache write fMemCache;
    /// default on-disk local cache settings
    // - can be overriden by Url[].DiskCache property
    property DiskCache: THttpProxyDisk
      read fDiskCache write fDiskCache;
    /// access the remote content sources settings
    // - owned as a TSynAutoCreateFields dynarray
    property Url: THttpProxyUrlSettingsObjArray
      read fUrl;
  end;

  /// define how a THttpProxyUrl definition is implemented
  THttpProxySource = (
    hpsUndefined,
    hpsLocalFolder,
    hpsRemoteUri,
    hpsEvent);
  THttpProxySources = set of THttpProxySource;

  /// internal HEAD request cache entry for a given resource (80-bytes on CPU64)
  THeadCache = packed record
    /// origin headers ready to be sent back to the client with the file content
    PurgedHeaders: RawUtf8;
    /// resource full size, e.g. from headers Content-Length
    Size: Int64;
    /// resource remote timestamp, e.g. from headers Last-Modified
    TimeMS: TUnixMSTime;
    /// HTTP code number from remote server, e.g. 200, 206, 301 or 404
    Status: word;
    /// Base-32 encoded URI + etag/lastmod from Headers
    HashB32: TShort32;
    /// THashDigest-compatible 20-bytes truncated SHA-256(Uri+etag/lastmod)
    HashDigest: THashAlgo;
    Hash160: THash160;
  end;
  THeadCaches = array of THeadCache;

  /// process one remote content source for THttpProxyServer
  THttpProxyUrl = class(TSynPersistent)
  protected
    fSettings: THttpProxyUrlSettings;
    fOwner: THttpProxyServer;
    fSource: THttpProxySource;
    fAlgos: THashAlgos; // may be in [hfMD5, hfSha1, hfSha256] range
    fRemoteUri: TUri;
    fHeadCache: TSynDictionary; // THash160 / THeadCache
    fMemCache: TSynDictionary;  // THash160 / Content:RawByteString
    fHashCache: TSynDictionary; // name:RawUtf8 / hash[fAlgos]:TRawUtf8DynArray
    fReject: TUriMatch;
    fRemoteClient: IHttpClient;
    fOsSafe: TOSLightLock; // non-reentrant lock for fRemoteClient + file access
    procedure SetRemoteClient;
    procedure BackgroundGet(Sender: TObject);
  public
    /// initialize this instance
    constructor Create(aSettings: THttpProxyUrlSettings;
      aOwner: THttpProxyServer); reintroduce;
    /// finalize this instance and its associated Settings
    destructor Destroy; override;
    /// return a local file content with proper fMemCache support
    function ReturnFile(ctxt: THttpServerRequestAbstract; const hash: THash160;
      const filename: TFileName; const uri: TUriMatchName; var size: Int64;
      lastmod: TUnixMSTime; var loginfo: PUtf8Char; canbecached: boolean = true): cardinal;
    /// compute the (probably cached) hash of a given URI resource
    function ReturnHash(ctxt: THttpServerRequestAbstract;
      const name: RawUtf8; var fn: TFileName): integer;
    /// perform a HTTP HEAD on the remote proxy URI using a shared connection
    procedure RemoteClientHead(const uri: TUri; const hash: THash160;
      var cache: THeadCache);
    /// perform a HTTP GET on the remote proxy URI using a shared connection
    function RemoteClientGet(const uri: TUri): RawByteString;
  published
    /// how this URI is implemented
    property Source: THttpProxySource
      read fSource;
    /// access to the associated (and owned) settings for this URI
    property Settings: THttpProxyUrlSettings
      read fSettings;
  end;

  /// Exception class raised by THttpProxyServer
  EHttpProxyServer = class(ESynException);

  /// implements a HTTP server with forward proxy and caching
  THttpProxyServer = class(TSynPersistent)
  protected
    fSettings: THttpProxyServerSettings;
    fUrl: THttpProxyUrlObjArray;
    fLog: TSynLogClass;
    fFlags: set of (fUrlSet, fSettingsOwned, fHasLog, fHasCacheClean);
    fSources: THttpProxySources;
    fUrlOptions: THttpProxyUrlOptions; // consolidated from all fUrl[].Options
    fTempFilesTix: cardinal;
    fServer: THttpAsyncServer;
    fGC: TObjectDynArray;
    fPartials: THttpPartials;
    fBackgroundCount: integer; // detached workers still referencing this proxy
    procedure BackgroundFinished(Sender: TObject);
    function SetupTls(var tls: TNetTlsContext): boolean; virtual;
    procedure AfterServerStarted; virtual;
    procedure OnIdle(Sender: TObject; NowTix: Int64);
    function OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
    function OnGetHeadLocalFolder(Ctxt: THttpServerRequest; const Uri: TUriMatchName): cardinal;
    function OnGetHeadRemoteUri(Ctxt: THttpServerRequest; var Uri: TUriMatchName): cardinal;
    procedure OnBackgroundDeleteDeprecated(Sender: TObject);
  public
    /// initialize this forward proxy instance
    // - the supplied aSettings should be owned by the caller (e.g from a main
    // settings class instance) - if nil is supplied, this instance will setup
    // and own its own instance
    constructor Create(aSettings: THttpProxyServerSettings); reintroduce; virtual;
    /// finalize this class instance
    destructor Destroy; override;
    /// actually start the HTTP/HTTPS server
    // - you can specify a private key password if not already in Settings
    // - may raise some exceptions if the HTTP server cannot be started
    procedure Start(const aPrivateKeyPassword: SpiUtf8 = '');
    /// finalize the HTTP/HTTPS server
    procedure Stop;
    /// the local HTTP(S) asynchronous server
    property Server: THttpAsyncServer
      read fServer;
    /// the kind of definitions stored in Settings.Uri[]
    property Sources: THttpProxySources
      read fSources;
    /// access to the used settings
    property Settings: THttpProxyServerSettings
      read fSettings;
    /// access to the defined source URIs process, set between Start/Stop calls
    property Url: THttpProxyUrlObjArray
      read fUrl;
  end;

function ToText(hps: THttpProxySource): PShortString; overload;


implementation


{ ******************** THttpProxyServer HTTP Proxy with Cache }

{ THttpProxyMem }

constructor THttpProxyMem.Create;
begin
  inherited Create;
  fMaxSizeKB := -1; // use main THttpProxyServerSettings value
  fTimeoutSec := -1;
end;

function THttpProxyMem.FromUri(const uri: TUriMatchName): THttpProxyCacheKind;
begin
  result := [];
  if (fForceCsv <> '') and
     fForce.Check(fForceCsv, uri, PathCaseInsensitive) then
    include(result, pckForce);
  if (fIgnoreCsv <> '') and
     fIgnore.Check(fIgnoreCsv, uri, PathCaseInsensitive) then
    include(result, pckIgnore);
end;


{ THttpProxyUrlSettings }

constructor THttpProxyUrlSettings.Create;
begin
  inherited Create;
  fMethods := [urmGet, urmHead];
  fOptions := [];
  fHttpKeepAlive := 30;      // 30 seconds
  fHttpHeadCacheSec := 60;   // 1 minute
  fHttpDirectGetKB := 16;    // 16 KB
end;

function THttpProxyUrlSettings.SetOptions(aOptions: THttpProxyUrlOptions): THttpProxyUrlSettings;
begin
  result := self;
  if result <> nil then
    fOptions := fOptions + aOptions;
end;


{ THttpProxyUrl }

constructor THttpProxyUrl.Create(aSettings: THttpProxyUrlSettings;
  aOwner: THttpProxyServer);
begin
  inherited Create;
  fOsSafe.Init;
  fSettings := aSettings;
  fOwner := aOwner;
end;

destructor THttpProxyUrl.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fMemCache);
  FreeAndNil(fHashCache);
  FreeAndNil(fHeadCache);
  // fSettings are owned by THttpProxyServerSettings.Url[]
  fOsSafe.Done; // mandatory for TOSLightLock
end;

function THttpProxyUrl.ReturnHash(ctxt: THttpServerRequestAbstract;
  const name: RawUtf8; var fn: TFileName): integer;
var
  i: PtrInt;
  a, hf: THashAlgo;
  hashes: TRawUtf8DynArray;
  ext: PUtf8Char;
begin
  result := HTTP_NOTFOUND;
  ext := ExtractExtP(name, {withoutdot:}true);
  if ext = nil then
    exit;
  case PCardinal(ext)^ of
    MD5_LO:
      hf := hfMd5;
    SHA_LO + ord('1') shl 24:
      hf := hfSHA1;
    SHA_LO + ord('2') shl 24:
      hf := hfSHA256;
  else
    exit;
  end;
  if not (hf in fAlgos) then
    exit;
  if not fHashCache.FindAndCopy(name, hashes) then
  begin
    // first time this hash is requested: try to load and compute now
    i := length(fn);
    while i > 0 do
      if fn[i] = '.' then
        break // remove the .md5/.sha1/.sha256 file extension
      else
        dec(i);
    if i = 0 then
      exit;
    SetLength(fn, i - 1); // xxx.ext.md5 -> xxx.ext
    hashes := HashFileRaw(fn, fAlgos);
    if hashes = nil then
      exit; // no such file
    fHashCache.Add(name, hashes); // first insertion wins on contention
  end;
  // return the pre-computed hash of this file
  i := 0;
  for a := low(a) to high(a) do
    if a in fAlgos then
      if a = hf then
      begin
        ctxt.OutContent := hashes[i];
        ctxt.OutContentType := TEXT_CONTENT_TYPE;
        result := HTTP_SUCCESS;
        exit;
      end
      else
        inc(i);
end;

procedure THttpProxyUrl.SetRemoteClient;
var
  client: TSimpleHttpClient;
begin
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientHead: connect to %:%',
    [fRemoteUri.Server, fRemoteUri.Port], self);
  client := TSimpleHttpClient.Create(
    not (hpoClientAlllowWinApi in fSettings.Options));
  fRemoteClient := client;
  if not (hpoClientAllowRedirect in fSettings.Options) then
    client.Options^.RedirectMax := 0; // no automatic redirection by default
  if hpoClientIgnoreTlsError in fSettings.Options then
    client.Options^.TLS.IgnoreCertificateErrors := true;
  if Assigned(fSettings.OnRemoteClient) then
    fSettings.OnRemoteClient(self, fRemoteUri, client.Options^);
  if psoLogVerbose in fOwner.fSettings.Server.Options then
    client.OnLog := TSynLog.DoLog;
  // TODO: on scaling issue, maintain a pool of client connections
end;

const
  PURGED_PROXY =
    'CONTENT-LENGTH:|CONTENT-RANGE:|CONTENT-ENCODING:|CONNECTION:|' +
    'KEEP-ALIVE:|DATE:|TE:|TRAILER:|X-TIMER:|';
  PURGED: array[{=hpNoXCache}boolean] of PUtf8Char = (
    PURGED_PROXY,
    PURGED_PROXY + 'VIA:|X-CACHE|X-SERVED-BY:|X-CLACKS-|AGE:|');

procedure THttpProxyUrl.RemoteClientHead(const uri: TUri; const hash: THash160;
  var cache: THeadCache);
var
  keepalive, status: integer;
  d: TUnixTime; // converted to cache.TimeMS: TUnixMSTime
begin // this method is protected by fOsSafe.Lock
  // initialize the shared connection for HEAD and smallest GET
  if fRemoteClient = nil then
    SetRemoteClient;
  // always first try with a clean HEAD request
  keepalive := fSettings.HttpKeepAlive * MilliSecsPerSec;
  status := fRemoteClient.Request(uri, 'HEAD', '', '', '', keepalive);
  if StatusCodeIsSuccess(status) then // 2xx..3xx range
    if HttpRequestLength(pointer(fRemoteClient.Headers)) = nil then
      // server has no length: try range GET trick for a 200 response
      if status = HTTP_SUCCESS then // but return 204 NoContent or 3xx Redirect
      begin
        // Apache+Varnish may return 'Content-Range: bytes 0-0/*' for text/html
        status := fRemoteClient.Request(uri, 'GET',
          'Range: bytes=0-0', '', '', keepalive);
        if status = HTTP_PARTIALCONTENT then // 'Range:' triggers 206
          status := HTTP_SUCCESS;            // but we emulate a regular 200
      end;
  // set Headers and extract Size + TimeMS fields and base-32 encoded filename
  cache.Status := status;
  cache.Size := 0;
  d := 0;
  if StatusCodeIsSuccess(status) then // 2xx..3xx range
    GetHeaderInfo(fRemoteClient.Headers, cache.Size, d);
  cache.TimeMS := d * MilliSecsPerSec;
  cache.HashDigest := hfSHA256_160; // SHA-256 truncated to 160-bit
  HttpRequestHashBase32(Uri, @cache.HashB32, pointer(fRemoteClient.Headers),
    @cache.Hash160, hpoClientNormalizeCaseHash in fSettings.Options);
  cache.PurgedHeaders := PurgeHeaders(fRemoteClient.Headers, false,
    PURGED[hpNoXCache in fSettings.Options]);
  // optionnaly cache the headers and its decoded fields
  if Assigned(fHeadCache) and
     (status < HTTP_SERVERERROR) and  // retry on pure 5xx or 666 (client)
     (cache.Size >= 0) then           // only store if the size was known
    fHeadCache.Add(hash, cache);
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientHead(%)=% size=% lastmod=% as % %',
    [uri.Address, status, cache.Size, d, cache.HashB32,
     fRemoteClient.LastError], self);
end;

function THttpProxyUrl.RemoteClientGet(const uri: TUri): RawByteString;
var
  status: integer;
begin // this method is protected by fOsSafe.Lock
  FastAssignNew(result);
  status := fRemoteClient.Request(uri, 'GET', '', '', '',
       fSettings.HttpKeepAlive * MilliSecsPerSec);
  if status in HTTP_GET_OK then // 200/204/206
    result := fRemoteClient.Body;
  fOwner.fLog.Add.Log(sllTrace, 'RemoteClientGet(%)=% size=%',
    [uri.Address, status, length(result)], self);
end;

type
  // state machine for HEAD + GET request methods to the remote server
  {$ifdef USERECORDWITHMETHODS}
  TStartProxyRequest = record
  {$else}
  TStartProxyRequest = object
  {$endif USERECORDWITHMETHODS}
    ctxt: THttpServerRequestAbstract;
    proxy: THttpProxyUrl;       // server proxy definition
    filename: TFileName;        // full file name, including path
    localsize: Int64;           // local file size
    loginfo: PUtf8Char;         // optional error message / log context
    head: THeadCache;           // remote headers retrieved via HEAD
    remote: TUri;               // URI members of the remote resource
    remotehash: THash160;       // remote TUri hash without etag/lastmod
    function MakeHeadAndComputeFilename: cardinal;
    function MakeGet(const path: TUriMatchName): cardinal;
  end;
  TStartProxyRequestClient = class(THttpClientSocket)
  public
    // some additional internal parameters and methods for proper threading
    uri: RawUtf8;
    filestream: TFileStreamEx;
    writestream: TStream;
    partialid: THttpPartialID;
    expectedsize: Int64;
  end;

function TStartProxyRequest.MakeHeadAndComputeFilename: cardinal;
begin // this method is protected by proxy.fOsSafe.Lock
  // always perform a HEAD request to the original server
  if not Assigned(proxy.fHeadCache) or // see HttpHeadCacheSec for this URI
     not proxy.fHeadCache.FindAndCopy(remotehash, head,
       {updatetimeout=}not (hpoClientHeadNoRefresh in proxy.fSettings.Options)) then
    proxy.RemoteClientHead(remote, remotehash, head);
  result := head.Status;
  localsize := head.Size; // for proper logging
  if not StatusCodeIsSuccess(result) then // 4xx.. range
    exit;
  if (head.Size < 0) and
     (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) then // not 204 or 3xx
    // note: progressive download needs an eventual size (by now), but Apache
    // may not provide Content-Length/Range on dynamic content (text/html)
    if FindNameValue(pointer(head.PurgedHeaders), 'CONTENT-TYPE: TEXT/HTM') = nil then
    begin
      result := HTTP_BADGATEWAY; // 502
      loginfo := 'nosize';
      exit;
    end;
  // compute the local file name from the base-32 hash of these headers
  if head.HashB32[0] <> #0 then // '' if no etag/lastmod
    if hpoClientCacheSubFolder in proxy.Settings.Options then
      MakePath([ // hash partitioning into subfolders
        proxy.Settings.DiskCache.Path, head.HashB32[1], head.HashB32], filename)
    else
      MakePath([proxy.Settings.DiskCache.Path, head.HashB32], filename);
end;

function TStartProxyRequest.MakeGet(const path: TUriMatchName): cardinal;
var
  direct: RawByteString;
  background: TStartProxyRequestClient;
  log: TSynLogClass;
  id: THttpPartialID;
  stream: TFileStreamEx;
  opt: THttpRequestExtendedOptions; // local copy since is modified by Open()
begin // this method is protected by proxy.fOsSafe.Lock
  result := HTTP_BADREQUEST;
  log := proxy.fOwner.fLog;
  // no matching local file: need to download to return the GET body
  localsize := head.Size; // reset to the expected final size from HEAD
  if (filename = '') or
     (localsize <= 0) or
     (localsize < proxy.fSettings.HttpDirectGetKB shl 10) then
  begin
    // use the blocking connection for smallest files < 16KB (or without size)
    if localsize <> 0 then
      if (filename = '') and // e.g. dynamic HTML folder index from Apache
         not (hpoClientNoCacheDirect in proxy.fSettings.Options) then
      begin
        if proxy.fMemCache.FindAndCopy(remotehash, direct) then
          loginfo := 'smallmem'
        else
        begin
          direct := proxy.RemoteClientGet(remote);
          proxy.fMemCache.Add(remotehash, direct);
        end;
      end else
        direct := proxy.RemoteClientGet(remote); // eventually written as file
    if (localsize = 0) or
       (direct <> '') then
      if ((localsize < 0) or // no length/range = retrieve full dynamic content
          (length(direct) = localsize)) and
         ((filename = '') or
          FileFromString(direct, filename)) then
      begin
        if localsize < 0 then
          localsize := length(direct);
        if loginfo = nil then
          loginfo := 'small';
        result := ctxt.SetOutContent(direct,
                    not (hpoDisable304 in proxy.fSettings.Options));
      end
      else
      begin
        loginfo := 'geterror';
        result := HTTP_BADGATEWAY; // 502
      end;
    exit;
  end;
  // big files need an asynchronous GET to the uri server
  stream := TFileStreamEx.Create(filename, fmCreate or fmShareRead);
  id := proxy.fOwner.fPartials.Add(filename, {expected=}localsize,
    PHashDigest(@head.HashDigest), Ctxt.ConnectionHttp, {eventualtime=}0);
  if id = 0 then
  begin
    stream.Free;
    loginfo := 'part'; // paranoid
    result := HTTP_SERVERERROR; // 500
    exit;
  end;
  // connect and start background downloading (unlocked)
  opt := proxy.fRemoteClient.Options^; // local copy for this instance
  ctxt.SetOutProgressiveFile(filename, localsize);
  try
    background := TStartProxyRequestClient.OpenOptions(remote, opt);
    background.filestream := stream;
    background.partialid := id;
    background.expectedsize := localsize;
    if proxy.fSettings.HttpLimitPerSecond > 0 then
    begin
      background.writestream := TStreamRedirect.Create(stream);
      (background.writestream as TStreamRedirect).LimitPerSecond :=
        proxy.fSettings.HttpLimitPerSecond;
    end
    else
      background.writestream := stream;
    background.uri := remote.Address;
    Make(['get-', id], head.PurgedHeaders); // already set to OutCustomHeaders
    LockedInc32(@proxy.fOwner.fBackgroundCount);
    try
      TLoggedWorkThread.Create(log, head.PurgedHeaders,
        background, proxy.BackgroundGet, proxy.fOwner.BackgroundFinished);
    except
      LockedDec32(@proxy.fOwner.fBackgroundCount);
      raise;
    end;
    loginfo := pointer(head.PurgedHeaders);
    result := HTTP_SUCCESS;
  except
    // handle main exception from TStartProxyRequestClient.OpenOptions
    stream.Free;
    // abort all progressive clients associated with this failed download
    proxy.fOwner.fPartials.Abort(id);
    if FileExists(filename) and
       not DeleteFile(filename) then
      log.Add.Log(sllLastError, 'MakeGet: delete failed %', [filename], proxy);
    loginfo := 'connect';
    result := HTTP_BADGATEWAY;
  end;
end;

procedure THttpProxyUrl.BackgroundGet(Sender: TObject);
var
  back: TStartProxyRequestClient absolute Sender;
  status: integer;
  msg: RawUtf8;
  fn: TFileName; // local copy
  id: THttpPartialID;
  expected, actual: Int64;
  ok: boolean;
begin
  status := 0;
  actual := -1;
  msg := 'exception';
  ok := false;
  fn := back.filestream.FileName;
  id := back.partialid;
  expected := back.expectedsize;
  try
    // make the actual HTTP/HTTPS blocking huge GET in this thread
    status := back.Request(back.uri, 'GET',
      fSettings.HttpKeepAlive * MilliSecsPerSec, '', '', '',
      {AsRetry=}false, nil, back.writestream);
    actual := back.filestream.Size;
    if status in HTTP_GET_OK then
      if actual = expected then
      begin
        msg := 'ok';
        ok := true;
      end
      else
        msg := 'size mismatch'
    else
      msg := 'unexpected status';
  finally
    // stop writing before notifying progressive readers of a failure
    if back.writestream <> back.filestream then
      back.writestream.Free // TStreamRedirect.Destroy releases filestream
    else
      back.filestream.Free;
    back.Free;
    fOwner.fLog.Add.Log(sllInfo, 'BackgroundGet=%: % [%] size=% expected=%',
      [status, fn, msg, actual, expected], self);
    if not ok then
    begin
      // abort all progressive clients associated with this download
      fOwner.fPartials.Abort(id);
      fOsSafe.Lock; // as in OnGetHeadRemoteUri / MakeGet (non-rentrant lock)
      try
        if FileExists(fn) and
           not DeleteFile(fn) then
          fOwner.fLog.Add.Log(sllLastError,
            'BackgroundGet: delete failed %', [fn], self);
      finally
        fOsSafe.UnLock;
      end;
    end;
  end;
end;

function THttpProxyUrl.ReturnFile(ctxt: THttpServerRequestAbstract;
  const hash: THash160; const filename: TFileName;
  const uri: TUriMatchName; var size: Int64; lastmod: TUnixMSTime;
  var loginfo: PUtf8Char; canbecached: boolean): cardinal;

  procedure FromCache;
  var
    cached: RawByteString;
  begin
    if fMemCache.FindAndCopy(hash, cached) then
      loginfo := 'memcache'
    else
    begin
      cached := StringFromFile(filename);
      if canbecached then
        fMemCache.Add(hash, cached);
    end;
    ctxt.ExtractOutContentType; // reverse ctxt.SetOutFile(fn)
    ctxt.OutContent := cached;
  end;

var
  pck: THttpProxyCacheKind;
  with304: boolean;
begin
  // prepare file streaming as response
  with304 := not (hpoDisable304 in fSettings.Options);
  if lastmod = 0 then
    // from hpsLocalFolder: check local file size+date attributes
    result := ctxt.SetOutFile(filename, with304, '',
      fSettings.CacheControlMaxAgeSec, @size)
  else
  begin
    // from hpsRemoteUri: we have the resource size+date attributes
    result := ctxt.SetOutFile(filename, with304,
      size, lastmod, fSettings.CacheControlMaxAgeSec);
    if (result = HTTP_SUCCESS) or
       (result = HTTP_NOTMODIFIED) then
      FileSetDateFromUnixUtc(filename, UnixTimeUtc); // touch file timestamp
  end;
  loginfo := 'direct';
  // try to use the memory cache content (for GET, not HEAD)
  if (result <> HTTP_SUCCESS) or
     IsHead(ctxt.Method) or
     not Assigned(fMemCache) then
    exit;
  pck := fSettings.MemCache.FromUri(uri);
  if not (pckIgnore in pck) then
    if (pckForce in pck) or
       (size <= Int64(fSettings.MemCache.MaxSizeKB) shl 10) then
      FromCache;
end;


{ THttpProxyServerMainSettings }

constructor THttpProxyServerMainSettings.Create;
begin
  inherited Create;
  fThreadCount := SystemInfo.dwNumberOfProcessors + 1;
  fPort := '8098';
end;

function THttpProxyServerMainSettings.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := (self <> nil) and
            ((fCertificateFile <> '') or
             (fCACertificatesFile <> ''));
  if not result then
    exit;
  if tls.PrivatePassword = '' then
    tls.PrivatePassword := fPrivateKeyPassword;
  InitNetTlsContext(tls, {server=}true, fCertificateFile,
    fPrivateKeyFile, tls.PrivatePassword, fCACertificatesFile);
end;


{ THttpProxyServerSettings }

constructor THttpProxyServerSettings.Create;
begin
  // redirect to AutoCreateFields(self);
  inherited Create;
  // set default values in this main instance
  fDiskCache.Path := Executable.ProgramFilePath + 'proxycache';
  fDiskCache.TimeoutSec := 4 * SecsPerHour; // TTL on disk = 4 hours
  fMemCache.MaxSizeKB := 4;
  fMemCache.TimeoutSec := 15 * SecsPerMin;  // TTL in memory = 15 minutes
end;

function THttpProxyServerSettings.AddUrl(
  one: THttpProxyUrlSettings): THttpProxyUrlSettings;
begin
  result := one;
  if result <> nil then
    if (result.Source = '') and
       not Assigned(result.OnRequest) then
      FreeAndNil(result)
    else
      // will be owned as a new TSynAutoCreateFields fUri[] instance
      PtrArrayAdd(fUrl, result);
end;

function THttpProxyServerSettings.AddFolder(const folder: TFileName;
  const uri: RawUtf8; RaiseExceptionOnNonExistingFolder: ESynExceptionClass): THttpProxyUrlSettings;
begin
  if RaiseExceptionOnNonExistingFolder <> nil then
    if not DirectoryExists(folder) then
      RaiseExceptionOnNonExistingFolder.RaiseUtf8(
        '%.AddFolder: % does not exist', [self, folder]);
  result := THttpProxyUrlSettings.Create;
  result.Url := uri;
  if RaiseExceptionOnNonExistingFolder = nil then
    RaiseExceptionOnNonExistingFolder := EHttpProxyServer;
  result.Source := StringToUtf8(EnsureDirectoryExists(
    folder, RaiseExceptionOnNonExistingFolder));
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddProxy(
  const remoteuri, localuri: RawUtf8): THttpProxyUrlSettings;
begin
  if not IsHttp(remoteuri) then
    EHttpProxyServer.RaiseUtf8('%.AddRemote(%)', [self, remoteuri]);
  result := THttpProxyUrlSettings.Create;
  result.Url := localuri;
  result.Source := remoteuri;
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddEvent(
  const request: TOnHttpProxyServerRequest; const uri: RawUtf8): THttpProxyUrlSettings;
begin
  if not Assigned(request) then
    EHttpProxyServer.RaiseUtf8('%.AddEvent(nil)', [self]);
  result := THttpProxyUrlSettings.Create;
  result.Url := uri;
  result.OnRequest := request;
  result := AddUrl(result);
end;

function THttpProxyServerSettings.AddFromFiles(
  const settingsfolder, mask: TFileName): integer;
var
  F: TSearchRec;
  fn: TFileName;
  one: THttpProxyUrlSettings;
begin
  result := 0;
  if (settingsfolder = '') or
     (FindFirst(MakePath([settingsfolder, mask]), faAnyFile - faDirectory, F) <> 0) then
    exit;
  repeat
    if SearchRecValidFile(F) then
    begin
      MakePath([settingsfolder, F.Name], fn);
      one := THttpProxyUrlSettings.Create;
      if not JsonFileToObject(fn, one, nil, JSONPARSER_TOLERANTOPTIONS) then
        one.Free
      else if AddUrl(one) <> nil then
        inc(result); // successfully added this file
    end;
  until FindNext(F) <> 0;
  FindClose(F);
end;


{ THttpProxyServer }

constructor THttpProxyServer.Create(aSettings: THttpProxyServerSettings);
begin
  inherited Create; // call TSynAutoCreateFields
  fLog := TSynLog;
  if aSettings = nil then
  begin
    fSettings := THttpProxyServerSettings.Create;
    include(fFlags, fSettingsOwned);
  end
  else
    fSettings := aSettings;
end;

destructor THttpProxyServer.Destroy;
begin
  if fServer <> nil then
    fServer.Shutdown; // set flag ASAP
  Stop;
  if fSettingsOwned in fFlags then
    fSettings.Free;
  fSettings := nil;
  ObjArrayClear(fGC);
  inherited Destroy;
end;

procedure THttpProxyServer.Start(const aPrivateKeyPassword: SpiUtf8);
var
  {%H-}log: ISynLog;
  hso: THttpServerOptions;
  tls: TNetTlsContext;
  fav: RawByteString;
begin
  fLog.EnterLocal(log, 'Start %', [fSettings], self);
  if Assigned(log) then
    include(fFlags, fHasLog);
  if fServer <> nil then
    EHttpProxyServer.RaiseUtf8('Duplicated %.Start', [self]);
  // compute THttpAsyncServer options from settings
  hso := [hsoNoXPoweredHeader,  // we better hide ourself
          hsoIncludeDateHeader, // as most proxies do
          hsoThreadSmooting,    // won't hurt and seems the smoothest
          hsoIocpWriteDirect];  // better IOCP scaling when serving files
  if Assigned(log) and
     (psoLogVerbose in fSettings.Server.Options) then
    include(hso, hsoLogVerbose);
  if psoExcludeDateHeader in fSettings.Server.Options then
    exclude(hso, hsoIncludeDateHeader);
  if psoReusePort in fSettings.Server.Options then
    include(hso, hsoReusePort);
  if psoEnableLogging in fSettings.Server.Options then
    include(hso, hsoEnableLogging);
  if psoRejectBotUserAgent in fSettings.Server.Options then
    include(hso, hsoRejectBotUserAgent);
  if psoBan40xIP in fSettings.Server.Options then
    include(hso, hsoBan40xIP);
  tls.PrivatePassword := aPrivateKeyPassword; // if not in fSettings
  if (psoHttpsSelfSigned in fSettings.Server.Options) or
     SetupTls(tls) then
    include(hso, hsoEnableTls);
  // launch the HTTP(S) server
  fServer := THttpAsyncServer.Create(fSettings.Server.Port, nil, nil, '',
    fSettings.Server.ThreadCount, 30000, hso, fLog);
  if fSettings.Server.ServerName <> '' then
    fServer.ServerName := fSettings.Server.ServerName; // override 'mORMot (OS)'
  if fServer.Logger <> nil then
    fServer.Logger.Settings := fSettings.Server.Log;   // override default
  fav := StringFromFile(fSettings.Server.FaviconFile);
  if fav = '' then
    fav := 'default';
  fServer.SetFavIcon(fav); // do once
  fServer.IdleEverySecond; // initial setup of background processing
  fServer.OnIdle := OnIdle;
  if not (psoDisableGzip in fSettings.Server.Options) then
    fServer.RegisterCompress(CompressGZip);
  // setup the URI routes
  AfterServerStarted;
  if hpsRemoteUri in fSources then
  begin
    fPartials := THttpPartials.Create;
    if hsoLogVerbose in hso then
      fPartials.OnLog := fLog.DoLog;
    fServer.SetProgressiveRequests(fPartials);
  end;
  // wait for actual server availability
  if hsoEnableTls in hso then
    if psoHttpsSelfSigned in fSettings.Server.Options then
      fServer.WaitStartedHttps
    else
      fServer.WaitStarted(30, @tls)
  else
    fServer.WaitStarted;
  if Assigned(log) then
    log.Log(sllDebug, 'Start: %', [fServer], self);
  include(fFlags, fUrlSet); // now fUrl[] should remain stable
end;

procedure THttpProxyServer.Stop;
begin
  if fServer <> nil then
    with fLog.Enter('Stop %', [fServer], self) do
    begin
      fServer.Shutdown;
      FreeAndNil(fServer);
    end;
  // FreeAndNil(fServer) joined the request/idle threads, so the count
  // cannot grow any more - detached workers may still use fUrl/fPartials
  while fBackgroundCount <> 0 do
    SleepHiRes(10);
  FreeAndNil(fPartials);
  exclude(fFlags, fUrlSet);
  ObjArrayClear(fUrl);
end;

procedure THttpProxyServer.BackgroundFinished(Sender: TObject);
begin
  LockedDec32(@fBackgroundCount); // called from background GET
end;

function THttpProxyServer.SetupTls(var tls: TNetTlsContext): boolean;
begin
  result := fSettings.Server.SetupTls(tls); // can be overriden if needed
end;

procedure THttpProxyServer.AfterServerStarted;
var
  uri, nfo: RawUtf8;
  new, old: TUriRouter;
  one: THttpProxyUrl;
  s: THttpProxyUrlSettings;
  hps: THttpProxySource;
  i: PtrInt;
begin
  fSources := [];
  fUrlOptions := [];
  fFlags := fFlags - [fHasCacheClean, fUrlSet];
  ObjArrayClear(fUrl);
  new := TUriRouter.Create(TUriTreeNode);
  try
    // 1. compute all routes from Settings[]
    for i := 0 to high(fSettings.Url) do
    begin
      s := fSettings.Url[i];
      if s.Disabled then
        continue;
      one := THttpProxyUrl.Create(s, self);
      PtrArrayAdd(fUrl, one);
      // validate source as local file folder or remote http(s) server
      hps := hpsUndefined;
      if Assigned(s.OnRequest) then
        // execute a callback event
        hps := hpsEvent
      else if s.Source <> '' then
        if IsHttp(s.Source) then
        begin
          // detects also 'http://unix:/path/to/socket.sock:/url/path'
          if one.fRemoteUri.From(s.Source) then
            hps := hpsRemoteUri;
        end
        else
        begin
          // should be a local existing folder name
          Utf8ToFileName(s.Source, s.fLocalFolder);
          if DirectoryExists(s.fLocalFolder) then
          begin
            s.fLocalFolder := IncludeTrailingPathDelimiter(s.fLocalFolder);
            hps := hpsLocalFolder;
          end;
        end;
      one.fSource := hps;
      if hps = hpsUndefined then
      begin
        fLog.Add.Log(sllWarning, 'AfterServerStarted: unexpected %', [one], self);
        continue;
      end;
      include(fSources, hps);
      fUrlOptions := fUrlOptions + s.fOptions;
      // normalize cache settings
      nfo := '';
      if hps <> hpsEvent then
      begin
        if not (psoDisableMemCache in fSettings.Server.Options) then
        begin
          if s.MemCache.MaxSizeKB < 0 then
            s.MemCache.MaxSizeKB := fSettings.MemCache.MaxSizeKB;
          if s.MemCache.TimeoutSec < 0 then
            s.MemCache.TimeoutSec := fSettings.MemCache.TimeoutSec;
          if (s.MemCache.MaxSizeKB > 0) and
             (s.MemCache.TimeoutSec > 0) then
            one.fMemCache := TSynDictionary.Create(TypeInfo(THash160DynArray),
              TypeInfo(TRawByteStringDynArray), PathCaseInsensitive,
              s.MemCache.TimeoutSec);
        end;
        if hps = hpsRemoteUri then
        begin
          if s.DiskCache.MaxSizeKB < 0 then
            s.DiskCache.MaxSizeKB := fSettings.DiskCache.MaxSizeKB;
          if s.DiskCache.Path = '' then
            s.DiskCache.Path := fSettings.DiskCache.Path;
          s.DiskCache.Path := EnsureDirectoryExists(s.DiskCache.Path);
          if s.DiskCache.TimeoutSec <= 0 then
            s.DiskCache.TimeoutSec := fSettings.DiskCache.TimeoutSec;
          if s.HttpHeadCacheSec > 0 then
            one.fHeadCache := TSynDictionary.Create(TypeInfo(THash160DynArray),
              TypeInfo(THeadCaches), {caseins=}false, s.HttpHeadCacheSec);
          if (s.DiskCache.Path <> '') and
             (s.DiskCache.TimeoutSec > 0) then
            include(fFlags, fHasCacheClean);
          Make([' in ', s.DiskCache.Path], nfo);
        end;
      end;
      // prepare optional hash cache
      one.fAlgos := [];
      if hps = hpsLocalFolder then
      begin
        if hpoPublishMd5 in s.Options then
          include(one.fAlgos, hfMd5);
        if hpoPublishSha1 in s.Options then
          include(one.fAlgos, hfSha1);
        if hpoPublishSha256 in s.Options then
          include(one.fAlgos, hfSha256);
        if one.fAlgos <> [] then
          one.fHashCache := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
            TypeInfo(TRawUtf8DynArrayDynArray), PathCaseInsensitive, SecsPerHour);
      end;
      // compute and register this URI
      uri := s.fUrl;
      while (uri <> '') and
            (uri[length(uri)] = '/') do
         SetLength(uri, length(uri) - 1);
      if (uri <> '') and
         (uri[1] <> '/') then
        insert('/', uri, 1);
      if uri <> '' then
        new.Run(s.Methods, uri, OnExecute, one);
      new.Run(s.Methods, uri + '/', OnExecute, one);
      new.Run(s.Methods, uri + '/*', OnExecute, one);
      fLog.Add.Log(sllDebug, 'AfterServerStarted: register % URI as % from %%%',
        [uri, ToText(hps)^, s.fLocalFolder, one.fRemoteUri.URI, nfo], self);
    end;
    // 2. replace existing routes at once
    old := fServer.ReplaceRoute(new); // thread-safe
    new := nil; // is owned by fServer from now on
    if old <> nil then
      PtrArrayAdd(fGC, old); // delayed release at shutdown
  finally
    new.Free;
  end;
end;

procedure THttpProxyServer.OnIdle(Sender: TObject; NowTix: Int64);
var
  i, mem, hash, head: integer;
  tixmin: cardinal;
  one: ^THttpProxyUrl;
  tmp: TSynLogClass; // for Delphi 7 compilation
begin
  // delete any deprecated in-memory cached content - called every second
  if not (fUrlSet in fFlags) then
    exit; // called too early to access fUrl[] safely
  mem := 0;
  hash := 0;
  head := 0;
  one := pointer(fUrl);
  for i := 1 to length(fUrl) do
  begin
    inc(mem,  one^.fMemCache.DeleteDeprecated(NowTix));
    inc(hash, one^.fHashCache.DeleteDeprecated(NowTix));
    inc(head, one^.fHeadCache.DeleteDeprecated(NowTix));
    inc(one);
  end;
  if mem + hash + head <> 0 then
    fLog.Add.Log(sllTrace, 'OnIdle: gc mem=% hash=% head=%',
      [mem, hash, head], self);
  // delete deprecated file content in background thread - every 17 minutes
  if not (fHasCacheClean in fFlags) then
    exit;
  tixmin := (NowTix shr 20) + 1; // check folder every 1048 seconds
  if fTempFilesTix = tixmin then
    exit;
  fTempFilesTix := tixmin;
  tmp := nil;
  LockedInc32(@fBackgroundCount);
  try
    TLoggedWorkThread.Create(tmp, 'cacheclean', nil,
      OnBackgroundDeleteDeprecated, BackgroundFinished);
  except
    LockedDec32(@fBackgroundCount);
    raise;
  end;
end;

procedure THttpProxyServer.OnBackgroundDeleteDeprecated(Sender: TObject);
var
  size: Int64;
  one: ^THttpProxyUrl;
  cache: THttpProxyDisk;
  i, n: integer;
  ok: boolean;
begin // folder timestamp check is called every 17 minutes, and may be slow
  if not (fUrlSet in fFlags) then
    exit; // called too early to access fUrl[] safely
  one := pointer(fUrl);
  for i := 1 to length(fUrl) do
  begin
    cache := one^.fSettings.DiskCache;
    if (one^.fSource = hpsRemoteUri) and
       (cache.Path <> '') and
       (cache.TimeoutSec > 0) then
    begin
      n := 0;
      size := 0;
      ok := DirectoryDeleteOlderFiles(cache.Path,
        cache.TimeoutSec * SecsPerDate, FILES_ALL,
        {recursive=}hpoClientCacheSubFolder in one^.fSettings.Options, @size);
      if not (fUrlSet in fFlags) then
        exit; // paranoid abort at Shutdown
      if (n <> 0) or
         not ok then // something changed on disk
        fLog.Add.Log(sllTrace, 'OnIdle: remote=% delete=% old=%=%',
          [one^.fRemoteUri.Server, BOOL_STR[ok], n, KBNoSpace(size)], self);
    end;
    inc(one);
  end;
end;

function THttpProxyServer.OnGetHeadLocalFolder(Ctxt: THttpServerRequest;
  const Uri: TUriMatchName): cardinal;
var
  fn: TFileName;
  name, html: RawUtf8;
  siz: Int64;
  one: THttpProxyUrl;
  opt: THttpProxyUrlOptions;
  loginfo: PUtf8Char;
  dig: THash256Rec; // 160-bit truncated SHA-256 hash of resource name
  sha: TSha256;
begin
  // supplied URI should be a safe resource reference
  result := HTTP_FORBIDDEN; // 403
  one := Ctxt.RouteOpaque;
  opt := one.Settings.Options;
  // try to assign a local file to the output Ctxt
  UrlDecodeVar(Uri.Path.Text, Uri.Path.Len, name, {space=}'+');
  NormalizeFileNameU(name);
  if not SafePathNameU(name) then
    exit;
  if hpoNoSubFolder in opt then
    if PosExChar(PathDelim, name) <> 0 then
      exit;
  MakePath([one.Settings.fLocalFolder, name], fn);
  sha.Full(pointer(name), length(name), dig.b);
  // stream the content from local file
  result := one.ReturnFile(Ctxt, dig.sha1, fn, Uri, siz, {lastmod=}0, loginfo);
  // additional response types
  case result of
    HTTP_NOTFOUND:
      // this URI is no file, but may be a folder
      if (siz < 0) and // siz=-1 for folder
         not (hpoNoFolderHtmlIndex in opt) then
      begin
        // return the folder files info as cached HTML
        if (hpoDisableFolderHtmlIndexCache in opt) or
           not one.fMemCache.FindAndCopy(dig.sha1, html) then
        begin
          FolderHtmlIndex(fn, Ctxt.Url, StringReplaceChars(name, PathDelim, '/'),
            html, hpoNoSubFolder in opt);
          if Assigned(one.fMemCache) and
             not (hpoDisableFolderHtmlIndexCache in opt) then
            one.fMemCache.Add(dig.sha1, html); // first inserted wins
        end;
        loginfo := 'html';
        result := Ctxt.SetOutContent(
                    html, not (hpoDisable304 in opt), HTML_CONTENT_TYPE);
      end
      else if siz = 0 then
        // check URI for any .md5/.sha1/.sha256 hash extension
        if Assigned(one.fHashCache) then
          result := one.ReturnHash(Ctxt, name, fn);
  end; // may be e.g. HTTP_SUCCESS (200) or HTTP_NOTMODIFIED (304)
  if not StatusCodeIsSuccess(result) then // in 4xx.. range
    Ctxt.SetErrorMessage('serving %', [name]);
  if fHasLog in fFlags then
    fLog.Add.Log(sllDebug, 'OnExecute: % % fn=% status=% size=% %',
      [Ctxt.Method, Ctxt.Url, fn, result, siz, loginfo], self);
end;

function THttpProxyServer.OnGetHeadRemoteUri(Ctxt: THttpServerRequest;
  var Uri: TUriMatchName): cardinal;
var
  req: TStartProxyRequest;
  start: Int64;
  pid: THttpPartialID;
  localdate: TUnixMSTime;
  instance: TObject;
begin
  if fHasLog in fFlags then
    QueryPerformanceMicroSeconds(start);
  // supplied URI should be a safe resource reference
  result := HTTP_FORBIDDEN; // 403
  req.ctxt := Ctxt;
  req.proxy := Ctxt.RouteOpaque;
  req.loginfo := nil;
  // compute the remote URI and its hash corresponding to the original server
  if hpoNoSubFolder in req.proxy.Settings.Options then
    if ByteScanIndex(pointer(Uri.Path.Text), Uri.Path.Len, ord('/')) <> 0 then
      exit;
  req.remote := req.proxy.fRemoteUri;
  if hpoClientLowerCaseUri in req.proxy.Settings.Options then
    CaseBuffer(Uri.Path.Text, Uri.Path.Len, @NormToLowerAnsi7); // normalize
  Append(req.remote.Address, Uri.Path.Text, Uri.Path.Len);
  if not HttpRequestHashBase32(req.remote, nil, nil, @req.remotehash,
           hpoClientNormalizeCaseHash in req.proxy.Settings.Options) then
    exit; // paranoid
  // blocking to ensure file consistency and remote connection sharing
  req.proxy.fOsSafe.Lock;
  try
    // retrieve the headers, from cache or HEAD, and compute the local file name
    // from hashed URI + header etag/lastmod
    result := req.MakeHeadAndComputeFilename;
    Ctxt.OutCustomHeaders := req.head.PurgedHeaders;
    if IsHead(Ctxt.Method) then
    begin
      // no actual file is needed to implement a HEAD request
      if req.loginfo = nil then
        req.loginfo := 'head';
      if result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then // regular response
        Ctxt.SetOutProgressiveFile(req.filename, req.head.Size)
      else if (result <> HTTP_NOCONTENT) and
              (req.head.Size >= 0) then
        Ctxt.AddOutHeader(['Content-Length: ', req.head.Size]);
    end
    else if result in HTTP_GET_OK then // 2xx
    begin
      result := HTTP_NOTFOUND; // default to trigger req.MakeGet(Uri)
      // check any local file
      if (req.filename <> '') and
         FileInfoByName(req.filename, req.localsize, localdate) and
         (req.localsize >= 0) then
        // we have a local cached file
        if (req.head.Size >= 0) and
           (req.localsize = req.head.Size) then
          // it is safe to assume a complete local file is correct (as 200/304)
          result := req.proxy.ReturnFile(Ctxt, req.head.Hash160, req.filename,
            Uri, req.localsize, req.head.TimeMS, req.loginfo)
        else if fPartials.Find(PHashDigest(@req.head.HashDigest)^,
                  req.localsize, @pid, Ctxt.ConnectionHttp) = req.filename then
        begin
          // there is progressive/partial match
          Ctxt.SetOutProgressiveFile(req.filename, req.localsize);
          Make(['part-', pid], req.head.PurgedHeaders); // temp
          req.loginfo := pointer(req.head.PurgedHeaders);
          result := HTTP_SUCCESS;
        end
        else
        begin
          // delete aborted/invalid file on disk and retry from scratch
          fLog.Add.Log(sllWarning, 'OnExecute: delete=% disk=% expected=%',
            [req.head.HashB32, req.localsize, req.head.Size], self);
          if not DeleteFile(req.filename) then
            fLog.Add.Log(sllLastError, 'OnExecute: delete=%', [req.filename], self);
          result := HTTP_NOTFOUND;
        end;
      // check if no matching local file
      if not StatusCodeIsSuccess(result) then // in 4xx.. range
        result := req.MakeGet(Uri); // initiate a GET proxy request
    end;
  finally
    req.proxy.fOsSafe.UnLock;
  end;
  if (req.loginfo <> nil) and
     not StatusCodeIsSuccess(result) then // in 4xx.. range
    Ctxt.SetErrorMessage('proxy %', [req.loginfo])
  else if (req.head.HashB32[0] <> #0) and
          not (hpoNoXProxyName in req.proxy.Settings.Options) then
    Ctxt.AddOutHeader(['X-Proxy-Name: ', req.head.HashB32]);
  if not (fHasLog in fFlags) then
    exit;
  instance := nil;
  if fLog.HasLevel([sllTrace, sllDebug]) then
    instance := self; // include THttpProxyServer context in verbose logs
  fLog.Add.Log(sllInfo, 'OnExecute: % % ip=% fn=% status=% size=% info=% in %',
    [Ctxt.Method, Ctxt.Url, Ctxt.RemoteIP, req.head.HashB32, result,
     req.localsize, req.loginfo, MicroSecFrom(start)], instance);
end;

function THttpProxyServer.OnExecute(Ctxt: THttpServerRequestAbstract): cardinal;
var
  one: THttpProxyUrl;
  uri: TUriMatchName;
  met: TUriRouterMethod;
begin
  // retrieve O(1) execution context
  result := HTTP_NOTFOUND; // 404 by default
  one := Ctxt.RouteOpaque;
  if (one = nil) or
     one.Settings.Disabled or
     (one.Source = hpsUndefined) then
    exit;
  // validate the request method
  result := HTTP_NOTALLOWED; // 405 Method Not Allowed
  if not (UriMethod(Ctxt.Method, met) and
          (met in one.Settings.Methods)) then
    exit;
  // retrieve path and resource/file name from URI
  Ctxt.RouteAt(0, uri.Path);
  if uri.Path.Len > 512 then // obviously invalid
    exit;
  uri.ParsePath; // compute uri.Name for file-level TUriMatch
  // ensure was not marked as rejected
  result := HTTP_FORBIDDEN; // 403
  if (one.Settings.RejectCsv <> '') and
     one.fReject.Check(one.Settings.RejectCsv, uri, PathCaseInsensitive) then
    exit;
  // actual request processing
  result := HTTP_NOTALLOWED; // 405 Method Not Allowed
  case one.Source of
    hpsEvent:
      result := one.Settings.OnRequest(Ctxt, uri, met, one);
      // no cache nor method filtering for callback (by design)
  else
    case met of
      urmGet,
      urmHead:
        case one.Source of
          hpsLocalFolder:
            result := OnGetHeadLocalFolder(THttpServerRequest(Ctxt), uri);
          hpsRemoteUri:
            result := OnGetHeadRemoteUri(THttpServerRequest(Ctxt), uri);
        end;
      urmPost,
      urmPut,
      urmDelete:
          ; { TODO: implement proxy with POST/PUT/DELETE }
    end;
  end;
end;

function ToText(hps: THttpProxySource): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpProxySource), ord(hps));
end;


initialization
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(
    TypeInfo(THttpProxyUrlObjArray), THttpProxyUrl);
  Rtti.RegisterObjArray(
    TypeInfo(THttpProxyUrlSettingsObjArray), THttpProxyUrlSettings);
  {$endif HASDYNARRAYTYPE}

end.
