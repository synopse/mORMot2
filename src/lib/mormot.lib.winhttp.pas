/// Windows HTTP and WebSockets API Libraries
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.winhttp;

{
  *****************************************************************************

   Windows HTTP and WebSockets API Libraries
   - WinINet API Additional Wrappers
   - http.sys / HTTP Server API low-level direct access
   - winhttp.dll Windows API Definitions
   - websocket.dll Windows API Definitions

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef USEWININET}

// compile as a void unit if USEWININET is not defined

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.net.sock,
  Windows,
  WinINet;


{ ******************** WinINet API Additional Wrappers }

/// retrieve extended error information text after a WinINet API call
function SysErrorMessageWinInet(error: integer): string;

/// low-level retrieval of a Domain User from a transmitted Token
procedure GetDomainUserNameFromToken(UserToken: THandle; var result: RawUtf8);


{ ************  http.sys / HTTP Server API low-level direct access }

{$MINENUMSIZE 4}
{$A+}

{$ifdef FPC}
  {$packrecords C}
{$endif FPC}

type
  {$ifndef UNICODE} // circumvent oldest Delphi limitation
  ULONGLONG = Int64;
  {$else}
  ULONGLONG = Windows.ULONGLONG;
  {$endif UNICODE}

  TOverlapped = Windows.TOverlapped;
  ULARGE_INTEGER = Windows.ULARGE_INTEGER;

  HTTP_OPAQUE_ID = ULONGLONG;
  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
  HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
  HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;

  /// http.sys API 2.0 logging file supported layouts
  // - match low-level HTTP_LOGGING_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingType = (
    hltW3C,
    hltIIS,
    hltNCSA,
    hltRaw);

  /// http.sys API 2.0 logging file rollover types
  // - match low-level HTTP_LOGGING_ROLLOVER_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingRollOver = (
    hlrSize,
    hlrDaily,
    hlrWeekly,
    hlrMonthly,
    hlrHourly);

  /// http.sys API 2.0 logging option flags
  // - used to alter the default logging behavior
  // - hlfLocalTimeRollover would force the log file rollovers by local time,
  // instead of the default GMT time
  // - hlfUseUtf8Conversion will use UTF-8 instead of default local code page
  // - only one of hlfLogErrorsOnly and hlfLogSuccessOnly flag could be set
  // at a time: if neither of them are present, both errors and success will
  // be logged, otherwise mutually exclusive flags could be set to force only
  // errors or success logging
  // - match low-level HTTP_LOGGING_FLAG_* constants as defined in HTTP 2.0 API
  THttpApiLoggingFlags = set of (
    hlfLocalTimeRollover,
    hlfUseUtf8Conversion,
    hlfLogErrorsOnly,
    hlfLogSuccessOnly);

  /// http.sys API 2.0 fields used for W3C logging
  // - match low-level HTTP_LOG_FIELD_* constants as defined in HTTP 2.0 API
  THttpApiLogFields = set of (
    hlfDate,
    hlfTime,
    hlfClientIP,
    hlfUserName,
    hlfSiteName,
    hlfComputerName,
    hlfServerIP,
    hlfMethod,
    hlfUriStem,
    hlfUriQuery,
    hlfStatus,
    hlfWIN32Status,
    hlfBytesSent,
    hlfBytesRecv,
    hlfTimeTaken,
    hlfServerPort,
    hlfUserAgent,
    hlfCookie,
    hlfReferer,
    hlfVersion,
    hlfHost,
    hlfSubStatus);

  /// http.sys API 2.0 fields used for server-side authentication
  // - as used by THttpApiServer.SetAuthenticationSchemes/AuthenticationSchemes
  // - match low-level HTTP_AUTH_ENABLE_* constants as defined in HTTP 2.0 API
  THttpApiRequestAuthentications = set of (
    haBasic,
    haDigest,
    haNtlm,
    haNegotiate,
    haKerberos);

type
  // HTTP version used
  HTTP_VERSION = packed record
    MajorVersion: word;
    MinorVersion: word;
  end;

  // the req* values identify Request Headers, and resp* Response Headers
  THttpHeader = (
    reqCacheControl,
    reqConnection,
    reqDate,
    reqKeepAlive,
    reqPragma,
    reqTrailer,
    reqTransferEncoding,
    reqUpgrade,
    reqVia,
    reqWarning,
    reqAllow,
    reqContentLength,
    reqContentType,
    reqContentEncoding,
    reqContentLanguage,
    reqContentLocation,
    reqContentMd5,
    reqContentRange,
    reqExpires,
    reqLastModified,
    reqAccept,
    reqAcceptCharset,
    reqAcceptEncoding,
    reqAcceptLanguage,
    reqAuthorization,
    reqCookie,
    reqExpect,
    reqFrom,
    reqHost,
    reqIfMatch,
    reqIfModifiedSince,
    reqIfNoneMatch,
    reqIfRange,
    reqIfUnmodifiedSince,
    reqMaxForwards,
    reqProxyAuthorization,
    reqReferrer,
    reqRange,
    reqTe,
    reqTranslate,
    reqUserAgent,
    respAcceptRanges = 20{%H-},
    respAge,
    respEtag,
    respLocation,
    respProxyAuthenticate,
    respRetryAfter,
    respServer,
    respSetCookie,
    respVary,
    respWwwAuthenticate);

  THttpVerb = (
    hvUnparsed,
    hvUnknown,
    hvInvalid,
    hvOPTIONS,
    hvGET,
    hvHEAD,
    hvPOST,
    hvPUT,
    hvDELETE,
    hvTRACE,
    hvCONNECT,
    hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
    hvMOVE,
    hvCOPY,
    hvPROPFIND,
    hvPROPPATCH,
    hvMKCOL,
    hvLOCK,
    hvUNLOCK,
    hvSEARCH,
    hvMaximum);

  THttpChunkType = (
    hctFromMemory,
    hctFromFileHandle,
    hctFromFragmentCache);

  THttpServiceConfigID = (
    hscIPListenList,
    hscSSLCertInfo,
    hscUrlAclInfo,
    hscMax);

  THttpServiceConfigQueryType = (
    hscQueryExact,
    hscQueryNext,
    hscQueryMax);

  HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;

  HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;

  HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;

  // Pointers overlap and point into pFullUrl. nil if not present.
  HTTP_COOKED_URL = record
    FullUrlLength: word;     // in bytes not including the #0
    HostLength: word;        // in bytes not including the #0
    AbsPathLength: word;     // in bytes not including the #0
    QueryStringLength: word; // in bytes not including the #0
    pFullUrl: PWideChar;     // points to "http://hostname:port/abs/.../path?query"
    pHost: PWideChar;        // points to the first char in the hostname
    pAbsPath: PWideChar;     // Points to the 3rd '/' char
    pQueryString: PWideChar; // Points to the 1st '?' char or #0
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: PNetAddr;
    pLocalAddress: PNetAddr;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: word;          // in bytes not including the #0
    RawValueLength: word;      // in bytes not including the n#0
    pName: PUtf8Char;          // The header name (minus the ':' character)
    pRawValue: PUtf8Char;      // The header value
  end;

  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

  HTTP_UNKNOWN_HEADERS = array of HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    // warning: don't assume pRawValue is #0 terminated - use RawValueLength
    RawValueLength: word;
    pRawValue: PAnsiChar;
  end;

  PHTTP_KNOWN_HEADER = ^HTTP_KNOWN_HEADER;

  HTTP_RESPONSE_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: pointer;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..respWwwAuthenticate] of HTTP_KNOWN_HEADER;
  end;

  HTTP_REQUEST_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    // - warning: don't assume pRawValue is #0 terminated - use RawValueLength
    KnownHeaders: array[low(THttpHeader)..reqUserAgent] of HTTP_KNOWN_HEADER;
  end;

  HTTP_BYTE_RANGE = record
    StartingOffset: ULARGE_INTEGER;
    Length: ULARGE_INTEGER;
  end;

  // we use 3 distinct HTTP_DATA_CHUNK_* records since variable records
  // alignment is buggy/non compatible under Delphi XE3
  HTTP_DATA_CHUNK_INMEMORY = record
    DataChunkType: THttpChunkType; // always hctFromMemory
    Reserved1: ULONG;
    pBuffer: pointer;
    BufferLength: ULONG;
    Reserved2: ULONG;
    Reserved3: ULONG;
  end;

  PHTTP_DATA_CHUNK_INMEMORY = ^HTTP_DATA_CHUNK_INMEMORY;

  HTTP_DATA_CHUNK_FILEHANDLE = record
    DataChunkType: THttpChunkType; // always hctFromFileHandle
    ByteRange: HTTP_BYTE_RANGE;
    FileHandle: THandle;
  end;

  HTTP_DATA_CHUNK_FRAGMENTCACHE = record
    DataChunkType: THttpChunkType; // always hctFromFragmentCache
    FragmentNameLength: word;      // in bytes not including the #0
    pFragmentName: PWideChar;
  end;

  HTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags: ULONG;
    CertEncodedSize: ULONG;
    pCertEncoded: PUCHAR;
    Token: THandle;
    CertDeniedByMapper: boolean;
  end;

  PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

  HTTP_SSL_INFO = record
    ServerCertKeySize: word;
    ConnectionKeySize: word;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated: ULONG;
  end;

  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix: PWideChar;
  end;

  HTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor: PWideChar;
  end;

  HTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_URLACL_PARAM;
  end;

  HTTP_SERVICE_CONFIG_URLACL_QUERY = record
    QueryDesc: THttpServiceConfigQueryType;
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    dwToken: DWORD;
  end;

  HTTP_REQUEST_INFO_TYPE = (
    HttpRequestInfoTypeAuth,
    HttpRequestInfoTypeChannelBind,
    HttpRequestInfoTypeSslProtocol,
    HttpRequestInfoTypeSslTokenBindingDraft,
    HttpRequestInfoTypeSslTokenBinding,
    HttpRequestInfoTypeRequestTiming,
    HttpRequestInfoTypeTcpInfoV0,
    HttpRequestInfoTypeRequestSizing,
    HttpRequestInfoTypeQuicStats,
    HttpRequestInfoTypeTcpInfoV1);

  // about Authentication in HTTP Version 2.0
  // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
  HTTP_AUTH_STATUS = (
    HttpAuthStatusSuccess,
    HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure);

  HTTP_REQUEST_AUTH_TYPE = (
    HttpRequestAuthTypeNone,
    HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest,
    HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate,
    HttpRequestAuthTypeKerberos);

  SECURITY_STATUS = ULONG;

  HTTP_REQUEST_AUTH_INFO = record
    AuthStatus: HTTP_AUTH_STATUS;
    SecStatus: SECURITY_STATUS;
    Flags: ULONG;
    AuthType: HTTP_REQUEST_AUTH_TYPE;
    AccessToken: THandle;
    ContextAttributes: ULONG;
    PackedContextLength: ULONG;
    PackedContextType: ULONG;
    PackedContext: pointer;
    MutualAuthDataLength: ULONG;
    pMutualAuthData: PAnsiChar;
    PackageNameLength: word;
    pPackageName: LPWSTR;
  end;

  PHTTP_REQUEST_AUTH_INFO = ^HTTP_REQUEST_AUTH_INFO;

  HTTP_REQUEST_INFO = record
    InfoType: HTTP_REQUEST_INFO_TYPE;
    InfoLength: ULONG;
    pInfo: pointer;
  end;

  HTTP_REQUEST_INFOS = array[0..1000] of HTTP_REQUEST_INFO;

  PHTTP_REQUEST_INFOS = ^HTTP_REQUEST_INFOS;

  /// structure used to handle data associated with a specific request
  HTTP_REQUEST = record
    // either 0 (Only Header), either HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY
    Flags: cardinal;
    // An identifier for the connection on which the request was received
    ConnectionId: HTTP_CONNECTION_ID;
    // A value used to identify the request when calling
    // HttpReceiveRequestEntityBody, HttpSendHttpResponse, and/or
    // HttpSendResponseEntityBody
    RequestId: HTTP_REQUEST_ID;
    // The context associated with the URL prefix
    UrlContext: HTTP_URL_CONTEXT;
    // The HTTP version number
    Version: HTTP_VERSION;
    // An HTTP verb associated with this request
    Verb: THttpVerb;
    // The length of the verb string if the Verb field is hvUnknown
    // (in bytes not including the last #0)
    UnknownVerbLength: word;
    // The length of the raw (uncooked) URL (in bytes not including the last #0)
    RawUrlLength: word;
     // Pointer to the verb string if the Verb field is hvUnknown
    pUnknownVerb: PAnsiChar;
    // Pointer to the raw (uncooked) URL
    pRawUrl: PAnsiChar;
    // The canonicalized Unicode URL
    CookedUrl: HTTP_COOKED_URL;
    // Local and remote transport addresses for the connection
    Address: HTTP_TRANSPORT_ADDRESS;
    // The request headers.
    Headers: HTTP_REQUEST_HEADERS;
    // The total number of bytes received from network for this request
    BytesReceived: ULONGLONG;
    EntityChunkCount: word;
    pEntityChunks: pointer;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    // SSL connection information
    pSslInfo: PHTTP_SSL_INFO;
    { beginning of HTTP_REQUEST_V2 structure - manual padding is needed :( }
    {$ifdef CPU32}
    padding: dword;
    {$endif CPU32}
    /// how many extended info about a specific request is available in v2
    RequestInfoCount: word;
    /// v2 trailing structure used to handle extended info about a specific request
    pRequestInfo: PHTTP_REQUEST_INFOS;
  end;

  PHTTP_REQUEST = ^HTTP_REQUEST;

  HTTP_RESPONSE_INFO_TYPE = (
    HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty,
    HttpResponseInfoTypeQosProperty,
    HttpResponseInfoTypeChannelBind);

  HTTP_RESPONSE_INFO = record
    Typ: HTTP_RESPONSE_INFO_TYPE;
    Length: ULONG;
    pInfo: Pointer;
  end;

  PHTTP_RESPONSE_INFO = ^HTTP_RESPONSE_INFO;

  /// structure as expected by HttpSendHttpResponse() API
  HTTP_RESPONSE = object
  public
    Flags: cardinal;
    // The raw HTTP protocol version number
    Version: HTTP_VERSION;
    // The HTTP status code (e.g., 200)
    StatusCode: word;
    // in bytes not including the '\0'
    ReasonLength: word;
    // The HTTP reason (e.g., "OK"). This MUST not contain non-ASCII characters
    // (i.e., all chars must be in range 0x20-0x7E).
    pReason: PUtf8Char;
    // The response headers
    Headers: HTTP_RESPONSE_HEADERS;
    // number of elements in pEntityChunks[] array
    EntityChunkCount: word;
    // pEntityChunks points to an array of EntityChunkCount HTTP_DATA_CHUNK_*
    pEntityChunks: pointer;
    // contains the number of HTTP API 2.0 extended information
    ResponseInfoCount: word;
    // map the HTTP API 2.0 extended information
    pResponseInfo: PHTTP_RESPONSE_INFO;
    // will set both StatusCode and Reason
    // - OutStatus is a temporary variable which will be field with the
    // corresponding text
    procedure SetStatus(code: integer; var OutStatus: RawUtf8);
    // will set the content of the reponse, and ContentType header
    procedure SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY; const Content:
      RawByteString; const ContentType: RawUtf8 = 'text/html');
    /// will set all header values from lines
    // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
    // - all other headers will be set in temp UnknownHeaders[]
    procedure SetHeaders(P: PUtf8Char; var UnknownHeaders: HTTP_UNKNOWN_HEADERS);
    /// add one header value to the internal headers
    // - SetHeaders() method should have been called before to initialize the
    // internal UnknownHeaders[] array
    function AddCustomHeader(P: PUtf8Char; var UnknownHeaders:
      HTTP_UNKNOWN_HEADERS; ForceCustomHeader: boolean): PUtf8Char;
  end;

  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_PROPERTY_FLAGS = ULONG;

  HTTP_ENABLED_STATE = (
    HttpEnabledStateActive,
    HttpEnabledStateInactive);

  PHTTP_ENABLED_STATE = ^HTTP_ENABLED_STATE;

  HTTP_STATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    State: HTTP_ENABLED_STATE;
  end;

  PHTTP_STATE_INFO = ^HTTP_STATE_INFO;

  THTTP_503_RESPONSE_VERBOSITY = (
    Http503ResponseVerbosityBasic,
    Http503ResponseVerbosityLimited,
    Http503ResponseVerbosityFull);

  PHTTP_503_RESPONSE_VERBOSITY = ^THTTP_503_RESPONSE_VERBOSITY;

  HTTP_QOS_SETTING_TYPE = (
    HttpQosSettingTypeBandwidth,
    HttpQosSettingTypeConnectionLimit,
    HttpQosSettingTypeFlowRate // Windows Server 2008 R2 and Windows 7 only
  );

  PHTTP_QOS_SETTING_TYPE = ^HTTP_QOS_SETTING_TYPE;

  HTTP_QOS_SETTING_INFO = record
    QosType: HTTP_QOS_SETTING_TYPE;
    QosSetting: Pointer;
  end;

  PHTTP_QOS_SETTING_INFO = ^HTTP_QOS_SETTING_INFO;

  HTTP_CONNECTION_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxConnections: ULONG;
  end;

  PHTTP_CONNECTION_LIMIT_INFO = ^HTTP_CONNECTION_LIMIT_INFO;

  HTTP_BANDWIDTH_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
  end;

  PHTTP_BANDWIDTH_LIMIT_INFO = ^HTTP_BANDWIDTH_LIMIT_INFO;

  HTTP_FLOWRATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
    MaxPeakBandwidth: ULONG;
    BurstSize: ULONG;
  end;

  PHTTP_FLOWRATE_INFO = ^HTTP_FLOWRATE_INFO;

const
  HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG}  = 1024;
  HTTP_LIMIT_INFINITE {:ULONG}  = ULONG(-1);

type
  HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (
    IdleConnectionTimeout,
    HeaderWaitTimeout);

  PHTTP_SERVICE_CONFIG_TIMEOUT_KEY = ^HTTP_SERVICE_CONFIG_TIMEOUT_KEY;

  HTTP_SERVICE_CONFIG_TIMEOUT_PARAM = word;

  PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM = ^HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

  HTTP_SERVICE_CONFIG_TIMEOUT_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
  end;

  PHTTP_SERVICE_CONFIG_TIMEOUT_SET = ^HTTP_SERVICE_CONFIG_TIMEOUT_SET;

  HTTP_TIMEOUT_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EntityBody: word;
    DrainEntityBody: word;
    RequestQueue: word;
    IdleConnection: word;
    HeaderWait: word;
    MinSendRate: cardinal;
  end;

  PHTTP_TIMEOUT_LIMIT_INFO = ^HTTP_TIMEOUT_LIMIT_INFO;

  HTTP_LISTEN_ENDPOINT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EnableSharing: boolean;
  end;

  PHTTP_LISTEN_ENDPOINT_INFO = ^HTTP_LISTEN_ENDPOINT_INFO;

  HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
    DomainNameLength: word;
    DomainName: PWideChar;
    RealmLength: word;
    Realm: PWideChar;
  end;

  PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = ^HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

  HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
    RealmLength: word;
    Realm: PWideChar;
  end;

  PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = ^HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

const
  HTTP_AUTH_ENABLE_BASIC = $00000001;
  HTTP_AUTH_ENABLE_DIGEST = $00000002;
  HTTP_AUTH_ENABLE_NTLM = $00000004;
  HTTP_AUTH_ENABLE_NEGOTIATE = $00000008;
  HTTP_AUTH_ENABLE_KERBEROS = $00000010;
  HTTP_AUTH_ENABLE_ALL = $0000001F;
  HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING = $01;
  HTTP_AUTH_EX_FLAG_CAPTURE_CREDENTIAL = $02;

type
  HTTP_SERVER_AUTHENTICATION_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    AuthSchemes: ULONG;
    ReceiveMutualAuth: BYTEBOOL;
    ReceiveContextHandle: BYTEBOOL;
    DisableNTLMCredentialCaching: BYTEBOOL;
    ExFlags: BYTE;
    DigestParams: HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
    BasicParams: HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
  end;

  PHTTP_SERVER_AUTHENTICATION_INFO = ^HTTP_SERVER_AUTHENTICATION_INFO;

  HTTP_SERVICE_BINDING_TYPE = (
    HttpServiceBindingTypeNone,
    HttpServiceBindingTypeW,
    HttpServiceBindingTypeA);

  HTTP_SERVICE_BINDING_BASE = record
    BindingType: HTTP_SERVICE_BINDING_TYPE;
  end;

  PHTTP_SERVICE_BINDING_BASE = ^HTTP_SERVICE_BINDING_BASE;

  HTTP_SERVICE_BINDING_A = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PAnsiChar;
    BufferSize: ULONG;
  end;

  PHTTP_SERVICE_BINDING_A = HTTP_SERVICE_BINDING_A;

  HTTP_SERVICE_BINDING_W = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PWCHAR;
    BufferSize: ULONG;
  end;

  PHTTP_SERVICE_BINDING_W = ^HTTP_SERVICE_BINDING_W;

  HTTP_AUTHENTICATION_HARDENING_LEVELS = (
    HttpAuthenticationHardeningLegacy,
    HttpAuthenticationHardeningMedium,
    HttpAuthenticationHardeningStrict);

const
  HTTP_CHANNEL_BIND_PROXY = $1;
  HTTP_CHANNEL_BIND_PROXY_COHOSTING = $20;
  HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK = $2;
  HTTP_CHANNEL_BIND_DOTLESS_SERVICE = $4;
  HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN = $8;
  HTTP_CHANNEL_BIND_CLIENT_SERVICE = $10;

type
  HTTP_CHANNEL_BIND_INFO = record
    Hardening: HTTP_AUTHENTICATION_HARDENING_LEVELS;
    Flags: ULONG;
    ServiceNames: PHTTP_SERVICE_BINDING_BASE;
    NumberOfServiceNames: ULONG;
  end;

  PHTTP_CHANNEL_BIND_INFO = ^HTTP_CHANNEL_BIND_INFO;

  HTTP_REQUEST_CHANNEL_BIND_STATUS = record
    ServiceName: PHTTP_SERVICE_BINDING_BASE;
    ChannelToken: PUCHAR;
    ChannelTokenSize: ULONG;
    Flags: ULONG;
  end;

  PHTTP_REQUEST_CHANNEL_BIND_STATUS = ^HTTP_REQUEST_CHANNEL_BIND_STATUS;

const
  // Logging option flags. When used in the logging configuration alters
  // some default logging behaviour.

  // HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER - This flag is used to change
  //      the log file rollover to happen by local time based. By default
  //      log file rollovers happen by GMT time.
  HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER = 1;

  // HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION - When set the unicode fields
  //      will be converted to UTF-8 multibytes when writting to the log
  //      files. When this flag is not present, the local code page
  //      conversion happens.
  HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION = 2;

  // HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY -
  // HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY - These two flags are used to
  //      to do selective logging. If neither of them are present both
  //      types of requests will be logged. Only one these flags can be
  //      set at a time. They are mutually exclusive.
  HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY = 4;
  HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY = 8;

  // The known log fields recognized/supported by HTTPAPI. Following fields
  // are used for W3C logging. Subset of them are also used for error logging
  HTTP_LOG_FIELD_DATE = $00000001;
  HTTP_LOG_FIELD_TIME = $00000002;
  HTTP_LOG_FIELD_CLIENT_IP = $00000004;
  HTTP_LOG_FIELD_USER_NAME = $00000008;
  HTTP_LOG_FIELD_SITE_NAME = $00000010;
  HTTP_LOG_FIELD_COMPUTER_NAME = $00000020;
  HTTP_LOG_FIELD_SERVER_IP = $00000040;
  HTTP_LOG_FIELD_METHOD = $00000080;
  HTTP_LOG_FIELD_URI_STEM = $00000100;
  HTTP_LOG_FIELD_URI_QUERY = $00000200;
  HTTP_LOG_FIELD_STATUS = $00000400;
  HTTP_LOG_FIELD_WIN32_STATUS = $00000800;
  HTTP_LOG_FIELD_BYTES_SENT = $00001000;
  HTTP_LOG_FIELD_BYTES_RECV = $00002000;
  HTTP_LOG_FIELD_TIME_TAKEN = $00004000;
  HTTP_LOG_FIELD_SERVER_PORT = $00008000;
  HTTP_LOG_FIELD_USER_AGENT = $00010000;
  HTTP_LOG_FIELD_COOKIE = $00020000;
  HTTP_LOG_FIELD_REFERER = $00040000;
  HTTP_LOG_FIELD_VERSION = $00080000;
  HTTP_LOG_FIELD_HOST = $00100000;
  HTTP_LOG_FIELD_SUB_STATUS = $00200000;
  HTTP_ALL_NON_ERROR_LOG_FIELDS = HTTP_LOG_FIELD_SUB_STATUS * 2 - 1;

  // Fields that are used only for error logging
  HTTP_LOG_FIELD_CLIENT_PORT = $00400000;
  HTTP_LOG_FIELD_URI = $00800000;
  HTTP_LOG_FIELD_SITE_ID = $01000000;
  HTTP_LOG_FIELD_REASON = $02000000;
  HTTP_LOG_FIELD_QUEUE_NAME = $04000000;

type
  HTTP_LOGGING_TYPE = (
    HttpLoggingTypeW3C,
    HttpLoggingTypeIIS,
    HttpLoggingTypeNCSA,
    HttpLoggingTypeRaw);

  HTTP_LOGGING_ROLLOVER_TYPE = (
    HttpLoggingRolloverSize,
    HttpLoggingRolloverDaily,
    HttpLoggingRolloverWeekly,
    HttpLoggingRolloverMonthly,
    HttpLoggingRolloverHourly);

  HTTP_LOGGING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    LoggingFlags: ULONG;
    SoftwareName: PWideChar;
    SoftwareNameLength: word;
    DirectoryNameLength: word;
    DirectoryName: PWideChar;
    Format: HTTP_LOGGING_TYPE;
    Fields: ULONG;
    pExtFields: pointer;
    NumOfExtFields: word;
    MaxRecordSize: word;
    RolloverType: HTTP_LOGGING_ROLLOVER_TYPE;
    RolloverSize: ULONG;
    pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  end;

  PHTTP_LOGGING_INFO = ^HTTP_LOGGING_INFO;

  HTTP_LOG_DATA_TYPE = (
    HttpLogDataTypeFields);

  HTTP_LOG_DATA = record
    Typ: HTTP_LOG_DATA_TYPE
  end;

  PHTTP_LOG_DATA = ^HTTP_LOG_DATA;

  HTTP_LOG_FIELDS_DATA = record
    Base: HTTP_LOG_DATA;
    UserNameLength: word;
    UriStemLength: word;
    ClientIpLength: word;
    ServerNameLength: word;
    ServiceNameLength: word;
    ServerIpLength: word;
    MethodLength: word;
    UriQueryLength: word;
    HostLength: word;
    UserAgentLength: word;
    CookieLength: word;
    ReferrerLength: word;
    UserName: PWideChar;
    UriStem: PWideChar;
    ClientIp: PAnsiChar;
    ServerName: PAnsiChar;
    ServiceName: PAnsiChar;
    ServerIp: PAnsiChar;
    Method: PAnsiChar;
    UriQuery: PAnsiChar;
    Host: PAnsiChar;
    UserAgent: PAnsiChar;
    Cookie: PAnsiChar;
    Referrer: PAnsiChar;
    ServerPort: word;
    ProtocolStatus: word;
    Win32Status: ULONG;
    MethodNum: THttpVerb;
    SubStatus: word;
  end;

  PHTTP_LOG_FIELDS_DATA = ^HTTP_LOG_FIELDS_DATA;

  HTTP_BINDING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    RequestQueueHandle: THandle;
  end;

  HTTP_PROTECTION_LEVEL_TYPE = (
    HttpProtectionLevelUnrestricted,
    HttpProtectionLevelEdgeRestricted,
    HttpProtectionLevelRestricted);

  HTTP_PROTECTION_LEVEL_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    Level: HTTP_PROTECTION_LEVEL_TYPE;
  end;

  PHTTP_PROTECTION_LEVEL_INFO = ^HTTP_PROTECTION_LEVEL_INFO;

const
  // some values to avoid including the Windows unit in mormot.net.server
  ERROR_ALREADY_EXISTS = Windows.ERROR_ALREADY_EXISTS;
  ERROR_HANDLE_EOF = Windows.ERROR_HANDLE_EOF;
  ERROR_MORE_DATA = Windows.ERROR_MORE_DATA;
  ERROR_CONNECTION_INVALID = Windows.ERROR_CONNECTION_INVALID;
  ERROR_OLD_WIN_VERSION = Windows.ERROR_OLD_WIN_VERSION;
  ERROR_IO_PENDING = Windows.ERROR_IO_PENDING;
  
  HTTP_VERSION_UNKNOWN: HTTP_VERSION = (
    MajorVersion: 0;
    MinorVersion: 0
  );
  HTTP_VERSION_0_9: HTTP_VERSION = (
    MajorVersion: 0;
    MinorVersion: 9
  );
  HTTP_VERSION_1_0: HTTP_VERSION = (
    MajorVersion: 1;
    MinorVersion: 0
  );
  HTTP_VERSION_1_1: HTTP_VERSION = (
    MajorVersion: 1;
    MinorVersion: 1
  );
  /// error raised by HTTP API when the client disconnected (e.g. after timeout)
  HTTPAPI_ERROR_NONEXISTENTCONNECTION = 1229;
  // if set, available entity body is copied along with the request headers
  // into pEntityChunks
  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  // there is more entity body to be read for this request
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  // initialization for applications that use the HTTP Server API
  HTTP_INITIALIZE_SERVER = 1;
  // initialization for applications that use the HTTP configuration functions
  HTTP_INITIALIZE_CONFIG = 2;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364496
  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364499
  HTTP_SEND_RESPONSE_FLAG_DISCONNECT = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA = $00000002;
  HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA = $00000004;
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = $00000020;
  HTTP_SEND_RESPONSE_FLAG_OPAQUE = $00000040;
  // flag which can be used by HttpRemoveUrlFromUrlGroup()
  HTTP_URL_FLAG_REMOVE_ALL = 1;

  HTTP_KNOWNHEADERS: array[low(THttpHeader)..reqUserAgent] of string[19] = (
    'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma',
    'Trailer', 'Transfer-Encoding', 'Upgrade', 'Via', 'Warning',
    'Allow', 'Content-Length', 'Content-Type', 'Content-Encoding',
    'Content-Language', 'Content-Location', 'Content-MD5', 'Content-Range',
    'Expires', 'Last-Modified', 'Accept', 'Accept-Charset', 'Accept-Encoding',
    'Accept-Language', 'Authorization', 'Cookie', 'Expect', 'From', 'Host',
    'If-Match', 'If-Modified-Since', 'If-None-Match', 'If-Range',
    'If-Unmodified-Since', 'Max-Forwards', 'Proxy-Authorization', 'Referer',
    'Range', 'TE', 'Translate', 'User-Agent');

type
  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    HttpServerChannelBindProperty,
    HttpServerProtectionLevelProperty
    );

  /// direct late-binding access to the HTTP API server 1.0 or 2.0
  THttpApi = packed record
    /// access to the httpapi.dll loaded library
    Module: THandle;
    /// will be either 1.0 or 2.0, depending on the published .dll functions
    Version: HTTP_VERSION;
    /// The HttpInitialize function initializes the HTTP Server API driver, starts it,
    // if it has not already been started, and allocates data structures for the
    // calling application to support response-queue creation and other operations.
    // Call this function before calling any other functions in the HTTP Server API.
    Initialize: function(Version: HTTP_VERSION; Flags: cardinal;
      pReserved: pointer = nil): HRESULT; stdcall;
    /// The HttpTerminate function cleans up resources used by the HTTP Server API
    // to process calls by an application. An application should call HttpTerminate
    // once for every time it called HttpInitialize, with matching flag settings.
    Terminate: function(Flags: cardinal; Reserved: integer = 0): HRESULT; stdcall;
    /// The HttpCreateHttpHandle function creates an HTTP request queue for the
    // calling application and returns a handle to it.
    CreateHttpHandle: function(var ReqQueueHandle: THandle;
      Reserved: integer = 0): HRESULT; stdcall;
    /// The HttpAddUrl function registers a given URL so that requests that match
    // it are routed to a specified HTTP Server API request queue. An application
    // can register multiple URLs to a single request queue using repeated calls to
    // HttpAddUrl
    // - a typical url prefix is 'http://+:80/vroot/', 'https://+:80/vroot/' or
    // 'https://adatum.com:443/secure/database/' - here the '+' is called a
    // Strong wildcard, i.e. will match every IP or server name
    AddUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar;
      Reserved: integer = 0): HRESULT; stdcall;
    /// Unregisters a specified URL, so that requests for it are no longer
    // routed to a specified queue.
    RemoveUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar): HRESULT; stdcall;
    /// retrieves the next available HTTP request from the specified request queue
    ReceiveHttpRequest: function(ReqQueueHandle: THandle; RequestId:
      HTTP_REQUEST_ID; Flags: cardinal; var pRequestBuffer: HTTP_REQUEST;
      RequestBufferLength: ULONG; var pBytesReceived: ULONG;
      pOverlapped: pointer = nil): HRESULT; stdcall;
    /// sent the response to a specified HTTP request
    // - pLogData optional parameter is handled since HTTP API 2.0
    SendHttpResponse: function(ReqQueueHandle: THandle;
      RequestId: HTTP_REQUEST_ID; Flags: integer; var pHttpResponse: HTTP_RESPONSE;
      pReserved1: pointer; var pBytesSent: cardinal; pReserved2: pointer = nil;
      Reserved3: ULONG = 0; pOverlapped: pointer = nil;
      pLogData: PHTTP_LOG_DATA = nil): HRESULT; stdcall;
    /// receives additional entity body data for a specified HTTP request
    ReceiveRequestEntityBody: function(ReqQueueHandle: THandle; RequestId:
      HTTP_REQUEST_ID; Flags: ULONG; pBuffer: pointer; BufferLength: cardinal;
      var pBytesReceived: cardinal; pOverlapped: pointer = nil): HRESULT; stdcall;
    /// sends entity-body data associated with an HTTP response.
    SendResponseEntityBody: function(ReqQueueHandle: THandle; RequestId:
      HTTP_REQUEST_ID; Flags: integer; EntityChunkCount: word;
      pEntityChunks: pointer; var pBytesSent: cardinal; pReserved1: Pointer = nil;
      pReserved2: Pointer = nil; pOverlapped: POverlapped = nil;
      pLogData: PHTTP_LOG_DATA = nil): HRESULT; stdcall;
    /// set specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    SetServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer = nil): HRESULT; stdcall;
    /// deletes specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    DeleteServiceConfiguration: function(ServiceHandle: THandle; ConfigId:
      THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer = nil): HRESULT; stdcall;
    /// removes from the HTTP Server API cache associated with a given request
    // queue all response fragments that have a name whose site portion matches
    // a specified UrlPrefix
    FlushResponseCache: function(ReqQueueHandle: THandle; pUrlPrefix: PWideChar;
      Flags: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
    /// cancels a specified request
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CancelHttpRequest: function(ReqQueueHandle: THandle;
      RequestId: HTTP_REQUEST_ID; pOverlapped: pointer = nil): HRESULT; stdcall;
    /// creates a server session for the specified HTTP API version
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateServerSession: function(Version: HTTP_VERSION;
      var ServerSessionId: HTTP_SERVER_SESSION_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// deletes the server session identified by the server session ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseServerSession: function(ServerSessionId: HTTP_SERVER_SESSION_ID): HRESULT; stdcall;
    ///  creates a new request queue or opens an existing request queue
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - replaces the HTTP version 1.0 CreateHttpHandle() function
    CreateRequestQueue: function(Version: HTTP_VERSION; pName: PWideChar;
      pSecurityAttributes: Pointer; Flags: ULONG; var ReqQueueHandle: THandle): HRESULT; stdcall;
    /// sets a new server session property or modifies an existing property
    // on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a server property on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// creates a URL Group under the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateUrlGroup: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      var UrlGroupId: HTTP_URL_GROUP_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// closes the URL Group identified by the URL Group ID
    // - this call also removes all of the URLs that are associated with
    // the URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID): HRESULT; stdcall;
    /// adds the specified URL to the URL Group identified by the URL Group ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - this function replaces the HTTP version 1.0 AddUrl() function
    AddUrlToUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; UrlContext: HTTP_URL_CONTEXT = 0;
      Reserved: ULONG = 0): HRESULT; stdcall;
    /// removes the specified URL from the group identified by the URL Group ID
    // - this function removes one, or all, of the URLs from the group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - it replaces the HTTP version 1.0 RemoveUrl() function
    RemoveUrlFromUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; Flags: ULONG): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the specified
    // URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a property on the specified URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID; aProperty:
      HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the request
    // queue identified by the specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetRequestQueueProperty: function(ReqQueueHandle: THandle; aProperty:
      HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReserved: Pointer): HRESULT; stdcall;
    ///  queries a property of the request queue identified by the
    // specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryRequestQueueProperty: function(ReqQueueHandle: THandle;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReturnLength: PULONG;
      pReserved: Pointer): HRESULT; stdcall;
  end;

var
  Http: THttpApi;

type
  THttpApis = (
    hInitialize, hTerminate, hCreateHttpHandle, hAddUrl, hRemoveUrl,
    hReceiveHttpRequest, hSendHttpResponse, hReceiveRequestEntityBody,
    hResponseEntityBody, hSetServiceConfiguration, hDeleteServiceConfiguration,
    hFlushResponseCache, hCancelHttpRequest, hCreateServerSession,
    hCloseServerSession, hCreateRequestQueue, hSetServerSessionProperty,
    hQueryServerSessionProperty, hCreateUrlGroup, hCloseUrlGroup,
    hAddUrlToUrlGroup, hRemoveUrlFromUrlGroup, hSetUrlGroupProperty,
    hQueryUrlGroupProperty, hSetRequestQueueProperty, hQueryRequestQueueProperty);

const
  hHttpApi2First = hCancelHttpRequest;
  HttpNames: array[THttpApis] of PChar = (
    'HttpInitialize', 'HttpTerminate',
    'HttpCreateHttpHandle', 'HttpAddUrl', 'HttpRemoveUrl',
    'HttpReceiveHttpRequest', 'HttpSendHttpResponse',
    'HttpReceiveRequestEntityBody', 'HttpSendResponseEntityBody',
    'HttpSetServiceConfiguration', 'HttpDeleteServiceConfiguration',
    'HttpFlushResponseCache', 'HttpCancelHttpRequest', 'HttpCreateServerSession',
    'HttpCloseServerSession', 'HttpCreateRequestQueue',
    'HttpSetServerSessionProperty', 'HttpQueryServerSessionProperty',
    'HttpCreateUrlGroup', 'HttpCloseUrlGroup', 'HttpAddUrlToUrlGroup',
    'HttpRemoveUrlFromUrlGroup', 'HttpSetUrlGroupProperty',
    'HttpQueryUrlGroupProperty', 'HttpSetRequestQueueProperty',
    'HttpQueryRequestQueueProperty');


type
  /// exception raised during http.sys HTTP/1.1 process
  EHttpApiServer = class(ENetSock)
  protected
    fLastError: integer;
    fLastApi: THttpApis;
  public
    /// raise an EHttpApiServer if the http.sys API result code is an error
    class procedure RaiseOnError(api: THttpApis; Error: integer);
    /// initialize a new EHttpApiServer instance
    constructor Create(api: THttpApis; Error: integer); reintroduce;
  published
    /// the error code of this exception
    property LastError: integer
      read fLastError;
    /// the execution context of this exception
    property LastApi: THttpApis
      read fLastApi;
  end;



const
  /// the name of the Windows http.sys API library
  HTTPAPI_DLL = 'httpapi.dll';

/// ensure that the http.sys API has been loaded
procedure HttpApiInitialize;

/// compute a http.sys compatible URI from https://root:port fields
function RegURL(aRoot, aPort: RawUtf8; Https: boolean;
  aDomainName: RawUtf8): SynUnicode;

/// low-level adjustement of the HTTP_REQUEST headers
function RetrieveHeaders(const Request: HTTP_REQUEST;
  const RemoteIPHeadUp: RawUtf8; out RemoteIP: RawUtf8): RawUtf8;


{ ******************** winhttp.dll Windows API Definitions }

// some type redirections to avoid link to Windows and WinINet in uses clause

type
  ULONG = cardinal;

  HINTERNET = WinINet.HINTERNET;

  PLPWStr = Windows.PLPWStr;

const
  HTTP_QUERY_STATUS_CODE = WinINet.HTTP_QUERY_STATUS_CODE;
  HTTP_QUERY_RAW_HEADERS_CRLF = WinINet.HTTP_QUERY_RAW_HEADERS_CRLF;
  HTTP_QUERY_CONTENT_ENCODING = WinINet.HTTP_QUERY_CONTENT_ENCODING;
  HTTP_QUERY_ACCEPT_ENCODING = WinINet.HTTP_QUERY_ACCEPT_ENCODING;
  HTTP_QUERY_CONTENT_LENGTH = WinINet.HTTP_QUERY_CONTENT_LENGTH;

  ERROR_INVALID_HANDLE = Windows.ERROR_INVALID_HANDLE;
  ERROR_INSUFFICIENT_BUFFER = Windows.ERROR_INSUFFICIENT_BUFFER;

  ERROR_WINHTTP_AUTODETECTION_FAILED = 12180;
  
const
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4; // Windows 8.1 and newer

  WINHTTP_FLAG_BYPASS_PROXY_CACHE = $00000100; // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_SECURE = $00800000; // use SSL if applicable (HTTPS)
  WINHTTP_ADDREQ_FLAG_COALESCE = $40000000;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;

  // taken from http://www.tek-tips.com/faqs.cfm?fid=7493
  // status manifests for WinHttp status callback
  WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = $00000001;
  WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = $00000002;
  WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = $00000004;
  WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = $00000008;
  WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = $00000010;
  WINHTTP_CALLBACK_STATUS_REQUEST_SENT = $00000020;
  WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = $00000040;
  WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = $00000080;
  WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = $00000100;
  WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = $00000200;
  WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = $00000400;
  WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = $00000800;
  WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = $00001000;
  WINHTTP_CALLBACK_STATUS_REDIRECT = $00004000;
  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = $00008000;
  WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = $00010000;
  WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = $00020000;
  WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = $00040000;
  WINHTTP_CALLBACK_STATUS_READ_COMPLETE = $00080000;
  WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = $00100000;
  WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = $00200000;
  WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = $00400000;
  WINHTTP_CALLBACK_FLAG_RESOLVE_NAME =
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED;
  WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER =
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER;
  WINHTTP_CALLBACK_FLAG_SEND_REQUEST =
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or WINHTTP_CALLBACK_STATUS_REQUEST_SENT;
  WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE =
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED;
  WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION =
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED;
  WINHTTP_CALLBACK_FLAG_HANDLES =
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING;
  WINHTTP_CALLBACK_FLAG_DETECTING_PROXY =
    WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
  WINHTTP_CALLBACK_FLAG_REDIRECT = WINHTTP_CALLBACK_STATUS_REDIRECT;
  WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE =
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
  WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
  WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE =
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
  WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_READ_COMPLETE = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
  WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
  WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;
  WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS =
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE or
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE or
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE or
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE or
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE or
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;
  WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = $ffffffff;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 = $00000008;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 = $00000020;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1 = $00000080;

  // tls 1.1 & 1.2 const from here:
  // https://github.com/nihon-tc/Rtest/blob/master/header/Microsoft%20SDKs/Windows/v7.0A/Include/winhttp.h
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;

  // Sets an unsigned long integer value that specifies which secure protocols are acceptable.
  // By default only SSL3 and TLS1 are enabled in Windows 7 and Windows 8.
  // By default only SSL3, TLS1.0, TLS1.1, and TLS1.2 are enabled in Windows 8.1 and Windows 10.
  WINHTTP_OPTION_SECURE_PROTOCOLS = 84;
  // Instructs the stack to start a WebSocket handshake process with WinHttpSendRequest.
  // This option takes no parameters.
  WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;

  // if the following value is returned by WinHttpSetStatusCallback, then
  // probably an invalid (non-code) address was supplied for the callback
  WINHTTP_INVALID_STATUS_CALLBACK = -1;

  // values for WINHTTP_OPTION_DISABLE_FEATURE
  WINHTTP_DISABLE_COOKIES = $00000001;
  WINHTTP_DISABLE_REDIRECTS = $00000002;
  WINHTTP_DISABLE_AUTHENTICATION = $00000004;
  WINHTTP_DISABLE_KEEP_ALIVE = $00000008;

  WINHTTP_OPTION_ENABLE_FEATURE = 79;
  // values for WINHTTP_OPTION_ENABLE_FEATURE
  WINHTTP_ENABLE_SSL_REVOCATION = $00000001;
  WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION = $00000002;

  // from http://www.tek-tips.com/faqs.cfm?fid=7493
  WINHTTP_OPTION_SECURITY_FLAGS = 31;
  WINHTTP_OPTION_DISABLE_FEATURE = 63;
  WINHTTP_OPTION_CLIENT_CERT_CONTEXT = $0000002F;
  WINHTTP_NO_CLIENT_CERT_CONTEXT = $00000000;

  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = $00002F0C;

  SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = $00002000; // expired X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID = $00001000; // bad common name in X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = $00000200;
  SECURITY_FLAT_IGNORE_CERTIFICATES: DWORD =
    SECURITY_FLAG_IGNORE_UNKNOWN_CA or
    SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
    SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
    SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;

  WINHTTP_AUTH_TARGET_SERVER = 0;
  WINHTTP_AUTH_TARGET_PROXY = 1;
  WINHTTP_AUTH_SCHEME_BASIC = $00000001;
  WINHTTP_AUTH_SCHEME_NTLM = $00000002;
  WINHTTP_AUTH_SCHEME_PASSPORT = $00000004;
  WINHTTP_AUTH_SCHEME_DIGEST = $00000008;
  WINHTTP_AUTH_SCHEME_NEGOTIATE = $00000010;

  WINHTTP_AUTOPROXY_AUTO_DETECT = $00000001;
  WINHTTP_AUTOPROXY_CONFIG_URL = $00000002;
  WINHTTP_AUTO_DETECT_TYPE_DHCP = $00000001;
  WINHTTP_AUTO_DETECT_TYPE_DNS_A = $00000002;

  WINHTTP_NO_PROXY_NAME = nil;
  WINHTTP_NO_PROXY_BYPASS = nil;

type
  /// low-level API reference to a WebSocket session
  WEB_SOCKET_HANDLE = Pointer;
  /// WebSocket close status as defined by http://tools.ietf.org/html/rfc6455#section-7.4

  WEB_SOCKET_CLOSE_STATUS = Word;

  /// the bit values used to construct the WebSocket frame header for httpapi.dll
  // - not equals to WINHTTP_WEB_SOCKET_BUFFER_TYPE from winhttp.dll
  WEB_SOCKET_BUFFER_TYPE = ULONG;

type
  /// types of WebSocket buffers for winhttp.dll
  // it is the different thing than WEB_SOCKET_BUFFER_TYPE for httpapi.dll
  WINHTTP_WEB_SOCKET_BUFFER_TYPE = ULONG;

  WINHTTP_STATUS_CALLBACK = procedure(hInternet: HINTERNET; dwContext: PDWORD;
    dwInternetStatus: DWORD; lpvStatusInformation: pointer;
    dwStatusInformationLength: DWORD); stdcall;

  PWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;

  WINHTTP_AUTOPROXY_OPTIONS = record
    dwFlags: DWORD;
    dwAutoDetectFlags: DWORD;
    lpszAutoConfigUrl: PWideChar;
    lpvReserved: Pointer;
    dwReserved: DWORD;
    fAutoLogonIfChallenged: BOOL;
  end;
  PWINHTTP_AUTOPROXY_OPTIONS = ^WINHTTP_AUTOPROXY_OPTIONS;

  WINHTTP_PROXY_INFO = record
    dwAccessType: DWORD;          // see WINHTTP_ACCESS_* types
    lpszProxy: PWideChar;         // proxy server list
    lpszProxyBypass: PWideChar;   // proxy bypass list
  end;
  PWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;

  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG = record
    fAutoDetect: BOOL;
    lpszAutoConfigUrl: PWideChar;
    lpszProxy: PWideChar;
    lpszProxyBypass: PWideChar;
  end;
  PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG = ^WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;

  /// direct late-binding access to the WinHttp API
  // - note: WebSocket* API calls require Windows 8 and later
  TWinHttpBinding = packed record
    /// access to the winhttp.dll loaded library
    LibraryHandle: THandle;
    /// depends on the published .dll functions
    WebSocketEnabled: boolean;
    /// Initializes an application's use of the WinHttp functions.
    Open: function(pwszUserAgent: PWideChar; dwAccessType: DWORD; pwszProxyName,
      pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall;
    /// Sets up a callback function that WinHttp can call as progress is made during an operation.
    SetStatusCallback: function(hSession: HINTERNET;
      lpfnInternetCallback: WINHTTP_STATUS_CALLBACK; dwNotificationFlags: DWORD;
      dwReserved: PDWORD): WINHTTP_STATUS_CALLBACK; stdcall;
    /// Specifies the initial target server of an HTTP request.
    Connect: function(hSession: HINTERNET; pswzServerName: PWideChar;
      nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET; stdcall;
    /// Creates an HTTP request handle.
    OpenRequest: function(hConnect: HINTERNET; pwszVerb: PWideChar;
      pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
      ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;
    /// Closes a single HINTERNET handle.
    CloseHandle: function(hInternet: HINTERNET): BOOL; stdcall;
    /// Adds one or more HTTP request headers to the HTTP request handle.
    AddRequestHeaders: function(hRequest: HINTERNET; pwszHeaders: PWideChar;
      dwHeadersLength: DWORD; dwModifiers: DWORD): BOOL; stdcall;
    /// Sends the specified request to the HTTP server.
    SendRequest: function(hRequest: HINTERNET; pwszHeaders: PWideChar;
      dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD;
      dwTotalLength: DWORD; dwContext: DWORD): BOOL; stdcall;
    /// Ends an HTTP request that is initiated by WinHttpSendRequest.
    ReceiveResponse: function(hRequest: HINTERNET; lpReserved: Pointer): BOOL; stdcall;
    /// Retrieves header information associated with an HTTP request.
    QueryHeaders: function(hRequest: HINTERNET; dwInfoLevel: DWORD;
      pwszName: PWideChar; lpBuffer: Pointer; var lpdwBufferLength,
      lpdwIndex: DWORD): BOOL; stdcall;
    /// Returns the amount of data, in bytes, available to be read with WinHttpReadData.
    QueryDataAvailable: function(hRequest: HINTERNET;
      var lpdwNumberOfBytesAvailable: DWORD): BOOL; stdcall;
    /// Retrieves some options about the current connection.
    QueryOption: function(hInet: HINTERNET; dwOption: DWORD;
      lpBuffer: Pointer; var lpdwBufferLength: DWORD): BOOL; stdcall;
    /// Retrieves the low-level Proxy information for a given URI.
    GetProxyForUrl: function(hSession: HINTERNET; lpcwszUrl: LPCWSTR;
      pAutoProxyOptions: PWINHTTP_AUTOPROXY_OPTIONS;
      var pProxyInfo: WINHTTP_PROXY_INFO): BOOL; stdcall;
    // Retrievs the Internet Explorer Proxy information.
    GetIEProxyConfigForCurrentUser: function(
      var pProxyInfo: WINHTTP_CURRENT_USER_IE_PROXY_CONFIG): BOOL; stdcall;
    /// Reads data from a handle opened by the WinHttpOpenRequest function.
    ReadData: function(hRequest: HINTERNET; lpBuffer: Pointer;
      dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
    /// Sets the various time-outs that are involved with HTTP transactions.
    SetTimeouts: function(hInternet: HINTERNET; dwResolveTimeout: DWORD;
      dwConnectTimeout: DWORD; dwSendTimeout: DWORD; dwReceiveTimeout: DWORD): BOOL; stdcall;
    /// Sets an Internet option.
    SetOption: function(hInternet: HINTERNET; dwOption: DWORD; lpBuffer: Pointer;
      dwBufferLength: DWORD): BOOL; stdcall;
    /// Passes the required authorization credentials to the server.
    SetCredentials: function(hRequest: HINTERNET; AuthTargets: DWORD;
      AuthScheme: DWORD; pwszUserName: PWideChar; pwszPassword: PWideChar;
      pAuthParams: Pointer): BOOL; stdcall;
    /// Completes a WebSocket handshake started by WinHttpSendRequest.
    WebSocketCompleteUpgrade: function(hRequest: HINTERNET;
      lpReserved: Pointer): HINTERNET; stdcall;
    /// Closes a WebSocket connection.
    WebSocketClose: function(hWebSocket: HINTERNET; usStatus: Word;
      pvReason: Pointer; dwReasonLength: DWORD): DWORD; stdcall;
    /// Retrieves the close status sent by a server
    WebSocketQueryCloseStatus: function(hWebSocket: HINTERNET;
      out usStatus: Word; pvReason: Pointer; dwReasonLength: DWORD;
      out dwReasonLengthConsumed: DWORD): DWORD; stdcall;
    /// Sends data over a WebSocket connection.
    WebSocketSend: function(hWebSocket: HINTERNET;
      eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; pvBuffer: Pointer;
      dwBufferLength: DWORD): DWORD; stdcall;
    /// Receives data from a WebSocket connection.
    WebSocketReceive: function(hWebSocket: HINTERNET; pvBuffer: Pointer;
      dwBufferLength: DWORD; out dwBytesRead: DWORD;
      out eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): DWORD; stdcall;
    /// Writes data to a handle opened by the WinHttpOpenRequest function.
    WriteData: function(hRequest: HINTERNET; lpBuffer: Pointer;
      dwNumberOfBytesToWrite: DWORD; var lpdwNumberOfBytesWritten: DWORD): BOOL; stdcall;
  end;

var
  WinHttpApi: TWinHttpBinding;

type
  EWinHttp = class(ENetSock);
  
  TWinHttpApis = (
    hOpen, hSetStatusCallback, hConnect, hOpenRequest, hCloseHandle,
    hAddRequestHeaders, hSendRequest, hReceiveResponse, hQueryHeaders,
    hQueryDataAvailable, hQueryOption, hGetProxyForUrl,
    hGetIEProxyConfigForCurrentUser, hReadData,
    hSetTimeouts, hSetOption, hSetCredentials,
    hWebSocketCompleteUpgrade, hWebSocketClose, hWebSocketQueryCloseStatus,
    hWebSocketSend, hWebSocketReceive, hWriteData);

const
  hWebSocketApiFirst = hWebSocketCompleteUpgrade;

const
  WinHttpNames: array[TWinHttpApis] of PChar = (
    'WinHttpOpen', 'WinHttpSetStatusCallback', 'WinHttpConnect',
    'WinHttpOpenRequest', 'WinHttpCloseHandle', 'WinHttpAddRequestHeaders',
    'WinHttpSendRequest', 'WinHttpReceiveResponse', 'WinHttpQueryHeaders',
    'WinHttpQueryDataAvailable', 'WinHttpQueryOption', 'WinHttpGetProxyForUrl',
    'WinHttpGetIEProxyConfigForCurrentUser', 'WinHttpReadData', 'WinHttpSetTimeouts',
    'WinHttpSetOption', 'WinHttpSetCredentials', 'WinHttpWebSocketCompleteUpgrade',
    'WinHttpWebSocketClose', 'WinHttpWebSocketQueryCloseStatus',
    'WinHttpWebSocketSend', 'WinHttpWebSocketReceive', 'WinHttpWriteData');


/// low-level thread-safe initialization of the WinHtpp API
// - this unit will try to load winhttp.dll in its initialization section below
procedure WinHttpApiInitialize(RaiseOnError: boolean = true);

/// a callback raising a EWinHttp on error
procedure WinHttpSecurityErrorCallback(hInternet: hInternet; dwContext: PDWORD;
  dwInternetStatus: cardinal; lpvStatusInformation: pointer;
  dwStatusInformationLength: cardinal); stdcall;

type
  /// proxy information as returned by GetProxyInfo()
  TProxyInfo = record
    /// the proxy server address and port, e.g. '10.0.0.8:7985'
    URL: SynUnicode;
    /// the Bypass rule
    Bypass: SynUnicode;
    /// if the Proxy settings were auto-detected by Internet Explorer
    AutoDetected: Boolean;
    /// detailed error message, if GetProxyInfo() returned a non 0 error code
    ErrorMessage: string;
  end;

/// use WinHttp to retrieve the proxy information needed to access a given URI
// - it will first try to return the Internet Explorer settings, then
// try to create a WinHttp client connection, and retrieve the proxy information
// - will parse any system-defined Proxy Auto-Configuration (PAC) file if needed
// - returns 0 on success, or an error code on failure - see also ErrorMessage -
// e.g. ERROR_WINHTTP_AUTODETECTION_FAILED (12180)
// - note that this call may require a network access, and can be slow: if you
// can, try to store the proxy information in the settings, and only call it
// in case of connection failure
function WinHttpGetProxyInfo(const URL: SynUnicode;
  out ProxyInfo: TProxyInfo): DWORD;


{ ******************** websocket.dll Windows API Definitions }

type
  WEB_SOCKET_PROPERTY_TYPE = (
    WEB_SOCKET_RECEIVE_BUFFER_SIZE_PROPERTY_TYPE, //0
    WEB_SOCKET_SEND_BUFFER_SIZE_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_MASKING_PROPERTY_TYPE,
    WEB_SOCKET_ALLOCATED_BUFFER_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_UTF8_VERIFICATION_PROPERTY_TYPE,
    WEB_SOCKET_KEEPALIVE_INTERVAL_PROPERTY_TYPE,
    WEB_SOCKET_SUPPORTED_VERSIONS_PROPERTY_TYPE);

  WEB_SOCKET_ACTION_QUEUE = cardinal;

  WEB_SOCKET_ACTION = (
    WEB_SOCKET_NO_ACTION, //0
    WEB_SOCKET_SEND_TO_NETWORK_ACTION,
    WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION,
    WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION,
    WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION);

  PWEB_SOCKET_ACTION = ^WEB_SOCKET_ACTION;

  WEB_SOCKET_PROPERTY = record
    PropType: WEB_SOCKET_PROPERTY_TYPE;
    pvValue: Pointer;
    ulValueSize: ULONG;
  end;

  PWEB_SOCKET_PROPERTY = ^WEB_SOCKET_PROPERTY;

  WEB_SOCKET_HTTP_HEADER = record
    pcName: PAnsiChar;
    ulNameLength: ULONG;
    pcValue: PAnsiChar;
    ulValueLength: ULONG;
  end;

  PWEB_SOCKET_HTTP_HEADER = ^WEB_SOCKET_HTTP_HEADER;

  WEB_SOCKET_HTTP_HEADER_ARR = array of WEB_SOCKET_HTTP_HEADER;

  PWEB_SOCKET_BUFFER_DATA = ^WEB_SOCKET_BUFFER_DATA;

  WEB_SOCKET_BUFFER_DATA = record
    pbBuffer: PBYTE;
    ulBufferLength: ULONG;
    Reserved1: Word;
  end;

  WEB_SOCKET_BUFFER_CLOSE_STATUS = record
    pbReason: PBYTE;
    ulReasonLength: ULONG;
    usStatus: WEB_SOCKET_CLOSE_STATUS;
  end;

  /// direct late-binding access to the WebSocket Protocol Component API functions
  TWebSocketApi = packed record
    /// acces to the loaded library handle
    LibraryHandle: THandle;
    /// depends on Windows version
    WebSocketEnabled: boolean;
    /// aborts a WebSocket session handle created by WebSocketCreateClientHandle
    // or WebSocketCreateServerHandle
    AbortHandle: procedure(hWebSocket: WEB_SOCKET_HANDLE); stdcall;
    /// begins the client-side handshake
    BeginClientHandshake: function(hWebSocket: WEB_SOCKET_HANDLE;
      pszSubprotocols: PAnsiChar; ulSubprotocolCount: ULONG; pszExtensions:
      PAnsiChar; ulExtensionCount: ULONG; const pInitialHeaders:
      PWEB_SOCKET_HTTP_HEADER; ulInitialHeaderCount: ULONG;
      out pAdditionalHeaders: PWEB_SOCKET_HTTP_HEADER;
      out pulAdditionalHeaderCount: ULONG): HRESULT; stdcall;
    /// begins the server-side handshake
    BeginServerHandshake: function(hWebSocket: WEB_SOCKET_HANDLE;
      pszSubprotocolSelected: PAnsiChar; pszExtensionSelected: PAnsiChar;
      ulExtensionSelectedCount: ULONG; const pRequestHeaders:
      PWEB_SOCKET_HTTP_HEADER; ulRequestHeaderCount: ULONG; out pResponseHeaders:
      PWEB_SOCKET_HTTP_HEADER; out pulResponseHeaderCount: ULONG): HRESULT; stdcall;
    /// completes an action started by WebSocketGetAction
    CompleteAction: function(hWebSocket: WEB_SOCKET_HANDLE;
      pvActionContext: Pointer; ulBytesTransferred: ULONG): HRESULT; stdcall;
    /// creates a client-side WebSocket session handle
    CreateClientHandle: function(const pProperties: PWEB_SOCKET_PROPERTY;
      ulPropertyCount: ULONG; out phWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// creates a server-side WebSocket session handle
    CreateServerHandle: function(const pProperties: PWEB_SOCKET_PROPERTY;
      ulPropertyCount: ULONG; out phWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// deletes a WebSocket session handle created by WebSocketCreateClientHandle
    // or WebSocketCreateServerHandle
    DeleteHandle: procedure(hWebSocket: WEB_SOCKET_HANDLE); stdcall;
    /// completes the client-side handshake
    EndClientHandshake: function(hWebSocket: WEB_SOCKET_HANDLE; const
      pResponseHeaders: PWEB_SOCKET_HTTP_HEADER; ulReponseHeaderCount: ULONG;
      var pulSelectedExtensions: ULONG; var pulSelectedExtensionCount: ULONG;
      var pulSelectedSubprotocol: ULONG): HRESULT; stdcall;
    /// completes the server-side handshake
    EndServerHandshake: function(hWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// returns an action from a call to WebSocketSend, WebSocketReceive or WebSocketCompleteAction
    GetAction: function(hWebSocket: WEB_SOCKET_HANDLE; eActionQueue:
      WEB_SOCKET_ACTION_QUEUE; pDataBuffers: Pointer {WEB_SOCKET_BUFFER_DATA};
      var pulDataBufferCount: ULONG; var pAction: WEB_SOCKET_ACTION;
      var pBufferType: WEB_SOCKET_BUFFER_TYPE; var pvApplicationContext: Pointer;
      var pvActionContext: Pointer): HRESULT; stdcall;
    /// gets a single WebSocket property
    GetGlobalProperty: function(eType: WEB_SOCKET_PROPERTY_TYPE;
      pvValue: Pointer; var ulSize: ULONG): HRESULT; stdcall;
    /// adds a receive operation to the protocol component operation queue
    Receive: function(hWebSocket: WEB_SOCKET_HANDLE; pBuffer: Pointer;
      pvContext: Pointer): HRESULT; stdcall;
    /// adds a send operation to the protocol component operation queue
    Send: function(hWebSocket: WEB_SOCKET_HANDLE; BufferType:
      WEB_SOCKET_BUFFER_TYPE; pBuffer, Context: Pointer): HRESULT; stdcall;
  end;

  /// identify each TWebSocketApi late-binding API function
  TWebSocketApis = (
    hAbortHandle, hBeginClientHandshake, hBeginServerHandshake,
    hCompleteAction, hCreateClientHandle, hCreateServerHandle, hDeleteHandle,
    hEndClientHandshake, hEndServerHandshake, hGetAction, hGetGlobalProperty,
    hReceive, hSend);

  /// exception raised during http.sys WebSockets process
  EWebSocketApi = class(ENetSock)
  protected
    fLastError: integer;
    fLastApi: TWebSocketApis;
  public
    /// raise an EWebSocketApi if the http.sys API result code is an error
    class procedure RaiseOnError(api: TWebSocketApis; Error: integer);
    /// initialize a new EWebSocketApi instance
    constructor Create(api: TWebSocketApis; Error: integer); reintroduce; overload;
  published
    /// the error code of this exception
    property LastError: integer
      read fLastError;
    /// the execution context of this exception
    property LastApi: TWebSocketApis
      read fLastApi;
  end;

const
  WEBSOCKET_DLL = 'websocket.dll';
  WebSocketNames: array[TWebSocketApis] of PChar = (
    'WebSocketAbortHandle', 'WebSocketBeginClientHandshake',
    'WebSocketBeginServerHandshake', 'WebSocketCompleteAction',
    'WebSocketCreateClientHandle', 'WebSocketCreateServerHandle',
    'WebSocketDeleteHandle', 'WebSocketEndClientHandshake',
    'WebSocketEndServerHandshake', 'WebSocketGetAction',
    'WebSocketGetGlobalProperty', 'WebSocketReceive', 'WebSocketSend');
  WEB_SOCKET_SEND_ACTION_QUEUE = $1;
  WEB_SOCKET_RECEIVE_ACTION_QUEUE = $2;
  WEB_SOCKET_ALL_ACTION_QUEUE = WEB_SOCKET_SEND_ACTION_QUEUE or
    WEB_SOCKET_RECEIVE_ACTION_QUEUE;

  /// context ID of WebSocket URI group
  WEB_SOCKET_URL_CONTEXT = 1;

  /// the buffer contains the last, and possibly only, part of a UTF-8 message
  WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000000;
  /// the buffer contains part of a UTF-8 message
  WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000001;
  /// the buffer contains the last, and possibly only, part of a binary message
  WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000002;
  /// the buffer contains part of a binary message
  WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000003;
  /// the buffer contains a close message
  WEB_SOCKET_CLOSE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000004;
  /// the buffer contains a ping or pong message
  // - when sending, this value means 'ping'
  // - when processing received data, this value means 'pong'
  WEB_SOCKET_PING_PONG_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000005;
  /// the buffer contains an unsolicited pong message
  WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000006;

  // https://msdn.microsoft.com/en-us/library/windows/desktop/hh449347
  WEB_SOCKET_MAX_CLOSE_REASON_LENGTH = 123;
  /// Close completed successfully
  WEB_SOCKET_SUCCESS_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1000;
  /// The endpoint is going away and thus closing the connection
  WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1001;
  /// Peer detected protocol error and it is closing the connection
  WEB_SOCKET_PROTOCOL_ERROR_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1002;
  /// The endpoint cannot receive this type of data
  WEB_SOCKET_INVALID_DATA_TYPE_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1003;
  /// No close status code was provided
  WEB_SOCKET_EMPTY_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1005;
  /// The connection was closed without sending or receiving a close frame
  WEB_SOCKET_ABORTED_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1006;
  /// Data within a message is not consistent with the type of the message
  WEB_SOCKET_INVALID_PAYLOAD_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1007;
  /// The message violates an endpoint's policy
  WEB_SOCKET_POLICY_VIOLATION_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1008;
  /// The message sent was too large to process
  WEB_SOCKET_MESSAGE_TOO_BIG_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1009;
  /// A client endpoint expected the server to negotiate one or more extensions,
  // but the server didn't return them in the response message of the WebSocket handshake
  WEB_SOCKET_UNSUPPORTED_EXTENSIONS_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1010;
  /// An unexpected condition prevented the server from fulfilling the request
  WEB_SOCKET_SERVER_ERROR_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1011;
  /// The TLS handshake could not be completed
  WEB_SOCKET_SECURE_HANDSHAKE_ERROR_CLOSE_STATUS: WEB_SOCKET_CLOSE_STATUS = 1015;

var
  WebSocketApi: TWebSocketApi;

/// is HTTP.SYS web socket API available on the target system Windows 8 and UP
function WinHttp_WebSocketEnabled: boolean;

/// low-level loading of the WebSockets API
procedure WebSocketApiInitialize;

const
  sProtocolHeader: RawUtf8 = 'SEC-WEBSOCKET-PROTOCOL';

/// retrieve an array of headers from WebSockets low-level information
function HttpSys2ToWebSocketHeaders(const aHttpHeaders: HTTP_REQUEST_HEADERS): WEB_SOCKET_HTTP_HEADER_ARR;

/// retrieve the linefeed separated text from WebSockets array of headers
function WebSocketHeadersToText(const aHeaders: PWEB_SOCKET_HTTP_HEADER;
  const aHeadersCount: integer): RawUtf8;

{$endif USEWININET}

implementation

{$ifdef USEWININET} // compile as a void unit if USEWININET is not defined

uses
  mormot.net.http; // shared HTTP constants and functions


{ ************  http.sys / HTTP Server API low-level direct access }

function RegURL(aRoot, aPort: RawUtf8; Https: boolean; aDomainName: RawUtf8): SynUnicode;
const
  Prefix: array[boolean] of RawUtf8 = (
    'http://', 'https://');
begin
  if aPort = '' then
    aPort := DEFAULT_PORT[Https];
  aRoot := TrimU(aRoot);
  aDomainName := TrimU(aDomainName);
  if aDomainName = '' then
  begin
    result := '';
    exit;
  end;
  if aRoot <> '' then
  begin
    if aRoot[1] <> '/' then
      insert('/', aRoot, 1);
    if aRoot[length(aRoot)] <> '/' then
      aRoot := aRoot + '/';
  end
  else
    aRoot := '/'; // allow for instance 'http://*:2869/'
  aRoot := Prefix[Https] + aDomainName + ':' + aPort + aRoot;
  Utf8ToSynUnicode(aRoot, result);
end;

const
  REMOTEIP_HEADERLEN = 10;
  REMOTEIP_HEADER: string[REMOTEIP_HEADERLEN] = 'RemoteIP: ';

function RetrieveHeaders(const Request: HTTP_REQUEST;
  const RemoteIPHeadUp: RawUtf8; out RemoteIP: RawUtf8): RawUtf8;
var
  i, L, Lip: integer;
  H: THttpHeader;
  P: PHTTP_UNKNOWN_HEADER;
  D: PAnsiChar;
begin
  assert(low(HTTP_KNOWNHEADERS) = low(Request.Headers.KnownHeaders));
  assert(high(HTTP_KNOWNHEADERS) = high(Request.Headers.KnownHeaders));
  // compute remote IP
  L := length(RemoteIPHeadUp);
  if L <> 0 then
  begin
    P := Request.Headers.pUnknownHeaders;
    if P <> nil then
      for i := 1 to Request.Headers.UnknownHeaderCount do
        if (P^.NameLength = L) and
           IdemPChar(P^.pName, Pointer(RemoteIPHeadUp)) then
        begin
          FastSetString(RemoteIP, P^.pRawValue, P^.RawValueLength);
          break;
        end
        else
          inc(P);
  end;
  if (RemoteIP = '') and
     (Request.Address.pRemoteAddress <> nil) then
    RemoteIP := Request.Address.pRemoteAddress.IP(RemoteIPLocalHostAsVoidInServers);
  // compute headers length
  Lip := length(RemoteIP);
  if Lip <> 0 then
    L := (REMOTEIP_HEADERLEN + 2) + Lip
  else
    L := 0;
  for H := low(HTTP_KNOWNHEADERS) to high(HTTP_KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[H].RawValueLength <> 0 then
      inc(L, Request.Headers.KnownHeaders[H].RawValueLength +
        ord(HTTP_KNOWNHEADERS[H][0]) + 4);
  P := Request.Headers.pUnknownHeaders;
  if P <> nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do
    begin
      inc(L, P^.NameLength + P^.RawValueLength + 4); // +4 for each ': '+#13#10
      inc(P);
    end;
  // set headers content
  FastSetString(result{%H-}, nil, L);
  D := pointer(result);
  for H := low(HTTP_KNOWNHEADERS) to high(HTTP_KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[H].RawValueLength <> 0 then
    begin
      MoveFast(HTTP_KNOWNHEADERS[H][1], D^, ord(HTTP_KNOWNHEADERS[H][0]));
      inc(D, ord(HTTP_KNOWNHEADERS[H][0]));
      PWord(D)^ := ord(':') + ord(' ') shl 8;
      inc(D, 2);
      MoveFast(Request.Headers.KnownHeaders[H].pRawValue^,
        D^, Request.Headers.KnownHeaders[H].RawValueLength);
      inc(D, Request.Headers.KnownHeaders[H].RawValueLength);
      PWord(D)^ := 13 + 10 shl 8;
      inc(D, 2);
    end;
  P := Request.Headers.pUnknownHeaders;
  if P <> nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do
    begin
      MoveFast(P^.pName^, D^, P^.NameLength);
      inc(D, P^.NameLength);
      PWord(D)^ := ord(':') + ord(' ') shl 8;
      inc(D, 2);
      MoveFast(P^.pRawValue^, D^, P^.RawValueLength);
      inc(D, P^.RawValueLength);
      inc(P);
      PWord(D)^ := 13 + 10 shl 8;
      inc(D, 2);
    end;
  if Lip <> 0 then
  begin
    MoveFast(REMOTEIP_HEADER[1], D^, REMOTEIP_HEADERLEN);
    inc(D, REMOTEIP_HEADERLEN);
    MoveFast(pointer(RemoteIP)^, D^, Lip);
    inc(D, Lip);
    PWord(D)^ := 13 + 10 shl 8;
  end;
end;

procedure HttpApiInitialize;
var
  api: THttpApis;
  P: PPointer;
begin
  if Http.Module <> 0 then
    exit; // already loaded
  mormot.core.os.GlobalLock;
  try
    if Http.Module = 0 then
    try
      Http.Module := LoadLibrary(HTTPAPI_DLL);
      Http.Version.MajorVersion := 2; // API 2.0 if all functions are available
      if Http.Module <= 255 then
        raise EHttpApiServer.CreateFmt('Unable to find %s', [HTTPAPI_DLL]);
      P := @@Http.Initialize;
      for api := low(api) to high(api) do
      begin
        P^ := GetProcAddress(Http.Module, HttpNames[api]);
        if P^ = nil then
          if api < hHttpApi2First then
            raise EHttpApiServer.CreateFmt('Unable to find %s() in %s',
              [HttpNames[api], HTTPAPI_DLL])
          else
            Http.Version.MajorVersion := 1; // e.g. Windows XP or Server 2003
        inc(P);
      end;
    except
      on E: Exception do
      begin
        if Http.Module > 255 then
        begin
          FreeLibrary(Http.Module);
          Http.Module := 0;
        end;
        raise;
      end;
    end;
  finally
    mormot.core.os.GlobalUnlock;
  end;
end;



{ EHttpApiServer }

class procedure EHttpApiServer.RaiseOnError(api: THttpApis; Error: integer);
begin
  if Error <> NO_ERROR then
    raise self.Create(api, Error);
end;

constructor EHttpApiServer.Create(api: THttpApis; Error: integer);
begin
  fLastError := Error;
  fLastApi := api;
  inherited CreateFmt('%s failed: %s (%d)', [HttpNames[api],
    SysErrorMessagePerModule(Error, HTTPAPI_DLL), Error])
end;



{ HTTP_RESPONSE }

procedure HTTP_RESPONSE.SetStatus(code: integer; var OutStatus: RawUtf8);
begin
  StatusCode := code;
  StatusCodeToReason(code, OutStatus);
  ReasonLength := length(OutStatus);
  pReason := pointer(OutStatus);
end;

procedure HTTP_RESPONSE.SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
  const Content: RawByteString; const ContentType: RawUtf8);
begin
  FillcharFast(DataChunk, sizeof(DataChunk), 0);
  if ContentType <> '' then
  begin
    Headers.KnownHeaders[reqContentType].RawValueLength := length(ContentType);
    Headers.KnownHeaders[reqContentType].pRawValue := pointer(ContentType);
  end;
  if Content = '' then
    exit;
  DataChunk.DataChunkType := hctFromMemory;
  DataChunk.pBuffer := pointer(Content);
  DataChunk.BufferLength := length(Content);
  EntityChunkCount := 1;
  pEntityChunks := @DataChunk;
end;

{$ifndef NOXPOWEREDNAME}
const
  XPN: PUtf8Char = XPOWEREDNAME;
  XPV: PUtf8Char = XPOWEREDVALUE;
{$endif NOXPOWEREDNAME}

procedure HTTP_RESPONSE.SetHeaders(P: PUtf8Char;
  var UnknownHeaders: HTTP_UNKNOWN_HEADERS);
begin
  Headers.pUnknownHeaders := pointer(UnknownHeaders);
  {$ifdef NOXPOWEREDNAME}
  Headers.UnknownHeaderCount := 0;
  {$else}
  with UnknownHeaders[0] do
  begin
    pName := XPN;
    NameLength := length(XPOWEREDNAME);
    pRawValue := XPV;
    RawValueLength := length(XPOWEREDVALUE);
  end;
  Headers.UnknownHeaderCount := 1;
  {$endif NOXPOWEREDNAME}
  if P <> nil then
    repeat
      while ord(P^) in [10, 13] do
        inc(P);
      if P^ = #0 then
        break;
      P := AddCustomHeader(P, UnknownHeaders, false);
    until false;
end;

function HTTP_RESPONSE.AddCustomHeader(P: PUtf8Char;
  var UnknownHeaders: HTTP_UNKNOWN_HEADERS;
  ForceCustomHeader: boolean): PUtf8Char;
const
  KNOWNHEADERS: array[reqCacheControl..respWwwAuthenticate] of PAnsiChar = (
    'CACHE-CONTROL:', 'CONNECTION:', 'DATE:', 'KEEP-ALIVE:', 'PRAGMA:',
    'TRAILER:', 'TRANSFER-ENCODING:', 'UPGRADE:', 'VIA:', 'WARNING:', 'ALLOW:',
    'CONTENT-LENGTH:', 'CONTENT-TYPE:', 'CONTENT-ENCODING:', 'CONTENT-LANGUAGE:',
    'CONTENT-LOCATION:', 'CONTENT-MD5:', 'CONTENT-RANGE:', 'EXPIRES:',
    'LAST-MODIFIED:', 'ACCEPT-RANGES:', 'AGE:', 'ETAG:', 'LOCATION:',
    'PROXY-AUTHENTICATE:', 'RETRY-AFTER:', 'SERVER:', 'SET-COOKIE:', 'VARY:',
    'WWW-AUTHENTICATE:');
var
  UnknownName: PUtf8Char;
  i: integer;
begin
  if ForceCustomHeader then
    i := -1
  else
    i := IdemPCharArray(P, KNOWNHEADERS);
  // WebSockets need CONNECTION as unknown header
  if (i >= 0) and
     (THttpHeader(i) <> reqConnection) then
    with Headers.KnownHeaders[THttpHeader(i)] do
    begin
      while P^ <> ':' do
        inc(P);
      inc(P); // jump ':'
      while P^ = ' ' do
        inc(P);
      pRawValue := pointer(P);
      while P^ >= ' ' do
        inc(P);
      RawValueLength := P - pRawValue;
    end
  else
  begin
    UnknownName := pointer(P);
    while (P^ >= ' ') and
          (P^ <> ':') do
      inc(P);
    if P^ = ':' then
      with UnknownHeaders[Headers.UnknownHeaderCount] do
      begin
        pName := UnknownName;
        NameLength := P - pName;
        repeat
          inc(P)
        until P^ <> ' ';
        pRawValue := pointer(P);
        while P^ >= ' ' do
          inc(P);
        RawValueLength := P - pRawValue;
        if Headers.UnknownHeaderCount = high(UnknownHeaders) then
        begin
          SetLength(UnknownHeaders, Headers.UnknownHeaderCount + 32);
          Headers.pUnknownHeaders := pointer(UnknownHeaders);
        end;
        inc(Headers.UnknownHeaderCount);
      end
    else
      while P^ >= ' ' do
        inc(P);
  end;
  result := P;
end;


{ ******************** WinINet API Additional Wrappers }

function SysErrorMessageWinInet(error: integer): string;
var
  dwError, tmpLen: DWORD;
  tmp: string;
begin
  result := SysErrorMessagePerModule(error, 'wininet.dll');
  if error = ERROR_INTERNET_EXTENDED_ERROR then
  begin
    InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError, nil, tmpLen);
    if tmpLen > 0 then
    begin
      SetLength(tmp, tmpLen);
      InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError, PChar(tmp), tmpLen);
      result := result + ' [' + tmp + ']';
    end;
  end;
end;

procedure GetDomainUserNameFromToken(UserToken: THandle; var result: RawUtf8);
var
  Buffer: array[0..511] of byte;
  BufferSize, UserSize, DomainSize: DWORD;
  UserInfo: PSIDAndAttributes;
  NameUse: {$ifdef FPC}SID_NAME_USE{$else}cardinal{$endif};
  tmp: SynUnicode;
  P: PWideChar;
begin
  if not GetTokenInformation(UserToken, TokenUser, @Buffer, SizeOf(Buffer),
    BufferSize) then
    exit;
  UserInfo := @Buffer;
  UserSize := 0;
  DomainSize := 0;
  LookupAccountSidW(nil, UserInfo^.Sid, nil, UserSize, nil, DomainSize, NameUse);
  if (UserSize = 0) or
     (DomainSize = 0) then
    exit;
  SetLength(tmp, UserSize + DomainSize - 1);
  P := pointer(tmp);
  if not LookupAccountSidW(
     nil, UserInfo^.Sid, P + DomainSize, UserSize, P, DomainSize, NameUse) then
    exit;
  P[DomainSize] := '\';
  result := SynUnicodeToUtf8(tmp);
end;



{ ******************** winhttp.dll Windows API Definitions }

procedure WinHttpSecurityErrorCallback(hInternet: hInternet; dwContext: PDWORD;
  dwInternetStatus: cardinal; lpvStatusInformation: pointer;
  dwStatusInformationLength: cardinal); stdcall;
begin
  // in case lpvStatusInformation^=-2147483648 this is attempt to connect to
  // non-https socket wrong port - perhaps must be 443?
  raise EWinHttp.CreateFmt('WinHttp security error. Status %d, statusInfo: %d',
    [dwInternetStatus, PDWORD(lpvStatusInformation)^]);
end;

function WinHttpGetProxyInfo(const URL: SynUnicode;
  out ProxyInfo: TProxyInfo): DWORD;
// see https://stackoverflow.com/a/8961399/458259
var
  Session: HINTERNET;
  AutoDetectProxy: boolean;
  WinHttpProxyInfo: WINHTTP_PROXY_INFO;
  AutoProxyOptions: WINHTTP_AUTOPROXY_OPTIONS;
  IEProxyConfig: WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;
begin
  result := 0;
  ProxyInfo.URL := '';
  ProxyInfo.Bypass := '';
  ProxyInfo.ErrorMessage := '';
  ProxyInfo.AutoDetected := false;
  FillCharFast(WinHttpProxyInfo, SizeOf(WinHttpProxyInfo), 0);
  FillCharFast(AutoProxyOptions, SizeOf(AutoProxyOptions), 0);
  FillCharFast(IEProxyConfig, SizeOf(IEProxyConfig), 0);
  AutoDetectProxy := false;
  // check if the Internet Explorer's proxy configuration is
  // available and if so, check its settings to auto-detect
  // proxy settings and auto-config script URL options
  if WinHttpApi.GetIEProxyConfigForCurrentUser(IEProxyConfig) then
  begin
    // if the Internet Explorer is configured to auto-detect
    // proxy settings then we try to detect them later on
    if IEProxyConfig.fAutoDetect then
    begin
      AutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
      AutoProxyOptions.dwAutoDetectFlags :=
        WINHTTP_AUTO_DETECT_TYPE_DHCP or
        WINHTTP_AUTO_DETECT_TYPE_DNS_A;
      AutoDetectProxy := true;
    end;
    // if the Internet Explorer is configured to use the proxy
    // auto-config script then we try to use it
    if IEProxyConfig.lpszAutoConfigURL <> '' then
    begin
      AutoProxyOptions.dwFlags :=
        AutoProxyOptions.dwFlags or WINHTTP_AUTOPROXY_CONFIG_URL;
      AutoProxyOptions.lpszAutoConfigUrl := IEProxyConfig.lpszAutoConfigUrl;
      AutoDetectProxy := true;
    end;
    // if IE doesn't have auto-detect or auto-config set, we are
    // done here and we can fill the AProxyInfo with the IE settings
    if not AutoDetectProxy then
    begin
      ProxyInfo.URL := IEProxyConfig.lpszProxy;
      ProxyInfo.Bypass := IEProxyConfig.lpszProxyBypass;
      ProxyInfo.AutoDetected := false;
    end;
  end
  else
  begin
    // if the Internet Explorer's proxy configuration is not
    // available, then try to auto-detect it
    AutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
    AutoProxyOptions.dwAutoDetectFlags :=
      WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
    AutoDetectProxy := true;
  end;

  // if the IE proxy settings are not available or IE has
  // configured auto-config script or auto-detect proxy settings
  if AutoDetectProxy then
  begin
    // create a temporary WinHttp session to allow the WinHTTP
    // auto-detect proxy settings if possible
    Session := WinHttpApi.Open(nil, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
      WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
    // if the WinHttp session has been created then try to
    // get the proxy data for the specified URL else we assign
    // the last error code to the function result
    if Assigned(Session) then
    try
      // get the proxy data for the specified URL with the
      // auto-proxy options specified, if succeed then we can
      // fill the ProxyInfo with the retrieved settings else
      // we assign the last error code to the function result
      if WinHttpApi.GetProxyForUrl(Session, pointer(URL),
          @AutoProxyOptions, WinHttpProxyInfo) then
      begin
        ProxyInfo.URL := WinHttpProxyInfo.lpszProxy;
        ProxyInfo.Bypass := WinHttpProxyInfo.lpszProxyBypass;
        ProxyInfo.AutoDetected := True;
      end
      else
        result := GetLastError;
    finally
      // close the temporary WinHttp session
      WinHttpApi.CloseHandle(Session);
    end
    else
      result := GetLastError;
  end;
  if result <> 0 then
    ProxyInfo.ErrorMessage := SysErrorMessagePerModule(result, winhttpdll);
end;


procedure WinHttpApiInitialize(RaiseOnError: boolean);
var
  api: TWinHttpApis;
  P: PPointer;
begin
  if WinHttpApi.LibraryHandle <> 0 then
    exit; // already loaded
  mormot.core.os.GlobalLock;
  try
    if WinHttpApi.LibraryHandle <> 0 then
      exit; // thread-safe test
    WinHttpApi.LibraryHandle := SafeLoadLibrary(winhttpdll);
    if WinHttpApi.LibraryHandle = 0 then
      if RaiseOnError then
        raise EWinHttp.CreateFmt('Unable to load library %s', [winhttpdll])
      else
        exit;
    WinHttpApi.WebSocketEnabled := true; // set to false on old Windows
    P := @@WinHttpApi.Open;
    for api := low(api) to high(api) do
    begin
      P^ := GetProcAddress(WinHttpApi.LibraryHandle, WinHttpNames[api]);
      if P^ = nil then
        if api < hWebSocketApiFirst then
        begin
          FreeLibrary(WinHttpApi.LibraryHandle);
          WinHttpApi.LibraryHandle := 0;
          if RaiseOnError then
            raise EWinHttp.CreateFmt('Unable to find %s() export in %s',
              [WinHttpNames[api], winhttpdll]);
        end
        else
          // no WebSockets client e.g. on systems older than Windows 8
          WinHttpApi.WebSocketEnabled := false;
      inc(P);
    end;
    if WinHttpApi.WebSocketEnabled then
      WebSocketApiInitialize
    else
      WebSocketApi.WebSocketEnabled := false;
  finally
    mormot.core.os.GlobalUnlock;
  end;
end;



{ ******************** websocket.dll Windows API Definitions }

procedure WebSocketApiInitialize;
var
  api: TWebSocketApis;
  P: PPointer;
begin
  if WebSocketApi.LibraryHandle <> 0 then
    exit; // already loaded
  WebSocketApi.WebSocketEnabled := false;
  WebSocketApi.LibraryHandle := SafeLoadLibrary(WEBSOCKET_DLL);
  if WebSocketApi.LibraryHandle = 0 then
    exit;
  P := @@WebSocketApi.AbortHandle;
  for api := low(api) to high(api) do
  begin
    P^ := GetProcAddress(WebSocketApi.LibraryHandle, WebSocketNames[api]);
    if P^ = nil then
    begin
      FreeLibrary(WebSocketApi.LibraryHandle);
      WebSocketApi.LibraryHandle := 0;
      exit;
    end;
    inc(P);
  end;
  WebSocketApi.WebSocketEnabled := true;
end;

function WinHttp_WebSocketEnabled: boolean;
begin
  result := WebSocketApi.WebSocketEnabled;
end;

function HttpSys2ToWebSocketHeaders(
  const aHttpHeaders: HTTP_REQUEST_HEADERS): WEB_SOCKET_HTTP_HEADER_ARR;
var
  headerCnt: integer;
  i: PtrInt;
  h: THttpHeader;
  p: PHTTP_UNKNOWN_HEADER;
  r: PWEB_SOCKET_HTTP_HEADER;
begin
  result := nil;
  headerCnt := 0;
  for h := Low(HTTP_KNOWNHEADERS) to High(HTTP_KNOWNHEADERS) do
    if aHttpHeaders.KnownHeaders[h].RawValueLength <> 0 then
      inc(headerCnt);
  p := aHttpHeaders.pUnknownHeaders;
  if p <> nil then
    inc(headerCnt, aHttpHeaders.UnknownHeaderCount);
  SetLength(result, headerCnt);
  r := pointer(result);
  for h := Low(HTTP_KNOWNHEADERS) to High(HTTP_KNOWNHEADERS) do
    if aHttpHeaders.KnownHeaders[h].RawValueLength <> 0 then
    begin
      r^.pcName := @HTTP_KNOWNHEADERS[h][1];
      r^.ulNameLength := ord(HTTP_KNOWNHEADERS[h][0]);
      r^.pcValue := aHttpHeaders.KnownHeaders[h].pRawValue;
      r^.ulValueLength := aHttpHeaders.KnownHeaders[h].RawValueLength;
      inc(r);
    end;
  p := aHttpHeaders.pUnknownHeaders;
  if p <> nil then
    for i := 1 to aHttpHeaders.UnknownHeaderCount do
    begin
      r^.pcName := pointer(p^.pName);
      r^.ulNameLength := p^.NameLength;
      r^.pcValue := pointer(p^.pRawValue);
      r^.ulValueLength := p^.RawValueLength;
      inc(r);
      inc(p);
    end;
end;

function WebSocketHeadersToText(const aHeaders: PWEB_SOCKET_HTTP_HEADER; const
  aHeadersCount: integer): RawUtf8;
var
  i: integer;
  h: PWEB_SOCKET_HTTP_HEADER;
  len: integer;
  d: PAnsiChar;
begin
  len := 0;
  h := aHeaders;
  for i := 1 to aHeadersCount do
  begin
    if h^.ulValueLength <> 0 then
      inc(len, h^.ulNameLength + h^.ulValueLength + 4);
    inc(h);
  end;
  FastSetString(result{%H-}, nil, len);
  d := Pointer(result);
  h := aHeaders;
  for i := 1 to aHeadersCount do
  begin
    if h^.ulValueLength <> 0 then
    begin
      MoveFast(h^.pcName^, d^, h^.ulNameLength);
      inc(d, h^.ulNameLength);
      PWord(d)^ := Ord(':') + Ord(' ') shl 8;
      inc(d, 2);
      MoveFast(h^.pcValue^, d^, h^.ulValueLength);
      inc(d, h^.ulValueLength);
      PWord(d)^ := 13 + 10 shl 8;
      inc(d, 2);
    end;
    inc(h);
  end;
  Assert(d - Pointer(result) = len);
end;


{ EWebSocketApi }

class procedure EWebSocketApi.RaiseOnError(api: TWebSocketApis; Error: integer);
begin
  if Error <> NO_ERROR then
    raise self.Create(api, Error);
end;

constructor EWebSocketApi.Create(api: TWebSocketApis; Error: integer);
begin
  fLastError := Error;
  fLastApi := api;
  inherited CreateFmt('%s failed: %s (%d)', [WebSocketNames[api],
    SysErrorMessagePerModule(Error, WEBSOCKET_DLL), Error])
end;

const
  // paranoid check of the API mapping against our internal enumerations
  HTTP_LOG_FIELD_TEST_SUB_STATUS: THttpApiLogFields = [hlfSubStatus];
  
initialization
  Assert(
    {$ifdef CPU64}
    (sizeof(HTTP_REQUEST) = 864) and
    (sizeof(HTTP_SSL_INFO) = 48) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY) = 32) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE) = 32) and
    (sizeof(HTTP_REQUEST_HEADERS) = 688) and
    (sizeof(HTTP_RESPONSE_HEADERS) = 512) and
    (sizeof(HTTP_COOKED_URL) = 40) and
    (sizeof(HTTP_RESPONSE) = 568) and
    {$else}
    (sizeof(HTTP_REQUEST) = 472) and
    (sizeof(HTTP_SSL_INFO) = 28) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY) = 24) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE) = 32) and
    (sizeof(HTTP_RESPONSE) = 288) and
    (sizeof(HTTP_REQUEST_HEADERS) = 344) and
    (sizeof(HTTP_RESPONSE_HEADERS) = 256) and
    (sizeof(HTTP_COOKED_URL) = 24) and
    {$endif CPU64}
    (ord(reqUserAgent) = 40) and
    (ord(respLocation) = 23) and
    (sizeof(THttpHeader) = 4) and
    (integer(HTTP_LOG_FIELD_TEST_SUB_STATUS) = HTTP_LOG_FIELD_SUB_STATUS)
  );
  WinHttpApiInitialize({RaiseOnError=}false);

{$endif USEWININET}

end.

