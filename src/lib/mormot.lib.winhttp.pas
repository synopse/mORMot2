/// Windows HTTP and WebSockets API Libraries
// - this unit is a part of the freeware Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.winhttp;

{
  *****************************************************************************

   Windows HTTP and WebSockets API Libraries
   - WinINet API Additional Wrappers
   - winhttp.dll Windows API Definitions
   - websocket.dll Windows API Definitions
   - http.sys / HTTP API low-level direct access

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef USEWININET}

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


{ ************  http.sys / HTTP API low-level direct access }

{$MINENUMSIZE 4}
{$A+}

{$ifdef FPC}
{$PACKRECORDS C}
{$endif}

type
  {$ifndef UNICODE} // circumvent oldest Delphi limitation
  ULONGLONG = Int64;
  {$endif}

  HTTP_OPAQUE_ID = ULONGLONG;

  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;

  HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;

  HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;

  /// http.sys API 2.0 logging file supported layouts
  // - match low-level HTTP_LOGGING_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingType = (
    hltW3C, hltIIS, hltNCSA, hltRaw);

  /// http.sys API 2.0 logging file rollover types
  // - match low-level HTTP_LOGGING_ROLLOVER_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingRollOver = (hlrSize,
    hlrDaily, hlrWeekly, hlrMonthly, hlrHourly);

  /// http.sys API 2.0 logging option flags
  // - used to alter the default logging behavior
  // - hlfLocalTimeRollover would force the log file rollovers by local time,
  // instead of the default GMT time
  // - hlfUseUTF8Conversion will use UTF-8 instead of default local code page
  // - only one of hlfLogErrorsOnly and hlfLogSuccessOnly flag could be set
  // at a time: if neither of them are present, both errors and success will
  // be logged, otherwise mutually exclusive flags could be set to force only
  // errors or success logging
  // - match low-level HTTP_LOGGING_FLAG_* constants as defined in HTTP 2.0 API
  THttpApiLoggingFlags = set of (
    hlfLocalTimeRollover, hlfUseUTF8Conversion, hlfLogErrorsOnly, hlfLogSuccessOnly);

  /// http.sys API 2.0 fields used for W3C logging
  // - match low-level HTTP_LOG_FIELD_* constants as defined in HTTP 2.0 API
  THttpApiLogFields = set of (
    hlfDate, hlfTime, hlfClientIP, hlfUserName, hlfSiteName, hlfComputerName,
    hlfServerIP, hlfMethod, hlfURIStem, hlfURIQuery, hlfStatus, hlfWIN32Status,
    hlfBytesSent, hlfBytesRecv, hlfTimeTaken, hlfServerPort, hlfUserAgent,
    hlfCookie, hlfReferer, hlfVersion, hlfHost, hlfSubStatus);

  /// http.sys API 2.0 fields used for server-side authentication
  // - as used by THttpApiServer.SetAuthenticationSchemes/AuthenticationSchemes
  // - match low-level HTTP_AUTH_ENABLE_* constants as defined in HTTP 2.0 API
  THttpApiRequestAuthentications = set of (
    haBasic, haDigest, haNtlm, haNegotiate, haKerberos);

type
  // HTTP version used
  HTTP_VERSION = packed record
    MajorVersion: word;
    MinorVersion: word;
  end;

  // the req* values identify Request Headers, and resp* Response Headers
  THttpHeader = (reqCacheControl, reqConnection, reqDate, reqKeepAlive,
    reqPragma, reqTrailer, reqTransferEncoding, reqUpgrade, reqVia, reqWarning,
    reqAllow, reqContentLength, reqContentType, reqContentEncoding,
    reqContentLanguage, reqContentLocation, reqContentMd5, reqContentRange,
    reqExpires, reqLastModified, reqAccept, reqAcceptCharset, reqAcceptEncoding,
    reqAcceptLanguage, reqAuthorization, reqCookie, reqExpect, reqFrom, reqHost,
    reqIfMatch, reqIfModifiedSince, reqIfNoneMatch, reqIfRange,
    reqIfUnmodifiedSince, reqMaxForwards, reqProxyAuthorization, reqReferrer,
    reqRange, reqTe, reqTranslate, reqUserAgent, respAcceptRanges = 20, respAge,
    respEtag, respLocation, respProxyAuthenticate, respRetryAfter, respServer,
    respSetCookie, respVary, respWwwAuthenticate);

  THttpVerb = (
    hvUnparsed, hvUnknown, hvInvalid,
    hvOPTIONS, hvGET, hvHEAD, hvPOST, hvPUT, hvDELETE, hvTRACE, hvCONNECT,
    hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
    hvMOVE, hvCOPY, hvPROPFIND, hvPROPPATCH, hvMKCOL, hvLOCK, hvUNLOCK, hvSEARCH,
    hvMaximum);

  THttpChunkType = (
    hctFromMemory, hctFromFileHandle, hctFromFragmentCache);

  THttpServiceConfigID = (
    hscIPListenList, hscSSLCertInfo, hscUrlAclInfo, hscMax);

  THttpServiceConfigQueryType = (
    hscQueryExact, hscQueryNext, hscQueryMax);

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
    pName: PAnsiChar;          // The header name (minus the ':' character)
    pRawValue: PAnsiChar;      // The header value
  end;

  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

  HTTP_UNKNOWN_HEADERs = array of HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    RawValueLength: word;     // in bytes not including the #0
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

  HTTP_REQUEST_INFO_TYPE = (HttpRequestInfoTypeAuth,
    HttpRequestInfoTypeChannelBind, HttpRequestInfoTypeSslProtocol,
    HttpRequestInfoTypeSslTokenBindingDraft, HttpRequestInfoTypeSslTokenBinding,
    HttpRequestInfoTypeRequestTiming, HttpRequestInfoTypeTcpInfoV0,
    HttpRequestInfoTypeRequestSizing, HttpRequestInfoTypeQuicStats,
    HttpRequestInfoTypeTcpInfoV1);

  // about Authentication in HTTP Version 2.0
  // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
  HTTP_AUTH_STATUS = (HttpAuthStatusSuccess, HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure);

  HTTP_REQUEST_AUTH_TYPE = (HttpRequestAuthTypeNone, HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest, HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate, HttpRequestAuthTypeKerberos);

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

  HTTP_RESPONSE_INFO_TYPE = (HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty, HttpResponseInfoTypeQosProperty,
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
    pReason: PUTF8Char;
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
    procedure SetStatus(code: integer; var OutStatus: RawUTF8);
    // will set the content of the reponse, and ContentType header
    procedure SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY; const Content:
      RawByteString; const ContentType: RawUTF8 = 'text/html');
    /// will set all header values from lines
    // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
    // - all other headers will be set in temp UnknownHeaders[]
    procedure SetHeaders(P: PUTF8Char; var UnknownHeaders: HTTP_UNKNOWN_HEADERs);
    /// add one header value to the internal headers
    // - SetHeaders() method should have been called before to initialize the
    // internal UnknownHeaders[] array
    function AddCustomHeader(P: PUTF8Char; var UnknownHeaders:
      HTTP_UNKNOWN_HEADERs; ForceCustomHeader: boolean): PUTF8Char;
  end;

  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_PROPERTY_FLAGS = ULONG;

  HTTP_ENABLED_STATE = (HttpEnabledStateActive, HttpEnabledStateInactive);

  PHTTP_ENABLED_STATE = ^HTTP_ENABLED_STATE;

  HTTP_STATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    State: HTTP_ENABLED_STATE;
  end;

  PHTTP_STATE_INFO = ^HTTP_STATE_INFO;

  THTTP_503_RESPONSE_VERBOSITY = (Http503ResponseVerbosityBasic,
    Http503ResponseVerbosityLimited, Http503ResponseVerbosityFull);

  PHTTP_503_RESPONSE_VERBOSITY = ^THTTP_503_RESPONSE_VERBOSITY;

  HTTP_QOS_SETTING_TYPE = (HttpQosSettingTypeBandwidth,
    HttpQosSettingTypeConnectionLimit, HttpQosSettingTypeFlowRate // Windows Server 2008 R2 and Windows 7 only.
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
  HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (IdleConnectionTimeout, HeaderWaitTimeout);

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

  HTTP_SERVICE_BINDING_TYPE = (HttpServiceBindingTypeNone,
    HttpServiceBindingTypeW, HttpServiceBindingTypeA);

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

  HTTP_AUTHENTICATION_HARDENING_LEVELS = (HttpAuthenticationHardeningLegacy,
    HttpAuthenticationHardeningMedium, HttpAuthenticationHardeningStrict);

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
   //      will be converted to UTF8 multibytes when writting to the log
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
  HTTP_LOGGING_TYPE = (HttpLoggingTypeW3C, HttpLoggingTypeIIS,
    HttpLoggingTypeNCSA, HttpLoggingTypeRaw);

  HTTP_LOGGING_ROLLOVER_TYPE = (HttpLoggingRolloverSize,
    HttpLoggingRolloverDaily, HttpLoggingRolloverWeekly,
    HttpLoggingRolloverMonthly, HttpLoggingRolloverHourly);

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

  HTTP_LOG_DATA_TYPE = (HttpLogDataTypeFields);

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

  HTTP_PROTECTION_LEVEL_TYPE = (HttpProtectionLevelUnrestricted,
    HttpProtectionLevelEdgeRestricted, HttpProtectionLevelRestricted);

  HTTP_PROTECTION_LEVEL_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    Level: HTTP_PROTECTION_LEVEL_TYPE;
  end;

  PHTTP_PROTECTION_LEVEL_INFO = ^HTTP_PROTECTION_LEVEL_INFO;

const
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

  /// direct late-binding access to the WinHTTP API
  // - note: WebSocket* API calls require Windows 8 and later
  TWinHTTPBinding = packed record
    /// access to the winhttp.dll loaded library
    LibraryHandle: THandle;
    /// depends on the published .dll functions
    WebSocketEnabled: Boolean;
    /// Initializes an application's use of the WinHTTP functions.
    Open: function(pwszUserAgent: PWideChar; dwAccessType: DWORD; pwszProxyName,
      pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall;
    /// Sets up a callback function that WinHTTP can call as progress is made during an operation.
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
  WinHttpAPI: TWinHTTPBinding;

type
  TWinHttpAPIs = (hOpen, hSetStatusCallback, hConnect, hOpenRequest,
    hCloseHandle, hAddRequestHeaders, hSendRequest, hReceiveResponse,
    hQueryHeaders, hQueryDataAvailable, hReadData, hSetTimeouts, hSetOption,
    hSetCredentials, hWebSocketCompleteUpgrade, hWebSocketClose,
    hWebSocketQueryCloseStatus, hWebSocketSend, hWebSocketReceive, hWriteData);

const
  hWebSocketApiFirst = hWebSocketCompleteUpgrade;

const
  WinHttpNames: array[TWinHttpAPIs] of PChar = (
    'WinHttpOpen', 'WinHttpSetStatusCallback', 'WinHttpConnect',
    'WinHttpOpenRequest', 'WinHttpCloseHandle', 'WinHttpAddRequestHeaders',
    'WinHttpSendRequest', 'WinHttpReceiveResponse', 'WinHttpQueryHeaders',
    'WinHttpQueryDataAvailable', 'WinHttpReadData', 'WinHttpSetTimeouts',
    'WinHttpSetOption', 'WinHttpSetCredentials', 'WinHttpWebSocketCompleteUpgrade',
    'WinHttpWebSocketClose', 'WinHttpWebSocketQueryCloseStatus',
    'WinHttpWebSocketSend', 'WinHttpWebSocketReceive', 'WinHttpWriteData');


{ ******************** websocket.dll Windows API Definitions }

type
  WEB_SOCKET_PROPERTY_TYPE = (WEB_SOCKET_RECEIVE_BUFFER_SIZE_PROPERTY_TYPE, //0
    WEB_SOCKET_SEND_BUFFER_SIZE_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_MASKING_PROPERTY_TYPE, WEB_SOCKET_ALLOCATED_BUFFER_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_UTF8_VERIFICATION_PROPERTY_TYPE,
    WEB_SOCKET_KEEPALIVE_INTERVAL_PROPERTY_TYPE,
    WEB_SOCKET_SUPPORTED_VERSIONS_PROPERTY_TYPE);

  WEB_SOCKET_ACTION_QUEUE = Cardinal;

  WEB_SOCKET_ACTION = (WEB_SOCKET_NO_ACTION, //0
    WEB_SOCKET_SEND_TO_NETWORK_ACTION, WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION,
    WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION, WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION);

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
  TWebSocketAPI = packed record
    /// acces to the loaded library handle
    LibraryHandle: THandle;
    /// depends on Windows version
    WebSocketEnabled: Boolean;
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

  /// identify each TWebSocketAPI late-binding API function
  TWebSocketAPIs = (hAbortHandle, hBeginClientHandshake, hBeginServerHandshake,
    hCompleteAction, hCreateClientHandle, hCreateServerHandle, hDeleteHandle,
    hEndClientHandshake, hEndServerHandshake, hGetAction, hGetGlobalProperty,
    hReceive, hSend);

const
  WEBSOCKET_DLL = 'websocket.dll';
  WebSocketNames: array[TWebSocketAPIs] of PChar = ('WebSocketAbortHandle',
    'WebSocketBeginClientHandshake', 'WebSocketBeginServerHandshake',
    'WebSocketCompleteAction', 'WebSocketCreateClientHandle',
    'WebSocketCreateServerHandle', 'WebSocketDeleteHandle',
    'WebSocketEndClientHandshake', 'WebSocketEndServerHandshake',
    'WebSocketGetAction', 'WebSocketGetGlobalProperty', 'WebSocketReceive',
    'WebSocketSend');
  WEB_SOCKET_SEND_ACTION_QUEUE = $1;
  WEB_SOCKET_RECEIVE_ACTION_QUEUE = $2;
  WEB_SOCKET_ALL_ACTION_QUEUE = WEB_SOCKET_SEND_ACTION_QUEUE or
    WEB_SOCKET_RECEIVE_ACTION_QUEUE;

  /// context ID of WebSocket URI group
  WEB_SOCKET_URL_CONTEXT = 1;

  /// the buffer contains the last, and possibly only, part of a UTF8 message
  WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000000;
  /// the buffer contains part of a UTF8 message
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
  WebSocketAPI: TWebSocketAPI;

/// is HTTP.SYS web socket API available on the target system Windows 8 and UP
function WinHTTP_WebSocketEnabled: boolean;

/// low-level loading of the WebSockets API
procedure WebSocketApiInitialize;


{$endif USEWININET}

implementation

{$ifdef USEWININET}

uses
  mormot.net.http; // shared HTTP constants and functions


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


{ ******************** winhttp.dll Windows API Definitions }

{ ******************** websocket.dll Windows API Definitions }

procedure WebSocketApiInitialize;
var
  api: TWebSocketAPIs;
  P: PPointer;
begin
  if WebSocketAPI.LibraryHandle <> 0 then
    exit; // already loaded
  WebSocketAPI.WebSocketEnabled := false;
  WebSocketAPI.LibraryHandle := SafeLoadLibrary(WEBSOCKET_DLL);
  if WebSocketAPI.LibraryHandle = 0 then
    exit;
  P := @@WebSocketAPI.AbortHandle;
  for api := low(api) to high(api) do
  begin
    P^ := GetProcAddress(WebSocketAPI.LibraryHandle, WebSocketNames[api]);
    if P^ = nil then
    begin
      FreeLibrary(WebSocketAPI.LibraryHandle);
      WebSocketAPI.LibraryHandle := 0;
      exit;
    end;
    inc(P);
  end;
  WebSocketAPI.WebSocketEnabled := true;
end;

function WinHTTP_WebSocketEnabled: boolean;
begin
  Result := WebSocketAPI.WebSocketEnabled;
end;



{ HTTP_RESPONSE }

procedure HTTP_RESPONSE.SetStatus(code: integer; var OutStatus: RawUTF8);
begin
  StatusCode := code;
  OutStatus := StatusCodeToReason(code);
  ReasonLength := length(OutStatus);
  pReason := pointer(OutStatus);
end;

procedure HTTP_RESPONSE.SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
  const Content: RawByteString; const ContentType: RawUTF8);
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

procedure HTTP_RESPONSE.SetHeaders(P: PUTF8Char; var UnknownHeaders:
  HTTP_UNKNOWN_HEADERs);
{$ifndef NOXPOWEREDNAME}
const
  XPN: PAnsiChar = XPOWEREDNAME;
  XPV: PAnsiChar = XPOWEREDVALUE;
{$endif}
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
  {$endif}
  if P <> nil then
    repeat
      while ord(P^) in [10, 13] do
        inc(P);
      if P^ = #0 then
        break;
      P := AddCustomHeader(P, UnknownHeaders, false);
    until false;
end;

function HTTP_RESPONSE.AddCustomHeader(P: PUTF8Char; var UnknownHeaders:
  HTTP_UNKNOWN_HEADERs; ForceCustomHeader: boolean): PUTF8Char;
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
  UnknownName: PAnsiChar;
  i: integer;
begin
  if ForceCustomHeader then
    i := -1
  else
    i := IdemPCharArray(P, KNOWNHEADERS);
  // WebSockets need CONNECTION as unknown header
  if (i >= 0) and (THttpHeader(i) <> reqConnection) then
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
    while (P^ >= ' ') and (P^ <> ':') do
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

const // paranoid check of the API against our enumerations
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

finalization

{$endif USEWININET}

end.

