/// low-level access to the libcurl API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.curl;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler libcurl API
   - CURL Low-Level Constants and Types
   - CURL Functions API

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{.$define LIBCURLMULTI}
// to include the more advanced "multi session" API functions of libcurl
// see https://curl.se/libcurl/c/libcurl-multi.html interface

{.$define LIBCURLSTATIC}
// to use the static libcurl.a version of the library - mainly for Android

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.net.sock;


{ ************ CURL Low-Level Constants and Types }

{$Z4}

type
  /// low-level exception raised during libcurl library access
  ECurl = class(Exception);

  /// low-level options for libcurl library API calls
  TCurlOption = (
    coPort                 = 3,
    coTimeout              = 13,
    coInFileSize           = 14,
    coLowSpeedLimit        = 19,
    coLowSpeedTime         = 20,
    coResumeFrom           = 21,
    coCRLF                 = 27,
    coSSLVersion           = 32,
    coTimeCondition        = 33,
    coTimeValue            = 34,
    coVerbose              = 41,
    coHeader               = 42,
    coNoProgress           = 43,
    coNoBody               = 44,
    coFailOnError          = 45,
    coUpload               = 46,
    coPost                 = 47,
    coFTPListOnly          = 48,
    coFTPAppend            = 50,
    coNetRC                = 51,
    coFollowLocation       = 52,
    coTransferText         = 53,
    coPut                  = 54,
    coAutoReferer          = 58,
    coProxyPort            = 59,
    coPostFieldSize        = 60,
    coHttpProxyTunnel      = 61,
    coSSLVerifyPeer        = 64,
    coMaxRedirs            = 68,
    coFileTime             = 69,
    coMaxConnects          = 71,
    coClosePolicy          = 72,
    coFreshConnect         = 74,
    coForbidResue          = 75,
    coConnectTimeout       = 78,
    coHttpGet              = 80,
    coSSLVerifyHost        = 81,
    coHttpVersion          = 84,
    coFTPUseEPSV           = 85,
    coSSLEngineDefault     = 90,
    coDNSUseGlobalCache    = 91,
    coDNSCacheTimeout      = 92,
    coCookieSession        = 96,
    coBufferSize           = 98,
    coNoSignal             = 99,
    coProxyType            = 101,
    coUnrestrictedAuth     = 105,
    coFTPUseEPRT           = 106,
    coHttpAuth             = 107,
    coFTPCreateMissingDirs = 110,
    coProxyAuth            = 111,
    coFTPResponseTimeout   = 112,
    coIPResolve            = 113,
    coMaxFileSize          = 114,
    coFTPSSL               = 119,
    coTCPNoDelay           = 121,
    coFTPSSLAuth           = 129,
    coIgnoreContentLength  = 136,
    coFTPSkipPasvIp        = 137,
    coTimeoutMs            = 155, // since libcurl 7.16.2 - April 11 2007
    coConnectTimeoutMs     = 156,
    coFile                 = 10001,
    coURL                  = 10002,
    coProxy                = 10004,
    coUserPwd              = 10005,
    coProxyUserPwd         = 10006,
    coRange                = 10007,
    coInFile               = 10009,
    coErrorBuffer          = 10010,
    coPostFields           = 10015,
    coReferer              = 10016,
    coFTPPort              = 10017,
    coUserAgent            = 10018,
    coCookie               = 10022,
    coHttpHeader           = 10023,
    coHttpPost             = 10024,
    coSSLCert              = 10025,
    coSSLCertPasswd        = 10026,
    coQuote                = 10028,
    coWriteHeader          = 10029,
    coCookieFile           = 10031,
    coCustomRequest        = 10036,
    coStdErr               = 10037,
    coPostQuote            = 10039,
    coWriteInfo            = 10040,
    coProgressData         = 10057,
    coInterface            = 10062,
    coKRB4Level            = 10063,
    coCAInfo               = 10065,
    coTelnetOptions        = 10070,
    coRandomFile           = 10076,
    coEGDSocket            = 10077,
    coCookieJar            = 10082,
    coSSLCipherList        = 10083,
    coSSLCertType          = 10086,
    coSSLKey               = 10087,
    coSSLKeyType           = 10088,
    coSSLEngine            = 10089,
    coPreQuote             = 10093,
    coDebugData            = 10095,
    coCAPath               = 10097,
    coShare                = 10100,
    coEncoding             = 10102,
    coPrivate              = 10103,
    coHttp200Aliases       = 10104,
    coSSLCtxData           = 10109,
    coNetRCFile            = 10118,
    coSourceUserPwd        = 10123,
    coSourcePreQuote       = 10127,
    coSourcePostQuote      = 10128,
    coIOCTLData            = 10131,
    coSourceURL            = 10132,
    coSourceQuote          = 10133,
    coFTPAccount           = 10134,
    coCookieList           = 10135,
    coUnixSocketPath       = 10231,
    coWriteFunction        = 20011,
    coReadFunction         = 20012,
    coProgressFunction     = 20056,
    coHeaderFunction       = 20079,
    coDebugFunction        = 20094,
    coSSLCtxtFunction      = 20108,
    coIOCTLFunction        = 20130,
    coXferInfoFunction     = 20219,
    coInFileSizeLarge      = 30115,
    coResumeFromLarge      = 30116,
    coMaxFileSizeLarge     = 30117,
    coPostFieldSizeLarge   = 30120
  );

  /// low-level result codes for libcurl library API calls
  TCurlResult = (
    crOK,
    crUnsupportedProtocol,
    crFailedInit,
    crURLMalformat,
    crURLMalformatUser,
    crCouldNotResolveProxy,
    crCouldNotResolveHost,
    crCouldNotConnect,
    crFTPWeirdServerReply,
    crFTPAccessDenied,
    crFTPUserPasswordIncorrect,
    crFTPWeirdPassReply,
    crFTPWeirdUserReply,
    crFTPWeirdPASVReply,
    crFTPWeird227Format,
    crFTPCantGetHost,
    crFTPCantReconnect,
    crFTPCouldNotSetBINARY,
    crPartialFile,
    crFTPCouldNotRetrFile,
    crFTPWriteError,
    crFTPQuoteError,
    crHTTPReturnedError,
    crWriteError,
    crMalFormatUser,
    crFTPCouldNotStorFile,
    crReadError,
    crOutOfMemory,
    crOperationTimeouted,
    crFTPCouldNotSetASCII,
    crFTPPortFailed,
    crFTPCouldNotUseREST,
    crFTPCouldNotGetSize,
    crHTTPRangeError,
    crHTTPPostError,
    crSSLConnectError,
    crBadDownloadResume,
    crFileCouldNotReadFile,
    crLDAPCannotBind,
    crLDAPSearchFailed,
    crLibraryNotFound,
    crFunctionNotFound,
    crAbortedByCallback,
    crBadFunctionArgument,
    crBadCallingOrder,
    crInterfaceFailed,
    crBadPasswordEntered,
    crTooManyRedirects,
    crUnknownTelnetOption,
    crTelnetOptionSyntax,
    crObsolete,
    crSSLPeerCertificate,
    crGotNothing,
    crSSLEngineNotFound,
    crSSLEngineSetFailed,
    crSendError,
    crRecvError,
    crShareInUse,
    crSSLCertProblem,
    crSSLCipher,
    crSSLCACert,
    crBadContentEncoding,
    crLDAPInvalidURL,
    crFileSizeExceeded,
    crFTPSSLFailed,
    crSendFailRewind,
    crSSLEngineInitFailed,
    crLoginDenied,
    crTFTPNotFound,
    crTFTPPerm,
    crTFTPDiskFull,
    crTFTPIllegal,
    crTFTPUnknownID,
    crTFTPExists,
    crTFTPNoSuchUser);

  /// libcurl share interface result codes
  CURLSHcode = (
    CURLSHE_OK,           // all is fine
    CURLSHE_BAD_OPTION,   // 1
    CURLSHE_IN_USE,       // 2
    CURLSHE_INVALID,      // 3
    CURLSHE_NOMEM,        // 4 out of memory
    CURLSHE_NOT_BUILT_IN, // 5 feature not present in lib
    CURLSHE_LAST);        // never use

  /// libcurl share interface options
  CURLSHoption = (
    CURLSHOPT_NONE,
    CURLSHOPT_SHARE,
    CURLSHOPT_UNSHARE,
    CURLSHOPT_LOCKFUNC,
    CURLSHOPT_UNLOCKFUNC,
    CURLSHOPT_USERDATA,
    CURLSHOPT_LAST);

  /// low-level information enumeration for libcurl library API calls
  TCurlInfo = (
    ciNone,
    ciLastOne               = 28,
    ciEffectiveURL          = 1048577,
    ciContentType           = 1048594,
    ciPrivate               = 1048597,
    ciRedirectURL           = 1048607,
    ciPrimaryIP             = 1048608,
    ciLocalIP               = 1048617,
    ciResponseCode          = 2097154,
    ciHeaderSize            = 2097163,
    ciRequestSize           = 2097164,
    ciSSLVerifyResult       = 2097165,
    ciFileTime              = 2097166,
    ciRedirectCount         = 2097172,
    ciHTTPConnectCode       = 2097174,
    ciHTTPAuthAvail         = 2097175,
    ciProxyAuthAvail        = 2097176,
    ciOS_Errno              = 2097177,
    ciNumConnects           = 2097178,
    ciPrimaryPort           = 2097192,
    ciLocalPort             = 2097194,
    ciTotalTime             = 3145731,
    ciNameLookupTime        = 3145732,
    ciConnectTime           = 3145733,
    ciPreTRansferTime       = 3145734,
    ciSizeUpload            = 3145735,
    ciSizeDownload          = 3145736,
    ciSpeedDownload         = 3145737,
    ciSpeedUpload           = 3145738,
    ciContentLengthDownload = 3145743,
    ciContentLengthUpload   = 3145744,
    ciStartTransferTime     = 3145745,
    ciRedirectTime          = 3145747,
    ciAppConnectTime        = 3145761,
    ciSSLEngines            = 4194331,
    ciCookieList            = 4194332,
    ciCertInfo              = 4194338,
    ciSizeDownloadT         = 6291464,
    ciTotalTimeT            = 6291506, // (6) can be used for calculation "Content download time"
    ciNameLookupTimeT       = 6291507, // (1) DNS lookup
    ciConnectTimeT          = 6291508, // (2) connect time
    ciPreTransferTimeT      = 6291509, // (4)
    ciStartTransferTimeT    = 6291510, // (5) Time to first byte
    ciAppConnectTimeT       = 6291512  // (3) SSL handshake
  );

  {$ifdef LIBCURLMULTI}

  /// low-level result codes for libcurl library API calls in "multi" mode
  TCurlMultiCode = (
    cmcCallMultiPerform = -1,
    cmcOK = 0,
    cmcBadHandle,
    cmcBadEasyHandle,
    cmcOutOfMemory,
    cmcInternalError,
    cmcBadSocket,
    cmcUnknownOption,
    cmcAddedAlready,
    cmcRecursiveApiCall
  );

  /// low-level options for libcurl library API calls in "multi" mode
  TCurlMultiOption = (
    cmoPipeLining               = 3,
    cmoMaxConnects              = 6,
    cmoMaxHostConnections       = 7,
    cmoMaxPipelineLength        = 8,
    cmoMaxTotalConnections      = 13,
    cmoSocketData               = 10002,
    cmoTimerData                = 10005,
    cmoPipeliningSiteBL         = 10011,
    cmoPipeliningServerBL       = 10012,
    cmoPushData                 = 10015,
    cmoSocketFunction           = 20001,
    cmoTimerFunction            = 20004,
    cmoPushFunction             = 20014,
    cmoContentLengthPenaltySize = 30009,
    cmoChunkLengthPenaltySize   = 30010
  );

  {$endif LIBCURLMULTI}

  /// low-level version identifier of the libcurl library
  TCurlVersion = (
    cvFirst,
    cvSecond,
    cvThird,
    cvFour,
    cvLast);

  /// low-level initialization option for libcurl library API
  // - currently, only giSSL is set, since giWin32 is redundant with WinHttp
  TCurlGlobalInit = set of (
    giNone,
    giSSL,
    giWin32,
    giAll);

  /// low-level message state for libcurl library API
  TCurlMsg = (
    cmNone,
    cmDone);

  /// libcurl multipart/formdata HTTP POST result codes
  TCurlFormCode = (
    CURL_FORMADD_OK,
    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED,
    CURL_FORMADD_LAST);

  /// libcurl multipart/formdata HTTP POST options
  TCurlFormOption = (
    CURLFORM_NOTHING,
    CURLFORM_COPYNAME, // followed by a string which will be copied by libcurl
    CURLFORM_PTRNAME,  // followed by a string which pointer will stay available
    CURLFORM_NAMELENGTH, // if CURLFORM_COPYNAME/PTRNAME is not #0 terminated
    CURLFORM_COPYCONTENTS, // followed by a pointer to the contents to be copied
    CURLFORM_PTRCONTENTS,  // followed by a persistent pointer to the contents
    CURLFORM_CONTENTSLENGTH, // deprecated - use CURLFORM_CONTENTLEN
    CURLFORM_FILECONTENT, // followed by a filename - may not be a file upload
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE, // followed by a filename - will be a file upload part
    CURLFORM_BUFFER, // followed by a filename - file upload without CURLFORM_FILE
    CURLFORM_BUFFERPTR,     // CURLFORM_BUFFER persistent pointer
    CURLFORM_BUFFERLENGTH,  // CURLFORM_BUFFER persistent size
    CURLFORM_CONTENTTYPE, // followed by a pointer to a string, for CURLFORM_FILE
    CURLFORM_CONTENTHEADER, // followed by a curl_slist_append() list of headers
    CURLFORM_FILENAME, // followed by a pointer to a string, for CURLFORM_FILE
    CURLFORM_END,
    CURLFORM_OBSOLETE2,
    CURLFORM_STREAM,
    CURLFORM_CONTENTLEN, // added in 7.46.0, provide a curl_off_t 64-bit size
    CURLFORM_LASTENTRY
    );

  /// low-level version information for libcurl library
  TCurlVersionInfo = record
    age: TCurlVersion;
    version: PAnsiChar;
    version_num: cardinal;
    host: PAnsiChar;
    features: integer;
    ssl_version: PAnsiChar;
    ssl_version_num: PAnsiChar;
    libz_version: PAnsiChar;
    protocols: ^TPAnsiCharArray;
    ares: PAnsiChar;
    ares_num: integer;
    libidn: PAnsiChar;
  end;
  PCurlVersionInfo = ^TCurlVersionInfo;

  /// low-level access to the libcurl library instance
  TCurl = type pointer;

  /// low-level string list type for libcurl library API
  TCurlSList = type pointer;
  PCurlSList = ^TCurlSList;
  PPCurlSListArray = ^PCurlSListArray;
  PCurlSListArray = array[0 .. (MaxInt div SizeOf(PCurlSList)) - 1] of PCurlSList;

  /// low-level access to the libcurl share interface
  TCurlShare = type pointer;

  /// low-level access to the libcurl library instance in "multi" mode
  TCurlMulti = type pointer;

  /// low-level access to one libcurl library socket instance
  TCurlSocket = type TNetSocket;

  /// low-level certificate information for libcurl library API
  TCurlCertInfo = packed record
    num_of_certs: integer;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
    certinfo: PPCurlSListArray;
  end;
  PCurlCertInfo = ^TCurlCertInfo;

  PCurlHttpPost = ^TCurlHttpPost;

  /// defines a multipart/formdata HTTP POST section
  TCurlHttpPost = record
    /// next entry in the list
    next: PCurlHttpPost;
    /// pointer to allocated name
    name: PAnsiChar;
    /// length of name length
    namelength: integer;
    /// pointer to allocated data contents
    contents: PAnsiChar;
    /// length of contents field
    // - see also CURL_HTTPPOST_LARGE / 64-bit contentlen
    contentslength: integer;
    /// pointer to allocated buffer contents
    buffer: PAnsiChar;
    /// length of buffer field
    bufferlength: integer;
    /// Content-Type text
    contenttype: PAnsiChar;
    /// list of extra headers for this form
    contentheader: PPCurlSListArray;
    /// if one field name has more than one file, this link should link to following files
    more: PCurlHttpPost;
    /// as defined below
    flags: integer;
    /// file name to show. If not set, the actual file name will be used (if this is a file part)
    showfilename: PAnsiChar;
    /// custom pointer used for HTTPPOST_CALLBACK posts
    userp: pointer;
    /// alternative length of contents field - used if CURL_HTTPPOST_LARGE is set
    // - added in 7.46.0 - is defined as POSIX off_t
    contentlen: Int64;
  end;


  /// low-level message information for libcurl library API
  TCurlMsgRec = packed record
    msg: TCurlMsg;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
    easy_handle: TCurl;
    data: packed record case byte of
      0: (whatever: Pointer);
      1: (result: TCurlResult);
    end;
  end;
  PCurlMsgRec = ^TCurlMsgRec;

  /// low-level file description event handler for libcurl library API
  TCurlWaitFD = packed record
    fd: TCurlSocket;
    events: SmallInt;
    revents: SmallInt;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
  end;
  PCurlWaitFD = ^TCurlWaitFD;

  /// low-level write callback function signature for libcurl library API
  curl_write_callback = function (buffer: PAnsiChar; size,nitems: integer;
    outstream: pointer): integer; cdecl;

  /// low-level read callback function signature for libcurl library API
  curl_read_callback = function (buffer: PAnsiChar; size,nitems: integer;
    instream: pointer): integer; cdecl;

  curl_lock_data = (
    CURL_LOCK_DATA_NONE = 0,
    CURL_LOCK_DATA_SHARE,
    CURL_LOCK_DATA_COOKIE,
    CURL_LOCK_DATA_DNS,
    CURL_LOCK_DATA_SSL_SESSION,
    CURL_LOCK_DATA_CONNECT,
    CURL_LOCK_DATA_PSL,
    CURL_LOCK_DATA_LAST);

  curl_lock_access = (
    CURL_LOCK_ACCESS_NONE = 0,
    CURL_LOCK_ACCESS_SHARED = 1,
    CURL_LOCK_ACCESS_SINGLE = 2,
    CURL_LOCK_ACCESS_LAST);

  /// lock function signature for CURLSHOPT_LOCKFUNC
  curl_lock_function = procedure (handle: TCurl; data: curl_lock_data;
    locktype: curl_lock_access; userptr: pointer); cdecl;
  /// unlock function signature for CURLSHOPT_UNLOCKFUNC
  curl_unlock_function = procedure (handle: TCurl; data: curl_lock_data;
    userptr: pointer); cdecl;

{$Z1}

const
  // some aliases
  coWriteData = coFile;
  coXferInfoData = coProgressData;
  coAcceptEncoding = coEncoding;


{ ************ CURL Functions API }

const
  /// low-level libcurl library file name, depending on the running OS
  LIBCURL_DLL =
    {$ifdef OSDARWIN}
      'libcurl.dylib';
    {$else} {$ifdef OSWINDOWS}
      {$ifdef CPU64}
        'libcurl-x64.dll';
      {$else}
        'libcurl.dll';
      {$endif CPU64}
    {$else}
      'libcurl.so';
    {$endif OSWINDOWS}
    {$endif OSDARWIN}

type
  /// low-level late binding functions access to the libcurl library API
  // - ensure you called LibCurlInitialize or CurlIsAvailable functions to
  // setup this global instance before using any of its internal functions
  // - see also https://curl.se/libcurl/c/libcurl-multi.html interface
  {$ifdef LIBCURLSTATIC}
  TLibCurl = record
  {$else}
  TLibCurl = class(TSynLibrary)
  {$endif LIBCURLSTATIC}
  public
    /// global TCurlShare object, created by CurlEnableGlobalShare
    globalShare: TCurlShare;
    /// hold CurlEnableGlobalShare mutexes
    share_cs: array[curl_lock_data] of TRTLCriticalSection;
    /// initialize the library
    global_init: function(flags: TCurlGlobalInit): TCurlResult; cdecl;
    /// initialize the library and specify a memory manager
    // - set malloc, free, realloc, strdup and calloc functions
    global_init_mem: function(flags: TCurlGlobalInit;
      m, f, r, s, c: pointer): TCurlResult; cdecl;
    /// finalize the library
    global_cleanup: procedure; cdecl;
    /// returns run-time libcurl version info
    version_info: function(age: TCurlVersion): PCurlVersionInfo; cdecl;
    // start a libcurl easy session
    easy_init: function: pointer; cdecl;
    /// set options for a curl easy handle
    easy_setopt: function(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs;
    /// perform a blocking file transfer
    easy_perform: function(curl: TCurl): TCurlResult; cdecl;
    /// end a libcurl easy handle
    easy_cleanup: procedure(curl: TCurl); cdecl;
    /// extract information from a curl handle
    easy_getinfo: function(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl;
    /// clone a libcurl session handle
    easy_duphandle: function(curl: TCurl): pointer; cdecl;
    /// reset all options of a libcurl session handle
    easy_reset: procedure(curl: TCurl); cdecl;
    /// return string describing error code
    easy_strerror: function(code: TCurlResult): PAnsiChar; cdecl;
    /// add a string to an slist
    slist_append: function(list: TCurlSList; s: PAnsiChar): TCurlSList; cdecl;
    /// free an entire slist
    slist_free_all: procedure(list: TCurlSList); cdecl;
    /// add a section to a multipart/formdata HTTP POST request
    formadd: function(var first, last: PCurlHttpPost): TCurlFormCode; cdecl varargs;
    /// finalize the sections of a multipart/formdata HTTP POST request
    formfree: procedure(first: PCurlHttpPost); cdecl;

    /// create a shared object
    share_init: function: pointer; cdecl;
    /// clean up a shared object
    share_cleanup: function(share_handle: TCurlShare): CURLSHcode; cdecl;
    /// set options for a shared object
    share_setopt: function(share: TCurlShare; option: CURLSHoption): CURLSHcode; cdecl varargs;
    /// return the text description of an error code
    share_strerror: function(code: CURLSHcode): PAnsiChar; cdecl;

    {$ifdef LIBCURLMULTI}
    /// add an easy handle to a multi session
    multi_add_handle: function(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl;
    /// set data to associate with an internal socket
    multi_assign: function(mcurl: TCurlMulti; socket: TCurlSocket; data: pointer): TCurlMultiCode; cdecl;
    /// close down a multi session
    multi_cleanup: function(mcurl: TCurlMulti): TCurlMultiCode; cdecl;
    /// extracts file descriptor information from a multi handle
    multi_fdset: function(mcurl: TCurlMulti; read, write, exec: pointer; out max: integer): TCurlMultiCode; cdecl;
    /// read multi stack informationals
    multi_info_read: function(mcurl: TCurlMulti; out msgsqueue: integer): PCurlMsgRec; cdecl;
    /// create a multi handle
    multi_init: function: TCurlMulti; cdecl;
    /// reads/writes available data from each easy handle
    multi_perform: function(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl;
    /// remove an easy handle from a multi session
    multi_remove_handle: function(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl;
    /// set options for a curl multi handle
    multi_setopt: function(mcurl: TCurlMulti; option: TCurlMultiOption): TCurlMultiCode; cdecl varargs;
    /// reads/writes available data given an action
    multi_socket_action: function(mcurl: TCurlMulti; socket: TCurlSocket; mask: integer; out runningh: integer): TCurlMultiCode; cdecl;
    /// reads/writes available data - deprecated call
    multi_socket_all: function(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl;
    /// return string describing error code
    multi_strerror: function(code: TCurlMultiCode): PAnsiChar; cdecl;
    /// retrieve how long to wait for action before proceeding
    multi_timeout: function(mcurl: TCurlMulti; out ms: integer): TCurlMultiCode; cdecl;
    /// polls on all easy handles in a multi handle
    multi_wait: function(mcurl: TCurlMulti; fds: PCurlWaitFD; fdscount: cardinal; ms: integer; out ret: integer): TCurlMultiCode; cdecl;
    {$endif LIBCURLMULTI}

    /// contains numerical information about the initialized libcurl instance
    info: TCurlVersionInfo;
    /// contains textual information about the initialized libcurl instance
    infoText: string;

    {$ifndef LIBCURLSTATIC}
    /// finalize the library
    destructor Destroy; override;
    {$endif LIBCURLSTATIC}
  end;

var
  /// main access to the libcurl library API
  // - ensure you called LibCurlInitialize or CurlIsAvailable functions to
  // setup this global instance before using any of its internal functions
  curl: TLibCurl;

/// initialize the libcurl API, accessible via the curl global variable
// - do nothing if the library has already been loaded
// - will raise ECurl exception on any loading issue
// - you can specify the libcurl library name to load
procedure LibCurlInitialize(engines: TCurlGlobalInit = [giAll];
  const dllname: TFileName = LIBCURL_DLL);

/// return TRUE if a curl library is available
// - will load and initialize it, calling LibCurlInitialize if necessary,
// catching any exception during the process
function CurlIsAvailable: boolean;

/// Callback used by libcurl to write data; Usage:
// curl.easy_setopt(fHandle,coWriteFunction,@CurlWriteRawByteString);
// curl.easy_setopt(curlHandle,coFile,@curlRespBody);
// where curlRespBody should be a generic AnsiString/RawByteString, i.e.
// in practice a RawUtf8 or a RawByteString
function CurlWriteRawByteString(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;

/// enable libcurl multiple easy handles to share data
// - is called automatically during libcurl initialization
// - shared objects are: DNS cache, TLS session cache and connection cache
// - this way, each single transfer can take advantage of the context of the
// other transfer(s)
// - do nothing if the global share has already been enabled
// - see https://curl.se/libcurl/c/libcurl-share.html for details
function CurlEnableGlobalShare: boolean;

/// disable a global share for libcurl
// - is called automatically in finalization section
// - can be called on purpose, to ensure there is no active HTTP requests
// and prevent CURLSHE_IN_USE error
// - you can re-enable the libcurl global share by CurlEnableGlobalShare
function CurlDisableGlobalShare: CURLSHcode;


implementation 

{ ************ CURL Functions API }

{$ifdef LIBCURLSTATIC}

{$ifdef FPC}

  // compiled static library from https://github.com/gcesarmza/curl-android-ios
  {$ifdef OSANDROID}
    {$ifdef CPUAARCH64}
      {$linklib ..\..\static\aarch64-android\libcurl.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$linklib ..\..\static\arm-android\libcurl.a}
    {$endif CPUARM}
    {$linklib libz.so}
  {$endif OSANDROID}

  /// initialize the library
  function curl_global_init(flags: TCurlGlobalInit): TCurlResult; cdecl; external;
  /// initialize the library with a custom memory manager
  function curl_global_init_mem(flags: TCurlGlobalInit;
    m, f, r, s, c: pointer): TCurlResult; cdecl; external;
  /// finalize the library
  procedure curl_global_cleanup cdecl; external;
  /// returns run-time libcurl version info
  function curl_version_info(age: TCurlVersion): PCurlVersionInfo; cdecl; external;
  // start a libcurl easy session
  function curl_easy_init: pointer; cdecl; external;
  /// set options for a curl easy handle
  function curl_easy_setopt(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs; external;
  /// perform a blocking file transfer
  function curl_easy_perform(curl: TCurl): TCurlResult; cdecl; external;
  /// end a libcurl easy handle
  procedure curl_easy_cleanup(curl: TCurl); cdecl; external;
  /// extract information from a curl handle
  function curl_easy_getinfo(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl; external;
  /// clone a libcurl session handle
  function curl_easy_duphandle(curl: TCurl): pointer; cdecl; external;
  /// reset all options of a libcurl session handle
  procedure curl_easy_reset(curl: TCurl); cdecl; external;
  /// return string describing error code
  function curl_easy_strerror(code: TCurlResult): PAnsiChar; cdecl; external;
  /// add a string to an slist
  function curl_slist_append(list: TCurlSList; s: PAnsiChar): TCurlSList; cdecl; external;
  /// free an entire slist
  procedure curl_slist_free_all(list: TCurlSList); cdecl; external;
  /// add a section to a multipart/formdata HTTP POST
  function curl_formadd(var first, last: PCurlHttpPost): TCurlFormCode; cdecl varargs; external;
  /// finalize all sections of a multipart/formdata HTTP POST
  procedure curl_formfree(first: PCurlHttpPost); cdecl; external;

  /// create a shared object
  function curl_share_init: pointer; cdecl; external;
  /// clean up a shared object
  function curl_share_cleanup(share_handle: TCurlShare): CURLSHcode; cdecl; external;
  /// set options for a shared object
  function curl_share_setopt(share: TCurlShare; option: CURLSHoption): CURLSHcode; cdecl varargs; external;
  /// return string describing error code
  function curl_share_strerror(code: CURLSHcode): PAnsiChar;  cdecl; external;

  {$ifdef LIBCURLMULTI}
  /// add an easy handle to a multi session
  function curl_multi_add_handle(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl; external;
  /// set data to associate with an internal socket
  function curl_multi_assign(mcurl: TCurlMulti; socket: TCurlSocket; data: pointer): TCurlMultiCode; cdecl; external;
  /// close down a multi session
  function curl_multi_cleanup(mcurl: TCurlMulti): TCurlMultiCode; cdecl; external;
  /// extracts file descriptor information from a multi handle
  function curl_multi_fdset(mcurl: TCurlMulti; read, write, exec: pointer; out max: integer): TCurlMultiCode; cdecl; external;
  /// read multi stack informationals
  function curl_multi_info_read(mcurl: TCurlMulti; out msgsqueue: integer): PCurlMsgRec; cdecl; external;
  /// create a multi handle
  function curl_multi_init: TCurlMulti; cdecl; external;
  /// reads/writes available data from each easy handle
  function curl_multi_perform(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// remove an easy handle from a multi session
  function curl_multi_remove_handle(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl; external;
  /// set options for a curl multi handle
  function curl_multi_setopt(mcurl: TCurlMulti; option: TCurlMultiOption): TCurlMultiCode; cdecl varargs; external;
  /// reads/writes available data given an action
  function curl_multi_socket_action(mcurl: TCurlMulti; socket: TCurlSocket; mask: integer; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// reads/writes available data - deprecated call
  function curl_multi_socket_all(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// return string describing error code
  function curl_multi_strerror(code: TCurlMultiCode): PAnsiChar; cdecl; external;
  /// retrieve how long to wait for action before proceeding
  function curl_multi_timeout(mcurl: TCurlMulti; out ms: integer): TCurlMultiCode; cdecl; external;
  /// polls on all easy handles in a multi handle
  function curl_multi_wait(mcurl: TCurlMulti; fds: PCurlWaitFD; fdscount: cardinal; ms: integer; out ret: integer): TCurlMultiCode; cdecl; external;
  {$endif LIBCURLMULTI}

{$endif FPC}

{$endif LIBCURLSTATIC}

function CurlWriteRawByteString(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;
var
  storage: PRawByteString absolute opaque;
  n: integer;
begin
  if storage = nil then
    result := 0
  else
  begin
    n := length(storage^);
    result := size * nitems;
    SetLength(storage^, n + result);
    MoveFast(buffer^, PPAnsiChar(opaque)^[n], result);
  end;
end;

var
  curl_initialized: boolean;

function CurlIsAvailable: boolean;
begin
  if not curl_initialized then
    // try to initialize with the default library name
    LibCurlInitialize;
  result := {$ifdef LIBCURLSTATIC} true {$else} curl <> nil {$endif};
end;

// ensure libcurl will call our RTL MM, not the libc heap

function curl_malloc_callback(size: PtrInt) : pointer; cdecl;
begin
  GetMem(result, size);
end;

procedure curl_free_callback(ptr: pointer); cdecl;
begin
  FreeMem(ptr);
end;

function curl_realloc_callback(ptr: pointer; size: PtrInt) : pointer; cdecl;
begin
  ReallocMem(ptr, size);
  result := ptr;
end;

function curl_strdup_callback(str: PAnsiChar): PAnsiChar; cdecl;
var
  len: PtrInt;
begin
  len := StrLen(str);
  GetMem(result, len + 1);
  result[len] := #0;
  MoveFast(str^, result^, len);
end;

function curl_calloc_callback(nmemb, size: PtrInt): pointer; cdecl;
begin
  result := AllocMem(size * nmemb);
end;


procedure LibCurlInitialize(engines: TCurlGlobalInit; const dllname: TFileName);

var
  res: TCurlResult;
{$ifndef LIBCURLSTATIC}
  P: PPointerArray;
  api: PtrInt;

const
  NAMES: array[0 .. {$ifdef LIBCURLMULTI} 33 {$else} 19 {$endif}] of PAnsiChar = (
    'curl_global_init', 'curl_global_init_mem', 'curl_global_cleanup', 'curl_version_info',
    'curl_easy_init', 'curl_easy_setopt', 'curl_easy_perform', 'curl_easy_cleanup',
    'curl_easy_getinfo', 'curl_easy_duphandle', 'curl_easy_reset',
    'curl_easy_strerror', 'curl_slist_append', 'curl_slist_free_all',
    'curl_formadd', 'curl_formfree', 'curl_share_init',
    'curl_share_cleanup','curl_share_setopt', 'curl_share_strerror'
    {$ifdef LIBCURLMULTI},
    'curl_multi_add_handle', 'curl_multi_assign', 'curl_multi_cleanup',
    'curl_multi_fdset', 'curl_multi_info_read', 'curl_multi_init',
    'curl_multi_perform', 'curl_multi_remove_handle', 'curl_multi_setopt',
    'curl_multi_socket_action', 'curl_multi_socket_all', 'curl_multi_strerror',
    'curl_multi_timeout', 'curl_multi_wait'
    {$endif LIBCURLMULTI} );

{$endif LIBCURLSTATIC}

begin
  if curl_initialized
     {$ifndef LIBCURLSTATIC} and
     (curl <> nil)
     {$endif LIBCURLSTATIC} then
    exit; // set it once, but allow to retry a given dllname

  GlobalLock;
  try
    if curl_initialized then
      exit;

    {$ifdef LIBCURLSTATIC}

    curl.global_init := @curl_global_init;
    curl.global_init_mem := @curl_global_init_mem;
    curl.global_cleanup := @curl_global_cleanup;
    curl.version_info := @curl_version_info;
    curl.easy_init := @curl_easy_init;
    curl.easy_setopt := @curl_easy_setopt;
    curl.easy_perform := @curl_easy_perform;
    curl.easy_cleanup := @curl_easy_cleanup;
    curl.easy_getinfo := @curl_easy_getinfo;
    curl.easy_duphandle := @curl_easy_duphandle;
    curl.easy_reset := @curl_easy_reset;
    curl.easy_strerror := @curl_easy_strerror;
    curl.slist_append := @curl_slist_append;
    curl.slist_free_all := @curl_slist_free_all;
    curl.formadd := @curl_formadd;
    curl.formfree := @curl_formfree;
    curl.share_init := @curl_share_init;
    curl.share_cleanup := @curl_share_cleanup;
    curl.share_setopt := @curl_share_setopt;
    curl.share_strerror := @curl_share_strerror;
    {$ifdef LIBCURLMULTI}
    curl.multi_add_handle := @curl_multi_add_handle;
    curl.multi_assign := @curl_multi_assign;
    curl.multi_cleanup := @curl_multi_cleanup;
    curl.multi_fdset := @curl_multi_fdset;
    curl.multi_info_read := @curl_multi_info_read;
    curl.multi_init := @curl_multi_init;
    curl.multi_perform := @curl_multi_perform;
    curl.multi_remove_handle := @curl_multi_remove_handle;
    curl.multi_setopt := @curl_multi_setopt;
    curl.multi_socket_action := @curl_multi_socket_action;
    curl.multi_socket_all := @curl_multi_socket_all;
    curl.multi_strerror := @curl_multi_strerror;
    curl.multi_timeout := @curl_multi_timeout;
    curl.multi_wait := @curl_multi_wait;
    {$endif LIBCURLMULTI}

    {$else}

    curl := TLibCurl.Create;
    try
      curl.TryLoadLibrary([
      {$ifdef OSWINDOWS}
        // first try the libcurl.dll in the local executable folder
        Executable.ProgramFilePath + dllname,
      {$endif OSWINDOWS}
        // search standard library in path
        dllname
      {$ifdef OSDARWIN}
        // another common names on MacOS
        , 'libcurl.4.dylib', 'libcurl.3.dylib'
      {$else}
        {$ifdef OSPOSIX}
        // another common names on POSIX
        , 'libcurl.so.4', 'libcurl.so.3'
        // for latest Linux Mint and other similar distros using gnutls
        , 'libcurl-gnutls.so.4', 'libcurl-gnutls.so.3'
        {$endif OSPOSIX}
      {$endif OSDARWIN}
        ], ECurl);
      P := @@curl.global_init;
      for api := low(NAMES) to high(NAMES) do
        curl.Resolve(NAMES[api], @P[api], {raiseonfailure=}ECurl);
    except
      FreeAndNil(curl); // ECurl raised during initialization above
      exit;
    end;

    {$endif LIBCURLSTATIC}

    // if we reached here, the library has been successfully loaded
    res := curl.global_init_mem(engines, @curl_malloc_callback, @curl_free_callback,
      @curl_realloc_callback, @curl_strdup_callback, @curl_calloc_callback);
    if res <> crOK then
        raise ECurl.CreateFmt('curl_global_init_mem() failed as %d', [ord(res)]);
    curl.info := curl.version_info(cvFour)^;
    curl.infoText := format('%s version %s', [LIBCURL_DLL, curl.info.version]);
    if curl.info.ssl_version <> nil then
      curl.infoText := format('%s using %s', [curl.infoText, curl.info.ssl_version]);
    curl_initialized := true; // should be set last but before CurlEnableGlobalShare

    curl.globalShare := nil;
    CurlEnableGlobalShare; // won't hurt, and may benefit even for the OS
    // api := 0; with curl.info do while protocols[api]<>nil do
    // begin write(protocols[api], ' '); inc(api); end; writeln(#13#10,curl.infoText);
  finally
    GlobalUnLock;
  end;
end;

procedure curlShareLock(handle: TCurl; data: curl_lock_data;
  locktype: curl_lock_access; userptr: pointer); cdecl;
begin
  EnterCriticalSection(curl.share_cs[data]);
end;

procedure curlShareUnLock(handle: TCurl; data: curl_lock_data;
  userptr: pointer); cdecl;
begin
  LeaveCriticalSection(curl.share_cs[data]);
end;

function CurlEnableGlobalShare: boolean;
var
  d: curl_lock_data;
begin
  result := false;
  if not CurlIsAvailable or 
     (curl.globalShare <> nil) then
    exit; // not available, or already shared
  curl.globalShare := curl.share_init;
  if curl.globalShare = nil then
    // something went wrong (out of memory, etc.) and therefore
    // the share object was not created
    exit;
  for d := low(d) to high(d) do
    InitializeCriticalSection(curl.share_cs[d]);
  curl.share_setopt(curl.globalShare, CURLSHOPT_LOCKFUNC, @curlShareLock);
  curl.share_setopt(curl.globalShare, CURLSHOPT_UNLOCKFUNC, @curlShareUnLock);
  // share and cache DNS + TLS sessions (but not Connections)
  curl.share_setopt(curl.globalShare, CURLSHOPT_SHARE, CURL_LOCK_DATA_DNS);
  curl.share_setopt(curl.globalShare, CURLSHOPT_SHARE, CURL_LOCK_DATA_SSL_SESSION);
  // CURL_LOCK_DATA_CONNECT triggers GPF e.g. on Debian Burster 10
  if curl.info.version_num >= $00074400 then // seems to be fixed in 7.68
    // see https://github.com/curl/curl/issues/4544
    curl.share_setopt(curl.globalShare, CURLSHOPT_SHARE, CURL_LOCK_DATA_CONNECT);
  result := true;
end;

function CurlDisableGlobalShare: CURLSHcode;
var
  d: curl_lock_data;
begin
  result := CURLSHE_OK;
  if curl.globalShare = nil then
    exit; // already disabled
  result := curl.share_cleanup(curl.globalShare);
  if result = CURLSHE_OK then
    curl.globalShare := nil;
  for d := low(d) to high(d) do
    DeleteCriticalSection(curl.share_cs[d]);
end;

{$ifndef LIBCURLSTATIC}
destructor TLibCurl.Destroy;
begin
  CurlDisableGlobalShare;
  curl.global_cleanup;
end;
{$endif LIBCURLSTATIC}


initialization

finalization
  {$ifdef LIBCURLSTATIC}
  if curl_initialized then
  begin
    CurlDisableGlobalShare;
    curl.global_cleanup;
  end;
  {$else}
  curl.Free;
  {$endif LIBCURLSTATIC}

end.

