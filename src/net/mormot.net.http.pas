/// HTTP/HTTPS Abstract Process Classes and Definitions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.http;

{
  *****************************************************************************

   HTTP/HTTPS Abstract Process Classes and Definitions
   - Shared HTTP Constants and Functions
   - Reusable HTTP State Machine
   - THttpSocket Implementing HTTP over plain sockets
   - Abstract Server-Side Types used e.g. for Client-Server Protocol

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
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.zip,
  mormot.net.sock;


{ ******************** Shared HTTP Constants and Functions }

type
  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlz')
  // - the data is compressed (if Compress=TRUE) or uncompressed (if
  // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
  // - to be used with THttpSocket.RegisterCompress method
  THttpSocketCompress = function(var Data: RawByteString; Compress: boolean): RawUtf8;

  /// used to maintain a list of known compression algorithms
  THttpSocketCompressRec = record
    /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
    Name: RawUtf8;
    /// the function handling compression and decompression
    Func: THttpSocketCompress;
    /// the size in bytes after which compress will take place
    // - will be 1024 e.g. for 'zip' or 'deflate'
    // - could be 0 e.g. when encrypting the content, meaning "always compress"
    CompressMinSize: integer;
    /// a priority on which the compression is applied - highest is favored
    Priority: integer;
  end;

  /// list of known compression algorithms
  THttpSocketCompressRecDynArray = array of THttpSocketCompressRec;

  /// identify some items in a list of known compression algorithms
  // - filled from ACCEPT-ENCODING: header value
  THttpSocketCompressSet = set of 0..31;

  /// tune the 'synopsebin' protocol
  // - pboCompress will compress all frames payload using SynLZ
  // - pboNoLocalHostCompress won't compress frames on the loopback (127.0.0.1)
  // - pboNoLocalHostEncrypt won't encrypt frames on the loopback (127.0.0.1)
  TWebSocketProtocolBinaryOption = (
    pboSynLzCompress,
    pboNoLocalHostCompress,
    pboNoLocalHostEncrypt);

  /// how TWebSocketProtocolBinary implements the 'synopsebin' protocol
  // - should match on both client and server ends
  TWebSocketProtocolBinaryOptions = set of TWebSocketProtocolBinaryOption;

/// adjust HTTP body compression according to the supplied 'CONTENT-TYPE'
// - will detect most used compressible content (like 'text/*' or
// 'application/json') from OutContentType
procedure CompressContent(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUtf8;
  var OutContent: RawByteString; var OutContentEncoding: RawUtf8);

/// enable a give compression function for a HTTP link
function RegisterCompressFunc(var Comp: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress; var AcceptEncoding: RawUtf8;
  CompMinSize, CompPriority: integer): RawUtf8;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PUtf8Char): THttpSocketCompressSet;

/// search for a given compression function
function CompressIndex(const Compress: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress): PtrInt;


/// compute the 'Authorization: Bearer ####' HTTP header of a given token value
function AuthorizationBearer(const AuthToken: RawUtf8): RawUtf8;

/// will remove most usual HTTP headers which are to be recomputed on sending
function PurgeHeaders(const headers: RawUtf8): RawUtf8;

/// search, copy and remove a given HTTP header
procedure ExtractHeader(var headers: RawUtf8; const upname: RawUtf8;
  out res: RawUtf8);

/// 'HEAD' and 'OPTIONS' methods would be detected and return true
// - will check only the first four chars for efficiency
function HttpMethodWithNoBody(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// encode some text into a mime header compatible value
// - see https://tools.ietf.org/html/rfc2047
function MimeHeaderEncode(const header: RawUtf8): RawUtf8;

/// quick check for case-sensitive 'GET' HTTP method name
// - see also HttpMethodWithNoBody()
function IsGet(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// quick check for case-sensitive 'HEAD' HTTP method name
// - see also HttpMethodWithNoBody()
function IsHead(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// quick check for case-sensitive 'POST' HTTP method name
function IsPost(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// quick check for case-sensitive 'PUT' HTTP method name
function IsPut(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// quick check for case-sensitive 'DELETE' HTTP method name
function IsDelete(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// quick check for case-sensitive 'OPTIONS' HTTP method name
function IsOptions(const method: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// could be used e.g. in OnBeforeBody() callback to allow a GET /favicon.ico
function IsUrlFavIcon(P: PUtf8Char): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// decode a given parameter from an Url, in any position, into UTF-8 text
// - P^ should be either nil or point to P^ = '?'
// - UpperName should follow the UrlDecodeValue() format, e.g. 'NAME='
function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: RawUtf8): boolean; overload;

/// decode a given parameter from an Url, in any position, into a 32-bit cardinal
// - UpperName should follow the UrlDecodeCardinal() format, e.g. 'COUNT='
function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: cardinal): boolean; overload;

/// decode a given parameter from an Url, in any position, into a 64-bit Int64
// - UpperName should follow the UrlDecodeInt64() format, e.g. 'ID='
function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: Int64): boolean; overload;

/// extract a 64-bit value from a 'Range: xxx-xxx ' input
// - returned P^ points to the first non digit char - not as GetNextItemQWord()
function GetNextRange(var P: PUtf8Char): Qword;

const
  /// pseudo-header containing the current Synopse mORMot framework version
  XPOWEREDNAME = 'X-Powered-By';

  /// the full text of the current Synopse mORMot framework version
  // - we don't supply full version number with build revision
  // (as SYNOPSE_FRAMEWORK_VERSION), to reduce potential attacker knowledge
  XPOWEREDVALUE = SYNOPSE_FRAMEWORK_NAME + ' 2';


{ ******************** Reusable HTTP State Machine }

type
  /// the states of THttpRequestContext processing
  THttpRequestState = (
    hrsNoStateMachine,
    hrsGetCommand,
    hrsGetHeaders,
    hrsGetBodyChunkedHexFirst,
    hrsGetBodyChunkedHexNext,
    hrsGetBodyChunkedData,
    hrsGetBodyChunkedDataVoidLine,
    hrsGetBodyChunkedDataLastLine,
    hrsGetBodyContentLength,
    hrsWaitProcessing,
    hrsWaitAsyncProcessing,
    hrsSendBody,
    hrsResponseDone,
    hrsUpgraded,
    hrsErrorPayloadTooLarge,
    hrsErrorRejected,
    hrsErrorMisuse,
    hrsErrorUnsupportedFormat,
    hrsErrorUnsupportedRange,
    hrsErrorAborted,
    hrsErrorShutdownInProgress);

  /// set of states for THttpRequestContext processing
  THttpRequestStates = set of THttpRequestState;

  /// customize THttpRequestContext process
  THttpRequestOptions = set of (
    hroHeadersUnfiltered);

  /// map the presence of some HTTP headers for THttpRequestContext.HeaderFlags
  // - separated from THttpRequestReponseFlags so that they would both be stored
  // and accessed as a single byte - which is faster than word on Intel CPUs
  // - do not modify unless you fix the associated ToText() overloaded function
  THttpRequestHeaderFlags = set of (
    nfHeadersParsed,
    hfTransferChunked,
    hfConnectionClose,
    hfConnectionUpgrade,
    hfConnectionKeepAlive,
    hfExpect100,
    hfHasRemoteIP,
    hfHasAuthorization);

  /// map the output state for THttpRequestContext.ResponseFlags
  // - separated from THttpRequestHeaderFlags so that they would both be stored
  // and accessed as a single byte - which is faster than word on Intel CPUs
  THttpRequestReponseFlags = set of (
    rfAcceptRange,
    rfWantRange,
    rfRange,
    rfHttp10,
    rfContentStreamNeedFree,
    rfAsynchronous);

  PHttpRequestContext = ^THttpRequestContext;

  /// raw information used during THttpRequestContext header parsing
  TProcessParseLine = record
    P: PUtf8Char;
    Len: PtrInt;
    Line: PUtf8Char;
    LineLen: PtrInt;
  end;

  /// low-level reusable State Machine to parse and process any HTTP content
  // - shared e.g. by all our (web)socket-based HTTP client and server classes
  // - reduce memory allocations as much as possible, and parse the most used
  // headers in explicit fields
  {$ifdef USERECORDWITHMETHODS}
  THttpRequestContext = record
  {$else}
  THttpRequestContext = object
  {$endif USERECORDWITHMETHODS}
  private
    ContentLeft: Int64;
    ContentPos: PByte;
    ContentEncoding, CommandUriInstance, LastHost: RawUtf8;
    CommandUriInstanceLen: PtrInt;
    procedure SetRawUtf8(var res: RawUtf8; P: pointer; PLen: PtrInt;
      nointern: boolean);
    function ProcessParseLine(var st: TProcessParseLine): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    procedure GetTrimmed(P, P2: PUtf8Char; L: PtrInt; var result: RawUtf8;
      nointern: boolean = false);
      {$ifdef HASINLINE} inline; {$endif}
    function ValidateRange: boolean;
  public
    // reusable buffers for internal process - do not use
    Head, Process: TRawByteStringBuffer;
    /// the current state of this HTTP context
    State: THttpRequestState;
    /// map the presence of some HTTP headers, retrieved during ParseHeader
    HeaderFlags: THttpRequestHeaderFlags;
    /// some flags used when sending the response
    ResponseFlags: THttpRequestReponseFlags;
    /// customize the HTTP process
    Options: THttpRequestOptions;
    /// could be set so that ParseHeader/GetTrimmed will intern RawUtf8 values
    Interning: PRawUtf8InterningSlot;
    /// will contain the first header line on client side
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    // - THttpServerSocket will use it, but THttpAsyncServer won't
    CommandResp: RawUtf8;
    /// the HTTP method parsed from first header line, e.g. 'GET'
    CommandMethod: RawUtf8;
    /// the HTTP URI parsed from first header line, e.g. '/path/to/resource'
    CommandUri: RawUtf8;
    /// will contain all header lines after all ParseHeader
    // - use HeaderGetValue() to get one HTTP header item value by name
    Headers: RawUtf8;
    /// same as HeaderGetValue('CONTENT-TYPE'), but retrieved during ParseHeader
    ContentType: RawUtf8;
    /// same as HeaderGetValue('ACCEPT-ENCODING'), but retrieved during ParseHeader
    AcceptEncoding: RawUtf8;
    /// same as HeaderGetValue('HOST'), but retrieved during ParseHeader
    Host: RawUtf8;
    /// same as HeaderGetValue('USER-AGENT'), but retrieved during ParseHeader
    UserAgent: RawUtf8;
    /// same as HeaderGetValue('UPGRADE'), but retrieved during ParseHeader
    Upgrade: RawUtf8;
    /// same as HeaderGetValue('REFERER'), but retrieved during ParseHeader
    Referer: RawUtf8;
    /// same as FindNameValue(aInHeaders, HEADER_BEARER_UPPER, ...),
    // but retrieved during ParseHeader
    // - is the raw Token, excluding 'Authorization: Bearer ' trailing chars
    // - if hsrAuthorized is set, THttpServerSocketGeneric.Authorization() will
    // put the authenticate User name in this field
    BearerToken: RawUtf8;
    /// decoded 'Range: bytes=..' start value - default is 0
    // - e.g. 1024 for 'Range: bytes=1024-1025'
    // - equals -1 in case on unsupported multipart range requests
    RangeOffset: Int64;
    /// decoded 'Range: bytes=...' end value - default is -1 (until end of file)
    // - e.g. 2 for 'Range: bytes=1024-1025'
    // - e.g. -1 for 'Range: bytes=1024-'
    // - contains size for CompressContentAndFinalizeHead Content-Range: header
    RangeLength: Int64;
    /// will contain the data retrieved from the server, after all ParseHeader
    Content: RawByteString;
    /// same as HeaderGetValue('CONTENT-LENGTH'), but retrieved during ParseHeader
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: Int64;
    /// stream-oriented alternative to the Content in-memory buffer
    // - is typically a TFileStreamEx
    ContentStream: TStream;
    /// same as HeaderGetValue('SERVER-INTERNALSTATE'), but retrieved by ParseHeader
    // - proprietary header, used with our RESTful ORM access
    ServerInternalState: integer;
    /// the known Content-Encoding compression methods
    Compress: THttpSocketCompressRecDynArray;
    /// supported Content-Encoding compression methods as sent to the other side
    CompressAcceptEncoding: RawUtf8;
    /// index of protocol in Compress[], from Accept-encoding
    CompressAcceptHeader: THttpSocketCompressSet;
    /// same as HeaderGetValue('CONTENT-ENCODING'), but retrieved by ParseHeader
    // and mapped into the Compress[] array
    CompressContentEncoding: integer;
    /// reset this request context to be used without any ProcessInit/Read/Write
    procedure Clear;
    /// parse a HTTP header text line into Header and fill internal properties
    // - with default HeadersUnFiltered=false, only relevant headers are retrieved:
    // use directly the ContentLength/ContentType/ServerInternalState/Upgrade
    // and HeaderFlags fields since HeaderGetValue() would return ''
    // - force HeadersUnFiltered=true to store all headers including the
    // connection-related fields, but increase memory and reduce performance
    procedure ParseHeader(P: PUtf8Char; PLen: PtrInt;
      HeadersUnFiltered: boolean = false);
    /// to be called once all ParseHeader lines have been done to fill Headers
    // - also set CompressContentEncoding/CompressAcceptHeader from Compress[]
    // and Content-Encoding header value
    procedure ParseHeaderFinalize;
    /// parse Command into CommandMethod/CommandUri fields
    function ParseCommand: boolean;
    /// search a value from the internal parsed Headers
    // - supplied aUpperName should be already uppercased:
    // HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - note that GetHeader(HeadersUnFiltered=false) will set ContentType field
    // but let HeaderGetValue('CONTENT-TYPE') return ''
    function HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    /// search if a value exists from the internal parsed Headers
    function HeaderHasValue(const aUpperName: RawUtf8): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// initialize ContentStream/ContentLength from a given file name
    // - if CompressGz is set, would also try for a cached local FileName+'.gz'
    function ContentFromFile(const FileName: TFileName; CompressGz: integer): boolean;
    /// uncompress Content according to CompressContentEncoding header
    procedure UncompressData;
    /// (re)initialize the HTTP Server state machine for ProcessRead/ProcessWrite
    procedure ProcessInit(InStream: TStream);
      {$ifdef HASINLINE} inline; {$endif}
    /// receiving socket entry point of our asynchronous HTTP Server
    // - to be called with the incoming bytes from the socket receive buffer
    // - caller should have checked that current State is in HTTP_REQUEST_READ
    // - returns true if a new State was reached, or false if some more
    // input is needed
    function ProcessRead(var st: TProcessParseLine): boolean;
    /// compress Content according to CompressAcceptHeader, adding headers
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Content is not '', will add 'Content-Type: ' header
    // - always compute ContentLength and add a 'Content-Length: ' header
    // - then append small content (<MaxSizeAtOnce) to result if possible, and
    // refresh the final State to hrsSendBody/hrsResponseDone
    function CompressContentAndFinalizeHead(MaxSizeAtOnce: integer): PRawByteStringBuffer;
    /// body sending socket entry point of our asynchronous HTTP Server
    // - to be called when some bytes could be written to output socket
    procedure ProcessBody(var Dest: TRawByteStringBuffer; MaxSize: PtrInt);
    /// should be done when the HTTP Server state machine is done
    // - will check and process hfContentStreamNeedFree flag
    procedure ProcessDone;
      {$ifdef HASINLINE} inline; {$endif}
  end;

const
  /// when THttpRequestContext.State is expected some ProcessRead() data
  HTTP_REQUEST_READ =
    [hrsGetCommand,
     hrsGetHeaders,
     hrsGetBodyChunkedHexFirst,
     hrsGetBodyChunkedHexNext,
     hrsGetBodyChunkedData,
     hrsGetBodyChunkedDataVoidLine,
     hrsGetBodyContentLength];

  /// when THttpRequestContext.State is expected some ProcessWrite() data
  HTTP_REQUEST_WRITE =
    [hrsSendBody];


function ToText(st: THttpRequestState): PShortString; overload;
function ToText(hf: THttpRequestHeaderFlags): TShort8; overload;
function ToText(csp: TCrtSocketPending): PShortString; overload;
function ToText(tls: TCrtSocketTlsAfter): PShortString; overload;
function ToText(mak: TMacAddressKind): PShortString; overload;


{ ******************** THttpSocket Implementing HTTP over plain sockets }

type
  /// exception class raised during HTTP process
  EHttpSocket = class(ESynException);

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing HTTP/1.1 using the Socket API
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlz) protocols
  THttpSocket = class(TCrtSocket)
  protected
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// fill the internal state and flags to their default/void values
    procedure HttpStateReset;
    procedure CompressDataAndWriteHeaders(const OutContentType: RawUtf8;
      var OutContent: RawByteString; OutStream: TStream);
  public
    /// the whole context of the HTTP request
    Http: THttpRequestContext;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    // - with default HeadersUnFiltered=false, only relevant headers are retrieved:
    // use directly the ContentLength/ContentType/ServerInternalState/Upgrade
    // and HeaderFlags fields since HeaderGetValue() would return ''
    // - force HeadersUnFiltered=true to store all headers including the
    // connection-related fields, but increase memory and reduce performance
    function GetHeader(HeadersUnFiltered: boolean = false): boolean;
    /// retrieve the HTTP body (after uncompression if necessary)
    // - into Content or DestStream
    procedure GetBody(DestStream: TStream = nil);
    /// add an header 'name: value' entry
    procedure HeaderAdd(const aValue: RawUtf8);
    /// set all Header values at once, from CRLF delimited text
    // - won't parse the ContentLength/ContentType/ServerInternalState/Upgrade
    // and HeaderFlags fields
    procedure HeaderSetText(const aText: RawUtf8; const aForcedContentType: RawUtf8 = '');
    /// finalize all Http.Headers values
    // - you can optionally specify a value to be added as 'RemoteIP: ' header
    // - default GetHeader(HeadersUnFiltered=false) won't include the connection
    // related headers like ContentLength/ContentType/ServerInternalState/Upgrade
    procedure HeadersPrepare(const aRemoteIP: RawUtf8);
    /// HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - supplied aUpperName should be already uppercased
    // - note that GetHeader(HeadersUnFiltered=false) will set ContentType field
    // but let HeaderGetValue('CONTENT-TYPE') return ''
    function HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
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
  end;



{ ******************** Abstract Server-Side Types e.g. for Client-Server Protocol }

type
  {$M+} // to have existing RTTI for published properties
  THttpServerRequestAbstract = class;
  {$M-}

  /// a genuine identifier for a given client connection on server side
  // - maps e.g. http.sys ID, or a genuine 31-bit sequence increasing counter,
  // or the 'X-Conn-ID' header value behind a nginx reverse proxy
  THttpServerConnectionID = Int64;

  /// a dynamic array of client connection identifiers, e.g. for broadcasting
  THttpServerConnectionIDDynArray = array of THttpServerConnectionID;

  /// an opaque connection-specific pointers identifier with a strong type
  // - each THttpAsyncConnection or THttpServerSocket raw connection instance
  // maintains those two abstract PtrUInt tags, as a fConnectionOpaque field
  // - match TRestServerConnectionOpaque as defined in mormot.rest.core
  THttpServerConnectionOpaque = record
    /// pointer-sized tag reserved to mORMot (e.g. to idenfity a REST session)
    ValueInternal: PtrUInt;
    /// pointer-sized tag free for the end-user code
    // - could be used to avoid a lookup to a ConnectionID-indexed dictionary
    ValueExternal: PtrUInt;
  end;
  /// reference to an opaque connection-specific pointer identifier
  // - may be nil if unsupported, e.g. by the http.sys servers
  PHttpServerConnectionOpaque = ^THttpServerConnectionOpaque;

  /// the server-side available authentication schemes
  // - as used by THttpServerRequest.AuthenticationStatus
  // - hraNone..hraKerberos will match low-level HTTP_REQUEST_AUTH_TYPE enum as
  // defined in HTTP 2.0 API
  THttpServerRequestAuthentication = (
    hraNone,
    hraFailed,
    hraBasic,
    hraDigest,
    hraNtlm,
    hraNegotiate,
    hraKerberos);

  /// available THttpServerRequest connection attributes
  // - hsrHttps is set if the communication was made over HTTPS
  // - hsrSecured if the transmission is encrypted or in-process, using
  // e.g. HTTPS/TLS or our proprietary AES/ECDHE algorithm over WebSockets
  // - hsrWebsockets if communication was made using WebSockets
  // - hsrInProcess when run from the same process, i.e. on server side
  // - hsrConnectionUpgrade when "connection: upgrade" is within headers
  // - hsrAuthorized when a valid "authorization:" header is set
  // - should exactly match TRestUriParamsLowLevelFlag in mormot.rest.core
  THttpServerRequestFlag = (
    hsrHttps,
    hsrSecured,
    hsrWebsockets,
    hsrInProcess,
    hsrConnectionUpgrade,
    hsrAuthorized);

  /// the THttpServerRequest connection attributes
  THttpServerRequestFlags = set of THttpServerRequestFlag;

  /// event handler used by THttpServerGeneric.OnRequest, OnBeforeRequest and
  // OnAfterRequest
  // - Ctxt defines both input and output parameters
  // - result of the function is the HTTP status/error code (200 if OK, e.g.)
  TOnHttpServerRequest = function(Ctxt: THttpServerRequestAbstract): cardinal of object;

  /// event handler used by THttpServerGeneric.OnAfterResponse property
  // - main purpose is to apply post-response analysis, logging, etc...
  // - Code defines the HTTP response status the (200 if OK, e.g.)
  TOnHttpServerAfterResponse = procedure(Connection: THttpServerConnectionID;
    const User, Method, Host, Url, RemoteIP: RawUtf8;
    Flags: THttpServerRequestFlags; Code: cardinal) of object;

  /// event handler used by THttpServerGeneric.OnBeforeBody property
  // - if defined, is called just before the body is retrieved from the client
  // - supplied parameters reflect the current input state, and could be
  // modified on the fly to adapt to the expected behavior
  // - aBearerToken is either the "Authorization: Bearer xxxx" token, or
  // the authenticated user name if the hsrAuthorized flag is set
  // - should return HTTP_SUCCESS=200 to continue the process, or an HTTP error
  // code (e.g. HTTP_FORBIDDEN or HTTP_PAYLOADTOOLARGE) to reject the request
  // - returning HTTP_UNAUTHORIZED triggers the internal authentication process
  // available on some servers (e.g. THttpApiServer or THttpAsyncServer)
  TOnHttpServerBeforeBody = function(var aUrl, aMethod, aInHeaders,
    aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: Int64;
    aFlags: THttpServerRequestFlags): cardinal of object;

  /// event handler used by THttpServer.Process to send a local file
  // when STATICFILE_CONTENT_TYPE content-type is returned by the service
  // - can be defined e.g. to use NGINX X-Accel-Redirect header
  // - should return true if the Context has been modified to serve the file, or
  // false so that the file will be manually read and sent from memory
  // - any exception during process will be returned as a HTTP_NOTFOUND page
  TOnHttpServerSendFile = function(Context: THttpServerRequestAbstract;
    const LocalFileName: TFileName): boolean of object;

  {$M+}
  /// abstract generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  // - this abstract class may be used in communication protocols, without
  // the need to add mormot.net.server.pas dependency
  THttpServerRequestAbstract = class
  protected
    fRemoteIP,
    fUrl,
    fMethod,
    fInHeaders,
    fInContentType,
    fAuthenticatedUser,
    fHost,
    fAuthBearer,
    fUserAgent,
    fOutContentType,
    fOutCustomHeaders,
    fRespReason: RawUtf8;
    fInContent,
    fOutContent: RawByteString;
    fConnectionID: THttpServerConnectionID;
    fConnectionFlags: THttpServerRequestFlags;
    fAuthenticationStatus: THttpServerRequestAuthentication;
    fRespStatus: integer;
    fConnectionThread: TThread;
    fConnectionOpaque: PHttpServerConnectionOpaque;
    fUrlParamPos: PUtf8Char; // may be set by TUriTreeNode.LookupParam
    fRouteNode: TRadixTreeNodeParams; // is a TUriTreeNode
    fRouteName: pointer; // set by TUriTreeNode.LookupParam
    fRouteValuePosLen: TIntegerDynArray; // [pos1,len1,...] pairs in fUri
    function GetRouteValuePosLen(const Name: RawUtf8;
      var Value: TValuePUtf8Char): boolean;
    function GetRouteValue(const Name: RawUtf8): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// prepare an incoming request from a parsed THttpRequestContext
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - won't reset other parameters: should come after a plain Create or
    // an explicit THttpServerRequest.Recycle()
    procedure Prepare(const aHttp: THttpRequestContext; const aRemoteIP: RawUtf8;
      aAuthorize: THttpServerRequestAuthentication); overload;
    /// prepare an incoming request from explicit values
    // - could be used for non-HTTP execution, e.g. from a WebSockets link
    procedure Prepare(const aUrl, aMethod, aInHeaders: RawUtf8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(AppendedHeader: RawUtf8);
    /// append some values to the OutCustomHeaders output parameter
    // - will maintain CRLF between lines, but not on the last line
    procedure AddOutHeader(const Values: array of const);
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    property InContent: RawByteString
      read fInContent write fInContent;
    /// output parameter to be set to the response message body
    property OutContent: RawByteString
      read fOutContent write fOutContent;
    /// the thread which owns the connection of this execution context
    // - may be nil, depending on the HTTP server used
    // - depending on the HTTP server used, may not follow ConnectionID
    property ConnectionThread: TThread
      read fConnectionThread;
    /// some HTTP servers support a per-connection pointer storage
    // - may be nil if unsupported, e.g. by the http.sys servers
    // - could be used to avoid a lookup to a ConnectionID-indexed dictionary
    property ConnectionOpaque: PHttpServerConnectionOpaque
      read fConnectionOpaque;
    /// returns the TUriRouter <parameter> value parsed from URI as text
    // - Name lookup is case-sensitive
    // - is the default property to this function, so that you could write
    // ! Ctxt['param']
    property RouteValue[const Name: RawUtf8]: RawUtf8
      read GetRouteValue; default;
    /// returns the TUriRouter <parameter> value parsed from URI as Int64
    // - Name lookup is case-sensitive
    function RouteInt64(const Name: RawUtf8; out Value: Int64): boolean;
    /// returns the TUriRouter <parameter> value parsed from URI as RawUtf8
    // - Name lookup is case-sensitive
    function RouteUtf8(const Name: RawUtf8; out Value: RawUtf8): boolean;
    /// check a TUriRouter <parameter> value parsed from URI
    // - both Name lookup and value comparison are case-sensitive
    function RouteEquals(const Name, ExpectedValue: RawUtf8): boolean;
    /// retrieve and decode an URI-encoded parameter as UTF-8 text
    // - UpperName should follow the UrlDecodeValue() format, e.g. 'NAME='
    function UrlParam(const UpperName: RawUtf8; out Value: RawUtf8): boolean; overload;
    /// retrieve and decode an URI-encoded parameter as 32-bit unsigned cardinal
    // - UpperName should follow the UrlDecodeCardinal() format, e.g. 'COUNT='
    function UrlParam(const UpperName: RawUtf8; out Value: cardinal): boolean; overload;
    /// retrieve and decode an URI-encoded parameter as 64-bit signed Int64
    // - UpperName should follow the UrlDecodeInt64() format, e.g. 'ID='
    function UrlParam(const UpperName: RawUtf8; out Value: Int64): boolean; overload;
    /// set the OutContent and OutContentType fields with the supplied JSON
    procedure SetOutJson(const Json: RawUtf8); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// set the OutContent and OutContentType fields with the supplied JSON
    procedure SetOutJson(const Fmt: RawUtf8; const Args: array of const); overload;
    /// set the OutContent and OutContentType fields with the supplied text
    procedure SetOutText(const Fmt: RawUtf8; const Args: array of const;
      const ContentType: RawUtf8 = TEXT_CONTENT_TYPE);
  published
    /// input parameter containing the caller URI
    property Url: RawUtf8
      read fUrl write fUrl;
    /// input parameter containing the caller method (GET/POST...)
    property Method: RawUtf8
      read fMethod write fMethod;
    /// input parameter containing the caller message headers
    property InHeaders: RawUtf8
      read fInHeaders write fInHeaders;
    // input parameter defining the caller message body content type
    property InContentType: RawUtf8
      read fInContentType write fInContentType;
    /// output HTTP response status
    property RespStatus: integer
      read fRespStatus write fRespStatus;
    /// output parameter to define the reponse message body content type
    // - if OutContentType is STATICFILE_CONTENT_TYPE (i.e. '!STATICFILE'),
    // then OutContent is the UTF-8 file name of a file to be sent to the
    // client via http.sys or NGINX's X-Accel-Redirect header (faster than
    // local buffering/sending)
    // - if OutContentType is NORESPONSE_CONTENT_TYPE (i.e. '!NORESPONSE'), then
    // the actual transmission protocol may not wait for any answer - used
    // e.g. for WebSockets
    property OutContentType: RawUtf8
      read fOutContentType write fOutContentType;
    /// output parameter to be sent back as the response message header
    // - e.g. to set Content-Type/Location
    property OutCustomHeaders: RawUtf8
      read fOutCustomHeaders write fOutCustomHeaders;
    /// the client remote IP, as specified to Prepare()
    property RemoteIP: RawUtf8
      read fRemoteIP write fRemoteIP;
    /// the "Host" HTTP header token, as specified to Prepare()
    property Host: RawUtf8
      read fHost write fHost;
    /// the "Bearer" HTTP header token, as specified to Prepare()
    property AuthBearer: RawUtf8
      read fAuthBearer write fAuthBearer;
    /// the "User-Agent" HTTP header token, as specified to Prepare()
    property UserAgent: RawUtf8
      read fUserAgent write fUserAgent;
    /// the ID of the connection which called this execution context
    // - e.g. mormot.net.websocket's TWebSocketProcess.NotifyCallback method
    // would use this property to specify the client connection to be notified
    // - is set as an Int64 to match http.sys ID type, but will be an
    // increasing 31-bit integer sequence for (web)socket-based servers
    property ConnectionID: THttpServerConnectionID
      read fConnectionID;
    /// define how the client is connected
    property ConnectionFlags: THttpServerRequestFlags
      read fConnectionFlags write fConnectionFlags;
    /// contains the THttpServer-side authentication status
    // - e.g. when using http.sys authentication with HTTP API 2.0
    property AuthenticationStatus: THttpServerRequestAuthentication
      read fAuthenticationStatus write fAuthenticationStatus;
    /// contains the THttpServer-side authenticated user name, UTF-8 encoded
    // - e.g. when using http.sys authentication with HTTP API 2.0, the
    // domain user name is retrieved from the supplied AccessToken
    // - could also be set by the THttpServerGeneric.Request() method, after
    // proper authentication, so that it would be logged as expected
    property AuthenticatedUser: RawUtf8
      read fAuthenticatedUser write fAuthenticatedUser;
  end;
  {$M-}

  /// store a list of IPv4 which should be rejected at connection
  // - more tuned than TIPBan for checking just after accept()
  // - used e.g. to implement hsoBan40xIP or THttpPeerCache instable
  // peers list (with a per-minute resolution)
  // - the DoRotate method should be called every second
  THttpAcceptBan = class(TSynPersistent)
  protected
    fSafe: TOSLightLock; // almost never on contention, no R/W needed
    fCount, fLastSec: integer;
    fIP: array of TCardinalDynArray; // one [0..fMax] IP array per second
    fSeconds, fMax, fWhiteIP: cardinal;
    fRejected, fTotal: Int64;
    function IsBannedRaw(ip4: cardinal): boolean;
    procedure SetMax(Value: cardinal);
    procedure SetSeconds(Value: cardinal);
    procedure SetIP;
  public
    /// initialize the thread-safe storage process
    // - banseconds should be a power-of-two <= 128
    // - maxpersecond is the maximum number of banned IPs remembered per second
    constructor Create(banseconds: cardinal = 4; maxpersecond: cardinal = 1024;
      banwhiteip: cardinal = cLocalhost32); reintroduce;
    /// finalize this storage
    destructor Destroy; override;
    /// register an IP4 to be rejected
    function BanIP(ip4: cardinal): boolean; overload;
    /// register an IP4 to be rejected
    function BanIP(const ip4: RawUtf8): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// fast check if this IP4 is to be rejected
    function IsBanned(const addr: TNetAddr): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// fast check if this IP4 is to be rejected
    function IsBanned(ip4: cardinal): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// register an IP4 if status in >= 400 (but not 401 HTTP_UNAUTHORIZED)
    function ShouldBan(status, ip4: cardinal): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// register an IP4 if status in >= 400 (but not 401 HTTP_UNAUTHORIZED)
    function ShouldBan(status: cardinal; const ip4: RawUtf8): boolean; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// to be called every second to remove deprecated bans from the list
    // - implemented via a round-robin list of per-second banned IPs
    // - if you call it at another pace (e.g. every minute), then the list
    // Time-To-Live will follow this unit of time instead of seconds
    procedure DoRotate;
    /// a 32-bit IP4 which should never be banned
    // - is set to cLocalhost32, i.e. 127.0.0.1, by default
    property WhiteIP: cardinal
      read fWhiteIP write fWhiteIP;
    /// how many seconds a banned IP4 should be rejected
    // - should be a power of two, up to 128, with a default of 4 - the closed
    // power of two is selected if the Value is not an exact match
    // - if set, any previous banned IP will be flushed
    property Seconds: cardinal
      read fSeconds write SetSeconds;
    /// how many IP can be banned per second
    // - used to reduce memory allocation and O(n) search speed
    // - over this limit, BanIP() will store and replace at the last position
    // - assign 0 to disable the banning feature
    // - if set, any previous banned IP will be flushed
    property Max: cardinal
      read fMax write SetMax;
  published
    /// total number of accept() rejected by IsBanned()
    property Rejected: Int64
      read fRejected;
    /// total number of banned IP4 since the beginning
    property Total: Int64
      read fTotal;
    /// current number of banned IP4
    property Count: integer
      read fCount;
  end;

type
  /// most used tuning options for a modern and safe HTTP/HTTPS Server
  // - over the years, a lot of expectations where added to the basic behavior
  // of a HTTP server, e.g. for better security or interoperability: we define
  // a set of well-known behaviors
  // - flags used e.g. by TWebServerLocal, or mORMot 2 Boilerplate project
  TWebServerBehavior = (

    /// Allow cross-origin requests.
    // - see https://enable-cors.org https://www.w3.org/TR/cors
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
    // - Warning: Do not use this without understanding the consequences.
    // This will permit access from any other website.
    // Instead of using this file, consider using a specific rule such as
    // allowing access based on (sub)domain: "subdomain.example.com"
    wsbAllowCrossOrigin,

    /// Send the CORS header for images when browsers request it
    // - see
    // https://developer.mozilla.org/en-US/docs/Web/HTML/CORS_enabled_image
    // https://blog.chromium.org/2011/07/using-cross-domain-images-in-webgl-and.html
    // - use TWebServerLocal.FileTypesImage to specify the actual file types
    wsbAllowCrossOriginImages,

    /// Allow cross-origin access to web fonts
    // - see https://developers.google.com/fonts/docs/troubleshooting
    // - use TWebServerLocal.FileTypesFont to specify file types
    wsbAllowCrossOriginFonts,

    /// Allow cross-origin access to the timing information for all resources
    // - If a resource isn't served with a 'Timing-Allow-Origin' header that would
    // allow its timing information to be shared with the document, some of the
    // attributes of the 'PerformanceResourceTiming' object will be set to zero.
    // - see https://www.w3.org/TR/resource-timing/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Timing-Allow-Origin
    // https://www.stevesouders.com/blog/2014/08/21/resource-timing-practical-tips/
    wsbAllowCrossOriginTiming,

    /// Set content for HTTP 400 "Bad Request" response code equals to '/404'
    wsbDelegateBadRequestTo404,

    /// Set content for HTTP 401 "Unauthorized" response code equals to '/404'
    wsbDelegateUnauthorizedTo404,

    /// Set content for HTTP 403 "Forbidden" response code equals to '/404'
    wsbDelegateForbiddenTo404,

    /// Set content for HTTP 404 "Not Found" response code equals to '/404'
    wsbDelegateNotFoundTo404,

    /// Set content for HTTP 405 "Not Allowed" response code equals to '/404'
    wsbDelegateNotAllowedTo404,

    /// Set content for HTTP 406 "Not Acceptable" response code equals to '/404'
    wsbDelegateNotAcceptableTo404,

    /// Set Internet Explorer XUA Document mode
    // - Force Internet Explorer 8/9/10 to render pages in the highest mode
    // available in various cases when it may not.
    // - Warning: since Internet Explorer 11, document modes are deprecated.
    // If your business still relies on older web apps and services that were
    // designed for older versions of Internet Explorer, you might want to
    // consider enabling 'Enterprise Mode' throughout your company.
    // - see https://hsivonen.fi/doctype/#ie8
    // https://msdn.microsoft.com/en-us/library/ie/bg182625.aspx#docmode
    // https://blogs.msdn.microsoft.com/ie/2014/04/02/stay-up-to-date-with-enterprise-mode-for-internet-explorer-11/
    // https://msdn.microsoft.com/en-us/library/ff955275.aspx
    wsbSetXuaCompatible,

    // Serve resources with the proper media types (f.k.a. MIME types)
    // - use TWebServerGlobal.ForceMimeTypes/ForceMimeTypesExt to set the MIME types
    // - see http://www.iana.org/assignments/media-types
    // https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types
    wsbForceMimeType,

    // Serve all resources labeled as 'text/html' or 'text/plain'
    // with the media type 'charset' parameter set to 'UTF-8'
    wsbForceTextUtf8Charset,

    /// Serve the specified file types with the media type 'charset' parameter
    // set to 'UTF-8'
    // - use TWebServerLocal.FileTypesRequiredCharSet to setup file types
    wsbForceUtf8Charset,

    /// Redirect from 'http://' to the 'https://' version of the URL
    wsbForceHttps,

    /// Forcing 'https://' in the context of Let's Encrypt (ACME)
    // - If you're using cPanel AutoSSL or the Let's Encrypt webroot method it
    // will fail to validate the certificate if validation requests are
    // redirected to HTTPS. Turn on the condition(s) you need.
    // - see https://www.iana.org/assignments/well-known-uris/well-known-uris.xhtml
    //  https://tools.ietf.org/html/draft-ietf-acme-acme-12
    // $ /.well-known/acme-challenge/
    // $ /.well-known/cpanel-dcv/[\w-]+$
    // $ /.well-known/pki-validation/[A-F0-9]{32}\.txt(?:\ Comodo\ DCV)?$
    // - The next simplified patterns are used:
    // $ /.well-known/acme-challenge/*
    // $ /.well-known/cpanel-dcv/*
    // $ /.well-known/pki-validation/*
    wsbForceHttpsExceptLetsEncrypt,

    /// Protect website against clickjacking
    // - The example below sends the 'X-Frame-Options' response header with the
    // value 'DENY', informing browsers not to display the content of the web
    // page in any frame.
    // - This might not be the best setting for everyone. You should read about
    // the other two possible values the 'X-Frame-Options' header field can
    // have: 'SAMEORIGIN' and 'ALLOW-FROM'.
    // https://tools.ietf.org/html/rfc7034#section-2.1.
    // - Keep in mind that while you could send the 'X-Frame-Options' header for
    // all of your website's pages, this has the potential downside that it
    // forbids even non-malicious framing of your content (e.g.: when users
    // visit your website using a Google Image Search results page).
    // - Nonetheless, you should ensure that you send the 'X-Frame-Options' header
    // for all pages that allow a user to make a state-changing operation
    // (e.g: pages that contain one-click purchase links, checkout or
    // bank-transfer confirmation pages, pages that make permanent configuration
    // changes, etc.).
    // - Sending the 'X-Frame-Options' header can also protect your website
    // against more than just clickjacking attacks.
    // - see https://cure53.de/xfo-clickjacking.pdf.
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options
    // https://tools.ietf.org/html/rfc7034
    // https://blogs.msdn.microsoft.com/ieinternals/2010/03/30/combating-clickjacking-with-x-frame-options/
    // https://www.owasp.org/index.php/Clickjacking
    wsbSetXFrameOptions,

    /// Block access to all hidden files and directories except for the
    // visible content from within the '/.well-known/' hidden directory
    // - These types of files usually contain user preferences or the preserved
    // state of a utility, and can include rather private places like, for
    // example, the '.git' or '.svn' directories.
    // - The '/.well-known/' directory represents the standard (RFC 5785) path
    // prefix for "well-known locations" (e.g.: '/.well-known/manifest.json',
    // '/.well-known/keybase.txt'), and therefore, access to its visible content
    // should not be blocked.
    // - see https://www.mnot.net/blog/2010/04/07/well-known
    // https://tools.ietf.org/html/rfc5785
    wsbDelegateHidden,

    /// Block access to files that can expose sensitive information
    // - By default, block access to backup and source files that may be left by
    // some text editors and can pose a security risk when anyone has access to
    // them. see https://feross.org/cmsploit/
    //  - Use TWebServerLocal.FileTypesBlocked to specify file types
    // that might end up on your production server and can expose sensitive
    // information about your website. These files may include:
    // configuration files, files that contain metadata about the project
    // (e.g.: project dependencies, build scripts, etc.).
    // - use TWebServerLocal.FileTypesBlocked to specify file types
    // - this option also blocks any URL paths ended with '~' or '#'
    wsbDelegateBlocked,

    /// Prevent some browsers from MIME-sniffing the response
    // - This reduces exposure to drive-by download attacks and cross-origin data
    // leaks, and should be left uncommented, especially if the server is
    // serving user-uploaded content or content that could potentially be
    // treated as executable by the browser.
    // - see https://mimesniff.spec.whatwg.org/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Content-Type-Options
    // https://blogs.msdn.microsoft.com/ie/2008/07/02/ie8-security-part-v-comprehensive-protection/
    wsbPreventMimeSniffing,

    /// Protect website reflected Cross-Site Scripting (XSS) attacks
    // - Try to re-enable the cross-site scripting (XSS) filter built into
    // most web browsers.
    // - The filter is usually enabled by default, but in some cases, it may
    // be disabled by the user. However, in Internet Explorer, for example,
    // it can be re-enabled just by sending the  'X-XSS-Protection' header
    // with the value of '1'.
    // - Prevent web browsers from rendering the web page if a potential
    // reflected (a.k.a non-persistent) XSS attack is detected by the filter.
    // - By default, if the filter is enabled and browsers detect a reflected
    // XSS attack, they will attempt to block the attack by making the
    // smallest possible modifications to the returned web page.
    // - Unfortunately, in some browsers (e.g.: Internet Explorer), this
    // default behavior may allow the XSS filter to be exploited. Therefore,
    // it's better to inform browsers to prevent the rendering of the page
    // altogether, instead of attempting to modify it.
    // - warning: Do not rely on the XSS filter to prevent XSS attacks! Ensure that you
    // are taking all possible measures to prevent XSS attacks, the most
    // obvious being: validating and sanitizing your website's inputs.
    // - see https://hackademix.net/2009/11/21/ies-xss-filter-creates-xss-vulnerabilities
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-XSS-Protection
    // https://blogs.msdn.microsoft.com/ie/2008/07/02/ie8-security-part-iv-the-xss-filter/
    // https://blogs.msdn.microsoft.com/ieinternals/2011/01/31/controlling-the-xss-filter/
    // https://www.owasp.org/index.php/Cross-site_Scripting_%28XSS%29
    // - use TWebServerLocal.FileTypesAsset to exclude some file types
    wsbEnableXssFilter,

    /// Set a strict Referrer Policy to mitigate information leakage.
    // - The 'Referrer-Policy' header is included in responses for resources
    // that are able to request (or navigate to) other resources.
    // - This includes the commonly used resource types:
    // HTML, CSS, XML/SVG, PDF documents, scripts and workers.
    // - To prevent referrer leakage entirely, specify the 'no-referrer' value
    // instead. Note that the effect could impact analytics metrics negatively.
    // - // To check your Referrer Policy, you can use an online service, such as:
    // https://securityheaders.com/
    // https://observatory.mozilla.org/
    // https://scotthelme.co.uk/a-new-security-header-referrer-policy/
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy
    // - use TWebServerLocal.ReferrerPolicy and
    // TWebServerLocal.ReferrerPolicyContentTypes properties
    wsbEnableReferrerPolicy,

    /// Prevent the HTTP Server from responding to 'TRACE' HTTP requests
    // - The TRACE method, while seemingly harmless, can be successfully leveraged
    // in some scenarios to steal legitimate users' credentials.
    // - Modern browsers now prevent TRACE requests being made via JavaScript,
    // however, other ways of sending TRACE requests with browsers have been
    // discovered, such as using Java.
    // - see https://tools.ietf.org/html/rfc7231#section-4.3.8
    // https://www.owasp.org/index.php/Cross_Site_Tracing
    // https://www.owasp.org/index.php/Test_HTTP_Methods_(OTG-CONFIG-006)
    // https://httpd.apache.org/docs/current/mod/core.html#traceenable
    wsbDisableTraceMethod,

    /// Remove the 'X-Powered-By' response header that:
    // - is set by some frameworks and server-side languages (e.g.: ASP.NET, PHP),
    // and its value contains information about them (e.g.: their name, version
    // number)
    // - doesn't provide any value to users, contributes to header bloat, and in
    // some cases, the information it provides can expose vulnerabilities
    // - If you can, you should disable the 'X-Powered-By' header from the
    //     language/framework level (e.g.: for PHP, you can do that by setting
    //     'expose_php = off' in 'php.ini').
    // - see https://php.net/manual/en/ini.core.php#ini.expose-php
    wsbDeleteXPoweredBy,

    /// Force compression for mangled 'Accept-Encoding' request headers
    // - see https://calendar.perfplanet.com/2010/pushing-beyond-gzipping
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
    // - use TWebServerLocal.MangledEncodingHeaders and
    // TWebServerLocal.MangledEncodingHeaderValues properties
    wsbFixMangledAcceptEncoding,

    /// Map the specified filename extensions to the GZip encoding type
    // - to let the HTTP Server serve the file types with the appropriate
    // 'Content-Encoding' response header (do note that this will NOT make
    // HTTP Server compress them!).
    // - If these files types would be served without an appropriate
    // 'Content-Encoding' response header, client applications (e.g.: browsers)
    // wouldn't know that they first need to uncompress the response, and thus,
    // wouldn't be able to understand the content.
    // - see https://httpd.apache.org/docs/current/mod/mod_mime.html#addencoding
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Encoding
    // - use TWebServerLocal.FileTypesForceGZipHeader to setup file types
    wsbForceGZipHeader,

    /// Allow static assets to be cached by proxy servers
    wsbSetCachePublic,

    /// Allow static assets to be cached only by browser,
    // but not by intermediate proxy servers
    wsbSetCachePrivate,

    /// disable Content transformation
    // - Prevent intermediate caches or proxies (such as those used by mobile
    // network providers) and browsers data-saving features from modifying
    // the website's content using the 'cache-control: no-transform' directive.
    // - see https://tools.ietf.org/html/rfc7234#section-5.2.2.4
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // - warning: Carefully consider the impact on your visitors before disabling
    // content transformation. These transformations are performed to
    // improve the experience for data- and cost-constrained users
    // (e.g. users on a 2G connection).
    // - You can test the effects of content transformation applied by
    // Google's Lite Mode by visiting:
    // https://googleweblight.com/i?u=https://www.example.com
    // https://support.google.com/webmasters/answer/6211428
    // https://developers.google.com/speed/pagespeed/module/configuration#notransform
    wsbSetCacheNoTransform,

    /// Allow static assets to be validated with server before return cached copy
    wsbSetCacheNoCache,

    /// Allow static assets not to be cached
    wsbSetCacheNoStore,

    /// Allow static assets to be cached strictly following the server rules
    wsbSetCacheMustRevalidate,

    /// Add 'max-age' value based on content-type/expires mapping
    // - i.e. serve resources with a far-future expiration date.
    // - warning: If you don't control versioning with filename-based cache
    // busting, you should consider lowering the cache times to something like
    // one week.
    // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // - use TWebServerLocal.Expires options to control expirations
    wsbSetCacheMaxAge,

    /// Use ETag / If-None-Match caching
    // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag
    // https://developer.yahoo.com/performance/rules.html#etags
    // https://tools.ietf.org/html/rfc7232#section-2.3
    wsbEnableCacheByETag,

    /// Use Last-Modified/If-Modified-Since caching
    // - see https://developer.yahoo.com/performance/rules.html#etags
    // https://tools.ietf.org/html/rfc7232#section-2.3
    wsbEnableCacheByLastModified,

    /// Serve resources with a far-future expiration date
    // - If you don't control versioning with filename-based cache busting, you
    // should consider lowering the cache times to something like one week.
    // - see https://httpd.apache.org/docs/current/mod/mod_expires.html
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control
    // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Expires
    // - use TWebServerLocal.Expires property
    wsbSetExpires,

    /// Enables filename-based cache busting
    // - i.e. removes all query path of the URL '/style.css?v231' to '/style.css'
    wsbEnableCacheBusting,

    /// Filename-based cache busting
    // - i.e. removes infix query path of the URL '/style.123456.css' to '/style.css'
    // - if you're not using a build process to manage your filename version
    // revving, you might want to consider enabling the following directives.
    // - to understand why this is important and even a better solution than
    // using something like '*.css?v231', please see:
    // https://www.stevesouders.com/blog/2008/08/23/revving-filenames-dont-use-querystring/
    wsbEnableCacheBustingBeforeExt,

    /// Remove 'Server-InternalState' HTTP header
    wsbDeleteServerInternalState,

    // Delete content generation for '' and '/' URLs to '/index.html'
    wsbDelegateRootToIndex,

    /// Instead of index.html rendering the inherited "/Default" URL will be called
    // - allows to inject custom IMVCApplication.Default() interface method
    wsbDelegateIndexToInheritedDefault,

    /// Instead of 404.html rendering the inherited "/404" URL will be called
    // - allows to inject custom IMVCApplication._404() interface method
    wsbDelegate404ToInherited_404,

    /// Add 'Vary: Accept-Encoding' header for assets with GZip/Brotli encoding
    wsbVaryAcceptEncoding
  );

  /// set of tuning options for a modern and safe HTTP/HTTPS Web Server
  // - flags used e.g. by TWebServerLocal, or the mORMot 2 Boilerplate project
  TWebServerBehaviors = set of TWebServerBehavior;

  /// Suppressing or forcing the 'www.' host prefix at the beginning of URLs
  // - The same content should never be available under two different URLs,
  // especially not with and without 'www.' at the beginning.
  // This can cause SEO problems (duplicate content), and therefore, you should
  // choose one of the alternatives and redirect the other one.
  // - The rule assumes by default that both HTTP and HTTPS environments are
  // available for redirection. If your SSL certificate could not handle one
  // of the domains used during redirection, you should turn the condition on.
  // - wsrOff: won't suppress or force 'www.' at the beginning of URLs
  // - wsrSuppress: suppress the 'www.' at the beginning of URLs,
  // redirecting e.g. www.example.com into example.com
  // - wsrForce: forces the 'www.' at the beginning of URLs,
  // redirecting e.g. example.com into www.example.com
  // - Be aware that wsrForce might not be a good idea if you use "real"
  // subdomains for certain parts of your website
  TWebServerRewrite = (
    wsrOff,
    wsrSuppress,
    wsrForce);

  /// how to implement HTTP Strict Transport Security (HSTS) redirection
  // - If a user types 'example.com' in their browser, even if the server redirects
  // them to the secure version of the website, that still leaves a window of
  // opportunity (the initial HTTP connection) for an attacker to downgrade or
  // redirect the request.
  // - The HSTS header ensures that a browser only connects to your server
  // via HTTPS, regardless of what the users type in the browser's address bar.
  // - Be aware that Strict Transport Security is not revokable and you must
  // ensure being able to serve the site over HTTPS for the duration you've
  // specified in the 'max-age' directive. When you don't have a valid TLS
  // connection anymore (e.g. due to an expired TLS certificate) your visitors
  // will see a nasty error message even when attempting to connect over HTTP.
  // - wshOff: do not provide any HSTS header
  // - wshOn: add regular 'max-age=31536000' HSTS header value
  // - wshIncludeSubDomains: add 'max-age=31536000; includeSubDomains' HSTS header
  // - wshIncludeSubDomainsPreload: add
  // 'max-age=31536000; includeSubDomains; preload' HSTS header
  // - see https://tools.ietf.org/html/rfc6797#section-6.1
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security
  // https://www.html5rocks.com/en/tutorials/security/transport-layer-security/
  // https://blogs.msdn.microsoft.com/ieinternals/2014/08/18/strict-transport-security/
  // https://hstspreload.org/
  TWebServerHsts = (
    wshOff,
    wshOn,
    wshIncludeSubDomains,
    wshIncludeSubDomainsPreload);

  /// how to implement DNS Prefetch Control
  // - DNS prefetching is a feature by which browsers proactively perform
  // domain name resolution on both links that the user may choose to follow
  // as well as URLs for items referenced by the document, including images,
  // CSS, JavaScript, and so forth.
  // - This prefetching is performed in the background, so that the DNS is
  // likely to have been resolved by the time the referenced items are needed.
  // This reduces latency when the user clicks a link.
  // - wsdPrefetchNone: do not add 'X-DNS-Prefetch-Control' header
  // - wsdPrefetchOff: turn off DNS Prefetch
  // - wsdPrefetchOn: turn on DNS Prefetch (default)
  // - see https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-DNS-Prefetch-Control
  TWebServerDpc = (
    wsdPrefetchNone,
    wsdPrefetchOff,
    wsdPrefetchOn);

  /// the known encoding/compression schemes for a Web Server
  // - THttpServerGeneric.RegisterCompress can support any kind of compression
  // using a callback function; but we define here the most used encodings in
  // current browsers, e.g. to be able to cache content or hashes at runtime
  // - wseIdentity means no compression
  // - wseGZip will use the well-known GZip encoding (via libdeflate if available)
  // - wseBrotli is reserved for future use (e.g. mORMot 2 Boilerplate project)
  TWebServerEncoding = (
    wseIdentity,
    wseGZip,
    wseBrotli);
  /// the known encoding/compression schemes for a Web Server
  TWebServerEncodings = set of TWebServerEncoding;

  /// define the security parameters for a Web Server for a given route
  // - is defined as a record so that it could be assigned and processed per URI
  // - values will be assigned by reference, so with minimal memory consumption
  // - FileTypes*[] are ordered TWordDynArray indexes to TWebServerGlobal.MimeType[]
  TWebServerLocal = record
    /// most used tuning options for a modern and safe HTTP/HTTPS Server
    Behaviors: TWebServerBehaviors;
    /// how to handle the 'www.' host prefix at the beginning of URLs
    Rewrite: TWebServerRewrite;
    /// how to implement HTTP Strict Transport Security (HSTS) redirection
    Hsts: TWebServerHsts;
    /// how to implement DNS Prefetch Control
    Dpc: TWebServerDpc;
    /// file extension ordered indexes for wsbAllowCrossOriginImages behavior
    FileTypesImage: TWordDynArray;
    /// file extension ordered indexes for wsbAllowCrossOriginFonts behavior
    FileTypesFont: TWordDynArray;
    /// file extension ordered indexes for wsbForceUtf8Charset behavior
    FileTypesRequiredCharSet: TWordDynArray;
    /// file extension ordered indexes for wsbDelegateBlocked behavior
    FileTypesBlocked: TWordDynArray;
    /// file extension ordered indexes for wsbForceGZipHeader behavior
    FileTypesForceGZipHeader: TWordDynArray;
    /// supported Referrer Policy for wsbEnableReferrerPolicy behavior
    ReferrerPolicy: RawUtf8;
    /// content types for wsbEnableReferrerPolicy behavior
    ReferrerPolicyContentTypes: TWordDynArray;
    /// headers for wsbFixMangledAcceptEncoding behavior
    MangledEncodingHeaders: TRawUtf8DynArray;
    /// values for wsbFixMangledAcceptEncoding behavior
    MangledEncodingHeaderValues: TRawUtf8DynArray;
    /// expiration for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresDefault: integer;
    /// content-types for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresContentTypes: TRawUtf8DynArray;
    /// values for wsbSetCacheMaxAge and wsbSetExpires behaviors
    ExpiresValues: TCardinalDynArray;
  end;

  /// information about a given file extension of a supported mime type
  // - as stored in TWebServerGlobal.MimeType[]
  TWebServerMimeType = record
    /// file extensions of supported mime types, excluding initial '.' character
    Extension: RawUtf8;
    /// specific mime type for wsbForceMimeType behavior
    ForceMimeType: RawUtf8;
  end;
  /// information about all known file extensions of supported mime types
  // - as stored in TWebServerGlobal.MimeType[]
  TWebServerMimeTypeDynArray = array of TWebServerMimeType;

  /// define the security parameters common to all Web Servers
  TWebServerGlobal = class(TSynPersistent)
  protected
    fMimeType: TWebServerMimeTypeDynArray;
    fMimeTypeCount: integer;
    fMimeTypes: TDynArrayHashed;
  public
    /// initialize this instance
    constructor Create; override;
    /// quickly returns the index of a MimeType[] entry from this file extension
    function FindMimeType(Extension: PUtf8Char): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// search then add if not existing of a file extension
    // - returns the index of the matching MimeType[] entry
    function AddIfNeeded(const Extension: RawUtf8): PtrInt;
    /// search (or add) a file extension, then insert its index in a
    // TWordDynArray sorted array
    // - used e.g. to fill TWebServerLocal behavior-enabled properties
    function AddToIndexes(var Indexes: TSortedWordArray;
      const Extension: RawUtf8): PtrInt;
    /// file extensions of supported mime types for wsbForceMimeType behavior
    // - see also https://github.com/jshttp/mime-db/blob/master/db.json
    // - TWebServerLocal.FileTypes*[] TWordDynArray contains indexes to this
    // - warning: length(MimeType) is the capacity - use MimeTypeCount
    property MimeType: TWebServerMimeTypeDynArray
      read fMimeType;
    /// how many file extensions are currently known
    property MimeTypeCount: integer
      read fMimeTypeCount;
  end;


implementation


{ ******************** Shared HTTP Constants and Functions }

function AuthorizationBearer(const AuthToken: RawUtf8): RawUtf8;
begin
  if AuthToken = '' then
    result := ''
  else
    result := 'Authorization: Bearer ' + AuthToken;
end;

const
  TOBEPURGED: array[0..9] of PAnsiChar = (
    'CONTENT-',
    'CONNECTION:',
    'KEEP-ALIVE:',
    'TRANSFER-',
    'X-POWERED',
    'USER-AGENT',
    'REMOTEIP:',
    'HOST:',
    'ACCEPT:',
    nil);

function PurgeHeaders(const headers: RawUtf8): RawUtf8;
var
  pos, len: array[byte] of word;
  n, purged, i, l, tot: PtrInt;
  P, next: PUtf8Char;
begin
  n := 0;
  tot := 0;
  purged := 0;
  // put all allowed headers in pos[]/len[]
  P := pointer(headers);
  if length(headers) shr 16 = 0 then // defined as word
    while P <> nil do
    begin
      if P^ = #0 then
        break;
      next := GotoNextLine(P);
      if IdemPPChar(P, @TOBEPURGED) < 0 then
      begin
        if n = high(len) then
          break;
        pos[n] := P - pointer(headers);
        l := next - P;
        if next = nil then
          if purged = 0 then
            break
          else
            l := StrLen(P);
        inc(tot, l);
        len[n] := l;
        inc(n);
      end
      else
        inc(purged);
      P := next;
    end;
  // recreate an expurgated headers set
  if purged = 0 then
    // nothing to purge
    result := headers
  else if tot = 0 then
    // genocide
    result := ''
  else
  begin
    // allocate at once and append all non-purged headers
    FastSetString(result, nil, tot);
    P := pointer(result);
    for i := 0 to n - 1 do
    begin
      MoveFast(PByteArray(headers)[{%H-}pos[i]], P^, {%H-}len[i]);
      inc(P, len[i]);
    end;
    assert(P - pointer(result) = tot);
  end;
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

procedure ExtractHeader(var headers: RawUtf8; const upname: RawUtf8;
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

function MimeHeaderEncode(const header: RawUtf8): RawUtf8;
begin
  if IsAnsiCompatible(header) then
    // US-ASCII don't need any conversion in the RFC, but only 7-bit in practice
    result := header
  else
    result := '=?UTF-8?B?' + BinToBase64(header) + '?=';
end;

function HttpMethodWithNoBody(const method: RawUtf8): boolean;
var
  c: cardinal;
begin
  c := PCardinal(method)^;
  result := (((c xor cardinal(ord('H') + ord('E') shl 8 + ord('A') shl 16 +
                     ord('D') shl 24)) and $dfdfdfdf) = 0) or
            (((c xor cardinal(ord('O') + ord('P') shl 8 + ord('T') shl 16 +
                     ord('I') shl 24)) and $dfdfdfdf) = 0);
end;

function IsGet(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ = ord('G') + ord('E') shl 8 + ord('T') shl 16;
end;

function IsPost(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ =
    ord('P') + ord('O') shl 8 + ord('S') shl 16 + ord('T') shl 24;
end;

function IsPut(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ =
    ord('P') + ord('U') shl 8 + ord('T') shl 16;
end;

function IsDelete(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ =
    ord('D') + ord('E') shl 8 + ord('L') shl 16 + ord('E') shl 24;
end;

function IsOptions(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ =
    ord('O') + ord('P') shl 8 + ord('T') shl 16 + ord('I') shl 24;
end;

function IsHead(const method: RawUtf8): boolean;
begin
  result := PCardinal(method)^ =
    ord('H') + ord('E') shl 8 + ord('A') shl 16 + ord('D') shl 24;
end;


function IsUrlFavIcon(P: PUtf8Char): boolean;
begin
  result := (P <> nil) and
        (PCardinalArray(P)[0] =
           ord('/') + ord('f') shl 8 + ord('a') shl 16 + ord('v') shl 24) and
        (PCardinalArray(P)[1] =
           ord('i') + ord('c') shl 8 + ord('o') shl 16 + ord('n') shl 24) and
        (PCardinalArray(P)[2] =
           ord('.') + ord('i') shl 8 + ord('c') shl 16 + ord('o') shl 24) and
        (P[12] = #0);
end;

function ByPriority(const A, B): integer;
begin
  result := CompareInteger(THttpSocketCompressRec(A).Priority,
                           THttpSocketCompressRec(B).Priority);
end;

function RegisterCompressFunc(var Comp: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress; var AcceptEncoding: RawUtf8;
  CompMinSize, CompPriority: integer): RawUtf8;
var
  i, n: PtrInt;
  dummy: RawByteString;
  algo: RawUtf8;
begin
  result := '';
  if @CompFunction = nil then
    exit;
  n := length(Comp);
  algo := CompFunction(dummy, {compress}true); // just retrieve algo name
  for i := 0 to n - 1 do
    with Comp[i] do
      if Name = algo then
      begin
        // already set
        if @Func = @CompFunction then
          CompressMinSize := CompMinSize; // update size parameter
        exit;
      end;
  if n = SizeOf(THttpSocketCompressSet) * 8 then
    exit; // CompressAcceptHeader has 0..31 bits
  SetLength(Comp, n + 1);
  with Comp[n] do
  begin
    Name := algo;
    @Func := @CompFunction;
    CompressMinSize := CompMinSize;
    Priority := (CompPriority shl 14) or n; // by CompPriority, then call order
  end;
  DynArray(TypeInfo(THttpSocketCompressRecDynArray), Comp).Sort(ByPriority);
  if AcceptEncoding = '' then
    AcceptEncoding := 'Accept-Encoding: ' + algo
  else
    AcceptEncoding := AcceptEncoding + ',' + algo;
  result := algo;
end;

const
  _CONTENTCOMP: array[0..3] of PUtf8Char = (
    'TEXT/',
    'IMAGE/',
    'APPLICATION/',
    nil);
  _CONTENTIMG: array[0..2] of PUtf8Char = (
    'SVG',
    'X-ICO',
    nil);
  _CONTENTAPP: array[0..4] of PUtf8Char = (
    'JSON',
    'XML',
    'JAVASCRIPT',
    'VND.API+JSON',
    nil);

procedure CompressContent(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUtf8;
  var OutContent: RawByteString; var OutContentEncoding: RawUtf8);
var
  i, OutContentLen: integer;
  compressible: boolean;
  OutContentTypeP: PUtf8Char absolute OutContentType;
begin
  if (integer(Accepted) <> 0) and
     (OutContentType <> '') and
     (Handled <> nil) then
  begin
    OutContentLen := length(OutContent);
    case IdemPPChar(OutContentTypeP, @_CONTENTCOMP) of
      0:
        compressible := true;
      1:
        compressible := IdemPPChar(OutContentTypeP + 6, @_CONTENTIMG) >= 0;
      2:
        compressible := IdemPPChar(OutContentTypeP + 12, @_CONTENTAPP) >= 0;
    else
      compressible := false;
    end;
    for i := 0 to length(Handled) - 1 do
      if i in Accepted then
        with Handled[i] do
          if (CompressMinSize = 0) or // 0 means "always" (e.g. for encryption)
             (compressible and
              (OutContentLen >= CompressMinSize)) then
          begin
            // compression of the OutContent + update header
            OutContentEncoding := Func(OutContent, true);
            exit; // first in fCompress[] is prefered
          end;
  end;
  OutContentEncoding := '';
end;

function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PUtf8Char): THttpSocketCompressSet;
var
  i, len: PtrInt;
  Beg: PUtf8Char;
begin
  integer(result) := 0;
  if P <> nil then
    repeat
      while P^ in [' ', ','] do
        inc(P);
      Beg := P; // 'gzip;q=1.0, deflate' -> Name='gzip' then 'deflate'
      while not (P^ in [';', ',', #0]) do
        inc(P);
      len := P - Beg;
      if len <> 0 then
        for i := 0 to length(Compress) - 1 do
          if IdemPropNameU(Compress[i].Name, Beg, len) then
            include(result, i);
      while not (P^ in [',', #0]) do
        inc(P);
    until P^ = #0;
end;

function CompressIndex(const Compress: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress): PtrInt;
begin
  for result := 0 to length(Compress) - 1 do
    if @Compress[result].Func = @CompFunction then
      exit;
  result := -1;
end;

function HttpChunkToHex32(p: PAnsiChar): integer;
var
  v0, v1: byte;
begin
  // note: chunk is not regular two-chars-per-byte hexa since may have odd len
  result := 0;
  if p <> nil then
  begin
    while p^ = ' ' do
      inc(p); // trim left
    repeat
      v0 := ConvertHexToBin[ord(p[0])];
      if v0 = 255 then
        break; // not in '0'..'9','a'..'f' -> trim right
      v1 := ConvertHexToBin[ord(p[1])];
      inc(p);
      if v1 = 255 then
      begin
        result := (result shl 4) or v0; // odd number of hexa chars input
        break;
      end;
      result := (result shl 8) or (integer(v0) shl 4) or v1;
      inc(p);
    until false;
  end;
end;

function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: RawUtf8): boolean;
begin
  if P <> nil then
  begin
    result := true;
    inc(P);
    repeat
      if UrlDecodeValue(P, UpperName, Value, @P) then
        exit;
    until P = nil;
  end;
  result := false;
end;

function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: cardinal): boolean;
begin
  if P <> nil then
  begin
    result := true;
    inc(P);
    repeat
      if UrlDecodeCardinal(P, UpperName, Value, @P) then
        exit;
    until P = nil;
  end;
  result := false;
end;

function UrlDecodeParam(P: PUtf8Char; const UpperName: RawUtf8;
  out Value: Int64): boolean;
begin
  if P <> nil then
  begin
    result := true;
    inc(P);
    repeat
      if UrlDecodeInt64(P, UpperName, Value, @P) then
        exit;
    until P = nil;
  end;
  result := false;
end;

function GetNextRange(var P: PUtf8Char): Qword;
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


{ ******************** Reusable HTTP State Machine }

{ THttpRequestContext }

procedure THttpRequestContext.Clear;
begin
  Head.Reset;
  Process.Reset;
  State := hrsNoStateMachine;
  HeaderFlags := [];
  ResponseFlags := [];
  Options := [];
  Headers := '';
  ContentType := '';
  Upgrade := '';
  BearerToken := '';
  UserAgent := '';
  Referer := '';
  RangeOffset := 0;
  RangeLength := -1;
  Content := '';
  ContentLength := -1;
  ServerInternalState := 0;
  CompressContentEncoding := -1;
  integer(CompressAcceptHeader) := 0;
end;

procedure THttpRequestContext.GetTrimmed(P, P2: PUtf8Char; L: PtrInt;
  var result: RawUtf8; nointern: boolean);
begin
  while (P^ > #0) and
        (P^ <= ' ') do
    inc(P); // trim left
  if L >= 0 then
    dec(L, P - P2)
  else
    L := StrLen(P);
  repeat
    if (L = 0) or
       (P[L - 1] > ' ') then
      break;
    dec(L); // trim right
  until false;
  SetRawUtf8(result, P, L, nointern);
end;

function THttpRequestContext.ValidateRange: boolean;
var
  tosend: Int64;
begin
  if RangeOffset >= ContentLength then
    result := false // invalid offset: return error or void response
  else
  begin
    tosend := RangeLength;
    if (tosend < 0) or // -1 for end of file 'Range: 1024-'
       (RangeOffset + tosend > ContentLength) then
      tosend := ContentLength - RangeOffset; // truncate
    RangeLength := ContentLength; // contains size for Content-Range: header
    ContentLength := tosend;
    include(ResponseFlags, rfRange);
    result := true;
  end;
end;

procedure THttpRequestContext.ParseHeader(P: PUtf8Char; PLen: PtrInt;
  HeadersUnFiltered: boolean);
var
  i, len: PtrInt;
  P1, P2: PUtf8Char;
begin
  if P = nil then
    exit; // avoid unexpected GPF in case of wrong usage
  P2 := P;
  // standard headers are expected to be pure A-Z chars: fast lowercase search
  // - or $20 makes conversion to a-z lowercase, but won't affect - / : chars
  // - the worse case may be some false positive, which won't hurt unless
  // your network architecture suffers from HTTP request smuggling
  // - much less readable than cascaded IdemPPChar(), but slightly faster ;)
  case PCardinal(P)^ or $20202020 of
    ord('c') + ord('o') shl 8 + ord('n') shl 16 + ord('t') shl 24:
      if PCardinal(P + 4)^ or $20202020 =
        ord('e') + ord('n') shl 8 + ord('t') shl 16 + ord('-') shl 24 then
        // 'CONTENT-'
        case PCardinal(P + 8)^ or $20202020 of
          ord('l') + ord('e') shl 8 + ord('n') shl 16 + ord('g') shl 24:
            if PCardinal(P + 12)^ or $20202020 =
              ord('t') + ord('h') shl 8 + ord(':') shl 16 + ord(' ') shl 24 then
            begin
              // 'CONTENT-LENGTH:'
              ContentLength := GetInt64(P + 16);
              if not HeadersUnFiltered then
                exit;
            end;
          ord('t') + ord('y') shl 8 + ord('p') shl 16 + ord('e') shl 24:
            if P[12] = ':' then
            begin
              // 'CONTENT-TYPE:'
              P := GotoNextNotSpace(P + 13);
              if (PCardinal(P)^ or $20202020 =
                ord('a') + ord('p') shl 8 + ord('p') shl 16 + ord('l') shl 24) and
                 (PCardinal(P + 11)^ or $20202020 =
                ord('/') + ord('j') shl 8 + ord('s') shl 16 + ord('o') shl 24) then
              begin
                // 'APPLICATION/JSON'
                ContentType := JSON_CONTENT_TYPE_VAR;
                if not HeadersUnFiltered then
                  exit; // '' in headers means JSON for our REST server
              end
              else
              begin
                GetTrimmed(P, P2, PLen, ContentType);
                if ContentType = '' then
                  // 'CONTENT-TYPE:' is searched by HEADER_CONTENT_TYPE_UPPER
                  exit;
              end;
            end;
          ord('e') + ord('n') shl 8 + ord('c') shl 16 + ord('o') shl 24:
            if (Compress <> nil) and
               (PCardinal(P + 12)^ or $20202020 =
                ord('d') + ord('i') shl 8 + ord('n') shl 16 + ord('g') shl 24) and
               (P[16] = ':') then
            begin
              // 'CONTENT-ENCODING:'
              P := GotoNextNotSpace(P + 17);
              P1 := P;
              while P^ > ' ' do
                inc(P); // no control char should appear in any header
              len := P - P1;
              if len <> 0 then
                for i := 0 to length(Compress) - 1 do
                  if IdemPropNameU(Compress[i].Name, P1, len) then
                  begin
                    CompressContentEncoding := i; // will handle e.g. gzip
                    if not HeadersUnFiltered then
                      exit;
                    break;
                  end;
            end;
        end;
    ord('h') + ord('o') shl 8 + ord('s') shl 16 + ord('t') shl 24:
      if P[4] = ':' then
      begin
        // 'HOST:'
        inc(P, 5);
        while (P^ > #0) and
              (P^ <= ' ') do
          inc(P); // trim left
        if (LastHost <> '') and
           (StrComp(pointer(P), pointer(LastHost)) = 0) then
          Host := LastHost // optimistic approach
        else
        begin
          GetTrimmed(P, P2, PLen, Host);
          if LastHost = '' then
            LastHost := Host; // thread-safe cache for next reused call
        end;
        // always add to headers - 'host:' sometimes parsed directly
      end;
    ord('c') + ord('o') shl 8 + ord('n') shl 16 + ord('n') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
          ord('e') + ord('c') shl 8 + ord('t') shl 16 + ord('i') shl 24) and
        (PCardinal(P + 8)^ or $20202020 =
          ord('o') + ord('n') shl 8 + ord(':') shl 16 + ord(' ') shl 24) then
      begin
        // 'CONNECTION: '
        inc(P, 12);
        case PCardinal(P)^ or $20202020 of
          ord('c') + ord('l') shl 8 + ord('o') shl 16 + ord('s') shl 24:
            begin
              // 'CONNECTION: CLOSE'
              include(HeaderFlags, hfConnectionClose);
              if not HeadersUnFiltered then
                exit;
            end;
          ord('u') + ord('p') shl 8 + ord('g') shl 16 + ord('r') shl 24:
            begin
              // 'CONNECTION: UPGRADE'
              include(HeaderFlags, hfConnectionUpgrade);
              if not HeadersUnFiltered then
                exit;
            end;
          ord('k') + ord('e') shl 8 + ord('e') shl 16 + ord('p') shl 24:
            if (PCardinal(P + 4)^ or $20202020 =
                ord('-') + ord('a') shl 8 + ord('l') shl 16 + ord('i') shl 24) and
               (PWord(P + 8)^ or $2020 = ord('v') + ord('e') shl 8) then
            begin
              // 'CONNECTION: KEEP-ALIVE'
              include(HeaderFlags, hfConnectionKeepAlive);
              inc(P, 10);
              if P^ = ',' then
              begin
                repeat
                  inc(P);
                until P^ <> ' ';
                if PCardinal(P)^ or $20202020 =
                  ord('u') + ord('p') shl 8 + ord('g') shl 16 + ord('r') shl 24 then
                  // 'CONNECTION: KEEP-ALIVE, UPGRADE'
                  include(HeaderFlags, hfConnectionUpgrade);
              end;
              if not HeadersUnFiltered then
                exit;
            end;
        end;
      end;
    ord('a') + ord('c') shl 8 + ord('c') shl 16 + ord('e') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('p') + ord('t') shl 8 + ord('-') shl 16 + ord('e') shl 24) and
         (PCardinal(P + 8)^ or $20202020 =
        ord('n') + ord('c') shl 8 + ord('o') shl 16 + ord('d') shl 24) and
         (PCardinal(P + 12)^ or $20202020 =
        ord('i') + ord('n') shl 8 + ord('g') shl 16 + ord(':') shl 24) then
        begin
           // 'ACCEPT-ENCODING:'
          GetTrimmed(P + 17, P2, PLen, AcceptEncoding);
          if not HeadersUnFiltered then
            exit;
        end;
    ord('u') + ord('s') shl 8 + ord('e') shl 16 + ord('r') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('-') + ord('a') shl 8 + ord('g') shl 16 + ord('e') shl 24) and
         (PCardinal(P + 8)^ or $20202020 =
        ord('n') + ord('t') shl 8 + ord(':') shl 16 + ord(' ') shl 24) then
      begin
        // 'USER-AGENT:'
        GetTrimmed(P + 11, P2, PLen, UserAgent);
        if not HeadersUnFiltered then
          exit;
      end;
    ord('s') + ord('e') shl 8 + ord('r') shl 16 + ord('v') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('e') + ord('r') shl 8 + ord('-') shl 16 + ord('i') shl 24) and
         (PCardinal(P + 8)^ or $20202020 =
        ord('n') + ord('t') shl 8 + ord('e') shl 16 + ord('r') shl 24) and
         (PCardinal(P + 12)^ or $20202020 =
        ord('n') + ord('a') shl 8 + ord('l') shl 16 + ord('s') shl 24) and
         (PCardinal(P + 16)^ or $20202020 =
        ord('t') + ord('a') shl 8 + ord('t') shl 16 + ord('e') shl 24) and
         (P[20] = ':') then
      begin
        // 'SERVER-INTERNALSTATE:'
        inc(P, 21);
        ServerInternalState := GetCardinal(P);
        if not HeadersUnFiltered then
          exit;
      end;
    ord('e') + ord('x') shl 8 + ord('p') shl 16 + ord('e') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('c') + ord('t') shl 8 + ord(':') shl 16 + ord(' ') shl 24) and
         (PCardinal(P + 8)^ =
        ord('1') + ord('0') shl 8 + ord('0') shl 16 + ord('-') shl 24) then
      begin
        // 'Expect: 100-continue'
        include(HeaderFlags, hfExpect100);
        if not HeadersUnFiltered then
          exit;
      end;
    ord('a') + ord('u') shl 8 + ord('t') shl 16 + ord('h') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('o') + ord('r') shl 8 + ord('i') shl 16 + ord('z') shl 24) and
         (PCardinal(P + 8)^ or $20202020 =
        ord('a') + ord('t') shl 8 + ord('i') shl 16 + ord('o') shl 24) then
      begin
        include(HeaderFlags, hfHasAuthorization);
        if (PCardinal(P + 12)^ or $20202020 =
          ord('n') + ord(':') shl 8 + ord(' ') shl 16 + ord('b') shl 24) and
           (PCardinal(P + 16)^ or $20202020 =
          ord('e') + ord('a') shl 8 + ord('r') shl 16 + ord('e') shl 24) and
           (PWord(P + 20)^ or $2020 = ord('r') + ord(' ') shl 8) then
          // 'AUTHORIZATION: BEARER '
          GetTrimmed(P + 22, P2, PLen, BearerToken, {nointern=}true);
        // always allow FindNameValue(..., HEADER_BEARER_UPPER, ...) search
      end;
    ord('r') + ord('a') shl 8 + ord('n') shl 16 + ord('g') shl 24:
      if (PCardinal(P + 4)^ or $20202020 =
        ord('e') + ord(':') shl 8 + ord(' ') shl 16 + ord('b') shl 24) and
         (PCardinal(P + 8)^ or $20202020 =
        ord('y') + ord('t') shl 8 + ord('e') shl 16 + ord('s') shl 24) and
         (P[12] = '=') then
        if rfWantRange in ResponseFlags then
          State := hrsErrorUnsupportedRange // no multipart range
        else
        begin
          // 'RANGE: BYTES='
          P1 := GotoNextNotSpace(P + 13); // use pointer on stack
          RangeOffset := GetNextRange(P1);
          if P1^ = '-' then
          begin
            inc(P1);
            if P1^ in ['0'..'9'] then
            begin
              // "Range: bytes=0-499" -> start=0, len=500
              RangeLength := Int64(GetNextRange(P1)) - RangeOffset + 1;
              if RangeLength < 0 then
                RangeLength := 0;
            end;
            // "bytes=1000-" -> start=1000, keep RangeLength=-1 to eof
            if P1^ = ',' then
              State := hrsErrorUnsupportedRange // no multipart range
            else
              include(ResponseFlags, rfWantRange);
           end
          else
            State := hrsErrorUnsupportedRange;
          if not HeadersUnFiltered then
            exit;
        end;
    ord('u') + ord('p') shl 8 + ord('g') shl 16 + ord('r') shl 24:
      if PCardinal(P + 4)^ or $00202020 =
        ord('a') + ord('d') shl 8 + ord('e') shl 16 + ord(':') shl 24 then
      begin
        // 'UPGRADE:'
        GetTrimmed(P + 8, P2, PLen, Upgrade);
        if not HeadersUnFiltered then
          exit;
      end;
    ord('r') + ord('e') shl 8 + ord('f') shl 16 + ord('e') shl 24:
      if PCardinal(P + 4)^ or $00202020 =
        ord('r') + ord('e') shl 8 + ord('r') shl 16 + ord(':') shl 24 then
      begin
        // 'REFERER:'
        GetTrimmed(P + 8, P2, PLen, Referer, {nointern=}true);
        if not HeadersUnFiltered then
          exit;
      end;
    ord('t') + ord('r') shl 8 + ord('a') shl 16 + ord('n') shl 24:
      if IdemPChar(P + 4, 'SFER-ENCODING: CHUNKED') then
      begin
        // 'TRANSFER-ENCODING: CHUNKED'
        include(HeaderFlags, hfTransferChunked);
        if not HeadersUnFiltered then
          exit;
      end;
  end;
  // store meaningful headers into WorkBuffer, if not already there
  if PLen < 0 then
    PLen := StrLen(P2);
  Head.Append(P2, PLen);
  Head.AppendCRLF;
end;

function THttpRequestContext.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  FindNameValue(Headers, pointer(aUpperName), result{%H-}, false, ':');
end;

function THttpRequestContext.HeaderHasValue(const aUpperName: RawUtf8): boolean;
begin
  result := FindNameValue(pointer(Headers), pointer(aUpperName)) <> nil;
end;

procedure THttpRequestContext.ParseHeaderFinalize;
begin
  if nfHeadersParsed in HeaderFlags then
    exit;
  include(HeaderFlags, nfHeadersParsed);
  Head.AsText(Headers, {ForRemoteIP=}40, {usemainbuffer=}Interning <> nil);
  Head.Reset;
  if Compress <> nil then
    if AcceptEncoding <> '' then
      CompressAcceptHeader :=
        ComputeContentEncoding(Compress, pointer(AcceptEncoding));
end;

var
  _GETVAR, _POSTVAR, _HEADVAR: RawUtf8;

function THttpRequestContext.ParseCommand: boolean;
var
  P, B: PUtf8Char;
  L: PtrInt;
begin
  result := false;
  if nfHeadersParsed in HeaderFlags then
    exit;
  P := pointer(CommandUri);
  if P = nil then
    exit;
  case PCardinal(P)^ of
    ord('G') + ord('E') shl 8 + ord('T') shl 16 + ord(' ') shl 24:
      begin
        CommandMethod := _GETVAR; // optimistic
        inc(P, 4);
      end;
    ord('P') + ord('O') shl 8 + ord('S') shl 16 + ord('T') shl 24:
      begin
        CommandMethod := _POSTVAR;
        inc(P, 5);
      end;
    ord('H') + ord('E') shl 8 + ord('A') shl 16 + ord('D') shl 24:
      begin
        CommandMethod := _HEADVAR; // allow quick 'HEAD' search per pointer
        inc(P, 5);
      end;
  else
    begin
      B := P;
      while true do
        if P^ = ' ' then
          break
        else if P^ = #0 then
          exit
        else
          inc(P);
      L := P - B;
      if L > 10 then
        exit; // clearly invalid input (method name should be short)
      SetRawUtf8(CommandMethod, B, L, {nointern=}false);
      inc(P);
    end;
  end;
  B := P;
  while true do
    if P^ = ' ' then
      break
    else if P^ = #0 then
      exit
    else
      inc(P);
  L := P - B;
  MoveFast(B^, pointer(CommandUri)^, L); // in-place extract URI from Command
  FakeLength(CommandUri, L);
  if (PCardinal(P + 1)^ <>
       ord('H') + ord('T') shl 8 + ord('T') shl 16 + ord('P') shl 24) or
     (PCardinal(P + 5)^ and $ffffff <>
       ord('/') + ord('1') shl 8 + ord('.') shl 16) then
    exit;
  if P[8] <> '1' then
    include(ResponseFlags, rfHttp10);
  if not (hfConnectionClose in HeaderFlags) then
    if not (hfConnectionKeepAlive in HeaderFlags) and // allow HTTP1.0+keepalive
       (rfHttp10 in ResponseFlags) then // HTTP/1.1 is keep-alive by default
      include(HeaderFlags, hfConnectionClose); // standard HTTP/1.0
  result := true;
end;

procedure THttpRequestContext.UncompressData;
begin
  if cardinal(CompressContentEncoding) < cardinal(length(Compress)) then
  begin
    if Compress[CompressContentEncoding].Func(Content, false) = '' then
      // invalid content
      raise EHttpSocket.CreateUtf8('% UncompressData failed',
        [Compress[CompressContentEncoding].Name]);
    ContentLength := length(Content); // uncompressed Content-Length
  end;
end;

procedure THttpRequestContext.ProcessInit(InStream: TStream);
begin
  Clear;
  ContentStream := InStream;
  ContentLeft := 0;
  State := hrsGetCommand;
end;

procedure THttpRequestContext.SetRawUtf8(var res: RawUtf8;
  P: pointer; PLen: PtrInt; nointern: boolean);
begin
  if (Interning <> nil) and
     (PLen < 256) and
     not nointern then
    Interning^.UniqueFromBuffer(res, P, PLen, InterningHasher(0, P, PLen))
  else
    FastSetString(res, P, PLen);
end;

function THttpRequestContext.ProcessParseLine(var st: TProcessParseLine): boolean;
var
  Len: PtrInt;
  P: PUtf8Char;
begin
  Len := ByteScanIndex(pointer(st.P), st.Len, 13); // fast SSE2 or FPC IndexByte
  if PtrUInt(Len) < PtrUInt(st.Len) then // we just ignore the following #10
  begin
    P := st.P;
    st.Line := P;
    P[Len] := #0; // replace ending CRLF by #0
    st.LineLen := Len;
    inc(Len, 2);  // if 2nd char is not #10, parsing will fail as expected
    inc(st.P, Len);
    dec(st.Len, Len);
    result := true;
    // now we have the next full line in st.Line/st.LineLen
  end
  else
    result := false; // not enough input
end;

function THttpRequestContext.ProcessRead(var st: TProcessParseLine): boolean;
var
  previous: THttpRequestState;
begin
  result := false; // not enough input
  if st.Len = 0 then
    exit;
  previous := State;
  repeat
    case State of
      hrsGetCommand:
        if ProcessParseLine(st) then
        begin
          if Interning = nil then
            FastSetString(CommandUri, st.Line, st.LineLen)
          else
          begin
            // no real interning, but CommandUriInstance buffer reuse
            if st.LineLen > CommandUriInstanceLen then
            begin
              CommandUriInstanceLen := st.LineLen + 256;
              FastSetString(CommandUriInstance, nil, CommandUriInstanceLen);
            end;
            CommandUri := CommandUriInstance; // COW memory buffer reuse
            MoveFast(st.Line^, pointer(CommandUri)^, st.LineLen);
            FakeLength(CommandUri, st.LineLen);
          end;
          State := hrsGetHeaders;
        end
        else
          exit; // not enough input
      hrsGetHeaders:
        if ProcessParseLine(st) then
          if st.LineLen <> 0 then
            // Headers continue as long as text lines appear
            ParseHeader(st.Line, st.LineLen, hroHeadersUnfiltered in Options)
          else
            // void line: we reached end of headers
            if hfTransferChunked in HeaderFlags then
              // process chunked body
              State := hrsGetBodyChunkedHexFirst
            else if ContentLength > 0 then
              // regular process with explicit content-length
              State := hrsGetBodyContentLength
              // note: old HTTP/1.0 format with no Content-Length is unsupported
              // because officially not defined in HTTP/1.1 RFC2616 4.3
            else
              // no body
              State := hrsWaitProcessing
        else
          exit;
      hrsGetBodyChunkedHexFirst,
      hrsGetBodyChunkedHexNext:
        if ProcessParseLine(st) then
        begin
          ContentLeft := HttpChunkToHex32(PAnsiChar(st.Line));
          if ContentLeft <> 0 then
          begin
            if ContentStream = nil then
            begin
              // reserve appended chunk size to Content memory buffer
              SetLength(Content, length(Content) + ContentLeft);
              ContentPos := @PByteArray(Content)[length(Content)];
            end;
            inc(ContentLength, ContentLeft);
            State := hrsGetBodyChunkedData;
          end
          else
            State := hrsGetBodyChunkedDataLastLine;
        end
        else
          exit;
      hrsGetBodyChunkedData:
        begin
          if st.Len < ContentLeft then
            st.LineLen := st.Len
          else
            st.LineLen := ContentLeft;
          if ContentStream <> nil then
            ContentStream.WriteBuffer(st.P^, st.LineLen)
          else
          begin
            MoveFast(st.P^, ContentPos^, st.LineLen);
            inc(ContentPos, st.LineLen);
          end;
          dec(ContentLeft, st.LineLen);
          if ContentLeft = 0 then
            State := hrsGetBodyChunkedDataVoidLine
          else
            exit;
        end;
      hrsGetBodyChunkedDataVoidLine:
        if ProcessParseLine(st) then // chunks end with a void line
          State := hrsGetBodyChunkedHexNext
        else
          exit;
      hrsGetBodyChunkedDataLastLine:
        if ProcessParseLine(st) then // last chunk
          if st.Len <> 0 then
            State := hrsErrorUnsupportedFormat // should be no further input
          else
            State := hrsWaitProcessing
        else
          exit;
      hrsGetBodyContentLength:
        begin
          if ContentLeft = 0 then
            ContentLeft := ContentLength;
          if st.Len < ContentLeft then
            st.LineLen := st.Len
          else
            st.LineLen := ContentLeft;
          if ContentStream = nil then
          begin
            if Content = '' then // we need to allocate the result memory buffer
            begin
              if ContentLength > 1 shl 30 then // 1 GB mem chunk is fair enough
              begin
                State := hrsErrorPayloadTooLarge; // avoid memory overflow
                result := true;
                exit;
              end;
              SetLength(Content, ContentLength);
              ContentPos := pointer(Content);
            end;
            MoveFast(st.P^, ContentPos^, st.LineLen);
            inc(ContentPos, st.LineLen);
          end
          else
            ContentStream.WriteBuffer(st.P^, st.LineLen);
          dec(st.Len, st.LineLen);
          dec(ContentLeft, st.LineLen);
          if ContentLeft = 0 then
            if st.Len <> 0 then
              State := hrsErrorUnsupportedFormat // should be no further input
            else
              State := hrsWaitProcessing
          else
            exit;
        end;
    else
      State := hrsErrorMisuse; // out of context State for input
    end;
  until (State <> previous) and
        ((State = hrsGetBodyChunkedHexFirst) or
         (State = hrsGetBodyContentLength) or
         (State >= hrsWaitProcessing));
  result := true; // notify the next main state change
end;

function THttpRequestContext.CompressContentAndFinalizeHead(
  MaxSizeAtOnce: integer): PRawByteStringBuffer;
begin
  // same logic than THttpSocket.CompressDataAndWriteHeaders below
  if (integer(CompressAcceptHeader) <> 0) and
     (ContentStream = nil) then // no stream compression (yet)
    CompressContent(CompressAcceptHeader, Compress, ContentType,
      Content, ContentEncoding);
  // method will return a buffer to be sent
  result := @Head;
  // handle response body with optional range support
  if rfAcceptRange in ResponseFlags then
    result^.AppendShort('Accept-Ranges: bytes'#13#10);
  if ContentStream = nil then
  begin
    ContentPos := pointer(Content); // for ProcessBody below
    ContentLength := length(Content);
    if rfWantRange in ResponseFlags then
      if not (rfRange in ResponseFlags) then // not already from ContentFromFile
        if ValidateRange then
          inc(ContentPos, RangeOffset) // rfRange has just been set
        else
          ContentLength := 0; // invalid range: return void response
    // ContentStream<>nil did set ContentLength/rfRange in ContentFromFile
  end;
  if rfRange in ResponseFlags then
  begin
    // Content-Range: bytes 0-1023/146515
    result^.AppendShort('Content-Range: bytes ');
    result^.Append(RangeOffset);
    result^.Append('-');
    result^.Append(RangeOffset + ContentLength - 1);
    result^.Append('/');
    result^.Append(RangeLength); // = FileSize after ContentFromFile()
    result^.AppendCRLF;
  end;
  // finalize headers
  if ContentEncoding <> '' then
  begin
    result^.AppendShort('Content-Encoding: ');
    result^.Append(ContentEncoding);
    result^.AppendCRLF;
  end;
  result^.AppendShort('Content-Length: ');
  result^.Append(ContentLength);
  result^.AppendCRLF;
  if (ContentType <> '') and
     (ContentType[1] <> '!') then
  begin
    result^.AppendShort('Content-Type: ');
    result^.Append(ContentType);
    result^.AppendCRLF;
  end;
  if hfConnectionClose in HeaderFlags then
    result^.AppendShort('Connection: Close'#13#10#13#10) // end with a void line
  else
  begin
    if rfHttp10 in ResponseFlags then // implicit with HTTP/1.1
      result^.AppendShort('Connection: Keep-Alive'#13#10);
    if CompressAcceptEncoding <> '' then
    begin
      result^.Append(CompressAcceptEncoding);
      result^.AppendCRLF;
    end;
    result^.AppendCRLF;
  end;
  // try to send both headers and body in a single socket syscal
  Process.Reset;
  if pointer(CommandMethod) = pointer(_HEADVAR) then
    // return only the headers
    State := hrsResponseDone
  else
    // there is a body to send
    if ContentStream = nil then
      if (ContentLength = 0) or
         result^.TryAppend(ContentPos, ContentLength) then
        // single socket send() is possible (small body appended to headers)
        State := hrsResponseDone
      else
      begin
        if ContentLength + Head.Len < MaxSizeAtOnce then
        begin
          // single socket send() is possible (body fits in the sending buffer)
          Process.Reserve(Head.Len + ContentLength);
          Process.Append(Head.Buffer, Head.Len);
          Process.Append(ContentPos, ContentLength);
          Content := ''; // release ASAP
          Head.Reset;
          result := @Process; // DoRequest will use Process
          State := hrsResponseDone;
        end
        else
          // async huge body sent using Write polling
          State := hrsSendBody;
      end
    else
      // ContentStream <> nil requires async body sending
      State := hrsSendBody; // send the ContentStream out by chunks
end;

procedure THttpRequestContext.ProcessBody(
  var Dest: TRawByteStringBuffer; MaxSize: PtrInt);
var
  P: pointer;
begin
  // THttpAsyncConnection.DoRequest did send the headers: now send body chunks
  if State <> hrsSendBody then
    exit;
  // send in the background, using polling up to MaxSize (256KB typical)
  if ContentLength < MaxSize then
    MaxSize := ContentLength;
  if MaxSize > 0 then
  begin
    if ContentStream <> nil then
    begin
      P := Process.Reserve(MaxSize);
      MaxSize := ContentStream.Read(P^, MaxSize);
      Dest.Append(P, MaxSize);
    end
    else
    begin
      Dest.Append(ContentPos, MaxSize);
      inc(ContentPos, MaxSize);
    end;
    dec(ContentLength, MaxSize);
  end
  else if ContentLength = 0 then
    // we just finished background ProcessWrite of the last chunk
    State := hrsResponseDone
  else
    // paranoid check
    raise EHttpSocket.CreateUtf8('ProcessWrite: len=%', [MaxSize]);
end;

procedure THttpRequestContext.ProcessDone;
begin
  if rfContentStreamNeedFree in ResponseFlags then
    FreeAndNilSafe(ContentStream);
end;

function THttpRequestContext.ContentFromFile(
  const FileName: TFileName; CompressGz: integer): boolean;
var
  gz: TFileName;
begin
  Content := '';
  if (CompressGz >= 0) and
     (CompressGz in CompressAcceptHeader) and
     (pointer(CommandMethod) <> pointer(_HEADVAR)) and
     not (rfWantRange in ResponseFlags) then
  begin
    // try locally cached gzipped static content
    gz := FileName + '.gz';
    ContentLength := FileSize(gz);
    if ContentLength > 0 then
    begin
      // there is an already-compressed .gz file to send away
      ContentStream := TFileStreamEx.Create(gz, fmOpenReadDenyNone);
      ContentEncoding := 'gzip';
      include(ResponseFlags, rfContentStreamNeedFree);
      result := true;
      exit; // only use ContentStream to bypass recompression
    end;
  end;
  ContentLength := FileSize(FileName);
  result := ContentLength <> 0;
  if result and
     (rfWantRange in ResponseFlags) then
    if not ValidateRange then
      result := false; // invalid offset
  if not result then
    // there is no such file available, or range clearly wrong
    exit;
  include(ResponseFlags, rfAcceptRange);
  ContentStream := TFileStreamEx.Create(FileName, fmOpenReadDenyNone);
  if RangeOffset <> 0 then
    ContentStream.Seek(RangeOffset, soBeginning);
  if (ContentLength < 1 shl 20) and
     (pointer(CommandMethod) <> pointer(_HEADVAR)) then
  begin
    // smallest files (up to 1MB) in temp memory (and maybe compress them)
    SetLength(Content, ContentLength);
    ContentStream.Read(pointer(Content)^, ContentLength);
    FreeAndNilSafe(ContentStream);
  end
  else
  begin
    // stream existing big file by chunks (also used for HEAD or Range)
    include(ResponseFlags, rfContentStreamNeedFree);
  end;
end;


function ToText(st: THttpRequestState): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpRequestState), ord(st));
end;

function ToText(csp: TCrtSocketPending): PShortString;
begin
  result := GetEnumName(TypeInfo(TCrtSocketPending), ord(csp));
end;

function ToText(tls: TCrtSocketTlsAfter): PShortString;
begin
  result := GetEnumName(TypeInfo(TCrtSocketTlsAfter), ord(tls));
end;

function ToText(mak: TMacAddressKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TMacAddressKind), ord(mak));
end;


{ ******************** THttpSocket Implementing HTTP over plain sockets }

{ THttpSocket }

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: RawUtf8;
  var OutContent: RawByteString; OutStream: TStream);
var
  OutContentEncoding: RawUtf8;
  len: Int64;
begin
  if (integer(Http.CompressAcceptHeader) <> 0) and
     (OutStream = nil) then // no stream compression (yet)
  begin
    CompressContent(Http.CompressAcceptHeader, Http.Compress, OutContentType,
      OutContent, OutContentEncoding);
    if OutContentEncoding <> '' then
      SockSend(['Content-Encoding: ', OutContentEncoding]);
  end;
  if OutStream = nil then
    len := length(OutContent)
  else
    len := OutStream.Size;
  SockSend(['Content-Length: ', len]); // needed even 0
  if (OutContentType <> '') and
     (OutContentType[1] <> '!') then
    SockSend(['Content-Type: ', OutContentType]);
end;

procedure THttpSocket.HttpStateReset;
begin
  Http.Clear;
  fBodyRetrieved := false;
end;

const
  _FLAGS: PAnsiChar = 'ptcuk1if';

function ToText(hf: THttpRequestHeaderFlags): TShort8;
var
  b: cardinal;
  P, R: PAnsiChar;
  L: PtrInt;
begin
  b := byte(hf);
  L := 0;
  P := _FLAGS;
  R := @result;
  repeat
    if b and 1 <> 0 then
    begin
      inc(L);
      R[L] := P^;
    end;
    inc(P);
    b := b shr 1;
  until b = 0;
  R[0] := AnsiChar(L);
end;

function THttpSocket.GetHeader(HeadersUnFiltered: boolean): boolean;
var
  s: RawUtf8;
  err: integer;
  line: array[0..4095] of AnsiChar; // avoid most memory allocations
begin
  // parse the headers
  result := false;
  HttpStateReset;
  if SockIn <> nil then
    repeat
      {$I-}
      readln(SockIn^, line);
      err := ioresult;
      if err <> 0 then
        raise EHttpSocket.CreateUtf8('%.GetHeader error=%', [self, err]);
      {$I+}
      if line[0] = #0 then
        break; // HTTP headers end with a void line
      Http.ParseHeader(@line, {linelen=}-1, HeadersUnFiltered);
      if Http.State <> hrsNoStateMachine then
        exit; // error
    until false
  else
    repeat
      SockRecvLn(s);
      if s = '' then
        break;
      Http.ParseHeader(pointer(s), length(s), HeadersUnFiltered);
      if Http.State <> hrsNoStateMachine then
        exit; // error
    until false;
  // finalize the headers
  result := true;
  Http.ParseHeaderFinalize; // compute all meaningful headers
  if Assigned(OnLog) then
    OnLog(sllTrace, 'GetHeader % % flags=% len=% %', [Http.CommandMethod,
      Http.CommandUri, ToText(Http.HeaderFlags), Http.ContentLength,
      Http.ContentType], self);
end;

procedure THttpSocket.GetBody(DestStream: TStream);
var
  line: RawUtf8;
  chunkline: array[0..31] of AnsiChar; // 32 bits chunk length in hexa
  chunk: RawByteString;
  len32, err: integer;
  len64: Int64;
begin
  fBodyRetrieved := true;
  Http.Content := '';
  if DestStream <> nil then
    if (cardinal(Http.CompressContentEncoding) < cardinal(length(Http.Compress))) then
      raise EHttpSocket.CreateUtf8('%.GetBody(%) does not support compression',
        [self, DestStream]);
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if hfTransferChunked in Http.HeaderFlags then
  begin
    // Content-Length header should be ignored when chunked by RFC 2616 #4.4.3
    Http.ContentLength := 0;
    repeat // chunks decoding loop
      if SockIn <> nil then
      begin
        readln(SockIn^, chunkline); // use of a static PChar is faster
        err := ioresult;
        if err <> 0 then
          raise EHttpSocket.CreateUtf8('%.GetBody chunked ioresult=%', [self, err]);
        len32 := HttpChunkToHex32(chunkline); // get chunk length in hexa
      end
      else
      begin
        SockRecvLn(line);
        len32 := HttpChunkToHex32(pointer(line)); // get chunk length in hexa
      end;
      if len32 = 0 then
      begin
        SockRecvLn; // ignore next line (normally void)
        break; // reached the end of input stream
      end;
      if DestStream <> nil then
      begin
        if length({%H-}chunk) < len32 then
          SetString(chunk, nil, len32 + len32 shr 3); // +shr 3 to avoid realloc
        SockInRead(pointer(chunk), len32);
        DestStream.WriteBuffer(pointer(chunk)^, len32);
      end
      else
      begin
        SetLength(Http.Content, Http.ContentLength + len32); // reserve space for this chunk
        SockInRead(@PByteArray(Http.Content)[Http.ContentLength], len32); // append data
      end;
      inc(Http.ContentLength, len32);
      SockRecvLn; // ignore next #13#10
    until false;
  end
  else if Http.ContentLength > 0 then
    // read Content-Length header bytes
    if DestStream <> nil then
    begin
      len32 := 256 shl 10; // not chunked: use a 256 KB temp buffer
      if Http.ContentLength < len32 then
        len32 := Http.ContentLength;
      SetLength(chunk, len32);
      len64 := Http.ContentLength;
      repeat
        if len32 > len64 then
          len32 := len64;
        SockInRead(pointer(chunk), len32);
        DestStream.WriteBuffer(pointer(chunk)^, len32);
        dec(len64, len32);
      until len64 = 0;
    end
    else
    begin
      SetLength(Http.Content, Http.ContentLength); // not chuncked: direct read
      SockInRead(pointer(Http.Content), Http.ContentLength);
    end
  else if Http.ContentLength < 0 then // -1 means no Content-Length header
  begin
    // no Content-Length neither chunk -> read until the connection is closed
    // also for HTTP/1.1: https://www.rfc-editor.org/rfc/rfc7230#section-3.3.3
    if Assigned(OnLog) then
      OnLog(sllTrace, 'GetBody deprecated loop', [], self);
    // body = either Content-Length or Transfer-Encoding (HTTP/1.1 RFC2616 4.3)
    if SockIn <> nil then // client loop for compatibility with oldest servers
      while not eof(SockIn^) do
      begin
        readln(SockIn^, line);
        if Http.Content = '' then
          Http.Content := line
        else
          Http.Content := Http.Content + #13#10 + line;
      end;
    Http.ContentLength := length(Http.Content); // update Content-Length
    if DestStream <> nil then
    begin
      DestStream.WriteBuffer(pointer(Http.Content)^, Http.ContentLength);
      Http.Content := '';
    end;
    exit;
  end;
  // optionaly uncompress content
  if Http.CompressContentEncoding >= 0 then
    Http.UncompressData;
  if Assigned(OnLog) then
    OnLog(sllTrace, 'GetBody len=%', [Http.ContentLength], self);
  if SockIn <> nil then
  begin
    err := ioresult;
    if err <> 0 then
      raise EHttpSocket.CreateUtf8('%.GetBody ioresult2=%', [self, err]);
  end;
  {$I+}
end;

procedure THttpSocket.HeaderAdd(const aValue: RawUtf8);
begin
  if aValue <> '' then
    Http.Headers := NetConcat([Http.Headers, aValue, #13#10]);
end;

procedure THttpSocket.HeaderSetText(const aText: RawUtf8;
  const aForcedContentType: RawUtf8);
begin
  if aText = '' then
    Http.Headers := ''
  else if aText[length(aText) - 1] <> #10 then
    Http.Headers := aText + #13#10
  else
    Http.Headers := aText;
  if (aForcedContentType <> '') and
     (FindNameValue(pointer(aText), 'CONTENT-TYPE:') = nil) then
    Http.Headers := NetConcat([Http.Headers, 'Content-Type: ', aForcedContentType, #13#10]);
end;

procedure THttpSocket.HeadersPrepare(const aRemoteIP: RawUtf8);
begin
  if (aRemoteIP <> '') and
     not (hfHasRemoteIP in Http.HeaderFlags) then
  begin
    // Http.ParseHeaderFinalize did reserve 40 bytes for fast realloc
    Http.Headers := NetConcat([Http.Headers, 'RemoteIP: ', aRemoteIP, #13#10]);
    include(Http.HeaderFlags, hfHasRemoteIP);
  end;
end;

function THttpSocket.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  result := Http.HeaderGetValue(aUpperName);
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize, aPriority: integer): boolean;
begin
  result := RegisterCompressFunc(Http.Compress, aFunction,
    Http.CompressAcceptEncoding, aCompressMinSize, aPriority) <> '';
end;


{ ******************** Abstract Server-Side Types e.g. for Client-Server Protocol }

{ THttpServerRequestAbstract }

procedure THttpServerRequestAbstract.Prepare(
  const aUrl, aMethod, aInHeaders: RawUtf8; const aInContent: RawByteString;
  const aInContentType, aRemoteIP: RawUtf8);
begin
  // Create or Recycle() would have zeroed other fields
  fRemoteIP := aRemoteIP;
  fUrl := aUrl;
  fMethod := aMethod;
  fInHeaders := aInHeaders;
  fInContentType := aInContentType;
  fInContent := aInContent;
end;

procedure THttpServerRequestAbstract.Prepare(const aHttp: THttpRequestContext;
  const aRemoteIP: RawUtf8; aAuthorize: THttpServerRequestAuthentication);
begin
  fRemoteIP := aRemoteIP;
  fUrl := aHttp.CommandUri;
  fMethod := aHttp.CommandMethod;
  fInHeaders := aHttp.Headers;
  fInContentType := aHttp.ContentType;
  fHost := aHttp.Host;
  if hsrAuthorized in fConnectionFlags then
  begin
    // reflect the current valid "authorization:" header
    fAuthenticationStatus := aAuthorize;
    fAuthenticatedUser := aHttp.BearerToken; // set by fServer.Authorization()
  end
  else
    fAuthBearer := aHttp.BearerToken;
  fUserAgent := aHttp.UserAgent;
  fInContent := aHttp.Content;
end;

procedure THttpServerRequestAbstract.AddInHeader(AppendedHeader: RawUtf8);
begin
  TrimSelf(AppendedHeader);
  if AppendedHeader <> '' then
    if fInHeaders = '' then
      fInHeaders := AppendedHeader
    else
      fInHeaders := NetConcat([fInHeaders, #13#10, AppendedHeader]);
end;

procedure THttpServerRequestAbstract.AddOutHeader(const Values: array of const);
begin
  AppendLine(fOutCustomHeaders, Values);
end;

function THttpServerRequestAbstract.GetRouteValuePosLen(const Name: RawUtf8;
  var Value: TValuePUtf8Char): boolean;
var
  i: PtrInt;
  v: PIntegerArray;
begin
  result := false;
  Value.Text := nil;
  Value.Len := 0;
  if (self = nil) or
     (Name = '') or
     (fRouteName = nil) then
    exit;
  i := FindNonVoidRawUtf8(fRouteName, pointer(Name), length(Name),
                          PDALen(PAnsiChar(fRouteName) - _DALEN)^ + _DAOFF);
  if i < 0 then
    exit;
  v := @fRouteValuePosLen[i * 2]; // one [pos,len] pair in fUrl
  Value.Text := PUtf8Char(pointer(fUrl)) + v[0];
  Value.Len := v[1];
  result := true;
end;

function THttpServerRequestAbstract.GetRouteValue(const Name: RawUtf8): RawUtf8;
var
  v: TValuePUtf8Char;
begin
  GetRouteValuePosLen(Name, v);
  v.ToUtf8(result);
end;

function THttpServerRequestAbstract.RouteInt64(const Name: RawUtf8;
  out Value: Int64): boolean;
var
  v: TValuePUtf8Char;
begin
  result := GetRouteValuePosLen(Name, v);
  if result then
    Value := v.ToInt64;
end;

function THttpServerRequestAbstract.RouteUtf8(const Name: RawUtf8;
  out Value: RawUtf8): boolean;
var
  v: TValuePUtf8Char;
begin
  result := GetRouteValuePosLen(Name, v);
  if result then
    v.ToUtf8(Value);
end;

function THttpServerRequestAbstract.RouteEquals(
  const Name, ExpectedValue: RawUtf8): boolean;
var
  v: TValuePUtf8Char;
begin
  result := GetRouteValuePosLen(Name, v) and
            (CompareBuf(ExpectedValue, v.Text, v.Len) = 0);
end;

function THttpServerRequestAbstract.UrlParam(const UpperName: RawUtf8;
  out Value: RawUtf8): boolean;
begin
  if fUrlParamPos = nil then // may have been set by TUriTreeNode.LookupParam
    fUrlParamPos := PosChar(pointer(Url), '?');
  result := UrlDecodeParam(fUrlParamPos, UpperName, Value);
end;

function THttpServerRequestAbstract.UrlParam(const UpperName: RawUtf8;
  out Value: cardinal): boolean;
begin
  if fUrlParamPos = nil then
    fUrlParamPos := PosChar(pointer(Url), '?');
  result := UrlDecodeParam(fUrlParamPos, UpperName, Value);
end;

function THttpServerRequestAbstract.UrlParam(const UpperName: RawUtf8;
  out Value: Int64): boolean;
begin
  if fUrlParamPos = nil then
    fUrlParamPos := PosChar(pointer(Url), '?');
  result := UrlDecodeParam(fUrlParamPos, UpperName, Value);
end;

procedure THttpServerRequestAbstract.SetOutJson(const Json: RawUtf8);
begin
  fOutContent := Json;
  fOutContentType := JSON_CONTENT_TYPE_VAR;
end;

procedure THttpServerRequestAbstract.SetOutJson(const Fmt: RawUtf8;
  const Args: array of const);
begin
  FormatUtf8(Fmt, Args, RawUtf8(fOutContent));
  fOutContentType := JSON_CONTENT_TYPE_VAR;
end;

procedure THttpServerRequestAbstract.SetOutText(
  const Fmt: RawUtf8; const Args: array of const; const ContentType: RawUtf8);
begin
  FormatUtf8(Fmt, Args, RawUtf8(fOutContent));
  fOutContentType := ContentType;
end;


{ THttpAcceptBan }

constructor THttpAcceptBan.Create(
  banseconds, maxpersecond, banwhiteip: cardinal);
begin
  fMax := maxpersecond;
  SetSeconds(banseconds);
  fWhiteIP := banwhiteip;
  fSafe.Init;
end;

destructor THttpAcceptBan.Destroy;
begin
  inherited Destroy;
  fSafe.Done;
end;

procedure THttpAcceptBan.SetMax(Value: cardinal);
begin
  fSafe.Lock;
  try
    fMax := Value;
    SetIP;
  finally
    fSafe.UnLock;
  end;
end;

procedure THttpAcceptBan.SetSeconds(Value: cardinal);
var
  v: cardinal;
begin
  v := 128; // don't consume too much memory
  while (Value < v) and
        (v > 1) do
    v := v shr 1; // find closest power of two
  fSafe.Lock;
  try
    fSeconds := v;
    SetIP;
  finally
    fSafe.UnLock;
  end;
end;

procedure THttpAcceptBan.SetIP;
var
  i: PtrInt;
begin
  fCount := 0;
  fLastSec := 0;
  fIP := nil;
  if fMax = 0 then
    exit;
  SetLength(fIP, fSeconds); // fIP[secs,0]=count fIP[secs,1..fMax]=ips
  for i := 0 to fSeconds - 1 do
    SetLength(fIP[i], fMax + 1);
end;

function THttpAcceptBan.BanIP(ip4: cardinal): boolean;
var
  P: PCardinalArray;
begin
  if (self = nil) or
     (ip4 = 0) or
     (ip4 = fWhiteIP) then
   result := false
  else
  begin
    fSafe.Lock; // very quick O(1) process in the lock
    if fMax <> 0 then
      {$ifdef HASFASTTRYFINALLY}
      try
      {$else}
      begin
      {$endif HASFASTTRYFINALLY}
        P := pointer(fIP[fLastSec]); // fIP[secs,0]=count fIP[secs,1..fMax]=ips
        if P[0] < fMax then
        begin
          inc(P[0]);
          inc(fCount);
        end;
        P[P[0]] := ip4;
        inc(fTotal);
      {$ifdef HASFASTTRYFINALLY}
      finally
      {$endif HASFASTTRYFINALLY}
        fSafe.UnLock;
      end;
    result := true;
  end;
end;

function THttpAcceptBan.BanIP(const ip4: RawUtf8): boolean;
var
  c: cardinal;
begin
  result := (self <> nil) and
            NetIsIP4(pointer(ip4), @c) and
             ({%H-}c <> 0) and
             (c <> fWhiteIP) and
             BanIP(c);
end;

function THttpAcceptBan.IsBanned(const addr: TNetAddr): boolean;
var
  ip4: cardinal;
begin
  result := false;
  if (self = nil) or
     (fCount = 0) then
    exit;
  ip4 := addr.IP4;
  if (ip4 = 0) or
     (ip4 = fWhiteIP) then
    exit;
  result := IsBannedRaw(ip4);
end;

function THttpAcceptBan.IsBanned(ip4: cardinal): boolean;
begin
  result := (self <> nil) and
            (fCount <> 0) and
            (ip4 <> 0) and
            (ip4 <> fWhiteIP) and
            IsBannedRaw(ip4);
end;

function THttpAcceptBan.IsBannedRaw(ip4: cardinal): boolean;
var
  s: ^PCardinalArray;
  P: PCardinalArray;
  n: cardinal;
  cnt: PtrInt;
begin
  result := false;
  fSafe.Lock; // O(n) process, but from the main accept() thread only
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    s := pointer(fIP); // fIP[secs,0]=count fIP[secs,1..fMax]=ips
    n := fSeconds;
    if n <> 0 then
      repeat
        P := s^;
        inc(s);
        cnt := P[0];
        if (cnt <> 0) and
           IntegerScanExists(@P[1], cnt, ip4) then // O(n) SSE2 asm on Intel
        begin
          inc(fRejected);
          result := true;
          break;
        end;
        dec(n);
      until n = 0;
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    fSafe.UnLock;
  end;
end;

function THttpAcceptBan.ShouldBan(status, ip4: cardinal): boolean;
begin
  result := (self <> nil) and
            ((status = HTTP_BADREQUEST) or     // disallow 400,402..xxx
             (status > HTTP_UNAUTHORIZED)) and // allow 401 response
            BanIP(ip4)
end;

function THttpAcceptBan.ShouldBan(status: cardinal; const ip4: RawUtf8): boolean;
begin
  result := (self <> nil) and
            ((status = HTTP_BADREQUEST) or     // disallow 400,402..xxx
             (status > HTTP_UNAUTHORIZED)) and // allow 401 response
            BanIP(ip4)
end;

procedure THttpAcceptBan.DoRotate;
var
  n: PtrInt;
  p: PCardinal;
begin
  if (self = nil) or
     (fCount = 0) then
    exit;
  fSafe.Lock; // very quick O(1) process
  try
    if fCount <> 0 then
    begin
      n := fSeconds - 1;         // power of two bitmask
      n := (fLastSec + 1) and n; // per-second round robin
      fLastSec := n;
      p := @fIP[n][0]; // fIP[secs,0]=count fIP[secs,1..fMax]=ips
      dec(fCount, p^);
      p^ := 0;         // the oldest slot becomes the current (no memory move)
    end;
  finally
    fSafe.UnLock;
  end;
end;


{ TWebServerGlobal }

constructor TWebServerGlobal.Create;
begin
  inherited Create;
  fMimeTypes.InitSpecific(TypeInfo(TWebServerMimeTypeDynArray), fMimeType,
    ptPUtf8Char, @fMimeTypeCount, {caseinsensitive=}true);
  // index TWebServerMimeType.Extension as ptPUtf8Char for FindMimeType()
end;

function TWebServerGlobal.FindMimeType(Extension: PUtf8Char): PtrInt;
begin
  result := fMimeTypes.FindHashed(Extension); // search as PUtf8Char
end;

function TWebServerGlobal.AddIfNeeded(const Extension: RawUtf8): PtrInt;
var
  added: boolean;
begin
  result := -1;
  if Extension = '' then
    exit;
  result := fMimeTypes.FindHashedForAdding(Extension, added);
  if added then
    fMimeType[result].Extension := Extension;
end;

function TWebServerGlobal.AddToIndexes(var Indexes: TSortedWordArray;
  const Extension: RawUtf8): PtrInt;
begin
  result := AddIfNeeded(Extension);
  if (result >= 0) and
     (result <= high(word)) then
    Indexes.Add(result);
end;


initialization
  _GETVAR :=  'GET';
  _POSTVAR := 'POST';
  _HEADVAR := 'HEAD';

finalization

end.

