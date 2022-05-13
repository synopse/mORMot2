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
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.zip,
  mormot.core.threads,
  mormot.core.rtti,
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
function CompressContent(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUtf8;
  var OutContent: RawByteString): RawUtf8;

/// enable a give compression function for a HTTP link
function RegisterCompressFunc(var Comp: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress; var AcceptEncoding: RawUtf8;
  CompMinSize: integer): RawUtf8;

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

const
  /// pseudo-header containing the current Synopse mORMot framework version
  XPOWEREDNAME = 'X-Powered-By';

  /// the full text of the current Synopse mORMot framework version
  // - we don't supply full version number with build revision
  // (as SYNOPSE_FRAMEWORK_VERSION), to reduce potential attacker knowledge
  XPOWEREDVALUE = SYNOPSE_FRAMEWORK_NAME + ' 2 synopse.info';


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
    hrsSendBody,
    hrsResponseDone,
    hrsUpgraded,
    hrsErrorPayloadTooLarge,
    hrsErrorMisuse,
    hrsErrorUnsupportedFormat,
    hrsErrorAborted,
    hrsErrorShutdownInProgress);

  /// set of states for THttpRequestContext processing
  THttpRequestStates = set of THttpRequestState;

  /// customize THttpRequestContext process
  THttpRequestOptions = set of (
    hroHeadersUnfiltered);

  /// map the presence of some HTTP headers for THttpRequestContext.HeaderFlags
  THttpRequestHeaderFlags = set of (
    nfHeadersParsed,
    hfTransferChunked,
    hfConnectionClose,
    hfConnectionUpgrade,
    hfConnectionKeepAlive,
    hfExpect100,
    hfHasRemoteIP,
    hfContentStreamNeedFree);

  PHttpRequestContext = ^THttpRequestContext;

  /// optional callback triggered when THttpRequestContext state changes
  // - i.e. after Command, Headers or Content have been retrieved
  // - should return the current Sender.State, or an error to interrupt the
  // process (typically hrsErrorAborted)
  TOnHttpRequestStateChange = function(Previous: THttpRequestState;
     Sender: PHttpRequestContext): THttpRequestState of object;

  /// raw information used during THttpRequestContext header parsing
  TProcessParseLine = record
    P: PUtf8Char;
    Len: PtrInt;
    Line: PUtf8Char;
    LineLen: PtrInt;
  end;

  /// low-level reusable State Machine to parse and process any HTTP content
  {$ifdef USERECORDWITHMETHODS}
  THttpRequestContext = record
  {$else}
  THttpRequestContext = object
  {$endif USERECORDWITHMETHODS}
  private
    ContentLeft: Int64;
    ContentPos: PByte;
    ContentEncoding: RawUtf8;
  public
    // reusable buffers for internal process - do not use
    Head, Process: TRawByteStringBuffer;
    /// the current state of this HTTP context
    State: THttpRequestState;
    /// map the presence of some HTTP headers, but retrieved during ParseHeader
    HeaderFlags: THttpRequestHeaderFlags;
    /// customize the HTTP process
    Options: THttpRequestOptions;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: RawUtf8;
    /// the HTTP method parsed from Command, e.g. 'GET'
    CommandMethod: RawUtf8;
    /// the HTTP URI parsed from Command, e.g. '/path/to/resource'
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
    /// same as FindNameValue(aInHeaders, HEADER_BEARER_UPPER, ...),
    // but retrieved during ParseHeader
    // - is the raw Token, excluding 'Authorization: Bearer ' trailing chars
    BearerToken: RawUtf8;
    /// same as HeaderGetValue('X-POWERED-BY'), but retrieved during ParseHeader
    XPoweredBy: RawUtf8;
    /// will contain the data retrieved from the server, after all ParseHeader
    Content: RawByteString;
    /// same as HeaderGetValue('CONTENT-LENGTH'), but retrieved during ParseHeader
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: Int64;
    /// stream-oriented alternative to the Content in-memory buffer
    // - is typically a TFileStream
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
    procedure ParseHeader(P: PUtf8Char; HeadersUnFiltered: boolean = false);
    /// to be called once all ParseHeader lines have been done to fill Headers
    // - also set CompressContentEncoding/CompressAcceptHeader from Compress[]
    // and Content-Encoding header value
    procedure ParseHeaderFinalize;
    /// parse Command and Head into Headers and CommandMethod/CommandUri fields
    // - calls ParseHeaderFinalize() and server-side process the Command
    function ParseCommandAndHeader: boolean;
    /// search a value from the internal parsed Headers
    // - supplied aUpperName should be already uppercased:
    // HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - note that GetHeader(HeadersUnFiltered=false) will set ContentType field
    // but let HeaderGetValue('CONTENT-TYPE') return ''
    function HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
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
    // - then append small content (<MaxSizeAtOnce) to Head if possible, and
    // refresh the final State to hrsSendBody/hrsResponseDone
    procedure CompressContentAndFinalizeHead(MaxSizeAtOnce: integer);
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

  /// when this and following THttpRequestContext.State are fatal HTTP errors
  HTTP_REQUEST_FIRSTERROR = hrsErrorPayloadTooLarge;

function ToText(st: THttpRequestState): PShortString; overload;
function ToText(hf: THttpRequestHeaderFlags): TShort8; overload;


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
    procedure GetHeader(HeadersUnFiltered: boolean = false);
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
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer = 1024): boolean;
  end;



{ ******************** Abstract Server-Side Types used e.g. for Client-Server Protocol }

type
  {$M+} // to have existing RTTI for published properties
  THttpServerRequestAbstract = class;
  {$M-}

  /// a genuine identifier for a given client connection on server side
  // - maps http.sys ID, or is a genuine 31-bit value from increasing sequence
  THttpServerConnectionID = Int64;

  /// a dynamic array of client connection identifiers, e.g. for broadcasting
  THttpServerConnectionIDDynArray = array of THttpServerConnectionID;

  /// event handler used by THttpServerGeneric.OnRequest property
  // - Ctxt defines both input and output parameters
  // - result of the function is the HTTP error code (200 if OK, e.g.)
  // - OutCustomHeader will handle Content-Type/Location
  // - if OutContentType is STATICFILE_CONTENT_TYPE (i.e. '!STATICFILE'),
  // then OutContent is the UTF-8 filename of a file to be sent directly
  // to the client via http.sys or NGINX's X-Accel-Redirect; the
  // OutCustomHeader should contain the eventual 'Content-type: ....' value
  TOnHttpServerRequest = function(Ctxt: THttpServerRequestAbstract): cardinal of object;

  /// event handler used by THttpServerGeneric.OnAfterResponse property
  // - Ctxt defines both input and output parameters
  // - Code defines the HTTP response code the (200 if OK, e.g.)
  TOnHttpServerAfterResponse = procedure(const Method, Url, RemoteIP: RawUtf8;
    const Code: cardinal) of object;

  /// the server-side available authentication schemes
  // - as used by THttpServerRequest.AuthenticationStatus
  // - hraNone..hraKerberos will match low-level HTTP_REQUEST_AUTH_TYPE enum as
  // defined in HTTP 2.0 API and
  THttpServerRequestAuthentication = (
    hraNone,
    hraFailed,
    hraBasic,
    hraDigest,
    hraNtlm,
    hraNegotiate,
    hraKerberos);

  /// available THttpServerRequest connection attributes
  // - hsrHttps will indicates that the communication was made over HTTPS
  // - hsrSecured is set if the transmission is encrypted or in-process,
  // using e.g. HTTPS/TLS or our proprietary AES/ECDHE algorithms
  // - hsrWebsockets communication was made using WebSockets
  // - hsrInProcess is done when run from the same process, i.e. on server side
  // - should exactly match TRestUriParamsLowLevelFlag in mormot.rest.core
  THttpServerRequestFlag = (
    hsrHttps,
    hsrSecured,
    hsrWebsockets,
    hsrInProcess);

  /// the THttpServerRequest connection attributes
  THttpServerRequestFlags = set of THttpServerRequestFlag;

  /// event handler used by THttpServerGeneric.OnBeforeBody property
  // - if defined, is called just before the body is retrieved from the client
  // - supplied parameters reflect the current input state, and could be
  // modified on the fly to adapt to the expected behavior
  // - should return HTTP_SUCCESS=200 to continue the process, or an HTTP
  // error code (e.g. HTTP_FORBIDDEN or HTTP_PAYLOADTOOLARGE) to reject
  // the request
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
    fRequestID: integer;
    fConnectionID: THttpServerConnectionID;
    fConnectionFlags: THttpServerRequestFlags;
    fAuthenticationStatus: THttpServerRequestAuthentication;
    fRespStatus: integer;
    fConnectionThread: TSynThread;
  public
    /// prepare an incoming request from explicit values
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aUrl, aMethod, aInHeaders: RawUtf8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// prepare an incoming request from a parsed THttpRequestContext
    procedure Prepare(const aHttp: THttpRequestContext; const aRemoteIP: RawUtf8); overload;
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
    /// a 31-bit sequential number identifying this instance on the server
    property RequestID: integer
      read fRequestID;
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
    /// the thread which owns the connection of this execution context
    // - depending on the HTTP server used, may not follow ConnectionID
    property ConnectionThread: TSynThread
      read fConnectionThread;
    /// contains the THttpServer-side authentication status
    // - e.g. when using http.sys authentication with HTTP API 2.0
    property AuthenticationStatus: THttpServerRequestAuthentication
      read fAuthenticationStatus;
    /// contains the THttpServer-side authenticated user name, UTF-8 encoded
    // - e.g. when using http.sys authentication with HTTP API 2.0, the
    // domain user name is retrieved from the supplied AccessToken
    // - could also be set by the THttpServerGeneric.Request() method, after
    // proper authentication, so that it would be logged as expected
    property AuthenticatedUser: RawUtf8
      read fAuthenticatedUser;
  end;
  {$M-}


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
  pos: array[byte] of PUtf8Char;
  len: array[byte] of integer;
  n, purged, i, tot: PtrInt;
  P, next: PUtf8Char;
begin
  n := 0;
  tot := 0;
  purged := 0;
  // put all allowed headers in pos[]/len[]
  P := pointer(headers);
  while P <> nil do
  begin
    if P^ = #0 then
      break;
    next := GotoNextLine(P);
    if IdemPPChar(P, @TOBEPURGED) < 0 then
    begin
      if n = high(len) then
        break;
      pos[n] := P;
      if next <> nil then
        len[n] := next - P
      else if purged <> 0 then
        len[n] := StrLen(P);
      inc(tot, len[n]);
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
      MoveFast({%H-}pos[i]^, P^, {%H-}len[i]);
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

function RegisterCompressFunc(var Comp: THttpSocketCompressRecDynArray;
  CompFunction: THttpSocketCompress; var AcceptEncoding: RawUtf8;
  CompMinSize: integer): RawUtf8;
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
  end;
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
  _CONTENTAPP: array[0..3] of PUtf8Char = (
    'JSON',
    'XML',
    'JAVASCRIPT',
    nil);

function CompressContent(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUtf8;
  var OutContent: RawByteString): RawUtf8;
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
            result := Func(OutContent, true);
            exit; // first in fCompress[] is prefered
          end;
  end;
  result := '';
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

procedure GetTrimmed(P: PUtf8Char; var result: RawUtf8);
var
  B: PUtf8Char;
begin
  while (P^ > #0) and
        (P^ <= ' ') do
    inc(P); // trim left
  B := P;
  P := GotoNextControlChar(P);
  while (P > B) and
        (P[-1] <= ' ') do
    dec(P); // trim right
  FastSetString(result, B, P - B);
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
        result := (result shl 4) or v0; // odd number of hexa chars supplied
        break;
      end;
      result := (result shl 8) or (integer(v0) shl 4) or v1;
      inc(p);
    until false;
  end;
end;



{ ******************** Reusable HTTP State Machine }

{ THttpRequestContext }

procedure THttpRequestContext.Clear;
begin
  Head.Reset;
  Process.Reset;
  State := hrsNoStateMachine;
  HeaderFlags := [];
  Options := [];
  Headers := '';
  ContentType := '';
  Upgrade := '';
  BearerToken := '';
  UserAgent := '';
  XPoweredBy := '';
  Content := '';
  ContentLength := -1;
  ServerInternalState := 0;
  CompressContentEncoding := -1;
  integer(CompressAcceptHeader) := 0;
end;

const
  PARSEDHEADERS: array[0..11] of PAnsiChar = (
    'CONTENT-',                    // 0
    'TRANSFER-ENCODING: CHUNKED',  // 1
    'CONNECTION: ',                // 2
    'ACCEPT-ENCODING:',            // 3
    'UPGRADE:',                    // 4
    'SERVER-INTERNALSTATE:',       // 5
    'X-POWERED-BY:',               // 6
    'EXPECT: 100',                 // 7
    HEADER_BEARER_UPPER,           // 8
    'USER-AGENT:',                 // 9
    'HOST:',                       // 10
    nil);
  PARSEDHEADERS2: array[0..3] of PAnsiChar = (
    'LENGTH:',    // 0
    'TYPE:',      // 1
    'ENCODING:',  // 2
    nil);
  PARSEDHEADERS3: array[0..3] of PAnsiChar = (
    'CLOSE',      // 0
    'UPGRADE',    // 1
    'KEEP-ALIVE', // 2
    nil);

procedure THttpRequestContext.ParseHeader(P: PUtf8Char;
  HeadersUnFiltered: boolean);
var
  i, len: PtrInt;
  P2: PUtf8Char;
begin
  if P = nil then
    exit; // avoid unexpected GPF in case of wrong usage
  P2 := P;
  case IdemPPChar(P, @PARSEDHEADERS) of
    0:
      // 'CONTENT-'
      case IdemPPChar(P + 8, @PARSEDHEADERS2) of
        0:
          begin
            // 'CONTENT-LENGTH:'
            inc(P, 16);
            ContentLength := GetInt64(P);
          end;
        1:
          begin
            // 'CONTENT-TYPE:'
            P := GotoNextNotSpace(P + 13);
            if IdemPChar(P, 'APPLICATION/JSON') then
              ContentType := JSON_CONTENT_TYPE_VAR
            else
            begin
              GetTrimmed(P, ContentType);
              if ContentType <> '' then
                // 'CONTENT-TYPE:' is searched by HEADER_CONTENT_TYPE_UPPER
                HeadersUnFiltered := true;
            end;
          end;
        2:
          if Compress <> nil then
          begin
            // 'CONTENT-ENCODING:'
            P := GotoNextNotSpace(P + 17);
            P2 := P;
            while P^ > ' ' do
              inc(P); // no control char should appear in any header
            len := P - P2;
            if len <> 0 then
              for i := 0 to length(Compress) - 1 do
                if IdemPropNameU(Compress[i].Name, P2, len) then
                begin
                  CompressContentEncoding := i;
                  break;
                end;
          end;
      else
        HeadersUnFiltered := true;
      end;
    1:
      // 'TRANSFER-ENCODING: CHUNKED'
      include(HeaderFlags, hfTransferChunked);
    2:
      begin
        // 'CONNECTION: '
        inc(P, 12);
        case IdemPPChar(P, @PARSEDHEADERS3) of
          0:
            begin
              // 'CONNECTION: CLOSE'
              include(HeaderFlags, hfConnectionClose);
              inc(P, 5);
            end;
          1:
            // 'CONNECTION: UPGRADE'
            include(HeaderFlags, hfConnectionUpgrade);
          2:
            begin
              // 'CONNECTION: KEEP-ALIVE'
              include(HeaderFlags, hfConnectionKeepAlive);
              inc(P, 10);
              if P^ = ',' then
              begin
                P := GotoNextNotSpace(P + 1);
                if IdemPChar(P, 'UPGRADE') then
                  // 'CONNECTION: KEEP-ALIVE, UPGRADE'
                  include(HeaderFlags, hfConnectionUpgrade);
              end;
            end;
        else
          HeadersUnFiltered := true;
        end;
      end;
    3:
      begin
        // 'ACCEPT-ENCODING:'
        inc(P, 17);
        GetTrimmed(P, AcceptEncoding);
      end;
    4:
      // 'UPGRADE:'
      GetTrimmed(P + 8, Upgrade);
    5:
      begin
        // 'SERVER-INTERNALSTATE:'
        inc(P, 21);
        ServerInternalState := GetCardinal(P);
      end;
    6:
      begin
        // 'X-POWERED-BY:'
        inc(P, 13);
        GetTrimmed(P, XPoweredBy);
      end;
    7:
      // Expect: 100-continue
      include(HeaderFlags, hfExpect100);
    8:
      begin
        // 'AUTHORIZATION: BEARER '
        inc(P, 22);
        GetTrimmed(P, BearerToken);
        if BearerToken <> '' then
          // always allow FindNameValue(..., HEADER_BEARER_UPPER, ...) search
          HeadersUnFiltered := true;
      end;
    9:
      begin
        // 'USER-AGENT:'
        inc(P, 11);
        GetTrimmed(P, UserAgent);
      end;
    10:
      begin
        // 'HOST:'
        inc(P, 5);
        GetTrimmed(P, Host);
        HeadersUnFiltered := true; // may still be needed by some code
      end
  else
    // unrecognized name should be stored in Headers
    HeadersUnFiltered := true;
  end;
  if HeadersUnFiltered then
    // store meaningful headers into WorkBuffer, if not already there
    Head.Append(P2, GotoNextControlChar(P) - P2, {crlf=}true);
end;

function THttpRequestContext.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  FindNameValue(Headers, pointer(aUpperName), result, false, ':');
end;

procedure THttpRequestContext.ParseHeaderFinalize;
begin
  if nfHeadersParsed in HeaderFlags then
    exit;
  include(HeaderFlags, nfHeadersParsed);
  Head.AsText(Headers, {OverheadForRemoteIP=}40);
  Head.Reset;
  if Compress <> nil then
    if AcceptEncoding <> '' then
      CompressAcceptHeader :=
        ComputeContentEncoding(Compress, pointer(AcceptEncoding));
end;

function THttpRequestContext.ParseCommandAndHeader: boolean;
var
  P: PUtf8Char;
begin
  result := false;
  if nfHeadersParsed in HeaderFlags then
    exit;
  P := pointer(Command);
  if P = nil then
    exit;
  GetNextItem(P, ' ', CommandMethod); // GET
  GetNextItem(P, ' ', CommandUri);    // /path/to/resource
  if not IdemPChar(P, 'HTTP/1.') then
    exit;
  if not (hfConnectionClose in HeaderFlags) then
    if not (hfConnectionKeepAlive in HeaderFlags) and
       (P[7] <> '1') then
      include(HeaderFlags, hfConnectionClose);
  ParseHeaderFinalize;
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

function ProcessParseLine(var st: TProcessParseLine; line: PRawUtf8): boolean;
var
  P: PUtf8Char;
  Len: PtrInt;
begin
  P := st.P;
  Len := st.Len;
  result := false;
  if Len <= 0 then
    exit;
  // search for the CR or CRLF line end - assume no other control char appears
  dec(Len, 4);
  if Len >= 0 then
    repeat
      if P[0] > #13 then
        if P[1] > #13 then
          if P[2] > #13 then
            if P[3] > #13 then
            begin
              inc(P, 4);
              dec(Len, 4);
              if Len <= 0 then
                break;
              continue;
            end;
      break;
    until false;
  inc(Len, 4);
  // here Len=0..3
  if Len = 0 then
    exit;
  repeat
    if P[0] <= #13 then
      break;
    dec(Len);
    if Len = 0 then
      exit;
    inc(P);
  until false;
  // here P^ <= #13: we found a whole text line
  st.Line := st.P;
  st.LineLen := P - st.P;
  if line <> nil then
    FastSetString(line^, st.Line, st.LineLen);
  result := true;
  st.P := P; // will ensure below that st.line ends with #0
  // go to beginning of next line
  dec(Len);
  if (Len <> 0) and
     (PWord(P)^ = $0a0d) then
    begin
      inc(P);
      dec(Len);
    end;
  inc(P);
  st.P^ := #0;
  // prepare for parsing the next line
  st.P := P;
  st.Len := Len;
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
        if ProcessParseLine(st, @Command) then
          State := hrsGetHeaders
        else
          exit; // not enough input
      hrsGetHeaders:
        if ProcessParseLine(st, nil) then
          if st.LineLen <> 0 then
            // Headers end with a void line
            ParseHeader(st.Line, hroHeadersUnfiltered in Options)
          else
          // we reached end of headers
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
        if ProcessParseLine(st, nil) then
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
        if ProcessParseLine(st, nil) then // chunks end with a void line
          State := hrsGetBodyChunkedHexNext
        else
          exit;
      hrsGetBodyChunkedDataLastLine:
        if ProcessParseLine(st, nil) then // last chunk
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

procedure THttpRequestContext.CompressContentAndFinalizeHead(
  MaxSizeAtOnce: integer);
begin
  // same logic than THttpSocket.CompressDataAndWriteHeaders below
  if (integer(CompressAcceptHeader) <> 0) and
     (ContentStream = nil) then // no stream compression (yet)
    ContentEncoding := CompressContent(
      CompressAcceptHeader, Compress, ContentType, Content);
  if ContentEncoding <> '' then
    Head.Append(['Content-Encoding: ', ContentEncoding], {crlf=}true);
  if ContentStream = nil then
  begin
    ContentPos := pointer(Content);
    ContentLength := length(Content);
  end
  else if ContentLength = 0 then // maybe set by SetupResponse for local file
    ContentLength := ContentStream.Size - ContentStream.Position;
  Head.Append(['Content-Length: ', ContentLength], {crlf=}true);
  if (ContentType <> '') and
     (ContentType <> STATICFILE_CONTENT_TYPE) then
    Head.Append(['Content-Type: ', ContentType], {crlf=}true);
  if hfConnectionClose in HeaderFlags then
    Head.Append('Connection: Close', {crlf=}true)
  else
  begin
    if CompressAcceptEncoding <> '' then
      Head.Append(CompressAcceptEncoding, {crlf=}true);
    Head.Append('Connection: Keep-Alive', {crlf=}true);
  end;
  Head.Append(nil, 0, {crlf=}true); // headers always end with a void line
  Process.Reset;
  if ContentStream = nil then
    if ContentLength = 0 then
      // single socket send() is possible (no output body)
      State := hrsResponseDone
    else if Head.CanAppend(ContentLength) then
    begin
      // single socket send() is possible (small body appended to headers)
      Head.Append(Content);
      Content := '';
      State := hrsResponseDone;
    end
    else
    begin
      if ContentLength + Head.Len < MaxSizeAtOnce then
      begin
        // single socket send() is possible (body fits in the sending buffer)
        Process.Reserve(ContentLength + Head.Len);
        Process.Append(Head.Buffer, Head.Len);
        Process.Append(Content);
        Content := ''; // release ASAP
        Head.Reset; // DoRequest will use Process
        State := hrsResponseDone;
      end
      else
        // async huge body sent using Write polling
        State := hrsSendBody;
    end
  else
    // ContentStream requires async body sending
    State := hrsSendBody;
end;

procedure THttpRequestContext.ProcessBody(
  var Dest: TRawByteStringBuffer; MaxSize: PtrInt);
var
  P: pointer;
begin
  // THttpAsyncConnection.DoRequest did send the headers: now send body chunks
  if State <> hrsSendBody then
    exit;
  // send the body in the background, using polling up to socket.SendBufferSize
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
  if hfContentStreamNeedFree in HeaderFlags then
    FreeAndNilSafe(ContentStream);
end;

function THttpRequestContext.ContentFromFile(
  const FileName: TFileName; CompressGz: integer): boolean;
var
  gz: TFileName;
begin
  Content := '';
  if (CompressGz >= 0) and
     (CompressGz in CompressAcceptHeader) then
  begin
    // try locally cached gzipped static content
    gz := FileName + '.gz';
    ContentLength := FileSize(gz);
    if ContentLength > 0 then
    begin
      // there is an already-compressed .gz file to send away
      ContentStream := TFileStream.Create(gz, fmOpenRead or fmShareDenyNone);
      ContentEncoding := 'gzip';
      include(HeaderFlags, hfContentStreamNeedFree);
      result := true;
      exit; // use the stream to bypass recompression
    end;
  end;
  ContentLength := FileSize(FileName);
  result := ContentLength <> 0;
  if not result then
    // there is no such file available
    exit;
  ContentStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  if ContentLength < 1 shl 20 then
  begin
    // load smallest files (up to 1MB) in temp memory (and maybe compress them)
    SetLength(Content, ContentLength);
    ContentStream.Read(pointer(Content)^, ContentLength);
    FreeAndNilSafe(ContentStream);
  end
  else
    // stream existing big file by chunks
    include(HeaderFlags, hfContentStreamNeedFree);
end;


function ToText(st: THttpRequestState): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpRequestState), ord(st));
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
    OutContentEncoding := CompressContent(
      Http.CompressAcceptHeader, Http.Compress, OutContentType, OutContent);
    if OutContentEncoding <> '' then
      SockSend(['Content-Encoding: ', OutContentEncoding]);
  end;
  if OutStream = nil then
    len := length(OutContent)
  else
    len := OutStream.Size;
  SockSend(['Content-Length: ', len]); // needed even 0
  if (OutContentType <> '') and
     (OutContentType <> STATICFILE_CONTENT_TYPE) then
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
  P: PAnsiChar;
begin
  b := byte(hf);
  result[0] := #0;
  P := _FLAGS;
  repeat
    if b and 1 <> 0 then
    begin
      inc(result[0]);
      result[ord(result[0])] := P^;
    end;
    inc(P);
    b := b shr 1;
  until b = 0;
end;

procedure THttpSocket.GetHeader(HeadersUnFiltered: boolean);
var
  s: RawUtf8;
  err: integer;
  line: array[0..4095] of AnsiChar; // avoid most memory allocation
begin
  // parse the headers
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
      Http.ParseHeader(@line, HeadersUnFiltered);
    until false
  else
    repeat
      SockRecvLn(s);
      if s = '' then
        break;
      Http.ParseHeader(pointer(s), HeadersUnFiltered);
    until false;
  // finalize the headers
  Http.ParseHeaderFinalize; // compute all meaningful headers
  if Assigned(OnLog) then
    OnLog(sllTrace, 'GetHeader % flags=% len=% %', [Http.Command,
      ToText(Http.HeaderFlags), Http.ContentLength, Http.ContentType], self);
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
    // supplied Content-Length header should be ignored when chunked
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
          SetString(chunk, nil, len32 + len32 shr 3); // + shr 3 to avoid realloc
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
    if SockIn <> nil then // client loop for compatibility with old servers
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
  {$ifdef SYNCRTDEBUGLOW}
  TSynLog.Add.Log(sllCustom2, 'GetBody sock=% pending=% sockin=% len=% %',
    [fSock, SockInPending(0), PTextRec(SockIn)^.BufEnd - PTextRec(SockIn)^.bufpos,
    ContentLength, LogEscapeFull(Content)], self);
  {$endif SYNCRTDEBUGLOW}
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
    Http.Headers := Http.Headers + aValue + #13#10;
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
    Http.Headers := Http.Headers + 'Content-Type: ' + aForcedContentType + #13#10;
end;

procedure THttpSocket.HeadersPrepare(const aRemoteIP: RawUtf8);
begin
  if (aRemoteIP <> '') and
     not (hfHasRemoteIP in Http.HeaderFlags) then
  begin
    // Http.ParseHeaderFinalize did reserve 40 bytes for fast realloc
    Http.Headers := Http.Headers + 'RemoteIP: ' + aRemoteIP + #13#10;
    include(Http.HeaderFlags, hfHasRemoteIP);
  end;
end;

function THttpSocket.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  result := Http.HeaderGetValue(aUpperName);
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(Http.Compress, aFunction,
    Http.CompressAcceptEncoding, aCompressMinSize) <> '';
end;


{ ******************** Abstract Server-Side Types used e.g. for Client-Server Protocol }

{ THttpServerRequestAbstract }

procedure THttpServerRequestAbstract.Prepare(
  const aUrl, aMethod, aInHeaders: RawUtf8; const aInContent: RawByteString;
  const aInContentType, aRemoteIP: RawUtf8);
begin
  fUrl := aUrl;
  fMethod := aMethod;
  fRemoteIP := aRemoteIP;
  fInHeaders := aInHeaders;
  fHost := '';
  fAuthBearer := '';
  fUserAgent := '';
  fInContent := aInContent;
  fInContentType := aInContentType;
  fOutContent := '';
  fOutContentType := '';
  fOutCustomHeaders := '';
end;

procedure THttpServerRequestAbstract.Prepare(const aHttp: THttpRequestContext;
  const aRemoteIP: RawUtf8);
begin
  fUrl := aHttp.CommandUri;
  fMethod := aHttp.CommandMethod;
  fRemoteIP := aRemoteIP;
  fInHeaders := aHttp.Headers;
  fHost := aHttp.Host;
  fAuthBearer := aHttp.BearerToken;
  fUserAgent := aHttp.UserAgent;
  fInContent := aHttp.Content;
  fInContentType := aHttp.ContentType;
  fOutContent := '';
  fOutContentType := '';
  fOutCustomHeaders := '';
end;

procedure THttpServerRequestAbstract.AddInHeader(AppendedHeader: RawUtf8);
begin
  TrimSelf(AppendedHeader);
  if AppendedHeader <> '' then
    if fInHeaders = '' then
      fInHeaders := AppendedHeader
    else
      fInHeaders := fInHeaders + #13#10 + AppendedHeader;
end;

procedure THttpServerRequestAbstract.AddOutHeader(const Values: array of const);
begin
  AppendLine(fOutCustomHeaders, Values);
end;



end.

