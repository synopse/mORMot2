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
  mormot.net.sock;


{ ******************** Shared HTTP Constants and Functions }

type
  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlzo' or 'synlz')
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


/// adjust HTTP body compression according to the supplied 'CONTENT-TYPE'
// - will detect most used compressible content (like 'text/*' or
// 'application/json') from OutContentType
function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
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
    hrsGetBodyChunkedHex,
    hrsGetBodyChunkedData,
    hrsGetBodyChunkedDataVoidLine,
    hrsGetBodyChunkedDataLastLine,
    hrsGetBodyContentLength,
    hrsWaitProcessing,
    hrsSendHeaders,
    hrsSendBody,
    hrsResponseDone,
    hrsErrorMisuse,
    hrsErrorUnsupportedFormat,
    hrsErrorPayloadTooLarge,
    hrsErrorAborted);

  /// set of states for THttpRequestContext processing
  THttpRequestStates = set of THttpRequestState;

  /// customize THttpRequestContext process
  THttpRequestOptions = set of (
    hroHeadersUnfiltered);

  /// map the presence of some HTTP headers for THttpRequestContext.HeaderFlags
  THttpRequestHeaderFlags = set of (
    hfTransferChunked,
    hfConnectionClose,
    hfConnectionUpgrade,
    hfConnectionKeepAlive,
    hfHasRemoteIP,
    hfContentStreamNeedFree);

  PHttpRequestContext = ^THttpRequestContext;

  /// optional callback triggered when THttpRequestContext state changes
  // - i.e. after Command, Headers or Content have been retrieved
  // - should return the current Sender.State, or an error to interrupt the
  // process (typically hrsErrorAborted)
  TOnHttpRequestStateChange = function(Previous: THttpRequestState;
     Sender: PHttpRequestContext): THttpRequestState of object;

  /// low-level reusable State Machine to parse and process any HTTP content
  {$ifdef USERECORDWITHMETHODS}
  THttpRequestContext = record
  {$else}
  THttpRequestContext = object
  {$endif USERECORDWITHMETHODS}
  private
    // internal reusable buffers - reduce memory allocations during process
    Head, Process: TRawByteStringBuffer;
    ContentStream: TStream; // to be used instead of Content
    ContentLeft: Int64;
    ContentPos: PByte;
    ContentEncoding: RawUtf8;
    function ContentFromFile(const FileName: TFileName; CompressGz: integer): boolean;
  public
    /// the current state of this HTTP context
    State: THttpRequestState;
    /// map the presence of some HTTP headers, but retrieved during Request
    HeaderFlags: THttpRequestHeaderFlags;
    /// customize the HTTP process
    Options: THttpRequestOptions;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: RawUtf8;
    /// will contain all header lines after a Request
    // - use HeaderGetValue() to get one HTTP header item value by name
    Headers: RawUtf8;
    /// same as HeaderGetValue('CONTENT-TYPE'), but retrieved during Request
    ContentType: RawUtf8;
    /// same as HeaderGetValue('ACCEPT-ENCODING'), but retrieved during Request
    AcceptEncoding: RawUtf8;
    /// same as HeaderGetValue('UPGRADE'), but retrieved during Request
    Upgrade: RawUtf8;
    /// same as FindNameValue(aInHeaders, HEADER_BEARER_UPPER, ...),
    // but retrieved during Request
    // - is the raw Token, excluding 'Authorization: Bearer ' trailing chars
    BearerToken: RawUtf8;
    /// same as HeaderGetValue('X-POWERED-BY'), but retrieved during Request
    XPoweredBy: RawUtf8;
    /// will contain the data retrieved from the server, after the Request
    Content: RawByteString;
    /// same as HeaderGetValue('CONTENT-LENGTH'), but retrieved during Request
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: Int64;
    /// same as HeaderGetValue('SERVER-INTERNALSTATE'), but retrieved during Request
    // - proprietary header, used with our RESTful ORM access
    ServerInternalState: integer;
    /// the known Content-Encoding compression methods
    Compress: THttpSocketCompressRecDynArray;
    /// supported Content-Encoding compression methods as sent to the other side
    CompressAcceptEncoding: RawUtf8;
    /// index of protocol in Compress[], from Accept-encoding
    CompressAcceptHeader: THttpSocketCompressSet;
    /// same as HeaderGetValue('CONTENT-ENCODING'), but retrieved during Request
    // and mapped into the Compress[] array
    CompressContentEncoding: integer;
    /// optional callback to track the HTTP ProcessRead/ProcessWrite work
    OnStateChange: TOnHttpRequestStateChange;
    /// reset this request context to be used without any ProcessInit/Read/Write
    procedure Clear;
    /// parse a HTTP header into Headers and fill internal properties
    // - with default HeadersUnFiltered=false, only relevant headers are retrieved:
    // use directly the ContentLength/ContentType/ServerInternalState/Upgrade
    // and HeaderFlags fields since HeaderGetValue() would return ''
    // - force HeadersUnFiltered=true to store all headers including the
    // connection-related fields, but increase memory and reduce performance
    // - returns the position of the end of the line, excluding trailing #13/#0
    function ParseHeader(P: PUtf8Char; HeadersUnFiltered: boolean = false): PUtf8Char;
    /// to be called once all ParseHeader lines have been done
    // - also set CompressContentEncoding/CompressAcceptHeader from Compress[]
    // and Content-Encoding header value
    procedure ParseHeaderFinalize;
    /// search a value from the internal parsed Headers
    // - supplied aUpperName should be already uppercased:
    // HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - note that GetHeader(HeadersUnFiltered=false) will set ContentType field
    // but let HeaderGetValue('CONTENT-TYPE') return ''
    function HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
    /// uncompress Content according to CompressContentEncoding header
    procedure UncompressData;
    /// (re)initialize the HTTP Server state machine for ProcessRead/ProcessWrite
    procedure ProcessInit(InStream: TStream);
    /// receiving socket entry point of our asynchronous HTTP Server
    // - to be called with the incoming bytes from the socket receive buffer
    // - caller should have checked that current State is in HTTP_REQUEST_READ
    procedure ProcessRead(P: pointer; Len: PtrInt);
    /// compress Content according to CompressAcceptHeader, adding headers
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Content is not '', will add 'Content-Type: ' header
    // - always compute ContentLength and add a 'Content-Length: ' header
    procedure CompressContentAndFinalizeHead;
    /// sending socket entry point of our asynchronous HTTP Server
    // - to be called when some bytes could be written to output socket
    // - main entry point of the asynchronous Machine State of HTTP processing
    // - caller should have checked that current State is in HTTP_REQUEST_WRITE
    // - returns how many bytes from P should be sent to the socket
    function ProcessWrite(out P: pointer; MaxSize: PtrInt): Int64;
    /// should be done when the HTTP Server state machine is done
    procedure ProcessDone;
  end;

const
  /// when THttpRequestContext.State is expected some ProcessRead() data
  HTTP_REQUEST_READ =
    [hrsGetCommand,
     hrsGetHeaders,
     hrsGetBodyChunkedHex,
     hrsGetBodyChunkedData,
     hrsGetBodyChunkedDataVoidLine,
     hrsGetBodyContentLength];

  /// when THttpRequestContext.State is expected some ProcessWrite() data
  HTTP_REQUEST_WRITE =
    [hrsSendHeaders,
     hrsSendBody];


{ ******************** THttpSocket Implementing HTTP over plain sockets }

type
  /// exception class raised during HTTP process
  EHttpSocket = class(ENetSock);

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing HTTP/1.1 using the Socket API
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlzo/synlz) protocols
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
    /// get all Header values at once, as CRLF delimited text
    // - you can optionally specify a value to be added as 'RemoteIP: ' header
    // - default GetHeader(HeadersUnFiltered=false) won't include the connection
    // related headers like ContentLength/ContentType/ServerInternalState/Upgrade
    function HeaderGetText(const aRemoteIP: RawUtf8 = ''): RawUtf8;
    /// HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - supplied aUpperName should be already uppercased
    // - note that GetHeader(HeadersUnFiltered=false) will set ContentType field
    // but let HeaderGetValue('CONTENT-TYPE') return ''
    function HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
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
  TOnHttpServerAfterResponse = procedure(Ctxt: THttpServerRequestAbstract;
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
  // - match TRestUriParamsLowLevelFlag in mormot.rest.core
  THttpServerRequestFlag = (
    hsrHttps,
    hsrSecured,
    hsrWebsockets);

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
    fOutContentType,
    fOutCustomHeaders: RawUtf8;
    fInContent,
    fOutContent: RawByteString;
    fRequestID: integer;
    fConnectionID: THttpServerConnectionID;
    fConnectionFlags: THttpServerRequestFlags;
    fAuthenticationStatus: THttpServerRequestAuthentication;
    fRespStatus: integer;
  public
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aUrl, aMethod, aInHeaders: RawUtf8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8);
        virtual; abstract;
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(AppendedHeader: RawUtf8);
    /// prepare one reusable State Machine for sending the response
    procedure SetupResponse(var Context: THttpRequestContext;
      const ServerName: RawUtf8; const ErrorMessage: string;
      const OnSendFile: TOnHttpServerSendFile; CompressGz: integer);
    /// input parameter containing the caller URI
    property Url: RawUtf8
      read fUrl;
    /// input parameter containing the caller method (GET/POST...)
    property Method: RawUtf8
      read fMethod;
    /// input parameter containing the caller message headers
    property InHeaders: RawUtf8
      read fInHeaders;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    property InContent: RawByteString
      read fInContent;
    // input parameter defining the caller message body content type
    property InContentType: RawUtf8
      read fInContentType;
    /// output HTTP response status
    property RespStatus: integer
      read fRespStatus write fRespStatus;
    /// output parameter to be set to the response message body
    property OutContent: RawByteString
      read fOutContent write fOutContent;
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



implementation


{ ******************** Shared HTTP Constants and Functions }

function AuthorizationBearer(const AuthToken: RawUtf8): RawUtf8;
begin
  if AuthToken = '' then
    result := ''
  else
    result := 'Authorization: Bearer ' + AuthToken;
end;

function PurgeHeaders(const headers: RawUtf8): RawUtf8;
var
  ok: array[byte] of PUtf8Char;
  len: array[byte] of integer;
  n, purged, i, tot: PtrInt;
  P, next: PUtf8Char;
begin
  n := 0;
  tot := 0;
  purged := 0;
  P := pointer(headers);
  while P <> nil do
  begin
    if P^ = #0 then
      break;
    next := GotoNextLine(P);
    if IdemPCharArray(P, [
       'CONTENT-',
       'CONNECTION:',
       'KEEP-ALIVE:',
       'TRANSFER-',
       'X-POWERED',
       'USER-AGENT',
       'REMOTEIP:',
       'HOST:',
       'ACCEPT:']) < 0 then
    begin
      if n = high(len) then
        break;
      ok[n] := P;
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
      MoveFast({%H-}ok[i]^, P^, {%H-}len[i]);
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

function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
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
    case IdemPCharArray(OutContentTypeP,
          ['TEXT/',
           'IMAGE/',
           'APPLICATION/']) of
      0:
        compressible := true;
      1:
        compressible := IdemPCharArray(OutContentTypeP + 6,
          ['SVG',
           'X-ICO']) >= 0;
      2:
        compressible := IdemPCharArray(OutContentTypeP + 12,
          ['JSON',
           'XML',
           'JAVASCRIPT']) >= 0;
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
  while P^ > #13 do // a header would never contain a control char
    inc(P);
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
  Command := '';
  Headers := '';
  ContentType := '';
  Upgrade := '';
  BearerToken := '';
  XPoweredBy := '';
  Content := '';
  ContentLength := -1;
  ServerInternalState := 0;
  CompressContentEncoding := -1;
  integer(CompressAcceptHeader) := 0;
end;

function THttpRequestContext.ParseHeader(P: PUtf8Char;
  HeadersUnFiltered: boolean): PUtf8Char;
var
  i, len: PtrInt;
begin
  result := P;
  if P = nil then
    exit; // avoid unexpected GPF in case of wrong usage
  case IdemPCharArray(P, [
      'CONTENT-',
      'TRANSFER-ENCODING: CHUNKED',
      'CONNECTION: ',
      'ACCEPT-ENCODING:',
      'UPGRADE:',
      'SERVER-INTERNALSTATE:',
      'X-POWERED-BY:',
      HEADER_BEARER_UPPER]) of
    0:
      // 'CONTENT-'
      case IdemPCharArray(P + 8, [
            'LENGTH:',
            'TYPE:',
            'ENCODING:']) of
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
            len := PtrUInt(P);
            while P^ > #13 do
              inc(P); // no control char should appear in any header
            len := PtrInt(PtrUInt(P)) - len;
            if len <> 0 then
              for i := 0 to length(Compress) - 1 do
                if IdemPropNameU(Compress[i].Name, result, len) then
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
        case IdemPCharArray(P, [
              'CLOSE',
              'UPGRADE',
              'KEEP-ALIVE']) of
          0:
            // 'CONNECTION: CLOSE'
            include(HeaderFlags, hfConnectionClose);
          1:
            // 'CONNECTION: UPGRADE'
            include(HeaderFlags, hfConnectionUpgrade);
          2:
            begin
              // 'CONNECTION: KEEP-ALIVE'
              include(HeaderFlags, hfConnectionKeepAlive);
              if P[10] = ',' then
              begin
                P := GotoNextNotSpace(P + 11);
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
      // 'AUTHORIZATION: BEARER '
      begin
        inc(P, 22);
        GetTrimmed(P, BearerToken);
        if BearerToken <> '' then
          // always allow FindNameValue(..., HEADER_BEARER_UPPER, ...) search
          HeadersUnFiltered := true;
      end
  else
    // unrecognized name should be stored in Headers
    HeadersUnFiltered := true;
  end;
  while P^ > #13 do
    inc(P); // no control char should appear in any header
  if HeadersUnFiltered then
    // store meaningful headers into WorkBuffer, if not already there
    Head.Append(result, P - result, {crlf=}true);
  result := P;
end;

function THttpRequestContext.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  FindNameValue(Headers, pointer(aUpperName), result, false, ':');
end;

procedure THttpRequestContext.ParseHeaderFinalize;
begin
  Head.AsText(Headers, {OverheadForRemoteIP=}40);
  Head.Reset;
  if Compress <> nil then
    if AcceptEncoding <> '' then
      CompressAcceptHeader :=
        ComputeContentEncoding(Compress, pointer(AcceptEncoding));
end;

procedure THttpRequestContext.UncompressData;
begin
  if cardinal(CompressContentEncoding) < cardinal(length(Compress)) then
  begin
    if Compress[CompressContentEncoding].Func(Content, false) = '' then
      // invalid content
      raise EHttpSocket.CreateFmt('%s uncompress failed',
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

type
  TProcessParseLine = record
    P: PUtf8Char;
    Len: PtrInt;
    Line: PUtf8Char;
    LineLen: PtrInt;
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
    if P[0] > #13 then
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
  // go to beginning of next line
  dec(Len);
  if (Len <> 0) and
     (PWord(P)^ = $0a0d) then
    begin
      inc(P);
      dec(Len);
    end;
  inc(P);
  st.P := P;
  st.Len := Len;
end;

procedure THttpRequestContext.ProcessRead(P: pointer; Len: PtrInt);
var
  st: TProcessParseLine;
  previous: THttpRequestState;
label
  fin;
begin
  if (P = nil) or
     (Len <= 0) then
    exit;
  if Process.Len <> 0 then
  begin
    Process.Append(P, Len);
    st.P := Process.Buffer;
    st.Len := Process.Len;
  end
  else
  begin
    st.P := P; // most of the time, input comes as a single block
    st.Len := Len;
  end;
  previous := State;
  repeat
    // main input loop of our HTTP server asynchronous state machine
    case State of
      hrsGetCommand:
        if ProcessParseLine(st, @Command) then
          State := hrsGetHeaders
        else
          break; // not enough input
      hrsGetHeaders:
        if ProcessParseLine(st, nil) then
          if st.LineLen <> 0 then
            ParseHeader(st.P, hroHeadersUnfiltered in Options)
          else
          begin
            // void line indicates end of Headers
            ParseHeaderFinalize;
            // Content-Length or Transfer-Encoding (HTTP/1.1 RFC2616 4.3)
            if hfTransferChunked in HeaderFlags then
              State := hrsGetBodyChunkedHex
            else if ContentLength > 0 then
              State := hrsGetBodyContentLength
              // note: old HTTP/1.0 format with no Content-Length is unsupported
            else
              State := hrsWaitProcessing;
          end
        else
          break;
      hrsGetBodyChunkedHex:
        if ProcessParseLine(st, nil) then
        begin
          ContentLeft := HttpChunkToHex32(PAnsiChar(st.Line));
          if ContentLeft <> 0 then
          begin
            if ContentStream = nil then
            begin
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
          break;
      hrsGetBodyChunkedData:
        begin
          if st.Len < ContentLeft then
            Len := st.Len
          else
            Len := ContentLeft;
          if ContentStream <> nil then
            ContentStream.WriteBuffer(st.P^, Len)
          else
          begin
            MoveFast(st.P^, ContentPos^, Len);
            inc(ContentPos, Len);
          end;
          dec(ContentLeft, Len);
          if ContentLeft = 0 then
            State := hrsGetBodyChunkedDataVoidLine;
        end;
      hrsGetBodyChunkedDataVoidLine:
        if ProcessParseLine(st, nil) then // chunks end with a void line
          State := hrsGetBodyChunkedHex
        else
          break;
      hrsGetBodyChunkedDataLastLine:
        if ProcessParseLine(st, nil) then // last chunk
          if st.Len <> 0 then
            State := hrsErrorUnsupportedFormat // should be no further input
          else
            State := hrsWaitProcessing
        else
          break;
      hrsGetBodyContentLength:
        begin
          if ContentLeft = 0 then
            ContentLeft := ContentLength;
          if st.Len < ContentLeft then
            Len := st.Len
          else
            Len := ContentLeft;
          if ContentStream= nil then
          begin
            if Content = '' then
            begin
              if ContentLength > 1 shl 30 then // 1 GB
              begin
                State := hrsErrorPayloadTooLarge; // avoid memory overflow
                goto fin;
              end;
              SetLength(Content, ContentLength);
              ContentPos := pointer(Content);
            end;
            MoveFast(st.P^, ContentPos^, Len);
            inc(ContentPos, Len);
          end
          else
            ContentStream.WriteBuffer(st.P^, Len);
          dec(st.Len, Len);
          dec(ContentLeft, Len);
          if ContentLeft = 0 then
            if st.Len <> 0 then
              State := hrsErrorUnsupportedFormat // should be no further input
            else
              State := hrsWaitProcessing;
        end;
    else
      State := hrsErrorMisuse; // out of context State for input
    end;
    if (State <> previous) and
       (State <> hrsGetBodyChunkedHex) and
       (State <> hrsGetBodyChunkedDataVoidLine) and
       Assigned(OnStateChange) then
    begin
      State := OnStateChange(previous, @self);
      previous := State;
    end;
    if State < hrsWaitProcessing then
      continue;
    // everything is done, or an error occured
fin:Process.Reset;
    exit;
  until false;
  // if we reached here, we didn't have enough input for full parsing
  if Process.Len <> 0 then
    Process.Remove(st.P - Process.Buffer)
  else
    Process.Append(st.P, st.Len);
  // we may have more luck in next ProcessRead() call
end;

procedure THttpRequestContext.CompressContentAndFinalizeHead;
begin
  // same logic than THttpSocket.CompressDataAndWriteHeaders below
  if (integer(CompressAcceptHeader) <> 0) and
     (ContentStream = nil) then // no stream compression (yet)
    ContentEncoding := CompressDataAndGetHeaders(
      CompressAcceptHeader, Compress, ContentType, Content);
  if ContentEncoding <> '' then
    Head.Append(['Content-Encoding: ', ContentEncoding]);
  if ContentStream = nil then
  begin
    ContentPos := pointer(Content);
    ContentLength := length(Content);
  end
  else if ContentLength = 0 then // maybe set by SetupResponse for local file
    ContentLength := ContentStream.Size - ContentStream.Position;
  Head.Append(['Content-Length: ', ContentLength]); // needed even 0
  if (ContentType <> '') and
     (ContentType <> STATICFILE_CONTENT_TYPE) then
    Head.Append(['Content-Type: ', ContentType]);
  if not (hfConnectionClose in HeaderFlags) then
  begin
    if CompressAcceptEncoding <> '' then
      Head.Append(CompressAcceptEncoding, {crlf=}true);
    Head.Append('Connection: Keep-Alive', {crlf=}true);
  end;
  Head.Append(nil, 0, {crlf=}true); // headers finish with a void line
end;

function THttpRequestContext.ProcessWrite(out P: pointer; MaxSize: PtrInt): Int64;
var
  previous: THttpRequestState;
begin
  previous := State;
  case State of
    hrsSendHeaders:
      begin
        if (ContentStream = nil) and
           Head.CanAppend(length(Content)) then
        begin
          // single socket send() if possible (small or no output body)
          Head.Append(Content);
          State := hrsResponseDone;
        end
        else
          State := hrsSendBody;
        P := Head.Buffer;
        result := Head.Len;
      end;
    hrsSendBody:
      begin
        result := ContentLength;
        if result > MaxSize then
          result := MaxSize;
        if result > 0 then
        begin
          if ContentStream <> nil then
          begin
            P := Process.Reserve(result);
            result := ContentStream.Read(P^, result);
          end
          else
          begin
            P := ContentPos;
            inc(ContentPos, result);
          end;
          dec(ContentLeft, result);
          if ContentLeft = 0 then
            State := hrsResponseDone;
        end
        else
          raise EHttpSocket.Create('Unexpected ProcessWrite(%d)', [MaxSize]);
      end;
  else
    begin
      result := 0;
      exit; // nothing to write in this current state
    end;
  end;
  if (State <> previous) and
     Assigned(OnStateChange) then
    State := OnStateChange(previous, @self);
end;

procedure THttpRequestContext.ProcessDone;
begin
  if hfContentStreamNeedFree in HeaderFlags then
    FreeAndNil(ContentStream);
  HeaderFlags := [];
  Content := ''; // release memory ASAP
  ContentEncoding := '';
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
      exit;
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
    FreeAndNil(ContentStream);
  end
  else
    // stream existing big file by chunks
    include(HeaderFlags, hfContentStreamNeedFree);
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
    OutContentEncoding := CompressDataAndGetHeaders(
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

procedure THttpSocket.GetHeader(HeadersUnFiltered: boolean);
var
  s: RawUtf8;
  err: integer;
  line: array[0..4095] of AnsiChar; // avoid most memory allocation
begin
  // parse the headers
  HttpStateReset;
  if SockIn <> nil then
    while true do
    begin
      {$I-}
      readln(SockIn^, line);
      err := ioresult;
      if err <> 0 then
        raise EHttpSocket.CreateFmt('%s.GetHeader error=%d',
          [ClassNameShort(self)^, err]);
      {$I+}
      if line[0] = #0 then
        break; // HTTP headers end with a void line
      Http.ParseHeader(@line, HeadersUnFiltered);
    end
    else
    while true do
    begin
      SockRecvLn(s);
      if s = '' then
        break;
      Http.ParseHeader(pointer(s), HeadersUnFiltered);
    end;
  // finalize the headers
  Http.ParseHeaderFinalize; // compute all meaningful headers
end;

procedure THttpSocket.GetBody(DestStream: TStream);
var
  Line: RawUtf8;
  LinePChar: array[0..31] of AnsiChar; // 32 bits chunk length in hexa
  chunk: RawByteString;
  Len, LChunk, Error: integer;
begin
  fBodyRetrieved := true;
  Http.Content := '';
  if DestStream <> nil then
    if (cardinal(Http.CompressContentEncoding) < cardinal(length(Http.Compress))) then
      raise EHttpSocket.Create('%s.GetBody(%s) does not support compression',
        [ClassNameShort(self)^, ClassNameShort(DestStream)^]);
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if hfTransferChunked in Http.HeaderFlags then
  begin
    // supplied Content-Length header should be ignored when chunked
    Http.ContentLength := 0;
    repeat // chunks decoding loop
      if SockIn <> nil then
      begin
        readln(SockIn^, LinePChar); // use of a static PChar is faster
        Error := ioresult;
        if Error <> 0 then
          raise EHttpSocket.CreateFmt('GetBody chunked ioresult=%d', [Error]);
        Len := HttpChunkToHex32(LinePChar); // get chunk length in hexa
      end
      else
      begin
        SockRecvLn(Line);
        Len := HttpChunkToHex32(pointer(Line)); // get chunk length in hexa
      end;
      if Len = 0 then
      begin
        SockRecvLn; // ignore next line (normally void)
        break; // reached the end of input stream
      end;
      if DestStream <> nil then
      begin
        if length({%H-}chunk) < Len then
          SetString(chunk, nil, Len + Len shr 3); // + shr 3 to avoid realloc
        SockInRead(pointer(chunk), Len);
        DestStream.WriteBuffer(pointer(chunk)^, Len);
      end
      else
      begin
        SetLength(Http.Content, Http.ContentLength + Len); // reserve space for this chunk
        SockInRead(@PByteArray(Http.Content)[Http.ContentLength], Len); // append data
      end;
      inc(Http.ContentLength, Len);
      SockRecvLn; // ignore next #13#10
    until false;
  end
  else if Http.ContentLength > 0 then
    // read Content-Length header bytes
    if DestStream <> nil then
    begin
      LChunk := 256 shl 10; // not chunked: use a 256 KB temp buffer
      if Http.ContentLength < LChunk then
        LChunk := Http.ContentLength;
      SetLength(chunk, LChunk);
      Len := Http.ContentLength;
      repeat
        if LChunk > Len then
          LChunk := Len;
        SockInRead(pointer(chunk), LChunk);
        DestStream.WriteBuffer(pointer(chunk)^, LChunk);
        dec(Len, LChunk);
      until Len = 0;
    end
    else
    begin
      SetLength(Http.Content, Http.ContentLength); // not chuncked: direct read
      SockInRead(pointer(Http.Content), Http.ContentLength);
    end
  else if (Http.ContentLength < 0) and // -1 means no Content-Length header
          IdemPChar(pointer(Http.Command), 'HTTP/1.0 200') then
  begin
    // body = either Content-Length or Transfer-Encoding (HTTP/1.1 RFC2616 4.3)
    if SockIn <> nil then // client loop for compatibility with old servers
      while not eof(SockIn^) do
      begin
        readln(SockIn^, Line);
        if Http.Content = '' then
          Http.Content := Line
        else
          Http.Content := Http.Content + #13#10 + Line;
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
  Http.UncompressData;
  {$ifdef SYNCRTDEBUGLOW}
  TSynLog.Add.Log(sllCustom2, 'GetBody sock=% pending=% sockin=% len=% %',
    [fSock, SockInPending(0), PTextRec(SockIn)^.BufEnd - PTextRec(SockIn)^.bufpos,
    ContentLength, LogEscapeFull(Content)], self);
  {$endif SYNCRTDEBUGLOW}
  if SockIn <> nil then
  begin
    Error := ioresult;
    if Error <> 0 then
      raise EHttpSocket.CreateFmt('GetBody2 ioresult=%d', [Error]);
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

function THttpSocket.HeaderGetText(const aRemoteIP: RawUtf8): RawUtf8;
begin
  if (aRemoteIP <> '') and
     not (hfHasRemoteIP in Http.HeaderFlags) then
  begin
    // Http.ParseHeaderFinalize did reserve 40 bytes for fast realloc
    Http.Headers := Http.Headers + 'RemoteIP: ' + aRemoteIP + #13#10;
    include(Http.HeaderFlags, hfHasRemoteIP);
  end;
  result := Http.Headers;
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

procedure THttpServerRequestAbstract.AddInHeader(AppendedHeader: RawUtf8);
begin
  AppendedHeader := TrimU(AppendedHeader);
  if AppendedHeader <> '' then
    if fInHeaders = '' then
      fInHeaders := AppendedHeader
    else
      fInHeaders := fInHeaders + #13#10 + AppendedHeader;
end;

procedure THttpServerRequestAbstract.SetupResponse(var Context: THttpRequestContext;
  const ServerName: RawUtf8; const ErrorMessage: string;
  const OnSendFile: TOnHttpServerSendFile; CompressGz: integer);
var
  P, PEnd: PUtf8Char;
  len: PtrInt;
  reason: RawUtf8;
  fn: TFileName;
  err: string;
begin
  // note: caller should have set hfConnectionClose in Context.HeaderFlags
  err := ErrorMessage;
  // process content
  Context.ContentLength := 0;
  if OutContentType = NORESPONSE_CONTENT_TYPE then
    OutContentType := '' // true HTTP always expects a response
  else if (OutContent <> '') and
          (OutContentType = STATICFILE_CONTENT_TYPE) then
  begin
    ExtractHeader(fOutCustomHeaders, 'CONTENT-TYPE:', fOutContentType);
    Utf8ToFileName(OutContent, fn);
    if not Assigned(OnSendFile) or
       not OnSendFile(self, fn) then
      if not Context.ContentFromFile(fn, CompressGz) then
      begin
        FormatString('Impossible to find %', [fn], err);
        RespStatus := HTTP_NOTFOUND;
      end;
  end;
  StatusCodeToReason(RespStatus, reason);
  if err <> '' then
  begin
    OutCustomHeaders := '';
    OutContentType := 'text/html; charset=utf-8'; // create message to display
    OutContent := FormatUtf8('<body style="font-family:verdana">'#10 +
      '<h1>% Server Error %</h1><hr><p>HTTP % %<p>%<p><small>' + XPOWEREDVALUE,
      [ServerName, RespStatus, RespStatus, reason, HtmlEscapeString(err)]);
  end;
  // append Command
  Context.Head.Reset;
  if hfConnectionClose in Context.HeaderFlags then
    Context.Head.Append('HTTP/1.0 ')
  else
    Context.Head.Append('HTTP/1.1 ');
  Context.Head.Append([RespStatus, ' ', reason], {crlf=}true);
  // custom headers from Request() method
  P := pointer(OutCustomHeaders);
  if P <> nil then
  begin
    PEnd := P + length(OutCustomHeaders);
    repeat
      len := BufferLineLength(P, PEnd);
      if len > 0 then // no void line (means headers ending)
      begin
        if IdemPChar(P, 'CONTENT-ENCODING:') then
          // custom encoding: don't compress
          integer(Context.CompressAcceptHeader) := 0;
        Context.Head.Append(P, len, {crlf=}true); // normalize CR/LF endings
        inc(P, len);
      end;
      while P^ in [#10, #13] do
        inc(P);
    until P^ = #0;
  end;
  // generic headers
  Context.Head.Append('Server: ');
  Context.Head.Append(ServerName, {crlf=}true);
  {$ifndef NOXPOWEREDNAME}
  Context.Head.Append(XPOWEREDNAME + ': ' + XPOWEREDVALUE, true);
  {$endif NOXPOWEREDNAME}
  Context.Content := OutContent;
  Context.ContentType := OutContentType;
  Context.CompressContentAndFinalizeHead;
  // now Context.Head is ready and Context.ProcessWrite should be called
end;



end.

