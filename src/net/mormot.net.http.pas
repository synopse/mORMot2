/// HTTP/HTTPS Abstract Process Classes and Definitions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.http;

{
  *****************************************************************************

   HTTP/HTTPS Abstract Process Classes and Definitions
   - Shared HTTP Constants and Functions
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
function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: RawUtf8;
  aCompressMinSize: integer): RawUtf8;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PUtf8Char): THttpSocketCompressSet;


/// compute the 'Authorization: Bearer ####' HTTP header of a given token value
function AuthorizationBearer(const AuthToken: RawUtf8): RawUtf8;

/// will remove most usual HTTP headers which are to be recomputed on sending
function PurgeHeaders(P: PUtf8Char): RawUtf8;


{$ifndef NOXPOWEREDNAME}
const
  /// pseudo-header containing the current Synopse mORMot framework version
  XPOWEREDNAME = 'X-Powered-By';
  /// the full text of the current Synopse mORMot framework version
  // - we don't supply full version number with build revision
  // (as SYNOPSE_FRAMEWORK_VERSION), to reduce potential attacker knowledge
  XPOWEREDVALUE = SYNOPSE_FRAMEWORK_NAME + ' 2 synopse.info';
{$endif NOXPOWEREDNAME}


{ ******************** THttpSocket Implementing HTTP over plain sockets }

type
  /// exception class raised during HTTP process
  EHttpSocket = class(ENetSock);

  /// map the presence of some HTTP headers for THttpSocket.HeaderFlags
  THttpSocketHeaderFlags = set of (
    hfTransferChuked,
    hfConnectionClose,
    hfConnectionUpgrade,
    hfConnectionKeepAlive,
    hfHasRemoteIP);

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing HTTP/1.1 using the Socket API
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlzo/synlz) protocols
  THttpSocket = class(TCrtSocket)
  protected
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: RawUtf8;
    /// GetHeader set index of protocol in fCompress[], from ACCEPT-ENCODING:
    fCompressAcceptHeader: THttpSocketCompressSet;
    /// same as HeaderGetValue('CONTENT-ENCODING'), but retrieved during Request
    // and mapped into the fCompress[] array
    fContentCompress: integer;
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// fill the internal state and flags to their default/void values
    procedure HttpStateReset;
    /// compress the data, adding corresponding headers via SockSend()
    // - always add a 'Content-Length: ' header entry (even if length=0)
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Data is not '', will add 'Content-Type: ' header
    procedure CompressDataAndWriteHeaders(const OutContentType: RawUtf8;
      var OutContent: RawByteString);
  public
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: RawUtf8;
    /// will contain all header lines after a Request
    // - use HeaderGetValue() to get one HTTP header item value by name
    Headers: RawUtf8;
    /// will contain the data retrieved from the server, after the Request
    Content: RawByteString;
    /// same as HeaderGetValue('CONTENT-LENGTH'), but retrieved during Request
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: integer;
    /// same as HeaderGetValue('SERVER-INTERNALSTATE'), but retrieved during Request
    // - proprietary header, used with our RESTful ORM access
    ServerInternalState: integer;
    /// same as HeaderGetValue('CONTENT-TYPE'), but retrieved during Request
    ContentType: RawUtf8;
    /// same as HeaderGetValue('UPGRADE'), but retrieved during Request
    Upgrade: RawUtf8;
    /// same as FindNameValue(aInHeaders, HEADER_BEARER_UPPER, ...),
    // but retrieved during Request
    // - is the raw Token, excluding 'Authorization: Bearer ' trailing chars
    BearerToken: RawUtf8;
    /// same as HeaderGetValue('X-POWERED-BY'), but retrieved during Request
    XPoweredBy: RawUtf8;
    /// map the presence of some HTTP headers, but retrieved during Request
    HeaderFlags: THttpSocketHeaderFlags;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    // - with default HeadersUnFiltered=false, only relevant headers are retrieved:
    // use directly the ContentLength/ContentType/ServerInternalState/Upgrade
    // and HeaderFlags fields since HeaderGetValue() would return ''
    // - force HeadersUnFiltered=true to store all headers including the
    // connection-related fields, but increase memory and reduce performance
    procedure GetHeader(HeadersUnFiltered: boolean = false);
    /// retrieve the HTTP body (after uncompression if necessary) into Content
    procedure GetBody;
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
  TOnHttpServerBeforeBody = function(var aURL, aMethod, aInHeaders,
    aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: integer;
    aFlags: THttpServerRequestFlags): cardinal of object;

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
  public
    /// low-level property which may be used during requests processing
    Status: integer;
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aURL, aMethod, aInHeaders: RawUtf8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUtf8);
        virtual; abstract;
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(additionalHeader: RawUtf8);
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

function PurgeHeaders(P: PUtf8Char): RawUtf8;
var
  tmp: TTextWriterStackBuffer;
  next: PUtf8Char;
  W: TBaseWriter;
begin
  result := '';
  W := nil;
  try
    while P <> nil do
    begin
      next := GotoNextLine(P);
      if IdemPCharArray(P, ['CONTENT-', 'CONNECTION:', 'KEEP-ALIVE:', 'TRANSFER-',
         'X-POWERED', 'USER-AGENT', 'REMOTEIP:', 'HOST:', 'ACCEPT:']) < 0 then
      begin
        if W = nil then
          W := TBaseWriter.CreateOwnedStream(tmp);
        if next = nil then
          W.AddNoJsonEscape(P)
        else
          W.AddNoJsonEscape(P, next - P);
      end;
      P := next;
    end;
    if W <> nil then
      W.SetText(result);
  finally
    W.Free;
  end;
end;

function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: RawUtf8;
  aCompressMinSize: integer): RawUtf8;
var
  i, n: PtrInt;
  dummy: RawByteString;
  aName: RawUtf8;
begin
  result := '';
  if @aFunction = nil then
    exit;
  n := length(Compress);
  aName := aFunction(dummy, {compress}true); // just retrieve algo name
  for i := 0 to n - 1 do
    with Compress[i] do
      if Name = aName then
      begin
        // already set
        if @Func = @aFunction then // update min. compress size value
          CompressMinSize := aCompressMinSize;
        exit;
      end;
  if n = sizeof(integer) * 8 then
    exit; // fCompressAcceptHeader is 0..31 (casted as integer)
  SetLength(Compress, n + 1);
  with Compress[n] do
  begin
    Name := aName;
    @Func := @aFunction;
    CompressMinSize := aCompressMinSize;
  end;
  if aAcceptEncoding = '' then
    aAcceptEncoding := 'Accept-Encoding: ' + aName
  else
    aAcceptEncoding := aAcceptEncoding + ',' + aName;
  result := aName;
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
    case IdemPCharArray(OutContentTypeP, ['TEXT/', 'IMAGE/', 'APPLICATION/']) of
      0:
        compressible := true;
      1:
        compressible := IdemPCharArray(OutContentTypeP + 6,
          ['SVG', 'X-ICO']) >= 0;
      2:
        compressible := IdemPCharArray(OutContentTypeP + 12,
          ['JSON', 'XML', 'JAVASCRIPT']) >= 0;
    else
      compressible := false;
    end;
    for i := 0 to high(Handled) do
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
  i: PtrInt;
  aName: RawUtf8;
  Beg: PUtf8Char;
begin
  integer(result) := 0;
  if P <> nil then
    repeat
      while P^ in [' ', ','] do
        inc(P);
      Beg := P; // 'gzip;q=1.0, deflate' -> aName='gzip' then 'deflate'
      while not (P^ in [';', ',', #0]) do
        inc(P);
      FastSetString(aName, Beg, P - Beg);
      for i := 0 to high(Compress) do
        if aName = Compress[i].Name then
          include(result, i);
      while not (P^ in [',', #0]) do
        inc(P);
    until P^ = #0;
end;

procedure GetTrimmed(P: PUtf8Char; var result: RawUtf8);
var
  B: PUtf8Char;
begin
  while (P^ > #0) and
        (P^ <= ' ') do
    inc(P);
  B := P;
  while P^ <> #0 do
    inc(P);
  while (P > B) and
        (P[-1] <= ' ') do
    dec(P);
  FastSetString(result, B, P - B);
end;

function HttpChunkToHex32(p: PAnsiChar): integer;
var
  v0, v1: byte;
begin
  result := 0;
  if p <> nil then
  begin
    while p^ = ' ' do
      inc(p);
    repeat
      v0 := ConvertHexToBin[ord(p[0])];
      if v0 = 255 then
        break; // not in '0'..'9','a'..'f'
      v1 := ConvertHexToBin[ord(p[1])];
      inc(p);
      if v1 = 255 then
      begin
        result := (result shl 4) or v0; // only one hexa char supplied
        break;
      end;
      result := (result shl 8) or (integer(v0) shl 4) or v1;
      inc(p);
    until false;
  end;
end;



{ ******************** THttpSocket Implementing HTTP over plain sockets }

{ THttpSocket }

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: RawUtf8;
  var OutContent: RawByteString);
var
  OutContentEncoding: RawUtf8;
begin
  if integer(fCompressAcceptHeader) <> 0 then
  begin
    OutContentEncoding := CompressDataAndGetHeaders(
      fCompressAcceptHeader, fCompress, OutContentType, OutContent);
    if OutContentEncoding <> '' then
      SockSend(['Content-Encoding: ', OutContentEncoding]);
  end;
  SockSend(['Content-Length: ', length(OutContent)]); // needed even 0
  if (OutContentType <> '') and
     (OutContentType <> STATICFILE_CONTENT_TYPE) then
    SockSend(['Content-Type: ', OutContentType]);
end;

procedure THttpSocket.HttpStateReset;
begin
  HeaderFlags := [];
  Headers := '';
  fBodyRetrieved := false;
  fContentCompress := -1;
  integer(fCompressAcceptHeader) := 0;
  ContentType := '';
  Upgrade := '';
  ContentLength := -1;
  Content := '';
  ServerInternalState := 0;
  BearerToken := '';
end;

procedure THttpSocket.GetHeader(HeadersUnFiltered: boolean);
var
  s, c: RawUtf8;
  i, len: PtrInt;
  err: integer;
  P: PUtf8Char;
  line: array[0..4095] of AnsiChar; // avoid most memory allocation
begin
  HttpStateReset;
  fSndBufLen := 0; // SockSend() used as headers temp buffer to avoid getmem
  repeat
    P := @line;
    if (SockIn <> nil) and
       not HeadersUnFiltered then
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
    end
    else
    begin
      SockRecvLn(s);
      if s = '' then
        break;
      P := pointer(s);
    end;
    // note: set P=nil below to store in Headers[]
    case IdemPCharArray(P, [
        'CONTENT-',
        'TRANSFER-ENCODING: CHUNKED',
        'CONNECTION: ',
        'ACCEPT-ENCODING:',
        'UPGRADE:',
        'SERVER-INTERNALSTATE:',
        'X-POWERED-BY:', HEADER_BEARER_UPPER]) of
      0:
        // 'CONTENT-'
        case IdemPCharArray(P + 8, ['LENGTH:', 'TYPE:', 'ENCODING:']) of
          0:
            // 'CONTENT-LENGTH:'
            ContentLength := GetCardinal(P + 16);
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
                  P := nil;
              end;
            end;
          2:
            // 'CONTENT-ENCODING:'
            if fCompress <> nil then
            begin
              GetTrimmed(P + 17, c);
              for i := 0 to high(fCompress) do
                if fCompress[i].Name = c then
                begin
                  fContentCompress := i;
                  break;
                end;
            end;
        else
          P := nil;
        end;
      1:
        // 'TRANSFER-ENCODING: CHUNKED'
        include(HeaderFlags, hfTransferChuked);
      2:
        // 'CONNECTION: '
        case IdemPCharArray(P + 12, ['CLOSE', 'UPGRADE', 'KEEP-ALIVE']) of
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
              if P[22] = ',' then
              begin
                P := GotoNextNotSpace(P + 23);
                if IdemPChar(P, 'UPGRADE') then
                  // 'CONNECTION: KEEP-ALIVE, UPGRADE'
                  include(HeaderFlags, hfConnectionUpgrade);
              end;
            end;
        else
          P := nil;
        end;
      3:
        // 'ACCEPT-ENCODING:'
        if fCompress <> nil then
          fCompressAcceptHeader := ComputeContentEncoding(fCompress, P + 16)
        else
          P := nil;
      4:
        // 'UPGRADE:'
        GetTrimmed(P + 8, Upgrade);
      5:
        // 'SERVER-INTERNALSTATE:'
        ServerInternalState := GetCardinal(P + 21);
      6:
        // 'X-POWERED-BY:'
        GetTrimmed(P + 13, XPoweredBy);
      7:
        // 'AUTHORIZATION: BEARER '
        begin
          GetTrimmed(P + 22, BearerToken);
          if BearerToken <> '' then
            // allows FindNameValue(..., HEADER_BEARER_UPPER, ...) search
            P := nil;
        end
    else
      // unrecognized name should be stored in Headers
      P := nil;
    end;
    if (P = nil) or
       HeadersUnFiltered then
      // store meaningful headers into SockSend() fSndBuf/Len as temp buffer
      if {%H-}s = '' then
      begin
        len := StrLen(@line);
        if len > SizeOf(line) - 2 then
          break; // avoid buffer overflow
        PWord(@line[len])^ := 13 + 10 shl 8; // include CR + LF
        SockSend(@line, len + 2);
      end
      else
        SockSend(s);
  until false;
  // retrieve meaningful headers from SockSend() fSndBuf/fSndBufLen temp buffer
  Headers := copy(fSndBuf, 1, fSndBufLen);
  fSndBufLen := 0;
end;

procedure THttpSocket.GetBody;
var
  Line: RawUtf8;
  LinePChar: array[0..31] of AnsiChar; // 32 bits chunk length in hexa
  Len, LContent, Error: integer;
begin
  fBodyRetrieved := true;
  Content := '';
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if hfTransferChuked in HeaderFlags then
  begin
    // we ignore the Length
    LContent := 0; // current read position in Content
    repeat
      if SockIn <> nil then
      begin
        readln(SockIn^, LinePChar); // use of a static PChar is faster
        Error := ioresult;
        if Error <> 0 then
          raise EHttpSocket.CreateFmt('GetBody1 ioresult=%d', [Error]);
        Len := HttpChunkToHex32(LinePChar); // get chunk length in hexa
      end
      else
      begin
        SockRecvLn(Line);
        Len := HttpChunkToHex32(pointer(Line)); // get chunk length in hexa
      end;
      if Len = 0 then
      begin
        // ignore next line (normally void)
        SockRecvLn;
        break;
      end;
      SetLength(Content, LContent + Len); // reserve memory space for this chunk
      SockInRead(@PByteArray(Content)[LContent], Len); // append chunk data
      inc(LContent, Len);
      SockRecvLn; // ignore next #13#10
    until false;
  end
  else if ContentLength > 0 then
  begin
    SetLength(Content, ContentLength); // not chuncked: direct read
    SockInRead(pointer(Content), ContentLength); // works with SockIn=nil or not
  end
  else if (ContentLength < 0) and // -1 means no Content-Length header
          IdemPChar(pointer(Command), 'HTTP/1.0 200') then
  begin
    // body = either Content-Length or Transfer-Encoding (HTTP/1.1 RFC2616 4.3)
    if SockIn <> nil then // client loop for compatibility with old servers
      while not eof(SockIn^) do
      begin
        readln(SockIn^, Line);
        if Content = '' then
          Content := Line
        else
          Content := Content + #13#10 + Line;
      end;
    ContentLength := length(Content); // update Content-Length
    exit;
  end;
  // optionaly uncompress content
  if cardinal(fContentCompress) < cardinal(length(fCompress)) then
    if fCompress[fContentCompress].Func(Content, false) = '' then
      // invalid content
      raise EHttpSocket.CreateFmt(
        '%s uncompress', [fCompress[fContentCompress].Name]);
  ContentLength := length(Content); // update Content-Length
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
    Headers := Headers + aValue + #13#10;
end;

procedure THttpSocket.HeaderSetText(const aText: RawUtf8;
  const aForcedContentType: RawUtf8);
begin
  if aText = '' then
    Headers := ''
  else if aText[length(aText) - 1] <> #10 then
    Headers := aText + #13#10
  else
    Headers := aText;
  if (aForcedContentType <> '') and
     (FindNameValue(pointer(aText), 'CONTENT-TYPE:') = nil) then
    Headers := Headers + 'Content-Type: ' + aForcedContentType + #13#10;
end;

function THttpSocket.HeaderGetText(const aRemoteIP: RawUtf8): RawUtf8;
begin
  if (aRemoteIP <> '') and
     not (hfHasRemoteIP in HeaderFlags) then
  begin
    Headers := Headers + 'RemoteIP: ' + aRemoteIP + #13#10;
    include(HeaderFlags, hfHasRemoteIP);
  end;
  result := Headers;
end;

function THttpSocket.HeaderGetValue(const aUpperName: RawUtf8): RawUtf8;
begin
  FindNameValue(Headers, pointer(aUpperName), result, false, ':');
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(
    fCompress, aFunction, fCompressAcceptEncoding, aCompressMinSize) <> '';
end;


{ ******************** Abstract Server-Side Types used e.g. for Client-Server Protocol }

{ THttpServerRequestAbstract }

procedure THttpServerRequestAbstract.AddInHeader(additionalHeader: RawUtf8);
begin
  additionalHeader := TrimU(additionalHeader);
  if additionalHeader <> '' then
    if fInHeaders = '' then
      fInHeaders := additionalHeader
    else
      fInHeaders := fInHeaders + #13#10 + additionalHeader;
end;


end.

