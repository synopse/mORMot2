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

var
  /// THttpRequest timeout default value for DNS resolution
  // - leaving to 0 will let system default value be used
  HTTP_DEFAULT_RESOLVETIMEOUT: integer = 0;
  /// THttpRequest timeout default value for remote connection
  // - default is 60 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  HTTP_DEFAULT_CONNECTTIMEOUT: integer = 60000;
  /// THttpRequest timeout default value for data sending
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_SENDTIMEOUT: integer = 30000;
  /// THttpRequest timeout default value for data receiving
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_RECEIVETIMEOUT: integer = 30000;

const
  /// standard text used to identify the WebSockets protocol
  HTTP_WEBSOCKET_PROTOCOL: RawUTF8 = 'SEC-WEBSOCKET-PROTOCOL';


/// compute the 'Authorization: Bearer ####' HTTP header of a given token value
function AuthorizationBearer(const AuthToken: RawUTF8): RawUTF8;

/// will remove most usual HTTP headers which are to be recomputed on sending
function PurgeHeaders(P: PUTF8Char): RawUTF8;


{$ifndef NOXPOWEREDNAME}
const
  /// pseudo-header containing the current Synopse mORMot framework version
  XPOWEREDNAME = 'X-Powered-By';
  /// the full text of the current Synopse mORMot framework version
  // - we don't supply full version number with build revision
  // (as SYNOPSE_FRAMEWORK_VERSION), to reduce potential attack surface
  XPOWEREDVALUE = SYNOPSE_FRAMEWORK_NAME + ' 2 synopse.info';
{$endif NOXPOWEREDNAME}


{ ******************** THttpSocket Implementing HTTP over plain sockets }

type
  /// exception class raised during HTTP process
  EHttpSocket = class(ENetSock);

  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlzo' or 'synlz')
  // - the data is compressed (if Compress=TRUE) or uncompressed (if
  // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
  // - to be used with THttpSocket.RegisterCompress method
  THttpSocketCompress = function(var Data: RawByteString; Compress: boolean): RawUTF8;

  /// used to maintain a list of known compression algorithms
  THttpSocketCompressRec = record
    /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
    Name: RawUTF8;
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
    fCompressAcceptEncoding: RawUTF8;
    /// GetHeader set index of protocol in fCompress[], from ACCEPT-ENCODING:
    fCompressAcceptHeader: THttpSocketCompressSet;
    /// same as HeaderGetValue('CONTENT-ENCODING'), but retrieved during Request
    // and mapped into the fCompress[] array
    fContentCompress: integer;
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// compress the data, adding corresponding headers via SockSend()
    // - always add a 'Content-Length: ' header entry (even if length=0)
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Data is not '', will add 'Content-Type: ' header
    procedure CompressDataAndWriteHeaders(const OutContentType: RawUTF8;
      var OutContent: RawByteString);
  public
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    TCPPrefix: RawUTF8;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: RawUTF8;
    /// will contain all header lines after a Request
    // - use HeaderGetValue() to get one HTTP header item value by name
    Headers: RawUTF8;
    /// will contain the data retrieved from the server, after the Request
    Content: RawByteString;
    /// same as HeaderGetValue('CONTENT-LENGTH'), but retrieved during Request
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: integer;
    /// same as HeaderGetValue('SERVER-INTERNALSTATE'), but retrieved during Request
    // - proprietary header, used with our RESTful ORM access
    ServerInternalState: integer;
    /// same as HeaderGetValue('CONTENT-TYPE'), but retrieved during Request
    ContentType: RawUTF8;
    /// same as HeaderGetValue('UPGRADE'), but retrieved during Request
    Upgrade: RawUTF8;
    /// same as HeaderGetValue('X-POWERED-BY'), but retrieved during Request
    XPoweredBy: RawUTF8;
    /// map the presence of some HTTP headers, but retrieved during Request
    HeaderFlags: THttpSocketHeaderFlags;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    // - only relevant headers are retrieved, unless HeadersUnFiltered is set
    procedure GetHeader(HeadersUnFiltered: boolean = false);
    /// retrieve the HTTP body (after uncompression if necessary) into Content
    procedure GetBody;
    /// add an header 'name: value' entry
    procedure HeaderAdd(const aValue: RawUTF8);
    /// set all Header values at once, from CRLF delimited text
    procedure HeaderSetText(const aText: RawUTF8; const aForcedContentType: RawUTF8 = '');
    /// get all Header values at once, as CRLF delimited text
    // - you can optionally specify a value to be added as 'RemoteIP: ' header
    function HeaderGetText(const aRemoteIP: RawUTF8 = ''): RawUTF8;
    /// HeaderGetValue('CONTENT-TYPE')='text/html', e.g.
    // - supplied aUpperName should be already uppercased
    function HeaderGetValue(const aUpperName: RawUTF8): RawUTF8;
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


/// adjust HTTP body compression according to the supplied 'CONTENT-TYPE'
function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUTF8;
  var OutContent: RawByteString): RawUTF8;

/// enable a give compression function for a HTTP link
function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: RawUTF8;
  aCompressMinSize: integer): RawUTF8;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PUTF8Char): THttpSocketCompressSet;


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

  /// event handler used by THttpServerGeneric.OnBeforeBody property
  // - if defined, is called just before the body is retrieved from the client
  // - supplied parameters reflect the current input state
  // - should return HTTP_SUCCESS=200 to continue the process, or an HTTP
  // error code (e.g. HTTP_FORBIDDEN or HTTP_PAYLOADTOOLARGE) to reject
  // the request
  TOnHttpServerBeforeBody = function(const aURL, aMethod, aInHeaders,
    aInContentType, aRemoteIP: RawUTF8; aContentLength: integer;
    aUseSSL: boolean): cardinal of object;

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

  /// abstract generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  // - this abstract class may be used in communication protocols, without
  // the need to add mormot.net.server.pas dependency
  THttpServerRequestAbstract = class
  protected
    fRemoteIP,
    fURL,
    fMethod,
    fInHeaders,
    fInContentType,
    fAuthenticatedUser,
    fOutContentType,
    fOutCustomHeaders: RawUTF8;
    fInContent, fOutContent: RawByteString;
    fRequestID: integer;
    fConnectionID: THttpServerConnectionID;
    fUseSSL: boolean;
    fAuthenticationStatus: THttpServerRequestAuthentication;
  public
    /// low-level property which may be used during requests processing
    Status: integer;
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aURL, aMethod, aInHeaders: RawUTF8;
      const aInContent: RawByteString; const aInContentType, aRemoteIP: RawUTF8;
      aUseSSL: boolean = false); virtual; abstract;
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(additionalHeader: RawUTF8);
    /// input parameter containing the caller URI
    property URL: RawUTF8 read fURL;
    /// input parameter containing the caller method (GET/POST...)
    property Method: RawUTF8 read fMethod;
    /// input parameter containing the caller message headers
    property InHeaders: RawUTF8 read fInHeaders;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    property InContent: RawByteString
      read fInContent;
    // input parameter defining the caller message body content type
    property InContentType: RawUTF8
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
    property OutContentType: RawUTF8
      read fOutContentType write fOutContentType;
    /// output parameter to be sent back as the response message header
    // - e.g. to set Content-Type/Location
    property OutCustomHeaders: RawUTF8
      read fOutCustomHeaders write fOutCustomHeaders;
    /// the client remote IP, as specified to Prepare()
    property RemoteIP: RawUTF8
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
    /// is TRUE if the caller is connected via HTTPS
    // - only set for THttpApiServer class yet
    property UseSSL: boolean
      read fUseSSL;
    /// contains the THttpServer-side authentication status
    // - e.g. when using http.sys authentication with HTTP API 2.0
    property AuthenticationStatus: THttpServerRequestAuthentication
      read fAuthenticationStatus;
    /// contains the THttpServer-side authenticated user name, UTF-8 encoded
    // - e.g. when using http.sys authentication with HTTP API 2.0, the
    // domain user name is retrieved from the supplied AccessToken
    // - could also be set by the THttpServerGeneric.Request() method, after
    // proper authentication, so that it would be logged as expected
    property AuthenticatedUser: RawUTF8
      read fAuthenticatedUser;
  end;



implementation


{ ******************** Shared HTTP Constants and Functions }

function AuthorizationBearer(const AuthToken: RawUTF8): RawUTF8;
begin
  if AuthToken = '' then
    result := ''
  else
    result := 'Authorization: Bearer ' + AuthToken;
end;

function PurgeHeaders(P: PUTF8Char): RawUTF8;
var
  tmp: TTextWriterStackBuffer;
  next: PUTF8Char;
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
          W.AddNoJSONEscape(P)
        else
          W.AddNoJSONEscape(P, next - P);
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
  aFunction: THttpSocketCompress; var aAcceptEncoding: RawUTF8;
  aCompressMinSize: integer): RawUTF8;
var
  i, n: PtrInt;
  dummy: RawByteString;
  aName: RawUTF8;
begin
  result := '';
  if @aFunction = nil then
    exit;
  n := length(Compress);
  aName := aFunction(dummy, {compress}true); // just retrieve algo name
  for i := 0 to n - 1 do
    with Compress[i] do
      if Name = aName then
      begin // already set
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
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: RawUTF8;
  var OutContent: RawByteString): RawUTF8;
var
  i, OutContentLen: integer;
  compressible: boolean;
  OutContentTypeP: PUTF8Char absolute OutContentType;
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
        compressible := IdemPCharArray(OutContentTypeP + 6, ['SVG', 'X-ICO']) >= 0;
      2:
        compressible := IdemPCharArray(OutContentTypeP + 12,
          ['JSON', 'XML', 'JAVASCRIPT']) >= 0;
    else
      compressible := false;
    end;
    for i := 0 to high(Handled) do
      if i in Accepted then
        with Handled[i] do
          if (CompressMinSize = 0) or // 0 here means "always" (e.g. for encryption)
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
  P: PUTF8Char): THttpSocketCompressSet;
var
  i: PtrInt;
  aName: RawUTF8;
  Beg: PUTF8Char;
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

procedure GetTrimmed(P: PUTF8Char; out result: RawUTF8);
var
  B: PUTF8Char;
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
      if v0 > 15 then
        break; // not in '0'..'9','a'..'f'
      v1 := ConvertHexToBin[ord(p[1])];
      inc(p);
      if v1 > 15 then
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

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: RawUTF8;
  var OutContent: RawByteString);
var
  OutContentEncoding: RawUTF8;
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

procedure THttpSocket.GetHeader(HeadersUnFiltered: boolean);
var
  s, c: RawUTF8;
  i, len: PtrInt;
  err: integer;
  P: PUTF8Char;
  line: array[0..4095] of AnsiChar; // avoid most memory allocation
begin
  HeaderFlags := [];
  fBodyRetrieved := false;
  fContentCompress := -1;
  integer(fCompressAcceptHeader) := 0;
  ContentType := '';
  Upgrade := '';
  ContentLength := -1;
  ServerInternalState := 0;
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
        raise EHttpSocket.CreateFmt('%s.GetHeader error=%d', [ClassName, err]);
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
    case IdemPCharArray(P, ['CONTENT-', 'TRANSFER-ENCODING: CHUNKED',
      'CONNECTION: ', 'ACCEPT-ENCODING:', 'UPGRADE:', 'SERVER-INTERNALSTATE:',
      'X-POWERED-BY:']) of
      0:
        case IdemPCharArray(P + 8, ['LENGTH:', 'TYPE:', 'ENCODING:']) of
          0:
            ContentLength := GetCardinal(P + 16);
          1:
            begin
              inc(P, 13);
              while P^ = ' ' do
                inc(P);
              if IdemPChar(P, 'APPLICATION/JSON') then
                ContentType := JSON_CONTENT_TYPE_VAR
              else
              begin
                GetTrimmed(P, ContentType);
                if ContentType <> '' then
                  P := nil; // is searched by HEADER_CONTENT_TYPE_UPPER later on
              end;
            end;
          2:
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
        include(HeaderFlags, hfTransferChuked);
      2:
        case IdemPCharArray(P + 12, ['CLOSE', 'UPGRADE', 'KEEP-ALIVE']) of
          0:
            include(HeaderFlags, hfConnectionClose);
          1:
            include(HeaderFlags, hfConnectionUpgrade);
          2:
            begin
              include(HeaderFlags, hfConnectionKeepAlive);
              if P[22] = ',' then
              begin
                inc(P, 23);
                if P^ = ' ' then
                  inc(P);
                if IdemPChar(P, 'UPGRADE') then
                  include(HeaderFlags, hfConnectionUpgrade);
              end;
            end;
        else
          P := nil;
        end;
      3:
        if fCompress <> nil then
          fCompressAcceptHeader := ComputeContentEncoding(fCompress, P + 16)
        else
          P := nil;
      4:
        GetTrimmed(P + 8, Upgrade);
      5:
        ServerInternalState := GetCardinal(P + 21);
      6:
        GetTrimmed(P + 13, XPoweredBy);
    else
      P := nil;
    end;
    if (P = nil) or
       HeadersUnFiltered then
      // store meaningful headers into SockSend() fSndBuf/fSndLen as temp buffer
      if {%H-}s = '' then
      begin
        len := StrLen(@line);
        if len > SizeOf(line) - 2 then
          break; // avoid buffer overflow
        PWord(@line[len])^ := 13 + 10 shl 8; // CR + LF
        SockSend(@line, len + 2);
      end
      else
        SockSend(s);
  until false;
  Headers := copy(fSndBuf, 1, fSndBufLen);
  fSndBufLen := 0;
end;

procedure THttpSocket.GetBody;
var
  Line: RawUTF8; // 32 bits chunk length in hexa
  LinePChar: array[0..31] of AnsiChar;
  Len, LContent, Error: integer;
begin
  fBodyRetrieved := true;
  Content := '';
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if hfTransferChuked in HeaderFlags then
  begin // we ignore the Length
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
      begin // ignore next line (normally void)
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
  else if ContentLength < 0 then // ContentLength=-1 if no Content-Length
  begin
    // no Content-Length nor Chunked header -> read until eof()
    if SockIn <> nil then
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
      raise EHttpSocket.CreateFmt('%s uncompress', [fCompress[fContentCompress].Name]);
  ContentLength := length(Content); // update Content-Length
  {$ifdef SYNCRTDEBUGLOW}
  TSynLog.Add.Log(sllCustom2, 'GetBody sock=% pending=% sockin=% len=% %',
    [fSock, SockInPending(0), PTextRec(SockIn)^.BufEnd - PTextRec(SockIn)^.bufpos,
    ContentLength, LogEscapeFull(Content)], self);
  {$endif}
  if SockIn <> nil then
  begin
    Error := ioresult;
    if Error <> 0 then
      raise EHttpSocket.CreateFmt('GetBody2 ioresult=%d', [Error]);
  end;
  {$I+}
end;

procedure THttpSocket.HeaderAdd(const aValue: RawUTF8);
begin
  if aValue <> '' then
    Headers := Headers + aValue + #13#10;
end;

procedure THttpSocket.HeaderSetText(const aText, aForcedContentType: RawUTF8);
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

function THttpSocket.HeaderGetText(const aRemoteIP: RawUTF8): RawUTF8;
begin
  if (aRemoteIP <> '') and
     not (hfHasRemoteIP in HeaderFlags) then
  begin
    Headers := Headers + 'RemoteIP: ' + aRemoteIP + #13#10;
    include(HeaderFlags, hfHasRemoteIP);
  end;
  result := Headers;
end;

function THttpSocket.HeaderGetValue(const aUpperName: RawUTF8): RawUTF8;
begin
  FindNameValue(Headers, pointer(aUpperName), result);
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(
    fCompress, aFunction, fCompressAcceptEncoding, aCompressMinSize) <> '';
end;


{ ******************** Abstract Server-Side Types used e.g. for Client-Server Protocol }

{ THttpServerRequestAbstract }

procedure THttpServerRequestAbstract.AddInHeader(additionalHeader: RawUTF8);
begin
  additionalHeader := Trim(additionalHeader);
  if additionalHeader <> '' then
    if fInHeaders = '' then
      fInHeaders := additionalHeader
    else
      fInHeaders := fInHeaders + #13#10 + additionalHeader;
end;


end.

