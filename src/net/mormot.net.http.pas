/// HTTP/HTTPS Abstract Process Classes and Definitions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.http;

{
  *****************************************************************************

   HTTP/HTTPS Abstract Process Classes and Definitions
   - Shared HTTP Constants and Functions
   - THttpSocket Implementing HTTP over plain sockets

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
  /// void HTTP Status Code (not a standard value, for internal use only)
  HTTP_NONE = 0;
  /// HTTP Status Code for "Continue"
  HTTP_CONTINUE = 100;
  /// HTTP Status Code for "Switching Protocols"
  HTTP_SWITCHINGPROTOCOLS = 101;
  /// HTTP Status Code for "Success"
  HTTP_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  HTTP_CREATED = 201;
  /// HTTP Status Code for "Accepted"
  HTTP_ACCEPTED = 202;
  /// HTTP Status Code for "Non-Authoritative Information"
  HTTP_NONAUTHORIZEDINFO = 203;
  /// HTTP Status Code for "No Content"
  HTTP_NOCONTENT = 204;
  /// HTTP Status Code for "Reset Content"
  HTTP_RESETCONTENT = 205;
  /// HTTP Status Code for "Partial Content"
  HTTP_PARTIALCONTENT = 206;
  /// HTTP Status Code for "Multiple Choices"
  HTTP_MULTIPLECHOICES = 300;
  /// HTTP Status Code for "Moved Permanently"
  HTTP_MOVEDPERMANENTLY = 301;
  /// HTTP Status Code for "Found"
  HTTP_FOUND = 302;
  /// HTTP Status Code for "See Other"
  HTTP_SEEOTHER = 303;
  /// HTTP Status Code for "Not Modified"
  HTTP_NOTMODIFIED = 304;
  /// HTTP Status Code for "Use Proxy"
  HTTP_USEPROXY = 305;
  /// HTTP Status Code for "Temporary Redirect"
  HTTP_TEMPORARYREDIRECT = 307;
  /// HTTP Status Code for "Bad Request"
  HTTP_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  HTTP_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  HTTP_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  HTTP_NOTFOUND = 404;
  // HTTP Status Code for "Method Not Allowed"
  HTTP_NOTALLOWED = 405;
  // HTTP Status Code for "Not Acceptable"
  HTTP_NOTACCEPTABLE = 406;
  // HTTP Status Code for "Proxy Authentication Required"
  HTTP_PROXYAUTHREQUIRED = 407;
  /// HTTP Status Code for "Request Time-out"
  HTTP_TIMEOUT = 408;
  /// HTTP Status Code for "Conflict"
  HTTP_CONFLICT = 409;
  /// HTTP Status Code for "Payload Too Large"
  HTTP_PAYLOADTOOLARGE = 413;
  /// HTTP Status Code for "Internal Server Error"
  HTTP_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  HTTP_NOTIMPLEMENTED = 501;
  /// HTTP Status Code for "Bad Gateway"
  HTTP_BADGATEWAY = 502;
  /// HTTP Status Code for "Service Unavailable"
  HTTP_UNAVAILABLE = 503;
  /// HTTP Status Code for "Gateway Timeout"
  HTTP_GATEWAYTIMEOUT = 504;
  /// HTTP Status Code for "HTTP Version Not Supported"
  HTTP_HTTPVERSIONNONSUPPORTED = 505;

  /// standard text used to identify the WebSockets protocol
  HTTP_WEBSOCKET_PROTOCOL: RawUTF8 = 'SEC-WEBSOCKET-PROTOCOL';


/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
// - see http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - mORMot.StatusCodeToErrorMsg() will call this function
function StatusCodeToReason(Code: cardinal): RawUTF8;

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
    HeaderFlags: set of (transferChuked, connectionClose, connectionUpgrade,
      connectionKeepAlive, hasRemoteIP);
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


implementation

{ ******************** Shared HTTP Constants and Functions }

var
  ReasonCache: array[1..5, 0..13] of RawUTF8; // avoid memory allocation

function StatusCodeToReasonInternal(Code: cardinal): RawUTF8;
begin
  case Code of
    100:
      result := 'Continue';
    101:
      result := 'Switching Protocols';
    200:
      result := 'OK';
    201:
      result := 'Created';
    202:
      result := 'Accepted';
    203:
      result := 'Non-Authoritative Information';
    204:
      result := 'No Content';
    205:
      result := 'Reset Content';
    206:
      result := 'Partial Content';
    207:
      result := 'Multi-Status';
    300:
      result := 'Multiple Choices';
    301:
      result := 'Moved Permanently';
    302:
      result := 'Found';
    303:
      result := 'See Other';
    304:
      result := 'Not Modified';
    305:
      result := 'Use Proxy';
    307:
      result := 'Temporary Redirect';
    308:
      result := 'Permanent Redirect';
    400:
      result := 'Bad Request';
    401:
      result := 'Unauthorized';
    403:
      result := 'Forbidden';
    404:
      result := 'Not Found';
    405:
      result := 'Method Not Allowed';
    406:
      result := 'Not Acceptable';
    407:
      result := 'Proxy Authentication Required';
    408:
      result := 'Request Timeout';
    409:
      result := 'Conflict';
    410:
      result := 'Gone';
    411:
      result := 'Length Required';
    412:
      result := 'Precondition Failed';
    413:
      result := 'Payload Too Large';
    414:
      result := 'URI Too Long';
    415:
      result := 'Unsupported Media Type';
    416:
      result := 'Requested Range Not Satisfiable';
    426:
      result := 'Upgrade Required';
    500:
      result := 'Internal Server Error';
    501:
      result := 'Not Implemented';
    502:
      result := 'Bad Gateway';
    503:
      result := 'Service Unavailable';
    504:
      result := 'Gateway Timeout';
    505:
      result := 'HTTP Version Not Supported';
    511:
      result := 'Network Authentication Required';
  else
    result := 'Invalid Request';
  end;
end;

function StatusCodeToReason(Code: cardinal): RawUTF8;
var
  Hi, Lo: cardinal;
begin
  if Code = 200 then
  begin
    // optimistic approach :)
    Hi := 2;
    Lo := 0;
  end
  else
  begin
    Hi := Code div 100;
    Lo := Code - Hi * 100;
    if not ((Hi in [1..5]) and (Lo in [0..13])) then
    begin
      result := StatusCodeToReasonInternal(Code);
      exit;
    end;
  end;
  result := ReasonCache[Hi, Lo];
  if result <> '' then
    exit;
  result := StatusCodeToReasonInternal(Code);
  ReasonCache[Hi, Lo] := result;
end;

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
  if (integer(Accepted) <> 0) and (OutContentType <> '') and (Handled <> nil) then
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
             (compressible and (OutContentLen >= CompressMinSize)) then
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
  while (P^ > #0) and (P^ <= ' ') do
    inc(P);
  B := P;
  while P^ <> #0 do
    inc(P);
  while (P > B) and (P[-1] <= ' ') do
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
  if (OutContentType <> '') and (OutContentType <> STATICFILE_CONTENT_TYPE) then
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
  fSndBufLen := 0; // SockSend() internal buffer is used when adding headers
  repeat
    P := @line;
    if (SockIn <> nil) and not HeadersUnFiltered then
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
      P := pointer(s); // set P=nil below to store in Headers[]
    end;
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
        include(HeaderFlags, transferChuked);
      2:
        case IdemPCharArray(P + 12, ['CLOSE', 'UPGRADE', 'KEEP-ALIVE']) of
          0:
            include(HeaderFlags, connectionClose);
          1:
            include(HeaderFlags, connectionUpgrade);
          2:
            begin
              include(HeaderFlags, connectionKeepAlive);
              if P[22] = ',' then
              begin
                inc(P, 23);
                if P^ = ' ' then
                  inc(P);
                if IdemPChar(P, 'UPGRADE') then
                  include(HeaderFlags, connectionUpgrade);
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
    if (P = nil) or HeadersUnFiltered then // only store meaningful headers
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
  if transferChuked in HeaderFlags then
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
  if (aRemoteIP <> '') and not (hasRemoteIP in HeaderFlags) then
  begin
    Headers := Headers + 'RemoteIP: ' + aRemoteIP + #13#10;
    include(HeaderFlags, hasRemoteIP);
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


initialization

finalization

end.

