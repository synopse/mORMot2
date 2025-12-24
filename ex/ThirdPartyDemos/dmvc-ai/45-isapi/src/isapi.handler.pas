unit isapi.handler;

{$I mormot.defines.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.json,
  mormot.net.http,
  mormot.rest.core,
  mormot.rest.server;

type
  /// ISAPI EXTENSION_CONTROL_BLOCK structure
  TEXTENSION_CONTROL_BLOCK = record
    cbSize: DWORD;
    dwVersion: DWORD;
    ConnID: Pointer;
    dwHttpStatusCode: DWORD;
    lpszLogData: array[0..79] of AnsiChar;
    lpszMethod: PAnsiChar;
    lpszQueryString: PAnsiChar;
    lpszPathInfo: PAnsiChar;
    lpszPathTranslated: PAnsiChar;
    cbTotalBytes: DWORD;
    cbAvailable: DWORD;
    lpbData: Pointer;
    lpszContentType: PAnsiChar;
    GetServerVariable: function(ConnID: Pointer; VariableName: PAnsiChar;
      Buffer: Pointer; var Size: DWORD): BOOL; stdcall;
    WriteClient: function(ConnID: Pointer; Buffer: Pointer; var Bytes: DWORD;
      dwReserved: DWORD): BOOL; stdcall;
    ReadClient: function(ConnID: Pointer; Buffer: Pointer;
      var Size: DWORD): BOOL; stdcall;
    ServerSupportFunction: function(ConnID: Pointer; dwHSERequest: DWORD;
      lpvBuffer: Pointer; lpdwSize: LPDWORD; lpdwDataType: LPDWORD): BOOL; stdcall;
  end;
  PEXTENSION_CONTROL_BLOCK = ^TEXTENSION_CONTROL_BLOCK;

  /// ISAPI request handler that bridges requests to mORMot2 REST server
  /// This handler translates IIS ISAPI requests to mORMot2's REST processing
  TIsapiHandler = class
  private
    fServer: TRestServer;
    function GetServerVariable(ecb: PEXTENSION_CONTROL_BLOCK; const VarName: RawUtf8): RawUtf8;
    function ReadRequestBody(ecb: PEXTENSION_CONTROL_BLOCK): RawByteString;
    procedure SendResponse(ecb: PEXTENSION_CONTROL_BLOCK;
      Status: integer; const Headers, Body: RawUtf8);
  public
    constructor Create(aServer: TRestServer);
    /// Process ISAPI request by forwarding to mORMot REST server
    function ProcessRequest(ecb: PEXTENSION_CONTROL_BLOCK): DWORD;
  end;


implementation


{ TIsapiHandler }

constructor TIsapiHandler.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;
end;

function TIsapiHandler.GetServerVariable(ecb: PEXTENSION_CONTROL_BLOCK;
  const VarName: RawUtf8): RawUtf8;
var
  Buffer: array[0..4095] of AnsiChar;
  Size: DWORD;
  VarNameAnsi: AnsiString;
begin
  result := '';
  VarNameAnsi := AnsiString(VarName);
  Size := SizeOf(Buffer);
  FillChar(Buffer, SizeOf(Buffer), 0);
  if ecb.GetServerVariable(ecb.ConnID, PAnsiChar(VarNameAnsi), @Buffer[0], Size) then
    result := RawUtf8(Buffer);
end;

function TIsapiHandler.ReadRequestBody(ecb: PEXTENSION_CONTROL_BLOCK): RawByteString;
var
  BytesRead: DWORD;
  TotalRead: DWORD;
  Buffer: array[0..8191] of Byte;
begin
  result := '';
  if ecb.cbTotalBytes = 0 then
    exit;

  // First, get data already available
  if ecb.cbAvailable > 0 then
    FastSetRawByteString(result, ecb.lpbData, ecb.cbAvailable);

  TotalRead := ecb.cbAvailable;

  // Read remaining data if any
  while TotalRead < ecb.cbTotalBytes do
  begin
    BytesRead := SizeOf(Buffer);
    if ecb.cbTotalBytes - TotalRead < BytesRead then
      BytesRead := ecb.cbTotalBytes - TotalRead;

    if not ecb.ReadClient(ecb.ConnID, @Buffer, BytesRead) then
      break;

    if BytesRead = 0 then
      break;

    Append(result, @Buffer, BytesRead);
    Inc(TotalRead, BytesRead);
  end;
end;

procedure TIsapiHandler.SendResponse(ecb: PEXTENSION_CONTROL_BLOCK;
  Status: integer; const Headers, Body: RawUtf8);
var
  ResponseText: RawUtf8;
  StatusLine: RawUtf8;
  BytesWritten: DWORD;
begin
  // Build status line
  case Status of
    200: StatusLine := 'HTTP/1.1 200 OK';
    201: StatusLine := 'HTTP/1.1 201 Created';
    204: StatusLine := 'HTTP/1.1 204 No Content';
    400: StatusLine := 'HTTP/1.1 400 Bad Request';
    401: StatusLine := 'HTTP/1.1 401 Unauthorized';
    403: StatusLine := 'HTTP/1.1 403 Forbidden';
    404: StatusLine := 'HTTP/1.1 404 Not Found';
    405: StatusLine := 'HTTP/1.1 405 Method Not Allowed';
    500: StatusLine := 'HTTP/1.1 500 Internal Server Error';
  else
    StatusLine := FormatUtf8('HTTP/1.1 % Unknown', [Status]);
  end;

  // Build full response
  ResponseText := StatusLine + #13#10 + Headers;
  if (Body <> '') and (Pos('Content-Length', Headers) = 0) then
    ResponseText := ResponseText + 'Content-Length: ' + UInt32ToUtf8(Length(Body)) + #13#10;
  ResponseText := ResponseText + #13#10 + Body;

  // Send to client
  BytesWritten := Length(ResponseText);
  ecb.WriteClient(ecb.ConnID, Pointer(ResponseText), BytesWritten, 0);
end;

function TIsapiHandler.ProcessRequest(ecb: PEXTENSION_CONTROL_BLOCK): DWORD;
var
  Method, Url, ContentType: RawUtf8;
  InBody: RawByteString;
  InHeaders: RawUtf8;
  OutStatus: integer;
  OutBody: RawUtf8;
  OutHeaders: RawUtf8;
  Call: TRestUriParams;
  MethodStr, UrlStr: AnsiString;
begin
  try
    // Extract HTTP method
    MethodStr := ecb.lpszMethod;
    Method := RawUtf8(MethodStr);

    // Build URL from PathInfo and QueryString
    UrlStr := ecb.lpszPathInfo;
    if (ecb.lpszQueryString <> nil) and (ecb.lpszQueryString[0] <> #0) then
      UrlStr := UrlStr + '?' + AnsiString(ecb.lpszQueryString);
    Url := RawUtf8(UrlStr);

    // Get content type
    if ecb.lpszContentType <> nil then
      ContentType := RawUtf8(ecb.lpszContentType)
    else
      ContentType := '';

    // Read request body
    InBody := ReadRequestBody(ecb);

    // Build input headers
    InHeaders := '';
    if ContentType <> '' then
      InHeaders := 'Content-Type: ' + ContentType + #13#10;

    // Get additional headers from IIS
    InHeaders := InHeaders + 'X-Remote-IP: ' + GetServerVariable(ecb, 'REMOTE_ADDR') + #13#10;

    TSynLog.Add.Log(sllDebug, 'ISAPI Request: % % [% bytes]',
      [Method, Url, Length(InBody)]);

    // Initialize REST call parameters
    FillCharFast(Call, SizeOf(Call), 0);
    Call.Method := Method;
    Call.Url := Url;
    Call.InHead := InHeaders;
    Call.InBody := InBody;

    // Process through mORMot2 REST server
    fServer.Uri(Call);

    // Extract response
    OutStatus := Call.OutStatus;
    OutBody := Call.OutBody;
    OutHeaders := Call.OutHead;

    // Default headers if none provided
    if OutHeaders = '' then
      OutHeaders := 'Content-Type: application/json' + #13#10 +
                    'Server: mORMot2-ISAPI/1.0' + #13#10;

    TSynLog.Add.Log(sllDebug, 'ISAPI Response: % [% bytes]',
      [OutStatus, Length(OutBody)]);

    // Send response to client
    SendResponse(ecb, OutStatus, OutHeaders, OutBody);

    result := OutStatus;

  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'ISAPI ProcessRequest error: %', [E.Message]);

      // Send error response
      SendResponse(ecb, 500,
        'Content-Type: application/json' + #13#10,
        JsonEncode(['error', E.Message, 'type', E.ClassName]));

      result := 500;
    end;
  end;
end;


end.
