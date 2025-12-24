unit middleware.trace;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.rest.core,
  mormot.rest.server;

type
  /// Trace middleware that logs detailed request/response information
  // Port of DMVC TMVCTraceMiddleware
  TTraceMiddleware = class
  private
    fServer: TRestServer;
    fMaxBodySize: Integer;

    /// OnBeforeUri - called BEFORE request execution
    function OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;

    /// OnAfterUri - called AFTER request execution
    procedure OnAfterUri(Ctxt: TRestServerUriContext);

    function ExtractContentPreview(const aContent: RawByteString;
      const aContentType: RawUtf8): RawUtf8;
  public
    /// Creates the trace middleware and attaches to server events
    constructor Create(aServer: TRestServer; aMaxBodySize: Integer = 1024);
    destructor Destroy; override;
  end;

implementation

{ TTraceMiddleware }

constructor TTraceMiddleware.Create(aServer: TRestServer; aMaxBodySize: Integer);
begin
  inherited Create;

  fServer := aServer;
  fMaxBodySize := aMaxBodySize;

  // Attach middleware events to server
  // Port of DMVC OnBeforeRouting/OnAfterControllerAction
  fServer.OnBeforeUri := OnBeforeUri;
  fServer.OnAfterUri := OnAfterUri;

  TSynLog.Add.Log(sllInfo, 'Trace middleware initialized - max body size: % bytes', [fMaxBodySize]);
end;

destructor TTraceMiddleware.Destroy;
begin
  // Detach events
  if fServer <> nil then
  begin
    fServer.OnBeforeUri := nil;
    fServer.OnAfterUri := nil;
  end;

  inherited;
end;

function TTraceMiddleware.OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
var
  userAgent, accept, authorization, contentType: RawUtf8;
  bodyPreview: RawUtf8;
  method: RawUtf8;
begin
  // Port of DMVC OnBeforeRouting
  // Access data via Ctxt.Call^ pointer

  // Extract headers from incoming request
  FindNameValue(Ctxt.Call^.InHead, 'USER-AGENT:', userAgent);
  FindNameValue(Ctxt.Call^.InHead, 'ACCEPT:', accept);
  FindNameValue(Ctxt.Call^.InHead, 'AUTHORIZATION:', authorization);
  FindNameValue(Ctxt.Call^.InHead, 'CONTENT-TYPE:', contentType);

  // Get method name
  method := UpperCase(Ctxt.Call^.Method);

  // Log request information
  TSynLog.Add.Log(sllTrace,
    '[BEFORE ROUTING][%][IP: %][URL: %][LENGTH: %][ACCEPT: %][USER-AGENT: %][AUTHORIZATION: %]',
    [method, Ctxt.Call^.LowLevelRemoteIP, Ctxt.Call^.Url, Length(Ctxt.Call^.InBody),
     accept, userAgent,
     IdemPChar(pointer(authorization), 'BEARER ') or IdemPChar(pointer(authorization), 'BASIC ')]);

  // Log request body for POST/PUT
  if (method = 'POST') or (method = 'PUT') then
  begin
    bodyPreview := ExtractContentPreview(Ctxt.Call^.InBody, contentType);
    TSynLog.Add.Log(sllTrace, '[BEFORE ROUTING][REQUEST][BODY] %', [bodyPreview]);
  end;

  // Don't block request (return False = continue processing)
  Result := False;
end;

procedure TTraceMiddleware.OnAfterUri(Ctxt: TRestServerUriContext);
var
  statusText: RawUtf8;
  customHeaders: RawUtf8;
  bodyPreview: RawUtf8;
  contentType: RawUtf8;
  actionName: RawUtf8;
begin
  // Port of DMVC OnAfterControllerAction
  // Access data via Ctxt.Call^ pointer

  // Build action name (service.method)
  if Ctxt.Service <> nil then
    actionName := FormatUtf8('%.%', [Ctxt.Service.ClassName, Ctxt.ServiceMethodIndex])
  else
    actionName := 'Unknown';

  TSynLog.Add.Log(sllTrace, '[BEFORE ACTION][CONTROLLER: %][ACTION: %]',
    [Ctxt.Service.ClassName, actionName]);

  // Map status code to text
  case Ctxt.Call^.OutStatus of
    200: statusText := 'OK';
    201: statusText := 'Created';
    204: statusText := 'No Content';
    400: statusText := 'Bad Request';
    401: statusText := 'Unauthorized';
    404: statusText := 'Not Found';
    500: statusText := 'Internal Server Error';
  else
    statusText := 'Status ' + UInt32ToUtf8(Ctxt.Call^.OutStatus);
  end;

  TSynLog.Add.Log(sllTrace, '[AFTER ACTION][RESPONSE][STATUS] %: %',
    [Ctxt.Call^.OutStatus, statusText]);

  // Log custom headers
  if Ctxt.Call^.OutHead <> '' then
  begin
    customHeaders := StringReplaceAll(Ctxt.Call^.OutHead, #13#10, ' | ');
    TSynLog.Add.Log(sllTrace, '[AFTER ACTION][RESPONSE][CUSTOM HEADERS] %',
      [customHeaders]);
  end;

  // Log content type
  FindNameValue(Ctxt.Call^.OutHead, 'CONTENT-TYPE:', contentType);
  if contentType = '' then
    contentType := 'application/json'; // default

  TSynLog.Add.Log(sllTrace, '[AFTER ACTION][RESPONSE][CONTENT-TYPE] %',
    [contentType]);

  // Log response body preview
  bodyPreview := ExtractContentPreview(Ctxt.Call^.OutBody, contentType);
  TSynLog.Add.Log(sllTrace, '[AFTER ACTION][RESPONSE][BODY] %',
    [bodyPreview]);
end;

function TTraceMiddleware.ExtractContentPreview(const aContent: RawByteString;
  const aContentType: RawUtf8): RawUtf8;
var
  len: Integer;
begin
  // Port of DMVC content extraction logic
  if aContent = '' then
  begin
    Result := '<empty>';
    Exit;
  end;

  len := Length(aContent);

  // Check if content is text-based (JSON, XML, text, form data)
  if IdemPChar(pointer(aContentType), 'APPLICATION/JSON') or
     IdemPChar(pointer(aContentType), 'APPLICATION/XML') or
     IdemPChar(pointer(aContentType), 'TEXT/') or
     IdemPChar(pointer(aContentType), 'APPLICATION/X-WWW-FORM-URLENCODED') then
  begin
    // Return preview of text content
    if len > fMaxBodySize then
      Result := Copy(aContent, 1, fMaxBodySize) + FormatUtf8(' ... (% bytes total)', [len])
    else
      Result := aContent;
  end
  else
  begin
    // Binary content
    Result := FormatUtf8('<binary content: % bytes>', [len]);
  end;
end;

end.
