unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.threads,
  mormot.net.http,
  mormot.net.sock,
  mormot.net.server,
  storage;

type
  /// Custom HTTP server with true Server-Sent Events streaming support
  // Implements SSE per W3C specification: https://html.spec.whatwg.org/multipage/server-sent-events.html
  TSSEHttpServer = class(THttpServer)
  private
    fWwwPath: TFileName;
    fActiveSSEConnections: integer;
  protected
    /// Override Process to intercept SSE requests before normal handling
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID;
      ConnectionThread: TSynThread); override;
    /// Handle true SSE streaming - keeps connection open and pushes events
    procedure HandleSSEStream(ClientSock: THttpServerSocket);
    /// Serve static files from www folder
    function ServeStaticFile(ClientSock: THttpServerSocket): boolean;
  public
    property WwwPath: TFileName read fWwwPath write fWwwPath;
    property ActiveSSEConnections: integer read fActiveSSEConnections;
  end;

  /// Server-Sent Events sample server
  TSSESampleServer = class
  private
    fHttpServer: TSSEHttpServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property HttpServer: TSSEHttpServer read fHttpServer;
  end;

implementation

const
  SSE_HEADERS =
    'HTTP/1.1 200 OK'#13#10 +
    'Content-Type: text/event-stream; charset=utf-8'#13#10 +
    'Cache-Control: no-cache'#13#10 +
    'Connection: keep-alive'#13#10 +
    'Access-Control-Allow-Origin: *'#13#10 +
    'X-Accel-Buffering: no'#13#10 +  // Disable nginx buffering
    #13#10;  // End of headers

{ TSSEHttpServer }

procedure TSSEHttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var
  url: RawUtf8;
begin
  if (ClientSock = nil) or
     (ClientSock.Http.Headers = '') or
     Terminated then
    exit;

  url := ClientSock.Http.CommandUri;

  // Check for SSE endpoint: GET /root/stocks with Accept: text/event-stream
  if IsGet(ClientSock.Method) and
     (IdemPropNameU(url, '/ROOT/STOCKS') or IdemPropNameU(url, '/STOCKS')) then
  begin
    // Handle as true SSE streaming
    HandleSSEStream(ClientSock);
    // After SSE ends, don't keep connection alive
    ClientSock.KeepAliveClient := false;
    Exit;
  end;

  // Check for static files: GET /root/static/*
  if IsGet(ClientSock.Method) and
     (IdemPChar(pointer(url), '/ROOT/STATIC/') or IdemPChar(pointer(url), '/STATIC/')) then
  begin
    if ServeStaticFile(ClientSock) then
      Exit;
  end;

  // Default: normal HTTP processing
  inherited Process(ClientSock, ConnectionID, ConnectionThread);
end;

procedure TSSEHttpServer.HandleSSEStream(ClientSock: THttpServerSocket);
var
  lastEventID: Integer;
  currentEventID: Integer;
  stockData: RawUtf8;
  sseEvent: RawUtf8;
  lastIDHeader: RawUtf8;
  sent: boolean;
  remoteIP: RawUtf8;
  headers: RawByteString;
begin
  // Get client IP
  remoteIP := ClientSock.RemoteIP;

  // Get Last-Event-ID from header (for reconnection support)
  FindNameValue(ClientSock.Http.Headers, 'LAST-EVENT-ID:', lastIDHeader);
  if lastIDHeader <> '' then
    lastEventID := Utf8ToInteger(Trim(lastIDHeader))
  else
    lastEventID := 0;

  TSynLog.Add.Log(sllInfo, 'SSE stream started for client %, Last-Event-ID=%',
    [remoteIP, lastEventID]);

  LockedInc32(@fActiveSSEConnections);
  try
    // Send SSE headers directly to socket
    headers := SSE_HEADERS;
    sent := ClientSock.TrySndLow(pointer(headers), Length(headers));
    if not sent then
    begin
      TSynLog.Add.Log(sllWarning, 'SSE: Failed to send headers to %',
        [remoteIP]);
      Exit;
    end;

    // Send initial retry directive (reconnect after 3 seconds if disconnected)
    sseEvent := 'retry: 3000'#10#10;
    ClientSock.TrySndLow(pointer(sseEvent), Length(sseEvent));

    // Main SSE streaming loop - continues until client disconnects or server terminates
    while not Terminated do
    begin
      // Simulate processing delay (500ms + random up to 2 seconds, like DMVC)
      SleepHiRes(500 + Random(2000));

      // Check if client is still connected
      if not ClientSock.SockConnected then
      begin
        TSynLog.Add.Log(sllDebug, 'SSE: Client % disconnected', [remoteIP]);
        Break;
      end;

      // Get next stock update
      stockData := GetNextStockUpdate(lastEventID, currentEventID);

      // Format as SSE event per specification:
      //   event: eventname
      //   id: eventid
      //   data: jsondata
      //   <blank line to end event>
      sseEvent := FormatUtf8(
        'event: stockupdate'#10 +
        'id: %'#10 +
        'data: %'#10 +
        #10,  // Empty line marks end of event
        [currentEventID, stockData]);

      // Send event to client
      sent := ClientSock.TrySndLow(pointer(sseEvent), Length(sseEvent));
      if not sent then
      begin
        TSynLog.Add.Log(sllDebug, 'SSE: Send failed for client % (disconnected?)',
          [remoteIP]);
        Break;
      end;

      lastEventID := currentEventID;

      TSynLog.Add.Log(sllTrace, 'SSE event sent: id=% to %',
        [currentEventID, remoteIP]);
    end;

  finally
    LockedDec32(@fActiveSSEConnections);
    TSynLog.Add.Log(sllInfo, 'SSE stream ended for client %, sent % events',
      [remoteIP, currentEventID]);
  end;
end;

function TSSEHttpServer.ServeStaticFile(ClientSock: THttpServerSocket): boolean;
var
  url: RawUtf8;
  relPath: RawUtf8;
  fileName: TFileName;
  ext: RawUtf8;
  mimeType: RawUtf8;
  content: RawByteString;
  response: RawUtf8;
begin
  Result := false;
  url := ClientSock.Http.CommandUri;

  // Extract relative path
  if IdemPChar(pointer(url), '/ROOT/STATIC/') then
    relPath := Copy(url, Length('/root/static/') + 1, MaxInt)
  else if IdemPChar(pointer(url), '/STATIC/') then
    relPath := Copy(url, Length('/static/') + 1, MaxInt)
  else
    Exit;

  // Security: prevent directory traversal
  if PosEx('..', relPath) > 0 then
  begin
    TSynLog.Add.Log(sllWarning, 'Static: Directory traversal attempt: %', [relPath]);
    Exit;
  end;

  // Build file path
  if relPath = '' then
    fileName := fWwwPath + 'index.html'
  else
    fileName := fWwwPath + Utf8ToString(StringReplaceChars(relPath, '/', PathDelim));

  // Check if file exists
  if not FileExists(fileName) then
  begin
    TSynLog.Add.Log(sllTrace, 'Static: File not found: %', [fileName]);
    Exit;
  end;

  // Determine MIME type
  ext := LowerCase(StringToUtf8(ExtractFileExt(fileName)));
  if ext = '.html' then
    mimeType := 'text/html; charset=utf-8'
  else if ext = '.css' then
    mimeType := 'text/css; charset=utf-8'
  else if ext = '.js' then
    mimeType := 'application/javascript; charset=utf-8'
  else if ext = '.json' then
    mimeType := 'application/json; charset=utf-8'
  else if ext = '.png' then
    mimeType := 'image/png'
  else if ext = '.jpg' then
    mimeType := 'image/jpeg'
  else if ext = '.gif' then
    mimeType := 'image/gif'
  else if ext = '.ico' then
    mimeType := 'image/x-icon'
  else
    mimeType := 'application/octet-stream';

  // Read file content
  content := StringFromFile(fileName);
  if content = '' then
    Exit;

  // Build and send HTTP response
  response := FormatUtf8(
    'HTTP/1.1 200 OK'#13#10 +
    'Content-Type: %'#13#10 +
    'Content-Length: %'#13#10 +
    'Cache-Control: public, max-age=3600'#13#10 +
    'Connection: keep-alive'#13#10 +
    #13#10,
    [mimeType, Length(content)]);

  if ClientSock.TrySndLow(pointer(response), Length(response)) then
    ClientSock.TrySndLow(pointer(content), Length(content));

  Result := true;
  TSynLog.Add.Log(sllTrace, 'Static: Served %', [fileName]);
end;

{ TSSESampleServer }

constructor TSSESampleServer.Create(const aPort: RawUtf8);
var
  wwwPath: TFileName;
begin
  inherited Create;

  // Create custom HTTP server with thread pool
  fHttpServer := TSSEHttpServer.Create(aPort, nil, nil, 'SSE Sample', 4,
    30000, [hsoNoXPoweredHeader], nil);

  // Calculate www path relative to executable
  wwwPath := Executable.ProgramFilePath;
  // Try ../www first (when running from Win32/Debug or similar)
  if DirectoryExists(wwwPath + '..\www') then
    wwwPath := IncludeTrailingPathDelimiter(wwwPath + '..\www')
  else if DirectoryExists(wwwPath + 'www') then
    wwwPath := IncludeTrailingPathDelimiter(wwwPath + 'www')
  else
    wwwPath := IncludeTrailingPathDelimiter(wwwPath);

  fHttpServer.WwwPath := wwwPath;

  TSynLog.Add.Log(sllInfo, 'SSE Sample Server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, 'Static files path: %', [wwwPath]);
end;

destructor TSSESampleServer.Destroy;
begin
  fHttpServer.Free;
  inherited;
end;

procedure TSSESampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'SSE Sample Server started');
  WriteLn('');
  WriteLn('TRUE Server-Sent Events Implementation (mORMot2)');
  WriteLn('================================================');
  WriteLn('');
  WriteLn('This implements W3C SSE specification with:');
  WriteLn('  - Long-lived HTTP connection (NOT polling)');
  WriteLn('  - Server-push events to client');
  WriteLn('  - Automatic reconnection support (retry directive)');
  WriteLn('  - Last-Event-ID header for resuming');
  WriteLn('');
  WriteLn('Available endpoints:');
  WriteLn('  http://localhost:', fHttpServer.SockPort, '/static/       - Web interface');
  WriteLn('  http://localhost:', fHttpServer.SockPort, '/stocks        - SSE stream');
  WriteLn('');
  WriteLn('Test with curl:');
  WriteLn('  curl -N http://localhost:', fHttpServer.SockPort, '/stocks');
  WriteLn('');
end;

end.
