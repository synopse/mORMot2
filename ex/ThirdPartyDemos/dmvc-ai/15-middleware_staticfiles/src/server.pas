unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.net.http,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.net.server,
  api.interfaces,
  api.impl;

type
  /// Custom HTTP server with static file serving middleware
  TStaticFilesHttpServer = class(TRestHttpServer)
  protected
    fStaticRoots: array of record
      UrlPath: RawUtf8;
      FileSystemPath: TFileName;
      DefaultFile: TFileName;
      AllowDirectoryListing: Boolean;
      Charset: RawUtf8;
      CustomFilter: TStaticFileFilter;
      CustomMimeTypes: TRawUtf8DynArray;
      CustomMimeValues: TRawUtf8DynArray;
    end;
    /// Override the server response - must be thread-safe
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
    /// Serve a static file from the file system
    function ServeStaticFile(Ctxt: THttpServerRequestAbstract;
      const UrlPath, FileSystemPath, DefaultFile, Charset: RawUtf8;
      AllowDirectoryListing: Boolean;
      CustomFilter: TStaticFileFilter;
      const CustomMimeTypes, CustomMimeValues: TRawUtf8DynArray): cardinal;
    /// Get MIME type for a file extension
    function GetMimeType(const FileName: TFileName;
      const CustomMimeTypes, CustomMimeValues: TRawUtf8DynArray): RawUtf8;
  public
    /// Add a static file serving path (simple version)
    procedure AddStaticPath(const UrlPath, FileSystemPath: RawUtf8); overload;
    /// Add a static file serving path (with custom MIME types)
    procedure AddStaticPath(const UrlPath, FileSystemPath: RawUtf8;
      const DefaultFile: TFileName;
      AllowDirectoryListing: Boolean;
      const Charset: RawUtf8;
      CustomFilter: TStaticFileFilter;
      const CustomMimeTypes, CustomMimeValues: array of RawUtf8); overload;
  end;

  /// Main server class
  TStaticFilesSampleServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TStaticFilesHttpServer;
    fPort: RawUtf8;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property HttpServer: TStaticFilesHttpServer read fHttpServer;
  end;

implementation

{ TStaticFilesHttpServer }

procedure TStaticFilesHttpServer.AddStaticPath(const UrlPath,
  FileSystemPath: RawUtf8);
begin
  AddStaticPath(UrlPath, FileSystemPath, 'index.html', False, 'UTF-8', nil, [], []);
end;

procedure TStaticFilesHttpServer.AddStaticPath(const UrlPath,
  FileSystemPath: RawUtf8; const DefaultFile: TFileName;
  AllowDirectoryListing: Boolean; const Charset: RawUtf8;
  CustomFilter: TStaticFileFilter;
  const CustomMimeTypes, CustomMimeValues: array of RawUtf8);
var
  idx: Integer;
  i: Integer;
begin
  idx := Length(fStaticRoots);
  SetLength(fStaticRoots, idx + 1);
  fStaticRoots[idx].UrlPath := UrlPath;
  fStaticRoots[idx].FileSystemPath := FileSystemPath;
  fStaticRoots[idx].DefaultFile := DefaultFile;
  fStaticRoots[idx].AllowDirectoryListing := AllowDirectoryListing;
  fStaticRoots[idx].Charset := Charset;
  fStaticRoots[idx].CustomFilter := CustomFilter;

  // Copy custom MIME types
  SetLength(fStaticRoots[idx].CustomMimeTypes, Length(CustomMimeTypes));
  SetLength(fStaticRoots[idx].CustomMimeValues, Length(CustomMimeValues));
  for i := 0 to High(CustomMimeTypes) do
  begin
    fStaticRoots[idx].CustomMimeTypes[i] := CustomMimeTypes[i];
    fStaticRoots[idx].CustomMimeValues[i] := CustomMimeValues[i];
  end;

  TSynLog.Add.Log(sllInfo, 'Static path added: % -> %',
    [UrlPath, FileSystemPath]);
end;

function TStaticFilesHttpServer.GetMimeType(const FileName: TFileName;
  const CustomMimeTypes, CustomMimeValues: TRawUtf8DynArray): RawUtf8;
var
  ext: RawUtf8;
  i: Integer;
begin
  ext := LowerCase(StringToUtf8(ExtractFileExt(FileName)));

  // Check custom MIME types first
  for i := 0 to High(CustomMimeTypes) do
    if ext = CustomMimeTypes[i] then
    begin
      result := CustomMimeValues[i];
      Exit;
    end;

  // Default MIME types
  if ext = '.html' then
    result := 'text/html'
  else if ext = '.htm' then
    result := 'text/html'
  else if ext = '.css' then
    result := 'text/css'
  else if ext = '.js' then
    result := 'application/javascript'
  else if ext = '.json' then
    result := 'application/json'
  else if ext = '.xml' then
    result := 'application/xml'
  else if ext = '.txt' then
    result := 'text/plain'
  else if ext = '.png' then
    result := 'image/png'
  else if ext = '.jpg' then
    result := 'image/jpeg'
  else if ext = '.jpeg' then
    result := 'image/jpeg'
  else if ext = '.gif' then
    result := 'image/gif'
  else if ext = '.svg' then
    result := 'image/svg+xml'
  else if ext = '.ico' then
    result := 'image/x-icon'
  else if ext = '.pdf' then
    result := 'application/pdf'
  else if ext = '.zip' then
    result := 'application/zip'
  else if ext = '.xpi' then
    result := 'application/x-xpinstall'
  else
    result := 'application/octet-stream';
end;

function TStaticFilesHttpServer.ServeStaticFile(
  Ctxt: THttpServerRequestAbstract;
  const UrlPath, FileSystemPath, DefaultFile, Charset: RawUtf8;
  AllowDirectoryListing: Boolean;
  CustomFilter: TStaticFileFilter;
  const CustomMimeTypes, CustomMimeValues: TRawUtf8DynArray): cardinal;
var
  relPath: RawUtf8;
  fileName: TFileName;
  allowed: Boolean;
  pathInfo: string;
  mimeType: RawUtf8;
  contentType: RawUtf8;
begin
  // Extract relative path after UrlPath
  relPath := Copy(Ctxt.Url, Length(UrlPath) + 1, MaxInt);

  // Security: prevent directory traversal
  if PosEx('..', relPath) > 0 then
  begin
    result := HTTP_NOTFOUND;
    Exit;
  end;

  // Convert to file system path
  fileName := Utf8ToString(FileSystemPath) +
    StringReplace(Utf8ToString(relPath), '/', '\', [rfReplaceAll]);

  // If empty or ends with /, append default file
  if (relPath = '') or (relPath[Length(relPath)] = '/') then
    fileName := fileName + DefaultFile;

  // Apply custom filter if provided
  allowed := True;
  if Assigned(CustomFilter) then
  begin
    pathInfo := Utf8ToString(relPath);
    CustomFilter(pathInfo, allowed);

    if not allowed then
    begin
      result := HTTP_NOTFOUND;
      Exit;
    end;

    // Filter may have modified the path
    fileName := Utf8ToString(FileSystemPath) +
      StringReplace(pathInfo, '/', '\', [rfReplaceAll]);
  end;

  // Check if file exists
  if FileExists(fileName) then
  begin
    // Use idiomatic SetOutFile for static file serving
    // Benefits: automatic 304 Not Modified, ETag, Cache-Control, MIME detection

    // Check if we have custom MIME type for this file
    mimeType := GetMimeType(fileName, CustomMimeTypes, CustomMimeValues);

    // Build content type with charset if specified
    if Charset <> '' then
      contentType := FormatUtf8('%; charset=%', [mimeType, Charset])
    else
      contentType := mimeType;

    // SetOutFile handles:
    // - Setting OutContent to file path
    // - Setting OutContentType to STATICFILE_CONTENT_TYPE
    // - Checking If-Modified-Since header for 304 responses
    // - Adding ETag header
    // - Adding Cache-Control: max-age header
    result := Ctxt.SetOutFile(
      fileName,           // File path
      True,               // Handle304NotModified - automatic conditional GET support
      contentType,        // ContentType with charset
      3600                // CacheControlMaxAgeSec - 1 hour cache
    );

    if result = HTTP_SUCCESS then
      TSynLog.Add.Log(sllTrace, 'Served static file: %', [fileName])
    else if result = HTTP_NOTMODIFIED then
      TSynLog.Add.Log(sllTrace, 'File not modified (304): %', [fileName]);
  end
  else if DirectoryExists(fileName) and AllowDirectoryListing then
  begin
    // Directory listing (simple implementation)
    Ctxt.OutContent := '<html><body><h1>Directory listing not implemented yet</h1></body></html>';
    Ctxt.OutContentType := 'text/html';
    result := HTTP_SUCCESS;
  end
  else
  begin
    result := HTTP_NOTFOUND;
    TSynLog.Add.Log(sllTrace, 'File not found: %', [fileName]);
  end;
end;

function TStaticFilesHttpServer.Request(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  i: Integer;
begin
  // Only handle GET requests
  if Ctxt.Method = 'GET' then
  begin
    // Check all registered static paths
    for i := 0 to High(fStaticRoots) do
    begin
      if IdemPChar(pointer(Ctxt.Url), pointer(fStaticRoots[i].UrlPath)) then
      begin
        result := ServeStaticFile(Ctxt,
          fStaticRoots[i].UrlPath,
          fStaticRoots[i].FileSystemPath,
          fStaticRoots[i].DefaultFile,
          fStaticRoots[i].Charset,
          fStaticRoots[i].AllowDirectoryListing,
          fStaticRoots[i].CustomFilter,
          fStaticRoots[i].CustomMimeTypes,
          fStaticRoots[i].CustomMimeValues);
        Exit;
      end;
    end;
  end;

  // Call the associated TRestServer instance(s) for API endpoints
  result := inherited Request(Ctxt);
end;

{ TStaticFilesSampleServer }

constructor TStaticFilesSampleServer.Create(const aPort: RawUtf8);
var
  wwwPath, www2Path, www3Path: TFileName;
begin
  fPort := aPort;

  // Create ORM model (empty for this sample)
  fModel := TOrmModel.Create([]);

  // Create REST server
  fServer := TRestServerFullMemory.Create(fModel, 'root', False);

  // Register API service
  fServer.ServiceDefine(TApiService, [IApiService], sicShared)
    .SetOptions([], [optExecLockedPerInterface])
    .ResultAsJsonObjectWithoutResult := true;

  // Create HTTP server with static file support
  fHttpServer := TStaticFilesHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';

  // Calculate paths relative to executable
  wwwPath := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + '..\www\');
  www2Path := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + '..\www2\');
  www3Path := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + '..\www3\');

  // Add static file paths (equivalent to DMVC middleware)

  // First path: /static -> www folder
  fHttpServer.AddStaticPath('/static', StringToUtf8(wwwPath), 'index.html',
    False, 'UTF-8', nil, [], []);

  // Second path: /static2 -> www2 folder
  fHttpServer.AddStaticPath('/static2', StringToUtf8(www2Path), 'index.html',
    False, 'UTF-8', nil, [], []);

  // Third path: /static3 -> www3 folder with custom filter and MIME types
  fHttpServer.AddStaticPath('/static3', StringToUtf8(www3Path), 'index.html',
    True, 'UTF-8',
    // Custom filter: block .txt files and redirect file1.html to file2.html
    procedure(var PathInfo: string; var Allow: Boolean)
    begin
      Allow := not PathInfo.EndsWith('.txt', True);
      if Allow and PathInfo.Contains('file1.html') then
        PathInfo := PathInfo.Replace('file1.html', 'file2.html');
    end,
    // Custom MIME types
    ['.xpi'],
    ['application/x-xpinstall']);
end;

destructor TStaticFilesSampleServer.Destroy;
begin
  fHttpServer.Free;
  fServer.Free;
  fModel.Free;
  inherited;
end;

procedure TStaticFilesSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Server starting on port %', [fPort]);
  WriteLn('Static files middleware sample');
  WriteLn('==============================');
  WriteLn('Available endpoints:');
  WriteLn('  http://localhost:', fPort, '/static      - Serves www/ folder');
  WriteLn('  http://localhost:', fPort, '/static2     - Serves www2/ folder');
  WriteLn('  http://localhost:', fPort, '/static3     - Serves www3/ folder (with filters)');
  WriteLn('  http://localhost:', fPort, '/api         - API endpoint');
  WriteLn('  http://localhost:', fPort, '/            - Redirects to /static');
  WriteLn;
end;

end.
