unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  StrUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.log,
  mormot.core.json,
  mormot.net.http,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.rest.memserver,
  mormot.soa.core,
  api.interfaces,
  api.impl;

type
  /// Binary content sample server with raw file streaming
  /// Demonstrates serving files with proper Content-Type headers
  TBinaryContentSampleServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
    fFilesPath: TFileName;

    /// Handle direct file requests via OnBeforeUri
    function OnBeforeUri(Ctxt: TRestServerUriContext): boolean;

    /// Get MIME type from file extension
    function GetMimeType(const aFileName: RawUtf8): RawUtf8;

    /// Get the full file path (with security checks)
    function GetSecureFilePath(const aFileName: RawUtf8): TFileName;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    property FilesPath: TFileName read fFilesPath;
  end;

implementation

{ TBinaryContentSampleServer }

function TBinaryContentSampleServer.GetMimeType(const aFileName: RawUtf8): RawUtf8;
var
  ext: RawUtf8;
begin
  ext := LowerCase(ExtractFileExt(Utf8ToString(aFileName)));

  // Common MIME types for binary content
  if ext = '.pdf' then
    Result := 'application/pdf'
  else if ext = '.jpg' then
    Result := 'image/jpeg'
  else if ext = '.jpeg' then
    Result := 'image/jpeg'
  else if ext = '.png' then
    Result := 'image/png'
  else if ext = '.gif' then
    Result := 'image/gif'
  else if ext = '.bmp' then
    Result := 'image/bmp'
  else if ext = '.ico' then
    Result := 'image/x-icon'
  else if ext = '.svg' then
    Result := 'image/svg+xml'
  else if ext = '.txt' then
    Result := 'text/plain'
  else if ext = '.html' then
    Result := 'text/html'
  else if ext = '.css' then
    Result := 'text/css'
  else if ext = '.js' then
    Result := 'application/javascript'
  else if ext = '.json' then
    Result := 'application/json'
  else if ext = '.xml' then
    Result := 'application/xml'
  else if ext = '.zip' then
    Result := 'application/zip'
  else if ext = '.mp3' then
    Result := 'audio/mpeg'
  else if ext = '.mp4' then
    Result := 'video/mp4'
  else
    Result := 'application/octet-stream';
end;

function TBinaryContentSampleServer.GetSecureFilePath(const aFileName: RawUtf8): TFileName;
var
  fullPath: TFileName;
begin
  Result := '';

  // Build full path
  fullPath := fFilesPath + Utf8ToString(aFileName);
  fullPath := ExpandFileName(fullPath);

  // Security: Prevent directory traversal
  if not StartsStr(fFilesPath, fullPath) then
  begin
    TSynLog.Add.Log(sllWarning, 'Directory traversal attempt: %', [aFileName]);
    exit;
  end;

  // Check file exists
  if not FileExists(fullPath) then
  begin
    TSynLog.Add.Log(sllWarning, 'File not found: %', [fullPath]);
    exit;
  end;

  Result := fullPath;
end;

function TBinaryContentSampleServer.OnBeforeUri(Ctxt: TRestServerUriContext): boolean;
var
  url: RawUtf8;
  fileName: RawUtf8;
  filePath: TFileName;
  fileContent: RawByteString;
  mimeType: RawUtf8;
  fileSize: Int64;
begin
  Result := True; // Default: let normal processing continue

  url := Ctxt.Call^.Url;

  // Handle /files/<filename> for raw binary download
  if IdemPChar(pointer(url), '/FILES/') then
  begin
    // Extract filename from URL
    fileName := Copy(url, 8, MaxInt); // Skip '/files/'

    // URL decode the filename
    fileName := UrlDecode(fileName);

    TSynLog.Add.Log(sllInfo, 'Binary file request: %', [fileName]);

    // Get secure file path
    filePath := GetSecureFilePath(fileName);
    if filePath = '' then
    begin
      Ctxt.Error('File not found', HTTP_NOTFOUND);
      Result := False; // Stop processing
      exit;
    end;

    // Read file content
    fileContent := StringFromFile(filePath);
    if fileContent = '' then
    begin
      Ctxt.Error('Failed to read file', HTTP_SERVERERROR);
      Result := False;
      exit;
    end;

    fileSize := Length(fileContent);
    mimeType := GetMimeType(fileName);

    TSynLog.Add.Log(sllInfo, 'Serving file: % (% bytes, %)',
      [fileName, fileSize, mimeType]);

    // Return raw binary content with proper headers
    Ctxt.Returns(fileContent, HTTP_SUCCESS,
      'Content-Type: ' + mimeType + #13#10 +
      'Content-Disposition: inline; filename="' + fileName + '"' + #13#10 +
      'Content-Length: ' + Int64ToUtf8(fileSize) + #13#10 +
      'Cache-Control: public, max-age=3600');

    Result := False; // We handled the request, stop normal processing
    exit;
  end;

  // Handle /download/<filename> for attachment download
  if IdemPChar(pointer(url), '/DOWNLOAD/') then
  begin
    fileName := Copy(url, 11, MaxInt); // Skip '/download/'
    fileName := UrlDecode(fileName);

    TSynLog.Add.Log(sllInfo, 'Download request: %', [fileName]);

    filePath := GetSecureFilePath(fileName);
    if filePath = '' then
    begin
      Ctxt.Error('File not found', HTTP_NOTFOUND);
      Result := False;
      exit;
    end;

    fileContent := StringFromFile(filePath);
    if fileContent = '' then
    begin
      Ctxt.Error('Failed to read file', HTTP_SERVERERROR);
      Result := False;
      exit;
    end;

    fileSize := Length(fileContent);
    mimeType := GetMimeType(fileName);

    // Return as attachment (forces download dialog)
    Ctxt.Returns(fileContent, HTTP_SUCCESS,
      'Content-Type: ' + mimeType + #13#10 +
      'Content-Disposition: attachment; filename="' + fileName + '"' + #13#10 +
      'Content-Length: ' + Int64ToUtf8(fileSize));

    Result := False;
    exit;
  end;
end;

constructor TBinaryContentSampleServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Set up files directory
  fFilesPath := Executable.ProgramFilePath;
  fFilesPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fFilesPath));
  fFilesPath := IncludeTrailingPathDelimiter(fFilesPath) + 'files_repository' +
                PathDelim;

  // Create directory if not exists
  ForceDirectories(fFilesPath);

  TSynLog.Add.Log(sllInfo, 'Files directory: %', [fFilesPath]);

  // Create in-memory REST server
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Wire raw binary handler BEFORE normal SOA processing
  fRestServer.OnBeforeUri := OnBeforeUri;

  // Register service implementation for JSON-based operations
  fRestServer.ServiceDefine(TBinaryContentSample, [IBinaryContentSample], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    aPort,
    [fRestServer],
    '+',
    useHttpSocket
  );

  fHttpServer.AccessControlAllowOrigin := '*';

  TSynLog.Add.Log(sllInfo, 'Binary Content Server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, '');
  TSynLog.Add.Log(sllInfo, 'RAW BINARY endpoints (proper Content-Type):');
  TSynLog.Add.Log(sllInfo, '  GET /files/<filename>    - inline display');
  TSynLog.Add.Log(sllInfo, '  GET /download/<filename> - force download');
  TSynLog.Add.Log(sllInfo, '');
  TSynLog.Add.Log(sllInfo, 'JSON API endpoints (for structured operations):');
  TSynLog.Add.Log(sllInfo, '  POST /BinaryContentSample/UploadBinaryData');
end;

destructor TBinaryContentSampleServer.Destroy;
begin
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

end.
