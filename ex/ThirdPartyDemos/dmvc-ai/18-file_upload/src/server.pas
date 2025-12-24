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
  mormot.core.buffers,
  mormot.core.search,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.net.server,
  mormot.net.http,
  api.interfaces,
  api.impl;

type
  /// Custom REST server with file upload handling
  TFileUploadRestServer = class(TRestServerFullMemory)
  private
    fUploadFolder: TFileName;
  protected
    procedure SetupRoutes;
  public
    constructor Create(aModel: TOrmModel; const aUploadFolder: TFileName); reintroduce;

    /// Serve the main page (GET /)
    procedure ServePage(Ctxt: TRestServerUriContext);

    /// Handle file upload (POST /upload)
    procedure HandleFileUpload(Ctxt: TRestServerUriContext);
  published
    // These methods will be available as REST endpoints
    property Upload: TRestServerUriContext write HandleFileUpload;
  end;

  /// Custom HTTP server with static file serving and multipart form handling
  TFileUploadHttpServer = class(TRestHttpServer)
  private
    fUploadFolder: TFileName;
    fStaticFolder: TFileName;
  protected
    /// Override the server response to handle multipart/form-data and static files
    function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;

    /// Serve static files (CSS, etc.)
    function ServeStaticFile(Ctxt: THttpServerRequestAbstract;
      const RelativePath: RawUtf8): cardinal;

    /// Parse and save uploaded file using mORMot2's MultiPartFormDataDecode
    function HandleMultipartUpload(Ctxt: THttpServerRequestAbstract): cardinal;

    /// Generate the main HTML page with file list
    function GenerateMainPage: RawUtf8;
  public
    constructor Create(const aPort: RawUtf8; aServers: array of TRestServer;
      const aUploadFolder, aStaticFolder: TFileName); reintroduce;
  end;

  /// Main server class
  TFileUploadSampleServer = class
  private
    fModel: TOrmModel;
    fServer: TFileUploadRestServer;
    fHttpServer: TFileUploadHttpServer;
    fPort: RawUtf8;
    fUploadFolder: TFileName;
    fStaticFolder: TFileName;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property HttpServer: TFileUploadHttpServer read fHttpServer;
  end;

implementation

{ TFileUploadRestServer }

constructor TFileUploadRestServer.Create(aModel: TOrmModel;
  const aUploadFolder: TFileName);
begin
  inherited Create(aModel, 'root', False);
  fUploadFolder := aUploadFolder;
  SetupRoutes;
end;

procedure TFileUploadRestServer.SetupRoutes;
begin
  // Routes are handled by the HTTP server override
end;

procedure TFileUploadRestServer.ServePage(Ctxt: TRestServerUriContext);
begin
  // This is handled by the HTTP server
  Ctxt.Returns('', HTTP_NOTFOUND);
end;

procedure TFileUploadRestServer.HandleFileUpload(Ctxt: TRestServerUriContext);
begin
  // This is handled by the HTTP server's multipart parser
  Ctxt.Returns('', HTTP_NOTFOUND);
end;

{ TFileUploadHttpServer }

constructor TFileUploadHttpServer.Create(const aPort: RawUtf8;
  aServers: array of TRestServer; const aUploadFolder, aStaticFolder: TFileName);
begin
  inherited Create(aPort, aServers, '+', useHttpAsync);
  fUploadFolder := aUploadFolder;
  fStaticFolder := aStaticFolder;
  AccessControlAllowOrigin := '*';
end;

function TFileUploadHttpServer.GenerateMainPage: RawUtf8;
var
  html: RawUtf8;
  files: TFileNameDynArray;
  fileList: RawUtf8;
  i: Integer;
  fileName: RawUtf8;
begin
  // Get uploaded files
  files := FileNames(fUploadFolder, FILES_ALL);

  fileList := '';
  if Length(files) > 0 then
  begin
    for i := 0 to High(files) do
    begin
      fileName := StringToUtf8(ExtractFileName(files[i]));
      fileList := fileList + '<li>' + fileName + '</li>'#13#10;
    end;
  end;

  html := '<!DOCTYPE html>'#13#10 +
    '<html>'#13#10 +
    '<head>'#13#10 +
    '  <link rel="stylesheet" href="/static/milligram.min.css"/>'#13#10 +
    '  <meta charset="UTF-8">'#13#10 +
    '  <title>mORMot2 File Upload Demo</title>'#13#10 +
    '</head>'#13#10 +
    '<body>'#13#10 +
    '<div class="container">'#13#10 +
    '<h2>mORMot2 - File Upload DEMO</h2>'#13#10 +
    '<form action="/upload" method="post" enctype="multipart/form-data">'#13#10 +
    '<input class="btn" type="file" name="fupload">'#13#10 +
    '<hr>'#13#10 +
    '<input type="submit" value="Upload">'#13#10 +
    '</form>'#13#10 +
    '<h3>There are ' + IntToStr(Length(files)) + ' uploaded files</h3>'#13#10 +
    '<ul>'#13#10 +
    fileList +
    '</ul>'#13#10 +
    '</div>'#13#10 +
    '</body>'#13#10 +
    '</html>';

  result := html;
end;

function TFileUploadHttpServer.ServeStaticFile(
  Ctxt: THttpServerRequestAbstract; const RelativePath: RawUtf8): cardinal;
var
  fileName: TFileName;
  content: RawByteString;
  ext: RawUtf8;
begin
  // Security: prevent directory traversal
  if PosEx('..', RelativePath) > 0 then
  begin
    result := HTTP_NOTFOUND;
    Exit;
  end;

  // Build file path
  fileName := fStaticFolder + Utf8ToString(RelativePath);

  if FileExists(fileName) then
  begin
    // Read and serve the file
    content := StringFromFile(fileName);
    Ctxt.OutContent := content;

    // Set content type based on extension
    ext := LowerCase(StringToUtf8(ExtractFileExt(fileName)));
    if ext = '.css' then
      Ctxt.OutContentType := 'text/css'
    else if ext = '.js' then
      Ctxt.OutContentType := 'application/javascript'
    else if ext = '.html' then
      Ctxt.OutContentType := 'text/html'
    else
      Ctxt.OutContentType := 'application/octet-stream';

    result := HTTP_SUCCESS;
  end
  else
    result := HTTP_NOTFOUND;
end;

function TFileUploadHttpServer.HandleMultipartUpload(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  parts: TMultiPartDynArray;
  i: Integer;
  fileName: RawUtf8;
  targetFile: TFileName;
begin
  result := HTTP_BADREQUEST;

  // Use mORMot2's built-in multipart decoder
  if not MultiPartFormDataDecode(Ctxt.InContentType, Ctxt.InContent, parts) then
  begin
    Ctxt.OutContent := 'Invalid multipart/form-data';
    Exit;
  end;

  // Process each uploaded part
  for i := 0 to High(parts) do
  begin
    // Skip non-file parts (regular form fields)
    if parts[i].FileName = '' then
      Continue;

    fileName := parts[i].FileName;

    // Validate filename
    if fileName = '' then
      Continue;

    // Security: extract just the filename, no path
    fileName := StringToUtf8(ExtractFileName(Utf8ToString(fileName)));

    // Check if file already exists
    targetFile := fUploadFolder + Utf8ToString(fileName);
    if FileExists(targetFile) then
    begin
      result := HTTP_BADREQUEST;
      Ctxt.OutContent := 'File already exists: ' + fileName;
      Exit;
    end;

    // Save the file
    try
      FileFromString(parts[i].Content, targetFile);
      TSynLog.Add.Log(sllInfo, 'File uploaded: % (% bytes, type: %)',
        [fileName, Length(parts[i].Content), parts[i].ContentType]);

      // Redirect to main page
      Ctxt.OutCustomHeaders := 'Location: /'#13#10;
      result := HTTP_TEMPORARYREDIRECT;
      Exit;
    except
      on E: Exception do
      begin
        TSynLog.Add.Log(sllError, 'Error saving file %: %', [fileName, E.Message]);
        result := HTTP_SERVERERROR;
        Ctxt.OutContent := 'Error saving file: ' + StringToUtf8(E.Message);
        Exit;
      end;
    end;
  end;

  // No file found in multipart data
  if result = HTTP_BADREQUEST then
    Ctxt.OutContent := 'No file found in upload';
end;

function TFileUploadHttpServer.Request(
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  url: RawUtf8;
begin
  url := Ctxt.Url;

  // Handle root path - serve main page
  if (url = '/') and (Ctxt.Method = 'GET') then
  begin
    Ctxt.OutContent := GenerateMainPage;
    Ctxt.OutContentType := 'text/html; charset=UTF-8';
    result := HTTP_SUCCESS;
    Exit;
  end;

  // Handle upload POST
  if (url = '/upload') and (Ctxt.Method = 'POST') then
  begin
    result := HandleMultipartUpload(Ctxt);
    Exit;
  end;

  // Handle static files
  if IdemPChar(pointer(url), '/STATIC/') then
  begin
    result := ServeStaticFile(Ctxt, Copy(url, 9, MaxInt));
    Exit;
  end;

  // Call the associated TRestServer instance(s) for API endpoints
  result := inherited Request(Ctxt);
end;

{ TFileUploadSampleServer }

constructor TFileUploadSampleServer.Create(const aPort: RawUtf8);
begin
  fPort := aPort;

  // Set up folders
  fUploadFolder := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + '..\uploadedfiles\');
  fStaticFolder := IncludeTrailingPathDelimiter(
    Executable.ProgramFilePath + '..\www\');

  // Create upload folder if it doesn't exist
  if not DirectoryExists(fUploadFolder) then
    CreateDir(fUploadFolder);

  // Create ORM model (empty for this sample)
  fModel := TOrmModel.Create([]);

  // Create REST server
  fServer := TFileUploadRestServer.Create(fModel, fUploadFolder);

  // Register API service
  fServer.ServiceDefine(TFileUploadApiService, [IFileUploadApi], sicShared)
    .SetOptions([], [optExecLockedPerInterface])
    .ResultAsJsonObjectWithoutResult := true;

  // Create HTTP server with upload and static file support
  fHttpServer := TFileUploadHttpServer.Create(aPort, [fServer],
    fUploadFolder, fStaticFolder);
end;

destructor TFileUploadSampleServer.Destroy;
begin
  fHttpServer.Free;
  fServer.Free;
  fModel.Free;
  inherited;
end;

procedure TFileUploadSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Server starting on port %', [fPort]);
  WriteLn('File Upload Sample');
  WriteLn('==================');
  WriteLn('Available endpoints:');
  WriteLn('  http://localhost:', fPort, '/           - Main upload page');
  WriteLn('  http://localhost:', fPort, '/upload     - Upload endpoint (POST)');
  WriteLn('  http://localhost:', fPort, '/static/    - Static files (CSS)');
  WriteLn('  http://localhost:', fPort, '/root/FileUploadApi - REST API');
  WriteLn;
  WriteLn('Upload folder: ', fUploadFolder);
  WriteLn;
end;

end.
