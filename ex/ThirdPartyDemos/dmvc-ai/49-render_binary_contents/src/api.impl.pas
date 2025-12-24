unit api.impl;

interface

uses
  System.SysUtils,
  System.StrUtils,
  mormot.core.base,
  mormot.core.json,
  mormot.core.data,
  mormot.core.text,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.interfaces,
  mormot.core.unicode,
  mormot.rest.core,
  api.interfaces;

type
  TBinaryContentSample = class(TInjectableObject, IBinaryContentSample)
  private
    function GetContentType(const aFileName: RawUtf8): RawUtf8;
    function GetFilePath(const aFileName: RawUtf8): TFileName;
    procedure ValidateFilePath(const aPath: TFileName);
  public
    function GetFileByName(const FileName: RawUtf8): TFileDownloadDTO;
    function GetStreamByFileName(const FileName: RawUtf8): TFileDownloadDTO;
    function UploadBinaryData(const fieldname, filename, contenttype: RawUtf8;
      const data: RawByteString): TFileUploadResponseDTO;
  end;

implementation

{ TBinaryContentSample }

function TBinaryContentSample.GetContentType(const aFileName: RawUtf8): RawUtf8;
var
  ext: RawUtf8;
begin
  ext := LowerCase(ExtractFileExt(Utf8ToString(aFileName)));

  if ext = '.jpg' then
    result := 'image/jpeg'
  else if ext = '.txt' then
    result := 'text/plain'
  else if ext = '.pdf' then
    result := 'application/pdf'
  else if ext = '.png' then
    result := 'image/png'
  else if ext = '.html' then
    result := 'text/html'
  else
    result := 'application/octet-stream';
end;

function TBinaryContentSample.GetFilePath(const aFileName: RawUtf8): TFileName;
var
  basePath: TFileName;
  fn: TFileName;
begin
  // Get application path
  basePath := Executable.ProgramFilePath;

  // Combine with files_repository folder (one level up)
  basePath := ExtractFilePath(ExcludeTrailingPathDelimiter(basePath));
  basePath := IncludeTrailingPathDelimiter(basePath) + 'files_repository';

  fn := Utf8ToString(aFileName);
  result := IncludeTrailingPathDelimiter(basePath) + fn;

  // Normalize path
  result := ExpandFileName(result);
end;

procedure TBinaryContentSample.ValidateFilePath(const aPath: TFileName);
var
  appPath: TFileName;
begin
  appPath := Executable.ProgramFilePath;

  // Directory traversal check
  if not StartsStr(appPath, aPath) then
    raise ERestException.CreateUtf8('Invalid path: %', [aPath]);

  // File existence check
  if not FileExists(aPath) then
    raise ERestException.CreateUtf8('File not found: %', [aPath]);
end;

function TBinaryContentSample.GetFileByName(const FileName: RawUtf8): TFileDownloadDTO;
var
  filePath: TFileName;
  fileContent: RawByteString;
begin
  filePath := GetFilePath(FileName);
  ValidateFilePath(filePath);

  // Read file content
  fileContent := StringFromFile(filePath);

  // Fill result
  result.FileName := FileName;
  result.ContentType := GetContentType(FileName);
  result.Data := BinToBase64(fileContent); // Encode as base64 for JSON transport
end;

function TBinaryContentSample.GetStreamByFileName(const FileName: RawUtf8): TFileDownloadDTO;
begin
  // Same implementation as GetFileByName (alternative method for demonstration)
  result := GetFileByName(FileName);
end;

function TBinaryContentSample.UploadBinaryData(const fieldname, filename,
  contenttype: RawUtf8; const data: RawByteString): TFileUploadResponseDTO;
var
  filePath: TFileName;
  decodedData: RawByteString;
  savedFileName: RawUtf8;
begin
  try
    // Decode base64 data
    if not Base64ToBin(PAnsiChar(data), length(data), decodedData) then
      raise ERestException.Create('Invalid base64 data');

    // Generate safe filename with timestamp
    savedFileName := FormatUtf8('%_%', [NowUtc, filename]);
    filePath := GetFilePath(savedFileName);

    // Create directory if needed
    ForceDirectories(ExtractFilePath(filePath));

    // Save file
    FileFromString(decodedData, filePath);

    // Build success response
    result.Success := true;
    result.Message := 'File uploaded successfully';
    result.SavedFileName := savedFileName;
    result.Reference := FormatUtf8('files/%', [savedFileName]);
  except
    on E: Exception do
    begin
      result.Success := false;
      result.Message := StringToUtf8(E.Message);
      result.SavedFileName := '';
      result.Reference := '';
    end;
  end;
end;

end.
