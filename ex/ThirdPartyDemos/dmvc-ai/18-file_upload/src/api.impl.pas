unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.interfaces,
  mormot.soa.server,
  api.interfaces;

const
  UPLOAD_FOLDER = 'uploadedfiles';

type
  /// Implementation of file upload API
  TFileUploadApiService = class(TInjectableObjectRest, IFileUploadApi)
  public
    /// Get list of uploaded files
    function GetUploadedFiles(out Files: TUploadedFileInfoDynArray): Boolean;
  end;

implementation

uses
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.search,
  mormot.core.zip;

{ TFileUploadApiService }

function TFileUploadApiService.GetUploadedFiles(
  out Files: TUploadedFileInfoDynArray): Boolean;
var
  fileList: TFindFilesDynArray;
  i: Integer;
begin
  result := False;
  Files := nil;

  // Get all files in upload folder
  fileList := FindFiles(UPLOAD_FOLDER, '*');

  if Length(fileList) > 0 then
  begin
    SetLength(Files, Length(fileList));
    for i := 0 to High(fileList) do
    begin
      Files[i].FileName := StringToUtf8(fileList[i].Name);
      Files[i].Size := fileList[i].Size;
    end;
    result := True;
  end;
end;

end.
