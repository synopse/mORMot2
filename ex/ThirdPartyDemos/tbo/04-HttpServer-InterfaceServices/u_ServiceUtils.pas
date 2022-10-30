// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

unit u_ServiceUtils;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.data;

type
  TFileNameValidation = (fnvDirName, fnvFileName);
  TFileNameValidations = set of TFileNameValidation;

function CheckFileName(const pmcFileName: TFileName; pmValidationParts: TFileNameValidations; pmCheckedFileName: PFileName = Nil): Boolean;


implementation

uses
  System.IOUtils;

//==============================================================================

function CheckFileName(const pmcFileName: TFileName; pmValidationParts: TFileNameValidations; pmCheckedFileName: PFileName = Nil): Boolean;
var
  dirName, fileName: TFileName;
begin
  if (pmcFileName = '') or (pmValidationParts = []) then Exit(False); //=>

  Result := True;
  if fnvDirName in pmValidationParts then
  begin
    dirName := TPath.GetDirectoryName(pmcFileName);
    Result := Result and TPath.HasValidPathChars(dirName, False);
  end;

  if Result
    and (fnvFileName in pmValidationParts) then
  begin
    fileName := TPath.GetFileName(pmcFileName);
    Result := Result and TPath.HasValidFileNameChars(fileName, False);
  end;

  if Result
    and (pmCheckedFileName <> Nil) then
  begin
    if (dirName <> '') and (fileName <> '') then
      pmCheckedFileName^ := TPath.Combine(dirName, fileName, False)
    else if fileName <> '' then
      pmCheckedFileName^ :=  fileName
    else
      pmCheckedFileName^ :=  dirName;
  end;
end;

end.
