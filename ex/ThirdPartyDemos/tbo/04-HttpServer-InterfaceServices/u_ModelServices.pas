// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

unit u_ModelServices;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.soa.server,
  u_ServiceInterfaces;

type
  TCustomServiceObject = class(TInjectableObjectRest)
  public
    function GetSessionUserDirName(out pmoDirName: TFileName): Boolean;
  end;

  TDocumentService = class(TCustomServiceObject, IDocument)
  strict private
    type
      TInterfaceMethodNameRec = record
        const
          Load = 'Load';
          Save = 'Save';
          GetAllNames = 'GetAllNames';
      end;
  public
    const
      IMN: TInterfaceMethodNameRec = ();
  public
    //*** IDocument ***
    function Load(const pmcName: TFileName; out pmoData: RawBlob): Boolean;
    function Save(const pmcName: TFileName; const pmcData: RawBlob): Boolean;
    procedure GetAllNames(out pmoFileNames: TFileNameDynArray);
  end;


implementation

uses
  mormot.core.os,
  mormot.core.search,
  u_ServiceServer,
  u_ServiceUtils;

//==============================================================================
// TCustomServiceObject
//==============================================================================

function TCustomServiceObject.GetSessionUserDirName(out pmoDirName: TFileName): Boolean;
begin
  Result := False;
  var authUser: TFileAuthUser := TFileAuthUser(Server.SessionGetUser(ServiceRunningContext.Request.Session));
  if authUser <> Nil then
  try
    pmoDirName := MakePath([TFileRestServer(Server).DataFolder, Format('C%.5d', [authUser.CustomerNum])], True);
    Result := True;
  finally
    authUser.Free;
  end;
end;


//==============================================================================
// TDocumentService
//==============================================================================

function TDocumentService.Load(const pmcName: TFileName; out pmoData: RawBlob): Boolean;
var
  dirName, docName, fileName: TFileName;
begin
  Result := False;
  if pmcName = '' then Exit; //=>
  if not GetSessionUserDirName(dirName) then Exit; //=>

  if CheckFileName(pmcName, [fnvFileName], @docName) then  // Never trust data that comes from outside!
  begin
    if ExtractFileExt(docName) = '' then
      docName := ChangeFileExt(docName, TFileRestServer.DEFAULT_FILE_EXT);

    fileName := MakePath([dirName, docName]);
    if FileExists(fileName) then
    begin
      pmoData := StringFromFile(fileName);
      Result := (pmoData <> '');
    end;
  end;
end;


function TDocumentService.Save(const pmcName: TFileName; const pmcData: RawBlob): Boolean;
var
  dirName, docName, fileName: TFileName;
begin
  Result := False;
  if pmcName = '' then Exit; //=>
  if pmcData = '' then Exit; //=>
  if not GetSessionUserDirName(dirName) then Exit; //=>

  if CheckFileName(pmcName, [fnvFileName], @docName) then  // Never trust data that comes from outside!
  begin
    if ExtractFileExt(docName) = '' then
      docName := ChangeFileExt(docName, TFileRestServer.DEFAULT_FILE_EXT);

    fileName := MakePath([dirName, docName]);
    if EnsureDirectoryExists(ExtractFilePath(fileName)) <> '' then
      Result := FileFromString(pmcData, fileName);
  end;
end;


procedure TDocumentService.GetAllNames(out pmoFileNames: TFileNameDynArray);
var
  dirName: TFileName;
begin
  if not GetSessionUserDirName(dirName) then Exit; //=>

  pmoFileNames := FileNames(dirName, FILES_ALL, [ffoSortByName, ffoExcludesDir]);
  for var i: Integer := Low(pmoFileNames) to High(pmoFileNames) do
  begin
    if ExtractFileExt(pmoFileNames[i]) = TFileRestServer.DEFAULT_FILE_EXT then
      pmoFileNames[i] := GetFileNameWithoutExt(pmoFileNames[i]);
  end;
end;

end.
