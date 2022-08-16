// Author: Thomas Bogenrieder
// This unit provides utilities for creating my mORMot sample applications. Functions are kept as simple
// as possible. Classes have no special architecture. The source code is neither tested nor optimized.

unit u_Utilities;

{$I mormot.defines.inc}

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes, System.UITypes, System.IniFiles,
  Vcl.Controls, Vcl.Forms,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.unicode;

type
  TCompPropStorage = class(TObject)
  private
    const
      FORMRECT_TAG = '#';
      FORMRECT_IDENT = 'FormRect';
      COMPPATH_SEPERATOR = '-';
      PROPPATH_SEPERATOR = '.';
    type
      // Splits PropPath names into structure
      TCompPropStruct = record
        CompName: TComponentName;
        PropNames: TStringDynArray;
      end;
      PCompPropStruct = ^TCompPropStruct;

      TCompPropStructs = array of TCompPropStruct;
  public
    type
      TLoadMode = (
        lmOnlyDefined,   // Only defined properties
        lmAllParent,     // All existing concern the parent
        lmAllWithChilds  // All existing concern the parent and his children
      );

      // Definitions for own storage formats
      TFormRect = packed record
        Top, Left, Width, Height: Integer;
        WindowState: TWindowState;
      end;
  private
    class function CompPath(pmSender: TComponent; const pmcCompName: String): String;
    class procedure SplitPropPathNames(pmParent: TComponent; const pmcPropNames: TStringDynArray;
      out pmoCompPropStructs: TCompPropStructs);
    class procedure InternalLoadFromIni(pmParent: TComponent; pmIniReader: TCustomIniFile;
      pmLoadMode: TLoadMode; const pmcPropNames: TStringDynArray);
    class procedure InternalSaveToIni(pmParent: TComponent; pmIniWriter: TCustomIniFile;
      const pmcPropNames: TStringDynArray; pmWithChilds: Boolean = True);
  public
    class function PropPath(pmSender: TComponent; const pmcPropName: String): String;
    class procedure MergePropNames(var pmvSource: TStringDynArray; const pmcToMerge: TStringDynArray);
    class procedure LoadFromIniFile(pmParent: TComponent; const pmcFileName: TFileName;
      pmLoadMode: TLoadMode = lmAllWithChilds; const pmcPropNames: TStringDynArray = Nil);
    class procedure SaveToIniFile(pmParent: TComponent; const pmcFileName: TFileName;
      const pmcPropNames: TStringDynArray; pmWithChilds: Boolean = True);
  end;

  TFormStorage = class(TComponent)
  private
    FIniFileName: TFileName;
    FFormPropNames: TStringDynArray;
    FOwnerFormDestroyEvent: TNotifyEvent;
    procedure DoOwnerFormDestroy(pmSender: TObject);
  public
    const
      INI_EXTENSION = '.wnd';
  public
    constructor Create(pmOwner: TForm; const pmcIniFileName: TFileName = ''); reintroduce;
    procedure Load(const pmcPropNames: TStringDynArray = Nil);
  end;

type
  TFileNameValidation = (fnvDirName, fnvFileName);
  TFileNameValidations = set of TFileNameValidation;

function CheckFileName(const pmcFileName: TFileName; pmValidationParts: TFileNameValidations; pmCheckedFileName: PFileName = Nil): Boolean;


implementation

uses
  System.TypInfo,
  System.IOUtils,
  mormot.core.os;

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


//==============================================================================
// TCompPropStorage
//==============================================================================

class function TCompPropStorage.CompPath(pmSender: TComponent; const pmcCompName: String): String;
begin
  Result := '';
  if pmSender = Nil then Exit; //=>

  Result := pmSender.Name;
  if pmcCompName <> '' then
    Result := Result + COMPPATH_SEPERATOR + pmcCompName;
end;


class procedure TCompPropStorage.SplitPropPathNames(pmParent: TComponent; const pmcPropNames: TStringDynArray;
  out pmoCompPropStructs: TCompPropStructs);
var
  idx, pos: Integer;
  propName: String;
  compName: TComponentName;
  compStruct: PCompPropStruct;
begin
  if pmParent = Nil then Exit; //=>
  if Length(pmcPropNames) = 0 then Exit; //=>

  SetLength(pmoCompPropStructs, 1);
  compStruct := @pmoCompPropStructs[0];
  compStruct.CompName := pmParent.Name;
  for var i: Integer := 0 to High(pmcPropNames) do
  begin
    pos := System.Pos(PROPPATH_SEPERATOR, pmcPropNames[i]);
    if pos = 0 then
    begin
      propName := pmcPropNames[i];
      if propName = FORMRECT_TAG then
        propName := FORMRECT_IDENT;

      AddString(compStruct.PropNames, propName);
    end
    else
    begin
      compName := Copy(pmcPropNames[i], 1, pos - 1);
      propName := Copy(pmcPropNames[i], pos + 1, MaxInt);
      idx := 0;  // 0 -> full PropPath for parent
      while idx < Length(pmoCompPropStructs) do
      begin
        compStruct := @pmoCompPropStructs[idx];
        if AnsiICompW(PWideChar(compStruct.CompName), PWideChar(compName)) = 0 then
        begin
          AddString(compStruct.PropNames, propName);
          Break; //->
        end;

        Inc(idx);
      end;

      if idx = Length(pmoCompPropStructs) then
      begin
        SetLength(pmoCompPropStructs, idx + 1);
        compStruct := @pmoCompPropStructs[idx];
        compStruct.CompName := compName;
        AddString(compStruct.PropNames, propName);
      end;
    end;
  end;
end;


class procedure TCompPropStorage.InternalLoadFromIni(pmParent: TComponent; pmIniReader: TCustomIniFile;
  pmLoadMode: TLoadMode; const pmcPropNames: TStringDynArray);

  //-------- local functions ------------------------------------------

  procedure StringListToStringDynArray(_pmSource: TStringList; var _pmvResult: TStringDynArray);
  begin
    Finalize(_pmvResult);
    SetLength(_pmvResult, _pmSource.Count);
    for var i: Integer := 0 to _pmSource.Count - 1 do
      _pmvResult[i] := _pmSource[i];
  end;

  procedure LoadComponent(_pmComponent: TComponent; const _pmSectionPath: String; const _pmcIdentNames: TStringDynArray);
  var
    identName: String;
    rect: TCompPropStorage.TFormRect;
    prop: PRttiProp;
    propValue: RawUtf8;
  begin
    for var i: Integer := 0 to High(_pmcIdentNames) do
    begin
      identName := _pmcIdentNames[i];
      if identName = FORMRECT_IDENT then
      begin
        if not (_pmComponent is TForm) then Continue; //->

        var json: RawJson := StringToUtf8(pmIniReader.ReadString(_pmSectionPath, identName, ''));
        if IsValidJson(json)
          and RecordLoadJson(rect, json, TypeInfo(TCompPropStorage.TFormRect)) then
        begin
          if rect.WindowState <> wsMaximized then
          begin
            case TForm(_pmComponent).Position of
              poDefault, poDefaultPosOnly:
                TForm(_pmComponent).SetBounds(rect.Left, rect.Top, rect.Width, rect.Height);
              poDefaultSizeOnly:
                begin
                  TForm(_pmComponent).Top := rect.Top;
                  TForm(_pmComponent).Left := rect.Left;
                end;
            end;
          end
          else
            TForm(_pmComponent).WindowState := wsMaximized;
        end;
      end
      else
      begin
        propValue := StringToUtf8(pmIniReader.ReadString(_pmSectionPath, identName, ''));
        if propValue <> '' then
        begin
          prop := ClassFieldPropWithParents(_pmComponent.ClassType, ShortString(identName));
          if prop <> Nil then
            prop^.SetValueText(_pmComponent, propValue);
        end;
      end;
    end;
  end;

  //-------- end local functions --------------------------------------

var
  compStruct: PCompPropStruct;
  compStructs: TCompPropStructs;
begin
  if pmParent = Nil then Exit; //=>
  if pmIniReader = Nil then Exit; //=>

  if pmLoadMode = lmOnlyDefined then
  begin
    SplitPropPathNames(pmParent, pmcPropNames, compStructs);

    compStruct := @compStructs[0];
    LoadComponent(pmParent, CompPath(pmParent, ''), compStruct.PropNames);

    if Length(compStructs) > 1 then
    begin
      var run: TComponent;
      for var i: Integer := 0 to pmParent.ComponentCount - 1 do
      begin
        run := pmParent.Components[i];
        if run is TControl then
        begin
          for var n: Integer := 1 to High(compStructs) do
          begin
            compStruct := @compStructs[n];
            if AnsiICompW(PWideChar(run.Name), PWideChar(compStruct.CompName)) = 0 then
            begin
              LoadComponent(run, CompPath(pmParent, run.Name), compStruct.PropNames);
              Break; //->
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    SetLength(compStructs, 1);
    compStruct := @compStructs[0];

    var secPath: String;
    var secIdents: TStringList := TStringList.Create;
    try
      secPath := CompPath(pmParent, '');
      pmIniReader.ReadSection(secPath, secIdents);
      StringListToStringDynArray(secIdents, compStruct.PropNames);
      if Length(compStruct.PropNames) > 0 then
        LoadComponent(pmParent, secPath, compStruct.PropNames);

      if pmLoadMode = lmAllWithChilds then
      begin
        var run: TComponent;
        for var i: Integer := 0 to pmParent.ComponentCount - 1 do
        begin
          run := pmParent.Components[i];
          if run is TControl then
          begin
            secPath := CompPath(pmParent, run.Name);
            pmIniReader.ReadSection(secPath, secIdents);
            StringListToStringDynArray(secIdents, compStruct.PropNames);
            if Length(compStruct.PropNames) > 0 then
              LoadComponent(pmParent, secPath, compStruct.PropNames);
          end;
        end;
      end;
    finally
      secIdents.Free;
    end;
  end;
end;


class procedure TCompPropStorage.InternalSaveToIni(pmParent: TComponent; pmIniWriter: TCustomIniFile;
  const pmcPropNames: TStringDynArray; pmWithChilds: Boolean);

  //-------- local function -------------------------------------------

  procedure SaveComponent(_pmComponent: TComponent; const _pmSectionPath: String; const _pmcIdentNames: TStringDynArray);
  var
    identName: String;
    json: RawJson;
    rect: TCompPropStorage.TFormRect;
  begin
    for var i: Integer := 0 to High(_pmcIdentNames) do
    begin
      identName := _pmcIdentNames[i];
      if identName = FORMRECT_IDENT then
      begin
        if not (_pmComponent is TForm) then Continue; //->

        rect.WindowState := TForm(_pmComponent).WindowState;
        with TForm(_pmComponent).BoundsRect do
        begin
          rect.Top := Top;
          rect.Left := Left;
          rect.Width := Width;
          rect.Height := Height;
        end;

        json := RecordSaveJson(rect, TypeInfo(TCompPropStorage.TFormRect), True);
        pmIniWriter.WriteString(_pmSectionPath, identName, Utf8ToString(json));
      end
      else
        pmIniWriter.WriteString(_pmSectionPath, identName, VarToStr(GetPropValue(_pmComponent, identName)));
    end;
  end;

  //-------- end local function ---------------------------------------

var
  compStruct: PCompPropStruct;
  compStructs: TCompPropStructs;
begin
  if pmParent = Nil then Exit; //=>
  if pmIniWriter = Nil then Exit; //=>
  if Length(pmcPropNames) = 0 then Exit; //=>

  SplitPropPathNames(pmParent, pmcPropNames, compStructs);

  compStruct := @compStructs[0];
  if Length(compStruct.PropNames) > 0 then
    SaveComponent(pmParent, CompPath(pmParent, ''), compStruct.PropNames);

  if pmWithChilds then
  begin
    var run: TComponent;
    for var i: Integer := 0 to pmParent.ComponentCount - 1 do
    begin
      run := pmParent.Components[i];
      if run.InheritsFrom(TControl) then
      begin
        for var n: Integer := 1 to High(compStructs) do
        begin
          compStruct := @compStructs[n];
          if AnsiICompW(PWideChar(run.Name), PWideChar(compStruct.CompName)) = 0 then
          begin
            SaveComponent(run, CompPath(pmParent, run.Name), compStruct.PropNames);
            Break; //->
          end;
        end;
      end;
    end;
  end;
end;


class function TCompPropStorage.PropPath(pmSender: TComponent; const pmcPropName: String): String;
begin
  Result := '';
  if pmSender = Nil then Exit; //=>

  Result := pmSender.Name;
  if pmcPropName <> '' then
    Result := Result + PROPPATH_SEPERATOR + pmcPropName;
end;


class procedure TCompPropStorage.MergePropNames(var pmvSource: TStringDynArray; const pmcToMerge: TStringDynArray);
begin
  if Length(pmcToMerge) = 0 then Exit; //=>

  var insNeeded: Boolean;
  for var i: Integer := 0 to High(pmcToMerge) do
  begin
    insNeeded := True;
    for var n: Integer := 0 to High(pmvSource) do
    begin
      if AnsiICompW(PWideChar(pmvSource[n]), PWideChar(pmcToMerge[i])) = 0 then
      begin
        insNeeded := False;
        Break; //->
      end;
    end;

    if insNeeded then
      AddString(pmvSource, pmcToMerge[i]);
  end;
end;


class procedure TCompPropStorage.LoadFromIniFile(pmParent: TComponent; const pmcFileName: TFileName;
  pmLoadMode: TLoadMode = lmAllWithChilds; const pmcPropNames: TStringDynArray = Nil);
var
  iniReader: TMemIniFile;
begin
  if pmParent = Nil then Exit; //=>
  if not FileExists(pmcFileName) then Exit; //=>

  iniReader := TMemIniFile.Create(pmcFileName);
  try
    InternalLoadFromIni(pmParent, iniReader, pmLoadMode, pmcPropNames);
  finally
    iniReader.Free;
  end;
end;


class procedure TCompPropStorage.SaveToIniFile(pmParent: TComponent; const pmcFileName: TFileName;
  const pmcPropNames: TStringDynArray; pmWithChilds: Boolean);
var
  iniWriter: TMemIniFile;
begin
  if pmParent = Nil then Exit; //=>
  if pmcFileName = '' then Exit; //=>
  if Length(pmcPropNames) = 0 then Exit; //=>

  iniWriter := TMemIniFile.Create(pmcFileName);
  try
    InternalSaveToIni(pmParent, iniWriter, pmcPropNames, pmWithChilds);
    iniWriter.UpdateFile;
  finally
    iniWriter.Free;
  end;
end;


//==============================================================================
// TFormStorage
//==============================================================================

constructor TFormStorage.Create(pmOwner: TForm; const pmcIniFileName: TFileName);
begin
  inherited Create(pmOwner);
  SetName('TFormStorage');
  Assert(pmOwner <> Nil);
  if Assigned(pmOwner.OnDestroy) then
    FOwnerFormDestroyEvent := pmOwner.OnDestroy;
  pmOwner.OnDestroy := DoOwnerFormDestroy;
  FIniFileName := pmcIniFileName;
  if FIniFileName = '' then
    FIniFileName := ChangeFileExt(Executable.ProgramFileName, INI_EXTENSION);
  FFormPropNames := [TCompPropStorage.FORMRECT_TAG];
end;


procedure TFormStorage.DoOwnerFormDestroy(pmSender: TObject);
begin
  TCompPropStorage.SaveToIniFile(Owner, FIniFileName, FFormPropNames);
  if Assigned(FOwnerFormDestroyEvent) then
    FOwnerFormDestroyEvent(pmSender);
end;


procedure TFormStorage.Load(const pmcPropNames: TStringDynArray);
begin
  if Length(pmcPropNames) > 0 then
  begin
    TCompPropStorage.MergePropNames(FFormPropNames, pmcPropNames);
    TCompPropStorage.LoadFromIniFile(Owner, FIniFileName, lmOnlyDefined, FFormPropNames);
  end
  else
    TCompPropStorage.LoadFromIniFile(Owner, FIniFileName);
end;

end.
