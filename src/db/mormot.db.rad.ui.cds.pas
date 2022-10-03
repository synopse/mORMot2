/// ORM and JSON Process using TClientDataSet
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.ui.cds;

{
  *****************************************************************************

   Efficient Read/Write TClientDataSet for ORM and JSON Process
   - Fill a TClientDataset from TOrmTable/TOrmTableJson data

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.variants,
  mormot.orm.core,
  mormot.rest.core,
  mormot.db.core,
  mormot.db.rad,
  mormot.db.rad.ui,
  mormot.db.rad.ui.orm,
  {$ifdef FPC}
  BufDataSet,  
  {$else}
  {$ifdef OSWINDOWS}
  MidasLib,
  {$endif OSWINDOWS}  DBClient,
  {$endif FPC}  {$ifdef ISDELPHIXE2}
  Data.DB;  
  {$else}
  DB;
  {$endif ISDELPHIXE2}


{************ Fill a TClientDataset from TOrmTable/TOrmTableJson data }

{$ifdef FPC}
type
  /// FPC's pure pascal in-memory buffer is used instead of TClientDataSet
  TClientDataSet = class(TBufDataset)
  public
    /// override this method with a blank implementation to avoid a warning
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField); override;
  end;
{$endif FPC}


/// convert a TOrmTable/TOrmTableJson result into a new TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TOrmTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function ToClientDataSet(
  aOwner: TComponent; aTable: TOrmTable; aClient: TRest = nil
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TClientDataSet; overload;

/// convert a JSON result into a new TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TOrmTable content - see mormot.db.rad.ui.pas if you need a more
// efficient, but read-only version
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), UnicodeString will be used
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function JsonToClientDataSet(
  aOwner: TComponent; const aJson: RawUTF8; aClient: TRest = nil
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TClientDataSet; overload;

/// convert a JSON result into a new TClientDataSet
// - this overloaded method allows to specify the TOrm class types
// associated with the supplied JSON
function JsonToClientDataSet(
  aOwner: TComponent; const aJson: RawUTF8;
  const Tables: array of TOrmClass; aClient: TRest = nil
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TClientDataSet; overload;

type
  /// how ToClientDataSet/JsonToClientDataSet functions will fill
  // the TClientDataSet instance
  TClientDataSetMode = (
    cdsNew,
    cdsAppend,
    cdsReplace
  );

/// convert a TOrmTable/TOrmTableJson result into an existing TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TOrmTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function ToClientDataSet(
  aDataSet: TClientDataSet; aTable: TOrmTable; aClient: TRest = nil;
  aMode: TClientDataSetMode = cdsReplace; aLogChange: boolean = false
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): boolean; overload;

/// convert a JSON result into an existing TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TOrmTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), UnicodeString will be used
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function JsonToClientDataSet(
  aDataSet: TClientDataSet; const aJson: RawUTF8; aClient: TRest = nil;
  aMode: TClientDataSetMode = cdsReplace; aLogChange: boolean = false
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): boolean; overload;


implementation


{$ifdef FPC}

{ TClientDataSet }

procedure TClientDataSet.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
end;

{$endif FPC}


{************ Fill a TClientDataset from TOrmTable/TOrmTableJson data }

function JsonToClientDataSet(aDataSet: TClientDataSet; const aJson: RawUTF8;
  aClient: TRest; aMode: TClientDataSetMode; aLogChange: boolean
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): boolean;
var
  T: TOrmTableJson;
begin
  T := TOrmTableJson.Create('', aJson);
  try
    result := ToClientDataSet(aDataSet, T, aClient, aMode, aLogChange
      {$ifndef UNICODE}, aForceWideString{$endif});
  finally
    T.Free;
  end;
end;

function JsonToClientDataSet(aOwner: TComponent; const aJson: RawUTF8;
  aClient: TRest {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TClientDataSet;
var
  T: TOrmTableJson;
begin
  T := TOrmTableJson.Create('', aJson);
  try
    result := ToClientDataSet(aOwner, T, aClient{$ifndef UNICODE},
      aForceWideString{$endif});
  finally
    T.Free;
  end;
end;

function JsonToClientDataSet(aOwner: TComponent; const aJson: RawUTF8;
  const Tables: array of TOrmClass; aClient: TRest
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TClientDataSet;
var
  T: TOrmTableJson;
begin
  T := TOrmTableJson.CreateFromTables(Tables, '', aJson);
  try
    result := ToClientDataSet(aOwner, T, aClient
      {$ifndef UNICODE}, aForceWideString{$endif});
  finally
    T.Free;
  end;
end;

var
  GlobalDataSetCount: integer;

function ToClientDataSet(aOwner: TComponent; aTable: TOrmTable; aClient: TRest
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TClientDataSet;
begin
  result := TClientDataSet.Create(aOwner);
  try
    result.Name := 'mORMotDS' + IntToStr(GlobalDataSetCount); // unique name
    inc(GlobalDataSetCount);
    if aTable = nil then
      exit;
    if not ToClientDataSet(result, aTable, aClient, cdsNew, false
        {$ifndef UNICODE}, aForceWideString{$endif}) then
      FreeAndNil(result);
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

function ToClientDataSet(aDataSet: TClientDataSet; aTable: TOrmTable;
  aClient: TRest; aMode: TClientDataSetMode; aLogChange: boolean
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): boolean;
var
  f, i: integer;
  col: array of record
    Def: TDBFieldDef;
    Field: TField;
    WasReadOnly: boolean;
    OnChange: TFieldNotifyEvent;
  end;
  prev: record
    Active: Boolean;
    ReadOnly: Boolean;
    LogChanges: Boolean;
    AfterScroll: TDataSetNotifyEvent;
  end;
begin
  result := false;
  if (aDataSet = nil) or
     (aTable = nil) then
    exit;
  FillcharFast(prev, sizeof(prev), 0);
  if aDataSet.Active then
  begin
    prev.Active := true;
    {$ifndef FPC}
    prev.LogChanges := aDataSet.LogChanges;
    {$endif FPC}
    prev.ReadOnly := aDataSet.ReadOnly;
    prev.AfterScroll := aDataSet.AfterScroll;
    aDataSet.AfterScroll := nil;
    aDataSet.ReadOnly := false;
    aDataSet.DisableControls;
  end;
  if aMode = cdsReplace then
  begin
    {$ifndef FPC}
    if prev.LogChanges then
      aDataSet.LogChanges := false;
    aDataSet.EmptyDataSet;  
    {$else}
    aDataSet.MergeChangeLog;
    aDataSet.Close;
    aDataSet.Open;
    {$endif FPC}
  end;
  // handle col
  SetLength(col, aTable.FieldCount);
  for f := 0 to aTable.FieldCount - 1 do
    GetDBFieldDef(aTable, f, col[f].Def
      {$ifndef UNICODE}, aForceWideString{$endif});
  if aMode = cdsNew then
  begin
    for f := 0 to high(col) do
      with col[f].Def do
        aDataSet.FieldDefs.Add(FieldName, DBType, DBSize);
    aDataSet.CreateDataSet;
    for f := 0 to high(col) do
      col[f].Field := aDataSet.FieldByName(col[f].Def.FieldName);
  end
  else
    for f := 0 to high(col) do
      with col[f] do
      begin
        Field := aDataSet.FieldByName(col[f].Def.FieldName);
        if Field.ReadOnly then
        begin
          WasReadOnly := true;
          Field.ReadOnly := false;
        end;
        OnChange := Field.OnChange;
        Field.OnChange := nil;
      end;
  // append data
  try
    {$ifndef FPC}
    aDataSet.LogChanges := aLogChange;
    {$endif FPC}
    for i := 1 to aTable.RowCount do
    begin
      aDataSet.Append;
      for f := 0 to high(col) do
        with col[f] do
          GetDBFieldValue(aTable, i, Field, aDataSet, Def);
      aDataSet.Post;
    end;
    aDataSet.First;
    result := True;
  finally
    if prev.Active then
    begin
      {$ifndef FPC}
      aDataSet.LogChanges := prev.LogChanges;
      {$endif FPC}
      aDataSet.ReadOnly := prev.ReadOnly;
      aDataSet.AfterScroll := prev.AfterScroll;
      if Assigned(prev.AfterScroll) then
        prev.AfterScroll(aDataSet);
      aDataSet.EnableControls;
    end;
    if aMode <> cdsNew then
    begin
      for f := 0 to high(col) do
        with col[f] do
          if Field <> nil then
          begin
            Field.ReadOnly := WasReadOnly;
            Field.OnChange := OnChange;
          end;
    end;
  end;
end;


end.

