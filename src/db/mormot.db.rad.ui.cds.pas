/// SQL, ORM and JSON Process using TClientDataSet
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.ui.cds;

{
  *****************************************************************************

   Efficient Read/Write TClientDataSet for SQL, ORM and JSON Process
   - Fill a read/only TClientDataset from TOrmTable/TOrmTableJson data
   - Delphi writable TSqlDBClientDataSet inheriting from TClientDataSet

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
  mormot.db.sql,
  mormot.db.rad,
  mormot.db.rad.ui,
  mormot.db.rad.ui.orm,
  mormot.db.rad.ui.sql,
  {$ifdef FPC}
  // FPC will use the TBufDataset cross-platform component
  BufDataSet,
  {$else}
  // Delphi units (using the so-called MIDAS on Windows)
  {$ifdef OSWINDOWS}
  MidasLib,
  {$endif OSWINDOWS}
  DBClient,
  Provider,
  {$endif FPC}
  {$ifdef ISDELPHIXE2}
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
  aOwner: TComponent; const aJson: RawUtf8; aClient: TRest = nil
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TClientDataSet; overload;

/// convert a JSON result into a new TClientDataSet
// - this overloaded method allows to specify the TOrm class types
// associated with the supplied JSON
function JsonToClientDataSet(
  aOwner: TComponent; const aJson: RawUtf8;
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
  aDataSet: TClientDataSet; const aJson: RawUtf8; aClient: TRest = nil;
  aMode: TClientDataSetMode = cdsReplace; aLogChange: boolean = false
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): boolean; overload;



{************ Delphi writable TSqlDBClientDataSet inheriting from TClientDataSet }

{$ifdef ISDELPHI} // Delphi only implementation

type
  /// TClientDataSet allowing to apply updates on a mormot.db.sql connection
  // - typical usage may be for instance over a mormot.db.proxy connection:
  // ! props := TSqlDBWinHTTPConnectionProperties.Create(....);
  // ! ds := TSqlDBClientDataSet.Create(MainForm);
  // ! ds.CommandText := 'select * from people';
  // ! ds.Open;
  // ! // ... use ds as usual, including modifications
  // ! ds.ApplyUpdates(0);
  TSqlDBClientDataSet = class(TCustomClientDataSet)
  protected
    fDataSet: TSqlDataSet; // from mormot.db.rad.ui.sql
    fProvider: TDataSetProvider;
    fIgnoreColumnDataSize: boolean;
    function GetConnection: TSqlDBConnectionProperties; virtual;
    procedure SetConnection(Value: TSqlDBConnectionProperties); virtual;
    // from TDataSet
    procedure OpenCursor(InfoQuery: Boolean); override;
    {$ifdef ISDELPHI2007ANDUP}
    // from IProviderSupport
    function PSGetCommandText: string; override;
    {$endif ISDELPHI2007ANDUP}
  public
    /// initialize the instance
    constructor Create(AOwner: TComponent); override;
    /// retrieve the dataset parameters
    procedure FetchParams;
    /// initialize the internal TDataSet from a mormot.db.sql.pas
    // TSqlDBStatement result set
    // - the supplied TSqlDBStatement can then be freed by the caller, since
    // a private binary copy will be owned by this instance (in fDataSet.Data)
    procedure From(Statement: TSqlDBStatement; MaxRowCount: cardinal = 0);
    /// if field sizes should be left unset, allowing further filling with
    // any data length
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. mormot.db.rad.ui.sql.pas TBinaryDataSet.InternalInitFieldDefs
    // define the field as ftDefaultMemo)
    property IgnoreColumnDataSize: boolean
      read fIgnoreColumnDataSize write fIgnoreColumnDataSize;
  published
    property CommandText;
    property Active;
    property Aggregates;
    property AggregatesActive;
    property AutoCalcFields;
    property Constraints;
    property DisableStringTrim;
    property FileName;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs;
    property IndexDefs;
    property IndexFieldNames;
    property IndexName;
    property FetchOnDemand;
    property MasterFields;
    property MasterSource;
    property ObjectView;
    property PacketRecords;
    property Params;
    property ReadOnly;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
    /// the associated mormot.db.sql connection
    property Connection: TSqlDBConnectionProperties
      read GetConnection write SetConnection;
    /// the associated  TDataSet, used to retrieve and update data
    property DataSet: TSqlDataSet
      read fDataSet;
  end;

{$endif ISDELPHI}


implementation


{************ Fill a TClientDataset from TOrmTable/TOrmTableJson data }

{$ifdef FPC}

{ TClientDataSet }

procedure TClientDataSet.LoadBlobIntoBuffer(FieldDef: TFieldDef; ABlobBuf: PBufBlobField);
begin
end;

{$endif FPC}


function JsonToClientDataSet(aDataSet: TClientDataSet; const aJson: RawUtf8;
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

function JsonToClientDataSet(aOwner: TComponent; const aJson: RawUtf8;
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

function JsonToClientDataSet(aOwner: TComponent; const aJson: RawUtf8;
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
    {$ifdef ISDELPHI}
    prev.LogChanges := aDataSet.LogChanges;
    {$endif ISDELPHI}
    prev.ReadOnly := aDataSet.ReadOnly;
    prev.AfterScroll := aDataSet.AfterScroll;
    aDataSet.AfterScroll := nil;
    aDataSet.ReadOnly := false;
    aDataSet.DisableControls;
  end;
  if aMode = cdsReplace then
  begin
    {$ifdef ISDELPHI}
    if prev.LogChanges then
      aDataSet.LogChanges := false;
    aDataSet.EmptyDataSet;
    {$else}
    aDataSet.MergeChangeLog;
    aDataSet.Close;
    aDataSet.Open;
    {$endif ISDELPHI}
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
    {$ifdef ISDELPHI}
    aDataSet.LogChanges := aLogChange;
    {$endif ISDELPHI}
    for i := 1 to aTable.RowCount do
    begin
      aDataSet.Append;
      for f := 0 to high(col) do
        with col[f] do
          GetDBFieldValue(aTable, i, Field, aDataSet, Def);
      aDataSet.Post;
    end;
    aDataSet.First;
    result := true;
  finally
    if prev.Active then
    begin
      {$ifdef ISDELPHI}
      aDataSet.LogChanges := prev.LogChanges;
      {$endif ISDELPHI}
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


{************ Delphi writable TSqlDBClientDataSet inheriting from TClientDataSet }

{$ifdef ISDELPHI}

{ TSqlDBClientDataSet }

constructor TSqlDBClientDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fProvider := TDataSetProvider.Create(Self);
  fProvider.Name := 'InternalProvider';                 { Do not localize }
  fProvider.SetSubComponent(true);
  fProvider.Options := fProvider.Options + [poAllowCommandText];
  SetProvider(fProvider);
  fDataSet := TSqlDataSet.Create(self);
  fDataSet.Name := 'InternalDataSet';                   { Do not localize }
  fDataSet.SetSubComponent(true);
  fProvider.DataSet := fDataSet;
end;

procedure TSqlDBClientDataSet.From(Statement: TSqlDBStatement; MaxRowCount: cardinal);
begin
  fDataSet.From(Statement, MaxRowCount, fIgnoreColumnDataSize);
  fDataSet.CommandText := ''; // ensure no SQL execution
  Open;
  fDataSet.CommandText := Utf8ToString(Statement.SQL); // assign it AFTER Open
end;

procedure TSqlDBClientDataSet.FetchParams;
begin
  if not HasAppServer and
     Assigned(FProvider) then
    SetProvider(FProvider);
  inherited FetchParams;
end;

procedure TSqlDBClientDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Assigned(fProvider) then
    SetProvider(fProvider);
  if fProvider.DataSet = self then
    ESqlDBException.RaiseUtf8('Circular %.OpenCursor', [self]);
  inherited OpenCursor(InfoQuery);
end;

{$ifdef ISDELPHI2007ANDUP}
{$ifdef ISDELPHIXE3}
function TSqlDBClientDataSet.PSGetCommandText: string;
var
  IP: IProviderSupportNG;
begin
  if Supports(fDataSet, IProviderSupportNG, IP) then
    result := IP.PSGetCommandText
  else
    result := CommandText;
end;
{$else}
function TSqlDBClientDataSet.PSGetCommandText: string;
var
  IP: IProviderSupport;
begin
  if Supports(fDataSet, IProviderSupport, IP) then
    result := IP.PSGetCommandText
  else
    result := CommandText;
end;
{$endif ISDELPHIXE3}
{$endif ISDELPHI2007ANDUP}

function TSqlDBClientDataSet.GetConnection: TSqlDBConnectionProperties;
begin
  result := fDataSet.Connection;
end;

procedure TSqlDBClientDataSet.SetConnection(Value: TSqlDBConnectionProperties);
begin
  fDataSet.Connection := Value;
end;

{$endif ISDELPHI}


end.


