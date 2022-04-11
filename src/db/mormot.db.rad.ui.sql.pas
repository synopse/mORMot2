/// mormot.db.sql Compatible TDataset Components
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.ui.sql;

{
  *****************************************************************************

   Efficient Read/Only TDataSet Working With mormot.db.sql
    - TBinaryDataSet Filled From a TSqlDBStatement ResultSet
    - TSqlDataSet For Direct TSqlDBConnection Sql Execution

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
  mormot.db.core,
  mormot.db.rad,
  mormot.db.sql,
  mormot.db.proxy,
  mormot.db.rad.ui,
  {$ifdef ISDELPHIXE2}
  Data.DB;
  {$else}
  DB;
  {$endif ISDELPHIXE2}



{************ TBinaryDataSet Filled From a TSqlDBStatement ResultSet }

type
  /// read-only virtual TDataSet able to access a binary buffer as returned
  // by TSqlStatement.FetchAllToBinary method or directly a TSqlStatement
  TBinaryDataSet = class(TVirtualDataSet)
  protected
    fData: RawByteString;
    fDataAccess: TSqlDBProxyStatementRandomAccess;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: integer; OnlyCheckNull: boolean): pointer; override;
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a FetchAllToBinary() buffer
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUtf8 column sizes before calling this From() method
    procedure From(const BinaryData: RawByteString;
      DataRowPosition: PCardinalDynArray = nil;
      IgnoreColumnDataSize: boolean = false); overload; virtual;
    /// initialize the virtual TDataSet from a SynDB TSqlDBStatement result set
    // - the supplied ISqlDBRows instance can safely be freed by the caller,
    // since a private binary copy will be owned by this instance (in Data)
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUtf8 column sizes before calling this From() method
    procedure From(Statement: TSqlDBStatement; MaxRowCount: cardinal = 0;
      IgnoreColumnDataSize: boolean = false); overload; virtual;
    /// finalize the class instance
    destructor Destroy; override;
    /// read-only access to the internal binary buffer
    property Data: RawByteString
      read fData;
    /// read-only access to the internal SynDB data
    property DataAccess: TSqlDBProxyStatementRandomAccess
      read fDataAccess;
  end;


{************ TSqlDataSet For Direct TSqlDBConnection Sql Execution }

type
  /// exceptions raised by the TSqlDataSet class
  ESqlDataSet = class(EVirtualDataSet);

  /// TDataSet able to execute any Sql as SynDB's TSqlStatement result set
  // - this class is not meant to be used by itself, but via TSynDBDataSet,
  // defined in SynDBMidasVCL.pas, as a data provider able to apply updates to
  // the remote SynDB connection
  // - typical usage may be for instance over a SynDBRemote connection:
  // ! props := TSqlDBWinHTTPConnectionProperties.Create(....);
  // ! ds := TSqlDataSet.Create(MainForm);
  // ! ds.CommandText := 'select * from people';
  // ! ds.Open;
  // ! // ... use ds
  // ! ds.Close;
  // ! ds.CommandText := 'select * from customer where id=:(10):';
  // ! ds.Open;
  // ! // ... use ds
  TSqlDataSet = class(TBinaryDataSet)
  protected
    fConnection: TSqlDBConnectionProperties;
    fCommandText: string;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    // IProvider implementation
    procedure PSSetCommandText(const ACommandText: string); override;
    function PSGetTableName: string; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind;
      Delta: TDataSet): Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    {$ifdef ISDELPHIXE3}
    function PSExecuteStatement(const ASQL: string;
      AParams: TParams): integer; overload; override;
    function PSExecuteStatement(const ASQL: string;
      AParams: TParams; var ResultSet: TDataSet): integer; overload; override;
    {$else}
    function PSExecuteStatement(const ASQL: string;
      AParams: TParams; ResultSet: pointer): integer; overload; override;
    {$endif ISDELPHIXE3}
  public
    /// initialize the internal TDataSet from a SynDB TSqlDBStatement result set
    // - the supplied TSqlDBStatement can then be freed by the caller, since
    // a private binary copy will be owned by this instance (in fDataSet.Data)
    // - by default, ColumnDataSize would be computed from the supplied data,
    // unless you set IgnoreColumnDataSize=true to set the value to 0 (and
    // force e.g. SynDBVCL TBinaryDataSet.InternalInitFieldDefs define the
    // field as ftDefaultMemo) or you define some FieldDefs.Items[].Size values
    // for ftUtf8 column sizes before calling this From() method
    procedure From(Statement: TSqlDBStatement; MaxRowCount: cardinal = 0;
      IgnoreColumnDataSize: boolean = false); override;
    /// the associated connection properties
    property Connection: TSqlDBConnectionProperties
      read fConnection write fConnection;
  published
    /// the Sql statement to be executed
    // - since this statement will be executed via Connection.ExecuteInlined,
    // you can specify optionally inlined parameters to this Sql text
    property CommandText: string
      read fCommandText write fCommandText;
  end;


/// fetch a SynDB's TSqlDBStatement result into a VCL DataSet
// - just a wrapper around TSynSqlStatementDataSet.Create + Open
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TSynSqlStatementDataSet instance, using
// an optimized internal binary buffer: the supplied statement can be released
// - if you need a writable TDataSet, you can use the slower ToClientDataSet()
// function as defined in SynDBMidasVCL.pas
function ToDataSet(aOwner: TComponent; aStatement: TSqlDBStatement;
  aMaxRowCount: integer = 0): TBinaryDataSet; overload;

/// fetch a SynDB ISqlDBRows result set into a VCL DataSet
// - this overloaded function can use directly a result of the
// TSqlDBConnectionProperties.Execute() method, as such:
// ! ds1.DataSet := ToDataSet(self,props.Execute('select * from table',[]));
function ToDataSet(aOwner: TComponent; const aStatement: ISqlDBRows;
  aMaxRowCount: integer = 0): TBinaryDataSet; overload;

/// fetch a SynDB's TSqlDBStatement.FetchAllToBinary buffer into a VCL DataSet
// - just a wrapper around TBinaryDataSet.Create + Open
// - if you need a writable TDataSet, you can use the slower ToClientDataSet()
// function as defined in SynDBMidasVCL.pas
function BinaryToDataSet(aOwner: TComponent;
  const aBinaryData: RawByteString): TBinaryDataSet;



implementation



{************ TBinaryDataSet Filled From a TSqlDBStatement ResultSet }

{ TBinaryDataSet }

procedure TBinaryDataSet.From(const BinaryData: RawByteString;
  DataRowPosition: PCardinalDynArray; IgnoreColumnDataSize: boolean);
begin
  fData := BinaryData;
  fDataAccess := TSqlDBProxyStatementRandomAccess.Create(
    pointer(fData), length(fData), DataRowPosition, IgnoreColumnDataSize);
end;

procedure TBinaryDataSet.From(Statement: TSqlDBStatement; MaxRowCount: cardinal;
  IgnoreColumnDataSize: boolean);
var
  stream: TRawByteStringStream;
  rowpos: TCardinalDynArray;
begin
  stream := TRawByteStringStream.Create;
  try
    Statement.FetchAllToBinary(stream, MaxRowCount, @rowpos);
    From(stream.DataString, @rowpos, IgnoreColumnDataSize);
  finally
    stream.Free;
  end;
end;

destructor TBinaryDataSet.Destroy;
begin
  inherited;
  FreeAndNil(fDataAccess);
end;

function TBinaryDataSet.GetRecordCount: integer;
begin
  if fDataAccess = nil then
    result := 0
  else
    result := fDataAccess.DataRowCount;
end;

procedure TBinaryDataSet.InternalInitFieldDefs;
var
  f, custom: PtrInt;
  dbtype: TFieldType;
  names: TRawUtf8DynArray; // FieldDefs.Items[].Name
  sizes: TIntegerDynArray; // FieldDefs.Items[].Size
begin
  if FieldDefs.Count > 0 then
  begin // custom column sizes
    SetLength(names, FieldDefs.Count);
    SetLength(sizes, FieldDefs.Count);
    for f := 0 to FieldDefs.Count - 1 do
      with FieldDefs.Items[f] do
      begin
        names[f] := StringToUtf8(Name);
        sizes[f] := Size;
      end;
  end;
  FieldDefs.Clear;
  if fDataAccess = nil then
    exit;
  for f := 0 to fDataAccess.ColumnCount - 1 do
    with fDataAccess.Columns[f] do
    begin
      if names <> nil then
      begin
        custom := FindRawUtf8(names, ColumnName);
        if custom >= 0 then // retrieve custom max column length from FieldDefs
          ColumnDataSize := sizes[custom];
      end;
      case ColumnType of
        mormot.db.core.ftInt64:
          dbtype := ftLargeint;
        mormot.db.core.ftDate:
          dbtype := ftDateTime;
        mormot.db.core.ftUtf8:
          if ColumnDataSize = 0 then
            dbtype := ftDefaultMemo
          else // no size
            dbtype := ftWideString; // means UnicodeString for Delphi 2009+
        mormot.db.core.ftBlob:
          dbtype := ftBlob;
        mormot.db.core.ftDouble,
        mormot.db.core.ftCurrency:
          dbtype := ftFloat;
      else
        raise EVirtualDataSet.CreateUtf8('%.GetFieldData ColumnType=%',
          [self, TSqlDBFieldTypeToString(ColumnType)]);
      end;
      FieldDefs.Add(Utf8ToString(ColumnName), dbtype, ColumnDataSize);
    end;
end;

function TBinaryDataSet.GetRowFieldData(Field: TField; RowIndex: integer;
  out ResultLen: integer; OnlyCheckNull: boolean): pointer;
var
  f: PtrInt;
begin
  result := nil;
  f := Field.Index;
  if (fDataAccess = nil) or
     not fDataAccess.GotoRow(RowIndex) then
    exit;
  result := fDataAccess.ColumnData(f);
  if (result <> nil) and not OnlyCheckNull then
    case fDataAccess.Columns[f].ColumnType of
      mormot.db.core.ftInt64:
        begin
          fTemp64 := FromVarInt64(PByte(result));
          result := @fTemp64;
        end;
      mormot.db.core.ftCurrency:
        begin
          // ftFloat expects a DOUBLE value
          PDouble(@fTemp64)^ := PCurrency(result)^;
          result := @fTemp64;
        end;
      mormot.db.core.ftUtf8,
      mormot.db.core.ftBlob:
        ResultLen := FromVarUInt32(PByte(result));
    end; // other ColumnTypes are already in the expected format
end;

function TBinaryDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
begin
  if fDataAccess <> nil then
    result := fDataAccess.ColumnSearch(
      fDataAccess.ColumnIndex(aLookupFieldName), aLookupValue,
      loCaseInsensitive in aOptions)
  else
    result := 0;
end;


{************ TSqlDataSet For Direct TSqlDBConnection Sql Execution }

{ TSqlDataSet }

procedure TSqlDataSet.From(Statement: TSqlDBStatement; MaxRowCount: cardinal;
  IgnoreColumnDataSize: boolean);
begin
  inherited From(Statement, MaxRowCount, IgnoreColumnDataSize);
  fConnection := Statement.Connection.Properties;
end;

procedure TSqlDataSet.InternalClose;
begin
  inherited InternalClose;
  FreeAndNil(fDataAccess);
  fData := '';
end;

procedure TSqlDataSet.InternalOpen;
var
  rows: ISqlDBRows;
begin
  if fCommandText = '' then
  begin
    if fData <> '' then // called e.g. after From() method
      inherited InternalOpen;
    exit;
  end;
  rows := fConnection.ExecuteInlined(StringToUtf8(fCommandText), true);
  if rows <> nil then
  begin
    From(rows.Instance);
    inherited InternalOpen;
  end;
end;

{$ifdef ISDELPHIXE3}
function TSqlDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): integer;
var
  ds: TDataSet;
begin
  ds := nil;
  result := PSExecuteStatement(ASQL, AParams, ds);
  ds.Free;
end;

function TSqlDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  var ResultSet: TDataSet): integer;
{$else}
function TSqlDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: pointer): integer;
{$endif ISDELPHIXE3}
var
  stmt: ISqlDBStatement;
  blob: TBlobData;
  sql: RawUtf8;
  p: integer;
begin // only execute writes in current implementation
  StringToUtf8(ASQL, sql);
  if fConnection = nil then
    raise ESqlDataSet.CreateUtf8(
      '%.PSExecuteStatement with Connection=nil [%]', [self, sql]);
  stmt := fConnection.NewThreadSafeStatementPrepared(sql, false);
  if stmt <> nil then
  try
    if AParams <> nil then
      for p := 0 to AParams.Count - 1 do
        if AParams[p].DataType = ftBlob then
        begin
          blob := AParams[p].AsBlob;
          stmt.BindBlob(p + 1, pointer(blob), length(blob));
        end
        else
          stmt.BindVariant(p + 1, AParams[p].Value, False);
    stmt.ExecutePrepared;
    result := stmt.UpdateCount;
    if result = 0 then
      result := 1; // optimistic result, even if SynDB returned 0
  except
    result := 0;
  end
  else
    result := 0;
end;

function TSqlDataSet.PSGetTableName: string;
var
  sql, table: RawUtf8;
begin
  StringToUtf8(fCommandText, sql);
  table := GetTableNameFromSqlSelect(sql, {uniquetablein:}false);
  Utf8ToStringVar(table, result);
end;

function TSqlDataSet.PSIsSqlBased: Boolean;
begin
  result := true;
end;

function TSqlDataSet.PSIsSqlSupported: Boolean;
begin
  result := true;
end;

procedure TSqlDataSet.PSSetCommandText(const ACommandText: string);
begin
  inherited;
  fCommandText := ACommandText;
end;

function TSqlDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  result := false;
end;

function ToDataSet(aOwner: TComponent; aStatement: TSqlDBStatement;
  aMaxRowCount: integer): TBinaryDataSet;
begin
  result := TBinaryDataSet.Create(aOwner);
  result.From(aStatement, aMaxRowCount);
  result.Open;
end;

function ToDataSet(aOwner: TComponent; const aStatement: ISqlDBRows;
  aMaxRowCount: integer): TBinaryDataSet;
begin
  if aStatement = nil then
    result := nil
  else
    result := ToDataSet(aOwner, aStatement.Instance, aMaxRowCount);
end;

function BinaryToDataSet(aOwner: TComponent;
  const aBinaryData: RawByteString): TBinaryDataSet;
begin
  result := TBinaryDataSet.Create(aOwner);
  result.From(aBinaryData);
  result.Open;
end;



end.

