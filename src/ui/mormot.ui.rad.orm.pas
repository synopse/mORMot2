/// ORM/JSON Aware TDataset Components
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.rad.orm;

{
  *****************************************************************************

   Efficient Read/Only TDataSet for ORM and JSON Process
    - TOrmTableDataSet for TOrmTable/JSON access
    - JSON/ORM to TDataSet Wrappers Functions

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
  mormot.orm.base,
  mormot.orm.core,
  mormot.ui.rad,
  {$ifdef ISDELPHIXE2}
  Data.DB;
  {$else}
  DB;
  {$endif ISDELPHIXE2}



{************ TOrmTableDataSet for TOrmTable/JSON access }

type
  /// read-only virtual TDataSet able to access a TOrmTable
  TOrmTableDataSet = class(TVirtualDataSet)
  protected
    fTable: TOrmTable;
    {$ifndef UNICODE}
    fForceWideString: boolean;
    {$endif UNICODE}
    fTableShouldBeFreed: boolean;
    fTempBlob: RawBlob;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: integer; OnlyCheckNull: boolean): pointer; override;
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a TOrmTable
    // - WARNING: the supplied TOrmTable instance shall remain available
    // all the time the returned TOrmTableDataSet instance is used, unless
    // the TableShouldBeFreed property is set to true or CreateOwnedTable()
    // constructor is used instead
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor Create(Owner: TComponent; OrmTable: TOrmTable
      {$ifndef UNICODE}; ForceWideString: boolean = false{$endif}); reintroduce;
    /// initialize the virtual TDataSet owning a TOrmTable
    // - this constructor will set TableShouldBeFreed to TRUE
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateOwnedTable(Owner: TComponent; OrmTable: TOrmTable
      {$ifndef UNICODE}; ForceWideString: boolean = false{$endif}); reintroduce;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - this constructor will parse the supplied JSON content and create
    // an internal TOrmTableJson instance to process the data, guessing the
    // column types from the JSON content
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJson(Owner: TComponent; const Json: RawUtf8
      {$ifndef UNICODE}; ForceWideString: boolean = false{$endif});
        reintroduce; overload;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - you can set the expected column types matching the results column layout
    // - this constructor will parse the supplied JSON content and create
    // an internal TOrmTableJson instance to process the data
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJson(Owner: TComponent; const Json: RawUtf8;
      const ColumnTypes: array of TOrmFieldType
      {$ifndef UNICODE}; ForceWideString: boolean = false{$endif});
        reintroduce; overload;
    /// initialize the virtual TDataSet from a supplied JSON ORM result
    // - you can set the TOrm classes to retrieve the expected column types
    // - this constructor will parse the supplied JSON content and create
    // an internal TOrmTableJson instance to process the data
    // - with non-Unicode version of Delphi, you can set ForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJson(Owner: TComponent; const Json: RawUtf8;
      const Tables: array of TOrmClass
      {$ifndef UNICODE}; ForceWideString: boolean = false{$endif});
        reintroduce; overload;
    /// finalize the class instance
    destructor Destroy; override;

    /// if the supplied TOrmTable instance should be released with this class
    // - Create() will left to FALSE (meaning that the TOrmTable instance shall
    // remain available all the time the TOrmTableDataSet instance is used)
    // - CreateOwnedTable() will set to TRUE if you want the TOrmTable to be
    //  freed when this TOrmTableDataSet instance will be released
    // - you can also set it after Create(), on purpose
    property TableShouldBeFreed: boolean
      read fTableShouldBeFreed write fTableShouldBeFreed;
    /// access to the internal TOrmTable/TOrmTableJson data
    // - you can use e.g. the SortFields() methods
    // - you may change the table content on the fly, if the column remains the same
    property Table: TOrmTable
      read fTable write fTable;
  end;


{************ JSON/ORM to TDataSet Wrappers Functions }

type
  /// store low-level DB.pas field information
  // - as used by GetDBFieldDef() and GetDBFieldValue() functions
  TDBFieldDef = record
    FieldName: string;
    DBType: TFieldType;
    DBSize: integer;
    SqlType: TOrmFieldType;
    SqlIndex: integer;
    FieldType: POrmTableFieldType;
  end;

/// get low-level DB.pas field information
// - ready to be added to a TDataset as:
// !  aDataSet.FieldDefs.Add(FieldName,DBType,DBSize);
procedure GetDBFieldDef(aTable: TOrmTable; aField: integer;
  out DBFieldDef: TDBFieldDef
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif});

/// fill a DB.pas field content
// - used e.g. by mORMotMidasVCL.ToClientDataSet
procedure GetDBFieldValue(aTable: TOrmTable; aRow: integer; aField: TField;
  aDataSet: TDataSet; const DBFieldDef: TDBFieldDef);

/// convert a JSON result into a VCL DataSet, guessing the field types from JSON
// - this function is just a wrapper around TOrmTableDataSet.CreateFromJson()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JsonToDataSet(aOwner: TComponent; const aJson: RawUtf8
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TOrmTableDataSet;
    overload; {$ifdef HASINLINE} inline;{$endif}

/// convert a JSON ORM result into a VCL DataSet, following TOrm field types
// - this function is just a wrapper around TOrmTableDataSet.CreateFromJson()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JsonTableToDataSet(aOwner: TComponent; const aJson: RawUtf8;
  const Tables: array of TOrmClass
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}): TOrmTableDataSet;

/// convert a JSON result into a VCL DataSet, with a given set of column types
// - this function is just a wrapper around TOrmTableDataSet.CreateFromJson()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JsonToDataSet(aOwner: TComponent; const aJson: RawUtf8;
  const ColumnTypes: array of TOrmFieldType
  {$ifndef UNICODE}; aForceWideString: boolean = false{$endif}):
    TOrmTableDataSet; overload;


implementation


{************ TOrmTableDataSet for TOrmTable/JSON access }

{ TOrmTableDataSet }

constructor TOrmTableDataSet.Create(Owner: TComponent; OrmTable: TOrmTable
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  inherited Create(Owner);
  {$ifndef UNICODE}
  fForceWideString := ForceWideString;
  {$endif UNICODE}
  if OrmTable <> nil then
    fTable := OrmTable;
  Open;
end;

constructor TOrmTableDataSet.CreateOwnedTable(Owner: TComponent;
  OrmTable: TOrmTable {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  Create(Owner, OrmTable {$ifndef UNICODE}, ForceWideString{$endif});
  if OrmTable <> nil then
    fTableShouldBeFreed := true;
end;

constructor TOrmTableDataSet.CreateFromJson(
  Owner: TComponent; const Json: RawUtf8
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var
  T: TOrmTable;
begin
  T := TOrmTableJson.Create('', Json);
  try
    CreateOwnedTable(Owner, T{$ifndef UNICODE}, ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TOrmTableDataSet error
  end;
end;

constructor TOrmTableDataSet.CreateFromJson(Owner: TComponent;
  const Json: RawUtf8; const ColumnTypes: array of TOrmFieldType
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var
  T: TOrmTable;
begin
  T := TOrmTableJson.CreateWithColumnTypes(ColumnTypes, '', Json);
  try
    CreateOwnedTable(Owner, T {$ifndef UNICODE}, ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TOrmTableDataSet error
  end;
end;

constructor TOrmTableDataSet.CreateFromJson(Owner: TComponent;
  const Json: RawUtf8; const Tables: array of TOrmClass
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
var
  T: TOrmTable;
begin
  T := TOrmTableJson.CreateFromTables(Tables, '', Json);
  try
    CreateOwnedTable(Owner, T {$ifndef UNICODE}, ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TOrmTableDataSet error
  end;
end;

destructor TOrmTableDataSet.Destroy;
begin
  inherited;
  if fTableShouldBeFreed then
    FreeAndNil(fTable);
end;

function TOrmTableDataSet.GetRecordCount: integer;
begin
  if fTable <> nil then
    result := fTable.RowCount
  else
    result := 0;
end;

function TOrmTableDataSet.GetRowFieldData(Field: TField; RowIndex: integer;
  out ResultLen: integer; OnlyCheckNull: boolean): pointer;
var
  info: POrmTableFieldType;
  f: integer;
  P: PUtf8Char;
label
  txt;
begin
  result := nil;
  f := Field.Index;
  inc(RowIndex); // first TOrmTable row are field names
  P := fTable.Get(RowIndex, f, ResultLen);
  if P = nil then // null field or out-of-range RowIndex/f -> result := nil
    exit;
  result := @fTemp64; // let result point to Int64, Double or TDatetime
  if OnlyCheckNull then
    exit;
  case fTable.FieldType(f, info) of
    oftBoolean,
    oftInteger,
    oftID,
    oftTID:
      SetInt64(P, fTemp64);
    oftFloat,
    oftCurrency:
      PDouble(@fTemp64)^ := GetExtended(P);
    oftEnumerate,
    oftSet:
      if info^.ContentTypeInfo = nil then
        SetInt64(P, fTemp64)
      else
        goto txt;
    oftDateTime,
    oftDateTimeMS:
      PDouble(@fTemp64)^ := Iso8601ToDateTimePUtf8Char(P, 0);
    oftTimeLog,
    oftModTime,
    oftCreateTime:
      PDouble(@fTemp64)^ := TimeLogToDateTime(GetInt64(P));
    oftUnixTime:
      PDouble(@fTemp64)^ := UnixTimeToDateTime(GetInt64(P));
    oftUnixMSTime:
      PDouble(@fTemp64)^ := UnixMSTimeToDateTime(GetInt64(P));
    oftBlob:
      begin
        fTempBlob := BlobToRawBlob(P);
        result := pointer(fTempBlob);
        ResultLen := length(fTempBlob);
      end;
  else
txt: result := P; // e.g. oftUtf8Text
  end;
end;

procedure TOrmTableDataSet.InternalInitFieldDefs;
var
  f: integer;
  def: TDBFieldDef;
begin
  FieldDefs.Clear;
  for f := 0 to fTable.FieldCount - 1 do
  begin
    GetDBFieldDef(fTable, f, def {$ifndef UNICODE}, fForceWideString{$endif});
    FieldDefs.Add(def.FieldName, def.DBType, def.DBSize);
  end;
end;

function TOrmTableDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
var
  f: PtrInt;
  val: RawUtf8;
begin
  f := Table.FieldIndex(aLookupFieldName);
  if f < 0 then
    result := 0
  else
  begin
    VariantToUtf8(aLookupValue, val);
    if loPartialKey in aOptions then
      result := Table.SearchFieldIdemPChar(val, f)
    else
      result := Table.SearchFieldEquals(
        val, f, 1, not (loCaseInsensitive in aOptions));
  end;
end;

{************ JSON/ORM to TDataSet Wrappers Functions }


procedure GetDBFieldValue(aTable: TOrmTable; aRow: integer; aField: TField;
  aDataSet: TDataSet; const DBFieldDef: TDBFieldDef);
var
  blob: RawBlob;
  sstream, dstream: TStream;
  P: PUtf8Char;
begin
  if (aField <> nil) and
     (aRow > 0) then
    with DBFieldDef do
    begin
      P := aTable.Get(aRow, SqlIndex);
      if P = nil then
        aField.Clear
      else
        case SqlType of
          oftBoolean:
            aField.AsBoolean := GetInt64(P) <> 0;
          oftInteger,
          oftID,
          oftTID,
          oftSessionUserID:
            if aField.DataType = ftLargeInt then // handle Int64 values directly
              TLargeintField(aField).Value := GetInt64(P)
            else
              aField.AsInteger := GetInteger(P);
          oftFloat,
          oftCurrency:
            aField.AsFloat := GetExtended(P);
          oftEnumerate,
          oftSet:
            if FieldType^.ContentTypeInfo = nil then
              aField.AsInteger := GetInteger(P)
            else
              aField.AsString := aTable.GetString(aRow, SqlIndex);
          oftDateTime,
          oftDateTimeMS:
            aField.AsDateTime := Iso8601ToDateTimePUtf8Char(P, 0);
          oftUnixTime:
            aField.AsDateTime := UnixTimeToDateTime(GetInt64(P));
          oftUnixMSTime:
            aField.AsDateTime := UnixMSTimeToDateTime(GetInt64(P));
          oftTimeLog,
          oftModTime,
          oftCreateTime:
            aField.AsDateTime := TimeLogToDateTime(GetInt64(P));
          oftBlob:
            begin
              blob := BlobToRawBlob(P);
              if (blob = '') or
                 (aDataSet = nil) then
                aField.Clear
              else
              begin
                sstream := TRawByteStringStream.Create(blob);
                try
                  dstream := aDataSet.CreateBlobStream(aField, bmWrite);
                  try
                    dstream.CopyFrom(sstream, 0);
                  finally
                    dstream.Free;
                  end;
                finally
                  sstream.Free;
                end;
              end;
            end;
          oftUtf8Text:
            if aField.DataType = ftWideString then
              TWideStringField(aField).Value :=
                aTable.GetSynUnicode(aRow, SqlIndex)
            else
              aField.AsString := aTable.GetString(aRow, SqlIndex);
        else
          aField.AsVariant := aTable.GetVariant(aRow, SqlIndex);
        end;
    end;
end;

procedure GetDBFieldDef(aTable: TOrmTable; aField: integer;
  out DBFieldDef: TDBFieldDef
  {$ifndef UNICODE}; aForceWideString: boolean{$endif});
begin
  with DBFieldDef do
  begin
    DBSize := 0;
    SqlIndex := aField;
    FieldName := aTable.GetString(0, aField);
    if FieldName = '' then
    begin
      DBType := ftUnknown;
      SqlType := oftUnknown;
    end
    else
    begin
      SqlType := aTable.FieldType(aField, FieldType);
      case SqlType of
        oftBoolean:
          DBType := ftBoolean;
        oftInteger,
        oftID,
        oftTID:
          DBType := ftLargeint; // LargeInt=Int64
        oftFloat,
        oftCurrency:
          DBType := ftFloat;
        oftEnumerate,
        oftSet:
          if FieldType^.ContentTypeInfo = nil then
            DBType := ftInteger
          else
          begin
            DBSize := 64;
            DBType := ftDefaultVCLString;
          end;
        oftRecord:
          begin
            DBSize := 64;
            DBType := ftDefaultVCLString;
          end;
        oftDateTime,
        oftDateTimeMS,
        oftUnixTime,
        oftUnixMSTime,
        oftTimeLog,
        oftModTime,
        oftCreateTime:
          DBType := ftDateTime;
        oftBlob:
          begin
            DBSize := (aTable.FieldLengthMax(aField, true) * 3) shr 2;
            DBType := ftBlob;
          end;
        oftUtf8Text:
          begin
            DBSize := aTable.FieldLengthMax(aField, true);
            {$ifndef UNICODE}
            // for Delphi 2009+ TWideStringField = UnicodeString!
            if aForceWideString then
              DBType := ftWideString
            else
            {$endif UNICODE}
              DBType := ftDefaultVCLString;
          end;
      else
        begin
          DBType := ftDefaultVCLString;
          DBSize := aTable.FieldLengthMax(aField, true);
        end;
      end;
    end;
  end;
end;


function JsonToDataSet(aOwner: TComponent; const aJson: RawUtf8
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TOrmTableDataSet;
begin
  result := TOrmTableDataSet.CreateFromJson(aOwner, aJson
    {$ifndef UNICODE}, aForceWideString{$endif});
end;

function JsonToDataSet(aOwner: TComponent; const aJson: RawUtf8;
  const ColumnTypes: array of TOrmFieldType
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TOrmTableDataSet;
begin
  result := TOrmTableDataSet.CreateFromJson(aOwner, aJson, ColumnTypes
    {$ifndef UNICODE}, aForceWideString{$endif});
end;

function JsonTableToDataSet(aOwner: TComponent;
  const aJson: RawUtf8; const Tables: array of TOrmClass
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TOrmTableDataSet;
begin
  result := TOrmTableDataSet.CreateFromJson(aOwner, aJson, Tables
    {$ifndef UNICODE}, aForceWideString{$endif});
end;



end.

