/// Virtual TDataset Component Compatible With VCL/LCL/FMX UI
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.rad;

{
  *****************************************************************************

   Efficient Read/Only Abstract TDataSet for VCL/LCL/FMX UI
    - Cross-Compiler TVirtualDataSet Read/Only Data Access
    - JSON and Variants TDataSet Support

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
  {$ifdef ISDELPHIXE2}
  Data.DB,
  Data.FMTBcd;
  {$else}
  DB,
  FMTBcd;
  {$endif ISDELPHIXE2}



{************ Cross-Compiler TVirtualDataSet Read/Only Data Access }

type
  /// exception class raised by this unit
  EVirtualDataSet = class(ESynException);

  /// read-only virtual TDataSet able to access any content
  // - inherited classes should override InternalOpen, InternalInitFieldDefs,
  // GetRecordCount, GetRowFieldData abstract virtual methods, and optionally
  // SearchForField
  TVirtualDataSet = class(TDataSet)
  protected
    fCurrentRow: integer;
    fIsCursorOpen: boolean;

    // TDataSet overridden methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    function GetCanModify: boolean; override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: pointer); override;
    procedure InternalHandleException; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: pointer); override;
    procedure SetRecNo(Value: integer); override;
    function GetRecNo: integer; override;

    procedure InternalOpen; override;
    // result should point to Int64,Double,Blob,Utf8 data (if ResultLen<>nil)
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: integer; OnlyCheckNull: boolean): pointer; virtual; abstract;
    // search for a field value, returning RecNo (0 = not found by default)
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; virtual;
    // used to serialize TBcdVariant as JSON
    class procedure BcdWrite(const aWriter: TTextWriter; const aValue);
  public
    /// this overridden constructor will compute an unique Name property
    constructor Create(Owner: TComponent); override;
    /// get BLOB column data for the current active row
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    /// get BLOB column data for a given row (may not the active row)
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function GetBlobStream(Field: TField; RowIndex: integer): TStream;
    /// get column data for the current active row
    // - handle ftBoolean,ftInteger,ftLargeint,ftFloat,ftCurrency,ftDate,ftTime,
    // ftDateTime,ftString,ftWideString kind of fields via GetRowFieldData()
    {$ifdef ISDELPHIXE3}
    {$ifdef ISDELPHIXE4}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean; override;
    {$else}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): boolean; override;
    {$endif ISDELPHIXE4}
    {$else}
    function GetFieldData(Field: TField; Buffer: pointer): boolean; override;
    {$endif ISDELPHIXE3}
    {$ifndef UNICODE}
    function GetFieldData(Field: TField; Buffer: pointer;
      NativeFormat: boolean): boolean; override;
    {$endif UNICODE}
    /// searching a dataset for a specified record and making it the active record
    // - will call SearchForField protected virtual method for actual lookup
    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): boolean; override;
  published
    property Active;
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
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;


{************ JSON and Variants TDataSet Support }

/// export all rows of a TDataSet into JSON
// - will work for any kind of TDataSet
function DataSetToJson(Data: TDataSet): RawJson;

type
  TDocVariantArrayDataSetColumn = record
    Name: RawUtf8;
    FieldType: TSqlDBFieldType;
  end;
  PDocVariantArrayDataSetColumn = ^TDocVariantArrayDataSetColumn;

  /// read-only virtual TDataSet able to access a dynamic array of TDocVariant
  // - could be used e.g. from the result of TMongoCollection.FindDocs() to
  // avoid most temporary conversion into JSON or TClientDataSet buffers
  TDocVariantArrayDataSet = class(TVirtualDataSet)
  protected
    fValues: TVariantDynArray;
    fColumns: array of TDocVariantArrayDataSetColumn;
    fValuesCount: integer;
    fTemp64: Int64;
    fTempUtf8: RawUtf8;
    fTempBlob: RawByteString;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: integer; OnlyCheckNull: boolean): pointer; override;
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a dynamic array of TDocVariant
    // - you can set the expected column names and types matching the results
    // document layout; if no column information is specified, the first
    // TDocVariant object will be used as reference
    constructor Create(Owner: TComponent;
      const Data: TVariantDynArray; DataCount: integer;
      const ColumnNames: array of RawUtf8;
      const ColumnTypes: array of TSqlDBFieldType); reintroduce;
  end;

/// convert a dynamic array of TDocVariant result into a LCL/VCL DataSet
// - this function is just a wrapper around TDocVariantArrayDataSet.Create()
// - the TDataSet will be opened once created
function VariantsToDataSet(aOwner: TComponent;
  const Data: TVariantDynArray; DataCount: integer;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet; overload;

/// convert a dynamic array of TDocVariant result into a LCL/VCL DataSet
function VariantsToDataSet(aOwner: TComponent;
  const Data: TVariantDynArray): TDocVariantArrayDataSet; overload;

/// convert a TDocVariant array and associated columns name/type
// into a LCL/VCL TDataSet
function DocVariantToDataSet(aOwner: TComponent;
  const DocVariant: variant;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet; overload;

/// convert a TDocVariant array into a LCL/VCL TDataSet
// - return nil if the supplied DocVariant is not a dvArray
// - field types are guessed from the first TDocVariant array item
function DocVariantToDataSet(aOwner: TComponent;
  const DocVariant: variant): TDocVariantArrayDataSet; overload;


implementation


{************ Cross-Compiler TVirtualDataSet Read/Only Data Access }

var
  GlobalDataSetCount: integer;

type
  /// define how a single row is identified
  // - for TVirtualDataSet, it is just the row index (starting at 0)
  TRecInfoIdentifier = integer;
  PRecInfoIdentifier = ^TRecInfoIdentifier;

  /// pointer to an internal structure used to identify a row position
  PRecInfo = ^TRecInfo;

  /// internal structure used to identify a row position
  TRecInfo = record
    /// define how a single row is identified
    RowIndentifier: TRecInfoIdentifier;
    /// any associated bookmark
    Bookmark: TRecInfoIdentifier;
    /// any associated bookmark flag
    BookmarkFlag: TBookmarkFlag;
  end;


{ TVirtualDataSet }

constructor TVirtualDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  // ensure unique component name
  Name := ClassName + IntToStr(InterlockedIncrement(GlobalDataSetCount)); 
end;

function TVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := AllocMem(SizeOf(TRecInfo));
end;

procedure TVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TVirtualDataSet.GetBookmarkData(
  Buffer: TRecordBuffer; Data: pointer);
begin
  PRecInfoIdentifier(Data)^ := PRecInfo(Buffer)^.Bookmark;
end;

function TVirtualDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  result := PRecInfo(Buffer)^.BookmarkFlag;
end;

function TVirtualDataSet.GetCanModify: boolean;
begin
  result := false; // we define a READ-ONLY TDataSet
end;

{$ifndef UNICODE}
function TVirtualDataSet.GetFieldData(Field: TField; Buffer: pointer;
  NativeFormat: boolean): boolean;
begin
  if Field.DataType in [ftWideString] then
    NativeFormat := true; // to force Buffer as PWideString
  result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$endif UNICODE}

{$ifdef ISDELPHIXE3}
{$ifdef ISDELPHIXE4}
function TVirtualDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean;
{$else}
function TVirtualDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): boolean;
{$endif ISDELPHIXE4}
{$else}
function TVirtualDataSet.GetFieldData(Field: TField; Buffer: pointer): boolean;
{$endif ISDELPHIXE3}
var
  data, dest: pointer;
  ndx, len, maxlen: integer;
  tmp: RawByteString;
  onlytestfornull: boolean;
  ts: TTimeStamp;
begin
  onlytestfornull := (Buffer = nil);
  ndx := PRecInfo(ActiveBuffer).RowIndentifier;
  data := GetRowFieldData(Field, ndx, len, onlytestfornull);
  result := data <> nil; // null field or out-of-range ndx/Field
  if onlytestfornull or
     not result then
    exit;
  dest := pointer(Buffer); // works also if Buffer is [var] TValueBuffer=TArray<byte>
  case Field.DataType of // data^ points to Int64,Double,Blob,Utf8
    ftBoolean:
      PWordBool(dest)^ := PBoolean(data)^;
    ftInteger:
      PInteger(dest)^ := PInteger(data)^;
    ftLargeint,
    ftFloat,
    ftCurrency:
      PInt64(dest)^ := PInt64(data)^;
    ftDate,
    ftTime,
    ftDateTime:
      if PDateTime(data)^ = 0 then // handle 30/12/1899 date as NULL
        result := false
      else
      begin
        // inlined DataConvert(Field,data,dest,true)
        ts := DateTimeToTimeStamp(PDateTime(data)^);
        case Field.DataType of
          ftDate:
            PDateTimeRec(dest)^.Date := ts.Date;
          ftTime:
            PDateTimeRec(dest)^.Time := ts.Time;
          ftDateTime:
            if (ts.Time < 0) or
               (ts.Date <= 0) then // matches ValidateTimeStamp() expectations
              result := false
            else
              PDateTimeRec(dest)^.DateTime := TimeStampToMSecs(ts);
        end; // see NativeToDateTime/DateTimeToNative in TDataSet.DataConvert
      end;
    ftString:
      begin
        if len <> 0 then
        begin
          CurrentAnsiConvert.Utf8BufferToAnsi(data, len, tmp);
          len := length(tmp);
          maxlen := Field.DataSize - 1; // without trailing #0
          if len > maxlen then
            len := maxlen;
          MoveFast(pointer(tmp)^, dest^, len);
        end;
        PAnsiChar(dest)[len] := #0;
      end;
    ftWideString:
      begin
        {$ifdef ISDELPHI2007ANDUP} // here dest = PWideChar[] of DataSize bytes
        if len = 0 then
          PWideChar(dest)^ := #0
        else
          Utf8ToWideChar(dest, data, (Field.DataSize - 2) shr 1, len);
        {$else}
        // here dest is PWideString
        Utf8ToWideString(data, len, WideString(dest^));
        {$endif ISDELPHI2007ANDUP}
      end;
  // ftBlob,ftMemo,ftWideMemo should be retrieved by CreateBlobStream()
  else
    raise EVirtualDataSet.CreateUtf8(
      '%.GetFieldData unhandled DataType=% (%)',
      [self, GetEnumName(TypeInfo(TFieldType), ord(Field.DataType))^,
       ord(Field.DataType)]);
  end;
end;

function TVirtualDataSet.GetBlobStream(Field: TField;
  RowIndex: integer): TStream;
var
  data: pointer;
  len: integer;
begin
  data := GetRowFieldData(Field, RowIndex, len, false);
  if data = nil then // should point to Blob or Utf8 data
    result := nil
  else
    case Field.DataType of
      ftBlob:
        result := TSynMemoryStream.Create(data, len);
      ftMemo,
      ftString:
        result := TRawByteStringStream.Create(
          CurrentAnsiConvert.Utf8BufferToAnsi(data, len));
      {$ifdef HASDBFTWIDE}
      ftWideMemo,
      {$endif HASDBFTWIDE}
      ftWideString:
        result := TRawByteStringStream.Create(
          Utf8DecodeToRawUnicode(data, len));
    else
      raise EVirtualDataSet.CreateUtf8('%.CreateBlobStream DataType=%',
        [self, ord(Field.DataType)]);
    end;
end;

function TVirtualDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  if Mode <> bmRead then
    raise EVirtualDataSet.CreateUtf8('% BLOB should be ReadOnly', [self]);
  result := GetBlobStream(Field, PRecInfo(ActiveBuffer).RowIndentifier);
  if result = nil then
    result := TSynMemoryStream.Create; // null BLOB returns a void TStream
end;

function TVirtualDataSet.GetRecNo: integer;
begin
  result := fCurrentRow + 1;
end;

function TVirtualDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
begin
  result := grOK;
  case GetMode of
    gmPrior:
      if fCurrentRow > 0 then
        dec(fCurrentRow)
      else
        result := grBOF;
    gmCurrent:
      if fCurrentRow < 0 then
        result := grBOF
      else if fCurrentRow >= GetRecordCount then
        result := grEOF;
    gmNext:
      if fCurrentRow < GetRecordCount - 1 then
        inc(fCurrentRow)
      else
        result := grEOF;
  end;
  if result = grOK then
    with PRecInfo(Buffer)^ do
    begin
      RowIndentifier := fCurrentRow;
      BookmarkFlag := bfCurrent;
      Bookmark := fCurrentRow;
    end;
end;

function TVirtualDataSet.GetRecordSize: Word;
begin
  result := SizeOf(TRecInfoIdentifier); // excluding Bookmark information
end;

procedure TVirtualDataSet.InternalClose;
begin
  BindFields(false);
  {$ifdef ISDELPHIXE6}
  if not (lcPersistent in Fields.LifeCycles) then
  {$else}
  if DefaultFields then
  {$endif ISDELPHIXE6}
    DestroyFields;
  fIsCursorOpen := False;
end;

procedure TVirtualDataSet.InternalFirst;
begin
  fCurrentRow := -1;
end;

procedure TVirtualDataSet.InternalGotoBookmark(Bookmark: pointer);
begin
  fCurrentRow := PRecInfoIdentifier(Bookmark)^;
end;

procedure TVirtualDataSet.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(ExceptObject)
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

procedure TVirtualDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillcharFast(Buffer^, SizeOf(TRecInfo), 0);
end;

procedure TVirtualDataSet.InternalLast;
begin
  fCurrentRow := GetRecordCount;
end;

procedure TVirtualDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(TRecInfo) - SizeOf(TRecInfoIdentifier);
  InternalInitFieldDefs;
  {$ifdef ISDELPHIXE6}
  if not (lcPersistent in Fields.LifeCycles) then
  {$else}
  if DefaultFields then
  {$endif}
    CreateFields;
  BindFields(true);
  fCurrentRow := -1;
  fIsCursorOpen := True;
end;

procedure TVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fCurrentRow := PRecInfo(Buffer).RowIndentifier;
end;

function TVirtualDataSet.IsCursorOpen: boolean;
begin
  result := fIsCursorOpen;
end;

procedure TVirtualDataSet.SetBookmarkData(
  Buffer: TRecordBuffer; Data: pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PRecInfoIdentifier(Data)^;
end;

procedure TVirtualDataSet.SetBookmarkFlag(
  Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TVirtualDataSet.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  if Value <> RecNo then
  begin
    dec(Value);
    if cardinal(Value) >= cardinal(GetRecordCount) then
      raise EVirtualDataSet.CreateUtf8(
        '%.SetRecNo(%) with Count=%', [self, Value + 1, GetRecordCount]);
    DoBeforeScroll;
    fCurrentRow := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

function TVirtualDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
begin
  result := 0; // nothing found
end;

function TVirtualDataSet.Locate(const KeyFields: string;
  const KeyValues: variant; Options: TLocateOptions): boolean;
var
  i, l, h, found: integer;
  fields: TDatasetGetFieldList;
begin
  CheckActive;
  result := true;
  if not IsEmpty then
    if VarIsArray(KeyValues) then
    begin
      fields := TDatasetGetFieldList.Create;
      try
        GetFieldList(fields, KeyFields);
        l := VarArrayLowBound(KeyValues, 1);
        h := VarArrayHighBound(KeyValues, 1);
        if (fields.Count = 1) and
           (l < h) then
        begin
          found := SearchForField(StringToUtf8(KeyFields), KeyValues, Options);
          if found > 0 then
          begin
            RecNo := found;
            exit;
          end;
        end
        else
          for i := 0 to fields.Count - 1 do
          begin
            found := SearchForField(
              StringToUtf8(TField(fields[i]).FieldName),
              KeyValues[l + i], Options);
            if found > 0 then
            begin
              RecNo := found;
              exit;
            end;
          end;
      finally
        fields.Free;
      end;
    end
    else
    begin
      found := SearchForField(StringToUtf8(KeyFields), KeyValues, Options);
      if found > 0 then
      begin
        RecNo := found;
        exit;
      end;
    end;
  result := false;
end;

type
  // low-level class as defined in FMTBcd.pas implementation section
  TFMTBcdData = class(TPersistent)
  private
    fBcd: TBcd;
  end;

class procedure TVirtualDataSet.BcdWrite(const aWriter: TTextWriter; const aValue);
begin
  AddBcd(aWriter, TFMTBcdData(TVarData(aValue).VPointer).fBcd);
end;


{ ************ JSON and Variants TDataSet Support }

function DataSetToJson(Data: TDataSet): RawJson;
var
  W: TResultsWriter;
  f: PtrInt;
  blob: TRawByteStringStream;
begin
  result := 'null';
  if Data = nil then
    exit;
  Data.First;
  if Data.Eof then
    exit;
  W := TResultsWriter.Create(nil, true, false, nil, 16384);
  try
    // get col names and types
    SetLength(W.ColNames, Data.FieldCount);
    for f := 0 to high(W.ColNames) do
      StringToUtf8(Data.FieldDefs[f].Name, W.ColNames[f]);
    W.AddColumns;
    W.Add('[');
    repeat
      W.Add('{');
      for f := 0 to Data.FieldCount - 1 do
      begin
        W.AddString(W.ColNames[f]);
        with Data.Fields[f] do
          if IsNull then
            W.AddNull
          else
            case DataType of
              ftBoolean:
                W.Add(AsBoolean);
              ftSmallint,
              ftInteger,
              ftWord,
              ftAutoInc:
                W.Add(AsInteger);
              ftLargeInt:
                W.Add(TLargeIntField(Data.Fields[f]).AsLargeInt);
              ftFloat,
              ftCurrency: // TCurrencyField is sadly a TFloatField
                W.Add(AsFloat, TFloatField(Data.Fields[f]).Precision);
              ftBcd:
                W.AddCurr(AsCurrency);
              ftFMTBcd:
                AddBcd(W, AsBcd);
              ftTimeStamp,
              ftDate,
              ftTime,
              ftDateTime:
                begin
                  W.Add('"');
                  W.AddDateTime(AsDateTime);
                  W.Add('"');
                end;
              ftString,
              ftFixedChar,
              ftMemo,
              ftGuid:
                begin
                  W.Add('"');
                  {$ifdef UNICODE}
                  W.AddAnsiString(AsAnsiString, twJsonEscape);
                  {$else}
                  W.AddAnsiString(AsString, twJsonEscape);
                  {$endif UNICODE}
                  W.Add('"');
                end;
              ftWideString:
                begin
                  W.Add('"');
                  W.AddJsonEscapeW(pointer(TWideStringField(Data.Fields[f]).Value));
                  W.Add('"');
                end;
              ftVariant:
                W.AddVariant(AsVariant);
              ftBytes,
              ftVarBytes,
              ftBlob,
              ftGraphic,
              ftOraBlob,
              ftOraClob:
                begin
                  blob := TRawByteStringStream.Create;
                  try
                    (Data.Fields[f] as TBlobField).SaveToStream(blob);
                    W.WrBase64(pointer(blob.DataString), length(blob.DataString),
                     {withmagic=}true);
                  finally
                    blob.Free;
                  end;
                end;
              {$ifdef HASDBFTWIDE}
              ftWideMemo,
              ftFixedWideChar:
                begin
                  W.Add('"');
                  W.AddJsonEscapeW(pointer(AsWideString));
                  W.Add('"');
                end;
              {$endif HASDBFTWIDE}
              {$ifdef UNICODE}
              ftShortint,
              ftByte:
                W.Add(AsInteger);
              ftLongWord:
                W.AddU(TLongWordField(Data.Fields[f]).Value);
              ftExtended:
                W.AddDouble(AsFloat);
              ftSingle:
                W.Add(AsFloat, SINGLE_PRECISION);
              {$endif UNICODE}
            else
              W.AddNull; // unhandled field type
            end;
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add('}', ',');
      Data.Next;
    until Data.Eof;
    W.CancelLastComma;
    W.Add(']');
    W.SetText(RawUtf8(result));
  finally
    W.Free;
  end;
end;


{ TDocVariantArrayDataSet }

constructor TDocVariantArrayDataSet.Create(Owner: TComponent;
  const Data: TVariantDynArray; DataCount: integer;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType);
var
  n, ndx, j: PtrInt;
  first: PDocVariantData;
  col: PDocVariantArrayDataSetColumn;
begin
  fValues := Data;
  fValuesCount := DataCount;
  n := Length(ColumnNames);
  if n > 0 then
  begin
    // some columns name/type information has been supplied
    if n <> length(ColumnTypes) then
      raise EVirtualDataSet.CreateUtf8(
        '%.Create(ColumnNames<>ColumnTypes)', [self]);
    SetLength(fColumns, n);
    col := pointer(fColumns);
    for ndx := 0 to n - 1 do
    begin
      col^.Name := ColumnNames[ndx];
      col^.FieldType := ColumnTypes[ndx];
      inc(col);
    end;
  end
  else if fValues <> nil then
  begin
    // guess columns name/type from the first supplied TDocVariant
    first := _Safe(fValues[0], dvObject);
    SetLength(fColumns, first^.Count);
    col := pointer(fColumns);
    for ndx := 0 to first^.Count - 1 do
    begin
      col^.Name := first^.Names[ndx];
      col^.FieldType := VariantTypeToSqlDBFieldType(first^.Values[ndx]);
      case col^.FieldType of
        mormot.db.core.ftNull:
          col^.FieldType := mormot.db.core.ftBlob;
        mormot.db.core.ftCurrency:
          // TCurrencyField is a TFloatField
          col^.FieldType := mormot.db.core.ftDouble;
        mormot.db.core.ftInt64:
          // ensure type coherency of whole column
          for j := 1 to first^.Count - 1 do
            if j >= fValuesCount then
              break
            else
              // ensure objects are consistent
              with _Safe(fValues[j], dvObject)^ do
                if (ndx < Length(Names)) and
                   IdemPropNameU(Names[ndx], col^.Name) and
                   (VariantTypeToSqlDBFieldType(Values[ndx]) in
                     [mormot.db.core.ftNull,
                      mormot.db.core.ftDouble,
                      mormot.db.core.ftCurrency]) then
                  begin
                    col^.FieldType := mormot.db.core.ftDouble;
                    break;
                  end;
      end;
      inc(col);
    end;
  end;
  inherited Create(Owner);
end;

function TDocVariantArrayDataSet.GetRecordCount: integer;
begin
  result := fValuesCount;
end;

function TDocVariantArrayDataSet.GetRowFieldData(Field: TField;
  RowIndex: integer; out ResultLen: integer; OnlyCheckNull: boolean): pointer;
var
  f, ndx: PtrInt;
  wasstring: boolean;
  col: PDocVariantArrayDataSetColumn;
  dv: PDocVariantData;
  v: PVariant;
begin
  result := nil; // default is null on error
  f := Field.Index;
  if (cardinal(RowIndex) >= cardinal(fValuesCount)) or
     (cardinal(f) >= cardinal(length(fColumns))) then
    exit;
  col := @fColumns[f];
  if col^.FieldType in
           [mormot.db.core.ftNull,
            mormot.db.core.ftUnknown,
            mormot.db.core.ftCurrency] then
    exit;
  if not _SafeObject(fValues[RowIndex], dv) or
     (dv^.Count = 0) then
    exit;
  if IdemPropNameU(col^.Name, dv^.Names[f]) then
    ndx := f // optimistic match when fields are in-order (most common case)
  else
  begin
    ndx := dv^.GetValueIndex(col^.Name);
    if ndx < 0 then
      exit;
  end;
  v := @dv^.Values[ndx];
  if VarIsEmptyOrNull(v^) then
    exit
  else
  begin
    result := @fTemp64;
    if not OnlyCheckNull then
      case col^.FieldType of
        mormot.db.core.ftInt64:
          VariantToInt64(v^, fTemp64);
        mormot.db.core.ftDouble,
        mormot.db.core.ftDate:
          VariantToDouble(v^, unaligned(PDouble(@fTemp64)^));
        mormot.db.core.ftUtf8:
          begin
            VariantToUtf8(v^, fTempUtf8, wasstring);
            result := pointer(fTempUtf8);
            ResultLen := length(fTempUtf8);
          end;
        mormot.db.core.ftBlob:
          begin
            VariantToUtf8(v^, fTempUtf8, wasstring);
            if Base64MagicCheckAndDecode(
                 pointer(fTempUtf8), length(fTempUtf8), fTempBlob) then
            begin
              result := pointer(fTempBlob);
              ResultLen := length(fTempBlob);
            end;
          end;
      end;
  end;
end;

const
  TO_DB: array[TSqlDBFieldType] of TFieldType =(
    ftWideString, // ftUnknown
    ftWideString, // ftNull
    ftLargeint,   // ftInt64
    ftFloat,      // ftDouble
    ftFloat,      // ftCurrency
    ftDate,       // ftDate
    ftWideString, // ftUtf8
    ftBlob);      // ftBlob

procedure TDocVariantArrayDataSet.InternalInitFieldDefs;
var
  f, fieldsiz: PtrInt;
  fieldname: string;
begin
  FieldDefs.Clear;
  for f := 0 to high(fColumns) do
  begin
    if fColumns[f].FieldType = ftUtf8 then
      fieldsiz := 16
    else
      fieldsiz := 0;
    Utf8ToStringVar(fColumns[f].Name, fieldname);
    FieldDefs.Add(fieldname, TO_DB[fColumns[f].FieldType], fieldsiz);
  end;
end;

function TDocVariantArrayDataSet.SearchForField(
  const aLookupFieldName: RawUtf8; const aLookupValue: variant;
  aOptions: TLocateOptions): integer;
var
  f: integer;
  v: PDocVariantData;
begin
  f := -1; // allows O(1) field lookup for invariant object columns
  for result := 1 to fValuesCount do
    if _SafeObject(fValues[result - 1], v) and
       (v^.Count > 0) then
    begin
      if (cardinal(f) >= cardinal(v^.Count)) or
         not IdemPropNameU(aLookupFieldName, v^.Names[f]) then
        f := v^.GetValueIndex(aLookupFieldName);
      if (f >= 0) and
         (SortDynArrayVariantComp(
           TVarData(v^.Values[f]), TVarData(aLookupValue),
           loCaseInsensitive in aOptions) = 0) then
        exit;
    end;
  result := 0;
end;


function VariantsToDataSet(aOwner: TComponent;
  const Data: TVariantDynArray; DataCount: integer;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet;
begin
  result := TDocVariantArrayDataSet.Create(
    aOwner, Data, DataCount, ColumnNames, ColumnTypes);
  result.Open;
end;

function VariantsToDataSet(aOwner: TComponent;
  const Data: TVariantDynArray): TDocVariantArrayDataSet;
begin
  result := VariantsToDataSet(aOwner, Data, length(Data), [], []);
end;

function DocVariantToDataSet(aOwner: TComponent;
  const DocVariant: variant;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet;
var
  dv: PDocVariantData;
begin
  if _SafeArray(DocVariant, dv) then
    result := VariantsToDataSet(
      aOwner, dv^.Values, dv^.Count, ColumnNames, ColumnTypes)
  else
    result := nil;
end;

function DocVariantToDataSet(aOwner: TComponent;
  const DocVariant: variant): TDocVariantArrayDataSet;
begin
  result := DocVariantToDataSet(aOwner, DocVariant, [], []);
end;




end.

