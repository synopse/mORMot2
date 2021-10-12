/// Virtual TDataset Component Compatible With VCL/LCL/FMX UI
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.ui.rad;

{
  *****************************************************************************

   Efficient Read/Only TDataSet for VCL/LCL/FMX UI
    - Cross-Compiler TSynVirtualDataSet Read/Only Data Access
    - Database-Aware BCD Values Support
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
  {$ifdef ISDELPHIXE2}
  System.Generics.Collections,
  Data.DB,
  Data.FMTBcd;
  {$else}
  DB,
  FMTBcd;
  {$endif ISDELPHIXE2}



{************ Cross-Compiler TSynVirtualDataSet Read/Only Data Access }

type
  /// exception class raised by this unit
  ESynVirtualDataSet = class(ESynException);

  {$ifndef UNICODE} // defined as TRecordBuffer = PByte in newer DB.pas
  TRecordBuffer = PChar;
  {$endif UNICODE}

  PDateTimeRec = ^TDateTimeRec;

  /// read-only virtual TDataSet able to access any content
  // - inherited classes should override InternalOpen, InternalInitFieldDefs,
  // GetRecordCount, GetRowFieldData abstract virtual methods, and optionally
  // SearchForField
  TSynVirtualDataSet = class(TDataSet)
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
    class procedure BcdWrite(const aWriter: TBaseWriter; const aValue);
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
    function Locate(const KeyFields: string; const KeyValues: Variant;
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

const
  /// map the LCL/VCL string type, depending on the Delphi compiler version
  {$ifdef UNICODE}
  ftDefaultVCLString = ftWideString;
  {$else}
  ftDefaultVCLString = ftString;
  {$endif UNICODE}
  /// if you prefer LCL/Lazarus
  ftDefaultLCLString = ftDefaultVCLString;

  /// map the best ft*Memo type available, depending on the Delphi compiler version
  {$ifdef HASDBFTWIDE}
  ftDefaultMemo = ftWideMemo;
  {$else}
  ftDefaultMemo = ftMemo;
  {$endif HASDBFTWIDE}


{************ Database-Aware BCD Values Support }

/// append a TBcd value as text to the output buffer
// - very optimized for speed
procedure AddBcd(WR: TBaseWriter; const AValue: TBcd);

type
  /// a string buffer, used by InternalBcdToBuffer to store its output text
  TBcdBuffer = array[0..66] of AnsiChar;

/// convert a TBcd value as text to the output buffer
// - buffer is to be TBcdBuffer, i.e. a static array[0..66] of AnsiChar
// - returns the resulting text start in PBeg, and the length as function result
// - does not handle negative sign and 0 value - see AddBcd() function use case
// - very optimized for speed
function InternalBcdToBuffer(const AValue: TBcd; out ADest: TBcdBuffer;
  var PBeg: PAnsiChar): integer;

/// convert a TBcd value into a currency
// - purepascal version included in latest Delphi versions is slower than this
function BcdToCurr(const AValue: TBcd; var Curr: Currency): boolean;

/// convert a TBcd value into a RawUtf8 text
// - will call fast InternalBcdToBuffer function
procedure BcdToUtf8(const AValue: TBcd; var result: RawUtf8); overload;

/// convert a TBcd value into a RawUtf8 text
// - will call fast InternalBcdToBuffer function
function BcdToUtf8(const AValue: TBcd): RawUtf8; overload;
  {$ifdef HASINLINE} inline;{$endif}

/// convert a TBcd value into a LCL/VCL string text
// - will call fast InternalBcdToBuffer function
function BcdToString(const AValue: TBcd): string;


{************ JSON and Variants TDataSet Support }

/// export all rows of a TDataSet into JSON
// - will work for any kind of TDataSet
function DataSetToJson(Data: TDataSet): RawUtf8;

type
  /// read-only virtual TDataSet able to access a dynamic array of TDocVariant
  // - could be used e.g. from the result of TMongoCollection.FindDocs() to
  // avoid most temporary conversion into JSON or TClientDataSet buffers
  TDocVariantArrayDataSet = class(TSynVirtualDataSet)
  protected
    fValues: TVariantDynArray;
    fColumns: array of record
      Name: RawUtf8;
      FieldType: TSqlDBFieldType;
    end;
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
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet;

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


{************ Cross-Compiler TSynVirtualDataSet Read/Only Data Access }

var
  GlobalDataSetCount: integer;

type
  /// define how a single row is identified
  // - for TSynVirtualDataSet, it is just the row index (starting at 0)
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


{ TSynVirtualDataSet }

constructor TSynVirtualDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inc(GlobalDataSetCount);
  Name := ClassName + IntToStr(GlobalDataSetCount); // force unique name
end;

function TSynVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := AllocMem(SizeOf(TRecInfo));
end;

procedure TSynVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TSynVirtualDataSet.GetBookmarkData(
  Buffer: TRecordBuffer; Data: pointer);
begin
  PRecInfoIdentifier(Data)^ := PRecInfo(Buffer)^.Bookmark;
end;

function TSynVirtualDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  result := PRecInfo(Buffer)^.BookmarkFlag;
end;

function TSynVirtualDataSet.GetCanModify: boolean;
begin
  result := false; // we define a READ-ONLY TDataSet
end;

{$ifndef UNICODE}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: pointer;
  NativeFormat: boolean): boolean;
begin
  if Field.DataType in [ftWideString] then
    NativeFormat := true; // to force Buffer as PWideString
  result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$endif UNICODE}

{$ifdef ISDELPHIXE3}
{$ifdef ISDELPHIXE4}
function TSynVirtualDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): boolean;
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): boolean;
{$endif ISDELPHIXE4}
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: pointer): boolean;
{$endif ISDELPHIXE3}
var
  Data, Dest: pointer;
  RowIndex, DataLen, MaxLen: integer;
  Temp: RawByteString;
  OnlyTestForNull: boolean;
  TS: TTimeStamp;
begin
  OnlyTestForNull := (Buffer = nil);
  RowIndex := PRecInfo(ActiveBuffer).RowIndentifier;
  Data := GetRowFieldData(Field, RowIndex, DataLen, OnlyTestForNull);
  result := Data <> nil; // null field or out-of-range RowIndex/Field
  if OnlyTestForNull or
     not result then
    exit;
  Dest := pointer(Buffer); // works also if Buffer is [var] TValueBuffer
  case Field.DataType of // Data^ points to Int64,Double,Blob,Utf8
    ftBoolean:
      PWordBool(Dest)^ := PBoolean(Data)^;
    ftInteger:
      PInteger(Dest)^ := PInteger(Data)^;
    ftLargeint,
    ftFloat,
    ftCurrency:
      PInt64(Dest)^ := PInt64(Data)^;
    ftDate,
    ftTime,
    ftDateTime:
      if PDateTime(Data)^ = 0 then // handle 30/12/1899 date as NULL
        result := false
      else
      begin
        // inlined DataConvert(Field,Data,Dest,true)
        TS := DateTimeToTimeStamp(PDateTime(Data)^);
        case Field.DataType of
          ftDate:
            PDateTimeRec(Dest)^.Date := TS.Date;
          ftTime:
            PDateTimeRec(Dest)^.Time := TS.Time;
          ftDateTime:
            if (TS.Time < 0) or
               (TS.Date <= 0) then // matches ValidateTimeStamp() expectations
              result := false
            else
              PDateTimeRec(Dest)^.DateTime := TimeStampToMSecs(TS);
        end; // see NativeToDateTime/DateTimeToNative in TDataSet.DataConvert
      end;
    ftString:
      begin
        if DataLen <> 0 then
        begin
          CurrentAnsiConvert.Utf8BufferToAnsi(Data, DataLen, Temp);
          DataLen := length(Temp);
          MaxLen := Field.DataSize - 1; // without trailing #0
          if DataLen > MaxLen then
            DataLen := MaxLen;
          MoveFast(pointer(Temp)^, Dest^, DataLen);
        end;
        PAnsiChar(Dest)[DataLen] := #0;
      end;
    ftWideString:
      begin
        {$ifdef ISDELPHI2007ANDUP} // here Dest = PWideChar[] of DataSize bytes
        if DataLen = 0 then
          PWideChar(Dest)^ := #0
        else
          Utf8ToWideChar(Dest, Data, (Field.DataSize - 2) shr 1, DataLen);
        {$else}
        // here Dest is PWideString
        Utf8ToWideString(Data, DataLen, WideString(Dest^));
        {$endif ISDELPHI2007ANDUP}
      end;
  // ftBlob,ftMemo,ftWideMemo should be retrieved by CreateBlobStream()
  else
    raise ESynVirtualDataSet.CreateUtf8(
      '%.GetFieldData unhandled DataType=% (%)',
      [self, GetEnumName(TypeInfo(TFieldType), ord(Field.DataType))^,
       ord(Field.DataType)]);
  end;
end;

function TSynVirtualDataSet.GetBlobStream(Field: TField;
  RowIndex: integer): TStream;
var
  Data: pointer;
  DataLen: integer;
begin
  Data := GetRowFieldData(Field, RowIndex, DataLen, false);
  if Data = nil then // should point to Blob or Utf8 data
    result := nil
  else
    case Field.DataType of
      ftBlob:
        result := TSynMemoryStream.Create(Data, DataLen);
      ftMemo, ftString:
        result := TRawByteStringStream.Create(CurrentAnsiConvert.Utf8BufferToAnsi
          (Data, DataLen));
      {$ifdef HASDBFTWIDE}
      ftWideMemo,
      {$endif HASDBFTWIDE}
      ftWideString:
        result := TRawByteStringStream.Create(Utf8DecodeToRawUnicode(Data, DataLen));
    else
      raise ESynVirtualDataSet.CreateUtf8('%.CreateBlobStream DataType=%',
        [self, ord(Field.DataType)]);
    end;
end;

function TSynVirtualDataSet.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  if Mode <> bmRead then
    raise ESynVirtualDataSet.CreateUtf8('% BLOB should be ReadOnly', [self]);
  result := GetBlobStream(Field, PRecInfo(ActiveBuffer).RowIndentifier);
  if result = nil then
    result := TSynMemoryStream.Create; // null BLOB returns a void TStream
end;

function TSynVirtualDataSet.GetRecNo: integer;
begin
  result := fCurrentRow + 1;
end;

function TSynVirtualDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
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

function TSynVirtualDataSet.GetRecordSize: Word;
begin
  result := SizeOf(TRecInfoIdentifier); // excluding Bookmark information
end;

procedure TSynVirtualDataSet.InternalClose;
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

procedure TSynVirtualDataSet.InternalFirst;
begin
  fCurrentRow := -1;
end;

procedure TSynVirtualDataSet.InternalGotoBookmark(Bookmark: pointer);
begin
  fCurrentRow := PRecInfoIdentifier(Bookmark)^;
end;

procedure TSynVirtualDataSet.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(ExceptObject)
  else
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

procedure TSynVirtualDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillcharFast(Buffer^, SizeOf(TRecInfo), 0);
end;

procedure TSynVirtualDataSet.InternalLast;
begin
  fCurrentRow := GetRecordCount;
end;

procedure TSynVirtualDataSet.InternalOpen;
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

procedure TSynVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fCurrentRow := PRecInfo(Buffer).RowIndentifier;
end;

function TSynVirtualDataSet.IsCursorOpen: boolean;
begin
  result := fIsCursorOpen;
end;

procedure TSynVirtualDataSet.SetBookmarkData(
  Buffer: TRecordBuffer; Data: pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PRecInfoIdentifier(Data)^;
end;

procedure TSynVirtualDataSet.SetBookmarkFlag(
  Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TSynVirtualDataSet.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  if Value <> RecNo then
  begin
    dec(Value);
    if cardinal(Value) >= cardinal(GetRecordCount) then
      raise ESynVirtualDataSet.CreateUtf8(
        '%.SetRecNo(%) with Count=%', [self, Value + 1, GetRecordCount]);
    DoBeforeScroll;
    fCurrentRow := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

function TSynVirtualDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
begin
  result := 0; // nothing found
end;

type
  {$ifdef ISDELPHIXE4}
  TFieldList = TList<TField>;
  {$else}
  TFieldList = TList;
  {$endif ISDELPHIXE4}

function TSynVirtualDataSet.Locate(const KeyFields: string; const KeyValues:
  Variant; Options: TLocateOptions): boolean;
var
  i, l, h, found: integer;
  FieldList: TFieldList;
begin
  CheckActive;
  result := true;
  if not IsEmpty then
    if VarIsArray(KeyValues) then
    begin
      FieldList := TFieldList.Create;
      try
        GetFieldList(FieldList, KeyFields);
        l := VarArrayLowBound(KeyValues, 1);
        h := VarArrayHighBound(KeyValues, 1);
        if (FieldList.Count = 1) and
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
          for i := 0 to FieldList.Count - 1 do
          begin
            found := SearchForField(
              StringToUtf8(TField(FieldList[i]).FieldName),
              KeyValues[l + i], Options);
            if found > 0 then
            begin
              RecNo := found;
              exit;
            end;
          end;
      finally
        FieldList.Free;
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

class procedure TSynVirtualDataSet.BcdWrite(const aWriter: TBaseWriter; const aValue);
begin
  AddBcd(aWriter, TFMTBcdData(TVarData(aValue).VPointer).fBcd);
end;


{ ************ Database-Aware BCD Values Support }

function InternalBcdToBuffer(const AValue: TBcd; out ADest: TBcdBuffer;
  var PBeg: PAnsiChar): integer;
var
  i, DecimalPos: integer;
  P, Frac: PByte;
  PEnd: PAnsiChar;
begin
  result := 0;
  if AValue.Precision = 0 then
    exit;
  DecimalPos := AValue.Precision - (AValue.SignSpecialPlaces and $3F);
  P := @ADest;
  Frac := @AValue.Fraction;
  // convert TBcd digits into text
  for i := 0 to AValue.Precision - 1 do
  begin
    if i = DecimalPos then
      if i = 0 then
      begin
        PWord(P)^ := ord('0') + ord('.') shl 8;
        inc(P, 2);
      end
      else
      begin
        P^ := ord('.');
        inc(P);
      end;
    if (i and 1) = 0 then
      P^ := ((Frac^ and $F0) shr 4) + ord('0')
    else
    begin
      P^ := ((Frac^ and $0F)) + ord('0');
      inc(Frac);
    end;
    inc(P);
  end;
  // remove trailing 0 after decimal
  if AValue.Precision > DecimalPos then
  begin
    repeat
      dec(P)
    until (P^ <> ord('0')) or
          (P = @ADest);
    PEnd := pointer(P);
    if PEnd^ <> '.' then
      inc(PEnd);
  end
  else
    PEnd := pointer(P);
  PEnd^ := #0;
  // remove leading 0
  PBeg := @ADest;
  while (PBeg[0] = '0') and
        (PBeg[1] in ['0'..'9']) do
    inc(PBeg);
  result := PEnd - PBeg;
end;

procedure AddBcd(WR: TBaseWriter; const AValue: TBcd);
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := InternalBcdToBuffer(AValue, tmp, PBeg);
  if len <= 0 then
    WR.Add('0')
  else
  begin
    if AValue.SignSpecialPlaces and $80 = $80 then
      WR.Add('-');
    WR.AddNoJsonEscape(PBeg, len);
  end;
end;

function BcdToCurr(const AValue: TBcd; var Curr: Currency): boolean;
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := InternalBcdToBuffer(AValue, tmp, PBeg);
  if len <= 0 then
    Curr := 0
  else
  begin
    PInt64(@Curr)^ := StrToCurr64(pointer(PBeg));
    if AValue.SignSpecialPlaces and $80 = $80 then
      Curr := -Curr;
  end;
  result := true;
end;

procedure BcdToUtf8(const AValue: TBcd; var result: RawUtf8);
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := InternalBcdToBuffer(AValue, tmp, PBeg);
  FastSetString(result, PBeg, len);
end;

function BcdToUtf8(const AValue: TBcd): RawUtf8;
begin
  BcdToUtf8(AValue, result);
end;

function BcdToString(const AValue: TBcd): string;
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := InternalBcdToBuffer(AValue, tmp, PBeg);
  Ansi7ToString(PWinAnsiChar(PBeg), len, result);
end;



{ ************ JSON and Variants TDataSet Support }

function DataSetToJson(Data: TDataSet): RawUtf8;
var
  W: TJsonWriter;
  f: PtrInt;
  blob: TRawByteStringStream;
begin
  result := 'null';
  if Data = nil then
    exit;
  Data.First;
  if Data.Eof then
    exit;
  W := TJsonWriter.Create(nil, true, false, nil, 16384);
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
            W.AddShort('null')
          else
            case DataType of
              ftBoolean:
                W.Add(AsBoolean);
              ftSmallint,
              ftInteger,
              ftWord,
              ftAutoInc:
                W.Add(AsInteger);
              ftLargeint:
                W.Add(TLargeintField(Data.Fields[f]).AsLargeInt);
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
    W.SetText(result);
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
begin
  fValues := Data;
  fValuesCount := DataCount;
  n := Length(ColumnNames);
  if n > 0 then
  begin
    // some columns name/type information has been supplied
    if n <> length(ColumnTypes) then
      raise ESynVirtualDataSet.CreateUtf8(
        '%.Create(ColumnNames<>ColumnTypes)', [self]);
    SetLength(fColumns, n);
    for ndx := 0 to n - 1 do
    begin
      fColumns[ndx].Name := ColumnNames[ndx];
      fColumns[ndx].FieldType := ColumnTypes[ndx];
    end;
  end
  else if fValues <> nil then
  begin
    // guess columns name/type from the first supplied TDocVariant
    first := _Safe(fValues[0], dvObject);
    SetLength(fColumns, first^.Count);
    for ndx := 0 to first^.Count - 1 do
    begin
      fColumns[ndx].Name := first^.Names[ndx];
      fColumns[ndx].FieldType := VariantTypeToSqlDBFieldType(first^.Values[ndx]);
      case fColumns[ndx].FieldType of
        mormot.db.core.ftNull:
          fColumns[ndx].FieldType := mormot.db.core.ftBlob;
        mormot.db.core.ftCurrency:
          fColumns[ndx].FieldType := mormot.db.core.ftDouble;
        mormot.db.core.ftInt64: // ensure type coherency of whole column
          for j := 1 to first^.Count - 1 do
            if j >= fValuesCount then
              break
            else
              // ensure objects are consistent
              with _Safe(fValues[j], dvObject)^ do
                if (ndx < Length(Names)) and
                   IdemPropNameU(Names[ndx], fColumns[ndx].Name) and
                   (VariantTypeToSqlDBFieldType(Values[ndx]) in
                     [mormot.db.core.ftNull,
                      mormot.db.core.ftDouble,
                      mormot.db.core.ftCurrency]) then
                  begin
                    fColumns[ndx].FieldType := mormot.db.core.ftDouble;
                    break;
                  end;
      end;
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
  F, ndx: PtrInt;
  wasString: boolean;
begin
  result := nil;
  F := Field.Index;
  if (cardinal(RowIndex) < cardinal(fValuesCount)) and
     (cardinal(F) < cardinal(length(fColumns))) and
     not (fColumns[F].FieldType in
           [mormot.db.core.ftNull,
            mormot.db.core.ftUnknown,
            mormot.db.core.ftCurrency]) then
    with _Safe(fValues[RowIndex])^ do
      if (Kind = dvObject) and
         (Count > 0) then
      begin
        if IdemPropNameU(fColumns[F].Name, Names[F]) then
          ndx := F // optimistic match
        else
          ndx := GetValueIndex(fColumns[F].Name);
        if ndx >= 0 then
          if VarIsEmptyOrNull(Values[ndx]) then
            exit
          else
          begin
            result := @fTemp64;
            if not OnlyCheckNull then
              case fColumns[F].FieldType of
                mormot.db.core.ftInt64:
                  VariantToInt64(Values[ndx], fTemp64);
                mormot.db.core.ftDouble,
                mormot.db.core.ftDate:
                  VariantToDouble(Values[ndx], unaligned(PDouble(@fTemp64)^));
                mormot.db.core.ftUtf8:
                  begin
                    VariantToUtf8(Values[ndx], fTempUtf8, wasString);
                    result := pointer(fTempUtf8);
                    ResultLen := length(fTempUtf8);
                  end;
                mormot.db.core.ftBlob:
                  begin
                    VariantToUtf8(Values[ndx], fTempUtf8, wasString);
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
end;

const
  TYPESTODB: array[TSqlDBFieldType] of TFieldType =(
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
  f, siz: PtrInt;
  fieldname: string;
begin
  FieldDefs.Clear;
  for f := 0 to high(fColumns) do
  begin
    if fColumns[f].FieldType = ftUtf8 then
      siz := 16
    else
      siz := 0;
    Utf8ToStringVar(fColumns[f].Name, fieldname);
    FieldDefs.Add(fieldname, TYPESTODB[fColumns[f].FieldType], siz);
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
  begin
    v := _Safe(fValues[result - 1]);
    if (v^.Kind = dvObject) and
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


initialization


end.

