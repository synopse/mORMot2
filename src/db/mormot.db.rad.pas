/// Database Framework of SQL DB Connnection using TDataSet
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad;

{
  *****************************************************************************

   Parent Classes for TDataSet / DB.pas Database Access
    - Shared Wrappers Around DB.pas Classes and Functions
    - Database-Aware BCD Values Support
    - mormot.db.sql Abstract Connection for DB.pas TDataSet

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
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  {$ifdef ISDELPHIXE2}
  System.Generics.Collections,
  Data.DB,
  Data.FMTBcd;
  {$else}
  DB,
  FMTBcd;
  {$endif ISDELPHIXE2}


{ ************ Shared Wrappers Around DB.pas Classes and Functions }


const
  /// map the UI/RTL string type, depending on the actual compiler version
  {$ifdef UNICODE}
  ftDefaultVCLString = ftWideString;
  {$else}
  ftDefaultVCLString = ftString;
  {$endif UNICODE}

  /// if you prefer LCL/Lazarus naming
  ftDefaultLCLString = ftDefaultVCLString;

  /// map the best ft*Memo type available, depending on the Delphi compiler version
  {$ifdef HASDBFTWIDE}
  ftDefaultMemo = ftWideMemo;
  {$else}
  ftDefaultMemo = ftMemo;
  {$endif HASDBFTWIDE}

type
  {$ifndef UNICODE} // defined as TRecordBuffer = PByte in newer DB.pas
  TRecordBuffer = PChar;
  {$endif UNICODE}

  {$ifndef ISDELPHIXE4}
  TValueBuffer = pointer;
  {$endif ISDELPHIXE4}
  
  PDateTimeRec = ^TDateTimeRec;

  {$ifdef ISDELPHIXE4}
  TDatasetGetFieldList = TList<TField>;
  {$else}
  TDatasetGetFieldList = TList;
  {$endif ISDELPHIXE4}


{************ Database-Aware BCD Values Support }

// note: this unit will also register TBcd to be serialized as JSON "string"

type
  /// a string buffer, used by BcdToBuffer to store its output text
  TBcdBuffer = array[0..71] of AnsiChar;

/// convert a TBcd value as text to the output buffer
// - returns the resulting text start in PBeg, and the length as function result
function BcdToBuffer(const AValue: TBcd; out ADest: TBcdBuffer;
  var PBeg: PAnsiChar): integer;

/// convert a TBcd value into a currency
// - purepascal version included in latest Delphi versions is slower than this
function BcdToCurr(const AValue: TBcd; var Curr: Currency): boolean;

/// convert a TBcd value into a RawUtf8 text
procedure BcdToUtf8(const AValue: TBcd; var result: RawUtf8); overload;

/// convert a TBcd value into a RawUtf8 text
function BcdToUtf8(const AValue: TBcd): RawUtf8; overload;
  {$ifdef HASINLINE} inline;{$endif}

/// convert a TBcd value into a RTL string text
// - RTL BCDToStr() is slower, and not consistent between Delphi and FPC
function BcdToString(const AValue: TBcd): string;

/// append a TBcd value as text to the output buffer
// - emit a JSON-compatible floating point number text, with DecimalSeparator='.'
procedure AddBcd(WR: TTextWriter; const AValue: TBcd);

/// convert a text buffer into its matching TBcd value
// - supports BcdToBuffer() layout, but not the '123e-10' scientific notation
// - recognize a JSON-compatible floting point number, with DecimalSeparator='.'
function TryBufferToBcd(P: PUtf8Char; Len: PtrInt; out Bcd: TBcd): boolean;

/// convert a text RawUtf8 into its matching TBcd value
function TryUtf8ToBcd(const Text: RawUtf8; out Bcd: TBcd): boolean;

/// convert a text string into its matching TBcd value
// - RTL TryStrToBCD() is slower, and not consistent between Delphi and FPC
function TryStringToBcd(const Text: string; out Bcd: TBcd): boolean;


{ ************ mormot.db.sql Abstract Connection for DB.pas TDataSet }

type
  /// Exception type associated to generic TDataSet / DB.pas unit Dataset connection
  ESqlDBDataset = class(ESqlDBException);

  ///	implement properties shared by via the DB.pas TQuery-like connections
  TSqlDBDatasetConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    {$ifndef UNICODE}
    fForceInt64AsFloat: boolean;
    {$endif UNICODE}
    fForceUseWideString: boolean;
  public
    /// initialize the properties to connect via TDataSet database access
    // - this overridden method will enable the BATCH process (emulated in
    // TSqlDBDatasetStatement.ExecutePrepared, native e.g. for FireDAC)
    constructor Create(const aServerName, aDatabaseName,
      aUserID, aPassWord: RawUtf8); override;
    {$ifndef UNICODE}
    /// set to true to force all Int64 content to be processed as a truncated float
    // - by default, Int64 values will be bound either as an integer (if the
    // value is within expected range), either as Int64 variant
    // - on some versions of Delphi, and some version of TDataSet (e.g. BDE),
    // you may have to use a conversion to double to avoid a runtime error
    property ForceInt64AsFloat: boolean
      read fForceInt64AsFloat write fForceInt64AsFloat;
    {$endif UNICODE}
    /// set to true to force all text content to be processed as WideString
    // instead of the default faster AnsiString, for pre-Unicode version of Delphi
    // - by default, UTF-8 text parameter or column will use an AnsiString value:
    // for pre-Unicode Delphi, avoiding WideString/OleStr content
    // will speed up the process a lot, if you are sure that the current
    // charset matches the expected one (which is very likely)
    // - set this property to true so that WideString will be used when working
    // with the internal TDataSet, to avoid any character data loss:
    // the access to the property will be slower, but you won't have any
    // potential data loss
    // - if the text value contains only ASCII 7-bit characters, it won't be
    // converted to WideString (since it is not necessary)
    // - starting with Delphi 2009, the TEXT content will be processed as an
    // UnicodeString, so this property is not necessary for most cases,
    // but it appeared that some providers expects it to be defined
    property ForceUseWideString: boolean
      read fForceUseWideString write fForceUseWideString;
  end;

  ///	implements an abstract statement via the DB.pas TDataSet/TQuery-like
  // connection
  // - dedicated abstract class, able to use any TDataSet with any kind of
  // parameter linking (e.g. FireDAC/AnyDAC do have its own parameters type)
  TSqlDBDatasetStatementAbstract = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fQuery: TDataSet;
    fPrepared: boolean;
    fDatasetSupportBatchBinding: boolean;
    fForceUseWideString: boolean;
    fPreparedParamsCount: integer;
  protected
    /// convert SqlDBParamType to a standard DB.TParamType to be used in TQuery.Param
    function SqlParamTypeToDBParamType(IO: TSqlDBParamInOutType): TParamType; virtual;
    /// convert DB.TFieldType into mORMot fieldtype
    function ColumnTypeNativeToDB(aNativeType: TFieldType): TSqlDBFieldType; virtual;
    /// retrieve a given column
    function DatasetField(col: integer): TField; virtual;
  protected // inherited classes shall override those abstract virtual methods
    /// should initialize and set fQuery internal field as expected
    procedure DatasetCreate; virtual; abstract;
    /// should set the internal fQueryParams protected field
    function DatasetPrepare(const aSQL: string): boolean; virtual; abstract;
    /// execute underlying TQuery.ExecSQL
    procedure DatasetExecSQL; virtual; abstract;
    /// bind SqlDBParam to TQuery-like param
    // - aArrayIndex is >= 0 if array index should be used (in this case,
    // fDatasetSupportBatchBinding=false)
    // - if fDatasetSupportBatchBinding=true, should use array DML binding
    // - SQL Parameter to bind is aParam
    procedure DataSetBindSqlParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSqlDBParam); virtual; abstract;
    /// set the returned parameter after a stored proc execution
    procedure DataSetOutSqlParam(const aParamIndex: integer;
      var aParam: TSqlDBParam); virtual; abstract;
  public
    /// create a statement instance
    constructor Create(aConnection: TSqlDBConnection); override;
    /// release the prepared statement
    destructor Destroy; override;

    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is true, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBDataset on any error
    procedure Prepare(const aSQL: RawUtf8;
      ExpectResults: boolean = false); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also loop through all internal bound array
    // of values (if any), to implement BATCH mode even if the database library
    // does not support array binding (only mormot.db.rad.firedac does support it yet)
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESqlDBDataset on any error
    procedure ExecutePrepared; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESqlDBDataset on any error
    procedure Reset; override;

    /// access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is true, will put the cursor on the first row of results
    // - raise an ESqlDBDataset on any error
    function Step(SeekFirst: boolean = false): boolean; override;
    /// close the associated TQuery when ISqlDBStatement is back in cache
    procedure ReleaseRows; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): Int64; override;
    /// returns true if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// return one column value into JSON content
    procedure ColumnToJson(Col: integer; W: TJsonWriter); override;
  end;

  /// implements a statement via the DB.pas TDataSet/TQuery-like connection
  // - you should not use this abstract class directly, but one inherited
  // implementation with overridden Dataset*() protected methods to handle the
  // internal fQuery: TDataSet property
  TSqlDBDatasetStatement = class(TSqlDBDatasetStatementAbstract)
  protected
    fQueryParams: TParams;
    /// bind SqlDBParam to TQuery-like param using fQueryParams: DB.TParams
    procedure DataSetBindSqlParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSqlDBParam); override;
    /// set the returned parameter after a stored proc execution
    procedure DataSetOutSqlParam(const aParamIndex: integer;
      var aParam: TSqlDBParam); override;
  public
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is true, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBDataset on any error
    procedure Prepare(const aSQL: RawUtf8;
      ExpectResults: boolean = false); overload; override;
  end;

  
implementation


{ ************ Database-Aware BCD Values Support }

function BcdToBuffer(const AValue: TBcd; out ADest: TBcdBuffer;
  var PBeg: PAnsiChar): integer;
var
  i, decpos: integer;
  P, frac: PByte;
  PEnd: PAnsiChar;
begin
  result := 0;
  if AValue.Precision = 0 then
    exit;
  decpos := AValue.Precision - (AValue.SignSpecialPlaces and $3F);
  P := @ADest[1];
  frac := @AValue.Fraction;
  // convert TBcd digits into text
  for i := 0 to AValue.Precision - 1 do
  begin
    if i = decpos then
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
      P^ := ((frac^ and $F0) shr 4) + ord('0')
    else
    begin
      P^ := ((frac^ and $0F)) + ord('0');
      inc(frac);
    end;
    inc(P);
  end;
  // remove trailing 0 after decimal
  if AValue.Precision > decpos then
  begin
    repeat
      dec(P)
    until (P^ <> ord('0')) or
          (P = @ADest[1]);
    PEnd := pointer(P);
    if PEnd^ <> '.' then
      inc(PEnd);
  end
  else
    PEnd := pointer(P);
  PEnd^ := #0; // make dest buffer #0 terminated
  // remove leading 0
  PBeg := @ADest[1];
  while (PBeg[0] = '0') and
        (PBeg[1] in ['0'..'9']) do
    inc(PBeg);
  // handle specific cases: 0 or <0
  if PEnd = PBeg then
  begin
    PBeg^ := '0';
    inc(PBeg);
  end
  else if AValue.SignSpecialPlaces and $80 = $80 then
  begin
    dec(PBeg);
    PBeg^ := '-';
  end;
  PEnd^ := #0; // make ASCIIZ
  result := PEnd - PBeg;
end;

procedure AddBcd(WR: TTextWriter; const AValue: TBcd);
var
  len: PtrInt;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := BcdToBuffer(AValue, tmp, PBeg);
  WR.AddNoJsonEscape(PBeg, len);
end;

function BcdToCurr(const AValue: TBcd; var Curr: Currency): boolean;
var
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  BcdToBuffer(AValue, tmp, PBeg);
  PInt64(@Curr)^ := StrToCurr64(pointer(PBeg));
  result := true;
end;

procedure BcdToUtf8(const AValue: TBcd; var result: RawUtf8);
var
  len: PtrInt;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := BcdToBuffer(AValue, tmp, PBeg);
  FastSetString(result, PBeg, len);
end;

function BcdToUtf8(const AValue: TBcd): RawUtf8;
begin
  BcdToUtf8(AValue, result);
end;

function BcdToString(const AValue: TBcd): string;
var
  len: PtrInt;
  PBeg: PAnsiChar;
  tmp: TBcdBuffer;
begin
  len := BcdToBuffer(AValue, tmp, PBeg);
  Ansi7ToString(PWinAnsiChar(PBeg), len, result);
end;

function TryBufferToBcd(P: PUtf8Char; Len: PtrInt; out Bcd: TBcd): boolean;
var
  PEnd: PUtf8Char;
  neg: boolean;
  posDec, pos: integer;
  b: PByte;
  c: cardinal;
begin
  result := false;
  FillCharFast(Bcd, SizeOf(Bcd), 0);
  if (P = nil) or
     (Len <= 0) then
    exit;
  PEnd := P + Len;
  while (P < PEnd) and
        (P^ in [#1 .. ' ']) do
    inc(P);
  neg := P^ = '-';
  if neg or
     (P^ = '+') then
    inc(P);
  pos := 0;
  posDec := -1;
  while (PEnd > P) and
        (PEnd[-1] in [#1 .. ' '])  do
    dec(PEnd);
  if P = PEnd then
    exit;
  while P < PEnd do
  begin
    c := PByte(P)^ - Ord('0');
    if c > 9 then
      if P^ = '.' then
      begin
        if posDec >= 0 then
          exit
        else if pos = 0 then
          inc(pos)
        else if (pos = 1) and
                (P[-1] = '0') then
          dec(pos);
        posDec := pos;
        while (PEnd > P) and
              (PEnd[-1] = '0') do
          dec(PEnd);
        inc(P);
        continue;
      end
      else
        exit
    else if pos < SizeOf(Bcd.Fraction) * 2 then
    begin
      b := @Bcd.Fraction[pos shr 1];
      if pos and 1 = 0 then
        b^ := c shl 4
      else
        b^ := b^ or c;
      inc(pos);
    end
    else if posDec < 0 then
      exit;
    inc(P);
  end;
  if pos = 0 then
  begin
    Bcd.Precision := 10;
    Bcd.SignSpecialPlaces := 2;
  end
  else if pos > MaxFMTBcdFractionSize then
    exit
  else
  begin
    Bcd.Precision := pos;
    if posDec >= 0 then
      Bcd.SignSpecialPlaces := pos - posDec;
    if neg then
      Bcd.SignSpecialPlaces := Bcd.SignSpecialPlaces or $80;
  end;
  result := true;
end;

function TryUtf8ToBcd(const Text: RawUtf8; out Bcd: TBcd): boolean;
begin
  result := TryBufferToBcd(pointer(Text), length(Text), Bcd);
end;

{$ifdef UNICODE}
function TryStringToBcd(const Text: string; out Bcd: TBcd): boolean;
var
  tmp: TByteToByte; // no memory allocation needed
  i, L: PtrInt;
begin
  result := false;
  L := length(Text);
  if (L = 0) or
     (L >= high(tmp)) then
    exit;
  for i := 0 to L do // include trailing #0
    tmp[i] := PWordArray(Text)[i];
  result := TryBufferToBcd(PUtf8Char(@tmp), L, Bcd);
end;
{$else}
function TryStringToBcd(const Text: string; out Bcd: TBcd): boolean;
begin
  result := TryUtf8ToBcd(Text, Bcd);
end;
{$endif UNICODE}

// store TBcd as JSON "string" to keep the precision

procedure _JS_BCD(Data: PBcd; const Ctxt: TJsonSaveContext);
begin
  Ctxt.W.Add('"');
  AddBcd(Ctxt.W, Data^);
  Ctxt.W.AddDirect('"');
end;

procedure _JL_BCD(Data: PBcd; var Ctxt: TJsonParserContext);
var
  V: array[0..2] of TValuePUtf8Char;
begin
  if Ctxt.ParseNextAny then
    if Ctxt.WasString then
      // mORMot 2 new "1.0594631" format
      Ctxt.Valid := TryBufferToBcd(Ctxt.Value, Ctxt.ValueLen, Data^)
    else
    begin
      // mORMot 1 serialization with Delphi extended RTTI
      JsonDecode(Ctxt.Value,
        ['Precision', 'SignSpecialPlaces', 'Fraction'], @V);
      Ctxt.Valid := (V[0].Len <> 0) and
                    (V[1].Len <> 0) and
                    (V[2].Len = SizeOf(Data^.Fraction) * 2) and
                    mormot.core.text.HexToBin(PAnsiChar(V[2].Text),
                      @Data^.Fraction, SizeOf(Data^.Fraction));
      if not Ctxt.Valid then
        exit;
      Data^.Precision := V[0].ToCardinal;
      Data^.SignSpecialPlaces := V[1].ToCardinal;
    end;
end;



{ ************ mormot.db.sql Abstract Connection for DB.pas TDataSet }

const
  IsTLargeIntField   = 1;
  IsTWideStringField = 2;


{ TSqlDBDatasetConnectionProperties }

constructor TSqlDBDatasetConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  fBatchSendingAbilities := [cCreate, cUpdate, cDelete]; // always emulated
end;


{ TSqlDBDatasetStatementAbstract }

function TSqlDBDatasetStatementAbstract.ColumnBlob(Col: integer): RawByteString;
var
  strm: TStream;
  f: TField;
begin
  result := '';
  CheckCol(Col);
  f := TField(fColumns[Col].ColumnAttr);
  if f.IsNull then
    exit;
  if f.IsBlob then
  begin
    strm := f.DataSet.CreateBlobStream(f, bmRead);
    try
      result := StreamToRawByteString(strm);
    finally
      strm.Free;
    end;
  end
  else
  begin
    SetLength(result, f.DataSize);
    f.GetData(TValueBuffer(result), {nativeformat=}true);
  end;
end;

function TSqlDBDatasetStatementAbstract.ColumnCurrency(Col: integer): currency;
var
  f: TField;
begin
  CheckCol(Col);
  f := TField(fColumns[Col].ColumnAttr);
  if f.IsNull then
    result := 0
  else if f.DataType in [ftBCD, ftFMTBcd] then
    BcdToCurr(f.AsBCD, result) // direct conversion with exact decimals
  else
    result := f.AsCurrency;
end;

function TSqlDBDatasetStatementAbstract.ColumnDateTime(Col: integer): TDateTime;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0
    else
      result := TField(ColumnAttr).AsDateTime;
end;

function TSqlDBDatasetStatementAbstract.ColumnDouble(Col: integer): double;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0
    else
      result := TField(ColumnAttr).AsFloat;
end;

function TSqlDBDatasetStatementAbstract.ColumnInt(Col: integer): Int64;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0
    else if TField(ColumnAttr).DataType = ftBoolean then
      result := ord(TField(ColumnAttr).AsBoolean)
    else
      {$ifdef UNICODE}
      result := TField(ColumnAttr).AsLargeInt;
      {$else}
      if ColumnValueDBType = IsTLargeIntField then
        result := TLargeintField(ColumnAttr).AsLargeInt
      else
        result := TField(ColumnAttr).AsInteger;
      {$endif UNICODE}
end;

function TSqlDBDatasetStatementAbstract.ColumnNull(Col: integer): boolean;
begin
  CheckCol(Col);
  result := TField(fColumns[Col].ColumnAttr).IsNull;
end;

function TSqlDBDatasetStatementAbstract.ColumnUtf8(Col: integer): RawUtf8;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := ''
    else
      {$ifndef UNICODE}
      if ColumnValueDBType = IsTWideStringField then
      {$ifdef FPC}
        result := UnicodeStringToUtf8(TWideStringField(ColumnAttr).AsUnicodeString)
      {$else}
        result := WideStringToUtf8(TWideStringField(ColumnAttr).Value)
      {$endif FPC}
      else
      {$endif UNICODE}
        result := StringToUtf8(TField(ColumnAttr).AsString);
end;

constructor TSqlDBDatasetStatementAbstract.Create(aConnection: TSqlDBConnection);
begin
  fForceUseWideString := (aConnection.Properties
    as TSqlDBDatasetConnectionProperties).ForceUseWideString;
  inherited Create(aConnection);
  try
    DatasetCreate;
  except
    FreeAndNil(fQuery);
    raise;
  end;
end;

destructor TSqlDBDatasetStatementAbstract.Destroy;
begin
  FreeAndNil(fQuery);
  inherited;
end;

procedure TSqlDBDatasetStatementAbstract.Prepare(const aSQL: RawUtf8;
  ExpectResults: boolean);
var
  sqlu: RawUtf8;
begin
  SQLLogBegin(sllDB);
  if fPrepared then
    ESqlDBDataset.RaiseUtf8('%.Prepare(%) shall be called once', [self, aSQL]);
  inherited Prepare(aSQL, ExpectResults); // connect if necessary
  fPreparedParamsCount := ReplaceParamsByNames(aSQL, sqlu);
  fPrepared := DatasetPrepare(Utf8ToString(sqlu));
  SQLLogEnd;
  if not fPrepared then
    ESqlDBDataset.RaiseUtf8('%.Prepare: DatasetPrepare(%) failed', [self, sqlu]);
end;

procedure TSqlDBDatasetStatementAbstract.ExecutePrepared;
var
  i, arrndx: integer;
  p: PtrInt;
  field: TField;
begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // 1. bind parameters in fParams[] to fQuery.Params
  if fPreparedParamsCount <> fParamCount then
    ESqlDBDataset.RaiseUtf8('%.ExecutePrepared expected % bound parameters, got %',
      [self, fPreparedParamsCount, fParamCount]);
  arrndx := -1; // either Bind() or BindArray() with no Array DML support
  repeat
    if (not fDatasetSupportBatchBinding) and
       (fParamsArrayCount > 0) then
      inc(arrndx); // enable BindArray() emulation
    for p := 0 to fParamCount - 1 do
      DatasetBindSqlParam(arrndx, p, fParams[p]);
    // 2. Execute query (within a loop for BATCH mode)
    if fExpectResults then
    begin
      fQuery.Open;
      fCurrentRow := -1;
      ClearColumns;
      fColumn.Capacity := fQuery.FieldCount;
      for i := 0 to fQuery.FieldCount - 1 do
      begin
        field := DatasetField(i);
        with AddColumn(StringToUtf8(field.FieldName))^ do
        begin
          ColumnAttr := PtrUInt(field);
          ColumnType := ColumnTypeNativeToDB(field.DataType);
          if field.InheritsFrom(TLargeintField) then
            ColumnValueDBType := IsTLargeIntField
          else if field.InheritsFrom(TWideStringField) then
            ColumnValueDBType := IsTWideStringField
          else
            ColumnValueDBType := 0;
        end;
      end;
    end
    else
      DatasetExecSQL;
  until fDatasetSupportBatchBinding or
        (arrndx = fParamsArrayCount - 1);
  // 3. handle out parameters
  if fParamCount > 0 then
    if fParamsArrayCount > 0 then
      for p := 0 to fParamCount - 1 do
        fParams[p].VData := ''
    else
      // single statement mode -> return any stored procedure parameter
      for p := 0 to fParamCount - 1 do
        if fParams[p].VInOut <> paramIn then
          DataSetOutSqlParam(p, fParams[p]);
  SQLLogEnd;
end;

function TSqlDBDatasetStatementAbstract.Step(SeekFirst: boolean): boolean;
begin
  if SeekFirst then
  begin
    fQuery.First;
    fCurrentRow := 1;
  end
  else if fCurrentRow > 0 then
  begin
    fQuery.Next;
    inc(fCurrentRow);
  end
  else
    fCurrentRow := 1;
  result := not fQuery.Eof;
end;

procedure TSqlDBDatasetStatementAbstract.Reset;
begin
  ReleaseRows;
  inherited Reset;
end;

procedure TSqlDBDatasetStatementAbstract.ReleaseRows;
begin
  if (fQuery <> nil) and
     fQuery.Active then
    fQuery.Close;
  inherited ReleaseRows;
end;

function TSqlDBDatasetStatementAbstract.SqlParamTypeToDBParamType(
  IO: TSqlDBParamInOutType): TParamType;
begin
  case IO of
    paramIn:
      result := ptInput;
    paramOut:
      result := ptOutput;
    paramInOut:
      result := ptInputOutput;
  else
    result := ptUnknown;
  end;
end;

function TSqlDBDatasetStatementAbstract.ColumnTypeNativeToDB(
  aNativeType: TFieldType): TSqlDBFieldType;
begin
  case aNativeType of
  {$ifdef HASDBFNEW}
    ftLongWord,
    ftShortint,
    ftByte,
  {$endif HASDBFNEW}
    ftAutoInc,
    ftBoolean,
    ftSmallint,
    ftInteger,
    ftLargeint,
    ftWord:
      result := mormot.db.core.ftInt64;
  {$ifdef HASDBFNEW}
    ftExtended,
  {$endif HASDBFNEW}
  {$ifdef HASDBFSINGLE}
    ftSingle,
  {$endif HASDBFSINGLE}
    ftFloat:
      result := mormot.db.core.ftDouble;
    ftCurrency,
    ftBCD,
    ftFMTBcd:
      result := mormot.db.core.ftCurrency;
  {$ifdef HASDBFNEW}
    ftOraTimeStamp,
    ftOraInterval,
  {$endif HASDBFNEW}
    ftDate,
    ftTime,
    ftDateTime,
    ftTimeStamp:
      result := mormot.db.core.ftDate;
    ftBytes,
    ftVarBytes,
    ftBlob,
    ftGraphic,
    ftOraBlob:
      result := mormot.db.core.ftBlob;
  {$ifdef HASDBFTWIDE}
    ftFixedWideChar,
    ftWideMemo,
  {$endif HASDBFTWIDE}
    ftString,
    ftFixedChar,
    ftWideString,
    ftMemo,
    ftFmtMemo,
    ftOraClob,
    ftVariant,
    ftGuid:
      result := mormot.db.core.ftUtf8;
  else
    // will use TEXT for other fields (any feedback is welcome!)
    result := mormot.db.core.ftUtf8;
  end;
end;

function TSqlDBDatasetStatementAbstract.DatasetField(col: integer): TField;
begin
  result := fQuery.Fields[col];
end;

procedure TSqlDBDatasetStatementAbstract.ColumnToJson(Col: integer;
  W: TJsonWriter);
var
  f: TField;
  blob: RawByteString;
begin
  with fColumns[Col] do
  begin
    f := TField(ColumnAttr);
    if f.IsNull then
      W.AddNull
    else
      case ColumnType of
        mormot.db.core.ftNull:
          W.AddNull;
        mormot.db.core.ftInt64:
          if f.DataType = ftBoolean then
            W.AddU(f.AsBoolean) // normalize as 0 or 1
          else
          {$ifdef UNICODE}
            W.Add(f.AsLargeInt);
          {$else}
            if ColumnValueDBType = IsTLargeIntField then
              W.Add(TLargeIntField(f).AsLargeInt)
            else
              W.Add(f.AsInteger);
          {$endif UNICODE}
        mormot.db.core.ftDouble:
          W.AddDouble(f.AsFloat);
        mormot.db.core.ftCurrency:
          if f.DataType in [ftBCD, ftFMTBcd] then
            AddBcd(W, f.AsBCD)
          else
            W.AddCurr(f.AsCurrency);
        mormot.db.core.ftDate:
          begin
            W.Add('"');
            W.AddDateTime(f.AsDateTime, GetForceDateWithMS);
            W.AddDirect('"');
          end;
        mormot.db.core.ftUtf8:
          begin
            W.Add('"');
          {$ifndef UNICODE}
            if ColumnValueDBType = IsTWideStringField then
              {$ifdef FPC}
              W.AddJsonEscapeW(pointer(TWideStringField(ColumnAttr).AsUnicodeString))
              {$else}
              W.AddJsonEscapeW(pointer(TWideStringField(ColumnAttr).Value))
             {$endif FPC}
          else
          {$endif UNICODE}
              W.AddJsonEscapeString(f.AsString);
            W.AddDirect('"');
          end;
        mormot.db.core.ftBlob:
          if dsfForceBlobAsNull in fFlags then
            W.AddNull
          else
          begin
            blob := ColumnBlob(Col);
            W.WrBase64(pointer(blob), length(blob), true); // withMagic=true
          end;
      else
        ESqlDBException.RaiseUtf8('%: Invalid ColumnType()=%',
          [self, ord(ColumnType)]);
      end;
  end;
end;


{ TSqlDBDatasetStatement }

procedure TSqlDBDatasetStatement.DataSetBindSqlParam(
  const aArrayIndex, aParamIndex: integer; const aParam: TSqlDBParam);
var
  P: TParam;
  I64: Int64;
  tmp: RawUtf8;
begin
  P := fQueryParams[aParamIndex];
  with aParam do
  begin
    P.ParamType := SqlParamTypeToDBParamType(VInOut);
    if VinOut <> paramInOut then
      case VType of
        mormot.db.core.ftNull:
          begin
            P.Clear;
            {$ifdef UNICODE}
            P.AsBlob := nil; // avoid type errors when a blob field is adressed
            {$else}
            P.AsString := '';
            {$endif UNICODE}
          end;
        mormot.db.core.ftInt64:
          begin
            if aArrayIndex >= 0 then
              I64 := GetInt64(pointer(VArray[aArrayIndex]))
            else
              I64 := VInt64;
            {$ifdef UNICODE}
            P.AsLargeInt := I64;
            {$else}
            if (PInt64Rec(@I64)^.Hi = 0) or
               (PInt64Rec(@I64)^.Hi = cardinal(-1)) then
              P.AsInteger := I64
            else if TSqlDBDatasetConnectionProperties(Connection.Properties).
                      ForceInt64AsFloat then
              P.AsFloat := I64
            else
              P.Value := I64;
            {$endif UNICODE}
          end;
        mormot.db.core.ftDouble:
          if aArrayIndex >= 0 then
            P.AsFloat := GetExtended(pointer(VArray[aArrayIndex]))
          else
            P.AsFloat := unaligned(PDouble(@VInt64)^);
        mormot.db.core.ftCurrency:
          if aArrayIndex >= 0 then
            P.AsCurrency := StrToCurrency(pointer(VArray[aArrayIndex]))
          else
            P.AsCurrency := PCurrency(@VInt64)^;
        mormot.db.core.ftDate:
          if aArrayIndex >= 0 then
          begin
            UnQuoteSqlStringVar(pointer(VArray[aArrayIndex]), tmp);
            P.AsDateTime := Iso8601ToDateTime(tmp);
          end
          else
            P.AsDateTime := unaligned(PDateTime(@VInt64)^);
        mormot.db.core.ftUtf8:
          if aArrayIndex >= 0 then
            if (VArray[aArrayIndex] = '') and
               fConnection.Properties.StoreVoidStringAsNull then
              P.Clear
            else
            begin
              UnQuoteSqlStringVar(pointer(VArray[aArrayIndex]), tmp);
              if fForceUseWideString then
                P.Value := Utf8ToWideString(tmp)
              else
                P.AsString := Utf8ToString(tmp);
            end
          else if (VData = '') and
                  fConnection.Properties.StoreVoidStringAsNull then
            P.Clear
          else if fForceUseWideString then
            P.Value := Utf8ToWideString(VData)
          else
            P.AsString := Utf8ToString(VData);
        mormot.db.core.ftBlob:
          {$ifdef UNICODE}
          if aArrayIndex >= 0 then
            P.SetBlobData(TValueBuffer(VArray[aArrayIndex]), Length(VArray[aArrayIndex]))
          else
            P.SetBlobData(TValueBuffer(VData), Length(VData));
          {$else}
          if aArrayIndex >= 0 then
            P.AsString := VArray[aArrayIndex]
          else
            P.AsString := VData;
          {$endif UNICODE}
      else
        ESqlDBDataset.RaiseUtf8(
          '%.DataSetBindSqlParam: Invalid type % on bound parameter #%',
          [self, ord(VType), aParamIndex + 1]);
      end;
  end;
end;

procedure TSqlDBDatasetStatement.DataSetOutSqlParam(const aParamIndex: integer;
  var aParam: TSqlDBParam);
var
  par: TParam;
  {$ifdef UNICODE}
  tmp: TBytes;
  {$endif UNICODE}
begin
  par := fQueryParams[aParamIndex];
  case aParam.VType of
    mormot.db.core.ftInt64:
      {$ifdef UNICODE}
      aParam.VInt64 := par.AsLargeInt;
      {$else}
      aParam.VInt64 := trunc(par.AsFloat);
      {$endif UNICODE}
    mormot.db.core.ftDouble:
      unaligned(PDouble(@aParam.VInt64)^) := par.AsFloat;
    mormot.db.core.ftCurrency:
      PCurrency(@aParam.VInt64)^ := par.AsCurrency;
    mormot.db.core.ftDate:
      unaligned(PDateTime(@aParam.VInt64)^) := par.AsDateTime;
    mormot.db.core.ftUtf8:
      aParam.VData := StringToUtf8(par.AsString);
    mormot.db.core.ftBlob:
      begin
        {$ifdef UNICODE}
        tmp := par.AsBlob;
        FastSetRawByteString(aParam.VData, pointer(tmp), Length(tmp));
        {$else}
        aParam.VData := par.AsString;
        {$endif UNICODE}
      end;
  end;
end;

procedure TSqlDBDatasetStatement.Prepare(
  const aSQL: RawUtf8; ExpectResults: boolean);
begin
  inherited;
  if fPreparedParamsCount <> fQueryParams.Count then
    ESqlDBDataset.RaiseUtf8(
      '%.Prepare expected % parameters in request, found % - [%]',
      [self, fPreparedParamsCount, fQueryParams.Count, aSQL]);
end;


{$ifdef HASNOSTATICRTTI} // for Delphi 7/2007: mimics TypeInfo()
const
  _TBCD: TFakeTypeInfo = (
    Kind: rkRecord;
    Name4: 'TBCD';
    RecSize4: SizeOf(TBcd);
    ManagedCount4: 0);
{$endif HASNOSTATICRTTI}

initialization
  TRttiJson.RegisterCustomSerializerFunction(
    {$ifdef HASNOSTATICRTTI}
    @_TBCD, @_JL_BCD, @_JS_BCD);
    {$else}
    TypeInfo(TBcd), @_JL_BCD, @_JS_BCD);
    {$endif HASNOSTATICRTTI}

end.

