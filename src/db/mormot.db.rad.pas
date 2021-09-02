/// Database Framework of SQL DB Connnection using TDataSet
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad;

{
  *****************************************************************************

   Parent Classes for TDataSet / DB.pas Database Access
    - Shared Wrappers Around DB.pas Classes and Functions
    - TSynVirtualDataSet and TDocVariantArrayDataSet Classes
    - mormot.db.sql Abstract Connection for DB.pas TDataSet

  *****************************************************************************
}

interface

{$ifdef FPC} // currently only tested and supported with Delphi DB.pas unit

implementation // to compile a void unit on FPC

{$else}

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
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

type
  {$ifndef UNICODE} // defined as TRecordBuffer = PByte in newer DB.pas
  TRecordBuffer = PChar;
  {$endif UNICODE}
  
  PDateTimeRec = ^TDateTimeRec;

  {$ifndef ISDELPHIXE4}
  TValueBuffer = Pointer;
  {$endif ISDELPHIXE4}

const
  /// map the VCL string type, depending on the Delphi compiler version
  {$ifdef UNICODE}
  ftDefaultVCLString = ftWideString;
  {$else}
  ftDefaultVCLString = ftString;
  {$endif UNICODE}

  /// map the best ft*Memo type available, depending on the Delphi compiler version
  {$ifdef ISDELPHI2007ANDUP}
  ftDefaultMemo = ftWideMemo;
  {$else}
  ftDefaultMemo = ftMemo;
  {$endif ISDELPHI2007ANDUP}


/// append a TBcd value as text to the output buffer
// - very optimized for speed
procedure AddBcd(WR: TBaseWriter; const AValue: TBcd);

type
  /// a string buffer, used by InternalBCDToBuffer to store its output text
  TBCDBuffer = array[0..66] of AnsiChar;

/// convert a TBcd value as text to the output buffer
// - buffer is to be array[0..66] of AnsiChar
// - returns the resulting text start in PBeg, and the length as function result
// - does not handle negative sign and 0 value - see AddBcd() function use case
// - very optimized for speed
function InternalBCDToBuffer(const AValue: TBcd; out ADest: TBCDBuffer; var PBeg: PAnsiChar): integer;

/// convert a TBcd value into a currency
// - purepascal version included in latest Delphi versions is slower than this
function BCDToCurr(const AValue: TBcd;
  var Curr: Currency): boolean;

/// convert a TBcd value into a RawUtf8 text
// - will call fast InternalBCDToBuffer function
procedure BCDToUtf8(const AValue: TBcd; var result: RawUtf8); overload;

/// convert a TBcd value into a RawUtf8 text
// - will call fast InternalBCDToBuffer function
function BCDToUtf8(const AValue: TBcd): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TBcd value into a VCL string text
// - will call fast InternalBCDToBuffer function
function BCDToString(const AValue: TBcd): string;

  

{ ************ TSynVirtualDataSet and TDocVariantArrayDataSet Classes }

type
  /// read-only virtual TDataSet able to access any content
  TSynVirtualDataSet = class(TDataSet)
  protected
    fCurrentRow: integer;
    fIsCursorOpen: boolean;

    // TDataSet overridden methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    function GetCanModify: boolean; override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetRecNo(Value: integer); override;
    function GetRecNo: integer; override;

    // classses should override all those following methods:
    // - to read the data e.g. into memory:
    procedure InternalOpen; override;
    // - to initialize FieldDefs:
    // procedure InternalInitFieldDefs; override;
    // - to return row count:
    // function GetRecordCount: integer; override;
    // - result should point to Int64,Double,Blob,UTF-8 data (if ResultLen<>nil)
    function GetRowFieldData(Field: TField; RowIndex: integer; out ResultLen: integer;
      OnlyCheckNull: boolean): Pointer; virtual; abstract;
    // - to search for a field, returning RecNo (0 = not found by default)
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; virtual;
    // used to serialize TBCDVariant as JSON - BcdRead will always fail
    class procedure BcdWrite(const aWriter: TTextWriter; const aValue);
    //class function BcdRead(P: PUtf8Char; var aValue; out aValid: boolean): PUtf8Char;
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
      Options: TLocateOptions) : boolean; override;
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
    fTemp64: Int64;
    fTempUtf8: RawUtf8;
    fTempBlob: RawByteString;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: integer; OnlyCheckNull: boolean): Pointer; override;
    function SearchForField(const aLookupFieldName: RawUtf8;
      const aLookupValue: variant; aOptions: TLocateOptions): integer; override;
  public
    /// initialize the virtual TDataSet from a dynamic array of TDocVariant
    // - you can set the expected column names and types matching the results
    // document layout - if no column information is specified, the first
    // TDocVariant will be used as reference
    constructor Create(Owner: TComponent;
      const Data: TVariantDynArray;
      const ColumnNames: array of RawUtf8;
      const ColumnTypes: array of TSqlDBFieldType); reintroduce;
  end;

/// export all rows of a TDataSet into JSON
// - will work for any kind of TDataSet
function DataSetToJson(Data: TDataSet): RawUtf8;

/// convert a dynamic array of TDocVariant result into a VCL DataSet
// - this function is just a wrapper around TDocVariantArrayDataSet.Create()
// - the TDataSet will be opened once created
function ToDataSet(aOwner: TComponent; const Data: TVariantDynArray;
  const ColumnNames: array of RawUtf8; const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet; overload;


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
    fPreparedParamsCount: integer;
    fForceUseWideString: boolean;
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
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure ColumnsToJson(WR: TJsonWriter); override;
  end;

  ///	implements a statement via the DB.pas TDataSet/TQuery-like connection
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

{ ************ Shared Wrappers Around DB.pas Classes and Functions }

function InternalBCDToBuffer(const AValue: TBcd; out ADest: TBCDBuffer;
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
  tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue, tmp, PBeg);
  if len <= 0 then
    WR.Add('0')
  else
  begin
    if AValue.SignSpecialPlaces and $80 = $80 then
      WR.Add('-');
    WR.AddNoJsonEscape(PBeg, len);
  end;
end;

function BCDToCurr(const AValue: TBcd; var Curr: currency): boolean;
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue, tmp, PBeg);
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

procedure BCDToUtf8(const AValue: TBcd; var result: RawUtf8);
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue, tmp, PBeg);
  SetString(result, PBeg, len);
end;

function BCDToUtf8(const AValue: TBcd): RawUtf8;
begin
  BCDToUtf8(AValue, result);
end;

function BCDToString(const AValue: TBcd): string;
var
  len: integer;
  PBeg: PAnsiChar;
  tmp: TBCDBuffer;
begin
  len := InternalBCDToBuffer(AValue, tmp, PBeg);
  Ansi7ToString(PWinAnsiChar(PBeg), len, result);
end;


{ ************ TSynVirtualDataSet and TDocVariantArrayDataSet Classes }

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

function TSynVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := AllocMem(sizeof(TRecInfo));
end;

procedure TSynVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TSynVirtualDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
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
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer;
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
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer): boolean;
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
  case Field.DataType of // Data^ points to Int64,Double,Blob,UTF-8
    ftBoolean:
      PWORDBOOL(Dest)^ := PBoolean(Data)^;
    ftInteger:
      PInteger(Dest)^ := PInteger(Data)^;
    ftLargeint, ftFloat, ftCurrency:
      PInt64(Dest)^ := PInt64(Data)^;
    ftDate, ftTime, ftDateTime:
      if PDateTime(Data)^ = 0 then
         // handle 30/12/1899 date as NULL
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
               (TS.Date <= 0) then
              // should match ValidateTimeStamp() expectations 
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
          move(pointer(Temp)^, Dest^, DataLen);
        end;
        PAnsiChar(Dest)[DataLen] := #0;
      end;
    ftWideString:
      begin
        {$ifdef ISDELPHI2007ANDUP}
        // here Dest = PWideChar[] of DataSize bytes
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
    raise EDatabaseError.CreateFmt('%s.GetFieldData unhandled DataType=%s (%d)',
      [ClassName, GetEnumName(TypeInfo(TFieldType), ord(Field.DataType))^,
       ord(Field.DataType)]);
  end;
end;

function TSynVirtualDataSet.GetBlobStream(Field: TField; RowIndex: integer): TStream;
var
  Data: pointer;
  DataLen: integer;
begin
  Data := GetRowFieldData(Field, RowIndex, DataLen, false);
  if Data = nil then // should point to Blob or UTF-8 data
    result := nil
  else
    case Field.DataType of
      ftBlob:
        result := TSynMemoryStream.Create(Data, DataLen);
      ftMemo, ftString:
        result := TRawByteStringStream.Create(
          CurrentAnsiConvert.Utf8BufferToAnsi(Data, DataLen));
    {$ifdef ISDELPHI2007ANDUP}
    ftWideMemo,
    {$endif ISDELPHI2007ANDUP}
    ftWideString:
        result := TRawByteStringStream.Create(
          Utf8DecodeToRawUnicode(Data, DataLen));
    else
      raise EDatabaseError.CreateFmt('%s.CreateBlobStream DataType=%d',
        [ClassName, ord(Field.DataType)]);
    end;
end;

function TSynVirtualDataSet.CreateBlobStream(Field: TField; Mode:
  TBlobStreamMode): TStream;
begin
  if Mode <> bmRead then
    raise EDatabaseError.CreateFmt('%s BLOB should be ReadOnly', [ClassName]);
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
  fIsCursorOpen := false;
end;

procedure TSynVirtualDataSet.InternalFirst;
begin
  fCurrentRow := -1;
end;

procedure TSynVirtualDataSet.InternalGotoBookmark(Bookmark: Pointer);
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
  FillcharFast(Buffer^, sizeof(TRecInfo), 0);
end;

procedure TSynVirtualDataSet.InternalLast;
begin
  fCurrentRow := GetRecordCount;
end;

procedure TSynVirtualDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(TRecInfo) - sizeof(TRecInfoIdentifier);
  InternalInitFieldDefs;
  {$ifdef ISDELPHIXE6}
  if not (lcPersistent in Fields.LifeCycles) then
  {$else}
  if DefaultFields then
  {$endif ISDELPHIXE6}
    CreateFields;
  BindFields(true);
  fCurrentRow := -1;
  fIsCursorOpen := true;
end;

procedure TSynVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fCurrentRow := PRecInfo(Buffer).RowIndentifier;
end;

function TSynVirtualDataSet.IsCursorOpen: boolean;
begin
  result := fIsCursorOpen;
end;

procedure TSynVirtualDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PRecInfoIdentifier(Data)^;
end;

procedure TSynVirtualDataSet.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
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
      raise ERangeError.CreateFmt('%s.SetRecNo(%d) with Count=%d',
        [ClassName, Value + 1, GetRecordCount]);
    DoBeforeScroll;
    fCurrentRow := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

constructor TSynVirtualDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inc(GlobalDataSetCount);
  Name := ClassName + IntToStr(GlobalDataSetCount); // force unique name
end;

function TSynVirtualDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
begin
  result := 0; // nothing found
end;

function TSynVirtualDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): boolean;
var
  i, l, h, found: integer;
  {$ifdef ISDELPHIXE4}
  FieldList: TList<TField>;
  {$else}
  FieldList: TList;
  {$endif ISDELPHIXE4}
begin
  CheckActive;
  result := true;
  if not IsEmpty then
    if VarIsArray(KeyValues) then
    begin
      {$ifdef ISDELPHIXE4}
      FieldList := TList<TField>.Create;
      {$else}
      FieldList := TList.Create;
      {$endif ISDELPHIXE4}
      try
        GetFieldList(FieldList, KeyFields);
        l := VarArrayLowBound(KeyValues, 1);
        h := VarArrayHighBound(KeyValues, 1);
        if (FieldList.Count = 1) and
           (l < h) then
        begin
          found := SearchForField(
            StringToUtf8(KeyFields), KeyValues, Options);
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
              StringToUtf8(TField(FieldList[i]).FieldName), KeyValues[l + i],
              Options);
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

type // as in FMTBcd.pas
  TFMTBcdData = class(TPersistent)
  private
    FBcd: TBcd;
  end;

  TFMTBcdVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VBcd: TFMTBcdData;
    Reserved4: cardinal;
  end;

class procedure TSynVirtualDataSet.BcdWrite(const aWriter: TTextWriter;
  const aValue);
begin
  AddBCD(aWriter, TFMTBcdVarData(aValue).VBcd.FBcd);
end;

function DataSetToJson(Data: TDataSet): RawUtf8;
var
  W: TJsonWriter;
  f: integer;
  blob: TRawByteStringStream;
begin
  result := 'null';
  if Data = nil then
    exit;
  Data.First;
  if Data.Eof then
    exit;
  W := TJsonWriter.Create(nil, true, false);
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
              ftSmallint, ftInteger, ftWord, ftAutoInc:
                W.Add(AsInteger);
              ftLargeint:
                W.Add(TLargeintField(Data.Fields[f]).AsLargeInt);
              ftFloat, ftCurrency: // TCurrencyField is sadly a TFloatField
                W.Add(AsFloat, TFloatField(Data.Fields[f]).Precision);
              ftBCD:
                W.AddCurr(AsCurrency);
              ftFMTBcd:
                AddBcd(W, AsBCD);
              ftTimeStamp, ftDate, ftTime, ftDateTime:
                begin
                  W.Add('"');
                  W.AddDateTime(AsDateTime);
                  W.Add('"');
                end;
              ftString, ftFixedChar, ftMemo, ftGuid:
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
              ftBytes, ftVarBytes, ftBlob, ftGraphic, ftOraBlob, ftOraClob:
                begin
                  blob := TRawByteStringStream.Create;
                  try
                    (Data.Fields[f] as TBlobField).SaveToStream(blob);
                    W.WrBase64(pointer(blob.DataString), length(blob.DataString), true);
                  finally
                    blob.Free;
                  end;
                end;
            {$ifdef ISDELPHI2007ANDUP}
              ftWideMemo, ftFixedWideChar:
                begin
                  W.Add('"');
                  W.AddJsonEscapeW(pointer(AsWideString));
                  W.Add('"');
                end;
            {$endif ISDELPHI2007ANDUP}
            {$ifdef UNICODE}
              ftShortint, ftByte:
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
        W.AddComma;
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
  const Data: TVariantDynArray; const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType);
var
  n, ndx, j: PtrInt;
  first: PDocVariantData;
begin
  fValues := Data;
  n := Length(ColumnNames);
  if n > 0 then
  begin
    if n <> length(ColumnTypes) then
      raise ESynException.CreateUtf8(
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
    first := _Safe(fValues[0], dvObject);
    SetLength(fColumns, first^.Count);
    for ndx := 0 to first^.Count - 1 do
    begin
      fColumns[ndx].Name := first^.Names[ndx];
      fColumns[ndx].FieldType :=
        VariantTypeToSqlDBFieldType(first^.Values[ndx]);
      case fColumns[ndx].FieldType of
        mormot.db.core.ftNull:
          fColumns[ndx].FieldType := mormot.db.core.ftBlob;
        mormot.db.core.ftCurrency:
          fColumns[ndx].FieldType := mormot.db.core.ftDouble;
        mormot.db.core.ftInt64: // ensure type coherency of whole column
          for j := 1 to first^.Count - 1 do
            if j >= Length(fValues) then // check objects are consistent
              break
            else
              with _Safe(fValues[j], dvObject)^ do
                if (ndx < Length(Names)) and
                   IdemPropNameU(Names[ndx], fColumns[ndx].Name) then
                  if VariantTypeToSqlDBFieldType(Values[ndx]) in
                     [mormot.db.core.ftNull, mormot.db.core.ftDouble,
                      mormot.db.core.ftCurrency] then
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
  result := length(fValues);
end;

function TDocVariantArrayDataSet.GetRowFieldData(Field: TField;
  RowIndex: integer; out ResultLen: integer; OnlyCheckNull: boolean): Pointer;
var
  F, ndx: integer;
  wasString: boolean;
begin
  result := nil;
  F := Field.Index;
  if (cardinal(RowIndex) < cardinal(length(fValues))) and
     (cardinal(F) < cardinal(length(fColumns))) and
     not (fColumns[F].FieldType in [mormot.db.core.ftNull,
       mormot.db.core.ftUnknown, mormot.db.core.ftCurrency]) then
    with _Safe(fValues[RowIndex])^ do
      if IsObject and
         (Count > 0) then
      begin
        if IdemPropNameU(fColumns[F].Name, Names[F]) then
          ndx := F
        else
          // optimistic match
          ndx := GetValueIndex(fColumns[F].Name);
        if ndx >= 0 then
          if VarIsEmptyOrNull(Values[ndx]) then
            exit
          else
          begin
            result := @fTemp64;
            if not OnlyCheckNull then
              case fColumns[F].FieldType of
                ftInt64:
                  VariantToInt64(Values[ndx], fTemp64);
                ftDouble, mormot.db.core.ftDate:
                  VariantToDouble(Values[ndx], unaligned(PDouble(@fTemp64)^));
                ftUtf8:
                  begin
                    VariantToUtf8(Values[ndx], fTempUtf8, wasString);
                    result := pointer(fTempUtf8);
                    ResultLen := length(fTempUtf8);
                  end;
                mormot.db.core.ftBlob:
                  begin
                    VariantToUtf8(Values[ndx], fTempUtf8, wasString);
                    if Base64MagicCheckAndDecode(pointer(fTempUtf8),
                        length(fTempUtf8), fTempBlob) then
                    begin
                      result := pointer(fTempBlob);
                      ResultLen := length(fTempBlob);
                    end;
                  end;
              end;
          end;
      end;
end;

procedure TDocVariantArrayDataSet.InternalInitFieldDefs;
const
  TYPES: array[TSqlDBFieldType] of TFieldType = (
  // ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUtf8, ftBlob
    ftWideString, ftWideString, ftLargeint, ftFloat, ftFloat, ftDate,
    ftWideString, ftBlob);
var
  F, siz: integer;
begin
  FieldDefs.Clear;
  for F := 0 to high(fColumns) do
  begin
    if fColumns[F].FieldType = ftUtf8 then
      siz := 16
    else
      siz := 0;
    FieldDefs.Add(
      Utf8ToString(fColumns[F].Name), TYPES[fColumns[F].FieldType], siz);
  end;
end;

function TDocVariantArrayDataSet.SearchForField(const aLookupFieldName: RawUtf8;
  const aLookupValue: variant; aOptions: TLocateOptions): integer;
var
  f: integer;
begin
  f := -1; // allows O(1) field lookup for invariant object columns
  for result := 1 to length(fValues) do
    with _Safe(fValues[result - 1])^ do
      if IsObject and
         (Count > 0) then
      begin
        if (cardinal(f) >= cardinal(Count)) or
           not IdemPropNameU(aLookupFieldName, Names[f]) then
          f := GetValueIndex(aLookupFieldName);
        if (f >= 0) and
           (FastVarDataComp(
             @Values[f], @aLookupValue, loCaseInsensitive in aOptions) = 0) then
          exit;
      end;
  result := 0;
end;

function ToDataSet(aOwner: TComponent; const Data: TVariantDynArray;
  const ColumnNames: array of RawUtf8;
  const ColumnTypes: array of TSqlDBFieldType): TDocVariantArrayDataSet; 
begin
  result := TDocVariantArrayDataSet.Create(
    aOwner, Data, ColumnNames, ColumnTypes);
  result.Open;
end;



{ ************ mormot.db.sql Abstract Connection for DB.pas TDataSet }

const
  IsTLargeIntField = 1;
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
  Str: TStream;
begin
  result := '';
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      exit
    else if TField(ColumnAttr).IsBlob then
    begin
      Str := TField(ColumnAttr).DataSet.CreateBlobStream(
        TField(ColumnAttr), bmRead);
      try
        if Str.Size > 0 then
        begin
          SetLength(result, Str.Size);
          Str.Read(pointer(result)^, Str.Size);
        end;
      finally
        Str.Free;
      end;
    end
    else
    begin
      SetLength(result, TField(ColumnAttr).DataSize);
      TField(ColumnAttr).GetData(TValueBuffer(result));
    end;
end;

function TSqlDBDatasetStatementAbstract.ColumnCurrency(Col: integer): currency;
begin
  CheckCol(Col);
  with fColumns[Col] do
    if TField(ColumnAttr).IsNull then
      result := 0
    else if TField(ColumnAttr).DataType in [ftBCD, ftFMTBcd] then
      BCDToCurr(TField(ColumnAttr).AsBCD, result)
    else
      result := TField(ColumnAttr).AsCurrency;
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
        result := WideStringToUtf8(TWideStringField(ColumnAttr).Value)
      else
      {$endif UNICODE}
        result := StringToUtf8(TField(ColumnAttr).AsString);
end;

constructor TSqlDBDatasetStatementAbstract.Create(aConnection: TSqlDBConnection);
begin
  fForceUseWideString :=
    (aConnection.Properties as TSqlDBDatasetConnectionProperties).ForceUseWideString;
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
  oSQL: RawUtf8;
begin
  SQLLogBegin(sllDB);
  if fPrepared then
    raise ESqlDBDataset.CreateUtf8('%.Prepare() shall be called once', [self]);
  inherited Prepare(aSQL, ExpectResults); // connect if necessary
  fPreparedParamsCount := ReplaceParamsByNames(aSQL, oSQL);
  fPrepared := DatasetPrepare(Utf8ToString(oSQL));
  SQLLogEnd;
  if not fPrepared then
    raise ESqlDBDataset.CreateUtf8('%.DatasetPrepare not prepared', [self]);
end;

procedure TSqlDBDatasetStatementAbstract.ExecutePrepared;
var
  i, p: integer;
  lArrayIndex: integer;
  Field: TField;
begin
  SQLLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  // 1. bind parameters in fParams[] to fQuery.Params
  if fPreparedParamsCount <> fParamCount then
    raise ESqlDBDataset.CreateUtf8(
      '%.ExecutePrepared expected % bound parameters, got %',
      [self, fPreparedParamsCount, fParamCount]);
  lArrayIndex := -1; // either Bind() or BindArray() with no Array DML support
  repeat
    if (not fDatasetSupportBatchBinding) and
       (fParamsArrayCount > 0) then
      inc(lArrayIndex); // enable BindArray() emulation
    for p := 0 to fParamCount - 1 do
      DatasetBindSqlParam(lArrayIndex, p, fParams[p]);
    // 2. Execute query (within a loop for BATCH mode)
    if fExpectResults then
    begin
      fQuery.Open;
      fCurrentRow := -1;
      fColumnCount := 0;
      fColumn.ReHash;
      fColumn.Capacity := fQuery.FieldCount;
      for i := 0 to fQuery.FieldCount - 1 do
      begin
        Field := DatasetField(i);
        with PSqlDBColumnProperty(fColumn.AddAndMakeUniqueName(
              StringToUtf8(Field.FieldName)))^ do
        begin
          ColumnAttr := PtrUInt(Field);
          ColumnType := ColumnTypeNativeToDB(Field.DataType);
          if Field.InheritsFrom(TLargeintField) then
            ColumnValueDBType := IsTLargeIntField
          else if Field.InheritsFrom(TWideStringField) then
            ColumnValueDBType := IsTWideStringField
          else
            ColumnValueDBType := 0;
        end;
      end;
    end
    else
      DatasetExecSQL;
  until fDatasetSupportBatchBinding or
        (lArrayIndex = fParamsArrayCount - 1);
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
  if (fQuery <> nil) and fQuery.Active then
    fQuery.Close;
  inherited ReleaseRows;
end;

function TSqlDBDatasetStatementAbstract.SqlParamTypeToDBParamType(IO:
  TSqlDBParamInOutType): TParamType;
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

function TSqlDBDatasetStatementAbstract.ColumnTypeNativeToDB(aNativeType:
  TFieldType): TSqlDBFieldType;
begin
  case aNativeType of
  {$ifdef UNICODE}
    ftLongWord, ftShortint, ftByte,
  {$endif UNICODE}
    ftAutoInc, ftBoolean, ftSmallint, ftInteger, ftLargeint, ftWord:
      result := mormot.db.core.ftInt64;
  {$ifdef UNICODE}
    ftSingle, ftExtended,
  {$endif UNICODE}
    ftFloat:
      result := mormot.db.core.ftDouble;
    ftCurrency, ftBCD, ftFMTBcd:
      result := mormot.db.core.ftCurrency;
  {$ifdef UNICODE}
    ftOraTimeStamp, ftOraInterval,
  {$endif UNICODE}
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      result := mormot.db.core.ftDate;
    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftOraBlob:
      result := mormot.db.core.ftBlob;
  {$ifdef UNICODE}
    ftFixedWideChar, ftWideMemo,
  {$endif UNICODE} 
    ftString, ftFixedChar, ftWideString, ftMemo, ftFmtMemo,
    ftOraClob, ftVariant, ftGuid:
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

procedure TSqlDBDatasetStatementAbstract.ColumnsToJson(WR: TJsonWriter);
var
  col: integer;
  blob: RawByteString;
begin
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
    with fColumns[col] do
    begin
      if WR.Expand then
        WR.AddFieldName(ColumnName); // add '"ColumnName":'
      if TField(ColumnAttr).IsNull then
        WR.AddNull
      else
        case ColumnType of
          mormot.db.core.ftNull:
            WR.AddNull;
          mormot.db.core.ftInt64:
            if TField(ColumnAttr).DataType = ftBoolean then
              WR.Add(ord(TField(ColumnAttr).AsBoolean))
            else
            {$ifdef UNICODE}
              WR.Add(TField(ColumnAttr).AsLargeInt);
            {$else}
              if ColumnValueDBType=IsTLargeIntField then
                WR.Add(TLargeintField(ColumnAttr).AsLargeInt)
              else
                WR.Add(TField(ColumnAttr).AsInteger);
            {$endif UNICODE}
          mormot.db.core.ftDouble:
            WR.AddDouble(TField(ColumnAttr).AsFloat);
          mormot.db.core.ftCurrency:
            if TField(ColumnAttr).DataType in [ftBCD, ftFMTBcd] then
              AddBcd(WR, TField(ColumnAttr).AsBCD)
            else
              WR.AddCurr(TField(ColumnAttr).AsCurrency);
          mormot.db.core.ftDate:
            begin
              WR.Add('"');
              WR.AddDateTime(TField(ColumnAttr).AsDateTime, fForceDateWithMS);
              WR.Add('"');
            end;
          mormot.db.core.ftUtf8:
            begin
              WR.Add('"');
            {$ifndef UNICODE}
              if ColumnValueDBType=IsTWideStringField then
                WR.AddJsonEscapeW(Pointer(TWideStringField(ColumnAttr).Value))
              else
            {$endif UNICODE}
                WR.AddJsonEscapeString(TField(ColumnAttr).AsString);
              WR.Add('"');
            end;
          mormot.db.core.ftBlob:
            if fForceBlobAsNull then
              WR.AddNull
            else
            begin
              blob := ColumnBlob(col);
              WR.WrBase64(pointer(blob), length(blob), true); // withMagic=true
            end;
        else
          raise ESqlDBException.CreateUtf8('%: Invalid ColumnType()=%',
            [self, ord(ColumnType)]);
        end;
      WR.AddComma;
    end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
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
            P.AsDateTime := PDateTime(@VInt64)^;
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
        raise ESqlDBDataset.CreateUtf8(
          '%.DataSetBindSqlParam: Invalid type % on bound parameter #%',
          [self, ord(VType), aParamIndex + 1]);
      end;
  end;
end;

procedure TSqlDBDatasetStatement.DataSetOutSqlParam(const aParamIndex: integer;
  var aParam: TSqlDBParam);
var
  Par: TParam;
  {$ifdef UNICODE}
  tmpBytes: TBytes;
  {$endif UNICODE}
begin
  Par := fQueryParams[aParamIndex];
  case aParam.VType of
    mormot.db.core.ftInt64:
    {$ifdef UNICODE}
      aParam.VInt64 := Par.AsLargeInt;
    {$else}
      aParam.VInt64 := trunc(Par.AsFloat);
    {$endif UNICODE}
    mormot.db.core.ftDouble:
      unaligned(PDouble(@aParam.VInt64)^) := Par.AsFloat;
    mormot.db.core.ftCurrency:
      PCurrency(@aParam.VInt64)^ := Par.AsCurrency;
    mormot.db.core.ftDate:
      PDateTime(@aParam.VInt64)^ := Par.AsDateTime;
    mormot.db.core.ftUtf8:
      aParam.VData := StringToUtf8(Par.AsString);
    mormot.db.core.ftBlob:
      begin
      {$ifdef UNICODE}
        tmpBytes := Par.AsBlob;
        SetString(aParam.VData, PAnsiChar(pointer(tmpBytes)), Length(tmpBytes));
      {$else}
        aParam.VData := Par.AsString;
      {$endif UNICODE}
      end;
  end;
end;

procedure TSqlDBDatasetStatement.Prepare(const aSQL: RawUtf8; ExpectResults: boolean);
begin
  inherited;
  if fPreparedParamsCount <> fQueryParams.Count then
    raise ESqlDBDataset.CreateUtf8(
      '%.Prepare expected % parameters in request, found % - [%]',
      [self, fPreparedParamsCount, fQueryParams.Count, aSQL]);
end;

initialization
  { TODO : enhance RttiCustom to register VarFMTBcd }
  //TTextWriter.RegisterCustomJsonSerializerForVariantByType(
  // VarFMTBcd,nil,TSynVirtualDataSet.BcdWrite);

{$endif FPC} // currently only supports Delphi DB.pas unit

end.

