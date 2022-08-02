/// Database Framework Core Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.core;

{
  *****************************************************************************

   Shared Types and Definitions for Database Access
    - Shared Database Fields and Values Definitions
    - Nullable Values Stored as Variant
    - Date/Time SQL encoding
    - SQL Parameters Inlining and Processing
    - TResultsWriter Specialized for Database Export
    - TSelectStatement SQL SELECT Parser
    - JSON Object Decoder and SQL Generation
    - TID Processing Functions

    This unit is used by both mormot.db.* units and mormot.orm.* units.

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
  mormot.core.data,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json;


{ ************ Shared Database Fields and Values Definitions }

const
  /// maximum number of fields in a database Table
  // - default is 64, but can be set to 64, 128, 192 or 256
  // adding MAX_SQLFIELDS_128, MAX_SQLFIELDS_192 or MAX_SQLFIELDS_256
  // conditional directives for your project
  // - this constant is used internally to optimize memory usage in the
  // generated asm code, and statically allocate some arrays for better speed
  // - note that due to compiler restriction, 256 is the maximum value
  // (this is the maximum number of items in a Delphi/FPC set)
  {$ifdef MAX_SQLFIELDS_128}
  MAX_SQLFIELDS = 128;
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  MAX_SQLFIELDS = 192;
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  MAX_SQLFIELDS = 256;
  {$else}
  MAX_SQLFIELDS = 64;
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}

  /// sometimes, the ID field is external to the bits set
  MAX_SQLFIELDS_INCLUDINGID = MAX_SQLFIELDS + 1;

  /// maximum number of bound parameters to a SQLite3 statement
  // - empirical value, used e.g. for mormot.orm.sqlite3 Batch multi-insert
  // - matches DB_PARAMSMAX[dSQLite] as defined in mormot.db.sql
  // - the theoritical limit equals 999, but this number seems good enough
  MAX_SQLPARAMS = 500;


type
  /// the exception class raised by this unit
  ESynDBException = class(ESynException);

  /// handled field/parameter/column types for abstract database access
  // - this will map JSON-compatible low-level database-level access types, not
  // high-level object pascal types as TOrmFieldType defined in
  // mormot.orm.core.pas
  // - it does not map either all potential types as defined in DB.pas (which
  // are there for compatibility with old RDBMS, and are not abstract enough)
  // - those types can be mapped to standard SQLite3 generic types, i.e.
  // NULL, INTEGER, REAL, TEXT, BLOB (with the addition of a ftCurrency and
  // ftDate type, for better support of most DB engines)
  // see @http://www.sqlite.org/datatype3.html
  // - the only string type handled here uses UTF-8 encoding (implemented using
  // our RawUtf8 type), for full Unicode process on all compilers and targets
  TSqlDBFieldType = (
    ftUnknown,
    ftNull,
    ftInt64,
    ftDouble,
    ftCurrency,
    ftDate,
    ftUtf8,
    ftBlob);

  /// set of field/parameter/column types for abstract database access
  TSqlDBFieldTypes = set of TSqlDBFieldType;

  /// array of field/parameter/column types for abstract database access
  TSqlDBFieldTypeDynArray = array of TSqlDBFieldType;

  /// array of field/parameter/column types for abstract database access
  // - this array as a fixed size, ready to handle up to MAX_SQLFIELDS items
  TSqlDBFieldTypeArray = array[0..MAX_SQLFIELDS - 1] of TSqlDBFieldType;

  PSqlDBFieldTypeArray = ^TSqlDBFieldTypeArray;

  /// how TSqlVar may be processed
  // - by default, ftDate will use seconds resolution unless svoDateWithMS is set
  TSqlVarOption = (
    svoDateWithMS);

  /// defines how TSqlVar may be processed
  TSqlVarOptions = set of TSqlVarOption;

  /// memory structure used for database values by reference storage
  // - used mainly by mormot.db.sql, mormot.orm.sql and mormot.orm.sqlite3 units
  // - defines only TSqlDBFieldType data types (similar to those handled by
  // SQLite3, with the addition of ftCurrency and ftDate)
  // - cleaner/lighter dedicated type than TValue or variant/TVarData, strong
  // enough to be marshalled as JSON content
  // - variable-length data (e.g. UTF-8 text or binary BLOB) are never stored
  // within this record, but VText/VBlob will point to an external (temporary)
  // memory buffer
  // - date/time is stored as ISO-8601 text (with milliseconds if svoDateWithMS
  // option is set and the database supports it), and currency as double or BCD
  // in most databases
  TSqlVar = record
    /// how this value should be processed
    Options: TSqlVarOptions;
    /// the type of the value stored
    case VType: TSqlDBFieldType of
      ftInt64: (
        VInt64: Int64);
      ftDouble: (
        VDouble: double);
      ftDate: (
        VDateTime: TDateTime);
      ftCurrency: (
        VCurrency: currency);
      ftUtf8: (
        VText: PUtf8Char);
      ftBlob: (
        VBlob: pointer;
        VBlobLen: integer)
  end;

  /// dynamic array of database values by reference storage
  TSqlVarDynArray = array of TSqlVar;

  /// used to store bit set for all available fields in a Table
  // - with current MAX_SQLFIELDS value, 64 bits uses 8 bytes of memory
  // - see also IsZero() and IsEqual() functions
  // - you can also use ALL_FIELDS as defined in this unit
  TFieldBits = set of 0..MAX_SQLFIELDS - 1;

  /// points to a bit set used for all available fields in a Table
  PFieldBits = ^TFieldBits;

  /// used to store a field index in a Table
  // - note that -1 is commonly used for the ID/RowID field so the values should
  // be signed
  // - MAX_SQLFIELDS may be up to 256, so ShortInt (-128..127) would not have
  // been enough, so we use the SmallInt range (-32768..32767)
  TFieldIndex = SmallInt;

  /// used to store field indexes in a Table
  // - same as TFieldBits, but allowing to store the proper order
  TFieldIndexDynArray = array of TFieldIndex;


const
  /// TSqlDBFieldType kind of columns which have a fixed width
  FIXEDLENGTH_SQLDBFIELDTYPE =
    [ftInt64, ftDouble, ftCurrency, ftDate];

  /// conversion matrix from TSqlDBFieldType into VCL/LCL variant type
  // - will use varSynUnicode to enhance Delphi and Windows compatibility
  MAP_FIELDTYPE2VARTYPE: array[TSqlDBFieldType] of Word = (
    varEmpty,       // ftUnknown
    varNull,        // ftNull
    varInt64,       // ftInt64
    varDouble,      // ftDouble
    varCurrency,    // ftCurrency
    varDate,        // ftDate
    varSynUnicode,  // ftUtf8
    varString);     // ftBlob

/// retrieve the text of a given Database field type enumeration
// - see also TSqlDBFieldTypeToString() function
function ToText(Field: TSqlDBFieldType): PShortString; overload;

/// retrieve the ready-to-be displayed text of a given Database field
// type enumeration
function TSqlDBFieldTypeToString(aType: TSqlDBFieldType): TShort16;


/// returns TRUE if no bit inside this TFieldBits is set
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsZero(const Fields: TFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of two TFieldBits values
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsEqual(const A, B: TFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast initialize a TFieldBits with 0
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
procedure FillZero(var Fields: TFieldBits); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TFieldBits set of bits into an array of integers
procedure FieldBitsToIndex(const Fields: TFieldBits;
  out Index: TFieldIndexDynArray; MaxLength: PtrInt = MAX_SQLFIELDS); overload;

/// convert a TFieldBits set of bits into an array of integers
function FieldBitsToIndex(const Fields: TFieldBits;
  MaxLength: PtrInt = MAX_SQLFIELDS): TFieldIndexDynArray; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// add a field index to an array of field indexes
// - returns the index in Indexes[] of the newly appended Field value
function AddFieldIndex(var Indexes: TFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TFieldBits set of bits
procedure FieldIndexToBits(const Index: TFieldIndexDynArray;
  out Fields: TFieldBits); overload;

// search a field index in an array of field indexes
// - returns the index in Indexes[] of the given Field value, -1 if not found
function SearchFieldIndex(var Indexes: TFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TFieldBits set of bits
function FieldIndexToBits(const Index: TFieldIndexDynArray): TFieldBits; overload;
  {$ifdef HASINLINE}inline;{$endif}


/// returns TRUE if the specified field name is either 'ID', either 'ROWID'
function IsRowID(FieldName: PUtf8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif} overload;

/// returns TRUE if the specified field name is either 'ID', either 'ROWID'
function IsRowID(FieldName: PUtf8Char; FieldLen: integer): boolean;
  {$ifdef HASINLINE}inline;{$endif} overload;

/// returns TRUE if the specified field name is either 'ID', either 'ROWID'
function IsRowIDShort(const FieldName: ShortString): boolean;
  {$ifdef HASINLINE}inline;{$endif} overload;

/// returns the stored size of a TSqlVar database value
// - only returns VBlobLen / StrLen(VText) size, 0 otherwise
function SqlVarLength(const Value: TSqlVar): integer;

/// convert any Variant into a database value
// - ftBlob kind won't be handled by this function
// - complex variant types would be converted into ftUtf8 JSON object/array
procedure VariantToSqlVar(const Input: variant; var temp: RawByteString;
  var Output: TSqlVar);

/// convert any Variant into a value encoded as with :(..:) inlined parameters
// in FormatUtf8(Format,Args,Params)
// - will transform into a UTF-8, between double quotes for string values
procedure VariantToInlineValue(const V: Variant; var result: RawUtf8);

/// guess the correct TSqlDBFieldType from a raw variant type
function VariantVTypeToSqlDBFieldType(VType: cardinal): TSqlDBFieldType;

/// guess the correct TSqlDBFieldType from a variant value
function VariantTypeToSqlDBFieldType(const V: Variant): TSqlDBFieldType;
  {$ifdef HASINLINE}inline;{$endif}

/// guess the correct TSqlDBFieldType from the UTF-8 representation of a value
// - won't recognize ftDate nor ftUtf8 prefixes, just TextToVariantNumberType()
function TextToSqlDBFieldType(json: PUtf8Char): TSqlDBFieldType;

type
  /// SQL Query comparison operators
  // - used e.g. by CompareOperator() functions in mormot.orm.storage.pas
  TSqlCompareOperator = (
     soEqualTo,
     soNotEqualTo,
     soLessThan,
     soLessThanOrEqualTo,
     soGreaterThan,
     soGreaterThanOrEqualTo,
     soBeginWith,
     soContains,
     soSoundsLikeEnglish,
     soSoundsLikeFrench,
     soSoundsLikeSpanish);

const
  /// special TFieldBits value containing all field bits set to 1
  ALL_FIELDS: TFieldBits = [0 .. MAX_SQLFIELDS - 1];

  /// convert identified field types into high-level ORM types
  // - as will be implemented in TOrm classes
  SQLDBFIELDTYPE_TO_DELPHITYPE: array[TSqlDBFieldType] of RawUtf8 = (
    '???',        // ftUnknown
    '???',        // ftNull
    'Int64',      // ftInt64
    'Double',     // ftDouble
    'Currency',   // ftCurrency
    'TDateTime',  // ftDate
    'RawUtf8',    // ftUtf8
    'RawBlob');   // ftBlob

var
  /// contains 'ID' as UTF-8 text with positive RefCnt (avoid const realloc)
  ID_TXT: RawUtf8;

  /// contains 'RowID' as UTF-8 text with positive RefCnt (avoid const realloc)
  ROWID_TXT: RawUtf8;

function ToText(op: TSqlCompareOperator): PShortString; overload;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlFieldBits = TFieldBits;
  PSqlFieldBits = PFieldBits;
  TSqlFieldIndex = TFieldIndex;
  TSqlFieldIndexDynArray = TFieldIndexDynArray;

{$endif PUREMORMOT2}



{ ************ Nullable Values Stored as Variant }

type
  /// define a variant published property as a nullable integer
  // - either a varNull or a varInt64 value will be stored in the variant
  // - either a NULL or an INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Int: TNullableInteger read fInt write fInt;
  TNullableInteger = type variant;

  /// define a variant published property as a nullable boolean
  // - either a varNull or a varBoolean value will be stored in the variant
  // - either a NULL or a 0/1 INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Bool: TNullableBoolean read fBool write fBool;
  TNullableBoolean = type variant;

  /// define a variant published property as a nullable floating point value
  // - either a varNull or a varDouble value will be stored in the variant
  // - either a NULL or a FLOAT value will be stored in the database
  // - the property should be defined as such:
  // ! property Flt: TNullableFloat read fFlt write fFlt;
  TNullableFloat = type variant;

  /// define a variant published property as a nullable decimal value
  // - either a varNull or a varCurrency value will be stored in the variant
  // - either a NULL or a FLOAT value will be stored in the database
  // - the property should be defined as such:
  // ! property Cur: TNullableCurrency read fCur write fCur;
  TNullableCurrency = type variant;

  /// define a variant published property as a nullable date/time value
  // - either a varNull or a varDate value will be stored in the variant
  // - either a NULL or a ISO-8601 TEXT value will be stored in the database
  // - the property should be defined as such:
  // ! property Dat: TNullableDateTime read fDat write fDat;
  TNullableDateTime = type variant;

  /// define a variant published property as a nullable timestamp value
  // - either a varNull or a varInt64 value will be stored in the variant
  // - either a NULL or a TTimeLog INTEGER value will be stored in the database
  // - the property should be defined as such:
  // ! property Tim: TNullableTimrency read fTim write fTim;
  TNullableTimeLog = type variant;

  /// define a variant published property as a nullable UTF-8 encoded text
  // - either a varNull or varString (RawUtf8) will be stored in the variant
  // - either a NULL or a TEXT value will be stored in the database
  // - the property should be defined as such:
  // ! property Txt: TNullableUtf8Text read fTxt write fTxt;
  // or for a fixed-width VARCHAR (in external databases), here of 32 max chars:
  // ! property Txt: TNullableUtf8Text index 32 read fTxt write fTxt;
  // - warning: prior to Delphi 2009, since the variant will be stored as
  // RawUtf8 internally, you should not use directly the field value as a
  // VCL string=AnsiString like string(aField) but use VariantToString(aField)
  TNullableUtf8Text = type variant;

  /// can identify the TNullable* supported variant types
  // - as used by NullableVariantType()
  TNullableVariantType = (
    nvtNone,
    nvtInteger,
    nvtBoolean,
    nvtFloat,
    nvtCurrency,
    nvtDateTime,
    nvtTimeLog,
    nvtUtf8Text);

/// detect a TypeInfo(TNullable*) RTTI pointer to nullable variant types
function NullableVariantType(info: PRttiInfo): TNullableVariantType;

var
  /// a nullable integer value containing null
  NullableIntegerNull: TNullableInteger absolute NullVarData;

/// creates a nullable integer value from a supplied constant
// - FPC does not allow direct assignment to a TNullableInteger = type variant
// variable: use this function to circumvent it
function NullableInteger(const Value: Int64): TNullableInteger;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableInteger = type variant variable: use this
// function to circumvent those limitations
function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): boolean;

/// check if a TNullableInteger is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the integer value
function NullableIntegerToValue(const V: TNullableInteger;
  out Value: Int64): boolean; overload;

/// check if a TNullableInteger is null, or return its value
// - returns 0 if V is null or empty, or the stored integer value
function NullableIntegerToValue(const V: TNullableInteger): Int64;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable boolean value containing null
  NullableBooleanNull: TNullableBoolean absolute NullVarData;

/// creates a nullable boolean value from a supplied constant
// - FPC does not allow direct assignment to a TNullableBoolean = type variant
// variable: use this function to circumvent it
function NullableBoolean(Value: boolean): TNullableBoolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableBoolean = type variant variant: use this
// function to circumvent those limitations
function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): boolean;

/// check if a TNullableBoolean is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the boolean value
function NullableBooleanToValue(const V: TNullableBoolean;
  out Value: boolean): boolean; overload;

/// check if a TNullableBoolean is null, or return its value
// - returns false if V is null or empty, or the stored boolean value
function NullableBooleanToValue(const V: TNullableBoolean): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable float value containing null
  NullableFloatNull: TNullableFloat absolute NullVarData;

/// creates a nullable floating-point value from a supplied constant
// - FPC does not allow direct assignment to a TNullableFloat = type variant
// variable: use this function to circumvent it
function NullableFloat(const Value: double): TNullableFloat;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableFloat = type variant variable: use this
// function to circumvent those limitations
function NullableFloatIsEmptyOrNull(const V: TNullableFloat): boolean;

/// check if a TNullableFloat is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Float value
function NullableFloatToValue(const V: TNullableFloat;
  out Value: double): boolean; overload;

/// check if a TNullableFloat is null, or return its value
// - returns 0 if V is null or empty, or the stored Float value
function NullableFloatToValue(const V: TNullableFloat): double;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable currency value containing null
  NullableCurrencyNull: TNullableCurrency absolute NullVarData;

/// creates a nullable Currency value from a supplied currency value
// - we defined the currency type to circumvent FPC cross-platform issues
// with currency values;
// - warning: FPC does not support assignment to a TNullableCurrency = type variant
// variable: use this function to circumvent it
function NullableCurrency(const Value: currency): TNullableCurrency;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableCurrency = type variant variable: use this
// function to circumvent those limitations
function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): boolean;

/// check if a TNullableCurrency is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Currency value
// - we defined the currency type to circumvent FPC cross-platform issues
// with currency values;
function NullableCurrencyToValue(const V: TNullableCurrency;
  out Value: currency): boolean; overload;

/// check if a TNullableCurrency is null, or return its value
// - returns 0 if V is null or empty, or the stored Currency value
// - we defined the currency type to circumvent FPC cross-platform issues
// with currency values;
function NullableCurrencyToValue(const V: TNullableCurrency): currency;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable TDateTime value containing null
  NullableDateTimeNull: TNullableDateTime absolute NullVarData;

/// creates a nullable TDateTime value from a supplied constant
// - FPC does not allow direct assignment to a TNullableDateTime = type variant
// variable: use this function to circumvent it
function NullableDateTime(const Value: TDateTime): TNullableDateTime;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableDateTime = type variant variable: use this
// function to circumvent those limitations
function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): boolean;

/// check if a TNullableDateTime is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the DateTime value
function NullableDateTimeToValue(const V: TNullableDateTime;
  out Value: TDateTime): boolean; overload;

/// check if a TNullableDateTime is null, or return its value
// - returns 0 if V is null or empty, or the stored DateTime value
function NullableDateTimeToValue(const V: TNullableDateTime): TDateTime;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable TTimeLog value containing null
  NullableTimeLogNull: TNullableTimeLog absolute NullVarData;

/// creates a nullable TTimeLog value from a supplied constant
// - FPC does not allow direct assignment to a TNullableTimeLog = type variant
// variable: use this function to circumvent it
function NullableTimeLog(const Value: TTimeLog): TNullableTimeLog;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableTimeLog = type variant variable: use this
// function to circumvent those limitations
function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): boolean;

/// check if a TNullableTimeLog is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog;
  out Value: TTimeLog): boolean; overload;

/// check if a TNullableTimeLog is null, or return its value
// - returns 0 if V is null or empty, or the stored TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable UTF-8 encoded text value containing null
  NullableUtf8TextNull: TNullableUtf8Text absolute NullVarData;

/// creates a nullable UTF-8 encoded text value from a supplied constant
// - FPC does not allow direct assignment to a TNullableUtf8 = type variant
// variable: use this function to circumvent it
function NullableUtf8Text(const Value: RawUtf8): TNullableUtf8Text;

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableUtf8Text = type variant variable: use this
// function to circumvent those limitations
function NullableUtf8TextIsEmptyOrNull(const V: TNullableUtf8Text): boolean;

/// check if a TNullableUtf8Text is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Utf8Text value
function NullableUtf8TextToValue(const V: TNullableUtf8Text;
  out Value: RawUtf8): boolean; overload;

/// check if a TNullableUtf8Text is null, or return its value
// - returns '' if V is null or empty, or the stored UTF-8 encoded text value
function NullableUtf8TextToValue(const V: TNullableUtf8Text): RawUtf8; overload;


{ ************ Date/Time SQL encoding }

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a oftDateTime
// inline parameter by the TExtractInlineParameters decoder
// (JSON_SQLDATE_MAGIC_C will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSql(EncodeDate(2012,5,4))]);
function DateToSql(Date: TDateTime): RawUtf8; overload;

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a oftDateTime
// inline parameter by the TExtractInlineParameters decoder
// (JSON_SQLDATE_MAGIC_C will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSql(2012,5,4)]);
function DateToSql(Year, Month, Day: cardinal): RawUtf8; overload;

/// convert a date/time to a ISO-8601 string format for SQL '?' inlined parameters
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as '\uFFF1YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as '\uFFF1Thh:mm:ss'
// - otherwise, returns the ISO-8601 date and time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
// (JSON_SQLDATE_MAGIC_C will be used as prefix to create '\uFFF1...' pattern)
// - if WithMS is TRUE, will append '.sss' for milliseconds resolution
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSql(Now)]);
// - see TimeLogToSql() if you are using TTimeLog/TModTime/TCreateTime values
function DateTimeToSql(DT: TDateTime; WithMS: boolean = false): RawUtf8;

/// decode a SQL '?' inlined parameter (i.e. with JSON_BASE64_MAGIC_C prefix)
// - as generated by DateToSql/DateTimeToSql/TimeLogToSql functions
function SqlToDateTime(const ParamValueWithMagic: RawUtf8): TDateTime;

/// convert a TTimeLog value into a ISO-8601 string format for SQL '?' inlined
// parameters
// - handle TTimeLog bit-encoded Int64 format
// - follows the same pattern as DateToSql or DateTimeToSql functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// oftDateTime inline parameter by the TExtractInlineParameters decoder
// (JSON_SQLDATE_MAGIC_C will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[TimeLogToSql(TimeLogNow)]);
function TimeLogToSql(const Timestamp: TTimeLog): RawUtf8;

/// convert a Iso8601 encoded string into a ISO-8601 string format for SQL
// '?' inlined parameters
// - follows the same pattern as DateToSql or DateTimeToSql functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// oftDateTime inline parameter by the TExtractInlineParameters decoder
// (JSON_SQLDATE_MAGIC_C will be used as prefix to create '\uFFF1...' pattern)
// - in practice, just append the JSON_BASE64_MAGIC_C prefix to the supplied text
function Iso8601ToSql(const S: RawByteString): RawUtf8;



{ ************ SQL Parameters Inlining and Processing }

type
  /// generic parameter types, as recognized by TExtractInlineParameters.Parse
  TSqlParamType = (
    sptNull,
    sptInteger,
    sptFloat,
    sptText,
    sptBlob,
    sptDateTime);

  /// extract inlined :(1234): parameters into Types[]/Values[]
  {$ifdef USERECORDWITHMETHODS}
  TExtractInlineParameters = record
  {$else}
  TExtractInlineParameters = object
  {$endif USERECORDWITHMETHODS}
  public
    /// Values[0..Count-1] contains the unquoted parameters raw values
    Values: TRawUtf8DynArray;
    /// generic SQL statement with ? place holders for each inlined parameter
    GenericSql: RawUtf8;
    /// the number of parsed parameters, as filled in Values/Types
    Count: integer;
    /// the SQL type associated with each Values[]
    // - recognized types are sptInteger, sptFloat, sptUtf8Text, sptDateTime
    // (marked with '\uFFF1...' trailer) and sptBlob (with '\uFFF0...' trailer)
    // - store sptNull for NULL value
    Types: array[0..MAX_SQLFIELDS - 1] of TSqlParamType;
    /// parse and extract inlined :(1234): parameters
    // - fill Values[0..Count-1] Types[0..Count-1] Nulls and compute the
    // associated GenericSQL with ? place-holders
    // - if SQL as incorrect :(....): inlined parameters, will just copy SQL to
    // GenericSQL and set Count=0
    procedure Parse(const SQL: RawUtf8);
    /// parse one UTF-8 SQL value, as encoded in our inlined :(....): format
    // - low-level function called by Parse() into Values[Count] and Types[Count]
    // - oftInteger is set for an INTEGER value, e.g. :(1234):
    // - oftFloat is set for any floating point value (i.e. some digits
    // separated by a '.' character), e.g. :(12.34): or :(12E-34):
    // - oftUtf8Text is set for :("text"): or :('text'):, with double quoting
    // inside the value
    // - oftBlob will be recognized from the ':("\uFFF0base64encodedbinary"):'
    // pattern, and set raw binary (for direct blob parameter assignment)
    // - oftDateTime will be recognized from ':(\uFFF1"2012-05-04"):' pattern,
    // i.e. JSON_SQLDATE_MAGIC_C-prefixed string as returned by DateToSql() or
    // DateTimeToSql() functions, and set as ISO-8601 date/time text
    // - oftUnknown is set from a NULL value
    // - P=nil is returned on invalid content
    function ParseNext(P: PUtf8Char): PUtf8Char;
  end;

/// returns a 64-bit value as inlined ':(1234):' text
function InlineParameter(ID: Int64): ShortString; overload;

/// returns a string value as inlined ':("value"):' text
function InlineParameter(const value: RawUtf8): RawUtf8; overload;


/// go to the beginning of the SQL statement, ignoring all blanks and comments
// - used to check the SQL statement command (e.g. is it a SELECT?)
function SqlBegin(P: PUtf8Char): PUtf8Char;

/// add a condition to a SQL WHERE clause, with an ' and ' if where is not void
procedure SqlAddWhereAnd(var where: RawUtf8; const condition: RawUtf8);

/// return true if the parameter is void or begin with a 'SELECT' SQL statement
// - used to avoid code injection and to check if the cache must be flushed
// - VACUUM, PRAGMA, or EXPLAIN statements also return true, since they won't
// change the data content
// - WITH recursive statement expect no INSERT/UPDATE/DELETE pattern in the SQL
// - if P^ is a SELECT and SelectClause is set to a variable, it would
// contain the field names, from SELECT ...field names... FROM
function IsSelect(P: PUtf8Char; SelectClause: PRawUtf8 = nil): boolean;

/// compute the SQL corresponding to a WHERE clause
// - returns directly the Where value if it starts with one the
// ORDER/GROUP/LIMIT/OFFSET/JOIN keywords
// - otherwise, append ' WHERE '+Where
function SqlFromWhere(const Where: RawUtf8): RawUtf8;

/// compute a SQL SELECT statement from its parameters
function SqlFromSelect(const TableName, Select, Where, SimpleFields: RawUtf8): RawUtf8;

/// find out if the supplied WHERE clause starts with one of the
// ORDER/GROUP/LIMIT/OFFSET/JOIN keywords
function SqlWhereIsEndClause(const Where: RawUtf8): boolean;

/// get the order table name from a SQL statement
// - return the word following any 'ORDER BY' statement
// - return 'RowID' if none found
function SqlGetOrder(const Sql: RawUTF8): RawUTF8;

/// compute 'PropName in (...)' where clause for a SQL statement
// - if Values has no value, returns ''
// - if Values has a single value, returns 'PropName="Values0"' or inlined
// 'PropName=:("Values0"):' if ValuesInlined is true
// - if Values has more than one value, returns 'PropName in ("Values0","Values1",...)'
// or 'PropName in (:("Values0"):,:("Values1"):,...)' if length(Values)<ValuesInlinedMax
// - PropName can be used as a prefix to the 'in ()' clause, in conjunction
// with optional Suffix value
function SelectInClause(const PropName: RawUtf8; const Values: array of RawUtf8;
  const Suffix: RawUtf8 = ''; ValuesInlinedMax: integer = 0): RawUtf8; overload;

/// compute 'PropName in (...)' where clause for a SQL statement
// - if Values has no value, returns ''
// - if Values has a single value, returns 'PropName=Values0' or inlined
// 'PropName=:(Values0):' if ValuesInlined is bigger than 1
// - if Values has more than one value, returns 'PropName in (Values0,Values1,...)'
// or 'PropName in (:(Values0):,:(Values1):,...)' if length(Values)<ValuesInlinedMax
// - PropName can be used as a prefix to the 'in ()' clause, in conjunction
// with optional Suffix value
function SelectInClause(const PropName: RawUtf8; const Values: array of TID;
  const Suffix: RawUtf8 = ''; ValuesInlinedMax: integer = 0): RawUtf8; overload;

/// naive search of '... FROM TableName ...' pattern in the supplied SQL
function GetTableNameFromSqlSelect(const SQL: RawUtf8;
  EnsureUniqueTableInFrom: boolean): RawUtf8;

/// naive search of '... FROM Table1,Table2 ...' pattern in the supplied SQL
function GetTableNamesFromSqlSelect(const SQL: RawUtf8): TRawUtf8DynArray;


{ ************ TResultsWriter Specialized for Database Export }

type
  /// simple writer to a Stream, specialized for SQL export as JSON
  // - i.e. define some property/method helpers to export SQL resultset as JSON
  TResultsWriter = class(TJsonWriter)
  protected
    /// used to store output format
    fExpand: boolean;
    /// used to store output format for TOrm.GetJsonValues()
    fWithID: boolean;
    /// used to store field for TOrm.GetJsonValues()
    fFields: TFieldIndexDynArray;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    fStartDataPosition: integer;
  public
    /// used internally to store column names and count for AddColumns
    ColNames: TRawUtf8DynArray;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TFieldBits; aBufSize: integer = 8192); overload;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TFieldIndexDynArray = nil; aBufSize: integer = 8192;
      aStackBuffer: PTextWriterStackBuffer = nil); overload;
    /// rewind the Stream position and write void JSON object
    procedure CancelAllVoid;
    /// write or init field names for appropriate JSON Expand later use
    // - ColNames[] must have been initialized before calling this procedure
    // - if aKnownRowsCount is not null, a "rowCount":... item will be added
    // to the generated JSON stream (for faster unserialization of huge content)
    procedure AddColumns(aKnownRowsCount: integer = 0);
    /// write or init field names for appropriate JSON Expand later use
    // - accept a name directly supplied by the DB provider
    // - if Expand is true, will set ColNames[] with the expected format
    // - on Expand=false format, will directly write aColName to W
    procedure AddColumn(aColName: PUtf8Char; aColIndex, aColCount: PtrInt);
    /// allow to change on the fly an expanded format column layout
    // - by definition, a non expanded format will raise a ESynDBException
    // - caller should then set ColNames[] and run AddColumns()
    procedure ChangeExpandedFields(aWithID: boolean;
      const aFields: TFieldIndexDynArray); overload;
    /// end the serialized JSON object
    // - cancel last ','
    // - close the JSON object ']' or ']}'
    // - write non expanded postlog (,"rowcount":...), if needed
    // - flush the internal buffer content if aFlushFinal=true
    procedure EndJsonObject(aKnownRowsCount,aRowsCount: integer;
      aFlushFinal: boolean = true);
      {$ifdef HASINLINE}inline;{$endif}
    /// the first data row is erased from the content
    // - only works if the associated storage stream is TMemoryStream
    // - expect not Expanded format
    procedure TrimFirstRow;
    /// is set to TRUE in case of Expanded format
    property Expand: boolean
      read fExpand write fExpand;
    /// is set to TRUE if the ID field must be appended to the resulting JSON
    // - this field is used only by TOrm.GetJsonValues
    // - this field is ignored by TOrmTable.GetJsonValues
    property WithID: boolean
      read fWithID;
    /// Read-Only access to the field bits set for each column to be stored
    property Fields: TFieldIndexDynArray
      read fFields;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    property StartDataPosition: integer
      read fStartDataPosition;
  end;


{ ************ TSelectStatement SQL SELECT Parser }

type
  /// function prototype used to retrieve the index of a specified property name
  // - 'ID' is handled separately: here must be available only the custom fields
  TOnGetFieldIndex = function(const PropName: RawUtf8): integer of object;

  /// the recognized operators for a TSelectStatement where clause
  TSelectStatementOperator = (
     opEqualTo,
     opNotEqualTo,
     opLessThan,
     opLessThanOrEqualTo,
     opGreaterThan,
     opGreaterThanOrEqualTo,
     opIn,
     opIsNull,
     opIsNotNull,
     opLike,
     opContains,
     opFunction);

  /// a set of operators recognized by a TSelectStatement where clause
  TSelectStatementOperators = set of TSelectStatementOperator;

  /// one recognized SELECT expression for TSelectStatement
  TSelectStatementSelect = record
    /// the column SELECTed for the SQL statement, in the expected order
    // - contains 0 for ID/RowID, or the RTTI field index + 1
    Field: integer;
    /// an optional integer to be added
    // - recognized from .. +123 .. -123 patterns in the select
    ToBeAdded: integer;
    /// the optional column alias, e.g. 'MaxID' for 'max(id) as MaxID'
    Alias: RawUtf8;
    /// the optional function applied to the SELECTed column
    // - e.g. Max(RowID) would store 'Max' and SelectField[0]=0
    // - but Count( * ) would store 'Count' and SelectField[0]=0, and
    // set FunctionIsCountStart = TRUE
    FunctionName: RawUtf8;
    /// if the function needs a special process
    // - e.g. funcCountStar for the special Count( * ) expression or
    // funcDistinct, funcMax for distinct(...)/max(...) aggregation
    FunctionKnown: (
      funcNone, funcCountStar, funcDistinct, funcMax);
    /// MongoDB-like sub field e.g. 'mainfield.subfield1.subfield2'
    // - still identifying 'mainfield' in Field index, and setting
    // SubField='.subfield1.subfield2'
    SubField: RawUtf8;
  end;

  /// the recognized SELECT expressions for TSelectStatement
  TSelectStatementSelectDynArray = array of TSelectStatementSelect;

  /// one recognized WHERE expression for TSelectStatement
  TSelectStatementWhere = record
    /// expressions are evaluated as AND unless this field is set to TRUE
    JoinedOR: boolean;
    /// if this expression is preceded by a NOT modifier
    NotClause: boolean;
    /// the operator of the WHERE expression
    Operation: TSelectStatementOperator;
    /// the index of the field used for the WHERE expression
    // - WhereField=0 for ID, 1 for field #0, 2 for field #1,
    // and so on... (i.e. WhereField = RTTI field index +1)
    Field: integer;
    /// MongoDB-like sub field e.g. 'mainfield.subfield1.subfield2'
    // - still identifying 'mainfield' in Field index, and setting
    // SubField='.subfield1.subfield2'
    SubField: RawUtf8;
    /// the SQL function name associated to a Field and Value
    // - e.g. 'INTEGERDYNARRAYCONTAINS' and Field=0 for
    // IntegerDynArrayContains(RowID,10) and ValueInteger=10
    // - Value does not contain anything
    FunctionName: RawUtf8;
    /// any '(' before the actual expression
    ParenthesisBefore: RawUtf8;
    /// any ')' after the actual expression
    ParenthesisAfter: RawUtf8;
    /// the value used for the WHERE expression
    Value: RawUtf8;
    /// the raw value SQL buffer used for the WHERE expression
    ValueSql: PUtf8Char;
    /// the raw value SQL buffer length used for the WHERE expression
    ValueSqlLen: integer;
    /// an integer representation of WhereValue (used for ID check e.g.)
    ValueInteger: integer;
    /// the value used for the WHERE expression, encoded as Variant
    // - may be a TDocVariant for the IN operator
    ValueVariant: variant;
  end;

  /// the recognized WHERE expressions for TSelectStatement
  TSelectStatementWhereDynArray = array of TSelectStatementWhere;

  /// used to parse a SELECT SQL statement, following the SQlite3 syntax
  // - handle basic REST commands, i.e. a SELECT over a single table (no JOIN)
  // with its WHERE clause, and result column aliases
  // - handle also aggregate functions like "SELECT Count( * ) FROM TableName"
  // - will also parse any LIMIT, OFFSET, ORDER BY, GROUP BY statement clause
  TSelectStatement = class
  protected
    fSqlStatement: RawUtf8;
    fSelect: TSelectStatementSelectDynArray;
    fSelectFunctionCount: integer;
    fTableName: RawUtf8;
    fWhere: TSelectStatementWhereDynArray;
    fOrderByField: TFieldIndexDynArray;
    fGroupByField: TFieldIndexDynArray;
    fWhereHasParenthesis, fHasSelectSubFields, fWhereHasSubFields: boolean;
    fLimit: integer;
    fOffset: integer;
    fWriter: TResultsWriter;
    fOrderByFieldDesc: TFieldBits;
  public
    /// parse the given SELECT SQL statement and retrieve the corresponding
    // parameters into this class read-only properties
    // - the supplied GetFieldIndex() method is used to populate the
    // SelectedFields and Where[].Field properties; SimpleFields is used for '*'
    // field names - typically set from TOrmProperties.Fields.IndexByName and
    // TOrmProperties.SimpleFieldSelect
    // - SqlStatement is left '' if the SQL statement is not correct
    // - if SqlStatement is set, the caller must check for TableName to match
    // the expected value, then use the Where[] to retrieve the content
    constructor Create(const SQL: RawUtf8; const GetFieldIndex: TOnGetFieldIndex;
      const SimpleFields: TSelectStatementSelectDynArray);
    /// compute the SELECT column bits from the Select[] array
    // - optionally set Select[].SubField into SubFields[Select[].Field]
    // (e.g. to include specific fields from MongoDB embedded document)
    procedure SelectFieldBits(var Fields: TFieldBits; var withID: boolean;
      SubFields: PRawUtf8Array = nil);

    /// the SELECT SQL statement parsed
    // - equals '' if the parsing failed
    property SqlStatement: RawUtf8
      read fSqlStatement;
    /// the column SELECTed for the SQL statement, in the expected order
    property Select: TSelectStatementSelectDynArray
      read fSelect;
    /// if the SELECTed expression of this SQL statement have any function defined
    property SelectFunctionCount: integer
      read fSelectFunctionCount;
    /// the retrieved table name
    property TableName: RawUtf8
      read fTableName;
    /// if any Select[].SubField was actually set
    property HasSelectSubFields: boolean
      read fHasSelectSubFields;
    /// the WHERE clause of this SQL statement
    property Where: TSelectStatementWhereDynArray
      read fWhere;
    /// if the WHERE clause contains any ( ) parenthesis expression
    property WhereHasParenthesis: boolean
      read fWhereHasParenthesis;
    /// if the WHERE clause contains any Where[].SubField
    property WhereHasSubFields: boolean
      read fWhereHasSubFields;
    /// recognize an GROUP BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property GroupByField: TFieldIndexDynArray
      read fGroupByField;
    /// recognize an ORDER BY clause with one or several fields
    // - here 0 = ID, otherwise RTTI field index +1
    property OrderByField: TFieldIndexDynArray
      read fOrderByField;
    /// recognize an ORDER BY ... DESC clause with one or several fields
    // - follow the indexes within OrderByField[]
    property OrderByFieldDesc: TFieldBits
      read fOrderByFieldDesc;
    /// the number specified by the optional LIMIT ... clause
    // - set to 0 by default (meaning no LIMIT clause)
    property Limit: integer
      read fLimit;
    /// the number specified by the optional OFFSET ... clause
    // - set to 0 by default (meaning no OFFSET clause)
    property Offset: integer
      read fOffset;
    /// optional associated writer
    property Writer: TResultsWriter
      read fWriter write fWriter;
  end;

function ToText(Op: TSelectStatementOperator): PShortString; overload;

// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSynTableStatement = TSelectStatement;

{$endif PUREMORMOT2}


{ ************ TID Processing Functions }

/// cast a TID into a TOrm instance
// - just like pointer(Value) but cross-platform and more explicit
function CastID(Value: TID): pointer;
  {$ifdef HASINLINE}inline;{$endif}

/// similar to AddInt64() function, but for a TIDDynArray
// - some random GPF were identified with AddInt64(TInt64DynArray(Values),...)
// with the Delphi Win64 compiler
procedure AddID(var Values: TIDDynArray; var ValuesCount: integer;
  Value: TID); overload;

/// similar to AddInt64() function, but for a TIDDynArray
// - some random GPF were identified with AddInt64(TInt64DynArray(Values),...)
// with the Delphi Win64 compiler
procedure AddID(var Values: TIDDynArray; Value: TID); overload;

/// set the TID (=64-bit integer) value from the numerical text stored in P^
// - just a redirection to SetInt64()
procedure SetID(P: PUtf8Char; var result: TID); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// set the TID (=64-bit integer) value from the numerical text stored in U
// - just a redirection to SetInt64()
procedure SetID(const U: RawByteString; var result: TID); overload;
  {$ifdef HASINLINE}inline;{$endif}



{ ************ JSON Object Decoder and SQL Generation }

type
  /// the supported SQL database dialects
  // - will be used e.g. for TSqlDBConnectionProperties.SqlFieldCreate(), or
  // for OleDB/ODBC/ZDBC tuning according to the connected database engine
  TSqlDBDefinition = (
    dUnknown,
    dDefault,
    dOracle,
    dMSSQL,
    dJet,
    dMySQL,
    dSQLite,
    dFirebird,
    dNexusDB,
    dPostgreSQL,
    dDB2,
    dInformix);

  /// set of the available database definitions
  TSqlDBDefinitions = set of TSqlDBDefinition;

  /// the available options for TRest.BatchStart() process
  // - boInsertOrIgnore will create 'INSERT OR IGNORE' statements instead of
  // plain 'INSERT' - supported only by SQLite3 and MySQL
  // - boInsertOrUpdate will create 'REPLACE' statements instead of
  // plain 'INSERT' - supported only by SQLite3, Firebird and MySQL
  // - boExtendedJson will force the JSON to unquote the column names,
  // e.g. writing col1:...,col2:... instead of "col1":...,"col2"...
  // - boPostNoSimpleFields (client-side only) will avoid to send a
  // TRestBach.Add() with simple fields as "SIMPLE":[val1,val2...] or
  // "SIMPLE@tablename":[val1,val2...], without the field names - for
  // backward compatibility with very old mORMot 1 servers
  // - boPutNoCacheFlush won't force the associated Cache entry to be flushed:
  // it is up to the caller to ensure cache coherency
  // - boRollbackOnError will raise an exception and Rollback any transaction
  // if any step failed - default if to continue batch processs, but setting
  // a value <> 200/HTTP_SUCCESS in Results[]
  // - boNoModelEncoding (client-side only) will force the mORMot 1 more verbose
  // "POST"/"SIMPLE"/"PUT" encodings - for backward compatibility, or if client
  // and server TOrmModel tables or fields do not match
  // - boOnlyObjects will force to generate only a JSON array of raw JSON
  // objects with no BATCH prefix nor verbs
  TRestBatchOption = (
    boInsertOrIgnore,
    boInsertOrReplace,
    boExtendedJson,
    boPostNoSimpleFields,
    boPutNoCacheFlush,
    boRollbackOnError,
    boNoModelEncoding,
    boOnlyObjects);

  /// a set of options for TRest.BatchStart() process
  // - TJsonObjectDecoder will use it to compute the corresponding SQL
  TRestBatchOptions = set of TRestBatchOption;

  PRestBatchOptions = ^TRestBatchOptions;

  /// define how TJsonObjectDecoder.Decode() will handle JSON string values
  TJsonObjectDecoderParams = (
    pInlined,
    pQuoted,
    pNonQuoted);

  /// define how TJsonObjectDecoder.FieldTypeApproximation[] is identified
  TJsonObjectDecoderFieldType = (
    ftaNumber,
    ftaBoolean,
    ftaString,
    ftaDate,
    ftaNull,
    ftaBlob,
    ftaObject,
    ftaArray);

  /// exception class raised by TJsonObjectDecoder
  EJsonObjectDecoder = class(ESynException);

  /// JSON object decoding and SQL generation, in the context of ORM process
  // - this is the main process for marshalling JSON into SQL statements
  // - used e.g. by GetJsonObjectAsSql() function or ExecuteFromJson and
  // InternalBatchStop methods
  {$ifdef USERECORDWITHMETHODS}
  TJsonObjectDecoder = record
  {$else}
  TJsonObjectDecoder = object
  {$endif USERECORDWITHMETHODS}
  public
    /// contains the decoded field names text
    FieldNames: array[0..MAX_SQLFIELDS - 1] of PUtf8Char;
    /// contains the decoded field names length
    FieldNamesL: array[0..MAX_SQLFIELDS - 1] of byte;
    /// contains the decoded field values
    FieldValues: array[0..MAX_SQLFIELDS - 1] of RawUtf8;
    /// Decode() will set each field type approximation
    // - will recognize also JSON_BASE64_MAGIC_C/JSON_SQLDATE_MAGIC_C prefix
    FieldTypeApproximation:
      array[0..MAX_SQLFIELDS - 1] of TJsonObjectDecoderFieldType;
    /// number of fields decoded in FieldNames[] and FieldValues[]
    FieldCount: integer;
    /// define if and how the parameters are to be :(...): inlined
    InlinedParams: TJsonObjectDecoderParams;
    /// internal pointer over field names to be used after Decode() call
    // - either FieldNames[], either Fields[] array as defined in Decode(), or
    // external names as set by TRestStorageExternal.JsonDecodedPrepareToSql
    DecodedFieldNames: PPUtf8CharArray;
    /// the ID=.. value as sent within the JSON object supplied to Decode()
    DecodedRowID: TID;
    /// internal pointer over field types to be used after Decode() call
    // - to create 'INSERT INTO ... SELECT UNNEST(...)' or 'UPDATE ... FROM
    // SELECT UNNEST(...)' statements for very efficient bulk writes in a
    // PostgreSQL database
    // - as set by TRestStorageExternal.JsonDecodedPrepareToSql when
    // cPostgreBulkArray flag is detected - for mormot.db.sql.postgres.pas
    DecodedFieldTypesToUnnest: PSqlDBFieldTypeArray;
    /// decode the JSON object fields into FieldNames[] and FieldValues[]
    // - if Fields=nil, P should be a true JSON object, i.e. defined
    // as "COL1"="VAL1" pairs, stopping at '}' or ']'; otherwise, Fields[]
    // contains column names and expects a JSON array as "VAL1","VAL2".. in P
    // - P should be after the initial '{' or '[' character, i.e. at first field
    // - P returns the next object start or nil on unexpected end of input
    // - P^ buffer will let the JSON be decoded in-place, so consider using
    // the overloaded Decode(Json: RawUtf8; ...) method
    // - FieldValues[] strings will be quoted and/or inlined depending on Params
    // - if RowID is set, a RowID column will be added within the returned content
    procedure DecodeInPlace(var P: PUtf8Char; const Fields: TRawUtf8DynArray;
      Params: TJsonObjectDecoderParams; const RowID: TID = 0;
      ReplaceRowIDWithID: boolean = false);
    /// decode the JSON object fields into FieldNames[] and FieldValues[]
    // - overloaded method expecting a UTF-8 buffer private copy
    // - once done with the result, should call Json.Done
    procedure Decode(var Json: TSynTempBuffer;
      const Fields: TRawUtf8DynArray; Params: TJsonObjectDecoderParams;
      const RowID: TID; ReplaceRowIDWithID: boolean = false);
    /// can be used after Decode() to add a new field in FieldNames/FieldValues
    // - so that EncodeAsSql() will include this field in the generated SQL
    // - caller should ensure that the FieldName is not already defined in
    // FieldNames[] (e.g. when the TRecordVersion field is forced), and
    // will be stored in FieldNames[] as pointer, so FieldName variable
    // should remain untouched in memory during all the decoding
    // - the caller should ensure that the supplied FieldValue will match
    // the quoting/inlining expectations of Decode(TJsonObjectDecoderParams) -
    // e.g. that string values are quoted if needed
    procedure AddFieldValue(const FieldName, FieldValue: RawUtf8;
      FieldType: TJsonObjectDecoderFieldType);
    /// encode as a SQL-ready INSERT or UPDATE statement
    // - after a successfull call to Decode()
    // - escape SQL strings, according to the official SQLite3 documentation
    // (i.e. ' inside a string is stored as '')
    // - if InlinedParams was TRUE, it will create prepared parameters like
    // 'COL1=:("VAL1"):, COL2=:(VAL2):'
    // - called by GetJsonObjectAsSql() function or TRestStorageExternal
    function EncodeAsSql(const Prefix1, Prefix2: RawUtf8; Update: boolean;
      Prefix1Batch: PRestBatchOptions; DB: TSqlDBDefinition): RawUtf8;
    /// encode the FieldNames/FieldValues[] as a JSON object
    procedure EncodeAsJson(out result: RawUtf8);
    /// set the specified array to the fields names
    // - after a successfull call to Decode()
    procedure AssignFieldNamesTo(var Fields: TRawUtf8DynArray);
    /// returns TRUE if the specified array match the decoded fields names
    // - after a successfull call to Decode()
    function SameFieldNames(const Fields: TRawUtf8DynArray): boolean;
    /// search for a field name in the current identified FieldNames[]
    function FindFieldName(const FieldName: RawUtf8): PtrInt;
    /// returns the decoded field names as CSV text
    function GetFieldNames: RawUtf8;
  end;


/// decode JSON fields object into an UTF-8 encoded SQL-ready statement
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that it is an unique string
// - P should be after the initial '{' or '[' character, i.e. at first field
// - P contains the next object start or nil on unexpected end of input
// - if Fields is void, expects expanded "COL1"="VAL1" pairs in P^, stopping at '}' or ']'
// - otherwise, Fields[] contains the column names and expects "VAL1","VAL2".. in P^
// - returns 'COL1="VAL1", COL2=VAL2' if UPDATE is true (UPDATE SET format)
// - returns '(COL1, COL2) values ("VAL1", VAL2)' otherwise (INSERT format)
// - escape SQL strings, according to the official SQLite3 documentation
// (i.e. ' inside a string is stored as '')
// - if InlinedParams is set, will create prepared parameters like
// 'COL1=:("VAL1"):, COL2=:(VAL2):'
// - if RowID is set, a RowID column will be added within the returned content
function GetJsonObjectAsSql(var P: PUtf8Char; const Fields: TRawUtf8DynArray;
  Update, InlinedParams: boolean; RowID: TID = 0;
  ReplaceRowIDWithID: boolean = false): RawUtf8; overload;

/// decode JSON fields object into an UTF-8 encoded SQL-ready statement
// - is used e.g. by TRestServerDB.EngineAdd/EngineUpdate methods
// - expect a regular JSON expanded object as "COL1"="VAL1",...} pairs
// - make its own temporary copy of JSON data before calling GetJsonObjectAsSql() above
// - returns 'COL1="VAL1", COL2=VAL2' if UPDATE is true (UPDATE SET format)
// - returns '(COL1, COL2) values ("VAL1", VAL2)' otherwise (INSERT format)
// - if InlinedParams is set, will create prepared parameters like 'COL2=:(VAL2):'
// - if RowID is set, a RowID column will be added within the returned content
function GetJsonObjectAsSql(const Json: RawUtf8; Update, InlinedParams: boolean;
  RowID: TID = 0; ReplaceRowIDWithID: boolean = false): RawUtf8; overload;

/// get the FIRST field value of the FIRST row, from a JSON content
// - e.g. useful to get an ID without converting a JSON content into a TOrmTableJson
function UnJsonFirstField(var P: PUtf8Char): RawUtf8;

/// returns TRUE if the JSON content is in expanded format
// - i.e. as plain [{"ID":10,"FirstName":"John","LastName":"Smith"}...]
// - i.e. not as '{"fieldCount":3,"values":["ID","FirstName","LastName",...']}
function IsNotAjaxJson(P: PUtf8Char): boolean;

/// retrieve a JSON '{"Name":Value,....}' object
// - P is nil in return in case of an invalid object
// - returns the UTF-8 encoded JSON object, including first '{' and last '}'
// - if ExtractID is set, it will contain the "ID":203 field value, and this
// field won't be included in the resulting UTF-8 encoded JSON object unless
// KeepIDField is true
// - this function expects this "ID" property to be the FIRST in the
// "Name":Value pairs, as generated by TOrm.GetJsonValues(W)
procedure JsonGetObject(var P: PUtf8Char; ExtractID: PID;
  var EndOfObject: AnsiChar; KeepIDField: boolean; out JsonObject: RawUtf8);

/// retrieve the ID/RowID field of a JSON object
// - this function expects this "ID" property to be the FIRST in the
// "Name":Value pairs, as generated by TOrm.GetJsonValues(W)
// - returns TRUE if a ID/RowID>0 has been found, and set ID with the value
function JsonGetID(P: PUtf8Char; out ID: TID): boolean;

/// append "insert / insert or ignore / replace into ' text into W
// - depending on boInsertOrIgnore/boInsertOrReplace presence in BatchOptions
// - SQLite3 and MySQL should understand "REPLACE INTO"
// - Firebird has its "UPDATE OR INSERT INTO" own syntax
// - other databases are not supported, because they require a much more complex
// SQL statement to produce the same effect - a prefix is not enough
procedure EncodeInsertPrefix(W: TTextWriter; BatchOptions: TRestBatchOptions;
  DB: TSqlDBDefinition);




implementation


{ ************ Shared Database Fields and Values Definitions }

function ToText(Field: TSqlDBFieldType): PShortString;
begin
  result := GetEnumName(TypeInfo(TSqlDBFieldType), ord(Field));
end;

function TSqlDBFieldTypeToString(aType: TSqlDBFieldType): TShort16;
begin
  if aType <= high(aType) then
    result := TrimLeftLowerCaseToShort(ToText(aType))
  else
    FormatShort16('#%', [ord(aType)], result);
end;

function IsZero(const Fields: TFieldBits): boolean;
var
  f: TPtrIntArray absolute Fields;
begin
  {$ifdef CPU64}
  {$ifdef MAX_SQLFIELDS_128}
  result := (f[0] = 0) and
            (f[1] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (f[0] = 0) and
            (f[1] = 0) and
            (f[2] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (f[0] = 0) and
            (f[1] = 0) and
            (f[2] = 0) and
            (f[3] = 0);
  {$else}
  result := (f[0] = 0);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$else}
  {$ifdef MAX_SQLFIELDS_128}
  result := (f[0] = 0) and
            (f[1] = 0) and
            (f[2] = 0) and
            (f[3] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (f[0] = 0) and
            (f[1] = 0) and
            (f[2] = 0) and
            (f[3] = 0) and
            (f[4] = 0) and
            (f[5] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (f[0] = 0) and
            (f[1] = 0) and
            (f[2] = 0) and
            (f[3] = 0) and
            (f[4] = 0) and
            (f[5] = 0) and
            (f[6] = 0) and
            (f[7] = 0);
  {$else}
  result := (f[0] = 0) and
            (f[1] = 0);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$endif CPU64}
end;

function IsEqual(const A, B: TFieldBits): boolean;
var
  a_: TPtrIntArray absolute A;
  b_: TPtrIntArray absolute B;
begin
  {$ifdef CPU64}
  {$ifdef MAX_SQLFIELDS_128}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]) and
            (a_[2] = b_[2]);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]) and
            (a_[2] = b_[2]) and
            (a_[3] = b_[3]);
  {$else}
  result := (a_[0] = b_[0]);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$else}
  {$ifdef MAX_SQLFIELDS_128}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]) and
            (a_[2] = b_[2]) and
            (a_[3] = b_[3]);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]) and
            (a_[2] = b_[2]) and
            (a_[3] = b_[3]) and
            (a_[4] = b_[4]) and
            (a_[5] = b_[5]);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]) and
            (a_[2] = b_[2]) and
            (a_[3] = b_[3]) and
            (a_[4] = b_[4]) and
            (a_[5] = b_[5]) and
            (a_[6] = b_[6]) and
            (a_[7] = b_[7]);
  {$else}
  result := (a_[0] = b_[0]) and
            (a_[1] = b_[1]);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$endif CPU64}
end;

procedure FillZero(var Fields: TFieldBits);
begin
  {$ifdef MAX_SQLFIELDS_128}
  PInt64Array(@Fields)^[0] := 0;
  PInt64Array(@Fields)^[1] := 0;
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  PInt64Array(@Fields)^[0] := 0;
  PInt64Array(@Fields)^[1] := 0;
  PInt64Array(@Fields)^[2] := 0;
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  PInt64Array(@Fields)^[0] := 0;
  PInt64Array(@Fields)^[1] := 0;
  PInt64Array(@Fields)^[2] := 0;
  PInt64Array(@Fields)^[3] := 0;
  {$else}
  PInt64(@Fields)^ := 0;
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
end;

procedure FieldBitsToIndex(const Fields: TFieldBits;
  out Index: TFieldIndexDynArray; MaxLength: PtrInt);
var
  i: PtrInt;
  p: ^TFieldIndex;
begin
  if MaxLength > MAX_SQLFIELDS then
    raise ESynDBException.CreateUtf8('FieldBitsToIndex(MaxLength=%)', [MaxLength]);
  SetLength(Index, GetBitsCount(Fields, MaxLength));
  p := pointer(Index);
  for i := 0 to MaxLength - 1 do
    if byte(i) in Fields then
    begin
      p^ := i;
      inc(p);
    end;
end;

function FieldBitsToIndex(
  const Fields: TFieldBits; MaxLength: PtrInt): TFieldIndexDynArray;
begin
  FieldBitsToIndex(Fields, result, MaxLength);
end;

function AddFieldIndex(var Indexes: TFieldIndexDynArray; Field: integer): integer;
begin
  result := length(Indexes);
  SetLength(Indexes, result + 1);
  Indexes[result] := Field;
end;

function SearchFieldIndex(var Indexes: TFieldIndexDynArray; Field: integer): integer;
begin
  for result := 0 to length(Indexes) - 1 do
    if Indexes[result] = Field then
      exit;
  result := -1;
end;

procedure FieldIndexToBits(const Index: TFieldIndexDynArray;
  out Fields: TFieldBits);
var
  i: integer;
begin
  FillZero(Fields{%H-});
  for i := 0 to Length(Index) - 1 do
    if Index[i] >= 0 then
      include(Fields, Index[i]);
end;

function FieldIndexToBits(const Index: TFieldIndexDynArray): TFieldBits;
begin
  FieldIndexToBits(Index, result);
end;

function SqlVarLength(const Value: TSqlVar): integer;
begin
  case Value.VType of
    ftBlob:
      result := Value.VBlobLen;
    ftUtf8:
      result := StrLen(Value.VText); // fast enough for our purpose
  else
    result := 0; // simple/ordinal values, or ftNull
  end;
end;

{$ifdef CPUX64}
function IsRowID(FieldName: PUtf8Char): boolean;
var
  f: Int64;
begin
  if FieldName <> nil then
  begin
    f := PInt64(FieldName)^;
    result := (cardinal(f) and $ffdfdf = (ord('I') + ord('D') shl 8)) or
        (f and $ffdfdfdfdfdf = (ord('R') + ord('O') shl 8 + ord('W') shl 16 +
          ord('I') shl 24 + Int64(ord('D')) shl 32))
  end
  else
    result := false;
end;
{$else}
function IsRowID(FieldName: PUtf8Char): boolean;
begin
  if FieldName <> nil then
    result := (PInteger(FieldName)^ and $ffdfdf = ord('I') + ord('D') shl 8) or
      ((PIntegerArray(FieldName)^[0] and $dfdfdfdf =
       ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
       (PIntegerArray(FieldName)^[1] and $ffdf = ord('D')))
  else
    result := false;
end;
{$endif CPUX64}

function IsRowID(FieldName: PUtf8Char; FieldLen: integer): boolean;
begin
  if FieldLen = 2 then
    result := PWord(FieldName)^ and $dfdf = ord('I') + ord('D') shl 8
  else if FieldLen = 5 then
    result := (PInteger(FieldName)^ and $dfdfdfdf =
               ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
              (ord(FieldName[4]) and $df = ord('D'))
  else
    result := false;
end;

function IsRowIDShort(const FieldName: ShortString): boolean;
begin
  result := ((PIntegerArray(@FieldName)^[0] and $dfdfff =
              2 + ord('I') shl 8 + ord('D') shl 16) or
            ((PIntegerArray(@FieldName)^[0] and $dfdfdfff =
              5 + ord('R') shl 8 + ord('O') shl 16 + ord('W') shl 24) and
             (PIntegerArray(@FieldName)^[1] and $dfdf = ord('I') + ord('D') shl 8)));
end;

procedure VariantToSqlVar(const Input: variant; var temp: RawByteString;
  var Output: TSqlVar);
var
  wasString: boolean;
begin
  Output.Options := [];
  with TVarData(Input) do
    if VType = varVariantByRef then
      VariantToSqlVar(PVariant(VPointer)^, temp, Output)
    else
      case VType of
        varEmpty,
        varNull:
          Output.VType := ftNull;
        varByte:
          begin
            Output.VType := ftInt64;
            Output.VInt64 := VByte;
          end;
        varInteger:
          begin
            Output.VType := ftInt64;
            Output.VInt64 := VInteger;
          end;
        varLongWord:
          begin
            Output.VType := ftInt64;
            Output.VInt64 := VLongWord;
          end;
        varWord64,
        varInt64:
          begin
            Output.VType := ftInt64;
            Output.VInt64 := VInt64;
          end;
        varSingle:
          begin
            Output.VType := ftDouble;
            Output.VDouble := VSingle;
          end;
        varDouble:
          begin
            // varDate would be converted into ISO-8601 by VariantToUtf8()
            Output.VType := ftDouble;
            Output.VDouble := VDouble;
          end;
        varCurrency:
          begin
            Output.VType := ftCurrency;
            Output.VInt64 := VInt64;
          end;
        varString:
          begin
            // assume RawUtf8
            Output.VType := ftUtf8;
            Output.VText := VPointer;
          end;
      else
        // handle less current cases
        if VariantToInt64(Input, Output.VInt64) then
          Output.VType := ftInt64
        else
        begin
          VariantToUtf8(Input, RawUtf8(temp), wasString);
          if wasString then
          begin
            Output.VType := ftUtf8;
            Output.VText := pointer(temp);
          end
          else
            Output.VType := ftNull;
        end;
      end;
end;

procedure VariantToInlineValue(const V: Variant; var result: RawUtf8);
var
  tmp: RawUtf8;
  wasString: boolean;
begin
  VariantToUtf8(V, tmp, wasString);
  if wasString then
    QuotedStr(tmp, '"', result)
  else
    result := tmp;
end;

function VariantVTypeToSqlDBFieldType(VType: cardinal): TSqlDBFieldType;
begin
  case VType of
    varNull:
      result := ftNull;
    varShortInt,
    varWord,
    varLongWord,
    varSmallInt,
    varByte,
    varBoolean,
    varInteger,
    varInt64,
    varWord64:
      result := ftInt64;
    varSingle,
    varDouble:
      result := ftDouble;
    varDate:
      result := ftDate;
    varCurrency:
      result := ftCurrency;
    varString:
      result := ftUtf8;
  else
    result := ftUnknown; // includes varEmpty
  end;
end;

function VariantTypeToSqlDBFieldType(const V: Variant): TSqlDBFieldType;
var
  VD: TVarData absolute V;
  tmp: TVarData;
begin
  result := VariantVTypeToSqlDBFieldType(VD.VType);
  case result of
    ftUnknown:
      if VD.VType = varEmpty then
        result := ftUnknown
      else if SetVariantUnRefSimpleValue(V, tmp{%H-}) then
        result := VariantTypeToSqlDBFieldType(variant(tmp))
      else
        result := ftUtf8;
    ftUtf8:
      if (VD.VString <> nil) and
         (PCardinal(VD.VString)^ and $ffffff = JSON_BASE64_MAGIC_C) then
        result := ftBlob;
  end;
end;

function TextToSqlDBFieldType(json: PUtf8Char): TSqlDBFieldType;
begin
  if json = nil then
    result := ftNull
  else
    result := VariantVTypeToSqlDBFieldType(TextToVariantNumberType(json));
end;

function ToText(op: TSqlCompareOperator): PShortString;
begin
  result := GetEnumName(TypeInfo(TSqlCompareOperator), ord(op));
end;


{ ************ Nullable Values Stored as Variant }

function NullableVariantType(info: PRttiInfo): TNullableVariantType;
begin
  if info <> nil then
    if info <> TypeInfo(TNullableInteger) then
      if info <> TypeInfo(TNullableUtf8Text) then
        if info <> TypeInfo(TNullableBoolean) then
          if info <> TypeInfo(TNullableFloat) then
            if info <> TypeInfo(TNullableCurrency) then
              if info <> TypeInfo(TNullableDateTime) then
                if info <> TypeInfo(TNullableTimeLog) then
                  result := nvtNone
                else
                  result := nvtTimeLog
              else
                result := nvtDateTime
            else
              result := nvtCurrency
          else
            result := nvtFloat
        else
          result := nvtBoolean
      else
        result := nvtUtf8Text
    else
      result := nvtInteger
  else
    result := nvtNone;
end;


// TNullableInteger

function NullableInteger(const Value: Int64): TNullableInteger;
begin
  PVariant(@result)^ := Value;
end;

function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableIntegerToValue(const V: TNullableInteger; out Value: Int64): boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToInt64(PVariant(@V)^, Value);
end;

function NullableIntegerToValue(const V: TNullableInteger): Int64;
begin
  VariantToInt64(PVariant(@V)^, result);
end;

// TNullableBoolean

function NullableBoolean(Value: boolean): TNullableBoolean;
begin
  PVariant(@result)^ := Value;
end;

function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableBooleanToValue(const V: TNullableBoolean; out Value: boolean): boolean;
begin
  Value := false;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToBoolean(PVariant(@V)^, Value);
end;

function NullableBooleanToValue(const V: TNullableBoolean): boolean;
begin
  VariantToBoolean(PVariant(@V)^, result);
end;

// TNullableFloat

function NullableFloat(const Value: double): TNullableFloat;
begin
  PVariant(@result)^ := Value;
end;

function NullableFloatIsEmptyOrNull(const V: TNullableFloat): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableFloatToValue(const V: TNullableFloat; out Value: Double): boolean;
begin
  PInt64(@Value)^ := 0;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToDouble(PVariant(@V)^, Value);
end;

function NullableFloatToValue(const V: TNullableFloat): Double;
begin
  VariantToDouble(PVariant(@V)^, result);
end;

// TNullableCurrency

function NullableCurrency(const Value: currency): TNullableCurrency;
begin
  CurrencyToVariant(Value, PVariant(@result)^);
end;

function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableCurrencyToValue(const V: TNullableCurrency;
  out Value: currency): boolean;
begin
  PInt64(@Value)^ := 0;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToCurrency(PVariant(@V)^, Value);
end;

function NullableCurrencyToValue(const V: TNullableCurrency): currency;
begin
  VariantToCurrency(PVariant(@V)^, result);
end;

// TNullableDateTime

function NullableDateTime(const Value: TDateTime): TNullableDateTime;
begin
  PVariant(@result)^ := Value;
end;

function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableDateTimeToValue(const V: TNullableDateTime; out Value: TDateTime): boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToDouble(PVariant(@V)^, Double(Value));
end;

function NullableDateTimeToValue(const V: TNullableDateTime): TDateTime;
begin
  VariantToDouble(PVariant(@V)^, Double(result));
end;

// TNullableTimeLog

function NullableTimeLog(const Value: TTimeLog): TNullableTimeLog;
begin
  PVariant(@result)^ := Value;
end;

function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog; out Value: TTimeLog): boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToInt64(PVariant(@V)^, PInt64(@Value)^);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
begin
  VariantToInt64(PVariant(@V)^, PInt64(@result)^);
end;

// TNullableUtf8Text

function NullableUtf8Text(const Value: RawUtf8): TNullableUtf8Text;
begin
  ClearVariantForString(PVariant(@result)^);
  RawUtf8(TVarData(result).VAny) := Value;
end;

function NullableUtf8TextIsEmptyOrNull(const V: TNullableUtf8Text): boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableUtf8TextToValue(const V: TNullableUtf8Text; out Value: RawUtf8): boolean;
begin
  result := not VarDataIsEmptyOrNull(@V) and
            VariantToUtf8(PVariant(@V)^, Value);
end;

function NullableUtf8TextToValue(const V: TNullableUtf8Text): RawUtf8;
var
  dummy: boolean;
begin
  if VarDataIsEmptyOrNull(@V) then // VariantToUtf8() will return 'null'
    result := ''
  else
    VariantToUtf8(PVariant(@V)^, result, dummy);
end;


{ ************ Date/Time SQL encoding }

function DateToSql(Date: TDateTime): RawUtf8;
begin
  result := '';
  if Date <= 0 then
    exit;
  FastSetString(result, nil, 13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC_C;
  DateToIso8601PChar(Date, PUtf8Char(pointer(result)) + 3, True);
end;

function DateToSql(Year, Month, Day: cardinal): RawUtf8;
begin
  result := '';
  if (Year = 0) or
     (Month - 1 > 11) or
     (Day - 1 > 30) then
    exit;
  FastSetString(result, nil, 13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC_C;
  DateToIso8601PChar(PUtf8Char(pointer(result)) + 3, True, Year, Month, Day);
end;

var
  JSON_SQLDATE_MAGIC_TEXT: RawUtf8;

function DateTimeToSql(DT: TDateTime; WithMS: boolean): RawUtf8;
begin
  if DT <= 0 then
    result := ''
  else
  begin
    if frac(DT) = 0 then
      result := JSON_SQLDATE_MAGIC_TEXT + DateToIso8601(DT, true)
    else if trunc(DT) = 0 then
      result := JSON_SQLDATE_MAGIC_TEXT + TimeToIso8601(DT, true, 'T', WithMS)
    else
      result := JSON_SQLDATE_MAGIC_TEXT + DateTimeToIso8601(DT, true, 'T', WithMS);
  end;
end;

function TimeLogToSql(const Timestamp: TTimeLog): RawUtf8;
begin
  if Timestamp = 0 then
    result := ''
  else
    result := JSON_SQLDATE_MAGIC_TEXT + PTimeLogBits(@Timestamp)^.Text(true);
end;

function Iso8601ToSql(const S: RawByteString): RawUtf8;
begin
  if IsIso8601(pointer(S), length(S)) then
    result := JSON_SQLDATE_MAGIC_TEXT + S
  else
    result := '';
end;

function SqlToDateTime(const ParamValueWithMagic: RawUtf8): TDateTime;
begin
  result := Iso8601ToDateTimePUtf8Char(PUtf8Char(pointer(ParamValueWithMagic)) + 3,
    length(ParamValueWithMagic) - 3);
end;


{ ************ SQL Parameters Inlining and Processing }

function InlineParameter(ID: Int64): ShortString;
begin
  FormatShort(':(%):', [ID], result);
end;

function InlineParameter(const value: RawUtf8): RawUtf8;
begin
  QuotedStrJson(value, result, ':(', '):');
end;

const
  SELECT_STMT: array[0..6] of PAnsiChar = (
    'SELECT',
    'EXPLAIN ',
    'VACUUM',
    'PRAGMA',
    'WITH',
    'EXECUTE',
    nil);

function IsSelect(P: PUtf8Char; SelectClause: PRawUtf8): boolean;
var
  from: PUtf8Char;
begin
  P := SqlBegin(P);
  if P <> nil then
  begin
    case IdemPPChar(P, @SELECT_STMT) of
      0:
        // SELECT SelectClause^ FROM ...
        if (P[6] <= ' ') and
           (P[6] <> #0) then
        begin
          if SelectClause <> nil then
          begin
            inc(P, 7);
            from := StrPosI(' FROM ', P);
            if from = nil then
              SelectClause^ := ''
            else
              FastSetString(SelectClause^, P, from - P);
          end;
          result := true;
        end
        else
          result := false;
      1:
        // EXPLAIN ...
        result := true;
      2,
      3:
        // VACUUM or PRAGMA
        result := P[6] in [#0..' ', ';'];
      4:
        // WITH ... INSERT/UPDATE/DELETE
        result := (P[4] <= ' ') and
                  (StrPosI('INSERT', P + 5) = nil) and
                  (StrPosI('UPDATE', P + 5) = nil) and
                  (StrPosI('DELETE', P + 5) = nil);
      5:
        // FireBird specific EXECUTE BLOCK RETURNS
        begin
          P := GotoNextNotSpace(P + 7);
          result := IdemPChar(P, 'BLOCK') and
                    IdemPChar(GotoNextNotSpace(P + 5), 'RETURNS');
        end;
    else
      result := false;
    end;
  end
  else
    result := true; // assume '' statement is SELECT command
end;

function SqlBegin(P: PUtf8Char): PUtf8Char;
begin
  if P <> nil then
    repeat
      if P^ <= ' ' then
        // ignore blanks
        repeat
          if P^ = #0 then
            break
          else
            inc(P)
        until P^ > ' ';
      if PWord(P)^ = ord('-') + ord('-') shl 8 then
        // SQL comments
        repeat
          inc(P)
        until P^ in [#0, #10]
      else if PWord(P)^ = ord('/') + ord('*') shl 8 then
      begin
        // C comments
        inc(P);
        repeat
          inc(P);
          if PWord(P)^ = ord('*') + ord('/') shl 8 then
          begin
            inc(P, 2);
            break;
          end;
        until P^ = #0;
      end
      else
        break;
    until false;
  result := P;
end;

procedure SqlAddWhereAnd(var Where: RawUtf8; const Condition: RawUtf8);
begin
  if Where = '' then
    Where := Condition
  else
    Where := Where + ' and ' + Condition;
end;

const
  _ENDCLAUSE: array[0..9] of PUtf8Char = (
      'ORDER BY ',
      'GROUP BY ',
      'LIMIT ',
      'OFFSET ',
      'LEFT ',
      'RIGHT ',
      'INNER ',
      'OUTER ',
      'JOIN ',
      nil);

function SqlWhereIsEndClause(const Where: RawUtf8): boolean;
begin
  result := (Where <> '') and
            (IdemPPChar(GotoNextNotSpace(pointer(Where)), @_ENDCLAUSE) >= 0);
end;

function SqlFromWhere(const Where: RawUtf8): RawUtf8;
begin
  if Where = '' then
    result := ''
  else if SqlWhereIsEndClause(Where) then
    result := ' ' + Where
  else
    result := ' WHERE ' + Where;
end;

function SqlFromSelect(const TableName, Select, Where, SimpleFields: RawUtf8): RawUtf8;
begin
  if Select = '*' then
    // don't send BLOB values to query: retrieve simple = all non-blob fields
    result := SimpleFields
  else
    result := Select;
  result := 'SELECT ' + result + ' FROM ' + TableName + SqlFromWhere(Where);
end;

function SqlGetOrder(const Sql: RawUTF8): RawUTF8;
var
  P: PUtf8Char;
  i: integer;
begin
  i := PosI('ORDER BY ', Sql);
  if i > 0 then
  begin
    inc(i, 9);
    while Sql[i] in [#1 .. ' '] do
      inc(i); // trim left
    result := copy(Sql, i, maxInt);
    P := PosChar(pointer(result), ' ');
    if P = nil then
      P := PosChar(pointer(result), ';');
    if P <> nil then
      SetLength(result, P - pointer(result)); // trim right
  end;
  if result = '' then // by default, a SQLite3 query is ordered by ID
    result := ROWID_TXT;
end;

function SelectInClause(const PropName: RawUtf8; const Values: array of RawUtf8;
  const Suffix: RawUtf8; ValuesInlinedMax: integer): RawUtf8;
var
  n, i: integer;
  temp: TTextWriterStackBuffer;
begin
  n := length(Values);
  if n > 0 then
    with TJsonWriter.CreateOwnedStream(temp) do
    try
      AddString(PropName);
      if n = 1 then
      begin
        if ValuesInlinedMax > 1 then
          AddShorter('=:(')
        else
          Add('=');
        AddQuotedStr(pointer(Values[0]), length(Values[0]), '''');
        if ValuesInlinedMax > 1 then
          AddShorter('):');
      end
      else
      begin
        AddShorter(' in (');
        for i := 0 to n - 1 do
        begin
          if ValuesInlinedMax > n then
            Add(':', '(');
          AddQuotedStr(pointer(Values[i]), length(Values[i]), '''');
          if ValuesInlinedMax > n then
            AddShorter('):,')
          else
            AddComma;
        end;
        CancelLastComma;
        Add(')');
      end;
      AddString(Suffix);
      SetText(result);
    finally
      Free;
    end
  else
    result := '';
end;

function SelectInClause(const PropName: RawUtf8; const Values: array of TID;
  const Suffix: RawUtf8; ValuesInlinedMax: integer): RawUtf8;
var
  n, i: integer;
  temp: TTextWriterStackBuffer;
begin
  n := length(Values);
  if n > 0 then
    with TTextWriter.CreateOwnedStream(temp) do
    try
      AddString(PropName);
      if n = 1 then
      begin
        if ValuesInlinedMax > 1 then
          AddShorter('=:(')
        else
          Add('=');
        Add(Values[0]);
        if ValuesInlinedMax > 1 then
          AddShorter('):');
      end
      else
      begin
        AddShorter(' in (');
        for i := 0 to n - 1 do
        begin
          if ValuesInlinedMax > n then
            Add(':', '(');
          Add(Values[i]);
          if ValuesInlinedMax > n then
            AddShorter('):,')
          else
            AddComma;
        end;
        CancelLastComma;
        Add(')');
      end;
      AddString(Suffix);
      SetText(result);
    finally
      Free;
    end
  else
    result := '';
end;

function GetTableNameFromSqlSelect(const SQL: RawUtf8;
  EnsureUniqueTableInFrom: boolean): RawUtf8;
var
  i, j, k: PtrInt;
begin
  i := PosI(' FROM ', SQL);
  if i > 0 then
  begin
    inc(i, 6);
    while SQL[i] in [#1..' '] do
      inc(i);
    j := 0;
    while tcIdentifier in TEXT_CHARS[SQL[i + j]] do
      inc(j);
    if cardinal(j - 1) < 64 then
    begin
      k := i + j;
      while SQL[k] in [#1..' '] do
        inc(k);
      if not EnsureUniqueTableInFrom or
         (SQL[k] <> ',') then
      begin
        FastSetString(result, PAnsiChar(PtrInt(SQL) + i - 1), j);
        exit;
      end;
    end;
  end;
  result := '';
end;

function GetTableNamesFromSqlSelect(const SQL: RawUtf8): TRawUtf8DynArray;
var
  i, j, k, n: PtrInt;
begin
  result := nil;
  n := 0;
  i := PosI(' FROM ', SQL);
  if i > 0 then
  begin
    inc(i, 6);
    repeat
      while SQL[i] in [#1..' '] do
        inc(i);
      j := 0;
      while tcIdentifier in TEXT_CHARS[SQL[i + j]] do
        inc(j);
      if cardinal(j - 1) > 64 then
      begin
        result := nil;
        exit; // seems too big
      end;
      k := i + j;
      while SQL[k] in [#1..' '] do
        inc(k);
      SetLength(result, n + 1);
      FastSetString(result[n], PAnsiChar(PtrInt(SQL) + i - 1), j);
      inc(n);
      if SQL[k] <> ',' then
        break;
      i := k + 1;
    until false;
  end;
end;


{ TExtractInlineParameters }

procedure TExtractInlineParameters.Parse(const SQL: RawUtf8);
var
  ppBeg: integer;
  P, Gen: PUtf8Char;
begin
  Count := 0;
  ppBeg := PosEx(RawUtf8(':('), SQL, 1);
  if (ppBeg = 0) or
     (PosEx(RawUtf8('):'), SQL, ppBeg + 2) = 0) then
  begin
    // SQL code with no valid :(...): internal parameters -> leave Count=0
    GenericSQL := SQL;
    exit;
  end;
  // compute GenericSql from SQL, converting :(...): into ?
  FastSetString(GenericSQL, pointer(SQL), length(SQL)); // private copy
  P := pointer(GenericSQL); // in-place string unescape (keep SQL untouched)
  Gen := P + ppBeg - 1; // Gen^ just before :(
  inc(P, ppBeg + 1);    // P^ just after :(
  repeat
    if Count = high(Types) then
      raise ESynDBException.CreateUtf8('Too many parameters in %', [SQL]);
    Gen^ := '?'; // replace :(...): by ?
    inc(Gen);
    if length(Values) <= Count then
      SetLength(Values, Count + 16);
    P := ParseNext(P);
    if P = nil then
    begin
      Count := 0;
      GenericSQL := SQL;
      exit; // any invalid parameter -> try direct SQL
    end;
    while (P^ <> #0) and
          (PWord(P)^ <> Ord(':') + Ord('(') shl 8) do
    begin
      Gen^ := P^;
      inc(Gen);
      inc(P);
    end;
    if P^ = #0 then
      break;
    inc(P, 2);
    inc(Count);
  until false;
  // return generic SQL statement, with ? place-holders and params in Values[]
  FakeLength(GenericSQL, Gen);
  inc(Count);
end;

function TExtractInlineParameters.ParseNext(P: PUtf8Char): PUtf8Char;
var
  PBeg: PAnsiChar;
  L: integer;
  c: cardinal;
  spt: TSqlParamType;
begin
  result := nil; // indicates parsing error
  if P = nil then
    exit;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  case P^ of
    '''',
    '"':
      begin
        P := UnQuoteSqlStringVar(P, Values[Count]);
        if P = nil then
          // not a valid quoted string (e.g. unexpected end in middle of it)
          exit;
        spt := sptText;
        L := length(Values[Count]) - 3;
        if L > 0 then
        begin
          c := PInteger(Values[Count])^ and $00ffffff;
          if c = JSON_BASE64_MAGIC_C then
          begin
            // ':("\uFFF0base64encodedbinary"):' format -> decode
            Base64MagicDecode(Values[Count]); // in-place to avoid temp. string
            spt := sptBlob;
          end
          else if (c = JSON_SQLDATE_MAGIC_C) and
                  IsIso8601(PUtf8Char(pointer(Values[Count])) + 3, L) then
          begin
            // handle ':("\uFFF112012-05-04"):' format
            Delete(Values[Count], 1, 3);   // return only ISO-8601 text
            spt := sptDateTime;   // identified as Date/Time
          end;
        end;
      end;
    '-',
    '+',
    '0'..'9': // allow 0 or + in SQL
      begin
        // check if P^ is a true numerical value
        PBeg := pointer(P);
        spt := sptInteger;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']); // check digits
        if P^ = '.' then
        begin
          inc(P);
          if P^ in ['0'..'9'] then
          begin
            spt := sptFloat;
            repeat
              inc(P)
            until not (P^ in ['0'..'9']); // check fractional digits
          end
          else
            exit;
        end;
        if byte(P^) and $DF = ord('E') then
        begin
          spt := sptFloat;
          inc(P);
          if P^ = '+' then
            inc(P)
          else if P^ = '-' then
            inc(P);
          while P^ in ['0'..'9'] do
            inc(P);
        end;
        FastSetString(Values[Count], PBeg, P - PBeg);
      end;
    'n':
      if PInteger(P)^ = NULL_LOW then
      begin
        spt := sptNull;
        inc(P, 4);
      end
      else
        exit; // invalid content (only :(null): expected)
  else
    exit; // invalid content
  end;
  while (P^ <= ' ') and
        (P^ <> #0) do
    inc(P);
  if (P[0] <> ')') or
     (P[1] <> ':') then
    // we expect finishing with P^ pointing at '):'
    exit;
  // result<>nil only if value content was successfully decoded
  result := P + 2;
  Types[Count] := spt;
end;



{ ************ TResultsWriter Specialized for Database Export }

{ TResultsWriter }

const
  VOID_ARRAY: PAnsiChar = '[]'#10;
  VOID_FIELD: PAnsiChar = '{"FieldCount":0}';

procedure TResultsWriter.CancelAllVoid;
begin
  CancelAll; // rewind JSON
  if fExpand then
    // same as sqlite3_get_table()
    inc(fTotalFileSize, fStream.Write(VOID_ARRAY^, 3))
  else
    inc(fTotalFileSize, fStream.Write(VOID_FIELD^, 16));
end;

constructor TResultsWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TFieldBits; aBufSize: integer);
begin
  Create(aStream, Expand, withID, FieldBitsToIndex(Fields), aBufSize);
end;

constructor TResultsWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TFieldIndexDynArray; aBufSize: integer;
  aStackBuffer: PTextWriterStackBuffer);
begin
  if aStream = nil then
    if aStackBuffer <> nil then
      CreateOwnedStream(aStackBuffer^)
    else
      CreateOwnedStream(aBufSize)
  else if aStackBuffer <> nil then
    inherited Create(aStream, aStackBuffer, SizeOf(aStackBuffer^))
  else
    inherited Create(aStream, aBufSize);
  fExpand := Expand;
  fWithID := withID;
  fFields := Fields;
end;

procedure TResultsWriter.AddColumns(aKnownRowsCount: integer);
var
  i, len: PtrInt;
  c: PPointer;
  new: PAnsiChar;
begin
  if fExpand then
  begin
    c := pointer(ColNames);
    for i := 1 to length(ColNames) do
    begin
      len := PStrLen(PAnsiChar(c^) - _STRLEN)^; // ColNames[] <> ''
      if twoForceJsonExtended in CustomOptions then
      begin
        SetLength(RawUtf8(c^), len + 1); // reallocate in-place
        PAnsiChar(c^)[len] := ':';
      end
      else
      begin
        new := FastNewString(len + 3, CP_UTF8);
        new[0] := '"';
        MoveFast(c^^, new[1], len);
        PWord(new + len + 1)^ := ord('"') + ord(':') shl 8;
        FastAssignNew(c^, new);
      end;
      inc(c);
    end;
  end
  else
  begin
    AddShort('{"fieldCount":');
    AddU(length(ColNames));
    if aKnownRowsCount > 0 then
    begin
      AddShort(',"rowCount":');
      AddU(aKnownRowsCount);
    end;
    AddShort(',"values":["');
    // first row is FieldNames
    for i := 0 to length(ColNames) - 1 do
    begin
      AddString(ColNames[i]);
      AddShorter('","');
    end;
    CancelLastChar('"');
    fStartDataPosition := PtrInt(fStream.Position) + PtrInt(B - fTempBuf);
     // B := buf-1 at startup -> need ',val11' position in
     // "values":["col1","col2",val11,' i.e. current pos without the ','
  end;
end;

procedure TResultsWriter.AddColumn(aColName: PUtf8Char; aColIndex, aColCount: PtrInt);
var
  len: PtrInt;
  new: PAnsiChar;
begin
  len := StrLen(aColName);
  if fExpand then
  begin
    if aColIndex = 0 then // non-expanded mode doesn't use ColNames[]
      SetLength(ColNames, aColCount);
    if twoForceJsonExtended in CustomOptions then
    begin
      new := FastNewString(len + 1, CP_UTF8);
      MoveFast(aColName^, new^, len);
      new[len] := ':';
    end
    else
    begin
      new := FastNewString(len + 3, CP_UTF8);
      new[0] := '"';
      MoveFast(aColName^, new[1], len);
      PWord(new + len + 1)^ := ord('"') + ord(':') shl 8;
    end;
    FastAssignNew(ColNames[aColIndex], new);
  end
  else
  begin
    if aColIndex = 0 then
    begin
      AddShort('{"fieldCount":');
      Add(aColCount);
      AddShort(',"values":["');
      // first row is FieldNames in non-expanded format
    end;
    AddNoJsonEscape(aColName, len);
    if aColIndex = aColCount - 1 then
    begin
      // last AddColumn() call would finalize the non-expanded header
      Add('"' , ',');
      fStartDataPosition := PtrInt(fStream.Position) + PtrInt(B - fTempBuf);
    end
    else
      AddShorter('","');
  end;
end;

procedure TResultsWriter.ChangeExpandedFields(aWithID: boolean;
  const aFields: TFieldIndexDynArray);
begin
  if not Expand then
    raise ESynDBException.CreateUtf8(
      '%.ChangeExpandedFields() called with Expanded=false', [self]);
  fWithID := aWithID;
  fFields := aFields;
end;

procedure TResultsWriter.EndJsonObject(aKnownRowsCount, aRowsCount: integer;
  aFlushFinal: boolean);
begin
  CancelLastComma; // cancel last ','
  Add(']');
  if not fExpand then
  begin
    if aKnownRowsCount = 0 then
    begin
      AddShort(',"rowCount":');
      Add(aRowsCount);
    end;
    Add('}');
  end;
  Add(#10);
  if aFlushFinal then
    FlushFinal;
end;

procedure TResultsWriter.TrimFirstRow;
var
  P, PBegin, PEnd: PUtf8Char;
begin
  if (self = nil) or
     not fStream.InheritsFrom(TMemoryStream) or
     fExpand or
     (fStartDataPosition = 0) then
    exit;
  // go to begin of first row
  FlushToStream; // we need the data to be in fStream memory
  // PBegin^=val11 in { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
  PBegin := TMemoryStream(fStream).Memory;
  PEnd := PBegin + fStream.Position;
  PEnd^ := #0; // mark end of current values
  inc(PBegin, fStartDataPosition + 1); // +1 to include ',' of ',val11'
  // jump to end of first row
  P := GotoNextJsonItem(PBegin, length(ColNames));
  if P = nil then
    exit; // unexpected end
  // trim first row data
  if P^ <> #0 then
    MoveFast(P^, PBegin^, PEnd - P); // erase content
  fStream.Seek(PBegin - P, soCurrent){%H-}; // adjust current stream position
end;


{ ************ TSelectStatement SQL SELECT Parser }

{ TSelectStatement }

const
  NULL_UPP = ord('N') + ord('U') shl 8 + ord('L') shl 16 + ord('L') shl 24;
  ENDCLAUSE: array[0..4] of PAnsiChar = (
    'LIMIT',   // 0
    'OFFSET',  // 1
    'ORDER',   // 2
    'GROUP',   // 3
    nil);

constructor TSelectStatement.Create(const SQL: RawUtf8;
  const GetFieldIndex: TOnGetFieldIndex; const SimpleFields: TSelectStatementSelectDynArray);
var
  prop, whereBefore: RawUtf8;
  P, B: PUtf8Char;
  ndx, order, err, len, selectCount, whereCount: integer;
  whereWithOR, whereNotClause: boolean;

  function GetPropIndex: integer;
  begin
    if not GetNextFieldProp(P, prop) then
      result := -1
    else if IsRowID(pointer(prop)) then
      result := 0
    else
    begin
      // 0 = ID field
      result := GetFieldIndex(prop);
      if result >= 0 then // -1 = no valid field name
        inc(result);  // otherwise: PropertyIndex+1
    end;
  end;

  function GetNextSelectField: boolean;
  var
    select: TSelectStatementSelect;
    B: PUtf8Char;
  begin
    result := false;
    FillcharFast(select, SizeOf(select), 0);
    select.Field := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
    if select.Field < 0 then
    begin
      if P^ <> '(' then // Field not found -> try function(field)
        exit;
      P := GotoNextNotSpace(P + 1);
      select.FunctionName := prop;
      inc(fSelectFunctionCount);
      if IdemPropNameU(prop, 'COUNT') and
         (P^ = '*') then
      begin
        select.Field := 0; // count( * ) -> count(ID)
        select.FunctionKnown := funcCountStar;
        P := GotoNextNotSpace(P + 1);
      end
      else
      begin
        if IdemPropNameU(prop, 'DISTINCT') then
          select.FunctionKnown := funcDistinct
        else if IdemPropNameU(prop, 'MAX') then
          select.FunctionKnown := funcMax;
        select.Field := GetPropIndex;
        if select.Field < 0 then
          exit;
      end;
      if P^ <> ')' then
        exit;
      P := GotoNextNotSpace(P + 1);
    end
    else if P^ = '.' then
    begin
      // MongoDB-like field.subfield1.subfield2
      B := P;
      repeat
        inc(P);
      until not (jcJsonIdentifier in JSON_CHARS[P^]);
      FastSetString(select.SubField, B, P - B);
      fHasSelectSubFields := true;
    end;
    if P^ in ['+', '-'] then
    begin
      select.ToBeAdded := GetNextItemInteger(P, ' ');
      if select.ToBeAdded = 0 then
        exit;
      P := GotoNextNotSpace(P);
    end;
    if IdemPChar(P, 'AS ') then
    begin
      inc(P, 3);
      if not GetNextFieldProp(P, select.Alias) then
        exit;
    end;
    SetLength(fSelect, selectCount + 1);
    fSelect[selectCount] := select;
    inc(selectCount);
    result := true;
  end;

  function GetWhereValue(var Where: TSelectStatementWhere): boolean;
  var
    B: PUtf8Char;
  begin
    result := false;
    P := GotoNextNotSpace(P);
    Where.ValueSql := P;
    if PWord(P)^ = ord(':') + ord('(') shl 8 then
      inc(P, 2); // ignore :(...): parameter (no prepared statements here)
    if P^ in ['''', '"'] then
    begin
      // SQL String statement
      P := UnQuoteSqlStringVar(P, Where.Value);
      if P = nil then
        exit; // end of string before end quote -> incorrect
      RawUtf8ToVariant(Where.Value, Where.ValueVariant);
    end
    else if (PInteger(P)^ and $DFDFDFDF = NULL_UPP) and
            (P[4] in [#0..' ', ';']) then
    begin
      // NULL statement
      Where.Value := NULL_STR_VAR; // not void
      SetVariantNull(Where.ValueVariant);
      inc(P, 4);
    end
    else
    begin
      // numeric statement or 'true' or 'false' (OK for NormalizeValue)
      B := P;
      repeat
        inc(P);
      until P^ in [#0..' ', ';', ')', ','];
      FastSetString(Where.Value, B, P - B);
      VariantLoadJson(Where.ValueVariant, Where.Value);
      Where.ValueInteger := GetInteger(pointer(Where.Value), err);
    end;
    if PWord(P)^ = ord(')') + ord(':') shl 8 then
      inc(P, 2); // ignore :(...): parameter
    Where.ValueSqlLen := P - Where.ValueSql;
    P := GotoNextNotSpace(P);
    if (P^ = ')') and
       (Where.FunctionName = '') then
    begin
      B := P;
      repeat
        inc(P);
      until not (P^ in [#1..' ', ')']);
      while P[-1] = ' ' do
        dec(P); // trim right space
      FastSetString(Where.ParenthesisAfter, B, P - B);
      P := GotoNextNotSpace(P);
    end;
    result := true;
  end;

  function GetWhereValues(var Where: TSelectStatementWhere): boolean;
  var
    v: TSelectStatementWhereDynArray;
    n, w: integer;
    tmp: RawUtf8;
  begin
    result := false;
    if Where.ValueSqlLen <= 2 then
      exit;
    FastSetString(tmp, Where.ValueSql + 1, Where.ValueSqlLen - 2);
    P := pointer(tmp); // parse again the IN (...,...,... ) expression
    n := 0;
    try
      repeat
        if n = length({%H-}v) then
          SetLength(v, NextGrow(n));
        if not GetWhereValue(v[n]) then
          exit;
        inc(n);
        if P^ = #0 then
          break;
        if P^ <> ',' then
          exit;
        inc(P);
      until false;
    finally
      P := Where.ValueSql + Where.ValueSqlLen; // continue parsing as usual
    end;
    with TDocVariantData(Where.ValueVariant) do
    begin
      InitFast(n, dvArray);
      for w := 0 to n - 1 do
        AddItem(v[w].ValueVariant);
      Where.Value := ToJson;
    end;
    result := true;
  end;

  function GetWhereExpression(FieldIndex: integer;
    var Where: TSelectStatementWhere): boolean;
  var
    B: PUtf8Char;
  begin
    result := false;
    Where.ParenthesisBefore := whereBefore;
    Where.JoinedOR := whereWithOR;
    Where.NotClause := whereNotClause;
    Where.Field := FieldIndex; // 0 = ID, otherwise PropertyIndex+1
    if P^ = '.' then
    begin
      // MongoDB-like field.subfield1.subfield2
      B := P;
      repeat
        inc(P);
      until not (jcJsonIdentifier in JSON_CHARS[P^]);
      FastSetString(Where.SubField, B, P - B); // '.subfield1.subfield2'
      fWhereHasSubFields := true;
      P := GotoNextNotSpace(P);
    end;
    case P^ of
      '=':
        Where.Operation := opEqualTo;
      '>':
        if P[1] = '=' then
        begin
          inc(P);
          Where.Operation := opGreaterThanOrEqualTo;
        end
        else
          Where.Operation := opGreaterThan;
      '<':
        case P[1] of
          '=':
            begin
              inc(P);
              Where.Operation := opLessThanOrEqualTo;
            end;
          '>':
            begin
              inc(P);
              Where.Operation := opNotEqualTo;
            end;
        else
          Where.Operation := opLessThan;
        end;
      '!':
         if P[1] = '=' then
         begin
           inc(P);
           Where.Operation := opNotEqualTo;
         end
         else
           exit;
      'i',
      'I':
        case P[1] of
          's',
          'S':
            begin
              P := GotoNextNotSpace(P + 2);
              if IdemPChar(P, 'NULL') then
              begin
                Where.Value := NULL_STR_VAR;
                Where.Operation := opIsNull;
                Where.ValueSql := P;
                Where.ValueSqlLen := 4;
                TVarData(Where.ValueVariant).VType := varNull;
                inc(P, 4);
                result := true;
              end
              else if IdemPChar(P, 'NOT NULL') then
              begin
                Where.Value := 'not null';
                Where.Operation := opIsNotNull;
                Where.ValueSql := P;
                Where.ValueSqlLen := 8;
                TVarData(Where.ValueVariant).VType := varNull;
                inc(P, 8);
                result := true; // leave ValueVariant=unassigned
              end;
              exit;
            end;
          'n',
          'N':
            begin
              Where.Operation := opIn;
              P := GotoNextNotSpace(P + 2);
              if P^ <> '(' then
                exit; // incorrect SQL statement
              B := P; // get the IN() clause as JSON
              inc(P);
              while (P^ <> ')') or
                    (P[1] = ':') do // handle :(...): within the clause
                if P^ = #0 then
                  exit
                else
                  inc(P);
              inc(P);
              FastSetString(Where.Value, B, P - B);
              Where.ValueSql := B;
              Where.ValueSqlLen := P - B;
              result := GetWhereValues(Where);
              exit;
            end;
        end; // 'i','I':
      'l',
      'L':
        if IdemPChar(P + 1, 'IKE') then
        begin
          inc(P, 3);
          Where.Operation := opLike;
        end
        else
          exit;
    else
      exit; // unknown Operation
    end;
    // we got 'WHERE FieldName operator ' -> handle value
    inc(P);
    result := GetWhereValue(Where);
  end;

label
  lim, lim2;
begin
  P := pointer(SQL);
  if (P = nil) or
     (self = nil) then
    exit; // avoid GPF
  P := GotoNextNotSpace(P); // trim left
  if not IdemPChar(P, 'SELECT ') then
    exit
  else
    // handle only SELECT statement
    inc(P, 7);
  // 1. get SELECT clause: set bits in Fields from CSV field IDs in SQL
  selectCount := 0;
  P := GotoNextNotSpace(P); // trim left
  if P^ = #0 then
    exit; // no SQL statement
  if P^ = '*' then
  begin
    // all simple (not RawBlob/TOrmMany) fields
    inc(P);
    GetNextFieldProp(P, prop);
    if SimpleFields = nil then
      exit;
    fSelect := copy(SimpleFields); // use precalculated array of simple fields
  end
  else if not GetNextSelectField then
    // we need at least one field name
    exit
  else if P^ <> ',' then
    GetNextFieldProp(P, prop)
  else
    repeat
      while P^ in [',', #1..' '] do
        inc(P); // trim left
    until not GetNextSelectField; // add other CSV field names
  // 2. get FROM clause
  if not IdemPropNameU(prop, 'FROM') then
    exit; // incorrect SQL statement
  GetNextFieldProp(P, prop);
  fTableName := prop;
  // 3. get WHERE clause
  whereCount := 0;
  whereWithOR := false;
  whereNotClause := false;
  whereBefore := '';
  GetNextFieldProp(P, prop);
  if IdemPropNameU(prop, 'WHERE') then
  begin
    repeat
      B := P;
      if P^ = '(' then
      begin
        fWhereHasParenthesis := true;
        repeat
          inc(P);
        until not (P^ in [#1..' ', '(']);
        while P[-1] = ' ' do
          dec(P); // trim right space
        FastSetString(whereBefore, B, P - B);
        B := P;
      end;
      ndx := GetPropIndex;
      if ndx < 0 then
      begin
        if IdemPropNameU(prop, 'NOT') then
        begin
          whereNotClause := true;
          continue;
        end;
        if P^ = '(' then
        begin
          // where sum(...)
          inc(P);
          SetLength(fWhere, whereCount + 1);
          with fWhere[whereCount] do
          begin
            ParenthesisBefore := whereBefore;
            JoinedOR := whereWithOR;
            NotClause := whereNotClause;
            FunctionName := UpperCase(prop);
            // Byte/Word/Integer/cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
            len := length(prop);
            if (len > 16) and
               IdemPropName('DynArrayContains',
                 PUtf8Char(@PByteArray(prop)[len - 16]), 16) then
              Operation := opContains
            else
              Operation := opFunction;
            B := P;
            Field := GetPropIndex;
            if Field < 0 then
              P := B
            else if P^ <> ',' then
              break
            else
              P := GotoNextNotSpace(P + 1);
            if (P^ = ')') or
               (GetWhereValue(fWhere[whereCount]) and
                (P^ = ')')) then
            begin
              inc(P);
              break;
            end;
          end;
        end;
        P := B;
        break;
      end;
      SetLength(fWhere, whereCount + 1);
      if not GetWhereExpression(ndx, fWhere[whereCount]) then
        exit; // invalid SQL statement
      inc(whereCount);
      GetNextFieldProp(P, prop);
      if IdemPropNameU(prop, 'OR') then
        whereWithOR := true
      else if IdemPropNameU(prop, 'AND') then
        whereWithOR := false
      else
        goto lim2;
      whereNotClause := false;
      whereBefore := '';
    until false;
    // 4. get optional LIMIT/OFFSET/ORDER/GROUP end clause
lim:P := GotoNextNotSpace(P);
    while (P <> nil) and
          not (P^ in [#0, ';']) do
    begin
      GetNextFieldProp(P, prop);
lim2: case IdemPPChar(pointer(prop), @ENDCLAUSE) of
        0:
          // LIMIT
          fLimit := GetNextItemCardinal(P, ' ');
        1:
          // OFFSET
          fOffset := GetNextItemCardinal(P, ' ');
        2:
          begin
            // ORDER BY
            GetNextFieldProp(P, prop);
            if IdemPropNameU(prop, 'BY') or
               (fOrderByField <> nil) then
            begin
              repeat
                ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
                if ndx < 0 then
                  exit; // incorrect SQL statement
                order := AddFieldIndex(fOrderByField, ndx);
                if P^ <> ',' then
                begin
                  // check ORDER BY ... ASC/DESC
                  if GetNextFieldProp(P, prop) then
                    if IdemPropNameU(prop, 'DESC') then
                      include(fOrderByFieldDesc, order)
                    else if not IdemPropNameU(prop, 'ASC') then
                      goto lim2; // parse LIMIT OFFSET clauses after ORDER
                  if P^ <> ',' then
                    break; // no more fields in this ORDER BY clause
                end;
                P := GotoNextNotSpace(P + 1);
              until P^ in [#0, ';'];
            end
            else
              exit; // incorrect SQL statement
          end;
        3:
          begin
            // GROUP BY
            GetNextFieldProp(P, prop);
            if IdemPropNameU(prop, 'BY') then
            begin
              repeat
                ndx := GetPropIndex; // 0 = ID, otherwise PropertyIndex+1
                if ndx < 0 then
                  exit; // incorrect SQL statement
                AddFieldIndex(fGroupByField, ndx);
                if P^ <> ',' then
                  break;
                P := GotoNextNotSpace(P + 1);
              until P^ in [#0, ';'];
            end
            else
              exit; // incorrect SQL statement
          end;
      else if (prop <> '') or
              not (GotoNextNotSpace(P)^ in [#0, ';']) then
        exit // incorrect SQL statement
      else
        break; // reached the end of the statement
    end;
    end;
  end
  else if prop <> '' then
    goto lim2; // handle LIMIT OFFSET ORDER
  fSqlStatement := SQL; // make a private copy e.g. for Where[].ValueSql
end;

procedure TSelectStatement.SelectFieldBits(var Fields: TFieldBits;
  var withID: boolean; SubFields: PRawUtf8Array);
var
  i: integer;
  f: ^TSelectStatementSelect;
begin
  FillZero(Fields);
  withID := false;
  f := pointer(fSelect);
  for i := 1 to Length(fSelect) do
  begin
    if f^.Field = 0 then
      withID := true
    else
      include(Fields, f^.Field - 1);
    if (SubFields <> nil) and
       fHasSelectSubFields then
      SubFields^[f^.Field] := f^.SubField;
    inc(f);
  end;
end;

function ToText(Op: TSelectStatementOperator): PShortString;
begin
  result := GetEnumName(TypeInfo(TSelectStatementOperator), ord(Op));
end;


{ ************ TID Processing Functions }

function CastID(Value: TID): pointer;
begin
  result := pointer(PtrUInt(PtrInt(Value)));
end;

procedure AddID(var Values: TIDDynArray; var ValuesCount: integer; Value: TID);
begin
  if ValuesCount = length(Values) then
    SetLength(Values, NextGrow(ValuesCount));
  Values[ValuesCount] := Value;
  inc(ValuesCount);
end;

procedure AddID(var Values: TIDDynArray; Value: TID);
var
  n: PtrInt;
begin
  n := length(Values);
  SetLength(Values, n + 1);
  Values[n] := Value;
end;

procedure SetID(P: PUtf8Char; var result: TID);
begin
{$ifdef CPU64} // PtrInt is already int64 -> call PtrInt version
  result := GetInteger(P);
{$else}
  {$ifdef VER3_0} // FPC issue workaround
  SetInt64(P, result);
  {$else}
  SetInt64(P, PInt64(@result)^);
  {$endif}
{$endif CPU64}
end;

procedure SetID(const U: RawByteString; var result: TID);
begin
{$ifdef CPU64} // PtrInt is already int64 -> call PtrInt version
  result := GetInteger(pointer(U));
{$else}
  SetID(pointer(U), result);
{$endif CPU64}
end;


{ ************ JSON Object Decoder and SQL Generation }

{ TJsonObjectDecoder }

const
  ENDOFJSONFIELD = [',', ']', '}', ':'];

procedure GetJsonArrayOrObject(P: PUtf8Char; out PDest: PUtf8Char;
  EndOfObject: PUtf8Char; var result: RawUtf8);
var
  Beg: PUtf8Char;
begin
  PDest := nil;
  Beg := P;
  P := GotoEndJsonItem(P); // quick go to end of array of object
  if P = nil then
  begin
    result := '';
    exit;
  end;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  PDest := P + 1;
  FastSetString(result, Beg, P - Beg);
end;

procedure GetJsonArrayOrObjectAsQuotedStr(P: PUtf8Char; out PDest: PUtf8Char;
  EndOfObject: PUtf8Char; var result: RawUtf8);
var
  Beg: PUtf8Char;
begin
  PDest := nil;
  Beg := P;
  P := GotoEndJsonItem(P); // quick go to end of array of object
  if P = nil then
  begin
    result := '';
    exit;
  end;
  if EndOfObject <> nil then
    EndOfObject^ := P^;
  PDest := P + 1;
  QuotedStr(Beg, P - Beg, '''', result);
end;

procedure ParseSqlValue(var info: TGetJsonField; Params: TJsonObjectDecoderParams;
  var FieldType: TJsonObjectDecoderFieldType;  var FieldValue: RawUtf8);
var
  c: integer;
  P: PUtf8Char;
begin
  P := info.Json;
  if P = nil then
  begin
    FieldType := ftaNull;
    FieldValue := NULL_STR_VAR;
    exit;
  end;
  while P^ in [#1..' '] do
    inc(P);
  if (PInteger(P)^ = NULL_LOW) and
     (P[4] in [#0, #9, #10, #13, ' ', ',', '}', ']']) then
  begin
    /// GetJsonField('null') returns '' -> check here to make a diff with '""'
    FieldType := ftaNull;
    FieldValue := NULL_STR_VAR;
    inc(P, 4);
    while P^ in [#1..' '] do
      inc(P);
    if P^ = #0 then
      info.Json := nil
    else
    begin
      info.EndOfObject := P^;
      P^ := #0;
      info.Json := P + 1;
    end;
  end
  else if P^ in ['{', '['] then
  begin
    // handle nested object or array e.g. for custom variant types
    info.Json := P;
    info.GetJsonFieldOrObjectOrArray(true, false);
    if P^ = '{' then
      FieldType := ftaObject
    else
      FieldType := ftaArray;
    if (info.ValueLen = 0) or
       (Params = pNonQuoted) then
        FastSetString(FieldValue, info.Value, info.ValueLen)
      else
        QuotedStr(info.Value, info.ValueLen, '''', FieldValue);
  end
  else
  begin
    // handle JSON string, number or false/true in P
    info.Json := P;
    info.GetJsonField;
    if info.WasString then
    begin
      c := PInteger(info.Value)^ and $00ffffff;
      if c = JSON_BASE64_MAGIC_C then
      begin
        FieldType := ftaBlob;
        case Params of
          pInlined:
            // untouched -> recognized as BLOB by TExtractInlineParameters
            QuotedStr(info.Value, info.ValueLen, '''', FieldValue);
        else
          // returned directly as RawByteString (e.g. for INSERT/UPDATE)
          Base64ToBin(PAnsiChar(info.Value) + 3, info.ValueLen - 3,
            RawByteString(FieldValue));
        end;
      end
      else
      begin
        if c = JSON_SQLDATE_MAGIC_C then
        begin
          FieldType := ftaDate;
          inc(info.Value, 3); // ignore \uFFF1 magic marker
        end
        else
          FieldType := ftaString;
        // regular string content
        if Params = pNonQuoted then
          // returned directly as RawUtf8
          FastSetString(FieldValue, info.Value, info.ValueLen)
        else
          // single-quote SQL strings as in the official SQLite3 documentation
          QuotedStr(info.Value, info.ValueLen, '''', FieldValue);
      end;
    end
    else if info.Value = nil then
    begin
      FieldType := ftaNull;
      FieldValue := NULL_STR_VAR;
    end
    else // avoid GPF, but will return invalid SQL
    // non string params (numeric or false/true) are passed untouched
    if PInteger(info.Value)^ = FALSE_LOW then
    begin
      FieldType := ftaBoolean;
      FieldValue := SmallUInt32Utf8[0];
    end
    else if PInteger(info.Value)^ = TRUE_LOW then
    begin
      FieldType := ftaBoolean;
      FieldValue := SmallUInt32Utf8[1];
    end
    else
    begin
      FieldType := ftaNumber;
      FastSetString(FieldValue, info.Value, info.ValueLen);
    end;
  end;
end;

procedure TJsonObjectDecoder.DecodeInPlace(var P: PUtf8Char;
  const Fields: TRawUtf8DynArray; Params: TJsonObjectDecoderParams;
  const RowID: TID; ReplaceRowIDWithID: boolean);
var
  info: TGetJsonField;
  F: PtrInt;
  FieldIsRowID: boolean;
begin
  FieldCount := 0;
  DecodedRowID := 0;
  DecodedFieldTypesToUnnest := nil;
  InlinedParams := Params;
  info.Json := P;
  if pointer(Fields) = nil then
  begin
    // get "COL1":"VAL1" pairs, stopping at '}' or ']'
    DecodedFieldNames := @FieldNames;
    if RowID > 0 then
    begin
      // insert explicit RowID as first parameter
      if ReplaceRowIDWithID then
      begin
        FieldNames[0] := pointer(ID_TXT);
        FieldNamesL[0] := 2;
      end
      else
      begin
        FieldNames[0] := pointer(ROWID_TXT);
        FieldNamesL[0] := 5;
      end;
      Int64ToUtf8(RowID, FieldValues[0]);
      FieldTypeApproximation[0] := ftaNumber;
      FieldCount := 1;
      DecodedRowID := RowID;
    end;
    // parse all JSON fields and value, converting them to SQL values
    repeat
      if info.Json = nil then
        break;
      info.Value := GetJsonPropName(info.Json, @info.Valuelen);
      if (info.Value = nil) or
         (info.Json = nil) then
        break; // invalid JSON field name
      FieldIsRowID := IsRowId(info.Value, info.ValueLen);
      if FieldIsRowID then
        if RowID > 0 then
        begin
          info.GetJsonField; // ignore this if explicit RowID was supplied
          if info.EndOfObject in [#0, '}', ']'] then
            break
          else
            continue;
        end
        else if ReplaceRowIDWithID then
        begin
          info.Value := pointer(ID_TXT);
          info.Valuelen := 2;
        end;
      F := FieldCount;
      if F = MAX_SQLFIELDS then
        raise EJsonObjectDecoder.Create('Too many inlines in TJsonObjectDecoder');
      FieldNames[F] := info.Value;
      FieldNamesL[F] := info.Valuelen;
      ParseSqlValue(info, Params, FieldTypeApproximation[F], FieldValues[F]);
      if FieldIsRowID then
        SetID(FieldValues[F], DecodedRowID);
      inc(FieldCount);
    until info.EndOfObject in [#0, '}', ']'];
  end
  else
  begin
    // get "VAL1","VAL2"...
    if info.Json = nil then
      exit;
    if RowID > 0 then
      raise EJsonObjectDecoder.Create('TJsonObjectDecoder(expanded) won''t handle RowID');
    if length(Fields) > MAX_SQLFIELDS then
      raise EJsonObjectDecoder.Create('Too many inlines in TJsonObjectDecoder');
    DecodedFieldNames := pointer(Fields);
    FieldCount := length(Fields);
    for F := 0 to FieldCount - 1 do
      ParseSqlValue(info, Params, FieldTypeApproximation[F], FieldValues[F]);
  end;
  P := info.Json;
end;

procedure TJsonObjectDecoder.Decode(var Json: TSynTempBuffer;
  const Fields: TRawUtf8DynArray; Params: TJsonObjectDecoderParams;
  const RowID: TID; ReplaceRowIDWithID: boolean);
var
  P: PUtf8Char;
begin
  P := Json.buf;
  if P <> nil then
    while P^ in [#1..' ', '{', '['] do
      inc(P);
  DecodeInPlace(P, Fields, Params, RowID, ReplaceRowIDWithID);
end;

function TJsonObjectDecoder.SameFieldNames(const Fields: TRawUtf8DynArray): boolean;
var
  i: PtrInt;
begin
  result := false;
  if length(Fields) <> FieldCount then
    exit;
  for i := 0 to FieldCount - 1 do
    if not IdemPropNameU(Fields[i], FieldNames[i], FieldNamesL[i]) then
      exit;
  result := true;
end;

procedure TJsonObjectDecoder.AssignFieldNamesTo(var Fields: TRawUtf8DynArray);
var
  i: PtrInt;
begin
  Fields := nil;
  SetLength(Fields, FieldCount);
  for i := 0 to FieldCount - 1 do
    FastSetString(Fields[i], FieldNames[i], FieldNamesL[i]);
end;

function TJsonObjectDecoder.EncodeAsSql(const Prefix1, Prefix2: RawUtf8;
  Update: boolean; Prefix1Batch: PRestBatchOptions; DB: TSqlDBDefinition): RawUtf8;
var
  f: PtrInt;
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;

  procedure AddValue;
  begin
    if InlinedParams = pInlined then
      W.AddShorter(':(');
    W.AddString(FieldValues[f]);
    if InlinedParams = pInlined then
      W.AddShorter('):,')
    else
      W.AddComma;
  end;

begin
  result := '';
  if FieldCount = 0 then
    exit;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    if Prefix1Batch <> nil then
      EncodeInsertPrefix(W, Prefix1Batch^, DB)
    else
      W.AddString(Prefix1);
    W.AddString(Prefix2);
    if Update then
    begin
      for f := 0 to FieldCount - 1 do
        // append 'COL1=...,COL2=...'
        if not IsRowID(DecodedFieldNames^[f]) then
        begin
          W.AddNoJsonEscape(DecodedFieldNames^[f]);
          W.Add('=');
          AddValue;
        end;
      W.CancelLastComma;
    end
    else
    begin
      // returns ' (COL1,COL2) values ('VAL1',VAL2)'
      W.Add(' ', '(');
      for f := 0 to FieldCount - 1 do
      begin
        // append 'COL1,COL2'
        W.AddNoJsonEscape(DecodedFieldNames^[f]);
        W.AddComma;
      end;
      W.CancelLastComma;
      W.AddShort(') values (');
      for f := 0 to FieldCount - 1 do
        AddValue;
      W.CancelLastComma;
      W.Add(')');
    end;
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TJsonObjectDecoder.EncodeAsJson(out result: RawUtf8);
var
  f: PtrInt;
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  if FieldCount = 0 then
    exit;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    W.Add('{');
    for f := 0 to FieldCount - 1 do
    begin
      W.AddProp(DecodedFieldNames^[f]);
      if FieldTypeApproximation[f] in [ftaBlob, ftaDate, ftaString] then
        if InlinedParams = pNonQuoted then
          W.AddJsonString(FieldValues[f])
        else
          W.AddQuotedStringAsJson(FieldValues[f])
      else
        W.AddString(FieldValues[f]);
      W.AddComma;
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TJsonObjectDecoder.FindFieldName(const FieldName: RawUtf8): PtrInt;
begin
  for result := 0 to FieldCount - 1 do
    if IdemPropNameU(FieldName, FieldNames[result], FieldNamesL[result]) then
      exit;
  result := -1;
end;

function TJsonObjectDecoder.GetFieldNames: RawUtf8;
var
  i: PtrInt;
  tmp: TTextWriterStackBuffer;
begin
  if FieldCount = 0 then
    result := ''
  else
  with TTextWriter.CreateOwnedStream(tmp) do
    try
      for i := 0 to FieldCount - 1 do
      begin
        AddNoJsonEscape(FieldNames[i], FieldNamesL[i]);
        AddComma;
      end;
      CancelLastComma;
      SetText(result);
    finally
      Free;
    end;
end;

procedure TJsonObjectDecoder.AddFieldValue(const FieldName, FieldValue: RawUtf8;
  FieldType: TJsonObjectDecoderFieldType);
begin
  if FieldCount = MAX_SQLFIELDS then
    raise EJsonObjectDecoder.CreateUtf8(
      'Too many fields for TJsonObjectDecoder.AddField(%) max=%',
      [FieldName, MAX_SQLFIELDS]);
  FieldNames[FieldCount] := pointer(FieldName); // so FieldName should remain available
  FieldNamesL[FieldCount] := length(FieldName);
  FieldValues[FieldCount] := FieldValue;
  FieldTypeApproximation[FieldCount] := FieldType;
  inc(FieldCount);
end;

const
  FROMINLINED: array[boolean] of TJsonObjectDecoderParams = (pQuoted, pInlined);

function GetJsonObjectAsSql(var P: PUtf8Char; const Fields: TRawUtf8DynArray;
  Update, InlinedParams: boolean; RowID: TID; ReplaceRowIDWithID: boolean): RawUtf8;
var
  Decoder: TJsonObjectDecoder;
begin
  Decoder.DecodeInPlace(P, Fields, FROMINLINED[InlinedParams], RowID, ReplaceRowIDWithID);
  result := Decoder.EncodeAsSql('', '', Update, nil, dUnknown);
end;

function GetJsonObjectAsSql(const Json: RawUtf8; Update, InlinedParams: boolean;
  RowID: TID; ReplaceRowIDWithID: boolean): RawUtf8;
var
  Decoder: TJsonObjectDecoder;
  tmp: TSynTempBuffer;
begin
  tmp.Init(Json);
  try
    Decoder.Decode(tmp, nil, FROMINLINED[InlinedParams], RowID, ReplaceRowIDWithID);
    result := Decoder.EncodeAsSql('', '', Update, nil, dUnknown);
  finally
    tmp.Done;
  end;
end;

function UnJsonFirstField(var P: PUtf8Char): RawUtf8;
var
  info: TGetJsonField;
begin
  result := '';
  if P = nil then
    exit;
  if Expect(P, FIELDCOUNT_PATTERN, 14) then
  begin
    // expand=false: { "fieldCount":1,"values":["col1",val11] } -> vall11
    if GetNextItemCardinal(P) <> 1 then
      exit; // wrong field count
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P); // go to ["col1"
    inc(P); // go to "col1"
  end
  else
  begin
    // expand=true: [ {"col1":val11} ] -> val11
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P); // need an array of objects
    repeat
      inc(P);
      if P^ = #0 then
        exit;
    until P^ = '{'; // go to object begining
  end;
  info.Json := P;
  if GetJsonPropName(info.Json) <> nil then // ignore field name
    info.GetJsonValue(result); // get field value
  P := info.Json;
end;

function IsNotAjaxJson(P: PUtf8Char): boolean;
begin
  result := Expect(P, FIELDCOUNT_PATTERN, 14);
end;

function StartWithQuotedID(P: PUtf8Char; out ID: TID): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if PCardinal(P)^ and $ffffdfdf =
       ord('I') + ord('D') shl 8 + ord('"') shl 16 + ord(':') shl 24 then
  begin
    SetID(P + 4, ID{%H-});
    result := ID > 0;
    exit;
  end
  else if (PCardinalArray(P)^[0] and $dfdfdfdf =
           ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
         (PCardinalArray(P)^[1] and $ffffdf =
           ord('D') + ord('"') shl 8 + ord(':') shl 16) then
  begin
    SetID(P + 7, ID);
    result := ID > 0;
    exit;
  end;
  ID := 0;
  result := false;
end;

function StartWithID(P: PUtf8Char; out ID: TID): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if PCardinal(P)^ and $ffdfdf =
       ord('I') + ord('D') shl 8 + ord(':') shl 16 then
  begin
    SetID(P + 3, ID{%H-});
    result := ID > 0;
    exit;
  end
  else if (PCardinalArray(P)^[0] and $dfdfdfdf =
           ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
          (PCardinalArray(P)^[1] and $ffdf =
           ord('D') + ord(':') shl 8) then
  begin
    SetID(P + 6, ID);
    result := ID > 0;
    exit;
  end;
  ID := 0;
  result := false;
end;

function JsonGetID(P: PUtf8Char; out ID: TID): boolean;
begin
  if (P <> nil) and
     NextNotSpaceCharIs(P, '{') then
    if NextNotSpaceCharIs(P, '"') then
      result := StartWithQuotedID(P, ID{%H-})
    else
      result := StartWithID(P, ID)
  else
  begin
    ID := 0;
    result := false;
  end;
end;

procedure JsonGetObject(var P: PUtf8Char; ExtractID: PID;
  var EndOfObject: AnsiChar; KeepIDField: boolean; out JsonObject: RawUtf8);
var
  Beg, PC: PUtf8Char;
begin
  Beg := P;
  if Beg = nil then
    exit;
  while (Beg^ <= ' ') and
        (Beg^ <> #0) do
    inc(Beg);
  if Beg^ <> '{' then
    exit;
  P := GotoEndJsonItem(Beg);
  if (P = nil) or
     not (P^ in ENDOFJSONFIELD) then
    exit;
  EndOfObject := P^;
  inc(P); // ignore end of object, i.e. ',' or ']'
  if ExtractID <> nil then
  begin
    PC := Beg;
    repeat
      inc(PC);
    until PC^ > ' ';
    if ((PC^ = '"') and
        StartWithQuotedID(PC + 1, ExtractID^)) or
       StartWithID(PC, ExtractID^) then
      if not KeepIDField then
      begin
        PC := PosChar(PC + 3, ','); // ignore the '"ID":203,' pair
        if PC = nil then
          exit;
        PC^ := '{';
        Beg := PC;
      end;
  end;
  FastSetString(JsonObject, Beg, P - Beg - 1);
end;

procedure EncodeInsertPrefix(W: TTextWriter; BatchOptions: TRestBatchOptions;
  DB: TSqlDBDefinition);
begin
  if boInsertOrIgnore in BatchOptions then
    case DB of
      dMySQL:
        W.AddShort('insert ignore into ')
    else
      W.AddShort('insert or ignore into '); // SQlite3
    end
  else if boInsertOrReplace in BatchOptions then
    case DB of
      dFirebird:
        W.AddShort('update or insert into ');
    else
      W.AddShort('replace into '); // SQlite3 and MySQL
    end
  else
    W.AddShort('insert into ');
end;



initialization
  ShortStringToAnsi7String(JSON_SQLDATE_MAGIC_STR, JSON_SQLDATE_MAGIC_TEXT);
  ID_TXT := 'ID'; // avoid reallocation
  ROWID_TXT := 'RowID';

end.

