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
    - TJsonWriter Specialized for Database Export
    - TSelectStatement SQL SELECT Parser

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
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
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
  out Index: TFieldIndexDynArray;
  MaxLength: integer = MAX_SQLFIELDS; IndexStart: integer = 0); overload;

/// convert a TFieldBits set of bits into an array of integers
function FieldBitsToIndex(const Fields: TFieldBits;
  MaxLength: integer = MAX_SQLFIELDS): TFieldIndexDynArray; overload;
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
function IsRowIDShort(const FieldName: shortstring): boolean;
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

/// guess the correct TSqlDBFieldType from a variant type
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
  ID_TXT: RawUtf8;
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
function InlineParameter(ID: Int64): shortstring; overload;

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
function SelectInClause(const PropName: RawUtf8; const Values: array of Int64;
  const Suffix: RawUtf8 = ''; ValuesInlinedMax: integer = 0): RawUtf8; overload;

/// naive search of '... FROM TableName ...' pattern in the supplied SQL
function GetTableNameFromSqlSelect(const SQL: RawUtf8;
  EnsureUniqueTableInFrom: boolean): RawUtf8;

/// naive search of '... FROM Table1,Table2 ...' pattern in the supplied SQL
function GetTableNamesFromSqlSelect(const SQL: RawUtf8): TRawUtf8DynArray;



{ ************ TJsonWriter Specialized for Database Export }

type
  /// simple writer to a Stream, specialized for the JSON format and SQL export
  // - i.e. define some property/method helpers to export SQL resultset as JSON
  TJsonWriter = class(TTextWriter)
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
    // - on non-expanded format, will directly write aColName to W
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
    /// any '(' before the actual expression
    ParenthesisBefore: RawUtf8;
    /// any ')' after the actual expression
    ParenthesisAfter: RawUtf8;
    /// expressions are evaluated as AND unless this field is set to TRUE
    JoinedOR: boolean;
    /// if this expression is preceded by a NOT modifier
    NotClause: boolean;
    /// the index of the field used for the WHERE expression
    // - WhereField=0 for ID, 1 for field # 0, 2 for field #1,
    // and so on... (i.e. WhereField = RTTI field index +1)
    Field: integer;
    /// MongoDB-like sub field e.g. 'mainfield.subfield1.subfield2'
    // - still identifying 'mainfield' in Field index, and setting
    // SubField='.subfield1.subfield2'
    SubField: RawUtf8;
    /// the operator of the WHERE expression
    Operation: TSelectStatementOperator;
    /// the SQL function name associated to a Field and Value
    // - e.g. 'INTEGERDYNARRAYCONTAINS' and Field=0 for
    // IntegerDynArrayContains(RowID,10) and ValueInteger=10
    // - Value does not contain anything
    FunctionName: RawUtf8;
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
    fWriter: TJsonWriter;
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
    property Writer: TJsonWriter
      read fWriter write fWriter;
  end;

function ToText(Op: TSelectStatementOperator): PShortString; overload;

// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSynTableStatement = TSelectStatement;

{$endif PUREMORMOT2}


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
  out Index: TFieldIndexDynArray; MaxLength, IndexStart: integer);
var
  i, n: PtrInt;
  sets: array[0..MAX_SQLFIELDS - 1] of TFieldIndex; // to avoid memory reallocation
begin
  n := 0;
  if MaxLength > MAX_SQLFIELDS then
    raise ESynDBException.CreateUtf8('FieldBitsToIndex(MaxLength=%)', [MaxLength]);
  for i := 0 to MaxLength - 1 do
    if byte(i) in Fields then
    begin
      sets[n] := i;
      inc(n);
    end;
  SetLength(Index, IndexStart + n);
  for i := 0 to n - 1 do
    Index[IndexStart + i] := {%H-}sets[i];
end;

function FieldBitsToIndex(const Fields: TFieldBits;
  MaxLength: integer): TFieldIndexDynArray;
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

{$ifdef CPU64}
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
{$endif CPU64}

function IsRowID(FieldName: PUtf8Char; FieldLen: integer): boolean;
begin
  case FieldLen of
    2:
      result := PWord(FieldName)^ and $dfdf = ord('I') + ord('D') shl 8;
    5:
      result := (PInteger(FieldName)^ and $dfdfdfdf =
                 ord('R') + ord('O') shl 8 + ord('W') shl 16 + ord('I') shl 24) and
                (ord(FieldName[4]) and $df = ord('D'));
  else
    result := false;
  end;
end;

function IsRowIDShort(const FieldName: shortstring): boolean;
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
  result := not VarDataIsEmptyOrNull(@V) and VariantToInt64(PVariant(@V)^, Value);
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
  result := not VarDataIsEmptyOrNull(@V) and VariantToBoolean(PVariant(@V)^, Value);
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
  result := not VarDataIsEmptyOrNull(@V) and VariantToDouble(PVariant(@V)^, Value);
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
  result := not VarDataIsEmptyOrNull(@V) and VariantToCurrency(PVariant(@V)^, Value);
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
  result := not VarDataIsEmptyOrNull(@V) and VariantToUtf8(PVariant(@V)^, Value);
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

function InlineParameter(ID: Int64): shortstring;
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

function SqlWhereIsEndClause(const Where: RawUtf8): boolean;
begin
  if Where = '' then
    result := false
  else
    result := IdemPCharArray(GotoNextNotSpace(pointer(Where)), [
      'ORDER BY ',
      'GROUP BY ',
      'LIMIT ',
      'OFFSET ',
      'LEFT ',
      'RIGHT ',
      'INNER ',
      'OUTER ',
      'JOIN ']) >= 0;
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

function SelectInClause(const PropName: RawUtf8; const Values: array of RawUtf8;
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

function SelectInClause(const PropName: RawUtf8; const Values: array of Int64;
  const Suffix: RawUtf8; ValuesInlinedMax: integer): RawUtf8;
var
  n, i: integer;
  temp: TTextWriterStackBuffer;
begin
  n := length(Values);
  if n > 0 then
    with TBaseWriter.CreateOwnedStream(temp) do
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
  Gen^ := #0; // as SetLength(), but with no memory realloc
  PStrLen(PAnsiChar(pointer(GenericSQL)) - _STRLEN)^ := Gen - pointer(GenericSQL);
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



{ ************ TJsonWriter Specialized for Database Export }

{ TJsonWriter }

procedure TJsonWriter.CancelAllVoid;
const
  VOIDARRAY: PAnsiChar = '[]'#10;
  VOIDFIELD: PAnsiChar = '{"FieldCount":0}';
begin
  CancelAll; // rewind JSON
  if fExpand then
    // same as sqlite3_get_table()
    inc(fTotalFileSize, fStream.Write(VOIDARRAY^, 3))
  else
    inc(fTotalFileSize, fStream.Write(VOIDFIELD^, 16));
end;

constructor TJsonWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TFieldBits; aBufSize: integer);
begin
  Create(aStream, Expand, withID, FieldBitsToIndex(Fields), aBufSize);
end;

constructor TJsonWriter.Create(aStream: TStream; Expand, withID: boolean;
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

procedure TJsonWriter.AddColumns(aKnownRowsCount: integer);
var
  i: PtrInt;
begin
  if fExpand then
  begin
    if twoForceJsonExtended in CustomOptions then
      for i := 0 to length(ColNames) - 1 do
        ColNames[i] := ColNames[i] + ':'
    else
      for i := 0 to length(ColNames) - 1 do
        ColNames[i] := '"' + ColNames[i] + '":';
  end
  else
  begin
    AddShort('{"fieldCount":');
    Add(length(ColNames));
    if aKnownRowsCount > 0 then
    begin
      AddShort(',"rowCount":');
      Add(aKnownRowsCount);
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

procedure TJsonWriter.AddColumn(aColName: PUtf8Char; aColIndex, aColCount: PtrInt);
var
  len, collen: PtrInt;
  P: PUtf8Char;
begin
  len := StrLen(aColName);
  if fExpand then
  begin
    if aColIndex = 0 then // non-expanded mode doesn't use ColNames[]
      SetLength(ColNames, aColCount);
    collen := len + 1;
    if not (twoForceJsonExtended in CustomOptions) then
      inc(collen, 2);
    FastSetString(ColNames[aColIndex], nil, collen);
    P := pointer(ColNames[aColIndex]);
    if twoForceJsonExtended in CustomOptions then
      MoveFast(aColName^, P^, len)  // extended JSON unquoted field names
    else
    begin
      P[0] := '"';
      inc(P);
      MoveFast(aColName^, P^, len); // regular JSON quoted field name
      P[len] := '"';
      inc(P);
    end;
    P[len] := ':';
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
    end else
      AddShorter('","');
  end;
end;

procedure TJsonWriter.ChangeExpandedFields(aWithID: boolean;
  const aFields: TFieldIndexDynArray);
begin
  if not Expand then
    raise ESynDBException.CreateUtf8(
      '%.ChangeExpandedFields() called with Expanded=false', [self]);
  fWithID := aWithID;
  fFields := aFields;
end;

procedure TJsonWriter.EndJsonObject(aKnownRowsCount, aRowsCount: integer;
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

procedure TJsonWriter.TrimFirstRow;
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

constructor TSelectStatement.Create(const SQL: RawUtf8;
  const GetFieldIndex: TOnGetFieldIndex; const SimpleFields: TSelectStatementSelectDynArray);
var
  Prop, whereBefore: RawUtf8;
  P, B: PUtf8Char;
  ndx, order, err, len, selectCount, whereCount: integer;
  whereWithOR, whereNotClause: boolean;

  function GetPropIndex: integer;
  begin
    if not GetNextFieldProp(P, Prop) then
      result := -1
    else if IsRowID(pointer(Prop)) then
      result := 0
    else
    begin
      // 0 = ID field
      result := GetFieldIndex(Prop);
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
      select.FunctionName := Prop;
      inc(fSelectFunctionCount);
      if IdemPropNameU(Prop, 'COUNT') and
         (P^ = '*') then
      begin
        select.Field := 0; // count( * ) -> count(ID)
        select.FunctionKnown := funcCountStar;
        P := GotoNextNotSpace(P + 1);
      end
      else
      begin
        if IdemPropNameU(Prop, 'DISTINCT') then
          select.FunctionKnown := funcDistinct
        else if IdemPropNameU(Prop, 'MAX') then
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
      SetString(Where.Value, B, P - B);
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
      SetString(Where.ParenthesisAfter, B, P - B);
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
    SetString(tmp, PAnsiChar(Where.ValueSql) + 1, Where.ValueSqlLen - 2);
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
              SetString(Where.Value, PAnsiChar(B), P - B);
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
    GetNextFieldProp(P, Prop);
    if SimpleFields = nil then
      exit;
    fSelect := copy(SimpleFields); // use precalculated array of simple fields
  end
  else if not GetNextSelectField then
    // we need at least one field name
    exit
  else if P^ <> ',' then
    GetNextFieldProp(P, Prop)
  else
    repeat
      while P^ in [',', #1..' '] do
        inc(P); // trim left
    until not GetNextSelectField; // add other CSV field names
  // 2. get FROM clause
  if not IdemPropNameU(Prop, 'FROM') then
    exit; // incorrect SQL statement
  GetNextFieldProp(P, Prop);
  fTableName := Prop;
  // 3. get WHERE clause
  whereCount := 0;
  whereWithOR := false;
  whereNotClause := false;
  whereBefore := '';
  GetNextFieldProp(P, Prop);
  if IdemPropNameU(Prop, 'WHERE') then
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
        SetString(whereBefore, B, P - B);
        B := P;
      end;
      ndx := GetPropIndex;
      if ndx < 0 then
      begin
        if IdemPropNameU(Prop, 'NOT') then
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
            FunctionName := UpperCase(Prop);
            // Byte/Word/Integer/cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
            len := length(Prop);
            if (len > 16) and
               IdemPropName('DynArrayContains',
                 PUtf8Char(@PByteArray(Prop)[len - 16]), 16) then
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
      GetNextFieldProp(P, Prop);
      if IdemPropNameU(Prop, 'OR') then
        whereWithOR := true
      else if IdemPropNameU(Prop, 'AND') then
        whereWithOR := false
      else
        goto lim2;
      whereNotClause := false;
      whereBefore := '';
    until false;
    // 4. get optional LIMIT/OFFSET/ORDER clause
lim:P := GotoNextNotSpace(P);
    while (P <> nil) and
          not (P^ in [#0, ';']) do
    begin
      GetNextFieldProp(P, Prop);
lim2: if IdemPropNameU(Prop, 'LIMIT') then
        fLimit := GetNextItemCardinal(P, ' ')
      else if IdemPropNameU(Prop, 'OFFSET') then
        fOffset := GetNextItemCardinal(P, ' ')
      else if IdemPropNameU(Prop, 'ORDER') then
      begin
        GetNextFieldProp(P, Prop);
        if IdemPropNameU(Prop, 'BY') or
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
              if GetNextFieldProp(P, Prop) then
                if IdemPropNameU(Prop, 'DESC') then
                  include(fOrderByFieldDesc, order)
                else if not IdemPropNameU(Prop, 'ASC') then
                  goto lim2; // parse LIMIT OFFSET clauses after ORDER
              if P^ <> ',' then
                break; // no more fields in this ORDER BY clause
            end;
            P := GotoNextNotSpace(P + 1);
          until P^ in [#0, ';'];
        end
        else
          exit; // incorrect SQL statement
      end
      else if IdemPropNameU(Prop, 'GROUP') then
      begin
        GetNextFieldProp(P, Prop);
        if IdemPropNameU(Prop, 'BY') then
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
      end
      else if (Prop <> '') or
              not (GotoNextNotSpace(P)^ in [#0, ';']) then
        exit // incorrect SQL statement
      else
        break; // reached the end of the statement
    end;
  end
  else if Prop <> '' then
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


initialization
  ShortStringToAnsi7String(JSON_SQLDATE_MAGIC_STR, JSON_SQLDATE_MAGIC_TEXT);
  ID_TXT := 'ID'; // avoid reallocation
  ROWID_TXT := 'RowID';

end.

