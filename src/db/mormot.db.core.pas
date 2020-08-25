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
    - TJSONWriter Specialized for Database Export

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.json;


{ ************ Shared Database Fields and Values Definitions }

const
  /// maximum number of fields in a database Table
  // - default is 64, but can be set to 64, 128, 192 or 256
  // adding MAX_SQLFIELDS_128, MAX_SQLFIELDS_192 or MAX_SQLFIELDS_256
  // conditional directives for your project
  // - this constant is used internaly to optimize memory usage in the
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

  /// sometimes, the ID field is included in a bits set
  MAX_SQLFIELDS_INCLUDINGID = MAX_SQLFIELDS + 1;


type
  /// handled field/parameter/column types for abstract database access
  // - this will map JSON-compatible low-level database-level access types, not
  // high-level Delphi types as TSQLFieldType defined in mORMot.pas
  // - it does not map either all potential types as defined in DB.pas (which
  // are there for compatibility with old RDBMS, and are not abstract enough)
  // - those types can be mapped to standard SQLite3 generic types, i.e.
  // NULL, INTEGER, REAL, TEXT, BLOB (with the addition of a ftCurrency and
  // ftDate type, for better support of most DB engines)
  // see @http://www.sqlite.org/datatype3.html
  // - the only string type handled here uses UTF-8 encoding (implemented
  // using our RawUTF8 type), for cross-Delphi true Unicode process
  TSQLDBFieldType = (
    ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob);

  /// set of field/parameter/column types for abstract database access
  TSQLDBFieldTypes = set of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  TSQLDBFieldTypeDynArray = array of TSQLDBFieldType;

  /// array of field/parameter/column types for abstract database access
  // - this array as a fixed size, ready to handle up to MAX_SQLFIELDS items
  TSQLDBFieldTypeArray = array[0..MAX_SQLFIELDS - 1] of TSQLDBFieldType;
  PSQLDBFieldTypeArray = ^TSQLDBFieldTypeArray;

  /// how TSQLVar may be processed
  // - by default, ftDate will use seconds resolution unless svoDateWithMS is set
  TSQLVarOption = (svoDateWithMS);

  /// defines how TSQLVar may be processed
  TSQLVarOptions = set of TSQLVarOption;

  /// memory structure used for database values by reference storage
  // - used mainly by SynDB, mORMot, mORMotDB and mORMotSQLite3 units
  // - defines only TSQLDBFieldType data types (similar to those handled by
  // SQLite3, with the addition of ftCurrency and ftDate)
  // - cleaner/lighter dedicated type than TValue or variant/TVarData, strong
  // enough to be marshalled as JSON content
  // - variable-length data (e.g. UTF-8 text or binary BLOB) are never stored
  // within this record, but VText/VBlob will point to an external (temporary)
  // memory buffer
  // - date/time is stored as ISO-8601 text (with milliseconds if svoDateWithMS
  // option is set and the database supports it), and currency as double or BCD
  // in most databases
  TSQLVar = record
    /// how this value should be processed
    Options: TSQLVarOptions;
    /// the type of the value stored
    case VType: TSQLDBFieldType of
      ftInt64: (
        VInt64: Int64);
      ftDouble: (
        VDouble: double);
      ftDate: (
        VDateTime: TDateTime);
      ftCurrency: (
        VCurrency: Currency);
      ftUTF8: (
        VText: PUTF8Char);
      ftBlob: (
        VBlob: pointer;
        VBlobLen: Integer)
  end;

  /// dynamic array of database values by reference storage
  TSQLVarDynArray = array of TSQLVar;

  /// used to store bit set for all available fields in a Table
  // - with current MAX_SQLFIELDS value, 64 bits uses 8 bytes of memory
  // - see also IsZero() and IsEqual() functions
  // - you can also use ALL_FIELDS as defined in mORMot.pas
  TSQLFieldBits = set of 0..MAX_SQLFIELDS - 1;

  /// used to store a field index in a Table
  // - note that -1 is commonly used for the ID/RowID field so the values should
  // be signed
  // - even if ShortInt (-128..127) may have been enough, we define a 16 bit
  // safe unsigned integer to let the source compile with Delphi 5
  TSQLFieldIndex = SmallInt; // -32768..32767

  /// used to store field indexes in a Table
  // - same as TSQLFieldBits, but allowing to store the proper order
  TSQLFieldIndexDynArray = array of TSQLFieldIndex;

  /// points to a bit set used for all available fields in a Table
  PSQLFieldBits = ^TSQLFieldBits;

  /// generic parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamType = (
    sptUnknown, sptInteger, sptFloat, sptText, sptBlob, sptDateTime);

  /// array of parameter types, as recognized by SQLParamContent() and
  // ExtractInlineParameters() functions
  TSQLParamTypeDynArray = array of TSQLParamType;

/// returns TRUE if no bit inside this TSQLFieldBits is set
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsZero(const Fields: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast comparison of two TSQLFieldBits values
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
function IsEqual(const A, B: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast initialize a TSQLFieldBits with 0
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value
procedure FillZero(var Fields: TSQLFieldBits); overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a TSQLFieldBits set of bits into an array of integers
procedure FieldBitsToIndex(const Fields: TSQLFieldBits; var Index: TSQLFieldIndexDynArray;
  MaxLength: integer = MAX_SQLFIELDS; IndexStart: integer = 0); overload;

/// convert a TSQLFieldBits set of bits into an array of integers
function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer = MAX_SQLFIELDS): TSQLFieldIndexDynArray; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// add a field index to an array of field indexes
// - returns the index in Indexes[] of the newly appended Field value
function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray; var Fields: TSQLFieldBits); overload;

// search a field index in an array of field indexes
// - returns the index in Indexes[] of the given Field value, -1 if not found
function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;

/// convert an array of field indexes into a TSQLFieldBits set of bits
function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns the stored size of a TSQLVar database value
// - only returns VBlobLen / StrLen(VText) size, 0 otherwise
function SQLVarLength(const Value: TSQLVar): integer;

/// convert any Variant into a database value
// - ftBlob kind won't be handled by this function
// - complex variant types would be converted into ftUTF8 JSON object/array
procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);

/// guess the correct TSQLDBFieldType from a variant type
function VariantVTypeToSQLDBFieldType(VType: cardinal): TSQLDBFieldType;

/// guess the correct TSQLDBFieldType from a variant value
function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;
  {$ifdef HASINLINE}inline;{$endif}

/// guess the correct TSQLDBFieldType from the UTF-8 representation of a value
function TextToSQLDBFieldType(json: PUTF8Char): TSQLDBFieldType;

type
  /// SQL Query comparison operators
  // - used e.g. by CompareOperator() functions in SynTable.pas or vt_BestIndex()
  // in mORMotSQLite3.pas
  TCompareOperator = (
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
  /// convert identified field types into high-level ORM types
  // - as will be implemented in unit mORMot.pas
  SQLDBFIELDTYPE_TO_DELPHITYPE: array[TSQLDBFieldType] of RawUTF8 = (
    '???','???', 'Int64', 'Double', 'Currency', 'TDateTime', 'RawUTF8', 'TSQLRawBlob');



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
  // - either a varNull or varString (RawUTF8) will be stored in the variant
  // - either a NULL or a TEXT value will be stored in the database
  // - the property should be defined as such:
  // ! property Txt: TNullableUTF8Text read fTxt write fTxt;
  // or for a fixed-width VARCHAR (in external databases), here of 32 max chars:
  // ! property Txt: TNullableUTF8Text index 32 read fTxt write fTxt;
  // - warning: prior to Delphi 2009, since the variant will be stored as
  // RawUTF8 internally, you should not use directly the field value as a
  // VCL string=AnsiString like string(aField) but use VariantToString(aField)
  TNullableUTF8Text = type variant;


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
function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableInteger is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Integer value
function NullableIntegerToValue(const V: TNullableInteger; out Value: Int64): Boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableInteger is null, or return its value
// - returns 0 if V is null or empty, or the stored Integer value
function NullableIntegerToValue(const V: TNullableInteger): Int64;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable boolean value containing null
  NullableBooleanNull: TNullableBoolean absolute NullVarData;

/// creates a nullable Boolean value from a supplied constant
// - FPC does not allow direct assignment to a TNullableBoolean = type variant
// variable: use this function to circumvent it
function NullableBoolean(Value: boolean): TNullableBoolean;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableBoolean = type variant variant: use this
// function to circumvent those limitations
function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableBoolean is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Boolean value
function NullableBooleanToValue(const V: TNullableBoolean; out Value: Boolean): Boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableBoolean is null, or return its value
// - returns false if V is null or empty, or the stored Boolean value
function NullableBooleanToValue(const V: TNullableBoolean): Boolean;
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
function NullableFloatIsEmptyOrNull(const V: TNullableFloat): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableFloat is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Float value
function NullableFloatToValue(const V: TNullableFloat; out Value: double): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableFloat is null, or return its value
// - returns 0 if V is null or empty, or the stored Float value
function NullableFloatToValue(const V: TNullableFloat): double;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable currency value containing null
  NullableCurrencyNull: TNullableCurrency absolute NullVarData;

/// creates a nullable Currency value from a supplied currency constant
// - FPC does not allow direct assignment to a TNullableCurrency = type variant
// variable: use this function to circumvent it
// - use NullableCurrency() if you want to create a nullable Currency not from
// a constant, but from a TSynCurrency safe type
function NullableCurrencyConstant(const Value: system.currency): TNullableCurrency;
  {$ifdef HASINLINE}inline;{$endif}

/// creates a nullable Currency value from a supplied TSynCurrency value
// - we defined the TSynCurrency type to circumvent FPC cross-platform issues
// with currency values;
function NullableCurrency(const Value: TSynCurrency): TNullableCurrency;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableCurrency = type variant variable: use this
// function to circumvent those limitations
function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableCurrency is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the Currency value
// - we defined the TSynCurrency type to circumvent FPC cross-platform issues
// with currency values;
function NullableCurrencyToValue(const V: TNullableCurrency; out Value: TSynCurrency): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableCurrency is null, or return its value
// - returns 0 if V is null or empty, or the stored Currency value
// - we defined the TSynCurrency type to circumvent FPC cross-platform issues
// with currency values;
function NullableCurrencyToValue(const V: TNullableCurrency): TSynCurrency;
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
function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableDateTime is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the DateTime value
function NullableDateTimeToValue(const V: TNullableDateTime; out Value: TDateTime): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

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
function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableTimeLog is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog; out Value: TTimeLog): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableTimeLog is null, or return its value
// - returns 0 if V is null or empty, or the stored TimeLog value
function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
  overload; {$ifdef HASINLINE}inline;{$endif}


var
  /// a nullable UTF-8 encoded text value containing null
  NullableUTF8TextNull: TNullableUTF8Text absolute NullVarData;

/// creates a nullable UTF-8 encoded text value from a supplied constant
// - FPC does not allow direct assignment to a TNullableUTF8 = type variant
// variable: use this function to circumvent it
function NullableUTF8Text(const Value: RawUTF8): TNullableUTF8Text;
  {$ifdef HASINLINE}inline;{$endif}

/// same as VarIsEmpty(V) or VarIsEmpty(V), but faster
// - FPC VarIsNull() seems buggy with varByRef variants, and does not allow
// direct transtyping from a TNullableUTF8Text = type variant variable: use this
// function to circumvent those limitations
function NullableUTF8TextIsEmptyOrNull(const V: TNullableUTF8Text): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableUTF8Text is null, or return its value
// - returns FALSE if V is null or empty, or TRUE and set the UTF8Text value
function NullableUTF8TextToValue(const V: TNullableUTF8Text; out Value: RawUTF8): boolean;
  overload; {$ifdef HASINLINE}inline;{$endif}

/// check if a TNullableUTF8Text is null, or return its value
// - returns '' if V is null or empty, or the stored UTF8-encoded text value
function NullableUTF8TextToValue(const V: TNullableUTF8Text): RawUTF8;
  overload; {$ifdef HASINLINE}inline;{$endif}


{ ************ Date/Time SQL encoding }

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(EncodeDate(2012,5,4))]);
function DateToSQL(Date: TDateTime): RawUTF8; overload;

/// convert a date to a ISO-8601 string format for SQL '?' inlined parameters
// - will return the date encoded as '\uFFF1YYYY-MM-DD' - therefore
// ':("\uFFF12012-05-04"):' pattern will be recognized as a sftDateTime
// inline parameter in  SQLParamContent() / ExtractInlineParameters() functions
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum=?',[DateToSQL(2012,5,4)]);
function DateToSQL(Year, Month, Day: cardinal): RawUTF8; overload;

/// convert a date/time to a ISO-8601 string format for SQL '?' inlined parameters
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as '\uFFF1YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as '\uFFF1Thh:mm:ss'
// - otherwise, returns the ISO-8601 date and time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss'
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - if WithMS is TRUE, will append '.sss' for milliseconds resolution
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[DateTimeToSQL(Now)]);
// - see TimeLogToSQL() if you are using TTimeLog/TModTime/TCreateTime values
function DateTimeToSQL(DT: TDateTime; WithMS: boolean = false): RawUTF8;

/// decode a SQL '?' inlined parameter (i.e. with JSON_SQLDATE_MAGIC prefix)
// - as generated by DateToSQL/DateTimeToSQL/TimeLogToSQL functions
function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;

/// convert a TTimeLog value into a ISO-8601 string format for SQL '?' inlined
// parameters
// - handle TTimeLog bit-encoded Int64 format
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - to be used e.g. as in:
// ! aRec.CreateAndFillPrepare(Client,'Datum<=?',[TimeLogToSQL(TimeLogNow)]);
function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;

/// convert a Iso8601 encoded string into a ISO-8601 string format for SQL
// '?' inlined parameters
// - follows the same pattern as DateToSQL or DateTimeToSQL functions, i.e.
// will return the date or time encoded as '\uFFF1YYYY-MM-DDThh:mm:ss' -
// therefore ':("\uFFF12012-05-04T20:12:13"):' pattern will be recognized as a
// sftDateTime inline parameter in  SQLParamContent() / ExtractInlineParameters()
// (JSON_SQLDATE_MAGIC will be used as prefix to create '\uFFF1...' pattern)
// - in practice, just append the JSON_SQLDATE_MAGIC prefix to the supplied text
function Iso8601ToSQL(const S: RawByteString): RawUTF8;



{ ************ SQL Parameters Inlining and Processing }

/// guess the content type of an UTF-8 SQL value, in :(....): format
// - will be used e.g. by ExtractInlineParameters() to un-inline a SQL statement
// - sftInteger is returned for an INTEGER value, e.g. :(1234):
// - sftFloat is returned for any floating point value (i.e. some digits
// separated by a '.' character), e.g. :(12.34): or :(12E-34):
// - sftUTF8Text is returned for :("text"): or :('text'):, with double quoting
// inside the value
// - sftBlob will be recognized from the ':("\uFFF0base64encodedbinary"):'
// pattern, and return raw binary (for direct blob parameter assignment)
// - sftDateTime will be recognized from ':(\uFFF1"2012-05-04"):' pattern,
// i.e. JSON_SQLDATE_MAGIC-prefixed string as returned by DateToSQL() or
// DateTimeToSQL() functions
// - sftUnknown is returned on invalid content, or if wasNull is set to TRUE
// - if ParamValue is not nil, the pointing RawUTF8 string is set with the
// value inside :(...): without double quoting in case of sftUTF8Text
// - wasNull is set to TRUE if P was ':(null):' and ParamType is sftUnknwown
function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType;
  out ParamValue: RawUTF8; out wasNull: boolean): PUTF8Char;

/// this function will extract inlined :(1234): parameters into Types[]/Values[]
// - will return the generic SQL statement with ? place holders for inlined
// parameters and setting Values with SQLParamContent() decoded content
// - will set maxParam=0 in case of no inlined parameters
// - recognized types are sptInteger, sptFloat, sptDateTime ('\uFFF1...'),
// sptUTF8Text and sptBlob ('\uFFF0...')
// - sptUnknown is returned on invalid content
function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;

/// returns a 64-bit value as inlined ':(1234):' text
function InlineParameter(ID: Int64): shortstring; overload;

/// returns a string value as inlined ':("value"):' text
function InlineParameter(const value: RawUTF8): RawUTF8; overload;


{ ************ TJSONWriter Specialized for Database Export }

type
  /// simple writer to a Stream, specialized for the JSON format and SQL export
  // - i.e. define some property/method helpers to export SQL resultset as JSON
  TJSONWriter = class(TTextWriter)
  protected
    /// used to store output format
    fExpand: boolean;
    /// used to store output format for TSQLRecord.GetJSONValues()
    fWithID: boolean;
    /// used to store field for TSQLRecord.GetJSONValues()
    fFields: TSQLFieldIndexDynArray;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    fStartDataPosition: integer;
  public
    /// used internally to store column names and count for AddColumns
    ColNames: TRawUTF8DynArray;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits; aBufSize: integer = 8192); overload;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldIndexDynArray = nil; aBufSize: integer = 8192;
      aStackBuffer: PTextWriterStackBuffer = nil); overload;
    /// rewind the Stream position and write void JSON object
    procedure CancelAllVoid;
    /// write or init field names for appropriate JSON Expand later use
    // - ColNames[] must have been initialized before calling this procedure
    // - if aKnownRowsCount is not null, a "rowCount":... item will be added
    // to the generated JSON stream (for faster unserialization of huge content)
    procedure AddColumns(aKnownRowsCount: integer = 0);
    /// allow to change on the fly an expanded format column layout
    // - by definition, a non expanded format will raise a ESynException
    // - caller should then set ColNames[] and run AddColumns()
    procedure ChangeExpandedFields(aWithID: boolean; const aFields: TSQLFieldIndexDynArray); overload;
    /// end the serialized JSON object
    // - cancel last ','
    // - close the JSON object ']' or ']}'
    // - write non expanded postlog (,"rowcount":...), if needed
    // - flush the internal buffer content if aFlushFinal=true
    procedure EndJSONObject(aKnownRowsCount,aRowsCount: integer;
      aFlushFinal: boolean = true);
      {$ifdef HASINLINE}inline;{$endif}
    /// the first data row is erased from the content
    // - only works if the associated storage stream is TMemoryStream
    // - expect not Expanded format
    procedure TrimFirstRow;
    /// is set to TRUE in case of Expanded format
    property Expand: boolean read fExpand write fExpand;
    /// is set to TRUE if the ID field must be appended to the resulting JSON
    // - this field is used only by TSQLRecord.GetJSONValues
    // - this field is ignored by TSQLTable.GetJSONValues
    property WithID: boolean read fWithID;
    /// Read-Only access to the field bits set for each column to be stored
    property Fields: TSQLFieldIndexDynArray read fFields;
    /// if not Expanded format, contains the Stream position of the first
    // useful Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    property StartDataPosition: integer read fStartDataPosition;
  end;


implementation


{ ************ Shared Database Fields and Values Definitions }

function IsZero(const Fields: TSQLFieldBits): boolean;
var
  f: TPtrIntArray absolute Fields;
begin
  {$ifdef CPU64}
  {$ifdef MAX_SQLFIELDS_128}
  result := (f[0] = 0) and (f[1] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (f[0] = 0) and (f[1] = 0) and (f[2] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (f[0] = 0) and (f[1] = 0) and (f[2] = 0) and (f[3] = 0);
  {$else}
  result := (f[0] = 0);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$else}
  {$ifdef MAX_SQLFIELDS_128}
  result := (f[0] = 0) and (f[1] = 0) and (f[2] = 0) and (f[3] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (f[0] = 0) and (f[1] = 0) and (f[2] = 0) and (f[3] = 0) and
            (f[4] = 0) and (f[5] = 0);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (f[0] = 0) and (f[1] = 0) and (f[2] = 0) and (f[3] = 0) and
            (f[4] = 0) and (f[5] = 0) and (f[6] = 0) and (f[7] = 0);
  {$else}
  result := (f[0] = 0) and (f[1] = 0);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$endif CPU64}
end;

function IsEqual(const A, B: TSQLFieldBits): boolean;
var
  a_: TPtrIntArray absolute A;
  b_: TPtrIntArray absolute B;
begin
  {$ifdef CPU64}
  {$ifdef MAX_SQLFIELDS_128}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]) and (a_[2] = b_[2]);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]) and (a_[2] = b_[2]) and (a_[3] = b_[3]);
  {$else}
  result := (a_[0] = b_[0]);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$else}
  {$ifdef MAX_SQLFIELDS_128}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]) and (a_[2] = b_[2]) and (a_[3] = b_[3]);
  {$else}
  {$ifdef MAX_SQLFIELDS_192}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]) and (a_[2] = b_[2]) and
            (a_[3] = b_[3]) and (a_[4] = b_[4]) and (a_[5] = b_[5]);
  {$else}
  {$ifdef MAX_SQLFIELDS_256}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]) and (a_[2] = b_[2]) and
            (a_[3] = b_[3]) and (a_[4] = b_[4]) and (a_[5] = b_[5]) and
            (a_[6] = b_[6]) and (a_[7] = b_[7]);
  {$else}
  result := (a_[0] = b_[0]) and (a_[1] = b_[1]);
  {$endif MAX_SQLFIELDS_256}
  {$endif MAX_SQLFIELDS_192}
  {$endif MAX_SQLFIELDS_128}
  {$endif CPU64}
end;

procedure FillZero(var Fields: TSQLFieldBits);
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

procedure FieldBitsToIndex(const Fields: TSQLFieldBits; var Index:
  TSQLFieldIndexDynArray; MaxLength, IndexStart: integer);
var
  i, n: PtrInt;
  sets: array[0..MAX_SQLFIELDS - 1] of TSQLFieldIndex; // to avoid memory reallocation
begin
  n := 0;
  for i := 0 to MaxLength - 1 do
    if i in Fields then
    begin
      sets[n] := i;
      inc(n);
    end;
  SetLength(Index, IndexStart + n);
  for i := 0 to n - 1 do
    Index[IndexStart + i] := {%H-}sets[i];
end;

function FieldBitsToIndex(const Fields: TSQLFieldBits;
  MaxLength: integer): TSQLFieldIndexDynArray;
begin
  FieldBitsToIndex(Fields, result, MaxLength);
end;

function AddFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  result := length(Indexes);
  SetLength(Indexes, result + 1);
  Indexes[result] := Field;
end;

function SearchFieldIndex(var Indexes: TSQLFieldIndexDynArray; Field: integer): integer;
begin
  for result := 0 to length(Indexes) - 1 do
    if Indexes[result] = Field then
      exit;
  result := -1;
end;

procedure FieldIndexToBits(const Index: TSQLFieldIndexDynArray;
  var Fields: TSQLFieldBits);
var
  i: integer;
begin
  FillCharFast(Fields, SizeOf(Fields), 0);
  for i := 0 to Length(Index) - 1 do
    if Index[i] >= 0 then
      include(Fields, Index[i]);
end;

function FieldIndexToBits(const Index: TSQLFieldIndexDynArray): TSQLFieldBits;
begin
  FieldIndexToBits(Index, result);
end;

function SQLVarLength(const Value: TSQLVar): integer;
begin
  case Value.VType of
    ftBlob:
      result := Value.VBlobLen;
    ftUTF8:
      result := StrLen(Value.VText); // fast enough for our purpose
  else
    result := 0; // simple/ordinal values, or ftNull
  end;
end;

procedure VariantToSQLVar(const Input: variant; var temp: RawByteString;
  var Output: TSQLVar);
var
  wasString: boolean;
begin
  Output.Options := [];
  with TVarData(Input) do
    if VType = varVariant or varByRef then
      VariantToSQLVar(PVariant(VPointer)^, temp, Output)
    else
      case VType of
        varEmpty, varNull:
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
        varWord64, varInt64:
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
          begin // varDate would be converted into ISO8601 by VariantToUTF8()
            Output.VType := ftDouble;
            Output.VDouble := VDouble;
          end;
        varCurrency:
          begin
            Output.VType := ftCurrency;
            Output.VInt64 := VInt64;
          end;
        varString:
          begin // assume RawUTF8
            Output.VType := ftUTF8;
            Output.VText := VPointer;
          end;
      else // handle less current cases
        if VariantToInt64(Input, Output.VInt64) then
          Output.VType := ftInt64
        else
        begin
          VariantToUTF8(Input, RawUTF8(temp), wasString);
          if wasString then
          begin
            Output.VType := ftUTF8;
            Output.VText := pointer(temp);
          end
          else
            Output.VType := ftNull;
        end;
      end;
end;

function VariantVTypeToSQLDBFieldType(VType: cardinal): TSQLDBFieldType;
begin
  case VType of
    varNull:
      result := ftNull;
    varShortInt, varWord, varLongWord,
    varSmallInt, varByte, varBoolean, varInteger, varInt64, varWord64:
      result := ftInt64;
    varSingle, varDouble:
      result := ftDouble;
    varDate:
      result := ftDate;
    varCurrency:
      result := ftCurrency;
    varString:
      result := ftUTF8;
  else
    result := ftUnknown; // includes varEmpty
  end;
end;

function VariantTypeToSQLDBFieldType(const V: Variant): TSQLDBFieldType;
var
  VD: TVarData absolute V;
  tmp: TVarData;
begin
  result := VariantVTypeToSQLDBFieldType(VD.VType);
  case result of
    ftUnknown:
      if VD.VType = varEmpty then
        result := ftUnknown
      else if SetVariantUnRefSimpleValue(V, tmp{%H-}) then
        result := VariantTypeToSQLDBFieldType(variant(tmp))
      else
        result := ftUTF8;
    ftUTF8:
      if (VD.VString <> nil) and
         (PCardinal(VD.VString)^ and $ffffff = JSON_BASE64_MAGIC) then
        result := ftBlob;
  end;
end;

function TextToSQLDBFieldType(json: PUTF8Char): TSQLDBFieldType;
begin
  if json = nil then
    result := ftNull
  else
    result := VariantVTypeToSQLDBFieldType(TextToVariantNumberType(json));
end;


{ ************ Nullable Values Stored as Variant }

// TNullableInteger

function NullableInteger(const Value: Int64): TNullableInteger;
begin
  PVariant(@result)^ := Value;
end;

function NullableIntegerIsEmptyOrNull(const V: TNullableInteger): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableIntegerToValue(const V: TNullableInteger; out Value: Int64): Boolean;
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

function NullableBooleanIsEmptyOrNull(const V: TNullableBoolean): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableBooleanToValue(const V: TNullableBoolean; out Value: Boolean): Boolean;
begin
  Value := false;
  result := not VarDataIsEmptyOrNull(@V) and VariantToBoolean(PVariant(@V)^, Value);
end;

function NullableBooleanToValue(const V: TNullableBoolean): Boolean;
begin
  VariantToBoolean(PVariant(@V)^, result);
end;

// TNullableFloat

function NullableFloat(const Value: double): TNullableFloat;
begin
  PVariant(@result)^ := Value;
end;

function NullableFloatIsEmptyOrNull(const V: TNullableFloat): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableFloatToValue(const V: TNullableFloat; out Value: Double): Boolean;
begin
  PInt64(@Value)^ := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToDouble(PVariant(@V)^, Value);
end;

function NullableFloatToValue(const V: TNullableFloat): Double;
begin
  VariantToDouble(PVariant(@V)^, result);
end;

// TNullableCurrency

function NullableCurrencyConstant(const Value: system.currency): TNullableCurrency;
begin
  PVariant(@result)^ := Value;
end;

function NullableCurrency(const Value: TSynCurrency): TNullableCurrency;
begin
  CurrencyToVariant(Value, PVariant(@result)^);
end;

function NullableCurrencyIsEmptyOrNull(const V: TNullableCurrency): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableCurrencyToValue(const V: TNullableCurrency; out Value:
  TSynCurrency): Boolean;
begin
  PInt64(@Value)^ := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToCurrency(PVariant(@V)^, Value);
end;

function NullableCurrencyToValue(const V: TNullableCurrency): TSynCurrency;
begin
  VariantToCurrency(PVariant(@V)^, result);
end;

// TNullableDateTime

function NullableDateTime(const Value: TDateTime): TNullableDateTime;
begin
  PVariant(@result)^ := Value;
end;

function NullableDateTimeIsEmptyOrNull(const V: TNullableDateTime): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableDateTimeToValue(const V: TNullableDateTime; out Value: TDateTime): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToDouble(PVariant(@V)^,
    Double(Value));
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

function NullableTimeLogIsEmptyOrNull(const V: TNullableTimeLog): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog; out Value: TTimeLog): Boolean;
begin
  Value := 0;
  result := not VarDataIsEmptyOrNull(@V) and VariantToInt64(PVariant(@V)^,
    PInt64(@Value)^);
end;

function NullableTimeLogToValue(const V: TNullableTimeLog): TTimeLog;
begin
  VariantToInt64(PVariant(@V)^, PInt64(@result)^);
end;

// TNullableUTF8Text

function NullableUTF8Text(const Value: RawUTF8): TNullableUTF8Text;
begin
  ClearVariantForString(PVariant(@result)^);
  RawUTF8(TVarData(result).VAny) := Value;
end;

function NullableUTF8TextIsEmptyOrNull(const V: TNullableUTF8Text): Boolean;
begin
  result := VarDataIsEmptyOrNull(@V);
end;

function NullableUTF8TextToValue(const V: TNullableUTF8Text; out Value: RawUTF8): boolean;
begin
  result := not VarDataIsEmptyOrNull(@V) and VariantToUTF8(PVariant(@V)^, Value);
end;

function NullableUTF8TextToValue(const V: TNullableUTF8Text): RawUTF8;
var
  dummy: boolean;
begin
  if VarDataIsEmptyOrNull(@V) then // VariantToUTF8() will return 'null'
    result := ''
  else
    VariantToUTF8(PVariant(@V)^, result, dummy);
end;


{ ************ Date/Time SQL encoding }

function DateToSQL(Date: TDateTime): RawUTF8;
begin
  result := '';
  if Date <= 0 then
    exit;
  FastSetString(result, nil, 13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
  DateToIso8601PChar(Date, PUTF8Char(pointer(result)) + 3, True);
end;

function DateToSQL(Year, Month, Day: Cardinal): RawUTF8;
begin
  result := '';
  if (Year = 0) or (Month - 1 > 11) or (Day - 1 > 30) then
    exit;
  FastSetString(result, nil, 13);
  PCardinal(pointer(result))^ := JSON_SQLDATE_MAGIC;
  DateToIso8601PChar(PUTF8Char(pointer(result)) + 3, True, Year, Month, Day);
end;

var
  JSON_SQLDATE_MAGIC_TEXT: RawUTF8;

function DateTimeToSQL(DT: TDateTime; WithMS: boolean): RawUTF8;
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

function TimeLogToSQL(const Timestamp: TTimeLog): RawUTF8;
begin
  if Timestamp = 0 then
    result := ''
  else
    result := JSON_SQLDATE_MAGIC_TEXT + PTimeLogBits(@Timestamp)^.Text(true);
end;

function Iso8601ToSQL(const S: RawByteString): RawUTF8;
begin
  if IsIso8601(pointer(S), length(S)) then
    result := JSON_SQLDATE_MAGIC_TEXT + S
  else
    result := '';
end;

function SQLToDateTime(const ParamValueWithMagic: RawUTF8): TDateTime;
begin
  result := Iso8601ToDateTimePUTF8Char(PUTF8Char(
    pointer(ParamValueWithMagic)) + 3, length(ParamValueWithMagic) - 3);
end;


{ ************ SQL Parameters Inlining and Processing }

const
  NULL_LOW = ord('n') + ord('u') shl 8 + ord('l') shl 16 + ord('l') shl 24;

function SQLParamContent(P: PUTF8Char; out ParamType: TSQLParamType;
  out ParamValue: RawUTF8; out wasNull: boolean): PUTF8Char;
var
  PBeg: PAnsiChar;
  L: integer;
  c: cardinal;
begin
  ParamType := sptUnknown;
  wasNull := false;
  result := nil;
  if P = nil then
    exit;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  case P^ of
    '''', '"':
      begin
        P := UnQuoteSQLStringVar(P, ParamValue);
        if P = nil then
          exit; // not a valid quoted string (e.g. unexpected end in middle of it)
        ParamType := sptText;
        L := length(ParamValue) - 3;
        if L > 0 then
        begin
          c := PInteger(ParamValue)^ and $00ffffff;
          if c = JSON_BASE64_MAGIC then
          begin // ':("\uFFF0base64encodedbinary"):' format -> decode
            Base64MagicDecode(ParamValue); // wrapper function to avoid temp. string
            ParamType := sptBlob;
          end
          else if (c = JSON_SQLDATE_MAGIC) and
            IsIso8601(PUTF8Char(pointer(ParamValue)) + 3, L) then
          begin // handle ':("\uFFF112012-05-04"):' format
            Delete(ParamValue, 1, 3);   // return only ISO-8601 text
            ParamType := sptDateTime;   // identified as Date/Time
          end;
        end;
      end;
    '-', '+', '0'..'9':
      begin // allow 0 or + in SQL
        // check if P^ is a true numerical value
        PBeg := pointer(P);
        ParamType := sptInteger;
        repeat
          inc(P)
        until not (P^ in ['0'..'9']); // check digits
        if P^ = '.' then
        begin
          inc(P);
          if P^ in ['0'..'9'] then
          begin
            ParamType := sptFloat;
            repeat
              inc(P)
            until not (P^ in ['0'..'9']); // check fractional digits
          end
          else
          begin
            ParamType := sptUnknown; // invalid '23023.' value
            exit;
          end;
        end;
        if byte(P^) and $DF = ord('E') then
        begin
          ParamType := sptFloat;
          inc(P);
          if P^ = '+' then
            inc(P)
          else if P^ = '-' then
            inc(P);
          while P^ in ['0'..'9'] do
            inc(P);
        end;
        FastSetString(ParamValue, PBeg, P - PBeg);
      end;
    'n':
      if PInteger(P)^ = NULL_LOW then
      begin
        inc(P, 4);
        wasNull := true;
      end
      else
        exit; // invalid content (only :(null): expected)
  else
    exit; // invalid content
  end;
  while (P^ <= ' ') and (P^ <> #0) do
    inc(P);
  if PWord(P)^ <> Ord(')') + Ord(':') shl 8 then
    // we expect finishing with P^ pointing at '):'
    ParamType := sptUnknown
  else
    // result<>nil only if value content in P^
    result := P + 2;
end;

function ExtractInlineParameters(const SQL: RawUTF8;
  var Types: TSQLParamTypeDynArray; var Values: TRawUTF8DynArray;
  var maxParam: integer; var Nulls: TSQLFieldBits): RawUTF8;
var
  ppBeg: integer;
  P, Gen: PUTF8Char;
  wasNull: boolean;
begin
  maxParam := 0;
  FillCharFast(Nulls, SizeOf(Nulls), 0);
  ppBeg := PosEx(RawUTF8(':('), SQL, 1);
  if (ppBeg = 0) or (PosEx(RawUTF8('):'), SQL, ppBeg + 2) = 0) then
  begin
    // SQL code with no valid :(...): internal parameters -> leave maxParam=0
    result := SQL;
    exit;
  end;
  // compute GenericSQL from SQL, converting :(...): into ?
  FastSetString(result, pointer(SQL), length(SQL)); // private copy for unescape
  P := pointer(result); // in-place string unescape (keep SQL untouched)
  Gen := P + ppBeg - 1; // Gen^ just before :(
  inc(P, ppBeg + 1);   // P^ just after :(
  repeat
    Gen^ := '?'; // replace :(...): by ?
    inc(Gen);
    if length(Values) <= maxParam then
      SetLength(Values, maxParam + 16);
    if length(Types) <= maxParam then
      SetLength(Types, maxParam + 64);
    P := SQLParamContent(P, Types[maxParam], Values[maxParam], wasNull);
    if P = nil then
    begin
      maxParam := 0;
      result := SQL;
      exit; // any invalid parameter -> try direct SQL
    end;
    if wasNull then
      include(Nulls, maxParam);
    while (P^ <> #0) and (PWord(P)^ <> Ord(':') + Ord('(') shl 8) do
    begin
      Gen^ := P^;
      inc(Gen);
      inc(P);
    end;
    if P^ = #0 then
      Break;
    inc(P, 2);
    inc(maxParam);
  until false;
  // return generic SQL statement, with ? place-holders and params in Values[]
  SetLength(result, Gen - pointer(result));
  inc(maxParam);
end;

function InlineParameter(ID: Int64): shortstring;
begin
  FormatShort(':(%):', [ID], result);
end;

function InlineParameter(const value: RawUTF8): RawUTF8;
begin
  QuotedStrJSON(value, result, ':(', '):');
end;


{ ************ TJSONWriter Specialized for Database Export }

{ TJSONWriter }

procedure TJSONWriter.CancelAllVoid;
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

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits; aBufSize: integer);
begin
  Create(aStream, Expand, withID, FieldBitsToIndex(Fields), aBufSize);
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldIndexDynArray; aBufSize: integer;
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

procedure TJSONWriter.AddColumns(aKnownRowsCount: integer);
var
  i: PtrInt;
begin
  if fExpand then
  begin
    if twoForceJSONExtended in CustomOptions then
      for i := 0 to High(ColNames) do
        ColNames[i] := ColNames[i] + ':'
    else
      for i := 0 to High(ColNames) do
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
    for i := 0 to High(ColNames) do
    begin
      AddString(ColNames[i]);
      AddNoJSONEscape(PAnsiChar('","'), 3);
    end;
    CancelLastChar('"');
    fStartDataPosition := fStream.Position + (B - fTempBuf);
     // B := buf-1 at startup -> need ',val11' position in
     // "values":["col1","col2",val11,' i.e. current pos without the ','
  end;
end;

procedure TJSONWriter.ChangeExpandedFields(aWithID: boolean;
  const aFields: TSQLFieldIndexDynArray);
begin
  if not Expand then
    raise ESynException.CreateUTF8(
      '%.ChangeExpandedFields() called with Expanded=false', [self]);
  fWithID := aWithID;
  fFields := aFields;
end;

procedure TJSONWriter.EndJSONObject(aKnownRowsCount, aRowsCount: integer;
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

procedure TJSONWriter.TrimFirstRow;
var
  P, PBegin, PEnd: PUTF8Char;
begin
  if (self = nil) or not fStream.InheritsFrom(TMemoryStream) or
     fExpand or (fStartDataPosition = 0) then
    exit;
  // go to begin of first row
  FlushToStream; // we need the data to be in fStream memory
  // PBegin^=val11 in { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
  PBegin := TMemoryStream(fStream).Memory;
  PEnd := PBegin + fStream.Position;
  PEnd^ := #0; // mark end of current values
  inc(PBegin, fStartDataPosition + 1); // +1 to include ',' of ',val11'
  // jump to end of first row
  P := GotoNextJSONItem(PBegin, length(ColNames));
  if P = nil then
    exit; // unexpected end
  // trim first row data
  if P^ <> #0 then
    MoveFast(P^, PBegin^, PEnd - P); // erase content
  fStream.Seek(PBegin - P, soCurrent); // adjust current stream position
end;



initialization
  SetLength(JSON_SQLDATE_MAGIC_TEXT, 3);
  PCardinal(pointer(JSON_SQLDATE_MAGIC_TEXT))^ := JSON_SQLDATE_MAGIC;
end.

