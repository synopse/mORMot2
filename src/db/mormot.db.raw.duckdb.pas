/// DuckDB native C API binding for mORMot 2
// - dynamic loading of duckdb.dll / libduckdb.so / libduckdb.dylib
// - Delphi 7 .. Delphi 13 and FPC 3.2+ compatible syntax
// - note: official DuckDB binaries are 64-bit only - not for Win32
unit mormot.db.raw.duckdb;

{
  *****************************************************************************

   Native DuckDB C API (v1.1+) - tested with DuckDB v1.4.1
    - DuckDB C API Types and Constants
    - TDuckDBLib Dynamic Library Loader
    - DuckDB Values Conversion Helpers
    - DuckDB Logical Type Cache
    - Quack API Constants and Exception
    - TQuackType Logical Types Description
    - TQuackVector and TQuackChunk Result Data Access
    - TQuackResponse Messages Decoding
    - Quack Requests Encoding

  *****************************************************************************

  Quack notes:
  - Quack is the DuckDB client/server protocol, see
    https://duckdb.org/docs/current/quack/overview and
    https://github.com/duckdb/duckdb-quack
  - messages are HTTP POST to /quack, with a "application/vnd.duckdb" body
    using the DuckDB "BinarySerializer" format: 16-bit field ids, LEB128
    variable-length integers, and every object ending with a 0xFFFF id
  - result rows are sent as DuckDB DataChunk vectors: they are decoded
    "in place", i.e. with no memory copy of the fixed-size values, the strings
    or the validity masks, which are accessed directly from the HTTP response
    buffer - so a TQuackResponse instance owns the memory of its chunks
  - the released quack extension (DuckDB 1.5) uses the protocol version 1, and
    its development branch uses the version 3: both are supported, the main
    differences being in CONNECTION_REQUEST, PREPARE_REQUEST, FETCH_REQUEST
    and FETCH_RESPONSE messages
  - no sockets here: see mormot.db.sql.duckdb for the TSqlDB* classes
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.json,
  mormot.lib.static,
  mormot.db.core,
  mormot.db.sql;


{ ************ DuckDB C API Types and Constants }

const
  /// the TSqlDBDefinition used for DuckDB, since its SQL is PostgreSQL-like
  // - please use this constant in the DuckDB code instead of dPostgreSQL, in
  // case a specific dialect would be needed in the future
  dDuckDB = dPostgreSQL;

type
  /// exception raised by the DuckDB providers
  ESqlDBDuckDB = class(ESqlDBException);

  /// DuckDB idx_t - always 64-bit, even on 32-bit platforms
  TDuckIdx = Int64;

  /// duckdb_state result code
  TDuckDBState = integer;

  /// duckdb_type enumeration (DUCKDB_TYPE_* constants)
  TDuckDBType = integer;

  // opaque DuckDB API handles - pointers to a struct (void* internal_ptr)
  TDuckDBDatabase          = type pointer;
  TDuckDBConnection        = type pointer;
  TDuckDBConfig            = type pointer;
  TDuckDBPreparedStatement = type pointer;
  TDuckDBLogicalType       = type pointer;
  TDuckDBDataChunk         = type pointer;
  TDuckDBVector            = type pointer;
  TDuckDBAppender          = type pointer;

  /// duckdb_result C struct - should be kept as-is, 48 bytes on 64-bit
  // - duckdb_fetch_chunk() expects it BY VALUE: the compiler will follow the
  // platform ABI (pointer to a copy on Win64, stack copy on SysV x86_64)
  TDuckDBResult = record
    deprecated_column_count: TDuckIdx;
    deprecated_row_count: TDuckIdx;
    deprecated_rows_changed: TDuckIdx;
    deprecated_columns: pointer;
    deprecated_error_message: PUtf8Char;
    internal_data: pointer;
  end;
  PDuckDBResult = ^TDuckDBResult;

  /// duckdb_string_t internal VARCHAR/BLOB representation (stored as 16 bytes)
  // - inlined if Length <= 12, otherwise Prefix[] + Ptr
  // - weird definition, but without "packed" usage which is much slower on arm
  TDuckDBStringT = record
    Length: cardinal;
    Data: array[0..2] of cardinal;
  end;
  PDuckDBStringT = ^TDuckDBStringT;

  /// duckdb_hugeint / duckdb_uhugeint 128-bit integers
  // - no "packed" usage which is much slower on arm
  TDuckDBHugeInt = record
    Lower: Int64; // actually uint64_t
    Upper: Int64; // int64_t for hugeint, uint64_t for uhugeint
  end;
  PDuckDBHugeInt = ^TDuckDBHugeInt;

  /// duckdb_interval (stored as 16 bytes)
  // - no "packed" usage which is much slower on arm
  TDuckDBInterval = record
    Months: integer;
    Days: integer;
    Micros: Int64;
  end;
  PDuckDBInterval = ^TDuckDBInterval;

  /// duckdb_list_entry as two 64-bit integers (stored as 16 bytes)
  TDuckDBListEntry = record
    Offset: Int64;
    Length: Int64;
  end;
  PDuckDBListEntry = ^TDuckDBListEntry;

  /// typed access to DuckDB vector data - no pointer math needed
  TDuckI8Array     = array[0..MaxInt div 8] of ShortInt;
  PDuckI8Array     = ^TDuckI8Array;
  TDuckI16Array    = array[0..MaxInt div 8] of SmallInt;
  PDuckI16Array    = ^TDuckI16Array;
  TDuckU16Array    = array[0..MaxInt div 8] of word;
  PDuckU16Array    = ^TDuckU16Array;
  TDuckI32Array    = array[0..MaxInt div 8] of integer;
  PDuckI32Array    = ^TDuckI32Array;
  TDuckU32Array    = array[0..MaxInt div 8] of cardinal;
  PDuckU32Array    = ^TDuckU32Array;
  TDuckI64Array    = array[0..MaxInt div 16] of Int64;
  PDuckI64Array    = ^TDuckI64Array;
  TDuckSingleArray = array[0..MaxInt div 8] of single;
  PDuckSingleArray = ^TDuckSingleArray;
  TDuckDoubleArray = array[0..MaxInt div 16] of double;
  PDuckDoubleArray = ^TDuckDoubleArray;
  TDuckU8Array     = array[0..MaxInt div 8] of byte;
  PDuckU8Array     = ^TDuckU8Array;

const
  DuckDBSuccess = 0;
  DuckDBError   = 1;

  DUCKDB_TYPE_INVALID         = 0;
  DUCKDB_TYPE_BOOLEAN         = 1;
  DUCKDB_TYPE_TINYINT         = 2;
  DUCKDB_TYPE_SMALLINT        = 3;
  DUCKDB_TYPE_INTEGER         = 4;
  DUCKDB_TYPE_BIGINT          = 5;
  DUCKDB_TYPE_UTINYINT        = 6;
  DUCKDB_TYPE_USMALLINT       = 7;
  DUCKDB_TYPE_UINTEGER        = 8;
  DUCKDB_TYPE_UBIGINT         = 9;
  DUCKDB_TYPE_FLOAT           = 10;
  DUCKDB_TYPE_DOUBLE          = 11;
  DUCKDB_TYPE_TIMESTAMP       = 12;
  DUCKDB_TYPE_DATE            = 13;
  DUCKDB_TYPE_TIME            = 14;
  DUCKDB_TYPE_INTERVAL        = 15;
  DUCKDB_TYPE_HUGEINT         = 16;
  DUCKDB_TYPE_VARCHAR         = 17;
  DUCKDB_TYPE_BLOB            = 18;
  DUCKDB_TYPE_DECIMAL         = 19;
  DUCKDB_TYPE_TIMESTAMP_S     = 20;
  DUCKDB_TYPE_TIMESTAMP_MS    = 21;
  DUCKDB_TYPE_TIMESTAMP_NS    = 22;
  DUCKDB_TYPE_ENUM            = 23;
  DUCKDB_TYPE_LIST            = 24;
  DUCKDB_TYPE_STRUCT          = 25;
  DUCKDB_TYPE_MAP             = 26;
  DUCKDB_TYPE_UUID            = 27;
  DUCKDB_TYPE_UNION           = 28;
  DUCKDB_TYPE_BIT             = 29;
  DUCKDB_TYPE_TIME_TZ         = 30;
  DUCKDB_TYPE_TIMESTAMP_TZ    = 31;
  DUCKDB_TYPE_UHUGEINT        = 32;
  DUCKDB_TYPE_ARRAY           = 33;
  DUCKDB_TYPE_ANY             = 34;
  DUCKDB_TYPE_BIGNUM          = 35;
  DUCKDB_TYPE_SQLNULL         = 36;
  DUCKDB_TYPE_STRING_LITERAL  = 37;
  DUCKDB_TYPE_INTEGER_LITERAL = 38;
  DUCKDB_TYPE_TIME_NS         = 39;
  DUCKDB_TYPE_GEOMETRY        = 40;
  DUCKDB_TYPE_VARIANT         = 41;
  DUCKDB_TYPE_TIMESTAMP_TZ_NS = 42;

  /// DuckDB TIMESTAMP 'infinity' / '-infinity' special values
  DUCKDB_TIMESTAMP_INFINITY  = $7fffffffffffffff;
  DUCKDB_TIMESTAMP_NINFINITY = -$7fffffffffffffff;

{$ifdef OSWINDOWS}
  DUCKDB_LIBNAME = 'duckdb.dll';
{$else}
  {$ifdef OSDARWIN}
  DUCKDB_LIBNAME = 'libduckdb.dylib';
  {$else}
  DUCKDB_LIBNAME = 'libduckdb.so';
  {$endif OSDARWIN}
{$endif OSWINDOWS}


{ ************ TDuckDBLib Dynamic Library Loader }

type
  /// dynamic binding to the DuckDB C API
  // - all entries are resolved with the 'duckdb_' prefix
  // - the field order should match DUCKDB_ENTRIES[] below
  TDuckDBLib = class(TSynLibrary)
  public
    library_version: function: PUtf8Char; cdecl;
    open_ext: function(path: PUtf8Char; out database: TDuckDBDatabase;
      config: TDuckDBConfig; out_error: PPAnsiChar): TDuckDBState; cdecl;
    close: procedure(var database: TDuckDBDatabase); cdecl;
    connect: function(database: TDuckDBDatabase;
      out connection: TDuckDBConnection): TDuckDBState; cdecl;
    disconnect: procedure(var connection: TDuckDBConnection); cdecl;
    interrupt: procedure(connection: TDuckDBConnection); cdecl;
    create_config: function(out config: TDuckDBConfig): TDuckDBState; cdecl;
    set_config: function(config: TDuckDBConfig;
      name, option: PUtf8Char): TDuckDBState; cdecl;
    destroy_config: procedure(var config: TDuckDBConfig); cdecl;
    query: function(connection: TDuckDBConnection; sql: PUtf8Char;
      out_result: PDuckDBResult): TDuckDBState; cdecl;
    destroy_result: procedure(res: PDuckDBResult); cdecl;
    column_name: function(res: PDuckDBResult; col: TDuckIdx): PUtf8Char; cdecl;
    column_type: function(res: PDuckDBResult; col: TDuckIdx): TDuckDBType; cdecl;
    column_logical_type: function(res: PDuckDBResult;
      col: TDuckIdx): TDuckDBLogicalType; cdecl;
    column_count: function(res: PDuckDBResult): TDuckIdx; cdecl;
    rows_changed: function(res: PDuckDBResult): TDuckIdx; cdecl;
    result_error: function(res: PDuckDBResult): PUtf8Char; cdecl;
    // C prototype is duckdb_fetch_chunk(duckdb_result result) i.e. BY VALUE
    fetch_chunk: function(res: TDuckDBResult): TDuckDBDataChunk; cdecl;
    data_chunk_get_size: function(chunk: TDuckDBDataChunk): TDuckIdx; cdecl;
    data_chunk_get_vector: function(chunk: TDuckDBDataChunk;
      col: TDuckIdx): TDuckDBVector; cdecl;
    destroy_data_chunk: procedure(var chunk: TDuckDBDataChunk); cdecl;
    vector_get_data: function(vector: TDuckDBVector): pointer; cdecl;
    vector_get_validity: function(vector: TDuckDBVector): PDuckI64Array; cdecl;
    list_vector_get_child: function(vector: TDuckDBVector): TDuckDBVector; cdecl;
    array_vector_get_child: function(vector: TDuckDBVector): TDuckDBVector; cdecl;
    struct_vector_get_child: function(vector: TDuckDBVector;
      index: TDuckIdx): TDuckDBVector; cdecl;
    get_type_id: function(lt: TDuckDBLogicalType): TDuckDBType; cdecl;
    decimal_width: function(lt: TDuckDBLogicalType): byte; cdecl;
    decimal_scale: function(lt: TDuckDBLogicalType): byte; cdecl;
    decimal_internal_type: function(lt: TDuckDBLogicalType): TDuckDBType; cdecl;
    enum_internal_type: function(lt: TDuckDBLogicalType): TDuckDBType; cdecl;
    enum_dictionary_size: function(lt: TDuckDBLogicalType): cardinal; cdecl;
    enum_dictionary_value: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): PUtf8Char; cdecl;
    list_type_child_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    array_type_child_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    array_type_array_size: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    struct_type_child_count: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    struct_type_child_name: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): PUtf8Char; cdecl;
    struct_type_child_type: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): TDuckDBLogicalType; cdecl;
    map_type_key_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    map_type_value_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    union_type_member_count: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    union_type_member_name: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): PUtf8Char; cdecl;
    union_type_member_type: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): TDuckDBLogicalType; cdecl;
    destroy_logical_type: procedure(var lt: TDuckDBLogicalType); cdecl;
    prepare: function(connection: TDuckDBConnection; query: PUtf8Char;
      out stmt: TDuckDBPreparedStatement): TDuckDBState; cdecl;
    destroy_prepare: procedure(var stmt: TDuckDBPreparedStatement); cdecl;
    prepare_error: function(stmt: TDuckDBPreparedStatement): PUtf8Char; cdecl;
    nparams: function(stmt: TDuckDBPreparedStatement): TDuckIdx; cdecl;
    param_type: function(stmt: TDuckDBPreparedStatement;
      index: TDuckIdx): TDuckDBType; cdecl;
    clear_bindings: function(stmt: TDuckDBPreparedStatement): TDuckDBState; cdecl;
    bind_boolean: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      value: boolean): TDuckDBState; cdecl;
    bind_int64: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      value: Int64): TDuckDBState; cdecl;
    bind_double: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      value: double): TDuckDBState; cdecl;
    bind_timestamp: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      micros: Int64): TDuckDBState; cdecl;
    bind_varchar_length: function(stmt: TDuckDBPreparedStatement;
      index: TDuckIdx; value: PUtf8Char; length: TDuckIdx): TDuckDBState; cdecl;
    bind_blob: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      data: pointer; length: TDuckIdx): TDuckDBState; cdecl;
    bind_null: function(stmt: TDuckDBPreparedStatement;
      index: TDuckIdx): TDuckDBState; cdecl;
    execute_prepared: function(stmt: TDuckDBPreparedStatement;
      out_result: PDuckDBResult): TDuckDBState; cdecl;
    free_: procedure(ptr: pointer); cdecl; // duckdb_free - renamed to not hide TObject.Free
    // appender API - resolved as optional entries
    appender_create: function(connection: TDuckDBConnection;
      schema, table: PUtf8Char; out appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_error: function(appender: TDuckDBAppender): PUtf8Char; cdecl;
    appender_flush: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_close: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_destroy: function(var appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_end_row: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    append_bool: function(appender: TDuckDBAppender; value: boolean): TDuckDBState; cdecl;
    append_int64: function(appender: TDuckDBAppender; value: Int64): TDuckDBState; cdecl;
    append_double: function(appender: TDuckDBAppender; value: double): TDuckDBState; cdecl;
    append_timestamp: function(appender: TDuckDBAppender;
      micros: Int64): TDuckDBState; cdecl;
    append_varchar_length: function(appender: TDuckDBAppender; value: PUtf8Char;
      length: TDuckIdx): TDuckDBState; cdecl;
    append_blob: function(appender: TDuckDBAppender; data: pointer;
      length: TDuckIdx): TDuckDBState; cdecl;
    append_null: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    /// TRUE if the appender API was successfully resolved
    HasAppender: boolean;
    /// return the DuckDB library version as text, e.g. 'v1.4.1'
    Version: RawUtf8;
    /// load the library and resolve all entries
    // - raise ESqlDBDuckDB on failure
    constructor Create; override;
    /// raise ESqlDBDuckDB with the error message of a duckdb_result
    procedure RaiseResultError(res: PDuckDBResult; const ctxt: RawUtf8);
    /// low-level execution of a SQL query, raising ESqlDBDuckDB on error
    // - can execute several statements, separated by ';'
    // - returns the number of changed rows of the (last) statement
    function Exec(conn: TDuckDBConnection; const sql: RawUtf8): Int64;
  end;

var
  /// the DuckDB library file name(s) to be loaded
  // - you may set e.g. a full path before the first connection
  DuckDBLibraryName: TFileName = DUCKDB_LIBNAME;

  /// global access to the DuckDB C API
  // - initialized on need by DuckDBLibraryInitialize
  DuckDB: TDuckDBLib;

/// load the DuckDB library, if not already done
// - raise ESqlDBDuckDB if the library is not available
procedure DuckDBLibraryInitialize;

/// return TRUE if the DuckDB library can be loaded
function DuckDBLibraryAvailable: boolean;


{ ************ DuckDB Values Conversion Helpers  }

/// return the text pointer and length of a duckdb_string_t
// - warning: len should be defined as an exact integer type, not a PtrInt
function DuckStringPtr(s: PDuckDBStringT; var len: integer): PUtf8Char;
  {$ifdef HASINLINE} inline; {$endif}

/// return the address of the duckdb_string_t at a given row of a vector
function DuckStringAt(data: pointer; row: PtrInt): PDuckDBStringT;
  {$ifdef HASINLINE} inline; {$endif}

/// convert a signed/unsigned hugeint into a text buffer
// - Scale=0 for HUGEINT/UHUGEINT, 0..38 for DECIMAL
// - returns the text length, excluding the trailing #0
// - fill directly a TTemp47 stack structure with no heap allocation
function DuckHugeIntToTextBuf(const v: TDuckDBHugeInt;
  Signed: boolean; Scale: integer; var Buffer: TTemp47): PtrInt;

/// convert a signed 128-bit hugeint into its decimal RawUtf8 text
// - default Scale=0 for HUGEINT/UHUGEINT, 0..38 for DECIMAL
procedure DuckHugeIntToText(const v: TDuckDBHugeInt; Signed: boolean;
  var result: RawUtf8; Scale: integer = 0);

/// return TRUE if the hugeint fits into an Int64
function DuckHugeIntIsInt64(var v: TDuckDBHugeInt; Signed: boolean): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// convert a DuckDB DATE into Y/M/D (years 0000-03-01 through 9999-12-31)
procedure DuckDaysToYmd(Days: integer; out Y, M, D: integer);

/// convert a DuckDB DATE into ISO-8601 text (years 0000-03-01 .. 9999)
procedure DuckDateToText(Days: integer; var result: RawUtf8);

/// convert a DuckDB TIME (micros since midnight) into 'hh:mm:ss[.ffffff]'
procedure DuckTimeToText(Micros: Int64; var result: RawUtf8);

/// convert a DuckDB TIMESTAMP (micros since 1970-01-01) into
// 'YYYY-MM-DD hh:mm:ss[.ffffff]'
procedure DuckTimestampToText(Micros: Int64; var result: RawUtf8);

/// convert a DuckDB TIMESTAMP (micros since 1970-01-01) into a TDateTime
function DuckTimestampToDateTime(Micros: Int64): TDateTime;

/// convert a TDateTime into a DuckDB TIMESTAMP (micros since 1970-01-01)
function DateTimeToDuckTimestamp(Value: TDateTime): Int64;

/// convert a DuckDB INTERVAL into its DuckDB-like text representation
procedure DuckIntervalToText(const v: TDuckDBInterval; var result: RawUtf8);

/// convert a DuckDB UUID (hugeint with flipped sign bit) into a text buffer
function DuckUuidToTextBuf(const v: TDuckDBHugeInt; var Buffer: TTemp47): PtrInt;

/// convert a DuckDB UUID (hugeint with flipped sign bit) into its text
procedure DuckUuidToText(const v: TDuckDBHugeInt; var result: RawUtf8);

// reconstruct regular TGuid fields from a DuckDB UUID to match expected text output
procedure DuckUuidToGuid(v: PDuckDBHugeInt; g: PGuid);

/// convert a DuckDB BIT string_t content into '0101...' text
procedure DuckBitToText(P: PDuckU8Array; Len: integer; var result: RawUtf8);

/// decode a DuckDB TIME_TZ 64-bit value into micros and offset in seconds
procedure DuckTimeTzDecode(Bits: Int64; out Micros: Int64; out OffsetSec: integer);
  {$ifdef HASINLINE} inline; {$endif}

/// convert a DuckDB TIME_TZ 64-bit value into 'hh:mm:ss[.ffffff]+hh[:mm]'
procedure DuckTimeTzToText(Bits: Int64; var result: RawUtf8);

/// convert a DuckDB TIMESTAMP_S/MS/NS value into microseconds
function DuckTimestampMicros(TypeId: TDuckDBType; Value: Int64): Int64;

const
  /// powers of 10 up to the Int64 limit, e.g. to scale a DECIMAL mantissa
  DUCKDB_POW10: array[0..18] of Int64 = (
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000);

/// the size in bytes of a DuckDB fixed-size value, 0 for variable-size types
// - Internal is the storage type of a DECIMAL or an ENUM
function DuckFixedSize(TypeId, Internal: TDuckDBType): PtrInt;

/// decode a BOOLEAN, DATE or integer value as Int64, from a possibly unaligned P
// - UBIGINT is returned as its two's complement Int64 bits
function DuckFixedInt(TypeId: TDuckDBType; P: PAnsiChar): Int64;

/// convert a fixed-size value into a text buffer, with no memory allocation
// - handles BOOLEAN, integers, HUGEINT, DECIMAL, FLOAT, DOUBLE and UUID
// - returns the text length, 0 for SQLNULL, or -1 for other types
// - P may be unaligned: it is the address of the value, not of its vector
function DuckFixedToTextBuf(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; var Buffer: TTemp47): PtrInt;

/// convert a fixed-size value into its DuckDB text representation
// - handles DuckFixedToTextBuf() types, dates, times, timestamps and INTERVAL
// - returns '' for other types, e.g. ENUM which needs its dictionary
procedure DuckFixedToText(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; var result: RawUtf8);

/// append a fixed-size value as JSON: number, boolean or quoted text
// - handles all DuckFixedToText() types but not ENUM
procedure DuckFixedToJson(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; W: TJsonWriter);

type
  /// result of DuckFixedToInt64()
  TDuckConvert = (
    dcFallback,
    dcOk,
    dcOverflow);

/// convert a fixed-size value into an Int64
// - handles integers, BOOLEAN, HUGEINT, DECIMAL, floats, dates and times:
// the 64-bit date/time kinds are returned as their raw integer value
// - returns dcFallback if the value type should be converted from its text,
// or dcOverflow if a 128-bit value does not fit in an Int64
function DuckFixedToInt64(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: Int64): TDuckConvert;

/// convert a fixed-size value into a double
// - handles BOOLEAN, integers (but UBIGINT), FLOAT, DOUBLE and DECIMAL
// - returns false if the value type should be converted from its text
function DuckFixedToDouble(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: double): boolean;

/// convert a fixed-size value into a currency, with no precision loss for
// DECIMAL with up to 4 decimals
// - returns false if the value type should be converted from its text
function DuckFixedToCurrency(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: currency): boolean;

/// convert a fixed-size date, time or timestamp value into a TDateTime
// - returns false if the value type should be converted from its text
function DuckFixedToDateTime(TypeId: TDuckDBType; P: PAnsiChar;
  out Value: TDateTime): boolean;


{ ************ DuckDB Logical Type Cache }

type
  PDuckDBTypeInfoArray = ^TDuckDBTypeInfoArray;
  PDuckDBTypeInfo = ^TDuckDBTypeInfo;

  /// a DuckDB logical type, parsed once with all its nested types
  // - to avoid calling the DuckDB C API for every row of a result set
  // - initialized by DuckDBTypeInfoInit() and released by DuckDBTypeInfoDone()
  {$ifdef USERECORDWITHMETHODS}
  TDuckDBTypeInfo = record
  {$else}
  TDuckDBTypeInfo = object
  {$endif USERECORDWITHMETHODS}
  public
    /// 32-bit ordinal number corresponding to DUCKDB_TYPE_* constants
    TypeId: TDuckDBType;
    /// internal storage type of a DECIMAL or an ENUM
    Internal: TDuckDBType;
    /// number of decimals of a DECIMAL, or fixed size of an ARRAY
    Scale: integer;
    /// how many items are actually stored in Child[]
    ChildCount: integer;
    /// size in bytes of a fixed-size value, 0 for variable-size types
    Size: PtrInt;
    /// the DuckDB logical type handle, owned by this structure
    Handle: TDuckDBLogicalType;
    /// STRUCT field names, or ENUM values (cached on first use)
    Names: TRawUtf8DynArray;
    /// nested types of a LIST/ARRAY (item), MAP (key, value),
    // STRUCT (fields) or UNION (members)
    Child: PDuckDBTypeInfoArray;
    /// parse a DuckDB logical type and all its nested types, once
    // - the supplied Handle is owned by Info, and released by DuckDBTypeInfoDone()
    procedure Init(aHandle: TDuckDBLogicalType);
    /// release the DuckDB logical type handles of a DuckDBTypeInfoInit() structure
    procedure Done;
    /// allocate Child[0 .. ChildCount - 1] items
    function Nest(n: PtrInt = 1): PDuckDBTypeInfo;
    /// retrieve the text of an ENUM value, caching all the ENUM dictionary
    procedure EnumValue(Index: PtrInt; var Result: RawUtf8);
    /// return true if a type is a VARIANT, or contains a VARIANT as nested type
    function HasVariant: boolean;
  end;
  TDuckDBTypeInfoArray = array[word] of TDuckDBTypeInfo;

/// wrap a SELECT statement to retrieve its VARIANT columns as JSON
// - the VARIANT binary layout is not decoded by our drivers: DuckDB converts
// them to JSON, e.g. 'SELECT #1 AS "a", NULLIF(#2::JSON, 'null'::JSON) AS "b" FROM (sql)'
// - positional #n references allow duplicated column names, and a VARIANT NULL
// is always returned as SQL NULL (DuckDB 1.5 returned a JSON 'null')
// - IsVariant[] flags the columns to be converted
function DuckDBVariantSql(const Sql: RawUtf8; const Names: TRawUtf8DynArray;
  const IsVariant: TBytes): RawUtf8;

/// convert a DuckDB type into the corresponding mormot.db field type
function DuckDBTypeToField(TypeId: TDuckDBType; Scale: integer): TSqlDBFieldType;



{ ************ Quack API Constants and Exception }

type
  /// exception raised by the Quack client and its protocol decoding
  ESqlDBQuack = class(ESqlDBException);

const
  /// the latest Quack wire-protocol version implemented by this unit
  // - as used by the development version of the DuckDB quack extension
  QUACK_VERSION = 3;

  /// the Quack wire-protocol version of the quack extension of DuckDB 1.5
  // - this version has no lease, no batch numbers and no query cancellation
  QUACK_VERSION_1 = 1;

  /// default TCP port of a Quack server
  QUACK_DEFAULT_PORT = 9494;

  /// the HTTP URI path of the Quack server end point
  QUACK_HTTP_PATH = '/quack';

  /// the HTTP content-type of Quack requests and responses
  QUACK_CONTENT_TYPE = 'application/vnd.duckdb';

  /// Quack message types - as sent in the header of every message
  QUACK_CONNECTION_REQUEST  = 1;
  QUACK_CONNECTION_RESPONSE = 2;
  QUACK_PREPARE_REQUEST     = 3;
  QUACK_PREPARE_RESPONSE    = 4;
  QUACK_FETCH_REQUEST       = 7;
  QUACK_FETCH_RESPONSE      = 8;
  QUACK_SEND_DATA_REQUEST   = 9;
  QUACK_SUCCESS_RESPONSE    = 10;
  QUACK_DISCONNECT_MESSAGE  = 11;
  QUACK_CANCEL_REQUEST      = 12;
  QUACK_SEND_DATA_RESPONSE  = 14;
  QUACK_ACKNOWLEDGEMENT     = 15;
  QUACK_HEARTBEAT_REQUEST   = 16;
  QUACK_ERROR_RESPONSE      = 100;

  /// DuckDB LogicalTypeId values, as serialized by Quack
  QTYPE_SQLNULL          = 1;
  QTYPE_BOOLEAN          = 10;
  QTYPE_TINYINT          = 11;
  QTYPE_SMALLINT         = 12;
  QTYPE_INTEGER          = 13;
  QTYPE_BIGINT           = 14;
  QTYPE_DATE             = 15;
  QTYPE_TIME             = 16;
  QTYPE_TIMESTAMP_SEC    = 17;
  QTYPE_TIMESTAMP_MS     = 18;
  QTYPE_TIMESTAMP        = 19;
  QTYPE_TIMESTAMP_NS     = 20;
  QTYPE_DECIMAL          = 21;
  QTYPE_FLOAT            = 22;
  QTYPE_DOUBLE           = 23;
  QTYPE_CHAR             = 24;
  QTYPE_VARCHAR          = 25;
  QTYPE_BLOB             = 26;
  QTYPE_INTERVAL         = 27;
  QTYPE_UTINYINT         = 28;
  QTYPE_USMALLINT        = 29;
  QTYPE_UINTEGER         = 30;
  QTYPE_UBIGINT          = 31;
  QTYPE_TIMESTAMP_TZ     = 32;
  QTYPE_TIMESTAMP_TZ_NS  = 33;
  QTYPE_TIME_TZ          = 34;
  QTYPE_TIME_NS          = 35;
  QTYPE_BIT              = 36;
  QTYPE_BIGNUM           = 39;
  QTYPE_UHUGEINT         = 49;
  QTYPE_HUGEINT          = 50;
  QTYPE_POINTER          = 51;
  QTYPE_UUID             = 54;
  QTYPE_GEOMETRY         = 60;
  QTYPE_STRUCT           = 100;
  QTYPE_LIST             = 101;
  QTYPE_MAP              = 102;
  QTYPE_ENUM             = 104;
  QTYPE_UNION            = 107;
  QTYPE_ARRAY            = 108;
  QTYPE_VARIANT          = 109;
  QTYPE_TUPLE            = 110;


{ ************ TQuackType Logical Types Description }

type
  /// how the values of a given type are stored in a Quack vector
  TQuackStorage = (
    qsUnsupported,
    qsFixed,
    qsString,
    qsStruct,
    qsList,
    qsArray);

  /// a DuckDB logical type, as sent in the Quack result description
  // - owned by its TQuackResponse, including its Children
  TQuackType = class
  public
    /// the DuckDB LogicalTypeId, e.g. QTYPE_INTEGER
    Id: byte;
    /// how the values are stored in the vectors
    Storage: TQuackStorage;
    /// the number of bytes of each value for qsFixed storage
    ElemSize: byte;
    /// the equivalent DUCKDB_TYPE_* C API value (as in mormot.db.raw.duckdb)
    DuckType: TDuckDBType;
    /// DECIMAL width
    Width: byte;
    /// DECIMAL scale
    Scale: byte;
    /// number of items of an ARRAY
    ArraySize: cardinal;
    /// optional type name, e.g. 'JSON'
    Alias: RawUtf8;
    /// ENUM values
    EnumValues: TRawUtf8DynArray;
    /// STRUCT and UNION members names
    ChildNames: TRawUtf8DynArray;
    /// STRUCT and UNION members types - or [0] for LIST, MAP and ARRAY
    Children: array of TQuackType;
    /// the TSqlDBFieldType used to publish this column
    function FieldType: TSqlDBFieldType;
    /// guess Storage, ElemSize and DuckType from Id, Width and EnumValues
    procedure SetStorage;
    /// return true if this type is a VARIANT, or contains a VARIANT
    function HasVariant: boolean;
    /// decode a 8/16/32-bit enumerate value from a memory buffer
    function EnumIndexAt(D: PAnsiChar): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// compute the corresponding raw DuckDB integer type from its ElemSize
    function ElemSizeInternal: TDuckDBType;
      {$ifdef HASINLINE} inline; {$endif}
  end;
  PQuackType = ^TQuackType;
  TQuackTypeDynArray = array of TQuackType;


{ ************ TQuackVector and TQuackChunk Result Data Access }

type
  /// how a TQuackVector maps its rows to the stored values
  TQuackVectorMode = (
    qvmFlat,
    qvmConstant,
    qvmDictionary);

  /// one column of a TQuackChunk
  // - the content is a view over the HTTP response buffer, with no copy
  TQuackVector = class
  protected
    fOwned: RawByteString;
  public
    /// the type of this vector
    VType: TQuackType;
    /// how rows are mapped to the stored values
    Mode: TQuackVectorMode;
    /// the number of rows of this vector
    Count: PtrInt;
    /// the number of stored values (differs from Count if not qvmFlat)
    StoredCount: PtrInt;
    /// bit mask of the stored values validity - nil if there is no NULL
    Validity: PByteArray;
    /// fixed-size values (may be unaligned)
    Data: PAnsiChar;
    /// StoredCount offsets, then StoredCount lengths of the strings
    StrOffs: PCardinalArray;
    /// strings content, as referenced by StrOffs
    StrBytes: PAnsiChar;
    /// qvmDictionary 32-bit selection indexes (may be unaligned)
    Sel: PAnsiChar;
    /// LIST entries (offset, length), for each stored value
    Entries: PDuckDBListEntry;
    /// the child of a LIST or ARRAY
    Child: TQuackVector;
    /// the members of a STRUCT
    Children: array of TQuackVector;
    /// release the children and any owned memory
    destructor Destroy; override;
    /// return the stored value index of a given row
    function Stored(Row: PtrInt): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    /// return true if a stored value is not NULL
    function IsStoredValid(Index: PtrInt): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// return true if the row is not NULL
    function IsValid(Row: PtrInt): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// return a pointer to a stored fixed-size value
    function DataAt(Index: PtrInt): PAnsiChar;
      {$ifdef HASINLINE} inline; {$endif}
    /// return a pointer and the length of a stored string
    function StringAt(Index: PtrInt; out Len: integer): PUtf8Char;
    /// return a pointer to a VType.Storage = qsString text, or nil
    // - makes a local #0-ended copy to Temp for safety
    // - used e.g. as fallback for ColumnInt/ColumnDouble/ColumnCurrency() methods
    function TextAt(Index: PtrInt; var Temp: TTemp47): PUtf8Char;
  end;
  TQuackVectorDynArray = array of TQuackVector;

  /// one set of up to 2048 rows of a Quack result
  TQuackChunk = class
  public
    /// number of rows
    RowCount: PtrInt;
    /// one vector per column
    Vectors: TQuackVectorDynArray;
    /// release all vectors
    destructor Destroy; override;
  end;
  TQuackChunkDynArray = array of TQuackChunk;

/// read a 64-bit floating-point value which may be unaligned in the response
function QuackDouble(P: pointer): double;
  {$ifdef HASSAFEINLINE} inline; {$endif}

/// read a 32-bit floating-point value which may be unaligned in the response
function QuackSingle(P: pointer): double;
  {$ifdef HASSAFEINLINE} inline; {$endif}


{ ************ TQuackResponse Messages Decoding }

type
  /// a decoded Quack response message
  // - owns the HTTP response body, and all types and chunks it contains
  TQuackResponse = class
  protected
    fBody: RawByteString;
    fTypePool: TQuackTypeDynArray;
    fTypePoolCount: integer;
    fLastTypes: TQuackTypeDynArray;
    fLastTypesStart: PAnsiChar;
    fLastTypesLen: PtrInt;
    fTypeDepth: integer;
    fVersion: cardinal;
    function NewType: TQuackType;
    function ReadType(var R: TFastReader): TQuackType;
    function ReadTypes(var R: TFastReader): TQuackTypeDynArray;
    function ReadChunk(var R: TFastReader): TQuackChunk;
    function ReadVector(var R: TFastReader; T: TQuackType;
      Count: PtrInt): TQuackVector;
    procedure ReadVectorBody(var R: TFastReader; V: TQuackVector; Count: PtrInt);
    procedure ReadChunks(var R: TFastReader);
    procedure ReadPrepare(var R: TFastReader);
    procedure ReadFetch(var R: TFastReader);
    procedure ReadConnection(var R: TFastReader);
    procedure ReadError(var R: TFastReader);
  public
    /// the QUACK_*_RESPONSE type of this message
    MessageType: byte;
    /// the server-side connection identifier
    ConnectionId: RawUtf8;
    /// CONNECTION_RESPONSE: the server DuckDB version
    ServerVersion: RawUtf8;
    /// CONNECTION_RESPONSE: the server platform
    ServerPlatform: RawUtf8;
    /// CONNECTION_RESPONSE: the server Quack protocol version
    QuackVersion: cardinal;
    /// CONNECTION_RESPONSE: the heartbeat lease duration in seconds
    HeartbeatSeconds: cardinal;
    /// PREPARE_RESPONSE: the result columns types
    ColumnTypes: TQuackTypeDynArray;
    /// PREPARE_RESPONSE: the result columns names
    ColumnNames: TRawUtf8DynArray;
    /// PREPARE_RESPONSE: the columns which are, or contain, a VARIANT
    // - the VARIANT binary layout is not decoded: the response is not decoded
    // after the columns names, and the SQL should be wrapped by DuckDBVariantSql()
    // - nil if there is no VARIANT in the result
    VariantColumns: TBytes;
    /// PREPARE_RESPONSE: true if some FETCH_REQUEST are needed to get all rows
    NeedsMoreFetch: boolean;
    /// PREPARE_RESPONSE: the query identifier
    QueryUuid: TDuckDBHugeInt;
    /// FETCH_RESPONSE: the batch number of the chunks (from 1)
    BatchIndex: Int64;
    /// FETCH_RESPONSE: the number of batches - only set on the last response
    TotalBatches: Int64;
    /// PREPARE_RESPONSE or FETCH_RESPONSE: the rows
    Chunks: TQuackChunkDynArray;
    /// ERROR_RESPONSE: the error message
    ErrorMessage: RawUtf8;
    /// ERROR_RESPONSE: the DuckDB exception type
    ErrorType: RawUtf8;
    /// ERROR_RESPONSE: true if the server database is not usable any more
    MustInvalidate: boolean;
    /// PREPARE_RESPONSE: true if the result is the "Count" of a DML statement
    function IsCountResult: boolean;
    /// decode a response HTTP body
    // - Version is the protocol version negotiated with the server
    // - raise ESqlDBQuack on any malformed content
    constructor Create(const Body: RawByteString;
      Version: cardinal = QUACK_VERSION);
    /// release all types and chunks
    destructor Destroy; override;
  end;


{ ************ Quack Requests Encoding }

/// encode a CONNECTION_REQUEST message
// - ClientId and HeartbeatSeconds are only sent for the protocol version 3
function QuackConnectionRequest(const Token, ClientId: RawUtf8;
  HeartbeatSeconds, Version: cardinal): RawByteString;

/// encode a PREPARE_REQUEST message, which also runs the SQL on the server
// - the QueryUuid is only sent for the protocol version 3
function QuackPrepareRequest(const ConnectionId, Sql: RawUtf8;
  const QueryUuid: TDuckDBHugeInt; Version: cardinal): RawByteString;

/// encode a FETCH_REQUEST message
// - BatchIndex starts at 1, and AckIndex is the last batch received - they are
// only sent for the protocol version 3
function QuackFetchRequest(const ConnectionId: RawUtf8;
  const QueryUuid: TDuckDBHugeInt;
  BatchIndex, AckIndex, Version: cardinal): RawByteString;

/// encode a DISCONNECT_MESSAGE or HEARTBEAT_REQUEST message
function QuackSimpleRequest(MessageType: byte;
  const ConnectionId: RawUtf8): RawByteString;

/// encode a CANCEL_REQUEST or ACKNOWLEDGEMENT message
function QuackQueryRequest(MessageType: byte; const ConnectionId: RawUtf8;
  const QueryUuid: TDuckDBHugeInt): RawByteString;

/// compute a random query identifier
procedure QuackNewQueryUuid(out QueryUuid: TDuckDBHugeInt);

/// parse a Quack server URI into its host name and port
// - recognize 'quack:host', 'quack:host:9494', 'quack://[::1]:9494' or 'host'
// - the port is QUACK_DEFAULT_PORT if not specified
// - returns false if the URI is not valid
function QuackUriParse(const Uri: RawUtf8; out Host: RawUtf8;
  out Port: integer): boolean;

/// return true for the local host names, which use HTTP instead of HTTPS
function QuackHostIsLocal(const Host: RawUtf8): boolean;



implementation


{ ************ TDuckDBLib Dynamic Library Loader }

const
  DUCKDB_ENTRIES: array[0..63] of PAnsiChar = (
    'library_version',
    'open_ext',
    'close',
    'connect',
    'disconnect',
    'interrupt',
    'create_config',
    'set_config',
    'destroy_config',
    'query',
    'destroy_result',
    'column_name',
    'column_type',
    'column_logical_type',
    'column_count',
    'rows_changed',
    'result_error',
    'fetch_chunk',
    'data_chunk_get_size',
    'data_chunk_get_vector',
    'destroy_data_chunk',
    'vector_get_data',
    'vector_get_validity',
    'list_vector_get_child',
    'array_vector_get_child',
    'struct_vector_get_child',
    'get_type_id',
    'decimal_width',
    'decimal_scale',
    'decimal_internal_type',
    'enum_internal_type',
    'enum_dictionary_size',
    'enum_dictionary_value',
    'list_type_child_type',
    'array_type_child_type',
    'array_type_array_size',
    'struct_type_child_count',
    'struct_type_child_name',
    'struct_type_child_type',
    'map_type_key_type',
    'map_type_value_type',
    'union_type_member_count',
    'union_type_member_name',
    'union_type_member_type',
    'destroy_logical_type',
    'prepare',
    'destroy_prepare',
    'prepare_error',
    'nparams',
    'param_type',
    'clear_bindings',
    'bind_boolean',
    'bind_int64',
    'bind_double',
    'bind_timestamp',
    'bind_varchar_length',
    'bind_blob',
    'bind_null',
    'execute_prepared',
    'free',
    nil, nil, nil, nil); // padding - ResolveAll stops at first nil

  DUCKDB_APPENDER_ENTRIES: array[0..13] of PAnsiChar = (
    'appender_create',
    'appender_error',
    'appender_flush',
    'appender_close',
    'appender_destroy',
    'appender_end_row',
    'append_bool',
    'append_int64',
    'append_double',
    'append_timestamp',
    'append_varchar_length',
    'append_blob',
    'append_null',
    nil);

constructor TDuckDBLib.Create;
begin
  inherited Create;
  TryFromExecutableFolder := true;
  TryLoadLibrary([DuckDBLibraryName, DUCKDB_LIBNAME], ESqlDBDuckDB);
  ResolveAll(@DUCKDB_ENTRIES, @@library_version, 'duckdb_', ESqlDBDuckDB);
  HasAppender := ResolveAll(@DUCKDB_APPENDER_ENTRIES, @@appender_create,
    'duckdb_', nil); // no exception if appender API is not supported
  Version := RawUtf8(library_version);
end;

procedure TDuckDBLib.RaiseResultError(res: PDuckDBResult; const ctxt: RawUtf8);
var
  E: Exception;
begin
  E := ESqlDBDuckDB.CreateUtf8('DuckDB % failed: %', [ctxt, result_error(res)]);
  destroy_result(res); // release result_error(res)
  raise E;
end;

function TDuckDBLib.Exec(conn: TDuckDBConnection; const sql: RawUtf8): Int64;
var
  res: TDuckDBResult;
  state: TDuckDBState;
  saved: cardinal;
begin
  FillCharFast(res, SizeOf(res), 0);
  saved := SetFpuFlags; // the C++ engine expects masked FPU exceptions
  try
    state := query(conn, pointer(sql), @res);
  finally
    ResetFpuFlags(saved);
  end;
  if state <> DuckDBSuccess then
    RaiseResultError(@res, sql);
  result := rows_changed(@res);
  destroy_result(@res);
end;

procedure DuckDBLibraryInitialize;
begin
  if DuckDB <> nil then
    exit;
  GlobalLock;
  try
    if DuckDB = nil then
      DuckDB := TDuckDBLib.Create;
  finally
    GlobalUnLock;
  end;
end;

function DuckDBLibraryAvailable: boolean;
begin
  try
    DuckDBLibraryInitialize;
    result := true;
  except
    result := false;
  end;
end;


{ ************ Value Conversion Helpers }

function DuckStringPtr(s: PDuckDBStringT; var len: integer): PUtf8Char;
begin
  len := s^.Length;
  if len <= SizeOf(s^.Data) then
    result := @s^.Data
  else
    result := PPointer(@s^.Data[1])^; // 64-bit aligned
end;

function DuckStringAt(data: pointer; row: PtrInt): PDuckDBStringT;
begin
  result := pointer(PAnsiChar(data) + row * SizeOf(TDuckDBStringT));
end;

procedure U128Negate(var v: THash128Rec);
  {$ifdef HASINLINE} inline; {$endif}
var
  lo: QWord;
begin
  lo := v.L;
  v.L := not lo + 1;
  v.H := not v.H + byte(lo = 0);
end;

function DivU128By1B(var u: THash128Rec): cardinal;
var
  r, cur: QWord;
  i: PtrInt;
begin // see mormot.db.nosql.bson's div128bits9digits()
  r := 0;
  for i := 3 downto 0 do
  begin
    cur := (r shl 32) or QWord(u.c[i]);
    if cur = 0 then
      continue;
    u.c[i] := cardinal(cur div 1000000000);
    r := cur - QWord(u.c[i]) * 1000000000;
  end;
  result := cardinal(r);
end;

function DuckHugeIntToTextBuf(const v: TDuckDBHugeInt;
  Signed: boolean; Scale: integer; var Buffer: TTemp47): PtrInt;
var
  u: THash128Rec;
  neg: boolean;
  i, digits: PtrInt;
  n, q: cardinal;
  p, first, last: PUtf8Char;
begin
  if Scale < 0 then
    Scale := 0
  else if Scale > 38 then
    ESqlDBDuckDB.RaiseUtf8('Invalid DuckDB decimal scale=%', [Scale]);
  u.L := v.Lower;
  u.H := v.Upper;
  neg := Signed and (v.Upper < 0);
  if neg then
    U128Negate(u);
  // reserve one byte for the terminating #0
  last := @Buffer[high(Buffer)];
  p := last - 1;
  repeat
    n := DivU128By1B(u);
    if (u.L = 0) and
       (u.H = 0) then
    begin
      repeat
        q := n div 10;
        p^ := AnsiChar(ord('0') + n - q * 10);
        dec(p);
        n := q;
      until n = 0;
      break;
    end;
    for i := 1 to 9 do
    begin
      q := n div 10;
      p^ := AnsiChar(ord('0') + n - q * 10);
      dec(p);
      n := q;
    end;
  until false;
  first := p + 1;
  digits := last - first;
  // apply the DECIMAL scale directly within the stack buffer
  if Scale > 0 then
  begin
    if digits > Scale then
    begin
      // move the integer part left to insert '.'
      MoveFast(first^, (first - 1)^, digits - Scale);
      dec(first);
      first[digits - Scale] := '.';
    end
    else
    begin
      // e.g. 12 with Scale=4 -> 0.0012
      p := first - 1;
      for i := 1 to Scale - digits do
      begin
        p^ := '0';
        dec(p);
      end;
      p^ := '.';
      dec(p);
      p^ := '0';
      first := p;
    end;
  end;
  if neg then
  begin
    dec(first);
    first^ := '-';
  end;
  result := last - first;
  MoveFast(first^, Buffer[0], result);
  Buffer[result] := #0;
end;

procedure DuckHugeIntToText(const v: TDuckDBHugeInt;
  Signed: boolean; var result: RawUtf8; Scale: integer);
var
  tmp: TTemp47;
  len: PtrInt;
begin
  len := DuckHugeIntToTextBuf(v, Signed, Scale, tmp);
  FastSetString(result, @tmp[0], len);
end;

function DuckHugeIntIsInt64(var v: TDuckDBHugeInt; Signed: boolean): boolean;
begin
  if Signed then
    result := ((v.Upper = 0) and (v.Lower >= 0)) or
              ((v.Upper = -1) and (v.Lower < 0))
  else
    result := (v.Upper = 0) and (v.Lower >= 0);
end;

const
  // TSynSystemTime.FromUnixTime supports year 0000 only from March onward
  // - all earlier dates and years >= 10000 are outside this unit's scope
  DUCKDB_FIRST_DAY           = -719468;   // 0000-03-01
  DUCKDB_FIRST_TDATETIME_DAY = -719162;   // 0001-01-01
  DUCKDB_LAST_DAY            = 2932896;   // 9999-12-31

procedure DuckCheckDay(Days: Int64; ForDateTime: boolean = false);
begin
  if (Days < DUCKDB_FIRST_DAY) or
     (Days > DUCKDB_LAST_DAY) or
     (ForDateTime and (Days < DUCKDB_FIRST_TDATETIME_DAY)) then
    ESqlDBDuckDB.RaiseUtf8('DuckDB date=% outside TSynSystemTime range', [Days]);
end;

procedure FloorDivMod(const a, b: Int64; var q, r: Int64);
  {$ifdef HASINLINE} inline; {$endif}
begin
  q := a div b;
  r := a mod b;
  if r >= 0 then
    exit;
  dec(q);
  inc(r, b);
end;

procedure DuckDaysToYmd(Days: integer; out Y, M, D: integer);
var
  st: TSynSystemTime;
begin
  DuckCheckDay(Days);
  st.FromUnixTime(Int64(Days) * SecsPerDay);
  Y := st.Year;
  M := st.Month;
  D := st.Day;
end;

procedure DuckDateToText(Days: integer; var result: RawUtf8);
var
  st: TSynSystemTime;
begin
  if Days = $7fffffff then
    result := 'infinity'
  else if Days = -$7fffffff then
    result := '-infinity'
  else
  begin
    DuckCheckDay(Days);
    st.FromUnixTime(Int64(Days) * SecsPerDay);
    st.ToIsoDate(result);
  end;
end;

procedure DuckAppendMicroFraction(Micro: Int64; var result: RawUtf8);
var
  tmp: TShort31;
begin
  // TSynSystemTime handles milliseconds; preserve DuckDB's full microseconds
  if Micro = 0 then
    exit;
  tmp[0] := #0;
  AppendShortQWord(Micro + MicroSecsPerSec, tmp);
  tmp[1] := '.'; // replace first '1' = MicroSecsPerSec
  while tmp[length(tmp)] = '0' do
    dec(tmp[0]);
  Append(result, @tmp[1], length(tmp));
end;

procedure DuckTimeToText(Micros: Int64; var result: RawUtf8);
var
  st: TSynSystemTime;
  u: QWord;
begin
  // DuckDB INTERVAL uses this formatter too, including negative/>24h values
  if Micros < 0 then
    u := QWord(-(Micros + 1)) + 1 // also safe for Low(Int64)
  else
    u := QWord(Micros);
  st.FromMS(PtrUInt((u mod QWord(MicroSecsPerDay)) div 1000));
  st.ToIsoTime(result, '');
  if u >= QWord(MicroSecsPerDay) then
    result := Int64ToUtf8(Int64(u div MicroSecsPerHour)) +
              copy(result, 3, maxInt);
  DuckAppendMicroFraction(Int64(u mod MicroSecsPerSec), result);
  if Micros < 0 then
    result := '-' + result;
end;

procedure DuckTimestampToText(Micros: Int64; var result: RawUtf8);
var
  st: TSynSystemTime;
  secs, micro, days, rem: Int64;
begin
  if Micros = DUCKDB_TIMESTAMP_INFINITY then
    result := 'infinity'
  else if Micros = DUCKDB_TIMESTAMP_NINFINITY then
    result := '-infinity'
  else
  begin
    // Floor division correctly handles timestamps before the Unix epoch
    FloorDivMod(Micros, MicroSecsPerSec, secs, micro);
    FloorDivMod(secs, SecsPerDay, days, rem);
    DuckCheckDay(days);
    st.FromUnixTime(secs);
    st.ToIsoDateTime(result, ' ');
    if micro <> 0 then
      DuckAppendMicroFraction(micro, result);
  end;
end;

function DuckTimestampToDateTime(Micros: Int64): TDateTime;
var
  st: TSynSystemTime;
  ms, micro, days, rem: Int64;
begin
  FloorDivMod(Micros, MicroSecsPerMilliSec, ms, micro);
  if micro >= 500 then
    inc(ms); // nearest millisecond (TDateTime cannot store microseconds)
  FloorDivMod(ms, MSecsPerDay, days, rem);
  DuckCheckDay(days, {ForDateTime=}true);
  st.FromUnixMsTime(ms);
  result := st.ToDateTime;
  // TSynSystemTime.ToDateTime uses a positive time fraction. Delphi instead
  // requires a negative fraction when the date part is before 1899-12-30
  if days + UnixDelta < 0 then
    result := result - 2 * (rem / MSecsPerDay);
end;

function DateTimeToDuckTimestamp(Value: TDateTime): Int64;
var
  st: TSynSystemTime;
begin
  st.FromDateTime(Value);
  if (st.Year = 0) or
     (st.Year > 9999) then
    ESqlDBDuckDB.RaiseUtf8('TDateTime year=% outside 1..9999', [st.Year]);
  result := st.ToUnixMsTime * MicroSecsPerMilliSec;
end;

procedure AddIntervalPart(var result: ShortString; n: integer;
  const unitname: ShortString);
begin
  if n = 0 then
    exit;
  if result[0] <> #0 then
    AppendShortChar(' ', @result);
  AppendShortInt64(n, result);
  AppendShortChar(' ', @result);
  AppendShort(unitname, result);
  if (n > 1) or
     (n < -1) then
   AppendShortChar('s', @result);
end;

procedure DuckIntervalToText(const v: TDuckDBInterval; var result: RawUtf8);
var
  t: pointer;
  tmp: ShortSTring;
begin
  tmp[0] := #0;
  AddIntervalPart(tmp, v.Months div 12, 'year');
  AddIntervalPart(tmp, v.Months mod 12, 'month');
  AddIntervalPart(tmp, v.Days, 'day');
  if (v.Micros <> 0) or
     (tmp[0] = #0) then
  begin
    if tmp[0] <> #0 then
      AppendShortChar(' ', @tmp);
    t := nil;
    DuckTimeToText(v.Micros, RawUtf8(t));
    AppendShortAnsi7String(RawUtf8(t), tmp);
    FastAssignNew(t);
  end;
  ShortStringToAnsi7String(tmp, result);
end;

procedure DuckUuidToGuid(v: PDuckDBHugeInt; g: PGuid);
var
  src: PByteArray absolute v;
  dst: PByteArray absolute g;
begin
  dst^[0]  := src^[12];
  dst^[1]  := src^[13];
  dst^[2]  := src^[14];
  dst^[3]  := src^[15] xor $80;
  dst^[4]  := src^[10];
  dst^[5]  := src^[11];
  dst^[6]  := src^[8];
  dst^[7]  := src^[9];
  dst^[8]  := src^[7];
  dst^[9]  := src^[6];
  dst^[10] := src^[5];
  dst^[11] := src^[4];
  dst^[12] := src^[3];
  dst^[13] := src^[2];
  dst^[14] := src^[1];
  dst^[15] := src^[0];
end;

function DuckUuidToTextBuf(const v: TDuckDBHugeInt; var Buffer: TTemp47): PtrInt;
var
  g: TGuid;
  p: PUtf8Char;
begin
  // reconstruct native TGuid fields in their correct order
  DuckUuidToGuid(@v, @g);
  // same 8-4-4-4-12 format as DuckDB, lowercase, without braces
  p := GuidToText(@Buffer[0], @g, @TwoDigitsHexLower);
  p^ := #0;
  result := p - PUtf8Char(@Buffer[0]);
end;

procedure DuckUuidToText(const v: TDuckDBHugeInt; var result: RawUtf8);
var
  tmp: TTemp47;
  len: PtrInt;
begin
  len := DuckUuidToTextBuf(v, tmp);
  FastSetString(result, @tmp[0], len);
end;

procedure DuckBitToText(P: PDuckU8Array; Len: integer; var result: RawUtf8);
var
  padding, i, n: PtrInt;
  c: PByte;
begin
  FastAssignNew(result);
  if Len < 1 then
    exit;
  padding := P^[0];
  n := (Len - 1) * 8 - padding;
  if n <= 0 then
    exit;
  c := FastSetString(result, n);
  for i := padding to (Len - 1) * 8 - 1 do
  begin
    c^ := ord('0') + ((P^[1 + i shr 3] shr (7 - (i and 7))) and 1);
    inc(c);
  end;
end;

procedure DuckTimeTzDecode(Bits: Int64; out Micros: Int64; out OffsetSec: integer);
const
  MAX_OFFSET = 16 * 60 * 60 - 1; // +/-15:59:59
begin
  Micros := Bits shr 24; // logical shift: Bits is always positive in practice
  OffsetSec := MAX_OFFSET - integer(Bits and $ffffff);
end;

procedure DuckTimeTzToText(Bits: Int64; var result: RawUtf8);
var
  us: Int64;
  offs: integer;
begin
  DuckTimeTzDecode(Bits, us, offs);
  DuckTimeToText(us, result);
  if offs < 0 then
    Append(result, '-')
  else
    Append(result, '+');
  offs := abs(offs);
  AppendShortToUtf8(UInt2DigitsToShortFast(offs div 3600), result);
  if offs mod 3600 = 0 then
    exit; // no microseconds
  Append(result, ':');
  AppendShortToUtf8(UInt2DigitsToShortFast((offs div 60) mod 60), result);
end;

function DuckTimestampMicros(TypeId: TDuckDBType; Value: Int64): Int64;
begin
  case TypeId of
    DUCKDB_TYPE_TIMESTAMP_S:
      result := Value * 1000000;
    DUCKDB_TYPE_TIMESTAMP_MS:
      result := Value * 1000;
    DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      result := Value div 1000;
  else
    result := Value;
  end;
end;

function DuckFixedSize(TypeId, Internal: TDuckDBType): PtrInt;
begin
  case TypeId of
    DUCKDB_TYPE_SQLNULL,
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_UTINYINT:
      result := 1;
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_USMALLINT:
      result := 2;
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_UINTEGER,
    DUCKDB_TYPE_FLOAT,
    DUCKDB_TYPE_DATE:
      result := 4;
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_DOUBLE,
    DUCKDB_TYPE_TIME,
    DUCKDB_TYPE_TIME_NS,
    DUCKDB_TYPE_TIME_TZ,
    DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      result := 8;
    DUCKDB_TYPE_HUGEINT,
    DUCKDB_TYPE_UHUGEINT,
    DUCKDB_TYPE_UUID,
    DUCKDB_TYPE_INTERVAL:
      result := 16;
    DUCKDB_TYPE_DECIMAL,
    DUCKDB_TYPE_ENUM:
      result := DuckFixedSize(Internal, DUCKDB_TYPE_INVALID);
  else
    result := 0;
  end;
end;

function DuckFixedInt(TypeId: TDuckDBType; P: PAnsiChar): Int64;
begin
  case TypeId of
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_UTINYINT:
      result := PByte(P)^;
    DUCKDB_TYPE_TINYINT:
      result := PShortInt(P)^;
    DUCKDB_TYPE_SMALLINT:
      result := PSmallInt(P)^;
    DUCKDB_TYPE_USMALLINT:
      result := PWord(P)^;
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_DATE:
      result := PInteger(P)^;
    DUCKDB_TYPE_UINTEGER:
      result := PCardinal(P)^;
  else // BIGINT, UBIGINT and the 64-bit date/time kinds
    result := PInt64(P)^;
  end;
end;

procedure DuckFixedHuge(P: PAnsiChar; var H: TDuckDBHugeInt);
  {$ifdef HASINLINE} inline; {$endif}
begin
  H.Lower := PInt64(P)^;
  H.Upper := PInt64(P + 8)^;
end;

function DuckFixedToTextBuf(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; var Buffer: TTemp47): PtrInt;
var
  b, num: PAnsiChar;
  h: TDuckDBHugeInt;
  d: double;
  tmp: ShortString;
begin
  result := 0;
  Buffer[0] := #0;
  num := nil;
  case TypeId of
    DUCKDB_TYPE_BOOLEAN:
      begin
        b := @BOOL_STR[P^ <> #0]; // 'false'/'true'
        PInt64(@Buffer)^ := PInt64(b + 1)^; // include ending #0
        result := ord(b[0]);
      end;
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      num := StrInt64(@Buffer[high(Buffer)], DuckFixedInt(TypeId, P));
    DUCKDB_TYPE_UBIGINT:
      num := StrUInt64(@Buffer[high(Buffer)], PQWord(P)^);
    DUCKDB_TYPE_HUGEINT,
    DUCKDB_TYPE_UHUGEINT:
      begin
        DuckFixedHuge(P, h);
        result := DuckHugeIntToTextBuf(h, TypeId = DUCKDB_TYPE_HUGEINT, 0, Buffer);
      end;
    DUCKDB_TYPE_DECIMAL:
      begin
        case Internal of
          DUCKDB_TYPE_HUGEINT:
            DuckFixedHuge(P, h);
          DUCKDB_TYPE_SMALLINT,
          DUCKDB_TYPE_INTEGER:
            begin
              h.Lower := DuckFixedInt(Internal, P);
              if h.Lower < 0 then
                h.Upper := -1 // sign extension
              else
                h.Upper := 0;
            end;
        else
          begin
            h.Lower := PInt64(P)^;
            if h.Lower < 0 then
              h.Upper := -1
            else
              h.Upper := 0;
          end;
        end;
        result := DuckHugeIntToTextBuf(h, true, Scale, Buffer);
      end;
    DUCKDB_TYPE_FLOAT,
    DUCKDB_TYPE_DOUBLE:
      begin
        if TypeId = DUCKDB_TYPE_FLOAT then
          d := QuackSingle(P)
        else
          d := QuackDouble(P);
        if PInt64(@d)^ = 0 then
        begin
          PCardinal(@Buffer)^ := ord('0');
          result := 1;
        end
        else
        begin
          result := DoubleToShort(@tmp, d);
          if result > high(Buffer) then
            result := -1 // very unlikely
          else
          begin
            MoveFast(tmp[1], Buffer[0], result);
            Buffer[result] := #0;
          end;
        end;
      end;
    DUCKDB_TYPE_UUID:
      begin
        DuckFixedHuge(P, h);
        result := DuckUuidToTextBuf(h, Buffer);
      end;
    DUCKDB_TYPE_SQLNULL:
      ; // NULL -> empty buffer
  else
    result := -1; // dates, times, intervals, ENUM, VARCHAR, BIT and nested types
  end;
  // normalize the backward-generated 64-bit integer text
  if num = nil then
    exit;
  result := @Buffer[high(Buffer)] - num;
  MoveFast(num^, Buffer[0], result);
  Buffer[result] := #0;
end;

procedure DuckFixedToText(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; var result: RawUtf8);
var
  len: PtrInt;
  iv: TDuckDBInterval;
  buf: TTemp47;
begin
  len := DuckFixedToTextBuf(TypeId, Internal, Scale, P, buf);
  if len >= 0 then
  begin
    FastSetString(result, @buf[0], len); // most simple types
    exit;
  end;
  FastAssignNew(result);
  case TypeId of
    DUCKDB_TYPE_FLOAT:
      DoubleToStr(QuackSingle(P), result);
    DUCKDB_TYPE_DOUBLE:
      DoubleToStr(QuackDouble(P), result);
    DUCKDB_TYPE_DATE:
      DuckDateToText(PInteger(P)^, result);
    DUCKDB_TYPE_TIME:
      DuckTimeToText(PInt64(P)^, result);
    DUCKDB_TYPE_TIME_NS:
      DuckTimeToText(PInt64(P)^ div 1000, result);
    DUCKDB_TYPE_TIME_TZ:
      DuckTimeTzToText(PInt64(P)^, result);
    DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS:
      DuckTimestampToText(DuckTimestampMicros(TypeId, PInt64(P)^), result);
    DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      begin
        DuckTimestampToText(DuckTimestampMicros(TypeId, PInt64(P)^), result);
        result := result + '+00'; // stored as UTC
      end;
    DUCKDB_TYPE_INTERVAL:
      begin
        iv.Months := PInteger(P)^;
        iv.Days   := PInteger(P + 4)^;
        iv.Micros := PInt64(P + 8)^;
        DuckIntervalToText(iv, result);
      end;
  end;
end;

procedure DuckFixedToJson(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; W: TJsonWriter);
var
  n: PtrInt;
  s: single;
  buf: TTemp47;
  txt: pointer;
begin
  case TypeId of
    DUCKDB_TYPE_BOOLEAN:
      W.Add(P^ <> #0);
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      W.Add(DuckFixedInt(TypeId, P));
    DUCKDB_TYPE_FLOAT:
      begin
        PInteger(@s)^ := PInteger(P)^;
        W.AddSingle(s);
      end;
    DUCKDB_TYPE_DOUBLE:
      W.AddDouble(QuackDouble(P));
    DUCKDB_TYPE_SQLNULL:
      W.AddNull;
    DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_HUGEINT,
    DUCKDB_TYPE_UHUGEINT,
    DUCKDB_TYPE_DECIMAL:
      begin
        n := DuckFixedToTextBuf(TypeId, Internal, Scale, P, buf);
        W.AddNoJsonEscape(@buf[0], n);
      end;
  else
    begin
      // UUID, dates, times, INTERVAL -> JSON string
      W.AddDirect('"');
      n := DuckFixedToTextBuf(TypeId, Internal, Scale, P, buf);
      if n >= 0 then
        W.AddJsonEscape(@buf[0], n)
      else
      begin
        txt := nil;
        DuckFixedToText(TypeId, Internal, Scale, P, RawUtf8(txt));
        W.AddJsonEscape(txt, length(RawUtf8(txt)));
        FastAssignNew(txt); // manual RawUtf8 release
      end;
      W.AddDirect('"');
    end;
  end;
end;

function DuckDecimalMantissa(Internal: TDuckDBType; P: PAnsiChar): Int64;
  {$ifdef HASINLINE} inline; {$endif}
begin
  case Internal of
    DUCKDB_TYPE_SMALLINT:
      result := PSmallInt(P)^;
    DUCKDB_TYPE_INTEGER:
      result := PInteger(P)^;
  else
    result := PInt64(P)^;
  end;
end;

function DuckFixedToInt64(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: Int64): TDuckConvert;
var
  h: TDuckDBHugeInt;
  d: double;
begin
  result := dcOk;
  case TypeId of
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER,
    DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_DATE,
    DUCKDB_TYPE_TIME,
    DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      Value := DuckFixedInt(TypeId, P);
    DUCKDB_TYPE_HUGEINT,
    DUCKDB_TYPE_UHUGEINT:
      begin
        DuckFixedHuge(P, h);
        Value := h.Lower;
        if not DuckHugeIntIsInt64(h, TypeId = DUCKDB_TYPE_HUGEINT) then
          result := dcOverflow;
      end;
    DUCKDB_TYPE_DECIMAL:
      if Internal = DUCKDB_TYPE_HUGEINT then
        result := dcFallback // integer part from the text
      else
        Value := DuckDecimalMantissa(Internal, P) div DUCKDB_POW10[Scale];
    DUCKDB_TYPE_FLOAT,
    DUCKDB_TYPE_DOUBLE:
      begin
        DuckFixedToDouble(TypeId, Internal, Scale, P, d);
        Value := trunc(d);
      end;
  else
    result := dcFallback;
  end;
end;

function DuckFixedToDouble(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: double): boolean;
begin
  result := true;
  case TypeId of
    DUCKDB_TYPE_FLOAT:
      Value := QuackSingle(P);
    DUCKDB_TYPE_DOUBLE:
      Value := QuackDouble(P);
    DUCKDB_TYPE_DECIMAL:
      if Internal = DUCKDB_TYPE_HUGEINT then
        result := false
      else
        Value := DuckDecimalMantissa(Internal, P) / DUCKDB_POW10[Scale];
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      Value := DuckFixedInt(TypeId, P);
  else
    result := false;
  end;
end;

function DuckFixedToCurrency(TypeId, Internal: TDuckDBType; Scale: integer;
  P: PAnsiChar; out Value: currency): boolean;
var
  v: Int64;
  d: double;
begin
  result := true;
  case TypeId of
    DUCKDB_TYPE_DECIMAL:
      if (Internal <> DUCKDB_TYPE_HUGEINT) and
         (Scale <= 4) then
      begin
        v := DuckDecimalMantissa(Internal, P) * DUCKDB_POW10[4 - Scale];
        Value := PCurrency(@v)^;
      end
      else
        result := false;
    DUCKDB_TYPE_FLOAT,
    DUCKDB_TYPE_DOUBLE:
      begin
        DuckFixedToDouble(TypeId, Internal, Scale, P, d);
        Value := d;
      end;
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      Value := DuckFixedInt(TypeId, P);
  else
    result := false;
  end;
end;

function DuckFixedToDateTime(TypeId: TDuckDBType; P: PAnsiChar;
  out Value: TDateTime): boolean;
var
  us: Int64;
  offs: integer;
begin
  result := true;
  case TypeId of
    DUCKDB_TYPE_DATE:
      Value := PInteger(P)^ + UnixDelta;
    DUCKDB_TYPE_TIME:
      Value := PInt64(P)^ / MicroSecsPerDay;
    DUCKDB_TYPE_TIME_NS:
      Value := (PInt64(P)^ div 1000) / MicroSecsPerDay;
    DUCKDB_TYPE_TIME_TZ:
      begin
        DuckTimeTzDecode(PInt64(P)^, us, offs);
        Value := us / MicroSecsPerDay;
      end;
    DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      Value := DuckTimestampToDateTime(
        DuckTimestampMicros(TypeId, PInt64(P)^));
  else
    result := false;
  end;
end;


{ ************ DuckDB Logical Type Cache }

{ TDuckDBTypeInfo }

function TDuckDBTypeInfo.Nest(n: PtrInt): PDuckDBTypeInfo;
begin
  ChildCount := n;
  Child := AllocMem(n * SizeOf(Child[0]));
  result := pointer(Child);
end;

procedure TDuckDBTypeInfo.Init(aHandle: TDuckDBLogicalType);
var
  i: PtrInt;
  p: PUtf8Char;
  nfo: PDuckDBTypeInfo;
begin
  FillCharFast(self, SizeOf(self), 0);
  Handle := aHandle;
  TypeId := DuckDB.get_type_id(Handle);
  case TypeId of
    DUCKDB_TYPE_DECIMAL:
      begin
        Internal := DuckDB.decimal_internal_type(Handle);
        Scale := DuckDB.decimal_scale(Handle);
      end;
    DUCKDB_TYPE_ENUM:
      Internal := DuckDB.enum_internal_type(Handle);
    DUCKDB_TYPE_LIST:
      Nest^.Init(DuckDB.list_type_child_type(Handle));
    DUCKDB_TYPE_ARRAY:
      begin
        Scale := DuckDB.array_type_array_size(Handle);
        Nest^.Init(DuckDB.array_type_child_type(Handle));
      end;
    DUCKDB_TYPE_MAP:
      begin
        Nest(2);
        Child[0].Init(DuckDB.map_type_key_type(Handle));
        Child[1].Init(DuckDB.map_type_value_type(Handle));
      end;
    DUCKDB_TYPE_STRUCT:
      begin
        nfo := Nest(DuckDB.struct_type_child_count(Handle));
        SetLength(Names, ChildCount);
        for i := 0 to ChildCount - 1 do
        begin
          p := DuckDB.struct_type_child_name(Handle, i);
          FastSetString(Names[i], p, StrLen(p));
          DuckDB.free_(p);
          nfo^.Init(DuckDB.struct_type_child_type(Handle, i));
          inc(nfo);
        end;
      end;
    DUCKDB_TYPE_UNION:
      begin
        nfo := Nest(DuckDB.union_type_member_count(Handle));
        for i := 0 to ChildCount - 1 do
        begin
          nfo^.Init(DuckDB.union_type_member_type(Handle, i));
          inc(nfo);
        end;
      end;
  end;
  Size := DuckFixedSize(TypeId, Internal);
end;

function TDuckDBTypeInfo.HasVariant: boolean;
var
  i: TDALen;
  p: PDuckDBTypeInfo;
begin
  result := true;
  if TypeId = DUCKDB_TYPE_VARIANT then
    exit;
  p := pointer(Child);
  for i := 1 to ChildCount do
    if (p^.TypeId = DUCKDB_TYPE_VARIANT) or
       ((p^.ChildCount <> 0) and
        p^.HasVariant) then
      exit
    else
      inc(p);
  result := false;
end;

procedure TDuckDBTypeInfo.Done;
var
  i: TDALen;
  p: PDuckDBTypeInfo;
begin
  p := pointer(Child);
  if p <> nil then
  begin
    for i := 1 to ChildCount do
    begin
      p^.Done; // released nested instances
      inc(p);
    end;
    FreeMem(Child);
    Child := nil;
  end;
  if Handle <> nil then
    DuckDB.destroy_logical_type(Handle);
  if Names <> nil then
    Names := nil; // needed for nested levels only
end;

procedure TDuckDBTypeInfo.EnumValue(Index: PtrInt; var Result: RawUtf8);
var
  i: PtrInt;
  p: PUtf8Char;
begin
  if Names = nil then // retrieve once all values of this enumerate
  begin
    SetLength(Names, DuckDB.enum_dictionary_size(Handle));
    for i := 0 to high(Names) do
    begin
      p := DuckDB.enum_dictionary_value(Handle, i);
      FastSetString(Names[i], p, StrLen(p));
      DuckDB.free_(p);
    end;
  end;
  if PtrUInt(Index) < PtrUInt(length(Names)) then
    Result := Names[Index]
  else
    FastAssignNew(Result);
end;

function DuckDBVariantSql(const Sql: RawUtf8; const Names: TRawUtf8DynArray;
  const IsVariant: TBytes): RawUtf8;
const
  _SQL_: array[boolean] of RawUtf8 = (
    // default positional #n references to support duplicated column names
    '%#% AS %',
    // the VARIANT NULL is converted to JSON 'null' by some DuckDB versions
    '%NULLIF(#%::JSON, ''null''::JSON) AS %');
var
  i: PtrInt;
  list, text: RawUtf8;
begin
  for i := 0 to high(Names) do
  begin
    if i > 0 then
      Append(list, ', ');
    list := FormatUtf8(_SQL_[GetBitPtr(IsVariant, i)],
                       [list, i + 1, QuotedStr(Names[i], '"')]);
  end;
  text := TrimU(Sql);
  while (text <> '') and
        (text[length(text)] = ';') do
    FakeLength(text, length(text) - 1); // within a sub-query
  Join(['SELECT ', list, ' FROM (', text, ')'], result);
end;

function DuckDBTypeToField(TypeId: TDuckDBType; Scale: integer): TSqlDBFieldType;
begin
  case TypeId of
    DUCKDB_TYPE_BOOLEAN,
    DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER,
    DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_HUGEINT,
    DUCKDB_TYPE_UHUGEINT:
      result := ftInt64;
    DUCKDB_TYPE_FLOAT,
    DUCKDB_TYPE_DOUBLE:
      result := ftDouble;
    DUCKDB_TYPE_DECIMAL:
      if Scale <= 4 then
        result := ftCurrency
      else
        result := ftDouble;
    DUCKDB_TYPE_DATE,
    DUCKDB_TYPE_TIME,
    DUCKDB_TYPE_TIME_TZ,
    DUCKDB_TYPE_TIME_NS,
    DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
      result := ftDate;
    DUCKDB_TYPE_BLOB,
    DUCKDB_TYPE_GEOMETRY: // GEOMETRY as WKB
      result := ftBlob;
    DUCKDB_TYPE_SQLNULL:
      result := ftNull;
  else
    result := ftUtf8; // VARCHAR, UUID, ENUM, INTERVAL, BIT, or nested as JSON
  end;
end;


{ ************ Quack Constants and Exception }

const
  FIELD_END = $ffff; // MESSAGE_TERMINATOR_FIELD_ID

  // vector_type of a serialized vector
  VECTOR_FLAT       = 0;
  VECTOR_CONSTANT   = 2;
  VECTOR_DICTIONARY = 3;
  VECTOR_SEQUENCE   = 4;

  // LogicalTypeInfoType
  INFO_DECIMAL = 2;
  INFO_STRING  = 3;
  INFO_LIST    = 4;
  INFO_STRUCT  = 5;
  INFO_ENUM    = 6;
  INFO_ARRAY   = 9;


{ Low-Level Reading Functions }

procedure ExpectField(var R: TFastReader; Id: cardinal);
var
  f: cardinal;
begin
  f := R.Next2;
  if f <> Id then
    ESqlDBQuack.RaiseUtf8('unexpected field % instead of %', [f, Id]);
end;

procedure ExpectEnd(var R: TFastReader);
  {$ifdef HASINLINE}inline;{$endif}
begin
  ExpectField(R, FIELD_END);
end;

function ReadLen(var R: TFastReader): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
var
  v: QWord;
begin
  v := R.VarUInt64;
  if (Int64(v) < 0) or
     (v > PtrUInt(R.Last - R.P)) then
    ESqlDBQuack.RaiseUtf8('length % overflow', [Int64(v)]);
  result := v;
end;

function ReadCount(var R: TFastReader; const MinBytesPerItem: PtrInt): PtrInt;
  {$ifdef HASINLINE}inline;{$endif}
var
  v: QWord;
begin
  // a list count can't be bigger than the remaining bytes
  v := R.VarUInt64;
  if (Int64(v) < 0) or
     (Int64(v) > (R.Last - R.P) div MinBytesPerItem) then
    ESqlDBQuack.RaiseUtf8('list count % overflow', [Int64(v)]);
  result := v;
end;

procedure ReadStr(var R: TFastReader; var Str: RawUtf8);
var
  len: PtrInt;
begin
  len := ReadLen(R);
  FastSetString(Str, R.Next(len), len);
end;

procedure SkipStr(var R: TFastReader);
begin
  R.Next(ReadLen(R));
end;

function ReadIdx(var R: TFastReader): Int64;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := Int64(R.VarUInt64); // optional_idx: stored as idx_t
end;

procedure ReadHugeInt(var R: TFastReader; out V: TDuckDBHugeInt);
begin
  V.Upper := R.VarLeb64;
  V.Lower := Int64(R.VarUInt64);
end;

function QuackDouble(P: pointer): double;
begin
  PInt64(@result)^ := PInt64(P)^; // may be unaligned
end;

function QuackSingle(P: pointer): double;
var
  s: single;
begin
  PInteger(@s)^ := PInteger(P)^;
  result := s;
end;


{ ************ TQuackType Logical Types Description }

{ TQuackType }

function TQuackType.FieldType: TSqlDBFieldType;
begin
  result := DuckDBTypeToField(DuckType, Scale); // same mapping as the C API
end;

function TQuackType.HasVariant: boolean;
var
  i: PtrInt;
begin
  result := true;
  if Id = QTYPE_VARIANT then
    exit;
  for i := 0 to length(Children) - 1 do
    if Children[i].HasVariant then
      exit;
  result := false;
end;

function TQuackType.EnumIndexAt(D: PAnsiChar): PtrInt;
begin
  case ElemSize of
    1:
      result := PByte(D)^;
    2:
      result := PWord(D)^;
  else
    result := PCardinal(D)^;
  end;
end;

function TQuackType.ElemSizeInternal: TDuckDBType;
begin
  // storage type of a DECIMAL
  case ElemSize of
    2:
      result := DUCKDB_TYPE_SMALLINT;
    4:
      result := DUCKDB_TYPE_INTEGER;
    8:
      result := DUCKDB_TYPE_BIGINT;
  else
    result := DUCKDB_TYPE_HUGEINT;
  end;
end;

procedure TQuackType.SetStorage;
var
  n: PtrInt;
begin
  Storage := qsFixed;
  case Id of
    // fixed-size types
    QTYPE_SQLNULL:
      DuckType := DUCKDB_TYPE_SQLNULL;
    QTYPE_BOOLEAN:
      DuckType := DUCKDB_TYPE_BOOLEAN;
    QTYPE_TINYINT:
      DuckType := DUCKDB_TYPE_TINYINT;
    QTYPE_UTINYINT:
      DuckType := DUCKDB_TYPE_UTINYINT;
    QTYPE_SMALLINT:
      DuckType := DUCKDB_TYPE_SMALLINT;
    QTYPE_USMALLINT:
      DuckType := DUCKDB_TYPE_USMALLINT;
    QTYPE_INTEGER:
      DuckType := DUCKDB_TYPE_INTEGER;
    QTYPE_UINTEGER:
      DuckType := DUCKDB_TYPE_UINTEGER;
    QTYPE_BIGINT:
      DuckType := DUCKDB_TYPE_BIGINT;
    QTYPE_UBIGINT,
    QTYPE_POINTER:
      DuckType := DUCKDB_TYPE_UBIGINT;
    QTYPE_HUGEINT:
      DuckType := DUCKDB_TYPE_HUGEINT;
    QTYPE_UHUGEINT:
      DuckType := DUCKDB_TYPE_UHUGEINT;
    QTYPE_FLOAT:
      DuckType := DUCKDB_TYPE_FLOAT;
    QTYPE_DOUBLE:
      DuckType := DUCKDB_TYPE_DOUBLE;
    QTYPE_DATE:
      DuckType := DUCKDB_TYPE_DATE;
    QTYPE_TIME:
      DuckType := DUCKDB_TYPE_TIME;
    QTYPE_TIME_NS:
      DuckType := DUCKDB_TYPE_TIME_NS;
    QTYPE_TIME_TZ:
      DuckType := DUCKDB_TYPE_TIME_TZ;
    QTYPE_TIMESTAMP:
      DuckType := DUCKDB_TYPE_TIMESTAMP;
    QTYPE_TIMESTAMP_SEC:
      DuckType := DUCKDB_TYPE_TIMESTAMP_S;
    QTYPE_TIMESTAMP_MS:
      DuckType := DUCKDB_TYPE_TIMESTAMP_MS;
    QTYPE_TIMESTAMP_NS:
      DuckType := DUCKDB_TYPE_TIMESTAMP_NS;
    QTYPE_TIMESTAMP_TZ:
      DuckType := DUCKDB_TYPE_TIMESTAMP_TZ;
    QTYPE_TIMESTAMP_TZ_NS:
      DuckType := DUCKDB_TYPE_TIMESTAMP_TZ_NS;
    QTYPE_INTERVAL:
      DuckType := DUCKDB_TYPE_INTERVAL;
    QTYPE_UUID:
      DuckType := DUCKDB_TYPE_UUID;
    // variable-width fixed-size types
    QTYPE_DECIMAL:
      begin
        DuckType := DUCKDB_TYPE_DECIMAL;
        if Width <= 4 then
          ElemSize := 2
        else if Width <= 9 then
          ElemSize := 4
        else if Width <= 18 then
          ElemSize := 8
        else
          ElemSize := 16;
        exit;
      end;
    QTYPE_ENUM:
      begin
        DuckType := DUCKDB_TYPE_ENUM;
        n := length(EnumValues);
        if n <= $100 then
          ElemSize := 1
        else if n <= $10000 then
          ElemSize := 2
        else
          ElemSize := 4;
        exit;
      end;
    // strings and blobs
    QTYPE_CHAR,
    QTYPE_VARCHAR:
      begin
        Storage := qsString;
        DuckType := DUCKDB_TYPE_VARCHAR;
      end;
    QTYPE_BLOB,
    QTYPE_GEOMETRY:
      begin
        Storage := qsString;
        DuckType := DUCKDB_TYPE_BLOB;
      end;
    QTYPE_BIT:
      begin
        Storage := qsString;
        DuckType := DUCKDB_TYPE_BIT;
      end;
    QTYPE_BIGNUM:
      begin
        Storage := qsString;
        DuckType := DUCKDB_TYPE_BIGNUM;
      end;
    // nested types
    QTYPE_STRUCT,
    QTYPE_TUPLE:
      begin
        Storage := qsStruct;
        DuckType := DUCKDB_TYPE_STRUCT;
      end;
    QTYPE_UNION:
      begin
        Storage := qsStruct;
        DuckType := DUCKDB_TYPE_UNION;
      end;
    QTYPE_LIST:
      begin
        Storage := qsList;
        DuckType := DUCKDB_TYPE_LIST;
      end;
    QTYPE_MAP:
      begin
        Storage := qsList;
        DuckType := DUCKDB_TYPE_MAP;
      end;
    QTYPE_ARRAY:
      begin
        Storage := qsArray;
        DuckType := DUCKDB_TYPE_ARRAY;
      end;
  else
    Storage := qsUnsupported; // e.g. VARIANT
  end;
  if Storage = qsFixed then
    ElemSize := DuckFixedSize(DuckType, DUCKDB_TYPE_INVALID)
  else if (Storage in [qsList, qsArray]) and
          (length(Children) <> 1) then
    Storage := qsUnsupported;
end;


{ ************ TQuackVector and TQuackChunk Result Data Access }

{ TQuackVector }

destructor TQuackVector.Destroy;
begin
  Child.Free;
  ObjArrayClear(Children);
  inherited Destroy;
end;

function TQuackVector.Stored(Row: PtrInt): PtrInt;
begin
  case Mode of
    qvmConstant:
      result := 0;
    qvmDictionary:
      result := PCardinal(Sel + Row * 4)^;
  else
    result := Row;
  end;
end;

function TQuackVector.IsStoredValid(Index: PtrInt): boolean;
begin
  result := (Validity = nil) or
            (((Validity^[Index shr 3] shr (Index and 7)) and 1) <> 0);
end;

function TQuackVector.IsValid(Row: PtrInt): boolean;
begin
  result := IsStoredValid(Stored(Row));
end;

function TQuackVector.DataAt(Index: PtrInt): PAnsiChar;
begin
  result := Data + Index * VType.ElemSize;
end;

function TQuackVector.StringAt(Index: PtrInt; out Len: integer): PUtf8Char;
begin
  result := @StrBytes[StrOffs^[Index]];
  Len := StrOffs^[StoredCount + Index];
end;

function TQuackVector.TextAt(Index: PtrInt; var Temp: TTemp47): PUtf8Char;
var
  len: PtrInt;
begin
  result := @Temp;
  case VType.Storage of
    qsFixed: // convert e.g. UBIGINT or 128-bit DECIMAL into temporary text
      if DuckFixedToTextBuf(VType.DuckType, VType.ElemSizeInternal,
          VType.Scale, DataAt(Index), Temp) >= 0 then
        exit;
    qsString:
      begin
        len := StrOffs^[StoredCount + Index]; // inlined StringAt()
        if len <= high(Temp) then
        begin
          MoveFast(StrBytes[StrOffs^[Index]], Temp, len);
          Temp[len] := #0;
          exit;
        end;
      end;
  end;
  result := nil;
end;


{ TQuackChunk }

destructor TQuackChunk.Destroy;
begin
  ObjArrayClear(Vectors);
  inherited Destroy;
end;


{ ************ TQuackResponse Messages Decoding }

{ TQuackResponse }

function TQuackResponse.NewType: TQuackType;
begin
  result := TQuackType.Create;
  PtrArrayAdd(fTypePool, result, fTypePoolCount);
end;

function TQuackResponse.ReadType(var R: TFastReader): TQuackType;
var
  f, n, infotype, w: cardinal;
  i: PtrInt;
begin
  // a LogicalType is an object with its id and an optional LogicalTypeInfo
  inc(fTypeDepth);
  if fTypeDepth > 32 then
    ESqlDBQuack.RaiseU('types nesting overflow');
  result := NewType;
  ExpectField(R, 100);
  n := R.VarUInt32;
  if n > 255 then
    ESqlDBQuack.RaiseUtf8('unexpected type %', [n]);
  result.Id := n;
  infotype := 0;
  repeat
    f := R.Next2;
    case f of
      FIELD_END:
        break;
      101: // type_info as a nullable object
        begin
          if R.NextByte = 0 then
            continue;
          repeat
            f := R.Next2;
            case f of
              FIELD_END:
                break;
              100:
                infotype := R.VarUInt32;
              101:
                ReadStr(R, result.Alias);
              103:
                ESqlDBQuack.RaiseUtf8('unsupported extension type %', [result.Alias]);
              200:
                case infotype of
                  INFO_DECIMAL:
                    result.Width := R.VarUInt32;
                  INFO_STRING:
                    SkipStr(R); // collation
                  INFO_LIST, INFO_ARRAY:
                    begin
                      SetLength(result.Children, 1);
                      result.Children[0] := ReadType(R);
                    end;
                  INFO_STRUCT:
                    begin
                      n := ReadCount(R, 4);
                      SetLength(result.ChildNames, n);
                      SetLength(result.Children, n);
                      for i := 0 to n - 1 do
                      begin
                        ExpectField(R, 0);
                        ReadStr(R, result.ChildNames[i]);
                        ExpectField(R, 1);
                        result.Children[i] := ReadType(R);
                        ExpectEnd(R);
                      end;
                    end;
                  INFO_ENUM:
                    begin
                      n := R.VarUInt32; // values_count
                      if n > cardinal(R.Last - R.P) then
                        ESqlDBQuack.RaiseUtf8('enum size % overflow', [n]);
                      SetLength(result.EnumValues, n);
                    end;
                else
                  ESqlDBQuack.RaiseUtf8('unsupported type info %', [infotype]);
                end;
              201:
                case infotype of
                  INFO_DECIMAL:
                    result.Scale := R.VarUInt32;
                  INFO_ARRAY:
                    result.ArraySize := R.VarUInt32;
                  INFO_ENUM:
                    begin
                      w := ReadCount(R, 1);
                      if w <> cardinal(length(result.EnumValues)) then
                        ESqlDBQuack.RaiseU('enum values count mismatch');
                      for i := 0 to w - 1 do
                        ReadStr(R, result.EnumValues[i]);
                    end;
                else
                  ESqlDBQuack.RaiseUtf8('unsupported type info % field', [infotype]);
                end;
            else
              ESqlDBQuack.RaiseUtf8('unexpected type info field %', [f]);
            end;
          until false;
        end;
    else
      ESqlDBQuack.RaiseUtf8('unexpected type field %', [f]);
    end;
  until false;
  result.SetStorage;
  dec(fTypeDepth);
end;

function TQuackResponse.ReadTypes(var R: TFastReader): TQuackTypeDynArray;
var
  n, i: PtrInt;
begin
  n := ReadCount(R, 3);
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := ReadType(R);
end;

procedure TQuackResponse.ReadVectorBody(var R: TFastReader; V: TQuackVector;
  Count: PtrInt);
var
  t: TQuackType;
  i, n, len: PtrInt;
  lens: PAnsiChar;
  total, acc: Int64;
  o: PCardinalArray;
  e: PDuckDBListEntry;
begin
  t := V.VType;
  V.StoredCount := Count;
  if t.Storage = qsUnsupported then
    ESqlDBQuack.RaiseUtf8('unsupported type %', [t.Id]);
  if t.Id = QTYPE_GEOMETRY then
    if R.Peek2 = 99 then
    begin
      R.Next2;
      R.VarUInt32; // geometry_format: WKB as plain blob
    end;
  ExpectField(R, 100);
  if R.NextByte <> 0 then // has_validity_mask
  begin
    ExpectField(R, 101);
    len := ReadLen(R);
    if len <> ((Count + 63) shr 6) * 8 then
      ESqlDBQuack.RaiseU('validity size mismatch');
    V.Validity := R.Next(len);
  end;
  case t.Storage of
    qsFixed:
      begin
        ExpectField(R, 102);
        len := ReadLen(R);
        if len <> Count * t.ElemSize then
          ESqlDBQuack.RaiseUtf8('data size mismatch for type %', [t.Id]);
        V.Data := R.Next(len);
      end;
    qsString:
      begin
        o := FastNewRawByteString(V.fOwned, Count * 8 + 8);
        V.StrOffs := o;
        if R.Peek2 = 107 then
        begin
          // lengths and bytes as two blobs
          R.Next2;
          total := ReadIdx(R);
          ExpectField(R, 108);
          len := ReadLen(R);
          if len <> Count * 4 then
            ESqlDBQuack.RaiseU('string lengths size mismatch');
          lens := R.Next(len);
          ExpectField(R, 109);
          len := ReadLen(R);
          if len <> total then
            ESqlDBQuack.RaiseU('string bytes size mismatch');
          V.StrBytes := R.Next(len);
          acc := 0;
          for i := 0 to Count - 1 do
          begin
            o^[i] := acc;
            o^[Count + i] := PCardinal(lens + i * 4)^;
            inc(acc, o^[Count + i]);
            if acc > total then
              ESqlDBQuack.RaiseU('string lengths overflow');
          end;
          if acc <> total then
            ESqlDBQuack.RaiseU('string lengths mismatch');
        end
        else
        begin
          // older format: a list of strings (e.g. from the version 1 server)
          ExpectField(R, 102);
          n := ReadCount(R, 1);
          if n <> Count then
            ESqlDBQuack.RaiseU('strings count mismatch');
          V.StrBytes := R.P;
          for i := 0 to Count - 1 do
          begin
            len := ReadLen(R);
            o^[i] := R.P - V.StrBytes;
            o^[Count + i] := len;
            R.Next(len);
          end;
        end;
      end;
    qsStruct:
      begin
        ExpectField(R, 103);
        n := ReadCount(R, 2);
        if n <> length(t.Children) then
          ESqlDBQuack.RaiseUtf8('struct children count mismatch %<>%',
            [n, length(t.Children)]);
        SetLength(V.Children, n);
        for i := 0 to n - 1 do
        begin
          V.Children[i] := ReadVector(R, t.Children[i], Count);
          ExpectEnd(R);
        end;
      end;
    qsList:
      begin
        ExpectField(R, 104);
        total := Int64(R.VarUInt64); // list_size
        if (total < 0) or
           (total > MaxInt) then
          ESqlDBQuack.RaiseUtf8('list size=% overflow', [total]);
        ExpectField(R, 105);
        n := ReadCount(R, 6);
        if n <> Count then
          ESqlDBQuack.RaiseUtf8('list entries count mismatch %<>%', [n, Count]);
        e := FastNewRawByteString(V.fOwned, n * SizeOf(TDuckDBListEntry));
        V.Entries := e;
        for i := 0 to n - 1 do
        begin
          ExpectField(R, 100);
          e^.Offset := Int64(R.VarUInt64);
          ExpectField(R, 101);
          e^.Length := Int64(R.VarUInt64);
          ExpectEnd(R);
          if (e^.Offset < 0) or
             (e^.Length < 0) or
             (e^.Offset > total) or
             (e^.Length > total - e^.Offset) then
            if V.IsStoredValid(i) then // 0,0 on NULL
              ESqlDBQuack.RaiseU('list entry out of range');
          inc(e);
        end;
        ExpectField(R, 106);
        V.Child := ReadVector(R, t.Children[0], total);
        ExpectEnd(R);
      end;
    qsArray:
      begin
        ExpectField(R, 103);
        if Int64(R.VarUInt64) <> t.ArraySize then
          ESqlDBQuack.RaiseU('array size mismatch');
        ExpectField(R, 104);
        if Int64(Count) * t.ArraySize > MaxInt then
          ESqlDBQuack.RaiseU('array size overflow');
        V.Child := ReadVector(R, t.Children[0], Count * integer(t.ArraySize));
        ExpectEnd(R);
      end;
  end;
end;

function TQuackResponse.ReadVector(var R: TFastReader; T: TQuackType;
  Count: PtrInt): TQuackVector;
var
  vt: cardinal;
  len, i: PtrInt;
  start, step, v: Int64;
  d: PAnsiChar;
begin
  result := TQuackVector.Create;
  try
    result.VType := T;
    result.Count := Count;
    vt := VECTOR_FLAT;
    if R.Peek2 = 90 then
    begin
      R.Next2;
      vt := R.VarUInt32;
    end;
    case vt of
      VECTOR_FLAT:
        ReadVectorBody(R, result, Count);
      VECTOR_CONSTANT:
        begin
          result.Mode := qvmConstant;
          ReadVectorBody(R, result, 1); // a single stored value
        end;
      VECTOR_DICTIONARY:
        begin
          result.Mode := qvmDictionary;
          ExpectField(R, 91);
          len := ReadLen(R);
          if len <> Count * 4 then
            ESqlDBQuack.RaiseU('dictionary selection size mismatch');
          result.Sel := R.Next(len);
          ExpectField(R, 92);
          len := Int64(R.VarUInt64); // dict_count
          if (len < 0) or
             (len > R.Last - R.P) then
            ESqlDBQuack.RaiseU('dictionary size overflow');
          ReadVectorBody(R, result, len);
          for i := 0 to Count - 1 do
            if PCardinal(result.Sel + i * 4)^ >= cardinal(len) then
              ESqlDBQuack.RaiseU('dictionary index out of range');
        end;
      VECTOR_SEQUENCE:
        begin
          // materialize the sequence as a regular flat vector
          ExpectField(R, 91);
          start := R.VarLeb64;
          ExpectField(R, 92);
          step := R.VarLeb64;
          if (T.Storage <> qsFixed) or
             (T.ElemSize > 8) then
            ESqlDBQuack.RaiseUtf8('unsupported sequence type %', [T.Id]);
          d := FastNewRawByteString(result.fOwned, Count * T.ElemSize);
          result.Data := d;
          result.StoredCount := Count;
          v := start;
          for i := 0 to Count - 1 do
          begin
            MoveFast(v, d^, T.ElemSize); // little endian truncation
            inc(d, T.ElemSize);
            inc(v, step);
          end;
        end;
    else
      ESqlDBQuack.RaiseUtf8('unsupported vector type %', [vt]);
    end;
  except
    result.Free;
    raise;
  end;
end;

function TQuackResponse.ReadChunk(var R: TFastReader): TQuackChunk;
var
  i, n: PtrInt;
  start: PAnsiChar;
  types: TQuackTypeDynArray;
begin
  result := TQuackChunk.Create;
  try
    ExpectField(R, 100);
    result.RowCount := R.VarUInt32;
    ExpectField(R, 101);
    start := R.P;
    types := nil;
    if (fLastTypesStart <> nil) and
       (R.P + fLastTypesLen <= R.Last) and
       CompareMem(R.P, fLastTypesStart, fLastTypesLen) then
    begin
      // same types than the previous chunk or the response: no need to parse
      types := fLastTypes;
      inc(R.P, fLastTypesLen);
    end
    else
    begin
      types := ReadTypes(R);
      fLastTypes := types;
      fLastTypesStart := start;
      fLastTypesLen := R.P - start;
    end;
    ExpectField(R, 102);
    n := ReadCount(R, 2);
    if n <> length(types) then
      ESqlDBQuack.RaiseU('chunk columns count mismatch');
    SetLength(result.Vectors, n);
    for i := 0 to n - 1 do
    begin
      result.Vectors[i] := ReadVector(R, types[i], result.RowCount);
      ExpectEnd(R);
    end;
    ExpectEnd(R);
  except
    result.Free;
    raise;
  end;
end;

procedure TQuackResponse.ReadChunks(var R: TFastReader);
var
  i, n: PtrInt;
begin
  // a list of nullable DataChunkWrapper objects
  n := ReadCount(R, 4);
  SetLength(Chunks, n);
  for i := 0 to n - 1 do
  begin
    if R.NextByte = 0 then
      ESqlDBQuack.RaiseU('unexpected void chunk');
    ExpectField(R, 300);
    Chunks[i] := ReadChunk(R);
    ExpectEnd(R);
  end;
end;

procedure TQuackResponse.ReadPrepare(var R: TFastReader);
var
  f: cardinal;
  i, j, n: PtrInt;
  start: PAnsiChar;
begin
  repeat
    f := R.Next2;
    case f of
      FIELD_END:
        break;
      1: // result_types
        begin
          start := R.P;
          ColumnTypes := ReadTypes(R);
          fLastTypes := ColumnTypes;
          fLastTypesStart := start;
          fLastTypesLen := R.P - start;
        end;
      2: // result_names
        begin
          n := ReadCount(R, 1);
          SetLength(ColumnNames, n);
          for i := 0 to n - 1 do
            ReadStr(R, ColumnNames[i]);
          if length(ColumnNames) <> length(ColumnTypes) then
            ESqlDBQuack.RaiseU('columns names and types count mismatch');
          for i := 0 to n - 1 do
            if ColumnTypes[i].HasVariant then
            begin
              // the rows can't be decoded: ask them again as JSON
              SetLength(VariantColumns, (n + 31) shr 3);
              SetBitPtr(VariantColumns, i);
              for j := i + 1 to n - 1 do
                if ColumnTypes[j].HasVariant then
                  SetBitPtr(VariantColumns, j);
              exit;
            end;
        end;
      3:
        NeedsMoreFetch := R.NextByte <> 0;
      4:
        ReadChunks(R);
      5:
        ReadHugeInt(R, QueryUuid);
    else
      ESqlDBQuack.RaiseUtf8('unexpected prepare response field %', [f]);
    end;
  until false;
  if length(ColumnNames) <> length(ColumnTypes) then
    ESqlDBQuack.RaiseU('columns names and types count mismatch');
end;

procedure TQuackResponse.ReadFetch(var R: TFastReader);
var
  f: cardinal;
  i, count: PtrInt;
begin
  if fVersion < 3 then
  begin
    // version 1: the chunks are inlined, and no more chunks means the end
    repeat
      f := R.Next2;
      case f of
        FIELD_END:
          break;
        1:
          ReadChunks(R);
        2:
          BatchIndex := ReadIdx(R);
      else
        ESqlDBQuack.RaiseUtf8('unexpected fetch response field %', [f]);
      end;
    until false;
    exit;
  end;
  count := 0;
  repeat
    f := R.Next2;
    case f of
      FIELD_END:
        break;
      1:
        count := ReadCount(R, 4);
      2:
        TotalBatches := ReadIdx(R);
      3:
        BatchIndex := ReadIdx(R);
    else
      ESqlDBQuack.RaiseUtf8('unexpected fetch response field %', [f]);
    end;
  until false;
  // the chunks are sent as raw objects just after the message
  SetLength(Chunks, count);
  for i := 0 to count - 1 do
    Chunks[i] := ReadChunk(R);
end;

procedure TQuackResponse.ReadConnection(var R: TFastReader);
var
  f: cardinal;
begin
  repeat
    f := R.Next2;
    case f of
      FIELD_END:
        break;
      1:
        ReadStr(R, ServerVersion);
      2:
        ReadStr(R, ServerPlatform);
      3:
        QuackVersion := R.VarUInt32;
      4:
        HeartbeatSeconds := R.VarUInt32;
    else
      ESqlDBQuack.RaiseUtf8('unexpected connection response field %', [f]);
    end;
  until false;
end;

procedure TQuackResponse.ReadError(var R: TFastReader);
var
  f: cardinal;
  i, n: PtrInt;
begin
  repeat
    f := R.Next2;
    case f of
      FIELD_END:
        break;
      1:
        ReadStr(R, ErrorMessage);
      2:
        ReadStr(R, ErrorType);
      3: // extra_info map as a list of key/value objects: ignored
        begin
          n := ReadCount(R, 2);
          for i := 0 to n - 1 do
          begin
            ExpectField(R, 0);
            SkipStr(R);
            ExpectField(R, 1);
            SkipStr(R);
            ExpectEnd(R);
          end;
        end;
      4:
        MustInvalidate := R.NextByte <> 0;
    else
      ESqlDBQuack.RaiseUtf8('unexpected error response field %', [f]);
    end;
  until false;
end;

function TQuackResponse.IsCountResult: boolean;
begin
  // a single "Count" BIGINT column with the number of changed rows
  result := (length(ColumnTypes) = 1) and
            (ColumnTypes[0].Id = QTYPE_BIGINT) and
            (length(ColumnNames) = 1) and
            (ColumnNames[0] = 'Count') and
            (length(Chunks) > 0) and
            (Chunks[0].RowCount = 1) and
            Chunks[0].Vectors[0].IsValid(0);
end;

constructor TQuackResponse.Create(const Body: RawByteString;
  Version: cardinal);
var
  R: TFastReader;
  f: cardinal;
begin
  fBody := Body;
  fVersion := Version;
  BatchIndex := -1;
  TotalBatches := -1;
  R.Init(fBody);
  try
    // message header object
    ExpectField(R, 1);
    MessageType := R.VarUInt32;
    repeat
      f := R.Next2;
      case f of
        FIELD_END:
          break;
        2:
          ReadStr(R, ConnectionId);
        3:
          ReadIdx(R); // client_query_id
      else
        ESqlDBQuack.RaiseUtf8('unexpected header field %', [f]);
      end;
    until false;
    // message body object
    case MessageType of
      QUACK_CONNECTION_RESPONSE:
        ReadConnection(R);
      QUACK_PREPARE_RESPONSE:
        ReadPrepare(R);
      QUACK_FETCH_RESPONSE:
        ReadFetch(R);
      QUACK_ERROR_RESPONSE:
        ReadError(R);
      QUACK_SUCCESS_RESPONSE, QUACK_SEND_DATA_RESPONSE:
        begin
          while R.Next2 <> FIELD_END do
            ESqlDBQuack.RaiseU('unexpected response field');
        end;
    else
      ESqlDBQuack.RaiseUtf8('unexpected response message type %', [MessageType]);
    end;
  except
    on E: EFastReader do // the destructor is called by the exception
      ESqlDBQuack.RaiseUtf8('malformed response (%)', [E.Message]);
  end;
end;

destructor TQuackResponse.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(Chunks) do
    Chunks[i].Free;
  Chunks := nil;
  for i := 0 to fTypePoolCount - 1 do
    fTypePool[i].Free;
  fTypePool := nil;
  fTypePoolCount := 0;
  inherited Destroy;
end;


{ ************ Quack Requests Encoding }

procedure WriteField(W: TBufferWriter; Id: cardinal);
  {$ifdef HASINLINE} inline; {$endif}
begin
  W.Write2(Id); // raw 16-bit little endian field id
end;

procedure WriteString(W: TBufferWriter; Id: cardinal; const Value: RawUtf8);
begin
  if Value = '' then
    exit; // default values are not serialized
  WriteField(W, Id);
  W.WriteVarUInt32(length(Value));
  W.Write(pointer(Value), length(Value));
end;

procedure WriteHugeInt(W: TBufferWriter; Id: cardinal;
  const Value: TDuckDBHugeInt);
begin
  WriteField(W, Id);
  W.WriteVarLeb64(Value.Upper);
  W.WriteVarUInt64(QWord(Value.Lower));
end;

procedure WriteHeader(W: TBufferWriter; MessageType: byte;
  const ConnectionId: RawUtf8);
const
  // client_query_id is always serialized, as an unset optional_idx (~0)
  NO_QUERY_ID: array[0..9] of byte = (
    $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $01);
begin
  WriteField(W, 1);
  W.WriteVarUInt32(MessageType);
  WriteString(W, 2, ConnectionId);
  WriteField(W, 3);
  W.Write(@NO_QUERY_ID, SizeOf(NO_QUERY_ID));
  WriteField(W, FIELD_END);
end;

function QuackConnectionRequest(const Token, ClientId: RawUtf8;
  HeartbeatSeconds, Version: cardinal): RawByteString;
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBufferWriter.Create(tmp);
  try
    WriteHeader(W, QUACK_CONNECTION_REQUEST, '');
    WriteString(W, 1, Token);
    WriteString(W, 2, 'mORMot2');
    WriteString(W, 3, 'mormot2');
    WriteField(W, 4); // min_supported_quack_version
    W.WriteVarUInt32(Version);
    WriteField(W, 5); // max_supported_quack_version
    W.WriteVarUInt32(Version);
    if Version >= 3 then
    begin
      WriteString(W, 6, ClientId);
      WriteField(W, 7);
      W.WriteVarUInt32(HeartbeatSeconds);
    end;
    WriteField(W, FIELD_END);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

function QuackPrepareRequest(const ConnectionId, Sql: RawUtf8;
  const QueryUuid: TDuckDBHugeInt; Version: cardinal): RawByteString;
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBufferWriter.Create(tmp);
  try
    WriteHeader(W, QUACK_PREPARE_REQUEST, ConnectionId);
    WriteString(W, 1, Sql);
    if Version >= 3 then
      WriteHugeInt(W, 2, QueryUuid);
    WriteField(W, FIELD_END);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

function QuackFetchRequest(const ConnectionId: RawUtf8;
  const QueryUuid: TDuckDBHugeInt;
  BatchIndex, AckIndex, Version: cardinal): RawByteString;
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBufferWriter.Create(tmp);
  try
    WriteHeader(W, QUACK_FETCH_REQUEST, ConnectionId);
    WriteHugeInt(W, 1, QueryUuid);
    if Version >= 3 then
    begin
      if BatchIndex <> 0 then
      begin
        WriteField(W, 2);
        W.WriteVarUInt32(BatchIndex);
      end;
      if AckIndex <> 0 then
      begin
        WriteField(W, 3);
        W.WriteVarUInt32(AckIndex);
      end;
    end;
    WriteField(W, FIELD_END);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

function QuackSimpleRequest(MessageType: byte;
  const ConnectionId: RawUtf8): RawByteString;
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBufferWriter.Create(tmp);
  try
    WriteHeader(W, MessageType, ConnectionId);
    WriteField(W, FIELD_END); // void body
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

function QuackQueryRequest(MessageType: byte; const ConnectionId: RawUtf8;
  const QueryUuid: TDuckDBHugeInt): RawByteString;
var
  W: TBufferWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TBufferWriter.Create(tmp);
  try
    WriteHeader(W, MessageType, ConnectionId);
    WriteHugeInt(W, 1, QueryUuid);
    WriteField(W, FIELD_END);
    result := W.FlushTo;
  finally
    W.Free;
  end;
end;

procedure QuackNewQueryUuid(out QueryUuid: TDuckDBHugeInt);
begin
  QueryUuid.Lower := Int64(Random64);
  QueryUuid.Upper := Int64(Random64 shr 1) or 1; // not 0: this is a wildcard
end;

function QuackUriParse(const Uri: RawUtf8; out Host: RawUtf8;
  out Port: integer): boolean;
var
  s, ports: RawUtf8;
  p: PtrInt;
begin
  result := false;
  Port := QUACK_DEFAULT_PORT;
  s := TrimU(Uri);
  if IdemPChar(pointer(s), 'QUACK://') then
    delete(s, 1, 8)
  else if IdemPChar(pointer(s), 'QUACK:') then
    delete(s, 1, 6);
  ports := '';
  if (s <> '') and
     (s[1] = '[') then
  begin
    // IPv6 address
    p := PosExChar(']', s);
    if p = 0 then
      exit;
    Host := copy(s, 2, p - 2);
    if p < length(s) then
      if s[p + 1] = ':' then
        ports := copy(s, p + 2, 100)
      else
        exit;
  end
  else
  begin
    p := PosExChar(':', s);
    if p = 0 then
      Host := s
    else
    begin
      Host := copy(s, 1, p - 1);
      ports := copy(s, p + 1, 100);
    end;
  end;
  if Host = '' then
    exit;
  if ports <> '' then
  begin
    p := GetCardinalDef(pointer(ports), 0);
    if (p <= 0) or
       (p > 65535) or
       (UInt32ToUtf8(p) <> ports) then
      exit;
    Port := p;
  end;
  result := true;
end;

function QuackHostIsLocal(const Host: RawUtf8): boolean;
begin
  result := IdemPropNameU(Host, 'localhost') or
            (Host = '127.0.0.1') or
            (Host = '::1');
end;


initialization

finalization
  FreeAndNil(DuckDB);

end.
