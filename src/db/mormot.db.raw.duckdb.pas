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
    - Value Conversion Helpers (hugeint, decimal, date/time, string_t)

  *****************************************************************************
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.db.core,
  mormot.db.sql;


{ ************ DuckDB C API Types and Constants }

type
  /// exception raised by the DuckDB providers
  ESqlDBDuckDB = class(ESqlDBException);

  /// DuckDB idx_t - always 64-bit, even on 32-bit platforms
  TDuckIdx = Int64;

  /// duckdb_state result code
  TDuckDBState = integer;

  /// duckdb_type enumeration (DUCKDB_TYPE_* constants)
  TDuckDBType = integer;

  /// opaque handles - pointers to a struct (void* internal_ptr)
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
    deprecated_error_message: PAnsiChar;
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

  /// DuckDB TIMESTAMP 'infinity' / '-infinity' special values
  DUCKDB_TIMESTAMP_INFINITY  = $7fffffffffffffff;
  DUCKDB_TIMESTAMP_NINFINITY = -$7fffffffffffffff;

  /// days between the TDateTime epoch (1899-12-30) and 1970-01-01
  DUCKDB_UNIXDATE_DELTA = 25569;
  DUCKDB_MICROSPERDAY   = Int64(86400000000);

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
    library_version: function: PAnsiChar; cdecl;
    open_ext: function(path: PAnsiChar; out database: TDuckDBDatabase;
      config: TDuckDBConfig; out_error: PPAnsiChar): TDuckDBState; cdecl;
    close: procedure(var database: TDuckDBDatabase); cdecl;
    connect: function(database: TDuckDBDatabase;
      out connection: TDuckDBConnection): TDuckDBState; cdecl;
    disconnect: procedure(var connection: TDuckDBConnection); cdecl;
    interrupt: procedure(connection: TDuckDBConnection); cdecl;
    create_config: function(out config: TDuckDBConfig): TDuckDBState; cdecl;
    set_config: function(config: TDuckDBConfig;
      name, option: PAnsiChar): TDuckDBState; cdecl;
    destroy_config: procedure(var config: TDuckDBConfig); cdecl;
    query: function(connection: TDuckDBConnection; sql: PAnsiChar;
      out_result: PDuckDBResult): TDuckDBState; cdecl;
    destroy_result: procedure(res: PDuckDBResult); cdecl;
    column_name: function(res: PDuckDBResult; col: TDuckIdx): PAnsiChar; cdecl;
    column_type: function(res: PDuckDBResult; col: TDuckIdx): TDuckDBType; cdecl;
    column_logical_type: function(res: PDuckDBResult;
      col: TDuckIdx): TDuckDBLogicalType; cdecl;
    column_count: function(res: PDuckDBResult): TDuckIdx; cdecl;
    rows_changed: function(res: PDuckDBResult): TDuckIdx; cdecl;
    result_error: function(res: PDuckDBResult): PAnsiChar; cdecl;
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
      index: TDuckIdx): PAnsiChar; cdecl;
    list_type_child_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    array_type_child_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    array_type_array_size: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    struct_type_child_count: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    struct_type_child_name: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): PAnsiChar; cdecl;
    struct_type_child_type: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): TDuckDBLogicalType; cdecl;
    map_type_key_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    map_type_value_type: function(lt: TDuckDBLogicalType): TDuckDBLogicalType; cdecl;
    union_type_member_count: function(lt: TDuckDBLogicalType): TDuckIdx; cdecl;
    union_type_member_name: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): PAnsiChar; cdecl;
    union_type_member_type: function(lt: TDuckDBLogicalType;
      index: TDuckIdx): TDuckDBLogicalType; cdecl;
    destroy_logical_type: procedure(var lt: TDuckDBLogicalType); cdecl;
    prepare: function(connection: TDuckDBConnection; query: PAnsiChar;
      out stmt: TDuckDBPreparedStatement): TDuckDBState; cdecl;
    destroy_prepare: procedure(var stmt: TDuckDBPreparedStatement); cdecl;
    prepare_error: function(stmt: TDuckDBPreparedStatement): PAnsiChar; cdecl;
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
      index: TDuckIdx; value: PAnsiChar; length: TDuckIdx): TDuckDBState; cdecl;
    bind_blob: function(stmt: TDuckDBPreparedStatement; index: TDuckIdx;
      data: pointer; length: TDuckIdx): TDuckDBState; cdecl;
    bind_null: function(stmt: TDuckDBPreparedStatement;
      index: TDuckIdx): TDuckDBState; cdecl;
    execute_prepared: function(stmt: TDuckDBPreparedStatement;
      out_result: PDuckDBResult): TDuckDBState; cdecl;
    free_: procedure(ptr: pointer); cdecl; // duckdb_free - renamed to not hide TObject.Free
    // appender API - resolved as optional entries
    appender_create: function(connection: TDuckDBConnection;
      schema, table: PAnsiChar; out appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_error: function(appender: TDuckDBAppender): PAnsiChar; cdecl;
    appender_flush: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_close: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_destroy: function(var appender: TDuckDBAppender): TDuckDBState; cdecl;
    appender_end_row: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    append_bool: function(appender: TDuckDBAppender; value: boolean): TDuckDBState; cdecl;
    append_int64: function(appender: TDuckDBAppender; value: Int64): TDuckDBState; cdecl;
    append_double: function(appender: TDuckDBAppender; value: double): TDuckDBState; cdecl;
    append_timestamp: function(appender: TDuckDBAppender;
      micros: Int64): TDuckDBState; cdecl;
    append_varchar_length: function(appender: TDuckDBAppender; value: PAnsiChar;
      length: TDuckIdx): TDuckDBState; cdecl;
    append_blob: function(appender: TDuckDBAppender; data: pointer;
      length: TDuckIdx): TDuckDBState; cdecl;
    append_null: function(appender: TDuckDBAppender): TDuckDBState; cdecl;
    /// TRUE if the appender API was successfully resolved
    HasAppender: boolean;
    /// load the library and resolve all entries
    // - raise ESqlDBDuckDB on failure
    constructor Create; override;
    /// return the DuckDB library version as text, e.g. 'v1.4.1'
    function Version: RawUtf8;
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


{ ************ Value Conversion Helpers (hugeint, decimal, date/time, string_t) }

/// return the text pointer and length of a duckdb_string_t
// - warning: len should be defined as an exact integer type, not a PtrInt
function DuckStringPtr(s: PDuckDBStringT; var len: integer): PAnsiChar;
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

/// convert an integer mantissa text into a decimal number text with scale
// - e.g. ('-12345', 2) -> '-123.45'
procedure DuckApplyScale(var Text: RawUtf8; Scale: integer);

/// return TRUE if the hugeint fits into an Int64
function DuckHugeIntIsInt64(const v: TDuckDBHugeInt; Signed: boolean): boolean;

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

/// convert a DuckDB UUID (hugeint with flipped sign bit) into its text
procedure DuckUuidToText(const v: TDuckDBHugeInt; var result: RawUtf8);

/// convert a DuckDB BIT string_t content into '0101...' text
procedure DuckBitToText(P: PDuckU8Array; Len: integer; var result: RawUtf8);

/// decode a DuckDB TIME_TZ 64-bit value into micros and offset in seconds
procedure DuckTimeTzDecode(Bits: Int64; out Micros: Int64; out OffsetSec: integer);


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
    'duckdb_', nil);
end;

function TDuckDBLib.Version: RawUtf8;
begin
  result := RawUtf8(library_version);
end;

procedure TDuckDBLib.RaiseResultError(res: PDuckDBResult; const ctxt: RawUtf8);
var
  msg: RawUtf8;
begin
  msg := RawUtf8(result_error(res));
  destroy_result(res);
  raise ESqlDBDuckDB.CreateUtf8('DuckDB % failed: %', [ctxt, msg]);
end;

function TDuckDBLib.Exec(conn: TDuckDBConnection; const sql: RawUtf8): Int64;
var
  res: TDuckDBResult;
begin
  FillCharFast(res, SizeOf(res), 0);
  if query(conn, pointer(sql), @res) <> DuckDBSuccess then
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

function DuckStringPtr(s: PDuckDBStringT; var len: integer): PAnsiChar;
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

type
  TU128 = TBlock128; // little-endian 32-bit limbs

procedure U128Negate(var v: TU128);
var
  i: PtrInt;
  c: Int64;
begin
  c := 1;
  for i := 0 to 3 do
  begin
    c := Int64(not v[i]) + c;
    v[i] := cardinal(c);
    c := c shr 32;
  end;
end;

function DivU128By1B(var u: TU128): cardinal;
  {$ifdef HASINLINE} inline; {$endif}
var
  r, cur: QWord;
  i: PtrInt;
begin // see mormot.db.nosql.bson's div128bits9digits()
  r := 0;
  for i := 3 downto 0 do
  begin
    cur := (r shl 32) or QWord(u[i]);
    if cur = 0 then
      continue;
    u[i] := cardinal(cur div 1000000000);
    r := cur - QWord(u[i]) * 1000000000;
  end;
  result := cardinal(r);
end;

function DuckHugeIntToTextBuf(const v: TDuckDBHugeInt;
  Signed: boolean; Scale: integer; var Buffer: TTemp47): PtrInt;
var
  u: TU128;
  neg: boolean;
  i, digits: PtrInt;
  n, q: cardinal;
  p, first, last: PAnsiChar;
begin
  if Scale < 0 then
    Scale := 0
  else if Scale > 38 then
    raise ERangeError.Create('Invalid DuckDB decimal scale');
  PInt64(@u[0])^ := v.Lower;
  PInt64(@u[2])^ := v.Upper;
  neg := Signed and (v.Upper < 0);
  if neg then
    U128Negate(u);
  // reserve one byte for the terminating #0
  last := @Buffer[high(Buffer)];
  p := last - 1;
  repeat
    n := DivU128By1B(u);
    if (u[0] or u[1] or u[2] or u[3]) = 0 then
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

function DuckHugeIntIsInt64(const v: TDuckDBHugeInt; Signed: boolean): boolean;
begin
  if Signed then
    result := ((v.Upper = 0) and (v.Lower >= 0)) or
              ((v.Upper = -1) and (v.Lower < 0))
  else
    result := (v.Upper = 0) and (v.Lower >= 0);
end;

procedure DuckApplyScale(var Text: RawUtf8; Scale: integer);
var
  neg: boolean;
  digits: RawUtf8;
begin
  if Scale <= 0 then
    exit;
  neg := (Text <> '') and (Text[1] = '-');
  if neg then
    digits := copy(Text, 2, maxInt)
  else
    digits := Text;
  while length(digits) <= Scale do
    digits := '0' + digits;
  digits := copy(digits, 1, length(digits) - Scale) + '.' +
            copy(digits, length(digits) - Scale + 1, Scale);
  if neg then
    Text := '-' + digits
  else
    Text := digits;
end;

const
  // TSynSystemTime.FromUnixTime supports year 0000 only from March onward
  // - all earlier dates and years >= 10000 are outside this unit's scope
  DUCKDB_FIRST_DAY           = -719468;   // 0000-03-01
  DUCKDB_FIRST_TDATETIME_DAY = -719162; // 0001-01-01
  DUCKDB_LAST_DAY            = 2932896;   // 9999-12-31
  DUCKDB_MICROSPERSEC        = Int64(1000000);
  DUCKDB_MICROSPERHOUR       = Int64(3600000000);

procedure DuckCheckDay(Days: Int64; ForDateTime: boolean = false);
begin
  if (Days < DUCKDB_FIRST_DAY) or
     (Days > DUCKDB_LAST_DAY) or
     (ForDateTime and (Days < DUCKDB_FIRST_TDATETIME_DAY)) then
    raise ERangeError.Create('DuckDB date outside TSynSystemTime range');
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
  s: RawUtf8;
begin
  // TSynSystemTime handles milliseconds; preserve DuckDB's full microseconds.
  if Micro <> 0 then
  begin
    s := Int64ToUtf8(Micro + DUCKDB_MICROSPERSEC);
    s[1] := '.';
    while s[length(s)] = '0' do
      SetLength(s, length(s) - 1);
    result := result + s;
  end;
end;

procedure DuckTimeToText(Micros: Int64; var result: RawUtf8);
var
  st: TSynSystemTime;
  u: QWord;
begin
  // DuckDB INTERVAL uses this formatter too, including negative/>24h values.
  if Micros < 0 then
    u := QWord(-(Micros + 1)) + 1 // also safe for Low(Int64)
  else
    u := QWord(Micros);
  st.FromMS(PtrUInt((u mod QWord(DUCKDB_MICROSPERDAY)) div 1000));
  st.ToIsoTime(result, '');
  if u >= QWord(DUCKDB_MICROSPERDAY) then
    result := Int64ToUtf8(Int64(u div QWord(DUCKDB_MICROSPERHOUR))) +
              copy(result, 3, maxInt);
  DuckAppendMicroFraction(Int64(u mod DUCKDB_MICROSPERSEC), result);
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
    // Floor division correctly handles timestamps before the Unix epoch.
    FloorDivMod(Micros, DUCKDB_MICROSPERSEC, secs, micro);
    FloorDivMod(secs, SecsPerDay, days, rem);
    DuckCheckDay(days);
    st.FromUnixTime(secs);
    st.ToIsoDateTime(result, ' ');
    DuckAppendMicroFraction(micro, result);
  end;
end;

function DuckTimestampToDateTime(Micros: Int64): TDateTime;
var
  st: TSynSystemTime;
  ms, micro, days, rem: Int64;
begin
  FloorDivMod(Micros, 1000, ms, micro);
  if micro >= 500 then
    inc(ms); // nearest millisecond (TDateTime cannot store microseconds)
  FloorDivMod(ms, MSecsPerDay, days, rem);
  DuckCheckDay(days, {ForDateTime=}true);
  st.FromUnixMsTime(ms);
  result := st.ToDateTime;
  // TSynSystemTime.ToDateTime uses a positive time fraction. Delphi instead
  // requires a negative fraction when the date part is before 1899-12-30.
  if days + DUCKDB_UNIXDATE_DELTA < 0 then
    result := result - 2 * (rem / MSecsPerDay);
end;

function DateTimeToDuckTimestamp(Value: TDateTime): Int64;
var
  st: TSynSystemTime;
begin
  st.FromDateTime(Value);
  if (st.Year = 0) or (st.Year > 9999) then
    raise ERangeError.Create('TDateTime year outside 1..9999');
  result := st.ToUnixMsTime * 1000;
end;

procedure AddIntervalPart(var result: RawUtf8; n: integer; const unitname: RawUtf8);
var
  s: RawUtf8;
begin
  if n = 0 then
    exit;
  s := Int32ToUtf8(n) + ' ' + unitname;
  if (n > 1) or (n < -1) then
    s := s + 's';
  if result = '' then
    result := s
  else
    result := result + ' ' + s;
end;

procedure DuckIntervalToText(const v: TDuckDBInterval; var result: RawUtf8);
var
  t: RawUtf8;
begin
  result := '';
  AddIntervalPart(result, v.Months div 12, 'year');
  AddIntervalPart(result, v.Months mod 12, 'month');
  AddIntervalPart(result, v.Days, 'day');
  if (v.Micros <> 0) or (result = '') then
  begin
    DuckTimeToText(v.Micros, t);
    if result = '' then
      result := t
    else
      result := result + ' ' + t;
  end;
end;

procedure DuckUuidToText(const v: TDuckDBHugeInt; var result: RawUtf8);
var
  b: array[0..15] of byte;
  i: PtrInt;
  hi, lo: Int64;
  h: RawUtf8;
begin
  hi := v.Upper xor Int64($8000000000000000); // DuckDB flips the top bit
  lo := v.Lower;
  for i := 0 to 7 do
  begin
    b[7 - i] := byte(hi shr (i * 8));
    b[15 - i] := byte(lo shr (i * 8));
  end;
  h := LowerCase(BinToHex(@b, 16));
  result := Join([copy(h, 1, 8), '-', copy(h, 9, 4), '-', copy(h, 13, 4), '-',
    copy(h, 17, 4), '-', copy(h, 21, 12)]);
end;

procedure DuckBitToText(P: PDuckU8Array; Len: integer; var result: RawUtf8);
var
  padding, i, n: PtrInt;
  c: PUtf8Char;
begin
  result := '';
  if Len < 1 then
    exit;
  padding := P^[0];
  n := (Len - 1) * 8 - padding;
  if n <= 0 then
    exit;
  FastSetString(result, nil, n);
  c := pointer(result);
  for i := padding to (Len - 1) * 8 - 1 do
  begin
    if (P^[1 + i shr 3] shr (7 - (i and 7))) and 1 <> 0 then
      c^ := '1'
    else
      c^ := '0';
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


initialization

finalization
  FreeAndNil(DuckDB);

end.
