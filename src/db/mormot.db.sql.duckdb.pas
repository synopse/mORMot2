/// Database Framework Direct DuckDB Connection via the native C API
// - in-process access using duckdb.dll / libduckdb.so (x64)
// - Delphi 7 .. Delphi 13 and FPC 3.2+ compatible syntax
unit mormot.db.sql.duckdb;

{
  *****************************************************************************

   Direct DuckDB Client Access using mormot.db.raw.duckdb C API Wrapper
    - TSqlDBDuckDBConnectionProperties: one shared duckdb_database instance
    - TSqlDBDuckDBConnection: one duckdb_connection per (thread) connection
    - TSqlDBDuckDBStatement: prepared statements and chunk-based row access

  *****************************************************************************

  Notes:
  - DuckDB SQL is very close to PostgreSQL, so dPostgreSQL is used as
    TSqlDBDefinition (LIMIT syntax, DDL types, information_schema metadata)
  - one database file can only be opened by ONE process in read/write mode:
    the duckdb_database handle is owned by the connection properties instance,
    and every TSqlDBConnection runs duckdb_connect() on it
  - result sets are materialized by DuckDB, then read chunk by chunk
  - complex types (LIST, ARRAY, STRUCT, MAP, UNION) are returned as JSON text
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb;


{ ************ TSqlDBDuckDBConnection* and TSqlDBDuckDBStatement Classes }

type
  /// connection properties to an in-process DuckDB database
  // - aServerName is the database file name, or '' / ':memory:' for an
  // in-memory database shared by all connections of this instance
  // - aDatabaseName/aUserID/aPassWord are ignored
  TSqlDBDuckDBConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fDatabase: TDuckDBDatabase;
    fDatabaseLock: TLightLock;
    fConfig: RawUtf8;
    fReadOnly: boolean;
    procedure GetForeignKeys; override;
  public
    /// initialize the properties - the database is opened on first connection
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
      override;
    /// close the shared duckdb_database instance after all connections
    destructor Destroy; override;
    /// create a new connection - caller should free it
    function NewConnection: TSqlDBConnection; override;
    /// return the shared duckdb_database handle, opening it if needed
    function Database: TDuckDBDatabase;
    /// close the shared database instance
    // - all connections should have been released before
    procedure CloseDatabase;
    /// the DuckDB library version, e.g. 'v1.4.1'
    function LibraryVersion: RawUtf8;
  published
    /// DuckDB configuration options, applied when the database is opened
    // - format is 'name1=value1;name2=value2', e.g. 'threads=4;memory_limit=2GB'
    // - see https://duckdb.org/docs/stable/configuration/overview
    property Config: RawUtf8
      read fConfig write fConfig;
    /// open the database file in READ_ONLY access mode
    // - several processes may open the same file in read-only mode
    property ReadOnly: boolean
      read fReadOnly write fReadOnly;
  end;

  /// one connection to the shared DuckDB database instance
  TSqlDBDuckDBConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fConn: TDuckDBConnection;
  public
    /// connect to the database, i.e. run duckdb_connect()
    procedure Connect; override;
    /// release the duckdb_connection
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// initialize a new SQL query statement for this connection
    function NewStatement: TSqlDBStatement; override;
    /// BEGIN TRANSACTION
    procedure StartTransaction; override;
    /// COMMIT
    procedure Commit; override;
    /// ROLLBACK
    procedure Rollback; override;
    /// execute one or several ';' separated statements without parameters
    // - e.g. for DDL scripts, COPY, ATTACH, INSTALL/LOAD, PRAGMA...
    // - returns the number of changed rows of the last statement
    function ExecuteScript(const aSql: RawUtf8): Int64;
    /// interrupt any running query of this connection (thread-safe)
    procedure Interrupt;
    /// direct access to the duckdb_connection handle
    property Handle: TDuckDBConnection
      read fConn;
  end;

  /// cached information about one DuckDB result column
  TDuckDBColumnInfo = record
    /// 32-bit ordinal number corresponding to DUCKDB_TYPE_* constants
    TypeId: TDuckDBType;
    /// number precision retrieved for DECIMAL - 0 means integer
    Scale: integer;
    /// internal nested storage type for DECIMAL and ENUM
    Internal: TDuckDBType;
    /// DuckDB type info structure
    LogicalType: TDuckDBLogicalType;
    /// internal cache of ENUM values
    EnumValues: TRawUtf8DynArray;
    /// current chunk content
    Vector: TDuckDBVector;
    /// current position in the data buffer
    Data: pointer;
    /// cached vector_get_validity() values
    Validity: PDuckI64Array;
  end;
  PDuckDBColumnInfo = ^TDuckDBColumnInfo;
  TDuckDBColumnInfoDynArray = array of TDuckDBColumnInfo;

  /// a DuckDB prepared statement
  TSqlDBDuckDBStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fStmt: TDuckDBPreparedStatement;
    fRes: TDuckDBResult;
    fHasResult: boolean;
    fChunk: TDuckDBDataChunk;
    fChunkSize: PtrInt;
    fChunkRow: PtrInt;
    fUpdateCount: integer;
    fDirectQuery: boolean;
    fInfo: TDuckDBColumnInfoDynArray;
    procedure BindParams;
    procedure BindArrayRow(Row: integer; var Temp: RawUtf8);
    procedure ExecuteArray;
    procedure ReleaseResult;
    function NextChunk: boolean;
    function CheckCol(Col: integer): PDuckDBColumnInfo;
      {$ifdef HASINLINE} inline; {$endif}
    function DecimalMantissa(c: PDuckDBColumnInfo; out hi: Int64): Int64;
  public
    /// release all associated DuckDB handles
    destructor Destroy; override;
    /// prepare an UTF-8 encoded SQL statement with ? parameters
    procedure Prepare(const aSql: RawUtf8; ExpectResults: boolean = false);
      overload; override;
    /// execute the prepared statement with the bound parameters
    procedure ExecutePrepared; override;
    /// number of rows changed by the last INSERT/UPDATE/DELETE
    function UpdateCount: integer; override;
    /// reset the statement for a new execution with new parameters
    procedure Reset; override;
    /// release the current result set
    procedure ReleaseRows; override;
    /// move to the next row of the result set
    function Step(SeekFirst: boolean = false): boolean; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a column integer value (BOOLEAN as 0/1)
    function ColumnInt(Col: integer): Int64; override;
    /// return a column floating point value
    function ColumnDouble(Col: integer): double; override;
    /// return a column date/time value
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a column currency value - exact for DECIMAL(n,<=4)
    function ColumnCurrency(Col: integer): currency; override;
    /// return a column as UTF-8 text, in DuckDB native text format
    // - LIST/STRUCT/MAP/ARRAY/UNION are returned as JSON
    function ColumnUtf8(Col: integer): RawUtf8; override;
    /// return a column as binary content
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append a column value as JSON
    procedure ColumnToJson(Col: integer; W: TJsonWriter); override;
    /// low-level access to the DuckDB logical type ID of a column
    function ColumnDuckDBType(Col: integer): TDuckDBType;
  end;


  /// fast bulk loading of rows into a DuckDB table via the Appender API
  // - typically 10-100 times faster than INSERT statements
  // - values should be added in the table column order, then EndRow called
  // - data is flushed on Flush/Close/Destroy, constraint violations are
  // reported at this time
  TSqlDBDuckDBAppender = class
  protected
    fAppender: TDuckDBAppender;
    fRowCount: Int64;
    fTable: RawUtf8;
    procedure Check(res: TDuckDBState; const ctxt: RawUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    procedure Failed(res: TDuckDBState; const ctxt: RawUtf8);
  public
    /// create an appender for a given table of an opened connection
    constructor Create(aConnection: TSqlDBDuckDBConnection;
      const aTable: RawUtf8; const aSchema: RawUtf8 = '');
    /// flush and release the appender
    destructor Destroy; override;
    /// append a NULL value
    procedure AddNull;
    /// append an integer value
    procedure Add(Value: Int64); overload;
    /// append a floating point value
    procedure AddDouble(Value: double);
    /// append a currency value - exact for DECIMAL columns
    procedure AddCurrency(Value: currency);
    /// append a boolean value
    procedure AddBoolean(Value: boolean);
    /// append a TDateTime value as TIMESTAMP
    procedure AddDateTime(Value: TDateTime);
    /// append some UTF-8 text - also usable for any castable column type
    procedure AddText(const Value: RawUtf8); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// append some UTF-8 text - also usable for any castable column type
    procedure AddText(Value: PUtf8Char; Len: PtrInt); overload;
    /// append some binary content
    procedure AddBlob(const Value: RawByteString);
    /// append all values of a row, from Delphi values
    // - integer, Int64, double, currency, RawUtf8/string, boolean, nil=NULL
    procedure AddRow(const Values: array of const);
    /// finish the current row
    procedure EndRow;
    /// flush the pending rows to the table
    procedure Flush;
    /// flush and close the appender
    procedure Close;
    /// number of rows appended so far
    property RowCount: Int64
      read fRowCount;
  end;


/// append the value at Row of a DuckDB vector as JSON
// - handles all scalar types, and nested LIST/ARRAY/STRUCT/MAP/UNION
procedure DuckDBVectorValueToJson(vec: TDuckDBVector; lt: TDuckDBLogicalType;
  row: PtrInt; W: TJsonWriter);


/// return the value at Row of a DuckDB vector as text
// - scalar values in DuckDB text format, nested values as JSON
procedure DuckDBVectorValueToText(vec: TDuckDBVector; lt: TDuckDBLogicalType;
  row: PtrInt; var result: RawUtf8);

/// fill the simple value at Row of a DuckDB vector into a text buffer
// - returns the number of bytes written, or -1 if this type is not supported
function DuckDBVectorValueToTextBuf(vec: TDuckDBVector;  lt: TDuckDBLogicalType;
  row: PtrInt; var Buffer: TTemp47): PtrInt;


implementation


{ ************ Vector Value Conversion }

const
  POW10: array[0..18] of Int64 = (1, 10, 100, 1000, 10000, 100000, 1000000,
    10000000, 100000000, 1000000000, 10000000000, 100000000000,
    1000000000000, 10000000000000, 100000000000000, 1000000000000000,
    10000000000000000, 100000000000000000, 1000000000000000000);

function IsValid(validity: PDuckI64Array; row: PtrInt): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (validity = nil) or
            ((validity^[row shr 6] shr (row and 63)) and 1 <> 0);
end;

function DuckDBTypeToField(typeid: TDuckDBType; scale: integer): TSqlDBFieldType;
begin
  case typeid of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER, DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
      result := ftInt64;
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      result := ftDouble;
    DUCKDB_TYPE_DECIMAL:
      if scale <= 4 then
        result := ftCurrency
      else
        result := ftDouble;
    DUCKDB_TYPE_DATE, DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIME_TZ, DUCKDB_TYPE_TIME_NS,
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S, DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS, DUCKDB_TYPE_TIMESTAMP_TZ:
      result := ftDate;
    DUCKDB_TYPE_BLOB:
      result := ftBlob;
    DUCKDB_TYPE_SQLNULL:
      result := ftNull;
  else
    result := ftUtf8; // VARCHAR, UUID, ENUM, INTERVAL, BIT, nested as JSON
  end;
end;

function TimestampMicros(typeid: TDuckDBType; v: Int64): Int64;
begin
  case typeid of
    DUCKDB_TYPE_TIMESTAMP_S:
      result := v * 1000000;
    DUCKDB_TYPE_TIMESTAMP_MS:
      result := v * 1000;
    DUCKDB_TYPE_TIMESTAMP_NS:
      result := v div 1000;
  else
    result := v;
  end;
end;

function ReadIntegral(typeid: TDuckDBType; data: pointer; row: PtrInt): Int64;
begin
  case typeid of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_UTINYINT:
      result := PDuckU8Array(data)^[row];
    DUCKDB_TYPE_TINYINT:
      result := PDuckI8Array(data)^[row];
    DUCKDB_TYPE_SMALLINT:
      result := PDuckI16Array(data)^[row];
    DUCKDB_TYPE_USMALLINT:
      result := PDuckU16Array(data)^[row];
    DUCKDB_TYPE_INTEGER:
      result := PDuckI32Array(data)^[row];
    DUCKDB_TYPE_UINTEGER:
      result := PDuckU32Array(data)^[row];
  else // BIGINT, UBIGINT, DATE-related int64
    result := PDuckI64Array(data)^[row];
  end;
end;

procedure DecimalValue(lt: TDuckDBLogicalType; data: pointer; row: PtrInt;
  var mantissa: TDuckDBHugeInt; var scale: integer);
var
  v: Int64;
begin
  scale := DuckDB.decimal_scale(lt);
  case DuckDB.decimal_internal_type(lt) of
    DUCKDB_TYPE_HUGEINT:
      begin
        mantissa := PDuckDBHugeInt(PAnsiChar(data) + row * SizeOf(TDuckDBHugeInt))^;
        exit;
      end;
    DUCKDB_TYPE_SMALLINT:
      v := PDuckI16Array(data)^[row];
    DUCKDB_TYPE_INTEGER:
      v := PDuckI32Array(data)^[row];
  else
    v := PDuckI64Array(data)^[row];
  end;
  mantissa.Lower := v;
  if v < 0 then
    mantissa.Upper := -1
  else
    mantissa.Upper := 0;
end;

procedure EnumValue(lt: TDuckDBLogicalType; data: pointer; row: PtrInt;
  var result: RawUtf8);
var
  idx: Int64;
  p: PUtf8Char;
begin
  idx := ReadIntegral(DuckDB.enum_internal_type(lt), data, row);
  p := DuckDB.enum_dictionary_value(lt, idx);
  FastSetString(result, p, StrLen(p));
  DuckDB.free_(p);
end;

function DuckDBVectorValueToTextBuf(vec: TDuckDBVector;  lt: TDuckDBLogicalType;
  row: PtrInt; var Buffer: TTemp47): PtrInt;
var
  data: pointer;
  typeid: TDuckDBType;
  scale: integer;
  p, num: PAnsiChar;
  h: TDuckDBHugeInt;
  d: double;
  tmp: ShortString;
begin
  result := 0;
  Buffer[0] := #0;
  if not IsValid(DuckDB.vector_get_validity(vec), row) then
    exit;
  data := DuckDB.vector_get_data(vec);
  typeid := DuckDB.get_type_id(lt);
  num := nil;
  case typeid of
    DUCKDB_TYPE_BOOLEAN:
      begin
        p := @BOOL_STR[PDuckU8Array(data)^[row] <> 0]; // 'false'/'true'
        PInt64(@Buffer)^ := PInt64(p + 1)^;            // include ending #0
        result := ord(p[0]);
      end;
    DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      num := StrInt64(@Buffer[high(Buffer)],
        ReadIntegral(typeid, data, row));
    DUCKDB_TYPE_UBIGINT:
      num := StrUInt64(@Buffer[high(Buffer)],
        QWord(PDuckI64Array(data)^[row]));
    DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
      result := DuckHugeIntToTextBuf(
        PDuckDBHugeInt(PAnsiChar(data) + row * 16)^,
        typeid = DUCKDB_TYPE_HUGEINT, 0, Buffer);
    DUCKDB_TYPE_DECIMAL:
      begin
        DecimalValue(lt, data, row, h, scale);
        result := DuckHugeIntToTextBuf(h, true, scale, Buffer);
      end;
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      begin
        if typeid = DUCKDB_TYPE_FLOAT then
          d := PDuckSingleArray(data)^[row]
        else
          d := PDuckDoubleArray(data)^[row];
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
      result := DuckUuidToTextBuf(
        PDuckDBHugeInt(PAnsiChar(data) + row * 16)^, Buffer);
    DUCKDB_TYPE_SQLNULL:
      ; // NULL -> empty buffer
  else
    // Dates, times, intervals, ENUM, VARCHAR, DUCKDB_TYPE_BIT and nested types
    result := -1;
  end;
  // normalize the backward-generated 64-bit integer text
  if num = nil then
   exit;
  result := @Buffer[high(Buffer)] - num;
  MoveFast(num^, Buffer[0], result);
  Buffer[result] := #0;
end;

procedure ComplexDuckDBVectorValueToText(vec: TDuckDBVector;
  lt: TDuckDBLogicalType; row: PtrInt; var result: RawUtf8);
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    DuckDBVectorValueToJson(vec, lt, row, W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure DuckDBVectorValueToText(vec: TDuckDBVector;
  lt: TDuckDBLogicalType; row: PtrInt; var result: RawUtf8);
var
  typeid: TDuckDBType;
  len, offs: integer;
  data: PAnsiChar;
  p: pointer;
  us: Int64;
  d: double;
  buf: TTemp47;
begin
  len := DuckDBVectorValueToTextBuf(vec, lt, row, buf);
  if len >= 0 then
  begin
    FastSetString(result, @buf[0], len); // most simple types
    exit;
  end;
  FastAssignNew(result);
  data := DuckDB.vector_get_data(vec);
  typeid := DuckDB.get_type_id(lt);
  case typeid of
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      begin
        if typeid = DUCKDB_TYPE_FLOAT then
          d := PDuckSingleArray(data)^[row]
        else
          d := PDuckDoubleArray(data)^[row];
        DoubleToStr(d, result);
      end;
    DUCKDB_TYPE_DATE:
      DuckDateToText(PDuckI32Array(data)^[row], result);
    DUCKDB_TYPE_TIME:
      DuckTimeToText(PDuckI64Array(data)^[row], result);
    DUCKDB_TYPE_TIME_NS:
      DuckTimeToText(PDuckI64Array(data)^[row] div 1000, result);
    DUCKDB_TYPE_TIME_TZ:
      begin
        DuckTimeTzDecode(PDuckI64Array(data)^[row], us, offs);
        DuckTimeToText(us, result);
        if offs < 0 then
          result := result + '-'
        else
          result := result + '+';
        offs := abs(offs);
        AppendShortToUtf8(UInt2DigitsToShortFast(offs div 3600), result);
        if offs mod 3600 <> 0 then
        begin
          result := result + ':';
          AppendShortToUtf8(
            UInt2DigitsToShortFast((offs div 60) mod 60), result);
        end;
      end;
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
      DuckTimestampToText(
        TimestampMicros(typeid, PDuckI64Array(data)^[row]), result);
    DUCKDB_TYPE_TIMESTAMP_TZ:
      begin
        DuckTimestampToText(PDuckI64Array(data)^[row], result);
        result := result + '+00'; // stored as UTC
      end;
    DUCKDB_TYPE_INTERVAL:
      DuckIntervalToText(
        PDuckDBInterval(data + row * 16)^, result);
    DUCKDB_TYPE_VARCHAR, DUCKDB_TYPE_BLOB:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        FastSetString(result, p, len);
      end;
    DUCKDB_TYPE_BIT:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        DuckBitToText(pointer(p), len, result);
      end;
    DUCKDB_TYPE_ENUM:
      EnumValue(lt, data, row, result);
  else
    // LIST, ARRAY, STRUCT, MAP, UNION -> JSON
    ComplexDuckDBVectorValueToText(vec, lt, row, result);
  end;
end;

procedure ComplexDuckDBVectorValueToJson(vec: TDuckDBVector; data: PAnsiChar;
  lt: TDuckDBLogicalType; typeid: TDuckDBType; row: PtrInt; W: TJsonWriter);
var
  i: PtrInt;
  e: PDuckDBListEntry;
  child: TDuckDBVector;
  clt, klt: TDuckDBLogicalType;
  kvec, vvec: TDuckDBVector;
  txt: RawUtf8;
begin
  // those complex types require a temporary txt: RawUtf8 conversion
  case typeid of
    DUCKDB_TYPE_LIST, DUCKDB_TYPE_MAP:
      begin
        e := PDuckDBListEntry(data + row * SizeOf(TDuckDBListEntry));
        child := DuckDB.list_vector_get_child(vec);
        if typeid = DUCKDB_TYPE_LIST then
        begin
          clt := DuckDB.list_type_child_type(lt);
          W.Add('[');
          for i := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
          begin
            DuckDBVectorValueToJson(child, clt, i, W);
            W.AddComma;
          end;
          W.CancelLastComma(']');
          DuckDB.destroy_logical_type(clt);
        end
        else
        begin
          // MAP = LIST(STRUCT(key, value)) -> JSON object
          klt := DuckDB.map_type_key_type(lt);
          clt := DuckDB.map_type_value_type(lt);
          kvec := DuckDB.struct_vector_get_child(child, 0);
          vvec := DuckDB.struct_vector_get_child(child, 1);
          W.Add('{');
          for i := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
          begin
            DuckDBVectorValueToText(kvec, klt, i, txt);
            W.AddProp(pointer(txt), length(txt));
            DuckDBVectorValueToJson(vvec, clt, i, W);
            W.AddComma;
          end;
          W.CancelLastComma('}');
          DuckDB.destroy_logical_type(klt);
          DuckDB.destroy_logical_type(clt);
        end;
      end;
  else
    begin
      // ENUM, UUID, dates, INTERVAL, BIT -> JSON string
      DuckDBVectorValueToText(vec, lt, row, txt);
      W.Add('"');
      W.AddJsonEscape(pointer(txt), length(txt));
      W.Add('"');
    end;
  end;
end;

procedure DuckDBVectorValueToJson(vec: TDuckDBVector; lt: TDuckDBLogicalType;
  row: PtrInt; W: TJsonWriter);
var
  data: PAnsiChar;
  name, p: pointer;
  typeid: TDuckDBType;
  i, n, size: PtrInt; // no Int64 for-loop variable on Delphi 7
  len, scale: integer; // not PtrInt
  h: TDuckDBHugeInt;
  tag: integer;
  child: TDuckDBVector;
  clt: TDuckDBLogicalType;
  buf: TTemp47;
begin
  if not IsValid(DuckDB.vector_get_validity(vec), row) then
  begin
    W.AddNull;
    exit;
  end;
  data := DuckDB.vector_get_data(vec);
  typeid := DuckDB.get_type_id(lt);
  case typeid of
    DUCKDB_TYPE_BOOLEAN:
      W.Add(PDuckU8Array(data)^[row] <> 0);
    DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT, DUCKDB_TYPE_INTEGER,
    DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT,
    DUCKDB_TYPE_UINTEGER:
      W.Add(ReadIntegral(typeid, data, row));
    DUCKDB_TYPE_UBIGINT, DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
      begin
        n := DuckDBVectorValueToTextBuf(vec, lt, row, buf);
        W.AddNoJsonEscape(@buf[0], n);
      end;
    DUCKDB_TYPE_FLOAT:
      W.AddSingle(PDuckSingleArray(data)^[row]);
    DUCKDB_TYPE_DOUBLE:
      W.AddDouble(PDuckDoubleArray(data)^[row]);
    DUCKDB_TYPE_DECIMAL:
      begin
        DecimalValue(lt, data, row, h, scale);
        n := DuckHugeIntToTextBuf(h, true, scale, buf);
        W.AddNoJsonEscape(@buf[0], n);
      end;
    DUCKDB_TYPE_ARRAY:
      begin
        child := DuckDB.array_vector_get_child(vec);
        clt := DuckDB.array_type_child_type(lt);
        size := DuckDB.array_type_array_size(lt);
        W.Add('[');
        for i := row * size to row * size + size - 1 do
        begin
          DuckDBVectorValueToJson(child, clt, i, W);
          W.AddComma;
        end;
        W.CancelLastComma(']');
        DuckDB.destroy_logical_type(clt);
      end;
    DUCKDB_TYPE_STRUCT:
      begin
        n := DuckDB.struct_type_child_count(lt);
        W.Add('{');
        for i := 0 to n - 1 do
        begin
          name := DuckDB.struct_type_child_name(lt, i);
          W.AddProp(name, StrLen(name));
          DuckDB.free_(name);
          clt := DuckDB.struct_type_child_type(lt, i);
          DuckDBVectorValueToJson(DuckDB.struct_vector_get_child(vec, i), clt, row, W);
          DuckDB.destroy_logical_type(clt);
          W.AddComma;
        end;
        W.CancelLastComma('}');
      end;
    DUCKDB_TYPE_UNION:
      begin
        // UNION = STRUCT(tag UTINYINT, member1, member2, ...)
        tag := PDuckU8Array(DuckDB.vector_get_data(
          DuckDB.struct_vector_get_child(vec, 0)))^[row];
        clt := DuckDB.union_type_member_type(lt, tag);
        DuckDBVectorValueToJson(DuckDB.struct_vector_get_child(vec, tag + 1),
          clt, row, W);
        DuckDB.destroy_logical_type(clt);
      end;
    DUCKDB_TYPE_BLOB:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        W.WrBase64(p, len, {withmagic=}true);
      end;
    DUCKDB_TYPE_SQLNULL:
      W.AddNull;
    DUCKDB_TYPE_VARCHAR:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        W.Add('"');
        W.AddJsonEscape(p, len);
        W.Add('"');
      end
  else
    ComplexDuckDBVectorValueToJson(vec, data, lt, typeid, row, W);
  end;
end;


{ ************ TSqlDBDuckDBConnection* and TSqlDBDuckDBStatement Classes }

{ TSqlDBDuckDBConnectionProperties }

constructor TSqlDBDuckDBConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  DuckDBLibraryInitialize; // raise ESqlDBDuckDB if no duckdb.dll
  fDbms := dPostgreSQL;    // DuckDB SQL dialect is PostgreSQL-like
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  fEngineName := 'DuckDB';
  ForcedSchemaName := 'main'; // DuckDB default schema, for metadata queries
end;

destructor TSqlDBDuckDBConnectionProperties.Destroy;
begin
  inherited Destroy; // release all connections first
  CloseDatabase;
end;

function TSqlDBDuckDBConnectionProperties.LibraryVersion: RawUtf8;
begin
  result := DuckDB.Version;
end;

function TSqlDBDuckDBConnectionProperties.Database: TDuckDBDatabase;
var
  cfg: TDuckDBConfig;
  path, err: PUtf8Char;
  name, value, msg: RawUtf8;
  p: PUtf8Char;
  pair: RawUtf8;
begin
  result := fDatabase;
  if result <> nil then
    exit;
  fDatabaseLock.Lock;
  try
    if fDatabase = nil then
    begin
      cfg := nil;
      if DuckDB.create_config(cfg) <> DuckDBSuccess then
        ESqlDBDuckDB.RaiseU('duckdb_create_config failed');
      try
        if fReadOnly then
          DuckDB.set_config(cfg, 'access_mode', 'READ_ONLY');
        p := pointer(fConfig);
        while p <> nil do
        begin
          GetNextItem(p, ';', pair);
          Split(TrimU(pair), '=', name, value);
          name := TrimU(name);
          if name = '' then
            continue;
          if DuckDB.set_config(cfg, pointer(name), pointer(TrimU(value))) <>
               DuckDBSuccess then
            ESqlDBDuckDB.RaiseUtf8('%: invalid config option %=%',
              [self, name, value]);
        end;
        if (ServerName = '') or
           (ServerName = ':memory:') then
          path := nil
        else
          path := pointer(ServerName);
        err := nil;
        if DuckDB.open_ext(path, fDatabase, cfg, @err) <> DuckDBSuccess then
        begin
          msg := RawUtf8(err);
          if err <> nil then
            DuckDB.free_(err);
          fDatabase := nil;
          ESqlDBDuckDB.RaiseUtf8('%: cannot open [%]: %',
            [self, ServerName, msg]);
        end;
      finally
        DuckDB.destroy_config(cfg);
      end;
    end;
    result := fDatabase;
  finally
    fDatabaseLock.UnLock;
  end;
end;

procedure TSqlDBDuckDBConnectionProperties.CloseDatabase;
begin
  fDatabaseLock.Lock;
  try
    if fDatabase <> nil then
      DuckDB.close(fDatabase);
    fDatabase := nil;
  finally
    fDatabaseLock.UnLock;
  end;
end;

procedure TSqlDBDuckDBConnectionProperties.GetForeignKeys;
begin
  with Execute('select table_name || ''.'' || constraint_column_names[1], ' +
         'referenced_table || ''.'' || referenced_column_names[1] ' +
         'from duckdb_constraints() where constraint_type = ''FOREIGN KEY''',
         []) do
    while Step do
      fForeignKeys.Add(ColumnUtf8(0), ColumnUtf8(1));
end;

function TSqlDBDuckDBConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBDuckDBConnection.Create(self);
  TSqlDBDuckDBConnection(result).InternalProcess(speCreated);
end;


{ TSqlDBDuckDBConnection }

procedure TSqlDBDuckDBConnection.Connect;
var
  db: TDuckDBDatabase;
begin
  Disconnect;
  db := (fProperties as TSqlDBDuckDBConnectionProperties).Database;
  if DuckDB.connect(db, fConn) <> DuckDBSuccess then
  begin
    fConn := nil;
    ESqlDBDuckDB.RaiseUtf8('%.Connect: duckdb_connect failed', [self]);
  end;
  inherited Connect; // notify any re-connection
end;

procedure TSqlDBDuckDBConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fConn <> nil then
      DuckDB.disconnect(fConn);
    fConn := nil;
  end;
end;

function TSqlDBDuckDBConnection.IsConnected: boolean;
begin
  result := fConn <> nil;
end;

function TSqlDBDuckDBConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBDuckDBStatement.Create(self);
end;

function TSqlDBDuckDBConnection.ExecuteScript(const aSql: RawUtf8): Int64;
begin
  if not IsConnected then
    Connect;
  result := DuckDB.Exec(fConn, aSql);
end;

procedure TSqlDBDuckDBConnection.Interrupt;
begin
  if fConn <> nil then
    DuckDB.interrupt(fConn);
end;

procedure TSqlDBDuckDBConnection.StartTransaction;
begin
  inherited StartTransaction;
  try
    DuckDB.Exec(fConn, 'BEGIN TRANSACTION');
  except
    dec(fTransactionCount);
    raise;
  end;
end;

procedure TSqlDBDuckDBConnection.Commit;
begin
  inherited Commit;
  try
    DuckDB.Exec(fConn, 'COMMIT');
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBDuckDBConnection.Rollback;
begin
  inherited Rollback;
  DuckDB.Exec(fConn, 'ROLLBACK');
end;


{ TSqlDBDuckDBStatement }

destructor TSqlDBDuckDBStatement.Destroy;
begin
  try
    ReleaseResult;
    if fStmt <> nil then
      DuckDB.destroy_prepare(fStmt);
  finally
    inherited Destroy;
  end;
end;

procedure TSqlDBDuckDBStatement.ReleaseResult;
var
  i: PtrInt;
begin
  if fChunk <> nil then
    DuckDB.destroy_data_chunk(fChunk);
  fChunk := nil;
  fChunkSize := 0;
  fChunkRow := 0;
  for i := 0 to length(fInfo) - 1 do
    if fInfo[i].LogicalType <> nil then
      DuckDB.destroy_logical_type(fInfo[i].LogicalType);
  fInfo := nil;
  if fHasResult then
    DuckDB.destroy_result(@fRes);
  fHasResult := false;
end;

procedure TSqlDBDuckDBStatement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
var
  msg: RawUtf8;
begin
  SqlLogBegin(sllDB);
  inherited Prepare(aSql, ExpectResults); // set fSql + Connect if necessary
  if DuckDB.prepare(TSqlDBDuckDBConnection(fConnection).fConn,
       pointer(fSql), fStmt) <> DuckDBSuccess then
  begin
    msg := RawUtf8(DuckDB.prepare_error(fStmt));
    DuckDB.destroy_prepare(fStmt);
    fStmt := nil;
    // some statements expand into several statements (e.g. dynamic PIVOT
    // without IN list): without parameters, run them via duckdb_query()
    if (PosEx('multiple statements', msg) > 0) and
       (PosExChar('?', fSql) = 0) then
      fDirectQuery := true
    else
      ESqlDBDuckDB.RaiseUtf8('%.Prepare: % [%]', [self, msg, fSql]);
  end;
  SqlLogEnd;
end;

procedure TSqlDBDuckDBStatement.BindParams;
var
  i, len: PtrInt;
  p: PSqlDBParam;
  idx: TDuckIdx;
  res: TDuckDBState;
  tmp: TTemp32;
begin
  p := pointer(fParams);
  for i := 0 to fParamCount - 1 do
  begin
    idx := i + 1;
    case p^.VType of
      ftNull:
        res := DuckDB.bind_null(fStmt, idx);
      ftInt64:
        res := DuckDB.bind_int64(fStmt, idx, p^.VInt64);
      ftDouble:
        res := DuckDB.bind_double(fStmt, idx, unaligned(PDouble(@p^.VInt64)^));
      ftCurrency:
        if DuckDB.param_type(fStmt, idx) in
             [DUCKDB_TYPE_DECIMAL, DUCKDB_TYPE_VARCHAR] then
        begin
          // conversion into text, e.g. into a DECIMAL(19,4) column
          len := Curr64ToPChar(p^.VInt64, @tmp);
          res := DuckDB.bind_varchar_length(fStmt, idx, @tmp, len);
        end
        else
          res := DuckDB.bind_double(fStmt, idx, PCurrency(@p^.VInt64)^);
      ftDate:
        res := DuckDB.bind_timestamp(fStmt, idx,
          DateTimeToDuckTimestamp(unaligned(PDateTime(@p^.VInt64)^)));
      ftUtf8:
        res := DuckDB.bind_varchar_length(fStmt, idx, pointer(p^.VData),
          length(p^.VData));
      ftBlob:
        res := DuckDB.bind_blob(fStmt, idx, pointer(p^.VData), length(p^.VData));
    else
      res := 2; // 0=DuckDBSuccess 1=DuckDBError
    end;
    if res <> DuckDBSuccess then
      ESqlDBDuckDB.RaiseUtf8(
        '%.BindParams=%: cannot bind parameter #% type % [%]',
        [self, res, idx, ord(p^.VType), fSql]);
    inc(p);
  end;
end;

const
  NULL_TXT: RawUtf8 = 'null';

procedure TSqlDBDuckDBStatement.BindArrayRow(Row: integer; var Temp: RawUtf8);
var
  i: PtrInt;
  p: PSqlDBParam;
  idx: TDuckIdx;
  res: TDuckDBState;
  v: PRawUtf8;
begin
  // values are stored as SQL literals: number, 'quoted string', null
  p := pointer(fParams);
  for i := 0 to fParamCount - 1 do
  begin
    idx := i + 1;
    if Row < length(p^.VArray) then
      v := @p^.VArray[Row]
    else
      v := @NULL_TXT;
    if (p^.VType = ftNull) or
       (v^ = 'null') then
      res := DuckDB.bind_null(fStmt, idx)
    else
      case p^.VType of
        ftInt64:
          res := DuckDB.bind_int64(fStmt, idx, GetInt64(pointer(v^)));
        ftDouble:
          res := DuckDB.bind_double(fStmt, idx, GetExtended(pointer(v^)));
        ftBlob:
          res := DuckDB.bind_blob(fStmt, idx, pointer(v^), length(v^));
      else
        begin
          // ftUtf8, ftDate, ftCurrency: bind as text, DuckDB casts as needed
          if (v^ <> '') and
             (v^[1] = '''') then
          begin
            UnQuoteSqlStringVar(pointer(v^), Temp);
            v := @Temp;
          end;
          res := DuckDB.bind_varchar_length(fStmt, idx, pointer(v^), length(v^));
        end;
      end;
    if res <> DuckDBSuccess then
      ESqlDBDuckDB.CreateUtf8('%: cannot bind array parameter #% row % [%]',
        [self, idx, Row, fSql]);
    inc(p);
  end;
end;

procedure TSqlDBDuckDBStatement.ExecuteArray;
var
  row: integer;
  intrans: boolean;
  conn: TDuckDBConnection;
  temp: RawUtf8;
begin
  // run the prepared statement once per row - in an implicit transaction
  conn := TSqlDBDuckDBConnection(fConnection).fConn;
  intrans := fConnection.TransactionCount > 0;
  if not intrans then
    DuckDB.Exec(conn, 'BEGIN TRANSACTION');
  try
    for row := 0 to fParamsArrayCount - 1 do
    begin
      BindArrayRow(row, temp);
      FillCharFast(fRes, SizeOf(fRes), 0);
      if DuckDB.execute_prepared(fStmt, @fRes) <> DuckDBSuccess then
      begin
        temp := RawUtf8(DuckDB.result_error(@fRes));
        DuckDB.destroy_result(@fRes);
        ESqlDBDuckDB.RaiseUtf8('%.ExecutePrepared: % [%] row #%',
          [self, temp, fSql, row]);
      end;
      inc(fUpdateCount, DuckDB.rows_changed(@fRes));
      DuckDB.destroy_result(@fRes);
    end;
    if not intrans then
      DuckDB.Exec(conn, 'COMMIT');
  except
    if not intrans then
      DuckDB.Exec(conn, 'ROLLBACK');
    raise;
  end;
end;

procedure TSqlDBDuckDBStatement.ExecutePrepared;
var
  i: PtrInt;
  msg: RawUtf8;
  c: PDuckDBColumnInfo;
  col: PSqlDBColumnProperty;
  name: RawUtf8;
  res: TDuckDBState;
begin
  SqlLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  if (fStmt = nil) and
     not fDirectQuery then
    ESqlDBDuckDB.RaiseUtf8('%.ExecutePrepared without Prepare', [self]);
  ReleaseResult;
  ClearColumns;
  fColumnCount := 0;
  fCurrentRow := 0;
  fUpdateCount := 0;
  if fParamsArrayCount > 0 then
  begin
    // BindArray() was used: execute once per row (e.g. mormot.db.proxy batch)
    try
      ExecuteArray;
    finally
      SqlLogEnd;
    end;
    exit;
  end;
  FillCharFast(fRes, SizeOf(fRes), 0);
  if fDirectQuery then
    res := DuckDB.query(TSqlDBDuckDBConnection(fConnection).fConn,
      pointer(fSql), @fRes) // multi-statement SQL, without parameters
  else
  begin
    BindParams;
    res := DuckDB.execute_prepared(fStmt, @fRes);
  end;
  if res <> DuckDBSuccess then
  begin
    msg := RawUtf8(DuckDB.result_error(@fRes));
    DuckDB.destroy_result(@fRes);
    SqlLogEnd;
    ESqlDBDuckDB.RaiseUtf8('%.ExecutePrepared: % [%]', [self, msg, fSql]);
  end;
  fHasResult := true;
  fUpdateCount := DuckDB.rows_changed(@fRes);
  // retrieve the columns description
  SetLength(fInfo, DuckDB.column_count(@fRes));
  for i := 0 to length(fInfo) - 1 do
  begin
    c := @fInfo[i];
    c^.LogicalType := DuckDB.column_logical_type(@fRes, i);
    c^.TypeId := DuckDB.get_type_id(c^.LogicalType);
    case c^.TypeId of
      DUCKDB_TYPE_DECIMAL:
        begin
          c^.Internal := DuckDB.decimal_internal_type(c^.LogicalType);
          c^.Scale := DuckDB.decimal_scale(c^.LogicalType);
        end;
      DUCKDB_TYPE_ENUM:
        c^.Internal := DuckDB.enum_internal_type(c^.LogicalType);
    end;
    name := RawUtf8(DuckDB.column_name(@fRes, i));
    col := AddColumn(name);
    col^.ColumnType := DuckDBTypeToField(c^.TypeId, c^.Scale);
    col^.ColumnAttr := c^.TypeId;
  end;
  SqlLogEnd;
end;

function TSqlDBDuckDBStatement.UpdateCount: integer;
begin
  result := fUpdateCount;
end;

procedure TSqlDBDuckDBStatement.Reset;
begin
  ReleaseResult;
  fUpdateCount := 0;
  inherited Reset;
end;

procedure TSqlDBDuckDBStatement.ReleaseRows;
begin
  ReleaseResult;
  inherited ReleaseRows;
end;

function TSqlDBDuckDBStatement.NextChunk: boolean;
var
  i: PtrInt;
begin
  result := false;
  if fChunk <> nil then
    DuckDB.destroy_data_chunk(fChunk);
  fChunk := nil;
  repeat
    fChunk := DuckDB.fetch_chunk(fRes);
    if fChunk = nil then
      exit; // no more data
    fChunkSize := DuckDB.data_chunk_get_size(fChunk);
    if fChunkSize > 0 then
      break;
    DuckDB.destroy_data_chunk(fChunk);
  until false;
  fChunkRow := 0;
  for i := 0 to length(fInfo) - 1 do
    with fInfo[i] do
    begin
      Vector := DuckDB.data_chunk_get_vector(fChunk, i);
      Data := DuckDB.vector_get_data(Vector);
      Validity := DuckDB.vector_get_validity(Vector);
    end;
  result := true;
end;

function TSqlDBDuckDBStatement.Step(SeekFirst: boolean): boolean;
begin
  result := false;
  if SeekFirst and
     (fCurrentRow > 0) then
    ESqlDBDuckDB.RaiseUtf8('%.Step(SeekFirst=true) not implemented', [self]);
  if not fHasResult or
     (fInfo = nil) then
    exit;
  if fChunk <> nil then
    inc(fChunkRow);
  if (fChunk = nil) or
     (fChunkRow >= fChunkSize) then
    if not NextChunk then
    begin
      fCurrentRow := 0;
      ReleaseResult; // release memory ASAP
      exit;
    end;
  inc(fCurrentRow);
  inc(fTotalRowsRetrieved);
  result := true;
end;

function TSqlDBDuckDBStatement.CheckCol(Col: integer): PDuckDBColumnInfo;
begin
  if (fChunk = nil) or
     (cardinal(Col) >= cardinal(length(fInfo))) then
    ESqlDBDuckDB.RaiseUtf8('%: invalid Column(%) or no current row',
      [self, Col]);
  result := @fInfo[Col];
end;

function TSqlDBDuckDBStatement.ColumnDuckDBType(Col: integer): TDuckDBType;
begin
  if cardinal(Col) >= cardinal(length(fInfo)) then
    result := DUCKDB_TYPE_INVALID
  else
    result := fInfo[Col].TypeId;
end;

function TSqlDBDuckDBStatement.ColumnNull(Col: integer): boolean;
begin
  with CheckCol(Col)^ do
    result := not IsValid(Validity, fChunkRow);
end;

function TSqlDBDuckDBStatement.DecimalMantissa(c: PDuckDBColumnInfo;
  out hi: Int64): Int64;
var
  h: PDuckDBHugeInt;
begin
  case c^.Internal of
    DUCKDB_TYPE_SMALLINT:
      result := PDuckI16Array(c^.Data)^[fChunkRow];
    DUCKDB_TYPE_INTEGER:
      result := PDuckI32Array(c^.Data)^[fChunkRow];
    DUCKDB_TYPE_HUGEINT:
      begin
        h := PDuckDBHugeInt(PAnsiChar(c^.Data) + fChunkRow * 16);
        hi := h^.Upper;
        result := h^.Lower;
        exit;
      end;
  else
    result := PDuckI64Array(c^.Data)^[fChunkRow];
  end;
  if result < 0 then
    hi := -1
  else
    hi := 0;
end;

function TSqlDBDuckDBStatement.ColumnInt(Col: integer): Int64;
var
  c: PDuckDBColumnInfo;
  hi: Int64;
  h: TDuckDBHugeInt;
begin
  c := CheckCol(Col);
  result := 0;
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.TypeId of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER, DUCKDB_TYPE_UBIGINT:
      result := ReadIntegral(c^.TypeId, c^.Data, fChunkRow);
    DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
      begin
        h := PDuckDBHugeInt(PAnsiChar(c^.Data) + fChunkRow * 16)^;
        if not DuckHugeIntIsInt64(h, c^.TypeId = DUCKDB_TYPE_HUGEINT) then
          ESqlDBDuckDB.RaiseUtf8('%.ColumnInt(%): 128-bit overflow',
            [self, Col]);
        result := h.Lower;
      end;
    DUCKDB_TYPE_DECIMAL:
      if c^.Internal = DUCKDB_TYPE_HUGEINT then
        result := GetInt64(pointer(ColumnUtf8(Col))) // integer part
      else
        result := DecimalMantissa(c, hi) div POW10[c^.Scale];
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      result := trunc(ColumnDouble(Col));
    DUCKDB_TYPE_DATE:
      result := PDuckI32Array(c^.Data)^[fChunkRow];
    DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_S, DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
      result := PDuckI64Array(c^.Data)^[fChunkRow];
  else
    result := GetInt64(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBDuckDBStatement.ColumnDouble(Col: integer): double;
var
  c: PDuckDBColumnInfo;
  hi: Int64;
begin
  c := CheckCol(Col);
  result := 0;
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.TypeId of
    DUCKDB_TYPE_FLOAT:
      result := PDuckSingleArray(c^.Data)^[fChunkRow];
    DUCKDB_TYPE_DOUBLE:
      result := PDuckDoubleArray(c^.Data)^[fChunkRow];
    DUCKDB_TYPE_DECIMAL:
      if c^.Internal = DUCKDB_TYPE_HUGEINT then
        result := GetExtended(pointer(ColumnUtf8(Col)))
      else
        result := DecimalMantissa(c, hi) / POW10[c^.Scale];
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      result := ReadIntegral(c^.TypeId, c^.Data, fChunkRow);
  else
    result := GetExtended(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBDuckDBStatement.ColumnCurrency(Col: integer): currency;
var
  c: PDuckDBColumnInfo;
  hi, v: Int64;
begin
  c := CheckCol(Col);
  result := 0;
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.TypeId of
    DUCKDB_TYPE_DECIMAL:
      if (c^.Internal <> DUCKDB_TYPE_HUGEINT) and
         (c^.Scale <= 4) then
      begin
        v := DecimalMantissa(c, hi) * POW10[4 - c^.Scale];
        result := PCurrency(@v)^;
      end
      else
        PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      result := ColumnDouble(Col);
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      result := ReadIntegral(c^.TypeId, c^.Data, fChunkRow);
  else
    PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBDuckDBStatement.ColumnDateTime(Col: integer): TDateTime;
var
  c: PDuckDBColumnInfo;
  us: Int64;
  offs: integer;
begin
  c := CheckCol(Col);
  result := 0;
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.TypeId of
    DUCKDB_TYPE_DATE:
      result := PDuckI32Array(c^.Data)^[fChunkRow] + DUCKDB_UNIXDATE_DELTA;
    DUCKDB_TYPE_TIME:
      result := PDuckI64Array(c^.Data)^[fChunkRow] / DUCKDB_MICROSPERDAY;
    DUCKDB_TYPE_TIME_NS:
      result := (PDuckI64Array(c^.Data)^[fChunkRow] div 1000) / DUCKDB_MICROSPERDAY;
    DUCKDB_TYPE_TIME_TZ:
      begin
        DuckTimeTzDecode(PDuckI64Array(c^.Data)^[fChunkRow], us, offs);
        result := us / DUCKDB_MICROSPERDAY;
      end;
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_TZ, DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
      result := DuckTimestampToDateTime(
        TimestampMicros(c^.TypeId, PDuckI64Array(c^.Data)^[fChunkRow]));
  else
    result := Iso8601ToDateTime(ColumnUtf8(Col));
  end;
end;

function TSqlDBDuckDBStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  c: PDuckDBColumnInfo;
  p: PUtf8Char;
  len: integer; // not PtrInt
  idx: Int64;
  i: PtrInt;
begin
  c := CheckCol(Col);
  result := '';
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.TypeId of
    DUCKDB_TYPE_VARCHAR, DUCKDB_TYPE_BLOB:
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        FastSetString(result, p, len);
      end;
    DUCKDB_TYPE_ENUM:
      begin
        if c^.EnumValues = nil then
        begin
          // cache the whole enum dictionary for this column
          SetLength(c^.EnumValues, DuckDB.enum_dictionary_size(c^.LogicalType));
          for i := 0 to length(c^.EnumValues) - 1 do
          begin
            p := DuckDB.enum_dictionary_value(c^.LogicalType, i);
            c^.EnumValues[i] := RawUtf8(p);
            DuckDB.free_(p);
          end;
        end;
        idx := ReadIntegral(c^.Internal, c^.Data, fChunkRow);
        if (idx >= 0) and
           (idx < length(c^.EnumValues)) then
          result := c^.EnumValues[idx];
      end;
  else
    DuckDBVectorValueToText(c^.Vector, c^.LogicalType, fChunkRow, result);
  end;
end;

function TSqlDBDuckDBStatement.ColumnBlob(Col: integer): RawByteString;
var
  c: PDuckDBColumnInfo;
  p: PUtf8Char;
  len: integer; // not PtrInt
begin
  c := CheckCol(Col);
  result := '';
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  if c^.TypeId in [DUCKDB_TYPE_BLOB, DUCKDB_TYPE_VARCHAR] then
  begin
    p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
    FastSetRawByteString(result, p, len);
  end
  else
    result := ColumnUtf8(Col);
end;

procedure TSqlDBDuckDBStatement.ColumnToJson(Col: integer; W: TJsonWriter);
var
  c: PDuckDBColumnInfo;
  p: pointer;
  len: integer; // not PtrInt
begin
  c := CheckCol(Col);
  if not IsValid(c^.Validity, fChunkRow) then
  begin
    W.AddNull;
    exit;
  end;
  case c^.TypeId of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      W.Add(ReadIntegral(c^.TypeId, c^.Data, fChunkRow)); // boolean as 0/1
    DUCKDB_TYPE_VARCHAR:
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        W.Add('"');
        W.AddJsonEscape(p, len);
        W.Add('"');
      end;
    DUCKDB_TYPE_BLOB:
      if dsfForceBlobAsNull in fFlags then
        W.AddNull
      else
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        W.WrBase64(p, len, {withmagic=}true);
      end;
    DUCKDB_TYPE_DATE, DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIME_TZ, DUCKDB_TYPE_TIME_NS,
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S, DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS, DUCKDB_TYPE_TIMESTAMP_TZ:
      begin
        W.Add('"');
        W.AddDateTime(ColumnDateTime(Col), dsfForceDateWithMS in fFlags);
        W.Add('"');
      end;
    DUCKDB_TYPE_ENUM:
      begin
        W.Add('"');
        W.AddJsonEscape(pointer(ColumnUtf8(Col)));
        W.Add('"');
      end;
  else
    DuckDBVectorValueToJson(c^.Vector, c^.LogicalType, fChunkRow, W);
  end;
end;


{ TSqlDBDuckDBAppender }

procedure TSqlDBDuckDBAppender.Check(res: TDuckDBState; const ctxt: RawUtf8);
begin
  if res <> DuckDBSuccess then
    Failed(res, ctxt);
end;

constructor TSqlDBDuckDBAppender.Create(aConnection: TSqlDBDuckDBConnection;
  const aTable, aSchema: RawUtf8);
var
  schema: PUtf8Char;
begin
  inherited Create;
  if not DuckDB.HasAppender then
    ESqlDBDuckDB.RaiseU('DuckDB library has no appender API');
  if not aConnection.IsConnected then
    aConnection.Connect;
  fTable := aTable;
  schema := nil;
  if aSchema <> '' then
    schema := pointer(aSchema);
  if DuckDB.appender_create(aConnection.fConn,
       schema, pointer(aTable), fAppender) <> DuckDBSuccess then
  begin
    try
      Check(DuckDBError, 'create');
    finally
      DuckDB.appender_destroy(fAppender);
      fAppender := nil;
    end;
  end;
end;

destructor TSqlDBDuckDBAppender.Destroy;
begin
  try
    Close;
  finally
    inherited Destroy;
  end;
end;

procedure TSqlDBDuckDBAppender.Failed(res: TDuckDBState; const ctxt: RawUtf8);
var
  msg: RawUtf8;
begin
  if res = DuckDBSuccess then
    exit;
  if fAppender <> nil then
    msg := RawUtf8(DuckDB.appender_error(fAppender));
  ESqlDBDuckDB.RaiseUtf8('Appender(%).%: %', [fTable, ctxt, msg]);
end;

procedure TSqlDBDuckDBAppender.AddNull;
begin
  Check(DuckDB.append_null(fAppender), 'AddNull');
end;

procedure TSqlDBDuckDBAppender.Add(Value: Int64);
begin
  Check(DuckDB.append_int64(fAppender, Value), 'Add');
end;

procedure TSqlDBDuckDBAppender.AddDouble(Value: double);
begin
  Check(DuckDB.append_double(fAppender, Value), 'AddDouble');
end;

procedure TSqlDBDuckDBAppender.AddCurrency(Value: currency);
var
  tmp: TTemp32;
  len: PtrInt;
begin
  len := Curr64ToPChar(PInt64(@Value)^, @tmp); // exact text -> DECIMAL cast
  Check(DuckDB.append_varchar_length(fAppender, @tmp, len), 'AddCurrency');
end;

procedure TSqlDBDuckDBAppender.AddBoolean(Value: boolean);
begin
  Check(DuckDB.append_bool(fAppender, Value), 'AddBoolean');
end;

procedure TSqlDBDuckDBAppender.AddDateTime(Value: TDateTime);
begin
  Check(DuckDB.append_timestamp(fAppender, DateTimeToDuckTimestamp(Value)),
    'AddDateTime');
end;

procedure TSqlDBDuckDBAppender.AddText(Value: PUtf8Char; Len: PtrInt);
begin
  Check(DuckDB.append_varchar_length(fAppender, Value, Len), 'AddText');
end;

procedure TSqlDBDuckDBAppender.AddText(const Value: RawUtf8);
begin
  AddText(pointer(Value), length(Value));
end;

procedure TSqlDBDuckDBAppender.AddBlob(const Value: RawByteString);
begin
  Check(DuckDB.append_blob(fAppender, pointer(Value), length(Value)), 'AddBlob');
end;

procedure TSqlDBDuckDBAppender.AddRow(const Values: array of const);
var
  i: PtrInt;
  tmp: TTempUtf8;
begin
  for i := 0 to high(Values) do
    with Values[i] do
      case VType of
        vtInteger:
          Add(VInteger);
        vtInt64:
          Add(VInt64^);
        {$ifdef FPC}
        vtQWord:
          Add(VQWord^);
        {$endif FPC}
        vtBoolean:
          AddBoolean(VBoolean);
        vtExtended:
          AddDouble(VExtended^);
        vtCurrency:
          AddCurrency(VCurrency^);
        vtPointer:
          if VPointer = nil then
            AddNull
          else
            ESqlDBDuckDB.RaiseUtf8('Appender(%).AddRow: pointer', [fTable]);
      else
        begin
          VarRecToTempUtf8(@Values[i], tmp);
          AddText(tmp.Text, tmp.Len);
          TempUtf8Done(tmp);
        end;
      end;
  EndRow;
end;

procedure TSqlDBDuckDBAppender.EndRow;
begin
  Check(DuckDB.appender_end_row(fAppender), 'EndRow');
  inc(fRowCount);
end;

procedure TSqlDBDuckDBAppender.Flush;
begin
  Check(DuckDB.appender_flush(fAppender), 'Flush');
end;

procedure TSqlDBDuckDBAppender.Close;
var
  res: TDuckDBState;
begin
  if fAppender = nil then
    exit;
  res := DuckDB.appender_close(fAppender);
  try
    Check(res, 'Close');
  finally
    DuckDB.appender_destroy(fAppender);
    fAppender := nil;
  end;
end;


initialization
  TSqlDBDuckDBConnectionProperties.RegisterClassNameForDefinition;

end.
