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
    - TSqlDBQuackConnectionProperties: native client of a DuckDB Quack server
    - TSqlDBQuackConnection: one HTTP keep-alive socket per (thread) connection
    - TSqlDBQuackStatement: remote execution and chunk-based row access

  *****************************************************************************

  Notes:
  - DuckDB SQL is very close to PostgreSQL, so dDuckDB = dPostgreSQL is used as
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
  mormot.lib.static,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
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
    /// OnBatchInsert implementation using TSqlDBDuckDBAppender
    // - fallback to MultipleValuesInsert() if the Appender can't be used
    procedure AppendBatchInsert(Props: TSqlDBConnectionProperties;
      const TableName: RawUtf8; const FieldNames: TRawUtf8DynArray;
      const FieldTypes: TSqlDBFieldTypeArray; RowCount: integer;
      const FieldValues: TRawUtf8DynArrayDynArray;
      BatchOptions: TRestBatchOptions);
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
    /// start a Quack server (DuckDB client-server protocol over HTTP)
    // - runs CALL quack_serve(aEndpoint, token=aToken), the quack extension being autoloaded
    // - aEndpoint is e.g. 'quack:localhost' or 'quack:localhost:9494'
    // - the Quack extension is experimental, and needs to be downloadable
    // by DuckDB, unless it is already installed locally
    // - see https://duckdb.org/2026/05/12/quack-remote-protocol
    procedure QuackServe(const aEndpoint, aToken: RawUtf8);
    /// attach a remote Quack server as a database of this instance
    // - runs CREATE SECRET (TYPE quack, TOKEN ..) then ATTACH aEndpoint AS aAlias
    // - remote tables can then be accessed as aAlias.main.tablename
    procedure QuackAttach(const aEndpoint, aToken, aAlias: RawUtf8);
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
    /// the column type, parsed once with its nested types
    Info: TDuckDBTypeInfo;

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
    procedure ReleaseResult;
    function NextChunk: boolean;
    function CheckCol(Col: integer): PDuckDBColumnInfo;
      {$ifdef HASINLINE} inline; {$endif}
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
procedure DuckDBVectorValueToJson(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; W: TJsonWriter);


/// return the value at Row of a DuckDB vector as text
// - scalar values in DuckDB text format, nested values as JSON
procedure DuckDBVectorValueToText(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; var result: RawUtf8);

/// fill the simple value at Row of a DuckDB vector into a text buffer
// - returns the number of bytes written, or -1 if this type is not supported
function DuckDBVectorValueToTextBuf(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; var Buffer: TTemp47): PtrInt;



{ ************ TSqlDBQuackConnection* and TSqlDBQuackStatement Classes }

type
  /// how the HTTP transport of a Quack connection is encrypted
  // - qtmAuto follows DuckDB: plain HTTP for localhost, HTTPS otherwise
  TSqlDBQuackTlsMode = (
    qtmAuto,
    qtmOn,
    qtmOff);

  /// connection properties to a remote DuckDB Quack server
  // - aServerName is the Quack server URI, e.g. 'quack:localhost' or
  // 'quack://db.example.org:9494' - the default port is 9494
  // - aPassWord is the Quack authentication token
  // - aUserID is the optional client identifier, sent to the server
  // (4 characters minimum if set)
  // - aDatabaseName is ignored
  TSqlDBQuackConnectionProperties = class(TSqlDBConnectionPropertiesThreadSafe)
  protected
    fHost: RawUtf8;
    fPort: integer;
    fTlsMode: TSqlDBQuackTlsMode;
    fIgnoreTlsErrors: boolean;
    fHeartbeatSeconds: cardinal;
    fTimeOutMs: cardinal;
    fProtocolVersion: cardinal;
    function GetUseTls: boolean;
    procedure GetForeignKeys; override;
  public
    /// initialize the properties - the server is contacted on first connection
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
      override;
    /// create a new connection - caller should free it
    function NewConnection: TSqlDBConnection; override;
    /// the server host name, as parsed from ServerName
    property Host: RawUtf8
      read fHost;
    /// the server TCP port, as parsed from ServerName
    property Port: integer
      read fPort;
    /// true if HTTPS is used, depending on TlsMode and the server Host
    property UseTls: boolean
      read GetUseTls;
  published
    /// how the HTTP transport is encrypted - qtmAuto by default
    property TlsMode: TSqlDBQuackTlsMode
      read fTlsMode write fTlsMode;
    /// let HTTPS accept any certificate, e.g. a self-signed server certificate
    // - disable the server authentication so should be used only for tests
    property IgnoreTlsErrors: boolean
      read fIgnoreTlsErrors write fIgnoreTlsErrors;
    /// the lease duration, in seconds, requested to the server
    // - a connection idle for longer will be released by the server
    // - default is 60 seconds
    property HeartbeatSeconds: cardinal
      read fHeartbeatSeconds write fHeartbeatSeconds;
    /// the connection and response timeout, in milliseconds
    // - default is 30000, i.e. 30 seconds
    property TimeOutMs: cardinal
      read fTimeOutMs write fTimeOutMs;
    /// the Quack protocol version to use, or 0 to detect it on connection
    // - the released DuckDB 1.5 quack extension uses version 1, and the
    // development branch uses version 3
    property ProtocolVersion: cardinal
      read fProtocolVersion write fProtocolVersion;
  end;

  /// one connection to a Quack server
  // - uses its own HTTP keep-alive socket and server-side connection
  TSqlDBQuackConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fSocket: THttpClientSocket;
    fConnectionId: RawUtf8;
    fLeaseSeconds: cardinal;
    fLastRequestSec: cardinal;
    fVersion: cardinal;
    function Post(const Body: RawByteString): TQuackResponse;
    procedure CheckLease;
  public
    /// connect to the server, i.e. open the socket and send CONNECTION_REQUEST
    procedure Connect; override;
    /// send a DISCONNECT_MESSAGE and close the socket
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
    /// send a request to the server, and check the type of its response
    // - raise ESqlDBQuack if the server returned an error, or any other type
    // of message than the Expected one
    // - the caller should release the returned instance
    function Request(const Body: RawByteString; Expected: byte): TQuackResponse;
    /// send a request to the server, ignoring any error
    // - used to cancel or acknowledge a query
    procedure RequestSilently(const Body: RawByteString);
    /// execute a SQL statement without parameters on the server
    // - e.g. for DDL scripts, COPY, ATTACH, INSTALL/LOAD, PRAGMA...
    // - returns the number of changed rows, or 0 for a SELECT
    function ExecuteScript(const aSql: RawUtf8): Int64;
    /// the server-side connection identifier
    property ConnectionId: RawUtf8
      read fConnectionId;
    /// the Quack protocol version negotiated with the server
    property Version: cardinal
      read fVersion;
  end;

  /// a statement executed on a Quack server
  TSqlDBQuackStatement = class(TSqlDBStatementWithParamsAndColumns)
  protected
    fResponse: TQuackResponse;
    fChunk: TQuackChunk;
    fChunkIndex: integer;
    fChunkRow: PtrInt;
    fQueryUuid: TDuckDBHugeInt;
    fNextBatch: cardinal;
    fMoreToFetch: boolean;
    fExecuted: boolean;
    fUpdateCount: integer;
    function BuildSql: RawUtf8;
    procedure AddParam(W: TJsonWriter; Index: integer);
    procedure ReleaseResult;
    procedure FetchNext;
    function NextChunk: boolean;
    function CheckCol(Col: integer): TQuackVector;
  public
    /// release all associated resources, and cancel any running query
    destructor Destroy; override;
    /// prepare an UTF-8 encoded SQL statement with ? parameters
    // - nothing is sent to the server until ExecutePrepared is called
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
    // - will send a FETCH_REQUEST to the server when needed
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
  end;


/// append the value of a Quack vector at a given row as JSON
// - handles all scalar types, and nested LIST/ARRAY/STRUCT/MAP/UNION
procedure QuackValueToJson(V: TQuackVector; Row: PtrInt; W: TJsonWriter);

/// return the value of a Quack vector at a given row as text
// - scalar values in DuckDB text format, nested values as JSON
procedure QuackValueToText(V: TQuackVector; Row: PtrInt; var Result: RawUtf8);

implementation


{ ************ Vector Value Conversion }

const
  // integer types which can be read by ReadIntegral() as Int64
  DUCKDB_INT_TYPES = [DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT,
    DUCKDB_TYPE_SMALLINT, DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT,
    DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER];

function IsValid(validity: PDuckI64Array; row: PtrInt): boolean;
  {$ifdef HASINLINE} inline; {$endif}
begin
  result := (validity = nil) or
            ((validity^[row shr 6] shr (row and 63)) and 1 <> 0);
end;

function DuckDBFixedData(data: PAnsiChar; T: PDuckDBTypeInfo;
  row: PtrInt): PAnsiChar;
var
  size: PtrInt;
begin
  size := T^.Size;
  if (size = 0) and
     (T^.TypeId <> DUCKDB_TYPE_SQLNULL) then
    // e.g. VARIANT - would be returned as ''
    ESqlDBDuckDB.RaiseUtf8('Unsupported DuckDB type #% - use a SQL cast',
      [T^.TypeId]);
  result := data + row * size;
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

function DuckDBVectorValueToTextBuf(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; var Buffer: TTemp47): PtrInt;
begin
  if not IsValid(DuckDB.vector_get_validity(vec), row) then
  begin
    Buffer[0] := #0;
    result := 0;
    exit;
  end;
  result := DuckFixedToTextBuf(T^.TypeId, T^.Internal, T^.Scale,
    PAnsiChar(DuckDB.vector_get_data(vec)) +
      row * T^.Size, Buffer);
end;

procedure ComplexDuckDBVectorValueToText(vec: TDuckDBVector;
  T: PDuckDBTypeInfo; row: PtrInt; var result: RawUtf8);
var
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    DuckDBVectorValueToJson(vec, T, row, W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure DuckDBVectorValueToText(vec: TDuckDBVector;
  T: PDuckDBTypeInfo; row: PtrInt; var result: RawUtf8);
var
  typeid: TDuckDBType;
  len: integer;
  data: PAnsiChar;
  p: pointer;
begin
  FastAssignNew(result);
  if not IsValid(DuckDB.vector_get_validity(vec), row) then
    exit;
  data := DuckDB.vector_get_data(vec);
  typeid := T^.TypeId;
  case typeid of
    DUCKDB_TYPE_VARCHAR, DUCKDB_TYPE_BLOB, DUCKDB_TYPE_GEOMETRY:
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
      result := DuckDBEnumValue(T^,
        ReadIntegral(T^.Internal, data, row));
    DUCKDB_TYPE_LIST, DUCKDB_TYPE_ARRAY, DUCKDB_TYPE_STRUCT,
    DUCKDB_TYPE_MAP, DUCKDB_TYPE_UNION:
      ComplexDuckDBVectorValueToText(vec, T, row, result); // as JSON
  else
    DuckFixedToText(typeid, T^.Internal, T^.Scale,
      DuckDBFixedData(data, T, row), result);
  end;
end;

procedure AddVectorValueAsJsonString(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; W: TJsonWriter);
var
  txt: RawUtf8;
begin
  DuckDBVectorValueToText(vec, T, row, txt); // ENUM and BIT
  W.AddDirect('"');
  W.AddJsonEscape(pointer(txt), length(txt));
  W.AddDirect('"');
end;

procedure ComplexDuckDBVectorValueToJson(vec: TDuckDBVector; data: PAnsiChar;
  T: PDuckDBTypeInfo; row: PtrInt; W: TJsonWriter);
var
  i, n: PtrInt;
  e: PDuckDBListEntry;
  child, kvec, vvec: TDuckDBVector;
  buf: TTemp47;
  txt: RawUtf8;
begin
  e := PDuckDBListEntry(data + row * SizeOf(TDuckDBListEntry));
  child := DuckDB.list_vector_get_child(vec);
  if T^.TypeId = DUCKDB_TYPE_LIST then
  begin
    W.AddDirect('[');
    for i := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
    begin
      DuckDBVectorValueToJson(child, @T^.Children^[0], i, W);
      W.AddComma;
    end;
    W.CancelLastComma(']');
  end
  else
  begin
    // MAP = LIST(STRUCT(key, value)) -> JSON object
    kvec := DuckDB.struct_vector_get_child(child, 0);
    vvec := DuckDB.struct_vector_get_child(child, 1);
    W.AddDirect('{');
    for i := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
    begin
      n := DuckDBVectorValueToTextBuf(kvec, @T^.Children^[0], i, buf);
      if n >= 0 then
        W.AddProp(@buf[0], n)
      else
      begin
        DuckDBVectorValueToText(kvec, @T^.Children^[0], i, txt);
        W.AddProp(pointer(txt), length(txt));
      end;
      DuckDBVectorValueToJson(vvec, @T^.Children^[1], i, W);
      W.AddComma;
    end;
    W.CancelLastComma('}');
  end;
end;

procedure DuckDBVectorValueToJson(vec: TDuckDBVector; T: PDuckDBTypeInfo;
  row: PtrInt; W: TJsonWriter);
var
  data: PAnsiChar;
  p: pointer;
  typeid: TDuckDBType;
  i, size: PtrInt; // no Int64 for-loop variable on Delphi 7
  len: integer; // not PtrInt
  tag: integer;
  child: TDuckDBVector;
begin
  if not IsValid(DuckDB.vector_get_validity(vec), row) then
  begin
    W.AddNull;
    exit;
  end;
  data := DuckDB.vector_get_data(vec);
  typeid := T^.TypeId;
  case typeid of
    DUCKDB_TYPE_VARCHAR:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        W.AddDirect('"');
        W.AddJsonEscape(p, len);
        W.AddDirect('"');
      end;
    DUCKDB_TYPE_BLOB, DUCKDB_TYPE_GEOMETRY:
      begin
        p := DuckStringPtr(DuckStringAt(data, row), len);
        W.WrBase64(p, len, {withmagic=}true);
      end;
    DUCKDB_TYPE_LIST, DUCKDB_TYPE_MAP:
      ComplexDuckDBVectorValueToJson(vec, data, T, row, W);
    DUCKDB_TYPE_ARRAY:
      begin
        child := DuckDB.array_vector_get_child(vec);
        size := T^.Scale;
        W.AddDirect('[');
        for i := row * size to row * size + size - 1 do
        begin
          DuckDBVectorValueToJson(child, @T^.Children^[0], i, W);
          W.AddComma;
        end;
        W.CancelLastComma(']');
      end;
    DUCKDB_TYPE_STRUCT:
      begin
        W.AddDirect('{');
        for i := 0 to T^.ChildCount - 1 do
        begin
          W.AddProp(pointer(T^.Names[i]), length(T^.Names[i]));
          DuckDBVectorValueToJson(DuckDB.struct_vector_get_child(vec, i),
            @T^.Children^[i], row, W);
          W.AddComma;
        end;
        W.CancelLastComma('}');
      end;
    DUCKDB_TYPE_UNION:
      begin
        // UNION = STRUCT(tag UTINYINT, member1, member2, ...)
        tag := PDuckU8Array(DuckDB.vector_get_data(
          DuckDB.struct_vector_get_child(vec, 0)))^[row];
        DuckDBVectorValueToJson(DuckDB.struct_vector_get_child(vec, tag + 1),
          @T^.Children^[tag], row, W);
      end;
    DUCKDB_TYPE_ENUM, DUCKDB_TYPE_BIT:
      AddVectorValueAsJsonString(vec, T, row, W);
  else
    DuckFixedToJson(typeid, T^.Internal, T^.Scale,
      DuckDBFixedData(data, T, row), W);
  end;
end;


{ ************ TSqlDBDuckDBConnection* and TSqlDBDuckDBStatement Classes }

{ TSqlDBDuckDBConnectionProperties }

constructor TSqlDBDuckDBConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  DuckDBLibraryInitialize; // raise ESqlDBDuckDB if no duckdb.dll
  fDbms := dDuckDB;        // PostgreSQL-like SQL dialect
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  fEngineName := 'DuckDB';
  ForcedSchemaName := 'main'; // DuckDB default schema, for metadata queries
  if DuckDB.HasAppender then
  begin
    // the Appender is much faster than multi-values INSERT statements
    fBatchSendingAbilities := [cCreate];
    fOnBatchInsert := AppendBatchInsert;
    fBatchMaxSentAtOnce := 4096;
  end;
end;

procedure TSqlDBDuckDBConnectionProperties.AppendBatchInsert(
  Props: TSqlDBConnectionProperties; const TableName: RawUtf8;
  const FieldNames: TRawUtf8DynArray; const FieldTypes: TSqlDBFieldTypeArray;
  RowCount: integer; const FieldValues: TRawUtf8DynArrayDynArray;
  BatchOptions: TRestBatchOptions);
var
  cols: TSqlDBColumnDefineDynArray;
  app: TSqlDBDuckDBAppender;
  f, r: PtrInt;
  v: PRawUtf8;
  tmp: RawUtf8;
begin
  // the Appender expects all columns, in the table order, without conflict
  // handling: fallback to the regular multi-values INSERT otherwise
  GetFields(TableName, cols);
  if (BatchOptions * [boInsertOrIgnore, boInsertOrReplace, boUpsert] <> []) or
     (length(cols) <> length(FieldNames)) then
  begin
    MultipleValuesInsert(Props, TableName, FieldNames, FieldTypes, RowCount,
      FieldValues, BatchOptions);
    exit;
  end;
  for f := 0 to high(cols) do
    if not PropNameEquals(cols[f].ColumnName, FieldNames[f]) then
    begin
      MultipleValuesInsert(Props, TableName, FieldNames, FieldTypes, RowCount,
        FieldValues, BatchOptions);
      exit;
    end;
  app := TSqlDBDuckDBAppender.Create(
    Props.ThreadSafeConnection as TSqlDBDuckDBConnection, TableName);
  try
    for r := 0 to RowCount - 1 do
    begin
      for f := 0 to high(FieldNames) do
      begin
        v := @FieldValues[f, r]; // values are SQL literals, BLOB are decoded
        if (v^ = 'null') or
           (FieldTypes[f] = ftNull) then
          app.AddNull
        else
          case FieldTypes[f] of
            ftInt64:
              app.Add(GetInt64(pointer(v^)));
            ftDouble:
              app.AddDouble(GetExtended(pointer(v^)));
            ftBlob:
              app.AddBlob(v^);
            ftCurrency:
              app.AddText(v^); // exact text -> DECIMAL cast
          else
            begin
              // ftUtf8, ftDate: unquote and let DuckDB cast to the column type
              UnQuoteSqlStringVar(pointer(v^), tmp);
              app.AddText(tmp);
            end;
          end;
      end;
      app.EndRow;
    end;
  finally
    app.Free; // flush the rows
  end;
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
  state: TDuckDBState;
  saved: cardinal;
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
        saved := SetFpuFlags; // the worker threads inherit the FPU flags
        try
          state := DuckDB.open_ext(path, fDatabase, cfg, @err);
        finally
          ResetFpuFlags(saved);
        end;
        if state <> DuckDBSuccess then
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

procedure TSqlDBDuckDBConnectionProperties.QuackServe(
  const aEndpoint, aToken: RawUtf8);
begin
  (ThreadSafeConnection as TSqlDBDuckDBConnection).ExecuteScript(
    'CALL quack_serve(' + QuotedStr(aEndpoint) +
    ', token = ' + QuotedStr(aToken) + ')');
end;

procedure TSqlDBDuckDBConnectionProperties.QuackAttach(
  const aEndpoint, aToken, aAlias: RawUtf8);
var
  i: PtrInt;
begin
  if aAlias = '' then
    ESqlDBDuckDB.RaiseUtf8('%.QuackAttach: void alias', [self]);
  for i := 1 to length(aAlias) do // alias is not quoted: validate it
    if not (aAlias[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
      ESqlDBDuckDB.RaiseUtf8('%.QuackAttach: invalid alias [%]', [self, aAlias]);
  (ThreadSafeConnection as TSqlDBDuckDBConnection).ExecuteScript(
    'CREATE OR REPLACE SECRET (TYPE quack, TOKEN ' +
    QuotedStr(aToken) + ', SCOPE ' + QuotedStr(aEndpoint) + '); ATTACH ' +
    QuotedStr(aEndpoint) + ' AS ' + aAlias);
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
    DuckDBTypeInfoDone(fInfo[i].Info);
  fInfo := nil;
  if fHasResult then
    DuckDB.destroy_result(@fRes);
  fHasResult := false;
end;

procedure TSqlDBDuckDBStatement.Prepare(const aSql: RawUtf8; ExpectResults: boolean);
var
  msg: RawUtf8;
  state: TDuckDBState;
  saved: cardinal;
begin
  SqlLogBegin(sllDB);
  inherited Prepare(aSql, ExpectResults); // set fSql + Connect if necessary
  saved := SetFpuFlags; // the optimizer may fold constants
  try
    state := DuckDB.prepare(TSqlDBDuckDBConnection(fConnection).fConn,
      pointer(fSql), fStmt);
  finally
    ResetFpuFlags(saved);
  end;
  if state <> DuckDBSuccess then
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

procedure TSqlDBDuckDBStatement.ExecutePrepared;
var
  i: PtrInt;
  msg: RawUtf8;
  c: PDuckDBColumnInfo;
  col: PSqlDBColumnProperty;
  name: RawUtf8;
  res: TDuckDBState;
  saved: cardinal;
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
    ESqlDBDuckDB.RaiseUtf8('%.ExecutePrepared: BindArray() is not supported - ' +
      'use OnBatchInsert or a transaction', [self]);
  FillCharFast(fRes, SizeOf(fRes), 0);
  saved := SetFpuFlags; // the C++ engine expects masked FPU exceptions
  try
    if fDirectQuery then
      res := DuckDB.query(TSqlDBDuckDBConnection(fConnection).fConn,
        pointer(fSql), @fRes) // multi-statement SQL, without parameters
    else
    begin
      BindParams;
      res := DuckDB.execute_prepared(fStmt, @fRes);
    end;
  finally
    ResetFpuFlags(saved);
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
    DuckDBTypeInfoInit(c^.Info, DuckDB.column_logical_type(@fRes, i));
    name := RawUtf8(DuckDB.column_name(@fRes, i));
    col := AddColumn(name);
    col^.ColumnType := DuckDBTypeToField(c^.Info.TypeId, c^.Info.Scale);
    col^.ColumnAttr := c^.Info.TypeId;
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
  saved: cardinal;
begin
  result := false;
  if fChunk <> nil then
    DuckDB.destroy_data_chunk(fChunk);
  fChunk := nil;
  repeat
    saved := SetFpuFlags; // the chunks are computed during this call
    try
      fChunk := DuckDB.fetch_chunk(fRes);
    finally
      ResetFpuFlags(saved);
    end;
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
    result := fInfo[Col].Info.TypeId;
end;

function TSqlDBDuckDBStatement.ColumnNull(Col: integer): boolean;
begin
  with CheckCol(Col)^ do
    result := not IsValid(Validity, fChunkRow);
end;

function TSqlDBDuckDBStatement.ColumnInt(Col: integer): Int64;
var
  c: PDuckDBColumnInfo;
begin
  c := CheckCol(Col);
  result := 0;
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  with c^.Info do
    case DuckFixedToInt64(TypeId, Internal, Scale,
           PAnsiChar(c^.Data) + fChunkRow * Size, result) of
      dcOk:
        exit;
      dcOverflow:
        ESqlDBDuckDB.RaiseUtf8('%.ColumnInt(%): 128-bit overflow', [self, Col]);
    end;
  result := GetInt64(pointer(ColumnUtf8(Col)));
end;

function TSqlDBDuckDBStatement.ColumnDouble(Col: integer): double;
var
  c: PDuckDBColumnInfo;
begin
  c := CheckCol(Col);
  result := 0;
  if IsValid(c^.Validity, fChunkRow) then
    with c^.Info do
      if not DuckFixedToDouble(TypeId, Internal, Scale,
               PAnsiChar(c^.Data) + fChunkRow * Size, result) then
        result := GetExtended(pointer(ColumnUtf8(Col)));
end;

function TSqlDBDuckDBStatement.ColumnCurrency(Col: integer): currency;
var
  c: PDuckDBColumnInfo;
begin
  c := CheckCol(Col);
  result := 0;
  if IsValid(c^.Validity, fChunkRow) then
    with c^.Info do
      if not DuckFixedToCurrency(TypeId, Internal, Scale,
               PAnsiChar(c^.Data) + fChunkRow * Size, result) then
        PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
end;

function TSqlDBDuckDBStatement.ColumnDateTime(Col: integer): TDateTime;
var
  c: PDuckDBColumnInfo;
begin
  c := CheckCol(Col);
  result := 0;
  if IsValid(c^.Validity, fChunkRow) then
    if not DuckFixedToDateTime(c^.Info.TypeId,
             PAnsiChar(c^.Data) + fChunkRow * c^.Info.Size, result) then
      result := Iso8601ToDateTime(ColumnUtf8(Col));
end;
function TSqlDBDuckDBStatement.ColumnUtf8(Col: integer): RawUtf8;
var
  c: PDuckDBColumnInfo;
  p: PUtf8Char;
  len: integer; // not PtrInt
begin
  c := CheckCol(Col);
  result := '';
  if not IsValid(c^.Validity, fChunkRow) then
    exit;
  case c^.Info.TypeId of
    DUCKDB_TYPE_VARCHAR, DUCKDB_TYPE_BLOB:
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        FastSetString(result, p, len);
      end;
  else
    DuckDBVectorValueToText(c^.Vector, @c^.Info, fChunkRow, result);
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
  if c^.Info.TypeId in [DUCKDB_TYPE_BLOB, DUCKDB_TYPE_VARCHAR,
                        DUCKDB_TYPE_GEOMETRY] then
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
  case c^.Info.TypeId of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      W.Add(ReadIntegral(c^.Info.TypeId, c^.Data, fChunkRow)); // boolean as 0/1
    DUCKDB_TYPE_VARCHAR:
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        W.Add('"');
        W.AddJsonEscape(p, len);
        W.Add('"');
      end;
    DUCKDB_TYPE_BLOB, DUCKDB_TYPE_GEOMETRY:
      if dsfForceBlobAsNull in fFlags then
        W.AddNull
      else
      begin
        p := DuckStringPtr(DuckStringAt(c^.Data, fChunkRow), len);
        W.WrBase64(p, len, {withmagic=}true);
      end;
    DUCKDB_TYPE_DATE, DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIME_TZ, DUCKDB_TYPE_TIME_NS,
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S, DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS, DUCKDB_TYPE_TIMESTAMP_TZ,
    DUCKDB_TYPE_TIMESTAMP_TZ_NS:
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
    DuckDBVectorValueToJson(c^.Vector, @c^.Info, fChunkRow, W);
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
  sch, tbl: RawUtf8;
begin
  inherited Create;
  if not DuckDB.HasAppender then
    ESqlDBDuckDB.RaiseU('DuckDB library has no appender API');
  if not aConnection.IsConnected then
    aConnection.Connect;
  fTable := aTable;
  sch := aSchema;
  tbl := aTable;
  if sch = '' then
    tbl := SplitRight(aTable, '.', @sch); // 'schema.table' as set by the ORM
  schema := nil;
  if sch <> '' then
    schema := pointer(sch);
  if DuckDB.appender_create(aConnection.fConn,
       schema, pointer(tbl), fAppender) <> DuckDBSuccess then
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
var
  res: TDuckDBState;
  saved: cardinal;
begin
  saved := SetFpuFlags; // a full chunk is written to the table
  try
    res := DuckDB.appender_end_row(fAppender);
  finally
    ResetFpuFlags(saved);
  end;
  Check(res, 'EndRow');
  inc(fRowCount);
end;

procedure TSqlDBDuckDBAppender.Flush;
var
  res: TDuckDBState;
  saved: cardinal;
begin
  saved := SetFpuFlags; // the C++ engine expects masked FPU exceptions
  try
    res := DuckDB.appender_flush(fAppender);
  finally
    ResetFpuFlags(saved);
  end;
  Check(res, 'Flush');
end;

procedure TSqlDBDuckDBAppender.Close;
var
  res: TDuckDBState;
  saved: cardinal;
begin
  if fAppender = nil then
    exit;
  saved := SetFpuFlags; // the pending rows are flushed to the table
  try
    res := DuckDB.appender_close(fAppender);
  finally
    ResetFpuFlags(saved);
  end;
  try
    Check(res, 'Close');
  finally
    DuckDB.appender_destroy(fAppender);
    fAppender := nil;
  end;
end;



{ ************ Vector Value Conversion }

function EnumIndexAt(T: TQuackType; D: PAnsiChar): PtrInt;
begin
  case T.ElemSize of
    1:
      result := PByte(D)^;
    2:
      result := word(QuackInt16(D));
  else
    result := cardinal(QuackInt32(D));
  end;
end;

function QuackInternal(T: TQuackType): TDuckDBType;
begin
  // storage type of a DECIMAL
  case T.ElemSize of
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

procedure QuackValueToText(V: TQuackVector; Row: PtrInt; var Result: RawUtf8);
var
  i: PtrInt;
  t: TQuackType;
  p: PUtf8Char;
  len: integer;
  us: Int64;
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  FastAssignNew(Result);
  i := V.Stored(Row);
  if not V.IsStoredValid(i) then
    exit;
  t := V.VType;
  case t.Storage of
    qsString:
      begin
        p := V.StringAt(i, len);
        if t.DuckType = DUCKDB_TYPE_BIT then
          DuckBitToText(pointer(p), len, Result)
        else
          FastSetString(Result, p, len);
      end;
    qsFixed:
      if t.DuckType = DUCKDB_TYPE_ENUM then
      begin
        us := EnumIndexAt(t, V.DataAt(i));
        if us < length(t.EnumValues) then
          Result := t.EnumValues[us];
      end
      else
        DuckFixedToText(t.DuckType, QuackInternal(t), t.Scale,
          V.DataAt(i), Result);
    qsStruct, qsList, qsArray:
      begin
        // LIST, ARRAY, STRUCT, MAP, UNION -> JSON
        W := TJsonWriter.CreateOwnedStream(tmp);
        try
          QuackValueToJson(V, Row, W);
          W.SetText(Result);
        finally
          W.Free;
        end;
      end;
  end;
end;

procedure QuackValueToJson(V: TQuackVector; Row: PtrInt; W: TJsonWriter);
var
  i, k, n: PtrInt;
  t: TQuackType;
  p: PUtf8Char;
  len: integer;
  e: PDuckDBListEntry;
  txt: RawUtf8;
  sv: TQuackVector;
begin
  i := V.Stored(Row);
  if not V.IsStoredValid(i) then
  begin
    W.AddNull;
    exit;
  end;
  t := V.VType;
  case t.Storage of
    qsString:
      case t.DuckType of
        DUCKDB_TYPE_VARCHAR:
          begin
            p := V.StringAt(i, len);
            W.Add('"');
            W.AddJsonEscape(p, len);
            W.Add('"');
          end;
        DUCKDB_TYPE_BLOB:
          begin
            p := V.StringAt(i, len);
            W.WrBase64(PAnsiChar(p), len, {withmagic=}true);
          end;
      else
        begin // BIT, BIGNUM
          QuackValueToText(V, Row, txt);
          W.Add('"');
          W.AddJsonEscape(pointer(txt), length(txt));
          W.Add('"');
        end;
      end;
    qsFixed:
      if t.DuckType = DUCKDB_TYPE_ENUM then
      begin
        QuackValueToText(V, Row, txt);
        W.Add('"');
        W.AddJsonEscape(pointer(txt), length(txt));
        W.Add('"');
      end
      else
        DuckFixedToJson(t.DuckType, QuackInternal(t), t.Scale, V.DataAt(i), W);
    qsArray:
      begin
        k := i * PtrInt(t.ArraySize);
        W.Add('[');
        for n := 0 to integer(t.ArraySize) - 1 do
        begin
          QuackValueToJson(V.Child, k + n, W);
          W.AddComma;
        end;
        W.CancelLastComma(']');
      end;
    qsStruct:
      if t.DuckType = DUCKDB_TYPE_UNION then
      begin
        // UNION = STRUCT(tag UTINYINT, member1, member2, ...)
        k := PByte(V.Children[0].DataAt(V.Children[0].Stored(i)))^;
        if k + 1 < length(V.Children) then
          QuackValueToJson(V.Children[k + 1], i, W)
        else
          W.AddNull;
      end
      else
      begin
        W.Add('{');
        for n := 0 to high(V.Children) do
        begin
          W.AddProp(pointer(t.ChildNames[n]), length(t.ChildNames[n]));
          QuackValueToJson(V.Children[n], i, W);
          W.AddComma;
        end;
        W.CancelLastComma('}');
      end;
    qsList:
      begin
        e := PDuckDBListEntry(PAnsiChar(V.Entries) + i * SizeOf(TDuckDBListEntry));
        if t.DuckType = DUCKDB_TYPE_MAP then
        begin
          // MAP = LIST(STRUCT(key, value)) -> JSON object
          W.Add('{');
          for k := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
          begin
            sv := V.Child;
            n := sv.Stored(k);
            QuackValueToText(sv.Children[0], n, txt);
            W.AddProp(pointer(txt), length(txt));
            QuackValueToJson(sv.Children[1], n, W);
            W.AddComma;
          end;
          W.CancelLastComma('}');
        end
        else
        begin
          W.Add('[');
          for k := PtrInt(e^.Offset) to PtrInt(e^.Offset + e^.Length) - 1 do
          begin
            QuackValueToJson(V.Child, k, W);
            W.AddComma;
          end;
          W.CancelLastComma(']');
        end;
      end;
  end;
end;


{ ************ TSqlDBQuackConnection* and TSqlDBQuackStatement Classes }

{ TSqlDBQuackConnectionProperties }

constructor TSqlDBQuackConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  fDbms := dPostgreSQL; // DuckDB SQL dialect is PostgreSQL-like
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  fEngineName := 'Quack';
  ForcedSchemaName := 'main'; // DuckDB default schema, for metadata queries
  fHeartbeatSeconds := 60;
  fTimeOutMs := 30000;
  if not QuackUriParse(aServerName, fHost, fPort) then
    ESqlDBQuack.RaiseUtf8('%.Create: invalid Quack server [%]',
      [self, aServerName]);
end;

function TSqlDBQuackConnectionProperties.GetUseTls: boolean;
begin
  case fTlsMode of
    qtmOn:
      result := true;
    qtmOff:
      result := false;
  else
    result := not QuackHostIsLocal(fHost);
  end;
end;

procedure TSqlDBQuackConnectionProperties.GetForeignKeys;
begin
  with Execute('select table_name || ''.'' || constraint_column_names[1], ' +
         'referenced_table || ''.'' || referenced_column_names[1] ' +
         'from duckdb_constraints() where constraint_type = ''FOREIGN KEY''',
         []) do
    while Step do
      fForeignKeys.Add(ColumnUtf8(0), ColumnUtf8(1));
end;

function TSqlDBQuackConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBQuackConnection.Create(self);
  TSqlDBQuackConnection(result).InternalProcess(speCreated);
end;


{ TSqlDBQuackConnection }

function TSqlDBQuackConnection.Post(const Body: RawByteString): TQuackResponse;
var
  status: integer;
begin
  status := fSocket.Request(QUACK_HTTP_PATH, 'POST', 60000, '', Body,
    QUACK_CONTENT_TYPE);
  if status <> HTTP_SUCCESS then
    ESqlDBQuack.RaiseUtf8('%: HTTP error % on %', [self, status,
      TSqlDBQuackConnectionProperties(fProperties).ServerName]);
  result := TQuackResponse.Create(fSocket.Http.Content, fVersion);
  fLastRequestSec := GetTickSec;
end;

procedure TSqlDBQuackConnection.Connect;
var
  props: TSqlDBQuackConnectionProperties;
  tls: TNetTlsContext;
  resp: TQuackResponse;
  token, msg, port: RawUtf8;
  version: cardinal;
begin
  Disconnect;
  props := fProperties as TSqlDBQuackConnectionProperties;
  token := props.PassWord;
  if token = '' then
    ESqlDBQuack.RaiseUtf8('%.Connect: missing Quack token', [self]);
  InitNetTlsContext(tls);
  tls.IgnoreCertificateErrors := props.IgnoreTlsErrors;
  port := UInt32ToUtf8(props.Port);
  try
    fSocket := THttpClientSocket.Open(props.Host, port,
      nlTcp, props.TimeOutMs, props.UseTls, @tls);
  except
    on ENetSock do
      if IdemPropNameU(props.Host, 'localhost') then
        // a Quack server may listen only on the IPv6 loopback
        fSocket := THttpClientSocket.Open('::1', port,
          nlTcp, props.TimeOutMs, props.UseTls, @tls)
      else
        raise;
  end;
  try
    // the protocol version is detected, starting with the released one
    version := props.ProtocolVersion;
    if version = 0 then
      version := QUACK_VERSION_1;
    repeat
      fVersion := version;
      resp := Post(QuackConnectionRequest(token, props.UserID,
        props.HeartbeatSeconds, version));
      try
        if resp.MessageType = QUACK_CONNECTION_RESPONSE then
          break;
        msg := TrimU(resp.ErrorType + ' ' + resp.ErrorMessage);
      finally
        if resp.MessageType <> QUACK_CONNECTION_RESPONSE then
          FreeAndNil(resp);
      end;
      if (props.ProtocolVersion = 0) and
         (version < QUACK_VERSION) and
         (PosEx('version', msg) > 0) then
        version := QUACK_VERSION // try again with the newest protocol
      else
        ESqlDBQuack.RaiseUtf8('%.Connect: % on %', [self, msg, props.ServerName]);
    until false;
    try
      if resp.QuackVersion <> version then
        ESqlDBQuack.RaiseUtf8('%.Connect: unexpected Quack protocol ' +
          'version % (server) vs % (client)', [self, resp.QuackVersion, version]);
      fConnectionId := resp.ConnectionId;
      fLeaseSeconds := resp.HeartbeatSeconds;
    finally
      resp.Free;
    end;
  except
    FreeAndNil(fSocket);
    raise;
  end;
  inherited Connect; // notify any re-connection
end;

procedure TSqlDBQuackConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fSocket <> nil then
    begin
      if fConnectionId <> '' then
        RequestSilently(QuackSimpleRequest(
          QUACK_DISCONNECT_MESSAGE, fConnectionId));
      FreeAndNil(fSocket);
    end;
    fConnectionId := '';
  end;
end;

function TSqlDBQuackConnection.IsConnected: boolean;
begin
  result := fSocket <> nil;
end;

function TSqlDBQuackConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBQuackStatement.Create(self);
end;

procedure TSqlDBQuackConnection.CheckLease;
var
  resp: TQuackResponse;
begin
  if (fVersion < QUACK_VERSION) or // no lease in the version 1 protocol
     (fLeaseSeconds = 0) or
     (GetTickSec - fLastRequestSec < fLeaseSeconds div 2) then
    exit;
  // renew the lease, or recreate the connection if the server forgot us
  try
    resp := Post(QuackSimpleRequest(QUACK_HEARTBEAT_REQUEST, fConnectionId));
    try
      if resp.MessageType <> QUACK_SUCCESS_RESPONSE then
        ESqlDBQuack.RaiseUtf8('%: %', [self, resp.ErrorMessage]);
    finally
      resp.Free;
    end;
  except
    if fTransactionCount > 0 then
      raise; // the transaction was lost
    fConnectionId := ''; // no DISCONNECT_MESSAGE
    Connect;
  end;
end;

function TSqlDBQuackConnection.Request(const Body: RawByteString;
  Expected: byte): TQuackResponse;
var
  msg: RawUtf8;
begin
  if not IsConnected then
    Connect
  else
    CheckLease;
  result := Post(Body);
  if result.MessageType = Expected then
    exit;
  if result.MessageType = QUACK_ERROR_RESPONSE then
    if result.ErrorType = '' then // no exception type with the version 1
      msg := result.ErrorMessage
    else
      msg := result.ErrorType + ' Error: ' + result.ErrorMessage
  else
    FormatUtf8('unexpected response type %', [result.MessageType], msg);
  FreeAndNil(result);
  ESqlDBQuack.RaiseUtf8('%: %', [self, msg]);
end;

procedure TSqlDBQuackConnection.RequestSilently(const Body: RawByteString);
begin
  try
    if fSocket <> nil then
      Post(Body).Free;
  except
    // ignore any error: the server will release its resources by itself
  end;
end;

function TSqlDBQuackConnection.ExecuteScript(const aSql: RawUtf8): Int64;
var
  uuid: TDuckDBHugeInt;
  resp: TQuackResponse;
begin
  QuackNewQueryUuid(uuid);
  resp := Request(QuackPrepareRequest(fConnectionId, aSql, uuid, fVersion),
    QUACK_PREPARE_RESPONSE);
  try
    result := 0;
    if resp.IsCountResult then
      with resp.Chunks[0].Vectors[0] do
        result := QuackInt64(DataAt(Stored(0)));
    if resp.NeedsMoreFetch and
       (fVersion >= QUACK_VERSION) then
      RequestSilently(QuackQueryRequest(
        QUACK_CANCEL_REQUEST, fConnectionId, resp.QueryUuid));
  finally
    resp.Free;
  end;
end;

procedure TSqlDBQuackConnection.StartTransaction;
begin
  inherited StartTransaction;
  try
    ExecuteScript('BEGIN TRANSACTION');
  except
    dec(fTransactionCount);
    raise;
  end;
end;

procedure TSqlDBQuackConnection.Commit;
begin
  inherited Commit;
  try
    ExecuteScript('COMMIT');
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSqlDBQuackConnection.Rollback;
begin
  inherited Rollback;
  ExecuteScript('ROLLBACK');
end;


{ TSqlDBQuackStatement }

destructor TSqlDBQuackStatement.Destroy;
begin
  try
    ReleaseResult;
  finally
    inherited Destroy;
  end;
end;

procedure TSqlDBQuackStatement.ReleaseResult;
begin
  if fMoreToFetch and
     (fConnection <> nil) and
     (TSqlDBQuackConnection(fConnection).fVersion >= QUACK_VERSION) then
    // the result was not fully retrieved: stop its processing on the server
    TSqlDBQuackConnection(fConnection).RequestSilently(QuackQueryRequest(
      QUACK_CANCEL_REQUEST, TSqlDBQuackConnection(fConnection).fConnectionId,
      fQueryUuid));
  fMoreToFetch := false;
  FreeAndNil(fResponse);
  fChunk := nil;
  fChunkIndex := -1;
  fChunkRow := 0;
  fExecuted := false;
end;

procedure TSqlDBQuackStatement.Prepare(const aSql: RawUtf8;
  ExpectResults: boolean);
begin
  SqlLogBegin(sllDB);
  inherited Prepare(aSql, ExpectResults); // set fSql + Connect if necessary
  SqlLogEnd;
end;

function NextParam(P: PUtf8Char): PUtf8Char;
var
  quote: AnsiChar;
begin
  // find the next ? place holder, ignoring quoted text and identifiers
  repeat
    case P^ of
      #0, '?':
        break;
      '''', '"':
        begin
          quote := P^;
          inc(P);
          while P^ <> #0 do
            if P^ <> quote then
              inc(P)
            else if P[1] = quote then
              inc(P, 2) // escaped quote
            else
              break;
          if P^ = #0 then
            break;
        end;
    end;
    inc(P);
  until false;
  result := P;
end;

procedure AddBlobLiteral(W: TJsonWriter; P: PByteArray; Len: PtrInt);
const
  HEX: array[0..15] of AnsiChar = '0123456789abcdef';
var
  i: PtrInt;
begin
  // DuckDB blob literals use '\xAA\xBB' escapes, with one \x per byte
  W.Add('''');
  for i := 0 to Len - 1 do
  begin
    W.Add('\');
    W.Add('x');
    W.Add(HEX[P^[i] shr 4]);
    W.Add(HEX[P^[i] and 15]);
  end;
  W.AddShort('''::BLOB');
end;

procedure TSqlDBQuackStatement.AddParam(W: TJsonWriter; Index: integer);
var
  p: PSqlDBParam;
  bits: Int64;
begin
  p := @fParams[Index];
  case p^.VType of
    ftNull:
      W.AddShort('null');
    ftInt64:
      W.Add(p^.VInt64);
    ftDouble:
      begin
        bits := p^.VInt64;
        if (bits shr 52) and $7ff = $7ff then // NaN or Infinity
          if bits and $000fffffffffffff <> 0 then
            W.AddShort('''nan''::DOUBLE')
          else if bits < 0 then
            W.AddShort('''-inf''::DOUBLE')
          else
            W.AddShort('''inf''::DOUBLE')
        else
          W.AddDouble(unaligned(PDouble(@p^.VInt64)^));
      end;
    ftCurrency:
      W.AddCurr64(@p^.VInt64);
    ftDate:
      begin
        W.AddShort('TIMESTAMP ''');
        W.AddDateTime(unaligned(PDateTime(@p^.VInt64)^), {withms=}true);
        W.Add('''');
      end;
    ftUtf8:
      W.AddQuotedStr(pointer(p^.VData), length(p^.VData), '''');
    ftBlob:
      AddBlobLiteral(W, pointer(p^.VData), length(p^.VData));
  else
    ESqlDBQuack.RaiseUtf8('%: cannot bind parameter #% type %',
      [self, Index + 1, ord(p^.VType)]);
  end;
end;

function TSqlDBQuackStatement.BuildSql: RawUtf8;
var
  P, B: PUtf8Char;
  n: integer;
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  // the Quack protocol has no bound parameters: inline them as SQL literals
  if fParamCount = 0 then
  begin
    result := fSql;
    exit;
  end;
  W := TJsonWriter.CreateOwnedStream(tmp);
  try
    P := pointer(fSql);
    n := 0;
    repeat
      B := P;
      P := NextParam(P);
      W.AddNoJsonEscape(B, P - B);
      if P^ = #0 then
        break;
      inc(P); // jump P^='?'
      if n >= fParamCount then
        ESqlDBQuack.RaiseUtf8('%: missing parameter #% in [%]',
          [self, n + 1, fSql]);
      AddParam(W, n);
      inc(n);
    until false;
    if n <> fParamCount then
      ESqlDBQuack.RaiseUtf8('%: % bound parameters for % ? in [%]',
        [self, fParamCount, n, fSql]);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TSqlDBQuackStatement.ExecutePrepared;
var
  i: PtrInt;
  conn: TSqlDBQuackConnection;
  resp: TQuackResponse;
  col: PSqlDBColumnProperty;
  sql: RawUtf8;
begin
  SqlLogBegin(sllSQL);
  inherited ExecutePrepared; // set fConnection.fLastAccessTicks
  ReleaseResult;
  ClearColumns;
  fColumnCount := 0;
  fCurrentRow := 0;
  fUpdateCount := 0;
  if fParamsArrayCount > 0 then
    ESqlDBQuack.RaiseUtf8('%.ExecutePrepared: BindArray() is not supported - ' +
      'use OnBatchInsert or a transaction', [self]);
  conn := TSqlDBQuackConnection(fConnection);
  sql := BuildSql;
  QuackNewQueryUuid(fQueryUuid);
  try
    // PREPARE_REQUEST also runs the query on the server
    fResponse := conn.Request(QuackPrepareRequest(conn.fConnectionId, sql,
      fQueryUuid, conn.fVersion), QUACK_PREPARE_RESPONSE);
  except
    on E: Exception do
    begin
      SqlLogEnd;
      ESqlDBQuack.RaiseUtf8('%.ExecutePrepared: % [%]', [self, E.Message, fSql]);
    end;
  end;
  resp := fResponse;
  if (resp.QueryUuid.Lower <> 0) or
     (resp.QueryUuid.Upper <> 0) then
    fQueryUuid := resp.QueryUuid;
  fMoreToFetch := resp.NeedsMoreFetch;
  fNextBatch := 1;
  fExecuted := true;
  fChunkIndex := -1;
  // retrieve the columns description
  for i := 0 to length(resp.ColumnTypes) - 1 do
  begin
    col := AddColumn(resp.ColumnNames[i]);
    col^.ColumnType := resp.ColumnTypes[i].FieldType;
    col^.ColumnAttr := resp.ColumnTypes[i].DuckType;
  end;
  // DML statements return a single "Count" BIGINT row
  if resp.IsCountResult then
    with resp.Chunks[0].Vectors[0] do
      fUpdateCount := QuackInt64(DataAt(Stored(0)));
  SqlLogEnd;
end;

function TSqlDBQuackStatement.UpdateCount: integer;
begin
  result := fUpdateCount;
end;

procedure TSqlDBQuackStatement.Reset;
begin
  ReleaseResult;
  fUpdateCount := 0;
  inherited Reset;
end;

procedure TSqlDBQuackStatement.ReleaseRows;
begin
  ReleaseResult;
  inherited ReleaseRows;
end;

procedure TSqlDBQuackStatement.FetchNext;
var
  conn: TSqlDBQuackConnection;
  resp: TQuackResponse;
begin
  conn := TSqlDBQuackConnection(fConnection);
  resp := conn.Request(QuackFetchRequest(conn.fConnectionId, fQueryUuid,
    fNextBatch, fNextBatch - 1, conn.fVersion), QUACK_FETCH_RESPONSE);
  FreeAndNil(fResponse);
  fResponse := resp;
  fChunkIndex := -1;
  if length(resp.Chunks) = 0 then
  begin
    // an empty response means that all batches have been received
    fMoreToFetch := false;
    if conn.fVersion >= QUACK_VERSION then
      conn.RequestSilently(QuackQueryRequest(
        QUACK_ACKNOWLEDGEMENT, conn.fConnectionId, fQueryUuid));
  end
  else if conn.fVersion < QUACK_VERSION then
    exit // the version 1 protocol has no batch index
  else if resp.BatchIndex <> fNextBatch then
    ESqlDBQuack.RaiseUtf8('%: FETCH batch % received instead of %',
      [self, resp.BatchIndex, fNextBatch])
  else
    inc(fNextBatch);
end;

function TSqlDBQuackStatement.NextChunk: boolean;
begin
  result := false;
  repeat
    inc(fChunkIndex);
    while (fResponse <> nil) and
          (fChunkIndex >= length(fResponse.Chunks)) do
    begin
      if not fMoreToFetch then
      begin
        fChunk := nil;
        exit;
      end;
      FetchNext; // sets fChunkIndex := -1
      inc(fChunkIndex);
    end;
    if fResponse = nil then
      exit;
    fChunk := fResponse.Chunks[fChunkIndex];
  until fChunk.RowCount > 0;
  fChunkRow := 0;
  result := true;
end;

function TSqlDBQuackStatement.Step(SeekFirst: boolean): boolean;
begin
  result := false;
  if SeekFirst and
     (fCurrentRow > 0) then
    ESqlDBQuack.RaiseUtf8('%.Step(SeekFirst=true) not implemented', [self]);
  if not fExecuted or
     (fColumnCount = 0) then
    exit;
  if fChunk <> nil then
    inc(fChunkRow);
  if (fChunk = nil) or
     (fChunkRow >= fChunk.RowCount) then
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

function TSqlDBQuackStatement.CheckCol(Col: integer): TQuackVector;
begin
  if (fChunk = nil) or
     (cardinal(Col) >= cardinal(length(fChunk.Vectors))) then
    ESqlDBQuack.RaiseUtf8('%: invalid Column(%) or no current row',
      [self, Col]);
  result := fChunk.Vectors[Col];
end;

function TSqlDBQuackStatement.ColumnNull(Col: integer): boolean;
begin
  result := not CheckCol(Col).IsValid(fChunkRow);
end;

function TSqlDBQuackStatement.ColumnInt(Col: integer): Int64;
var
  v: TQuackVector;
  i: PtrInt;
  t: TQuackType;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  t := v.VType;
  if t.Storage = qsFixed then
    case DuckFixedToInt64(t.DuckType, QuackInternal(t), t.Scale,
           v.DataAt(i), result) of
      dcOk:
        exit;
      dcOverflow:
        ESqlDBQuack.RaiseUtf8('%.ColumnInt(%): 128-bit overflow', [self, Col]);
    end;
  result := GetInt64(pointer(ColumnUtf8(Col)));
end;

function TSqlDBQuackStatement.ColumnDouble(Col: integer): double;
var
  v: TQuackVector;
  i: PtrInt;
  t: TQuackType;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  t := v.VType;
  if (t.Storage <> qsFixed) or
     not DuckFixedToDouble(t.DuckType, QuackInternal(t), t.Scale,
           v.DataAt(i), result) then
    result := GetExtended(pointer(ColumnUtf8(Col)));
end;

function TSqlDBQuackStatement.ColumnCurrency(Col: integer): currency;
var
  v: TQuackVector;
  i: PtrInt;
  t: TQuackType;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  t := v.VType;
  if (t.Storage <> qsFixed) or
     not DuckFixedToCurrency(t.DuckType, QuackInternal(t), t.Scale,
           v.DataAt(i), result) then
    PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
end;

function TSqlDBQuackStatement.ColumnDateTime(Col: integer): TDateTime;
var
  v: TQuackVector;
  i: PtrInt;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if (v.VType.Storage <> qsFixed) or
     not DuckFixedToDateTime(v.VType.DuckType, v.DataAt(i), result) then
    result := Iso8601ToDateTime(ColumnUtf8(Col));
end;
function TSqlDBQuackStatement.ColumnUtf8(Col: integer): RawUtf8;
begin
  QuackValueToText(CheckCol(Col), fChunkRow, result);
end;

function TSqlDBQuackStatement.ColumnBlob(Col: integer): RawByteString;
var
  v: TQuackVector;
  i: PtrInt;
  p: PUtf8Char;
  len: integer;
begin
  v := CheckCol(Col);
  FastAssignNew(result);
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if v.VType.DuckType in [DUCKDB_TYPE_BLOB, DUCKDB_TYPE_VARCHAR] then
  begin
    p := v.StringAt(i, len);
    FastSetRawByteString(result, p, len);
  end
  else
    result := ColumnUtf8(Col);
end;

procedure TSqlDBQuackStatement.ColumnToJson(Col: integer; W: TJsonWriter);
var
  v: TQuackVector;
  i: PtrInt;
begin
  v := CheckCol(Col);
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
  begin
    W.AddNull;
    exit;
  end;
  case v.VType.DuckType of
    DUCKDB_TYPE_BOOLEAN:
      W.Add(PByte(v.DataAt(i))^); // boolean as 0/1
    DUCKDB_TYPE_BLOB:
      if dsfForceBlobAsNull in fFlags then
        W.AddNull
      else
        QuackValueToJson(v, fChunkRow, W);
    DUCKDB_TYPE_DATE, DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIME_TZ,
    DUCKDB_TYPE_TIME_NS, DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS,
    DUCKDB_TYPE_TIMESTAMP_TZ:
      begin
        W.Add('"');
        W.AddDateTime(ColumnDateTime(Col), dsfForceDateWithMS in fFlags);
        W.Add('"');
      end;
  else
    QuackValueToJson(v, fChunkRow, W);
  end;
end;


initialization
  TSqlDBDuckDBConnectionProperties.RegisterClassNameForDefinition;
  TSqlDBQuackConnectionProperties.RegisterClassNameForDefinition;

end.
