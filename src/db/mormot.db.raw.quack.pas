/// Database Framework Low-Level DuckDB Quack Client/Server Protocol
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// - Delphi 7 .. Delphi 13 and FPC 3.2+ compatible syntax
unit mormot.db.raw.quack;

{
  *****************************************************************************

   DuckDB Quack Wire Protocol Encoding and Decoding
    - Quack Constants and Exception
    - TQuackType Logical Types Description
    - TQuackVector and TQuackChunk Result Data Access
    - TQuackResponse Messages Decoding
    - Quack Requests Encoding

  *****************************************************************************

  Notes:
  - Quack is the DuckDB client/server protocol, see
    https://duckdb.org/docs/current/quack/overview and
    https://github.com/duckdb/duckdb-quack
  - messages are HTTP POST to /quack, with a "application/vnd.duckdb" body
    using the DuckDB "BinarySerializer" format: 16-bit field ids, LEB128
    variable-length integers, and every object ending with a 0xFFFF id
  - result rows are sent as DuckDB DataChunk vectors: this unit decodes them
    "in place", i.e. with no memory copy of the fixed-size values, the strings
    or the validity masks, which are accessed directly from the HTTP response
    buffer - so a TQuackResponse instance owns the memory of its chunks
  - the released quack extension (DuckDB 1.5) uses the protocol version 1, and
    its development branch uses the version 3: both are supported, the main
    differences being in CONNECTION_REQUEST, PREPARE_REQUEST, FETCH_REQUEST
    and FETCH_RESPONSE messages
  - no sockets here: see mormot.db.sql.quack for the TSqlDB* classes
}

interface

{$I mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb;


{ ************ Quack Constants and Exception }

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
    fOwned: pointer;
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

/// read a 16-bit/32-bit/64-bit value which may be unaligned in the response
function QuackInt16(P: pointer): integer;
function QuackInt32(P: pointer): integer;
function QuackInt64(P: pointer): Int64;
function QuackDouble(P: pointer): double;


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


{ ************ Low-Level Reading Functions }

procedure RaiseQuack(const Msg: RawUtf8; const Args: array of const);
begin
  ESqlDBQuack.RaiseUtf8('Quack: ' + Msg, Args);
end;

function PeekField(var R: TFastReader): cardinal;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if R.P + 1 >= R.Last then
    R.ErrorOverflow;
  result := PWord(R.P)^;
end;

procedure ExpectField(var R: TFastReader; Id: cardinal);
var
  f: cardinal;
begin
  f := R.Next2;
  if f <> Id then
    RaiseQuack('unexpected field % instead of %', [f, Id]);
end;

procedure ExpectEnd(var R: TFastReader);
begin
  ExpectField(R, FIELD_END);
end;

function ReadLen(var R: TFastReader): PtrInt;
var
  v: QWord;
begin
  v := R.VarUInt64;
  if (Int64(v) < 0) or
     (Int64(v) > R.Last - R.P) then
    RaiseQuack('length % overflow', [Int64(v)]);
  result := v;
end;

function ReadCount(var R: TFastReader; MinBytesPerItem: PtrInt): PtrInt;
var
  v: QWord;
begin
  // a list count can't be bigger than the remaining bytes
  v := R.VarUInt64;
  if (Int64(v) < 0) or
     (Int64(v) > (R.Last - R.P) div MinBytesPerItem) then
    RaiseQuack('list count % overflow', [Int64(v)]);
  result := v;
end;

function ReadStr(var R: TFastReader): RawUtf8;
var
  len: PtrInt;
begin
  len := ReadLen(R);
  FastSetString(result, R.Next(len), len);
end;

function ReadIdx(var R: TFastReader): Int64;
begin
  result := Int64(R.VarUInt64); // optional_idx: stored as idx_t
end;

function ReadSVarInt64(var R: TFastReader): Int64;
var
  shift: integer;
  b: cardinal;
begin
  result := 0;
  shift := 0;
  repeat
    b := R.NextByte;
    if shift < 64 then
      result := result or (Int64(b and $7f) shl shift);
    inc(shift, 7);
  until b < $80;
  if (shift < 64) and
     (b and $40 <> 0) then
    result := result or (Int64(-1) shl shift); // sign extension
end;

procedure ReadHugeInt(var R: TFastReader; out V: TDuckDBHugeInt);
begin
  V.Upper := ReadSVarInt64(R);
  V.Lower := Int64(R.VarUInt64);
end;

function QuackInt16(P: pointer): integer;
begin
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  result := 0;
  MoveFast(P^, result, 2);
  result := SmallInt(result);
  {$else}
  result := PSmallInt(P)^;
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function QuackInt32(P: pointer): integer;
begin
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  MoveFast(P^, result, 4);
  {$else}
  result := PInteger(P)^;
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function QuackInt64(P: pointer): Int64;
begin
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  MoveFast(P^, result, 8);
  {$else}
  result := PInt64(P)^;
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function QuackDouble(P: pointer): double;
begin
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  MoveFast(P^, result, 8);
  {$else}
  result := PDouble(P)^;
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;


{ ************ TQuackType Logical Types Description }

{ TQuackType }

function TQuackType.FieldType: TSqlDBFieldType;
begin
  case Id of
    QTYPE_BOOLEAN, QTYPE_TINYINT, QTYPE_SMALLINT, QTYPE_INTEGER, QTYPE_BIGINT,
    QTYPE_UTINYINT, QTYPE_USMALLINT, QTYPE_UINTEGER, QTYPE_UBIGINT,
    QTYPE_HUGEINT, QTYPE_UHUGEINT:
      result := ftInt64;
    QTYPE_FLOAT, QTYPE_DOUBLE:
      result := ftDouble;
    QTYPE_DECIMAL:
      if Scale <= 4 then
        result := ftCurrency
      else
        result := ftDouble;
    QTYPE_DATE, QTYPE_TIME, QTYPE_TIME_TZ, QTYPE_TIME_NS, QTYPE_TIMESTAMP,
    QTYPE_TIMESTAMP_SEC, QTYPE_TIMESTAMP_MS, QTYPE_TIMESTAMP_NS,
    QTYPE_TIMESTAMP_TZ:
      result := ftDate;
    QTYPE_BLOB, QTYPE_GEOMETRY:
      result := ftBlob;
    QTYPE_SQLNULL:
      result := ftNull;
  else
    result := ftUtf8; // VARCHAR, UUID, ENUM, INTERVAL, BIT, nested as JSON
  end;
end;

type
  TQuackFixedType = record
    Id: byte;
    Size: byte;
    DuckType: byte;
  end;

const
  // the fixed-size types, with their DUCKDB_TYPE_* equivalent
  FIXED_TYPES: array[0..28] of TQuackFixedType = (
    (Id: QTYPE_SQLNULL;         Size: 1;  DuckType: DUCKDB_TYPE_SQLNULL),
    (Id: QTYPE_BOOLEAN;         Size: 1;  DuckType: DUCKDB_TYPE_BOOLEAN),
    (Id: QTYPE_TINYINT;         Size: 1;  DuckType: DUCKDB_TYPE_TINYINT),
    (Id: QTYPE_UTINYINT;        Size: 1;  DuckType: DUCKDB_TYPE_UTINYINT),
    (Id: QTYPE_SMALLINT;        Size: 2;  DuckType: DUCKDB_TYPE_SMALLINT),
    (Id: QTYPE_USMALLINT;       Size: 2;  DuckType: DUCKDB_TYPE_USMALLINT),
    (Id: QTYPE_INTEGER;         Size: 4;  DuckType: DUCKDB_TYPE_INTEGER),
    (Id: QTYPE_UINTEGER;        Size: 4;  DuckType: DUCKDB_TYPE_UINTEGER),
    (Id: QTYPE_DATE;            Size: 4;  DuckType: DUCKDB_TYPE_DATE),
    (Id: QTYPE_FLOAT;           Size: 4;  DuckType: DUCKDB_TYPE_FLOAT),
    (Id: QTYPE_BIGINT;          Size: 8;  DuckType: DUCKDB_TYPE_BIGINT),
    (Id: QTYPE_UBIGINT;         Size: 8;  DuckType: DUCKDB_TYPE_UBIGINT),
    (Id: QTYPE_POINTER;         Size: 8;  DuckType: DUCKDB_TYPE_UBIGINT),
    (Id: QTYPE_DOUBLE;          Size: 8;  DuckType: DUCKDB_TYPE_DOUBLE),
    (Id: QTYPE_TIME;            Size: 8;  DuckType: DUCKDB_TYPE_TIME),
    (Id: QTYPE_TIME_NS;         Size: 8;  DuckType: DUCKDB_TYPE_TIME_NS),
    (Id: QTYPE_TIME_TZ;         Size: 8;  DuckType: DUCKDB_TYPE_TIME_TZ),
    (Id: QTYPE_TIMESTAMP;       Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP),
    (Id: QTYPE_TIMESTAMP_SEC;   Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP_S),
    (Id: QTYPE_TIMESTAMP_MS;    Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP_MS),
    (Id: QTYPE_TIMESTAMP_NS;    Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP_NS),
    (Id: QTYPE_TIMESTAMP_TZ;    Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP_TZ),
    (Id: QTYPE_TIMESTAMP_TZ_NS; Size: 8;  DuckType: DUCKDB_TYPE_TIMESTAMP_TZ),
    (Id: QTYPE_INTERVAL;        Size: 16; DuckType: DUCKDB_TYPE_INTERVAL),
    (Id: QTYPE_HUGEINT;         Size: 16; DuckType: DUCKDB_TYPE_HUGEINT),
    (Id: QTYPE_UHUGEINT;        Size: 16; DuckType: DUCKDB_TYPE_UHUGEINT),
    (Id: QTYPE_UUID;            Size: 16; DuckType: DUCKDB_TYPE_UUID),
    (Id: QTYPE_DECIMAL;         Size: 0;  DuckType: DUCKDB_TYPE_DECIMAL),
    (Id: QTYPE_ENUM;            Size: 0;  DuckType: DUCKDB_TYPE_ENUM));

procedure SetTypeStorage(T: TQuackType);
var
  i, n: PtrInt;
begin
  T.Storage := qsFixed;
  for i := 0 to high(FIXED_TYPES) do
    if FIXED_TYPES[i].Id = T.Id then
    begin
      T.ElemSize := FIXED_TYPES[i].Size;
      T.DuckType := FIXED_TYPES[i].DuckType;
      case T.Id of
        QTYPE_DECIMAL: // the storage depends on the width
          if T.Width <= 4 then
            T.ElemSize := 2
          else if T.Width <= 9 then
            T.ElemSize := 4
          else if T.Width <= 18 then
            T.ElemSize := 8
          else
            T.ElemSize := 16;
        QTYPE_ENUM: // the storage depends on the dictionary size
          begin
            n := length(T.EnumValues);
            if n <= $100 then
              T.ElemSize := 1
            else if n <= $10000 then
              T.ElemSize := 2
            else
              T.ElemSize := 4;
          end;
      end;
      exit;
    end;
  // variable-length or nested storage
  case T.Id of
    QTYPE_CHAR, QTYPE_VARCHAR:
      begin
        T.Storage := qsString;
        T.DuckType := DUCKDB_TYPE_VARCHAR;
      end;
    QTYPE_BLOB, QTYPE_GEOMETRY:
      begin
        T.Storage := qsString;
        T.DuckType := DUCKDB_TYPE_BLOB;
      end;
    QTYPE_BIT:
      begin
        T.Storage := qsString;
        T.DuckType := DUCKDB_TYPE_BIT;
      end;
    QTYPE_BIGNUM:
      begin
        T.Storage := qsString;
        T.DuckType := DUCKDB_TYPE_BIGNUM;
      end;
    QTYPE_STRUCT, QTYPE_TUPLE:
      begin
        T.Storage := qsStruct;
        T.DuckType := DUCKDB_TYPE_STRUCT;
      end;
    QTYPE_UNION:
      begin
        T.Storage := qsStruct;
        T.DuckType := DUCKDB_TYPE_UNION;
      end;
    QTYPE_LIST:
      begin
        T.Storage := qsList;
        T.DuckType := DUCKDB_TYPE_LIST;
      end;
    QTYPE_MAP:
      begin
        T.Storage := qsList;
        T.DuckType := DUCKDB_TYPE_MAP;
      end;
    QTYPE_ARRAY:
      begin
        T.Storage := qsArray;
        T.DuckType := DUCKDB_TYPE_ARRAY;
      end;
  else
    T.Storage := qsUnsupported; // e.g. VARIANT
  end;
  if (T.Storage in [qsList, qsArray]) and
     (length(T.Children) <> 1) then
    T.Storage := qsUnsupported;
end;


{ ************ TQuackVector and TQuackChunk Result Data Access }

{ TQuackVector }

destructor TQuackVector.Destroy;
var
  i: PtrInt;
begin
  Child.Free;
  for i := 0 to high(Children) do
    Children[i].Free;
  if fOwned <> nil then
    FreeMem(fOwned);
  inherited Destroy;
end;

function TQuackVector.Stored(Row: PtrInt): PtrInt;
begin
  case Mode of
    qvmConstant:
      result := 0;
    qvmDictionary:
      result := cardinal(QuackInt32(Sel + Row * 4));
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
  result := PUtf8Char(StrBytes + StrOffs^[Index]);
  Len := StrOffs^[StoredCount + Index];
end;

{ TQuackChunk }

destructor TQuackChunk.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(Vectors) do
    Vectors[i].Free;
  inherited Destroy;
end;


{ ************ TQuackResponse Messages Decoding }

{ TQuackResponse }

function TQuackResponse.NewType: TQuackType;
begin
  result := TQuackType.Create;
  if fTypePoolCount = length(fTypePool) then
    SetLength(fTypePool, fTypePoolCount + 8 + fTypePoolCount shr 1);
  fTypePool[fTypePoolCount] := result;
  inc(fTypePoolCount);
end;

function TQuackResponse.ReadType(var R: TFastReader): TQuackType;
var
  f, n, infotype, w: cardinal;
  i: PtrInt;
begin
  // a LogicalType is an object with its id and an optional LogicalTypeInfo
  inc(fTypeDepth);
  if fTypeDepth > 32 then
    RaiseQuack('types nesting overflow', []);
  result := NewType;
  ExpectField(R, 100);
  n := R.VarUInt32;
  if n > 255 then
    RaiseQuack('unexpected type %', [n]);
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
                result.Alias := ReadStr(R);
              103:
                RaiseQuack('unsupported extension type %', [result.Alias]);
              200:
                case infotype of
                  INFO_DECIMAL:
                    result.Width := R.VarUInt32;
                  INFO_STRING:
                    ReadStr(R); // collation
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
                        result.ChildNames[i] := ReadStr(R);
                        ExpectField(R, 1);
                        result.Children[i] := ReadType(R);
                        ExpectEnd(R);
                      end;
                    end;
                  INFO_ENUM:
                    begin
                      n := R.VarUInt32; // values_count
                      if n > cardinal(R.Last - R.P) then
                        RaiseQuack('enum size % overflow', [n]);
                      SetLength(result.EnumValues, n);
                    end;
                else
                  RaiseQuack('unsupported type info %', [infotype]);
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
                        RaiseQuack('enum values count mismatch', []);
                      for i := 0 to w - 1 do
                        result.EnumValues[i] := ReadStr(R);
                    end;
                else
                  RaiseQuack('unsupported type info % field', [infotype]);
                end;
            else
              RaiseQuack('unexpected type info field %', [f]);
            end;
          until false;
        end;
    else
      RaiseQuack('unexpected type field %', [f]);
    end;
  until false;
  SetTypeStorage(result);
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
    RaiseQuack('unsupported type %', [t.Id]);
  if t.Id = QTYPE_GEOMETRY then
    if PeekField(R) = 99 then
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
      RaiseQuack('validity size mismatch', []);
    V.Validity := R.Next(len);
  end;
  case t.Storage of
    qsFixed:
      begin
        ExpectField(R, 102);
        len := ReadLen(R);
        if len <> Count * t.ElemSize then
          RaiseQuack('data size mismatch for type %', [t.Id]);
        V.Data := R.Next(len);
      end;
    qsString:
      begin
        GetMem(V.fOwned, Count * 8 + 8);
        o := V.fOwned;
        V.StrOffs := o;
        if PeekField(R) = 107 then
        begin
          // lengths and bytes as two blobs
          R.Next2;
          total := ReadIdx(R);
          ExpectField(R, 108);
          len := ReadLen(R);
          if len <> Count * 4 then
            RaiseQuack('string lengths size mismatch', []);
          lens := R.Next(len);
          ExpectField(R, 109);
          len := ReadLen(R);
          if len <> total then
            RaiseQuack('string bytes size mismatch', []);
          V.StrBytes := R.Next(len);
          acc := 0;
          for i := 0 to Count - 1 do
          begin
            o^[i] := acc;
            o^[Count + i] := cardinal(QuackInt32(lens + i * 4));
            inc(acc, o^[Count + i]);
            if acc > total then
              RaiseQuack('string lengths overflow', []);
          end;
          if acc <> total then
            RaiseQuack('string lengths mismatch', []);
        end
        else
        begin
          // older format: a list of strings (e.g. from the version 1 server)
          ExpectField(R, 102);
          n := ReadCount(R, 1);
          if n <> Count then
            RaiseQuack('strings count mismatch', []);
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
          RaiseQuack('struct children count mismatch', []);
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
          RaiseQuack('list size overflow', []);
        ExpectField(R, 105);
        n := ReadCount(R, 6);
        if n <> Count then
          RaiseQuack('list entries count mismatch', []);
        GetMem(V.fOwned, n * SizeOf(TDuckDBListEntry) + 1);
        e := V.fOwned;
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
              RaiseQuack('list entry out of range', []);
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
          RaiseQuack('array size mismatch', []);
        ExpectField(R, 104);
        if Int64(Count) * t.ArraySize > MaxInt then
          RaiseQuack('array size overflow', []);
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
    if PeekField(R) = 90 then
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
            RaiseQuack('dictionary selection size mismatch', []);
          result.Sel := R.Next(len);
          ExpectField(R, 92);
          len := Int64(R.VarUInt64); // dict_count
          if (len < 0) or
             (len > R.Last - R.P) then
            RaiseQuack('dictionary size overflow', []);
          ReadVectorBody(R, result, len);
          for i := 0 to Count - 1 do
            if cardinal(QuackInt32(result.Sel + i * 4)) >= cardinal(len) then
              RaiseQuack('dictionary index out of range', []);
        end;
      VECTOR_SEQUENCE:
        begin
          // materialize the sequence as a regular flat vector
          ExpectField(R, 91);
          start := ReadSVarInt64(R);
          ExpectField(R, 92);
          step := ReadSVarInt64(R);
          if (T.Storage <> qsFixed) or
             (T.ElemSize > 8) then
            RaiseQuack('unsupported sequence type %', [T.Id]);
          GetMem(result.fOwned, Count * T.ElemSize + 1);
          d := result.fOwned;
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
      RaiseQuack('unsupported vector type %', [vt]);
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
      RaiseQuack('chunk columns count mismatch', []);
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
      RaiseQuack('unexpected void chunk', []);
    ExpectField(R, 300);
    Chunks[i] := ReadChunk(R);
    ExpectEnd(R);
  end;
end;

procedure TQuackResponse.ReadPrepare(var R: TFastReader);
var
  f: cardinal;
  i, n: PtrInt;
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
            ColumnNames[i] := ReadStr(R);
        end;
      3:
        NeedsMoreFetch := R.NextByte <> 0;
      4:
        ReadChunks(R);
      5:
        ReadHugeInt(R, QueryUuid);
    else
      RaiseQuack('unexpected prepare response field %', [f]);
    end;
  until false;
  if length(ColumnNames) <> length(ColumnTypes) then
    RaiseQuack('columns names and types count mismatch', []);
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
        RaiseQuack('unexpected fetch response field %', [f]);
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
      RaiseQuack('unexpected fetch response field %', [f]);
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
        ServerVersion := ReadStr(R);
      2:
        ServerPlatform := ReadStr(R);
      3:
        QuackVersion := R.VarUInt32;
      4:
        HeartbeatSeconds := R.VarUInt32;
    else
      RaiseQuack('unexpected connection response field %', [f]);
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
        ErrorMessage := ReadStr(R);
      2:
        ErrorType := ReadStr(R);
      3: // extra_info map as a list of key/value objects: ignored
        begin
          n := ReadCount(R, 2);
          for i := 0 to n - 1 do
          begin
            ExpectField(R, 0);
            ReadStr(R);
            ExpectField(R, 1);
            ReadStr(R);
            ExpectEnd(R);
          end;
        end;
      4:
        MustInvalidate := R.NextByte <> 0;
    else
      RaiseQuack('unexpected error response field %', [f]);
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
          ConnectionId := ReadStr(R);
        3:
          ReadIdx(R); // client_query_id
      else
        RaiseQuack('unexpected header field %', [f]);
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
            RaiseQuack('unexpected response field', []);
        end;
    else
      RaiseQuack('unexpected response message type %', [MessageType]);
    end;
  except
    on E: EFastReader do // the destructor is called by the exception
      RaiseQuack('malformed response (%)', [E.Message]);
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

procedure WriteSVarInt64(W: TBufferWriter; Value: Int64);
var
  b: cardinal;
begin
  repeat
    b := Value and $7f;
    if Value < 0 then
      Value := not (not Value shr 7) // arithmetic shift
    else
      Value := Value shr 7;
    if ((Value = 0) and
        (b and $40 = 0)) or
       ((Value = -1) and
        (b and $40 <> 0)) then
    begin
      W.Write1(b);
      exit;
    end;
    W.Write1(b or $80);
  until false;
end;

procedure WriteHugeInt(W: TBufferWriter; Id: cardinal;
  const Value: TDuckDBHugeInt);
begin
  WriteField(W, Id);
  WriteSVarInt64(W, Value.Upper);
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

end.
