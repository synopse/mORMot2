/// Database Framework Direct DuckDB Quack Client Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// - Delphi 7 .. Delphi 13 and FPC 3.2+ compatible syntax
unit mormot.db.sql.quack;

{
  *****************************************************************************

   Native DuckDB Quack Client using mormot.db.raw.quack Protocol Decoder
    - TSqlDBQuackConnectionProperties: server end point and credentials
    - TSqlDBQuackConnection: one HTTP keep-alive socket per (thread) connection
    - TSqlDBQuackStatement: remote execution and chunk-based row access

  *****************************************************************************

  Notes:
  - Quack is the DuckDB client/server protocol over HTTP: this unit is a
    pure Pascal client, which does not need any DuckDB library
    (see mormot.db.sql.duckdb for the embedded in-process engine)
  - the server is started with CALL quack_serve('quack:localhost',
    token = 'secret') on a DuckDB instance, see
    https://duckdb.org/docs/current/quack/overview
  - the protocol sends the SQL as text, so bound parameters are inlined as SQL
    literals by this client - always use ? parameters and Bind*() methods
  - the released quack extension of DuckDB 1.5 uses the protocol version 1, and
    its development branch uses version 3: both are supported and the version
    is detected on connection (see ProtocolVersion)
  - results are streamed by batches, using FETCH requests on demand
  - DuckDB SQL is very close to PostgreSQL, so dPostgreSQL is used as
    TSqlDBDefinition (LIMIT syntax, DDL types, information_schema metadata)
  - a connection has a server-side lease, which is renewed by every request:
    an idle connection is checked and re-created if needed by the next request
    (unless a transaction was active, which will raise an exception)
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
  mormot.core.log,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb,
  mormot.db.raw.quack;


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
    function BuildSql(Row: integer): RawUtf8;
    procedure AddParam(W: TJsonWriter; Index, Row: integer);
    procedure ExecuteArray;
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
  POW10: array[0..18] of Int64 = (1, 10, 100, 1000, 10000, 100000, 1000000,
    10000000, 100000000, 1000000000, 10000000000, 100000000000,
    1000000000000, 10000000000000, 100000000000000, 1000000000000000,
    10000000000000000, 100000000000000000, 1000000000000000000);

function IntegralAt(TypeId: TDuckDBType; D: PAnsiChar): Int64;
begin
  case TypeId of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_UTINYINT:
      result := PByte(D)^;
    DUCKDB_TYPE_TINYINT:
      result := PShortInt(D)^;
    DUCKDB_TYPE_SMALLINT:
      result := QuackInt16(D);
    DUCKDB_TYPE_USMALLINT:
      result := word(QuackInt16(D));
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_DATE:
      result := QuackInt32(D);
    DUCKDB_TYPE_UINTEGER:
      result := cardinal(QuackInt32(D));
  else // BIGINT, UBIGINT and the 64-bit date/time kinds
    result := QuackInt64(D);
  end;
end;

procedure HugeAt(D: PAnsiChar; out H: TDuckDBHugeInt);
begin
  H.Lower := QuackInt64(D);
  H.Upper := QuackInt64(D + 8);
end;

procedure DecimalAt(T: TQuackType; D: PAnsiChar; out Mantissa: TDuckDBHugeInt);
var
  v: Int64;
begin
  case T.ElemSize of
    2:
      v := QuackInt16(D);
    4:
      v := QuackInt32(D);
    8:
      v := QuackInt64(D);
  else
    begin
      HugeAt(D, Mantissa);
      exit;
    end;
  end;
  Mantissa.Lower := v;
  if v < 0 then
    Mantissa.Upper := -1
  else
    Mantissa.Upper := 0;
end;

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

function TimestampMicros(TypeId: TDuckDBType; Value: Int64): Int64;
begin
  case TypeId of
    DUCKDB_TYPE_TIMESTAMP_S:
      result := Value * 1000000;
    DUCKDB_TYPE_TIMESTAMP_MS:
      result := Value * 1000;
    DUCKDB_TYPE_TIMESTAMP_NS:
      result := Value div 1000;
  else
    result := Value;
  end;
end;

procedure QuackValueToText(V: TQuackVector; Row: PtrInt; var Result: RawUtf8);
var
  i: PtrInt;
  t: TQuackType;
  d: PAnsiChar;
  p: PUtf8Char;
  len, offs: integer;
  h: TDuckDBHugeInt;
  iv: TDuckDBInterval;
  us: Int64;
  s: single;
  dbl: double;
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
      begin
        d := V.DataAt(i);
        case t.DuckType of
          DUCKDB_TYPE_BOOLEAN:
            if d^ <> #0 then
              Result := 'true'
            else
              Result := 'false';
          DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT, DUCKDB_TYPE_INTEGER,
          DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT,
          DUCKDB_TYPE_UINTEGER:
            Int64ToUtf8(IntegralAt(t.DuckType, d), Result);
          DUCKDB_TYPE_UBIGINT:
            UInt64ToUtf8(QWord(QuackInt64(d)), Result);
          DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
            begin
              HugeAt(d, h);
              DuckHugeIntToText(h, t.DuckType = DUCKDB_TYPE_HUGEINT, Result);
            end;
          DUCKDB_TYPE_DECIMAL:
            begin
              DecimalAt(t, d, h);
              DuckHugeIntToText(h, true, Result, t.Scale);
            end;
          DUCKDB_TYPE_FLOAT:
            begin
              MoveFast(d^, s, SizeOf(s));
              dbl := s;
              DoubleToStr(dbl, Result);
            end;
          DUCKDB_TYPE_DOUBLE:
            DoubleToStr(QuackDouble(d), Result);
          DUCKDB_TYPE_UUID:
            begin
              HugeAt(d, h);
              DuckUuidToText(h, Result);
            end;
          DUCKDB_TYPE_DATE:
            DuckDateToText(QuackInt32(d), Result);
          DUCKDB_TYPE_TIME:
            DuckTimeToText(QuackInt64(d), Result);
          DUCKDB_TYPE_TIME_NS:
            DuckTimeToText(QuackInt64(d) div 1000, Result);
          DUCKDB_TYPE_TIME_TZ:
            begin
              DuckTimeTzDecode(QuackInt64(d), us, offs);
              DuckTimeToText(us, Result);
              if offs < 0 then
                Result := Result + '-'
              else
                Result := Result + '+';
              offs := abs(offs);
              AppendShortToUtf8(UInt2DigitsToShortFast(offs div 3600), Result);
              if offs mod 3600 <> 0 then
              begin
                Result := Result + ':';
                AppendShortToUtf8(
                  UInt2DigitsToShortFast((offs div 60) mod 60), Result);
              end;
            end;
          DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S,
          DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
            DuckTimestampToText(
              TimestampMicros(t.DuckType, QuackInt64(d)), Result);
          DUCKDB_TYPE_TIMESTAMP_TZ:
            begin
              DuckTimestampToText(QuackInt64(d), Result);
              Result := Result + '+00'; // stored as UTC
            end;
          DUCKDB_TYPE_INTERVAL:
            begin
              iv.Months := QuackInt32(d);
              iv.Days := QuackInt32(d + 4);
              iv.Micros := QuackInt64(d + 8);
              DuckIntervalToText(iv, Result);
            end;
          DUCKDB_TYPE_ENUM:
            begin
              us := EnumIndexAt(t, d);
              if us < length(t.EnumValues) then
                Result := t.EnumValues[us];
            end;
        end; // SQLNULL -> ''
      end;
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
  d: PAnsiChar;
  p: PUtf8Char;
  len: integer;
  s: single;
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
      begin
        d := V.DataAt(i);
        case t.DuckType of
          DUCKDB_TYPE_BOOLEAN:
            W.Add(d^ <> #0);
          DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT, DUCKDB_TYPE_INTEGER,
          DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT,
          DUCKDB_TYPE_UINTEGER:
            W.Add(IntegralAt(t.DuckType, d));
          DUCKDB_TYPE_FLOAT:
            begin
              MoveFast(d^, s, SizeOf(s));
              W.AddSingle(s);
            end;
          DUCKDB_TYPE_DOUBLE:
            W.AddDouble(QuackDouble(d));
          DUCKDB_TYPE_UBIGINT, DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT,
          DUCKDB_TYPE_DECIMAL:
            begin
              QuackValueToText(V, Row, txt);
              W.AddNoJsonEscape(pointer(txt), length(txt));
            end;
          DUCKDB_TYPE_SQLNULL:
            W.AddNull;
        else
          begin // ENUM, UUID, dates, INTERVAL -> JSON string
            QuackValueToText(V, Row, txt);
            W.Add('"');
            W.AddJsonEscape(pointer(txt), length(txt));
            W.Add('"');
          end;
        end;
      end;
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

procedure TSqlDBQuackStatement.AddParam(W: TJsonWriter; Index, Row: integer);
var
  p: PSqlDBParam;
  bits: Int64;
  v: RawUtf8;
begin
  p := @fParams[Index];
  if (Row >= 0) and
     (p^.VArray <> nil) then
  begin
    // array values are already stored as SQL literals
    if Row >= length(p^.VArray) then
      W.AddShort('null')
    else
    begin
      v := p^.VArray[Row];
      if p^.VType = ftBlob then
        AddBlobLiteral(W, pointer(v), length(v))
      else
        W.AddNoJsonEscape(pointer(v), length(v));
    end;
    exit;
  end;
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

function TSqlDBQuackStatement.BuildSql(Row: integer): RawUtf8;
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
      AddParam(W, n, Row);
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

procedure TSqlDBQuackStatement.ExecuteArray;
var
  row: integer;
  intrans: boolean;
  conn: TSqlDBQuackConnection;
begin
  // run the statement once per row - in an implicit transaction
  conn := TSqlDBQuackConnection(fConnection);
  intrans := conn.TransactionCount > 0;
  if not intrans then
    conn.ExecuteScript('BEGIN TRANSACTION');
  try
    for row := 0 to fParamsArrayCount - 1 do
      inc(fUpdateCount, conn.ExecuteScript(BuildSql(row)));
    if not intrans then
      conn.ExecuteScript('COMMIT');
  except
    if not intrans then
      conn.ExecuteScript('ROLLBACK');
    raise;
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
  begin
    // BindArray() was used: execute once per row (e.g. mormot.db.proxy batch)
    try
      ExecuteArray;
    finally
      SqlLogEnd;
    end;
    exit;
  end;
  conn := TSqlDBQuackConnection(fConnection);
  sql := BuildSql(-1);
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
  d: PAnsiChar;
  h: TDuckDBHugeInt;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if v.VType.Storage <> qsFixed then
  begin
    result := GetInt64(pointer(ColumnUtf8(Col)));
    exit;
  end;
  d := v.DataAt(i);
  case v.VType.DuckType of
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER, DUCKDB_TYPE_UBIGINT,
    DUCKDB_TYPE_DATE, DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIMESTAMP,
    DUCKDB_TYPE_TIMESTAMP_TZ, DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
      result := IntegralAt(v.VType.DuckType, d);
    DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT:
      begin
        HugeAt(d, h);
        if not DuckHugeIntIsInt64(h, v.VType.DuckType = DUCKDB_TYPE_HUGEINT) then
          ESqlDBQuack.RaiseUtf8('%.ColumnInt(%): 128-bit overflow',
            [self, Col]);
        result := h.Lower;
      end;
    DUCKDB_TYPE_DECIMAL:
      if v.VType.ElemSize = 16 then
        result := GetInt64(pointer(ColumnUtf8(Col))) // integer part
      else
      begin
        DecimalAt(v.VType, d, h);
        result := h.Lower div POW10[v.VType.Scale];
      end;
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      result := trunc(ColumnDouble(Col));
  else
    result := GetInt64(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBQuackStatement.ColumnDouble(Col: integer): double;
var
  v: TQuackVector;
  i: PtrInt;
  d: PAnsiChar;
  h: TDuckDBHugeInt;
  s: single;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if v.VType.Storage <> qsFixed then
  begin
    result := GetExtended(pointer(ColumnUtf8(Col)));
    exit;
  end;
  d := v.DataAt(i);
  case v.VType.DuckType of
    DUCKDB_TYPE_FLOAT:
      begin
        MoveFast(d^, s, SizeOf(s));
        result := s;
      end;
    DUCKDB_TYPE_DOUBLE:
      result := QuackDouble(d);
    DUCKDB_TYPE_DECIMAL:
      if v.VType.ElemSize = 16 then
        result := GetExtended(pointer(ColumnUtf8(Col)))
      else
      begin
        DecimalAt(v.VType, d, h);
        result := h.Lower / POW10[v.VType.Scale];
      end;
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      result := IntegralAt(v.VType.DuckType, d);
  else
    result := GetExtended(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBQuackStatement.ColumnCurrency(Col: integer): currency;
var
  v: TQuackVector;
  i: PtrInt;
  d: PAnsiChar;
  h: TDuckDBHugeInt;
  c: Int64;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if v.VType.Storage <> qsFixed then
  begin
    PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
    exit;
  end;
  d := v.DataAt(i);
  case v.VType.DuckType of
    DUCKDB_TYPE_DECIMAL:
      if (v.VType.ElemSize < 16) and
         (v.VType.Scale <= 4) then
      begin
        DecimalAt(v.VType, d, h);
        c := h.Lower * POW10[4 - v.VType.Scale];
        result := PCurrency(@c)^;
      end
      else
        PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
    DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
      result := ColumnDouble(Col);
    DUCKDB_TYPE_BOOLEAN, DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT,
    DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT, DUCKDB_TYPE_UTINYINT,
    DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
      result := IntegralAt(v.VType.DuckType, d);
  else
    PInt64(@result)^ := StrToCurr64(pointer(ColumnUtf8(Col)));
  end;
end;

function TSqlDBQuackStatement.ColumnDateTime(Col: integer): TDateTime;
var
  v: TQuackVector;
  i: PtrInt;
  d: PAnsiChar;
  us: Int64;
  offs: integer;
begin
  v := CheckCol(Col);
  result := 0;
  i := v.Stored(fChunkRow);
  if not v.IsStoredValid(i) then
    exit;
  if v.VType.Storage <> qsFixed then
  begin
    result := Iso8601ToDateTime(ColumnUtf8(Col));
    exit;
  end;
  d := v.DataAt(i);
  case v.VType.DuckType of
    DUCKDB_TYPE_DATE:
      result := QuackInt32(d) + DUCKDB_UNIXDATE_DELTA;
    DUCKDB_TYPE_TIME:
      result := QuackInt64(d) / DUCKDB_MICROSPERDAY;
    DUCKDB_TYPE_TIME_NS:
      result := (QuackInt64(d) div 1000) / DUCKDB_MICROSPERDAY;
    DUCKDB_TYPE_TIME_TZ:
      begin
        DuckTimeTzDecode(QuackInt64(d), us, offs);
        result := us / DUCKDB_MICROSPERDAY;
      end;
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_TZ, DUCKDB_TYPE_TIMESTAMP_S,
    DUCKDB_TYPE_TIMESTAMP_MS, DUCKDB_TYPE_TIMESTAMP_NS:
      result := DuckTimestampToDateTime(
        TimestampMicros(v.VType.DuckType, QuackInt64(d)));
  else
    result := Iso8601ToDateTime(ColumnUtf8(Col));
  end;
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
  TSqlDBQuackConnectionProperties.RegisterClassNameForDefinition;

end.
