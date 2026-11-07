/// regression tests for the DuckDB SynDB provider
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// - tests are skipped if the DuckDB library (duckdb.dll/libduckdb.so) is not
// available on this system
unit test.db.duckdb;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb,
  mormot.db.sql.duckdb,
  mormot.net.http,
  mormot.net.server;

type
  /// regression tests for mormot.db.raw.duckdb.pas and mormot.db.sql.duckdb.pas
  TTestDuckDB = class(TSynTestCase)
  protected
    fProps: TSqlDBDuckDBConnectionProperties;
    function Prepare: boolean; override;
    procedure CleanUp; override;
  published
    procedure BasicTypes;
    procedure ParamsAndCrud;
    procedure Transactions;
    procedure NestedTypes;
    procedure Json;
    procedure Errors;
    procedure Variants;
    procedure Metadata;
    procedure Bulk;
    procedure BatchInsert;
    procedure Appender;
  end;

  /// regression tests for the Quack client of mormot.db.raw.duckdb.pas and
  // mormot.db.sql.duckdb.pas
  TTestQuack = class(TSynTestCase)
  protected
    fMock: THttpServer;
    fQueries: TRawUtf8DynArray;
    fFetches: integer;
    fCancels: integer;
    fDisconnects: integer;
    fMockError: RawUtf8;
    fMockVersion: cardinal;
    procedure ClientTest(MockVersion: cardinal);
    procedure CompareToEmbedded(Embedded, Quack: TSqlDBConnectionProperties;
      const Sql: RawUtf8);
    function OnMockRequest(Ctxt: THttpServerRequestAbstract): cardinal;
    function MockRequest(Ctxt: THttpServerRequestAbstract): cardinal;
    function MockQuery(const Sql: RawUtf8): RawByteString;
  public
    procedure CleanUp; override;
  published
    /// the URI parsing and the requests encoding
    procedure Codec;
    /// the vectors decoding, including the compressed ones
    procedure Vectors;
    /// the TSqlDB* classes using a mock Quack server of the protocol version 1
    procedure ClientVersion1;
    /// the TSqlDB* classes using a mock Quack server of the protocol version 3
    procedure ClientVersion3;
    {$ifdef CPU64}
    /// the TSqlDB* classes using a real DuckDB Quack server
    procedure DuckDBServer;
    {$endif CPU64}
  end;



implementation

{ TTestDuckDB }

function TTestDuckDB.Prepare: boolean;
begin
  result := false;
  {$ifdef CPU64}
  // our tests require a local 64-bit DuckDB library within the main executable
  if not FileExists(Executable.ProgramFilePath + DuckDBLibraryName) then
    exit;
  // try to initialize this instance
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  try
    if DuckDBLibraryAvailable then
      fProps := TSqlDBDuckDBConnectionProperties.Create(':memory:', '', '', '')
  finally
    TSynLog.Family.ExceptionIgnoreCurrentThread := false;
  end;
  result := fProps <> nil;
  if result then
    fIdent := MakeString(['Duck DB (', DuckDB.LibraryName, ' ', DuckDB.Version, ')'])
  else
    NotifyProgress(['DuckDB library failed to load - tests skipped']);
  {$endif CPU64}
end;

procedure TTestDuckDB.CleanUp;
begin
  FreeAndNil(fProps);
end;

procedure TTestDuckDB.BasicTypes;
var
  rows: ISqlDBRows;
  dt: TDateTime;
  h: TDuckDBHugeInt;
  buf: TTemp47;
  n: PtrInt;

  procedure Test(const Expected: RawUtf8);
  var
    actual: RawUtf8;
  begin
    FastSetString(actual, @buf[0], n);
    CheckEqual(actual, Expected);
    Check(buf[n] = #0, 'Missing buffer terminator');
    FillCharFast(buf, SizeOf(buf), 255);
  end;

begin
  CheckEqual(SizeOf(TDuckDBStringT),   16, 'SizeOf(TDuckDBStringT)');
  CheckEqual(SizeOf(TDuckDBHugeInt),   16, 'SizeOf(TDuckDBHugeInt)');
  CheckEqual(SizeOf(TDuckDBInterval),  16, 'SizeOf(TDuckDBInterval)');
  CheckEqual(SizeOf(TDuckDBListEntry), 16, 'SizeOf(TDuckDBListEntry)');
  CheckEqual(SizeOf(TDuckDBResult),
    3 * SizeOf(TDuckIdx) + 3 * SizeOf(pointer), 'SizeOf(TDuckDBResult)');
  FillCharFast(buf, SizeOf(buf), 255);
  h.Lower := 12345;
  h.Upper := 0;
  n := DuckHugeIntToTextBuf(h, true, 0, buf);
  Test('12345');
  n := DuckHugeIntToTextBuf(h, true, 2, buf);
  Test('123.45');
  n := DuckHugeIntToTextBuf(h, true, 5, buf);
  Test('0.12345');
  n := DuckHugeIntToTextBuf(h, true, 7, buf);
  Test('0.0012345');
  h.Lower := -12345;
  h.Upper := -1;
  n := DuckHugeIntToTextBuf(h, true, 2, buf);
  Test('-123.45');
  h.Lower := 0;
  h.Upper := 0;
  n := DuckHugeIntToTextBuf(h, true, 4, buf);
  Test('0.0000');
  h.Lower := -1;
  h.Upper := -1;
  n := DuckHugeIntToTextBuf(h, false, 0, buf);
  Test('340282366920938463463374607431768211455');
  n := DuckHugeIntToTextBuf(h, true, 0, buf);
  Test('-1');
  h.Lower := 0;
  h.Upper := low(Int64);
  n := DuckHugeIntToTextBuf(h, true, 0, buf);
  Test('-170141183460469231731687303715884105728');
  h.Upper := high(Int64);
  h.Lower := -1;
  n := DuckHugeIntToTextBuf(h, true, 0, buf);
  Test('170141183460469231731687303715884105727');
  h.Lower := low(Int64);
  h.Upper := -1;
  n := DuckHugeIntToTextBuf(h, true, 0, buf);
  Test('-9223372036854775808');
  // UUID representation with DuckDB's highest-bit flip
  h.Upper := Int64($8011223344556677);
  h.Lower := Int64($8899AABBCCDDEEFF);
  n := DuckUuidToTextBuf(h, buf);
  Test('00112233-4455-6677-8899-aabbccddeeff');
  if fProps = nil then
    exit;
  rows := fProps.Execute(
    'select 42::INTEGER as i, 9007199254740993::BIGINT as b, ' +
    'true as t, 1.5::DOUBLE as d, 1234.5678::DECIMAL(18,4) as c, ' +
    '123456789012.3456::DECIMAL(38,4) as hc, ' +
    'decode(from_hex(''68c3a46c6c6f2077c3b6726c642c2061206c6f6e67657220737472696e67203e203132206368617273'')) as s, ''short'' as ss, ' +
    'DATE ''2024-02-29'' as dd, TIMESTAMP ''2024-02-29 13:14:15.678'' as ts, ' +
    'TIME ''23:59:58'' as tt, NULL::INTEGER as n, ''\xAA\x00\x5C\xFF''::BLOB as bl, ' +
    'sum(x)::HUGEINT as h, ''8f14e45f-ceea-467a-a5f0-6e0bd3f1d2a1''::UUID as u ' +
    'from (values (1),(2)) v(x)', []);
  Check(rows.Step, 'step');
  CheckEqual(rows.ColumnInt(0), 42, 'integer');
  CheckEqual(rows.ColumnInt(1), 9007199254740993, 'bigint');
  CheckEqual(rows.ColumnInt(2), 1, 'boolean');
  Check(rows.ColumnDouble(3) = 1.5, 'double');
  Check(rows.ColumnCurrency(4) = 1234.5678, 'decimal(18,4) currency');
  Check(rows.ColumnType(4) = ftCurrency, 'decimal(18,4) type');
  Check(rows.ColumnCurrency(5) = 123456789012.3456, 'decimal(38,4) currency');
  CheckEqual(rows.ColumnUtf8(5), '123456789012.3456', 'decimal(38,4) text');
  CheckEqual(rows.ColumnUtf8(6), RawUtf8(HexToBin('68c3a46c6c6f2077c3b6726c642c2061206c6f6e67657220737472696e67203e203132206368617273')), 'varchar');
  CheckEqual(rows.ColumnUtf8(7), 'short', 'inlined varchar');
  dt := rows.ColumnDateTime(8);
  CheckEqual(DateToIso8601Text(dt), '2024-02-29', 'date');
  CheckEqual(rows.ColumnUtf8(8), '2024-02-29', 'date text');
  dt := rows.ColumnDateTime(9);
  CheckEqual(DateTimeToIso8601(dt, true, 'T', true), '2024-02-29T13:14:15.678',
    'timestamp');
  Check(rows.ColumnType(9) = ftDate, 'timestamp type');
  CheckEqual(TimeToIso8601(rows.ColumnDateTime(10), true), 'T23:59:58', 'time');
  Check(rows.ColumnNull(11), 'null');
  Check(not rows.ColumnNull(0), 'not null');
  CheckEqual(BinToHex(rows.ColumnBlob(12)), 'AA005CFF', 'blob');
  CheckEqual(rows.ColumnInt(13), 3, 'hugeint');
  CheckEqual(rows.ColumnUtf8(14), '8f14e45f-ceea-467a-a5f0-6e0bd3f1d2a1', 'uuid');
  Check(not rows.Step, 'eof');
  rows := nil;
  if DuckDB.Version < 'v2' then
    exit; // types below were introduced in DuckDB 1.5 / 2.0
  rows := fProps.Execute(
    'select ''2001-02-03 04:05:06.123456789+00''::TIMESTAMPTZ_NS as tzns, ' +
    '''POINT(1 2)''::GEOMETRY as g, 42::VARIANT as v', []);
  Check(rows.Step, 'step2');
  Check(rows.ColumnType(0) = ftDate, 'timestamptz_ns type');
  CheckEqual(DateTimeToIso8601(rows.ColumnDateTime(0), true, 'T', true),
    '2001-02-03T04:05:06.123', 'timestamptz_ns');
  CheckEqual(rows.ColumnUtf8(0), '2001-02-03 04:05:06.123456+00',
    'timestamptz_ns text');
  Check(rows.ColumnType(1) = ftBlob, 'geometry type');
  CheckEqual(BinToHex(rows.ColumnBlob(1)),
    '0101000000000000000000F03F0000000000000040', 'geometry as wkb');
  CheckEqual(rows.ColumnUtf8(2), '42', 'variant as json');
end;

procedure TTestDuckDB.ParamsAndCrud;
var
  conn: TSqlDBConnection;
  st: ISqlDBStatement;
  rows: ISqlDBRows;
  i: integer;
  blob: RawByteString;
  dt: TDateTime;
begin
  if fProps = nil then
    exit;
  conn := fProps.ThreadSafeConnection;
  fProps.ExecuteNoResult('drop table if exists people', []);
  fProps.ExecuteNoResult('create table people (id INTEGER primary key, ' +
    'name VARCHAR, amount DECIMAL(19,4), born DATE, seen TIMESTAMP, ' +
    'ratio DOUBLE, photo BLOB)', []);
  st := conn.NewStatementPrepared(
    'insert into people values (?,?,?,?,?,?,?)', false, true);
  blob := HexToBin('000102275C78FF'); // binary with quote, backslash and #255
  dt := EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 789);
  for i := 1 to 10 do
  begin
    st.Bind(1, i);
    st.BindTextU(2, FormatUtf8('O''Neil ? -- #%', [i]));
    st.BindCurrency(3, i * 1000 + 0.1234);
    st.BindDateTime(4, EncodeDate(2000, 1, i));
    st.BindDateTime(5, dt);
    st.Bind(6, i / 4);
    if i = 3 then
      st.BindNull(7)
    else
      st.BindBlob(7, blob);
    st.ExecutePrepared;
    CheckEqual(st.UpdateCount, 1, 'insert updatecount');
    st.Instance.Reset; // mandatory between executions for mormot.db.proxy clients
  end;
  rows := fProps.Execute('select count(*), sum(amount) from people', []);
  Check(rows.Step, 'count step');
  CheckEqual(rows.ColumnInt(0), 10, 'count');
  Check(rows.ColumnCurrency(1) = 55001.234, 'sum(decimal)');
  rows := nil;
  rows := fProps.Execute('select * from people where id = ?', [7]);
  Check(rows.Step, 'select by id');
  CheckEqual(rows.ColumnUtf8(1), 'O''Neil ? -- #7', 'quoted text param');
  Check(rows.ColumnCurrency(2) = 7000.1234, 'currency param');
  CheckEqual(DateToIso8601Text(rows.ColumnDateTime(3)), '2000-01-07', 'date param');
  CheckEqual(DateTimeToIso8601(rows.ColumnDateTime(4), true, 'T', true),
    '2001-02-03T04:05:06.789', 'timestamp param');
  Check(rows.ColumnDouble(5) = 1.75, 'double param');
  Check(rows.ColumnBlob(6) = blob, 'blob param');
  rows := nil;
  rows := fProps.Execute('select photo from people where id = ?', [3]);
  Check(rows.Step and rows.ColumnNull(0), 'null param');
  rows := nil;
  CheckEqual(fProps.ExecuteNoResult(
    'update people set ratio = ratio * 2 where id > ?', [5]), 5, 'update count');
  CheckEqual(fProps.ExecuteNoResult(
    'delete from people where id in (?, ?)', [9, 10]), 2, 'delete count');
  rows := fProps.Execute('select count(*) from people', []);
  Check(rows.Step and (rows.ColumnInt(0) = 8), 'count after delete');
end;

procedure TTestDuckDB.Transactions;
var
  conn: TSqlDBConnection;
  rows: ISqlDBRows;
begin
  if fProps = nil then
    exit;
  conn := fProps.ThreadSafeConnection;
  fProps.ExecuteNoResult('create or replace table tx (id INTEGER)', []);
  conn.StartTransaction;
  fProps.ExecuteNoResult('insert into tx values (?)', [1]);
  fProps.ExecuteNoResult('insert into tx values (?)', [2]);
  rows := fProps.Execute('select count(*) from tx', []);
  Check(rows.Step and (rows.ColumnInt(0) = 2), 'read own writes in transaction');
  rows := nil;
  conn.Rollback;
  rows := fProps.Execute('select count(*) from tx', []);
  Check(rows.Step and (rows.ColumnInt(0) = 0), 'rollback');
  rows := nil;
  conn.StartTransaction;
  fProps.ExecuteNoResult('insert into tx values (?)', [3]);
  fProps.ExecuteNoResult('insert into tx values (?)', [4]);
  conn.Commit;
  rows := fProps.Execute('select sum(id) from tx', []);
  Check(rows.Step and (rows.ColumnInt(0) = 7), 'commit');
  rows := nil;
  // failing commit (constraint) keeps data unchanged
  fProps.ExecuteNoResult('create or replace table txu (id INTEGER primary key)', []);
  conn.StartTransaction;
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  try
    fProps.ExecuteNoResult('insert into txu values (?)', [1]);
    fProps.ExecuteNoResult('insert into txu values (?)', [1]); // duplicate
    conn.Commit;
    Check(false, 'duplicate key should fail');
  except
    on E: Exception do
    begin
      Check(true, 'duplicate key raised');
      if conn.TransactionCount > 0 then
        conn.Rollback;
    end;
  end;
  TSynLog.Family.ExceptionIgnoreCurrentThread := false;
  rows := fProps.Execute('select count(*) from txu', []);
  Check(rows.Step and (rows.ColumnInt(0) = 0), 'atomic failure');
end;

procedure TTestDuckDB.NestedTypes;
var
  rows: ISqlDBRows;
begin
  if fProps = nil then
    exit;
  rows := fProps.Execute('select [1, 2, NULL] as l, {''a'': 1, ''b'': ''x''} as s, ' +
    'MAP {''k1'': 10, ''k2'': 20} as m, ''sad''::ENUM(''sad'',''happy'') as e, ' +
    'INTERVAL 3 DAY as iv, [[1,2],[3]] as ll', []);
  Check(rows.Step, 'nested step');
  CheckEqual(rows.ColumnUtf8(0), '[1,2,null]', 'list');
  CheckEqual(rows.ColumnUtf8(1), '{"a":1,"b":"x"}', 'struct');
  CheckEqual(rows.ColumnUtf8(2), '{"k1":10,"k2":20}', 'map');
  CheckEqual(rows.ColumnUtf8(3), 'sad', 'enum');
  CheckEqual(rows.ColumnUtf8(4), '3 days', 'interval');
  CheckEqual(rows.ColumnUtf8(5), '[[1,2],[3]]', 'list of list');
end;

procedure TTestDuckDB.Json;
var
  json: RawUtf8;
  doc: TDocVariantData;
begin
  if fProps = nil then
    exit;
  with fProps.ThreadSafeConnection.NewStatementPrepared(
    'select id, name, amount, born, [id, id*2] as arr from people where id < ? order by id',
    true, true) do
  begin
    Bind(1, 3);
    ExecutePreparedAndFetchAllAsJson(true, json);
  end;
  doc.InitJson(json, JSON_FAST);
  if CheckEqual(doc.Count, 2, 'json rows') then
  begin
    CheckEqual(_Safe(doc.Values[1])^.U['name'], 'O''Neil ? -- #2', 'json text');
    CheckEqual(_Safe(doc.Values[0])^.U['born'], '2000-01-01', 'json date');
    CheckEqual(_Safe(doc.Values[1])^.U['amount'], '2000.1234', 'json currency');
    CheckEqual(_Safe(doc.Values[1])^.U['arr'], '[2,4]', 'json nested');
  end;
end;

procedure TTestDuckDB.Errors;
var
  raised: boolean;
begin
  if fProps = nil then
    exit;
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  raised := false;
  try
    fProps.Execute('select * from table_does_not_exist', []);
  except
    on E: ESqlDBException do
      raised := Pos('table_does_not_exist', E.Message) > 0;
  end;
  TSynLog.Family.ExceptionIgnoreCurrentThread := false;
  Check(raised, 'missing table error');
  // connection is still usable after an error
  with fProps.Execute('select 1', []) do
    Check(Step and (ColumnInt(0) = 1), 'usable after error');
end;

procedure TTestDuckDB.Variants;
var
  rows: ISqlDBRows;
begin
  // DuckDB 1.5+ VARIANT columns are retrieved as JSON text
  if (fProps = nil) or
     (PosEx('v1.4', fProps.LibraryVersion) = 1) then
    exit;
  fProps.ExecuteNoResult('drop table if exists events', []);
  fProps.ExecuteNoResult('create table events (id integer, name varchar, ' +
    'payload variant)', []);
  fProps.ExecuteNoResult('insert into events values ' +
    '(1, ''obj'', {''a'': 1, ''b'': [1, 2]}::variant), ' +
    '(2, ''txt'', ''hello''::variant), (3, ''num'', 42::variant), ' +
    '(4, ''nul'', null::variant)', []);
  rows := fProps.Execute('select * from events where id > ? order by id', [0]);
  Check(rows.Step);
  CheckEqual(rows.ColumnCount, 3);
  CheckEqual(rows.ColumnInt(0), 1);
  CheckEqual(rows.ColumnUtf8(1), 'obj');
  CheckEqual(rows.ColumnUtf8(2), '{"a":1,"b":[1,2]}', 'object');
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(2), '"hello"', 'text');
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(2), '42', 'number');
  Check(rows.Step);
  Check(rows.ColumnNull(2), 'variant null is a SQL NULL');
  Check(not rows.Step);
  rows := nil;
  // cached statement with other parameters
  rows := fProps.Execute('select id, payload from events where id = ?', [1]);
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(1), '{"a":1,"b":[1,2]}');
  rows := fProps.Execute('select id, payload from events where id = ?', [3]);
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(1), '42');
  rows := nil;
  // VARIANT within a nested type
  rows := fProps.Execute('select [payload] l from events where id = 2', []);
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(0), '["hello"]');
  rows := nil;
  // other columns are not affected
  rows := fProps.Execute('select id, name from events order by id', []);
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(1), 'obj');
  rows := nil;
  // duplicated names are supported
  rows := fProps.Execute('select payload, id, payload from events order by id', []);
  Check(rows.Step);
  CheckEqual(rows.ColumnUtf8(0), '{"a":1,"b":[1,2]}', 'dup1');
  CheckEqual(rows.ColumnUtf8(2), '{"a":1,"b":[1,2]}', 'dup2');
  rows := nil;
end;

procedure TTestDuckDB.Metadata;
var
  tables: TRawUtf8DynArray;
  fields: TSqlDBColumnDefineDynArray;
begin
  if fProps = nil then
    exit;
  fProps.GetTableNames(tables);
  Check(FindRawUtf8(tables, 'main.people', false) >= 0, 'GetTableNames');
  fProps.GetFields('people', fields);
  if CheckEqual(length(fields), 7, 'GetFields count') then
  begin
    CheckEqual(fields[0].ColumnName, 'id', 'field name');
    Check(fields[1].ColumnType = ftUtf8, 'field type text');
    Check(fields[5].ColumnType = ftDouble, 'field type double');
  end;
end;

procedure TTestDuckDB.Bulk;
const
  COUNT = 1000;
var
  conn: TSqlDBConnection;
  st: ISqlDBStatement;
  i: integer;
  rows: ISqlDBRows;
  n: integer;
  timer: TPrecisionTimer;
begin
  if fProps = nil then
    exit;
  conn := fProps.ThreadSafeConnection;
  fProps.ExecuteNoResult('create or replace table bulk (id INTEGER, v DOUBLE, t VARCHAR)', []);
  timer.Start;
  conn.StartTransaction;
  st := conn.NewStatementPrepared('insert into bulk values (?,?,?)', false, true);
  for i := 1 to COUNT do
  begin
    st.Bind(1, i);
    st.Bind(2, i * 0.5);
    st.BindTextU(3, Int32ToUtf8(i mod 7));
    st.ExecutePrepared;
    st.Instance.Reset;
  end;
  conn.Commit;
  NotifyTestSpeed('insert', COUNT, 0, @timer);
  timer.Start;
  rows := fProps.Execute('select t, count(*), sum(v) from bulk group by t order by t', []);
  n := 0;
  while rows.Step do
    inc(n);
  CheckEqual(n, 7, 'group by');
  NotifyTestSpeed('group by', COUNT, 0, @timer);
  timer.Start;
  rows := fProps.Execute('select * from bulk order by id', []);
  n := 0;
  while rows.Step do
    inc(n);
  CheckEqual(n, COUNT, 'read all rows (multiple chunks)');
  NotifyTestSpeed('read all', COUNT, 0, @timer);
end;

procedure TTestDuckDB.BatchInsert;
const
  COUNT = 1000;
var
  names: TRawUtf8DynArray;
  types: TSqlDBFieldTypeArray;
  values: TRawUtf8DynArrayDynArray;
  i: integer;
  rows: ISqlDBRows;
  timer: TPrecisionTimer;
begin
  // setup the new table
  if fProps = nil then
    exit;
  Check(Assigned(fProps.OnBatchInsert), 'OnBatchInsert');
  Check(fProps.BatchSendingAbilities = [cCreate], 'abilities');
  fProps.ExecuteNoResult('create or replace table arr (id INTEGER, ' +
    'name VARCHAR, amount DECIMAL(19,4), d TIMESTAMP, v DOUBLE)', []);
  // pre-computed inserted values
  SetLength(names, 5);
  names[0] := 'id';
  names[1] := 'name';
  names[2] := 'amount';
  names[3] := 'd';
  names[4] := 'v';
  types[0] := ftInt64;
  types[1] := ftUtf8;
  types[2] := ftCurrency;
  types[3] := ftDate;
  types[4] := ftDouble;
  SetLength(values, 5);
  for i := 0 to 4 do
    SetLength(values[i], COUNT);
  for i := 0 to COUNT - 1 do
  begin
    Int32ToUtf8(i + 1, values[0, i]);
    values[1, i] := QuotedStr(FormatUtf8('it''s #%', [i + 1]), '''');
    Make([i + 1, '.5'], values[2, i]);
    values[3, i] := QuotedStr(DateToIso8601Text(EncodeDate(2025, 1, 1) + i), '''');
    if i = 0 then
      values[4, i] := 'null'
    else
      values[4, i] := DoubleToStr(i / 8);
  end;
  // first pass uses the Appender
  timer.Start;
  // the ORM supplies all schema-qualified field names
  fProps.OnBatchInsert(fProps, 'main.arr', names, types, COUNT, values, []);
  NotifyTestSpeed('insert all', COUNT, 0, @timer);
  // second pass validates the multi-values INSERT fallback
  // (inserting only id and name for 100 more rows)
  SetLength(names, 2);
  SetLength(values, 2);
  for i := 0 to 100 - 1 do
    Int32ToUtf8(COUNT + i + 1, values[0, i]);
  timer.Start;
  // not all columns: the Appender is not usable -> regular INSERT
  fProps.OnBatchInsert(fProps, 'arr', names, types, 100, values, []);
  NotifyTestSpeed('insert two', 100, 0, @timer);
  // verify added values
  rows := fProps.Execute('select count(*), sum(amount), max(d), ' +
    'max(name) filter (where id = ?), sum(v), count(v) from arr', [1000]);
  Check(rows.Step, 'batch select');
  CheckEqual(rows.ColumnInt(0), COUNT + 100, 'batch count');
  Check(rows.ColumnCurrency(1) = 501000, 'batch sum(decimal)');
  CheckEqual(DateToIso8601Text(rows.ColumnDateTime(2)),
    DateToIso8601Text(EncodeDate(2025, 1, 1) + COUNT - 1), 'batch dates');
  CheckEqual(rows.ColumnUtf8(3), 'it''s #1000', 'batch quoted text');
  Check(rows.ColumnDouble(4) = 62437.5, 'batch doubles');
  CheckEqual(rows.ColumnInt(5), COUNT - 1, 'batch null');
end;

procedure TTestDuckDB.Appender;
const
  COUNT = 20000;
var
  app: TSqlDBDuckDBAppender;
  i: integer;
  rows: ISqlDBRows;
  timer: TPrecisionTimer;
begin
  if fProps = nil then
    exit;
  fProps.ExecuteNoResult('create or replace table app (id INTEGER, ' +
    'v DECIMAL(19,4), t VARCHAR, d TIMESTAMP)', []);
  timer.Start;
  app := TSqlDBDuckDBAppender.Create(
    fProps.ThreadSafeConnection as TSqlDBDuckDBConnection, 'app');
  try
    for i := 1 to COUNT do
    begin
      app.Add(i);
      app.AddCurrency(i + 0.1234);
      app.AddText('row');
      app.AddDateTime(EncodeDate(2024, 1, 1) + i / 1000);
      app.EndRow;
    end;
    app.AddRow([-1, 1.5, 'addrow', nil]);
  finally
    app.Free;
  end;
  NotifyTestSpeed('insert', COUNT, 0, @timer);
  rows := fProps.Execute('select count(*), sum(v) from app', []);
  Check(rows.Step);
  CheckEqual(rows.ColumnInt(0), 20001, 'appender count');
  CheckEqual(rows.ColumnUtf8(1), '200012469.5000', 'appender sum');
end;


{ ************ Mock Quack Server Messages Encoding }

// Quack mock protocol is always written to a binary stream.  No temporary
// RawByteString values, AnsiString concatenation or implicit codepage changes.
// Quack field ids are little endian; unsigned idx_t uses LEB128 (VarUInt64),
// and signed values use signed LEB128, not zigzag encoding.
// TBufferWriter.WriteVarUInt64 / WriteVarLeb64 do exactly what Quack needs.

type
  TMockQuackType = (
    mqtInteger, mqtVarchar, mqtBigint, mqtDecimal, mqtDate,
    mqtBoolean, mqtUuid, mqtEnum, mqtListInt, mqtStructAB,
    mqtDouble, mqtVariant, mqtTimestamp, mqtBlob);

  TMockQuackWriter = class(TBufferWriter)
  public
    // Own an internal TRawByteStringStream and its 4KB binary buffer.
    constructor Create; reintroduce;
    // FlushTo returns raw bytes without any code-page conversion.
    procedure Field(Id: cardinal);
    procedure EndObject;                     {$ifdef HASINLINE} inline; {$endif}
    procedure UInt(Value: QWord);            {$ifdef HASINLINE} inline; {$endif}
    procedure Signed(Value: Int64);          {$ifdef HASINLINE} inline; {$endif}
    procedure Text(const Value: RawUtf8);    {$ifdef HASINLINE} inline; {$endif}
    procedure BlobInt32(const Values: array of integer);
    procedure Header(MsgType: cardinal; const ConnectionId: RawUtf8);
    procedure WriteType(Typ: TMockQuackType);
    procedure BeginPrepare(const Types: array of TMockQuackType;
      const Names: array of RawUtf8; NeedsMore: boolean; Chunks: integer);
    procedure EndPrepare;
    procedure BeginChunk(Rows: integer; const Types: array of TMockQuackType;
      Wrapped: boolean);
    procedure EndChunk(Wrapped: boolean);
    procedure BeginVector(const Validity: array of boolean);
    procedure VectorInt32(const Values: array of integer;
      const Validity: array of boolean);
    procedure VectorInt64(const Values: array of Int64;
      const Validity: array of boolean);
    procedure VectorDouble(const Values: array of double;
      const Validity: array of boolean);
    procedure VectorBytes(const Values: array of byte;
      const Validity: array of boolean);
    procedure VectorText(const Values: array of RawUtf8;
      const Validity: array of boolean);
    procedure VectorBlobBytes(const Values: array of byte);
    procedure BigintChunk(First, Count: integer; Wrapped: boolean);
  end;

constructor TMockQuackWriter.Create;
begin
  inherited Create(TRawByteStringStream);
end;

procedure TMockQuackWriter.Field(Id: cardinal);
begin
  Write2(Id); // Quack uint16, little endian (as on supported x86 targets)
end;

procedure TMockQuackWriter.EndObject;
begin
  Field($ffff);
end;

procedure TMockQuackWriter.UInt(Value: QWord);
begin
  WriteVarUInt64(Value);  // unsigned LEB128: NOT WriteVarInt64 (zigzag)
end;

procedure TMockQuackWriter.Signed(Value: Int64);
begin
  WriteVarLeb64(Value);   // signed LEB128, as consumed by TFastReader.VarLeb64
end;

procedure TMockQuackWriter.Text(const Value: RawUtf8);
begin
  Write(Value); // has VarUInt32 length prefix
end;

procedure TMockQuackWriter.BlobInt32(const Values: array of integer);
begin
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values) * SizeOf(integer))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.Header(MsgType: cardinal; const ConnectionId: RawUtf8);
begin
  Field(1);
  UInt(MsgType);
  if ConnectionId <> '' then
  begin
    Field(2);
    Text(ConnectionId);
  end;
  Field(3);
  UInt(High(QWord)); // optional client_query_id = -1
  EndObject;
end;

procedure TMockQuackWriter.WriteType(Typ: TMockQuackType);
var
  id: cardinal;
begin
  case Typ of
    mqtDecimal:
      begin
        Field(100); UInt(QTYPE_DECIMAL);
        Field(101); Write1(1);
        Field(100); UInt(2); // LogicalTypeInfoType: DECIMAL
        Field(200); UInt(10);
        Field(201); UInt(2);
        EndObject;
        EndObject;
        exit;
      end;
    mqtListInt:
      begin
        Field(100); UInt(QTYPE_LIST);
        Field(101); Write1(1);
        Field(100); UInt(4); // LogicalTypeInfoType: LIST
        Field(200); WriteType(mqtInteger);
        EndObject;
        EndObject;
        exit;
      end;
    mqtStructAB:
      begin
        Field(100); UInt(QTYPE_STRUCT);
        Field(101); Write1(1);
        Field(100); UInt(5); // LogicalTypeInfoType: STRUCT
        Field(200); UInt(2);
        Field(0); Text('a'); Field(1); WriteType(mqtInteger); EndObject;
        Field(0); Text('b'); Field(1); WriteType(mqtVarchar); EndObject;
        EndObject;
        EndObject;
        exit;
      end;
    mqtEnum:
      begin
        Field(100); UInt(QTYPE_ENUM);
        Field(101); Write1(1);
        Field(100); UInt(6); // LogicalTypeInfoType: ENUM
        Field(200); UInt(2);
        Field(201); UInt(2);
        Text('red');
        Text('green');
        EndObject;
        EndObject;
        exit;
      end;
    mqtInteger:
      id := QTYPE_INTEGER;
    mqtVarchar:
      id := QTYPE_VARCHAR;
    mqtBigint:
      id := QTYPE_BIGINT;
    mqtDate:
      id := QTYPE_DATE;
    mqtBoolean:
      id := QTYPE_BOOLEAN;
    mqtUuid:
      id := QTYPE_UUID;
    mqtDouble:
      id := QTYPE_DOUBLE;
    mqtVariant:
      id := QTYPE_VARIANT;
    mqtTimestamp:
      id := QTYPE_TIMESTAMP;
    mqtBlob:
      id := QTYPE_BLOB;
  else
    exit;
  end;
  Field(100);
  UInt(id);
  EndObject;
end;

procedure TMockQuackWriter.BeginPrepare(
  const Types: array of TMockQuackType; const Names: array of RawUtf8;
  NeedsMore: boolean; Chunks: integer);
var
  i: integer;
begin
  Header(QUACK_PREPARE_RESPONSE, '');
  Field(1);
  UInt(length(Types));
  for i := 0 to high(Types) do
    WriteType(Types[i]);
  Field(2);
  UInt(length(Names));
  for i := 0 to high(Names) do
    Text(Names[i]);
  if NeedsMore then
  begin
    Field(3);
    Write1(1);
  end;
  if Chunks <> 0 then
  begin
    Field(4);
    UInt(Chunks);
  end;
end;

procedure TMockQuackWriter.EndPrepare;
begin
  Field(5);
  Signed(-1234567);
  UInt($123456789abcdef);
  EndObject;
end;

procedure TMockQuackWriter.BeginChunk(Rows: integer;
  const Types: array of TMockQuackType; Wrapped: boolean);
var
  i: integer;
begin
  if Wrapped then
  begin
    Write1(1); // non-null chunk wrapper
    Field(300);
  end;
  Field(100);
  UInt(Rows);
  Field(101);
  UInt(length(Types));
  for i := 0 to high(Types) do
    WriteType(Types[i]);
  Field(102);
  UInt(length(Types));
end;

procedure TMockQuackWriter.EndChunk(Wrapped: boolean);
begin
  EndObject; // chunk
  if Wrapped then
    EndObject; // nullable DataChunkWrapper
end;

procedure TMockQuackWriter.BeginVector(const Validity: array of boolean);
var
  i, n: integer;
  b: array of byte;
begin
  Field(100);
  if length(Validity) = 0 then
  begin
    Write1(0);
    exit;
  end;
  Write1(1);
  n := ((length(Validity) + 63) shr 6) * 8;
  SetLength(b, n);
  for i := 0 to high(Validity) do
    if Validity[i] then
      b[i shr 3] := b[i shr 3] or (1 shl (i and 7));
  Field(101);
  WriteVar(pointer(b), n);
end;

procedure TMockQuackWriter.VectorInt32(const Values: array of integer;
  const Validity: array of boolean);
begin
  BeginVector(Validity);
  Field(102);
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values) * SizeOf(integer))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.VectorInt64(const Values: array of Int64;
  const Validity: array of boolean);
begin
  BeginVector(Validity);
  Field(102);
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values) * SizeOf(Int64))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.VectorDouble(const Values: array of double;
  const Validity: array of boolean);
begin
  BeginVector(Validity);
  Field(102);
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values) * SizeOf(double))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.VectorBytes(const Values: array of byte;
  const Validity: array of boolean);
begin
  BeginVector(Validity);
  Field(102);
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.VectorText(const Values: array of RawUtf8;
  const Validity: array of boolean);
var
  i: integer;
  total: PtrInt;
begin
  BeginVector(Validity);
  total := 0;
  for i := 0 to high(Values) do
    inc(total, length(Values[i]));
  Field(107);
  UInt(total);
  Field(108);
  UInt(length(Values) * SizeOf(integer));
  for i := 0 to high(Values) do
  begin
    Write4(length(Values[i]));
  end;
  Field(109);
  UInt(total);
  for i := 0 to high(Values) do
    WriteBinary(Values[i]);
end;

procedure TMockQuackWriter.VectorBlobBytes(const Values: array of byte);
begin
  BeginVector([]);
  Field(107); UInt(length(Values));
  Field(108); UInt(4);
  Write4(length(Values)); // int32 byte length
  Field(109);
  if length(Values) <> 0 then
    WriteVar(@Values[0], length(Values))
  else
    WriteVar(nil, 0);
end;

procedure TMockQuackWriter.BigintChunk(First, Count: integer;
  Wrapped: boolean);
var
  values: array of Int64;
  i: integer;
begin
  SetLength(values, Count);
  for i := 0 to Count - 1 do
    values[i] := First + i;
  BeginChunk(Count, [mqtBigint], Wrapped);
  VectorInt64(values, []);
  EndObject; // vector object
  EndChunk(Wrapped);
end;

function MockErrorResponse(const ErrType, Msg: RawUtf8): RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_ERROR_RESPONSE, '');
    w.Field(1); w.Text(Msg);
    w.Field(2); w.Text(ErrType);
    w.EndObject;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockConnectionResponse(Version: cardinal): RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_CONNECTION_RESPONSE, 'CONN1');
    w.Field(1); w.Text('v1.5.0');
    w.Field(2); w.Text('mock');
    w.Field(3); w.UInt(Version);
    if Version = 3 then
    begin
      w.Field(4);
      w.UInt(60);
    end;
    w.EndObject;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockSuccessResponse: RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_SUCCESS_RESPONSE, '');
    w.EndObject;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockEmptyFetchResponseV1: RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_FETCH_RESPONSE, '');
    w.Field(2);
    w.UInt(High(QWord));
    w.EndObject;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockTruncatedPrepare: RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_PREPARE_RESPONSE, '');
    w.Field(1); // truncated body
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockIncompleteSuccess: RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_SUCCESS_RESPONSE, '');
    // Missing the body terminator on purpose.
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function MockFetchResponse(Version: cardinal; Batch, Fetches: integer): RawByteString;
var
  w: TMockQuackWriter;
begin
  w := TMockQuackWriter.Create;
  try
    w.Header(QUACK_FETCH_RESPONSE, '');
    if Version = 1 then
    begin
      if Fetches = 1 then
      begin
        w.Field(1); w.UInt(3);
        w.BigintChunk(4, 3, true);
        w.BigintChunk(7, 2, true);
        w.BigintChunk(9, 4, true);
        w.Field(2); w.UInt(1);
        w.EndObject;
      end
      else
      begin
        w.Field(2); w.UInt(High(QWord));
        w.EndObject;
      end;
    end
    else
    begin
      case Batch of
        1:
          begin
            w.Field(1); w.UInt(2); // two raw chunks
            w.Field(3); w.UInt(1);
            w.EndObject;
            w.BigintChunk(4, 3, false);
            w.BigintChunk(7, 2, false);
          end;
        2:
          begin
            w.Field(1); w.UInt(1);
            w.Field(3); w.UInt(2);
            w.EndObject;
            w.BigintChunk(9, 4, false);
          end;
      else
        w.Field(2); w.UInt(2); // terminal total batch count
        w.EndObject;
      end;
    end;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

{ TTestQuack }

procedure TTestQuack.CleanUp;
begin
  FreeAndNil(fMock);
  TSynLog.Family.ExceptionIgnore.Remove(ESqlDBQuack);
end;

function TTestQuack.MockQuery(const Sql: RawUtf8): RawByteString;
var
  w: TMockQuackWriter;
begin
  if copy(Sql, 1, 4) = 'fail' then
  begin
    result := MockErrorResponse('Catalog', 'Table does not exist!');
    exit;
  end;
  w := TMockQuackWriter.Create;
  try
    if Sql = 'select 1' then
    begin
      // three rows of INTEGER, VARCHAR, DOUBLE
      w.BeginPrepare([mqtInteger, mqtVarchar, mqtDouble],
        ['i', 's', 'd'], false, 1);
      w.BeginChunk(3, [mqtInteger, mqtVarchar, mqtDouble], true);
      w.VectorInt32([1, 0, 3], [true, false, true]); w.EndObject;
      w.VectorText(['a', '', 'ccc'], [true, false, true]); w.EndObject;
      w.VectorDouble([1.5, 2.5, 3.5], []); w.EndObject;
      w.EndChunk(true);
      w.EndPrepare;
    end
    else if Sql = 'select big' then
    begin
      w.BeginPrepare([mqtBigint], ['n'], true, 1);
      w.BigintChunk(1, 3, true);
      w.EndPrepare;
    end
    else if Sql = 'select variant' then
    begin
      w.BeginPrepare([mqtInteger, mqtVariant], ['id', 'v'], false, 0);
      w.EndPrepare;
    end
    else if Sql = 'SELECT #1 AS "id", NULLIF(#2::JSON, ''null''::JSON) AS "v" FROM (select variant)' then
    begin
      w.BeginPrepare([mqtInteger, mqtVarchar], ['id', 'v'], false, 1);
      w.BeginChunk(2, [mqtInteger, mqtVarchar], true);
      w.VectorInt32([1, 2], []); w.EndObject;
      w.VectorText(['{"a":[1,2]}', ''], [true, false]); w.EndObject;
      w.EndChunk(true);
      w.EndPrepare;
    end
    else if Sql = 'select flags' then
    begin
      // logical conversions and a BLOB containing $00/$FF
      w.BeginPrepare([mqtBoolean, mqtDecimal, mqtDate, mqtTimestamp,
        mqtListInt, mqtBlob], ['b', 'm', 'd', 't', 'l', 'x'], false, 1);
      w.BeginChunk(1, [mqtBoolean, mqtDecimal, mqtDate, mqtTimestamp,
        mqtListInt, mqtBlob], true);
      w.VectorBytes([1], []); w.EndObject;
      w.VectorInt64([12345], []); w.EndObject;
      w.VectorInt32([19000], []); w.EndObject;
      w.VectorInt64([981173106789000], []); w.EndObject;
      // one list [7,8] encoded with a flat child vector
      w.BeginVector([]);
      w.Field(104); w.UInt(2);
      w.Field(105); w.UInt(1);
      w.Field(100); w.UInt(0); w.Field(101); w.UInt(2); w.EndObject;
      w.Field(106); w.VectorInt32([7, 8], []); w.EndObject;
      w.EndObject; // list column
      w.VectorBlobBytes([1, 0, 255]); w.EndObject;
      w.EndChunk(true);
      w.EndPrepare;
    end
    else if copy(Sql, 1, 4) = 'echo' then
    begin
      w.BeginPrepare([mqtVarchar], ['sql'], false, 1);
      w.BeginChunk(1, [mqtVarchar], true);
      w.VectorText([Sql], []); w.EndObject;
      w.EndChunk(true);
      w.EndPrepare;
    end
    else if copy(Sql, 1, 6) = 'insert' then
    begin
      w.BeginPrepare([mqtBigint], ['Count'], false, 1);
      w.BigintChunk(7, 1, true);
      w.EndPrepare;
    end
    else
    begin
      w.BeginPrepare([mqtBigint], ['Count'], false, 1);
      w.BigintChunk(0, 1, true);
      w.EndPrepare;
    end;
    result := w.FlushTo;
  finally
    w.Free;
  end;
end;

function TTestQuack.OnMockRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  try
    result := MockRequest(Ctxt);
  except
    on E: Exception do
    begin
      fMockError := FormatUtf8('% %', [E, E.Message]);
      result := HTTP_SERVERERROR;
    end;
  end;
  TSynLog.Family.ExceptionIgnoreCurrentThread := false;
end;

function TTestQuack.MockRequest(Ctxt: THttpServerRequestAbstract): cardinal;
var
  r: TFastReader;
  mt, f, batch, minv, maxv, lease: cardinal;
  conn, sql, token: RawUtf8;
begin
  result := HTTP_SUCCESS;
  Ctxt.OutContentType := QUACK_CONTENT_TYPE;
  if Ctxt.Url <> QUACK_HTTP_PATH then
  begin
    result := HTTP_NOTFOUND;
    exit;
  end;
  // decode the request header
  r.Init(Ctxt.InContent);
  CheckEqual(r.Next2, 1, 'header type');
  mt := r.VarUInt32;
  repeat
    f := r.Next2;
    case f of
      2: conn := r.VarUtf8;
      3: r.VarUInt64;
    end;
  until f = $ffff;
  if mt <> QUACK_CONNECTION_REQUEST then
    if conn <> 'CONN1' then
    begin
      Ctxt.OutContent := MockErrorResponse('Invalid', 'Invalid connection id');
      exit;
    end;
  case mt of
    QUACK_CONNECTION_REQUEST:
      begin
        minv := 0;
        maxv := 0;
        lease := 0;
        repeat
          f := r.Next2;
          case f of
            1: token := r.VarUtf8;
            2, 3, 6: r.VarUtf8;
            4: minv := r.VarUInt32;
            5: maxv := r.VarUInt32;
            7: lease := r.VarUInt32;
          end;
        until f = $ffff;
        if (fMockVersion = 1) and (lease <> 0) then
          result := HTTP_SERVERERROR
        else if (minv > fMockVersion) or (maxv < fMockVersion) then
          Ctxt.OutContent := MockErrorResponse('InvalidInput', FormatUtf8(
            'Unsupported Quack version - server only supports version %',
            [fMockVersion]))
        else if (fMockVersion = 3) and (lease = 0) then
          Ctxt.OutContent := MockErrorResponse('InvalidInput',
            'heartbeat_timeout out of range')
        else if token <> 'secret' then
          Ctxt.OutContent := MockErrorResponse('Invalid', 'Authentication failed')
        else
          Ctxt.OutContent := MockConnectionResponse(fMockVersion);
      end;
    QUACK_PREPARE_REQUEST:
      begin
        CheckEqual(r.Next2, 1, 'sql field');
        sql := r.VarUtf8;
        AddRawUtf8(fQueries, sql);
        Ctxt.OutContent := MockQuery(sql);
      end;
    QUACK_FETCH_REQUEST:
      begin
        batch := 0;
        CheckEqual(r.Next2, 1, 'uuid field');
        r.VarUInt64; // upper
        r.VarUInt64; // lower
        repeat
          f := r.Next2;
          if f = 2 then
            batch := r.VarUInt32
          else if f = 3 then
            CheckEqual(r.VarUInt32, batch - 1, 'ack index');
        until f = $ffff;
        inc(fFetches);
        Ctxt.OutContent := MockFetchResponse(fMockVersion, batch, fFetches);
      end;
    QUACK_CANCEL_REQUEST:
      begin
        inc(fCancels);
        Ctxt.OutContent := MockSuccessResponse;
      end;
    QUACK_DISCONNECT_MESSAGE:
      begin
        inc(fDisconnects);
        Ctxt.OutContent := MockSuccessResponse;
      end;
  else
    Ctxt.OutContent := MockSuccessResponse;
  end;
end;

procedure TTestQuack.Codec;
var
  host: RawUtf8;
  port: integer;
  req: RawByteString;
  uuid: TDuckDBHugeInt;
  r: TFastReader;
  resp: TQuackResponse;
begin
  TSynLog.Family.ExceptionIgnore.Add(ESqlDBQuack);
  Check(QuackUriParse('quack:localhost', host, port));
  CheckEqual(host, 'localhost');
  CheckEqual(port, 9494);
  Check(QuackUriParse(' quack://db.example.org:1234 ', host, port));
  CheckEqual(host, 'db.example.org');
  CheckEqual(port, 1234);
  Check(QuackUriParse('quack:[::1]:99', host, port));
  CheckEqual(host, '::1');
  CheckEqual(port, 99);
  Check(QuackUriParse('127.0.0.1', host, port));
  CheckEqual(port, 9494);
  Check(not QuackUriParse('quack:', host, port));
  Check(not QuackUriParse('quack:host:0', host, port));
  Check(not QuackUriParse('quack:host:70000', host, port));
  Check(not QuackUriParse('quack:host:1a', host, port));
  Check(not QuackUriParse('quack:[::1', host, port));
  Check(QuackHostIsLocal('LocalHost'));
  Check(QuackHostIsLocal('::1'));
  Check(not QuackHostIsLocal('example.org'));
  // heartbeat message: type=11 + void connection_id + no client_query_id
  req := QuackSimpleRequest(QUACK_HEARTBEAT_REQUEST, 'abc');
  CheckEqual(BinToHexLower(req),
    '0100' + '10' + '0200' + '03616263' + '0300' + 'ffffffffffffffffff01' +
    'ffff' + 'ffff');
  // the prepare request is made of the sql text and a signed 128-bit uuid
  uuid.Upper := -2;
  uuid.Lower := 300;
  req := QuackPrepareRequest('c', 'select 1', uuid, 3);
  CheckEqual(BinToHexLower(req),
    '0100' + '03' + '0200' + '0163' + '0300' + 'ffffffffffffffffff01' + 'ffff' +
    '0100' + '0873656c65637420' + '31' + '0200' + '7e' + 'ac02' + 'ffff');
  // messages captured from a real DuckDB 1.5.6 server (protocol version 1)
  uuid.Upper := 0;
  uuid.Lower := 0;
  CheckEqual(BinToHexLower(QuackPrepareRequest(
    '877CE45F500BE01EAE5EF96DE8FB31AF', 'SELECT 42 AS a', uuid, 1)),
    '01000302002038373743453435463530304245303145414535454639364445384642' +
    '333141460300ffffffffffffffffff01ffff01000e53454c4543542034322041532061' +
    'ffff', 'real prepare');
  CheckEqual(BinToHexLower(QuackSimpleRequest(QUACK_DISCONNECT_MESSAGE,
    '877CE45F500BE01EAE5EF96DE8FB31AF')),
    '01000b02002038373743453435463530304245303145414535454639364445384642' +
    '333141460300ffffffffffffffffff01ffffffff', 'real disconnect');
  CheckEqual(BinToHexLower(MockEmptyFetchResponseV1),
    '0100080300ffffffffffffffffff01ffff0200ffffffffffffffffff01ffff',
    'real empty fetch');
  resp := TQuackResponse.Create(HexToBin(
    '01000202002038373743453435463530304245303145414535454639364445384642' +
    '333141460300ffffffffffffffffff01ffff01000676312e352e3602000b6c696e75' +
    '785f616d643634030001ffff'), 1);
  try
    CheckEqual(resp.MessageType, QUACK_CONNECTION_RESPONSE);
    CheckEqual(resp.ConnectionId, '877CE45F500BE01EAE5EF96DE8FB31AF');
    CheckEqual(resp.ServerVersion, 'v1.5.6');
    CheckEqual(resp.ServerPlatform, 'linux_amd64');
    CheckEqual(resp.QuackVersion, 1);
  finally
    resp.Free;
  end;
  resp := TQuackResponse.Create(HexToBin(
    '0100040300ffffffffffffffffff01ffff01000164000dffff0200010161040001012c' +
    '0164000165000164000dffff6600016400006600042a000000ffffffffffff0500b49a' +
    'c9fc8a8baad06fdc9dbfbc8eb2e6f78801ffff'), 1);
  try
    CheckEqual(resp.MessageType, QUACK_PREPARE_RESPONSE);
    CheckEqual(length(resp.ColumnTypes), 1);
    CheckEqual(resp.ColumnNames[0], 'a');
    Check(not resp.NeedsMoreFetch);
    CheckEqual(length(resp.Chunks), 1);
    CheckEqual(resp.Chunks[0].RowCount, 1);
    CheckEqual(PInteger(resp.Chunks[0].Vectors[0].DataAt(0))^, 42);
    Check((resp.QueryUuid.Lower <> 0) or
          (resp.QueryUuid.Upper <> 0));
  finally
    resp.Free;
  end;
  // fetch request with its batch and ack indexes
  req := QuackFetchRequest('c', uuid, 5, 4, 3);
  r.Init(req);
  CheckEqual(r.Next2, 1);
  CheckEqual(r.VarUInt32, QUACK_FETCH_REQUEST);
  // random uuid are not void
  QuackNewQueryUuid(uuid);
  Check((uuid.Lower <> 0) or
        (uuid.Upper <> 0));
  // check the generated mock header byte for byte under FPC i386
  CheckEqual(BinToHexLower(copy(MockQuery('select 1'), 1, 19)),
    '0100040300ffffffffffffffffff01ffff0100', 'mock header bytes');
  // malformed responses are rejected
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  try
    TQuackResponse.Create(MockTruncatedPrepare).Free;
    Check(false, 'truncated');
  except
    on ESqlDBQuack do
      Check(true);
  end;
  try
    TQuackResponse.Create(MockIncompleteSuccess).Free;
    Check(false, 'void body');
  except
    on ESqlDBQuack do
      Check(true);
  end;
  TSynLog.Family.ExceptionIgnoreCurrentThread := false;
end;

function VectorText(V: TQuackVector; Row: PtrInt): RawUtf8;
begin
  QuackValueToText(V, Row, result);
end;

procedure TTestQuack.Vectors;
var
  resp: TQuackResponse;
  c: TQuackChunk;
  i: integer;
  sel: array[0..3] of integer;
  w: TJsonWriter;
  tmp: TTextWriterStackBuffer;
  js: RawUtf8;
  writer: TMockQuackWriter;
  message: RawByteString;
const
  ROWS = 4;
begin
  sel[0] := 0;
  sel[1] := 1;
  sel[2] := 1;
  sel[3] := 0;
  // flat, constant, dictionary and sequence vectors
  writer := TMockQuackWriter.Create;
  try
    writer.BeginPrepare(
      [mqtInteger, mqtVarchar, mqtBigint, mqtDecimal, mqtDate,
       mqtBoolean, mqtUuid, mqtEnum],
      ['k', 'dict', 'seq', 'dec', 'day', 'flag', 'id', 'color'], false, 1);
    writer.BeginChunk(ROWS,
      [mqtInteger, mqtVarchar, mqtBigint, mqtDecimal, mqtDate,
       mqtBoolean, mqtUuid, mqtEnum], true);
    writer.Field(90); writer.UInt(2); // constant vector
    writer.VectorInt32([42], []); writer.EndObject;
    writer.Field(90); writer.UInt(3); // dictionary vector
    writer.Field(91); writer.WriteVar(@sel[0], SizeOf(sel));
    writer.Field(92); writer.UInt(2);
    writer.VectorText(['one', ''], [true, false]); writer.EndObject;
    writer.Field(90); writer.UInt(4); // sequence vector
    writer.Field(91); writer.Signed(-5);
    writer.Field(92); writer.Signed(3);
    writer.EndObject;
    writer.VectorInt64([12345, -5, 0, 100], []); writer.EndObject;
    writer.VectorInt32([19000, 0, -1, 1], []); writer.EndObject;
    writer.VectorBytes([1, 0, 1, 0], []); writer.EndObject;
    writer.VectorInt64([1, 0, 2, 0, 3, 0, 4, 0], []); writer.EndObject;
    writer.VectorBytes([0, 1, 1, 0], []); writer.EndObject;
    writer.EndChunk(true);
    writer.EndPrepare;
    message := writer.FlushTo;
  finally
    writer.Free;
  end;
  resp := TQuackResponse.Create(message);
  try
    CheckEqual(resp.MessageType, QUACK_PREPARE_RESPONSE);
    CheckEqual(length(resp.ColumnTypes), 8);
    CheckEqual(length(resp.Chunks), 1);
    CheckEqual(resp.ColumnNames[1], 'dict');
    Check(not resp.NeedsMoreFetch);
    CheckEqual(resp.QueryUuid.Upper, -1234567);
    CheckEqual(resp.QueryUuid.Lower, $123456789abcdef);
    c := resp.Chunks[0];
    CheckEqual(c.RowCount, ROWS);
    for i := 0 to ROWS - 1 do
      CheckEqual(VectorText(c.Vectors[0], i), '42', 'constant');
    CheckEqual(VectorText(c.Vectors[1], 0), 'one', 'dict0');
    CheckEqual(VectorText(c.Vectors[1], 1), '', 'dict1');
    Check(c.Vectors[1].IsValid(0));
    Check(not c.Vectors[1].IsValid(1));
    Check(not c.Vectors[1].IsValid(2));
    Check(c.Vectors[1].IsValid(3));
    CheckEqual(VectorText(c.Vectors[2], 0), '-5', 'seq0');
    CheckEqual(VectorText(c.Vectors[2], 1), '-2', 'seq1');
    CheckEqual(VectorText(c.Vectors[2], 3), '4', 'seq3');
    CheckEqual(VectorText(c.Vectors[3], 0), '123.45', 'decimal');
    CheckEqual(VectorText(c.Vectors[3], 1), '-0.05', 'neg decimal');
    CheckEqual(VectorText(c.Vectors[3], 3), '1.00', 'decimal scale');
    CheckEqual(VectorText(c.Vectors[4], 0), '2022-01-08', 'date');
    CheckEqual(VectorText(c.Vectors[4], 1), '1970-01-01', 'date epoch');
    CheckEqual(VectorText(c.Vectors[5], 0), 'true', 'bool');
    CheckEqual(VectorText(c.Vectors[5], 1), 'false', 'bool');
    CheckEqual(VectorText(c.Vectors[7], 0), 'red', 'enum');
    CheckEqual(VectorText(c.Vectors[7], 1), 'green', 'enum');
    Check(resp.ColumnTypes[3].FieldType = ftCurrency);
    Check(resp.ColumnTypes[1].FieldType = ftUtf8);
  finally
    resp.Free;
  end;
  // nested LIST and STRUCT vectors
  writer := TMockQuackWriter.Create;
  try
    writer.BeginPrepare([mqtListInt, mqtStructAB], ['l', 's'], false, 1);
    writer.BeginChunk(3, [mqtListInt, mqtStructAB], true);
    writer.BeginVector([true, true, false]);
    writer.Field(104); writer.UInt(3);
    writer.Field(105); writer.UInt(3);
    writer.Field(100); writer.UInt(0); writer.Field(101); writer.UInt(2);
    writer.EndObject;
    writer.Field(100); writer.UInt(2); writer.Field(101); writer.UInt(1);
    writer.EndObject;
    writer.Field(100); writer.UInt(0); writer.Field(101); writer.UInt(0);
    writer.EndObject;
    writer.Field(106);
    writer.VectorInt32([10, 20, 30], []); writer.EndObject;
    writer.EndObject; // list vector
    writer.BeginVector([]);
    writer.Field(103); writer.UInt(2);
    writer.VectorInt32([1, 0, 3], [true, false, true]); writer.EndObject;
    writer.VectorText(['x', 'y', 'z'], []); writer.EndObject;
    writer.EndObject; // struct vector
    writer.EndChunk(true);
    writer.EndPrepare;
    message := writer.FlushTo;
  finally
    writer.Free;
  end;
  resp := TQuackResponse.Create(message);
  try
    c := resp.Chunks[0];
    CheckEqual(VectorText(c.Vectors[0], 0), '[10,20]', 'list0');
    CheckEqual(VectorText(c.Vectors[0], 1), '[30]', 'list1');
    CheckEqual(VectorText(c.Vectors[0], 2), '', 'null list');
    CheckEqual(VectorText(c.Vectors[1], 0), '{"a":1,"b":"x"}', 'struct0');
    CheckEqual(VectorText(c.Vectors[1], 1), '{"a":null,"b":"y"}', 'struct1');
    w := TJsonWriter.CreateOwnedStream(tmp);
    try
      for i := 0 to 2 do
      begin
        QuackValueToJson(c.Vectors[0], i, w);
        w.AddComma;
      end;
      w.CancelLastComma;
      w.SetText(js);
    finally
      w.Free;
    end;
    CheckEqual(js, '[10,20],[30],null', 'json');
  finally
    resp.Free;
  end;
  // invalid chunk content must be rejected before accessing its bounds
  writer := TMockQuackWriter.Create;
  try
    writer.BeginPrepare([mqtBigint], ['n'], false, 1);
    writer.BeginChunk(3, [mqtBigint], true);
    writer.VectorInt64([1, 2], []); writer.EndObject;
    writer.EndChunk(true);
    writer.EndPrepare;
    message := writer.FlushTo;
  finally
    writer.Free;
  end;
  try
    TQuackResponse.Create(message).Free;
    Check(false, 'data size');
  except
    on ESqlDBQuack do
      Check(true);
  end;
  writer := TMockQuackWriter.Create;
  try
    writer.BeginPrepare([mqtVarchar], ['s'], false, 1);
    writer.BeginChunk(2, [mqtVarchar], true);
    writer.BeginVector([]);
    writer.Field(107); writer.UInt(3); // total bytes intentionally wrong
    writer.Field(108); writer.BlobInt32([2, 2]);
    writer.Field(109); writer.Text('abc');
    writer.EndObject;
    writer.EndChunk(true);
    writer.EndPrepare;
    message := writer.FlushTo;
  finally
    writer.Free;
  end;
  try
    TQuackResponse.Create(message).Free;
    Check(false, 'string lengths');
  except
    on ESqlDBQuack do
      Check(true);
  end;
end;

procedure TTestQuack.ClientVersion1;
begin
  ClientTest(1);
end;

procedure TTestQuack.ClientVersion3;
begin
  ClientTest(3);
end;

procedure TTestQuack.ClientTest(MockVersion: cardinal);
var
  port: integer;
  props: TSqlDBQuackConnectionProperties;

  procedure DoTests;
  var
    conn: TSqlDBConnection;
    rows: ISqlDBRows;
    st: ISqlDBStatement;
    u, m: RawUtf8;
    i: integer;
    bad: TSqlDBQuackConnectionProperties;
  begin
    CheckEqual(props.Host, '127.0.0.1');
    CheckEqual(props.Port, 8890 + integer(MockVersion));
    Check(not props.UseTls);
    // wrong token
    bad := TSqlDBQuackConnectionProperties.Create(
      FormatUtf8('quack:127.0.0.1:%', [port]), '', '', 'wrong');
    try
      try
        bad.ThreadSafeConnection.Connect;
        Check(false, 'bad token');
      except
        on ESqlDBQuack do
          Check(true);
      end;
    finally
      bad.Free;
    end;
    // single response with all columns types
    rows := props.Execute('select 1', []);
    Check(rows <> nil);
    Check(rows.Step);
    CheckEqual(rows.ColumnInt(0), 1);
    CheckEqual(rows.ColumnUtf8(1), 'a');
    Check(rows.ColumnDouble(2) = 1.5);
    Check(rows.Step);
    Check(rows.ColumnNull(0));
    Check(rows.ColumnNull(1));
    CheckEqual(rows.ColumnInt(0), 0);
    Check(rows.Step);
    CheckEqual(rows.ColumnInt(0), 3);
    CheckEqual(rows.ColumnUtf8(1), 'ccc');
    Check(rows.ColumnDouble(2) = 3.5);
    Check(not rows.Step);
    CheckEqual(rows.ColumnCount, 3);
    CheckEqual(rows.ColumnName(1), 's');
    Check(rows.ColumnType(0) = ftInt64);
    Check(rows.ColumnType(1) = ftUtf8);
    Check(rows.ColumnType(2) = ftDouble);
    // JSON export
    rows := props.Execute('select 1', []);
    CheckEqual(rows.FetchAllAsJson(true),
      '[{"i":1,"s":"a","d":1.5},{"i":null,"s":null,"d":2.5},' +
      '{"i":3,"s":"ccc","d":3.5}]'#10, 'json');
    // VARIANT columns are asked as JSON
    rows := props.Execute('select variant', []);
    Check(rows.Step);
    CheckEqual(rows.ColumnInt(0), 1);
    CheckEqual(rows.ColumnUtf8(1), '{"a":[1,2]}');
    Check(rows.Step);
    Check(rows.ColumnNull(1));
    Check(not rows.Step);
    CheckEqual(fQueries[high(fQueries)],
      'SELECT #1 AS "id", NULLIF(#2::JSON, ''null''::JSON) AS "v" FROM (select variant)');
    rows := nil;
    // values conversions
    rows := props.Execute('select flags', []);
    Check(rows.Step);
    CheckEqual(rows.ColumnInt(0), 1);
    CheckEqual(CurrencyToStr(rows.ColumnCurrency(1)), '123.45', 'currency');
    CheckEqual(DoubleToStr(rows.ColumnDouble(1)), '123.45', 'double');
    CheckEqual(rows.ColumnInt(1), 123);
    CheckEqual(DateToIso8601Text(rows.ColumnDateTime(2)), '2022-01-08');
    CheckEqual(rows.ColumnUtf8(2), '2022-01-08');
    CheckEqual(DateTimeToIso8601(rows.ColumnDateTime(3), true, 'T', true),
      '2001-02-03T04:05:06.789');
    CheckEqual(rows.ColumnUtf8(3), '2001-02-03 04:05:06.789');
    CheckEqual(rows.ColumnUtf8(4), '[7,8]');
    CheckEqual(BinToHexLower(rows.ColumnBlob(5)), '0100ff', 'blob');
    Check(rows.ColumnType(1) = ftCurrency);
    Check(rows.ColumnType(2) = ftDate);
    Check(rows.ColumnType(5) = ftBlob);
    rows := nil;
    rows := props.Execute('select flags', []);
    PCardinal(FastSetString(m, 3))^ := JSON_BASE64_MAGIC_C;
    Join(['[{"b":1,"m":123.45,"d":"2022-01-08","t":"2001-02-03T04:05:06",' +
          '"l":[7,8],"x":"', m, 'AQD/"}]'#10], u);
    CheckEqual(rows.FetchAllAsJson(true), u, 'flags json');
    // several batches, requested on demand
    fFetches := 0;
    rows := props.Execute('select big', []);
    for i := 1 to 13 do
      if i <= 12 then
      begin
        Check(rows.Step, 'big step');
        CheckEqual(rows.ColumnInt(0), i, 'big value');
        if i = 3 then
          CheckEqual(fFetches, 0, 'inlined');
        if i = 4 then
          CheckEqual(fFetches, 1, 'first batch');
      end
      else
        Check(not rows.Step, 'big end');
    if MockVersion = 1 then
      CheckEqual(fFetches, 2, 'fetches until the empty one')
    else
      CheckEqual(fFetches, 3, 'fetches until the last one');
    rows := nil;
    // a result not fully retrieved is canceled
    fCancels := 0;
    rows := props.Execute('select big', []);
    Check(rows.Step);
    rows.ReleaseRows;
    rows := nil;
    if MockVersion = 1 then
      CheckEqual(fCancels, 0, 'no cancel in version 1')
    else
      CheckEqual(fCancels, 1, 'cancel');
    // DML count
    CheckEqual(props.ExecuteNoResult('insert into t values(1)', []), 7);
    // errors
    try
      props.ExecuteNoResult('fail now', []);
      Check(false, 'error');
    except
      on E: ESqlDBQuack do
        Check(Pos('Catalog Error: Table does not exist!', E.Message) > 0,
          E.Message);
    end;
    // the SQL parameters are inlined as literals
    conn := props.ThreadSafeConnection;
    CheckEqual(TSqlDBQuackConnection(conn).Version, MockVersion, 'detected');
    st := conn.NewStatementPrepared('echo ?,?,?,?,?,?,?,?', true, true);
    st.Bind(1, 5);
    st.BindTextU(2, 'O''Neil ?');
    st.BindCurrency(3, 12.5);
    st.BindBlob(4, HexToBin('0102ff'));
    st.BindNull(5);
    st.Bind(6, 1.25);
    st.BindDateTime(7, EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 789));
    st.BindTextU(8, 'a'#13#10'b');
    st.ExecutePrepared;
    Check(st.Step);
    CheckEqual(st.ColumnUtf8(0),
      'echo 5,''O''''Neil ?'',12.5,''\x01\x02\xff''::BLOB,null,1.25,' +
      'TIMESTAMP ''2001-02-03T04:05:06.789'',''a'#13#10'b''', 'inlined');
    st := nil;
    // a ? within a quoted text is not a parameter
    st := conn.NewStatementPrepared('echo ''?'', "?", ?', true, true);
    st.Bind(1, 1);
    st.ExecutePrepared;
    Check(st.Step);
    CheckEqual(st.ColumnUtf8(0), 'echo ''?'', "?", 1');
    st := nil;
    // transactions
    conn.StartTransaction;
    conn.Commit;
    conn.StartTransaction;
    conn.Rollback;
    CheckEqual(fQueries[high(fQueries)], 'ROLLBACK');
  end;

begin
  fMockVersion := MockVersion;
  FreeAndNil(fMock);
  port := 8890 + MockVersion;
  fMock := THttpServer.Create(UInt32ToUtf8(port), nil, nil,
    'quack', 2);
  fMock.OnRequest := OnMockRequest;
  fMock.WaitStarted;
  props := TSqlDBQuackConnectionProperties.Create(
    FormatUtf8('quack:127.0.0.1:%', [port]), '', '', 'secret');
  try
    // local sub-procedure to ensure all interfaces are cleared
    DoTests;
    // validates that the server is notified when disconnecting
    fDisconnects := 0;
    props.ClearConnectionPool; // no dandling interface should remain now
    CheckEqual(fDisconnects, 1, 'disconnect');
  finally
    props.Free;
    CheckEqual(fMockError, '', 'mock server');
  end;
end;

procedure TTestQuack.CompareToEmbedded(Embedded: TSqlDBConnectionProperties;
  Quack: TSqlDBConnectionProperties; const Sql: RawUtf8);
var
  e, q: ISqlDBRows;
  row, col: integer;
begin
  e := Embedded.Execute(Sql, []);
  q := Quack.Execute(Sql, []);
  row := 0;
  while e.Step do
  begin
    if not q.Step then
    begin
      Check(false, 'missing row');
      exit;
    end;
    for col := 0 to e.ColumnCount - 1 do
      if e.ColumnNull(col) <> q.ColumnNull(col) then
        CheckUtf8(false, 'null row % col %', [row, e.ColumnName(col)])
      else if e.ColumnUtf8(col) <> q.ColumnUtf8(col) then
        CheckUtf8(false, 'row % col % embedded=% quack=%',
          [row, e.ColumnName(col), e.ColumnUtf8(col), q.ColumnUtf8(col)]);
    inc(row);
  end;
  Check(not q.Step, 'extra row');
  CheckEqual(row, 5000);
end;

{$ifdef CPU64}
procedure TTestQuack.DuckDBServer;
var
  server: TSqlDBDuckDBConnectionProperties;
  client: TSqlDBQuackConnectionProperties;
  rows: ISqlDBRows;
  ok: boolean;
  i: integer;
  err: string;
  st: ISqlDBStatement;
begin
  // requires the local 64-bit DuckDB library, and the quack extension
  if not FileExists(Executable.ProgramFilePath + DuckDBLibraryName) then
    exit;
  server := nil;
  ok := false;
  TSynLog.Family.ExceptionIgnoreCurrentThread := true;
  try
    try
      if DuckDBLibraryAvailable then
      begin
        server := TSqlDBDuckDBConnectionProperties.Create(':memory:', '', '', '');
        server.QuackServe('quack:127.0.0.1:9498', 'secret');
        ok := true;
      end;
    except
      on E: Exception do
      begin
        ok := false;
        err := E.Message;
      end;
    end;
  finally
    TSynLog.Family.ExceptionIgnoreCurrentThread := false;
  end;
  if not ok then
  begin
    NotifyProgress(['DuckDB Quack server not available (', err,
      ') - tests skipped']);
    FreeAndNil(server);
    exit;
  end;
  try
    server.ExecuteNoResult('create table qt(id integer, name varchar, ' +
      'amount decimal(10,2), tags integer[], born date)', []);
    server.ExecuteNoResult('insert into qt select i, ''name '' || i, ' +
      'i / 4.0, [i, i + 1], date ''2000-01-01'' + cast(i as integer) from range(5000) t(i)', []);
    client := TSqlDBQuackConnectionProperties.Create(
      'quack:127.0.0.1:9498', '', '', 'secret');
    try
      rows := client.Execute('select * from qt order by id', []);
      i := 0;
      while rows.Step do
      begin
        CheckEqual(rows.ColumnInt(0), i);
        CheckEqual(rows.ColumnUtf8(1), FormatUtf8('name %', [i]));
        CheckEqual(rows.ColumnUtf8(3), FormatUtf8('[%,%]', [i, i + 1]));
        inc(i);
      end;
      CheckEqual(i, 5000, 'all rows');
      rows := nil;
      CheckEqual(client.ExecuteNoResult(
        'update qt set name = ? where id < ?', ['x', 10]), 10);
      // parameters, transactions and errors
      client.ExecuteNoResult('create table qp(id integer, name varchar, ' +
        'amount decimal(19,4), born timestamp, photo blob, ratio double)', []);
      st := client.NewThreadSafeStatementPrepared(
        'insert into qp values (?,?,?,?,?,?)', false, true);
      st.Bind(1, 1);
      st.BindTextU(2, 'O''Neil ? -- "x" \n');
      st.BindCurrency(3, 1234.5678);
      st.BindDateTime(4, EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 789));
      st.BindBlob(5, HexToBin('0001275cff')); //  #0#1'''\'#255
      st.Bind(6, 0.1);
      st.ExecutePrepared;
      CheckEqual(st.UpdateCount, 1);
      st := nil;
      rows := client.Execute('select * from qp where id = ?', [1]);
      Check(rows.Step);
      CheckEqual(rows.ColumnUtf8(1), 'O''Neil ? -- "x" \n');
      CheckEqual(CurrencyToStr(rows.ColumnCurrency(2)), '1234.5678', 'cur');
      CheckEqual(DateTimeToIso8601(rows.ColumnDateTime(3), true, 'T', true),
        '2001-02-03T04:05:06.789');
      CheckEqual(BinToHexLower(rows.ColumnBlob(4)), '0001275cff');
      CheckEqual(DoubleToStr(rows.ColumnDouble(5)), '0.1', 'dbl');
      rows := nil;
      client.ThreadSafeConnection.StartTransaction;
      client.ExecuteNoResult('delete from qp', []);
      client.ThreadSafeConnection.Rollback;
      rows := client.Execute('select count(*) from qp', []);
      Check(rows.Step);
      CheckEqual(rows.ColumnInt(0), 1, 'rollback');
      rows := nil;
      try
        client.ExecuteNoResult('select * from missing_table', []);
        Check(false, 'error');
      except
        on E: ESqlDBQuack do
          Check(Pos('missing_table does not exist', E.Message) > 0, E.Message);
      end;
      // a result released before its end does not break the next queries
      rows := client.Execute('select * from range(100000)', []);
      Check(rows.Step);
      rows.ReleaseRows;
      rows := nil;
      rows := client.Execute('select 42', []);
      Check(rows.Step);
      CheckEqual(rows.ColumnInt(0), 42);
      rows := nil;
      // all types should be identical to the embedded engine
      CompareToEmbedded(server, client,
        'select i, true b, (i % 100 - 50)::tinyint ti, (i - 2500)::smallint si, ' +
        '-i * 1000::integer ii, i * 123456789012::bigint bi, (i % 256)::utinyint u1, ' +
        '(i * 7)::usmallint u2, (i * 100000)::uinteger u3, ' +
        '(i * 1000000000000)::ubigint u4, (i * 1000000000000000000000)::hugeint h, ' +
        '(i / 3.0)::float f, (i / 7.0)::double d, (i / 8.0)::decimal(10,2) dec1, ' +
        '(i * 1234567890123456789.12345)::decimal(30,5) dec2, ' +
        '(i % 100 / 4.0)::decimal(4,1) dec3, ' +
        '''text '' || i s, case when i % 3 = 0 then null else ''é€ '' || i end u, ' +
        'repeat(''const'', 2) c, ''\xAA\xBB''::blob bl, ' +
        'date ''2024-02-29'' + cast(i % 1000 as integer) dt, ' +
        'time ''12:34:56.789'' tm, ' +
        'timestamp ''2024-02-29 12:34:56.123456'' + to_seconds(i) ts, ' +
        'timestamptz ''2024-02-29 12:34:56+00'' tz, ' +
        'to_months(i % 20) + to_days(i % 5) iv, ' +
        'uuid() = uuid() eq, gen_random_uuid() is not null ug, ' +
        '[i, i + 1, null] l, [[i], [i, i], []] ll, {''a'': i, ''b'': ''x'' || i} st, ' +
        'map {''k'': i} m, [i, i + 1]::integer[2] ar, null::integer n, ' +
        'case when i % 2 = 0 then 1 else null end::union(a integer, b varchar) un ' +
        'from range(5000) t(i)');
      // VARIANT columns, including nested ones, are retrieved as JSON
      if DuckDB.Version >= 'v2' then
      begin
        rows := client.Execute('select {''a'': 1, ''l'': [1, null]}::variant v', []);
        Check(rows.Step);
        CheckEqual(rows.ColumnUtf8(0), '{"a":1,"l":[1,null]}', 'variant');
        rows := nil;
        CompareToEmbedded(server, client,
          'select i, case when i % 3 = 0 then null::variant when i % 3 = 1 ' +
          'then {''a'': i, ''l'': [i, null]}::variant else (''s'' || i)::variant ' +
          'end v, [i::variant] lv from range(5000) t(i)');
      end;
      // types with nanoseconds resolution and GEOMETRY, as WKB
      if DuckDB.Version >= 'v2' then
        CompareToEmbedded(server, client,
          'select i, timestamp_ns ''2024-02-29 12:34:56.123456789'' + ' +
          'to_seconds(i) tns, (timestamp_ns ''2024-02-29 12:34:56.123456789'' + ' +
          'to_seconds(i))::timestamptz_ns tzns, (''POINT('' || i || '' 2)'')::geometry g ' +
          'from range(5000) t(i)');
    finally
      client.Free;
    end;
  finally
    server.Free;
  end;
end;
{$endif CPU64}


end.
