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
  mormot.core.json,
  mormot.core.variants,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.test,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.raw.duckdb,
  mormot.db.sql.duckdb;

type
  /// regression tests for mormot.db.sql.duckdb.pas
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
    procedure Metadata;
    procedure Bulk;
    procedure BatchInsert;
    procedure Appender;
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
  if not result then
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
  try
    rows.ColumnUtf8(2);
    Check(false, 'variant');
  except
    on E: ESqlDBDuckDB do
      Check(Pos('#41', E.Message) > 0, 'variant error');
  end;
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
  names, names2: TRawUtf8DynArray;
  types: TSqlDBFieldTypeArray;
  values, values2: TRawUtf8DynArrayDynArray;
  i, pass: integer;
  rows: ISqlDBRows;
  timer: TPrecisionTimer;
begin
  if fProps = nil then
    exit;
  Check(Assigned(fProps.OnBatchInsert), 'OnBatchInsert');
  CheckEqual(ord(fProps.BatchSendingAbilities = [cCreate]), 1);
  fProps.ExecuteNoResult('create or replace table arr (id INTEGER, ' +
    'name VARCHAR, amount DECIMAL(19,4), d TIMESTAMP, v DOUBLE)', []);
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
  // pass 0 uses the Appender, pass 1 the multi-values INSERT fallback
  // (inserting only id and name for 100 more rows)
  for pass := 0 to 1 do
  begin
    for i := 0 to COUNT - 1 do
    begin
      values[0, i] := Int32ToUtf8(pass * COUNT + i + 1);
      values[1, i] := QuotedStr(FormatUtf8('it''s #%', [i + 1]), '''');
      values[2, i] := Int32ToUtf8(i + 1) + '.5';
      values[3, i] := QuotedStr(DateToIso8601Text(EncodeDate(2025, 1, 1) + i), '''');
      if i = 0 then
        values[4, i] := 'null'
      else
        values[4, i] := DoubleToStr(i / 8);
    end;
    timer.Start;
    if pass = 0 then
      // the ORM supplies schema-qualified table names
      fProps.OnBatchInsert(fProps, 'main.arr', names, types, COUNT, values, [])
    else
    begin
      // not all columns: the Appender is not usable -> regular INSERT
      SetLength(names2, 2);
      SetLength(values2, 2);
      for i := 0 to 1 do
      begin
        names2[i] := names[i];
        values2[i] := values[i];
      end;
      fProps.OnBatchInsert(fProps, 'arr', names2, types, 100, values2, []);
    end;
    NotifyTestSpeed('insert', COUNT, 0, @timer);
  end;
  rows := fProps.Execute('select count(*), sum(amount), max(d), ' +
    'max(name) filter (where id = 1000), sum(v), count(v) from arr', []);
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

end.
