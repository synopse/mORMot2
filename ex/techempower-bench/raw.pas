program raw;

{
TechEmpower framework benchmarks implementation
See https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview
}

{$I mormot.defines.inc}

{.$define USE_SQLITE3}
// may be defined to use a SQLite3 database instead of external PostgresSQL DB
// - note: /rawupdates and /rawqueries are PostgresSQL specific and will fail

{.$define WITH_LOGS}
// logging is fine for debugging, less for benchmarking ;)

uses
  {$I mormot.uses.inc} // include mormot.core.fpcx64mm
  sysutils,
  classes,
  BaseUnix,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.perf,
  mormot.core.mustache,
  mormot.orm.core,
  mormot.orm.sql,
  mormot.db.core,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  {$ifdef USE_SQLITE3}
  mormot.db.sql.sqlite3,
  {$endif USE_SQLITE3}
  mormot.rest.sqlite3,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async,
  mormot.db.sql,
  mormot.db.sql.postgres;

type
  // data structures
  TMessageRec = packed record
    message: RawUtf8;
  end;
  TWorldRec = packed record
    id: integer;
    randomNumber: integer;
  end;
  TWorlds = array of TWorldRec;
  TFortune = packed record
    id: integer;
    message: RawUtf8;
  end;
  TFortunes = array of TFortune;

  // ORM definitions
  TOrmWorld = class(TOrm)
  protected
    fRandomNumber: integer;
  published
    property randomNumber: integer
      read fRandomNumber write fRandomNumber;
  end;
  TOrmCachedWorld = class(TOrmWorld);
  TOrmWorldClass = class of TOrmWorld;
  TOrmFortune = class(TOrm)
  protected
    fMessage: RawUtf8;
  published
    property Message: RawUtf8
      read fMessage write fMessage;
  end;
  TOrmFortunes = array of TOrmFortune;

  // main server class
  TRawAsyncServer = class(TSynPersistent)
  private
    fHttpServer: THttpAsyncServer;
    fDbPool: TSqlDBConnectionProperties;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
  protected
    {$ifdef USE_SQLITE3}
    procedure GenerateDB;
    {$endif USE_SQLITE3}
    // as used by rawqueries and rawupdates
    procedure getRawRandomWorlds(cnt: PtrInt; out res: TWorlds);
    // implements /queries and /cached-queries endpoints
    function doqueries(ctxt: THttpServerRequestAbstract; orm: TOrmWorldClass;
      const search: RawUtf8): cardinal;
  public
    constructor Create(threadCount: integer); reintroduce;
    destructor Destroy; override;
  published
    // all service URI are implemented by these published methods using RTTI
    function plaintext(ctxt: THttpServerRequestAbstract): cardinal;
    function json(ctxt: THttpServerRequestAbstract): cardinal;
    function db(ctxt: THttpServerRequestAbstract): cardinal;
    function queries(ctxt: THttpServerRequestAbstract): cardinal;
    function cached_queries(ctxt: THttpServerRequestAbstract): cardinal;
    function fortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function updates(ctxt: THttpServerRequestAbstract): cardinal;
    function rawdb(ctxt: THttpServerRequestAbstract): cardinal;
    function rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
    function rawfortunes(ctxt: THttpServerRequestAbstract): cardinal;
    function rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
  end;

const
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  WORLD_COUNT = 10000;

  WORLD_READ_SQL = 'select id, randomNumber from World where id=?';
  WORLD_UPDATE_SQLN ='update World as t set randomNumber = v.r from ' +
    '(SELECT unnest(?::NUMERIC[]), unnest(?::NUMERIC[])) as v(id, r)' +
    ' where t.id = v.id';
  FORTUNES_SQL = 'select id, message from Fortune';

  FORTUNES_MESSAGE = 'Additional fortune added at request time.';
  FORTUNES_TPL = '<!DOCTYPE html>' +
                 '<html>' +
                 '<head><title>Fortunes</title></head>' +
                 '<body>' +
                 '<table>' +
                 '<tr><th>id</th><th>message</th></tr>' +
                 '{{#.}}' +
                 '<tr><td>{{id}}</td><td>{{message}}</td></tr>' +
                 '{{/.}}' +
                 '</table>' +
                 '</body>' +
                 '</html>';


function RandomWorld: integer; inline;
begin
  result := Random32(WORLD_COUNT) + 1;
end;

function getQueriesParamValue(ctxt: THttpServerRequestAbstract;
  const search: RawUtf8 = 'QUERIES='): cardinal;
begin
  if not ctxt.UrlParam(search, result) then
    result := 1
  else if result > 500 then
    result := 500;
end;


{ TRawAsyncServer }

constructor TRawAsyncServer.Create(threadCount: integer);
begin
  inherited Create;
  {$ifdef USE_SQLITE3}
  fDbPool := TSqlDBSQLite3ConnectionProperties.Create(
      SQLITE_MEMORY_DATABASE_NAME, '', '', '');
  fDbPool.StatementCacheReplicates := threadcount; // shared SQlite3 connection
  {$else}
  fDbPool := TSqlDBPostgresConnectionProperties.Create(
    'tfb-database:5432', 'hello_world', 'benchmarkdbuser', 'benchmarkdbpass');
  {$endif USE_SQLITE3}
  fModel := TOrmModel.Create([TOrmWorld, TOrmFortune, TOrmCachedWorld]);
  OrmMapExternal(fModel, [TOrmWorld, TOrmFortune], fDbPool);
  // CachedWorld table doesn't exists in DB, but should as read in requirements.
  // Use world table as in other implementations.
  OrmMapExternal(fModel, TOrmCachedWorld, fDbPool, 'world');
  fStore := TRestServerDB.Create(fModel, SQLITE_MEMORY_DATABASE_NAME);
  fStore.NoAjaxJson := true;
  {$ifdef USE_SQLITE3}
  GenerateDB;
  {$else}
  fStore.Server.CreateMissingTables; // create SQlite3 virtual tables
  {$endif USE_SQLITE3}
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fTemplate := TSynMustache.Parse(FORTUNES_TPL);
  fHttpServer := THttpAsyncServer.Create(
    '8080', nil, nil, '', threadCount,
    5 * 60 * 1000,         // 5 minutes keep alive connections
    [hsoNoXPoweredHeader,  // not needed for a benchmark
     hsoHeadersInterning,  // reduce memory contention for /plaintext and /json
     hsoNoStats,           // disable low-level statistic counters
     hsoThreadCpuAffinity, // for better scaling of /plaintext
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TPW General Test Requirements #5
    ]);
  fHttpServer.HttpQueueLength := 100000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.Route.RunMethods([urmGet], self);
  // writeln(fHttpServer.Route.Tree[urmGet].ToText);
  fHttpServer.WaitStarted; // raise exception e.g. on binding issue
end;

destructor TRawAsyncServer.Destroy;
begin
  fHttpServer.Free;
  fStore.Free;
  fModel.Free;
  fDBPool.free;
  inherited Destroy;
end;

{$ifdef USE_SQLITE3}

const
  _FORTUNES: array[1..12] of RawUtf8 = (
  'fortune: No such file or directory',
  'A computer scientist is someone who fixes things that aren''t broken.',
  'After enough decimal places, nobody gives a damn.',
  'A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1',
  'A computer program does what you tell it to do, not what you want it to do.',
  'Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen',
  'Any program that runs right is obsolete.',
  'A list is only as strong as its weakest link. — Donald Knuth',
  'Feature: A bug with seniority.',
  'Computers make very fast, very accurate mistakes.',
  '<script>alert("This should not be displayed in a browser alert box.");</script>',
  'フレームワークのベンチマーク');

procedure TRawAsyncServer.GenerateDB;
var
  i: PtrInt;
  b: TRestBatch;
  w: TOrmWorld;
  f: TOrmFortune;
begin
  fStore.Server.CreateMissingTables;
  w := TOrmWorld.Create;
  f := TOrmFortune.Create;
  b := TRestBatch.Create(fStore.Orm, nil);
  try
    for i := 1 to WORLD_COUNT do
    begin
      w.IDValue := i;
      w.RandomNumber := RandomWorld;
      b.Add(w, true, true);
    end;
    for i := low(_FORTUNES) to high(_FORTUNES) do
    begin
      f.IDValue := i;
      f.Message := _FORTUNES[i];
      b.Add(f, true, true);
    end;
    if fStore.Orm.BatchSend(b) <> HTTP_SUCCESS then
      raise EOrmBatchException.Create('GenerateDB failed');
  finally
    b.Free;
    f.Free;
    w.Free;
  end;
end;

{$endif USE_SQLITE3}

function TRawAsyncServer.plaintext(ctxt: THttpServerRequestAbstract): cardinal;
begin
  ctxt.OutContentType := TEXT_CONTENT_TYPE_NO_ENCODING;
  ctxt.OutContent := HELLO_WORLD;
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.json(ctxt: THttpServerRequestAbstract): cardinal;
var
  msgRec: TMessageRec;
begin
  msgRec.message := HELLO_WORLD;
  ctxt.SetOutJson(SaveJson(msgRec, TypeInfo(TMessageRec)));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.rawdb(ctxt: THttpServerRequestAbstract): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  stmt.Bind(1, RandomWorld);
  stmt.ExecutePrepared;
  if stmt.Step then
  begin
    ctxt.SetOutJson('{"id":%,"randomNumber":%}',
      [stmt.ColumnInt(0), stmt.ColumnInt(1)]);
    result := HTTP_SUCCESS;
    stmt.ReleaseRows;
  end;
  stmt := nil;
end;

function TRawAsyncServer.db(ctxt: THttpServerRequestAbstract): cardinal;
var
  w: TOrmWorld;
begin
  w := TOrmWorld.Create(fStore.Orm, RandomWorld);
  try
    ctxt.SetOutJson('{"id":%,"randomNumber":%}', [w.IDValue, w.randomNumber]);
    result := HTTP_SUCCESS;
  finally
    w.Free;
  end;
end;

function TRawAsyncServer.queries(ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := doqueries(ctxt, TOrmWorld, 'QUERIES=');
end;

function TRawAsyncServer.cached_queries(ctxt: THttpServerRequestAbstract): cardinal;
begin
  result := doqueries(ctxt, TOrmCachedWorld, 'COUNT=');
end;

procedure TRawAsyncServer.getRawRandomWorlds(cnt: PtrInt; out res: TWorlds);
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  {$ifndef USE_SQLITE3}
  pStmt: TSqlDBPostgresStatement;
  {$endif USE_SQLITE3}
  i: PtrInt;
begin
  SetLength(res{%H-}, cnt);
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  {$ifdef USE_SQLITE3}
  for i := 0 to cnt - 1 do
  begin
    stmt.Bind(1, RandomWorld);
    stmt.ExecutePrepared;
    if not stmt.Step then
      exit;
    res[i].id := stmt.ColumnInt(0);
    res[i].randomNumber := stmt.ColumnInt(1);
  end;
  {$else}
  // specific code to use PostgresSQL pipelining mode
  TSqlDBPostgresConnection(conn).EnterPipelineMode;
  pStmt := (stmt as TSqlDBPostgresStatement);
  for i := 0 to cnt - 1 do
  begin
    stmt.Bind(1, RandomWorld);
    pStmt.SendPipelinePrepared;
  end;
  TSqlDBPostgresConnection(conn).PipelineSync;
  for i := 0 to cnt - 1 do
  begin
    pStmt.GetPipelineResult(i = 0);
    if not stmt.Step then
      exit;
    res[i].id := stmt.ColumnInt(0);
    res[i].randomNumber := stmt.ColumnInt(1);
  end;
  TSqlDBPostgresConnection(conn).ExitPipelineMode(true);
  {$endif USE_SQLITE3}
end;

function TRawAsyncServer.rawqueries(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt: PtrInt;
  res: TWorlds;
begin
  cnt := getQueriesParamValue(ctxt);
  getRawRandomWorlds(cnt, res);
  if res = nil then
    exit(HTTP_SERVERERROR);
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.doqueries(ctxt: THttpServerRequestAbstract;
  orm: TOrmWorldClass; const search: RawUtf8): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  w: TOrmWorld;
begin
  result := HTTP_SERVERERROR;
  cnt := getQueriesParamValue(ctxt, search);
  SetLength(res, cnt);
  w := orm.Create; // TOrmWorld or TOrmCachedWorld
  try
    for i := 0 to cnt - 1 do
    begin
      if not fStore.Orm.Retrieve(RandomWorld, w) then
        exit;
      res[i].id := w.IDValue;
      res[i].randomNumber := w.RandomNumber;
    end;
  finally
    w.Free;
  end;
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;

function OrmFortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TOrmFortune(A).Message), pointer(TOrmFortune(B).Message));
end;

function TRawAsyncServer.fortunes(ctxt: THttpServerRequestAbstract): cardinal;
var
  list: TOrmFortunes;
  new: TOrmFortune;
  arr: TDynArray;
begin
  result := HTTP_SERVERERROR;
  arr.Init(TypeInfo(TOrmFortunes), list);
  if fStore.Orm.RetrieveListObjArray(list, TOrmFortune, '', []) then
    try
      new := TOrmFortune.Create;
      new.Message := FORTUNES_MESSAGE;
      arr.Add(new);
      arr.Sort(OrmFortuneCompareByMessage);
      ctxt.OutContent := fTemplate.RenderDataArray(arr);
      ctxt.OutContentType := HTML_CONTENT_TYPE;
      result := HTTP_SUCCESS;
    finally
      arr.Clear;
    end;
end;

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequestAbstract): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  list: TFortunes;
  f: TFortune;
  arr: TDynArray;
  n: integer;
begin
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
  arr.Init(TypeInfo(TFortunes), list, @n);
  while stmt.Step do
  begin
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnUtf8(1);
    arr.Add(f);
  end;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Add(f);
  arr.Sort(FortuneCompareByMessage);
  ctxt.OutContent := fTemplate.RenderDataArray(arr);
  ctxt.OutContentType := HTML_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.updates(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  w: TOrmWorld;
  b: TRestBatch;
begin
  result := HTTP_SERVERERROR;
  cnt := getQueriesParamValue(ctxt);
  SetLength(res, cnt);
  b := TRestBatch.Create(fStore.ORM, TOrmWorld, {transrows=}0,
    [boExtendedJson, boNoModelEncoding, boPutNoCacheFlush]);
  w := TOrmWorld.Create;
  try
    for i := 0 to cnt - 1 do
    begin
      if not fStore.Orm.Retrieve(RandomWorld, w) then
        exit;
      w.RandomNumber := RandomWorld;
      b.Update(w);
      res[i].id := w.IDValue;
      res[i].randomNumber := w.RandomNumber;
    end;
    result := fStore.Orm.BatchSend(b);
  finally
    w.Free;
    b.Free;
  end;
  if result <> HTTP_SUCCESS then
    exit;
  ctxt.SetOutJson(SaveJson(res, TypeInfo(TWorlds)));
end;

function TRawAsyncServer.rawupdates(ctxt: THttpServerRequestAbstract): cardinal;
var
  cnt, i: PtrInt;
  words: TWorlds;
  ids, nums: TInt64DynArray;
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  cnt := getQueriesParamValue(ctxt);
  getRawRandomWorlds(cnt, words);
  if length(words) <> cnt then
    exit(HTTP_SERVERERROR);
  setLength(ids, cnt);
  setLength(nums, cnt);
  // generate new randoms, fill parameters arrays for update
  for i := 0 to cnt - 1 do
  begin
    words[i].randomNumber := RandomWorld;
    ids[i] := words[i].id;
    nums[i] := words[i].randomNumber;
  end;
  conn := fDbPool.ThreadSafeConnection;
  //conn.StartTransaction;
  stmt := conn.NewStatementPrepared(WORLD_UPDATE_SQLN, false, true);
  stmt.BindArray(1, nums);
  stmt.BindArray(2, ids);
  stmt.ExecutePrepared;
  //conn.Commit; // autocommit
  ctxt.SetOutJson(SaveJson(words, TypeInfo(TWorlds)));
  result := HTTP_SUCCESS;
end;



var
  rawServer: TRawAsyncServer;
  threads: integer;

begin
  {$ifdef WITH_LOGS}
  TSynLog.Family.Level := LOG_VERBOSE; // disable logs for benchmarking
  TSynLog.Family.HighResolutionTimestamp := true;
  TSynLog.Family.AutoFlushTimeOut := 1;
  {$else}
  {$ifdef USE_SQLITE3}
  TSynLog.Family.Level := LOG_STACKTRACE; // minimal debug logs on fatal errors
  {$endif USE_SQLITE3}
  {$endif WITH_LOGS}
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  Rtti.RegisterFromText([
    TypeInfo(TMessageRec), 'message:RawUtf8',
    TypeInfo(TWorldRec),   'id,randomNumber:integer',
    TypeInfo(TFortune),    'id:integer message:RawUtf8']);

  if (ParamCount <> 1) or
     not TryStrToInt(ParamStr(1), threads) then
    threads := SystemInfo.dwNumberOfProcessors * 4;
  if threads < 16 then
    threads := 16
  else if threads > 256 then
    threads := 256; // max. threads for THttpAsyncServer

  rawServer := TRawAsyncServer.Create(threads);
  try
    {$I-}
    writeln;
    writeln(rawServer.fHttpServer.ClassName, ' running on localhost:',
      rawServer.fHttpServer.SockPort, '; num thread=', threads, ' db=',
      rawServer.fDbPool.DbmsEngineName, #10);
    {$ifdef USE_SQLITE3}
    writeln('Press [Enter] or Ctrl+C or send SIGTERM to terminate'#10);
    ConsoleWaitForEnterKey;
    {$else}
    writeln('Press Ctrl+C or use SIGTERM to terminate'#10);
    FpPause; // mandatory for the actual benchmark tool
    {$endif USE_SQLITE3}
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    writeln(ObjectToJsonDebug(rawServer.fHttpServer, [woDontStoreVoid, woHumanReadable]));
    {$ifdef FPC_X64MM}
    WriteHeapStatus(' ', 16, 8, {compileflags=}true);
    {$endif FPC_X64MM}
  finally
    rawServer.Free;
  end;

end.

