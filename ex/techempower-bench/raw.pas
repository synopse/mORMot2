program raw;

{
 ----------------------------------------------------
   TechEmpower Framework Benchmarks implementation
   in modern pascal and the mORMot 2 framework
 ----------------------------------------------------
 https://github.com/TechEmpower/FrameworkBenchmarks/wiki
 command line optional syntax: raw [threads] [cores] [servers]
}

{$I mormot.defines.inc}

{.$define USE_SQLITE3}
// may be defined to use a SQLite3 database instead of external PostgresSQL DB

{.$define WITH_LOGS}
// logging is fine for debugging, less for benchmarking ;)

uses
  {$I mormot.uses.inc} // include mormot.core.fpcx64mm or mormot.core.fpclibcmm
  sysutils,
  classes,
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
  mormot.db.sql,
  {$ifdef USE_SQLITE3}
  mormot.db.sql.sqlite3,
  {$else}
  mormot.db.sql.postgres,
  {$endif USE_SQLITE3}
  mormot.rest.sqlite3,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async;

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
  TOrmWorlds = array of TOrmWorld;
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
    // as used by /rawqueries and /rawupdates
    function GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
  public
    constructor Create(threadCount: integer; flags: THttpServerOptions); reintroduce;
    destructor Destroy; override;
  published
    // all service URI are implemented by these published methods using RTTI
    function plaintext(ctxt: THttpServerRequest): cardinal;
    function json(ctxt: THttpServerRequest): cardinal;
    function db(ctxt: THttpServerRequest): cardinal;
    function queries(ctxt: THttpServerRequest): cardinal;
    function cached_queries(ctxt: THttpServerRequest): cardinal;
    function fortunes(ctxt: THttpServerRequest): cardinal;
    function updates(ctxt: THttpServerRequest): cardinal;
    function rawdb(ctxt: THttpServerRequest): cardinal;
    function rawqueries(ctxt: THttpServerRequest): cardinal;
    function rawfortunes(ctxt: THttpServerRequest): cardinal;
    function rawupdates(ctxt: THttpServerRequest): cardinal;
  end;

const
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';

  WORLD_COUNT      = 10000;
  WORLD_READ_SQL   = 'select id,randomNumber from World where id=?';
  FORTUNES_SQL     = 'select id,message from Fortune';

  FORTUNES_MESSAGE = 'Additional fortune added at request time.';
  FORTUNES_TPL     = '<!DOCTYPE html>' +
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


function ComputeRandomWorld: integer; inline;
begin
  result := Random32(WORLD_COUNT) + 1;
end;

function GetQueriesParamValue(ctxt: THttpServerRequest;
  const search: RawUtf8 = 'QUERIES='): cardinal;
begin
  if not ctxt.UrlParam(search, result) or
     (result = 0) then
    result := 1
  else if result > 500 then
    result := 500;
end;


{ TRawAsyncServer }

constructor TRawAsyncServer.Create(
  threadCount: integer; flags: THttpServerOptions);
begin
  inherited Create;
  // setup the DB connection
  {$ifdef USE_SQLITE3}
  fDbPool := TSqlDBSQLite3ConnectionProperties.Create(
      SQLITE_MEMORY_DATABASE_NAME, '', '', '');
  fDbPool.StatementCacheReplicates := threadcount; // shared SQlite3 connection
  {$else}
  fDbPool := TSqlDBPostgresConnectionProperties.Create(
    'tfb-database:5432', 'hello_world', 'benchmarkdbuser', 'benchmarkdbpass');
  {$endif USE_SQLITE3}
  // customize JSON serializatoin for TFB expectations
  TOrmWorld.OrmProps.Fields.JsonRenameProperties([
    'ID',           'id',
    'RandomNumber', 'randomNumber']);
  TOrmCachedWorld.OrmProps.Fields.JsonRenameProperties([
    'ID',           'id',
    'RandomNumber', 'randomNumber']);
  // setup the ORM data model
  fModel := TOrmModel.Create([TOrmWorld, TOrmFortune, TOrmCachedWorld]);
  OrmMapExternal(fModel, [TOrmWorld, TOrmFortune], fDbPool);
  // CachedWorld table doesn't exists in DB, but should as read in requirements.
  // Use world table as in other implementations.
  OrmMapExternal(fModel, TOrmCachedWorld, fDbPool, 'world');
  // setup the main ORM store
  fStore := TRestServerDB.Create(fModel, SQLITE_MEMORY_DATABASE_NAME);
  fStore.NoAjaxJson := true;
  {$ifdef USE_SQLITE3}
  GenerateDB;
  {$else}
  fStore.Server.CreateMissingTables; // create SQlite3 virtual tables
  {$endif USE_SQLITE3}
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  // initialize the mustache template for /fortunes
  fTemplate := TSynMustache.Parse(FORTUNES_TPL);
  // setup the HTTP server
  fHttpServer := THttpAsyncServer.Create(
    '8080', nil, nil, '', threadCount,
    5 * 60 * 1000,         // 5 minutes keep alive connections
    [hsoNoXPoweredHeader,  // not needed for a benchmark
     hsoHeadersInterning,  // reduce memory contention for /plaintext and /json
     hsoNoStats,           // disable low-level statistic counters
     //hsoThreadCpuAffinity, // worse scaling on multi-servers
     hsoThreadSmooting,    // seems a good option, even if not magical
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TPW General Test Requirements #5
    ] + flags);
  fHttpServer.HttpQueueLength := 100000; // needed e.g. from wrk/ab benchmarks
  // use default routing using RTTI on the TRawAsyncServer published methods
  fHttpServer.Route.RunMethods([urmGet], self);
  // wait for the server to be ready and raise exception e.g. on binding issue
  fHttpServer.WaitStarted;
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
      w.RandomNumber := ComputeRandomWorld;
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

// query DB world table for /rawqueries and /rawupdates endpoints

function TRawAsyncServer.GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  {$ifndef USE_SQLITE3}
  pConn: TSqlDBPostgresConnection absolute conn;
  pStmt: TSqlDBPostgresStatement;
  {$endif USE_SQLITE3}
  i: PtrInt;
begin
  result := false;
  SetLength(res{%H-}, cnt);
  conn := fDbPool.ThreadSafeConnection;
  {$ifdef USE_SQLITE3}
  for i := 0 to cnt - 1 do
  begin
    stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
    stmt.Bind(1, ComputeRandomWorld);
    stmt.ExecutePrepared;
    if not stmt.Step then
      exit;
    res[i].id := stmt.ColumnInt(0);
    res[i].randomNumber := stmt.ColumnInt(1);
  end;
  {$else}
  // specific code to use PostgresSQL pipelining mode
  // see test_nosync in
  // https://github.com/postgres/postgres/blob/master/src/test/modules/libpq_pipeline/libpq_pipeline.c
  if not conn.IsConnected then
    conn.Connect;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  pConn.EnterPipelineMode;
  pStmt := TSqlDBPostgresStatement(stmt.Instance);
  for i := 0 to cnt - 1 do
  begin
    pStmt.Bind(1, ComputeRandomWorld);
    pStmt.SendPipelinePrepared;
    pConn.PipelineSync;
  end;
  for i := 0 to cnt - 1 do
  begin
    pStmt.GetPipelineResult;
    if not stmt.Step then
      exit;
    res[i].id := pStmt.ColumnInt(0);
    res[i].randomNumber := pStmt.ColumnInt(1);
    pStmt.ReleaseRows;
    pConn.CheckPipelineSync;
  end;
  pConn.ExitPipelineMode;
  {$endif USE_SQLITE3}
  result := true;
end;

// following methods implement the server endpoints

function TRawAsyncServer.plaintext(ctxt: THttpServerRequest): cardinal;
begin
  ctxt.OutContent := HELLO_WORLD;
  ctxt.OutContentType := TEXT_CONTENT_TYPE_NO_ENCODING;
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.json(ctxt: THttpServerRequest): cardinal;
var
  msgRec: TMessageRec;
begin
  msgRec.message := HELLO_WORLD;
  ctxt.SetOutJson(@msgRec, TypeInfo(TMessageRec));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.db(ctxt: THttpServerRequest): cardinal;
var
  w: TOrmWorld;
begin
  w := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld);
  try
    ctxt.SetOutJson(w);
    result := HTTP_SUCCESS;
  finally
    w.Free;
  end;
end;

function TRawAsyncServer.queries(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'QUERIES='));
  for i := 0 to length(res) - 1 do
    res[i] := TOrmWorld.Create(fStore.Orm, ComputeRandomWorld);
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  ObjArrayClear(res);
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.cached_queries(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  cache: TOrmCache;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  cache := fStore.Orm.CacheOrNil;
  for i := 0 to length(res) - 1 do
    res[i] := pointer(cache.Get(TOrmCachedWorld, ComputeRandomWorld));
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  result := HTTP_SUCCESS;
end;

function OrmFortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TOrmFortune(A).Message), pointer(TOrmFortune(B).Message));
end;

function TRawAsyncServer.fortunes(ctxt: THttpServerRequest): cardinal;
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

function TRawAsyncServer.updates(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
  w: TOrmWorld;
  b: TRestBatch;
begin
  result := HTTP_SERVERERROR;
  SetLength(res, GetQueriesParamValue(ctxt));
  b := TRestBatch.Create(fStore.ORM, TOrmWorld, {transrows=}0,
    [boExtendedJson, boNoModelEncoding, boPutNoCacheFlush]);
  try
    for i := 0 to length(res) - 1 do
    begin
      w := TOrmWorld.Create;
      res[i] := w;
      if not fStore.Orm.Retrieve(ComputeRandomWorld, w) then
        exit;
      w.RandomNumber := ComputeRandomWorld;
      b.Update(w);
    end;
    result := b.Send;
    if result = HTTP_SUCCESS then
      ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  finally
    b.Free;
    ObjArrayClear(res);
  end;
end;

function TRawAsyncServer.rawdb(ctxt: THttpServerRequest): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  stmt.Bind(1, ComputeRandomWorld);
  stmt.ExecutePrepared;
  if stmt.Step then
  begin
    ctxt.SetOutJson(
      '{"id":%,"randomNumber":%}', [stmt.ColumnInt(0), stmt.ColumnInt(1)]);
    result := HTTP_SUCCESS;
    stmt.ReleaseRows;
  end;
  stmt := nil;
end;

function TRawAsyncServer.rawqueries(ctxt: THttpServerRequest): cardinal;
var
  res: TWorlds;
begin
  if not GetRawRandomWorlds(GetQueriesParamValue(ctxt), res) then
    exit(HTTP_SERVERERROR);
  ctxt.SetOutJson(@res, TypeInfo(TWorlds));
  result := HTTP_SUCCESS;
end;

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequest): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
  list: TFortunes;
  arr: TDynArray;
  n: integer;
  f: ^TFortune;
begin
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
  arr.Init(TypeInfo(TFortunes), list, @n);
  while stmt.Step do
  begin
    f := arr.NewPtr;
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnUtf8(1);
  end;
  f := arr.NewPtr;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Sort(FortuneCompareByMessage);
  ctxt.OutContent := fTemplate.RenderDataArray(arr);
  ctxt.OutContentType := HTML_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

var
  LastComputeUpdateSql: RawUtf8;
  LastComputeUpdateSqlCnt: integer;
  LastComputeUpdateSqlSafe: TLightLock;

function ComputeUpdateSql(cnt: integer): RawUtf8;
var
  i: integer;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  LastComputeUpdateSqlSafe.Lock;
  if cnt <> LastComputeUpdateSqlCnt then
  begin
    // update table set randomNumber = CASE id when ? then ? when ? then ? ...
    // when ? then ? else randomNumber end where id in (?,?,?,?,?)
    // - this weird syntax gives best number for TFB /rawupdates?queries=20 but
    // seems not good for smaller or higher count - we won't include it in the
    // ORM but only for our RAW results - as other frameworks (e.g. ntex) do
    LastComputeUpdateSqlCnt := cnt;
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      W.AddShort('UPDATE world SET randomnumber = CASE id');
      for i := 1 to cnt do
        W.AddShort(' when ? then ?');
      W.AddShort(' else randomNumber end where id in (');
      repeat
        W.Add('?', ',');
        dec(cnt);
      until cnt = 0;
      W.CancelLastComma;
      W.Add(')');
      W.SetText(LastComputeUpdateSql);
    finally
      W.Free;
    end;
  end;
  result := LastComputeUpdateSql;
  LastComputeUpdateSqlSafe.UnLock;
end;

function TRawAsyncServer.rawupdates(ctxt: THttpServerRequest): cardinal;
var
  cnt, i: PtrInt;
  res: TWorlds;
  conn: TSqlDBConnection;
  stmt: ISQLDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, res) then
    exit;
  stmt := conn.NewStatementPrepared(ComputeUpdateSql(cnt), false, true);
  for i := 0 to cnt - 1 do
  begin
    res[i].randomNumber := ComputeRandomWorld;
    stmt.Bind(i * 2 + 1, res[i].id);
    stmt.Bind(i * 2 + 2, res[i].randomNumber);
    stmt.Bind(cnt * 2 + i + 1, res[i].id);
  end;
  stmt.ExecutePrepared;
  ctxt.SetOutJson(@res, TypeInfo(TWorlds));
  result := HTTP_SUCCESS;
end;



var
  rawServers: array of TRawAsyncServer;
  threads, servers, i: integer;
  flags: THttpServerOptions;

procedure ComputeExecutionContextFromParams;
var
  cores: integer;
begin
  // user specified some values at command line: raw [threads] [cores] [servers]
  // in practice, [cores] is just ignored
  if not TryStrToInt(ParamStr(1), threads) then
    threads := SystemInfo.dwNumberOfProcessors * 4;
  if not TryStrToInt(ParamStr(2), cores) then
    cores := 16;
  if not TryStrToInt(ParamStr(3), servers) then
    servers := 1;
  if threads < 2 then
    threads := 2
  else if threads > 256 then
    threads := 256; // max. threads for THttpAsyncServer
  {if SystemInfo.dwNumberOfProcessors > cores then
    SystemInfo.dwNumberOfProcessors := cores; // for hsoThreadCpuAffinity}
  if servers < 1 then
    servers := 1
  else if servers > 256 then
    servers := 256;
end;

procedure ComputeExecutionContextFromNumberOfProcessors;
var
  logicalcores: integer;
begin
  // automatically guess best parameters depending on available CPU cores
  logicalcores := SystemInfo.dwNumberOfProcessors;
  if logicalcores >= 12 then
  begin
    // high-end CPU - scale using several listeners (one per core)
    // see https://synopse.info/forum/viewtopic.php?pid=39263#p39263
    servers := logicalcores;
    threads := 8;
  end
  else
  begin
    // regular CPU - a single instance and a few threads per core
    servers := 1;
    threads := logicalcores * 4;
  end;
end;

begin
  // setup logs
  {$ifdef WITH_LOGS}
  TSynLog.Family.Level := LOG_VERBOSE; // disable logs for benchmarking
  TSynLog.Family.HighResolutionTimestamp := true;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.AutoFlushTimeOut := 1;
  {$else}
  {$ifdef USE_SQLITE3}
  TSynLog.Family.Level := LOG_STACKTRACE; // minimal debug logs on fatal errors
  {$endif USE_SQLITE3}
  SynDBLog := nil; // slightly faster: no need to check log level
  {$endif WITH_LOGS}

  // register some RTTI for records JSON serialization
  Rtti.RegisterFromText([
    TypeInfo(TMessageRec), 'message:RawUtf8',
    TypeInfo(TWorldRec),   'id,randomNumber:integer',
    TypeInfo(TFortune),    'id:integer message:RawUtf8']);

  // setup execution context
  if ParamCount > 1 then
    ComputeExecutionContextFromParams
  else
    ComputeExecutionContextFromNumberOfProcessors;
  if servers > 1 then
    include(flags, hsoReusePort); // allow several bindings on the same port

  // start the server instance(s), in hsoReusePort mode if needed
  SetLength(rawServers, servers);
  for i := 0 to servers - 1 do
    rawServers[i] := TRawAsyncServer.Create(threads, flags);
  try
    // display some information and wait for SIGTERM
    {$I-}
    writeln;
    writeln(rawServers[0].fHttpServer.ClassName,
     ' running on localhost:', rawServers[0].fHttpServer.SockPort);
    writeln(' num thread=', threads,
            ', num CPU=', SystemInfo.dwNumberOfProcessors,
            ', num servers=', servers,
            ', total workers=', threads * servers,
            ', db=', rawServers[0].fDbPool.DbmsEngineName);
    writeln(' options=', GetSetName(TypeInfo(THttpServerOptions), flags));
    writeln('Press [Enter] or Ctrl+C or send SIGTERM to terminate'#10);
    ConsoleWaitForEnterKey;
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    for i := 0 to servers - 1 do
      writeln(ObjectToJsonDebug(rawServers[i].fHttpServer,
        [woDontStoreVoid, woHumanReadable]));
  finally
    // clear all server instance(s)
    ObjArrayClear(rawServers);
  end;
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.

