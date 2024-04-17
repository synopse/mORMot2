program raw;

{
 ----------------------------------------------------
   TechEmpower Framework Benchmarks implementation
   in modern pascal and the mORMot 2 framework
 ----------------------------------------------------
 https://github.com/TechEmpower/FrameworkBenchmarks/wiki
 command line optional syntax: run "raw -?"
}

{$I mormot.defines.inc}

{.$define USE_SQLITE3}
// may be defined to use a SQLite3 database instead of external PostgresSQL DB
// - note that /rawfortunes and /async* won't work

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
  mormot.orm.base,
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
    message: PUtf8Char;
  end;
  TWorldRec = packed record
    id: integer;
    randomNumber: integer;
  end;
  TWorlds = array of TWorldRec;
  TFortune = packed record
    id: PtrUInt;
    message: PUtf8Char;
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
  protected
    fHttpServer: THttpAsyncServer;
    fModel: TOrmModel;
    fStore: TRestServerDB;
    fTemplate: TSynMustache;
    fCachedWorldsTable: POrmCacheTable;
    fRawCache: TOrmWorlds;
    {$ifdef USE_SQLITE3}
    fDbPool: TSqlDBSQLite3ConnectionProperties;
    procedure GenerateDB;
    {$else}
    fDbPool: TSqlDBPostgresConnectionProperties;
    procedure OnAsyncDb(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    procedure OnAsyncFortunes(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    {$endif USE_SQLITE3}
    // pipelined reading as used by /rawqueries and /rawupdates
    function GetRawRandomWorlds(cnt: PtrInt; out res: TWorlds): boolean;
    function ComputeRawFortunes(stmt: TSqlDBStatement; ctxt: THttpServerRequest): integer;
  public
    constructor Create(threadCount: integer; flags: THttpServerOptions;
      pin2Core: integer); reintroduce;
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
    function rawcached(ctxt: THttpServerRequest): cardinal;
    function rawfortunes(ctxt: THttpServerRequest): cardinal;
    function rawupdates(ctxt: THttpServerRequest): cardinal;
    {$ifndef USE_SQLITE3} // asynchronous PostgreSQL pipelined DB access
    function asyncdb(ctxt: THttpServerRequest): cardinal;
    function asyncqueries(ctxt: THttpServerRequest): cardinal;
    function asyncfortunes(ctxt: THttpServerRequest): cardinal;
    function asyncupdates(ctxt: THttpServerRequest): cardinal;
    {$endif USE_SQLITE3}
  end;

{$I-}

const
  HELLO_WORLD: RawUtf8 = 'Hello, World!';
  TEXT_CONTENT_TYPE_NO_ENCODING: RawUtf8 = 'text/plain';

  WORLD_COUNT       = 10000;
  WORLD_READ_SQL    = 'select id,randomNumber from World where id=?';
  WORLD_UPDATE_SQLN = 'update World as t set randomNumber = v.r from ' +
    '(SELECT unnest(?::integer[]), unnest(?::integer[]) order by 1) as v(id, r)' +
    ' where t.id = v.id';
  FORTUNES_SQL      = 'select id,message from Fortune';

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
  const search: RawUtf8 = 'QUERIES='): cardinal; inline;
begin
  if not ctxt.UrlParam(search, result) or
     (result = 0) then
    result := 1
  else if result > 500 then
    result := 500;
end;


{ TRawAsyncServer }

constructor TRawAsyncServer.Create(
  threadCount: integer; flags: THttpServerOptions; pin2Core: integer);
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
  // fDbPool.ArrayParamsAsBinary := true; // seems not really faster
  {$endif USE_SQLITE3}
  // customize JSON serialization for TFB expectations
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
  // pre-fill the ORM
  if fStore.Server.Cache.SetCache(TOrmCachedWorld) then
    fStore.Server.Cache.FillFromQuery(TOrmCachedWorld, '', []);
  fCachedWorldsTable := fStore.Orm.Cache.Table(TOrmCachedWorld);
  fStore.Orm.RetrieveListObjArray(fRawCache, TOrmCachedWorld, 'order by id', []);
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
     hsoEnablePipelining,  // as expected by /plaintext
     {$ifdef WITH_LOGS}
     hsoLogVerbose,
     {$endif WITH_LOGS}
     hsoIncludeDateHeader  // required by TFB General Test Requirements #5
    ] + flags);
  if pin2Core <> -1 then
    fHttpServer.Async.SetCpuAffinity(pin2Core);
  fHttpServer.HttpQueueLength := 10000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.ServerName := 'M';
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
  fDBPool.Free;
  ObjArrayClear(fRawCache);
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
  stmt: ISqlDBStatement;
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
  // specific code to use PostgresSQL pipelining mode in the main connection thread
  stmt := conn.NewStatementPrepared(WORLD_READ_SQL, true, true);
  pConn.EnterPipelineMode;
  pStmt := TSqlDBPostgresStatement(stmt.Instance);
  for i := 0 to cnt - 1 do
  begin
    pStmt.Bind(1, ComputeRandomWorld);
    pStmt.SendPipelinePrepared;
    pConn.PipelineSync; // mandatory in TFB requirements (but not realistic)
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

function FortuneCompareByMessage(const A, B): integer;
begin
  result := StrComp(pointer(TFortune(A).message), pointer(TFortune(B).message));
end;

function TRawAsyncServer.ComputeRawFortunes(
  stmt: TSqlDBStatement; ctxt: THttpServerRequest): integer;
var
  list: TFortunes;
  arr: TDynArray;
  n: integer;
  f: ^TFortune;
  mus: TSynMustacheContextData;
begin
  result := HTTP_BADREQUEST;
  {$ifndef USE_SQLITE3}
  if stmt = nil then
  {$endif USE_SQLITE3}
    // stmt.ColumnPUtf8 and stmt.Connection.GetThreadOwned won't work on SQLite3
    exit;
  arr.Init(TypeInfo(TFortunes), list, @n);
  while stmt.Step do
  begin
    f := arr.NewPtr;
    f.id := stmt.ColumnInt(0);
    f.message := stmt.ColumnPUtf8(1);
  end;
  f := arr.NewPtr;
  f.id := 0;
  f.message := FORTUNES_MESSAGE;
  arr.Sort(FortuneCompareByMessage);
  mus := stmt.Connection.GetThreadOwned(TSynMustacheContextData);
  if mus = nil then
    mus := stmt.Connection.SetThreadOwned(fTemplate.NewMustacheContextData);
  ctxt.OutContent := mus.RenderArray(arr);
  ctxt.OutContentType := HTML_CONTENT_TYPE;
  result := HTTP_SUCCESS;
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
  msgRec.message := pointer(HELLO_WORLD);
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
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  for i := 0 to length(res) - 1 do
    res[i] := fCachedWorldsTable.Get(ComputeRandomWorld);
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
  stmt: ISqlDBStatement;
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

function TRawAsyncServer.rawcached(ctxt: THttpServerRequest): cardinal;
var
  i: PtrInt;
  res: TOrmWorlds;
begin
  SetLength(res, GetQueriesParamValue(ctxt, 'COUNT='));
  for i := 0 to length(res) - 1 do
    res[i] := fRawCache[ComputeRandomWorld - 1];
  ctxt.SetOutJson(@res, TypeInfo(TOrmWorlds));
  result := HTTP_SUCCESS;
end;

function TRawAsyncServer.rawfortunes(ctxt: THttpServerRequest): cardinal;
var
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  conn := fDbPool.ThreadSafeConnection;
  stmt := conn.NewStatementPrepared(FORTUNES_SQL, true, true);
  stmt.ExecutePrepared;
  result := ComputeRawFortunes(stmt.Instance, ctxt);
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
    // update table set .. from values (), (), ... where id = id
    // we won't include it in the ORM but only for our RAW results
    LastComputeUpdateSqlCnt := cnt;
    W := TTextWriter.CreateOwnedStream(tmp{%H-});
    try
      W.AddShort('UPDATE world SET randomNumber = v.randomNumber FROM (VALUES');
      for i := 1 to cnt do
      begin
        W.AddShort('(?::integer, ?::integer)');
        W.Add(',');
      end;
      W.CancelLastComma;
      W.AddShort(' order by 1) AS v (id, randomNumber) WHERE world.id = v.id');
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
  params: TInt64DynArray;
  conn: TSqlDBConnection;
  stmt: ISqlDBStatement;
begin
  result := HTTP_SERVERERROR;
  conn := fDbPool.ThreadSafeConnection;
  cnt := getQueriesParamValue(ctxt);
  if not getRawRandomWorlds(cnt, res) then
    exit;
  // generate new randoms
  for i := 0 to cnt - 1 do
    res[i].randomNumber := ComputeRandomWorld;
  if cnt > 20 then
  begin
    // fill parameters arrays for update with nested select (PostgreSQL only)
    stmt := conn.NewStatementPrepared(WORLD_UPDATE_SQLN, false, true);
    SetLength(params{%H-}, cnt);
    for i := 0 to cnt - 1 do
      params[i] := res[i].id;
    stmt.BindArray(1, params);
    for i := 0 to cnt - 1 do
      params[i] := res[i].randomNumber;
    stmt.BindArray(2, params);
  end
  else
  begin
    // fill parameters for update up to 20 items as VALUES(?,?),(?,?),...
    stmt := conn.NewStatementPrepared(ComputeUpdateSql(cnt), false, true);
    for i := 0 to cnt - 1 do
    begin
      stmt.Bind(i * 2 + 1, res[i].id);
      stmt.Bind(i * 2 + 2, res[i].randomNumber);
    end;
  end;
  stmt.ExecutePrepared;
  ctxt.SetOutJson(@res, TypeInfo(TWorlds));
  result := HTTP_SUCCESS;
end;

{$ifndef USE_SQLITE3} // asynchronous PostgreSQL pipelined DB access

const
  // follow TFB requirements, and potential patched libpq
  ASYNC_OPT = [asoForceConnectionFlush, asoForcePipelineSync];

function TRawAsyncServer.asyncdb(ctxt: THttpServerRequest): cardinal;
begin
  with fDbPool.Async.PrepareLocked(WORLD_READ_SQL, {res=}true, ASYNC_OPT) do
  try
    Bind(1, ComputeRandomWorld);
    ExecuteAsync(ctxt, OnAsyncDb);
  finally
    UnLock;
  end;
  result := ctxt.SetAsyncResponse;
end;

procedure TRawAsyncServer.OnAsyncDb(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
var
  ctxt: THttpServerRequest absolute Context;
begin
  if (Statement = nil) or
     not Statement.Step then
    ctxt.ErrorMessage := 'asyncdb failed'
  else
    ctxt.SetOutJson('{"id":%,"randomNumber":%}',
      [Statement.ColumnInt(0), Statement.ColumnInt(1)]);
  ctxt.OnAsyncResponse(ctxt);
end;

function TRawAsyncServer.asyncfortunes(ctxt: THttpServerRequest): cardinal;
begin
  fDbPool.Async.PrepareLocked(FORTUNES_SQL, {res=}true, ASYNC_OPT).
    ExecuteAsyncNoParam(ctxt, OnAsyncFortunes);
  result := ctxt.SetAsyncResponse;
end;

procedure TRawAsyncServer.OnAsyncFortunes(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
var
  ctxt: THttpServerRequest absolute Context;
begin
  ctxt.OnAsyncResponse(ctxt, ComputeRawFortunes(Statement, ctxt));
end;

type
  // simple state machine used for /asyncqueries and /asyncupdates
  TAsyncWorld = class
  public
    request: THttpServerRequest;
    res: TWorlds;
    count, current: integer;
    update: TSqlDBPostgresAsyncStatement; // prepared before any callback
    function Queries(async: TSqlDBPostgresAsync; ctxt: THttpServerRequest): cardinal;
    function Updates(async: TSqlDBPostgresAsync; ctxt: THttpServerRequest): cardinal;
    procedure DoUpdates;
    procedure OnQueries(Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
    procedure OnRes({%H-}Statement: TSqlDBPostgresAsyncStatement; Context: TObject);
  end;

function TRawAsyncServer.asyncqueries(ctxt: THttpServerRequest): cardinal;
begin
  result := TAsyncWorld.Create.Queries(fDBPool.Async, ctxt);
end;

function TRawAsyncServer.asyncupdates(ctxt: THttpServerRequest): cardinal;
begin
  result := TAsyncWorld.Create.Updates(fDBPool.Async, ctxt);
end;


{ TAsyncWorld }

function TAsyncWorld.Queries(async: TSqlDBPostgresAsync; ctxt: THttpServerRequest): cardinal;
var
  n: integer;
  opt: TSqlDBPostgresAsyncStatementOptions; // for modified libpq
begin
  request := ctxt;
  if count = 0 then
    count := getQueriesParamValue(ctxt);
  SetLength(res, count); // count is > 0
  with async.PrepareLocked(WORLD_READ_SQL, {res=}true, ASYNC_OPT) do
  try
    opt := AsyncOptions - [asoForceConnectionFlush];
    n := count;
    repeat
      dec(n);
      Bind(1, ComputeRandomWorld);
      if n = 0 then // last item should include asoForceConnectionFlush (if set)
        opt := AsyncOptions;
      ExecuteAsync(ctxt, OnQueries, @opt);
    until n = 0;
  finally
    UnLock;
  end;
  result := ctxt.SetAsyncResponse;
end;

function TAsyncWorld.Updates(async: TSqlDBPostgresAsync; ctxt: THttpServerRequest): cardinal;
begin
  count := getQueriesParamValue(ctxt);
  update := async.Prepare(WORLD_UPDATE_SQLN, false, ASYNC_OPT);
  result := Queries(async, ctxt);
end;

procedure TAsyncWorld.OnQueries(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
begin
  if (Statement <> nil) and
     Statement.Step then
    with res[current] do
    begin
      id := Statement.ColumnInt(0);
      randomNumber := Statement.ColumnInt(1);
    end;
  inc(current);
  if current = count then // we retrieved all SELECT
    if Assigned(update) then
      DoUpdates
    else
      OnRes(Statement, Context);
end;

procedure TAsyncWorld.DoUpdates;
var
  i: PtrInt;
  params: TIntegerDynArray;
begin
  for i := 0 to count - 1 do
    res[i].randomNumber := ComputeRandomWorld;
  SetLength(params, count);
  for i := 0 to count - 1 do
    params[i] := res[i].id;
  update.BindArrayInt32(1, params);
  for i := 0 to count - 1 do
    params[i] := res[i].randomNumber;
  update.BindArrayInt32(2, params);
  update.ExecuteAsync(request, OnRes);
end;

procedure TAsyncWorld.OnRes(Statement: TSqlDBPostgresAsyncStatement;
  Context: TObject);
begin
  request.SetOutJson(@res, TypeInfo(TWorlds));
  request.OnAsyncResponse(Context as THttpServerRequest);
  Free; // we don't need this state machine any more
end;

{$endif USE_SQLITE3}


var
  rawServers: array of TRawAsyncServer;
  threads, servers, i, k, cpuIdx, cpuCount: integer;
  pinServers2Cores: boolean;
  cpuMask: TCpuSet;
  flags: THttpServerOptions;
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
    TypeInfo(TMessageRec), 'message:PUtf8Char',
    TypeInfo(TWorldRec),   'id,randomNumber:cardinal',
    TypeInfo(TFortune),    'id:PtrUInt message:PUtf8Char']);

  // compute default execution context from HW information
  cpuCount := CurrentCpuSet(cpuMask); // may run from a "taskset" command
  if GetEnvironmentVariable('TFB_TEST_NAME') = 'mormot-postgres-async' then
  begin
    // asynchronous tests do not require several listeners
    servers := 1;
    threads := cpucount * 4;
    pinServers2Cores := false;
  end
  else if cpuCount >= 6 then
  begin
    // high-end CPU would scale better using several listeners (one per core)
    // see https://synopse.info/forum/viewtopic.php?pid=39263#p39263
    servers := cpuCount;
    threads := 8;
    pinServers2Cores := true;
  end
  else
  begin
    // simple CPU will have a single instance and a few threads per core
    servers := 1;
    threads := cpuCount * 4;
    pinServers2Cores := false;
  end;

  // parse command line parameters
  with Executable.Command do
  begin
    ExeDescription := 'TFB Server using mORMot 2';
    if Option(['p', 'pin'], 'pin each server to a CPU') then
      pinServers2Cores := true;
    if Option('nopin', 'disable the CPU pinning') then
      pinServers2Cores := false; // no option would keep the default boolean
    Get(['s', 'servers'], servers, '#count of servers (listener sockets)', servers);
    Get(['t', 'threads'], threads, 'per-server thread pool #size', threads);
    if Option(['?', 'help'], 'display this message') then
    begin
      ConsoleWrite(FullDescription);
      exit;
    end;
    if ConsoleWriteUnknown then
      exit;
  end;

  // start the server instance(s), in hsoReusePort mode if needed
  flags := [];
  if servers > 1 then
    include(flags, hsoReusePort) // allow several bindings on the same port
  else
    pinServers2Cores := false;   // pinning a single server won't make any sense
  SetLength(rawServers{%H-}, servers);
  cpuIdx := -1; // do not pin to CPU by default
  for i := 0 to servers - 1 do
  begin
    if pinServers2Cores then
    begin
      k := i mod cpuCount;
      cpuIdx := -1;
      // find real CPU index according to the cpuMask
      repeat
        inc(cpuIdx);
        if GetBit(cpuMask, cpuIdx) then
          dec(k);
      until k = -1;
      ConsoleWrite(['Pin #', i, ' server to #', cpuIdx, ' CPU']);
    end;
    rawServers[i] := TRawAsyncServer.Create(threads, flags, cpuIdx)
  end;

  try
    // display some information and wait for SIGTERM
    ConsoleWrite([CRLF, rawServers[0].fHttpServer.ClassName,
      ' running on localhost:', rawServers[0].fHttpServer.SockPort], ccWhite);
    ConsoleWrite([' num servers=',   servers,
      ', threads per server=', threads,
      ', total threads=',      threads * servers,
      ', total CPU=',          SystemInfo.dwNumberOfProcessors,
      ', accessible CPU=',     cpuCount,
      ', pinned=',             pinServers2Cores,
      ', db=',                 rawServers[0].fDbPool.DbmsEngineName, CRLF,
      ' options=', GetSetName(TypeInfo(THttpServerOptions), flags), CRLF]);
    ConsoleWrite('Press [Enter] or Ctrl+C or send SIGTERM to terminate', ccWhite);
    ConsoleWaitForEnterKey;
    //TSynLog.Family.Level := LOG_VERBOSE; // enable shutdown logs for debug
    if servers = 1 then
      ConsoleObject(rawServers[0].fHttpServer)
    else
    begin
      ConsoleWrite('Per-server accepted connections:');
      for i := 0 to servers - 1 do
        ConsoleWrite([' ', rawServers[i].fHttpServer.Async.Accepted], ccLightGray, true);
      ConsoleWrite([#10'Please wait: Shutdown ', servers, ' servers and ',
        threads * servers, ' threads']);
    end;
  finally
    // clear all server instance(s)
    ObjArrayClear(rawServers);
  end;
  ConsoleWrite('Shutdown complete'#10);
  {$ifdef FPC_X64MM}
  WriteHeapStatus(' ', 16, 8, {compileflags=}true);
  {$endif FPC_X64MM}
end.


