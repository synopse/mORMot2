
unit ServAppTaskManager;

{ Daemon / composition root for the Task Manager (Recommended Patterns B.6.1).

  This is the ONE place that names concrete infrastructure classes
  (TTaskRepositoryOrm / TTagRepositoryOrm) and wires them to the CQRS services.
  The app/ services depend only on the dom/ interfaces; swapping a backend is an
  infra/ + this-unit change and nothing else. Keeping it under serv/app/ makes
  the persistence boundary visible exactly as the suggested layout prescribes.

  Topology (A.6.2, collapsed into a single executable):
   - Persistence = TRestServerDB owns SQLite + FTS5, stays OFF the network.
   - Dispatcher  = TRestServerFullMemory (void model) hosts the CQRS services
     and is the only server handed to TRestHttpServer. }

{$I mormot.defines.inc}

interface

/// Register the SOA interfaces, build the two-server topology, wire the
/// repositories into the CQRS services, run the test suite and (unless
/// `--test` was passed) start the public HTTP listener.
procedure RunTaskManagerDaemon;

implementation

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.core.test,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.net.server,
  mormot.net.http,
  mormot.db.raw.sqlite3.static,
  // Shared types / config
  shared_types,
  app_config,
  app_settings,
  // Task feature module
  task,                   // dom/tasks (TOrm aggregate)
  task_dtos,              // app/tasks
  task_mappers,           // app/tasks
  task_repository,        // dom/tasks
  task_repository_orm,    // infra/tasks
  task_query,             // dom/tasks
  task_command,           // dom/tasks
  task_query_impl,        // app/tasks
  task_command_impl,      // app/tasks
  task_tests,             // tests/tasks
  // Tag feature module
  tag,                    // dom/tags (TOrm aggregate)
  tag_dtos,               // app/tags
  tag_mappers,            // app/tags
  tag_repository,         // dom/tags
  tag_repository_orm,     // infra/tags
  tag_query,              // dom/tags
  tag_command,            // dom/tags
  tag_query_impl,         // app/tags
  tag_command_impl,       // app/tags
  tag_tests;              // tests/tags

type
  TStaticFileServer = class
  public
    Folder: TFileName;
    function ServeFile(Ctxt: THttpServerRequestAbstract): cardinal;
  end;

var
  Settings: TTaskManagerSettings;
  // Persistence: TRestServerDB owns the SQLite + FTS5 tables. Lives in-process
  // only — it is NOT added to the public TRestHttpServer. Repositories hold
  // its IRestOrm so all data access stays inside the daemon.
  PersistenceModel: TOrmModel;
  Persistence: TRestServerDB;
  // Public dispatcher: TRestServerFullMemory hosts the CQRS services. Its
  // OrmModel is empty (services-only). Services resolve their ITaskRepository
  // / ITagRepository from the concrete instances seeded into the dispatcher's
  // own DI resolver via InjectInstance (see below).
  DispatcherModel: TOrmModel;
  Dispatcher: TRestServerFullMemory;
  HttpServer: TRestHttpServer;
  StaticServer: TStaticFileServer;
  // Concrete repository instances; kept alive for the daemon's lifetime.
  TaskRepoImpl: TTaskRepositoryOrm;
  TagRepoImpl: TTagRepositoryOrm;

function TStaticFileServer.ServeFile(Ctxt: THttpServerRequestAbstract): cardinal;
var
  FileName: TFileName;
  RequestPath: RawUtf8;
begin
  RequestPath := Ctxt.Url;
  if IdemPChar(pointer(RequestPath), '/STATIC/') then
    Delete(RequestPath, 1, 8)
  else if IdemPChar(pointer(RequestPath), '/STATIC') then
    RequestPath := 'index.html';

  if (RequestPath = '') or (RequestPath = '/') then
    RequestPath := 'index.html';

  FileName := Folder + Utf8ToString(StringReplaceChars(RequestPath, '/', PathDelim));

  if FileExists(FileName) then
  begin
    Ctxt.OutContentType := STATICFILE_CONTENT_TYPE;
    Ctxt.OutContent := StringToUtf8(FileName);
    Result := HTTP_SUCCESS;
  end
  else
  begin
    Ctxt.OutContentType := TEXT_CONTENT_TYPE;
    Ctxt.OutContent := 'File not found: ' + RequestPath;
    Result := HTTP_NOTFOUND;
  end;
end;

procedure CreateSampleData;
var
  Task: TTask;
  Tag: TTag;
  TagID1, TagID2, TagID3: TID;
  Comment: TTaskComment;
  TagArr: TIDDynArray;
  CommentArr: TTaskCommentDynArray;
  TagRepo: ITagRepository;
  TaskRepo: ITaskRepository;
begin
  WriteLn('Creating sample data...');
  // Use the typed interfaces so the seed code goes through the same port.
  TagRepo := TagRepoImpl;
  TaskRepo := TaskRepoImpl;

  Tag := TTag.Create;
  try
    Tag.Name := 'urgent';
    Tag.Color := '#FF3333';
    Tag.CreatedAt := NowUtc;
    Tag.SchemaVersion := TAG_CURRENT_VERSION;
    TagID1 := TagRepo.Add(Tag);
  finally
    Tag.Free;
  end;

  Tag := TTag.Create;
  try
    Tag.Name := 'work';
    Tag.Color := '#3498DB';
    Tag.CreatedAt := NowUtc;
    Tag.SchemaVersion := TAG_CURRENT_VERSION;
    TagID2 := TagRepo.Add(Tag);
  finally
    Tag.Free;
  end;

  Tag := TTag.Create;
  try
    Tag.Name := 'personal';
    Tag.Color := '#2ECC71';
    Tag.CreatedAt := NowUtc;
    Tag.SchemaVersion := TAG_CURRENT_VERSION;
    TagID3 := TagRepo.Add(Tag);
  finally
    Tag.Free;
  end;

  Task := TTask.Create;
  try
    Task.Title := 'Welcome to Task Manager';
    Task.Description := 'This is a sample task created automatically';
    Task.Priority := 2;
    Task.DueDate := NowUtc + 7;
    Task.Status := 'pending';
    Task.IsCompleted := false;
    Task.CreatedAt := NowUtc;
    Task.UpdatedAt := NowUtc;
    Task.SchemaVersion := TASK_CURRENT_VERSION;
    SetLength(TagArr, 1);
    TagArr[0] := TagID3;
    Task.TagIDs := TagArr;
    TaskRepo.Add(Task);
  finally
    Task.Free;
  end;

  Task := TTask.Create;
  try
    Task.Title := 'Learn mORMot2 SOA Architecture';
    Task.Description := 'Study CQRS, DDD Aggregates, and interface-based services';
    Task.Priority := 3;
    Task.DueDate := NowUtc + 3;
    Task.Status := 'in_progress';
    Task.IsCompleted := false;
    Task.CreatedAt := NowUtc;
    Task.UpdatedAt := NowUtc;
    Task.SchemaVersion := TASK_CURRENT_VERSION;
    SetLength(TagArr, 1);
    TagArr[0] := TagID2;
    Task.TagIDs := TagArr;
    Comment.Content := 'Started reading the documentation';
    Comment.Author := 'Developer';
    Comment.CreatedAt := NowUtc;
    Comment.UpdatedAt := NowUtc;
    Comment.IsEdited := false;
    SetLength(CommentArr, 1);
    CommentArr[0] := Comment;
    Task.Comments := CommentArr;
    TaskRepo.Add(Task);
  finally
    Task.Free;
  end;

  Task := TTask.Create;
  try
    Task.Title := 'Implement Aggregate Pattern';
    Task.Description := 'Refactor entities to use DDD aggregates with embedded sub-structures';
    Task.Priority := 3;
    Task.DueDate := NowUtc + 1;
    Task.Status := 'completed';
    Task.IsCompleted := true;
    Task.CreatedAt := NowUtc - 2;
    Task.UpdatedAt := NowUtc;
    Task.SchemaVersion := TASK_CURRENT_VERSION;
    SetLength(TagArr, 2);
    TagArr[0] := TagID2;
    TagArr[1] := TagID1;
    Task.TagIDs := TagArr;
    TaskRepo.Add(Task);
  finally
    Task.Free;
  end;

  WriteLn('Sample data created');
end;

type
  TTaskManagerTests = class(TSynTests)
  published
    procedure TaskFeatureModule;
    procedure TagFeatureModule;
  end;

procedure TTaskManagerTests.TaskFeatureModule;
begin
  AddCase([TTestTask]);
end;

procedure TTaskManagerTests.TagFeatureModule;
begin
  AddCase([TTestTag]);
end;

procedure RunTests;
var
  Tests: TTaskManagerTests;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Running TSynTestCase Tests');
  WriteLn('========================================');
  WriteLn('');

  // Tests seed data via the persistence ORM and resolve services via the
  // public dispatcher (same split as the live daemon).
  SetTestServers(Persistence, Dispatcher);
  SetTagTestServers(Persistence, Dispatcher);

  Tests := TTaskManagerTests.Create('Task Manager SOA Tests');
  try
    Tests.Run;
  finally
    Tests.Free;
  end;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('Tests Completed');
  WriteLn('========================================');
  WriteLn('');
end;

procedure Run;
var
  DataFolder: string;
  DBFileName: string;
  ListenURI: RawUtf8;
  TestOnly: boolean;
begin
  // Headless mode: run the test suite and exit without opening the HTTP
  // listener (useful for CI / sandboxes where binding a socket is restricted).
  TestOnly := (ParamCount >= 1) and (ParamStr(1) = '--test');

  WriteLn('mORMot2 Task Manager — SOA Architecture');
  WriteLn('========================================');
  WriteLn('');

  Settings := TTaskManagerSettings.Create;
  try
    DataFolder := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'data';
    ForceDirectories(DataFolder);
    DBFileName := DataFolder + PathDelim + Settings.DBFileName;
    WriteLn('Database: ', DBFileName);
    WriteLn('Root:     ', Settings.Root);
    WriteLn('Port:     ', Settings.HttpPort);

    // ----- Persistence server (internal) -----
    // TOrmMonitorUsage stores the framework's aggregated SOA statistics
    // (see the TSynMonitorUsageRest wiring below).
    PersistenceModel := TOrmModel.Create([TTask, TTag, TTaskFts5, TOrmMonitorUsage],
      Settings.Root);
    try
      Persistence := TRestServerDB.Create(PersistenceModel, DBFileName);
      try
        Persistence.CreateMissingTables;
        WriteLn('Persistence ready');

        // ----- Composition root -----
        // Repositories wrap the persistence ORM. They are seeded into the
        // dispatcher's own resolver (InjectInstance, Recommended Patterns B.5)
        // so TInjectableObjectRest.AutoResolve injects them into the CQRS
        // services — a per-composition dependency list, not global mutable
        // state. The container ref-counts them and releases them on Free.
        TaskRepoImpl := TTaskRepositoryOrm.Create(Persistence.Orm);
        TagRepoImpl  := TTagRepositoryOrm.Create(Persistence.Orm);

        // ----- Public dispatcher (services-only, no DB) -----
        DispatcherModel := TOrmModel.Create([], Settings.Root);
        Dispatcher := TRestServerFullMemory.Create(DispatcherModel);
        try
          // Seed the dispatcher's DI resolver with the repository ports
          // (InjectInstance, Recommended Patterns B.5) — a per-composition
          // dependency list, not global mutable state. ServiceContainer
          // materializes the resolver; this must happen before ServiceDefine,
          // which eagerly creates each sicShared instance and runs AutoResolve.
          Dispatcher.ServiceContainer.InjectInstance([TaskRepoImpl, TagRepoImpl]);

          WriteLn('Registering CQRS services on dispatcher (sicShared)...');
          Dispatcher.ServiceDefine(TTaskQueryService, [ITaskQuery], sicShared);
          Dispatcher.ServiceDefine(TTaskCommandService, [ITaskCommand], sicShared);
          Dispatcher.ServiceDefine(TTagQueryService, [ITagQuery], sicShared);
          Dispatcher.ServiceDefine(TTagCommandService, [ITagCommand], sicShared);
          WriteLn('Services registered: ITaskQuery, ITaskCommand, ITagQuery, ITagCommand');

          // ----- Built-in SOA statistics -----
          // The framework already collects per-method statistics for every
          // interface-based service (mlInterfaces is in TRestServer.StatLevels
          // by default): call counts, timing, input/output sizes, error counts.
          // No instrumentation code is needed in the service classes.
          // TSynMonitorUsageRest additionally aggregates those counters per
          // time period (hour/day/month/year) into the TOrmMonitorUsage table
          // of the SQLite database. It is released via `StatUsage := nil` in
          // the finally block below, which saves pending data — and must
          // happen while Persistence (its storage) is still alive.
          Dispatcher.StatUsage := TSynMonitorUsageRest.Create(Persistence.Orm, 0);

          if Persistence.Orm.TableRowCount(TTask) = 0 then
            CreateSampleData;

          RunTests;

          if TestOnly then
            WriteLn('Test-only mode (--test): HTTP server skipped.')
          else
          begin
          WriteLn('Starting HTTP server...');

          StaticServer := TStaticFileServer.Create;
          StaticServer.Folder := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'static' + PathDelim;

          ListenURI := Settings.HttpPort;
          HttpServer := TRestHttpServer.Create(
            ListenURI,
            [Dispatcher],
            '+',
            useBidirSocket
          );
          try
            HttpServer.AccessControlAllowOrigin := '*';
            HttpServer.Route.Get('/static/<path:path>', StaticServer.ServeFile);
            HttpServer.Route.Get('/static', StaticServer.ServeFile);

            WriteLn('');
            WriteLn('Server running!');
            WriteLn('');
            WriteLn('CQRS Services (SOA endpoints):');
            WriteLn('  http://localhost:', ListenURI, '/', Settings.Root, '/TaskQuery');
            WriteLn('  http://localhost:', ListenURI, '/', Settings.Root, '/TaskCommand');
            WriteLn('  http://localhost:', ListenURI, '/', Settings.Root, '/TagQuery');
            WriteLn('  http://localhost:', ListenURI, '/', Settings.Root, '/TagCommand');
            WriteLn('');
            WriteLn('Statistics (built-in, per service method):');
            WriteLn('  http://localhost:', ListenURI, '/', Settings.Root, '/stat?withall=1');
            WriteLn('');
            WriteLn('Web Interface:');
            WriteLn('  http://localhost:', ListenURI, '/static/index.html');
            WriteLn('');
            WriteLn('Press [Enter] to quit');
            WriteLn('');

            ReadLn;
            WriteLn('Shutting down...');
          finally
            HttpServer.Free;
            StaticServer.Free;
          end;
          end;
        finally
          // Assigning nil frees the TSynMonitorUsageRest, saving pending
          // statistics into TOrmMonitorUsage. TRestServer does NOT free
          // StatUsage in its destructor, and the save needs Persistence
          // (the storage target) to still be alive — so release it here,
          // before both servers go down.
          Dispatcher.StatUsage := nil;
          // Dispatcher.Free releases its DI resolver, which ref-count-frees the
          // repositories seeded via InjectInstance — before Persistence (whose
          // IRestOrm they hold) is freed below.
          Dispatcher.Free;
          DispatcherModel.Free;
        end;
      finally
        Persistence.Free;
      end;
    finally
      PersistenceModel.Free;
    end;
  finally
    Settings.Free;
  end;
end;

procedure RunTaskManagerDaemon;
begin
  EnableSynLogs;

  // RPC-exposed CQRS interfaces. ITaskRepository / ITagRepository are NOT
  // registered here — they are local DI ports and their IList<T> results
  // are not part of the SOA wire contract.
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ITaskQuery),
    TypeInfo(ITaskCommand),
    TypeInfo(ITagQuery),
    TypeInfo(ITagCommand)
  ]);

  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ReadLn;
    end;
  end;
end;

end.
