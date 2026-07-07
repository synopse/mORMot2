
unit AppTaskManagerClientLocal;

{ In-process backend (Recommended Patterns B.6.1: App<Name>ClientLocal.pas / A.6.1).

  Builds the same two-server topology as the daemon, but inside the consumer's
  own process: a TRestServerDB owns the SQLite + FTS5 tables, the repositories
  are seeded into the dispatcher's DI resolver (InjectInstance), and a void-model
  TRestServerFullMemory hosts the CQRS services. CreateClient resolves the four
  ports from that dispatcher and hands
  them back behind ITaskManagerClient; the returned object owns the whole
  topology and tears it down on release. }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.memserver,
  mormot.soa.core,
  mormot.soa.server,
  mormot.db.raw.sqlite3.static,
  app_settings,
  task,
  task_repository,
  task_repository_orm,
  task_query,
  task_command,
  task_query_impl,
  task_command_impl,
  tag,
  tag_repository,
  tag_repository_orm,
  tag_query,
  tag_command,
  tag_query_impl,
  tag_command_impl,
  AppTaskManagerClient;

/// Build an in-process client with an embedded SQLite database resolved from
/// aSettings. RegisterTaskManagerInterfaces must have been called first.
function CreateClient(aSettings: TTaskManagerSettings): ITaskManagerClient;

implementation

type
  TTaskManagerClientLocal = class(TInterfacedObject, ITaskManagerClient)
  private
    fPersistenceModel: TOrmModel;
    fPersistence: TRestServerDB;
    fDispatcherModel: TOrmModel;
    fDispatcher: TRestServerFullMemory;
    fTaskRepo: TTaskRepositoryOrm;
    fTagRepo: TTagRepositoryOrm;
    fTaskCmd: ITaskCommand;
    fTaskQry: ITaskQuery;
    fTagCmd: ITagCommand;
    fTagQry: ITagQuery;
  public
    constructor Create(aSettings: TTaskManagerSettings);
    destructor Destroy; override;
    function TaskCommand: ITaskCommand;
    function TaskQuery: ITaskQuery;
    function TagCommand: ITagCommand;
    function TagQuery: ITagQuery;
  end;

constructor TTaskManagerClientLocal.Create(aSettings: TTaskManagerSettings);
var
  DataFolder, DBFileName: string;
begin
  inherited Create;
  DataFolder := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + 'data';
  DBFileName := DataFolder + PathDelim + aSettings.DBFileName;
  ForceDirectories(DataFolder);

  // ----- Persistence server (internal, off the network) -----
  fPersistenceModel := TOrmModel.Create([TTask, TTag, TTaskFts5], aSettings.Root);
  fPersistence := TRestServerDB.Create(fPersistenceModel, DBFileName);
  fPersistence.CreateMissingTables;

  // ----- Composition root: repos wrap the ORM as the DI ports -----
  fTaskRepo := TTaskRepositoryOrm.Create(fPersistence.Orm);
  fTagRepo := TTagRepositoryOrm.Create(fPersistence.Orm);

  // ----- Public dispatcher (services-only, void model) -----
  fDispatcherModel := TOrmModel.Create([], aSettings.Root);
  fDispatcher := TRestServerFullMemory.Create(fDispatcherModel);
  // Seed the dispatcher's DI resolver with the repository ports (InjectInstance,
  // Recommended Patterns B.5) so AutoResolve injects them into the services —
  // a per-composition dependency list, not global mutable state. ServiceContainer
  // materializes the resolver; this must precede ServiceDefine, which eagerly
  // creates each sicShared instance and runs AutoResolve.
  fDispatcher.ServiceContainer.InjectInstance([fTaskRepo, fTagRepo]);
  fDispatcher.ServiceDefine(TTaskQueryService, [ITaskQuery], sicShared);
  fDispatcher.ServiceDefine(TTaskCommandService, [ITaskCommand], sicShared);
  fDispatcher.ServiceDefine(TTagQueryService, [ITagQuery], sicShared);
  fDispatcher.ServiceDefine(TTagCommandService, [ITagCommand], sicShared);

  fDispatcher.Services.Resolve(ITaskCommand, fTaskCmd);
  fDispatcher.Services.Resolve(ITaskQuery, fTaskQry);
  fDispatcher.Services.Resolve(ITagCommand, fTagCmd);
  fDispatcher.Services.Resolve(ITagQuery, fTagQry);
end;

destructor TTaskManagerClientLocal.Destroy;
begin
  // Release service instances before the dispatcher that owns them.
  fTaskCmd := nil;
  fTaskQry := nil;
  fTagCmd := nil;
  fTagQry := nil;
  // fDispatcher.Free releases its DI resolver, ref-count-freeing the repos
  // seeded via InjectInstance — before Persistence (whose ORM they hold) goes.
  fDispatcher.Free;
  fDispatcherModel.Free;
  fPersistence.Free;
  fPersistenceModel.Free;
  inherited Destroy;
end;

function TTaskManagerClientLocal.TaskCommand: ITaskCommand;
begin
  Result := fTaskCmd;
end;

function TTaskManagerClientLocal.TaskQuery: ITaskQuery;
begin
  Result := fTaskQry;
end;

function TTaskManagerClientLocal.TagCommand: ITagCommand;
begin
  Result := fTagCmd;
end;

function TTaskManagerClientLocal.TagQuery: ITagQuery;
begin
  Result := fTagQry;
end;

function CreateClient(aSettings: TTaskManagerSettings): ITaskManagerClient;
begin
  Result := TTaskManagerClientLocal.Create(aSettings);
end;

end.
