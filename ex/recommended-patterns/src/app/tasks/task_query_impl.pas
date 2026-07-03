
unit task_query_impl;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.collections,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.soa.server,
  task,
  task_dtos,
  task_mappers,
  task_repository,
  task_query;

type
  /// Thread-safety: sicShared instance.
  /// `Repo` is auto-resolved by `TInjectableObjectRest.AutoResolve` against
  /// the server's service container (which sees ITaskRepository globally
  /// registered at the composition root).
  TTaskQueryService = class(TInjectableObjectRest, ITaskQuery)
  private
    fRepo: ITaskRepository;
    fMonitor: TSynMonitor;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetTaskView(aTaskID: TID): TTaskViewDTO;
    function ListTasks(const aStatus: RawUtf8): TTaskListItemDTODynArray;
    function SearchTasks(const aCriteria: TTaskSearchDTO): TTaskListItemDTODynArray;
  published
    /// Filled by AutoResolve from the server's service container.
    property Repo: ITaskRepository
      read fRepo write fRepo;
  end;

implementation

constructor TTaskQueryService.Create;
begin
  inherited Create;
  fMonitor := TSynMonitor.Create('TaskQuery');
end;

destructor TTaskQueryService.Destroy;
begin
  fMonitor.Free;
  inherited Destroy;
end;

function TTaskQueryService.GetTaskView(aTaskID: TID): TTaskViewDTO;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      TSynLog.Add.Log(sllWarning, 'GetTaskView: task % not found', [aTaskID]);
      FillChar(Result, SizeOf(Result), 0);
      exit;
    end;
    try
      Result := OrmToTaskViewDTO(Task);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskQueryService.ListTasks(const aStatus: RawUtf8): TTaskListItemDTODynArray;
var
  Tasks: IList<TTask>;
  StatusFilter: RawUtf8;
  i: integer;
begin
  fMonitor.ProcessStart;
  try
    if (aStatus <> '') and IsValidStatus(aStatus) then
      StatusFilter := aStatus
    else
      StatusFilter := '';
    // repository returns aggregates already migrated to the current schema
    Tasks := fRepo.List(StatusFilter);
    SetLength(Result, Tasks.Count);
    for i := 0 to Tasks.Count - 1 do
      Result[i] := OrmToTaskListItemDTO(Tasks[i]);
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskQueryService.SearchTasks(const aCriteria: TTaskSearchDTO): TTaskListItemDTODynArray;
var
  Tasks: IList<TTask>;
  StatusFilter: RawUtf8;
  i: integer;
begin
  fMonitor.ProcessStart;
  try
    if (aCriteria.Status <> '') and IsValidStatus(aCriteria.Status) then
      StatusFilter := aCriteria.Status
    else
      StatusFilter := '';
    // The repository owns the FTS5-vs-LIKE choice and the migration; the query
    // service just normalises the status filter and projects the results.
    if aCriteria.SearchTerm = '' then
      Tasks := fRepo.List(StatusFilter)
    else
      Tasks := fRepo.Search(aCriteria.SearchTerm, StatusFilter);
    SetLength(Result, Tasks.Count);
    for i := 0 to Tasks.Count - 1 do
      Result[i] := OrmToTaskListItemDTO(Tasks[i]);
  finally
    fMonitor.ProcessEnd;
  end;
end;

end.
