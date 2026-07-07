
unit task_command_impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.server,
  mormot.soa.server,
  shared_types,
  task,
  task_dtos,
  task_mappers,
  task_repository,
  task_command;

type
  /// Thread-safety: sicShared instance.
  /// `Repo` is auto-resolved by `TInjectableObjectRest.AutoResolve` against
  /// the server's service container (globally-registered ITaskRepository).
  /// `TSynMonitor` is internally locked.
  ///
  /// The service is pure use-case orchestration: it validates the request,
  /// loads the aggregate, asks the aggregate to mutate itself (TTask owns its
  /// invariants — see B.3), and persists through the repository. Transactions,
  /// FTS5 sync and schema migration are entirely the repository's concern.
  TTaskCommandService = class(TInjectableObjectRest, ITaskCommand)
  private
    fRepo: ITaskRepository;
    fMonitor: TSynMonitor;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateTask(const aData: TTaskCreateDTO): TCommandResult;
    function UpdateTask(const aData: TTaskUpdateDTO): TCommandResult;
    function DeleteTask(aTaskID: TID): TCommandResult;
    function MarkComplete(aTaskID: TID; aIsComplete: boolean): TCommandResult;
    function AddComment(const aData: TTaskAddCommentDTO): TCommandResult;
    function UpdateComment(const aData: TTaskUpdateCommentDTO): TCommandResult;
    function DeleteComment(aTaskID: TID; aCommentIndex: integer): TCommandResult;
    function AddTag(aTaskID: TID; aTagID: TID): TCommandResult;
    function RemoveTag(aTaskID: TID; aTagID: TID): TCommandResult;
  published
    property Repo: ITaskRepository
      read fRepo write fRepo;
  end;

implementation

constructor TTaskCommandService.Create;
begin
  inherited Create;
  fMonitor := TSynMonitor.Create('TaskCommand');
end;

destructor TTaskCommandService.Destroy;
begin
  fMonitor.Free;
  inherited Destroy;
end;

function TTaskCommandService.CreateTask(const aData: TTaskCreateDTO): TCommandResult;
var
  Task: TTask;
  DueDateValue: TDateTime;
  ErrMsg: RawUtf8;
  NewID: TID;
begin
  fMonitor.ProcessStart;
  try
    Task := TTask.Create;
    try
      if not CreateTaskDTOToOrm(aData, Task, DueDateValue, ErrMsg) then
      begin
        TSynLog.Add.Log(sllWarning, 'CreateTask: %', [ErrMsg]);
        Result := CommandError(ErrMsg);
        exit;
      end;
      try
        NewID := fRepo.Add(Task); // atomic: transaction + FTS5 sync inside
        if NewID > 0 then
        begin
          TSynLog.Add.Log(sllInfo, 'CreateTask: created ID=%', [NewID]);
          Result := CommandSuccess(NewID);
        end
        else
          Result := CommandError('Failed to create task', srDbError);
      except
        on E: Exception do
        begin
          TSynLog.Add.Log(sllError, 'CreateTask: %', [E.Message]);
          Result := CommandError(StringToUtf8(E.Message), srDbError);
        end;
      end;
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.UpdateTask(const aData: TTaskUpdateDTO): TCommandResult;
var
  Task: TTask;
  ErrMsg: RawUtf8;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aData.TaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      if not ApplyUpdateTaskDTO(aData, Task, ErrMsg) then
      begin
        Result := CommandError(ErrMsg);
        exit;
      end;
      try
        if fRepo.Update(Task) then // atomic: transaction + FTS5 sync inside
          Result := CommandSuccess(Task.ID)
        else
          Result := CommandError('Failed to update task', srDbError);
      except
        on E: Exception do
          Result := CommandError(StringToUtf8(E.Message), srDbError);
      end;
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.DeleteTask(aTaskID: TID): TCommandResult;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    Task.Free;

    try
      if fRepo.Delete(aTaskID) then // atomic: transaction + FTS5 row removal
      begin
        TSynLog.Add.Log(sllInfo, 'DeleteTask: deleted ID=%', [aTaskID]);
        Result := CommandSuccess(aTaskID);
      end
      else
        Result := CommandError('Failed to delete task', srDbError);
    except
      on E: Exception do
        Result := CommandError(StringToUtf8(E.Message), srDbError);
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.MarkComplete(aTaskID: TID; aIsComplete: boolean): TCommandResult;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      Task.SetCompleted(aIsComplete);
      if fRepo.Update(Task) then
        Result := CommandSuccess(aTaskID)
      else
        Result := CommandError('Failed to update task', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.AddComment(const aData: TTaskAddCommentDTO): TCommandResult;
var
  Task: TTask;
  NewComment: TTaskComment;
begin
  fMonitor.ProcessStart;
  try
    if Length(aData.Content) = 0 then
    begin
      Result := CommandError('Comment content cannot be empty');
      exit;
    end;
    if Length(aData.Content) > 10000 then
    begin
      Result := CommandError('Comment content too long');
      exit;
    end;
    if Length(aData.Author) = 0 then
    begin
      Result := CommandError('Comment author is required');
      exit;
    end;

    Task := fRepo.GetByID(aData.TaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      AddCommentDTOToComment(aData, NewComment);
      Task.AppendComment(NewComment);
      if fRepo.Update(Task) then
      begin
        TSynLog.Add.Log(sllInfo, 'AddComment: added to task %', [aData.TaskID]);
        Result := CommandSuccess(aData.TaskID);
      end
      else
        Result := CommandError('Failed to add comment', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.UpdateComment(const aData: TTaskUpdateCommentDTO): TCommandResult;
var
  Task: TTask;
  Changed: boolean;
begin
  fMonitor.ProcessStart;
  try
    if Length(aData.Content) = 0 then
    begin
      Result := CommandError('Comment content cannot be empty');
      exit;
    end;
    Task := fRepo.GetByID(aData.TaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      if not Task.UpdateCommentContent(aData.CommentIndex, aData.Content, Changed) then
      begin
        Result := CommandError('Comment index out of range');
        exit;
      end;
      if not Changed then
      begin
        Result := CommandSuccess(aData.TaskID);
        exit;
      end;
      if fRepo.Update(Task) then
        Result := CommandSuccess(aData.TaskID)
      else
        Result := CommandError('Failed to update comment', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.DeleteComment(aTaskID: TID; aCommentIndex: integer): TCommandResult;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      if not Task.DeleteComment(aCommentIndex) then
      begin
        Result := CommandError('Comment index out of range');
        exit;
      end;
      if fRepo.Update(Task) then
        Result := CommandSuccess(aTaskID)
      else
        Result := CommandError('Failed to delete comment', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.AddTag(aTaskID: TID; aTagID: TID): TCommandResult;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      if not Task.AddTag(aTagID) then
      begin
        // already present — idempotent success, no write needed
        Result := CommandSuccess(aTaskID);
        exit;
      end;
      if fRepo.Update(Task) then
        Result := CommandSuccess(aTaskID)
      else
        Result := CommandError('Failed to add tag', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

function TTaskCommandService.RemoveTag(aTaskID: TID; aTagID: TID): TCommandResult;
var
  Task: TTask;
begin
  fMonitor.ProcessStart;
  try
    Task := fRepo.GetByID(aTaskID);
    if Task = nil then
    begin
      Result := CommandError('Task not found', srNotFound);
      exit;
    end;
    try
      if not Task.RemoveTag(aTagID) then
      begin
        // not present — idempotent success, no write needed
        Result := CommandSuccess(aTaskID);
        exit;
      end;
      if fRepo.Update(Task) then
        Result := CommandSuccess(aTaskID)
      else
        Result := CommandError('Failed to remove tag', srDbError);
    finally
      Task.Free;
    end;
  finally
    fMonitor.ProcessEnd;
  end;
end;

end.
