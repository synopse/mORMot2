
unit task_tests;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.os,
  mormot.core.test,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  shared_types,
  task,
  task_dtos,
  task_mappers,
  task_query,
  task_command;

type
  /// TSynTestCase-based tests for the Task feature module
  /// `fOrm` points at the persistence server (used to seed test data);
  /// `fServer` is the public dispatcher where services are resolved.
  TTestTask = class(TSynTestCase)
  protected
    fOrm: IRestOrm;
    fServer: TRestServer;
    procedure Setup; override;
  published
    procedure CreateAndRetrieveTask;
    procedure UpdateTask;
    procedure MarkComplete;
    procedure DeleteTask;
    procedure SearchTasks;
    procedure AddAndManageComments;
    procedure AddAndRemoveTags;
    procedure LazyMigration;
    procedure MapperProjections;
    procedure MapperWriteSide;
  end;

/// Wire the tests to the two-server daemon layout (call before Run).
/// `aPersistence` carries the ORM; `aDispatcher` owns the services.
procedure SetTestServers(aPersistence, aDispatcher: TRestServer);

implementation

var
  _Persistence: TRestServer;
  _Dispatcher: TRestServer;

procedure SetTestServers(aPersistence, aDispatcher: TRestServer);
begin
  _Persistence := aPersistence;
  _Dispatcher := aDispatcher;
end;

procedure TTestTask.Setup;
begin
  fServer := _Dispatcher;
  fOrm := _Persistence.Orm;
end;

procedure TTestTask.CreateAndRetrieveTask;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
begin
  Check(fServer.Services.Resolve(ITaskCommand, CmdSvc), 'resolve ITaskCommand');
  Check(fServer.Services.Resolve(ITaskQuery, QrySvc), 'resolve ITaskQuery');

  CreateDTO.Title := 'Test Task';
  CreateDTO.Description := 'Test description';
  CreateDTO.Priority := 3;
  CreateDTO.DueDate := DateTimeToIso8601(NowUtc + 7, true);

  Res := CmdSvc.CreateTask(CreateDTO);
  Check(Res.Success, 'CreateTask success');
  Check(Res.ID > 0, 'CreateTask returned ID');

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.Title, 'Test Task', 'title matches');
  CheckEqual(View.Priority, 3, 'priority matches');
  CheckEqual(View.Status, 'pending', 'default status');
  Check(not View.IsCompleted, 'not completed');

  // an invalid create (empty title) reports the explicit invalid-request status
  CreateDTO.Title := '';
  Check(not CmdSvc.CreateTask(CreateDTO).Success, 'empty title rejected');
  Check(CmdSvc.CreateTask(CreateDTO).Status = srInvalidRequest,
    'invalid create status srInvalidRequest');

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.UpdateTask;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
  UpdateDTO: TTaskUpdateDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Original';
  CreateDTO.Description := 'Original desc';
  CreateDTO.Priority := 1;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);
  Check(Res.Success);

  UpdateDTO.TaskID := Res.ID;
  UpdateDTO.Title := 'Updated';
  UpdateDTO.Description := 'Updated desc';
  UpdateDTO.Priority := 5;
  UpdateDTO.DueDate := '';
  Res := CmdSvc.UpdateTask(UpdateDTO);
  Check(Res.Success, 'UpdateTask success');

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.Title, 'Updated');
  CheckEqual(View.Priority, 5);

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.MarkComplete;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Complete me';
  CreateDTO.Description := '';
  CreateDTO.Priority := 2;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);

  Res := CmdSvc.MarkComplete(Res.ID, true);
  Check(Res.Success);

  View := QrySvc.GetTaskView(Res.ID);
  Check(View.IsCompleted, 'is completed');
  CheckEqual(View.Status, 'completed');

  Res := CmdSvc.MarkComplete(Res.ID, false);
  View := QrySvc.GetTaskView(Res.ID);
  Check(not View.IsCompleted, 'not completed');
  CheckEqual(View.Status, 'pending');

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.DeleteTask;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Delete me';
  CreateDTO.Description := '';
  CreateDTO.Priority := 1;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);
  Check(Res.Success);

  Res := CmdSvc.DeleteTask(Res.ID);
  Check(Res.Success, 'DeleteTask success');
  Check(Res.Status = srSuccess, 'DeleteTask status srSuccess');

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.ID, 0, 'task no longer exists');

  // deleting a non-existent ID reports the explicit not-found status
  Res := CmdSvc.DeleteTask(999999);
  Check(not Res.Success, 'delete missing task fails');
  Check(Res.Status = srNotFound, 'delete missing task status srNotFound');

  // updating a non-existent ID also reports not-found
  Res := CmdSvc.MarkComplete(999999, true);
  Check(Res.Status = srNotFound, 'mark missing task status srNotFound');
end;

procedure TTestTask.SearchTasks;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  Items: TTaskListItemDTODynArray;
  Criteria: TTaskSearchDTO;
  CreateDTO: TTaskCreateDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Searchable unique item';
  CreateDTO.Description := 'findme keyword';
  CreateDTO.Priority := 2;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);

  Items := QrySvc.ListTasks('');
  Check(Length(Items) > 0, 'list has items');

  Criteria.SearchTerm := 'findme';
  Criteria.Status := '';
  Items := QrySvc.SearchTasks(Criteria);
  Check(Length(Items) > 0, 'search found items');

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.AddAndManageComments;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
  CommentDTO: TTaskAddCommentDTO;
  UpdateCommentDTO: TTaskUpdateCommentDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Commentable';
  CreateDTO.Description := '';
  CreateDTO.Priority := 2;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);

  CommentDTO.TaskID := Res.ID;
  CommentDTO.Content := 'First comment';
  CommentDTO.Author := 'Tester';
  Res := CmdSvc.AddComment(CommentDTO);
  Check(Res.Success, 'AddComment success');

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.CommentCount, 1, 'one comment');
  CheckEqual(View.Comments[0].Content, 'First comment');

  UpdateCommentDTO.TaskID := Res.ID;
  UpdateCommentDTO.CommentIndex := 0;
  UpdateCommentDTO.Content := 'Updated comment';
  Res := CmdSvc.UpdateComment(UpdateCommentDTO);
  Check(Res.Success);

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.Comments[0].Content, 'Updated comment');
  Check(View.Comments[0].IsEdited, 'marked as edited');

  Res := CmdSvc.DeleteComment(Res.ID, 0);
  Check(Res.Success);

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(View.CommentCount, 0, 'no comments');

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.AddAndRemoveTags;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Res: TCommandResult;
  View: TTaskViewDTO;
  CreateDTO: TTaskCreateDTO;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  CreateDTO.Title := 'Taggable';
  CreateDTO.Description := '';
  CreateDTO.Priority := 2;
  CreateDTO.DueDate := '';
  Res := CmdSvc.CreateTask(CreateDTO);

  Res := CmdSvc.AddTag(Res.ID, 100);
  Check(Res.Success, 'AddTag success');

  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(Length(View.TagIDs), 1, 'one tag');

  CmdSvc.AddTag(Res.ID, 100);
  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(Length(View.TagIDs), 1, 'still one tag');

  CmdSvc.RemoveTag(Res.ID, 100);
  View := QrySvc.GetTaskView(Res.ID);
  CheckEqual(Length(View.TagIDs), 0, 'no tags');

  CmdSvc.DeleteTask(Res.ID);
end;

procedure TTestTask.LazyMigration;
var
  CmdSvc: ITaskCommand;
  QrySvc: ITaskQuery;
  Task: TTask;
  View: TTaskViewDTO;
  TaskID: TID;
begin
  fServer.Services.Resolve(ITaskCommand, CmdSvc);
  fServer.Services.Resolve(ITaskQuery, QrySvc);

  Task := TTask.Create;
  try
    Task.Title := 'Legacy task';
    Task.SchemaVersion := 0;
    Task.Status := '';
    Task.Priority := 0;
    TaskID := fOrm.Add(Task, true);
  finally
    Task.Free;
  end;

  View := QrySvc.GetTaskView(TaskID);
  CheckEqual(View.Status, 'pending', 'status migrated');
  CheckEqual(View.Priority, 2, 'priority migrated');

  CmdSvc.DeleteTask(TaskID);
end;

procedure TTestTask.MapperProjections;
var
  Task: TTask;
  View: TTaskViewDTO;
  Item: TTaskListItemDTO;
  Comments: TTaskCommentDynArray;
  Tags: TIDDynArray;
  Created, Updated, Due: TDateTime;
begin
  // Build an ORM aggregate with known field values, including two comments
  // and two tag references, then assert the hand-written projections copy
  // every field through and recompute the derived ones (§A.7/§15.4).
  Created := NowUtc - 5;
  Updated := NowUtc - 1;
  Due := NowUtc + 3;

  SetLength(Comments, 2);
  Comments[0].Content := 'First';
  Comments[0].Author := 'Alice';
  Comments[1].Content := 'Second';
  Comments[1].Author := 'Bob';

  SetLength(Tags, 2);
  Tags[0] := 11;
  Tags[1] := 22;

  Task := TTask.Create;
  try
    Task.Title := 'Projected';
    Task.Description := 'Projected desc';
    Task.Priority := 4;
    Task.DueDate := Due;
    Task.Status := 'in_progress';
    Task.IsCompleted := true;
    Task.CreatedAt := Created;
    Task.UpdatedAt := Updated;
    Task.Comments := Comments;
    Task.TagIDs := Tags;

    // --- OrmToTaskViewDTO: scalar copies + computed PriorityName/CommentCount ---
    View := OrmToTaskViewDTO(Task);
    CheckEqual(View.Title, 'Projected', 'view title copied');
    CheckEqual(View.Description, 'Projected desc', 'view description copied');
    CheckEqual(View.Priority, 4, 'view priority copied');
    CheckEqual(View.PriorityName, GetPriorityName(4), 'view PriorityName computed');
    CheckEqual(View.PriorityName, 'Urgent', 'view PriorityName value');
    CheckSame(View.DueDate, Due, 1E-6, 'view DueDate copied');
    CheckEqual(View.Status, 'in_progress', 'view status copied');
    Check(View.IsCompleted, 'view IsCompleted copied');
    CheckSame(View.CreatedAt, Created, 1E-6, 'view CreatedAt copied');
    CheckSame(View.UpdatedAt, Updated, 1E-6, 'view UpdatedAt copied');
    CheckEqual(View.CommentCount, 2, 'view CommentCount = Length(Comments)');
    CheckEqual(Length(View.Comments), 2, 'view Comments copied through');
    CheckEqual(View.Comments[0].Content, 'First', 'view Comments[0] content');
    CheckEqual(View.Comments[1].Author, 'Bob', 'view Comments[1] author');
    CheckEqual(Length(View.TagIDs), 2, 'view TagIDs copied through');
    CheckEqual(View.TagIDs[0], 11, 'view TagIDs[0]');
    CheckEqual(View.TagIDs[1], 22, 'view TagIDs[1]');

    // --- OrmToTaskListItemDTO: scalar copies + computed counts ---
    Item := OrmToTaskListItemDTO(Task);
    CheckEqual(Item.Title, 'Projected', 'item title copied');
    CheckEqual(Item.Priority, 4, 'item priority copied');
    CheckEqual(Item.PriorityName, GetPriorityName(4), 'item PriorityName computed');
    CheckEqual(Item.Status, 'in_progress', 'item status copied');
    Check(Item.IsCompleted, 'item IsCompleted copied');
    CheckSame(Item.DueDate, Due, 1E-6, 'item DueDate copied');
    CheckEqual(Item.CommentCount, 2, 'item CommentCount = Length(Comments)');
    CheckEqual(Item.TagCount, 2, 'item TagCount = Length(TagIDs)');
  finally
    Task.Free;
  end;
end;

procedure TTestTask.MapperWriteSide;
var
  Task: TTask;
  CreateDTO: TTaskCreateDTO;
  UpdateDTO: TTaskUpdateDTO;
  AddCommentDTO: TTaskAddCommentDTO;
  Comment: TTaskComment;
  Due: TDateTime;
  Err: RawUtf8;
  PreservedCreatedAt: TDateTime;
begin
  // --- CreateTaskDTOToOrm happy path: fills ORM + server-managed defaults ---
  CreateDTO.Title := 'Created';
  CreateDTO.Description := 'Created desc';
  CreateDTO.Priority := 3;
  CreateDTO.DueDate := DateTimeToIso8601(NowUtc + 7, true);

  Task := TTask.Create;
  try
    Due := 0;
    Err := 'x';
    Check(CreateTaskDTOToOrm(CreateDTO, Task, Due, Err), 'create happy path');
    CheckEqual(Err, '', 'create no error');
    CheckEqual(Task.Title, 'Created', 'create title');
    CheckEqual(Task.Description, 'Created desc', 'create description');
    CheckEqual(Task.Priority, 3, 'create priority');
    Check(Due > 0, 'create parsed DueDate out param');
    CheckSame(Task.DueDate, Due, 1E-6, 'create DueDate assigned from parse');
    CheckEqual(Task.Status, GetDefaultStatus, 'create default Status');
    Check(not Task.IsCompleted, 'create IsCompleted=false');
    CheckEqual(Task.SchemaVersion, TASK_CURRENT_VERSION, 'create SchemaVersion');
    Check(Task.CreatedAt > 0, 'create CreatedAt set');
    Check(Task.UpdatedAt > 0, 'create UpdatedAt set');

    // failure: empty title
    CreateDTO.Title := '';
    Check(not CreateTaskDTOToOrm(CreateDTO, Task, Due, Err), 'create empty title fails');
    CheckEqual(Err, 'Task title cannot be empty', 'create empty title error');

    // failure: priority out of range
    CreateDTO.Title := 'Ok';
    CreateDTO.Priority := 6;
    Check(not CreateTaskDTOToOrm(CreateDTO, Task, Due, Err), 'create bad priority fails');
    CheckEqual(Err, 'Priority must be between 1 and 5', 'create bad priority error');
    CreateDTO.Priority := 0;
    Check(not CreateTaskDTOToOrm(CreateDTO, Task, Due, Err), 'create zero priority fails');

    // failure: invalid DueDate string
    CreateDTO.Priority := 2;
    CreateDTO.DueDate := 'not-a-date';
    Check(not CreateTaskDTOToOrm(CreateDTO, Task, Due, Err), 'create bad date fails');
    CheckEqual(Err, 'Invalid due date format', 'create bad date error');
  finally
    Task.Free;
  end;

  // --- ApplyUpdateTaskDTO: mutate user fields, preserve CreatedAt, refresh UpdatedAt ---
  Task := TTask.Create;
  try
    PreservedCreatedAt := NowUtc - 10;
    Task.CreatedAt := PreservedCreatedAt;
    Task.UpdatedAt := 1.0; // stale marker
    Task.Title := 'Before';
    Task.Priority := 1;

    UpdateDTO.TaskID := 42;
    UpdateDTO.Title := 'After';
    UpdateDTO.Description := 'After desc';
    UpdateDTO.Priority := 5;
    UpdateDTO.DueDate := '';

    Check(ApplyUpdateTaskDTO(UpdateDTO, Task, Err), 'update happy path');
    CheckEqual(Err, '', 'update no error');
    CheckEqual(Task.Title, 'After', 'update title mutated');
    CheckEqual(Task.Description, 'After desc', 'update description mutated');
    CheckEqual(Task.Priority, 5, 'update priority mutated');
    CheckSame(Task.CreatedAt, PreservedCreatedAt, 1E-6, 'update preserves CreatedAt');
    Check(Task.UpdatedAt > 1.0, 'update refreshes UpdatedAt');

    // failure: empty title
    UpdateDTO.Title := '';
    Check(not ApplyUpdateTaskDTO(UpdateDTO, Task, Err), 'update empty title fails');
    CheckEqual(Err, 'Task title cannot be empty', 'update empty title error');

    // failure: priority out of range
    UpdateDTO.Title := 'Ok';
    UpdateDTO.Priority := 9;
    Check(not ApplyUpdateTaskDTO(UpdateDTO, Task, Err), 'update bad priority fails');
    CheckEqual(Err, 'Priority must be between 1 and 5', 'update bad priority error');

    // failure: invalid DueDate string
    UpdateDTO.Priority := 3;
    UpdateDTO.DueDate := 'garbage';
    Check(not ApplyUpdateTaskDTO(UpdateDTO, Task, Err), 'update bad date fails');
    CheckEqual(Err, 'Invalid due date format', 'update bad date error');
  finally
    Task.Free;
  end;

  // --- AddCommentDTOToComment: fills Content/Author, timestamps, IsEdited=false ---
  AddCommentDTO.TaskID := 7;
  AddCommentDTO.Content := 'Hello';
  AddCommentDTO.Author := 'Carol';
  Comment.IsEdited := true; // pre-dirty to prove the mapper resets it
  AddCommentDTOToComment(AddCommentDTO, Comment);
  CheckEqual(Comment.Content, 'Hello', 'comment content');
  CheckEqual(Comment.Author, 'Carol', 'comment author');
  Check(Comment.CreatedAt > 0, 'comment CreatedAt set');
  Check(Comment.UpdatedAt > 0, 'comment UpdatedAt set');
  Check(not Comment.IsEdited, 'comment IsEdited=false');
end;

end.
