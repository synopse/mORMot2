
unit task_mappers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.datetime,
  mormot.core.os,
  task,
  task_dtos;

// ---- Reads (ORM -> DTO). Mechanical, no I/O. ----

function OrmToTaskViewDTO(aTask: TTask): TTaskViewDTO;
function OrmToTaskListItemDTO(aTask: TTask): TTaskListItemDTO;

// ---- Writes (DTO -> ORM). Carry server-managed fields / defaults. ----

/// Fill a freshly created TTask from a TTaskCreateDTO.
/// Sets CreatedAt, UpdatedAt, default Status, IsCompleted=false, SchemaVersion.
/// Returns false if the parsed DueDate is invalid; out param carries the value.
function CreateTaskDTOToOrm(const aDto: TTaskCreateDTO; aTask: TTask;
  out aDueDateValue: TDateTime; out aError: RawUtf8): boolean;

/// Apply an update to an already-loaded TTask. Preserves CreatedAt; refreshes UpdatedAt.
function ApplyUpdateTaskDTO(const aDto: TTaskUpdateDTO; aTask: TTask;
  out aError: RawUtf8): boolean;

/// Build an embedded TTaskComment from an add-comment DTO.
procedure AddCommentDTOToComment(const aDto: TTaskAddCommentDTO;
  out aComment: TTaskComment);

implementation

function OrmToTaskViewDTO(aTask: TTask): TTaskViewDTO;
begin
  Result.ID := aTask.ID;
  Result.Title := aTask.Title;
  Result.Description := aTask.Description;
  Result.Priority := aTask.Priority;
  Result.PriorityName := GetPriorityName(aTask.Priority);
  Result.DueDate := aTask.DueDate;
  Result.Status := aTask.Status;
  Result.IsCompleted := aTask.IsCompleted;
  Result.CreatedAt := aTask.CreatedAt;
  Result.UpdatedAt := aTask.UpdatedAt;
  Result.CommentCount := Length(aTask.Comments);
  Result.TagIDs := aTask.TagIDs;
  Result.Comments := aTask.Comments;
end;

function OrmToTaskListItemDTO(aTask: TTask): TTaskListItemDTO;
begin
  Result.ID := aTask.ID;
  Result.Title := aTask.Title;
  Result.Priority := aTask.Priority;
  Result.PriorityName := GetPriorityName(aTask.Priority);
  Result.Status := aTask.Status;
  Result.IsCompleted := aTask.IsCompleted;
  Result.DueDate := aTask.DueDate;
  Result.CommentCount := Length(aTask.Comments);
  Result.TagCount := Length(aTask.TagIDs);
end;

function ParseDueDate(const aText: RawUtf8;
  out aValue: TDateTime; out aError: RawUtf8): boolean;
begin
  aValue := 0;
  aError := '';
  if aText = '' then
  begin
    Result := true;
    exit;
  end;
  try
    aValue := Iso8601ToDateTime(aText);
    Result := aValue > 0;
    if not Result then
      aError := 'Invalid due date format';
  except
    Result := false;
    aError := 'Invalid due date format';
  end;
end;

function CreateTaskDTOToOrm(const aDto: TTaskCreateDTO; aTask: TTask;
  out aDueDateValue: TDateTime; out aError: RawUtf8): boolean;
begin
  aError := '';
  if aDto.Title = '' then
  begin
    aError := 'Task title cannot be empty';
    Result := false;
    exit;
  end;
  if (aDto.Priority < 1) or (aDto.Priority > 5) then
  begin
    aError := 'Priority must be between 1 and 5';
    Result := false;
    exit;
  end;
  if not ParseDueDate(aDto.DueDate, aDueDateValue, aError) then
  begin
    Result := false;
    exit;
  end;
  aTask.Title := aDto.Title;
  aTask.Description := aDto.Description;
  aTask.Priority := aDto.Priority;
  aTask.DueDate := aDueDateValue;
  aTask.Status := GetDefaultStatus;
  aTask.IsCompleted := false;
  aTask.CreatedAt := NowUtc;
  aTask.UpdatedAt := NowUtc;
  aTask.SchemaVersion := TASK_CURRENT_VERSION;
  Result := true;
end;

function ApplyUpdateTaskDTO(const aDto: TTaskUpdateDTO; aTask: TTask;
  out aError: RawUtf8): boolean;
var
  DueDateValue: TDateTime;
begin
  aError := '';
  if aDto.Title = '' then
  begin
    aError := 'Task title cannot be empty';
    Result := false;
    exit;
  end;
  if (aDto.Priority < 1) or (aDto.Priority > 5) then
  begin
    aError := 'Priority must be between 1 and 5';
    Result := false;
    exit;
  end;
  if not ParseDueDate(aDto.DueDate, DueDateValue, aError) then
  begin
    Result := false;
    exit;
  end;
  aTask.Title := aDto.Title;
  aTask.Description := aDto.Description;
  aTask.Priority := aDto.Priority;
  aTask.DueDate := DueDateValue;
  aTask.UpdatedAt := NowUtc;
  Result := true;
end;

procedure AddCommentDTOToComment(const aDto: TTaskAddCommentDTO;
  out aComment: TTaskComment);
begin
  aComment.Content := aDto.Content;
  aComment.Author := aDto.Author;
  aComment.CreatedAt := NowUtc;
  aComment.UpdatedAt := NowUtc;
  aComment.IsEdited := false;
end;

end.
