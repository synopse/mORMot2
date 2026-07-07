
unit cli_client_core;

{ CLI presentation logic (Recommended Patterns B.6.1).

  This unit is pure presentation: it parses arguments and calls the CQRS
  ports through ITaskManagerClient. It is identical regardless of topology —
  the program file that links a backend (AppTaskManagerClientLocal or
  AppTaskManagerClientRemote) passes that backend's CreateClient factory in,
  so the ports may be reached in-process or over HTTP without this unit
  knowing (location transparency, A.6).

  The program files (src/cli_client.pas for the scripts, src/delphi/cli_client.dpr
  for the IDEs) are thin shells around RunCliClient. }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

interface

uses
  mormot.core.base,
  AppTaskManagerClient;

/// parse the command line and execute one CLI command against the backend
/// whose factory is passed as aCreateClient; aModeText names that backend
/// in the usage screen (e.g. 'REMOTE (HTTP client to localhost:8080)')
procedure RunCliClient(aCreateClient: TCreateClientFunc;
  const aModeText: RawUtf8);

implementation

uses
  SysUtils,
  mormot.core.os,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.orm.core,
  shared_types,
  task,
  task_dtos,
  task_command,
  task_query,
  app_config,
  app_settings;

var
  CreateClientFn: TCreateClientFunc;
  ModeText: RawUtf8;
  Settings: TTaskManagerSettings;
  Client: ITaskManagerClient;
  TaskCmd: ITaskCommand;
  TaskQry: ITaskQuery;

procedure PrintUsage;
begin
  WriteLn('Usage: cli_client <command> [arguments]');
  WriteLn('');
  WriteLn('Commands:');
  WriteLn('  list [status]              List tasks (status: pending, in_progress, completed)');
  WriteLn('  add <title> <desc> <prio>  Create a task (priority: 1-5)');
  WriteLn('  get <id>                   Show task details');
  WriteLn('  complete <id>              Mark task as complete');
  WriteLn('  delete <id>                Delete a task');
  WriteLn('  search <term>              Search tasks by title/description');
  WriteLn('  comment <id> <text> <author>  Add a comment to a task');
  WriteLn('  help                       Show this help');
  WriteLn('');
  WriteLn('Mode: ', ModeText);
end;

procedure PrintTask(const V: TTaskViewDTO);
var
  i: integer;
begin
  WriteLn('  ID:          ', V.ID);
  WriteLn('  Title:       ', V.Title);
  WriteLn('  Description: ', V.Description);
  WriteLn('  Priority:    ', V.Priority, ' (', V.PriorityName, ')');
  WriteLn('  Status:      ', V.Status);
  WriteLn('  Completed:   ', V.IsCompleted);
  WriteLn('  Due:         ', DateTimeToIso8601(V.DueDate, true));
  WriteLn('  Created:     ', DateTimeToIso8601(V.CreatedAt, true));
  WriteLn('  Updated:     ', DateTimeToIso8601(V.UpdatedAt, true));
  WriteLn('  Tags:        ', Length(V.TagIDs), ' tag(s)');
  WriteLn('  Comments:    ', V.CommentCount);
  if V.CommentCount > 0 then
    for i := 0 to High(V.Comments) do
      WriteLn('    [', i, '] ', V.Comments[i].Author, ': ', V.Comments[i].Content);
end;

procedure PrintTaskRow(const V: TTaskListItemDTO);
var
  Title: RawUtf8;
begin
  Title := V.Title;
  if Length(Title) > 40 then
    Title := copy(Title, 1, 37) + '...';
  WriteLn(Format('  %-4d %-40s %-12s P%d  %s',
    [V.ID, Title, V.Status, V.Priority, V.PriorityName]));
end;

procedure CmdList(const Status: RawUtf8);
var
  Items: TTaskListItemDTODynArray;
  i: integer;
begin
  Items := TaskQry.ListTasks(Status);
  if Length(Items) = 0 then
  begin
    WriteLn('No tasks found.');
    exit;
  end;
  WriteLn(Format('  %-4s %-40s %-12s %-3s %s',
    ['ID', 'Title', 'Status', 'Pri', 'Priority']));
  WriteLn(StringOfChar('-', 75));
  for i := 0 to High(Items) do
    PrintTaskRow(Items[i]);
  WriteLn('');
  WriteLn(Length(Items), ' task(s).');
end;

procedure CmdAdd(const Title, Description: RawUtf8; Priority: integer);
var
  Res: TCommandResult;
  DTO: TTaskCreateDTO;
begin
  DTO.Title := Title;
  DTO.Description := Description;
  DTO.Priority := Priority;
  DTO.DueDate := '';
  Res := TaskCmd.CreateTask(DTO);
  if Res.Success then
    WriteLn('Created task ID=', Res.ID)
  else
    WriteLn('Error: ', Res.ErrorMessage);
end;

procedure CmdGet(ID: TID);
var
  V: TTaskViewDTO;
begin
  V := TaskQry.GetTaskView(ID);
  if V.ID = 0 then
  begin
    WriteLn('Task not found.');
    exit;
  end;
  PrintTask(V);
end;

procedure CmdComplete(ID: TID);
var
  Res: TCommandResult;
begin
  Res := TaskCmd.MarkComplete(ID, true);
  if Res.Success then
    WriteLn('Task ', ID, ' marked complete.')
  else
    WriteLn('Error: ', Res.ErrorMessage);
end;

procedure CmdDelete(ID: TID);
var
  Res: TCommandResult;
begin
  Res := TaskCmd.DeleteTask(ID);
  if Res.Success then
    WriteLn('Task ', ID, ' deleted.')
  else
    WriteLn('Error: ', Res.ErrorMessage);
end;

procedure CmdSearch(const Term: RawUtf8);
var
  Crit: TTaskSearchDTO;
  Items: TTaskListItemDTODynArray;
  i: integer;
begin
  Crit.SearchTerm := Term;
  Crit.Status := '';
  Items := TaskQry.SearchTasks(Crit);
  if Length(Items) = 0 then
  begin
    WriteLn('No matches.');
    exit;
  end;
  for i := 0 to High(Items) do
    PrintTaskRow(Items[i]);
  WriteLn('');
  WriteLn(Length(Items), ' task(s) found.');
end;

procedure CmdComment(ID: TID; const Content, Author: RawUtf8);
var
  Res: TCommandResult;
  DTO: TTaskAddCommentDTO;
begin
  DTO.TaskID := ID;
  DTO.Content := Content;
  DTO.Author := Author;
  Res := TaskCmd.AddComment(DTO);
  if Res.Success then
    WriteLn('Comment added to task ', ID, '.')
  else
    WriteLn('Error: ', Res.ErrorMessage);
end;

procedure Dispatch(const Cmd: RawUtf8);
begin
  if Cmd = 'list' then
  begin
    if ParamCount >= 2 then
      CmdList(StringToUtf8(ParamStr(2)))
    else
      CmdList('');
  end
  else if Cmd = 'add' then
  begin
    if ParamCount < 4 then
    begin
      WriteLn('Usage: cli_client add <title> <description> <priority>');
      exit;
    end;
    CmdAdd(
      StringToUtf8(ParamStr(2)),
      StringToUtf8(ParamStr(3)),
      StrToIntDef(ParamStr(4), 2)
    );
  end
  else if Cmd = 'get' then
  begin
    if ParamCount < 2 then
    begin
      WriteLn('Usage: cli_client get <id>');
      exit;
    end;
    CmdGet(StrToInt64Def(ParamStr(2), 0));
  end
  else if Cmd = 'complete' then
  begin
    if ParamCount < 2 then
    begin
      WriteLn('Usage: cli_client complete <id>');
      exit;
    end;
    CmdComplete(StrToInt64Def(ParamStr(2), 0));
  end
  else if Cmd = 'delete' then
  begin
    if ParamCount < 2 then
    begin
      WriteLn('Usage: cli_client delete <id>');
      exit;
    end;
    CmdDelete(StrToInt64Def(ParamStr(2), 0));
  end
  else if Cmd = 'search' then
  begin
    if ParamCount < 2 then
    begin
      WriteLn('Usage: cli_client search <term>');
      exit;
    end;
    CmdSearch(StringToUtf8(ParamStr(2)));
  end
  else if Cmd = 'comment' then
  begin
    if ParamCount < 4 then
    begin
      WriteLn('Usage: cli_client comment <task_id> <content> <author>');
      exit;
    end;
    CmdComment(
      StrToInt64Def(ParamStr(2), 0),
      StringToUtf8(ParamStr(3)),
      StringToUtf8(ParamStr(4))
    );
  end
  else
  begin
    WriteLn('Unknown command: ', Cmd);
    WriteLn('');
    PrintUsage;
  end;
end;

procedure Run;
var
  Cmd: RawUtf8;
begin
  if ParamCount < 1 then
  begin
    PrintUsage;
    exit;
  end;

  Cmd := StringToUtf8(LowerCase(ParamStr(1)));
  if Cmd = 'help' then
  begin
    PrintUsage;
    exit;
  end;

  RegisterTaskManagerInterfaces;

  Settings := TTaskManagerSettings.Create;
  try
    // CreateClientFn is the CreateClient of whichever backend unit the
    // program file linked (AppTaskManagerClientLocal or ...Remote).
    Client := CreateClientFn(Settings);
    try
      TaskCmd := Client.TaskCommand;
      TaskQry := Client.TaskQuery;
      try
        Dispatch(Cmd);
      finally
        TaskCmd := nil;
        TaskQry := nil;
      end;
    finally
      Client := nil;
    end;
  finally
    Settings.Free;
  end;
end;

procedure RunCliClient(aCreateClient: TCreateClientFunc;
  const aModeText: RawUtf8);
begin
  CreateClientFn := aCreateClient;
  ModeText := aModeText;
  EnableSynLogs;
  try
    Run;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

end.
