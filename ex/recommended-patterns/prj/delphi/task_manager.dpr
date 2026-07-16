/// mORMot2 Task Manager - server daemon (Recommended Patterns B.6.1)
// - IDE project file for Delphi and Lazarus (see ..\lazarus\task_manager.lpi)
// - same thin entry point as ..\..\src\task_manager.pas used by the shell scripts:
// all composition-root logic lives in ServAppTaskManager
program task_manager;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  // mormot.core.json must initialize before the DTO units below: their
  // initialization sections call Rtti.RegisterFromText, which needs the
  // JSON-aware RTTI that mormot.core.json installs on the global Rtti
  mormot.core.json,
  shared_types in '..\..\src\shared_types.pas',
  app_config in '..\..\src\app_config.pas',
  app_settings in '..\..\src\app_settings.pas',
  task in '..\..\src\dom\tasks\task.pas',
  task_repository in '..\..\src\dom\tasks\task_repository.pas',
  task_query in '..\..\src\dom\tasks\task_query.pas',
  task_command in '..\..\src\dom\tasks\task_command.pas',
  task_dtos in '..\..\src\app\tasks\task_dtos.pas',
  task_mappers in '..\..\src\app\tasks\task_mappers.pas',
  task_query_impl in '..\..\src\app\tasks\task_query_impl.pas',
  task_command_impl in '..\..\src\app\tasks\task_command_impl.pas',
  task_repository_orm in '..\..\src\infra\tasks\task_repository_orm.pas',
  tag in '..\..\src\dom\tags\tag.pas',
  tag_repository in '..\..\src\dom\tags\tag_repository.pas',
  tag_query in '..\..\src\dom\tags\tag_query.pas',
  tag_command in '..\..\src\dom\tags\tag_command.pas',
  tag_dtos in '..\..\src\app\tags\tag_dtos.pas',
  tag_mappers in '..\..\src\app\tags\tag_mappers.pas',
  tag_query_impl in '..\..\src\app\tags\tag_query_impl.pas',
  tag_command_impl in '..\..\src\app\tags\tag_command_impl.pas',
  tag_repository_orm in '..\..\src\infra\tags\tag_repository_orm.pas',
  task_tests in '..\..\tests\tasks\task_tests.pas',
  tag_tests in '..\..\tests\tags\tag_tests.pas',
  ServAppTaskManager in '..\..\src\serv\app\ServAppTaskManager.pas';

begin
  RunTaskManagerDaemon;
end.
