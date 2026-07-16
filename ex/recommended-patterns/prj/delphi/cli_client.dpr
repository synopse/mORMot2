/// mORMot2 Task Manager - CLI client (Recommended Patterns B.6.1)
// - IDE project file for Delphi and Lazarus (see ..\lazarus\cli_client.lpi)
// - same thin entry point as ..\..\src\cli_client.pas used by the shell scripts:
// all presentation logic lives in cli_client_core
// - define LOCAL_MODE to link the embedded-SQLite backend (A.6.1); leave it
// undefined to link the HTTP-client backend that talks to the daemon (A.6.2)
program cli_client;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

{ $define LOCAL_MODE}

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
  task_query in '..\..\src\dom\tasks\task_query.pas',
  task_command in '..\..\src\dom\tasks\task_command.pas',
  task_dtos in '..\..\src\app\tasks\task_dtos.pas',
  tag_query in '..\..\src\dom\tags\tag_query.pas',
  tag_command in '..\..\src\dom\tags\tag_command.pas',
  tag_dtos in '..\..\src\app\tags\tag_dtos.pas',
  AppTaskManagerClient in '..\..\src\AppTaskManagerClient.pas',
  {$ifdef LOCAL_MODE}
  task_repository in '..\..\src\dom\tasks\task_repository.pas',
  task_mappers in '..\..\src\app\tasks\task_mappers.pas',
  task_query_impl in '..\..\src\app\tasks\task_query_impl.pas',
  task_command_impl in '..\..\src\app\tasks\task_command_impl.pas',
  task_repository_orm in '..\..\src\infra\tasks\task_repository_orm.pas',
  tag in '..\..\src\dom\tags\tag.pas',
  tag_repository in '..\..\src\dom\tags\tag_repository.pas',
  tag_mappers in '..\..\src\app\tags\tag_mappers.pas',
  tag_query_impl in '..\..\src\app\tags\tag_query_impl.pas',
  tag_command_impl in '..\..\src\app\tags\tag_command_impl.pas',
  tag_repository_orm in '..\..\src\infra\tags\tag_repository_orm.pas',
  AppTaskManagerClientLocal in '..\..\src\AppTaskManagerClientLocal.pas',
  {$else}
  AppTaskManagerClientRemote in '..\..\src\AppTaskManagerClientRemote.pas',
  {$endif}
  cli_client_core in '..\..\src\cli_client_core.pas';

begin
  {$ifdef LOCAL_MODE}
  RunCliClient(@CreateClient, 'LOCAL (embedded SQLite)');
  {$else}
  RunCliClient(@CreateClient, 'REMOTE (HTTP client to localhost:8080)');
  {$endif}
end.
