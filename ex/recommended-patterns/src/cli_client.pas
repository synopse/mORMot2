
program cli_client;

{ Thin CLI entry point (Recommended Patterns B.6.1).

  All presentation logic lives in cli_client_core; this file only links ONE
  backend and hands its CreateClient factory over. Backend selection stays in
  the program file (not in a unit) so that switching -dLOCAL_MODE always takes
  effect: the program is recompiled on every build, cached units are not.

  Define LOCAL_MODE (compile_cli.sh local) to link the embedded-SQLite
  backend (A.6.1); leave it undefined to link the HTTP-client backend that
  talks to the daemon (A.6.2). }

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  {$ifdef LOCAL_MODE}
  AppTaskManagerClientLocal,
  {$else}
  AppTaskManagerClientRemote,
  {$endif}
  cli_client_core;

begin
  {$ifdef LOCAL_MODE}
  RunCliClient(@CreateClient, 'LOCAL (embedded SQLite)');
  {$else}
  RunCliClient(@CreateClient, 'REMOTE (HTTP client to localhost:8080)');
  {$endif}
end.
