
program task_manager;

{ Thin entry point (Recommended Patterns B.6.1: serv/app/<name>serverstart).
  All composition-root logic — building the two-server topology, wiring the
  repositories into the CQRS services, and starting the listener — lives in
  ServAppTaskManager. This file only brings in cthreads (must be the first unit
  on Unix for thread support) and hands control to the daemon. }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  ServAppTaskManager;

begin
  RunTaskManagerDaemon;
end.
