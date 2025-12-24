program LogFilterSample;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TLogFilterServer;

begin
  WriteLn('mORMot2 Log Filter Sample');
  WriteLn('=========================');
  WriteLn('Port of DMVC log_filter sample to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - Custom log filtering by URI pattern');
  WriteLn('  - Multiple log levels (verbose, info, warning, error)');
  WriteLn('  - Conditional logging with custom filters');
  WriteLn('  - Per-request logging control');
  WriteLn;

  try
    srv := TLogFilterServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:8080/root/LogFilterApi/Index');
      WriteLn('       Returns HTML page (LOGGED to file and console)');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/LogFilterApi/NotLogged');
      WriteLn('       Returns timestamp (NOT LOGGED - filtered out)');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/LogFilterApi/VerboseOnly');
      WriteLn('       Demonstrates verbose level logging');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/LogFilterApi/TestLevels');
      WriteLn('       Demonstrates all log levels (trace/debug/info/warning/error)');
      WriteLn;
      WriteLn('Log behavior:');
      WriteLn('  - Requests to /NotLogged are filtered out (not written to log)');
      WriteLn('  - All other requests are logged with full details');
      WriteLn('  - Check LogFilterSample.log for filtered output');
      WriteLn('  - Console shows all activity (unfiltered)');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;

      WriteLn;
      WriteLn('Log Statistics:');
      WriteLn('  Total log events: ', srv.Stats.TotalLogEvents);
      WriteLn('  Logged events: ', srv.Stats.LoggedEvents);
      WriteLn('  Filtered events: ', srv.Stats.FilteredEvents);
    finally
      srv.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message], E);
    end;
  end;

end.
