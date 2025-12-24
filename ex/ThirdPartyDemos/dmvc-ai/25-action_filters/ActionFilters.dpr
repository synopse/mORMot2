program ActionFilters;

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
  mormot.core.unicode,
  server in 'src\server.pas',
  action.filters in 'src\action.filters.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  BusinessObjectsU in 'BusinessObjectsU.pas';

var
  srv: TActionFiltersServer;

begin
  // Set up logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 Action Filters Sample (v2 - Service-Level)');
  WriteLn('===================================================');
  WriteLn('Port of DMVC action_filters sample to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - OnMethodExecute filter (authorization/validation per service)');
  WriteLn('  - AddInterceptor hooks (before/after logging per service)');
  WriteLn('  - Service lifecycle hooks (Create/Destroy)');
  WriteLn('  - Per-service isolation (no global state pollution)');
  WriteLn('  - Composable interceptors (multiple can be registered)');
  WriteLn;

  try
    srv := TActionFiltersServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:8080/root/ActionFiltersApi/Person/<id>');
      WriteLn('       Returns person data as JSON (id is ignored, returns fixed person)');
      WriteLn;
      WriteLn('Service-level action filter behavior:');
      WriteLn('  - OnMethodExecute: Validates request (blocks on weekends!)');
      WriteLn('  - Interceptor (smsBefore): Logs method execution start');
      WriteLn('  - Interceptor (smsAfter): Logs execution time and success');
      WriteLn('  - Interceptor (smsError): Logs exceptions with timing');
      WriteLn('  - Lifecycle hooks: Log filter create/destroy');
      WriteLn;
      WriteLn('Test commands:');
      WriteLn('  curl http://localhost:8080/root/ActionFiltersApi/Person/123');
      WriteLn;
      WriteLn('To test weekend blocking:');
      WriteLn('  - On weekdays: returns person data');
      WriteLn('  - On weekends: returns 403 Forbidden error');
      WriteLn;
      WriteLn('Check logs in ActionFilters.log for filter activity');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;
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
