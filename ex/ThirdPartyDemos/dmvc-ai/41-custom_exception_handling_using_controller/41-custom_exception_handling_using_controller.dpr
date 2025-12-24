program CustomExceptionHandlingUsingController;

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
  srv: TExceptionHandlingServer;

begin
  // Port of DMVC logging setup
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 Custom Exception Handling Using Controller Sample');
  WriteLn('==========================================================');
  WriteLn('Port of DMVC exception_handling_with_controller sample to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - Controller/Service-level exception handling');
  WriteLn('  - Overriding OnError method in service implementation');
  WriteLn('  - Content negotiation (HTML vs JSON error responses)');
  WriteLn('  - Different handling for service vs standard exceptions');
  WriteLn;

  try
    srv := TExceptionHandlingServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/Index');
      WriteLn('       Returns welcome message');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/RaiseServiceError');
      WriteLn('       Raises service exception (EServiceException)');
      WriteLn('       Shows: custom HTML error page (red)');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/RaiseStandardError');
      WriteLn('       Raises standard exception');
      WriteLn('       Shows: generic error page (red)');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/GetCustomer?aID=123');
      WriteLn('       Returns empty response (HTTP 200)');
      WriteLn;
      WriteLn('Test with curl:');
      WriteLn('  # HTML response (default):');
      WriteLn('  curl http://localhost:8080/root/ExceptionHandlingApi/RaiseServiceError');
      WriteLn;
      WriteLn('  # JSON response (with Accept header):');
      WriteLn('  curl -H "Accept: application/json" \');
      WriteLn('    http://localhost:8080/root/ExceptionHandlingApi/RaiseServiceError');
      WriteLn;
      WriteLn('Key difference from sample 40:');
      WriteLn('  - Sample 40: Global exception handler at server level');
      WriteLn('  - Sample 41: Per-service exception handler (OnError override)');
      WriteLn;
      WriteLn('Check CustomExceptionHandlingUsingController.log for exception logging');
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
