program CustomExceptionHandling;

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

  WriteLn('mORMot2 Custom Exception Handling Sample');
  WriteLn('=========================================');
  WriteLn('Port of DMVC custom_exception_handling sample to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - Custom exception class with rich error information');
  WriteLn('  - Global exception handler at server level');
  WriteLn('  - HTML error page generation');
  WriteLn('  - Different handling for custom vs standard exceptions');
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
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/RaiseCustomError');
      WriteLn('       Raises custom exception (EMyException)');
      WriteLn('       Shows: code, severity, details, diagnostics, expression');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/RaiseStandardError');
      WriteLn('       Raises standard exception');
      WriteLn('       Shows: simple error message');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/ExceptionHandlingApi/GetCustomer?aID=123');
      WriteLn('       Returns empty response (HTTP 200)');
      WriteLn;
      WriteLn('Test with curl:');
      WriteLn('  curl http://localhost:8080/root/ExceptionHandlingApi/RaiseCustomError');
      WriteLn('  curl http://localhost:8080/root/ExceptionHandlingApi/RaiseStandardError');
      WriteLn;
      WriteLn('Check CustomExceptionHandling.log for exception logging');
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
