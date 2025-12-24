program MiddlewareSample;

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
  middleware in 'src\middleware.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TMiddlewareServer;

begin
  // Port of DMVC logging setup
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 Middleware Sample');
  WriteLn('=========================');
  WriteLn('Port of DMVC middleware sample to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - Request logging middleware (before/after hooks)');
  WriteLn('  - User-Agent based redirection middleware');
  WriteLn('  - Custom header injection middleware');
  WriteLn;

  try
    srv := TMiddlewareServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:8080/root/MiddlewareApi/Index');
      WriteLn('       Returns repeated * characters (1024 bytes)');
      WriteLn('       Middleware adds X-Powered-By header');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/MiddlewareApi/ShowHeaders');
      WriteLn('       Shows request headers middleware can inspect');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/root/MiddlewareApi/Echo?msg=test');
      WriteLn('       Echo service with logging');
      WriteLn;
      WriteLn('Middleware behavior:');
      WriteLn('  - All requests are logged (before & after)');
      WriteLn('  - Android User-Agent redirects to Play Store');
      WriteLn('  - All responses get X-Powered-By header');
      WriteLn;
      WriteLn('Test Android redirect:');
      WriteLn('  curl -H "User-Agent: Mozilla/5.0 (Linux; Android 10)" \');
      WriteLn('    http://localhost:8080/root/MiddlewareApi/Index');
      WriteLn;
      WriteLn('Check logs in MiddlewareSample.log for middleware activity');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      ReadLn;

      WriteLn;
      WriteLn('Statistics:');
      WriteLn('  Total requests processed: ', srv.RequestLogger.RequestCount);
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
