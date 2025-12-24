program ConsoleSample;

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
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TConsoleSampleServer;

procedure HandleShutdown;
begin
  WriteLn;
  WriteLn('Shutdown signal received. Stopping server...');
  TSynLog.Add.Log(sllInfo, 'Shutdown signal received');
  if Assigned(srv) then
  begin
    srv.Free;
    srv := nil;
  end;
  WriteLn('Server stopped. Goodbye!');
end;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('=======================================');
  WriteLn('mORMot2 Console Sample Server');
  WriteLn('=======================================');
  WriteLn('Simple REST API demonstrating:');
  WriteLn('  - Console application server');
  WriteLn('  - Graceful CTRL+C shutdown');
  WriteLn('  - Basic logging');
  WriteLn('  - REST endpoints');
  WriteLn('=======================================');
  WriteLn;

  try
    srv := TConsoleSampleServer.Create('8080');
    try
      srv.Start;

      WriteLn('Server is running on http://localhost:8080');
      WriteLn;
      WriteLn('Try the API:');
      WriteLn('  curl -X POST http://localhost:8080/GreetingService/CreateGreeting \');
      WriteLn('    -H "Content-Type: application/json" \');
      WriteLn('    -d ''{"name":"John","message":"Hello World"}''');
      WriteLn;
      WriteLn('  curl http://localhost:8080/GreetingService/GetAllGreetings');
      WriteLn;
      WriteLn('Press ENTER to stop the server');
      WriteLn('=======================================');
      WriteLn;

      // Wait for user to press ENTER
      ConsoleWaitForEnterKey;

      HandleShutdown;

    except
      on E: Exception do
      begin
        WriteLn('Error during server operation: ', E.Message);
        TSynLog.Add.Log(sllError, 'Server operation error: %', [E.Message]);
        srv.Free;
        srv := nil;
      end;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error creating server: ', E.Message);
      TSynLog.Add.Log(sllError, 'Server creation error: %', [E.Message]);
    end;
  end;

end.
