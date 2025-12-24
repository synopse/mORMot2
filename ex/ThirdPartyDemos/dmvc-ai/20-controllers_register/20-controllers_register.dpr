program ControllersRegister;

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
  srv: TControllersRegisterServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Controllers Register Demo');
  WriteLn('==================================');
  WriteLn('Port of DMVC controllers_register to mORMot2');
  WriteLn('Demonstrates dynamic service registration/unregistration');
  WriteLn;

  try
    srv := TControllersRegisterServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn;
      WriteLn('Service Registry Manager:');
      WriteLn('  POST http://localhost:8080/ServiceRegistry/ListServices');
      WriteLn('  POST http://localhost:8080/ServiceRegistry/LoadService');
      WriteLn('       Body: {"ServiceName":"Service1"}  or  {"ServiceName":"Service2"}');
      WriteLn('  POST http://localhost:8080/ServiceRegistry/UnloadService');
      WriteLn('       Body: {"ServiceName":"Service1"}  or  {"ServiceName":"Service2"}');
      WriteLn('  POST http://localhost:8080/ServiceRegistry/GetServiceStatus');
      WriteLn('       Body: {"ServiceName":"Service1"}');
      WriteLn;
      WriteLn('Service 1 (pre-loaded):');
      WriteLn('  POST http://localhost:8080/Service1/GetInfo');
      WriteLn;
      WriteLn('Service 2 (load first):');
      WriteLn('  POST http://localhost:8080/Service2/GetPerson');
      WriteLn;
      WriteLn('Example workflow:');
      WriteLn('  1. List services:  curl -X POST http://localhost:8080/ServiceRegistry/ListServices');
      WriteLn('  2. Load Service2:  curl -X POST http://localhost:8080/ServiceRegistry/LoadService \');
      WriteLn('                          -H "Content-Type: application/json" \');
      WriteLn('                          -d ''{"ServiceName":"Service2"}''');
      WriteLn('  3. Call Service2:  curl -X POST http://localhost:8080/Service2/GetPerson');
      WriteLn('  4. Unload Service2: curl -X POST http://localhost:8080/ServiceRegistry/UnloadService \');
      WriteLn('                           -H "Content-Type: application/json" \');
      WriteLn('                           -d ''{"ServiceName":"Service2"}''');
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
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
    end;
  end;

end.
