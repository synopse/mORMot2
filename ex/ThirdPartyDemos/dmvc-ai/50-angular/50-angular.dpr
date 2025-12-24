program AngularSample;

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
  srv: TAngularSampleServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Angular Sample Server');
  WriteLn('=============================');
  WriteLn('Port of DMVC Angular sample to mORMot2');
  WriteLn;

  try
    srv := TAngularSampleServer.Create('8080');
    try
      // Seed database with sample data
      srv.SeedDatabase;

      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('CORS enabled for Angular frontend');
      WriteLn('Default Angular dev server: http://localhost:4200');
      WriteLn;
      WriteLn('Available API endpoints (JSON-RPC):');
      WriteLn('  POST http://localhost:8080/CustomerApi.GetAll');
      WriteLn('  POST http://localhost:8080/CustomerApi.GetById');
      WriteLn('       Body: {"id":1}');
      WriteLn('  POST http://localhost:8080/CustomerApi.CreateCustomer');
      WriteLn('       Body: {"customer":{"firstname":"John","lastname":"Doe",...}}');
      WriteLn('  POST http://localhost:8080/CustomerApi.Update');
      WriteLn('       Body: {"id":1,"customer":{...}}');
      WriteLn('  POST http://localhost:8080/CustomerApi.Delete');
      WriteLn('       Body: {"id":1}');
      WriteLn('  POST http://localhost:8080/CustomerApi.Search');
      WriteLn('       Body: {"query":"John"}');
      WriteLn;
      WriteLn('Frontend files location: frontend/');
      WriteLn('To run Angular app:');
      WriteLn('  cd frontend');
      WriteLn('  npm install');
      WriteLn('  ng serve');
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
