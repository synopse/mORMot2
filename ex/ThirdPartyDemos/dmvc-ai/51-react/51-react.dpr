program ReactServer;

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
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TReactServer;

begin
  // Setup logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 React Sample Server');
  WriteLn('===========================');
  WriteLn('Port of DMVC React sample to mORMot2');
  WriteLn;

  try
    srv := TReactServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('RESTful API endpoints:');
      WriteLn('  GET    http://localhost:8080/api/customers      - Get all customers');
      WriteLn('  GET    http://localhost:8080/api/customers/:id  - Get customer by ID');
      WriteLn('  POST   http://localhost:8080/api/customers      - Create customer');
      WriteLn('  PUT    http://localhost:8080/api/customers/:id  - Update customer');
      WriteLn('  DELETE http://localhost:8080/api/customers/:id  - Delete customer');
      WriteLn;
      WriteLn('React frontend:');
      WriteLn('  1. cd frontend');
      WriteLn('  2. npm install');
      WriteLn('  3. npm start');
      WriteLn('  4. Open http://localhost:3000');
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
