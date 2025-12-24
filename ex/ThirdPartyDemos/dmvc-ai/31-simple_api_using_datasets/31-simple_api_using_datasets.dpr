program SimpleApiUsingDatasets;

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
  srv: TCustomerDataApiServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Dataset-Based Customer API Sample');
  WriteLn('==========================================');
  WriteLn;
  WriteLn('Port of DMVCFramework simple_api_using_datasets sample');
  WriteLn;

  try
    srv := TCustomerDataApiServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080/customerdata');
      WriteLn;
      WriteLn('Available endpoints (using direct SQL):');
      WriteLn('  GET    /customerdata/CustomerDataApi/GetCustomers');
      WriteLn('  GET    /customerdata/CustomerDataApi/GetCustomerById?id=1');
      WriteLn('  POST   /customerdata/CustomerDataApi/CreateCustomer');
      WriteLn('  POST   /customerdata/CustomerDataApi/UpdateCustomer');
      WriteLn('  DELETE /customerdata/CustomerDataApi/DeleteCustomer?id=1');
      WriteLn;
      WriteLn('Example curl commands:');
      WriteLn('  curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomers');
      WriteLn('  curl http://localhost:8080/customerdata/CustomerDataApi/GetCustomerById?id=1');
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
