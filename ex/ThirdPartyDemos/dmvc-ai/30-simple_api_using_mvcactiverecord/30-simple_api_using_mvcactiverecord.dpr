program SimpleApiUsingMvcActiveRecord;

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
  srv: TCustomerApiServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Simple Customer API Sample');
  WriteLn('===================================');
  WriteLn;
  WriteLn('Port of DMVCFramework simple_api_using_mvcactiverecord sample');
  WriteLn;

  try
    srv := TCustomerApiServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080/customers');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET    /customers/CustomerApi/GetAllCustomers');
      WriteLn('  GET    /customers/CustomerApi/GetCustomer?id=1');
      WriteLn('  GET    /customers/CustomerApi/GetCustomersByCity?city=New York');
      WriteLn('  POST   /customers/CustomerApi/CreateCustomer');
      WriteLn('  POST   /customers/CustomerApi/UpdateCustomer');
      WriteLn('  DELETE /customers/CustomerApi/DeleteCustomer?id=1');
      WriteLn('  POST   /customers/CustomerApi/BulkCreateCustomers');
      WriteLn;
      WriteLn('Example curl commands:');
      WriteLn('  curl http://localhost:8080/customers/CustomerApi/GetAllCustomers');
      WriteLn('  curl http://localhost:8080/customers/CustomerApi/GetCustomer?id=1');
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
