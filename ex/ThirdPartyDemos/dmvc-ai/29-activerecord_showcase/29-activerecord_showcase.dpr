program ActiveRecordShowcase;

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
  srv: TActiveRecordShowcaseServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 ActiveRecord Showcase Sample');
  WriteLn('=====================================');
  WriteLn;
  WriteLn('Port of DMVCFramework activerecord_showcase sample');
  WriteLn;

  try
    srv := TActiveRecordShowcaseServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080/showcase');
      WriteLn;
      WriteLn('Feature Categories:');
      WriteLn('  1. Basic CRUD Operations');
      WriteLn('     - GetCustomer, CreateCustomer, UpdateCustomer, DeleteCustomer');
      WriteLn;
      WriteLn('  2. Filtering & Searching');
      WriteLn('     - GetCustomersByCity, GetCustomersByRating, GetActiveCustomers');
      WriteLn;
      WriteLn('  3. Aggregation');
      WriteLn('     - GetCustomerCount, GetAverageRating, GetCustomerStats');
      WriteLn;
      WriteLn('  4. Batch Operations');
      WriteLn('     - BulkCreateCustomers, BulkUpdateRating');
      WriteLn;
      WriteLn('  5. Articles Management');
      WriteLn('     - CreateArticle, GetArticle, GetLowStockArticles');
      WriteLn;
      WriteLn('  6. Orders & Relationships');
      WriteLn('     - CreateOrder, GetOrdersByCustomer, GetOrderWithCustomer');
      WriteLn;
      WriteLn('Example endpoints:');
      WriteLn('  curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetAllCustomers');
      WriteLn('  curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetCustomerStats');
      WriteLn('  curl http://localhost:8080/showcase/ActiveRecordShowcaseApi/GetLowStockArticles?threshold=10');
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
