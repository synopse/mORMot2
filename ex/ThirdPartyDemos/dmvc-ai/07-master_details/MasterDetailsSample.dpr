program MasterDetailsSample;

{$APPTYPE CONSOLE}

{$I mormot.defines.inc}

uses
  {$I mormot.uses.inc}
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.unicode,
  System.SysUtils,
  entities in 'src\entities.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas',
  server in 'src\server.pas';

var
  srv: TMasterDetailsSampleServer;

begin
  try
    // Configure logging
    with TSynLog.Family do
    begin
      Level := LOG_VERBOSE;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
    end;

    Writeln('mORMot2 Master-Details (Relationships) Sample');
    Writeln('==============================================');
    Writeln('');
    Writeln('This sample demonstrates:');
    Writeln('- 1-N relationships (Orders -> OrderItems)');
    Writeln('- Cascading deletes');
    Writeln('- Nested JSON responses');
    Writeln('- Order total recalculation');
    Writeln('');

    // Create server on port 8080
    srv := TMasterDetailsSampleServer.Create('8080', 'orders.db3');
    try
      Writeln('Server is running on http://localhost:8080');
      Writeln('');
      Writeln('API Endpoints:');
      Writeln('');
      Writeln('Articles (Product Catalog):');
      Writeln('  GET  /OrdersAPI/GetArticles');
      Writeln('       Returns: All available articles');
      Writeln('');
      Writeln('  GET  /OrdersAPI/GetArticleByID?id=1');
      Writeln('       Returns: Single article by ID');
      Writeln('');
      Writeln('Orders Management:');
      Writeln('  GET  /OrdersAPI/GetOrders');
      Writeln('       Returns: All orders with nested items');
      Writeln('');
      Writeln('  GET  /OrdersAPI/GetOrderByID?id=1');
      Writeln('       Returns: Single order with all items');
      Writeln('');
      Writeln('  POST /OrdersAPI/CreateOrder');
      Writeln('       Body: {"customerid":123,"orderdate":"2025-12-19T00:00:00"}');
      Writeln('       Returns: New order ID');
      Writeln('');
      Writeln('  POST /OrdersAPI/UpdateOrder?id=1');
      Writeln('       Body: {"customerid":123,"orderdate":"2025-12-19T00:00:00"}');
      Writeln('       Returns: Success boolean');
      Writeln('');
      Writeln('  GET  /OrdersAPI/DeleteOrder?id=1');
      Writeln('       Returns: Success boolean (cascades to items)');
      Writeln('');
      Writeln('Order Items Management:');
      Writeln('  POST /OrdersAPI/AddItemToOrder?orderid=1');
      Writeln('       Body: {"articleid":1,"unitprice":10.5,"discount":0,"quantity":2,"description":"Widget A"}');
      Writeln('       Returns: New item ID (auto-recalculates order total)');
      Writeln('');
      Writeln('  POST /OrdersAPI/UpdateOrderItem?id=1');
      Writeln('       Body: {"articleid":1,"unitprice":10.5,"discount":10,"quantity":3,"description":"Widget A"}');
      Writeln('       Returns: Success boolean (auto-recalculates order total)');
      Writeln('');
      Writeln('  GET  /OrdersAPI/RemoveItemFromOrder?orderid=1&itemid=1');
      Writeln('       Returns: Success boolean (auto-recalculates order total)');
      Writeln('');
      Writeln('Search:');
      Writeln('  GET  /OrdersAPI/GetOrdersByTotalGreaterThan?total=50.00');
      Writeln('       Returns: Orders with total >= specified amount');
      Writeln('');
      Writeln('Examples using curl:');
      Writeln('');
      Writeln('  # Get all articles');
      Writeln('  curl http://localhost:8080/OrdersAPI/GetArticles');
      Writeln('');
      Writeln('  # Create a new order');
      Writeln('  curl -X POST http://localhost:8080/OrdersAPI/CreateOrder \');
      Writeln('    -H "Content-Type: application/json" \');
      Writeln('    -d "{\"customerid\":100,\"orderdate\":\"2025-12-19T00:00:00\"}"');
      Writeln('');
      Writeln('  # Add item to order (replace 1 with actual order ID)');
      Writeln('  curl -X POST "http://localhost:8080/OrdersAPI/AddItemToOrder?orderid=1" \');
      Writeln('    -H "Content-Type: application/json" \');
      Writeln('    -d "{\"articleid\":1,\"unitprice\":10.5,\"discount\":0,\"quantity\":2,\"description\":\"Widget A\"}"');
      Writeln('');
      Writeln('  # Get order with all items');
      Writeln('  curl http://localhost:8080/OrdersAPI/GetOrderByID?id=1');
      Writeln('');
      Writeln('  # Delete order (cascades to items)');
      Writeln('  curl http://localhost:8080/OrdersAPI/DeleteOrder?id=1');
      Writeln('');
      Writeln('Press [Enter] to stop the server');
      Readln;
    finally
      srv.Free;
    end;

    Writeln('Server stopped.');
  except
    on E: Exception do
    begin
      Writeln('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
