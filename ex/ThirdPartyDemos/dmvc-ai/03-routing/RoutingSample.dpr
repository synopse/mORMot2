program RoutingSample;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.log,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  server: TRoutingSampleServer;
  port: RawUtf8;

begin
  // Enable logging to console
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.EchoToConsole := LOG_VERBOSE;

  // Get port from command line or use default
  if ParamCount > 0 then
    port := StringToUtf8(ParamStr(1))
  else
    port := '8080';

  WriteLn('');
  WriteLn('=============================================================');
  WriteLn('  mORMot2 Routing Sample - DMVC Conversion Example');
  WriteLn('=============================================================');
  WriteLn('');
  WriteLn('This sample demonstrates different routing patterns in mORMot2:');
  WriteLn('  - Path parameters (user ID in URL)');
  WriteLn('  - Query parameters (filter, pagination)');
  WriteLn('  - Different HTTP method simulations (GET, POST, PUT, DELETE)');
  WriteLn('  - Complex routing (search, batch operations)');
  WriteLn('');
  WriteLn('Note: mORMot2 interface-based services use POST for all methods');
  WriteLn('      with JSON bodies containing the parameters.');
  WriteLn('');
  WriteLn('Starting server on port ', port, '...');
  WriteLn('');

  try
    server := TRoutingSampleServer.Create(port);
    try
      server.Start;
      WriteLn('');
      WriteLn('Server is running. Try these curl commands:');
      WriteLn('');
      WriteLn('# Get user by ID:');
      WriteLn('curl -X POST http://localhost:', port, '/root/RoutingApi.GetUser -H "Content-Type: application/json" -d "{\"id\":1}"');
      WriteLn('');
      WriteLn('# List active users (limit 5):');
      WriteLn('curl -X POST http://localhost:', port, '/root/RoutingApi.ListUsers -H "Content-Type: application/json" -d "{\"filter\":\"active\",\"limit\":5}"');
      WriteLn('');
      WriteLn('# Search for users:');
      WriteLn('curl -X POST http://localhost:', port, '/root/RoutingApi.Search -H "Content-Type: application/json" -d "{\"term\":\"user1\"}"');
      WriteLn('');
      WriteLn('# Create new user:');
      WriteLn('curl -X POST http://localhost:', port, '/root/RoutingApi.CreateUser -H "Content-Type: application/json" -d "{\"name\":\"John Doe\",\"email\":\"john@example.com\"}"');
      WriteLn('');
      WriteLn('# Get users by status with pagination:');
      WriteLn('curl -X POST http://localhost:', port, '/root/RoutingApi.GetUsersByStatus -H "Content-Type: application/json" -d "{\"status\":\"active\",\"page\":1,\"pageSize\":10}"');
      WriteLn('');
      WriteLn('Press ENTER to stop the server...');
      ReadLn;
    finally
      server.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('Server stopped.');
end.
