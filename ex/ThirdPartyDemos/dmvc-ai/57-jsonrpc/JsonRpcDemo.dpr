program JsonRpcDemo;

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
  mormot.core.text,
  jsonrpc.entities in 'src\jsonrpc.entities.pas',
  jsonrpc.interfaces in 'src\jsonrpc.interfaces.pas',
  jsonrpc.impl in 'src\jsonrpc.impl.pas',
  jsonrpc.server in 'src\jsonrpc.server.pas';

var
  srv: TJsonRpcServer;

procedure RunServer;
begin
  WriteLn('Starting JSON-RPC Server...');
  WriteLn;

  srv := TJsonRpcServer.Create('8080');
  try
    srv.Start;

    WriteLn('Server is running on http://localhost:8080');
    WriteLn;
    WriteLn('JSON-RPC 2.0 Endpoints:');
    WriteLn('  Calculator Service:');
    WriteLn('    curl -X POST http://localhost:8080/CalculatorService/Add \');
    WriteLn('      -H "Content-Type: application/json" \');
    WriteLn('      -d ''{"a":10,"b":5}''');
    WriteLn;
    WriteLn('    curl -X POST http://localhost:8080/CalculatorService/Subtract \');
    WriteLn('      -H "Content-Type: application/json" \');
    WriteLn('      -d ''{"a":10,"b":5}''');
    WriteLn;
    WriteLn('    curl -X POST http://localhost:8080/CalculatorService/GetHistory');
    WriteLn;
    WriteLn('  User Service:');
    WriteLn('    curl -X POST http://localhost:8080/UserService/CreateUser \');
    WriteLn('      -H "Content-Type: application/json" \');
    WriteLn('      -d ''{"username":"john","email":"john@example.com"}''');
    WriteLn;
    WriteLn('    curl -X POST http://localhost:8080/UserService/GetAllUsers');
    WriteLn;
    WriteLn('Press ENTER to stop the server');
    WriteLn('=======================================');
    WriteLn;

    // Wait for ENTER
    ReadLn;

  finally
    srv.Free;
  end;
end;

procedure RunClient;
begin
  WriteLn('Client mode not yet implemented.');
  WriteLn('Please use curl to test the server endpoints.');
  WriteLn('See the server output for example curl commands.');
end;

var
  mode: string;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('=======================================');
  WriteLn('mORMot2 JSON-RPC 2.0 Demo');
  WriteLn('=======================================');
  WriteLn('This sample demonstrates:');
  WriteLn('  - JSON-RPC 2.0 protocol');
  WriteLn('  - Interface-based services');
  WriteLn('  - Multiple services in one server');
  WriteLn('  - Automatic client proxy generation');
  WriteLn('  - Calculator and User services');
  WriteLn('=======================================');
  WriteLn;

  // Check command line for mode
  mode := '';
  if ParamCount > 0 then
    mode := LowerCase(ParamStr(1));

  try
    if mode = 'server' then
    begin
      RunServer;
    end
    else if mode = 'client' then
    begin
      RunClient;
    end
    else
    begin
      WriteLn('Usage:');
      WriteLn('  JsonRpcDemo server   - Run JSON-RPC server');
      WriteLn('  JsonRpcDemo client   - Run client demo');
      WriteLn;
      WriteLn('To test:');
      WriteLn('  1. Open terminal 1: JsonRpcDemo server');
      WriteLn('  2. Open terminal 2: JsonRpcDemo client');
      WriteLn;
      WriteLn('Or test with curl as shown in server mode.');
      WriteLn;
      WriteLn('Starting server mode by default...');
      WriteLn;
      RunServer;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;
end.
