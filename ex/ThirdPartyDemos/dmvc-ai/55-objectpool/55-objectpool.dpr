program ObjectPoolDemo;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  server in 'src\server.pas',
  worker in 'src\worker.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TObjectPoolServer;

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Object Pool Demo');
  WriteLn('========================');
  WriteLn('Demonstrates object pooling patterns for performance optimization');
  WriteLn;

  try
    srv := TObjectPoolServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  POST http://localhost:8080/ObjectPoolApi/ExecuteSimpleOperation');
      WriteLn('       Body: {"aData":"test data"}');
      WriteLn;
      WriteLn('  POST http://localhost:8080/ObjectPoolApi/ExecuteParallelOperations');
      WriteLn('       Body: {"aCount":20}');
      WriteLn;
      WriteLn('  GET  http://localhost:8080/ObjectPoolApi/GetPoolStats');
      WriteLn('  POST http://localhost:8080/ObjectPoolApi/ResetPoolStats');
      WriteLn('  GET  http://localhost:8080/ObjectPoolApi/GetPoolInfo');
      WriteLn;
      WriteLn('Test with curl:');
      WriteLn('  curl -X POST http://localhost:8080/ObjectPoolApi/ExecuteSimpleOperation \');
      WriteLn('    -H "Content-Type: application/json" \');
      WriteLn('    -d ''{"aData":"hello world"}''');
      WriteLn;
      WriteLn('  curl -X POST http://localhost:8080/ObjectPoolApi/ExecuteParallelOperations \');
      WriteLn('    -H "Content-Type: application/json" \');
      WriteLn('    -d ''{"aCount":50}''');
      WriteLn;
      WriteLn('  curl http://localhost:8080/ObjectPoolApi/GetPoolStats');
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
      ExitCode := 1;
    end;
  end;

end.
