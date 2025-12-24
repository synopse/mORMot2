program ServerContainerDemo;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

{ mORMot2 port of DMVC servercontainer sample

  This sample demonstrates running multiple REST servers in a single process.
  Each server runs on a different port with different API implementations.

  Server 1 (port 3000): Standard calculator
  Server 2 (port 3010): Calculator with doubled addition
  Server 3 (port 3020): Calculator with 10x division

  Test endpoints:
    http://localhost:3000/root/CalculatorApi.GetInfo
    http://localhost:3000/root/CalculatorApi.Divide?a=10&b=2
    http://localhost:3010/root/CalculatorApi.Add?a=5&b=3  (returns 16 instead of 8)
    http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2  (returns 50 instead of 5)
}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  server.container in 'src\server.container.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  container: TServerContainer;

begin
  // Initialize logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Server Container Demo');
  WriteLn('=============================');
  WriteLn('Port of DMVC servercontainer to mORMot2');
  WriteLn;

  try
    container := TServerContainer.Create;
    try
      WriteLn('Starting multiple servers...');

      // Add servers with different implementations
      container.AddServer('Server01', '3000', TCalculatorApi1);
      container.AddServer('Server02', '3010', TCalculatorApi2);
      container.AddServer('Server03', '3020', TCalculatorApi3);

      container.StartAll;

      WriteLn;
      WriteLn(Format('Started %d servers:', [container.Count]));
      WriteLn('  Server 1: http://localhost:3000/root/CalculatorApi.GetInfo');
      WriteLn('  Server 2: http://localhost:3010/root/CalculatorApi.Add?a=5&b=3 (returns 16)');
      WriteLn('  Server 3: http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2 (returns 50)');
      WriteLn;
      WriteLn('Press [Enter] to stop all servers and quit');
      ReadLn;

      container.StopAll;
    finally
      container.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
      ExitCode := 1;
    end;
  end;

  WriteLn('Application finished');
end.
