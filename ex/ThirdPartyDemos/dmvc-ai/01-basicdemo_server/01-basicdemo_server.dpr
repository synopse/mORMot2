program BasicdemoServer;

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
  srv: TBasicDemoServer;

begin
  // Port of DMVC logging setup
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Basic Demo Server');
  WriteLn('=========================');
  WriteLn('Port of DMVC basicdemo_server to mORMot2');
  WriteLn;

  try
    srv := TBasicDemoServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available endpoints:');
      WriteLn('  GET  http://localhost:8080/BasicDemoApi/HelloWorld');
      WriteLn('  POST http://localhost:8080/BasicDemoApi/HelloWorldPost');
      WriteLn('  GET  http://localhost:8080/BasicDemoApi/Divide?par1=10&par2=2');
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
