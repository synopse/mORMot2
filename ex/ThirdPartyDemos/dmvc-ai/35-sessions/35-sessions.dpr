program SessionSample;

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
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TSessionSampleServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Session Management Sample');
  WriteLn('==================================');
  WriteLn;

  try
    srv := TSessionSampleServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
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
