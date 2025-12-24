program SSESample;

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
  storage in 'src\storage.pas';

var
  srv: TSSESampleServer;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Server-Sent Events (SSE) Sample');
  WriteLn('========================================');
  WriteLn;
  WriteLn('TRUE SSE streaming - equivalent to DMVCFramework serversentevents sample');
  WriteLn;

  try
    srv := TSSESampleServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server running on http://localhost:8080');
      WriteLn('Open http://localhost:8080/static/index.html in your browser');
      WriteLn;
      WriteLn('Press [Enter] to quit');
      WriteLn;
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
