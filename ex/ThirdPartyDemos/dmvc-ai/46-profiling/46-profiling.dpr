program ProfilingSample;

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
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.unicode,
  server in 'src\server.pas',
  api.interfaces in 'src\api.interfaces.pas',
  api.impl in 'src\api.impl.pas';

var
  srv: TProfilingServer;

begin
  // Configure logging with profiling enabled
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true; // Enable high-res profiling

  WriteLn('mORMot2 Profiling Demo Server');
  WriteLn('=============================');
  WriteLn('Port of DMVC profiling sample to mORMot2');
  WriteLn('Demonstrates TPrecisionTimer and ISynLog profiling');
  WriteLn;

  try
    srv := TProfilingServer.Create('8080');
    try
      srv.Start;
      WriteLn('Server started on http://localhost:8080');
      WriteLn;
      WriteLn('Available profiled endpoints:');
      WriteLn('  GET  http://localhost:8080/api                      - Index with recursive calls');
      WriteLn('  GET  http://localhost:8080/api/profilersample1      - Nested profiling demo');
      WriteLn('  GET  http://localhost:8080/api/profilersample2      - Simple profiling demo');
      WriteLn;
      WriteLn('Check the log file for detailed profiling information');
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
