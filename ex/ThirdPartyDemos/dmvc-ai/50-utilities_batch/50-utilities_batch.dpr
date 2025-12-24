program UtilitiesBatch;

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc}
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.perf,
  demos in 'src\demos.pas';

begin
  // Configure logging
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.HighResolutionTimestamp := true;

  WriteLn('mORMot2 Utilities & Batch Operations Showcase');
  WriteLn('==============================================');
  WriteLn('Port of DMVC utility samples to mORMot2');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  - High-performance data structures');
  WriteLn('  - Thread pool and concurrent operations');
  WriteLn('  - Batch processing utilities');
  WriteLn('  - Performance monitoring');
  WriteLn('  - Memory-efficient operations');
  WriteLn;

  try
    RunAllDemos;

    WriteLn;
    WriteLn('Press [Enter] to quit');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message], E);
    end;
  end;

end.
