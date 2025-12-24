program DataPumpSample;

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
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  entities in 'src\entities.pas',
  datapump in 'src\datapump.pas';

var
  pump: TDataPump;
  sourceDb, destDb: RawUtf8;

begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

  WriteLn('mORMot2 Data Pump Sample');
  WriteLn('========================');
  WriteLn('Port of DMVC datapump to mORMot2');
  WriteLn('Demonstrates ETL (Extract, Transform, Load) patterns');
  WriteLn;

  try
    // Create source and destination database paths
    sourceDb := StringToUtf8(Executable.ProgramFilePath + 'source.db');
    destDb := StringToUtf8(Executable.ProgramFilePath + 'destination.db');

    pump := TDataPump.Create;
    try
      WriteLn('Step 1: Creating source database with sample data...');
      pump.CreateSourceDatabase(sourceDb);
      WriteLn('  Created ', pump.GetRecordCount(sourceDb, 'Customers'), ' customers');
      WriteLn('  Created ', pump.GetRecordCount(sourceDb, 'Orders'), ' orders');
      WriteLn;

      WriteLn('Step 2: Creating destination database schema...');
      pump.CreateDestinationDatabase(destDb);
      WriteLn('  Destination database ready');
      WriteLn;

      WriteLn('Step 3: Pumping data from source to destination...');
      pump.PumpData(sourceDb, destDb);
      WriteLn;

      WriteLn('Step 4: Verifying data transfer...');
      WriteLn('  Source customers: ', pump.GetRecordCount(sourceDb, 'Customers'));
      WriteLn('  Dest customers:   ', pump.GetRecordCount(destDb, 'Customers'));
      WriteLn('  Source orders:    ', pump.GetRecordCount(sourceDb, 'Orders'));
      WriteLn('  Dest orders:      ', pump.GetRecordCount(destDb, 'Orders'));
      WriteLn;

      WriteLn('Step 5: Demonstrating batch operations...');
      pump.DemonstrateBatchInsert(destDb);
      WriteLn;

      WriteLn('All operations completed successfully!');
      WriteLn;
      WriteLn('Database files:');
      WriteLn('  Source:      ', Utf8ToString(sourceDb));
      WriteLn('  Destination: ', Utf8ToString(destDb));
      WriteLn;
      WriteLn('Press [Enter] to exit');
      ReadLn;
    finally
      pump.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message]);
      WriteLn;
      WriteLn('Press [Enter] to exit');
      ReadLn;
      ExitCode := 1;
    end;
  end;

end.
