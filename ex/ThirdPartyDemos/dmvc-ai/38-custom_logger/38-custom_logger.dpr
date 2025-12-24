program CustomLoggerSample;

{$I ..\..\..\src\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif OSWINDOWS}

uses
  {$I ..\..\..\src\mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  custom.logger in 'src\custom.logger.pas';

begin
  // Custom logger is initialized in custom.logger unit
  // DMVC: SetDefaultLogger(GetLogger) or decorator pattern

  WriteLn('** mORMot2 Custom Logger Sample **');
  WriteLn;
  WriteLn('Port of: DMVCFramework samples/custom_logger');
  WriteLn;
  WriteLn('IMPORTANT: Check the custom log files in:');
  WriteLn('  ', IncludeTrailingPathDelimiter(GetCurrentDir), 'MyFolder\MyLogs\');
  WriteLn;
  WriteLn('The custom logger:');
  WriteLn('  - Writes to custom folder: MyFolder\MyLogs\');
  WriteLn('  - Rotates files (10 files max)');
  WriteLn('  - Includes OutputDebugString for Windows debugger');
  WriteLn('  - Auto-flushes every second');
  WriteLn;

  try
    // Test logging at various levels
    TCustomSynLog.Add.Log(sllInfo, 'Application started - custom logger active');
    TCustomSynLog.Add.Log(sllDebug, 'This is a debug message');
    TCustomSynLog.Add.Log(sllTrace, 'This is a trace message');

    WriteLn('Check the log files in MyFolder\MyLogs\');
    WriteLn;
    WriteLn('Sample log messages written:');
    WriteLn('  - INFO: Application started');
    WriteLn('  - DEBUG: Debug message');
    WriteLn('  - TRACE: Trace message');
    WriteLn;
    WriteLn('Press [Enter] to quit');
    ReadLn;

    TCustomSynLog.Add.Log(sllInfo, 'Application ending');
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      TCustomSynLog.Add.Log(sllException, E);
    end;
  end;
end.
