program LoggingSample;

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
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.text,
  mormot.core.threads,
  server in 'src\server.pas';

procedure ShowMenu;
begin
  WriteLn;
  WriteLn('=======================================');
  WriteLn('Choose a demo:');
  WriteLn('=======================================');
  WriteLn('1. Basic logging levels (Debug, Info, Warning, Error)');
  WriteLn('2. Structured logging with context');
  WriteLn('3. Multi-threaded logging stress test');
  WriteLn('4. Log with exceptions');
  WriteLn('5. Custom log levels and tags');
  WriteLn('0. Exit');
  WriteLn('=======================================');
  Write('Enter choice: ');
end;

procedure RunDemo(choice: integer);
var
  demo: TLoggingDemoServer;
begin
  WriteLn;
  WriteLn('--- Running demo ---');
  WriteLn;

  demo := TLoggingDemoServer.Create;
  try
    case choice of
      1: demo.DemoBasicLogging;
      2: demo.DemoStructuredLogging;
      3: demo.DemoMultiThreadedLogging;
      4: demo.DemoExceptionLogging;
      5: demo.DemoCustomLevels;
    end;
  finally
    demo.Free;
  end;

  WriteLn;
  WriteLn('--- Demo complete - Check LoggingSample.log ---');
  WriteLn;
  Write('Press Enter to continue...');
  ReadLn;
end;

var
  choice: string;
  choiceNum: integer;

begin
  // Configure logging with all levels enabled
  TSynLog.Family.Level := LOG_VERBOSE;  // All log levels
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;  // One file, all threads
  TSynLog.Family.HighResolutionTimestamp := true;  // Precise timing
  TSynLog.Family.AutoFlushTimeOut := 1;  // Flush every second for demo visibility

  WriteLn('=======================================');
  WriteLn('mORMot2 Logging Sample');
  WriteLn('=======================================');
  WriteLn('This sample demonstrates TSynLog capabilities:');
  WriteLn('  - Multiple log levels (Debug, Info, Warning, Error, etc.)');
  WriteLn('  - Thread-safe logging');
  WriteLn('  - Structured logging with formatting');
  WriteLn('  - Exception tracking');
  WriteLn('  - High-performance async file writes');
  WriteLn('=======================================');
  WriteLn;
  WriteLn('Log file: LoggingSample.log');
  WriteLn;

  TSynLog.Add.Log(sllInfo, 'Logging sample started');

  try
    repeat
      ShowMenu;
      ReadLn(choice);

      if TryStrToInt(choice, choiceNum) then
      begin
        if choiceNum = 0 then
          break
        else if (choiceNum >= 1) and (choiceNum <= 5) then
          RunDemo(choiceNum)
        else
          WriteLn('Invalid choice. Please enter 0-5.');
      end
      else
        WriteLn('Invalid input. Please enter a number.');
    until false;

    WriteLn;
    WriteLn('Shutting down...');
    TSynLog.Add.Log(sllInfo, 'Logging sample shutting down');

  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      TSynLog.Add.Log(sllError, 'Fatal error: %', [E.Message], E);
    end;
  end;

  WriteLn('Goodbye!');
end.
