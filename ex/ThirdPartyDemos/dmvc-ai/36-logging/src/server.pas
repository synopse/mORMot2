unit server;

{$I ..\..\..\src\mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.text,
  mormot.core.threads;

type
  /// Simple thread for logging demo
  TLoggerThread = class(TThread)
  private
    fThreadNum: integer;
  protected
    procedure Execute; override;
  public
    constructor Create(aThreadNum: integer);
  end;

  /// Logging demonstration server
  TLoggingDemoServer = class
  public
    /// Basic logging at different levels
    procedure DemoBasicLogging;
    /// Structured logging with formatted output
    procedure DemoStructuredLogging;
    /// Multi-threaded logging stress test
    procedure DemoMultiThreadedLogging;
    /// Exception logging and tracking
    procedure DemoExceptionLogging;
    /// Custom log levels and tags
    procedure DemoCustomLevels;
  end;

implementation

{ TLoggerThread }

constructor TLoggerThread.Create(aThreadNum: integer);
begin
  inherited Create(false);  // Not suspended
  FreeOnTerminate := false;
  fThreadNum := aThreadNum;
end;

procedure TLoggerThread.Execute;
var
  threadId: TThreadID;
  j: integer;
  threadName: RawUtf8;
begin
  threadId := GetCurrentThreadId;
  threadName := FormatUtf8('Thread-%', [threadId]);

  for j := 1 to 50 do
  begin
    TSynLog.Add.Log(sllDebug, '%: Message % of 50', [threadName, j]);

    if j mod 10 = 0 then
      TSynLog.Add.Log(sllInfo, '%: Checkpoint - % messages written',
        [threadName, j]);

    Sleep(1);  // Small delay to interleave messages
  end;

  TSynLog.Add.Log(sllInfo, '%: Thread complete - all 50 messages written',
    [threadName]);
end;

{ TLoggingDemoServer }

procedure TLoggingDemoServer.DemoBasicLogging;
begin
  WriteLn('Demo 1: Basic Logging Levels');
  WriteLn('-----------------------------');
  WriteLn('Logging at different severity levels...');

  // Different log levels
  TSynLog.Add.Log(sllDebug, 'This is a DEBUG message - detailed diagnostic info');
  TSynLog.Add.Log(sllTrace, 'This is a TRACE message - very detailed execution flow');
  TSynLog.Add.Log(sllInfo, 'This is an INFO message - general information');
  TSynLog.Add.Log(sllWarning, 'This is a WARNING message - something unexpected');
  TSynLog.Add.Log(sllError, 'This is an ERROR message - something went wrong');

  WriteLn('Basic logging complete - 5 messages written');
end;

procedure TLoggingDemoServer.DemoStructuredLogging;
var
  userName: RawUtf8;
  userId: integer;
  startTime, endTime: Int64;
  duration: Int64;
begin
  WriteLn('Demo 2: Structured Logging');
  WriteLn('---------------------------');
  WriteLn('Logging with formatted data and context...');

  userName := 'JohnDoe';
  userId := 12345;

  // Structured logging with parameters
  TSynLog.Add.Log(sllInfo, 'User login: name=% id=%', [userName, userId]);

  // Simulating an operation with timing
  startTime := GetTickCount64;
  TSynLog.Add.Log(sllTrace, 'Starting data processing for user %', [userName]);

  Sleep(100);  // Simulate work

  endTime := GetTickCount64;
  duration := endTime - startTime;

  TSynLog.Add.Log(sllInfo, 'Data processing complete: user=% duration=%ms',
    [userName, duration]);

  // Logging with multiple data points
  TSynLog.Add.Log(sllDebug,
    'Transaction details: user=% id=% amount=% currency=% status=%',
    [userName, userId, 99.99, 'USD', 'completed']);

  WriteLn('Structured logging complete - check log for formatted output');
end;

procedure TLoggingDemoServer.DemoMultiThreadedLogging;
var
  threads: array[0..3] of TLoggerThread;
  i: integer;
begin
  WriteLn('Demo 3: Multi-threaded Logging');
  WriteLn('-------------------------------');
  WriteLn('Starting 4 threads, each writing 50 log messages...');

  TSynLog.Add.Log(sllInfo, 'Multi-threaded logging test starting with 4 threads');

  // Create and start threads
  for i := Low(threads) to High(threads) do
    threads[i] := TLoggerThread.Create(i);

  WriteLn('Waiting for threads to complete...');

  // Wait for all threads to finish
  for i := Low(threads) to High(threads) do
    threads[i].WaitFor;

  // Clean up threads
  for i := Low(threads) to High(threads) do
    threads[i].Free;

  TSynLog.Add.Log(sllInfo, 'Multi-threaded logging test complete');
  WriteLn('Multi-threaded logging complete - 200 messages from 4 threads');
end;

procedure TLoggingDemoServer.DemoExceptionLogging;
{$HINTS OFF}  // Suppress hints for intentional exception demo
var
  divisor: integer;
begin
  WriteLn('Demo 4: Exception Logging');
  WriteLn('--------------------------');
  WriteLn('Demonstrating exception capture in logs...');

  TSynLog.Add.Log(sllTrace, 'Testing exception handling');

  // Intentional exception
  try
    TSynLog.Add.Log(sllDebug, 'Attempting division by zero...');
    divisor := 0;  // Set to zero
    divisor := 42 div divisor;  // This will raise an exception
  except
    on E: Exception do
    begin
      // Log the exception with full stack trace
      TSynLog.Add.Log(sllError, 'Exception caught: %', [E.Message], E);
      WriteLn('Exception logged: ', E.ClassName, ' - ', E.Message);
    end;
  end;

  // Another exception scenario
  try
    TSynLog.Add.Log(sllDebug, 'Attempting invalid type conversion...');
    raise EConvertError.Create('Invalid data format: expected integer, got "abc"');
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'Conversion error in processing: %', [E.Message], E);
      WriteLn('Conversion error logged');
    end;
  end;

  // Custom exception with context
  try
    TSynLog.Add.Log(sllDebug, 'Simulating business logic error...');
    raise Exception.CreateFmt('User %s not found (ID: %d)', ['Alice', 999]);
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'Business logic error: %', [E.Message], E);
      WriteLn('Business logic error logged');
    end;
  end;

  TSynLog.Add.Log(sllInfo, 'Exception logging demo complete');
  WriteLn('Exception logging complete - check log for stack traces');
end;
{$HINTS ON}

procedure TLoggingDemoServer.DemoCustomLevels;
begin
  WriteLn('Demo 5: Custom Log Levels');
  WriteLn('--------------------------');
  WriteLn('Using different TSynLog event types...');

  // Standard levels
  TSynLog.Add.Log(sllDebug, 'Standard DEBUG level');
  TSynLog.Add.Log(sllInfo, 'Standard INFO level');
  TSynLog.Add.Log(sllWarning, 'Standard WARNING level');
  TSynLog.Add.Log(sllError, 'Standard ERROR level');

  // Special levels
  TSynLog.Add.Log(sllEnter, 'Method entry point logged');
  TSynLog.Add.Log(sllLeave, 'Method exit point logged');

  // Custom events (sllCustom1, sllCustom2, etc. can be used for app-specific events)
  TSynLog.Add.Log(sllCustom1, 'CUSTOM EVENT: Payment processed successfully');
  TSynLog.Add.Log(sllCustom2, 'CUSTOM EVENT: Cache invalidated');
  TSynLog.Add.Log(sllCustom3, 'CUSTOM EVENT: Email notification sent');
  TSynLog.Add.Log(sllCustom4, 'CUSTOM EVENT: Report generated');

  // Different event types
  TSynLog.Add.Log(sllNewRun, 'Application session started');
  TSynLog.Add.Log(sllMemory, 'Memory allocation: % bytes', [1024 * 1024]);

  WriteLn('Custom logging complete - various event types used');
end;

end.
