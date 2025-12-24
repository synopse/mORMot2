/// Profiling Showcase - Nested Call Profiling with mORMot2
// Demonstrates automatic profiling and timing of nested procedure calls
// Port of DelphiMVCFramework profiling_showcase sample
// - this is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program ProfilingShowcase;

{$I ..\..\src\mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
{$endif OSWINDOWS}

uses
  {$I ..\..\src\mormot.uses.inc}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.perf,
  mormot.core.log;

type
  /// Helper class for automatic scope-based profiling
  TProfiler = record
  private
    fTimer: TPrecisionTimer;
    fName: string;
    fThreshold: cardinal; // milliseconds
    class var fLogsOnlyIfOverThreshold: boolean;
  public
    /// Start profiling with a given name
    class function Start(const aName: string; aThresholdMs: cardinal = 0): TProfiler; static;
    /// Stop profiling and log result (called automatically when out of scope)
    procedure Stop;
    /// Execute a procedure N times and log average time
    class procedure Trace(const aName: string; const aProc: TProc; aIterations: integer = 1); static;
    /// Enable threshold-based logging
    class property LogsOnlyIfOverThreshold: boolean
      read fLogsOnlyIfOverThreshold write fLogsOnlyIfOverThreshold;
  end;

  /// Showcase demonstrating profiling patterns
  TProfilingDemo = class
  private
    fCalls: integer;
    procedure ProcA;
    procedure ProcB;
    procedure DoSomething;
    procedure DoSomethingElse;
    procedure NotProfiled;
  public
    procedure RunSimple;
    procedure RunNestedCalls;
    procedure RunNestedCallsInLoop;
    procedure RunTrace;
    procedure RunAll;
  end;

{ TProfiler }

class function TProfiler.Start(const aName: string; aThresholdMs: cardinal): TProfiler;
begin
  Result.fName := aName;
  Result.fThreshold := aThresholdMs;
  Result.fTimer.Start;
end;

procedure TProfiler.Stop;
var
  elapsed: RawUtf8;
  ms: QWord;
begin
  elapsed := fTimer.Stop;
  ms := QWord(fTimer.TimeInMicroSec) div 1000;

  if fLogsOnlyIfOverThreshold and (ms < fThreshold) then
    Exit;

  ConsoleWrite('  [PROFILE] % - %', [fName, elapsed]);
end;

class procedure TProfiler.Trace(const aName: string; const aProc: TProc;
  aIterations: integer);
var
  i: integer;
  timer: TPrecisionTimer;
  totalUs: QWord;
  avgUs: QWord;
begin
  timer.Start;
  totalUs := 0;

  for i := 1 to aIterations do
  begin
    timer.Resume;
    aProc();
    timer.Pause;
    Inc(totalUs, timer.LastTimeInMicroSec);
  end;

  avgUs := totalUs div QWord(aIterations);
  ConsoleWrite('  [TRACE] % - % iterations, avg %us (total %)',
    [aName, aIterations, avgUs, MicroSecToString(totalUs)]);
end;

{ TProfilingDemo }

procedure TProfilingDemo.NotProfiled;
begin
  Sleep(50);
end;

procedure TProfilingDemo.DoSomething;
begin
  begin var lProf := TProfiler.Start('DoSomething');
    Sleep(100 + Random(50));
    lProf.Stop;
  end;
end;

procedure TProfilingDemo.DoSomethingElse;
begin
  begin var lProf := TProfiler.Start('DoSomethingElse');
    Sleep(200 + Random(100));
    DoSomething();
    lProf.Stop;
  end;
end;

procedure TProfilingDemo.ProcA;
begin
  begin var lProf := TProfiler.Start('TProfilingDemo.ProcA');
    Inc(fCalls);
    Sleep(5);
    if fCalls < 20 then
      ProcB;
    lProf.Stop;
  end;
end;

procedure TProfilingDemo.ProcB;
begin
  begin var lProf := TProfiler.Start('TProfilingDemo.ProcB');
    Inc(fCalls);
    Sleep(5);
    if fCalls < 20 then
      ProcA;
    lProf.Stop;
  end;
end;

procedure TProfilingDemo.RunSimple;
begin
  ConsoleWrite(#10'=== Example 1: Simple Profiling ===' + #10);
  DoSomething;
end;

procedure TProfilingDemo.RunNestedCalls;
begin
  ConsoleWrite(#10'=== Example 2: Nested Calls ===' + #10);

  NotProfiled(); // Not profiled

  begin var lProf := TProfiler.Start('NestedCalls');
    DoSomething();
    DoSomethingElse();
    lProf.Stop;
  end;

  NotProfiled(); // Not profiled
end;

procedure TProfilingDemo.RunNestedCallsInLoop;
begin
  ConsoleWrite(#10'=== Example 3: Many Nested Calls ===' + #10);

  fCalls := 0;
  begin var lProf := TProfiler.Start('ManyNestedCalls');
    ProcA;
    lProf.Stop;
  end;

  ConsoleWrite('  Total recursive calls: %', [fCalls]);
end;

procedure TProfilingDemo.RunTrace;
begin
  ConsoleWrite(#10'=== Example 4: Trace Method ===' + #10);

  TProfiler.Trace('Fast operation',
    procedure
    begin
      Sleep(Random(10));
    end, 10);

  TProfiler.Trace('Medium operation',
    procedure
    var
      i: integer;
      sum: integer;
    begin
      sum := 0;
      for i := 1 to 1000 do
        Inc(sum, i);
      if sum > 0 then // Use sum to avoid hint
        Sleep(Random(50));
    end, 10);
end;

procedure TProfilingDemo.RunAll;
begin
  ConsoleWrite('===================================================');
  ConsoleWrite('  mORMot2 Profiling Showcase');
  ConsoleWrite('  Port of DMVC profiling_showcase sample');
  ConsoleWrite('===================================================' + #10);

  RunSimple;
  RunNestedCalls;
  RunNestedCallsInLoop;
  RunTrace;

  ConsoleWrite(#10'=== Testing Threshold Filter ===' + #10);
  ConsoleWrite('Setting threshold to 150ms (only ops > 150ms will be logged)');
  TProfiler.LogsOnlyIfOverThreshold := true;

  begin var lProf := TProfiler.Start('Fast (< 150ms)', 150);
    Sleep(50);
    lProf.Stop;
  end;

  begin var lProf := TProfiler.Start('Slow (> 150ms)', 150);
    Sleep(200);
    lProf.Stop;
  end;

  ConsoleWrite(#10'Notice: Fast operation was not logged (under threshold)');

  ConsoleWrite(#10'===================================================');
  ConsoleWrite('  All examples completed');
  ConsoleWrite('===================================================');
end;

var
  demo: TProfilingDemo;
begin
  try
    Randomize;
    demo := TProfilingDemo.Create;
    try
      demo.RunAll;
      ConsoleWrite(#10'Press Enter to exit...');
      ConsoleWaitForEnterKey;
    finally
      demo.Free;
    end;
  except
    on E: Exception do
      ConsoleWrite('ERROR: %', [E.Message]);
  end;
end.
