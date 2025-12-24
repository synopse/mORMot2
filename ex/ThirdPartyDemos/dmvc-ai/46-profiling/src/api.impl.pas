unit api.impl;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.interfaces,
  api.interfaces;

type
  /// Implementation of profiling API
  // Demonstrates mORMot2 performance monitoring patterns:
  // - TPrecisionTimer for manual timing
  // - ISynLog for automatic RAII-style profiling
  TProfilingApi = class(TInterfacedObject, IProfilingApi)
  protected
    fCalls: integer;

    procedure ProcA;
    procedure ProcB;
    procedure DoSomething;
    procedure DoSomethingElse;
    procedure NotProfiled;
  public
    /// Index endpoint - demonstrates recursive call profiling
    function Index: RawUtf8;

    /// Nested profiling demo - equivalent to DMVC ProfilerSample1
    // Shows ISynLog for block profiling and nested calls
    function ProfilerSample1: RawUtf8;

    /// Simple profiling demo - equivalent to DMVC ProfilerSample2
    // Shows basic ISynLog usage
    function ProfilerSample2: RawUtf8;
  end;


implementation


{ TProfilingApi }

function TProfilingApi.Index: RawUtf8;
var
  log: ISynLog;
begin
  // ISynLog provides RAII-style profiling - automatically logs entry/exit
  // with high-resolution timing
  log := TSynLog.Enter(Self, 'Index');

  fCalls := 0;
  ProcA;

  Result := 'Hello mORMot2 (profiled) World - ' +
    FormatUtf8('Recursive calls made: %', [fCalls]);

  // log.Leave is called automatically when ISynLog goes out of scope
  // Timing is written to TSynLog.Family log file
end;

procedure TProfilingApi.ProcA;
var
  log: ISynLog;
begin
  log := TSynLog.Enter(Self, 'ProcA');
  Inc(fCalls);
  ProcB;
end;

procedure TProfilingApi.ProcB;
var
  log: ISynLog;
begin
  log := TSynLog.Enter(Self, 'ProcB');
  Inc(fCalls);
  if fCalls < 20 then
    ProcA;
end;

function TProfilingApi.ProfilerSample1: RawUtf8;
var
  log: ISynLog;
begin
  // This demonstrates the mORMot2 equivalent of DMVC's scoped profiling
  // NotProfiled() calls are intentionally outside profiled blocks

  NotProfiled; // Not profiled

  // Begin profiled block
  log := TSynLog.Enter(Self, 'ProfilerSample1.MainBlock');
  DoSomething;
  DoSomethingElse;
  Result := 'Just executed ProfilerSample1 with nested profiling';
  // log.Leave called automatically here

  NotProfiled; // Not profiled
end;

function TProfilingApi.ProfilerSample2: RawUtf8;
var
  log: ISynLog;
begin
  // Simple profiling example
  log := TSynLog.Enter(Self, 'ProfilerSample2');

  Sleep(100); // Simulate work

  Result := 'Hello World (profiled with 100ms sleep)';
end;

procedure TProfilingApi.DoSomething;
var
  log: ISynLog;
begin
  log := TSynLog.Enter(Self, 'DoSomething');
  Sleep(100); // Simulate work
end;

procedure TProfilingApi.DoSomethingElse;
var
  log: ISynLog;
begin
  log := TSynLog.Enter(Self, 'DoSomethingElse');
  Sleep(100);
  DoSomething; // Nested call - will show in profiling tree
end;

procedure TProfilingApi.NotProfiled;
begin
  // No ISynLog here - this won't appear in profiling output
  Sleep(100);
end;


end.
