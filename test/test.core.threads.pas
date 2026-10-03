/// regression tests for mormot.core.threads and low-level thread primitives
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.threads;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.test;


type
  /// execution profile for the comparatively expensive contention loops
  TThreadTestProfile = (
    ttpFast,
    ttpFull);

  /// all exclusive low-level lock primitives with a Lock/TryLock/UnLock API
  // - includes every T*LightLock exclusive primitive from mormot.core.os
  // - TOSLock is kept as the reentrant OS-backed reference implementation
  TExclusiveLockKind = (
    elkLight,
    elkMultiLight,
    elkOSLight,
    elkOS);

  /// R/W lock primitives from mormot.core.os
  TRWLockKind = (
    rwlkLight,
    rwlkRW,
    rwlkOSRWLight);

  /// scheduling preference implemented by a R/W lock
  // - test logic below intentionally does not depend on this value
  // - it is metadata, so adding a differently biased lock does not silently
  //   inherit a writer- or reader-preference assertion
  TRWLockPreference = (
    rwlpUnspecified,
    rwlpReader,
    rwlpWriter);

  TExclusiveStats = record
    Active: cardinal;
    MaxActive: cardinal;
    Acquired: cardinal;
    Failed: cardinal;
    Errors: cardinal;
    Value: cardinal;
    BlockingStarted: cardinal;
    BlockingFinished: cardinal;
    TryStarted: cardinal;
    TryFinished: cardinal;
  end;

  TRWStats = record
    Readers: cardinal;
    Writers: cardinal;
    MaxReaders: cardinal;
    Errors: cardinal;
    Version: cardinal;
    Value1: cardinal;
    Value2: cardinal;
    ReadSuccess: cardinal;
    ReadFailure: cardinal;
    WriteSuccess: cardinal;
    WriteFailure: cardinal;
  end;


  /// regression tests for low-level synchronization primitives
  TTestCoreThreads = class(TSynTestCase)
  protected
    fProfile: TThreadTestProfile;
    // concrete lock storage - the active kind is selected by the enumerates
    fExclusiveKind: TExclusiveLockKind;
    fLight: TLightLock;
    fMultiLight: TMultiLightLock;
    fOSLight: TOSLightLock;
    fOS: TOSLock;
    fRWKind: TRWLockKind;
    fRWLight: TRWLightLock;
    fRW: TRWLock;
    fOSRWLight: TOSRWLightLock;
    // shared worker state - assertions are only made by the main test thread
    fIterations: integer;
    fExclusiveStats: TExclusiveStats;
    fRWStats: TRWStats;
    // TSynEvent accepts one waiter by design, so use one event per direction
    fEntered: TSynEvent;
    fAcquired: TSynEvent;
    fGate: TSynEvent;
    fDone: TSynEvent;
    fProbeResult: cardinal;
    fMainOwns: cardinal;
    procedure Setup; override;
    procedure CleanUp; override;
    function WorkerCount: integer;
    function StressIterations: integer;
    function TransitionIterations: integer;
    function WaitMS: cardinal;
    procedure ResetProbe;
    // exclusive-lock dispatch
    procedure ExclusiveInit;
    procedure ExclusiveDone;
    procedure ExclusiveLock;
    function ExclusiveTryLock: boolean;
    procedure ExclusiveUnLock;
    function ExclusiveIsLocked: boolean;
    // exclusive-lock workers and tests
    procedure ExclusiveTryProbe(Sender: TObject);
    procedure ExclusiveBlockingProbe(Sender: TObject);
    procedure ExclusiveBlockingWorker(Sender: TObject);
    procedure ExclusiveTryWorker(Sender: TObject);
    procedure TestExclusiveKind(Kind: TExclusiveLockKind);
    procedure TestMultiLightSpecial;
    procedure TestOSLockSpecial;
    // R/W dispatch
    procedure RWInit;
    procedure RWDone;
    procedure RWReadLock;
    procedure RWReadUnLock;
    procedure RWWriteLock;
    procedure RWWriteUnLock;
    function RWTryReadLock: boolean;
    function RWTryWriteLock: boolean;
    function RWIsLocked: boolean;
    // R/W workers and tests
    procedure RWReaderProbe(Sender: TObject);
    procedure RWWriterProbe(Sender: TObject);
    procedure RWReaderWorker(Sender: TObject);
    procedure RWWriterWorker(Sender: TObject);
    procedure RWTryReaderWorker(Sender: TObject);
    procedure RWTryWriterWorker(Sender: TObject);
    procedure TestRWKind(Kind: TRWLockKind);
    procedure StressRW;
    procedure StressTryRW;
    // TSynEvent worker
    procedure EventWorker(Sender: TObject);
    // TSynQueue workers
    procedure TSynQueueSlow1(Sender: TObject);
    procedure TSynQueueSlow2(Sender: TObject);
  published
    /// validate TSynEvent/TSynQueue state transitions and cross-thread coverage
    procedure CoreClasses;
    /// validate TLightLock/TMultiLightLock/TOSLightLock/TOSLock
    procedure ExclusiveLocks;
    /// validate TRWLightLock/TRWLock without embedding a fairness assumption
    procedure ReadWriteLocks;
  end;


const
  EXCLUSIVE_LOCK_NAME: array[TExclusiveLockKind] of TShort31 = (
    'TLightLock',
    'TMultiLightLock',
    'TOSLightLock',
    'TOSLock');

  /// same-thread recursive Lock/TryLock contract
  EXCLUSIVE_LOCK_REENTRANT: array[TExclusiveLockKind] of boolean = (
    false,  // TLightLock
    true,   // TMultiLightLock
    false,  // TOSLightLock
    true);  // TOSLock

  /// IsLocked is part of TLightLock/TMultiLightLock but not TOS*Lock API
  EXCLUSIVE_LOCK_HAS_ISLOCKED: array[TExclusiveLockKind] of boolean = (
    true,
    true,
    false,
    false);

  RW_LOCK_NAME: array[TRWLockKind] of RawUtf8 = (
    'TRWLightLock',
    'TRWLock',
    'TOSRWLightLock');

  /// Current mormot.core.os behavior.
  // - TRWLightLock documents writer preference as part of its public contract.
  // - TRWLock currently installs its write bit before draining readers, so new
  //   ReadOnlyLock calls wait as well; its public docs focus on reentrancy and
  //   upgrade semantics rather than promising fairness.
  // - No generic test below asserts this array: preference-specific tests can
  //   be added separately if/when the policy itself should become a contract.
  RW_LOCK_PREFERENCE: array[TRWLockKind] of TRWLockPreference = (
    rwlpWriter,
    rwlpUnspecified,
    rwlpReader);

  RW_LOCK_HAS_TRY: array[TRWLockKind] of boolean = (
    true,   // TRWLightLock.TryReadLock/TryWriteLock
    false,  // TRWLock has richer ReadOnly/ReadWrite/Write API instead
    true);  // TOSRWLightLock

  RW_LOCK_READ_REENTRANT: array[TRWLockKind] of boolean = (
    true,
    true,
    true);

  RW_LOCK_WRITE_REENTRANT: array[TRWLockKind] of boolean = (
    false,
    true,
    false);

  RW_LOCK_UPGRADABLE: array[TRWLockKind] of boolean = (
    false,
    true,
    false);

  // --fullthreads triggers extensive stressing; default is local/PR smoke runs
  PROFILE_STRESS_ITERATIONS: array[TThreadTestProfile] of integer = (
    2000,
    100000);
  PROFILE_TRANSITION_ITERATIONS: array[TThreadTestProfile] of integer = (
    1000,
    50000);
  PROFILE_WORKER_CAP: array[TThreadTestProfile] of integer = (
    4,
    16);
  PROFILE_WAIT_MS: array[TThreadTestProfile] of cardinal = (
    5000,
    30000);


implementation


{ ************ atomic helpers }

procedure TestLockedInc(var Value: cardinal);
begin
  LockedInc32(@Value);
end;

procedure TestLockedDec(var Value: cardinal);
begin
  LockedDec32(@Value);
end;

procedure TestLockedMax(var Value: cardinal; NewValue: cardinal);
var
  OldValue: cardinal;
begin
  repeat
    OldValue := Value;
    if NewValue <= OldValue then
      exit;
  until LockedExc32(Value, NewValue, OldValue);
end;


{ ************ TTestCoreThreads setup/helpers }

procedure TTestCoreThreads.Setup;
begin
  inherited Setup;
  if Executable.Command.Option('fullthreads') then
    fProfile := ttpFull
  else
    fProfile := ttpFast;
  fEntered := TSynEvent.Create;
  fAcquired := TSynEvent.Create;
  fGate := TSynEvent.Create;
  fDone := TSynEvent.Create;
end;

procedure TTestCoreThreads.CleanUp;
begin
  // tasks reference this test case and its events, so release the pool first
  FreeAndNil(fDone);
  FreeAndNil(fGate);
  FreeAndNil(fAcquired);
  FreeAndNil(fEntered);
  inherited CleanUp;
end;

function TTestCoreThreads.WorkerCount: integer;
begin
  result := CpuThreads * 2;
  if result < 2 then
    result := 2;
  if result > PROFILE_WORKER_CAP[fProfile] then
    result := PROFILE_WORKER_CAP[fProfile];
  ThreadCountAdjust(result); // e.g. WinARM PRISM
end;

function TTestCoreThreads.StressIterations: integer;
begin
  result := PROFILE_STRESS_ITERATIONS[fProfile];
end;

function TTestCoreThreads.TransitionIterations: integer;
begin
  result := PROFILE_TRANSITION_ITERATIONS[fProfile];
end;

function TTestCoreThreads.WaitMS: cardinal;
begin
  result := PROFILE_WAIT_MS[fProfile];
end;

procedure TTestCoreThreads.ResetProbe;
begin
  fEntered.ResetEvent;
  fAcquired.ResetEvent;
  fGate.ResetEvent;
  fDone.ResetEvent;
  fProbeResult := 0;
  fMainOwns := 0;
end;


{ ************ exclusive-lock dispatch }

procedure TTestCoreThreads.ExclusiveInit;
begin
  case fExclusiveKind of
    elkLight:
      fLight.Init;
    elkMultiLight:
      fMultiLight.Init;
    elkOSLight:
      fOSLight.Init;
    elkOS:
      fOS.Init;
  end;
end;

procedure TTestCoreThreads.ExclusiveDone;
begin
  case fExclusiveKind of
    elkLight:
      fLight.Done;
    elkMultiLight:
      fMultiLight.Done;
    elkOSLight:
      fOSLight.Done;
    elkOS:
      fOS.Done;
  end;
end;

procedure TTestCoreThreads.ExclusiveLock;
begin
  case fExclusiveKind of
    elkLight:
      fLight.Lock;
    elkMultiLight:
      fMultiLight.Lock;
    elkOSLight:
      fOSLight.Lock;
    elkOS:
      fOS.Lock;
  end;
end;

function TTestCoreThreads.ExclusiveTryLock: boolean;
begin
  case fExclusiveKind of
    elkLight:
      result := fLight.TryLock;
    elkMultiLight:
      result := fMultiLight.TryLock;
    elkOSLight:
      result := fOSLight.TryLock;
    elkOS:
      result := fOS.TryLock;
  else
    result := false;
  end;
end;

procedure TTestCoreThreads.ExclusiveUnLock;
begin
  case fExclusiveKind of
    elkLight:
      fLight.UnLock;
    elkMultiLight:
      fMultiLight.UnLock;
    elkOSLight:
      fOSLight.UnLock;
    elkOS:
      fOS.UnLock;
  end;
end;

function TTestCoreThreads.ExclusiveIsLocked: boolean;
begin
  case fExclusiveKind of
    elkLight:
      result := fLight.IsLocked;
    elkMultiLight:
      result := fMultiLight.IsLocked;
  else
    result := false; // TOSLock/TOSLightLock don't publish this API
  end;
end;


{ ************ exclusive-lock workers }

procedure TTestCoreThreads.ExclusiveTryProbe(Sender: TObject);
begin
  fEntered.SetEvent;
  if ExclusiveTryLock then
  begin
    fProbeResult := 1;
    ExclusiveUnLock;
  end
  else
    fProbeResult := 0;
end;

procedure TTestCoreThreads.ExclusiveBlockingProbe(Sender: TObject);
begin
  fEntered.SetEvent;
  ExclusiveLock;
  try
    TestLockedInc(fExclusiveStats.Active);
    if fExclusiveStats.Active <> 1 then
      TestLockedInc(fExclusiveStats.Errors);
    if fMainOwns <> 0 then
      TestLockedInc(fExclusiveStats.Errors);
    fAcquired.SetEvent;
  finally
    TestLockedDec(fExclusiveStats.Active);
    ExclusiveUnLock;
  end;
end;

procedure TTestCoreThreads.ExclusiveBlockingWorker(Sender: TObject);
var
  i, n: cardinal;
begin
  TestLockedInc(fExclusiveStats.BlockingStarted);
  try
    for i := 1 to fIterations do
    begin
      ExclusiveLock;
      try
        TestLockedInc(fExclusiveStats.Acquired);
        TestLockedInc(fExclusiveStats.Active);
        n := fExclusiveStats.Active;
        TestLockedMax(fExclusiveStats.MaxActive, n);
        if n <> 1 then
          TestLockedInc(fExclusiveStats.Errors);
        inc(fExclusiveStats.Value);
        if i and 127 = 0 then
          SwitchToThread;
      finally
        TestLockedDec(fExclusiveStats.Active);
        ExclusiveUnLock;
      end;
    end;
  finally
    TestLockedInc(fExclusiveStats.BlockingFinished);
  end;
end;

procedure TTestCoreThreads.ExclusiveTryWorker(Sender: TObject);
var
  i, n: cardinal;
begin
  TestLockedInc(fExclusiveStats.TryStarted);
  try
    for i := 1 to fIterations do
    begin
      if not ExclusiveTryLock then
      begin
        TestLockedInc(fExclusiveStats.Failed);
        if i and 7 = 0 then
          SwitchToThread;
        continue;
      end;
      try
        TestLockedInc(fExclusiveStats.Acquired);
        TestLockedInc(fExclusiveStats.Active);
        n := fExclusiveStats.Active;
        TestLockedMax(fExclusiveStats.MaxActive, n);
        if n <> 1 then
          TestLockedInc(fExclusiveStats.Errors);
        inc(fExclusiveStats.Value);
      finally
        TestLockedDec(fExclusiveStats.Active);
        ExclusiveUnLock;
      end;
    end;
  finally
    TestLockedInc(fExclusiveStats.TryFinished);
  end;
end;

procedure TTestCoreThreads.TestExclusiveKind(Kind: TExclusiveLockKind);
var
  i, workers, blocking: integer;
  got: boolean;
begin
  fExclusiveKind := Kind;
  ExclusiveInit;
  try
    // initial/uncontended and same-thread recursion contract
    if EXCLUSIVE_LOCK_HAS_ISLOCKED[Kind] then
      CheckUtf8(not ExclusiveIsLocked, EXCLUSIVE_LOCK_NAME[Kind]);
    CheckUtf8(ExclusiveTryLock, EXCLUSIVE_LOCK_NAME[Kind]);
    if EXCLUSIVE_LOCK_HAS_ISLOCKED[Kind] then
      CheckUtf8(ExclusiveIsLocked, EXCLUSIVE_LOCK_NAME[Kind]);
    got := ExclusiveTryLock;
    CheckUtf8(got = EXCLUSIVE_LOCK_REENTRANT[Kind], EXCLUSIVE_LOCK_NAME[Kind]);
    if got then
      ExclusiveUnLock;
    ExclusiveUnLock;
    if EXCLUSIVE_LOCK_HAS_ISLOCKED[Kind] then
      CheckUtf8(not ExclusiveIsLocked, EXCLUSIVE_LOCK_NAME[Kind]);
    // another thread must never acquire TryLock while main owns the lock
    ResetProbe;
    ExclusiveLock;
    try
      RunTask(ExclusiveTryProbe, EXCLUSIVE_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'TryLock probe entered');
      WaitTasks('TryLock probe done', WaitMS);
      CheckEqual(fProbeResult, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    finally
      ExclusiveUnLock;
    end;
    // real blocking hand-off without Sleep()/polling
    ResetProbe;
    FillCharFast(fExclusiveStats, SizeOf(fExclusiveStats), 0);
    ExclusiveLock;
    try
      fMainOwns := 1;
      RunTask(ExclusiveBlockingProbe, EXCLUSIVE_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'blocking probe entered');
      Check(not fAcquired.Notified, 'must not acquire while main owns lock');
    finally
      fMainOwns := 0;
      ExclusiveUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'blocking probe acquired');
    WaitTasks('blocking probe done', WaitMS);
    CheckEqual(fExclusiveStats.Errors, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    // rapid uncontended state transitions
    for i := 1 to TransitionIterations do
    begin
      CheckUtf8(ExclusiveTryLock, EXCLUSIVE_LOCK_NAME[Kind]);
      ExclusiveUnLock;
      ExclusiveLock;
      ExclusiveUnLock;
    end;
    // mixed blocking/TryLock contention on the persistent task pool
    FillCharFast(fExclusiveStats, SizeOf(fExclusiveStats), 0);
    fIterations := StressIterations;
    workers := WorkerCount;
    blocking := workers div 2;
    if blocking < 1 then
      blocking := 1;
    RunTasks(ExclusiveBlockingWorker, blocking, EXCLUSIVE_LOCK_NAME[Kind]);
    RunTasks(ExclusiveTryWorker, workers - blocking, EXCLUSIVE_LOCK_NAME[Kind]);
    WaitTasks(EXCLUSIVE_LOCK_NAME[Kind], 120 * 1000);
    if false then
      AddConsole('% block=%/% try=%/% acquired=% failed=% active=% errors=%',
        [EXCLUSIVE_LOCK_NAME[Kind],
         fExclusiveStats.BlockingFinished,
         fExclusiveStats.BlockingStarted,
         fExclusiveStats.TryFinished,
         fExclusiveStats.TryStarted,
         fExclusiveStats.Acquired,
         fExclusiveStats.Failed,
         fExclusiveStats.Active,
         fExclusiveStats.Errors]);
    CheckEqual(fExclusiveStats.Active, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    CheckEqual(fExclusiveStats.Errors, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    CheckEqual(fExclusiveStats.MaxActive, 1, EXCLUSIVE_LOCK_NAME[Kind]);
    CheckEqual(fExclusiveStats.Value, fExclusiveStats.Acquired,
      EXCLUSIVE_LOCK_NAME[Kind]);
    // Failed may legitimately remain zero on a single-core/serialized runner;
    // the deterministic foreign-thread probe above already checks failure.
  finally
    ExclusiveDone;
  end;
end;

procedure TTestCoreThreads.TestMultiLightSpecial;
begin
  fMultiLight.Init;
  try
    // explicit recursion depth
    Check(fMultiLight.TryLock);
    Check(fMultiLight.TryLock);
    Check(fMultiLight.IsLocked);
    fMultiLight.UnLock;
    Check(fMultiLight.IsLocked);
    fMultiLight.UnLock;
    Check(not fMultiLight.IsLocked);
  finally
    fMultiLight.Done;
  end;
  // Done deliberately makes following TryLock calls fail
  fMultiLight.Init;
  fMultiLight.Done;
  Check(not fMultiLight.TryLock, 'TMultiLightLock.Done');
  // ForceLock intentionally overrides the previous ownership/state
  fMultiLight.Init;
  try
    fExclusiveKind := elkMultiLight;
    fMultiLight.ForceLock;
    Check(fMultiLight.IsLocked, 'TMultiLightLock.ForceLock');
    // the forced owner is still exclusive to this thread
    ResetProbe;
    RunTask(ExclusiveTryProbe, 'TMultiLightLock.ForceLock');
    Check(fEntered.WaitFor(WaitMS), 'ForceLock probe entered');
    WaitTasks('ForceLock probe done', WaitMS);
    CheckEqual(fProbeResult, 0, 'TMultiLightLock.ForceLock ownership');
  finally
    // don't balance ForceLock with a single UnLock: ForceLock uses a sentinel
    fMultiLight.Done;
  end;
end;

procedure TTestCoreThreads.TestOSLockSpecial;
begin
  // validate the lazy-initialization convenience entry point separately
  FillCharFast(fOS, SizeOf(fOS), 0);
  fOS.LockAndInitIfNeeded;
  try
    Check(fOS.TryLock, 'TOSLock recursive after LockAndInitIfNeeded');
    fOS.UnLock;
  finally
    fOS.UnLock;
    fOS.Done;
  end;
end;


{ ************ R/W dispatch }

procedure TTestCoreThreads.RWInit;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.Init;
    rwlkRW:
      fRW.Init;
    rwlkOSRWLight:
      fOSRWLight.Init;
  end;
end;

procedure TTestCoreThreads.RWDone;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.Done;
    rwlkRW:
      fRW.AssertDone;
    rwlkOSRWLight:
      fOSRWLight.Done;
  end;
end;

procedure TTestCoreThreads.RWReadLock;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.ReadLock;
    rwlkRW:
      fRW.ReadOnlyLock;
    rwlkOSRWLight:
      fOSRWLight.ReadLock;
  end;
end;

procedure TTestCoreThreads.RWReadUnLock;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.ReadUnLock;
    rwlkRW:
      fRW.ReadOnlyUnLock;
    rwlkOSRWLight:
      fOSRWLight.ReadUnlock;
  end;
end;

procedure TTestCoreThreads.RWWriteLock;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.WriteLock;
    rwlkRW:
      fRW.WriteLock;
    rwlkOSRWLight:
      fOSRWLight.WriteLock;
  end;
end;

procedure TTestCoreThreads.RWWriteUnLock;
begin
  case fRWKind of
    rwlkLight:
      fRWLight.WriteUnLock;
    rwlkRW:
      fRW.WriteUnLock;
    rwlkOSRWLight:
      fOSRWLight.WriteUnLock;
  end;
end;

function TTestCoreThreads.RWTryReadLock: boolean;
begin
  case fRWKind of
    rwlkLight:
      result := fRWLight.TryReadLock;
    rwlkOSRWLight:
      result := fOSRWLight.TryReadLock;
  else
    result := false;
  end;
end;

function TTestCoreThreads.RWTryWriteLock: boolean;
begin
  case fRWKind of
    rwlkLight:
      result := fRWLight.TryWriteLock;
    rwlkOSRWLight:
      result := fOSRWLight.TryWriteLock;
  else
    result := false;
  end;
end;

function TTestCoreThreads.RWIsLocked: boolean;
begin
  case fRWKind of
    rwlkLight:
      result := fRWLight.IsLocked;
    rwlkRW:
      result := fRW.IsLocked;
    rwlkOSRWLight:
      result := fOSRWLight.IsLocked;
  else
    result := false;
  end;
end;


{ ************ R/W workers }

procedure TTestCoreThreads.RWReaderProbe(Sender: TObject);
begin
  fEntered.SetEvent;
  RWReadLock;
  try
    fAcquired.SetEvent;
  finally
    RWReadUnLock;
  end;
end;

procedure TTestCoreThreads.RWWriterProbe(Sender: TObject);
begin
  fEntered.SetEvent;
  RWWriteLock;
  try
    fAcquired.SetEvent;
  finally
    RWWriteUnLock;
  end;
end;

procedure TTestCoreThreads.RWReaderWorker(Sender: TObject);
var
  i, n, v: cardinal;
begin
  for i := 1 to fIterations do
  begin
    RWReadLock;
    try
      TestLockedInc(fRWStats.Readers);
      n := fRWStats.Readers;
      TestLockedMax(fRWStats.MaxReaders, n);
      if fRWStats.Writers <> 0 then
        TestLockedInc(fRWStats.Errors);
      v := fRWStats.Value1;
      if i and 127 = 0 then
        SwitchToThread;
      if fRWStats.Value2 <> v * 2 then
        TestLockedInc(fRWStats.Errors);
    finally
      TestLockedDec(fRWStats.Readers);
      RWReadUnLock;
    end;
  end;
end;

procedure TTestCoreThreads.RWWriterWorker(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to fIterations do
  begin
    RWWriteLock;
    try
      TestLockedInc(fRWStats.Writers);
      if fRWStats.Writers <> 1 then
        TestLockedInc(fRWStats.Errors);
      if fRWStats.Readers <> 0 then
        TestLockedInc(fRWStats.Errors);
      inc(fRWStats.Version);
      fRWStats.Value1 := fRWStats.Version;
      if i and 127 = 0 then
        SwitchToThread;
      fRWStats.Value2 := fRWStats.Version * 2;
    finally
      TestLockedDec(fRWStats.Writers);
      RWWriteUnLock;
    end;
  end;
end;

procedure TTestCoreThreads.RWTryReaderWorker(Sender: TObject);
var
  i, v: cardinal;
begin
  for i := 1 to fIterations do
    if RWTryReadLock then
    begin
      TestLockedInc(fRWStats.ReadSuccess);
      try
        TestLockedInc(fRWStats.Readers);
        if fRWStats.Writers <> 0 then
          TestLockedInc(fRWStats.Errors);
        v := fRWStats.Value1;
        if i and 127 = 0 then
          SwitchToThread;
        if fRWStats.Value2 <> v * 2 then
          TestLockedInc(fRWStats.Errors);
      finally
        TestLockedDec(fRWStats.Readers);
        RWReadUnLock;
      end;
    end
    else
    begin
      TestLockedInc(fRWStats.ReadFailure);
      if i and 7 = 0 then
        SwitchToThread;
    end;
end;

procedure TTestCoreThreads.RWTryWriterWorker(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to fIterations do
    if RWTryWriteLock then
    begin
      TestLockedInc(fRWStats.WriteSuccess);
      try
        TestLockedInc(fRWStats.Writers);
        if fRWStats.Writers <> 1 then
          TestLockedInc(fRWStats.Errors);
        if fRWStats.Readers <> 0 then
          TestLockedInc(fRWStats.Errors);
        inc(fRWStats.Version);
        fRWStats.Value1 := fRWStats.Version;
        if i and 127 = 0 then
          SwitchToThread;
        fRWStats.Value2 := fRWStats.Version * 2;
      finally
        TestLockedDec(fRWStats.Writers);
        RWWriteUnLock;
      end;
    end
    else
    begin
      TestLockedInc(fRWStats.WriteFailure);
      if i and 7 = 0 then
        SwitchToThread;
    end;
end;

procedure TTestCoreThreads.StressRW;
var
  readers, writers: integer;
begin
  FillCharFast(fRWStats, SizeOf(fRWStats), 0);
  fIterations := StressIterations;
  readers := WorkerCount div 2;
  if readers < 1 then
    readers := 1;
  writers := WorkerCount - readers;
  if writers < 1 then
    writers := 1;
  RunTasks(RWReaderWorker, readers, RW_LOCK_NAME[fRWKind]);
  RunTasks(RWWriterWorker, writers, RW_LOCK_NAME[fRWKind]);
  WaitTasks(RW_LOCK_NAME[fRWKind], 120 * 1000);
  CheckEqual(fRWStats.Readers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Writers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Errors, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Version, cardinal(writers * fIterations),
    RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value1, fRWStats.Version, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value2, fRWStats.Version * 2, RW_LOCK_NAME[fRWKind]);
  // MaxReaders > 1 is not asserted here: a single-core or serialized scheduler
  // may still run one worker at a time. The deterministic probe below proves
  // that concurrent readers are accepted by the lock itself.
end;

procedure TTestCoreThreads.StressTryRW;
var
  readers, writers: integer;
begin
  if not RW_LOCK_HAS_TRY[fRWKind] then
    exit;
  FillCharFast(fRWStats, SizeOf(fRWStats), 0);
  fIterations := StressIterations;
  readers := WorkerCount div 2;
  if readers < 1 then
    readers := 1;
  writers := WorkerCount - readers;
  if writers < 1 then
    writers := 1;
  RunTasks(RWTryReaderWorker, readers, RW_LOCK_NAME[fRWKind]);
  RunTasks(RWTryWriterWorker, writers, RW_LOCK_NAME[fRWKind]);
  WaitTasks(RW_LOCK_NAME[fRWKind], 120 * 1000);
  CheckEqual(fRWStats.Readers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Writers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Errors, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value1, fRWStats.Version, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value2, fRWStats.Version * 2, RW_LOCK_NAME[fRWKind]);
  // No scheduler-dependent Success/Failure minimums are asserted: both success
  // and exclusion failure cases are covered deterministically in TestRWKind.
end;


procedure TTestCoreThreads.TestRWKind(Kind: TRWLockKind);
var
  i: integer;
  got: boolean;
begin
  fRWKind := Kind;
  RWInit;
  try
    CheckUtf8(not RWIsLocked, RW_LOCK_NAME[Kind]);
    // basic read semantics + recursive reader contract
    RWReadLock;
    try
      CheckUtf8(RWIsLocked, RW_LOCK_NAME[Kind]);
      if RW_LOCK_HAS_TRY[Kind] then
      begin
        CheckUtf8(RWTryReadLock, RW_LOCK_NAME[Kind]);
        RWReadUnLock;
        CheckUtf8(not RWTryWriteLock, RW_LOCK_NAME[Kind]);
      end;
      if RW_LOCK_READ_REENTRANT[Kind] then
      begin
        RWReadLock;
        RWReadUnLock;
      end;
    finally
      RWReadUnLock;
    end;
    CheckUtf8(not RWIsLocked, RW_LOCK_NAME[Kind]);
    // a second thread must be able to acquire a read lock concurrently
    ResetProbe;
    RWReadLock;
    try
      RunTask(RWReaderProbe, RW_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'reader probe entered');
      Check(fAcquired.WaitFor(WaitMS), 'concurrent reader acquired');
      WaitTasks('concurrent reader done', WaitMS);
    finally
      RWReadUnLock;
    end;
    // basic write semantics
    RWWriteLock;
    try
      CheckUtf8(RWIsLocked, RW_LOCK_NAME[Kind]);
      if RW_LOCK_HAS_TRY[Kind] then
      begin
        got := RWTryReadLock;
        CheckUtf8(not got, RW_LOCK_NAME[Kind]);
        if got then
          RWReadUnLock;

        got := RWTryWriteLock;
        CheckUtf8(not got, RW_LOCK_NAME[Kind]);
        if got then
          RWWriteUnLock;
      end;

      if RW_LOCK_WRITE_REENTRANT[Kind] then
      begin
        RWWriteLock;
        RWWriteUnLock;
      end;
    finally
      RWWriteUnLock;
    end;
    CheckUtf8(not RWIsLocked, RW_LOCK_NAME[Kind]);
    // writer waits until an existing reader drains
    ResetProbe;
    RWReadLock;
    try
      RunTask(RWWriterProbe, RW_LOCK_NAME[Kind]);
      CheckUtf8(fEntered.WaitFor(WaitMS),
        'writer probe entered %', [RW_LOCK_NAME[Kind]]);
      CheckUtf8(not fAcquired.Notified,
        'writer must wait for reader %', [RW_LOCK_NAME[Kind]]);
    finally
      RWReadUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'writer acquired after reader drain');
    WaitTasks('writer probe done', WaitMS);
    // reader waits until an existing writer releases
    ResetProbe;
    RWWriteLock;
    try
      RunTask(RWReaderProbe, RW_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'reader probe entered behind writer');
      Check(not fAcquired.Notified, 'reader must wait for writer');
    finally
      RWWriteUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'reader acquired after writer release');
    WaitTasks('reader probe done behind writer', WaitMS);
    // TRWLock-only reentrant/upgradable path
    if RW_LOCK_UPGRADABLE[Kind] then
    begin
      fRW.ReadWriteLock;
      try
        fRW.ReadWriteLock; // reentrant
        fRW.ReadWriteUnLock;
        fRW.WriteLock;     // supported upgrade from ReadWriteLock
        fRW.WriteUnLock;
      finally
        fRW.ReadWriteUnLock;
      end;
      Check(not fRW.IsLocked, 'TRWLock upgrade/reentrancy');
    end;
    // rapid state transitions without any fairness/policy assertion
    for i := 1 to TransitionIterations do
      if RW_LOCK_HAS_TRY[Kind] then
      begin
        CheckUtf8(RWTryReadLock, RW_LOCK_NAME[Kind]);
        RWReadUnLock;
        CheckUtf8(RWTryWriteLock, RW_LOCK_NAME[Kind]);
        RWWriteUnLock;
      end
      else
      begin
        RWReadLock;
        RWReadUnLock;
        RWWriteLock;
        RWWriteUnLock;
      end;
    StressRW;
    StressTryRW;
    CheckUtf8(not RWIsLocked, RW_LOCK_NAME[Kind]);
  finally
    RWDone;
  end;
end;


{ ************ published tests }

procedure TTestCoreThreads.EventWorker(Sender: TObject);
begin
  fEntered.SetEvent;
  if fGate.WaitFor(WaitMS) then
    fDone.SetEvent;
end;

procedure TTestCoreThreads.CoreClasses;
var
  i: integer;
begin
  // simple single-thread state-transition coverage
  CheckEqual(PtrUInt(GetCurrentThreadID), PtrUInt(MainThreadID), 'mainthread');
  for i := 1 to 10 do
  begin
    fEntered.ResetEvent;
    fEntered.SetEvent;
    Check(fEntered.WaitFor(1000), 'WaitFor signal');
    fEntered.SetEvent;
    fEntered.ResetEvent;
    fEntered.SetEvent;
    Check(fEntered.WaitFor(INFINITE), 'WaitFor(INFINITE) signal');
    fEntered.ResetEvent;
    fEntered.SetEvent;
    Check(fEntered.WaitForSafe(1000), 'WaitForSafe signal');
    fEntered.SetEvent;
    fEntered.ResetEvent;
    fEntered.SetEvent;
    Check(fEntered.WaitForSafe(INFINITE), 'WaitForSafe(INFINITE) signal');
  end;
  // validate TSynQueue with all kind of values in a background thread
  Run(TSynQueueSlow1, self, 'TSynQueue1');
  Run(TSynQueueSlow2, self, 'TSynQueue2');
  // real cross-thread handshake: one waiter per TSynEvent instance
  ResetProbe;
  RunTask(EventWorker, 'TSynEvent');
  Check(fEntered.WaitFor(WaitMS), 'event worker entered');
  Check(not fDone.Notified, 'event worker should wait on gate');
  fGate.SetEvent;
  Check(fDone.WaitFor(WaitMS), 'event worker released');
  WaitTasks('TSynEvent task done', WaitMS);
  // TSynQueueSlow1/2 above still use the test framework background worker
  RunWait(false, 5, false);
end;

type
  TNotifyTask = record // a typical event for TSynQueue record validation
    Name: string;
    Payload: RawJson;
    Active: boolean;
  end;
  TNotifyTaskDynArray = array of TNotifyTask;

procedure TTestCoreThreads.TSynQueueSlow1(Sender: TObject);
var
  o, i, j, k, n: integer; // not PtrInt
  f: TSynQueue;
  u, v: RawUtf8;
  r1, r2: TNotifyTask;
  savedint: TIntegerDynArray;
  savedu: TRawUtf8DynArray;
begin
  // validate TSynQueue with integer values
  f := TSynQueue.Create(TypeInfo(TIntegerDynArray));
  try
    for o := 1 to 1000 do
    begin
      checkEqual(f.Count, 0);
      check(not f.Pending);
      for i := 1 to o do
        f.Push(i);
      check(f.Pending);
      checkEqual(f.Count, o);
      check(f.Capacity >= o);
      f.Save(savedint);
      check(Length(savedint) = o);
      check(f.Contains(@o), 'cont0'); // O(n) since queue is a FIFO
      for i := 1 to o do
      begin
        j := -1;
        check(f.Peek(j), 'peek');
        checkEqual(j, i);
        check(f.Contains(@i), 'cont1'); // O(1) since find immediately
        checkEqual(f.PeekCompare(nil), 1);
        checkEqual(f.PeekCompare(@j), 0);
        j := -1;
        checkEqual(f.PeekCompare(@j), 1);
        check(not f.PopEquals(@j, j), 'popeq');
        check(f.Pop(j), 'pop');
        checkEqual(j, i);
        if i < 10 then // is O(n) after Pop()
          check(not f.Contains(@i), 'cont2');
      end;
      check(not f.Pending);
      checkEqual(f.Count, 0);
      checkEqual(f.PeekCompare(@j), -1);
      check(f.Capacity > 0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(not f.Pending);
      checkEqual(f.Count, 0);
      checkEqual(f.Capacity, 0);
      checkEqual(Length(savedint), o);
      for i := 1 to o do
        checkEqual(savedint[i - 1], i);
      n := 0;
      for i := 1 to o do
        if i and 7 = 0 then
        begin
          j := -1;
          check(f.Pop(j));
          check(j and 7 <> 0);
          dec(n);
        end
        else
        begin
          f.Push(i);
          inc(n);
        end;
      checkEqual(f.Count, n);
      check(f.Pending);
      check(f.Contains(@o) = (o and 7 <> 0), 'cont3');
      f.Save(savedint);
      checkEqual(Length(savedint), n);
      for i := 1 to n do
        check(savedint[i - 1] and 7 <> 0);
      for i := 1 to n do
      begin
        j := -1;
        check(f.Peek(j));
        k := -1;
        check(f.Pop(k));
        checkEqual(j, k);
        check(j and 7 <> 0);
      end;
      checkEqual(f.Count, 0);
      check(f.Capacity > 0);
    end;
  finally
    f.Free;
  end;
  // validate TSynQueue with string values
  f := TSynQueue.Create(TypeInfo(TRawUtf8DynArray));
  try
    for o := 1 to 1000 do
    begin
      check(not f.Pending);
      check(f.Count = 0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(f.Count = 0);
      check(f.Capacity = 0);
      n := 0;
      for i := 1 to o do
        if i and 7 = 0 then
        begin
          u := '7';
          check(f.Pop(u));
          check(GetInteger(pointer(u)) and 7 <> 0);
          dec(n);
        end
        else
        begin
          u := UInt32ToUtf8(i);
          f.Push(u);
          inc(n);
        end;
      check(f.Pending);
      check(f.Count = n);
      f.Save(savedu);
      check(Length(savedu) = n);
      for i := 1 to n do
        check(GetInteger(pointer(savedu[i - 1])) and 7 <> 0);
      for i := 1 to n do
      begin
        u := '';
        check(f.Peek(u));
        check(f.Contains(@u), 'cont4'); // O(1) since find immediately
        checkEqual(f.PeekCompare(@u), 0);
        v := '';
        checkEqual(f.PeekCompare(@v), 1);
        check(f.Pop(v));
        check(u = v);
        check(GetInteger(pointer(u)) and 7 <> 0);
      end;
      check(not f.Pending);
      check(f.Count = 0);
      check(f.Capacity > 0);
    end;
    check(Length(savedu) = length(savedint));
  finally
    f.Free;
  end;
  // validate TSynQueue with complex record type
  f := TSynQueue.Create(TypeInfo(TNotifyTaskDynArray));
  try
    checkEqual(f.Count, 0);
    check(not f.Pending);
    for i := 1 to 100 do
    begin
      r1.Name := IntToStr(i);
      r1.Active := i and 3 = 0;
      r1.Payload := Make(['{"int":', i, '}']);
      checkNotEqual(f.Count, i);
      f.Push(r1);
      checkEqual(f.Count, i);
      check(f.Pending);
    end;
    for i := 1 to 100 do
    begin
      check(f.Pending);
      RecordZero(@r2, TypeInfo(TNotifyTask));
      Check(r2.Name = '');
      Check(not r2.Active);
      Check(r2.Payload = '');
      Check(f.Pop(r2));
      Check(r2.Name = IntToStr(i));
      Check(r2.Active = (i and 3 = 0));
    end;
    checkEqual(f.Count, 0);
    Check(not f.Pop(r2));
    checkEqual(f.Count, 0);
  finally
    f.Free;
  end;
end;

const
  WAITERS = 8;

type
  TSynQueuePushTask = class(TSynThreadTask)
  public
    Queue: TSynQueue;
    Value: integer;
    DelayMS: cardinal;
    procedure DoExecute(aCaller: TSynThreadPoolWorkThread); override;
  end;

  TSynQueueWaitTask = class(TSynThreadTask)
  public
    Queue: TSynQueue;
    TimeoutMS: integer;
    Success: PBoolean;
    Value: PInteger;
    procedure DoExecute(aCaller: TSynThreadPoolWorkThread); override;
  end;


{ TSynQueuePushTask }

procedure TSynQueuePushTask.DoExecute(aCaller: TSynThreadPoolWorkThread);
begin
  TSynLog.Add.Log(sllTrace, 'Queue.Push(%)', [Value], self);
  if DelayMS <> 0 then
    SleepHiRes(DelayMS);
  Queue.Push(Value);
end;


{ TSynQueueWaitTask }

procedure TSynQueueWaitTask.DoExecute(aCaller: TSynThreadPoolWorkThread);
var
  v: integer;
begin
  TSynLog.Add.Log(sllTrace, 'Queue.WaitPop(%)', [TimeoutMS], self);
  v := 0;
  Success^ := Queue.WaitPop(TimeoutMS, nil, v);
  Value^ := v;
  TSynLog.Add.Log(sllTrace, 'Queue.WaitPop=%', [Success^], self);
end;


procedure TTestCoreThreads.TSynQueueSlow2(Sender: TObject);
var
  i: PtrInt;
  j, v, expected, mask: integer;
  p: pointer;
  q: TSynQueue;
  tasks: TSynThreadTasks;
  success: array[0 .. WAITERS - 1] of boolean;
  value: array[0 .. WAITERS - 1] of integer;

  procedure WaitForRegisteredWaiters(ExpectedCount: integer);
  var
    timeout: Int64;
  begin
    timeout := mormot.core.os.GetTickCount64 + 2000;
    repeat
      if q.Waiters = ExpectedCount then
        exit;
      SleepHiRes(1);
    until mormot.core.os.GetTickCount64 > timeout;
    CheckEqual(q.Waiters, ExpectedCount,
      'TSynQueue WaitPop registration');
    TSynLog.Add.Log(sllTrace, 'TSynQueueSlow2: WaitForRegisteredWaiters %=%',
      [q.Waiters, ExpectedCount], self);
  end;

  procedure WaitForTasks(const Msg: RawUtf8);
  begin
    TSynLog.Add.Log(sllTrace, 'TSynQueueSlow2: WaitForTasks %', [Msg], self);
    CheckUtf8(tasks.WaitFor(5000), Msg);
  end;

  procedure PushAsync(aValue: integer; aDelayMS: cardinal);
  var
    task: TSynQueuePushTask;
  begin
    task := TSynQueuePushTask.Create;
    task.Queue := q;
    task.Value := aValue;
    task.DelayMS := aDelayMS;
    Check(tasks.Add(task), 'TSynQueue push task');
    TSynLog.Add.Log(sllTrace, 'TSynQueueSlow2: added Push(%,%)',
      [aValue, aDelayMS], self);
  end;

  procedure StartWaiters(aTimeoutMS: integer);
  var
    n: PtrInt;
    task: TSynQueueWaitTask;
  begin
    for n := 0 to WAITERS - 1 do
    begin
      success[n] := false;
      value[n] := 0;
      task := TSynQueueWaitTask.Create;
      task.Queue := q;
      task.TimeoutMS := aTimeoutMS;
      task.Success := @success[n];
      task.Value := @value[n];
      Check(tasks.Add(task), 'TSynQueue WaitPop task');
      TSynLog.Add.Log(sllTrace, 'TSynQueueSlow2: added Wait(%)',
        [aTimeoutMS], self);
    end;
    WaitForRegisteredWaiters(WAITERS);
  end;

  procedure CleanupQueue;
  begin
    q.WaitPopFinalize(1000);
    WaitForTasks('TSynQueue task cleanup');
  end;

begin
  tasks := TSynThreadTasks.Create(WAITERS, 'queue');
  try
    // WaitPop notification
    q := TSynQueue.Create(TypeInfo(TIntegerDynArray));
    try
      // Push deliberately happens after WaitPop() has had enough time to
      // enter OsWaitOnValue() on supported platforms.
      PushAsync(123456, 50);
      v := 0;
      Check(q.WaitPop(2000, nil, v), 'WaitPop notification');
      CheckEqual(v, 123456, 'WaitPop notification value');
      WaitForTasks('WaitPop notification worker');
      CheckEqual(q.Count, 0);
      // WaitPeekLocked notification
      PushAsync(654321, 50);
      p := q.WaitPeekLocked(2000, nil);
      Check(p <> nil, 'WaitPeekLocked notification');
      if p <> nil then
      begin
        CheckEqual(PInteger(p)^, 654321,
          'WaitPeekLocked notification value');
        q.Safe.ReadWriteUnLock;
      end;
      WaitForTasks('WaitPeekLocked notification worker');
      v := 0;
      Check(q.Pop(v));
      CheckEqual(v, 654321);
      CheckEqual(q.Count, 0);
      // compared WaitPop keeps its polling semantics
      v := 11;
      q.Push(v);
      expected := 12;
      v := 0;
      Check(not q.WaitPop(20, nil, v, @expected),
        'WaitPop compared mismatch');
      CheckEqual(q.Count, 1);
      expected := 11;
      Check(q.WaitPop(20, nil, v, @expected),
        'WaitPop compared match');
      CheckEqual(v, 11);
      CheckEqual(q.Count, 0);
      // several concurrent waiters / WakeOne
      StartWaiters(5000);
      // Give registered consumers a chance to actually enter the OS wait.
      // Correctness must not depend on this delay - the sequence protects
      // that race - but it makes the WakeOne path well exercised.
      SleepHiRes(20);
      for i := 1 to WAITERS do
      begin
        j := i;
        q.Push(j);
      end;
      WaitForTasks('multiple WaitPop workers');
      mask := 0;
      for i := 0 to WAITERS - 1 do
      begin
        Check(success[i], 'multiple WaitPop notification');
        Check((value[i] >= 1) and
              (value[i] <= WAITERS),
          'multiple WaitPop value range');
        if (value[i] >= 1) and
           (value[i] <= WAITERS) then
        begin
          j := 1 shl (value[i] - 1);
          Check(mask and j = 0, 'duplicate WaitPop value');
          mask := mask or j;
        end;
      end;
      CheckEqual(mask, (1 shl WAITERS) - 1,
        'all WaitPop values consumed');
      CheckEqual(q.Count, 0);
      CheckEqual(q.Waiters, 0);
    finally
      CleanupQueue;
      q.Free;
    end;
    // WaitPopFinalize must wake all sleepers
    q := TSynQueue.Create(TypeInfo(TIntegerDynArray));
    try
      // Long timeout: these threads should terminate because of
      // WaitPopFinalize(), not because WaitPop naturally timed out.
      StartWaiters(5000);
      SleepHiRes(20);
      q.WaitPopFinalize(1000);
      // On futex/WaitOnAddress platforms WakeAll should make this reach
      // zero immediately. On fallback platforms the existing SleepStep
      // polling should still make it reach zero well inside 1 second.
      CheckEqual(q.Waiters, 0,
        'WaitPopFinalize should release all waiters');
      WaitForTasks('WaitPopFinalize workers');
      for i := 0 to WAITERS - 1 do
        Check(not success[i],
          'WaitPopFinalize WaitPop result');
      // Future WaitPop calls should return immediately and, importantly,
      // should not increase fWaitPopCounter.
      v := 0;
      Check(not q.WaitPop(10, nil, v),
        'WaitPop after WaitPopFinalize');
      CheckEqual(q.Waiters, 0,
        'WaitPop after finalize should not register a waiter');
      // Should therefore also be harmless/immediate if called again.
      q.WaitPopFinalize(10);
      CheckEqual(q.Waiters, 0);
    finally
      CleanupQueue;
      q.Free;
    end;
    // WaitPopFinalize / WaitPopReset with several concurrent sleepers
    q := TSynQueue.Create(TypeInfo(TIntegerDynArray));
    try
      // 1. first generation: all waiters are aborted by Finalize()
      StartWaiters(5000);
      SleepHiRes(20);
      q.WaitPopFinalize(1000);
      // WakeAll should release all futex waiters immediately.
      // The polling fallback should also finish well within 1 second.
      CheckEqual(q.Waiters, 0,
        'WaitPopFinalize should release all waiters');
      WaitForTasks('WaitPopFinalize workers');
      for i := 0 to WAITERS - 1 do
        Check(not success[i],
          'WaitPopFinalize WaitPop result');
      // While finalized, new WaitPop() calls should not even register.
      v := 0;
      Check(not q.WaitPop(10, nil, v),
        'WaitPop after WaitPopFinalize');
      CheckEqual(q.Waiters, 0,
        'WaitPop after finalize should not register a waiter');
      // 2. reset then start a completely fresh generation of waiters
      Check(q.WaitPopReset,
        'WaitPopReset after all waiters terminated');
      StartWaiters(5000);
      SleepHiRes(20);
      // One Push() per waiter: validates that normal WakeOne behavior
      // is working again after WaitPopReset().
      for i := 1 to WAITERS do
      begin
        v := 100 + i;
        q.Push(v);
      end;
      WaitForTasks('WaitPopReset workers');
      mask := 0;
      for i := 0 to WAITERS - 1 do
      begin
        Check(success[i],
          'WaitPop after WaitPopReset');
        Check((value[i] > 100) and
              (value[i] <= 100 + WAITERS),
          'WaitPopReset value range');
        if (value[i] > 100) and
           (value[i] <= 100 + WAITERS) then
        begin
          j := 1 shl (value[i] - 101);
          Check(mask and j = 0,
            'WaitPopReset duplicate value');
          mask := mask or j;
        end;
      end;
      CheckEqual(mask, (1 shl WAITERS) - 1,
        'all WaitPopReset values consumed');
      CheckEqual(q.Count, 0);
      CheckEqual(q.Waiters, 0);
      // 3. make sure Finalize() still works after Reset()
      StartWaiters(5000);
      SleepHiRes(20);
      q.WaitPopFinalize(1000);
      CheckEqual(q.Waiters, 0,
        'second WaitPopFinalize should release all waiters');
      WaitForTasks('second WaitPopFinalize workers');
      for i := 0 to WAITERS - 1 do
        Check(not success[i],
          'second WaitPopFinalize WaitPop result');
      // A second reset cycle should work as well.
      Check(q.WaitPopReset,
        'second WaitPopReset');
      // The queue is operational again.
      v := 123456;
      q.Push(v);
      v := 0;
      Check(q.WaitPop(1000, nil, v),
        'WaitPop after second WaitPopReset');
      CheckEqual(v, 123456);
    finally
      CleanupQueue;
      q.Free;
    end;
  finally
    tasks.Free;
  end;
end;

procedure TTestCoreThreads.ExclusiveLocks;
var
  kind: TExclusiveLockKind;
begin
  for kind := low(TExclusiveLockKind) to high(TExclusiveLockKind) do
    TestExclusiveKind(kind);
  TestMultiLightSpecial;
  TestOSLockSpecial;
end;

procedure TTestCoreThreads.ReadWriteLocks;
var
  kind: TRWLockKind;
begin
  for kind := low(TRWLockKind) to high(TRWLockKind) do
    TestRWKind(kind);
end;


end.
