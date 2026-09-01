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
    procedure RunWorker(const Worker: TNotifyEvent; const Name: RawUtf8);
    procedure RunWorkers(const Worker: TNotifyEvent; Count: integer;
      const Name: RawUtf8);
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
  published
    /// validate TSynEvent state transitions and a real cross-thread handshake
    procedure _TSynEvent;
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
  FreeAndNil(fDone);
  FreeAndNil(fGate);
  FreeAndNil(fAcquired);
  FreeAndNil(fEntered);
  inherited CleanUp;
end;

function TTestCoreThreads.WorkerCount: integer;
begin
  // TLoggedWorker itself limits simultaneous execution to CpuThreads and queues
  // forced jobs, so asking for more than CpuThreads also validates queue reuse.
  result := CpuThreads * 2;
  if result < 2 then
    result := 2;
  if result > PROFILE_WORKER_CAP[fProfile] then
    result := PROFILE_WORKER_CAP[fProfile];
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

procedure TTestCoreThreads.RunWorker(
  const Worker: TNotifyEvent; const Name: RawUtf8);
begin
  Run(Worker, self, Name,
    {Threaded=}true, {NotifyTask=}false, {ForcedThreaded=}true);
end;


procedure TTestCoreThreads.RunWorkers(const Worker: TNotifyEvent;
  Count: integer; const Name: RawUtf8);
begin
  while Count > 0 do
  begin
    RunWorker(Worker, Name);
    dec(Count);
  end;
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
  fDone.SetEvent;
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
  fDone.SetEvent;
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
      RunWorker(ExclusiveTryProbe, EXCLUSIVE_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'TryLock probe entered');
      Check(fDone.WaitFor(WaitMS), 'TryLock probe done');
      CheckEqual(fProbeResult, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    finally
      ExclusiveUnLock;
    end;
    RunWait(false, 5, false);
    // real blocking hand-off without Sleep()/polling
    ResetProbe;
    FillCharFast(fExclusiveStats, SizeOf(fExclusiveStats), 0);
    ExclusiveLock;
    try
      fMainOwns := 1;
      RunWorker(ExclusiveBlockingProbe, EXCLUSIVE_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'blocking probe entered');
      Check(not fAcquired.Notified, 'must not acquire while main owns lock');
    finally
      fMainOwns := 0;
      ExclusiveUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'blocking probe acquired');
    Check(fDone.WaitFor(WaitMS), 'blocking probe done');
    RunWait(false, 5, false);
    CheckEqual(fExclusiveStats.Errors, 0, EXCLUSIVE_LOCK_NAME[Kind]);
    // rapid uncontended state transitions
    for i := 1 to TransitionIterations do
    begin
      CheckUtf8(ExclusiveTryLock, EXCLUSIVE_LOCK_NAME[Kind]);
      ExclusiveUnLock;
      ExclusiveLock;
      ExclusiveUnLock;
    end;
    // mixed blocking/TryLock contention; ForcedThreaded=true lets TLoggedWorker
    // queue surplus jobs and reuse worker threads within this batch
    FillCharFast(fExclusiveStats, SizeOf(fExclusiveStats), 0);
    fIterations := StressIterations;
    workers := WorkerCount;
    blocking := workers div 2;
    if blocking < 1 then
      blocking := 1;
    RunWorkers(ExclusiveBlockingWorker, blocking, EXCLUSIVE_LOCK_NAME[Kind]);
    RunWorkers(ExclusiveTryWorker, workers - blocking, EXCLUSIVE_LOCK_NAME[Kind]);
    RunWait(false, 120, false);
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
    RunWorker(ExclusiveTryProbe, 'TMultiLightLock.ForceLock');
    Check(fEntered.WaitFor(WaitMS), 'ForceLock probe entered');
    Check(fDone.WaitFor(WaitMS), 'ForceLock probe done');
    CheckEqual(fProbeResult, 0, 'TMultiLightLock.ForceLock ownership');
    RunWait(false, 5, false);
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
  fDone.SetEvent;
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
  fDone.SetEvent;
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
  RunWorkers(RWReaderWorker, readers, RW_LOCK_NAME[fRWKind]);
  RunWorkers(RWWriterWorker, writers, RW_LOCK_NAME[fRWKind]);
  RunWait(false, 120, false);
  CheckEqual(fRWStats.Readers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Writers, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Errors, 0, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Version, cardinal(writers * fIterations),
    RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value1, fRWStats.Version, RW_LOCK_NAME[fRWKind]);
  CheckEqual(fRWStats.Value2, fRWStats.Version * 2, RW_LOCK_NAME[fRWKind]);
  // MaxReaders > 1 is not asserted here: TLoggedWorker may have a one-thread
  // runtime on a single-core target. Concurrent readers are proven separately
  // with a deterministic main-thread + background-worker handshake.
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
  RunWorkers(RWTryReaderWorker, readers, RW_LOCK_NAME[fRWKind]);
  RunWorkers(RWTryWriterWorker, writers, RW_LOCK_NAME[fRWKind]);
  RunWait(false, 120, false);
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
      RunWorker(RWReaderProbe, RW_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'reader probe entered');
      Check(fAcquired.WaitFor(WaitMS), 'concurrent reader acquired');
      Check(fDone.WaitFor(WaitMS), 'concurrent reader done');
    finally
      RWReadUnLock;
    end;
    RunWait(false, 5, false);
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
      RunWorker(RWWriterProbe, RW_LOCK_NAME[Kind]);
      CheckUtf8(fEntered.WaitFor(WaitMS),
        'writer probe entered %', [RW_LOCK_NAME[Kind]]);
      CheckUtf8(not fAcquired.Notified,
        'writer must wait for reader %', [RW_LOCK_NAME[Kind]]);
    finally
      RWReadUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'writer acquired after reader drain');
    Check(fDone.WaitFor(WaitMS), 'writer probe done');
    RunWait(false, 5, false);
    // reader waits until an existing writer releases
    ResetProbe;
    RWWriteLock;
    try
      RunWorker(RWReaderProbe, RW_LOCK_NAME[Kind]);
      Check(fEntered.WaitFor(WaitMS), 'reader probe entered behind writer');
      Check(not fAcquired.Notified, 'reader must wait for writer');
    finally
      RWWriteUnLock;
    end;
    Check(fAcquired.WaitFor(WaitMS), 'reader acquired after writer release');
    Check(fDone.WaitFor(WaitMS), 'reader probe done behind writer');
    RunWait(false, 5, false);
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

procedure TTestCoreThreads._TSynEvent;
var
  i: integer;
begin
  // preserve the existing single-thread state-transition coverage from
  // TTestCoreBase._TSynQueue, but keep TSynEvent in its own test.
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
  // real cross-thread handshake: one waiter per TSynEvent instance
  ResetProbe;
  RunWorker(EventWorker, 'TSynEvent');
  Check(fEntered.WaitFor(WaitMS), 'event worker entered');
  Check(not fDone.Notified, 'event worker should wait on gate');
  fGate.SetEvent;
  Check(fDone.WaitFor(WaitMS), 'event worker released');
  RunWait(false, 5, false);
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
