unit worker;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.data;

type
  /// A worker object that performs expensive operations
  // Pooling these workers reduces allocation overhead
  TExpensiveWorker = class
  private
    fWorkerId: Integer;
    fCreatedAt: Int64;
    fUsageCount: Integer;
  public
    /// Initialize the worker (simulates expensive setup)
    constructor Create(aWorkerId: Integer); reintroduce;
    /// Cleanup the worker
    destructor Destroy; override;

    /// Perform some work (simulates processing)
    function DoWork(const aData: RawUtf8): RawUtf8;

    /// Reset worker state for reuse
    procedure Reset;

    property WorkerId: Integer read fWorkerId;
    property UsageCount: Integer read fUsageCount;
  end;

  /// Thread-safe object pool for TExpensiveWorker instances
  TWorkerPool = class
  private
    fAvailableWorkers: TSynObjectListLocked;
    fMaxPoolSize: Integer;
    fNextWorkerId: Integer;
    // Statistics
    fTotalCreated: Integer;
    fTotalDestroyed: Integer;
    fTotalBorrowed: Integer;
    fTotalReturned: Integer;
    fPeakActive: Integer;
    fCurrentActive: Integer;

    function CreateWorker: TExpensiveWorker;
  public
    /// Create the worker pool with max size
    constructor Create(aMaxPoolSize: Integer = 10); reintroduce;
    /// Destroy the pool and all workers
    destructor Destroy; override;

    /// Borrow a worker from the pool (creates new if needed)
    function BorrowWorker: TExpensiveWorker;

    /// Return a worker to the pool (or destroys if pool is full)
    procedure ReturnWorker(aWorker: TExpensiveWorker);

    /// Get current pool statistics
    procedure GetStats(out aTotalCreated, aTotalDestroyed, aCurrentActive,
      aCurrentPooled, aTotalBorrowed, aTotalReturned, aPeakActive: Integer);

    /// Reset statistics
    procedure ResetStats;

    /// Get pool configuration info
    function GetInfo: RawUtf8;
  end;


implementation


{ TExpensiveWorker }

constructor TExpensiveWorker.Create(aWorkerId: Integer);
begin
  inherited Create;
  fWorkerId := aWorkerId;
  fCreatedAt := GetTickCount64;
  fUsageCount := 0;

  // Simulate expensive initialization
  SleepHiRes(5);  // 5ms delay
end;

destructor TExpensiveWorker.Destroy;
begin
  // Simulate cleanup overhead
  SleepHiRes(2);  // 2ms delay
  inherited Destroy;
end;

function TExpensiveWorker.DoWork(const aData: RawUtf8): RawUtf8;
begin
  Inc(fUsageCount);
  // Simulate some processing work
  SleepHiRes(10);  // 10ms work
  Result := FormatUtf8('Worker #% processed: "%" (usage count: %)',
    [fWorkerId, aData, fUsageCount]);
end;

procedure TExpensiveWorker.Reset;
begin
  // Reset any stateful data (in this simple demo, nothing to reset)
  // In real scenarios, might clear buffers, reset counters, etc.
end;


{ TWorkerPool }

constructor TWorkerPool.Create(aMaxPoolSize: Integer);
begin
  inherited Create;
  fMaxPoolSize := aMaxPoolSize;
  fAvailableWorkers := TSynObjectListLocked.Create(True); // Owns objects
  fNextWorkerId := 1;
  fTotalCreated := 0;
  fTotalDestroyed := 0;
  fTotalBorrowed := 0;
  fTotalReturned := 0;
  fPeakActive := 0;
  fCurrentActive := 0;
end;

destructor TWorkerPool.Destroy;
begin
  fAvailableWorkers.Free;
  inherited Destroy;
end;

function TWorkerPool.CreateWorker: TExpensiveWorker;
begin
  Result := TExpensiveWorker.Create(InterlockedIncrement(fNextWorkerId));
  InterlockedIncrement(fTotalCreated);
end;

function TWorkerPool.BorrowWorker: TExpensiveWorker;
var
  active: Integer;
begin
  InterlockedIncrement(fTotalBorrowed);

  // Try to get from pool first
  fAvailableWorkers.Safe.WriteLock;
  try
    if fAvailableWorkers.Count > 0 then
    begin
      Result := fAvailableWorkers[fAvailableWorkers.Count - 1];
      fAvailableWorkers.Delete(fAvailableWorkers.Count - 1);
    end
    else
    begin
      Result := CreateWorker;
    end;
  finally
    fAvailableWorkers.Safe.WriteUnLock;
  end;

  // Update active count
  active := InterlockedIncrement(fCurrentActive);
  if active > fPeakActive then
    fPeakActive := active;

  Result.Reset;
end;

procedure TWorkerPool.ReturnWorker(aWorker: TExpensiveWorker);
begin
  if aWorker = nil then
    Exit;

  InterlockedIncrement(fTotalReturned);
  InterlockedDecrement(fCurrentActive);

  fAvailableWorkers.Safe.WriteLock;
  try
    // Return to pool if not full, otherwise destroy
    if fAvailableWorkers.Count < fMaxPoolSize then
    begin
      fAvailableWorkers.Add(aWorker);
    end
    else
    begin
      aWorker.Free;
      InterlockedIncrement(fTotalDestroyed);
    end;
  finally
    fAvailableWorkers.Safe.WriteUnLock;
  end;
end;

procedure TWorkerPool.GetStats(out aTotalCreated, aTotalDestroyed,
  aCurrentActive, aCurrentPooled, aTotalBorrowed, aTotalReturned,
  aPeakActive: Integer);
begin
  fAvailableWorkers.Safe.ReadOnlyLock;
  try
    aTotalCreated := fTotalCreated;
    aTotalDestroyed := fTotalDestroyed;
    aCurrentActive := fCurrentActive;
    aCurrentPooled := fAvailableWorkers.Count;
    aTotalBorrowed := fTotalBorrowed;
    aTotalReturned := fTotalReturned;
    aPeakActive := fPeakActive;
  finally
    fAvailableWorkers.Safe.ReadOnlyUnLock;
  end;
end;

procedure TWorkerPool.ResetStats;
begin
  fTotalBorrowed := 0;
  fTotalReturned := 0;
  fPeakActive := 0;
  // Note: Don't reset created/destroyed counts as they reflect actual pool lifecycle
end;

function TWorkerPool.GetInfo: RawUtf8;
begin
  Result := FormatUtf8('WorkerPool: MaxSize=%, CurrentPooled=%',
    [fMaxPoolSize, fAvailableWorkers.Count]);
end;


end.
