unit api.impl;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.threads,
  mormot.core.log,
  mormot.soa.server,
  api.interfaces,
  worker;

type
  /// Implementation of Object Pool API
  TObjectPoolApi = class(TInterfacedObject, IObjectPoolApi)
  private
    fWorkerPool: TWorkerPool;
  public
    /// Initialize the API with a worker pool
    constructor Create; reintroduce;
    /// Cleanup
    destructor Destroy; override;

    // IObjectPoolApi implementation
    function ExecuteSimpleOperation(const aData: RawUtf8): TOperationResultDTO;
    function ExecuteParallelOperations(aCount: Integer): RawUtf8;
    function GetPoolStats: TPoolStatsDTO;
    procedure ResetPoolStats;
    function GetPoolInfo: RawUtf8;
  end;


implementation


{ TObjectPoolApi }

constructor TObjectPoolApi.Create;
begin
  inherited Create;
  // Create pool with max 10 workers
  fWorkerPool := TWorkerPool.Create(10);
  TSynLog.Add.Log(sllInfo, 'ObjectPoolApi initialized with worker pool');
end;

destructor TObjectPoolApi.Destroy;
begin
  fWorkerPool.Free;
  inherited Destroy;
end;

function TObjectPoolApi.ExecuteSimpleOperation(const aData: RawUtf8): TOperationResultDTO;
var
  worker: TExpensiveWorker;
  startTime: Int64;
begin
  startTime := GetTickCount64;

  // Borrow worker from pool
  worker := fWorkerPool.BorrowWorker;
  try
    Result.Success := True;
    Result.Message := worker.DoWork(aData);
    Result.WorkerId := worker.WorkerId;
    Result.ExecutionTimeMs := (GetTickCount64 - startTime) / 1000.0;

    TSynLog.Add.Log(sllDebug, 'ExecuteSimpleOperation: Worker #% processed in %ms',
      [worker.WorkerId, Result.ExecutionTimeMs * 1000]);
  finally
    // Return worker to pool
    fWorkerPool.ReturnWorker(worker);
  end;
end;

function TObjectPoolApi.ExecuteParallelOperations(aCount: Integer): RawUtf8;
var
  i: Integer;
  results: array of TOperationResultDTO;
  totalTime: Int64;
  avgTime: Double;
begin
  if aCount <= 0 then
    aCount := 10;
  if aCount > 100 then
    aCount := 100; // Limit to prevent abuse

  SetLength(results, aCount);
  totalTime := GetTickCount64;

  // Execute operations sequentially (but using pool)
  for i := 0 to aCount - 1 do
  begin
    results[i] := ExecuteSimpleOperation(
      FormatUtf8('Operation #%', [i + 1]));
  end;

  totalTime := GetTickCount64 - totalTime;
  avgTime := totalTime / aCount;

  if results[High(results)].WorkerId <= 10 then
    Result := FormatUtf8(
      '{"operations":%,"totalTimeMs":%,"avgTimeMs":%,"poolReused":true}',
      [aCount, totalTime, avgTime])
  else
    Result := FormatUtf8(
      '{"operations":%,"totalTimeMs":%,"avgTimeMs":%,"poolReused":false}',
      [aCount, totalTime, avgTime]);

  TSynLog.Add.Log(sllInfo, 'ExecuteParallelOperations: % ops in %ms (avg %ms)',
    [aCount, totalTime, avgTime]);
end;

function TObjectPoolApi.GetPoolStats: TPoolStatsDTO;
var
  created, destroyed, active, pooled, borrowed, returned, peak: Integer;
begin
  fWorkerPool.GetStats(created, destroyed, active, pooled,
    borrowed, returned, peak);

  Result.TotalCreated := created;
  Result.TotalDestroyed := destroyed;
  Result.CurrentActive := active;
  Result.CurrentPooled := pooled;
  Result.TotalBorrowed := borrowed;
  Result.TotalReturned := returned;
  Result.PeakActive := peak;
end;

procedure TObjectPoolApi.ResetPoolStats;
begin
  fWorkerPool.ResetStats;
  TSynLog.Add.Log(sllInfo, 'Pool statistics reset');
end;

function TObjectPoolApi.GetPoolInfo: RawUtf8;
begin
  Result := fWorkerPool.GetInfo;
end;


end.
