unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Statistics about object pool usage
  TPoolStatsDTO = packed record
    TotalCreated: Integer;
    TotalDestroyed: Integer;
    CurrentActive: Integer;
    CurrentPooled: Integer;
    TotalBorrowed: Integer;
    TotalReturned: Integer;
    PeakActive: Integer;
  end;

  /// Result of a pooled operation
  TOperationResultDTO = packed record
    Success: Boolean;
    Message: RawUtf8;
    ExecutionTimeMs: Double;
    WorkerId: Integer;
  end;

  /// Object Pool API interface
  IObjectPoolApi = interface(IInvokable)
    ['{B8F5A3C2-4D1E-4F9B-8E2A-9C7F6B3D5E8A}']

    /// Execute a simple operation using pooled worker
    function ExecuteSimpleOperation(const aData: RawUtf8): TOperationResultDTO;

    /// Execute multiple operations in parallel to test pooling
    function ExecuteParallelOperations(aCount: Integer): RawUtf8;

    /// Get current pool statistics
    function GetPoolStats: TPoolStatsDTO;

    /// Reset pool statistics
    procedure ResetPoolStats;

    /// Get pool configuration info
    function GetPoolInfo: RawUtf8;
  end;


implementation


end.
