
unit task_repository;

{$mode delphi}{$H+}

interface

uses
  mormot.core.base,
  mormot.core.collections,
  mormot.core.interfaces,
  mormot.orm.core,
  task;

type
  /// Persistence-port for the Task aggregate.
  /// Hides the IRestOrm / SQL / FTS5 / transaction / migration choices from
  /// app-layer services entirely: Add/Update/Delete are atomic (they own the
  /// transaction and keep the FTS5 index in sync internally), and reads return
  /// already-migrated aggregates. Nothing FTS5- or transaction-shaped leaks
  /// through this interface.
  /// Returns `IList<TTask>`; items are freed when the interface ref-count
  /// drops (TTaskObjArray is registered as a T*ObjArray).
  /// Caller still owns TTask instances returned by GetByID.
  ITaskRepository = interface(IInvokable)
    ['{C1D2E3F4-1111-2222-3333-444455556666}']
    /// Load one task by ID (migrated to the current schema), nil if missing.
    function GetByID(aID: TID): TTask;
    /// Persist a new task atomically (transaction + FTS5 sync); 0 on failure.
    function Add(aTask: TTask): TID;
    /// Persist changes to an existing task atomically (transaction + FTS5 sync).
    function Update(aTask: TTask): boolean;
    /// Delete a task atomically (transaction + FTS5 row removal).
    function Delete(aID: TID): boolean;
    /// List tasks (all, or filtered by status), each migrated to current schema.
    function List(const aStatus: RawUtf8): IList<TTask>;
    /// Full-text search (FTS5 with automatic LIKE fallback), optionally filtered
    /// by status; each result migrated to current schema.
    function Search(const aSearchTerm, aStatus: RawUtf8): IList<TTask>;
    function Count: integer;
  end;

implementation

end.
