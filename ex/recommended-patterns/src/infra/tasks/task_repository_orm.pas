
unit task_repository_orm;

{$ifdef FPC}{$mode delphi}{$H+}{$endif}

interface

uses
  SysUtils,
  contnrs,
  mormot.core.base,
  mormot.core.collections,
  mormot.core.data,
  mormot.core.log,
  mormot.orm.core,
  task,
  task_repository;

type
  /// FTS5 full-text search index for Task title and description.
  /// Persistence-side schema (NOT part of the Task aggregate, hence here in
  /// infra/ rather than dom/): the repository owns it and keeps it in sync on
  /// every write.
  TTaskFts5 = class(TOrmFts5Porter)
  private
    fTitle: RawUtf8;
    fDescription: RawUtf8;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Description: RawUtf8 read fDescription write fDescription;
  end;

  /// IRestOrm-backed implementation of ITaskRepository.
  /// All SQL / FTS5 / transaction / migration knowledge lives in this unit;
  /// the app layer never sees it. Writes are atomic (one transaction each,
  /// keeping the FTS5 index in sync); reads return already-migrated aggregates.
  TTaskRepositoryOrm = class(TInterfacedObject, ITaskRepository)
  private
    fOrm: IRestOrm;
    function RetrieveToIList(const aWhere: RawUtf8;
      const aBoundArgs: array of const): IList<TTask>;
    function ListByLike(const aPattern, aStatus: RawUtf8): IList<TTask>;
    function FtsSearchIDs(const aSearchTerm: RawUtf8): TIDDynArray;
    procedure SyncFts5(aTaskID: TID; const aTitle, aDescription: RawUtf8);
    procedure DeleteFts5(aTaskID: TID);
    procedure MigrateAll(const aList: IList<TTask>);
  public
    constructor Create(const aOrm: IRestOrm);
    function GetByID(aID: TID): TTask;
    function Add(aTask: TTask): TID;
    function Update(aTask: TTask): boolean;
    function Delete(aID: TID): boolean;
    function List(const aStatus: RawUtf8): IList<TTask>;
    function Search(const aSearchTerm, aStatus: RawUtf8): IList<TTask>;
    function Count: integer;
  end;

implementation

{ Lazy migration: upgrades a task record to the current schema version.
  Persistence concern (it writes through IRestOrm), so it lives here in infra/
  alongside the repository rather than in the dom/ aggregate. }
procedure MigrateTaskIfNeeded(const aOrm: IRestOrm; aTask: TTask);
begin
  if aTask.SchemaVersion >= TASK_CURRENT_VERSION then
    exit;
  // Version 0 -> 1: ensure default values for new aggregate fields
  if aTask.SchemaVersion < 1 then
  begin
    if aTask.Status = '' then
      aTask.Status := GetDefaultStatus;
    if (aTask.Priority < 1) or (aTask.Priority > 5) then
      aTask.Priority := 2;
    // Comments and TagIDs arrays default to empty via zero-init
  end;
  aTask.SchemaVersion := TASK_CURRENT_VERSION;
  aOrm.Update(aTask);
end;

constructor TTaskRepositoryOrm.Create(const aOrm: IRestOrm);
begin
  inherited Create;
  fOrm := aOrm;
end;

function TTaskRepositoryOrm.RetrieveToIList(const aWhere: RawUtf8;
  const aBoundArgs: array of const): IList<TTask>;
var
  Raw: TObjectList;
  i: integer;
begin
  Result := Collections.NewList<TTask>([], TypeInfo(TTaskObjArray));
  Raw := fOrm.RetrieveList(TTask, aWhere, aBoundArgs, '');
  if Raw = nil then
    exit;
  try
    // Move TTask ownership from TObjectList into the IList; OwnsObjects=false
    // prevents Raw.Free from double-freeing them.
    Raw.OwnsObjects := false;
    Result.Capacity := Raw.Count;
    for i := 0 to Raw.Count - 1 do
      Result.Add(TTask(Raw[i]));
  finally
    Raw.Free;
  end;
end;

procedure TTaskRepositoryOrm.MigrateAll(const aList: IList<TTask>);
var
  i: integer;
begin
  for i := 0 to aList.Count - 1 do
    MigrateTaskIfNeeded(fOrm, aList[i]);
end;

function TTaskRepositoryOrm.GetByID(aID: TID): TTask;
begin
  Result := TTask.Create(fOrm, aID);
  if Result.ID = 0 then
  begin
    Result.Free;
    Result := nil;
    exit;
  end;
  // reads return aggregates already at the current schema version
  MigrateTaskIfNeeded(fOrm, Result);
end;

function TTaskRepositoryOrm.Add(aTask: TTask): TID;
begin
  Result := 0;
  if not fOrm.TransactionBegin(TTask) then
    raise Exception.Create('TTaskRepositoryOrm.Add: could not begin transaction');
  try
    Result := fOrm.Add(aTask, true);
    if Result > 0 then
    begin
      SyncFts5(Result, aTask.Title, aTask.Description);
      fOrm.Commit(1, true);
    end
    else
      fOrm.RollBack;
  except
    fOrm.RollBack;
    raise;
  end;
end;

function TTaskRepositoryOrm.Update(aTask: TTask): boolean;
begin
  if not fOrm.TransactionBegin(TTask) then
    raise Exception.Create('TTaskRepositoryOrm.Update: could not begin transaction');
  try
    Result := fOrm.Update(aTask);
    if Result then
    begin
      SyncFts5(aTask.ID, aTask.Title, aTask.Description);
      fOrm.Commit(1, true);
    end
    else
      fOrm.RollBack;
  except
    fOrm.RollBack;
    raise;
  end;
end;

function TTaskRepositoryOrm.Delete(aID: TID): boolean;
begin
  if not fOrm.TransactionBegin(TTask) then
    raise Exception.Create('TTaskRepositoryOrm.Delete: could not begin transaction');
  try
    DeleteFts5(aID);
    Result := fOrm.Delete(TTask, aID);
    if Result then
      fOrm.Commit(1, true)
    else
      fOrm.RollBack;
  except
    fOrm.RollBack;
    raise;
  end;
end;

function TTaskRepositoryOrm.List(const aStatus: RawUtf8): IList<TTask>;
begin
  if aStatus <> '' then
    Result := RetrieveToIList('Status=?', [aStatus])
  else
    Result := RetrieveToIList('', []);
  MigrateAll(Result);
end;

function TTaskRepositoryOrm.ListByLike(const aPattern, aStatus: RawUtf8): IList<TTask>;
begin
  if aStatus <> '' then
    Result := RetrieveToIList(
      '(Title LIKE ? OR Description LIKE ?) AND Status=?',
      [aPattern, aPattern, aStatus])
  else
    Result := RetrieveToIList(
      'Title LIKE ? OR Description LIKE ?',
      [aPattern, aPattern]);
end;

function TTaskRepositoryOrm.FtsSearchIDs(const aSearchTerm: RawUtf8): TIDDynArray;
var
  Fts: TObjectList;
  i: integer;
begin
  SetLength(Result, 0);
  Fts := fOrm.RetrieveList(TTaskFts5, 'TaskFts5 MATCH ?', [aSearchTerm]);
  if Fts = nil then
    exit;
  try
    SetLength(Result, Fts.Count);
    for i := 0 to Fts.Count - 1 do
      Result[i] := TTaskFts5(Fts[i]).IDValue;
  finally
    Fts.Free;
  end;
end;

function TTaskRepositoryOrm.Search(const aSearchTerm, aStatus: RawUtf8): IList<TTask>;
var
  FtsIDs: TIDDynArray;
  FtsOk: boolean;
  Task: TTask;
  i: integer;
begin
  // FTS5 path: resolve matching IDs, then hydrate (and migrate) each aggregate.
  FtsOk := false;
  try
    FtsIDs := FtsSearchIDs(aSearchTerm);
    FtsOk := true;
  except
    on E: Exception do
      TSynLog.Add.Log(sllWarning,
        'Search: FTS5 unavailable (%), falling back to LIKE', [E.Message]);
  end;
  if FtsOk and (Length(FtsIDs) > 0) then
  begin
    Result := Collections.NewList<TTask>([], TypeInfo(TTaskObjArray));
    for i := 0 to High(FtsIDs) do
    begin
      Task := GetByID(FtsIDs[i]); // GetByID migrates; ownership moves to the list
      if Task = nil then
        continue;
      if (aStatus <> '') and (Task.Status <> aStatus) then
      begin
        Task.Free;
        continue;
      end;
      Result.Add(Task);
    end;
    exit;
  end;

  // LIKE fallback (FTS5 unavailable or no hit)
  Result := ListByLike('%' + aSearchTerm + '%', aStatus);
  MigrateAll(Result);
end;

function TTaskRepositoryOrm.Count: integer;
begin
  Result := fOrm.TableRowCount(TTask);
end;

procedure TTaskRepositoryOrm.SyncFts5(aTaskID: TID;
  const aTitle, aDescription: RawUtf8);
var
  Fts: TTaskFts5;
begin
  fOrm.Delete(TTaskFts5, aTaskID);
  Fts := TTaskFts5.Create;
  try
    Fts.IDValue := aTaskID;
    Fts.Title := aTitle;
    Fts.Description := aDescription;
    fOrm.Add(Fts, true);
  finally
    Fts.Free;
  end;
end;

procedure TTaskRepositoryOrm.DeleteFts5(aTaskID: TID);
begin
  fOrm.Delete(TTaskFts5, aTaskID);
end;

end.
