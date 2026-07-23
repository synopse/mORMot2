
# CQRS Services Documentation

## Overview

The Task Manager uses **CQRS (Command Query Responsibility Segregation)** with mORMot2's interface-based SOA layer. Each feature context has separate **Query** (read) and **Command** (write) interfaces.

The `§…` and `B.x` references below follow the mORMot2 SAD [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md) guide.

## Architecture

### Why CQRS?

- **Read optimization**: Query services return purpose-built DTOs (e.g., `TTaskListItemDTO` for list views vs. `TTaskViewDTO` for detail views)
- **Write isolation**: Command services handle validation, transactions, FTS5 sync, and return a uniform `TCommandResult`
- **Testability**: Each interface can be tested independently
- **Scalability**: Read and write paths can be optimized separately

### Service Structure

Files are spread across the `dom/ infra/ app/` layers:

```
dom/<entity>/
├── <entity>.pas             → TOrm aggregate (domain object)
├── <entity>_repository.pas  → I<Entity>Repository  (persistence port)
├── <entity>_query.pas       → I<Entity>Query       (read interface)
└── <entity>_command.pas     → I<Entity>Command     (write interface)

infra/<entity>/
└── <entity>_repository_orm.pas      → T<Entity>RepositoryOrm (IRestOrm backend), FTS5 schema, lazy migration

app/<entity>/
├── <entity>_dtos.pas         → packed-record DTO family
├── <entity>_mappers.pas      → OrmTo* / *ToOrm / Apply* (pure)
├── <entity>_query_impl.pas   → T<Entity>QueryService   (sicShared)
└── <entity>_command_impl.pas → T<Entity>CommandService (sicShared)
```

Services receive `I<Entity>Repository` via property injection — a published `Repo`
property auto-resolved during construction (`AutoResolve`), not a constructor argument.
They never touch `IRestOrm` directly. The repository hides all SQL/FTS5 detail in `infra/`.

## Task Context

### ITaskQuery — Read Operations

**File**: `src/dom/tasks/task_query.pas`

```pascal
ITaskQuery = interface(IInvokable)
  ['{A1B2C3D4-1111-2222-3333-444455556666}']
  function GetTaskView(aTaskID: TID): TTaskViewDTO;
  function ListTasks(const aStatus: RawUtf8): TTaskListItemDTODynArray;
  function SearchTasks(const aCriteria: TTaskSearchDTO): TTaskListItemDTODynArray;
end;
```

| Method | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| GetTaskView | aTaskID: TID | TTaskViewDTO | Full task details with comments and tag IDs |
| ListTasks | aStatus: RawUtf8 | TTaskListItemDTODynArray | Compact list items, filtered by status |
| SearchTasks | aCriteria: TTaskSearchDTO | TTaskListItemDTODynArray | FTS5 full-text search with LIKE fallback |

**Implementation** (`app/tasks/task_query_impl.pas`):
- Descends from `TInjectableObjectRest`; publishes `Repo: ITaskRepository`, filled by `AutoResolve` from the repo seeded into the dispatcher's resolver (`InjectInstance`). No `IRestOrm` in sight.
- No monitoring code: per-method timing and counters come from the framework's built-in SOA statistics (`GET /taskmanager/stat?withall=1`; persisted to the `MonitorUsage` table by the composition root).
- Reads receive aggregates **already migrated** to the current schema — the repository's `GetByID` / `List` / `Search` migrate internally, so the service never calls migration itself.
- `SearchTasks` just normalises the status filter and calls `fRepo.Search`; the FTS5-vs-`LIKE` choice and the fallback live entirely inside the repository.
- Lists are returned as `IList<TTask>` — interface-managed lifetime, no `try…finally Free`.
- Read field copy delegated to `OrmToTaskViewDTO` / `OrmToTaskListItemDTO` in `app/tasks/task_mappers.pas` (pure).

### ITaskCommand — Write Operations

**File**: `src/dom/tasks/task_command.pas`

```pascal
ITaskCommand = interface(IInvokable)
  ['{A1B2C3D4-5555-6666-7777-888899990000}']
  function CreateTask(const aData: TTaskCreateDTO): TCommandResult;
  function UpdateTask(const aData: TTaskUpdateDTO): TCommandResult;
  function DeleteTask(aTaskID: TID): TCommandResult;
  function MarkComplete(aTaskID: TID; aIsComplete: boolean): TCommandResult;
  function AddComment(const aData: TTaskAddCommentDTO): TCommandResult;
  function UpdateComment(const aData: TTaskUpdateCommentDTO): TCommandResult;
  function DeleteComment(aTaskID: TID; aCommentIndex: integer): TCommandResult;
  function AddTag(aTaskID: TID; aTagID: TID): TCommandResult;
  function RemoveTag(aTaskID: TID; aTagID: TID): TCommandResult;
end;
```

| Method | Parameters | Description |
|--------|-----------|-------------|
| CreateTask | TTaskCreateDTO | Create task with validation, FTS5 sync |
| UpdateTask | TTaskUpdateDTO | Update task properties, FTS5 sync |
| DeleteTask | TID | Delete task and FTS5 entry |
| MarkComplete | TID, boolean | Toggle completion, update status |
| AddComment | TTaskAddCommentDTO | Append comment to embedded array |
| UpdateComment | TTaskUpdateCommentDTO | Update comment by index, set IsEdited |
| DeleteComment | TID, integer | Remove comment by index |
| AddTag | TID, TID | Add tag reference (idempotent) |
| RemoveTag | TID, TID | Remove tag reference |

**Implementation** (`app/tasks/task_command_impl.pas`):
- Descends from `TInjectableObjectRest`; publishes `Repo: ITaskRepository`, filled by `AutoResolve`. No `IRestOrm` in sight.
- Writes go through the **atomic** `fRepo.Add` / `fRepo.Update` / `fRepo.Delete`: each opens its own transaction and keeps the FTS5 index in sync internally. The service holds no transaction or FTS5 logic — it just translates the outcome to a `TCommandResult`.
- Validation, defaults, and write-side field copy delegated to `CreateTaskDTOToOrm` / `ApplyUpdateTaskDTO` in `app/tasks/task_mappers.pas` (pure).
- Comment and tag mutations are **methods on the `TTask` aggregate** (`AppendComment`, `UpdateCommentContent`, `DeleteComment`, `AddTag`, `RemoveTag`, `SetCompleted`) — the aggregate owns its embedded arrays and its `UpdatedAt` stamp ([§B.3](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b3-two-types-per-entity-not-three)); the service never touches its fields.

## Tag Context

### ITagQuery — Read Operations

**File**: `src/dom/tags/tag_query.pas`

```pascal
ITagQuery = interface(IInvokable)
  ['{B2C3D4E5-1111-2222-3333-444455556677}']
  function GetTagView(aTagID: TID): TTagViewDTO;
  function ListTags: TTagViewDTODynArray;
  function SearchTags(const aSearchTerm: RawUtf8): TTagViewDTODynArray;
end;
```

### ITagCommand — Write Operations

**File**: `src/dom/tags/tag_command.pas`

```pascal
ITagCommand = interface(IInvokable)
  ['{B2C3D4E5-5555-6666-7777-888899990011}']
  function CreateTag(const aData: TTagCreateDTO): TCommandResult;
  function UpdateTag(const aData: TTagUpdateDTO): TCommandResult;
  function DeleteTag(aTagID: TID): TCommandResult;
end;
```

## Shared Types

### TCommandResult (`src/shared_types.pas`)

All command operations return this uniform result:

```pascal
TServiceResult = (srSuccess, ...);   // explicit outcome discriminator

TCommandResult = packed record
  Success: boolean;         // convenience: true exactly when Status = srSuccess
  ID: TID;
  ErrorMessage: RawUtf8;
  Status: TServiceResult;   // the explicit discriminator
end;
```

`Status` is the explicit outcome; `Success` is a convenience flag that is true
exactly when `Status = srSuccess` (kept for existing callers/tests). The enum is
registered with `Rtti.RegisterType(TypeInfo(TServiceResult))` so `RegisterFromText`
can resolve the `Status` field.

Helper functions: `CommandSuccess(aID)`, `CommandSuccess`, `CommandError(aMessage, aStatus)`.

## DTO Registration

All DTOs are `packed record` types registered in their unit's `initialization` section using `Rtti.RegisterFromText`:

```pascal
initialization
  Rtti.RegisterFromText(TypeInfo(TTaskCreateDTO),
    'Title RawUtf8 Description RawUtf8 Priority integer DueDate RawUtf8');
```

This is required on FPC 3.2.2 because the compiler does not emit field-level RTTI for records. Without `RegisterFromText`, mORMot cannot deserialize JSON into record parameters.

## REST API Format

**Endpoint pattern**: `POST /taskmanager/{ServiceName}.{MethodName}`

**Request body**: JSON array of parameters (positional)

**Response**: JSON object with a `Result` key — mORMot's default SOA response wrapper (no extra configuration required)

```bash
# Example: Create a task
curl -X POST http://localhost:8080/taskmanager/TaskCommand.CreateTask \
  -H "Content-Type: application/json" \
  -d '[{"title":"Buy groceries","description":"Milk and bread","priority":2,"dueDate":"2026-04-01"}]'

# Response
{"Result":{"success":true,"iD":4,"errorMessage":"","status":0}}
```

## Service Registration

The composition root in `serv/app/ServAppTaskManager.pas` (`RunTaskManagerDaemon`) splits persistence from the public dispatcher (§15.9), then seeds the repositories into the dispatcher's resolver (`InjectInstance`) so services pick them up via `AutoResolve`. (`task_manager.pas` is a thin entry point that just calls into it.)

```pascal
// Register the RPC CQRS interfaces (NOT ITaskRepository / ITagRepository —
// those are local DI ports, not part of the SOA wire contract).
TInterfaceFactory.RegisterInterfaces([
  TypeInfo(ITaskQuery), TypeInfo(ITaskCommand),
  TypeInfo(ITagQuery),  TypeInfo(ITagCommand)
]);

// Persistence: internal TRestServerDB, not added to TRestHttpServer.
Persistence := TRestServerDB.Create(PersistenceModel, DBFileName);
Persistence.CreateMissingTables;

// Repositories wrap the persistence ORM.
TaskRepoImpl := TTaskRepositoryOrm.Create(Persistence.Orm);
TagRepoImpl  := TTagRepositoryOrm.Create(Persistence.Orm);

// Public dispatcher: services-only TRestServerFullMemory.
Dispatcher := TRestServerFullMemory.Create(DispatcherModel);
// Seed the dispatcher's resolver so AutoResolve resolves the published Repo
// property. Must precede ServiceDefine, which (class form) eagerly constructs
// each service via CreateWithResolverAndRest and runs AutoResolve.
Dispatcher.ServiceContainer.InjectInstance([TaskRepoImpl, TagRepoImpl]);
Dispatcher.ServiceDefine(TTaskQueryService,   [ITaskQuery],   sicShared);
Dispatcher.ServiceDefine(TTaskCommandService, [ITaskCommand], sicShared);
Dispatcher.ServiceDefine(TTagQueryService,    [ITagQuery],    sicShared);
Dispatcher.ServiceDefine(TTagCommandService,  [ITagCommand],  sicShared);

// HTTP exposes only the dispatcher.
HttpServer := TRestHttpServer.Create(Settings.HttpPort, [Dispatcher], '+', useBidirSocket);
```

Key points:

- **Class-form `ServiceDefine`** + `sicShared` → the framework owns construction; each service's published `Repo` is filled by `AutoResolve` against the dispatcher's resolver (seeded via `InjectInstance`).
- **Two-server layout**: `Persistence` is in-process state behind the repositories — there is no HTTP path to the ORM tables. All client traffic goes through services on `Dispatcher`.
- The CLI mirrors this layout in `AppTaskManagerClientLocal.pas` (LOCAL mode); `AppTaskManagerClientRemote.pas` (REMOTE mode) resolves services through `HttpClient.Services` directly. `cli_client.pas` selects the backend via the `-dLOCAL_MODE` compile flag.

## Testing

Tests use mORMot's `TSynTestCase` / `TSynTests` framework:

- **TTestTask** (`task_tests.pas`): 10 test methods
- **TTestTag** (`tag_tests.pas`): 8 test methods

Tests run automatically at server startup and verify all CQRS operations end-to-end.

## See Also

- [ARCHITECTURE.md](ARCHITECTURE.md) - Overall architecture
- [API.md](API.md) - Complete API reference with examples
- [DEVELOPMENT.md](DEVELOPMENT.md) - Development guide
