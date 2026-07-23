
# Task Manager Architecture

## Overview

This document describes the architecture of the mORMot2 Task Manager. It combines:

- **DDD (Domain-Driven Design)** aggregates with embedded sub-structures.
- **CQRS (Command Query Responsibility Segregation)** — separate read and write interfaces per context.
- **Hexagonal layering** — `dom/` (TOrm aggregates + interfaces), `infra/` (IRestOrm-backed impls), `app/` (DTOs, mappers, services), `serv/` (daemon / composition root — the only place that names concrete `infra/` classes).
- **`TInjectableObjectRest` + `AutoResolve`** for repository injection — services publish a `Repo` interface property, the framework fills it from the dispatcher's own resolver, seeded per-dispatcher via `ServiceContainer.InjectInstance`.
- **Two-server daemon shape** (§15.9): an internal `TRestServerDB` for persistence + a public `TRestServerFullMemory` dispatcher that hosts the CQRS services. Only the dispatcher is exposed over HTTP.
- **`sicShared` SOA registration** in class form (`ServiceDefine(TServiceClass, [I], sicShared)`).

> **Pattern references.** The `§…`, `A.x`, and `B.x` citations throughout this
> document refer to the mORMot2 SAD
> [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md)
> guide, which this project deliberately follows.

## Architecture Layers

### 1. Domain Layer — DDD Aggregates

#### Task Aggregate (`src/dom/tasks/task.pas`)

`TTask` is the **aggregate root** with embedded sub-structures:

```
TTask (Aggregate Root)
├── Title, Description, Priority, DueDate, Status, IsCompleted
├── CreatedAt, UpdatedAt, SchemaVersion
├── Comments: TTaskCommentDynArray  ← embedded JSON array
└── TagIDs: TIDDynArray             ← embedded JSON array of Tag IDs
```

- **Comments** are embedded within the Task aggregate as a JSON array in a TEXT column — there is no separate Comment table.
- **TagIDs** are stored as a JSON array of Tag entity IDs — there is no junction table.
- **TTaskComment** is a packed record: Content, Author, CreatedAt, UpdatedAt, IsEdited.

**Design decisions**:
- Embedding comments inside the aggregate ensures consistency without joins.
- Tag references by ID allow the Tag aggregate to remain independent.
- Priority uses a 1-5 scale (Low, Medium, High, Urgent, Critical).
- The `TOrm` class doubles as the domain object — there is no separate `TSynPersistent` shadow class.

#### Tag Aggregate (`src/dom/tags/tag.pas`)

`TTag` is an independent aggregate with Name, Color, CreatedAt, SchemaVersion.

#### FTS5 Index (`TTaskFts5`)

A `TOrmFts5Porter` class indexes Task Title and Description for full-text search. It is a persistence-only schema, so the class itself is declared in `infra/tasks/task_repository_orm.pas` (not with the `dom/` aggregate). The FTS5 index is kept in sync **inside** `TTaskRepositoryOrm` — its `Add` / `Update` / `Delete` re-index the row as part of the same transaction, so the application layer never sees FTS5 at all.

#### Schema Versioning

Both aggregates carry a `SchemaVersion` field. The repository upgrades older records lazily on read (`GetByID` / `List` / `Search` migrate each aggregate before returning it), so callers always receive current-schema objects:

```pascal
procedure MigrateTaskIfNeeded(const aOrm: IRestOrm; aTask: TTask);
begin
  if aTask.SchemaVersion >= TASK_CURRENT_VERSION then exit;
  if aTask.SchemaVersion < 1 then begin
    if aTask.Status = '' then aTask.Status := GetDefaultStatus;
    if (aTask.Priority < 1) or (aTask.Priority > 5) then aTask.Priority := 2;
  end;
  aTask.SchemaVersion := TASK_CURRENT_VERSION;
  aOrm.Update(aTask);
end;
```

The procedure lives in `infra/<entity>/<entity>_repository_orm.pas` (it writes through `IRestOrm`, so it is a persistence concern, not part of the `dom/` aggregate) and is invoked **internally by the repository's read methods** — it is not part of the port surface, so the app layer never triggers (or even knows about) migration, and never calls `IRestOrm` itself.

### 2. Repository Layer (Persistence Ports)

For each aggregate there is a **persistence port** declared in `dom/` and an **ORM-backed implementation** in `infra/`:

| Port (dom)                                      | Implementation (infra)                                  |
|------------------------------------------------|---------------------------------------------------------|
| `ITaskRepository` (`dom/tasks/task_repository.pas`) | `TTaskRepositoryOrm` (`infra/tasks/task_repository_orm.pas`) |
| `ITagRepository`  (`dom/tags/tag_repository.pas`)   | `TTagRepositoryOrm`  (`infra/tags/tag_repository_orm.pas`)   |

The port surface for `ITaskRepository`:

```pascal
ITaskRepository = interface(IInvokable)
  ['{C1D2E3F4-1111-2222-3333-444455556666}']
  function GetByID(aID: TID): TTask;                              // migrated on read
  function Add(aTask: TTask): TID;                                // atomic: tx + FTS5 sync
  function Update(aTask: TTask): boolean;                         // atomic: tx + FTS5 sync
  function Delete(aID: TID): boolean;                             // atomic: tx + FTS5 removal
  function List(const aStatus: RawUtf8): IList<TTask>;            // migrated on read
  function Search(const aSearchTerm, aStatus: RawUtf8): IList<TTask>; // FTS5+LIKE, migrated
  function Count: integer;
end;
```

The port is deliberately **free of transaction, FTS5 and migration mechanics**: writes are atomic (each owns its transaction and keeps the FTS5 index in sync internally), and reads return already-migrated aggregates. Transactions, `SyncFts5`/`DeleteFts5`, the `MATCH`/`LIKE` choice and `MigrateTaskIfNeeded` are all private to `TTaskRepositoryOrm`. This is what keeps `app/` ignorant of SQL/FTS5 (Recommended-Patterns [§B.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b1-storage-behind-an-interface)/[§B.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure)).

Lists return `IList<TTask>` from `mormot.core.collections` — interface-managed lifetime, no caller `try…finally Free` (§14.6 / §15.7). `TTaskObjArray` is registered via `Rtti.RegisterObjArray` in `dom/tasks/task.pas` so the list frees its `TTask` items when its ref-count drops. `GetByID` still returns a caller-owned `TTask`. `Search` performs the FTS5 query (with an automatic `LIKE` fallback) and hydrates full aggregates internally, so callers just project the results.

`ITaskRepository` / `ITagRepository` are **local DI ports**, not RPC services — they are not passed to `TInterfaceFactory.RegisterInterfaces`, only seeded into the dispatcher's resolver via `ServiceContainer.InjectInstance` at the composition root.

**Why this matters:** the application layer (`app/*_impl.pas`) depends only on the *interface*. To swap SQLite for SQL Server, Oracle, or an in-memory stub: write a new `T<Entity>Repository<Backend>` in `infra/`, seed it at the composition root. The service code does not change.

### 3. CQRS Service Layer

Each context has **separate Query and Command interfaces**, both declared in `dom/<entity>/`:

#### Task Context

**ITaskQuery** (`dom/tasks/task_query.pas`):
```pascal
ITaskQuery = interface(IInvokable)
  function GetTaskView(aTaskID: TID): TTaskViewDTO;
  function ListTasks(const aStatus: RawUtf8): TTaskListItemDTODynArray;
  function SearchTasks(const aCriteria: TTaskSearchDTO): TTaskListItemDTODynArray;
end;
```

**ITaskCommand** (`dom/tasks/task_command.pas`):
```pascal
ITaskCommand = interface(IInvokable)
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

#### Tag Context

`ITagQuery` and `ITagCommand` follow the same shape (see `dom/tags/`).

The split buys:
- Per-direction authorization (read clients cannot accidentally call a write method).
- Read side could later be served by a replica or cache without touching the write path.
- Self-documenting endpoints — `TaskQuery.ListTasks` vs `TaskCommand.CreateTask`.

### 4. Typed DTOs (`app/<entity>/<entity>_dtos.pas`)

A **family** of operation-specific `packed record` DTOs per entity — no single fat shape:

- Read side: `TTaskViewDTO`, `TTaskListItemDTO`, `TTaskSearchDTO`, `TTagViewDTO`.
- Write side: `TTaskCreateDTO`, `TTaskUpdateDTO`, `TTaskAddCommentDTO`, `TTaskUpdateCommentDTO`, `TTagCreateDTO`, `TTagUpdateDTO`.
- Command return: `TCommandResult` (Success, ID, ErrorMessage) — shared in `shared_types.pas`.

All registered via `Rtti.RegisterFromText(TypeInfo(T), 'field definitions')` in the unit's `initialization` section for JSON serialization on FPC 3.2.2.

**JSON casing** — each DTO unit normalizes its wire contract to camelCase right after the `RegisterFromText` calls (the FPC ordering required by *Recommended Patterns* [A.7](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a7-copying-between-objects-records-and-dtos), Gotcha 3) with `Rtti[TypeInfo(T)].Props.NameChangeCase(DtoJsonCase)`, where `DtoJsonCase = scLowerCaseFirst` is the single switch in `shared_types.pas`. This is applied to the DTO record types **only — never the `TOrm`** (`NameChangeCase` is a no-op on ORM JSON anyway, since the ORM serializes via its own `TOrmProperties`). `scLowerCaseFirst` lowercases the first character only, so a bare `ID` field goes out as `iD`; see [API.md](API.md) for the exact wire names. The embedded `TTaskComment` record follows the same convention (its call lives in `dom/tasks/task.pas`, alongside its own `RegisterFromText`).

`string` is **forbidden** on the wire — `RawUtf8` for text (zero-copy at the HTTP boundary), `RawByteString` / `TSQLRawBlob` for binary.

### 5. Mappers (`app/<entity>/<entity>_mappers.pas`)

A dedicated mapper unit per entity holds **pure** procedures with the prescribed naming:

- `OrmTo<Dto>` — read direction. Mechanical field copy, no I/O.
- `<Dto>ToOrm` — create direction. Fills a fresh `TOrm`, sets `CreatedAt`, defaults, validations.
- `Apply<Dto>` — update direction. Mutates an already-loaded `TOrm`, preserves `CreatedAt`, refreshes `UpdatedAt`.

**Rules:**
1. No DB lookups, no `IRestOrm` calls, no `Repo.` calls inside mappers. Stealth N+1 queries die here.
2. Create and update are different functions, never one. Prevents an update from accidentally resetting a server-managed field like `CreatedAt`.
3. Reads are mechanical; writes carry real logic (normalization, defaults, validation). They're grouped separately within the unit.

This makes the mappers the cheapest unit-test target in the project.

### 6. Service Implementations (`app/<entity>/<entity>_*_impl.pas`)

Each service implementation:

- Inherits from `TInjectableObjectRest` (`mormot.soa.server`).
- Publishes `Repo: I<Entity>Repository` as a **published interface property** — `AutoResolve` fills it from the dispatcher's resolver (seeded via `InjectInstance`) on construction.
- Delegates persistence to the repository; delegates field copying to the mappers.
- Logs operations via `TSynLog`.
- Contains **no monitoring code** — per-method timing and counters come from the framework's built-in SOA statistics (see [Performance](#performance)).

```pascal
TTaskCommandService = class(TInjectableObjectRest, ITaskCommand)
private
  fRepo: ITaskRepository;
public
  // ... ITaskCommand methods
published
  property Repo: ITaskRepository
    read fRepo write fRepo;              // filled by AutoResolve
end;
```

The service has no `Create(aRepo)` overload — the framework's `TServiceFactoryServer.CreateInstance` calls `CreateWithResolverAndRest`, which runs `AutoResolve` against the dispatcher's `TServiceContainer`, where the repos were seeded via `ServiceContainer.InjectInstance([repoImpl])` before the services were defined.

**Thread-safety contract** (sicShared):
- Fields are immutable after construction (`fRepo`).
- `TOrm` instances are created per call and never shared between threads.
- Transactions begin/commit/rollback through the repository — never around a shared `TOrm`.

Example flow for `CreateTask`:
```
1. Mapper validates TTaskCreateDTO and fills a fresh TTask
       (CreateTaskDTOToOrm sets defaults + server-managed fields)
2. Repo.Add(Task) → newID          (atomic: opens a transaction, inserts,
       re-indexes FTS5 and commits — or rolls back — entirely inside infra)
3. Return TCommandResult.Success(newID)
```

The service code reads top-to-bottom as a use-case script. All SQL / FTS5 / transaction detail is behind the repo; all field-copy detail is behind the mapper; and mutations that touch the aggregate's own state (comments, tags, completion) are methods on `TTask` itself ([§B.3](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b3-two-types-per-entity-not-three)), so the service never reaches into its fields.

### 7. Composition Roots

#### Server (`src/serv/app/ServAppTaskManager.pas`)

`src/task_manager.pas` is a thin entry point — it pulls in `cthreads` and calls `RunTaskManagerDaemon`. The composition root itself lives in `serv/app/ServAppTaskManager.pas`, the only unit that names the concrete `infra/` repository classes.

A two-server layout (§15.9) — persistence stays internal, only the dispatcher is exposed:

```pascal
Settings := TTaskManagerSettings.Create;   // TSynDaemonSettings descendant
// 1. Persistence: TRestServerDB with the TOrm tables — NOT added to HTTP.
PersistenceModel := TOrmModel.Create([TTask, TTag, TTaskFts5], Settings.Root);
Persistence := TRestServerDB.Create(PersistenceModel, DBFileName);
Persistence.CreateMissingTables;

// 2. Repositories wrap the persistence ORM.
TaskRepoImpl := TTaskRepositoryOrm.Create(Persistence.Orm);
TagRepoImpl  := TTagRepositoryOrm.Create(Persistence.Orm);

// 3. Public dispatcher: services-only, empty TOrmModel, same Root.
DispatcherModel := TOrmModel.Create([], Settings.Root);
Dispatcher := TRestServerFullMemory.Create(DispatcherModel);
// Seed the dispatcher's own DI resolver so AutoResolve injects the repos into
// the services (Recommended Patterns B.5). ServiceContainer materializes the
// resolver; this MUST precede ServiceDefine, which eagerly builds each
// sicShared instance and runs AutoResolve.
Dispatcher.ServiceContainer.InjectInstance([TaskRepoImpl, TagRepoImpl]);
Dispatcher.ServiceDefine(TTaskQueryService,   [ITaskQuery],   sicShared);
Dispatcher.ServiceDefine(TTaskCommandService, [ITaskCommand], sicShared);
Dispatcher.ServiceDefine(TTagQueryService,    [ITagQuery],    sicShared);
Dispatcher.ServiceDefine(TTagCommandService,  [ITagCommand],  sicShared);

// 4. HTTP exposes only the dispatcher.
HttpServer := TRestHttpServer.Create(Settings.HttpPort, [Dispatcher], '+', useBidirSocket);
```

Key shape:
- `ServiceDefine` uses the **class form** (not an instance). The framework constructs each service via `CreateWithResolverAndRest`, which runs `AutoResolve` and fills the published `Repo` property.
- Only `Dispatcher` is added to `TRestHttpServer`. `Persistence` is in-process state behind the repositories — clients cannot bypass the services to hit the ORM.

#### CLI consumer (`src/cli_client.pas` + `AppTaskManagerClient*`)

The consumer follows the *Recommended Patterns* [B.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure) client layout — a transport-agnostic port plus two interchangeable backends — so the command code is identical whether the services run in-process or over HTTP (location transparency, [A.6](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a6-minimum-recommended-server-configuration)):

- **`AppTaskManagerClient.pas`** declares `ITaskManagerClient` (the four CQRS ports) and `RegisterTaskManagerInterfaces`. It is the only unit `cli_client` talks to for wiring.
- **`AppTaskManagerClientLocal.pas`** ([A.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a61-trusted--local-deployment)) — `CreateClient` builds the server's two-server layout (persistence + dispatcher), seeds the repos into the dispatcher's resolver (`InjectInstance`), and resolves services via `Dispatcher.Services.Resolve`.
- **`AppTaskManagerClientRemote.pas`** ([A.6.2](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a62-internet-facing-or-distributed-deployment)) — `CreateClient` connects to the daemon's dispatcher and resolves through the HTTP client's stub container:

```pascal
fHttpClient := TRestHttpClient.Create('localhost', aSettings.HttpPort, fModel);
fHttpClient.ServiceDefine([ITaskQuery, ITaskCommand, ITagQuery, ITagCommand], sicShared);
fHttpClient.Services.Resolve(ITaskCommand, fTaskCmd);
fHttpClient.Services.Resolve(ITaskQuery,   fTaskQry);
```

`cli_client.pas` is pure presentation: it parses arguments, calls `CreateClient` once, and drives the resolved ports. It `uses` exactly one backend, selected by `{$define LOCAL_MODE}`. The returned `ITaskManagerClient` owns its transport (servers or HTTP client) and tears it down when the last reference is released. No local service composition over `HttpClient.Orm` — the CLI calls the same SOA endpoints as any other client.

### 8. Tests (`tests/<entity>/<entity>_tests.pas`)

Tests are `TSynTestCase` descendants. They take both servers (`SetTestServers(persistence, dispatcher)`): seed data through `persistence.Orm`, resolve services through `dispatcher.Services` — exactly the way real clients do:

```pascal
Check(fServer.Services.Resolve(ITaskCommand, CmdSvc), 'resolve ITaskCommand');
Check(fServer.Services.Resolve(ITaskQuery,   QrySvc), 'resolve ITaskQuery');
```

Wrapped in `TTaskManagerTests = class(TSynTests)` at startup of `serv/app/ServAppTaskManager.pas`. Run automatically before the HTTP server starts.

**Coverage:** 18 test methods across the two suites (Task: 10 methods, Tag: 8 methods); all assertions pass at startup.

### 9. Web Clients (`static/index.html`, `static/app.html`)

Two single-page applications call the SOA endpoints directly: `index.html` is the
streamlined client (task CRUD + statistics), and `app.html` is the full-featured
client that additionally exercises tags and comments. Both share the same
`soaCall()` helper (see [WEB_CLIENTS.md](WEB_CLIENTS.md) for the full comparison):

```javascript
async function soaCall(service, method, params) {
    const url = `${API_BASE}/${service}.${method}`;
    const response = await fetch(url, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params)
    });
    return response.json();
}
```

## Data Flow

### Creating a Task (Command)

```
Browser → POST /taskmanager/TaskCommand.CreateTask
       → [TTaskCreateDTO as JSON]
       → HTTP Server → Service Router → TTaskCommandService.CreateTask
       → task_mappers.CreateTaskDTOToOrm  (pure: validate + fill)
       → ITaskRepository.Add(Task)   (atomic inside infra: transaction +
             insert + FTS5 re-index + commit/rollback)
       → TCommandResult → JSON → Browser
```

### Listing Tasks (Query)

```
Browser → POST /taskmanager/TaskQuery.ListTasks
       → ["pending"]
       → HTTP Server → Service Router → TTaskQueryService.ListTasks
       → ITaskRepository.List("pending")   (migrates each row inside infra)
       → for each: task_mappers.OrmToTaskListItemDTO
       → TTaskListItemDTODynArray → JSON → Browser
```

## Performance

1. **Built-in SOA statistics** — the framework collects per-method call counts, timing, input/output sizes and error counts for every interface-based service (`mlInterfaces` is in `TRestServer.StatLevels` by default), with zero code in the service classes. Live numbers: `GET /taskmanager/stat?withall=1`. The composition root additionally wires `TSynMonitorUsageRest`, which aggregates the counters per hour/day/month/year into the `TOrmMonitorUsage` table of the SQLite database. One subtlety: `TRestServer` does not free `StatUsage` in its destructor, so the composition root releases it explicitly (`Dispatcher.StatUsage := nil`, which saves pending slices) while the persistence server is still alive.
2. **Async HTTP server** (`useBidirSocket`) — high concurrency.
3. **FTS5 full-text search** — indexed search with `LIKE` fallback when FTS5 is unavailable.
4. **SQLite WAL mode** — concurrent reads alongside writes.
5. **Packed-record DTOs** — value-type carriers, no class allocations on the wire.
6. **`sicShared` services** — single instance, no per-request construction.

## Security Considerations

**Current** (development):
- No authentication. `TAuthGuard` is the natural next step (see audit gap #6).
- CORS enabled for all origins.
- Local network access only.

## Dependencies

- **mORMot2** — `mormot.core.*`, `mormot.orm.*`, `mormot.rest.*`, `mormot.soa.*`, `mormot.net.*`, `mormot.db.raw.sqlite3.static`.
