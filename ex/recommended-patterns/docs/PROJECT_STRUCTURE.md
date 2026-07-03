
# Project Structure

## Overview

The mORMot2 Task Manager is organized as **DDD + CQRS feature modules** split across the canonical `dom/ infra/ app/` layers, one subfolder per entity. This keeps the persistence boundary visible: domain abstractions in `dom/`, IRestOrm-backed implementations in `infra/`, application/use-case code in `app/`.

The layout and the `§…` / `A.x` / `B.x` citations in this document follow the mORMot2 SAD [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md) guide.

## Directory Structure

<pre>
/
├── <a href="../src/">src/</a>
│   ├── <a href="../src/task_manager.pas">task_manager.pas</a>                  # Thin server entry point (cthreads + RunTaskManagerDaemon)
│   ├── <a href="../src/cli_client.pas">cli_client.pas</a>                    # Thin CLI consumer (arg parsing only; links one backend)
│   ├── <a href="../src/AppTaskManagerClient.pas">AppTaskManagerClient.pas</a>          # Consumer port: ITaskManagerClient + RegisterTaskManagerInterfaces (<a href="https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure">B.6.1</a>)
│   ├── <a href="../src/AppTaskManagerClientLocal.pas">AppTaskManagerClientLocal.pas</a>     # In-process backend: embedded SQLite + dispatcher (<a href="https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a61-trusted--local-deployment">A.6.1</a>)
│   ├── <a href="../src/AppTaskManagerClientRemote.pas">AppTaskManagerClientRemote.pas</a>    # HTTP backend: TRestHttpClient to the daemon (<a href="https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a62-internet-facing-or-distributed-deployment">A.6.2</a>)
│   ├── <a href="../src/shared_types.pas">shared_types.pas</a>                  # TCommandResult and helpers
│   ├── <a href="../src/app_config.pas">app_config.pas</a>                    # Logging configuration
│   ├── <a href="../src/app_settings.pas">app_settings.pas</a>                  # TTaskManagerSettings (TSynDaemonSettings)
│   ├── <a href="../src/dom/">dom/</a>                              # Domain: TOrm aggregates + interfaces (no infrastructure leaks)
│   │   ├── <a href="../src/dom/tasks/">tasks/</a>
│   │   │   ├── <a href="../src/dom/tasks/task.pas">task.pas</a>                  # TTask aggregate (TOrm), TTaskComment
│   │   │   ├── <a href="../src/dom/tasks/task_repository.pas">task_repository.pas</a>       # ITaskRepository — persistence port
│   │   │   ├── <a href="../src/dom/tasks/task_query.pas">task_query.pas</a>            # ITaskQuery (CQRS read)
│   │   │   └── <a href="../src/dom/tasks/task_command.pas">task_command.pas</a>          # ITaskCommand (CQRS write)
│   │   └── <a href="../src/dom/tags/">tags/</a>
│   │       ├── <a href="../src/dom/tags/tag.pas">tag.pas</a>                   # TTag aggregate (TOrm)
│   │       ├── <a href="../src/dom/tags/tag_repository.pas">tag_repository.pas</a>        # ITagRepository — persistence port
│   │       ├── <a href="../src/dom/tags/tag_query.pas">tag_query.pas</a>             # ITagQuery
│   │       └── <a href="../src/dom/tags/tag_command.pas">tag_command.pas</a>           # ITagCommand
│   ├── <a href="../src/infra/">infra/</a>                            # Infrastructure: IRestOrm / SQL / FTS5
│   │   ├── <a href="../src/infra/tasks/">tasks/</a>
│   │   │   └── <a href="../src/infra/tasks/task_repository_orm.pas">task_repository_orm.pas</a>   # TTaskRepositoryOrm, TTaskFts5 schema, migration
│   │   └── <a href="../src/infra/tags/">tags/</a>
│   │       └── <a href="../src/infra/tags/tag_repository_orm.pas">tag_repository_orm.pas</a>    # TTagRepositoryOrm, migration
│   └── <a href="../src/app/">app/</a>                              # Application layer: DTOs, mappers, services
│       ├── <a href="../src/app/tasks/">tasks/</a>
│       │   ├── <a href="../src/app/tasks/task_dtos.pas">task_dtos.pas</a>             # Family of packed-record DTOs (view/list/create/update/…)
│       │   ├── <a href="../src/app/tasks/task_mappers.pas">task_mappers.pas</a>          # OrmTo* / *ToOrm / Apply* — pure procedures
│       │   ├── <a href="../src/app/tasks/task_query_impl.pas">task_query_impl.pas</a>       # TTaskQueryService (sicShared)
│       │   └── <a href="../src/app/tasks/task_command_impl.pas">task_command_impl.pas</a>     # TTaskCommandService (sicShared)
│       └── <a href="../src/app/tags/">tags/</a>
│           ├── <a href="../src/app/tags/tag_dtos.pas">tag_dtos.pas</a>
│           ├── <a href="../src/app/tags/tag_mappers.pas">tag_mappers.pas</a>
│           ├── <a href="../src/app/tags/tag_query_impl.pas">tag_query_impl.pas</a>
│           └── <a href="../src/app/tags/tag_command_impl.pas">tag_command_impl.pas</a>
│   └── <a href="../src/serv/">serv/</a>                             # Daemon / composition root (the only place infra classes are named)
│       └── <a href="../src/serv/app/">app/</a>
│           └── <a href="../src/serv/app/ServAppTaskManager.pas">ServAppTaskManager.pas</a>    # Builds the two-server topology, wires repos, registers services
├── <a href="../tests/">tests/</a>                                # Test units, one folder per entity
│   ├── <a href="../tests/tasks/">tasks/</a>
│   │   └── <a href="../tests/tasks/task_tests.pas">task_tests.pas</a>                # TTestTask (TSynTestCase) — 10 test methods
│   └── <a href="../tests/tags/">tags/</a>
│       └── <a href="../tests/tags/tag_tests.pas">tag_tests.pas</a>                 # TTestTag (TSynTestCase) — 8 test methods
├── <a href="../static/">static/</a>                               # Web client files
│   ├── <a href="../static/index.html">index.html</a>                        # Streamlined client (task CRUD + stats)
│   └── <a href="../static/app.html">app.html</a>                          # Full-featured client (tags + comments)
├── <a href="../data/">data/</a>                                 # SQLite database (runtime)
├── <a href="../logs/">logs/</a>                                 # Log files (runtime)
├── <a href="../bin/">bin/</a>                                  # Compiled binaries
├── <a href="../compile.sh">compile.sh</a>                            # Server compilation
├── <a href="../compile_cli.sh">compile_cli.sh</a>                        # CLI compilation
└── *.md                                  # Documentation
</pre>

## Layers

### `dom/` — Domain (pure interfaces)

The domain layer. No `IRestOrm`, no `mormot.rest.*`, no SQL — it depends only on the ORM *base type* (`mormot.orm.core`). Per entity:

- **`<entity>.pas`** — the `TOrm` aggregate. Doubles as the domain object (no separate `TSynPersistent` shadow class), owning its own invariants (comment/tag mutators, completion state). Lives in `dom/` because it *is* the domain object (Recommended Patterns [B.3](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b3-two-types-per-entity-not-three)/[B.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure)).
- **`I<Entity>Repository`** — persistence port. Hides the storage choice from the application.
- **`I<Entity>Query`** — CQRS read interface (IInvokable, GUID, registered on Server.Services).
- **`I<Entity>Command`** — CQRS write interface (IInvokable, GUID).

This is what the rest of the code depends on. Swapping SQLite for SQL Server / Mongo / a stub for tests is a `dom/` → new `infra/` impl change; nothing in `app/` moves.

### `infra/` — Infrastructure (IRestOrm-backed implementations)

- **`<entity>_repository_orm.pas`** — `T<Entity>RepositoryOrm` implements `I<Entity>Repository` using `IRestOrm` (Add/Update/Delete/RetrieveList, FTS5 sync, transactions). Also home to the persistence-only `TTaskFts5` schema class and the `IRestOrm`-based `Migrate<Entity>IfNeeded` lazy upgrade — both write through the ORM, so they belong here rather than with the domain aggregate.

All SQL/FTS5 knowledge lives here. If the application layer ever calls `IRestOrm` directly, that's a leak to fix.

### `app/` — Application (DTOs, mappers, services)

- **`<entity>_dtos.pas`** — family of **operation-specific** `packed record` DTOs (`T<Entity>ViewDTO`, `T<Entity>ListItemDTO`, `T<Entity>CreateDTO`, `T<Entity>UpdateDTO`, …) registered via `Rtti.RegisterFromText` in the unit's `initialization` section.
- **`<entity>_mappers.pas`** — **pure** procedures:
  - `OrmTo<Dto>` for reads (mechanical field copy).
  - `<Dto>ToOrm` for create (fills a fresh `TOrm`, sets `CreatedAt`, defaults, validations).
  - `Apply<Dto>` for update (mutates an already-loaded `TOrm`, preserves `CreatedAt`, refreshes `UpdatedAt`).
  - No DB lookups, no `IRestOrm` calls — pure transforms. The cheapest unit-test target in the codebase.
- **`<entity>_query_impl.pas`** / **`<entity>_command_impl.pas`** — `T<Entity>QueryService` / `T<Entity>CommandService` descend from `TInjectableObjectRest` and implement the CQRS interfaces. They hold a single collaborator (`I<Entity>Repository`) as a **published interface property** filled by `AutoResolve` against the repo seeded into the dispatcher's resolver (`InjectInstance`). Stateless (sicShared); per-call thread-safety comes from `TSynMonitor`'s internal locker and from never sharing `TOrm` instances across threads.

### `tests/`

One subfolder per entity. Tests use `TSynTestCase` and resolve services via `Dispatcher.Services.Resolve` (passed in via `SetTestServers(persistence, dispatcher)`), seeding data through `persistence.Orm`. Mapper round-trip tests live closest to the mappers they exercise.

### `serv/` — Daemon / composition root

The one place that names concrete `infra/` classes and wires them to the services. Keeping it in its own folder (per the Recommended Patterns [suggested layout](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure)) makes the persistence boundary visible: every other layer depends on `dom/` interfaces, and swapping a backend touches only `infra/` + this unit.

## Composition roots

### `src/serv/app/ServAppTaskManager.pas`

`src/task_manager.pas` is a **thin entry point**: it brings in `cthreads` (must be the first unit on Unix) and calls `RunTaskManagerDaemon`. All composition-root logic lives in `ServAppTaskManager`.

`ServAppTaskManager.RunTaskManagerDaemon` registers the SOA interfaces, then builds two servers (§15.9): an internal `TRestServerDB` for persistence + a public `TRestServerFullMemory` dispatcher that hosts the services. Repositories are seeded into the dispatcher's own DI resolver (`InjectInstance`, Recommended Patterns [B.5](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b5-soa-composition-root-and-dependency-injection)) so `TInjectableObjectRest.AutoResolve` injects them at service-construction time — a per-composition dependency list, not global mutable state:

```pascal
Persistence := TRestServerDB.Create(PersistenceModel, DBFileName);
Persistence.CreateMissingTables;

TaskRepoImpl := TTaskRepositoryOrm.Create(Persistence.Orm);
TagRepoImpl  := TTagRepositoryOrm.Create(Persistence.Orm);

Dispatcher := TRestServerFullMemory.Create(DispatcherModel);
// Seed the dispatcher's resolver BEFORE ServiceDefine (which eagerly builds
// each sicShared instance and runs AutoResolve). ServiceContainer materializes
// the resolver on first access.
Dispatcher.ServiceContainer.InjectInstance([TaskRepoImpl, TagRepoImpl]);
Dispatcher.ServiceDefine(TTaskQueryService,   [ITaskQuery],   sicShared);
Dispatcher.ServiceDefine(TTaskCommandService, [ITaskCommand], sicShared);
Dispatcher.ServiceDefine(TTagQueryService,    [ITagQuery],    sicShared);
Dispatcher.ServiceDefine(TTagCommandService,  [ITagCommand],  sicShared);

HttpServer := TRestHttpServer.Create(Settings.HttpPort, [Dispatcher], '+', useBidirSocket);
```

Class-form `ServiceDefine` → the framework constructs each service via `CreateWithResolverAndRest`, which runs `AutoResolve` and fills the published `Repo` property. Only `Dispatcher` is exposed via HTTP — `Persistence` is in-process state behind the repositories.

### `src/cli_client.pas` + the `AppTaskManagerClient*` units

Following the Recommended-Patterns [B.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#b61-suggested-folder-structure) consumer layout, the CLI is split into a transport-agnostic port plus two interchangeable backends, so the command code is identical whether the services run in-process or over HTTP (location transparency, [A.6](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a6-minimum-recommended-server-configuration)):

- **`AppTaskManagerClient.pas`** — the consumer dependency unit. Declares `ITaskManagerClient` (exposes the four CQRS ports) and `RegisterTaskManagerInterfaces`. Knows nothing about transport.
- **`AppTaskManagerClientLocal.pas`** — in-process backend ([A.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a61-trusted--local-deployment)). Its `CreateClient` builds the server's two-server layout (persistence + dispatcher), seeds the repos into the dispatcher's resolver (`InjectInstance`), and resolves services via `Dispatcher.Services.Resolve`. The returned `ITaskManagerClient` owns the topology and tears it down on release.
- **`AppTaskManagerClientRemote.pas`** — HTTP backend ([A.6.2](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a62-internet-facing-or-distributed-deployment)). Its `CreateClient` connects via `TRestHttpClient`, calls `ServiceDefine([…], sicShared)`, and resolves through the client's stub container. No local service composition.
- **`cli_client.pas`** — pure presentation: parses arguments, calls `CreateClient` once, and drives the ports. `uses` exactly one backend, selected by `{$define LOCAL_MODE}`.

## Module Dependencies

<pre>
<a href="../src/task_manager.pas">task_manager.pas</a>  → <a href="../src/serv/app/ServAppTaskManager.pas">serv/app/ServAppTaskManager.pas</a>
    ├── <a href="../src/shared_types.pas">shared_types.pas</a>, <a href="../src/app_config.pas">app_config.pas</a>
    ├── tasks:
    │   ├── <a href="../src/dom/tasks/task.pas">dom/tasks/task.pas</a>              # TOrm aggregate (depends only on mormot.orm.core)
    │   ├── <a href="../src/app/tasks/task_dtos.pas">app/tasks/task_dtos.pas</a>         → task
    │   ├── <a href="../src/app/tasks/task_mappers.pas">app/tasks/task_mappers.pas</a>      → task, task_dtos
    │   ├── <a href="../src/dom/tasks/task_repository.pas">dom/tasks/task_repository.pas</a>   → task
    │   ├── <a href="../src/infra/tasks/task_repository_orm.pas">infra/tasks/task_repository_orm.pas</a>
    │   │       → task, task_repository
    │   ├── <a href="../src/dom/tasks/task_query.pas">dom/tasks/task_query.pas</a>        → task_dtos
    │   ├── <a href="../src/dom/tasks/task_command.pas">dom/tasks/task_command.pas</a>      → task_dtos, shared_types
    │   ├── <a href="../src/app/tasks/task_query_impl.pas">app/tasks/task_query_impl.pas</a>
    │   │       → task_query, task, task_dtos, task_mappers, task_repository
    │   ├── <a href="../src/app/tasks/task_command_impl.pas">app/tasks/task_command_impl.pas</a>
    │   │       → task_command, task, task_dtos, task_mappers, task_repository
    │   └── <a href="../tests/tasks/task_tests.pas">tests/tasks/task_tests.pas</a>      → task_query, task_command, task_dtos
    └── tags: (symmetric)
</pre>

The key rule: `app/*_impl.pas` depends on `dom/*_repository.pas` (interface), **never** on `infra/*_repository_orm.pas` (implementation). The composition root is the only file that knows the concrete `T*RepositoryOrm` class exists.

## File Naming Conventions

| Pattern | Layer | Role |
|---|---|---|
| `<entity>.pas`          | dom   | `TOrm` aggregate (doubles as domain object) |
| `*_repository.pas`      | dom   | `I*Repository` persistence port |
| `*_repository_orm.pas`  | infra | IRestOrm-backed impl + FTS5 schema + migration |
| `*_dtos.pas`            | app   | `packed record` DTO family |
| `*_mappers.pas`         | app   | Pure `OrmTo*` / `*ToOrm` / `Apply*` |
| `*_query.pas`           | dom   | `I*Query` CQRS read interface |
| `*_command.pas`         | dom   | `I*Command` CQRS write interface |
| `*_query_impl.pas`      | app   | `T*QueryService` |
| `*_command_impl.pas`    | app   | `T*CommandService` |
| `*_tests.pas`           | tests | `TSynTestCase` per feature |
| `ServApp*.pas`          | serv  | Daemon / composition root |
| `App*Client.pas`        | src   | Transport-agnostic consumer port (`ITaskManagerClient`) |
| `App*ClientLocal.pas`   | src   | In-process backend ([A.6.1](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a61-trusted--local-deployment)) |
| `App*ClientRemote.pas`  | src   | HTTP backend ([A.6.2](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md#a62-internet-facing-or-distributed-deployment)) |

## Adding a New Feature

1. Create `src/dom/<entity>/`, `src/infra/<entity>/`, `src/app/<entity>/`, `tests/<entity>/`.
2. **Dom**: define the `TOrm` aggregate (and any embedded records) in `<entity>.pas` — depends only on `mormot.orm.core`, no `IRestOrm`.
3. **Dom**: declare `I<Entity>Repository` (with a fresh GUID) in `<entity>_repository.pas`.
4. **Infra**: implement `T<Entity>RepositoryOrm` over `IRestOrm` in `<entity>_repository_orm.pas`, alongside any FTS5 schema class and the `Migrate<Entity>IfNeeded` helper.
5. **App**: define the DTO family in `<entity>_dtos.pas` and register each shape with `Rtti.RegisterFromText` in `initialization`.
6. **App**: write the mappers in `<entity>_mappers.pas` (pure — no `IRestOrm`).
7. **Dom**: declare `I<Entity>Query` and `I<Entity>Command` (with fresh GUIDs).
8. **App**: implement `T<Entity>QueryService` and `T<Entity>CommandService` (descend from `TInjectableObjectRest`, publish `Repo: I<Entity>Repository`; override `constructor Create` to allocate the `TSynMonitor`).
9. **Tests**: write `T<Entity>Tests` in `tests/<entity>/<entity>_tests.pas`.
10. **Composition root** (`serv/app/ServAppTaskManager.pas` and `AppTaskManagerClientLocal.pas`; the remote backend needs only the new Query/Command `TypeInfo()`s in `RegisterTaskManagerInterfaces`):
    - Add the `TOrm` class to the persistence `TOrmModel.Create([...])`.
    - Add the **Query and Command** interface `TypeInfo()`s to `TInterfaceFactory.RegisterInterfaces` — **not** `I<Entity>Repository`, which is a local DI port and not an RPC contract.
    - Construct the repository: `RepoImpl := T<Entity>RepositoryOrm.Create(Persistence.Orm)`.
    - Seed it into the dispatcher's resolver before `ServiceDefine`: `Dispatcher.ServiceContainer.InjectInstance([RepoImpl])` (add to the existing array).
    - Register services on the dispatcher (class form): `Dispatcher.ServiceDefine(T<Entity>QueryService, [I<Entity>Query], sicShared)`; same for Command.
    - Add the test case to the `TSynTests` suite.
11. **compile.sh / compile_cli.sh**: nothing to add — the existing `-Fusrc/dom/<entity> -Fusrc/infra/<entity> -Fusrc/app/<entity>` patterns already cover any new entity once the folder exists; just add new `-Fu` lines per entity folder.
