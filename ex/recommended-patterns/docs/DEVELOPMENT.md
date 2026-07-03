
# Development Guide

## Project Structure

See [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md) for the full layout. The `§…` / `B.x` references in this guide follow the mORMot2 SAD [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md). In short:

```
src/
├── dom/<entity>/     # TOrm aggregate (<entity>.pas) + I<Entity>Repository, I<Entity>Query, I<Entity>Command
├── infra/<entity>/   # T<Entity>RepositoryOrm (+ FTS5 schema, migration)
├── app/<entity>/     # DTOs, mappers, T<Entity>QueryService, T<Entity>CommandService
├── serv/app/ServAppTaskManager.pas     # Server composition root (RunTaskManagerDaemon)
├── task_manager.pas                    # Thin server entry point (calls RunTaskManagerDaemon)
├── cli_client.pas                      # Thin CLI consumer (selects backend via -dLOCAL_MODE)
├── AppTaskManagerClient{,Local,Remote}.pas  # CLI backends: LOCAL (embedded) / REMOTE (HTTP)
├── shared_types.pas, app_config.pas, app_settings.pas
tests/<entity>/       # TSynTestCase per entity
```

The hard rule: `app/*` depends on `dom/*_repository.pas` (interface), **never** on `infra/*_repository_orm.pas` (implementation). The composition root is the only place where the concrete `T<Entity>RepositoryOrm` class is named — it constructs it and seeds it into the dispatcher's resolver via `Dispatcher.ServiceContainer.InjectInstance([repoImpl])` so service `Repo` properties resolve through `AutoResolve`.

## Prerequisites

### Required Software

1. **Free Pascal Compiler (FPC)** version 3.2.2 or later
   ```bash
   fpc -version
   ```

2. **mORMot2 Framework** — this sample ships inside the mORMot2 tree, so the
   framework sources are the enclosing repository at `../../` (relative to this
   project), with static libraries under `../../static/x86_64-linux/`

## Building

### Compile Server

```bash
./compile.sh
```

### Compile CLI Client

```bash
# Remote mode (connects to running server)
./compile_cli.sh

# Local mode (embedded SQLite, no server needed)
./compile_cli.sh local
```

### Key Compiler Flags (in compile.sh)

- `-Fl../../static/x86_64-linux`: Link static SQLite3 library
- `-Fusrc -Fusrc/dom/{tasks,tags} -Fusrc/infra/{tasks,tags} -Fusrc/app/{tasks,tags} -Fusrc/serv/app -Futests/{tasks,tags}`: Unit search paths
- `-Fu../../src/{core,orm,rest,soa,...}`: mORMot2 unit paths
- `-FUbin/units`: Output compiled units
- `-Mobjfpc`: Object Pascal mode

## Running

```bash
./run.sh
# or directly:
./bin/task_manager
```

The server will:
1. Create the database if it doesn't exist
2. Create tables if needed
3. Register CQRS services (ITaskQuery, ITaskCommand, ITagQuery, ITagCommand)
4. Insert sample data if the database is empty
5. Run the TSynTestCase suites (Task: 10 methods, Tag: 8 methods)
6. Start HTTP server on port 8080

### CLI Usage

```bash
./bin/cli_client list [status]
./bin/cli_client add "Title" "Description" 3
./bin/cli_client get 1
./bin/cli_client complete 1
./bin/cli_client delete 1
./bin/cli_client search "term"
./bin/cli_client comment 1 "Comment text" "Author"
./bin/cli_client help
```

## Development Workflow

### 1. Edit Source Code

Feature files are split across `src/dom/<entity>/`, `src/infra/<entity>/`, `src/app/<entity>/`, with tests in `tests/<entity>/`. See PROJECT_STRUCTURE.md for what lives where.

### 2. Compile

```bash
./compile.sh
```

### 3. Test

Tests run automatically at server startup. For a headless run — the suite then
exits without opening the HTTP listener (handy for CI or sandboxes where binding
a socket is restricted):

```bash
./bin/task_manager --test
```

To test manually against a live server:

```bash
# Start server (runs tests then serves HTTP)
./bin/task_manager

# Test CQRS endpoints
curl -s -X POST http://localhost:8080/taskmanager/TaskQuery.ListTasks \
  -H "Content-Type: application/json" -d '[""]'

curl -s -X POST http://localhost:8080/taskmanager/TaskCommand.CreateTask \
  -H "Content-Type: application/json" \
  -d '[{"title":"Test","description":"Desc","priority":2,"dueDate":""}]'
```

### 4. Web Interface

Open `http://localhost:8080/static/index.html` in your browser.

## Adding a New Feature (CQRS Pattern)

### 1. Create the four folders

```
src/dom/myfeature/
src/infra/myfeature/
src/app/myfeature/
tests/myfeature/
```

### 2. Define the ORM aggregate (dom)

The `TOrm` doubles as the domain object, so it lives in `dom/` and depends only on the ORM *base type* (`mormot.orm.core`) — no `IRestOrm`. The `IRestOrm`-based lazy migration belongs with the repository in `infra/` (step 4), not here.

```pascal
// src/dom/myfeature/my.pas
unit my;
{$mode objfpc}{$H+}
interface
uses mormot.core.base, mormot.orm.core;

const
  MY_CURRENT_VERSION = 1;

type
  TMyEntity = class(TOrm)
  private
    fName: RawUtf8;
    fSchemaVersion: integer;
  published
    property Name: RawUtf8 read fName write fName;
    property SchemaVersion: integer read fSchemaVersion write fSchemaVersion;
  end;
implementation
end.
```

### 3. Declare the persistence port (dom)

```pascal
// src/dom/myfeature/my_repository.pas
unit my_repository;
{$mode objfpc}{$H+}
interface
uses mormot.core.base, mormot.core.collections, mormot.core.interfaces, mormot.orm.core, my;

type
  IMyRepository = interface(IInvokable)
    ['{NEW-GUID-HERE}']
    function GetByID(aID: TID): TMyEntity;   // migrate internally before returning
    function Add(aEntity: TMyEntity): TID;   // make atomic (own the transaction)
    function Update(aEntity: TMyEntity): boolean;
    function Delete(aID: TID): boolean;
    function List: IList<TMyEntity>;
  end;
  // Keep the port free of transaction / FTS5 / migration mechanics: writes are
  // atomic and reads return already-migrated aggregates, so the app layer never
  // calls IRestOrm, begins a transaction, or triggers migration (§B.1/§B.6.1).
implementation
end.
```

### 4. Implement the repository over IRestOrm (infra)

```pascal
// src/infra/myfeature/my_repository_orm.pas
type
  TMyRepositoryOrm = class(TInterfacedObject, IMyRepository)
  private
    fOrm: IRestOrm;
  public
    constructor Create(const aOrm: IRestOrm);
    function GetByID(aID: TID): TMyEntity;
    function Add(aEntity: TMyEntity): TID;
    // ... etc.
  end;

// MigrateMyIfNeeded(const aOrm: IRestOrm; aEntity: TMyEntity) and any FTS5
// schema class live in this unit too — they write through IRestOrm, so they
// are persistence concerns, not part of the dom/ aggregate. The read methods
// call MigrateMyIfNeeded internally before returning.
```

All SQL / FTS5 / `IRestOrm` knowledge lives here. The app layer never sees it.

### 5. Define DTOs (app)

```pascal
// src/app/myfeature/my_dtos.pas
type
  TMyViewDTO = packed record
    ID: TID;
    Name: RawUtf8;
  end;
  TCreateMyDTO = packed record
    Name: RawUtf8;
  end;
  TUpdateMyDTO = packed record
    ID: TID;
    Name: RawUtf8;
  end;

initialization
  Rtti.RegisterFromText(TypeInfo(TMyViewDTO),   'ID Int64 Name RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TCreateMyDTO), 'Name RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TUpdateMyDTO), 'ID Int64 Name RawUtf8');
```

**Important**: `Rtti.RegisterFromText` is required on FPC 3.2.2 because the compiler does not emit field-level RTTI for records. Records must be `packed` to avoid alignment padding mismatches.

### 6. Write mappers — pure procedures (app)

```pascal
// src/app/myfeature/my_mappers.pas
function OrmToMyViewDTO(aEntity: TMyEntity): TMyViewDTO;
function CreateMyDTOToOrm(const aDto: TCreateMyDTO; aEntity: TMyEntity;
  out aError: RawUtf8): boolean;
function ApplyUpdateMyDTO(const aDto: TUpdateMyDTO; aEntity: TMyEntity;
  out aError: RawUtf8): boolean;
```

Rules: no DB lookups, no `IRestOrm` calls, no `Repo.` calls. Create and update are different functions, never one. Reads are mechanical; writes carry the real logic (defaults, normalization, validation).

### 7. Declare CQRS interfaces (dom)

```pascal
// src/dom/myfeature/my_query.pas
type
  IMyQuery = interface(IInvokable)
    ['{NEW-GUID-HERE}']
    function GetView(aID: TID): TMyViewDTO;
    function ListAll: TMyViewDTODynArray;
  end;

// src/dom/myfeature/my_command.pas
type
  IMyCommand = interface(IInvokable)
    ['{NEW-GUID-HERE}']
    function Create(const aData: TCreateMyDTO): TCommandResult;
    function Delete(aID: TID): TCommandResult;
  end;
```

### 8. Implement the services (app)

```pascal
// src/app/myfeature/my_command_impl.pas
type
  // sicShared: fRepo is filled by AutoResolve and immutable after construction;
  // fMonitor is internally locked.
  TMyCommandService = class(TInjectableObjectRest, IMyCommand)
  private
    fRepo: IMyRepository;        // filled by AutoResolve, NEVER IRestOrm
    fMonitor: TSynMonitor;
  public
    constructor Create; override;             // allocate TSynMonitor
    destructor Destroy; override;
    function Create(const aData: TCreateMyDTO): TCommandResult;
    function Delete(aID: TID): TCommandResult;
  published
    property Repo: IMyRepository
      read fRepo write fRepo;                  // resolved at construction
  end;
```

### 9. Add tests (tests)

```pascal
// tests/myfeature/my_tests.pas
type
  TTestMyFeature = class(TSynTestCase)
  protected
    fOrm: IRestOrm;
    fServer: TRestServer;     // the dispatcher — where services live
    procedure Setup; override;
  published
    procedure CreateAndRetrieve;
  end;
```

`Setup` reads `fOrm` from the persistence server (for seeding) and `fServer` from the dispatcher (for resolving services) — both injected via `SetMyTestServers(persistence, dispatcher)` called from `serv/app/ServAppTaskManager.pas` before the test suite runs. Resolve services via `fServer.Services.Resolve(IMyCommand, ...)` — same path real clients take.

### 10. Register in the composition roots

In both `serv/app/ServAppTaskManager.pas` (server) and `AppTaskManagerClientLocal.pas` (CLI LOCAL mode):

```pascal
// Add the TOrm class to the PERSISTENCE model only
PersistenceModel := TOrmModel.Create(
  [TTask, TTag, TTaskFts5, TMyEntity], Settings.Root);

// Register only the RPC interfaces (NOT IMyRepository — local DI port).
TInterfaceFactory.RegisterInterfaces([
  ..., TypeInfo(IMyQuery), TypeInfo(IMyCommand)
]);

// Wire the repository over persistence, then seed it into the dispatcher's
// resolver for AutoResolve. InjectInstance must run BEFORE ServiceDefine, which
// eagerly builds each sicShared instance and resolves its Repo property.
MyRepoImpl := TMyRepositoryOrm.Create(Persistence.Orm);
Dispatcher.ServiceContainer.InjectInstance([..., MyRepoImpl]);

// Class-form ServiceDefine on the dispatcher — framework constructs services
// via CreateWithResolverAndRest and AutoResolve fills the published Repo.
Dispatcher.ServiceDefine(TMyQueryService,   [IMyQuery],   sicShared);
Dispatcher.ServiceDefine(TMyCommandService, [IMyCommand], sicShared);
```

No manual cleanup needed: `InjectInstance` ref-counts the repos, and `Dispatcher.Free` releases them — ahead of `Persistence.Free` (whose `IRestOrm` they hold), so the references drop in the right order.

### 11. Update compile scripts

Add the new folders to `compile.sh` and `compile_cli.sh`:

```
-Fusrc/dom/myfeature \
-Fusrc/infra/myfeature \
-Fusrc/app/myfeature \
-Futests/myfeature \
```

## Debugging

### Logging

Logging is enabled by `EnableSynLogs` in `src/app_config.pas`:
- **Log files**: Full verbose detail in `logs/`
- **Console**: Warnings, errors, and exceptions echoed to stdout
- **Service logging**: Use `TSynLog.Add.Log(sllWarning, 'message')` or `sllInfo`

### Database Inspection

```bash
sqlite3 data/tasks.db3
.tables
SELECT * FROM Task;
-- Comments and TagIDs stored as JSON in TEXT columns:
SELECT ID, Title, Comments, TagIDs FROM Task;
```

## Common Issues

**Cannot find mORMot2 units**: This sample lives under `ex/` in the mORMot2 tree; ensure the framework sources at `../../src/` are present.

**Static library not found**: Check `../../static/x86_64-linux/` contains .o and .a files.

**Port 8080 already in use**: Change the `fHttpPort` default in `src/app_settings.pas` or stop the conflicting process.

**RegisterFromText size mismatch**: Ensure records are `packed record` and field types in the text definition match exactly (use `Int64` for TID, `boolean` for boolean, etc.).

**SOA returns binary instead of JSON**: Ensure the interfaces are registered via `TInterfaceFactory.RegisterInterfaces([...])` and every DTO record is registered with `Rtti.RegisterFromText`. mORMot returns the JSON `Result` wrapper by default — no extra flag is needed.

## Code Style

- Use `RawUtf8` for strings (not `string`)
- Use `NowUtc` for timestamps (not `Now`)
- Prefix fields with `f`, parameters with `a`
- Always free objects in try/finally blocks
- Use `packed record` for all DTOs
- Register packed records with `Rtti.RegisterFromText`
- Wrap aggregate mutations in transactions

## Resources

- [mORMot2 Documentation](https://synopse.info)
- [mORMot2 GitHub](https://github.com/synopse/mORMot2)
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
