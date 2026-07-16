# Simple FPC mORMot2 Task Manager

A source code example of the mORMot2 [*Recommended Patterns*](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md) guide: a **task manager** built with CQRS and DDD patterns, made of an HTTP server, a web client, a CLI client and an automated test suite. It compiles with FPC, Lazarus or Delphi.

## Quick Start

This sample ships inside the [mORMot2](https://github.com/synopse/mORMot2) tree, so the framework sources are already at hand: the enclosing repository at `../../` (relative to this project). No installation is needed beyond your usual toolchain setup:

- A Pascal toolchain: Free Pascal (FPC) 3.2.2 or later, [Lazarus](https://www.lazarus-ide.org/), or Delphi
- The pre-compiled mORMot2 static libraries, extracted from [mormot2static.tgz](https://synopse.info/files/mormot2static.tgz) / [mormot2static.7z](https://synopse.info/files/mormot2static.7z) into `../../static/` — see the [main repository README](../../README.md#quick-start) if you have not done this yet

### Compile the server — pick your toolchain

Each subfolder of [`prj/`](prj/) targets one toolchain; all three build the same sources from [`src/`](src/) into [`bin/`](bin/):

- **FPC** (command line):
  ```bash
  ./prj/fpc/compile.sh        # Windows: prj\fpc\compile.bat
  ```
- **Lazarus**: open and build `prj/lazarus/task_manager.lpi` and
  `prj/lazarus/cli_client.lpi`. They require the `mormot2` package
  (`packages/lazarus/mormot2.lpk` in the enclosing repository). The
  `cli_client.lpi` "local" build mode compiles the embedded-SQLite backend.
  Headless: `lazbuild prj/lazarus/task_manager.lpi`.
- **Delphi**: open and build `prj/delphi/task_manager.dpr` and
  `prj/delphi/cli_client.dpr`, with the mORMot2 `src` folders on the library
  path. The same `.dpr` files are the main sources of the Lazarus projects.

### Run the server

```bash
./prj/fpc/run.sh              # Windows: prj\fpc\run.bat
```

or launch `bin/task_manager` directly (`--test` runs the test suite headlessly and exits).

### Open the web interface

- Streamlined client: `http://localhost:8080/static/index.html`
- Full-featured client (tags + comments): `http://localhost:8080/static/app.html`

## Usage

### Web Interface

The web interface provides an intuitive way to manage tasks:

1. **Create Task**: Fill in the form with task details and click "Create Task"
2. **View Tasks**: All tasks are displayed in card format
3. **Filter Tasks**: Use filter buttons to show All, Pending, In Progress, or Completed tasks
4. **Complete Task**: Click the "Complete" button to mark as done
5. **Delete Task**: Click the "Delete" button to remove a task

### CQRS API Access

All operations use POST requests to SOA endpoints with JSON array parameters:

```bash
# List all tasks
curl -X POST http://localhost:8080/taskmanager/TaskQuery.ListTasks \
  -H "Content-Type: application/json" \
  -d '[""]'

# Get task details
curl -X POST http://localhost:8080/taskmanager/TaskQuery.GetTaskView \
  -H "Content-Type: application/json" \
  -d '[1]'

# Create task
curl -X POST http://localhost:8080/taskmanager/TaskCommand.CreateTask \
  -H "Content-Type: application/json" \
  -d '[{"title":"My Task","description":"Task description","priority":2,"dueDate":"2026-04-01"}]'

# Mark task complete
curl -X POST http://localhost:8080/taskmanager/TaskCommand.MarkComplete \
  -H "Content-Type: application/json" \
  -d '[1, true]'

# Delete task
curl -X POST http://localhost:8080/taskmanager/TaskCommand.DeleteTask \
  -H "Content-Type: application/json" \
  -d '[1]'

# Search tasks
curl -X POST http://localhost:8080/taskmanager/TaskQuery.SearchTasks \
  -H "Content-Type: application/json" \
  -d '[{"searchTerm":"keyword","status":""}]'
```

See [docs/API.md](docs/API.md) for complete API documentation.

### CLI Client

The CLI client uses mORMot2's typed SOA interfaces to call the CQRS services:

```bash
# Build remote mode (talks to server on localhost:8080)
./prj/fpc/compile_cli.sh

# Build local mode (embedded SQLite, no server needed)
./prj/fpc/compile_cli.sh local

# Usage
./bin/cli_client list
./bin/cli_client list pending
./bin/cli_client add "My Task" "Description" 3
./bin/cli_client get 1
./bin/cli_client complete 1
./bin/cli_client delete 1
./bin/cli_client search "keyword"
./bin/cli_client comment 1 "Great progress!" "Author"
```

On Windows the equivalents are `prj\fpc\compile_cli.bat` / `prj\fpc\compile_cli.bat local` and
`bin\cli_client.exe list`, `bin\cli_client.exe add "My Task" "Description" 3`, etc.

## Project Structure
<pre>
/
├── <a href="src/">src/</a>
│   ├── <a href="src/task_manager.pas">task_manager.pas</a>             # Thin server entry point (calls RunTaskManagerDaemon)
│   ├── <a href="src/cli_client.pas">cli_client.pas</a>               # Thin CLI entry point (selects backend via -dLOCAL_MODE)
│   ├── <a href="src/cli_client_core.pas">cli_client_core.pas</a>          # CLI presentation logic (backend-agnostic)
│   ├── <a href="src/AppTaskManagerClient.pas">AppTaskManagerClient.pas</a>     # ITaskManagerClient handle + interface registration
│   ├── <a href="src/AppTaskManagerClientLocal.pas">AppTaskManagerClientLocal.pas</a> # LOCAL backend (embedded SQLite, in-process dispatcher)
│   ├── <a href="src/AppTaskManagerClientRemote.pas">AppTaskManagerClientRemote.pas</a> # REMOTE backend (HTTP client)
│   ├── <a href="src/shared_types.pas">shared_types.pas</a>             # TCommandResult and helpers
│   ├── <a href="src/app_config.pas">app_config.pas</a>               # Logging configuration
│   ├── <a href="src/app_settings.pas">app_settings.pas</a>             # TTaskManagerSettings (HTTP port, root URI, DB name)
│   ├── <a href="src/dom/">dom/</a>                         # Domain: TOrm aggregates + interfaces (pure, no infrastructure)
│   │   ├── <a href="src/dom/tasks/">tasks/</a>
│   │   │   ├── <a href="src/dom/tasks/task.pas">task.pas</a>              # TTask aggregate (TOrm), TTaskComment
│   │   │   ├── <a href="src/dom/tasks/task_repository.pas">task_repository.pas</a>   # ITaskRepository — persistence port
│   │   │   ├── <a href="src/dom/tasks/task_query.pas">task_query.pas</a>        # ITaskQuery (CQRS read)
│   │   │   └── <a href="src/dom/tasks/task_command.pas">task_command.pas</a>      # ITaskCommand (CQRS write)
│   │   └── <a href="src/dom/tags/">tags/</a>
│   │       ├── <a href="src/dom/tags/tag.pas">tag.pas</a>               # TTag aggregate (TOrm)
│   │       ├── <a href="src/dom/tags/tag_repository.pas">tag_repository.pas</a>    # ITagRepository
│   │       ├── <a href="src/dom/tags/tag_query.pas">tag_query.pas</a>         # ITagQuery
│   │       └── <a href="src/dom/tags/tag_command.pas">tag_command.pas</a>       # ITagCommand
│   ├── <a href="src/infra/">infra/</a>                       # Infrastructure (IRestOrm / SQL / FTS5)
│   │   ├── <a href="src/infra/tasks/">tasks/</a>
│   │   │   └── <a href="src/infra/tasks/task_repository_orm.pas">task_repository_orm.pas</a>  # TTaskRepositoryOrm, TTaskFts5 schema, migration
│   │   └── <a href="src/infra/tags/">tags/</a>
│   │       └── <a href="src/infra/tags/tag_repository_orm.pas">tag_repository_orm.pas</a>   # TTagRepositoryOrm, migration
│   ├── <a href="src/app/">app/</a>                         # Application: DTOs, mappers, services
│   │   ├── <a href="src/app/tasks/">tasks/</a>
│   │   │   ├── <a href="src/app/tasks/task_dtos.pas">task_dtos.pas</a>          # packed-record DTO family
│   │   │   ├── <a href="src/app/tasks/task_mappers.pas">task_mappers.pas</a>       # OrmTo* / *ToOrm / Apply* (pure)
│   │   │   ├── <a href="src/app/tasks/task_query_impl.pas">task_query_impl.pas</a>    # TTaskQueryService (sicShared)
│   │   │   └── <a href="src/app/tasks/task_command_impl.pas">task_command_impl.pas</a>  # TTaskCommandService (sicShared)
│   │   └── <a href="src/app/tags/">tags/</a>
│   │       ├── <a href="src/app/tags/tag_dtos.pas">tag_dtos.pas</a>
│   │       ├── <a href="src/app/tags/tag_mappers.pas">tag_mappers.pas</a>
│   │       ├── <a href="src/app/tags/tag_query_impl.pas">tag_query_impl.pas</a>
│   │       └── <a href="src/app/tags/tag_command_impl.pas">tag_command_impl.pas</a>
│   └── <a href="src/serv/">serv/</a>                        # Composition root: server wiring + daemon host
│       └── <a href="src/serv/app/">app/</a>
│           └── <a href="src/serv/app/ServAppTaskManager.pas">ServAppTaskManager.pas</a>   # RunTaskManagerDaemon: two-server topology, HTTP host
├── <a href="prj/">prj/</a>                             # Per-toolchain project files and build scripts
│   ├── <a href="prj/fpc/">fpc/</a>                         # Command-line build/run scripts (FPC)
│   │   ├── <a href="prj/fpc/compile.sh">compile.sh</a> / <a href="prj/fpc/compile.bat">compile.bat</a>  # Server build (Linux / Windows)
│   │   ├── <a href="prj/fpc/compile_cli.sh">compile_cli.sh</a> / <a href="prj/fpc/compile_cli.bat">compile_cli.bat</a>  # CLI build
│   │   └── <a href="prj/fpc/run.sh">run.sh</a> / <a href="prj/fpc/run.bat">run.bat</a>          # Run the server
│   ├── <a href="prj/delphi/">delphi/</a>                      # Delphi project files
│   │   ├── <a href="prj/delphi/task_manager.dpr">task_manager.dpr</a>         # Server project (also main source of the .lpi)
│   │   └── <a href="prj/delphi/cli_client.dpr">cli_client.dpr</a>           # CLI project (also main source of the .lpi)
│   └── <a href="prj/lazarus/">lazarus/</a>                     # Lazarus project files (need the mormot2 package)
│       ├── <a href="prj/lazarus/task_manager.lpi">task_manager.lpi</a>         # Server project
│       └── <a href="prj/lazarus/cli_client.lpi">cli_client.lpi</a>           # CLI project ("local" build mode = -dLOCAL_MODE)
├── <a href="tests/">tests/</a>                           # Test units, one folder per entity
│   ├── <a href="tests/tasks/task_tests.pas">tasks/task_tests.pas</a>         # TTestTask (TSynTestCase) — 10 test methods
│   └── <a href="tests/tags/tag_tests.pas">tags/tag_tests.pas</a>           # TTestTag (TSynTestCase) — 8 test methods
├── <a href="static/">static/</a>
│   ├── <a href="static/index.html">index.html</a>                   # Streamlined web client (SOA)
│   └── <a href="static/app.html">app.html</a>                     # Full-featured client (tags + comments)
├── <a href="data/">data/</a>
│   └── tasks.db3                    # SQLite database (auto-created)
├── <a href="logs/">logs/</a>                            # Log files (auto-created)
└── <a href="bin/">bin/</a>
    ├── task_manager                 # Server executable
    ├── cli_client                   # CLI client executable
    └── units/                       # Compiled units
</pre>

## CQRS Service Endpoints

### TaskQuery (Read Operations)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/taskmanager/TaskQuery.GetTaskView` | Get full task details with comments and tags |
| POST | `/taskmanager/TaskQuery.ListTasks` | List tasks, optionally filtered by status |
| POST | `/taskmanager/TaskQuery.SearchTasks` | Search tasks by title/description (FTS5) |

### TaskCommand (Write Operations)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/taskmanager/TaskCommand.CreateTask` | Create a new task |
| POST | `/taskmanager/TaskCommand.UpdateTask` | Update task properties |
| POST | `/taskmanager/TaskCommand.DeleteTask` | Delete a task |
| POST | `/taskmanager/TaskCommand.MarkComplete` | Mark task complete/incomplete |
| POST | `/taskmanager/TaskCommand.AddComment` | Add a comment to a task |
| POST | `/taskmanager/TaskCommand.UpdateComment` | Update comment content |
| POST | `/taskmanager/TaskCommand.DeleteComment` | Delete a comment by index |
| POST | `/taskmanager/TaskCommand.AddTag` | Associate a tag with a task |
| POST | `/taskmanager/TaskCommand.RemoveTag` | Remove a tag from a task |

### TagQuery (Read Operations)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/taskmanager/TagQuery.GetTagView` | Get tag details |
| POST | `/taskmanager/TagQuery.ListTags` | List all tags |
| POST | `/taskmanager/TagQuery.SearchTags` | Search tags by name |

### TagCommand (Write Operations)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/taskmanager/TagCommand.CreateTag` | Create a new tag |
| POST | `/taskmanager/TagCommand.UpdateTag` | Update tag properties |
| POST | `/taskmanager/TagCommand.DeleteTag` | Delete a tag |

## Database Schema

### Task Table (DDD Aggregate Root)

| Column | Type | Description |
|--------|------|-------------|
| ID | INTEGER | Primary key (auto-increment) |
| Title | TEXT | Task title |
| Description | TEXT | Task description |
| Priority | INTEGER | 1=Low, 2=Medium, 3=High, 4=Urgent, 5=Critical |
| DueDate | REAL | Due date (TDateTime) |
| Status | TEXT | pending/in_progress/completed |
| IsCompleted | BOOLEAN | Completion flag |
| CreatedAt | REAL | Creation timestamp |
| UpdatedAt | REAL | Last update timestamp |
| SchemaVersion | INTEGER | Record version for lazy migration |
| Comments | TEXT | JSON array of embedded TTaskComment records |
| TagIDs | TEXT | JSON array of Tag ID references |

### Tag Table (DDD Aggregate)

| Column | Type | Description |
|--------|------|-------------|
| ID | INTEGER | Primary key (auto-increment) |
| Name | TEXT | Unique tag name (lowercase) |
| Color | TEXT | Color code (#RRGGBB) |
| CreatedAt | REAL | Creation timestamp |
| SchemaVersion | INTEGER | Record version for lazy migration |

### TaskFts5 Table (FTS5 Index)

| Column | Type | Description |
|--------|------|-------------|
| Title | TEXT | Indexed task title |
| Description | TEXT | Indexed task description |

## Configuration

### Changing the Port

The HTTP port default lives in `src/app_settings.pas` (`TTaskManagerSettings.Create`):

```pascal
fHttpPort := '8080';  // Change this port number
```

The composition root in `src/serv/app/ServAppTaskManager.pas` reads it back when
constructing the HTTP host (`ListenURI := Settings.HttpPort`).

### Database Location

The database is stored at `data/tasks.db3` by default, resolved relative to the
binary's parent directory (`bin/../data`), i.e. `data/` at the project root.

## Troubleshooting

**Server won't start**:
- Check if port 8080 is already in use
- Ensure you have write permissions to the `data/` directory

**Compilation fails**:
- Ensure static libraries are present: `../../static/x86_64-linux/` (Linux) or `../../static/x86_64-win64/` (Windows)

**Web interface doesn't load tasks**:
- Ensure the server is running
- Check browser console for errors

## Documentation

- [README.md](README.md) - This file (overview and quick start)
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) - System architecture and design
- [docs/API.md](docs/API.md) - Complete API reference
- [docs/WEB_CLIENTS.md](docs/WEB_CLIENTS.md) - Web client guide (basic and full-featured)
- [docs/DEVELOPMENT.md](docs/DEVELOPMENT.md) - Development guide
- [docs/SOA_SERVICES.md](docs/SOA_SERVICES.md) - CQRS services documentation
- [docs/COMMENTS_FEATURE.md](docs/COMMENTS_FEATURE.md) - Embedded comments feature
- [docs/PROJECT_STRUCTURE.md](docs/PROJECT_STRUCTURE.md) - Code organization

## Technical Stack

**Backend**:
- Language: Object Pascal
- Framework: mORMot2
- Database: SQLite3
- ORM: mORMot2 ORM with DDD aggregates
- HTTP Server: mORMot2 HTTP Server (async)
- Architecture: CQRS + DDD

**Frontend**:
- HTML5, CSS3, Vanilla JavaScript (ES6+)
- Fetch API for SOA calls

## Learning Resources

- [mORMot2 SAD — Recommended Patterns](https://github.com/synopse/mORMot2/blob/master/docs/mORMot2-SAD-Recommended-Patterns.md) — the design guide this project follows; the `§`, `A.x`, and `B.x` references throughout the docs point here
- [mORMot2 Documentation](https://synopse.info)
- [mORMot2 GitHub Repository](https://github.com/synopse/mORMot2)
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)

