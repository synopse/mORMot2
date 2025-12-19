# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `mORMot2/src/app` folder contains application framework units for building cross-platform **daemons/services** and **console applications**. These units provide the foundation for creating self-contained background executables and microservices using the mORMot framework.

**Key Concepts:**
- **Daemon** = POSIX background process / Windows Service
- **MicroService** = Self-sufficient, uncoupled SOA server (often as daemon)
- **Cross-platform** = Same code works on Windows (Service) and POSIX (daemon)

## Architecture

### Three Core Units

1. **`mormot.app.console.pas`** - Console application support
   - `ICommandLine` interface for parsing command line arguments
   - `TCommandLine` class with optional console interactivity
   - Supports GUI mode (NoConsole) and batch mode (NoPrompt)

2. **`mormot.app.daemon.pas`** - Daemon/Service framework
   - `TSynDaemon` base class for daemon implementation
   - `TSynDaemonSettings` for JSON/INI configuration persistence
   - Built-in command handling (install, start, stop, console, etc.)

3. **`mormot.app.agl.pas`** - Process manager (Angelize)
   - `TSynAngelize` main launcher and watcher service
   - `TSynAngelizeService` sub-process definition (start/stop/watch)
   - Similar to NSSM but cross-platform and JSON-configured

### Daemon Pattern

```pascal
// 1. Define settings (optional - can use TSynDaemonSettings directly)
type
  TMyDaemonSettings = class(TSynDaemonSettings)
  published
    property MyCustomSetting: string;
  end;

// 2. Implement daemon
type
  TMyDaemon = class(TSynDaemon)
  public
    procedure Start; override;  // Start your service here
    procedure Stop; override;   // Clean shutdown here
  end;

// 3. Main program
begin
  with TMyDaemon.Create(TMyDaemonSettings, '', '', '') do
  try
    CommandLine;  // Handles all switches automatically
  finally
    Free;
  end;
end.
```

### Command Line Switches

**Windows:**
```
myapp.exe /console   - Run in console mode (not as service)
myapp.exe /install   - Install as Windows service
myapp.exe /start     - Start the service
myapp.exe /stop      - Stop the service
myapp.exe /uninstall - Remove service
myapp.exe /state     - Show service status
myapp.exe /version   - Show version info
myapp.exe /help      - Show syntax
```

**POSIX:**
```
./myapp --console -c  - Run in foreground
./myapp --run -r      - Run as daemon (background)
./myapp --fork -f     - Fork to background
./myapp --kill -k     - Kill daemon
./myapp --state       - Show status
./myapp --help -h     - Show syntax
```

## Key Classes

### TSynDaemon

**Purpose:** Base class for all daemons/services

**Key Methods:**
- `Start` - Override to initialize your service
- `Stop` - Override to clean shutdown (must be safe to call multiple times)
- `Resume` - Override for pause/resume support (Windows only)
- `CommandLine(autoStart)` - Main entry point, processes switches
- `Command(cmd, autoStart, param)` - Direct command execution

**Key Properties:**
- `Settings: TSynDaemonAbstractSettings` - Configuration access
- `ConsoleMode: boolean` - True if running as console app
- `WorkFolderName: TFileName` - Data folder path

**Configuration:**
- Settings stored in JSON file (`<exename>.settings` by default)
- Fallback to INI format if no JSON found
- Logging configured via `Settings.Log`, `Settings.LogPath`

### TSynAngelizeService

**Purpose:** Define a sub-process to be managed by TSynAngelize

**Key Actions** (in `Start`, `Stop`, `Watch` arrays):
- `start:/path/to/exe` - Launch and monitor process (like NSSM)
- `stop:/path/to/exe` - Stop monitored process
- `exec:/path/to/exe` - Execute without waiting
- `wait:/path/to/exe` - Execute and wait for exit code 0
- `http://host/path` - HTTP GET request (expects 200)
- `sleep:1000` - Wait N milliseconds
- `service:Name` - Control Windows service (Windows only)

**Auto-restart:**
- If monitored process exits unexpectedly, auto-restarts with backoff
- `RetryStableSec` - Seconds before considering process "stable" (default 60)
- `AbortExitCodes` - Exit codes that prevent restart
- `Notify` - Send notification on failure (email/HTTP/log/exec)

**Example Configuration (JSON):**
```json
{
  "Name": "WebServer",
  "Run": "/usr/local/bin/myserver",
  "Start": [ "start:%run%" ],
  "Stop": [ "stop:%run%" ],
  "Watch": [ "http://127.0.0.1:8080/health=200" ],
  "WatchDelaySec": 60,
  "RetryStableSec": 120,
  "Notify": "admin@example.com,%log%alerts.log"
}
```

### ICommandLine / TCommandLine

**Purpose:** Parse command line arguments with optional prompting

**Pattern:**
```pascal
var
  cmd: ICommandLine;
begin
  cmd := TCommandLine.Create;

  // Get switch values (prompts user if missing and not NoPrompt)
  Host := cmd.AsString('host', 'localhost', 'Server hostname: ');
  Port := cmd.AsInt('port', 8080, 'Server port: ');
  Verbose := cmd.AsEnum('log', 'info', TypeInfo(TLogLevel), 'Log level: ');

  // Non-interactive mode
  if cmd.NoPrompt then
    WriteLn('Running in batch mode');
end.
```

**Key Methods:**
- `AsUtf8(switch, default, prompt)` - Get UTF-8 string value
- `AsString(switch, default, prompt)` - Get string value
- `AsInt(switch, default, prompt)` - Get integer value
- `AsDate(switch, default, prompt)` - Get ISO-8601 date
- `AsEnum(switch, default, typeinfo, prompt)` - Get enum value
- `AsArray` - Get all params as array (no switch parsing)
- `NoPrompt` - True if `-noprompt` switch present

## Configuration Files

### Daemon Settings

**Default locations:**
- **Windows:** Same folder as executable
- **Linux:** `/etc` for settings, `/var/log` for logs

**Format:** JSON (INI fallback)
```json
{
  "ServiceName": "MyService",
  "ServiceDisplayName": "My Application Service",
  "Log": ["sllInfo", "sllWarning", "sllError"],
  "LogPath": "/var/log/myapp",
  "LogRotateFileCount": 7
}
```

### Angelize Configuration

**Sub-process definitions** stored in JSON arrays:
```json
{
  "Services": [
    {
      "Name": "api",
      "Description": "REST API Server",
      "Run": "/opt/app/api-server",
      "Start": [ "start:%run%" ],
      "Stop": [ "stop:%run%" ],
      "Watch": [ "http://localhost:8080/health" ],
      "WatchDelaySec": 30,
      "RetryStableSec": 60,
      "RedirectLogFile": "%log%api-console.log",
      "RedirectLogRotateFiles": 5,
      "RedirectLogRotateBytes": 10485760
    }
  ]
}
```

## Logging

All daemon classes integrate with `mormot.core.log`:

```pascal
// In TSynDaemonSettings.Create or AfterCreate:
Settings.SetLog(TSynLog);  // Enable logging

// In daemon code:
with TSynLog.Enter(self, 'MyMethod') do
try
  // Your code
  TSynLog.Add.Log(sllInfo, 'Server started on port %', [Port]);
except
  on E: Exception do
    TSynLog.Add.Log(sllError, 'Failed: %', [E.Message], self);
end;
```

**Log levels in settings:**
- `LOG_VERBOSE` - Everything
- `LOG_STACKTRACE` - Includes call stack on errors (default)
- Individual: `sllInfo`, `sllWarning`, `sllError`, `sllDebug`, etc.

## Common Patterns

### Simple Daemon

```pascal
program MyDaemon;
uses
  mormot.app.daemon, mormot.core.log;

type
  TMyDaemon = class(TSynDaemon)
  public
    procedure Start; override;
    procedure Stop; override;
  end;

procedure TMyDaemon.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting service');
  // Initialize your service
end;

procedure TMyDaemon.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping service');
  // Clean shutdown
end;

begin
  with TMyDaemon.Create(TSynDaemonSettings, '', '', '') do
  try
    CommandLine;
  finally
    Free;
  end;
end.
```

### HTTP Microservice Daemon

```pascal
procedure TMyDaemon.Start;
begin
  // Create ORM model and database
  Model := TOrmModel.Create([TOrmSample]);
  DB := TRestServerDB.Create(Model, 'data.db');
  DB.CreateMissingTables;

  // Start HTTP server
  HttpServer := TRestHttpServer.Create(
    '8080',           // Port
    [DB],             // REST servers
    '+',              // Domain mask
    useHttpAsync,     // HTTP mode
    4                 // Thread pool
  );

  TSynLog.Add.Log(sllInfo, 'HTTP server started on :8080');
end;

procedure TMyDaemon.Stop;
begin
  FreeAndNil(HttpServer);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;
```

### Process Manager (Angelize)

```pascal
program ProcessManager;
uses
  mormot.app.agl;

type
  TMyAngelize = class(TSynAngelize)
  end;

var
  Angelize: TMyAngelize;
begin
  // Configuration in ProcessManager.settings JSON:
  // {
  //   "Services": [
  //     { "Name": "web", "Run": "/opt/web/server", ... },
  //     { "Name": "api", "Run": "/opt/api/server", ... }
  //   ]
  // }

  Angelize := TMyAngelize.Create(TMyAngelizeSettings, '', '', '');
  try
    Angelize.CommandLine;
  finally
    Angelize.Free;
  end;
end.
```

## Testing Daemons

**Console mode** for development:
```bash
# Run interactively
./mydaemon --console --verbose

# On Windows
mydaemon.exe /console /verbose
```

**Service mode** for production:
```bash
# Linux
./mydaemon --run
./mydaemon --state
./mydaemon --kill

# Windows
mydaemon.exe /install
net start MyService
mydaemon.exe /state
net stop MyService
mydaemon.exe /uninstall
```

## Integration with mORMot Stack

These app units are designed to work seamlessly with other mORMot2 layers:

- **`mormot.orm.core`** - ORM persistence (SQLite/Postgres/etc)
- **`mormot.rest.http.server`** - RESTful HTTP servers
- **`mormot.soa.core`** - Interface-based services
- **`mormot.net.client`** - HTTP client (for health checks)
- **`mormot.core.log`** - Unified logging
- **`mormot.core.threads`** - Thread management

## Important Notes

- **Settings persistence**: Auto-saved on change, loaded on startup
- **Graceful shutdown**: `Stop` must handle being called multiple times
- **Cross-platform**: Avoid OS-specific code in daemon classes
- **No GUI in daemons**: Windows services cannot show UI
- **Log rotation**: Configured via `LogRotateFileCount` (default 2)
- **Folder defaults**: Windows=exe folder, Linux=/etc, /var/log
- **Command parsing**: Case-insensitive, supports both `/` and `--` prefixes

## Examples

See `/mnt/w/mORMot2/ex/ThirdPartyDemos/martin-doyle/05-HttpDaemonORM/` for a complete working example of HTTP microservice as daemon.

## Documentation

**📖 SAD Chapters**:
- [Chapter 20: Application Servers](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-20.md) - Daemon/service architecture, Angelize
- [Chapter 25: Testing and Logging](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-25.md) - TSynLog integration

---

**Last Updated:** 2025-10-10
**mORMot Version:** 2.x
**License:** MPL/GPL/LGPL
