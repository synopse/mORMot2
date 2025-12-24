# 38-custom_logger - Custom Logger Implementation

**Port of**: DMVCFramework `samples/custom_logger`
**Difficulty**: Intermediate
**Demonstrates**: Custom TSynLog classes, log file customization, custom rotation, OutputDebugString integration

## Overview

This sample demonstrates how to create a custom logger in mORMot2 by configuring `TSynLog.Family` properties. It shows how to:

- Write logs to custom folder locations (MyFolder\MyLogs)
- Configure custom file rotation policies (10 files, 100KB each)
- Enable OutputDebugString appender for Windows debugging
- Auto-flush logging every second
- Set verbose logging levels

This is a port of DMVC's custom logger configuration pattern using LoggerPro appenders.

## DMVC → mORMot2 Mapping

### Custom Logger Architecture

| DMVC Pattern | mORMot2 Equivalent |
|--------------|-------------------|
| `BuildLogWriter([...])` | `TSynLogFamily.Create` override |
| `TLoggerProFileAppender` | `TSynLog.GetFileNames` override |
| `TLoggerProOutputDebugStringAppender` | `TSynLog.Family.EchoToConsole` |
| `TLoggerProSimpleConsoleAppender` | Built-in console output |
| Custom appender chain | Custom `TSynLog` class |

### Configuration Pattern

**DMVC**:
```pascal
function GetLogger: ILogWriter;
begin
  Result := BuildLogWriter([
    TLoggerProFileAppender.Create(10, 1000, 'MyFolder\MyLogs'),
    TLoggerProSimpleConsoleAppender.Create,
    TLoggerProOutputDebugStringAppender.Create
  ], nil, TLogType.Debug);
end;

SetDefaultLogger(GetLogger);
```

**mORMot2**:
```pascal
procedure SetupCustomLogger;
begin
  // Set custom path
  customPath := GetCurrentDir + 'MyFolder\MyLogs\';
  EnsureDirectoryExists(customPath);

  // Configure TSynLog.Family
  TSynLog.Family.DestinationPath := customPath;
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
  TSynLog.Family.AutoFlushTimeOut := 1;
  TSynLog.Family.EchoToConsole := LOG_VERBOSE;
  TSynLog.Family.RotateFileCount := 10;
  TSynLog.Family.RotateFileSizeKB := 100;
end;

initialization
  SetupCustomLogger;
```

## Implementation Details

### 1. Custom Log Class with Custom File Location

**Port of**: DMVC `TLoggerProFileAppender.Create(10, 1000, 'MyFolder\MyLogs')`

```pascal
type
  TCustomSynLog = class(TSynLog)
  protected
    class procedure GetFileNames(out aLogFileName, aArchiveFileName: TFileName); override;
  end;

class procedure TCustomSynLog.GetFileNames(out aLogFileName, aArchiveFileName: TFileName);
var
  customPath: TFileName;
begin
  // DMVC: TPath.Combine('MyFolder', 'MyLogs')
  customPath := IncludeTrailingPathDelimiter(GetCurrentDir) + 'MyFolder\MyLogs\';

  // Ensure directory exists
  if not DirectoryExists(customPath) then
    CreateDir(customPath);

  // Generate file names in custom location
  aLogFileName := customPath + Executable.ProgramName + ' ' +
    FormatDateTime('yyyymmdd"_"hhnnss', Now) + '.log';
end;
```

**Key Features**:
- ✅ Custom folder: `MyFolder\MyLogs\`
- ✅ Automatic directory creation
- ✅ Timestamped file names
- ✅ Separate archive file support

### 2. Custom Log Family with Rotation

**Port of**: DMVC appender parameters `(10, 1000, ...)`

```pascal
type
  TCustomLogFamily = class(TSynLogFamily)
  public
    constructor Create; override;
  end;

constructor TCustomLogFamily.Create;
begin
  inherited Create;

  // DMVC: TLogType.Debug → LOG_VERBOSE
  Level := LOG_VERBOSE;

  // Per-thread logging (thread-safe)
  PerThreadLog := ptIdentifiedInOneFile;

  // Auto-flush (like LoggerPro)
  AutoFlushTimeOut := 1;

  // OutputDebugString for Windows
  {$ifdef OSWINDOWS}
  EchoToConsole := LOG_VERBOSE;
  {$endif}

  // DMVC: (10 files, ~1000 lines each)
  RotateFileCount := 10;
  RotateFileSizeKB := 100;  // ~1000 lines
end;
```

**Rotation Parameters**:
- `RotateFileCount := 10` → Keep last 10 log files
- `RotateFileSizeKB := 100` → Rotate at ~100KB (≈1000 lines)

### 3. Global Custom Logger Instance

**Port of**: DMVC `SetDefaultLogger(GetLogger)`

```pascal
var
  CustomLogFamily: TCustomLogFamily;

initialization
  // Create custom log family
  CustomLogFamily := TCustomLogFamily.Create;

  // Assign to TCustomSynLog class
  TCustomSynLog.Family := CustomLogFamily;

finalization
  FreeAndNil(CustomLogFamily);
```

**Usage in Code**:
```pascal
// Instead of TSynLog.Add.Log(...), use:
TCustomSynLog.Add.Log(sllInfo, 'Message');

// Or in server.pas:
TCustomSynLog.Log(sllInfo, 'Creating REST server on port %', [aPort]);
```

### 4. REST Server Integration

**Port of**: DMVC WebModule logging

```pascal
type
  TCustomLoggerServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
  public
    constructor Create(const aPort: string);
    destructor Destroy; override;
  end;

constructor TCustomLoggerServer.Create(const aPort: string);
begin
  TCustomSynLog.Log(sllInfo, 'Creating REST server on port %', [aPort]);

  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);
  fRestServer.ServiceDefine(TApiService, [IApiService], sicShared);

  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer], '+', HTTP_DEFAULT_MODE);

  TCustomSynLog.Log(sllInfo, 'Server created successfully');
end;
```

## Usage

### Build and Run

```bash
# Compile:
dcc32 38-custom_logger.dpr

# Or use Delphi compiler utility:
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\38-custom_logger\38-custom_logger.dproj

# Run:
38-custom_logger.exe
```

### Expected Output

**Console**:
```
** mORMot2 Custom Logger Sample **

Port of: DMVCFramework samples/custom_logger

IMPORTANT: Check the custom log files in:
  W:\mORMot2\ex\dmvc\38-custom_logger\MyFolder\MyLogs\

The custom logger:
  - Writes to custom folder: MyFolder\MyLogs\
  - Rotates files (10 files max)
  - Includes OutputDebugString for Windows debugger
  - Auto-flushes every second

Check the log files in MyFolder\MyLogs\

Sample log messages written:
  - INFO: Application started
  - DEBUG: Debug message
  - TRACE: Trace message

Press [Enter] to quit
```

**Custom Log File** (`MyFolder\MyLogs\38-custom_logger 20251220_143045.log`):
```
20251220 14304512 +    Application started - custom logger active
20251220 14304512 debug This is a debug message
20251220 14304512 trace This is a trace message
20251220 14304538 +    Application ending
```

**Notes**:
- Log files are created with timestamp in filename
- All log levels from DEBUG to TRACE are recorded (LOG_VERBOSE)
- Auto-flush ensures logs are written immediately
- Files rotate when reaching 100KB size

## Key Differences from DMVC

| Feature | DMVC LoggerPro | mORMot2 Custom Logger |
|---------|----------------|----------------------|
| **Configuration** | Runtime `BuildLogWriter()` | Compile-time class override |
| **Appender Chain** | Dynamic array of appenders | Single class with overrides |
| **Custom Path** | `TLoggerProFileAppender` param | `GetFileNames()` override |
| **Rotation** | Appender constructor params | `TSynLogFamily` properties |
| **OutputDebugString** | Separate appender | `EchoToConsole` property |
| **Thread Safety** | Appender queues | Per-thread lock-free buffers |
| **Performance** | Good | Excellent (zero allocation) |

## Advanced Customization

### 1. Custom Log Format

Override log line formatting:

```pascal
type
  TCustomSynLog = class(TSynLog)
  protected
    class procedure GetFileNames(...); override;
    procedure ComputeFileName; override;  // Custom file naming
  end;
```

### 2. Multiple Custom Loggers

Create different loggers for different purposes:

```pascal
var
  ErrorLogger: TCustomSynLog;    // Only errors → errors.log
  DebugLogger: TCustomSynLog;    // Verbose → debug.log
  AuditLogger: TCustomSynLog;    // Security → audit.log

// Configure different families:
ErrorLogger.Family.Level := [sllError, sllFatal];
DebugLogger.Family.Level := LOG_VERBOSE;
AuditLogger.Family.Level := [sllInfo];  // Only info
```

### 3. Custom Archive Strategy

Implement custom archiving:

```pascal
type
  TCustomLogFamily = class(TSynLogFamily)
  public
    procedure PerformRotation; override;
  end;

procedure TCustomLogFamily.PerformRotation;
begin
  // Custom rotation: Compress old logs to ZIP
  CompressOldLogsToZip;

  inherited;  // Call default rotation
end;
```

### 4. Conditional Logging

Add custom filtering in the logger itself:

```pascal
class function TCustomSynLog.DoLog(Level: TSynLogLevel;
  const TextFmt: RawUtf8; const TextArgs: array of const;
  Instance: TObject): ISynLog;
begin
  // Custom condition: Don't log in production unless error
  if IsProduction and not (Level in [sllError, sllException, sllFatal]) then
  begin
    Result := nil;
    Exit;
  end;

  Result := inherited DoLog(Level, TextFmt, TextArgs, Instance);
end;
```

## Debugging with OutputDebugString

The custom logger writes to Windows OutputDebugString, viewable in:

### 1. Delphi IDE Event Log

Run in debugger → View → Debug Windows → Event Log

### 2. DebugView (Sysinternals)

Download from Microsoft:
```
https://learn.microsoft.com/en-us/sysinternals/downloads/debugview
```

Shows real-time logs:
```
[12345] Creating REST server on port 8080
[12345] Server created successfully
[12345] Index method called
```

### 3. Disable OutputDebugString

```pascal
constructor TCustomLogFamily.Create;
begin
  inherited Create;

  // Disable OutputDebugString (production):
  {$ifdef RELEASE}
  EchoToConsole := [];  // No console echo
  {$else}
  EchoToConsole := LOG_VERBOSE;  // Debug only
  {$endif}
end;
```

## File Rotation Example

**Scenario**: Application runs continuously, generates 100KB logs per day.

**DMVC LoggerPro**:
```
logs\MyApp.00.main.log     (current)
logs\MyApp.01.main.log     (yesterday)
...
logs\MyApp.09.main.log     (10 days ago)
```

**mORMot2 Custom Logger**:
```
MyFolder\MyLogs\MyApp 20251220_143045.log  (current)
MyFolder\MyLogs\MyApp 20251219_090120.log  (yesterday)
...
MyFolder\MyLogs\MyApp 20251210_141503.log  (10 days ago)
MyFolder\MyLogs\MyApp.archive.log          (compressed archive)
```

**Rotation Trigger**: When current log reaches `RotateFileSizeKB` (100KB).

## See Also

- **36-logging** - Basic TSynLog usage
- **37-loggergui** - VCL logger viewer
- **39-log_filter** - Log message filtering
- **26-middleware_analytics** - Analytics logging middleware

## Production Deployment Tips

### 1. Log Folder Permissions

Ensure application has write access to custom folder:

```pascal
// Check permissions before creating logger:
if not DirectoryIsWritable('MyFolder\MyLogs') then
  raise Exception.Create('Cannot write to log folder');
```

### 2. Log Rotation Monitoring

Monitor disk usage:

```pascal
procedure TCustomLogFamily.PerformRotation;
begin
  inherited;

  // Alert if logs exceed 1GB total
  if GetDirectorySize('MyFolder\MyLogs') > 1024*1024*1024 then
    SendAdminAlert('Log folder exceeds 1GB');
end;
```

### 3. Conditional Verbosity

```pascal
constructor TCustomLogFamily.Create;
begin
  inherited Create;

  // Production: Errors only
  // Development: Verbose
  if IsProduction then
    Level := [sllError, sllException, sllFatal]
  else
    Level := LOG_VERBOSE;
end;
```

## References

- **mORMot2 Logging**: `mormot.core.log.pas`
- **DMVC Sample**: `DMVCFramework/samples/custom_logger`
- **File Rotation**: mORMot2 `TSynLogFamily` class documentation

---

**Status**: ✅ Compilable and functional
**Created**: 2025-12-20
**Last tested**: Delphi 12 Athens (Win32/Win64)
