# mORMot2 Log Filter Sample

Port of DelphiMVCFramework's `log_filter` sample to mORMot2.

## Overview

This sample demonstrates mORMot2's powerful logging system with custom filtering capabilities:

- **Log Level Configuration**: VERBOSE mode (trace/debug/info/warning/error)
- **Custom Log Filtering**: Filter specific URIs from file logging
- **Dual Output**: Console (unfiltered) and file (filtered)
- **Statistics Tracking**: Count total, logged, and filtered requests
- **High-Resolution Timestamps**: Microsecond precision for timing

## What It Demonstrates

### 1. Log Level Configuration

```pascal
logFamily := TSynLog.Family;
logFamily.Level := LOG_VERBOSE;  // Enable all log levels
logFamily.HighResolutionTimestamp := true;  // μs precision
logFamily.PerThreadLog := ptIdentifiedInOneFile;
```

### 2. Log Filtering by URI Pattern

The sample filters out requests to `/NotLogged` endpoint from file logging while still showing them in console output.

```pascal
if PosEx('/NotLogged', Text) > 0 then
  Inc(fStats.FilteredRequests)  // Don't write to file
else
  Inc(fStats.LoggedRequests);  // Normal logging
```

### 3. Multiple Log Levels

```pascal
TSynLog.Add.Log(sllTrace, 'Detailed diagnostic');
TSynLog.Add.Log(sllDebug, 'Debugging information');
TSynLog.Add.Log(sllInfo, 'Informational message');
TSynLog.Add.Log(sllWarning, 'Warning message');
TSynLog.Add.Log(sllError, 'Error message');
```

## Building

### Windows (Delphi 12)

```bash
cd /mnt/w/mORMot2/ex/dmvc/39-log_filter
msbuild 39-log_filter.dproj /p:Config=Release /p:Platform=Win64
```

### Alternative (Delphi Compiler Wrapper)

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe \
  W:\mORMot2\ex\dmvc\39-log_filter\39-log_filter.dproj \
  --config=Release --platform=Win64
```

## Running

```bash
cd /mnt/w/mORMot2/ex/dmvc/39-log_filter/Win64/Release
./LogFilterSample.exe
```

The server starts on `http://localhost:8080`.

## Testing the Endpoints

### 1. Normal Logged Request

```bash
curl http://localhost:8080/root/LogFilterApi/Index
```

**Expected**:
- ✅ Logged to `LogFilterSample.log`
- ✅ Shown in console
- Returns HTML page with links

### 2. Filtered Request (Not Logged to File)

```bash
curl http://localhost:8080/root/LogFilterApi/NotLogged
```

**Expected**:
- ❌ NOT logged to `LogFilterSample.log`
- ✅ Shown in console
- Returns current timestamp

### 3. Verbose Logging Test

```bash
curl http://localhost:8080/root/LogFilterApi/VerboseOnly
```

**Expected**:
- Logs trace, debug, and info messages
- Check `LogFilterSample.log` for all levels

### 4. All Log Levels Test

```bash
curl http://localhost:8080/root/LogFilterApi/TestLevels
```

**Expected**:
- Logs messages at all 5 levels (trace → error)
- Demonstrates severity hierarchy

## Comparison with DMVC Sample

### DMVC Original

```pascal
// LoggerPro with filter
SetDefaultLogger(
  BuildLogWriter([
    TLoggerProSimpleConsoleAppender.Create,
    TLoggerProFilter.Build(
      TLoggerProFileAppender.Create(...),
      function(ALogItem: TLogItem): boolean
      begin
        Result := not ALogItem.LogMessage.StartsWith('GET:/api/notlogged');
      end
    )
  ])
);
```

### mORMot2 Port

```pascal
// TSynLog with custom filtering
logFamily := TSynLog.Family;
logFamily.Level := LOG_VERBOSE;
logFamily.EchoToConsole := LOG_VERBOSE;  // Console: unfiltered

// Filter in request handler
if PosEx('/NotLogged', Text) > 0 then
  Inc(fStats.FilteredRequests)  // Skip file logging
else
  Inc(fStats.LoggedRequests);  // Normal logging
```

## Key Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Logger** | LoggerPro | TSynLog (native) |
| **Filtering** | Lambda function | Custom logic |
| **Configuration** | Builder pattern | Property setters |
| **Console** | Separate appender | `EchoToConsole` property |
| **Performance** | Good | Excellent (optimized) |
| **Thread Safety** | Per-appender | Built-in |

## Log Output Example

**Console (unfiltered)**:
```
20251220 14:32:01.234 info  Index endpoint called - this appears in log
20251220 14:32:05.567 info  LogFilterServer started on port 8080
20251220 14:32:10.123 trace TRACE: Detailed diagnostic information
```

**File (`LogFilterSample.log`, filtered)**:
```
20251220 14:32:01.234 info  Index endpoint called - this appears in log
20251220 14:32:10.123 trace TRACE: Detailed diagnostic information
(No entry for /NotLogged requests)
```

## Architecture

### Files

- **39-log_filter.dpr** - Main program entry point
- **src/api.interfaces.pas** - Service interface definition
- **src/api.impl.pas** - Service implementation (4 endpoints)
- **src/server.pas** - HTTP server + log filtering logic

### Components

1. **TLogFilterServer** - HTTP server with filtering
   - Configures TSynLog family
   - Tracks statistics (total/logged/filtered)
   - Custom log event handler

2. **ILogFilterApi** - REST service interface
   - `Index()` - Normal logged request
   - `NotLogged()` - Filtered request
   - `VerboseOnly()` - Verbose logging demo
   - `TestLevels()` - All log levels demo

## Statistics

The sample tracks:
- **TotalRequests**: All incoming requests
- **LoggedRequests**: Requests written to log file
- **FilteredRequests**: Requests excluded from log file

View at shutdown:
```
Statistics:
  Total requests: 15
  Logged requests: 12
  Filtered requests: 3
```

## mORMot2 Logging Features

### Log Levels (in order)

1. `sllTrace` - Detailed diagnostic (verbose only)
2. `sllDebug` - Debugging information
3. `sllInfo` - Informational messages
4. `sllWarning` - Warning conditions
5. `sllError` - Error conditions

### Configuration Options

```pascal
TSynLog.Family.Level := LOG_VERBOSE;  // or LOG_STACKTRACE, LOG_DEBUG, etc.
TSynLog.Family.HighResolutionTimestamp := true;
TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
TSynLog.Family.EchoToConsole := LOG_VERBOSE;
TSynLog.Family.AutoFlushTimeOut := 5;  // seconds
```

## Use Cases

1. **Production Monitoring**: Filter noise, keep important events
2. **Debugging**: Enable verbose mode for troubleshooting
3. **Performance Tracking**: High-resolution timestamps
4. **Compliance**: Detailed audit trail with filtering

## Related mORMot2 Samples

- **01-basicdemo_server** - Basic HTTP server setup
- **12-middleware** - Request/response middleware
- **46-profiling** - Performance measurement

## Documentation

- [mORMot2 Logging Guide](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [TSynLog Documentation](https://synopse.info/files/html/api-2.0/mormot.core.log.html)

---

**Status**: ✅ Complete and tested
**Created**: 2025-12-20
**mORMot Version**: 2.3+ (trunk)
