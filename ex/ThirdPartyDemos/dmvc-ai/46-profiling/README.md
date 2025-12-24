# mORMot2 Profiling Sample

**Port of DMVC `profiling` sample to mORMot2**

## Overview

This sample demonstrates mORMot2's built-in performance profiling capabilities, equivalent to DMVC's `Profiler` class but using mORMot2's high-performance logging and timing infrastructure.

## Key Concepts

### DMVC vs mORMot2 Profiling Comparison

| DMVC Pattern | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `var lProf := Profiler.Start(...)` | `var log: ISynLog; log := TSynLog.Enter(...)` | RAII-style automatic timing |
| Manual `Profiler.ProfileLogger := Log` | Automatic via `TSynLog.Family` | No setup needed |
| Profiler writes to DMVC log | ISynLog writes to `.log` file | High-resolution timestamps |
| Block-scoped profiling | Interface reference counting | Automatic cleanup |

### mORMot2 Profiling Tools

1. **ISynLog Interface (Recommended)**
   - RAII-style profiling (automatic entry/exit logging)
   - Thread-safe, high-resolution timestamps
   - Automatic method name tracking
   - Zero overhead when logging disabled

   ```pascal
   function MyMethod: RawUtf8;
   var
     log: ISynLog;
   begin
     log := TSynLog.Enter(Self, 'MyMethod');
     // Your code here
     Result := 'Done';
     // log.Leave called automatically when log goes out of scope
   end;
   ```

2. **TPrecisionTimer (Manual Timing)**
   - For custom timing scenarios
   - Sub-microsecond precision
   - Useful for benchmarking specific code blocks

   ```pascal
   var
     timer: TPrecisionTimer;
   begin
     timer.Start;
     // Code to measure
     WriteLn('Elapsed: ', timer.Stop); // "123.45ms"
   end;
   ```

## Architecture

```
46-profiling.dpr               Main program (enables high-res logging)
├── src/server.pas             TProfilingServer - HTTP server setup
├── src/api.interfaces.pas     IProfilingApi - Service interface
└── src/api.impl.pas           TProfilingApi - Profiled implementation
```

### Key Implementation Details

- **High-Resolution Timestamps**: Enabled via `TSynLog.Family.HighResolutionTimestamp := true`
- **Nested Profiling**: ISynLog tracks call hierarchy automatically
- **Selective Profiling**: Only methods with `ISynLog` declarations are profiled
- **Log Output**: Written to `ProfilingSample.log` in executable directory

## Endpoints

All endpoints demonstrate different profiling patterns:

1. **GET** `/api` - **Index**
   - Recursive call profiling (ProcA ↔ ProcB, 20 iterations)
   - Shows nested timing in log output

2. **GET** `/api/profilersample1` - **Nested Profiling**
   - Demonstrates selective profiling
   - `NotProfiled()` calls intentionally excluded
   - `DoSomething()` → `DoSomethingElse()` → `DoSomething()` call chain

3. **GET** `/api/profilersample2` - **Simple Profiling**
   - Basic ISynLog usage
   - 100ms sleep for visible timing

## Running the Sample

```bash
# Compile
delphi-compiler.exe /mnt/w/mORMot2/ex/dmvc/46-profiling/46-profiling.dproj

# Run
./46-profiling.exe

# Test endpoints
curl http://localhost:8080/api
curl http://localhost:8080/api/profilersample1
curl http://localhost:8080/api/profilersample2
```

## Reading Profiling Output

Check `ProfilingSample.log` for detailed timing:

```
20231220 10:15:30.123 +    Enter TProfilingApi.Index
20231220 10:15:30.123 +      Enter TProfilingApi.ProcA
20231220 10:15:30.123 +        Enter TProfilingApi.ProcB
20231220 10:15:30.123 +          Enter TProfilingApi.ProcA
...
20231220 10:15:30.456 -        Leave TProfilingApi.ProcB (333.12ms)
20231220 10:15:30.456 -      Leave TProfilingApi.ProcA (333.45ms)
20231220 10:15:30.456 -    Leave TProfilingApi.Index (333.89ms)
```

**Legend:**
- `+` = Enter method
- `-` = Leave method (with elapsed time)
- Indentation = call hierarchy depth

## Performance Considerations

- **ISynLog overhead**: ~0.1-1 μs per Enter/Leave (negligible for most use cases)
- **Logging disabled**: Zero overhead when `TSynLog.Family.Level` excludes `sllEnter/sllLeave`
- **Thread-safe**: Each thread gets isolated log context
- **Async writes**: Log file I/O is asynchronous (no blocking)

## Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| Profiling API | `Profiler.Start()` + manual scope | `ISynLog` + RAII |
| Logger setup | `Profiler.ProfileLogger := Log` | Automatic via `TSynLog.Family` |
| Output format | Custom CSV/text | Structured log with hierarchy |
| Call depth tracking | Manual | Automatic via interface stack |
| Thread safety | Requires manual sync | Built-in (thread-local storage) |

## See Also

- [mormot.core.perf.pas](../../../src/core/mormot.core.perf.pas) - Performance monitoring classes
- [mormot.core.log.pas](../../../src/core/mormot.core.log.pas) - Logging infrastructure
- [DMVC profiling sample](https://github.com/danieleteti/delphimvcframework/tree/master/samples/profiling) - Original DMVC implementation

## Notes

- This sample uses in-memory REST server (`TRestServerFullMemory`) - no database required
- Profiling works identically for database-backed services
- For production: adjust `TSynLog.Family.Level` to exclude verbose profiling if not needed
- High-resolution timestamps require Windows Vista+ or Linux 2.6.28+ (fallback available)

---

**Last Updated**: 2025-12-20
**mORMot Version**: 2.3+
**Original DMVC Sample**: DelphiMVCFramework/samples/profiling
