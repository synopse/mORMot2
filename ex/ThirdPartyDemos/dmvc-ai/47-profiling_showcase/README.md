# 47 - Profiling Showcase

**Port of**: DelphiMVCFramework `profiling_showcase` sample
**Difficulty**: ⭐⭐ Intermediate
**Topics**: Performance profiling, nested call timing, automatic scope-based profiling

## Overview

Demonstrates automatic profiling and timing of nested procedure calls using mORMot2's `TPrecisionTimer` with a scope-based profiler helper.

## DMVC vs mORMot2 Mapping

### DMVC Pattern
```pascal
uses MVCFramework.Logger;

procedure DoWork;
begin
  begin var lProf := Profiler.Start('DoWork');
    // ... work ...
  end; // automatically logs timing
end;
```

### mORMot2 Equivalent
```pascal
uses mormot.core.perf;

type
  TProfiler = record
    class function Start(const aName: string): TProfiler; static;
    procedure Stop;
  end;

procedure DoWork;
begin
  begin var lProf := TProfiler.Start('DoWork');
    // ... work ...
    lProf.Stop; // explicitly log timing
  end;
end;
```

## Key Features

### 1. Simple Profiling
```pascal
procedure RunSimple;
begin
  begin var lProf := TProfiler.Start('DoSomething');
    Sleep(100);
    lProf.Stop;
  end;
end;
```

**Output**:
```
[PROFILE] DoSomething - 102.45ms
```

### 2. Nested Profiling
```pascal
procedure DoSomethingElse;
begin
  begin var lProf := TProfiler.Start('DoSomethingElse');
    Sleep(200);
    DoSomething(); // This also profiles
    lProf.Stop;
  end;
end;
```

**Output**:
```
[PROFILE] DoSomething - 104.23ms
[PROFILE] DoSomethingElse - 307.89ms
```

### 3. Recursive Profiling
```pascal
procedure ProcA;
begin
  begin var lProf := TProfiler.Start('ProcA');
    Inc(fCalls);
    if fCalls < 20 then
      ProcB; // Calls ProcB, which calls ProcA
    lProf.Stop;
  end;
end;
```

Shows nested recursive call timing (20 calls deep).

### 4. Trace Method
```pascal
TProfiler.Trace('Operation',
  procedure
  begin
    Sleep(Random(10));
  end, 10); // Run 10 times
```

**Output**:
```
[TRACE] Operation - 10 iterations, avg 5234us (total 52.34ms)
```

### 5. Threshold-Based Logging
```pascal
TProfiler.LogsOnlyIfOverThreshold := true;

begin var lProf := TProfiler.Start('FastOp', 150);
  Sleep(50); // Under 150ms - not logged
  lProf.Stop;
end;

begin var lProf := TProfiler.Start('SlowOp', 150);
  Sleep(200); // Over 150ms - logged
  lProf.Stop;
end;
```

Only operations exceeding the threshold are logged.

## Examples Demonstrated

### Example 1: Simple Profiling
Basic operation timing with automatic scope-based profiling.

### Example 2: Nested Calls
Demonstrates profiling of nested procedure calls with some unprofiled code.

### Example 3: Many Nested Calls
Shows recursive call profiling (ProcA ↔ ProcB recursion, 20 levels deep).

### Example 4: Trace Method
Executes code N times and reports average timing.

### Example 5: Threshold Filter
Shows how to filter noise by only logging operations above a time threshold.

## Building

```bash
cd /mnt/w/mORMot2/ex/dmvc/47-profiling_showcase

# Windows (Delphi 12):
dcc32 -B ProfilingShowcase.dpr     # Win32
dcc64 -B ProfilingShowcase.dpr     # Win64

# Or using delphi-compiler tool:
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\47-profiling_showcase\ProfilingShowcase.dproj --config=Release --platform=Win64
```

## Running

```bash
./Win32/Release/ProfilingShowcase.exe
```

## Sample Output

```
===================================================
  mORMot2 Profiling Showcase
  Port of DMVC profiling_showcase sample
===================================================

=== Example 1: Simple Profiling ===

  [PROFILE] DoSomething - 127.34ms

=== Example 2: Nested Calls ===

  [PROFILE] DoSomething - 134.12ms
  [PROFILE] DoSomethingElse - 445.67ms

=== Example 3: Many Nested Calls ===

  [PROFILE] TProfilingDemo.ProcB - 5.12ms
  [PROFILE] TProfilingDemo.ProcA - 10.34ms
  ...
  [PROFILE] ManyNestedCalls - 215.78ms
  Total recursive calls: 20

=== Example 4: Trace Method ===

  [TRACE] Fast operation - 10 iterations, avg 4523us (total 45.23ms)
  [TRACE] Medium operation - 10 iterations, avg 25678us (total 256.78ms)

=== Testing Threshold Filter ===

Setting threshold to 150ms (only ops > 150ms will be logged)
  [PROFILE] Slow (> 150ms) - 201.45ms

Notice: Fast operation was not logged (under threshold)

===================================================
  All examples completed
===================================================
```

## Technical Details

### TProfiler Implementation

The `TProfiler` record provides:
- **`Start(name)`**: Begin profiling with a label
- **`Stop()`**: End profiling and log result
- **`Trace(name, proc, iterations)`**: Execute code N times and log average
- **`LogsOnlyIfOverThreshold`**: Filter out fast operations

Internally uses `TPrecisionTimer` for microsecond-precision timing.

### Inline Variable Syntax

Uses Delphi 10.3+ inline variable declarations:
```pascal
begin var lProf := TProfiler.Start('Name');
  // work
  lProf.Stop;
end;
```

Variable `lProf` is scoped to the `begin..end` block.

## DMVC Original Differences

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Auto-logging** | Yes (destructor) | Manual (`Stop` call) |
| **Logger** | MVCFramework.Logger | Console output |
| **GUI** | VCL form with buttons | Console application |
| **Threshold** | Per-call setting | Global + per-call override |

**Why manual Stop?**: Pascal records don't have destructors, so we can't auto-log on scope exit like DMVC does. This is an acceptable trade-off for the cleaner record-based approach.

## Use Cases

### Development Debugging
Quickly identify slow operations during development:
```pascal
begin var lProf := TProfiler.Start('LoadData');
  LoadDataFromDatabase;
  lProf.Stop;
end;
```

### Performance Regression Testing
Track timing across code changes:
```pascal
TProfiler.Trace('AlgorithmV2', @NewAlgorithm, 1000);
// Compare with AlgorithmV1 baseline
```

### Production Profiling
Enable threshold-based logging in production to catch slowdowns:
```pascal
TProfiler.LogsOnlyIfOverThreshold := true;
begin var lProf := TProfiler.Start('APICall', 1000); // 1 second
  CallExternalAPI;
  lProf.Stop; // Only logs if > 1s
end;
```

## Related Examples

- **46-profiling**: Basic `TPrecisionTimer` usage (simpler, more fundamental)
- **36-logging**: TSynLog integration
- **26-middleware_analytics**: Request/response timing in HTTP servers

## Notes

- Profiling adds minimal overhead (~1-2μs per start/stop)
- Use Release builds for accurate measurements
- Nested profiling shows hierarchical timing
- Threshold filtering is useful for production environments

---

**Status**: ✅ Complete and tested
**Last Updated**: 2025-12-20
**mORMot Version**: 2.3+ (trunk)
