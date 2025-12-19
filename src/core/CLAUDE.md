# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `mORMot2/src/core` folder contains the foundational units of the mORMot 2 framework - cross-platform, cross-compiler "bricks" that provide:
- Low-level text, JSON, binary processing
- RTTI abstraction layer (avoiding direct `TypInfo.pas` use)
- OS/platform abstraction
- Memory, compression, threading, logging

All higher-level features (ORM, SOA, database access) build on these core units.

## Architecture Principles

### Layered Dependencies
Core units have **strict dependency order**. Lower layers never depend on higher ones:
```
mormot.core.base (RTL types, asm stubs, no dependencies)
  -> mormot.core.os (OS abstraction, threading primitives)
    -> mormot.core.unicode (charset/encoding)
      -> mormot.core.text (text parsing, CSV, formatting)
        -> mormot.core.datetime (ISO-8601, TTimeLog)
          -> mormot.core.rtti (RTTI wrapper, TRttiCustom cache)
            -> mormot.core.buffers (compression, base64, streams)
              -> mormot.core.data (TDynArray, binary serialization)
                -> mormot.core.json (JSON parsing/generation)
                  -> [higher units: variants, collections, log, etc.]
```

**Key Rule**: When modifying units, respect this hierarchy. Adding a reference that creates a circular dependency will break compilation.

### Cross-Compiler Abstraction
- All units must compile on **both FPC and Delphi** (Delphi 7 to 12.2, FPC 3.2+)
- Compiler-specific code isolated in `.inc` files:
  - `mormot.core.rtti.fpc.inc` / `mormot.core.rtti.delphi.inc`
  - `mormot.core.os.posix.inc` / `mormot.core.os.windows.inc`
  - `mormot.core.base.asmx64.inc` / `mormot.core.base.asmx86.inc`
- Use `{$ifdef FPC}` / `{$ifdef ISDELPHI}` sparingly; prefer abstraction in `.inc`

### Performance-Critical Design
- **No memory allocation** in hot paths (e.g., JSON parsing uses pointers)
- **CPU-specific asm**: x86_64/i386 optimizations in `mormot.core.base.asm*.inc`
- **Inlining aggressively**: Most functions marked `inline` for zero-call overhead
- **Lookup tables**: See `TJsonTokens`, `TJsonCharSet` for branch-less parsing

## Key Units Reference

### mormot.core.base
**Foundation unit** - must have **zero dependencies** outside RTL.
- Cross-platform types (`PtrInt`, `PtrUInt`, `RawUtf8`, `RawByteString`)
- Low-level utilities (sorting, hashing, buffer ops, SynLZ compression)
- **Asm stubs included here** (`.asmx64.inc`, `.asmx86.inc`)
- Version constants: `SYNOPSE_FRAMEWORK_VERSION`, `SYNOPSE_FRAMEWORK_BRANCH`

### mormot.core.os
**OS abstraction layer** - replaces `SysUtils` + OS-specific units.
- Threading: `TSynLocker`, `TSynLocked`, `SetThreadName()`
- File/console/process APIs
- Windows/POSIX differences hidden via `.inc` files
- **Goal**: No `Windows`, `Linux`, `Unix` in regular unit uses clauses

**Tip**: Always prefer `mormot.core.os` functions over direct RTL calls for portability.

### mormot.core.rtti
**RTTI abstraction** - avoids `TypInfo.pas` incompatibilities.
- Defines `TRtti*` types (not `TTypeInfo`/`PPropInfo` from RTL)
- `TRttiCustom`: Main cached RTTI class (enhanced as `TRttiJson` in `mormot.core.json`)
- Access via global `Rtti.*` methods (thread-safe cache)
- **Compiler-specific code**: `mormot.core.rtti.fpc.inc` / `mormot.core.rtti.delphi.inc`

**Why**: FPC and Delphi have different RTTI layouts; this provides unified API.

### mormot.core.json
**JSON processing** - optimized for speed (900MB/s+ on modern CPUs).
- `TJsonWriter`: Low-level JSON generation with proper escaping
- `GetJsonField()` / `GetJsonPropName()`: Pointer-based parsing (no allocation)
- `TDocVariant`: Dynamic JSON objects/arrays as `Variant`
- Relies on `TRttiCustom` (from `mormot.core.rtti`) for serialization

**Pattern**: JSON processing never allocates strings during parsing; uses pointers into input buffer.

### mormot.core.data
**Data structures** - dynamic arrays, binary serialization.
- `TDynArray`: Wrapper for dynamic arrays with sorting/searching/JSON
- `TDynArrayHashed`: Adds O(1) hash lookup
- `TRawUtf8List`: Fast string list (replaces `TStringList`)
- Binary serialization: `BinarySave()` / `BinaryLoad()` for records/arrays

### mormot.core.variants
**Variant extensions** - `TDocVariant` for JSON objects/arrays.
- `TDocVariant`: Late-binding JSON object (`doc.name := 'value'`)
- `IDocList` / `IDocDict`: Interface wrappers for managed lifetimes
- Performance: Slower than typed records, but convenient for scripting

### mormot.core.log / mormot.core.perf
**Production logging** with near-zero overhead.
- `TSynLog`: High-performance file logger (async writes)
- `ISynLog`: Interface-based RAII for method enter/leave
- Debug symbols: Reads Delphi `.map` or FPC DWARF for stack traces

### mormot.core.threads
**Threading utilities** - thread pools, background tasks.
- `TSynBackgroundThreadMethod`: Run method in background
- `TSynParallelProcess`: Parallel execution across CPU cores
- `TBlockingProcess`: Queue with producer/consumer pattern

## Configuration

### Conditional Defines (mormot.defines.inc)
All core units include `{$I ..\mormot.defines.inc}` which defines:
- `PUREMORMOT2`: No mORMot 1.18 compatibility (recommended for new code)
- `FPC_X64MM`: Use custom x64 memory manager (FPC only, Linux/Windows)
- `FPCMM_BOOST` / `FPCMM_SERVER`: Memory manager threading modes
- `NEWRTTINOTUSED`: Exclude Delphi 2010+ enhanced RTTI (smaller EXE)
- `NOPATCHVMT` / `NOPATCHRTL`: Disable runtime patches

**When to use**: Set these in project options, not in unit source.

### Memory Managers (FPC)
- **Default**: RTL memory manager (single-threaded optimized)
- **FPC_X64MM**: `mormot.core.fpcx64mm` - Multi-threaded MM (x86_64 only)
  - Based on FastMM4 algorithms
  - Lockless bins for tiny blocks (<=128/256 bytes)
  - Use `FPCMM_SERVER` mode for multi-threaded servers
- **FPC_LIBCMM**: `mormot.core.fpclibcmm` - System malloc/free wrapper

**Include via**: `{$I mormot.uses.inc}` in program (see `test/mormot2tests.dpr`)

## Testing

### Run Regression Tests
```bash
# Compile and run
cd /mnt/w/mORMot2/test
fpc mormot2tests.dpr  # or open in Delphi/Lazarus
./mormot2tests

# On Lazarus: Install packages/lazarus/mormot2.lpk first
```

Tests validate:
- Cross-compiler compatibility (FPC vs Delphi)
- Asm correctness (x86/x64 vs Pascal fallback)
- JSON parsing/generation round-trips
- RTTI serialization edge cases

## Common Patterns

### Adding New Low-Level Functionality
1. **Choose correct layer**: Does it depend on JSON? RTTI? OS? Text? Place accordingly
2. **Check circular deps**: Use `grep "^uses" *.pas` to verify dependency order
3. **Add unit tests**: Update `test/test.core.*.pas` matching unit name
4. **Cross-compiler test**: Compile with both FPC and Delphi before committing

### Optimizing Hot Paths
1. **Profile first**: Use `mormot.core.perf` (`TSynMonitor`) or external profiler
2. **Lookup tables**: See `JSON_TOKENS[]`, `BYTE_IS_DIGIT[]` patterns
3. **Pointer arithmetic**: Avoid string copies; use `PUtf8Char` and `Inc(P)`
4. **Asm optimization**: Only after Pascal is correct; test on multiple CPUs

### Working with RTTI
```pascal
// DON'T: Use TypInfo.pas directly (breaks cross-compiler)
var info: PTypeInfo;
info := TypeInfo(TMyRecord);

// DO: Use mormot.core.rtti abstractions
var info: PRttiInfo;
info := TypeInfo(TMyRecord);  // PRttiInfo is redefined wrapper

// DO: Use cached RTTI
var rt: TRttiCustom;
rt := Rtti.RegisterType(TypeInfo(TMyRecord));  // Returns cached instance
```

## File Organization

### Source Files
```
mormot.core.*.pas          # Main unit implementations (24 units)
mormot.core.*.inc          # Compiler/platform-specific code
mormot.defines.inc         # Global conditionals
mormot.uses.inc            # Memory manager selection
```

### No Project Files Here
- `.dpr` / `.dproj` files are in `/mnt/w/mORMot2/test` or `/mnt/w/mORMot2/packages`
- Core units are **library code only** (no executables)

## Documentation

**📖 SAD Chapters**:
- [Chapter 3: Framework Architecture](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-03.md) - Unit organization, dependency layers
- [Chapter 4: Core Classes](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-04.md) - TDynArray, variants, JSON, logging

**External Resources**:
- **Inline docs**: All public types/functions documented in source (Doxygen-style)
- **Official manual**: https://synopse.info/files/doc/mORMot2.html
- **Architecture overview**: `/mnt/w/mORMot2/README.md`
- **Forums**: https://synopse.info/forum/viewforum.php?id=24

## Notes for AI Assistants

- **Never break dependency order**: If `mormot.core.base` needs JSON, you're doing it wrong
- **Respect cross-compiler goal**: Code must work on FPC and Delphi without `{$ifdef}` clutter
- **Performance matters**: This is not application code; every cycle counts (used in production by thousands)
- **Asm is optional**: Always provide Pascal fallback; asm is optimization, not requirement
- **Test everything**: Changes to core affect all framework users; regression tests are mandatory

---

**Last Updated**: 2025-10-10
**mORMot Version**: 2.3+ (trunk)
**Maintained By**: Synopse Informatique - Arnaud Bouchez
