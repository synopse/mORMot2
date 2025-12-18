# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Overview

The mORMot scripting layer provides thread-safe JavaScript engine integration for client and server-side scripting. It enables user-defined workflows and automation in applications built with the mORMot framework.

### Supported Engines

- **QuickJS** (`mormot.script.quickjs.pas`) - Small, standalone JavaScript interpreter ideal for client-side execution
- **SpiderMonkey** (planned) - Mozilla's JavaScript interpreter with JIT compilation for high-performance server scenarios

**Note**: SpiderMonkey support is referenced but not yet implemented in this directory.

---

## Architecture

### Two-Layer Design

1. **Abstract Core** (`mormot.script.core.pas` - 731 lines)
   - Engine-agnostic base classes and interfaces
   - Thread-safe engine pooling via `TThreadSafeManager`
   - Remote debugging infrastructure (Firefox DevTools protocol)
   - Worker thread management

2. **Engine Implementation** (`mormot.script.quickjs.pas` - 30 lines)
   - Currently minimal (implementation pending)
   - Will implement QuickJS-specific bindings

### Key Classes

#### TThreadSafeEngine (Abstract)
Per-thread JavaScript execution context. Never instantiate directly - always obtained via `TThreadSafeManager.ThreadSafeEngine`.

**Critical Properties**:
- `Runtime`/`Context` - Opaque handles to native JS runtime/context (pointers work across all engines)
- `ThreadID` - Owning thread (engine instances are NOT thread-safe across threads)
- `ContentVersion` - Incremental counter for hot-reload detection
- `NeverExpire` - Prevents automatic expiration cleanup

**Lifecycle Hooks** (override in implementations):
- `AfterCreate` - Called outside Manager lock after construction
- `BeforeDestroy` - Called outside Manager lock before destruction
- `DoBeginRequest`/`DoEndRequest` - FPU state management, embraced by `ThreadSafeCall()`
- `From(aContext)` - Static method to retrieve engine from execution context

#### TThreadSafeManager
Manages a pool of per-thread engine instances with automatic expiration.

**Core Methods**:
- `ThreadSafeEngine(ThreadData, Tag)` - Get or create engine for current thread
- `Engine(ThreadID)` - Lookup engine by thread ID (returns nil if unknown)
- `InitializeMainEngine` - Creates non-pooled main engine (call once from main thread)
- `NewEngine` - Create standalone engine outside pool (caller must free)

**Expiration Management**:
- `EngineExpireTimeOutMinutes` (default: 0 = never) - Auto-recreate engines to prevent JS memory leaks
- Production recommendation: 240 minutes (4 hours)
- Engines with `NeverExpire=true` are exempt

**Debugging**:
- `StartDebugger(Port)` - Listen for Firefox DevTools connections (default port: 6000)
- `StopDebugger` - Shutdown debugger thread
- `PauseDebuggerOnFirstStep` - Break on first line of script

**Events**:
- `OnNewEngine` - Triggered after creating engine (customize initial state)
- `OnGetName` - Provide debug name for engine
- `OnGetWebAppRootPath` - Set web app root for debugger context
- `OnDebuggerInit` - Configure debugger when it connects

---

## Thread Safety Model

### Per-Thread Isolation
- Each thread gets its own `TThreadSafeEngine` instance
- Manager maintains a locked list (`TSynObjectListLightLocked`) mapping `ThreadID → Engine`
- **NEVER** share engine instances between threads

### FPU State Protection
`DoBeginRequest`/`DoEndRequest` save/restore FPU flags to prevent corruption from JS execution (especially for SpiderMonkey). Automatically handled by `ThreadSafeCall()`.

### Hot Reload Pattern
```pascal
// Application code increments version when scripts change
Manager.ContentVersion := Manager.ContentVersion + 1;

// Framework auto-detects stale engines and recreates them
Engine := Manager.ThreadSafeEngine; // May return new instance
if Engine.ContentVersion <> Manager.ContentVersion then
  // Engine was recreated - reload your scripts
```

---

## Remote Debugging

### Firefox DevTools Protocol
Implements Firefox's remote debugging protocol (not Chrome DevTools). See:
https://firefox-source-docs.mozilla.org/devtools/backend/protocol.html

### Setup
```pascal
Manager.StartDebugger('6000'); // Default port
// Connect Firefox DevTools to localhost:6000
```

### Interfaces
- `IRemoteDebugger` - Debugger server implementation
- `IWorkerManager` - Worker thread coordination

---

## Implementation Status

**📖 Full Documentation**: See [SAD Chapter 22: Scripting Engine](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-22.md) for complete examples.

### Completed
- Abstract base classes (`TThreadSafeEngine`, `TThreadSafeManager`)
- Thread pooling and lifecycle management
- Debugging infrastructure (Firefox DevTools protocol)
- Worker thread interfaces
- **Low-level QuickJS bindings** (`mormot.lib.quickjs`) - fully functional with `JS_NewRuntime`, `JS_Eval`, etc.

### In Development
- High-level QuickJS wrapper (`mormot.script.quickjs`) - convenience layer
- TQuickJSVariant custom variant type for late-binding
- ORM/SOA integration helpers

### Planned
- SpiderMonkey integration (for high-performance server scenarios)
- JIT compilation support

---

## Integration with mORMot Core

### Dependencies
```pascal
uses
  mormot.core.base,      // Base types
  mormot.core.os,        // OS abstractions
  mormot.core.unicode,   // UTF-8 handling
  mormot.core.text,      // Text utilities
  mormot.core.data,      // Data structures
  mormot.core.rtti,      // RTTI integration
  mormot.core.json,      // JSON serialization
  mormot.lib.static;     // Static library loading
```

### Logging
- `OnLog: TSynLogProc` - Redirect to mORMot's TSynLog
- Manager logs engine creation/destruction if `OnLog` assigned

---

## Common Patterns

### Basic Usage (Typical Server Scenario)
```pascal
var
  Manager: TThreadSafeManager;
  Engine: TThreadSafeEngine;
begin
  Manager := TQuickJSManager.Create(TQuickJSEngine); // Hypothetical
  try
    // In request handler thread:
    Engine := Manager.ThreadSafeEngine;
    Engine.ThreadSafeCall(
      procedure(E: TThreadSafeEngine)
      begin
        // Execute script here
      end);
  finally
    Manager.Free; // Frees all pooled engines
  end;
end;
```

### Main Thread Execution
```pascal
Manager.InitializeMainEngine;
Manager.MainEngine.ThreadSafeCall(...);
```

### Custom Engine Initialization
```pascal
Manager.OnNewEngine :=
  procedure(Engine: TThreadSafeEngine)
  begin
    // Register custom native functions
    // Set global objects
    // Load standard libraries
  end;
```

---

## Key Takeaways for AI Development

1. **Never call engine constructors directly** - Use `TThreadSafeManager.ThreadSafeEngine`
2. **Thread affinity is strict** - One engine per thread, never shared
3. **Low-level QuickJS is available** - Use `mormot.lib.quickjs` for direct JS execution
4. **High-level wrapper in development** - `mormot.script.quickjs` convenience layer coming
5. **Debugging uses Firefox protocol** - Not Chrome DevTools
6. **ContentVersion drives hot reload** - Application responsibility to increment
7. **FPU corruption prevention** - Always use `ThreadSafeCall()` for script execution
8. **Expiration prevents memory leaks** - Set `EngineExpireTimeOutMinutes` for long-running servers

---

## Related Documentation

**📖 SAD Documentation**: [SAD Chapter 22: Scripting Engine](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-22.md) - comprehensive guide with code examples

**External References**:
- Main README: `/mnt/w/mORMot2/README.md`
- Framework docs: https://synopse.info/files/doc/mORMot2.html
- QuickJS upstream: https://bellard.org/quickjs/
- Firefox debugging protocol: https://firefox-source-docs.mozilla.org/devtools/backend/protocol.html

---

**Last Updated**: 2025-10-10 (based on source analysis)
**Status**: Abstract layer complete, engine implementations pending
**License**: MPL/GPL/LGPL tri-license
