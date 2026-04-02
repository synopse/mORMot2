# 22. Scripting Engine

*JavaScript Integration for Dynamic Applications*

mORMot provides JavaScript scripting capabilities for applications that need dynamic behavior, user-defined workflows, or hot-reloadable business logic. This chapter covers the scripting architecture and available engines.

---

## 22.1. Why Scripting?

### 22.1.1. Use Cases

| Scenario | Benefit |
|----------|---------|
| User-defined workflows | End-users customize behavior without recompilation |
| Business rules | Domain experts define evolving logic |
| Customization | Single executable serves multiple clients |
| Reporting | Dynamic report definitions |
| Plugin systems | Third-party extensions |
| Rapid prototyping | Quick iteration without compilation |

### 22.1.2. Language Choice: JavaScript

mORMot chose JavaScript for scripting because:

- **Universal knowledge** - Nearly every programmer knows JavaScript
- **Powerful when used well** - Modern ES2020+ features
- **Rich ecosystem** - Thousands of libraries available
- **Client/server sharing** - Same validation logic on both sides
- **Proven engines** - QuickJS and SpiderMonkey are production-ready

---

## 22.2. Architecture Overview

### 22.2.1. Two-Layer Design

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Code                             │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                   mormot.script.core.pas                        │
│           (Abstract Engine Management Layer)                    │
│                                                                 │
│  ┌─────────────────────┐  ┌──────────────────────────────┐      │
│  │ TThreadSafeManager  │  │ TThreadSafeEngine (abstract) │      │
│  │                     │  │                              │      │
│  │ • Engine pooling    │  │ • Per-thread execution       │      │
│  │ • Thread safety     │  │ • Context isolation          │      │
│  │ • Hot reload        │  │ • FPU state protection       │      │
│  │ • Remote debugging  │  │ • Lifecycle hooks            │      │
│  └─────────────────────┘  └──────────────────────────────┘      │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                   Engine Implementations                        │
│                                                                 │
│  ┌──────────────────────┐  ┌──────────────────────────┐         │
│  │ mormot.lib.quickjs   │  │ (SpiderMonkey planned)   │         │
│  │                      │  │                          │         │
│  │ • ES2020 support     │  │ • JIT compilation        │         │
│  │ • Small footprint    │  │ • High performance       │         │
│  │ • Static linking     │  │ • node.js compatible     │         │
│  └──────────────────────┘  └──────────────────────────┘         │
└─────────────────────────────────────────────────────────────────┘
```

### 22.2.2. Supported Engines

| Engine | Use Case | Status |
|--------|----------|--------|
| **QuickJS** | Client-side, embedded | Available (via `mormot.lib.quickjs`) |
| **SpiderMonkey** | Server-side, high-performance | Planned |

---

## 22.3. QuickJS Integration

### 22.3.1. About QuickJS

QuickJS is a small, embeddable JavaScript engine by Fabrice Bellard:

- **ES2020 support** - Modules, async/await, BigInt, Proxies
- **Small footprint** - ~400KB static library
- **No external dependencies** - Fully standalone
- **Cross-platform** - Windows, Linux, macOS

### 22.3.2. Low-Level API

The `mormot.lib.quickjs.pas` unit provides direct QuickJS bindings. Here's a complete working example demonstrating script execution, function calls, and proper memory management:

```pascal
uses
  mormot.lib.quickjs,
  mormot.core.base;

procedure QuickJSExample;
var
  Runtime: JSRuntime;
  Context: JSContext;
  Script, FuncName: RawUtf8;
  Global, Func, Res: JSValue;
  Args: array[0..1] of JSValue;
  ResultInt: integer;
begin
  // 1. Create runtime and context
  Runtime := JS_NewRuntime;
  if Runtime = nil then
    raise Exception.Create('Failed to create JSRuntime');

  Context := JS_NewContext(Runtime);
  if Context = nil then
  begin
    JS_FreeRuntime(Runtime);
    raise Exception.Create('Failed to create JSContext');
  end;

  try
    // 2. Execute JavaScript to define a function
    Script := 'function multiply(a, b) { return a * b; }';
    Res.Raw := JS_Eval(Context, pointer(Script), length(Script),
                       'init.js', JS_EVAL_TYPE_GLOBAL);

    // Check for script errors (IsException and type checks are on JSValue)
    if Res.IsException then
    begin
      WriteLn('Script error');
      Context^.Free(Res);
      Exit;
    end;
    Context^.Free(Res);  // Free eval result

    // 3. Get the global object (JS_GetGlobalObject returns JSValueRaw)
    Global.Raw := JS_GetGlobalObject(Context);

    // 4. Get the function by name (JS_GetPropertyStr returns JSValueRaw)
    FuncName := 'multiply';
    Func.Raw := JS_GetPropertyStr(Context, Global.Raw, pointer(FuncName));

    if JS_IsFunction(Context, Func.Raw) = 0 then
    begin
      WriteLn('multiply is not a function');
      Context^.Free(Func);
      Context^.Free(Global);
      Exit;
    end;

    // 5. Prepare arguments: integer tags need no heap allocation
    Args[0].From32(7);
    Args[1].From32(6);

    // 6. Call the function (JS_Call returns JSValueRaw)
    Res.Raw := JS_Call(Context, Func.Raw, Global.Raw, 2, @Args[0]);

    // 7. Get result (IsNumber, IsException are methods on JSValue)
    if Res.IsNumber then
    begin
      if JS_ToInt32(Context, @ResultInt, Res.Raw) = 0 then
        WriteLn('7 × 6 = ', ResultInt);
    end
    else if Res.IsException then
      WriteLn('Function call error');

    // 8. Clean up JSValues that reference heap objects
    Context^.Free(Res);
    // Args[0] and Args[1] are JS_TAG_INT — no heap allocation, no Free needed
    Context^.Free(Func);
    Context^.Free(Global);

  finally
    // 9. Free context and runtime
    JS_FreeContext(Context);
    JS_FreeRuntime(Runtime);
  end;
end;
```

**Key Points:**
- **Memory Management**: Every `JSValue` referencing a heap object (strings, objects) must be freed via `Context^.Free(v)`. There is no standalone `JS_FreeValue` procedure; use the `TJSContext.Free` method instead.
- **Raw vs. Wrapper**: Most QuickJS C-API functions (`JS_Eval`, `JS_Call`, `JS_GetPropertyStr`, etc.) return/accept `JSValueRaw`. Assign to `JSValue` via the `.Raw` property. Type-checking helpers (`IsException`, `IsNumber`, `IsString`, `IsObject`) are methods on `JSValue`, not on `JSValueRaw`.
- **No JS_NewInt32**: There is no `JS_NewInt32()` helper. Create integer-tagged values using `JSValue.From32(val)` instead.
- **Checking Functions**: Use `JS_IsFunction(ctx, val.Raw)` (returns 0/1) to test whether a value is callable; `JSValue` has no `IsFunction` method.
- **Cleanup Order**: Free values before freeing context, and context before runtime.

### 22.3.3. Enabling QuickJS

QuickJS requires the `LIBQUICKJSSTATIC` conditional:

```pascal
// In project options or .inc file:
{$define LIBQUICKJSSTATIC}
```

Static object files are located under the `static/` folder, in platform-specific subdirectories:

```
static/
  x86_64-linux/quickjs.o    // Linux 64-bit (FPC)
  i386-linux/quickjs.o      // Linux 32-bit (FPC)
  aarch64-linux/quickjs.o   // Linux ARM64 (FPC)
  arm-linux/quickjs.o       // Linux ARM (FPC)
  x86_64-win64/quickjs.o    // Windows 64-bit (FPC)
  i386-win32/quickjs.o      // Windows 32-bit (FPC)
  delphi/quickjs.obj        // Windows 32-bit (Delphi)
  delphi/quickjs.o          // Windows 64-bit (Delphi)
```

---

## 22.4. Thread-Safe Engine Management

### 22.4.1. TThreadSafeManager

The `TThreadSafeManager` class provides thread-safe engine pooling:

> **Note:** `TThreadSafeEngine` is an abstract base class. You must implement your own concrete engine class (e.g., `TMyQuickJSEngine`) that inherits from it and provides the actual scripting functionality.

```pascal
uses
  mormot.script.core;

type
  // Your engine implementation (inherits from TThreadSafeEngine)
  // NOTE: This is user-implemented - not provided by mORMot
  TMyQuickJSEngine = class(TThreadSafeEngine)
  protected
    procedure AfterCreate; override;
    procedure DoBeginRequest; override;
    procedure DoEndRequest; override;
  end;

var
  Manager: TThreadSafeManager;
begin
  Manager := TThreadSafeManager.Create(TMyQuickJSEngine);
  try
    // Configure expiration (recommended for long-running servers)
    Manager.EngineExpireTimeOutMinutes := 240;  // 4 hours

    // Use in request handlers...
  finally
    Manager.Free;
  end;
end;
```

### 22.4.2. Per-Thread Engine Access

```pascal
procedure HandleRequest;
var
  Engine: TThreadSafeEngine;
begin
  // Get or create engine for current thread
  Engine := Manager.ThreadSafeEngine;

  // Execute within protected context
  Engine.ThreadSafeCall(
    procedure(E: TThreadSafeEngine)
    begin
      // FPU state is protected here
      // Execute JavaScript safely
    end);
end;
```

### 22.4.3. Thread Safety Model

```
┌─────────────────────────────────────────────────────────────────┐
│                    Thread Safety Model                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Thread 1 ──────► Engine 1 (exclusive)                          │
│                                                                 │
│  Thread 2 ──────► Engine 2 (exclusive)                          │
│                                                                 │
│  Thread 3 ──────► Engine 3 (exclusive)                          │
│                                                                 │
│                   ▲                                             │
│                   │                                             │
│  ┌────────────────┴────────────────┐                            │
│  │     TThreadSafeManager          │                            │
│  │  (thread-safe pool management)  │                            │
│  └─────────────────────────────────┘                            │
│                                                                 │
│  Key Rules:                                                     │
│  • One engine per thread (never shared)                         │
│  • Manager handles allocation/deallocation                      │
│  • Automatic expiration prevents memory leaks                   │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## 22.5. Hot Reload Pattern

### 22.5.1. Content Versioning

```pascal
// Application detects script changes
if ScriptFilesModified then
begin
  // Increment version - engines will recreate on next access
  Manager.ContentVersion := Manager.ContentVersion + 1;
end;

// In request handler
Engine := Manager.ThreadSafeEngine;
if Engine.ContentVersion <> Manager.ContentVersion then
begin
  // Engine was recreated - reload scripts
  LoadScriptsIntoEngine(Engine);
  Engine.ContentVersion := Manager.ContentVersion;
end;
```

### 22.5.2. Engine Expiration

For long-running servers, prevent JavaScript memory leaks:

```pascal
// Recreate engines every 4 hours
Manager.EngineExpireTimeOutMinutes := 240;

// Mark critical engines as permanent
MainEngine.NeverExpire := true;
```

---

## 22.6. Remote Debugging

### 22.6.1. Firefox DevTools Protocol

mORMot implements the Firefox Remote Debugging Protocol:

```pascal
// Start debugger on port 6000
Manager.StartDebugger('6000');

// Optional: Break on first line
Manager.PauseDebuggerOnFirstStep := true;
```

### 22.6.2. Connecting Firefox

1. Open Firefox
2. Navigate to `about:debugging`
3. Click "This Firefox" → "Settings"
4. Enable Remote Debugging
5. Connect to `localhost:6000`

**Note:** This uses Firefox DevTools protocol, NOT Chrome DevTools.

---

## 22.7. mORMot Integration Patterns

> **⚠️ NOTE**: The high-level APIs shown in sections 22.7-22.8 are **illustrative patterns**, not exact API signatures. The `mormot.script.quickjs` unit provides `TQuickJSEngine` with `Evaluate()`, `RegisterMethod()`, `Global` variant and `TQuickJSVariant` for late-binding. The specific method names in the examples below (e.g. `RegisterFunction`, `Engine.Call`) differ from the actual API names.
>
> For exact signatures, see `mormot.script.quickjs.pas`. For maximum control use the low-level QuickJS C API in `mormot.lib.quickjs.pas` (see Section 22.3).

### 22.7.1. Exposing ORM to JavaScript

```pascal
// Register native function to access ORM
procedure RegisterOrmAccess(Engine: TThreadSafeEngine);
begin
  // Example: Expose Customer retrieval
  Engine.RegisterFunction('getCustomer',
    function(Args: TJSArgs): TJSValue
    var
      ID: TID;
      Customer: TOrmCustomer;
    begin
      ID := Args[0].AsInt64;
      Customer := TOrmCustomer.Create;
      try
        if Server.Orm.Retrieve(ID, Customer) then
          Result := CustomerToJS(Customer)
        else
          Result := JS_NULL;
      finally
        Customer.Free;
      end;
    end);
end;
```

### 22.7.2. Calling Services from JavaScript

```pascal
// JavaScript can call interface-based services
Engine.RegisterFunction('callService',
  function(Args: TJSArgs): TJSValue
  var
    ServiceName, MethodName: RawUtf8;
    Params: TDocVariantData;
  begin
    ServiceName := Args[0].AsString;
    MethodName := Args[1].AsString;
    Params := Args[2].AsVariant;

    // Execute service and return result as JSON
    Result := ExecuteServiceMethod(ServiceName, MethodName, Params);
  end);
```

### 22.7.3. Business Rules in JavaScript

```pascal
// Define validation rules in JavaScript
const
  VALIDATION_SCRIPT = '''
    function validateOrder(order) {
      if (order.total > 10000 && !order.managerApproval) {
        return { valid: false, error: "Manager approval required" };
      }
      if (order.items.length === 0) {
        return { valid: false, error: "Order must have items" };
      }
      return { valid: true };
    }
  ''';

// Use from Delphi
function ValidateOrder(const Order: TOrder): boolean;
var
  Engine: TThreadSafeEngine;
  Result: Variant;
begin
  Engine := Manager.ThreadSafeEngine;
  Result := Engine.Call('validateOrder', [OrderToVariant(Order)]);
  ValidateOrder := Result.valid;
  if not ValidateOrder then
    raise EValidation.Create(Result.error);
end;
```

---

## 22.8. Custom Variant Type

### 22.8.1. Late-Binding Access

mORMot provides a custom variant type for seamless JavaScript access:

```pascal
var
  JSObj: Variant;
begin
  // Create JavaScript object
  JSObj := Engine.NewObject;

  // Late-binding property access (like JavaScript)
  JSObj.name := 'John';
  JSObj.age := 30;
  JSObj.greet := Engine.Function('return "Hello, " + this.name');

  // Call method
  Writeln(JSObj.greet());  // "Hello, John"
end;
```

### 22.8.2. Array Handling

```pascal
var
  JSArray: Variant;
begin
  JSArray := Engine.NewArray;

  // Push elements
  JSArray.push(1);
  JSArray.push(2);
  JSArray.push(3);

  // Access by index
  Writeln(JSArray[0]);  // 1

  // Array methods
  JSArray.sort();
  JSArray.reverse();
end;
```

---

## 22.9. Key Units Reference

| Unit | Purpose |
|------|---------|
| `mormot.script.core` | Abstract engine management, thread pooling |
| `mormot.script.quickjs` | QuickJS high-level wrapper (`TQuickJSEngine`, `TQuickJSVariant`) |
| `mormot.lib.quickjs` | Low-level QuickJS API bindings |
| `mormot.lib.static` | Static library loading infrastructure |

---

## 22.10. Current Status and Roadmap

### 22.10.1. Implemented

- ✓ Abstract `TThreadSafeEngine` and `TThreadSafeManager`
- ✓ Thread-safe pooling with expiration
- ✓ Remote debugging infrastructure
- ✓ Low-level QuickJS bindings (`mormot.lib.quickjs`)
- ✓ `TQuickJSEngine` high-level wrapper (`mormot.script.quickjs`)
- ✓ `TQuickJSVariant` custom variant type for late-binding JavaScript access

### 22.10.2. In Development

- ORM/SOA integration helpers

### 22.10.3. Planned

- SpiderMonkey integration (for high-performance server scenarios)
- JIT compilation support
- node.js module compatibility

---

## 22.11. Best Practices

### 22.11.1. When to Use Scripting

| Use Scripting For | Use Compiled Code For |
|-------------------|----------------------|
| User-customizable logic | Performance-critical paths |
| Frequently changing rules | Core business logic |
| Plugin/extension systems | Security-sensitive operations |
| Rapid prototyping | Database access layer |
| Report templates | Infrastructure code |

### 22.11.2. Security Considerations

> **⚠️ NOTE**: The security control properties shown below (`DisableFileAccess`, `MaxExecutionTimeMs`, etc.) are **conceptual** names not present in the current API.
>
> Use `TQuickJSEngine.TimeoutValue` (in seconds) for execution timeout control. QuickJS C API functions `JS_SetMaxStackSize()` and `JS_SetMemoryLimit()` are available via the low-level bindings for stack and memory limits.

```pascal
// Conceptual API - limit script capabilities
Engine.DisableFileAccess := true;
Engine.DisableNetworkAccess := true;
Engine.MaxExecutionTimeMs := 5000;  // 5 second timeout
Engine.MaxMemoryBytes := 64 * 1024 * 1024;  // 64MB limit
```

### 22.11.3. Error Handling

```pascal
try
  Result := Engine.Eval(Script);
except
  on E: EJSException do
  begin
    Log.Error('JavaScript error at line %d: %s',
              [E.LineNumber, E.Message]);
    // E.StackTrace contains full JS stack
  end;
end;
```

---

## 22.12. Summary

### 22.12.1. Quick Reference

| Need | Solution |
|------|----------|
| Embed JavaScript | `mormot.lib.quickjs` + `LIBQUICKJSSTATIC` |
| Thread-safe execution | `TThreadSafeManager` |
| Engine pooling | `Manager.ThreadSafeEngine` |
| Hot reload | `Manager.ContentVersion` |
| Remote debugging | `Manager.StartDebugger('6000')` |
| Memory management | `EngineExpireTimeOutMinutes` |

### 22.12.2. When to Choose Each Engine

| QuickJS | SpiderMonkey (when available) |
|---------|-------------------------------|
| Client-side scripts | Server-side heavy processing |
| Small footprint needed | JIT performance required |
| Static linking preferred | node.js compatibility needed |
| ES2020 sufficient | Latest ECMAScript features |

---

*Note: The scripting layer in mORMot2 is actively evolving. Check the source code and forum for the latest implementation status.*

*Next: Chapter 23 covers Asymmetric Encryption (ECC) for secure communications.*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 21: Security](mORMot2-SAD-Chapter-21.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 23: Asymmetric Encryption](mORMot2-SAD-Chapter-23.md) |
