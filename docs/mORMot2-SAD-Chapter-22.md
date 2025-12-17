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

The `mormot.lib.quickjs.pas` unit provides direct QuickJS bindings:

```pascal
uses
  mormot.lib.quickjs;

var
  Runtime: JSRuntime;
  Context: JSContext;
  Value: JSValue;
  Script: RawUtf8;
begin
  // Create runtime and context
  Runtime := JS_NewRuntime;
  Context := JS_NewContext(Runtime);
  try
    // Execute JavaScript
    Script := 'function add(a, b) { return a + b; } add(2, 3);';
    Value := JS_Eval(Context, pointer(Script), length(Script),
                     'script.js', JS_EVAL_TYPE_GLOBAL);

    // Check result
    if JS_IsNumber(Value) then
      Writeln('Result: ', JS_ToFloat64(Context, Value));

    JS_FreeValue(Context, Value);
  finally
    JS_FreeContext(Context);
    JS_FreeRuntime(Runtime);
  end;
end;
```

### 22.3.3. Enabling QuickJS

QuickJS requires the `LIBQUICKJSSTATIC` conditional:

```pascal
// In project options or .inc file:
{$define LIBQUICKJSSTATIC}
```

Static libraries are located in:
```
/mnt/w/mORMot2/static/
  libquickjs.a        // Linux/macOS
  libquickjs.obj      // Windows
```

---

## 22.4. Thread-Safe Engine Management

### 22.4.1. TThreadSafeManager

The `TThreadSafeManager` class provides thread-safe engine pooling:

```pascal
uses
  mormot.script.core;

type
  // Your engine implementation (inherits from TThreadSafeEngine)
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
┌────────────────────────────────────────────────────────────────┐
│                    Thread Safety Model                          │
├────────────────────────────────────────────────────────────────┤
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
└────────────────────────────────────────────────────────────────┘
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
| `mormot.script.quickjs` | QuickJS high-level wrapper (in development) |
| `mormot.lib.quickjs` | Low-level QuickJS API bindings |
| `mormot.lib.static` | Static library loading infrastructure |

---

## 22.10. Current Status and Roadmap

### 22.10.1. Implemented

- ✓ Abstract `TThreadSafeEngine` and `TThreadSafeManager`
- ✓ Thread-safe pooling with expiration
- ✓ Remote debugging infrastructure
- ✓ Low-level QuickJS bindings (`mormot.lib.quickjs`)

### 22.10.2. In Development

- `mormot.script.quickjs` high-level wrapper
- Custom variant type for late-binding
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

```pascal
// Limit script capabilities
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
