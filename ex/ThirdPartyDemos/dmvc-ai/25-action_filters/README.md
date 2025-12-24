# mORMot2 Action Filters Sample

Port of DelphiMVCFramework's `action_filters` sample to mORMot2.

## Overview

This sample demonstrates **service-level action filters** - pre and post-processing hooks that intercept service method execution. It's the mORMot2 equivalent of DMVC's `OnBeforeAction` and `OnAfterAction` controller methods.

**REFACTORED (v2)**: Now uses **per-service hooks** (`OnMethodExecute` + `AddInterceptor`) instead of global server hooks. This is more idiomatic and provides better isolation.

## Key Concepts Demonstrated

### 1. **OnMethodExecute Filter** (Authorization/Validation)
- **DMVC**: `TMVCController.OnBeforeAction`
- **mORMot2**: `TServiceFactoryServer.OnMethodExecute`
- Validates requests before method execution
- Can block requests (returns `False` to deny)
- Example: Weekend blocking validation
- **Benefit**: Service-specific, no global state pollution

### 2. **AddInterceptor Hook** (Before/After Logging)
- **DMVC**: `TMVCController.OnAfterAction`
- **mORMot2**: `TServiceFactoryServer.AddInterceptor`
- Called before (`smsBefore`) and after (`smsAfter`) method execution
- Logs request details and execution time
- Also handles errors (`smsError`)
- **Benefit**: Multiple interceptors can be registered, composable

### 3. **Service Lifecycle Hooks**
- **DMVC**: `MVCControllerAfterCreate` / `MVCControllerBeforeDestroy`
- **mORMot2**: Constructor / Destructor of middleware class
- Logs service filter creation/destruction

### 4. **Per-Service Isolation**
- Filters are registered per service (not globally)
- Multiple services can have different filter policies
- No risk of one service's filters affecting another

## Architecture

```
Service-Level Action Filters Flow:

1. Request arrives at TRestServer
2. Routing to TServiceFactoryServer (IActionFiltersApi)
3. OnMethodExecute (TActionFilterMiddleware.OnMethodExecute)
   ├─> Validates request (e.g., check if weekend)
   ├─> If invalid: return False, block request
   └─> If valid: return True, continue to step 4
4. Interceptor BEFORE (smsBefore)
   └─> Log method execution starting
5. Service method executes (TActionFiltersApiService.GetPerson)
6. Interceptor AFTER (smsAfter)
   └─> Log execution time, success/failure
7. Response sent to client
```

## DMVC vs mORMot2 Mapping

| DMVC | mORMot2 (v1 - Global) | mORMot2 (v2 - Per-Service) | Purpose |
|------|----------------------|---------------------------|---------|
| `TMVCController.OnBeforeAction` | `TRestServer.OnBeforeUri` | `TServiceFactoryServer.OnMethodExecute` | Pre-processing validation |
| `TMVCController.OnAfterAction` | `TRestServer.OnAfterUri` | `TServiceFactoryServer.AddInterceptor` | Post-processing logging |
| `MVCControllerAfterCreate` | Middleware constructor | Middleware constructor | Lifecycle initialization |
| `MVCControllerBeforeDestroy` | Middleware destructor | Middleware destructor | Lifecycle cleanup |
| `TActionFiltersController.GetPerson` | `IActionFiltersApi.GetPerson` | `IActionFiltersApi.GetPerson` | Service method |

## Files

- **ActionFilters.dpr**: Main program (server initialization)
- **src/server.pas**: Server setup with action filter middleware
- **src/action.filters.pas**: Action filter implementation (before/after hooks)
- **src/api.interfaces.pas**: Service interface definition
- **src/api.impl.pas**: Service implementation (GetPerson method)
- **BusinessObjectsU.pas**: Domain model (TPerson class)

## Running the Sample

```bash
# Compile
dcc32 ActionFilters.dpr

# Run
ActionFilters.exe
```

Server starts on `http://localhost:8080`.

## Testing

### Basic Request (Weekday)
```bash
curl http://localhost:8080/root/ActionFiltersApi/Person/123
```

**Response** (weekday):
```json
{
  "FirstName": "Daniele",
  "LastName": "Teti",
  "DOB": "1975-05-02T00:00:00",
  "Married": true
}
```

### Weekend Blocking Test
```bash
curl http://localhost:8080/root/ActionFiltersApi/Person/123
```

**Response** (weekend):
```
HTTP/1.1 403 Forbidden
Content-Type: text/plain

You cannot use this service in the WeekEnd
```

## Expected Log Output

```
ActionFilterMiddleware created (equivalent to MVCControllerAfterCreate)
OnBeforeAction: Validating action /root/ActionFiltersApi/Person/123 from 127.0.0.1
GetPerson called with id=123
GetPerson returning JSON: {"FirstName":"Daniele",...}
OnAfterAction: ACTION CALLED: mGET mapped to /root/ActionFiltersApi/Person/123 from 127.0.0.1 (took 5 ms)
```

## Benefits of Service-Level Filters (v2 Refactoring)

### Why Use `OnMethodExecute` + `AddInterceptor`?

| Aspect | Global Hooks (v1) | Service-Level (v2) | Winner |
|--------|------------------|-------------------|--------|
| **Scope** | Affects ALL services | Only target service | ✅ Service-level |
| **Isolation** | Shared state pollution | No interference | ✅ Service-level |
| **Composition** | Single handler | Multiple interceptors | ✅ Service-level |
| **Testing** | Hard (global setup) | Easy (isolated mocking) | ✅ Service-level |
| **Flexibility** | One policy for all | Different per service | ✅ Service-level |
| **Idiomatic** | Not recommended | mORMot2 best practice | ✅ Service-level |

### Example: Multiple Services with Different Policies

```pascal
// Service 1: Strict weekend blocking
Factory1 := Server.Services.Info(TypeInfo(IPublicApi)) as TServiceFactoryServer;
Factory1.OnMethodExecute := BlockWeekends;

// Service 2: No restrictions, just logging
Factory2 := Server.Services.Info(TypeInfo(IAdminApi)) as TServiceFactoryServer;
Factory2.AddInterceptor(LogOnly);

// Service 3: Both validation + metrics
Factory3 := Server.Services.Info(TypeInfo(IPaymentApi)) as TServiceFactoryServer;
Factory3.OnMethodExecute := ValidatePayment;
Factory3.AddInterceptor(LogPayment);
Factory3.AddInterceptor(CollectMetrics);
```

### When to Use Each Approach

| Use Case | Recommended Hook |
|----------|-----------------|
| **Authorization** (allow/deny) | `OnMethodExecute` (can return False) |
| **Logging** (before/after) | `AddInterceptor` (smsBefore/smsAfter) |
| **Metrics collection** | `AddInterceptor` (composable) |
| **Rate limiting** | `OnMethodExecute` (early exit) |
| **Input validation** | `OnMethodExecute` (block invalid) |
| **Response transformation** | `AddInterceptor` (smsAfter) |

## Key Differences from DMVC

### 1. **Event Hook Registration**
**DMVC**: Override virtual methods in controller
```pascal
procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean); override;
```

**mORMot2 (v2)**: Assign per-service event handlers
```pascal
Factory.OnMethodExecute := OnMethodExecute;
Factory.AddInterceptor(OnInterceptMethod);
```

### 2. **Request Blocking**
**DMVC**: Set `Handled := True` or raise exception
```pascal
var Handled: Boolean;
if Invalid then
  Handled := True; // or raise Exception
```

**mORMot2 (v2)**: Return `False` from `OnMethodExecute`
```pascal
function OnMethodExecute(Ctxt: TRestServerUriContext;
  const Method: TInterfaceMethod): Boolean;
begin
  if Invalid then
  begin
    Ctxt.Error('Error message', HTTP_FORBIDDEN);
    Result := False; // Block request
  end;
end;
```

### 3. **Context Information**
**DMVC**: `TWebContext` parameter
```pascal
Context.Request.PathInfo
Context.Request.ClientIP
```

**mORMot2 (v2)**: `TRestServerUriContext` + `TInterfaceMethod`
```pascal
// OnMethodExecute
Ctxt.Call^.Url
Ctxt.Call^.LowLevelRemoteIP
Method.InterfaceDotMethodName

// AddInterceptor
Sender.Method.InterfaceDotMethodName
Sender.MicroSecondsElapsed
Sender.LastException
```

## Educational Value

This sample teaches:

1. ✅ **Pre/post action filters** - How to intercept method execution
2. ✅ **Request validation** - Business logic validation before processing
3. ✅ **Lifecycle hooks** - Controller creation/destruction events
4. ✅ **Action logging** - Tracking request execution details
5. ✅ **Error handling** - Converting exceptions to HTTP responses

## Related Samples

- **12-middleware**: General middleware pattern (request/response pipeline)
- **08-basicauth**: Authentication filters (pre-processing)
- **09-custom_auth**: Custom authorization filters

## Original DMVC Sample

Source: `DMVCFramework/samples/action_filters/`

Key differences:
- DMVC uses controller inheritance with virtual method overrides
- mORMot2 uses event-based middleware with callback assignments
- Both achieve the same pre/post processing functionality
