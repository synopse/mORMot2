# 12-middleware - Middleware Patterns in mORMot2

**Port of**: DMVCFramework `samples/middleware`
**Difficulty**: Intermediate
**Demonstrates**: Request/response hooks, custom headers, user-agent redirection, request logging

## Overview

This sample demonstrates how to implement middleware patterns in mORMot2, porting DMVC's `IMVCMiddleware` interface to mORMot2's event-based hooks.

## DMVC → mORMot2 Mapping

### Middleware Architecture

| DMVC Pattern | mORMot2 Equivalent |
|--------------|-------------------|
| `IMVCMiddleware` interface | Event callbacks on `TRestServer` |
| `OnBeforeRouting` | `OnBeforeUri` event |
| `OnBeforeControllerAction` | `OnFilterRequest` event |
| `OnAfterControllerAction` | `OnAfterServiceMethod` event |
| `OnAfterRouting` | `OnFilterResponse` event |
| `AddMiddleware()` chain | Set event handlers on server instance |

### Key Concepts

**DMVC Middleware**:
```pascal
type
  IMVCMiddleware = interface
    procedure OnBeforeRouting(Context: TWebContext; var Handled: Boolean);
    procedure OnBeforeControllerAction(Context: TWebContext; ...);
    procedure OnAfterControllerAction(Context: TWebContext; ...);
    procedure OnAfterRouting(Context: TWebContext; const AHandled: Boolean);
  end;

// Usage
fEngine.AddMiddleware(TMVCSalutationMiddleware.Create);
```

**mORMot2 Equivalent**:
```pascal
// Event-based hooks
TRestServer.OnBeforeUri := function(...): Boolean;
TRestServer.OnFilterRequest := function(...): Boolean;
TRestServer.OnAfterServiceMethod := procedure(...);
TRestServer.OnFilterResponse := procedure(...);

// Middleware classes set these events in their constructor
```

## Implementation Details

### 1. Custom Header Middleware (`TCustomHeaderMiddleware`)

**Port of**: `TMVCSalutationMiddleware`

**DMVC**:
```pascal
procedure TMVCSalutationMiddleware.OnAfterControllerAction(...);
begin
  Context.Response.CustomHeaders.Values['X-PROUD-HEADER'] := 'Proudly served by...';
end;
```

**mORMot2**:
```pascal
procedure TCustomHeaderMiddleware.OnAfterServiceMethod(
  Sender: TRestServer; const Ctxt: TRestServerUriContext);
begin
  Ctxt.OutHead := Ctxt.OutHead + #13#10 +
    'X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)';
end;
```

### 2. User-Agent Redirect Middleware (`TUserAgentRedirectMiddleware`)

**Port of**: `TMVCRedirectAndroidDeviceOnPlayStore`

**DMVC**:
```pascal
procedure TMVCRedirectAndroidDeviceOnPlayStore.OnBeforeRouting(...);
begin
  if Context.Request.Headers['User-Agent'].Contains('Android') then
  begin
    Context.Response.Location := 'https://play.google.com';
    Context.Response.StatusCode := HTTP_STATUS.TemporaryRedirect;
    Handled := True;  // Stop processing
  end;
end;
```

**mORMot2**:
```pascal
function TUserAgentRedirectMiddleware.OnBeforeUri(...): Boolean;
var
  userAgent: RawUtf8;
begin
  FindNameValue(aInHeaders, 'USER-AGENT:', userAgent);
  if PosEx('Android', userAgent) > 0 then
  begin
    aStatus := HTTP_TEMPORARYREDIRECT;
    aOutHeaders := 'Location: https://play.google.com';
    Result := True;  // Stop processing
  end
  else
    Result := False;  // Continue
end;
```

### 3. Request Logging Middleware (`TRequestLoggerMiddleware`)

**New functionality** (demonstrates before/after hooks)

```pascal
// OnFilterRequest - called BEFORE service execution
function OnFilterRequest(...): Boolean;
begin
  Inc(fRequestCount);
  TSynLog.Add.Log(sllInfo, 'REQUEST #% [%] % %',
    [fRequestCount, NowToString, Method, Uri]);
  Result := False;  // Continue processing
end;

// OnFilterResponse - called AFTER service execution
procedure OnFilterResponse(...);
begin
  TSynLog.Add.Log(sllInfo, 'RESPONSE #% - Status: % - Body: % bytes',
    [fRequestCount, OutStatus, Length(OutBody)]);
end;
```

## Event Execution Order

When a request arrives:

```
1. OnBeforeUri          → Can short-circuit (return True)
2. OnFilterRequest      → Can short-circuit (return True)
3. Service method       → Business logic
4. OnAfterServiceMethod → Post-processing
5. OnFilterResponse     → Final logging
```

## Middleware Chain

The sample demonstrates this chain:

```
Request → Logger (before) → UserAgent Check → Service → CustomHeaders → Logger (after) → Response
```

Middleware order matters:
- **Before hooks**: First registered = First executed
- **After hooks**: Last registered = Last executed (reverse)

## Usage

### Build and Run

```bash
cd /mnt/w/mORMot2/ex/dmvc/12-middleware
dcc32 12-middleware.dpr
12-middleware.exe
```

### Test Endpoints

**Normal Request**:
```bash
curl http://localhost:8080/root/MiddlewareApi/Index
# Returns: ************************... (1024 chars)
# Headers include: X-Powered-By: mORMot2
```

**Show Headers**:
```bash
curl http://localhost:8080/root/MiddlewareApi/ShowHeaders
# Returns: Request Headers: <header dump>
```

**Echo Test**:
```bash
curl http://localhost:8080/root/MiddlewareApi/Echo?msg=test
# Returns: Echo: test
```

**Android Redirect**:
```bash
curl -H "User-Agent: Mozilla/5.0 (Linux; Android 10)" \
  http://localhost:8080/root/MiddlewareApi/Index
# HTTP 307 Temporary Redirect
# Location: https://play.google.com
```

### Check Logs

Logs are written to `12-middleware.log`:

```
20251220 12:34:56.789 +    info  REQUEST #1 [2025-12-20 12:34:56] GET /root/MiddlewareApi/Index
20251220 12:34:56.790 +   trace  CustomHeaderMiddleware: Added X-Powered-By header
20251220 12:34:56.791 +    info  RESPONSE #1 - Status: 200 - Body: 1024 bytes - Duration: 2 ms
```

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Pattern** | Interface-based (`IMVCMiddleware`) | Event-based callbacks |
| **Registration** | `AddMiddleware()` chain | Set event handlers directly |
| **Context** | `TWebContext` object | `TRestServerUriContext` record |
| **Headers** | `Context.Request.Headers[]` | Parse `InHead` with `FindNameValue()` |
| **Response** | `Context.Response.StatusCode` | `Ctxt.OutStatus` / `aStatus` parameter |
| **Short-circuit** | `var Handled: Boolean` | Return `True` from function |
| **Middleware state** | Instance fields | Instance fields (same) |

## Advanced Features

### Conditional Middleware

```pascal
function OnFilterRequest(...): Boolean;
begin
  // Apply middleware only to specific paths
  if PosEx('/api/', Request.Uri) > 0 then
  begin
    // Middleware logic here
  end;
  Result := False;
end;
```

### Stateful Middleware

```pascal
type
  TRateLimitMiddleware = class
  private
    fRequestCounts: TSynDictionary;  // IP -> count
  public
    function OnFilterRequest(...): Boolean;
  end;

function TRateLimitMiddleware.OnFilterRequest(...): Boolean;
var
  count: Integer;
begin
  count := fRequestCounts.IncrementValue(aRemoteIP);
  if count > 100 then  // Rate limit
  begin
    Request.Error('Too many requests', HTTP_TOOMANYREQUESTS);
    Result := True;  // Block request
  end
  else
    Result := False;
end;
```

### Custom Middleware Events

mORMot2 provides many more events than DMVC:

```pascal
OnBeforeUri         // Earliest hook, before routing
OnFilterRequest     // Before service method
OnBeforeBody        // Before reading request body
OnAfterServiceMethod // After service method
OnFilterResponse    // After response prepared
OnHttpThreadStart   // HTTP thread initialization
OnHttpThreadTerminate // HTTP thread cleanup
```

## See Also

- [08-basicauth](../08-basicauth/) - Authentication middleware
- [09-custom_auth](../09-custom_auth/) - Custom authentication handlers
- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#6-middleware-equivalents)

## References

- **DMVC Source**: `/mnt/w/DMVCframework/samples/middleware/`
- **mORMot2 Events**: `mormot.rest.server.pas` → `TRestServer` class
- **Documentation**: https://synopse.info/fossil/wiki/Synopse+mORMot+Framework
