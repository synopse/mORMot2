# 27-middleware_trace - Request/Response Tracing Middleware

**Port of**: DMVCFramework `samples/middleware_trace`
**Difficulty**: Intermediate
**Demonstrates**: Detailed request/response tracing, debugging middleware, request lifecycle logging

## Overview

This sample demonstrates how to implement trace middleware in mORMot2 that logs detailed information about every request and response. It's invaluable for debugging, monitoring, and understanding request flow in production systems.

## DMVC → mORMot2 Mapping

### Trace Middleware

| DMVC Pattern | mORMot2 Equivalent |
|--------------|-------------------|
| `TMVCTraceMiddleware` | `TTraceMiddleware` class |
| `OnBeforeRouting` | `OnBeforeUri` event |
| `OnBeforeControllerAction` | Logged in `OnAfterUri` |
| `OnAfterControllerAction` | `OnAfterUri` event |
| `fMaxBodySize` parameter | Same (configurable max body preview) |

### Key Concepts

**DMVC Middleware**:
```pascal
TMVCTraceMiddleware = class(TInterfacedObject, IMVCMiddleware)
  constructor Create(const MaxBodySizeInTrace: UInt64 = 1024);
  procedure OnBeforeRouting(...);
  procedure OnAfterControllerAction(...);
end;

// Usage:
FMVC.AddMiddleware(TMVCTraceMiddleware.Create);
```

**mORMot2 Equivalent**:
```pascal
TTraceMiddleware = class
  constructor Create(aServer: TRestServer; aMaxBodySize: Integer = 1024);
  function OnBeforeUri(Ctxt: TRestServerUriContext): Boolean;
  procedure OnAfterUri(Ctxt: TRestServerUriContext);
end;

// Usage:
fTraceMiddleware := TTraceMiddleware.Create(fServer, 1024);
```

## Implementation Details

### 1. Request Tracing (Before)

**Port of**: DMVC `OnBeforeRouting`

Logs these request details:
- **HTTP Method**: GET, POST, PUT, DELETE, etc.
- **Client IP**: Remote address
- **URL**: Full request path
- **Query String**: URL parameters
- **Content Length**: Request body size
- **Accept Header**: Client's accepted content types
- **User-Agent**: Client application/browser
- **Authorization**: Presence of auth header (not the value!)
- **Request Body**: Preview for POST/PUT (respects MaxBodySize)

**Example Log Output**:
```
[BEFORE ROUTING][GET][IP: 127.0.0.1][URL: /root/TraceApi/GetCustomers]
[LENGTH: 0][ACCEPT: application/json][USER-AGENT: curl/7.68.0][AUTHORIZATION: Bearer]
```

### 2. Action Tracing

**Port of**: DMVC `OnBeforeControllerAction`

Logs the controller and action being executed:

```
[BEFORE ACTION][CONTROLLER: TTraceApiService][ACTION: TTraceApiService.Method0]
```

### 3. Response Tracing (After)

**Port of**: DMVC `OnAfterControllerAction`

Logs these response details:
- **Status Code**: HTTP status (200, 404, 500, etc.)
- **Status Text**: Human-readable status
- **Custom Headers**: All response headers
- **Content-Type**: Response content type
- **Response Body**: Preview (respects MaxBodySize)

**Example Log Output**:
```
[AFTER ACTION][RESPONSE][STATUS] 200: OK
[AFTER ACTION][RESPONSE][CUSTOM HEADERS] Content-Type: application/json
[AFTER ACTION][RESPONSE][CONTENT-TYPE] application/json
[AFTER ACTION][RESPONSE][BODY] {"customers":[{"id":1,"name":"Customer 1"}]}
```

### 4. Body Preview Logic

**Port of**: DMVC content extraction

```pascal
function ExtractContentPreview(const aContent: RawByteString;
  const aContentType: RawUtf8): RawUtf8;
begin
  // Text content: Show preview up to MaxBodySize
  if IsTextContent(aContentType) then
    Result := Copy(aContent, 1, fMaxBodySize) + '...'
  else
    // Binary content: Show size only
    Result := '<binary content: NNN bytes>';
end;
```

Recognizes these text types:
- `application/json`
- `application/xml`
- `application/x-www-form-urlencoded`
- `text/*` (text/html, text/plain, etc.)

## Usage

### Build and Run

```bash
cd /mnt/w/mORMot2/ex/dmvc/27-middleware_trace
dcc32 27-middleware_trace.dpr
27-middleware_trace.exe
```

### Test Endpoints

**Normal GET Request**:
```bash
curl http://localhost:8080/root/TraceApi/Index
# Logs: Full request trace + response trace
```

**GET with Parameters**:
```bash
curl "http://localhost:8080/root/TraceApi/GetReversedString?aValue=hello"
# Returns: "olleh"
# Logs: Query parameters in trace
```

**POST Request** (to see body tracing):
```bash
curl -X POST http://localhost:8080/root/TraceApi/CreateCustomer \
  -H "Content-Type: application/json" \
  -d '{"name":"John Doe","email":"john@example.com"}'
# Logs: Request body preview + response
```

**Error Scenario**:
```bash
curl http://localhost:8080/root/TraceApi/DoError
# Returns: HTTP 500
# Logs: Complete error trace
```

### Check Trace Logs

Traces are written to mORMot2 log file:

```bash
# Linux/WSL
cat 27-middleware_trace.log | grep trace

# Windows
type 27-middleware_trace.log | findstr trace
```

**Example Trace Session**:
```
20251220 12:34:56.123 trace [BEFORE ROUTING][GET][IP: 127.0.0.1][URL: /root/TraceApi/Index]...
20251220 12:34:56.124 trace [BEFORE ACTION][CONTROLLER: TTraceApiService][ACTION: TTraceApiService.Method0]
20251220 12:34:56.125 trace [AFTER ACTION][RESPONSE][STATUS] 200: OK
20251220 12:34:56.126 trace [AFTER ACTION][RESPONSE][BODY] {"message":"Hello from trace..."}
```

## Configuration

### Adjust Max Body Size

Control how much of request/response bodies are logged:

```pascal
// Default: 1024 bytes
fTraceMiddleware := TTraceMiddleware.Create(fServer, 1024);

// Large bodies (e.g., file uploads): Show less
fTraceMiddleware := TTraceMiddleware.Create(fServer, 256);

// Small API responses: Show more
fTraceMiddleware := TTraceMiddleware.Create(fServer, 4096);

// Unlimited (dangerous in production!)
fTraceMiddleware := TTraceMiddleware.Create(fServer, MaxInt);
```

### Filter Sensitive Data

Modify `ExtractContentPreview()` to redact sensitive information:

```pascal
function TTraceMiddleware.ExtractContentPreview(...): RawUtf8;
var
  preview: RawUtf8;
begin
  preview := Copy(aContent, 1, fMaxBodySize);

  // Redact password fields
  preview := StringReplaceAll(preview, '"password":"', '"password":"***');

  // Redact credit cards (simple regex)
  preview := ReplaceRegex(preview, '\d{4}-\d{4}-\d{4}-\d{4}', 'XXXX-XXXX-XXXX-XXXX');

  Result := preview;
end;
```

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Log output** | `Log.Debug()` (LoggerPro) | `TSynLog.Add.Log()` (TSynLog) |
| **Context object** | `TWebContext` | `TRestServerUriContext` record |
| **Request body** | `Context.Request.RawWebRequest.RawContent` | `aInBody` parameter |
| **Response body** | `AContext.Response.RawWebResponse.Content` | `Ctxt.OutBody` |
| **Headers** | `Context.Request.Headers[]` | Parse `Ctxt.InHead` with `FindNameValue()` |
| **Action name** | `AControllerQualifiedClassName.AActionName` | `Service.ClassName.MethodIndex` |

## Advanced Features

### Conditional Tracing

Only trace specific paths or methods:

```pascal
function TTraceMiddleware.OnBeforeUri(...): Boolean;
begin
  // Only trace API endpoints
  if PosEx('/api/', aInUrl) = 0 then
    Exit(False); // Skip tracing

  // Normal trace logic
  LogRequestDetails(...);
  Result := False;
end;
```

### Performance Timing

Add timing information to traces:

```pascal
type
  TTraceMiddleware = class
  private
    fRequestStartTime: TDateTime;
  end;

function TTraceMiddleware.OnBeforeUri(...): Boolean;
begin
  fRequestStartTime := NowUtc; // Start timer
  // ... trace logic
end;

procedure TTraceMiddleware.OnAfterServiceMethod(...);
var
  duration: Int64;
begin
  duration := MilliSecondsBetween(NowUtc, fRequestStartTime);
  TSynLog.Add.Log(sllTrace, '[TIMING] Request took % ms', [duration], 'trace');
  // ... rest of trace logic
end;
```

### Trace Filtering by Log Level

Control trace verbosity via log level:

```pascal
// In server setup:
TSynLog.Family.Level := LOG_VERBOSE;  // Show all traces

// Production:
TSynLog.Family.Level := [sllInfo, sllWarning, sllError];  // Hide traces

// Development:
TSynLog.Family.Level := [sllTrace, sllDebug, sllInfo, sllWarning, sllError];
```

## Use Cases

1. **API Debugging**: See exact request/response for failed API calls
2. **Integration Testing**: Verify request/response format matches spec
3. **Performance Analysis**: Combined with timing, identify slow endpoints
4. **Security Auditing**: Log all API access for compliance
5. **Client Troubleshooting**: Reproduce client issues by examining traces
6. **API Documentation**: Use traces to generate API examples

## Production Considerations

⚠️ **Warning**: Trace middleware can generate LARGE log files!

**Best Practices**:
- ✅ **Use small `MaxBodySize`** (256-1024 bytes) in production
- ✅ **Filter sensitive data** (passwords, tokens, PII)
- ✅ **Enable only for debug sessions** (controlled via config)
- ✅ **Rotate log files frequently** (daily or by size)
- ✅ **Consider performance impact** (~1-5% overhead)
- ❌ **Don't trace in high-traffic production** (use sampling instead)

**Sampling Example**:
```pascal
function TTraceMiddleware.OnBeforeUri(...): Boolean;
begin
  // Only trace 1% of requests
  if Random(100) > 1 then
    Exit(False);

  // Normal trace logic for sampled request
  LogRequestDetails(...);
end;
```

## See Also

- [26-middleware_analytics](../26-middleware_analytics/) - Analytics logging to CSV
- [12-middleware](../12-middleware/) - Basic middleware patterns
- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#6-middleware-equivalents)

## References

- **DMVC Source**: `/mnt/w/DMVCframework/samples/middleware_trace/`
- **mORMot2 Logging**: `mormot.core.log.pas` → `TSynLog` class
- **Documentation**: https://synopse.info/fossil/wiki/Synopse+mORMot+Framework
