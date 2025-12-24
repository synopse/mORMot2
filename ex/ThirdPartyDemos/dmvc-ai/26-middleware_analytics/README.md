# 26-middleware_analytics - Request Analytics Middleware

**Port of**: DMVCFramework `samples/middleware_analytics`
**Difficulty**: Intermediate
**Demonstrates**: Request/response analytics logging to CSV, middleware patterns, metrics collection

## Overview

This sample demonstrates how to implement analytics middleware in mORMot2 that logs detailed request/response metrics to a CSV file for later analysis. It ports DMVC's `TMVCAnalyticsMiddleware` to mORMot2's event-based architecture.

## DMVC → mORMot2 Mapping

### Analytics Middleware

| DMVC Pattern | mORMot2 Equivalent |
|--------------|-------------------|
| `TMVCAnalyticsMiddleware` | `TAnalyticsMiddleware` class |
| `OnBeforeControllerAction` | `OnFilterRequest` event |
| `OnAfterRouting` | `OnAfterServiceMethod` event |
| `ILogWriter` (LoggerPro) | Direct file I/O with `AppendToFile()` |
| CSV log format | Same CSV format preserved |

### Key Concepts

**DMVC Middleware**:
```pascal
TMVCAnalyticsMiddleware = class(TInterfacedObject, IMVCMiddleware)
  procedure OnAfterRouting(AContext: TWebContext; const AHandled: Boolean);
  // Logs: IP;Method;Path;Status;Action;ContentLength;Host
end;

// Usage:
FMVC.AddMiddleware(TMVCAnalyticsMiddleware.Create(GetAnalyticsDefaultLogger));
```

**mORMot2 Equivalent**:
```pascal
TAnalyticsMiddleware = class
  function OnFilterRequest(Ctxt: TRestServerUriContext): Boolean;
  procedure OnAfterServiceMethod(Sender: TRestServer;
    const Ctxt: TRestServerUriContext);
end;

// Usage:
fAnalyticsMiddleware := TAnalyticsMiddleware.Create(fServer, logFileName);
```

## Implementation Details

### 1. Analytics Data Collection

**Port of**: DMVC `OnAfterRouting` method

The middleware collects these metrics for each request:
- **Timestamp**: `yyyy-mm-dd hh:nn:ss` format
- **Log Level**: info/warning/error (based on HTTP status code)
- **Client IP**: Remote client address
- **HTTP Method**: GET, POST, PUT, DELETE, etc.
- **Path**: Request URI
- **Status Code**: HTTP response status
- **Action**: Service class and method name
- **Content Length**: Response body size in bytes
- **Host**: Request host header

### 2. CSV Log Format

**Port of**: LoggerPro CSV format

```csv
timestamp;level;client_ip;method;path;status;action;content_length;host
2025-12-20 12:34:56;info;127.0.0.1;GET;/root/AnalyticsApi/Index;200;TAnalyticsApiService.0;12;localhost:8080
2025-12-20 12:34:57;error;127.0.0.1;GET;/root/AnalyticsApi/DoError;500;TAnalyticsApiService.2;45;localhost:8080
```

### 3. Log Level Mapping

**Port of**: DMVC `LOG_LEVEL` array

```pascal
case aStatusCode div 100 of
  1, 2, 3: level := 'info';    // Success/redirect
  4: level := 'warning';        // Client error
  5: level := 'error';          // Server error
end;
```

### 4. Thread-Safe Logging

Uses `AppendToFile()` which is thread-safe for concurrent request logging:

```pascal
procedure LogAnalytics(...);
begin
  logLine := FormatUtf8('%;%;%;%;%;%;%;%;%'#13#10,
    [timestamp, level, aClientIP, aMethod, aPath,
     aStatusCode, aAction, aContentLength, aHost]);

  AppendToFile(logLine, fLogFileName);  // Thread-safe
end;
```

## Usage

### Build and Run

```bash
cd /mnt/w/mORMot2/ex/dmvc/26-middleware_analytics
dcc32 26-middleware_analytics.dpr
26-middleware_analytics.exe
```

### Test Endpoints

**Normal Request**:
```bash
curl http://localhost:8080/root/AnalyticsApi/Index
# Returns: ["hello","world"]
# Logs: 2025-12-20 12:34:56;info;127.0.0.1;GET;/root/AnalyticsApi/Index;200;...
```

**String Reversal**:
```bash
curl "http://localhost:8080/root/AnalyticsApi/GetReversedString?aValue=hello"
# Returns: "olleh"
# Logs request analytics
```

**Error Trigger**:
```bash
curl http://localhost:8080/root/AnalyticsApi/DoError
# Returns HTTP 500 error
# Logs: 2025-12-20 12:34:57;error;127.0.0.1;GET;/root/AnalyticsApi/DoError;500;...
```

**CRUD Endpoints** (stub implementations):
```bash
curl http://localhost:8080/root/AnalyticsApi/GetCustomers
curl http://localhost:8080/root/AnalyticsApi/GetCustomer?aId=1
```

### Check Analytics Logs

Log files are created in the system log directory:
```bash
# Linux/WSL
cat ~/logs/analytics/2025-12-20.csv

# Windows
type %APPDATA%\analytics\2025-12-20.csv
```

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Logger** | LoggerPro with `ILogWriter` | Direct file I/O with `AppendToFile()` |
| **Middleware pattern** | `IMVCMiddleware` interface | Event-based callbacks |
| **Context data** | `AContext.Data.Items['fqaction']` | Extract from `Ctxt.Service` directly |
| **Log rotation** | LoggerPro handles automatically | Manual (date-based filename) |
| **Action name** | `ControllerClass.ActionName` | `ServiceClass.MethodIndex` |
| **Thread safety** | LoggerPro queue | `AppendToFile()` thread-safe |

## Advanced Features

### Custom Analytics Fields

Extend the middleware to log custom data:

```pascal
procedure TAnalyticsMiddleware.OnAfterServiceMethod(...);
var
  userAgent: RawUtf8;
  referer: RawUtf8;
begin
  // Extract custom headers
  FindNameValue(Ctxt.InHead, 'USER-AGENT:', userAgent);
  FindNameValue(Ctxt.InHead, 'REFERER:', referer);

  // Log extended data
  logLine := FormatUtf8('%;%;%;%;%;%;%;%;%;%;%',
    [timestamp, level, clientIP, method, path,
     statusCode, action, contentLength, host,
     userAgent, referer]);  // Extended
end;
```

### Analytics Aggregation

Process the CSV logs to generate insights:

```bash
# Count requests by status code
awk -F';' '{print $6}' analytics.csv | sort | uniq -c

# Average response size
awk -F';' '{sum+=$8; count++} END {print sum/count}' analytics.csv

# Requests per hour
awk -F';' '{print substr($1,12,2)}' analytics.csv | sort | uniq -c
```

### Real-Time Analytics

Combine with `TRequestLoggerMiddleware` for real-time display:

```pascal
constructor TAnalyticsServer.Create(const aPort: RawUtf8);
begin
  // ...
  fAnalyticsMiddleware := TAnalyticsMiddleware.Create(fServer, logPath);
  fRequestLoggerMiddleware := TRequestLoggerMiddleware.Create(fServer);
  // Both middleware work together
end;
```

## Performance Considerations

- **File I/O**: Each request writes to disk (async I/O recommended for high traffic)
- **CSV parsing**: Use tools like `awk`, `cut`, or import into SQLite for analysis
- **Log rotation**: Implement daily rotation by including date in filename
- **Compression**: Compress old log files (`.csv.gz`) to save disk space

## Use Cases

1. **Traffic Analysis**: Monitor request patterns, popular endpoints
2. **Performance Metrics**: Track response sizes, identify slow endpoints
3. **Error Monitoring**: Alert on high error rates (4xx/5xx)
4. **User Analytics**: Track user behavior, A/B testing results
5. **Security Auditing**: Log suspicious access patterns
6. **Compliance**: Maintain audit trails for regulatory requirements

## See Also

- [12-middleware](../12-middleware/) - Basic middleware patterns
- [27-middleware_trace](../27-middleware_trace/) - Request tracing middleware
- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#6-middleware-equivalents)

## References

- **DMVC Source**: `/mnt/w/DMVCframework/samples/middleware_analytics/`
- **mORMot2 Events**: `mormot.rest.server.pas` → `TRestServer` class
- **File I/O**: `mormot.core.os.pas` → `AppendToFile()` function
