# Testing Guide - Middleware Sample

## Quick Test

```bash
# Terminal 1: Start server
cd /mnt/w/mORMot2/ex/dmvc/12-middleware
12-middleware.exe

# Terminal 2: Run tests
curl http://localhost:8080/root/MiddlewareApi/Index
```

## Test Cases

### 1. Basic Request with Custom Headers

**Test**: Verify custom header middleware

```bash
curl -i http://localhost:8080/root/MiddlewareApi/Index
```

**Expected**:
```
HTTP/1.1 200 OK
Content-Type: application/json
X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)

************************... (1024 * characters)
```

**Verify**:
- ✅ Status 200
- ✅ `X-Powered-By` header present
- ✅ 1024 bytes of `*` characters

### 2. Android User-Agent Redirect

**Test**: Verify user-agent middleware redirects Android devices

```bash
curl -i -H "User-Agent: Mozilla/5.0 (Linux; Android 10)" \
  http://localhost:8080/root/MiddlewareApi/Index
```

**Expected**:
```
HTTP/1.1 307 Temporary Redirect
Location: https://play.google.com
```

**Verify**:
- ✅ Status 307 (not 200)
- ✅ `Location` header points to Play Store
- ✅ No body content (request stopped by middleware)

### 3. Non-Android User-Agent (Normal Flow)

**Test**: Verify other user-agents pass through

```bash
curl -i -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64)" \
  http://localhost:8080/root/MiddlewareApi/Index
```

**Expected**:
```
HTTP/1.1 200 OK
X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)

************************...
```

**Verify**:
- ✅ Status 200 (middleware passed through)
- ✅ Custom headers added
- ✅ Full response body

### 4. Request Logging

**Test**: Verify all requests are logged

```bash
# Make multiple requests
curl http://localhost:8080/root/MiddlewareApi/Index
curl http://localhost:8080/root/MiddlewareApi/Echo?msg=test1
curl http://localhost:8080/root/MiddlewareApi/Echo?msg=test2
```

**Check**: `12-middleware.log` file

**Expected log entries**:
```
info  REQUEST #1 [2025-12-20 12:34:56] GET /root/MiddlewareApi/Index
trace CustomHeaderMiddleware: Added X-Powered-By header
info  RESPONSE #1 - Status: 200 - Body: 1024 bytes - Duration: 2 ms

info  REQUEST #2 [2025-12-20 12:34:57] GET /root/MiddlewareApi/Echo?msg=test1
trace Echo: msg=test1
info  RESPONSE #2 - Status: 200 - Body: 12 bytes - Duration: 1 ms

info  REQUEST #3 [2025-12-20 12:34:58] GET /root/MiddlewareApi/Echo?msg=test2
trace Echo: msg=test2
info  RESPONSE #3 - Status: 200 - Body: 12 bytes - Duration: 1 ms
```

**Verify**:
- ✅ Each request logged with sequential counter
- ✅ Method and URI captured
- ✅ Response status and body size logged
- ✅ Request duration calculated

### 5. Show Headers Endpoint

**Test**: Verify middleware can inspect request headers

```bash
curl -i -H "X-Custom-Header: TestValue" \
  -H "Authorization: Bearer abc123" \
  http://localhost:8080/root/MiddlewareApi/ShowHeaders
```

**Expected**:
```
HTTP/1.1 200 OK
X-Powered-By: mORMot2 (https://github.com/synopse/mORMot2)

Request Headers:
Host: localhost:8080
User-Agent: curl/7.68.0
Accept: */*
X-Custom-Header: TestValue
Authorization: Bearer abc123
```

**Verify**:
- ✅ All sent headers visible
- ✅ Standard headers (Host, User-Agent) present
- ✅ Custom headers (X-Custom-Header, Authorization) present

### 6. Echo Endpoint with Logging

**Test**: Verify parameter passing and logging

```bash
curl "http://localhost:8080/root/MiddlewareApi/Echo?msg=HelloWorld"
```

**Expected**:
```
"Echo: HelloWorld"
```

**Check log**:
```
info  REQUEST #N [...] GET /root/MiddlewareApi/Echo?msg=HelloWorld
trace Echo: msg=HelloWorld
info  RESPONSE #N - Status: 200 - Body: 17 bytes
```

**Verify**:
- ✅ Parameter correctly decoded
- ✅ Echo logged at trace level
- ✅ Request/response logged at info level

### 7. Request Counter

**Test**: Verify middleware maintains state across requests

```bash
# Make 5 requests
for i in {1..5}; do
  curl http://localhost:8080/root/MiddlewareApi/Index
done

# Stop server (press Enter)
# Check final output
```

**Expected output on exit**:
```
Statistics:
  Total requests processed: 5
```

**Verify**:
- ✅ Counter increments for each request
- ✅ Counter persists across requests (not reset)
- ✅ Final count displayed on shutdown

### 8. Middleware Execution Order

**Test**: Verify middleware chain executes in correct order

```bash
curl http://localhost:8080/root/MiddlewareApi/Index
```

**Check log for sequence**:
```
1. REQUEST #N [...]              ← Logger.OnFilterRequest (BEFORE)
2. Index: Returning 1024 bytes   ← Service method
3. CustomHeaderMiddleware: ...   ← CustomHeaders.OnAfterServiceMethod
4. RESPONSE #N - Status: 200 ... ← Logger.OnFilterResponse (AFTER)
```

**Verify**:
- ✅ Logger before-hook executes first
- ✅ Service method executes second
- ✅ Custom header middleware executes third (after service)
- ✅ Logger after-hook executes last

## Performance Tests

### 9. Overhead Measurement

**Test**: Measure middleware overhead

```bash
# Without middleware (baseline)
time for i in {1..100}; do
  curl -s http://localhost:8080/root/MiddlewareApi/Index > /dev/null
done

# Check average response time in logs
grep "RESPONSE" 12-middleware.log | tail -100 | grep -oP "Duration: \K\d+" | awk '{sum+=$1; count++} END {print sum/count " ms average"}'
```

**Expected**:
- ✅ Average < 5ms per request
- ✅ Consistent times (middleware doesn't accumulate overhead)

### 10. Concurrent Requests

**Test**: Verify middleware is thread-safe

```bash
# Install Apache Bench if needed: sudo apt install apache2-utils

ab -n 1000 -c 10 http://localhost:8080/root/MiddlewareApi/Index
```

**Expected**:
```
Concurrency Level:      10
Time taken for tests:   X seconds
Complete requests:      1000
Failed requests:        0
```

**Verify**:
- ✅ All 1000 requests succeed
- ✅ No race conditions in request counter
- ✅ All responses include custom header

## Integration Tests

### 11. CORS Headers (if implemented)

**Test**: Verify CORS middleware (if enabled)

```bash
curl -i -X OPTIONS \
  -H "Origin: http://example.com" \
  -H "Access-Control-Request-Method: GET" \
  http://localhost:8080/root/MiddlewareApi/Index
```

**Expected** (if CORS enabled):
```
HTTP/1.1 200 OK
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET,POST,PUT,DELETE
```

### 12. Multiple Android Requests

**Test**: Verify redirect middleware doesn't affect subsequent requests

```bash
# Android request (should redirect)
curl -i -H "User-Agent: Android" http://localhost:8080/root/MiddlewareApi/Index

# Normal request (should succeed)
curl -i http://localhost:8080/root/MiddlewareApi/Index
```

**Verify**:
- ✅ First request: 307 redirect
- ✅ Second request: 200 success
- ✅ Middleware state doesn't leak between requests

## Troubleshooting

### No X-Powered-By header

**Check**:
- Middleware properly instantiated in `server.pas`
- `OnAfterServiceMethod` event handler set
- Response not cached by client

### Logs not appearing

**Check**:
- `TSynLog.Family.Level := LOG_VERBOSE` in main program
- Log file permissions
- Check `12-middleware.log` in current directory

### Android redirect not working

**Check**:
- User-Agent header contains "Android" (case-sensitive)
- `OnBeforeUri` event handler set
- Middleware created before server starts

### Request counter wrong

**Check**:
- Only one `TRequestLoggerMiddleware` instance created
- Instance not recreated per request
- Thread-safety if high concurrency

## Success Criteria

All tests pass when:

- ✅ Custom headers added to all responses
- ✅ Android user-agents redirected (307)
- ✅ Non-Android user-agents pass through (200)
- ✅ All requests logged with sequential counters
- ✅ Request/response timing captured
- ✅ Middleware chain executes in correct order
- ✅ No memory leaks or errors
- ✅ Performance overhead < 5ms per request

## Next Steps

After verifying all tests:

1. Review `middleware.pas` implementation
2. Compare with DMVC original in `/mnt/w/DMVCframework/samples/middleware/`
3. Experiment with custom middleware
4. Check [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#6-middleware-equivalents) for patterns
