# Testing Guide - Action Filters Sample

## Compilation Test

✅ **Status**: PASSED
- Platform: Win64
- Config: Debug
- Errors: 0
- Warnings: 0
- Hints: 0

## Manual Testing

### Test 1: Basic Request (Weekday)

**Objective**: Verify service works normally on weekdays

**Steps**:
1. Run `ActionFilters.exe`
2. Execute:
   ```bash
   curl http://localhost:8080/root/ActionFiltersApi/Person/123
   ```

**Expected Result**:
```json
{
  "FirstName": "Daniele",
  "LastName": "Teti",
  "DOB": "1975-05-02T00:00:00",
  "Married": true
}
```

**Verification**:
- [ ] Response is valid JSON
- [ ] Status code is 200 OK
- [ ] Person data matches expected values

### Test 2: Weekend Blocking

**Objective**: Verify OnBeforeAction blocks weekend requests

**Steps**:
1. Run `ActionFilters.exe` (on a weekend)
2. Execute:
   ```bash
   curl -v http://localhost:8080/root/ActionFiltersApi/Person/123
   ```

**Expected Result**:
```
HTTP/1.1 403 Forbidden
Content-Type: text/plain

You cannot use this service in the WeekEnd
```

**Verification**:
- [ ] Status code is 403 Forbidden
- [ ] Error message is displayed
- [ ] Request is blocked before GetPerson executes

**Alternative Test** (any day):
Temporarily modify `action.filters.pas` line 61:
```pascal
// Change this:
if DayOfWeek(Now) in [1, 7] then

// To this (always block):
if True then
```

### Test 3: Logging Verification

**Objective**: Verify OnBeforeAction and OnAfterAction logging

**Steps**:
1. Run `ActionFilters.exe`
2. Execute a request:
   ```bash
   curl http://localhost:8080/root/ActionFiltersApi/Person/456
   ```
3. Check `ActionFilters.log` file

**Expected Log Entries**:
```
ActionFilterMiddleware created (equivalent to MVCControllerAfterCreate)
OnBeforeAction: Validating action /root/ActionFiltersApi/Person/456 from 127.0.0.1
GetPerson called with id=456
GetPerson returning JSON: {"FirstName":"Daniele","LastName":"Teti",...}
OnAfterAction: ACTION CALLED: mGET mapped to /root/ActionFiltersApi/Person/456 from 127.0.0.1 (took X ms)
```

**Verification**:
- [ ] Before-action validation logged
- [ ] Service method execution logged
- [ ] After-action details logged (with timing)
- [ ] Client IP address captured

### Test 4: Multiple Requests

**Objective**: Verify filters work consistently across multiple requests

**Steps**:
1. Run `ActionFilters.exe`
2. Execute multiple requests:
   ```bash
   curl http://localhost:8080/root/ActionFiltersApi/Person/1
   curl http://localhost:8080/root/ActionFiltersApi/Person/2
   curl http://localhost:8080/root/ActionFiltersApi/Person/3
   ```

**Expected Result**:
- All requests succeed (weekday)
- Each request is logged separately
- Timing information varies per request

**Verification**:
- [ ] All 3 requests return valid JSON
- [ ] Log shows 3 distinct before/after cycles
- [ ] No filter state leakage between requests

### Test 5: Lifecycle Hooks

**Objective**: Verify middleware creation/destruction logging

**Steps**:
1. Run `ActionFilters.exe`
2. Make one request
3. Press Enter to stop server
4. Check log file

**Expected Log Entries**:
```
ActionFilterMiddleware created (equivalent to MVCControllerAfterCreate)
... (request processing) ...
ActionFilterMiddleware being destroyed (equivalent to MVCControllerBeforeDestroy)
```

**Verification**:
- [ ] Creation message at startup
- [ ] Destruction message at shutdown
- [ ] Order matches DMVC controller lifecycle

### Test 6: Invalid ID (Edge Case)

**Objective**: Verify filters work with any parameter value

**Steps**:
1. Run `ActionFilters.exe`
2. Execute:
   ```bash
   curl http://localhost:8080/root/ActionFiltersApi/Person/invalid
   curl http://localhost:8080/root/ActionFiltersApi/Person/
   curl http://localhost:8080/root/ActionFiltersApi/Person/999999
   ```

**Expected Result**:
- All requests pass through filters
- Service method receives parameter as-is
- Returns same fixed person data (ID not used in implementation)

**Verification**:
- [ ] OnBeforeAction runs for all variants
- [ ] OnAfterAction runs for all variants
- [ ] No filter crashes on invalid input

## Performance Testing

### Request Timing

**Objective**: Measure filter overhead

**Steps**:
1. Run `ActionFilters.exe`
2. Execute:
   ```bash
   curl -w "\nTime: %{time_total}s\n" http://localhost:8080/root/ActionFiltersApi/Person/123
   ```
3. Check log for server-side timing

**Expected Result**:
- Client time: < 100ms
- Server log shows similar timing in OnAfterAction

**Verification**:
- [ ] Filter overhead is minimal (< 10ms)
- [ ] Total request time is acceptable

## Comparison with DMVC Original

### Feature Parity Checklist

- [x] OnBeforeAction equivalent (OnFilterRequest)
- [x] OnAfterAction equivalent (OnAfterServiceMethod)
- [x] Weekend validation logic
- [x] Action name logging
- [x] Client IP logging
- [x] Request timing
- [x] Lifecycle hooks (Create/Destroy)
- [x] Exception → HTTP error conversion

### Behavior Verification

**DMVC Behavior**:
```pascal
procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
  var Handled: Boolean); override;
begin
  if DayOfWeek(date) in [1, 7] then
    raise Exception.Create('You cannot use this service in the WeekEnd');
end;
```

**mORMot2 Equivalent**:
```pascal
function OnBeforeServiceMethod(...): Boolean;
begin
  if DayOfWeek(Now) in [1, 7] then
  begin
    Ctxt.Error('You cannot use this service in the WeekEnd', HTTP_FORBIDDEN);
    Result := True; // Block
  end;
end;
```

**Verification**:
- [ ] Same weekend check logic
- [ ] Same error message
- [ ] Same blocking behavior

## Known Issues / Limitations

1. **Date Simulation**: No built-in way to simulate weekend for testing
   - **Workaround**: Temporarily modify `DayOfWeek(Now)` check

2. **Filter Ordering**: mORMot2 doesn't have explicit middleware chains
   - **Impact**: Single filter instance per server (not a chain)
   - **Note**: DMVC original also has single controller instance

3. **Context Differences**:
   - DMVC: `Context.Request.PathInfo`
   - mORMot2: `Ctxt.Uri`
   - **Impact**: Minor API differences, same functionality

## Success Criteria

✅ Project compiles without errors
✅ Weekday requests succeed
✅ Weekend requests blocked with 403
✅ OnBeforeAction validation runs
✅ OnAfterAction logging runs
✅ Lifecycle hooks logged
✅ Multiple requests work consistently
✅ Timing information captured

## Automated Test Script (Optional)

```bash
#!/bin/bash
# Test Action Filters sample

echo "Test 1: Basic request"
response=$(curl -s -w "\n%{http_code}" http://localhost:8080/root/ActionFiltersApi/Person/123)
status=$(echo "$response" | tail -n1)
body=$(echo "$response" | head -n-1)

if [ "$status" -eq "200" ]; then
  echo "✅ PASS: Status 200 OK"
  echo "$body" | grep -q "Daniele" && echo "✅ PASS: Body contains expected data"
else
  echo "❌ FAIL: Status $status (expected 200)"
fi

echo ""
echo "Test 2: Check logs"
if grep -q "OnBeforeAction" ActionFilters.log; then
  echo "✅ PASS: OnBeforeAction logged"
else
  echo "❌ FAIL: OnBeforeAction not found in logs"
fi

if grep -q "OnAfterAction" ActionFilters.log; then
  echo "✅ PASS: OnAfterAction logged"
else
  echo "❌ FAIL: OnAfterAction not found in logs"
fi
```

## Documentation

### What This Sample Teaches

1. **Action Filters Pattern**: Pre/post processing hooks for service methods
2. **Request Validation**: Business logic checks before execution
3. **Logging Best Practices**: Structured logging with timing information
4. **Lifecycle Management**: Proper initialization and cleanup
5. **Error Handling**: Converting validation failures to HTTP errors

### Real-World Use Cases

- **Authentication**: Check API keys before action execution
- **Rate Limiting**: Track request counts per client
- **Request Logging**: Audit trail for compliance
- **Performance Monitoring**: Track slow endpoints
- **Input Validation**: Sanitize/validate parameters
- **Response Transformation**: Add headers, compress responses
