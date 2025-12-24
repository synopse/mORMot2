# 03-routing Refactoring Summary

## Objective
Add TUriRouter-based RESTful endpoints alongside existing RPC-style interface-based services to demonstrate both routing patterns in mORMot2.

## Implementation Date
2025-12-23

## Changes Made

### 1. Updated `/mnt/w/mORMot2/ex/dmvc/03-routing/src/server.pas`

#### Added Units
- `mormot.core.json` - JSON serialization helpers
- `mormot.core.rtti` - Type information for serialization
- `mormot.core.variants` - TDocVariantData for JSON parsing
- `mormot.net.server` - TUriRouter support
- `mormot.net.http` - THttpServerRequestAbstract

#### Added Fields to TRoutingSampleServer
```pascal
fApiImpl: TRoutingApi;  // Shared implementation instance
```

#### Added Methods to TRoutingSampleServer
```pascal
// RESTful endpoint handlers (TUriRouter)
function DoGetUser(Ctxt: THttpServerRequestAbstract): cardinal;
function DoListUsers(Ctxt: THttpServerRequestAbstract): cardinal;
function DoCreateUser(Ctxt: THttpServerRequestAbstract): cardinal;
function DoUpdateUser(Ctxt: THttpServerRequestAbstract): cardinal;
function DoDeleteUser(Ctxt: THttpServerRequestAbstract): cardinal;
function DoSearchUsers(Ctxt: THttpServerRequestAbstract): cardinal;
function DoGetUsersByStatus(Ctxt: THttpServerRequestAbstract): cardinal;

// Setup RESTful routes using TUriRouter
procedure SetupRestfulRoutes;
```

#### Modified Constructor
- Creates shared `fApiImpl` instance
- Calls `SetupRestfulRoutes` to register RESTful endpoints
- Logs both RPC and RESTful endpoints at startup

#### Modified Destructor
- Frees `fApiImpl` instance

#### Implemented SetupRestfulRoutes
Registers 7 RESTful routes:
```pascal
fHttpServer.Route.Get('/api/users/<int:id>', DoGetUser);
fHttpServer.Route.Get('/api/users', DoListUsers);
fHttpServer.Route.Get('/api/search/<term>', DoSearchUsers);
fHttpServer.Route.Post('/api/users', DoCreateUser);
fHttpServer.Route.Put('/api/users/<int:id>', DoUpdateUser);
fHttpServer.Route.Delete('/api/users/<int:id>', DoDeleteUser);
fHttpServer.Route.Get('/api/users/status/<status>', DoGetUsersByStatus);
```

#### Implemented Handler Functions
All handlers:
- Extract parameters from URL path using `Ctxt.RouteInt64()` or `Ctxt['param']`
- Extract query parameters using `Ctxt.UrlParam()`
- Parse JSON bodies using `TDocVariantData.InitJson()`
- Call shared `fApiImpl` methods for business logic
- Manually serialize responses using `RecordSaveJson()` or `DynArraySaveJson()`
- Return appropriate HTTP status codes

### 2. Updated `/mnt/w/mORMot2/ex/dmvc/03-routing/README.md`

Complete rewrite with:
- Side-by-side comparison table of RPC vs RESTful endpoints
- Detailed explanation of both routing patterns
- TUriRouter feature documentation
- Implementation patterns and best practices
- When to use each pattern
- Migration guide from DMVC to mORMot2 RESTful
- Testing examples (curl and PowerShell)
- Performance comparison
- Advanced TUriRouter features

## Key Design Decisions

### 1. Shared Business Logic
Both routing styles share the same `TRoutingApi` implementation:
- RPC endpoints: Automatically instantiated by ServiceDefine
- RESTful endpoints: Manually instantiated and stored in `fApiImpl`
- All business logic remains in `api.impl.pas` (single source of truth)

### 2. Parameter Extraction Patterns
**URL Path Parameters**:
```pascal
Ctxt.RouteInt64('id', userId)  // Type-safe integer extraction
Ctxt['status']                  // String parameter
```

**Query Parameters**:
```pascal
Ctxt.UrlParam('FILTER=', filter)
Ctxt.UrlParam('LIMIT=', limit)
```

**JSON Body**:
```pascal
var doc: TDocVariantData;
doc.InitJson(Ctxt.InContent, JSON_FAST)
name := doc.U['name']
```

### 3. Error Handling
RESTful handlers catch `EServiceException` and convert to appropriate HTTP status codes:
- 400 (Bad Request) - Invalid parameters or JSON
- 404 (Not Found) - Resource not found
- 201 (Created) - Successful creation
- 200 (Success) - Successful operation

## Compilation Results

**Status**: SUCCESS (warnings only)
- 0 errors
- 10 warnings (implicit string casts in api.impl.pas - pre-existing)
- 6 hints (inline functions, unused assignment - minor)

Executable location:
```
/mnt/w/mORMot2/ex/dmvc/03-routing/bin/Win32/Release/RoutingSample.exe
```

## Testing Verification

### RPC Endpoints
```bash
curl -X POST http://localhost:8080/root/RoutingApi.GetUser \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

### RESTful Endpoints
```bash
curl -X GET http://localhost:8080/api/users/1
curl -X GET "http://localhost:8080/api/users?filter=active&limit=5"
curl -X GET http://localhost:8080/api/search/user1
curl -X POST http://localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name":"Jane","email":"jane@example.com"}'
curl -X PUT http://localhost:8080/api/users/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"Jane Updated","email":"jane@example.com"}'
curl -X DELETE http://localhost:8080/api/users/1
curl -X GET "http://localhost:8080/api/users/status/active?page=1&pageSize=10"
```

## Files Modified

1. `/mnt/w/mORMot2/ex/dmvc/03-routing/src/server.pas` (~357 lines, +220 new code)
2. `/mnt/w/mORMot2/ex/dmvc/03-routing/README.md` (~595 lines, complete rewrite)
3. `/mnt/w/mORMot2/ex/dmvc/03-routing/REFACTOR-SUMMARY.md` (new file)

## Files Unchanged

- `src/api.interfaces.pas` - No changes needed
- `src/api.impl.pas` - No changes needed
- `RoutingSample.dpr` - No changes needed
- `RoutingSample.dproj` - No changes needed

## Benefits Achieved

1. **Educational Value**: Demonstrates both routing paradigms side-by-side
2. **Practical Example**: Shows how to share business logic between patterns
3. **DMVC Migration Path**: Clear examples for DMVC developers
4. **RESTful API Support**: mORMot2 can now expose REST APIs with proper HTTP semantics
5. **Flexibility**: Developers can choose the right pattern for each use case

## TUriRouter Performance

Based on `/mnt/w/mORMot2/test/test.net.proto.pas`:
- URI lookups: 25.7M/sec (37ns avg)
- Static execute: 10.4M/sec (91ns avg)
- Parametrized execute: 5.8M/sec (162ns avg)
- Zero heap allocations for static routes

## Documentation References

- TUriRouter investigation: `TUriRouter-investigation.json`
- TUriRouter source: `/mnt/w/mORMot2/src/net/mormot.net.server.pas` (lines 181-326)
- TUriRouter SAD: `/mnt/w/mORMot2/docs/sad-drafts/11.X-TUriRouter.md`
- Real-world usage: `/mnt/w/mORMot2/ex/techempower-bench/raw.pas`

## Conclusion

The refactoring successfully demonstrates that mORMot2 supports both:
1. **RPC-Style (Interface-Based)**: Traditional mORMot2 approach with automatic JSON handling
2. **RESTful (TUriRouter-Based)**: Direct HTTP routing with proper REST semantics

Both patterns can coexist in the same application, sharing business logic, providing maximum flexibility for API design.

---
**Implementation Status**: ✅ COMPLETE
**Compilation Status**: ✅ SUCCESS
**Documentation Status**: ✅ COMPLETE
