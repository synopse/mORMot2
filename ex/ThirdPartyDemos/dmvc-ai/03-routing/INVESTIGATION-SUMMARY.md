# TUriRouter Investigation Summary

## Objective
Investigate TUriRouter API in mORMot2 to understand how to implement RESTful routing with path parameters for the 03-routing example.

## Key Findings

### 1. **Location and Documentation**
- **Class**: `TUriRouter`
- **File**: `/mnt/w/mORMot2/src/net/mormot.net.server.pas` (lines 181-326)
- **Documentation**: `/mnt/w/mORMot2/docs/sad-drafts/11.X-TUriRouter.md` (comprehensive 765-line guide)
- **Tests**: `/mnt/w/mORMot2/test/test.net.proto.pas` (TNetworkProtocols._TUriTree)

### 2. **Architecture**
- Built on **Radix Tree** (compressed trie) data structure
- **Per-method routing trees**: Separate tree for each HTTP method (GET/POST/PUT/DELETE/PATCH/OPTIONS/HEAD)
- **Thread-safe**: Uses `TObjectRWLightLock` (read locks during request processing)
- **Ultra-fast performance**: 25.7M lookups/second, zero-allocation for static routes

### 3. **Parameter Syntax**

| Syntax | Description | Example |
|--------|-------------|---------|
| `<param>` | Any non-empty string | `/user/<id>` |
| `<int:param>` | Integer validation | `/user/<int:id>` |
| `<path:param>` | Greedy match (includes slashes) | `/static/<path:filepath>` |
| `*` | Wildcard (synonym for `<path:path>`) | `/static/*` |

### 4. **Core API Methods**

#### Registration
```pascal
// Static route with callback
Server.Route.Get('/plaintext', DoPlainText);

// Parametrized route with path parameters
Server.Route.Get('/user/<user>/pic/<id>', DoUserPic);

// Multi-method route (CRUD)
Server.Route.Run([urmGet, urmPost, urmPut, urmDelete], '/api/<resource>/<id>', DoCrud);

// URI rewriting
Server.Route.Rewrite(urmGet, '/user/<id>', urmGet, '/root/userservice/new?id=<id>');

// RTTI-based automatic routing
Server.Route.RunMethods([urmGet], ControllerInstance, '/api/');
```

#### Parameter Extraction
```pascal
// Default property (string)
userId := Ctxt['id'];

// Type-safe integer extraction
if Ctxt.RouteInt64('id', userIdInt64) then
  // userIdInt64 is now a valid Int64

// Equality check
if Ctxt.RouteEquals('status', 'active') then
  // status parameter equals 'active'
```

### 5. **Performance Characteristics**

From benchmark tests (`TNetworkProtocols._TUriTree`):

| Operation | Speed | Avg Time |
|-----------|-------|----------|
| URI lookups | 25.7M/s | 37ns |
| Static rewrites | 11.9M/s | 80ns |
| Parametrized rewrites | 8.1M/s | 117ns |
| Static execute | 10.4M/s | 91ns |
| Parametrized execute | 5.8M/s | 162ns |

**Key insight**: Zero heap allocations for static routes during request processing.

### 6. **Usage Pattern Example**

```pascal
function DoGetUser(Ctxt: THttpServerRequestAbstract): cardinal;
var
  userId: Int64;
begin
  // Extract and validate path parameter
  if not Ctxt.RouteInt64('id', userId) then
  begin
    Ctxt.OutContent := '{"error":"Invalid user ID"}';
    result := HTTP_BADREQUEST;
    exit;
  end;

  // Process request
  // ... retrieve user from database ...

  Ctxt.OutContent := userJson;
  Ctxt.OutContentType := JSON_CONTENT_TYPE;
  result := HTTP_SUCCESS;
end;

// Registration
Server.Route.Get('/api/users/<int:id>', DoGetUser);
```

### 7. **Real-World Examples**

- **TechEmpower Benchmark**: `/mnt/w/mORMot2/ex/techempower-bench/raw.pas`
  - Uses `RunMethods()` for automatic RTTI-based routing

- **Unit Tests**: `/mnt/w/mORMot2/test/test.net.proto.pas`
  - Extensive test coverage showing all parameter types
  - Examples: `/user/<id>`, `/do/<one>/pic/<two>`, `/da/<one>/<two>/<three>/<four>/`

## Recommended Refactor for 03-routing Example

### Current State
- **Pattern**: RPC-style interface-based services
- **Example**: `POST /root/RoutingApi.GetUser` with JSON body `{"id":123}`

### Proposed Enhancement
**Keep both patterns** to demonstrate the differences:

1. **Existing RPC endpoints** (via `IRoutingApi` interface)
   - POST /root/RoutingApi.GetUser
   - POST /root/RoutingApi.CreateUser
   - etc.

2. **New RESTful endpoints** (via TUriRouter)
   - GET /api/users/:id
   - POST /api/users
   - PUT /api/users/:id
   - DELETE /api/users/:id
   - GET /api/users?filter=active&limit=10

### Implementation Approach

```pascal
type
  TRoutingServer = class
  private
    fHttpServer: THttpAsyncServer;

    // RESTful route handlers
    function DoGetUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoListUsers(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoCreateUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoUpdateUser(Ctxt: THttpServerRequestAbstract): cardinal;
    function DoDeleteUser(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    constructor Create(const Port: RawUtf8);
  end;

constructor TRoutingServer.Create(const Port: RawUtf8);
begin
  fHttpServer := THttpAsyncServer.Create(Port, nil, nil, '', 4);

  // Register RESTful routes with path parameters
  with fHttpServer.Route do
  begin
    Get('/api/users/<int:id>', DoGetUser);
    Get('/api/users', DoListUsers);
    Post('/api/users', DoCreateUser);
    Put('/api/users/<int:id>', DoUpdateUser);
    Delete('/api/users/<int:id>', DoDeleteUser);
  end;

  // Existing RPC-style interface registration remains unchanged
  fHttpServer.ServiceDefine(TRoutingApiService, [IRoutingApi], sicShared);
end;
```

### Benefits of This Approach

1. ✅ **Educational**: Shows both RPC and RESTful patterns side-by-side
2. ✅ **Non-breaking**: Existing code continues to work
3. ✅ **Performance**: TUriRouter is ultra-fast (25M+ lookups/sec)
4. ✅ **Standards**: RESTful endpoints follow industry conventions
5. ✅ **Type safety**: `<int:id>` validates parameters automatically

## Next Steps

1. ✅ **Investigation complete** - TUriRouter API fully documented
2. ⏭️ **Ready for refactor** - Implement RESTful endpoints alongside existing RPC
3. 📝 **Documentation** - Add examples showing both patterns
4. 🧪 **Testing** - Verify both RPC and RESTful routes work correctly

## Additional Resources

- **Full Investigation**: `TUriRouter-investigation.json` (complete API reference)
- **SAD Documentation**: `/mnt/w/mORMot2/docs/sad-drafts/11.X-TUriRouter.md`
- **Source Code**: `/mnt/w/mORMot2/src/net/mormot.net.server.pas`
- **Tests**: `/mnt/w/mORMot2/test/test.net.proto.pas`
- **CLAUDE.md**: `/mnt/w/mORMot2/src/net/CLAUDE.md` (network layer overview)

---

**Investigation Date**: 2025-12-23
**Status**: ✅ Complete - Ready for implementation
