# 22-custom_role_auth - Custom Role-Based Authentication

**Converted from**: DelphiMVCFramework `samples/custom_role_auth`
**Status**: ✅ Compilable
**Difficulty**: ⭐⭐⭐

## Overview

This example demonstrates advanced role-based authentication and authorization in mORMot2, including:

- Multiple users with different role combinations
- Role-based access control (admin, role1, role2, role3)
- Per-method authorization checks
- Public endpoints (no auth required)
- Protected endpoints (auth required)
- AND/OR role logic
- Dynamic role parameters

## Key Concepts

### 1. Multiple User/Role Combinations

```pascal
// In auth.handler.pas - TCustomRoleAuthHandler.CheckPassword
if (username = 'admin') and (aPassWord = 'adminpass') then
begin
  AddRawUtf8(roles, 'admin');
  AddRawUtf8(roles, 'role1');
  AddRawUtf8(roles, 'role2');
end
else if (username = 'user1_2') and (aPassWord = 'user1_2pass') then
begin
  AddRawUtf8(roles, 'role1');
  AddRawUtf8(roles, 'role2');
end
```

### 2. Role Storage in Session

Roles are stored as CSV in the user's session data:

```pascal
User.Data := RawUtf8ArrayToCsv(roles); // "role1,role2"
```

### 3. AND/OR Role Logic

```pascal
// OnlyRole1And2 - Requires BOTH role1 AND role2
hasRole1 := False;
hasRole2 := False;
for i := 0 to High(roles) do
begin
  if roles[i] = 'role1' then hasRole1 := True;
  if roles[i] = 'role2' then hasRole2 := True;
end;
Result := hasRole1 and hasRole2;

// OnlyRole1Or2 - Requires EITHER role1 OR role2
for i := 0 to High(roles) do
  if (roles[i] = 'role1') or (roles[i] = 'role2') then
    Result := True;
```

### 4. Per-Method Authorization

Authorization is checked in `OnMethodAuth`:

```pascal
function TCustomRoleAuthServer.OnMethodAuth(
  Sender: TRestServer; Ctxt: TRestServerUriContext;
  const aInterfaceDotMethodName: RawUtf8): boolean;
```

## Static Users

| Username | Password | Roles |
|----------|----------|-------|
| admin | adminpass | admin, role1, role2 |
| user1 | user1pass | role1 |
| user2 | user2pass | role2 |
| user1_2 | user1_2pass | role1, role2 |
| user3 | user3pass | role3 |

## Endpoints

### Public Endpoints

| Endpoint | Method | Auth | Description |
|----------|--------|------|-------------|
| `/root/PrivateApi.PublicAction` | POST | None | Public action in private API |

### Protected Endpoints

| Endpoint | Method | Requires | Description |
|----------|--------|----------|-------------|
| `/root/PrivateApi.Index` | POST | Any auth | Index (any user) |
| `/root/PrivateApi.OnlyRole1` | POST | role1 | Role1 only |
| `/root/PrivateApi.OnlyRole2` | POST | role2 | Role2 only |
| `/root/PrivateApi.OnlyRole1And2` | POST | role1 AND role2 | Both roles |
| `/root/PrivateApi.OnlyRole1Or2` | POST | role1 OR role2 | Either role |
| `/root/PrivateApi.AccessByRole` | POST | Dynamic | Role parameter |

## Access Control Matrix

| User | Index | Role1 | Role2 | Role1&2 | Role1\|2 |
|------|-------|-------|-------|---------|----------|
| admin | ✅ | ✅ | ✅ | ✅ | ✅ |
| user1 | ✅ | ✅ | ❌ | ❌ | ✅ |
| user2 | ✅ | ❌ | ✅ | ❌ | ✅ |
| user1_2 | ✅ | ✅ | ✅ | ✅ | ✅ |
| user3 | ✅ | ❌ | ❌ | ❌ | ❌ |

## Testing

Run the test script:

```bash
chmod +x test-endpoints.sh
./test-endpoints.sh
```

### Manual Testing

1. **Login as admin**:
```bash
curl -X POST http://localhost:8080/root/Auth \
  -H "Content-Type: application/json" \
  -d '{"userName":"admin","password":"adminpass"}'
```

Response:
```json
{
  "result": 1,
  "data": {
    "session_signature": "000000010099EDB4CA51..."
  }
}
```

2. **Test role1 endpoint** (admin has role1):
```bash
curl -X POST http://localhost:8080/root/PrivateApi.OnlyRole1 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer 1+000000010099EDB4CA51..." \
  -d '{}'
```

Response:
```json
{
  "result": "OK from a \"role1\" action"
}
```

3. **Login as user1** (only has role1):
```bash
curl -X POST http://localhost:8080/root/Auth \
  -H "Content-Type: application/json" \
  -d '{"userName":"user1","password":"user1pass"}'
```

4. **Test role1 and role2 endpoint** (user1 doesn't have both):
```bash
curl -X POST http://localhost:8080/root/PrivateApi.OnlyRole1And2 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer SESSION_ID+SIGNATURE" \
  -d '{}'
```

Expected: `403 Forbidden` (insufficient privileges)

5. **Login as user1_2** (has both role1 and role2):
```bash
curl -X POST http://localhost:8080/root/Auth \
  -H "Content-Type: application/json" \
  -d '{"userName":"user1_2","password":"user1_2pass"}'
```

6. **Test role1 and role2 endpoint** (user1_2 has both):
```bash
curl -X POST http://localhost:8080/root/PrivateApi.OnlyRole1And2 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer SESSION_ID+SIGNATURE" \
  -d '{}'
```

Expected: `200 OK` with `"OK from a \"role1 and role2\" action"`

## Comparison with DMVC

### DMVC Approach
- Uses `TRoleBasedAuthHandler` middleware
- Attributes: `[MVCRequiresRole('role1')]`, `[MVCRequiresRole('role1;role2', MVCRoleEval.reAND)]`
- Automatic role checking based on attributes
- OnAuthentication callback populates UserRoles list

### mORMot2 Approach
- Uses custom `TCustomRoleAuthHandler` authentication
- Stores roles in session data as CSV
- Manual authorization check in `OnMethodAuth`
- Explicit role validation per method

## Learning Points

1. **Session data usage**: Store arbitrary data (like roles) in `TAuthUser.Data`
2. **CSV role storage**: Simple, efficient role list serialization
3. **Admin bypass**: Check for admin role first to grant full access
4. **AND logic**: Check for all required roles using boolean flags
5. **OR logic**: Check for any matching role using loop with early exit
6. **Dynamic authorization**: Parse method name to determine required roles

## Files

- `CustomRoleAuthSample.dpr` - Main program with startup banner
- `src/api.interfaces.pas` - IPrivateApi interface
- `src/api.impl.pas` - TPrivateApi implementation
- `src/auth.handler.pas` - Custom authentication and authorization
- `src/server.pas` - Server setup
- `test-endpoints.sh` - Automated endpoint testing

## Related Examples

- **09-custom_auth** - Basic custom authentication (foundation)
- **08-basicauth** - HTTP Basic authentication
- **10-jsonwebtoken** - JWT-based authentication

## Next Steps

After this example, you might explore:
- Combining with JWT tokens (example 10)
- Database-backed user/role management
- Permission caching and optimization
- Role hierarchy (admin > manager > user)
- Attribute-based access control (ABAC)
