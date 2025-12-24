# 33-jsonwebtoken_roleauth - JWT Role-Based Authorization

**Port of**: DMVCFramework `samples/jsonwebtoken_roleauth`
**Difficulty**: Intermediate
**Demonstrates**: JWT authentication with role-based authorization and custom claims

## Overview

This sample demonstrates JWT authentication with **role-based authorization**. It shows how to:

1. **Embed roles in JWT tokens** - Roles are included as custom claims in the JWT payload
2. **Enforce role requirements** - Endpoints require specific roles for access
3. **Store custom claims** - Additional user data (custom claims) in JWT and session
4. **Authorization logic** - Server validates user roles before granting access to protected resources

This is the standard pattern for implementing authorization in stateless REST APIs.

## DMVC → mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `TMVCJWTAuthenticationMiddleware` | `TJwtRoleAuth` (custom `TRestServerAuthentication`) | JWT middleware → custom auth handler |
| `[MVCRequiresRole('role1')]` | `ServiceDefine().SetAuthGroupByIndex(0, 'role1')` | Method-level role requirements |
| `Context.LoggedUser.Roles` | `TAuthUser.GroupRights` (list of `TAuthGroup`) | User roles/groups |
| `Context.LoggedUser.CustomData` | `TAuthSession.PrivateData` | Custom session data |
| `OnAuthentication` callback | `Login()` method | User authentication logic |
| `OnAuthorization` callback | mORMot2 automatic via `TAuthGroup` | Role checking |

## Implementation Details

### Key Components

#### 1. JWT Authentication with Roles (`auth.pas`)

```pascal
procedure TJwtRoleAuth.Login(Ctxt: TRestServerUriContext);
begin
  // Determine roles based on username
  roles.InitArray([], JSON_FAST);
  if (username = 'user1') or (username = 'user3') then
    roles.AddItem('role1');
  if (username = 'user2') or (username = 'user3') then
    roles.AddItem('role2');

  // Add custom claims
  customData.InitObject(['customkey1', 'customvalue1',
                         'customkey2', 'customvalue2'], JSON_FAST);

  // Generate JWT with roles and custom claims
  token := fJwt.Compute(
    [
      'roles', variant(roles),
      'customkey1', 'customvalue1',
      'customkey2', 'customvalue2'
    ],
    'mORMot2 JWT Role Auth Demo',  // issuer
    username);                      // subject
end;
```

#### 2. Role Extraction from JWT

```pascal
function TJwtRoleAuth.RetrieveSession(...): cardinal;
begin
  // Verify JWT
  if fJwt.Verify(token, content) <> jwtValid then
    exit;

  // Extract roles from JWT claims
  if content.data.GetAsDocVariant('roles', roles) then
  begin
    for i := 0 to roles.Count - 1 do
    begin
      group := TAuthGroup.Create;
      group.Ident := StringToUtf8(roles.Items[i]);
      user.GroupRights.Add(group);
    end;
  end;

  // Create session with roles
  Result := Server.SessionCreate(user, Ctxt);
end;
```

#### 3. Service Authorization Configuration

```pascal
// Register service with method-level role requirements
fRestServer.ServiceDefine(TAdminApi, [IAdminApi], sicShared)
  .SetAuthGroupByIndex(0, 'role1')  // Method 0: ProtectedRole1 requires role1
  .SetAuthGroupByIndex(1, 'role1')  // Method 1: ProtectedRole1Json requires role1
  .SetAuthGroupByIndex(2, 'role2'); // Method 2: ProtectedRole2 requires role2
```

### JWT Claims Structure

#### Standard Claims
| Claim | Value | Purpose |
|-------|-------|---------|
| `iss` | `mORMot2 JWT Role Auth Demo` | Issuer |
| `sub` | username | Subject (user identifier) |
| `exp` | timestamp | Expiration (Unix time) |
| `iat` | timestamp | Issued at (Unix time) |

#### Custom Claims
| Claim | Type | Example Value | Purpose |
|-------|------|---------------|---------|
| `roles` | Array | `["role1", "role2"]` | User roles for authorization |
| `customkey1` | String | `customvalue1` | Application-specific data |
| `customkey2` | String | `customvalue2` | Application-specific data |

## Usage

### 1. Build and Run

```bash
cd /mnt/w/mORMot2/ex/dmvc/33-jsonwebtoken_roleauth
delphi-compiler.exe JwtRoleAuthServer.dproj --config=Debug --platform=Win32
bin\Win32\Debug\JwtRoleAuthServer.exe
```

Server starts on `http://localhost:8080`

### 2. Login as Different Users

#### User1 (has role1 only)
```bash
curl -X POST http://localhost:8080/root/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"user1","password":"user1"}'
```

Response:
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "user": "user1",
  "roles": ["role1"],
  "customclaims": {
    "customkey1": "customvalue1",
    "customkey2": "customvalue2"
  }
}
```

#### User2 (has role2 only)
```bash
curl -X POST http://localhost:8080/root/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"user2","password":"user2"}'
```

#### User3 (has both roles)
```bash
curl -X POST http://localhost:8080/root/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"user3","password":"user3"}'
```

### 3. Test Role-Based Access

#### Access role1 Endpoint with user1 Token (SUCCESS)
```bash
curl http://localhost:8080/root/AdminApi/ProtectedRole1 \
  -H "Authorization: Bearer USER1_TOKEN"
```

Response:
```
Hey! Hello user1, now you are a logged user and this is a protected content!
As logged user you have the following roles: role1
Your custom claims: {"customkey1":"customvalue1","customkey2":"customvalue2"}
```

#### Access role1 Endpoint with user2 Token (FORBIDDEN)
```bash
curl http://localhost:8080/root/AdminApi/ProtectedRole1 \
  -H "Authorization: Bearer USER2_TOKEN"
```

Response:
```
HTTP 403 Forbidden
User lacks required role: role1
```

#### Access role2 Endpoint with user2 Token (SUCCESS)
```bash
curl http://localhost:8080/root/AdminApi/ProtectedRole2 \
  -H "Authorization: Bearer USER2_TOKEN"
```

#### Access Any Endpoint with user3 Token (SUCCESS)
```bash
curl http://localhost:8080/root/AdminApi/ProtectedRole1 \
  -H "Authorization: Bearer USER3_TOKEN"

curl http://localhost:8080/root/AdminApi/ProtectedRole2 \
  -H "Authorization: Bearer USER3_TOKEN"
```

Both succeed - user3 has all roles.

### 4. JSON Response Endpoint

```bash
curl http://localhost:8080/root/AdminApi/ProtectedRole1Json \
  -H "Authorization: Bearer USER1_TOKEN"
```

Response:
```json
{
  "message": "This is protected content accessible only by users with role1",
  "username": "user1",
  "roles": "role1",
  "customclaims": "{\"customkey1\":\"customvalue1\",\"customkey2\":\"customvalue2\"}",
  "querystringparameters": {}
}
```

## Available Endpoints

| Method | Path | Auth | Role | Description |
|--------|------|------|------|-------------|
| POST | `/root/auth/login` | No | - | Login and get JWT with roles |
| GET | `/root/PublicApi/PublicInfo` | No | - | Public information |
| GET | `/root/AdminApi/ProtectedRole1` | Yes | role1 | Protected text for role1 users |
| GET | `/root/AdminApi/ProtectedRole1Json` | Yes | role1 | Protected JSON for role1 users |
| GET | `/root/AdminApi/ProtectedRole2` | Yes | role2 | Protected text for role2 users |

## Test Users

| Username | Password | Roles | Can Access |
|----------|----------|-------|------------|
| `user1` | `user1` | `role1` | ProtectedRole1, ProtectedRole1Json |
| `user2` | `user2` | `role2` | ProtectedRole2 |
| `user3` | `user3` | `role1`, `role2` | All protected endpoints |

## Key Differences from DMVC

### 1. Role Declaration

**DMVC**: Attribute-based on controller methods
```pascal
[MVCRequiresRole('role1')]
procedure OnlyRole1;
```

**mORMot2**: Configuration-based during service registration
```pascal
ServiceDefine(TAdminApi, [IAdminApi], sicShared)
  .SetAuthGroupByIndex(0, 'role1')  // Method index 0
```

### 2. Role Assignment

**DMVC**: Via `UserRoles.Add()` in `OnAuthentication`
```pascal
procedure TAuthentication.OnAuthentication(...; UserRoles: TList<string>; ...);
begin
  if UserName = 'user1' then
    UserRoles.Add('role1');
end;
```

**mORMot2**: Via `TAuthGroup` added to user
```pascal
group := TAuthGroup.Create;
group.Ident := 'role1';
user.GroupRights.Add(group);
```

### 3. Custom Claims

**DMVC**: Via `SessionData` dictionary
```pascal
SessionData.AddOrSetValue('customkey1', 'customvalue1');
```

**mORMot2**: Via `TAuthSession.PrivateData` (TDocVariant)
```pascal
session.PrivateData := TDocVariantData.NewObject([
  'customkey1', 'customvalue1',
  'customkey2', 'customvalue2'
]);
```

### 4. Authorization Enforcement

**DMVC**: Middleware checks `[MVCRequiresRole]` attribute automatically

**mORMot2**: Built-in via `TAuthGroup` - server checks group membership before calling service method

## Testing Scenarios

### Scenario 1: Correct Role Access
1. Login as user1 (role1)
2. Access ProtectedRole1 → SUCCESS
3. Access ProtectedRole2 → FORBIDDEN (lacks role2)

### Scenario 2: Multiple Roles
1. Login as user3 (role1 + role2)
2. Access ProtectedRole1 → SUCCESS
3. Access ProtectedRole2 → SUCCESS

### Scenario 3: No Roles
1. Try to access protected endpoint without login → UNAUTHORIZED

### Scenario 4: Expired Token
1. Login → Get token
2. Wait until token expires (1 hour by default)
3. Access protected endpoint → UNAUTHORIZED (expired token)

### Scenario 5: Custom Claims
1. Login as any user
2. Access protected endpoint
3. Verify custom claims (customkey1, customkey2) are accessible

## Logging Output

Server logs show role-based authorization:

```
20241220 12:00:00.000 INFO JWT role-based authentication initialized
20241220 12:00:05.123 DEBUG Login attempt for user: user1
20241220 12:00:05.124 INFO User user1 logged in, session=1, roles=["role1"]
20241220 12:00:10.456 DEBUG JWT verified for subject: user1
20241220 12:00:10.457 DEBUG User user1 has roles: ["role1"]
20241220 12:00:10.458 DEBUG Session created: 1 for user: user1 with roles: ["role1"]
20241220 12:00:15.789 WARNING Access denied: user user1 lacks role: role2
```

## Production Considerations

1. **Role hierarchy** - Implement role inheritance (admin > moderator > user)
2. **Dynamic roles** - Load roles from database instead of hardcoded logic
3. **Role caching** - Cache role lookups to avoid database queries
4. **Audit logging** - Log all authorization failures for security monitoring
5. **Token refresh** - Implement refresh tokens for long-lived sessions
6. **Principle of least privilege** - Grant minimum required roles

## Advanced Usage

### Custom Role Validation

Extend `TJwtRoleAuth` to add custom authorization logic:

```pascal
function TJwtRoleAuth.CheckCustomPermission(user: TAuthUser;
  const permission: RawUtf8): Boolean;
begin
  // Custom permission check beyond roles
  Result := (user.LogonName = 'admin') or
            HasRole(user, 'superuser');
end;
```

### Dynamic Role Loading

Replace hardcoded roles with database lookup:

```pascal
procedure TJwtRoleAuth.Login(Ctxt: TRestServerUriContext);
var
  userRoles: TRawUtf8DynArray;
begin
  // Load roles from database
  userRoles := LoadUserRolesFromDatabase(username);

  for role in userRoles do
    roles.AddItem(role);
end;
```

## See Also

- **10-jsonwebtoken** - Basic JWT authentication (no roles)
- **32-jsonwebtoken_livevaliditywindow** - JWT with short expiration for testing
- **DMVC Original**: `/mnt/w/DMVCframework/samples/jsonwebtoken_roleauth/`
- **mORMot2 JWT**: `mormot.crypt.jwt.pas`
- **mORMot2 Auth**: `mormot.rest.server.pas` - `TRestServerAuthentication`

## References

- JWT Best Practices: RFC 8725
- RBAC Model: https://en.wikipedia.org/wiki/Role-based_access_control
- mORMot2 Authentication: https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html#TITLE_83
