# Custom Authentication Sample

**Converted from**: DelphiMVCFramework `samples/custom_auth/`
**mORMot2 Pattern**: Interface-based services with custom authentication handler

## Overview

This sample demonstrates how to implement custom authentication and role-based authorization in mORMot2, equivalent to DMVC's `TMVCCustomAuthenticationMiddleware`.

### Key Features

- **Custom user validation** - Authenticate users against custom logic (database, LDAP, etc.)
- **Role-based authorization** - Control access to methods based on user roles
- **Per-method access control** - Fine-grained permissions
- **Public endpoints** - Some endpoints don't require authentication
- **Session management** - Built-in mORMot2 session handling

## DMVC vs mORMot2 Mapping

### Authentication Flow

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| Auth Handler | `IMVCAuthenticationHandler` | `TRestServerAuthenticationDefault` |
| User Validation | `OnAuthentication` callback | `CheckPassword` override |
| Authorization | `OnAuthorization` callback | `OnBeforeUriAuth` event |
| Session Storage | `SessionData` dictionary | `TAuthUser.Data` field |
| Roles | `UserRoles: TList<string>` | Stored in `TAuthUser.Data` as CSV |

### Code Comparison

#### DMVC Authentication

```pascal
type
  TCustomAuth = class(TInterfacedObject, IMVCAuthenticationHandler)
  public
    procedure OnAuthentication(const AContext: TWebContext;
      const UserName, Password: string;
      UserRoles: TList<string>; var IsValid: Boolean);
    procedure OnAuthorization(const AContext: TWebContext;
      UserRoles: TList<string>; const ControllerQualifiedClassName,
      ActionName: string; var IsAuthorized: Boolean);
  end;

// Usage
FMVC.AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(
  TCustomAuth.Create, '/system/users/logged'));
```

#### mORMot2 Authentication

```pascal
type
  TCustomAuthenticationHandler = class(TRestServerAuthenticationDefault)
  protected
    function CheckPassword(Ctxt: TRestServerUriContext; User: TAuthUser;
      const aClientNonce, aPassWord: RawUtf8): boolean; override;
  end;

  TCustomAuthServer = class(TRestServerFullMemory)
  protected
    function OnMethodAuth(Sender: TRestServer;
      Ctxt: TRestServerUriContext;
      const aInterfaceDotMethodName: RawUtf8): boolean;
  end;

// Usage
Server.AuthenticationRegister(TCustomAuthenticationHandler);
Server.OnBeforeUriAuth := Server.OnMethodAuth;
```

## User Roles

The sample includes three predefined users:

| Username | Password | Roles | Access |
|----------|----------|-------|--------|
| `admin` | `adminpass` | admin | All endpoints |
| `user1` | `user1pass` | role1 | PublicApi.*, PrivateApi.OnlyRole1, PrivateApi.PublicAction |
| `user2` | `user2pass` | role2 | PublicApi.*, PrivateApi.OnlyRole2, PrivateApi.PublicAction |

## API Endpoints

### Public (No Authentication)

| Method | Endpoint | Description |
|--------|----------|-------------|
| `PublicApi.Index` | POST /root/PublicApi.Index | Public hello world |
| `PrivateApi.PublicAction` | POST /root/PrivateApi.PublicAction | Public action in private controller |

### Protected (Authentication Required)

| Method | Endpoint | Required Role |
|--------|----------|---------------|
| `PrivateApi.Index` | POST /root/PrivateApi.Index | admin |
| `PrivateApi.OnlyRole1` | POST /root/PrivateApi.OnlyRole1 | role1, admin |
| `PrivateApi.OnlyRole2` | POST /root/PrivateApi.OnlyRole2 | role2, admin |

## Testing

### 1. Start the Server

```bash
cd /mnt/w/mORMot2/ex/dmvc/09-custom_auth
./bin/Win64/Debug/CustomAuthSample.exe
```

### 2. Test Public Endpoints

```bash
# Public API (no auth)
curl -X POST http://localhost:8080/root/PublicApi.Index \
  -H "Content-Type: application/json" \
  -d "{}"

# Public action in private controller
curl -X POST http://localhost:8080/root/PrivateApi.PublicAction \
  -H "Content-Type: application/json" \
  -d "{}"
```

### 3. Authenticate

```bash
# Login as admin
curl -X POST http://localhost:8080/root/Auth \
  -H "Content-Type: application/json" \
  -d '{"userName":"admin","password":"adminpass"}'

# Response (example):
# {
#   "result": 12345,
#   "data": {
#     "session_signature": "ABC123..."
#   }
# }
```

### 4. Test Protected Endpoints

Use the `session_signature` from authentication:

```bash
# Admin-only endpoint
curl -X POST http://localhost:8080/root/PrivateApi.Index \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer 12345+ABC123..." \
  -d "{}"

# Role1 endpoint (requires role1 or admin)
curl -X POST http://localhost:8080/root/PrivateApi.OnlyRole1 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer SESSION_ID+SIGNATURE" \
  -d "{}"
```

## Implementation Details

### Custom Authentication Handler

The `TCustomAuthenticationHandler` class validates users and populates their session with roles:

```pascal
function TCustomAuthenticationHandler.CheckPassword(
  Ctxt: TRestServerUriContext; User: TAuthUser;
  const aClientNonce, aPassWord: RawUtf8): boolean;
var
  roles: TRawUtf8DynArray;
begin
  // Validate credentials (database, LDAP, etc.)
  if (User.LogonName = 'admin') and (aPassWord = 'adminpass') then
  begin
    Result := True;
    AddRawUtf8(roles, 'admin');
    // Store roles in session
    User.Data := RawUtf8ArrayToCsv(roles);
  end;
  // ... more users
end;
```

### Authorization Check

The `OnMethodAuth` callback controls access to each method:

```pascal
function TCustomAuthServer.OnMethodAuth(Sender: TRestServer;
  Ctxt: TRestServerUriContext;
  const aInterfaceDotMethodName: RawUtf8): boolean;
var
  interfaceName, methodName: RawUtf8;
  roles: TRawUtf8DynArray;
begin
  Split(aInterfaceDotMethodName, '.', interfaceName, methodName);

  // Public API - no auth
  if interfaceName = 'IPublicApi' then
    Exit(True);

  // Private API - check authentication and roles
  if Ctxt.AuthUser = '' then
    Exit(False);  // Not authenticated

  // Get user roles from session
  CsvToRawUtf8DynArray(pointer(session.User.Data), roles);

  // Check if user has required role for this method
  // ...
end;
```

## Key Differences from DMVC

1. **Authentication Method**
   - DMVC: HTTP Basic Auth or custom header
   - mORMot2: Session-based with `/root/Auth` endpoint

2. **Routing**
   - DMVC: REST-style paths (`GET /private/role1`)
   - mORMot2: RPC-style (`POST /root/PrivateApi.OnlyRole1`)

3. **Session Management**
   - DMVC: Middleware handles session creation
   - mORMot2: Built-in `TAuthSession` and `TAuthUser` ORM

4. **Role Storage**
   - DMVC: In-memory `TList<string>`
   - mORMot2: Stored in `TAuthUser.Data` field as CSV

## Production Considerations

### Replace Hardcoded Users

In production, replace the static user validation with database or LDAP:

```pascal
function TCustomAuthenticationHandler.CheckPassword(...): boolean;
var
  userRecord: TOrmUser;
begin
  // Query database
  userRecord := TOrmUser.Create(Server.Orm,
    'LogonName=?', [User.LogonName]);
  try
    if userRecord.ID <> 0 then
    begin
      // Verify password hash
      Result := VerifyPassword(aPassWord, userRecord.PasswordHash);
      if Result then
        User.Data := userRecord.Roles;  // Load roles from DB
    end;
  finally
    userRecord.Free;
  end;
end;
```

### Use JWT for Stateless Auth

For stateless authentication, use JWT tokens instead of sessions:

```pascal
uses
  mormot.crypt.jwt;

// Generate JWT on successful login
var jwt: TJwtContent;
jwt.aud := ['myapp'];
jwt.data['userid'] := UserId;
jwt.data['roles'] := User.Data;
var token := jwt.Compute(['mysecret']);

// Validate JWT on each request
if Request.AuthenticationCheck(jwtAlgHS256) then
  // Extract roles from JWT
```

### Add HTTPS

Enable HTTPS for production:

```pascal
fHttpServer := TRestHttpServer.Create(
  aPort,
  [fServer],
  '+',
  useHttpAsync,
  32,
  secTLS,  // Enable HTTPS
  'cert.pem',
  'key.pem'
);
```

## Files

- `src/api.interfaces.pas` - Service interface definitions
- `src/api.impl.pas` - Service implementations
- `src/auth.handler.pas` - Custom authentication and authorization logic
- `src/server.pas` - Server setup and configuration
- `CustomAuthSample.dpr` - Main program

## References

- mORMot2 authentication: `mormot.rest.server` unit
- Session management: `TAuthSession`, `TAuthUser` classes
- JWT support: `mormot.crypt.jwt` unit

## See Also

- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#7-authentication) - Authentication patterns
- mORMot2 documentation: SAD (Software Architecture Design)
