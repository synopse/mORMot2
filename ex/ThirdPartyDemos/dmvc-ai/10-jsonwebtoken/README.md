# mORMot2 JWT Authentication Demo

Port of DMVCFramework's jsonwebtoken sample, demonstrating JWT-based authentication with mORMot2.

## Overview

This sample demonstrates:
- **JWT Token Generation**: Login endpoint that issues JWT tokens (HS256 algorithm)
- **Token Verification**: Automatic JWT verification on protected endpoints
- **Role-based Access Control**: Different user roles with different permissions
- **Custom Claims**: Additional data embedded in JWT payload
- **Token Expiration**: Configurable token lifetime (1 hour default)
- **Public/Protected Endpoints**: Mix of authenticated and non-authenticated endpoints

## Comparison with DMVC

| Feature | DMVCFramework | mORMot2 Port |
|---------|---------------|--------------|
| JWT Library | MVCFramework.JWT | mormot.crypt.jwt (TJwtHS256) |
| Authentication | TMVCAuthenticationHandler | TRestServerAuthentication |
| Middleware | UseJWTMiddleware | Custom TJwtAuthenticationServer |
| Controllers | TMVCController with attributes | Interface-based services |
| Login Endpoint | /login (middleware auto-registers) | /auth/Login (explicit registration) |
| Token Validation | Middleware intercepts requests | RetrieveSession override |

## Project Structure

```
10-jsonwebtoken/
├── src/
│   ├── api.interfaces.pas    # Service interfaces (IPublicService, IProtectedService)
│   ├── api.impl.pas          # Service implementations
│   ├── auth.pas              # JWT authentication (TJwtAuthenticationServer)
│   └── server.pas            # Server setup (TJwtServer)
├── www/
│   └── index.html           # Web UI with API documentation
├── JwtServer.dpr            # Main program
├── JwtServer.dproj          # Delphi project file (D12)
└── README.md                # This file
```

## Features

### 1. JWT Token Generation

Login endpoint authenticates users and issues JWT tokens:

**Request:**
```bash
curl -X POST http://localhost:8080/auth/Login \
  -H "Content-Type: application/json" \
  -d '{"username":"user1","password":"user1"}'
```

**Response:**
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjdXN0b21rZXkxIjoiY3VzdG9tdmFsdWUxIiwiY3VzdG9ta2V5MiI6ImN1c3RvbXZhbHVlMiIsInJvbGVzIjpbInJvbGUxIl0sImlzcyI6Im1PUk1vdDIgSldUIERlbW8iLCJzdWIiOiJ1c2VyMSIsImV4cCI6MTYzNDU2Nzg5MCwiaWF0IjoxNjM0NTY0MjkwfQ.signature",
  "user": "user1",
  "roles": ["role1"]
}
```

**JWT Payload** (decoded):
```json
{
  "customkey1": "customvalue1",
  "customkey2": "customvalue2",
  "roles": ["role1"],
  "iss": "mORMot2 JWT Demo",
  "sub": "user1",
  "exp": 1634567890,
  "iat": 1634564290
}
```

### 2. Token Verification

Protected endpoints automatically verify JWT tokens from `Authorization: Bearer <token>` header:

**Request:**
```bash
TOKEN="eyJhbGc..."
curl http://localhost:8080/ProtectedService/GetUserInfo \
  -H "Authorization: Bearer $TOKEN"
```

**Response:**
```json
{
  "user": "user1",
  "roles": ["role1"],
  "customClaims": {
    "customkey1": "customvalue1",
    "customkey2": "customvalue2"
  }
}
```

### 3. Role-based Access Control

Three test users with different roles:

| Username | Password | Roles |
|----------|----------|-------|
| user1 | user1 | role1 |
| user2 | user2 | role2 |
| user3 | user3 | role1, role2 |

Protected endpoints check user roles:
- `/ProtectedService/GetUserInfo` - Requires role1
- `/ProtectedService/GetAdminInfo` - Requires role2

### 4. Public Endpoints

Some endpoints are accessible without authentication:

**Request:**
```bash
curl http://localhost:8080/PublicService/GetPublicMessage
```

**Response:**
```json
"This is a public section - no authentication required"
```

## Building and Running

### Build

```bash
# Using delphi-compiler tool
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe JwtServer.dproj

# Or manually
dcc32 JwtServer.dpr
```

### Run

```bash
./bin/Win32/Debug/JwtServer.exe
```

Output:
```
=======================================
mORMot2 JWT Authentication Demo
=======================================
Demonstrates:
  - JWT token generation (HS256)
  - Login endpoint issuing tokens
  - Protected endpoints with JWT verification
  - Role-based access control
  - Token expiration handling
=======================================

Server running on http://localhost:8080

Test Users (username=password):
  user1/user1 (role1)
  user2/user2 (role2)
  user3/user3 (role1, role2)

Try the API:
...
Press CTRL+C to stop the server
=======================================
```

## Testing the API

### 1. Access Public Endpoint (No Auth)

```bash
curl http://localhost:8080/PublicService/GetPublicMessage
```

Response: `"This is a public section - no authentication required"`

### 2. Login and Get JWT Token

```bash
curl -X POST http://localhost:8080/auth/Login \
  -H "Content-Type: application/json" \
  -d '{"username":"user1","password":"user1"}'
```

Response:
```json
{
  "token": "eyJhbGc...",
  "user": "user1",
  "roles": ["role1"]
}
```

**Save the token** for subsequent requests.

### 3. Access Protected Endpoint with Token

```bash
TOKEN="<token from step 2>"
curl http://localhost:8080/ProtectedService/GetUserInfo \
  -H "Authorization: Bearer $TOKEN"
```

Response:
```json
{
  "user": "user1",
  "roles": ["role1"],
  "customClaims": {
    "customkey1": "customvalue1",
    "customkey2": "customvalue2"
  }
}
```

### 4. Try Without Token (Should Fail)

```bash
curl http://localhost:8080/ProtectedService/GetUserInfo
```

Response: **401 Unauthorized**

### 5. Test Role-based Access

```bash
# user1 has role1, so this works:
curl http://localhost:8080/ProtectedService/GetUserInfo \
  -H "Authorization: Bearer $TOKEN"

# user1 does NOT have role2, so this should fail:
curl http://localhost:8080/ProtectedService/GetAdminInfo \
  -H "Authorization: Bearer $TOKEN"
```

### 6. Test Different Users

```bash
# Login as user3 (has both role1 and role2)
curl -X POST http://localhost:8080/auth/Login \
  -H "Content-Type: application/json" \
  -d '{"username":"user3","password":"user3"}'

# user3 can access both endpoints
TOKEN3="<token for user3>"
curl http://localhost:8080/ProtectedService/GetUserInfo \
  -H "Authorization: Bearer $TOKEN3"

curl http://localhost:8080/ProtectedService/GetAdminInfo \
  -H "Authorization: Bearer $TOKEN3"
```

## Key Implementation Details

### JWT Configuration

```pascal
// From auth.pas - TJwtAuthenticationServer.Create
fJwt := TJwtHS256.Create(
  fSecret,           // Secret key: 'mys3cr37'
  0,                 // PBKDF2 rounds (0 = direct secret)
  [jrcIssuer, jrcSubject, jrcExpirationTime, jrcIssuedAt],  // Required claims
  [],                // Optional claims
  3600);             // Expiration: 1 hour
```

### Token Generation

```pascal
// From auth.pas - TJwtAuthenticationServer.Login
token := fJwt.Compute(
  [
    'customkey1', 'customvalue1',
    'customkey2', 'customvalue2',
    'roles', variant(roles)
  ],
  'mORMot2 JWT Demo',  // Issuer
  username);           // Subject
```

### Token Verification

```pascal
// From auth.pas - TJwtAuthenticationServer.RetrieveSession
authHeader := Ctxt.InHeader['Authorization'];
// Parse "Bearer <token>"
token := ExtractToken(authHeader);

// Verify JWT
if fJwt.Verify(token, content) <> jwtValid then
  exit; // Authentication failed

// Extract user info from JWT
username := content.reg[jrcSubject];
roles := content.data['roles'];
```

## Key Differences from DMVC

### 1. Authentication Handler

**DMVC:**
```pascal
uses MVCFramework.Middleware.JWT;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  MVC.AddMiddleware(UseJWTMiddleware(
    TAuthenticationSample.Create,
    lClaimsSetup,
    'mys3cr37',
    '/login',
    [TJWTCheckableClaim.ExpirationTime, ...]
  ));
end;
```

**mORMot2:**
```pascal
uses mormot.crypt.jwt, mormot.rest.server;

fAuth := TJwtAuthenticationServer.Create(fRestServer, 'mys3cr37');
fRestServer.ServiceMethodRegisterPublishedMethods('auth', fAuth);
```

### 2. Protected Endpoints

**DMVC:**
```pascal
[MVCPath('/admin/role1')]
procedure TAdminController.OnlyRole1;
begin
  // Middleware auto-validates JWT
  ResponseStream.AppendLine('Hello ' + Context.LoggedUser.UserName);
end;
```

**mORMot2:**
```pascal
function TProtectedService.GetUserInfo: RawUtf8;
var
  user: TAuthUser;
begin
  // TJwtAuthenticationServer.RetrieveSession already validated JWT
  user := Server.SessionGetUser(ServiceContext.Request.Session);
  // Build response...
end;
```

### 3. JWT Verification Flow

**DMVC:**
1. Middleware intercepts request
2. Extracts `Authorization: Bearer <token>` header
3. Verifies JWT signature and expiration
4. Populates `Context.LoggedUser` with claims
5. Controller accesses authenticated user

**mORMot2:**
1. `TJwtAuthenticationServer.RetrieveSession` called by mORMot framework
2. Extracts `Authorization: Bearer <token>` header
3. Verifies JWT signature and expiration
4. Creates mORMot session with user/roles from JWT claims
5. Service accesses session via `ServiceContext.Request.Session`

## Security Notes

### Production Recommendations

1. **Secret Key**: Use strong, random secret (at least 32 bytes)
   ```pascal
   fSecret := 'CHANGE-THIS-TO-A-SECURE-RANDOM-KEY-AT-LEAST-32-BYTES';
   ```

2. **HTTPS Only**: Always use HTTPS in production to prevent token interception
   ```pascal
   fHttpServer := TRestHttpServer.Create(
     aPort,
     [fRestServer],
     '+',
     useHttpSocket,
     32,      // Thread pool size
     secTLS   // Enable TLS
   );
   ```

3. **Token Expiration**: Configure appropriate expiration based on use case
   ```pascal
   fJwt := TJwtHS256.Create(fSecret, 0, [...], [],
     900);  // 15 minutes for high-security APIs
   ```

4. **Refresh Tokens**: Implement refresh token mechanism for long-lived sessions

5. **Rate Limiting**: Add rate limiting on login endpoint to prevent brute force

6. **Revocation**: Implement token revocation (blacklist) for logout/compromise

### Demo Limitations

This sample uses simplified authentication for demonstration:
- **Username = Password**: For easy testing only
- **No Password Hashing**: Production should hash passwords
- **No Refresh Tokens**: Tokens expire after 1 hour, no renewal
- **No Revocation**: No way to invalidate tokens before expiration
- **No Rate Limiting**: Login endpoint has no brute force protection

## Advanced Features

### Custom JWT Claims

Add application-specific data to JWT payload:

```pascal
token := fJwt.Compute(
  [
    'userId', 12345,
    'email', 'user@example.com',
    'permissions', '["read","write"]',
    'customData', '{"foo":"bar"}'
  ],
  'MyApp',
  username
);
```

Access in service:

```pascal
var content: TJwtContent;
if fJwt.Verify(token, content) = jwtValid then
begin
  userId := content.data.I['userId'];
  email := content.data.U['email'];
end;
```

### Different JWT Algorithms

mORMot2 supports multiple JWT algorithms:

```pascal
// HS256 (HMAC-SHA256) - Default
fJwt := TJwtHS256.Create(secret, 0, [...], [], 3600);

// HS384 (HMAC-SHA384) - Stronger
fJwt := TJwtHS384.Create(secret, 0, [...], [], 3600);

// HS512 (HMAC-SHA512) - Strongest symmetric
fJwt := TJwtHS512.Create(secret, 0, [...], [], 3600);

// ES256 (ECDSA P-256) - Asymmetric
uses mormot.crypt.ecc;
fJwt := TJwtES256.Create([...]);

// RS256 (RSA-SHA256) - Asymmetric
uses mormot.crypt.rsa;
fJwt := TJwtRS256.Create([...]);
```

### Token Refresh

Implement refresh tokens for better UX:

```pascal
// Issue both access token (short-lived) and refresh token (long-lived)
accessToken := fJwt.Compute([...], 'MyApp', username);  // 15 min
refreshToken := GenerateRefreshToken(username);         // 7 days

// Store refresh token in database
StoreRefreshToken(username, refreshToken);

// Client uses refresh token to get new access token
function RefreshAccessToken(refreshToken: RawUtf8): RawUtf8;
begin
  if ValidateRefreshToken(refreshToken) then
    Result := fJwt.Compute([...], 'MyApp', GetUserFromRefreshToken(refreshToken));
end;
```

## What's Next?

See other samples for more features:
- **08-basicauth**: HTTP Basic authentication
- **09-custom_auth**: Custom authentication schemes
- **_template**: Template for creating new samples

## References

- **JWT RFC 7519**: https://tools.ietf.org/html/rfc7519
- **mORMot2 Cryptography**: `/mnt/w/mORMot2/src/crypt/CLAUDE.md`
- **mORMot2 REST Authentication**: `/mnt/w/mORMot2/src/rest/CLAUDE.md`
- **JWT Libraries**: `mormot.crypt.jwt.pas` - Complete JWT implementation

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
