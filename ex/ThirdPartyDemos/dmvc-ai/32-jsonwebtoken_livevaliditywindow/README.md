# 32-jsonwebtoken_livevaliditywindow - JWT Live Validity Window

**Port of**: DMVCFramework `samples/jsonwebtoken_livevaliditywindow`
**Difficulty**: Intermediate
**Demonstrates**: JWT token validation with SHORT expiration window for real-time validity checking

## Overview

This sample demonstrates JWT authentication with **live validity window checking**. Unlike traditional JWT implementations that set long expiration times (hours or days), this sample uses a **60-second validity window** to demonstrate:

1. **Real-time expiration validation** - Tokens expire quickly to show time-based validation
2. **Detailed expiration logging** - Server logs exactly when tokens expire and how long ago
3. **JWT claim validation** - Uses `exp` (expiration), `iat` (issued at), and `nbf` (not before) claims
4. **Role-based authorization** - Users have roles embedded in JWT tokens

This is useful for scenarios requiring high security where tokens should not remain valid for long periods.

## DMVC → mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `TMVCJWTAuthenticationMiddleware` | `TJwtLiveValidityAuth` (custom `TRestServerAuthentication`) | JWT middleware → custom auth handler |
| `MVCFramework.JWT` | `mormot.crypt.jwt` | JWT library |
| `TJWTCheckableClaims` | `TJwtContent.reg[]` | Standard JWT claims (exp, iat, nbf) |
| Controller attributes (`[MVCRequiresRole]`) | `ServiceDefine().SetAuthGroupByIndex()` | Role requirements |
| Middleware chain | `TRestServer.OnBeforeBody` callback | Request interception |
| Session custom data | `TAuthSession.PrivateData` | Custom claims storage |

## Implementation Details

### Key Components

#### 1. Live Validity Authentication (`auth.pas`)

```pascal
constructor TJwtLiveValidityAuth.Create(aServer: TRestServer;
  const aSecret: RawUtf8; aValidityWindowSeconds: Integer);
begin
  // Create JWT with SHORT expiration window
  fJwt := TJwtHS256.Create(fSecret, 0,
    [jrcIssuer, jrcSubject, jrcExpirationTime, jrcIssuedAt, jrcNotBefore],
    [],
    fValidityWindowSeconds); // 60 seconds for demonstration
end;
```

#### 2. Token Verification with Expiration Details

```pascal
function TJwtLiveValidityAuth.RetrieveSession(...): cardinal;
var
  verifyResult: TJwtResult;
  expiresAt: TDateTime;
  remainingSeconds: Integer;
begin
  verifyResult := fJwt.Verify(token, content);

  if verifyResult = jwtExpired then
  begin
    // Calculate how long ago it expired
    expiresAt := Iso8601ToDateTime(content.reg[jrcExpirationTime]);
    remainingSeconds := SecondsBetween(now, expiresAt);
    AuthenticationFailed(Ctxt,
      FormatUtf8('JWT token EXPIRED %s seconds ago', [remainingSeconds]));
  end;
end;
```

#### 3. Login Response with Expiration Info

```pascal
Ctxt.Returns(
  JsonEncode([
    'token', token,
    'expires_in_seconds', fValidityWindowSeconds,
    'issued_at', DateTimeToStr(now),
    'expires_at', DateTimeToStr(expiresAt),
    'message', 'Token is valid for 60 seconds. Use it quickly!'
  ]),
  HTTP_SUCCESS
);
```

### JWT Claims Used

| Claim | Type | Purpose |
|-------|------|---------|
| `iss` | Standard | Issuer (mORMot2 JWT Live Validity Demo) |
| `sub` | Standard | Subject (username) |
| `exp` | Standard | Expiration time (Unix timestamp) |
| `iat` | Standard | Issued at time (Unix timestamp) |
| `nbf` | Standard | Not before time (Unix timestamp) |
| `roles` | Custom | Array of role names |
| `customkey1` | Custom | Demo custom claim |
| `customkey2` | Custom | Demo custom claim |
| `valid_for_seconds` | Custom | Validity duration for reference |

## Usage

### 1. Build and Run

```bash
cd /mnt/w/mORMot2/ex/dmvc/32-jsonwebtoken_livevaliditywindow
delphi-compiler.exe JwtLiveValidityServer.dproj --config=Debug --platform=Win32
bin\Win32\Debug\JwtLiveValidityServer.exe
```

Server starts on `http://localhost:8080`

### 2. Login to Get JWT Token (Valid for 60 Seconds)

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
  "expires_in_seconds": 60,
  "issued_at": "2024-12-20 12:00:00",
  "expires_at": "2024-12-20 12:01:00",
  "message": "Token is valid for 60 seconds. Use it quickly!"
}
```

### 3. Access Protected Endpoint (Immediately)

```bash
curl http://localhost:8080/root/AdminRole1Api/ProtectedRole1 \
  -H "Authorization: Bearer YOUR_TOKEN_HERE"
```

**SUCCESS** - Token is still valid

### 4. Wait 60+ Seconds and Try Again

```bash
# Wait 65 seconds...
curl http://localhost:8080/root/AdminRole1Api/ProtectedRole1 \
  -H "Authorization: Bearer SAME_TOKEN"
```

**FAILURE** - Server responds:
```
HTTP 401 Unauthorized
JWT token EXPIRED 5 seconds ago (validity window was 60)
```

### 5. Access Public Endpoint (No Token Required)

```bash
curl http://localhost:8080/root/PublicApi/PublicInfo
```

Always succeeds - no authentication required.

## Available Endpoints

| Method | Path | Auth Required | Role Required | Description |
|--------|------|---------------|---------------|-------------|
| POST | `/root/auth/login` | No | - | Login and receive JWT token |
| GET | `/root/PublicApi/PublicInfo` | No | - | Public information |
| GET | `/root/AdminRole1Api/ProtectedRole1` | Yes | role1 | Protected content for role1 |
| GET | `/root/AdminRole1Api/ProtectedRole1Json` | Yes | role1 | Protected JSON for role1 |
| GET | `/root/AdminRole2Api/ProtectedRole2` | Yes | role2 | Protected content for role2 |

## Test Users

| Username | Password | Roles | Description |
|----------|----------|-------|-------------|
| `user1` | `user1` | `role1` | Can access role1 endpoints |
| `user2` | `user2` | `role2` | Can access role2 endpoints |
| `user3` | `user3` | `role1`, `role2` | Can access all endpoints |

## Key Differences from DMVC

### 1. Token Validity Configuration

**DMVC**: Validity configured in middleware setup
```pascal
JWT := TMVCJWTAuthenticationMiddleware.Create(
  TAuthentication.Create,
  'my-secret',
  '/login',
  [TJWTCheckableClaim.ExpirationTime, TJWTCheckableClaim.IssuedAt],
  60  // Seconds
);
```

**mORMot2**: Validity configured in JWT instance creation
```pascal
fJwt := TJwtHS256.Create(fSecret, 0,
  [jrcIssuer, jrcSubject, jrcExpirationTime, jrcIssuedAt, jrcNotBefore],
  [],
  60); // Seconds - last parameter
```

### 2. Expiration Validation

**DMVC**: Automatic via middleware claims check

**mORMot2**: Manual but with detailed feedback
```pascal
if verifyResult = jwtExpired then
  // Calculate exact expiration time and log details
```

### 3. Session Management

**DMVC**: Stateless (JWT only)

**mORMot2**: Hybrid - JWT validation creates server-side session for duration of request

## Testing Scenarios

### Scenario 1: Valid Token Usage
1. Login → Get token
2. Immediately access protected endpoint → SUCCESS
3. Token contains all claims and roles

### Scenario 2: Expired Token
1. Login → Get token
2. Wait 65 seconds
3. Try to access protected endpoint → FAILURE with detailed error

### Scenario 3: No Token
1. Try to access protected endpoint without Authorization header → FAILURE

### Scenario 4: Invalid Token
1. Access protected endpoint with malformed token → FAILURE

### Scenario 5: Wrong Role
1. Login as user1 (role1 only)
2. Try to access role2 endpoint → FORBIDDEN

## Logging Output

Server logs show detailed authentication flow:

```
20241220 12:00:00.000 INFO JWT live validity auth initialized: window=60 seconds
20241220 12:00:05.123 DEBUG Login attempt for user: user1
20241220 12:00:05.124 INFO User user1 logged in, session=1, expires in 60s at 2024-12-20 12:01:05
20241220 12:00:10.456 DEBUG JWT VALID for subject: user1 (expires in 55s)
20241220 12:01:10.789 WARNING JWT EXPIRED for subject: user1 (expired 5 seconds ago)
```

## Production Considerations

1. **Increase validity window** - 60 seconds is for demonstration only
   - Web apps: 15-60 minutes
   - Mobile apps: 1-24 hours
   - APIs: 5-15 minutes

2. **Token refresh strategy** - Implement token refresh endpoint
3. **Clock skew tolerance** - Add grace period for clock differences
4. **Secure secret** - Use strong, random secret key
5. **HTTPS only** - Never transmit JWT over HTTP in production

## See Also

- **10-jsonwebtoken** - Basic JWT authentication (longer expiration)
- **33-jsonwebtoken_roleauth** - JWT with role-based authorization (standard expiration)
- **DMVC Original**: `/mnt/w/DMVCframework/samples/jsonwebtoken_livevaliditywindow/`
- **mORMot2 JWT Docs**: `mormot.crypt.jwt.pas`

## References

- JWT Specification: RFC 7519
- JWT Claims: https://www.iana.org/assignments/jwt/jwt.xhtml
- mORMot2 JWT: https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html#TITLE_86
