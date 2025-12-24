# 08-basicauth - HTTP Basic Authentication

## Overview

This example demonstrates HTTP Basic Authentication in mORMot2, ported from the DelphiMVCFramework `middleware_basicauthentication` sample.

## Features

- **HTTP Basic Authentication**: Parse and validate credentials from Authorization header
- **Role-Based Authorization**: Different endpoints require different roles
- **Public Endpoints**: Some endpoints accessible without authentication
- **Custom Authentication Logic**: Demo implementation with username=password validation

## Original DMVC Sample

**Source**: `/mnt/w/DMVCframework/samples/middleware_basicauthentication/`

**Key Components**:
- `TAuthenticationSample`: Authentication handler implementing `IMVCAuthenticationHandler`
- `TMVCBasicAuthenticationMiddleware`: Middleware to intercept requests and validate auth
- `TApp1MainController`: Public endpoints controller
- `TAdminController`: Protected endpoints requiring authentication

## mORMot2 Port Architecture

### Components

1. **`authentication.pas`**: Authentication and authorization logic
   - `TBasicAuthHandler`: Static class with authentication methods
   - `RequiresAuthentication()`: Check if endpoint needs auth
   - `Authenticate()`: Validate username/password, return roles
   - `Authorize()`: Check if user has required role for endpoint
   - `ParseBasicAuth()`: Parse HTTP Basic Auth header

2. **`api.interfaces.pas`**: Service interface definition
   - `IBasicAuthApi`: Interface with public and protected methods

3. **`api.impl.pas`**: Service implementation
   - `TBasicAuthApi`: Implementation of `IBasicAuthApi`
   - `SetAuthContext()`: Store current user and roles for request

4. **`server.pas`**: HTTP server with authentication filter
   - `TBasicAuthServer`: Server with `OnBeforeBody` filter
   - Authentication filter intercepts all requests before execution
   - Validates credentials and checks authorization

## Key Differences from DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| **Architecture** | Middleware pipeline | `OnBeforeBody` filter hook |
| **Authentication** | `IMVCAuthenticationHandler` interface | Custom `TBasicAuthHandler` class |
| **Authorization** | Attribute-based `[MVCRequiresAuthentication]` | Method name-based in filter |
| **User Context** | `Context.LoggedUser` | Instance variable in service |
| **Controllers** | `TMVCController` subclasses | Interface-based services |

## Test Credentials

The demo uses **username = password** validation (for demonstration only).

| Username | Password | Roles | Access |
|----------|----------|-------|--------|
| `user1` | `user1` | role1 | Can access OnlyRole1 endpoints |
| `user2` | `user2` | role2 | Can access OnlyRole2 endpoints |
| `user3` | `user3` | role1, role2 | Can access all protected endpoints |

## Endpoints

### Public (No Authentication Required)

- `GET /root/BasicAuthApi.PublicSection`
- `GET /root/BasicAuthApi.Index`

### Protected (Require Authentication)

- `GET /root/BasicAuthApi.OnlyRole1` (requires role1)
- `GET /root/BasicAuthApi.OnlyRole1Json?par1=value` (requires role1)
- `GET /root/BasicAuthApi.OnlyRole2` (requires role2)

## Testing

### Using curl

```bash
# Public endpoint (no authentication)
curl http://localhost:8080/root/BasicAuthApi.PublicSection

# Protected endpoint with valid credentials
curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole1

# Protected endpoint with wrong role (should return 403 Forbidden)
curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole2

# JSON endpoint with parameters
curl -u user1:user1 "http://localhost:8080/root/BasicAuthApi.OnlyRole1Json?par1=hello"

# No credentials on protected endpoint (should return 401 Unauthorized)
curl http://localhost:8080/root/BasicAuthApi.OnlyRole1

# Invalid credentials (should return 401 Unauthorized)
curl -u user1:wrongpassword http://localhost:8080/root/BasicAuthApi.OnlyRole1
```

### Using Browser

For public endpoints, just navigate to the URL. For protected endpoints, the browser will prompt for credentials.

## Implementation Notes

### Authentication Filter

The `OnBeforeBody` filter in `TBasicAuthServer`:

1. Extracts the method name from the URI
2. Checks if authentication is required for this method
3. Parses the `Authorization` header for Basic Auth credentials
4. Validates credentials using `TBasicAuthHandler.Authenticate()`
5. Checks authorization using `TBasicAuthHandler.Authorize()`
6. Sets user context in the service instance
7. Returns `True` to continue or `False` to reject with 401/403

### Base64 Decoding

HTTP Basic Auth sends credentials as `Authorization: Basic <base64(username:password)>`.

The `ParseBasicAuth` method:
- Checks for "Basic " prefix
- Decodes base64 payload using mORMot2's `Base64ToBin()`
- Splits on colon to extract username and password

### Role-Based Authorization

Roles are stored as `TRawUtf8DynArray` and checked using mORMot2's `FindRawUtf8()` function.

## Production Considerations

**⚠️ This is a DEMO implementation. For production use:**

1. **Never use username = password validation**
2. **Use proper password hashing** (bcrypt, Argon2, etc.)
3. **Store credentials securely** (database with hashed passwords)
4. **Consider HTTPS** (Basic Auth sends credentials in base64, not encrypted)
5. **Implement rate limiting** to prevent brute force attacks
6. **Use proper session management** (not re-authenticating on every request)
7. **Consider JWT tokens** instead of Basic Auth for APIs

## See Also

- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md#7-authentication) - Authentication patterns
- [mORMot2 Documentation](https://synopse.info/files/html/Synopse%20mORMot2%20Framework%20SAD%201.18.html#TITL_65) - Security and Authentication
- [RFC 7617](https://tools.ietf.org/html/rfc7617) - HTTP Basic Authentication

## Files

```
08-basicauth/
├── 08-basicauth.dpr          # Main program
├── 08-basicauth.dproj        # Delphi project file
├── README.md                 # This file
└── src/
    ├── api.interfaces.pas    # Service interface
    ├── api.impl.pas          # Service implementation
    ├── authentication.pas    # Authentication/authorization logic
    └── server.pas            # HTTP server with auth filter
```

## Compilation

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\08-basicauth\08-basicauth.dproj
```

## Running

```bash
cd /mnt/w/mORMot2/ex/dmvc/08-basicauth
./Win32/Debug/08-basicauth.exe
```

Server will start on `http://localhost:8080`. Press Enter to stop.
