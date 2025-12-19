# 21. Security

*Authentication, Authorization, and Process Safety*

mORMot implements a comprehensive security architecture through three complementary layers: process safety, authentication, and authorization. This chapter covers the security mechanisms built into the framework.

---

## 21.1. Security Overview

### 21.1.1. The Three Pillars

```
┌─────────────────────────────────────────────────────────────────┐
│                    Security Architecture                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │
│  │ Process       │  │ Authentication│  │ Authorization │        │
│  │ Safety        │  │ (Who?)        │  │ (What?)       │        │
│  ├───────────────┤  ├───────────────┤  ├───────────────┤        │
│  │ • Encryption  │  │ • Sessions    │  │ • Per-table   │        │
│  │ • ACID DB     │  │ • Signatures  │  │ • Per-service │        │
│  │ • Stateless   │  │ • SSPI/Kerb   │  │ • Per-method  │        │
│  │ • Type safety │  │ • JWT         │  │ • Groups      │        │
│  │ • Testing     │  │ • HTTP Basic  │  │ • Access bits │        │
│  └───────────────┘  └───────────────┘  └───────────────┘        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 21.1.2. Security Principles

| Principle | Implementation |
|-----------|----------------|
| Defense in depth | Multiple security layers |
| Least privilege | Group-based access rights |
| Fail secure | Reject by default |
| No security by obscurity | Published algorithms |
| Session management | Server-side session tracking |

---

## 21.2. Process Safety

### 21.2.1. Built-in Safety Mechanisms

mORMot provides process safety at every architectural level:

**Encryption:**
- AES-256 encryption for sensitive data
- HTTPS support for transport security
- Optional AES encryption over HTTP (deprecated, use TLS)

**Database Integrity:**
- SQLite3 ACID transactions
- Atomic operations
- Safe concurrent access

**Stateless Architecture:**
- Each request is independent
- Session tokens for state identification
- No server-side state dependency

**Type Safety:**
- Strong Pascal typing
- ORM type validation
- JSON schema enforcement

---

## 21.3. Authentication

### 21.3.1. Authentication Concepts

Authentication confirms user identity. mORMot supports multiple authentication schemes:

| Scheme | Class | Security | Use Case |
|--------|-------|----------|----------|
| mORMot Default | `TRestServerAuthenticationDefault` | ★★★★ | Delphi clients |
| SSPI/Kerberos | `TRestServerAuthenticationSspi` | ★★★★ | Windows domain |
| HTTP Basic | `TRestServerAuthenticationHttpBasic` | ★★ | Browser/legacy |
| None | `TRestServerAuthenticationNone` | ★ | Testing only |
| JWT | Via `JwtForUnauthenticatedRequest` | ★★★ | Public APIs |

### 21.3.2. Enabling Authentication

```pascal
uses
  mormot.rest.sqlite3,
  mormot.rest.server;

var
  Server: TRestServerDB;
begin
  // Create server WITH authentication enabled
  Server := TRestServerDB.Create(Model, 'data.db3', True);  // aHandleUserAuthentication = True
  try
    Server.CreateMissingTables;  // Creates AuthUser/AuthGroup tables
    // Server now requires authentication
  finally
    Server.Free;
  end;
end;
```

The `True` parameter enables:
- `TAuthUser` and `TAuthGroup` tables
- Session management
- Default authentication schemes

### 21.3.3. Authentication Classes

```
TRestServerAuthentication (abstract)
├── TRestServerAuthenticationSignedUri
│   ├── TRestServerAuthenticationDefault   → mORMot secure challenge
│   └── TRestServerAuthenticationSspi      → Windows SSPI/Kerberos │
├── TRestServerAuthenticationNone          → Weak (username only)
└── TRestServerAuthenticationHttpAbstract
    └── TRestServerAuthenticationHttpBasic → HTTP Basic (Base64)
```

---

## 21.4. Default mORMot Authentication

### 21.4.1. Challenge-Response Protocol

The default authentication uses a secure two-pass challenge:

```
Client                                    Server
  │                                         │
  │  GET /auth?UserName=John               │
  ├────────────────────────────────────────►│
  │                                         │
  │  {"result":"<hex_nonce>"}              │
  │◄────────────────────────────────────────┤
  │                                         │
  │  GET /auth?UserName=John&              │
  │      Password=<computed>&              │
  │      ClientNonce=<random>              │
  ├────────────────────────────────────────►│
  │                                         │
  │  {"result":"SessionID+PrivateKey",     │
  │   "logonname":"John"}                  │
  │◄────────────────────────────────────────┤
  │                                         │
  │  All requests now include:             │
  │  ?session_signature=XXXX               │
  ├────────────────────────────────────────►│
```

### 21.4.2. Password Computation

The password sent is computed as:

```
Password = SHA256(ModelRoot + ServerNonce + ClientNonce + UserName +
                  SHA256('salt' + PlainPassword))
```

This ensures:
- Password never transmitted in clear
- Replay attacks prevented by nonces
- Server doesn't store plain password

### 21.4.3. Session Signature

Each authenticated request includes a signature:

```
session_signature = Hexa8(SessionID) +
                   Hexa8(Timestamp) +
                   Hexa8(CRC32(SessionID + PrivateKey +
                               SHA256('salt' + Password) +
                               Timestamp + URL))
```

Example URL:
```
root/Customer/123?session_signature=0000004C000F6DD02E24541C
                                    ^^^^^^^^ ^^^^^^^^ ^^^^^^^^
                                    SessionID Timestamp Signature
```

### 21.4.4. Client Authentication

```pascal
uses
  mormot.rest.http.client;

var
  Client: TRestHttpClientWinHTTP;
begin
  Client := TRestHttpClientWinHTTP.Create('localhost', '8080', Model);
  try
    // Authenticate with username/password
    if not Client.SetUser('Admin', 'synopse') then
      raise Exception.Create('Authentication failed');

    // All subsequent requests are signed automatically
    Client.Orm.Retrieve(ID, Customer);
  finally
    Client.Free;
  end;
end;
```

### 21.4.5. Signature Algorithm Options

```pascal
// Configure signature algorithm (server-side)
(Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
  TRestServerAuthenticationSignedUri).Algorithm := suaSHA256;
```

Available algorithms:

| Algorithm | Speed | Security | Notes |
|-----------|-------|----------|-------|
| `suaCRC32` | ★★★★ | ★★ | Default, fast |
| `suaMD5` | ★★★ | ★★ | Legacy |
| `suaSHA256` | ★★ | ★★★★ | Recommended for high security |
| `suaSHA512` | ★ | ★★★★ | Highest security |
| `suaSHA3` | ★★ | ★★★★ | Modern |

### 21.4.6. Timestamp Tolerance

```pascal
// For AJAX clients with network latency
(Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
  TRestServerAuthenticationSignedUri).NoTimestampCoherencyCheck := true;

// Or adjust tolerance (default: 5 seconds)
(Server.AuthenticationRegister(TRestServerAuthenticationDefault) as
  TRestServerAuthenticationSignedUri).TimestampCoherencySeconds := 10;
```

---

## 21.5. Windows Authentication (SSPI)

### 21.5.1. SSPI Overview

SSPI allows using Windows credentials without entering username/password:

```pascal
uses
  mormot.rest.http.client;

var
  Client: TRestHttpClientWinHTTP;
begin
  Client := TRestHttpClientWinHTTP.Create('server', '8080', Model);
  try
    // Empty username = use Windows credentials
    Client.SetUser('', '');  // NTLM authentication

    // Or with Kerberos SPN
    Client.SetUser('', 'mymormotservice/myserver.mydomain.tld');
  finally
    Client.Free;
  end;
end;
```

### 21.5.2. Kerberos Setup

For Kerberos authentication, register an SPN:

```batch
rem Register SPN for service running under SYSTEM account
setspn -a mymormotservice/myserver.mydomain.tld myserver

rem Register SPN for service running under domain account
setspn -a mymormotservice/myserver.mydomain.tld myserviceaccount
```

### 21.5.3. User Mapping

SSPI-authenticated users must exist in `TAuthUser`:

```pascal
// User.LogonName must match 'DOMAIN\Username'
User := TAuthUser.Create;
try
  User.LogonName := 'MYDOMAIN\JohnDoe';
  User.DisplayName := 'John Doe';
  User.GroupRights := TAuthGroup(AdminGroupID);
  Server.Orm.Add(User, true);
finally
  User.Free;
end;
```

---

## 21.6. HTTP Basic Authentication

### 21.6.1. Browser Compatibility

For browser clients or legacy systems:

```pascal
// Server: Register HTTP Basic
Server.AuthenticationRegister(TRestServerAuthenticationHttpBasic);

// Client: Use HTTP Basic
TRestServerAuthenticationHttpBasic.ClientSetUser(Client, 'User', 'password');
```

**Warning:** HTTP Basic sends credentials as Base64 (not encrypted). Always use HTTPS!

### 21.6.2. Proxy-Only Authentication

For authentication without creating a mORMot session:

```pascal
// Client-side only, for proxy authentication
TRestServerAuthenticationHttpBasic.ClientSetUserHttpOnly(
  Client, 'proxyUser', 'proxyPass');
```

---

## 21.7. JWT Authentication

### 21.7.1. JWT for Public APIs

JWT provides stateless authentication for public APIs:

```pascal
uses
  mormot.crypt.jwt,
  mormot.rest.server;

var
  Server: TRestServerDB;
  JwtEngine: TJwtHS256;
begin
  Server := TRestServerDB.Create(Model, 'data.db3', False);  // No session auth

  // Configure JWT validation
  JwtEngine := TJwtHS256.Create(
    'my-secret-key-at-least-32-bytes!',  // Secret
    60000,                                 // Clock tolerance ms
    [jrcIssuer, jrcExpirationTime],       // Required claims
    [],                                    // Audience (optional)
    60                                     // Expiration minutes
  );
  Server.JwtForUnauthenticatedRequest := JwtEngine;  // Server owns it

  // Optionally restrict to specific IPs
  Server.JwtForUnauthenticatedRequestWhiteIP := '192.168.1.0/24';
end;
```

### 21.7.2. Client JWT Usage

```pascal
// Obtain JWT from your authentication service
var Token: RawUtf8 := GetJwtFromAuthService('user', 'pass');

// Set as HTTP header
Client.SessionHttpHeader := AuthorizationBearer(Token);

// All requests now include: Authorization: Bearer <token>
```

### 21.7.3. JWT Algorithms

| Class | Algorithm | Key Type |
|-------|-----------|----------|
| `TJwtHS256` | HMAC-SHA256 | Symmetric |
| `TJwtHS384` | HMAC-SHA384 | Symmetric |
| `TJwtHS512` | HMAC-SHA512 | Symmetric |
| `TJwtES256` | ECDSA P-256 | Asymmetric |
| `TJwtRS256` | RSA-SHA256 | Asymmetric |
| `TJwtPS256` | RSA-PSS-SHA256 | Asymmetric |

---

## 21.8. TAuthUser and TAuthGroup

### 21.8.1. TAuthGroup Structure

```pascal
TAuthGroup = class(TOrm)
published
  property Ident: RawUtf8;           // Group name ('Admin', 'User', etc.)
  property SessionTimeout: integer;   // Session timeout in minutes
  property AccessRights: RawUtf8;     // CSV-encoded TOrmAccessRights
end;
```

### 21.8.2. TAuthUser Structure

```pascal
TAuthUser = class(TOrm)
published
  property LogonName: RawUtf8;       // Login identifier
  property DisplayName: RawUtf8;      // Display name
  property PasswordHashHexa: RawUtf8; // SHA-256 hash of password
  property GroupRights: TAuthGroup;   // Associated group
  property Data: RawBlob;            // Custom application data
end;
```

### 21.8.3. Default Groups

When authentication is enabled, these groups are created automatically:

| Group | POST SQL | SELECT SQL | Auth R | Auth W | Tables R | Tables W | Services |
|-------|----------|------------|--------|--------|----------|----------|----------|
| Admin | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Supervisor | ✗ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ |
| User | ✗ | ✗ | ✗ | ✗ | ✓ | ✓ | ✓ |
| Guest | ✗ | ✗ | ✗ | ✗ | ✓ | ✗ | ✗ |

**Important:** Default password for all users is `synopse`. Change immediately in production!

### 21.8.4. Creating Custom Users

```pascal
var
  User: TAuthUser;
  Group: TAuthGroup;
begin
  // Find or create group
  Group := TAuthGroup.Create;
  try
    if not Server.Orm.Retrieve('Ident=?', [], ['CustomGroup'], Group) then
    begin
      Group.Ident := 'CustomGroup';
      Group.SessionTimeout := 30;
      Group.OrmAccessRights := SUPERVISOR_ACCESS_RIGHTS;
      Server.Orm.Add(Group, true);
    end;

    // Create user
    User := TAuthUser.Create;
    try
      User.LogonName := 'newuser';
      User.DisplayName := 'New User';
      User.SetPasswordPlain('secure_password');
      User.GroupRights := TAuthGroup(Group.ID);
      Server.Orm.Add(User, true);
    finally
      User.Free;
    end;
  finally
    Group.Free;
  end;
end;
```

### 21.8.5. Password Hashing

```pascal
// Simple SHA-256 (mORMot 1 compatible)
User.SetPasswordPlain('password');  // Uses SHA256('salt' + password)

// PBKDF2-HMAC-SHA256 (more secure)
User.SetPassword('password', 'unique-salt', 10000);  // 10000 rounds
```

---

## 21.9. Authorization

### 21.9.1. TOrmAccessRights

Authorization is controlled by `TOrmAccessRights`:

```pascal
TOrmAccessRights = record
  AllowRemoteExecute: TOrmAllowRemoteExecute;  // SQL/service flags
  GET: TOrmTableBits;    // Read access per table
  POST: TOrmTableBits;   // Create access per table
  PUT: TOrmTableBits;    // Update access per table
  DELETE: TOrmTableBits; // Delete access per table
end;
```

### 21.9.2. AllowRemoteExecute Flags

```pascal
TOrmAllowRemoteExecute = set of (
  reSQL,                    // Allow POST with SQL statements
  reSQLSelectWithoutTable,  // Allow complex SELECT (JOINs)
  reService,                // Allow interface-based services
  reUrlEncodedSQL,          // Allow SQL in URL parameters
  reUrlEncodedDelete,       // Allow DELETE with WHERE clause
  reOneSessionPerUser       // Enforce single session per user
);
```

### 21.9.3. Predefined Access Rights

```pascal
const
  // Full access (use only for local/in-process)
  FULL_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute: [reSQL, reSQLSelectWithoutTable,
                         reService, reUrlEncodedSQL,
                         reUrlEncodedDelete];
    GET: ALL_ACCESS_RIGHTS;
    POST: ALL_ACCESS_RIGHTS;
    PUT: ALL_ACCESS_RIGHTS;
    DELETE: ALL_ACCESS_RIGHTS;
  );

  // Admin access (remote, with SQL)
  ADMIN_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute: [reSQL, reSQLSelectWithoutTable, reService];
    // ...
  );

  // Supervisor access (remote, SELECT only)
  SUPERVISOR_ACCESS_RIGHTS: TOrmAccessRights = (
    AllowRemoteExecute: [reSQLSelectWithoutTable, reService];
    // ...
  );
```

### 21.9.4. Per-Table Access Control

```pascal
var
  Rights: TOrmAccessRights;
begin
  // Start with no access
  FillChar(Rights, SizeOf(Rights), 0);
  Rights.AllowRemoteExecute := [reService];

  // Grant full CRUD on Customer table
  Rights.Edit(Model, TCustomer, True, True, True, True);  // C, R, U, D

  // Grant read-only on Order table
  Rights.Edit(Model, TOrder, False, True, False, False);  // R only

  // Apply to group
  Group.OrmAccessRights := Rights;
end;
```

### 21.9.5. Service Authorization

For interface-based services:

```pascal
// Disable authentication for specific service
Server.ServiceDefine(TMyPublicService, [IMyPublicService], sicShared)
      .ByPassAuthentication := True;

// Or restrict to specific groups
Server.ServiceDefine(TMyAdminService, [IMyAdminService], sicShared)
      .AllowedGroups := [1];  // Group ID 1 only (Admin)
```

For method-based services:

```pascal
// Bypass authentication for specific method
Server.ServiceMethodByPassAuthentication('Timestamp');
Server.ServiceMethodByPassAuthentication('Auth');
```

---

## 21.10. Session Management

### 21.10.1. Session Storage

Sessions are stored in-memory as `TAuthSession` instances:

```pascal
TAuthSession = class(TSynPersistent)
  property ID: cardinal;              // Session identifier
  property User: TAuthUser;           // Associated user (loaded)
  property TimeOutTix: cardinal;      // Expiration tick
  property RemoteIP: RawUtf8;         // Client IP address
  property ConnectionID: TRestConnectionID;
end;
```

### 21.10.2. Session Lifecycle

```
┌──────────────────────────────────────────────────────────────┐
│                    Session Lifecycle                          │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Client.SetUser()  ──► Auth Request ──► Session Created       │
│         │                                    │                │
│         │                                    ▼                │
│         │                            ┌─────────────┐          │
│         │                            │ In-Memory   │          │
│         │                            │ TAuthSession│          │
│         │                            └─────────────┘          │
│         │                                    │                │
│         │                          ┌─────────┴─────────┐      │
│         │                          ▼                   ▼      │
│         │                    Session Timeout    Explicit Close│
│         │                          │                   │      │
│         │                          └─────────┬─────────┘      │
│         │                                    ▼                │
│         │                            Session Destroyed        │
│         │                                                     │
└──────────────────────────────────────────────────────────────┘
```

### 21.10.3. Session Timeout

Configure via `TAuthGroup.SessionTimeout`:

```pascal
// Set 30-minute timeout for a group
Group.SessionTimeout := 30;
Server.Orm.Update(Group);
```

### 21.10.4. Session Persistence

Sessions can be persisted for server restarts:

```pascal
// Save sessions before shutdown
Server.Shutdown('sessions.bin');

// Restore sessions after restart
Server.SessionsLoadFromFile('sessions.bin');
```

**Note:** This works for ORM sessions only, not SOA with stateful services.

### 21.10.5. Accessing Session Information

```pascal
// Server-side: Get current session user
var
  User: TAuthUser;
begin
  User := Server.SessionGetUser(Ctxt.SessionID);
  if User <> nil then
    Log('Request from: %s', [User.DisplayName]);
end;

// In service implementation
procedure TMyService.DoSomething;
var
  Ctxt: TRestServerUriContext;
begin
  Ctxt := ServiceRunningContext;
  if Ctxt <> nil then
    Log('Session ID: %d, User: %s',
        [Ctxt.SessionID, Ctxt.SessionUser.LogonName]);
end;
```

---

## 21.11. Security Best Practices

### 21.11.1. Transport Security

```pascal
// Always use HTTPS in production
HttpServer := TRestHttpServer.Create(
  '443',
  [Server],
  '+',
  useHttpAsync,
  secTLS           // Enable TLS
);

// Configure certificate
HttpServer.TLS.CertificateFile := 'server.crt';
HttpServer.TLS.PrivateKeyFile := 'server.key';
```

### 21.11.2. Password Management

```pascal
// ❌ Never store plain passwords
User.PasswordHashHexa := 'plaintext';

// ✓ Always hash passwords
User.SetPasswordPlain('password');  // SHA-256

// ✓ Or use PBKDF2 for better security
User.SetPassword('password', RandomString(16), 10000);
```

### 21.11.3. SQL Injection Prevention

```pascal
// ❌ Dangerous: SQL injection possible
Server.Orm.ExecuteFmt('SELECT * FROM Customer WHERE Name = ''%s''', [UserInput]);

// ✓ Safe: Use parameterized queries
Server.Orm.Retrieve('Name = ?', [], [UserInput], Customer);
```

### 21.11.4. Principle of Least Privilege

```pascal
// ❌ Don't give all users admin rights
User.GroupRights := TAuthGroup(AdminGroupID);

// ✓ Create specific groups with minimal rights
User.GroupRights := TAuthGroup(ReadOnlyGroupID);
```

### 21.11.5. Audit Logging

```pascal
// Enable detailed logging
Server.OnAfterUri := procedure(Ctxt: TRestServerUriContext)
begin
  if Ctxt.SessionUser <> nil then
    Log('%s: %s %s from %s', [
      DateTimeToIso8601(Now, True),
      Ctxt.SessionUser.LogonName,
      Ctxt.Uri,
      Ctxt.RemoteIP
    ]);
end;
```

---

## 21.12. Custom Authentication

### 21.12.1. Creating Custom Scheme

```pascal
type
  TRestServerAuthenticationCustom = class(TRestServerAuthentication)
  public
    function Auth(Ctxt: TRestServerUriContext;
      const aUserName: RawUtf8): boolean; override;
    function RetrieveSession(
      Ctxt: TRestServerUriContext): TAuthSession; override;
  end;

function TRestServerAuthenticationCustom.Auth(
  Ctxt: TRestServerUriContext;
  const aUserName: RawUtf8): boolean;
var
  User: TAuthUser;
  Token: RawUtf8;
begin
  Result := False;
  Token := Ctxt.InputUtf8['token'];

  // Validate token with your external service
  if not ValidateExternalToken(Token, aUserName) then
    Exit;

  // Create session
  User := GetUser(Ctxt, aUserName);
  if User <> nil then
  begin
    SessionCreate(Ctxt, User);
    Result := True;
  end;
end;
```

### 21.12.2. Registering Custom Scheme

```pascal
Server.AuthenticationRegister(TRestServerAuthenticationCustom);
```

### 21.12.3. Custom User Retrieval

```pascal
// Override user retrieval from external source
Server.OnAuthenticationUserRetrieve :=
  function(Sender: TRestServerAuthentication;
           Ctxt: TRestServerUriContext;
           const aUserName: RawUtf8): TAuthUser;
  begin
    Result := TAuthUser.Create;
    Result.LogonName := aUserName;
    Result.ID := GetUserIdFromLDAP(aUserName);
    Result.GroupRights := TAuthGroup(GetGroupFromLDAP(aUserName));
  end;
```

---

## 21.13. Summary

### 21.13.1. Authentication Quick Reference

| Need | Solution |
|------|----------|
| Delphi clients | `TRestServerAuthenticationDefault` |
| Windows domain | `TRestServerAuthenticationSspi` |
| Browser/REST API | `TRestServerAuthenticationHttpBasic` + HTTPS |
| Public stateless API | JWT via `JwtForUnauthenticatedRequest` |
| Development/testing | `TRestServerAuthenticationNone` |

### 21.13.2. Authorization Quick Reference

| Need | Property |
|------|----------|
| Per-table CRUD | `TOrmAccessRights.GET/POST/PUT/DELETE` |
| SQL execution | `AllowRemoteExecute` flags |
| Service access | `ByPassAuthentication`, `AllowedGroups` |
| Group management | `TAuthGroup.AccessRights` |

### 21.13.3. Key Units

| Unit | Purpose |
|------|---------|
| `mormot.rest.server` | Authentication classes, sessions |
| `mormot.rest.core` | `TAuthUser`, `TAuthGroup` |
| `mormot.orm.core` | `TOrmAccessRights` |
| `mormot.crypt.jwt` | JWT classes |
| `mormot.crypt.secure` | Cryptographic primitives |

---

*Next: Chapter 22 covers the Scripting Engine (if applicable to your mORMot2 version).*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 20: Hosting and Deployment](mORMot2-SAD-Chapter-20.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 22: Scripting Engine](mORMot2-SAD-Chapter-22.md) |
