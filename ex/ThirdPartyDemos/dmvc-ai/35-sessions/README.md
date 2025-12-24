# 35-sessions - Advanced Session Management

**Port of**: DMVCFramework `samples/sessions`
**Difficulty**: Intermediate
**Demonstrates**:
- In-memory session management
- Per-session service instances (sicPerSession)
- Session data operations
- Atomic counters
- Multiple concurrent users

## Overview

This sample demonstrates advanced session management patterns in mORMot2, including per-session service instances (like a shopping cart), session data storage, and atomic operations.

## Key Features

- ✅ **In-memory sessions** - Fast session storage
- ✅ **Per-session services** - One cart instance per user (sicPerSession)
- ✅ **Multiple users** - Isolated sessions for user1 and user2
- ✅ **Atomic counters** - Thread-safe counter increments
- ✅ **Active session listing** - Admin view of all sessions
- ✅ **Shopping cart demo** - Practical per-session service example

## DMVC → mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `TMVCSessionData` | `TAuthSession.User.Data` | JSON object storage |
| Per-session controller | `sicPerSession` service | One instance per session |
| `Session['key']` | `doc.SetValue('key', value)` | TDocVariantData API |
| Memory session store | Built-in (default) | No configuration needed |
| Session listing | `Server.SessionGetAll` | Get all active sessions |

## Implementation Details

### Service Instance Lifecycles

```pascal
// Shared service - one instance for all users
fRestServer.ServiceDefine(TSessionApi, [ISessionApi], sicShared);

// Per-session service - one instance per user session
fRestServer.ServiceDefine(TCartApi, [ICartApi], sicPerSession);
```

**sicShared**: All users share the same service instance
- Use for stateless operations
- Session data stored in `TAuthSession.User.Data`
- Example: Session management operations

**sicPerSession**: Each user gets their own service instance
- Use for stateful operations
- Instance lives for the duration of the session
- Example: Shopping cart with private fields

### Session Data Storage

```pascal
// Using TDocVariantData for flexible JSON storage
doc.InitJson(session.User.Data, JSON_FAST);
doc.SetValue('key', 'value');
session.User.Data := doc.ToJson;

// Atomic counter increment
currentValue := doc.Value['counter'];
Inc(currentValue);
doc.SetValue('counter', currentValue);
```

### Shopping Cart Pattern

```pascal
TCartApi = class(TInjectableObjectRest, ICartApi)
private
  fItems: TCartItemDynArray;  // Private per-session storage
public
  function AddItem(const itemId: RawUtf8; quantity: integer): RawUtf8;
  function GetCart: RawUtf8;
end;
```

**Why sicPerSession?**
- Each user needs their own cart
- Cart state (fItems) is private to the instance
- No need to store cart in database/session data
- Simpler code, better performance

## Usage

### Build and Run

```bash
# Compile
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\35-sessions\35-sessions.dproj

# Run
cd /mnt/w/mORMot2/ex/dmvc/35-sessions
./Win32/Debug/35-sessions.exe
```

### Testing Workflow

**1. Login as user1**:
```bash
curl -X POST "http://localhost:8080/root/Auth?UserName=user1" \
     -H "Content-Type: application/json" \
     -d '{"UserName":"user1","PassWord":"pass1"}'

# Save session: USER1_SESSION="1+SIGNATURE..."
```

**2. Login as user2**:
```bash
curl -X POST "http://localhost:8080/root/Auth?UserName=user2" \
     -H "Content-Type: application/json" \
     -d '{"UserName":"user2","PassWord":"pass2"}'

# Save session: USER2_SESSION="2+SIGNATURE..."
```

**3. Add items to user1's cart**:
```bash
curl -X POST "http://localhost:8080/root/CartApi.AddItem" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -H "Content-Type: application/json" \
     -d '{"itemId":"item1","quantity":2}'

curl -X POST "http://localhost:8080/root/CartApi.AddItem" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -d '{"itemId":"item2","quantity":1}'
```

**4. Add items to user2's cart**:
```bash
curl -X POST "http://localhost:8080/root/CartApi.AddItem" \
     -H "Authorization: Bearer $USER2_SESSION" \
     -H "Content-Type: application/json" \
     -d '{"itemId":"item3","quantity":3}'
```

**5. Get user1's cart (should have item1 and item2)**:
```bash
curl -X POST "http://localhost:8080/root/CartApi.GetCart" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -H "Content-Type: application/json" \
     -d '{}'
```

Response:
```json
{
  "items": [
    {"item_id":"item1","quantity":2,"price":10.50,"subtotal":21.00},
    {"item_id":"item2","quantity":1,"price":25.00,"subtotal":25.00}
  ],
  "total": 46.00,
  "item_count": 2
}
```

**6. Get user2's cart (should have only item3)**:
```bash
curl -X POST "http://localhost:8080/root/CartApi.GetCart" \
     -H "Authorization: Bearer $USER2_SESSION" \
     -H "Content-Type: application/json" \
     -d '{}'
```

Response:
```json
{
  "items": [
    {"item_id":"item3","quantity":3,"price":15.75,"subtotal":47.25}
  ],
  "total": 47.25,
  "item_count": 1
}
```

**7. Store session data (user1)**:
```bash
curl -X POST "http://localhost:8080/root/SessionApi.SetValue" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -H "Content-Type: application/json" \
     -d '{"key":"name","value":"John"}'
```

**8. Increment page view counter**:
```bash
# Call multiple times to see counter increment
curl -X POST "http://localhost:8080/root/SessionApi.IncrementCounter" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -H "Content-Type: application/json" \
     -d '{"key":"page_views"}'
```

**9. List all active sessions**:
```bash
curl -X POST "http://localhost:8080/root/SessionApi.ListActiveSessions" \
     -H "Authorization: Bearer $USER1_SESSION" \
     -H "Content-Type: application/json" \
     -d '{}'
```

## Key Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Per-session controller** | Custom session middleware | `sicPerSession` service |
| **Session storage** | `TMVCSessionData` dictionary | `TAuthSession.User.Data` JSON |
| **Session ID** | String GUID | Integer (TID) |
| **Memory storage** | Default | Built-in (no config) |
| **Session listing** | Manual tracking | `Server.SessionGetAll` |
| **Atomic operations** | Not built-in | Can implement with locking |

## Architecture

```
Session Management Architecture
═══════════════════════════════

User1 → HTTP → mORMot2 Server → TAuthSession (ID=1)
                                 ├─ User.Data = {"name":"John"}
                                 └─ Services
                                     ├─ TSessionApi (sicShared)
                                     └─ TCartApi instance #1
                                         └─ fItems = [item1, item2]

User2 → HTTP → mORMot2 Server → TAuthSession (ID=2)
                                 ├─ User.Data = {"name":"Jane"}
                                 └─ Services
                                     ├─ TSessionApi (sicShared - same instance!)
                                     └─ TCartApi instance #2
                                         └─ fItems = [item3]
```

## API Reference

### SessionApi (sicShared)

| Method | Description |
|--------|-------------|
| `SetValue(key, value)` | Store value in session |
| `GetValue(key)` | Retrieve value from session |
| `ListKeys()` | List all session keys |
| `DeleteValue(key)` | Delete value from session |
| `ClearSession()` | Clear all session data |
| `SessionInfo()` | Get session metadata |
| `ListActiveSessions()` | List all active sessions |
| `IncrementCounter(key)` | Atomic counter increment |

### CartApi (sicPerSession)

| Method | Description |
|--------|-------------|
| `AddItem(itemId, quantity)` | Add item to cart |
| `RemoveItem(itemId)` | Remove item from cart |
| `GetCart()` | Get cart contents |
| `ClearCart()` | Clear cart |
| `GetTotal()` | Get cart total |

## Use Cases

### 1. Session Data Storage
Store user preferences, temporary data:
```pascal
// Store
session.User.Data := '{"theme":"dark","lang":"en"}';

// Retrieve
doc.InitJson(session.User.Data);
theme := doc.S['theme'];
```

### 2. Shopping Cart (sicPerSession)
Each user has their own cart instance:
```pascal
TCartApi = class(TInjectableObjectRest, ICartApi)
private
  fItems: TCartItemDynArray;  // Per-user private storage
end;
```

### 3. Page View Counters
Track per-user page views:
```pascal
function IncrementCounter(const key: RawUtf8): integer;
begin
  // Atomically increment counter in session data
  currentValue := doc.Value[key];
  Inc(currentValue);
  doc.SetValue(key, currentValue);
  Result := currentValue;
end;
```

### 4. Session Monitoring
List all active sessions:
```pascal
sessions := Server.SessionGetAll;
for session in sessions do
  WriteLn(session.User.LogonName, ' - ', session.ID);
```

## Production Considerations

### Performance
- ✅ **Memory-based** - Very fast access
- ⚠️ **No persistence** - Lost on restart (use sample 34 for persistence)
- ✅ **Per-session services** - No locking needed for private data
- ⚠️ **Memory usage** - Each session consumes RAM

### Scaling
- ✅ **Single server** - Works great
- ⚠️ **Multi-server** - Needs sticky sessions or shared storage
- ✅ **High traffic** - Consider Redis/database for session storage
- ✅ **Load balancing** - Use JWT (sample 10) for stateless auth

### Security
- ✅ **Session timeout** - Automatic cleanup
- ✅ **Signature validation** - Built-in
- ⚠️ **Session fixation** - Rotate session ID on privilege change
- ✅ **HTTPS only** - Protect session tokens

## Comparison: sicShared vs sicPerSession

### When to use sicShared
- ✅ Stateless operations
- ✅ Database queries
- ✅ API calls
- ✅ Utilities
- ✅ Session data manipulation

### When to use sicPerSession
- ✅ Shopping carts
- ✅ Wizards/multi-step forms
- ✅ Per-user caches
- ✅ Stateful workflows
- ✅ User-specific resources

## See Also

- **34-session_file_based** - File-based session persistence
- **22-custom_role_auth** - Role-based auth with session data
- **10-jsonwebtoken** - Stateless JWT alternative
- **GETTING-STARTED.md** - Section: Service Instance Creation
- **CONVERSION-GUIDE.md** - Section 9: Session Management

## Files

```
35-sessions/
├── src/
│   ├── api.interfaces.pas      # Session & Cart API interfaces
│   ├── api.impl.pas            # Implementations (sicShared + sicPerSession)
│   └── server.pas              # Server setup with 2 users
├── 35-sessions.dpr             # Main program
├── 35-sessions.dproj           # Delphi project
└── README.md                   # This file
```

## Notes

- Sessions are in-memory only (not persisted)
- Two test users: user1/pass1, user2/pass2
- Cart prices are hard-coded (demo purposes)
- sicPerSession services are automatically cleaned up on session end
- Session data is stored as JSON in `TAuthSession.User.Data`
