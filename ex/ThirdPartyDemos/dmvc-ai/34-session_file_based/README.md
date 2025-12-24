# 34-session_file_based - File-Based Session Storage

**Port of**: DMVCFramework `samples/session_file_based`
**Difficulty**: Low
**Demonstrates**:
- File-based session persistence
- Custom authentication handler
- Session data storage
- Session file management

## Overview

This sample demonstrates how to implement file-based session persistence in mORMot2. Session data is automatically saved to JSON files and survives server restarts.

## Key Features

- ✅ **File persistence** - Sessions stored as JSON files
- ✅ **Automatic save/load** - Sessions saved on every access
- ✅ **Session directory** - Organized session file storage
- ✅ **Key-value storage** - Store arbitrary data in sessions
- ✅ **Session info** - Query session metadata

## DMVC → mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Notes |
|--------------|-------------------|-------|
| `TMVCSessionData` | `TAuthSession.User.Data` | Store as JSON object |
| File-based session provider | `TFileBasedAuthHandler` | Custom auth handler |
| `Session['key'] := value` | `doc.SetValue('key', value)` | Use TDocVariantData |
| Session file storage | `sessions/*.json` | Automatic file management |

## Implementation Details

### Custom Authentication Handler

```pascal
TFileBasedAuthHandler = class(TRestServerAuthenticationDefault)
  function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
end;
```

**Key points**:
- Overrides `RetrieveSession` to load from file
- Saves session data on every access
- Uses `TAuthSession.User.Data` for storage
- JSON format for easy debugging

### Session Storage Pattern

```pascal
// Store value
doc.InitJson(session.User.Data, JSON_FAST);
doc.SetValue('key', 'value');
session.User.Data := doc.ToJson;

// Load value
doc.InitJson(session.User.Data, JSON_FAST);
value := doc.Value['key'];
```

### File Format

Sessions are stored as JSON files:

```json
{
  "id": 1,
  "user": "user",
  "created": "2025-12-20T10:30:00",
  "data": {
    "name": "John Doe",
    "counter": 5
  }
}
```

## Usage

### Build and Run

```bash
# Compile
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\34-session_file_based\34-session_file_based.dproj

# Run
cd /mnt/w/mORMot2/ex/dmvc/34-session_file_based
./Win32/Debug/34-session_file_based.exe
```

### Testing

**1. Login (get session)**:
```bash
curl -X POST "http://localhost:8080/root/Auth?UserName=user" \
     -H "Content-Type: application/json" \
     -d '{"UserName":"user","PassWord":"password"}'
```

Response:
```json
{
  "result": 1,
  "data": {
    "session_signature": "0000000100ABC123..."
  }
}
```

**2. Store value**:
```bash
SESSION="1+0000000100ABC123..."

curl -X POST "http://localhost:8080/root/SessionApi.SetValue" \
     -H "Authorization: Bearer $SESSION" \
     -H "Content-Type: application/json" \
     -d '{"key":"name","value":"John Doe"}'
```

**3. Get value**:
```bash
curl -X POST "http://localhost:8080/root/SessionApi.GetValue" \
     -H "Authorization: Bearer $SESSION" \
     -H "Content-Type: application/json" \
     -d '{"key":"name"}'
```

**4. List keys**:
```bash
curl -X POST "http://localhost:8080/root/SessionApi.ListKeys" \
     -H "Authorization: Bearer $SESSION" \
     -H "Content-Type: application/json" \
     -d '{}'
```

**5. Session info**:
```bash
curl -X POST "http://localhost:8080/root/SessionApi.SessionInfo" \
     -H "Authorization: Bearer $SESSION" \
     -H "Content-Type: application/json" \
     -d '{}'
```

**6. Test persistence**:
```bash
# 1. Store value
curl -X POST "http://localhost:8080/root/SessionApi.SetValue" \
     -H "Authorization: Bearer $SESSION" \
     -d '{"key":"test","value":"persisted"}'

# 2. Stop server (Ctrl+C)

# 3. Restart server

# 4. Get value (should still be there!)
curl -X POST "http://localhost:8080/root/SessionApi.GetValue" \
     -H "Authorization: Bearer $SESSION" \
     -d '{"key":"test"}'
```

## Key Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Session storage** | Custom provider interface | `TAuthSession.User.Data` field |
| **File format** | Provider-specific | JSON (TDocVariantData) |
| **File location** | Configurable | `./sessions/` subfolder |
| **Auto-save** | Provider-dependent | On every session access |
| **Session ID** | String-based | Integer-based (TID) |

## Architecture

```
TFileBasedAuthHandler
  ├─ RetrieveSession() override
  │   ├─ Load from file
  │   └─ Save to file
  ├─ SaveSessionToFile()
  │   └─ Write JSON to sessions/session_N.json
  └─ LoadSessionFromFile()
      └─ Read JSON from sessions/session_N.json

TSessionApi (sicShared)
  ├─ SetValue() - Store in session.User.Data
  ├─ GetValue() - Retrieve from session.User.Data
  ├─ ListKeys() - List all keys
  ├─ DeleteValue() - Remove key
  ├─ ClearSession() - Clear all data
  └─ SessionInfo() - Get metadata
```

## Production Considerations

### Security
- ⚠️ Session files contain sensitive data - protect directory
- ✅ Use HTTPS in production
- ✅ Implement session timeout cleanup

### Performance
- ⚠️ File I/O on every request - consider caching
- ✅ Use memory sessions for high-traffic scenarios
- ✅ Consider Redis/database for scaling

### Cleanup
```pascal
// Add session timeout cleanup
procedure CleanupExpiredSessions;
var
  files: TFileNameDynArray;
  i: integer;
  age: TDateTime;
begin
  files := FileNames(sessionPath, '*.json');
  for i := 0 to high(files) do
  begin
    age := FileAgeToDateTime(files[i]);
    if Now - age > 1 then // 1 day old
      DeleteFile(files[i]);
  end;
end;
```

## See Also

- **35-sessions** - In-memory session management with per-session services
- **22-custom_role_auth** - Role-based authentication with sessions
- **10-jsonwebtoken** - Stateless JWT authentication (alternative to sessions)
- **CONVERSION-GUIDE.md** - Section 9: Session Management

## Files

```
34-session_file_based/
├── src/
│   ├── api.interfaces.pas      # Session API interface
│   ├── api.impl.pas            # Session API implementation
│   ├── auth.handler.pas        # File-based auth handler
│   └── server.pas              # Server setup
├── 34-session_file_based.dpr   # Main program
├── 34-session_file_based.dproj # Delphi project
└── README.md                   # This file
```

## Notes

- Session files are created in `./sessions/` relative to executable
- File format is JSON for easy debugging and portability
- Session data is loaded AND saved on every access
- Files are named `session_N.json` where N is the session ID
- No automatic cleanup - implement as needed for production
