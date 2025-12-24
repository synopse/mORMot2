# mORMot2 Basic Demo Server

Port of DMVC Framework's `basicdemo_server` sample to mORMot2.

## Overview

This project demonstrates how DMVC Framework REST endpoints map to mORMot2's interface-based services. It's a direct port of the DMVC `basicdemo_server` sample, showing the equivalence between the two frameworks.

## Project Structure

```
01-basicdemo_server/
├── src/
│   ├── entities.pas          # Domain entities (ORM classes)
│   ├── api.interfaces.pas    # API interface definitions and DTOs
│   ├── api.impl.pas          # API implementation
│   └── server.pas            # Server initialization and HTTP setup
├── 01-basicdemo_server.dpr   # Main program
├── 01-basicdemo_server.dproj # Delphi project file (D12)
└── README.md                 # This file
```

## Features

This basic demo includes:

- **Simple GET endpoint**: Returns a "Hello World" message with the current time
- **POST endpoint**: Echoes back posted JSON data with a server modification
- **Parameterized GET endpoint**: Divides two numbers passed as URL parameters
- **JSON responses**: All endpoints return JSON-formatted data
- **Console logging**: mORMot2's TSynLog for structured logging

## DMVC to mORMot2 Mapping

| DMVC Concept | mORMot2 Equivalent | Example |
|--------------|-------------------|---------|
| `TMVCController` | `TInterfacedObject` implementing service interface | `TBasicDemoApi` |
| `[MVCPath]` attribute | Interface method name | `HelloWorld` method |
| `[MVCHTTPMethod]` | Implied by method signature | GET/POST via JSON-RPC |
| `Render()` | Return value from method | `function HelloWorld: THelloWorldResponse` |
| `Context.Request.Body` | Method parameters | `function HelloWorldPost(const data: RawUtf8)` |
| `LogI()` / `Log.Info()` | `TSynLog.Add.Log()` | `TSynLog.Add.Log(sllInfo, ...)` |
| `TWebModule` | `TRestServerDB` + `TRestHttpServer` | `TBasicDemoServer` |

## Building and Running

### Compile

```bash
cd /mnt/w/mORMot2/ex/dmvc/01-basicdemo_server
dcc32 01-basicdemo_server.dpr
```

Or using the helper tool:

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\01-basicdemo_server\01-basicdemo_server.dproj
```

### Run

```bash
./01-basicdemo_server.exe
```

The server will start on http://localhost:8080

## Testing the Endpoints

### 1. Hello World (GET)

Returns a simple greeting with current time.

**DMVC:**
```bash
curl http://localhost:8080/hello
```

**mORMot2:**
```bash
curl http://localhost:8080/BasicDemoApi/HelloWorld
```

**Expected response:**
```json
{
  "message": "Hello World! It's 14:30:00 in the mORMot2 Land!",
  "time": "14:30:00"
}
```

### 2. Hello World POST (POST)

Echoes back the posted JSON with a modification.

**DMVC:**
```bash
curl -X POST http://localhost:8080/hello \
  -H "Content-Type: application/json" \
  -d '{"test":"value"}'
```

**mORMot2:**
```bash
curl -X POST http://localhost:8080/BasicDemoApi/HelloWorldPost \
  -H "Content-Type: application/json" \
  -d '{"data":"{\"test\":\"value\"}"}'
```

**Expected response:**
```json
{
  "data": "{\"test\":\"value\"}",
  "modified": "from server"
}
```

### 3. Division (GET with parameters)

Divides two numbers.

**DMVC:**
```bash
curl http://localhost:8080/div/10/2
```

**mORMot2:**
```bash
curl http://localhost:8080/BasicDemoApi/Divide?par1=10&par2=2
```

**Expected response:**
```json
{
  "result": 5.0
}
```

## Key Differences

### URL Routing

- **DMVC**: Uses `[MVCPath]` attributes for custom routes (`/hello`, `/div/$par1/$par2`)
- **mORMot2**: Uses interface-based routing (`/ServiceName/MethodName`)

### Request Handling

- **DMVC**: Controller methods access request via `Context.Request`
- **mORMot2**: Method parameters are automatically populated from JSON-RPC request

### Response Formatting

- **DMVC**: Uses `Render()` with various overloads for different content types
- **mORMot2**: Returns DTOs (records) that are automatically serialized to JSON

### Logging

- **DMVC**: Uses `LogI()`, `Log.Info()` from MVCFramework.Logger
- **mORMot2**: Uses `TSynLog.Add.Log()` with severity levels (sllInfo, sllError, etc.)

## Implementation Notes

### DTO Design

The port introduces DTOs (Data Transfer Objects) for structured responses:

```pascal
THelloWorldResponse = packed record
  Message: RawUtf8;
  Time: RawUtf8;
end;
```

This is more type-safe than DMVC's string-based `Render()` and allows compile-time verification of API contracts.

### Error Handling

Division by zero is handled with a standard exception:

```pascal
if par2 = 0 then
  raise EDivByZero.Create('Division by zero');
```

mORMot2 automatically converts exceptions to proper HTTP error responses.

### Static Files

The DMVC version includes static file middleware for serving HTML/images. This basic port focuses on REST API functionality only. For static file serving in mORMot2, see the `_template` sample.

## Original DMVC Code

The original DMVC code is located at:
- `/mnt/w/DMVCframework/samples/basicdemo_server/`

Key files ported:
- `App1MainControllerU.pas` → `api.impl.pas`
- `WebModuleUnit1.pas` → `server.pas`
- `BasicDemo.dpr` → `01-basicdemo_server.dpr`

## Learn More

- [mORMot2 Documentation](https://synopse.info/fossil/wiki?name=SQLite3+Framework)
- [DMVC Framework](https://github.com/danieleteti/delphimvcframework)
- [Template Sample](../_template/README.md) - Full-featured example with CRUD operations
- [mORMot2 GitHub](https://github.com/synopse/mORMot2)

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
