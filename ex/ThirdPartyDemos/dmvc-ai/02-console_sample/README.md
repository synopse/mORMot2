# mORMot2 Console Sample

Port of DMVCFramework's basicdemo_server, demonstrating a simple console REST server with mORMot2.

## Overview

This sample demonstrates:
- **Console Application**: Simple command-line REST server
- **CTRL+C Handling**: Graceful shutdown on termination signal
- **Basic Logging**: Using mORMot2's TSynLog for application logging
- **REST API**: Simple greeting service with CRUD operations

## Comparison with DMVC

| Feature | DMVCFramework | mORMot2 Port |
|---------|---------------|--------------|
| Server Type | TIdHTTPWebBrokerBridge | TRestHttpServer |
| Shutdown | WaitForTerminationSignal | ConsoleCtrlHandler + Terminated flag |
| Logging | MVCFramework.Logger.LogI | TSynLog.Add.Log |
| Controllers | TMVCController with attributes | Interface-based services |
| Database | - | SQLite3 (built-in ORM) |

## Project Structure

```
02-console_sample/
├── src/
│   ├── entities.pas          # ORM entities (TOrmGreeting)
│   ├── api.interfaces.pas    # API interface (IGreetingService)
│   ├── api.impl.pas          # API implementation (TGreetingService)
│   └── server.pas            # Server class (TConsoleSampleServer)
├── ConsoleSample.dpr         # Main program with CTRL+C handling
├── ConsoleSample.dproj       # Delphi project file (D12)
└── README.md                 # This file
```

## Features

### 1. Console Server
Simple console application that starts an HTTP server on port 8080.

### 2. Graceful Shutdown
Press CTRL+C to trigger graceful shutdown:
- Signal handler captures CTRL+C
- Server is properly destroyed
- Database connections closed
- Clean exit

### 3. Basic Logging
All operations logged using TSynLog:
- Server creation and shutdown
- API calls (create, get, delete)
- Error conditions
- Logs saved to file automatically

### 4. REST API
Simple greeting service demonstrating CRUD operations:

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | /GreetingService/CreateGreeting | Create new greeting |
| GET | /GreetingService/GetGreeting?id=n | Get greeting by ID |
| GET | /GreetingService/GetAllGreetings | Get all greetings |
| POST | /GreetingService/DeleteGreeting | Delete greeting by ID |

## Building and Running

### Build
```bash
# Using delphi-compiler tool
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe ConsoleSample.dproj

# Or manually
dcc32 ConsoleSample.dpr
```

### Run
```bash
./Win32/Debug/ConsoleSample.exe
```

Output:
```
=======================================
mORMot2 Console Sample Server
=======================================
Simple REST API demonstrating:
  - Console application server
  - Graceful CTRL+C shutdown
  - Basic logging
  - REST endpoints
=======================================

Server is running on http://localhost:8080

Try the API:
  curl -X POST http://localhost:8080/GreetingService/CreateGreeting \
    -H "Content-Type: application/json" \
    -d '{"name":"John","message":"Hello World"}'

  curl http://localhost:8080/GreetingService/GetAllGreetings

Press CTRL+C to stop the server
=======================================
```

## Testing the API

### Create a Greeting
```bash
curl -X POST http://localhost:8080/GreetingService/CreateGreeting \
  -H "Content-Type: application/json" \
  -d '{"name":"John","message":"Hello World"}'
```

Response:
```json
{"result":1}
```

### Get All Greetings
```bash
curl http://localhost:8080/GreetingService/GetAllGreetings
```

Response:
```json
{
  "result": [
    {
      "id": 1,
      "name": "John",
      "msg": "Hello World",
      "created": 1734657600000
    }
  ]
}
```

### Get Greeting by ID
```bash
curl "http://localhost:8080/GreetingService/GetGreeting?id=1"
```

Response:
```json
{
  "result": {
    "id": 1,
    "name": "John",
    "msg": "Hello World",
    "created": 1734657600000
  }
}
```

### Delete Greeting
```bash
curl -X POST http://localhost:8080/GreetingService/DeleteGreeting \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

Response:
```json
{"result":true}
```

## Key Differences from DMVC

### 1. Signal Handling
**DMVC:**
```pascal
uses MVCFramework.Signal;

WaitForTerminationSignal;
EnterInShutdownState;
```

**mORMot2:**
```pascal
{$ifdef OSWINDOWS}
SetConsoleCtrlHandler(@ConsoleCtrlHandler, true);
{$endif OSWINDOWS}

while not Terminated do
  Sleep(100);
```

### 2. Logging
**DMVC:**
```pascal
uses MVCFramework.Logger;

LogI('Server started on port 8080');
Log.Info('Message', 'category');
```

**mORMot2:**
```pascal
uses mormot.core.log;

TSynLog.Family.Level := LOG_VERBOSE;
TSynLog.Add.Log(sllInfo, 'Server started on port 8080');
```

### 3. Controller vs Service
**DMVC:**
```pascal
[MVCPath('/hello')]
[MVCHTTPMethod([httpGET])]
procedure TController.HelloWorld;
```

**mORMot2:**
```pascal
IGreetingService = interface(IInvokable)
  function CreateGreeting(const name, message: RawUtf8): TGreetingID;
end;
```

## What's Next?

See other samples for more features:
- **01-tdd-service**: Test-Driven Development patterns
- **_template**: Template for creating new samples

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).
