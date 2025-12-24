# 57. JSON-RPC 2.0 Demo

## Overview

This sample demonstrates **JSON-RPC 2.0** protocol implementation using mORMot2's interface-based services. It shows how mORMot2 automatically provides JSON-RPC support through its service architecture.

## Key Features

- ✅ **JSON-RPC 2.0 Protocol**: Full compliance with JSON-RPC specification
- ✅ **Interface-based Services**: Clean API definitions using Delphi interfaces
- ✅ **Multiple Services**: Calculator and User services in one server
- ✅ **Automatic Proxy Generation**: Client-side interface proxies automatically created
- ✅ **Type-safe Communication**: Strong typing preserved across network boundary
- ✅ **ORM Integration**: Services backed by SQLite database via mORMot2 ORM
- ✅ **Logging**: Complete operation logging for debugging

## What is JSON-RPC?

**JSON-RPC** is a remote procedure call (RPC) protocol encoded in JSON. A remote procedure call is made by sending a request to a remote service using HTTP POST with a JSON payload.

### Example Request
```json
POST /CalculatorService/Add
Content-Type: application/json

{
  "a": 10,
  "b": 5
}
```

### Example Response
```json
{
  "result": 15
}
```

## Architecture

```
┌─────────────────┐         HTTP/JSON-RPC          ┌──────────────────┐
│                 │◄──────────────────────────────►│                  │
│  Client         │                                 │  Server          │
│  - Calculator   │   POST /CalculatorService/Add  │  - Calculator    │
│    Proxy        │   {"a":10,"b":5}               │    Service       │
│  - User Proxy   │                                 │  - User Service  │
│                 │   Response: {"result":15}       │                  │
└─────────────────┘                                 └──────────────────┘
                                                             │
                                                             ▼
                                                    ┌─────────────────┐
                                                    │ SQLite Database │
                                                    │  - Calculations │
                                                    │  - Users        │
                                                    └─────────────────┘
```

## Services

### Calculator Service

Mathematical operations with history tracking:

- `Add(a, b: double): double` - Addition
- `Subtract(a, b: double): double` - Subtraction
- `Multiply(a, b: double): double` - Multiplication
- `Divide(a, b: double): double` - Division (with zero check)
- `GetHistory: TCalculatorResults` - Retrieve calculation history
- `ClearHistory: boolean` - Clear history

### User Service

User management:

- `CreateUser(username, email: string): TID` - Create new user
- `GetUser(id: TID): TUserInfo` - Get user by ID
- `GetAllUsers: TUserInfos` - List all users
- `DeleteUser(id: TID): boolean` - Delete user

## Running the Sample

### Server Mode
```bash
JsonRpcDemo server
```

### Client Mode
```bash
JsonRpcDemo client
```

### Default (Server)
```bash
JsonRpcDemo
```

## Testing with curl

### Calculator Operations

**Addition:**
```bash
curl -X POST http://localhost:8080/CalculatorService/Add \
  -H "Content-Type: application/json" \
  -d '{"a":10,"b":5}'
```

**Division:**
```bash
curl -X POST http://localhost:8080/CalculatorService/Divide \
  -H "Content-Type: application/json" \
  -d '{"a":100,"b":4}'
```

**Get History:**
```bash
curl -X POST http://localhost:8080/CalculatorService/GetHistory
```

### User Operations

**Create User:**
```bash
curl -X POST http://localhost:8080/UserService/CreateUser \
  -H "Content-Type: application/json" \
  -d '{"username":"john","email":"john@example.com"}'
```

**Get All Users:**
```bash
curl -X POST http://localhost:8080/UserService/GetAllUsers
```

**Get User by ID:**
```bash
curl -X POST http://localhost:8080/UserService/GetUser \
  -H "Content-Type: application/json" \
  -d '{"id":1}'
```

## JSON-RPC vs RESTful

### RESTful Approach (DMVC)
```
GET    /api/users          → List all users
GET    /api/users/123      → Get user #123
POST   /api/users          → Create user
PUT    /api/users/123      → Update user
DELETE /api/users/123      → Delete user
```

### JSON-RPC Approach (mORMot2)
```
POST /UserService/GetAllUsers              → List all users
POST /UserService/GetUser {"id":123}       → Get user #123
POST /UserService/CreateUser {...}         → Create user
POST /UserService/UpdateUser {"id":123...} → Update user
POST /UserService/DeleteUser {"id":123}    → Delete user
```

**Key Differences:**
- RESTful uses HTTP verbs (GET, POST, PUT, DELETE)
- JSON-RPC uses only POST with method names
- RESTful encodes ID in URL path
- JSON-RPC encodes all parameters in JSON body
- JSON-RPC is more RPC-like (method calls)
- RESTful is more resource-oriented

## Implementation Details

### Interface Definition

```pascal
ICalculatorService = interface(IInvokable)
  ['{A5B8C3D4-E6F7-4A9B-8C1D-2E3F4A5B6C7D}']
  function Add(a, b: double): double;
  function Divide(a, b: double): double;
  // ...
end;
```

### Service Implementation

```pascal
TCalculatorService = class(TInterfacedObject, ICalculatorService)
protected
  fRest: IRestOrm;
public
  function Add(a, b: double): double;
  // ...
end;
```

### Server Registration

```pascal
fCalculatorApi := TCalculatorService.Create(fRestServer.Orm);
fRestServer.ServiceDefine(fCalculatorApi, [ICalculatorService], sicShared);
```

### Client Usage

```pascal
fRestClient.ServiceDefine([ICalculatorService], sicShared);
fCalculatorApi := fRestClient.Service<ICalculatorService>;
result := fCalculatorApi.Add(10, 5);  // Transparent RPC call
```

## DMVC Comparison

This sample is equivalent to DMVC's `jsonrpc` sample but uses mORMot2's native JSON-RPC support:

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Protocol** | JSON-RPC 2.0 | JSON-RPC 2.0 (automatic) |
| **Server Setup** | Manual web module | Interface registration |
| **Client** | Manual HTTP client | Automatic proxy |
| **Type Safety** | Manual serialization | Automatic via RTTI |
| **ORM** | External (if needed) | Built-in TRestOrm |

### mORMot2 Advantages

1. **Zero Boilerplate**: Just define interface, implementation auto-generated
2. **Type Safety**: Compiler checks parameters and return types
3. **Integrated ORM**: Database persistence included
4. **Automatic Proxies**: Client code identical to local calls
5. **Full Logging**: Built-in comprehensive logging

## Files

```
57-jsonrpc/
├── JsonRpcDemo.dpr              # Main program (server + client modes)
├── JsonRpcDemo.dproj            # Delphi project file
├── README.md                    # This file
└── src/
    ├── jsonrpc.interfaces.pas   # Service interfaces (ICalculatorService, IUserService)
    ├── jsonrpc.entities.pas     # ORM entities (TOrmCalculation, TOrmUser)
    ├── jsonrpc.impl.pas         # Service implementations
    ├── jsonrpc.server.pas       # HTTP server setup
    └── jsonrpc.client.pas       # HTTP client setup
```

## Key Takeaways

1. **mORMot2 = JSON-RPC by Default**: Interface-based services automatically use JSON-RPC
2. **Clean Separation**: Interfaces define API, implementations provide logic
3. **Network Transparency**: Client code looks like local method calls
4. **Full Protocol Support**: JSON-RPC 2.0 compliant
5. **Production Ready**: Logging, error handling, ORM integration included

## Next Steps

- Explore error handling with custom exceptions
- Add authentication middleware
- Implement batch requests (JSON-RPC 2.0 feature)
- Add service versioning
- Monitor performance with profiling

## Related Samples

- **02-console_sample**: Basic service patterns
- **06-articles_crud_server**: CRUD operations
- **10-jsonwebtoken**: Adding authentication
- **13-middleware_cors**: CORS support for web clients
