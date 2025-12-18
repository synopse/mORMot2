# 11. Client-Server Architecture

*Communication Layers and Protocols*

mORMot provides a flexible Client-Server architecture supporting multiple communication protocols. This chapter covers the protocol options, class hierarchy, and configuration patterns.

---

## 11.1. Architecture Overview

### 11.1.1. Communication Layers

```
┌─────────────────────────────────────────────────────────────────┐
│                       Your Application                          │
└─────────────────────────────────────────────────────────────────┘
                               │
┌─────────────────────────────────────────────────────────────────┐
│                    TRest (Abstract Parent)                      │
│    ├── Orm: IRestOrm        (Object-Relational Mapping)         │
│    ├── Services             (Service-Oriented Architecture)     │
│    └── Run: TRestRunThreads (Threading)                         │
└─────────────────────────────────────────────────────────────────┘
           │                                    │
┌──────────────────────┐          ┌──────────────────────┐         
│    TRestClient       │          │    TRestServer                │
│  (Client-side)       │          │  (Server-side)                │
└──────────────────────┘          └──────────────────────┘         
           │                                    │
┌─────────────────────────────────────────────────────────────────┐
│                      Transport Layer                            │
│  ┌──────────┐  ┌─────────┐  ┌──────────┐  ┌────────────────┐    │
│  │In-Process│  │  HTTP   │  │WebSockets│  │Named Pipes/Msg │    │
│  └──────────┘  └─────────┘  └──────────┘  └────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

### 11.1.2. Protocol Comparison

| Protocol | Speed | Scaling | Hosting | Use Case |
|----------|-------|---------|---------|----------|
| In-Process | ★★★★ | ★★★★ | Same process | Testing, embedded |
| HTTP | ★★ | ★★★★ | Remote | Production servers |
| WebSockets | ★★★ | ★★★ | Remote | Bidirectional callbacks |
| Named Pipes | ★★★ | ★ | Local | Windows services |

### 11.1.3. Access Methods

| Method | Best For | Considerations |
|--------|----------|----------------|
| SOA Interfaces | Public/private services | Recommended, full features |
| SOA Methods | Full REST control | More verbose |
| MVC Web | Dynamic websites | HTML-oriented |
| ORM REST | Testing, internal | Not for public APIs |

---

## 11.2. Server Classes

### 11.2.1. Class Hierarchy

```
TRest (abstract)
└── TRestServer (abstract)
    ├── TRestServerDB           → SQLite3 backend
    ├── TRestServerFullMemory   → In-memory (no SQLite3)
    └── TRestServerRemoteDB     → Proxy to remote ORM
```

### 11.2.2. TRestServerDB (SQLite3)

The primary server class with full SQLite3 database:

```pascal
uses
  mormot.orm.core,
  mormot.rest.sqlite3;

var
  Model: TOrmModel;
  Server: TRestServerDB;
begin
  Model := TOrmModel.Create([TOrmCustomer, TOrmOrder]);
  Server := TRestServerDB.Create(Model, 'data.db3');
  try
    Server.CreateMissingTables;
    // Server ready...
  finally
    Server.Free;
    Model.Free;
  end;
end;
```

### 11.2.3. TRestServerFullMemory (No SQLite3)

Lightweight server without SQLite3 dependency:

```pascal
uses
  mormot.rest.memserver;

var
  Server: TRestServerFullMemory;
begin
  Server := TRestServerFullMemory.Create(Model);
  try
    // Fast in-memory storage
    // Can persist to JSON/binary files
    Server.Flush('backup.json');
  finally
    Server.Free;
  end;
end;
```

**Use cases:**
- Testing without database setup
- Simple CRUD + authentication
- Services with minimal ORM needs

### 11.2.4. TRestServerRemoteDB (Proxy)

> **⚠️ DEPRECATED**: This class does not exist in current mORMot2 codebase.
>
> For proxy server scenarios, consider:
> - Using method-based services for custom routing
> - Implementing interface-based services with delegation
> - Using TRestServer with custom URI routing

**Historical documentation (for reference only):**

Proxies ORM operations to another server:

```pascal
// NOTE: This code is conceptual - TRestServerRemoteDB does not exist
uses
  mormot.rest.core;

var
  RemoteClient: TRestHttpClient;
  ProxyServer: TRestServerRemoteDB;
begin
  RemoteClient := TRestHttpClientWinHTTP.Create('dbserver', '8080', Model);
  ProxyServer := TRestServerRemoteDB.Create(RemoteClient);
  // ProxyServer forwards ORM to RemoteClient
end;
```

**Historical use cases:**
- DMZ deployment (public proxy → internal database)
- Service aggregation
- Load distribution

---

## 11.3. Client Classes

### 11.3.1. Class Hierarchy

```
TRest (abstract)
└── TRestClientUri (abstract)
    ├── TRestClientDB              → Direct SQLite3 access
    ├── TRestClientLibraryRequest  → In-process DLL
    └── TRestHttpClientGeneric     → HTTP transport
        ├── TRestHttpClientSocket     → Raw sockets
        ├── TRestHttpClientWinHTTP    → WinHTTP API (recommended)
        ├── TRestHttpClientWinINet    → WinINet API
        ├── TRestHttpClientCurl       → libcurl (cross-platform)
        └── TRestHttpClientWebsockets → WebSocket upgrade
```

### 11.3.2. In-Process Client (TRestClientDB)

Direct access without network overhead:

```pascal
uses
  mormot.rest.sqlite3;

var
  Client: TRestClientDB;
begin
  // Creates internal TRestServerDB
  Client := TRestClientDB.Create(Model, nil, 'data.db3', TRestServerDB);
  try
    Client.Orm.Add(Customer, True);
  finally
    Client.Free;
  end;
end;
```

### 11.3.3. HTTP Clients

```pascal
uses
  mormot.rest.http.client;

var
  Client: TRestHttpClientWinHTTP;
begin
  // Recommended HTTP client for Windows
  Client := TRestHttpClientWinHTTP.Create('localhost', '8080', Model);
  try
    if Client.SetUser('user', 'password') then
      Client.Orm.Retrieve(123, Customer);
  finally
    Client.Free;
  end;
end;
```

**Client comparison:**

| Class | Platform | HTTPS | Speed | Notes |
|-------|----------|-------|-------|-------|
| `TRestHttpClientWinHTTP` | Windows | ✓ | Fast | **Recommended** |
| `TRestHttpClientWinINet` | Windows | ✓ | Medium | IE proxy integration |
| `TRestHttpClientSocket` | Cross-platform | ✗ | Fastest | No SSL, raw sockets |
| `TRestHttpClientCurl` | Cross-platform | ✓ | Fast | Requires libcurl |

### 11.3.4. WebSocket Client

For bidirectional communication:

```pascal
uses
  mormot.rest.http.client;

var
  Client: TRestHttpClientWebsockets;
begin
  Client := TRestHttpClientWebsockets.Create('localhost', '8080', Model);
  try
    // Upgrade to WebSocket
    Client.WebSocketsUpgrade('');

    // Now supports server-to-client callbacks
    Client.Services.Resolve(IMyCallback, Callback);
  finally
    Client.Free;
  end;
end;
```

---

## 11.4. HTTP Server

### 11.4.1. TRestHttpServer

Wraps `TRestServer` instances for HTTP access:

```pascal
uses
  mormot.rest.http.server;

var
  HttpServer: TRestHttpServer;
begin
  HttpServer := TRestHttpServer.Create(
    '8080',           // Port
    [Server],         // TRestServer instances
    '+',              // Domain ('+' = all)
    useHttpAsync      // Server mode
  );
  try
    HttpServer.AccessControlAllowOrigin := '*';  // CORS
    // Server running...
    ReadLn;
  finally
    HttpServer.Free;
  end;
end;
```

### 11.4.2. Server Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| `useHttpApi` | Windows HTTP.SYS (kernel-mode) | Windows production |
| `useHttpSocket` | Thread-per-connection | Behind reverse proxy |
| `useHttpAsync` | Event-driven async | **Best scaling** |
| `useBidirSocket` | WebSockets + threads | Callbacks (small scale) |
| `useBidirAsync` | WebSockets + async | **Callbacks at scale** |

```pascal
// Production server (async, best performance)
HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);

// WebSocket support
HttpServer := TRestHttpServer.Create('8080', [Server], '+', useBidirAsync);
```

### 11.4.3. HTTPS / SSL

```pascal
// Enable TLS (Port, Servers, Domain, Use, ThreadPoolCount, Security)
HttpServer := TRestHttpServer.Create('443', [Server], '+',
  useHttpAsync, 32, secTLS);

// Self-signed certificate (development only)
HttpServer := TRestHttpServer.Create('443', [Server], '+',
  useHttpAsync, 32, secTLSSelfSigned);
```

For http.sys, certificates must be registered:
```bash
# Register certificate (Windows)
netsh http add sslcert ipport=0.0.0.0:443 certhash=<thumbprint> appid={<guid>}
```

### 11.4.4. Multiple REST Servers

One HTTP server can host multiple REST servers:

```pascal
var
  ApiServer, AdminServer: TRestServerDB;
begin
  ApiServer := TRestServerDB.Create(ApiModel, 'api.db3');
  ApiServer.Model.Root := 'api';

  AdminServer := TRestServerDB.Create(AdminModel, 'admin.db3');
  AdminServer.Model.Root := 'admin';

  HttpServer := TRestHttpServer.Create('8080',
    [ApiServer, AdminServer], '+', useHttpAsync);

  // Access via:
  // http://localhost:8080/api/...
  // http://localhost:8080/admin/...
end;
```

---

## 11.5. Windows http.sys Server

### 11.5.1. Why http.sys?

Windows kernel-mode HTTP server provides:

- **Kernel-mode queuing**: Lower context switching overhead
- **Stability**: Worker process failures don't drop requests
- **Performance**: Direct kernel-to-process routing
- **Embedded SSL**: Kernel-level HTTPS handling
- **URL sharing**: Multiple apps on same port

### 11.5.2. URI Authorization

http.sys requires URI registration (Administrator rights):

```pascal
// Option 1: Auto-register (run as Admin once)
HttpServer := TRestHttpServer.Create('8080', [Server], '+',
  useHttpApiRegisteringURI);

// Option 2: Manual registration
THttpApiServer.AddUrlAuthorize('api', '8080', False, '+');
```

**Manual registration via netsh:**
```bash
# List registered URLs
netsh http show urlacl

# Add URL reservation
netsh http add urlacl url=http://+:8080/api/ user=Everyone

# Delete URL reservation
netsh http delete urlacl url=http://+:8080/api/
```

### 11.5.3. Firewall Configuration

```bash
# Open port in Windows Firewall
netsh advfirewall firewall add rule name="mORMot Server" ^
  dir=in action=allow protocol=TCP localport=8080
```

---

## 11.6. Thread Safety

### 11.6.1. Client Thread Safety

`TRestClientUri` classes are thread-safe by design:

```pascal
// Safe: URI() method is internally locked
procedure TWorkerThread.Execute;
begin
  GlobalClient.Orm.Retrieve(ID, Record);  // Thread-safe
end;
```

### 11.6.2. Server Execution Modes

```pascal
type
  TRestServerAcquireMode = (
    amUnlocked,                  // No locking (read-only operations)
    amLocked,                    // Mutex protection (default for writes)
    amBackgroundThread,          // Queue to background thread
    amBackgroundOrmSharedThread, // Queue to shared ORM background thread
    amMainThread                 // Queue to main thread (GUI)
  );

// Configure execution modes
Server.AcquireWriteMode := amLocked;
Server.AcquireExecutionMode[execSoaByInterface] := amLocked;
```

### 11.6.3. Execution Contexts

| Context | Default Mode | Description |
|---------|--------------|-------------|
| `execOrmGet` | `amUnlocked` | ORM read operations |
| `execOrmWrite` | `amLocked` | ORM write operations |
| `execSoaByMethod` | `amUnlocked` | Method-based services |
| `execSoaByInterface` | `amLocked` | Interface-based services |

---

## 11.7. Connection Patterns

### 11.7.1. Single Server, Multiple Protocols

```pascal
var
  Server: TRestServerDB;
  HttpServer: TRestHttpServer;
begin
  Server := TRestServerDB.Create(Model, 'data.db3');

  // HTTP access
  HttpServer := TRestHttpServer.Create('8080', [Server]);

  // Same server accessible via HTTP and in-process
end;
```

### 11.7.2. Load Balancer Setup

```
                    ┌─────────────────┐
                    │  Load Balancer  │
                    │   (nginx/HAProxy)│
                    └────────┬────────┘
           ┌─────────────────┼─────────────────┐
           ▼                 ▼                 ▼
    ┌─────────────┐   ┌─────────────┐   ┌─────────────┐
    │ mORMot Srv 1│   │ mORMot Srv 2│   │ mORMot Srv 3│
    └─────────────┘   └─────────────┘   └─────────────┘
           │                 │                 │
           └─────────────────┼─────────────────┘
                             ▼
                    ┌─────────────────┐
                    │   Database      │
                    │  (PostgreSQL)   │
                    └─────────────────┘
```

### 11.7.3. DMZ Architecture

```
       Internet
           │
   ┌───────┴───────┐
   │     DMZ       │  TRestServerRemoteDB (services only)
   │  ┌─────────┐  │
   │  │ Proxy   │  │
   │  │ Server  │  │
   │  └────┬────┘  │
   └───────┼───────┘
           │ (Internal network)
   ┌───────┴───────┐
   │   Internal    │  TRestServerDB (full ORM + services)
   │  ┌─────────┐  │
   │  │  Main   │  │
   │  │ Server  │  │
   │  └─────────┘  │
   └───────────────┘
```

---

## 11.8. Proper Shutdown

### 11.8.1. Shutdown Order

```pascal
// CORRECT shutdown order
FreeAndNil(HttpServer);  // 1. Stop accepting connections
FreeAndNil(Server);      // 2. Free REST server
FreeAndNil(Model);       // 3. Free model last

// WRONG - will cause access violations
FreeAndNil(Model);       // Model freed while Server uses it!
FreeAndNil(Server);
FreeAndNil(HttpServer);
```

### 11.8.2. Graceful Shutdown

```pascal
procedure GracefulShutdown;
begin
  // Signal shutdown
  HttpServer.Shutdown;

  // Wait for pending requests (with timeout)
  Sleep(1000);

  // Free resources
  FreeAndNil(HttpServer);
  FreeAndNil(Server);
  FreeAndNil(Model);
end;
```

---

## 11.9. Migration from mORMot 1

### 11.9.1. Class Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `TSQLRest` | `TRest` |
| `TSQLRestServer` | `TRestServer` |
| `TSQLRestServerDB` | `TRestServerDB` |
| `TSQLRestServerFullMemory` | `TRestServerFullMemory` |
| `TSQLRestClient` | `TRestClient` |
| `TSQLRestClientDB` | `TRestClientDB` |
| `TSQLHttpServer` | `TRestHttpServer` |
| `TSQLHttpClient*` | `TRestHttpClient*` |

### 11.9.2. Unit Renames

| mORMot 1 | mORMot 2 |
|----------|----------|
| `mORMot.pas` | `mormot.rest.core` + `mormot.orm.core` |
| `mORMotSQLite3.pas` | `mormot.rest.sqlite3` |
| `mORMotHttpServer.pas` | `mormot.rest.http.server` |
| `mORMotHttpClient.pas` | `mormot.rest.http.client` |

### 11.9.3. API Changes

```pascal
// mORMot 1: Direct ORM access on TRest
Server.Add(Customer, True);

// mORMot 2: Via Orm interface
Server.Orm.Add(Customer, True);
```

---

*Next Chapter: Client-Server ORM Operations*

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 10: JSON and RESTful Fundamentals](mORMot2-SAD-Chapter-10.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 12: Client-Server ORM Operations](mORMot2-SAD-Chapter-12.md) |
