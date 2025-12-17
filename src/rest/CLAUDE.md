# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `mORMot2/src/rest` directory contains the RESTful-oriented features of the mORMot framework. This implements a Client-Server architecture using JSON for data representation, with support for HTTP/HTTPS, WebSockets, and in-process calls.

**Key Concept**: mORMot is REST-oriented but not truly RESTful. It's designed for method-based and interface-based services (more RPC-oriented) rather than pure resource-based REST.

## Architecture: Composition Over Inheritance

mORMot 2 follows SOLID principles with composition-based design:

```
TRest (abstract parent)
├── ORM: IRestOrm interface          → Object-Relational-Mapping
├── Services: TServiceContainer      → Service-Oriented-Architecture
├── Run: TRestRunThreads            → Threading abilities
└── REST features                    → URI routing, auth, sessions
    ├── TRestClient (client-side)
    └── TRestServer (server-side)
```

## Core Units

### mormot.rest.core.pas (195KB)
**Foundation layer** - Shared types and abstract REST process

Key classes:
- `TRest` - Abstract parent class (do NOT instantiate directly)
- `TRestBackgroundTimer` - Multi-threaded periodic tasks and async operations
- `TRestRunThreads` - Thread pool for REST operations
- `TRestAcquireExecution` - Execution mode control (locked/unlocked/background)
- `TOrmHistory`/`TOrmTableDeleted` - Modification tracking

Key enums:
- `TRestServerUriContextCommand` - execNone, execSoaByMethod, execSoaByInterface, execOrmGet, execOrmWrite
- `TRestServerAcquireMode` - amUnlocked, amLocked, amBackgroundThread, amMainThread

**Important**: Access ORM via `TRest.Orm` interface, not direct inheritance.

### mormot.rest.server.pas (338KB)
**Server-side implementation**

Key classes:
- `TRestServer` - Abstract REST server (inherit from this for actual servers)
- `TRestServerUriContext` - Execution context for REST requests (virtual methods for custom routing)
- `TRestRouter` - **Radix Tree-based URI multiplexer** (high performance routing)
- `TAuthSession` - In-memory user sessions
- `TRestServerAuthentication` - Authentication scheme implementation
- `TRestServerMonitor` - Statistics and monitoring

**Routing schemes**:
- `TRestServerRoutingRest` - RESTful routing: `/Model/Interface.Method`
- `TRestServerRoutingJsonRpc` - JSON-RPC routing: `/Model/Interface` with JSON body

**Router nodes** (`TRestNode`):
- `rnTable` - `/ModelRoot/TableName` (GET/POST/PUT/DELETE)
- `rnTableID` - `/ModelRoot/TableName/<id>`
- `rnTableIDBlob` - `/ModelRoot/TableName/<id>/Blob`
- `rnTableMethod` - `/ModelRoot/TableName/<method>`
- `rnInterface` - `/ModelRoot/InterfaceName[/.MethodName]`
- `rnMethod` - `/ModelRoot/MethodName`

### mormot.rest.client.pas (128KB)
**Client-side implementation**

Key classes:
- `TRestClientUri` - Base class for all REST clients
- `TRestClientAuthentication` - Client authentication base class
  - `TRestClientAuthenticationUri` - Weak (session_signature in URL)
  - `TRestClientAuthenticationSignedUri` - Strong (digital signature with timestamps)
- `TRestClientLibraryRequest` - In-process client (via exported server library)

**Routing schemes** (client-side):
- `TRestClientRoutingRest` - Matches server RESTful routing
- `TRestClientRoutingJsonRpc` - Matches server JSON-RPC routing

**Authentication**:
- Password kinds: `passClear`, `passHashed`, `passKerberosSpn`, `passModularCrypt`
- Signature algorithms: `suaCRC32`, `suaMD5`, `suaSHA256`, `suaSHA512`, `suaSHA3`

### mormot.rest.http.server.pas (72KB)
**HTTP/WebSockets server transport**

Key class:
- `TRestHttpServer` - RESTful HTTP server wrapper

**Server modes** (`TRestHttpServerUse`):
- `useHttpApi` - Windows HTTP.SYS (kernel-mode, IIS-like)
- `useHttpSocket` - Thread-per-connection (good behind reverse proxy)
- `useHttpAsync` - Event-driven async (best scaling for many connections)
- `useBidirSocket` - WebSockets with thread-per-client
- `useBidirAsync` - WebSockets event-driven (best scaling)

**Security** (`TRestHttpServerSecurity`):
- `secNone` - Plain HTTP
- `secTLS` - HTTPS with certificate
- `secTLSSelfSigned` - HTTPS with self-signed cert

**Best practices**:
- Use `useHttpAsync` for modern production servers (scales well)
- Use `useHttpSocket` behind reverse proxies configured for HTTP/1.0
- Use `useBidirAsync` for WebSockets at scale
- Always use `secTLS` in production (not `secSynShaAes` - deprecated)

### mormot.rest.http.client.pas (47KB)
**HTTP/WebSockets client transport**

Key classes:
- `TRestHttpClientGeneric` - Base HTTP client
- `TRestHttpClientWinSock` - Socket-based client
- `TRestHttpClientWebsockets` - WebSockets client
- `TRestHttpClientWinINet`/`TRestHttpClientWinHTTP` - Windows clients
- `TRestHttpClientCurl` - libcurl-based (cross-platform)

### mormot.rest.memserver.pas (26KB)
**In-memory standalone server**

Key classes:
- `TRestServerFullMemory` - Complete in-memory REST server (includes ORM engine)

Use cases:
- Testing without SQLite3 dependency
- Simple CRUD + authentication scenarios
- Services with minimal ORM needs
- JSON or binary persistence to disk

### mormot.rest.sqlite3.pas (16KB)
**SQLite3 database integration**

Key classes:
- `TRestServerDB` - REST server with SQLite3 backend
- `TRestClientDB` - Direct SQLite3 client access

Use when you need a full SQL database with ORM.

### mormot.rest.mvc.pas (20KB)
**Model-View-Controller support**

MVC/MVVM pattern over `TRestServer` with Mustache templating.

## Common Patterns

### Creating a REST Server
```pascal
// 1. Define your ORM model
Model := TOrmModel.Create([TOrmUser, TOrmData]);

// 2. Choose server type based on persistence needs
Server := TRestServerDB.Create(Model, 'data.db3');      // SQLite3
// OR
Server := TRestServerFullMemory.Create(Model);          // In-memory

// 3. Wrap in HTTP server
HttpServer := TRestHttpServer.Create(
  '8080',           // Port
  [Server],         // REST server instances
  '+',              // Domain name
  useHttpAsync      // Use async mode for scaling
);

// 4. Register services
Server.ServiceDefine(TMyService, [IMyService], sicShared);

// 5. Start
HttpServer.Start;
```

### Creating a REST Client
```pascal
// 1. Choose transport
Client := TRestHttpClientWebsockets.Create(
  'localhost', '8080', TOrmModel.Create([TOrmUser])
);

// 2. Authenticate
Client.SetUser('username', 'password');

// 3. Use ORM
Client.Orm.Retrieve(ID, User);

// 4. Use services
Client.Services.Resolve(IMyService, MyService);
```

### Custom Routing
```pascal
// Override UriComputeRoutes in TRestServerUriContext descendant
class procedure TMyRoutingContext.UriComputeRoutes(
  Router: TRestRouter; Server: TRestServer);
begin
  // Register custom routes using Router.Setup()
  Router.Setup([mGET, mPOST], ['/api/custom'], rnMethod);
end;

// Set as ServicesRouting
Server.ServicesRouting := TMyRoutingContext;
```

### Background Processing
```pascal
// Async interface method execution
Server.BackgroundTimer.AsyncRedirect(
  IMyService,
  MyServiceInstance,
  AsyncServiceProxy
);

// Async batch operations
Server.BackgroundTimer.AsyncBatchStart(TOrmData, 10); // 10 sec or count threshold
Server.BackgroundTimer.AsyncBatchAdd(TOrmData, Data);
Server.BackgroundTimer.AsyncBatchStop(TOrmData);

// Periodic tasks
Server.TimerEnable(MyTimerCallback, 5000); // Every 5 seconds
```

## Threading Models

**Execution modes** (`TRestServerAcquireMode`):
- `amUnlocked` - No locking (read-only or thread-safe operations)
- `amLocked` - Mutex-protected (default for writes)
- `amBackgroundThread` - Queued to background thread
- `amMainThread` - Queued to main thread (GUI apps)

Configure via:
```pascal
Server.AcquireWriteMode := amBackgroundOrmSharedThread; // ORM writes in background
Server.AcquireExecutionMode[execSoaByInterface] := amLocked; // Interface calls locked
```

## Authentication

**Server-side registration**:
```pascal
Server.AuthenticationRegister(TRestServerAuthenticationDefault);
Server.AuthenticationRegister(TRestServerAuthenticationHttpBasic);
Server.AuthenticationRegister(TRestServerAuthenticationSSPI); // Windows domain
```

**Client-side**:
```pascal
// Default (strongest available)
if not Client.SetUser('user', 'pass') then
  raise Exception.Create('Auth failed');

// Explicit authentication class
TRestClientAuthenticationSignedUri.ClientSetUser(
  Client, 'user', 'pass', passClear
);
```

## Monitoring & Statistics

```pascal
// Enable monitoring
Server.CreateMissingTables; // Needed for monitoring tables

// Access stats
Stats := Server.Stats; // TRestServerMonitor
Connections := Server.Sessions.Count;

// Custom monitoring callback
Server.OnUpdateEvent := procedure(Sender: TRest; const Event: RawUtf8)
begin
  LogEvent(Event);
end;
```

## Common Issues

### "Unexpected X.UriComputeRoutes" Error
**Cause**: Using abstract `TRestServerUriContext` directly.
**Fix**: Use `TRestServerRoutingRest` or `TRestServerRoutingJsonRpc` as ServicesRouting.

### Services Not Found (404)
**Check**:
1. `ServiceDefine()` called before `CreateMissingTables()`?
2. Correct routing class set via `ServicesRouting`?
3. Router recomputed via `ComputeRoutes()` if changed dynamically?
4. Client and server using matching routing schemes?

### Interface Method Not Called
**Check**:
1. Interface registered with `{$M+}` directive?
2. Method has exactly matching signature on client/server?
3. Authentication successful (`Client.SessionUser <> nil`)?
4. Not blocked by authentication/authorization rules?

### Memory Leaks
**Common mistakes**:
- Not freeing `TRestHttpServer` before `TRestServer`
- Circular references via `IRestOrm` interface (use weak references)
- Services with `sicClientDriven` not freed by client
- Async redirect callbacks not freed before server destruction

**Proper shutdown**:
```pascal
FreeAndNil(HttpServer);  // Free HTTP wrapper first
FreeAndNil(Server);      // Then REST server
FreeAndNil(Model);       // Finally model
```

## Testing Strategies

### Unit Testing
Use `TRestServerFullMemory` for fast isolated tests:
```pascal
Server := TRestServerFullMemory.Create(Model);
try
  // No HTTP overhead, pure in-memory
  Server.Orm.Add(TestData);
  // ... assertions
finally
  Server.Free;
end;
```

### Integration Testing
Use actual HTTP transport with random port:
```pascal
Server := TRestServerDB.Create(Model, ':memory:'); // SQLite in-memory
HttpServer := TRestHttpServer.Create('0', [Server], '+', useHttpAsync);
ActualPort := HttpServer.Port; // Get assigned port
```

### Load Testing
- Prefer `useHttpAsync` for server mode
- Monitor via `Server.Stats.NotifyOrm/NotifyService`
- Use multiple client instances for concurrency
- Check `ServiceRunningContext.RunningThread` for thread distribution

## Dependencies

**Required mORMot units**:
- `mormot.core.*` - Base functionality (JSON, RTTI, threads, buffers)
- `mormot.crypt.*` - Cryptography (secure, JWT)
- `mormot.orm.*` - ORM layer (`IRestOrm` interface)
- `mormot.soa.*` - Service-Oriented-Architecture
- `mormot.db.core` - Database abstraction
- `mormot.net.*` - Network layer (HTTP, WebSockets) for HTTP transport

**Platform support**:
- Windows: All features including HTTP.SYS (`useHttpApi`)
- Linux/BSD: Socket and async modes only
- macOS: Socket and async modes only
- Mobile (Delphi): Client-side only

## Documentation

**📖 SAD Chapters**:
- [Chapter 10: JSON and RESTful Fundamentals](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-10.md) - JSON serialization, REST concepts
- [Chapter 11: Client-Server Architecture](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-11.md) - TRestServer, TRestClient
- [Chapter 12: REST ORM Operations](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-12.md) - CRUD, batch, caching
- [Chapter 13: Authentication and Sessions](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-13.md) - Security, user management

**External Resources**:
- **Official docs**: https://synopse.info/files/doc/mORMot2.html
- **Forum**: https://synopse.info/forum/viewforum.php?id=24
- **Samples**: `/mnt/w/mORMot2/ex/` (especially Thomas Tutorials)
- **Source**: Read inline comments - heavily documented

## Version Notes

This is **mORMot 2** (rewrite from 1.18):
- Units renamed: `TSQLRest` → `TRest`, `TSQLRecord` → `TOrm`
- Composition over inheritance: `TRest.Orm` vs. direct `TRestOrm` parent
- Split large units into smaller scope-refined units
- Enhanced async/event-driven servers
- Modernized syntax (generics, enumerators)

**Migration from v1**: Not compatible. Requires code refactoring.
