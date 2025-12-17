# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `mormot.soa.*` units implement Interface-based Service-Oriented Architecture (SOA) for mORMot 2. This allows defining services as Pascal interfaces that can be consumed remotely via JSON/REST, with automatic client stub generation and bidirectional WebSocket communication.

## Architecture Layers

### Core Abstraction (`mormot.soa.core.pas`)
Shared types and base classes used by both client and server:

- **`TServiceFactory`** - Abstract service provider managing interface registration, contract validation, and execution context
- **`TServiceContainer`** - Abstract holder for multiple service factories (accessible via `TRest.Services` property)
- **`TOrmServiceLog`** - Database-backed execution statistics (method calls, timing, I/O)
- **`TOrmServiceNotifications`** - Asynchronous notification tracking via database polling

### Client Side (`mormot.soa.client.pas`)
Generates fake interface implementations that serialize calls to JSON/REST:

- **`TServiceFactoryClient`** - Creates fake `TInterfacedObject` instances that marshal calls over HTTP
- **`TServiceContainerClient`** - Client-side service registry
- Registration: `Client.ServiceRegister([TypeInfo(ICalculator)], sicShared)`

### Server Side (`mormot.soa.server.pas`)
Manages real service implementation instances with lifecycle control:

- **`TServiceFactoryServer`** - Instance creation, authorization, logging, lifecycle management
- **`TInjectableObjectRest`** - Base class for service implementations with DI support
- **`TServiceContainerServer`** - Server-side service registry
- Registration: `Server.ServiceRegister(TServiceCalculator, [TypeInfo(ICalculator)], sicShared)`

### Code Generation (`mormot.soa.codegen.pas`)
Generate client code and documentation from server definitions:

- **`WrapperFromModel()`** - Generate client code using Mustache templates
- **`ContextFromModel()`** - Extract ORM/SOA metadata as JSON for code generation
- **Async interface generation** - Convert synchronous interfaces to async versions

## Service Instance Implementation Patterns

The `TServiceInstanceImplementation` enum controls server-side instance lifecycle:

| Pattern | Description | Thread Safety |
|---------|-------------|---------------|
| `sicSingle` | New instance per call (default) | Not required |
| `sicShared` | One instance for all calls | **Required** |
| `sicClientDriven` | Instance tied to client lifetime (stateful) | Not required |
| `sicPerSession` | One instance per authenticated session | **Required** |
| `sicPerUser` | One instance per user across sessions | **Required** |
| `sicPerGroup` | One instance per user group | **Required** |
| `sicPerThread` | One instance per calling thread | Not required |

**Key Insight**: Choose based on state requirements and performance trade-offs. `sicSingle` is safest but slowest. `sicShared` is fastest but requires thread-safe implementation.

## Contract Validation

Services use **contract signatures** to ensure client/server compatibility:

- **Default contract**: MD5 hash of interface methods + parameter types (automatic)
- **Custom contract**: Version string or GUID for explicit compatibility control
- **Validation**: Client/server contracts must match or connection is rejected

Example with custom contract:
```pascal
// Server
Server.ServiceRegister(TMyService, [TypeInfo(IMyService)], sicShared)
  .ContractExpected := 'v2.5';

// Client
Client.ServiceRegister([TypeInfo(IMyService)], sicShared, 'v2.5');
```

## Authorization & Security

Per-method authorization using `TAuthGroup` IDs:

```pascal
Factory := Server.Services.Info(ICalculator) as TServiceFactoryServer;

// Deny all by default
Factory.DenyAll;

// Allow specific groups
Factory.Allow(ICalculator, [ADMIN_GROUP_ID, MANAGER_GROUP_ID]);

// Deny specific methods
Factory.Deny(ICalculator, 'DeleteAll', [MANAGER_GROUP_ID]);
```

Authorization states:
- `idAllowAll` - All groups allowed (default)
- `idDenyAll` - All groups denied
- `idAllowed` - Only specified groups allowed
- `idDenied` - Specified groups denied

## Execution Logging

Enable automatic call logging to database:

```pascal
// Server-side logging
Factory.SetServiceLog(RestServer, TOrmServiceLog);

// Logs capture:
// - Method name (interface.method)
// - Input/output parameters as JSON
// - Execution time (microseconds)
// - User/Session/IP context
```

`TOrmServiceLog` stores statistics for performance tuning and debugging. `TOrmServiceNotifications` extends this for async notification patterns.

## Key Design Patterns

### 1. Client Usage Pattern
```pascal
var
  I: ICalculator;
begin
  if Client.Services.Resolve(ICalculator, I) then
    Result := I.Add(10, 20); // Serialized to JSON, sent to server
end;
```

### 2. Server Implementation Pattern
```pascal
type
  TServiceCalculator = class(TInjectableObjectRest, ICalculator)
  public
    function Add(A, B: Integer): Integer;
  end;

function TServiceCalculator.Add(A, B: Integer): Integer;
begin
  // Direct access to ORM via inherited Server property
  Result := A + B;
  Server.InternalLog('Calculator.Add called', sllTrace);
end;
```

### 3. Bidirectional Communication (WebSockets)
Define callback interfaces as method parameters:

```pascal
type
  IProgress = interface
    procedure Update(Percent: Integer);
  end;

  ILongTask = interface
    procedure Execute(const Callback: IProgress);
  end;

// Server pushes progress updates to client via callback interface
```

## Method-Based vs Interface-Based Services

mORMot supports two SOA approaches:

| Aspect | Method-Based | Interface-Based (this folder) |
|--------|--------------|-------------------------------|
| Definition | Class methods on `TRest` | Pascal `interface` types |
| Client code | Manual implementation | Auto-generated stubs |
| Performance | Fastest (direct HTTP access) | Slightly slower (JSON marshalling) |
| Type safety | Low (manual JSON parsing) | High (compiler-checked) |
| WebSockets | Not supported | Built-in bidirectional |
| Best for | Low-level endpoints, file uploads | Business logic, DDD services |

**Recommendation**: Use interface-based services from this folder for 95% of use cases. Only drop to method-based for performance-critical endpoints or non-standard HTTP operations.

## Common Patterns

### Dependency Injection in Services
```pascal
type
  TMyService = class(TInjectableObjectRest, IMyService)
  private
    fRepository: ICustomerRepository; // Injected dependency
  public
    constructor Create(const aRepository: ICustomerRepository); override;
  end;

// Container auto-resolves dependencies
Server.Services.Resolve(ICustomerRepository, RepositoryInstance);
Server.ServiceRegister(TMyService, [TypeInfo(IMyService)], sicShared);
```

### Async Notifications via Database
```pascal
// Client polls for notifications since last known ID
var
  Events: TDocVariantData;
begin
  if TOrmServiceNotifications.LastEventsAsObjects(
       Rest, LastID, 100, Factory, Events) then
    ProcessEvents(Events);
end;
```

### Thread-Safe Shared Services
```pascal
type
  TCacheService = class(TInjectableObjectRest, ICacheService)
  private
    fLock: TRTLCriticalSection;
    fCache: TSynDictionary;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Get(const Key: RawUtf8): Variant; // Thread-safe
  end;

// Register as sicShared for best performance
Server.ServiceRegister(TCacheService, [TypeInfo(ICacheService)], sicShared);
```

## Files Summary

| File | Purpose |
|------|---------|
| `mormot.soa.core.pas` | Base types, contracts, logging, authorization |
| `mormot.soa.client.pas` | Fake interface stubs, JSON marshalling |
| `mormot.soa.server.pas` | Instance management, execution, DI support |
| `mormot.soa.codegen.pas` | Mustache-based code generation, async conversion |

## Testing Services

No dedicated test infrastructure in this folder. See `/mnt/w/mORMot2/test/` for regression tests.

Typical testing approach:
1. Create in-memory server: `TRestServerFullMemory.Create(Model)`
2. Register service: `Server.ServiceRegister(...)`
3. Create client: `TRestClientUri.Create(Server)`
4. Register client interface: `Client.ServiceRegister(...)`
5. Resolve interface and call methods
6. Validate results and database side effects

## Related Components

This folder depends on:
- **`mormot.core.interfaces`** - Interface RTTI and fake object generation
- **`mormot.core.json`** - JSON serialization of method calls
- **`mormot.orm.*`** - Database logging via `TOrmServiceLog`
- **`mormot.rest.*`** - HTTP transport and routing

**📖 SAD Chapters**:
- [Chapter 14: Interface-Based Services](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-14.md) - Service architecture
- [Chapter 15: Client-Server Services](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-15.md) - Service implementation
- [Chapter 16: Advanced Service Features](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-16.md) - Async, callbacks, code generation

See `/mnt/w/mORMot2/src/README.md` for overall framework structure.

## Key Takeaways

1. **Contracts prevent version mismatches** - Always validate client/server compatibility
2. **Instance patterns affect scalability** - Choose based on state requirements
3. **Authorization is per-method** - Fine-grained security control
4. **Logging is optional but valuable** - Enable for performance tuning
5. **DI works out of the box** - Use `TInjectableObjectRest` base class
6. **WebSockets enable callbacks** - Interface parameters become bidirectional channels
7. **Code generation reduces boilerplate** - Mustache templates generate clients automatically

---

**Last Updated**: 2025-10-10 (mORMot 2)
