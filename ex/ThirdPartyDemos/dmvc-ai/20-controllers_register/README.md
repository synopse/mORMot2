# 20-controllers_register - Dynamic Service Registration

**Difficulty**: ⭐⭐⭐ Advanced

Port of DMVC `controllers_register` sample to mORMot2, demonstrating dynamic service registration and unregistration at runtime.

## Overview

This sample demonstrates how to:
- ✅ Register services dynamically at runtime
- ✅ Unregister services (mark as unloaded)
- ✅ List available services
- ✅ Query service status
- ✅ Manage multiple services from a central registry

## Original DMVC Sample

**Source**: `/mnt/w/DMVCframework/samples/controllers_register/`

**Key files**:
- `MyController1U.pas` - First controller (general API info)
- `MyController2U.pas` - Second controller (person data)
- `WebModuleMainU.pas` - Controller registration using TControllersRegister
- `MainFormU.pas` - GUI form with server controls

## DMVC Approach

```pascal
// In controller initialization section
initialization
  TControllersRegister.Instance.RegisterController(TMyController1, 'MyServerName');
end.

// In WebModule
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FEngine := TMVCEngine.Create(Self);
  TControllersRegister.Instance.AddControllersInEngine(FEngine, 'MyServerName');
end;
```

## mORMot2 Approach

```pascal
// Service Registry Manager
type
  IServiceRegistry = interface(IInvokable)
    function ListServices: RawUtf8;
    function LoadService(const ServiceName: RawUtf8): Boolean;
    function UnloadService(const ServiceName: RawUtf8): Boolean;
    function GetServiceStatus(const ServiceName: RawUtf8): RawUtf8;
  end;

// Dynamic registration
function TServiceRegistry.LoadService(const ServiceName: RawUtf8): Boolean;
begin
  if ServiceName = 'Service1' then
  begin
    fServer.ServiceDefine(TService1, [TypeInfo(IService1)], sicShared);
    Result := True;
  end;
end;
```

## Architecture

### Services

1. **IService1** - General API information
   - Port of DMVC `TMyController1` (`/api`)
   - Returns server info (name, online status, datetime)

2. **IService2** - Person data
   - Port of DMVC `TMyController2` (`/api/person`)
   - Returns person information

3. **IServiceRegistry** - Service manager
   - New capability in mORMot2 port
   - Manages service lifecycle
   - Lists/loads/unloads services dynamically

### Project Structure

```
20-controllers_register/
├── src/
│   ├── entities.pas          # TPerson domain entity
│   ├── api.interfaces.pas    # IService1, IService2, IServiceRegistry
│   ├── api.impl.pas          # TService1, TService2
│   └── server.pas            # TServiceRegistry, TControllersRegisterServer
├── 20-controllers_register.dpr
├── 20-controllers_register.dproj
└── README.md
```

## API Endpoints

### Service Registry Manager

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/ServiceRegistry/ListServices` | POST | List all registered services |
| `/ServiceRegistry/LoadService` | POST | Register a service by name |
| `/ServiceRegistry/UnloadService` | POST | Unregister a service |
| `/ServiceRegistry/GetServiceStatus` | POST | Get service status |

### Service 1 (pre-loaded)

| Endpoint | Method | Description | Original DMVC |
|----------|--------|-------------|---------------|
| `/Service1/GetInfo` | POST | Get API info | `GET /api` |

### Service 2 (load first)

| Endpoint | Method | Description | Original DMVC |
|----------|--------|-------------|---------------|
| `/Service2/GetPerson` | POST | Get person data | `GET /api/person` |

## Usage Examples

### 1. List Services

```bash
curl -X POST http://localhost:8080/ServiceRegistry/ListServices
```

**Response**:
```json
{
  "result": ["Service1"]
}
```

### 2. Load Service2

```bash
curl -X POST http://localhost:8080/ServiceRegistry/LoadService \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
```

**Response**:
```json
{
  "result": true
}
```

### 3. Call Service2

```bash
curl -X POST http://localhost:8080/Service2/GetPerson
```

**Response**:
```json
{
  "result": {
    "Name": "João Antônio Duarte",
    "Age": 26,
    "Country": "Brasil"
  }
}
```

### 4. Check Service Status

```bash
curl -X POST http://localhost:8080/ServiceRegistry/GetServiceStatus \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
```

**Response**:
```json
{
  "result": "loaded"
}
```

### 5. Unload Service2

```bash
curl -X POST http://localhost:8080/ServiceRegistry/UnloadService \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
```

**Response**:
```json
{
  "result": true
}
```

### 6. List Services Again

```bash
curl -X POST http://localhost:8080/ServiceRegistry/ListServices
```

**Response**:
```json
{
  "result": ["Service1"]
}
```

## Complete Workflow

```bash
# 1. Start server
./Win32/Release/20-controllers_register.exe

# 2. List initial services (Service1 pre-loaded)
curl -X POST http://localhost:8080/ServiceRegistry/ListServices
# Response: {"result":["Service1"]}

# 3. Call Service1
curl -X POST http://localhost:8080/Service1/GetInfo
# Response: {"result":{"Name":"MyServerName","Age":1,"Country":"2025-12-20T..."}}

# 4. Try to call Service2 (not loaded yet)
curl -X POST http://localhost:8080/Service2/GetPerson
# Response: Error (service not registered)

# 5. Load Service2
curl -X POST http://localhost:8080/ServiceRegistry/LoadService \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
# Response: {"result":true}

# 6. Now call Service2 successfully
curl -X POST http://localhost:8080/Service2/GetPerson
# Response: {"result":{"Name":"João Antônio Duarte","Age":26,"Country":"Brasil"}}

# 7. Unload Service2
curl -X POST http://localhost:8080/ServiceRegistry/UnloadService \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
# Response: {"result":true}

# 8. Verify Service2 is unloaded
curl -X POST http://localhost:8080/ServiceRegistry/GetServiceStatus \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
# Response: {"result":"unloaded"}
```

## Build Instructions

```bash
# Compile with Delphi compiler
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\20-controllers_register\20-controllers_register.dproj --config=Release

# Run
cd /mnt/w/mORMot2/ex/dmvc/20-controllers_register
./Win32/Release/20-controllers_register.exe
```

## Key Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Controller registration** | `TControllersRegister.Instance` | Service registry manager |
| **Registration scope** | Server name string | Service name tracking |
| **Runtime unload** | Supported | Simulated (marked as unloaded) |
| **Controller discovery** | Automatic via attributes | Manual via LoadService |
| **Endpoint style** | RESTful (`GET /api`) | JSON-RPC (`POST /Service1/GetInfo`) |

## Important Notes

### mORMot2 Service Lifecycle

**Limitation**: mORMot2 doesn't support true dynamic service unregistration. Once a service is registered via `ServiceDefine()`, it remains in the server's service list.

**Workaround**: This sample simulates unloading by:
1. Maintaining a list of "loaded" services in `TServiceRegistry`
2. Removing services from the list when "unloaded"
3. Clients can check service status via `GetServiceStatus()`

**Why**: mORMot2's interface-based service architecture is designed for compile-time service definitions. Dynamic loading/unloading would require:
- Service instance cleanup (not provided by framework)
- Method resolver updates (internal to TRestServer)
- Thread-safe registration/unregistration

### Pre-loaded Services

In this sample:
- **Service1** is pre-loaded (simulates DMVC's `AddControllersInEngine('MyServerName')`)
- **Service2** must be loaded manually via `LoadService()`

This demonstrates both:
- Static registration at server startup
- Dynamic registration at runtime

## Extension Ideas

### 1. Add More Services

```pascal
// In api.interfaces.pas
type
  IService3 = interface(IInvokable)
    ['{NEW-GUID}']
    function GetData: RawUtf8;
  end;

// In api.impl.pas
type
  TService3 = class(TInjectableObject, IService3)
    function GetData: RawUtf8;
  end;

// In server.pas TServiceRegistry.LoadService
else if ServiceName = 'Service3' then
begin
  RegisterService('Service3', TService3, TypeInfo(IService3));
  Result := True;
end;
```

### 2. Service Configuration

Add service-specific configuration when loading:

```pascal
function LoadServiceWithConfig(const ServiceName, Config: RawUtf8): Boolean;
```

### 3. Service Dependencies

Track service dependencies:

```pascal
function LoadService(const ServiceName: RawUtf8): Boolean;
begin
  // Load dependencies first
  if ServiceName = 'ServiceB' then
    LoadService('ServiceA'); // ServiceB depends on ServiceA
  // Then load the service
end;
```

### 4. Health Checks

Add health check endpoints:

```pascal
function CheckServiceHealth(const ServiceName: RawUtf8): RawUtf8;
```

## Performance Considerations

- **Service registration**: O(1) operation (hash table lookup)
- **Service list**: O(n) where n = number of services (typically small)
- **No overhead**: After registration, service calls have zero registry overhead

## Testing

### Unit Tests

```pascal
// Test service registration
procedure TestLoadService;
var
  Registry: IServiceRegistry;
begin
  Registry := GetServiceRegistry;
  Assert(Registry.LoadService('Service2') = True);
  Assert(Registry.GetServiceStatus('Service2') = 'loaded');
end;

// Test service unloading
procedure TestUnloadService;
var
  Registry: IServiceRegistry;
begin
  Registry := GetServiceRegistry;
  Registry.LoadService('Service2');
  Assert(Registry.UnloadService('Service2') = True);
  Assert(Registry.GetServiceStatus('Service2') = 'unloaded');
end;
```

### Integration Tests

```bash
# Test complete workflow
./test-controllers-register.sh
```

## Troubleshooting

### Service not found

**Issue**: `curl -X POST http://localhost:8080/Service2/GetPerson` returns error

**Solution**: Load the service first:
```bash
curl -X POST http://localhost:8080/ServiceRegistry/LoadService \
  -H "Content-Type: application/json" \
  -d '{"ServiceName":"Service2"}'
```

### Service still accessible after unload

**Issue**: Can still call service after `UnloadService()`

**Explanation**: mORMot2 doesn't support true unregistration. The service remains callable, but the registry marks it as "unloaded". This is a design limitation, not a bug.

## Learn More

- **Service interfaces**: [mORMot2 SAD - Interface-Based Services](https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html#TITLE_151)
- **Service lifecycle**: See mORMot2 documentation on `ServiceDefine()`
- **Original DMVC sample**: `/mnt/w/DMVCframework/samples/controllers_register/`

## Related Samples

- **01-basicdemo_server** - Basic service definition
- **06-articles_crud_server** - Full CRUD service
- **12-middleware** - Request/response filtering

---

**Status**: ✅ Compiled successfully (Delphi 12 Athens, Win32, Release)

**Last Updated**: 2025-12-20
