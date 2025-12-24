# Deployment Samples (Phase 19)

## Overview

This document describes the deployment samples ported from DelphiMVCFramework to mORMot2, demonstrating various hosting and deployment scenarios.

## Samples Status

| # | Sample | Status | Description |
|---|--------|--------|-------------|
| 42 | server_in_dll | ✅ Complete | REST server hosted in DLL |
| 43 | windows_service | ✅ Complete | REST server as Windows Service |
| 44 | servercontainer | ✅ Complete | Multiple servers in one process |
| 45 | isapi | ⏸️ Deferred | ISAPI module for IIS |

## Sample 42: Server in DLL

**Projects:** 2 (DLL + Host Application)

Demonstrates hosting a mORMot2 REST server inside a DLL that can be loaded by any Windows application.

**Key Features:**
- `stdcall` exported functions for server lifecycle
- No external memory manager required (uses `RawUtf8`)
- Interface-based service API
- Win32/Win64 support

**Files:**
- `dll/ServerDLL.dpr` - REST server DLL
- `host/UsingServerInDLL.dpr` - Host application
- Shared: `src/api.interfaces.pas`, `src/api.impl.pas`, `src/server.pas`

**Compilation:** ✅ Success (both Win32 and Win64)

## Sample 43: Windows Service

**Projects:** 1

Demonstrates hosting a mORMot2 REST server as a Windows Service that runs in the background.

**Key Features:**
- Standard Windows Service infrastructure (TService)
- Service lifecycle management (Install/Start/Stop/Uninstall)
- Interface-based service API with status tracking
- Automatic startup on system boot
- Logs via TSynLog

**Service Commands:**
```cmd
43-windows_service.exe /install
net start "mORMot2 REST Service (DMVC port)"
net stop "mORMot2 REST Service (DMVC port)"
43-windows_service.exe /uninstall
```

**Files:**
- `43-windows_service.dpr` - Main service executable
- `src/service.impl.pas` - Service implementation
- `src/api.interfaces.pas`, `src/api.impl.pas` - Service API

**Compilation:** ✅ Success (Win32 and Win64)

## Sample 44: Server Container

**Projects:** 1

Demonstrates running multiple REST servers in a single process, each on different ports with different API implementations.

**Key Features:**
- Container class managing multiple server instances
- Each server has independent `TRestServerFullMemory` and `TRestHttpServer`
- Demonstrates service implementation inheritance
- Customized behavior per server (Server 2: 2x addition, Server 3: 10x division)

**Servers:**
- Server 1 (port 3000): Standard calculator with enhanced info
- Server 2 (port 3010): Calculator with doubled addition
- Server 3 (port 3020): Calculator with 10x division

**Files:**
- `44-servercontainer.dpr` - Main application
- `src/server.container.pas` - Container implementation
- `src/api.interfaces.pas`, `src/api.impl.pas` - Calculator API

**Compilation:** ✅ Success (Win32 and Win64)

## Sample 45: ISAPI Module

**Status:** ⏸️ Deferred

ISAPI module implementation is deferred due to:
- Complexity of WebBroker/ISAPI integration
- IIS-specific configuration requirements
- Availability of better alternatives (HTTP.sys via Windows Service)

See `45-isapi/README.md` for recommended deployment alternatives.

## Key Differences from DMVC

### 1. Memory Management
- **DMVC**: Requires ShareMem/BORLNDMM.DLL for string handling across DLL boundaries
- **mORMot2**: Uses `RawUtf8` (reference-counted), no external memory manager needed

### 2. Service Architecture
- **DMVC**: Controller-based with `TMVCEngine`
- **mORMot2**: Interface-based services with `TRestServer.ServiceDefine()`

### 3. HTTP Server
- **DMVC**: `TIdHTTPWebBrokerBridge` (Indy)
- **mORMot2**: `TRestHttpServer` (native, faster, HTTP.sys support)

### 4. Logging
- **DMVC**: `MVCFramework.Logger`
- **mORMot2**: `TSynLog` (more features, better performance)

### 5. Deployment Recommendation
- **DMVC**: ISAPI for IIS integration
- **mORMot2**: Windows Service with HTTP.sys (better performance, easier management)

## Compilation Results

All implemented samples compiled successfully:
```
✅ 42-server_in_dll (ServerDLL.dll + UsingServerInDLL.exe)
✅ 43-windows_service (43-windows_service.exe)
✅ 44-servercontainer (44-servercontainer.exe)
```

Platforms tested: Win32, Win64

## Testing the Samples

### Sample 42 (Server in DLL)
1. Build DLL: `dcc32 dll\ServerDLL.dpr`
2. Build host: `dcc32 host\UsingServerInDLL.dpr`
3. Run host application
4. Test: `http://localhost:8080/root/MainApi.GetMessage`

### Sample 43 (Windows Service)
1. Build: `dcc32 43-windows_service.dpr`
2. Install: `43-windows_service.exe /install`
3. Start: `net start "mORMot2 REST Service (DMVC port)"`
4. Test: `http://localhost:8080/root/ServiceApi.GetStatus`
5. Stop: `net stop "mORMot2 REST Service (DMVC port)"`
6. Uninstall: `43-windows_service.exe /uninstall`

### Sample 44 (Server Container)
1. Build: `dcc32 44-servercontainer.dpr`
2. Run: `44-servercontainer.exe`
3. Test Server 1: `http://localhost:3000/root/CalculatorApi.GetInfo`
4. Test Server 2: `http://localhost:3010/root/CalculatorApi.Add?a=5&b=3` (returns 16)
5. Test Server 3: `http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2` (returns 50)

## Implementation Notes

### DLL Exports (Sample 42)
```pascal
procedure RunServer(const Port: Integer); stdcall;
procedure StopServer; stdcall;
```

### Service Registration (All Samples)
```pascal
// Define interface
IMyApi = interface(IInvokable)
  ['{GUID}']
  function MyMethod: RawUtf8;
end;

// Register service
fServer.ServiceDefine(TMyApiImpl, [IMyApi], sicShared);
```

### Container Pattern (Sample 44)
```pascal
container := TServerContainer.Create;
container.AddServer('Server01', '3000', TCalculatorApi1);
container.AddServer('Server02', '3010', TCalculatorApi2);
container.StartAll;
```

## Lessons Learned

1. **mORMot2's interface-based services** are simpler than DMVC's controller pattern for deployment scenarios
2. **HTTP.sys integration** via `useHttpSocket` provides better IIS-like performance without ISAPI complexity
3. **Windows Service** deployment is more robust than ISAPI for production use
4. **Server containers** enable easy microservices-style architecture in a single process

## Next Steps

Future enhancements could include:
- ISAPI module implementation (if required)
- Linux daemon equivalent of Windows Service
- Docker containerization examples
- Kubernetes deployment manifests
- Load balancer configuration samples
