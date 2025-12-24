# 42 - Server in DLL

mORMot2 port of DMVC `server_in_dll` sample.

## Overview

This sample demonstrates how to host a mORMot2 REST server inside a DLL that can be loaded by any Windows application.

## Key Differences from DMVC

- **Memory Management**: DMVC required ShareMem/BORLNDMM.DLL for string handling across DLL boundaries. mORMot2 uses `RawUtf8` which is reference-counted and doesn't require external memory managers.
- **Interface-based Services**: Uses mORMot2's native interface-based services instead of DMVC controllers.
- **Logging**: Uses TSynLog instead of DMVC's logging system.

## Projects

### ServerDLL.dpr (dll/)
The REST server DLL that exports two functions:
- `RunServer(Port: Integer)` - Starts the REST server on the specified port
- `StopServer()` - Stops the REST server

### UsingServerInDLL.dpr (host/)
Host application that loads the DLL and controls the server lifecycle.

## Building

1. Build the DLL first: `dcc32 dll\ServerDLL.dpr` or `dcc64 dll\ServerDLL.dpr`
2. Build the host: `dcc32 host\UsingServerInDLL.dpr` or `dcc64 host\UsingServerInDLL.dpr`
3. Run the host application

## Testing

Once running, test with:
```
http://localhost:8080/root/MainApi.GetMessage
http://localhost:8080/root/MainApi.Divide?a=10&b=2
```

## Implementation Notes

- The DLL maintains a global `gServer` instance
- Uses `stdcall` convention for DLL exports
- REST endpoints are exposed via interface-based services
- Supports both Win32 and Win64 platforms
