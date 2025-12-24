# 44 - Server Container

mORMot2 port of DMVC `servercontainer` sample.

## Overview

This sample demonstrates how to run multiple REST servers in a single process, each listening on different ports with different API implementations.

## Key Differences from DMVC

- **Server Management**: Custom `TServerContainer` class manages multiple mORMot2 HTTP servers
- **Interface-based Services**: Uses mORMot2's native interface-based services instead of DMVC controllers
- **Custom Implementations**: Each server uses a different `ICalculatorApi` implementation
- **No WebModule**: Direct REST server instances instead of WebBroker framework

## Servers

The sample runs three servers simultaneously:

### Server 1 (Port 3000)
- Standard calculator with enhanced info
- Endpoint: `http://localhost:3000/root/CalculatorApi.GetInfo`

### Server 2 (Port 3010)
- Calculator with doubled addition results
- Test: `http://localhost:3010/root/CalculatorApi.Add?a=5&b=3` returns 16 instead of 8

### Server 3 (Port 3020)
- Calculator with 10x division results
- Test: `http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2` returns 50 instead of 5

## Building and Running

Build the project:
```cmd
dcc32 44-servercontainer.dpr
```

Run:
```cmd
44-servercontainer.exe
```

## Testing

Test each server:
```
curl http://localhost:3000/root/CalculatorApi.GetInfo
curl http://localhost:3010/root/CalculatorApi.Add?a=5&b=3
curl http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2
```

## Implementation Notes

- All servers share the same `ICalculatorApi` interface
- Each server has its own `TRestServerFullMemory` instance
- Each server has its own `TRestHttpServer` instance
- Container manages lifecycle of all servers
- Supports both Win32 and Win64 platforms
- Uses class inheritance to customize behavior per server
