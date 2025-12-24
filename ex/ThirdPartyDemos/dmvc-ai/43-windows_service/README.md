# 43-windows_service - mORMot2 Windows Service Sample

Port of DelphiMVCFramework's `windows_service` sample to mORMot2.

## Overview

This sample demonstrates how to create a **Windows Service** that hosts a mORMot2 REST server using mORMot2's native `TServiceSingle` class from `mormot.core.os`.

### Key Features

- **Native mORMot2 Service**: Uses `TServiceSingle` and `ServiceSingleRun` from `mormot.core.os` (no VCL dependencies)
- **Windows Service Management**: Install, uninstall, start, stop via command line
- **Console Mode**: Run as console application for testing
- **REST API**: Simple service status and echo endpoints
- **Logging**: Full mORMot2 logging with `TSynLog`

## Architecture

### Service Implementation

The service uses mORMot2's service infrastructure:

- **`TServiceSingle`**: Base class for Windows services (from `mormot.core.os`)
- **`ServiceSingleRun`**: Global function to launch the service
- **Event Handlers**: `OnStart`, `OnStop`, `OnExecute` for service lifecycle
- **`TRestHttpServer`**: HTTP server running in background threads

### REST API

Simple interface-based service API:

```pascal
IServiceApi = interface(IInvokable)
  function GetStatus: TServiceStatus;
  function Echo(const aMessage: RawUtf8): RawUtf8;
end;
```

## Usage

### Install Service

```cmd
43-windows_service.exe /install
```

### Start Service

```cmd
net start "mORMot2 REST Service"
```

### Stop Service

```cmd
net stop "mORMot2 REST Service"
```

### Uninstall Service

```cmd
43-windows_service.exe /remove
```

### Console Mode (Testing)

```cmd
43-windows_service.exe /console
```

Press Enter to quit.

## Testing Endpoints

Once running (as service or console):

### Get Service Status
```
GET http://localhost:8080/root/ServiceApi.GetStatus
```

Returns:
```json
{
  "Running": true,
  "Uptime": 123,
  "Port": 8080,
  "Message": "Service running for 123 seconds"
}
```

### Echo Test
```
GET http://localhost:8080/root/ServiceApi.Echo?aMessage=hello
```

Returns:
```json
"Echo: hello"
```

## Comparison with DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| Service Base | `Vcl.SvcMgr.TService` | `mormot.core.os.TServiceSingle` |
| HTTP Server | DMVC HTTP Server | `TRestHttpServer` |
| REST API | DMVC Controllers | Interface-based services |
| Dependencies | VCL required | No VCL (RTL-only) |

## Files

- **`43-windows_service.dpr`**: Main program with service management logic
- **`src/service.impl.pas`**: Windows Service implementation (`TRestServiceImpl`)
- **`src/api.interfaces.pas`**: REST API interface definition (`IServiceApi`)
- **`src/api.impl.pas`**: REST API implementation (`TServiceApi`)

## Notes

### AppType
The `.dproj` has `<AppType>Application</AppType>` as required for Windows services (not `Console`).

### Service Name
Service is registered as `"mORMot2 REST Service"` in Windows Service Manager.

### Logging
Logs are written to the executable directory using mORMot2's `TSynLog` system.

### Port
Default port is `8080`. To change, modify the `PORT` constant in `service.impl.pas`.

## See Also

- Original DMVC sample: DelphiMVCFramework `samples/windows_service/`
- mORMot2 service documentation: `mormot.core.os.pas` (TService, TServiceSingle)
- mORMot2 REST server: `mormot.rest.http.server.pas` (TRestHttpServer)
