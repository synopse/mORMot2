# ISAPI Sample - mORMot2 REST Server as ISAPI Module

This sample demonstrates how to deploy a mORMot2 REST server as an ISAPI module for IIS.

## Overview

ISAPI (Internet Server Application Programming Interface) allows you to create DLLs that extend IIS functionality. This sample shows how to wrap a mORMot2 REST server in an ISAPI extension.

**Architecture**:
```
IIS → ISAPI Extension (DLL) → ISAPI Handler → mORMot2 REST Server
```

The ISAPI module:
1. Receives `EXTENSION_CONTROL_BLOCK` from IIS
2. Converts to mORMot2 HTTP request format
3. Processes through mORMot2 REST server
4. Returns response to IIS

## Files

- `45-isapi.dpr` - ISAPI library entry point
- `src/server.pas` - mORMot2 REST server implementation
- `src/isapi.handler.pas` - ISAPI-to-mORMot2 bridge
- `src/api.interfaces.pas` - Service interface definitions
- `src/api.impl.pas` - Service implementations
- `src/entities.pas` - ORM entity definitions

## Building

```bash
# Compile with Delphi 12
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe /mnt/w/Packages290/mORMot2/45-isapi.dproj
```

Output: `45-isapi.dll`

## IIS Deployment

1. **Copy DLL to IIS folder**:
   ```
   copy 45-isapi.dll C:\inetpub\wwwroot\api\
   ```

2. **Configure IIS**:
   - Open IIS Manager
   - Select your website/application
   - Add ISAPI Filter or create Virtual Directory pointing to DLL
   - Ensure IIS application pool has appropriate permissions

3. **Enable ISAPI**:
   - In IIS Manager → Server → ISAPI and CGI Restrictions
   - Add `45-isapi.dll` and set to "Allowed"

4. **Test**:
   ```
   http://localhost/api/45-isapi.dll/timestamp
   http://localhost/api/45-isapi.dll/divide/10/2
   ```

## Limitations

- This is a demonstration of ISAPI integration technique
- For production, consider using mORMot2's `TRestHttpServer` with http.sys on Windows
- ISAPI has thread pooling limitations compared to modern async servers
- IIS adds overhead compared to standalone mORMot2 servers

## Performance Considerations

mORMot2's native HTTP servers (`TRestHttpServer`, `THttpAsyncServer`) are typically faster than ISAPI because:
- No IIS overhead
- Direct socket control
- Optimized thread pools
- Support for async I/O

Use ISAPI when:
- You need IIS integration (authentication, logging, management)
- Corporate environment requires IIS
- You need reverse proxy features built into IIS

## Alternative Deployment Methods

For better performance on Windows, consider:

1. **http.sys mode** (recommended for Windows):
   ```pascal
   Server := TRestHttpServer.Create('8080', [RestServer], '+', useHttpApiRegisteringURI);
   ```

2. **Standalone socket server**:
   ```pascal
   Server := TRestHttpServer.Create('8080', [RestServer], '+', HTTP_DEFAULT_MODE);
   ```

3. **Windows Service** (see sample 43-windows_service)

## See Also

- Sample 42: Server in DLL (generic DLL hosting)
- Sample 43: Windows Service (service deployment)
- mORMot2 SAD Chapter 11: Client-Server Architecture
- mORMot2 SAD Chapter 20: Application Servers
