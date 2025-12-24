# Deployment Samples - Quick Start Guide

## 5-Minute Overview

Choose your deployment scenario:

### 🚀 Windows Service (Recommended for Production)
**Sample:** 43-windows_service

**When to use:**
- Production Windows deployment
- Need automatic startup on system boot
- Background service without console
- Integration with Windows service management

**Quick Start:**
```cmd
cd /mnt/w/mORMot2/ex/dmvc/43-windows_service
dcc32 43-windows_service.dpr
43-windows_service.exe /install
net start "mORMot2 REST Service (DMVC port)"
```

Test: `http://localhost:8080/root/ServiceApi.GetStatus`

---

### 📦 DLL Hosting (For Third-Party Integration)
**Sample:** 42-server_in_dll

**When to use:**
- Embed REST server in existing application
- Plugin architecture
- Need to control server from non-Delphi application
- Dynamic loading/unloading

**Quick Start:**
```cmd
cd /mnt/w/mORMot2/ex/dmvc/42-server_in_dll
dcc32 dll\ServerDLL.dpr
dcc32 host\UsingServerInDLL.dpr
host\Win32\Debug\UsingServerInDLL.exe
```

Test: `http://localhost:8080/root/MainApi.GetMessage`

---

### 🔀 Multiple Servers (Microservices Pattern)
**Sample:** 44-servercontainer

**When to use:**
- Multiple APIs in one process
- Different services on different ports
- Testing microservices locally
- Development environment

**Quick Start:**
```cmd
cd /mnt/w/mORMot2/ex/dmvc/44-servercontainer
dcc32 44-servercontainer.dpr
44-servercontainer.exe
```

Test:
- Server 1: `http://localhost:3000/root/CalculatorApi.GetInfo`
- Server 2: `http://localhost:3010/root/CalculatorApi.Add?a=5&b=3`
- Server 3: `http://localhost:3020/root/CalculatorApi.Divide?a=10&b=2`

---

## Decision Tree

```
Need deployment on Windows?
├─ Yes
│  ├─ Production server?
│  │  └─ YES → Use Sample 43 (Windows Service)
│  │
│  ├─ Embed in existing app?
│  │  └─ YES → Use Sample 42 (DLL)
│  │
│  └─ Multiple services for testing?
│     └─ YES → Use Sample 44 (Container)
│
└─ No (Linux/Docker)
   └─ Use standalone samples (01-02) + systemd/Docker
```

## Common Commands

### Windows Service (Sample 43)
```cmd
# Install
43-windows_service.exe /install

# Start
net start "mORMot2 REST Service (DMVC port)"

# Stop
net stop "mORMot2 REST Service (DMVC port)"

# Uninstall
43-windows_service.exe /uninstall

# Check status
sc query "mORMot2RestService"
```

### DLL Server (Sample 42)
```pascal
// In your application
var
  DllHandle: THandle;
  RunServer: procedure(Port: Integer); stdcall;
  StopServer: procedure; stdcall;

DllHandle := LoadLibrary('ServerDLL.dll');
@RunServer := GetProcAddress(DllHandle, 'RunServer');
@StopServer := GetProcAddress(DllHandle, 'StopServer');

RunServer(8080);  // Start server
// ... use server ...
StopServer();     // Stop server
FreeLibrary(DllHandle);
```

### Server Container (Sample 44)
```pascal
// Add more servers
container := TServerContainer.Create;
container.AddServer('MyAPI', '4000', TMyApiImpl);
container.AddServer('OtherAPI', '4010', TOtherApiImpl);
container.StartAll;
```

## Testing Your Deployment

### Using cURL
```bash
# Windows Service
curl http://localhost:8080/root/ServiceApi.GetStatus

# DLL Server
curl http://localhost:8080/root/MainApi.Divide?a=10&b=2

# Container
curl http://localhost:3000/root/CalculatorApi.GetInfo
```

### Using PowerShell
```powershell
# Windows Service
Invoke-RestMethod http://localhost:8080/root/ServiceApi.GetStatus

# DLL Server
Invoke-RestMethod "http://localhost:8080/root/MainApi.Divide?a=10&b=2"

# Container
Invoke-RestMethod http://localhost:3000/root/CalculatorApi.GetInfo
```

### Using Browser
Just open the URLs in any browser:
- http://localhost:8080/root/ServiceApi.GetStatus
- http://localhost:8080/root/MainApi.GetMessage
- http://localhost:3000/root/CalculatorApi.GetInfo

## Troubleshooting

### Port Already in Use
```cmd
# Find process using port
netstat -ano | findstr :8080

# Kill process (use PID from above)
taskkill /PID <pid> /F
```

### Service Won't Start
```cmd
# Check service status
sc query mORMot2RestService

# Check event log
eventvwr.msc
# Navigate to: Windows Logs → Application
```

### DLL Not Found
```cmd
# Verify DLL is in same directory as EXE
dir ServerDLL.dll

# Or add to system PATH
set PATH=%PATH%;C:\Path\To\DLL
```

### Can't Access from Network
```cmd
# Check firewall
netsh advfirewall firewall add rule name="mORMot2" dir=in action=allow protocol=TCP localport=8080

# Verify HTTP.sys URL reservation (if using HTTP.sys)
netsh http show urlacl
```

## Production Deployment Checklist

### Windows Service Deployment

- [ ] Build in Release mode
- [ ] Test service install/uninstall
- [ ] Configure automatic startup
- [ ] Set up logging directory
- [ ] Configure firewall rules
- [ ] Set appropriate service account permissions
- [ ] Test service recovery options
- [ ] Document port numbers
- [ ] Set up monitoring
- [ ] Configure SSL/TLS if needed

### DLL Deployment

- [ ] Build in Release mode
- [ ] Test on target system
- [ ] Include all dependencies
- [ ] Document DLL interface
- [ ] Version the DLL
- [ ] Test loading/unloading
- [ ] Handle errors gracefully
- [ ] Document memory requirements

### Container Deployment

- [ ] Build in Release mode
- [ ] Document all ports
- [ ] Test port conflicts
- [ ] Configure health checks
- [ ] Set up logging
- [ ] Document shutdown procedure
- [ ] Test resource limits
- [ ] Monitor memory usage

## Performance Tips

1. **Use HTTP.sys for production** (Windows Service)
   ```pascal
   fHttpServer := TRestHttpServer.Create('80', [fServer], '+', useHttpSocket);
   ```

2. **Enable compression** for better network performance
   ```pascal
   fHttpServer.CompressGz;
   ```

3. **Configure thread pool** for high load
   ```pascal
   fServer.AcquireExecutionMode[execSoaByInterface] := amBackgroundThread;
   ```

4. **Use shared instances** for stateless services
   ```pascal
   fServer.ServiceDefine(TMyApi, [IMyApi], sicShared);
   ```

## Next Steps

1. **Read the full documentation:** `/mnt/w/mORMot2/ex/dmvc/docs/DEPLOYMENT-SAMPLES.md`
2. **Check sample README files** for detailed information
3. **Explore other samples** for additional features
4. **Review mORMot2 documentation** for advanced configuration

## Support

- **mORMot2 Documentation:** https://synopse.info/fossil/wiki?name=mORMot
- **Sample Source Code:** `/mnt/w/mORMot2/ex/dmvc/42-45-*`
- **DMVC Comparison:** See `DEPLOYMENT-SAMPLES.md`
