# 20. Hosting and Deployment

*From Development to Production*

This chapter covers deployment patterns for mORMot 2 applications, from simple stand-alone executables to complex multi-server architectures with CDN integration.

---

## 20.1. Platform Support

### 20.1.1. Operating Systems

mORMot 2 natively supports:

| Platform | Compiler | Notes |
|----------|----------|-------|
| Windows (32/64-bit) | Delphi, FPC | Full support including http.sys |
| Linux (x86_64, aarch64) | Delphi 12+, FPC | Recommended for servers |
| macOS (x86_64, aarch64) | Delphi, FPC | Development and servers |
| FreeBSD | FPC | Server deployments |
| Android | Delphi, FPC | Client applications |

### 20.1.2. Resource Requirements

mORMot applications are extremely efficient:

| Component | Typical Requirement |
|-----------|-------------------|
| **RAM** | 50-200 MB for typical server |
| **CPU** | Minimal; single-core sufficient for most workloads |
| **Storage** | SQLite3 database + application (~5-50 MB) |
| **Network** | Standard HTTP/HTTPS ports |

Compared to traditional stacks (IIS + .NET + SQL Server), mORMot requires:
- **10x less RAM**
- **Simpler configuration**
- **No additional dependencies**

---

## 20.2. Deployment Patterns

### 20.2.1. Stand-Alone Application

The simplest deployment — a single executable:

```
┌─────────────────────────────────────┐
│           Application               │
│  ┌─────────┐  ┌─────────────────┐   │
│  │ Client  │──│  Server (HTTP)  │   │
│  │  Code   │  │  + ORM + SOA    │   │
│  └─────────┘  └─────────────────┘   │
│                     │               │
│              ┌──────┴──────┐        │
│              │  SQLite3    │        │
│              │  Database   │        │
│              └─────────────┘        │
└─────────────────────────────────────┘
```

Perfect for:
- Desktop applications with local data
- Development and testing
- Single-user scenarios

```pascal
// In-process server access
var
  Server: TRestServerDB;
  Client: TRestClientDB;
begin
  Server := TRestServerDB.Create(Model, 'data.db3');
  Client := TRestClientDB.Create(Server);
  // Client and server in same process
end;
```

### 20.2.2. Shared Server

One server handling both ORM and SOA:

```
┌──────────┐        ┌──────────┐
│ Client 1 │───┐    │ Client 2 │
│ (Delphi) │   │    │  (AJAX)  │
└──────────┘   │    └──────────┘
               │         │
               ▼         ▼
         ┌─────────────────────┐
         │    HTTP Server      │
         │  ┌───────────────┐  │
         │  │   ORM + SOA   │  │
         │  └───────┬───────┘  │
         │          │          │
         │    ┌─────┴─────┐    │
         │    │  SQLite3  │    │
         │    └───────────┘    │
         └─────────────────────┘
```

Suitable for:
- Small to medium applications
- Corporate intranets
- Single-location deployments

### 20.2.3. Separated Services (DMZ)

For security-sensitive deployments:

```
    Internet                    DMZ                    Internal Network
       │                         │                           │
┌──────┴──────┐           ┌──────┴──────┐            ┌───────┴───────┐
│  AJAX/Web   │           │  Services   │            │     ORM       │
│   Clients   │─────────▶ │   Server    │──────────▶ │    Server     │
└─────────────┘           │  (Stateless)│            │  + Database   │
                          └─────────────┘            └───────────────┘
                                │
                          ┌─────┴─────┐
                          │  Firewall │
                          └───────────┘
```

Benefits:
- Database never exposed to Internet
- Services can be stateless (scalable)
- Clear security boundaries

Implementation:

```pascal
// Services server (in DMZ)
Server := TRestServerFullMemory.Create(Model);
Server.ServiceDefine(TMyService, [IMyService], sicShared);
// Connect to internal ORM server
Server.RemoteDataCreate(InternalOrmClient, TOrmArticle);

// Internal ORM server
OrmServer := TRestServerDB.Create(Model, 'data.db3');
```

### 20.2.4. Microservices

Multiple specialized servers:

```
┌─────────┐   ┌─────────┐   ┌─────────┐
│ Client  │   │ Client  │   │ C│
└────┬────┘   └────┬────┘   └────┬────┘
     │             │             │
     └──────┬──────┴──────┬──────┘
            ▼             │
     ┌──────────────┐     │
     │   Gateway    │     │
     │   Server     │     │
     └──────┬───────┘     │
            │             │
     ┌──────┼──────┬──────┼──────┐
     ▼      ▼      ▼      ▼      ▼
┌────────┐ ┌────────┐ ┌────────┐
│ Auth   │ │ Orders │ │ Reports│
│ Service│ │ Service│ │ Service│
└────────┘ └────────┘ └────────┘
```

Each service:
- Has its own database
- Independently deployable
- Communicates via REST/JSON

---

## 20.3. Windows Deployment

### 20.3.1. Console Application

Simplest deployment for development:

```pascal
program MyServer;
{$APPTYPE CONSOLE}
begin
  // Server initialization
  WriteLn('Server running on http://localhost:8080');
  ReadLn;  // Wait for Enter to stop
end.
```

### 20.3.2. Windows Service

For production deployment:

```pascal
program MyServerService;

uses
  mormot.app.daemon;

type
  TMyDaemon = class(TDaemon)
  protected
    fServer: TRestServerDB;
    fHttp: TRestHttpServer;
    procedure DoStart; override;
    procedure DoStop; override;
  end;

procedure TMyDaemon.DoStart;
begin
  fServer := TRestServerDB.Create(Model, 'data.db3');
  fHttp := TRestHttpServer.Create('8080', [fServer]);
end;

procedure TMyDaemon.DoStop;
begin
  fHttp.Free;
  fServer.Free;
end;

begin
  TDaemonService.Create(TMyDaemon, 'MyServer', 'My mORMot Server');
  TDaemonService.RunAsService;
end.
```

Service management:
```batch
:: Install service
MyServerService.exe /install

:: Start service
net start MyServer

:: Stop service
net stop MyServer

:: Uninstall service
MyServerService.exe /uninstall
```

### 20.3.3. HTTP.SYS Configuration

For best Windows performance, use http.sys:

```pascal
HttpServer := TRestHttpServer.Create('8080', [Server], '+',
  useHttpApiRegisteringUri);  // Uses http.sys
```

URL reservation (run as Administrator):
```batch
netsh http add urlacl url=http://+:8080/ user=Everyone
```

SSL certificate binding:
```batch
netsh http add sslcert ipport=0.0.0.0:443 ^
  certhash=YOUR_CERT_THUMBPRINT ^
  appid={YOUR_APP_GUID}
```

---

## 20.4. Linux Deployment

### 20.4.1. Systemd Service

Create `/etc/systemd/system/mormot-server.service`:

```ini
[Unit]
Description=mORMot Server
After=network.target

[Service]
Type=simple
User=mormot
Group=mormot
WorkingDirectory=/opt/mormot
ExecStart=/opt/mormot/myserver
Restart=always
RestartSec=5
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

Management:
```bash
# Enable and start
sudo systemctl enable mormot-server
sudo systemctl start mormot-server

# Check status
sudo systemctl status mormot-server

# View logs
sudo journalctl -u mormot-server -f
```

### 20.4.2. Docker Deployment

`Dockerfile`:
```dockerfile
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    libsqlite3-0 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY myserver /app/
COPY Views/ /app/Views/

EXPOSE 8080
USER nobody:nogroup

CMD ["/app/myserver"]
```

`docker-compose.yml`:
```yaml
version: '3.8'
services:
  mormot-server:
    build: .
    ports:
      - "8080:8080"
    volumes:
      - ./data:/app/data
    restart: unless-stopped
    environment:
      - MORMOT_LOG_LEVEL=debug
```

### 20.4.3. Performance Tuning

System limits (`/etc/security/limits.conf`):
```
mormot soft nofile 65535
mormot hard nofile 65535
```

Kernel parameters (`/etc/sysctl.conf`):
```
net.core.somaxconn = 65535
net.ipv4.tcp_max_syn_backlog = 65535
net.ipv4.ip_local_port_range = 1024 65535
```

---

## 20.5. Process Manager (Angelize)

### 20.5.1. Overview

`TSynAngelize` is mORMot's cross-platform process manager — similar to NSSM (Windows) or supervisord (Linux), but JSON-configured and integrated with the daemon framework.

**Key Features:**
- Launch and monitor multiple sub-processes
- Auto-restart on crash with configurable backoff
- Health checks via HTTP endpoints
- Notifications on failure (email, HTTP, log, exec)
- Cross-platform (Windows services + POSIX daemons)
- Console output redirection and rotation

### 20.5.2. Architecture

```
┌─────────────────────────────────────────────────┐
│              TSynAngelize                        │
│         (Main Launcher/Watcher)                  │
│                                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌──────────┐ │
│  │ Service 1   │  │ Service 2   │  │ Service N│ │
│  │ (API Server)│  │ (Worker)    │  │ (...)    │ │
│  └─────────────┘  └─────────────┘  └──────────┘ │
│         │               │               │        │
│    [Watch]         [Watch]         [Watch]       │
│    Health          Process         Custom        │
│    Check           Monitor         Script        │
└─────────────────────────────────────────────────┘
```

### 20.5.3. Configuration

Configuration stored in `<exename>.settings` JSON file:

```json
{
  "Services": [
    {
      "Name": "api",
      "Description": "REST API Server",
      "Run": "/opt/app/api-server",
      "Start": [ "start:%run%" ],
      "Stop": [ "stop:%run%" ],
      "Watch": [ "http://127.0.0.1:8080/health=200" ],
      "WatchDelaySec": 60,
      "RetryStableSec": 120,
      "RedirectLogFile": "%log%api-console.log",
      "RedirectLogRotateFiles": 5,
      "RedirectLogRotateBytes": 10485760,
      "Notify": "admin@example.com,%log%alerts.log"
    },
    {
      "Name": "worker",
      "Description": "Background Worker",
      "Run": "/opt/app/worker",
      "Start": [ "start:%run%" ],
      "Stop": [ "stop:%run%" ],
      "AbortExitCodes": [ 99 ]
    }
  ]
}
```

### 20.5.4. Action Commands

Actions in `Start`, `Stop`, and `Watch` arrays:

| Action | Description |
|--------|-------------|
| `start:/path/to/exe` | Launch and monitor process (like NSSM) |
| `stop:/path/to/exe` | Stop monitored process |
| `exec:/path/to/exe` | Execute without waiting |
| `wait:/path/to/exe` | Execute and wait for exit code 0 |
| `http://host/path` | HTTP GET request (expects 200) |
| `http://host/path=CODE` | HTTP GET expecting specific status code |
| `sleep:1000` | Wait N milliseconds |
| `service:Name` | Control Windows service (Windows only) |

**Variables:**
- `%run%` — Expands to the `Run` property value
- `%log%` — Expands to the log directory path

### 20.5.5. Auto-Restart Behavior

When a monitored process exits unexpectedly:

1. **Immediate restart** attempted
2. If crash recurs within `RetryStableSec`, **backoff delay** increases
3. Process considered "stable" after running `RetryStableSec` seconds
4. `AbortExitCodes` can prevent restart (e.g., exit code 99 = don't restart)
5. `Notify` triggered on persistent failures

### 20.5.6. Health Checks

The `Watch` array defines periodic health checks:

```json
{
  "Watch": [
    "http://127.0.0.1:8080/health=200",
    "http://127.0.0.1:8080/ready"
  ],
  "WatchDelaySec": 30
}
```

If health check fails:
1. Process is considered unhealthy
2. Restart triggered
3. Notification sent if `Notify` configured

### 20.5.7. Usage Example

```pascal
program ProcessManager;

uses
  mormot.app.agl;

type
  TMyAngelize = class(TSynAngelize)
  end;

begin
  with TMyAngelize.Create(TSynAngelizeSettings, '', '', '') do
  try
    CommandLine;  // Handles install/start/stop/console
  finally
    Free;
  end;
end.
```

Command line:
```bash
# Windows
ProcessManager.exe /install
ProcessManager.exe /start
ProcessManager.exe /console  # Run in foreground for debugging

# Linux
./ProcessManager --run       # Run as daemon
./ProcessManager --console   # Run in foreground
./ProcessManager --kill      # Stop daemon
```

### 20.5.8. Comparison with Alternatives

| Feature | Angelize | NSSM | supervisord | systemd |
|---------|----------|------|-------------|---------|
| Cross-platform | ✅ | ❌ Windows | ❌ Linux | ❌ Linux |
| JSON config | ✅ | ❌ | ❌ INI | ❌ Unit files |
| Health checks | ✅ HTTP | ❌ | ✅ | ✅ |
| Log rotation | ✅ | ❌ | ✅ | ✅ journald |
| Notifications | ✅ Email/HTTP/Log | ❌ | ❌ | ❌ |
| mORMot integration | ✅ Native | ❌ | ❌ | ❌ |

---

## 20.6. Reverse Proxy Configuration

### 20.6.1. Nginx

```nginx
upstream mormot {
    server 127.0.0.1:8080;
    keepalive 32;
}

server {
    listen 80;
    server_name api.example.com;

    location / {
        proxy_pass http://mormot;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Connection "";

        # WebSocket support
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
    }

    # Static files served by Nginx
    location /.static/ {
        alias /opt/mormot/static/;
        expires 30d;
    }
}
```

### 20.6.2. Apache

```apache
<VirtualHost *:80>
    ServerName api.example.com

    ProxyPreserveHost On
    ProxyPass / http://127.0.0.1:8080/
    ProxyPassReverse / http://127.0.0.1:8080/

    # WebSocket support
    RewriteEngine On
    RewriteCond %{HTTP:Upgrade} =websocket [NC]
    RewriteRule /(.*) ws://127.0.0.1:8080/$1 [P,L]
</VirtualHost>
```

### 20.6.3. SSL Termination

With Let's Encrypt (certbot):
```bash
sudo certbot --nginx -d api.example.com
```

Or manually in Nginx:
```nginx
server {
    listen 443 ssl http2;
    server_name api.example.com;

    ssl_certificate /etc/letsencrypt/live/api.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.example.com/privkey.pem;

    # Modern SSL configuration
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256;
    ssl_prefer_server_ciphers off;

    location / {
        proxy_pass http://127.0.0.1:8080;
        # ... proxy settings
    }
}
```

---

## 20.7. CDN Integration

### 20.7.1. Architecture

```
┌─────────┐     ┌─────────┐     ┌─────────┐
│ Client  │     │ Client  │     │ Client  │
│  (US)   │     │  (EU)   │     │ (Asia)  │
└────┬────┘     └────┬────┘     └────┬────┘
     │               │               │
     ▼               ▼               ▼
┌─────────┐     ┌─────────┐     ┌─────────┐
│ CDN     │     │ CDN     │     │ CDN     │
│ Edge US │     │ Edge EU │     │Edge Asia│
└────┬────┘     └────┬────┘     └────┬────┘
     │               │               │
     └───────────────┼───────────────┘
                     │
                     ▼
              ┌─────────────┐
              │   Origin    │
              │   Server    │
              │  (mORMot)   │
              └─────────────┘
```

### 20.7.2. Cache Headers

Enable caching for appropriate endpoints:

```pascal
procedure TMyServer.GetPublicData(Ctxt: TRestServerUriContext);
begin
  Ctxt.Returns(Data, HTTP_SUCCESS,
    'Content-Type: application/json'#13#10 +
    'Cache-Control: public, max-age=300',  // 5 minutes
    True);  // Handle304NotModified
end;
```

### 20.7.3. Cloudflare Configuration

Page Rules:
- `api.example.com/public/*` → Cache Everything, Edge TTL: 5 minutes
- `api.example.com/auth/*` → Bypass Cache
- `api.example.com/api/*` → Bypass Cache (authenticated)

Important: Authenticated endpoints must bypass cache:

```pascal
// Disable authentication for cacheable endpoints
Server.ServiceMethodByPassAuthentication('GetPublicData');
```

---

## 20.8. Monitoring and Logging

### 20.8.1. Built-in Logging

```pascal
// Configure logging
with TSynLog.Family do
begin
  Level := LOG_VERBOSE;
  DestinationPath := '/var/log/mormot/';
  RotateFileCount := 10;
  RotateFileSizeKB := 10240;  // 10 MB per file
end;
```

### 20.8.2. Health Checks

```pascal
procedure TMyServer.Health(Ctxt: TRestServerUriContext);
var
  Status: TDocVariantData;
begin
  Status.InitObject([
    'status', 'ok',
    'timestamp', NowUtc,
    'uptime', GetTickCount64 - fStartTime,
    'connections', fActiveConnections
  ]);
  Ctxt.Returns(Status.ToJson);
end;

// Register without authentication
Server.ServiceMethodByPassAuthentication('Health');
```

### 20.8.3. Metrics Endpoint

```pascal
procedure TMyServer.Metrics(Ctxt: TRestServerUriContext);
var
  Info: TDocVariantData;
begin
  Info.InitObject([
    'requests_total', fRequestCount,
    'requests_per_second', fRequestsPerSecond,
    'memory_mb', GetHeapStatus.TotalAllocated div (1024*1024),
    'db_connections', Server.DB.ConnectionCount,
    'active_sessions', Server.Sessions.Count
  ]);
  Ctxt.Returns(Info.ToJson);
end;
```

---

## 20.9. High Availability

### 20.9.1. Load Balancing

Multiple mORMot instances behind a load balancer:

```nginx
upstream mormot_cluster {
    least_conn;
    server 10.0.0.1:8080 weight=5;
    server 10.0.0.2:8080 weight=5;
    server 10.0.0.3:8080 backup;
    keepalive 32;
}
```

### 20.9.2. Session Affinity

For stateful services, use sticky sessions:

```nginx
upstream mormot_cluster {
    ip_hash;  # Same client always goes to same server
    server 10.0.0.1:8080;
    server 10.0.0.2:8080;
}
```

Or use external session storage (Redis):

```pascal
// Store sessions externally
Server.SessionClass := TAuthSessionRedis;
```

### 20.9.3. Database Replication

For high availability with SQLite3:

```pascal
// Master server
MasterServer := TRestServerDB.Create(Model, 'master.db3');

// Replica servers (read-only)
ReplicaServer := TRestServerDB.Create(Model, 'replica.db3');
ReplicaServer.DB.OpenV2('replica.db3', SQLITE_OPEN_READONLY);
```

Or use external databases with built-in replication (PostgreSQL, MySQL).

---

## 20.10. Security Checklist

### 20.10.1. Network Security

- [ ] Use HTTPS in production
- [ ] Configure firewall (only expose needed ports)
- [ ] Use reverse proxy for SSL termination
- [ ] Enable rate limiting
- [ ] Configure CORS properly

### 20.10.2. Application Security

- [ ] Enable authentication for sensitive endpoints
- [ ] Use strong password hashing (SHA-256 + salt)
- [ ] Implement proper authorization (per-method)
- [ ] Validate all input
- [ ] Sanitize output (automatic with Mustache)

### 20.10.3. Server Hardening

- [ ] Run as non-root user
- [ ] Minimize installed packages
- [ ] Keep system updated
- [ ] Configure log rotation
- [ ] Set up monitoring/alerting

---

## Summary

mORMot 2 deployment options:

| Scenario | Recommended Setup |
|----------|------------------|
| Development | Console application |
| Windows Production | Windows Service + http.sys |
| Linux Production | systemd + Nginx |
| Containers | Docker with Alpine/Debian |
| High Traffic | Load balancer + CDN |
| High Availability | Cluster + session sharing |

Key takeaways:
- mORMot requires minimal resources
- Single executable deployment
- Native cross-platform support
- Easy integration with standard infrastructure
- Built-in logging and monitoring support

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 19: MVC/MVVM Web Applications](mORMot2-SAD-Chapter-19.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 21: Security](mORMot2-SAD-Chapter-21.md) |
