# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

The `mormot.net.*` units provide the complete network communication layer for the mORMot 2 framework, implementing high-performance client/server protocols with cross-platform support (Windows/Linux/BSD/MacOS).

## Architecture Layers

### Foundation Layer (Low-Level)

**mormot.net.sock** - Cross-platform socket abstraction
- Encapsulates OS-specific APIs (WinSock2/POSIX) behind unified `TNetSocket` wrapper
- Platform-specific code in `.inc` files: `mormot.net.sock.windows.inc` and `mormot.net.sock.posix.inc`
- Key abstractions:
  - `TNetSocket` - Platform-agnostic socket handle with methods
  - `TNetAddr` - Universal address structure (IPv4/IPv6/Unix domain)
  - `TCrtSocket` - Buffered socket I/O class
  - `TUri` - URL parsing/generation
- TLS/HTTPS abstract layer (OpenSSL, SChannel, SecureTransport backends)
- Cross-platform socket polling: IOCP (Windows), epoll (Linux), kqueue (BSD/macOS)
- NTP/SNTP client implementation

**Important**: Low-level socket APIs are intentionally not exposed - always use the framework's abstractions.

### Protocol Layer (Mid-Level)

**mormot.net.http** - HTTP/HTTPS state machine and utilities
- Reusable HTTP parser (`THttpRequestContext`)
- `THttpSocket` - HTTP over plain sockets
- Compression framework (`THttpSocketCompressList`) supporting gzip/deflate/custom
- Shared server-side types and logging/monitoring

**mormot.net.client** - HTTP clients
- `THttpClientSocket` - Socket-based client
- `THttpRequest` - Abstract client interface with multiple backends:
  - `TWinHttp` - Windows HTTP API (production recommended)
  - `TWinINet` - WinINet-based (legacy)
  - `TCurlHttp` - libcurl-based (cross-platform)
- `TSimpleHttpClient` - High-level wrapper with `IHttpClient` interface
- `THttpMultiPartStream` - RFC 2488 multipart/formdata POST
- SMTP client for email sending
- DNS resolution cache

**mormot.net.server** - HTTP/UDP servers
- `THttpServer` - Multi-threaded HTTP/1.1 server over sockets
- `THttpApiServer` - Windows http.sys integration (high performance)
- `TUriRouter` - Radix tree-based URI routing (~26M lookups/sec)
  - Supports parametrized routes: `/users/<id>` → extracts `<id>`
  - URI rewriting with placeholders
  - Per-HTTP-method trees (GET/POST/PUT/DELETE/OPTIONS/HEAD/PATCH)
- `TUdpServerThread` - Abstract UDP server base

### Asynchronous Layer (High-Performance)

**mormot.net.async** - Event-driven architecture
- `TPollAsyncSockets` - Non-blocking connection manager
  - Uses IOCP on Windows, epoll on Linux, kqueue on BSD/macOS
  - Handles thousands of concurrent connections efficiently
- `THttpAsyncServer` - Async HTTP/1.1 server
- `THttpProxyServer` - HTTP proxy with caching
- Used by relay and RTSP modules for scalability

### WebSockets Layer

**mormot.net.ws.core** - WebSockets protocol core
- Frame encoding/decoding per RFC 6455
- Protocol negotiation and extensions
- `TWebSocketProtocolChat` - Simple text protocol
- Socket.IO / Engine.IO raw protocol support

**mormot.net.ws.client** - WebSockets client
- `THttpClientWebSockets` - Bidirectional REST client over WS

**mormot.net.ws.server** - WebSockets server
- `TWebSocketServerSocket` - Bidirectional REST server over WS

**mormot.net.ws.async** - Async WebSockets server
- `TWebSocketAsyncServer` - Event-driven WS server
- `TWebSocketAsyncServerRest` - Async bidirectional REST

### Specialized Protocols

**mormot.net.relay** - Secured tunneling over WebSockets
- Firewall traversal solution: clients connect to public relay, which tunnels to private server
- Enables remote access to servers behind corporate firewalls without configuration
- Frame validation and optional over-encryption
- See diagram in source for architecture (Public Relay ↔ Private Relay ↔ ORM/SOA Server)

**mormot.net.rtsphttp** - RTSP over HTTP tunneling
- Apple's RTSP tunneling protocol (POST for commands, GET for video stream)
- Used for streaming video through firewalls

**mormot.net.tunnel** - TCP/UDP port forwarding
- Local NAT client/server for stream tunneling

**mormot.net.acme** - ACME v2 client (Let's Encrypt/ZeroSSL)
- Automatic TLS certificate management
- JWS HTTP client implementation
- HTTP-01 challenge server (port 80)
- **Requires**: `USE_OPENSSL` conditional define

**mormot.net.ldap** - LDAP client
- CLDAP client functions
- LDIF data interchange format
- HTTP BASIC authentication via LDAP/Kerberos
- `TLdapCheckMember` for group membership

**mormot.net.dns** - DNS protocol client
- Low-level DNS query/response handling
- Used by socket layer for hostname resolution with caching

**mormot.net.tftp.client** - TFTP protocol definitions
- RFC 1350/2347/2348/2349/7440 support
- **Note**: Client implementation not yet complete (protocol only)

**mormot.net.tftp.server** - TFTP server
- `TTftpServerThread` implementation
- **Note**: Only RRQ (read requests) currently supported/tested

**mormot.net.openapi** - OpenAPI/Swagger client generator
- Parse OpenAPI 3.x JSON specifications
- Generate Delphi/FPC client code from API definitions
- See: https://blog.synopse.info/?post/2024/09/06/Swagger/OpenAPI-Client-Generator-for-Delphi-and-FPC

## Key Design Patterns

### Socket Abstraction Pattern
```pascal
// Don't use OS-specific socket APIs directly
var sock: TNetSocket;
sock := NewSocket('example.com', '443', nlTcp, {bind:}false, 10000);
if sock <> nil then
  try
    // Use sock.Send(), sock.Recv(), etc.
  finally
    sock.Close;
  end;
```

### Compression Registration
```pascal
// Both client and server must register same compression algorithms
HttpClient.RegisterCompress(CompressGzip, 1024);
HttpServer.RegisterCompress(CompressGzip, 1024);
```

### URI Router Pattern
```pascal
// Efficient routing with parameter extraction
Router := TUriRouter.Create(TUriTreeNode);
Router.Rewrite(urmGet, '/api/users/<id>', urmGet, '/internal/user?id=<id>');
Router.Run(urmPost, '/api/users', @HandleCreateUser);
// Router.Process() achieves ~8M parametrized rewrites/sec
```

### Async Server Pattern
```pascal
// Event-driven server for high concurrency
TMyConnection = class(TPollAsyncConnection)
  procedure OnRead(...); override; // Parse protocol
end;

Server := THttpAsyncServer.Create(...);
Server.Start; // Non-blocking, handles thousands of connections
```

## Platform-Specific Notes

### Windows
- IOCP available via `USE_WINIOCP` (enabled by default, very fast)
- http.sys integration via `THttpApiServer` (production-grade, IIS-level performance)
- TLS: SChannel (system) or OpenSSL (if `USE_OPENSSL` defined)

### POSIX (Linux/BSD/macOS)
- Event notification: epoll (Linux), kqueue (BSD/macOS), poll (fallback)
- TLS: OpenSSL required (`USE_OPENSSL` must be defined)
- Unix domain sockets supported via `nlUnix` layer

### Conditional Defines
- `USE_OPENSSL` - Enable OpenSSL for TLS (required on POSIX, optional on Windows)
- `FORCE_OPENSSL` - Force OpenSSL even on Windows (bypass SChannel)
- `USEWININET` - Enable WinHTTP/WinINet client classes (Windows only)
- `USELIBCURL` - Enable libcurl client class (cross-platform)
- `NO_ASYNC_WINIOCP` - Force select() instead of IOCP on Windows (debug only)
- `DOMAINRESTAUTH` - Enable SSPI/GSSAPI domain authentication

## Performance Characteristics

From `TNetworkProtocols._TUriTree` benchmarks (typical laptop):
- URI lookups: **26M/sec** (37ns avg)
- Static rewrites: **12M/sec** (80ns avg)
- Parametrized rewrites: **8M/sec** (117ns avg)
- Static execute: **10M/sec** (91ns avg)
- Parametrized execute: **6M/sec** (162ns avg)

Async servers (`THttpAsyncServer`/`TWebSocketAsyncServer`):
- Thousands of concurrent connections with minimal memory
- Single-threaded event loop (can run pool for multi-core)
- Used in production for relay and streaming scenarios

## Common Development Tasks

### Adding Custom HTTP Header
1. Add to `THttpHeader` enum in `mormot.net.http`
2. Update `KnownHttpHeader()` parser function
3. Use in `THttpRequestContext.ParseHeader()`

### Custom Compression Algorithm
```pascal
function MyCompress(var Data: RawByteString; Compress: boolean): RawUtf8;
begin
  result := 'myalgo';
  if Compress then
    Data := CompressData(Data)
  else
    Data := DecompressData(Data);
end;

Client.RegisterCompress(MyCompress, {minsize:}512, {priority:}10);
```

### Custom URI Router Node
```pascal
TMyTreeNode = class(TUriTreeNode)
  // Add custom Data fields or override LookupParam()
end;

Router := TUriRouter.Create(TMyTreeNode, [rtoQueryWithRouterParam]);
```

## Testing

See `/mnt/w/mORMot2/test/` for comprehensive regression tests covering:
- Socket layer (`TNetSocket`, address resolution, polling)
- HTTP client/server (all backends, compression, chunking)
- WebSockets (client/server, protocols, async)
- Specialized protocols (LDAP, DNS, ACME, etc.)

## Dependencies

**Core units** (always required):
- `mormot.core.base` - Base types, UTF-8, RTTI
- `mormot.core.os` - OS abstraction
- `mormot.core.text` - Text processing
- `mormot.core.buffers` - Memory buffers
- `mormot.core.threads` - Threading primitives

**Optional units**:
- `mormot.lib.openssl11` - OpenSSL 1.1/3.x binding (TLS on POSIX)
- `mormot.lib.curl` - libcurl binding (`TCurlHttp` client)
- `mormot.lib.sspi` - Windows SSPI (Negotiate auth)
- `mormot.lib.gssapi` - POSIX GSSAPI (Kerberos auth)
- `mormot.lib.winhttp` - Windows WinHTTP/WinINet bindings

## Build Notes

- No special build commands needed - units compile as part of mORMot 2 framework
- Include `mormot.defines.inc` for proper conditional compilation
- For servers, consider defining `USE_OPENSSL` for maximum TLS compatibility
- For ACME/Let's Encrypt features, `USE_OPENSSL` is mandatory

## Related Documentation

**📖 SAD Chapters**:
- [Chapter 11: Client-Server Architecture](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-11.md) - HTTP transport modes
- [Chapter 20: Application Servers](/mnt/w/mORMot2/DOCS/mORMot2-SAD-Chapter-20.md) - Daemon/service deployment

**External Resources**:
- [Official mORMot 2 Documentation](https://synopse.info/files/doc/mORMot2.html)
- [Synopse Forum - Network Discussion](https://synopse.info/forum/viewforum.php?id=24)
- [mORMot Blog - Network Articles](https://blog.synopse.info)
- [Source Code Samples](../../ex/) - See HTTP/WebSockets examples
- [RFC 6455](https://tools.ietf.org/html/rfc6455) - WebSockets Protocol
- [RFC 8555](https://tools.ietf.org/html/rfc8555) - ACME Protocol

---

**Last Updated**: 2025-10-10 (Based on mORMot 2 commit from 2025-10-09)
**Framework Version**: mORMot 2 (replaces mORMot 1.18)
**License**: MPL/GPL/LGPL three-license
