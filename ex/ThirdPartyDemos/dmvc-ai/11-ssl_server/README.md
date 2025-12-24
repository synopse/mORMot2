# mORMot2 SSL/HTTPS Server Demo

Port of DMVCframework `ssl_server` sample to mORMot2.

## Overview

This example demonstrates how to create a secure HTTPS server using mORMot2's built-in TLS/SSL support. The server can use either:
- Custom SSL certificates (production use)
- Self-signed certificates (testing/development)

## Original DMVCframework Sample

- **Source**: `/mnt/w/DMVCframework/samples/ssl_server/`
- **Features**:
  - HTTPS server using Indy's SSL/OpenSSL support
  - Certificate-based encryption
  - Standard port 443 or custom port
  - JSON API endpoints

## mORMot2 Implementation

### Key Differences from DMVCframework

| Feature | DMVCframework | mORMot2 |
|---------|---------------|---------|
| **TLS Backend** | Indy + OpenSSL DLLs | Native (SChannel/OpenSSL) |
| **Configuration** | TIdServerIOHandlerSSLOpenSSL | `hsoEnableTls` option + `WaitStarted()` |
| **Certificates** | Manual DLL management | Built-in certificate loading |
| **Self-Signed** | External tools required | `WaitStartedHttps()` method |
| **Dependencies** | libeay32.dll, ssleay32.dll | Platform-native (Windows: SChannel, Linux: OpenSSL) |

### Architecture

```
11-ssl_server.dpr           - Main program with SSL/HTTPS setup
src/
  server.pas                - TSslDemoServer (THttpServer wrapper)
  entities.pas              - TPerson entity
  api.interfaces.pas        - IMyApi interface definition
  api.impl.pas              - TMyApi implementation
```

### TLS/SSL Support in mORMot2

mORMot2 provides native TLS support through:

1. **Windows**: SChannel API (built into Windows)
2. **Linux**: OpenSSL library
3. **Fallback**: Pure Pascal implementation

**Key Classes/Methods**:
- `THttpServer` with `hsoEnableTls` option
- `WaitStarted()` - Configure custom certificates
- `WaitStartedHttps()` - Use self-signed certificates
- `TNetTlsContext` - TLS configuration structure

## Building

```bash
# Compile with Delphi
cd /mnt/w/mORMot2/ex/dmvc/11-ssl_server
dcc64 11-ssl_server.dpr

# Or use the compiler tool
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 11-ssl_server.dproj --config=Debug --platform=Win64
```

## Running the Server

### Option 1: Self-Signed Certificate (Development)

The server automatically generates and uses a self-signed certificate if no certificate files are found:

```bash
Win64\Debug\SslServer.exe
```

Access at: `https://localhost:8443/`

**Note**: Your browser will show a security warning (this is expected for self-signed certificates).

### Option 2: Custom Certificates (Production)

1. Generate or obtain SSL certificates:
   - `cacert.pem` - Certificate file
   - `privkey.pem` - Private key file

2. Place them in the executable directory

3. Run the server:
```bash
Win64\Debug\SslServer.exe
```

The server will detect and use the certificate files automatically.

## Generating Certificates

### For Testing (Self-Signed)

The mORMot2 server can automatically generate self-signed certificates. No manual generation needed.

### For Production (Let's Encrypt)

Use certbot or another ACME client:

```bash
certbot certonly --standalone -d yourdomain.com
```

Or use mORMot2's built-in ACME client (see `mormot.net.acme` unit).

### Using OpenSSL (Manual)

If you need to manually generate certificates:

```bash
# Generate private key
openssl genrsa -out privkey.pem 2048

# Generate certificate signing request
openssl req -new -key privkey.pem -out cert.csr

# Generate self-signed certificate
openssl x509 -req -days 365 -in cert.csr -signkey privkey.pem -out cacert.pem
```

## API Endpoints

### GET /
Returns a single person object:

```bash
curl -k https://localhost:8443/
```

Response:
```json
{
  "FirstName": "Daniele",
  "LastName": "Teti",
  "Age": 38
}
```

### GET /people
Returns a list of people:

```bash
curl -k https://localhost:8443/people
```

Response:
```json
[
  {"FirstName":"Daniele","LastName":"Teti","Age":38},
  {"FirstName":"John","LastName":"Doe","Age":35},
  {"FirstName":"Jane","LastName":"Doe","Age":32},
  {"FirstName":"Bruce","LastName":"Banner","Age":60}
]
```

**Note**: Use `-k` flag with curl to skip certificate verification for self-signed certificates.

## Key Code Snippets

### Creating HTTPS Server with Custom Certificates

```pascal
fHttpServer := THttpServer.Create(
  '8443',           // Port
  nil,              // OnStart callback
  nil,              // ThreadPool
  '',               // ServerName
  32,               // ThreadPoolCount
  30000,            // KeepAliveTimeOut
  [hsoEnableTls]    // Enable TLS/SSL
);

// Configure certificates
fHttpServer.WaitStarted(
  10,                    // Timeout in seconds
  'cacert.pem',          // Certificate file
  'privkey.pem',         // Private key file
  ''                     // Password (if encrypted)
);
```

### Creating HTTPS Server with Self-Signed Certificate

```pascal
fHttpServer := THttpServer.Create(
  '8443',
  nil, nil, '', 32, 30000,
  [hsoEnableTls]
);

// Use self-signed certificate (automatic generation)
fHttpServer.WaitStartedHttps(10, true);
```

## Security Considerations

### Self-Signed Certificates
- ✅ **Good for**: Development, testing, internal networks
- ❌ **Bad for**: Public-facing production servers
- ⚠️ **Warning**: Browsers will show security warnings

### Production Certificates
- ✅ Use certificates from trusted Certificate Authorities (CA)
- ✅ Consider Let's Encrypt (free, automated)
- ✅ Use mORMot2's ACME client for automatic renewal
- ✅ Keep private keys secure and encrypted

### TLS Configuration
- Use TLS 1.2+ (mORMot2 default)
- Disable older protocols (SSL 2.0, SSL 3.0, TLS 1.0, TLS 1.1)
- Configure cipher suites appropriately

## Differences from DMVCframework

### Simplified Setup
DMVCframework requires:
1. Download OpenSSL DLLs compatible with Indy
2. Download Apache for certificate generation
3. Manual DLL management
4. Complex SSL configuration

mORMot2 provides:
1. Platform-native TLS support (no DLLs needed on Windows)
2. Built-in certificate generation
3. Simple API (`hsoEnableTls` + `WaitStarted()`)
4. Automatic TLS initialization

### No External Dependencies
- **DMVCframework**: Requires `libeay32.dll` and `ssleay32.dll`
- **mORMot2**: Uses Windows SChannel API (built-in) or system OpenSSL

### Automatic Certificate Management
mORMot2 includes:
- Self-signed certificate generation
- ACME v2 client (Let's Encrypt/ZeroSSL)
- Automatic certificate renewal support

## Performance Notes

- mORMot2's HTTP server is highly optimized
- Native TLS support avoids DLL overhead
- Thread pool efficiently handles concurrent connections
- Typical throughput: 10,000+ HTTPS requests/second

## Related mORMot2 Examples

- `ex/http-server-files` - File server with HTTPS support
- `ex/mvc-blog` - MVC application with TLS configuration
- `src/net/mormot.net.acme.pas` - ACME client for Let's Encrypt

## Documentation

- **TLS Support**: `/mnt/w/mORMot2/src/net/CLAUDE.md`
- **HTTP Server**: `mormot.net.server.pas` documentation
- **ACME Client**: `mormot.net.acme.pas` for Let's Encrypt integration

## Testing

### Using curl
```bash
# Test with self-signed certificate (skip verification)
curl -k https://localhost:8443/
curl -k https://localhost:8443/people

# Test with trusted certificate
curl https://localhost:8443/
```

### Using a Web Browser
Navigate to:
- `https://localhost:8443/`
- `https://localhost:8443/people`

For self-signed certificates, you'll need to accept the security warning.

## Troubleshooting

### Certificate Not Found
- Check that `cacert.pem` and `privkey.pem` are in the executable directory
- Or let the server use self-signed certificates

### Port Already in Use
Change the port in `11-ssl_server.dpr`:
```pascal
srv := TSslDemoServer.Create('9443'); // Use port 9443
```

### TLS Handshake Errors
- Ensure certificates are in PEM format
- Check private key password (if encrypted)
- Verify certificate validity dates

### OpenSSL Not Found (Linux)
Install OpenSSL development libraries:
```bash
# Ubuntu/Debian
sudo apt-get install libssl-dev

# Red Hat/CentOS
sudo yum install openssl-devel
```

## License

Part of mORMot2 framework - MPL/GPL/LGPL tri-license

## See Also

- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md) - General DMVCframework to mORMot2 conversion guide
- Original DMVCframework sample: `/mnt/w/DMVCframework/samples/ssl_server/`
