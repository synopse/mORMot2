# SSL/HTTPS Client - mORMot2 DMVC Port

This example demonstrates how to create a secure HTTP client that connects to HTTPS endpoints using mORMot2's networking capabilities.

## Original DMVC Sample

Based on DelphiMVCFramework's `ssl_client` sample, showcasing:
- HTTPS connections with TLS/SSL
- Certificate validation and error handling
- Option to ignore certificate errors (for development)
- Secure GET and POST requests

## mORMot2 Implementation

### Key Components Used

1. **TSimpleHttpClient** - High-level HTTP/HTTPS client
   - Automatic protocol detection (HTTP/HTTPS)
   - Built-in TLS/SSL support
   - Easy-to-use API for common operations

2. **TNetTlsContext** - TLS/SSL configuration
   - Certificate validation control
   - Custom CA certificates support
   - Client certificate authentication
   - TLS version management

### Main Features

#### Certificate Validation
```pascal
// For production - validate certificates (default)
fClient.Options^.TLS.IgnoreCertificateErrors := False;

// For development - ignore certificate errors (self-signed certs)
fClient.Options^.TLS.IgnoreCertificateErrors := True;
```

#### Custom CA Certificates
```pascal
// Use custom CA certificate bundle
fClient.Options^.TLS.CACertificatesFile := 'path/to/ca-bundle.crt';
```

#### Client Certificate Authentication
```pascal
// Configure client certificate (mutual TLS)
fClient.Options^.TLS.CertificateFile := 'client-cert.pem';
fClient.Options^.TLS.PrivateKeyFile := 'client-key.pem';
fClient.Options^.TLS.PrivatePassword := 'key-password';
```

#### TLS Protocol Control
```pascal
// Disable deprecated TLS versions
fClient.Options^.TLS.AllowDeprecatedTls := False;
```

## Building and Running

### Prerequisites

1. **mORMot2 Framework**
   - Location: `/mnt/w/mORMot2/`
   - Required units in search path (configured in .dproj)

2. **HTTPS Test Server**
   - Use example `11-ssl_server` or any HTTPS endpoint
   - For testing with self-signed certificates, enable "Ignore Certificate Errors"

### Compilation

```bash
# Using delphi-compiler utility
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe 23-ssl_client.dproj --config=Debug

# Or compile from Delphi IDE
# Open 23-ssl_client.dproj and build
```

### Running the Application

1. Start the application
2. Enter an HTTPS URL (e.g., `https://localhost:8443/api/people`)
3. Check "Ignore TLS Certificate Errors" if using self-signed certificates
4. Click **GET** or **POST** to make a secure request
5. View the response in the memo control

## Code Structure

```
23-ssl_client/
├── SSLClientSample.dpr           # Main program
├── 23-ssl_client.dproj           # Delphi project file
├── src/
│   ├── MainFormU.pas             # Main form implementation
│   └── MainFormU.dfm             # Form design
└── README.md                     # This file
```

## Security Considerations

### Production Deployment

**CRITICAL:** Always validate certificates in production!

```pascal
// Production configuration
fClient.Options^.TLS.IgnoreCertificateErrors := False;  // REQUIRED
fClient.Options^.TLS.AllowDeprecatedTls := False;       // Recommended
fClient.Options^.TLS.CACertificatesFile := 'ca-bundle.crt';  // If needed
```

### Development/Testing

For development with self-signed certificates:

```pascal
fClient.Options^.TLS.IgnoreCertificateErrors := True;  // Development only!
```

**⚠️ WARNING:** Never deploy to production with `IgnoreCertificateErrors = True`!

### Certificate Management

1. **System Certificates**
   - mORMot2 uses system certificate store by default
   - Works with certificates trusted by the OS

2. **Custom CA Certificates**
   - Use `CACertificatesFile` for custom root certificates
   - Useful for internal PKI or specific trust chains

3. **Client Certificates**
   - Configure `CertificateFile` and `PrivateKeyFile` for mutual TLS
   - Required for some enterprise APIs and services

## Comparison with DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| HTTP Client | TMVCRESTClient | TSimpleHttpClient |
| TLS Config | TRESTClient properties | TNetTlsContext record |
| Cert Validation | SetValidateServerCertificateProc | IgnoreCertificateErrors |
| Async Support | Async() method | Built-in async APIs |
| Certificate Setup | Component properties | Options^ record |

## Key Differences from DMVC

1. **Configuration Model**
   - DMVC: Component-based configuration (TRESTClient)
   - mORMot2: Record-based configuration (TNetTlsContext)

2. **Certificate Validation**
   - DMVC: Callback procedure (`SetValidateServerCertificateProc`)
   - mORMot2: Boolean flag (`IgnoreCertificateErrors`)

3. **Client Creation**
   - DMVC: `TMVCRESTClient.New.BaseURL('https://...')`
   - mORMot2: `TSimpleHttpClient.Create` + `Request(uri, ...)`

## Advanced Usage

### Custom Headers
```pascal
status := fClient.Request(uri, 'GET', nil,
  'Authorization: Bearer token123'#13#10 +
  'X-Custom-Header: value'#13#10,
  headers, response);
```

### Timeout Configuration
```pascal
fClient.Options^.Timeout := 30000;  // 30 seconds
```

### Connection Pooling
```pascal
// TSimpleHttpClient reuses connections automatically
// Just keep the same instance for multiple requests
```

## Testing Scenarios

### 1. Valid HTTPS Endpoint
- URL: `https://api.github.com/users/synopse`
- Certificate: Valid, trusted by system
- Expected: Successful connection, JSON response

### 2. Self-Signed Certificate
- URL: `https://localhost:8443/api/people`
- Certificate: Self-signed (from 11-ssl_server example)
- Expected: Fails without ignore option, succeeds with ignore enabled

### 3. Expired Certificate
- Certificate: Expired SSL certificate
- Expected: Fails validation, shows certificate error

## Troubleshooting

### "Certificate validation failed"
- **Cause:** Invalid or untrusted certificate
- **Solution:**
  - For production: Fix certificate chain
  - For development: Enable "Ignore Certificate Errors"

### "Connection refused"
- **Cause:** Server not running or firewall blocking
- **Solution:**
  - Verify server is running on specified port
  - Check firewall settings

### "TLS handshake failed"
- **Cause:** TLS version mismatch
- **Solution:**
  - Check server TLS configuration
  - Try enabling `AllowDeprecatedTls` (for legacy servers only)

## Related Examples

- **11-ssl_server** - HTTPS server with TLS/SSL support
- **19-basicdemo_vclclient** - Basic HTTP client (non-secure)
- **21-restclient** - Full REST client implementation

## References

- [mORMot2 Network Documentation](../../../src/net/README.md)
- [TLS/SSL Configuration Guide](../../../src/crypt/README.md)
- [HTTP Client Architecture](../ARCHITECTURE.md)

## Author Notes

This example demonstrates mORMot2's comprehensive TLS/SSL support for HTTP clients. The framework provides fine-grained control over certificate validation, TLS versions, and authentication methods.

Key advantages:
- ✅ Production-ready certificate validation
- ✅ Flexible configuration for development and testing
- ✅ Support for client certificates (mutual TLS)
- ✅ Cross-platform TLS implementation
- ✅ Built-in connection pooling

For production applications, always follow security best practices and validate certificates properly!
