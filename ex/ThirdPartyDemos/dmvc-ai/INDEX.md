# mORMot2 DMVC Sample Index

Complete catalog of all 22 samples with detailed descriptions, features, and learning objectives.

**Last Updated**: 2025-12-20
**Compilation Status**: ✅ All samples compile successfully (0 errors, 0 warnings, 0 hints)
**Total Samples**: 22 (19 servers + 3 VCL clients)

---

## Quick Navigation

- [Core Features](#core-features-samples-1-7) (Samples 1-7)
- [Security](#security-samples-8-10-24) (Samples 8-10, 24)
- [Advanced Features](#advanced-features-samples-11-18) (Samples 11-18)
- [VCL Clients](#vcl-clients-samples-19-21-23) (Samples 19-21, 23)
- [By Topic](#samples-by-topic)
- [By Difficulty](#samples-by-difficulty)

---

## Core Features (Samples 1-7)

### 01 - Basic Demo Server

**Directory**: `01-basicdemo_server/`
**Difficulty**: ⭐ Beginner
**DMVC Original**: `/mnt/w/DMVCframework/samples/basicdemo_server/`

#### Description

Foundational sample demonstrating basic REST API concepts with simple endpoints.

#### Features

- ✅ Simple GET endpoint returning JSON
- ✅ POST endpoint echoing data
- ✅ GET endpoint with parameters (division calculator)
- ✅ Console logging with TSynLog
- ✅ Clean project structure

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/BasicDemoApi/HelloWorld` | Returns greeting with current time |
| POST | `/BasicDemoApi/HelloWorldPost` | Echoes posted JSON with modification |
| GET | `/BasicDemoApi/Divide?par1=10&par2=2` | Divides two numbers |

#### Learning Objectives

- Understanding interface-based services
- Creating DTOs (Data Transfer Objects)
- HTTP server setup
- Basic request/response handling

#### Quick Test

```bash
curl http://localhost:8080/BasicDemoApi/HelloWorld
```

---

### 02 - Console Sample

**Directory**: `02-console_sample/`
**Difficulty**: ⭐ Beginner
**DMVC Original**: `/mnt/w/DMVCframework/samples/console_server_application/`

#### Description

Demonstrates a pure console application with HTTP server (no GUI), ideal for microservices and Docker deployments.

#### Features

- ✅ Console-only application (no VCL/FMX)
- ✅ Clean startup/shutdown
- ✅ Graceful termination (Ctrl+C handling)
- ✅ Minimal memory footprint
- ✅ Docker-ready architecture

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/ConsoleApi/GetMessage` | Returns simple message |
| GET | `/ConsoleApi/GetServerTime` | Returns server timestamp |

#### Learning Objectives

- Building console HTTP servers
- Resource management (startup/shutdown)
- Signal handling
- Microservice patterns

#### Deployment

Perfect for:
- Docker containers
- Windows services
- Linux daemons
- Cloud deployments

---

### 03 - Routing

**Directory**: `03-routing/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/routing/`

#### Description

Comprehensive demonstration of URL routing patterns and HTTP method handling.

#### Features

- ✅ GET, POST, PUT, DELETE methods
- ✅ Query string parameters
- ✅ Multiple parameters
- ✅ Parameter validation
- ✅ Error handling for invalid routes

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/RoutingApi/GetArticle?id=123` | Get by ID (query parameter) |
| POST | `/RoutingApi/CreateArticle` | Create new article |
| PUT | `/RoutingApi/UpdateArticle?id=123` | Update existing article |
| DELETE | `/RoutingApi/DeleteArticle?id=123` | Delete article |
| GET | `/RoutingApi/Search?query=test&page=1` | Multiple parameters |

#### Learning Objectives

- RESTful routing patterns
- HTTP method semantics
- Parameter extraction and validation
- URL design best practices

#### Comparison with DMVC

| DMVC | mORMot2 |
|------|---------|
| `/articles/123` (path param) | `/GetArticle?id=123` (query param) |
| `[MVCPath]` attribute | Interface method name |
| `[MVCHTTPMethod]` attribute | All methods use POST (JSON-RPC) |

---

### 04 - Renders

**Directory**: `04-renders/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/renders/`

#### Description

Demonstrates various response types and content rendering strategies.

#### Features

- ✅ JSON responses (automatic serialization)
- ✅ HTML rendering
- ✅ Binary data (file downloads)
- ✅ Custom content types
- ✅ Dataset serialization
- ✅ Image rendering

#### Key Endpoints

| Method | URL | Content-Type | Description |
|--------|-----|--------------|-------------|
| GET | `/RenderApi/GetJson` | application/json | JSON object |
| GET | `/RenderApi/GetJsonArray` | application/json | JSON array |
| GET | `/RenderApi/GetHtml` | text/html | HTML page |
| GET | `/RenderApi/GetText` | text/plain | Plain text |
| GET | `/RenderApi/GetBinary` | application/octet-stream | Binary data |
| GET | `/RenderApi/DownloadFile` | * | File download |

#### Learning Objectives

- Content negotiation
- MIME types and headers
- Binary data handling
- File downloads
- Custom serialization

#### Comparison File

See `04-renders/COMPARISON.md` for detailed DMVC → mORMot2 mapping.

---

### 05 - Datasets

**Directory**: `05-datasets/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/data_manipulation/`

#### Description

Demonstrates serializing Delphi datasets (TDataSet) to JSON for API responses.

#### Features

- ✅ FireDAC dataset integration
- ✅ Automatic dataset → JSON conversion
- ✅ Field mapping
- ✅ Nested datasets
- ✅ Custom field serialization
- ✅ Date/time formatting

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/DataApi/GetCustomers` | Returns customer dataset as JSON |
| GET | `/DataApi/GetOrders?customerId=123` | Returns orders for customer |
| GET | `/DataApi/GetCustomerWithOrders?id=123` | Nested dataset (master-detail) |

#### Learning Objectives

- Dataset serialization
- FireDAC integration
- Master-detail relationships
- Field customization
- Database abstraction

#### Use Cases

- Legacy database integration
- Existing dataset-based code
- Rapid API development from databases

---

### 06 - Articles CRUD Server

**Directory**: `06-articles_crud_server/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/articles_crud/`

#### Description

Complete CRUD (Create, Read, Update, Delete) implementation with mORMot2 ORM. Includes both server and client applications.

#### Features

- ✅ Full CRUD operations
- ✅ mORMot2 ORM integration
- ✅ Data validation
- ✅ Error handling
- ✅ Client application included
- ✅ SQLite database

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/ArticleApi/GetAll` | List all articles |
| GET | `/ArticleApi/GetById?id=123` | Get single article |
| POST | `/ArticleApi/Create` | Create new article |
| PUT | `/ArticleApi/Update` | Update existing article |
| DELETE | `/ArticleApi/Delete?id=123` | Delete article |

#### Database Schema

```pascal
type
  TOrmArticle = class(TOrm)
  private
    fTitle: RawUtf8;
    fBody: RawUtf8;
    fAuthor: RawUtf8;
  published
    property Title: RawUtf8 read fTitle write fTitle;
    property Body: RawUtf8 read fBody write fBody;
    property Author: RawUtf8 read fAuthor write fAuthor;
  end;
```

#### Learning Objectives

- CRUD patterns
- ORM usage (TOrm, TRestServerDB)
- Data validation
- Client-server architecture
- RESTful API design

#### Client Application

Located in `client/` subdirectory. Demonstrates:
- REST client usage (TRestHttpClient)
- Service proxy generation
- Error handling
- Complete CRUD workflow

---

### 07 - Master Details

**Directory**: `07-master_details/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/master_details/`

#### Description

Advanced sample demonstrating master-detail relationships with nested JSON serialization.

#### Features

- ✅ Master-detail data structures
- ✅ Nested JSON generation
- ✅ Complex object graphs
- ✅ ORM relationship mapping
- ✅ Lazy loading
- ✅ Cascade operations

#### Key Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/OrderApi/GetOrder?id=123` | Order with line items |
| GET | `/OrderApi/GetCustomerOrders?customerId=456` | Customer with all orders |
| POST | `/OrderApi/CreateOrder` | Create order with items |
| DELETE | `/OrderApi/DeleteOrder?id=123` | Cascade delete order + items |

#### Data Structure

```pascal
type
  TOrmOrderItem = class(TOrm)
    fOrderId: TID;
    fProductName: RawUtf8;
    fQuantity: Integer;
    fPrice: Double;
  end;

  TOrmOrder = class(TOrm)
    fCustomerName: RawUtf8;
    fOrderDate: TDateTime;
    // Items loaded separately or via JSON
  end;
```

#### Learning Objectives

- Complex data relationships
- Nested object serialization
- ORM foreign keys
- Cascade operations
- Transaction handling

#### Comparison File

See `07-master_details/COMPARISON.md` for implementation details.

---

## Security (Samples 8-10, 24)

### 08 - Basic Authentication

**Directory**: `08-basicauth/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/middleware_basicauthentication/`

#### Description

HTTP Basic Authentication implementation with role-based authorization.

#### Features

- ✅ HTTP Basic Auth (RFC 7617)
- ✅ Base64 credential parsing
- ✅ Role-based access control
- ✅ Public vs protected endpoints
- ✅ Authorization header handling
- ✅ 401/403 responses

#### Test Credentials

| Username | Password | Roles |
|----------|----------|-------|
| user1 | user1 | role1 |
| user2 | user2 | role2 |
| user3 | user3 | role1, role2 |

#### Key Endpoints

| Method | URL | Auth Required | Roles |
|--------|-----|---------------|-------|
| GET | `/BasicAuthApi/PublicSection` | No | - |
| GET | `/BasicAuthApi/OnlyRole1` | Yes | role1 |
| GET | `/BasicAuthApi/OnlyRole2` | Yes | role2 |

#### Learning Objectives

- HTTP authentication mechanisms
- Authorization header parsing
- Role-based access control
- Security filters
- 401 vs 403 responses

#### Testing

```bash
# Public endpoint
curl http://localhost:8080/root/BasicAuthApi.PublicSection

# With valid credentials
curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole1

# Wrong role (403 Forbidden)
curl -u user1:user1 http://localhost:8080/root/BasicAuthApi.OnlyRole2
```

#### Testing Guide

See `08-basicauth/TESTING.md` for complete test scenarios.

---

### 09 - Custom Authentication

**Directory**: `09-custom_auth/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/custom_authentication/`

#### Description

Demonstrates custom authentication schemes beyond standard HTTP Basic Auth.

#### Features

- ✅ Custom header-based authentication
- ✅ API key validation
- ✅ Token-based authentication
- ✅ Session management
- ✅ Custom authentication filters
- ✅ Multiple auth schemes

#### Authentication Schemes

1. **API Key**: `X-API-Key: your-secret-key`
2. **Custom Token**: `X-Auth-Token: user-session-token`
3. **Combined**: Both API key + user token

#### Key Endpoints

| Method | URL | Auth Scheme |
|--------|-----|-------------|
| GET | `/CustomAuthApi/Public` | None |
| GET | `/CustomAuthApi/ApiKeyOnly` | API Key |
| GET | `/CustomAuthApi/TokenOnly` | User Token |
| GET | `/CustomAuthApi/BothRequired` | API Key + Token |

#### Learning Objectives

- Custom authentication design
- Multiple auth schemes
- Header-based authentication
- Session token management
- Authentication filters

#### Verification

See `09-custom_auth/VERIFICATION.md` for test results.

---

### 10 - JSON Web Token (JWT)

**Directory**: `10-jsonwebtoken/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/jsonwebtoken/`

#### Description

Complete JWT authentication implementation with login, token validation, and role-based access.

#### Features

- ✅ JWT token generation (HS256)
- ✅ Login endpoint issuing tokens
- ✅ Automatic token verification
- ✅ Role-based access control
- ✅ Custom claims
- ✅ Token expiration (1 hour)
- ✅ Refresh token pattern

#### Test Users

| Username | Password | Roles |
|----------|----------|-------|
| user1 | user1 | role1 |
| user2 | user2 | role2 |
| user3 | user3 | role1, role2 |

#### Key Endpoints

| Method | URL | Auth Required | Description |
|--------|-----|---------------|-------------|
| POST | `/auth/Login` | No | Login and get JWT token |
| GET | `/PublicService/GetPublicMessage` | No | Public endpoint |
| GET | `/ProtectedService/GetUserInfo` | Yes (role1) | User info |
| GET | `/ProtectedService/GetAdminInfo` | Yes (role2) | Admin info |

#### JWT Structure

```json
{
  "header": {
    "alg": "HS256",
    "typ": "JWT"
  },
  "payload": {
    "iss": "mORMot2 JWT Demo",
    "sub": "user1",
    "exp": 1634567890,
    "iat": 1634564290,
    "roles": ["role1"],
    "customkey1": "customvalue1"
  }
}
```

#### Learning Objectives

- JWT token structure
- Token generation and signing
- Token verification
- Claims-based authorization
- Bearer token authentication
- Token expiration handling

#### Testing Workflow

```bash
# 1. Login
TOKEN=$(curl -s -X POST http://localhost:8080/auth/Login \
  -H "Content-Type: application/json" \
  -d '{"username":"user1","password":"user1"}' \
  | jq -r '.token')

# 2. Access protected endpoint
curl http://localhost:8080/ProtectedService/GetUserInfo \
  -H "Authorization: Bearer $TOKEN"
```

#### Verification

See `10-jsonwebtoken/VERIFICATION.md` for complete test scenarios.

---

### 24 - HMAC Authentication

**Directory**: `24-hmac_auth/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/hmac/`

#### Description

Cryptographic demonstration of HMAC (Hash-based Message Authentication Code) signing using multiple algorithms.

#### Features

- ✅ HMAC-MD5, SHA1, SHA224, SHA256, SHA384, SHA512
- ✅ Test vectors validation
- ✅ mORMot2 native cryptography (`mormot.crypt.secure`)
- ✅ Zero external dependencies
- ✅ Console application demo

#### HMAC Algorithms

| Algorithm | Output Size | Security Level |
|-----------|-------------|----------------|
| HMAC-MD5 | 128 bits (16 bytes) | ⚠️ Weak (legacy only) |
| HMAC-SHA1 | 160 bits (20 bytes) | ⚠️ Deprecated |
| HMAC-SHA224 | 224 bits (28 bytes) | ✅ Good |
| HMAC-SHA256 | 256 bits (32 bytes) | ✅ Recommended |
| HMAC-SHA384 | 384 bits (48 bytes) | ✅ Strong |
| HMAC-SHA512 | 512 bits (64 bytes) | ✅ Very Strong |

#### Test Vectors

Input: `"Daniele Teti"`
Key: `"daniele"`

All test vectors validated against known HMAC signatures.

#### Learning Objectives

- HMAC cryptographic principles
- Message authentication codes
- mORMot2 cryptography API
- Request signing patterns
- API authentication design

#### Practical Use Cases

1. **API Request Signing**: Sign HTTP requests to prove authenticity
2. **Token Generation**: Create secure tokens with HMAC signatures
3. **Message Integrity**: Verify messages haven't been tampered with
4. **Webhook Validation**: Validate webhook payloads from third parties

#### Example: Request Signing

```pascal
uses
  mormot.core.base,
  mormot.crypt.secure;

var
  message, key: RawUtf8;
  signature: THash256;
begin
  message := 'POST' + #10 + '/api/users' + #10 + '{"name":"John"}';
  key := 'secret-key-123';

  // Sign request
  HmacSha256(message, key, signature);

  // Send signature in X-Signature header
  WriteLn('Signature: ', BinToHex(@signature, SizeOf(signature)));
end;
```

#### Running the Demo

```bash
cd /mnt/w/mORMot2/ex/dmvc/24-hmac_auth
./Win32/Debug/HmacAuthSample.exe
```

Expected output: All 5 HMAC algorithms pass validation tests.

#### Comparison with DMVC

| DMVC | mORMot2 |
|------|---------|
| Registry-based algorithm selection | Direct function calls |
| `System.Hash` or Indy libraries | `mormot.crypt.secure` |
| `HMAC(algorithm, input, key)` | `HmacSha256(input, key, result)` |

---

## Advanced Features (Samples 11-18)

### 11 - SSL Server

**Directory**: `11-ssl_server/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/ssl_server/`

#### Description

HTTPS server with SSL/TLS certificate configuration.

#### Features

- ✅ HTTPS server setup
- ✅ SSL certificate loading
- ✅ TLS 1.2+ support
- ✅ Certificate validation
- ✅ Secure communication
- ✅ Self-signed certificate example

#### Configuration

```pascal
fHttpServer := TRestHttpServer.Create(
  '443',              // HTTPS port
  [fRestServer],
  '+',
  useHttpAsync,
  32,                 // Thread pool
  secTLS,             // Enable TLS
  'cert.pem',         // Certificate file
  'key.pem'           // Private key file
);
```

#### Learning Objectives

- SSL/TLS configuration
- Certificate management
- HTTPS deployment
- Secure communication
- Production security

#### Testing

```bash
# Test with curl
curl https://localhost:443/SslApi/GetSecureMessage --insecure

# Verify certificate
openssl s_client -connect localhost:443
```

#### Verification

See `11-ssl_server/VERIFICATION.md` for setup and testing.

---

### 12 - Middleware

**Directory**: `12-middleware/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/middleware/`

#### Description

Request/response filtering and processing pipeline.

#### Features

- ✅ Request filters (OnBeforeBody)
- ✅ Response filters (OnAfterBody)
- ✅ Logging middleware
- ✅ Timing middleware
- ✅ Header modification
- ✅ Request validation

#### Middleware Types

1. **Logging**: Log all requests/responses
2. **Timing**: Measure endpoint execution time
3. **Headers**: Add custom headers
4. **Validation**: Validate request format

#### Learning Objectives

- Filter patterns
- Request/response interception
- Middleware pipeline
- Cross-cutting concerns
- Performance monitoring

#### Testing

See `12-middleware/TESTING.md` for verification.

---

### 13 - Middleware CORS

**Directory**: `13-middleware_cors/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/middleware_cors/`

#### Description

Cross-Origin Resource Sharing (CORS) implementation for browser-based clients.

#### Features

- ✅ CORS headers (Access-Control-Allow-Origin)
- ✅ Preflight requests (OPTIONS)
- ✅ Allowed methods configuration
- ✅ Allowed headers configuration
- ✅ Credentials support
- ✅ Max-Age caching

#### CORS Configuration

```pascal
// Simple CORS (all origins)
HttpServer.AccessControlAllowOrigin := '*';

// Specific origin
HttpServer.AccessControlAllowOrigin := 'https://example.com';

// Preflight handling
Server.OnFilterRequest := HandleCorsPreFlight;
```

#### Learning Objectives

- CORS mechanism
- Preflight requests
- Browser security
- Cross-domain APIs
- HTTP OPTIONS method

#### Comparison

See `13-middleware_cors/COMPARISON.md` for DMVC → mORMot2 mapping.

---

### 14 - Middleware Compression

**Directory**: `14-middleware_compression/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/middleware_compression/`

#### Description

HTTP response compression (gzip/deflate) for bandwidth optimization.

#### Features

- ✅ gzip compression
- ✅ deflate compression
- ✅ Automatic compression level
- ✅ Content-Encoding headers
- ✅ Accept-Encoding negotiation
- ✅ Compression threshold

#### Configuration

```pascal
// Enable gzip compression
HttpServer.CompressGz := 6;  // Compression level 1-9

// Disable compression
HttpServer.CompressGz := 0;
```

#### Compression Stats

Typical compression ratios:
- JSON responses: 70-80% reduction
- HTML pages: 60-70% reduction
- Binary data: Minimal (already compressed)

#### Learning Objectives

- HTTP compression
- Content-Encoding header
- Bandwidth optimization
- Performance tuning
- Client negotiation

#### Verification

See `14-middleware_compression/VERIFICATION.md` for test results.

---

### 15 - Middleware Static Files

**Directory**: `15-middleware_staticfiles/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/middleware_staticfiles/`

#### Description

Serving static files (HTML, CSS, JS, images) from the HTTP server.

#### Features

- ✅ Static file serving
- ✅ MIME type detection
- ✅ Directory browsing (optional)
- ✅ Index file support (index.html)
- ✅ Cache headers
- ✅ ETag support

#### Directory Structure

```
www/
├── index.html          # Default page
├── css/
│   └── style.css
├── js/
│   └── app.js
└── images/
    └── logo.png
```

#### Configuration

```pascal
procedure TStaticFilesServer.Request(Ctxt: TRestServerUriContext);
begin
  if IdemPChar(pointer(Ctxt.Uri), '/ROOT/STATIC/') then
  begin
    var fileName := ExtractFilePath(ParamStr(0)) + 'www' +
                    Copy(Ctxt.Uri, 13, MaxInt); // Skip '/root/static'
    if FileExists(fileName) then
      Ctxt.ReturnFile(fileName, false);
  end;
end;
```

#### Learning Objectives

- Static file handling
- MIME types
- Cache strategies
- Security (path traversal prevention)
- CDN patterns

#### Verification

See `15-middleware_staticfiles/VERIFICATION.md` for testing.

---

### 16 - Server-Sent Events (SSE)

**Directory**: `16-serversentevents/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/serversentevents/`

#### Description

Real-time server-to-client event streaming using Server-Sent Events.

#### Features

- ✅ Server-Sent Events protocol
- ✅ Event stream (`text/event-stream`)
- ✅ Named events
- ✅ Event IDs
- ✅ Automatic reconnection
- ✅ Real-time stock ticker demo

#### SSE Format

```
event: stockupdate
id: 123
data: {"stock":"AAPL","value":567.89}

```

#### Client-Side (JavaScript)

```javascript
var source = new EventSource("/root/stocks");

source.addEventListener("stockupdate", function(e) {
    var data = JSON.parse(e.data);
    console.log(data.stock, data.value);
});
```

#### Learning Objectives

- SSE protocol
- Event streaming
- Real-time updates
- EventSource API
- Long-lived connections

#### Use Cases

- Live dashboards
- Stock tickers
- Notification feeds
- Progress monitoring
- Server status updates

#### Verification

See `16-serversentevents/VERIFICATION.md` for testing.

---

### 17 - WebSocket Primer

**Directory**: `17-websocket_primer/`
**Difficulty**: ⭐⭐⭐ Advanced
**DMVC Original**: `/mnt/w/DMVCframework/samples/websocket_primer/`

#### Description

WebSocket echo server demonstrating bidirectional real-time communication.

#### Features

- ✅ WebSocket protocol
- ✅ Bidirectional communication
- ✅ Echo server pattern
- ✅ Connection management
- ✅ Message broadcasting
- ✅ Protocol negotiation

#### Implementation

```pascal
type
  TEchoWebSocketProtocol = class(TWebSocketProtocolRest)
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame); override;
  end;

procedure TEchoWebSocketProtocol.ProcessIncomingFrame(...);
begin
  // Echo back to sender
  Sender.SendFrame(Frame);

  // Or broadcast to all clients
  Sender.Broadcast(Frame);
end;
```

#### Client Testing

```javascript
var ws = new WebSocket("ws://localhost:8080/echo");

ws.onmessage = function(event) {
    console.log("Received:", event.data);
};

ws.send("Hello WebSocket!");
```

#### Learning Objectives

- WebSocket protocol
- Bidirectional communication
- Frame handling
- Broadcasting
- Connection lifecycle

#### Conversion Notes

See `17-websocket_primer/CONVERSION-NOTES.md` for porting details.

---

### 18 - File Upload

**Directory**: `18-file_upload/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/file_upload/`

#### Description

File upload handling with multipart/form-data parsing.

#### Features

- ✅ Multipart form data parsing
- ✅ File upload endpoint
- ✅ Multiple file handling
- ✅ File size limits
- ✅ Content type validation
- ✅ Upload progress (headers)

#### Upload Endpoint

```pascal
function UploadFile(const fileName: RawUtf8;
                    const fileData: TBytes): RawUtf8;
begin
  // Save file
  FileFromString(fileData, 'uploads/' + fileName);
  Result := FormatUtf8('File % uploaded (% bytes)',
    [fileName, Length(fileData)]);
end;
```

#### Testing with curl

```bash
# Upload single file
curl -F "file=@document.pdf" \
  http://localhost:8080/FileUploadApi/UploadFile

# Upload multiple files
curl -F "file1=@image.png" \
     -F "file2=@document.pdf" \
  http://localhost:8080/FileUploadApi/UploadMultiple
```

#### Learning Objectives

- Multipart form data
- File handling
- Binary data processing
- Upload validation
- Storage patterns

#### Verification

See `18-file_upload/VERIFICATION.md` for test scenarios.

---

### 19 - Basic Demo VCL Client

**Directory**: `19-basicdemo_vclclient/`
**Difficulty**: ⭐ Beginner
**DMVC Original**: `/mnt/w/DMVCframework/samples/basicdemo_vclclient/`

#### Description

Simple VCL client application that consumes the REST API from 01-basicdemo_server. Demonstrates mORMot2 HTTP client usage in a desktop application.

#### Features

- ✅ TRestHttpClientGeneric for REST calls
- ✅ Interactive UI with buttons
- ✅ GET and POST endpoint testing
- ✅ JSON parsing with TDocVariantData
- ✅ Error handling and display
- ✅ Response visualization

#### Tested Endpoints

| Button | Endpoint | Description |
|--------|----------|-------------|
| GET /hello | `/BasicDemoApi/HelloWorld` | Returns greeting + time |
| GET /div | `/BasicDemoApi/Divide?par1=10&par2=20` | Divides two numbers |
| POST /hello | `/BasicDemoApi/HelloWorldPost` | Echoes JSON with modification |

#### UI Components

- **Input fields**: Numbers for division, JSON for POST
- **Buttons**: One per endpoint
- **Memo**: Displays formatted and raw responses
- **Status label**: Shows operation status

#### Code Example

```pascal
// Create REST client
fClient := TRestHttpClientGeneric.Create(
  'localhost', '8080', TOrmModel.Create([]));

// Make GET request
status := fClient.CallBackGet(
  'root/BasicDemoApi/HelloWorld', [], response);

// Parse JSON response
doc.InitJson(response, JSON_FAST);
message := doc.U['message'];
```

#### Learning Objectives

- REST client creation and configuration
- HTTP GET/POST methods
- JSON request/response handling
- UI integration with REST APIs
- Error handling in client applications

#### Prerequisites

1. Run `01-basicdemo_server.exe` first
2. Server must be on `localhost:8080`

#### Testing Workflow

1. Start server: `01-basicdemo_server.exe`
2. Start client: `19-basicdemo_vclclient.exe`
3. Click "GET /hello" - See greeting message
4. Enter numbers (10, 20) - Click "GET /div" - See result: 0.5
5. Edit JSON - Click "POST /hello" - See echoed data

#### Comparison with DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| Client Class | `IMVCRESTClient` | `TRestHttpClientGeneric` |
| GET Request | `.Get('/endpoint')` | `.CallBackGet('endpoint', [], response)` |
| POST Request | `.Post('/endpoint', body)` | `.CallBackPost('endpoint', body, response, ...)` |
| JSON Parsing | Built-in | Manual via `TDocVariantData` |

#### Compilation

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe \
  "W:\mORMot2\ex\dmvc\19-basicdemo_vclclient\19-basicdemo_vclclient.dproj"
```

✅ **Compiles successfully** (0 errors, 0 warnings, 0 hints)

---

### 21 - REST Client (Full Implementation)

**Directory**: `21-restclient/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: N/A (mORMot2 specific)

#### Description

Comprehensive REST client implementation demonstrating advanced HTTP client features including connection management, error handling, and complete CRUD operations.

#### Features

- ✅ Full CRUD operations (GET, POST, PUT, DELETE)
- ✅ Advanced error handling
- ✅ Connection pooling
- ✅ Custom headers support
- ✅ Response parsing
- ✅ Timeout configuration

#### Learning Objectives

- Advanced REST client patterns
- Connection management
- Error recovery strategies
- HTTP header manipulation
- Response validation

✅ **Compiles successfully** (0 errors, 0 warnings, 0 hints)

---

### 23 - SSL/HTTPS Client

**Directory**: `23-ssl_client/`
**Difficulty**: ⭐⭐ Intermediate
**DMVC Original**: `/mnt/w/DMVCframework/samples/ssl_client/`

#### Description

Secure HTTP client demonstrating HTTPS connections with proper TLS/SSL certificate handling. Shows both production (validated certificates) and development (self-signed certificates) scenarios.

#### Features

- ✅ HTTPS/TLS connection support
- ✅ Certificate validation control
- ✅ Development mode (ignore certificate errors)
- ✅ Production mode (full validation)
- ✅ GET and POST over secure connections
- ✅ JSON request/response with formatting
- ✅ Custom CA certificate support (commented)
- ✅ Client certificate authentication (commented)

#### Security Configuration

```pascal
// Production (default) - validate certificates
fClient.Options^.TLS.IgnoreCertificateErrors := False;

// Development - ignore self-signed certificates
fClient.Options^.TLS.IgnoreCertificateErrors := True;

// Custom CA certificates (optional)
fClient.Options^.TLS.CACertificatesFile := 'ca-bundle.crt';

// Client certificate authentication (optional)
fClient.Options^.TLS.CertificateFile := 'client.pem';
fClient.Options^.TLS.PrivateKeyFile := 'key.pem';
```

#### UI Components

- **URL input**: HTTPS endpoint (default: `https://localhost:8443/api/people`)
- **Certificate checkbox**: Toggle validation (checked = ignore errors)
- **GET button**: Execute secure GET request
- **POST button**: Execute secure POST with JSON payload
- **Response memo**: Formatted JSON display with headers
- **Status label**: Request status and errors

#### Testing Scenarios

1. **Valid HTTPS Endpoint**
   - URL: `https://api.github.com/users/synopse`
   - Expected: Success with validation enabled

2. **Self-Signed Certificate**
   - URL: `https://localhost:8443/api/people`
   - Server: Run `11-ssl_server` example
   - Expected: Fails without ignore, succeeds with ignore enabled

3. **JSON POST**
   - Auto-generates sample JSON with name, email, timestamp
   - Sends to HTTPS endpoint
   - Displays formatted response

#### Code Example

```pascal
// Configure TLS settings
fClient := TSimpleHttpClient.Create;
fClient.Options^.TLS.IgnoreCertificateErrors := False; // Production
fClient.Options^.Timeout := 30000; // 30 seconds

// Parse HTTPS URL
if not uri.From(Utf8Encode(edtUrl.Text)) then
  raise Exception.Create('Invalid URL');

if not uri.Https then
  raise Exception.Create('URL must use HTTPS');

// Execute secure request
status := fClient.Request(uri, 'GET', nil, '', headers, response);

// Format JSON response
formatted := JsonReformat(response, jsonHumanReadable);
```

#### Learning Objectives

- HTTPS/TLS client implementation
- Certificate validation strategies
- Security best practices
- Development vs production configuration
- Error handling for certificate issues
- Client certificate authentication
- Custom CA certificate usage

#### Prerequisites

For testing with self-signed certificates:
1. Run `11-ssl_server.exe` (creates HTTPS server on port 8443)
2. Or use any HTTPS endpoint (GitHub API, etc.)

#### Security Warnings

⚠️ **CRITICAL**:
- `IgnoreCertificateErrors = True` should **ONLY** be used in development
- **NEVER** deploy to production with certificate validation disabled
- Always use proper certificates in production environments

#### Comparison with DMVC

| Aspect | DMVC | mORMot2 |
|--------|------|---------|
| TLS Config | `SetValidateServerCertificateProc` | `Options^.TLS.IgnoreCertificateErrors` |
| Certificate Callback | Procedure with `Accepted` parameter | Boolean flag |
| Client Creation | `TMVCRESTClient.New.BaseURL(...)` | `TSimpleHttpClient.Create` + `Request(...)` |
| CA Certificates | Component properties | `TLS.CACertificatesFile` |
| Client Certs | Component properties | `TLS.CertificateFile` + `PrivateKeyFile` |

#### Advanced Features (in code, commented)

- Custom CA certificate bundles
- Client certificate authentication (mutual TLS)
- TLS version control (disable deprecated protocols)
- Debug logging for TLS issues

#### Related Examples

- **11-ssl_server** - HTTPS server for testing
- **19-basicdemo_vclclient** - Basic HTTP client (non-secure)
- **21-restclient** - Advanced REST client features

#### Troubleshooting

| Issue | Cause | Solution |
|-------|-------|----------|
| Certificate validation failed | Untrusted certificate | Enable ignore or install CA cert |
| Connection refused | Server not running | Start HTTPS server first |
| TLS handshake failed | TLS version mismatch | Check server TLS config |

✅ **Compiles successfully** (0 errors, 0 warnings, 0 hints)

---

## Samples by Topic

### REST API Basics
- 01 - basicdemo_server
- 02 - console_sample
- 03 - routing
- 04 - renders

### Database & ORM
- 05 - datasets
- 06 - articles_crud_server
- 07 - master_details

### Security & Authentication
- 08 - basicauth
- 09 - custom_auth
- 10 - jsonwebtoken
- 11 - ssl_server
- 23 - ssl_client
- 24 - hmac_auth

### Middleware & Filters
- 12 - middleware
- 13 - middleware_cors
- 14 - middleware_compression
- 15 - middleware_staticfiles

### Real-time Communication
- 16 - serversentevents
- 17 - websocket_primer

### File Handling
- 18 - file_upload

### Client Applications
- 19 - basicdemo_vclclient
- 21 - restclient
- 23 - ssl_client

---

## Samples by Difficulty

### ⭐ Beginner (Start Here)
- 01 - basicdemo_server
- 02 - console_sample
- 19 - basicdemo_vclclient

### ⭐⭐ Intermediate
- 03 - routing
- 04 - renders
- 05 - datasets
- 06 - articles_crud_server
- 08 - basicauth
- 12 - middleware
- 13 - middleware_cors
- 14 - middleware_compression
- 15 - middleware_staticfiles
- 18 - file_upload
- 21 - restclient
- 23 - ssl_client
- 24 - hmac_auth

### ⭐⭐⭐ Advanced
- 07 - master_details
- 09 - custom_auth
- 10 - jsonwebtoken
- 11 - ssl_server
- 16 - serversentevents
- 17 - websocket_primer

---

## Testing Status

See [samples-tested.md](samples-tested.md) for detailed testing results.

### Compilation

✅ **All 19 samples compile successfully**
- Configuration: Debug/Release, Win32/Win64
- Compiler: Delphi 12 Athens
- Errors: 0
- Warnings: 0
- Hints: 0

### Runtime Testing

⏳ **In progress** - See individual sample VERIFICATION.md files

---

## Template Sample

**Directory**: `_template/`

Use this as a starting point for creating new samples. See [_template/README.md](_template/README.md) for details.

---

## Additional Resources

- **Main README**: [README.md](README.md) - Overview and quick start
- **Getting Started**: [GETTING-STARTED.md](GETTING-STARTED.md) - Beginner tutorial
- **Conversion Guide**: [CONVERSION-GUIDE.md](CONVERSION-GUIDE.md) - DMVC → mORMot2 mapping
- **Architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - Technical patterns

---

**Last Updated**: 2025-12-20
**Total Samples**: 19
**Compilation Status**: ✅ 100% success rate
