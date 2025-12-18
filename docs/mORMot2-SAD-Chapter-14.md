# 14. Client-Server Services via Methods

*The Quick and Powerful Way*

To implement a service in the *Synopse mORMot 2 framework*, the most direct approach is to define published methods on the server side, then use simple JSON or URL parameter handling to encode and decode requests on both ends.

This chapter covers **method-based services** — a straightforward, low-level mechanism for exposing custom functionality over HTTP. While mORMot 2 also provides interface-based services (covered in chapters 15-16) for more structured SOA, method-based services offer maximum flexibility and control.

---

## 14.1. Publishing a Service on the Server

### 14.1.1. Basic Structure

On the server side, we customize a `TRestServer` descendant (typically `TRestServerDB` with SQLite3, or the lighter `TRestServerFullMemory`) by adding a new `published` method:

```pascal
type
  TRestServerTest = class(TRestServerFullMemory)
  published
    procedure Sum(Ctxt: TRestServerUriContext);
  end;
```

The method name (`Sum`) determines the URI routing — it will be accessible remotely from `ModelRoot/Sum`. The `ModelRoot` is the `Root` parameter defined when creating the model.

### 14.1.2. Method Signature

All server-side methods **MUST** follow the `TOnRestServerCallBack` prototype:

```pascal
type
  TOnRestServerCallBack = procedure(Ctxt: TRestServerUriContext) of object;
```

The single `Ctxt` parameter provides full access to the execution context: incoming parameters, HTTP headers, session information, and output facilities.

### 14.1.3. Implementation

```pascal
procedure TRestServerTest.Sum(Ctxt: TRestServerUriContext);
begin
  Ctxt.Results([Ctxt.InputDouble['a'] + Ctxt.InputDouble['b']]);
end;
```

The `Ctxt` object exposes typed properties for parameter retrieval:

| Property | Return Type | Exception on Missing |
|----------|-------------|---------------------|
| `InputInt['name']` | `Int64` | Yes |
| `InputDouble['name']` | `Double` | Yes |
| `InputUtf8['name']` | `RawUtf8` | Yes |
| `Input['name']` | `Variant` | Yes |
| `InputIntOrVoid['name']` | `Int64` | No (returns 0) |
| `InputDoubleOrVoid['name']` | `Double` | No (returns 0) |
| `InputUtf8OrVoid['name']` | `RawUtf8` | No (returns '') |
| `InputOrVoid['name']` | `Variant` | No (returns Unassigned) |
| `InputExists['name']` | `Boolean` | N/A |

The default `Input['name']` array property (via `variant`) allows the concise syntax `Ctxt['name']`.

### 14.1.4. Response Format

`Ctxt.Results([])` encodes values as a JSON object with a `"Result"` member:

```
GET /root/Sum?a=3.12&b=4.2
```

Returns:
```json
{"Result":7.32}
```

This is perfectly AJAX-friendly and compatible with any HTTP client.

### 14.1.5. Thread Safety

**Important**: Method implementations **MUST be thread-safe**. The `TRestServer.Uri` method expects callbacks to handle thread safety internally. This design maximizes performance and scalability by allowing fine-grained resource locking.

For read-only operations, no locking may be needed. For shared state modifications:

```pascal
procedure TRestServerTest.UpdateCounter(Ctxt: TRestServerUriContext);
begin
  fCounterLock.Lock;
  try
    Inc(fCounter);
    Ctxt.Results([fCounter]);
  finally
    fCounterLock.UnLock;
  end;
end;
```

---

## 14.2. Defining the Client

### 14.2.1. Basic Client Call

The client uses dedicated methods to call services by name with parameters:

```pascal
function Sum(aClient: TRestClientUri; a, b: Double): Double;
var
  err: Integer;
begin
  Val(aClient.CallBackGetResult('sum', ['a', a, 'b', b]), Result, err);
end;
```

### 14.2.2. Client Method Pattern

A cleaner approach encapsulates service calls in a dedicated client class:

```pascal
type
  TMyClient = class(TRestHttpClientSocket)  // or TRestHttpClientWebSockets
  public
    function Sum(a, b: Double): Double;
  end;

function TMyClient.Sum(a, b: Double): Double;
var
  err: Integer;
begin
  Val(CallBackGetResult('sum', ['a', a, 'b', b]), Result, err);
end;
```

### 14.2.3. Client API Methods

`TRestClientUri` provides several methods for service invocation:

| Method | Purpose |
|--------|---------|
| `CallBackGetResult` | GET request, returns the JSON `"Result"` value as `RawUtf8` |
| `CallBackGet` | GET request, returns full response with HTTP status |
| `CallBackPut` | PUT request with body data |
| `CallBack` | Generic request with any HTTP method |

#### CallBackGet Signature

```pascal
function CallBackGet(const aMethodName: RawUtf8;
  const aNameValueParameters: array of const;
  out aResponse: RawUtf8;
  aTable: TOrmClass = nil;
  aID: TID = 0;
  aResponseHead: PRawUtf8 = nil): Integer;
```

#### CallBackGetResult Signature

```pascal
function CallBackGetResult(const aMethodName: RawUtf8;
  const aNameValueParameters: array of const;
  aTable: TOrmClass = nil;
  aID: TID = 0): RawUtf8;
```

### 14.2.4. Object Parameters

Objects can be serialized to JSON automatically:

```pascal
function TMyClient.ProcessPerson(Person: TPerson): RawUtf8;
begin
  Result := CallBackGetResult('processperson', ['person', ObjectToJson(Person)]);
end;
```

---

## 14.3. Direct Parameter Marshalling

### 14.3.1. Low-Level Access

For maximum performance, bypass the high-level `Input*[]` properties and parse `Ctxt.Parameters` directly:

```pascal
procedure TRestServerTest.Sum(Ctxt: TRestServerUriContext);
var
  a, b: Double;
begin
  if UrlDecodeNeedParameters(Ctxt.Parameters, 'A,B') then
  begin
    while Ctxt.Parameters <> nil do
    begin
      UrlDecodeDouble(Ctxt.Parameters, 'A=', a);
      UrlDecodeDouble(Ctxt.Parameters, 'B=', b, @Ctxt.Parameters);
    end;
    Ctxt.Results([a + b]);
  end
  else
    Ctxt.Error('Missing Parameter');
end;
```

### 14.3.2. URL Decode Functions

Available in `mormot.core.text`:

| Function | Purpose |
|----------|---------|
| `UrlDecodeNeedParameters` | Verify required parameters exist |
| `UrlDecodeInteger` | Extract integer parameter |
| `UrlDecodeInt64` | Extract 64-bit integer |
| `UrlDecodeDouble` | Extract floating-point |
| `UrlDecodeValue` | Extract string value |
| `UrlDecodeObject` | Deserialize JSON to object |

### 14.3.3. JSON Body Access

For POST/PUT requests, the body is available in `Ctxt.Call^.InBody`:

```pascal
procedure TRestServerTest.ProcessData(Ctxt: TRestServerUriContext);
var
  doc: TDocVariantData;
begin
  if doc.InitJson(Ctxt.Call^.InBody, JSON_FAST) then
  begin
    // Process doc...
    Ctxt.Success;
  end
  else
    Ctxt.Error('Invalid JSON body');
end;
```

---

## 14.4. Returning Non-JSON Content

### 14.4.1. Custom MIME Types

Use `Ctxt.Returns()` to return any content type:

```pascal
procedure TRestServer.Timestamp(Ctxt: TRestServerUriContext);
begin
  Ctxt.Returns(Int64ToUtf8(ServerTimestamp), HTTP_SUCCESS, TEXT_CONTENT_TYPE_HEADER);
end;
```

### 14.4.2. Binary File Response

```pascal
procedure TRestServer.GetFile(Ctxt: TRestServerUriContext);
var
  fileName: TFileName;
  content: RawByteString;
begin
  fileName := 'c:\data\' + ExtractFileName(Utf8ToString(Ctxt.InputUtf8['filename']));
  content := StringFromFile(fileName);
  if content = '' then
    Ctxt.Error('', HTTP_NOTFOUND)
  else
    Ctxt.Returns(content, HTTP_SUCCESS,
      HEADER_CONTENT_TYPE + GetMimeContentType(pointer(content), Length(content), fileName));
end;
```

### 14.4.3. Client-Side Handling

```pascal
function TMyClient.GetFile(const aFileName: RawUtf8): RawByteString;
var
  resp: RawUtf8;
begin
  if CallBackGet('GetFile', ['filename', aFileName], resp) <> HTTP_SUCCESS then
    raise Exception.CreateFmt('Impossible to get file: %s', [resp]);
  Result := RawByteString(resp);
end;
```

Note: For file serving, prefer `Ctxt.ReturnFile()` or `Ctxt.ReturnFileFromFolder()` (covered in section 14.7).

---

## 14.5. Advanced Server-Side Processing

### 14.5.1. RESTful URI with Table Context

Methods can be linked to ORM tables via RESTful URIs like `ModelRoot/TableName/TableID/MethodName`:

```pascal
procedure TRestServerTest.DataAsHex(Ctxt: TRestServerUriContext);
var
  aData: RawBlob;
begin
  if (Self = nil) or (Ctxt.Table <> TOrmPeople) or (Ctxt.TableID <= 0) then
    Ctxt.Error('Need a valid record and its ID')
  else if (Ctxt.Server.Orm as TRestOrmServer).RetrieveBlob(
      TOrmPeople, Ctxt.TableID, 'Data', aData) then
    Ctxt.Results([BinToHex(aData)])
  else
    Ctxt.Error('Impossible to retrieve the Data BLOB field');
end;
```

Corresponding client call:

```pascal
function TOrmPeople.DataAsHex(aClient: TRestClientUri): RawUtf8;
begin
  Result := aClient.CallBackGetResult('DataAsHex', [], TOrmPeople, ID);
end;
```

### 14.5.2. Context Properties

The `TRestServerUriContext` exposes rich information:

| Property | Description |
|----------|-------------|
| `Table` | `TOrmClass` decoded from URI |
| `TableIndex` | Index in Server.Model |
| `TableID` | Record ID from URI |
| `Session` | Session ID (0 = not started, 1 = auth disabled) |
| `SessionUser` | Current user's TID |
| `SessionGroup` | Current group's TID |
| `SessionUserName` | User's logon name |
| `Method` | HTTP verb (`mGET`, `mPOST`, etc.) |
| `Call^.InHead` | Raw incoming HTTP headers |
| `Call^.InBody` | Raw request body |
| `RemoteIP` | Client IP address |
| `UserAgent` | Client user-agent string |

### 14.5.3. Session and User Details

```pascal
procedure TRestServerTest.WhoAmI(Ctxt: TRestServerUriContext);
var
  User: TAuthUser;
begin
  if Ctxt.Session = CONST_AUTHENTICATION_NOT_USED then
    Ctxt.Returns(['message', 'Authentication not enabled'])
  else if Ctxt.Session = CONST_AUTHENTICATION_SESSION_NOT_STARTED then
    Ctxt.Returns(['message', 'Not authenticated'])
  else
  begin
    User := Ctxt.Server.SessionGetUser(Ctxt.Session);
    try
      if User <> nil then
        Ctxt.Returns(['user', User.LogonName, 'group', Ctxt.SessionGroup])
      else
        Ctxt.Error('Session not found', HTTP_FORBIDDEN);
    finally
      User.Free;
    end;
  end;
end;
```

---

## 14.6. Browser Speed-Up for Unmodified Requests

### 14.6.1. HTTP 304 Not Modified

The optional `Handle304NotModified` parameter enables browser caching:

```pascal
procedure TRestServerTest.GetStaticData(Ctxt: TRestServerUriContext);
var
  data: RawUtf8;
begin
  data := GetCachedData; // Your cached data source
  Ctxt.Returns(data, HTTP_SUCCESS, JSON_CONTENT_TYPE_HEADER, true); // Handle304NotModified=true
end;
```

When enabled:
- Response content is hashed using `crc32c` (with SSE4.2 hardware acceleration if available)
- If unchanged since the last request, returns `304 Not Modified` without body
- Significantly reduces bandwidth for periodic polling

### 14.6.2. Caveats

- **Authentication conflict**: RESTful authentication uses per-URI signatures that change frequently. Use `Server.ServiceMethodByPassAuthentication()` to disable authentication for cached methods.
- **Hash collisions**: While extremely rare with `crc32c`, false positives are theoretically possible. Don't use for sensitive accounting data.

### 14.6.3. CDN Integration

This stateless REST model enables multiple levels of caching:
- Browser cache
- Proxy servers
- Content Delivery Networks (CDN)

Combined with proper HTTP headers, your mORMot server can scale to thousands of concurrent users worldwide.

---

## 14.7. Returning File Content

### 14.7.1. ReturnFile Method

`Ctxt.ReturnFile()` efficiently serves files with automatic MIME type detection:

```pascal
procedure TRestServerTest.DownloadReport(Ctxt: TRestServerUriContext);
begin
  Ctxt.ReturnFile('c:\reports\' + Ctxt.InputUtf8['name'] + '.pdf', true);
end;
```

Features:
- Automatic MIME type from file extension
- Optional `Handle304NotModified` using file timestamp
- High-performance: HTTP.SYS (Windows) serves files asynchronously from kernel mode

### 14.7.2. ReturnFileFromFolder Method

Serves any file from a folder based on the URI path:

```pascal
procedure TRestServerTest.StaticFiles(Ctxt: TRestServerUriContext);
begin
  Ctxt.ReturnFileFromFolder('c:\www\static\', true, 'index.html', '/404.html');
end;
```

Parameters:
- `FolderName`: Base folder path
- `Handle304NotModified`: Enable browser caching
- `DefaultFileName`: Served for root requests (default: `'index.html'`)
- `Error404Redirect`: Redirect URI for missing files

This is ideal for serving static web content (HTML, CSS, JS, images) alongside your REST API.

---

## 14.8. JSON Web Tokens (JWT)

### 14.8.1. Overview

JSON Web Tokens (JWT) provide stateless authentication and secure information exchange. mORMot 2 implements JWT in `mormot.crypt.jwt`:

**Supported Algorithms**:
| Algorithm | Description |
|-----------|-------------|
| `HS256/384/512` | HMAC-SHA2 (symmetric) |
| `ES256` | ECDSA P-256 (asymmetric) |
| `RS256/384/512` | RSA (asymmetric) |
| `PS256/384/512` | RSA-PSS (asymmetric) |
| `S3256/384/512` | SHA-3 (non-standard) |
| `none` | No signature (use with caution) |

**Features**:
- All standard JWT claims validated
- Thread-safe with optional caching
- Cross-platform (no external DLLs)
- Immune to algorithm confusion attacks

### 14.8.2. Class Hierarchy

```
TJwtAbstract
├── TJwtNone           (algorithm: "none")
├── TJwtSynSignerAbstract
│   ├── TJwtHS256/384/512   (HMAC-SHA2)
│   └── TJwtS3256/384/512   (SHA-3)
├── TJwtES256          (ECDSA P-256)
├── TJwtRS256/384/512  (RSA)
├── TJwtPS256/384/512  (RSA-PSS)
└── TJwtCrypt          (factory-based, recommended)
```

> **Note**: Only HMAC-SHA2 and SHA-3 variants inherit from `TJwtSynSignerAbstract`. The asymmetric algorithms (ECDSA, RSA, RSA-PSS) are direct descendants of `TJwtAbstract`.

### 14.8.3. Verifying JWTs

```pascal
uses
  mormot.crypt.jwt;

var
  jwt: TJwtAbstract;
  content: TJwtContent;
begin
  jwt := TJwtHS256.Create('secret', 0, [jrcSubject], []);
  try
    jwt.Verify(
      'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.' +
      'eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.' +
      'TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ',
      content);

    Assert(content.result = jwtValid);
    Assert(content.reg[jrcSubject] = '1234567890');
    Assert(content.data.U['name'] = 'John Doe');
    Assert(content.data.B['admin'] = True);
  finally
    jwt.Free;
  end;
end;
```

### 14.8.4. Creating JWTs

```pascal
var
  jwt: TJwtAbstract;
  token: RawUtf8;
begin
  jwt := TJwtHS256.Create('secret', 10000,  // 10000 PBKDF2 rounds
    [jrcIssuer, jrcExpirationTime, jrcIssuedAt, jrcJWTID], [], 3600);  // 1 hour expiry
  try
    token := jwt.Compute(['http://example.com/is_root', True], 'joe');
    // token payload: {"http://example.com/is_root":true,"iss":"joe","iat":...,"exp":...,"jti":...}
  finally
    jwt.Free;
  end;
end;
```

### 14.8.5. JWT in Method-Based Services

Integrate JWT validation using `Ctxt.AuthenticationCheck()`:

```pascal
type
  TMyDaemon = class(TRestServerFullMemory)
  private
    fJwt: TJwtAbstract;
  public
    constructor Create(aModel: TOrmModel);
  published
    procedure SecureFiles(Ctxt: TRestServerUriContext);
  end;

constructor TMyDaemon.Create(aModel: TOrmModel);
begin
  inherited Create(aModel);
  fJwt := TJwtHS256.Create('my-secret-key', 10000, [jrcSubject], [], 3600);
end;

procedure TMyDaemon.SecureFiles(Ctxt: TRestServerUriContext);
begin
  if Ctxt.AuthenticationCheck(fJwt) then  // Returns boolean
    Ctxt.ReturnFileFromFolder('c:\protected\')
  else
    ; // AuthenticationCheck already returned HTTP 401
end;
```

### 14.8.6. Server-Wide JWT Authentication

Assign a JWT instance to handle all unauthenticated requests:

```pascal
Server.JwtForUnauthenticatedRequest := TJwtHS256.Create('secret', 10000, [], []);
```

---

## 14.9. Handling Errors

### 14.9.1. Automatic Exception Handling

Missing parameters in `Input*[]` properties raise `EParsingException`, which the server catches and returns as a structured error response:

```json
{
  "ErrorCode": 400,
  "ErrorText": "EParsingException: Missing parameter 'name'"
}
```

### 14.9.2. Explicit Error Handling

Use `Ctxt.Error()` for custom error responses:

```pascal
procedure TRestServer.UpdateRecord(Ctxt: TRestServerUriContext);
begin
  if not CanUpdate(Ctxt.InputInt['id']) then
    Ctxt.Error('Record is locked', HTTP_FORBIDDEN)
  else if DoUpdate(Ctxt.InputInt['id'], Ctxt.InputUtf8['data']) then
    Ctxt.Success
  else
    Ctxt.Error('Update failed', HTTP_SERVERERROR);
end;
```

### 14.9.3. Success Without Content

For operations that don't return data:

```pascal
procedure TRestServer.DeleteItem(Ctxt: TRestServerUriContext);
begin
  if DoDelete(Ctxt.InputInt['id']) then
    Ctxt.Success  // Returns HTTP 200 with empty body
  else
    Ctxt.Error('Delete failed');
end;
```

### 14.9.4. HTTP Status Codes

| Method | Default Status |
|--------|----------------|
| `Ctxt.Results()` | 200 OK |
| `Ctxt.Returns()` | 200 OK (customizable) |
| `Ctxt.Success()` | 200 OK (customizable) |
| `Ctxt.Error()` | 400 Bad Request (customizable) |

Common status codes:
- `HTTP_SUCCESS` = 200
- `HTTP_CREATED` = 201
- `HTTP_NOCONTENT` = 204
- `HTTP_BADREQUEST` = 400
- `HTTP_FORBIDDEN` = 403
- `HTTP_NOTFOUND` = 404
- `HTTP_SERVERERROR` = 500

---

## 14.10. Bypassing Authentication

### 14.10.1. Per-Method Bypass

Certain methods (like `Timestamp` or public API endpoints) should be accessible without authentication:

```pascal
Server.ServiceMethodByPassAuthentication('Timestamp');
Server.ServiceMethodByPassAuthentication('GetPublicData');
```

### 14.10.2. Allowed HTTP Methods

Restrict which HTTP verbs are allowed for a method:

```pascal
// In TRestServerMethod, set during initialization
Server.PublishedMethods['MyMethod'].Methods := [mGET, mPOST];
```

---

## 14.11. Benefits and Limitations

### 14.11.1. Benefits

Method-based services provide:

| Benefit | Description |
|---------|-------------|
| **Full control** | Direct access to HTTP headers, binary data, custom MIME types |
| **RESTful integration** | Can be linked to ORM tables via URI routing |
| **Low overhead** | Minimal abstraction layer, maximum performance |
| **Flexibility** | Handle any request type (AJAX, SOAP, custom protocols) |
| **Simple debugging** | Direct mapping between URI and code |

### 14.11.2. Security

The mORMot implementation is inherently secure against certain attacks:
- **Hash collision attacks**: Not vulnerable (unlike some Apache configurations)
- **Parameter injection**: Typed accessors validate input
- **Thread safety**: Enforced by design

### 14.11.3. Limitations

| Limitation | Solution |
|------------|----------|
| Manual parameter marshalling | Use interface-based services (Chapter 16) |
| No automatic client stub generation | Use interface-based services |
| Flat service namespace | Organize via naming conventions or interfaces |
| No automatic documentation | Generate manually or use OpenAPI export |

### 14.11.4. When to Use

**Use method-based services when**:
- You need binary data handling or custom MIME types
- You're building a simple REST API
- You need maximum performance
- You're integrating with non-mORMot clients
- You want full HTTP control

**Use interface-based services when**:
- Building complex SOA systems
- You want automatic parameter marshalling
- You need client stub generation
- You prefer strongly-typed contracts

---

## 14.12. Complete Example

### 14.12.1. Server

```pascal
unit RestServerUnit;

interface

uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.json,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver;

type
  TMyRestServer = class(TRestServerFullMemory)
  published
    procedure Sum(Ctxt: TRestServerUriContext);
    procedure Echo(Ctxt: TRestServerUriContext);
    procedure Time(Ctxt: TRestServerUriContext);
  end;

implementation

procedure TMyRestServer.Sum(Ctxt: TRestServerUriContext);
begin
  Ctxt.Results([Ctxt.InputDouble['a'] + Ctxt.InputDouble['b']]);
end;

procedure TMyRestServer.Echo(Ctxt: TRestServerUriContext);
begin
  Ctxt.Returns(Ctxt.Call^.InBody, HTTP_SUCCESS, TEXT_CONTENT_TYPE_HEADER);
end;

procedure TMyRestServer.Time(Ctxt: TRestServerUriContext);
begin
  Ctxt.Returns(['timestamp', ServerTimestamp, 'utc', DateTimeToIso8601(NowUtc, true)]);
end;

end.
```

### 14.12.2. Client

```pascal
unit RestClientUnit;

interface

uses
  mormot.core.base,
  mormot.rest.client,
  mormot.rest.http.client;

type
  TMyRestClient = class(TRestHttpClientSocket)
  public
    function Sum(a, b: Double): Double;
    function Echo(const Text: RawUtf8): RawUtf8;
    function GetServerTime: TDateTime;
  end;

implementation

uses
  mormot.core.json;

function TMyRestClient.Sum(a, b: Double): Double;
var
  err: Integer;
begin
  Val(CallBackGetResult('sum', ['a', a, 'b', b]), Result, err);
end;

function TMyRestClient.Echo(const Text: RawUtf8): RawUtf8;
var
  resp: RawUtf8;
begin
  if CallBack(mPOST, 'echo', Text, resp) = HTTP_SUCCESS then
    Result := resp
  else
    Result := '';
end;

function TMyRestClient.GetServerTime: TDateTime;
var
  doc: TDocVariantData;
  resp: RawUtf8;
begin
  if CallBackGet('time', [], resp) = HTTP_SUCCESS then
  begin
    doc.InitJson(resp, JSON_FAST);
    Result := Iso8601ToDateTime(doc.U['utc']);
  end
  else
    Result := 0;
end;

end.
```

### 14.12.3. Main Program

```pascal
program MethodServicesDemo;

uses
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.http.server,
  RestServerUnit,
  RestClientUnit;

var
  Model: TOrmModel;
  Server: TMyRestServer;
  HttpServer: TRestHttpServer;
  Client: TMyRestClient;
begin
  Model := TOrmModel.Create([], 'root');
  Server := TMyRestServer.Create(Model);
  try
    Server.ServiceMethodByPassAuthentication('Time');

    HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);
    try
      // Client demo
      Client := TMyRestClient.Create('localhost', '8080', TOrmModel.Create([], 'root'));
      try
        WriteLn('Sum(3.5, 2.5) = ', Client.Sum(3.5, 2.5):0:2);
        WriteLn('Echo: ', Client.Echo('Hello mORMot!'));
        WriteLn('Server time: ', DateTimeToStr(Client.GetServerTime));
      finally
        Client.Free;
      end;

      WriteLn('Press Enter to stop...');
      ReadLn;
    finally
      HttpServer.Free;
    end;
  finally
    Server.Free;
    Model.Free;
  end;
end.
```

---

## Summary

Method-based services in mORMot 2 provide:

- **Direct HTTP control**: Full access to headers, body, and response formatting
- **Simple implementation**: Just add a published method with the right signature
- **Flexible responses**: JSON, HTML, binary, any MIME type
- **RESTful integration**: Link methods to ORM tables via URI patterns
- **JWT support**: Built-in token validation
- **Browser caching**: HTTP 304 support for optimized polling
- **Thread safety**: By design, with fine-grained locking

While interface-based services (covered in Chapter 16) offer more structure for complex SOA systems, method-based services remain the go-to choice for simple APIs, binary data handling, and maximum control over the HTTP layer.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 13: Server-Side ORM Processing](mORMot2-SAD-Chapter-13.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 15: Interfaces and SOLID Design](mORMot2-SAD-Chapter-15.md) |
