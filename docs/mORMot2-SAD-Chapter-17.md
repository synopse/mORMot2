# 17. Cross-Platform Clients

*Reaching Beyond Windows*

mORMot 2 is designed from the ground up as a **cross-platform framework**. The core libraries compile natively on Windows, Linux, macOS, FreeBSD, and Android using either Delphi or Free Pascal. This chapter covers strategies for consuming mORMot services from various platforms and generating client code.

---

## 17.1. Native Cross-Platform Support

### 17.1.1. mORMot 2 Platform Coverage

Unlike mORMot 1 (which required separate `SynCrossPlatform*` units), mORMot 2's main units are inherently cross-platform:

| Platform | Delphi | Free Pascal |
|----------|--------|-------------|
| Windows (32/64-bit) | ✅ | ✅ |
| Linux (x86_64, aarch64) | ✅ (Delphi 12+) | ✅ |
| macOS (x86_64, aarch64) | ✅ | ✅ |
| FreeBSD | — | ✅ |
| Android | ✅ (FireMonkey) | ✅ |
| iOS | ✅ (FireMonkey) | — |

### 17.1.2. Core Cross-Platform Units

The `mormot.core.*` units provide the foundation:

| Unit | Purpose |
|------|---------|
| `mormot.core.base` | Base types, memory management |
| `mormot.core.os` | OS abstraction (files, threads, processes) |
| `mormot.core.unicode` | UTF-8/UTF-16 handling |
| `mormot.core.text` | Text processing, formatting |
| `mormot.core.buffers` | Binary data handling |
| `mormot.core.data` | Collections, dynamic arrays |
| `mormot.core.json` | JSON parsing and generation |
| `mormot.core.variants` | TDocVariant for flexible JSON |
| `mormot.core.rtti` | Cross-platform RTTI |
| `mormot.core.interfaces` | Interface invocation, stubs, mocks |

These units have **zero GUI dependencies** and work identically across all supported platforms.

### 17.1.3. Network Layer

HTTP client support via `mormot.net.*`:

```pascal
uses
  mormot.net.client,
  mormot.rest.http.client;

var
  Client: TRestHttpClientSocket;
begin
  // Works on Windows, Linux, macOS, etc.
  Client := TRestHttpClientSocket.Create('api.example.com', '443', Model, True);
  try
    Client.ServiceDefine([IMyService], sicShared);
    // Use services...
  finally
    Client.Free;
  end;
end;
```

Available HTTP client classes:

| Class | Transport | Platform |
|-------|-----------|----------|
| `TRestHttpClientSocket` | Raw sockets | All |
| `TRestHttpClientWebSockets` | WebSocket | All |
| `TRestHttpClientWinHttp` | WinHTTP API | Windows |
| `TRestHttpClientCurl` | libcurl | Linux/macOS |

---

## 17.2. Generating Client Wrappers

### 17.2.1. The Code Generation Framework

mORMot 2 includes a powerful code generation system in `mormot.soa.codegen.pas` that creates client wrappers from server definitions using Mustache templates.

Key functions:

| Function | Purpose |
|----------|---------|
| `ContextFromModel()` | Extract ORM/SOA metadata as JSON |
| `WrapperFromModel()` | Generate code from Mustache template |
| `WrapperMethod()` | HTTP handler for browser-based generation |
| `AddToServerWrapperMethod()` | Add wrapper endpoint to server |

### 17.2.2. Publishing the Wrapper Endpoint

```pascal
program MyServer;

uses
  mormot.rest.server,
  mormot.rest.http.server,
  mormot.soa.codegen;

var
  Server: TRestServerDB;
  HttpServer: TRestHttpServer;
begin
  Server := TRestServerDB.Create(Model, 'data.db3');
  try
    Server.ServiceDefine(TMyService, [IMyService], sicShared);

    // Add wrapper generation endpoint
    AddToServerWrapperMethod(Server, ['./templates', '../templates']);

    HttpServer := TRestHttpServer.Create('8080', [Server]);
    try
      WriteLn('Wrapper available at http://localhost:8080/root/wrapper');
      ReadLn;
    finally
      HttpServer.Free;
    end;
  finally
    Server.Free;
  end;
end.
```

### 17.2.3. Using the Web Interface

Navigate to `http://localhost:8080/root/wrapper` in your browser:

```
Client Wrappers
===============

Available Templates:
- Delphi
  mORMotClient.pas - download as file | see as text | see template

- TypeScript
  mORMotClient.ts - download as file | see as text | see template

- OpenAPI
  openapi.json - download as file | see as text | see template

Template context (JSON)
```

### 17.2.4. Mustache Template System

Templates use the Mustache logic-less syntax:

```mustache
// Generated client for {{root}} API
unit {{filename}};

interface

uses
  mormot.core.base,
  mormot.rest.client;

type
{{#services}}
  {{interfaceName}} = interface(IInvokable)
    ['{{{guid}}}']
  {{#methods}}
    {{declaration}};
  {{/methods}}
  end;

{{/services}}

implementation

{{#services}}
// {{interfaceName}} implementation
{{#methods}}
function T{{serviceName}}.{{methodName}}({{args}}): {{resultType}};
begin
  // Generated stub code
end;

{{/methods}}
{{/services}}

end.
```

### 17.2.5. Available Template Variables

The context includes:

| Variable | Description |
|----------|-------------|
| `{{root}}` | API root URI |
| `{{port}}` | Server port |
| `{{filename}}` | Output filename |
| `{{orm}}` | Array of ORM classes |
| `{{services}}` | Array of service interfaces |
| `{{#service.methods}}` | Methods within service |
| `{{typeDelphi}}` | Delphi type name |
| `{{typeTS}}` | TypeScript type name |
| `{{typeCS}}` | C# type name |
| `{{typeJava}}` | Java type name |

---

## 17.3. Delphi/FPC Native Clients

### 17.3.1. Direct mORMot Usage

The simplest approach — use mORMot directly on any supported platform:

```pascal
program CrossPlatformClient;

{$APPTYPE CONSOLE}

uses
  mormot.core.base,
  mormot.core.os,
  mormot.orm.core,
  mormot.rest.http.client,
  MyServiceInterface;

var
  Client: TRestHttpClientSocket;
  Service: IMyService;
begin
  Client := TRestHttpClientSocket.Create('server.example.com', '8080',
    TOrmModel.Create([], 'api'));
  try
    Client.ServiceDefine([IMyService], sicShared);

    if Client.Services.Resolve(IMyService, Service) then
      WriteLn('Result: ', Service.Calculate(10, 20));
  finally
    Client.Free;
  end;
end.
```

This compiles and runs identically on Windows, Linux, and macOS.

### 17.3.2. Generated Client Wrapper

For projects that can't include full mORMot dependencies, use generated wrappers:

```pascal
// Generated mORMotClient.pas
unit mORMotClient;

interface

uses
  mormot.core.base,
  mormot.rest.client;

type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1, n2: Integer): Integer;
    function Multiply(n1, n2: Int64): Int64;
  end;

/// Create a connected client instance
function GetClient(const aServer: RawUtf8;
  const aPort: RawUtf8 = '8080'): TRestHttpClientSocket;

implementation

function GetClient(const aServer, aPort: RawUtf8): TRestHttpClientSocket;
begin
  Result := TRestHttpClientSocket.Create(aServer, aPort,
    TOrmModel.Create([], 'api'));
  Result.ServiceDefine([ICalculator], sicShared);
end;

end.
```

### 17.3.3. FPC-Specific Generation

For Free Pascal, additional RTTI registration may be needed:

```pascal
procedure ComputeFPCInterfacesUnit(const Path: array of TFileName;
  DestFileName: TFileName = '');
```

This generates a unit with explicit interface registration to work around FPC RTTI limitations.

---

## 17.4. REST/JSON Clients (Any Language)

### 17.4.1. Protocol Overview

mORMot services use standard HTTP with JSON:

**Request Format:**
```
GET /api/Calculator/Add?n1=10&n2=20 HTTP/1.1
Host: server.example.com
```

**Response Format:**
```json
{"result": 30}
```

For POST requests with complex parameters:
```
POST /api/Calculator.Add HTTP/1.1
Content-Type: application/json

[10, 20]
```

### 17.4.2. Authentication

mORMot's default authentication uses a challenge-response protocol:

1. Client requests timestamp: `GET /api/auth`
2. Server returns: `{"result": "1234567890"}`
3. Client computes: `HMAC-SHA256(password, timestamp + username)`
4. Client authenticates: `GET /api/auth?UserName=xxx&PasswordHashHexa=yyy&ClientNonce=zzz`
5. Server returns session info

For simpler integration, consider:
- **Basic Auth**: `Authorization: Basic base64(user:pass)`
- **Bearer Token**: `Authorization: Bearer jwt_token`
- **No Auth**: `Server.AuthenticationRegister(TRestServerAuthenticationNone)`

### 17.4.3. TypeScript/JavaScript Client

Example generated TypeScript client:

```typescript
// mORMotClient.ts
export interface ICalculator {
  add(n1: number, n2: number): Promise<number>;
  multiply(n1: number, n2: number): Promise<number>;
}

export class CalculatorClient implements ICalculator {
  constructor(private baseUrl: string) {}

  async add(n1: number, n2: number): Promise<number> {
    const response = await fetch(
      `${this.baseUrl}/Calculator/Add?n1=${n1}&n2=${n2}`
    );
    const data = await response.json();
    return data.result;
  }

  async multiply(n1: number, n2: number): Promise<number> {
    const response = await fetch(
      `${this.baseUrl}/Calculator/Multiply?n1=${n1}&n2=${n2}`
    );
    const data = await response.json();
    return data.result;
  }
}

// Usage
const calc = new CalculatorClient('http://localhost:8080/api');
const sum = await calc.add(10, 20);
```

### 17.4.4. Python Client

```python
import requests

class CalculatorClient:
    def __init__(self, base_url: str):
        self.base_url = base_url.rstrip('/')

    def add(self, n1: int, n2: int) -> int:
        response = requests.get(
            f"{self.base_url}/Calculator/Add",
            params={"n1": n1, "n2": n2}
        )
        return response.json()["result"]

    def multiply(self, n1: int, n2: int) -> int:
        response = requests.get(
            f"{self.base_url}/Calculator/Multiply",
            params={"n1": n1, "n2": n2}
        )
        return response.json()["result"]

# Usage
calc = CalculatorClient("http://localhost:8080/api")
print(calc.add(10, 20))  # 30
```

### 17.4.5. C# Client

```csharp
using System.Net.Http.Json;

public interface ICalculator
{
    Task<int> Add(int n1, int n2);
    Task<long> Multiply(long n1, long n2);
}

public class CalculatorClient : ICalculator
{
    private readonly HttpClient _client;
    private readonly string _baseUrl;

    public CalculatorClient(string baseUrl)
    {
        _client = new HttpClient();
        _baseUrl = baseUrl.TrimEnd('/');
    }

    public async Task<int> Add(int n1, int n2)
    {
        var response = await _client.GetFromJsonAsync<ResultWrapper<int>>(
            $"{_baseUrl}/Calculator/Add?n1={n1}&n2={n2}");
        return response.Result;
    }

    public async Task<long> Multiply(long n1, long n2)
    {
        var response = await _client.GetFromJsonAsync<ResultWrapper<long>>(
            $"{_baseUrl}/Calculator/Multiply?n1={n1}&n2={n2}");
        return response.Result;
    }

    private record ResultWrapper<T>(T Result);
}
```

---

## 17.5. OpenAPI/Swagger Integration

### 17.5.1. Generating OpenAPI Specification

Create a Mustache template for OpenAPI 3.0:

```mustache
{
  "openapi": "3.0.0",
  "info": {
    "title": "{{root}} API",
    "version": "1.0.0"
  },
  "servers": [
    {"url": "http://localhost:{{port}}/{{root}}"}
  ],
  "paths": {
{{#services}}
{{#methods}}
    "/{{../serviceName}}/{{methodName}}": {
      "get": {
        "operationId": "{{../serviceName}}_{{methodName}}",
        "parameters": [
{{#args}}
          {
            "name": "{{argName}}",
            "in": "query",
            "schema": {"type": "{{openApiType}}"}
          }{{^last}},{{/last}}
{{/args}}
        ],
        "responses": {
          "200": {
            "description": "Success",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "properties": {
                    "result": {"type": "{{openApiResultType}}"}
                  }
                }
              }
            }
          }
        }
      }
    }{{^last}},{{/last}}
{{/methods}}
{{/services}}
  }
}
```

### 17.5.2. Using with Swagger UI

Once you have the OpenAPI spec, integrate with Swagger UI:

```pascal
// Add static file serving for Swagger UI
procedure TMyServer.SwaggerUI(Ctxt: TRestServerUriContext);
begin
  Ctxt.ReturnFileFromFolder('./swagger-ui/', True, 'index.html');
end;
```

---

## 17.6. WebSocket Clients

### 17.6.1. JavaScript WebSocket Client

For bidirectional communication:

```javascript
class MorMotWebSocket {
  constructor(url) {
    this.ws = new WebSocket(url);
    this.callbacks = new Map();
    this.callId = 0;

    this.ws.onmessage = (event) => {
      const response = JSON.parse(event.data);
      const callback = this.callbacks.get(response.id);
      if (callback) {
        callback(response.result);
        this.callbacks.delete(response.id);
      }
    };
  }

  call(service, method, params) {
    return new Promise((resolve) => {
      const id = ++this.callId;
      this.callbacks.set(id, resolve);
      this.ws.send(JSON.stringify({
        id,
        method: `${service}.${method}`,
        params
      }));
    });
  }
}

// Usage
const ws = new MorMotWebSocket('ws://localhost:8080/api');
const result = await ws.call('Calculator', 'Add', [10, 20]);
```

### 17.6.2. Handling Callbacks

For server-to-client callbacks:

```javascript
ws.onmessage = (event) => {
  const msg = JSON.parse(event.data);

  if (msg.callback) {
    // Server-initiated callback
    handleCallback(msg.callback, msg.params);
  } else if (msg.id) {
    // Response to our request
    resolveRequest(msg.id, msg.result);
  }
};

function handleCallback(name, params) {
  switch (name) {
    case 'IProgress.Update':
      updateProgressBar(params.percent);
      break;
    case 'IProgress.Completed':
      showComplete(params.success);
      break;
  }
}
```

---

## 17.7. Mobile Considerations

### 17.7.1. Network Connectivity

Mobile apps must handle:
- Intermittent connectivity
- High latency
- Battery constraints

```pascal
// Retry logic with exponential backoff
function CallWithRetry(Client: TRestHttpClientSocket;
  const Method: RawUtf8; MaxRetries: Integer = 3): RawJson;
var
  Attempt: Integer;
  Delay: Integer;
begin
  Delay := 100; // Initial delay in ms
  for Attempt := 1 to MaxRetries do
  try
    Result := Client.CallBackGetResult(Method, []);
    Exit;
  except
    on E: Exception do
    begin
      if Attempt = MaxRetries then
        raise;
      Sleep(Delay);
      Delay := Delay * 2; // Exponential backoff
    end;
  end;
end;
```

### 17.7.2. Data Caching

For offline support:

```pascal
type
  TCachedClient = class
  private
    fClient: TRestHttpClientSocket;
    fCache: TDocVariantData;
  public
    function GetData(const Key: RawUtf8): Variant;
  end;

function TCachedClient.GetData(const Key: RawUtf8): Variant;
begin
  // Try cache first
  if fCache.GetValueByPath(Key, Result) then
    Exit;

  // Fetch from server
  try
    Result := fClient.CallBackGetResult('GetData', ['key', Key]);
    fCache.AddValue(Key, Result);
  except
    // Return cached data even if stale
    Result := fCache.Value[Key];
  end;
end;
```

### 17.7.3. Delphi FireMonkey

mORMot works with FireMonkey for iOS/Android:

```pascal
uses
  mormot.rest.http.client,
  FMX.Forms;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  // Same code works on mobile
  fClient := TRestHttpClientSocket.Create(
    EditServer.Text,
    EditPort.Text,
    fModel
  );
  fClient.ServiceDefine([IMyService], sicShared);
end;
```

**Note**: Ensure you're using `TRestHttpClientSocket` or `TRestHttpClientCurl` on mobile, not Windows-specific classes.

---

## 17.8. Best Practices

### 17.8.1. API Versioning

Include version in your API root:

```pascal
// Server
Server := TRestServerDB.Create(Model, 'data.db3');
Server.Model.Root := 'api/v1';

// Client
Client := TRestHttpClientSocket.Create('server', '8080',
  TOrmModel.Create([], 'api/v1'));
```

### 17.8.2. Error Handling

Standardize error responses:

```json
{
  "errorCode": 400,
  "errorText": "Invalid parameter: n1 must be positive"
}
```

Handle on client:
```typescript
async function callService(method: string, params: any): Promise<any> {
  const response = await fetch(`${baseUrl}/${method}?${new URLSearchParams(params)}`);
  const data = await response.json();

  if (data.errorCode) {
    throw new Error(`${data.errorCode}: ${data.errorText}`);
  }

  return data.result;
}
```

### 17.8.3. Security

1. **Always use HTTPS** in production
2. **Validate input** on server side
3. **Use authentication** for sensitive operations
4. **Rate limiting** to prevent abuse
5. **CORS headers** for browser clients:

```pascal
HttpServer.AccessControlAllowOrigin := '*'; // Or specific origins
```

---

## Summary

mORMot 2's cross-platform capabilities:

- **Native support**: Same code compiles on Windows, Linux, macOS, Android
- **Code generation**: Mustache-based templates for any target language
- **Standard protocols**: HTTP/JSON for universal compatibility
- **OpenAPI integration**: Generate documentation and client SDKs
- **WebSocket support**: Bidirectional communication across platforms
- **Mobile-ready**: Works with Delphi FireMonkey and FPC

The framework's clean REST/JSON protocol means any HTTP client can consume mORMot services, while native Pascal clients get the full type-safe experience with automatic stub generation.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 16: Client-Server Services via Interfaces](mORMot2-SAD-Chapter-16.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 18: The MVC Pattern](mORMot2-SAD-Chapter-18.md) |
