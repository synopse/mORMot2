# CORS Middleware - DMVC vs mORMot2 Comparison

This document provides a detailed comparison between the DMVC and mORMot2 implementations of CORS middleware.

## Architecture Comparison

### DMVC Architecture

```
middleware_cors/
├── middleware_cors.dpr           # Main server (port 8080)
├── WebModuleU.pas/.dfm           # Web module with CORS middleware
├── MainControllerU.pas           # Controller with POST /api/customers
└── SimpleWebServer/              # Simple HTTP server for HTML (port 9090)
    ├── SimpleWebServer.dpr
    └── WebModuleU.pas
```

### mORMot2 Architecture

```
13-middleware_cors/
├── MiddlewareCors.dpr            # Main program with CORS modes
├── src/
│   ├── entities.pas              # TOrmCustomer entity
│   ├── api.interfaces.pas        # ICustomerApi interface
│   ├── api.impl.pas              # TCustomerApi implementation
│   └── server.pas                # Server with CORS configuration
└── www/
    └── index.html                # HTML test page (serve on port 9090)
```

## Code Comparison

### 1. CORS Configuration

#### DMVC - WebModuleU.pas
```pascal
procedure TMyWebModule.WebModuleCreate(Sender: TObject);
begin
  FMVC := TMVCEngine.Create(Self, ...);
  FMVC.AddController(TMyController);

  // Allow all origins -> *
  FMVC.AddMiddleware(TMVCCORSMiddleware.Create);

  // Or allow 2 origins
  // FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com,http://localhost:9090'));

  // Or disallow localhost:9090
  // FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com'));
end;
```

#### mORMot2 - server.pas
```pascal
constructor TCorsSampleServer.Create(...; aCorsMode: TCorsMode);
begin
  fRestServer := TRestServerDB.CreateSqlite3([TOrmCustomer], aDbFileName);
  fCustomerApi := TCustomerApi.Create(fRestServer.Orm);
  fRestServer.ServiceDefine(fCustomerApi, [ICustomerApi], sicShared);

  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer], '+', HTTP_DEFAULT_MODE, 32);

  // Configure CORS based on mode
  case aCorsMode of
    cmAllowAll:
      fHttpServer.AccessControlAllowOrigin := '*';
    cmSpecificOrigins:
      fHttpServer.AccessControlAllowOrigin := 'https://anotherserver.com,http://localhost:9090';
    cmRestrictive:
      fHttpServer.AccessControlAllowOrigin := 'https://anotherserver.com';
  end;

  // Additional CORS headers
  fHttpServer.AccessControlAllowMethods := 'GET,POST,PUT,DELETE,OPTIONS';
  fHttpServer.AccessControlAllowHeaders := 'Content-Type,Authorization,X-Requested-With';
  fHttpServer.AccessControlExposeHeaders := 'Content-Type,Content-Length';
  fHttpServer.AccessControlMaxAge := 86400; // 24 hours
end;
```

**Key Differences:**
- DMVC: Middleware class pattern
- mORMot2: Direct HTTP server properties
- DMVC: Configuration via constructor
- mORMot2: Configuration via properties + more granular control

---

### 2. API Endpoint

#### DMVC - MainControllerU.pas
```pascal
type
  [MVCPath('/api')]
  TMyController = class(TMVCController)
  public
    [MVCPath('/customers')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCustomer(const [MVCFromBody] Dict: TMVCStringDictionary);
  end;

implementation

procedure TMyController.CreateCustomer(const [MVCFromBody] Dict: TMVCStringDictionary);
begin
  Render(
    ObjectDict().Add('data', StrDict.Add('message', Dict['hello']))
  )
end;
```

**Route:** `POST /api/customers`
**Request Body:** `{"hello":"world"}`
**Response:** `{"data":{"message":"world"}}`

#### mORMot2 - api.interfaces.pas + api.impl.pas
```pascal
// Interface
type
  ICustomerApi = interface(IInvokable)
    ['{8B3F9A2E-1C4D-4E5F-9A2B-3C4D5E6F7A8B}']
    function CreateCustomer(const hello: RawUtf8): RawJson;
  end;

// Implementation
function TCustomerApi.CreateCustomer(const hello: RawUtf8): RawJson;
var
  customer: TOrmCustomer;
  response: RawUtf8;
begin
  customer := TOrmCustomer.Create;
  try
    customer.Name := 'Demo Customer';
    customer.Email := FormatUtf8('demo@%.com', [hello]);

    if fOrm.Add(customer, true) <> 0 then
    begin
      response := FormatUtf8('{"data":{"message":"Customer created with message: %"}}', [hello]);
      Result := RawJson(response);
    end
    else
      Result := RawJson('{"error":"Failed to create customer"}');
  finally
    customer.Free;
  end;
end;
```

**Route:** `POST /CustomerApi/CreateCustomer`
**Request Body:** `{"hello":"world"}`
**Response:** `{"data":{"message":"Customer created with message: world"}}`

**Key Differences:**
- DMVC: RESTful path `/api/customers`, method attributes
- mORMot2: JSON-RPC style `/CustomerApi/CreateCustomer`, interface-based
- DMVC: `Render()` for automatic serialization
- mORMot2: Manual JSON formatting with `FormatUtf8()` or return DTO
- DMVC: Dictionary parameter `TMVCStringDictionary`
- mORMot2: Typed parameter `RawUtf8`

---

### 3. HTML Test Client

Both implementations use very similar HTML/JavaScript:

#### DMVC - bin/www/index.html
```javascript
document.getElementById('btnCORS').onclick = () => {
    fetch('http://localhost:8080/api/customers', {
        "method": 'POST',
        "body": JSON.stringify({ "hello": "world" })
    })
        .then((res) => res.json())
        .then((json) => {
            output.innerHTML = JSON.stringify(json);
        });
};
```

#### mORMot2 - www/index.html
```javascript
document.getElementById('btnCORS').onclick = () => {
    fetch('http://localhost:8080/CustomerApi/CreateCustomer', {
        "method": 'POST',
        "headers": {
            "Content-Type": "application/json"
        },
        "body": JSON.stringify({ "hello": "world" })
    })
        .then((res) => res.json())
        .then((json) => {
            logOutput('Success! Response data:');
            logOutput(JSON.stringify(json, null, 2));
        })
        .catch((err) => {
            logOutput('CORS Error: ' + err.message, true);
        });
};
```

**Key Differences:**
- DMVC: Path `/api/customers`
- mORMot2: Path `/CustomerApi/CreateCustomer`
- mORMot2: Explicit `Content-Type: application/json` header
- mORMot2: Better error handling and logging

---

### 4. CORS Headers in Response

Both frameworks send similar CORS headers:

#### DMVC Response Headers
```
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
Access-Control-Allow-Headers: Content-Type,Authorization
Access-Control-Max-Age: 86400
```

#### mORMot2 Response Headers
```
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
Access-Control-Allow-Headers: Content-Type,Authorization,X-Requested-With
Access-Control-Expose-Headers: Content-Type,Content-Length
Access-Control-Max-Age: 86400
```

**Differences:**
- mORMot2 includes `X-Requested-With` in allowed headers
- mORMot2 explicitly exposes `Content-Type` and `Content-Length`

---

### 5. Preflight Request Handling

Both frameworks automatically handle OPTIONS (preflight) requests.

#### DMVC
```
OPTIONS /api/customers HTTP/1.1
Origin: http://localhost:9090

→ 200 OK
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
```

#### mORMot2
```
OPTIONS /CustomerApi/CreateCustomer HTTP/1.1
Origin: http://localhost:9090

→ 200 OK
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
Access-Control-Max-Age: 86400
```

**No significant differences** - both handle preflight automatically.

---

## Feature Comparison Table

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **CORS Configuration** | Middleware class | HTTP server properties |
| **Setup Complexity** | Medium (middleware pattern) | Low (direct properties) |
| **Allow All Origins** | `TMVCCORSMiddleware.Create` | `AccessControlAllowOrigin := '*'` |
| **Specific Origins** | Constructor parameter | Property assignment |
| **Methods Control** | Via middleware | `AccessControlAllowMethods` |
| **Headers Control** | Via middleware | `AccessControlAllowHeaders` |
| **Expose Headers** | Via middleware | `AccessControlExposeHeaders` |
| **Preflight Caching** | Automatic | `AccessControlMaxAge` property |
| **Custom Logic** | Override middleware methods | `OnBeforeBody` callback |
| **Routing Style** | RESTful (`/api/customers`) | JSON-RPC (`/CustomerApi/CreateCustomer`) |
| **Parameter Binding** | Automatic via attributes | Automatic via interface |
| **Response Format** | `Render()` helper | Manual or DTO serialization |

---

## Advantages

### DMVC Advantages
1. **Middleware Pattern**: More familiar to web developers
2. **RESTful Routes**: Standard REST API patterns (`/api/resource`)
3. **Automatic Serialization**: `Render()` handles everything
4. **Rich Ecosystem**: Many built-in middlewares

### mORMot2 Advantages
1. **Built-in CORS**: No middleware needed, less code
2. **Type Safety**: Interface-based services with compile-time checking
3. **Performance**: Interface calls are optimized
4. **Fine-Grained Control**: More CORS properties available
5. **Simpler Setup**: Direct property assignment
6. **Flexibility**: Can use method-based or interface-based approach

---

## Migration Path

If migrating from DMVC CORS to mORMot2:

1. **Remove middleware:**
   ```pascal
   // DMVC
   FMVC.AddMiddleware(TMVCCORSMiddleware.Create('...'));
   ```

2. **Add properties:**
   ```pascal
   // mORMot2
   HttpServer.AccessControlAllowOrigin := '...';
   HttpServer.AccessControlAllowMethods := '...';
   ```

3. **Update routes:**
   - DMVC: `POST /api/customers`
   - mORMot2: `POST /CustomerApi/CreateCustomer`

4. **Update client code:**
   Change API endpoint URLs in JavaScript/frontend code.

---

## Conclusion

Both frameworks provide excellent CORS support:

- **DMVC**: Best for developers wanting explicit middleware pattern and RESTful APIs
- **mORMot2**: Best for developers wanting built-in support and interface-based services

The mORMot2 approach is **simpler** (fewer classes, direct properties) while DMVC is more **conventional** (middleware pattern familiar from other frameworks).

For most use cases, mORMot2's built-in CORS support is sufficient and easier to maintain.

---

**See Also:**
- [CONVERSION-GUIDE.md](../CONVERSION-GUIDE.md) - Complete DMVC to mORMot2 guide
- [README.md](README.md) - This sample's documentation
