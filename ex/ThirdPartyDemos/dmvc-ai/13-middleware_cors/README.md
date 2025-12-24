# CORS Middleware Sample - mORMot2

Port of DMVCFramework `middleware_cors` sample to mORMot2.

## Overview

This sample demonstrates how to configure **CORS (Cross-Origin Resource Sharing)** in mORMot2, which is equivalent to DMVCFramework's `TMVCCORSMiddleware`.

### What is CORS?

CORS is a security mechanism that allows web pages from one origin (domain/port) to access resources from a different origin. Without CORS, browsers block such cross-origin requests.

## DMVC vs mORMot2

### DMVC (Original)
```pascal
// Allow all origins
FMVC.AddMiddleware(TMVCCORSMiddleware.Create);

// Allow specific origins
FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com,http://localhost:9090'));

// Disallow localhost:9090
FMVC.AddMiddleware(TMVCCORSMiddleware.Create('https://anotherserver.com'));
```

### mORMot2 (This Sample)
```pascal
// Allow all origins
HttpServer.AccessControlAllowOrigin := '*';

// Allow specific origins
HttpServer.AccessControlAllowOrigin := 'https://anotherserver.com,http://localhost:9090';

// Disallow localhost:9090
HttpServer.AccessControlAllowOrigin := 'https://anotherserver.com';
```

## Features Demonstrated

1. **Allow All Origins** (`cmAllowAll` mode)
   - Accepts requests from any origin
   - `Access-Control-Allow-Origin: *`

2. **Specific Origins** (`cmSpecificOrigins` mode)
   - Accepts requests from listed origins only
   - Example: `https://anotherserver.com,http://localhost:9090`

3. **Restrictive Mode** (`cmRestrictive` mode)
   - Accepts requests from a single origin
   - Blocks all other origins

4. **CORS Headers Configuration**
   - `Access-Control-Allow-Methods`: GET, POST, PUT, DELETE, OPTIONS
   - `Access-Control-Allow-Headers`: Content-Type, Authorization
   - `Access-Control-Expose-Headers`: Content-Type, Content-Length
   - `Access-Control-Max-Age`: 86400 seconds (24 hours)

5. **Preflight Requests**
   - Automatic handling of OPTIONS requests
   - Browser sends OPTIONS before actual request

## Project Structure

```
13-middleware_cors/
├── src/
│   ├── entities.pas          # TOrmCustomer entity
│   ├── api.interfaces.pas    # ICustomerApi interface + DTOs
│   ├── api.impl.pas          # TCustomerApi implementation
│   └── server.pas            # Server with CORS configuration
├── www/
│   └── index.html            # HTML test page for CORS
├── MiddlewareCors.dpr        # Main program
├── MiddlewareCors.dproj      # Delphi project (D12)
└── README.md                 # This file
```

## Building and Running

### 1. Compile the Server

```bash
# Using Delphi IDE
# Open MiddlewareCors.dproj and press F9

# Or using command line
cd /mnt/w/mORMot2/ex/dmvc/13-middleware_cors
dcc32 MiddlewareCors.dpr
```

### 2. Run the Server

```bash
# Allow all origins (default)
./MiddlewareCors

# Or explicitly specify mode
./MiddlewareCors all

# Allow specific origins
./MiddlewareCors specific

# Restrictive mode (blocks localhost:9090)
./MiddlewareCors restrict
```

The server will start on **http://localhost:8080**

### 3. Test CORS from Browser

#### Option A: Using the HTML Test Page

1. **Serve the HTML page from a different port:**
   ```bash
   cd www
   python -m http.server 9090
   ```

2. **Open in browser:**
   ```
   http://localhost:9090
   ```

3. **Click the CORS test buttons:**
   - **POST with CORS**: Should work if server allows `http://localhost:9090`
   - **POST with No-CORS**: Always works but response is opaque
   - **GET all customers**: Retrieves all customers from database

#### Option B: Using curl

```bash
# Create a customer (simulates CORS request)
curl -X POST http://localhost:8080/CustomerApi/CreateCustomer \
  -H "Content-Type: application/json" \
  -H "Origin: http://localhost:9090" \
  -d '{"hello":"world"}' \
  -v

# Check CORS headers in response
# Look for: Access-Control-Allow-Origin

# Test preflight (OPTIONS)
curl -X OPTIONS http://localhost:8080/CustomerApi/CreateCustomer \
  -H "Origin: http://localhost:9090" \
  -H "Access-Control-Request-Method: POST" \
  -H "Access-Control-Request-Headers: Content-Type" \
  -v
```

## Testing Different CORS Modes

### Mode 1: Allow All (`./MiddlewareCors all`)

**Expected:**
- ✅ Requests from `http://localhost:9090` → **Allowed**
- ✅ Requests from `https://anotherserver.com` → **Allowed**
- ✅ Requests from any origin → **Allowed**

**Response Header:**
```
Access-Control-Allow-Origin: *
```

### Mode 2: Specific Origins (`./MiddlewareCors specific`)

**Expected:**
- ✅ Requests from `http://localhost:9090` → **Allowed**
- ✅ Requests from `https://anotherserver.com` → **Allowed**
- ❌ Requests from other origins → **Blocked**

**Response Header:**
```
Access-Control-Allow-Origin: https://anotherserver.com,http://localhost:9090
```

### Mode 3: Restrictive (`./MiddlewareCors restrict`)

**Expected:**
- ❌ Requests from `http://localhost:9090` → **Blocked**
- ✅ Requests from `https://anotherserver.com` → **Allowed**
- ❌ Requests from other origins → **Blocked**

**Response Header:**
```
Access-Control-Allow-Origin: https://anotherserver.com
```

## API Endpoints

All endpoints use **POST** (mORMot2 interface-based services):

| Endpoint | Description |
|----------|-------------|
| `POST /CustomerApi/CreateCustomer` | Create customer with `{"hello":"world"}` |
| `POST /CustomerApi/GetCustomer` | Get customer by ID: `{"id":123}` |
| `POST /CustomerApi/GetAllCustomers` | Get all customers: `{}` |

## CORS Flow Diagram

```
Browser (localhost:9090)
    │
    ├─> OPTIONS /CustomerApi/CreateCustomer  (Preflight)
    │       Headers: Origin, Access-Control-Request-Method
    │
    ├─< 200 OK
    │       Access-Control-Allow-Origin: *
    │       Access-Control-Allow-Methods: GET,POST,PUT,DELETE,OPTIONS
    │       Access-Control-Max-Age: 86400
    │
    └─> POST /CustomerApi/CreateCustomer  (Actual request)
            Headers: Origin, Content-Type

        ├─< 200 OK
                Access-Control-Allow-Origin: *
                {"data":{"message":"Customer created..."}}
```

## Key Differences from DMVC

| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Setup** | Middleware class | HTTP server property |
| **Origins** | Constructor parameter | `AccessControlAllowOrigin` |
| **Methods** | Configurable | `AccessControlAllowMethods` |
| **Headers** | Configurable | `AccessControlAllowHeaders` |
| **Preflight** | Automatic | Automatic |
| **Custom Logic** | Middleware class | `OnBeforeBody` callback |

## Advanced Customization

For advanced CORS scenarios, use the `OnBeforeBody` callback:

```pascal
fHttpServer.OnBeforeBody := OnBeforeBody;

function TCorsSampleServer.OnBeforeBody(...): cardinal;
begin
  // Custom CORS logic here
  if aMethod = 'OPTIONS' then
  begin
    // Custom preflight handling
    Result := 200;
  end
  else
    Result := 0; // Continue normal processing
end;
```

## Learn More

- **DMVC Original Sample**: `/mnt/w/DMVCframework/samples/middleware_cors/`
- **CORS Spec**: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
- **mORMot2 CORS**: See `TRestHttpServer.AccessControlAllowOrigin` in docs
- **Conversion Guide**: `../CONVERSION-GUIDE.md` (Section 6: Middleware Equivalents)

## Troubleshooting

### Browser shows CORS error

**Problem:**
```
Access to fetch at 'http://localhost:8080/...' from origin 'http://localhost:9090'
has been blocked by CORS policy
```

**Solutions:**
1. Check server is running in correct CORS mode: `./MiddlewareCors all`
2. Verify HTML page is served from `http://localhost:9090` (not file://)
3. Check browser console for exact CORS error details

### Preflight request fails

**Problem:**
```
Response to preflight request doesn't pass access control check
```

**Solutions:**
1. Check `AccessControlAllowMethods` includes the HTTP method (POST, PUT, etc.)
2. Check `AccessControlAllowHeaders` includes required headers (Content-Type, etc.)
3. Use `curl -v` to inspect preflight OPTIONS response

### Response is opaque

**Problem:**
JavaScript can't read response even though request succeeds.

**Solution:**
This is expected with `mode: 'no-cors'`. Use normal fetch (without no-cors) after configuring CORS on server.

## License

Same as mORMot2 framework (MPL/GPL/LGPL triple license).

## See Also

- **08-basicauth**: Basic authentication example
- **09-custom_auth**: Custom authentication
- **10-jsonwebtoken**: JWT authentication with CORS
