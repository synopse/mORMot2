# Static Files Middleware Sample

**mORMot2 port of DMVC Framework middleware_staticfiles sample**

## Overview

This sample demonstrates how to serve static files (HTML, CSS, JS, images) in mORMot2 using a custom HTTP server implementation. It's the equivalent of DelphiMVCFramework's `TMVCStaticFilesMiddleware`.

## Features Demonstrated

### 1. Multiple Static File Paths
- **`/static`** → serves files from `www/` folder
- **`/static2`** → serves files from `www2/` folder
- **`/static3`** → serves files from `www3/` folder with custom filters

### 2. Static File Features
- ✅ Default file serving (index.html)
- ✅ MIME type detection (HTML, CSS, JS, images, etc.)
- ✅ Custom MIME types (e.g., `.xpi` for Firefox extensions)
- ✅ **HTTP 304 Not Modified** - Automatic conditional GET support via `If-Modified-Since` header
- ✅ **ETag support** - Automatic entity tag generation for cache validation
- ✅ Cache headers (Cache-Control: max-age=3600)
- ✅ Charset specification
- ✅ Security: Directory traversal prevention (`..` in paths)

### 3. Custom File Filters
The `/static3` path demonstrates custom filtering:
- Block certain file types (e.g., `.txt` files)
- URL rewriting (redirect `file1.html` → `file2.html`)
- Custom access control logic

### 4. API Endpoints
- `/api` - Returns welcome message
- `/` - Redirects to `/static`

## Architecture

### Key Components

#### TStaticFilesHttpServer
Custom HTTP server class that extends `TRestHttpServer`:
- Overrides `Request()` method to intercept GET requests
- Maintains array of static file path mappings
- Handles MIME type detection and file serving

#### Static Path Configuration
```pascal
fHttpServer.AddStaticPath(
  '/static',              // URL path
  'www/',                 // File system path
  'index.html',           // Default file
  False,                  // Allow directory listing
  'UTF-8',                // Charset
  CustomFilterProc,       // Optional filter callback
  ['.xpi'],               // Custom MIME extensions
  ['application/x-xpinstall']  // Custom MIME types
);
```

#### Custom Filter Callback
```pascal
procedure(var PathInfo: string; var Allow: Boolean)
begin
  // Block .txt files
  Allow := not PathInfo.EndsWith('.txt', True);

  // Rewrite URLs
  if Allow and PathInfo.Contains('file1.html') then
    PathInfo := PathInfo.Replace('file1.html', 'file2.html');
end
```

## DMVC vs mORMot2 Comparison

### DMVC Framework
```pascal
// Add middleware to engine
FMVC.AddMiddleware(TMVCStaticFilesMiddleware.Create(
  '/static',
  TPath.Combine(ExtractFilePath(GetModuleName(HInstance)), 'www'),
  'index.html',
  True,
  'UTF-8',
  FilterCallback,
  MimeTypesCallback
));
```

### mORMot2 Equivalent
```pascal
// Custom HTTP server with static file support
TStaticFilesHttpServer = class(TRestHttpServer)
protected
  function Request(Ctxt: THttpServerRequestAbstract): cardinal; override;
public
  procedure AddStaticPath(...);
end;

// Usage
fHttpServer := TStaticFilesHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
fHttpServer.AddStaticPath('/static', wwwPath, 'index.html', False, 'UTF-8', ...);
```

### Key Differences
| Feature | DMVC | mORMot2 |
|---------|------|---------|
| **Pattern** | Middleware chain | Custom HTTP server override |
| **Configuration** | Per-middleware constructor | AddStaticPath() method |
| **Integration** | Pluggable middleware | Server subclass |
| **Flexibility** | High (add/remove middleware) | Medium (compile-time inheritance) |

## File Structure

```
15-middleware_staticfiles/
├── StaticFilesSample.dpr        # Main program
├── StaticFilesSample.dproj      # Delphi project
├── src/
│   ├── server.pas               # HTTP server with static file support
│   ├── api.interfaces.pas       # Service interface definitions
│   └── api.impl.pas             # Service implementations
├── www/                         # Static files for /static
│   ├── index.html
│   ├── www1/
│   └── www2/
├── www2/                        # Static files for /static2
│   ├── index.html
│   └── nothiddenbymiddleware.html
└── www3/                        # Static files for /static3 (with filters)
    ├── file1.html               # Redirected to file2.html
    ├── file2.html
    └── file3.html
```

## Running the Sample

### Build
```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe StaticFilesSample.dproj
```

### Run
```bash
./Win32/Debug/StaticFilesSample.exe
```

### Test
```bash
# Test static file serving
curl http://localhost:8080/static/
curl http://localhost:8080/static2/
curl http://localhost:8080/static3/

# Test custom filter (file1.html → file2.html)
curl http://localhost:8080/static3/file1.html

# Test blocked .txt files (should return 404)
curl http://localhost:8080/static3/test.txt

# Test API endpoint
curl http://localhost:8080/api

# Test redirect
curl -L http://localhost:8080/
```

## Implementation Notes

### MIME Type Detection
Default MIME types supported:
- **HTML**: `.html`, `.htm`
- **Styles**: `.css`
- **Scripts**: `.js`
- **Images**: `.png`, `.jpg`, `.jpeg`, `.gif`, `.svg`, `.ico`
- **Documents**: `.pdf`, `.xml`, `.json`, `.txt`
- **Archives**: `.zip`
- **Custom**: User-defined via AddStaticPath()

### Cache Headers
All static files include cache headers:
```http
Cache-Control: public, max-age=3600
```

### Security
- **Path traversal prevention**: Requests containing `..` are rejected (404)
- **Custom filters**: Can implement additional security checks
- **File existence validation**: Non-existent files return 404

### Performance Considerations
1. **File serving**: Uses mORMot2's idiomatic `SetOutFile()` method for efficient streaming
2. **Conditional GET**: Automatic 304 Not Modified responses reduce bandwidth
3. **ETag validation**: Browser cache validation via entity tags
4. **Thread safety**: All request handling is thread-safe
5. **Async mode**: Uses `useHttpAsync` for optimal concurrency

### Idiomatic Static File API

This sample uses mORMot2's **idiomatic static file API** (`THttpServerRequestAbstract.SetOutFile`) rather than manual field assignment:

```pascal
// ✅ IDIOMATIC (what this sample uses)
result := Ctxt.SetOutFile(
  fileName,           // File path
  True,               // Handle304NotModified - automatic conditional GET support
  contentType,        // ContentType with charset
  3600                // CacheControlMaxAgeSec - 1 hour cache
);

// ❌ NOT IDIOMATIC (manual approach)
Ctxt.OutContent := StringToUtf8(fileName);
Ctxt.OutContentType := HTTP_RESP_STATICFILE;
Ctxt.OutCustomHeaders := FormatUtf8('Content-Type: %; charset=%'#13#10 +
  'Cache-Control: public, max-age=3600', [mimeType, Charset]);
```

**Benefits of SetOutFile():**
- ✅ Automatic 304 Not Modified support (checks `If-Modified-Since` header)
- ✅ Automatic ETag generation and validation
- ✅ Built-in Cache-Control header management
- ✅ Automatic MIME type detection (with override support)
- ✅ Returns proper HTTP status codes (200, 304, 404)
- ✅ Less error-prone, cleaner code
- ✅ Follows mORMot2 conventions

## Conversion Notes

### What Was Changed
1. **Middleware → Server Override**: DMVC's middleware pattern replaced with TRestHttpServer.Request() override
2. **WebContext → THttpServerRequestAbstract**: Context object differs
3. **Response Format**: DMVC uses Context.Response → mORMot2 uses Ctxt.OutContent/OutContentType
4. **Path Configuration**: DMVC relative paths → mORMot2 explicit Executable.ProgramFilePath

### What Stayed the Same
1. **URL routing**: `/static`, `/static2`, `/static3` paths preserved
2. **Filter logic**: Custom filter callback interface identical
3. **MIME types**: Same custom MIME type support (.xpi example)
4. **Security**: Directory traversal prevention maintained

## Learning Points

### For DMVC Developers
- mORMot2 doesn't have a middleware system like DMVC
- Static files require custom HTTP server implementation
- Response handling is more low-level (direct field access vs. context methods)

### For mORMot2 Developers
- Overriding `TRestHttpServer.Request()` enables custom routing
- `HTTP_RESP_STATICFILE` tells mORMot2 to serve file from OutContent path
- Must call `inherited Request()` to allow REST API routing

## Related Samples

- **04-renders** - Response rendering patterns
- **03-routing** - Custom routing logic
- **11-ssl_server** - Secure file serving with SSL

## References

- **Original DMVC Sample**: `/mnt/w/DMVCframework/samples/middleware_staticfiles/`
- **mORMot2 Static Example**: `/mnt/w/mORMot2/ex/mORMot1/04-http-client-server/`
- **Conversion Guide**: `/mnt/w/mORMot2/ex/dmvc/CONVERSION-GUIDE.md`
