# Getting Started with mORMot2 DMVC Samples

Step-by-step guide for newcomers to mORMot2 REST API development.

## Prerequisites

Before you begin, ensure you have:

- ✅ **Delphi 11+** or **Free Pascal 3.2+** installed
- ✅ **mORMot2** source code (available in this repository)
- ✅ **Git** (optional, for cloning repository)
- ✅ **curl** or **Postman** (for testing APIs)
- ✅ **Web browser** (for HTML-based samples)

### Windows Setup

If using WSL (Windows Subsystem for Linux):

```bash
# Verify compiler tool is available
ls -l /mnt/w/Agentic-Coding/Tools/delphi-compiler.exe

# Should show the executable
```

## Your First Sample (5 Minutes)

### Step 1: Choose a Sample

Start with the simplest sample: **01-basicdemo_server**

```bash
cd /mnt/w/mORMot2/ex/dmvc/01-basicdemo_server
```

### Step 2: Read the Documentation

```bash
cat README.md
# or open in your editor
```

Key information:
- What the sample demonstrates
- API endpoints available
- How to test

### Step 3: Compile

Using the Delphi compiler tool:

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\01-basicdemo_server\01-basicdemo_server.dproj
```

Expected output:
```
Compiling 01-basicdemo_server.dpr...
[dcc32 Info] Build succeeded
  - 0 errors
  - 0 warnings
  - 0 hints
```

Output binary: `Win32/Release/01-basicdemo_server.exe`

### Step 4: Run the Server

```bash
./Win32/Release/01-basicdemo_server.exe
```

Expected output:
```
========================================
mORMot2 Basic Demo Server
========================================
Demonstrates:
  - Simple REST endpoints
  - JSON responses
  - Parameter handling
========================================

Server running on http://localhost:8080

Available endpoints:
  GET  /BasicDemoApi/HelloWorld
  POST /BasicDemoApi/HelloWorldPost
  GET  /BasicDemoApi/Divide?par1=10&par2=2

Press ENTER to stop the server
========================================
```

**Server is now running!** Leave this terminal open.

### Step 5: Test the API

Open a **new terminal** and test the endpoints:

#### Test 1: Hello World (GET)

```bash
curl http://localhost:8080/BasicDemoApi/HelloWorld
```

Expected response:
```json
{
  "message": "Hello World! It's 14:30:00 in the mORMot2 Land!",
  "time": "14:30:00"
}
```

#### Test 2: Division (GET with parameters)

```bash
curl "http://localhost:8080/BasicDemoApi/Divide?par1=10&par2=2"
```

Expected response:
```json
{
  "result": 5.0
}
```

#### Test 3: Echo POST

```bash
curl -X POST http://localhost:8080/BasicDemoApi/HelloWorldPost \
  -H "Content-Type: application/json" \
  -d '{"data":"{\"test\":\"value\"}"}'
```

Expected response:
```json
{
  "data": "{\"test\":\"value\"}",
  "modified": "from server"
}
```

### Step 6: Stop the Server

Go back to the server terminal and press **ENTER**.

🎉 **Congratulations!** You've successfully run your first mORMot2 REST API sample.

## Understanding the Code

Let's explore the basic demo structure:

```
01-basicdemo_server/
├── src/
│   ├── api.interfaces.pas    # Service interface definition
│   ├── api.impl.pas          # Service implementation
│   └── server.pas            # HTTP server setup
├── 01-basicdemo_server.dpr   # Main program
└── README.md                 # Documentation
```

### 1. Service Interface (api.interfaces.pas)

Defines the API contract:

```pascal
type
  // Response DTOs (Data Transfer Objects)
  THelloWorldResponse = packed record
    Message: RawUtf8;
    Time: RawUtf8;
  end;

  TDivideResponse = packed record
    Result: Double;
  end;

  // Service interface
  IBasicDemoApi = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789ABC}']

    // Simple GET endpoint
    function HelloWorld: THelloWorldResponse;

    // POST endpoint with parameter
    function HelloWorldPost(const data: RawUtf8): THelloWorldPostResponse;

    // GET with parameters
    function Divide(par1, par2: Double): TDivideResponse;
  end;
```

**Key points:**
- Interface inherits from `IInvokable` (required for mORMot2 services)
- Must have a unique GUID (generate with Ctrl+Shift+G in Delphi)
- Methods become API endpoints automatically
- Return values are serialized to JSON automatically

### 2. Service Implementation (api.impl.pas)

Implements the interface:

```pascal
type
  TBasicDemoApi = class(TInterfacedObject, IBasicDemoApi)
  public
    function HelloWorld: THelloWorldResponse;
    function HelloWorldPost(const data: RawUtf8): THelloWorldPostResponse;
    function Divide(par1, par2: Double): TDivideResponse;
  end;

implementation

function TBasicDemoApi.HelloWorld: THelloWorldResponse;
begin
  Result.Message := FormatUtf8('Hello World! It''s % in the mORMot2 Land!',
    [TimeToString(Now)]);
  Result.Time := TimeToString(Now);
end;

function TBasicDemoApi.Divide(par1, par2: Double): TDivideResponse;
begin
  if par2 = 0 then
    raise EDivByZero.Create('Division by zero');
  Result.Result := par1 / par2;
end;
```

**Key points:**
- Implements the interface methods
- Business logic goes here
- Exceptions are automatically converted to HTTP errors
- No manual JSON serialization needed!

### 3. Server Setup (server.pas)

Creates and configures the HTTP server:

```pascal
type
  TBasicDemoServer = class
  private
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
  end;

constructor TBasicDemoServer.Create(const aPort: RawUtf8);
begin
  // Create REST server (no database for this simple example)
  fRestServer := TRestServerDB.CreateSqlite3([], ':memory:');

  // Register the service interface
  fRestServer.ServiceDefine(TBasicDemoApi, [IBasicDemoApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    aPort,              // Port
    [fRestServer],      // REST servers
    '+',                // Domain (+ = all)
    useHttpAsync        // Server mode
  );
end;
```

**Key points:**
- `TRestServerDB` is the core REST server
- `ServiceDefine` registers interface-based services
- `TRestHttpServer` wraps REST server with HTTP protocol
- `useHttpAsync` = high-performance async I/O mode

### 4. Main Program (01-basicdemo_server.dpr)

Starts the server:

```pascal
program BasicDemoServer;

uses
  server,
  api.interfaces,
  api.impl;

var
  Server: TBasicDemoServer;
begin
  Server := TBasicDemoServer.Create('8080');
  try
    WriteLn('Server running on http://localhost:8080');
    WriteLn('Press ENTER to stop');
    ReadLn;
  finally
    Server.Free;
  end;
end.
```

## URL Structure

mORMot2 uses this URL pattern:

```
http://localhost:8080/ServiceName/MethodName?param1=value1&param2=value2
```

For our example:

```
http://localhost:8080/BasicDemoApi/HelloWorld
                       └─────┬─────┘ └────┬────┘
                          Interface     Method
                            name         name
```

**Important differences from DMVC:**

| DMVC | mORMot2 |
|------|---------|
| `/api/hello` | `/BasicDemoApi/HelloWorld` |
| Custom via `[MVCPath]` | Automatic from interface |
| `/user/123` (path param) | `/GetUser?id=123` (query param) |

## Next Steps

### Tutorial Samples (Recommended Order)

#### 1. Routing Patterns (03-routing)

```bash
cd /mnt/w/mORMot2/ex/dmvc/03-routing
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\03-routing\RoutingSample.dproj
./Win32/Release/RoutingSample.exe
```

Learn:
- Different HTTP methods (GET, POST, PUT, DELETE)
- URL parameters
- Query string parameters
- Path parameters mapping

#### 2. Different Response Types (04-renders)

```bash
cd /mnt/w/mORMot2/ex/dmvc/04-renders
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\04-renders\RendersSample.dproj
./Win32/Release/RendersSample.exe
```

Learn:
- JSON responses
- HTML rendering
- Binary data (file downloads)
- Custom content types

#### 3. CRUD Operations (06-articles_crud_server)

```bash
cd /mnt/w/mORMot2/ex/dmvc/06-articles_crud_server
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\06-articles_crud_server\06-articles_crud_server.dproj
./Win32/Release/06-articles_crud_server.exe
```

Learn:
- Database integration with ORM
- Create, Read, Update, Delete operations
- Data validation
- Client-server architecture

#### 4. Authentication (08-basicauth)

```bash
cd /mnt/w/mORMot2/ex/dmvc/08-basicauth
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\08-basicauth\08-basicauth.dproj
./Win32/Release/08-basicauth.exe
```

Learn:
- HTTP Basic Authentication
- Role-based authorization
- Protected endpoints
- Authentication filters

## Common Tasks

### Task 1: Create a New Endpoint

**Scenario**: Add a "Goodbye" endpoint to the basic demo.

1. **Define in interface** (api.interfaces.pas):

```pascal
type
  IGoodbyeResponse = packed record
    Message: RawUtf8;
  end;

  IBasicDemoApi = interface(IInvokable)
    ['{...}']
    // ... existing methods ...
    function Goodbye: TGoodbyeResponse;  // NEW
  end;
```

2. **Implement** (api.impl.pas):

```pascal
function TBasicDemoApi.Goodbye: TGoodbyeResponse;
begin
  Result.Message := 'Goodbye from mORMot2!';
end;
```

3. **Recompile and test**:

```bash
/mnt/w/Agentic-Coding/Tools/delphi-compiler.exe W:\mORMot2\ex\dmvc\01-basicdemo_server\01-basicdemo_server.dproj
./Win32/Release/01-basicdemo_server.exe

# In another terminal:
curl http://localhost:8080/BasicDemoApi/Goodbye
```

### Task 2: Add a Parameter

**Scenario**: Greet a specific person.

```pascal
// Interface
type
  TGreetingResponse = packed record
    Message: RawUtf8;
  end;

  function Greet(const name: RawUtf8): TGreetingResponse;

// Implementation
function TBasicDemoApi.Greet(const name: RawUtf8): TGreetingResponse;
begin
  Result.Message := FormatUtf8('Hello, %!', [name]);
end;

// Test
curl "http://localhost:8080/BasicDemoApi/Greet?name=John"
```

### Task 3: Return Complex Data

**Scenario**: Return a list of items.

```pascal
// Interface
type
  TItem = packed record
    Id: Integer;
    Name: RawUtf8;
    Price: Double;
  end;
  TItemDynArray = array of TItem;

  function GetItems: TItemDynArray;

// Implementation
function TBasicDemoApi.GetItems: TItemDynArray;
begin
  SetLength(Result, 3);
  Result[0].Id := 1;
  Result[0].Name := 'Item 1';
  Result[0].Price := 10.50;

  Result[1].Id := 2;
  Result[1].Name := 'Item 2';
  Result[1].Price := 20.75;

  Result[2].Id := 3;
  Result[2].Name := 'Item 3';
  Result[2].Price := 30.00;
end;

// Test
curl http://localhost:8080/BasicDemoApi/GetItems
# Returns JSON array automatically
```

## Debugging Tips

### Enable Verbose Logging

Edit server.pas:

```pascal
uses
  mormot.core.log;

// In server constructor:
TSynLog.Family.Level := LOG_VERBOSE;  // Enable all log levels
TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;
```

Logs will be written to `*.log` files in the executable directory.

### Common Errors

#### Error: "Interface not registered"

**Cause**: Forgot to call `ServiceDefine`

**Fix**:
```pascal
fRestServer.ServiceDefine(TBasicDemoApi, [IBasicDemoApi], sicShared);
```

#### Error: "Port already in use"

**Cause**: Another instance is running or port 8080 is occupied

**Fix 1**: Stop the other instance
**Fix 2**: Change port in server creation:
```pascal
Server := TBasicDemoServer.Create('9090');  // Use port 9090
```

#### Error: "Method not found"

**Cause**: Typo in URL or method name mismatch

**Fix**: Check exact method name (case-insensitive but must match):
```bash
# Wrong: /BasicDemoApi/helloworld (missing caps)
# Right: /BasicDemoApi/HelloWorld
```

#### Error: JSON parsing failed

**Cause**: Invalid JSON in POST body

**Fix**: Validate JSON format:
```bash
# Wrong:
curl -d "not json" http://...

# Right:
curl -H "Content-Type: application/json" \
     -d '{"key":"value"}' http://...
```

## Testing Tools

### 1. curl (Command Line)

Best for quick API testing:

```bash
# GET request
curl http://localhost:8080/BasicDemoApi/HelloWorld

# POST request with JSON
curl -X POST http://localhost:8080/BasicDemoApi/HelloWorldPost \
  -H "Content-Type: application/json" \
  -d '{"data":"test"}'

# With authentication
curl -u username:password http://localhost:8080/...

# Save response to file
curl http://localhost:8080/... -o response.json

# Show HTTP headers
curl -i http://localhost:8080/...
```

### 2. Postman (GUI)

1. Download from https://www.postman.com/
2. Create new request
3. Set method (GET/POST/etc.)
4. Enter URL: `http://localhost:8080/BasicDemoApi/HelloWorld`
5. Click "Send"

### 3. Browser (for GET requests)

Simply navigate to: `http://localhost:8080/BasicDemoApi/HelloWorld`

Good for quick testing of GET endpoints.

### 4. HTTPie (User-Friendly CLI)

Install: `pip install httpie`

```bash
# GET request
http localhost:8080/BasicDemoApi/HelloWorld

# POST request
http POST localhost:8080/BasicDemoApi/HelloWorldPost data='test'

# Pretty JSON output (default)
```

## Best Practices

### 1. Use DTOs (Data Transfer Objects)

✅ **Good**: Type-safe records
```pascal
type
  TUserDto = packed record
    Id: Integer;
    Name: RawUtf8;
    Email: RawUtf8;
  end;

function GetUser(id: Integer): TUserDto;
```

❌ **Bad**: Untyped strings
```pascal
function GetUser(id: Integer): RawUtf8;  // Returns raw JSON string
```

### 2. Handle Exceptions

✅ **Good**: Meaningful error messages
```pascal
function Divide(a, b: Double): Double;
begin
  if b = 0 then
    raise EDivByZero.Create('Division by zero is not allowed');
  Result := a / b;
end;
```

❌ **Bad**: Let it crash
```pascal
function Divide(a, b: Double): Double;
begin
  Result := a / b;  // Crashes on division by zero
end;
```

### 3. Use Shared Service Instances

✅ **Good**: Efficient resource usage
```pascal
fRestServer.ServiceDefine(TMyService, [IMyService], sicShared);
```

❌ **Bad**: New instance per request (unless needed)
```pascal
fRestServer.ServiceDefine(TMyService, [IMyService], sicPerThread);
```

**Service Instance Contracts:**
- `sicShared` - One instance for all requests (best performance)
- `sicPerThread` - One instance per thread
- `sicPerSession` - One instance per user session
- `sicPerUser` - One instance per authenticated user

### 4. Keep Interfaces Clean

✅ **Good**: Focused, single-purpose interfaces
```pascal
IUserService = interface(IInvokable)
  function GetUser(id: Integer): TUserDto;
  function CreateUser(const user: TUserDto): Integer;
end;

IAuthService = interface(IInvokable)
  function Login(const username, password: RawUtf8): RawUtf8;
end;
```

❌ **Bad**: Giant "god" interfaces
```pascal
IEverything = interface(IInvokable)
  function GetUser(...): ...;
  function CreateUser(...): ...;
  function Login(...): ...;
  function SendEmail(...): ...;
  // 50+ methods...
end;
```

## Understanding mORMot2 Concepts

### What is IInvokable?

`IInvokable` is mORMot2's base interface for remote services. It:
- Enables JSON-RPC serialization
- Supports automatic parameter conversion
- Allows client proxy generation
- Works across process boundaries

### What is RawUtf8?

`RawUtf8` is mORMot2's optimized string type:
- UTF-8 encoded (efficient for JSON)
- Reference-counted (automatic memory management)
- Zero-copy operations when possible
- Use instead of `string` for better performance

### What is sicShared?

Service Instance Contract (sic) defines object lifetime:
- `sicShared` - Singleton (one instance)
- `sicPerThread` - Thread-local instance
- `sicPerSession` - Per HTTP session
- `sicPerUser` - Per authenticated user

## Getting Help

### Documentation

- **This Guide**: Complete beginner tutorial
- **Sample READMEs**: Each sample has detailed documentation
- **[CONVERSION-GUIDE.md](CONVERSION-GUIDE.md)**: DMVC → mORMot2 mapping
- **[ARCHITECTURE.md](ARCHITECTURE.md)**: Deep technical details
- **[mORMot2 SAD](https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html)**: Official documentation

### Community

- **Forum**: https://synopse.info/forum/
- **GitHub**: https://github.com/synopse/mORMot2/issues
- **Blog**: https://blog.synopse.info/

### Common Questions

**Q: Can I use regular `string` instead of `RawUtf8`?**
A: Yes, but `RawUtf8` is more efficient for REST APIs.

**Q: How do I enable HTTPS?**
A: See sample 11-ssl_server for complete SSL/TLS setup.

**Q: Can I use a database other than SQLite?**
A: Yes, mORMot2 supports PostgreSQL, MySQL, MSSQL, Oracle, etc.

**Q: How do I implement authentication?**
A: See samples 08 (Basic Auth), 09 (Custom Auth), or 10 (JWT).

**Q: Can I deploy on Linux?**
A: Yes, compile with Free Pascal (FPC) on Linux.

## Next Steps

Once you're comfortable with the basics:

1. **Explore Security** → Sample 08 (basicauth)
2. **Learn Database ORM** → Sample 06 (articles_crud_server)
3. **Add Middleware** → Sample 13 (middleware_cors)
4. **Real-time Features** → Sample 16 (serversentevents)
5. **Read Architecture Guide** → [ARCHITECTURE.md](ARCHITECTURE.md)

**Happy coding with mORMot2!** 🚀
