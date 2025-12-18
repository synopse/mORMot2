# 16. Client-Server Services via Interfaces

*Service-Oriented Architecture Made Simple*

In Chapter 14, we covered method-based services — a direct approach with full HTTP control. This chapter introduces **interface-based services**, mORMot's powerful SOA implementation that provides automatic client stub generation, contract validation, multiple instance lifetimes, and bidirectional communication via WebSockets.

---

## 16.1. Why Interface-Based Services?

Method-based services have limitations:
- Manual parameter marshalling on both ends
- No automatic client stub generation
- Flat service namespace
- Testing requires manual mocking
- No built-in session/workflow management
- Security checked manually per method
- No callback mechanism

Interface-based services solve these problems:

| Feature | Description |
|---------|-------------|
| **Design by Contract** | Interfaces define the service contract in pure Pascal |
| **Auto Marshalling** | JSON serialization handled automatically |
| **Factory Driven** | Get implementations from interfaces on both client and server |
| **Multiple Lifetimes** | Per-call, shared, per-session, per-user, per-group, client-driven |
| **Contract Validation** | Client/server compatibility verified before execution |
| **Bidirectional** | Callback interfaces for real-time notifications |
| **Secure** | Per-method authorization via user groups |
| **Cross-Platform** | Generated client code for Delphi, FPC, JavaScript |

---

## 16.2. Defining the Service Contract

### 16.2.1. Basic Interface

```pascal
type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    /// add two signed 32-bit integers
    function Add(n1, n2: Integer): Integer;
    /// multiply two 64-bit integers
    function Multiply(n1, n2: Int64): Int64;
  end;
```

Requirements:
- Must inherit from `IInvokable` (ensures RTTI)
- Must have a GUID (for identification)
- ASCII method names only (convention for services)
- `register` calling convention (default)

### 16.2.2. Supported Parameter Types

| Type | Serialization |
|------|---------------|
| `Boolean` | JSON `true`/`false` |
| `Integer`, `Cardinal`, `Int64`, `Double`, `Currency` | JSON number |
| Enumerations | JSON number (ordinal value) |
| Sets | JSON number (bitmask, up to 32 elements) |
| `TDateTime`, `TDateTimeMS` | ISO 8601 JSON string |
| `RawUtf8`, `string`, `UnicodeString` | JSON string (UTF-8) |
| `RawJson` | JSON passthrough (no escaping) |
| `RawByteString` | Base64-encoded JSON string |
| `TPersistent`, `TOrm` | JSON object (published properties) |
| `TObjectList` | JSON array with `"ClassName"` field |
| Dynamic arrays | JSON array |
| `record` | JSON object (with RTTI or custom serialization) |
| `variant`, `TDocVariant` | Native JSON |
| `TServiceCustomAnswer` | Custom response (binary, HTML, etc.) |
| `interface` | Callback for bidirectional communication |

### 16.2.3. Parameter Direction

```pascal
function Process(const Input: RawUtf8;     // Client → Server only
                 var InOut: Integer;        // Client ↔ Server (both ways)
                 out Output: RawUtf8        // Server → Client only
                ): Boolean;                 // Server → Client (result)
```

### 16.2.4. Complex Interface Example

```pascal
type
  IComplexService = interface(IInvokable)
    ['{8B5A2B10-7B3C-4A7D-95F3-8C9D7E6A5B4C}']
    // Simple types
    function Calculate(n1, n2: Double): Double;

    // Record parameters
    function ProcessOrder(const Order: TOrderRecord): TOrderResult;

    // Dynamic arrays
    function FilterItems(const Items: TRawUtf8DynArray;
                        const Filter: RawUtf8): TRawUtf8DynArray;

    // Object parameters (caller allocates)
    procedure TransformCustomer(var Customer: TCustomer);

    // Variant/TDocVariant for flexible JSON
    function QueryData(const Params: Variant): Variant;

    // Custom binary response
    function GetReport(ReportID: Integer): TServiceCustomAnswer;
  end;
```

### 16.2.5. TServiceCustomAnswer

For non-JSON responses (PDF, images, HTML):

```pascal
function TMyService.GetReport(ReportID: Integer): TServiceCustomAnswer;
begin
  Result.Header := HEADER_CONTENT_TYPE + 'application/pdf';
  Result.Content := GeneratePDF(ReportID);
  Result.Status := HTTP_SUCCESS;
end;
```

**Note**: Methods returning `TServiceCustomAnswer` cannot have `var` or `out` parameters.

---

## 16.3. Server-Side Implementation

### 16.3.1. Implementing the Contract

```pascal
type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1, n2: Integer): Integer;
    function Multiply(n1, n2: Int64): Int64;
  end;

function TServiceCalculator.Add(n1, n2: Integer): Integer;
begin
  Result := n1 + n2;
end;

function TServiceCalculator.Multiply(n1, n2: Int64): Int64;
begin
  Result := n1 * n2;
end;
```

### 16.3.2. Registering the Service

```pascal
// Using TypeInfo
Server.ServiceRegister(TServiceCalculator, [TypeInfo(ICalculator)], sicShared);

// Or using registered interface directly
Server.ServiceDefine(TServiceCalculator, [ICalculator], sicShared);
```

### 16.3.3. Instance Lifetime Modes

| Mode | Description | Thread Safety |
|------|-------------|---------------|
| `sicSingle` | New instance per call (default, safest) | Not required |
| `sicShared` | One instance for all calls (fastest) | **Required** |
| `sicClientDriven` | Instance lives until client releases interface | Not required |
| `sicPerSession` | One instance per authentication session | **Required** |
| `sicPerUser` | One instance per user across sessions | **Required** |
| `sicPerGroup` | One instance per user group | **Required** |
| `sicPerThread` | One instance per server thread | Not required |

#### Choosing the Right Mode

| Use Case | Recommended Mode |
|----------|------------------|
| Stateless operations, resource-intensive | `sicSingle` |
| Simple stateless service, high throughput | `sicShared` |
| Workflow with state between calls | `sicClientDriven` |
| Session-specific caching | `sicPerSession` |
| User preferences/settings | `sicPerUser` |
| Group-level configuration | `sicPerGroup` |
| Thread-local resources (e.g., database connection) | `sicPerThread` |

### 16.3.4. Client-Driven Example

```pascal
type
  IComplexNumber = interface(IInvokable)
    ['{29D753B2-E7EF-41B3-B7C3-827FEB082DC1}']
    procedure Assign(aReal, aImaginary: Double);
    function GetReal: Double;
    procedure SetReal(const Value: Double);
    function GetImaginary: Double;
    procedure SetImaginary(const Value: Double);
    procedure Add(aReal, aImaginary: Double);
    property Real: Double read GetReal write SetReal;
    property Imaginary: Double read GetImaginary write SetImaginary;
  end;

  TServiceComplexNumber = class(TInterfacedObject, IComplexNumber)
  private
    fReal, fImaginary: Double;
  public
    procedure Assign(aReal, aImaginary: Double);
    function GetReal: Double;
    procedure SetReal(const Value: Double);
    function GetImaginary: Double;
    procedure SetImaginary(const Value: Double);
    procedure Add(aReal, aImaginary: Double);
  end;

// Registration
Server.ServiceDefine(TServiceComplexNumber, [IComplexNumber], sicClientDriven);
```

The server maintains `fReal` and `fImaginary` between calls until the client releases the interface.

---

## 16.4. Accessing Execution Context

### 16.4.1. Using TInjectableObjectRest

The recommended approach — inherit from `TInjectableObjectRest`:

```pascal
type
  TMyService = class(TInjectableObjectRest, IMyService)
  public
    function GetCurrentUser: RawUtf8;
    procedure LogActivity(const Action: RawUtf8);
  end;

function TMyService.GetCurrentUser: RawUtf8;
begin
  if Server <> nil then
    Result := Server.SessionGetUser(Factory.CurrentSession).LogonName
  else
    Result := '';
end;

procedure TMyService.LogActivity(const Action: RawUtf8);
begin
  Server.Add(TOrmActivityLog, [
    'Action', Action,
    'User', GetCurrentUser,
    'Timestamp', NowUtc
  ]);
end;
```

Properties available:
- `Server: TRestServer` — Access to ORM and server methods
- `Factory: TServiceFactoryServer` — Service factory instance

### 16.4.2. Using ServiceRunningContext

For services not inheriting from `TInjectableObjectRest`:

```pascal
function TMyService.ProcessRequest: RawUtf8;
var
  Ctxt: PServiceRunningContext;
begin
  Ctxt := PerThreadRunningContextAddress;
  if Ctxt^.Request <> nil then
    Result := Ctxt^.Request.SessionUserName
  else
    Result := 'Unknown';
end;
```

**Note**: Prefer `TInjectableObjectRest` — it's safer and works outside client-server context.

---

## 16.5. Client-Side Usage

### 16.5.1. Registering the Interface

```pascal
// Must match server-side mode
Client.ServiceRegister([TypeInfo(ICalculator)], sicShared);

// Or with registered interface
Client.ServiceDefine([ICalculator], sicShared);
```

### 16.5.2. Resolving and Using Services

```pascal
var
  Calc: ICalculator;
begin
  if Client.Services.Resolve(ICalculator, Calc) then
    ShowMessage(IntToStr(Calc.Add(10, 20)));
end;
```

Generic syntax (Delphi 2010+):
```pascal
var
  Calc: ICalculator;
begin
  Calc := Client.Service<ICalculator>;
  if Calc <> nil then
    ShowMessage(IntToStr(Calc.Add(10, 20)));
end;
```

### 16.5.3. Client-Driven Services

```pascal
var
  CN: IComplexNumber;
begin
  if Client.Services.Resolve(IComplexNumber, CN) then
  begin
    CN.Assign(0.01, 3.14);
    CN.Add(100, 200);
    ShowMessage(Format('%.2f + %.2fi', [CN.Real, CN.Imaginary]));
  end;
end; // CN released here → server instance also released
```

### 16.5.4. Auto-Registration

For `sicClientDriven`, explicit registration is optional:
```pascal
// This works without prior ServiceRegister call
var
  CN: IComplexNumber;
begin
  Client.Services.Info(IComplexNumber).Get(CN);  // Auto-registers as sicClientDriven
end;
```

---

## 16.6. Contract Validation

### 16.6.1. Automatic Contract Hash

By default, mORMot generates an MD5 hash of the interface signature:
- Method names
- Parameter types and directions
- Return types

If client and server contracts don't match, connection fails with a clear error.

### 16.6.2. Custom Contract Strings

For explicit version control:

```pascal
// Server
Server.ServiceRegister(TMyService, [TypeInfo(IMyService)], sicShared)
  .SetOptions([], 'v2.5');  // Contract = 'v2.5'

// Client must match
Client.ServiceRegister([TypeInfo(IMyService)], sicShared, 'v2.5');
```

This allows:
- Semantic versioning
- Gradual client migration
- Clear compatibility rules

---

## 16.7. Authorization and Security

### 16.7.1. Per-Method Authorization

```pascal
var
  Factory: TServiceFactoryServer;
begin
  Factory := Server.Services.Info(ICalculator) as TServiceFactoryServer;

  // Deny all by default
  Factory.DenyAll;

  // Allow specific groups by ID
  Factory.Allow(ICalculator, [ADMIN_GROUP_ID]);

  // Allow specific methods for other groups by name
  // Note: AllowByName takes group names (RawUtf8), not IDs
  Factory.AllowByName(['Add', 'Multiply'], ['User', 'Guest']);
end;
```

### 16.7.2. Authentication Bypass

For public methods:

```pascal
Server.ServiceMethodByPassAuthentication('Calculator.GetVersion');
```

### 16.7.3. Execution Options

```pascal
Factory.SetOptions([optExecInMainThread]);  // Execute in main VCL thread
Factory.SetOptions([optFreeInMainThread]);  // Free instance in main thread
Factory.SetOptions([optExecInPerInterfaceThread]);  // Dedicated thread per interface
```

---

## 16.8. Service Logging

### 16.8.1. Enabling Logging

```pascal
Factory.SetServiceLog(Server, TOrmServiceLog);
```

This logs:
- Method name (`Interface.Method`)
- Input parameters (JSON)
- Output parameters (JSON)
- Execution time (microseconds)
- Session/User context

### 16.8.2. Custom Log Table

```pascal
type
  TOrmMyServiceLog = class(TOrmServiceLog)
  published
    property CustomField: RawUtf8 read fCustomField write fCustomField;
  end;

Factory.SetServiceLog(Server, TOrmMyServiceLog);
```

---

## 16.9. Bidirectional Communication (Callbacks)

### 16.9.1. Defining Callback Interfaces

```pascal
type
  // Callback interface (client implements this)
  IProgressCallback = interface(IInvokable)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Progress(Percent: Integer; const Status: RawUtf8);
    procedure Completed(Success: Boolean);
  end;

  // Service interface
  ILongRunningTask = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789012}']
    procedure StartTask(const TaskName: RawUtf8; const Callback: IProgressCallback);
    procedure CancelTask(const TaskID: RawUtf8);
  end;
```

### 16.9.2. Server Implementation

```pascal
type
  TLongRunningTask = class(TInjectableObjectRest, ILongRunningTask)
  public
    procedure StartTask(const TaskName: RawUtf8; const Callback: IProgressCallback);
    procedure CancelTask(const TaskID: RawUtf8);
  end;

procedure TLongRunningTask.StartTask(const TaskName: RawUtf8;
  const Callback: IProgressCallback);
begin
  // Start background work
  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
    begin
      for i := 0 to 100 do
      begin
        Sleep(100);
        Callback.Progress(i, Format('Processing %s...', [TaskName]));
      end;
      Callback.Completed(True);
    end
  ).Start;
end;
```

### 16.9.3. Client Implementation

```pascal
type
  TMyProgressCallback = class(TInterfacedCallback, IProgressCallback)
  private
    fForm: TForm;
  public
    constructor Create(aForm: TForm; aRest: TRestClientUri);
    procedure Progress(Percent: Integer; const Status: RawUtf8);
    procedure Completed(Success: Boolean);
  end;

constructor TMyProgressCallback.Create(aForm: TForm; aRest: TRestClientUri);
begin
  inherited Create(aRest, IProgressCallback);  // Register callback
  fForm := aForm;
end;

procedure TMyProgressCallback.Progress(Percent: Integer; const Status: RawUtf8);
begin
  TThread.Queue(nil,
    procedure
    begin
      fForm.ProgressBar.Position := Percent;
      fForm.StatusLabel.Caption := Status;
    end);
end;
```

### 16.9.4. Using WebSockets

Callbacks require WebSocket transport:

```pascal
// Server
HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);
HttpServer.WebSocketsEnable(Server, 'privatekey');

// Client (note: lowercase 's' in Websockets)
Client := TRestHttpClientWebsockets.Create('localhost', '8080', Model);
Client.WebSocketsConnect('privatekey');
Client.ServiceDefine([ILongRunningTask], sicShared);
```

---

## 16.10. Using Services on the Server

### 16.10.1. Resolving Services

```pascal
procedure TMyOtherService.DoSomething;
var
  Calc: ICalculator;
begin
  if Resolve(ICalculator, Calc) then
    Result := Calc.Add(10, 20);
end;
```

### 16.10.2. Generic Syntax

```pascal
procedure TMyOtherService.DoSomething;
var
  Calc: ICalculator;
begin
  Calc := Server.Service<ICalculator>;
  if Calc <> nil then
    Result := Calc.Add(10, 20);
end;
```

---

## 16.11. Client Code Generation

The `mormot.soa.codegen.pas` unit provides automatic client code generation from server-side service definitions.

### 16.11.1. Overview

Generate client stubs automatically for:
- **Delphi/FPC** — Native Pascal clients
- **JavaScript** — Browser and Node.js clients
- **TypeScript** — Type-safe JavaScript clients
- **Custom templates** — Any language via Mustache

### 16.11.2. Extracting Service Metadata

```pascal
uses
  mormot.soa.codegen;

var
  Context: TDocVariantData;
begin
  // Extract ORM and SOA metadata as JSON
  Context := ContextFromModel(Server.Model, Server.Services);

  // Context now contains:
  // - All TOrm classes with properties
  // - All registered interfaces with methods
  // - Parameter types and directions
  // - Service instance modes
end;
```

### 16.11.3. Generating Client Code

```pascal
var
  Output: RawUtf8;
begin
  // Generate using built-in template
  Output := WrapperFromModel(
    Server.Model,
    Server.Services,
    'delphi'  // Template name: 'delphi', 'javascript', 'typescript'
  );

  // Or use custom Mustache template
  Output := WrapperFromModel(
    Server.Model,
    Server.Services,
    '',                          // No built-in template
    FileToString('MyTemplate.mustache')  // Custom template
  );

  // Save generated code
  FileFromString(Output, 'GeneratedClient.pas');
end;
```

### 16.11.4. Built-in Templates

| Template | Output | Use Case |
|----------|--------|----------|
| `delphi` | `.pas` unit | Native Delphi/FPC client |
| `javascript` | `.js` file | Browser/Node.js client |
| `typescript` | `.ts` file | TypeScript projects |

### 16.11.5. Async Interface Generation

Convert synchronous interfaces to async versions:

```pascal
type
  // Original synchronous interface
  ICalculator = interface(IInvokable)
    function Add(n1, n2: Integer): Integer;
  end;

// Generated async version (conceptual):
//   ICalculatorAsync = interface(IInvokable)
//     procedure AddAsync(n1, n2: Integer;
//       const OnSuccess: TOnAsyncSuccess<Integer>;
//       const OnError: TOnAsyncError);
//   end;
```

### 16.11.6. REST API Documentation

Generate OpenAPI/Swagger documentation:

```pascal
uses
  mormot.net.openapi;

var
  OpenAPI: TDocVariantData;
begin
  // Generate OpenAPI 3.0 spec from model
  OpenAPI := OpenAPIFromModel(Server.Model, Server.Services);
  FileFromString(OpenAPI.ToJson('', '', jsonHumanReadable), 'api-spec.json');
end;
```

### 16.11.7. HTTP Endpoint for Code Generation

Expose code generation via HTTP:

```pascal
// Register method-based service
Server.ServiceMethodRegisterPublishedMethods('wrapper', TCodeGenService.Create(Server));

// Client can now GET /root/wrapper/delphi to download generated code
```

---

## 16.12. Complete Example

### 16.12.1. Shared Interface Unit

```pascal
unit ProjectInterface;

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1, n2: Integer): Integer;
    function Multiply(n1, n2: Int64): Int64;
  end;

const
  ROOT_NAME = 'api';
  PORT_NAME = '8080';

implementation

initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator)]);
end.
```

### 16.12.2. Server Application

```pascal
program Server;

{$APPTYPE CONSOLE}

uses
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  ProjectInterface;

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1, n2: Integer): Integer;
    function Multiply(n1, n2: Int64): Int64;
  end;

function TServiceCalculator.Add(n1, n2: Integer): Integer;
begin
  Result := n1 + n2;
end;

function TServiceCalculator.Multiply(n1, n2: Int64): Int64;
begin
  Result := n1 * n2;
end;

var
  Model: TOrmModel;
  Server: TRestServerFullMemory;
  HttpServer: TRestHttpServer;
begin
  Model := TOrmModel.Create([], ROOT_NAME);
  Server := TRestServerFullMemory.Create(Model);
  try
    Server.ServiceDefine(TServiceCalculator, [ICalculator], sicShared);

    HttpServer := TRestHttpServer.Create(PORT_NAME, [Server], '+', useHttpAsync);
    try
      WriteLn('Server running on http://localhost:', PORT_NAME);
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

### 16.12.3. Client Application

```pascal
program Client;

{$APPTYPE CONSOLE}

uses
  mormot.core.base,
  mormot.orm.core,
  mormot.rest.client,
  mormot.rest.http.client,
  ProjectInterface;

var
  Model: TOrmModel;
  Client: TRestHttpClientSocket;
  Calc: ICalculator;
begin
  Model := TOrmModel.Create([], ROOT_NAME);
  Client := TRestHttpClientSocket.Create('localhost', PORT_NAME, Model);
  try
    Client.ServiceDefine([ICalculator], sicShared);

    if Client.Services.Resolve(ICalculator, Calc) then
    begin
      WriteLn('10 + 20 = ', Calc.Add(10, 20));
      WriteLn('10 * 20 = ', Calc.Multiply(10, 20));
    end
    else
      WriteLn('Service not available');
  finally
    Client.Free;
    Model.Free;
  end;
end.
```

---

## Summary

Interface-based services in mORMot 2 provide:

- **Clean contracts** via Pascal interfaces
- **Automatic marshalling** of complex types to/from JSON
- **Multiple instance modes** for different use cases
- **Contract validation** ensuring client/server compatibility
- **Per-method authorization** with user groups
- **Bidirectional communication** via WebSocket callbacks
- **Execution logging** to database
- **Dependency injection** via `TInjectableObjectRest`

For most applications, interface-based services are the recommended approach. They provide the structure, safety, and features needed for robust SOA while keeping implementation simple and type-safe.

---

## Navigation

| Previous | Index | Next |
|----------|-------|------|
| [Chapter 15: Interfaces and SOLID Design](mORMot2-SAD-Chapter-15.md) | [Index](mORMot2-SAD-Index.md) | [Chapter 17: Cross-Platform Clients](mORMot2-SAD-Chapter-17.md) |
