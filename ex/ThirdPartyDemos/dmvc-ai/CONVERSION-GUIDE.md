# DMVC Framework to mORMot2 Conversion Guide

**Quick Reference for Converting DelphiMVCFramework Applications to mORMot2**

**Last Updated**: 2025-12-19

---

## Table of Contents

1. [Architecture Patterns](#1-architecture-patterns)
2. [Controller Conversion](#2-controller-conversion)
3. [Routing Mapping](#3-routing-mapping)
4. [Response Rendering](#4-response-rendering)
5. [Database Patterns](#5-database-patterns)
6. [Middleware Equivalents](#6-middleware-equivalents)
7. [Authentication](#7-authentication)
8. [WebSockets](#8-websockets)
9. [Session Management](#9-session-management)
10. [Common Pitfalls](#10-common-pitfalls)

---

## 1. Architecture Patterns

### Server Setup

#### DMVC
```pascal
uses
  MVCFramework,
  MVCFramework.Server,
  MVCFramework.Server.Impl;

var
  LServer: IMVCServer;
begin
  LServer := TMVCListener.Create(
    TMVCListenerProperties.New
      .SetName('MyServer')
      .SetPort(8080)
      .SetMaxConnections(1024)
  );

  LServer.AddController(TMyController);
  LServer.Start;
end;
```

#### mORMot2 Equivalent
```pascal
uses
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server;

var
  Model: TOrmModel;
  Server: TRestServerDB;
  HttpServer: TRestHttpServer;
begin
  // Define data model (optional for pure services)
  Model := TOrmModel.Create([]);

  // Create REST server
  Server := TRestServerFullMemory.Create(Model, 'root', {authentication=}true);

  // Register interface-based services
  Server.ServiceDefine(TMyService, [IMyService], sicShared);

  // Create HTTP server
  HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);
end;
```

**Key Differences**:
- DMVC uses **TMVCEngine** with controllers → mORMot2 uses **TRestServer** with services
- DMVC controllers are classes → mORMot2 services are **interfaces** (IInvokable)
- DMVC routing is attribute-based → mORMot2 routing is **automatic** from interface methods
- mORMot2 requires a **TOrmModel** even for non-ORM services (can be empty)

**Server Mode Options**:
```pascal
// mORMot2 HTTP server modes
useHttpApi        // Windows http.sys kernel-mode (fastest on Windows)
useHttpAsync      // Socket-based event-driven with thread pool (best scaling)
useHttpSocket     // Socket-based one thread per connection (simple)
```

---

## 2. Controller Conversion

### Method-Based Services (Simple Pattern)

#### DMVC Controller
```pascal
type
  TMyController = class(TMVCController)
  public
    [MVCPath('/api/hello')]
    [MVCHTTPMethod([httpGET])]
    procedure HelloWorld;

    [MVCPath('/api/user/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUser(id: Integer);
  end;

implementation

procedure TMyController.HelloWorld;
begin
  Render('Hello World');
end;

procedure TMyController.GetUser(id: Integer);
begin
  Render(TUser.Create(id).AsJSON);
end;
```

#### mORMot2 Method-Based Service
```pascal
type
  TMyRestServer = class(TRestServerFullMemory)
  published
    // Published methods automatically become service endpoints
    procedure HelloWorld(Ctxt: TRestServerUriContext);
    procedure GetUser(Ctxt: TRestServerUriContext);
  end;

implementation

procedure TMyRestServer.HelloWorld(Ctxt: TRestServerUriContext);
begin
  if Ctxt.Method = mGET then
    Ctxt.Returns('Hello World', HTTP_SUCCESS)
  else
    Ctxt.Error('Method not allowed', HTTP_NOTALLOWED);
end;

procedure TMyRestServer.GetUser(Ctxt: TRestServerUriContext);
var
  id: Integer;
begin
  if (Ctxt.Method = mGET) and
     Ctxt.InputInt['id'].ToInteger(id) then
  begin
    var user := TUser.Create(id);
    try
      Ctxt.Returns(user.AsJSON, HTTP_SUCCESS);
    finally
      user.Free;
    end;
  end
  else
    Ctxt.Error('Bad Request', HTTP_BADREQUEST);
end;
```

**Routes**:
- DMVC: `GET /api/hello` → mORMot2: `GET /root/HelloWorld`
- DMVC: `GET /api/user/123` → mORMot2: `GET /root/GetUser?id=123`

**Important Notes**:
- Method signature must be: `procedure MethodName(Ctxt: TRestServerUriContext)`
- Methods must be in the `published` section
- Manual HTTP method checking required (`if Ctxt.Method = mGET then`)
- Access parameters via `Ctxt.InputUtf8['name']` or `Ctxt.InputInt['name']`

---

### Interface-Based Services (Recommended Pattern)

#### DMVC Controller (Complex Logic)
```pascal
type
  [MVCPath('/api/users')]
  TUserController = class(TMVCController)
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    procedure GetAllUsers;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUser(id: Integer);

    [MVCPath('')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateUser;

    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateUser(id: Integer);
  end;
```

#### mORMot2 Interface-Based Service
```pascal
type
  // Define service interface
  IUserService = interface(IInvokable)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function GetAllUsers: TUserDynArray;
    function GetUser(id: Integer): TUserDto;
    function CreateUser(const user: TUserDto): Integer;
    procedure UpdateUser(id: Integer; const user: TUserDto);
  end;

  // Implementation
  TUserService = class(TInterfacedObject, IUserService)
  public
    function GetAllUsers: TUserDynArray;
    function GetUser(id: Integer): TUserDto;
    function CreateUser(const user: TUserDto): Integer;
    procedure UpdateUser(id: Integer; const user: TUserDto);
  end;

implementation

// Register interface
initialization
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IUserService)]);

// Server registration
Server.ServiceDefine(TUserService, [IUserService], sicShared);
```

**Routes** (automatic from interface):
- `GetAllUsers` → `POST /root/UserService.GetAllUsers` (JSON-RPC style)
- `GetUser` → `POST /root/UserService.GetUser` with `{"id":123}` body
- `CreateUser` → `POST /root/UserService.CreateUser` with user JSON
- `UpdateUser` → `POST /root/UserService.UpdateUser` with `{"id":123, "user":{...}}`

**Key Advantages**:
- ✅ Automatic JSON serialization/deserialization
- ✅ Type-safe method signatures
- ✅ No manual HTTP method checking
- ✅ Client proxy auto-generation
- ✅ Built-in exception handling

**Interface Contract Options**:
```pascal
Server.ServiceDefine(TMyService, [IMyService], sicShared);      // Singleton
Server.ServiceDefine(TMyService, [IMyService], sicPerSession);  // Per session
Server.ServiceDefine(TMyService, [IMyService], sicPerThread);   // Per thread
Server.ServiceDefine(TMyService, [IMyService], sicPerUser);     // Per authenticated user
```

---

## 3. Routing Mapping

### Route Patterns

| DMVC Pattern | mORMot2 Method-Based | mORMot2 Interface-Based |
|-------------|---------------------|------------------------|
| `GET /api/hello` | `GET /root/Hello` | `POST /root/Service.Hello` |
| `GET /api/user/123` | `GET /root/GetUser?id=123` | `POST /root/Service.GetUser` + body |
| `POST /api/user` | `POST /root/CreateUser` | `POST /root/Service.CreateUser` + body |
| `PUT /api/user/123` | `PUT /root/UpdateUser?id=123` | `POST /root/Service.UpdateUser` + body |
| `DELETE /api/user/123` | `DELETE /root/DeleteUser?id=123` | `POST /root/Service.DeleteUser` + body |

### RESTful vs JSON-RPC

#### DMVC (RESTful)
```
GET    /api/articles          → List all
GET    /api/articles/123      → Get one
POST   /api/articles          → Create
PUT    /api/articles/123      → Update
DELETE /api/articles/123      → Delete
```

#### mORMot2 Method-Based (RESTful-like)
```
GET    /root/Articles                → List all
GET    /root/Article?id=123          → Get one
POST   /root/CreateArticle           → Create
PUT    /root/UpdateArticle?id=123    → Update
DELETE /root/DeleteArticle?id=123    → Delete
```

#### mORMot2 Interface-Based (JSON-RPC)
```
POST /root/ArticleService.GetAll
POST /root/ArticleService.GetById    {"id":123}
POST /root/ArticleService.Create     {"article":{...}}
POST /root/ArticleService.Update     {"id":123, "article":{...}}
POST /root/ArticleService.Delete     {"id":123}
```

**Note**: mORMot2 interface-based services use **POST** for all operations with JSON bodies.

---

## 4. Response Rendering

### JSON Responses

#### DMVC
```pascal
procedure TMyController.GetUsers;
var
  users: TObjectList<TUser>;
begin
  users := TUserService.GetAll;
  try
    Render(users);  // Automatic JSON serialization
  finally
    users.Free;
  end;
end;

procedure TMyController.GetUser;
var
  user: TUser;
begin
  user := TUserService.GetById(Context.Request.Params['id'].ToInteger);
  try
    Render(user);
  finally
    user.Free;
  end;
end;

// Custom status code
procedure TMyController.NotFound;
begin
  Render(HTTP_STATUS.NotFound, 'User not found');
end;
```

#### mORMot2 Method-Based
```pascal
procedure TMyRestServer.GetUsers(Ctxt: TRestServerUriContext);
var
  users: TUserDynArray;
  json: RawUtf8;
begin
  users := TUserService.GetAll;
  json := DynArraySaveJson(users, TypeInfo(TUserDynArray));
  Ctxt.Returns(json, HTTP_SUCCESS, [], TEXT_CONTENT_TYPE_HEADER);
end;

procedure TMyRestServer.GetUser(Ctxt: TRestServerUriContext);
var
  user: TUserDto;
  id: Integer;
begin
  if Ctxt.InputInt['id'].ToInteger(id) then
  begin
    if TUserService.GetById(id, user) then
      Ctxt.Returns(user.ToJson, HTTP_SUCCESS)
    else
      Ctxt.Error('User not found', HTTP_NOTFOUND);
  end;
end;
```

#### mORMot2 Interface-Based (Automatic)
```pascal
type
  IUserService = interface(IInvokable)
    ['{...}']
    function GetAll: TUserDynArray;           // Automatic JSON array
    function GetById(id: Integer): TUserDto;   // Automatic JSON object
  end;

implementation

function TUserService.GetAll: TUserDynArray;
begin
  Result := FRepository.LoadAll;  // Automatic serialization!
end;

function TUserService.GetById(id: Integer): TUserDto;
begin
  if not FRepository.Load(id, Result) then
    raise EServiceException.CreateUtf8('User % not found', [id]);
end;
```

**Key Differences**:
- DMVC: `Render()` auto-serializes → mORMot2 method-based: Manual `DynArraySaveJson()` or `ObjectToJson()`
- DMVC: Status codes via `Render(statusCode)` → mORMot2: `Ctxt.Returns(json, HTTP_SUCCESS)` or `Ctxt.Error(msg, HTTP_NOTFOUND)`
- mORMot2 interface-based: **Fully automatic** serialization/deserialization!

---

### Other Response Types

#### DMVC
```pascal
// HTML
Render('<h1>Hello</h1>', 'text/html');

// Binary (file download)
RenderFile('path/to/file.pdf');

// Stream
RenderStream(myStream, 'application/pdf');

// SSE (Server-Sent Events)
ResponseStream.SendChunk('data: Hello\n\n');
```

#### mORMot2
```pascal
// HTML
Ctxt.Returns('<h1>Hello</h1>', HTTP_SUCCESS, [], 'Content-Type: text/html');

// Binary (file download)
Ctxt.ReturnFile('path/to/file.pdf', {deletefile=}false);

// Stream
Ctxt.ReturnStream(myStream, HTTP_SUCCESS, {ownstream=}true, 'application/pdf');

// SSE (Server-Sent Events)
Ctxt.ServerSendEvents([TServerSentEvent.Create('Hello')]);
```

---

## 5. Database Patterns

### ActiveRecord Pattern

#### DMVC ActiveRecord
```pascal
uses
  MVCFramework.ActiveRecord;

type
  TUser = class(TMVCActiveRecord)
  private
    [MVCTableField('id', [foPrimaryKey, foAutoGenerated])]
    FId: Integer;

    [MVCTableField('name')]
    FName: string;

    [MVCTableField('email')]
    FEmail: string;
  end;

// Usage
var user := TUser.Create;
user.Name := 'John';
user.Email := 'john@example.com';
user.Insert;

// Find
var users := TMVCActiveRecord.SelectRQL<TUser>('eq(email,"john@example.com")', 10);

// Update
var user := TMVCActiveRecord.GetByPK<TUser>(123);
user.Name := 'Jane';
user.Update;

// Delete
user.Delete;
```

#### mORMot2 ORM
```pascal
uses
  mormot.orm.core,
  mormot.orm.base;

type
  TOrmUser = class(TOrm)
  private
    fName: RawUtf8;
    fEmail: RawUtf8;
  published
    property Name: RawUtf8 index 100 read fName write fName;
    property Email: RawUtf8 index 100 read fEmail write fEmail stored AS_UNIQUE;
  end;

// Usage (requires TRestServer instance)
var user := TOrmUser.Create;
user.Name := 'John';
user.Email := 'john@example.com';
Server.Orm.Add(user, true);  // true = send result back

// Find
var user := TOrmUser.Create(Server.Orm, 'Email=?', ['john@example.com']);

// Batch retrieve
var users: TOrmUserObjArray;
Server.Orm.RetrieveListObjArray(users, TOrmUser, 'Name LIKE ?', ['J%']);

// Update
user.Name := 'Jane';
Server.Orm.Update(user);

// Delete
Server.Orm.Delete(TOrmUser, user.ID);
```

**Key Differences**:
- DMVC: Self-contained ActiveRecord → mORMot2: Requires `TRestServer.Orm` instance
- DMVC: `Insert/Update/Delete` methods → mORMot2: `Server.Orm.Add/Update/Delete`
- DMVC: RQL queries → mORMot2: SQL-like queries with parameter binding
- mORMot2: Published properties are **persistent**, private are transient
- mORMot2: Automatic ID field (`TOrm.ID: TID`)

---

### Repository Pattern

#### DMVC with Dependency Injection
```pascal
type
  IUserRepository = interface
    ['{...}']
    function GetAll: TObjectList<TUser>;
    function GetById(id: Integer): TUser;
    procedure Save(user: TUser);
    procedure Delete(id: Integer);
  end;

  TUserController = class(TMVCController)
  private
    [Inject] FRepository: IUserRepository;
  public
    procedure GetAll;
  end;

procedure TUserController.GetAll;
begin
  Render(FRepository.GetAll);
end;
```

#### mORMot2 with Service Dependency Injection
```pascal
type
  // Repository interface
  IUserRepository = interface(IInvokable)
    ['{...}']
    function GetAll: TUserDynArray;
    function GetById(id: Integer; out user: TUserDto): Boolean;
    procedure Save(const user: TUserDto);
    procedure Delete(id: Integer);
  end;

  // Service interface (uses repository)
  IUserService = interface(IInvokable)
    ['{...}']
    function GetAll: TUserDynArray;
    function GetById(id: Integer): TUserDto;
  end;

  // Service implementation with constructor injection
  TUserService = class(TInjectableObject, IUserService)
  private
    fRepository: IUserRepository;
  public
    constructor Create(const aRepository: IUserRepository); reintroduce;
    function GetAll: TUserDynArray;
    function GetById(id: Integer): TUserDto;
  end;

constructor TUserService.Create(const aRepository: IUserRepository);
begin
  inherited Create;
  fRepository := aRepository;
end;

// Registration
Server.ServiceDefine(TUserRepository, [IUserRepository], sicShared);
Server.ServiceDefine(TUserService, [IUserService], sicShared)
      .SetOptions([], [optExecLockedPerInterface])  // Thread-safe
      .ByPassAuthentication([]);  // Require auth
```

**Resolver Pattern** (for complex DI):
```pascal
Server.ServiceResolver := TServiceResolverCustom.Create;
Server.ServiceResolver.Add(IUserRepository, TUserRepository);
```

---

## 6. Middleware Equivalents

### CORS Middleware

#### DMVC
```pascal
procedure TMVCWebModule.WebModuleCreate(Sender: TObject);
begin
  fEngine := TMVCEngine.Create(Self);

  // CORS middleware
  fEngine.AddMiddleware(TMVCCORSMiddleware.Create(
    'GET,POST,PUT,DELETE',  // Methods
    '*',                     // Origins
    'Content-Type,Authorization'  // Headers
  ));
end;
```

#### mORMot2
```pascal
// Server setup
HttpServer := TRestHttpServer.Create('8080', [Server], '+', useHttpAsync);

// CORS support (built-in)
HttpServer.AccessControlAllowOrigin := '*';  // All origins
// or specific origins:
HttpServer.AccessControlAllowOrigin := 'https://example.com';

// Custom headers
HttpServer.OnHttpThreadStart := procedure(Sender: TRestHttpServerDefinition)
begin
  Sender.HttpServer.ServerName := 'MyAPI/1.0';
  // Add custom middleware logic here
end;
```

**Advanced CORS** (custom event):
```pascal
Server.OnFilterRequest := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext): Boolean
begin
  // Add CORS headers for preflight
  if Request.Method = mOPTIONS then
  begin
    Request.OutHead := 'Access-Control-Allow-Methods: GET,POST,PUT,DELETE'#13#10 +
                       'Access-Control-Allow-Headers: Content-Type,Authorization'#13#10 +
                       'Access-Control-Max-Age: 86400';
    Request.Returns('', HTTP_SUCCESS);
    Result := true;  // Handled
  end
  else
    Result := false;  // Continue processing
end;
```

---

### Authentication Middleware

#### DMVC Custom Auth
```pascal
procedure TMVCWebModule.WebModuleCreate(Sender: TObject);
begin
  fEngine.AddMiddleware(TMVCCustomAuthenticationMiddleware.Create(
    procedure(const Username, Password: string; UserRoles: TList<string>;
              var IsValid: Boolean; const SessionData: TMVCSessionData)
    begin
      IsValid := ValidateUser(Username, Password);
      if IsValid then
      begin
        UserRoles.Add('user');
        SessionData['username'] := Username;
      end;
    end
  ));
end;

// Protected controller
type
  [MVCRequiresAuthentication]
  TProtectedController = class(TMVCController)
  public
    [MVCPath('/api/protected')]
    [MVCHTTPMethod([httpGET])]
    procedure ProtectedEndpoint;
  end;
```

#### mORMot2 Authentication
```pascal
// Server setup with authentication
Server := TRestServerFullMemory.Create(Model, 'root', {authentication=}true);

// Default authentication (session-based)
Server.HandleAuthentication;

// Register auth handler
Server.AuthenticationRegister(TRestServerAuthenticationDefault);

// Create users
Server.AuthenticationRegister(TAuthUser.Create);
var user := TAuthUser.Create;
user.LogonName := 'john';
user.PasswordHashHexa := SHA256('password123');  // Hashed!
Server.Orm.Add(user, true);

// Client login
Client.SetUser(TAuthUser, 'john', 'password123');

// Service with authentication check
type
  IProtectedService = interface(IInvokable)
    ['{...}']
    [ServiceMethodAuthenticationRequired]  // Attribute!
    function GetData: RawUtf8;
  end;
```

**JWT Authentication**:
```pascal
uses
  mormot.crypt.jwt;

// Server-side JWT generation
var jwt: TJwtContent;
jwt.aud := ['myapp'];
jwt.iat := NowUtc;
jwt.exp := NowUtc + 1;  // 1 day
jwt.data['userid'] := UserId;
var token := jwt.Compute(['mysecret']);

// Client sends token
Client.SessionHttpHeader := 'Authorization: Bearer ' + token;

// Server validates in OnFilterRequest
if Request.AuthenticationCheck(jwtAlgHS256) then
  // Authenticated
```

---

### Logging Middleware

#### DMVC
```pascal
fEngine.AddMiddleware(TMVCLoggerMiddleware.Create(
  function: IMVCLogger
  begin
    Result := TMVCDefaultLogger.Create;
  end
));
```

#### mORMot2
```pascal
uses
  mormot.core.log;

// Global logger (automatic in mORMot2)
TSynLog.Family.Level := LOG_VERBOSE;  // All levels
TSynLog.Family.PerThreadLog := ptIdentifiedInOneFile;

// HTTP traffic logging
HttpServer.HttpServer.LogClass := TSQLRestServerDB;
Server.LogFamily.Level := LOG_VERBOSE;

// Custom request logging
Server.OnFilterRequest := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext): Boolean
begin
  TSynLog.Add.Log(sllTrace, 'Request: % %', [Request.Method, Request.Uri]);
  Result := false;  // Continue
end;

Server.OnFilterResponse := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext)
begin
  TSynLog.Add.Log(sllTrace, 'Response: % bytes', [Length(Request.OutBody)]);
end;
```

**Log Output**:
```
20251219 12:34:56.789 +    trace Request: GET /root/GetUser?id=123
20251219 12:34:56.795 +    trace Response: 345 bytes
```

---

### Compression Middleware

#### DMVC
```pascal
fEngine.AddMiddleware(TMVCCompressionMiddleware.Create);
```

#### mORMot2 (Built-in)
```pascal
// Automatic compression (enabled by default)
HttpServer.CompressGz := 6;  // gzip compression level 1-9 (0=disabled)

// Or disable
HttpServer.CompressGz := 0;
```

---

### Rate Limiting

#### DMVC (Redis-based)
```pascal
fEngine.AddMiddleware(TMVCRateLimitMiddleware.Create(
  TRedisRateLimiter.Create('localhost', 6379),
  100,  // max requests
  60    // per 60 seconds
));
```

#### mORMot2 (Custom Implementation)
```pascal
type
  TRateLimiter = class
  private
    fRequests: TSynDictionary;  // IP -> request count
    fLock: TRWLightLock;
  public
    function CheckLimit(const IP: RawUtf8; maxRequests, seconds: Integer): Boolean;
  end;

var
  RateLimiter := TRateLimiter.Create;

Server.OnFilterRequest := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext): Boolean
begin
  if not RateLimiter.CheckLimit(Request.RemoteIP, 100, 60) then
  begin
    Request.Error('Rate limit exceeded', HTTP_TOOMANYREQUESTS);
    Result := true;  // Stop processing
  end
  else
    Result := false;  // Continue
end;
```

**Note**: mORMot2 doesn't have built-in rate limiting. Implement via `OnFilterRequest`.

---

## 7. Authentication

### Basic Authentication

#### DMVC
```pascal
type
  [MVCRequiresAuthentication]
  TMyController = class(TMVCController)
  public
    [MVCPath('/login')]
    [MVCHTTPMethod([httpPOST])]
    procedure Login;

    [MVCPath('/protected')]
    [MVCHTTPMethod([httpGET])]
    procedure Protected;
  end;

procedure TMyController.Login;
var
  username, password: string;
begin
  username := Context.Request.Headers['X-Username'];
  password := Context.Request.Headers['X-Password'];

  if ValidateCredentials(username, password) then
  begin
    Session['username'] := username;
    Render(HTTP_STATUS.OK);
  end
  else
    Render(HTTP_STATUS.Unauthorized);
end;
```

#### mORMot2
```pascal
// Server with authentication
Server := TRestServerFullMemory.Create(Model, 'root', true);
Server.AuthenticationRegister(TRestServerAuthenticationDefault);

// Create user
var user := TAuthUser.Create;
user.LogonName := 'john';
user.PasswordHashHexa := Sha256('password123');
Server.Orm.Add(user, true);

// Client login
var Client := TRestHttpClientWinHttp.Create('localhost', '8080', Model);
if Client.SetUser(TAuthUser, 'john', 'password123') then
  ShowMessage('Logged in: Session ID = ' + IntToStr(Client.SessionUser.ID));

// Service method with auth check
procedure TMyRestServer.Protected(Ctxt: TRestServerUriContext);
begin
  if Ctxt.Session = SESSION_USER_UNAUTHORIZED then
  begin
    Ctxt.AuthenticationFailed(HTTP_UNAUTHORIZED);
    Exit;
  end;

  Ctxt.Returns('Protected data', HTTP_SUCCESS);
end;
```

---

### JWT Authentication

#### DMVC
```pascal
uses
  MVCFramework.JWT;

procedure TMyController.Login;
var
  jwt: TJWT;
  token: string;
begin
  jwt := TJWT.Create('mysecret');
  try
    jwt.Claims.Issuer := 'MyApp';
    jwt.Claims.Subject := 'john@example.com';
    jwt.Claims.ExpirationTime := Now + 1;  // 1 day
    jwt.CustomClaims['userid'] := '123';

    token := jwt.GetToken;
    Render(TJSONObject.Create.AddPair('token', token));
  finally
    jwt.Free;
  end;
end;

procedure TMyController.Protected;
var
  token: string;
  jwt: TJWT;
begin
  token := Context.Request.Headers['Authorization'].Replace('Bearer ', '');

  jwt := TJWT.Create('mysecret');
  try
    if jwt.LoadToken(token) and jwt.IsValid then
      Render('Protected data')
    else
      Render(HTTP_STATUS.Unauthorized);
  finally
    jwt.Free;
  end;
end;
```

#### mORMot2
```pascal
uses
  mormot.crypt.jwt;

type
  TMyService = class(TInjectableObject, IMyService)
  private
    fServer: TRestServer;
  public
    function Login(const username, password: RawUtf8): RawUtf8;
    [ServiceMethodAuthenticationRequired]
    function GetProtectedData: RawUtf8;
  end;

function TMyService.Login(const username, password: RawUtf8): RawUtf8;
var
  jwt: TJwtContent;
  userid: Integer;
begin
  if ValidateCredentials(username, password, userid) then
  begin
    jwt.aud := ['myapp'];
    jwt.iss := 'MyApp';
    jwt.exp := UnixTimeUtc + 86400;  // 1 day
    jwt.data['userid'] := userid;
    jwt.data['username'] := username;

    Result := TJwtAbstract.Compute(jwt, 'mysecret', jwtHS256);
  end
  else
    raise EServiceException.Create('Invalid credentials');
end;

// Server-side JWT validation
Server.OnFilterRequest := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext): Boolean
var
  token: RawUtf8;
  jwt: TJwtContent;
begin
  token := Request.InHeader['Authorization'];
  if IdemPChar(pointer(token), 'BEARER ') then
  begin
    Delete(token, 1, 7);
    if TJwtAbstract.Verify(token, 'mysecret', jwtHS256, jwt) then
    begin
      Request.Session := jwt.data.U['userid'];  // Set user context
      Result := false;  // Continue
    end
    else
    begin
      Request.Error('Invalid token', HTTP_UNAUTHORIZED);
      Result := true;  // Stop
    end;
  end;
end;
```

---

### Role-Based Authorization

#### DMVC
```pascal
type
  [MVCRequiresRole('admin')]
  TAdminController = class(TMVCController)
  public
    [MVCPath('/admin/users')]
    [MVCHTTPMethod([httpGET])]
    procedure GetUsers;
  end;
```

#### mORMot2
```pascal
uses
  mormot.rest.server;

type
  // Extend TAuthUser with roles
  TAuthUserWithRoles = class(TAuthUser)
  private
    fRoles: RawUtf8;  // Comma-separated: 'admin,editor'
  published
    property Roles: RawUtf8 read fRoles write fRoles;
  end;

// Custom authorization
Server.OnFilterRequest := procedure(const Sender: TRestServer;
  const Request: TRestServerUriContext): Boolean
var
  user: TAuthUserWithRoles;
  requiredRole: RawUtf8;
begin
  // Check if admin endpoint
  if IdemPChar(pointer(Request.Uri), '/ROOT/ADMIN') then
  begin
    requiredRole := 'admin';

    // Load user
    user := TAuthUserWithRoles.Create(Server.Orm, Request.Session);
    try
      if Pos(requiredRole, user.Roles) = 0 then
      begin
        Request.Error('Forbidden', HTTP_FORBIDDEN);
        Result := true;  // Stop
        Exit;
      end;
    finally
      user.Free;
    end;
  end;

  Result := false;  // Continue
end;

// Or use interface-based with custom attribute
type
  IAdminService = interface(IInvokable)
    ['{...}']
    [RequiresRole('admin')]  // Custom attribute
    procedure DeleteAllUsers;
  end;
```

---

## 8. WebSockets

### Basic WebSocket Server

#### DMVC
```pascal
uses
  MVCFramework.WebSocket;

type
  TMyWebSocket = class(TWebSocketController)
  public
    procedure OnMessage(const Message: string); override;
    procedure OnConnect; override;
    procedure OnDisconnect; override;
  end;

procedure TMyWebSocket.OnConnect;
begin
  inherited;
  SendToAll('User connected');
end;

procedure TMyWebSocket.OnMessage(const Message: string);
begin
  SendToAll('Echo: ' + Message);
end;

// Registration
fEngine.AddWebSocketController('/ws', TMyWebSocket);
```

#### mORMot2
```pascal
uses
  mormot.net.websocket,
  mormot.rest.websockets;

type
  TMyWebSocketProtocol = class(TWebSocketProtocolRest)
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame); override;
  end;

procedure TMyWebSocketProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame);
begin
  // Echo back
  Sender.SendFrame(Frame);

  // Broadcast to all
  Sender.Broadcast(Frame);
end;

// Server registration
var protocol := TMyWebSocketProtocol.Create('myprotocol', 'user');
HttpServer.WebSocketsEnable(Server, 'myprotocol')
          .SetFullLog;  // Full WebSocket logging
```

**Client**:
```pascal
uses
  mormot.net.websocket;

var ws := TWebSocketClient.Create;
ws.Connect('ws://localhost:8080', 'myprotocol');
ws.Send('Hello');

// Receive
var frame: TWebSocketFrame;
if ws.ReceiveFrame(frame, 5000) then
  ShowMessage(frame.PayloadUtf8);
```

---

### WebSocket with Authentication

#### DMVC
```pascal
type
  TAuthWebSocket = class(TWebSocketController)
  protected
    function Authorize: Boolean; override;
  end;

function TAuthWebSocket.Authorize: Boolean;
var
  token: string;
begin
  token := Context.Request.Headers['Sec-WebSocket-Protocol'];
  Result := ValidateToken(token);
end;
```

#### mORMot2
```pascal
// Server with WebSocket + Auth
Server := TRestServerFullMemory.Create(Model, 'root', true);
Server.AuthenticationRegister(TRestServerAuthenticationDefault);

var protocol := TWebSocketProtocolRest.Create('myprotocol', 'user');
HttpServer.WebSocketsEnable(Server, 'myprotocol');

// Client auth + WebSocket upgrade
var Client := TRestHttpClientWebsockets.Create('localhost', '8080', Model);
Client.WebSocketsUpgrade('myprotocol');
Client.SetUser(TAuthUser, 'john', 'password123');

// Callbacks over WebSocket
Client.ServiceDefine([IMyCallback], sicClientDriven);
Client.Services.CallBackSet(TMyCallback.Create(Client));
```

---

### WebSocket Chat Example

#### DMVC
```pascal
type
  TChatWebSocket = class(TWebSocketController)
  private
    class var fClients: TThreadList<TWebSocketContext>;
  public
    procedure OnConnect; override;
    procedure OnMessage(const Message: string); override;
    procedure OnDisconnect; override;
  end;

procedure TChatWebSocket.OnMessage(const Message: string);
var
  list: TList<TWebSocketContext>;
  ctx: TWebSocketContext;
begin
  list := fClients.LockList;
  try
    for ctx in list do
      ctx.Send(Message);
  finally
    fClients.UnlockList;
  end;
end;
```

#### mORMot2
```pascal
type
  TChatProtocol = class(TWebSocketProtocolRest)
  private
    fClients: TSynDictionary;  // Thread-safe
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame); override;
  public
    procedure AfterCreate; override;
    procedure BeforeDestroy; override;
  end;

procedure TChatProtocol.AfterCreate;
begin
  inherited;
  fClients := TSynDictionary.Create(TypeInfo(TPointerDynArray), TypeInfo(TWebSocketProcess));
end;

procedure TChatProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame);
var
  i: Integer;
  clients: TWebSocketProcessDynArray;
begin
  // Broadcast to all clients
  fClients.Safe.ReadLock;
  try
    clients := fClients.Values;
  finally
    fClients.Safe.ReadUnLock;
  end;

  for i := 0 to High(clients) do
    if clients[i] <> Sender then  // Don't echo to sender
      clients[i].SendFrame(Frame);
end;
```

**Full Example**: See `/mnt/w/mORMot2/ex/rest-websockets/` for complete chat server/client.

---

## 9. Session Management

### In-Memory Sessions

#### DMVC
```pascal
procedure TMyController.Login;
begin
  Session['userid'] := 123;
  Session['username'] := 'john';
  Render(HTTP_STATUS.OK);
end;

procedure TMyController.Protected;
var
  userid: Integer;
begin
  if Session.Contains('userid') then
  begin
    userid := Session['userid'];
    Render('User ID: ' + userid.ToString);
  end
  else
    Render(HTTP_STATUS.Unauthorized);
end;
```

#### mORMot2
```pascal
// Built-in session management (via authentication)
Server := TRestServerFullMemory.Create(Model, 'root', true);
Server.AuthenticationRegister(TRestServerAuthenticationDefault);

// Session data stored in TAuthUser
var user := TAuthUser.Create(Server.Orm, Ctxt.Session);
try
  // Access user data
  ShowMessage('User: ' + user.LogonName);
finally
  user.Free;
end;

// Custom session data (use TRestServer.SessionAccess)
Server.SessionAccess(Ctxt.Session).Data['custom_key'] := 'custom_value';
```

---

### Redis Sessions

#### DMVC
```pascal
uses
  MVCFramework.Session.Redis;

// Global configuration
TSessionStore.SetDefaultStore(TRedisSessionStore.Create('localhost', 6379));

// Usage (same as in-memory)
Session['key'] := 'value';
```

#### mORMot2
```pascal
// No built-in Redis session support
// Implement custom session storage:

uses
  mormot.db.nosql.redis;

type
  TRedisSessionStore = class
  private
    fRedis: TRedisClient;
  public
    procedure SetSession(sessionID: TID; const key, value: RawUtf8);
    function GetSession(sessionID: TID; const key: RawUtf8): RawUtf8;
  end;

var
  Redis := TRedisClient.Create;
  Redis.Connect('localhost', 6379);

procedure TRedisSessionStore.SetSession(sessionID: TID; const key, value: RawUtf8);
begin
  fRedis.Set(FormatUtf8('session:%:%', [sessionID, key]), value);
  fRedis.Expire(FormatUtf8('session:%:%', [sessionID, key]), 3600);  // 1 hour TTL
end;
```

**Note**: mORMot2 sessions are typically user-based (via authentication). For custom session storage, implement via events.

---

## 10. Common Pitfalls

### 1. HTTP Method Checking

❌ **DMVC (Automatic)**
```pascal
[MVCHTTPMethod([httpGET])]
procedure GetUser;
begin
  // GET only, enforced by framework
end;
```

⚠️ **mORMot2 Method-Based (Manual)**
```pascal
procedure GetUser(Ctxt: TRestServerUriContext);
begin
  // MUST check manually!
  if Ctxt.Method <> mGET then
  begin
    Ctxt.Error('Method not allowed', HTTP_NOTALLOWED);
    Exit;
  end;
  // ...
end;
```

✅ **mORMot2 Interface-Based (Automatic)**
```pascal
type
  IUserService = interface(IInvokable)
    ['{...}']
    function GetUser(id: Integer): TUserDto;  // POST only (JSON-RPC)
  end;
```

---

### 2. Parameter Binding

❌ **DMVC (Automatic)**
```pascal
[MVCPath('/user/($id)')]
procedure GetUser(id: Integer);  // Automatic conversion
```

⚠️ **mORMot2 Method-Based (Manual)**
```pascal
procedure GetUser(Ctxt: TRestServerUriContext);
var
  id: Integer;
begin
  // MUST manually extract and validate!
  if not Ctxt.InputInt['id'].ToInteger(id) then
  begin
    Ctxt.Error('Invalid ID', HTTP_BADREQUEST);
    Exit;
  end;
  // ...
end;
```

✅ **mORMot2 Interface-Based (Automatic)**
```pascal
function GetUser(id: Integer): TUserDto;  // Automatic!
```

---

### 3. JSON Serialization

❌ **DMVC (Automatic)**
```pascal
var users := GetAllUsers;
Render(users);  // Automatic JSON serialization
```

⚠️ **mORMot2 Method-Based (Manual)**
```pascal
var
  users: TUserDynArray;
  json: RawUtf8;
begin
  users := GetAllUsers;
  json := DynArraySaveJson(users, TypeInfo(TUserDynArray));  // Manual!
  Ctxt.Returns(json, HTTP_SUCCESS);
end;
```

✅ **mORMot2 Interface-Based (Automatic)**
```pascal
function GetAllUsers: TUserDynArray;  // Automatic serialization!
begin
  Result := FRepository.LoadAll;
end;
```

---

### 4. Exception Handling

❌ **DMVC**
```pascal
procedure GetUser(id: Integer);
begin
  var user := UserRepository.GetById(id);  // Raises exception if not found
  Render(user);
end;
```

✅ **mORMot2 Method-Based**
```pascal
procedure GetUser(Ctxt: TRestServerUriContext);
begin
  try
    var user := UserRepository.GetById(id);
    Ctxt.Returns(user.ToJson, HTTP_SUCCESS);
  except
    on E: ENotFoundException do
      Ctxt.Error('Not found', HTTP_NOTFOUND);
    on E: Exception do
      Ctxt.Error(E.Message, HTTP_SERVERERROR);
  end;
end;
```

✅ **mORMot2 Interface-Based**
```pascal
function GetUser(id: Integer): TUserDto;
begin
  if not FRepository.Load(id, Result) then
    raise EServiceException.CreateUtf8('User % not found', [id]);
    // Automatically converted to HTTP error!
end;
```

---

### 5. Content-Type Headers

❌ **DMVC (Automatic)**
```pascal
Render(user);  // Automatic Content-Type: application/json
```

⚠️ **mORMot2 Method-Based (Manual for non-JSON)**
```pascal
Ctxt.Returns('<html>...</html>', HTTP_SUCCESS, [], 'Content-Type: text/html');
                                                      // Must specify!
```

✅ **mORMot2 Interface-Based (JSON Automatic)**
```pascal
function GetData: TDto;  // Automatic Content-Type: application/json
```

---

### 6. Database Connection Management

❌ **DMVC ActiveRecord (Self-Managed)**
```pascal
var user := TMVCActiveRecord.GetByPK<TUser>(123);  // Connection from pool
```

⚠️ **mORMot2 ORM (Requires Server Instance)**
```pascal
// WRONG: No standalone ORM
var user := TOrmUser.Create;
user.Name := 'John';
// user.Insert;  // NO SUCH METHOD!

// CORRECT: Requires TRestServer
Server.Orm.Add(user, true);
```

---

### 7. Routing Case Sensitivity

❌ **DMVC (Case-Insensitive)**
```
GET /api/users
GET /API/USERS    → Same endpoint
```

⚠️ **mORMot2 (Case-Sensitive URI, Case-Insensitive Method)**
```
GET /root/GetUsers    → OK
GET /ROOT/GETUSERS    → OK (case-insensitive)
GET /root/getusers    → OK
```

---

### 8. Return Types

❌ **DMVC (Void Methods)**
```pascal
procedure CreateUser;
begin
  // ...
  Render(HTTP_STATUS.Created);
end;
```

✅ **mORMot2 Interface-Based (Explicit Return)**
```pascal
function CreateUser(const user: TUserDto): Integer;  // Returns ID
begin
  Result := FRepository.Insert(user);
end;

// Or void:
procedure DeleteUser(id: Integer);
begin
  FRepository.Delete(id);
end;
```

---

## Migration Checklist

### Phase 1: Architecture Planning
- [ ] Decide: Method-based vs Interface-based services (recommend interface-based)
- [ ] Map DMVC routes to mORMot2 service methods
- [ ] Identify shared DTOs and create Pascal records/classes
- [ ] Plan authentication strategy (default, JWT, custom)

### Phase 2: Data Layer
- [ ] Convert DMVC ActiveRecord classes to TOrm descendants
- [ ] Create TOrmModel with all entity types
- [ ] Implement repositories (if using repository pattern)
- [ ] Test ORM CRUD operations

### Phase 3: Service Layer
- [ ] Define service interfaces (IInvokable descendants)
- [ ] Implement service classes
- [ ] Register interfaces with TInterfaceFactory
- [ ] Register services with Server.ServiceDefine()

### Phase 4: Server Setup
- [ ] Create TRestServer instance
- [ ] Configure authentication
- [ ] Register services
- [ ] Create TRestHttpServer
- [ ] Test endpoints with REST client (Postman, curl)

### Phase 5: Middleware/Cross-Cutting
- [ ] Implement CORS (via HttpServer.AccessControlAllowOrigin)
- [ ] Add logging (TSynLog)
- [ ] Add custom filters (OnFilterRequest/OnFilterResponse)
- [ ] Add exception handling (OnException)

### Phase 6: Client/Frontend
- [ ] Update API calls (POST with JSON bodies for interface-based)
- [ ] Update authentication flow
- [ ] Test end-to-end workflows

---

## Quick Reference Card

| Feature | DMVC | mORMot2 Method-Based | mORMot2 Interface-Based |
|---------|------|---------------------|------------------------|
| **Routing** | `[MVCPath('/api/user')]` | `GET /root/User` | `POST /root/Service.User` |
| **HTTP Methods** | `[MVCHTTPMethod([httpGET])]` | Manual check `if Ctxt.Method = mGET` | N/A (POST only) |
| **Parameters** | Method params | `Ctxt.InputUtf8['name']` | Method params (auto) |
| **Response** | `Render(obj)` | `Ctxt.Returns(json, status)` | `return obj` (auto) |
| **JSON** | Automatic | Manual `DynArraySaveJson()` | Automatic |
| **Auth** | `[MVCRequiresAuthentication]` | Manual `Ctxt.Session` check | `[ServiceMethodAuthenticationRequired]` |
| **Exceptions** | Auto HTTP error | Manual `try/except` | Auto HTTP error |
| **Database** | ActiveRecord (standalone) | TOrm + Server.Orm | TOrm + Server.Orm |
| **Client Type** | Any REST client | Any REST client | mORMot2 client (proxy) |

---

## Additional Resources

- **mORMot2 Documentation**: [https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html](https://synopse.info/files/html/Synopse%20mORMot%202%20Framework%20SAD%201.18.html)
- **Examples**: `/mnt/w/mORMot2/ex/`
  - `ex/ThirdPartyDemos/tbo/03-HttpServer-MethodServices` - Method-based services
  - `ex/ThirdPartyDemos/tbo/04-HttpServer-InterfaceServices` - Interface-based services
  - `ex/mvc-blog` - Complete MVC application with ORM
  - `ex/rest-websockets` - WebSocket chat example
- **DMVC Samples**: `/mnt/w/DMVCframework/samples/` (110 samples)

---

**End of Conversion Guide**

*For questions or issues, consult the mORMot2 forum: https://synopse.info/forum/*
