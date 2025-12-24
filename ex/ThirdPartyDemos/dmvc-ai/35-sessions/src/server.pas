unit server;

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.soa.core;

type
  TSessionSampleServer = class
  private
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const port: RawUtf8);
    destructor Destroy; override;
    procedure Start;
  end;

implementation

uses
  SysUtils,
  mormot.core.interfaces,
  mormot.db.raw.sqlite3,
  api.interfaces,
  api.impl;

{ TSessionSampleServer }

constructor TSessionSampleServer.Create(const port: RawUtf8);
var
  model: TOrmModel;
begin
  // Create ORM model
  model := TOrmModel.Create([TAuthUser, TAuthGroup], 'root');

  // Create REST server with in-memory database
  fRestServer := TRestServerDB.Create(model, SQLITE_MEMORY_DATABASE_NAME);
  fRestServer.CreateMissingTables;

  // Setup authentication
  fRestServer.AuthenticationRegister(TRestServerAuthenticationDefault);

  // Create test users
  with TAuthUser.Create do
  try
    LogonName := 'user1';
    PasswordPlain := 'pass1';  // Use PasswordPlain property for simple setup
    fRestServer.Orm.Add(TAuthUser(self), true);
  finally
    Free;
  end;

  with TAuthUser.Create do
  try
    LogonName := 'user2';
    PasswordPlain := 'pass2';
    fRestServer.Orm.Add(TAuthUser(self), true);
  finally
    Free;
  end;

  // Register interface-based services
  // Session API is shared (sicShared) - one instance for all users
  fRestServer.ServiceDefine(TSessionApi, [ISessionApi], sicShared);

  // Cart API is per-session (sicPerSession) - one instance per user session
  // This demonstrates how state can be maintained per user
  fRestServer.ServiceDefine(TCartApi, [ICartApi], sicPerSession);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(port, [fRestServer],
    '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*'; // Enable CORS

  TSynLog.Add.Log(sllInfo, 'Session sample server created on port %', [port]);
end;

destructor TSessionSampleServer.Destroy;
begin
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

procedure TSessionSampleServer.Start;
begin
  WriteLn('Session Management Sample Server');
  WriteLn('=================================');
  WriteLn;
  WriteLn('This sample demonstrates:');
  WriteLn('  1. Session storage (key-value pairs in session data)');
  WriteLn('  2. Per-session service instances (shopping cart)');
  WriteLn('  3. Session counters and atomic operations');
  WriteLn('  4. Active session listing');
  WriteLn;
  WriteLn('Test users:');
  WriteLn('  user1 / pass1');
  WriteLn('  user2 / pass2');
  WriteLn;
  WriteLn('Available endpoints:');
  WriteLn;
  WriteLn('Authentication:');
  WriteLn('  POST /root/Auth?UserName=user1 - Login and get session');
  WriteLn;
  WriteLn('Session API (sicShared - shared instance):');
  WriteLn('  POST /root/SessionApi.SetValue - Store value');
  WriteLn('  POST /root/SessionApi.GetValue - Get value');
  WriteLn('  POST /root/SessionApi.ListKeys - List keys');
  WriteLn('  POST /root/SessionApi.DeleteValue - Delete value');
  WriteLn('  POST /root/SessionApi.ClearSession - Clear session');
  WriteLn('  POST /root/SessionApi.SessionInfo - Get session info');
  WriteLn('  POST /root/SessionApi.ListActiveSessions - List all sessions');
  WriteLn('  POST /root/SessionApi.IncrementCounter - Atomic increment');
  WriteLn;
  WriteLn('Cart API (sicPerSession - one instance per session):');
  WriteLn('  POST /root/CartApi.AddItem - Add item to cart');
  WriteLn('  POST /root/CartApi.RemoveItem - Remove item');
  WriteLn('  POST /root/CartApi.GetCart - Get cart contents');
  WriteLn('  POST /root/CartApi.ClearCart - Clear cart');
  WriteLn('  POST /root/CartApi.GetTotal - Get cart total');
  WriteLn;
  WriteLn('Example workflow:');
  WriteLn('================');
  WriteLn;
  WriteLn('1. Login as user1:');
  WriteLn('   curl -X POST "http://localhost:8080/root/Auth?UserName=user1" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{"UserName":"user1","PassWord":"pass1"}''');
  WriteLn;
  WriteLn('2. Store session data:');
  WriteLn('   curl -X POST "http://localhost:8080/root/SessionApi.SetValue" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{"key":"name","value":"John"}''');
  WriteLn;
  WriteLn('3. Add items to cart (sicPerSession):');
  WriteLn('   curl -X POST "http://localhost:8080/root/CartApi.AddItem" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{"itemId":"item1","quantity":2}''');
  WriteLn;
  WriteLn('4. Get cart (will be empty for user2, filled for user1):');
  WriteLn('   curl -X POST "http://localhost:8080/root/CartApi.GetCart" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{}''');
  WriteLn;
  WriteLn('5. Increment counter (atomic operation):');
  WriteLn('   curl -X POST "http://localhost:8080/root/SessionApi.IncrementCounter" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{"key":"page_views"}''');
  WriteLn;
  WriteLn('Try logging in as both user1 and user2 to see isolated sessions!');
end;

end.
