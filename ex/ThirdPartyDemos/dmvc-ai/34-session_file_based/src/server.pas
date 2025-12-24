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
  mormot.soa.core,
  mormot.rest.http.server;

type
  TFileBasedSessionServer = class
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
  api.impl,
  auth.handler;

{ TFileBasedSessionServer }

constructor TFileBasedSessionServer.Create(const port: RawUtf8);
var
  model: TOrmModel;
  auth: TRestServerAuthentication;
  user: TAuthUser;
begin
  // Create ORM model (we need at least TAuthUser and TAuthGroup for auth)
  model := TOrmModel.Create([TAuthUser, TAuthGroup], 'root');

  // Create REST server with in-memory database
  fRestServer := TRestServerDB.Create(model, SQLITE_MEMORY_DATABASE_NAME, false);
  fRestServer.CreateMissingTables;

  // Setup authentication with file-based session persistence
  auth := fRestServer.AuthenticationRegister(TFileBasedAuthHandler);

  // Report any existing persisted session files
  if auth is TFileBasedAuthHandler then
    TFileBasedAuthHandler(auth).ReportPersistedSessions;

  // Create a default user for testing
  user := TAuthUser.Create;
  try
    user.LogonName := 'user';
    user.PasswordHashHexa := TAuthUser.ComputeHashedPassword('user', 'password');
    fRestServer.Orm.Add(user, true);
  finally
    user.Free;
  end;

  // Register interface-based services
  fRestServer.ServiceDefine(TSessionApi, [ISessionApi], sicPerSession);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(port, [fRestServer],
    '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*'; // Enable CORS

  TSynLog.Add.Log(sllInfo, 'File-based session server created on port %', [port]);
end;

destructor TFileBasedSessionServer.Destroy;
begin
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

procedure TFileBasedSessionServer.Start;
begin
  WriteLn('File-Based Session Sample Server');
  WriteLn('=================================');
  WriteLn;
  WriteLn('Session files are stored in: ', Executable.ProgramFilePath, 'sessions\');
  WriteLn;
  WriteLn('** SESSION RESTORATION ON RESTART **');
  WriteLn('  - Session DATA is persisted by USER ID (not session ID)');
  WriteLn('  - After server restart, user re-authenticates');
  WriteLn('  - Previous session data is automatically restored');
  WriteLn('  - This matches how most real-world systems work');
  WriteLn;
  WriteLn('Authentication:');
  WriteLn('  Username: user');
  WriteLn('  Password: password');
  WriteLn;
  WriteLn('Available endpoints:');
  WriteLn('  POST /root/Auth?UserName=user - Authenticate and get session');
  WriteLn('  POST /root/SessionApi.SetValue - Store value in session');
  WriteLn('  POST /root/SessionApi.GetValue - Get value from session');
  WriteLn('  POST /root/SessionApi.ListKeys - List all session keys');
  WriteLn('  POST /root/SessionApi.DeleteValue - Delete value from session');
  WriteLn('  POST /root/SessionApi.ClearSession - Clear all session data');
  WriteLn('  POST /root/SessionApi.SessionInfo - Get session info');
  WriteLn;
  WriteLn('Test session persistence:');
  WriteLn('1. Login and set a value');
  WriteLn('2. Stop the server (Ctrl+C)');
  WriteLn('3. Restart the server');
  WriteLn('4. Login again - your previous data is restored!');
  WriteLn;
  WriteLn('Example workflow:');
  WriteLn('1. Login:');
  WriteLn('   curl -X POST "http://localhost:8080/root/Auth?UserName=user" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -d ''{"UserName":"user","PassWord":"password"}''');
  WriteLn;
  WriteLn('2. Set value (use session_signature from login):');
  WriteLn('   curl -X POST "http://localhost:8080/root/SessionApi.SetValue" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -d ''{"key":"name","value":"John Doe"}''');
  WriteLn;
  WriteLn('3. Get value:');
  WriteLn('   curl -X POST "http://localhost:8080/root/SessionApi.GetValue" \');
  WriteLn('        -H "Content-Type: application/json" \');
  WriteLn('        -H "Authorization: Bearer SESSION_ID+SIGNATURE" \');
  WriteLn('        -d ''{"key":"name"}''');
  WriteLn;
  WriteLn('Note: Session data persists across server restarts via user-keyed files.');
end;

end.
