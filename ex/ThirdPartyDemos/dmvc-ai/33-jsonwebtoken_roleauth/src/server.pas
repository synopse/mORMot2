unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.json,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  auth,
  api.interfaces,
  api.impl;

type
  TJwtRoleAuthServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
    fJwtAuth: TJwtAuthenticationServer;
  public
    constructor Create(const aPort: RawUtf8; const aJwtSecret: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TJwtRoleAuthServer }

constructor TJwtRoleAuthServer.Create(const aPort: RawUtf8;
  const aJwtSecret: RawUtf8);
begin
  inherited Create;

  // Create REST server (in-memory, no database)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([], false, 'root');

  // Register interface-based services with role-based authorization
  // Public service - no authentication required
  fRestServer.ServiceDefine(TPublicService, [IPublicService], sicShared);

  // User service - requires authentication (any role)
  fRestServer.ServiceDefine(TUserService, [IUserService], sicShared);

  // Manager service - role checking done in implementation
  fRestServer.ServiceDefine(TManagerService, [IManagerService], sicShared);

  // Admin service - role checking done in implementation
  fRestServer.ServiceDefine(TAdminService, [IAdminService], sicShared);

  // Multi-role service - complex role logic in implementation
  fRestServer.ServiceDefine(TMultiRoleService, [IMultiRoleService], sicShared);

  // Initialize JWT authentication
  fJwtAuth := TJwtAuthenticationServer.Create(fRestServer, aJwtSecret);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer], '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for testing

  TSynLog.Add.Log(sllInfo, 'JWT Role Auth server created on port %', [aPort]);
end;

destructor TJwtRoleAuthServer.Destroy;
begin
  fHttpServer.Free;
  fJwtAuth.Free;
  fRestServer.Free;
  inherited;
end;

procedure TJwtRoleAuthServer.Start;
begin
  // Register custom /login endpoint
  fRestServer.ServiceMethodRegister('auth/login',
    fJwtAuth.Login, false); // No authentication required for login

  TSynLog.Add.Log(sllInfo, 'Server started - Endpoints:');
  TSynLog.Add.Log(sllInfo, '  POST /root/auth/login - Login (get JWT with roles)');
  TSynLog.Add.Log(sllInfo, '  GET  /root/PublicService/GetPublicMessage - Public (no auth)');
  TSynLog.Add.Log(sllInfo, '  GET  /root/UserService/GetProfile - Any authenticated user');
  TSynLog.Add.Log(sllInfo, '  GET  /root/UserService/GetMyData - Any authenticated user');
  TSynLog.Add.Log(sllInfo, '  GET  /root/ManagerService/GetTeamInfo - Requires manager role');
  TSynLog.Add.Log(sllInfo, '  GET  /root/ManagerService/ApproveRequest?aRequestId=123 - Requires manager role');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminService/GetSystemInfo - Requires admin role');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminService/ManageUsers - Requires admin role');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminService/GetAllSessions - Requires admin role');
  TSynLog.Add.Log(sllInfo, '  GET  /root/MultiRoleService/GetFinancialReport - Requires manager AND reports roles');
  TSynLog.Add.Log(sllInfo, '  GET  /root/MultiRoleService/GetAuditLog - Requires admin OR auditor role');
  WriteLn('JWT Role-Based Auth Server started. Press Ctrl+C to stop.');
end;

procedure TJwtRoleAuthServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Server stopped');
end;

end.
