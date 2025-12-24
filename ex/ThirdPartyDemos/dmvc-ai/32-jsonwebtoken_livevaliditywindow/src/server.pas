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
  TJwtLiveValidityServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
    fJwtAuth: TJwtLiveValidityAuth;
  public
    constructor Create(const aPort: RawUtf8; const aJwtSecret: RawUtf8;
      aValidityWindowSeconds: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TJwtLiveValidityServer }

constructor TJwtLiveValidityServer.Create(const aPort: RawUtf8;
  const aJwtSecret: RawUtf8; aValidityWindowSeconds: Integer);
begin
  inherited Create;

  // Create REST server (in-memory, no database)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([], false, 'root');

  // Register interface-based services
  // Note: Role authorization is checked in the auth layer via sessions
  fRestServer.ServiceDefine(TPublicApi, [IPublicApi], sicShared);
  fRestServer.ServiceDefine(TAdminRole1Api, [IAdminRole1Api], sicShared);
  fRestServer.ServiceDefine(TAdminRole2Api, [IAdminRole2Api], sicShared);

  // Initialize JWT authentication with live validity window
  fJwtAuth := TJwtLiveValidityAuth.Create(fRestServer, aJwtSecret,
    aValidityWindowSeconds);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer], '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for testing

  TSynLog.Add.Log(sllInfo, 'JWT Live Validity server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, 'Token validity window: % seconds', [aValidityWindowSeconds]);
end;

destructor TJwtLiveValidityServer.Destroy;
begin
  fHttpServer.Free;
  fJwtAuth.Free;
  fRestServer.Free;
  inherited;
end;

procedure TJwtLiveValidityServer.Start;
begin
  // Register custom /login endpoint
  fRestServer.ServiceMethodRegister('auth/login',
    fJwtAuth.Login, false); // Not requires authentication

  TSynLog.Add.Log(sllInfo, 'Server started - Endpoints:');
  TSynLog.Add.Log(sllInfo, '  POST /root/auth/login - Login (get JWT token)');
  TSynLog.Add.Log(sllInfo, '  GET  /root/PublicApi/PublicInfo - Public (no auth)');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminRole1Api/ProtectedRole1 - Role1 required');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminRole1Api/ProtectedRole1Json - Role1 required');
  TSynLog.Add.Log(sllInfo, '  GET  /root/AdminRole2Api/ProtectedRole2 - Role2 required');
  WriteLn('JWT Live Validity Server started. Press Ctrl+C to stop.');
end;

procedure TJwtLiveValidityServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Server stopped');
end;

end.
