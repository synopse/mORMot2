unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  auth.handler;

type
  TCustomRoleAuthSampleServer = class
  private
    fModel: TOrmModel;
    fServer: TCustomRoleAuthServer;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property Server: TCustomRoleAuthServer read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

uses
  mormot.soa.server,
  api.interfaces,
  api.impl;

{ TCustomRoleAuthSampleServer }

constructor TCustomRoleAuthSampleServer.Create(const aPort: RawUtf8);
var
  factory: TServiceFactoryServerAbstract;
begin
  inherited Create;

  // Create ORM model (empty for this example, but required)
  fModel := TOrmModel.Create([], 'root');

  // Create REST server with custom authentication
  fServer := TCustomRoleAuthServer.Create(fModel, 'root');

  // Register service interfaces
  factory := fServer.ServiceDefine(TPrivateApi, [IPrivateApi], sicShared);
  if factory <> nil then
    (factory as TServiceFactoryServer).OnMethodExecute := fServer.CheckMethodAuthorization;

  // Authorization is handled by OnMethodExecute in TCustomRoleAuthServer

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    aPort,
    [fServer],
    '+',           // Domain name ('+' = all)
    useHttpAsync,  // High-performance async mode
    32,            // Thread pool size
    secNone        // No HTTPS for this example
  );

  // Configure CORS (allow all origins for this example)
  fHttpServer.AccessControlAllowOrigin := '*';

  TSynLog.Add.Log(sllInfo, 'Server created on port %', [aPort]);
end;

destructor TCustomRoleAuthSampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Shutting down server...');
  fHttpServer.Free;
  fServer.Free;
  fModel.Free;
  inherited;
end;

procedure TCustomRoleAuthSampleServer.Start;
begin
  TSynLog.Add.Log(sllDebug, 'Server started');
end;

end.
