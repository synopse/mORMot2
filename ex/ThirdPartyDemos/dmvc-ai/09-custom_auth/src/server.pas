unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.log,
  mormot.core.data,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  api.interfaces,
  api.impl,
  auth.handler;

type
  /// Custom authentication sample server
  TCustomAuthSampleServer = class
  private
    fModel: TOrmModel;
    fServer: TCustomAuthServer;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    property Server: TCustomAuthServer read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

{ TCustomAuthSampleServer }

constructor TCustomAuthSampleServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create ORM model (empty for this example, but required)
  fModel := TOrmModel.Create([], 'root');

  // Create REST server with custom authentication
  fServer := TCustomAuthServer.Create(fModel, 'root');

  // Register service interfaces
  fServer.ServiceDefine(TPublicApi, [IPublicApi], sicShared);
  fServer.ServiceDefine(TPrivateApi, [IPrivateApi], sicShared);

  // Wire up custom authorization callbacks (must be called after ServiceDefine)
  fServer.SetupServiceAuthorization;

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

destructor TCustomAuthSampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Shutting down server...');
  fHttpServer.Free;
  fServer.Free;
  fModel.Free;
  inherited;
end;

procedure TCustomAuthSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Server started successfully');
  WriteLn('Server is now running...');
end;

end.
