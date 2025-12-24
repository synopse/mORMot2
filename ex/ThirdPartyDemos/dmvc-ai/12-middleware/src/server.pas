unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces,
  middleware;

type
  /// Main server class with middleware support
  // Port of DMVC WebModuleUnit1 with middleware chain
  TMiddlewareServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;

    // Middleware instances
    fCustomHeaderMiddleware: TCustomHeaderMiddleware;
    fUserAgentRedirectMiddleware: TUserAgentRedirectMiddleware;
    fRequestLoggerMiddleware: TRequestLoggerMiddleware;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TRestServerFullMemory read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
    property RequestLogger: TRequestLoggerMiddleware read fRequestLoggerMiddleware;
  end;

implementation

uses
  api.impl;

{ TMiddlewareServer }

constructor TMiddlewareServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create data model (empty for service-only server)
  fModel := TOrmModel.Create([], 'middleware');

  // Create REST server
  fServer := TRestServerFullMemory.Create(fModel, 'root', {authentication=}false);

  // Register the API service interface
  // Port of DMVC: fEngine.AddController(TApp1MainController)
  fServer.ServiceDefine(TMiddlewareApiService, [IMiddlewareApi], sicShared);

  // Set up middleware chain (registration order determines execution order!)
  // Each middleware saves the previous handler and calls it, forming a chain.
  // Last registered middleware executes FIRST, then calls the previous one.
  //
  // Port of DMVC middleware chain:
  //   .AddMiddleware(TMVCSalutationMiddleware.Create)
  //   .AddMiddleware(TMVCRedirectAndroidDeviceOnPlayStore.Create)

  // 1. Request logger - registers first (executes last in chain)
  //    Hooks both OnBeforeUri and OnAfterUri
  fRequestLoggerMiddleware := TRequestLoggerMiddleware.Create(fServer);

  // 2. User-Agent redirect - registers second (executes before logger)
  //    Hooks OnBeforeUri only - can short-circuit request processing
  fUserAgentRedirectMiddleware := TUserAgentRedirectMiddleware.Create(fServer);

  // 3. Custom headers - registers third (executes before logger for OnAfterUri)
  //    Hooks OnAfterUri only - adds headers to all responses
  fCustomHeaderMiddleware := TCustomHeaderMiddleware.Create(fServer);

  // Execution order:
  //   OnBeforeUri:  UserAgentRedirect -> Logger (if not redirected)
  //   OnAfterUri:   CustomHeaders -> Logger
  TSynLog.Add.Log(sllInfo, 'Middleware chain: OnBeforeUri=[UserAgent->Logger], OnAfterUri=[CustomHeaders->Logger]');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS support
end;

destructor TMiddlewareServer.Destroy;
begin
  Stop;

  FreeAndNil(fCustomHeaderMiddleware);
  FreeAndNil(fUserAgentRedirectMiddleware);
  FreeAndNil(fRequestLoggerMiddleware);
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);

  inherited;
end;

procedure TMiddlewareServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server on port %',
    [fHttpServer.Port]);
end;

procedure TMiddlewareServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping HTTP server');
end;

end.
