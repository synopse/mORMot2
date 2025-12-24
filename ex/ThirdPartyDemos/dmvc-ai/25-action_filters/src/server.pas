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
  mormot.soa.server,
  api.interfaces,
  action.filters;

type
  /// Main server class with action filters
  // Port of DMVC WebModuleU with TActionFiltersController
  TActionFiltersServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;

    // Action filter middleware
    fActionFilterMiddleware: TActionFilterMiddleware;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TRestServerFullMemory read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

uses
  api.impl;

{ TActionFiltersServer }

constructor TActionFiltersServer.Create(const aPort: RawUtf8);
var
  Factory: TServiceFactoryServer;
begin
  inherited Create;

  // Create data model (empty for service-only server)
  fModel := TOrmModel.Create([], 'actionfilters');

  // Create REST server
  fServer := TRestServerFullMemory.Create(fModel, 'root', {authentication=}false);

  // Register the API service interface
  // Port of DMVC: fEngine.AddController(TActionFiltersController)
  fServer.ServiceDefine(TActionFiltersApiService, [IActionFiltersApi], sicShared);

  // Get the service factory for per-service filtering
  Factory := fServer.Services.Info(TypeInfo(IActionFiltersApi)) as TServiceFactoryServer;

  // Set up action filters AT SERVICE LEVEL (not global)
  // Port of DMVC controller lifecycle and action filters:
  //   - MVCControllerAfterCreate
  //   - OnBeforeAction (validation) → OnMethodExecute
  //   - OnAfterAction (logging) → AddInterceptor
  //   - MVCControllerBeforeDestroy
  // REFACTORED: Pass factory to enable service-level hooks
  fActionFilterMiddleware := TActionFilterMiddleware.Create(fServer, Factory);

  TSynLog.Add.Log(sllInfo, 'Service-level action filters initialized (OnMethodExecute + Interceptor)');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS support
end;

destructor TActionFiltersServer.Destroy;
begin
  Stop;

  TSynLog.Add.Log(sllInfo, 'ActionFilterMiddleware being destroyed (equivalent to MVCControllerBeforeDestroy)');

  FreeAndNil(fActionFilterMiddleware);
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);

  inherited;
end;

procedure TActionFiltersServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server on port %',
    [fHttpServer.Port]);
end;

procedure TActionFiltersServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping HTTP server');
end;

end.
