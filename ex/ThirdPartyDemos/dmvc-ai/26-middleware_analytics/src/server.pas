unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces,
  middleware.analytics;

type
  /// Main server class with analytics middleware
  // Port of DMVC TMyWebModule
  TAnalyticsServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fAnalyticsMiddleware: TAnalyticsMiddleware;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TRestServerFullMemory read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
    property Analytics: TAnalyticsMiddleware read fAnalyticsMiddleware;
  end;

implementation

uses
  api.impl;

{ TAnalyticsServer }

constructor TAnalyticsServer.Create(const aPort: RawUtf8);
var
  logPath: TFileName;
begin
  inherited Create;

  // Create data model (empty for service-only server)
  fModel := TOrmModel.Create([], 'analytics');

  // Create REST server
  // Port of DMVC: FMVC := TMVCEngine.Create(Self, ...)
  fServer := TRestServerFullMemory.Create(fModel, 'root', {authentication=}false);

  // Register the API service interface
  // Port of DMVC: .AddController(TMainController)
  fServer.ServiceDefine(TAnalyticsApiService, [IAnalyticsApi], sicShared);

  // Set up analytics middleware
  // Port of DMVC: .AddMiddleware(TMVCAnalyticsMiddleware.Create(GetAnalyticsDefaultLogger))
  logPath := GetSystemPath(spLog) + 'analytics' + PathDelim +
    FormatDateTime('yyyy-mm-dd', Now) + '.csv';
  fAnalyticsMiddleware := TAnalyticsMiddleware.Create(fServer, logPath);

  TSynLog.Add.Log(sllInfo, 'Analytics middleware configured - logging to: %', [logPath]);

  // Create HTTP server with CORS support
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TAnalyticsServer.Destroy;
begin
  Stop;

  FreeAndNil(fAnalyticsMiddleware);
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);

  inherited;
end;

procedure TAnalyticsServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server on port %', [fHttpServer.Port]);
  TSynLog.Add.Log(sllInfo, 'Analytics middleware active - requests will be logged to CSV');
end;

procedure TAnalyticsServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping HTTP server - total requests logged: %',
      [fAnalyticsMiddleware.RequestCount]);
end;

end.
