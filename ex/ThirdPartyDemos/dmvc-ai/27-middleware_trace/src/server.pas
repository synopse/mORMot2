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
  middleware.trace;

type
  TTraceServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fTraceMiddleware: TTraceMiddleware;
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

{ TTraceServer }

constructor TTraceServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  fModel := TOrmModel.Create([], 'trace');
  fServer := TRestServerFullMemory.Create(fModel, 'root', {authentication=}false);
  fServer.ServiceDefine(TTraceApiService, [ITraceApi], sicShared);

  // Add trace middleware
  fTraceMiddleware := TTraceMiddleware.Create(fServer, 1024);

  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TTraceServer.Destroy;
begin
  Stop;
  FreeAndNil(fTraceMiddleware);
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TTraceServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server on port % with trace middleware',
    [fHttpServer.Port]);
end;

procedure TTraceServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping HTTP server');
end;

end.
