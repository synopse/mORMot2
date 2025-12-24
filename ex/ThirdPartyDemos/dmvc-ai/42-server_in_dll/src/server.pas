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
  api.impl;

type
  /// REST server for server_in_dll sample
  TDllRestServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TDllRestServer }

constructor TDllRestServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create model
  fModel := TOrmModel.Create([]);

  // Create server
  fServer := TRestServerFullMemory.Create(fModel, false);
  fServer.CreateMissingTables;

  // Register interface-based service
  fServer.ServiceDefine(TMainApi, [IMainApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useBidirSocket);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for development
end;

destructor TDllRestServer.Destroy;
begin
  Stop;
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TDllRestServer.Start;
begin
  if Assigned(fHttpServer) then
    TSynLog.Add.Log(sllInfo, 'REST server started on port %', [fHttpServer.Port]);
end;

procedure TDllRestServer.Stop;
begin
  if Assigned(fHttpServer) then
    TSynLog.Add.Log(sllInfo, 'REST server stopping');
end;

end.
