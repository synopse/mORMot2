unit server;

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.http.server,
  api.interfaces,
  api.impl;

type
  TCustomLoggerServer = class
  private
    fHttpServer: TRestHttpServer;
    fRestServer: TRestServerFullMemory;
  public
    constructor Create(const aPort: string);
    destructor Destroy; override;
    procedure Start;
    property RestServer: TRestServerFullMemory read fRestServer;
  end;

implementation

{ TCustomLoggerServer }

constructor TCustomLoggerServer.Create(const aPort: string);
begin
  inherited Create;

  TCustomSynLog.Log(sllInfo, 'Creating REST server on port %', [aPort]);

  // Create REST server
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register service
  fRestServer.ServiceDefine(TApiService, [IApiService], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer], '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*';

  TCustomSynLog.Log(sllInfo, 'Server created successfully');
end;

destructor TCustomLoggerServer.Destroy;
begin
  TCustomSynLog.Log(sllInfo, 'Shutting down server');

  fHttpServer.Free;
  fRestServer.Free;

  inherited;
end;

procedure TCustomLoggerServer.Start;
begin
  TCustomSynLog.Log(sllInfo, 'Starting HTTP server');
end;

end.
