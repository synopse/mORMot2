unit server;

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.orm.core,
  mormot.soa.core,
  api.interfaces,
  api.impl,
  entities;

type
  TRendersSampleServer = class
  private
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8; const aDbFileName: TFileName);
    destructor Destroy; override;

    property RestServer: TRestServerDB read fRestServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

{ TRendersSampleServer }

constructor TRendersSampleServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  inherited Create;

  // Create REST server with ORM
  fRestServer := TRestServerDB.Create(
    TOrmModel.Create([TOrmPerson, TOrmCustomer]),
    aDbFileName
  );

  try
    // Create tables if needed
    fRestServer.CreateMissingTables;

    // Register the API service (will be created by ServiceDefine)
    fRestServer.ServiceDefine(TRendersSample, [IRendersSample], sicShared);

    // Create HTTP server
    fHttpServer := TRestHttpServer.Create(
      aPort,
      [fRestServer],
      '+',  // domain name
      HTTP_DEFAULT_MODE,
      32   // thread pool size
    );

    // Enable CORS for web clients
    fHttpServer.AccessControlAllowOrigin := '*';

    TSynLog.Add.Log(sllInfo, 'Server started on port % with database %',
      [aPort, aDbFileName]);
  except
    FreeAndNil(fRestServer);
    raise;
  end;
end;

destructor TRendersSampleServer.Destroy;
begin
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

end.
