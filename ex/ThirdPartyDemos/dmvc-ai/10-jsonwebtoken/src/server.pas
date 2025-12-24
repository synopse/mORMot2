unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.net.server,
  auth,
  api.interfaces,
  api.impl;

type
  TJwtServer = class
  private
    fRestServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
    fAuth: TJwtAuthenticationServer;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TJwtServer }

constructor TJwtServer.Create(const aPort: RawUtf8);
begin
  TSynLog.Add.Log(sllInfo, 'Creating JWT server on port %', [aPort]);

  // Create REST server (no ORM needed, just services)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);
  try
    // Register services
    fRestServer.ServiceDefine(TPublicService, [IPublicService], sicShared);
    fRestServer.ServiceDefine(TProtectedService, [IProtectedService], sicShared);

    // Create JWT authentication
    fAuth := TJwtAuthenticationServer.Create(fRestServer, 'mys3cr37');

    // Register login endpoint (public, no auth)
    fRestServer.ServiceMethodRegisterPublishedMethods('auth', fAuth);

    // Create HTTP server
    fHttpServer := TRestHttpServer.Create(
      aPort,
      [fRestServer],
      '+',  // Domain name
      useHttpSocket
    );

    // Serve static files from www folder
    fHttpServer.RootRedirectToUri('index.html');
    // Note: Static file serving would need additional setup

    TSynLog.Add.Log(sllInfo, 'JWT server created successfully');
  except
    on E: Exception do
    begin
      TSynLog.Add.Log(sllError, 'Error creating server: %', [E.Message]);
      fRestServer.Free;
      raise;
    end;
  end;
end;

destructor TJwtServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying JWT server');
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

procedure TJwtServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting JWT server');
  // Server starts automatically in constructor
end;

procedure TJwtServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping JWT server');
  // Cleanup in destructor
end;

end.
