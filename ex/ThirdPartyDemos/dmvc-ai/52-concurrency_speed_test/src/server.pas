unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
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
  /// Concurrency Test Server
  // Ports DMVC concurrency_speed_test to mORMot2
  // Demonstrates thread pooling and concurrent request handling
  TConcurrencyTestServer = class
  protected
    fRestServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8 = '9999'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TConcurrencyTestServer }

constructor TConcurrencyTestServer.Create(const aPort: RawUtf8);
begin
  // Create in-memory REST server (no database needed for this test)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register the API implementation
  // Pass class, not instance - mORMot2 will instantiate it
  fRestServer.ServiceDefine(TConcurrencyTestApi, [IConcurrencyTestApi], sicShared);

  // Create HTTP server with thread pooling
  // Port of DMVC: MaxConnections = 0, ListenQueue = 500
  // mORMot2: threaded=32 provides thread pool for concurrent requests
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'Concurrency Test Server created on port %', [aPort]);
  TSynLog.Add.Log(sllInfo, 'Thread pool size: 32 threads');
end;

destructor TConcurrencyTestServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TConcurrencyTestServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Concurrency Test Server...');
end;

procedure TConcurrencyTestServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Concurrency Test Server...');
end;


end.
