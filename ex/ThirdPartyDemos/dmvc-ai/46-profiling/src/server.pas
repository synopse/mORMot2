unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.perf,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces,
  api.impl;

type
  /// Profiling Demo Server
  // Demonstrates mORMot2 performance monitoring capabilities
  TProfilingServer = class
  protected
    fRestServer: TRestServerFullMemory;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8 = '8080'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TProfilingServer }

constructor TProfilingServer.Create(const aPort: RawUtf8);
begin
  // Create in-memory REST server (no database needed for this demo)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register the profiling API implementation as a service
  fRestServer.ServiceDefine(TProfilingApi, [IProfilingApi], sicShared);

  // Create HTTP server with profiling-friendly settings
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'Profiling Server created on port % with high-res timing', [aPort]);
end;

destructor TProfilingServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TProfilingServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Profiling Server...');
  TSynLog.Add.Log(sllInfo, 'Profiling enabled - check log file for timing details');
end;

procedure TProfilingServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Profiling Server...');
end;


end.
