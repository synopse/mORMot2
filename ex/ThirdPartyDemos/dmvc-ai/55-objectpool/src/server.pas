unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.log,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.http.server,
  mormot.soa.core,
  api.interfaces,
  api.impl;

type
  /// Object Pool Demo Server
  // Demonstrates object pooling patterns for performance
  TObjectPoolServer = class
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


{ TObjectPoolServer }

constructor TObjectPoolServer.Create(const aPort: RawUtf8);
begin
  // Create in-memory REST server (no database needed for this demo)
  fRestServer := TRestServerFullMemory.CreateWithOwnModel([]);

  // Register the object pool API as a singleton service
  // sicSingle ensures one pool instance is shared across all requests
  fRestServer.ServiceDefine(TObjectPoolApi, [IObjectPoolApi], sicSingle);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'ObjectPool Server created on port %', [aPort]);
end;

destructor TObjectPoolServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TObjectPoolServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'ObjectPool Server started');
end;

procedure TObjectPoolServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'ObjectPool Server stopped');
end;


end.
