unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.soa.core,
  entities,
  api.interfaces,
  api.impl;

type
  /// Basic Demo Server
  // Ports DMVC WebModule1 and RunServer to mORMot2
  TBasicDemoServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'basicdemo.db'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TBasicDemoServer }

constructor TBasicDemoServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmMessage], aDbFileName);

  // Register the API implementation (will be created by ServiceDefine)
  fRestServer.ServiceDefine(TBasicDemoApi, [IBasicDemoApi], sicShared);

  // Create HTTP server
  // Port of DMVC: LServer.MaxConnections := 0; LServer.ListenQueue := 200;
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'Basic Demo Server created on port %', [aPort]);
end;

destructor TBasicDemoServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TBasicDemoServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Basic Demo Server...');
end;

procedure TBasicDemoServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Basic Demo Server...');
end;


end.
