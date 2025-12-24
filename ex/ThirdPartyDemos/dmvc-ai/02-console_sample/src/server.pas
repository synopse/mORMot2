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
  /// Console Sample Server
  // - Simple REST server with greeting API
  // - Demonstrates basic mORMot2 HTTP server functionality
  TConsoleSampleServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'console_sample.db'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TConsoleSampleServer }

constructor TConsoleSampleServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  TSynLog.Add.Log(sllInfo, 'Creating Console Sample Server...');

  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmGreeting], aDbFileName);
  TSynLog.Add.Log(sllInfo, 'Database initialized: %', [aDbFileName]);

  // Register the API implementation (will be created by ServiceDefine)
  fRestServer.ServiceDefine(TGreetingService, [IGreetingService], sicShared);
  TSynLog.Add.Log(sllInfo, 'Greeting Service registered');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins
  TSynLog.Add.Log(sllInfo, 'HTTP Server created on port %', [aPort]);

  TSynLog.Add.Log(sllInfo, 'Console Sample Server created successfully');
end;

destructor TConsoleSampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying Console Sample Server...');
  Stop;
  fHttpServer.Free;
  fRestServer.Free;
  TSynLog.Add.Log(sllInfo, 'Console Sample Server destroyed');
  inherited Destroy;
end;

procedure TConsoleSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Console Sample Server started');
  TSynLog.Add.Log(sllInfo, 'Available endpoints:');
  TSynLog.Add.Log(sllInfo, '  POST /GreetingService/CreateGreeting');
  TSynLog.Add.Log(sllInfo, '  GET  /GreetingService/GetGreeting?id=<n>');
  TSynLog.Add.Log(sllInfo, '  GET  /GreetingService/GetAllGreetings');
  TSynLog.Add.Log(sllInfo, '  POST /GreetingService/DeleteGreeting');
end;

procedure TConsoleSampleServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Console Sample Server stopping...');
end;


end.
