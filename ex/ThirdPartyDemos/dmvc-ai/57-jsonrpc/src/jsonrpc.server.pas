unit jsonrpc.server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  jsonrpc.entities,
  jsonrpc.interfaces,
  jsonrpc.impl;

type
  /// JSON-RPC Server
  // - Demonstrates mORMot2's automatic JSON-RPC 2.0 support
  // - Multiple services exposed through interface-based services
  TJsonRpcServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    fCalculatorApi: ICalculatorService;
    fUserApi: IUserService;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'jsonrpc.db'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;

implementation

{ TJsonRpcServer }

constructor TJsonRpcServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  TSynLog.Add.Log(sllInfo, 'Creating JSON-RPC Server...');

  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3(
    [TOrmCalculation, TOrmUser], aDbFileName);
  TSynLog.Add.Log(sllInfo, 'Database initialized: %', [aDbFileName]);

  // Create and register Calculator service
  fCalculatorApi := TCalculatorService.Create(fRestServer.Orm);
  fRestServer.ServiceDefine(TCalculatorService, [ICalculatorService], sicShared);
  TSynLog.Add.Log(sllInfo, 'Calculator Service registered');

  // Create and register User service
  fUserApi := TUserService.Create(fRestServer.Orm);
  fRestServer.ServiceDefine(TUserService, [IUserService], sicShared);
  TSynLog.Add.Log(sllInfo, 'User Service registered');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins
  TSynLog.Add.Log(sllInfo, 'HTTP Server created on port %', [aPort]);

  TSynLog.Add.Log(sllInfo, 'JSON-RPC Server created successfully');
end;

destructor TJsonRpcServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying JSON-RPC Server...');
  Stop;
  fHttpServer.Free;
  fCalculatorApi := nil;
  fUserApi := nil;
  fRestServer.Free;
  TSynLog.Add.Log(sllInfo, 'JSON-RPC Server destroyed');
  inherited Destroy;
end;

procedure TJsonRpcServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'JSON-RPC Server started');
  TSynLog.Add.Log(sllInfo, 'Available endpoints (JSON-RPC 2.0):');
  TSynLog.Add.Log(sllInfo, '  Calculator Service:');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/Add');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/Subtract');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/Multiply');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/Divide');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/GetHistory');
  TSynLog.Add.Log(sllInfo, '    POST /CalculatorService/ClearHistory');
  TSynLog.Add.Log(sllInfo, '  User Service:');
  TSynLog.Add.Log(sllInfo, '    POST /UserService/CreateUser');
  TSynLog.Add.Log(sllInfo, '    POST /UserService/GetUser');
  TSynLog.Add.Log(sllInfo, '    POST /UserService/GetAllUsers');
  TSynLog.Add.Log(sllInfo, '    POST /UserService/DeleteUser');
end;

procedure TJsonRpcServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'JSON-RPC Server stopping...');
end;

end.
