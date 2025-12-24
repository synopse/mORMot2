unit jsonrpc.client;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.http.client,
  jsonrpc.interfaces;

type
  /// JSON-RPC Client
  // - Demonstrates mORMot2's client-side JSON-RPC support
  // - Automatic interface proxy generation
  TJsonRpcClient = class
  protected
    fRestClient: TRestHttpClient;
    fCalculatorApi: ICalculatorService;
    fUserApi: IUserService;
  public
    constructor Create(const aServerUrl: RawUtf8 = 'localhost:8080'); reintroduce;
    destructor Destroy; override;
    /// get calculator service interface
    property Calculator: ICalculatorService read fCalculatorApi;
    /// get user service interface
    property User: IUserService read fUserApi;
  end;

implementation

{ TJsonRpcClient }

constructor TJsonRpcClient.Create(const aServerUrl: RawUtf8);
begin
  TSynLog.Add.Log(sllInfo, 'Creating JSON-RPC Client...');

  // Create HTTP client
  fRestClient := TRestHttpClientWinHttp.Create(aServerUrl, 8080, TOrmModel.Create([]));
  TSynLog.Add.Log(sllInfo, 'HTTP Client created for %', [aServerUrl]);

  // Get Calculator service interface proxy
  fRestClient.ServiceDefine([ICalculatorService], sicShared);
  fRestClient.Services.Resolve(ICalculatorService, fCalculatorApi);
  TSynLog.Add.Log(sllInfo, 'Calculator Service proxy created');

  // Get User service interface proxy
  fRestClient.ServiceDefine([IUserService], sicShared);
  fRestClient.Services.Resolve(IUserService, fUserApi);
  TSynLog.Add.Log(sllInfo, 'User Service proxy created');

  TSynLog.Add.Log(sllInfo, 'JSON-RPC Client created successfully');
end;

destructor TJsonRpcClient.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying JSON-RPC Client...');
  fCalculatorApi := nil;
  fUserApi := nil;
  fRestClient.Free;
  TSynLog.Add.Log(sllInfo, 'JSON-RPC Client destroyed');
  inherited Destroy;
end;

end.
