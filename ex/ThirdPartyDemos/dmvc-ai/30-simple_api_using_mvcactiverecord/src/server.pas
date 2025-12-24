unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.orm.sqlite3,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.soa.core,
  entities,
  api.interfaces;

type
  /// Simple Customer API server
  TCustomerApiServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    procedure InitializeDatabase;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Server: TRestServerDB read fServer;
    property HttpServer: TRestHttpServer read fHttpServer;
  end;

implementation

uses
  api.impl;

{ TCustomerApiServer }

constructor TCustomerApiServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  fModel := TOrmModel.Create([TCustomerOrm], 'customers');
  fServer := TRestServerDB.Create(fModel, 'customers.db3');
  fServer.NoAjaxJson := false;
  fServer.CreateMissingTables;

  InitializeDatabase;

  fServer.ServiceDefine(TCustomerApiService, [ICustomerApi], sicShared);

  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TCustomerApiServer.Destroy;
begin
  Stop;
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TCustomerApiServer.InitializeDatabase;
var
  customer: TCustomerOrm;
  count: Integer;
begin
  count := fServer.TableRowCount(TCustomerOrm);
  if count > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already contains % customers', [count]);
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Initializing database with sample customers');

  customer := TCustomerOrm.Create;
  try
    customer.Code := 'ACME';
    customer.CompanyName := 'ACME Corporation';
    customer.City := 'New York';
    customer.Rating := 5;
    customer.Note := 'Premium customer';
    fServer.Add(customer, true);

    customer.Code := 'GLOBEX';
    customer.CompanyName := 'Globex Corporation';
    customer.City := 'Springfield';
    customer.Rating := 4;
    customer.Note := 'Good customer';
    fServer.Add(customer, true);

    customer.Code := 'INITECH';
    customer.CompanyName := 'Initech Inc.';
    customer.City := 'Austin';
    customer.Rating := 3;
    customer.Note := 'Regular customer';
    fServer.Add(customer, true);

    customer.Code := 'WAYNE';
    customer.CompanyName := 'Wayne Enterprises';
    customer.City := 'Gotham';
    customer.Rating := 5;
    customer.Note := 'VIP customer';
    fServer.Add(customer, true);
  finally
    customer.Free;
  end;

  TSynLog.Add.Log(sllInfo, 'Sample customers created');
end;

procedure TCustomerApiServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Customer API server started on port %',
    [fHttpServer.Port]);
end;

procedure TCustomerApiServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping Customer API server');
end;

end.
