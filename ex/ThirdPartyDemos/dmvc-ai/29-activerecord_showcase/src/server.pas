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
  TActiveRecordShowcaseServer = class
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

{ TActiveRecordShowcaseServer }

constructor TActiveRecordShowcaseServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  fModel := TOrmModel.Create([TCustomerOrm, TArticleOrm, TOrderOrm], 'showcase');
  fServer := TRestServerDB.Create(fModel, 'showcase.db3');
  fServer.NoAjaxJson := false;
  fServer.CreateMissingTables;

  InitializeDatabase;

  fServer.ServiceDefine(TActiveRecordShowcaseService,
    [IActiveRecordShowcaseApi], sicShared);

  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TActiveRecordShowcaseServer.Destroy;
begin
  Stop;
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TActiveRecordShowcaseServer.InitializeDatabase;
var
  customer: TCustomerOrm;
  article: TArticleOrm;
  order: TOrderOrm;
  count: Integer;
begin
  count := fServer.TableRowCount(TCustomerOrm);
  if count > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already initialized with % customers', [count]);
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Initializing showcase database with sample data');

  // Create sample customers
  customer := TCustomerOrm.Create;
  try
    customer.Code := 'ACME';
    customer.CompanyName := 'ACME Corporation';
    customer.City := 'New York';
    customer.Rating := 5;
    customer.Note := 'Premium customer';
    customer.LastContact := Now - 5;
    customer.IsActive := true;
    fServer.Add(customer, true);

    customer.Code := 'GLOBEX';
    customer.CompanyName := 'Globex Corporation';
    customer.City := 'Springfield';
    customer.Rating := 4;
    customer.Note := 'Good customer';
    customer.LastContact := Now - 10;
    customer.IsActive := true;
    fServer.Add(customer, true);

    customer.Code := 'INITECH';
    customer.CompanyName := 'Initech Inc.';
    customer.City := 'Austin';
    customer.Rating := 3;
    customer.Note := 'Regular customer';
    customer.LastContact := Now - 2;
    customer.IsActive := true;
    fServer.Add(customer, true);

    customer.Code := 'WAYNE';
    customer.CompanyName := 'Wayne Enterprises';
    customer.City := 'Gotham';
    customer.Rating := 5;
    customer.Note := 'VIP customer';
    customer.LastContact := Now - 1;
    customer.IsActive := true;
    fServer.Add(customer, true);

    customer.Code := 'OLD';
    customer.CompanyName := 'Old Inactive Corp';
    customer.City := 'Nowhere';
    customer.Rating := 2;
    customer.Note := 'Inactive';
    customer.LastContact := Now - 365;
    customer.IsActive := false;
    fServer.Add(customer, true);
  finally
    customer.Free;
  end;

  // Create sample articles
  article := TArticleOrm.Create;
  try
    article.Code := 'LAP001';
    article.Description := 'Laptop Computer';
    article.Price := 1299.99;
    article.Quantity := 25;
    fServer.Add(article, true);

    article.Code := 'MOU001';
    article.Description := 'Wireless Mouse';
    article.Price := 29.99;
    article.Quantity := 5; // Low stock
    fServer.Add(article, true);

    article.Code := 'KEY001';
    article.Description := 'Mechanical Keyboard';
    article.Price := 129.99;
    article.Quantity := 15;
    fServer.Add(article, true);

    article.Code := 'MON001';
    article.Description := '27" Monitor';
    article.Price := 399.99;
    article.Quantity := 3; // Low stock
    fServer.Add(article, true);
  finally
    article.Free;
  end;

  // Create sample orders
  order := TOrderOrm.Create;
  try
    order.CustomerID := 1; // ACME
    order.OrderDate := Now - 7;
    order.TotalAmount := 1299.99;
    order.Status := 'Completed';
    fServer.Add(order, true);

    order.CustomerID := 2; // GLOBEX
    order.OrderDate := Now - 3;
    order.TotalAmount := 529.98;
    order.Status := 'Shipped';
    fServer.Add(order, true);

    order.CustomerID := 1; // ACME again
    order.OrderDate := Now - 1;
    order.TotalAmount := 159.98;
    order.Status := 'Processing';
    fServer.Add(order, true);
  finally
    order.Free;
  end;

  TSynLog.Add.Log(sllInfo, 'Showcase sample data created successfully');
end;

procedure TActiveRecordShowcaseServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'ActiveRecord Showcase server started on port %',
    [fHttpServer.Port]);
end;

procedure TActiveRecordShowcaseServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping ActiveRecord Showcase server');
end;

end.
