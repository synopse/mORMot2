unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.sqlite3,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.rest.memserver,
  mormot.soa.core,
  mormot.soa.server,
  mormot.net.http,
  entities,
  api.interfaces,
  api.impl;

type
  /// React sample HTTP server
  // Port of DMVC TwmMain and TIdHTTPWebBrokerBridge to mORMot2
  // Provides RESTful API with CORS support for React frontend
  TReactServer = class
  private
    fModel: TOrmModel;
    fRest: TRestServerDB;
    fHttpServer: TRestHttpServer;
    fPort: RawUtf8;
    /// Custom request handler for RESTful routing
    function OnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
    /// Handle CORS preflight requests
    procedure HandleCors(Ctxt: THttpServerRequestAbstract);
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
  end;


implementation


{ TReactServer }

constructor TReactServer.Create(const aPort: RawUtf8);
begin
  inherited Create;
  fPort := aPort;

  // Create SQLite3 database with ORM model
  fModel := TOrmModel.Create([TOrmCustomer], 'customers');
  fRest := TRestServerDB.Create(fModel, ChangeFileExt(Executable.ProgramFileName, '.db'), false);
  fRest.NoAjaxJson := false; // Enable JSON format
  fRest.CreateMissingTables;

  // Register interface-based service
  fRest.ServiceDefine(TCustomersApi, [ICustomersApi], sicShared);

  // Add sample data if database is empty
  if fRest.Orm.TableRowCount(TOrmCustomer) = 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Initializing database with sample customers');
    var customer := TOrmCustomer.Create;
    try
      customer.Code := 'C001';
      customer.Description := 'Acme Corporation';
      customer.City := 'New York';
      customer.Note := 'Main customer';
      customer.Rating := 5;
      fRest.Orm.Add(customer, true);

      customer.Code := 'C002';
      customer.Description := 'Tech Solutions Inc';
      customer.City := 'San Francisco';
      customer.Note := 'Technology partner';
      customer.Rating := 4;
      fRest.Orm.Add(customer, true);

      customer.Code := 'C003';
      customer.Description := 'Global Traders';
      customer.City := 'London';
      customer.Note := 'International client';
      customer.Rating := 3;
      fRest.Orm.Add(customer, true);
    finally
      customer.Free;
    end;
  end;
end;

destructor TReactServer.Destroy;
begin
  fHttpServer.Free;
  fRest.Free;
  fModel.Free;
  inherited;
end;

procedure TReactServer.HandleCors(Ctxt: THttpServerRequestAbstract);
begin
  // Port of TMVCCORSMiddleware functionality
  Ctxt.OutCustomHeaders :=
    'Access-Control-Allow-Origin: *'#13#10 +
    'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS'#13#10 +
    'Access-Control-Allow-Headers: Content-Type, Authorization'#13#10 +
    'Access-Control-Max-Age: 86400';
end;

function TReactServer.OnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
var
  method, url, path, idStr: RawUtf8;
  id: TID;
  customerDto: TCustomerDto;
  customers: TCustomerDtos;
  api: ICustomersApi;
  json: RawUtf8;
begin
  // Always add CORS headers
  HandleCors(Ctxt);

  method := UpperCase(Ctxt.Method);
  url := Ctxt.Url;

  // Handle OPTIONS for CORS preflight
  if method = 'OPTIONS' then
  begin
    result := HTTP_SUCCESS;
    exit;
  end;

  TSynLog.Add.Log(sllDebug, '% %', [method, url]);

  // Get the API service instance
  if not fRest.Services.Resolve(ICustomersApi, api) then
  begin
    result := HTTP_SERVERERROR;
    Ctxt.OutContent := 'Service not available';
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    exit;
  end;

  try
    // Parse RESTful routes: /api/customers[/:id]
    if not IdemPChar(pointer(url), '/API/CUSTOMERS') then
    begin
      result := HTTP_NOTFOUND;
      Ctxt.OutContent := '{"error":"Not found"}';
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      exit;
    end;

    // Extract path after /api/customers
    path := copy(url, 16, MaxInt); // Skip '/api/customers'
    if (path <> '') and (path[1] = '/') then
      Delete(path, 1, 1);

    // Route: GET /api/customers - Get all customers
    if (method = 'GET') and (path = '') then
    begin
      customers := api.GetAll;
      json := DynArraySaveJson(customers, TypeInfo(TCustomerDtos));
      Ctxt.OutContent := json;
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_SUCCESS;
    end
    // Route: GET /api/customers/:id - Get customer by ID
    else if (method = 'GET') and (path <> '') then
    begin
      idStr := path;
      id := GetInt64(pointer(idStr));
      if id = 0 then
      begin
        result := HTTP_BADREQUEST;
        Ctxt.OutContent := '{"error":"Invalid ID"}';
        Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
        exit;
      end;

      customerDto := api.GetById(id);
      json := RecordSaveJson(customerDto, TypeInfo(TCustomerDto));
      Ctxt.OutContent := json;
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_SUCCESS;
    end
    // Route: POST /api/customers - Create customer
    else if (method = 'POST') and (path = '') then
    begin
      RecordLoadJson(customerDto, Ctxt.InContent, TypeInfo(TCustomerDto));
      id := api.CreateCustomer(customerDto);
      Ctxt.OutContent := FormatUtf8('{"id":%}', [id]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_CREATED;
    end
    // Route: PUT /api/customers/:id - Update customer
    else if (method = 'PUT') and (path <> '') then
    begin
      idStr := path;
      id := GetInt64(pointer(idStr));
      if id = 0 then
      begin
        result := HTTP_BADREQUEST;
        Ctxt.OutContent := '{"error":"Invalid ID"}';
        Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
        exit;
      end;

      RecordLoadJson(customerDto, Ctxt.InContent, TypeInfo(TCustomerDto));
      api.Update(id, customerDto);
      customerDto := api.GetById(id); // Return updated customer
      json := RecordSaveJson(customerDto, TypeInfo(TCustomerDto));
      Ctxt.OutContent := json;
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_SUCCESS;
    end
    // Route: DELETE /api/customers/:id - Delete customer
    else if (method = 'DELETE') and (path <> '') then
    begin
      idStr := path;
      id := GetInt64(pointer(idStr));
      if id = 0 then
      begin
        result := HTTP_BADREQUEST;
        Ctxt.OutContent := '{"error":"Invalid ID"}';
        Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
        exit;
      end;

      api.Delete(id);
      Ctxt.OutContent := '{"result":"register successfully deleted"}';
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTTP_SUCCESS;
    end
    else
    begin
      result := HTTP_NOTALLOWED;
      Ctxt.OutContent := '{"error":"Method not allowed"}';
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    end;

  except
    on E: EServiceException do
    begin
      result := HTTP_BADREQUEST;
      Ctxt.OutContent := FormatUtf8('{"error":"%"}', [E.Message]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      TSynLog.Add.Log(sllWarning, 'Service error: %', [E.Message], self);
    end;
    on E: Exception do
    begin
      result := HTTP_SERVERERROR;
      Ctxt.OutContent := FormatUtf8('{"error":"%"}', [E.Message]);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      TSynLog.Add.Log(sllError, 'Unexpected error: %', [E.Message], self);
    end;
  end;
end;

procedure TReactServer.Start;
begin
  // Create HTTP server with custom request handler
  fHttpServer := TRestHttpServer.Create(fPort, [fRest], '+', HTTP_DEFAULT_MODE);
  fHttpServer.AccessControlAllowOrigin := '*'; // Enable CORS

  // Register custom route handler for RESTful API
  fHttpServer.Route.Get('/api/customers', OnRequest);
  fHttpServer.Route.Get('/api/customers/<id>', OnRequest);
  fHttpServer.Route.Post('/api/customers', OnRequest);
  fHttpServer.Route.Put('/api/customers/<id>', OnRequest);
  fHttpServer.Route.Delete('/api/customers/<id>', OnRequest);
  fHttpServer.Route.Options('/api/customers', OnRequest);
  fHttpServer.Route.Options('/api/customers/<id>', OnRequest);

  TSynLog.Add.Log(sllInfo, 'HTTP server started on port %', [fPort]);
end;


end.
