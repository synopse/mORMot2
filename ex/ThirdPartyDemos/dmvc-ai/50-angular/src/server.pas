unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.rest.core,
  mormot.net.http,
  mormot.soa.core,
  entities,
  api.interfaces,
  api.impl;

type
  /// Angular Sample Server
  // - Demonstrates mORMot2 REST backend for Angular frontend
  // - Includes CORS configuration for cross-origin requests
  TAngularSampleServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    function OnBeforeBody(var aUrl, aMethod, aInHeaders, aInContentType,
      aRemoteIP: RawUtf8; aContentLength: integer;
      aFlags: THttpServerRequestFlags): cardinal;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'angular_customers.db'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
    /// seed the database with sample data
    procedure SeedDatabase;
  end;


implementation


{ TAngularSampleServer }

constructor TAngularSampleServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  TSynLog.Add.Log(sllInfo, 'Creating Angular Sample Server...');

  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmCustomer], aDbFileName);
  TSynLog.Add.Log(sllInfo, 'Database initialized: %', [aDbFileName]);

  // Create and register the API implementation (sicShared - one instance for all clients)
  fRestServer.ServiceDefine(TCustomerApi, [ICustomerApi], sicShared);
  TSynLog.Add.Log(sllInfo, 'Customer API registered');

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);

  // Configure CORS for Angular development
  // Allow requests from typical Angular dev server (http://localhost:4200)
  // TODO: Fix CORS implementation - OnBeforeBody signature changed
  // fHttpServer.HttpServer.OnBeforeBody := OnBeforeBody;

  TSynLog.Add.Log(sllInfo, 'Server created on port %', [aPort]);
end;

destructor TAngularSampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Destroying Angular Sample Server...');
  fHttpServer.Free;
  fRestServer.Free;
  inherited;
end;

function TAngularSampleServer.OnBeforeBody(var aUrl, aMethod, aInHeaders,
  aInContentType, aRemoteIP: RawUtf8; aContentLength: integer;
  aFlags: THttpServerRequestFlags): cardinal;
var
  origin: RawUtf8;
  corsHeaders: RawUtf8;
begin
  // Extract Origin header
  FindNameValue(aInHeaders, 'ORIGIN:', origin);

  // Build CORS headers
  // Allow requests from localhost:4200 (Angular dev server) and others
  corsHeaders := 'Access-Control-Allow-Origin: *'#13#10 +
                 'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS'#13#10 +
                 'Access-Control-Allow-Headers: Content-Type, Authorization, X-Requested-With'#13#10 +
                 'Access-Control-Max-Age: 86400';

  // Handle preflight OPTIONS request
  if aMethod = 'OPTIONS' then
  begin
    aInHeaders := corsHeaders;
    result := HTTP_SUCCESS; // 200 OK
    Exit;
  end;

  // Add CORS headers to all responses
  aInHeaders := corsHeaders + #13#10 + aInHeaders;

  result := 0; // Continue processing
end;

procedure TAngularSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting HTTP server...');
  // Server is already started in constructor
end;

procedure TAngularSampleServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping HTTP server...');
  // Server will be stopped in destructor
end;

procedure TAngularSampleServer.SeedDatabase;
var
  customer: TOrmCustomer;
  i: integer;
const
  SAMPLE_DATA: array[0..4] of record
    FirstName, LastName, Email, Phone, City, Country: RawUtf8;
  end = (
    (FirstName: 'John'; LastName: 'Doe'; Email: 'john.doe@example.com';
     Phone: '+1-555-1234'; City: 'New York'; Country: 'USA'),
    (FirstName: 'Jane'; LastName: 'Smith'; Email: 'jane.smith@example.com';
     Phone: '+1-555-5678'; City: 'Los Angeles'; Country: 'USA'),
    (FirstName: 'Bob'; LastName: 'Johnson'; Email: 'bob.johnson@example.com';
     Phone: '+1-555-9012'; City: 'Chicago'; Country: 'USA'),
    (FirstName: 'Alice'; LastName: 'Williams'; Email: 'alice.williams@example.com';
     Phone: '+44-20-1234-5678'; City: 'London'; Country: 'UK'),
    (FirstName: 'Charlie'; LastName: 'Brown'; Email: 'charlie.brown@example.com';
     Phone: '+33-1-23-45-67-89'; City: 'Paris'; Country: 'France')
  );
begin
  TSynLog.Add.Log(sllInfo, 'Seeding database with sample data...');

  // Check if database already has data
  if fRestServer.Orm.TableRowCount(TOrmCustomer) > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already contains data, skipping seed');
    Exit;
  end;

  // Add sample customers
  for i := Low(SAMPLE_DATA) to High(SAMPLE_DATA) do
  begin
    customer := TOrmCustomer.Create;
    try
      customer.FirstName := SAMPLE_DATA[i].FirstName;
      customer.LastName := SAMPLE_DATA[i].LastName;
      customer.Email := SAMPLE_DATA[i].Email;
      customer.Phone := SAMPLE_DATA[i].Phone;
      customer.City := SAMPLE_DATA[i].City;
      customer.Country := SAMPLE_DATA[i].Country;

      if fRestServer.Orm.Add(customer, true) = 0 then
        TSynLog.Add.Log(sllWarning, 'Failed to add customer: %', [customer.Email])
      else
        TSynLog.Add.Log(sllInfo, 'Added customer: % %', [customer.FirstName, customer.LastName]);
    finally
      customer.Free;
    end;
  end;

  TSynLog.Add.Log(sllInfo, 'Database seeding completed');
end;


end.
