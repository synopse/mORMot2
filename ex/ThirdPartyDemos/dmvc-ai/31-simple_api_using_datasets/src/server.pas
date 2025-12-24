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
  /// Dataset-based API server
  TCustomerDataApiServer = class
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

{ TCustomerDataApiServer }

constructor TCustomerDataApiServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  fModel := TOrmModel.Create([TCustomerOrm], 'customerdata');
  fServer := TRestServerDB.Create(fModel, 'customerdata.db3');
  fServer.NoAjaxJson := false;
  fServer.CreateMissingTables;

  InitializeDatabase;

  // Register service using class-based approach
  fServer.ServiceDefine(TCustomerDataApiService, [ICustomerDataApi], sicShared);

  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TCustomerDataApiServer.Destroy;
begin
  Stop;
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TCustomerDataApiServer.InitializeDatabase;
var
  count: Integer;
begin
  // Check if database has data
  count := fServer.TableRowCount(TCustomerOrm);
  if count > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already contains % customers', [count]);
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Initializing database with sample customers');

  // Use direct SQL INSERT (dataset approach)
  fServer.Orm.ExecuteFmt(
    'INSERT INTO CustomerOrm (Code, CompanyName, City, Rating, Note) ' +
    'VALUES (%, %, %, %, %)', ['ACME', 'ACME Corporation', 'New York', 5, 'Premium customer']);

  fServer.Orm.ExecuteFmt(
    'INSERT INTO CustomerOrm (Code, CompanyName, City, Rating, Note) ' +
    'VALUES (%, %, %, %, %)', ['GLOBEX', 'Globex Corporation', 'Springfield', 4, 'Good customer']);

  fServer.Orm.ExecuteFmt(
    'INSERT INTO CustomerOrm (Code, CompanyName, City, Rating, Note) ' +
    'VALUES (%, %, %, %, %)', ['INITECH', 'Initech Inc.', 'Austin', 3, 'Regular customer']);

  TSynLog.Add.Log(sllInfo, 'Sample customers created');
end;

procedure TCustomerDataApiServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Dataset-based API server started on port %',
    [fHttpServer.Port]);
end;

procedure TCustomerDataApiServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping Dataset-based API server');
end;

end.
