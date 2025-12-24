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
  /// ActiveRecord RESTful CRUD server
  // Demonstrates mORMot2 ORM with SQLite storage
  // Similar to DMVC's ActiveRecord sample with database backend
  TActiveRecordServer = class
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

{ TActiveRecordServer }

constructor TActiveRecordServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  // Create ORM model with our entities
  fModel := TOrmModel.Create([TPersonOrm, TArticleOrm, TPhoneOrm], 'activerecord');

  // Create SQLite-based REST server
  fServer := TRestServerDB.Create(fModel, 'activerecord.db3');
  fServer.NoAjaxJson := false;
  fServer.CreateMissingTables; // Auto-create tables if needed

  // Initialize with sample data
  InitializeDatabase;

  // Register the ActiveRecord API service
  fServer.ServiceDefine(TActiveRecordApiService, [IActiveRecordApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fServer], '+', useHttpAsync);
  fHttpServer.AccessControlAllowOrigin := '*';
end;

destructor TActiveRecordServer.Destroy;
begin
  Stop;
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  inherited;
end;

procedure TActiveRecordServer.InitializeDatabase;
var
  person: TPersonOrm;
  article: TArticleOrm;
  phone: TPhoneOrm;
  count: Integer;
begin
  // Check if database already has data
  count := fServer.TableRowCount(TPersonOrm);
  if count > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already contains % people', [count]);
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Initializing database with sample data');

  // Add sample people
  person := TPersonOrm.Create;
  try
    person.FirstName := 'John';
    person.LastName := 'Doe';
    person.DOB := EncodeDate(1990, 5, 15);
    person.IsMale := true;
    person.Note := 'Sample person 1';
    fServer.Add(person, true);

    person.FirstName := 'Jane';
    person.LastName := 'Smith';
    person.DOB := EncodeDate(1985, 8, 22);
    person.IsMale := false;
    person.Note := 'Sample person 2';
    fServer.Add(person, true);
  finally
    person.Free;
  end;

  // Add sample articles
  article := TArticleOrm.Create;
  try
    article.Description := 'Laptop Computer';
    article.Price := 1299.99;
    fServer.Add(article, true);

    article.Description := 'Wireless Mouse';
    article.Price := 29.99;
    fServer.Add(article, true);

    article.Description := 'USB-C Cable';
    article.Price := 12.50;
    fServer.Add(article, true);
  finally
    article.Free;
  end;

  // Add sample phones
  phone := TPhoneOrm.Create;
  try
    phone.PersonID := 1; // John Doe
    phone.PhoneNumber := '+1-555-1234';
    phone.NumberType := 'mobile';
    fServer.Add(phone, true);

    phone.PersonID := 1; // John Doe
    phone.PhoneNumber := '+1-555-5678';
    phone.NumberType := 'work';
    fServer.Add(phone, true);

    phone.PersonID := 2; // Jane Smith
    phone.PhoneNumber := '+1-555-9999';
    phone.NumberType := 'mobile';
    fServer.Add(phone, true);
  finally
    phone.Free;
  end;

  TSynLog.Add.Log(sllInfo, 'Sample data created successfully');
end;

procedure TActiveRecordServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'ActiveRecord CRUD server started on port %',
    [fHttpServer.Port]);
end;

procedure TActiveRecordServer.Stop;
begin
  if fHttpServer <> nil then
    TSynLog.Add.Log(sllInfo, 'Stopping ActiveRecord CRUD server');
end;

end.
