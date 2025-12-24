unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.orm.core,
  mormot.orm.sqlite3,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  entities,
  api.interfaces,
  api.impl;

type
  /// Articles CRUD Server
  // Ports DMVC articles_crud_server to mORMot2
  TArticlesCrudServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    fArticlesApi: IArticlesApi;
    /// Initialize database with sample data if empty
    procedure InitializeSampleData;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'articles.db'); reintroduce;
    destructor Destroy; override;
    /// Start the HTTP server
    procedure Start;
    /// Stop the HTTP server
    procedure Stop;
  end;


implementation


{ TArticlesCrudServer }

constructor TArticlesCrudServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.Create(TOrmModel.Create([TOrmArticle]), aDbFileName);
  fRestServer.CreateMissingTables;  // Auto-create tables if needed

  // Create and register the Articles API implementation
  fArticlesApi := TArticlesApi.CreateApi(fRestServer.Orm);
  fRestServer.ServiceDefine(fArticlesApi, [IArticlesApi], sicShared);

  // Create HTTP server
  // Port of DMVC: LServer.MaxConnections := 1024; LServer.ListenQueue := 500;
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  // Initialize with sample data if database is empty
  InitializeSampleData;

  TSynLog.Add.Log(sllInfo, 'Articles CRUD Server created on port %', [aPort]);
end;

destructor TArticlesCrudServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fArticlesApi := nil;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TArticlesCrudServer.InitializeSampleData;
var
  count: Integer;
  article: TOrmArticle;
begin
  // Check if database has data
  count := fRestServer.Orm.TableRowCount(TOrmArticle);
  if count > 0 then
  begin
    TSynLog.Add.Log(sllInfo, 'Database already has % articles', [count]);
    Exit;
  end;

  TSynLog.Add.Log(sllInfo, 'Initializing database with sample articles...');

  // Add sample articles (similar to DMVC data)
  article := TOrmArticle.Create;
  try
    article.Code := 'C001';
    article.Description := 'Margherita Pizza';
    article.Price := 6.50;
    article.CreatedAt := Now;
    article.UpdatedAt := Now;
    fRestServer.Orm.Add(article, true);

    article.Code := 'C002';
    article.Description := 'Pepperoni Pizza';
    article.Price := 8.50;
    article.CreatedAt := Now;
    article.UpdatedAt := Now;
    fRestServer.Orm.Add(article, true);

    article.Code := 'C003';
    article.Description := 'Hawaiian Pizza';
    article.Price := 9.00;
    article.CreatedAt := Now;
    article.UpdatedAt := Now;
    fRestServer.Orm.Add(article, true);

    article.Code := 'C100';
    article.Description := 'Quattro Formaggi Pizza';
    article.Price := 10.50;
    article.CreatedAt := Now;
    article.UpdatedAt := Now;
    fRestServer.Orm.Add(article, true);

    article.Code := 'C1000';
    article.Description := 'Vegetariana Pizza';
    article.Price := 7.50;
    article.CreatedAt := Now;
    article.UpdatedAt := Now;
    fRestServer.Orm.Add(article, true);

    TSynLog.Add.Log(sllInfo, 'Sample data initialized: 5 articles added');
  finally
    article.Free;
  end;
end;

procedure TArticlesCrudServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Articles CRUD Server...');
end;

procedure TArticlesCrudServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Articles CRUD Server...');
end;


end.
