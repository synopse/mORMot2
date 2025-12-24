unit server;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.soa.core,
  mormot.soa.server,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  entities,
  api.interfaces,
  api.impl;

type
  /// Complete example server integrating all mORMot2 features
  TCompleteExampleServer = class
  private
    fModel: TOrmModel;
    fServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    procedure InitializeDatabase;
    procedure CreateSampleData;
  public
    constructor Create(const aPort: RawUtf8);
    destructor Destroy; override;
    procedure Start;
  end;

implementation

{ TCompleteExampleServer }

constructor TCompleteExampleServer.Create(const aPort: RawUtf8);
begin
  inherited Create;

  TSynLog.Add.Log(sllInfo, 'Creating Complete Example Server on port %', [aPort]);

  // Create ORM model
  fModel := TOrmModel.Create([TArticle], 'root');

  // Create REST server with SQLite backend
  fServer := TRestServerDB.Create(fModel, ':memory:');
  fServer.CreateMissingTables;

  // Initialize database with indexes
  InitializeDatabase;

  // Register interface-based service
  fServer.ServiceDefine(TCompleteApiService, [ICompleteApi], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(
    aPort,
    [fServer],
    '+',
    useHttpApiRegisteringURI
  );

  TSynLog.Add.Log(sllInfo, 'Server created successfully');
end;

destructor TCompleteExampleServer.Destroy;
begin
  TSynLog.Add.Log(sllInfo, 'Shutting down Complete Example Server');

  fHttpServer.Free;
  fServer.Free;
  fModel.Free;

  inherited;
end;

procedure TCompleteExampleServer.InitializeDatabase;
begin
  TSynLog.Add.Log(sllTrace, 'Initializing database schema');

  // Create indexes for performance
  fServer.Server.CreateSqlMultiIndex(TArticle, ['Author', 'Published'], false);
  fServer.Server.CreateSqlIndex(TArticle, 'CreatedAt', false);

  TSynLog.Add.Log(sllDebug, 'Database indexes created');
end;

procedure TCompleteExampleServer.CreateSampleData;
var
  article: TArticle;
  i: Integer;
begin
  TSynLog.Add.Log(sllTrace, 'Creating sample data');

  for i := 1 to 10 do
  begin
    article := TArticle.Create;
    try
      article.Title := FormatUtf8('Sample Article #%', [i]);
      article.Author := FormatUtf8('Author %', [(i mod 3) + 1]);
      article.Content := FormatUtf8(
        'This is the content for sample article number %. ' +
        'It demonstrates the complete mORMot2 ORM capabilities.', [i]);
      article.CreatedAt := NowUtc;
      article.UpdatedAt := NowUtc;
      article.ViewCount := i * 10;
      article.Published := (i mod 2) = 0;

      fServer.Server.Add(article, true);
    finally
      article.Free;
    end;
  end;

  TSynLog.Add.Log(sllInfo, 'Created 10 sample articles');
end;

procedure TCompleteExampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Complete Example Server');

  // Create sample data
  CreateSampleData;

  TSynLog.Add.Log(sllInfo, 'Server started and ready to accept connections');
end;

end.
