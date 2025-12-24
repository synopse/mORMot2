unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.log,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  entities,
  api.interfaces,
  api.impl;

type
  /// Template Sample Server
  TTemplateSampleServer = class
  protected
    fRestServer: TRestServerDB;
    fHttpServer: TRestHttpServer;
    fSampleApi: ITemplateSample;
  public
    constructor Create(const aPort: RawUtf8 = '8080';
      const aDbFileName: TFileName = 'template.db'); reintroduce;
    destructor Destroy; override;
    /// start the HTTP server
    procedure Start;
    /// stop the HTTP server
    procedure Stop;
  end;


implementation


{ TTemplateSampleServer }

constructor TTemplateSampleServer.Create(const aPort: RawUtf8;
  const aDbFileName: TFileName);
begin
  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmSample], aDbFileName);

  // Create and register the API implementation
  fSampleApi := TTemplateSample.Create(fRestServer.Orm);
  fRestServer.ServiceDefine(fSampleApi, [ITemplateSample], sicShared);

  // Create HTTP server
  fHttpServer := TRestHttpServer.Create(aPort, [fRestServer],
    '+', HTTP_DEFAULT_MODE, {threaded=}32);
  fHttpServer.AccessControlAllowOrigin := '*'; // CORS for all origins

  TSynLog.Add.Log(sllInfo, 'Template Sample Server created on port %', [aPort]);
end;

destructor TTemplateSampleServer.Destroy;
begin
  Stop;
  fHttpServer.Free;
  fSampleApi := nil;
  fRestServer.Free;
  inherited Destroy;
end;

procedure TTemplateSampleServer.Start;
begin
  TSynLog.Add.Log(sllInfo, 'Starting Template Sample Server...');
end;

procedure TTemplateSampleServer.Stop;
begin
  TSynLog.Add.Log(sllInfo, 'Stopping Template Sample Server...');
end;


end.
