unit server;

{$I mormot.defines.inc}

interface

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.log,
  mormot.core.rtti,
  mormot.orm.core,
  mormot.rest.sqlite3,
  mormot.soa.core,
  entities,
  api.interfaces,
  api.impl;

type
  /// ISAPI Sample Server
  TIsapiSampleServer = class
  protected
    fRestServer: TRestServerDB;
  public
    constructor Create(const aDbFileName: TFileName = ':memory:'); reintroduce;
    destructor Destroy; override;
    /// Get the REST server for ISAPI handler
    property RestServer: TRestServerDB read fRestServer;
  end;


implementation


{ TIsapiSampleServer }

constructor TIsapiSampleServer.Create(const aDbFileName: TFileName);
begin
  // Create SQLite3 database with ORM entities
  fRestServer := TRestServerDB.CreateSqlite3([TOrmSample], aDbFileName);

  // Register the calculator service
  fRestServer.ServiceDefine(TCalculator, [ICalculator], sicShared);

  // Create missing tables
  fRestServer.CreateMissingTables;

  TSynLog.Add.Log(sllInfo, 'ISAPI Sample Server created with database: %', [aDbFileName]);
end;

destructor TIsapiSampleServer.Destroy;
begin
  fRestServer.Free;
  inherited Destroy;
end;


end.
