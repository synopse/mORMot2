/// REpresentation State Tranfer (REST) SQlite3 Server and Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.sqlite3;

{
  *****************************************************************************

   REST Server and Client Using an Embedded SQlite3 Database Engine
    - TRestServerDB REST Server with Direct Access to a SQLite3 Database
    - TRestClientDB REST Client with Direct Access to a SQLite3 Database

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.orm.sqlite3,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.db.raw.sqlite3;


{ ************ TRestServerDB REST Server with Direct Access to a SQLite3 Database }

type
  TRestServerDB = class(TRestServer)
  protected
    function GetDB: TSqlDatabase;
    function GetStatementLastException: RawUtf8;
    // include addition SQLite3 specific information to the returned content
    procedure InternalStat(Ctxt: TRestServerURIContext; W: TTextWriter); override;
    procedure InternalInfo(var info: TDocVariantData); override;
  public
    /// initialize a REST server with a SQLite3 database
    // - any needed TSqlVirtualTable class should have been already registered
    // via the RegisterVirtualTableModule() method
    constructor Create(aModel: TOrmModel; aDB: TSqlDataBase;
      aHandleUserAuthentication: boolean = false;
      aOwnDB: boolean = false); reintroduce; overload; virtual;
    /// initialize a REST server with a database, by specifying its filename
    // - TRestServerDB will initialize a owned TSqlDataBase, and free it on Destroy
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    // - it will then call the other overloaded constructor to initialize the server
    constructor Create(aModel: TOrmModel; const aDBFileName: TFileName;
      aHandleUserAuthentication: boolean = false; const aPassword: RawUtf8 = '';
      aDefaultCacheSize: integer = 10000;
      aDefaultPageSize: integer = 4096); reintroduce; overload;
    /// initialize a REST server with a database, and a temporary Database Model
    // - a Model will be created with supplied tables, and owned by the server
    // - if you instantiate a TRestServerFullMemory or TRestServerDB
    // with this constructor, an in-memory engine will be created, with
    // enough abilities to run regression tests, for instance
    constructor CreateWithOwnModel(const aTables: array of TOrmClass;
      const aDBFileName: TFileName; aHandleUserAuthentication: boolean = false;
      const aRoot: RawUtf8 = 'root'; const aPassword: RawUtf8 = '';
      aDefaultCacheSize: integer = 10000;
      aDefaultPageSize: integer = 4096); overload;
    /// create a REST server with an in-memory SQLite3 engine
    constructor Create(aModel: TOrmModel;
      aHandleUserAuthentication: boolean = false); overload; override;
    /// initialize a REST server with an in-memory SQLite3 database and a
    // temporary Database Model
    // - could be used for test purposes
    constructor CreateWithOwnModel(const aTables: array of TOrmClass;
      aHandleUserAuthentication: boolean = false;
      const aRoot: RawUtf8 = 'root'); overload;
    /// initialize a SQLite3 REST server from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// save the TRestOrmServerDB properties into a persistent storage object
    // - RegisteredClassCreateFrom() will expect Definition.DatabaseName to store
    // the DBFileName, and optionally encrypt the file using Definition.Password
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;

  published
    /// associated database
    property DB: TSqlDataBase read GetDB;
    /// contains some textual information about the latest Exception raised
    // during SQL statement execution
    property StatementLastException: RawUtf8
      read GetStatementLastException;
  end;

  /// class-reference type (metaclass) of a REST server using SQLite3 as main engine
  TRestServerDBClass = class of TRestServerDB;



{ ************ TRestClientDB REST Client with Direct Access to a SQLite3 Database }

type
  /// REST client with direct access to a SQLite3 database
  // - a hidden TRestServerDB server is created and called internaly
  TRestClientDB = class(TRestClientURI)
  protected
    fServer: TRestServerDB;
    fOwnedServer: TRestServerDB;
    fOwnedDB: TSqlDataBase;
    fInternalHeader: RawUtf8;
    function GetDB: TSqlDataBase;
      {$ifdef HASINLINE}inline;{$endif}
    /// method calling the RESTful server fServer
    procedure InternalURI(var Call: TRestURIParams); override;
    /// overridden protected method do nothing (direct DB access has no connection)
    function InternalCheckOpen: boolean; override;
    /// overridden protected method do nothing (direct DB access has no connection)
    procedure InternalClose; override;
  public
    /// initializes the class, and creates an internal TRestServerDB to
    // internaly answer to the REST queries
    // - aServerClass could be TRestServerDB by default
    constructor Create(aClientModel, aServerModel: TOrmModel; aDB: TSqlDataBase;
      aServerClass: TRestServerDBClass;
      aHandleUserAuthentication: boolean = false); reintroduce; overload;
    /// same as above, from a SQLite3 filename specified
    // - an internal TSqlDataBase will be created internaly and freed on Destroy
    // - aServerClass could be TRestServerDB by default
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    constructor Create(aClientModel, aServerModel: TOrmModel;
      const aDBFileName: TFileName; aServerClass: TRestServerDBClass;
      aHandleUserAuthentication: boolean = false; const aPassword: RawUtf8 = '';
      aDefaultCacheSize: integer = 10000); reintroduce; overload;
    /// initialize the class, for an existing TRestServerDB
    // - the client TOrmModel will be cloned from the server's one
    // - the TRestServerDB and TSqlDatabase instances won't be managed by the
    // client, but will access directly to the server
    constructor Create(aRunningServer: TRestServerDB); reintroduce; overload;
    /// release the server
    destructor Destroy; override;

    /// associated Server
    property Server: TRestServerDB
      read fServer;
    /// associated database
    property DB: TSqlDataBase
      read GetDB;
  end;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSqlRestClientDB = TRestClientDB;
  // should be a proper type for RegisterClassNameForDefinition
  TSqlRestServerDB = type TRestServerDB;

{$endif PUREMORMOT2}


implementation


{ ************ TRestServerDB REST Server with Direct Access to a SQLite3 Database }

{ TRestServerDB }

constructor TRestServerDB.Create(aModel: TOrmModel; aDB: TSqlDataBase;
  aHandleUserAuthentication: boolean; aOwnDB: boolean);
begin
  inherited Create(aModel, aHandleUserAuthentication);
  with TRestOrmServerDB.Create(self, aDB, aOwnDB) do // assign the SQlite3 engine
    if DB <> nil then
      // ensure the low-level SQLite3 engine will share the same log
      DB.Log := fLogClass;
end;

constructor TRestServerDB.Create(aModel: TOrmModel;
  const aDBFileName: TFileName; aHandleUserAuthentication: boolean;
  const aPassword: RawUtf8; aDefaultCacheSize, aDefaultPageSize: integer);
var
  db: TSqlDatabase;
begin
  db := TSqlDatabase.Create(aDBFileName, aPassword, 0,
    aDefaultCacheSize, aDefaultPageSize);
  Create(aModel, db, aHandleUserAuthentication, {owndb=}true);
end;

constructor TRestServerDB.CreateWithOwnModel(const aTables: array of TOrmClass;
  const aDBFileName: TFileName; aHandleUserAuthentication: boolean;
  const aRoot, aPassword: RawUtf8; aDefaultCacheSize, aDefaultPageSize: integer);
var
  model: TOrmModel;
begin
  model := TOrmModel.Create(aTables, aRoot);
  Create(model, aDBFileName, aHandleUserAuthentication, aPassword,
    aDefaultCacheSize, aDefaultPageSize);
  model.Owner := self;
end;

constructor TRestServerDB.Create(aModel: TOrmModel;
  aHandleUserAuthentication: boolean);
begin
  Create(aModel, SQLITE_MEMORY_DATABASE_NAME, aHandleUserAuthentication);
end;

constructor TRestServerDB.CreateWithOwnModel(const aTables: array of TOrmClass;
  aHandleUserAuthentication: boolean; const aRoot: RawUtf8);
var
  model: TOrmModel;
begin
  model := TOrmModel.Create(aTables, aRoot);
  Create(model, aHandleUserAuthentication);
  model.Owner := self;
end;

constructor TRestServerDB.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition;
  aServerHandleAuthentication: boolean);
begin
  Create(aModel, Utf8ToString(aDefinition.ServerName),
    aServerHandleAuthentication, aDefinition.PasswordPlain);
end;

procedure TRestServerDB.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition=nil then
    exit;
  inherited DefinitionTo(Definition); // set Kind
  with fOrmInstance as TRestOrmServerDB do
    if DB <> nil then
    begin
      Definition.ServerName := StringToUtf8(DB.FileName);
      Definition.PasswordPlain := DB.Password;
    end;
end;

function TRestServerDB.GetDB: TSqlDatabase;
begin
  result := (fOrmInstance as TRestOrmServerDB).DB;
end;

function TRestServerDB.GetStatementLastException: RawUtf8;
begin
  result := (fOrmInstance as TRestOrmServerDB).StatementLastException;
end;

procedure TRestServerDB.InternalStat(Ctxt: TRestServerURIContext; W: TTextWriter);
var
  i: PtrInt;
  ndx: TIntegerDynArray;
begin
  inherited InternalStat(Ctxt, W);
  if Ctxt.InputExists['withall'] or
     Ctxt.InputExists['withsqlite3'] then
    with fOrmInstance as TRestOrmServerDB do
    begin
      W.CancelLastChar('}');
      W.AddShort(',"sqlite3":[');
      DB.Lock;
      try
        StatementCache.SortCacheByTotalTime(ndx);
        with StatementCache do
        for i := 0 to Count - 1 do
          with Cache[ndx[i]] do
          begin
            W.AddJsonEscape([StatementSQL, Timer]);
            W.Add(',');
          end;
      finally
        DB.UnLock;
      end;
      W.CancelLastComma;
      W.Add(']', '}');
    end;
end;

procedure TRestServerDB.InternalInfo(var info: TDocVariantData);
begin
  inherited InternalInfo(info);
  info.AddValue(
    'db', FormatString('% %', [ExtractFileName(DB.FileName), KB(DB.FileSize)]));
end;


{ ************ TRestClientDB REST Client with Direct Access to a SQLite3 Database }

{ TRestClientDB }

function TRestClientDB.GetDB: TSqlDataBase;
begin
  result := fServer.DB;
end;

procedure TRestClientDB.InternalURI(var Call: TRestURIParams);
begin
  if fInternalHeader = '' then
    fInternalHeader := 'RemoteIP: 127.0.0.1'#13#10'ConnectionID: ' +
      PointerToHex(self);
  AddToCsv(fInternalHeader, call.InHead, #13#10);
  call.RestAccessRights := @FULL_ACCESS_RIGHTS;
  fServer.URI(call);
  if (call.OutInternalState = 0) and
     (fServer.DB.InternalState <> nil) then
    // manual update if necessary
    call.OutInternalState := fServer.DB.InternalState^;
end;

function TRestClientDB.InternalCheckOpen: boolean;
begin
  result := true;
end;

procedure TRestClientDB.InternalClose;
begin
end;

constructor TRestClientDB.Create(aClientModel, aServerModel: TOrmModel;
  aDB: TSqlDataBase; aServerClass: TRestServerDBClass;
  aHandleUserAuthentication: boolean);
begin
  aDB.UseCache := true;      // we better use caching in this JSON oriented use
  inherited Create(aClientModel);
  if aServerModel = nil then
    // clone the server model from client, if none specified
    aServerModel := TOrmModel.Create(aClientModel);
  // next line will create aModel tables if necessary
  fOwnedServer := aServerClass.Create(aServerModel, aDB, aHandleUserAuthentication);
  fServer := fOwnedServer;
  fServer.NoAjaxJson := true; // use smaller JSON size in this local use
end;

constructor TRestClientDB.Create(aClientModel, aServerModel: TOrmModel;
  const aDBFileName: TFileName; aServerClass: TRestServerDBClass;
  aHandleUserAuthentication: boolean; const aPassword: RawUtf8;
  aDefaultCacheSize: integer);
begin
  fOwnedDB := TSqlDataBase.Create(
    aDBFileName, aPassword, 0, aDefaultCacheSize);
  Create(aClientModel, aServerModel,
    fOwnedDB, aServerClass, aHandleUserAuthentication);
end;

constructor TRestClientDB.Create(aRunningServer: TRestServerDB);
var
  m: TOrmModel;
begin
  if aRunningServer = nil then
    raise ERestException.CreateUtf8('%.Create(nil)', [self]);
  m := TOrmModel.Create(aRunningServer.Model);
  m.Owner := Self; // auto-free m in TSqlRest.Destroy
  inherited Create(m);
  fServer := aRunningServer; // leave fOwnedServer=nil
end;

destructor TRestClientDB.Destroy;
var
  m: TOrmModel;
begin
  try
    inherited Destroy; // UnLock records + SessionClose
  finally
    if fOwnedServer <> nil then
    begin
      if fServer = nil then
        m := nil
      else
        m := fServer.Model;
      if (m <> nil) and
         (m.Owner <> nil) then
        // free associated model only if it's owned by nobody
        m := nil;
      try
        FreeAndNil(fOwnedServer);
        fServer := nil;
      finally
        m.Free;
        fOwnedDB.Free;
      end;
    end;
  end;
end;



initialization
  TRestServerDB.RegisterClassNameForDefinition;
  {$ifndef PUREMORMOT2}
  TSqlRestServerDB.RegisterClassNameForDefinition;
  {$endif PUREMORMOT2}

end.

