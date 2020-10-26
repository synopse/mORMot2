/// Database Framework for NexusDB TDataSet Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.nexusdb;

{
  *****************************************************************************

   Third Party NexusDB Components Database Access for mormot.db.rad
    - NexusDB Database Engine Connection

  *****************************************************************************
}

interface

{$ifdef FPC} // NexusDB is a Delphi-specific feature

implementation // to compile a void unit on FPC

{$else}

{.$define SYNDB_FULLNEXUSDB}
// by default, only the NexusDB Free Embedded version is interfaced
// - you can define this conditional in project options to use all units

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  {$ifdef ISDELPHIXE2}
  Data.DB,
  {$else}
  DB,
  {$endif ISDELPHIXE2}
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.rad,
  nxDB,
  nxsdServerEngine,
  nxsrServerEngine,
  nxsqlEngine,
  nxlgEventLog,
  nxllTransport,
  {$ifdef SYNDB_FULLNEXUSDB}
  nxchCommandHandler,
  nxsiServerInfoPluginServer,
  nxtcCOMTransport,
  nxtmSharedMemoryTransport,
  nxtnNamedPipeTransport,
  nxtsBlowfishRC4SecuredTransport,
  nxtwWinsockTransport,
  {$endif SYNDB_FULLNEXUSDB}
  nxseAutoComponent;
  

{ ************ NexusDB Database Engine Connection }

type
  /// Exception type associated to the direct NexusDB connection
  ESQLDBNexusDB = class(ESQLDBDataset);

  // available communication protocols used by NexusDB between client and server
  // - nxpFOLDER: default protocol, accessing NexusDB database in a Windows Folder
  // - nxpTCPIP: TCP/IP transport, indicated by nxtcp://
  // - nxpPIPE: Windows Named Pipe transport, indicated by nxpipe://
  // - nxpMEM: direct memory transport, indicated by nxmem://
  // - nxpBFISH: BlowFish transport, indicated by nxbfisch://
  TNXProtocol = (
    nxpUnknown,
    nxpFOLDER,
    nxpTCPIP,
    nxpPIPE,
    nxpCOM,
    nxpMEM,
    nxpBFISH);

  // implement properties shared by native NexusDB connections
  // - note that only the embedded engine is implemented by now - feedback needed!
  TSQLDBNexusDBConnectionProperties = class(TSQLDBDatasetConnectionProperties)
  private
    fProtocol: TNXProtocol;
  protected
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect to the NexusDB engine
    // - this overridden method will initialize the protocol to be used as stated
    // by aServerName i.e. nxpTCIP://11.23.34.43
    // - Default protocol is nxpFolder
    // - if protocol is nxpFolder then aDatabaseName will contain the path to the
    // folder to be used
    // - if protocol is other then nxpFolder than aServerName will contain the server
    // to connect to and aDatabaseName will contains the alias of the database
    // - Possible aServerName formats:
    // $ <protocol>://<servername>/<alias>  (aDatabaseName will be overwritten by this alias)
    // $ <protocol>://servername            (aDatabaseName will contain alias)
    // $ ''                                 (aDatabaseName contains path to nxpFOLDER database)
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// convert a textual column data type, as retrieved e.g. from SQLGetField,
    // into our internal primitive types
    function ColumnTypeNativeToDB(const aNativeType: RawUTF8; aScale: integer): TSQLDBFieldType; override;
    /// Determine if database exists
    // - just test if the corresponding folder exists
    function DatabaseExists: boolean; virtual;
    /// create the database folder (if not existing)
    function CreateDatabase: boolean; virtual;
    /// delete the database folder
    // - including all its files - so to be used carefully!
    function DeleteDatabase: boolean; virtual;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBNexusDBConnection instance
    function NewConnection: TSQLDBConnection; override;
  published
    /// the transport protocol used to connect to the NexusDB engine
    property Protocol: TNXProtocol read fProtocol;
  end;


  // implements a direct connection to the native NexusDB database
  TSQLDBNexusDBConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: TnxDatabase;
    fSession: TnxSession;
    fServerEngine: TnxBaseServerEngine;
    procedure SetServerEngine(aServerEngine: TnxBaseServerEngine);
  public
    /// prepare a connection to a specified NexusDB database server
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified NexusDB server
    // - should raise an ESQLDBNexusDB on error
    procedure Connect; override;
    /// stop connection to the specified NexusDB database server
    // - should raise an ESQLDBNexusDB on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated NexusDB connection instance
    property Database: TnxDatabase read fDatabase;
    /// associated NexusDB server engine
    property ServerEngine: TnxBaseServerEngine read fServerEngine write SetServerEngine;
  end;

  // implements a statement via the native NexusDB connection
  TSQLDBNexusDBStatement = class(TSQLDBDatasetStatement)
  protected
    /// initialize and set fQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TQuery.ExecSQL
    procedure DatasetExecSQL; override;
  public
  end;


const
  /// set aServerName to this value to create an in-memory table
  // - do not use this constant, since it was not working as expected yet
  NEXUSDB_INMEMORY = '#INMEM';


// determine NexusDB transport protocol (TNXProtocol) to be used, based on
// protocol indicator in connection string
// - if no protocol specifier is included in the connectionstring then nxpFOLDER
// is assumed.
// - aServerName will contain the URL to the Server if the protocol
// is not nxpFOLDER
function GetNXProtocol(const aConnectionString: RawUTF8; out aServerName: RawUTF8;
  out aAlias: RawUTF8): TNXProtocol;

/// return the internal NexusDB embedded engine
// - initialize it, if was not already the case
function NexusEmbeddedEngine: TnxServerEngine;

/// release any internal NexusDB embedded engine
// - returns nil on success, or PtrInt(-1) if was not initialized  
function DropNexusEmbeddedEngine: TnxServerEngine;


implementation


{ ************ NexusDB Database Engine Connection }

implementation

uses
  {$ifdef SYNDB_FULLNEXUSDB}
  nxreRemoteServerEngine,
  uMiscellaneous,
  {$endif SYNDB_FULLNEXUSDB}
  nxsdConst,
  nxsdTypes;


{ TSQLDBNexusDBConnectionProperties }

function TSQLDBNexusDBConnectionProperties.ColumnTypeNativeToDB(const aNativeType: RawUTF8;
  aScale: integer): TSQLDBFieldType;
const
  CONV_TABLE: array[TnxFieldType] of TSQLDBFieldType  = (
    mormot.db.core.ftInt64, mormot.db.core.ftUTF8,  mormot.db.core.ftUTF8,
    mormot.db.core.ftInt64, mormot.db.core.ftInt64, mormot.db.core.ftInt64,
    mormot.db.core.ftInt64, mormot.db.core.ftInt64, mormot.db.core.ftInt64,
    mormot.db.core.ftInt64, mormot.db.core.ftInt64, mormot.db.core.ftDouble,
    mormot.db.core.ftDouble, mormot.db.core.ftDouble, mormot.db.core.ftCurrency,
    mormot.db.core.ftDate,  mormot.db.core.ftDate, mormot.db.core.ftDate,
    mormot.db.core.ftInt64, mormot.db.core.ftBlob, mormot.db.core.ftUTF8,
    mormot.db.core.ftBlob, mormot.db.core.ftBlob, mormot.db.core.ftUTF8,
    mormot.db.core.ftUTF8, mormot.db.core.ftUTF8, mormot.db.core.ftInt64,
    mormot.db.core.ftUTF8, mormot.db.core.ftCurrency, mormot.db.core.ftUTF8,
    mormot.db.core.ftDouble );
begin
  result := CONV_TABLE[FieldDataTypesMapSQL(UTF8ToString(aNativeType))];
end;

constructor TSQLDBNexusDBConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
var
  lServerURL, lAlias: RawUTF8;
begin
  fDBMS := dNexusDB;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  fProtocol := GetNXProtocol(aServerName, lServerURL, lAlias);
  if fProtocol = nxpFOLDER then
    fServerName := ''
  else if fProtocol > nxpFOLDER then
    fServerName := lServerURL;
  if fProtocol > nxpUnknown then
    fDatabaseName := lAlias
  else
    fDatabaseName := '';
end;

procedure TSQLDBNexusDBConnectionProperties.GetForeignKeys;
begin
  with Execute(
    'select F.FK_CONSTRAINT_TABLE_NAME||''.''||C.FK_CONSTRAINT_REFERENCING_COLUMNS_NAME col, ' +
    '       F.FK_CONSTRAINT_REFERENCES_TABLE_NAME||''.''||R.FK_CONSTRAINT_REFERENCED_COLUMNS_NAME ref ' +
    '  from #FOREIGNKEY_CONSTRAINTS F, ' +
    '       #FOREIGNKEY_CONSTRAINTS_REFERENCING_COLUMNS C, ' +
    '       #FOREIGNKEY_CONSTRAINTS_REFERENCED_COLUMNS R ' + ' where ' +
    '       F.FK_CONSTRAINT_TABLE_NAME = C.FK_CONSTRAINT_TABLE_NAME' +
    '   and F.FK_CONSTRAINT_NAME = C.FK_CONSTRAINT_NAME' +
    '       F.FK_CONSTRAINT_TABLE_NAME = R.FK_CONSTRAINT_TABLE_NAME' +
    '   and F.FK_CONSTRAINT_NAME = R.FK_CONSTRAINT_NAME', []) do
    while Step do
      fForeignKeys.Add(ColumnUTF8(0), ColumnUTF8(1));
end;

function TSQLDBNexusDBConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBNexusDBConnection.Create(self);
end;

function TSQLDBNexusDBConnectionProperties.DatabaseExists: boolean;
begin
  if (fProtocol = nxpFOLDER) and
     (fDatabaseName <> NEXUSDB_INMEMORY) then
    result := DirectoryExists(UTF8ToString(fDatabaseName))
  else
    result := True; // if we cannot determine directly, assume it exists
end;

function TSQLDBNexusDBConnectionProperties.CreateDatabase: boolean;
begin
  if fProtocol = nxpFOLDER then
    if fDatabaseName = NEXUSDB_INMEMORY then
      result := true
    else
      result := ForceDirectories(UTF8ToString(fDatabaseName))
  else
    result := false;
end;

function TSQLDBNexusDBConnectionProperties.DeleteDatabase: boolean;
begin
  if fProtocol = nxpFOLDER then
    if fDatabaseName = NEXUSDB_INMEMORY then
      result := true
    else
      result := DirectoryDelete(UTF8ToString(fDatabaseName))
  else
    result := false;
end;


{ TSQLDBNexusDBConnection }

procedure TSQLDBNexusDBConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSQLDBNexusDBConnection.Connect;
var
  Log: ISynLog;
begin
  Log := SynDBLog.Enter;
  try
    ServerEngine.Active := True;
    ServerEngine.Connected := True;
    fDatabase.Name := ClassName;
    fDatabase.Connect;
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      Log.Log(sllError, E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

constructor TSQLDBNexusDBConnection.Create(aProperties: TSQLDBConnectionProperties);
var
  lProp: TSQLDBNexusDBConnectionProperties;
var
  Log: ISynLog;
begin
  Log := SynDBLog.Enter;
  inherited Create(aProperties);
  lProp := aProperties as TSQLDBNexusDBConnectionProperties; // type check to make sure
  if lProp.Protocol = nxpUnknown then
    raise ESQLDBNexusDB.CreateUTF8('%.Create: Unknown NexusDB protocol in Servername=[%]',
      [self, lProp.ServerName]);
  fDatabase := TnxDatabase.Create(nil);
  fSession := TnxSession.Create(nil);
  fSession.UserName := UTF8ToString(lProp.UserID);
  fSession.Password := UTF8ToString(lProp.PassWord);
  fDatabase.Session := fSession;
  if lProp.Protocol = nxpFOLDER then
  begin
    SetServerEngine(NexusEmbeddedEngine);
    if not lProp.DatabaseExists then
    begin
      Log.Log(sllDB, 'Database % does not exists -> create folder', [lProp.DatabaseName]);
      lProp.CreateDatabase;
    end;
    Database.AliasPath := lProp.DatabaseName;
    Log.Log(sllDB, 'NexusDB % using database folder %', [fDatabase.Version,
      lProp.DatabaseName]);
  end
  else
  begin
    raise ESQLDBNexusDB.Create('Remote NexusDB engine not supported (yet)');
{    SetServerEngine(TnxRemoteServerEngine.Create(nil));
    Database.AliasName := lProp.DatabaseName;
    case lProp.Protocol of
      nxpTCPIP:
        FTransport := TnxWinsockTransport.Create(nil);
      nxpPIPE:
        FTransport := TnxNamedPipeTransport.Create(nil);
      nxpCOM:
        FTransport := TnxRegisteredCOMTransport.Create(nil);
      nxpMEM:
        FTransport := TnxSharedMemoryTransport.Create(nil);
      nxpBFISH:
        FTransport := TnxBlowfishRC4SecuredTransport.Create(nil);
    end;
    TnxRemoteServerEngine(FServerEngine).Transport := FTransport;
    FTransport.ServerName := lProp.Servername;
    FTransport.CommandHandler := FCommandHandler;
    FTransport.EventLog := FEventLog; }
  end;
end;

destructor TSQLDBNexusDBConnection.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(fDatabase);
  FreeAndNil(fSession);
end;

procedure TSQLDBNexusDBConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statements
  finally
    if Assigned(fDatabase) then
      fDatabase.Close;
    if Assigned(fSession) and fSession.Active then
    begin
      fSession.CloseInactiveTables;
      fSession.CloseInactiveFolders;
      fSession.Close;
    end;
  end;
end;

function TSQLDBNexusDBConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and fDatabase.Connected;
end;

function TSQLDBNexusDBConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBNexusDBStatement.Create(self);
end;

procedure TSQLDBNexusDBConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSQLDBNexusDBConnection.SetServerEngine(aServerEngine: TnxBaseServerEngine);
begin
  if FServerEngine <> aServerEngine then
  begin
    FServerEngine := aServerEngine;
    fSession.ServerEngine := aServerEngine;
  end;
end;

procedure TSQLDBNexusDBConnection.StartTransaction;
begin
  inherited StartTransaction;
  if not fDatabase.TryStartTransaction then
    raise ESQLDBNexusDB.Create('Error occcured trying to start a transaction');
end;


{ TSQLDBNexusDBStatement }

function TSQLDBNexusDBStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as TnxQuery).SQL.Text := aSQL;
  fQueryParams := TnxQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSQLDBNexusDBStatement.DatasetExecSQL;
begin
  (fQuery as TnxQuery).ExecSQL;
end;

procedure TSQLDBNexusDBStatement.DatasetCreate;
begin
  fQuery := TnxQuery.Create(nil);
  with TnxQuery(fQuery) do
  begin
    Database := (fConnection as TSQLDBNexusDBConnection).Database;
    Session := TSQLDBNexusDBConnection(fConnection).Database.Session;
  end;
end;


{ low-level NexusDB engine functions }

var
  vNexusEmbeddedEngine: TnxServerEngine;

function GetNXProtocol(const aConnectionString: RawUTF8;
  out aServerName: RawUTF8; out aAlias: RawUTF8): TNXProtocol;
const
  NXPROTNAMES: array[nxpFOLDER..high(TNXProtocol)] of RawUTF8 = (
    'nxemb', 'nxtcp', 'nxpipe', 'nxcom', 'nxmem', 'nxbfish');
var
  Prot, Alias, l, r: RawUTF8;
  IsPath: boolean;
  pr: TNXProtocol;
begin
  Split(aConnectionString, ':', Prot, Alias);
  IsPath := (Prot = '') or // no protocol indicated: assume it's a path (relative)
    (Prot = NEXUSDB_INMEMORY) or
    ((Length(Prot) = 1) and // check for drive letter
     (Prot[1] in ['a'..'z', 'A'..'Z']) and DirectoryExists(TFileName(Prot[1]) + ':\')) or
      // ensure explicit root folder for safety 
      IdemPChar(Pointer(aConnectionString), '.\') or
      IdemPChar(Pointer(aConnectionString), '..\');
  if IsPath then
  begin
    result := nxpFOLDER;
    aAlias := aConnectionString;
  end
  else
  begin
    result := nxpUnknown;
    for pr := Low(NXPROTNAMES) to High(NXPROTNAMES) do
      if Prot = NXPROTNAMES[pr] then
      begin
        result := pr;
        break;
      end;
    if result = nxpFOLDER then
    begin
      if Prot <> '' then
        aAlias := Alias
      else
        aAlias := aConnectionString;
    end
    else if result > nxpFOLDER then
    begin
      Split(aConnectionString, '://', l, r);
      Split(r, '/', aServerName, aAlias);
    end;
  end;
end;

function DropNexusEmbeddedEngine: TnxServerEngine;
var
  cmp: TComponent;
  i: integer;
begin
  if PtrInt(vNexusEmbeddedEngine) <> -1 then
  begin
    if Assigned(vNexusEmbeddedEngine) then
    begin
      vNexusEmbeddedEngine.Close;
      vNexusEmbeddedEngine.SqlEngine := nil;
      vNexusEmbeddedEngine.EventLog := nil;
      // ensure our components get destroyed before destroying the engine
      for i := vNexusEmbeddedEngine.ComponentCount - 1 downto 0 do
      begin
        cmp := vNexusEmbeddedEngine.Components[i];
        {$ifdef SYNDB_FULLNEXUSDB}
        if cmp is TnxServerCommandHandler then
          with TnxServerCommandHandler(cmp) do
          begin
            ServerEngine := nil;
            Close;
            Free;
          end
        else
        {$endif SYNDB_FULLNEXUSDB}
        if cmp is TnxSqlEngine then
          with TnxSqlEngine(cmp) do
          begin
            Close;
            Free;
          end
        else if cmp is TnxEventLog then
          with TnxEventLog(cmp) do
          begin
            Flush;
            Free;
          end;
      end;
    end;
    FreeAndNil(vNexusEmbeddedEngine);
  end;
  result := vNexusEmbeddedEngine;
end;

function NexusEmbeddedEngine: TnxServerEngine;
{$ifdef SYNDB_FULLNEXUSDB}
var
  CommandHandler: TnxServerCommandHandler;
{$endif SYNDB_FULLNEXUSDB}
begin
  if PtrInt(vNexusEmbeddedEngine) = -1 then
    raise ESQLDBNexusDB.Create('Nexus Embedded engine was already finalized!')
  else if vNexusEmbeddedEngine = nil then
  begin
    vNexusEmbeddedEngine := TnxServerEngine.Create(nil);
    //vNexusEmbeddedEngine.Options := vNexusEmbeddedEngine.Options+[seoInMemOnly];
    {$ifdef SYNDB_FULLNEXUSDB}
    CommandHandler := TnxServerCommandHandler.Create(vNexusEmbeddedEngine);
    {$endif SYNDB_FULLNEXUSDB}
    vNexusEmbeddedEngine.SqlEngine := TnxSqlEngine.Create(vNexusEmbeddedEngine);
    vNexusEmbeddedEngine.EventLog := TnxEventLog.Create(vNexusEmbeddedEngine);
    {$ifdef SYNDB_FULLNEXUSDB}
    CommandHandler.ServerEngine := vNexusEmbeddedEngine;
    {$endif SYNDB_FULLNEXUSDB}
  end;
  result := vNexusEmbeddedEngine;
end;

procedure FinalizeNXEmbeddedEngine;
begin
  if PtrInt(vNexusEmbeddedEngine) <> -1 then
  try
    DropNexusEmbeddedEngine;
  finally
    PtrInt(vNexusEmbeddedEngine) := -1; // mark always as finalized
  end;
end;


initialization
  TSQLDBNexusDBConnectionProperties.RegisterClassNameForDefinition;

finalization
  FinalizeNXEmbeddedEngine;

{$endif FPC} // NexusDB is a Delphi-specific

end.

