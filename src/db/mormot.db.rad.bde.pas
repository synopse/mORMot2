/// Database Framework for BDE TDataSet Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.bde;

{
  *****************************************************************************

   Legacy BDE Database Access for mormot.db.rad
    - BDE Database Engine Connection

  *****************************************************************************
}

interface

{$ifdef FPC} // the old and deprecated BDE is a Delphi-specific "feature"

implementation // to compile a void unit on FPC

{$else}

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.rad,
  DBTables;

  
{ ************ BDE Database Engine Connection }

type
  /// Exception type associated to the direct BDE connection
  ESqlDBBde = class(ESqlDBDataset);


  /// implement properties shared by BDE connections
  TSqlDBBdeConnectionProperties = class(TSqlDBDatasetConnectionProperties)
  protected
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (BDE metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// this overridden method will retrieve the kind of DBMS from the main connection
    function GetDbms: TSqlDBDefinition; override;
  public
    /// initialize the properties to connect to the BDE engine
    // - aServerName shall contain the BDE Alias name
    // - aDatabaseName is ignored
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBBdeConnection instance
    function NewConnection: TSqlDBConnection; override;
  end;


  /// implements a direct connection via the BDE access layer
  TSqlDBBdeConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fDatabase: TDatabase;
    fSession: TSession;
    fDbms: TSqlDBDefinition;
    fDbmsName: RawUtf8;
  public
    /// prepare a connection to a specified BDE database server
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified BDE server
    // - should raise an ESqlDBBde on error
    procedure Connect; override;
    /// stop connection to the specified BDE database server
    // - should raise an ESqlDBBde on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSqlDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated BDE connection instance
    property Database: TDatabase
     read fDatabase;
  published
    /// the remote DBMS name, as retrieved at BDE connection creation
    property DbmsName: RawUtf8
      read fDbmsName;
    /// the remote DBMS type, as retrieved at BDE connection creation
    property Dbms: TSqlDBDefinition
      read fDbms;
  end;

  /// implements a statement via a BDE connection
  TSqlDBBdeStatement = class(TSqlDBDatasetStatement)
  protected
    /// initialize and set fQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TQuery.ExecSQL
    procedure DatasetExecSQL; override;
  public
  end;


implementation

{ ************ BDE Database Engine Connection }


{ TSqlDBBdeConnectionProperties }

constructor TSqlDBBdeConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
begin
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  {$ifndef UNICODE}
  fForceInt64AsFloat := true; // BDE is old and deprecated :(
  {$endif UNICODE}
end;

procedure TSqlDBBdeConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from BDE metadata ? }
end;

function TSqlDBBdeConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBBdeConnection.Create(self);
  TSqlDBBdeConnection(result).InternalProcess(speCreated);
end;

function TSqlDBBdeConnectionProperties.GetDbms: TSqlDBDefinition;
begin
  if fDbms = dUnknown then
    // retrieve DBMS type from alias driver name
    fDbms := (MainConnection as TSqlDBBdeConnection).Dbms;
  result := fDbms;
end;


{ TSqlDBBdeConnection }

procedure TSqlDBBdeConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

var
  BDEConnectionCount: integer = 0;

const
  PCHARS: array[0 .. 3] of PAnsiChar = (
    'ORACLE', 'MSSQL', 'MSACCESS', nil);
  TYPES: array[-1 .. high(PCHARS) - 1] of TSqlDBDefinition = (
    dDefault, dOracle, dMSSQL, dJet);

constructor TSqlDBBdeConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  alias: string;
begin
  inherited Create(aProperties);
  fDatabase := TDatabase.Create(nil);
  fSession := TSession.Create(nil);
  fSession.AutoSessionName := true;
  fDatabase.SessionName := fSession.SessionName;
  fDatabase.LoginPrompt := false;
  inc(BDEConnectionCount);
  alias := Utf8ToString(fProperties.ServerName);
  fDatabase.DatabaseName := 'SynDB' + alias + IntToStr(BDEConnectionCount);
  fDatabase.AliasName := alias;
  fDatabase.Params.Text := FormatString('USER NAME=%'#13#10'PASSWORD=%',
    [fProperties.UserID, fProperties.PassWord]);
  fDbmsName := StringToUtf8(fSession.GetAliasDriverName(alias));
  fDbms := TYPES[IdemPPChar(pointer(fDbmsName), @PCHARS)];
end;

procedure TSqlDBBdeConnection.Connect;
var
  {%H-}log: ISynLog;
begin
  if (fSession = nil) or
     (fDatabase = nil) then
    ESqlDBBde.RaiseUtf8('%.Connect() on % failed: Database=nil',
      [self, fProperties.ServerName]);
  SynDBLog.EnterLocal(log, 'Connect to Alias=%', [fDatabase.AliasName], self);
  try
    fSession.Open;
    fDatabase.Open;
    inherited Connect; // notify any re-connection 
  except
    on E: Exception do
    begin
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

destructor TSqlDBBdeConnection.Destroy;
begin
  try
   Disconnect;
  except
    on Exception do
  end;
  inherited;
  FreeAndNil(fDatabase);
  FreeAndNil(fSession);
end;

procedure TSqlDBBdeConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statements
  finally
    if fDatabase <> nil then
      fDatabase.Close;
    if (fSession <> nil) and fSession.Active then
      fSession.Close;
  end;
end;

function TSqlDBBdeConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and
            fDatabase.Connected;
end;

function TSqlDBBdeConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBBdeStatement.Create(self);
end;

procedure TSqlDBBdeConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSqlDBBdeConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSqlDBBdeStatement }

procedure TSqlDBBdeStatement.DatasetCreate;
begin
  fQuery := DBTables.TQuery.Create(nil);
  with DBTables.TQuery(fQuery) do
  begin
    DatabaseName := (fConnection as TSqlDBBdeConnection).Database.DatabaseName;
    SessionName := TSqlDBBdeConnection(fConnection).Database.Session.SessionName;
  end;
end;

function TSqlDBBdeStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as DBTables.TQuery).SQL.Text := aSQL;
  fQueryParams := DBTables.TQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSqlDBBdeStatement.DatasetExecSQL;
begin
  (fQuery as DBTables.TQuery).ExecSQL;
end;


initialization
  TSqlDBBdeConnectionProperties.RegisterClassNameForDefinition;
  
{$endif FPC} // the old and deprecated BDE is a Delphi-specific "feature"

end.

