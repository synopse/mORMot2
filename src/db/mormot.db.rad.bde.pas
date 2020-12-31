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
  ESqlDBBDE = class(ESqlDBDataset);


  /// implement properties shared by BDE connections
  TSqlDBBDEConnectionProperties = class(TSqlDBDatasetConnectionProperties)
  protected
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (BDE metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// this overridden method will retrieve the kind of DBMS from the main connection
    function GetDBMS: TSqlDBDefinition; override;
  public
    /// initialize the properties to connect to the BDE engine
    // - aServerName shall contain the BDE Alias name
    // - aDatabaseName is ignored
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBBDEConnection instance
    function NewConnection: TSqlDBConnection; override;
  end;


  /// implements a direct connection via the BDE access layer
  TSqlDBBDEConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fDatabase: TDatabase;
    fSession: TSession;
    fDBMS: TSqlDBDefinition;
    fDBMSName: RawUTF8;
  public
    /// prepare a connection to a specified BDE database server
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified BDE server
    // - should raise an ESqlDBBDE on error
    procedure Connect; override;
    /// stop connection to the specified BDE database server
    // - should raise an ESqlDBBDE on error
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
    property DBMSName: RawUTF8
      read fDBMSName;
    /// the remote DBMS type, as retrieved at BDE connection creation
    property DBMS: TSqlDBDefinition
      read fDBMS;
  end;

  /// implements a statement via a BDE connection
  TSqlDBBDEStatement = class(TSqlDBDatasetStatement)
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


{ TSqlDBBDEConnectionProperties }

constructor TSqlDBBDEConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
  {$ifndef UNICODE}
  fForceInt64AsFloat := true; // BDE is old and deprecated :(
  {$endif UNICODE}
end;

procedure TSqlDBBDEConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from BDE metadata ? }
end;

function TSqlDBBDEConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBBDEConnection.Create(self);
end;

function TSqlDBBDEConnectionProperties.GetDBMS: TSqlDBDefinition;
begin
  if fDBMS = dUnknown then
    // retrieve DBMS type from alias driver name
    fDBMS := (MainConnection as TSqlDBBDEConnection).DBMS;
  result := fDBMS;
end;


{ TSqlDBBDEConnection }

procedure TSqlDBBDEConnection.Commit;
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

constructor TSqlDBBDEConnection.Create(aProperties: TSqlDBConnectionProperties);
const
  PCHARS: array[0 .. 2] of PAnsiChar = (
    'ORACLE','MSSQL','MSACCESS');
  TYPES: array[-1 .. high(PCHARS)] of TSqlDBDefinition = (
    dDefault,dOracle,dMSSQL,dJet);
var alias: string;
begin
  inherited Create(aProperties);
  fDatabase := TDatabase.Create(nil);
  fSession := TSession.Create(nil);
  fSession.AutoSessionName := true;
  fDatabase.SessionName := fSession.SessionName;
  fDatabase.LoginPrompt := false;
  inc(BDEConnectionCount);
  alias := UTF8ToString(fProperties.ServerName);
  fDatabase.DatabaseName := 'SynDB' + alias + IntToStr(BDEConnectionCount);
  fDatabase.AliasName := alias;
  fDatabase.Params.Text := Format('USER NAME=%s'#13#10'PASSWORD=%s',
    [UTF8ToString(fProperties.UserID), UTF8ToString(fProperties.PassWord)]);
  fDBMSName := StringToUTF8(fSession.GetAliasDriverName(alias));
  fDBMS := TYPES[IdemPCharArray(pointer(fDBMSName), PCHARS)];
end;

procedure TSqlDBBDEConnection.Connect;
var Log: ISynLog;
begin
  if (fSession = nil) or
     (fDatabase = nil) then
    raise ESqlDBBDE.CreateUTF8('%.Connect() on % failed: Database=nil',
      [self, fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to Alias=%', [fDatabase.AliasName], self);
  try
    fSession.Open;
    fDatabase.Open;
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

destructor TSqlDBBDEConnection.Destroy;
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

procedure TSqlDBBDEConnection.Disconnect;
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

function TSqlDBBDEConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and
            fDatabase.Connected;
end;

function TSqlDBBDEConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBBDEStatement.Create(self);
end;

procedure TSqlDBBDEConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSqlDBBDEConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSqlDBBDEStatement }

procedure TSqlDBBDEStatement.DatasetCreate;
begin
  fQuery := DBTables.TQuery.Create(nil);
  with DBTables.TQuery(fQuery) do
  begin
    DatabaseName := (fConnection as TSqlDBBDEConnection).Database.DatabaseName;
    SessionName := TSqlDBBDEConnection(fConnection).Database.Session.SessionName;
  end;
end;

function TSqlDBBDEStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as DBTables.TQuery).SQL.Text := aSQL;
  fQueryParams := DBTables.TQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSqlDBBDEStatement.DatasetExecSQL;
begin
  (fQuery as DBTables.TQuery).ExecSQL;
end;


initialization
  TSqlDBBDEConnectionProperties.RegisterClassNameForDefinition;
  
{$endif FPC} // the old and deprecated BDE is a Delphi-specific "feature"

end.

