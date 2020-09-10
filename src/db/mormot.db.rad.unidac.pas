/// Database Framework for UniDac/AnyDac TDataSet Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.unidac;

{
  *****************************************************************************

   Third Party UniDac Components Database Access for mormot.db.rad
    - UniDac Database Engine Connection

  *****************************************************************************
}

interface

{$ifdef FPC} // UniDac is a Delphi-specific feature

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
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.core.log,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.rad,
  Uni,
  UniProvider,
  UniScript;
  

{ ************ UniDac Database Engine Connection }

const
  cMSSQLProvider = 'prDirect';

type
  /// Exception type associated to UniDAC database access
  ESQLDBUniDAC = class(ESQLDBDataset);


  ///	connection properties definition using UniDAC database access
  TSQLDBUniDACConnectionProperties = class(TSQLDBDatasetConnectionProperties)
  protected
    fSpecificOptions: TStringList;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (UniDAC metadata may be used in the future)
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect via UniDAC database access
    // - aServerName shall contain the UniDAC provider name, e.g. 'Oracle' - you
    // can use the TSQLDBUniDACConnectionProperties.URI() to retrieve the
    // provider name from its SynDB.TSQLDBDefinition enumeration, and optionally
    // set some options, which will be added to the internal SpecificOptions[]:
    // ! 'Oracle?ClientLibrary=oci64\oci.dll'
    // ! 'MySQL?Server=192.168.2.60;Port=3306', 'world', 'root', 'dev'
    // - aDatabaseName shall contain the database server name
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// release internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBUniDACConnection instance
    function NewConnection: TSQLDBConnection; override;

    /// compute the UniDAC URI from a given database engine and server name
    // - the optional server name can contain a port number, specified after ':'
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSQLDBUniDACConnectionProperties.Create(
    // !   TSQLDBUniDACConnectionProperties.URI(dOracle,'','oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSQLDBUniDACConnectionProperties.Create(
    // !   TSQLDBUniDACConnectionProperties.URI(dFirebird,'',
    // !   'Firebird\fbembed.dll'),'databasefilename','','');
    // ! PropsMySQL := TSQLDBUniDACConnectionProperties.Create(
    // !   TSQLDBUniDACConnectionProperties.URI(dMySQL,'192.168.2.60:3306'),
    // !   'world', 'root', 'dev');
    class function URI(aServer: TSQLDBDefinition; const aServerName: RawUTF8;
      const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUTF8;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUTF8; out Fields: TSQLDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetTableNames(out Tables: TRawUTF8DynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetIndexes(const aTableName: RawUTF8; out Indexes: TSQLDBIndexDefineDynArray); override;
    /// allow to set the options specific to a UniDAC driver
    // - for instance, you can set for both SQLite3 and Firebird/Interbase:
    // ! Props.SpecificOptions.Values['ClientLibrary'] := ClientDllName;
    property SpecificOptions: TStringList read fSpecificOptions;
  end;


  ///	implements a direct connection via UniDAC database access
  TSQLDBUniDACConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: TUniConnection;
  public
    /// prepare a connection for a specified UniDAC database access
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified database server using UniDAC
    // - should raise an ESQLDBUniDAC on error
    procedure Connect; override;
    /// stop connection to the specified database server using UniDAC
    // - should raise an ESQLDBUniDAC on error
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
    /// access to the associated UniDAC connection instance
    property Database: TUniConnection read fDatabase;
  end;

  ///	implements a statement via a UniDAC connection
  TSQLDBUniDACStatement = class(TSQLDBDatasetStatement)
  protected
    /// initialize and set fQuery: TUniQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TUniQuery.ExecSQL
    procedure DatasetExecSQL; override;
    /// overriden by itSDS to properly handle UniDAC parameters
    procedure DataSetBindSQLParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSQLDBParam); override;
  public
  end;


const
  /// UniDAC provider names corresponding to SynDB recognized SQL engines
  UNIDAC_PROVIDER: array[dOracle..high(TSQLDBDefinition)] of RawUTF8 = (
    'Oracle', 'SQL Server', 'Access', 'MySQL', 'SQLite', 'InterBase', 
    'NexusDB', 'PostgreSQL', 'DB2', '');


implementation

uses
  DAScript,
  CRDataTypeMap,
  DBAccess;


{ TSQLDBUniDACConnectionProperties }

constructor TSQLDBUniDACConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
var
  p: TSQLDBDefinition;
  provider, options, namevalue: RawUTF8;
  opt: PUTF8Char;
begin
  Split(aServerName, '?', provider, options);
  for p := Low(UNIDAC_PROVIDER) to high(UNIDAC_PROVIDER) do
    if SameTextU(UNIDAC_PROVIDER[p], provider) then
    begin
      fDBMS := p;
      break;
    end;
  inherited Create(provider, aDatabaseName, aUserID, aPassWord);
  fSpecificOptions := TStringList.Create;
  opt := pointer(options);
  while opt <> nil do
  begin
    GetNextItem(opt, ';', namevalue);
    if namevalue <> '' then
      fSpecificOptions.Add(UTF8ToString(namevalue));
  end;
  case fDBMS of
    dSQLite:
      begin // UniDAC support of SQLite3 is just buggy
        fSpecificOptions.Values['ForceCreateDatabase'] := 'true';
        fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for UniDAC
      end;
    dFirebird:
      begin
    {$ifndef UNICODE}
        fForceUseWideString := true;
    {$endif}
        fSpecificOptions.Values['CharSet'] := 'UTF8';
        fSpecificOptions.Values['UseUnicode'] := 'true';
        fSpecificOptions.Values['CharLength'] := '2';
        fSpecificOptions.Values['DescribeParams'] := 'true';
        // http://www.devart.com/unidac/docs/index.html?ibprov_article.htm
      end;
    dOracle:
      begin
        fSpecificOptions.Values['UseUnicode'] := 'true';
        fSpecificOptions.Values['Direct'] := 'true';
        fSpecificOptions.Values['HOMENAME'] := '';
      end;
    dMySQL:
      begin
        // s.d. 30.11.19 Damit der Connect schneller geht ! CRVioTCP.pas WaitForConnect
        fSpecificOptions.Values['MySQL.ConnectionTimeout'] := '0';
      end;
    dMSSQL:
      begin
        if aUserID = '' then
          fSpecificOptions.Values['Authentication'] := 'auWindows';
        fSpecificOptions.Values['SQL Server.Provider'] := cMSSQLProvider;
        // s.d. 30.11.19 Damit der Connect im Direct Mode so Schnell ist wie mit prAuto/OleDB
        fSpecificOptions.Values['SQL Server.ConnectionTimeout'] := '0';
        // http://www.devart.com/unidac/docs/index.html?sqlprov_article.htm
      end;
    dPostgreSQL:
      begin  // thanks delphinium for the trick!
        fSpecificOptions.Values['CharSet'] := 'UTF8';
        fSpecificOptions.Values['UseUnicode'] := 'true';
      end;
  end;
end;

destructor TSQLDBUniDACConnectionProperties.Destroy;
begin
  fSpecificOptions.Free;
  inherited;
end;

procedure TSQLDBUniDACConnectionProperties.GetFields(const aTableName: RawUTF8;
  out Fields: TSQLDBColumnDefineDynArray);
var
  meta: TDAMetaData;
  n: integer;
  F: TSQLDBColumnDefine;
  FA: TDynArray;
  hasSubType: boolean;
  Owner, Table: RawUTF8;
begin
  meta := (MainConnection as TSQLDBUniDACConnection).fDatabase.CreateMetaData;
  try
    FA.Init(TypeInfo(TSQLDBColumnDefineDynArray), Fields, @n);
    FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
    FillChar(F, sizeof(F), 0);
    meta.MetaDataKind := 'Columns';
    Split(aTableName, '.', Owner, Table);
    if Table = '' then
    begin
      Table := Owner;
      Owner := '';
    end;
    if Owner = '' then
      Owner := MainConnection.Properties.DatabaseName; // itSDS
    if Owner <> '' then
      meta.Restrictions.Values['TABLE_SCHEMA'] := UTF8ToString(UpperCase(Owner))
    else
      meta.Restrictions.Values['SCOPE'] := 'LOCAL';
    meta.Restrictions.Values['TABLE_NAME'] := UTF8ToString(UpperCase(Table));
    meta.Open;
    hasSubType := meta.FindField('DATA_SUBTYPE') <> nil;
    while not meta.Eof do
    begin
      F.ColumnName := StringToUTF8(meta.FieldByName('COLUMN_NAME').AsString);
      F.ColumnTypeNative := StringToUTF8(meta.FieldByName('DATA_TYPE').AsString);
      if hasSubType then
        F.ColumnTypeNative := F.ColumnTypeNative +
          StringToUTF8(meta.FieldByName('DATA_SUBTYPE').AsString);
      F.ColumnLength := meta.FieldByName('DATA_LENGTH').AsInteger;
      F.ColumnScale := meta.FieldByName('DATA_SCALE').AsInteger;
      F.ColumnPrecision := meta.FieldByName('DATA_PRECISION').AsInteger;
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
      if F.ColumnType = ftUnknown then
      begin // UniDAC metadata failed -> use SQL
        inherited GetFields(aTableName, Fields);
        exit;
      end;
      FA.Add(F);
      meta.Next;
    end;
    Setlength(Fields, n);
    GetIndexesAndSetFieldsColumnIndexed(aTableName, Fields);
  finally
    meta.Free;
  end;
end;

procedure TSQLDBUniDACConnectionProperties.GetIndexes(const aTableName: RawUTF8;
  out Indexes: TSQLDBIndexDefineDynArray);
var
  meta, indexs: TDAMetaData;
  F: TSQLDBIndexDefine;
  FA: TDynArray;
  n: integer;
  ColName: RawUTF8;
  ndxName: string;
begin
  SetLength(Indexes, 0);
  FA.Init(TypeInfo(TSQLDBIndexDefineDynArray), Indexes, @n);
  fillchar(F, sizeof(F), 0);
  meta := (MainConnection as TSQLDBUniDACConnection).fDatabase.CreateMetaData;
  indexs := (MainConnection as TSQLDBUniDACConnection).fDatabase.CreateMetaData;
  try
    meta.MetaDataKind := 'Indexes';
    meta.Restrictions.Values['TABLE_NAME'] := UTF8ToString(UpperCase(aTableName));
    meta.Open;
    while not meta.Eof do
    begin
      ndxName := meta.FieldByName('INDEX_NAME').AsString;
      F.IndexName := StringToUTF8(ndxName);
      F.KeyColumns := '';
      indexs.MetaDataKind := 'indexcolumns';
      indexs.Restrictions.Values['TABLE_NAME'] := UTF8ToString(UpperCase(aTableName));
      indexs.Restrictions.Values['INDEX_NAME'] := ndxName;
      indexs.Open;
      while not indexs.Eof do
      begin
        ColName := StringToUTF8(indexs.FieldByName('COLUMN_NAME').AsString);
        if F.KeyColumns = '' then
          F.KeyColumns := ColName
        else
          F.KeyColumns := F.KeyColumns + ',' + ColName;
        indexs.Next;
      end;
      FA.Add(F);
      indexs.Close;
      meta.Next;
    end;
    SetLength(Indexes, n);
  finally
    indexs.Free;
    meta.Free;
  end;
end;

procedure TSQLDBUniDACConnectionProperties.GetForeignKeys;
var
  conn: TUniConnection;
begin
  conn := (MainConnection as TSQLDBUniDACConnection).Database;
  if conn = nil then
    exit;
  { TODO : get FOREIGN KEYS from UniDAC metadata ? }
end;

procedure TSQLDBUniDACConnectionProperties.GetTableNames(out Tables: TRawUTF8DynArray);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    (MainConnection as TSQLDBUniDACConnection).fDatabase.GetTableNames(List);
    StringListToRawUTF8DynArray(List, Tables);
    exit;
  finally
    List.Free;
  end;
  inherited;
end;

function TSQLDBUniDACConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBUniDACConnection.Create(self);
end;

class function TSQLDBUniDACConnectionProperties.URI(aServer: TSQLDBDefinition;
  const aServerName: RawUTF8; const aLibraryLocation: TFileName;
  aLibraryLocationAppendExePath: boolean): RawUTF8;
var
  Server, Port: RawUTF8;
begin
  if aServer < low(UNIDAC_PROVIDER) then
    result := ''
  else
    result := UNIDAC_PROVIDER[aServer];
  if result = '' then
    exit;
  if aLibraryLocation <> '' then
  begin
    result := result + '?ClientLibrary=';
    if aLibraryLocationAppendExePath then
      result := result + StringToUTF8(ExtractFilePath(ParamStr(0)));
    result := result + StringToUTF8(aLibraryLocation);
  end;
  if aServerName <> '' then
  begin
    Split(aServerName, ':', Server, Port);
    if aLibraryLocation = '' then
      result := result + '?'
    else
      result := result + ';';
    result := result + 'Server=' + Server;
    if Port <> '' then
      result := result + ';Port=' + Port;
  end;
end;


{ TSQLDBUniDACConnection }

procedure TSQLDBUniDACConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

constructor TSQLDBUniDACConnection.Create(aProperties: TSQLDBConnectionProperties);
var
  options: TStrings;
  PortNumber, i: Integer;
begin
  inherited Create(aProperties);
  if (aProperties.DBMS = dMSSQL) and (not SameText(cMSSQLProvider, 'prDirect')) then
    CoInit;
  fDatabase := TUniConnection.Create(nil);
  fDatabase.LoginPrompt := false;
  fDatabase.ProviderName := UTF8ToString(fProperties.ServerName);
  case aProperties.DBMS of
    dSQLite, dFirebird, dPostgreSQL, dMySQL, dDB2, dMSSQL:
      fDatabase.Database := UTF8ToString(fProperties.DatabaseName);
  else
    fDatabase.Server := UTF8ToString(fProperties.DatabaseName);
  end;
  fDatabase.Username := UTF8ToString(fProperties.UserID);
  fDatabase.Password := UTF8ToString(fProperties.PassWord);
  if aProperties.DBMS = dMySQL then
    // s.d. 30.11.19 Damit der Connect schneller geht
    fDatabase.SpecificOptions.Add('MySQL.ConnectionTimeout=0');
  if aProperties.DBMS = dMSSQL then
  begin
    fDatabase.SpecificOptions.Add('SQL Server.Provider=' + cMSSQLProvider);
    // s.d. 30.11.19 Damit der Connect im Direct Mode so Schnell ist wie mit prAuto/OleDB
    fDatabase.SpecificOptions.Add('SQL Server.ConnectionTimeout=0');
  end;
  // handle the options set by TSQLDBUniDACConnectionProperties.URI()
  options := (fProperties as TSQLDBUniDACConnectionProperties).fSpecificOptions;
  if fDatabase.Server = '' then
    fDatabase.Server := options.Values['Server'];
  if fDatabase.Database = '' then
    fDatabase.Database := options.Values['Database'];
  if (fDatabase.Port = 0) and TryStrToInt(options.Values['Port'], PortNumber) then
    fDatabase.Port := PortNumber;
  for i := 0 to options.Count - 1 do
    if FindRawUTF8(['Server', 'Database', 'Port'],
        StringToUTF8(options.Names[i]), false) < 0 then
      fDatabase.SpecificOptions.Add(options[i]);
end;

procedure TSQLDBUniDACConnection.Connect;
var
  Log: ISynLog;
begin
  if fDatabase = nil then
    raise ESQLDBUniDAC.CreateUTF8('%.Connect(%): Database=nil', [self,
      fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to ProviderName=% Database=% on Server=%',
    [fDatabase.ProviderName, fDatabase.Database, fDatabase.Server], self);
  try
    case fProperties.DBMS of
      dFirebird:
        if (fDatabase.Server = '') and not FileExists(fDatabase.Database) then
          with TUniScript.Create(nil) do // always create database for embedded Firebird
          try
            NoPreconnect := true;
            SQL.Text := UTF8ToString(fProperties.SQLCreateDatabase(fProperties.DatabaseName));
            Connection := fDatabase;
            Execute;
          finally
            Free;
          end;
    end;
    fDatabase.Open;
    inherited Connect; // notify any re-connection
    Log.Log(sllDB, 'Connected to % (%)', [fDatabase.ProviderName, fDatabase.ServerVersionFull]);
  except
    on E: Exception do
    begin
      Log.Log(sllError, E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBUniDACConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fDatabase <> nil then
      fDatabase.Close;
  end;
end;

destructor TSQLDBUniDACConnection.Destroy;
begin
  try
    Disconnect;
    if (fProperties.DBMS = dMSSQL) and (not SameText(cMSSQLProvider, 'prDirect')) then
      CoUnInit;
  except
    on Exception do

  end;
  inherited;
  FreeAndNil(fDatabase);
end;

function TSQLDBUniDACConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and fDatabase.Connected;
end;

function TSQLDBUniDACConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBUniDACStatement.Create(self);
end;

procedure TSQLDBUniDACConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSQLDBUniDACConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;



{ TSQLDBUniDACStatement }

procedure TSQLDBUniDACStatement.DataSetBindSQLParam(const aArrayIndex,
  aParamIndex: integer; const aParam: TSQLDBParam);
var
  P: TDAParam;
begin
  P := TDAParam(fQueryParams[aParamIndex]);
  if P.InheritsFrom(TDAParam) then
    with aParam do
      if (VinOut <> paramInOut) and (VType = SynTable.ftBlob) then
      begin
        P.ParamType := SQLParamTypeToDBParamType(VInOut);
        {$ifdef UNICODE}
        if aArrayIndex >= 0 then
          P.SetBlobData(Pointer(VArray[aArrayIndex]), Length(VArray[aArrayIndex]))
        else
          P.SetBlobData(Pointer(VData), Length(VData));
        {$else}
        if aArrayIndex >= 0 then
          P.AsString := VArray[aArrayIndex]
        else
          P.AsString := VData;
        {$endif UNICODE}
        exit;
      end;
  inherited DataSetBindSQLParam(aArrayIndex, aParamIndex, aParam);
end;

procedure TSQLDBUniDACStatement.DatasetCreate;
begin
  fQuery := TUniQuery.Create(nil);
  TUniQuery(fQuery).Connection := (fConnection as TSQLDBUniDACConnection).Database;
end;

function TSQLDBUniDACStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as TUniQuery).SQL.Text := aSQL;
  fQueryParams := TUniQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSQLDBUniDACStatement.DatasetExecSQL;
begin
  (fQuery as TUniQuery).Execute;
end;


{ ************ UniDac Database Engine Connection }

initialization
  TSQLDBUniDACConnectionProperties.RegisterClassNameForDefinition;

{$endif FPC} // UniDac is a Delphi-specific

end.

