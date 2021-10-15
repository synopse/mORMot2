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
  mormot.core.data,
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
  ESqlDBUniDAC = class(ESqlDBDataset);


  ///	connection properties definition using UniDAC database access
  TSqlDBUniDACConnectionProperties = class(TSqlDBDatasetConnectionProperties)
  protected
    fSpecificOptions: TStringList;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (UniDAC metadata may be used in the future)
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect via UniDAC database access
    // - aServerName shall contain the UniDAC provider name, e.g. 'Oracle' - you
    // can use the TSqlDBUniDACConnectionProperties.URI() to retrieve the
    // provider name from its mormot.db.sql.TSqlDBDefinition enumeration,
    // and optionally set some options, which will be added to the internal
    // SpecificOptions[]:
    // ! 'Oracle?ClientLibrary=oci64\oci.dll'
    // ! 'MySQL?Server=192.168.2.60;Port=3306', 'world', 'root', 'dev'
    // - aDatabaseName shall contain the database server name
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
    /// release internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBUniDACConnection instance
    function NewConnection: TSqlDBConnection; override;

    /// compute the UniDAC URI from a given database engine and server name
    // - the optional server name can contain a port number, specified after ':'
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSqlDBUniDACConnectionProperties.Create(
    // !   TSqlDBUniDACConnectionProperties.URI(dOracle,'','oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSqlDBUniDACConnectionProperties.Create(
    // !   TSqlDBUniDACConnectionProperties.URI(dFirebird,'',
    // !   'Firebird\fbembed.dll'),'databasefilename','','');
    // ! PropsMySQL := TSqlDBUniDACConnectionProperties.Create(
    // !   TSqlDBUniDACConnectionProperties.URI(dMySQL,'192.168.2.60:3306'),
    // !   'world', 'root', 'dev');
    class function URI(aServer: TSqlDBDefinition; const aServerName: RawUtf8;
      const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUtf8;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUtf8; out Fields: TSqlDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - this overridden method will use UniDAC metadata to retrieve the information
    procedure GetIndexes(const aTableName: RawUtf8; out Indexes: TSqlDBIndexDefineDynArray); override;
    /// allow to set the options specific to a UniDAC driver
    // - for instance, you can set for both SQLite3 and Firebird/Interbase:
    // ! Props.SpecificOptions.Values['ClientLibrary'] := ClientDllName;
    property SpecificOptions: TStringList
      read fSpecificOptions;
  end;


  ///	implements a direct connection via UniDAC database access
  TSqlDBUniDACConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fDatabase: TUniConnection;
  public
    /// prepare a connection for a specified UniDAC database access
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified database server using UniDAC
    // - should raise an ESqlDBUniDAC on error
    procedure Connect; override;
    /// stop connection to the specified database server using UniDAC
    // - should raise an ESqlDBUniDAC on error
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
    /// access to the associated UniDAC connection instance
    property Database: TUniConnection
      read fDatabase;
  end;

  ///	implements a statement via a UniDAC connection
  TSqlDBUniDACStatement = class(TSqlDBDatasetStatement)
  protected
    fBatchExecute: Boolean;
    /// initialize and set fQuery: TUniQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TUniQuery.ExecSQL
    procedure DatasetExecSQL; override;
    /// overriden by itSDS to properly handle UniDAC parameters
    procedure DataSetBindSqlParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSqlDBParam); override;
  public
  end;


const
  /// UniDAC provider names corresponding to mormot.db.sql recognized SQL engines
  UNIDAC_PROVIDER: array[dOracle..high(TSqlDBDefinition)] of RawUtf8 = (
    'Oracle', 'SQL Server', 'Access', 'MySQL', 'SQLite', 'InterBase', 
    'NexusDB', 'PostgreSQL', 'DB2', '');


implementation

uses
  DAScript,
  CRDataTypeMap,
  DBAccess;


{ TSqlDBUniDACConnectionProperties }

constructor TSqlDBUniDACConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
var
  p: TSqlDBDefinition;
  provider, options, namevalue: RawUtf8;
  opt: PUtf8Char;
begin
  Split(aServerName, '?', provider, options);
  for p := Low(UNIDAC_PROVIDER) to high(UNIDAC_PROVIDER) do
    if SameTextU(UNIDAC_PROVIDER[p], provider) then
    begin
      fDbms := p;
      break;
    end;
  inherited Create(provider, aDatabaseName, aUserID, aPassWord);
  fSpecificOptions := TStringList.Create;
  opt := pointer(options);
  while opt <> nil do
  begin
    GetNextItem(opt, ';', namevalue);
    if namevalue <> '' then
      fSpecificOptions.Add(Utf8ToString(namevalue));
  end;
  case fDbms of
    dSQLite:
      begin
        // UniDAC support of SQLite3 is just buggy
        {$ifdef FPC}
        fSpecificOptions.Values['UseUnicode'] := 'true'; // FPC strings do like UTF8
        {$else}
        {$ifndef UNICODE}
        fForceUseWideString := true; // for non-unicode Delphi
        {$endif UNICODE}
        {$endif FPC}
        fSpecificOptions.Values['ForceCreateDatabase'] := 'true';
        fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for UniDAC
      end;
    dFirebird:
      begin
        {$ifdef FPC}
        fSpecificOptions.Values['UseUnicode'] := 'true'; // FPC strings do like UTF8
        {$else}
        {$ifndef UNICODE}
        fForceUseWideString := true;
        {$endif UNICODE}
        {$endif FPC}
        fSpecificOptions.Values['CharSet'] := 'UTF8';
        fSpecificOptions.Values['CharLength'] := '2';
        fSpecificOptions.Values['DescribeParams'] := 'true';
        // http://www.devart.com/unidac/docs/index.html?ibprov_article.htm
      end;
    dOracle:
      begin
        {$ifdef FPC}
        fSpecificOptions.Values['UseUnicode'] := 'true'; // FPC strings do like UTF8
        {$else}
        {$ifndef UNICODE}
        fForceUseWideString := true; // for non-unicode Delphi
        {$endif UNICODE}
        {$endif FPC}
        fSpecificOptions.Values['Direct'] := 'true';
        fSpecificOptions.Values['HOMENAME'] := '';
      end;
    dMySQL:
      begin
        {$ifdef FPC}
        fSpecificOptions.Values['UseUnicode'] := 'true'; // FPC strings do like UTF8
        {$else}
        {$ifndef UNICODE}
        fForceUseWideString := true;
        {$endif UNICODE}
        {$endif FPC}
        // s.d. 30.11.19 Damit der Connect schneller geht ! CRVioTCP.pas WaitForConnect
        fSpecificOptions.Values['MySQL.ConnectionTimeout'] := '0';
      end;
    dMSSQL:
      begin
        {$ifndef FPC}
        {$ifndef UNICODE}
        fForceUseWideString := true;
        {$endif UNICODE}
        {$endif FPC}
        if aUserID = '' then
          fSpecificOptions.Values['Authentication'] := 'auWindows';
        fSpecificOptions.Values['SQL Server.Provider'] := cMSSQLProvider;
        // s.d. 30.11.19 Damit der Connect im Direct Mode so Schnell ist wie mit prAuto/OleDB
        fSpecificOptions.Values['SQL Server.ConnectionTimeout'] := '0';
        // http://www.devart.com/unidac/docs/index.html?sqlprov_article.htm
      end;
    dPostgreSQL:
      begin  // thanks delphinium for the trick!
        {$ifdef FPC}
        fSpecificOptions.Values['UseUnicode'] := 'true'; // FPC strings do like UTF8
        {$else}
        {$ifndef UNICODE}
        fForceUseWideString := true;
        {$endif UNICODE}
        {$endif FPC}
        fSpecificOptions.Values['CharSet'] := 'UTF8';
      end;
  end;
end;

destructor TSqlDBUniDACConnectionProperties.Destroy;
begin
  fSpecificOptions.Free;
  inherited;
end;

procedure TSqlDBUniDACConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  meta: TDAMetaData;
  n: integer;
  F: TSqlDBColumnDefine;
  FA: TDynArray;
  hasSubType: boolean;
  Owner, Table: RawUtf8;
begin
  meta := (MainConnection as TSqlDBUniDACConnection).fDatabase.CreateMetaData;
  try
    FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Fields, @n);
    FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
    FillCharFast(F, SizeOf(F), 0);
    meta.MetaDataKind := 'Columns';
    Split(aTableName, '.', Owner, Table);
    if Table = '' then
    begin
      Table := Owner;
      Owner := '';
    end;
    if (Owner = '') and
       (fDbms <> dOracle) then
      Owner := MainConnection.Properties.DatabaseName; // itSDS
    if Owner <> '' then
      meta.Restrictions.Values['TABLE_SCHEMA'] := Utf8ToString(UpperCase(Owner))
    else
      meta.Restrictions.Values['SCOPE'] := 'LOCAL';
    meta.Restrictions.Values['TABLE_NAME'] := Utf8ToString(UpperCase(Table));
    meta.Open;
    hasSubType := meta.FindField('DATA_SUBTYPE') <> nil;
    while not meta.Eof do
    begin
      F.ColumnName := StringToUtf8(meta.FieldByName('COLUMN_NAME').AsString);
      F.ColumnTypeNative := StringToUtf8(meta.FieldByName('DATA_TYPE').AsString);
      if hasSubType then
        F.ColumnTypeNative := F.ColumnTypeNative +
          StringToUtf8(meta.FieldByName('DATA_SUBTYPE').AsString);
      F.ColumnLength := meta.FieldByName('DATA_LENGTH').AsInteger;
      F.ColumnScale := meta.FieldByName('DATA_SCALE').AsInteger;
      F.ColumnPrecision := meta.FieldByName('DATA_PRECISION').AsInteger;
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
      if F.ColumnType = ftUnknown then
      begin
        // UniDAC metadata failed -> use SQL
        Fields := nil;
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

procedure TSqlDBUniDACConnectionProperties.GetIndexes(const aTableName: RawUtf8;
  out Indexes: TSqlDBIndexDefineDynArray);
var
  meta, indexs: TDAMetaData;
  F: TSqlDBIndexDefine;
  FA: TDynArray;
  n: integer;
  ColName, Owner, Table: RawUtf8;
  ndxName: string;
begin
  Indexes := nil;
  FA.Init(TypeInfo(TSqlDBIndexDefineDynArray), Indexes, @n);
  FillCharFast(F, SizeOf(F), 0);
  meta := (MainConnection as TSqlDBUniDACConnection).fDatabase.CreateMetaData;
  indexs := (MainConnection as TSqlDBUniDACConnection).fDatabase.CreateMetaData;
  try
    meta.MetaDataKind := 'Indexes';
    meta.Restrictions.Values['TABLE_NAME'] := Utf8ToString(UpperCase(aTableName));
    Split(aTableName, '.', Owner, Table);
    if Table = '' then 
    begin
      Table := Owner;
      Owner := '';
    end;
    if (Owner = '') and 
       (fDbms <> dOracle) then
      Owner := MainConnection.Properties.DatabaseName; // itSDS
    if Owner <> '' then
      meta.Restrictions.Values['TABLE_SCHEMA'] := UTF8ToString(UpperCase(Owner))
    else
      meta.Restrictions.Values['SCOPE'] := 'LOCAL';
    meta.Restrictions.Values['TABLE_NAME'] := UTF8ToString(UpperCase(Table));

    meta.Open;
    while not meta.Eof do
    begin
      ndxName := meta.FieldByName('INDEX_NAME').AsString;
      F.IndexName := StringToUtf8(ndxName);
      F.KeyColumns := '';
      indexs.MetaDataKind := 'indexcolumns';
      if Owner <> '' then
        indexs.Restrictions.Values['TABLE_SCHEMA'] := UTF8ToString(UpperCase(Owner)) 
      else
        indexs.Restrictions.Values['SCOPE'] := 'LOCAL';
      indexs.Restrictions.Values['TABLE_NAME'] := Utf8ToString(UpperCase(aTableName));
      indexs.Restrictions.Values['INDEX_NAME'] := ndxName;
      indexs.Open;
      while not indexs.Eof do
      begin
        ColName := StringToUtf8(indexs.FieldByName('COLUMN_NAME').AsString);
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

procedure TSqlDBUniDACConnectionProperties.GetForeignKeys;
var
  conn: TUniConnection;
begin
  conn := (MainConnection as TSqlDBUniDACConnection).Database;
  if conn = nil then
    exit;
  { TODO : get FOREIGN KEYS from UniDAC metadata ? }
end;

procedure TSqlDBUniDACConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    (MainConnection as TSqlDBUniDACConnection).fDatabase.GetTableNames(List);
    StringListToRawUtf8DynArray(List, Tables);
    exit;
  finally
    List.Free;
  end;
  inherited;
end;

function TSqlDBUniDACConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBUniDACConnection.Create(self);
end;

class function TSqlDBUniDACConnectionProperties.URI(aServer: TSqlDBDefinition;
  const aServerName: RawUtf8; const aLibraryLocation: TFileName;
  aLibraryLocationAppendExePath: boolean): RawUtf8;
var
  Server, Port: RawUtf8;
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
      result := result + StringToUtf8(ExtractFilePath(ParamStr(0)));
    result := result + StringToUtf8(aLibraryLocation);
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


{ TSqlDBUniDACConnection }

procedure TSqlDBUniDACConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

constructor TSqlDBUniDACConnection.Create(aProperties: TSqlDBConnectionProperties);
var
  options: TStrings;
  PortNumber, i: integer;
begin
  inherited Create(aProperties);
  if (aProperties.Dbms = dMSSQL) and
     not SameText(cMSSQLProvider, 'prDirect') then
    CoInit;
  fDatabase := TUniConnection.Create(nil);
  fDatabase.LoginPrompt := false;
  fDatabase.ProviderName := Utf8ToString(fProperties.ServerName);
  case aProperties.Dbms of
    dSQLite, dFirebird, dPostgreSQL, dMySQL, dDB2, dMSSQL:
      fDatabase.Database := Utf8ToString(fProperties.DatabaseName);
  else
    fDatabase.Server := Utf8ToString(fProperties.DatabaseName);
  end;
  fDatabase.Username := Utf8ToString(fProperties.UserID);
  fDatabase.Password := Utf8ToString(fProperties.PassWord);
  if aProperties.Dbms = dMySQL then
    // s.d. 30.11.19 Damit der Connect schneller geht
    fDatabase.SpecificOptions.Add('MySQL.ConnectionTimeout=0');
  if aProperties.Dbms = dMSSQL then
  begin
    fDatabase.SpecificOptions.Add('SQL Server.Provider=' + cMSSQLProvider);
    // s.d. 30.11.19 Damit der Connect im Direct Mode so Schnell ist wie mit prAuto/OleDB
    fDatabase.SpecificOptions.Add('SQL Server.ConnectionTimeout=0');
  end;
  // handle the options set by TSqlDBUniDACConnectionProperties.URI()
  options := (fProperties as TSqlDBUniDACConnectionProperties).fSpecificOptions;
  if fDatabase.Server = '' then
    fDatabase.Server := options.Values['Server'];
  if fDatabase.Database = '' then
    fDatabase.Database := options.Values['Database'];
  if (fDatabase.Port = 0) and TryStrToInt(options.Values['Port'], PortNumber) then
    fDatabase.Port := PortNumber;
  for i := 0 to options.Count - 1 do
    if FindRawUtf8(['Server', 'Database', 'Port'],
        StringToUtf8(options.Names[i]), false) < 0 then
      fDatabase.SpecificOptions.Add(options[i]);
end;

procedure TSqlDBUniDACConnection.Connect;
var
  Log: ISynLog;
begin
  if fDatabase = nil then
    raise ESqlDBUniDAC.CreateUtf8('%.Connect(%): Database=nil', [self,
      fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to ProviderName=% Database=% on Server=%',
    [fDatabase.ProviderName, fDatabase.Database, fDatabase.Server], self);
  try
    case fProperties.Dbms of
      dFirebird:
        if (fDatabase.Server = '') and
           not FileExists(fDatabase.Database) then
          with TUniScript.Create(nil) do // always create database for embedded Firebird
          try
            NoPreconnect := true;
            SQL.Text := Utf8ToString(fProperties.SQLCreateDatabase(fProperties.DatabaseName));
            Connection := fDatabase;
            Execute;
          finally
            Free;
          end;
    end;
    fDatabase.Open;
    inherited Connect; // notify any re-connection
    Log.Log(sllDB, 'Connected to % (%)',
      [fDatabase.ProviderName, fDatabase.ServerVersionFull]);
  except
    on E: Exception do
    begin
      Log.Log(sllError, E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSqlDBUniDACConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fDatabase <> nil then
      fDatabase.Close;
  end;
end;

destructor TSqlDBUniDACConnection.Destroy;
begin
  try
    Disconnect;
    if (fProperties.Dbms = dMSSQL) and
       not SameText(cMSSQLProvider, 'prDirect') then
      CoUnInit;
  except
    on Exception do

  end;
  inherited;
  FreeAndNil(fDatabase);
end;

function TSqlDBUniDACConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and
            fDatabase.Connected;
end;

function TSqlDBUniDACConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBUniDACStatement.Create(self);
end;

procedure TSqlDBUniDACConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSqlDBUniDACConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;



{ TSqlDBUniDACStatement }

procedure TSqlDBUniDACStatement.DataSetBindSqlParam(const aArrayIndex,
  aParamIndex: integer; const aParam: TSqlDBParam);
var
  P: TDAParam;
  i: Integer;
  tmp: RawUTF8;
  StoreVoidStringAsNull: boolean;
begin
  P := TDAParam(fQueryParams[aParamIndex]);
  if not P.InheritsFrom(TDAParam) then 
  begin
    inherited DataSetBindSQLParam(aArrayIndex, aParamIndex, aParam);
    Exit;
  end;
  if fDatasetSupportBatchBinding then
    fBatchExecute := (aArrayIndex < 0) and
                     (fParamsArrayCount > 0)
  else
    fBatchExecute := false;
  if fBatchExecute then
    P.ValueCount := fParamsArrayCount
  else
    P.ValueCount := 1;
  with aParam do begin
    P.ParamType := SQLParamTypeToDBParamType(VInOut);
    if VinOut <> paramInOut then
      case VType of
        mormot.db.core.ftNull:
          if fBatchExecute then
            for i := 0 to fParamsArrayCount - 1 do
              P.Values[i].Clear 
          else
            P.Clear;
        mormot.db.core.ftInt64:
          begin
            if fBatchExecute then
              for i := 0 to fParamsArrayCount-1 do
                if VArray[i] = 'null' then
                  P.Values[i].Clear
                else
                  P.Values[i].AsLargeInt := GetInt64(pointer(VArray[i]))
            else if aArrayIndex >= 0 then
              if VArray[aArrayIndex] = 'null' then
                P.Clear
              else
                P.AsLargeInt := GetInt64(pointer(VArray[aArrayIndex]))
            else
              P.AsLargeInt := VInt64;
          end;
        mormot.db.core.ftDouble:
          if fBatchExecute then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Values[i].Clear
              else
                P.Values[i].AsFloat := GetExtended(pointer(VArray[i]))
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsFloat := GetExtended(pointer(VArray[aArrayIndex]))
          else
            P.AsFloat := PDouble(@VInt64)^;
        mormot.db.core.ftCurrency:
          if fBatchExecute then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Values[i].Clear
              else
                P.Values[i].AsCurrency := StrToCurrency(pointer(VArray[i]))
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsCurrency := StrToCurrency(pointer(VArray[aArrayIndex]))
          else
            P.AsCurrency := PCurrency(@VInt64)^;
        mormot.db.core.ftDate:
          if fBatchExecute then
            for i := 0 to fParamsArrayCount - 1 do
            if VArray[i] = 'null' then
              P.Values[i].Clear
            else
            begin
              UnQuoteSQLStringVar(pointer(VArray[i]), tmp);
              P.Values[i].AsDateTime := Iso8601ToDateTime(tmp);
            end
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
            begin
              UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]), tmp);
              P.AsDateTime := Iso8601ToDateTime(tmp);
            end
          else
            P.AsDateTime := PDateTime(@VInt64)^;
        mormot.db.core.ftUTF8:
          if fBatchExecute then
          begin
            StoreVoidStringAsNull := fConnection.Properties.StoreVoidStringAsNull;
            for i := 0 to fParamsArrayCount - 1 do
              if (VArray[i] = 'null') or
                 (StoreVoidStringAsNull and
                  (VArray[i] = #39#39)) then
                P.Values[i].Clear
              else
              begin
                UnQuoteSQLStringVar(pointer(VArray[i]), tmp);
                {$ifdef UNICODE}
                P.Values[i].AsWideString := UTF8ToString(tmp);
                {$else}
                if fForceUseWideString then
                  P.Values[i].AsWideString := UTF8ToWideString(tmp)
                else
                  P.Values[i].AsString := UTF8ToString(tmp);
                {$endif UNICODE}
              end
          end
          else if aArrayIndex >= 0 then
            if (VArray[aArrayIndex] = 'null') or
               (fConnection.Properties.StoreVoidStringAsNull and
                (VArray[aArrayIndex] = #39#39)) then
              P.Clear
            else
            begin
              UnQuoteSQLStringVar(pointer(VArray[aArrayIndex]), tmp);
              {$ifdef UNICODE}
              P.AsWideString := UTF8ToString(tmp);
              {$else}
              if fForceUseWideString then
                P.AsWideString := UTF8ToWideString(tmp)
              else
                P.AsString := UTF8ToString(tmp);
              {$endif UNICODE}
          end
          else if (VData = '') and
                  fConnection.Properties.StoreVoidStringAsNull then
            P.Clear
          else
            {$ifdef UNICODE}
            P.AsWideString := UTF8ToString(VData);
            {$else}
            if not fForceUseWideString then
              P.AsString := UTF8ToString(VData)
            else
              P.AsWideString := UTF8ToWideString(VData);
            {$endif UNICODE}
        mormot.db.core.ftBlob:
          if fBatchExecute then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Values[i].Clear
              else
              {$ifdef UNICODE}
              begin
                P.Values[i].AsBlobRef.Clear;
                P.Values[i].AsBlobRef.Write(0, Length(VArray[aArrayIndex]),
                  Pointer(VArray[aArrayIndex]));
              end
              {$else}
              P.Values[i].AsString := VArray[aArrayIndex]
              {$endif UNICODE}
          else
          if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
          {$ifdef UNICODE}
            begin
              P.AsBlobRef.Clear;
              P.AsBlobRef.Write(0, Length(VArray[aArrayIndex]),
                Pointer(VArray[aArrayIndex]));
            end
          else
          begin
            P.AsBlobRef.Clear;
            P.AsBlobRef.Write(0, Length(VData), Pointer(VData));
          end;
          {$else}
            P.AsString := VArray[aArrayIndex]
          else
            P.AsString := VData
          {$endif UNICODE}
        else
          raise ESQLDBUniDAC.CreateUTF8(
            '%.DataSetBindSQLParam: invalid type % on bound parameter #%',
            [Self, ord(VType), aParamIndex + 1]);
      end;
  end;
end;

procedure TSqlDBUniDACStatement.DatasetCreate;
begin
  fQuery := TUniQuery.Create(nil);
  TUniQuery(fQuery).Connection :=
    (fConnection as TSqlDBUniDACConnection).Database;
  fDatasetSupportBatchBinding := true;
end;

function TSQLDBUniDACStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as TUniQuery).SQL.Text := aSQL;
  TUniQuery(fQuery).Prepare;
  fQueryParams := TUniQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSQLDBUniDACStatement.DatasetExecSQL;
begin
  if fBatchExecute then
    (fQuery as TUniQuery).Execute(fParamsArrayCount) 
  else
    (fQuery as TUniQuery).Execute;
end;


{ ************ UniDac Database Engine Connection }

initialization
  TSqlDBUniDACConnectionProperties.RegisterClassNameForDefinition;

{$endif FPC} // UniDac is a Delphi-specific

end.

