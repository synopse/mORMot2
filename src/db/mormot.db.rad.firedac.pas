/// Database Framework for FireDac/AnyDac TDataSet Connection
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.rad.firedac;

{
  *****************************************************************************

   Third Party FireDac/AnyDac Components Database Access for mormot.db.rad
    - FireDac Database Engine Connection

  *****************************************************************************
}

interface

{$ifdef FPC} // FireDac is a Delphi-specific feature

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
  {$ifdef ISDELPHIXE5}
  FireDAC.Comp.Client, // FireDac units
  FireDAC.Stan.Param;
  {$else}
  uADCompClient,       // AnyDac units
  uADStanParam;
  {$endif ISDELPHIXE5}


{ ************ FireDac Database Engine Connection }

type
  /// Exception type associated to FireDAC/AnyDAC database access
  ESqlDBFireDAC = class(ESqlDBDataset);


  ///	connection properties definition using FireDAC/AnyDAC database access
  TSqlDBFireDACConnectionProperties = class(TSqlDBDatasetConnectionProperties)
  protected
    fFireDACOptions: TStringList;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (FireDAC metadata may be used in the future)
    procedure GetForeignKeys; override;
  public
    /// initialize the properties to connect via FireDAC/AnyDAC database access
    // - aServerName shall contain the FireDAC provider DriverID, e.g. 'Ora', and
    // some optional parameters (e.g. remote server name if needed), after a '?'
    // and separated by ';' - for instance:
    // ! Create('Ora','TNSNAME','User','Password');
    // ! Create('Ora?CharacterSet=cl8mswin1251','TNSNAME','User','Password');
    // ! Create('MSSQL?Server=127.0.0.1\SQLEXPRESS','Northwind','User','Password');
    // ! Create('MSSQL?Server=.\SQLEXPRESS;OSAuthent=Yes','','','');
    // ! Create('MSAcc','c:\data\access.mdb','','');
    // ! Create('MySQL?Server=127.0.0.1;Port=3306','MyDB','User','Password');
    // ! Create('SQLite','c:\data\myapp.db3','','');
    // ! Create('SQLite',SQLITE_MEMORY_DATABASE_NAME,'','');
    // ! Create('IB','127.0.0.1:C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('IB?Server=my_host/3055','C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('IB?CreateDatabase=Yes','127.0.0.1:C:\ib\ADDEMO_IB2007.IB','User','Password');
    // ! Create('DB2?Server=localhost;Port=50000','SAMPLE','db2admin','db2Password');
    // ! Create('PG?Server=localhost;Port=5432','postgres','postgres','postgresPassword');
    // ! Create('MySQL?Server=localhost;Port=3306','test','root','');
    // - aDatabaseName shall contain the database server name
    // - note that you need to link the FireDAC driver by including the
    // expected uADPhys*.pas / FireDAC.Phy.*.pas units into a uses clause
    // of your application, e.g. uADPhysOracle, uADPhysMSSQL, uADPhysMSAcc,
    // uADPhysMySQL, uADPhysSQLite, uADPhysIB or uADPhysDB2 (depending on the
    // expected provider) - or FireDAC.Phys.Oracle, FireDAC.Phys.MSAcc,
    // FireDAC.Phys.MSSQL, FireDAC.Phys.SQLite, FireDAC.Phys.IB, FireDAC.Phys.PG
    // or FireDAC.Phys.DB2 since Delphi XE5 namespace modifications
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUtf8); override;
    /// release internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSqlDBFireDACConnection instance
    function NewConnection: TSqlDBConnection; override;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUtf8; out Fields: TSqlDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetTableNames(out Tables: TRawUtf8DynArray); override;
    /// retrieve the advanced indexed information of a specified Table
    // - this overridden method will use FireDAC metadata to retrieve the information
    procedure GetIndexes(const aTableName: RawUtf8; out Indexes: TSqlDBIndexDefineDynArray); override;

    /// allow to set the options specific to a FireDAC driver
    // - by default, ServerName, DatabaseName, UserID and Password are set by
    // the Create() constructor according to the underlying FireDAC driver
    // - you can add some additional options here
    property Parameters: TStringList
      read fFireDACOptions;
  end;


  ///	implements a direct connection via FireDAC/AnyDAC database access
  TSqlDBFireDACConnection = class(TSqlDBConnectionThreadSafe)
  protected
    fDatabase: {$ifdef ISDELPHIXE5}TFDConnection{$else}TADConnection{$endif};
  public
    /// prepare a connection for a specified FireDAC/AnyDAC database access
    constructor Create(aProperties: TSqlDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified database server using FireDAC
    // - should raise an ESqlDBFireDAC on error
    procedure Connect; override;
    /// stop connection to the specified database server using FireDAC
    // - should raise an ESqlDBFireDAC on error
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
    /// access to the associated FireDAC connection instance
    property Database: {$ifdef ISDELPHIXE5}TFDConnection{$else}TADConnection{$endif}
      read fDatabase;
  end;

  ///	implements a statement via a FireDAC connection
  // - this specific version will handle the FireDAC specific parameter classes
  // - it will also handle Array DML commands, if possible
  TSqlDBFireDACStatement = class(TSqlDBDatasetStatementAbstract)
  protected
    fQueryParams: {$ifdef ISDELPHIXE5}TFDParams{$else}TADParams{$endif};
    fPreparedUseArrayDML: boolean;
    /// initialize and set fQuery: TUniQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TUniQuery.ExecSQL
    procedure DatasetExecSQL; override;
    /// bind SqlDBParam to TQuery-like param using fQueryParams: DB.TParams
    procedure DataSetBindSqlParam(const aArrayIndex, aParamIndex: integer;
      const aParam: TSqlDBParam); override;
    /// set the returned parameter after a stored proc execution
    procedure DataSetOutSqlParam(const aParamIndex: integer;
      var aParam: TSqlDBParam); override;
  public
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESqlDBFireDAC on any error
    procedure Prepare(const aSQL: RawUtf8; ExpectResults: boolean = false); overload; override;
  end;


const
  /// FireDAC DriverID values corresponding to mormot.db.sql recognized SQL engines
  {$ifdef ISDELPHIXE5}
  FIREDAC_PROVIDER: array[dOracle..high(TSqlDBDefinition)] of RawUtf8 = (
    'Ora', 'MSSQL', 'MSAcc', 'MySQL', 'SQLite', 'FB', '', 'PG', 'DB2', 'Infx');
  {$else}
  FIREDAC_PROVIDER: array[dOracle..high(TSqlDBDefinition)] of RawUtf8 = (
    'Ora', 'MSSQL', 'MSAcc', 'MySQL', 'SQLite', 'IB', '', 'PG', 'DB2', 'Infx');
  {$endif ISDELPHIXE5}

implementation


{$ifdef ISDELPHIXE5}

uses
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.DApt,
  FireDAC.Stan.Async;

type
  TADConnection = TFDConnection;
  TADQuery = TFDQuery;
  TADMetaInfoQuery = TFDMetaInfoQuery;
  TADParam = TFDParam;
  TADParams = TFDParams;
  TADPhysMetaInfoKind = TFDPhysMetaInfoKind;

{$else}

uses
  uADPhysIntf,
  uADStanDef,
  uADDAptManager,
  uADStanAsync;

{$endif ISDELPHIXE5}


{ TSqlDBFireDACConnectionProperties }

constructor TSqlDBFireDACConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUtf8);
var
  p: TSqlDBDefinition;
  server, options, namevalue: RawUtf8;
  opt: PUtf8Char;
begin
  Split(aServerName, '?', server, options);
  if server <> '' then
    for p := Low(FIREDAC_PROVIDER) to high(FIREDAC_PROVIDER) do
      if SameTextU(FIREDAC_PROVIDER[p], server) then
      begin
        fDbms := p;
        break;
      end;
  inherited Create(server, aDatabaseName, aUserID, aPassWord);
  fOnBatchInsert := nil; // MultipleValuesInsert is slower than FireDAC ArrayDML
  fFireDACOptions := TStringList.Create;
  if ((fDbms < low(FIREDAC_PROVIDER)) or
      (fDbms > high(FIREDAC_PROVIDER))) and
     (fDbms <> dNexusDB) then
    if SameTextU(server, 'ASA') then
      fDbms := dMSSQL
    else
    begin
      for p := Low(FIREDAC_PROVIDER) to high(FIREDAC_PROVIDER) do
        namevalue := ' ' + namevalue + FIREDAC_PROVIDER[p];
      raise ESqlDBFireDAC.CreateUtf8('%.Create: unknown provider - available:%',
        [self, namevalue]);
    end;
  if server = '' then
    server := FIREDAC_PROVIDER[fDbms];
  fFireDACOptions.Text := FormatString(
    'DriverID=%'#13#10'User_Name=%'#13#10'Password=%'#13#10'Database=%',
    [server, fUserId, fPassWord, fDatabaseName]);
  opt := pointer(options);
  while opt <> nil do
  begin
    GetNextItem(opt, ';', namevalue);
    if namevalue <> '' then
      fFireDACOptions.Add(Utf8ToString(namevalue));
  end;
  case fDbms of
    dSQLite:
      begin
        if fFireDACOptions.Values['CharacterSet'] = '' then
          // force UTF-8 for mormot.db.sql
          fFireDACOptions.Values['CharacterSet'] := 'UTF8';
        {$ifdef UNICODE}
        // CreateUTF16 is the default value for Delphi 2009+
        if fFireDACOptions.Values['OpenMode'] = '' then
          // force UTF-8 for mormot.db.sql
          fFireDACOptions.Values['OpenMode'] := 'CreateUtf8';
        {$else}
        // as expected by FireDAC when UTF-8 is enabled
        ForceUseWideString := true;
        {$endif UNICODE}
        // SQLite3 INTEGER = 32bit for FireDAC
        fSQLCreateField[ftInt64] := ' BIGINT';
      end;
    dFirebird, dMySQL, dPostgreSQL, dDB2:
      begin
        if fFireDACOptions.Values['CharacterSet'] = '' then
          // force UTF-8 for mormot.db.sql
          fFireDACOptions.Values['CharacterSet'] := 'UTF8';
        {$ifndef UNICODE}
        ForceUseWideString := true; // as expected by FireDAC when UTF-8 is enabled
        {$endif}
      end;
  end;
end;

destructor TSqlDBFireDACConnectionProperties.Destroy;
begin
  fFireDACOptions.Free;
  inherited;
end;

procedure TSqlDBFireDACConnectionProperties.GetTableNames(out Tables: TRawUtf8DynArray);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    (MainConnection as TSqlDBFireDACConnection).fDatabase.GetTableNames('', '',
      '', List, [osMy], [tkTable]);
    StringListToRawUtf8DynArray(List, Tables);
    exit;
  finally
    List.Free;
  end;
  inherited;
end;

procedure TSqlDBFireDACConnectionProperties.GetFields(const aTableName: RawUtf8;
  out Fields: TSqlDBColumnDefineDynArray);
var
  meta: TADMetaInfoQuery;
  n: integer;
  F: TSqlDBColumnDefine;
  FA: TDynArray;
begin
  meta := TADMetaInfoQuery.Create(nil);
  try
    meta.Connection := (MainConnection as TSqlDBFireDACConnection).fDatabase;
    FA.Init(TypeInfo(TSqlDBColumnDefineDynArray), Fields, @n);
    FA.Compare := SortDynArrayAnsiStringI; // FA.Find() case insensitive
    FillCharFast(F, sizeof(F), 0);
    meta.MetaInfoKind := mkTableFields;
    meta.ObjectName := Utf8ToString(UpperCase(aTableName));
    meta.Open;
    while not meta.Eof do
    begin
      F.ColumnName := StringToUtf8(meta.FieldByName('COLUMN_NAME').AsString);
      F.ColumnTypeNative := StringToUtf8(meta.FieldByName('COLUMN_TYPENAME').AsString);
      F.ColumnLength := meta.FieldByName('COLUMN_LENGTH').AsInteger;
      F.ColumnScale := meta.FieldByName('COLUMN_SCALE').AsInteger;
      F.ColumnPrecision := meta.FieldByName('COLUMN_PRECISION').AsInteger;
      { TODO : retrieve ColumnType from high-level FireDAC type information }
      F.ColumnType := ColumnTypeNativeToDB(F.ColumnTypeNative, F.ColumnScale);
      FA.Add(F);
      meta.Next;
    end;
    Setlength(Fields, n);
    GetIndexesAndSetFieldsColumnIndexed(aTableName, Fields);
  finally
    meta.Free;
  end;
end;

procedure TSqlDBFireDACConnectionProperties.GetIndexes(const aTableName: RawUtf8;
  out Indexes: TSqlDBIndexDefineDynArray);
var
  kind: boolean;
  meta, indexs: TADMetaInfoQuery;
  TableName: string;
  ColName: RawUtf8;
  F: TSqlDBIndexDefine;
  FA: TDynArray;
  n: integer;
const
  MASTER: array[boolean] of TADPhysMetaInfoKind = (mkPrimaryKey, mkIndexes);
  CHILD: array[boolean] of TADPhysMetaInfoKind = (mkPrimaryKeyFields, mkIndexFields);
begin
  TableName := Utf8ToString(UpperCase(aTableName));
  FA.Init(TypeInfo(TSqlDBIndexDefineDynArray), Indexes, @n);
  FillCharFast(F, sizeof(F), 0);
  meta := TADMetaInfoQuery.Create(nil);
  indexs := TADMetaInfoQuery.Create(nil);
  try
    meta.Connection := (MainConnection as TSqlDBFireDACConnection).fDatabase;
    indexs.Connection := meta.Connection;
    for kind := true to true do
    begin
      // primary keys may not be indexed
      meta.MetaInfoKind := MASTER[kind];
      meta.ObjectName := TableName;
      meta.Open;
      while not meta.Eof do
      begin
        indexs.MetaInfoKind := CHILD[kind];
        indexs.BaseObjectName := TableName;
        indexs.ObjectName := meta.FieldByName('INDEX_NAME').AsString;
        indexs.Open;
        F.IndexName := StringToUtf8(indexs.ObjectName);
        F.IsPrimaryKey := not kind;
        F.KeyColumns := '';
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
      meta.Close;
    end;
    SetLength(Indexes, n);
  finally
    indexs.Free;
    meta.Free;
  end;
end;

procedure TSqlDBFireDACConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from FireDAC metadata using mkForeignKeys  }
end;

function TSqlDBFireDACConnectionProperties.NewConnection: TSqlDBConnection;
begin
  result := TSqlDBFireDACConnection.Create(self);
end;


{ TSqlDBFireDACConnection }

procedure TSqlDBFireDACConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

constructor TSqlDBFireDACConnection.Create(aProperties: TSqlDBConnectionProperties);
begin
  inherited Create(aProperties);
  fDatabase := TADConnection.Create(nil);
  fDatabase.ResourceOptions.SilentMode := True; // no need for wait cursor
  fDatabase.LoginPrompt := false;
  fDatabase.Params.Text := (fProperties as TSqlDBFireDACConnectionProperties).
    fFireDACOptions.Text;
end;

procedure TSqlDBFireDACConnection.Connect;
var
  Log: ISynLog;
begin
  if fDatabase = nil then
    raise ESqlDBFireDAC.CreateUtf8('%.Connect(%): Database=nil',
      [self, fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to DriverID=% Database=%',
    [FIREDAC_PROVIDER[fProperties.Dbms], fProperties.DatabaseName], self);
  try
    fDatabase.Open;
    inherited Connect; // notify any re-connection
    Log.Log(sllDB, 'Connected to % (%)', [fDatabase.DriverName, fProperties.DatabaseName]);
  except
    on E: Exception do
    begin
      Log.Log(sllError, E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSqlDBFireDACConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if fDatabase <> nil then
      fDatabase.Close;
  end;
end;

destructor TSqlDBFireDACConnection.Destroy;
begin
  try
    Disconnect;
  except
    on Exception do

  end;
  inherited;
  FreeAndNil(fDatabase);
end;

function TSqlDBFireDACConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and
            fDatabase.Connected;
end;

function TSqlDBFireDACConnection.NewStatement: TSqlDBStatement;
begin
  result := TSqlDBFireDACStatement.Create(self);
end;

procedure TSqlDBFireDACConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSqlDBFireDACConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSqlDBFireDACStatement }

procedure TSqlDBFireDACStatement.DatasetCreate;
begin
  fQuery := TADQuery.Create(nil);
  TADQuery(fQuery).Connection := (fConnection as TSqlDBFireDACConnection).Database;
  fDatasetSupportBatchBinding := true;
end;

function TSqlDBFireDACStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as TADQuery).SQL.Text := aSQL;
  fQueryParams := TADQuery(fQuery).Params;
  result := fQueryParams <> nil;
end;

procedure TSqlDBFireDACStatement.Prepare(const aSQL: RawUtf8; ExpectResults: boolean);
begin
  inherited;
  if fPreparedParamsCount <> fQueryParams.Count then
    raise ESqlDBFireDAC.CreateUtf8(
      '%.Prepare() expected % parameters in request, found % - [%]',
      [self, fPreparedParamsCount, fQueryParams.Count, aSQL]);
end;

procedure TSqlDBFireDACStatement.DatasetExecSQL;
begin
  if fPreparedUseArrayDML then
    (fQuery as TADQuery).Execute(fParamsArrayCount)
  else
    (fQuery as TADQuery).Execute;
end;

procedure TSqlDBFireDACStatement.DataSetBindSqlParam(const aArrayIndex,
  aParamIndex: integer; const aParam: TSqlDBParam);
var
  P: TADParam;
  i: PtrInt;
  tmp: RawUtf8;
  StoreVoidStringAsNull: boolean;
begin
  if fDatasetSupportBatchBinding then
    fPreparedUseArrayDML := (aArrayIndex < 0) and
                            (fParamsArrayCount > 0)
  else
    fPreparedUseArrayDML := false;
  if fPreparedUseArrayDML and
     (fQueryParams.ArraySize <> fParamsArrayCount) then
    fQueryParams.ArraySize := fParamsArrayCount;
  with aParam do
  begin
    P := fQueryParams[aParamIndex];
    P.ParamType := SqlParamTypeToDBParamType(VInOut);
    if VinOut <> paramInOut then
      case VType of
        mormot.db.core.ftNull:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
              P.Clear(i)
          else
            P.Clear;
        mormot.db.core.ftInt64: begin
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Clear(i)
              else
                P.AsLargeInts[i] := GetInt64(pointer(VArray[i]))
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsLargeInt := GetInt64(pointer(VArray[aArrayIndex]))
          else
            P.AsLargeInt := VInt64;
        end;
        mormot.db.core.ftDouble:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Clear(i)
              else
                P.AsFloats[i] := GetExtended(pointer(VArray[i]))
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsFloat := GetExtended(pointer(VArray[aArrayIndex]))
          else
            P.AsFloat := PDouble(@VInt64)^;
        mormot.db.core.ftCurrency:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Clear(i)
              else
                P.AsCurrencys[i] := StrToCurrency(pointer(VArray[i]))
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsCurrency := StrToCurrency(pointer(VArray[aArrayIndex]))
          else
            P.AsCurrency := PCurrency(@VInt64)^;
        mormot.db.core.ftDate:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
            if VArray[i] = 'null' then
              P.Clear(i)
            else
            begin
              UnQuoteSqlStringVar(pointer(VArray[i]), tmp);
              P.AsDateTimes[i] := Iso8601ToDateTime(tmp);
            end
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
            begin
              UnQuoteSqlStringVar(pointer(VArray[aArrayIndex]), tmp);
              P.AsDateTime := Iso8601ToDateTime(tmp);
            end
          else
            P.AsDateTime := PDateTime(@VInt64)^;
        mormot.db.core.ftUtf8:
          if fPreparedUseArrayDML then
          begin
            StoreVoidStringAsNull := fConnection.Properties.StoreVoidStringAsNull;
            for i := 0 to fParamsArrayCount - 1 do
              if (VArray[i] = 'null') or
                 (StoreVoidStringAsNull and
                  (VArray[i] = #39#39)) then
                P.Clear(i)
              else
              begin
                UnQuoteSqlStringVar(pointer(VArray[i]), tmp);
                {$ifdef UNICODE} // for FireDAC: TADWideString=UnicodeString
                P.AsWideStrings[i] := Utf8ToString(tmp);
                {$else}
                if fForceUseWideString then
                  P.AsWideStrings[i] := Utf8ToWideString(tmp)
                else
                  P.AsStrings[i] := Utf8ToString(tmp);
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
              UnQuoteSqlStringVar(pointer(VArray[aArrayIndex]), tmp);
              {$ifdef UNICODE}
              P.AsWideString := Utf8ToString(tmp); // TADWideString=string
              {$else}
              if fForceUseWideString then
                P.AsWideString := Utf8ToWideString(tmp)
              else
                P.AsString := Utf8ToString(tmp);
              {$endif UNICODE}
          end
          else if (VData = '') and fConnection.Properties.StoreVoidStringAsNull then
            P.Clear
          else
            {$ifdef UNICODE}
            P.AsWideString := Utf8ToString(VData); // TADWideString=string
            {$else}
            if (not fForceUseWideString) {or IsAnsiCompatible(VData)} then
              P.AsString := Utf8ToString(VData)
            else
              P.AsWideString := Utf8ToWideString(VData);
            {$endif UNICODE}
        mormot.db.core.ftBlob:
          if fPreparedUseArrayDML then
            for i := 0 to fParamsArrayCount - 1 do
              if VArray[i] = 'null' then
                P.Clear(i)
              else
                P.AsBlobs[i] := VArray[i]
          else if aArrayIndex >= 0 then
            if VArray[aArrayIndex] = 'null' then
              P.Clear
            else
              P.AsBlob := VArray[aArrayIndex]
          else
            P.AsBlob := VData;
        else
          raise ESqlDBFireDAC.CreateUtf8(
            '%.DataSetBindSqlParam: invalid type % on bound parameter #%',
            [Self,ord(VType),aParamIndex + 1]);
        end;   
  end;
end;

procedure TSqlDBFireDACStatement.DataSetOutSqlParam(const aParamIndex: integer;
  var aParam: TSqlDBParam);
var
  Par: TADParam;
begin
  Par := fQueryParams[aParamIndex];
  case aParam.VType of
    mormot.db.core.ftInt64:
      aParam.VInt64 := Par.AsLargeInt;
    mormot.db.core.ftDouble:
      PDouble(@aParam.VInt64)^ := Par.AsFloat;
    mormot.db.core.ftCurrency:
      PCurrency(@aParam.VInt64)^ := Par.AsCurrency;
    mormot.db.core.ftDate:
      PDateTime(@aParam.VInt64)^ := Par.AsDateTime;
    mormot.db.core.ftUtf8:
      aParam.VData := StringToUtf8(Par.AsString);
    mormot.db.core.ftBlob:
      aParam.VData := Par.AsBlob;
  end;
end;

initialization
  TSqlDBFireDACConnectionProperties.RegisterClassNameForDefinition;

{$endif FPC} // FireDac is a Delphi-specific

end.

