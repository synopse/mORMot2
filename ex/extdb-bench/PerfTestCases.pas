unit PerfTestCases;

interface

{$I mormot.defines.inc}

// enable/disable third-party libraries
{.$define USENEXUSDB}
{.$define USEBDE}
{.$define USEUNIDAC}
{.$define USEZEOS}
{.$define USEIBX}
{.$define USEFIREDAC}

// enable/disable database engines
{.$define USEJET}
{.$define USEFIREBIRDEMB}
{.$define ODBCSQLITEFIREBIRD}
{.$define USELOCALMSSQLEXPRESS}    // SQL Server 2008 R2 Express locally
{.$define USELOCALDBMSSQLEXPRESS}  // SQL Server 2012 LocalDB edition
{.$define USELOCALDB2}
{.$define USELOCALPOSTGRESQL}
{.$define USELOCALMYSQL}
{.$define USEMONGODB}

{$ifdef CPU64}
  {$undef USENEXUSDB} // official NexusDB is not yet 64 bit ready :(
  {$undef USEJET}     // MS Access / JET is not available under Win64
{$endif}

{$ifndef OSWINDOWS}
  {$undef USENEXUSDB}
  {$undef USEJET}
{$endif OSWINDOWS}

// if defined, will create two "stored false" properties, to test UNIQUE columns
{.$define UNIK}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.os,
  mormot.core.perf,
  mormot.core.unicode,
  mormot.core.test,
  mormot.orm.core,
  mormot.orm.base,
  mormot.orm.sql,
  mormot.orm.storage,
  mormot.orm.rest,
  mormot.rest.core,
  mormot.rest.sqlite3,
  mormot.rest.memserver,
  mormot.rest.server,
  mormot.db.sql,
  mormot.db.sql.sqlite3,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.proxy,
  mormot.core.log,
  {$ifdef OSWINDOWS}
  mormot.db.sql.oledb,
  mormot.db.rad,
  {$endif}
  mormot.db.sql.odbc,
  {$ifdef USENEXUSDB}
  mormot.db.rad.nexusdb,
  {$endif}
  {$ifdef USEBDE}
  mormot.db.rad.bde,
  {$endif}
  {$ifdef USEUNIDAC}
  mormot.db.rad.unidac,
  SQLiteUniProvider,
  InterbaseUniProvider,
  OracleUniProvider,
  DB2UniProvider,
  SQLServerUniProvider,
  PostgreSQLUniProvider,
  MySqlUniProvider,
  {$endif}
  {$ifdef USEZEOS}
  mormot.db.sql.zeos,
  {$endif}
  {$ifdef USEIBX}
  mormot.db.sql.ibx,
  {$endif}
  {$ifdef USEFIREDAC}
  mormot.db.rad.firedac,
  {$ifdef ISDELPHIXE5}
  FireDAC.Phys.Oracle,
  FireDAC.Phys.MSAcc,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.IB,
  FireDAC.Phys.PG,
  FireDAC.Phys.DB2,
  {$else}
  uADPhysOracle,
  uADPhysMSAcc,
  uADPhysMSSQL,
  uADPhysSQLite,
  uADPhysIB,
  uADPhysPG,
  uADPhysDB2,
  uADPhysMySQL,
  {$endif}
  {$endif}
  {$ifdef USEMONGODB}
  mormot.db.nosql.mongodb,
  {$endif}
  mormot.db.core;

type
  TStat = class(TSynPersistent)
  private
    fCreateTable: RawUtf8;
    fNumberOfElements: integer;
    fInsertTime: RawUtf8;
    fEngine: RawUtf8;
    fClientCloseTime: RawUtf8;
    fInsertRate: integer;
    fReadOneByOneTime: RawUtf8;
    fReadOneByOneRate: integer;
    fInsertBatchTransactionRate: integer;
    fInsertTransactionRate: integer;
    fInsertBatchRate: integer;
    fInsertBatchTransactionTime: RawUtf8;
    fInsertTransactionTime: RawUtf8;
    fInsertBatchTime: RawUtf8;
    fReadAllVirtualRate: integer;
    fReadAllDirectRate: integer;
    fReadAllDirectTime: RawUtf8;
    fReadAllVirtualTime: RawUtf8;
    {$ifdef UNIK}
    fReadOneByNameRate: integer;
    fReadOneByNameTime: RawUtf8;
    {$endif UNIK}
  published
    property Engine: RawUtf8
      read fEngine;
    property CreateTableTime: RawUtf8
      read fCreateTable write fCreateTable;
    property NumberOfElements: integer
      read fNumberOfElements write fNumberOfElements;
    property InsertTime: RawUtf8
      read fInsertTime;
    property InsertRate: integer
      read fInsertRate;
    property InsertBatchTime: RawUtf8
      read fInsertBatchTime;
    property InsertBatchRate: integer
      read fInsertBatchRate;
    property InsertTransactionTime: RawUtf8
      read fInsertTransactionTime;
    property InsertTransactionRate: integer
      read fInsertTransactionRate;
    property InsertBatchTransactionTime: RawUtf8
      read fInsertBatchTransactionTime;
    property InsertBatchTransactionRate: integer
      read fInsertBatchTransactionRate;
    property ReadOneByOneTime: RawUtf8
      read fReadOneByOneTime;
    property ReadOneByOneRate: integer
      read fReadOneByOneRate;
    {$ifdef UNIK}
    property ReadOneByNameTime: RawUtf8
      read fReadOneByNameTime;
    property ReadOneByNameRate: integer
      read fReadOneByNameRate;
    {$endif}
    property ReadAllVirtualTime: RawUtf8
      read fReadAllVirtualTime;
    property ReadAllVirtualRate: integer
      read fReadAllVirtualRate;
    property ReadAllDirectTime: RawUtf8
      read fReadAllDirectTime;
    property ReadAllDirectRate: integer
      read fReadAllDirectRate;
    property ClientCloseTime: RawUtf8
      read fClientCloseTime;
  end;

  TOrmSample = class(TOrm)
  private
    fFirstName: RawUtf8;
    fLastName: RawUtf8;
    fAmount: Currency;
    fBirthDate: TDateTime;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUtf8 index 40
      read fFirstName write fFirstName
      {$ifdef UNIK} stored AS_UNIQUE{$endif};
    property LastName: RawUtf8 index 40
      read fLastName write fLastName
      {$ifdef UNIK} stored AS_UNIQUE{$endif};
    property Amount: Currency
      read fAmount write fAmount;
    property BirthDate: TDateTime
      read fBirthDate write fBirthDate;
    property LastChange: TModTime
      read fLastChange write fLastChange;
    property CreatedAt: TCreateTime
      read fCreatedAt write fCreatedAt;
  end;

  TTestDatabaseBenchmark = class(TSynTestsLogged)
  public
    Ini: RawUtf8;
    Stats: TSynObjectList;
    constructor Create(const Ident: string = ''); override;
    destructor Destroy; override;
    procedure SaveStats;
  published
    procedure DirectDatabaseAccess;
    procedure ExternalDatabaseAccess;
  end;

  TTestDatabaseFlags = set of (
    dbIsFile,
    dbInMemory,
    dbInMemoryVirtual,
    dbPropIsMemory,
    dbPropUntouched,
    dbDropTable,
    dbSlowInsert);

  TTestDatabaseAbstract = class(TSynTestCase)
  protected
    Main: TTestDatabaseBenchmark;
    Value: TOrmSample;
    Stat: TStat;
    Namee, Num: RawUtf8;
    ChangeStart: TTimeLog;
    RunTimer: TPrecisionTimer;
    SQlite3Mode: TSQLSynchronousMode;
    SQlite3Lock: TSQLLockingMode;
    Client: TRestClientDB;
    Orm: IRestOrmClient;
    Server: TRestServerDB;
    DBFileName: TFileName;
    DBPassword: RawUtf8;
    ValueLastName, ValueFirstName: TRawUtf8DynArray;
    Res: TIDDynArray;
    Flags: TTestDatabaseFlags;
    procedure Setup; override;
    procedure CleanUp; override;
    procedure MethodSetup; override;
    procedure MethodCleanUp; override;
    procedure RunTests; virtual;
    function ModelCreate: TOrmModel; virtual;
    procedure ClientCreate; virtual;
    procedure ClientFree; virtual;
    procedure RunWrites(UseTransactions, UseBatch: boolean); virtual;
    procedure ValueCheck(i: PtrInt);
    procedure RunModeLock(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode); virtual;
  end;

  TTestDirectSqliteEngine = class(TTestDatabaseAbstract)
  protected
    procedure RunModeLock(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode); override;
  published
    procedure SqliteFileFull;
    procedure SqliteFileOff;
    procedure SqliteFileOffExc;
    procedure SqliteFileOffExcAes;
    procedure SqliteInMemory;
  end;

  TTestInMemoryEngine = class(TTestDatabaseAbstract)
  protected
    function ModelCreate: TOrmModel; override;
    procedure ClientCreate; override;
  published
    procedure InMemoryStatic;
    procedure InMemoryVirtual;
  end;

  TTestDatabaseExternalAbstract = class(TTestDatabaseAbstract)
  protected
    Props: TSQLDBConnectionProperties;
    function ModelCreate: TOrmModel; override;
    procedure ClientCreate; override;
    procedure ClientFree; override;
    procedure RunExternal(P: TSQLDBConnectionProperties); virtual;
  public
  end;

  TTestSqliteExternal = class(TTestDatabaseExternalAbstract)
  protected
    fn: TFileName;
    sm: TSQLSynchronousMode;
    lm: TSQLLockingMode;
    procedure ClientCreate; override;
    procedure RunExternalSqlite(Mode: TSQLSynchronousMode; Lock: TSQLLockingMode);
  published
    procedure ExternalSqliteFileFull;
    procedure ExternalSqliteFileOff;
    procedure ExternalSqliteFileOffExc;
    procedure ExternalSqliteInMemory;
  end;

  TTestSqliteRemote = class(TTestDatabaseExternalAbstract)
  protected
    RemoteProps: TSQLDBSQLite3ConnectionProperties;
    RemoteServer: TSQLDBServerRemote;
    RemoteClient: TSQLDBConnectionPropertiesClass;
    procedure ClientCreate; override;
    procedure ClientFree; override;
  published
    {$ifdef OSWINDOWS}
    procedure RemoteSqliteWinHTTP;
    {$endif OSWINDOWS}
    procedure RemoteSqliteSocket;
  end;

  {$ifdef USELOCALPOSTGRESQL}
  TTestPostgresql = class(TTestDatabaseExternalAbstract)
  published
    procedure _SynDBPostgres;
    {$ifdef USEZEOS}
    procedure ZeosPostgres;
    {$endif}
  end;
  {$endif USELOCALPOSTGRESQL}

  {$ifdef USEFIREBIRDEMB}

  { TTestFirebird }

  TTestFirebird = class(TTestDatabaseExternalAbstract)
  published
    {$ifdef USEIBX}
    procedure IbxFirebird;
    {$endif}
    {$ifdef USEZEOS}
    procedure ZeosFirebird;
    {$endif}
  end;

  {$endif USEFIREBIRDEMB}



implementation

{$ifdef USEFIREBIRDEMB}

{ TTestFirebird }

const
  /// Please configure parameters for firebird connection
  {$ifdef CPU64}
//  cFIREBIRDEMBEDDEDDLL = '';
  cFIREBIRDEMBEDDEDDLL = 'C:\Firebird\Firebird3_64\fbclient.dll';
  {$else}
  cFIREBIRDEMBEDDEDDLL = 'C:\Firebird\firebird3_32\fbclient.dll';
  {$endif}
  {$ifdef USEZEOS}
  cFirebirdServerZeos = '';  // 'localhost:3033'
  cFirebirdDBFileZeos = 'r:\perftestZeos.fdb';
  {$endif}
  {$ifdef USEIBX}
  cFirebirdServerIbx = '';  // 'localhost/3033'
  cFirebirdDBFileIbx = 'r:\perftestIbx.fdb';
  {$endif}
  cUserName = 'SYSDBA';
  cPassword = 'masterkey';

{$ifdef USEZEOS}
procedure TTestFirebird.ZeosFirebird;
begin
  Flags := [dbPropUntouched, dbDropTable];
//  if FileExists(cFirebirdDBFileZeos) then
//    DeleteFile(cFirebirdDBFileZeos);
  RunExternal(TSqlDBZeosConnectionProperties.Create('zdbc:firebird://' +
    cFirebirdServerZeos + '/' + cFirebirdDBFileZeos + '?username=' + cUserName +
    ';password=' + cPassword + ';LibLocation=' + cFIREBIRDEMBEDDEDDLL, '', '', ''));
end;
{$endif USEZEOS}

{$ifdef USEIBX}

procedure TTestFirebird.IbxFirebird;
var
  lProps: TSqlDBIbxConnectionProperties;
begin
  Flags := Flags - [dbPropUntouched, dbDropTable];
//  if FileExists(cFirebirdDBFileIbx) then
//    DeleteFile(cFirebirdDBFileIbx);
  lProps := TSqlDBIbxConnectionProperties.Create(cFirebirdServerIbx,
    cFirebirdDBFileIbx, cUserName, cPassword);
  lProps.FirebirdLibraryPathName := cFIREBIRDEMBEDDEDDLL;
  lProps.CreateDescendingPK := True;
  RunExternal(lProps);
end;
{$endif USEIBX}

{$endif USEFIREBIRDEMB}


{ TTestDatabaseAbstract }

procedure TTestDatabaseAbstract.Setup;
begin
  EnsureDirectoryExists(Executable.ProgramFilePath + 'db');
  Main := Owner as TTestDatabaseBenchmark;
  Value := TOrmSample.Create;
  Namee := 'Name/ ';
  UniqueRawUtf8(Namee);        // FPC does not call it
  PWord(@Namee[4])^ := $a9c3;  // some 'e'acute to test UTF-8 encoding
  SQlite3Mode := smOff;        // fastest SQlite3 modes by default
  SQlite3Lock := lmExclusive;
end;

procedure TTestDatabaseAbstract.Cleanup;
begin
  // warning: this method could be called several times after a single Setup
  FreeAndNil(Value);
end;

procedure TTestDatabaseAbstract.MethodSetup;
begin
  Flags := [];
  Stat := TStat.Create;
  Stat.fEngine := ToUTF8(Owner.CurrentMethodInfo^.TestName);
end;

procedure TTestDatabaseAbstract.MethodCleanup;
begin
  if Stat.Engine <> '' then
    Main.Stats.Add(Stat)
  else
    FreeAndNil(Stat);
  ClientFree;
end;

procedure TTestDatabaseAbstract.RunTests;
const
  _ID: array['1'..'4'] of RawUtf8 = (
    '',
    ' batch',
    ' trans',
    ' batch trans');
var
  trans, batch, UseDirect: boolean;
  time: RawUtf8;
  rate: QWord;
  i: PtrInt;
  {%H-}log: ISynLog;
begin
  // Insertion tests
  Num := '1';
  for trans := false to true do
    for batch := false to true do
    begin
      log := TSynLog.Enter('% Insert%',
        [Owner.CurrentMethodInfo^.IdentTestName, _ID[Num[1]]], self);
      RunWrites(trans, batch);
      NotifyTestSpeed('insert%',
        [_ID[Num[1]]], Stat.NumberOfElements, 0, @RunTimer);
      time := RunTimer.LastTime;
      rate := RunTimer.PerSec(Stat.NumberOfElements);
      log := nil;
      case Num[1] of
        '1':
          begin
            Stat.fInsertTime := time;
            Stat.fInsertRate := rate;
          end;
        '2':
          begin
            Stat.fInsertBatchTime := time;
            Stat.fInsertBatchRate := rate;
          end;
        '3':
          begin
            Stat.fInsertTransactionTime := time;
            Stat.fInsertTransactionRate := rate;
          end;
        '4':
          begin
            Stat.fInsertBatchTransactionTime := time;
            Stat.fInsertBatchTransactionRate := rate;
            break; // still need Client for Read tests
          end;
      end;
      inc(Num[1]);
      ClientFree;
    end;
  // Read tests
  log := TSynLog.Enter('% Read One',
    [Owner.CurrentMethodInfo^.IdentTestName], self);
  Value.ClearProperties;
  RunTimer.Start;
  for i := 0 to Stat.NumberOfElements - 1 do
  begin
    Orm.Retrieve(Res[i], Value);
    ValueCheck(i);
  end;
  NotifyTestSpeed('read one', Stat.NumberOfElements, 0, @RunTimer);
  Stat.fReadOneByOneTime := RunTimer.LastTime;
  Stat.fReadOneByOneRate := RunTimer.PerSec(Stat.NumberOfElements);
  log := nil;
  {$ifdef UNIK}
  // one by one retrieve values using Name property
  log := TSynLog.Enter('% Read Unique',
    [Owner.CurrentMethodInfo^.IdentTestName], self);
  RunTimer.Start;
  for i := 0 to Stat.NumberOfElements - 1 do
  begin
    Client.Retrieve('LastName=?', [], [ValueLastName[i]], Value);
    Check((Value.IDValue = Res[i]) and
          (PInt64(@Value.Amount)^ = (i + 1) * 100) and
          (Value.LastChange >= ChangeStart));
  end;
  NotifyTestSpeed('read unique', Stat.NumberOfElements, 0, @RunTimer);
  Stat.fReadOneByNameTime := RunTimer.LastTime;
  Stat.fReadOneByNameRate := RunTimer.PerSec(Stat.NumberOfElements);
  {$endif UNIK}
  // retrieve all rows with or without the virtual module
  for UseDirect := false to true do
  begin
    log := nil;
    log := TSynLog.Enter('% Read Direct=%',
      [Owner.CurrentMethodInfo^.IdentTestName, BOOL_STR[UseDirect]], self);
    Server.Server.Cache.Flush; // fair benchmark
    Server.DB.CacheFlush; // fair benchmark (16100 rows/s->456000 with cache!)
    Server.Server.SetStaticVirtualTableDirect(UseDirect);
    RunTimer.Start;
    Value.ClearProperties;
    Check(Value.FillPrepare(Client.Orm, 'order by RowId'), 'FillPrepare');
    //FileFromString((Value.FillTable as TSQLTableJSON).PrivateInternalCopy,Stat.Engine+'.json');
    i := 0;
    while Value.FillOne do
    begin
      ValueCheck(i);
      inc(i);
    end;
    CheckEqual(i, Stat.NumberOfElements, 'FillOne');
    if UseDirect then
    begin
      NotifyTestSpeed('read direct', Stat.NumberOfElements, 0, @RunTimer);
      Stat.fReadAllDirectTime := RunTimer.LastTime;
      Stat.fReadAllDirectRate := RunTimer.PerSec(Stat.NumberOfElements);
    end
    else
    begin
      NotifyTestSpeed('read virtual', Stat.NumberOfElements, 0, @RunTimer);
      Stat.fReadAllVirtualTime := RunTimer.LastTime;
      Stat.fReadAllVirtualRate := RunTimer.PerSec(Stat.NumberOfElements);
    end;
  end;
end;

function TTestDatabaseAbstract.ModelCreate: TOrmModel;
begin
  result := TOrmModel.Create([TOrmSample]);
end;

procedure TTestDatabaseAbstract.ClientCreate;
var
  fn: TFileName;
begin
  if dbIsFile in Flags then
    fn := DBFileName
  else
    fn := SQLITE_MEMORY_DATABASE_NAME;
  Client := TRestClientDB.Create(ModelCreate, nil, fn, TRestServerDB,
    {auth=}false, DBPassword);
  Client.Model.Owner := Client;
  Orm := Client.Client;
  Server := (Client as TRestClientDB).Server;
  Server.DB.Synchronous := SQlite3Mode;
  Server.DB.LockingMode := SQlite3Lock;
end;

procedure TTestDatabaseAbstract.ClientFree;
begin
  Orm := nil; // to be freed before Client
  FreeAndNil(Client);
end;

procedure TTestDatabaseAbstract.RunWrites(UseTransactions, UseBatch: boolean);
var
  i: PtrInt;
  forceID: boolean;
begin
  DBFileName := FormatString('%db%%.%.db', [Executable.ProgramFilePath,
    PathDelim, Owner.CurrentMethodInfo^.MethodName, Num]);
  if FileExists(DBFileName) then
    DeleteFile(DBFileName);
  RunTimer.Start;
  ClientCreate;
  if CheckFailed(Client <> nil, 'Client?') then
    exit; // avoid GPF
  Server.Server.CreateMissingTables;
  ChangeStart := Server.OrmInstance.GetServerTimestamp; // use by ValueCheck
  if Stat.CreateTableTime = '' then
    Stat.CreateTableTime := RunTimer.Stop;
  if (dbSlowInsert in Flags) and not UseTransactions then // full synch is slow
    Stat.NumberOfElements := 200
  else
    Stat.NumberOfElements := 10000;
  SetLength(ValueLastName, Stat.NumberOfElements);
  SetLength(ValueFirstName, Stat.NumberOfElements);
  for i := 0 to Stat.NumberOfElements - 1 do
    if ValueLastName[i] = '' then
    begin
      UInt32ToUtf8(i + 1, ValueLastName[i]);
      {$ifndef UNIK}
      if i <> 100 then // test https://synopse.info/fossil/info/e8c211062e
      {$endif UNIK}
        ValueFirstName[i] := Namee + ValueLastName[i];
    end;
  RunTimer.Start;
  if UseTransactions then
    Orm.TransactionBegin(TOrmSample, Client.SessionID);
  if UseBatch then
    Orm.BatchStart(TOrmSample)
  else if length(Res) < Stat.NumberOfElements then
    SetLength(Res, Stat.NumberOfElements);
  for i := 0 to Stat.NumberOfElements - 1 do
  begin
    Value.Amount := (i + 1) * 0.01;
    Value.LastName := ValueLastName[i];
    Value.FirstName := ValueFirstName[i];
    Value.BirthDate := i + 1;
    forceID := i and 3 = 1;
    if forceID then
      if {$ifdef UNIK} (dbInMemory in Flags) or {$endif UNIK}
         (Res[i - 1] = 0) then
        forceID := false // not yet in TRestStorageInMemory.AddOne
      else
        Value.IDValue := Res[i - 1] + 1;
    if UseBatch then
      Check(Orm.BatchAdd(Value, true, forceID) >= 0)
    else
    begin
      Res[i] := Orm.Add(Value, true, forceID);
      Check(Res[i] > 0, 'Add');
    end;
  end;
  if UseBatch then
    Check(Orm.BatchSend(Res) = HTTP_SUCCESS);
  if UseTransactions then
    Orm.Commit(Client.SessionID);
  Value.ClearProperties;
  Check(Orm.Retrieve(Res[1], Value), 'One Retrieve after Add');
  ValueCheck(1);
end;

procedure TTestDatabaseAbstract.ValueCheck(i: PtrInt);
begin
  CheckEqual(Value.IDValue, Res[i], 'ID');
  CheckEqual(PInt64(@Value.Amount)^, (i + 1) * 100, 'Amount');
  Check(Value.LastChange >= ChangeStart, 'LastChange');
  CheckEqual(Value.FirstName, ValueFirstName[i], 'FirstName');
  Value.IDValue := 0;
  Value.Amount := 0;
  Value.FirstName := '';
  Value.LastChange := 0;
end;

procedure TTestDatabaseAbstract.RunModeLock(Mode: TSQLSynchronousMode; Lock:
  TSQLLockingMode);
begin
  SQlite3Mode := Mode;
  SQlite3Lock := Lock;
  RunTests;
end;


{ TTestDirectSqliteEngine }

procedure TTestDirectSqliteEngine.RunModeLock(
  Mode: TSQLSynchronousMode; Lock: TSQLLockingMode);
begin
  Flags := [dbIsFile];
  if Mode = smFull then
    include(Flags, dbSlowInsert);
  inherited RunModeLock(Mode, Lock);
end;

procedure TTestDirectSqliteEngine.SqliteFileFull;
begin
  RunModeLock(smFull, lmNormal);
end;

procedure TTestDirectSqliteEngine.SqliteFileOff;
begin
  RunModeLock(smOff, lmNormal);
end;

procedure TTestDirectSqliteEngine.SqliteFileOffExc;
begin
  RunModeLock(smOff, lmExclusive);
end;

procedure TTestDirectSqliteEngine.SqliteFileOffExcAes;
begin
  DBPassword := 'password';
  RunModeLock(smOff, lmExclusive);
  DBPassword := '';
end;

procedure TTestDirectSqliteEngine.SqliteInMemory;
begin
  Flags := [];
  RunTests;
end;


{ TTestInMemoryEngine }

function TTestInMemoryEngine.ModelCreate: TOrmModel;
begin
  result := inherited ModelCreate;
  // registration should be done BEFORE Client is initialized
  if dbInMemoryVirtual in Flags then
    result.VirtualTableRegister(TOrmSample, TOrmVirtualTableBinary)
end;

procedure TTestInMemoryEngine.ClientCreate;
begin
  inherited ClientCreate;
  if not (dbInMemoryVirtual in Flags) then
    StaticDataCreate(Client.Server.OrmInstance, TOrmSample, DBFileName, true);
end;

procedure TTestInMemoryEngine.InMemoryStatic;
begin
  Flags := [dbInMemory];
  RunTests;
end;

procedure TTestInMemoryEngine.InMemoryVirtual;
begin
  Flags := [dbInMemory, dbInMemoryVirtual];
  RunTests;
end;


{ TTestDatabaseExternalAbstract }

function TTestDatabaseExternalAbstract.ModelCreate: TOrmModel;
begin
  result := inherited ModelCreate;
  // registration should be done BEFORE Client is initialized
  VirtualTableExternalRegister(result, TOrmSample, Props, 'SampleRecord');
end;

procedure TTestDatabaseExternalAbstract.ClientCreate;
var
  lTables: TRawUtf8DynArray;
begin
  if (Props <> nil) and (dbDropTable in Flags) then
  begin
    // drop only if table exist
    Props.GetTableNames(lTables);
    if FindPropName(lTables, 'SAMPLERECORD') >= 0 then
    begin
      Props.ClearConnectionPool;
      Props.ThreadSafeConnection.Disconnect;
      Props.ExecuteNoResult('drop table SAMPLERECORD', []);
    end;
  end;
  inherited ClientCreate;
end;

procedure TTestDatabaseExternalAbstract.ClientFree;
begin
  inherited ClientFree;
  if not (dbPropUntouched in Flags) then
    FreeAndNil(Props);
end;

procedure TTestDatabaseExternalAbstract.RunExternal(P: TSQLDBConnectionProperties);
begin
  Flags := Flags + [dbPropUntouched, dbDropTable, dbPropIsMemory];
  Props := P;
  try
    Props.ThreadSafeConnection.Connect;
    Check(Props.ThreadSafeConnection.Connected, 'connected');
    RunTests;
  finally
    FreeAndNil(Props);
  end;
end;


{ TTestSqliteExternal }

procedure TTestSqliteExternal.ClientCreate;
var
  P: TSqlDBSQLite3ConnectionProperties;
begin
  Exclude(Flags, dbDropTable);
  if fn <> SQLITE_MEMORY_DATABASE_NAME then
  begin
    DeleteFile(fn);
    P := TSqlDBSQLite3ConnectionProperties.Create(StringToUtf8(fn), '', '', '');
    P.MainSQLite3DB.Synchronous := sm;
    P.MainSQLite3DB.LockingMode := lm;
    Props := P;
  end;
  inherited ClientCreate;
end;

procedure TTestSqliteExternal.RunExternalSqlite(Mode: TSQLSynchronousMode; Lock:
  TSQLLockingMode);
begin
  if dbPropIsMemory in Flags then
    fn := SQLITE_MEMORY_DATABASE_NAME
  else
    fn := FormatString('%db%%.db', [Executable.ProgramFilePath, PathDelim,
      Owner.CurrentMethodInfo^.MethodName]);
  sm := Mode;
  lm := Lock;
  if Mode = smFull then
    include(Flags, dbSlowInsert);
  Flags := Flags + [dbPropIsMemory];
  RunTests;
end;

procedure TTestSqliteExternal.ExternalSqliteFileFull;
begin
  RunExternalSqlite(smFull, lmNormal);
end;

procedure TTestSqliteExternal.ExternalSqliteFileOff;
begin
  RunExternalSqlite(smOff, lmNormal);
end;

procedure TTestSqliteExternal.ExternalSqliteFileOffExc;
begin
  RunExternalSqlite(smOff, lmExclusive);
end;

procedure TTestSqliteExternal.ExternalSqliteInMemory;
begin
  Flags := [dbPropIsMemory];
  RunExternalSqlite(smOff, lmExclusive);
end;


{ TTestSqliteRemote }

procedure TTestSqliteRemote.ClientCreate;
begin
  // initialize a fast in-memory SQLite3 remote server
  RemoteProps := TSQLDBSQLite3ConnectionProperties.Create(
    SQLITE_MEMORY_DATABASE_NAME, '', '', '');
  RemoteProps.MainSQLite3DB.Synchronous := SQlite3Mode;
  RemoteProps.MainSQLite3DB.LockingMode := SQlite3Lock;
  RemoteServer := TSQLDBServerRemote.Create(
    RemoteProps, 'root', '8888', 'user', 'password');
  // we actually connect to this DB using our remote binary protocol over HTTP
  Props := RemoteClient.Create('localhost:8888', 'root', 'user', 'password');
  inherited ClientCreate;
end;

procedure TTestSqliteRemote.ClientFree;
begin
  inherited ClientFree;
  RemoteServer.Free;
  RemoteProps.Free;
end;

procedure TTestSqliteRemote.RemoteSqliteSocket;
begin
  RemoteClient := TSQLDBSocketConnectionProperties;
  RunTests;
end;

{$ifdef OSWINDOWS}
procedure TTestSqliteRemote.RemoteSqliteWinHTTP;
begin
  RemoteClient := TSQLDBWinHTTPConnectionProperties;
  RunTests;
end;
{$endif OSWINDOWS}


{$ifdef USELOCALPOSTGRESQL}

{ TTestPostgresql }

procedure TTestPostgresql._SynDBPostgres;
begin
  RunExternal(TSQLDBPostgresConnectionProperties.Create(
    'localhost', 'postgres', 'postgres', 'docker'));
end;


{$ifdef USEZEOS}
procedure TTestPostgresql.ZeosPostgres;
begin
  RunExternal(TSQLDBZEOSConnectionProperties.Create(
    TSQLDBZEOSConnectionProperties.URI(dPostgreSQL, 'localhost', 'libpq.so.5', false),
    'postgres', 'postgres', 'docker'));
end;
{$endif}

{$endif USELOCALPOSTGRESQL}


{ TTestDatabaseBenchmark }

constructor TTestDatabaseBenchmark.Create(const Ident: string);
var
  fn: TFileName;
begin
  Stats := TSynObjectList.Create;
  fn := ChangeFileExt(Executable.ProgramFileName, '.ini');
  if FileExists(fn) then
    Ini := StringFromFile(fn)
  else
    FileFromString('', fn);
  inherited Create(Ident);
end;

destructor TTestDatabaseBenchmark.Destroy;
begin
  inherited Destroy;
  SaveStats;
  Stats.Free;
end;

procedure TTestDatabaseBenchmark.SaveStats;
type
  TStatArray = array[0..1000] of TStat;
var
  Stat: ^TStatArray;
  mode, s, txt: RawUtf8;
  m, nCat, col1len: integer;
  max, Cat1, Cat2, Eng1, Eng2, Eng: RawUtf8;
  Rows: TRawUtf8DynArray;
  Doc, Cons: RawUtf8;

  procedure SetCategories(const Title: RawUtf8; const Cat: array of RawUtf8);
  var
    i: integer;
  begin
    mode := UrlEncode(Title);
    s := s + '<h1>' + copy(Title, 1, posEx(' (', Title) - 1) + '</h1>'#13#10;
    max := Int32ToUtf8(m);
    nCat := length(Cat);
    Cat1 := '';
    Cat2 := '';
    SetLength(Rows, Stats.Count + 1);
    Rows[0] := '<td>&nbsp;</td>';
    Cons := Cons + #13#10 + Title + #13#10 + RawUtf8OfChar(' ', col1len + 2);
    for i := 0 to high(Cat) do
    begin
      Rows[0] := Rows[0] + '<td><b>' + Cat[i] + '</b></td>';
      Cat1 := Cat1 + UrlEncode(Cat[i]) + '|';
      Cat2 := Cat2 + UrlEncode(Cat[high(Cat) - i]) + '|';
      Cons := Cons + Cat[i];
      if i <> high(Cat) then
        Cons := Cons + RawUtf8OfChar(' ', 12 - length(Cat[i]));
    end;
    Cons := Cons + #13#10;
    SetLength(Cat1, length(Cat1) - 1);
    SetLength(Cat2, length(Cat2) - 1);
    Eng1 := '';
    Eng2 := '';
    for i := 0 to Stats.Count - 1 do
    begin
      Eng := Stat[i].Engine;
     { j := PosEx(' ',Eng);
      if j>0 then begin
        Delete(Eng,j,1);
        insert('<br>',Eng,j);
      end;}
      Rows[i + 1] := '<td><b>' + Eng + '</b></td>';
      Eng1 := Eng1 + UrlEncode(Stat[i].Engine) + '|';
      Eng2 := Eng2 + UrlEncode(Stat[Stats.Count - 1 - i].Engine) + '|';
    end;
    SetLength(Eng1, length(Eng1) - 1);
    SetLength(Eng2, length(Eng2) - 1);
  end;

  procedure Pic1(const Leg: RawUtf8; n: integer);
  var
    i: integer;
  begin
    txt := 'http://chart.apis.google.com/chart?chtt=' + mode + '&chxl=1:|' +
     Leg + '&chxt=x,y&chbh=a&chs=600x500&cht=bhg&chco=';
  //  for i := 1 to 5 do txt := txt+IntToHex($309F30+i*$010101,3)+',';
  //  txt[length(txt)] := '&';
  //    '3D7930,3D8930,309F30,6070F0,5070E0,40C355,65D055,80C1A2,F05050,F0A280'+
    txt := txt + '3D7930,3D8930,309F30,40C355&';
    //,6070F0,5070E0,65D055,80C1A2,3D7930,3D8930,F05050,F04050,F04040,F01040,F0A280&';
    txt := txt + 'chxr=0,0,' + max + '&chds=';
    for i := 1 to n do
      txt := txt + '0,' + max + ',';
    txt[length(txt)] := '&';
    txt := txt + 'chd=t:';
  end;

  procedure PicEnd(const Legend: RawUtf8);
  begin
    txt[length(txt)] := '&';
    s := s + '<p><img src=' + txt + 'chdl=' + Legend + '></p>'#13#10;
    txt := '';
  end;

  procedure SetValues(var Rows: RawUtf8; const eng: RawUtf8; const v: array of const);
  var
    j: integer;
    fmt, s: RawUtf8;
  begin
    for j := 2 to length(v) do
      fmt := fmt + '%,';
    fmt := fmt + '%|';
    txt := txt + FormatUTF8(fmt, v);
    fmt := '';
    for j := 1 to length(v) do
      fmt := fmt + '<td>%</td>';
    Rows := Rows + FormatUTF8(fmt, v);
    fmt := eng + RawUtf8OfChar(' ', col1len - length(eng) + 2);
    for j := 0 to high(v) do
    begin
      VarRecToUTF8(v[j], s);
      if j <> high(v) then
        s := s + RawUtf8OfChar(' ', 12 - length(s));
      fmt := fmt + s;
    end;
    Cons := Cons + fmt + #13#10;
  end;

  procedure Table;
  var
    i: integer;
  begin
    s := s + '<p><table>';
    for i := 0 to High(Rows) do
      s := s + '<tr align=center>' + Rows[i] + '</tr>'#13#10;
    s := s + '</table></p>';
    Doc := Doc + '|%30';
    for i := 1 to nCat do
      Doc := Doc + '%15';
    Doc := Doc + #13#10;
    for i := 0 to High(Rows) do
    begin
      Doc := Doc + StringReplaceAll(Rows[i], [
        '</td>', '',
        '</tr>', '',
        '<tr align=center>', '',
        '</b>', '}',
        '</td>', '',
        '<b>', '{\b ',
        '<td>', '|',
        '&nbsp;', '']) + #13#10;
    end;
    Doc := Doc + '|%'#13#10;
  end;

var
  i, j: integer;
begin
  // introducting text
  Stat := pointer(Stats.List);
  s := FormatUTF8('Running tests using Synopse mORMot framework %, ' +
    'compiled with %, against SQLite %, on %, at %.',
    [SYNOPSE_FRAMEWORK_VERSION, COMPILER_VERSION, SQLite3.libversion,
     OSVersionText, NowToString]);
  Cons := '[code]'#13#10 + s + #13#10#13#10;
  s := '<p>' + s + '</p>';
  // compute max Insertion rate value for charts
  m := 0;
  col1len := 0;
  for i := 0 to Stats.Count - 1 do
    with Stat[i] do
    begin
      if InsertRate > m then
        m := InsertRate;
      if InsertBatchRate > m then
        m := InsertBatchRate;
      if InsertTransactionRate > m then
        m := InsertTransactionRate;
      if InsertBatchTransactionRate > m then
        m := InsertBatchTransactionRate;
      j := length(Engine);
      if j > col1len then
        col1len := j;
    end;
  // Insertion Categories
  SetCategories('Insertion speed (rows/second)',
    ['Direct', 'Batch', 'Trans', 'Batch Trans']);
  // Insertion per-Engine Values and Chart
  Pic1(Cat2, 5);
  for i := 0 to Stats.Count - 1 do
    with Stat[i] do
      SetValues(Rows[i + 1], Engine, [InsertRate, InsertBatchRate,
        InsertTransactionRate, InsertBatchTransactionRate]);
  Table;
  PicEnd(Eng1);
  // Insertion per-Category Chart
  Pic1(Eng2, Stats.Count);
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].InsertRate) + ',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].InsertBatchRate) + ',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].InsertTransactionRate) + ',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].InsertBatchTransactionRate) + ',';
  PicEnd(Cat1);
  // compute max Reading rate value for charts
  m := 0;
  for i := 0 to Stats.Count - 1 do
    with Stat[i] do
    begin
      if ReadOneByOneRate > m then
        m := ReadOneByOneRate;
      {$ifdef UNIK}
      if ReadOneByNameRate > m then
        m := ReadOneByNameRate;
      {$endif UNIK}
      if ReadAllVirtualRate > m then
        m := ReadAllVirtualRate;
      if ReadAllDirectRate > m then
        m := ReadAllDirectRate;
    end;
  // Reading Categories
  SetCategories('Read speed (rows/second)',
    ['By one',
     {$ifdef UNIK}
     'By name',
     {$endif}
     'All Virtual',
     'All Direct']);
  // Reading per-Engine Values and Chart
  {$ifdef UNIK}
  Pic1(Cat2, 4);
  {$else}
  Pic1(Cat2, 3);
  {$endif UNIK}
  for i := 0 to Stats.Count - 1 do
    with Stat[i] do
      SetValues(Rows[i + 1], Engine, [ReadOneByOneRate, {$ifdef UNIK}
        ReadOneByNameRate, {$endif}
        ReadAllVirtualRate, ReadAllDirectRate]);
  Table;
  PicEnd(Eng1);
  // Reading per-Category Chart
  Pic1(Eng2, Stats.Count);
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].ReadOneByOneRate) + ',';
  txt[length(txt)] := '|';
  {$ifdef UNIK}
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].ReadOneByNameRate) + ',';
  txt[length(txt)] := '|';
  {$endif}
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].ReadAllVirtualRate) + ',';
  txt[length(txt)] := '|';
  for i := 0 to Stats.Count - 1 do
    txt := txt + Int32ToUtf8(Stat[i].ReadAllDirectRate) + ',';
  PicEnd(Cat1);
  // save to local files
  FileFromString(Doc, ChangeFileExt(Executable.ProgramFileName, '.doc'));
  FileFromString(Cons + '[/code]',
    ChangeFileExt(Executable.ProgramFileName, '.txt'));
  s := '<html><body>'#13#10 + s;
  FileFromString(s, ChangeFileExt(Executable.ProgramFileName, '.htm'));
  FileFromString(s, FormatString('%%-%%.html',
    [EnsureDirectoryExists(Executable.ProgramFilePath + 'reports'),
     DateTimeToFileShort(Now), COMP_TEXT, OS_TEXT]));
end;

procedure TTestDatabaseBenchmark.DirectDatabaseAccess;
begin
  //exit;
  AddCase(TTestDirectSqliteEngine);
  AddCase(TTestInMemoryEngine);
end;

procedure TTestDatabaseBenchmark.ExternalDatabaseAccess;
begin
  //exit;
  AddCase(TTestSqliteExternal);
  AddCase(TTestSqliteRemote);
  {$ifdef USELOCALPOSTGRESQL}
  AddCase(TTestPostgresql);
  {$endif}
  {$ifdef USEFIREBIRDEMB}
  AddCase(TTestFirebird);
  {$endif}
end;

end.

