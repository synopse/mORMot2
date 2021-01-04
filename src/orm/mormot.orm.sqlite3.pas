/// ORM Types and Classes for direct SQLite3 Database Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.sqlite3;

{
  *****************************************************************************

   ORM SQLite3 Database Access using mormot.db.raw.sqlite3 unit
    - TOrmTableDB as Efficient ORM-Aware TOrmTable
    - TOrmVirtualTableModuleServerDB for SQLite3 Virtual Tables
    - TRestStorageShardDB for REST Storage Sharded Over SQlite3 Files
    - TRestOrmServerDB REST Server ORM Engine over SQLite3
    - TRestOrmClientDB REST Client ORM Engine over SQLite3

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
  mormot.core.json,
  mormot.core.threads,
  mormot.core.crypto,
  mormot.core.jwt,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.server,
  mormot.db.raw.sqlite3;


{ *********** TOrmTableDB as Efficient ORM-Aware TOrmTable  }

type
  /// Execute a SQL statement in the local SQLite3 database engine, and get
  // result in memory
  // - all DATA (even the BLOB fields) is converted into UTF-8 TEXT
  // - uses a TOrmTableJson internaly: faster than sqlite3_get_table()
  // (less memory allocation/fragmentation) and allows efficient caching
  TOrmTableDB = class(TOrmTableJson)
  private
  public
    /// Execute a SQL statement, and init TOrmTable fields
    // - FieldCount=0 if no result is returned
    // - the BLOB data is converted into TEXT: you have to retrieve it with
    //  a special request explicitely (note that JSON format returns BLOB data)
    // - uses a TOrmTableJson internaly: all currency is transformed to its
    // floating point TEXT representation, and allows efficient caching
    // - if the SQL statement is in the DB cache, it's retrieved from its
    // cached value: our JSON parsing is a lot faster than SQLite3 engine
    // itself, and uses less memory
    // - will raise an Exception on any error
    constructor Create(aDB: TSqlDatabase; const Tables: array of TOrmClass;
      const aSql: RawUtf8; Expand: boolean); reintroduce;
  end;


{ *********** TOrmVirtualTableModuleServerDB for SQLite3 Virtual Tables }

type
  /// define a Virtual Table module for a stand-alone SQLite3 engine
  // - it's not needed to free this instance: it will be destroyed by the SQLite3
  // engine together with the DB connection
  TOrmVirtualTableModuleSQLite3 = class(TOrmVirtualTableModule)
  protected
    fDB: TSqlDataBase;
    /// used internaly to register the module to the SQLite3 engine
    fModule: TSqlite3Module;
  public
    /// initialize the module for a given DB connection
    // - internally set fModule and call sqlite3_create_module_v2(fModule)
    // - will raise EBusinessLayerException if aDB is incorrect, or SetDB() has
    // already been called for this module
    // - will call sqlite3_check() to raise the corresponding ESqlite3Exception
    // - in case of success (no exception), the SQLite3 engine will release the
    // module by itself; but in case of error (an exception is raised), it is
    // up to the caller to intercept it via a try..except and free the
    // TOrmVirtualTableModuleSQLite3 instance
    procedure Attach(aDB: TSqlDataBase);
    /// retrieve the file name to be used for a specific Virtual Table
    // - overridden method returning a file located in the DB file folder, and
    // '' if the main DB was created as SQLITE_MEMORY_DATABASE_NAME (i.e.
    // ':memory:' so that no file should be written)
    // - of course, if a custom FilePath property value is specified, it will be
    // used, even if the DB is created as SQLITE_MEMORY_DATABASE_NAME
    function FileName(const aTableName: RawUtf8): TFileName; override;
    /// the associated SQLite3 database connection
    property DB: TSqlDataBase
      read fDB;
  end;

  /// define a Virtual Table module for a TRestOrmServerDB SQLite3 engine
  TOrmVirtualTableModuleServerDB = class(TOrmVirtualTableModuleSQLite3)
  public
    /// register the Virtual Table to the database connection of a TRestOrmServerDB server
    // - in case of an error, an excepton will be raised
    constructor Create(aClass: TOrmVirtualTableClass; aServer: TRestOrmServer); override;
  end;

/// initialize a Virtual Table Module for a specified database
// - to be used for low-level access to a virtual module, e.g. with
// TSqlVirtualTableLog
// - when using our ORM, you should call TSqlModel.VirtualTableRegister()
// instead to associate a TSqlRecordVirtual class to a module
// - returns the created TSqlVirtualTableModule instance (which will be a
// TSqlVirtualTableModuleSQLite3 instance in fact)
// - will raise an exception of failure
function RegisterVirtualTableModule(aModule: TOrmVirtualTableClass;
  aDatabase: TSqlDataBase): TOrmVirtualTableModule;



{ *********** TRestStorageShardDB for REST Storage Sharded Over SQlite3 Files }

type
    /// REST storage sharded over several SQlite3 instances
  // - numerotated '*0000.dbs' SQLite3 files would contain the sharded data
  // - here *.dbs is used as extension, to avoid any confusion with regular
  // SQLite3 database files (*.db or *.db3)
  // - when the server is off (e.g. on periodic version upgrade), you may safely
  // delete/archive some oldest *.dbs files, for easy and immediate purge of
  // your database content: such process would be much faster and cleaner than
  // regular "DELETE FROM TABLE WHERE ID < ?" + "VACUUM" commands
  TRestStorageShardDB = class(TRestStorageShard)
  protected
    fShardRootFileName: TFileName;
    fSynchronous: TSqlSynchronousMode;
    fInitShardsIsLast: boolean;
    fCacheSizePrevious, fCacheSizeLast: integer;
    procedure InitShards; override;
    function InitNewShard: TRestOrm; override;
    function DBFileName(ShardIndex: integer): TFileName; virtual;
  public
    /// initialize the table storage redirection for sharding over SQLite3 DB
    // - if no aShardRootFileName is set, the executable folder and stored class
    // table name would be used
    // - will also register to the aServer.StaticDataServer[] internal array
    // - you may define some low-level tuning of SQLite3 process via aSynchronous
    // / aCacheSizePrevious / aCacheSizeLast / aMaxShardCount parameters, if
    // the default smOff / 1MB / 2MB / 100 values are not enough
    constructor Create(aClass: TOrmClass; aServer: TRestServer;
      aShardRange: TID;
      aOptions: TRestStorageShardOptions = [];
      const aShardRootFileName: TFileName = '';
      aMaxShardCount: integer = 100;
      aSynchronous: TSqlSynchronousMode = smOff;
      aCacheSizePrevious: integer = 250;
      aCacheSizeLast: integer = 500); reintroduce; virtual;
  published
    /// associated file name for the SQLite3 database files
    // - contains the folder, and root file name for the storage
    // - each shard would end with its 4 digits index: actual file name would
    // append '0000.dbs' to this ShardRootFileName
    property ShardRootFileName: TFileName
      read fShardRootFileName;
  end;


{ *********** TRestOrmServerDB REST ORM Engine over SQLite3 }

  TRestOrmServerDB = class(TRestOrmServer)
  protected
    /// access to the associated SQLite3 database engine
    fDB: TSqlDataBase;
    /// initialized by Create(aModel,aDBFileName)
    fOwnedDB: TSqlDataBase;
    fStatementCache: TSqlStatementCached;
    /// used during GetAndPrepareStatement() execution (run in global lock)
    fStatement: PSqlRequest;
    fStaticStatement: TSqlRequest;
    fStatementTimer: PPrecisionTimer;
    fStatementMonitor: TSynMonitor;
    fStaticStatementTimer: TPrecisionTimer;
    fStatementSql: RawUtf8;
    fStatementGenericSql: RawUtf8;
    fStatementMaxParam: integer;
    fStatementLastException: RawUtf8;
    fStatementTruncateSqlLogLen: integer;
    fStatementPreparedSelectQueryPlan: boolean;
    /// check if a VACUUM statement is possible
    // - VACUUM in fact DISCONNECT all virtual modules (sounds like a SQLite3
    // design problem), so calling it during process could break the engine
    // - if you can safely run VACUUM, returns TRUE and release all active
    // SQL statements (otherwise VACUUM will fail)
    // - if there are some static virtual tables, returns FALSE and do nothing:
    // in this case, VACUUM will be a no-op
    function PrepareVacuum(const aSql: RawUtf8): boolean;
  protected
    fBatchMethod: TUriMethod;
    fBatchOptions: TRestBatchOptions;
    fBatchTableIndex: integer;
    fBatchID: TIDDynArray;
    fBatchIDCount: integer;
    fBatchIDMax: TID;
    fBatchValues: TRawUtf8DynArray;
    fBatchValuesCount: integer;
    /// retrieve a TSqlRequest instance in fStatement
    // - will set @fStaticStatement if no :(%): internal parameters appear:
    // in this case, the TSqlRequest.Close method must be called
    // - will set a @fStatementCache[].Statement, after having bounded the
    // :(%): parameter values; in this case, TSqlRequest.Close must not be called
    // - expect sftBlob, sftBlobDynArray and sftBlobRecord properties
    // to be encoded as ':("\uFFF0base64encodedbinary"):'
    procedure GetAndPrepareStatement(const SQL: RawUtf8;
      ForceCacheStatement: boolean);
    /// free a static prepared statement on success or from except on E: Exception block
    procedure GetAndPrepareStatementRelease(E: Exception = nil;
      const Msg: ShortString = ''; ForceBindReset: boolean = false); overload;
    procedure GetAndPrepareStatementRelease(E: Exception;
      const Format: RawUtf8; const Args: array of const;
      ForceBindReset: boolean = false); overload;
    /// create or retrieve from the cache a TSqlRequest instance in fStatement
    // - called e.g. by GetAndPrepareStatement()
    procedure PrepareStatement(Cached: boolean);
  public
    /// overridden methods for direct sqlite3 database engine call:
    function MainEngineList(const SQL: RawUtf8; ForceAjax: boolean;
      ReturnedRowCount: PPtrInt): RawUtf8; override;
    function MainEngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function MainEngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function MainEngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function MainEngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function MainEngineDeleteWhere(TableModelIndex: integer;
      const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean; override;
    function MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function MainEngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue,
            WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function MainEngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    function EngineExecute(const aSql: RawUtf8): boolean; override;
    /// execute one SQL statement
    // - intercept any DB exception and return false on error, true on success
    // - optional LastInsertedID can be set (if ValueInt/ValueUTF8 are nil) to
    // retrieve the proper ID when aSql is an INSERT statement (thread safe)
    // - optional LastChangeCount can be set (if ValueInt/ValueUTF8 are nil) to
    // retrieve the modified row count when aSql is an UPDATE statement (thread safe)
    function InternalExecute(const aSql: RawUtf8; ForceCacheStatement: boolean;
      ValueInt: PInt64 = nil; ValueUTF8: PRawUtf8 = nil;
      ValueInts: PInt64DynArray = nil; LastInsertedID: PInt64 = nil;
      LastChangeCount: PInteger = nil): boolean;
    // overridden method returning TRUE for next calls to EngineAdd
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Method: TUriMethod;
      BatchOptions: TRestBatchOptions): boolean; override;
    // internal method called by TRestOrmServer.RunBatch() to process fast
    // multi-INSERT statements to the SQLite3 engine
    procedure InternalBatchStop; override;
    /// reset the cache if necessary
    procedure SetNoAjaxJson(const Value: boolean); override;
  public
    /// begin a transaction (implements REST BEGIN Member)
    // - to be used to speed up some SQL statements like Insert/Update/Delete
    // - must be ended with Commit on success
    // - must be aborted with Rollback if any SQL statement failed
    // - return true if no transaction is active, false otherwise
    function TransactionBegin(aTable: TOrmClass;
      SessionID: cardinal = 1): boolean; override;
    /// end a transaction (implements REST END Member)
    // - write all pending SQL statements to the disk
    procedure Commit(SessionID: cardinal = 1;
      RaiseException: boolean = false); override;
    /// abort a transaction (implements REST ABORT Member)
    // - restore the previous state of the database, before the call to TransactionBegin
    procedure RollBack(SessionID: cardinal = 1); override;

     /// overridden method for direct SQLite3 database engine call
     // - it will update all BLOB fields at once, in one SQL statement
    function UpdateBlobFields(Value: TOrm): boolean; override;
     /// overridden method for direct SQLite3 database engine call
     // - it will retrieve all BLOB fields at once, in one SQL statement
    function RetrieveBlobFields(Value: TOrm): boolean; override;

    /// retrieves the per-statement detailed timing, as a TDocVariantData
    procedure ComputeDBStats(out result: variant); overload;
    /// retrieves the per-statement detailed timing, as a TDocVariantData
    function ComputeDBStats: variant; overload;

    /// initialize the associated DB connection
    // - called by Create and on Backup/Restore just after DB.DBOpen
    // - will register all *_in() functions for available TOrmRTree
    // - will register all modules for available TOrmVirtualTable*ID
    // with already registered modules via RegisterVirtualTableModule()
    // - you can override this method to call e.g. DB.RegisterSQLFunction()
    procedure InitializeEngine; virtual;
    /// call this method when the internal DB content is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but some virtual tables (e.g.
    // TRestStorageExternal classes defined in SQLite3DB) could flush
    // the database content without proper notification
    // - this overridden implementation will call TSqlDataBase.CacheFlush method
    procedure FlushInternalDBCache; override;
    /// call this method to flush the internal SQL prepared statements cache
    // - you should not have to flush the cache, only e.g. before a DROP TABLE
    // - in all cases, running this method would never harm, nor be slow
    procedure FlushStatementCache;
    /// execute one SQL statement, and apply an Event to every record
    // - lock the database during the run
    // - call a fast "stored procedure"-like method for each row of the request;
    // this method must use low-level DB access in any attempt to modify the
    // database (e.g. a prepared TSqlRequest with Reset+Bind+Step), and not
    // the TRestOrmServerDB.Engine*() methods which include a Lock(): this Lock()
    // is performed by the main loop in EngineExecute() and any attempt to
    // such high-level call will fail into an endless loop
    // - caller may use a transaction in order to speed up StoredProc() writing
    // - intercept any DB exception and return false on error, true on success
    function StoredProcExecute(const aSql: RawUtf8;
      const StoredProc: TOnSqlStoredProc): boolean;
  public
    /// initialize a TRest-owned ORM server with an in-memory SQLite3 database
    constructor Create(aRest: TRest); overload; override;
    /// initialize a TRest-owned ORM server with a given SQLite3 database
    // - you should specify a TSqlDataBase and a TRest associated instance
    constructor Create(aRest: TRest;
      aDB: TSqlDataBase; aOwnDB: boolean); reintroduce; overload; virtual;
    /// initialize a stand-alone REST ORM server with a given SQLite3 database
    // - you can specify an associated TOrmModel but no TRest
    constructor CreateStandalone(aModel: TOrmModel; aRest: TRest;
      aDB: TSqlDataBase; aOwnDB: boolean); reintroduce;
    /// close any owned database and free used memory
    destructor Destroy; override;
    /// Missing tables are created if they don't exist yet for every TOrm
    // class of the Database Model
    // - you must call explicitely this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method also create additional fields, if the TOrm definition
    // has been modified; only field adding is available, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TOrm type)
    procedure CreateMissingTables(user_version: cardinal = 0;
      Options: TOrmInitializeTableOptions = []); override;
    /// search for the last inserted ID in a table
    // - will execute not default select max(rowid) from Table, but faster
    // $ select rowid from Table order by rowid desc limit 1
    function TableMaxID(Table: TOrmClass): TID; override;
    /// prepared statements with parameters for faster SQLite3 execution
    // - used for SQL code with :(%): internal parameters
    property StatementCache: TSqlStatementCached
      read fStatementCache;
    /// after how many bytes a sllSQL statement log entry should be truncated
    // - default is 0, meaning no truncation
    // - typical value is 2048 (2KB), which will avoid any heap allocation
    property StatementTruncateSqlLogLen: integer
      read fStatementTruncateSqlLogLen write fStatementTruncateSqlLogLen;
    /// executes (therefore log) the QUERY PLAN for each prepared statement
    property StatementPreparedSelectQueryPlan: boolean
      read fStatementPreparedSelectQueryPlan
      write fStatementPreparedSelectQueryPlan;
  published
    /// associated database
    property DB: TSqlDataBase
      read fDB;
    /// contains some textual information about the latest Exception raised
    // during SQL statement execution
    property StatementLastException: RawUtf8
      read fStatementLastException;
  end;


{ *********** TRestOrmClientDB REST Client ORM Engine over SQLite3 }

type
  /// REST ORM client with direct access to a SQLite3 database
  // - a hidden TRestOrmDB class is created and called internaly
  TRestOrmClientDB = class(TRestOrmClientUri)
  private
    // use internaly a TRestServerDB to access data in the proper JSON format
    fServer: TRestOrmServerDB;
    function GetDB: TSqlDataBase;
  public
    /// initialize the ORM storage, with the associated ORM Server
    constructor Create(aRest: TRest; aServer: TRestOrmServerDB); reintroduce;
    /// retrieve a list of members as a TOrmTable (implements REST GET Collection)
    // - this overridden method call directly the database to get its result,
    // without any Uri() call, but with use of DB JSON cache if available
    // - other TRestClientDB methods use Uri() function and JSON conversion
    // of only one record properties values, which is very fast
    function List(const Tables: array of TOrmClass;
      const SqlSelect: RawUtf8 = 'ID';
      const SqlWhere: RawUtf8 = ''): TOrmTable; override;
    /// associated ORM Server
    property Server: TRestOrmServerDB
      read fServer;
    /// associated database
    property DB: TSqlDataBase
      read GetDB;
  end;



implementation


{ *********** TOrmTableDB as Efficient ORM-Aware TOrmTable  }

{ TOrmTableDB }

constructor TOrmTableDB.Create(aDB: TSqlDatabase;
  const Tables: array of TOrmClass; const aSql: RawUtf8; Expand: boolean);
var
  JSONCached: RawUtf8;
  R: TSqlRequest;
  n: PtrInt;
begin
  if aDB = nil then
    exit;
  JSONCached := aDB.LockJson(aSql, @n);
  if JSONCached = '' then
    // not retrieved from cache -> call SQLite3 engine
    try
      n := 0;
      JSONCached := R.ExecuteJson(aDB.DB, aSql, Expand, @n);
      // big JSON is faster than sqlite3_get_table(): less heap allocations
      inherited CreateFromTables(Tables, aSql, JSONCached);
      Assert(n = fRowCount);
    finally
      aDB.UnLockJson(JSONCached, n);
    end
  else
  begin
    inherited CreateFromTables(Tables, aSql, JSONCached);
    Assert(n = fRowCount);
  end;
end;



{ *********** TOrmVirtualTableModuleServerDB for SQLite3 Virtual Tables }

// asssociated low-level vt*() SQlite3 wrapper functions

procedure Notify(const Format: RawUtf8; const Args: array of const);
begin
  TSynLog.DebuggerNotify(sllWarning, Format, Args);
end;

function vt_Create(DB: TSqlite3DB; pAux: Pointer; argc: integer;
  const argv: PPUtf8CharArray; var ppVTab: PSqlite3VTab;
  var pzErr: PUtf8Char): integer; cdecl;
var
  Module: TOrmVirtualTableModuleSQLite3 absolute pAux;
  Table: TOrmVirtualTable;
  Structure: RawUtf8;
  ModuleName: RawUtf8;
begin
  if Module <> nil then
    ModuleName := Module.ModuleName;
  if (Module = nil) or
     (Module.DB.DB <> DB) or
     (StrIComp(pointer(ModuleName), argv[0]) <> 0) then
  begin
    Notify('vt_Create(%<>%)', [argv[0], ModuleName]);
    result := SQLITE_ERROR;
    exit;
  end;
  ppVTab := sqlite3.malloc(sizeof(TSqlite3VTab));
  if ppVTab = nil then
  begin
    result := SQLITE_NOMEM;
    exit;
  end;
  FillcharFast(ppVTab^, sizeof(ppVTab^), 0);
  try
    Table := Module.TableClass.Create(
      Module, RawUtf8(argv[2]), argc - 3, @argv[3]);
  except
    on E: Exception do
    begin
      ExceptionToSqlite3Err(E, pzErr);
      sqlite3.free_(ppVTab);
      result := SQLITE_ERROR;
      exit;
    end;
  end;
  Structure := Table.Structure;
  result := sqlite3.declare_vtab(DB, pointer(Structure));
  if result <> SQLITE_OK then
  begin
    Notify('vt_Create(%) declare_vtab(%)', [ModuleName, Structure]);
    Table.Free;
    sqlite3.free_(ppVTab);
    result := SQLITE_ERROR;
  end
  else
    ppVTab^.pInstance := Table;
end;

function vt_Disconnect(pVTab: PSqlite3VTab): integer; cdecl;
begin
  TOrmVirtualTable(pVTab^.pInstance).Free;
  sqlite3.free_(pVTab);
  result := SQLITE_OK;
end;

function vt_Destroy(pVTab: PSqlite3VTab): integer; cdecl;
begin
  if TOrmVirtualTable(pVTab^.pInstance).Drop then
    result := SQLITE_OK
  else
  begin
    Notify('vt_Destroy', []);
    result := SQLITE_ERROR;
  end;
  vt_Disconnect(pVTab); // release memory
end;

function vt_BestIndex(var pVTab: TSqlite3VTab;
  var pInfo: TSqlite3IndexInfo): integer; cdecl;
const
  COST: array[TOrmVirtualTablePreparedCost] of double = (
         1E10, 1E8, 10, 1);
      // costFullScan, costScanWhere, costSecondaryIndex, costPrimaryIndex
var
  Prepared: POrmVirtualTablePrepared;
  Table: TOrmVirtualTable;
  i, n: PtrInt;
begin
  result := SQLITE_ERROR;
  Table := TOrmVirtualTable(pVTab.pInstance);
  if (cardinal(pInfo.nOrderBy) > MAX_SQLFIELDS) or
     (cardinal(pInfo.nConstraint) > MAX_SQLFIELDS) then
  begin
    // avoid buffer overflow
    Notify('nOrderBy=% nConstraint=%', [pInfo.nOrderBy, pInfo.nConstraint]);
    exit;
  end;
  Prepared := sqlite3.malloc(sizeof(TOrmVirtualTablePrepared));
  try
    // encode the incoming parameters into Prepared^ record
    Prepared^.WhereCount := pInfo.nConstraint;
    Prepared^.EstimatedCost := costFullScan;
    for i := 0 to pInfo.nConstraint - 1 do
      with Prepared^.Where[i], pInfo.aConstraint^[i] do
      begin
        OmitCheck := False;
        Value.VType := ftUnknown;
        if usable then
        begin
          Column := iColumn;
          case op of
            SQLITE_INDEX_CONSTRAINT_EQ:
              Operation := soEqualTo;
            SQLITE_INDEX_CONSTRAINT_GT:
              Operation := soGreaterThan;
            SQLITE_INDEX_CONSTRAINT_LE:
              Operation := soLessThanOrEqualTo;
            SQLITE_INDEX_CONSTRAINT_LT:
              Operation := soLessThan;
            SQLITE_INDEX_CONSTRAINT_GE:
              Operation := soGreaterThanOrEqualTo;
            SQLITE_INDEX_CONSTRAINT_MATCH:
              Operation := soBeginWith;
          else
            Column := VIRTUAL_TABLE_IGNORE_COLUMN; // unhandled operator
          end;
        end
        else
          Column := VIRTUAL_TABLE_IGNORE_COLUMN;
      end;
    Prepared^.OmitOrderBy := false;
    if pInfo.nOrderBy > 0 then
    begin
      assert(sizeof(TOrmVirtualTablePreparedOrderBy) = sizeof(TSqlite3IndexOrderBy));
      Prepared^.OrderByCount := pInfo.nOrderBy;
      MoveFast(pInfo.aOrderBy^[0], Prepared^.OrderBy[0],
        pInfo.nOrderBy * sizeof(Prepared^.OrderBy[0]));
    end
    else
      Prepared^.OrderByCount := 0;
    // perform the index query
    if not Table.Prepare(Prepared^) then
      exit;
    // update pInfo and store Prepared into pInfo.idxStr for vt_Filter()
    n := 0;
    for i := 0 to pInfo.nConstraint - 1 do
      if Prepared^.Where[i].Value.VType <> ftUnknown then
      begin
        if i <> n then
          // expression needed for Search() method to be moved at [n]
          MoveFast(Prepared^.Where[i], Prepared^.Where[n],
            sizeof(Prepared^.Where[i]));
        inc(n);
        pInfo.aConstraintUsage[i].argvIndex := n;
        pInfo.aConstraintUsage[i].omit := Prepared^.Where[i].OmitCheck;
      end;
    Prepared^.WhereCount := n; // will match argc in vt_Filter()
    if Prepared^.OmitOrderBy then
      pInfo.orderByConsumed := 1
    else
      pInfo.orderByConsumed := 0;
    pInfo.estimatedCost := COST[Prepared^.EstimatedCost];
    if sqlite3.VersionNumber >= 3008002000 then
      // starting with SQLite 3.8.2: fill estimatedRows
      case Prepared^.EstimatedCost of
        costFullScan:
          pInfo.estimatedRows := Prepared^.EstimatedRows;
        costScanWhere:
          // estimate a WHERE clause is a slight performance gain
          pInfo.estimatedRows := Prepared^.EstimatedRows shr 1;
        costSecondaryIndex:
          pInfo.estimatedRows := 10;
        costPrimaryIndex:
          pInfo.estimatedRows := 1;
      else
        raise EOrmException.Create('vt_BestIndex: unexpected EstimatedCost');
      end;
    pInfo.idxStr := pointer(Prepared);
    pInfo.needToFreeIdxStr := 1; // will do sqlite3.free(idxStr) when needed
    result := SQLITE_OK;
    {$ifdef OrmVirtualLOGS}
    if Table.Static is TRestStorageExternal then
      TRestStorageExternal(Table.Static).ComputeSql(Prepared^);
    SQLite3Log.Add.Log(sllDebug, 'vt_BestIndex(%) plan=% -> cost=% rows=%',
      [sqlite3.VersionNumber, ord(Prepared^.EstimatedCost),
       pInfo.estimatedCost, pInfo.estimatedRows]);
    {$endif OrmVirtualLOGS}
  finally
    if result <> SQLITE_OK then
      // avoid memory leak on error
      sqlite3.free_(Prepared);
  end;
end;

function vt_Filter(var pVtabCursor: TSqlite3VTabCursor; idxNum: integer;
  const idxStr: PAnsiChar; argc: integer;
  var argv: TSqlite3ValueArray): integer; cdecl;
var
  Prepared: POrmVirtualTablePrepared absolute idxStr; // idxNum is not used
  i: PtrInt;
begin
  result := SQLITE_ERROR;
  if Prepared^.WhereCount <> argc then
  begin
    // invalid prepared array (should not happen)
    Notify('vt_Filter WhereCount=% argc=%', [Prepared^.WhereCount, argc]);
    exit;
  end;
  for i := 0 to argc - 1 do
    SQlite3ValueToSqlVar(argv[i], Prepared^.Where[i].Value);
  if TOrmVirtualTableCursor(pVtabCursor.pInstance).Search(Prepared^) then
    result := SQLITE_OK
  else
    Notify('vt_Filter Search()', []);
end;

function vt_Open(var pVTab: TSqlite3VTab;
  var ppCursor: PSqlite3VTabCursor): integer; cdecl;
var
  Table: TOrmVirtualTable;
begin
  ppCursor := sqlite3.malloc(sizeof(TSqlite3VTabCursor));
  if ppCursor = nil then
  begin
    result := SQLITE_NOMEM;
    exit;
  end;
  Table := TOrmVirtualTable(pVTab.pInstance);
  if (Table = nil) or
     (Table.Module = nil) or
     (Table.Module.CursorClass = nil) then
  begin
    Notify('vt_Open', []);
    sqlite3.free_(ppCursor);
    result := SQLITE_ERROR;
    exit;
  end;
  ppCursor.pInstance := Table.Module.CursorClass.Create(Table);
  result := SQLITE_OK;
end;

function vt_Close(pVtabCursor: PSqlite3VTabCursor): integer; cdecl;
begin
  TOrmVirtualTableCursor(pVtabCursor^.pInstance).Free;
  sqlite3.free_(pVtabCursor);
  result := SQLITE_OK;
end;

function vt_next(var pVtabCursor: TSqlite3VTabCursor): integer; cdecl;
begin
  if TOrmVirtualTableCursor(pVtabCursor.pInstance).Next then
    result := SQLITE_OK
  else
    result := SQLITE_ERROR;
end;

function vt_Eof(var pVtabCursor: TSqlite3VTabCursor): integer; cdecl;
begin
  if TOrmVirtualTableCursor(pVtabCursor.pInstance).HasData then
    result := 0
  else
    result := 1; // reached actual EOF
end;

function vt_Column(var pVtabCursor: TSqlite3VTabCursor;
  sContext: TSqlite3FunctionContext; N: integer): integer; cdecl;
var
  Res: TSqlVar;
begin
  Res.VType := ftUnknown;
  if (N >= 0) and
     TOrmVirtualTableCursor(pVtabCursor.pInstance).Column(N, Res) and
     SqlVarToSQlite3Context(Res, sContext) then
    result := SQLITE_OK
  else
  begin
    Notify('vt_Column(%) Res=%', [N, ord(Res.VType)]);
    result := SQLITE_ERROR;
  end;
end;

function vt_Rowid(var pVtabCursor: TSqlite3VTabCursor;
  var pRowid: Int64): integer; cdecl;
var
  Res: TSqlVar;
begin
  result := SQLITE_ERROR;
  with TOrmVirtualTableCursor(pVtabCursor.pInstance) do
    if Column(-1, Res) then
    begin
      case Res.VType of
        ftInt64:
          pRowid := Res.VInt64;
        ftDouble:
          pRowid := trunc(Res.VDouble);
        ftCurrency:
          pRowid := trunc(Res.VCurrency);
        ftUtf8:
          pRowid := GetInt64(Res.VText);
      else
        begin
          Notify('vt_Rowid Res=%', [ord(Res.VType)]);
          exit;
        end;
      end;
      result := SQLITE_OK;
    end
    else
      Notify('vt_Rowid Column', []);
end;

function vt_Update(var pVTab: TSqlite3VTab; nArg: integer;
  var ppArg: TSqlite3ValueArray; var pRowid: Int64): integer; cdecl;
var
  Values: TSqlVarDynArray;
  Table: TOrmVirtualTable;
  RowID0, RowID1: Int64;
  i: PtrInt;
  OK: boolean;
begin
  // call Delete/Insert/Update methods according to supplied parameters
  Table := TOrmVirtualTable(pVTab.pInstance);
  result := SQLITE_ERROR;
  if (nArg <= 0) or
     (nArg > 1024) then
    exit;
  case sqlite3.value_type(ppArg[0]) of
    SQLITE_INTEGER:
      RowID0 := sqlite3.value_int64(ppArg[0]);
    SQLITE_NULL:
      RowID0 := 0;
  else
    exit; // invalid call
  end;
  if nArg = 1 then
    OK := Table.Delete(RowID0)
  else
  begin
    case sqlite3.value_type(ppArg[1]) of
      SQLITE_INTEGER:
        RowID1 := sqlite3.value_int64(ppArg[1]);
      SQLITE_NULL:
        RowID1 := 0;
    else
      exit; // invalid call
    end;
    SetLength(Values, nArg - 2);
    for i := 0 to nArg - 3 do
      SQlite3ValueToSqlVar(ppArg[i + 2], Values[i]);
    if RowID0 = 0 then
      OK := Table.Insert(RowID1, Values, pRowid)
    else
      OK := Table.Update(RowID0, RowID1, Values);
  end;
  if OK then
    result := SQLITE_OK
  else
    Notify('vt_Update(%)', [pRowid]);
end;

function InternalTrans(pVTab: TSqlite3VTab; aState: TOrmVirtualTableTransaction;
  aSavePoint: integer): integer;
begin
  if TOrmVirtualTable(pVTab.pInstance).Transaction(aState, aSavePoint) then
    result := SQLITE_OK
  else
  begin
    Notify('Transaction(%,%)', [ToText(aState)^, aSavePoint]);
    result := SQLITE_ERROR;
  end;
end;

function vt_Begin(var pVTab: TSqlite3VTab): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttBegin, 0);
end;

function vt_Commit(var pVTab: TSqlite3VTab): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttCommit, 0);
end;

function vt_RollBack(var pVTab: TSqlite3VTab): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttRollBack, 0);
end;

function vt_Sync(var pVTab: TSqlite3VTab): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttSync, 0);
end;

function vt_SavePoint(var pVTab: TSqlite3VTab; iSavepoint: integer): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttSavePoint, iSavepoint);
end;

function vt_Release(var pVTab: TSqlite3VTab; iSavepoint: integer): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttRelease, iSavepoint);
end;

function vt_RollBackTo(var pVTab: TSqlite3VTab; iSavepoint: integer): integer; cdecl;
begin
  result := InternalTrans(pVTab, vttRollBackTo, iSavepoint);
end;

function vt_Rename(var pVTab: TSqlite3VTab; const zNew: PAnsiChar): integer; cdecl;
begin
  if TOrmVirtualTable(pVTab.pInstance).Rename(RawUtf8(zNew)) then
    result := SQLITE_OK
  else
  begin
    Notify('vt_Rename(%)', [zNew]);
    result := SQLITE_ERROR;
  end;
end;

procedure sqlite3InternalFreeModule(p: pointer); cdecl;
begin
  if (p <> nil) and
     (TOrmVirtualTableModuleSQLite3(p).fDB <> nil) then
    TOrmVirtualTableModuleSQLite3(p).Free;
end;


{ TOrmVirtualTableModuleSQLite3 }

function TOrmVirtualTableModuleSQLite3.FileName(
  const aTableName: RawUtf8): TFileName;
begin
  if FilePath <> '' then
    // if a file path is specified (e.g. by SynDBExplorer) -> always use this
    result := inherited FileName(aTableName)
  else if SameText(DB.FileName, SQLITE_MEMORY_DATABASE_NAME) then
    // in-memory databases virtual tables should remain in memory
    result := ''
  else
    // change file path to current DB folder
    result := ExtractFilePath(DB.FileName) +
              ExtractFileName(inherited FileName(aTableName));
end;

procedure TOrmVirtualTableModuleSQLite3.Attach(aDB: TSqlDataBase);
begin
  if aDB = nil then
    raise ERestStorage.CreateUtf8('aDB=nil at %.SetDB()', [self]);
  if fDB <> nil then
    raise ERestStorage.CreateUtf8('fDB<>nil at %.SetDB()', [self]);
  FillCharFast(fModule, sizeof(fModule), 0);
  fModule.iVersion := 1;
  fModule.xCreate := vt_Create;
  fModule.xConnect := vt_Create;
  fModule.xBestIndex := vt_BestIndex;
  fModule.xDisconnect := vt_Disconnect;
  fModule.xDestroy := vt_Destroy;
  fModule.xOpen := vt_Open;
  fModule.xClose := vt_Close;
  fModule.xFilter := vt_Filter;
  fModule.xNext := vt_Next;
  fModule.xEof := vt_Eof;
  fModule.xColumn := vt_Column;
  fModule.xRowid := vt_Rowid;
  if vtWrite in Features then
  begin
    fModule.xUpdate := vt_Update;
    if vtTransaction in Features then
    begin
      fModule.xBegin := vt_Begin;
      fModule.xSync := vt_Sync;
      fModule.xCommit := vt_Commit;
      fModule.xRollback := vt_RollBack;
    end;
    if vtSavePoint in Features then
    begin
      fModule.iVersion := 2;
      fModule.xSavePoint := vt_SavePoint;
      fModule.xRelease := vt_Release;
      fModule.xRollBackTo := vt_RollBackTo;
    end;
    fModule.xRename := vt_Rename;
  end;
  sqlite3_check(aDB.DB, sqlite3.create_module_v2(aDB.DB, pointer(fModuleName),
    fModule, self, sqlite3InternalFreeModule)); // raise ESqlite3Exception on error
  fDB := aDB; // mark successfull create_module() for sqlite3InternalFreeModule
end;


{ TOrmVirtualTableModuleServerDB }

constructor TOrmVirtualTableModuleServerDB.Create(aClass: TOrmVirtualTableClass;
  aServer: TRestOrmServer);
begin
  if not aServer.InheritsFrom(TRestOrmServerDB) then
    raise ERestStorage.CreateUtf8('%.Create expects a DB Server', [self]);
  inherited;
  Attach(TRestOrmServerDB(aServer).DB);
  // any exception in Attach() will let release the instance by the RTL
end;


function RegisterVirtualTableModule(aModule: TOrmVirtualTableClass;
  aDatabase: TSqlDataBase): TOrmVirtualTableModule;
begin
  result := TOrmVirtualTableModuleSQLite3.Create(aModule, nil);
  try
    TOrmVirtualTableModuleSQLite3(result).Attach(aDatabase);
  except
    on Exception do begin
      result.Free; // should be released by hand here
      raise; // e.g. EBusinessLayerException or ESqlite3Exception
    end;
  end;
end;


{ *********** TRestStorageShardDB for REST Storage Sharded Over SQlite3 Files }

{ TRestStorageShardDB }

constructor TRestStorageShardDB.Create(aClass: TOrmClass;
  aServer: TRestServer; aShardRange: TID;
  aOptions: TRestStorageShardOptions; const aShardRootFileName: TFileName;
  aMaxShardCount: integer; aSynchronous: TSqlSynchronousMode;
  aCacheSizePrevious, aCacheSizeLast: integer);
var
  orm: TRestOrmServer;
begin
  fShardRootFileName := aShardRootFileName;
  fSynchronous := aSynchronous;
  fCacheSizePrevious := aCacheSizePrevious;
  fCacheSizeLast := aCacheSizeLast;
  orm := aServer.OrmInstance as TRestOrmServer;
  if orm = nil then
    raise ERestStorage.CreateUtf8(
      '%.Create: % has no OrmInstance - use TRestServerDB', [self, aServer]);
  inherited Create(aClass, orm, aShardRange, aOptions, aMaxShardCount);
  orm.StaticTableSetup(fStoredClassProps.TableIndex, self, sStaticDataTable);
end;

function TRestStorageShardDB.DBFileName(ShardIndex: integer): TFileName;
begin
  result := Format('%s%.4d.dbs',
    [fShardRootFileName, fShardOffset + ShardIndex]);
end;

function TRestStorageShardDB.InitNewShard: TRestOrm;
var
  db: TRestOrmServerDB;
  cachesize: integer;
  sql: TSqlDataBase;
  model: TOrmModel;
begin
  inc(fShardLast);
  model := TOrmModel.Create([fStoredClass], FormatUtf8('shard%', [fShardLast]));
  if fInitShardsIsLast then
    // last/new .dbs = 2MB cache, previous 1MB only
    cachesize := fCacheSizeLast
  else
    cachesize := fCacheSizePrevious;
  sql := TSqlDatabase.Create(DBFileName(fShardLast), '', 0, cachesize);
  sql.LockingMode := lmExclusive;
  sql.Synchronous := fSynchronous;
  db := TRestOrmServerDB.CreateStandalone(model, fRest, sql, {owndb=}true);
  db._AddRef;
  db.CreateMissingTables;
  result := db;
  SetLength(fShards, fShardLast + 1);
  fShards[fShardLast] := result;
end;

procedure TRestStorageShardDB.InitShards;
var
  f, i, first: PtrInt;
  num: integer;
  db: TFindFilesDynArray;
  mask: TFileName;
begin
  if fShardRootFileName = '' then
    fShardRootFileName := ExeVersion.ProgramFilePath +
      Utf8ToString(fStoredClass.SqlTableName);
  mask := DBFileName(0);
  i := Pos('0000', mask);
  if i > 0 then
  begin
    system.Delete(mask, i, 3);
    mask[i] := '*';
  end
  else
    mask := fShardRootFileName + '*.dbs';
  db := FindFiles(ExtractFilePath(mask), ExtractFileName(mask),
    '', {sorted=}true);
  if db = nil then
    exit; // no existing data
  fShardOffset := -1;
  first := length(db) - integer(fMaxShardCount);
  if first < 0 then
    first := 0;
  for f := first to high(db) do
  begin
    i := Pos('.dbs', db[f].Name);
    if (i <= 4) or
       not TryStrToInt(Copy(db[f].Name, i - 4, 4), num) then
    begin
      InternalLog('InitShards(%)?', [db[f].Name], sllWarning);
      continue;
    end;
    if fShardOffset < 0 then
      fShardOffset := num;
    dec(num, fShardOffset);
    if not SameText(DBFileName(num), db[f].Name) then
      raise EOrmException.CreateUtf8('%.InitShards(%)', [self, db[f].Name]);
    if f = high(db) then
      fInitShardsIsLast := true;
    fShardLast := num - 1; // 'folder\root0005.dbs' -> fShardLast := 4
    InitNewShard;         // now fShardLast=5, fShards[5] contains root005.dbs
  end;
  if fShardOffset < 0 then
    fShardOffset := 0;
  if integer(fShardLast) < 0 then
  begin
    InternalLog('InitShards?', sllWarning);
    exit;
  end;
  fInitShardsIsLast := true; // any newly appended .dbs would use 2MB of cache
  fShardLastID := fShards[fShardLast].TableMaxID(fStoredClass);
  if fShardLastID < 0 then
    fShardLastID := 0; // no data yet
end;



{ *********** TRestOrmServerDB REST ORM Engine over SQLite3 }

{ TRestOrmServerDB }

procedure TRestOrmServerDB.PrepareStatement(Cached: boolean);
var
  wasPrepared: boolean;
  timer: PPPrecisionTimer;
begin
  fStaticStatementTimer.Start;
  if not Cached then
  begin
    fStaticStatement.Prepare(DB.DB, fStatementGenericSql);
    fStatementGenericSql := '';
    fStatement := @fStaticStatement;
    fStatementTimer := @fStaticStatementTimer;
    fStatementMonitor := nil;
    exit;
  end;
  if (fOwner <> nil) and
     (mlSQLite3 in fOwner.StatLevels) then
    timer := @fStatementTimer
  else
    timer := nil;
  fStatement := fStatementCache.Prepare(fStatementGenericSql, @wasPrepared,
    timer, @fStatementMonitor);
  if wasPrepared then
  begin
    InternalLog('prepared % % %', [fStaticStatementTimer.Stop,
      DB.FileNameWithoutPath, fStatementGenericSql], sllDB);
    if fStatementPreparedSelectQueryPlan then
      DB.ExecuteJson('explain query plan ' +
        StringReplaceChars(fStatementGenericSql, '?', '1'), {expand=}true);
  end;
  if timer = nil then
  begin
    fStaticStatementTimer.Start;
    fStatementTimer := @fStaticStatementTimer;
    fStatementMonitor := nil;
  end;
end;

procedure TRestOrmServerDB.GetAndPrepareStatement(const SQL: RawUtf8;
  ForceCacheStatement: boolean);
var
  i, sqlite3param: PtrInt;
  Types: TSqlParamTypeDynArray;
  Nulls: TFieldBits;
  Values: TRawUtf8DynArray;
begin
  // prepare statement
  fStatementSql := SQL;
  fStatementGenericSql := ExtractInlineParameters(
    SQL, Types, Values, fStatementMaxParam, Nulls);
  PrepareStatement(ForceCacheStatement or (fStatementMaxParam <> 0));
  // bind parameters
  if fStatementMaxParam = 0 then
    exit; // no valid :(...): inlined parameter found -> manual bind
  sqlite3param := sqlite3.bind_parameter_count(fStatement^.Request);
  if sqlite3param <> fStatementMaxParam then
    raise EOrmException.CreateUtf8(
      '%.GetAndPrepareStatement(%) recognized % params, and % for SQLite3',
      [self, fStatementGenericSql, fStatementMaxParam, sqlite3param]);
  for i := 0 to fStatementMaxParam - 1 do
    if i in Nulls then
      fStatement^.BindNull(i + 1)
    else
      case Types[i] of
        sptDateTime, // date/time are stored as ISO-8601 TEXT in SQLite3
        sptText:
          fStatement^.Bind(i + 1, Values[i]);
        sptBlob:
          fStatement^.BindBlob(i + 1, Values[i]);
        sptInteger:
          fStatement^.Bind(i + 1, GetInt64(pointer(Values[i])));
        sptFloat:
          fStatement^.Bind(i + 1, GetExtended(pointer(Values[i])));
      end;
end;

procedure TRestOrmServerDB.GetAndPrepareStatementRelease(E: Exception;
  const Msg: ShortString; ForceBindReset: boolean);
var
  c: AnsiChar;
begin
  try
    if fStatementTimer <> nil then
    begin
      if fStatementMonitor <> nil then
        fStatementMonitor.ProcessEnd
      else
        fStatementTimer^.Pause;
      if E = nil then
        if (fStatementTruncateSqlLogLen > 0) and
           (length(fStatementSql) > fStatementTruncateSqlLogLen) then
        begin
          c := fStatementSql[fStatementTruncateSqlLogLen];
          fStatementSql[fStatementTruncateSqlLogLen] := #0; // truncate
          InternalLog('% % %... len=%', [fStatementTimer^.LastTime, Msg,
            PAnsiChar(pointer(fStatementSql)), length(fStatementSql)], sllSQL);
          fStatementSql[fStatementTruncateSqlLogLen] := c; // restore
        end
        else
          InternalLog('% % %', [fStatementTimer^.LastTime, Msg, fStatementSql], sllSQL)
      else
        InternalLog('% for % // %', [E, fStatementSql, fStatementGenericSql], sllError);
      fStatementTimer := nil;
    end;
    fStatementMonitor := nil;
  finally
    if fStatement <> nil then
    begin
      if fStatement = @fStaticStatement then
        fStaticStatement.Close
      else if (fStatementMaxParam <> 0) or
              ForceBindReset then
        fStatement^.BindReset; // release bound RawUtf8 ASAP
      fStatement := nil;
    end;
    fStatementSql := '';
    fStatementGenericSql := '';
    fStatementMaxParam := 0;
    if E <> nil then
      FormatUtf8('% %', [E, ObjectToJsonDebug(E)], fStatementLastException);
  end;
end;

procedure TRestOrmServerDB.GetAndPrepareStatementRelease(E: Exception;
  const Format: RawUtf8; const Args: array of const; ForceBindReset: boolean);
var
  msg: shortstring;
begin
  FormatShort(Format, Args, msg);
  GetAndPrepareStatementRelease(E, msg, ForceBindReset);
end;

procedure TRestOrmServerDB.FlushStatementCache;
begin
  DB.Lock;
  try
    fStatementCache.ReleaseAllDBStatements;
  finally
    DB.Unlock;
  end;
end;

function TRestOrmServerDB.TableMaxID(Table: TOrmClass): TID;
var
  SQL: RawUtf8;
begin
  if StaticTable[Table] <> nil then
    result := inherited TableMaxID(Table)
  else
  begin
    SQL := 'select rowid from ' + Table.SqlTableName +
           ' order by rowid desc limit 1';
    if not InternalExecute(SQL, true, PInt64(@result)) then
      result := 0;
  end;
end;

function TRestOrmServerDB.MainEngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  Props: TOrmProperties;
  SQL: RawUtf8;
  Decoder: TJsonObjectDecoder;
begin
  result := 0;
  if TableModelIndex < 0 then
    exit;
  Props := fModel.TableProps[TableModelIndex].Props;
  SQL := Props.SqlTableName;
  if fBatchMethod <> mNone then
  begin
    result := 0; // indicates error
    if SentData = '' then
      InternalLog('BATCH with MainEngineAdd(%,SentData="") -> ' +
        'DEFAULT VALUES not implemented', [SQL], sllError)
    else if (fBatchMethod = mPOST) and
            (fBatchIDMax >= 0) and
            ((fBatchTableIndex < 0) or
             (fBatchTableIndex = TableModelIndex)) then
    begin
      fBatchTableIndex := TableModelIndex;
      if JsonGetID(pointer(SentData), result) then
      begin
        if result > fBatchIDMax then
          fBatchIDMax := result;
      end
      else
      begin
        if fBatchIDMax = 0 then
        begin
          fBatchIDMax := TableMaxID(Props.Table);
          if fBatchIDMax < 0 then
            // will force error for whole BATCH block
            exit;
        end;
        inc(fBatchIDMax);
        result := fBatchIDMax;
      end;
      AddID(fBatchID, fBatchIDCount, result);
      AddRawUtf8(fBatchValues, fBatchValuesCount, SentData);
    end;
    exit;
  end;
  SQL := 'INSERT INTO ' + SQL;
  if TrimU(SentData) = '' then
    SQL := SQL + ' DEFAULT VALUES;'
  else
  begin
    JsonGetID(pointer(SentData), result);
    Decoder.Decode(SentData, nil, pInlined, result, false);
    if (fOwner <> nil) and
       (Props.RecordVersionField <> nil) then
      fOwner.RecordVersionHandle(ooInsert, TableModelIndex, Decoder,
        Props.RecordVersionField);
    SQL := SQL + Decoder.EncodeAsSql(false) + ';';
  end;
  if InternalExecute(SQL, true, nil, nil, nil, PInt64(@result)) then
    InternalUpdateEvent(oeAdd, TableModelIndex, result, SentData, nil);
end;

procedure InternalRTreeIn(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
var
  aRTree: TOrmRTreeClass;
  BlobA, BlobB: pointer;
begin
  if argc <> 2 then
  begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  aRTree := sqlite3.user_data(Context);
  BlobA := sqlite3.value_blob(argv[0]);
  BlobB := sqlite3.value_blob(argv[1]);
  if (aRTree = nil) or
     (BlobA = nil) or
     (BlobB = nil) then
    sqlite3.result_error(Context, 'invalid call')
  else
    sqlite3.result_int64(Context, byte(aRTree.ContainedIn(BlobA^, BlobB^)));
end;

procedure TRestOrmServerDB.InitializeEngine;
var
  i: PtrInt;
  module: TOrmVirtualTableClass;
  registered: array of TOrmVirtualTableClass;
begin
  for i := 0 to high(model.TableProps) do
    case model.TableProps[i].Kind of
      ovkRTree, ovkRTreeInteger:
        // register all RTREE associated *_in() SQL functions
        sqlite3_check(DB.DB, sqlite3.create_function_v2(
          DB.DB, pointer(TOrmRTreeClass(model.Tables[i]).RTreeSQLFunctionName),
          2, SQLITE_ANY, model.Tables[i], InternalRTreeIn, nil, nil, nil));
      ovkCustomForcedID, ovkCustomAutoID:
        begin
          // register once each TOrmVirtualTableModuleServerDB
          module := pointer(fModel.VirtualTableModule(fModel.Tables[i]));
          if (module <> nil) and
             (PtrArrayFind(registered, module) < 0) then
          begin
            TOrmVirtualTableModuleServerDB.Create(module, self);
            PtrArrayAdd(registered, module); // register it once for this DB
          end;
        end;
    end;
end;

procedure TRestOrmServerDB.CreateMissingTables(user_version: cardinal;
  Options: TOrmInitializeTableOptions);
var
  t, f, nt, nf: PtrInt;
  TableNamesAtCreation, aFields: TRawUtf8DynArray;
  TableJustCreated: TOrmFieldTables;
  aSql: RawUtf8;
begin
  if DB.TransactionActive then
    raise ERestStorage.Create('CreateMissingTables in transaction');
  fDB.GetTableNames(TableNamesAtCreation);
  nt := length(TableNamesAtCreation);
  QuickSortRawUtf8(TableNamesAtCreation, nt, nil, @StrIComp);
  fDB.Log.Add.Log(sllDB, 'CreateMissingTables on %', [fDB], self);
  fDB.Log.Add.Log(sllDB, 'GetTables', TypeInfo(TRawUtf8DynArray),
    TableNamesAtCreation, self);
  FillcharFast(TableJustCreated, sizeof(TOrmFieldTables), 0);
  try
    // create not static and not existing tables
    for t := 0 to high(model.Tables) do
      if (fStaticData = nil) or
         (fStaticData[t] = nil) then
        // this table is not static -> check if already existing, create if necessary
        with model.TableProps[t], Props do
          if not NoCreateMissingTable then
            if FastFindPUtf8CharSorted(
              pointer(TableNamesAtCreation), nt - 1,
              pointer(SqlTableName), @StrIComp) < 0 then
            begin
              if not DB.TransactionActive then
                // make initialization faster by using transaction
                DB.TransactionBegin;
              // note: don't catch Execute() exception in constructor
              DB.Execute(model.GetSqlCreate(t));
              include(TableJustCreated, t); // mark to be initialized below
            end
            else if not (itoNoCreateMissingField in Options) then
            begin
              // this table is existing: check that all fields exist -> create if necessary
              DB.GetFieldNames(aFields, SqlTableName);
              nf := length(aFields);
              QuickSortRawUtf8(aFields, nf, nil, @StrIComp);
              for f := 0 to Fields.Count - 1 do
                with Fields.List[f] do
                  if OrmFieldType in COPIABLE_FIELDS then
                    /// real database columns exist for Simple + Blob fields (not Many)
                    if FastFindPUtf8CharSorted(pointer(aFields), nf - 1,
                      pointer(Name), @StrIComp) < 0 then
                    begin
                      aSql := model.GetSqlAddField(t, f);
                      if aSql <> '' then
                      begin
                        // need a true field with data
                        if not DB.TransactionActive then
                          // make initialization faster by using transaction
                          DB.TransactionBegin;
                        DB.Execute(aSql);
                      end;
                      model.Tables[t].InitializeTable(self, Name, Options);
                    end;
            end;
    if not DB.TransactionActive then
      exit;
    // database schema was modified -> update user version in SQLite3 file
    if user_version <> 0 then
      DB.user_version := user_version;
    // initialize new tables AFTER creation of ALL tables
    if not IsZero(@TableJustCreated, sizeof(TOrmFieldTables)) then
      for t := 0 to high(model.Tables) do
        if t in TableJustCreated then
          if not (model.TableProps[t].Kind in IS_CUSTOM_VIRTUAL) or
             not TableHasRows(model.Tables[t]) then
            // check is really void
            model.Tables[t].InitializeTable(self, '', Options);
            // FieldName='' for table creation
    DB.Commit;
  except
    on E: Exception do
    begin
      DB.RollBack; // will close any active Transaction
      raise;      // caller must handle exception
    end;
  end;
end;

function TRestOrmServerDB.MainEngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  if (TableModelIndex < 0) or
     (ID <= 0) then
    result := false
  else
  begin
    // notify BEFORE deletion
    InternalUpdateEvent(oeDelete, TableModelIndex, ID, '', nil);
    result := ExecuteFmt('DELETE FROM % WHERE RowID=:(%):;',
      [fModel.TableProps[TableModelIndex].Props.SqlTableName, ID]);
  end;
end;

function TRestOrmServerDB.MainEngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  i: PtrInt;
  aSqlWhere: RawUtf8;
begin
  if (TableModelIndex < 0) or
     (IDs = nil) then
    result := false
  else
  begin
    // notify BEFORE deletion
    for i := 0 to high(IDs) do
      InternalUpdateEvent(oeDelete, TableModelIndex, IDs[i], '', nil);
    if IdemPChar(pointer(SqlWhere), 'LIMIT ') or
       IdemPChar(pointer(SqlWhere), 'ORDER BY ') then
      // LIMIT is not handled by SQLite3 when built from amalgamation
      // see http://www.sqlite.org/compile.html#enable_update_delete_limit
      aSqlWhere := Int64DynArrayToCsv(pointer(IDs), length(IDs), 'RowID IN (', ')')
    else
      aSqlWhere := SqlWhere;
    result := ExecuteFmt('DELETE FROM %%',
      [fModel.TableProps[TableModelIndex].Props.SqlTableName,
       SqlFromWhere(aSqlWhere)]);
  end;
end;

constructor TRestOrmServerDB.Create(aRest: TRest);
begin
  if fDB = nil then
    // if not set by overloaded TRestOrmServerDB.Create(aRest, aModel, aDB)
    fDB := TSqlDataBase.Create(SQLITE_MEMORY_DATABASE_NAME);
  fStatementCache.Init(fDB.DB);
  fDB.UseCache := true; // we better use caching in this JSON oriented use
  if fDB.InternalState = nil then
  begin
    // should be done once
    InternalState := 1;
    fDB.InternalState := @InternalState; // to update our own InternalState
  end;
  inherited Create(aRest);
  InitializeEngine;
end;

constructor TRestOrmServerDB.Create(aRest: TRest;
  aDB: TSqlDataBase; aOwnDB: boolean);
begin
  fDB := aDB; // should be done before CreateWithoutRest/Create
  if aOwnDB then
    fOwnedDB := fDB;
  Create(aRest);
end;

constructor TRestOrmServerDB.CreateStandalone(aModel: TOrmModel; aRest: TRest;
  aDB: TSqlDataBase; aOwnDB: boolean);
begin
  fModel := aModel;
  fModel.Owner := self; // TRestOrmServerDB.Destroy will free its TOrmModel
  fRest := aRest;
  fOwner := aRest as TRestServer;
  Create(nil, aDB, aOwnDB);
end;

destructor TRestOrmServerDB.Destroy;
var
  log: ISynLog;
begin
  log := fDB.Log.Enter('Destroy %', [fModel.SafeRoot], self);
  try
    if (fDB <> nil) and
       (fDB.InternalState = @InternalState) then
      // avoid memory modification on free block
      fDB.InternalState := nil;
    inherited Destroy;
  finally
    try
      fStatementCache.ReleaseAllDBStatements;
    finally
      fOwnedDB.Free; // do nothing if DB<>fOwnedDB
    end;
  end;
end;

function TRestOrmServerDB.PrepareVacuum(const aSql: RawUtf8): boolean;
begin
  result := not IdemPChar(Pointer(aSql), 'VACUUM');
  if result then
    exit;
  result := (fStaticVirtualTable = nil) or
    IsZero(fStaticVirtualTable, length(fStaticVirtualTable) * sizeof(pointer));
  if result then
    // VACUUM will fail if there are one or more active SQL statements
    fStatementCache.ReleaseAllDBStatements;
end;

function TRestOrmServerDB.InternalExecute(const aSql: RawUtf8;
  ForceCacheStatement: boolean; ValueInt: PInt64; ValueUTF8: PRawUtf8;
  ValueInts: PInt64DynArray; LastInsertedID: PInt64;
  LastChangeCount: PInteger): boolean;
var
  ValueIntsCount, Res: integer;
  msg: shortstring;
begin
  msg := '';
  if (self <> nil) and
     (DB <> nil) then
  try
    DB.Lock(aSql);
    try
      result := true;
      if not PrepareVacuum(aSql) then
        // no-op if there are some static virtual tables around
        exit;
      try
        GetAndPrepareStatement(aSql, ForceCacheStatement);
        if ValueInts <> nil then
        begin
          ValueIntsCount := 0;
          repeat
            Res := fStatement^.Step;
            if Res = SQLITE_ROW then
              AddInt64(ValueInts^, ValueIntsCount, fStatement^.FieldInt(0));
          until Res = SQLITE_DONE;
          SetLength(ValueInts^, ValueIntsCount);
          FormatShort('returned Int64 len=%', [ValueIntsCount], msg);
        end
        else if (ValueInt = nil) and
                (ValueUTF8 = nil) then
        begin
          // default execution: loop through all rows
          repeat
          until fStatement^.Step <> SQLITE_ROW;
          if LastInsertedID <> nil then
          begin
            LastInsertedID^ := DB.LastInsertRowID;
            FormatShort(' lastInsertedID=%', [LastInsertedID^], msg);
          end;
          if LastChangeCount <> nil then
          begin
            LastChangeCount^ := DB.LastChangeCount;
            FormatShort(' lastChangeCount=%', [LastChangeCount^], msg);
          end;
        end
        else
        // get one row, and retrieve value
        if fStatement^.Step <> SQLITE_ROW then
          result := false
        else if ValueInt <> nil then
        begin
          ValueInt^ := fStatement^.FieldInt(0);
          FormatShort('returned=%', [ValueInt^], msg);
        end
        else
        begin
          ValueUTF8^ := fStatement^.FieldUtf8(0);
          FormatShort('returned="%"', [ValueUTF8^], msg);
        end;
        GetAndPrepareStatementRelease(nil, msg);
      except
        on E: Exception do
        begin
          GetAndPrepareStatementRelease(E);
          result := false;
        end;
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESqlite3Exception do
    begin
      InternalLog('% for % // %', [E, aSql, fStatementGenericSql], sllError);
      result := false;
    end;
  end
  else
    result := false;
end;

function TRestOrmServerDB.StoredProcExecute(const aSql: RawUtf8;
  const StoredProc: TOnSqlStoredProc): boolean;
var
  R: TSqlRequest; // we don't use fStatementCache[] here
  Res: integer;
  log: ISynLog;
begin
  result := false;
  if (self <> nil) and
     (DB <> nil) and
     (aSql <> '') and
     Assigned(StoredProc) then
  try
    log := fDB.Log.Enter('StoredProcExecute(%)', [aSql], self);
    DB.LockAndFlushCache; // even if aSql is SELECT, StoredProc may update data
    try
      try
        R.Prepare(DB.DB, aSql);
        if R.FieldCount > 0 then
          repeat
            Res := R.Step;
            if Res = SQLITE_ROW then
              StoredProc(R); // apply the stored procedure to all rows
          until Res = SQLITE_DONE;
        result := true;
      finally
        R.Close; // always release statement
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESqlite3Exception do
    begin
      fDB.Log.Add.Log(sllError, '% for %', [E, aSql], self);
      result := false;
    end;
  end;
end;

function TRestOrmServerDB.EngineExecute(const aSql: RawUtf8): boolean;
begin
  result := InternalExecute(aSql, {forcecache=}false);
end;

procedure TRestOrmServerDB.ComputeDBStats(out result: variant);
var
  i: PtrInt;
  ndx: TIntegerDynArray;
  doc: TDocVariantData absolute result;
begin
  if self = nil then
    exit;
  doc.Init(JSON_OPTIONS_FAST_EXTENDED, dvObject);
  DB.Lock;
  try
    fStatementCache.SortCacheByTotalTime(ndx);
    with fStatementCache do
      for i := 0 to Count - 1 do
        with Cache[ndx[i]] do
          doc.AddValue(StatementSql, timer.ComputeDetails);
  finally
    DB.UnLock;
  end;
end;

function TRestOrmServerDB.ComputeDBStats: variant;
begin
  ComputeDBStats(result);
end;

function TRestOrmServerDB.MainEngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
var
  MS: TRawByteStringStream;
  RowCount: integer;
begin
  result := '';
  RowCount := 0;
  if (self <> nil) and
     (DB <> nil) and
     (SQL <> '') then
  begin
    // need a SQL request for R.Execute() to prepare a statement
    result := DB.LockJson(SQL, ReturnedRowCount); // lock and try from cache
    if result <> '' then
      exit;
    try
      // Execute request if was not got from cache
      try
        GetAndPrepareStatement(SQL, {forcecache=}false);
        MS := TRawByteStringStream.Create;
        try
          RowCount := fStatement^.Execute(0, '', MS,
            ForceAjax or not fOwner.NoAjaxJson);
          result := MS.DataString;
        finally
          MS.Free;
        end;
        GetAndPrepareStatementRelease(nil, 'returned % as %',
          [Plural('row', RowCount), KB(result)]);
      except
        on E: ESqlite3Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLockJson(result, RowCount);
    end;
  end;
  if ReturnedRowCount <> nil then
    ReturnedRowCount^ := RowCount;
end;

function TRestOrmServerDB.MainEngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  aSql: RawUtf8;
begin
  result := '';
  if (ID < 0) or
     (TableModelIndex < 0) then
    exit;
  with model.TableProps[TableModelIndex] do
    FormatUtf8('SELECT % FROM % WHERE RowID=:(%):;',
      [sql.TableSimpleFields[true, false], Props.SqlTableName, ID], aSql);
  result := EngineList(aSql, {ForceAjax=}true); // ForceAjax -> '[{...}]'#10
  if result <> '' then
    if IsNotAjaxJson(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := ''
    else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result, 2, length(result) - 3);
end;

function TRestOrmServerDB.MainEngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  SQL: RawUtf8;
begin
  result := false;
  if (aID < 0) or
     (TableModelIndex < 0) or
     not BlobField^.IsRawBlob then
    exit;
  // retrieve the BLOB using SQL
  try
    SQL := FormatUtf8('SELECT % FROM % WHERE RowID=?',
      [BlobField^.NameUtf8, model.TableProps[TableModelIndex].Props.SqlTableName],
      [aID]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      GetAndPrepareStatement(SQL, true);
      try
        if (fStatement^.FieldCount = 1) and
           (fStatement^.Step = SQLITE_ROW) then
        begin
          BlobData := fStatement^.FieldBlob(0);
          result := true;
        end;
        GetAndPrepareStatementRelease(nil, KB(BlobData));
      except
        on E: Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLock;
    end;
  except
    on ESqlite3Exception do
      result := false;
  end;
end;

function TRestOrmServerDB.RetrieveBlobFields(Value: TOrm): boolean;
var
  s: TRestOrm;
  SQL: RawUtf8;
  f: PtrInt;
  size: Int64;
  data: TSqlVar;
begin
  result := false;
  if Value = nil then
    exit;
  s := GetStaticTable(POrmClass(Value)^);
  if s <> nil then
    result := s.RetrieveBlobFields(Value)
  else if (DB <> nil) and
          (Value.ID > 0) and
          (POrmClass(Value)^ <> nil) then
    with Value.Orm do
      if BlobFields <> nil then
      begin
        SQL := FormatUtf8('SELECT % FROM % WHERE ROWID=?',
          [SqlTableRetrieveBlobFields, SqlTableName], [Value.ID]);
        DB.Lock(SQL);
        try
          GetAndPrepareStatement(SQL, true);
          try
            if fStatement^.Step <> SQLITE_ROW then
              exit;
            size := 0;
            for f := 0 to high(BlobFields) do
            begin
              SQlite3ValueToSqlVar(fStatement^.FieldValue(f), data);
              BlobFields[f].SetFieldSqlVar(Value, data); // OK for all blobs
              inc(size, SqlVarLength(data));
            end;
            GetAndPrepareStatementRelease(nil, KB(size));
            result := true;
          except
            on E: Exception do
              GetAndPrepareStatementRelease(E);
          end;
        finally
          DB.UnLock;
        end;
      end;
end;

procedure TRestOrmServerDB.SetNoAjaxJson(const Value: boolean);
begin
  inherited;
  if Value = fOwner.NoAjaxJson then
    exit;
  fDB.Cache.Reset; // we changed the JSON format -> cache must be updated
end;

function TRestOrmServerDB.MainEngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUtf8): boolean;
var
  Props: TOrmProperties;
  Decoder: TJsonObjectDecoder;
  SQL: RawUtf8;
begin
  if (TableModelIndex < 0) or
     (ID <= 0) then
    result := false
  else if SentData = '' then
    // update with no simple field -> valid no-op
    result := true
  else
  begin
    // this SQL statement use :(inlined params): for all values
    Props := fModel.TableProps[TableModelIndex].Props;
    Decoder.Decode(SentData, nil, pInlined, ID, false);
    if Props.RecordVersionField <> nil then
      fOwner.RecordVersionHandle(ooUpdate, TableModelIndex,
        Decoder, Props.RecordVersionField);
    SQL := Decoder.EncodeAsSql(true);
    result := ExecuteFmt('UPDATE % SET % WHERE RowID=:(%):',
      [Props.SqlTableName, SQL, ID]);
    InternalUpdateEvent(oeUpdate, TableModelIndex, ID, SentData, nil);
  end;
end;

function TRestOrmServerDB.MainEngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  SQL: RawUtf8;
  AffectedField: TFieldBits;
  Props: TOrmProperties;
begin
  result := false;
  if (aID < 0) or
     (TableModelIndex < 0) or
     not BlobField^.IsRawBlob then
    exit;
  Props := model.TableProps[TableModelIndex].Props;
  try
    FormatUtf8('UPDATE % SET %=? WHERE RowID=?',
      [Props.SqlTableName, BlobField^.NameUtf8], SQL);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      GetAndPrepareStatement(SQL, true);
      try
        if BlobData = '' then
          fStatement^.BindNull(1)
        else
          fStatement^.BindBlob(1, BlobData);
        fStatement^.Bind(2, aID);
        repeat
        until fStatement^.Step <> SQLITE_ROW; // Execute
        GetAndPrepareStatementRelease(nil, 'stored % in ID=%',
          [KB(BlobData), aID], true);
        result := true;
      except
        on E: Exception do
          GetAndPrepareStatementRelease(E);
      end;
    finally
      DB.UnLock;
    end;
    Props.FieldBitsFromBlobField(BlobField, AffectedField);
    InternalUpdateEvent(oeUpdateBlob, TableModelIndex, aID, '', @AffectedField);
  except
    on ESqlite3Exception do
      result := false;
  end;
end;

function TRestOrmServerDB.MainEngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUtf8;
  Increment: Int64): boolean;
var
  Props: TOrmProperties;
  Value: Int64;
begin
  result := false;
  if (TableModelIndex < 0) or
     (FieldName = '') then
    exit;
  Props := model.TableProps[TableModelIndex].Props;
  if Props.Fields.IndexByName(FieldName) < 0 then
    Exit;
  if InternalUpdateEventNeeded(TableModelIndex) or
     (Props.RecordVersionField <> nil) then
    result := OneFieldValue(Props.Table, FieldName, 'ID=?', [], [ID], Value) and
      UpdateField(Props.Table, ID, FieldName, [Value + Increment])
  else
    result := RecordCanBeUpdated(Props.Table, ID, oeUpdate) and
      ExecuteFmt('UPDATE % SET %=%+:(%): WHERE ID=:(%):',
       [Props.SqlTableName, FieldName, FieldName, Increment, ID]);
end;

function TRestOrmServerDB.MainEngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  Props: TOrmProperties;
  WhereID, RecordVersion: TID;
  i: PtrInt;
  JSON, IDs: RawUtf8;
  ID: TIDDynArray;
begin
  result := false;
  if (TableModelIndex < 0) or
     (SetFieldName = '') then
    exit;
  Props := model.TableProps[TableModelIndex].Props;
  if Props.Fields.IndexByName(SetFieldName) < 0 then
    exit;
  if IsRowID(pointer(WhereFieldName)) then
  begin
    WhereID := GetInt64(Pointer(WhereValue));
    if WhereID <= 0 then
      exit;
  end
  else if Props.Fields.IndexByName(WhereFieldName) < 0 then
    exit
  else
    WhereID := 0;
  if InternalUpdateEventNeeded(TableModelIndex) or
     (Props.RecordVersionField <> nil) then
  begin
    if WhereID > 0 then
    begin
      SetLength(ID, 1);
      ID[0] := WhereID;
    end
    else if not InternalExecute(FormatUtf8('select RowID from % where %=:(%):',
       [Props.SqlTableName, WhereFieldName, WhereValue]), true, nil, nil,
       @ID) then
      exit
    else if ID = nil then
    begin
      result := true; // nothing to update, but return success
      exit;
    end;
    for i := 0 to high(ID) do
      if not RecordCanBeUpdated(Props.Table, ID[i], oeUpdate) then
        exit;
    if Length(ID) = 1 then
      if Props.RecordVersionField = nil then
        result := ExecuteFmt('UPDATE % SET %=:(%): WHERE RowID=:(%):',
          [Props.SqlTableName, SetFieldName, SetValue, ID[0]])
      else
        result := ExecuteFmt('UPDATE % SET %=:(%):,%=:(%): WHERE RowID=:(%):',
          [Props.SqlTableName, SetFieldName, SetValue,
          Props.RecordVersionField.Name, RecordVersionCompute, ID[0]])
    else
    begin
      IDs := Int64DynArrayToCsv(pointer(ID), length(ID));
      if Props.RecordVersionField = nil then
        result := ExecuteFmt('UPDATE % SET %=% WHERE RowID IN (%)',
          [Props.SqlTableName, SetFieldName, SetValue, IDs])
      else
      begin
        RecordVersion := RecordVersionCompute;
        result := ExecuteFmt('UPDATE % SET %=%,%=% WHERE RowID IN (%)',
          [Props.SqlTableName, SetFieldName, SetValue,
           Props.RecordVersionField.Name, RecordVersion, IDs]);
      end;
    end;
    if not result then
      exit;
    JsonEncodeNameSQLValue(SetFieldName, SetValue, JSON);
    for i := 0 to high(ID) do
      InternalUpdateEvent(oeUpdate, TableModelIndex, ID[i], JSON, nil);
  end
  else if (WhereID > 0) and
          not RecordCanBeUpdated(Props.Table, WhereID, oeUpdate) then
    exit
  else // limitation: will only check for update when RowID is provided
    result := ExecuteFmt('UPDATE % SET %=:(%): WHERE %=:(%):',
      [Props.SqlTableName, SetFieldName, SetValue, WhereFieldName, WhereValue]);
end;

function TRestOrmServerDB.UpdateBlobFields(Value: TOrm): boolean;
var
  s: TRestOrm;
  SQL: RawUtf8;
  TableModelIndex, f: PtrInt;
  data: TSqlVar;
  size: Int64;
  temp: RawByteString;
begin
  result := false;
  if Value = nil then
    exit;
  TableModelIndex := model.GetTableIndexExisting(POrmClass(Value)^);
  s := GetStaticTableIndex(TableModelIndex);
  if s <> nil then
    result := s.UpdateBlobFields(Value)
  else if (DB <> nil) and
          (Value.ID > 0) and
          (POrmClass(Value)^ <> nil) then
    with model.TableProps[TableModelIndex].Props do
      if BlobFields <> nil then
      begin
        FormatUtf8('UPDATE % SET % WHERE ROWID=?',
          [SqlTableName, SqlTableUpdateBlobFields], SQL);
        DB.Lock(SQL); // UPDATE for all blob fields -> no cache flush, but UI refresh
        try
          GetAndPrepareStatement(SQL, true);
          try
            size := 0;
            for f := 1 to length(BlobFields) do
            begin
              // GetFieldSqlVar() works well to get blobs into a RawByteString
              BlobFields[f - 1].GetFieldSqlVar(Value, data, temp);
              if data.VType = ftBlob then
              begin
                fStatement^.Bind(f, data.VBlob, data.VBlobLen);
                inc(size, data.VBlobLen);
              end
              else
                fStatement^.BindNull(f); // e.g. Value was ''
            end;
            fStatement^.Bind(length(BlobFields) + 1, Value.ID);
            repeat
            until fStatement^.Step <> SQLITE_ROW; // Execute
            GetAndPrepareStatementRelease(nil, 'stored % in ID=%',
              [KB(size), Value.ID], true);
            result := true;
          except
            on E: Exception do
              GetAndPrepareStatementRelease(E);
          end;
        finally
          DB.UnLock;
        end;
        InternalUpdateEvent(oeUpdateBlob, TableModelIndex, Value.ID, '',
          @FieldBits[oftBlob]);
      end
      else
        result := true; // as TRestOrm.UpdateblobFields()
end;

procedure TRestOrmServerDB.Commit(SessionID: cardinal;
  RaiseException: boolean);
begin
  inherited Commit(SessionID, RaiseException);
  // reset fTransactionActive + write all TOrmVirtualTableJson
  try
    DB.Commit; // will call DB.Lock
  except
    on Exception do
      if RaiseException then
        raise;  // default RaiseException=false will just ignore the exception
  end;
end;

procedure TRestOrmServerDB.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset TRestOrmServerDB.fTransactionActive flag
  try
    DB.RollBack; // will call DB.Lock
  except
    on ESqlite3Exception do
      ; // just catch exception
  end;
end;

function TRestOrmServerDB.TransactionBegin(aTable: TOrmClass;
  SessionID: cardinal): boolean;
begin
  result := not DB.TransactionActive and
            inherited TransactionBegin(aTable, SessionID);
  if not result then
    // fTransactionActive flag was already set
    exit;
  try
    DB.TransactionBegin; // will call DB.Lock
  except
    on ESqlite3Exception do
      result := false;
  end;
end;

procedure TRestOrmServerDB.FlushInternalDBCache;
begin
  inherited;
  if DB = nil then
    exit;
  DB.Lock;
  try
    DB.CacheFlush;
  finally
    DB.UnLock;
  end;
end;

function TRestOrmServerDB.InternalBatchStart(Method: TUriMethod;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false; // means BATCH mode not supported
  if Method = mPOST then
  begin // POST=ADD=INSERT -> MainEngineAdd() to fBatchValues[]
    if (fBatchMethod <> mNone) or
       (fBatchValuesCount <> 0) or
       (fBatchIDCount <> 0) then
      raise EOrmBatchException.CreateUtf8(
        '%.InternalBatchStop should have been called', [self]);
    fBatchMethod := Method;
    fBatchOptions := BatchOptions;
    fBatchTableIndex := -1;
    fBatchIDMax := 0; // MainEngineAdd() will search for max(id)
    result := true; // means BATCH mode is supported
  end;
end;

procedure TRestOrmServerDB.InternalBatchStop;
const
  MAX_PARAMS = 500; // pragmatic value (theoritical limit is 999)
var
  ndx, r, prop, fieldCount, valuesCount, rowCount, valuesFirstRow: integer;
  f: PtrInt;
  P: PUtf8Char;
  DecodeSaved, UpdateEventNeeded: boolean;
  Fields, Values: TRawUtf8DynArray;
  ValuesNull: TByteDynArray;
  Types: TSqlDBFieldTypeDynArray;
  SQL: RawUtf8;
  Props: TOrmProperties;
  Decode: TJsonObjectDecoder;
  tmp: TSynTempBuffer;
begin
  if (fBatchValuesCount = 0) or
     (fBatchTableIndex < 0) then
    exit; // nothing to add
  if fBatchMethod <> mPOST then
    raise EOrmBatchException.CreateUtf8('%.InternalBatchStop: BatchMethod=%',
      [self, ToText(fBatchMethod)^]);
  try
    if fBatchValuesCount <> fBatchIDCount then
      raise EOrmBatchException.CreateUtf8(
        '%.InternalBatchStop(*Count?)', [self]);
    UpdateEventNeeded := InternalUpdateEventNeeded(fBatchTableIndex);
    Props := fModel.Tables[fBatchTableIndex].OrmProps;
    if fBatchValuesCount = 1 then
    begin
      // handle single record insert
      Decode.Decode(fBatchValues[0], nil, pInlined, fBatchID[0]);
      if Props.RecordVersionField <> nil then
        fOwner.RecordVersionHandle(
          ooInsert, fBatchTableIndex, Decode, Props.RecordVersionField);
      SQL := 'INSERT INTO ' + Props.SqlTableName + Decode.EncodeAsSql(False) + ';';
      if not InternalExecute(SQL, true) then
        // just like ESqlite3Exception below
        raise EOrmBatchException.CreateUtf8(
          '%.InternalBatchStop failed on %', [self, SQL]);
      if UpdateEventNeeded then
        InternalUpdateEvent(oeAdd, fBatchTableIndex,
          fBatchID[0], fBatchValues[0], nil);
      exit;
    end;
    DecodeSaved := true;
    valuesCount := 0;
    rowCount := 0;
    valuesFirstRow := 0;
    SetLength(ValuesNull, (MAX_PARAMS shr 3) + 1);
    SetLength(Values, 32);
    Fields := nil; // makes compiler happy
    fieldCount := 0;
    ndx := 0;
    repeat
      repeat
        // decode a row
        if DecodeSaved then
        try
          if UpdateEventNeeded then
          begin
            tmp.Init(fBatchValues[ndx]);
            P := tmp.buf;
          end
          else
            P := pointer(fBatchValues[ndx]);
          if P = nil then
            raise EOrmBatchException.CreateUtf8(
              '%.InternalBatchStop: fBatchValues[%]=""', [self, ndx]);
          while P^ in [#1..' ', '{', '['] do
            inc(P);
          Decode.Decode(P, nil, pNonQuoted, fBatchID[ndx]);
          if Props.RecordVersionField <> nil then
            fOwner.RecordVersionHandle(ooInsert, fBatchTableIndex,
              Decode, Props.RecordVersionField);
          inc(ndx);
          DecodeSaved := false;
        finally
          if UpdateEventNeeded then
            tmp.Done;
        end;
        if Fields = nil then
        begin
          Decode.AssignFieldNamesTo(Fields);
          fieldCount := Decode.FieldCount;
          SQL := Decode.EncodeAsSqlPrepared(Props.SqlTableName, ooInsert, '',
            fBatchOptions);
          SetLength(Types, fieldCount);
          for f := 0 to fieldCount - 1 do
          begin
            prop := Props.Fields.IndexByNameOrExcept(Decode.FieldNames[f]);
            if prop < 0 then
              // RowID
              Types[f] := ftInt64
            else
              Types[f] := Props.Fields.List[prop].SqlDBFieldType;
          end;
        end
        else if not Decode.SameFieldNames(Fields) then
          // this item would break the SQL statement
          break
        else
          if valuesCount + fieldCount > MAX_PARAMS then
            // this item would bound too many params
            break;
        // if we reached here, we can add this row to Values[]
        if valuesCount + fieldCount > length(Values) then
          SetLength(Values, MAX_PARAMS);
        for f := 0 to fieldCount - 1 do
          if Decode.FieldTypeApproximation[f] = ftaNull then
            SetBitPtr(pointer(ValuesNull), valuesCount + f)
          else
            Values[valuesCount + f] := Decode.FieldValues[f];
        inc(valuesCount, fieldCount);
        inc(rowCount);
        DecodeSaved := true;
      until ndx = fBatchValuesCount;
      // INSERT Values[] into the DB
      DB.LockAndFlushCache;
      try
        try
          FormatUtf8('% multi %', [rowCount, SQL], fStatementSql);
          if rowCount > 1 then
            SQL := SQL + ',' +
              CsvOfValue('(' + CsvOfValue('?', fieldCount) + ')', rowCount - 1);
          fStatementGenericSql := SQL; // full log on error
          PrepareStatement((rowCount < 5) or
                           (valuesCount + fieldCount > MAX_PARAMS));
          prop := 0;
          for f := 0 to valuesCount - 1 do
          begin
            if GetBitPtr(pointer(ValuesNull), f) then
              fStatement^.BindNull(f + 1)
            else
              case Types[prop] of
                ftInt64:
                  fStatement^.Bind(f + 1, GetInt64(pointer(Values[f])));
                ftDouble, ftCurrency:
                  fStatement^.Bind(f + 1, GetExtended(pointer(Values[f])));
                ftDate, ftUtf8:
                  fStatement^.Bind(f + 1, Values[f]);
                ftBlob:
                  fStatement^.BindBlob(f + 1, Values[f]);
              end;
            inc(prop);
            if prop = fieldCount then
              prop := 0;
          end;
          repeat
          until fStatement^.Step <> SQLITE_ROW; // ESqlite3Exception on error
          if UpdateEventNeeded then
            for r := valuesFirstRow to valuesFirstRow + rowCount - 1 do
              InternalUpdateEvent(oeAdd, fBatchTableIndex,
                fBatchID[r], fBatchValues[r], nil);
          inc(valuesFirstRow, rowCount);
          GetAndPrepareStatementRelease;
        except
          on E: Exception do
          begin
            GetAndPrepareStatementRelease(E);
            raise;
          end;
        end;
      finally
        DB.UnLock;
      end;
      FillcharFast(ValuesNull[0], (valuesCount shr 3) + 1, 0);
      valuesCount := 0;
      rowCount := 0;
      Fields := nil; // force new SQL statement and Values[]
    until DecodeSaved and
          (ndx = fBatchValuesCount);
    if valuesFirstRow <> fBatchValuesCount then
      raise EOrmBatchException.CreateUtf8(
        '%.InternalBatchStop(valuesFirstRow)', [self]);
  finally
    fBatchMethod := mNone;
    fBatchValuesCount := 0;
    fBatchValues := nil;
    fBatchIDCount := 0;
    fBatchID := nil;
  end;
end;



{ *********** TRestOrmClientDB REST Client ORM Engine over SQLite3 }

{ TRestOrmClientDB }

function TRestOrmClientDB.GetDB: TSqlDataBase;
begin
  if (self = nil) or
     (fServer = nil) then
    result := nil
  else
    result := fServer.fDB;
end;

constructor TRestOrmClientDB.Create(aRest: TRest; aServer: TRestOrmServerDB);
begin
  fServer := aServer;
  inherited Create(aRest);
end;

function TRestOrmClientDB.List(const Tables: array of TOrmClass;
  const SqlSelect: RawUtf8; const SqlWhere: RawUtf8): TOrmTable;
var
  aSql: RawUtf8;
  n: integer;
begin
  result := nil;
  n := length(Tables);
  if (self <> nil) and
     (n > 0) then
  try
    // direct SQL execution, using the JSON cache if available
    aSql := fModel.SqlFromSelectWhere(Tables, SqlSelect, SqlWhere);
    if n = 1 then
      // InternalListJson will handle both static and DB tables
      result := fServer.ExecuteList(Tables, aSql)
    else
      // we access localy the DB -> TOrmTableDB handle Tables parameter
      result := TOrmTableDB.Create(fServer.DB, Tables, aSql,
        not fServer.Owner.NoAjaxJson);
    if fServer.DB.InternalState <> nil then
      result.InternalState := fServer.DB.InternalState^;
  except
    on ESqlite3Exception do
      result := nil;
  end;
end;


end.

