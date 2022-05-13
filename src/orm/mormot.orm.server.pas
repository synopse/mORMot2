/// ORM Types and Classes for the Server side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.server;

{
  *****************************************************************************

   Server-Side Object-Relational-Mapping (ORM) Process
    - TRestOrmServer Abstract Server

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
  mormot.crypt.core,
  mormot.crypt.jwt,
  mormot.core.perf,
  mormot.core.search,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.soa.core,
  mormot.soa.server,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server;


{ ************ TRestOrmServer Abstract Server }

type
  /// implements TRestServer.ORM process for REST server with abstract storage
  // - works in conjunction with TRestClientUri from mormot.rest.client.pas
  // - you should inherit it to provide its main storage capabilities
  // - is able to register and redirect some TOrm classes to their own
  // dedicated TRestStorage
  TRestOrmServer = class(TRestOrm, IRestOrmServer)
  protected
    fOwner: TRestServer;
    /// will contain the in-memory representation of some static tables
    // - this array has the same length as the associated Model.Tables[]
    // - fStaticData[] will contain pure in-memory tables, not declared as
    // SQLite3 virtual tables, therefore not available from joined SQL statements
    fStaticData: TRestOrmDynArray;
    /// map TRestStorageInMemory or TRestStorageExternal engines
    // - this array has the same length as the associated Model.Tables[]
    // - fStaticVirtualTable[] will contain in-memory or external tables declared
    // as SQLite3 virtual tables, therefore available from joined SQL statements
    // - the very same TRestStorage is handled in fStaticData
    fStaticVirtualTable: TRestOrmDynArray;
    fVirtualTableDirect: boolean;
    fCreateMissingTablesOptions: TOrmInitializeTableOptions;
    fRecordVersionMax: TRecordVersion;
    fRecordVersionDeleteIgnore: boolean;
    fOrmVersionDeleteTable: TOrmTableDeletedClass;
    // TOrmHistory.ModifiedRecord handles up to 64 (=1 shl 6) tables
    fTrackChangesHistoryTableIndex: TIntegerDynArray;
    fTrackChangesHistoryTableIndexCount: integer;
    fTrackChangesHistory: array of record
      CurrentRow: integer;
      MaxSentDataJsonRow: integer;
      MaxRevisionJson: integer;
      MaxUncompressedBlobSize: integer;
    end;
    function GetStaticDataServer(aClass: TOrmClass): TRestOrm;
    function GetVirtualTable(aClass: TOrmClass): TRestOrm;
    function GetStaticTable(aClass: TOrmClass): TRestOrm;
      {$ifdef HASINLINE}inline;{$endif}
    function MaxUncompressedBlobSize(Table: TOrmClass): integer;
    /// will retrieve the monotonic value of a TRecordVersion field from the DB
    procedure InternalRecordVersionMaxFromExisting(RetrieveNext: PID); virtual;
    procedure InternalRecordVersionDelete(TableIndex: integer; ID: TID;
      Batch: TRestBatch); virtual;
    /// will compute the next monotonic value for a TRecordVersion field
    // - you may override this method to customize the returned Int64 value
    // (e.g. to support several synchronization nodes)
    function InternalRecordVersionComputeNext: TRecordVersion; virtual;
  public
    /// overridden methods which will perform CRUD operations
    // - will call any static TRestStorage, or call MainEngine*() virtual methods
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineRetrieve(TableModelIndex: integer;
      ID: TID): RawUtf8; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt=nil): RawUtf8; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer; const SetFieldName,
      SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    function EngineBatchSend(Table: TOrmClass; var Data: RawUtf8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; override;
  public
    /// virtual abstract methods which will perform CRUD operations on the main DB
    function MainEngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; virtual; abstract;
    function MainEngineRetrieve(TableModelIndex: integer;
      ID: TID): RawUtf8; virtual; abstract;
    function MainEngineList(const SQL: RawUtf8; ForceAjax: boolean;
      ReturnedRowCount: PPtrInt): RawUtf8; virtual; abstract;
    function MainEngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; virtual; abstract;
    function MainEngineDelete(TableModelIndex: integer;
      ID: TID): boolean; virtual; abstract;
    function MainEngineDeleteWhere(TableModelIndex: integer;
      const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean; virtual; abstract;
    function MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; virtual; abstract;
    function MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; virtual; abstract;
    function MainEngineUpdateField(TableModelIndex: integer; const SetFieldName,
        SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; virtual; abstract;
    function MainEngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; virtual; abstract;
    /// this method is overridden for setting the NoAjaxJson field
    // of all associated TRestStorage servers
    procedure SetNoAjaxJson(const Value: boolean); virtual;
  public
    /// this integer property is incremented by the database engine when any SQL
    // statement changes the database contents (i.e. on any not SELECT statement)
    // - its value can be published to the client on every remote request
    // - it may be used by client to avoid retrieve data only if necessary
    // - if its value is 0, this feature is not activated on the server, and the
    // client must ignore it and always retrieve the content
    InternalState: cardinal;
    /// a method can be specified here to trigger events after any table update
    // - is called BEFORE deletion, and AFTER insertion or update
    // - note that the aSentData parameter does not contain all record fields,
    // but only transmitted information: e.g. if only one field is updated, only
    // this single field (and the ID) is available
    // - to be used only server-side, not to synchronize some clients: the framework
    // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
    // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
    OnUpdateEvent: TOnOrmEvent;
    /// a method can be specified here to trigger events after any blob update
    // - is called AFTER update of one or several blobs, never on delete nor insert
    // - to be used only server-side, not to synchronize some clients: the framework
    // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
    // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
    OnBlobUpdateEvent: TOnOrmFieldEvent;

    /// initialize the class, and associated to a TRest and its TOrmModel
    constructor Create(aRest: TRest); override;
    /// release memory and any existing associated resource
    destructor Destroy; override;
    /// ensure the current thread will be taken into account during process
    // - this default implementation will call the BeginCurrentThread methods
    // of all its internal TRestStorage instances
    procedure BeginCurrentThread(Sender: TThread); override;
    /// called when thread is finished to ensure
    // - this default implementation will call the EndCurrentThread methods
    // of all its internal TRestStorage instances
    procedure EndCurrentThread(Sender: TThread); override;
    /// missing tables are created if they don't exist yet for every TOrm
    // class of the Database Model
    // - you must call explicitly this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method should also create additional fields, if the TOrm definition
    // has been modified; only field adding is mandatory, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TOrm type)
    // - this virtual method do nothing by default - overridden versions should
    // implement it as expected by the underlying storage engine (e.g. SQLite3
    // or TRestServerFullInMemory)
    // - you can tune some options transmitted to the TOrm.InitializeTable
    // virtual methods, e.g. to avoid the automatic create of indexes
    procedure CreateMissingTables(user_version: cardinal = 0;
      options: TOrmInitializeTableOptions = []); virtual;
    /// run the TOrm.InitializeTable methods for all void tables of the model
    // - can be used instead of CreateMissingTables e.g. for MongoDB storage
    // - you can specify the creation options, e.g. INITIALIZETABLE_NOINDEX
    procedure InitializeTables(Options: TOrmInitializeTableOptions);
    /// check on which storage instance a SQL SELECT statement is to be executed
    // - returns nil if the main engine is to be used
    // - or returns the target TRestStorage instance, with the adapted SQL
    // statement, ready to be run on it
    function InternalAdaptSql(TableIndex: integer; var SQL: RawUtf8): TRestOrm;
    /// retrieve a list of members as JSON encoded data
    // - used by OneFieldValue() and MultiFieldValue() methods
    function InternalListRawUtf8(TableIndex: integer; const SQL: RawUtf8): RawUtf8;
    /// virtual method called when a record is updated
    // - default implementation will call the OnUpdateEvent/OnBlobUpdateEvent
    // methods, if defined
    // - will also handle TOrmHistory tables, as defined by TrackChanges()
    // - returns true on success, false if an error occured (but action must continue)
    // - you can override this method to implement a server-wide notification,
    // but be aware it may be the first step to break the stateless architecture
    // of the framework
    function InternalUpdateEvent(aEvent: TOrmEvent; aTableIndex: integer; aID: TID;
      const aSentData: RawUtf8; aIsBlobFields: PFieldBits; aRec: TOrm): boolean; virtual;
    /// this method is called internally after any successfull deletion to
    // ensure relational database coherency
    // - reset all matching TRecordReference properties in the database Model,
    // for database coherency, into 0
    // - delete all records containing a matched TRecordReferenceToBeDeleted
    // property value in the database Model (e.g. TOrmHistory)
    // - reset all matching TOrm properties in the database Model,
    // for database coherency, into 0
    // - important notice: we don't use FOREIGN KEY constraints in this framework,
    // and handle all integrity check within this method (it's therefore less
    // error-prone, and more cross-database engine compatible)
    function AfterDeleteForceCoherency(aTableIndex: integer; aID: TID): boolean; virtual;
    /// call this method when the internal DB content is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but some virtual tables (e.g.
    // TRestStorageExternal classes defined in mormot.orm.sql) could flush
    // the database content without proper notification
    // - this default implementation will just do nothing, but mormot.orm.sqlite3.pas
    // unit will call TSqlDataBase.CacheFlush method
    procedure FlushInternalDBCache; virtual;
    /// called from STATE remote HTTP method
    procedure RefreshInternalStateFromStatic;
    /// assign a TRestOrm instance for a given slot
    // - called e.g. by TOrmVirtualTable.Create, StaticMongoDBRegister(),
    // StaticDataCreate() or TRestOrmServer.RemoteDataCreate
    procedure StaticTableSetup(aTableIndex: integer; aStatic: TRestOrm;
      aKind: TRestServerKind);
    /// fast get the associated static server or virtual table from its index, if any
    // - returns nil if aTableIndex is invalid or is not assigned to a TRestOrm
    function GetStaticTableIndex(aTableIndex: integer): TRestOrm; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// fast get the associated static server or virtual table from its index, if any
    // - returns nil if aTableIndex is invalid or is not assigned to a TRestOrm
    function GetStaticTableIndex(aTableIndex: integer;
      out Kind: TRestServerKind): TRestOrm; overload;
       {$ifdef HASINLINE}inline;{$endif}
    /// create an external static redirection for a specific class
    // - call it just after Create, before IRestOrmServer.CreateMissingTables;
    // warning: if you don't call this method before CreateMissingTable method
    // is called, the table will be created as a regular table by the main
    // database engine, and won't be static
    // - the specified TOrm class will have all its CRUD / ORM methods be
    // redirected to aRemoteRest, which may be a TRestClient or another
    // TRestServer instance (e.g. a fast SQLITE_MEMORY_DATABASE_NAME)
    // - if aRemoteRest is a TRestClient, it should have been authenticated
    // to the remote TRestServer, so that CRUD / ORM operations will pass
    // - this will enable easy creation of proxies, or local servers, with they
    // own cache and data model - e.g. a branch office server which may serve
    // its local clients over Ethernet, but communicating to a main mORMot
    // server via Internet, storing the corporate data in the main office server
    // - you may also share some tables (e.g. TAuthUser and TAuthGroup)
    // between TRestServer instances in a single service
    // - returns a newly created TRestStorageRemote instance
    function RemoteDataCreate(aClass: TOrmClass;
      aRemoteRest: TRestOrmParent): TRestOrmParent; virtual;
    /// fast get the associated TRestStorageRemote from its index, if any
    // - returns nil if aTableIndex is invalid or is not assigned to a TRestStorageRemote
    function GetRemoteTable(TableIndex: integer): TRestOrmParent;
    /// initialize change tracking for the given tables
    // - by default, it will use the TOrmHistory table to store the
    // changes - you can specify a dedicated class as aTableHistory parameter
    // - if aTableHistory is not already part of the TOrmModel, it will be added
    // - note that this setting should be consistent in time: if you disable
    // tracking for a while, or did not enable tracking before adding a record,
    // then the content history won't be consistent (or disabled) for this record
    // - at every change, aTableHistory.SentDataJson records will be added, up
    // to aMaxHistoryRowBeforeBlob items - then aTableHistory.History will store
    // a compressed version of all previous changes
    // - aMaxHistoryRowBeforeBlob is the maximum number of JSON rows per Table
    // before compression into BLOB is triggerred
    // - aMaxHistoryRowPerRecord is the maximum number of JSON rows per record,
    // above which the versions will be compressed as BLOB
    // - aMaxUncompressedBlobSize is the maximum BLOB size per record
    // - you can specify aMaxHistoryRowBeforeBlob=0 to disable change tracking
    // - you should call this method after the CreateMissingTables call
    // - note that change tracking may slow down the writing process, and
    // may increase storage space a lot (even if BLOB maximum size can be set),
    // so should be defined only when necessary
    procedure TrackChanges(const aTable: array of TOrmClass;
      aTableHistory: TOrmClass = nil;
      aMaxHistoryRowBeforeBlob: integer = 1000;
      aMaxHistoryRowPerRecord: integer = 10;
      aMaxUncompressedBlobSize: integer = 64*1024); virtual;
    /// force compression of all aTableHistory.SentDataJson into History BLOB
    // - by default, this will take place in InternalUpdateEvent() when
    // aMaxHistoryRowBeforeBlob - as set by TrackChanges() method - is reached
    // - you can manually call this method to force History BLOB update, e.g.
    // when the server is in Idle state, and ready for process
    procedure TrackChangesFlush(aTableHistory: TOrmClass); virtual;
    /// check if OnUpdateEvent or change tracked has been defined for this table
    // - is used internally e.g. by TRestServerDB.MainEngineUpdateField to
    // ensure that the updated ID fields will be computed as expected
    function InternalUpdateEventNeeded(aEvent: TOrmEvent; aTableIndex: integer): boolean;
    /// will compute the next monotonic value for a TRecordVersion field
    function RecordVersionCompute: TRecordVersion;
    /// read only access to the current monotonic value for a TRecordVersion field
    function RecordVersionCurrent: TRecordVersion;
    /// synchronous master/slave replication from a slave TRest
    // - apply all the updates from another (distant) master TRestOrm for a given
    // TOrm table, using its TRecordVersion field, to the calling slave
    // - both remote Master and local slave TRestServer should have the supplied
    // Table class in their data model (maybe in diverse order)
    // - by default, all pending updates are retrieved, but you can define a value
    // to ChunkRowLimit, so that the updates will be retrieved by smaller chunks
    // - returns -1 on error, or the latest applied revision number (which may
    // be 0 if there is no data in the table)
    // - this method will use regular REST ORM commands, so will work with any
    // communication channels: for real-time push synchronization, consider using
    // RecordVersionSynchronizeMasterStart and RecordVersionSynchronizeSlaveStart
    // over a bidirectionnal communication channel like WebSockets
    // - you can use RecordVersionSynchronizeSlaveToBatch if your purpose is
    // to access the updates before applying to the current slave storage
    function RecordVersionSynchronizeSlave(Table: TOrmClass;
      const Master: IRestOrm; ChunkRowLimit: integer = 0;
      const OnWrite: TOnBatchWrite = nil): TRecordVersion;
    /// synchronous master/slave replication from a slave TRest into a Batch
    // - will retrieve all the updates from a (distant) master TRest for a
    // given TOrm table, using its TRecordVersion field, and a supplied
    // TRecordVersion monotonic value, into a TRestBatch instance
    // - both remote Source and local TRestServer should have the supplied
    // Table class in each of their data model
    // - by default, all pending updates are retrieved, but you can define a value
    // to MaxRowLimit, so that the updates will be retrieved by smaller chunks
    // - returns nil if nothing new was found, or a TRestBatch instance
    // containing all modifications since RecordVersion revision
    // - when executing the returned TRestBatch on the database, you should
    // set TRestServer.RecordVersionDeleteIgnore := true so that the
    // TRecordVersion fields will be forced from the supplied value
    // - usually, you should not need to use this method, but rather the more
    // straightforward RecordVersionSynchronizeSlave()
    function RecordVersionSynchronizeSlaveToBatch(Table: TOrmClass;
      const Master: IRestOrm; var RecordVersion: TRecordVersion; MaxRowLimit: integer = 0;
      const OnWrite: TOnBatchWrite = nil): TRestBatch; virtual;
    /// access to the associated TRestServer main instance
    property Owner: TRestServer
      read fOwner;
    /// low-level value access to process TRecordVersion field
    property RecordVersionMax: TRecordVersion
      read fRecordVersionMax write fRecordVersionMax;
    /// retrieve the TRestStorage instance used to store and manage
    // a specified TOrmClass in memory
    // - raise an EModelException if aClass is not part of the database Model
    // - returns nil if this TOrmClass is handled by the main engine
    property StaticDataServer[aClass: TOrmClass]: TRestOrm
      read GetStaticDataServer;
    /// retrieve a running TRestStorage virtual table
    // - associated e.g. to a 'JSON' or 'Binary' virtual table module, or may
    // return a TRestStorageExternal instance (as defined in mormot.orm.sql)
    // - this property will return nil if there is no Virtual Table associated
    // or if the corresponding module is not a TOrmVirtualTable
    // (e.g. "pure" static tables registered by StaticDataCreate will be
    // accessible only via StaticDataServer[], not via StaticVirtualTable[])
    // - has been associated by the TOrmModel.VirtualTableRegister method or
    // the VirtualTableExternalRegister() global function
    property StaticVirtualTable[aClass: TOrmClass]: TRestOrm
      read GetVirtualTable;
    /// fast get the associated static server or virtual table, if any
    // - same as a dual call to StaticDataServer[aClass] + StaticVirtualTable[aClass]
    property StaticTable[aClass: TOrmClass]: TRestOrm
      read GetStaticTable;
    /// you can force this property to TRUE so that any Delete() will not
    // write to the TOrmTableDelete table for TRecordVersion tables
    // - to be used when applying a TRestBatch instance as returned by
    // RecordVersionSynchronizeToBatch()
    property RecordVersionDeleteIgnore: boolean
      read fRecordVersionDeleteIgnore write fRecordVersionDeleteIgnore;
    /// the options specified to TRestServer.CreateMissingTables
    // - as expected by TOrm.InitializeTable methods
    property CreateMissingTablesOptions: TOrmInitializeTableOptions
      read fCreateMissingTablesOptions;
  public
    { IRestOrm overriden methods }
    /// implement Server-Side TRest deletion
    // - uses internally EngineDelete() function for calling the database engine
    // - call corresponding fStaticData[] if necessary
    // - this record is also erased in all available TRecordReference properties
    // in the database Model, for relational database coherency
    function Delete(Table: TOrmClass; ID: TID): boolean; override;
    /// implement Server-Side TRest deletion with a WHERE clause
    // - will process all ORM-level validation, coherency checking and
    // notifications together with a low-level SQL deletion work (if possible)
    function Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean; override;
    /// overridden method for direct static class call (if any)
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// overridden method for direct static class call (if any)
    function TableHasRows(Table: TOrmClass): boolean; override;
    /// overridden method for direct static class call (if any)
    function MemberExists(Table: TOrmClass; ID: TID): boolean; override;
    /// update all BLOB fields of the supplied Value
    // - this overridden method will execute the direct static class, if any
    function UpdateBlobFields(Value: TOrm): boolean; override;
    /// get all BLOB fields of the supplied value from the remote server
    // - this overridden method will execute the direct static class, if any
    function RetrieveBlobFields(Value: TOrm): boolean; override;
    /// implement Server-Side TRest unlocking
    // - to be called e.g. after a Retrieve() with forupdate=TRUE
    // - implements our custom UNLOCK REST-like verb
    // - locking is handled by TRestOrmServer.Model
    // - returns true on success
    function UnLock(Table: TOrmClass; aID: TID): boolean; override;
    /// end a transaction
    // - implements REST END collection
    // - write all pending TOrmVirtualTableJson data to the disk
    procedure Commit(SessionID: cardinal; RaiseException: boolean); override;
  public
    { IRestOrmServer methods }
    /// create an index for the specific FieldName
    // - will call CreateSqlMultiIndex() internally
    function CreateSqlIndex(Table: TOrmClass;
      const FieldName: RawUtf8; Unique: boolean;
      const IndexName: RawUtf8 = ''): boolean; overload;
    /// create one or multiple index(es) for the specific FieldName(s)
    function CreateSqlIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean): boolean; overload;
    /// create one index for all specific FieldNames at once
    // - will call any static engine for the index creation of such tables, or
    // execute a CREATE INDEX IF NOT EXISTS on the main engine
    // - note that with SQLite3, your database schema should never contain two
    // indices where one index is a prefix of the other, e.g. if you defined:
    // ! aServer.CreateSqlMultiIndex(TEmails, ['Email','GroupID'], True);
    // Then the following index is not mandatory for SQLite3:
    // ! aServer.CreateSqlIndex(TEmails, 'Email', False);
    // see "1.6 Multi-Column Indices" in @http://www.sqlite.org/queryplanner.html
    function CreateSqlMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8;
      Unique: boolean; IndexName: RawUtf8 = ''): boolean; virtual;
    /// check if the supplied TOrm is not a virtual or static table
    function IsInternalSQLite3Table(aTableIndex: integer): boolean;
    /// returns true if the server will handle per-user authentication and
    // access right management
    function HandleAuthentication: boolean;
    /// this property can be left to its TRUE default value, to handle any
    // TOrmVirtualTableJson static tables (module JSON or BINARY) with direct
    // calls to the storage instance
    procedure SetStaticVirtualTableDirect(direct: boolean);
  published
    /// this property can be left to its TRUE default value, to handle any
    // TOrmVirtualTableJson static tables (module JSON or BINARY) with direct
    // calls to the storage instance
    // - see also IRestOrmServer.SetStaticVirtualTableDirect
    // - is set to TRUE by default to enable faster Direct mode
    // - in Direct mode, GET/POST/PUT/DELETE of individual records (or BLOB fields)
    // from Uri() will call directly the corresponding TRestStorage
    // instance, for better speed for most used RESTful operations; but complex
    // SQL requests (e.g. joined SELECT) will rely on the main SQL engine
    // - if set to false, will use the main SQLite3 engine for all statements
    // (should not to be used normally, because it will add unnecessary overhead)
    property StaticVirtualTableDirect: boolean read fVirtualTableDirect
      write fVirtualTableDirect;
  end;



implementation

uses
  mormot.orm.storage;

{ ************ TRestOrmServer Abstract Server}

{ TRestOrmServer }

constructor TRestOrmServer.Create(aRest: TRest);
var
  t: PtrInt;
begin
  if aRest <> nil then
    fOwner := aRest as TRestServer;
  // set fRest+fModel
  inherited Create(aRest);
  // faster direct Static call by default
  fVirtualTableDirect := true;
  // initialize TrackChanges() associated tables
  if fModel.Tables <> nil then
  begin
    fTrackChangesHistoryTableIndexCount := length(fModel.Tables);
    SetLength(fTrackChangesHistory, fTrackChangesHistoryTableIndexCount);
    if fTrackChangesHistoryTableIndexCount > 64 then
      // rows are identified as RecordRef
      fTrackChangesHistoryTableIndexCount := 64;
    SetLength(fTrackChangesHistoryTableIndex, fTrackChangesHistoryTableIndexCount);
    for t := 0 to fTrackChangesHistoryTableIndexCount - 1 do
      fTrackChangesHistoryTableIndex[t] := -1;
    fOrmVersionDeleteTable := TOrmTableDeleted;
    for t := 0 to high(fModel.Tables) do
      if fModel.Tables[t].OrmProps.RecordVersionField <> nil then
      begin
        fOrmVersionDeleteTable := fModel.AddTableInherited(TOrmTableDeleted);
        break;
      end;
  end;
end;

destructor TRestOrmServer.Destroy;
var
  i: PtrInt;
  orm: TRestOrm;
begin
  // free all virtual TRestStorage instances
  for i := 0 to high(fStaticVirtualTable) do
    if fStaticVirtualTable[i] <> nil then
    begin
      if fStaticVirtualTable[i].RefCount <> 1 then
        raise ERestStorage.CreateUtf8('%.Destroy: static virtual % refcnt=%',
          [self, fStaticVirtualTable[i], fStaticVirtualTable[i].RefCount]);
      IInterface(fStaticVirtualTable[i])._Release;
      if fStaticData <> nil then
        // free once as fStaticVirtualTable[i], just clear reference here
        fStaticData[i] := nil;
    end;
  // free lasting TRestStorage instances and update file if necessary
  for i := 0 to high(fStaticData) do
  begin
    orm := fStaticData[i];
    if orm <> nil then
    begin
      if orm.RefCount <> 1 then
        raise ERestStorage.CreateUtf8('%.Destroy: static % refcnt=%',
          [self, orm, orm.RefCount]);
      IInterface(orm)._Release;
    end;
  end;
  inherited Destroy; // fCache.Free
end;

procedure TRestOrmServer.BeginCurrentThread(Sender: TThread);
var
  i: PtrInt;
begin
  for i := 0 to length(fStaticVirtualTable) - 1 do
    if fStaticVirtualTable[i] <> nil then
      fStaticVirtualTable[i].BeginCurrentThread(Sender);
end;

procedure TRestOrmServer.EndCurrentThread(Sender: TThread);
var
  i: PtrInt;
begin
  for i := 0 to length(fStaticVirtualTable) - 1 do
    if fStaticVirtualTable[i] <> nil then
      fStaticVirtualTable[i].EndCurrentThread(Sender);
end;

procedure TRestOrmServer.CreateMissingTables(user_version: cardinal;
  options: TOrmInitializeTableOptions);
begin
  fCreateMissingTablesOptions := options;
end;

procedure TRestOrmServer.InitializeTables(Options: TOrmInitializeTableOptions);
var
  t: PtrInt;
begin
  if (self <> nil) and
     (fModel <> nil) then
    for t := 0 to fModel.TablesMax do
      if not TableHasRows(fModel.Tables[t]) then
        fModel.Tables[t].InitializeTable(self, '', Options);
end;

procedure TRestOrmServer.SetNoAjaxJson(const Value: boolean);
begin
  // do nothing at this level
end;

function TRestOrmServer.GetStaticDataServer(aClass: TOrmClass): TRestOrm;
var
  i: cardinal;
begin
  if (self <> nil) and
     (fStaticData <> nil) then
  begin
    i := fModel.GetTableIndexExisting(aClass);
    if i < cardinal(length(fStaticData)) then
      result := fStaticData[i] // no IRestOrm refcnt involved here
    else
      result := nil;
  end
  else
    result := nil;
end;

function TRestOrmServer.GetVirtualTable(aClass: TOrmClass): TRestOrm;
var
  i: PtrInt;
begin
  result := nil;
  if fStaticVirtualTable <> nil then
  begin
    i := fModel.GetTableIndexExisting(aClass);
    if (i >= 0) and
       (fModel.TableProps[i].Kind in IS_CUSTOM_VIRTUAL) then
      result := fStaticVirtualTable[i]; // no IRestOrm refcnt involved here
  end;
end;

function TRestOrmServer.GetStaticTable(aClass: TOrmClass): TRestOrm;
begin
  if (aClass = nil) or
     ((fStaticData = nil) and
      (fStaticVirtualTable = nil)) then
    result := nil
  else
    result := GetStaticTableIndex(fModel.GetTableIndexExisting(aClass));
end;

function TRestOrmServer.GetStaticTableIndex(aTableIndex: integer): TRestOrm;
begin
  result := nil;
  if aTableIndex >= 0 then
  begin
    if cardinal(aTableIndex) < cardinal(length(fStaticData)) then
      result := fStaticData[aTableIndex]; // no IRestOrm refcnt here
    if result = nil then
      if fVirtualTableDirect and
         (fStaticVirtualTable <> nil) then
        result := fStaticVirtualTable[aTableIndex];
  end;
end;

function TRestOrmServer.GetStaticTableIndex(aTableIndex: integer;
  out Kind: TRestServerKind): TRestOrm;
begin
  result := nil;
  Kind := sMainEngine;
  if aTableIndex >= 0 then
  begin
    if cardinal(aTableIndex) < cardinal(length(fStaticData)) then
    begin
      result := fStaticData[aTableIndex]; // no IRestOrm refcnt here
      if result <> nil then
      begin
        Kind := sStaticDataTable;
        exit;
      end;
    end;
    if fVirtualTableDirect and
       (fStaticVirtualTable <> nil) then
    begin
      result := fStaticVirtualTable[aTableIndex]; // no IRestOrm refcnt here
      if result <> nil then
        Kind := sVirtualTable;
    end;
  end;
end;

function TRestOrmServer.RemoteDataCreate(aClass: TOrmClass;
  aRemoteRest: TRestOrmParent): TRestOrmParent;
var
  t: integer;
  existing: TRestOrm;
begin
  t := Model.GetTableIndexExisting(aClass);
  existing := GetStaticTableIndex(t);
  if existing <> nil then
    raise ERestStorage.CreateUtf8('Duplicated %.RemoteDataCreate(%) as %',
      [self, aClass, existing]);
  result := TRestStorageRemote.Create(aClass, self, aRemoteRest as TRestOrm);
  StaticTableSetup(t, result as TRestOrm, sStaticDataTable);
end;

function TRestOrmServer.GetRemoteTable(TableIndex: integer): TRestOrmParent;
begin
  if (cardinal(TableIndex) >= cardinal(length(fStaticData))) or
     (fStaticData[TableIndex] = nil) or
     not fStaticData[TableIndex].InheritsFrom(TRestStorageRemote) then
    result := nil
  else
    result := TRestStorageRemote(fStaticData[TableIndex]).RemoteRest;
end;

function TRestOrmServer.MaxUncompressedBlobSize(Table: TOrmClass): integer;
var
  i: integer;
begin
  i := fModel.GetTableIndexExisting(Table);
  if (i >= 0) and
     (cardinal(i) < cardinal(length(fTrackChangesHistory))) then
    result := fTrackChangesHistory[i].MaxUncompressedBlobSize
  else
    result := 0;
end;

procedure TRestOrmServer.InternalRecordVersionMaxFromExisting(RetrieveNext: PID);
var
  m: PtrInt;
  field: TOrmPropInfoRttiRecordVersion;
  current, max, mDeleted: Int64;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    if fRecordVersionMax = 0 then // check twice to avoid race condition
    begin
      current := 0;
      for m := 0 to fModel.TablesMax do
      begin
        field := fModel.Tables[m].OrmProps.RecordVersionField;
        if field <> nil then
        begin
          if OneFieldValue(fModel.Tables[m],
              'max(' + field.Name + ')', '', [], [], max) then
            if max > current then
              current := max;
          mDeleted := Int64(m) shl ORMVERSION_DELETEID_SHIFT;
          if OneFieldValue(fOrmVersionDeleteTable, 'max(ID)', 'ID>? and ID<?',
              [], [mDeleted, mDeleted + ORMVERSION_DELETEID_RANGE], max) then
          begin
            max := max and pred(ORMVERSION_DELETEID_RANGE);
            if max > current then
              current := max;
          end;
        end;
      end;
    end
    else
      current := fRecordVersionMax;
    if RetrieveNext <> nil then
    begin
      inc(current);
      RetrieveNext^ := current;
    end;
    fRecordVersionMax := current;
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;

procedure TRestOrmServer.InternalRecordVersionDelete(TableIndex: integer;
  ID: TID; Batch: TRestBatch);
var
  deleted: TOrmTableDeleted;
  revision: TRecordVersion;
begin
  if fRecordVersionDeleteIgnore then
    exit;
  deleted := fOrmVersionDeleteTable.Create;
  try
    revision := RecordVersionCompute;
    deleted.IDValue := revision +
      Int64(TableIndex) shl ORMVERSION_DELETEID_SHIFT;
    deleted.Deleted := ID;
    if Batch <> nil then
      Batch.Add(deleted, True, True)
    else
      Add(deleted, True, True);
    if (fOwner <>nil) and
       (fOwner.Services <> nil) then
      (fOwner.Services as TServiceContainerServer).
        RecordVersionNotifyDelete(TableIndex, ID, revision);
  finally
    deleted.Free;
  end;
end;

function TRestOrmServer.InternalRecordVersionComputeNext: TRecordVersion;
begin
  if fRecordVersionMax = 0 then
    InternalRecordVersionMaxFromExisting(@result)
  else
  begin
    fRest.AcquireExecution[execOrmWrite].Safe.Lock;
    inc(fRecordVersionMax);
    result := fRecordVersionMax;
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;

function TRestOrmServer.RecordVersionCompute: TRecordVersion;
begin
  result := InternalRecordVersionComputeNext;
  if result >= ORMVERSION_DELETEID_RANGE then
    raise EOrmException.CreateUtf8(
     '%.InternalRecordVersionCompute=% overflow: %.ID should be < 2^%)',
     [self, result, fOrmVersionDeleteTable, ORMVERSION_DELETEID_SHIFT]);
end;

function TRestOrmServer.RecordVersionCurrent: TRecordVersion;
begin
  if self = nil then
    result := 0
  else
  begin
    if fRecordVersionMax = 0 then
      InternalRecordVersionMaxFromExisting(nil);
    result := fRecordVersionMax;
  end;
end;

function TRestOrmServer.RecordVersionSynchronizeSlave(
  Table: TOrmClass; const Master: IRestOrm; ChunkRowLimit: integer;
  const OnWrite: TOnBatchWrite): TRecordVersion;
var
  batch: TRestBatch;
  ids: TIDDynArray;
  status: integer;
  {%H-}log: ISynLog;
begin
  log := fRest.LogClass.Enter('RecordVersionSynchronizeSlave %', [Table], self);
  result := -1; // error
  if fRecordVersionMax = 0 then
    InternalRecordVersionMaxFromExisting(nil);
  repeat
    batch := RecordVersionSynchronizeSlaveToBatch(Table, Master,
      fRecordVersionMax, ChunkRowLimit, OnWrite);
    if batch = nil then
      // error
      exit;
    if batch.Count = 0 then
    begin
      // nothing new (e.g. reached last chunk)
      result := fRecordVersionMax;
      batch.Free;
      break;
    end;
    try
      fRest.AcquireExecution[execOrmWrite].Safe.Lock;
      fRecordVersionDeleteIgnore := true;
      status := BatchSend(batch, ids);
      if status = HTTP_SUCCESS then
      begin
        InternalLog(
          'RecordVersionSynchronize(%) Added=% Updated=% Deleted=% on %',
          [Table, batch.AddCount, batch.UpdateCount, batch.DeleteCount,
           Master], sllDebug);
        if ChunkRowLimit = 0 then
        begin
          result := fRecordVersionMax;
          break;
        end;
      end
      else
      begin
        InternalLog('RecordVersionSynchronize(%) BatchSend=%',
          [Table, status], sllError);
        fRecordVersionMax := 0; // force recompute the maximum from DB
        break;
      end;
    finally
      fRecordVersionDeleteIgnore := false;
      fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
      batch.Free;
    end;
  until false; // continue synch until nothing new is found
end;

function TRestOrmServer.RecordVersionSynchronizeSlaveToBatch(
  Table: TOrmClass; const Master: IRestOrm; var RecordVersion: TRecordVersion;
  MaxRowLimit: integer; const OnWrite: TOnBatchWrite): TRestBatch;
var
  tableindex, sourcetableindex, updatedrow, deletedrow: integer;
  props: TOrmProperties;
  where: RawUtf8;
  updatedversion, deletedversion: TRecordVersion;
  listupdated, listdeleted: TOrmTable;
  rec: TOrm;
  deletedminid: TID;
  deleted: TOrmTableDeleted;
  {%H-}log: ISynLog;
begin
  log := fRest.LogClass.Enter(
    'RecordVersionSynchronizeSlaveToBatch % vers=% maxrow=%',
    [Table, RecordVersion, MaxRowLimit], self);
  result := nil;
  if Master = nil then
    raise EOrmException.CreateUtf8(
      '%.RecordVersionSynchronizeSlaveToBatch(Master=nil)', [self]);
  tableindex := Model.GetTableIndexExisting(Table);
  sourcetableindex := Master.Model.GetTableIndexExisting(Table); // <>tableindex?
  props := Model.TableProps[tableindex].props;
  if props.RecordVersionField = nil then
    raise EOrmException.CreateUtf8(
      '%.RecordVersionSynchronizeSlaveToBatch(%) with no TRecordVersion field',
      [self, Table]);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    where := '%>? order by %';
    if MaxRowLimit > 0 then
      where := FormatUtf8('% limit %', [where, MaxRowLimit]);
    listupdated := Master.MultiFieldValues(Table, '*', where,
      [props.RecordVersionField.Name, props.RecordVersionField.Name],
      [RecordVersion]);
    if listupdated = nil then
      exit; // DB error
    listdeleted := nil;
    try
      deletedminid := Int64(sourcetableindex) shl ORMVERSION_DELETEID_SHIFT;
      where := 'ID>? and ID<? order by ID';
      if MaxRowLimit > 0 then
        where := FormatUtf8('% limit %', [where, MaxRowLimit]);
      listdeleted := Master.MultiFieldValues(fOrmVersionDeleteTable,
        'ID,Deleted', where, [deletedminid + RecordVersion,
         deletedminid + ORMVERSION_DELETEID_RANGE]);
      if listdeleted = nil then
        exit; // DB error
      result := TRestBatch.Create(self, nil, 10000);
      result.OnWrite := OnWrite;
      if (listupdated.RowCount = 0) and
         (listdeleted.RowCount = 0) then
        // nothing new -> returns void TRestBach with Count=0
        exit;
      rec := Table.Create;
      deleted := fOrmVersionDeleteTable.Create;
      try
        rec.FillPrepare(listupdated);
        deleted.FillPrepare(listdeleted);
        updatedrow := 1;
        deletedrow := 1;
        updatedversion := 0;
        deletedversion := 0;
        repeat
          // compute all changes in increasing version order
          if updatedversion = 0 then
            if updatedrow <= listupdated.RowCount then
            begin
              rec.FillRow(updatedrow);
              updatedversion := props.RecordVersionField.PropInfo.GetInt64Prop(rec);
              inc(updatedrow);
            end;
          if deletedversion = 0 then
            if deletedrow <= listdeleted.RowCount then
            begin
              deleted.FillRow(deletedrow);
              deletedversion := deleted.IDValue and pred(ORMVERSION_DELETEID_RANGE);
              inc(deletedrow);
            end;
          if (updatedversion = 0) and
             (deletedversion = 0) then
            break; // no more update available
          if (updatedversion > 0) and
             ((deletedversion = 0) or
              (updatedversion < deletedversion)) then
          begin
            if (RecordVersion = 0) or
               (OneFieldValue(Table, 'ID', rec.IDValue) = '') then
              result.Add(rec, true, true, rec.FillContext.TableMapFields, true)
            else
              result.Update(rec, [], true);
            RecordVersion := updatedversion;
            updatedversion := 0;
          end
          else if deletedversion > 0 then
          begin
            result.Delete(Table, deleted.deleted);
            deleted.IDValue := deletedversion + // local ID follows current Model
              Int64(tableindex) shl ORMVERSION_DELETEID_SHIFT;
            result.Add(deleted, true, true, [], true);
            RecordVersion := deletedversion;
            deletedversion := 0;
          end;
        until false;
      finally
        deleted.Free;
        rec.Free;
      end;
    finally
      listupdated.Free;
      listdeleted.Free;
    end;
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;


{ overridden methods which will perform CRUD operations }

function TRestOrmServer.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineAdd(TableModelIndex, SentData)
  else
    result := rest.EngineAdd(TableModelIndex, SentData);
end;

function TRestOrmServer.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineRetrieve(TableModelIndex, ID)
  else
    result := rest.EngineRetrieve(TableModelIndex, ID);
end;

function TRestOrmServer.EngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
var
  rest: TRestOrm;
  sqladapted: RawUtf8;
begin
  sqladapted := SQL;
  rest := InternalAdaptSql(
    fModel.GetTableIndexFromSqlSelect(SQL, false), sqladapted);
  if rest = nil then
    result := MainEngineList(SQL, ForceAjax, ReturnedRowCount)
  else
    result := rest.EngineList(sqladapted, ForceAjax, ReturnedRowCount);
end;

function TRestOrmServer.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUtf8): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineUpdate(TableModelIndex, ID, SentData)
  else
    result := rest.EngineUpdate(TableModelIndex, ID, SentData);
end;

function TRestOrmServer.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineDelete(TableModelIndex, ID)
  else
    result := rest.EngineDelete(TableModelIndex, ID);
  if result then
    if fModel.TableProps[TableModelIndex].Props.RecordVersionField <> nil then
      InternalRecordVersionDelete(TableModelIndex, ID, nil);
end;

function TRestOrmServer.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  rest: TRestOrm;
  batch: TRestBatch;
  i: PtrInt;
begin
  case length(IDs) of
    0:
      result := false;
    1:
      result := EngineDelete(TableModelIndex, IDs[0]);
  else
    begin
      rest := GetStaticTableIndex(TableModelIndex);
      if rest = nil then
        result := MainEngineDeleteWhere(TableModelIndex, SqlWhere, IDs)
      else
        result := rest.EngineDeleteWhere(TableModelIndex, SqlWhere, IDs);
      if (fModel.TableProps[TableModelIndex].Props.RecordVersionField = nil) or
         not result then
        exit;
      batch := TRestBatch.Create(self, fOrmVersionDeleteTable, 1000);
      try
        for i := 0 to high(IDs) do
          InternalRecordVersionDelete(TableModelIndex, IDs[i], batch);
        BatchSend(batch); // allow faster deletion for engines supporting it
      finally
        batch.Free;
      end;
    end;
  end;
end;

function TRestOrmServer.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineRetrieveBlob(TableModelIndex,
      aID, BlobField, BlobData)
  else
    result := rest.EngineRetrieveBlob(TableModelIndex,
      aID, BlobField, BlobData);
end;

function TRestOrmServer.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineUpdateBlob(TableModelIndex,
      aID, BlobField, BlobData)
  else
    result := rest.EngineUpdateBlob(TableModelIndex,
      aID, BlobField, BlobData);
end;

function TRestOrmServer.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineUpdateField(TableModelIndex,
      SetFieldName, SetValue, WhereFieldName, WhereValue)
  else
    result := rest.EngineUpdateField(TableModelIndex,
      SetFieldName, SetValue, WhereFieldName, WhereValue);
end;

function TRestOrmServer.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUtf8; Increment: Int64): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineUpdateFieldIncrement(TableModelIndex,
      ID, FieldName, Increment)
  else
    result := rest.EngineUpdateFieldIncrement(TableModelIndex,
      ID, FieldName, Increment);
end;

function TRestOrmServer.EngineBatchSend(Table: TOrmClass;
  var Data: RawUtf8; var Results: TIDDynArray;
  ExpectedResultsCount: integer): integer;
var
  info: TGetJsonField;
  ok, runmaintrans: boolean;
  runstatickind: TRestServerKind;
  batchoptions: TRestBatchOptions;
  fmt: TSaveFieldsAsObject;
  tablename, value, errmsg: RawUtf8;
  encoding, runningbatchencoding: TRestBatchEncoding;
  runningbatchrest, runningrest: TRestOrm;
  cmd, cmdtable, simplevalue: PUtf8Char;
  P: PAnsiChar;
  transperrow, rowcountpercurrtrans: cardinal;
  runtabletrans: array of TRestOrm;
  id: TID;
  timeouttix: Int64;
  runtable, runningbatchtable: TOrmClass;
  runtableindex, i, tableindex, count, errors: integer;
  runstatic: TRestOrm;
  uricontext: TRestServerUriContext;
  timer: TPrecisionTimer;
  runfields: TFieldBits;
  counts: array[TRestBatchEncoding] of cardinal;

  procedure PerformAutomaticCommit;
  var
    i: PtrInt;
  begin
    if runningbatchrest <> nil then
    begin
      runningbatchrest.InternalBatchStop; // send pending rows before commit
      runningbatchrest := nil;
      runningbatchtable := nil;
    end;
    for i := 0 to high(runtabletrans) do
      if runtabletrans[i] <> nil then
      begin
        runtabletrans[i].Commit(CONST_AUTHENTICATION_NOT_USED, true);
        if runtabletrans[i] = self then
          runmaintrans := false;
        runtabletrans[i] := nil; // to acquire and begin a new transaction
      end;
    rowcountpercurrtrans := 0;
  end;

  function IsNotAllowed: boolean;
    {$ifdef FPC} inline; {$endif}
  begin
    result := (uricontext <> nil) and
              (uricontext.Command = execOrmWrite) and
              not uricontext.CanExecuteOrmWrite(BATCH_METHOD[encoding],
      runtable, runtableindex, id, uricontext.Call.RestAccessRights^);
  end;

var
  log: ISynLog;
begin
  log := fRest.LogClass.Enter('EngineBatchSend % inlen=%',
    [Table, length(Data)], self);
  //log.Log(sllCustom2, Data, self, 100 shl 10);
  info.Json := pointer(Data); // will be parsed therefore in-place modified
  if info.Json = nil then
    raise EOrmBatchException.CreateUtf8(
      '%.EngineBatchSend(%,"")', [self, Table]);
  if Table <> nil then
  begin
    tableindex := fModel.GetTableIndexExisting(Table);
    // unserialize expected sequence array as '{"Table":["cmd",values,...]}'
    if not NextNotSpaceCharIs(info.Json, '{') then
      raise EOrmBatchException.CreateUtf8('%.EngineBatchSend: Missing {', [self]);
    tablename := GetJsonPropName(info.Json);
    if (tablename = '') or
       (info.Json = nil) or
       not IdemPropNameU(tablename,
         fModel.TableProps[tableindex].Props.SqlTableName) then
      raise EOrmBatchException.CreateUtf8(
        '%.EngineBatchSend(%): Wrong "Table":"%"', [self, Table, tablename]);
  end
  else
    // or '["cmd@Table":values,...]'
    tableindex := -1;
  if not NextNotSpaceCharIs(info.Json, '[') then
    raise EOrmBatchException.CreateUtf8(
      '%.EngineBatchSend: Missing [', [self]);
  if IdemPChar(info.Json, '"AUTOMATICTRANSACTIONPERROW",') then
  begin
    inc(info.Json, 29);
    transperrow := GetNextItemCardinal(info.Json, ',');
  end
  else
    transperrow := 0;
  SetLength(runtabletrans, fModel.TablesMax + 1);
  runmaintrans := false;
  rowcountpercurrtrans := 0;
  if IdemPChar(info.Json, '"OPTIONS",') then
  begin
    inc(info.Json, 10);
    byte(batchoptions) := GetNextItemCardinal(info.Json, ',');
  end
  else
    byte(batchoptions) := 0;
  timer.Start;
  uricontext := ServiceRunningContext^.Request;
  cmd := nil;
  runningbatchrest := nil;
  runningbatchtable := nil;
  runningbatchencoding := encPost;
  runningrest := nil;
  runstatic := nil;
  runtableindex := -1;
  count := 0;
  errors := 0;
  FillZero(runfields);
  FillCharFast(counts, SizeOf(counts), 0);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock; // multi thread protection
  // try..except to intercept any error
  try
    // try..finally for transactions, writelock and InternalBatchStart
    try
      // main loop: process one POST/PUT/DELETE per iteration
      // "POST",{object}  "SIMPLE",[values]  "PUT",{object}  "DELETE",id
      repeat
        // retrieve cmd name and associated (static) table
        if info.Json = nil then
          raise EOrmBatchException.CreateUtf8(
            '%.EngineBatchSend: unexpected end of input', [self]);
        info.Json := GotoNextNotSpace(info.Json);
        if info.Json^ = '"' then
        begin
          info.GetJsonField;
          cmd := info.Value;
          if (info.Json = nil) or
             (cmd = nil) then
            raise EOrmBatchException.CreateUtf8(
              '%.EngineBatchSend: Missing CMD', [self]);
          if tableindex >= 0 then
          begin
            // e.g. '{"Table":[...,"POST",{object},...]}'
            runtableindex := tableindex;
            runtable := Table;
          end
          else
          begin
            // e.g. '[...,"POST@Table",{object},...]'
            cmdtable := PosChar(cmd, '@');
            if cmdtable <> nil then
            begin
              cmdtable^ := #0; // isolate 'POST' or 'hex'/'ihex' prefix
              runtableindex := fModel.GetTableIndexPtr(cmdtable + 1);
              if runtableindex < 0 then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: Unknown %', [self, cmdtable]);
              runtable := fModel.Tables[runtableindex];
            end;
          end;
          runstatic := GetStaticTableIndex(runtableindex, runstatickind);
          if runstatic = nil then
            runningrest := self
          else
            runningrest := runstatic;
        end
        else
        begin
          // allow "POST",{obj1},{obj2} or "SIMPLE",[v1],[v2] or "DELETE",id1,id2
          // (never appearing if boNoModelEncoding was set on Client side)
          if (runtableindex < 0) or
             (cmd = nil) then
            // plain "POST",{object} should reuse the previous table
            raise EOrmBatchException.CreateUtf8(
              '%.EngineBatchSend: "..@Table" expected', [self]);
        end;
        // get CRUD cmd and associated value/id
        case PWord(cmd)^ of // enough to check the first 2 chars
          ord('P') + ord('O') shl 8:
            begin
              // '{"Table":[...,"POST",{object},...]}'
              // or '[...,"POST@Table",{object},...]'
              encoding := encPost;
              value := JsonGetObject(info.Json, @id, info.EndOfObject, true);
              if info.Json = nil then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: Wrong POST', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: POST/Add not allowed on %',
                  [self, runtable]);
              if not RecordCanBeUpdated(runtable, id, oeAdd, @errmsg) then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: POST impossible: %', [self, errmsg]);
            end;
          ord('P') + ord('U') shl 8:
            begin
              // '{"Table":[...,"PUT",{object},...]}'
              // or '[...,"PUT@Table",{object},...]'
              encoding := encPut;
              value := JsonGetObject(info.Json, @id, info.EndOfObject, false);
              if (info.Json = nil) or
                 (value = '') or
                 (id <= 0) then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: Wrong PUT', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: PUT/Update not allowed on %',
                  [self, runtable]);
            end;
          ord('D') + ord('E') shl 8:
            begin
              // '{"Table":[...,"DELETE",id,...]}'
              // or '[...,"DELETE@Table",id,...]'
              encoding := encDelete;
              id := info.GetJsonInt64;
              if (id <= 0) or
                 info.WasString then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: Wrong DELETE', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: DELETE not allowed on %',
                  [self, runtable]);
              if not RecordCanBeUpdated(runtable, id, oeDelete, @errmsg) then
                raise EOrmBatchException.CreateUtf8(
                  '%.EngineBatchSend: DELETE impossible [%]', [self, errmsg]);
            end;
          ord('S') + ord('I') shl 8:
            begin
              // '{"Table":[...,"SIMPLE",[values...' or '[...,"SIMPLE@Table"...
              id := 0; // no id is never transmitted with "SIMPLE" fields
              encoding := encSimple;
              runfields := runtable.OrmProps.SimpleFieldsBits[ooInsert];
            end;
        else
          begin
            id := 0; // no id by default
            encoding := encPostHex;   // "hex",[...] or "hex@Table",[...]
            FillZero(runfields);
            P := pointer(cmd);
            case P^ of
              'i': // "ihex",[id,...] or "ihex@Table",[id,...]
                begin
                  encoding := encPostHexID;
                  inc(P);
                end;
              'u':
                begin // "uhex",[id,...] or "uhex@Table",[id,...]
                  encoding := encPutHexID;
                  inc(P);
                end;
            end;
            if not HexDisplayToBin(P, @runfields, StrLen(P) shr 1) then
              raise EOrmBatchException.CreateUtf8(
                '%.EngineBatchSend: Unknown [%] cmd', [self, cmd]);
          end;
        end;
        if encoding in BATCH_DIRECT then
        begin
          // first InternalBatchDirect(sent=nil) call to check if supported
          if (runningrest.InternalBatchDirect(
               encoding, runtableindex, runfields, nil) <> 0) and
             (info.Json^ = '[')  then
          begin
            // this storage engine allows direct JSON array process
            // (see e.g. TRestOrmServerDB.InternalBatchDirect
            //  or TRestStorageTOrm.InternalBatchDirect)
            info.GetJsonFieldOrObjectOrArray;
            simplevalue := info.Value;
          end
          else
          begin
            // convert input array into a JSON object as regular "POST"/"PUT"
            if encoding = encPostHexID then
              fmt := [sfoExtendedJson, sfoStartWithID, sfoPutIDFirst]
            else if encoding = encPutHexID then
              fmt := [sfoExtendedJson, sfoStartWithID]
            else
              fmt := [sfoExtendedJson];
            value := fModel.TableProps[runtableindex].Props.
              SaveFieldsFromJsonArray(info.Json, runfields, @id, @info.EndOfObject, fmt);
            if encoding = encPutHexID then
              encoding := encPut
            else
              encoding := encPost;
            if (info.Json = nil) or
               (value = '') then
              raise EOrmBatchException.CreateUtf8(
                '%.EngineBatchSend: % incorrect format', [self, cmd]);
          end;
          if IsNotAllowed then
            raise EOrmBatchException.CreateUtf8(
              '%.EngineBatchSend: % not allowed on %', [self, cmd, runtable]);
          if not RecordCanBeUpdated(runtable, 0, BATCH_EVENT[encoding], @errmsg) then
            raise EOrmBatchException.CreateUtf8(
              '%.EngineBatchSend: % impossible: %', [self, cmd, errmsg]);
        end;
        if (count = 0) and
           (info.EndOfObject = ']') then
        begin
          // single op do not need a transaction nor InternalBatchStart/Stop
          transperrow := 0;
          SetLength(Results, 1);
          if encoding in BATCH_DIRECT then
          begin
            // InternalBatchDirect requires InternalBatchStart -> object fallback
            if encoding in BATCH_DIRECT_ID then
              fmt := [sfoExtendedJson, sfoStartWithID, sfoPutIDFirst]
            else
              fmt := [sfoExtendedJson];
            value := fModel.TableProps[runtableindex].Props.
              SaveFieldsFromJsonArray(simplevalue, runfields, @id, nil, fmt);
            if encoding = encPutHexID then
              encoding := encPut
            else
              encoding := encPost;
          end;
        end
        else
        begin
          // handle auto-committed transaction process
          if transperrow > 0 then
          begin
            if rowcountpercurrtrans = transperrow then
              // reached transperrow chunk
              PerformAutomaticCommit;
            inc(rowcountpercurrtrans);
            if runtabletrans[runtableindex] = nil then
              // initiate transaction for this table if not started yet
              if (runstatic <> nil) or
                 not runmaintrans then
              begin
                timeouttix := GetTickCount64 + 2000;
                repeat
                  if runningrest.TransactionBegin(
                    runtable, CONST_AUTHENTICATION_NOT_USED) then
                  begin
                    // acquired transaction
                    runtabletrans[runtableindex] := runningrest;
                    if runstatic = nil then
                      runmaintrans := true;
                    Break;
                  end;
                  if GetTickCount64 > timeouttix then
                    raise EOrmBatchException.CreateUtf8(
                      '%.EngineBatchSend: %.TransactionBegin timeout',
                      [self, runningrest]);
                  SleepHiRes(1); // retry in 1 ms
                until (fOwner <> nil) and
                      (fOwner.ShutdownRequested);
              end;
          end;
          // handle batch pending request sending (if table or cmd changed)
          if (runningbatchrest <> nil) and
             ((runtable <> runningbatchtable) or
              (runningbatchencoding <> encoding)) then
          begin
            runningbatchrest.InternalBatchStop; // send pending statements
            runningbatchrest := nil;
            runningbatchtable := nil;
          end;
          if (runstatic <> nil) and
             (runstatic <> runningbatchrest) and
             runstatic.InternalBatchStart(encoding, batchoptions) then
          begin
            runningbatchrest := runstatic;
            runningbatchtable := runtable;
            runningbatchencoding := encoding;
          end
          else
          if (runningbatchrest = nil) and
             (runstatic = nil) and
             InternalBatchStart(encoding, batchoptions) then
          begin
            runningbatchrest := self; // e.g. multi-insert in main SQlite3 engine
            runningbatchtable := runtable;
            runningbatchencoding := encoding;
          end;
          if count >= length(Results) then
            SetLength(Results, NextGrow(count));
        end;
        // process CRUD cmd operation
        ok := false;
        Results[count] := HTTP_NOTMODIFIED;
        case encoding of
          encPost:
            begin
              id := EngineAdd(runtableindex, value);
              Results[count] := id;
              if id <> 0 then
              begin
                if fCache <> nil then
                  fCache.Notify(runtableindex, id, value, ooInsert);
                ok := true;
              end;
            end;
          encSimple,
          encPostHex,
          encPostHexID,
          encPutHexID:
            begin
              // note: DB operation could be delayed in InternalBatchDirect()
              // (for multi-insert, may be up to InternalBatchStop)
              id := runningrest.InternalBatchDirect(
                encoding, runtableindex, runfields, simplevalue);
              Results[count] := id;
              if id <> 0 then
                ok := true;
              // no ready-to-used value -> no fCache notification
            end;
          encPut:
            if EngineUpdate(runtableindex, id, value) then
            begin
              Results[count] := HTTP_SUCCESS; // 200 ok
              ok := true;
              if fCache <> nil then
                // JSON value may be uncomplete -> delete from cache
                if not (boPutNoCacheFlush in batchoptions) then
                  fCache.NotifyDeletion(runtableindex, id);
            end;
          encDelete:
            if EngineDelete(runtableindex, id) then
            begin
              if fCache <> nil then
                fCache.NotifyDeletion(runtableindex, id);
              if (runningbatchrest <> nil) or
                 AfterDeleteForceCoherency(runtableindex, id) then
              begin
                Results[count] := HTTP_SUCCESS; // 200 ok
                ok := true;
              end;
            end;
        end;
        if not ok then
          if boRollbackOnError in batchoptions then
            raise EOrmBatchException.CreateUtf8(
              '%.EngineBatchSend: Results[%]=% on % %',
              [self, count, Results[count], cmd, runtable])
          else
            inc(errors);
        inc(count);
        inc(counts[encoding]);
      until info.EndOfObject = ']';
      if (transperrow > 0) and
         (rowcountpercurrtrans > 0) then
        // send pending rows within transaction
        PerformAutomaticCommit;
    finally
      try
        if runningbatchrest <> nil then
          // send pending rows, and release Safe.Lock
          runningbatchrest.InternalBatchStop;
      finally
        fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
        log.Log(LOG_TRACEERROR[errors <> 0], 'EngineBatchSend json=% count=% ' +
          'errors=% post=% simple=% hex=% hexid=% put=% delete=% % %/s',
          [KB(Data), count, errors, counts[encPost], counts[encSimple],
           counts[encPostHex], counts[encPostHexID], counts[encPut],
           counts[encDelete], timer.Stop, timer.PerSec(count)], self);
      end;
    end;
  except
    on E: Exception do
    begin
      if (transperrow > 0) and
         (rowcountpercurrtrans > 0) then
      begin
        for i := 0 to high(runtabletrans) do
          if runtabletrans[i] <> nil then
            runtabletrans[i].RollBack(CONST_AUTHENTICATION_NOT_USED);
        UniqueRawUtf8ZeroToTilde(Data, 1 shl 16);
        log.Log(sllWarning, '% -> PARTIAL rollback of latest auto-committed ' +
          'transaction data=%', [E, Data]);
      end;
      raise;
    end;
  end;
  if Table <> nil then
  begin
    // '{"Table":["cmd":values,...]}' format
    if info.Json = nil then
      raise EOrmBatchException.CreateUtf8(
        '%.EngineBatchSend: % Truncated', [self, Table]);
    while not (info.Json^ in ['}', #0]) do
      inc(info.Json);
    if info.Json^ <> '}' then
      raise EOrmBatchException.CreateUtf8(
        '%.EngineBatchSend(%): Missing }', [self, Table]);
  end;
  // if we reached here, process was ok
  if count = 0 then
    Results := nil
  else
    DynArrayFakeLength(Results, count);
  result := HTTP_SUCCESS;
end;

procedure TRestOrmServer.TrackChanges(const aTable: array of TOrmClass;
  aTableHistory: TOrmClass; aMaxHistoryRowBeforeBlob,
  aMaxHistoryRowPerRecord, aMaxUncompressedBlobSize: integer);
var
  t, tableindex, tablehistoryindex: PtrInt;
begin
  if (self = nil) or
     (high(aTable) < 0) then
    exit;
  if (aTableHistory = nil) or
     not aTableHistory.InheritsFrom(TOrmHistory) then
    raise EOrmException.CreateUtf8('%.TrackChanges: % is not a TOrmHistory',
      [self, aTableHistory]);
  if aMaxHistoryRowBeforeBlob <= 0 then
    // disable change tracking
    tablehistoryindex := -1
  else
  begin
    if aTableHistory = nil then
      aTableHistory := TOrmHistory;
    tablehistoryindex := fModel.GetTableIndexExisting(aTableHistory);
  end;
  for t := 0 to high(aTable) do
  begin
    tableindex := fModel.GetTableIndexExisting(aTable[t]);
    if aTable[t].InheritsFrom(TOrmHistory) then
      raise EOrmException.CreateUtf8(
        '%.TrackChanges([%]) not allowed', [self, aTable[t]]);
    if cardinal(tableindex) < cardinal(fTrackChangesHistoryTableIndexCount) then
    begin
      fTrackChangesHistoryTableIndex[tableindex] := tablehistoryindex;
      if tablehistoryindex >= 0 then
        with fTrackChangesHistory[tablehistoryindex] do
        begin
          if CurrentRow = 0 then
            CurrentRow := TableRowCount(aTableHistory);
          MaxSentDataJsonRow := aMaxHistoryRowBeforeBlob;
          MaxRevisionJson := aMaxHistoryRowPerRecord;
          MaxUncompressedBlobSize := aMaxUncompressedBlobSize;
        end;
    end;
  end;
end;

procedure TRestOrmServer.TrackChangesFlush(aTableHistory: TOrmClass);
var
  histblob: TOrmHistory;
  rec: TOrm;
  histjson: TOrmHistory;
  where, json: RawUtf8;
  histid, modifiedrec: TInt64DynArray;
  tablehistoryindex, i, histidcount, n: PtrInt;
  modifrec, modifreccount, maxrevision: integer;
  T: TOrmTable;
  {%H-}log: ISynLog;
begin
  log := fRest.LogClass.Enter('TrackChangesFlush(%)', [aTableHistory], self);
  if (aTableHistory = nil) or
     not aTableHistory.InheritsFrom(TOrmHistory) then
    raise EOrmException.CreateUtf8('%.TrackChangesFlush: % is not a TOrmHistory',
      [self, aTableHistory]);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock; // avoid race condition
  try
    // low-level Add(TOrmHistory) without cache
    tablehistoryindex := fModel.GetTableIndexExisting(aTableHistory);
    maxrevision := fTrackChangesHistory[tablehistoryindex].MaxRevisionJson;
    if maxrevision <= 0 then
      maxrevision := 10;
    // we will compress into BLOB only when we got more than 10 revisions of a record
    T := MultiFieldValues(aTableHistory, 'RowID,ModifiedRecord',
      'Event<>%', [ord(heArchiveBlob)], []);
    try
      T.GetRowValues(T.FieldIndexID, histid);
      T.GetRowValues(T.FieldIndex('ModifiedRecord'), modifiedrec);
    finally
      T.Free;
    end;
    QuickSortInt64(pointer(modifiedrec), pointer(histid),
      0, high(modifiedrec));
    modifrec := 0;
    modifreccount := 0;
    n := 0;
    histidcount := 0;
    for i := 0 to high(modifiedrec) do
    begin
      if (modifiedrec[i] = 0) or
         (histid[i] = 0) then
        raise EOrmException.CreateUtf8('%.TrackChangesFlush: Invalid %.ID=%',
          [self, aTableHistory, histid[i]]);
      if modifiedrec[i] <> modifrec then
      begin
        if modifreccount > maxrevision then
          histidcount := n
        else
          n := histidcount;
        modifrec := modifiedrec[i];
        modifreccount := 1;
      end
      else
        inc(modifreccount);
      histid[n] := histid[i];
      inc(n);
    end;
    if modifreccount > maxrevision then
      histidcount := n;
    if histidcount = 0 then
      exit; // nothing to compress
    QuickSortInt64(Pointer(histid), 0, histidcount - 1);
    where := Int64DynArrayToCsv(Pointer(histid), histidcount, 'RowID in (', ')');
    { following SQL can be very slow with external tables, and won't work
      with TRestStorageInMemory -> manual process instead
    where := FormatUtf8('ModifiedRecord in (select ModifiedRecord from ' +
      '(select ModifiedRecord, count(*) NumItems from % group by ModifiedRecord) ' +
      'where NumItems>% order by ModifiedRecord) and History is null',
      [aTableHistory.SqlTableName, maxrevision]); }
    rec := nil;
    histblob := nil;
    histjson := TOrmHistoryClass(aTableHistory).CreateAndFillPrepare(self, where);
    try
      histblob := aTableHistory.Create as TOrmHistory;
      while histjson.FillOne do
      begin
        if histjson.ModifiedRecord <> histblob.ModifiedRecord then
        begin
          if histblob.ModifiedRecord <> 0 then
            histblob.HistorySave(self, rec);
          FreeAndNil(rec);
          histblob.History := '';
          histblob.IDValue := 0;
          histblob.Event := heArchiveBlob;
          if not Retrieve('ModifiedRecord=? and Event=%',
              [ord(heArchiveBlob)], [histjson.ModifiedRecord], histblob) then
            histblob.ModifiedRecord := histjson.ModifiedRecord
          else
            RetrieveBlobFields(histblob);
          if not histblob.HistoryOpen(fModel) then
          begin
            InternalLog('Invalid %.History BLOB content for ID=%: % ' +
              'layout may have changed -> flush any previous content',
              [histblob.RecordClass, histblob.IDValue,
               histjson.ModifiedTable(fModel)], sllError);
            histblob.IDValue := 0;
          end;
          if histblob.IDValue <> 0 then
            // allow changes appending to histblob
            rec := histblob.HistoryGetLast
          else
          begin
            // histblob.fID=0 -> no previous BLOB content
            json := JsonEncode([
              'ModifiedRecord', histjson.ModifiedRecord,
              'Timestamp', GetServerTimestamp,
              'Event', ord(heArchiveBlob)]);
            if histjson.Event = heAdd then
            begin
              // allow versioning from scratch
              histblob.IDValue := EngineAdd(tablehistoryindex, json);
              rec := histjson.ModifiedTable(fModel).Create;
              histblob.HistoryOpen(fModel);
            end
            else
            begin
              rec := Retrieve(histjson.ModifiedRecord);
              if rec <> nil then
              try
                // initialize BLOB with latest revision
                histblob.IDValue := EngineAdd(tablehistoryindex, json);
                histblob.HistoryOpen(fModel);
                histblob.HistoryAdd(rec, histjson);
              finally
                FreeAndNil(rec); // ignore partial SentDataJson for this record
              end;
            end;
          end;
        end;
        if (rec = nil) or
           (histblob.IDValue = 0) then
          // only append modifications to BLOB if valid
          continue;
        rec.FillFrom(pointer(histjson.SentDataJson));
        histblob.HistoryAdd(rec, histjson);
      end;
      if histblob.ModifiedRecord <> 0 then
        histblob.HistorySave(self, rec);
      SetLength(histid, histidcount);
      EngineDeleteWhere(tablehistoryindex, where, TIDDynArray(histid));
    finally
      histjson.Free;
      histblob.Free;
      rec.Free;
    end;
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;

function TRestOrmServer.InternalUpdateEventNeeded(aEvent: TOrmEvent; 
  aTableIndex: integer): boolean;
begin
  result := (self <> nil) and
    (Assigned(OnUpdateEvent) or
     ((cardinal(aTableIndex) < cardinal(fTrackChangesHistoryTableIndexCount)) and
      (fTrackChangesHistoryTableIndex[aTableIndex] >= 0))) or
     ((aEvent = oeUpdateBlob) and
      Assigned(OnBlobUpdateEvent));
end;

procedure SetStaticTable(aTableIndex, aTableCount: integer; aStatic: TRestOrm;
  var aStatics: TRestOrmDynArray);
begin
  if (aStatic = nil) and
     (aStatics = nil) then
    // nothing to setup
    exit;
  if length(aStatics) <> aTableCount then
    SetLength(aStatics, aTableCount);
  if (aStatic <> nil) and
     (aStatics[aTableIndex] <> nil) and
     (aStatics[aTableIndex] <> aStatic) then
    raise ERestException.CreateUtf8(
      'SetStaticTable(%): existing % for %',
      [aTableIndex, aStatics[aTableIndex], aStatic]);
  if aStatic <> nil then
    IInterface(aStatic)._AddRef // manual reference counting
  else
    IInterface(aStatics[aTableIndex])._Release;
  aStatics[aTableIndex] := aStatic;
  if IsZero(pointer(aStatics), aTableCount * SizeOf(pointer)) then
    // void array if no more static
    aStatics := nil;
end;

procedure TRestOrmServer.StaticTableSetup(aTableIndex: integer;
  aStatic: TRestOrm; aKind: TRestServerKind);
var
  n: cardinal;
begin
  n := length(fModel.Tables);
  if cardinal(aTableIndex) >= n then
    raise ERestException.CreateUtf8(
      '%.StaticVirtualTableSetup(%?,%)', [self, aTableIndex, aStatic]);
  case aKind of
    sStaticDataTable:
      SetStaticTable(aTableIndex, n, aStatic, fStaticData);
    sVirtualTable:
      SetStaticTable(aTableIndex, n, aStatic, fStaticVirtualTable);
  else
    raise ERestException.CreateUtf8('%.StaticVirtualTableSetup(%,%?)',
      [self, aStatic, GetEnumName(TypeInfo(TRestServerKind), ord(aKind))^]);
  end;
end;

function TRestOrmServer.InternalAdaptSql(TableIndex: integer;
  var SQL: RawUtf8): TRestOrm;
begin
  result := nil;
  if (self <> nil) and
     (TableIndex >= 0) then
  begin
    // SQL refers to this unique table
    if cardinal(TableIndex) < cardinal(length(fStaticData)) then
      // no SQLite3 module available for fStaticData[] -> we need to
      // retrieve manualy any static table from the SQL SELECT statement
      result := fStaticData[TableIndex];
    if (result = nil) and
       fVirtualTableDirect and
       (fStaticVirtualTable <> nil) then
    begin
      result := fStaticVirtualTable[TableIndex];
      // virtual table may need adaptation (e.g. RowID -> ID)
      if result <> nil then
        if result.InheritsFrom(TRestStorage) and
           not TRestStorage(result).AdaptSqlForEngineList(SQL) then
          // complex request will use SQlite3 virtual engine module
          result := nil;
    end;
  end;
end;

function TRestOrmServer.InternalListRawUtf8(TableIndex: integer;
  const SQL: RawUtf8): RawUtf8;
var
  aSql: RawUtf8; // use a private copy for InternalAdaptSql()
  Rest: TRestOrm;
begin
  aSql := SQL;
  Rest := InternalAdaptSql(TableIndex, aSql);
  if Rest <> nil then
     // this SQL statement is handled by direct connection, faster adaptation
    result := Rest.EngineList(aSql)
  else
    // complex TOrmVirtualTableJson/External queries will rely on virtual table
    result := MainEngineList(SQL, false, nil);
  if result = '[]'#$A then
    result := '';
end;

function TRestOrmServer.InternalUpdateEvent(aEvent: TOrmEvent;
  aTableIndex: integer; aID: TID; const aSentData: RawUtf8;
  aIsBlobFields: PFieldBits; aRec: TOrm): boolean;

  procedure DoTrackChanges(TableHistoryIndex: integer);
  var
    histclass: TOrmHistoryClass;
    json, data: RawUtf8;
    event: TOrmHistoryEvent;
  begin
    case aEvent of
      oeAdd:
        event := heAdd;
      oeUpdate:
        event := heUpdate;
      oeDelete:
        event := heDelete;
    else
      exit;
    end;
    histclass := TOrmHistoryClass(fModel.Tables[TableHistoryIndex]);
    data := aSentData;
    if (data = '') and
       (aRec <> nil) then
      GetJsonValue(aRec, false, EVENT2OCCASION[aEvent], data);
    histclass.InitializeFields([
      'ModifiedRecord', aTableIndex + aID shl 6,
      'Event', ord(event),
      'SentDataJson', data,
      'Timestamp', GetServerTimestamp], json);
    fRest.AcquireExecution[execOrmWrite].Safe.Lock; // avoid race condition
    try // low-level Add(TOrmHistory) without cache
      EngineAdd(TableHistoryIndex, json);
      { TODO: use a BATCH (in background thread) to speed up TOrmHistory storage? }
      if fTrackChangesHistory[TableHistoryIndex].CurrentRow >
           fTrackChangesHistory[TableHistoryIndex].MaxSentDataJsonRow then
      begin
        // gather & compress TOrmHistory.SentDataJson into History BLOB
        TrackChangesFlush(histclass);
        fTrackChangesHistory[TableHistoryIndex].CurrentRow := 0;
      end
      else
        // fast append as json until reached MaxSentDataJsonRow
        inc(fTrackChangesHistory[TableHistoryIndex].CurrentRow);
    finally
      fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
    end;
  end;

var
  histtableindex: integer;
begin
  if aID <= 0 then
    result := false
  else if aIsBlobFields <> nil then
    // BLOB fields update
    if (aEvent = oeUpdateBlob) and
       Assigned(OnBlobUpdateEvent) then
      result := OnBlobUpdateEvent(Owner, oeUpdate,
        fModel.Tables[aTableIndex], aID, aIsBlobFields^)
    else
      result := true
  else
  begin
    // track simple fields modification
    if cardinal(aTableIndex) < cardinal(fTrackChangesHistoryTableIndexCount) then
    begin
      histtableindex := fTrackChangesHistoryTableIndex[aTableIndex];
      if histtableindex >= 0 then
        DoTrackChanges(histtableindex);
    end;
    if Assigned(OnUpdateEvent) then
      result := OnUpdateEvent(Owner, aEvent,
        fModel.Tables[aTableIndex], aID, aSentData)
    else
      result := true; // true on success, false if error (but action continues)
  end;
end;

function TRestOrmServer.AfterDeleteForceCoherency(aTableIndex: integer;
  aID: TID): boolean;

  procedure PerformCascade(const Where: Int64; Ref: POrmModelReference);
  var
    W: RawUtf8;
    cascadeOK: boolean;
    rest: TRestOrm;
  begin
    // set Field=0 or delete row where Field references aID
    if Where = 0 then
      exit;
    Int64ToUtf8(Where, W);
    if Ref^.CascadeDelete then
      cascadeOK := Delete(fModel.Tables[Ref^.TableIndex],
        Ref^.FieldType.Name + '=:(' + W + '):')
    else
    begin
      rest := GetStaticTableIndex(Ref^.TableIndex);
      if rest <> nil then
        // fast direct call
        cascadeOK := rest.EngineUpdateField(Ref^.TableIndex,
          Ref^.FieldType.Name, '0', Ref^.FieldType.Name, W)
      else
        cascadeOK := MainEngineUpdateField(Ref^.TableIndex,
          Ref^.FieldType.Name, '0', Ref^.FieldType.Name, W);
    end;
    if not cascadeOK then
      InternalLog('AfterDeleteForceCoherency() failed update %.%=%',
        [fModel.Tables[Ref^.TableIndex], Ref^.FieldType.Name, W], sllWarning);
  end;

var
  i: integer;
  ref: POrmModelReference;
begin
  ref := pointer(fModel.RecordReferences);
  if ref <> nil then
  begin
    for i := 1 to length(fModel.RecordReferences) do
    begin
      if ref^.FieldTableIndex = -2 then
        // lazy initialization
        ref^.FieldTableIndex := fModel.GetTableIndexSafe(ref^.FieldTable, false);
      case ref^.FieldType.OrmFieldType of
        oftRecord:
          // TRecordReference published field
          PerformCascade(RecordReference(aTableIndex, aID), ref);
        oftID:
          // TOrm published field
          if ref^.FieldTableIndex = aTableIndex then
            PerformCascade(aID, ref);
        oftTID:
          // TTableID = type TID published field
          if ref^.FieldTableIndex = aTableIndex then
            PerformCascade(aID, ref);
      end;
      inc(ref);
    end;
  end;
  result := true; // success even if no match found, or some cascade warnings
end;

procedure TRestOrmServer.FlushInternalDBCache;
begin
  // do nothing by default
end;

procedure TRestOrmServer.RefreshInternalStateFromStatic;
var
  i: PtrInt;
begin
  for i := 0 to high(fStaticData) do
    if (fStaticData[i] <> nil) and
        fStaticData[i].InheritsFrom(TRestStorage) then
      if TRestStorage(fStaticData[i]).RefreshedAndModified then
      begin
        // force refresh
        inc(InternalState);
        break;
      end;
end;

{ IRestOrmServer overriden methods }

function TRestOrmServer.CreateSqlIndex(Table: TOrmClass;
  const FieldName: RawUtf8; Unique: boolean; const IndexName: RawUtf8): boolean;
begin
  result := CreateSqlMultiIndex(Table, [FieldName], Unique, IndexName);
end;

function TRestOrmServer.CreateSqlIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to high(FieldNames) do
    if not CreateSqlMultiIndex(Table, [FieldNames[i]], Unique) then
      result := false;
end;

function TRestOrmServer.CreateSqlMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean;
  IndexName: RawUtf8): boolean;
var
  sql: RawUtf8;
  i, tableindex: PtrInt;
  props: TOrmProperties;
  rest: TRestOrm;
begin
  result := false;
  if high(FieldNames) < 0 then
    // avoid endless loop for TRestStorage with no overridden method
    exit;
  tableindex := fModel.GetTableIndexExisting(Table);
  rest := nil;
  if tableindex >= 0 then
  begin
    // bypass fVirtualTableDirect
    if PtrUInt(tableindex) < PtrUInt(length(fStaticData)) then
      rest := fStaticData[tableindex];
    if (rest = nil) and
       (fStaticVirtualTable <> nil) then
      rest := fStaticVirtualTable[tableindex];
  end;
  if (rest <> nil) and
     rest.InheritsFrom(TRestStorage) then
  begin
    // create the index on the static table (e.g. for external DB)
    result := TRestStorage(rest).CreateSqlMultiIndex(
      Table, FieldNames, Unique, IndexName);
    exit;
  end;
  if (high(FieldNames) = 0) and
     IsRowID(pointer(FieldNames[0])) then
  begin
    // SQLite3 has always its ID/RowID primary key indexed
    result := true;
    exit;
  end;
  props := fModel.TableProps[tableindex].props;
  for i := 0 to high(FieldNames) do
    if not IsRowID(pointer(FieldNames[i])) then
      if props.Fields.IndexByName(FieldNames[i]) < 0 then
        // wrong field name
        exit;
  if Unique then
    sql := 'UNIQUE '
  else
    sql := '';
  if IndexName = '' then
  begin
    IndexName := RawUtf8ArrayToCsv(FieldNames, '');
    if length(IndexName) + length(props.SqlTableName) > 64 then
      // avoid reaching potential identifier name size limit
      IndexName := crc32cUtf8ToHex(props.SqlTableName) +
                   crc32cUtf8ToHex(IndexName);
  end;
  sql := FormatUtf8('CREATE %INDEX IF NOT EXISTS Index%% ON %(%);',
    [sql, props.SqlTableName, IndexName, props.SqlTableName,
     RawUtf8ArrayToCsv(FieldNames, ',')]);
  result := EngineExecute(sql);
end;

function TRestOrmServer.IsInternalSQLite3Table(aTableIndex: integer): boolean;
begin
  result := ((cardinal(aTableIndex) >= cardinal(length(fStaticData))) or
             (fStaticData[aTableIndex] = nil)) and
            ((cardinal(aTableIndex) >= cardinal(length(fStaticVirtualTable))) or
             (fStaticVirtualTable[aTableIndex] = nil));
end;

function TRestOrmServer.HandleAuthentication: boolean;
begin
  if fOwner = nil then
    result := false
  else
    // the main TRestServer is responsible of sessions and authentication
    result := fOwner.HandleAuthentication;
end;

procedure TRestOrmServer.SetStaticVirtualTableDirect(direct: boolean);
begin
  fVirtualTableDirect := direct;
end;


{ IRestOrm overriden methods }

function TRestOrmServer.UnLock(Table: TOrmClass; aID: TID): boolean;
begin
  result := Model.UnLock(Table, aID);
end;

procedure TRestOrmServer.Commit(SessionID: cardinal; RaiseException: boolean);
var
  i: PtrInt;
begin
  inherited Commit(SessionID, RaiseException);
  if self <> nil then
    for i := 0 to high(fStaticVirtualTable) do
      if fStaticVirtualTable[i] <> nil then
        with TRestStorageInMemory(fStaticVirtualTable[i]) do
          if InheritsFrom(TRestStorageInMemory) and
             not CommitShouldNotUpdateFile then
            UpdateFile; // will do nothing if not Modified
end;

function TRestOrmServer.Delete(Table: TOrmClass; ID: TID): boolean;
begin
  result := inherited Delete(Table, ID); // call EngineDelete
  if result then
    // force relational database coherency (i.e. our FOREIGN KEY implementation)
    AfterDeleteForceCoherency(Model.GetTableIndex(Table), ID);
end;

function TRestOrmServer.Delete(Table: TOrmClass;
  const SqlWhere: RawUtf8): boolean;
var
  ids: TIDDynArray;
  tableindex, i: PtrInt;
begin
  result := InternalDeleteNotifyAndGetIDs(Table, SqlWhere, ids);
  if (ids = nil) or
     not result then
    // nothing to delete
    exit;
  tableindex := Model.GetTableIndexExisting(Table);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    // may be within a batch in another thread
    result := EngineDeleteWhere(tableindex, SqlWhere, ids);
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.Unlock;
  end;
  if result then
    // force relational database coherency (i.e. our FOREIGN KEY implementation)
    for i := 0 to high(ids) do
      AfterDeleteForceCoherency(tableindex, ids[i]);
end;

function TRestOrmServer.TableRowCount(Table: TOrmClass): Int64;
var
  rest: TRestOrm;
begin
  rest := GetStaticTable(Table);
  if rest <> nil then
    // faster direct call
    result := rest.TableRowCount(Table)
  else
    result := inherited TableRowCount(Table);
end;

function TRestOrmServer.TableHasRows(Table: TOrmClass): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTable(Table);
  if rest <> nil then
    // faster direct call
    result := rest.TableHasRows(Table)
  else
    result := inherited TableHasRows(Table);
end;

function TRestOrmServer.MemberExists(Table: TOrmClass; ID: TID): boolean;
var
  rest: TRestOrm;
begin
  rest := GetStaticTable(Table);
  if rest <> nil then
    // faster direct call (External, MongoDB, IsMemory)
    result := rest.MemberExists(Table, ID)
  else
    result := inherited MemberExists(Table, ID);
end;

function TRestOrmServer.UpdateBlobFields(Value: TOrm): boolean;
var
  rest: TRestOrm;
begin
  // overridden method to update all BLOB fields at once
  if (Value = nil) or
     (Value.IDValue <= 0) then
    result := false
  else
  begin
    rest := GetStaticTable(POrmClass(Value)^);
    if rest <> nil then
      // faster direct call
      result := rest.UpdateBlobFields(Value)
    else
      result := inherited UpdateBlobFields(Value);
  end;
end;

function TRestOrmServer.RetrieveBlobFields(Value: TOrm): boolean;
var
  rest: TRestOrm;
begin
  // overridden method to update all BLOB fields at once
  if Value = nil then
    result := false
  else
  begin
    rest := GetStaticTable(POrmClass(Value)^);
    if rest <> nil then
      // faster direct call
      result := rest.RetrieveBlobFields(Value)
    else
      result := inherited RetrieveBlobFields(Value);
  end;
end;


end.

