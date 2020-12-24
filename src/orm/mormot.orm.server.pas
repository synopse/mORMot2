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
  mormot.core.crypto,
  mormot.core.jwt,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
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
  // - works in conjunction with TRestClientURI from mormot.rest.client.pas
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
    fTrackChangesHistoryTableIndexCount: cardinal;
    fTrackChangesHistory: array of record
      CurrentRow: integer;
      MaxSentDataJsonRow: integer;
      MaxRevisionJSON: integer;
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
      const SentData: RawUTF8): TID; override;
    function EngineRetrieve(TableModelIndex: integer;
      ID: TID): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer; const SetFieldName,
      SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    function EngineBatchSend(Table: TOrmClass; var Data: RawUTF8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; override;
  public
    /// virtual abstract methods which will perform CRUD operations on the main DB
    function MainEngineAdd(TableModelIndex: integer;
      const SentData: RawUTF8): TID; virtual; abstract;
    function MainEngineRetrieve(TableModelIndex: integer;
      ID: TID): RawUTF8; virtual; abstract;
    function MainEngineList(const SQL: RawUTF8; ForceAJAX: boolean;
      ReturnedRowCount: PPtrInt): RawUTF8; virtual; abstract;
    function MainEngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; virtual; abstract;
    function MainEngineDelete(TableModelIndex: integer;
      ID: TID): boolean; virtual; abstract;
    function MainEngineDeleteWhere(TableModelIndex: integer;
      const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean; virtual; abstract;
    function MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; virtual; abstract;
    function MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; virtual; abstract;
    function MainEngineUpdateField(TableModelIndex: integer; const SetFieldName,
        SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; virtual; abstract;
    function MainEngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; virtual; abstract;
    /// this method is overridden for setting the NoAJAXJSON field
    // of all associated TRestStorage servers
    procedure SetNoAJAXJSON(const Value: boolean); virtual;
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
    // clients ask the server for refresh (see TRestClientURI.UpdateFromServer)
    OnUpdateEvent: TOnOrmEvent;
    /// a method can be specified here to trigger events after any blob update
    // - is called AFTER update of one or several blobs, never on delete nor insert
    // - to be used only server-side, not to synchronize some clients: the framework
    // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
    // clients ask the server for refresh (see TRestClientURI.UpdateFromServer)
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
    // - you must call explicitely this before having called StaticDataCreate()
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
    function InternalAdaptSQL(TableIndex: integer; var SQL: RawUTF8): TRestOrm;
    /// retrieve a list of members as JSON encoded data
    // - used by OneFieldValue() and MultiFieldValue() methods
    function InternalListRawUTF8(TableIndex: integer; const SQL: RawUTF8): RawUTF8;
    /// virtual method called when a record is updated
    // - default implementation will call the OnUpdateEvent/OnBlobUpdateEvent
    // methods, if defined
    // - will also handle TOrmHistory tables, as defined by TrackChanges()
    // - returns true on success, false if an error occured (but action must continue)
    // - you can override this method to implement a server-wide notification,
    // but be aware it may be the first step to break the stateless architecture
    // of the framework
    function InternalUpdateEvent(aEvent: TOrmEvent; aTableIndex: integer; aID: TID;
      const aSentData: RawUTF8; aIsBlobFields: PFieldBits): boolean; virtual;
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
    // - this default implementation will just do nothing, but mORMotSQlite3
    // unit will call TSQLDataBase.CacheFlush method
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
      aRemoteRest: TRestOrm): TRestOrm; virtual;
    /// fast get the associated TRestStorageRemote from its index, if any
    // - returns nil if aTableIndex is invalid or is not assigned to a TRestStorageRemote
    function GetRemoteTable(TableIndex: integer): TRestOrm;
    /// initialize change tracking for the given tables
    // - by default, it will use the TOrmHistory table to store the
    // changes - you can specify a dedicated class as aTableHistory parameter
    // - if aTableHistory is not already part of the TOrmModel, it will be added
    // - note that this setting should be consistent in time: if you disable
    // tracking for a while, or did not enable tracking before adding a record,
    // then the content history won't be consistent (or disabled) for this record
    // - at every change, aTableHistory.SentDataJSON records will be added, up
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
      aTableHistory: TOrmHistoryClass = nil;
      aMaxHistoryRowBeforeBlob: integer = 1000;
      aMaxHistoryRowPerRecord: integer = 10;
      aMaxUncompressedBlobSize: integer = 64*1024); virtual;
    /// force compression of all aTableHistory.SentDataJson into History BLOB
    // - by default, this will take place in InternalUpdateEvent() when
    // aMaxHistoryRowBeforeBlob - as set by TrackChanges() method - is reached
    // - you can manually call this method to force History BLOB update, e.g.
    // when the server is in Idle state, and ready for process
    procedure TrackChangesFlush(aTableHistory: TOrmHistoryClass); virtual;
    /// check if OnUpdateEvent or change tracked has been defined for this table
    // - is used internally e.g. by TRestServerDB.MainEngineUpdateField to
    // ensure that the updated ID fields will be computed as expected
    function InternalUpdateEventNeeded(aTableIndex: integer): boolean;
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
      Master: TRest; ChunkRowLimit: integer = 0;
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
      Master: TRest; var RecordVersion: TRecordVersion; MaxRowLimit: integer = 0;
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
    function Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean; override;
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
    // - write all pending TOrmVirtualTableJSON data to the disk
    procedure Commit(SessionID: cardinal; RaiseException: boolean); override;
  public
    { IRestOrmServer methods }
    /// create an index for the specific FieldName
    // - will call CreateSQLMultiIndex() internaly
    function CreateSQLIndex(Table: TOrmClass;
      const FieldName: RawUTF8; Unique: boolean;
      const IndexName: RawUTF8 = ''): boolean; overload;
    /// create one or multiple index(es) for the specific FieldName(s)
    function CreateSQLIndex(Table: TOrmClass;
      const FieldNames: array of RawUTF8; Unique: boolean): boolean; overload;
    /// create one index for all specific FieldNames at once
    // - will call any static engine for the index creation of such tables, or
    // execute a CREATE INDEX IF NOT EXISTS on the main engine
    // - note that with SQLite3, your database schema should never contain two
    // indices where one index is a prefix of the other, e.g. if you defined:
    // ! aServer.CreateSQLMultiIndex(TEmails, ['Email','GroupID'], True);
    // Then the following index is not mandatory for SQLite3:
    // ! aServer.CreateSQLIndex(TEmails, 'Email', False);
    // see "1.6 Multi-Column Indices" in @http://www.sqlite.org/queryplanner.html
    function CreateSQLMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8 = ''): boolean; virtual;
    /// check if the supplied TOrm is not a virtual or static table
    function IsInternalSQLite3Table(aTableIndex: integer): boolean;
    /// returns true if the server will handle per-user authentication and
    // access right management
    function HandleAuthentication: boolean;
  published
    /// this property can be left to its TRUE default value, to handle any
    // TOrmVirtualTableJSON static tables (module JSON or BINARY) with direct
    // calls to the storage instance
    // - is set to TRUE by default to enable faster Direct mode
    // - in Direct mode, GET/POST/PUT/DELETE of individual records (or BLOB fields)
    // from URI() will call directly the corresponding TRestStorage
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
  fOwner := aRest as TRestServer;
  // set fRest+fModel
  inherited Create(aRest);
  // faster direct Static call by default
  fVirtualTableDirect := true;
  // initialize TrackChanges() associated tables
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
    if fModel.Tables[t].RecordProps.RecordVersionField <> nil then
    begin
      fOrmVersionDeleteTable := fModel.AddTableInherited(TOrmTableDeleted);
      break;
    end;
end;

destructor TRestOrmServer.Destroy;
var
  i: PtrInt;
begin
  // free all virtual TRestStorage instances
  for i := 0 to high(fStaticVirtualTable) do
    if fStaticVirtualTable[i] <> nil then
    begin
      if fStaticVirtualTable[i].RefCount <> 1 then
        raise ERestStorage.CreateUTF8('%.Destroy: static virtual % refcnt=%',
          [self, fStaticVirtualTable[i], fStaticVirtualTable[i].RefCount]);
      TRestOrmServer(fStaticVirtualTable[i])._Release;
      if fStaticData <> nil then
        // free once as fStaticVirtualTable[i], just clear reference here
        fStaticData[i] := nil;
    end;
  // free lasting TRestStorage instances and update file if necessary
  for i := 0 to high(fStaticData) do
    if fStaticData[i] <> nil then
      if fStaticData[i].RefCount <> 1 then
        raise ERestStorage.CreateUTF8('%.Destroy: static % refcnt=%',
          [self, fStaticData[i], fStaticData[i].RefCount])
      else
        TRestOrmServer(fStaticData[i])._Release;
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

procedure TRestOrmServer.SetNoAJAXJSON(const Value: boolean);
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

function TRestOrmServer.RemoteDataCreate(aClass: TOrmClass;
  aRemoteRest: TRestOrm): TRestOrm;
var
  t: integer;
  existing: TRestOrm;
begin
  t := Model.GetTableIndexExisting(aClass);
  existing := GetStaticTableIndex(t);
  if existing <> nil then
    raise ERestStorage.CreateUTF8('Duplicated %.RemoteDataCreate(%) as %',
      [self, aClass, existing]);
  result := TRestStorageRemote.Create(aClass, self, aRemoteRest);
  StaticTableSetup(t, result, sStaticDataTable);
end;

function TRestOrmServer.GetRemoteTable(TableIndex: integer): TRestOrm;
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
  field: TOrmPropInfoRTTIRecordVersion;
  current, max, mDeleted: Int64;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    if fRecordVersionMax = 0 then // check twice to avoid race condition
    begin
      current := 0;
      for m := 0 to fModel.TablesMax do
      begin
        field := fModel.Tables[m].RecordProps.RecordVersionField;
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
    if fOwner.Services <> nil then
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
    raise EOrmException.CreateUTF8(
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
  Table: TOrmClass; Master: TRest; ChunkRowLimit: integer;
  const OnWrite: TOnBatchWrite): TRecordVersion;
var
  Writer: TRestBatch;
  IDs: TIDDynArray;
  status: integer;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fRest.LogClass.Enter('RecordVersionSynchronizeSlave %', [Table], self);
  result := -1; // error
  if fRecordVersionMax = 0 then
    InternalRecordVersionMaxFromExisting(nil);
  repeat
    Writer := RecordVersionSynchronizeSlaveToBatch(Table, Master,
      fRecordVersionMax, ChunkRowLimit, OnWrite);
    if Writer = nil then
      // error
      exit;
    if Writer.Count = 0 then
    begin
      // nothing new (e.g. reached last chunk)
      result := fRecordVersionMax;
      Writer.Free;
      break;
    end;
    try
      fRest.AcquireExecution[execOrmWrite].Safe.Lock;
      fRecordVersionDeleteIgnore := true;
      status := BatchSend(Writer, IDs);
      if status = HTTP_SUCCESS then
      begin
        InternalLog(
          'RecordVersionSynchronize(%) Added=% Updated=% Deleted=% on %',
          [Table, Writer.AddCount, Writer.UpdateCount, Writer.DeleteCount,
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
      Writer.Free;
    end;
  until false; // continue synch until nothing new is found
end;

function TRestOrmServer.RecordVersionSynchronizeSlaveToBatch(
  Table: TOrmClass; Master: TRest; var RecordVersion: TRecordVersion;
  MaxRowLimit: integer; const OnWrite: TOnBatchWrite): TRestBatch;
var
  TableIndex, SourceTableIndex, UpdatedRow, DeletedRow: integer;
  Props: TOrmProperties;
  Where: RawUTF8;
  UpdatedVersion, DeletedVersion: TRecordVersion;
  ListUpdated, ListDeleted: TOrmTable;
  Rec: TOrm;
  DeletedMinID: TID;
  Deleted: TOrmTableDeleted;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fRest.LogClass.Enter('RecordVersionSynchronizeSlaveToBatch %',
    [Table], self);
  result := nil;
  if Master = nil then
    raise EOrmException.CreateUTF8(
      '%.RecordVersionSynchronizeSlaveToBatch(Master=nil)', [self]);
  TableIndex := Model.GetTableIndexExisting(Table);
  SourceTableIndex := Master.Model.GetTableIndexExisting(Table); // <>TableIndex?
  Props := Model.TableProps[TableIndex].Props;
  if Props.RecordVersionField = nil then
    raise EOrmException.CreateUTF8(
      '%.RecordVersionSynchronizeSlaveToBatch(%) with no TRecordVersion field',
      [self, Table]);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    Where := '%>? order by %';
    if MaxRowLimit > 0 then
      Where := FormatUTF8('% limit %', [Where, MaxRowLimit]);
    ListUpdated := Master.ORM.MultiFieldValues(Table, '*', Where,
      [Props.RecordVersionField.Name, Props.RecordVersionField.Name],
      [RecordVersion]);
    if ListUpdated = nil then
      exit; // DB error
    ListDeleted := nil;
    try
      DeletedMinID :=
        Int64(SourceTableIndex) shl ORMVERSION_DELETEID_SHIFT;
      Where := 'ID>? and ID<? order by ID';
      if MaxRowLimit > 0 then
        Where := FormatUTF8('% limit %', [Where, MaxRowLimit]);
      ListDeleted := Master.ORM.MultiFieldValues(fOrmVersionDeleteTable,
        'ID,Deleted', Where, [DeletedMinID + RecordVersion,
         DeletedMinID + ORMVERSION_DELETEID_RANGE]);
      if ListDeleted = nil then
        exit; // DB error
      result := TRestBatch.Create(self, nil, 10000);
      result.OnWrite := OnWrite;
      if (ListUpdated.RowCount = 0) and
         (ListDeleted.RowCount = 0) then
        // nothing new -> returns void TRestBach with Count=0
        exit;
      Rec := Table.Create;
      Deleted := fOrmVersionDeleteTable.Create;
      try
        Rec.FillPrepare(ListUpdated);
        Deleted.FillPrepare(ListDeleted);
        UpdatedRow := 1;
        DeletedRow := 1;
        UpdatedVersion := 0;
        DeletedVersion := 0;
        repeat
          // compute all changes in increasing version order
          if UpdatedVersion = 0 then
            if UpdatedRow <= ListUpdated.RowCount then
            begin
              Rec.FillRow(UpdatedRow);
              UpdatedVersion := Props.RecordVersionField.PropInfo.GetInt64Prop(Rec);
              inc(UpdatedRow);
            end;
          if DeletedVersion = 0 then
            if DeletedRow <= ListDeleted.RowCount then
            begin
              Deleted.FillRow(DeletedRow);
              DeletedVersion := Deleted.IDValue and
                pred(ORMVERSION_DELETEID_RANGE);
              inc(DeletedRow);
            end;
          if (UpdatedVersion = 0) and
             (DeletedVersion = 0) then
            break; // no more update available
          if (UpdatedVersion > 0) and
             ((DeletedVersion = 0) or
              (UpdatedVersion < DeletedVersion)) then
          begin
            if (RecordVersion = 0) or
               (OneFieldValue(Table, 'ID', Rec.IDValue) = '') then
              result.Add(Rec, true, true, Rec.FillContext.TableMapFields, true)
            else
              result.Update(Rec, [], true);
            RecordVersion := UpdatedVersion;
            UpdatedVersion := 0;
          end
          else if DeletedVersion > 0 then
          begin
            result.Delete(Table, Deleted.Deleted);
            Deleted.IDValue := DeletedVersion + // local ID follows current Model
              Int64(TableIndex) shl ORMVERSION_DELETEID_SHIFT;
            result.Add(Deleted, true, true, [], true);
            RecordVersion := DeletedVersion;
            DeletedVersion := 0;
          end;
        until false;
      finally
        Deleted.Free;
        Rec.Free;
      end;
    finally
      ListUpdated.Free;
      ListDeleted.Free;
    end;
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;


{ overridden methods which will perform CRUD operations }

function TRestOrmServer.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
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
  ID: TID): RawUTF8;
var
  rest: TRestOrm;
begin
  rest := GetStaticTableIndex(TableModelIndex);
  if rest = nil then
    result := MainEngineRetrieve(TableModelIndex, ID)
  else
    result := rest.EngineRetrieve(TableModelIndex, ID);
end;

function TRestOrmServer.EngineList(const SQL: RawUTF8; ForceAJAX: boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
var
  rest: TRestOrm;
  sqladapted: RawUTF8;
begin
  sqladapted := SQL;
  rest := InternalAdaptSQL(fModel.GetTableIndexFromSQLSelect(SQL, false), sqladapted);
  if rest = nil then
    result := MainEngineList(SQL, ForceAJAX, ReturnedRowCount)
  else
    result := rest.EngineList(sqladapted, ForceAJAX, ReturnedRowCount);
end;

function TRestOrmServer.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUTF8): boolean;
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
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
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
        result := MainEngineDeleteWhere(TableModelIndex, SQLWhere, IDs)
      else
        result := rest.EngineDeleteWhere(TableModelIndex, SQLWhere, IDs);
      if (fModel.TableProps[TableModelIndex].Props.RecordVersionField = nil) or
         not result then
        exit;
      batch := TRestBatch.Create(self, fModel.Tables[TableModelIndex], 1000);
      try
        for i := 0 to high(IDs) do
          InternalRecordVersionDelete(TableModelIndex, IDs[i], batch);
        BatchSend(batch); // allow faster deletion for engines allowing it
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
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
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
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
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
  var Data: RawUTF8; var Results: TIDDynArray;
  ExpectedResultsCount: integer): integer;
var
  EndOfObject: AnsiChar;
  wasString, OK: boolean;
  TableName, Value, ErrMsg: RawUTF8;
  URIMethod, RunningBatchURIMethod: TURIMethod;
  RunningBatchRest, RunningRest: TRestOrm;
  Sent, Method, MethodTable: PUTF8Char;
  AutomaticTransactionPerRow: cardinal;
  RowCountForCurrentTransaction: cardinal;
  RunTableTransactions: array of TRestOrm;
  RunMainTransaction: boolean;
  ID: TID;
  Count: integer;
  timeoutTix: Int64;
  batchOptions: TRestBatchOptions;
  RunTable, RunningBatchTable: TOrmClass;
  RunTableIndex, i, TableIndex: integer;
  RunStatic: TRestOrm;
  RunStaticKind: TRestServerKind;
  CurrentContext: TRestServerURIContext;
  counts: array[mPOST..mDELETE] of cardinal;

  procedure PerformAutomaticCommit;
  var
    i: PtrInt;
  begin
    if RunningBatchRest <> nil then
    begin
      RunningBatchRest.InternalBatchStop; // send pending rows before commit
      RunningBatchRest := nil;
      RunningBatchTable := nil;
    end;
    for i := 0 to high(RunTableTransactions) do
      if RunTableTransactions[i] <> nil then
      begin
        RunTableTransactions[i].Commit(CONST_AUTHENTICATION_NOT_USED, true);
        if RunTableTransactions[i] = self then
          RunMainTransaction := false;
        RunTableTransactions[i] := nil; // to acquire and begin a new transaction
      end;
    RowCountForCurrentTransaction := 0;
  end;

  function IsNotAllowed: boolean; {$ifdef HASINLINE} inline; {$endif}
  begin
    result := (CurrentContext <> nil) and
              (CurrentContext.Command = execOrmWrite) and
              not CurrentContext.CanExecuteORMWrite(URIMethod, RunTable,
                RunTableIndex, ID, CurrentContext.Call.RestAccessRights^);
  end;

var
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fRest.LogClass.Enter('EngineBatchSend % inlen=%',
    [Table, length(Data)], self);
  Sent := UniqueRawUTF8(Data); // parsed, therefore modified in-placed
  if Sent = nil then
    raise EOrmBatchException.CreateUTF8(
      '%.EngineBatchSend(%,"")', [self, Table]);
  if Table <> nil then
  begin
    TableIndex := fModel.GetTableIndexExisting(Table);
    // unserialize expected sequence array as '{"Table":["cmd",values,...]}'
    if not NextNotSpaceCharIs(Sent, '{') then
      raise EOrmBatchException.CreateUTF8('%.EngineBatchSend: Missing {', [self]);
    TableName := GetJSONPropName(Sent);
    if (TableName = '') or
       (Sent = nil) or
       not IdemPropNameU(TableName,
         fModel.TableProps[TableIndex].Props.SQLTableName) then
      raise EOrmBatchException.CreateUTF8(
        '%.EngineBatchSend(%): Wrong "Table":"%"', [self, Table, TableName]);
  end
  else
    // or '["cmd@Table":values,...]'
    TableIndex := -1;
  if not NextNotSpaceCharIs(Sent, '[') then
    raise EOrmBatchException.CreateUTF8(
      '%.EngineBatchSend: Missing [', [self]);
  if IdemPChar(Sent, '"AUTOMATICTRANSACTIONPERROW",') then
  begin
    inc(Sent, 29);
    AutomaticTransactionPerRow := GetNextItemCardinal(Sent, ',');
  end
  else
    AutomaticTransactionPerRow := 0;
  SetLength(RunTableTransactions, fModel.TablesMax + 1);
  RunMainTransaction := false;
  RowCountForCurrentTransaction := 0;
  if IdemPChar(Sent, '"OPTIONS",') then
  begin
    inc(Sent, 10);
    byte(batchOptions) := GetNextItemCardinal(Sent, ',');
  end
  else
    byte(batchOptions) := 0;
  CurrentContext := ServiceRunningContext^.Request;
  MethodTable := nil;
  RunningBatchRest := nil;
  RunningBatchTable := nil;
  RunningBatchURIMethod := mNone;
  Count := 0;
  FillCharFast(counts, SizeOf(counts), 0);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock; // multi thread protection
  try // to protect automatic transactions and global write lock
    try // to protect InternalBatchStart/Stop locking
      // main loop: process one POST/PUT/DELETE per iteration
      repeat
        // retrieve method name and associated (static) table
        Method := GetJSONField(Sent, Sent, @wasString);
        if (Sent = nil) or
           (Method = nil) or
           not wasString then
          raise EOrmBatchException.CreateUTF8(
            '%.EngineBatchSend: Missing CMD', [self]);
        MethodTable := PosChar(Method, '@');
        if MethodTable = nil then
        begin
          // e.g. '{"Table":[...,"POST",{object},...]}'
          if TableIndex < 0 then
            raise EOrmBatchException.CreateUTF8(
              '%.EngineBatchSend: "..@Table" expected', [self]);
          RunTableIndex := TableIndex;
          RunTable := Table;
        end
        else
        begin
          // e.g. '[...,"POST@Table",{object},...]'
          RunTableIndex := fModel.GetTableIndexPtr(MethodTable + 1);
          if RunTableIndex < 0 then
            raise EOrmBatchException.CreateUTF8(
              '%.EngineBatchSend: Unknown %', [self, MethodTable]);
          RunTable := fModel.Tables[RunTableIndex];
        end;
        RunStatic := GetStaticTableIndex(RunTableIndex, RunStaticKind);
        if RunStatic = nil then
          RunningRest := self
        else
          RunningRest := RunStatic;
        // get CRUD method and associated Value/ID
        case IdemPCharArray(Method, ['POST', 'PUT', 'DELETE', 'SIMPLE']) of
          // IdemPCharArray() ignores '@' char if appended after method name
          0:
            begin
              // '{"Table":[...,"POST",{object},...]}'
              // or '[...,"POST@Table",{object},...]'
              URIMethod := mPOST;
              Value := JSONGetObject(Sent, @ID, EndOfObject, true);
              if Sent = nil then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: Wrong POST', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: POST/Add not allowed on %',
                  [self, RunTable]);
              if not RecordCanBeUpdated(RunTable, ID, oeAdd, @ErrMsg) then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: POST impossible: %', [self, ErrMsg]);
            end;
          1:
            begin
              // '{"Table":[...,"PUT",{object},...]}'
              // or '[...,"PUT@Table",{object},...]'
              URIMethod := mPUT;
              Value := JSONGetObject(Sent, @ID, EndOfObject, false);
              if (Sent = nil) or
                 (Value = '') or
                 (ID <= 0) then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: Wrong PUT', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: PUT/Update not allowed on %',
                  [self, RunTable]);
            end;
          2:
            begin
              // '{"Table":[...,"DELETE",ID,...]}'
              // or '[...,"DELETE@Table",ID,...]'
              URIMethod := mDELETE;
              ID := GetInt64(GetJSONField(Sent, Sent, @wasString, @EndOfObject));
              if (ID <= 0) or
                 wasString then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: Wrong DELETE', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: DELETE not allowed on %',
                  [self, RunTable]);
              if not RecordCanBeUpdated(RunTable, ID, oeDelete, @ErrMsg) then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: DELETE impossible [%]', [self, ErrMsg]);
            end;
          3:
            begin
              // '{"Table":[...,"SIMPLE",[values],...]}'
              // or '[...,"SIMPLE@Table",[values],...]'
              URIMethod := mPOST;
              Value := fModel.TableProps[RunTableIndex].Props.
                SaveSimpleFieldsFromJsonArray(Sent, EndOfObject, true);
              ID := 0; // no ID is never transmitted with simple fields
              if (Sent = nil) or
                 (Value = '') then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: Wrong SIMPLE', [self]);
              if IsNotAllowed then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: SIMPLE/Add not allowed on %',
                  [self, RunTable]);
              if not RecordCanBeUpdated(RunTable, 0, oeAdd, @ErrMsg) then
                raise EOrmBatchException.CreateUTF8(
                  '%.EngineBatchSend: SIMPLE/Add impossible: %', [self, ErrMsg]);
            end;
        else
          raise EOrmBatchException.CreateUTF8(
            '%.EngineBatchSend: Unknown [%] method', [self, Method]);
        end;
        if (Count = 0) and
           (EndOfObject = ']') then
        begin
          // single operation do not need a transaction nor InternalBatchStart/Stop
          AutomaticTransactionPerRow := 0;
          SetLength(Results, 1);
        end
        else
        begin
          // handle auto-committed transaction process
          if AutomaticTransactionPerRow > 0 then
          begin
            if RowCountForCurrentTransaction = AutomaticTransactionPerRow then
              // reached AutomaticTransactionPerRow chunk
              PerformAutomaticCommit;
            inc(RowCountForCurrentTransaction);
            if RunTableTransactions[RunTableIndex] = nil then
              // initiate transaction for this table if not started yet
              if (RunStatic <> nil) or
                 not RunMainTransaction then
              begin
                timeoutTix := GetTickCount64 + 2000;
                repeat
                  if RunningRest.TransactionBegin(
                    RunTable, CONST_AUTHENTICATION_NOT_USED) then
                  begin
                    // acquired transaction
                    RunTableTransactions[RunTableIndex] := RunningRest;
                    if RunStatic = nil then
                      RunMainTransaction := true;
                    Break;
                  end;
                  if GetTickCount64 > timeoutTix then
                    raise EOrmBatchException.CreateUTF8(
                      '%.EngineBatchSend: %.TransactionBegin timeout',
                      [self, RunningRest]);
                  SleepHiRes(1); // retry in 1 ms
                until fOwner.ShutdownRequested;
              end;
          end;
          // handle batch pending request sending (if table or method changed)
          if (RunningBatchRest <> nil) and
             ((RunTable <> RunningBatchTable) or
              (RunningBatchURIMethod <> URIMethod)) then
          begin
            RunningBatchRest.InternalBatchStop; // send pending statements
            RunningBatchRest := nil;
            RunningBatchTable := nil;
          end;
          if (RunStatic <> nil) and
             (RunStatic <> RunningBatchRest) and
             RunStatic.InternalBatchStart(URIMethod, batchOptions) then
          begin
            RunningBatchRest := RunStatic;
            RunningBatchTable := RunTable;
            RunningBatchURIMethod := URIMethod;
          end
          else
          if (RunningBatchRest = nil) and
             (RunStatic = nil) and
             InternalBatchStart(URIMethod, batchOptions) then
          begin
            RunningBatchRest := self; // e.g. multi-insert in main SQlite3 engine
            RunningBatchTable := RunTable;
            RunningBatchURIMethod := URIMethod;
          end;
          if Count >= length(Results) then
            SetLength(Results, NextGrow(Count));
        end;
        // process CRUD method operation
        OK := false;
        Results[Count] := HTTP_NOTMODIFIED;
        case URIMethod of
          mDELETE:
            begin
              if EngineDelete(RunTableIndex, ID) then
              begin
                if fCache <> nil then
                  fCache.NotifyDeletion(RunTableIndex, ID);
                if (RunningBatchRest <> nil) or
                   AfterDeleteForceCoherency(RunTableIndex, ID) then
                begin
                  Results[Count] := HTTP_SUCCESS; // 200 OK
                  OK := true;
                end;
              end;
            end;
          mPOST:
            begin
              ID := EngineAdd(RunTableIndex, Value);
              Results[Count] := ID;
              if ID <> 0 then
              begin
                if fCache <> nil then
                  fCache.Notify(RunTableIndex, ID, Value, ooInsert);
                OK := true;
              end;
            end;
          mPUT:
            if EngineUpdate(RunTableIndex, ID, Value) then
            begin
              Results[Count] := HTTP_SUCCESS; // 200 OK
              OK := true;
              if fCache <> nil then
                // JSON Value may be uncomplete -> delete from cache
                if not (boPutNoCacheFlush in batchOptions) then
                  fCache.NotifyDeletion(RunTableIndex, ID);
            end;
        end;
        if (boRollbackOnError in batchOptions) and
           not OK then
          raise EOrmBatchException.CreateUTF8(
            '%.EngineBatchSend: Results[%]=% on % %',
            [self, Count, Results[Count], Method, RunTable]);
        inc(Count);
        inc(counts[URIMethod]);
      until EndOfObject = ']';
      if (AutomaticTransactionPerRow > 0) and
         (RowCountForCurrentTransaction > 0) then
        // send pending rows within transaction
        PerformAutomaticCommit;
    finally
      try
        if RunningBatchRest <> nil then
          // send pending rows, and release Safe.Lock
          RunningBatchRest.InternalBatchStop;
      finally
        fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
        InternalLog('EngineBatchSend json=% add=% update=% delete=% %%',
          [KB(Data), counts[mPOST], counts[mPUT], counts[mDELETE],
           MethodTable, Table]);
      end;
    end;
  except
    on E: Exception do
    begin
      if (AutomaticTransactionPerRow > 0) and
         (RowCountForCurrentTransaction > 0) then
      begin
        for i := 0 to high(RunTableTransactions) do
          if RunTableTransactions[i] <> nil then
            RunTableTransactions[i].RollBack(CONST_AUTHENTICATION_NOT_USED);
        UniqueRawUTF8ZeroToTilde(Data, 1 shl 16);
        InternalLog('% -> PARTIAL rollback of latest auto-committed transaction data=%',
          [E, Data], sllWarning);
      end;
      raise;
    end;
  end;
  if Table <> nil then
  begin
    // '{"Table":["cmd":values,...]}' format
    if Sent = nil then
      raise EOrmBatchException.CreateUTF8(
        '%.EngineBatchSend: % Truncated', [self, Table]);
    while not (Sent^ in ['}', #0]) do
      inc(Sent);
    if Sent^ <> '}' then
      raise EOrmBatchException.CreateUTF8(
        '%.EngineBatchSend(%): Missing }', [self, Table]);
  end;
  // if we reached here, process was OK
  SetLength(Results, Count);
  result := HTTP_SUCCESS;
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

procedure TRestOrmServer.TrackChanges(const aTable: array of TOrmClass;
  aTableHistory: TOrmHistoryClass; aMaxHistoryRowBeforeBlob,
  aMaxHistoryRowPerRecord, aMaxUncompressedBlobSize: integer);
var
  t, tableIndex, TableHistoryIndex: PtrInt;
begin
  if (self = nil) or
     (high(aTable) < 0) then
    exit;
  if aMaxHistoryRowBeforeBlob <= 0 then
    // disable change tracking
    TableHistoryIndex := -1
  else
  begin
    if aTableHistory = nil then
      aTableHistory := TOrmHistory;
    TableHistoryIndex := fModel.GetTableIndexExisting(aTableHistory);
  end;
  for t := 0 to high(aTable) do
  begin
    tableIndex := fModel.GetTableIndexExisting(aTable[t]);
    if aTable[t].InheritsFrom(TOrmHistory) then
      raise EOrmException.CreateUTF8(
        '%.TrackChanges([%]) not allowed', [self, aTable[t]]);
    if cardinal(tableIndex) < fTrackChangesHistoryTableIndexCount then
    begin
      fTrackChangesHistoryTableIndex[tableIndex] := TableHistoryIndex;
      if TableHistoryIndex >= 0 then
        with fTrackChangesHistory[TableHistoryIndex] do
        begin
          if CurrentRow = 0 then
            CurrentRow := TableRowCount(aTableHistory);
          MaxSentDataJsonRow := aMaxHistoryRowBeforeBlob;
          MaxRevisionJSON := aMaxHistoryRowPerRecord;
          MaxUncompressedBlobSize := aMaxUncompressedBlobSize;
        end;
    end;
  end;
end;

procedure TRestOrmServer.TrackChangesFlush(
  aTableHistory: TOrmHistoryClass);
var
  HistBlob: TOrmHistory;
  Rec: TOrm;
  HistJson: TOrmHistory;
  WhereClause, JSON: RawUTF8;
  HistID, ModifiedRecord: TInt64DynArray;
  TableHistoryIndex, i, HistIDCount, n: PtrInt;
  ModifRecord, ModifRecordCount, MaxRevisionJSON: integer;
  T: TOrmTable;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fRest.LogClass.Enter('TrackChangesFlush(%)', [aTableHistory], self);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock; // avoid race condition
  try
    // low-level Add(TOrmHistory) without cache
    TableHistoryIndex := fModel.GetTableIndexExisting(aTableHistory);
    MaxRevisionJSON := fTrackChangesHistory[TableHistoryIndex].MaxRevisionJSON;
    if MaxRevisionJSON <= 0 then
      MaxRevisionJSON := 10;
    // we will compress into BLOB only when we got more than 10 revisions of a record
    T := MultiFieldValues(aTableHistory, 'RowID,ModifiedRecord',
      'Event<>%', [ord(heArchiveBlob)], []);
    try
      T.GetRowValues(T.FieldIndexID, HistID);
      T.GetRowValues(T.FieldIndex('ModifiedRecord'), ModifiedRecord);
    finally
      T.Free;
    end;
    QuickSortInt64(pointer(ModifiedRecord), pointer(HistID),
      0, high(ModifiedRecord));
    ModifRecord := 0;
    ModifRecordCount := 0;
    n := 0;
    HistIDCount := 0;
    for i := 0 to high(ModifiedRecord) do
    begin
      if (ModifiedRecord[i] = 0) or
         (HistID[i] = 0) then
        raise EOrmException.CreateUTF8('%.TrackChangesFlush: Invalid %.ID=%',
          [self, aTableHistory, HistID[i]]);
      if ModifiedRecord[i] <> ModifRecord then
      begin
        if ModifRecordCount > MaxRevisionJSON then
          HistIDCount := n
        else
          n := HistIDCount;
        ModifRecord := ModifiedRecord[i];
        ModifRecordCount := 1;
      end
      else
        inc(ModifRecordCount);
      HistID[n] := HistID[i];
      inc(n);
    end;
    if ModifRecordCount > MaxRevisionJSON then
      HistIDCount := n;
    if HistIDCount = 0 then
      exit; // nothing to compress
    QuickSortInt64(Pointer(HistID), 0, HistIDCount - 1);
    WhereClause := Int64DynArrayToCSV(Pointer(HistID), HistIDCount,
      'RowID in (', ')');
    { following SQL can be very slow with external tables, and won't work
      with TRestStorageInMemory -> manual process instead
    WhereClause := FormatUTF8('ModifiedRecord in (select ModifiedRecord from '+
        '(select ModifiedRecord, count(*) NumItems from % group by ModifiedRecord) '+
        'where NumItems>% order by ModifiedRecord) and History is null',
        [aTableHistory.SQLTableName,MaxRevisionJSON]); }
    Rec := nil;
    HistBlob := nil;
    HistJson := aTableHistory.CreateAndFillPrepare(self, WhereClause);
    try
      HistBlob := aTableHistory.Create;
      while HistJson.FillOne do
      begin
        if HistJson.ModifiedRecord <> HistBlob.ModifiedRecord then
        begin
          if HistBlob.ModifiedRecord <> 0 then
            HistBlob.HistorySave(self, Rec);
          FreeAndNil(Rec);
          HistBlob.History := '';
          HistBlob.IDValue := 0;
          HistBlob.Event := heArchiveBlob;
          if not Retrieve('ModifiedRecord=? and Event=%',
              [ord(heArchiveBlob)], [HistJson.ModifiedRecord], HistBlob) then
            HistBlob.ModifiedRecord := HistJson.ModifiedRecord
          else
            RetrieveBlobFields(HistBlob);
          if not HistBlob.HistoryOpen(fModel) then
          begin
            InternalLog('Invalid %.History BLOB content for ID=%: % ' +
              'layout may have changed -> flush any previous content',
              [HistBlob.RecordClass, HistBlob.IDValue,
               HistJson.ModifiedTable(fModel)], sllError);
            HistBlob.IDValue := 0;
          end;
          if HistBlob.IDValue <> 0 then
            // allow changes appending to HistBlob
            Rec := HistBlob.HistoryGetLast
          else
          begin
            // HistBlob.fID=0 -> no previous BLOB content
            JSON := JSONEncode([
              'ModifiedRecord', HistJson.ModifiedRecord,
              'Timestamp', GetServerTimestamp,
              'Event', ord(heArchiveBlob)]);
            if HistJson.Event = heAdd then
            begin
              // allow versioning from scratch
              HistBlob.IDValue := EngineAdd(TableHistoryIndex, JSON);
              Rec := HistJson.ModifiedTable(fModel).Create;
              HistBlob.HistoryOpen(fModel);
            end
            else
            begin
              Rec := Retrieve(HistJson.ModifiedRecord);
              if Rec <> nil then
              try
                // initialize BLOB with latest revision
                HistBlob.IDValue := EngineAdd(TableHistoryIndex, JSON);
                HistBlob.HistoryOpen(fModel);
                HistBlob.HistoryAdd(Rec, HistJson);
              finally
                FreeAndNil(Rec); // ignore partial SentDataJSON for this record
              end;
            end;
          end;
        end;
        if (Rec = nil) or
           (HistBlob.IDValue = 0) then
          // only append modifications to BLOB if valid
          continue;
        Rec.FillFrom(pointer(HistJson.SentDataJSON));
        HistBlob.HistoryAdd(Rec, HistJson);
      end;
      if HistBlob.ModifiedRecord <> 0 then
        HistBlob.HistorySave(self, Rec);
      SetLength(HistID, HistIDCount);
      EngineDeleteWhere(TableHistoryIndex, WhereClause, TIDDynArray(HistID));
    finally
      HistJson.Free;
      HistBlob.Free;
      Rec.Free;
    end;
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
end;

function TRestOrmServer.InternalUpdateEventNeeded(aTableIndex: integer): boolean;
begin
  result := (self <> nil) and
    (Assigned(OnUpdateEvent) or
     ((cardinal(aTableIndex) < fTrackChangesHistoryTableIndexCount) and
      (fTrackChangesHistoryTableIndex[aTableIndex] >= 0)));
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
  if (aStatics[aTableIndex] <> nil) and
     (aStatics[aTableIndex] <> aStatic) then
    raise ERestException.CreateUTF8(
      'SetStaticTable(%): existing % for %',
      [aTableIndex, aStatics[aTableIndex], aStatic]);
  if aStatic <> nil then
    TRestOrmServer(aStatic)._AddRef // manual reference counting
  else
    TRestOrmServer(aStatics[aTableIndex])._Release;
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
    raise ERestException.CreateUTF8(
      '%.StaticVirtualTableSetup(%?,%)', [self, aTableIndex, aStatic]);
  case aKind of
    sStaticDataTable:
      SetStaticTable(aTableIndex, n, aStatic, fStaticData);
    sVirtualTable:
      SetStaticTable(aTableIndex, n, aStatic, fStaticVirtualTable);
  else
    raise ERestException.CreateUTF8('%.StaticVirtualTableSetup(%,%?)',
      [self, aStatic, GetEnumName(TypeInfo(TRestServerKind), ord(aKind))^]);
  end;
end;

function TRestOrmServer.InternalAdaptSQL(TableIndex: integer;
  var SQL: RawUTF8): TRestOrm;
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
           not TRestStorage(result).AdaptSQLForEngineList(SQL) then
          // complex request will use SQlite3 virtual engine module
          result := nil;
    end;
  end;
end;

function TRestOrmServer.InternalListRawUTF8(TableIndex: integer;
  const SQL: RawUTF8): RawUTF8;
var
  aSQL: RawUTF8; // use a private copy for InternalAdaptSQL()
  Rest: TRestOrm;
begin
  aSQL := SQL;
  Rest := InternalAdaptSQL(TableIndex, aSQL);
  if Rest <> nil then
     // this SQL statement is handled by direct connection, faster adaptation
    result := Rest.EngineList(aSQL)
  else
    // complex TOrmVirtualTableJSON/External queries will rely on virtual table
    result := MainEngineList(SQL, false, nil);
  if result = '[]'#$A then
    result := '';
end;

function TRestOrmServer.InternalUpdateEvent(aEvent: TOrmEvent;
  aTableIndex: integer; aID: TID; const aSentData: RawUTF8;
  aIsBlobFields: PFieldBits): boolean;

  procedure DoTrackChanges(TableHistoryIndex: integer);
  var
    TableHistoryClass: TOrmHistoryClass;
    JSON: RawUTF8;
    Event: TOrmHistoryEvent;
  begin
    case aEvent of
      oeAdd:
        Event := heAdd;
      oeUpdate:
        Event := heUpdate;
      oeDelete:
        Event := heDelete;
    else
      exit;
    end;
    TableHistoryClass := TOrmHistoryClass(
      fModel.Tables[TableHistoryIndex]);
    TableHistoryClass.InitializeFields([
      'ModifiedRecord', aTableIndex + aID shl 6,
      'Event', ord(Event),
      'SentDataJSON', aSentData,
      'Timestamp', GetServerTimestamp], JSON);
    fRest.AcquireExecution[execOrmWrite].Safe.Lock; // avoid race condition
    try // low-level Add(TOrmHistory) without cache
      EngineAdd(TableHistoryIndex, JSON);
      { TODO: use a BATCH (in background thread) to speed up TOrmHistory storage? }
      if fTrackChangesHistory[TableHistoryIndex].CurrentRow >
           fTrackChangesHistory[TableHistoryIndex].MaxSentDataJsonRow then
      begin
        // gather & compress TOrmHistory.SentDataJson into History BLOB
        TrackChangesFlush(TableHistoryClass);
        fTrackChangesHistory[TableHistoryIndex].CurrentRow := 0;
      end
      else
        // fast append as JSON until reached MaxSentDataJsonRow
        inc(fTrackChangesHistory[TableHistoryIndex].CurrentRow);
    finally
      fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
    end;
  end;

var
  TableHistoryIndex: integer;
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
    if cardinal(aTableIndex) < fTrackChangesHistoryTableIndexCount then
    begin
      TableHistoryIndex := fTrackChangesHistoryTableIndex[aTableIndex];
      if TableHistoryIndex >= 0 then
        DoTrackChanges(TableHistoryIndex);
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
    W: RawUTF8;
    cascadeOK: boolean;
    Rest: TRestOrm;
  begin
    // set Field=0 or delete row where Field references aID
    if Where = 0 then
      exit;
    Int64ToUTF8(Where, W);
    if Ref^.CascadeDelete then
      cascadeOK := Delete(fModel.Tables[Ref^.TableIndex],
        Ref^.FieldType.Name + '=:(' + W + '):')
    else
    begin
      Rest := GetStaticTableIndex(Ref^.TableIndex);
      if Rest <> nil then // fast direct call
        cascadeOK := Rest.EngineUpdateField(Ref^.TableIndex,
          Ref^.FieldType.Name, '0', Ref^.FieldType.Name, W)
      else
        cascadeOK := MainEngineUpdateField(Ref^.TableIndex,
          Ref^.FieldType.Name, '0', Ref^.FieldType.Name, W);
    end;
    if not cascadeOK then
      InternalLog('AfterDeleteForceCoherency() failed to handle field %.%',
        [fModel.Tables[Ref^.TableIndex], Ref^.FieldType.Name], sllWarning);
  end;

var
  i: integer;
  Ref: POrmModelReference;
begin
  Ref := pointer(fModel.RecordReferences);
  if Ref <> nil then
  begin
    for i := 1 to length(fModel.RecordReferences) do
    begin
      if Ref^.FieldTableIndex = -2 then
        // lazy initialization
        Ref^.FieldTableIndex := fModel.GetTableIndexSafe(Ref^.FieldTable, false);
      case Ref^.FieldType.OrmFieldType of
        oftRecord:
          // TRecordReference published field
          PerformCascade(RecordReference(aTableIndex, aID), Ref);
        oftID:
          // TOrm published field
          if Ref^.FieldTableIndex = aTableIndex then
            PerformCascade(aID, Ref);
        oftTID:
          // TTableID = type TID published field
          if Ref^.FieldTableIndex = aTableIndex then
            PerformCascade(aID, Ref);
      end;
      inc(Ref);
    end;
  end;
  result := true; // success even if no match found, or some cascade warnings
end;

procedure TRestOrmServer.FlushInternalDBCache;
begin // do nothing by default
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

function TRestOrmServer.CreateSQLIndex(Table: TOrmClass;
  const FieldName: RawUTF8; Unique: boolean; const IndexName: RawUTF8): boolean;
begin
  result := CreateSQLMultiIndex(Table, [FieldName], Unique, IndexName);
end;

function TRestOrmServer.CreateSQLIndex(Table: TOrmClass;
  const FieldNames: array of RawUTF8; Unique: boolean): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to high(FieldNames) do
    if not CreateSQLMultiIndex(Table, [FieldNames[i]], Unique) then
      result := false;
end;

function TRestOrmServer.CreateSQLMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUTF8; Unique: boolean;
  IndexName: RawUTF8): boolean;
var
  SQL: RawUTF8;
  i, TableIndex: PtrInt;
  Props: TOrmProperties;
  Rest: TRestOrm;
begin
  result := false;
  if high(FieldNames) < 0 then
    // avoid endless loop for TRestStorage with no overridden method
    exit;
  TableIndex := fModel.GetTableIndexExisting(Table);
  Rest := nil;
  if TableIndex >= 0 then
  begin
    // bypass fVirtualTableDirect
    if PtrUInt(TableIndex) < PtrUInt(length(fStaticData)) then
      Rest := fStaticData[TableIndex];
    if (Rest = nil) and
       (fStaticVirtualTable <> nil) then
      Rest := fStaticVirtualTable[TableIndex];
  end;
  if (Rest <> nil) and
     Rest.InheritsFrom(TRestStorage) then
  begin
    // create the index on the static table (e.g. for external DB)
    result := TRestStorage(Rest).CreateSQLMultiIndex(
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
  Props := fModel.TableProps[TableIndex].Props;
  for i := 0 to high(FieldNames) do
    if not IsRowID(pointer(FieldNames[i])) then
      if Props.Fields.IndexByName(FieldNames[i]) < 0 then
        // wrong field name
        exit;
  if Unique then
    SQL := 'UNIQUE '
  else
    SQL := '';
  if IndexName = '' then
  begin
    IndexName := RawUTF8ArrayToCSV(FieldNames, '');
    if length(IndexName) + length(Props.SQLTableName) > 64 then
      // avoid reaching potential identifier name size limit
      IndexName := crc32cUTF8ToHex(Props.SQLTableName) +
                   crc32cUTF8ToHex(IndexName);
  end;
  SQL := FormatUTF8('CREATE %INDEX IF NOT EXISTS Index%% ON %(%);',
    [SQL, Props.SQLTableName, IndexName, Props.SQLTableName,
     RawUTF8ArrayToCSV(FieldNames, ',')]);
  result := EngineExecute(SQL);
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
  // the main TRestServer is responsible of sessions and authentication
  result := fOwner.HandleAuthentication;
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
  const SQLWhere: RawUTF8): boolean;
var
  IDs: TIDDynArray;
  TableIndex, i: PtrInt;
begin
  result := InternalDeleteNotifyAndGetIDs(Table, SQLWhere, IDs);
  if (IDs = nil) or
     not result then
    // nothing to delete
    exit;
  TableIndex := Model.GetTableIndexExisting(Table);
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    // may be within a batch in another thread
    result := EngineDeleteWhere(TableIndex, SQLWhere, IDs);
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.Unlock;
  end;
  if result then
    // force relational database coherency (i.e. our FOREIGN KEY implementation)
    for i := 0 to high(IDs) do
      AfterDeleteForceCoherency(TableIndex, IDs[i]);
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
    // faster direct call
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

