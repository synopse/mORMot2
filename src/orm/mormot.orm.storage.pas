/// ORM Types and Classes for the Server side JSON/Binary Storage
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.storage;

{
  *****************************************************************************

   Server-Side Storage Process using JSON or Binary Persistence
    - Virtual Table ORM Support
    - TRestStorage Abstract Class for ORM/REST Storage
    - TRestStorageInMemory as Stand-Alone JSON/Binary Storage
    - TOrmVirtualTableJson/TOrmVirtualTableBinary Virtual Tables
    - TRestStorageRemote for CRUD Redirection
    - TRestStorageShard as Abstract Sharded Storage Engine
    - TRestStorageMulti as Abstract Multi-User Storage Engine

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
  mormot.crypt.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client;

// most types are defined as a single "type" statement due to classes coupling

type
  // some forward class definitions
  TRestStorage = class;
  TOrmVirtualTable = class;
  TRestStorageInMemory = class;


{ ************ Virtual Table ORM Support }

  /// Record associated to a Virtual Table implemented in Delphi, with ID
  // forced at INSERT
  // - will use TOrmVirtualTableModule / TOrmVirtualTable / TOrmVirtualTableCursor
  // classes for a generic Virtual table mechanism on the Server side
  // - call Model.VirtualTableRegister() before TRestOrmServer.Create on the
  // Server side (not needed for Client) to associate such a record with a
  // particular Virtual Table module, otherwise an exception will be raised:
  // ! Model.VirtualTableRegister(TOrmDali1,TOrmVirtualTableJson);
  TOrmVirtualTableForcedID = class(TOrmVirtual);

  /// Record associated to Virtual Table implemented in Delphi, with ID
  // generated automatically at INSERT
  // - will use TOrmVirtualTableModule / TOrmVirtualTable / TOrmVirtualTableCursor
  // classes for a generic Virtual table mechanism
  // - call Model.VirtualTableRegister() before TRestOrmServer.Create on the
  // Server side (not needed for Client) to associate such a record with a
  // particular Virtual Table module, otherwise an exception will be raised:
  // ! Model.VirtualTableRegister(TOrmDali1,TOrmVirtualTableJson);
  TOrmVirtualTableAutoID = class(TOrmVirtual);

  /// class-reference type (metaclass) of our abstract table storage
  // - may be e.g. TRestStorageInMemory, TRestStorageInMemoryExternal,
  // TRestStorageExternal or TRestStorageMongoDB
  TRestStorageClass = class of TRestStorage;

  /// class-reference type (metaclass) of a virtual table implementation
  TOrmVirtualTableClass = class of TOrmVirtualTable;

  /// a WHERE constraint as set by the TOrmVirtualTable.Prepare() method
  TOrmVirtualTablePreparedConstraint = packed record
    /// Column on left-hand side of constraint
    // - The first column of the virtual table is column 0
    // - The RowID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index
    // - if this field contains VIRTUAL_TABLE_IGNORE_COLUMN (-2), TOrmVirtualTable.
    // Prepare() should ignore this entry
    Column: integer;
    /// The associated expression
    // - TOrmVirtualTable.Prepare() must set Value.VType to not ftUnknown
    // (e.g. to ftNull), if an expression is expected at vt_BestIndex() call
    // - TOrmVirtualTableCursor.Search() will receive an expression value,
    // to be retrieved e.g. via sqlite3_value_*() functions
    Value: TSqlVar;
    /// Constraint operator to be transmitted at SQL level
    // - MATCH keyword is parsed into soBeginWith, and should be handled as
    // soBeginWith, soContains or soSoundsLike* according to the effective
    // expression text value ('text*', '%text'...)
    Operation: TSqlCompareOperator;
    /// If true, the constraint is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default (OmitCheck=false), the SQLite core double checks all
    // constraints on each row of the virtual table that it receives
    // - TOrmVirtualTable.Prepare() can set this property to true
    OmitCheck: boolean;
  end;

  POrmVirtualTablePreparedConstraint = ^TOrmVirtualTablePreparedConstraint;

  /// an ORDER BY clause as set by the TOrmVirtualTable.Prepare() method
  // - warning: this structure should match exactly TSqlite3IndexOrderBy as
  // defined in mormot.db.raw.sqlite3
  TOrmVirtualTablePreparedOrderBy = record
    /// Column number
    // - The first column of the virtual table is column 0
    // - The RowID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    Column: integer;
    /// True for DESCending order, false for ASCending order.
    Desc: boolean;
  end;

  /// abstract planning execution of a query, as set by TOrmVirtualTable.Prepare
  TOrmVirtualTablePreparedCost = (
    costFullScan,
    costScanWhere,
    costSecondaryIndex,
    costPrimaryIndex);

  /// the WHERE and ORDER BY statements as set by TOrmVirtualTable.Prepare
  // - Where[] and OrderBy[] are fixed sized arrays, for fast and easy code
  {$ifdef USERECORDWITHMETHODS}
  TOrmVirtualTablePrepared = record
  {$else}
  TOrmVirtualTablePrepared = object
  {$endif USERECORDWITHMETHODS}
  public
    /// number of WHERE statement parameters in Where[] array
    WhereCount: integer;
    /// numver of ORDER BY statement parameters in OrderBy[]
    OrderByCount: integer;
    /// if true, the ORDER BY statement is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default (OmitOrderBy=false), the SQLite core sort all rows of the
    // virtual table that it receives according in order
    OmitOrderBy: boolean;
    ///  Estimated cost of using this prepared index
    // - SQLite uses this value to make a choice between several calls to
    // the TOrmVirtualTable.Prepare() method with several expressions
    EstimatedCost: TOrmVirtualTablePreparedCost;
    ///  Estimated number of rows of using this prepared index
    // - does make sense only if EstimatedCost=costFullScan
    // - SQLite uses this value to make a choice between several calls to
    // the TOrmVirtualTable.Prepare() method with several expressions
    // - is used only starting with SQLite 3.8.2
    EstimatedRows: Int64;
    /// WHERE statement parameters, in TOrmVirtualTableCursor.Search() order
    Where: array[0 .. MAX_SQLFIELDS - 1] of TOrmVirtualTablePreparedConstraint;
    /// ORDER BY statement parameters
    OrderBy: array[0 .. MAX_SQLFIELDS - 1] of TOrmVirtualTablePreparedOrderBy;
    /// returns TRUE if there is only one ID=? statement in this search
    function IsWhereIDEquals(CalledFromPrepare: boolean): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if there is only one FieldName=? statement in this search
    function IsWhereOneFieldEquals: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  POrmVirtualTablePrepared = ^TOrmVirtualTablePrepared;

  TOrmVirtualTableCursor = class;

  /// class-reference type (metaclass) of a cursor on an abstract Virtual Table
  TOrmVirtualTableCursorClass = class of TOrmVirtualTableCursor;

  /// the possible features of a Virtual Table
  // - vtWrite is to be set if the table is not Read/Only
  // - vtTransaction if handles vttBegin, vttSync, vttCommit, vttRollBack
  // - vtSavePoint if handles vttSavePoint, vttRelease, vttRollBackTo
  // - vtWhereIDPrepared if the ID=? WHERE statement will be handled in
  // TOrmVirtualTableCursor.Search()
  TOrmVirtualTableFeature = (
    vtWrite,
    vtTransaction,
    vtSavePoint,
    vtWhereIDPrepared);

  /// a set of features of a Virtual Table
  TOrmVirtualTableFeatures = set of TOrmVirtualTableFeature;

  /// used to store and handle the main specifications of a TOrmVirtualTableModule
  TVirtualTableModuleProperties = record
    /// a set of features of a Virtual Table
    Features: TOrmVirtualTableFeatures;
    /// the associated cursor class
    CursorClass: TOrmVirtualTableCursorClass;
    /// the associated TOrm class
    // - used to retrieve the field structure with all collations
    RecordClass: TOrmClass;
    /// the associated TRestStorage class used for storage
    // - is e.g. TRestStorageInMemory for TOrmVirtualTableJson,
    // TRestStorageExternal for TOrmVirtualTableExternal, or nil for
    // TOrmVirtualTableLog
    StaticClass: TRestStorageClass;
    /// can be used to customize the extension of the filename
    // - the '.' is not to be included
    FileExtension: TFileName;
  end;

  /// parent class able to define a Virtual Table module
  // - in order to implement a new Virtual Table type, you'll have to define a so
  // called "Module" to handle the fields and data access and an associated
  // TOrmVirtualTableCursorClass for handling the SELECT statements
  // - for our framework, the SQLite3 unit will inherit from this class to define
  // a TOrmVirtualTableModuleSQLite3 class, which will register the associated
  // virtual table definition into a SQLite3 connection, on the server side
  // - children should override abstract methods in order to implement the
  // association with the database engine itself
  TOrmVirtualTableModule = class
  protected
    fModuleName: RawUtf8;
    fTableClass: TOrmVirtualTableClass;
    fServer: TRestOrmServer;
    fFeatures: TVirtualTableModuleProperties;
    fFilePath: TFileName;
  public
    /// create the Virtual Table instance according to the supplied class
    // - inherited constructors may register the Virtual Table to the specified
    // database connection
    constructor Create(aTableClass: TOrmVirtualTableClass;
      aServer: TRestOrmServer); virtual;
    /// retrieve the file name to be used for a specific Virtual Table
    // - returns by default a file located in the executable folder, with the
    // table name as file name, and module name as extension
    function FileName(const aTableName: RawUtf8): TFileName; virtual;
    /// the Virtual Table module features
    property Features: TOrmVirtualTableFeatures
      read fFeatures.Features;
    /// the associated virtual table class
    property TableClass: TOrmVirtualTableClass
      read fTableClass;
    /// the associated virtual table cursor class
    property CursorClass: TOrmVirtualTableCursorClass
      read fFeatures.CursorClass;
    /// the associated TRestStorage class used for storage
    // - e.g. returns TRestStorageInMemory for TOrmVirtualTableJson,
    // or TRestStorageExternal for TOrmVirtualTableExternal, or
    // either nil for TOrmVirtualTableLog
    property StaticClass: TRestStorageClass
      read fFeatures.StaticClass;
    /// the associated TOrm class
    // - is mostly nil, e.g. for TOrmVirtualTableJson
    // - used to retrieve the field structure for TOrmVirtualTableLog e.g.
    property RecordClass: TOrmClass
      read fFeatures.RecordClass;
    /// the extension of the filename (without any left '.')
    property FileExtension: TFileName
      read fFeatures.FileExtension;
    /// the full path to be used for the filename
    // - is '' by default, i.e. will use the executable path
    // - you can specify here a custom path, which will be used by the FileName
    // method to retrieve the .json/.data full file
    property FilePath: TFileName
      read fFilePath write fFilePath;
    /// the associated Server instance
    // - may be nil, in case of direct access to the virtual table
    property Server: TRestOrmServer
      read fServer;
    /// the corresponding module name
    property ModuleName: RawUtf8
      read fModuleName;
  end;

  /// the available transaction levels
  TOrmVirtualTableTransaction = (
    vttBegin,
    vttSync,
    vttCommit,
    vttRollBack,
    vttSavePoint,
    vttRelease,
    vttRollBackTo);

  /// abstract class able to access a Virtual Table content
  // - override the Prepare/Structure abstract virtual methods for reading
  // access to the virtual table content
  // - you can optionaly override Drop/Delete/Insert/Update/Rename/Transaction
  // virtual methods to allow content writing to the virtual table
  // - the same virtual table mechanism can be used with several database module,
  // with diverse database engines
  TOrmVirtualTable = class
  protected
    fModule: TOrmVirtualTableModule;
    fTableName: RawUtf8;
    fStatic: TRestOrm;
    fStaticStorage: TRestStorage;
    fStaticTable: TOrmClass;
    fStaticTableIndex: integer;
  public
    /// create the virtual table access instance
    // - the created instance will be released when the virtual table will be
    // disconnected from the DB connection (e.g. xDisconnect method for SQLite3)
    // - shall raise an exception in case of invalid parameters (e.g. if the
    // supplied module is not associated to a TRestOrmServer instance)
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TOrmVirtualTableAutoID class and
    // create any associated Static: TRestStorage instance
    constructor Create(aModule: TOrmVirtualTableModule; const aTableName: RawUtf8;
      FieldCount: integer; Fields: PPUtf8CharArray); virtual;
    /// release the associated memory, especially the Static instance
    destructor Destroy; override;
    /// retrieve the corresponding module name
    // - will use the class name, triming any T/TSql/TSqlVirtual/TOrmVirtualTable*
    // - when the class is instanciated, it will be faster to retrieve the same
    // value via Module.ModuleName
    class function ModuleName: RawUtf8;
    /// a generic method to get a 'CREATE TABLE' structure from a supplied
    // TOrm class
    // - is called e.g. by the Structure method
    class function StructureFromClass(aClass: TOrmClass;
      const aTableName: RawUtf8): RawUtf8;
    /// the associated Virtual Table module
    property Module: TOrmVirtualTableModule
      read fModule;
    /// the name of the Virtual Table, as specified following the TABLE keyword
    // in the CREATE VIRTUAL TABLE statement
    property TableName: RawUtf8
      read fTableName;
  public { virtual methods to be overridden }
    /// should return the main specifications of the associated TOrmVirtualTableModule
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); virtual; abstract;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TOrmVirtualTableCursor.Search()
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck to true if double check is not necessary
    // - OmitOrderBy must be set to true if double sort is not necessary
    // - EstimatedCost and EstimatedRows should receive the estimated cost
    // - default implementation will let the DB engine perform the search,
    // and prepare for ID=? statement if vtWhereIDPrepared was set
    function Prepare(var Prepared: TOrmVirtualTablePrepared): boolean; virtual;
    /// should retrieve the format (the names and datatypes of the columns) of
    // the virtual table, as expected by sqlite3_declare_vtab()
    // - default implementation is to retrieve the structure for the associated
    // Module.RecordClass property (as set by GetTableModuleProperties) or
    // the Static.StoredClass: in both cases, column numbering will follow
    // the TOrm published field order (TOrm.Orm.Fields[])
    function Structure: RawUtf8; virtual;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Drop: boolean; virtual;
    /// called to delete a virtual table row
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Delete(aRowID: Int64): boolean; virtual;
    /// called to insert a virtual table row content from an array of TSqlVar
    // - should return true on success, false otherwise
    // - should return the just created row ID in insertedRowID on success
    // - does nothing by default, and returns false, i.e. always fails
    function Insert(aRowID: Int64; var Values: TSqlVarDynArray;
      out insertedRowID: Int64): boolean; virtual;
    /// called to update a virtual table row content from an array of TSqlVar
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Update(oldRowID, newRowID: Int64;
      var Values: TSqlVarDynArray): boolean; virtual;
    /// called to begin a transaction to the virtual table row
    // - do nothing by default, and returns false in case of RollBack/RollBackto
    // - aSavePoint is used for vttSavePoint, vttRelease and vttRollBackTo only
    // - note that if you don't nest your writing within a transaction, SQLite
    // will call vttCommit for each INSERT/UPDATE/DELETE, just like a regular
    // SQLite database - it could make bad written code slow even with Virtual
    // Tables
    function Transaction(aState: TOrmVirtualTableTransaction;
      aSavePoint: integer): boolean; virtual;
    /// called to rename the virtual table
    // - by default, returns false, i.e. always fails
    function Rename(const NewName: RawUtf8): boolean; virtual;
    /// the associated virtual table storage instance
    // - can be e.g. a TRestStorageInMemory for TOrmVirtualTableJson,
    // or a TRestStorageExternal for TOrmVirtualTableExternal, or nil
    // for TOrmVirtualTableLog
    property Static: TRestOrm
      read fStatic;
    /// the associated virtual table storage instance, if is a TRestStorage
    property StaticStorage: TRestStorage
      read fStaticStorage;
    /// the associated virtual table storage table
    property StaticTable: TOrmClass
      read fStaticTable;
    /// the associated virtual table storage index in its Model.Tables[] array
    property StaticTableIndex: integer
      read fStaticTableIndex;
  end;

  /// abstract class able to define a Virtual Table cursor
  // - override the Search/HasData/Column/Next abstract virtual methods to
  // implement the search process
  TOrmVirtualTableCursor = class
  protected
    fTable: TOrmVirtualTable;
    /// used internally between two Column() method calls for GetFieldSqlVar()
    fColumnTemp: RawByteString;
    /// easy set a TSqlVar content for the Column() method
    procedure SetColumn(var aResult: TSqlVar; aValue: Int64); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSqlVar; const aValue: double); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSqlVar; const aValue: RawUtf8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSqlVar; aValue: PUtf8Char; aValueLength: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnBlob(var aResult: TSqlVar; aValue: pointer; aValueLength: integer);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnDate(var aResult: TSqlVar; const aValue: TDateTime;
      aWithMS: boolean); {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnCurr64(var aResult: TSqlVar; aValue64: PInt64);
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the cursor instance
    // - it will be destroyed when by the DB engine (e.g. via xClose in SQLite3)
    constructor Create(aTable: TOrmVirtualTable); virtual;
    /// the associated Virtual Table class instance
    property Table: TOrmVirtualTable
      read fTable;
  public { abstract methods to be overridden }
    /// called to begin a search in the virtual table
    // - the TOrmVirtualTablePrepared parameters were set by
    // TOrmVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved e.g. by x_BestIndex() from a TSqlite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - should move cursor to first row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; virtual; abstract;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; virtual; abstract;
    /// called to retrieve a column value of the current data row into a TSqlVar
    // - if aColumn=-1, should return the row ID as varInt64 into aResult
    // - should return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSqlVar): boolean; virtual; abstract;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; virtual; abstract;
  end;

  /// A generic Virtual Table cursor associated to Current/Max index properties
  TOrmVirtualTableCursorIndex = class(TOrmVirtualTableCursor)
  protected
    fCurrent: integer;
    fMax: integer;
  public
    /// called after Search() to check if there is data to be retrieved
    // - will return false if reached the end of matching data, according to
    // the fCurrent/fMax protected properties values
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - will check the fCurrent/fMax protected properties values
    function Next: boolean; override;
    /// called to begin a search in the virtual table
    // - this no-op version will mark EOF, i.e. fCurrent=0 and fMax=-1
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; override;
  end;




{ ************ TRestStorage Abstract Class for ORM/REST Storage }

  /// exception raised during ORM/REST Storage process
  ERestStorage = class(EOrmException);

  /// REST class with direct access to an external database engine
  // - you can set an alternate per-table database engine by using this class
  // - this abstract class is to be overridden with a proper implementation
  // (e.g. TRestStorageInMemory in this unit, or TRestStorageExternal
  // from mormot.orm.sql unit, or TRestStorageMongoDB from
  // mormot.orm.mongodb.pas unit)
  TRestStorage = class(TRestOrm)
  protected
    fStoredClass: TOrmClass;
    fStoredClassProps: TOrmModelProperties;
    fStoredClassRecordProps: TOrmProperties;
    fStoredClassMapping: POrmMapping;
    fStorageLockShouldIncreaseOwnerInternalState: boolean;
    fModified: boolean;
    fOutInternalStateForcedRefresh: boolean;
    {$ifdef DEBUGSTORAGELOCK}
    fStorageCriticalSectionCount: integer;
    {$endif DEBUGSTORAGELOCK}
    fOwner: TRestOrmServer;
    fStorageVirtual: TOrmVirtualTable;
    fBasicSqlCount: RawUtf8;
    fBasicSqlHasRows: array[boolean] of RawUtf8;
    fStorageCriticalSection: TRTLCriticalSection;
    fTempBuffer: PTextWriterStackBuffer;
    procedure RecordVersionFieldHandle(Occasion: TOrmOccasion;
      var Decoder: TJsonObjectDecoder);
    function GetStoredClassName: RawUtf8;
  public
    /// initialize the abstract storage data
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer); reintroduce; virtual;
    /// finalize the storage instance
    destructor Destroy; override;

    /// should be called before any access to the storage content
    // - and protected with a try ... finally StorageUnLock; end section
    procedure StorageLock(WillModifyContent: boolean
        {$ifdef DEBUGSTORAGELOCK}; const msg: shortstring{$endif}); virtual;
    /// should be called after any StorageLock-protected access to the content
    // - e.g. protected with a try ... finally StorageUnLock; end section
    procedure StorageUnLock;
      {$ifndef DEBUGSTORAGELOCK} {$ifdef FPC} inline; {$endif} {$endif}
    /// low-level access to how StorageLock(true) affetcs TRestServer.InternalState
    property StorageLockShouldIncreaseOwnerInternalState: boolean
      read fStorageLockShouldIncreaseOwnerInternalState
      write fStorageLockShouldIncreaseOwnerInternalState;

    /// implement Rest unlocking (UNLOCK verb)
    // - to be called e.g. after a Retrieve() with forupdate=TRUE
    // - locking is handled at (Owner.)Model level
    // - returns true on success
    function UnLock(Table: TOrmClass; aID: TID): boolean; override;
    /// overridden method calling the owner (if any) to guess if this record
    // can be updated or deleted
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID; Action: TOrmEvent;
      ErrorMsg: PRawUtf8 = nil): boolean; override;
    /// override this method if you want to update the refresh state
    // - returns FALSE if the static table content was not modified (default
    // method implementation is to always return FALSE)
    // - returns TRUE if the table has been refreshed and its content was modified:
    // therefore the client will know he'll need to refresh some content
    function RefreshedAndModified: boolean; virtual;
    /// TRestOrmServer.Uri use it for Static.EngineList to by-pass virtual table
    // - this default implementation will return TRUE and replace SQL with
    // Sql.SelectAll[true] if it SQL equals Sql.SelectAll[false] (i.e. 'SELECT *')
    // - this method is called only if the WHERE clause of SQL refers to the
    // static table name only (not needed to check it twice)
    function AdaptSqlForEngineList(var SQL: RawUtf8): boolean; virtual;
    /// create one index for all specific FieldNames at once
    // - do nothing virtual/abstract method by default: will return FALSE (i.e. error)
    function CreateSqlMultiIndex(Table: TOrmClass; const FieldNames: array of RawUtf8;
      Unique: boolean; IndexName: RawUtf8 = ''): boolean; virtual;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    // - this default implementation will call the overloaded SearchField()
    // value after conversion of the FieldValue into RawUtf8
    function SearchField(const FieldName: RawUtf8; FieldValue: Int64;
      out ResultID: TIDDynArray): boolean; overload; virtual;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - this virtual implementation redirect to OneFieldValues method, which
    // creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUtf8;
      out ResultID: TIDDynArray): boolean; overload; virtual;
    /// returns the current authentication session ID from TRestOrmServer owner
    function GetCurrentSessionUserID: TID; override;
    /// internal method returning 0 for 'ID'/'RowID' or the TOrm field index + 1
    function GetFieldIndex(const FieldName: RawUtf8; out Index: integer): boolean;

    /// read only access to a boolean value set to true if table data was modified
    property Modified: boolean
      read fModified write fModified;
    /// read only access to the ORM properties of the associated record type
    // - may be nil if this instance is not associated with a TOrmModel
    property StoredClassProps: TOrmModelProperties
      read fStoredClassProps;
    /// read only access to the RTTI properties of the associated record type
    property StoredClassRecordProps: TOrmProperties
      read fStoredClassRecordProps;
    /// read only access to the TRestOrmServer using this storage engine
    property Owner: TRestOrmServer
      read fOwner;
    /// read only access to the class defining the record type stored in this
    // REST storage
    property StoredClass: TOrmClass
      read fStoredClass;
    /// allow to force refresh for a given Static table
    // - default FALSE means to return the main TRestOrmServer.InternalState
    // - TRUE indicates that OutInternalState := cardinal(-1) will be returned
    property OutInternalStateForcedRefresh: boolean
      read fOutInternalStateForcedRefresh;
  published
    /// name of the class defining the record type stored in this REST storage
    property StoredClassName: RawUtf8
      read GetStoredClassName;
  end;



{ ************ TRestStorageInMemory as Stand-Alone JSON/Binary Storage }

  /// event prototype called by TRestStorageInMemory.FindWhereEqual(),
  // FindWhere() or ForEach() methods
  // - aDest is an opaque pointer, as supplied to FindWhereEqual(), which may
  // point e.g. to a result list, or a shared variable to apply the process
  // - aRec will point to the corresponding item
  // - aIndex will identify the item index in the internal list
  TOnFindWhereEqual = procedure(
    aDest: pointer; aRec: TOrm; aIndex: integer) of object;

  /// abstract REST storage exposing some internal TOrm-based methods
  TRestStorageTOrm = class(TRestStorage)
  public
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    /// internal method called by TRestServer.Batch() to process SIMPLE input
    // - overriden for optimized multi-insert of the supplied JSON array values
    function InternalBatchDirectSupport(Encoding: TRestBatchEncoding;
      RunTableIndex: integer): TRestOrmBatchDirect; override;
    /// internal method called by TRestServer.Batch() to process SIMPLE input
    // - overriden for optimized multi-insert of the supplied JSON array values
    function InternalBatchDirectOne(Encoding: TRestBatchEncoding;
      RunTableIndex: integer; const Fields: TFieldBits; Sent: PUtf8Char): TID; override;
    /// manual Add of a TOrm
    // - returns the ID created on success
    // - returns -1 on failure (not UNIQUE field value e.g., optionally setting
    // the index into ExistingIndex PtrInt)
    // - on success, the Rec instance is added to the Values[] list: caller
    // doesn't need to Free it
    function AddOne(Rec: TOrm; ForceID: boolean;
      const SentData: RawUtf8; ExistingIndex: PPtrInt = nil): TID; virtual; abstract;
    /// manual Retrieval of a TOrm field values
    // - an instance of the associated static class is created
    // - and all its properties are filled from the Items[] values
    // - caller can modify these properties, then use UpdateOne() if the changes
    // have to be stored inside the Items[] list
    // - calller must always free the returned instance
    // - returns NIL if any error occured, e.g. if the supplied aID was incorrect
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function GetOne(aID: TID): TOrm; virtual; abstract;
    /// manual Update of a TOrm field values
    // - Rec.ID specifies which record is to be updated
    // - will update bit-wise Fields specified properties
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function UpdateOne(Rec: TOrm; const Fields: TFieldBits;
      const SentData: RawUtf8): boolean; overload; virtual; abstract;
    /// manual Update of a TOrm field values from an array of TSqlVar
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    // - this default implementation will create a temporary TOrm instance
    // with the supplied Values[], and will call overloaded UpdateOne() method
    function UpdateOne(ID: TID;
      const Values: TSqlVarDynArray): boolean; overload; virtual;
  end;

  /// class able to handle a O(1) hashed-based search of a property
  // - used e.g. to hash TRestStorageInMemory field values
  TRestStorageInMemoryUnique = class
  protected
    fHasher: TDynArrayHasher;
    fOwner: TRestStorageInMemory;
    fPropInfo: TOrmPropInfo;
    fCaseInsensitive: boolean;
    fLastFindHashCode: cardinal;
  public
    /// initialize a hash for a record array field
    // - aField maps the "stored AS_UNIQUE" published property
    constructor Create(aOwner: TRestStorageInMemory; aField: TOrmPropInfo);
    /// fast search using O(1) internal hash table
    // - returns -1 if not found or not indexed (self=nil)
    function Find(Rec: TOrm): integer;
    /// called by TRestStorageInMemory.AddOne after a precious Find()
    function AddedAfterFind(Rec: TOrm): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// the corresponding field RTTI
    property PropInfo: TOrmPropInfo
      read fPropInfo;
    /// if the string comparison shall be case-insensitive
    property CaseInsensitive: boolean
      read fCaseInsensitive;
    /// access to the internal hash table
    property Hasher: TDynArrayHasher
      read fHasher;
  end;

  /// REST storage with direct access to a TObjectList memory-stored table
  // - store the associated TOrm values in memory
  // - handle one TOrm per TRestStorageInMemory instance
  // - must be registered individualy in a TRestOrmServer to access data from a
  // common client, by using the TRestOrmServer.OrmMapInMemory method:
  // it allows an unique access for both SQLite3 and Static databases
  // - handle basic REST commands, no full SQL interpreter is implemented: only
  // valid SQL command is "SELECT Field1,Field2 FROM Table WHERE ID=120;", i.e
  // a one Table SELECT with one optional "WHERE fieldname operator value"
  // statement, with = < <= <> != >= > operators, and IS / IS NULL / ID IN (...)
  // - if used within a TOrmVirtualTableJson, you'll be able to handle any kind of
  // SQL statement (even joined SELECT or such) with this memory-stored database
  // via the SQlite3 virtual tables engine
  // - data can be stored and retrieved from a file (JSON format is used by
  // default, if BinaryFile parameter is left to false; a proprietary compressed
  // binary format can be used instead) if a file name is supplied at creating
  // the TRestStorageInMemory instance
  // - our TRestStorageInMemory database engine is very optimized and is a lot
  // faster than SQLite3 for such queries - but its values remain in RAM,
  // therefore it is not meant to deal with more than 100,000 rows or if
  // ACID commit on disk is required
  TRestStorageInMemory = class(TRestStorageTOrm)
  protected
    fValue: TOrmObjArray;
    fCount: integer;
    fCommitShouldNotUpdateFile: boolean;
    fBinaryFile: boolean;
    fExpandedJson: boolean;
    fUnSortedID: boolean;
    fTrackChangesAndFlushRunning: boolean;
    fFileName: TFileName;
    fSearchRec: TOrm; // temporary record to store the searched value
    fBasicUpperSqlSelect: array[boolean] of RawUtf8;
    fUnique, fUniquePerField: array of TRestStorageInMemoryUnique;
    fMaxID: TID;
    fValues: TDynArrayHashed; // hashed by ID
    fTrackChangesFieldBitsOffset: PtrUInt;
    fTrackChangesPersistence: IRestOrm;
    fTrackChangesDeleted: TInt64DynArray; // TIDDynArray
    fTrackChangesDeletedCount: integer;
    function UniqueFieldsUpdateOK(aRec: TOrm; aUpdateIndex: integer;
      aFields: PFieldBits): boolean;
    procedure RaiseGetItemOutOfRange(Index: integer);
    function GetItem(Index: integer): TOrm;
      {$ifdef HASINLINE}inline;{$endif}
    function GetID(Index: integer): TID;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalTrackChangeUpdated(aRec: TOrm; const Fields: TFieldBits);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetFileName(const aFileName: TFileName);
    procedure ComputeStateAfterLoad(var loaded: TPrecisionTimer; binary: boolean);
    procedure SetBinaryFile(aBinary: boolean);
    procedure GetJsonValuesEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// used to create the JSON content from a SELECT parsed command
    // - WhereField index follows FindWhereEqual / TSelectStatement.WhereField
    // - returns the number of data row added (excluding field names)
    // - this method is very fast and optimized (for search and JSON serializing)
    function GetJsonValues(Stream: TStream; Expand: boolean;
      Stmt: TSelectStatement): PtrInt;
  public
    /// TRestOrmServer.Uri use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle basic queries as handled by EngineList()
    function AdaptSqlForEngineList(var SQL: RawUtf8): boolean; override;
    /// overridden methods for direct in-memory database engine thread-safe process
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; override;
    function EngineExecute(const aSql: RawUtf8): boolean; override;
  public
    /// initialize the table storage data, reading it from a file if necessary
    // - data encoding on file is UTF-8 JSON format by default, or
    // should be some binary format if aBinaryFile is set to true
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean = false); reintroduce; virtual;
    /// free used memory
    // - especially release all fValue[] instances
    destructor Destroy; override;
    /// clear all the values of this table
    // - will reset the associated database file, if any
    procedure DropValues(andUpdateFile: boolean = true);
    /// load the values from JSON data
    // - a temporary copy of aJson is made to ensure it won't be modified in-place
    // - consider using the overlaoded PUtf8Char/len method if you don't need this copy
    procedure LoadFromJson(const aJson: RawUtf8); overload;
    /// load the values from JSON data
    procedure LoadFromJson(JsonBuffer: PUtf8Char; JsonBufferLen: PtrInt); overload;
    /// save the values into JSON data
    function SaveToJson(Expand: boolean): RawUtf8; overload;
    /// save the values into JSON data
    procedure SaveToJson(Stream: TStream; Expand: boolean); overload;
    /// load the values from binary file/stream
    // - the binary format is a custom compressed format (using our SynLZ fast
    // compression algorithm), with variable-length record storage
    // - the binary content is first checked for consistency, before loading
    // - warning: the field layout should be the same at SaveToBinary call;
    // for instance, it won't be able to read a file content with a renamed
    // or modified field type
    // - will return false if the binary content is invalid
    function LoadFromBinary(Stream: TStream): boolean; overload;
    /// load the values from binary data
    // - uses the same compressed format as the overloaded stream/file method
    // - will return false if the binary content is invalid
    function LoadFromBinary(const Buffer: RawByteString): boolean; overload;
    /// load the values from binary resource
    // - the resource name is expected to be the TOrm class name,
    // with a resource type of 10
    // - uses the same compressed format as the overloaded stream/file method
    // - you can specify a library (dll) resource instance handle, if needed
    procedure LoadFromResource(ResourceName: string = ''; Instance: THandle = 0);
    /// save the values into a binary file/stream
    // - the binary format is a custom compressed format (using our SynLZ fast
    // compression algorithm), with variable-length record storage: e.g. a 27 KB
    // Dali1.json content is stored into a 6 KB Dali2.data file
    // (this data has a text redundant field content in its FirstName field);
    // 502 KB People.json content is stored into a 92 KB People.data file
    // - returns the number of bytes written into Stream
    function SaveToBinary(Stream: TStream): integer; overload;
    /// save the values into a binary buffer
    // - uses the same compressed format as the overloaded stream/file method
    function SaveToBinary: RawByteString; overload;
    /// if file was modified, the file is updated on disk
    // - this method is called automaticaly when the TRestStorage
    // instance is destroyed: should should want to call in in some cases,
    // in order to force the data to be saved regularly
    // - do nothing if the table content was not modified
    // - will write JSON content by default, or binary content if BinaryFile
    // property was set to true
    procedure UpdateFile;
    /// will reload all content from the current disk file
    // - any not saved modification will be lost (e.g. if Updatefile has not
    // been called since)
    procedure ReloadFromFile;
    /// retrieve the index in Items[] of a particular ID
    // - return -1 if this ID was not found
    // - use internally fast O(1) hashed search algorithm
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function IDToIndex(ID: TID): PtrInt;
    /// retrieve all IDs stored at once
    // - will make a thread-safe copy, for unlocked use
    procedure GetAllIDs(out ID: TIDDynArray);
    /// low-level Add of a TOrm instance
    // - returns the ID created on success
    // - returns -1 on failure (not UNIQUE field value e.g.)
    // - on success, the Rec instance is added to the Values[] list: caller
    // doesn't need to Free it, since it will be owned by the storage
    // - in practice, SentData is used only for OnUpdateEvent/OnBlobUpdateEvent
    // and the history feature
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function AddOne(Rec: TOrm; ForceID: boolean;
      const SentData: RawUtf8; ExistingIndex: PPtrInt = nil): TID; override;
    /// manual Retrieval of a TOrm field values
    // - an instance of the associated static class is created, and filled with
    // the actual properties values
    // - and all its properties are filled from the Items[] values
    // - caller can modify these properties, then use UpdateOne() if the changes
    // have to be stored inside the Items[] list
    // - calller must always free the returned instance
    // - returns NIL if any error occured, e.g. if the supplied aID was incorrect
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function GetOne(aID: TID): TOrm; override;
    /// manual Update of a TOrm field values
    // - Rec.ID specifies which record is to be updated
    // - will update bit-wise Fields specified properties
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function UpdateOne(Rec: TOrm; const Fields: TFieldBits;
      const SentData: RawUtf8): boolean; override;
    /// manual Update of a TOrm field values from a TSqlVar array
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function UpdateOne(ID: TID;
      const Values: TSqlVarDynArray): boolean; override;
    /// direct deletion of a TOrm, from its index in Values[]
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function DeleteOne(aIndex: integer): boolean; virtual;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; override;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestOrmServer
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    /// overridden method for direct in-memory database engine call
    function UpdateBlobFields(Value: TOrm): boolean; override;
    /// overridden method for direct in-memory database engine call
    function RetrieveBlobFields(Value: TOrm): boolean; override;
    /// overridden method for direct in-memory database engine call
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// overridden method for direct in-memory database engine call
    function TableHasRows(Table: TOrmClass): boolean; override;
    /// overridden method for direct in-memory database engine call
    function MemberExists(Table: TOrmClass; ID: TID): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUtf8;
      out ResultID: TIDDynArray): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return the found TOrm on success, nil if none did match
    // - warning: it returns a reference to one item of the unlocked internal
    // list, so you should NOT use this on a read/write table, but rather
    // use the slightly slower but safer SearchCopy() method or make explicit
    // ! StorageLock ... try ... SearchInstance ... finally StorageUnlock end
    function SearchInstance(const FieldName, FieldValue: RawUtf8;
      CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): pointer; overload;
    /// search for a field value, according to its SQL content representation
    // - overloaded function using a FieldIndex (0=RowID,1..=RTTI)
    function SearchInstance(FieldIndex: integer; const FieldValue: RawUtf8;
      CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): pointer; overload;
    /// search for a field value, according to its SQL content representation
    // - return the found TOrm index on success, -1 if none did match
    // - warning: it returns a reference to the current index of the unlocked
    // internal list, so you should NOT use without StorageLock/StorageUnlock
    function SearchIndex(const FieldName, FieldValue: RawUtf8;
      CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): integer;
    /// search for a field value, according to its SQL content representation
    // - return a copy of the found TOrm on success, nil if no match
    // - you should use SearchCopy() instead of SearchInstance(), unless you
    // are sure that the internal TOrm list won't change
    function SearchCopy(const FieldName, FieldValue: RawUtf8;
      CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): pointer;
    /// search and count for a field value, according to its SQL content representation
    // - return the number of found entries on success, 0 if it was not found
    function SearchCount(const FieldName, FieldValue: RawUtf8;
      CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): integer;
    /// search for a field value, according to its SQL content representation
    // - call the supplied OnFind event on match
    // - returns the number of found entries
    // - is just a wrapper around FindWhere() with StorageLock protection
    function SearchEvent(const FieldName, FieldValue: RawUtf8;
      const OnFind: TOnFindWhereEqual; Dest: pointer;
      FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): integer; overload;
    /// search for a field value, according to its SQL content representation
    // - overloaded function using a FieldIndex (0=RowID,1..=RTTI)
    function SearchEvent(FieldIndex: integer; const FieldValue: RawUtf8;
      const OnFind: TOnFindWhereEqual; Dest: pointer;
      FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean = true;
      Op: TSelectStatementOperator = opEqualTo): integer; overload;
    /// optimized search of WhereValue in WhereField (0=RowID,1..=RTTI)
    // - will use fast O(1) hash for fUnique[] fields
    // - will use SYSTEMNOCASE case-insensitive search for text values, unless
    // CaseInsensitive is set to FALSE
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhereEqual(WhereField: integer; const WhereValue: RawUtf8;
      const OnFind: TOnFindWhereEqual; Dest: pointer;
      FoundLimit, FoundOffset: PtrInt;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// optimized search of WhereValue in a field, specified by name
    // - will use fast O(1) hash for fUnique[] fields
    // - will use SYSTEMNOCASE case-insensitive search for text values, unless
    // CaseInsensitive is set to FALSE
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhereEqual(const WhereFieldName, WhereValue: RawUtf8;
      const OnFind: TOnFindWhereEqual; Dest: pointer;
      FoundLimit, FoundOffset: integer;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// comparison lookup of the WhereValue in a field, specified by name
    // - this method won't use any index but brute force using the comparison
    // function over each stored item to implement < <= <> > >= search
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhere(WhereField: integer; const WhereValue: RawUtf8;
      WhereOp: TSelectStatementOperator; const OnFind: TOnFindWhereEqual;
      Dest: pointer; FoundLimit, FoundOffset: integer;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// comparison lookup of the WhereValue in a field, specified by name
    // - this method won't use any index but brute force using the comparison
    // function over each stored item to implement < <= <> > >= search
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhere(const WhereFieldName, WhereValue: RawUtf8;
      WhereOp: TSelectStatementOperator; const OnFind: TOnFindWhereEqual;
      Dest: pointer; FoundLimit, FoundOffset: integer;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// search the maximum value of a given column
    // - will only handle integer/Int64 kind of column
    function FindMax(WhereField: integer; out max: Int64): boolean;
    /// execute a method on every TOrm item
    // - the loop execution will be protected via StorageLock/StorageUnlock
    procedure ForEach(WillModifyContent: boolean;
      const OnEachProcess: TOnFindWhereEqual; Dest: pointer);
    /// allow background write into another persistence of modified TOrm
    // - you should define a TrackedFields: TFieldBits public field and register
    // its offset as TrackChanges(@TOrmClass(nil).TrackedFields, OrmWriter);
    // - then AddOne/UpdateOne/DeleteOne methods will be tracked
    // - if you manually update one record, call TrackChangeUpdated() overloads
    procedure TrackChanges(FieldBitsOffset: pointer; const Persistence: IRestOrm);
    /// after TrackChanges, refresh a TOrm manually updated in-place
    procedure TrackChangeUpdated(aRec: TOrm; const Fields: TFieldBits); overload;
    /// after TrackChanges, refresh a TOrm manually updated in-place
    // - if Fields is [], then simple fields are marked as updated
    procedure TrackChangeUpdated(aRec: TOrm; const Fields: array of PUtf8Char); overload;
    /// send all TrackChanges modifications into the associated Persistence
    // - could be called from a background thread, e.g. once a few seconds
    // - returns false on error, true if changes have been pushed (or nothing to do)
    function TrackChangesAndFlush: boolean;
    /// low-level TOnFindWhereEqual callback doing nothing
    class procedure DoNothingEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// low-level TOnFindWhereEqual callback making PPointer(aDest)^ := aRec
    class procedure DoInstanceEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// low-level TOnFindWhereEqual callback making PInteger(aDest)^ := aIndex
    class procedure DoIndexEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// low-level TOnFindWhereEqual callback making PPointer(aDest)^ := aRec.CreateCopy
    class procedure DoCopyEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// low-level TOnFindWhereEqual callback calling TSynList(aDest).Add(aRec)
    class procedure DoAddToListEvent(aDest: pointer; aRec: TOrm; aIndex: integer);
    /// read-only access to the TOrm values, storing the data
    // - this returns directly the item class instance stored in memory: if you
    // change the content, it will affect the internal data - so for instance
    // DO NOT change the ID values, unless you may have unexpected behavior
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property Items[Index: integer]: TOrm
      read GetItem; default;
    /// direct access to the memory of the internal dynamic array storage
    // - Items[] is preferred, since it will check the index, but is slightly
    // slower, e.g. in a loop or after a IDToIndex() call
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property Value: TOrmObjArray
      read fValue;
    /// read-only access to the ID of a TOrm values
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property ID[Index: integer]: TID
      read GetID;
  published
    /// read only access to the file name specified by constructor
    // - you can call the TRestOrmServer.StaticData method to
    // update the file name of an already instanciated static table
    // - if you change manually the file name from this property, the storage
    // will be marked as "modified" so that UpdateFile will save the content
    property FileName: TFileName
      read fFileName write SetFileName;
    /// if set to true, file content on disk will expect binary format
    // - default format on disk is JSON but can be overridden at constructor call
    // - binary format should be more efficient in term of speed and disk usage,
    // but can be proprietary
    // - if you change manually the file format from this property, the storage
    // will be marked as "modified" so that UpdateFile will save the content
    property BinaryFile: boolean
      read fBinaryFile write SetBinaryFile;
    // JSON writing, can set if the format should be expanded or not
    // - by default, the JSON will be in the custom non-expanded format,
    // to save disk space and time
    // - you can force the JSON to be emitted as an array of objects,
    // e.g. for better human friendliness (reading and modification)
    property ExpandedJson: boolean
      read fExpandedJson write fExpandedJson;
    /// set this property to TRUE if you want the COMMIT statement not to
    // update the associated TOrmVirtualTableJson
    property CommitShouldNotUpdateFile: boolean
      read fCommitShouldNotUpdateFile write fCommitShouldNotUpdateFile;
    /// read-only access to the number of TOrm values
    property Count: integer
      read fCount;
  end;

  /// a dynamic array of TRestStorageInMemory instances
  // - used e.g. by TRestOrmServerFullMemory
  TRestStorageInMemoryDynArray = array of TRestStorageInMemory;

  /// class-reference type (metaclass) of our TObjectList memory-stored table storage
  // - may be TRestStorageInMemory or TRestStorageInMemoryExternal
  TRestStorageInMemoryClass = class of TRestStorageInMemory;

  /// REST storage with direct access to a memory database, to be used as
  // an external SQLite3 Virtual table
  // - this is the kind of in-memory table expected by TOrmVirtualTableJson,
  // in order to be consistent with the internal DB cache
  TRestStorageInMemoryExternal = class(TRestStorageInMemory)
  public
    /// initialize the table storage data, reading it from a file if necessary
    // - data encoding on file is UTF-8 JSON format by default, or
    // should be some binary format if aBinaryFile is set to true
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean = false); override;
    /// this overridden method will notify the Owner when the internal DB content
    // is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but TOrmVirtualTableJson virtual
    // tables could flush the database content without proper notification
    // - this overridden implementation will call Owner.FlushInternalDBCache
    procedure StorageLock(WillModifyContent: boolean
      {$ifdef DEBUGSTORAGELOCK}; const msg: shortstring {$endif}); override;
  end;



{ ************ TOrmVirtualTableJson/TOrmVirtualTableBinary Virtual Tables }

  /// A Virtual Table cursor for reading a TRestStorageInMemory content
  // - this is the cursor class associated to TOrmVirtualTableJson
  TOrmVirtualTableCursorJson = class(TOrmVirtualTableCursorIndex)
  public
    /// called to begin a search in the virtual table
    // - the TOrmVirtualTablePrepared parameters were set by
    // TOrmVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSqlite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - only handled WHERE clause is for "ID = value" - other request will
    // return all records in ID order, and let the database engine handle it
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row into a TSqlVar
    // - if aColumn=-1, will return the row ID as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSqlVar): boolean; override;
  end;

  /// A TRestStorageInMemory-based virtual table using JSON storage
  // - for ORM access, you should use TOrmModel.VirtualTableRegister method to
  // associated this virtual table module to a TOrmVirtualTableAutoID class
  // - transactions are not handled by this module
  // - by default, no data is written on disk: you will need to call explicitly
  // aServer.GetVirtualStorage(aClass).UpdateToFile for file creation or refresh
  // - file extension is set to '.json'
  TOrmVirtualTableJson = class(TOrmVirtualTable)
  protected
    fStaticInMemory: TRestStorageInMemory;
  public
    { overridden methods }
    /// create the virtual table access instance
    // - the created instance will be released when the virtual table will be
    // disconnected from the DB connection (e.g. xDisconnect method for SQLite3)
    // - shall raise an exception in case of invalid parameters (e.g. if the
    // supplied module is not associated to a TRestOrmServer instance)
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TOrmVirtualTableAutoID class and
    // create any associated Static: TRestStorage instance
    constructor Create(aModule: TOrmVirtualTableModule;
      const aTableName: RawUtf8; FieldCount: integer;
      Fields: PPUtf8CharArray); override;
    /// returns the main specifications of the associated TOrmVirtualTableModule
    // - this is a read/write table, without transaction, associated to the
    // TOrmVirtualTableCursorJson cursor type, with 'JSON' as module name
    // - no particular class is supplied here, since it will depend on the
    // associated Static instance
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TOrmVirtualTableCursor.Search()
    // - only prepared WHERE statement is for "ID = value"
    // - only prepared ORDER BY statement is for ascending IDs
    function Prepare(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content from a TSqlVar array
    // - column order follows the Structure method, i.e.
    // StoredClassRecordProps.Fields[] order
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    // - does nothing by default, and returns false, i.e. always fails
    function Insert(aRowID: Int64; var Values: TSqlVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content from a TSqlVar array
    // - column order follows the Structure method, i.e.
    // StoredClassRecordProps.Fields[] order
    // - returns true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Update(oldRowID, newRowID: Int64;
      var Values: TSqlVarDynArray): boolean; override;
  end;

  /// A TRestStorageInMemory-based virtual table using Binary storage
  // - for ORM access, you should use TOrmModel.VirtualTableRegister method to
  // associated this virtual table module to a TOrmVirtualTableAutoID class
  // - transactions are not handled by this module
  // - by default, no data is written on disk: you will need to call explicitly
  // aServer.GetVirtualStorage(aClass).UpdateToFile for file creation or refresh
  // - binary format is more efficient in term of speed and disk usage than
  // the JSON format implemented by TOrmVirtualTableJson
  // - binary format will be set by TOrmVirtualTableJson.CreateTableInstance
  // - file extension is set to '.data'
  TOrmVirtualTableBinary = class(TOrmVirtualTableJson);



{ ************ TOrmVirtualTableLog Virtual Table }

  /// Implements a read/only virtual table able to access a .log file, as created
  // by TSynLog
  // - to be used e.g. by a TOrmLog_Log ('Log_' will identify this 'Log' module)
  // - the .log file name will be specified by the Table Name, to which a '.log'
  // file extension will be appended before loading it from the current directory
  TOrmVirtualTableLog = class(TOrmVirtualTable)
  protected
    fLogFile: TSynLogFile;
  public
    /// returns the main specifications of the associated TOrmVirtualTableModule
    // - this is a read only table, with transaction, associated to the
    // TOrmVirtualTableCursorLog cursor type, with 'Log' as module name,
    // and associated to TOrmLog_Log table field layout
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); override;
    /// creates the TOrmVirtualTable according to the supplied parameters
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TOrmVirtualTableAutoID class
    constructor Create(aModule: TOrmVirtualTableModule; const aTableName: RawUtf8;
      FieldCount: integer; Fields: PPUtf8CharArray); override;
    /// release the associated .log file mapping and all internal structures
    destructor Destroy; override;
  end;

  /// A Virtual Table cursor for reading a TSynLogFile content
  // - this is the cursor class associated to TOrmVirtualTableLog
  TOrmVirtualTableCursorLog = class(TOrmVirtualTableCursorIndex)
  public
    /// called to begin a search in the virtual table
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row as TSqlVar
    function Column(aColumn: integer; var aResult: TSqlVar): boolean; override;
  end;


{ ************ TRestStorageRemote for CRUD Redirection }

  /// REST storage with redirection to another REST instance
  // - allows redirection of all CRUD operations for a table to another
  // TRestOrm instance, may be a remote TRestOrmClient or a TRestOrmServer
  // - will be used by TRestOrmServer.RemoteDataCreate() method
  TRestStorageRemote = class(TRestStorage)
  protected
    fRemoteRest: TRestOrm;
    fRemoteTableIndex: integer;
  public
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    function EngineExecute(const aSql: RawUtf8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
  public
    /// initialize the table storage redirection
    // - you should not have to use this constructor, but rather the
    // TRestOrmServer.RemoteDataCreate() method which will create and register
    // one TRestStorageRemote instance
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer;
      aRemoteRest: TRestOrm); reintroduce; virtual;
  published
    /// the remote ORM instance used for data persistence
    // - may be a TRestOrmClient or a TRestOrmServer instance
    property RemoteRest: TRestOrm
      read fRemoteRest;
  end;



{ ************ TRestStorageShard as Abstract Sharded Storage Engine }

  /// defines how TRestStorageShard will handle its partioned process
  TRestStorageShardOption = (
    ssoNoUpdate,
    ssoNoUpdateButLastShard,
    ssoNoDelete,
    ssoNoDeleteButLastShard,
    ssoNoBatch,
    ssoNoList,
    ssoNoExecute,
    ssoNoUpdateField,
    ssoNoConsolidateAtDestroy);

  /// how TRestStorageShard will handle its partioned process
  TRestStorageShardOptions = set of TRestStorageShardOption;

  /// abstract REST storage with redirection to several REST instances,
  // implementing range ID partitioning for horizontal scaling
  // - such database shards will allow to scale with typical BigData storage
  // - this storage will add items on a server, initializing a new server
  // when the ID reached a defined range
  // - it will maintain a list of previous storages, then redirect reading and
  // updates to the server managing this ID (if possible - older shards may
  // be deleted/ignored to release resources)
  // - inherited class should override InitShards/InitNewShard to customize the
  // kind of TRestOrm instances to be used for each shard (which may be local
  // or remote, a SQLite3 engine or an external SQL/NoSQL database)
  // - see inherited TRestStorageShardDB as defined in mormot.orm.sqlite3.pas
  TRestStorageShard = class(TRestStorage)
  protected
    fShardRange: TID;
    fShardOffset: integer;
    fMaxShardCount: integer;
    fLastID: TID;
    fOptions: TRestStorageShardOptions;
    fShards: array of TRestOrm;
    fShardLast: integer;
    fShardLastID: TID;
    fShardNextID: TID;
    fShardTableIndex: TIntegerDynArray;
    fShardBatch: array of TRestBatch;
    // will set Shards[],fShardLast,fShardLastID,fShardOffset
    procedure InitShards; virtual; abstract;
    // should always return non nil shard to contain new added IDs
    function InitNewShard: TRestOrm; virtual; abstract;
    procedure InternalAddNewShard;
    function InternalShardBatch(ShardIndex: integer): TRestBatch;
  public
    // overriden methods which will handle all ORM process
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    function EngineExecute(const aSql: RawUtf8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    function InternalBatchStart(Encoding: TRestBatchEncoding;
      BatchOptions: TRestBatchOptions): boolean; override;
    procedure InternalBatchStop; override;
  public
    /// initialize the table storage redirection for sharding
    // - you should not have to use this abstract constructor, but e.g.
    // TRestStorageShardDB.Create which involved the actual persistence
    // - the supplied aShardRange should be < 1000 - and once set, you should NOT
    // change this value on an existing shard, unless process will be broken
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer;
      aShardRange: TID; aOptions: TRestStorageShardOptions;
      aMaxShardCount: integer); reintroduce; virtual;
    /// finalize the table storage, including Shards[] instances
    destructor Destroy; override;
    /// you may call this method sometimes to consolidate the sharded data
    // - may e.g. merge/compact shards, depending on scaling expectations
    // - also called by Destroy - do nothing by default
    procedure ConsolidateShards; virtual;
    /// remove a shard database from the current set
    // - it will allow e.g. to delete a *.dbs file at runtime, without
    // restarting the server
    // - this default implementation will free and nil fShard[aShardIndex],
    // which is enough for most implementations (e.g. TRestStorageShardDB)
    procedure RemoveShard(aShardIndex: integer); virtual;
    /// retrieve the ORM shard instance corresponding to an ID
    // - may return false if the correspondig shard is not available any more
    // - may return true, and a TRestOrmHookClient or a TRestOrmHookServer instance
    // with its associated index in TRestOrm.Model.Tables[]
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function ShardFromID(aID: TID; out aShardTableIndex: integer;
      out aShard: TRestOrm; aOccasion: TOrmOccasion = ooSelect;
      aShardIndex: PInteger = nil): boolean; virtual;
    /// get the row count of a specified table
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TOrmClass): boolean; override;
  published
    /// how much IDs should store each ORM shard instance
    // - once set, you should NEVER change this value on an existing shard,
    // otherwise the whole ID partition will fail
    // - each shard will hold [ShardIndex*ShardRange..(ShardIndex+1)*ShardRange-1] IDs
    property ShardRange: TID
      read fShardRange;
    /// how many shards should be maintained at most
    // - if some older shards are available on disk, they won't be loaded by
    // InitShards, and newly added shard via InitNewShard will trigger
    // RemoveShard if the total number of shards
    property MaxShardCount: integer
      read fMaxShardCount;
    /// defines how this instance will handle its sharding process
    // - by default, update/delete operations or per ID retrieval will take
    // place on all shards, whereas EngineList and EngineExecute will only run
    // only on the latest shard (to save resources)
    property Options: TRestStorageShardOptions
      read fOptions write fOptions;
  end;

  /// class metadata of a Sharding storage engine
  TRestStorageShardClass = class of TRestStorageShard;


{ *********** TRestStorageMulti as Abstract Multi-User Storage Engine }

type
  /// exception raised during TRestStorageMulti process
  ERestStorageMulti = class(ERestStorage);

  /// identifier of one TRestStorageMulti/TRestStorageMultiDB database
  // - up to 63-bit could be used, but it could be a good idea to use a bit mask
  // to reserve some bits in the User ID, which would map to a group of users
  // (e.g. a company or organization), equal to this TRestStorageMultiID, so
  // that your service could use TRestStorageMulti to get the corresponding
  // IRestOrmServer instance
  TRestStorageMultiDatabaseID = type TID;

  /// abstract settings for REST storage with several database instances
  TRestStorageMultiSettings = class(TSynAutoCreateFields)
  protected
    fTimeOutSec: integer;
  published
    /// how many seconds a DB storage instance should be kept in cache
    property TimeOutSec: integer
      read fTimeOutSec write fTimeOutSec;
  end;

  /// abstract REST storage with several database instances
  // - e.g. to maintain a per-User or per-Group (company) storage
  // - inherited class should override the NewStore virtual method, e.g.
  // TRestStorageMultiDB as defined in mormot.orm.sqlite3.pas for SQlite3 storage
  // - your custom class should override NewModel to provide the proper data model
  TRestStorageMulti = class(TSynPersistentRWLightLock)
  protected
    fDatabaseIDBits: byte;
    fDatabaseIDMax: TRestStorageMultiDatabaseID;
    fLog: TSynLogFamily;
    fCache: TSynDictionary; // TRestStorageMultiDatabaseID/IRestOrmServer
    fSettings: TRestStorageMultiSettings;
    fModelClasses: TOrmClassDynArray;
    function GetShutdown: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function GetCached: RawJson;
    procedure SetShutdown(Value: boolean); virtual;
    function IDText(aID: TRestStorageMultiDatabaseID): TShort16;
    function GetDB(aID: TRestStorageMultiDatabaseID): IRestOrmServer; virtual;
    function NewModel: TOrmModel; virtual;
    // inherited classes should implement this abstract virtual method
    function NewDB(aID: TRestStorageMultiDatabaseID): IRestOrmServer; virtual; abstract;
  public
    /// initialize this REST storage with several database instances
    // - aDatabaseIDBits will define how many bits (1-63) are allowed for
    // TRestStorageMultiDatabaseID values
    // - aSettings instance will be owned by this main class instance
    constructor Create(aLog: TSynLogFamily; aDatabaseIDBits: byte;
      const aModelClasses: array of TOrmClass;
      aSettings: TRestStorageMultiSettings); reintroduce;
    /// finalize this REST storage instance and its associated databases
    destructor Destroy; override;
    /// could be assigned to TRestServer.OnInternalInfo to include information
    // about the current database instances within the timestamp/info output
    procedure OnServerInfo(Ctxt: TRestUriContext; var Info: TDocVariantData);
    /// check if the supplied database ID matches DatabaseIDBits definition
    function IsDatabaseIDCorrect(aID: TRestStorageMultiDatabaseID): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// raise ERestStorageMulti if the supplied database ID is out of range
    procedure EnsureDatabaseIDCorrect(aID: TRestStorageMultiDatabaseID;
      const aCaller: shortstring);
    /// access to the associated TSynLog instances
    property Log: TSynLogFamily
      read fLog;
    /// access to a given database from its ID
    property DB[aID: TRestStorageMultiDatabaseID]: IRestOrmServer
      read GetDB; default;
    /// notify that no further access is possible
    // - setting true will release the cache and all stored IRestOrmServer
    property Shutdown: boolean
      read GetShutdown write SetShutdown;
    /// how many bits are allowed to DB[] ID range
    property DatabaseIDBits: byte
      read fDatabaseIDBits;
    /// access to the associated settings
    property Settings: TRestStorageMultiSettings
      read fSettings;
  published
    property Cached: RawJson
      read GetCached;
  end;

  /// metaclass of a REST storage with several database instances
  TRestStorageMultiClass = class of TRestStorageMulti;

  /// abstract REST storage with several database instances stored in a folder
  TRestStorageMultiOnDisk = class(TRestStorageMulti)
  protected
    fDataFolder, fFilePrefix: TFileName;
    function GetDiskAvailableMB: integer; virtual;
    function GetDBPassword(aID: TRestStorageMultiDatabaseID): SpiUtf8; virtual;
    function GetDBFileName(aID: TRestStorageMultiDatabaseID): TFileName; virtual;
  public
    /// initialize this REST storage with several database instances on disk
    constructor Create(aLog: TSynLogFamily; aDatabaseIDBits: byte;
      const aModelClasses: array of TOrmClass;
      const aDataFolder, aFilePrefix: TFileName;
      aSettings: TRestStorageMultiSettings); reintroduce;
  published
    /// where the data is actually stored
    property DataFolder: TFileName
      read fDataFolder;
    /// how much free space there is in the data folder drive
    property DiskAvailableMB: integer
      read GetDiskAvailableMB;
  end;



function ToText(t: TOrmVirtualTableTransaction): PShortString; overload;

/// extract the ID from first value of the encPostHexID input JSON
// - and move Sent^ to a fake '[' with the first real simple parameter
// - used by InternalBatchDirectOne method of TRestStorageTOrm and TRestOrmServerDB
function BatchExtractSimpleID(var Sent: PUtf8Char): TID;


implementation

uses
  mormot.rest.server;



{ ************ Virtual Table ORM Support }

{ TOrmVirtualTablePrepared }

function TOrmVirtualTablePrepared.IsWhereIDEquals(CalledFromPrepare: boolean): boolean;
begin
  result := (WhereCount = 1) and
            (Where[0].Column = VIRTUAL_TABLE_ROWID_COLUMN) and
            (CalledFromPrepare or
             (Where[0].Value.VType = ftInt64)) and
            (Where[0].Operation = soEqualTo);
end;

function TOrmVirtualTablePrepared.IsWhereOneFieldEquals: boolean;
begin
  result := (WhereCount = 1) and
            (Where[0].Column >= 0) and
            (Where[0].Operation = soEqualTo);
end;


{ TOrmVirtualTableModule }

constructor TOrmVirtualTableModule.Create(aTableClass: TOrmVirtualTableClass;
  aServer: TRestOrmServer);
begin
  fTableClass := aTableClass;
  fServer := aServer;
  fTableClass.GetTableModuleProperties(fFeatures);
  fModuleName := fTableClass.ModuleName;
  if fFeatures.FileExtension = '' then
    // default extension is the module name
    Utf8ToFileName(LowerCase(fModuleName), fFeatures.FileExtension);
end;

function TOrmVirtualTableModule.FileName(const aTableName: RawUtf8): TFileName;
begin
  result := Utf8ToString(aTableName) + '.' + FileExtension;
  if fFilePath = '' then
    result := Executable.ProgramFilePath + result
  else
    result := IncludeTrailingPathDelimiter(fFilePath) + result;
end;


{ TOrmVirtualTable }

constructor TOrmVirtualTable.Create(aModule: TOrmVirtualTableModule;
  const aTableName: RawUtf8; FieldCount: integer; Fields: PPUtf8CharArray);
var
  aClass: TRestStorageClass;
  aServer: TRestOrmServer;
begin
  if (aModule = nil) or
     (aTableName = '') then
    raise EModelException.CreateUtf8('Invalid %.Create(%,"%")',
      [self, aModule, aTableName]);
  fModule := aModule;
  fTableName := aTableName;
  if fModule.fFeatures.StaticClass <> nil then
  begin
    // create new fStatic instance e.g. for TOrmVirtualTableLog
    aServer := fModule.Server;
    if aServer = nil then
      raise EModelException.CreateUtf8('%.Server=nil for %.Create', [Module, self])
    else
      fStaticTableIndex := aServer.Model.GetTableIndex(aTableName);
    if fStaticTableIndex >= 0 then
    begin
      fStaticTable := aServer.Model.Tables[fStaticTableIndex];
      aClass := fModule.fFeatures.StaticClass;
      if aClass.InheritsFrom(TRestStorageInMemory) then
        fStatic := TRestStorageInMemoryClass(aClass).Create(
          fStaticTable, fModule.Server, fModule.FileName(aTableName),
          self.InheritsFrom(TOrmVirtualTableBinary))
      else
        fStatic := aClass.Create(fStaticTable, fModule.Server);
      aServer.StaticTableSetup(fStaticTableIndex, fStatic, sVirtualTable);
      fStaticStorage := TRestStorage(fStatic);
      fStaticStorage.fStorageVirtual := self;
    end;
  end;
end;

destructor TOrmVirtualTable.Destroy;
begin
  if fStatic <> nil then
  begin
    if (Module <> nil) and
       (Module.Server <> nil) and
       (fStaticTableIndex >= 0) then
      // needed e.g. for temporary release during backup
      Module.Server.StaticTableSetup(fStaticTableIndex, nil, sVirtualTable);
    fStatic.Free;
  end;
  inherited Destroy;
end;

function TOrmVirtualTable.Prepare(var Prepared: TOrmVirtualTablePrepared): boolean;
begin
  result := self <> nil;
  if result then
    if (vtWhereIDPrepared in fModule.Features) and
       Prepared.IsWhereIDEquals(true) then
      with Prepared.Where[0] do
      begin
        // optimize for WHERE ID=? clause
        Value.VType := ftNull; // mark TOrmVirtualTableCursorJson expects it
        OmitCheck := true;
        Prepared.EstimatedCost := costPrimaryIndex;
        Prepared.EstimatedRows := 1;
      end
    else
    begin
      Prepared.EstimatedCost := costFullScan;
      Prepared.EstimatedRows := 1000000;
    end;
end;

function TOrmVirtualTable.Drop: boolean;
begin
  result := false;  // no DROP TABLE to be implemented here
end;

function TOrmVirtualTable.Delete(aRowID: Int64): boolean;
begin
  result := false;  // no DELETE to be implemented here
end;

function TOrmVirtualTable.Insert(aRowID: Int64; var Values: TSqlVarDynArray;
  out insertedRowID: Int64): boolean;
begin
  result := false;  // no INSERT to be implemented here
end;

function TOrmVirtualTable.Update(oldRowID, newRowID: Int64;
  var Values: TSqlVarDynArray): boolean;
begin
  result := false;  // no UPDATE to be implemented here
end;

function TOrmVirtualTable.Transaction(aState: TOrmVirtualTableTransaction;
  aSavePoint: integer): boolean;
begin
  result := (Module <> nil) and
            (vtWrite in Module.Features) and
            (aState in [vttBegin, vttSync, vttCommit, vttSavePoint, vttRelease]);
end;

function TOrmVirtualTable.Rename(const NewName: RawUtf8): boolean;
begin
  result := false;
end;

class function TOrmVirtualTable.ModuleName: RawUtf8;
const
  NAM: array[0..6] of PUtf8Char = (
    'TSQLVIRTUALTABLE',
    'TSQLVIRTUAL',
    'TSQL',
    'TORMVIRTUALTABLE',
    'TORMVIRTUAL',
    'TORM',
    nil);
  LEN: array[-1..5] of byte = (
    1,  // 'T'
    16, // 'TSQLVIRTUALTABLE'
    11, // 'TSQLVIRTUAL'
    4,  // 'TSQL'
    16, // 'TORMVIRTUALTABLE'
    11, // 'TORMVIRTUAL'
    4); // 'TORM'
begin
  if self = nil then
    result := ''
  else
  begin
    ClassToText(self, result);
    system.delete(result, 1, LEN[IdemPPChar(pointer(result), @NAM)]);
  end;
end;

class function TOrmVirtualTable.StructureFromClass(aClass: TOrmClass;
  const aTableName: RawUtf8): RawUtf8;
begin
  FormatUtf8('CREATE TABLE % (%', [aTableName,
    GetVirtualTableSqlCreate(aClass.OrmProps)], result);
end;

function TOrmVirtualTable.Structure: RawUtf8;
begin
  result := '';
  if self <> nil then
    if static <> nil then
      // e.g. for TOrmVirtualTableJson or TOrmVirtualTableExternal
      result := StructureFromClass(StaticTable, TableName)
    else if (Module <> nil) and
            (Module.RecordClass <> nil) then
      // e.g. for TOrmVirtualTableLog
      result := StructureFromClass(Module.RecordClass, TableName);
end;


{ TOrmVirtualTableCursor }

constructor TOrmVirtualTableCursor.Create(aTable: TOrmVirtualTable);
begin
  fTable := aTable;
end;

procedure TOrmVirtualTableCursor.SetColumn(var aResult: TSqlVar; aValue: Int64);
begin
  aResult.Options := [];
  aResult.VType := ftInt64;
  aResult.VInt64 := aValue;
end;

procedure TOrmVirtualTableCursor.SetColumn(var aResult: TSqlVar;
  const aValue: double);
begin
  aResult.Options := [];
  aResult.VType := ftDouble;
  aResult.VDouble := aValue;
end;

procedure TOrmVirtualTableCursor.SetColumn(var aResult: TSqlVar;
  const aValue: RawUtf8);
begin
  aResult.Options := [];
  aResult.VType := ftUtf8;
  fColumnTemp := aValue; // temporary copy available until next Column() call
  aResult.VText := pointer(fColumnTemp);
end;

procedure TOrmVirtualTableCursor.SetColumn(var aResult: TSqlVar;
  aValue: PUtf8Char; aValueLength: integer);
begin
  aResult.Options := [];
  aResult.VType := ftUtf8;
  FastSetString(RawUtf8(fColumnTemp), aValue, aValueLength); // temporary copy
  aResult.VText := pointer(fColumnTemp);
end;

procedure TOrmVirtualTableCursor.SetColumnBlob(var aResult: TSqlVar;
  aValue: pointer; aValueLength: integer);
begin
  aResult.Options := [];
  aResult.VType := ftBlob;
  FastSetRawByteString(fColumnTemp, aValue, aValueLength); // temporary copy
  aResult.VBlob := pointer(fColumnTemp);
  aResult.VBlobLen := aValueLength;
end;

procedure TOrmVirtualTableCursor.SetColumnDate(var aResult: TSqlVar;
  const aValue: TDateTime; aWithMS: boolean);
begin
  if aWithMS then
    aResult.Options := [svoDateWithMS]
  else
    aResult.Options := [];
  aResult.VType := ftDate;
  aResult.VDateTime := aValue;
end;

procedure TOrmVirtualTableCursor.SetColumnCurr64(var aResult: TSqlVar;
  aValue64: PInt64);
begin
  aResult.Options := [];
  aResult.VType := ftCurrency;
  PInt64(@aResult.VCurrency)^ := aValue64^;
end;



{ TOrmVirtualTableCursorIndex }

function TOrmVirtualTableCursorIndex.HasData: boolean;
begin
  result := (self <> nil) and
            (fCurrent <= fMax);
end;

function TOrmVirtualTableCursorIndex.Next: boolean;
begin
  if self = nil then
    result := false
  else
  begin
    if fCurrent <= fMax then
      inc(fCurrent);
    result := true;
  end;
end;

function TOrmVirtualTableCursorIndex.Search(
  var Prepared: TOrmVirtualTablePrepared): boolean;
begin
  fCurrent := 0; // mark EOF by default
  fMax := -1;
  result := true;
end;



function _GetVirtualTableModuleName(VirtualTableClass: TClass): RawUtf8;
begin
  if VirtualTableClass.InheritsFrom(TOrmVirtualTable) then
    result := TOrmVirtualTableClass(VirtualTableClass).ModuleName
  else
    result := '';
end;



{ ************ TRestStorage Abstract Class for ORM/REST Storage }

{ TRestStorage }

constructor TRestStorage.Create(aClass: TOrmClass; aServer: TRestOrmServer);
begin
  if aServer <> nil then // may be nil for a stand-alone single TOrm engine
    fRest := aServer.Owner; // redirect high-level methods to the main TRestServer
  inherited Create(nil);
  if aClass = nil then
    raise ERestStorage.CreateUtf8('%.Create(aClass=nil)', [self]);
  InitializeCriticalSection(fStorageCriticalSection);
  fStoredClass := aClass;
  fStoredClassRecordProps := aClass.OrmProps;
  if aServer <> nil then
  begin
    fOwner := aServer;
    fModel := aServer.Model;
  end
  else if fModel = nil then
  begin
    // fallback to an owned model instance
    fModel := TOrmModel.Create([aClass]);
    fModel.Owner := self;
  end;
  fStoredClassProps := fModel.Props[aClass];
  fStoredClassMapping := @fStoredClassProps.ExternalDB;
  fBasicSqlCount := 'SELECT COUNT(*) FROM ' +
                    fStoredClassRecordProps.SqlTableName;
  fBasicSqlHasRows[false] := 'SELECT RowID FROM ' +
                             fStoredClassRecordProps.SqlTableName + ' LIMIT 1';
  fBasicSqlHasRows[true] := fBasicSqlHasRows[false];
  system.delete(fBasicSqlHasRows[true], 8, 3);
  GetMem(fTempBuffer, SizeOf(fTempBuffer^)); // 8KB pre-allocated buffer
end;

destructor TRestStorage.Destroy;
begin
  inherited;
  {$ifdef DEBUGSTORAGELOCK}
  if fStorageCriticalSectionCount <> 0 then
    raise ERestStorage.CreateUtf8('%.Destroy with CS=%',
      [self, fStorageCriticalSectionCount]);
  {$endif DEBUGSTORAGELOCK}
  DeleteCriticalSection(fStorageCriticalSection);
  if fStorageVirtual <> nil then
  begin
    // no GPF e.g. if DB release after server
    fStorageVirtual.fStatic := nil;
    fStorageVirtual.fStaticStorage := nil;
  end;
  FreeMem(fTempBuffer);
end;

function TRestStorage.CreateSqlMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean; IndexName: RawUtf8): boolean;
begin
  result := false; // not implemented in this basic REST static class
end;

function TRestStorage.SearchField(const FieldName: RawUtf8; FieldValue: Int64;
  out ResultID: TIDDynArray): boolean;
begin
  result := SearchField(FieldName, Int64ToUtf8(FieldValue), ResultID);
end;

function TRestStorage.SearchField(const FieldName, FieldValue: RawUtf8;
  out ResultID: TIDDynArray): boolean;
begin
  result := OneFieldValues(fStoredClass, 'ID',
    FormatUtf8('%=?', [FieldName, FieldValue]), TInt64DynArray(ResultID));
end;

function TRestStorage.GetFieldIndex(
  const FieldName: RawUtf8; out Index: integer): boolean;
begin
  result := true;
  if IsRowID(pointer(FieldName)) then
    Index := 0
  else
  begin
    Index := fStoredClassRecordProps.Fields.IndexByName(pointer(FieldName));
    if Index >= 0 then
      inc(Index) // FindWhereEqual() expects index = RTTI+1
    else
      result := false;
  end;
end;

function TRestStorage.RecordCanBeUpdated(Table: TOrmClass; ID: TID;
  Action: TOrmEvent; ErrorMsg: PRawUtf8 = nil): boolean;
begin
  result := (fOwner = nil) or
            fOwner.RecordCanBeUpdated(Table, ID, Action, ErrorMsg);
end;

function TRestStorage.RefreshedAndModified: boolean;
begin
  result := false; // no refresh necessary with "normal" static tables
end;

procedure TRestStorage.StorageLock(WillModifyContent: boolean
  {$ifdef DEBUGSTORAGELOCK}; const msg: shortstring {$endif});
begin
  {$ifdef DEBUGSTORAGELOCK}
  if true or //fStorageLockLogTrace or
     (fStorageCriticalSectionCount > 1) then
    InternalLog('StorageLock % [%] %',
      [fStoredClass, msg, fStorageCriticalSectionCount]);
  {$endif DEBUGSTORAGELOCK}
  mormot.core.os.EnterCriticalSection(fStorageCriticalSection);
  {$ifdef DEBUGSTORAGELOCK}
  inc(fStorageCriticalSectionCount);
  {$endif DEBUGSTORAGELOCK}
  if WillModifyContent and
     fStorageLockShouldIncreaseOwnerInternalState and
     (fOwner <> nil) then
    inc(fOwner.InternalState);
end;

procedure TRestStorage.StorageUnLock;
begin
  {$ifdef DEBUGSTORAGELOCK}
  dec(fStorageCriticalSectionCount);
  if true then // fStorageLockLogTrace then
    InternalLog('StorageUnlock % %',
      [fStoredClass, fStorageCriticalSectionCount]);
  if fStorageCriticalSectionCount < 0 then
    raise ERestStorage.CreateUtf8('%.StorageUnLock with CS=%',
      [self, fStorageCriticalSectionCount]);
  {$endif DEBUGSTORAGELOCK}
  mormot.core.os.LeaveCriticalSection(fStorageCriticalSection);
end;

function TRestStorage.GetCurrentSessionUserID: TID;
begin
  if fOwner = nil then
    result := 0
  else
    result := fOwner.GetCurrentSessionUserID;
end;

procedure TRestStorage.RecordVersionFieldHandle(Occasion: TOrmOccasion;
  var Decoder: TJsonObjectDecoder);
begin
  // caller should ensure that fStoredClassRecordProps.RecordVersionField <> nil
  if fOwner = nil then
    raise ERestStorage.CreateUtf8('Owner=nil for %.%: TRecordVersion',
      [fStoredClass, fStoredClassRecordProps.RecordVersionField.Name]);
  fOwner.Owner.RecordVersionHandle(Occasion, fStoredClassProps.TableIndex,
    Decoder, fStoredClassRecordProps.RecordVersionField);
end;

function TRestStorage.UnLock(Table: TOrmClass; aID: TID): boolean;
begin
  result := Model.UnLock(Table, aID);
end;

function TRestStorage.AdaptSqlForEngineList(var SQL: RawUtf8): boolean;
begin
  if fStoredClassProps = nil then
    result := false
  else
  begin
    result := IdemPropNameU(fStoredClassProps.Sql.SelectAllWithRowID, SQL);
    if result then
      SQL := fStoredClassProps.SQL.SelectAllWithID
    else
      result := IdemPropNameU(fStoredClassProps.Sql.SelectAllWithID, SQL);
  end;
end;

function TRestStorage.GetStoredClassName: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    ClassToText(fStoredClass, result);
end;



{ ************ TRestStorageInMemory as Stand-Alone JSON/Binary Storage }


{ TRestStorageTOrm }

function TRestStorageTOrm.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  rec: TOrm;
begin
  result := 0; // mark error
  if TableModelIndex <> fStoredClassProps.TableIndex then
    exit;
  rec := fStoredClass.Create;
  try
    rec.FillFrom(SentData);
    StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'EngineAdd'{$endif});
    try
      result := AddOne(rec, rec.IDValue > 0, SentData);
    finally
      StorageUnLock;
    end;
  finally
    if result <= 0 then
      rec.Free; // on success, rec is owned by fValue: TObjectList
  end;
end;

function BatchExtractSimpleID(var Sent: PUtf8Char): TID;
var
  info: TGetJsonField;
begin
  if Sent^ = '[' then
    inc(Sent);
  info.Json := Sent;
  info.GetJsonField;
  if info.Json = nil then
  begin
    result := 0; // clearly invalid input
    exit;
  end;
  result := GetInt64(info.Value);
  Sent := info.Json - 1;
  Sent^ := '['; // ignore the first field (stored in fBatch.ID)
end;

function TRestStorageTOrm.InternalBatchDirectSupport(
  Encoding: TRestBatchEncoding; RunTableIndex: integer): TRestOrmBatchDirect;
begin
  if Encoding in BATCH_DIRECT then
    result := dirWriteNoLock
  else
    result := dirUnsupported;
end;

function TRestStorageTOrm.InternalBatchDirectOne(Encoding: TRestBatchEncoding;
  RunTableIndex: integer; const Fields: TFieldBits; Sent: PUtf8Char): TID;
var
  rec: TOrm;
begin
  // called a second time with the proper JSON array
  result := 0;
  if Encoding in [encPutHexID, encPostHexID] then
  begin
    result := BatchExtractSimpleID(Sent);
    if result <= 0 then
      exit; // invalid input
  end;
  // same logic than EngineAdd/EngineUpdate but with no memory alloc
  rec := fStoredClass.Create;
  try
    if rec.FillFromArray(Fields, Sent) then
    begin
      rec.IDValue := result;
      if Encoding = encPutHexID then
        if UpdateOne(rec, Fields, '') then // no SentData
          result := HTTP_SUCCESS
        else
          result := HTTP_NOTFOUND
      else
      begin
        StorageLock(true
          {$ifdef DEBUGSTORAGELOCK}, 'InternalBatchDirectOne' {$endif});
        try
          result := AddOne(rec, result > 0, ''); // no SentData
        finally
          StorageUnLock;
        end;
      end;
    end;
  finally
    if (Encoding = encPutHexID) or
       (result <= 0) then
      rec.Free; // on AddOne success, rec is owned by fValue: TObjectList
  end;
end;

function TRestStorageTOrm.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUtf8): boolean;
var
  rec: TOrm;
  fields: TFieldBits; // to handle partial fields update
begin
  if (ID <= 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
  begin
    result := false; // mark error
    exit;
  end;
  rec := fStoredClass.Create;
  try
    rec.FillFrom(SentData, @fields);
    rec.IDValue := ID;
    result := UpdateOne(rec, fields, SentData);
  finally
    rec.Free;
  end;
end;

function TRestStorageTOrm.UpdateOne(ID: TID;
  const Values: TSqlVarDynArray): boolean;
var
  rec: TOrm;
  json: RawUtf8;
begin
  if ID <= 0 then
  begin
    result := false; // mark error
    exit;
  end;
  rec := fStoredClass.Create;
  try
    rec.SetFieldSqlVars(Values);
    rec.IDValue := ID;
    GetJsonValue(rec, {withID=}false, fStoredClassRecordProps.CopiableFieldsBits, json);
    result := UpdateOne(rec, fStoredClassRecordProps.CopiableFieldsBits, json);
  finally
    rec.Free;
  end;
end;


{ TRestStorageInMemoryUnique }

constructor TRestStorageInMemoryUnique.Create(aOwner: TRestStorageInMemory;
  aField: TOrmPropInfo);
var
  hash: TOnDynArrayHashOne;
  cmp: TOnDynArraySortCompare;
begin
  fOwner := aOwner;
  fPropInfo := aField;
  fCaseInsensitive := not (aBinaryCollation in aField.Attributes);
  if fCaseInsensitive then
  begin
    hash := fPropInfo.EventHashI;
    cmp := fPropInfo.EventCompareI;
  end
  else
  begin
    hash := fPropInfo.EventHash;
    cmp := fPropInfo.EventCompare;
  end;
  fHasher.Init(@fOwner.fValues, nil, hash, nil, nil, cmp, false);
end;

function TRestStorageInMemoryUnique.Find(Rec: TOrm): integer;
begin
  if self = nil then // no Unique index for this field
    result := -1
  else
  begin
    fLastFindHashCode := fPropInfo.GetHash(Rec, fCaseInsensitive);
    result := fHasher.Find(@Rec, fLastFindHashCode);
  end;
end;

function TRestStorageInMemoryUnique.AddedAfterFind(Rec: TOrm): boolean;
begin
  fHasher.FindBeforeAdd(@Rec, result, fLastFindHashCode);
end;


{ TRestStorageInMemory }

constructor TRestStorageInMemory.Create(aClass: TOrmClass;
  aServer: TRestOrmServer; const aFileName: TFileName; aBinaryFile: boolean);
var
  f, n: PtrInt;
  fields: TOrmPropInfoList;
begin
  // setup the storage instance
  inherited Create(aClass, aServer);
  if (fStoredClassProps <> nil) and
     (fStoredClassProps.Kind in INSERT_WITH_ID) then
    raise ERestStorage.CreateUtf8('%.Create: % virtual table can''t be static',
      [self, aClass]);
  fFileName := aFileName;
  fBinaryFile := aBinaryFile;
  fSearchRec := fStoredClass.Create; // used to searched values
  // hashed and compared by ID, with proper T*ObjArray (fake) RTTI information
  fValues.InitRtti(fStoredClassRecordProps.TableObjArrayRtti, fValue,
    TObjectWithIDDynArrayHashOne, TObjectWithIDDynArrayCompare, nil, @fCount);
  // setup SELECT statements as used by AdaptSqlForEngineList() method
  if (ClassType <> TRestStorageInMemory) and
     (fStoredClassProps <> nil) then
    with fStoredClassProps do
    begin
      fBasicUpperSqlSelect[false] := UpperCase(Sql.SelectAllWithRowID);
      SetLength(fBasicUpperSqlSelect[false],
        length(fBasicUpperSqlSelect[false]) - 1); // trim right ';'
      fBasicUpperSqlSelect[true] :=
        StringReplaceAll(fBasicUpperSqlSelect[false], ' ROWID,', ' ID,');
    end;
  // initialize fUnique[] fUniquePerField[] lookup tables
  fields := fStoredClassRecordProps.Fields;
  n := FieldBitCount(fStoredClassRecordProps.IsUniqueFieldsBits);
  if n > 0 then
  begin
    SetLength(fUnique, n);
    SetLength(fUniquePerField, fields.Count);
    n := 0;
    for f := 0 to fields.Count - 1 do
      if FieldBitGet(fStoredClassRecordProps.IsUniqueFieldsBits, f) then
      begin
        fUnique[n] := TRestStorageInMemoryUnique.Create(self, fields.List[f]);
        fUniquePerField[f] := fUnique[n];
        inc(n);
      end;
  end;
  ReloadFromFile;
end;

destructor TRestStorageInMemory.Destroy;
begin
  UpdateFile;
  ObjArrayClear(fUnique);
  fValues.Clear; // to free all stored TOrm instances
  fSearchRec.Free;
  inherited Destroy;
end;

function TRestStorageInMemory.GetID(Index: integer): TID;
begin
  if (self = nil) or
     (cardinal(Index) >= cardinal(fCount)) then
    result := 0
  else
    result := fValue[Index].IDValue;
end;

procedure TRestStorageInMemory.RaiseGetItemOutOfRange(Index: integer);
begin
  raise ERestStorage.CreateUtf8('%.GetItem(%) over % is out of range (Count=%)',
    [self, Index, fStoredClass, fCount]);
end;

function TRestStorageInMemory.GetItem(Index: integer): TOrm;
begin
  result := nil;
  if self <> nil then
    if cardinal(Index) >= cardinal(fCount) then
      RaiseGetItemOutOfRange(Index)
    else
      result := fValue[Index];
end;

procedure TRestStorageInMemory.InternalTrackChangeUpdated(aRec: TOrm;
  const Fields: TFieldBits);
var
  b: PFieldBits;
begin
  b := pointer(PAnsiChar(aRec) + fTrackChangesFieldBitsOffset);
  {$if SizeOf(TFieldBits) = 8}
  PInt64(b)^ := PInt64(b)^ or PInt64(@Fields)^;
  {$else}
  b^ := b^ + Fields;
  {$ifend}
end;

function TRestStorageInMemory.IDToIndex(ID: TID): PtrInt;
begin
  if self <> nil then
  begin
    fSearchRec.IDValue := ID;
    result := fValues.FindHashed(fSearchRec);
  end
  else
    result := -1;
end;

function TRestStorageInMemory.AddOne(Rec: TOrm; ForceID: boolean;
  const SentData: RawUtf8; ExistingIndex: PPtrInt): TID;
var
  ndx, f: PtrInt;
  added: boolean;
begin
  result := -1; // error
  if (self = nil) or
     (Rec = nil) then
    exit;
  // ensure no duplicated ID or unique field
  for f := 0 to length(fUnique) - 1 do
  begin
    ndx := fUnique[f].Find(Rec);
    if ndx >= 0 then
    begin
      if ExistingIndex <> nil then
        ExistingIndex^ := ndx
      else
        InternalLog('AddOne: non unique %.% on % %',
          [fStoredClass, fUnique[f].PropInfo.Name, fValue[ndx], Rec], sllDB);
      exit;
    end;
  end;
  if ForceID then
  begin
    if Rec.IDValue <= 0 then
      raise ERestStorage.CreateUtf8('%.AddOne(%.ForceID=0)', [self, Rec]);
    ndx := fValues.FindHashed(Rec);
    if ndx >= 0 then
    begin
      if ExistingIndex <> nil then
        ExistingIndex^ := ndx
      else
        InternalLog('AddOne: non unique %.ID on % %',
          [fStoredClass, fValue[ndx], Rec], sllDB);
      exit;
    end;
    if Rec.IDValue > fMaxID then
      fMaxID := Rec.IDValue
    else
      fUnSortedID := true;
  end
  else
  begin
    inc(fMaxID); // increasing sequence
    Rec.IDValue := fMaxID;
  end;
  // update internal hash tables and add to internal list
  for f := 0 to length(fUnique) - 1 do
    if not fUnique[f].AddedAfterFind(Rec) then // paranoid
      raise ERestStorage.CreateUtf8('%.AddOne on %.%',
        [self, Rec, fUnique[f].PropInfo.Name]);
  ndx := fValues.FindHashedForAdding(Rec, added);
  if added then
    fValue[ndx] := Rec
  else
    raise ERestStorage.CreateUtf8('%.AddOne % failed', [self, Rec]); // paranoid
  result := Rec.IDValue; // success
  fModified := true;
  if (fOwner <> nil) then
    fOwner.InternalUpdateEvent(
      oeAdd, fStoredClassProps.TableIndex, result, SentData, nil, Rec);
  if fTrackChangesFieldBitsOffset <> 0 then
    InternalTrackChangeUpdated(Rec, ALL_FIELDS); // ALL_FIELDS means Add()
end;

function TRestStorageInMemory.UniqueFieldsUpdateOK(aRec: TOrm;
  aUpdateIndex: integer; aFields: PFieldBits): boolean;
var
  f, ndx: PtrInt;
begin
  result := false;
  for f := 0 to length(fUnique) - 1 do
    with fUnique[f] do
      if (aFields = nil) or
         FieldBitGet(aFields^, PropInfo.PropertyIndex) then
      begin
        ndx := Find(aRec);
        if (ndx >= 0) and
           (ndx <> aUpdateIndex) then
        begin
          InternalLog('UniqueFieldsUpdateOK failed on % for %',
            [PropInfo.Name, aRec], sllDB);
          exit;
        end;
      end;
  result := true;
end;

function TRestStorageInMemory.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  if (self = nil) or
     (ID <= 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
    result := false
  else
  begin
    StorageLock(True {$ifdef DEBUGSTORAGELOCK}, 'EngineDelete'{$endif});
    try
      result := DeleteOne(IDToIndex(ID));
    finally
      StorageUnLock;
    end;
  end;
end;

function FindMaxID(p: POrm; n: integer): TID;
var
  id: TID;
begin
  result := 0;
  if n > 0 then
    repeat
      id := p^.IDValue;
      if id > result then // branchless cmovg on 64-bit FPC
        result := id;
      inc(p);
      dec(n);
    until n = 0;
end;

function FindMaxIDAndCheckSorted(p: POrm; n: integer;
  var unsorted: boolean): TID;
var
  id, prev: TID;
  {$ifndef CPUX86}
  lastnotsorted: pointer;
  {$endif CPUX86}
begin
  prev := 0;
  result := 0;
  {$ifdef CPUX86}
  unsorted := false;
  {$else}
  lastnotsorted := nil;
  {$endif CPUX86}
  if n > 0 then
    repeat
      id := p^.IDValue;
      if id > result then   // cmovg on 64-bit FPC
        result := id;
      if id <= prev then
        {$ifdef CPUX86}
        unsorted := true;
        {$else}
        lastnotsorted := p; // cmovle on 64-bit FPC
        {$endif CPUX86}
      prev := id;
      inc(p);
      dec(n);
    until n = 0;
  {$ifndef CPUX86}
  unsorted := lastnotsorted <> nil;
  {$endif CPUX86}
end;

function TRestStorageInMemory.DeleteOne(aIndex: integer): boolean;
var
  f: PtrInt;
  id: TID;
  rec: TOrm;
begin
  if cardinal(aIndex) >= cardinal(fCount) then
    result := false
  else
  begin
    rec := fValue[aIndex];
    id := rec.IDValue;
    if id = fMaxID then
      fMaxID := 0; // recompute
    if fOwner <> nil then
      // notify BEFORE deletion
      fOwner.InternalUpdateEvent(oeDelete, fStoredClassProps.TableIndex,
        id, '', nil, nil);
    for f := 0 to length(fUnique) - 1 do
      if fUnique[f].Hasher.FindBeforeDelete(@rec) < aIndex then
        raise ERestStorage.CreateUtf8('%.DeleteOne(%) failed on %',
          [self, aIndex, fUnique[f].PropInfo.Name]);
    if fValues.FindHashedAndDelete(rec) <> aIndex then
      raise ERestStorage.CreateUtf8('%.DeleteOne(%) failed', [self, aIndex]);
    if fMaxID = 0 then
      fMaxID := FindMaxID(pointer(fValue), fCount);
    if (fTrackChangesFieldBitsOffset <> 0) and
       not Int64ScanExists(pointer(fTrackChangesDeleted), fTrackChangesDeletedCount, id) then
      AddInt64(fTrackChangesDeleted, fTrackChangesDeletedCount, id);
      // note: any previous Add/Update are deleted with rec since TrackChanges
      // is part of it - no need to manually clean them
    fModified := true;
    result := true;
  end;
end;

function TRestStorageInMemory.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  ndx: TIntegerDynArray;
  n, i: PtrInt;
begin
  // RecordCanBeUpdated() has already been called
  result := false;
  n := length(IDs);
  SetLength(ndx, n);
  dec(n);
  StorageLock(True {$ifdef DEBUGSTORAGELOCK}, 'EngineDeleteWhere' {$endif});
  try
    for i := 0 to n do
    begin
      if IDs[i] = fMaxID then
        fMaxID := 0; // force recompute
      ndx[i] := IDToIndex(IDs[i]);
      if ndx[i] < 0 then
        exit;
    end;
    QuickSortInteger(pointer(ndx), 0, n); // slightly faster in reverse order
    for i := n downto 0 do
      DeleteOne(ndx[i]);
    if fMaxID = 0 then
      fMaxID := FindMaxID(pointer(fValue), fCount);
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.EngineExecute(const aSql: RawUtf8): boolean;
begin
  result := false; // there is no SQL engine with this class
end;

procedure TRestStorageInMemory.GetJsonValuesEvent(aDest: pointer;
  aRec: TOrm; aIndex: integer);
var
  W: TOrmWriter absolute aDest;
begin
  aRec.GetJsonValues(W);
  W.AddComma;
end;

function TRestStorageInMemory.AdaptSqlForEngineList(var SQL: RawUtf8): boolean;
var
  P: PUtf8Char;
  Prop: RawUtf8;
  WithoutRowID: boolean;
begin
  result := inherited AdaptSqlForEngineList(SQL);
  if result then
    // 'select * from table'
    exit;
  if IdemPropNameU(fBasicSqlCount, SQL) or
     IdemPropNameU(fBasicSqlHasRows[false], SQL) or
     IdemPropNameU(fBasicSqlHasRows[true], SQL) then
  begin
    // 'select count(*) from table' will be handled as static
    result := true;
    exit;
  end;
  if fBasicUpperSqlSelect[false] = '' then
    exit;
  if IdemPChar(pointer(SQL), pointer(fBasicUpperSqlSelect[false])) then
    WithoutRowID := false
  else if IdemPChar(pointer(SQL), pointer(fBasicUpperSqlSelect[true])) then
    WithoutRowID := true
  else
    exit;
  P := pointer(SQL);
  inc(P, length(fBasicUpperSqlSelect[WithoutRowID]));
  if P^ in [#0, ';'] then
  begin
    // properly ended the WHERE clause as 'SELECT * FROM table'
    result := true;
    exit;
  end;
  P := GotoNextNotSpace(P);
  if not IdemPChar(P, 'WHERE ') then
  begin
    if IdemPChar(P, 'LIMIT ') then
      result := true;
    exit;
  end;
  P := GotoNextNotSpace(P + 6);
  GetNextItem(P, '=', Prop);
  if (P = nil) or
     (fStoredClassRecordProps.Fields.IndexByName(Prop) < 0) then
    exit;
  if PWord(P)^ = ord(':') + ord('(') shl 8 then
    inc(P, 2); // +2 to ignore :(...): parameter
  if P^ in ['''', '"'] then
  begin
    P := GotoEndOfQuotedString(P);
    if not (P^ in ['''', '"']) then
      exit;
  end;
  repeat
    inc(P)
    // go to end of value
  until P^ in [#0..' ', ';', ')'];
  if PWord(P)^ = ord(')') + ord(':') shl 8 then
    inc(P, 2); // ignore :(...): parameter
  P := GotoNextNotSpace(P);
  if (P^ in [#0, ';']) or
     IdemPChar(P, 'LIMIT ') then
    // properly ended the WHERE clause as 'FIELDNAME=value'
    result := true;
end;

function TRestStorageInMemory.FindWhereEqual(
  const WhereFieldName, WhereValue: RawUtf8; const OnFind: TOnFindWhereEqual;
  Dest: pointer; FoundLimit, FoundOffset: integer; CaseInsensitive: boolean): PtrInt;
var
  ndx: integer;
begin
  if (self = nil) or
     (not Assigned(OnFind)) or
     (fCount = 0) or
     not GetFieldIndex(WhereFieldName, ndx) then
    result := 0
  else
    result := FindWhereEqual(ndx, WhereValue, OnFind, Dest,
      FoundLimit, FoundOffset, CaseInsensitive);
end;

function TRestStorageInMemory.FindWhereEqual(WhereField: integer;
  const WhereValue: RawUtf8; const OnFind: TOnFindWhereEqual; Dest: pointer;
  FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean): PtrInt;
var
  i, found: PtrInt;
  v: Int64;
  err: integer;
  P: TOrmPropInfo;
  nfo: PRttiProp;
  offs: PtrUInt;
  vp: PPtrUInt;

  function FoundOneAndReachedLimit: boolean;
  begin
    result := false; // continue search
    if FoundOffset > 0 then
    begin
      // omit first FoundOffset rows
      dec(FoundOffset);
      if FoundOffset >= 0 then
        exit;
    end;
    if Assigned(OnFind) then
      OnFind(Dest, fValue[i], i);
    inc(found);
    if found >= FoundLimit then
      result := true; // stop the loop
  end;

begin
  result := 0;
  if fCount = 0 then
    exit;
  if FoundLimit <= 0 then
    FoundLimit := maxInt;
  if WhereField = 0 then
  begin
    // search ID
    if FoundOffset <= 0 then // omit first FoundOffset rows
    begin
      v := GetInt64(pointer(WhereValue), err);
      if (err = 0) and
         (v > 0) then
      begin
        i := IDToIndex(v); // use fast ID hash table
        if i >= 0 then
        begin
          if Assigned(OnFind) then
            OnFind(Dest, fValue[i], i);
          inc(result);
        end;
      end;
    end;
    exit;
  end
  else if cardinal(WhereField) > cardinal(fStoredClassRecordProps.Fields.Count) then
    exit;
  // handle WHERE WhereField=WhereValue (WhereField=RTTIfield+1)
  dec(WhereField);
  P := fStoredClassRecordProps.Fields.List[WhereField];
  if not (P.OrmFieldType in COPIABLE_FIELDS) then
    // nothing to search (e.g. oftUnknown or oftMany)
    exit;
  // use fUnique[] hash array for O(1) search if available
  if FieldBitGet(fStoredClassRecordProps.IsUniqueFieldsBits, WhereField) then
  begin
    if FoundOffset <= 0 then
    begin
      // omit first FoundOffset rows
      P.SetValueVar(fSearchRec, WhereValue, false); // private copy for comparison
      i := fUniquePerField[WhereField].Find(fSearchRec);
      if i >= 0 then
      begin
        if Assigned(OnFind) then
          OnFind(Dest, fValue[i], i);
        inc(result);
      end;
    end;
    exit;
  end;
  // full scan optimized search for a specified value
  found := 0;
  case P.PropInfoClass of
    picInt32:
      begin
        // search 8/16/32-bit properties
        if not GetInt64Bool(pointer(WhereValue), v) then // 64-bit for cardinal
          exit;
        vp := pointer(fValue);
        nfo := TOrmPropInfoRtti(P).PropInfo;
        offs := TOrmPropInfoRtti(P).GetterIsFieldPropOffset;
        if offs <> 0 then
          // plain field with no getter
          case TOrmPropInfoRtti(P).PropRtti.Cache.Size of
            SizeOf(Byte):
              // e.g. boolean property
              for i := 0 to fCount - 1 do
                if (PByte(vp^ + offs)^ = PByte(@v)^) and
                   FoundOneAndReachedLimit then
                  break
                else
                  inc(vp);
            SizeOf(Word):
              for i := 0 to fCount - 1 do
                if (PWord(vp^ + offs)^ = PWord(@v)^) and
                   FoundOneAndReachedLimit then
                  break
                else
                  inc(vp);
            SizeOf(Cardinal):
              // handle very common 32-bit integer field
              for i := 0 to fCount - 1 do
                if (PCardinal(vp^ + offs)^ = PCardinal(@v)^) and
                   FoundOneAndReachedLimit then
                  break
                else
                  inc(vp);
          end
        else
          // has getter -> use GetOrdProp()
          for i := 0 to fCount - 1 do
            if (nfo^.GetOrdProp(pointer(vp^)) = v) and
               FoundOneAndReachedLimit then
              break
            else
              inc(vp);
      end;
    picInt64:
      begin
        // search 64-bit integer property
        v := GetInt64(pointer(WhereValue), err);
        if err <> 0 then
          exit;
        nfo := TOrmPropInfoRtti(P).PropInfo;
        offs := TOrmPropInfoRtti(P).GetterIsFieldPropOffset;
        if offs <> 0 then
        begin
          // plain field with no getter
          vp := pointer(fValue);
          for i := 0 to fCount - 1 do
            if (PInt64(vp^ + offs)^ = v) and
               FoundOneAndReachedLimit then
              break
            else
              inc(vp);
        end
        else
          // search using the getter method
          for i := 0 to fCount - 1 do
            if (nfo^.GetInt64Prop(fValue[i]) = v) and
               FoundOneAndReachedLimit then
              break;
      end;
  else
    begin
      // generic search using fast CompareValue() overridden method
      P.SetValueVar(fSearchRec, WhereValue, false); // private copy for comparison
      for i := 0 to fCount - 1 do
        if (P.CompareValue(fValue[i], fSearchRec, CaseInsensitive) = 0) and
           FoundOneAndReachedLimit then
          break;
    end;
  end;
  result := found;
end;

function TRestStorageInMemory.FindWhere(WhereField: integer;
  const WhereValue: RawUtf8; WhereOp: TSelectStatementOperator;
  const OnFind: TOnFindWhereEqual; Dest: pointer;
  FoundLimit, FoundOffset: integer; CaseInsensitive: boolean): PtrInt;
var
  P: TOrmPropInfo;
  cmp: integer;
  id: Int64;
  i: integer;
  found: boolean;
  v: POrm;
begin
  // prepare the search
  result := 0;
  if (self = nil) or
     (not Assigned(OnFind)) or
     (fCount = 0) then
    exit;
  case WhereOp of
    opEqualTo:
      begin
        // redirect to the optimized FindWhereEqual() method
        result := FindWhereEqual(WhereField, WhereValue, OnFind, Dest,
          FoundLimit, FoundOffset, CaseInsensitive);
        exit;
      end;
    opNotEqualTo,
    opLessThan,
    opLessThanOrEqualTo,
    opGreaterThan,
    opGreaterThanOrEqualTo:
      ; // CompareValue() operations
  else
    exit; // unsupported operation
  end;
  // generic search using fast CompareValue() overridden methods
  if FoundLimit <= 0 then
    FoundLimit := maxInt;
  if WhereField = 0 then
    if ToInt64(WhereValue, id) then
      P := nil // will search v^.IDValue
    else
      exit
  else
  begin
    P := fStoredClassRecordProps.Fields.List[WhereField - 1];
    P.SetValueVar(fSearchRec, WhereValue, false); // private copy for comparison
  end;
  v := pointer(fValue);
  for i := 0 to fCount - 1 do
  begin
    if P = nil then
      cmp := CompareInt64(v^.IDValue, id{%H-})
    else
      cmp := P.CompareValue(v^, fSearchRec, CaseInsensitive); // fast override
    if cmp < 0 then
      found := WhereOp in [opNotEqualTo, opLessThan, opLessThanOrEqualTo]
    else if cmp > 0 then
      found := WhereOp in [opNotEqualTo, opGreaterThan, opGreaterThanOrEqualTo]
    else
      // cmp = 0 -> opEqualTo has been handled above
      found := WhereOp in [opLessThanOrEqualTo, opGreaterThanOrEqualTo];
    if found then
      if FoundOffset > 0 then
        // omit first FoundOffset rows
        dec(FoundOffset)
      else
      begin
        // notify match found
        OnFind(Dest, v^, i);
        inc(result);
        if result >= FoundLimit then
          exit;
      end;
    inc(v);
  end;
end;

function TRestStorageInMemory.FindWhere(
  const WhereFieldName, WhereValue: RawUtf8; WhereOp: TSelectStatementOperator;
  const OnFind: TOnFindWhereEqual; Dest: pointer;
  FoundLimit, FoundOffset: integer; CaseInsensitive: boolean): PtrInt;
var
  ndx: integer;
begin
  if GetFieldIndex(WhereFieldName, ndx) then
    result := FindWhere(ndx, WhereValue, WhereOp, OnFind, Dest,
      FoundLimit, FoundOffset, CaseInsensitive)
  else
    result := 0;
end;

function TRestStorageInMemory.FindMax(WhereField: integer;
  out max: Int64): boolean;
var
  P: TOrmPropInfo;
  i: PtrInt;
  v: Int64;
begin
  result := false;
  max := low(Int64);
  if fCount = 0 then
    exit;
  if WhereField = 0 then
  begin
    // return the known Max(RowID) value
    max := fMaxID;
    result := true;
    exit;
  end;
  if cardinal(WhereField) > cardinal(fStoredClassRecordProps.Fields.Count) then
    exit;
  // search WHERE WhereField=WhereValue (WhereField=RTTIfield+1)
  dec(WhereField);
  P := fStoredClassRecordProps.Fields.List[WhereField];
  case P.PropInfoClass of
    picInt32:
      begin
        for i := 0 to fCount - 1 do
        begin
          v := TOrmPropInfoRttiInt32(P).GetValueInt32(fValue[i]);
          if v > max then
            max := v;
        end;
        result := true;
      end;
    picInt64:
      begin
        for i := 0 to fCount - 1 do
        begin
          v := TOrmPropInfoRttiInt64(P).GetValueInt64(fValue[i]);
          if v > max then
            max := v;
        end;
        result := true;
      end;
  end;
end;

procedure TRestStorageInMemory.ForEach(WillModifyContent: boolean;
  const OnEachProcess: TOnFindWhereEqual; Dest: pointer);
var
  i: PtrInt;
begin
  if (self = nil) or
     (fCount = 0) or
     (not Assigned(OnEachProcess)) then
    exit;
  StorageLock(WillModifyContent {$ifdef DEBUGSTORAGELOCK}, 'ForEach' {$endif});
  try
    for i := 0 to fCount - 1 do
      OnEachProcess(Dest, fValue[i], i);
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageInMemory.TrackChanges(FieldBitsOffset: pointer;
  const Persistence: IRestOrm);
begin
  if self = nil then
    exit;
  fTrackChangesFieldBitsOffset := 0;
  fTrackChangesPersistence := nil;
  if Persistence = nil then
    exit;
  if (FieldBitsOffset = nil) or
     (PtrUInt(FieldBitsOffset) > PtrUInt(fStoredClass.InstanceSize)) then
    raise ERestStorage.CreateUtf8('%.TrackChanges(%)', [self, FieldBitsOffset]);
  Persistence.Model.GetTableIndexExisting(fStoredClass); // ensure support table
  fTrackChangesFieldBitsOffset := PtrUInt(FieldBitsOffset);
  fTrackChangesPersistence := Persistence;
end;

procedure TRestStorageInMemory.TrackChangeUpdated(aRec: TOrm;
  const Fields: TFieldBits);
begin
  if (aRec <> nil) and
     (fTrackChangesFieldBitsOffset <> 0) and
     not IsZero(Fields) then
    InternalTrackChangeUpdated(aRec, Fields);
end;

procedure TRestStorageInMemory.TrackChangeUpdated(aRec: TOrm;
  const Fields: array of PUtf8Char);
var
  upd: TFieldBits;
begin
  if (aRec = nil) or
     (fTrackChangesFieldBitsOffset = 0) then
    exit;
  if high(Fields) < 0 then
    upd := StoredClassRecordProps.SimpleFieldsBits[ooUpdate]
  else if not StoredClassRecordProps.FieldBitsFrom(Fields, upd) then
    raise ERestStorage.CreateUtf8('Invalid %.TrackChangeUpdated', [self]);
  InternalTrackChangeUpdated(aRec, upd);
end;

function TRestStorageInMemory.TrackChangesAndFlush: boolean;
var
  batch: TRestBatch;
  b: PFieldBits;
  off, i: PtrInt;
  log: ISynLog;
  p, pEnd: POrm; // this is the fastest pattern to iterate over the list
begin
  result := true; // success (may be no write at all)
  off := fTrackChangesFieldBitsOffset;
  if (off = 0) or
     fTrackChangesAndFlushRunning or
     (fTrackChangesPersistence = nil) or
     (fCount = 0) then
    exit;
  fTrackChangesAndFlushRunning := true; // paranoid check if BatchSend is slow
  batch := TRestBatch.Create(fTrackChangesPersistence, fStoredClass);
  try
    // fill the batch instance with pending tracked changes
    StorageLock({modify=}false {$ifdef DEBUGSTORAGELOCK}, 'TrackChangesAndFlush'{$endif});
    try
      // delete any previous value before any Add/Update
      for i := 0 to fTrackChangesDeletedCount - 1 do
        batch.Delete(fTrackChangesDeleted[i]);
      if fTrackChangesDeletedCount > 10000 then
        fTrackChangesDeleted := nil;  // release memory only if > 16KB
      fTrackChangesDeletedCount := 0; // flush
      // first loop is for all Add() - to generate multi-insert if possible
      p := pointer(fValue);
      pEnd := @fValue[Count];
      while p <> pEnd do
      begin
        b := pointer(PAnsiChar(p^) + off);
        if IsAllFields(b^) then
        begin
          if log = nil then // only start logging if something is to be written
            log := logclass.Enter(self, 'TrackChangesAndFlush Add');
          batch.Add(p^, {senddata=}true, {forceid=}true, ALL_FIELDS, true);
          FillZero(b^); // flush
        end;
        inc(p);
      end;
      // next loop is for all Update() - not worth regrouping by fieldbits
      p := pointer(fValue);
      while p <> pEnd do
      begin
        b := pointer(PAnsiChar(p^) + off);
        if not IsZero(b^) then
        begin
          if log = nil then
            log := logclass.Enter(self, 'TrackChangesAndFlush Update');
          batch.Update(p^, b^, {DoNotAutoComputeFields=}true);
          FillZero(b^); // flush
        end;
        inc(p);
      end;
    finally
      StorageUnLock;
    end;
    // send the pending data as batch in the background thread
    if batch.Count > 0 then
      if fTrackChangesPersistence.BatchSend(batch) <> HTTP_SUCCESS then
      begin
        log.Log(sllError, 'TrackChangesAndFlush: SendBatch error', self);
        result := false; // notify error
      end;
  finally
    fTrackChangesAndFlushRunning := false;
    batch.Free;
  end;
end;

function TRestStorageInMemory.GetJsonValues(Stream: TStream; Expand: boolean;
  Stmt: TSelectStatement): PtrInt;
var
  ndx, KnownRowsCount, j: PtrInt;
  id: Int64;
  W: TOrmWriter;
  IsNull: boolean;
  Prop: TOrmPropInfo;
  bits: TFieldBits;
  withID: boolean;
  tmp: PTextWriterStackBuffer;
label
  err;
begin
  // exact same format as TOrmTable.GetJsonValues()
  result := 0;
  if length(Stmt.Where) > 1 then
    raise ERestStorage.CreateUtf8('%.GetJsonValues on % with Stmt.Where[]=%:' +
      ' our in-memory engine only supports a single WHERE clause operation',
      [self, fStoredClass, length(Stmt.Where)]);
  tmp := fTempBuffer; // aBufSize=65500 is ignored if tmp<>nil
  if Stmt.Where = nil then
    // no WHERE statement -> get all rows -> guess rows count
    if (Stmt.Limit > 0) and
       (fCount > Stmt.Limit) then
      KnownRowsCount := Stmt.Limit
    else
    begin
      KnownRowsCount := fCount;
      tmp := nil; // 64KB heap buffer when getting all rows - 8KB otherwise
    end
  else
    KnownRowsCount := 0;
  Stmt.SelectFieldBits(bits, withID);
  W := fStoredClassRecordProps.CreateJsonWriter(
    Stream, Expand, withID, bits, KnownRowsCount, 65500, tmp);
  if W <> nil then
  try
    if Expand then
      W.Add('[');
    if Stmt.Where = nil then
    begin
      // no WHERE statement -> all rows
      for ndx := 0 to KnownRowsCount - 1 do
      begin
        if Expand then
          W.AddCR; // for better readability
        fValue[ndx].GetJsonValues(W);
        W.AddComma;
      end;
      result := KnownRowsCount;
    end
    else
      case Stmt.Where[0].Operation of
        opEqualTo:
          result := FindWhereEqual(Stmt.Where[0].Field, Stmt.Where[0].Value,
            GetJsonValuesEvent, W, Stmt.Limit, Stmt.Offset);
        opNotEqualTo,
        opLessThan,
        opLessThanOrEqualTo,
        opGreaterThan,
        opGreaterThanOrEqualTo:
          with Stmt.Where[0] do
            result := FindWhere(Field, Value, Operation,
              GetJsonValuesEvent, W, Stmt.Limit, Stmt.Offset);
        opIn:
          // only handle  ID IN (..)  syntax by now
          if (Stmt.Where[0].Field <> 0) or
             (Stmt.Offset > 0) then
            goto err
          else
            with _Safe(Stmt.Where[0].ValueVariant, dvArray)^ do
              for ndx := 0 to Count - 1 do
                if VariantToInt64(Values[ndx], id) then
                begin
                  j := IDToIndex(id);
                  if j >= 0 then
                  begin
                    fValue[j].GetJsonValues(W);
                    W.AddComma;
                    inc(result);
                    if (Stmt.Limit > 0) and
                       (result >= Stmt.Limit) then
                      break;
                  end;
                end
                else
                  goto err;
        opIsNull,
        opIsNotNull:
          if Stmt.Where[0].Field > 0 then
          begin
            Prop := fStoredClassRecordProps.Fields.List[Stmt.Where[0].Field - 1];
            IsNull := Stmt.Where[0].Operation = opIsNull;
            for ndx := 0 to fCount - 1 do
              if Prop.IsValueVoid(fValue[ndx]) = IsNull then
              begin
                fValue[ndx].GetJsonValues(W);
                W.AddComma;
                inc(result);
                if (Stmt.Limit > 0) and
                  (result >= Stmt.Limit) then
                  break;
              end;
          end
          else
            goto err;
      else
        begin
err:      W.CancelAll;
          result := 0;
          exit;
        end;
      end;
    if (result = 0) and
       W.Expand then
    begin
      // we want the field names at least, even with no data
      W.Expand := false; //  {"fieldCount":2,"values":["col1","col2"]}
      W.CancelAll;
      fStoredClassRecordProps.SetJsonWriterColumnNames(W, 0);
    end;
    W.EndJsonObject(KnownRowsCount, result);
  finally
    W.Free;
  end;
end;

procedure TRestStorageInMemory.GetAllIDs(out ID: TIDDynArray);
var
  i: PtrInt;
begin
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'GetAllIDs' {$endif});
  try
    SetLength(ID, fCount);
    for i := 0 to Count - 1 do
      ID[i] := fValue[i].IDValue;
  finally
    StorageUnlock;
  end;
end;

function TRestStorageInMemory.EngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
// - GetJsonValues/FindWhere(Equal) will handle basic REST commands (not all SQL)
// - note: sufficient for OneFieldValue() and MultiFieldValue() to work
var
  MS: TRawByteStringStream;
  ResCount: PtrInt;
  Stmt: TSelectStatement;
  max: Int64;

  procedure SetCount(aCount: integer);
  begin
    FormatUtf8('[{"Count(*)":%}]'#$A, [aCount], result);
    ResCount := 1;
  end;

begin
  result := '';
  ResCount := 0;
  if IdemPropNameU(fBasicSqlCount, SQL) then
    SetCount(TableRowCount(fStoredClass))
  else if IdemPropNameU(fBasicSqlHasRows[false], SQL) or
          IdemPropNameU(fBasicSqlHasRows[true], SQL) then
    if TableHasRows(fStoredClass) then
    begin
      // return one expanded row with fake ID=1 - enough for the ORM usecase
      result := '[{"RowID":1}]'#$A;
      ResCount := 1;
    end
    else
    begin
      // return the not expanded field name if no row, as a regular SQL engine
      result := '{"fieldCount":1,"values":["RowID"]}'#$A;
      ResCount := 0;
    end
  else
  begin
    // parse SQL SELECT with a single where clause
    Stmt := TSelectStatement.Create(SQL,
      fStoredClassRecordProps.Fields.IndexByName,
      fStoredClassRecordProps.SimpleFieldSelect);
    try
      if (Stmt.SqlStatement = '') or // parsing failed
         (length(Stmt.Where) > 1) or // only a SINGLE expression is allowed yet
         not IdemPropNameU(Stmt.TableName, fStoredClassRecordProps.SqlTableName) then
        // invalid request -> return ''
        exit;
      if Stmt.SelectFunctionCount = 0 then
      begin
        // this is the main execution block, for regular SELECT statements
        MS := TRawByteStringStream.Create;
        try
          if fOwner <> nil then
            ForceAjax := ForceAjax or not fOwner.Owner.NoAjaxJson;
          StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'GetJsonValues' {$endif});
          try
            // save rows as JSON, with appropriate search according to Where.*
            ResCount := GetJsonValues(MS, ForceAjax, Stmt);
          finally
            StorageUnLock;
          end;
          result := MS.DataString;
        finally
          MS.Free;
        end;
      end
      else if (length(Stmt.Select) <> 1) or
              (Stmt.SelectFunctionCount <> 1) or
              (Stmt.Limit > 1) or
              (Stmt.Offset <> 0) then
        // handle a single max() or count() SQL function with no LIMIT nor OFFSET
        exit
      else
        // handle the known max() or count() SQL functions
        case Stmt.Select[0].FunctionKnown of
          funcCountStar:
            if Stmt.Where = nil then
              // was e.g. "SELECT Count(*) FROM TableName;"
              SetCount(TableRowCount(fStoredClass))
            else
            begin
              // was e.g. "SELECT Count(*) FROM TableName WHERE ..."
              StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'Count' {$endif});
              try
                ResCount := FindWhereEqual(Stmt.Where[0].Field,
                  Stmt.Where[0].Value, DoNothingEvent, nil, 0, 0);
                case Stmt.Where[0].Operation of
                  opEqualTo:
                    SetCount(ResCount);
                  opNotEqualTo:
                    SetCount(TableRowCount(fStoredClass) - ResCount);
                end;
              finally
                StorageUnLock;
              end;
            end;
          funcMax:
            if Stmt.Where = nil then
            begin
              StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'Max' {$endif});
              try
                if FindMax(Stmt.Select[0].Field, max) then
                begin
                  FormatUtf8('[{"Max()":%}]'#$A, [max], result);
                  ResCount := 1;
                end;
              finally
                StorageUnLock;
              end;
            end;
        else
          // unhandled Distinct() or other SQL functions
          exit;
        end;
    finally
      Stmt.Free;
    end;
  end;
  if ReturnedRowCount <> nil then
    ReturnedRowCount^ := ResCount;
end;

procedure TRestStorageInMemory.DropValues(andUpdateFile: boolean);
var
  f: PtrInt;
  timer: TPrecisionTimer;
begin
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'DropValues' {$endif});
  try
    fUnSortedID := false;
    fMaxID := 0;
    if fCount > 0 then
    begin
      timer.Start;
      fValues.Clear;
      for f := 0 to length(fUnique) - 1 do
        fUnique[f].Hasher.ForceReHash;
      fValues.Hasher.ForceReHash;
      if andUpdateFile then
      begin
        fModified := true;
        UpdateFile;
      end;
      InternalLog('DropValues % in %', [fStoredClass, timer.Stop]);
    end;
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageInMemory.LoadFromJson(const aJson: RawUtf8);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(aJson);
  try
    LoadFromJson(tmp.buf, tmp.len);
  finally
    tmp.Done;
  end;
end;

procedure TRestStorageInMemory.ComputeStateAfterLoad(
  var loaded: TPrecisionTimer; binary: boolean);
const
  _CALLER: array[boolean] of string[7] = (
    'Json', 'Binary');
var
  f: PtrInt;
  dup: integer; // should be an integer and not a PtrInt for ForceRehash(@dup)
  dupfield: RawUtf8;
  timer: TPrecisionTimer;
begin
  // now fValue[] contains the just loaded data
  loaded.Pause;
  timer.Start;
  fCount := length(fValue);
  fValues.Hasher.ForceReHash(@dup);
  if dup > 0 then
    dupfield := ID_TXT
  else
    for f := 0 to length(fUnique) - 1 do
    begin
      fUnique[f].Hasher.ForceReHash(@dup);
      if dup > 0 then
      begin
        dupfield := fUnique[f].PropInfo.Name;
        break;
      end;
    end;
  if dup > 0 then
  begin
    DropValues({andupdatefile=}false);
    raise ERestStorage.CreateUtf8('%.LoadFrom%: found % % in %.% field',
      [self, _CALLER[binary], Plural('duplicate', dup), fStoredClass,
       {%H-}dupfield]);
  end;
  if binary then
  begin
    // IDs are sorted by SaveToBinary design
    if fCount = 0 then
      fMaxID := 0
    else
      fMaxID := fValue[fCount - 1].IDValue;
    fUnSortedID := false;
  end
  else
    // JSON may have been tempered, so we actually ensure IDs are sorted
    fMaxID := FindMaxIDAndCheckSorted(pointer(fValue), fCount, fUnSortedID);
  InternalLog('LoadFrom% % count=% load=% index=%',
    [_CALLER[binary], fStoredClass, fCount, loaded.Stop, timer.Stop]);
end;

procedure TRestStorageInMemory.LoadFromJson(
  JsonBuffer: PUtf8Char; JsonBufferLen: PtrInt);
var
  T: TOrmTableJson;
  timer: TPrecisionTimer;
begin
  timer.Start;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'LoadFromJson' {$endif});
  try
    if fCount > 0 then
      DropValues({andupdatefile=}false);
    fModified := false;
    if JsonBuffer = nil then
      exit;
    T := TOrmTableJson.CreateFromTables(
      [fStoredClass], '', JsonBuffer, JsonBufferLen);
    try
      if T.FieldIndexID < 0 then // no ID field -> load is impossible
        exit;
      T.ToObjArray(fValue, fStoredClass);
    finally
      T.Free;
    end;
    ComputeStateAfterLoad(timer, {binary=}false);
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageInMemory.SaveToJson(Stream: TStream; Expand: boolean);
var
  i, j: PtrInt;
  W: TOrmWriter;
  ndx: TIntegerDynArray;
begin
  if self = nil then
    exit;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'SaveToJson' {$endif});
  try
    if fUnSortedID then
      fValues.CreateOrderedIndex(ndx, nil); // write in ascending ID order
    W := fStoredClassRecordProps.CreateJsonWriter(Stream, Expand, true,
      ALL_FIELDS, fCount, {bufsize=}1 shl 20);
    try
      if Expand then
        W.Add('[');
      for i := 0 to fCount - 1 do
      begin
        if Expand then
          W.AddCR; // for better readability
        if ndx = nil then
          j := i
        else
          j := ndx[i];
        fValue[j].GetJsonValues(W);
        W.AddComma;
      end;
      W.EndJsonObject(fCount, fCount);
    finally
      W.Free;
    end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.SaveToJson(Expand: boolean): RawUtf8;
var
  MS: TRawByteStringStream;
begin
  if self = nil then
    result := ''
  else
  begin
    MS := TRawByteStringStream.Create;
    try
      SaveToJson(MS, Expand);
      result := MS.DataString;
    finally
      MS.Free;
    end;
  end;
end;

function TRestStorageInMemory.SaveToBinary: RawByteString;
var
  MS: TRawByteStringStream;
begin
  if self = nil then
    result := ''
  else
  begin
    MS := TRawByteStringStream.Create;
    try
      SaveToBinary(MS);
      result := MS.DataString;
    finally
      MS.Free;
    end;
  end;
end;

const
  TRESTSTORAGEINMEMORY_MAGIC = $A5ABA5A5;

function TRestStorageInMemory.LoadFromBinary(Stream: TStream): boolean;
var
  R: TFastReader;
  MS: TMemoryStream;
  i, n, f: PtrInt;
  ID32: TIntegerDynArray;
  rec: TOrm;
  id: QWord;
  s: RawUtf8;
  prop: TOrmPropInfo;
  timer: TPrecisionTimer;
begin
  result := false;
  if self = nil then
    exit;
  timer.Start;
  MS := AlgoSynLZ.StreamUnCompress(Stream, TRESTSTORAGEINMEMORY_MAGIC);
  if MS = nil then
    exit;
  R.Init(MS.Memory, MS.Size);
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'LoadFromBinary' {$endif});
  try
    try
      // check header: expect same exact RTTI
      R.VarUtf8(s);
      if (s <> '') and // 0='' in recent mORMot format
         not IdemPropNameU(s, 'TSqlRecordProperties') then // old buggy format
        exit;
      if not fStoredClassRecordProps.CheckBinaryHeader(R) then
        exit;
      // create instances and read their IDs
      if fCount > 0 then
        DropValues({andupdatefile=}false);
      fModified := false;
      n := R.ReadVarUInt32Array(ID32);
      SetLength(fValue, abs(n)); // allocate all at once
      if n < 0 then
      begin
        // was wkFakeMarker -> TID were stored as VarUInt64 diffs
        n := abs(n);
        id := 0;
        for i := 0 to n - 1 do
        begin
          rec := fStoredClass.Create;
          inc(id, R.VarUInt64);
          rec.IDValue := id;
          fValue[i] := rec;
        end;
      end
      else
        // ReadVarUInt32Array() decoded TID into ID32[]
        for i := 0 to n - 1 do
        begin
          rec := fStoredClass.Create;
          rec.IDValue := ID32[i];
          fValue[i] := rec;
        end;
      // read content, grouped by field (for better compression)
      for f := 0 to fStoredClassRecordProps.Fields.Count - 1 do
      begin
        prop := fStoredClassRecordProps.Fields.List[f];
        for i := 0 to n - 1 do
          prop.SetBinary(fValue[i], R);
      end;
      ComputeStateAfterLoad(timer, {binary=}true);
      result := true;
    except
      DropValues(false); // on error, reset all values and return false
    end;
  finally
    StorageUnlock;
    MS.Free;
  end;
end;

function TRestStorageInMemory.LoadFromBinary(const Buffer: RawByteString): boolean;
var
  S: TStream;
begin
  S := TRawByteStringStream.Create(Buffer);
  try
    result := LoadFromBinary(S);
  finally
    S.Free;
  end;
end;

procedure TRestStorageInMemory.LoadFromResource(ResourceName: string;
  Instance: THandle);
var
  S: TStream;
begin
  if ResourceName = '' then
    ResourceName := fStoredClass.ClassName;
  if Instance = 0 then
    Instance := HInstance;
  S := TResourceStream.Create(Instance, ResourceName, pointer(10));
  try
    if not LoadFromBinary(S) then
      raise ERestStorage.CreateUtf8('%.LoadFromResource with invalid % content',
        [self, fStoredClass]);
  finally
    S.Free;
  end;
end;

function TRestStorageInMemory.SaveToBinary(Stream: TStream): integer;
var
  W: TBufferWriter;
  MS: TMemoryStream;
  i, j, f: PtrInt;
  hasInt64ID: boolean;
  p: PID;
  lastID, newID: TID;
  ndx, id32: TIntegerDynArray;
begin
  result := 0;
  if (self = nil) or
     (Stream = nil) then
    exit;
  MS := TMemoryStream.Create;
  W := TBufferWriter.Create(MS, 1 shl 20);
  try
    // primitive magic and fields signature for file type identification
    W.Write1(0); // ClassName='TSqlRecordProperties' in old buggy format
    fStoredClassRecordProps.SaveBinaryHeader(W);
    StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'SaveToBinary' {$endif});
    try
      // write IDs - in increasing order
      if fUnSortedID then
        fValues.CreateOrderedIndex(ndx, nil);
      SetLength(id32, fCount);
      hasInt64ID := false;
      for i := 0 to fCount - 1 do
      begin
        if ndx = nil then
          j := i
        else
          j := ndx[i];
        p := @fValue[j].IDValue;
        if p^ > high(cardinal) then
        begin
          hasInt64ID := true;
          break;
        end
        else
          id32[i] := PInteger(p)^;
      end;
      if hasInt64ID then
      begin
        // some IDs are 64-bit -> manual store difference as WriteVarUInt64
        W.WriteVarUInt32(fCount);
        W.Write1(ord(wkFakeMarker)); // fake marker
        lastID := 0;
        for i := 0 to fCount - 1 do
        begin
          // a bit less efficient than wkSorted (no optimization of +1 diff)
          if ndx = nil then
            j := i
          else
            j := ndx[i];
          newID := fValue[j].IDValue;
          if newID <= lastID then
            raise ERestStorage.CreateUtf8('%.SaveToBinary(%): duplicated ID',
              [self, fStoredClass]);
          W.WriteVarUInt64(newID - lastID);
          lastID := newID;
        end;
      end
      else
        // we can use the efficient wkSorted algorithm on id32[] values
        W.WriteVarUInt32Values(pointer(id32), fCount, wkSorted);
      // write content, grouped by field (for better compression)
      for f := 0 to fStoredClassRecordProps.Fields.Count - 1 do
        with fStoredClassRecordProps.Fields.List[f] do
          if ndx = nil then
            for i := 0 to fCount - 1 do
              GetBinary(fValue[i], W)
          else
            for i := 0 to fCount - 1 do
              GetBinary(fValue[ndx[i]], W);
    finally
      StorageUnLock;
    end;
    W.Flush;
    result := AlgoSynLZ.StreamCompress(MS, Stream, TRESTSTORAGEINMEMORY_MAGIC);
  finally
    W.Free;
    MS.Free;
  end;
end;

function TRestStorageInMemory.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  i: PtrInt;
begin
  // TableModelIndex is not useful here
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'EngineRetrieve' {$endif});
  try
    i := IDToIndex(ID);
    if i < 0 then
      result := ''
    else
      GetJsonValue(fValue[i], {withID=}true, [], result);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.GetOne(aID: TID): TOrm;
var
  i: PtrInt;
begin
  if aID = 0 then
    result := nil
  else
  begin
    StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'GetOne' {$endif});
    try
      i := IDToIndex(aID);
      if i < 0 then
        result := nil
      else
        result := fValue[i].CreateCopy;
    finally
      StorageUnLock;
    end;
  end;
end;

function TRestStorageInMemory.EngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUtf8;
  Increment: Int64): boolean;
var
  i, err: integer;
  P: TOrmPropInfo;
  V: RawUtf8;
  wasString: boolean;
  int: Int64;
  upd: TFieldBits;
begin
  result := false;
  if (ID < 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
    exit;
  P := fStoredClassProps.Prop[FieldName];
  if P = nil then
    exit;
  if FieldBitGet(fStoredClassRecordProps.IsUniqueFieldsBits, P.PropertyIndex) then
  begin
    InternalLog('EngineUpdateFieldIncrement(%) on UNIQUE %.%',
      [ID, fStoredClass, P.Name], sllDB);
    exit;
  end;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'EngineUpdateFieldIncrement' {$endif});
  try
    i := IDToIndex(ID);
    if i < 0 then
    begin
      InternalLog('EngineUpdateFieldIncrement(%): %.ID=% not found',
        [P.Name, fStoredClass, ID], sllDB);
      exit;
    end;
    P.GetValueVar(fValue[i], false, V, @wasString);
    int := GetInt64(pointer(V), err);
    if wasString or
       (err <> 0) then
    begin
      InternalLog('EngineUpdateFieldIncrement: %.%=[%] not an integer',
        [fStoredClass, P.Name, V], sllDB);
      exit;
    end;
    Int64ToUtf8(int + Increment, V);
    P.SetValueVar(fValue[i], V, false);
    if fTrackChangesFieldBitsOffset <> 0 then
    begin
      FillZero(upd);
      FieldBitSet(upd, P.PropertyIndex);
      InternalTrackChangeUpdated(fValue[i], upd);
    end;
    fModified := true;
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  P: TOrmPropInfo;
  WhereValueString, SetValueString, SetValueJson: RawUtf8;
  i: PtrInt;
  ndx: integer;
  SetValueWasString: boolean;
  match: TSynList;
  rec: TOrm;
  upd: TFieldBits;
begin
  result := false;
  if (TableModelIndex <> fStoredClassProps.TableIndex) or
     (SetFieldName = '') or
     (SetValue = '') or
     (WhereFieldName = '') or
     (WhereValue = '') then
    exit;
  // handle search field RTTI
  if not GetFieldIndex(WhereFieldName, ndx) then
    exit;
  if (ndx = 0) or
     (WhereValue[1] <> '"') then
    WhereValueString := WhereValue
  else
    UnQuoteSqlStringVar(pointer(WhereValue), WhereValueString);
  // handle destination field RTTI
  P := fStoredClassRecordProps.Fields.ByRawUtf8Name(SetFieldName);
  if P = nil then
    exit; // don't allow setting ID field, which is Read Only
  if FieldBitGet(fStoredClassRecordProps.IsUniqueFieldsBits, P.PropertyIndex) then
  begin
    InternalLog('EngineUpdateField on UNIQUE %.%', [fStoredClass, P.Name], sllDB);
    exit; { TODO : allow update UNIQUE field? }
  end;
  SetValueWasString := SetValue[1] = '"';
  if SetValueWasString then
    UnQuoteSqlStringVar(pointer(SetValue), SetValueString)
  else
    SetValueString := SetValue;
  // search indexes, then apply updates
  match := TSynList.Create;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'EngineUpdateField' {$endif});
  try
    // find matching match[]
    if FindWhereEqual(ndx, WhereValueString,
        DoAddToListEvent, match, 0, 0) = 0 then
    begin
      // match.Count=0 -> nothing to update
      result := true; // as with SQL UPDATE
      exit;
    end;
    // check that all records can be updated
    for i := 0 to match.Count - 1 do
      if not RecordCanBeUpdated(fStoredClass,
          TOrm(match.List[i]).IDValue, oeUpdate) then
        // one record update fails -> abort all
        exit;
    // update field value
    for i := 0 to match.Count - 1 do
    begin
      rec := match.List[i];
      P.SetValueVar(rec, SetValueString, SetValueWasString);
      if fOwner <> nil then
      begin
        if {%H-}SetValueJson = '' then
          JsonEncodeNameSQLValue(P.Name, SetValue, SetValueJson);
        fOwner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
          rec.IDValue, SetValueJson, nil, nil);
      end;
      if (fTrackChangesFieldBitsOffset <> 0) and
         (ndx <> 0) then
      begin
        FillZero(upd);
        FieldBitSet(upd, ndx - 1);
        InternalTrackChangeUpdated(rec, upd);
      end;
  end;
    fModified := true;
    result := true;
  finally
    StorageUnLock;
    match.Free;
  end;
end;

function TRestStorageInMemory.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUtf8): boolean;
var
  i: PtrInt;
  rec: TOrm;
  modified: TFieldBits;
begin
  result := false;
  if (ID < 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
    exit;
  if SentData = '' then
  begin
    result := True;
    exit;
  end;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'EngineUpdate' {$endif});
  try
    i := IDToIndex(ID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, ID, oeUpdate) then
      exit;
    if fUnique <> nil then
    begin
      // use temp rec to ensure no collision when updated Unique field
      rec := fValue[i].CreateCopy;       // copy since can be a partial update
      rec.FillFrom(SentData, @modified); // overwrite updated properties
      if not UniqueFieldsUpdateOK(rec, i, @modified) then
      begin
        rec.Free;
        exit;
      end;
      fValue[i].Free;   // avoid memory leak
      fValue[i] := rec; // replace item in list
    end
    else
      // direct in-place (partial) update
      fValue[i].FillFrom(SentData);
    fModified := true;
    result := true;
    if fOwner <> nil then
      fOwner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        ID, SentData, nil, nil);
    if fTrackChangesFieldBitsOffset <> 0 then
      InternalTrackChangeUpdated(fValue[i], modified);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateOne(Rec: TOrm; const Fields: TFieldBits;
  const SentData: RawUtf8): boolean;
var
  i, f: PtrInt;
  dest: TOrm;
  nfo: TOrmPropInfoList;
begin
  result := false;
  if (Rec = nil) or
     (POrmClass(Rec)^ <> fStoredClass) or
     (Rec.IDValue <= 0) then
    exit;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'UpdateOne' {$endif});
  try
    i := IDToIndex(Rec.IDValue);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, Rec.IDValue, oeUpdate) then
      exit;
    if (fUnique <> nil) and
       not UniqueFieldsUpdateOK(Rec, i, @Fields) then
      exit;
    dest := fValue[i];
    nfo := fStoredClassRecordProps.Fields;
    for f := 0 to nfo.Count - 1 do
      if FieldBitGet(Fields, f) then
        nfo.List[f].CopyValue(Rec, dest);
    fModified := true;
    result := true;
    if fOwner <> nil then
      fOwner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        Rec.IDValue, SentData, nil, nil);
    if fTrackChangesFieldBitsOffset <> 0 then
      InternalTrackChangeUpdated(dest, Fields);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateOne(ID: TID;
  const Values: TSqlVarDynArray): boolean;
var
  i: PtrInt;
  rec: TOrm;
begin
  result := false;
  if ID <= 0 then
    exit;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'UpdateOne' {$endif});
  try
    i := IDToIndex(ID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, ID, oeUpdate) then
      exit;
    if fUnique <> nil then
    begin
      // use temp rec to ensure no collision when updated Unique field
      rec := fValue[i].CreateCopy; // copy since can be a partial update
      if not rec.SetFieldSqlVars(Values) or
         not UniqueFieldsUpdateOK(rec, i, nil) then
      begin
        rec.Free;
        exit;
      end;
      fValue[i].Free; // avoid memory leak
      fValue[i] := rec;
    end
    else
      // direct in-place (partial) update
      if not fValue[i].SetFieldSqlVars(Values) then
        exit;
    fModified := true;
    result := true;
    if fOwner <> nil then
      fOwner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        ID, '', nil, fValue[i]);
    if fTrackChangesFieldBitsOffset <> 0 then
      InternalTrackChangeUpdated(
        fValue[i], fStoredClassRecordProps.CopiableFieldsBits);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  i: integer;
begin
  result := false;
  if (TableModelIndex <> fStoredClassProps.TableIndex) or
     not BlobField^.IsRawBlob then
    exit;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'EngineRetrieveBlob' {$endif});
  try
    i := IDToIndex(aID);
    if i < 0 then
      exit;
    // get result blob directly from RTTI property description
    BlobField.GetLongStrProp(fValue[i], RawByteString(BlobData));
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.RetrieveBlobFields(Value: TOrm): boolean;
var
  i, f: integer;
begin
  result := false;
  if (Value <> nil) and
     (Value.IDValue > 0) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.Orm do
      if BlobFields <> nil then
      begin
        StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'RetrieveBlobFields' {$endif});
        try
          i := IDToIndex(Value.IDValue);
          if i < 0 then
            exit;
          for f := 0 to high(BlobFields) do
            BlobFields[f].CopyValue(fValue[i], Value);
          result := true;
        finally
          StorageUnLock;
        end;
      end;
end;

function TRestStorageInMemory.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  i: PtrInt;
  upd: TFieldBits;
begin
  result := false;
  if (aID < 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) or
     not BlobField^.IsRawBlob then
    exit;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'EngineUpdateBlob' {$endif});
  try
    i := IDToIndex(aID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, aID, oeUpdate) then
      exit;
    // set blob value directly from RTTI property description
    BlobField.SetLongStrProp(fValue[i], BlobData);
    if (fOwner <> nil) or
       (fTrackChangesFieldBitsOffset <> 0)  then
      fStoredClassRecordProps.FieldBitsFromBlobField(BlobField, upd);
    if fOwner <> nil then
      fOwner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
        aID, '', @upd, nil);
    if fTrackChangesFieldBitsOffset <> 0 then
      InternalTrackChangeUpdated(fValue[i], upd);
    fModified := true;
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateBlobFields(Value: TOrm): boolean;
var
  i, f: PtrInt;
begin
  result := false;
  if (Value <> nil) and
     (Value.IDValue > 0) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.Orm do
    if BlobFields <> nil then
    begin
      StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'UpdateBlobFields' {$endif});
      try
        i := IDToIndex(Value.IDValue);
        if (i < 0) or
           not RecordCanBeUpdated(Table, Value.IDValue, oeUpdate) then
          exit;
        for f := 0 to high(BlobFields) do
          BlobFields[f].CopyValue(Value, fValue[i]);
        if fOwner <> nil then
          fOwner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
            Value.IDValue, '', @fStoredClassRecordProps.FieldBits[oftBlob], nil);
        if fTrackChangesFieldBitsOffset <> 0 then
          InternalTrackChangeUpdated(
            fValue[i], fStoredClassRecordProps.FieldBits[oftBlob]);
        fModified := true;
        result := true;
      finally
        StorageUnLock;
      end;
    end
    else
      result := true; // as TRestOrm.UpdateblobFields()
end;

function TRestStorageInMemory.TableRowCount(Table: TOrmClass): Int64;
begin
  if Table <> fStoredClass then
    result := 0
  else
    result := fCount;
end;

function TRestStorageInMemory.TableHasRows(Table: TOrmClass): boolean;
begin
  result := (Table = fStoredClass) and
            (fCount > 0);
end;

function TRestStorageInMemory.MemberExists(Table: TOrmClass;
  ID: TID): boolean;
begin
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'UpdateFile' {$endif});
  try
    result := (Table = fStoredClass) and
              (IDToIndex(ID) >= 0);
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageInMemory.UpdateFile;
var
  F: TFileStream;
  timer: TPrecisionTimer;
begin
  if (self = nil) or
     not fModified or
     (FileName = '') then
    exit;
  timer.Start;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'UpdateFile' {$endif});
  try
    DeleteFile(FileName); // always overwrite previous file
    if fCount > 0 then
    begin
      F := TFileStream.Create(FileName, fmCreate);
      try
        if BinaryFile then
          SaveToBinary(F)
        else
          SaveToJson(F, {expandedjson=} true);
      finally
        F.Free;
      end;
    end;
    fModified := false;
  finally
    StorageUnLock;
  end;
  InternalLog('UpdateFile % in %', [fStoredClass, timer.Stop], sllDB);
end;

procedure TRestStorageInMemory.SetFileName(const aFileName: TFileName);
begin
  if aFileName = fFileName then
    exit;
  fFileName := aFileName;
  fModified := true;
end;

procedure TRestStorageInMemory.SetBinaryFile(aBinary: boolean);
begin
  if aBinary = fBinaryFile then
    Exit;
  fBinaryFile := aBinary;
  fModified := true;
end;

procedure TRestStorageInMemory.ReloadFromFile;
var
  json: RawUtf8;
  stream: TStream;
begin
  if (fFileName <> '') and
     FileExists(fFileName) then
  begin
    if fBinaryFile then
    begin
      stream := FileStreamSequentialRead(fFileName);
      try
        LoadFromBinary(stream)
      finally
        stream.Free;
      end;
    end
    else
    begin
      json := AnyTextFileToRawUtf8(fFileName, true);
      LoadFromJson(pointer(json), length(json)); // buffer parsed in-place
    end;
  end;
end;

function TRestStorageInMemory.SearchField(const FieldName, FieldValue: RawUtf8;
  out ResultID: TIDDynArray): boolean;
var
  n, WhereField, i: integer;
  match: TSynList;
begin
  result := false;
  if (self = nil) or
     (fCount = 0) then
    exit;
  if IsRowID(pointer(FieldName)) then
    WhereField := 0
  else
  begin
    WhereField := fStoredClassRecordProps.Fields.IndexByName(FieldName);
    if WhereField < 0 then
      exit;
    inc(WhereField); // FindWhereEqual() expects index = RTTI+1
  end;
  match := TSynList.Create;
  try
    StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'SearchField' {$endif});
    try
      n := FindWhereEqual(WhereField, FieldValue, DoAddToListEvent, match, 0, 0);
      if n = 0 then
        exit;
      SetLength(ResultID, n);
      for i := 0 to n - 1 do
        ResultID[i] := TOrm(match.List[i]).IDValue;
      result := true;
    finally
      StorageUnLock;
    end;
  finally
    match.Free;
  end;
end;

function TRestStorageInMemory.SearchEvent(const FieldName, FieldValue: RawUtf8;
  const OnFind: TOnFindWhereEqual; Dest: pointer;
  FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean;
  Op: TSelectStatementOperator): integer;
var
  ndx: integer;
begin
  if GetFieldIndex(FieldName, ndx) then
    result := SearchEvent(ndx, FieldValue, OnFind, Dest,
      FoundLimit, FoundOffset, CaseInsensitive, Op)
  else
    result := 0;
end;

function TRestStorageInMemory.SearchEvent(FieldIndex: integer;
  const FieldValue: RawUtf8; const OnFind: TOnFindWhereEqual; Dest: pointer;
  FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean;
  Op: TSelectStatementOperator): integer;
begin
  result := 0;
  if (self = nil) or
     (fCount = 0) or
     (FieldIndex < 0) then
    exit;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'SearchEvent' {$endif});
  try
    result := FindWhere(FieldIndex, FieldValue, Op, OnFind, Dest,
      FoundLimit, FoundOffset, CaseInsensitive);
  finally
    StorageUnlock;
  end;
end;

function TRestStorageInMemory.SearchCopy(
  const FieldName, FieldValue: RawUtf8;
  CaseInsensitive: boolean; Op: TSelectStatementOperator): pointer;
begin
  if SearchEvent(FieldName, FieldValue, DoCopyEvent,
      @result, 1, 0, CaseInsensitive, Op) = 0 then
    result := nil;
end;

function TRestStorageInMemory.SearchInstance(
  const FieldName, FieldValue: RawUtf8;
  CaseInsensitive: boolean; Op: TSelectStatementOperator): pointer;
begin
  if SearchEvent(FieldName, FieldValue, DoInstanceEvent,
      @result, 1, 0, CaseInsensitive, Op) = 0 then
    result := nil;
end;

function TRestStorageInMemory.SearchInstance(
  FieldIndex: integer; const FieldValue: RawUtf8;
  CaseInsensitive: boolean; Op: TSelectStatementOperator): pointer;
begin
  if SearchEvent(FieldIndex, FieldValue, DoInstanceEvent,
      @result, 1, 0, CaseInsensitive, Op) = 0 then
    result := nil;
end;

function TRestStorageInMemory.SearchIndex(
  const FieldName, FieldValue: RawUtf8;
  CaseInsensitive: boolean; Op: TSelectStatementOperator): integer;
begin
  if SearchEvent(FieldName, FieldValue, DoIndexEvent,
      @result, 1, 0, CaseInsensitive, Op) = 0 then
    result := -1;
end;

function TRestStorageInMemory.SearchCount(
  const FieldName, FieldValue: RawUtf8;
  CaseInsensitive: boolean; Op: TSelectStatementOperator): integer;
begin
  result := SearchEvent(FieldName, FieldValue, DoNothingEvent,
      nil, 0, 0, CaseInsensitive, Op);
end;

class procedure TRestStorageInMemory.DoNothingEvent(
  aDest: pointer; aRec: TOrm; aIndex: integer);
begin
end;

class procedure TRestStorageInMemory.DoCopyEvent(
  aDest: pointer; aRec: TOrm; aIndex: integer);
begin
  if aDest <> nil then
    PPointer(aDest)^ := aRec.CreateCopy;
end;

class procedure TRestStorageInMemory.DoAddToListEvent(
  aDest: pointer; aRec: TOrm; aIndex: integer);
begin
  if aDest <> nil then
    TSynList(aDest).Add(aRec);
end;

class procedure TRestStorageInMemory.DoInstanceEvent(
  aDest: pointer; aRec: TOrm; aIndex: integer);
begin
  if aDest <> nil then
    PPointer(aDest)^ := aRec;
end;

class procedure TRestStorageInMemory.DoIndexEvent(
  aDest: pointer; aRec: TOrm; aIndex: integer);
begin
  if aDest <> nil then
    PInteger(aDest)^ := aIndex;
end;


{ TRestStorageInMemoryExternal }

constructor TRestStorageInMemoryExternal.Create(aClass: TOrmClass;
  aServer: TRestOrmServer; const aFileName: TFileName; aBinaryFile: boolean);
begin
  inherited Create(aClass, aServer, aFileName, aBinaryFile);
  fStorageLockShouldIncreaseOwnerInternalState := false; // done by overriden StorageLock()
end;

procedure TRestStorageInMemoryExternal.StorageLock(WillModifyContent: boolean
   {$ifdef DEBUGSTORAGELOCK}; const msg: shortstring {$endif});
begin
  inherited StorageLock(WillModifyContent {$ifdef DEBUGSTORAGELOCK}, msg {$endif});
  if WillModifyContent and
     (fOwner <> nil) then
    fOwner.FlushInternalDBCache;
end;




{ ************ TOrmVirtualTableJson/TOrmVirtualTableBinary Virtual Tables }


{ TOrmVirtualTableCursorJson }

function TOrmVirtualTableCursorJson.Column(aColumn: integer;
  var aResult: TSqlVar): boolean;
var
  store: TRestStorageInMemory;
begin
  if (self = nil) or
     (fCurrent > fMax) or
     (TOrmVirtualTableJson(Table).Static = nil) then
  begin
    result := false;
    exit;
  end;
  store := TOrmVirtualTableJson(Table).fStaticInMemory;
  if cardinal(fCurrent) >= cardinal(store.fCount) then
    result := false
  else
  begin
    if aColumn = VIRTUAL_TABLE_ROWID_COLUMN then
    begin
      aResult.VType := ftInt64;
      aResult.VInt64 := store.fValue[fCurrent].IDValue;
    end
    else
      with store.fStoredClassRecordProps.Fields do
        if cardinal(aColumn) >= cardinal(Count) then
          aResult.VType := ftNull
        else
          List[aColumn].GetFieldSqlVar(
            store.fValue[fCurrent], aResult, fColumnTemp);
    result := true;
  end;
end;

function TOrmVirtualTableCursorJson.Search(
  var Prepared: TOrmVirtualTablePrepared): boolean;
var
  store: TRestStorageInMemory;
begin
  result := false;
  inherited Search(Prepared); // mark EOF by default
  if not Table.InheritsFrom(TOrmVirtualTableJson) then
    exit;
  store := TOrmVirtualTableJson(Table).fStaticInMemory;
  if store = nil then
    exit;
  if store.fCount > 0 then
    // if something to search in
    if Prepared.IsWhereIDEquals(false) then
    begin
      // process WHERE ID=? statement
      fMax := store.IDToIndex(Prepared.Where[0].Value.VInt64); // O(1) hash
      if fMax >= 0 then
        fCurrent := fMax; // ID found
    end
    else
    begin
      // loop all records in ID order by default
      fMax := store.fCount - 1;
      if Prepared.IsWhereOneFieldEquals then
        with Prepared.Where[0] do
          if Column in store.fStoredClassRecordProps.IsUniqueFieldsBits then
          begin
            store.fStoredClassRecordProps.Fields.List[Column].SetFieldSqlVar(
              store.fSearchRec, Value);
            fMax := store.fUniquePerField[Column].Find(store.fSearchRec);
            if fMax >= 0 then
              fCurrent := fMax; // value found with O(1) search
          end;
    end;
  result := true; // no DB error
end;



{ TOrmVirtualTableJson }

constructor TOrmVirtualTableJson.Create(aModule: TOrmVirtualTableModule;
  const aTableName: RawUtf8; FieldCount: integer; Fields: PPUtf8CharArray);
begin
  inherited Create(aModule, aTableName, FieldCount, Fields);
  fStaticInMemory := fStatic as TRestStorageInMemory;
end;

function TOrmVirtualTableJson.Delete(aRowID: Int64): boolean;
begin
  result := (static <> nil) and static.Delete(StaticTable, aRowID);
  if result and
     (StaticStorage <> nil) and
     (StaticStorage.Owner <> nil) then
    StaticStorage.Owner.CacheOrNil.NotifyDeletion(StaticTable, aRowID);
end;

function TOrmVirtualTableJson.Drop: boolean;
begin
  if (self <> nil) and
     (static <> nil) then
  begin
    fStaticInMemory.RollBack(0); // close any pending transaction
    fStaticInMemory.DropValues({andupdatefile=}true);
    result := true;
  end
  else
    result := false;
end;

class procedure TOrmVirtualTableJson.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite, vtWhereIDPrepared];
  aProperties.CursorClass := TOrmVirtualTableCursorJson;
  aProperties.StaticClass := TRestStorageInMemoryExternal; // will flush Cache
  if InheritsFrom(TOrmVirtualTableBinary) then
    aProperties.FileExtension := 'data';
  // default will follow the class name, e.g. '.json' for TOrmVirtualTableJson
end;

function TOrmVirtualTableJson.Insert(aRowID: Int64;
  var Values: TSqlVarDynArray; out insertedRowID: Int64): boolean;
var
  rec: TOrm;
  json: RawUtf8;
begin
  result := false;
  if (self = nil) or
     (static = nil) then
    exit;
  rec := StaticTable.Create;
  try
    if rec.SetFieldSqlVars(Values) then
    begin
      if aRowID > 0 then
        rec.IDValue := aRowID;
      fStaticInMemory.GetJsonValue(rec, false, ooInsert, json);
      insertedRowID := fStaticInMemory.AddOne(rec, aRowID > 0, json);
      if insertedRowID > 0 then
      begin
        if fStaticInMemory.Owner <> nil then
          fStaticInMemory.Owner.CacheOrNil.Notify(rec, ooInsert);
        result := true;
      end;
    end;
  finally
    if not result then
      rec.Free; // on success, rec will stay in Values[]
  end;
end;

function TOrmVirtualTableJson.Prepare(
  var Prepared: TOrmVirtualTablePrepared): boolean;
begin
  result := inherited Prepare(Prepared); // optimize ID=? WHERE clause
  if result and
     (static <> nil) then
  begin
    if Prepared.IsWhereOneFieldEquals then
      with Prepared.Where[0] do
        if (Column >= 0) and
           (Column in fStaticInMemory.fStoredClassRecordProps.IsUniqueFieldsBits) then
        begin
          Value.VType := ftNull; // mark TOrmVirtualTableCursorJson expects it
          OmitCheck := true;
          Prepared.EstimatedCost := costSecondaryIndex;
          Prepared.EstimatedRows := 10;
        end
        else if Prepared.EstimatedCost in [costFullScan, costScanWhere] then
          Prepared.EstimatedRows := fStaticInMemory.Count;
  end;
end;

function TOrmVirtualTableJson.Update(oldRowID, newRowID: Int64;
  var Values: TSqlVarDynArray): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (static = nil) or
     (oldRowID <> newRowID) or
     (newRowID <= 0) then // don't allow ID change
    exit;
  if fStaticInMemory.UpdateOne(newRowID, Values) then
  begin
    if fStaticInMemory.Owner <> nil then
    begin
      i := fStaticInMemory.IDToIndex(newRowID);
      if i >= 0 then
        fStaticInMemory.Owner.CacheOrNil.Notify(
          fStaticInMemory.fValue[i], ooUpdate);
    end;
    result := true;
  end;
end;



{ ************ TOrmVirtualTableLog Virtual Table }

{ TOrmVirtualTableLog }

type
  /// Record associated to Virtual Table implemented in Delphi, for Read/Only
  // access to a .log file, as created by TSynLog
  // - not used as real instances, but only used by the TOrmVirtualTableLog module
  // to provide the field layout needed to create the column layout for the
  // CREATE TABLE statement
  TOrmLogFile = class(TOrmVirtualTableAutoID)
  protected
    fContent: RawUtf8;
    fDateTime: TDateTime;
    fLevel: TSynLogInfo;
  published
    /// the log event time stamp
    property DateTime: TDateTime read fDateTime;
    /// the log event level
    property Level: TSynLogInfo read fLevel;
    /// the textual message associated to the log event
    property Content: RawUtf8 read fContent;
  end;

constructor TOrmVirtualTableLog.Create(aModule: TOrmVirtualTableModule;
  const aTableName: RawUtf8; FieldCount: integer; Fields: PPUtf8CharArray);
var
  aFileName: TFileName;
begin
  inherited Create(aModule, aTableName, FieldCount, Fields);
  if FieldCount = 1 then
    Utf8ToFileName(Fields[0], aFileName)
  else
    aFileName := aModule.FileName(aTableName);
  fLogFile := TSynLogFile.Create(aFileName);
end;

destructor TOrmVirtualTableLog.Destroy;
begin
  fLogFile.Free;
  inherited;
end;

class procedure TOrmVirtualTableLog.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWhereIDPrepared];
  aProperties.CursorClass := TOrmVirtualTableCursorLog;
  aProperties.RecordClass := TOrmLogFile;
end;


{ TOrmVirtualTableCursorLog }

function TOrmVirtualTableCursorLog.Column(aColumn: integer;
  var aResult: TSqlVar): boolean;
var
  LogFile: TSynLogFile;
begin
  result := false;
  if (self = nil) or
     (fCurrent > fMax) then
    exit;
  LogFile := TOrmVirtualTableLog(Table).fLogFile;
  if LogFile = nil then
    exit;
  case aColumn of
    -1:
      SetColumn(aResult, fCurrent + 1); // ID = row index + 1
    0:
      SetColumnDate(aResult, LogFile.EventDateTime(fCurrent), true);
    1:
      SetColumn(aResult, ord(LogFile.EventLevel[fCurrent]));
    2:
      SetColumn(aResult, LogFile.LinePointers[fCurrent],
        LogFile.LineSize(fCurrent));
  else
    exit;
  end;
  result := true;
end;

function TOrmVirtualTableCursorLog.Search(
  var Prepared: TOrmVirtualTablePrepared): boolean;
begin
  result := inherited Search(Prepared); // mark EOF by default
  if result then
  begin
    fMax := TOrmVirtualTableLog(Table).fLogFile.Count - 1; // search all range
    if Prepared.IsWhereIDEquals(false) then
    begin
      // handle obvious ID=? case to return index := ID-1
      fCurrent := Prepared.Where[0].Value.VInt64 - 1;
      if cardinal(fCurrent) <= cardinal(fMax) then
        // found one
        fMax := fCurrent
      else
        // out of range ID
        fMax := fCurrent - 1;
    end;
  end;
end;



{ ************ TRestStorageRemote for CRUD Redirection }


{ TRestStorageRemote }

constructor TRestStorageRemote.Create(aClass: TOrmClass;
  aServer: TRestOrmServer; aRemoteRest: TRestOrm);
begin
  if aRemoteRest = nil then
    raise ERestStorage.CreateUtf8('%.Create(nil)', [self]);
  inherited Create(aClass, aServer);
  fRemoteTableIndex := aRemoteRest.Model.GetTableIndexExisting(aClass);
  fRemoteRest := aRemoteRest;
end;

function TRestStorageRemote.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
begin
  result := fRemoteRest.EngineAdd(fRemoteTableIndex, SentData);
end;

function TRestStorageRemote.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  result := fRemoteRest.EngineDelete(fRemoteTableIndex, ID);
end;

function TRestStorageRemote.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
begin
  result := fRemoteRest.EngineDeleteWhere(fRemoteTableIndex, SqlWhere, IDs);
end;

function TRestStorageRemote.EngineExecute(const aSql: RawUtf8): boolean;
begin
  result := fRemoteRest.EngineExecute(aSql);
end;

function TRestStorageRemote.EngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
begin
  result := fRemoteRest.EngineList(SQL, ForceAjax, ReturnedRowCount);
end;

function TRestStorageRemote.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
begin
  result := fRemoteRest.EngineRetrieve(fRemoteTableIndex, ID);
end;

function TRestStorageRemote.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
begin
  if (self = nil) or
     (BlobField = nil) then
    result := false
  else
    result := fRemoteRest.EngineRetrieveBlob(fRemoteTableIndex,
      aID, BlobField, BlobData);
end;

function TRestStorageRemote.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUtf8): boolean;
begin
  result := fRemoteRest.EngineUpdate(fRemoteTableIndex, ID, SentData);
end;

function TRestStorageRemote.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
begin
  if (self = nil) or
     (BlobField = nil) then
    result := false
  else
    result := fRemoteRest.EngineUpdateBlob(fRemoteTableIndex,
      aID, BlobField, BlobData);
end;

function TRestStorageRemote.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
begin
  result := fRemoteRest.EngineUpdateField(fRemoteTableIndex,
    SetFieldName, SetValue, WhereFieldName, WhereValue);
end;

function TRestStorageRemote.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUtf8; Increment: Int64): boolean;
begin
  result := fRemoteRest.EngineUpdateFieldIncrement(fRemoteTableIndex,
    ID, FieldName, Increment);
end;



{ ************ TRestStorageShard as Abstract Sharded Storage Engine }


{ TRestStorageShard }

const
  MIN_SHARD = 1000;

constructor TRestStorageShard.Create(aClass: TOrmClass;
  aServer: TRestOrmServer; aShardRange: TID; aOptions: TRestStorageShardOptions; aMaxShardCount: integer);
var
  i, n: PtrInt;
begin
  if aShardRange < MIN_SHARD then
    raise ERestStorage.CreateUtf8('Unexpected %.Create(%,aShardRange=%<%)',
      [self, aClass, aShardRange, MIN_SHARD]);
  inherited Create(aClass, aServer);
  fShardRange := aShardRange;
  fShardLast := -1;
  fOptions := aOptions;
  if aMaxShardCount < 2 then
    fMaxShardCount := 2
  else
    fMaxShardCount := aMaxShardCount;
  InitShards; // set fShards[], fShardLast, fShardOffset and fShardLastID
  n := length(fShards);
  fShardNextID := (n + fShardOffset) * fShardRange + 1;
  SetLength(fShardTableIndex, n);
  for i := 0 to fShardLast do
    if fShards[i] = nil then
      fShardTableIndex[i] := -1
    else
      fShardTableIndex[i] := fShards[i].Model.GetTableIndexExisting(aClass);
  InternalLog('Create(%,range=%,maxcount=%) [%..%]', [fStoredClass,
    fShardRange, fMaxShardCount, fShardOffset, fShardOffset + n - 1], sllDB);
end;

destructor TRestStorageShard.Destroy;
var
  i, j: PtrInt;
  rest: TRestOrm;
begin
  try
    if not (ssoNoConsolidateAtDestroy in fOptions) then
      ConsolidateShards;
  finally
    inherited Destroy;
    for i := 0 to high(fShards) do
    begin
      rest := fShards[i];
      if rest = nil then
        continue;
      if rest.RefCount <> 1 then
        raise ERestStorage.CreateUtf8('%.Destroy: %.RefCount=%',
          [self, rest.RefCount]);
      IInterface(rest)._Release; // manual reference counting
      for j := i + 1 to high(fShards) do
        if fShards[j] = rest then
          fShards[j] := nil; // same instance re-used in fShards[]
    end;
  end;
end;

procedure TRestStorageShard.ConsolidateShards;
begin
  // do nothing by default
end;

procedure TRestStorageShard.RemoveShard(aShardIndex: integer);
var
  rest: TRestOrm;
begin
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'RemoveShard' {$endif});
  try
    if (fShards <> nil) and
       (cardinal(aShardIndex) <= cardinal(fShardLast)) then
    begin
      rest := fShards[aShardIndex];
      if (rest = nil) or
         (rest.RefCount <> 1) then
        raise ERestStorage.CreateUtf8('%.RemoveShard(%): %?',
          [self, aShardIndex, fShards[aShardIndex]]);
      if rest <> nil then
      begin
        IInterface(rest)._Release; // manual reference counting
        fShards[aShardIndex] := nil;
      end;
      fShardTableIndex[aShardIndex] := -1;
    end;
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageShard.InternalAddNewShard;
var
  rest: TRestOrm;
  i: PtrInt;
  {%H-}log: ISynLog;
begin
  log := fOwner.Owner.Enter('InternalAddNewShard: #% for %',
    [fShardLast + 1, fStoredClass], self);
  rest := InitNewShard;
  if rest = nil then
    raise ERestStorage.CreateUtf8('%.InitNewShard(%) =nil',
      [self, fStoredClass]);
  inc(fShardNextID, fShardRange);
  SetLength(fShardTableIndex, fShardLast + 1);
  fShardTableIndex[fShardLast] := rest.Model.GetTableIndexExisting(fStoredClass);
  if fShardLast >= fMaxShardCount then
    for i := 0 to fShardLast do
      if fShards[i] <> nil then
      begin
        RemoveShard(i);
        break;
      end;
end;

function TRestStorageShard.ShardFromID(aID: TID;
  out aShardTableIndex: integer; out aShard: TRestOrm;
  aOccasion: TOrmOccasion; aShardIndex: PInteger): boolean;
var
  ndx: integer;
begin
  result := false;
  if (self = nil ) or
     (fShards = nil) or
     (aID <= 0) then
    exit;
  case aOccasion of
    ooUpdate:
      if ssoNoUpdate in fOptions then
        exit;
    ooDelete:
      if ssoNoDelete in fOptions then
        exit;
  end;
  ndx := ((aID - 1) div fShardRange) - fShardOffset;
  if (ndx >= 0) and
     (ndx <= fShardLast) and
     (fShards[ndx] <> nil) then
  begin
    case aOccasion of
      ooUpdate:
        if (ssoNoUpdateButLastShard in fOptions) and
           (ndx <> fShardLast) then
          exit;
      ooDelete:
        if (ssoNoDeleteButLastShard in fOptions) and
           (ndx <> fShardLast) then
          exit;
    end;
    aShard := fShards[ndx];
    aShardTableIndex := fShardTableIndex[ndx];
    if aShardIndex <> nil then
      aShardIndex^ := ndx;
    result := true;
  end;
end;

function TRestStorageShard.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  data: RawUtf8;
  i: PtrInt;
begin
  if JsonGetID(pointer(SentData), result) then
    raise ERestStorage.CreateUtf8('%.EngineAdd(%) unexpected ID in %',
      [self, fStoredClass, SentData]);
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardAdd' {$endif});
  try
    inc(fShardLastID);
    if fShardLastID >= fShardNextID then
    begin
      InternalAddNewShard;
      if fShardLastID >= fShardNextID then
        raise ERestStorage.CreateUtf8('%.EngineAdd(%) fShardNextID',
          [self, fStoredClass]);
    end;
    result := fShardLastID;
    i := PosExChar('{', SentData);
    if i = 0 then
      FormatUtf8('{ID:%}', [result], data)
    else
    begin
      data := SentData;
      insert(FormatUtf8('ID:%,', [result]), data, i + 1);
    end;
    if fShardBatch <> nil then
      InternalShardBatch(fShardLast).RawAdd(data)
    else
    begin
      if fShards[fShardLast].EngineAdd(
        fShardTableIndex[fShardLast], data) <> result then
      begin
        InternalLog('EngineAdd error %.ID=%', [fStoredClass, result], sllDB);
        result := 0;
      end;
    end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
var
  tableIndex, shardIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardDelete' {$endif});
  try
    if not ShardFromID(ID, tableIndex, rest, ooDelete, @shardIndex) then
      result := false
    else if fShardBatch <> nil then
      result := InternalShardBatch(shardIndex).Delete(ID) >= 0
    else
      result := rest.EngineDelete(tableIndex, ID);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  i: PtrInt;
  ndx: cardinal;
  id: array of TInt64DynArray; // IDs split per shard
  idn: TIntegerDynArray;
  sql: RawUtf8;
begin
  result := false;
  if (IDs = nil) or
     (ssoNoDelete in fOptions) then
    exit;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardDeleteWhere' {$endif});
  try
    SetLength(id, fShardLast + 1);
    SetLength(idn, fShardLast + 1);
    for i := 0 to high(IDs) do
    begin
      ndx := ((IDs[i] - 1) div fShardRange) - fShardOffset; // inlined ShardFromID()
      if (ndx >= cardinal(fShardLast)) or
         (fShards[ndx] = nil) then
        continue;
      if (ssoNoDeleteButLastShard in fOptions) and
         (ndx <> cardinal(fShardLast)) then
        continue;
      AddInt64(id[ndx], idn[ndx], IDs[i]);
    end;
    result := true;
    for i := 0 to high(id) do
      if id[i] <> nil then
      begin
        sql := Int64DynArrayToCsv(pointer(id[i]), idn[i], 'ID in (', ')');
        if not fShards[i].EngineDeleteWhere(
            fShardTableIndex[i], sql, TIDDynArray(id[i])) then
          result := false;
      end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineExecute(const aSql: RawUtf8): boolean;
begin
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ShardExecute' {$endif});
  try
    if (fShardLast >= 0) and
       not (ssoNoExecute in fOptions) then
      result := fShards[fShardLast].EngineExecute(aSql)
    else
      result := false;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.TableHasRows(Table: TOrmClass): boolean;
begin
  result := fShards <> nil;
end;

function TRestStorageShard.TableRowCount(Table: TOrmClass): Int64;
var
  i: PtrInt;
begin
  result := 0;
  InternalLog('TableRowCount(%) may take a while', [fStoredClass], sllWarning);
  for i := 0 to high(fShards) do
    // no StorageLock protection to avoid blocking
    if fShards[i] <> nil then
      inc(result, fShards[i].TableRowCount(fStoredClass));
end;

function TRestStorageShard.EngineList(const SQL: RawUtf8;
  ForceAjax: boolean; ReturnedRowCount: PPtrInt): RawUtf8;
var
  ResCount: PtrInt;
begin
  result := ''; // indicates error occurred
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ShardList' {$endif});
  try
    ResCount := 0;
    if IdemPropNameU(fBasicSqlCount, SQL) then
    begin
      FormatUtf8('[{"Count(*)":%}]'#$A, [TableRowCount(fStoredClass)], result);
      ResCount := 1;
    end
    else if IdemPropNameU(fBasicSqlHasRows[false], SQL) or
            IdemPropNameU(fBasicSqlHasRows[true], SQL) then
      if fShards <> nil then
      begin
        // return one row with fake ID=1
        result := '[{"RowID":1}]'#$A;
        ResCount := 1;
      end
      else
        result := '{"fieldCount":1,"values":["RowID"]}'#$A
    else
    begin
      if (fShardLast >= 0) and
         not (ssoNoList in fOptions) then
        result := fShards[fShardLast].EngineList(SQL, ForceAjax, @ResCount);
    end;
    if ReturnedRowCount <> nil then
      ReturnedRowCount^ := ResCount;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  tableIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ShardRetrieve' {$endif});
  try
    if not ShardFromID(ID, tableIndex, rest) then
      result := ''
    else
      result := rest.EngineRetrieve(tableIndex, ID);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  tableIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ShardRetrieveBlob' {$endif});
  try
    if not ShardFromID(aID, tableIndex, rest) then
      result := false
    else
      result := rest.EngineRetrieveBlob(tableIndex, aID, BlobField, BlobData);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUtf8): boolean;
var
  tableIndex, shardIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardUpdate' {$endif});
  try
    if not ShardFromID(ID, tableIndex, rest, ooUpdate, @shardIndex) then
      result := false
    else if fShardBatch <> nil then
    begin
      InternalShardBatch(shardIndex).RawUpdate(SentData, ID);
      result := true;
    end
    else
      result := rest.EngineUpdate(tableIndex, ID, SentData);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  tableIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  result := false;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardUpdateBlob' {$endif});
  try
    if ShardFromID(aID, tableIndex, rest, ooUpdate) then
      result := rest.EngineUpdateBlob(tableIndex, aID, BlobField, BlobData);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
begin
  result := false;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardUpdateField' {$endif});
  try
    if not ((ssoNoUpdate in fOptions) or
       (ssoNoUpdateField in fOptions)) then
      result := fShards[fShardLast].EngineUpdateField(fShardTableIndex[fShardLast],
        SetFieldName, SetValue, WhereFieldName, WhereValue);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUtf8; Increment: Int64): boolean;
var
  tableIndex: integer; // should be integer - not PtrInt
  rest: TRestOrm;
begin
  result := false;
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardUpdateFieldIncrement' {$endif});
  try
    if ShardFromID(ID, tableIndex, rest, ooUpdate) then
      result := rest.EngineUpdateFieldIncrement(tableIndex,
        ID, FieldName, Increment);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.InternalBatchStart(Encoding: TRestBatchEncoding;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false;
  if ssoNoBatch in fOptions then
    exit;
  // lock is protected by try..finally in TRestOrmServer.RunBatch caller method
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ShardBatchStart' {$endif});
  try
    if fShardBatch <> nil then
      raise ERestStorage.CreateUtf8('Missing %.InternalBatchStop call', [self]);
    if fShardLast < 0 then
      // new DB -> force fShardBatch<>nil
      SetLength(fShardBatch, 1)
    else
      SetLength(fShardBatch, fShardLast + 1);
    result := true;
  finally
    if not result then
      // release lock on error
      StorageUnLock;
  end;
end;

function TRestStorageShard.InternalShardBatch(ShardIndex: integer): TRestBatch;
begin
  if cardinal(ShardIndex) > cardinal(fShardLast) then
    raise ERestStorage.CreateUtf8('Unexpected %.InternalShardBatch(%)',
      [self, ShardIndex]);
  if fShardBatch = nil then
    raise ERestStorage.CreateUtf8('Missing %.InternalBatchStart call', [self]);
  if ShardIndex >= length(fShardBatch) then
    // InitNewShard just called
    SetLength(fShardBatch, ShardIndex + 1);
  if fShardBatch[ShardIndex] = nil then
    if fShards[ShardIndex] <> nil then
      fShardBatch[ShardIndex] := TRestBatch.Create(fShards[ShardIndex],
        fStoredClass, 10000, [boExtendedJson])
    else
      raise ERestStorage.CreateUtf8('%.InternalShardBatch fShards[%]=nil',
        [self, ShardIndex]);
  result := fShardBatch[ShardIndex];
end;

procedure TRestStorageShard.InternalBatchStop;
var
  i: PtrInt;
begin
  try
    for i := 0 to high(fShardBatch) do
      if fShardBatch[i] <> nil then
        if fShards[i].BatchSend(fShardBatch[i]) <> HTTP_SUCCESS then
          InternalLog('InternalBatchStop(%): %.BatchSend failed for shard #%',
            [fStoredClass, fShards[i].ClassType, i], sllWarning);
  finally
    ObjArrayClear(fShardBatch);
    StorageUnLock;
  end;
end;


{ *********** TRestStorageMulti as Abstract Multi-User Storage Engine }

{ TRestStorageMulti }

constructor TRestStorageMulti.Create(aLog: TSynLogFamily; aDatabaseIDBits: byte;
  const aModelClasses: array of TOrmClass; aSettings: TRestStorageMultiSettings);
var
  n: PtrInt;
begin
  if aDatabaseIDBits - 1 > 62 then
    raise ERestStorageMulti.CreateUtf8(
      '%.Create: invalid aDatabaseIDBits=% (1..63)', [self, aDatabaseIDBits]);
  n := length(aModelClasses);
  if n = 0 then
    raise ERestStorageMulti.CreateUtf8('%.Create with no Model class', [self]);
  SetLength(fModelClasses, n);
  MoveFast(aModelClasses[0], fModelClasses[0], n * SizeOf(aModelClasses[0]));
  fLog := aLog;
  fSettings := aSettings;
  fDatabaseIDBits := aDatabaseIDBits;
  fDatabaseIDMax := pred(1 shl aDatabaseIDBits);
  fCache := TSynDictionary.Create(TypeInfo(TIDDynArray),
    TypeInfo(TInterfaceDynArray), {keyinsensitive=}false, aSettings.TimeOutSec);
end;

destructor TRestStorageMulti.Destroy;
begin
  SetShutdown(true);
  inherited Destroy;
end;

function TRestStorageMulti.GetShutdown: boolean;
begin
  result := (self <> nil) and
            Assigned(fCache);
end;

procedure TRestStorageMulti.SetShutdown(Value: boolean);
begin
  if Value = GetShutdown then
    exit;
  fSafe.WriteLock;
  try
    FreeAndNil(fCache);
  finally
    fSafe.WriteUnLock;
  end;
end;

function TRestStorageMulti.GetCached: RawJson;
begin
  result := fCache.SaveValuesToJson;
end;

function TRestStorageMulti.IDText(aID: TRestStorageMultiDatabaseID): TShort16;
begin
  BinToHexDisplayLowerShort16(aID, fDatabaseIDBits, result);
end;

function TRestStorageMulti.IsDatabaseIDCorrect(aID: TRestStorageMultiDatabaseID): boolean;
begin
  result := (self <> nil) and
            (aID > 0) and
            (aID <= fDatabaseIDMax);
end;

procedure TRestStorageMulti.EnsureDatabaseIDCorrect(
  aID: TRestStorageMultiDatabaseID; const aCaller: shortstring);
begin
  if not IsDatabaseIDCorrect(aID) then
    raise ERestStorageMulti.CreateUtf8('Invalid %.%(%)',
      [self, aCaller, Int64ToHexShort(aID)]);
end;

function TRestStorageMulti.GetDB(aID: TRestStorageMultiDatabaseID): IRestOrmServer;
begin
  EnsureDatabaseIDCorrect(aID, 'GetDB');
  if not fCache.FindAndCopy(aID, result) then
  begin
    result := NewDB(aID);
    fLog.SynLog.Log(sllInfo, 'Initialized DB[%]=%',
      [IDText(aID), ObjectFromInterface(result)], self);
  end;
end;

function TRestStorageMulti.NewModel: TOrmModel;
begin
  // no URI root in this unpublished Model
  result := TOrmModel.Create(fModelClasses, '');
end;

procedure TRestStorageMulti.OnServerInfo(Ctxt: TRestUriContext;
  var Info: TDocVariantData);
begin
  Info.AddNameValuesToObject([
    'multi', self
    ]);
end;


{ TRestStorageMultiOnDisk }

constructor TRestStorageMultiOnDisk.Create(aLog: TSynLogFamily;
  aDatabaseIDBits: byte; const aModelClasses: array of TOrmClass;
  const aDataFolder, aFilePrefix: TFileName;
  aSettings: TRestStorageMultiSettings);
begin
  inherited Create(aLog, aDatabaseIDBits, aModelClasses, aSettings);
  if aDatafolder <> '' then
    fDataFolder := EnsureDirectoryExists(aDatafolder);
  fFilePrefix := aFilePrefix;
end;

function TRestStorageMultiOnDisk.GetDiskAvailableMB: integer;
var
  name: TFileName;
  avail, free, total: QWord;
begin
  if fDataFolder = '' then
  begin
    result := 0;
    exit;
  end;
  name := fDataFolder;
  GetDiskInfo(name, avail, free, total);
  result := free shr 20; // returns free space in MB
end;

function TRestStorageMultiOnDisk.GetDBPassword(
  aID: TRestStorageMultiDatabaseID): SpiUtf8;
begin
  // no encryption by default
  result := '';
end;

function TRestStorageMultiOnDisk.GetDBFileName(
  aID: TRestStorageMultiDatabaseID): TFileName;
begin
  EnsureDatabaseIDCorrect(aID, 'GetDBFileName');
  result := FormatString('%%%.db', [fDataFolder, fFilePrefix, IDText(aID)]);
end;


function ToText(t: TOrmVirtualTableTransaction): PShortString;
begin
  result := GetEnumName(TypeInfo(TOrmVirtualTableTransaction), ord(t));
end;


initialization
  // set late-binding return of TOrmVirtualTableClass.ModuleName
  GetVirtualTableModuleName := @_GetVirtualTableModuleName;

end.

