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
    - TORMVirtualTableJSON/TORMVirtualTableBinary Virtual Tables
    - TRestStorageRemote for CRUD Redirection
    - TRestStorageShard as Abstract Sharded Storage Engine

  *****************************************************************************
}


interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  {$ifndef FPC}
//  typinfo, // for proper Delphi inlining
  {$endif FPC}
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
  mormot.orm.server,
  mormot.soa.core,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client;

// most types are defined as a single "type" statement due to classes coupling

type
  // some forward class definitions
  TRestStorage = class;
  TORMVirtualTable = class;
  TRestStorageInMemory = class;


{ ************ Virtual Table ORM Support }

  /// Record associated to a Virtual Table implemented in Delphi, with ID
  // forced at INSERT
  // - will use TORMVirtualTableModule / TORMVirtualTable / TORMVirtualTableCursor
  // classes for a generic Virtual table mechanism on the Server side
  // - call Model.VirtualTableRegister() before TRestORMServer.Create on the
  // Server side (not needed for Client) to associate such a record with a
  // particular Virtual Table module, otherwise an exception will be raised:
  // ! Model.VirtualTableRegister(TORMDali1,TORMVirtualTableJSON);
  TORMVirtualTableForcedID = class(TORMVirtual);

  /// Record associated to Virtual Table implemented in Delphi, with ID
  // generated automatically at INSERT
  // - will use TORMVirtualTableModule / TORMVirtualTable / TORMVirtualTableCursor
  // classes for a generic Virtual table mechanism
  // - call Model.VirtualTableRegister() before TRestORMServer.Create on the
  // Server side (not needed for Client) to associate such a record with a
  // particular Virtual Table module, otherwise an exception will be raised:
  // ! Model.VirtualTableRegister(TORMDali1,TORMVirtualTableJSON);
  TORMVirtualTableAutoID = class(TORMVirtual);

  /// class-reference type (metaclass) of our abstract table storage
  // - may be e.g. TRestStorageInMemory, TRestStorageInMemoryExternal,
  // TRestStorageExternal or TRestStorageMongoDB
  TRestStorageClass = class of TRestStorage;

  /// class-reference type (metaclass) of a virtual table implementation
  TORMVirtualTableClass = class of TORMVirtualTable;

  /// a WHERE constraint as set by the TORMVirtualTable.Prepare() method
  TORMVirtualTablePreparedConstraint = packed record
    /// Column on left-hand side of constraint
    // - The first column of the virtual table is column 0
    // - The RowID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index
    // - if this field contains VIRTUAL_TABLE_IGNORE_COLUMN (-2), TORMVirtualTable.
    // Prepare() should ignore this entry
    Column: integer;
    /// The associated expression
    // - TORMVirtualTable.Prepare() must set Value.VType to not svtUnknown
    // (e.g. to svtNull), if an expression is expected at vt_BestIndex() call
    // - TORMVirtualTableCursor.Search() will receive an expression value,
    // to be retrieved e.g. via sqlite3_value_*() functions
    Value: TSQLVar;
    /// Constraint operator
    // - MATCH keyword is parsed into soBeginWith, and should be handled as
    // soBeginWith, soContains or soSoundsLike* according to the effective
    // expression text value ('text*', '%text'...)
    Operation: TSQLCompareOperator;
    /// If true, the constraint is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default (OmitCheck=false), the SQLite core double checks all
    // constraints on each row of the virtual table that it receives
    // - TORMVirtualTable.Prepare() can set this property to true
    OmitCheck: boolean;
  end;

  PSQLVirtualTablePreparedConstraint = ^TORMVirtualTablePreparedConstraint;

  /// an ORDER BY clause as set by the TORMVirtualTable.Prepare() method
  // - warning: this structure should match exactly TSQLite3IndexOrderBy as
  // defined in SynSQLite3
  TORMVirtualTablePreparedOrderBy = record
    /// Column number
    // - The first column of the virtual table is column 0
    // - The RowID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    Column: integer;
    /// True for DESCending order, false for ASCending order.
    Desc: boolean;
  end;

  /// abstract planning execution of a query, as set by TORMVirtualTable.Prepare
  TORMVirtualTablePreparedCost = (
    costFullScan, costScanWhere, costSecondaryIndex, costPrimaryIndex);

  /// the WHERE and ORDER BY statements as set by TORMVirtualTable.Prepare
  // - Where[] and OrderBy[] are fixed sized arrays, for fast and easy code
  //{$ifdef USERECORDWITHMETHODS}TORMVirtualTablePrepared = record{$else}
    TORMVirtualTablePrepared = object
  //{$endif USERECORDWITHMETHODS}
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
    // the TORMVirtualTable.Prepare() method with several expressions
    EstimatedCost: TORMVirtualTablePreparedCost;
    ///  Estimated number of rows of using this prepared index
    // - does make sense only if EstimatedCost=costFullScan
    // - SQLite uses this value to make a choice between several calls to
    // the TORMVirtualTable.Prepare() method with several expressions
    // - is used only starting with SQLite 3.8.2
    EstimatedRows: Int64;
    /// WHERE statement parameters, in TORMVirtualTableCursor.Search() order
    Where: array[0 .. MAX_SQLFIELDS - 1] of TORMVirtualTablePreparedConstraint;
    /// ORDER BY statement parameters
    OrderBy: array[0 .. MAX_SQLFIELDS - 1] of TORMVirtualTablePreparedOrderBy;
    /// returns TRUE if there is only one ID=? statement in this search
    function IsWhereIDEquals(CalledFromPrepare: boolean): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if there is only one FieldName=? statement in this search
    function IsWhereOneFieldEquals: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  PSQLVirtualTablePrepared = ^TORMVirtualTablePrepared;

  TORMVirtualTableCursor = class;

  /// class-reference type (metaclass) of a cursor on an abstract Virtual Table
  TORMVirtualTableCursorClass = class of TORMVirtualTableCursor;

  /// the possible features of a Virtual Table
  // - vtWrite is to be set if the table is not Read/Only
  // - vtTransaction if handles vttBegin, vttSync, vttCommit, vttRollBack
  // - vtSavePoint if handles vttSavePoint, vttRelease, vttRollBackTo
  // - vtWhereIDPrepared if the ID=? WHERE statement will be handled in
  // TORMVirtualTableCursor.Search()
  TORMVirtualTableFeature = (
    vtWrite, vtTransaction, vtSavePoint, vtWhereIDPrepared);

  /// a set of features of a Virtual Table
  TORMVirtualTableFeatures = set of TORMVirtualTableFeature;

  /// used to store and handle the main specifications of a TORMVirtualTableModule
  TVirtualTableModuleProperties = record
    /// a set of features of a Virtual Table
    Features: TORMVirtualTableFeatures;
    /// the associated cursor class
    CursorClass: TORMVirtualTableCursorClass;
    /// the associated TORM class
    // - used to retrieve the field structure with all collations
    RecordClass: TORMClass;
    /// the associated TRestStorage class used for storage
    // - is e.g. TRestStorageInMemory for TORMVirtualTableJSON,
    // TRestStorageExternal for TORMVirtualTableExternal, or nil for
    // TORMVirtualTableLog
    StaticClass: TRestStorageClass;
    /// can be used to customize the extension of the filename
    // - the '.' is not to be included
    FileExtension: TFileName;
  end;

  /// parent class able to define a Virtual Table module
  // - in order to implement a new Virtual Table type, you'll have to define a so
  // called "Module" to handle the fields and data access and an associated
  // TORMVirtualTableCursorClass for handling the SELECT statements
  // - for our framework, the SQLite3 unit will inherit from this class to define
  // a TORMVirtualTableModuleSQLite3 class, which will register the associated
  // virtual table definition into a SQLite3 connection, on the server side
  // - children should override abstract methods in order to implement the
  // association with the database engine itself
  TORMVirtualTableModule = class
  protected
    fModuleName: RawUTF8;
    fTableClass: TORMVirtualTableClass;
    fServer: TRestORMServer;
    fFeatures: TVirtualTableModuleProperties;
    fFilePath: TFileName;
  public
    /// create the Virtual Table instance according to the supplied class
    // - inherited constructors may register the Virtual Table to the specified
    // database connection
    constructor Create(aTableClass: TORMVirtualTableClass;
      aServer: TRestORMServer); virtual;
    /// retrieve the file name to be used for a specific Virtual Table
    // - returns by default a file located in the executable folder, with the
    // table name as file name, and module name as extension
    function FileName(const aTableName: RawUTF8): TFileName; virtual;
    /// the Virtual Table module features
    property Features: TORMVirtualTableFeatures read fFeatures.Features;
    /// the associated virtual table class
    property TableClass: TORMVirtualTableClass read fTableClass;
    /// the associated virtual table cursor class
    property CursorClass: TORMVirtualTableCursorClass read fFeatures.CursorClass;
    /// the associated TRestStorage class used for storage
    // - e.g. returns TRestStorageInMemory for TORMVirtualTableJSON,
    // or TRestStorageExternal for TORMVirtualTableExternal, or
    // either nil for TORMVirtualTableLog
    property StaticClass: TRestStorageClass read fFeatures.StaticClass;
    /// the associated TORM class
    // - is mostly nil, e.g. for TORMVirtualTableJSON
    // - used to retrieve the field structure for TORMVirtualTableLog e.g.
    property RecordClass: TORMClass read fFeatures.RecordClass;
    /// the extension of the filename (without any left '.')
    property FileExtension: TFileName read fFeatures.FileExtension;
    /// the full path to be used for the filename
    // - is '' by default, i.e. will use the executable path
    // - you can specify here a custom path, which will be used by the FileName
    // method to retrieve the .json/.data full file
    property FilePath: TFileName read fFilePath write fFilePath;
    /// the associated Server instance
    // - may be nil, in case of direct access to the virtual table
    property Server: TRestORMServer read fServer;
    /// the corresponding module name
    property ModuleName: RawUTF8 read fModuleName;
  end;

  /// the available transaction levels
  TORMVirtualTableTransaction = (
    vttBegin, vttSync, vttCommit, vttRollBack,
    vttSavePoint, vttRelease, vttRollBackTo);

  /// abstract class able to access a Virtual Table content
  // - override the Prepare/Structure abstract virtual methods for reading
  // access to the virtual table content
  // - you can optionaly override Drop/Delete/Insert/Update/Rename/Transaction
  // virtual methods to allow content writing to the virtual table
  // - the same virtual table mechanism can be used with several database module,
  // with diverse database engines
  TORMVirtualTable = class
  protected
    fModule: TORMVirtualTableModule;
    fTableName: RawUTF8;
    fStatic: TRestORM;
    fStaticStorage: TRestStorage;
    fStaticTable: TORMClass;
    fStaticTableIndex: integer;
  public
    /// create the virtual table access instance
    // - the created instance will be released when the virtual table will be
    // disconnected from the DB connection (e.g. xDisconnect method for SQLite3)
    // - shall raise an exception in case of invalid parameters (e.g. if the
    // supplied module is not associated to a TRestORMServer instance)
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TORMVirtualTableAutoID class and
    // create any associated Static: TRestStorage instance
    constructor Create(aModule: TORMVirtualTableModule; const aTableName: RawUTF8;
      FieldCount: integer; Fields: PPUTF8CharArray); virtual;
    /// release the associated memory, especially the Static instance
    destructor Destroy; override;
    /// retrieve the corresponding module name
    // - will use the class name, triming any T/TSQL/TSQLVirtual/TORMVirtualTable*
    // - when the class is instanciated, it will be faster to retrieve the same
    // value via Module.ModuleName
    class function ModuleName: RawUTF8;
    /// a generic method to get a 'CREATE TABLE' structure from a supplied
    // TORM class
    // - is called e.g. by the Structure method
    class function StructureFromClass(aClass: TORMClass;
      const aTableName: RawUTF8): RawUTF8;
    /// the associated Virtual Table module
    property Module: TORMVirtualTableModule read fModule;
    /// the name of the Virtual Table, as specified following the TABLE keyword
    // in the CREATE VIRTUAL TABLE statement
    property TableName: RawUTF8 read fTableName;
  public { virtual methods to be overridden }
    /// should return the main specifications of the associated TORMVirtualTableModule
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); virtual; abstract;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TORMVirtualTableCursor.Search()
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck to true if double check is not necessary
    // - OmitOrderBy must be set to true if double sort is not necessary
    // - EstimatedCost and EstimatedRows should receive the estimated cost
    // - default implementation will let the DB engine perform the search,
    // and prepare for ID=? statement if vtWhereIDPrepared was set
    function Prepare(var Prepared: TORMVirtualTablePrepared): boolean; virtual;
    /// should retrieve the format (the names and datatypes of the columns) of
    // the virtual table, as expected by sqlite3_declare_vtab()
    // - default implementation is to retrieve the structure for the associated
    // Module.RecordClass property (as set by GetTableModuleProperties) or
    // the Static.StoredClass: in both cases, column numbering will follow
    // the TORM published field order (TORM.RecordProps.Fields[])
    function Structure: RawUTF8; virtual;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Drop: boolean; virtual;
    /// called to delete a virtual table row
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Delete(aRowID: Int64): boolean; virtual;
    /// called to insert a virtual table row content from an array of TSQLVar
    // - should return true on success, false otherwise
    // - should return the just created row ID in insertedRowID on success
    // - does nothing by default, and returns false, i.e. always fails
    function Insert(aRowID: Int64; var Values: TSQLVarDynArray;
      out insertedRowID: Int64): boolean; virtual;
    /// called to update a virtual table row content from an array of TSQLVar
    // - should return true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Update(oldRowID, newRowID: Int64;
      var Values: TSQLVarDynArray): boolean; virtual;
    /// called to begin a transaction to the virtual table row
    // - do nothing by default, and returns false in case of RollBack/RollBackto
    // - aSavePoint is used for vttSavePoint, vttRelease and vttRollBackTo only
    // - note that if you don't nest your writing within a transaction, SQLite
    // will call vttCommit for each INSERT/UPDATE/DELETE, just like a regular
    // SQLite database - it could make bad written code slow even with Virtual
    // Tables
    function Transaction(aState: TORMVirtualTableTransaction;
      aSavePoint: integer): boolean; virtual;
    /// called to rename the virtual table
    // - by default, returns false, i.e. always fails
    function Rename(const NewName: RawUTF8): boolean; virtual;
    /// the associated virtual table storage instance
    // - can be e.g. a TRestStorageInMemory for TORMVirtualTableJSON,
    // or a TRestStorageExternal for TORMVirtualTableExternal, or nil
    // for TORMVirtualTableLog
    property Static: TRestORM read fStatic;
    /// the associated virtual table storage instance, if is a TRestStorage
    property StaticStorage: TRestStorage read fStaticStorage;
    /// the associated virtual table storage table
    property StaticTable: TORMClass read fStaticTable;
    /// the associated virtual table storage index in its Model.Tables[] array
    property StaticTableIndex: integer read fStaticTableIndex;
  end;

  /// abstract class able to define a Virtual Table cursor
  // - override the Search/HasData/Column/Next abstract virtual methods to
  // implement the search process
  TORMVirtualTableCursor = class
  protected
    fTable: TORMVirtualTable;
    /// used internaly between two Column() method calls for GetFieldSQLVar()
    fColumnTemp: RawByteString;
    /// easy set a TSQLVar content for the Column() method
    procedure SetColumn(var aResult: TSQLVar; aValue: Int64); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSQLVar; const aValue: double); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSQLVar; const aValue: RawUTF8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumn(var aResult: TSQLVar; aValue: PUTF8Char; aValueLength: integer); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnBlob(var aResult: TSQLVar; aValue: pointer; aValueLength: integer);
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnDate(var aResult: TSQLVar; const aValue: TDateTime;
      aWithMS: boolean); {$ifdef HASINLINE}inline;{$endif}
    procedure SetColumnCurr64(var aResult: TSQLVar; aValue64: PInt64);
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the cursor instance
    // - it will be destroyed when by the DB engine (e.g. via xClose in SQLite3)
    constructor Create(aTable: TORMVirtualTable); virtual;
    /// the associated Virtual Table class instance
    property Table: TORMVirtualTable read fTable;
  public { abstract methods to be overridden }
    /// called to begin a search in the virtual table
    // - the TORMVirtualTablePrepared parameters were set by
    // TORMVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved e.g. by x_BestIndex() from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - should move cursor to first row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Search(const Prepared: TORMVirtualTablePrepared): boolean; virtual; abstract;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; virtual; abstract;
    /// called to retrieve a column value of the current data row into a TSQLVar
    // - if aColumn=-1, should return the row ID as varInt64 into aResult
    // - should return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; virtual; abstract;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; virtual; abstract;
  end;

  /// A generic Virtual Table cursor associated to Current/Max index properties
  TORMVirtualTableCursorIndex = class(TORMVirtualTableCursor)
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
    function Search(const Prepared: TORMVirtualTablePrepared): boolean; override;
  end;




{ ************ TRestStorage Abstract Class for ORM/REST Storage }

  /// exception raised during ORM/REST Storage process
  ERestStorage = class(EORMException);

  /// REST class with direct access to an external database engine
  // - you can set an alternate per-table database engine by using this class
  // - this abstract class is to be overridden with a proper implementation
  // (e.g. TRestStorageInMemory in this unit, or TRestStorageExternal
  // from mORMotDB unit, or TRestStorageMongoDB from mORMotMongoDB unit)
  TRestStorage = class(TRestORM)
  protected
    fStoredClass: TORMClass;
    fStoredClassProps: TORMModelRecordProperties;
    fStoredClassRecordProps: TORMProperties;
    fStoredClassMapping: PORMPropertiesMapping;
    fStorageLockShouldIncreaseOwnerInternalState: boolean;
    fStorageLockLogTrace: boolean;
    fModified: boolean;
    fOwner: TRestORMServer;
    fStorageCriticalSection: TRTLCriticalSection;
    fStorageCriticalSectionCount: integer;
    fBasicSQLCount: RawUTF8;
    fBasicSQLHasRows: array[boolean] of RawUTF8;
    fStorageVirtual: TORMVirtualTable;
    /// any set bit in this field indicates UNIQUE field value
    fIsUnique: TFieldBits;
    fOutInternalStateForcedRefresh: boolean;
    procedure RecordVersionFieldHandle(Occasion: TORMOccasion;
      var Decoder: TJSONObjectDecoder);
    function GetStoredClassName: RawUTF8;
  public
    /// initialize the abstract storage data
    constructor Create(aClass: TORMClass; aServer: TRestORMServer); reintroduce; virtual;
    /// finalize the storage instance
    destructor Destroy; override;

    /// should be called before any access to the storage content
    // - and protected with a try ... finally StorageUnLock; end section
    procedure StorageLock(WillModifyContent: boolean; const msg: RawUTF8); virtual;
    /// should be called after any StorageLock-protected access to the content
    // - e.g. protected with a try ... finally StorageUnLock; end section
    procedure StorageUnLock; virtual;
    /// low-level access to how StorageLock(true) affetcs TRestServer.InternalState
    property StorageLockShouldIncreaseOwnerInternalState: boolean
      read fStorageLockShouldIncreaseOwnerInternalState
      write fStorageLockShouldIncreaseOwnerInternalState;

    /// implement Rest unlocking (UNLOCK verb)
    // - to be called e.g. after a Retrieve() with forupdate=TRUE
    // - locking is handled at (Owner.)Model level
    // - returns true on success
    function UnLock(Table: TORMClass; aID: TID): boolean; override;
    /// overridden method calling the owner (if any) to guess if this record
    // can be updated or deleted
    function RecordCanBeUpdated(Table: TORMClass; ID: TID; Action: TORMEvent;
      ErrorMsg: PRawUTF8 = nil): boolean; override;
    /// override this method if you want to update the refresh state
    // - returns FALSE if the static table content was not modified (default
    // method implementation is to always return FALSE)
    // - returns TRUE if the table has been refreshed and its content was modified:
    // therefore the client will know he'll need to refresh some content
    function RefreshedAndModified: boolean; virtual;
    /// TRestORMServer.URI use it for Static.EngineList to by-pass virtual table
    // - this default implementation will return TRUE and replace SQL with
    // SQLSelectAll[true] if it SQL equals SQLSelectAll[false] (i.e. 'SELECT *')
    // - this method is called only if the WHERE clause of SQL refers to the
    // static table name only (not needed to check it twice)
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; virtual;
    /// create one index for all specific FieldNames at once
    // - do nothing virtual/abstract method by default: will return FALSE (i.e. error)
    function CreateSQLMultiIndex(Table: TORMClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; virtual;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    // - this default implementation will call the overloaded SearchField()
    // value after conversion of the FieldValue into RawUTF8
    function SearchField(const FieldName: RawUTF8; FieldValue: Int64;
      out ResultID: TIDDynArray): boolean; overload; virtual;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUTF8;
      out ResultID: TIDDynArray): boolean; overload; virtual; abstract;
    /// returns the current authentication session ID from TRestORMServer owner
    function GetCurrentSessionUserID: TID; override;

    /// read only access to a boolean value set to true if table data was modified
    property Modified: boolean
      read fModified write fModified;
    /// read only access to the ORM properties of the associated record type
    // - may be nil if this instance is not associated with a TORMModel
    property StoredClassProps: TORMModelRecordProperties
      read fStoredClassProps;
    /// read only access to the RTTI properties of the associated record type
    property StoredClassRecordProps: TORMProperties
      read fStoredClassRecordProps;
    /// read only access to the TRestORMServer using this storage engine
    property Owner: TRestORMServer
      read fOwner;
    /// enable low-level trace of StorageLock/StorageUnlock methods
    // - may be used to resolve low-level race conditions
    property StorageLockLogTrace: boolean
      read fStorageLockLogTrace write fStorageLockLogTrace;
    /// read only access to the class defining the record type stored in this
    // REST storage
    property StoredClass: TORMClass
      read fStoredClass;
    /// allow to force refresh for a given Static table
    // - default FALSE means to return the main TRestORMServer.InternalState
    // - TRUE indicates that OutInternalState := cardinal(-1) will be returned
    property OutInternalStateForcedRefresh: boolean
      read fOutInternalStateForcedRefresh;
  published
    /// name of the class defining the record type stored in this REST storage
    property StoredClassName: RawUTF8
      read GetStoredClassName;
  end;



{ ************ TRestStorageInMemory as Stand-Alone JSON/Binary Storage }

  /// event prototype called by TRestStorageInMemory.FindWhereEqual() or
  // TRestStorageInMemory.ForEach() methods
  // - aDest is an opaque pointer, as supplied to FindWhereEqual(), which may
  // point e.g. to a result list, or a shared variable to apply the process
  // - aRec will point to the corresponding item
  // - aIndex will identify the item index in the internal list
  TFindWhereEqualEvent = procedure(
    aDest: pointer; aRec: TORM; aIndex: integer) of object;

  /// abstract REST storage exposing some internal TORM-based methods
  TRestStorageRecordBased = class(TRestStorage)
  public
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    /// manual Add of a TORM
    // - returns the ID created on success
    // - returns -1 on failure (not UNIQUE field value e.g.)
    // - on success, the Rec instance is added to the Values[] list: caller
    // doesn't need to Free it
    function AddOne(Rec: TORM; ForceID: boolean;
      const SentData: RawUTF8): TID; virtual; abstract;
    /// manual Retrieval of a TORM field values
    // - an instance of the associated static class is created
    // - and all its properties are filled from the Items[] values
    // - caller can modify these properties, then use UpdateOne() if the changes
    // have to be stored inside the Items[] list
    // - calller must always free the returned instance
    // - returns NIL if any error occured, e.g. if the supplied aID was incorrect
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function GetOne(aID: TID): TORM; virtual; abstract;
    /// manual Update of a TORM field values
    // - Rec.ID specifies which record is to be updated
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function UpdateOne(Rec: TORM;
      const SentData: RawUTF8): boolean; overload; virtual; abstract;
    /// manual Update of a TORM field values from an array of TSQLVar
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    // - this default implementation will create a temporary TORM instance
    // with the supplied Values[], and will call overloaded UpdateOne() method
    function UpdateOne(ID: TID;
      const Values: TSQLVarDynArray): boolean; overload; virtual;
  end;

  /// class able to handle a O(1) hashed-based search of a property
  // - used e.g. to hash TRestStorageInMemory field values
  TRestStorageInMemoryUnique = class
  protected
    fHasher: TDynArrayHasher;
    fOwner: TRestStorageInMemory;
    fPropInfo: TORMPropInfo;
    fCaseInsensitive: boolean;
    fLastFindHashCode: cardinal;
    function EventCompare(const A,B): integer; // match TEventDynArraySortCompare
    function EventHash(const Elem): cardinal;  // match TEventDynArrayHashOne
  public
    /// initialize a hash for a record array field
    // - aField maps the "stored AS_UNIQUE" published property
    constructor Create(aOwner: TRestStorageInMemory; aField: TORMPropInfo);
    /// fast search using O(1) internal hash table
    // - returns -1 if not found or not indexed (self=nil)
    function Find(Rec: TORM): integer;
    /// called by TRestStorageInMemory.AddOne after a precious Find()
    function AddedAfterFind(Rec: TORM): boolean;
    /// the corresponding field RTTI
    property PropInfo: TORMPropInfo read fPropInfo;
    /// if the string comparison shall be case-insensitive
    property CaseInsensitive: boolean read fCaseInsensitive;
    /// access to the internal hash table
    property Hasher: TDynArrayHasher read fHasher;
  end;

  /// REST storage with direct access to a TObjectList memory-stored table
  // - store the associated TORM values in memory
  // - handle one TORM per TRestStorageInMemory instance
  // - must be registered individualy in a TRestORMServer to access data from a
  // common client, by using the TRestORMServer.StaticDataCreate method:
  // it allows an unique access for both SQLite3 and Static databases
  // - handle basic REST commands, no SQL interpreter is implemented: only
  // valid SQL command is "SELECT Field1,Field2 FROM Table WHERE ID=120;", i.e
  // a one Table SELECT with one optional "WHERE fieldname = value" statement;
  // if used within a TORMVirtualTableJSON, you'll be able to handle any kind of
  // SQL statement (even joined SELECT or such) with this memory-stored database
  // - data can be stored and retrieved from a file (JSON format is used by
  // default, if BinaryFile parameter is left to false; a proprietary compressed
  // binary format can be used instead) if a file name is supplied at creating
  // the TRestStorageInMemory instance
  // - our TRestStorage database engine is very optimized and is a lot
  // faster than SQLite3 for such queries - but its values remain in RAM,
  // therefore it is not meant to deal with more than 100,000 rows or if
  // ACID commit on disk is required
  TRestStorageInMemory = class(TRestStorageRecordBased)
  protected
    fValue: TORMObjArray;
    fCount: integer;
    fFileName: TFileName;
    fCommitShouldNotUpdateFile: boolean;
    fBinaryFile: boolean;
    fExpandedJSON: boolean;
    fUnSortedID: boolean;
    fSearchRec: TORM; // temporary record to store the searched value
    fBasicUpperSQLSelect: array[boolean] of RawUTF8;
    fUnique: array of TRestStorageInMemoryUnique;
    fMaxID: TID;
    fValues: TDynArrayHashed; // hashed by ID
    function UniqueFieldsUpdateOK(aRec: TORM; aUpdateIndex: integer): boolean;
    function GetItem(Index: integer): TORM; {$ifdef HASINLINE}inline;{$endif}
    function GetID(Index: integer): TID;
    procedure SetFileName(const aFileName: TFileName);
    procedure ComputeStateAfterLoad(var loaded: TPrecisionTimer; binary: boolean);
    procedure SetBinaryFile(aBinary: boolean);
    procedure GetJSONValuesEvent(aDest: pointer; aRec: TORM; aIndex: integer);
    /// used to create the JSON content from a SELECT parsed command
    // - WhereField index follows FindWhereEqual / TSynTableStatement.WhereField
    // - returns the number of data row added (excluding field names)
    // - this method is very fast and optimized (for search and JSON serializing)
    function GetJSONValues(Stream: TStream; Expand: boolean;
      Stmt: TSynTableStatement): PtrInt;
  public
    /// TRestORMServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle basic queries as handled by EngineList()
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    /// overridden methods for direct in-memory database engine thread-safe process
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: TRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: TRawBlob): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
  public
    /// initialize the table storage data, reading it from a file if necessary
    // - data encoding on file is UTF-8 JSON format by default, or
    // should be some binary format if aBinaryFile is set to true
    constructor Create(aClass: TORMClass; aServer: TRestORMServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean = false); reintroduce; virtual;
    /// free used memory
    // - especially release all fValue[] instances
    destructor Destroy; override;
    /// clear all the values of this table
    // - will reset the associated database file, if any
    procedure DropValues(andUpdateFile: boolean = true);
    /// load the values from JSON data
    // - a temporary copy of aJSON is made to ensure it won't be modified in-place
    // - consider using the overlaoded PUTF8Char/len method if you don't need this copy
    procedure LoadFromJSON(const aJSON: RawUTF8); overload;
    /// load the values from JSON data
    procedure LoadFromJSON(JSONBuffer: PUTF8Char; JSONBufferLen: integer); overload;
    /// save the values into JSON data
    function SaveToJSON(Expand: boolean): RawUTF8; overload;
    /// save the values into JSON data
    procedure SaveToJSON(Stream: TStream; Expand: boolean); overload;
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
    // - the resource name is expected to be the TORM class name,
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
    /// low-level Add of a TORM instance
    // - returns the ID created on success
    // - returns -1 on failure (not UNIQUE field value e.g.)
    // - on success, the Rec instance is added to the Values[] list: caller
    // doesn't need to Free it, since it will be owned by the storage
    // - in practice, SentData is used only for OnUpdateEvent/OnBlobUpdateEvent
    // and the history feature
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function AddOne(Rec: TORM; ForceID: boolean;
      const SentData: RawUTF8): TID; override;
    /// manual Retrieval of a TORM field values
    // - an instance of the associated static class is created, and filled with
    // the actual properties values
    // - and all its properties are filled from the Items[] values
    // - caller can modify these properties, then use UpdateOne() if the changes
    // have to be stored inside the Items[] list
    // - calller must always free the returned instance
    // - returns NIL if any error occured, e.g. if the supplied aID was incorrect
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function GetOne(aID: TID): TORM; override;
    /// manual Update of a TORM field values
    // - Rec.ID specifies which record is to be updated
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function UpdateOne(Rec: TORM;
      const SentData: RawUTF8): boolean; override;
    /// manual Update of a TORM field values from a TSQLVar array
    // - will update all properties, including BLOB fields and such
    // - returns TRUE on success, FALSE on any error (e.g. invalid Rec.ID)
    // - method available since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function UpdateOne(ID: TID;
      const Values: TSQLVarDynArray): boolean; override;
    /// direct deletion of a TORM, from its index in Values[]
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function DeleteOne(aIndex: integer): boolean; virtual;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// overridden method for direct in-memory database engine call
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestORMServer
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    /// overridden method for direct in-memory database engine call
    function UpdateBlobFields(Value: TORM): boolean; override;
    /// overridden method for direct in-memory database engine call
    function RetrieveBlobFields(Value: TORM): boolean; override;
    /// overridden method for direct in-memory database engine call
    function TableRowCount(Table: TORMClass): Int64; override;
    /// overridden method for direct in-memory database engine call
    function TableHasRows(Table: TORMClass): boolean; override;
    /// overridden method for direct in-memory database engine call
    function MemberExists(Table: TORMClass; ID: TID): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUTF8;
      out ResultID: TIDDynArray): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return the found TORM on success, nil if none did match
    // - warning: it returns a reference to one item of the unlocked internal
    // list, so you should NOT use this on a read/write table, but rather
    // use the slightly slower but safer SearchCopy() method or make explicit
    // ! StorageLock ... try ... SearchInstance ... finally StorageUnlock end
    function SearchInstance(const FieldName, FieldValue: RawUTF8): pointer;
    /// search for a field value, according to its SQL content representation
    // - return the found TORM index on success, -1 if none did match
    // - warning: it returns a reference to the current index of the unlocked
    // internal list, so you should NOT use without StorageLock/StorageUnlock
    function SearchIndex(const FieldName, FieldValue: RawUTF8): integer;
    /// search for a field value, according to its SQL content representation
    // - return a copy of the found TORM on success, nil if no match
    // - you should use SearchCopy() instead of SearchInstance(), unless you
    // are sure that the internal TORM list won't change
    function SearchCopy(const FieldName, FieldValue: RawUTF8): pointer;
    /// search and count for a field value, according to its SQL content representation
    // - return the number of found entries on success, 0 if it was not found
    function SearchCount(const FieldName, FieldValue: RawUTF8): integer;
    /// search for a field value, according to its SQL content representation
    // - call the supplied OnFind event on match
    // - returns the number of found entries
    // - is just a wrapper around FindWhereEqual() with StorageLock protection
    function SearchEvent(const FieldName, FieldValue: RawUTF8;
      OnFind: TFindWhereEqualEvent; Dest: pointer;
      FoundLimit, FoundOffset: PtrInt): integer;
    /// optimized search of WhereValue in WhereField (0=RowID,1..=RTTI)
    // - will use fast O(1) hash for fUnique[] fields
    // - will use SYSTEMNOCASE case-insensitive search for text values, unless
    // CaseInsensitive is set to FALSE
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhereEqual(WhereField: integer; const WhereValue: RawUTF8;
      OnFind: TFindWhereEqualEvent; Dest: pointer;
      FoundLimit, FoundOffset: PtrInt;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// optimized search of WhereValue in a field, specified by name
    // - will use fast O(1) hash for fUnique[] fields
    // - will use SYSTEMNOCASE case-insensitive search for text values, unless
    // CaseInsensitive is set to FALSE
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function FindWhereEqual(const WhereFieldName, WhereValue: RawUTF8;
      OnFind: TFindWhereEqualEvent; Dest: pointer;
      FoundLimit, FoundOffset: integer;
      CaseInsensitive: boolean = true): PtrInt; overload;
    /// search the maximum value of a given column
    // - will only handle integer/Int64 kind of column
    function FindMax(WhereField: integer; out max: Int64): boolean;
    /// execute a method on every TORM item
    // - the loop execution will be protected via StorageLock/StorageUnlock
    procedure ForEach(WillModifyContent: boolean;
      OnEachProcess: TFindWhereEqualEvent; Dest: pointer);
    /// low-level TFindWhereEqualEvent callback doing nothing
    class procedure DoNothingEvent(aDest: pointer;
      aRec: TORM; aIndex: integer);
    /// low-level TFindWhereEqualEvent callback making PPointer(aDest)^ := aRec
    class procedure DoInstanceEvent(aDest: pointer;
      aRec: TORM; aIndex: integer);
    /// low-level TFindWhereEqualEvent callback making PInteger(aDest)^ := aIndex
    class procedure DoIndexEvent(aDest: pointer;
      aRec: TORM; aIndex: integer);
    /// low-level TFindWhereEqualEvent callback making PPointer(aDest)^ := aRec.CreateCopy
    class procedure DoCopyEvent(aDest: pointer;
      aRec: TORM; aIndex: integer);
    /// low-level TFindWhereEqualEvent callback calling TSynList(aDest).Add(aRec)
    class procedure DoAddToListEvent(aDest: pointer;
      aRec: TORM; aIndex: integer);
    /// read-only access to the TORM values, storing the data
    // - this returns directly the item class instance stored in memory: if you
    // change the content, it will affect the internal data - so for instance
    // DO NOT change the ID values, unless you may have unexpected behavior
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property Items[Index: integer]: TORM
      read GetItem; default;
    /// direct access to the memory of the internal dynamic array storage
    // - Items[] is preferred, since it will check the index, but is slightly
    // slower, e.g. in a loop or after a IDToIndex() call
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property Value: TORMObjArray
      read fValue;
    /// read-only access to the ID of a TORM values
    // - warning: this method should be protected via StorageLock/StorageUnlock
    property ID[Index: integer]: TID
      read GetID;
  published
    /// read only access to the file name specified by constructor
    // - you can call the TRestORMServer.StaticDataCreate method to
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
    property ExpandedJSON: boolean
      read fExpandedJSON write fExpandedJSON;
    /// set this property to TRUE if you want the COMMIT statement not to
    // update the associated TORMVirtualTableJSON
    property CommitShouldNotUpdateFile: boolean
      read fCommitShouldNotUpdateFile write fCommitShouldNotUpdateFile;
    /// read-only access to the number of TORM values
    property Count: integer read fCount;
  end;

  /// a dynamic array of TRestStorageInMemory instances
  // - used e.g. by TRestORMServerFullMemory
  TRestStorageInMemoryDynArray = array of TRestStorageInMemory;

  /// class-reference type (metaclass) of our TObjectList memory-stored table storage
  // - may be TRestStorageInMemory or TRestStorageInMemoryExternal
  TRestStorageInMemoryClass = class of TRestStorageInMemory;

  /// REST storage with direct access to a memory database, to be used as
  // an external SQLite3 Virtual table
  // - this is the kind of in-memory table expected by TORMVirtualTableJSON,
  // in order to be consistent with the internal DB cache
  TRestStorageInMemoryExternal = class(TRestStorageInMemory)
  public
    /// initialize the table storage data, reading it from a file if necessary
    // - data encoding on file is UTF-8 JSON format by default, or
    // should be some binary format if aBinaryFile is set to true
    constructor Create(aClass: TORMClass; aServer: TRestORMServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean = false); override;
    /// this overridden method will notify the Owner when the internal DB content
    // is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but TORMVirtualTableJSON virtual
    // tables could flush the database content without proper notification
    // - this overridden implementation will call Owner.FlushInternalDBCache
    procedure StorageLock(WillModifyContent: boolean;
      const msg: RawUTF8); override;
  end;



{ ************ TORMVirtualTableJSON/TORMVirtualTableBinary Virtual Tables }

  /// A Virtual Table cursor for reading a TRestStorageInMemory content
  // - this is the cursor class associated to TORMVirtualTableJSON
  TORMVirtualTableCursorJSON = class(TORMVirtualTableCursorIndex)
  public
    /// called to begin a search in the virtual table
    // - the TORMVirtualTablePrepared parameters were set by
    // TORMVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - only handled WHERE clause is for "ID = value" - other request will
    // return all records in ID order, and let the database engine handle it
    function Search(const Prepared: TORMVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row into a TSQLVar
    // - if aColumn=-1, will return the row ID as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; override;
  end;

  /// A TRestStorageInMemory-based virtual table using JSON storage
  // - for ORM access, you should use TORMModel.VirtualTableRegister method to
  // associated this virtual table module to a TORMVirtualTableAutoID class
  // - transactions are not handled by this module
  // - by default, no data is written on disk: you will need to call explicitly
  // aServer.StaticVirtualTable[aClass].UpdateToFile for file creation or refresh
  // - file extension is set to '.json'
  TORMVirtualTableJSON = class(TORMVirtualTable)
  protected
    fStaticInMemory: TRestStorageInMemory;
  public
    { overridden methods }
    /// create the virtual table access instance
    // - the created instance will be released when the virtual table will be
    // disconnected from the DB connection (e.g. xDisconnect method for SQLite3)
    // - shall raise an exception in case of invalid parameters (e.g. if the
    // supplied module is not associated to a TRestORMServer instance)
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TORMVirtualTableAutoID class and
    // create any associated Static: TRestStorage instance
    constructor Create(aModule: TORMVirtualTableModule;
      const aTableName: RawUTF8; FieldCount: integer;
      Fields: PPUTF8CharArray); override;
    /// returns the main specifications of the associated TORMVirtualTableModule
    // - this is a read/write table, without transaction, associated to the
    // TORMVirtualTableCursorJSON cursor type, with 'JSON' as module name
    // - no particular class is supplied here, since it will depend on the
    // associated Static instance
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TORMVirtualTableCursor.Search()
    // - only prepared WHERE statement is for "ID = value"
    // - only prepared ORDER BY statement is for ascending IDs
    function Prepare(var Prepared: TORMVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content from a TSQLVar array
    // - column order follows the Structure method, i.e.
    // StoredClassRecordProps.Fields[] order
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    // - does nothing by default, and returns false, i.e. always fails
    function Insert(aRowID: Int64; var Values: TSQLVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content from a TSQLVar array
    // - column order follows the Structure method, i.e.
    // StoredClassRecordProps.Fields[] order
    // - returns true on success, false otherwise
    // - does nothing by default, and returns false, i.e. always fails
    function Update(oldRowID, newRowID: Int64;
      var Values: TSQLVarDynArray): boolean; override;
  end;

  /// A TRestStorageInMemory-based virtual table using Binary storage
  // - for ORM access, you should use TORMModel.VirtualTableRegister method to
  // associated this virtual table module to a TORMVirtualTableAutoID class
  // - transactions are not handled by this module
  // - by default, no data is written on disk: you will need to call explicitly
  // aServer.StaticVirtualTable[aClass].UpdateToFile for file creation or refresh
  // - binary format is more efficient in term of speed and disk usage than
  // the JSON format implemented by TORMVirtualTableJSON
  // - binary format will be set by TORMVirtualTableJSON.CreateTableInstance
  // - file extension is set to '.data'
  TORMVirtualTableBinary = class(TORMVirtualTableJSON);



{ ************ TORMVirtualTableLog Virtual Table }

  /// Implements a read/only virtual table able to access a .log file, as created
  // by TSynLog
  // - to be used e.g. by a TORMLog_Log ('Log_' will identify this 'Log' module)
  // - the .log file name will be specified by the Table Name, to which a '.log'
  // file extension will be appended before loading it from the current directory
  TORMVirtualTableLog = class(TORMVirtualTable)
  protected
    fLogFile: TSynLogFile;
  public
    /// returns the main specifications of the associated TORMVirtualTableModule
    // - this is a read only table, with transaction, associated to the
    // TORMVirtualTableCursorLog cursor type, with 'Log' as module name,
    // and associated to TORMLog_Log table field layout
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); override;
    /// creates the TORMVirtualTable according to the supplied parameters
    // - aTableName will be checked against the current aModule.Server.Model
    // to retrieve the corresponding TORMVirtualTableAutoID class
    constructor Create(aModule: TORMVirtualTableModule; const aTableName: RawUTF8;
      FieldCount: integer; Fields: PPUTF8CharArray); override;
    /// release the associated .log file mapping and all internal structures
    destructor Destroy; override;
  end;

  /// A Virtual Table cursor for reading a TSynLogFile content
  // - this is the cursor class associated to TORMVirtualTableLog
  TORMVirtualTableCursorLog = class(TORMVirtualTableCursorIndex)
  public
    /// called to begin a search in the virtual table
    function Search(const Prepared: TORMVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row as TSQLVar
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; override;
  end;


{ ************ TRestStorageRemote for CRUD Redirection }

  /// REST storage with redirection to another REST instance
  // - allows redirection of all CRUD operations for a table to another
  // TRestORM instance, may be a remote TRestORMClient or a TRestORMServer
  // - will be used by TRestORMServer.RemoteDataCreate() method
  TRestStorageRemote = class(TRestStorage)
  protected
    fRemoteRest: TRestORM;
    fRemoteTableIndex: integer;
  public
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: TRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: TRawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
  public
    /// initialize the table storage redirection
    // - you should not have to use this constructor, but rather the
    // TRestORMServer.RemoteDataCreate() method which will create and register
    // one TRestStorageRemote instance
    constructor Create(aClass: TORMClass; aServer: TRestORMServer;
      aRemoteRest: TRestORM); reintroduce; virtual;
  published
    /// the remote ORM instance used for data persistence
    // - may be a TRestORMClient or a TRestORMServer instance
    property RemoteRest: TRestORM read fRemoteRest;
  end;



{ ************ TRestStorageShard as Abstract Sharded Storage Engine }

  /// defines how TRestStorageShard will handle its partioned process
  TRestStorageShardOption = (
    ssoNoUpdate, ssoNoUpdateButLastShard,
    ssoNoDelete, ssoNoDeleteButLastShard, ssoNoBatch,
    ssoNoList, ssoNoExecute, ssoNoUpdateField, ssoNoConsolidateAtDestroy);

  /// how TRestStorageShard will handle its partioned process
  TRestStorageShardOptions = set of TRestStorageShardOption;

  /// abstract REST storage with redirection to several REST instances, implementing
  // range ID partitioning for horizontal scaling
  // - such database shards will allow to scale with typical BigData storage
  // - this storage will add items on a server, initializing a new server
  // when the ID reached a defined range
  // - it will maintain a list of previous storages, then redirect reading and
  // updates to the server managing this ID (if possible - older shards may
  // be deleted/ignored to release resources)
  // - inherited class should override InitShards/InitNewShard to customize the
  // kind of TRestORM instances to be used for each shard (which may be local
  // or remote, a SQLite3 engine or an external SQL/NoSQL database)
  // - see inherited TRestStorageShardDB as defined in mORMotSQLite3.pas
  TRestStorageShard = class(TRestStorage)
  protected
    fShardRange: TID;
    fShardOffset: integer;
    fMaxShardCount: integer;
    fLastID: TID;
    fOptions: TRestStorageShardOptions;
    fShards: array of TRestORM;
    fShardLast: integer;
    fShardLastID: TID;
    fShardNextID: TID;
    fShardTableIndex: TIntegerDynArray;
    fShardBatch: array of TRestBatch;
    // will set Shards[],fShardLast,fShardLastID,fShardOffset
    procedure InitShards; virtual; abstract;
    // should always return non nil shard to contain new added IDs
    function InitNewShard: TRestORM; virtual; abstract;
    procedure InternalAddNewShard;
    function InternalShardBatch(ShardIndex: integer): TRestBatch;
  public
    // overriden methods which will handle all ORM process
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: TRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: TRawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    function InternalBatchStart(Method: TURIMethod;
      BatchOptions: TRestBatchOptions): boolean; override;
    procedure InternalBatchStop; override;
  public
    /// initialize the table storage redirection for sharding
    // - you should not have to use this constructor, but e.g.
    // TRestStorageShardDB.Create on a main TRestORMServer.StaticDataAdd()
    // - the supplied aShardRange should be < 1000 - and once set, you should NOT
    // change this value on an existing shard, unless process will be broken
    constructor Create(aClass: TORMClass; aServer: TRestORMServer;
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
    // - may return true, and a TRestORMHookClient or a TRestORMHookServer instance
    // with its associated index in TRestORM.Model.Tables[]
    // - warning: this method should be protected via StorageLock/StorageUnlock
    function ShardFromID(aID: TID; out aShardTableIndex: integer;
      out aShard: TRestORM; aOccasion: TORMOccasion = ooSelect;
      aShardIndex: PInteger = nil): boolean; virtual;
    /// get the row count of a specified table
    function TableRowCount(Table: TORMClass): Int64; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TORMClass): boolean; override;
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




implementation

uses
  mormot.rest.server;



{ ************ Virtual Table ORM Support }

{ TORMVirtualTablePrepared }

function TORMVirtualTablePrepared.IsWhereIDEquals(CalledFromPrepare: boolean): boolean;
begin
  result := (WhereCount = 1) and
            (Where[0].Column = VIRTUAL_TABLE_ROWID_COLUMN) and
            (CalledFromPrepare or (Where[0].Value.VType = ftInt64)) and
            (Where[0].Operation = soEqualTo);
end;

function TORMVirtualTablePrepared.IsWhereOneFieldEquals: boolean;
begin
  result := (WhereCount = 1) and
            (Where[0].Column >= 0) and
            (Where[0].Operation = soEqualTo);
end;


{ TORMVirtualTableModule }

constructor TORMVirtualTableModule.Create(aTableClass: TORMVirtualTableClass;
  aServer: TRestORMServer);
begin
  fTableClass := aTableClass;
  fServer := aServer;
  fTableClass.GetTableModuleProperties(fFeatures);
  fModuleName := fTableClass.ModuleName;
  if fFeatures.FileExtension = '' then // default extension is the module name
    fFeatures.FileExtension := UTF8ToString(LowerCase(fModuleName));
end;

function TORMVirtualTableModule.FileName(const aTableName: RawUTF8): TFileName;
begin
  result := UTF8ToString(aTableName) + '.' + FileExtension;
  if fFilePath = '' then
    result := ExeVersion.ProgramFilePath + result
  else
    result := IncludeTrailingPathDelimiter(fFilePath) + result;
end;


{ TORMVirtualTable }

constructor TORMVirtualTable.Create(aModule: TORMVirtualTableModule;
  const aTableName: RawUTF8; FieldCount: integer; Fields: PPUTF8CharArray);
var
  aClass: TRestStorageClass;
  aServer: TRestORMServer;
begin
  if (aModule = nil) or
     (aTableName = '') then
    raise EModelException.CreateUTF8('Invalid %.Create(%,"%")',
      [self, aModule, aTableName]);
  fModule := aModule;
  fTableName := aTableName;
  if fModule.fFeatures.StaticClass <> nil then
  begin
    // create new fStatic instance e.g. for TORMVirtualTableLog
    aServer := fModule.Server;
    if aServer = nil then
      raise EModelException.CreateUTF8('%.Server=nil for %.Create', [Module, self])
    else
      fStaticTableIndex := aServer.Model.GetTableIndex(aTableName);
    if fStaticTableIndex >= 0 then
    begin
      fStaticTable := aServer.Model.Tables[fStaticTableIndex];
      aClass := fModule.fFeatures.StaticClass;
      if aClass.InheritsFrom(TRestStorageInMemory) then
        fStatic := TRestStorageInMemoryClass(aClass).Create(
          fStaticTable, fModule.Server, fModule.FileName(aTableName),
          self.InheritsFrom(TORMVirtualTableBinary))
      else
        fStatic := aClass.Create(fStaticTable, fModule.Server);
      aServer.StaticTableSetup(fStaticTableIndex, fStatic, sVirtualTable);
      fStaticStorage := TRestStorage(fStatic);
      fStaticStorage.fStorageVirtual := self;
    end;
  end;
end;

destructor TORMVirtualTable.Destroy;
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

function TORMVirtualTable.Prepare(var Prepared: TORMVirtualTablePrepared): boolean;
begin
  result := self <> nil;
  if result then
    if (vtWhereIDPrepared in fModule.Features) and
       Prepared.IsWhereIDEquals(true) then
      with Prepared.Where[0] do
      begin
        // optimize for WHERE ID=? clause
        Value.VType := ftNull; // mark TORMVirtualTableCursorJSON expects it
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

function TORMVirtualTable.Drop: boolean;
begin
  result := false;  // no DROP TABLE to be implemented here
end;

function TORMVirtualTable.Delete(aRowID: Int64): boolean;
begin
  result := false;  // no DELETE to be implemented here
end;

function TORMVirtualTable.Insert(aRowID: Int64; var Values: TSQLVarDynArray;
  out insertedRowID: Int64): boolean;
begin
  result := false;  // no INSERT to be implemented here
end;

function TORMVirtualTable.Update(oldRowID, newRowID: Int64;
  var Values: TSQLVarDynArray): boolean;
begin
  result := false;  // no UPDATE to be implemented here
end;

function TORMVirtualTable.Transaction(aState: TORMVirtualTableTransaction;
  aSavePoint: integer): boolean;
begin
  result := (Module <> nil) and
            (vtWrite in Module.Features) and
            (aState in [vttBegin, vttSync, vttCommit, vttSavePoint, vttRelease]);
end;

function TORMVirtualTable.Rename(const NewName: RawUTF8): boolean;
begin
  result := false;
end;

class function TORMVirtualTable.ModuleName: RawUTF8;
const
  LEN: array[-1..5] of byte = (
    1, 16, 11, 4, 16, 11, 4);
begin
  if self = nil then
    result := ''
  else
  begin
    ClassToText(self, result);
    system.delete(result, 1, LEN[IdemPCharArray(pointer(result),
      ['TSQLVIRTUALTABLE', 'TSQLVIRTUAL', 'TSQL',
       'TORMVIRTUALTABLE', 'TORMVIRTUAL', 'TORM'])]);
  end;
end;

class function TORMVirtualTable.StructureFromClass(aClass: TORMClass;
  const aTableName: RawUTF8): RawUTF8;
begin
  FormatUTF8('CREATE TABLE % (%', [aTableName,
    GetVirtualTableSQLCreate(aClass.RecordProps)], result);
end;

function TORMVirtualTable.Structure: RawUTF8;
begin
  result := '';
  if self <> nil then
    if static <> nil then
      // e.g. for TORMVirtualTableJSON or TORMVirtualTableExternal
      result := StructureFromClass(StaticTable, TableName)
    else if (Module <> nil) and
            (Module.RecordClass <> nil) then
      // e.g. for TORMVirtualTableLog
      result := StructureFromClass(Module.RecordClass, TableName);
end;


{ TORMVirtualTableCursor }

constructor TORMVirtualTableCursor.Create(aTable: TORMVirtualTable);
begin
  fTable := aTable;
end;

procedure TORMVirtualTableCursor.SetColumn(var aResult: TSQLVar; aValue: Int64);
begin
  aResult.Options := [];
  aResult.VType := ftInt64;
  aResult.VInt64 := aValue;
end;

procedure TORMVirtualTableCursor.SetColumn(var aResult: TSQLVar;
  const aValue: double);
begin
  aResult.Options := [];
  aResult.VType := ftDouble;
  aResult.VDouble := aValue;
end;

procedure TORMVirtualTableCursor.SetColumn(var aResult: TSQLVar;
  const aValue: RawUTF8);
begin
  aResult.Options := [];
  aResult.VType := ftUTF8;
  fColumnTemp := aValue; // temporary copy available until next Column() call
  aResult.VText := pointer(fColumnTemp);
end;

procedure TORMVirtualTableCursor.SetColumn(var aResult: TSQLVar;
  aValue: PUTF8Char; aValueLength: integer);
begin
  aResult.Options := [];
  aResult.VType := ftUTF8;
  FastSetString(RawUTF8(fColumnTemp), aValue, aValueLength); // temporary copy
  aResult.VText := pointer(fColumnTemp);
end;

procedure TORMVirtualTableCursor.SetColumnBlob(var aResult: TSQLVar;
  aValue: pointer; aValueLength: integer);
begin
  aResult.Options := [];
  aResult.VType := ftBlob;
  SetString(fColumnTemp, PAnsiChar(aValue), aValueLength); // temporary copy
  aResult.VBlob := pointer(fColumnTemp);
  aResult.VBlobLen := aValueLength;
end;

procedure TORMVirtualTableCursor.SetColumnDate(var aResult: TSQLVar;
  const aValue: TDateTime; aWithMS: boolean);
begin
  if aWithMS then
    aResult.Options := [svoDateWithMS]
  else
    aResult.Options := [];
  aResult.VType := ftDate;
  aResult.VDateTime := aValue;
end;

procedure TORMVirtualTableCursor.SetColumnCurr64(var aResult: TSQLVar;
  aValue64: PInt64);
begin
  aResult.Options := [];
  aResult.VType := ftCurrency;
  PInt64(@aResult.VCurrency)^ := aValue64^;
end;



{ TORMVirtualTableCursorIndex }

function TORMVirtualTableCursorIndex.HasData: boolean;
begin
  result := (self <> nil) and (fCurrent <= fMax);
end;

function TORMVirtualTableCursorIndex.Next: boolean;
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

function TORMVirtualTableCursorIndex.Search(
  const Prepared: TORMVirtualTablePrepared): boolean;
begin
  fCurrent := 0; // mark EOF by default
  fMax := -1;
  result := true;
end;



{ ************ TRestStorage Abstract Class for ORM/REST Storage }

{ TRestStorage }

constructor TRestStorage.Create(aClass: TORMClass; aServer: TRestORMServer);
begin
  inherited Create(nil);
  if aClass = nil then
    raise ERestStorage.CreateUTF8('%.Create(aClass=nil)', [self]);
  InitializeCriticalSection(fStorageCriticalSection);
  fStoredClass := aClass;
  fStoredClassRecordProps := aClass.RecordProps;
  if aServer <> nil then
  begin
    fOwner := aServer;
    fModel := aServer.Model;
  end
  else
  begin
    // fallback to an owned model instance
    fModel := TORMModel.Create([aClass]);
    fModel.Owner := self;
  end;
  fStoredClassProps := fModel.Props[aClass];
  fStoredClassMapping := @fStoredClassProps.ExternalDB;
  fIsUnique := fStoredClassRecordProps.IsUniqueFieldsBits;
  fBasicSQLCount := 'SELECT COUNT(*) FROM ' +
    fStoredClassRecordProps.SQLTableName;
  fBasicSQLHasRows[false] := 'SELECT RowID FROM ' +
    fStoredClassRecordProps.SQLTableName + ' LIMIT 1';
  fBasicSQLHasRows[true] := fBasicSQLHasRows[false];
  system.delete(fBasicSQLHasRows[true], 8, 3);
end;

destructor TRestStorage.Destroy;
begin
  inherited;
  if fStorageCriticalSectionCount <> 0 then
    raise ERestStorage.CreateUTF8('%.Destroy with CS=%',
      [self, fStorageCriticalSectionCount]);
  DeleteCriticalSection(fStorageCriticalSection);
  if fStorageVirtual <> nil then
  begin
    // no GPF e.g. if DB release after server
    fStorageVirtual.fStatic := nil;
    fStorageVirtual.fStaticStorage := nil;
  end;
end;

function TRestStorage.CreateSQLMultiIndex(Table: TORMClass;
  const FieldNames: array of RawUTF8; Unique: boolean; IndexName: RawUTF8): boolean;
begin
  result := false; // not implemented in this basic REST static class
end;

function TRestStorage.SearchField(const FieldName: RawUTF8; FieldValue: Int64;
  out ResultID: TIDDynArray): boolean;
begin
  result := SearchField(FieldName, Int64ToUTF8(FieldValue), ResultID);
end;

function TRestStorage.RecordCanBeUpdated(Table: TORMClass; ID: TID;
  Action: TORMEvent; ErrorMsg: PRawUTF8 = nil): boolean;
begin
  result := (Owner = nil) or
            Owner.RecordCanBeUpdated(Table, ID, Action, ErrorMsg);
end;

function TRestStorage.RefreshedAndModified: boolean;
begin
  result := false; // no refresh necessary with "normal" static tables
end;

procedure TRestStorage.StorageLock(WillModifyContent: boolean;
  const msg: RawUTF8);
begin
  if fStorageLockLogTrace or
     (fStorageCriticalSectionCount > 1) then
    InternalLog('StorageLock % [%] %',
      [fStoredClass, msg, fStorageCriticalSectionCount]);
  EnterCriticalSection(fStorageCriticalSection);
  inc(fStorageCriticalSectionCount);
  if WillModifyContent and
     fStorageLockShouldIncreaseOwnerInternalState and
     (Owner <> nil) then
    inc(Owner.InternalState);
end;

procedure TRestStorage.StorageUnLock;
begin
  dec(fStorageCriticalSectionCount);
  if fStorageLockLogTrace then
    InternalLog('StorageUnlock % %',
      [fStoredClass, fStorageCriticalSectionCount]);
  if fStorageCriticalSectionCount < 0 then
    raise ERestStorage.CreateUTF8('%.StorageUnLock with CS=%',
      [self, fStorageCriticalSectionCount]);
  LeaveCriticalSection(fStorageCriticalSection);
end;

function TRestStorage.GetCurrentSessionUserID: TID;
begin
  if fOwner = nil then
    result := 0
  else
    result := fOwner.GetCurrentSessionUserID;
end;

procedure TRestStorage.RecordVersionFieldHandle(Occasion: TORMOccasion;
  var Decoder: TJSONObjectDecoder);
begin
  if fStoredClassRecordProps.RecordVersionField = nil then
    exit;
  if Owner = nil then
    raise ERestStorage.CreateUTF8('Owner=nil for %.%: TRecordVersion',
      [fStoredClass, fStoredClassRecordProps.RecordVersionField.Name]);
  Owner.Owner.RecordVersionHandle(Occasion, fStoredClassProps.TableIndex,
    Decoder, fStoredClassRecordProps.RecordVersionField);
end;

function TRestStorage.UnLock(Table: TORMClass; aID: TID): boolean;
begin
  result := Model.UnLock(Table, aID);
end;

function TRestStorage.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
begin
  if fStoredClassProps = nil then
    result := false
  else
  begin
    result := IdemPropNameU(fStoredClassProps.SQL.SelectAllWithRowID, SQL);
    if result then
      SQL := fStoredClassProps.SQL.SelectAllWithID
    else
      result := IdemPropNameU(fStoredClassProps.SQL.SelectAllWithID, SQL);
  end;
end;

function TRestStorage.GetStoredClassName: RawUTF8;
begin
  if self = nil then
    result := ''
  else
    ClassToText(fStoredClass, result);
end;



{ ************ TRestStorageInMemory as Stand-Alone JSON/Binary Storage }


{ TRestStorageRecordBased }

function TRestStorageRecordBased.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
var
  rec: TORM;
begin
  result := 0; // mark error
  if TableModelIndex <> fStoredClassProps.TableIndex then
    exit;
  rec := fStoredClass.Create;
  try
    rec.FillFrom(SentData);
    StorageLock(true, 'EngineAdd');
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

function TRestStorageRecordBased.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUTF8): boolean;
var
  rec: TORM;
begin
  // this implementation won't handle partial fields update (e.g. BatchUpdate
  // after FillPrepare) - but TRestStorageInMemory.EngineUpdate will
  if (ID <= 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
  begin
    result := false; // mark error
    exit;
  end;
  StorageLock(true, 'EngineUpdate');
  try
    rec := fStoredClass.Create;
    try
      rec.FillFrom(SentData);
      rec.IDValue := ID;
      result := UpdateOne(rec, SentData);
    finally
      rec.Free;
    end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageRecordBased.UpdateOne(ID: TID;
  const Values: TSQLVarDynArray): boolean;
var
  rec: TORM;
begin
  if ID <= 0 then
  begin
    result := false; // mark error
    exit;
  end;
  StorageLock(true, 'UpdateOne');
  try
    rec := fStoredClass.Create;
    try
      rec.SetFieldSQLVars(Values);
      rec.IDValue := ID;
      result := UpdateOne(rec, rec.GetJSONValues(true, False, ooUpdate));
    finally
      rec.Free;
    end;
  finally
    StorageUnLock;
  end;
end;


{ TRestStorageInMemoryUnique }

constructor TRestStorageInMemoryUnique.Create(aOwner: TRestStorageInMemory;
  aField: TORMPropInfo);
begin
  fOwner := aOwner;
  fPropInfo := aField;
  fCaseInsensitive := not (aBinaryCollation in aField.Attributes);
  fHasher.Init(@fOwner.fValues, nil, EventHash, nil, nil, EventCompare, false);
end;

function TRestStorageInMemoryUnique.EventCompare(const A, B): integer;
begin
  result := fPropInfo.CompareValue(
    TORM(A), TORM(B), fCaseInsensitive);
end;

function TRestStorageInMemoryUnique.EventHash(const Elem): cardinal;
begin
  result := fPropInfo.GetHash(TORM(Elem), fCaseInsensitive);
end;

function TRestStorageInMemoryUnique.Find(Rec: TORM): integer;
begin
  if self = nil then // no Unique index for this field
    result := -1
  else
  begin
    fLastFindHashCode := fPropInfo.GetHash(Rec, fCaseInsensitive);
    result := fHasher.Find(@Rec, fLastFindHashCode);
  end;
end;

function TRestStorageInMemoryUnique.AddedAfterFind(Rec: TORM): boolean;
begin
  fHasher.FindBeforeAdd(@Rec, result, fLastFindHashCode);
end;


{ TRestStorageInMemory }

constructor TRestStorageInMemory.Create(aClass: TORMClass;
  aServer: TRestORMServer; const aFileName: TFileName; aBinaryFile: boolean);
var
  f: PtrInt;
begin
  inherited Create(aClass, aServer);
  if (fStoredClassProps <> nil) and
     (fStoredClassProps.Kind in INSERT_WITH_ID) then
    raise ERestStorage.CreateUTF8('%.Create: % virtual table can''t be static',
      [self, aClass]);
  fFileName := aFileName;
  fBinaryFile := aBinaryFile;
  fValues.Init(TypeInfo(TORMObjArray), fValue, TORMDynArrayHashOne,
    TORMDynArrayCompare, nil, @fCount); // hashed and compared by ID
  fSearchRec := fStoredClass.Create;
  if (ClassType <> TRestStorageInMemory) and
     (fStoredClassProps <> nil) then
    with fStoredClassProps do
    begin
      // used by AdaptSQLForEngineList() method
      fBasicUpperSQLSelect[false] := UpperCase(SQL.SelectAllWithRowID);
      SetLength(fBasicUpperSQLSelect[false],
        length(fBasicUpperSQLSelect[false]) - 1); // trim right ';'
      fBasicUpperSQLSelect[true] :=
        StringReplaceAll(fBasicUpperSQLSelect[false], ' ROWID,', ' ID,');
    end;
  if not IsZero(fIsUnique) then
    with fStoredClassRecordProps.Fields do
    begin
      SetLength(fUnique, Count);
      for f := 0 to Count - 1 do
        if f in fIsUnique then
          fUnique[f] := TRestStorageInMemoryUnique.Create(self, List[f]);
    end;
  ReloadFromFile;
end;

destructor TRestStorageInMemory.Destroy;
begin
  UpdateFile;
  ObjArrayClear(fUnique);
  fValues.Clear; // to free all stored TORM instances
  fSearchRec.Free;
  inherited Destroy;
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

function TRestStorageInMemory.AddOne(Rec: TORM; ForceID: boolean;
  const SentData: RawUTF8): TID;
var
  ndx, f: PtrInt;
  added: boolean;
begin
  result := -1; // error
  if (self = nil) or
     (Rec = nil) then
    exit;
  // ensure no duplicated ID or unique field
  for f := 0 to high(fUnique) do
    if f in fIsUnique then
    begin
      ndx := fUnique[f].Find(Rec);
      if ndx >= 0 then
      begin
        InternalLog('AddOne: non unique %.% on % %',
          [fStoredClass, fUnique[f].PropInfo.Name, fValue[ndx], Rec], sllDB);
        exit;
      end;
    end;
  if ForceID then
  begin
    if Rec.IDValue <= 0 then
      raise ERestStorage.CreateUTF8('%.AddOne(%.ForceID=0)', [self, Rec]);
    ndx := fValues.FindHashed(Rec);
    if ndx >= 0 then
    begin
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
  for f := 0 to high(fUnique) do
    if f in fIsUnique then
      if not fUnique[f].AddedAfterFind(Rec) then // paranoid
        raise ERestStorage.CreateUTF8('%.AddOne on %.%',
          [self, Rec, fUnique[f].PropInfo.Name]);
  ndx := fValues.FindHashedForAdding(Rec, added);
  if added then
    fValue[ndx] := Rec
  else
    raise ERestStorage.CreateUTF8('%.AddOne % failed', [self, Rec]); // paranoid
  result := Rec.IDValue; // success
  fModified := true;
  if Owner <> nil then
    Owner.InternalUpdateEvent(oeAdd, fStoredClassProps.TableIndex,
      result, SentData, nil);
end;

function TRestStorageInMemory.UniqueFieldsUpdateOK(aRec: TORM;
  aUpdateIndex: integer): boolean;
var
  f, ndx: PtrInt;
begin
  result := false;
  for f := 0 to high(fUnique) do
    if f in fIsUnique then
    begin
      ndx := fUnique[f].Find(aRec);
      if (ndx >= 0) and
         (ndx <> aUpdateIndex) then
      begin
        InternalLog('UniqueFieldsUpdateOK failed on % for %',
          [fUnique[f].PropInfo.Name, aRec], sllDB);
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
    StorageLock(True, 'EngineDelete');
    try
      result := DeleteOne(IDToIndex(ID));
    finally
      StorageUnLock;
    end;
  end;
end;

function FindMaxID(p: PORM; n: integer): TID;
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

function FindMaxIDAndCheckSorted(p: PORM; n: integer;
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
      if id > result then // cmovg on 64-bit FPC
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
  rec: TORM;
begin
  if cardinal(aIndex) >= cardinal(fCount) then
    result := false
  else
  begin
    rec := fValue[aIndex];
    if rec.IDValue = fMaxID then
      fMaxID := 0; // recompute
    if Owner <> nil then
      // notify BEFORE deletion
      Owner.InternalUpdateEvent(oeDelete, fStoredClassProps.TableIndex,
        rec.IDValue, '', nil);
    for f := 0 to high(fUnique) do
      if f in fIsUnique then
        if fUnique[f].Hasher.FindBeforeDelete(@rec) < aIndex then
          raise ERestStorage.CreateUTF8('%.DeleteOne(%) failed on %',
            [self, aIndex, fUnique[f].PropInfo.Name]);
    if fValues.FindHashedAndDelete(rec) <> aIndex then
      raise ERestStorage.CreateUTF8('%.DeleteOne(%) failed', [self, aIndex]);
    if fMaxID = 0 then
      fMaxID := FindMaxID(pointer(fValue), fCount);
    fModified := true;
    result := true;
  end;
end;

function TRestStorageInMemory.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
var
  ndx: TIntegerDynArray;
  n, i: PtrInt;
begin // RecordCanBeUpdated() has already been called
  result := false;
  n := length(IDs);
  SetLength(ndx, n);
  dec(n);
  StorageLock(True, 'EngineDeleteWhere');
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

function TRestStorageInMemory.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  result := false; // there is no SQL engine with this class
end;

function TRestStorageInMemory.GetID(Index: integer): TID;
begin
  if (self = nil) or
     (cardinal(Index) >= cardinal(fCount)) then
    result := 0
  else
    result := fValue[Index].IDValue;
end;

function TRestStorageInMemory.GetItem(Index: integer): TORM;
begin
  if self <> nil then
    if cardinal(Index) >= cardinal(fCount) then
      raise ERestStorage.CreateUTF8('%.GetItem(%) out of range', [self, Index])
    else
      result := fValue[Index]
  else
    result := nil;
end;

procedure TRestStorageInMemory.GetJSONValuesEvent(aDest: pointer;
  aRec: TORM; aIndex: integer);
var
  W: TJSONSerializer absolute aDest;
begin
  aRec.GetJSONValues(W);
  W.Add(',');
end;

function TRestStorageInMemory.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
var
  P: PUTF8Char;
  Prop: RawUTF8;
  WithoutRowID: boolean;
begin
  result := inherited AdaptSQLForEngineList(SQL);
  if result then
    // 'select * from table'
    exit;
  if IdemPropNameU(fBasicSQLCount, SQL) or
     IdemPropNameU(fBasicSQLHasRows[false], SQL) or
     IdemPropNameU(fBasicSQLHasRows[true], SQL) then
  begin
    // 'select count(*) from table' will be handled as static
    result := true;
    exit;
  end;
  if fBasicUpperSQLSelect[false] = '' then
    exit;
  if IdemPChar(pointer(SQL), pointer(fBasicUpperSQLSelect[false])) then
    WithoutRowID := false
  else if IdemPChar(pointer(SQL), pointer(fBasicUpperSQLSelect[true])) then
    WithoutRowID := true
  else
    exit;
  P := pointer(SQL);
  inc(P, length(fBasicUpperSQLSelect[WithoutRowID]));
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
  const WhereFieldName, WhereValue: RawUTF8; OnFind: TFindWhereEqualEvent;
  Dest: pointer; FoundLimit, FoundOffset: integer; CaseInsensitive: boolean): PtrInt;
var
  WhereFieldIndex: integer;
begin
  result := 0;
  if (self = nil) or
     not Assigned(OnFind) then
    exit;
  if IsRowID(pointer(WhereFieldName)) then
    WhereFieldIndex := 0
  else
  begin
    WhereFieldIndex := fStoredClassRecordProps.Fields.
      IndexByName(pointer(WhereFieldName));
    if WhereFieldIndex < 0 then
      exit;
    inc(WhereFieldIndex); // FindWhereEqual() expects index = RTTI+1
  end;
  result := FindWhereEqual(WhereFieldIndex, WhereValue, OnFind, Dest,
    FoundLimit, FoundOffset, CaseInsensitive);
end;

function TRestStorageInMemory.FindWhereEqual(WhereField: integer;
  const WhereValue: RawUTF8; OnFind: TFindWhereEqualEvent; Dest: pointer;
  FoundLimit, FoundOffset: PtrInt; CaseInsensitive: boolean): PtrInt;
var
  i, currentRow, found: PtrInt;
  v: Int64;
  err: integer;
  P: TORMPropInfo;
  nfo: PRttiProp;
  offs: PtrUInt;
  ot: TRttiOrd;
  vp: PPtrUInt;

  function FoundOneAndReachedLimit: boolean;
  begin
    result := false; // continue search
    if FoundOffset > 0 then
    begin
      // omit first FoundOffset rows
      inc(currentRow);
      if currentRow > FoundOffset then
        FoundOffset := 0
      else
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
  if not (P.ORMFieldType in COPIABLE_FIELDS) then
    // nothing to search (e.g. oftUnknown or oftMany)
    exit;
  // use fUnique[] hash array for O(1) search if available
  if WhereField in fIsUnique then
  begin
    if FoundOffset <= 0 then
    begin
      // omit first FoundOffset rows
      P.SetValueVar(fSearchRec, WhereValue, false); // private copy for comparison
      i := fUnique[WhereField].Find(fSearchRec);
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
  currentRow := 0;
  if P.InheritsFrom(TORMPropInfoRTTIInt32) and
     (TORMPropInfoRTTIInt32(P).PropRtti.Kind in [rkInteger, rkEnumeration, rkSet]) then
  begin
    // search 8/16/32-bit properties
    v := GetInt64(pointer(WhereValue), err); // 64-bit for cardinal
    if err <> 0 then
      exit;
    nfo := TORMPropInfoRTTI(P).PropInfo;
    offs := TORMPropInfoRTTI(P).GetterIsFieldPropOffset;
    if offs <> 0 then
    begin
      // plain field with no getter
      ot := TORMPropInfoRTTI(P).PropRtti.Cache.RttiOrd;
      if ot in [roSLong, roULong] then
      begin
        // handle very common 32-bit integer field
        vp := pointer(fValue);
        for i := 0 to fCount - 1 do
          if (PCardinal(vp^ + offs)^ = PCardinal(@v)^) and
             FoundOneAndReachedLimit then
            break
          else
            inc(vp);
      end
      else
        // inlined GetOrdProp() for 8-bit or 16-bit values
        for i := 0 to fCount - 1 do
          if (FromRttiOrd(ot, pointer(PtrUInt(fValue[i]) + offs)) = v) and
             FoundOneAndReachedLimit then
            break;
    end
    else
      // has getter -> use GetOrdProp()
      for i := 0 to fCount - 1 do
        if (nfo^.GetOrdProp(fValue[i]) = v) and
           FoundOneAndReachedLimit then
          break;
  end
  else if P.InheritsFrom(TORMPropInfoRTTIInt64) then
  begin
    // search 64-bit integer property
    v := GetInt64(pointer(WhereValue), err);
    if err <> 0 then
      exit;
    nfo := TORMPropInfoRTTI(P).PropInfo;
    offs := TORMPropInfoRTTI(P).GetterIsFieldPropOffset;
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
  end
  else
  begin
    // generic search using fast CompareValue() overridden method
    P.SetValueVar(fSearchRec, WhereValue, false); // private copy for comparison
    for i := 0 to fCount - 1 do
      if (P.CompareValue(fValue[i], fSearchRec, CaseInsensitive) = 0) and
         FoundOneAndReachedLimit then
        break;
  end;
  result := found;
end;

function TRestStorageInMemory.FindMax(WhereField: integer;
  out max: Int64): boolean;
var
  P: TORMPropInfo;
  nfo: PRttiProp;
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
  if P.InheritsFrom(TORMPropInfoRTTIInt32) then
  begin
    nfo := TORMPropInfoRTTI(P).PropInfo;
    for i := 0 to fCount - 1 do
    begin
      v := nfo.GetOrdProp(fValue[i]);
      if v > max then
        max := v;
    end;
    result := true;
  end
  else if P.InheritsFrom(TORMPropInfoRTTIInt64) then
  begin
    nfo := TORMPropInfoRTTI(P).PropInfo;
    for i := 0 to fCount - 1 do
    begin
      v := nfo.GetInt64Prop(fValue[i]);
      if v > max then
        max := v;
    end;
    result := true;
  end;
end;

procedure TRestStorageInMemory.ForEach(WillModifyContent: boolean;
  OnEachProcess: TFindWhereEqualEvent; Dest: pointer);
var
  i: PtrInt;
begin
  if (self = nil) or
     (fCount = 0) or
     not Assigned(OnEachProcess) then
    exit;
  StorageLock(WillModifyContent, 'ForEach');
  try
    for i := 0 to fCount - 1 do
      OnEachProcess(Dest, fValue[i], i);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.GetJSONValues(Stream: TStream; Expand: boolean;
  Stmt: TSynTableStatement): PtrInt;
var
  ndx, KnownRowsCount: PtrInt;
  j: PtrInt;
  id: Int64;
  W: TJSONSerializer;
  IsNull: boolean;
  Prop: TORMPropInfo;
  bits: TFieldBits;
  withID: boolean;
label
  err;
begin // exact same format as TORMTable.GetJSONValues()
  result := 0;
  if length(Stmt.Where) > 1 then
    raise ERestStorage.CreateUTF8('%.GetJSONValues on % with Stmt.Where[]=%',
      [self, fStoredClass, length(Stmt.Where)]);
  if Stmt.Where = nil then
    // no WHERE statement -> get all rows -> set rows count
    if (Stmt.Limit > 0) and
       (fCount > Stmt.Limit) then
      KnownRowsCount := Stmt.Limit
    else
      KnownRowsCount := fCount
  else
    KnownRowsCount := 0;
  Stmt.SelectFieldBits(bits, withID);
  W := fStoredClassRecordProps.CreateJSONWriter(Stream, Expand, withID,
    bits, KnownRowsCount, {bufsize=}256 shl 10);
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
        fValue[ndx].GetJSONValues(W);
        W.Add(',');
      end;
      result := KnownRowsCount;
    end
    else
      case Stmt.Where[0].Operation of
        opEqualTo:
          result := FindWhereEqual(Stmt.Where[0].Field, Stmt.Where[0].Value,
            GetJSONValuesEvent, W, Stmt.Limit, Stmt.Offset);
        opIn:
          // only handle ID IN (..) by now
          if (Stmt.Where[0].Field <> 0) or
             (Stmt.Offset > 0) then
            goto err
          else
            with _Safe(Stmt.Where[0].ValueVariant)^ do
              for ndx := 0 to Count - 1 do
                if VariantToInt64(Values[ndx], id) then
                begin
                  j := IDToIndex(id);
                  if j >= 0 then
                  begin
                    fValue[j].GetJSONValues(W);
                    W.Add(',');
                    inc(result);
                    if (Stmt.Limit > 0) and
                       (result >= Stmt.Limit) then
                      break;
                  end;
                end
                else
                  goto err;
        opIsNull, opIsNotNull:
          if Stmt.Where[0].Field > 0 then
          begin
            Prop := fStoredClassRecordProps.Fields.List[Stmt.Where[0].Field - 1];
            if Prop.InheritsFrom(TORMPropInfoRTTIRawBlob) then
            begin
              IsNull := Stmt.Where[0].Operation = opIsNull;
              for ndx := 0 to fCount - 1 do
                if TORMPropInfoRTTIRawBlob(Prop).IsNull(fValue[ndx]) = IsNull then
                begin
                  fValue[ndx].GetJSONValues(W);
                  W.Add(',');
                  inc(result);
                  if (Stmt.Limit > 0) and
                    (result >= Stmt.Limit) then
                    break;
                end;
            end
            else
              goto err;
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
      fStoredClassRecordProps.SetJSONWriterColumnNames(W, 0);
    end;
    W.EndJSONObject(KnownRowsCount, result);
  finally
    W.Free;
  end;
end;

procedure TRestStorageInMemory.GetAllIDs(out ID: TIDDynArray);
var
  i: PtrInt;
begin
  StorageLock(false, 'GetAllIDs');
  try
    SetLength(ID, fCount);
    for i := 0 to Count - 1 do
      ID[i] := fValue[i].IDValue;
  finally
    StorageUnlock;
  end;
end;

function TRestStorageInMemory.EngineList(const SQL: RawUTF8; ForceAJAX: boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
// - GetJSONValues/FindWhereEqual will handle basic REST commands (not all SQL)
// only valid SQL command is "SELECT Field1,Field2 FROM Table WHERE ID=120;",
// i.e one Table SELECT with one optional "WHERE fieldname = value" statement
// - handle also basic "SELECT Count(*) FROM TableName;" SQL statement
// Note: this is sufficient for OneFieldValue() and MultiFieldValue() to work
var
  MS: TRawByteStringStream;
  ResCount: PtrInt;
  Stmt: TSynTableStatement;
  max: Int64;

  procedure SetCount(aCount: integer);
  begin
    FormatUTF8('[{"Count(*)":%}]'#$A, [aCount], result);
    ResCount := 1;
  end;

begin
  result := '';
  ResCount := 0;
  StorageLock(false, 'EngineList');
  try
    if IdemPropNameU(fBasicSQLCount, SQL) then
      SetCount(TableRowCount(fStoredClass))
    else if IdemPropNameU(fBasicSQLHasRows[false], SQL) or
            IdemPropNameU(fBasicSQLHasRows[true], SQL) then
      if TableRowCount(fStoredClass) = 0 then
      begin
        result := '{"fieldCount":1,"values":["RowID"]}'#$A;
        ResCount := 0;
      end
      else
      begin
        // return one row with fake ID=1
        result := '[{"RowID":1}]'#$A;
        ResCount := 1;
      end
    else
    begin
      Stmt := TSynTableStatement.Create(SQL,
        fStoredClassRecordProps.Fields.IndexByName,
        fStoredClassRecordProps.SimpleFieldsBits[ooSelect]);
      try
        if (Stmt.SQLStatement = '') or // parsing failed
           (length(Stmt.Where) > 1) or // only a SINGLE expression is allowed yet
           not IdemPropNameU(Stmt.TableName, fStoredClassRecordProps.SQLTableName) then
          // invalid request -> return ''
          exit;
        if Stmt.SelectFunctionCount = 0 then
        begin
          // save rows as JSON, with appropriate search according to Where.* arguments
          MS := TRawByteStringStream.Create;
          try
            ForceAJAX := ForceAJAX or not Owner.Owner.NoAJAXJSON;
            ResCount := GetJSONValues(MS, ForceAJAX, Stmt);
            result := MS.DataString;
          finally
            MS.Free;
          end;
        end
        else if (length(Stmt.Select) <> 1) or
                (Stmt.SelectFunctionCount <> 1) or
                (Stmt.Limit > 1) or (Stmt.Offset <> 0) then
          // handle a single max() or count() function with no LIMIT nor OFFSET
          exit
        else
          case Stmt.Select[0].FunctionKnown of
            funcCountStar:
              if Stmt.Where = nil then
                // was e.g. "SELECT Count(*) FROM TableName;"
                SetCount(TableRowCount(fStoredClass))
              else
              begin
                // was e.g. "SELECT Count(*) FROM TableName WHERE ..."
                ResCount := FindWhereEqual(Stmt.Where[0].Field,
                  Stmt.Where[0].Value, DoNothingEvent, nil, 0, 0);
                case Stmt.Where[0].Operation of
                  opEqualTo:
                    SetCount(ResCount);
                  opNotEqualTo:
                    SetCount(TableRowCount(fStoredClass) - ResCount);
                end;
              end;
            funcMax:
              if (Stmt.Where = nil) and
                 FindMax(Stmt.Select[0].Field, max) then
              begin
                FormatUTF8('[{"Max()":%}]'#$A, [max], result);
                ResCount := 1;
              end;
          else
            // unhandled Distinct() or other SQL functions
            exit;
          end;
      finally
        Stmt.Free;
      end;
    end;
  finally
    StorageUnLock;
  end;
  if ReturnedRowCount <> nil then
    ReturnedRowCount^ := ResCount;
end;

procedure TRestStorageInMemory.DropValues(andUpdateFile: boolean);
var
  f: PtrInt;
  timer: TPrecisionTimer;
begin
  StorageLock(true, 'DropValues');
  try
    fUnSortedID := false;
    fMaxID := 0;
    if fCount > 0 then
    begin
      timer.Start;
      for f := 0 to high(fUnique) do
        if f in fIsUnique then
          fUnique[f].Hasher.Clear;
      fValues.Hasher.Clear;
      fValues.Clear;
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

procedure TRestStorageInMemory.LoadFromJSON(const aJSON: RawUTF8);
var
  tmp: TSynTempBuffer;
begin
  tmp.Init(aJSON);
  try
    LoadFromJSON(tmp.buf, tmp.len);
  finally
    tmp.Done;
  end;
end;

procedure TRestStorageInMemory.ComputeStateAfterLoad(
  var loaded: TPrecisionTimer; binary: boolean);
const
  _CALLER: array[boolean] of string[7] = (
    'JSON', 'Binary');
var
  f, c: PtrInt;
  cf: RawUTF8;
  timer: TPrecisionTimer;
begin
  // now fValue[] contains the just loaded data
  loaded.Pause;
  timer.Start;
  fCount := length(fValue);
  c := fValues.ReHash;
  if c > 0 then
    cf := 'ID'
  else
    for f := 0 to high(fUnique) do
      if f in fIsUnique then
      begin
        c := fUnique[f].Hasher.ReHash({forced=}true, {grow=}false);
        if c > 0 then
        begin
          cf := fUnique[f].PropInfo.Name;
          break;
        end;
      end;
  if c > 0 then
  begin
    DropValues({andupdatefile=}false);
    raise ERestStorage.CreateUTF8('%.LoadFrom%: found % % in %.% field',
      [self, _CALLER[binary], Plural('duplicate', c), fStoredClass, {%H-}cf]);
  end;
  if binary then
  begin
    fMaxID := FindMaxID(pointer(fValue), fCount);
    fUnSortedID := false; // by SaveToBinary design
  end
  else
  // JSON may have been tampered
    fMaxID := FindMaxIDAndCheckSorted(pointer(fValue), fCount, fUnSortedID);
  InternalLog('LoadFrom% % count=% load=% index=%',
    [_CALLER[binary], fStoredClass, fCount, loaded.Stop, timer.Stop]);
end;

procedure TRestStorageInMemory.LoadFromJSON(
  JSONBuffer: PUTF8Char; JSONBufferLen: integer);
var
  T: TORMTableJSON;
  timer: TPrecisionTimer;
begin
  StorageLock(true, 'LoadFromJSON');
  try
    timer.Start;
    if fCount > 0 then
      DropValues({andupdatefile=}false);
    fModified := false;
    if JSONBuffer = nil then
      exit;
    T := TORMTableJSON.CreateFromTables(
      [fStoredClass], '', JSONBuffer, JSONBufferLen);
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

procedure TRestStorageInMemory.SaveToJSON(Stream: TStream; Expand: boolean);
var
  i, j: PtrInt;
  W: TJSONSerializer;
  ndx: TIntegerDynArray;
begin
  if self = nil then
    exit;
  StorageLock(false, 'SaveToJSON');
  try
    if fUnSortedID then
      fValues.CreateOrderedIndex(ndx, nil); // write in ascending ID order
    W := fStoredClassRecordProps.CreateJSONWriter(Stream, Expand, true,
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
        fValue[j].GetJSONValues(W);
        W.Add(',');
      end;
      W.EndJSONObject(fCount, fCount);
    finally
      W.Free;
    end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.SaveToJSON(Expand: boolean): RawUTF8;
var
  MS: TRawByteStringStream;
begin
  if self = nil then
    result := ''
  else
  begin
    MS := TRawByteStringStream.Create;
    try
      SaveToJSON(MS, Expand);
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
  rec: TORM;
  id: TID;
  s: RawUTF8;
  prop: TORMPropInfo;
  timer: TPrecisionTimer;
begin
  result := false;
  if self = nil then
    exit;
  timer.Start;
  MS := AlgoSynLZ.StreamUnCompress(Stream, TRESTSTORAGEINMEMORY_MAGIC);
  if MS = nil then
    exit;
  StorageLock(true, 'LoadFromBinary');
  try
    // check header: expect same exact RTTI
    R.Init(MS.Memory, MS.Size);
    R.VarUTF8(s);
    if (s <> '') and  // new fixed format
       not IdemPropNameU(s, 'TORMProperties') then // old buggy format
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
        rec := fStoredClass.Create;  // avoid URW699 with Delphi6/Kylix
        id := id + {$ifdef FPC_OR_UNICODE}TID{$endif}(R.VarUInt64);
        rec.IDValue := id;
        fValue[i] := rec;
      end;
    end
    else
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
      begin
        R.P := prop.SetBinary(fValue[i], R.P, R.Last);
        if R.P = nil then
        begin
          DropValues(false); // on error, reset all values
          exit;
        end;
      end;
    end;
    ComputeStateAfterLoad(timer, {binary=}true);
    result := true;
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
      raise ERestStorage.CreateUTF8('%.LoadFromResource with invalid % content',
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
    StorageLock(false, 'SaveToBinary');
    try
      // primitive magic and fields signature for file type identification
      W.Write1(0); // ClassName='TORMProperties' in old buggy format
      fStoredClassRecordProps.SaveBinaryHeader(W);
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
            raise ERestStorage.CreateUTF8('%.SaveToBinary(%): duplicated ID',
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
  ID: TID): RawUTF8;
var
  i: PtrInt;
begin
  // TableModelIndex is not useful here
  StorageLock(false, 'EngineRetrieve');
  try
    i := IDToIndex(ID);
    if i < 0 then
      result := ''
    else
      result := fValue[i].GetJSONValues(true, true, ooSelect);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.GetOne(aID: TID): TORM;
var
  i: PtrInt;
begin
  if aID = 0 then
    result := nil
  else
  begin
    StorageLock(false, 'GetOne');
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
  TableModelIndex: integer; ID: TID; const FieldName: RawUTF8;
  Increment: Int64): boolean;
var
  i, err: integer;
  P: TORMPropInfo;
  V: RawUTF8;
  wasString: boolean;
  int: Int64;
begin
  result := false;
  if (ID < 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) then
    exit;
  P := fStoredClassProps.Prop[FieldName];
  if P = nil then
    exit;
  if P.PropertyIndex in fIsUnique then
  begin
    InternalLog('EngineUpdateFieldIncrement(%) on UNIQUE %.%',
      [ID, fStoredClass, P.Name], sllDB);
    exit;
  end;
  StorageLock(false, 'EngineUpdateFieldIncrement');
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
    if wasString or (err <> 0) then
    begin
      InternalLog('EngineUpdateFieldIncrement: %.%=[%] not an integer',
        [fStoredClass, P.Name, V], sllDB);
      exit;
    end;
    Int64ToUtf8(int + Increment, V);
    P.SetValueVar(fValue[i], V, false);
    fModified := true;
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var
  P: TORMPropInfo;
  WhereValueString, SetValueString, SetValueJson: RawUTF8;
  i, WhereFieldIndex: PtrInt;
  SetValueWasString: boolean;
  match: TSynList;
  rec: TORM;
begin
  result := false;
  if (TableModelIndex <> fStoredClassProps.TableIndex) or
     (SetFieldName = '') or
     (SetValue = '') or
     (WhereFieldName = '') or
     (WhereValue = '') then
    exit;
  // handle destination field RTTI
  P := fStoredClassRecordProps.Fields.ByRawUTF8Name(SetFieldName);
  if P = nil then
    exit; // don't allow setting ID field, which is Read Only
  if P.PropertyIndex in fIsUnique then
  begin
    InternalLog('EngineUpdateField on UNIQUE %.%', [fStoredClass, P.Name], sllDB);
    exit; { TODO : allow update UNIQUE field? }
  end;
  SetValueWasString := SetValue[1] = '"';
  if SetValueWasString then
    UnQuoteSQLStringVar(pointer(SetValue), SetValueString)
  else
    SetValueString := SetValue;
  // handle search field RTTI
  if IsRowID(pointer(WhereFieldName)) then
  begin
    WhereFieldIndex := 0;
    WhereValueString := WhereValue;
  end
  else
  begin
    WhereFieldIndex := fStoredClassRecordProps.Fields.IndexByName(WhereFieldName);
    if WhereFieldIndex < 0 then
      exit;
    inc(WhereFieldIndex); // FindWhereEqual() expects index = RTTI+1
  end;
  if WhereValue[1] = '"' then
    UnQuoteSQLStringVar(pointer(WhereValue), WhereValueString)
  else
    WhereValueString := WhereValue;
  // search indexes, then apply updates
  match := TSynList.Create;
  StorageLock(true, 'EngineUpdateField');
  try
    // find matching match[]
    if FindWhereEqual(WhereFieldIndex, WhereValueString,
        DoAddToListEvent, match, 0, 0) = 0 then
      // match.Count=0 -> nothing to update
      exit;
    // check that all records can be updated
    for i := 0 to match.Count - 1 do
      if not RecordCanBeUpdated(fStoredClass,
          TORM(match.List[i]).IDValue, oeUpdate) then
        // one record update fails -> abort all
        exit;
    // update field value
    for i := 0 to match.Count - 1 do
    begin
      rec := match.List[i];
      P.SetValueVar(rec, SetValueString, SetValueWasString);
      if Owner <> nil then
      begin
        if {%H-}SetValueJson = '' then
          JSONEncodeNameSQLValue(P.Name, SetValue, SetValueJson);
        Owner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
          rec.IDValue, SetValueJson, nil);
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
  const SentData: RawUTF8): boolean;
var
  i: PtrInt;
  rec: TORM;
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
  StorageLock(true, 'EngineUpdate');
  try
    i := IDToIndex(ID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, ID, oeUpdate) then
      exit;
    if fUnique <> nil then
    begin
      // use temp rec to ensure no collision when updated Unique field
      rec := fValue[i].CreateCopy; // copy since can be a partial update
      rec.FillFrom(SentData);      // overwrite updated properties
      if not UniqueFieldsUpdateOK(rec, i) then
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
    if Owner <> nil then
      Owner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        ID, SentData, nil);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateOne(Rec: TORM;
  const SentData: RawUTF8): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (Rec = nil) or
     (PORMClass(Rec)^ <> fStoredClass) or
     (Rec.IDValue <= 0) then
    exit;
  StorageLock(true, 'UpdateOne');
  try
    i := IDToIndex(Rec.IDValue);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, Rec.IDValue, oeUpdate) then
      exit;
    if (fUnique <> nil) and
       not UniqueFieldsUpdateOK(Rec, i) then
      exit;
    CopyObject(Rec, fValue[i]);
    fModified := true;
    result := true;
    if Owner <> nil then
      Owner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        Rec.IDValue, SentData, nil);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateOne(ID: TID;
  const Values: TSQLVarDynArray): boolean;
var
  i: PtrInt;
  rec: TORM;
begin
  result := false;
  if ID <= 0 then
    exit;
  StorageLock(true, 'UpdateOne');
  try
    i := IDToIndex(ID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, ID, oeUpdate) then
      exit;
    if fUnique <> nil then
    begin
      // use temp rec to ensure no collision when updated Unique field
      rec := fValue[i].CreateCopy; // copy since can be a partial update
      if not rec.SetFieldSQLVars(Values) or
         not UniqueFieldsUpdateOK(rec, i) then
      begin
        rec.Free;
        exit;
      end;
      fValue[i].Free; // avoid memory leak
      fValue[i] := rec;
    end
    else
      // direct in-place (partial) update
      if not fValue[i].SetFieldSQLVars(Values) then
        exit;
    fModified := true;
    result := true;
    if Owner <> nil then
      Owner.InternalUpdateEvent(oeUpdate, fStoredClassProps.TableIndex,
        ID, fValue[i].GetJSONValues(True, False, ooUpdate), nil);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: TRawBlob): boolean;
var
  i: integer;
begin
  result := false;
  if (TableModelIndex <> fStoredClassProps.TableIndex) or
     not BlobField^.IsBlob then
    exit;
  StorageLock(false, 'EngineRetrieveBlob');
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

function TRestStorageInMemory.RetrieveBlobFields(Value: TORM): boolean;
var
  i, f: integer;
begin
  result := false;
  if (Value <> nil) and
     (Value.IDValue > 0) and
     (PORMClass(Value)^ = fStoredClass) then
    with Value.RecordProps do
      if BlobFields <> nil then
      begin
        StorageLock(false, 'RetrieveBlobFields');
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
  aID: TID; BlobField: PRttiProp; const BlobData: TRawBlob): boolean;
var
  i: integer;
  AffectedField: TFieldBits;
begin
  result := false;
  if (aID < 0) or
     (TableModelIndex <> fStoredClassProps.TableIndex) or
     not BlobField^.IsBlob then
    exit;
  StorageLock(true, 'EngineUpdateBlob');
  try
    i := IDToIndex(aID);
    if (i < 0) or
       not RecordCanBeUpdated(fStoredClass, aID, oeUpdate) then
      exit;
    // set blob value directly from RTTI property description
    BlobField.SetLongStrProp(fValue[i], BlobData);
    if Owner <> nil then
    begin
      fStoredClassRecordProps.FieldBitsFromBlobField(BlobField, AffectedField);
      Owner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
        aID, '', @AffectedField);
    end;
    fModified := true;
    result := true;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageInMemory.UpdateBlobFields(Value: TORM): boolean;
var
  i, f: integer;
begin
  result := false;
  if (Value <> nil) and
     (Value.IDValue > 0) and
     (PORMClass(Value)^ = fStoredClass) then
    with Value.RecordProps do
    if BlobFields <> nil then
    begin
      StorageLock(true, 'UpdateBlobFields');
      try
        i := IDToIndex(Value.IDValue);
        if (i < 0) or
           not RecordCanBeUpdated(Table, Value.IDValue, oeUpdate) then
          exit;
        for f := 0 to high(BlobFields) do
          BlobFields[f].CopyValue(Value, fValue[i]);
        if Owner <> nil then
          Owner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
            Value.IDValue, '', @fStoredClassRecordProps.FieldBits[oftBlob]);
        fModified := true;
        result := true;
      finally
        StorageUnLock;
      end;
    end
    else
      result := true; // as TRestORM.UpdateblobFields()
end;

function TRestStorageInMemory.TableRowCount(Table: TORMClass): Int64;
begin
  if Table <> fStoredClass then
    result := 0
  else
    result := fCount;
end;

function TRestStorageInMemory.TableHasRows(Table: TORMClass): boolean;
begin
  result := (Table = fStoredClass) and (fCount > 0);
end;

function TRestStorageInMemory.MemberExists(Table: TORMClass;
  ID: TID): boolean;
begin
  StorageLock(false, 'UpdateFile');
  try
    result := (Table = fStoredClass) and (IDToIndex(ID) >= 0);
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
  StorageLock(false, 'UpdateFile');
  try
    DeleteFile(FileName); // always overwrite previous file
    if fCount > 0 then
    begin
      F := TFileStream.Create(FileName, fmCreate);
      try
        if BinaryFile then
          SaveToBinary(F)
        else
          SaveToJSON(F, true);
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
  JSON: RawUTF8;
  Stream: TStream;
begin
  if (fFileName <> '') and
     FileExists(fFileName) then
  begin
    if fBinaryFile then
    begin
      Stream := FileStreamSequentialRead(fFileName);
      try
        LoadFromBinary(Stream)
      finally
        Stream.Free;
      end;
    end
    else
    begin
      JSON := AnyTextFileToRawUTF8(fFileName, true);
      LoadFromJSON(pointer(JSON), length(JSON)); // buffer parsed in-place
    end;
  end;
end;

function TRestStorageInMemory.SearchField(const FieldName, FieldValue: RawUTF8;
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
    StorageLock(false, 'SearchField');
    try
      n := FindWhereEqual(WhereField, FieldValue, DoAddToListEvent, match, 0, 0);
      if n = 0 then
        exit;
      SetLength(ResultID, n);
      for i := 0 to n - 1 do
        ResultID[i] := TORM(match.List[i]).IDValue;
    finally
      StorageUnLock;
    end;
  finally
    match.Free;
  end;
end;

function TRestStorageInMemory.SearchEvent(const FieldName, FieldValue: RawUTF8;
  OnFind: TFindWhereEqualEvent; Dest: pointer; FoundLimit, FoundOffset: PtrInt): integer;
begin
  result := 0;
  if (self = nil) or
     (fCount = 0) or
     (FieldName = '') then
    exit;
  StorageLock(false, 'SearchEvent');
  try
    result := FindWhereEqual(FieldName, FieldValue, OnFind, Dest,
      FoundLimit, FoundOffset);
  finally
    StorageUnlock;
  end;
end;

function TRestStorageInMemory.SearchCopy(
  const FieldName, FieldValue: RawUTF8): pointer;
begin
  if SearchEvent(FieldName, FieldValue, DoCopyEvent, @result, 1, 0) = 0 then
    result := nil;
end;

function TRestStorageInMemory.SearchInstance(
  const FieldName, FieldValue: RawUTF8): pointer;
begin
  if SearchEvent(FieldName, FieldValue, DoInstanceEvent, @result, 1, 0) = 0 then
    result := nil;
end;

function TRestStorageInMemory.SearchIndex(
  const FieldName, FieldValue: RawUTF8): integer;
begin
  if SearchEvent(FieldName, FieldValue, DoIndexEvent, @result, 1, 0) = 0 then
    result := -1;
end;

function TRestStorageInMemory.SearchCount(
  const FieldName, FieldValue: RawUTF8): integer;
begin
  result := SearchEvent(FieldName, FieldValue, DoNothingEvent, nil, 0, 0);
end;

class procedure TRestStorageInMemory.DoNothingEvent(
  aDest: pointer; aRec: TORM; aIndex: integer);
begin
end;

class procedure TRestStorageInMemory.DoCopyEvent(
  aDest: pointer; aRec: TORM; aIndex: integer);
begin
  if aDest <> nil then
    PPointer(aDest)^ := aRec.CreateCopy;
end;

class procedure TRestStorageInMemory.DoAddToListEvent(
  aDest: pointer; aRec: TORM; aIndex: integer);
begin
  if aDest <> nil then
    TSynList(aDest).Add(aRec);
end;

class procedure TRestStorageInMemory.DoInstanceEvent(
  aDest: pointer; aRec: TORM; aIndex: integer);
begin
  if aDest <> nil then
    PPointer(aDest)^ := aRec;
end;

class procedure TRestStorageInMemory.DoIndexEvent(
  aDest: pointer; aRec: TORM; aIndex: integer);
begin
  if aDest <> nil then
    PInteger(aDest)^ := aIndex;
end;


{ TRestStorageInMemoryExternal }

constructor TRestStorageInMemoryExternal.Create(aClass: TORMClass;
  aServer: TRestORMServer; const aFileName: TFileName; aBinaryFile: boolean);
begin
  inherited Create(aClass, aServer, aFileName, aBinaryFile);
  fStorageLockShouldIncreaseOwnerInternalState := false; // done by overriden StorageLock()
end;

procedure TRestStorageInMemoryExternal.StorageLock(WillModifyContent: boolean;
  const msg: RawUTF8);
begin
  inherited StorageLock(WillModifyContent, msg);
  if WillModifyContent and (Owner <> nil) then
    Owner.FlushInternalDBCache;
end;




{ ************ TORMVirtualTableJSON/TORMVirtualTableBinary Virtual Tables }


{ TORMVirtualTableCursorJSON }

function TORMVirtualTableCursorJSON.Column(aColumn: integer;
  var aResult: TSQLVar): boolean;
var
  store: TRestStorageInMemory;
begin
  if (self = nil) or
     (fCurrent > fMax) or
     (TORMVirtualTableJSON(Table).static = nil) then
  begin
    result := false;
    exit;
  end;
  store := TORMVirtualTableJSON(Table).fStaticInMemory;
  if Cardinal(fCurrent) >= Cardinal(store.fCount) then
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
          List[aColumn].GetFieldSQLVar(
            store.fValue[fCurrent], aResult, fColumnTemp);
    result := true;
  end;
end;

function TORMVirtualTableCursorJSON.Search(
  const Prepared: TORMVirtualTablePrepared): boolean;
var
  store: TRestStorageInMemory;
begin
  result := false;
  inherited Search(Prepared); // mark EOF by default
  if not Table.InheritsFrom(TORMVirtualTableJSON) then
    exit;
  store := TORMVirtualTableJSON(Table).fStaticInMemory;
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
          if Column in store.fIsUnique then
          begin
            store.fStoredClassRecordProps.Fields.List[Column].SetFieldSQLVar(
              store.fSearchRec, Value);
            fMax := store.fUnique[Column].Find(store.fSearchRec);
            if fMax >= 0 then
              fCurrent := fMax; // value found with O(1) search
          end;
    end;
  result := true; // no DB error
end;



{ TORMVirtualTableJSON }

constructor TORMVirtualTableJSON.Create(aModule: TORMVirtualTableModule;
  const aTableName: RawUTF8; FieldCount: integer; Fields: PPUTF8CharArray);
begin
  inherited Create(aModule, aTableName, FieldCount, Fields);
  fStaticInMemory := fStatic as TRestStorageInMemory;
end;

function TORMVirtualTableJSON.Delete(aRowID: Int64): boolean;
begin
  result := (static <> nil) and static.Delete(StaticTable, aRowID);
  if result and (StaticStorage <> nil) and (StaticStorage.Owner <> nil) then
    StaticStorage.Owner.CacheOrNil.NotifyDeletion(StaticTable, aRowID);
end;

function TORMVirtualTableJSON.Drop: boolean;
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

class procedure TORMVirtualTableJSON.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite, vtWhereIDPrepared];
  aProperties.CursorClass := TORMVirtualTableCursorJSON;
  aProperties.StaticClass := TRestStorageInMemoryExternal; // will flush Cache
  if InheritsFrom(TORMVirtualTableBinary) then
    aProperties.FileExtension := 'data';
  // default will follow the class name, e.g. '.json' for TORMVirtualTableJSON
end;

function TORMVirtualTableJSON.Insert(aRowID: Int64;
  var Values: TSQLVarDynArray; out insertedRowID: Int64): boolean;
var
  rec: TORM;
begin
  result := false;
  if (self = nil) or
     (static = nil) then
    exit;
  rec := StaticTable.Create;
  try
    if rec.SetFieldSQLVars(Values) then
    begin
      if aRowID > 0 then
        rec.IDValue := aRowID;
      insertedRowID := fStaticInMemory.AddOne(rec, aRowID > 0,
        rec.GetJSONValues(true, false, ooInsert));
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

function TORMVirtualTableJSON.Prepare(
  var Prepared: TORMVirtualTablePrepared): boolean;
begin
  result := inherited Prepare(Prepared); // optimize ID=? WHERE clause
  if result and (static <> nil) then
  begin
    if Prepared.IsWhereOneFieldEquals then
      with Prepared.Where[0] do
        if (Column >= 0) and
           (Column in fStaticInMemory.fIsUnique) then
        begin
          Value.VType := ftNull; // mark TORMVirtualTableCursorJSON expects it
          OmitCheck := true;
          Prepared.EstimatedCost := costSecondaryIndex;
          Prepared.EstimatedRows := 10;
        end
        else if Prepared.EstimatedCost in [costFullScan, costScanWhere] then
          Prepared.EstimatedRows := fStaticInMemory.Count;
  end;
end;

function TORMVirtualTableJSON.Update(oldRowID, newRowID: Int64;
  var Values: TSQLVarDynArray): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (static = nil) or
     (oldRowID <> newRowID) or (newRowID <= 0) then // don't allow ID change
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



{ ************ TORMVirtualTableLog Virtual Table }

{ TORMVirtualTableLog }

type
  /// Record associated to Virtual Table implemented in Delphi, for Read/Only
  // access to a .log file, as created by TSynLog
  // - not used as real instances, but only used by the TORMVirtualTableLog module
  // to provide the field layout needed to create the column layout for the
  // CREATE TABLE statement
  TORMLogFile = class(TORMVirtualTableAutoID)
  protected
    fContent: RawUTF8;
    fDateTime: TDateTime;
    fLevel: TSynLogInfo;
  published
    /// the log event time stamp
    property DateTime: TDateTime read fDateTime;
    /// the log event level
    property Level: TSynLogInfo read fLevel;
    /// the textual message associated to the log event
    property Content: RawUTF8 read fContent;
  end;

constructor TORMVirtualTableLog.Create(aModule: TORMVirtualTableModule;
  const aTableName: RawUTF8; FieldCount: integer; Fields: PPUTF8CharArray);
var
  aFileName: TFileName;
begin
  inherited Create(aModule, aTableName, FieldCount, Fields);
  if FieldCount = 1 then
    aFileName := UTF8ToString(Fields[0])
  else
    aFileName := aModule.FileName(aTableName);
  fLogFile := TSynLogFile.Create(aFileName);
end;

destructor TORMVirtualTableLog.Destroy;
begin
  fLogFile.Free;
  inherited;
end;

class procedure TORMVirtualTableLog.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWhereIDPrepared];
  aProperties.CursorClass := TORMVirtualTableCursorLog;
  aProperties.RecordClass := TORMLogFile;
end;


{ TORMVirtualTableCursorLog }

function TORMVirtualTableCursorLog.Column(aColumn: integer;
  var aResult: TSQLVar): boolean;
var
  LogFile: TSynLogFile;
begin
  result := false;
  if (self = nil) or
     (fCurrent > fMax) then
    exit;
  LogFile := TORMVirtualTableLog(Table).fLogFile;
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

function TORMVirtualTableCursorLog.Search(
  const Prepared: TORMVirtualTablePrepared): boolean;
begin
  result := inherited Search(Prepared); // mark EOF by default
  if result then
  begin
    fMax := TORMVirtualTableLog(Table).fLogFile.Count - 1; // search all range
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

constructor TRestStorageRemote.Create(aClass: TORMClass;
  aServer: TRestORMServer; aRemoteRest: TRestORM);
begin
  if aRemoteRest = nil then
    raise ERestStorage.CreateUTF8('%.Create(nil)', [self]);
  inherited Create(aClass, aServer);
  fRemoteTableIndex := aRemoteRest.Model.GetTableIndexExisting(aClass);
  fRemoteRest := aRemoteRest;
end;

function TRestStorageRemote.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
begin
  result := fRemoteRest.EngineAdd(fRemoteTableIndex, SentData);
end;

function TRestStorageRemote.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  result := fRemoteRest.EngineDelete(fRemoteTableIndex, ID);
end;

function TRestStorageRemote.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
begin
  result := fRemoteRest.EngineDeleteWhere(fRemoteTableIndex, SQLWhere, IDs);
end;

function TRestStorageRemote.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  result := fRemoteRest.EngineExecute(aSQL);
end;

function TRestStorageRemote.EngineList(const SQL: RawUTF8; ForceAJAX: boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
begin
  result := fRemoteRest.EngineList(SQL, ForceAJAX, ReturnedRowCount);
end;

function TRestStorageRemote.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
begin
  result := fRemoteRest.EngineRetrieve(fRemoteTableIndex, ID);
end;

function TRestStorageRemote.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: TRawBlob): boolean;
begin
  if (self = nil) or
     (BlobField = nil) then
    result := false
  else
    result := fRemoteRest.EngineRetrieveBlob(fRemoteTableIndex,
      aID, BlobField, BlobData);
end;

function TRestStorageRemote.EngineUpdate(TableModelIndex: integer;
  ID: TID; const SentData: RawUTF8): boolean;
begin
  result := fRemoteRest.EngineUpdate(fRemoteTableIndex, ID, SentData);
end;

function TRestStorageRemote.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: TRawBlob): boolean;
begin
  if (self = nil) or
     (BlobField = nil) then
    result := false
  else
    result := fRemoteRest.EngineUpdateBlob(fRemoteTableIndex,
      aID, BlobField, BlobData);
end;

function TRestStorageRemote.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
begin
  result := fRemoteRest.EngineUpdateField(fRemoteTableIndex,
    SetFieldName, SetValue, WhereFieldName, WhereValue);
end;

function TRestStorageRemote.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
begin
  result := fRemoteRest.EngineUpdateFieldIncrement(fRemoteTableIndex,
    ID, FieldName, Increment);
end;



{ ************ TRestStorageShard as Abstract Sharded Storage Engine }


{ TRestStorageShard }

const
  MIN_SHARD = 1000;

constructor TRestStorageShard.Create(aClass: TORMClass;
  aServer: TRestORMServer; aShardRange: TID; aOptions: TRestStorageShardOptions; aMaxShardCount: integer);
var
  i, n: PtrInt;
begin
  if aShardRange < MIN_SHARD then
    raise ERestStorage.CreateUTF8('%.Create(%,aShardRange=%<%) does not make sense',
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
  rest: TRestORM;
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
      rest.Free;
      for j := i + 1 to high(fShards) do
        if fShards[j] = rest then
          fShards[j] := nil; // same instance re-used in fShards[]
    end;
  end;
end;

procedure TRestStorageShard.ConsolidateShards;
begin // do nothing by default
end;

procedure TRestStorageShard.RemoveShard(aShardIndex: integer);
begin
  StorageLock(true, 'RemoveShard');
  try
    if (fShards <> nil) and
       (cardinal(aShardIndex) <= cardinal(fShardLast)) then
    begin
      FreeAndNil(fShards[aShardIndex]);
      fShardTableIndex[aShardIndex] := -1;
    end;
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageShard.InternalAddNewShard;
var
  rest: TRestORM;
  i: PtrInt;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fOwner.Owner.Enter('InternalAddNewShard: #% for %',
    [fShardLast + 1, fStoredClass], self);
  rest := InitNewShard;
  if rest = nil then
    raise ERestStorage.CreateUTF8('%.InitNewShard(%) =nil',
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
  out aShardTableIndex: integer; out aShard: TRestORM;
  aOccasion: TORMOccasion; aShardIndex: PInteger): boolean;
var
  ndx: cardinal;
begin
  result := false;
  if aID <= 0 then
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
  if (ndx <= cardinal(fShardLast)) and
     (fShards[ndx] <> nil) then
  begin
    case aOccasion of
      ooUpdate:
        if (ssoNoUpdateButLastShard in fOptions) and
           (ndx <> cardinal(fShardLast)) then
          exit;
      ooDelete:
        if (ssoNoDeleteButLastShard in fOptions) and
           (ndx <> cardinal(fShardLast)) then
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
  const SentData: RawUTF8): TID;
var
  data: RawUTF8;
  i: PtrInt;
begin
  if JSONGetID(pointer(SentData), result) then
    raise ERestStorage.CreateUTF8('%.EngineAdd(%) unexpected ID in %',
      [self, fStoredClass, SentData]);
  StorageLock(true, 'EngineAdd');
  try
    inc(fShardLastID);
    if fShardLastID >= fShardNextID then
    begin
      InternalAddNewShard;
      if fShardLastID >= fShardNextID then
        raise ERestStorage.CreateUTF8('%.EngineAdd(%) fShardNextID',
          [self, fStoredClass]);
    end;
    result := fShardLastID;
    i := PosExChar('{', SentData);
    if i = 0 then
      FormatUTF8('{ID:%}', [result], data)
    else
    begin
      data := SentData;
      insert(FormatUTF8('ID:%,', [result]), data, i + 1);
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
  tableIndex, shardIndex: integer;
  rest: TRestORM;
begin
  StorageLock(true, 'EngineDelete');
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
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
var
  i: PtrInt;
  ndx: cardinal;
  id: array of TInt64DynArray; // IDs split per shard
  idn: TIntegerDynArray;
  sql: RawUTF8;
begin
  result := false;
  if (IDs = nil) or
     (ssoNoDelete in fOptions) then
    exit;
  StorageLock(true, 'EngineDeleteWhere');
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
        sql := Int64DynArrayToCSV(pointer(id[i]), idn[i], 'ID in (', ')');
        if not fShards[i].EngineDeleteWhere(
            fShardTableIndex[i], sql, TIDDynArray(id[i])) then
          result := false;
      end;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  StorageLock(false, 'EngineExecute');
  try
    if (fShardLast >= 0) and
       not (ssoNoExecute in fOptions) then
      result := fShards[fShardLast].EngineExecute(aSQL)
    else
      result := false;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.TableHasRows(Table: TORMClass): boolean;
begin
  result := fShards <> nil;
end;

function TRestStorageShard.TableRowCount(Table: TORMClass): Int64;
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

function TRestStorageShard.EngineList(const SQL: RawUTF8;
  ForceAJAX: boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var
  ResCount: PtrInt;
begin
  result := ''; // indicates error occurred
  StorageLock(false, 'EngineList');
  try
    ResCount := 0;
    if IdemPropNameU(fBasicSQLCount, SQL) then
    begin
      FormatUTF8('[{"Count(*)":%}]'#$A, [TableRowCount(fStoredClass)], result);
      ResCount := 1;
    end
    else if IdemPropNameU(fBasicSQLHasRows[false], SQL) or
            IdemPropNameU(fBasicSQLHasRows[true], SQL) then
      if fShards <> nil then
      begin // return one row with fake ID=1
        result := '[{"RowID":1}]'#$A;
        ResCount := 1;
      end
      else
        result := '{"fieldCount":1,"values":["RowID"]}'#$A
    else
    begin
      if (fShardLast >= 0) and
         not (ssoNoList in fOptions) then
        result := fShards[fShardLast].EngineList(SQL, ForceAJAX, @ResCount);
    end;
    if ReturnedRowCount <> nil then
      ReturnedRowCount^ := ResCount;
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
var
  tableIndex: integer;
  rest: TRestORM;
begin
  StorageLock(false, 'EngineRetrieve');
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
  aID: TID; BlobField: PRttiProp; out BlobData: TRawBlob): boolean;
var
  tableIndex: integer;
  rest: TRestORM;
begin
  StorageLock(false, 'EngineRetrieveBlob');
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
  ID: TID; const SentData: RawUTF8): boolean;
var
  tableIndex, shardIndex: integer;
  rest: TRestORM;
begin
  StorageLock(true, 'EngineUpdate');
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
  aID: TID; BlobField: PRttiProp; const BlobData: TRawBlob): boolean;
var
  tableIndex: integer;
  rest: TRestORM;
begin
  result := false;
  StorageLock(true, 'EngineUpdateBlob');
  try
    if ShardFromID(aID, tableIndex, rest, ooUpdate) then
      result := rest.EngineUpdateBlob(tableIndex, aID, BlobField, BlobData);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
begin
  result := false;
  StorageLock(true, 'EngineUpdateField');
  try
    if not ((ssoNoUpdate in fOptions) or (ssoNoUpdateField in fOptions)) then
      result := fShards[fShardLast].EngineUpdateField(fShardTableIndex[fShardLast], SetFieldName, SetValue, WhereFieldName, WhereValue);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUTF8; Increment: Int64): boolean;
var
  tableIndex: integer;
  rest: TRestORM;
begin
  result := false;
  StorageLock(true, 'EngineUpdateFieldIncrement');
  try
    if ShardFromID(ID, tableIndex, rest, ooUpdate) then
      result := rest.EngineUpdateFieldIncrement(tableIndex,
        ID, FieldName, Increment);
  finally
    StorageUnLock;
  end;
end;

function TRestStorageShard.InternalBatchStart(Method: TURIMethod;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false;
  if ssoNoBatch in fOptions then
    exit;
  // lock is protected by try..finally in TRestORMServer.RunBatch caller method
  StorageLock(true, 'InternalBatchStart');
  try
    if fShardBatch <> nil then
      raise ERestStorage.CreateUTF8('%.InternalBatchStop should have been called', [self]);
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
    raise ERestStorage.CreateUTF8('%.InternalShardBatch(%)',
      [self, ShardIndex]);
  if fShardBatch = nil then
    raise ERestStorage.CreateUTF8('%.InternalBatchStart should have been called',
      [self]);
  if ShardIndex >= length(fShardBatch) then
    // InitNewShard just called
    SetLength(fShardBatch, ShardIndex + 1);
  if fShardBatch[ShardIndex] = nil then
    if fShards[ShardIndex] <> nil then
      fShardBatch[ShardIndex] := TRestBatch.Create(fShards[ShardIndex],
        fStoredClass, 10000, [boExtendedJSON])
    else
      raise ERestStorage.CreateUTF8('%.InternalShardBatch fShards[%]=nil',
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

end.

