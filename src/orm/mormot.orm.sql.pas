/// ORM Types and Classes for the SQL Database Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.sql;

{
  *****************************************************************************

   ORM SQL Database Access using mormot.db.sql units
    - TRestStorageExternal for ORM/REST Storage over SQL
    - TOrmVirtualTableExternal for External SQL Virtual Tables
    - External SQL Database Engines Registration Functions

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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.db.core,
  mormot.db.sql,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver;


{ *********** TRestStorageExternal for ORM/REST Storage over SQL }

type
  TRestStorageExternal = class;

  /// event handler called to customize the computation of a new ID
  // - should set Handled=TRUE if a new ID has been computed and returned
  // - Handled=FALSE would let the default ID computation take place
  // - note that execution of this method would be protected by a mutex, so
  // it would be thread-safe
  TOnEngineAddComputeID = function(Sender: TRestStorageExternal;
    var Handled: boolean): TID of object;

  /// REST server with direct access to a mormot.db.sql-based external database
  // - handle all REST commands, using the external SQL database connection,
  // and prepared statements
  // - is used by TRestServer.Uri for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TOrmVirtualTableExternal[Cursor] classes
  TRestStorageExternal = class(TRestStorage)
  protected
    /// values retrieved from fStoredClassProps.ExternalDB settings
    fTableName: RawUtf8;
    fProperties: TSqlDBConnectionProperties;
    fSelectOneDirectSQL, fSelectAllDirectSQL, fSelectTableHasRowsSQL: RawUtf8;
    fRetrieveBlobFieldsSQL, fUpdateBlobfieldsSQL: RawUtf8;
    // ID handling during Add/Insert
    fEngineAddUseSelectMaxID: boolean;
    fEngineLockedMaxID: TID;
    fOnEngineAddComputeID: TOnEngineAddComputeID;
    fEngineAddForcedID: TID;
    /// external column layout as retrieved by fProperties
    // - used internally to guess e.g. if the column is indexed
    // - fFieldsExternal[] contains the external table info, and the internal
    // column name is available via fFieldsExternalToInternal[]
    fFieldsExternal: TSqlDBColumnDefineDynArray;
    /// gives the index of each fFieldsExternal[] item in Props.Fields[]
    // - is >=0 for index in Props.Fields[], -1 for RowID/ID, -2 if unknown
    // - use InternalFieldNameToFieldExternalIndex() to convert from column name
    fFieldsExternalToInternal: TIntegerDynArray;
    /// gives the index of each in Props.Fields[]+1 in fFieldsExternal[]
    // - expects [0] of RowID/ID, [1..length(fFieldNames)] for others
    fFieldsInternalToExternal: TIntegerDynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TUriMethod;
    fBatchOptions: TRestBatchOptions;
    fBatchCapacity, fBatchCount: integer;
    // BATCH sending uses TEXT storage for direct sending to database driver
    fBatchIDs: TIDDynArray;
    fBatchValues: TRawUtf8DynArray;
    // some sub-functions used by Create() during DB initialization
    procedure InitializeExternalDB(const log: ISynLog);
    procedure LogFields(const log: ISynLog);
    procedure FieldsInternalInit;
    function FieldsExternalIndexOf(
      const ColName: RawUtf8; CaseSensitive: boolean): PtrInt;
    function PropInfoToExternalField(Prop: TOrmPropInfo;
      var Column: TSqlDBColumnCreate): boolean;
    /// get fFieldsExternal[] index using fFieldsExternalToInternal[] mapping
    // - do handle ID/RowID fields and published methods
    function InternalFieldNameToFieldExternalIndex(
      const InternalFieldName: RawUtf8): integer;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function PrepareInlinedForRows(const aSql: RawUtf8): ISqlDBStatement;
    /// overloaded method using FormatUtf8() and binding mormot.db.sql parameters
    function PrepareDirectForRows(SqlFormat: PUtf8Char;
      const Args, Params: array of const): ISqlDBStatement;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function ExecuteInlined(const aSql: RawUtf8;
      ExpectResults: boolean): ISqlDBRows; overload;
    /// overloaded method using FormatUtf8() and inlined parameters
    function ExecuteInlined(SqlFormat: PUtf8Char; const Args: array of const;
      ExpectResults: boolean): ISqlDBRows; overload;
    /// overloaded method using FormatUtf8() and binding mormot.db.sql parameters
    function ExecuteDirect(SqlFormat: PUtf8Char; const Args, Params: array of const;
      ExpectResults: boolean): ISqlDBRows;
    /// overloaded method using FormatUtf8() and binding mormot.db.sql parameters
    function ExecuteDirectSqlVar(SqlFormat: PUtf8Char; const Args: array of const;
       var Params: TSqlVarDynArray; const LastIntegerParam: Int64;
       ParamsMatchCopiableFields: boolean): boolean;
    /// run INSERT of UPDATE from the corresponding JSON object
    // - Occasion parameter shall be only either soInsert or soUpate
    // - each JSON field will be bound with the proper SQL type corresponding to
    // the real external table columns (e.g. as TEXT for variant)
    // - returns 0 on error, or the Updated/Inserted ID
    function ExecuteFromJson(const SentData: RawUtf8; Occasion: TOrmOccasion;
      UpdatedID: TID; BatchOptions: TRestBatchOptions): TID;
    /// compute the INSERT or UPDATE statement as decoded from a JSON object
    function JsonDecodedPrepareToSql(var Decoder: TJsonObjectDecoder;
      out ExternalFields: TRawUtf8DynArray; out Types: TSqlDBFieldTypeArray;
      Occasion: TOrmOccasion; BatchOptions: TRestBatchOptions;
      BoundArray: boolean): RawUtf8;
    function GetConnectionProperties: TSqlDBConnectionProperties;
    /// check rpmClearPoolOnConnectionIssue in fStoredClassMapping.Options
    function HandleClearPoolOnConnectionIssue: boolean;
  public
    // overridden methods calling the external engine with SQL via Execute
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function EngineExecute(const aSql: RawUtf8): boolean; override;
    function EngineLockedNextID: TID; virtual;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID; const
       SentData: RawUtf8): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; out ResultID: TIDDynArray): boolean;
    // overridden method returning TRUE for next calls to EngineAdd/Update/Delete
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Encoding: TRestBatchEncoding;
      BatchOptions: TRestBatchOptions): boolean; override;
    // internal method called by TRestServer.RunBatch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    procedure InternalBatchStop; override;
    /// called internally by EngineAdd/EngineUpdate/EngineDelete in batch mode
    procedure InternalBatchAppend(const aValue: RawUtf8; aID: TID);
    /// TRestServer.Uri use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - change 'RowID' into 'ID' column name, internal field names into
    // mapped external field names ('AS [InternalFieldName]' if needed), and
    // SqlTableName into fTableName
    // - any 'LIMIT #' clause will be changed into the appropriate SQL statement
    // - handle also statements to avoid slow virtual table full scan, e.g.
    // $ SELECT count(*) FROM table
    function AdaptSqlForEngineList(var SQL: RawUtf8): boolean; override;
  public
    /// initialize the remote database connection
    // - you should not use this, but rather call OrmMapExternal()
    // - OrmProps.ExternalDatabase will map the associated TSqlDBConnectionProperties
    // - OrmProps.ExternalTableName will retrieve the real full table name,
    // e.g. including any databas<e schema prefix
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer); override;
    /// delete a row, calling the external engine with SQL
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: RawUtf8; FieldValue: Int64;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName, FieldValue: RawUtf8;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// overridden method for direct external database engine call
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// overridden method for direct external database engine call
    function TableHasRows(Table: TOrmClass): boolean; override;
    /// overridden method for direct external database engine call
    function MemberExists(Table: TOrmClass; ID: TID): boolean; override;
    /// begin a transaction (implements REST BEGIN Member)
    // - to be used to speed up some SQL statements like Insert/Update/Delete
    // - must be ended with Commit on success
    // - must be aborted with Rollback if any SQL statement failed
    // - return true if no transaction is active, false otherwise
    function TransactionBegin(aTable: TOrmClass;
      SessionID: cardinal = 1): boolean; override;
    /// end a transaction (implements REST END Member)
    // - write all pending SQL statements to the external database
    procedure Commit(SessionID: cardinal = 1; RaiseException: boolean = false); override;
    /// abort a transaction (implements REST ABORT Member)
    // - restore the previous state of the database, before the call to TransactionBegin
    procedure RollBack(SessionID: cardinal = 1); override;
     /// overridden method for direct external database engine call
    function UpdateBlobFields(Value: TOrm): boolean; override;
     /// overridden method for direct external database engine call
    function RetrieveBlobFields(Value: TOrm): boolean; override;
    /// update a field value of the external database
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; override;
    /// update a field value of the external database
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method will in fact call the SqlAddIndex method, if the index
    // is not already existing
    // - for databases which do not support indexes on BLOB fields (i.e. all
    // engine but SQLite3), such FieldNames will be ignored
    function CreateSqlMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8;
      Unique: boolean; IndexName: RawUtf8 = ''): boolean; override;
    /// this method is called by TRestServer.EndCurrentThread method just
    // before a thread is finished to ensure that the associated external DB
    // connection will be released for this thread
    // - this overridden implementation will clean thread-specific connections,
    // i.e. call TSqlDBConnectionPropertiesThreadSafe.EndCurrentThread method
    // - this method shall be called directly, nor from the main thread
    procedure EndCurrentThread(Sender: TThread); override;
    /// reset the internal cache of external table maximum ID
    // - next EngineAdd/BatchAdd will execute SELECT max(ID) FROM externaltable
    // - is a lighter alternative to EngineAddUseSelectMaxID=TRUE, since this
    // method may be used only once, when some records have been inserted into
    // the external database outside this class scope (e.g. by legacy code)
    procedure EngineAddForceSelectMaxID;
    /// compute the SQL query corresponding to a prepared request
    // - can be used internally e.g. for debugging purposes
    function ComputeSql(var Prepared: TOrmVirtualTablePrepared): RawUtf8;

    /// retrieve the REST server instance corresponding to an external TOrm
    // - just map aServer.GetVirtualStorage(aClass) and will return nil if not
    // a TRestStorageExternal
    // - you can use it e.g. to call MapField() method in a fluent interface
    class function Instance(aClass: TOrmClass;
      aServer: TRestOrmServer): TRestStorageExternal;
    /// retrieve the external database connection associated to a TOrm
    // - just map aServer.GetVirtualStorage(aClass) and will return nil if not
    // a TRestStorageExternal
    class function ConnectionProperties(aClass: TOrmClass;
      aServer: TRestOrmServer): TSqlDBConnectionProperties; overload;
    /// disable internal ID generation for INSERT
    // - by default, a new ID will be set (either with 'select max(ID)' or via
    // the OnEngineLockedNextID event)
    // - if the client supplies a forced ID within its JSON content, it would
    // be used for adding
    // - define this property to a non 0 value if no such ID is expected to be
    // supplied, but a fixed "fake ID" is returned by the Add() method; at
    // external DB level, no such ID field would be computed nor set at INSERT -
    // this feature may be useful when working with a legacy database - of
    // course any ID-based ORM method would probably fail to work
    property EngineAddForcedID: TID
      read fEngineAddForcedID write fEngineAddForcedID;
    /// define an alternate method of compute the ID for INSERT
    // - by default, a new ID will be with 'select max(ID)', and an internal
    // counter (unless EngineAddUseSelectMaxID is true)
    // - you can specify a custom callback, which may compute the ID as
    // expected (e.g. using a SQL sequence)
    property OnEngineAddComputeID: TOnEngineAddComputeID
      read fOnEngineAddComputeID write fOnEngineAddComputeID;
  published
    /// the associated external mormot.db.sql database connection properties
    property Properties: TSqlDBConnectionProperties
      read GetConnectionProperties;
    /// by default, any INSERT will compute the new ID from an internal variable
    // - it is very fast and reliable, unless external IDs can be created
    // outside this engine
    // - you can set EngineAddUseSelectMaxID=true to execute a slower
    // 'select max(ID) from TableName' SQL statement before each EngineAdd()
    // - a lighter alternative may be to call EngineAddForceSelectMaxID only
    // when required, i.e. when the external DB has just been modified
    // by a third-party/legacy SQL process
    property EngineAddUseSelectMaxID: boolean
      read fEngineAddUseSelectMaxID write fEngineAddUseSelectMaxID;
  end;



{ *********** TOrmVirtualTableExternal for External SQL Virtual Tables }

type
    /// A Virtual Table cursor for reading a TSqlDBStatement content
  // - this is the cursor class associated to TOrmVirtualTableExternal
  TOrmVirtualTableCursorExternal = class(TOrmVirtualTableCursor)
  protected
    fStatement: ISqlDBStatement;
    fSql: RawUtf8;
    fHasData: boolean;
    // on exception, release fStatement and optionally clear the pool
    procedure HandleClearPoolOnConnectionIssue;
  public
    /// finalize the external cursor by calling ReleaseRows
    destructor Destroy; override;
    /// called to begin a search in the virtual table, creating a SQL query
    // - the TOrmVirtualTablePrepared parameters were set by
    // TOrmVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSqlite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - all WHERE and ORDER BY clauses are able to be translated into a plain
    // SQL statement calling the external DB engine
    // - will create the internal fStatement from a SQL query, bind the
    // parameters, then execute it, ready to be accessed via HasData/Next
    function Search(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row
    // - if aColumn=VIRTUAL_TABLE_ROWID_COLUMN(-1), will return the row ID
    // as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSqlVar): boolean; override;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; override;
    /// read-only access to the SELECT statement
    property SQL: RawUtf8
      read fSql;
  end;

  /// mormot.db.sql-based virtual table for accessing any external database
  // - for ORM access, you should use the OrmMapExternal() function to
  // associate this virtual table module to any TOrm class
  // - transactions are handled by this module, according to the external database
  TOrmVirtualTableExternal = class(TOrmVirtualTable)
  public { overridden methods }
    /// returns the main specifications of the associated TOrmVirtualTableModule
    // - this is a read/write table, without transaction (yet), associated to the
    // TOrmVirtualTableCursorExternal cursor type, with 'External' as module name
    // and TRestStorageExternal as the related Static class
    // - no particular class is supplied here, since it will depend on the
    // associated Static TRestStorageExternal instance
    class procedure GetTableModuleProperties(
      var aProperties: TVirtualTableModuleProperties); override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TOrmVirtualTableCursor.Search()
    // - this overridden method will let the external DB engine perform the search,
    // using a standard SQL "SELECT * FROM .. WHERE .. ORDER BY .." statement
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck always set to true since double check is not necessary
    // - OmitOrderBy will be set to true since double sort is not necessary
    // - EstimatedCost/EstimatedRows will receive the estimated cost, with
    // lowest value if fStatic.fFieldsExternal[].ColumnIndexed is set
    // (i.e. if column has an index)
    function Prepare(var Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    function Insert(aRowID: Int64; var Values: TSqlVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    function Update(oldRowID, newRowID: Int64;
      var Values: TSqlVarDynArray): boolean; override;
  end;



{ *********** External SQL Database Engines Registration }

/// register on the Server-side an external database for an ORM class
// - will associate the supplied class with a TOrmVirtualTableExternal module
// (calling aModel.VirtualTableRegister method), even if the class does not
// inherit from TOrmVirtualTableAutoID (it can be any plain TOrm or
// TOrmMany sub-class for instance)
// - note that TOrmModel.Create() will reset all supplied classes to be defined
// as non virtual (i.e. Kind=ovkSQLite3)
// - this function shall be called BEFORE TRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSqlDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - the full table name, as expected by the external database, could be
// provided here (SqlTableName will be used internally as table name when
// called via the associated SQLite3 Virtual Table) - if no table name is
// specified (''), will use SqlTableName (e.g. 'Customer' for 'TOrmCustomer')
// - typical usage is therefore for instance:
// !  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
// !  Model := TOrmModel.Create([TOrmCustomer],'root');
// !  OrmMapExternal(Model,TOrmCustomer,Props,'Sales.Customer');
// !  Server := TRestServerDB.Create(aModel,'application.db'),true)
// - the supplied aExternalDB parameter is stored within aClass.OrmProps, so
// the instance must stay alive until all database access to this external table
// is finished (e.g. use a private/protected property)
// - aMappingOptions can be specified now, or customized later
// - server-side may omit a call to OrmMapExternal() if the need of
// an internal database is expected: it will allow custom database configuration
// at runtime, depending on the customer's expectations (or license)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
// - this method would allow to chain MapField() or MapAutoKeywordFields
// definitions, in a fluent interface, to refine the fields mapping
function OrmMapExternal(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSqlDBConnectionProperties; const aExternalTableName: RawUtf8 = '';
  aMappingOptions: TOrmMappingOptions = []): POrmMapping; overload;

/// register several tables of the model to be external
// - just a wrapper over the overloaded OrmMapExternal() method
function OrmMapExternal(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSqlDBConnectionProperties;
  aMappingOptions: TOrmMappingOptions = []): boolean; overload;

type
  /// all possible options for OrmMapExternalAll/TRestExternalDBCreate
  // - by default, TAuthUser and TAuthGroup tables will be handled via the
  // external DB, but you can avoid it for speed when handling session and security
  // by setting regDoNotRegisterUserGroupTables - it would also allow to encrypt
  // the SQLite3 instance and its authentication information for higher security
  // - you can set regMapAutoKeywordFields to ensure that the mapped field names
  // won't conflict with a SQL reserved keyword on the external database by
  // mapping a name with a trailing '_' character for the external column
  // - regClearPoolOnConnectionIssue will call ClearConnectionPool when a
  // connection-linked exception is discovered
  TOrmMapExternalOption = (
    regDoNotRegisterUserGroupTables,
    regMapAutoKeywordFields,
    regClearPoolOnConnectionIssue);

  /// set of options for OrmMapExternalAll/TRestExternalDBCreate functions
  TOrmMapExternalOptions = set of TOrmMapExternalOption;

/// register all tables of the model to be external, with some options
// - by default, all tables are handled by the SQLite3 engine, unless they
// are explicitly declared as external via OrmMapExternal: this
// function can be used to register all tables to be handled by an external DBs
// - this function shall be called BEFORE TRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSqlDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - by default, TAuthUser and TAuthGroup tables will be handled via the
// external DB, but you can avoid it for speed when handling session and security
// by setting regDoNotRegisterUserGroupTables in aExternalOptions
// - other aExternalOptions can be defined to tune the ORM process e.g. about
// mapping or connection loss detection
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function OrmMapExternalAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties;
  aExternalOptions: TOrmMapExternalOptions): boolean; overload;


/// create a new TRest instance, and possibly an external database, from its
// Model and stored values
// - if aDefinition.Kind matches a TRest registered class, one new instance
// of this kind will be created and returned
// - if aDefinition.Kind is a registered TSqlDBConnectionProperties class name,
// it will instantiate an in-memory TRestServerDB or a TRestServerFullMemory
// instance, then call OrmMapExternalAll() on this connection
// - will return nil if the supplied aDefinition does not match any registered
// TRest or TSqlDBConnectionProperties types
function TRestExternalDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalOptions: TOrmMapExternalOptions): TRest; overload;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TVirtualTableExternalRegisterOptions = TOrmMappingOptions;

function VirtualTableExternalRegister(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSqlDBConnectionProperties; const aExternalTableName: RawUtf8 = '';
  aMappingOptions: TVirtualTableExternalRegisterOptions = []): boolean; overload;
function VirtualTableExternalRegister(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSqlDBConnectionProperties;
  aMappingOptions: TVirtualTableExternalRegisterOptions = []): boolean; overload;
function VirtualTableExternalMap(aModel: TOrmModel;
  aClass: TOrmClass; aExternalDB: TSqlDBConnectionProperties;
  const aExternalTableName: RawUtf8 = '';
  aMapping: TVirtualTableExternalRegisterOptions = []): POrmMapping;
function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties;
  aExternalOptions: TOrmMapExternalOptions): boolean; overload;
function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties;
  DoNotRegisterUserGroupTables: boolean = false;
  ClearPoolOnConnectionIssue: boolean = false): boolean; overload;

{$endif PUREMORMOT2}


implementation

{ *********** TRestStorageExternal for ORM/REST Storage over SQL }


{ TRestStorageExternal }

procedure TRestStorageExternal.FieldsInternalInit;
var
  i, n, f: PtrInt;
begin
  n := length(fFieldsExternal);
  SetLength(fFieldsExternalToInternal, n);
  with fStoredClassMapping^ do
  begin
    SetLength(fFieldsInternalToExternal, length(ExtFieldNames) + 1);
    for i := 0 to high(fFieldsInternalToExternal) do
      fFieldsInternalToExternal[i] := -1;
    for i := 0 to n - 1 do
    begin
      f := ExternalToInternalIndex(fFieldsExternal[i].ColumnName);
      fFieldsExternalToInternal[i] := f;
      inc(f);
      if f >= 0 then
        // fFieldsInternalToExternal[0]=RowID, then follows fFieldsExternal[]
        fFieldsInternalToExternal[f] := i;
    end;
  end;
end;

function TRestStorageExternal.PropInfoToExternalField(Prop: TOrmPropInfo;
  var Column: TSqlDBColumnCreate): boolean;
begin
  case Prop.OrmFieldType of
    oftUnknown,
    oftMany:
      begin
        // ignore unknown/virtual fields
        result := false;
        exit;
      end;
    // ftUnknown identify 32-bit values, ftInt64=SqlDBFieldType for 64-bit
    oftEnumerate,
    oftBoolean:
      Column.DBType := ftUnknown;
  else
    // Prop may have identified e.g. T*ObjArray as ftUtf8
    Column.DBType := Prop.SqlDBFieldType;
  end;
  if Column.DBType = ftUtf8 then
    Column.Width := Prop.FieldWidth
  else
    Column.Width := 0;
  Column.Unique := aIsUnique in Prop.Attributes;
  Column.PrimaryKey := false;
  Column.Name := fStoredClassMapping^.ExtFieldNames[Prop.PropertyIndex];
  result := true;
end;

procedure TRestStorageExternal.LogFields(const log: ISynLog);
begin
  fProperties.GetFields(UnQuotedSQLSymbolName(fTableName), fFieldsExternal);
  log.Log(sllDebug, 'GetFields', TypeInfo(TSqlDBColumnDefineDynArray),
    fFieldsExternal, self);
end;

function TRestStorageExternal.FieldsExternalIndexOf(
  const ColName: RawUtf8; CaseSensitive: boolean): PtrInt;
begin
  if CaseSensitive then
  begin
    for result := 0 to high(fFieldsExternal) do
      if fFieldsExternal[result].ColumnName = ColName then
        exit;
  end
  else
    for result := 0 to high(fFieldsExternal) do
      if IdemPropNameU(fFieldsExternal[result].ColumnName, ColName) then
        exit;
  result := -1;
end;

procedure TRestStorageExternal.InitializeExternalDB(const log: ISynLog);
var
  s: RawUtf8;
  i, f: PtrInt;
  nfo: TOrmPropInfo;
  Field: TSqlDBColumnCreate;
  TableCreated, TableModified: boolean;
  CreateColumns: TSqlDBColumnCreateDynArray;
  options: TOrmMappingOptions;
begin
  // initialize external DB properties
  options := fStoredClassMapping^.options;
  fTableName := fStoredClassMapping^.TableName;
  fProperties :=
    fStoredClassMapping^.ConnectionProperties as TSqlDBConnectionProperties;
  log.Log(sllInfo, '% as % % Server=%',
    [StoredClass, fTableName, fProperties, Owner], self);
  if fProperties = nil then
    raise ERestStorage.CreateUtf8('%.Create: no external DB defined for %',
      [self, StoredClass]);
  // ensure external field names are compatible with the external DB keywords
  for f := 0 to StoredClassRecordProps.Fields.Count - 1 do
  begin
    nfo := StoredClassRecordProps.Fields.List[f];
    if nfo.OrmFieldType in COPIABLE_FIELDS then // ignore oftMany
    begin
      s := fStoredClassMapping^.ExtFieldNames[f];
      if rpmQuoteFieldName in options then
        fStoredClassMapping^.MapField(nfo.Name, '"' + s + '"')
      else if fProperties.IsSqlKeyword(s) then
      begin
        log.Log(sllWarning, '%.%: Field name % is not compatible with %',
          [fStoredClass, nfo.Name, s, fProperties.DbmsEngineName], self);
        if rpmAutoMapKeywordFields in options then
        begin
          log.Log(sllWarning, '-> %.% mapped to %_',
            [fStoredClass, nfo.Name, s], self);
          fStoredClassMapping^.MapField(nfo.Name, s + '_');
        end
        else
          log.Log(sllWarning, '-> you should better use MapAutoKeywordFields', self);
      end;
    end;
  end;
  // create corresponding external table if necessary, and retrieve its fields info
  TableCreated := false;
  LogFields(log);
  if not (rpmNoCreateMissingTable in options) then
    if fFieldsExternal = nil then
    begin
      // table is not yet existing -> try to create it
      with fStoredClass.OrmProps do
      begin
        SetLength(CreateColumns, Fields.Count + 1);
        CreateColumns[0].Name := fStoredClassMapping^.RowIDFieldName;
        CreateColumns[0].DBType := ftInt64;
        CreateColumns[0].Unique := true;
        CreateColumns[0].NonNullable := true;
        CreateColumns[0].PrimaryKey := true;
        f := 1;
        for i := 0 to Fields.Count - 1 do
          if PropInfoToExternalField(Fields.List[i], CreateColumns[f]) then
            inc(f);
        if f <> Length(CreateColumns) then
          // just ignore non handled field types
          SetLength(CreateColumns, f);
      end;
      s := fProperties.SqlCreate(fTableName, CreateColumns, false);
      if Assigned(fProperties.OnTableCreate) then
        TableCreated := fProperties.OnTableCreate(
          fProperties, fTableName, CreateColumns, s)
      else if s <> '' then
        TableCreated := ExecuteDirect(pointer(s), [], [], false) <> nil;
      if TableCreated then
      begin
        LogFields(log);
        if fFieldsExternal = nil then
          raise ERestStorage.CreateUtf8(
            '%.Create: external table creation % failed: GetFields() ' +
            'returned nil - sql=[%]', [self, StoredClass, fTableName, s]);
      end;
    end;
  FieldsInternalInit;
  // create any missing field if necessary
  if not (rpmNoCreateMissingField in options) then
    if not TableCreated then
    begin
      TableModified := false;
      with StoredClassRecordProps do
        for f := 0 to Fields.Count - 1 do
          if Fields.List[f].OrmFieldType in COPIABLE_FIELDS then // ignore oftMany
            /// real database columns exist for Simple + Blob fields (not Many)
            if FieldsExternalIndexOf(
                fStoredClassMapping^.ExtFieldNamesUnQuotedSql[f],
                rpmMissingFieldNameCaseSensitive in options) < 0 then
            begin
              // add new missing Field
              Finalize(Field);
              FillcharFast(Field, SizeOf(Field), 0);
              if PropInfoToExternalField(Fields.List[f], Field) then
              begin
                s := fProperties.SqlAddColumn(fTableName, Field);
                if Assigned(fProperties.OnTableAddColumn) then
                begin
                  if fProperties.OnTableAddColumn(
                      fProperties, fTableName, Field, s) then
                    TableModified := true; // don't raise ERestStorage from here
                end
                else if s <> '' then
                  if ExecuteDirect(pointer(s), [], [], false) <> nil then
                    TableModified := true
                  else
                    raise ERestStorage.CreateUtf8('%.Create: %: ' +
                      'unable to create external missing field %.% - sql=[%]',
                      [self, StoredClass, fTableName, Fields.List[f].Name, s]);
              end;
            end;
      if TableModified then
      begin
        // retrieve raw field information from DB after ALTER TABLE
        LogFields(log);
        FieldsInternalInit;
      end;
    end;
  // compute the sql statements used internally for external DB requests
  with fStoredClassMapping^ do
  begin
    s := SQL.TableSimpleFields[{withid=}false, {withtablename=}false];
    if s <> '' then // compute if not only blob
      FormatUtf8('select % from % where %=?', [s, fTableName, RowIDFieldName],
        fSelectOneDirectSQL); // don't return ID field
    FormatUtf8('select %,% from %', [sql.InsertSet, RowIDFieldName, fTableName],
      fSelectAllDirectSQL);
    fRetrieveBlobFieldsSQL := InternalCsvToExternalCsv(
      StoredClassRecordProps.SqlTableRetrieveBlobFields);
    fUpdateBlobFieldsSQL := InternalCsvToExternalCsv(
      StoredClassRecordProps.SqlTableUpdateBlobFields, '=?,', '=?');
  end;
  fSelectTableHasRowsSQL := FormatUtf8('select ID from % limit 1',
    [StoredClassRecordProps.SqlTableName]);
  AdaptSqlForEngineList(fSelectTableHasRowsSQL);
end;

constructor TRestStorageExternal.Create(aClass: TOrmClass; aServer: TRestOrmServer);
var
  log: ISynLog;
begin
  if aServer = nil then
    raise ERestStorage.CreateUtf8('%.Create(%): aServer=%', [self, aClass, aServer]);
  log := aServer.LogClass.Enter('Create %', [aClass], self);
  inherited Create(aClass, aServer);
  // initialize external DB process: setup ORM mapping, and create table/columns
  InitializeExternalDB(log);
end;

function TRestStorageExternal.AdaptSqlForEngineList(var SQL: RawUtf8): boolean;
var
  stmt: TSelectStatement;
  W: TJsonWriter;
  limit: TSqlDBDefinitionLimitClause;
  limitSQL, name: RawUtf8;
  f, n: PtrInt;
  temp: TTextWriterStackBuffer; // shared fTempBuffer is not protected now
begin
  result := false;
  if SQL = '' then
    exit;
  stmt := TSelectStatement.Create(SQL,
    fStoredClassRecordProps.Fields.IndexByName,
    fStoredClassRecordProps.SimpleFieldSelect);
  try
    if (stmt.SqlStatement = '') or // parsing failed
      not IdemPropNameU(stmt.TableName, fStoredClassRecordProps.SqlTableName) then
    begin
      {$ifdef DEBUGSQLVIRTUALTABLE}
      InternalLog('AdaptSqlForEngineList: complex statement -> switch to ' +
        'SQLite3 virtual engine - check efficiency', [], sllDebug);
      {$endif DEBUGSQLVIRTUALTABLE}
      exit;
    end;
    if stmt.Offset <> 0 then
    begin
      InternalLog('AdaptSqlForEngineList: unsupported OFFSET for [%]',
        [SQL], sllWarning);
      exit;
    end;
    if stmt.Limit = 0 then
      limit.Position := posNone
    else
    begin
      limit := fProperties.SqlLimitClause(stmt);
      if limit.Position = posNone then
      begin
        InternalLog('AdaptSqlForEngineList: unknown % LIMIT syntax for [%]',
          [ToText(fProperties.Dbms)^, SQL], sllWarning);
        exit;
      end;
      if limit.Position = posOuter then
        FormatUtf8(limit.InsertFmt, ['%', stmt.Limit], limitSQL)
      else
        FormatUtf8(limit.InsertFmt, [stmt.Limit], limitSQL);
    end;
    W := TJsonWriter.CreateOwnedStream(temp);
    try
      W.AddShorter('select ');
      if limit.Position = posSelect then
        W.AddString(limitSQL);
      for f := 0 to high(stmt.Select) do
        with stmt.Select[f] do
        begin
          if FunctionName <> '' then
          begin
            W.AddString(FunctionName);
            W.Add('(');
          end;
          if FunctionKnown = funcCountStar then
            W.Add('*')
          else
          begin
            W.AddString(fStoredClassMapping^.FieldNameByIndex(Field - 1));
            W.AddString(SubField);
          end;
          if FunctionName <> '' then
            W.Add(')');
          if ToBeAdded <> 0 then
          begin
            if ToBeAdded > 0 then
              W.Add('+');
            W.Add(ToBeAdded);
          end;
          if Alias <> '' then
          begin
            W.AddShorter(' as ');
            W.AddString(Alias);
          end
          else if not (Field in fStoredClassMapping^.FieldNamesMatchInternal) then
          begin
            if Field = 0 then
              name := ID_TXT
            else
              // RowID may be reserved (e.g. for Oracle)
              name := fStoredClassRecordProps.Fields.List[Field - 1].name;
            W.AddShorter(' as ');
            if (FunctionName = '') or
               (FunctionKnown in [funcDistinct, funcMax]) then
              W.AddString(name)
            else
            begin
              W.Add('"');
              W.AddString(FunctionName);
              W.Add('(');
              W.AddString(name);
              W.Add(')', '"');
            end;
          end;
          W.AddComma;
        end;
      W.CancelLastComma;
      W.AddShorter(' from ');
      W.AddString(fTableName);
      n := length(stmt.Where);
      if n = 0 then
      begin
        if limit.Position = posWhere then
        begin
          W.AddShorter(' where ');
          W.AddString(limitSQL);
        end;
      end
      else
      begin
        dec(n);
        W.AddShorter(' where ');
        if limit.Position = posWhere then
        begin
          W.AddString(limitSQL);
          W.AddShorter(' and ');
        end;
        for f := 0 to n do
          with stmt.Where[f] do
          begin
            if (FunctionName <> '') or
               (Operation > high(DB_SQLOPERATOR)) then
            begin
              InternalLog('AdaptSqlForEngineList: unsupported function %() for [%]',
                [FunctionName, SQL], sllWarning);
              exit;
            end;
            if f > 0 then
              if JoinedOR then
                W.AddShorter(' or ')
              else
                W.AddShorter(' and ');
            if NotClause then
              W.AddShorter('not ');
            if ParenthesisBefore <> '' then
              W.AddString(ParenthesisBefore);
            W.AddString(fStoredClassMapping^.FieldNameByIndex(Field - 1));
            W.AddString(SubField);
            W.AddString(DB_SQLOPERATOR[Operation]);
            if not (Operation in [opIsNull, opIsNotNull]) then
              W.AddNoJsonEscape(ValueSql, ValueSqlLen);
            if ParenthesisAfter <> '' then
              W.AddString(ParenthesisAfter);
          end;
      end;
      if stmt.GroupByField <> nil then
      begin
        W.AddShort(' group by ');
        for f := 0 to high(stmt.GroupByField) do
        begin
          W.AddString(fStoredClassMapping^.FieldNameByIndex(stmt.GroupByField[f] - 1));
          W.AddComma;
        end;
        W.CancelLastComma;
      end;
      if stmt.OrderByField <> nil then
      begin
        W.AddShort(' order by ');
        for f := 0 to high(stmt.OrderByField) do
        begin
          W.AddString(fStoredClassMapping^.FieldNameByIndex(stmt.OrderByField[f] - 1));
          if FieldBitGet(stmt.OrderByFieldDesc, f) then
            W.AddShorter(' desc');
          W.AddComma;
        end;
        W.CancelLastComma;
      end;
      if limit.Position = posAfter then
        W.AddString(limitSQL);
      W.SetText(SQL);
      if limit.Position = posOuter then
        SQL := FormatUtf8(limitSQL, [SQL]);
      result := true;
    finally
      W.Free;
    end;
  finally
    stmt.Free;
  end;
end;

function TRestStorageExternal.EngineLockedNextID: TID;

  procedure RetrieveFromDB;
  // fProperties.SqlCreate: ID Int64 PRIMARY KEY -> compute unique RowID
  // (not all DB engines handle autoincrement feature - e.g. Oracle does not)
  var
    rows: ISqlDBRows;
  begin
    fEngineLockedMaxID := 0;
    rows := ExecuteDirect('select max(%) from %',
      [fStoredClassMapping^.RowIDFieldName, fTableName], [], true);
    if rows = nil then
      exit;
    if rows.Step then
      fEngineLockedMaxID := rows.ColumnInt(0);
    rows.ReleaseRows;
  end;

var
  handled: boolean;
begin
  if fEngineAddForcedID <> 0 then
  begin
    result := fEngineAddForcedID;
    exit;
  end;
  if Assigned(fOnEngineAddComputeID) then
  begin
    result := fOnEngineAddComputeID(self, handled);
    if handled then
      exit;
  end;
  if (fEngineLockedMaxID = 0) or
     EngineAddUseSelectMaxID then
    RetrieveFromDB;
  inc(fEngineLockedMaxID);
  result := fEngineLockedMaxID;
end;

function TRestStorageExternal.InternalBatchStart(Encoding: TRestBatchEncoding;
  BatchOptions: TRestBatchOptions): boolean;
const
  BATCH: array[mPOST..mDELETE] of TSqlDBStatementCRUD = (
    cCreate, cUpdate, cDelete);
var
  Method: TUriMethod;
begin
  result := false; // means BATCH mode not supported
  Method := BATCH_METHOD[Encoding];
  if (self <> nil) and
     (Method in [mPOST..mDELETE]) and
     (BATCH[Method] in fProperties.BatchSendingAbilities) then
  begin
    StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ExtBatchStart' {$endif});
    // lock protected by try..finally in TRestServer.RunBatch caller
    try
      if fBatchMethod <> mNone then
        raise ERestStorage.CreateUtf8('Missing previous %.InternalBatchStop(%)',
          [self, StoredClass]);
      fBatchMethod := Method;
      fBatchOptions := BatchOptions;
      fBatchCount := 0;
      result := true; // means BATCH mode is supported
    finally
      if not result then
        StorageUnLock;
    end;
  end;
end;

procedure TRestStorageExternal.InternalBatchStop;
var
  i, j, n, max, BatchBegin, BatchEnd, ValuesMax: PtrInt;
  Query: ISqlDBStatement;
  ev: TOrmEvent;
  SQL: RawUtf8;
  P: PUtf8Char;
  Fields, ExternalFields: TRawUtf8DynArray;
  Types: TSqlDBFieldTypeArray;
  Values: TRawUtf8DynArrayDynArray;
  Occasion: TOrmOccasion;
  Decode: TJsonObjectDecoder;
  tmp: TSynTempBuffer;
begin
  if fBatchMethod = mNone then
    raise ERestStorage.CreateUtf8('%.InternalBatchStop(%).BatchMethod=mNone',
      [self, StoredClass]);
  try
    if fBatchCount = 0 then
      exit; // nothing to do
    if (Owner <> nil) and
       (fBatchMethod = mDelete) then
      // notify BEFORE deletion
      for i := 0 to fBatchCount - 1 do
        Owner.InternalUpdateEvent(
          oeDelete, fStoredClassProps.TableIndex, fBatchIDs[i], '', nil, nil);
    with fProperties do
      if BatchMaxSentAtOnce > 0 then
        max := BatchMaxSentAtOnce
      else
        max := 1000;
    BatchBegin := 0;
    BatchEnd := fBatchCount - 1;
    repeat
      case fBatchMethod of
        mPost,
        mPut:
          begin
            assert(fBatchIDs <> nil);
            BatchEnd := fBatchCount - 1;
            for i := BatchBegin to BatchEnd do
            begin
              tmp.Init(fBatchValues[i]); // Decode() modify the buffer in-place
              try
                P := tmp.buf;
                while P^ in [#1..' ', '{', '['] do
                  inc(P);
                if fBatchMethod = mPost then
                  Occasion := ooInsert
                else
                  Occasion := ooUpdate;
                case Occasion of
                  ooInsert:
                    // mPost=INSERT with the supplied fields and computed ID
                    Decode.DecodeInPlace(P, nil, pQuoted, fBatchIDs[i], true);
                  ooUpdate:
                    // mPut=UPDATE with the supplied fields and ID set appart
                    Decode.DecodeInPlace(P, nil, pQuoted, 0, true);
                end;
                if fStoredClassRecordProps.RecordVersionField <> nil then
                  RecordVersionFieldHandle(Occasion, Decode);
                if {%H-}Fields = nil then
                begin
                  Decode.AssignFieldNamesTo(Fields);
                  SQL := JsonDecodedPrepareToSql(Decode, ExternalFields, Types,
                    Occasion, fBatchOptions, {array=}true);
                  SetLength(Values, Decode.FieldCount);
                  ValuesMax := fBatchCount - BatchBegin;
                  if ValuesMax > max then
                    ValuesMax := max;
                  for j := 0 to Decode.FieldCount - 1 do
                    SetLength(Values[j], ValuesMax);
                end
                else if not Decode.SameFieldNames(Fields) then
                  // this item would break the SQL statement
                  break;
                n := i - BatchBegin;
                for j := 0 to high(Fields) do
                  Values[j, n] := Decode.FieldValues[j]; // regroup by parameter
                if Occasion = ooUpdate then
                  // ?=ID parameter
                  Values[length(Fields), n] := Int64ToUtf8(fBatchIDs[i]); // D2007 fails with var
                BatchEnd := i; // mark fBatchValues[i] has to be copied in Values[]
                if n + 1 >= max then
                  // do not send too much items at once, for better speed
                  break;
              finally
                tmp.Done;
              end;
            end;
          end;
        mDelete:
          begin
            if cPostgreBulkArray in fProperties.BatchSendingAbilities then
              // for mormot.db.sql.postgres array binding
              SQL := 'delete from % where %=ANY(?)'
            else
              // regular SQL
              SQL := 'delete from % where %=?';
            SQL := FormatUtf8(SQL, [fTableName, fStoredClassMapping^.RowIDFieldName]);
            n := BatchEnd - BatchBegin + 1;
            if n + 1 >= max then
            begin
              // do not send too much items at once, for better speed
              n := max;
              BatchEnd := BatchBegin + max - 1;
            end;
            SetLength(Values, 1);
            SetLength(Values[0], n);
            for i := 0 to n - 1 do
              Values[0, i] := Int64ToUtf8(fBatchIDs[BatchBegin + i]); // var fails on D2007
          end;
      end;
      n := BatchEnd - BatchBegin + 1;
      if n <= 0 then
        break;
      try
        if (fBatchMethod = mPost) and
           Assigned(fProperties.OnBatchInsert) then
          // use multiple insert dedicated function if available
          fProperties.OnBatchInsert(
            fProperties, fTableName, ExternalFields, Types, n, Values, fBatchOptions)
        else
        begin
          // use array binding
          Query := fProperties.NewThreadSafeStatementPrepared(
            SQL, {results=}false, {except=}true);
          case fBatchMethod of
            mPost,
            mPut:
              for i := 0 to high(Values) do
                Query.BindArray(i + 1, Types[i], Values[i], n);
            mDelete:
              Query.BindArray(1, ftInt64, Values[0], n);
          end;
          Query.ExecutePrepared;
          Query.ReleaseRows;
          Query := nil;
        end;
      except
        Query := nil;
        HandleClearPoolOnConnectionIssue;
        raise;
      end;
      if Owner <> nil then
      begin
        // add/update/delete should flush DB cache
        Owner.FlushInternalDBCache;
        // force deletion coherency
        if fBatchMethod = mDelete then
          for i := 0 to high(Values) do
            Owner.AfterDeleteForceCoherency(
              fStoredClassProps.TableIndex, GetInt64(pointer(Values[i])));
      end;
      Fields := nil; // force new sending block
      BatchBegin := BatchEnd + 1;
    until BatchBegin >= fBatchCount;
    if Owner <> nil then
    begin
      if fBatchMethod in [mPost, mPut] then
      begin
        if fBatchMethod = mPost then
          ev := oeAdd
        else
          ev := oeUpdate;
        if Owner.InternalUpdateEventNeeded(ev, fStoredClassProps.TableIndex) then
          for i := 0 to fBatchCount - 1 do
            Owner.InternalUpdateEvent(ev, fStoredClassProps.TableIndex,
              fBatchIDs[i], fBatchValues[i], nil, nil);
      end;
      Owner.FlushInternalDBCache;
    end;
  finally
    fBatchValues := nil;
    fBatchIDs := nil;
    fBatchCount := 0;
    fBatchCapacity := 0;
    fBatchMethod := mNone;
    fBatchOptions := [];
    StorageUnLock;
  end;
end;

procedure TRestStorageExternal.InternalBatchAppend(const aValue: RawUtf8;
  aID: TID);
begin
  if fBatchCount >= fBatchCapacity then
  begin
    if fBatchCapacity = 0 then
      fBatchCapacity := 64
    else
      fBatchCapacity := NextGrow(fBatchCapacity);
    SetLength(fBatchIDs, fBatchCapacity);
    if fBatchValues <> nil then
      SetLength(fBatchValues, fBatchCapacity);
  end;
  if aValue <> '' then
  begin
    if fBatchValues = nil then
      SetLength(fBatchValues, fBatchCapacity);
    fBatchValues[fBatchCount] := aValue;
  end;
  fBatchIDs[fBatchCount] := aID;
  inc(fBatchCount);
end;

function TRestStorageExternal.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
begin
  if (TableModelIndex < 0) or
     (fModel.Tables[TableModelIndex] <> fStoredClass) then
    result := 0 // avoid GPF
  else
    if fBatchMethod <> mNone then
      // BATCH process
      if fBatchMethod <> mPOST then
        result := 0
      else
      begin
        if not JsonGetID(pointer(SentData), result) then
          result := EngineLockedNextID
        else if result > fEngineLockedMaxID then
          fEngineLockedMaxID := result;
        InternalBatchAppend(SentData, result);
      end
    else
    begin
      // regular insert with EngineLockedNextID (UpdatedID=0)
      result := ExecuteFromJson(SentData, ooInsert, 0, fBatchOptions);
      if (result > 0) and
         (Owner <> nil) then
      begin
        if EngineAddForcedID = 0 then // only worth it if result is a true ID
          Owner.InternalUpdateEvent(
            oeAdd, TableModelIndex, result, SentData, nil, nil);
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TRestStorageExternal.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUtf8): boolean;
begin
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else if fBatchMethod <> mNone then
    // BATCH process
    if fBatchMethod <> mPUT then
      result := false
    else
    begin
      InternalBatchAppend(SentData, ID);
      result := true;
    end
  else
  begin
    // regular update
    result := ExecuteFromJson(SentData, ooUpdate, ID, fBatchOptions) = ID;
    if result and
       (Owner <> nil) then
    begin
      Owner.InternalUpdateEvent(
        oeUpdate, TableModelIndex, ID, SentData, nil, nil);
      Owner.FlushInternalDBCache;
    end;
  end;
end;

function TRestStorageExternal.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else if fBatchMethod <> mNone then
    // BATCH process
    if fBatchMethod <> mDELETE then
      result := false
    else
    begin
      InternalBatchAppend('', ID);
      result := true;
    end
  else
  begin
    // regular deletion
    if Owner <> nil then // notify BEFORE deletion
      Owner.InternalUpdateEvent(oeDelete, TableModelIndex, ID, '', nil, nil);
    result := ExecuteDirect('delete from % where %=?',
      [fTableName, fStoredClassMapping^.RowIDFieldName], [ID], false) <> nil;
    if result and
       (Owner <> nil) then
      Owner.FlushInternalDBCache;
  end;
end;

function TRestStorageExternal.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
const
  CHUNK_SIZE = 200;
var
  i, n, chunk, pos: PtrInt;
  rowid: RawUtf8;
begin
  result := false;
  if (IDs = nil) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  n := length(IDs);
  if fBatchMethod <> mNone then
    // BATCH process
    if fBatchMethod <> mDELETE then
      exit
    else
      for i := 0 to n - 1 do
        InternalBatchAppend('', IDs[i])
  else
  begin
    // regular deletion
    if Owner <> nil then // notify BEFORE deletion
      for i := 0 to n - 1 do
        Owner.InternalUpdateEvent(oeDelete, TableModelIndex, IDs[i], '', nil, nil);
    rowid := fStoredClassMapping^.RowIDFieldName;
    pos := 0;
    repeat
      // delete by chunks using primary key
      chunk := n - pos;
      if chunk = 0 then
        break;
      if chunk > CHUNK_SIZE then
        chunk := CHUNK_SIZE;
      if ExecuteInlined('delete from % where % in (%)',
          [fTableName, rowid, Int64DynArrayToCsv(pointer(@IDs[pos]), chunk)],
          false) = nil then
        exit;
      inc(pos, chunk);
    until false;
    if Owner <> nil then
      Owner.FlushInternalDBCache;
  end;
  result := true;
end;

function TRestStorageExternal.EngineList(const SQL: RawUtf8;
  ForceAjax: boolean; ReturnedRowCount: PPtrInt): RawUtf8;
var
  stmt: ISqlDBStatement;
begin
  result := '';
  if ReturnedRowCount <> nil then
    raise ERestStorage.CreateUtf8('%.EngineList(ReturnedRowCount<>nil) for %',
      [self, StoredClass]);
  stmt := PrepareInlinedForRows(SQL);
  if stmt <> nil then
  try
    stmt.ExecutePreparedAndFetchAllAsJson(
      ForceAjax or
      (Owner = nil) or
      not Owner.Owner.NoAjaxJson, result);
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  stmt: ISqlDBStatement;
  w: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  // TableModelIndex is not useful here
  result := '';
  if (self = nil) or
     (ID <= 0) then
    exit;
  if fSelectOneDirectSQL <> '' then // has some simple fields to retrieve
    try
      stmt := fProperties.NewThreadSafeStatementPrepared(
        fSelectOneDirectSQL, {results=}true, {except=}true);
      if stmt = nil then
        exit;
      stmt.Bind(1, ID);
      stmt.ExecutePrepared;
      w := AcquireJsonWriter(tmp);
      try
        stmt.StepToJson(w, {seekfirst=}false);
        w.SetText(result);
        // we don't return "ID": because caller will set it
      finally
        ReleaseJsonWriter(w);
      end;
    except
      stmt := nil;
      HandleClearPoolOnConnectionIssue;
    end
  else if MemberExists(fStoredClass, ID) then
    FormatUtf8('{"ID":%}', [ID], result); // only blob: returns something
end;

function TRestStorageExternal.EngineExecute(const aSql: RawUtf8): boolean;
begin
  if aSql = '' then
    result := false
  else
    result := ExecuteInlined(aSql, false) <> nil;
end;

function TRestStorageExternal.TableHasRows(Table: TOrmClass): boolean;
var
  rows: ISqlDBRows;
begin
  if (self = nil) or
     (Table <> fStoredClass) then
    result := false
  else
  begin
    rows := ExecuteDirect(pointer(fSelectTableHasRowsSQL), [], [], true);
    if rows = nil then
      result := false
    else
    begin
      result := rows.Step;
      rows.ReleaseRows;
    end;
  end;
end;

function TRestStorageExternal.TableRowCount(Table: TOrmClass): Int64;
var
  rows: ISqlDBRows;
begin
  result := 0;
  if (self = nil) or
     (Table <> fStoredClass) then
    exit;
  rows := ExecuteDirect('select count(*) from %', [fTableName], [], true);
  if rows = nil then
    exit;
  if rows.Step then
    result := rows.ColumnInt(0);
  rows.ReleaseRows;
end;

function TRestStorageExternal.MemberExists(Table: TOrmClass; ID: TID): boolean;
var
  rows: ISqlDBRows;
begin
  result := false;
  if (self = nil) or
     (Table <> fStoredClass) then
    exit;
  with fStoredClassMapping^ do
    rows := ExecuteDirect('select % from % where %=?',
      [RowIDFieldName, fTableName, RowIDFieldName], [ID], true);
  if rows = nil then
    exit;
  if rows.Step then
  try
    result := rows.ColumnInt(0) = ID;
    rows.ReleaseRows;
  except
    rows := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  rows: ISqlDBRows;
begin
  result := false;
  if (aID <= 0) or
     not BlobField^.IsRawBlob or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  with fStoredClassMapping^ do
    rows := ExecuteDirect('select % from % where %=?',
      [InternalToExternal(BlobField), fTableName, RowIDFieldName],
      [aID], {results=}true);
  if (rows <> nil) and
     rows.Step then
  try
    BlobData := rows.ColumnBlob(0);
    rows.ReleaseRows;
    result := true; // success
  except
    rows := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.RetrieveBlobFields(Value: TOrm): boolean;
var
  rows: ISqlDBRows;
  f: PtrInt;
  data: TSqlVar;
  temp: RawByteString;
begin
  result := false;
  if (Value <> nil) and
     (Value.ID > 0) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.Orm do
      if BlobFields <> nil then
      begin
        rows := ExecuteDirect('select % from % where %=?',
          [fRetrieveBlobFieldsSQL, fTableName,
           fStoredClassMapping^.RowIDFieldName], [Value.ID], true);
        if (rows <> nil) and
           rows.Step then
        try
          for f := 0 to High(BlobFields) do
          begin
            rows.ColumnToSqlVar(f, data, temp);
            BlobFields[f].SetFieldSqlVar(Value, data);
          end;
          rows.ReleaseRows;
          result := true; // success
        except
          rows := nil;
          HandleClearPoolOnConnectionIssue;
        end;
      end;
end;

function TRestStorageExternal.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  rows: ISqlDBRows;
  ExtWhereFieldName, json: RawUtf8;
begin
  if (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else
    with fStoredClassMapping^ do
    begin
      ExtWhereFieldName := InternalToExternal(WhereFieldName);
      result := ExecuteInlined('update % set %=:(%): where %=:(%):',
        [fTableName, InternalToExternal(SetFieldName), SetValue,
         ExtWhereFieldName, WhereValue], false) <> nil;
      if result and
         (Owner <> nil) then
      begin
        if Owner.InternalUpdateEventNeeded(oeUpdate, TableModelIndex) then
        begin
          rows := ExecuteInlined('select % from % where %=:(%):',
            [RowIDFieldName, fTableName, ExtWhereFieldName, WhereValue], true);
          if rows = nil then
            exit;
          JsonEncodeNameSQLValue(SetFieldName, SetValue, json);
          while rows.Step do
            Owner.InternalUpdateEvent(
              oeUpdate, TableModelIndex, rows.ColumnInt(0), json, nil, nil);
          rows.ReleaseRows;
        end;
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TRestStorageExternal.EngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUtf8;
  Increment: Int64): boolean;
var
  extField: RawUtf8;
  Value: Int64;
begin
  result := false;
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  if (Owner <> nil) and
     Owner.InternalUpdateEventNeeded(oeUpdate, TableModelIndex) then
    result :=
      OneFieldValue(fStoredClass, FieldName, 'ID=?', [], [ID], Value) and
      UpdateField(fStoredClass, ID, FieldName, [Value + Increment])
  else
  try
    with fStoredClassMapping^ do
    begin
      extField := InternalToExternal(FieldName);
      result := ExecuteInlined('update % set %=%+:(%): where %=:(%):',
        [fTableName, extField, extField, Increment, RowIDFieldName, ID],
        false) <> nil;
    end;
    if result and
       (Owner <> nil) then
      Owner.FlushInternalDBCache;
  except
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  stmt: ISqlDBStatement;
  modfields: TFieldBits;
begin
  result := false;
  if (aID <= 0) or
     not BlobField^.IsRawBlob or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  try
    if Owner <> nil then
      Owner.FlushInternalDBCache;
    with fStoredClassMapping^ do
      stmt := fProperties.NewThreadSafeStatementPrepared(
        'update % set %=? where %=?',
        [fTableName, InternalToExternal(BlobField), RowIDFieldName],
        {results=}false, {except=}true);
    if stmt <> nil then
    begin
      if BlobData = '' then
        stmt.BindNull(1)
      else
        stmt.BindBlob(1, BlobData); // fast explicit BindBlob() call
      stmt.Bind(2, aID);
      stmt.ExecutePrepared;
      if Owner <> nil then
      begin
        fStoredClassRecordProps.FieldBitsFromBlobField(BlobField, modfields);
        Owner.InternalUpdateEvent(
          oeUpdateBlob, TableModelIndex, aID, '', @modfields, nil);
        Owner.FlushInternalDBCache;
      end;
      result := true; // success
    end;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue; // leave result=false to notify error
  end;
end;

function TRestStorageExternal.UpdateBlobFields(Value: TOrm): boolean;
var
  f: PtrInt;
  aID: TID;
  temp: array of RawByteString;
  Params: TSqlVarDynArray;
begin
  result := false;
  if (Value <> nil) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.Orm do
      if BlobFields <> nil then
      begin
        aID := Value.ID;
        if aID <= 0 then
          exit;
        if Owner <> nil then
          Owner.FlushInternalDBCache;
        SetLength(Params, length(BlobFields));
        SetLength(temp, length(BlobFields));
        for f := 0 to high(Params) do
          BlobFields[f].GetFieldSqlVar(Value, Params[f], temp[f]);
        result := ExecuteDirectSqlVar('update % set % where %=?',
          [fTableName, fUpdateBlobFieldsSQL, fStoredClassMapping^.RowIDFieldName],
          Params, aID, false);
        if result and
           (Owner <> nil) then
        begin
          Owner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
            aID, '', @fStoredClassRecordProps.FieldBits[oftBlob], nil);
          Owner.FlushInternalDBCache;
        end;
      end
      else
        result := true; // as TRest.UpdateblobFields()
end;

function TRestStorageExternal.PrepareInlinedForRows(
  const aSql: RawUtf8): ISqlDBStatement;
var
  stmt: ISqlDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  try
    stmt := fProperties.PrepareInlined(aSql, true);
    if (stmt <> nil) and
       (oftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.ExecuteInlined(const aSql: RawUtf8;
  ExpectResults: boolean): ISqlDBRows;
var
  stmt: ISqlDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  if not ExpectResults and
     (Owner <> nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    stmt := fProperties.PrepareInlined(aSql, ExpectResults);
    if stmt = nil then
      exit;
    if ExpectResults and
       (oftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    stmt.ExecutePrepared;
    if not ExpectResults then
      stmt.ReleaseRows;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue; // leave result=nil to notify error
  end;
end;

function TRestStorageExternal.ExecuteInlined(SqlFormat: PUtf8Char;
  const Args: array of const; ExpectResults: boolean): ISqlDBRows;
begin
  result := ExecuteInlined(FormatUtf8(SqlFormat, Args), ExpectResults);
end;

function TRestStorageExternal.PrepareDirectForRows(SqlFormat: PUtf8Char;
  const Args, Params: array of const): ISqlDBStatement;
var
  stmt: ISqlDBStatement;
begin
  result := nil;
  if self <> nil then
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(
      SqlFormat, Args, {results=}true, {except=}true);
    if stmt = nil then
      exit;
    stmt.Bind(Params);
    if oftDateTimeMS in fStoredClassRecordProps.HasTypeFields then
      stmt.ForceDateWithMS := true;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.ExecuteDirect(SqlFormat: PUtf8Char;
  const Args, Params: array of const; ExpectResults: boolean): ISqlDBRows;
var
  stmt: ISqlDBStatement;
begin
  result := nil;
  if self = nil then
    exit;
  if not ExpectResults and
     (Owner <> nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(
      SqlFormat, Args, ExpectResults, {except=}true);
    stmt.Bind(Params);
    if ExpectResults and
       (oftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    stmt.ExecutePrepared;
    if IdemPChar(SqlFormat, 'DROP TABLE ') then
      fEngineLockedMaxID := 0;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue; // leave result=nil to notify error
  end;
end;

function TRestStorageExternal.ExecuteDirectSqlVar(SqlFormat: PUtf8Char;
  const Args: array of const; var Params: TSqlVarDynArray;
  const LastIntegerParam: Int64; ParamsMatchCopiableFields: boolean): boolean;
var
  stmt: ISqlDBStatement;
  ParamsCount, f: PtrInt;
begin
  result := false;
  if Self <> nil then
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(SqlFormat, Args,
      {results=}false, {except=}true);
    if stmt = nil then
      exit;
    ParamsCount := length(Params);
    if ParamsMatchCopiableFields and
       (ParamsCount <> Length(fStoredClassRecordProps.CopiableFields)) then
      raise ERestStorage.CreateUtf8(
        '%.ExecuteDirectSqlVar(ParamsMatchCopiableFields) for %',
        [self, StoredClass]);
    for f := 0 to ParamsCount - 1 do
      if ParamsMatchCopiableFields and
         (fStoredClassRecordProps.CopiableFields[f].
           OrmFieldTypeStored in [oftDateTime, oftDateTimeMS]) and
         (Params[f].VType = ftUtf8) then
        stmt.BindDateTime(f + 1, Iso8601ToDateTimePUtf8Char(Params[f].VText))
      else
        stmt.Bind(f + 1, Params[f]);
    if LastIntegerParam <> 0 then
      stmt.Bind(ParamsCount + 1, LastIntegerParam);
    stmt.ExecutePrepared;
    result := true;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue; // leave result=false to notify error
  end;
end;

function TRestStorageExternal.EngineSearchField(const FieldName: ShortString;
  const FieldValue: array of const; out ResultID: TIDDynArray): boolean;
var
  n: integer;
  rows: ISqlDBRows;
begin
  result := false;
  try
    n := 0;
    rows := ExecuteDirect('select % from % where %=?',
      [fStoredClassMapping^.RowIDFieldName, fTableName, FieldName],
      FieldValue, true);
    if rows <> nil then
    begin
      while rows.Step do
        AddInt64(TInt64DynArray(ResultID), n, rows.ColumnInt(0));
      rows.ReleaseRows;
    end;
    SetLength(ResultID, n);
    result := n > 0;
  except
    rows := nil;
    HandleClearPoolOnConnectionIssue; // leave result=false to notify error
  end;
end;

function TRestStorageExternal.SearchField(const FieldName: RawUtf8;
  FieldValue: Int64; out ResultID: TIDDynArray): boolean;
begin
  result := EngineSearchField(FieldName, [FieldValue], ResultID);
end;

function TRestStorageExternal.SearchField(const FieldName, FieldValue: RawUtf8;
  out ResultID: TIDDynArray): boolean;
begin
  result := EngineSearchField(FieldName, [FieldValue], ResultID);
end;

function TRestStorageExternal.TransactionBegin(aTable: TOrmClass;
  SessionID: cardinal): boolean;
begin
  if (aTable = fStoredClass) and
     inherited TransactionBegin(aTable, SessionID) then
    result := fProperties.SharedTransaction(SessionID, transBegin) <> nil
  else
    result := false;
end;

procedure TRestStorageExternal.Commit(SessionID: cardinal;
  RaiseException: boolean);
const
  ACTION: array[boolean] of TSqlDBSharedTransactionAction = (
    transCommitWithoutException, transCommitWithException);
begin
  inherited Commit(SessionID, RaiseException);
  // reset fTransactionActive + write all TOrmVirtualTableJson
  fProperties.SharedTransaction(SessionID, ACTION[RaiseException]);
end;

procedure TRestStorageExternal.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset fTransactionActive
  fProperties.SharedTransaction(SessionID, transRollback);
end;

function TRestStorageExternal.CreateSqlMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean;
  IndexName: RawUtf8): boolean;
var
  SQL: RawUtf8;
  ExtFieldNames: TRawUtf8DynArray;
  IntFieldIndex: TIntegerDynArray;
  NeedDesc: boolean;
  i, n, extfield: PtrInt;
begin
  result := false;
  NeedDesc := false;
  n := length(FieldNames);
  if (self = nil) or
     (fProperties = nil) or
     (Table <> fStoredClass) or
     (n <= 0) then
    exit;
  fStoredClassMapping^.InternalToExternalDynArray(
    FieldNames, ExtFieldNames, @IntFieldIndex);
  if n = 1 then
  begin
    // handle case of index over a single column
    if (IntFieldIndex[0] < 0) and // ID/RowID?
      fProperties.IsPrimaryKeyIndexed(NeedDesc) then
    begin
      // most DB create an implicit index on primary key
      result := true;
      exit;
    end;
    if not NeedDesc then
    begin
      // we identify just if indexed, not the order
      extfield := fFieldsInternalToExternal[IntFieldIndex[0] + 1];
      if (extfield >= 0) and
         (fFieldsExternal[extfield].ColumnIndexed) then
      begin
        result := true; // column already indexed
        exit;
      end;
    end;
  end;
  if not (fProperties.Dbms in DB_HANDLEINDEXONBLOBS) then
    // BLOB fields cannot be indexed (only in SQLite3+PostgreSQL)
    for i := 0 to n - 1 do
    begin
      extfield := fFieldsInternalToExternal[IntFieldIndex[i] + 1];
      if (extfield >= 0) and
         (fFieldsExternal[extfield].ColumnType in [ftBlob, ftUtf8]) and
         (fFieldsExternal[extfield].ColumnLength <= 0) then
      begin
        if i = 0 then
          exit; // impossible to create an index with no field!
        SetLength(ExtFieldNames, i); // truncate to the last indexable field
        break;
      end;
    end;
  SQL := fProperties.SqlAddIndex(
    fTableName, ExtFieldNames, Unique, NeedDesc, IndexName);
  if Assigned(fProperties.OnTableCreateMultiIndex) then
    result := fProperties.OnTableCreateMultiIndex(
      fProperties, fTableName, FieldNames, Unique, IndexName, SQL)
  else if SQL <> '' then
    result := ExecuteDirect(pointer(SQL), [], [], false) <> nil;
  if not result then
    exit;
  extfield := fFieldsInternalToExternal[IntFieldIndex[0] + 1];
  if extfield >= 0 then
    // mark first column as indexed by now
    fFieldsExternal[extfield].ColumnIndexed := true;
end;

class function TRestStorageExternal.Instance(aClass: TOrmClass;
  aServer: TRestOrmServer): TRestStorageExternal;
begin
  if (aClass = nil) or
     (aServer = nil) then
    result := nil
  else
  begin
    result := TRestStorageExternal(aServer.GetVirtualStorage(aClass));
    if result <> nil then
      if not result.InheritsFrom(TRestStorageExternal) then
        result := nil;
  end;
end;

class function TRestStorageExternal.ConnectionProperties(aClass: TOrmClass;
  aServer: TRestOrmServer): TSqlDBConnectionProperties;
begin
  result := Instance(aClass, aServer).GetConnectionProperties;
end;

function TRestStorageExternal.GetConnectionProperties: TSqlDBConnectionProperties;
begin
  if self = nil then
    result := nil
  else
    result := fProperties;
end;

function TRestStorageExternal.HandleClearPoolOnConnectionIssue: boolean;
var
  conn: TSqlDBConnection;
begin
  result := false;
  if (self <> nil) and
     (fStoredClassMapping <> nil) and
     (rpmClearPoolOnConnectionIssue in fStoredClassMapping.Options) then
  begin
    conn := fProperties.ThreadSafeConnection;
    if conn.LastErrorWasAboutConnection then
    begin
      InternalLog(
        'HandleClearPoolOnConnectionIssue: ClearConnectionPool after %',
        [conn.LastErrorException], sllDB);
      fProperties.ClearConnectionPool;
      result := true;
    end;
  end;
end;

function TRestStorageExternal.ExecuteFromJson(const SentData: RawUtf8;
  Occasion: TOrmOccasion; UpdatedID: TID; BatchOptions: TRestBatchOptions): TID;
var
  Decoder: TJsonObjectDecoder;
  SQL: RawUtf8;
  Types: TSqlDBFieldTypeArray;
  ExternalFields: TRawUtf8DynArray;
  InsertedID: TID;
  F: PtrInt;
  stmt: ISqlDBStatement;
  tmp: TSynTempBuffer;
begin
  result := 0;
  StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ExtExecuteFromJson' {$endif});
  // lock to avoid race condition against max(ID)
  try
    case Occasion of
      ooInsert:
        if not JsonGetID(pointer(SentData), InsertedID) then
          // no specified "ID":... field value -> compute next
          InsertedID := EngineLockedNextID
        else if InsertedID > fEngineLockedMaxID then
          fEngineLockedMaxID := InsertedID;
      ooUpdate:
        if UpdatedID <> 0 then
          InsertedID := 0
        else
          raise ERestStorage.CreateUtf8(
            '%.ExecuteFromJson(%,soUpdate,UpdatedID=%)',
            [self, StoredClass, UpdatedID]);
    else
      raise ERestStorage.CreateUtf8('%.ExecuteFromJson(%,Occasion=%)?',
        [self, StoredClass, ToText(Occasion)^]);
    end;
    // decode fields
    tmp.Init(SentData);
    try
      if (fEngineAddForcedID <> 0) and
         (InsertedID = fEngineAddForcedID) then
        Decoder.Decode(tmp, nil, pNonQuoted, 0, true)
      else
        Decoder.Decode(tmp, nil, pNonQuoted, InsertedID, true);
      if (Decoder.FieldCount = 0) and
         (Occasion = ooUpdate) then
      begin
        // SentData='' -> no column to update
        result := UpdatedID;
        exit;
      end;
      if fStoredClassRecordProps.RecordVersionField <> nil then
        RecordVersionFieldHandle(Occasion, Decoder);
      // compute SQL statement and associated bound parameters
      SQL := JsonDecodedPrepareToSql(
        Decoder, ExternalFields, Types, Occasion, BatchOptions, {array=}false);
      if Occasion = ooUpdate then
        // Int64ToUtf8(var) fails on D2007
        Decoder.FieldValues[Decoder.FieldCount - 1] := Int64ToUtf8(UpdatedID);
      // execute statement
      try
        stmt := fProperties.NewThreadSafeStatementPrepared(
          SQL, {results=}false, {except=}true);
        if stmt = nil then
          exit;
        for F := 0 to Decoder.FieldCount - 1 do
          if Decoder.FieldTypeApproximation[F] = ftaNull then
            stmt.BindNull(F + 1)
          else
            stmt.Bind(F + 1, Types[F], Decoder.FieldValues[F], true);
        stmt.ExecutePrepared;
      except
        stmt := nil;
        HandleClearPoolOnConnectionIssue;
        exit; // leave result=0
      end;
    finally
      tmp.Done;
    end;
    // mark success
    if UpdatedID = 0 then
      result := InsertedID
    else
      result := UpdatedID;
  finally
    StorageUnLock;
  end;
end;

procedure TRestStorageExternal.EndCurrentThread(Sender: TThread);
begin
  if fProperties.InheritsFrom(TSqlDBConnectionPropertiesThreadSafe) then
    TSqlDBConnectionPropertiesThreadSafe(fProperties).EndCurrentThread;
end;

function TRestStorageExternal.InternalFieldNameToFieldExternalIndex(
  const InternalFieldName: RawUtf8): integer;
begin
  result := fStoredClassRecordProps.Fields.IndexByNameOrExcept(InternalFieldName);
  result := fFieldsInternalToExternal[result + 1];
end;

function TRestStorageExternal.JsonDecodedPrepareToSql(
  var Decoder: TJsonObjectDecoder; out ExternalFields: TRawUtf8DynArray;
  out Types: TSqlDBFieldTypeArray; Occasion: TOrmOccasion;
  BatchOptions: TRestBatchOptions; BoundArray: boolean): RawUtf8;
var
  f, k: PtrInt;
begin
  SetLength(ExternalFields, Decoder.FieldCount);
  for f := 0 to Decoder.FieldCount - 1 do
  begin
    if IsRowID(Decoder.FieldNames[f], Decoder.FieldNamesL[f]) then
      k := VIRTUAL_TABLE_ROWID_COLUMN
    else
    begin
      k := fStoredClassRecordProps.Fields.IndexByName(Decoder.FieldNames[f]);
      if k < 0 then
        k := -2;
    end;
    if k >= VIRTUAL_TABLE_ROWID_COLUMN then
    begin
      ExternalFields[f] := fStoredClassMapping^.FieldNameByIndex(k);
      k := fFieldsInternalToExternal[k + 1];
    end;
    if k < 0 then
      raise ERestStorage.CreateUtf8(
        '%.JsonDecodedPrepareToSql(%): No column for [%] field in table %',
        [self, StoredClass, Decoder.FieldNames[f], fTableName]);
    Types[f] := fFieldsExternal[k].ColumnType;
  end;
  // compute SQL statement and associated bound parameters
  Decoder.DecodedFieldNames := pointer(ExternalFields);
  if BoundArray and
     (cPostgreBulkArray in fProperties.BatchSendingAbilities) then
    // efficient mormot.db.sql.postgres array binding
    // e.g. via 'insert into ... values (unnest...)'
    Decoder.DecodedFieldTypesToUnnest := @Types;
  result := EncodeAsSqlPrepared(Decoder, fTableName, Occasion,
    fStoredClassMapping^.RowIDFieldName, BatchOptions, fProperties.Dbms);
  if Occasion = ooUpdate then
    if Decoder.FieldCount = MAX_SQLFIELDS then
      raise ERestStorage.CreateUtf8(
        'Too many fields for %.JsonDecodedPrepareToSql', [self])
    else
    begin
      // add "where ID=?" parameter
      Types[Decoder.FieldCount] := ftInt64;
      inc(Decoder.FieldCount);
    end;
end;

procedure TRestStorageExternal.EngineAddForceSelectMaxID;
begin
  StorageLock(true {$ifdef DEBUGSTORAGELOCK}, 'ExtAddForceSelectMaxID' {$endif});
  fEngineLockedMaxID := 0;
  StorageUnLock;
end;

const
  SQL_OPER_WITH_PARAM: array[soEqualTo..soGreaterThanOrEqualTo] of string[3] = (
    '=?',      // soEqualTo
    '<>?',     // soNotEqualTo
    '<?',      // soLessThan
    '<=?',     // soLessThanOrEqualTo
    '>?',      // soGreaterThan
    '>=?');    // soGreaterThanOrEqualTo

function TRestStorageExternal.ComputeSql(
  var Prepared: TOrmVirtualTablePrepared): RawUtf8;
var
  WR: TJsonWriter;
  i: PtrInt;
  where: POrmVirtualTablePreparedConstraint;
  order: ^TOrmVirtualTablePreparedOrderBy;
  {$ifdef DEBUGSQLVIRTUALTABLE}
  log: RawUtf8;
  {$endif DEBUGSQLVIRTUALTABLE}
  temp: TTextWriterStackBuffer; // shared fTempBuffer is not protected now
begin
  if (Prepared.WhereCount = 0) and
     (Prepared.OrderByCount = 0) then
  begin
    result := fSelectAllDirectSQL; // if Prepared is not supported -> full scan
    exit;
  end;
  result := '';
  WR := TJsonWriter.CreateOwnedStream(temp);
  try
    WR.AddString(fSelectAllDirectSQL);
    where := @Prepared.Where;
    for i := 0 to Prepared.WhereCount - 1 do
    begin
      {$ifdef DEBUGSQLVIRTUALTABLE}
      log := FormatUtf8('% [column=% oper=% omitcheck=%]',
        [log, where^.Column, ToText(where^.Operation)^, where^.OmitCheck]);
      {$endif DEBUGSQLVIRTUALTABLE}
      if where^.Operation > high(SQL_OPER_WITH_PARAM) then
      begin
        {$ifdef DEBUGSQLVIRTUALTABLE}
        log := log + ':UNSUPPORTED';
        {$endif DEBUGSQLVIRTUALTABLE}
        fRest.InternalLog('ComputeSql: unsupported % on column %',
          [ToText(where^.Operation)^, where^.Column], sllWarning);
        where^.OmitCheck := false; // unsupported operator -> manual search
        continue;
      end;
      if i = 0 then
        WR.AddShorter(' where ')
      else
        WR.AddShorter(' and ');
      if fStoredClassMapping^.AppendFieldName(where^.Column, WR) then
      begin
        // invalid "where" column index -> abort search and return ''
        fRest.InternalLog('ComputeSql: unknown where % % ? column',
          [where^.Column, SQL_OPER_WITH_PARAM[where^.Operation]], sllWarning);
        exit;
      end;
      WR.AddShorter(SQL_OPER_WITH_PARAM[where^.Operation]);
      inc(where);
    end;
    // e.g. 'select FirstName,..,ID from PeopleExternal where FirstName=? and LastName=?'
    order := @Prepared.OrderBy;
    for i := 0 to Prepared.OrderByCount - 1 do
    begin
      {$ifdef DEBUGSQLVIRTUALTABLE}
      log := FormatUtf8('% [column=% desc=%]', [log, order^.Column, order^.Desc]);
      {$endif DEBUGSQLVIRTUALTABLE}
      if i = 0 then
        WR.AddShort(' order by ')
      else
        WR.Add(',', ' ');
      if fStoredClassMapping^.AppendFieldName(order^.Column, WR) then
      begin
        // invalid "order" column index -> abort search and return ''
        fRest.InternalLog('ComputeSql: unknown order % collumn',
          [order^.Column], sllWarning);
        exit;
      end;
      if order^.Desc then
        WR.AddShorter(' desc');
      inc(order);
    end;
    WR.SetText(result);
  finally
    WR.Free;
    {$ifdef DEBUGSQLVIRTUALTABLE}
    fRest.InternalLog('ComputeSql [%] [omitorder=% cost=% rows=%]%',
      [result, Prepared.OmitOrderBy, Prepared.EstimatedCost,
       Prepared.EstimatedRows, log], sllDB);
    {$endif DEBUGSQLVIRTUALTABLE}
  end;
end;



{ *********** TOrmVirtualTableExternal for External SQL Virtual Tables }

{ TOrmVirtualTableCursorExternal }

procedure TOrmVirtualTableCursorExternal.HandleClearPoolOnConnectionIssue;
begin
  fStatement := nil;
  fHasData := false;
  if (self <> nil) and
     (Table <> nil) and
     (Table.Static <> nil) then
    (Table.Static as TRestStorageExternal).HandleClearPoolOnConnectionIssue;
end;

destructor TOrmVirtualTableCursorExternal.Destroy;
begin
  // TSynLog.Add.Log(sllCustom2, 'Destroy', self);
  if fStatement <> nil then
    fStatement.ReleaseRows; // if not already done in HasData
  inherited Destroy;
end;

function TOrmVirtualTableCursorExternal.Column(aColumn: integer;
  var aResult: TSqlVar): boolean;
var
  n: cardinal;
begin
  result := false;
  if (self <> nil) and
     (fStatement <> nil) then
  try
    n := fStatement.ColumnCount - 1;
    if aColumn = VIRTUAL_TABLE_ROWID_COLUMN then
      // RowID is latest column (select %,RowID from..)
      aColumn := n
    else if cardinal(aColumn) >= n then
      // error if aColumn is out of range
      exit;
    fStatement.ColumnToSqlVar(aColumn, aResult, fColumnTemp);
    result := aResult.VType <> ftUnknown;
  except
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TOrmVirtualTableCursorExternal.HasData: boolean;
begin
  if (self = nil) or
     (fStatement = nil) then
    result := false
  else if fHasData then
    result := true
  else
  begin
    if fStatement <> nil then
    begin
      fStatement.ReleaseRows;
      fStatement := nil; // need to release statement ASAP for proper reuse
    end;
    result := false;
  end;
end;

function TOrmVirtualTableCursorExternal.Next: boolean;
begin
  result := false;
  if (self <> nil) and
     (fStatement <> nil) then
    try
      fHasData := fStatement.Step;
      if not fHasData then
      begin
        fStatement.ReleaseRows;
        fStatement := nil; // need to release statement ASAP for proper reuse
      end;
      result := true; // success (may be with no more data)
    except
      HandleClearPoolOnConnectionIssue;
    end;
end;

function TOrmVirtualTableCursorExternal.Search(
  var Prepared: TOrmVirtualTablePrepared): boolean;
var
  i: PtrInt;
  storage: TRestStorageExternal;
begin
  result := false;
  if (Self <> nil) and
     (Table <> nil) and
     (Table.Static <> nil) then
  begin
    storage := Table.Static as TRestStorageExternal;
    {$ifndef DEBUGSQLVIRTUALTABLE}
    if fSql = '' then
    {$endif DEBUGSQLVIRTUALTABLE}
      fSql := storage.ComputeSql(Prepared);
    try
      fStatement := storage.fProperties.NewThreadSafeStatementPrepared(
        fSql, {results=}true, {except=}true);
      if fStatement <> nil then
      begin
        if oftDateTimeMS in storage.fStoredClassRecordProps.HasTypeFields then
          fStatement.ForceDateWithMS := true;
        for i := 1 to Prepared.WhereCount do
          fStatement.Bind(i, Prepared.Where[i - 1].Value);
        fStatement.ExecutePrepared;
        result := Next; // on execution success, go to the first row
      end;
      storage.LogFamily.SynLog.Log(sllSQL, 'Search %', [fSql], self);
    except
      self.HandleClearPoolOnConnectionIssue;
    end;
  end;
end;


{ TOrmVirtualTableExternal }

function TOrmVirtualTableExternal.Drop: boolean;
begin
  if (self = nil) or
     (Static = nil) then
    result := false
  else
    with Static as TRestStorageExternal do
      result := ExecuteDirect('drop table %', [fTableName], [], false) <> nil;
end;

class procedure TOrmVirtualTableExternal.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite];
  aProperties.CursorClass := TOrmVirtualTableCursorExternal;
  aProperties.StaticClass := TRestStorageExternal;
end;

function TOrmVirtualTableExternal.Prepare(
  var Prepared: TOrmVirtualTablePrepared): boolean;
var
  i, col: PtrInt;
  Fields: TOrmPropInfoList;
begin
  result := inherited Prepare(Prepared); // set costFullScan or costPrimaryIndex
  if result and
     (Static <> nil) then
    with Static as TRestStorageExternal do
    begin
      // mark Where[] clauses will be handled by SQL
      Fields := StoredClassRecordProps.Fields;
      result := false;
      for i := 0 to Prepared.WhereCount - 1 do
        with Prepared.Where[i] do
          if (Column <> VIRTUAL_TABLE_IGNORE_COLUMN) and
             (Operation <= high(SQL_OPER_WITH_PARAM)) then
          begin
            if Column = VIRTUAL_TABLE_ROWID_COLUMN then
              // is an indexed primary key
              Prepared.EstimatedCost := costPrimaryIndex
            else
            begin
              if cardinal(Column) >= cardinal(Fields.Count) then
                // invalid column index -> abort query
                exit;
              col := fFieldsInternalToExternal[Column + 1];
              if col < 0 then
                // column not known in the external database -> abort query
                exit;
              if fFieldsExternal[col].ColumnIndexed then
              begin
                if Prepared.EstimatedCost < costSecondaryIndex then
                  Prepared.EstimatedCost := costSecondaryIndex;
              end
              else if Prepared.EstimatedCost < costScanWhere then
                Prepared.EstimatedCost := costScanWhere;
            end;
            OmitCheck := true;     // search handled via SQL query
            Value.VType := ftNull; // caller vt_BestIndex() expects <> ftUnknown
          end;
      // check the OrderBy[] clauses
      if Prepared.OrderByCount > 0 then
      begin
        for i := 0 to Prepared.OrderByCount - 1 do
          with Prepared.OrderBy[i] do
            if (Column <> VIRTUAL_TABLE_ROWID_COLUMN) and
               (cardinal(Column) >= cardinal(Fields.Count)) then
              // invalid column index -> abort query
              exit;
        Prepared.OmitOrderBy := true; // order handled via SQL query
      end;
      result := true; // success
    end;
end;

// here below, virtual write operations do not call Engine*() but direct SQL
// -> InternalUpdateEvent() were already called by MainEngine*() methods

function TOrmVirtualTableExternal.Delete(aRowID: Int64): boolean;
begin
  if (self <> nil) and
     (Static <> nil) and
     (aRowID > 0) then
    with Static as TRestStorageExternal do
      result := ExecuteDirect('delete from % where %=?',
        [fTableName, fStoredClassMapping^.RowIDFieldName],
        [aRowID], false) <> nil
  else
    result := false;
end;

function TOrmVirtualTableExternal.Insert(aRowID: Int64;
  var Values: TSqlVarDynArray; out insertedRowID: Int64): boolean;
begin
  // aRowID is just ignored here since IDs are always auto calculated
  result := false;
  if (self <> nil) and
     (Static <> nil) then
    with Static as TRestStorageExternal do
    begin
      StorageLock(false {$ifdef DEBUGSTORAGELOCK}, 'ExtInsert' {$endif});
      // lock to avoid race condition against max(RowID)
      try
        insertedRowID := EngineLockedNextID;
        with fStoredClassMapping^ do
          result := ExecuteDirectSqlVar('insert into % (%,%) values (%,?)',
            [fTableName, Sql.InsertSet, RowIDFieldName,
             CsvOfValue('?', length(Values))],
            Values, insertedRowID, true);
      finally
        StorageUnLock;
      end;
    end;
end;

function TOrmVirtualTableExternal.Update(oldRowID, newRowID: Int64;
  var Values: TSqlVarDynArray): boolean;
begin
  if (self <> nil) and
     (Static <> nil) and
     (oldRowID = newRowID) and
     (newRowID > 0) then // don't allow ID change
    with Static as TRestStorageExternal, fStoredClassMapping^ do
      result := ExecuteDirectSqlVar('update % set % where %=?',
        [fTableName, SQL.UpdateSetAll, RowIDFieldName],
        Values, oldRowID, true)
  else
    result := false;
end;


{ *********** External SQL Database Engines Registration }

function OrmMapExternal(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSqlDBConnectionProperties; const aExternalTableName: RawUtf8;
  aMappingOptions: TOrmMappingOptions): POrmMapping;
var
  ExternalTableName: RawUtf8;
  Props: TOrmModelProperties;
begin
  result := nil;
  if (aModel = nil) or
     (aClass = nil) or
     (aExternalDB = nil) then
    // avoid GPF
    exit;
  Props := aModel.Props[aClass];
  if Props = nil then
    // if aClass is not part of the model
    exit;
  Props.Kind := ovkCustomAutoID; // force creation use of SQLite3 virtual table
  if aExternalTableName = '' then
    ExternalTableName := Props.Props.SqlTableName
  else
    ExternalTableName := aExternalTableName;
  result := aModel.VirtualTableRegister(aClass, TOrmVirtualTableExternal,
    aExternalDB.SqlFullTableName(ExternalTableName), aExternalDB, aMappingOptions);
end;

function OrmMapExternal(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSqlDBConnectionProperties;
  aMappingOptions: TOrmMappingOptions): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to high(aClass) do
    if OrmMapExternal(aModel, aClass[i], aExternalDB, '', aMappingOptions) = nil then
      result := false;
end;

function OrmMapExternalAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties;
  aExternalOptions: TOrmMapExternalOptions): boolean;
var
  i: PtrInt;
  rec: TOrmClass;
  opt: TOrmMappingOptions;
begin
  result := (aModel <> nil) and
            (aExternalDB <> nil);
  if not result then
    exit; // avoid GPF
  opt := [];
  if regClearPoolOnConnectionIssue in aExternalOptions then
    include(opt, rpmClearPoolOnConnectionIssue);
  for i := 0 to high(aModel.Tables) do
  begin
    rec := aModel.Tables[i];
    if (regDoNotRegisterUserGroupTables in aExternalOptions) and
       (rec.InheritsFrom (TAuthGroup) or
        rec.InheritsFrom(TAuthUser)) then
      continue
    else if OrmMapExternal(aModel, rec, aExternalDB, '', opt) = nil then
      result := false
    else if regMapAutoKeywordFields in aExternalOptions then
      aModel.TableProps[i].ExternalDB.MapAutoKeywordFields;
  end;
end;

function TRestExternalDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalOptions: TOrmMapExternalOptions): TRest;
var
  propsClass: TSqlDBConnectionPropertiesClass;
  props: TSqlDBConnectionProperties;
begin
  result := nil;
  if aDefinition = nil then
    exit;
  propsClass := TSqlDBConnectionProperties.ClassFrom(aDefinition);
  if propsClass <> nil then
  begin
    props := nil;
    try
      // aDefinition.Kind was a TSqlDBConnectionProperties -> all external DB
      props := propsClass.Create(aDefinition.ServerName,
        aDefinition.DatabaseName, aDefinition.User, aDefinition.PassWordPlain);
      OrmMapExternalAll(aModel, props, aExternalOptions);
      // instantiate either a SQLite3 :memory: DB or a TRestServerFullMemory
      result := CreateInMemoryServer(
        aModel, aHandleAuthentication);
    except
      FreeAndNilSafe(result);
      props.Free;  // avoid memory leak
    end;
  end
  else
    // not external DB -> try if aDefinition.Kind is a TRest class
    result := TRest.CreateTryFrom(aModel, aDefinition, aHandleAuthentication);
end;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

function VirtualTableExternalRegister(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSqlDBConnectionProperties; const aExternalTableName: RawUtf8;
  aMappingOptions: TVirtualTableExternalRegisterOptions): boolean;
begin
  result := OrmMapExternal(aModel, aClass, aExternalDB, aExternalTableName,
    aMappingOptions) <> nil;
end;

function VirtualTableExternalRegister(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSqlDBConnectionProperties;
  aMappingOptions: TVirtualTableExternalRegisterOptions): boolean;
begin
  result := OrmMapExternal(aModel, aClass, aExternalDB, aMappingOptions);
end;

function VirtualTableExternalMap(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSqlDBConnectionProperties; const aExternalTableName: RawUtf8;
  aMapping: TVirtualTableExternalRegisterOptions): POrmMapping;
begin
  result := OrmMapExternal(aModel, aClass, aExternalDB, aExternalTableName, aMapping);
end;

function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties;
  aExternalOptions: TOrmMapExternalOptions): boolean;
begin
  result := OrmMapExternalAll(aModel, aExternalDB, aExternalOptions);
end;

function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSqlDBConnectionProperties; DoNotRegisterUserGroupTables: boolean;
  ClearPoolOnConnectionIssue: boolean): boolean;
var
  opt: TOrmMapExternalOptions;
begin
  opt := []; // to call the overloaded function below with proper options
  if DoNotRegisterUserGroupTables then
    include(opt, regDoNotRegisterUserGroupTables);
  if ClearPoolOnConnectionIssue then
    include(opt, regClearPoolOnConnectionIssue);
  result := OrmMapExternalAll(aModel, aExternalDB, opt);
end;

{$endif PUREMORMOT2}


end.

