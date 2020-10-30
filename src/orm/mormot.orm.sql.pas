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
  mormot.core.secure,
  mormot.core.log,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.db.core,
  mormot.db.sql,
  mormot.rest.core,
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

  /// REST server with direct access to a SynDB-based external database
  // - handle all REST commands, using the external SQL database connection,
  // and prepared statements
  // - is used by TRestServer.URI for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TOrmVirtualTableExternal[Cursor] classes
  TRestStorageExternal = class(TRestStorage)
  protected
    /// values retrieved from fStoredClassProps.ExternalDB settings
    fTableName: RawUTF8;
    fProperties: TSQLDBConnectionProperties;
    fSelectOneDirectSQL, fSelectAllDirectSQL, fSelectTableHasRowsSQL: RawUTF8;
    fRetrieveBlobFieldsSQL, fUpdateBlobfieldsSQL: RawUTF8;
    // ID handling during Add/Insert
    fEngineAddUseSelectMaxID: boolean;
    fEngineLockedMaxID: TID;
    fOnEngineAddComputeID: TOnEngineAddComputeID;
    fEngineAddForcedID: TID;
    /// external column layout as retrieved by fProperties
    // - used internaly to guess e.g. if the column is indexed
    // - fFieldsExternal[] contains the external table info, and the internal
    // column name is available via fFieldsExternalToInternal[]
    fFieldsExternal: TSQLDBColumnDefineDynArray;
    /// gives the index of each fFieldsExternal[] item in Props.Fields[]
    // - is >=0 for index in Props.Fields[], -1 for RowID/ID, -2 if unknown
    // - use InternalFieldNameToFieldExternalIndex() to convert from column name
    fFieldsExternalToInternal: TIntegerDynArray;
    /// gives the index of each in Props.Fields[]+1 in fFieldsExternal[]
    // - expects [0] of RowID/ID, [1..length(fFieldNames)] for others
    fFieldsInternalToExternal: TIntegerDynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TURIMethod;
    fBatchCapacity, fBatchCount: integer;
    // BATCH sending uses TEXT storage for direct sending to database driver
    fBatchValues: TRawUTF8DynArray;
    fBatchIDs: TIDDynArray;
    /// get fFieldsExternal[] index using fFieldsExternalToInternal[] mapping
    // - do handle ID/RowID fields and published methods
    function InternalFieldNameToFieldExternalIndex(
      const InternalFieldName: RawUTF8): integer;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function PrepareInlinedForRows(const aSQL: RawUTF8): ISQLDBStatement;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function PrepareDirectForRows(SQLFormat: PUTF8Char;
      const Args, Params: array of const): ISQLDBStatement;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function ExecuteInlined(const aSQL: RawUTF8;
      ExpectResults: boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and inlined parameters
    function ExecuteInlined(SQLFormat: PUTF8Char; const Args: array of const;
      ExpectResults: boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirect(SQLFormat: PUTF8Char; const Args, Params: array of const;
      ExpectResults: boolean): ISQLDBRows;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirectSQLVar(SQLFormat: PUTF8Char; const Args: array of const;
       var Params: TSQLVarDynArray; const LastIntegerParam: Int64;
       ParamsMatchCopiableFields: boolean): boolean;
    /// run INSERT of UPDATE from the corresponding JSON object
    // - Occasion parameter shall be only either soInsert or soUpate
    // - each JSON field will be bound with the proper SQL type corresponding to
    // the real external table columns (e.g. as TEXT for variant)
    // - returns 0 on error, or the Updated/Inserted ID
    function ExecuteFromJSON(const SentData: RawUTF8; Occasion: TOrmOccasion;
      UpdatedID: TID): TID;
    /// compute the INSERT or UPDATE statement as decoded from a JSON object
    function JSONDecodedPrepareToSQL(var Decoder: TJSONObjectDecoder;
      out ExternalFields: TRawUTF8DynArray; out Types: TSQLDBFieldTypeArray;
      Occasion: TOrmOccasion; BatchOptions: TRestBatchOptions;
      BoundArray: boolean): RawUTF8;
    function GetConnectionProperties: TSQLDBConnectionProperties;
    /// check rpmClearPoolOnConnectionIssue in fStoredClassMapping.Options
    function HandleClearPoolOnConnectionIssue: boolean;
  public
    // overridden methods calling the external engine with SQL via Execute
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    function EngineLockedNextID: TID; virtual;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID; const
       SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; out ResultID: TIDDynArray): boolean;
    // overridden method returning TRUE for next calls to EngineAdd/Update/Delete
    // will properly handle operations until InternalBatchStop is called
    // BatchOptions is ignored with external DB (syntax are too much specific)
    function InternalBatchStart(Method: TURIMethod;
      BatchOptions: TRestBatchOptions): boolean; override;
    // internal method called by TRestServer.RunBatch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    procedure InternalBatchStop; override;
    /// called internally by EngineAdd/EngineUpdate/EngineDelete in batch mode
    procedure InternalBatchAdd(const aValue: RawUTF8; const aID: TID);
    /// TRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - change 'RowID' into 'ID' column name, internal field names into
    // mapped external field names ('AS [InternalFieldName]' if needed), and
    // SQLTableName into fTableName
    // - any 'LIMIT #' clause will be changed into the appropriate SQL statement
    // - handle also statements to avoid slow virtual table full scan, e.g.
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
  public
    /// initialize the remote database connection
    // - you should not use this, but rather call VirtualTableExternalRegister()
    // - RecordProps.ExternalDatabase will map the associated TSQLDBConnectionProperties
    // - RecordProps.ExternalTableName will retrieve the real full table name,
    // e.g. including any databas<e schema prefix
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer); override;
    /// delete a row, calling the external engine with SQL
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: RawUTF8; FieldValue: Int64;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName, FieldValue: RawUTF8;
      out ResultID: TIDDynArray): boolean; overload; override;
    /// overridden method for direct external database engine call
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// overridden method for direct external database engine call
    function TableHasRows(Table: TOrmClass): boolean; override;
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
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// update a field value of the external database
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method will in fact call the SQLAddIndex method, if the index
    // is not already existing
    // - for databases which do not support indexes on BLOB fields (i.e. all
    // engine but SQLite3), such FieldNames will be ignored
    function CreateSQLMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8 = ''): boolean; override;
    /// this method is called by TRestServer.EndCurrentThread method just
    // before a thread is finished to ensure that the associated external DB
    // connection will be released for this thread
    // - this overridden implementation will clean thread-specific connections,
    // i.e. call TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method
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
    function ComputeSQL(const Prepared: TOrmVirtualTablePrepared): RawUTF8;

    /// retrieve the REST server instance corresponding to an external TOrm
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TRestStorageExternal
    // - you can use it e.g. to call MapField() method in a fluent interface
    class function Instance(aClass: TOrmClass;
      aServer: TRestORMServer): TRestStorageExternal;
    /// retrieve the external database connection associated to a TOrm
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TRestStorageExternal
    class function ConnectionProperties(aClass: TOrmClass;
      aServer: TRestORMServer): TSQLDBConnectionProperties; overload;
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
    property OnEngineAddComputeID: TOnEngineAddComputeID read
      fOnEngineAddComputeID write fOnEngineAddComputeID;
  published
    /// the associated external SynDB database connection properties
    property Properties: TSQLDBConnectionProperties
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
    /// A Virtual Table cursor for reading a TSQLDBStatement content
  // - this is the cursor class associated to TOrmVirtualTableExternal
  TOrmVirtualTableCursorExternal = class(TOrmVirtualTableCursor)
  protected
    fStatement: ISQLDBStatement;
    fSQL: RawUTF8;
    fHasData: boolean;
    // on exception, release fStatement and optionally clear the pool
    procedure HandleClearPoolOnConnectionIssue;
  public
    /// finalize the external cursor by calling ReleaseRows
    destructor Destroy; override;
    /// called to begin a search in the virtual table, creating a SQL query
    // - the TOrmVirtualTablePrepared parameters were set by
    // TOrmVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - all WHERE and ORDER BY clauses are able to be translated into a plain
    // SQL statement calling the external DB engine
    // - will create the internal fStatement from a SQL query, bind the
    // parameters, then execute it, ready to be accessed via HasData/Next
    function Search(const Prepared: TOrmVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row
    // - if aColumn=VIRTUAL_TABLE_ROWID_COLUMN(-1), will return the row ID
    // as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; override;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; override;
    /// read-only access to the SELECT statement
    property SQL: RawUTF8 read fSQL;
  end;

  /// A SynDB-based virtual table for accessing any external database
  // - for ORM access, you should use VirtualTableExternalRegister method to
  //   associate this virtual table module to any TOrm class
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
    function Insert(aRowID: Int64; var Values: TSQLVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    function Update(oldRowID, newRowID: Int64;
      var Values: TSQLVarDynArray): boolean; override;
  end;



{ *********** External SQL Database Engines Registration }

/// register on the Server-side an external database for an ORM class
// - will associate the supplied class with a TOrmVirtualTableExternal module
// (calling aModel.VirtualTableRegister method), even if the class does not
// inherit from TOrmVirtualTableAutoID (it can be any plain TOrm or
// TOrmMany sub-class for instance)
// - note that TOrmModel.Create() will reset all supplied classes to be defined
// as non virtual (i.e. Kind=rSQLite3)
// - this function shall be called BEFORE TRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - the full table name, as expected by the external database, could be
// provided here (SQLTableName will be used internaly as table name when
// called via the associated SQLite3 Virtual Table) - if no table name is
// specified (''), will use SQLTableName (e.g. 'Customer' for 'TOrmCustomer')
// - typical usage is therefore for instance:
// !  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
// !  Model := TOrmModel.Create([TOrmCustomer],'root');
// !  VirtualTableExternalRegister(Model,TOrmCustomer,Props,'Sales.Customer');
// !  Server := TRestServerDB.Create(aModel,'application.db'),true)
// - the supplied aExternalDB parameter is stored within aClass.RecordProps, so
// the instance must stay alive until all database access to this external table
// is finished (e.g. use a private/protected property)
// - aMappingOptions can be specified now, or customized later
// - server-side may omit a call to VirtualTableExternalRegister() if the need of
// an internal database is expected: it will allow custom database configuration
// at runtime, depending on the customer's expectations (or license)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegister(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8 = '';
  aMappingOptions: TOrmPropertiesMappingOptions = []): boolean; overload;

/// register several tables of the model to be external
// - just a wrapper over the overloaded VirtualTableExternalRegister() method
function VirtualTableExternalRegister(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSQLDBConnectionProperties;
  aMappingOptions: TOrmPropertiesMappingOptions = []): boolean; overload;

/// register one table of the model to be external, with optional mapping
// - this method would allow to chain MapField() or MapAutoKeywordFields
// definitions, in a fluent interface:
function VirtualTableExternalMap(aModel: TOrmModel;
  aClass: TOrmClass; aExternalDB: TSQLDBConnectionProperties;
  const aExternalTableName: RawUTF8 = '';
  aMapping: TOrmPropertiesMappingOptions = []): POrmPropertiesMapping;

type
  /// all possible options for VirtualTableExternalRegisterAll/TRestExternalDBCreate
  // - by default, TAuthUser and TAuthGroup tables will be handled via the
  // external DB, but you can avoid it for speed when handling session and security
  // by setting regDoNotRegisterUserGroupTables
  // - you can set regMapAutoKeywordFields to ensure that the mapped field names
  // won't conflict with a SQL reserved keyword on the external database by
  // mapping a name with a trailing '_' character for the external column
  // - regClearPoolOnConnectionIssue will call ClearConnectionPool when a
  // connection-linked exception is discovered
  TVirtualTableExternalRegisterOption = (
    regDoNotRegisterUserGroupTables,
    regMapAutoKeywordFields,
    regClearPoolOnConnectionIssue);

  /// set of options for VirtualTableExternalRegisterAll/TRestExternalDBCreate functions
  TVirtualTableExternalRegisterOptions = set of TVirtualTableExternalRegisterOption;

/// register all tables of the model to be external, with some options
// - by default, all tables are handled by the SQLite3 engine, unless they
// are explicitely declared as external via VirtualTableExternalRegister: this
// function can be used to register all tables to be handled by an external DBs
// - this function shall be called BEFORE TRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - by default, TAuthUser and TAuthGroup tables will be handled via the
// external DB, but you can avoid it for speed when handling session and security
// by setting regDoNotRegisterUserGroupTables in aExternalOptions
// - other aExternalOptions can be defined to tune the ORM process e.g. about
// mapping or connection loss detection
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSQLDBConnectionProperties;
  aExternalOptions: TVirtualTableExternalRegisterOptions): boolean; overload;

/// register all tables of the model to be external
// - mainly for retro-compatibility with existing code
// - just a wrapper around the VirtualTableExternalRegisterAll() overloaded
// function with some boolean flags instead of TVirtualTableExternalRegisterOptions
function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSQLDBConnectionProperties;
  DoNotRegisterUserGroupTables: boolean = false;
  ClearPoolOnConnectionIssue: boolean = false): boolean; overload;


/// create a new TRest instance, and possibly an external database, from its
// Model and stored values
// - if aDefinition.Kind matches a TRest registered class, one new instance
// of this kind will be created and returned
// - if aDefinition.Kind is a registered TSQLDBConnectionProperties class name,
// it will instantiate an in-memory TRestServerDB or a TRestServerFullMemory
// instance, then call VirtualTableExternalRegisterAll() on this connection
// - will return nil if the supplied aDefinition does not match any registered
// TRest or TSQLDBConnectionProperties types
function TRestExternalDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalOptions: TVirtualTableExternalRegisterOptions): TRest; overload;


implementation

{ *********** TRestStorageExternal for ORM/REST Storage over SQL }


{ TRestStorageExternal }

const
  ORM_TO_SQLDB: array[TOrmFieldType] of TSQLDBFieldType =
    // ftUnknown is used for Int32 values, ftInt64 for Int64 values
    (ftUnknown,   // oftUnknown
     ftUTF8,      // oftAnsiText
     ftUTF8,      // oftUTF8Text
     ftUnknown,   // oftEnumerate
     ftInt64,     // oftSet
     ftInt64,     // oftInteger
     ftInt64,     // oftID
     ftInt64,     // oftRecord
     ftUnknown,   // oftBoolean
     ftDouble,    // oftFloat
     ftDate,      // oftDateTime
     ftInt64,     // oftTimeLog
     ftCurrency,  // oftCurrency
     ftUTF8,      // oftObject
     ftUTF8,      // oftVariant
     ftUTF8,      // oftNullable (retrieved from Prop.OrmFieldTypeStored)
     ftBlob,      // oftBlob
     ftBlob,      // oftBlobDynArray
     ftBlob,      // oftBlobCustom
     ftUTF8,      // oftUTF8Comp
     ftInt64,     // oftMany
     ftInt64,     // oftModTime
     ftInt64,     // oftCreateTime
     ftInt64,     // oftTID
     ftInt64,     // oftRecordVersion
     ftInt64,     // oftSessionUserID
     ftDate,      // oftDateTimeMS
     ftInt64,     // oftUnixTime
     ftInt64);    // oftUnixMSTime

constructor TRestStorageExternal.Create(aClass: TOrmClass; aServer: TRestOrmServer);

  procedure FieldsInternalInit;
  var
    i, n, int: PtrInt;
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
        int := ExternalToInternalIndex(fFieldsExternal[i].ColumnName);
        fFieldsExternalToInternal[i] := int;
        inc(int);
        if int >= 0 then
          // fFieldsInternalToExternal[0]=RowID, then follows fFieldsExternal[]
          fFieldsInternalToExternal[int] := i;
      end;
    end;
  end;

  function PropInfoToExternalField(Prop: TOrmPropInfo;
    var Column: TSQLDBColumnCreate): boolean;
  begin
    if Prop.ORMFieldType in [oftUnknown, oftMany] then
    begin
      // ignore unknown/virtual fields
      result := false;
      exit;
    end;
    Column.DBType := ORM_TO_SQLDB[Prop.OrmFieldTypeStored];
    Column.Name := fStoredClassMapping^.ExtFieldNames[Prop.PropertyIndex];
    if Column.DBType = ftUTF8 then
      Column.Width := Prop.FieldWidth
    else
      Column.Width := 0;
    Column.Unique := aIsUnique in Prop.Attributes;
    Column.PrimaryKey := false;
    result := true;
  end;

var
  SQL: RawUTF8;
  i, f: PtrInt;
  nfo: TOrmPropInfo;
  Field: TSQLDBColumnCreate;
  TableCreated, FieldAdded: boolean;
  CreateColumns: TSQLDBColumnCreateDynArray;
  options: TOrmPropertiesMappingOptions;
  log: TSynLog;

  procedure GetFields;
  begin
    fProperties.GetFields(UnQuotedSQLSymbolName(fTableName), fFieldsExternal);
    log.Log(sllDebug, 'GetFields', TypeInfo(TSQLDBColumnDefineDynArray),
      fFieldsExternal, self);
  end;

  function FieldsExternalIndexOf(const ColName: RawUTF8): PtrInt;
  begin
    if rpmMissingFieldNameCaseSensitive in options then
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

begin
  log := Owner.LogClass.Add;
  log.Enter('Create %', [aClass], self);
  inherited Create(aClass, aServer);
  // initialize external DB properties
  options := fStoredClassMapping^.options;
  fTableName := fStoredClassMapping^.TableName;
  fProperties :=
    fStoredClassMapping^.ConnectionProperties as TSQLDBConnectionProperties;
  log.Log(sllInfo, '% as % % Server=%',
    [StoredClass, fTableName, fProperties, Owner], self);
  if fProperties = nil then
    raise ERestStorage.CreateUTF8('%.Create: no external DB defined for %',
      [self, StoredClass]);
  // ensure external field names are compatible with the external DB keywords
  for f := 0 to StoredClassRecordProps.Fields.Count - 1 do
  begin
    nfo := StoredClassRecordProps.Fields.List[f];
    if nfo.OrmFieldType in COPIABLE_FIELDS then // ignore oftMany
    begin
      SQL := fStoredClassMapping^.ExtFieldNames[f];
      if rpmQuoteFieldName in options then
        fStoredClassMapping^.MapField(nfo.Name, '"' + SQL + '"')
      else if fProperties.IsSQLKeyword(SQL) then
      begin
        log.Log(sllWarning, '%.%: Field name % is not compatible with %',
          [fStoredClass, nfo.Name, SQL, fProperties.DBMSEngineName], self);
        if rpmAutoMapKeywordFields in options then
        begin
          log.Log(sllWarning, '-> %.% mapped to %_',
            [fStoredClass, nfo.Name, SQL], self);
          fStoredClassMapping^.MapField(nfo.Name, SQL + '_');
        end
        else
          log.Log(sllWarning, '-> you should better use MapAutoKeywordFields', self);
      end;
    end;
  end;
  // create corresponding external table if necessary, and retrieve its fields info
  TableCreated := false;
  GetFields;
  if not (rpmNoCreateMissingTable in options) then
    if fFieldsExternal = nil then
    begin
      // table is not yet existing -> try to create it
      with aClass.RecordProps do
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
      SQL := fProperties.SQLCreate(fTableName, CreateColumns, false);
      if SQL <> '' then
        if ExecuteDirect(pointer(SQL), [], [], false) <> nil then
        begin
          GetFields;
          if fFieldsExternal = nil then
            raise ERestStorage.CreateUTF8(
              '%.Create: external table creation % failed:  GetFields() ' +
              'returned nil - SQL="%"', [self, StoredClass, fTableName, SQL]);
          TableCreated := true;
        end;
    end;
  FieldsInternalInit;
  // create any missing field if necessary
  if not (rpmNoCreateMissingField in options) then
    if not TableCreated then
    begin
      FieldAdded := false;
      with StoredClassRecordProps do
        for f := 0 to Fields.Count - 1 do
          if Fields.List[f].OrmFieldType in COPIABLE_FIELDS then // ignore oftMany
            /// real database columns exist for Simple + Blob fields (not Many)
            if FieldsExternalIndexOf(
                fStoredClassMapping^.ExtFieldNamesUnQuotedSQL[f]) < 0 then
            begin
              // add new missing Field
              Finalize(Field);
              FillcharFast(Field, sizeof(Field), 0);
              if PropInfoToExternalField(Fields.List[f], Field) then
              begin
                SQL := fProperties.SQLAddColumn(fTableName, Field);
                if (SQL <> '') and
                   (ExecuteDirect(pointer(SQL), [], [], false) <> nil) then
                  FieldAdded := true
                else
                  raise ERestStorage.CreateUTF8(
                    '%.Create: %: unable to create external missing field %.% - SQL="%"',
                    [self, StoredClass, fTableName, Fields.List[f].Name, SQL]);
              end;
            end;
      if FieldAdded then
      begin
        // get from DB after ALTER TABLE
        GetFields;
        FieldsInternalInit;
      end;
    end;
  // compute the SQL statements used internaly for external DB requests
  with fStoredClassMapping^ do
  begin
    fSelectOneDirectSQL := FormatUTF8('select % from % where %=?',
      [SQL.TableSimpleFields[true, false], fTableName, RowIDFieldName]);
    fSelectAllDirectSQL := FormatUTF8('select %,% from %',
      [SQL.InsertSet, RowIDFieldName, fTableName]);
    fRetrieveBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableRetrieveBlobFields);
    fUpdateBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableUpdateBlobFields, '=?,', '=?');
  end;
  fSelectTableHasRowsSQL := FormatUTF8('select ID from % limit 1',
    [StoredClassRecordProps.SQLTableName]);
  AdaptSQLForEngineList(fSelectTableHasRowsSQL);
end;

function TRestStorageExternal.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
var
  Stmt: TSelectStatement;
  W: TTextWriter;
  limit: TSQLDBDefinitionLimitClause;
  limitSQL, name: RawUTF8;
  f, n: PtrInt;
  tmp: TTextWriterStackBuffer;
begin
  result := false;
  if SQL = '' then
    exit;
  Stmt := TSelectStatement.Create(SQL,
    fStoredClassRecordProps.Fields.IndexByName,
    fStoredClassRecordProps.SimpleFieldsBits[ooSelect]);
  try
    if (Stmt.SQLStatement = '') or // parsing failed
      not IdemPropNameU(Stmt.TableName, fStoredClassRecordProps.SQLTableName) then
    begin
      InternalLog('AdaptSQLForEngineList: complex statement -> switch to ' +
        'SQLite3 virtual engine - check efficiency', [], sllDebug);
      exit;
    end;
    if Stmt.Offset <> 0 then
    begin
      InternalLog('AdaptSQLForEngineList: unsupported OFFSET for [%]',
        [SQL], sllWarning);
      exit;
    end;
    if Stmt.Limit = 0 then
      limit.Position := posNone
    else
    begin
      limit := fProperties.SQLLimitClause(Stmt);
      if limit.Position = posNone then
      begin
        InternalLog('AdaptSQLForEngineList: unknown % LIMIT syntax for [%]',
          [ToText(fProperties.DBMS)^, SQL], sllWarning);
        exit;
      end;
      if limit.Position = posOuter then
        FormatUTF8(limit.InsertFmt, ['%', Stmt.Limit], limitSQL)
      else
        FormatUTF8(limit.InsertFmt, [Stmt.Limit], limitSQL);
    end;
    W := TTextWriter.CreateOwnedStream(tmp);
    try
      W.AddShorter('select ');
      if limit.Position = posSelect then
        W.AddString(limitSQL);
      for f := 0 to high(Stmt.Select) do
        with Stmt.Select[f] do
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
              name := 'ID'
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
          W.Add(',');
        end;
      W.CancelLastComma;
      W.AddShorter(' from ');
      W.AddString(fTableName);
      n := length(Stmt.Where);
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
          with Stmt.Where[f] do
          begin
            if (FunctionName <> '') or
               (Operation > high(DB_SQLOPERATOR)) then
            begin
              InternalLog('AdaptSQLForEngineList: unsupported function %() for [%]',
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
              W.AddNoJSONEscape(ValueSQL, ValueSQLLen);
            if ParenthesisAfter <> '' then
              W.AddString(ParenthesisAfter);
          end;
      end;
      if Stmt.GroupByField <> nil then
      begin
        W.AddShort(' group by ');
        for f := 0 to high(Stmt.GroupByField) do
        begin
          W.AddString(fStoredClassMapping^.FieldNameByIndex(Stmt.GroupByField[f] - 1));
          W.Add(',');
        end;
        W.CancelLastComma;
      end;
      if Stmt.OrderByField <> nil then
      begin
        W.AddShort(' order by ');
        for f := 0 to high(Stmt.OrderByField) do
        begin
          W.AddString(fStoredClassMapping^.FieldNameByIndex(Stmt.OrderByField[f] - 1));
          W.Add(',');
        end;
        W.CancelLastComma;
        if Stmt.OrderByDesc then
          W.AddShorter(' desc');
      end;
      if limit.Position = posAfter then
        W.AddString(limitSQL);
      W.SetText(SQL);
      if limit.Position = posOuter then
        SQL := FormatUTF8(limitSQL, [SQL]);
      result := true;
    finally
      W.Free;
    end;
  finally
    Stmt.Free;
  end;
end;

function TRestStorageExternal.EngineLockedNextID: TID;

  procedure RetrieveFromDB;
  // fProperties.SQLCreate: ID Int64 PRIMARY KEY -> compute unique RowID
  // (not all DB engines handle autoincrement feature - e.g. Oracle does not)
  var
    rows: ISQLDBRows;
  begin
    rows := ExecuteDirect('select max(%) from %',
      [fStoredClassMapping^.RowIDFieldName, fTableName], [], true);
    if (rows <> nil) and
       rows.Step then
      fEngineLockedMaxID := rows.ColumnInt(0)
    else
      fEngineLockedMaxID := 0;
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

function TRestStorageExternal.InternalBatchStart(Method: TURIMethod;
  BatchOptions: TRestBatchOptions): boolean;
const
  BATCH: array[mPOST..mDELETE] of TSQLDBStatementCRUD = (
    cCreate, cUpdate, cDelete);
begin
  result := false; // means BATCH mode not supported
  if (self <> nil) and
     (Method in [mPOST..mDELETE]) and
     (BATCH[Method] in fProperties.BatchSendingAbilities) then
  begin
    StorageLock(true, 'InternalBatchStart');
    // lock protected by try..finally in TRestServer.RunBatch caller
    try
      if fBatchMethod <> mNone then
        raise ERestStorage.CreateUTF8('Missing previous %.InternalBatchStop(%)',
          [self, StoredClass]);
      fBatchMethod := Method;
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
  Query: ISQLDBStatement;
  NotifySQLEvent: TOrmEvent;
  SQL: RawUTF8;
  P: PUTF8Char;
  Fields, ExternalFields: TRawUTF8DynArray;
  Types: TSQLDBFieldTypeArray;
  Values: TRawUTF8DynArrayDynArray;
  Occasion: TOrmOccasion;
  Decode: TJSONObjectDecoder;
  tmp: TSynTempBuffer;
begin
  if fBatchMethod = mNone then
    raise ERestStorage.CreateUTF8('%.InternalBatchStop(%).BatchMethod=mNone',
      [self, StoredClass]);
  try
    if fBatchCount = 0 then
      exit; // nothing to do
    if (Owner <> nil) and
       (fBatchMethod = mDelete) then
      // notify BEFORE deletion
      for i := 0 to fBatchCount - 1 do
        Owner.InternalUpdateEvent(
          oeDelete, fStoredClassProps.TableIndex, fBatchIDs[i], '', nil);
    with fProperties do
      if BatchMaxSentAtOnce > 0 then
        max := BatchMaxSentAtOnce
      else
        max := 1000;
    BatchBegin := 0;
    BatchEnd := fBatchCount - 1;
    repeat
      case fBatchMethod of
        mPost, mPut:
          begin
            assert(fBatchIDs <> nil);
            BatchEnd := fBatchCount - 1;
            for i := BatchBegin to BatchEnd do
            begin
              tmp.Init(fBatchValues[i]);
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
                    Decode.Decode(P, nil, pQuoted, fBatchIDs[i], true);
                  ooUpdate:
                    // mPut=UPDATE with the supplied fields and ID set appart
                    Decode.Decode(P, nil, pQuoted, 0, true);
                end;
                RecordVersionFieldHandle(Occasion, Decode);
                if {%H-}Fields = nil then
                begin
                  Decode.AssignFieldNamesTo(Fields);
                  SQL := JSONDecodedPrepareToSQL(Decode, ExternalFields, Types,
                    Occasion, [], {array=}true);
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
              // for SynDBPostgres array binding
              SQL := 'delete from % where %=ANY(?)'
            else
              // regular SQL
              SQL := 'delete from % where %=?';
            SQL := FormatUTF8(SQL, [fTableName, fStoredClassMapping^.RowIDFieldName]);
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
              Values[0, i] := Int64ToUTF8(fBatchIDs[BatchBegin + i]); // var fails on D2007
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
            fProperties, fTableName, ExternalFields, Types, n, Values)
        else
        begin
          // use array binding
          Query := fProperties.NewThreadSafeStatementPrepared(
            SQL, {results=}false, {except=}true);
          case fBatchMethod of
            mPost, mPut:
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
          NotifySQLEvent := oeAdd
        else
          NotifySQLEvent := oeUpdate;
        for i := 0 to fBatchCount - 1 do
          Owner.InternalUpdateEvent(NotifySQLEvent,
            fStoredClassProps.TableIndex, fBatchIDs[i], fBatchValues[i], nil);
      end;
      Owner.FlushInternalDBCache;
    end;
  finally
    fBatchValues := nil;
    fBatchIDs := nil;
    fBatchCount := 0;
    fBatchCapacity := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;

procedure TRestStorageExternal.InternalBatchAdd(const aValue: RawUTF8;
  const aID: TID);
begin
  if fBatchCount >= fBatchCapacity then
  begin
    fBatchCapacity := fBatchCapacity + 64 + fBatchCount shr 3;
    SetLength(fBatchIDs, fBatchCapacity);
    if aValue <> '' then
      SetLength(fBatchValues, fBatchCapacity);
  end;
  if aValue <> '' then
    fBatchValues[fBatchCount] := aValue;
  fBatchIDs[fBatchCount] := aID;
  inc(fBatchCount);
end;

function TRestStorageExternal.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
begin
  if (TableModelIndex < 0) or
     (fModel.Tables[TableModelIndex] <> fStoredClass) then
    result := 0
  else // avoid GPF
    if fBatchMethod <> mNone then
      if fBatchMethod <> mPOST then
        result := 0
      else
      begin
        if not JSONGetID(pointer(SentData), result) then
          result := EngineLockedNextID
        else if result > fEngineLockedMaxID then
          fEngineLockedMaxID := result;
        InternalBatchAdd(SentData, result);
      end
    else
    begin
      result := ExecuteFromJSON(SentData, ooInsert, 0);
      // UpdatedID=0 -> insert with EngineLockedNextID
      if (result > 0) and
         (Owner <> nil) then
      begin
        if EngineAddForcedID = 0 then // only worth it if result is a true ID
          Owner.InternalUpdateEvent(oeAdd, TableModelIndex, result, SentData, nil);
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TRestStorageExternal.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
begin
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else if fBatchMethod <> mNone then
    if fBatchMethod <> mPUT then
      result := false
    else
    begin
      InternalBatchAdd(SentData, ID);
      result := true;
    end
  else
  begin
    result := ExecuteFromJSON(SentData, ooUpdate, ID) = ID;
    if result and
       (Owner <> nil) then
    begin
      Owner.InternalUpdateEvent(oeUpdate, TableModelIndex, ID, SentData, nil);
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
    if fBatchMethod <> mDELETE then
      result := false
    else
    begin
      InternalBatchAdd('', ID);
      result := true;
    end
  else
  begin
    if Owner <> nil then // notify BEFORE deletion
      Owner.InternalUpdateEvent(oeDelete, TableModelIndex, ID, '', nil);
    result := ExecuteDirect('delete from % where %=?',
      [fTableName, fStoredClassMapping^.RowIDFieldName], [ID], false) <> nil;
    if result and
       (Owner <> nil) then
      Owner.FlushInternalDBCache;
  end;
end;

function TRestStorageExternal.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
const
  CHUNK_SIZE = 200;
var
  i, n, chunk, pos: PtrInt;
  rowid: RawUTF8;
begin
  result := false;
  if (IDs = nil) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  n := length(IDs);
  if fBatchMethod <> mNone then
    if fBatchMethod <> mDELETE then
      exit
    else
      for i := 0 to n - 1 do
        InternalBatchAdd('', IDs[i])
  else
  begin
    if Owner <> nil then // notify BEFORE deletion
      for i := 0 to n - 1 do
        Owner.InternalUpdateEvent(oeDelete, TableModelIndex, IDs[i], '', nil);
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
          [fTableName, rowid, Int64DynArrayToCSV(pointer(@IDs[pos]), chunk)],
          false) = nil then
        exit;
      inc(pos, chunk);
    until false;
    if Owner <> nil then
      Owner.FlushInternalDBCache;
  end;
  result := true;
end;

function TRestStorageExternal.EngineList(const SQL: RawUTF8;
  ForceAJAX: boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var
  Stmt: ISQLDBStatement;
begin
  result := '';
  if ReturnedRowCount <> nil then
    raise ERestStorage.CreateUTF8('%.EngineList(ReturnedRowCount<>nil) for %',
      [self, StoredClass]);
  Stmt := PrepareInlinedForRows(SQL);
  if Stmt <> nil then
  try
    Stmt.ExecutePreparedAndFetchAllAsJSON(
      ForceAJAX or (Owner = nil) or not Owner.Owner.NoAJAXJSON, result);
  except
    Stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
var
  Stmt: ISQLDBStatement;
begin // TableModelIndex is not useful here
  result := '';
  if (self = nil) or
     (ID <= 0) then
    exit;
  Stmt := PrepareDirectForRows(pointer(fSelectOneDirectSQL), [], [ID]);
  if Stmt <> nil then
  try
    // Expanded=true -> '[{"ID":10,...}]'#10
    Stmt.ExecutePreparedAndFetchAllAsJSON(true, result);
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := ''
    else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result, 2, length(result) - 3);
  except
    Stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  if aSQL = '' then
    result := false
  else
    result := ExecuteInlined(aSQL, false) <> nil;
end;

function TRestStorageExternal.TableHasRows(Table: TOrmClass): boolean;
var
  rows: ISQLDBRows;
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
      result := rows.Step;
  end;
end;

function TRestStorageExternal.TableRowCount(Table: TOrmClass): Int64;
var
  rows: ISQLDBRows;
begin
  if (self = nil) or
     (Table <> fStoredClass) then
    result := 0
  else
  begin
    rows := ExecuteDirect('select count(*) from %', [fTableName], [], true);
    if (rows = nil) or
       not rows.Step then
      result := 0
    else
      result := rows.ColumnInt(0);
  end;
end;

function TRestStorageExternal.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  rows: ISQLDBRows;
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
    rows := nil;
    result := true; // success
  except
    rows := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.RetrieveBlobFields(Value: TOrm): boolean;
var
  rows: ISQLDBRows;
  f: PtrInt;
  data: TSQLVar;
  temp: RawByteString;
begin
  result := false;
  if (Value <> nil) and
     (Value.ID > 0) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.RecordProps do
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
            rows.ColumnToSQLVar(f, data, temp);
            BlobFields[f].SetFieldSQLVar(Value, data);
          end;
          rows.ReleaseRows;
          rows := nil;
          result := true; // success
        except
          HandleClearPoolOnConnectionIssue;
        end;
      end;
end;

function TRestStorageExternal.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var
  rows: ISQLDBRows;
  ExtWhereFieldName, JSON: RawUTF8;
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
      if result and (Owner <> nil) then
      begin
        if Owner.InternalUpdateEventNeeded(TableModelIndex) then
        begin
          rows := ExecuteInlined('select % from % where %=:(%):',
            [RowIDFieldName, fTableName, ExtWhereFieldName, WhereValue], true);
          if rows = nil then
            exit;
          JSONEncodeNameSQLValue(SetFieldName, SetValue, JSON);
          while rows.Step do
            Owner.InternalUpdateEvent(
              oeUpdate, TableModelIndex, rows.ColumnInt(0), JSON, nil);
          rows.ReleaseRows;
        end;
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TRestStorageExternal.EngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUTF8;
  Increment: Int64): boolean;
var
  extField: RawUTF8;
  Value: Int64;
begin
  result := false;
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  if (Owner <> nil) and
     Owner.InternalUpdateEventNeeded(TableModelIndex) then
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
    if result and (Owner <> nil) then
      Owner.FlushInternalDBCache;
  except
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  Statement: ISQLDBStatement;
  AffectedField: TFieldBits;
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
      Statement := fProperties.NewThreadSafeStatementPrepared(
        'update % set %=? where %=?',
        [fTableName, InternalToExternal(BlobField), RowIDFieldName],
        {results=}false, {except=}true);
    if Statement <> nil then
    begin
      if BlobData = '' then
        Statement.BindNull(1)
      else
        Statement.BindBlob(1, BlobData); // fast explicit BindBlob() call
      Statement.Bind(2, aID);
      Statement.ExecutePrepared;
      if Owner <> nil then
      begin
        fStoredClassRecordProps.FieldBitsFromBlobField(BlobField, AffectedField);
        Owner.InternalUpdateEvent(
          oeUpdateBlob, TableModelIndex, aID, '', @AffectedField);
        Owner.FlushInternalDBCache;
      end;
      result := true; // success
    end;
  except
    Statement := nil;
    HandleClearPoolOnConnectionIssue; // leave result=false to notify error
  end;
end;

function TRestStorageExternal.UpdateBlobFields(Value: TOrm): boolean;
var
  f: PtrInt;
  aID: TID;
  temp: array of RawByteString;
  Params: TSQLVarDynArray;
begin
  result := false;
  if (Value <> nil) and
     (POrmClass(Value)^ = fStoredClass) then
    with Value.RecordProps do
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
          BlobFields[f].GetFieldSQLVar(Value, Params[f], temp[f]);
        result := ExecuteDirectSQLVar('update % set % where %=?',
          [fTableName, fUpdateBlobFieldsSQL, fStoredClassMapping^.RowIDFieldName],
          Params, aID, false);
        if result and (Owner <> nil) then
        begin
          Owner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex,
            aID, '', @fStoredClassRecordProps.FieldBits[oftBlob]);
          Owner.FlushInternalDBCache;
        end;
      end
      else
        result := true; // as TRest.UpdateblobFields()
end;

function TRestStorageExternal.PrepareInlinedForRows(
  const aSQL: RawUTF8): ISQLDBStatement;
var
  stmt: ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  try
    stmt := fProperties.PrepareInlined(aSQL, true);
    if (stmt <> nil) and
       (oftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TRestStorageExternal.ExecuteInlined(const aSQL: RawUTF8;
  ExpectResults: boolean): ISQLDBRows;
var
  stmt: ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self = nil then
    exit;
  if not ExpectResults and
     (Owner <> nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    stmt := fProperties.PrepareInlined(aSQL, ExpectResults);
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

function TRestStorageExternal.ExecuteInlined(SQLFormat: PUTF8Char;
  const Args: array of const; ExpectResults: boolean): ISQLDBRows;
begin
  result := ExecuteInlined(FormatUTF8(SQLFormat, Args), ExpectResults);
end;

function TRestStorageExternal.PrepareDirectForRows(SQLFormat: PUTF8Char;
  const Args, Params: array of const): ISQLDBStatement;
var
  stmt: ISQLDBStatement;
begin
  result := nil;
  if self <> nil then
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(
      SQLFormat, Args, {results=}true, {except=}true);
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

function TRestStorageExternal.ExecuteDirect(SQLFormat: PUTF8Char;
  const Args, Params: array of const; ExpectResults: boolean): ISQLDBRows;
var
  stmt: ISQLDBStatement;
begin
  result := nil;
  if self = nil then
    exit;
  if not ExpectResults and
     (Owner <> nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(
      SQLFormat, Args, ExpectResults, {except=}true);
    stmt.Bind(Params);
    if ExpectResults and
       (oftDateTimeMS in fStoredClassRecordProps.HasTypeFields) then
      stmt.ForceDateWithMS := true;
    stmt.ExecutePrepared;
    if IdemPChar(SQLFormat, 'DROP TABLE ') then
    begin
      fEngineLockedMaxID := 0;
    end;
    result := stmt;
  except
    stmt := nil;
    HandleClearPoolOnConnectionIssue; // leave result=nil to notify error
  end;
end;

function TRestStorageExternal.ExecuteDirectSQLVar(SQLFormat: PUTF8Char;
  const Args: array of const; var Params: TSQLVarDynArray;
  const LastIntegerParam: Int64; ParamsMatchCopiableFields: boolean): boolean;
var
  stmt: ISQLDBStatement;
  ParamsCount, f: PtrInt;
begin
  result := false;
  if Self <> nil then
  try
    stmt := fProperties.NewThreadSafeStatementPrepared(SQLFormat, Args,
      {results=}false, {except=}true);
    if stmt = nil then
      exit;
    ParamsCount := length(Params);
    if ParamsMatchCopiableFields and
       (ParamsCount <> Length(fStoredClassRecordProps.CopiableFields)) then
      raise ERestStorage.CreateUTF8(
        '%.ExecuteDirectSQLVar(ParamsMatchCopiableFields) for %',
        [self, StoredClass]);
    for f := 0 to ParamsCount - 1 do
      if ParamsMatchCopiableFields and
         (fStoredClassRecordProps.CopiableFields[f].
           OrmFieldTypeStored in [oftDateTime, oftDateTimeMS]) and
         (Params[f].VType = ftUTF8) then
        stmt.BindDateTime(f + 1, Iso8601ToDateTimePUTF8Char(Params[f].VText))
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
  rows: ISQLDBRows;
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

function TRestStorageExternal.SearchField(const FieldName: RawUTF8;
  FieldValue: Int64; out ResultID: TIDDynArray): boolean;
begin
  result := EngineSearchField(FieldName, [FieldValue], ResultID);
end;

function TRestStorageExternal.SearchField(const FieldName, FieldValue: RawUTF8;
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
  ACTION: array[boolean] of TSQLDBSharedTransactionAction = (
    transCommitWithoutException, transCommitWithException);
begin
  inherited Commit(SessionID, RaiseException);
  // reset fTransactionActive + write all TOrmVirtualTableJSON
  fProperties.SharedTransaction(SessionID, ACTION[RaiseException]);
end;

procedure TRestStorageExternal.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset fTransactionActive
  fProperties.SharedTransaction(SessionID, transRollback);
end;

function TRestStorageExternal.CreateSQLMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUTF8; Unique: boolean;
  IndexName: RawUTF8): boolean;
var
  SQL: RawUTF8;
  ExtFieldNames: TRawUTF8DynArray;
  IntFieldIndex: TIntegerDynArray;
  Descending: boolean;
  i, n, extfield: PtrInt;
begin
  result := false;
  Descending := false;
  n := length(FieldNames);
  if (self = nil) or
     (fProperties = nil) or
     (Table <> fStoredClass) or
     (n <= 0) then
    exit;
  fStoredClassMapping^.InternalToExternalDynArray(
    FieldNames, ExtFieldNames, @IntFieldIndex);
  if n = 1 then
  begin // handle case of index over a single column
    if IntFieldIndex[0] < 0 then // ID/RowID?
      case fProperties.DBMS of
        dSQLite, dPostgreSQL, dMSSQL, dMySQL, dOracle, dNexusDB:
          begin
            // most DB create an implicit index on primary key
            result := true;
            exit;
          end;
        dFirebird:
          // see http://www.firebirdfaq.org/faq205
          Descending := true;
      end;
    if not Descending then
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
  if not (fProperties.DBMS in DB_HANDLEINDEXONBLOBS) then
    // BLOB fields cannot be indexed (only in SQLite3+PostgreSQL)
    for i := 0 to n - 1 do
    begin
      extfield := fFieldsInternalToExternal[IntFieldIndex[i] + 1];
      if (extfield >= 0) and
         (fFieldsExternal[extfield].ColumnType in [ftBlob, ftUTF8]) and
         (fFieldsExternal[extfield].ColumnLength <= 0) then
      begin
        if i = 0 then
          exit; // impossible to create an index with no field!
        SetLength(ExtFieldNames, i); // truncate index to the last indexable field
        break;
      end;
    end;
  SQL := fProperties.SQLAddIndex(
    fTableName, ExtFieldNames, Unique, Descending, IndexName);
  if (SQL = '') or
     (ExecuteDirect(pointer(SQL), [], [], false) = nil) then
    exit;
  result := true;
  extfield := fFieldsInternalToExternal[IntFieldIndex[0] + 1];
  if extfield >= 0 then
    // mark first column as indexed by now
    fFieldsExternal[extfield].ColumnIndexed := true;
end;

class function TRestStorageExternal.Instance(aClass: TOrmClass;
  aServer: TRestORMServer): TRestStorageExternal;
begin
  if (aClass = nil) or
     (aServer = nil) then
    result := nil
  else
  begin
    result := TRestStorageExternal(aServer.StaticVirtualTable[aClass]);
    if result <> nil then
      if not result.InheritsFrom(TRestStorageExternal) then
        result := nil;
  end;
end;

class function TRestStorageExternal.ConnectionProperties(aClass: TOrmClass;
  aServer: TRestORMServer): TSQLDBConnectionProperties;
begin
  result := Instance(aClass, aServer).GetConnectionProperties;
end;

function TRestStorageExternal.GetConnectionProperties: TSQLDBConnectionProperties;
begin
  if self = nil then
    result := nil
  else
    result := fProperties;
end;

function TRestStorageExternal.HandleClearPoolOnConnectionIssue: boolean;
var
  conn: TSQLDBConnection;
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

function TRestStorageExternal.ExecuteFromJSON(const SentData: RawUTF8;
  Occasion: TOrmOccasion; UpdatedID: TID): TID;
var
  Decoder: TJSONObjectDecoder;
  SQL: RawUTF8;
  Types: TSQLDBFieldTypeArray;
  ExternalFields: TRawUTF8DynArray;
  InsertedID: TID;
  F: PtrInt;
  stmt: ISQLDBStatement;
begin
  result := 0;
  StorageLock(false, 'ExecuteFromJson'); // avoid race condition against max(ID)
  try
    case Occasion of
      ooInsert:
        if not JSONGetID(pointer(SentData), InsertedID) then
          // no specified "ID":... field value -> compute next
          InsertedID := EngineLockedNextID
        else if InsertedID > fEngineLockedMaxID then
          fEngineLockedMaxID := InsertedID;
      ooUpdate:
        if UpdatedID <> 0 then
          InsertedID := 0
        else
          raise ERestStorage.CreateUTF8(
            '%.ExecuteFromJSON(%,soUpdate,UpdatedID=%)',
            [self, StoredClass, UpdatedID]);
    else
      raise ERestStorage.CreateUTF8('%.ExecuteFromJSON(%,Occasion=%)?',
        [self, StoredClass, ToText(Occasion)^]);
    end;
    // decode fields
    if (fEngineAddForcedID <> 0) and
       (InsertedID = fEngineAddForcedID) then
      Decoder.Decode(SentData, nil, pNonQuoted, 0, true)
    else
      Decoder.Decode(SentData, nil, pNonQuoted, InsertedID, true);
    if (Decoder.FieldCount = 0) and
       (Occasion = ooUpdate) then
    begin
      // SentData='' -> no column to update
      result := UpdatedID;
      exit;
    end;
    RecordVersionFieldHandle(Occasion, Decoder);
    // compute SQL statement and associated bound parameters
    SQL := JSONDecodedPrepareToSQL(
      Decoder, ExternalFields, Types, Occasion, [], {array=}false);
    if Occasion = ooUpdate then
      // Int64ToUTF8(var) fails on D2007
      Decoder.FieldValues[Decoder.FieldCount - 1] := Int64ToUTF8(UpdatedID);
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
  if fProperties.InheritsFrom(TSQLDBConnectionPropertiesThreadSafe) then
    TSQLDBConnectionPropertiesThreadSafe(fProperties).EndCurrentThread;
end;

function TRestStorageExternal.InternalFieldNameToFieldExternalIndex(
  const InternalFieldName: RawUTF8): integer;
begin
  result := fStoredClassRecordProps.Fields.IndexByNameOrExcept(InternalFieldName);
  result := fFieldsInternalToExternal[result + 1];
end;

function TRestStorageExternal.JSONDecodedPrepareToSQL(
  var Decoder: TJSONObjectDecoder; out ExternalFields: TRawUTF8DynArray;
  out Types: TSQLDBFieldTypeArray; Occasion: TOrmOccasion;
  BatchOptions: TRestBatchOptions; BoundArray: boolean): RawUTF8;
var
  f, k: PtrInt;
begin
  SetLength(ExternalFields, Decoder.FieldCount);
  for f := 0 to Decoder.FieldCount - 1 do
  begin
    k := fStoredClassRecordProps.Fields.IndexByNameOrExcept(
      Decoder.FieldNames[f]);
    ExternalFields[f] := fStoredClassMapping^.FieldNameByIndex(k);
    k := fFieldsInternalToExternal[k + 1]; // retrieve exact Types[f] from SynDB
    if k < 0 then
      raise ERestStorage.CreateUTF8(
        '%.JSONDecodedPrepareToSQL(%): No column for [%] field in table %',
        [self, StoredClass, Decoder.FieldNames[f], fTableName]);
    Types[f] := fFieldsExternal[k].ColumnType;
  end;
  // compute SQL statement and associated bound parameters
  Decoder.DecodedFieldNames := pointer(ExternalFields);
  if BoundArray and
     (cPostgreBulkArray in fProperties.BatchSendingAbilities) then
    // SynDBPostgres array binding e.g. via 'insert into ... values (unnest...)'
    Decoder.DecodedFieldTypesToUnnest := @Types;
  result := Decoder.EncodeAsSQLPrepared(fTableName, Occasion,
    fStoredClassMapping^.RowIDFieldName, BatchOptions);
  if Occasion = ooUpdate then
    if Decoder.FieldCount = MAX_SQLFIELDS then
      raise ERestStorage.CreateUTF8(
        'Too many fields for %.JSONDecodedPrepareToSQL', [self])
    else
    begin
      // add "where ID=?" parameter
      Types[Decoder.FieldCount] := ftInt64;
      inc(Decoder.FieldCount);
    end;
end;

procedure TRestStorageExternal.EngineAddForceSelectMaxID;
begin
  StorageLock(true, 'EngineAddForceSelectMaxID');
  fEngineLockedMaxID := 0;
  StorageUnLock;
end;

const
  SQL_OPER_WITH_PARAM: array[soEqualTo..soGreaterThanOrEqualTo] of RawUTF8 = (
    '=?', '<>?', '<?', '<=?', '>?', '>=?');

function TRestStorageExternal.ComputeSQL(
  const Prepared: TOrmVirtualTablePrepared): RawUTF8;
var
  i: PtrInt;
  constraint: POrmVirtualTablePreparedConstraint;
  {$ifdef SQLVIRTUALLOGS}
  log: RawUTF8;
  {$endif SQLVIRTUALLOGS}
begin
  result := fSelectAllDirectSQL;
  for i := 0 to Prepared.WhereCount - 1 do
  begin
    constraint := @Prepared.Where[i];
    {$ifdef SQLVIRTUALLOGS}
    log := FormatUTF8('% [column=% oper=%]', [log, constraint^.Column,
      ToText(constraint^.Operation)^]);
    {$endif SQLVIRTUALLOGS}
    if constraint^.Operation > high(SQL_OPER_WITH_PARAM) then
      exit; // invalid specified operator -> abort search
    if i = 0 then
      result := result + ' where '
    else
      result := result + ' and ';
    if fStoredClassMapping^.AppendFieldName(constraint^.Column, result) then
      // invalid column index -> abort search
      exit;
    result := result + SQL_OPER_WITH_PARAM[constraint^.Operation];
  end;
  // e.g. 'select FirstName,..,ID from PeopleExternal where FirstName=? and LastName=?'
  for i := 0 to Prepared.OrderByCount - 1 do
    with Prepared.OrderBy[i] do
    begin
      if i = 0 then
        result := result + ' order by '
      else
        result := result + ', ';
      if fStoredClassMapping^.AppendFieldName(Column, result) then
        // invalid column index -> abort search
        exit;
      if Desc then
        result := result + ' desc';
    end;
  {$ifdef SQLVIRTUALLOGS}
  SQLite3Log.Add.Log(sllDebug, '%.ComputeSQL [%] %', [ClassType, result, log], self);
  {$endif SQLVIRTUALLOGS}
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
  if fStatement <> nil then
    fStatement.ReleaseRows;
  inherited Destroy;
end;

function TOrmVirtualTableCursorExternal.Column(aColumn: integer;
  var aResult: TSQLVar): boolean;
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
    fStatement.ColumnToSQLVar(aColumn, aResult, fColumnTemp);
    result := aResult.VType <> ftUnknown;
  except
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TOrmVirtualTableCursorExternal.HasData: boolean;
begin
  result := (self <> nil) and
            (fStatement <> nil) and
            fHasData;
end;

function TOrmVirtualTableCursorExternal.Next: boolean;
begin
  result := false;
  if (self <> nil) and
     (fStatement <> nil) then
  try
    fHasData := fStatement.Step;
    result := true; // success (may be with no more data)
  except
    HandleClearPoolOnConnectionIssue;
  end;
end;

function TOrmVirtualTableCursorExternal.Search(
  const Prepared: TOrmVirtualTablePrepared): boolean;
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
    {$ifndef SQLVIRTUALLOGS}
    if fSQL = '' then
    {$endif SQLVIRTUALLOGS}
      fSQL := storage.ComputeSQL(Prepared);
    try
      fStatement := storage.fProperties.NewThreadSafeStatementPrepared(
        fSQL, {results=}true, {except=}true);
      if fStatement <> nil then
      begin
        if oftDateTimeMS in storage.fStoredClassRecordProps.HasTypeFields then
          fStatement.ForceDateWithMS := true;
        for i := 1 to Prepared.WhereCount do
          fStatement.Bind(i, Prepared.Where[i - 1].Value);
        fStatement.ExecutePrepared;
        result := Next; // on execution success, go to the first row
      end;
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
  if result and (Static <> nil) then
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
     (Static <> nil) and (aRowID > 0) then
    with Static as TRestStorageExternal do
      result := ExecuteDirect('delete from % where %=?',
        [fTableName, fStoredClassMapping^.RowIDFieldName],
        [aRowID], false) <> nil
  else
    result := false;
end;

function TOrmVirtualTableExternal.Insert(aRowID: Int64;
  var Values: TSQLVarDynArray; out insertedRowID: Int64): boolean;
begin // aRowID is just ignored here since IDs are always auto calculated
  result := false;
  if (self <> nil) and
     (Static <> nil) then
    with Static as TRestStorageExternal do
    begin
      StorageLock(false, 'Insert'); // to avoid race condition against max(RowID)
      try
        insertedRowID := EngineLockedNextID;
        with fStoredClassMapping^ do
          result := ExecuteDirectSQLVar('insert into % (%,%) values (%,?)',
            [fTableName, SQL.InsertSet, RowIDFieldName,
             CSVOfValue('?', length(Values))],
            Values, insertedRowID, true);
      finally
        StorageUnLock;
      end;
    end;
end;

function TOrmVirtualTableExternal.Update(oldRowID, newRowID: Int64;
  var Values: TSQLVarDynArray): boolean;
begin
  if (self <> nil) and
     (Static <> nil) and
     (oldRowID = newRowID) and
     (newRowID > 0) then // don't allow ID change
    with Static as TRestStorageExternal, fStoredClassMapping^ do
      result := ExecuteDirectSQLVar('update % set % where %=?',
        [fTableName, SQL.UpdateSetAll, RowIDFieldName],
        Values, oldRowID, true)
  else
    result := false;
end;


{ *********** External SQL Database Engines Registration }

function VirtualTableExternalRegister(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8;
  aMappingOptions: TOrmPropertiesMappingOptions): boolean;
var
  ExternalTableName: RawUTF8;
  Props: TOrmModelProperties;
begin
  result := False;
  if (aModel = nil) or
     (aClass = nil) or
     (aExternalDB = nil) then
    // avoid GPF
    exit;
  Props := aModel.Props[aClass];
  if Props = nil then
    // if aClass is not part of the model
    exit;
  Props.Kind := rCustomAutoID; // force creation use of SQLite3 virtual table
  if aExternalTableName = '' then
    ExternalTableName := Props.Props.SQLTableName
  else
    ExternalTableName := aExternalTableName;
  result := aModel.VirtualTableRegister(aClass, TOrmVirtualTableExternal,
    aExternalDB.SQLFullTableName(ExternalTableName), aExternalDB, aMappingOptions);
end;

function VirtualTableExternalRegister(aModel: TOrmModel;
  const aClass: array of TOrmClass; aExternalDB: TSQLDBConnectionProperties;
  aMappingOptions: TOrmPropertiesMappingOptions): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to High(aClass) do
    if not VirtualTableExternalRegister(
       aModel, aClass[i], aExternalDB, '', aMappingOptions) then
      result := false;
end;

function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSQLDBConnectionProperties;
  DoNotRegisterUserGroupTables, ClearPoolOnConnectionIssue: boolean): boolean;
var
  opt: TVirtualTableExternalRegisterOptions;
begin
  opt := []; // to call the overloaded function below with proper options
  if DoNotRegisterUserGroupTables then
    include(opt, regDoNotRegisterUserGroupTables);
  if ClearPoolOnConnectionIssue then
    include(opt, regClearPoolOnConnectionIssue);
  result := VirtualTableExternalRegisterAll(aModel, aExternalDB, opt);
end;

function VirtualTableExternalRegisterAll(aModel: TOrmModel;
  aExternalDB: TSQLDBConnectionProperties;
  aExternalOptions: TVirtualTableExternalRegisterOptions): boolean;
var
  i: PtrInt;
  rec: TOrmClass;
  opt: TOrmPropertiesMappingOptions;
begin
  result := (aModel <> nil) and (aExternalDB <> nil);
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
    else if not VirtualTableExternalRegister(aModel, rec, aExternalDB, '', opt) then
      result := false
    else if regMapAutoKeywordFields in aExternalOptions then
      aModel.TableProps[i].ExternalDB.MapAutoKeywordFields;
  end;
end;

function VirtualTableExternalMap(aModel: TOrmModel; aClass: TOrmClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8;
  aMapping: TOrmPropertiesMappingOptions): POrmPropertiesMapping;
begin
  if VirtualTableExternalRegister(aModel, aClass, aExternalDB,
    aExternalTableName, aMapping) then
    result := @aModel.Props[aClass].ExternalDB
  else
    result := nil;
end;

function TRestExternalDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aExternalOptions: TVirtualTableExternalRegisterOptions): TRest;
var
  propsClass: TSQLDBConnectionPropertiesClass;
  props: TSQLDBConnectionProperties;
begin
  result := nil;
  if aDefinition = nil then
    exit;
  propsClass := TSQLDBConnectionProperties.ClassFrom(aDefinition);
  if propsClass <> nil then
  begin
    props := nil;
    try
      // aDefinition.Kind was a TSQLDBConnectionProperties -> all external DB
      props := propsClass.Create(aDefinition.ServerName,
        aDefinition.DatabaseName, aDefinition.User, aDefinition.PassWordPlain);
      VirtualTableExternalRegisterAll(aModel, props, aExternalOptions);
      // instantiate either a SQLite3 :memory: DB or a TRestServerFullMemory
      result := CreateInMemoryServerForAllVirtualTables(
        aModel, aHandleAuthentication);
    except
      FreeAndNil(result);
      props.Free;  // avoid memory leak
    end;
  end
  else
    // not external DB -> try if aDefinition.Kind is a TRest class
    result := TRest.CreateTryFrom(aModel, aDefinition, aHandleAuthentication);
end;



end.

