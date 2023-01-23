/// Object-Relational-Mapping (ORM) Abstract REST Implementation
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.rest;

{
  *****************************************************************************

   IRestOrm Implementation as used by TRest
    - Some definitions Used by TRestOrm Implementation
    - TRestOrm Parent Class for abstract REST client/server
    - TOrmTableWritable Read/Write TOrmTable
   
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
  mormot.crypt.secure,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.json,
  mormot.orm.base,
  mormot.orm.core,
  mormot.rest.core,
  mormot.db.core;


{ ************ Some definitions Used by TRestOrm Implementation }

const
  /// convert a TRestBatch encoding scheme into the corresponding ORM TUriMethod
  BATCH_METHOD: array[TRestBatchEncoding] of TUriMethod = (
    mPOST,     // encPost
    mPOST,     // encSimple
    mPOST,     // encPostHex
    mPOST,     // encPostHexID
    mPUT,      // encPut
    mPUT,      // encPutHex
    mDELETE);  // encDelete

  /// convert a TRestBatch encoding scheme into the corresponding ORM TUriMethod
  BATCH_EVENT: array[TRestBatchEncoding] of TOrmEvent = (
    oeAdd,     // encPost
    oeAdd,     // encSimple
    oeAdd,     // encPostHex
    oeAdd,     // encPostHexID
    oeUpdate,  // encPut
    oeUpdate,  // encPutHex
    oeDelete); // encDelete


{ ************ TRestOrm Parent Class for abstract REST client/server }

type
  /// low-level TRestOrm.InternalBatchDirectSupport response
  TRestOrmBatchDirect = (
    dirUnsupported,
    dirWriteLock,
    dirWriteNoLock);

  {$M+}

  /// implements TRest.ORM process for abstract REST client/server
  TRestOrm = class(TRestOrmParent, IRestOrm)
  protected
    fRest: TRest;
    fModel: TOrmModel; // owned by the TRest associated instance
    fCache: TOrmCache;
    fTransactionActiveSession: cardinal;
    fTransactionTable: TOrmClass;
    fTempJsonWriter: TJsonWriter;
    fTempJsonWriterLock: TLightLock;
    /// compute SELECT ... FROM TABLE WHERE ...
    function SQLComputeForSelect(Table: TOrmClass;
      const FieldNames, WhereClause: RawUtf8): RawUtf8;
    /// used by all overloaded Add/Delete methods
    procedure GetJsonValuesForAdd(TableIndex: integer; Value: TOrm;
      ForceID, DoNotAutoComputeFields, WithBlobs: boolean;
      CustomFields: PFieldBits; var result: RawUtf8);
    function InternalAdd(Value: TOrm; SendData: boolean;
      CustomFields: PFieldBits;
      ForceID, DoNotAutoComputeFields: boolean): TID; virtual;
    function InternalDeleteNotifyAndGetIDs(Table: TOrmClass;
      const SqlWhere: RawUtf8; var IDs: TIDDynArray): boolean;
  public
    // ------- abstract methods to be overriden by the real database engine
    /// retrieve a list of members as JSON encoded data
    // - implements REST GET collection
    // - returns '' on error, or JSON data, even with no result rows
    // - override this method for direct data retrieval from the database engine
    // and direct JSON export, avoiding a TOrmTable which allocates memory for every
    // field values before the JSON export
    // - can be called for a single Table (ModelRoot/Table), or with low level SQL
    // query (ModelRoot + SQL sent as request body)
    // - if ReturnedRowCount points to an integer variable, it must be filled with
    // the number of row data returned (excluding field names)
    // - this method must be implemented in a thread-safe manner
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; virtual; abstract;
    /// Execute directly a SQL statement, without any result
    // - implements POST SQL on ModelRoot URI
    // - return true on success
    // - override this method for proper calling the database engine
    // - don't call this method in normal cases
    // - this method must be implemented to be thread-safe
    function EngineExecute(const aSql: RawUtf8): boolean; virtual; abstract;
    /// get a member from its ID
    // - implements REST GET member
    // - returns the data of this object as JSON
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; virtual; abstract;
    /// create a new member
    // - implements REST POST collection
    // - SentData can contain the JSON object with field values to be added
    // - class is taken from Model.Tables[TableModelIndex]
    // - returns the TOrm ID/RowID value, 0 on error
    // - if a "RowID":.. or "ID":.. member is set in SentData, it shall force
    // this value as insertion ID
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineAdd(TableModelIndex: integer; const SentData: RawUtf8): TID; virtual; abstract;
    /// update a member
    // - implements REST PUT collection
    // - SentData can contain the JSON object with field values to be added
    // - returns true on success
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineUpdate(TableModelIndex: integer; ID: TID; const SentData: RawUtf8): boolean; virtual; abstract;
    /// delete a member
    // - implements REST DELETE collection
    // - returns true on success
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; virtual; abstract;
    /// delete several members, from a WHERE clause
    // - IDs[] contains the already-computed matching IDs for SqlWhere
    // - returns true on success
    // - override this method for proper calling the database engine, i.e.
    // using either IDs[] or a faster SQL statement
    // - this method must be implemented in a thread-safe manner
    function EngineDeleteWhere(TableModelIndex: integer; const SqlWhere: RawUtf8;
      const IDs: TIDDynArray): boolean; virtual; abstract;
    /// get a blob field content from its member ID and field name
    // - implements REST GET member with a supplied blob field name
    // - returns TRUE on success
    // - returns the data of this blob as raw binary (not JSON) in BlobData
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; virtual; abstract;
    /// update a blob field content from its member ID and field name
    // - implements REST PUT member with a supplied blob field name
    // - returns TRUE on success
    // - the data of this blob must be specified as raw binary (not JSON) in BlobData
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; virtual; abstract;
    /// update an individual record field value from a specified ID or Value
    // - return true on success
    // - will allow execution of requests like
    // $ UPDATE tablename SET setfieldname=setvalue WHERE wherefieldname=wherevalue
    // - SetValue and WhereValue parameters must match our inline format, i.e.
    // by double quoted with " for strings, or be plain text for numbers - e.g.
    // $ Client.EngineUpdateField(TOrmMyRecord,'FirstName','"Smith"','RowID','10')
    // but you should better use the UpdateField() overload methods instead
    // - WhereFieldName and WhereValue must be set: for security reasons,
    // implementations of this method will reject an UPDATE without any WHERE
    // clause, so you won't be able to use it to execute such statements:
    // $ UPDATE tablename SET setfieldname=setvalue
    // - this method must be implemented in a thread-safe manner
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean; virtual; abstract;
    /// increments one integer field value
    // - this default implementation is just a wrapper around OneFieldValue +
    // UpdateField methods
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; virtual;
    /// send/execute the supplied JSON BATCH content, and return the expected array
    // - this method will be implemented for TRestClient and TRestServer only
    // - this default implementation will trigger an EOrmException
    // - warning: supplied JSON Data can be parsed in-place, so modified
    function EngineBatchSend(Table: TOrmClass; var Data: RawUtf8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; virtual;
    /// internal method called by TRestServer.Batch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    // - returns TRUE if this method is handled by the engine, or FALSE if
    // individual calls to Engine*() are expected
    // - this default implementation returns FALSE
    // - an overridden method returning TRUE shall ensure that calls to
    // EngineAdd / EngineUpdate / EngineDelete (depending of supplied Method)
    // will properly handle operations until InternalBatchStop() is called
    function InternalBatchStart(Encoding: TRestBatchEncoding;
      BatchOptions: TRestBatchOptions): boolean; virtual;
    /// internal method called by TRestServer.Batch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    // - this default implementation will raise an EOrmException (since
    // InternalBatchStart returns always FALSE at this TRest level)
    // - InternalBatchStart/Stop may safely use a lock for multithreading:
    // implementation in TRestServer.Batch use a try..finally block
    procedure InternalBatchStop; virtual;
    /// internal method called by TRestServer.Batch() to process SIMPLE input
    // - an optimized storage engine could override it to process the Sent
    // JSON array values directly from the memory buffer
    // - called first to return if supported in overriden methods
    function InternalBatchDirectSupport(Encoding: TRestBatchEncoding;
      RunTableIndex: integer): TRestOrmBatchDirect; virtual;
    /// internal method called by TRestServer.Batch() to process SIMPLE input
    // - an optimized storage engine could override it to process the Sent
    // JSON array values directly from the memory buffer
    // - called a second time with the proper Sent JSON array of values,
    // returning the inserted ID or 200 after proper update
    function InternalBatchDirectOne(Encoding: TRestBatchEncoding;
      RunTableIndex: integer; const Fields: TFieldBits; Sent: PUtf8Char): TID; virtual;
  public
    // ------- TRestOrm main methods
    /// initialize the class, and associated to a TRest and its TOrmModel
    constructor Create(aRest: TRest); reintroduce; virtual;
      /// initialize the class, and associated to TOrmModel with no main TRest
    constructor CreateWithoutRest(aModel: TOrmModel); reintroduce; virtual;
    /// release internal used instances
    destructor Destroy; override;
    /// internal TOrm value serialization to a JSON object
    // - will use shared AcquireJsonWriter instance if available
    procedure GetJsonValue(Value: TOrm; withID: boolean; const Fields: TFieldBits;
      out Json: RawUtf8; LowerCaseID: boolean = false); overload;
    /// internal TOrm value serialization to a JSON object
    // - will use shared AcquireJsonWriter instance if available
    procedure GetJsonValue(Value: TOrm; withID: boolean; Occasion: TOrmOccasion;
      var Json: RawUtf8; LowerCaseID: boolean = false); overload;
      {$ifdef FPC} inline; {$endif} // avoid URW1111 on Delphi 2010
    /// access to a thread-safe internal cached TJsonWriter instance
    function AcquireJsonWriter(var tmp: TTextWriterStackBuffer): TJsonWriter;
      {$ifdef HASINLINE} inline; {$endif}
    /// release the thread-safe cached TJsonWriter returned by AcquireJsonWriter
    procedure ReleaseJsonWriter(WR: TJsonWriter);
      {$ifdef HASINLINE} inline; {$endif}
    /// low-level access to the current TOrm class holding a transaction
    // - equals nil outside of a TransactionBegin/Commit scope
    property TransactionTable: TOrmClass
      read fTransactionTable;
  public
    // ------- IRestOrm interface implementation methods
    // calls internally the "SELECT Count(*) FROM TableName;" SQL statement
    function TableRowCount(Table: TOrmClass): Int64; virtual;
    // calls internally a "SELECT RowID FROM TableName LIMIT 1" SQL statement,
    // which is much faster than testing if "SELECT count(*)" equals 0 - see
    // @http://stackoverflow.com/questions/8988915
    function TableHasRows(Table: TOrmClass): boolean; virtual;
    // executes by default "SELECT max(rowid) FROM TableName"
    function TableMaxID(Table: TOrmClass): TID; virtual;
    // try from cache, then from DB
    function MemberExists(Table: TOrmClass; ID: TID): boolean; virtual;
    {$ifdef ORMGENERICS}
    function RetrieveIList(T: TOrmClass; var IList;
      const CustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    function RetrieveIList(T: TOrmClass; var IList;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    {$endif ORMGENERICS}
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8): RawUtf8; overload;
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8; Default: Int64 = 0): Int64;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): RawUtf8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const): RawUtf8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      WhereID: TID): RawUtf8; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      const WhereClause: RawUtf8): boolean; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      WhereID: TID): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; out Data: TRawUtf8DynArray): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; var Data: TInt64DynArray;
      SQL: PRawUtf8 = nil): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8 = ''; const Separator: RawUtf8 = ','): RawUtf8; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUtf8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClause: RawUtf8 = ''): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8;
      const BoundsSqlWhere: array of const): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8;
      const Args, Bounds: array of const): TOrmTable; overload;
    function FTSMatch(Table: TOrmFts3Class; const WhereClause: RawUtf8;
      var DocID: TIDDynArray): boolean; overload;
    function FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUtf8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUtf8;
    function MainFieldID(Table: TOrmClass; const Value: RawUtf8): TID;
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUtf8;
      out IDs: TIDDynArray): boolean;
    function Retrieve(const SqlWhere: RawUtf8; Value: TOrm;
      const CustomFieldsCsv: RawUtf8 = ''): boolean; overload; virtual;
    function Retrieve(const WhereClauseFmt: RawUtf8;
      const Args, Bounds: array of const; Value: TOrm;
      const CustomFieldsCsv: RawUtf8 = ''): boolean; overload;
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; overload; virtual;
    function Retrieve(Reference: TRecordReference;
      ForUpdate: boolean = false): TOrm; overload;
    function Retrieve(aPublishedRecord, aValue: TOrm): boolean; overload;
    function RetrieveList(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8 = ''): TObjectList; overload;
    function RetrieveListJson(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8 = ''; aForceAjax: boolean = false): RawJson; overload;
    function RetrieveListJson(Table: TOrmClass;
      const SqlWhere: RawUtf8; const CustomFieldsCsv: RawUtf8 = '';
      aForceAjax: boolean = false): RawJson; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, CustomFieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUtf8; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const; const CustomFieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const): variant;
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8): variant;
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const CustomFieldsCsv: RawUtf8 = ''): boolean;
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const OutputFieldName: RawUtf8; W: TOrmWriter;
      const CustomFieldsCsv: RawUtf8 = '');
    function RTreeMatch(DataTable: TOrmClass;
      const DataTableBlobFieldName: RawUtf8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUtf8): TOrmTable; virtual;
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJson; virtual;
    function Execute(const aSql: RawUtf8): boolean; virtual;
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args: array of const): boolean; overload;
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args, Bounds: array of const): boolean; overload;
    function UnLock(Table: TOrmClass; aID: TID): boolean; overload; virtual; abstract;
    function UnLock(Rec: TOrm): boolean; overload;
    function Add(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomCsvFields: RawUtf8;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomFields: TFieldBits;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function AddWithBlobs(Value: TOrm;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; virtual;
    function AddSimple(aTable: TOrmClass;
      const aSimpleFields: array of const; ForcedID: TID = 0): TID;
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload; virtual;
    function Update(Value: TOrm; const CustomCsvFields: RawUtf8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(aTable: TOrmClass; aID: TID;
      const aSimpleFields: array of const): boolean; overload;
    function AddOrUpdate(Value: TOrm; ForceID: boolean = false): TID;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUtf8;
      const WhereFieldValue: array of const; const FieldName: RawUtf8;
      const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUtf8; const WhereFieldValue: variant;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    function UpdateFieldAt(Table: TOrmClass; const IDs: array of TID;
      const FieldName: RawUtf8; const FieldValue: variant): boolean;
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; Increment: Int64 = 1): boolean;
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUtf8 = nil): boolean; virtual;
    function Delete(Table: TOrmClass; ID: TID): boolean; overload; virtual;
    function Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean; overload; virtual;
    function Delete(Table: TOrmClass; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUtf8;
      out BlobData: RawBlob): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUtf8;
      out BlobStream: TCustomMemoryStream): boolean; overload; virtual;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; const BlobData: RawBlob): boolean; overload; virtual;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; BlobData: TStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; BlobData: pointer; BlobSize: integer): boolean; overload;
    function UpdateBlobFields(Value: TOrm): boolean; virtual;
    function RetrieveBlobFields(Value: TOrm): boolean; virtual;
    function TransactionBegin(aTable: TOrmClass; SessionID: cardinal): boolean; virtual;
    function TransactionActiveSession: cardinal;
    procedure Commit(SessionID: cardinal; RaiseException: boolean = false); virtual;
    procedure RollBack(SessionID: cardinal); virtual;
    procedure WriteLock;
      {$ifdef HASINLINE}inline;{$endif}
    procedure WriteUnLock;
      {$ifdef HASINLINE}inline;{$endif}
    function BatchSend(Batch: TRestBatch; var Results: TIDDynArray): integer; overload;
    function BatchSend(Batch: TRestBatch): integer; overload;
    function BatchSend(Table: TOrmClass; var Data: RawUtf8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; overload;
    function AsyncBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJson]): boolean;
    function AsyncBatchStop(Table: TOrmClass): boolean;
    function AsyncBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsyncBatchRawAdd(Table: TOrmClass; const SentData: RawUtf8): integer;
    procedure AsyncBatchRawAppend(Table: TOrmClass; SentData: TJsonWriter);
    function AsyncBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;
    function Model: TOrmModel;
      {$ifdef HASINLINE}inline;{$endif}
    function Cache: TOrmCache;
    function CacheOrNil: TOrmCache;
      {$ifdef HASINLINE}inline;{$endif}
    function CacheWorthItForTable(aTableIndex: cardinal): boolean; virtual;
    function LogClass: TSynLogClass;
      {$ifdef HASINLINE}inline;{$endif}
    function LogFamily: TSynLogFamily;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalLog(const Text: RawUtf8; Level: TSynLogInfo); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalLog(const Format: RawUtf8; const Args: array of const;
      Level: TSynLogInfo = sllTrace); overload;
    function GetServerTimestamp: TTimeLog;
      {$ifdef HASINLINE}inline;{$endif}
    function GetCurrentSessionUserID: TID; virtual;
  end;

  {$M-}

  /// a dynamic array of TRestOrm instances
  TRestOrmDynArray = array of TRestOrm;

  /// a dynamic array of TRestOrm instances, owning the instances
  TRestOrmObjArray = array of TRestOrm;


{ ************ TOrmTableWritable Read/Write TOrmTable }

type
  /// store a writable ORM result table, optionally read from a JSON message
  // - in respect to TOrmTableJson, this class allows to modify field values,
  // and add some new fields on the fly, even joined from another TOrmTable
  TOrmTableWritable = class(TOrmTableJson)
  protected
    fUpdatedValues: TRawUtf8DynArray;
    fUpdatedValuesCount: integer;
    fUpdatedRowsCount: integer;
    fUpdatedValuesInterning: TRawUtf8Interning;
    fUpdatedRows, fUpdatedRowsFields: TIntegerDynArray;
    fNoUpdateTracking: boolean;
  public
    /// modify a field value in-place, using a RawUtf8 text value
    procedure Update(Row, Field: PtrInt; const Value: RawUtf8); overload;
    /// modify a field value in-place, using a RawUtf8 text value
    procedure Update(Row: PtrInt; const FieldName, Value: RawUtf8); overload;
    /// modify a field value in-place, using a variant value
    procedure Update(Row, Field: PtrInt; const Value: variant); overload;
    /// modify a field value in-place, using a variant value
    procedure Update(Row: PtrInt; const FieldName: RawUtf8;
      const Value: variant); overload;
    /// define a new field to be stored in this table
    // - returns the internal index of the newly created field
    function AddField(const FieldName: RawUtf8): integer; overload;
    /// define a new field to be stored in this table
    // - returns the internal index of the newly created field
    function AddField(const FieldName: RawUtf8; FieldType: TOrmFieldType;
      FieldTypeInfo: pointer = nil; FieldSize: integer = -1): integer; overload;
    /// define a TOrm property to be stored as new table field
    // - returns the internal index of the newly created field
    function AddField(const FieldName: RawUtf8; FieldTable: TOrmClass;
      const FieldTableName: RawUtf8 = ''): integer; overload;
    /// append/merge data from a secondary TOrmTable
    // - you should specify the primary keys on which the data rows are merged
    // - merged data will point to From.fResults[] content: so the From instance
    // should remain available as long as you use this TOrmTableWritable
    // - warning: will call From.SortFields(FromKeyField) for faster process
    procedure Join(From: TOrmTable; const FromKeyField, KeyField: RawUtf8);
    /// append tracked Update() values to a BATCH process
    // - will only work if this table has a single associated TOrmClass
    function UpdatesToBatch(Batch: TRestBatch;
      aServerTimeStamp: TTimeLog = 0): integer;
    /// generate a JSON of tracked Update() values
    // - will only work if this table has a single associated TOrmClass
    // - BATCH-compatible JSON is possible if boOnlyObjects is not part
    // of the supplied options, e.g. as [boExtendedJson] - in this context any
    // TModTime field will be processed; set aServerTimeStamp e.g. from
    // TRestClientUri.GetServerTimestamp, if TimeLogNowUtc is not enough
    function UpdatesToJson(aOptions: TRestBatchOptions = [boOnlyObjects];
      aServerTimeStamp: TTimeLog = 0): RawJson;
    /// optionaly de-duplicate Update() values
    property UpdatedValuesInterning: TRawUtf8Interning
      read fUpdatedValuesInterning write fUpdatedValuesInterning;
    /// how many values have been written via Update() overloaded methods
    // - is not updated if UpdatedValuesInterning was defined
    property UpdatedValuesCount: integer
      read fUpdatedValuesCount;
    /// the rows numbers (1..RowCount) which have been modified by Update()
    // - Join() and AddField() are not tracked by this list - just Update()
    // - the numbers are stored in increasing order
    // - track the modified rows using UpdatedRows[0..UpdatedRowsCount - 1] and
    // UpdatedRowsFields[0..UpdatedRowsCount - 1] - unless NoUpdateTracking was set
    property UpdatedRows: TIntegerDynArray
      read fUpdatedRows;
    /// how many rows (0..RowCount) have been modified by Update()
    property UpdatedRowsCount: integer
      read fUpdatedRowsCount;
    /// 32-bit field bits which have been modified by Update()
    // - Join() and AddField() are not tracked by this list - just Update()
    // - follow UpdatedRows[0..UpdatedRowsCount - 1] row numbers
    // - if more than 32 field indexes were updated, contains 0
    property UpdatedRowsFields: TIntegerDynArray
      read fUpdatedRowsFields;
    /// if UpdatedRows/UpdatedRowsFields should not be tracked during Update()
    property NoUpdateTracking: boolean
      read fNoUpdateTracking write fNoUpdateTracking;
  end;


implementation


{ ************ TRestOrm Parent Class for abstract REST client/server }

{ TRestOrm }

// ------- TRestOrm main methods

constructor TRestOrm.Create(aRest: TRest);
begin
  inherited Create;
  fTempJsonWriter := TJsonWriter.CreateOwnedStream(16384, {nosharedstream=}true);
  if aRest = nil then
    exit;
  fRest := aRest;
  fModel := fRest.Model;
  fRest.SetOrmInstance(self); // inject this ORM instance to the main TRest
end;

constructor TRestOrm.CreateWithoutRest(aModel: TOrmModel);
begin
  fModel := aModel;
  Create(nil);
end;

destructor TRestOrm.Destroy;
begin
  FreeAndNilSafe(fCache);
  inherited Destroy;
  if (fModel <> nil) and
     (fModel.Owner = self) then
    // make sure we are the Owner (TRestStorage has fModel<>nil e.g.)
    FreeAndNilSafe(fModel);
  fTempJsonWriter.Free;
end;

function TRestOrm.SQLComputeForSelect(Table: TOrmClass;
  const FieldNames, WhereClause: RawUtf8): RawUtf8;
begin
  result := '';
  if (self = nil) or
     (Table = nil) then
    exit;
  if FieldNames = '' then
    result := fModel.Props[Table].SqlFromSelectWhere('*', WhereClause)
  else
    with Table.OrmProps do
      if FieldNames = '*' then
        result := SqlFromSelect(SqlTableName, SqlTableRetrieveAllFields, WhereClause, '')
      else if (PosExChar(',', FieldNames) = 0) and
              (PosExChar('(', FieldNames) = 0) and
              not IsFieldName(pointer(FieldNames)) then
        // prevent SQL error
        result := ''
      else
        result := SqlFromSelect(SqlTableName, FieldNames, WhereClause, '');
end;

function TRestOrm.AcquireJsonWriter(var tmp: TTextWriterStackBuffer): TJsonWriter;
begin
  if fTempJsonWriterLock.TryLock then
    result := fTempJsonWriter
  else
    result := TJsonWriter.CreateOwnedStream(tmp);
end;

procedure TRestOrm.ReleaseJsonWriter(WR: TJsonWriter);
begin
  if WR = fTempJsonWriter then
  begin
    WR.CancelAllAsNew;
    fTempJsonWriterLock.UnLock;
  end
  else
    WR.Free;
end;

procedure TRestOrm.GetJsonValue(Value: TOrm; withID: boolean;
  Occasion: TOrmOccasion; var Json: RawUtf8; LowerCaseID: boolean);
begin
  GetJsonValue(
    Value, withID, Value.Orm.SimpleFieldsBits[Occasion], Json, LowerCaseID);
end;

procedure TRestOrm.GetJsonValue(Value: TOrm; withID: boolean;
  const Fields: TFieldBits; out Json: RawUtf8; LowerCaseID: boolean);
var
  WR: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  // faster than Json := Value.GetJsonValues(true, withID, Fields);
  WR := AcquireJsonWriter(tmp);
  {$ifdef HASFASTTRYFINALLY}
  try
  {$else}
  begin
  {$endif HASFASTTRYFINALLY}
    Value.AppendAsJsonObject(WR, Fields, withID, LowerCaseID);
    WR.SetText(Json);
  {$ifdef HASFASTTRYFINALLY}
  finally
  {$endif HASFASTTRYFINALLY}
    ReleaseJsonWriter(WR);
  end;
end;

procedure TRestOrm.GetJsonValuesForAdd(TableIndex: integer; Value: TOrm;
  ForceID, DoNotAutoComputeFields, WithBlobs: boolean;
  CustomFields: PFieldBits; var result: RawUtf8);
var
  fields: TFieldBits;
  props: TOrmProperties;
begin
  if not DoNotAutoComputeFields then // update TModTime/TCreateTime fields
    Value.ComputeFieldsBeforeWrite(self, oeAdd);
  if fModel.TableProps[TableIndex].Kind in INSERT_WITH_ID then
    ForceID := true;
  if (fModel.IDGenerator <> nil) and
     (fModel.IDGenerator[TableIndex] <> nil) then
  begin
    if (Value.IDValue = 0) or
       not ForceID then
    begin
      Value.IDValue := fModel.IDGenerator[TableIndex].ComputeNew;
      ForceID := true;
    end;
  end
  else if Value.IDValue = 0 then
    ForceID := false;
  props := Value.Orm;
  if CustomFields <> nil then
    if DoNotAutoComputeFields then
      fields := CustomFields^ * props.CopiableFieldsBits
    else
      fields := CustomFields^ * props.CopiableFieldsBits + props.ComputeBeforeAddFieldsBits
  else if WithBlobs then
    fields := props.CopiableFieldsBits
  else
    fields := props.SimpleFieldsBits[ooInsert];
  if not ForceID and
     IsZero(fields) then
    result := ''
  else
    GetJsonValue(Value, ForceID, fields, result);
end;

function TRestOrm.InternalAdd(Value: TOrm; SendData: boolean;
  CustomFields: PFieldBits; ForceID, DoNotAutoComputeFields: boolean): TID;
var
  json: RawUtf8;
  t: integer;
begin
  if Value = nil then
  begin
    result := 0;
    exit;
  end;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  if SendData then
    GetJsonValuesForAdd(t, Value, ForceID, DoNotAutoComputeFields,
      false, CustomFields, json)
  else
    json := '';
  // on success, returns the new RowID value; on error, returns 0
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    // may be within a batch in another thread -> use execOrmWrite lock
    result := EngineAdd(t, json); // will call static if necessary
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
  // on success, Value.ID is updated with the new RowID
  Value.IDValue := result;
  if (result <> 0) and
     SendData then
    fCache.NotifyAllFields(t, Value);
end;


// ------- IRestOrm interface implementation methods

function TRestOrm.Model: TOrmModel;
begin
  result := fModel;
end;

function TRestOrm.CacheOrNil: TOrmCache;
begin
  result := fCache;
end;

function TRestOrm.LogClass: TSynLogClass;
begin
  result := fRest.LogClass;
end;

function TRestOrm.LogFamily: TSynLogFamily;
begin
  result := fRest.LogFamily;
end;

procedure TRestOrm.InternalLog(const Text: RawUtf8; Level: TSynLogInfo);
begin
  fRest.InternalLog(Text, Level);
end;

function TRestOrm.GetServerTimestamp: TTimeLog;
begin
  result := fRest.GetServerTimeStamp(0);
end;

procedure TRestOrm.WriteLock;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
end;

procedure TRestOrm.WriteUnLock;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
end;

{$ifdef ORMGENERICS}

function TRestOrm.RetrieveIList(T: TOrmClass; var IList;
  const CustomFieldsCsv: RawUtf8): boolean;
begin
  result := RetrieveIList(T, IList, '', [], CustomFieldsCsv);
end;

function TRestOrm.RetrieveIList(T: TOrmClass; var IList;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const CustomFieldsCsv: RawUtf8): boolean;
var
  table: TOrmTable;
begin
  result := false;
  IInterface(IList) := nil;
  if self = nil then
    exit;
  table := MultiFieldValues(T, CustomFieldsCsv, FormatSqlWhere, BoundsSqlWhere);
  if table <> nil then
    try
      table.ToNewIList(T, IList);
      result := true;
    finally
      table.Free;
    end;
end;

{$endif ORMGENERICS}

function TRestOrm.TableRowCount(Table: TOrmClass): Int64;
var
  T: TOrmTable;
begin
  if (self = nil) or
     (Table = nil) then
    T := nil
  else
    T := ExecuteList([Table], 'SELECT Count(*) FROM ' +
      Table.OrmProps.SqlTableName);
  if T <> nil then
  try
    result := T.GetAsInt64(1, 0);
  finally
    T.Free;
  end
  else
    result := -1;
end;

function TRestOrm.TableHasRows(Table: TOrmClass): boolean;
var
  T: TOrmTable;
begin
  if (self = nil) or
     (Table = nil) then
    T := nil
  else
    T := ExecuteList([Table], 'SELECT RowID FROM ' +
      Table.OrmProps.SqlTableName + ' LIMIT 1');
  if T <> nil then
  try
    result := T.RowCount > 0;
  finally
    T.Free;
  end
  else
    result := false;
end;

function TRestOrm.TableMaxID(Table: TOrmClass): TID;
var
  T: TOrmTable;
begin
  if (self = nil) or
     (Table = nil) then
    T := nil
  else
    T := ExecuteList([Table], 'SELECT max(RowID) FROM ' +
      Table.OrmProps.SqlTableName);
  if T <> nil then
  try
    result := T.GetAsInt64(1, 0);
  finally
    T.Free;
  end
  else
    result := -1;
end;

function TRestOrm.MemberExists(Table: TOrmClass; ID: TID): boolean;
var
  t: PtrInt;
begin
  t := fModel.GetTableIndexExisting(Table);
  if fCache.Exists(t, ID) then
    result := true
  else
    result := EngineRetrieve(t, ID) <> ''; // try from DB
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName,
  WhereClause: RawUtf8): RawUtf8;
var
  res: array[0..0] of RawUtf8;
begin
  if MultiFieldValue(Table, [FieldName], res, WhereClause) then
    result := res[0]
  else
    result := '';
end;

function TRestOrm.OneFieldValueInt64(Table: TOrmClass; const FieldName,
  WhereClause: RawUtf8; Default: Int64): Int64;
var
  res: array[0..0] of RawUtf8;
begin
  if not MultiFieldValue(Table, [FieldName], res, WhereClause) or
     not ToInt64(res[0], result) then
    result := Default;
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): RawUtf8;
begin
  result := OneFieldValue(Table, FieldName,
    FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere));
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const): RawUtf8;
begin
  result := OneFieldValue(Table, FieldName,
    FormatUtf8(WhereClauseFmt, Args, Bounds));
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const;
  out Data: Int64): boolean;
var
  res: array[0..0] of RawUtf8;
  err: integer;
  where: RawUtf8;
begin
  result := false;
  where := FormatUtf8(WhereClauseFmt, Args, Bounds);
  if MultiFieldValue(Table, [FieldName], res, where) then
    if res[0] <> '' then
    begin
      Data := GetInt64(pointer(res[0]), err);
      if err = 0 then
        result := true;
    end;
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  WhereID: TID): RawUtf8;
var
  res: array[0..0] of RawUtf8;
begin
  if (WhereID > 0) and
     MultiFieldValue(Table, [FieldName], res,
       'RowID=:(' + Int64ToUtf8(WhereID) + '):') then
    result := res[0]
  else
    result := '';
end;

function TRestOrm.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
  const WhereClause: RawUtf8): boolean;
var
  sql, where: RawUtf8;
  v, f: PtrInt;
  T: TOrmTable;
begin
  result := false;
  if (self <> nil) and
     (Table <> nil) and
     (high(FieldName) = high(FieldValue)) then
    with Table.OrmProps do
    begin
      where := SqlTableName + SqlFromWhere(WhereClause);
      if (high(FieldName) = 0) and
         IdemPChar(pointer(FieldName[0]), 'COUNT(*)') then
        sql := 'SELECT COUNT(*) FROM ' + where
      else
      begin
        for f := 0 to high(FieldName) do
          if not IsFieldNameOrFunction(FieldName[f]) then
            // prevent sql error or security breach
            exit
          else if sql = '' then
            sql := 'SELECT ' + FieldName[f]
          else
            sql := sql + ',' + FieldName[f];
        sql := sql + ' FROM ' + where + ' LIMIT 1';
      end;
      T := ExecuteList([Table], sql);
      if T <> nil then
      try
        if (T.FieldCount <> length(FieldName)) or
           (T.RowCount <= 0) then
          exit;
        // get field values from the first (and unique) row
        for f := 0 to high(FieldName) do
        begin
          v := f; // regular SQL SELECT order by default
          if (high(FieldName) <> 0) and
             not IdemPropNameU(FieldName[f], T.Results[f], T.ResultsLen[f]) then
          begin
            // RowID/ID or wrong order (e.g. TRestStorageInMemory = TOrm order)
            v := T.FieldIndex(pointer(FieldName[f]));
            if v < 0 then
              if IsSqlFunction(pointer(FieldName[f])) then
                v := f // function result could be renamed but in-order
              else
                exit;  // something went wrong
          end;
          inc(v, T.FieldCount); // value offset
          FastSetString(FieldValue[f], T.Results[v], T.ResultsLen[v]);
        end;
        result := true;
      finally
        T.Free;
      end;
    end;
end;

function TRestOrm.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
  WhereID: TID): boolean;
begin
  result := MultiFieldValue(Table, FieldName, FieldValue,
    'RowID=:(' + Int64ToUtf8(WhereID) + '):');
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUtf8;
  out Data: TRawUtf8DynArray): boolean;
var
  T: TOrmTable;
begin
  result := false;
  T := MultiFieldValues(Table, FieldName, WhereClause);
  if T <> nil then
  try
    result := T.GetRowValues(0, Data) > 0;
  finally
    T.Free;
  end;
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUtf8; var Data: TInt64DynArray;
  SQL: PRawUtf8): boolean;
var
  T: TOrmTable;
  V: Int64;
  P: PUtf8Char;
  field: shortstring;
begin
  Data := nil;
  // handle naive expressions like SELECT ID from Table where ID=10
  if IsRowID(pointer(FieldName)) and
     (length(WhereClause) > 2) then
  begin
    P := pointer(WhereClause);
    GetNextFieldPropSameLine(P, field);
    if IsRowIDShort(field) and
       (StrPosI('AND', P) = nil) and
       (StrPosI('OR', P) = nil) then
      case P^ of
        '=':
          begin
            // SELECT RowID from Table where RowID=10
            P := GotoNextNotSpace(P + 1);
            if PWord(P)^ = ord(':') + ord('(') shl 8 then
              inc(P, 2); // handle inlined parameters
            SetInt64(P, V);
            if V > 0 then
            begin
              SetLength(Data, 1);
              Data[0] := V;
              result := true;
              exit;
            end;
          end;
        'i',
        'I':
          if P[1] in ['n', 'N'] then
          begin
            // SELECT RowID from Table where RowID in [1,2,3]
            P := GotoNextNotSpace(P + 2);
            if (P^ = '(') and
               (GotoNextNotSpace(P + 1)^ in ['0'..'9']) then
            begin
              CsvToInt64DynArray(P + 1, Data);
              if Data <> nil then
              begin
                result := true;
                exit;
              end;
            end;
          end;
      end;
  end;
  // retrieve the content from database
  result := false;
  T := MultiFieldValues(Table, FieldName, WhereClause);
  if T <> nil then
  try
    if (T.FieldCount <> 1) or
       (T.RowCount <= 0) then
      exit;
    T.GetRowValues(0, Data);
    if SQL <> nil then
      SQL^ := T.QuerySql;
    result := true;
  finally
    T.Free;
  end;
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause, Separator: RawUtf8): RawUtf8;
var
  i, Len, SepLen, L: PtrInt;
  T: TOrmTable;
  P: PUtf8Char;
begin
  result := '';
  T := MultiFieldValues(Table, FieldName, WhereClause);
  if T <> nil then
  try
    if (T.FieldCount <> 1) or
       (T.RowCount <= 0) then
      exit;
    // calculate row values CSV needed memory
    SepLen := length(Separator);
    Len := SepLen * (T.RowCount - 1);
    for i := 1 to T.RowCount do
      inc(Len, T.ResultsLen[i]); // ignore Results[0] i.e. field name
    SetLength(result, Len);
    // add row values as CSV
    P := pointer(result);
    i := 1;
    repeat
      L := T.ResultsLen[i];
      if L <> 0 then
      begin
        MoveFast(T.Results[i]^, P^, L);
        inc(P, L);
      end;
      if i = T.RowCount then
        break;
      if SepLen <> 0 then
      begin
        MoveByOne(pointer(Separator), P, SepLen);
        inc(P, SepLen);
      end;
      inc(i);
    until false;
    //assert(P-pointer(result)=Len);
  finally
    T.Free;
  end;
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUtf8; Strings: TStrings;
  IDToIndex: PID): boolean;
var
  Row: integer;
  aID: TID;
  T: TOrmTable;
begin
  result := false;
  if (Strings <> nil) and
     (self <> nil) and
     (Table <> nil) then
  try
    Strings.BeginUpdate;
    Strings.Clear;
    T := ExecuteList([Table], SqlFromSelect(Table.SqlTableName,
      'RowID,' + FieldName, WhereClause, ''));
    if T <> nil then
    try
      if (T.FieldCount = 2) and
         (T.RowCount > 0) then
      begin
        for Row := 1 to T.RowCount do
        begin
          // ignore Row 0 i.e. field names
          aID := GetInt64(T.Get(Row, 0));
          Strings.AddObject(T.GetString(Row, 1), pointer(PtrInt(aID)));
          if (IDToIndex <> nil) and
             (aID = IDToIndex^) then
          begin
            IDToIndex^ := TID(Row) - 1;
            IDToIndex := nil; // set once
          end;
        end;
        result := true;
      end;
    finally
      T.Free;
    end;
  finally
    Strings.EndUpdate;
  end;
  if IDToIndex <> nil then
    IDToIndex^ := -1; // ID not found
end;

function TRestOrm.MultiFieldValues(Table: TOrmClass;
  const FieldNames, WhereClause: RawUtf8): TOrmTable;
var
  sql: RawUtf8;
begin
  sql := SQLComputeForSelect(Table, FieldNames, WhereClause);
  if sql = '' then
    result := nil
  else
    result := ExecuteList([Table], sql);
end;

function TRestOrm.MultiFieldValues(Table: TOrmClass;
  const FieldNames, WhereClauseFormat: RawUtf8;
  const BoundsSqlWhere: array of const): TOrmTable;
var
  where: RawUtf8;
begin
  where := FormatUtf8(WhereClauseFormat, [], BoundsSqlWhere);
  result := MultiFieldValues(Table, FieldNames, where);
end;

function TRestOrm.MultiFieldValues(Table: TOrmClass;
  const FieldNames, WhereClauseFormat: RawUtf8;
  const Args, Bounds: array of const): TOrmTable;
var
  where: RawUtf8;
begin
  where := FormatUtf8(WhereClauseFormat, Args, Bounds);
  result := MultiFieldValues(Table, FieldNames, where);
end;

function TRestOrm.FTSMatch(Table: TOrmFts3Class;
  const WhereClause: RawUtf8; var DocID: TIDDynArray): boolean;
begin
  // FTS3 tables don't have any ID, but RowID or DocID
  result := OneFieldValues(Table, 'RowID', WhereClause, TInt64DynArray(DocID));
end;

function TRestOrm.FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUtf8;
  var DocID: TIDDynArray; const PerFieldWeight: array of double;
  limit: integer; offset: integer): boolean;
var
  WhereClause: RawUtf8;
  i: PtrInt;
begin
  result := false;
  with Table.OrmProps do
    if length(PerFieldWeight) <> length(SimpleFields) then
      exit
    else
      WhereClause := FormatUtf8('% MATCH ? ORDER BY rank(matchinfo(%)',
        [SqlTableName, SqlTableName], [MatchClause]);
  for i := 0 to high(PerFieldWeight) do
    WhereClause := FormatUtf8('%,?', [WhereClause], [PerFieldWeight[i]]);
  WhereClause := WhereClause + ') DESC';
  if limit > 0 then
    WhereClause := FormatUtf8('% LIMIT % OFFSET %', [WhereClause, limit, offset]);
  result := FTSMatch(Table, WhereClause, DocID);
end;

function TRestOrm.MainFieldValue(Table: TOrmClass; ID: TID;
  ReturnFirstIfNoUnique: boolean): RawUtf8;
begin
  if (self = nil) or
     (Table = nil) or
     (ID <= 0) then
    result := ''
  else
  begin
    result := Table.OrmProps.MainFieldName(ReturnFirstIfNoUnique);
    if result <> '' then
      result := OneFieldValue(Table, result, ID);
  end;
end;

function TRestOrm.MainFieldID(Table: TOrmClass; const Value: RawUtf8): TID;
var
  main: integer;
begin
  result := 0;
  if (self <> nil) and
     (Value <> '') and
     (Table <> nil) then
    with Table.OrmProps do
    begin
      main := MainField[false];
      if main >= 0 then
        SetID(OneFieldValue(Table, 'RowID', fields.List[main].Name +
          '=:(' + QuotedStr(Value, '''') + '):'), result);
    end;
end;

function TRestOrm.MainFieldIDs(Table: TOrmClass;
  const Values: array of RawUtf8; out IDs: TIDDynArray): boolean;
var
  main, id: TID;
begin
  if (self <> nil) and
     (high(Values) >= 0) and
     (Table <> nil) then
    if high(Values) = 0 then
    begin
      // handle special case of one Values[] item
      id := MainFieldID(Table, Values[0]);
      if id > 0 then
      begin
        SetLength(IDs, 1);
        IDs[0] := id;
      end;
    end
    else
      with Table.OrmProps do
      begin
        // request all Values[] IDs at once
        main := MainField[false];
        if main >= 0 then
          OneFieldValues(Table, 'RowID',
            SelectInClause(fields.List[main].Name, Values), TInt64DynArray(IDs));
      end;
  result := {%H-}IDs <> nil;
end;

function TRestOrm.Retrieve(const SqlWhere: RawUtf8; Value: TOrm;
  const CustomFieldsCsv: RawUtf8): boolean;
var
  T: TOrmTable;
  sql: RawUtf8;
begin
  result := false;
  if (self = nil) or
     (Value = nil) then
    exit;
  sql := TrimU(SqlWhere);
  if not EndWith(sql, ' LIMIT 1') then
    sql := sql + ' LIMIT 1'; // we keep a single record below
  T := MultiFieldValues(POrmClass(Value)^, CustomFieldsCsv, sql);
  if T <> nil then
  try
    if T.RowCount >= 1 then
    begin
      Value.FillFrom(T, 1); // fetch data from first result row
      result := true;
    end
    else
      Value.IDValue := 0;
  finally
    T.Free;
  end;
end;

function TRestOrm.Retrieve(const WhereClauseFmt: RawUtf8;
  const Args, Bounds: array of const; Value: TOrm;
  const CustomFieldsCsv: RawUtf8): boolean;
var
  where: RawUtf8;
begin
  where := FormatUtf8(WhereClauseFmt, Args, Bounds);
  result := Retrieve(where, Value, CustomFieldsCsv);
end;

function TRestOrm.Retrieve(aID: TID; Value: TOrm; ForUpdate: boolean): boolean;
var
  t: integer; // used by EngineRetrieve() for SQL statement caching
  resp: RawUtf8;
begin
  // check parameters
  result := false;
  if Value = nil then
    exit; // avoid GPF
  Value.IDValue := 0;
  if (self = nil) or
     (aID = 0) then
    exit;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  // try to lock before retrieval (if ForUpdate)
  if ForUpdate and
     not fModel.Lock(t, aID) then
    exit;
  // try to retrieve existing record from internal cache
  result := fCache.Retrieve(aID, Value, t);
  if result then
    exit;
  // get JSON object '{...}' in resp from corresponding EngineRetrieve() method
  resp := EngineRetrieve(t, aID);
  if resp = '' then
  begin
    fCache.NotifyDeletion(t, aID); // ensure there is no cache for this ID
    exit;
  end;
  // fill Value from JSON if was correctly retrieved
  Value.FillFrom(resp);
  Value.IDValue := aID; // resp may not contain the "RowID": field
  if not ForUpdate then
    fCache.NotifyAllFields(t, Value);
  result := true;
end;

function TRestOrm.Retrieve(Reference: TRecordReference; ForUpdate: boolean): TOrm;
var
  c: TOrmClass;
begin
  result := nil;
  if (self = nil) or
     (RecordRef(Reference).ID = 0) then
    exit;
  c := RecordRef(Reference).Table(fModel);
  if c = nil then
    exit;
  result := c.Create(self, RecordRef(Reference).ID, ForUpdate);
  if result.IDValue = 0 then
    FreeAndNil(result); // error during value retrieval
end;

function TRestOrm.Retrieve(aPublishedRecord, aValue: TOrm): boolean;
begin
  result := Retrieve(aPublishedRecord.ID, aValue);
end;

function TRestOrm.RetrieveList(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const CustomFieldsCsv: RawUtf8): TObjectList;
var
  T: TOrmTable;
begin
  result := nil;
  if (self = nil) or
     (Table = nil) then
    exit;
  T := MultiFieldValues(Table, CustomFieldsCsv, FormatSqlWhere, BoundsSqlWhere);
  if T <> nil then
  try
    result := TObjectList.Create;
    T.ToObjectList(result, Table);
  finally
    T.Free;
  end;
end;

function TRestOrm.RetrieveListJson(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const CustomFieldsCsv: RawUtf8; aForceAjax: boolean): RawJson;
var
  where: RawUtf8;
begin
  where := FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere);
  result := RetrieveListJson(Table, where, CustomFieldsCsv, aForceAjax)
end;

function TRestOrm.RetrieveListJson(Table: TOrmClass;
  const SqlWhere: RawUtf8; const CustomFieldsCsv: RawUtf8;
  aForceAjax: boolean): RawJson;
var
  sql: RawUtf8;
begin
  sql := SQLComputeForSelect(Table, CustomFieldsCsv, SqlWhere);
  if sql = '' then
    result := ''
  else
    result := EngineList(sql, aForceAjax);
end;

function TRestOrm.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName, CustomFieldsCsv: RawUtf8;
  FirstRecordID, LastRecordID: PID): variant;
begin
  result := RetrieveDocVariantArray(Table, ObjectName, '', [], CustomFieldsCsv,
    FirstRecordID, LastRecordID);
end;

function TRestOrm.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName: RawUtf8; const FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const; const CustomFieldsCsv: RawUtf8;
  FirstRecordID, LastRecordID: PID): variant;
var
  T: TOrmTable;
  v: variant;
begin
  TVarData(v).VType := varNull;
  if (self <> nil) and
     (Table <> nil) then
  begin
    T := MultiFieldValues(Table, CustomFieldsCsv, FormatSqlWhere, BoundsSqlWhere);
    if T <> nil then
    try
      T.ToDocVariant(v, {readonly=}false); // not readonly -> TDocVariant dvArray
      if FirstRecordID <> nil then
        FirstRecordID^ := T.GetID(1);
      if LastRecordID <> nil then
        LastRecordID^ := T.GetID(T.RowCount);
    finally
      T.Free;
    end;
  end;
  if ObjectName <> '' then
    result := _ObjFast([ObjectName, v])
  else
    result := v;
end;

function TRestOrm.RetrieveOneFieldDocVariantArray(Table: TOrmClass;
  const FieldName, FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const): variant;
var
  T: TOrmTable;
  row: integer;
  doc: TDocVariantData absolute result;
begin
  VarClear(result);
  if (self <> nil) and
     (Table <> nil) then
  begin
    T := MultiFieldValues(Table, FieldName, FormatSqlWhere, BoundsSqlWhere);
    if T <> nil then
    try
      doc.InitFast(T.RowCount, dvArray);
      doc.SetCount(T.RowCount);
      for row := 1 to T.RowCount do
        T.GetAsVariant(row, 0, doc.Values[row - 1], false, false, false,
          JSON_FAST);
    finally
      T.Free;
    end;
  end;
end;

function TRestOrm.RetrieveDocVariant(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const CustomFieldsCsv: RawUtf8): variant;
var
  T: TOrmTable;
  bits: TFieldBits;
  Rec: TOrm;
  ID: TID;
begin
  SetVariantNull(result);
  if (self <> nil) and
     (Table <> nil) then
  begin
    with Table.OrmProps do
      // handle optimized primary key direct access
      if fCache.IsCached(Table) and
         (length(BoundsSqlWhere) = 1) and
         VarRecToInt64(BoundsSqlWhere[0], Int64(ID)) and
         FieldBitsFromCsv(CustomFieldsCsv, bits) and
         (IdemPropNameU('RowID=?', FormatSqlWhere) or
          IdemPropNameU('ID=?', FormatSqlWhere)) then
      begin
        if IsZero(bits) then
          // get all simple fields if none supplied, like MultiFieldValues()
          bits := SimpleFieldsBits[ooSelect];
        if bits - SimpleFieldsBits[ooSelect] = [] then // only simple fields
        begin
          Rec := Table.Create(self, ID); // we can use the cache
          try
            Rec.GetAsDocVariant(true, bits, result, nil, {"id"=}true);
          finally
            Rec.Free;
          end;
          exit;
        end;
      end;
    T := MultiFieldValues(Table, CustomFieldsCsv, FormatSqlWhere, BoundsSqlWhere);
    if T <> nil then
    try
      T.ToDocVariant(1, result)
    finally
      T.Free;
    end;
  end;
end;

function TRestOrm.RetrieveListObjArray(var ObjArray; Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const CustomFieldsCsv: RawUtf8): boolean;
var
  T: TOrmTable;
begin
  result := false;
  if (self = nil) or
     (Table = nil) then
    exit;
  T := MultiFieldValues(Table, CustomFieldsCsv, FormatSqlWhere, BoundsSqlWhere);
  if T <> nil then
  try
    result := T.ToObjArray(ObjArray, Table);
  finally
    T.Free;
  end;
end;

procedure TRestOrm.AppendListAsJsonArray(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const OutputFieldName: RawUtf8; W: TOrmWriter; const CustomFieldsCsv: RawUtf8);
var
  Rec: TOrm;
begin
  if (self = nil) or
     (Table = nil) or
     (W = nil) then
    exit;
  Rec := Table.CreateAndFillPrepare(Self, FormatSqlWhere, BoundsSqlWhere,
    CustomFieldsCsv);
  try
    Rec.AppendFillAsJsonArray(OutputFieldName, W, Rec.FillContext.TableMapFields);
  finally
    Rec.Free;
  end;
end;

function TRestOrm.RTreeMatch(DataTable: TOrmClass;
  const DataTableBlobFieldName: RawUtf8; RTreeTable: TOrmRTreeClass;
  const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
var
  Blob: PRttiProp;
  T: TOrmTable;
  BDouble: TOrmTreeCoords;
  BInteger: TOrmTreeCoordsInteger absolute BDouble;
  Where, SQL: RawUtf8;
  Data, RTree: TOrmProperties;
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (DataTable = nil) or
     (RTreeTable = nil) or
     (DataTableBlobField = '') then
    exit;
  RTree := RTreeTable.OrmProps;
  Data := DataTable.OrmProps;
  Blob := Data.BlobFieldPropFromRawUtf8(DataTableBlobFieldName);
  if Blob = nil then
    exit;
  if RTreeTable.InheritsFrom(TOrmRTree) then
  begin
    TOrmRTree(RTreeTable).BlobToCoord(pointer(DataTableBlobField)^, BDouble);
    for i := 0 to (RTree.RTreeCoordBoundaryFields shr 1) - 1 do
      Where := FormatUtf8('%%>=:(%): and %<=:(%): and ', [{%H-}Where,
        RTree.Fields.List[i * 2].Name, BDouble[i].Min * (1 - 0.00000012),
        RTree.Fields.List[i * 2+ 1].Name, BDouble[i].Max * (1 + 0.00000012)]);
    { from http://sqlite.org/rtree.html:
      For a "contained-within" style query, rounding the bounding boxes outward
      might cause some entries to be excluded from the result set if the edge of
      the entry bounding box corresponds to the edge of the query bounding box.
      To guard against this, applications should expand their contained-within
      query boxes slightly (by 0.000012%) by rounding down the lower coordinates
      and rounding up the top coordinates, in each dimension. }
  end
  else if RTreeTable.InheritsFrom(TOrmRTreeInteger) then
  begin
    TOrmRTreeInteger(RTreeTable).BlobToCoord(
      pointer(DataTableBlobField)^, BInteger);
    for i := 0 to (RTree.RTreeCoordBoundaryFields shr 1) - 1 do
      Where := FormatUtf8('%%>=:(%): and %<=:(%): and ', [Where,
        RTree.Fields.List[i * 2].Name, BInteger[i].Min,
        RTree.Fields.List[i * 2 + 1].Name, BInteger[i].Max]);
  end
  else
    exit;
  FormatUtf8('select %.RowID from %,% where %.RowID=%.RowID and %%(%,:(%):);',
    [RTree.SqlTableName, Data.SqlTableName, RTree.SqlTableName,
     Data.SqlTableName, Where, RTreeTable.RTreeSQLFunctionName,
     Data.SqlTableName, BinToBase64WithMagic(DataTableBlobField)], SQL);
  T := ExecuteList([DataTable, RTreeTable], SQL);
  if T <> nil then
  try
    if (T.FieldCount <> 1) or
       (T.RowCount <= 0) then
      exit;
    T.GetRowValues(0, TInt64DynArray(DataID));
    result := true;
  finally
    T.Free;
  end;
end;

function TRestOrm.ExecuteList(const Tables: array of TOrmClass;
  const SQL: RawUtf8): TOrmTable;
var
  json: RawUtf8;
begin
  json := EngineList(SQL, false);
  if json <> '' then
    result := TOrmTableJson.CreateFromTables(Tables, SQL, json,
      {ownjson=}PStrCnt(PAnsiChar(pointer(json)) - _STRCNT)^ = 1)
  else
    result := nil;
end;

function TRestOrm.ExecuteJson(const Tables: array of TOrmClass;
  const SQL: RawUtf8; ForceAjax: boolean; ReturnedRowCount: PPtrInt): RawJson;
begin
  result := EngineList(SQL, ForceAjax, ReturnedRowCount);
end;

function TRestOrm.Execute(const aSql: RawUtf8): boolean;
begin
  result := EngineExecute(aSql);
end;

function TRestOrm.ExecuteFmt(const SqlFormat: RawUtf8;
  const Args: array of const): boolean;
var
  SQL: RawUtf8;
begin
  SQL := FormatUtf8(SqlFormat, Args);
  result := EngineExecute(SQL);
end;

function TRestOrm.ExecuteFmt(const SqlFormat: RawUtf8;
  const Args, Bounds: array of const): boolean;
var
  SQL: RawUtf8;
begin
  SQL := FormatUtf8(SqlFormat, Args, Bounds);
  result := EngineExecute(SQL);
end;

function TRestOrm.UnLock(Rec: TOrm): boolean;
begin
  if (self = nil) or
     (Rec = nil) or
     (Rec.IDValue <= 0) then
    result := false
  else
    result := UnLock(POrmClass(Rec)^, Rec.IDValue);
end;

function TRestOrm.Add(Value: TOrm; SendData, ForceID,
  DoNotAutoComputeFields: boolean): TID;
begin
  result := InternalAdd(Value, SendData, nil, ForceID, DoNotAutoComputeFields);
end;

function TRestOrm.Add(Value: TOrm; const CustomCsvFields: RawUtf8;
  ForceID, DoNotAutoComputeFields: boolean): TID;
var
  f: TFieldBits;
begin
  with Value.Orm do
    if CustomCsvFields = '*' then
      // FieldBitsFromCsv('*') will use [ooSelect]
      f := SimpleFieldsBits[ooInsert]
    else if not FieldBitsFromCsv(CustomCsvFields, f) then
    begin
      result := 0; // one of the csv field name is invalid
      exit;
    end;
  result := InternalAdd(Value, true, @f, ForceID, DoNotAutoComputeFields);
end;

function TRestOrm.Add(Value: TOrm; const CustomFields: TFieldBits;
  ForceID, DoNotAutoComputeFields: boolean): TID;
begin
  result := InternalAdd(Value, true, @CustomFields, ForceID, DoNotAutoComputeFields);
end;

function TRestOrm.AddWithBlobs(Value: TOrm; ForceID: boolean;
  DoNotAutoComputeFields: boolean): TID;
var
  t: integer;
  json: RawUtf8;
begin
  if Value = nil then
  begin
    result := 0;
    exit;
  end;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  GetJsonValuesForAdd(t, Value, ForceID, DoNotAutoComputeFields,
    true, nil, json);
  // on success, returns the new RowID value; on error, returns 0
  WriteLock;
  try
    // may be within a batch in another thread
    result := EngineAdd(t, json); // will call static if necessary
  finally
    WriteUnLock;
  end;
  // on success, Value.ID is updated with the new RowID
  Value.IDValue := result;
  // here fCache.Notify is not called, since the JsonValues is verbose
end;

function TRestOrm.AddSimple(aTable: TOrmClass;
  const aSimpleFields: array of const; ForcedID: TID): TID;
var
  Value: TOrm;
begin
  result := 0; // means error
  if (self = nil) or
     (aTable = nil) then
    exit;
  Value := aTable.Create;
  try
    if Value.SimplePropertiesFill(aSimpleFields) then
    begin
      if ForcedID <> 0 then
        Value.IDValue := ForcedID;
      result := Add(Value, true, (ForcedID <> 0));
    end;
  finally
    Value.Free;
  end;
end;

function TRestOrm.Update(Value: TOrm; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): boolean;
var
  json: RawUtf8;
  t: integer;
  bits: TFieldBits;
begin
  if (self = nil) or
     (Value = nil) or
     (Value.IDValue = 0) or
     not RecordCanBeUpdated(POrmClass(Value)^, Value.IDValue, oeUpdate) then
  begin
    result := false; // current user don't have enough right to update this record
    exit;
  end;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  if not DoNotAutoComputeFields then
    Value.ComputeFieldsBeforeWrite(self, oeUpdate); // update TModTime fields
  if IsZero(CustomFields) then
    if (Value.FillContext <> nil) and
       (Value.FillContext.Table <> nil) and
       (Value.FillContext.TableMapRecordManyInstances = nil) then
      // within FillPrepare/FillOne loop: update ID, TModTime and mapped fields
      bits := Value.FillContext.TableMapFields + Value.Orm.FieldBits[oftModTime]
    else
      // update all simple/custom fields (also for FillPrepareMany)
      bits := Value.Orm.SimpleFieldsBits[ooUpdate]
  else
    // CustomFields<>[] -> update specified (and TModTime fields)
    if DoNotAutoComputeFields then
      bits := CustomFields
    else
      bits := CustomFields + Value.Orm.FieldBits[oftModTime];
  if IsZero(bits) then
  begin
    result := true; // a TOrm with NO simple fields (e.g. ID/blob pair)
    exit;
  end;
  GetJsonValue(Value, {withid=}false, bits, json);
  WriteLock;
  try
    // may be within a batch in another thread
    result := EngineUpdate(t, Value.IDValue, json);
  finally
    WriteUnLock;
  end;
  if result then
    fCache.NotifyUpdate(t, Value, bits);
end;

function TRestOrm.Update(Value: TOrm; const CustomCsvFields: RawUtf8;
  DoNotAutoComputeFields: boolean): boolean;
var
  bits: TFieldBits;
begin
  if (self = nil) or
     (Value = nil) or
     not Value.Orm.FieldBitsFromCsv(CustomCsvFields, bits) then
    result := false
  else
    result := Update(Value, bits, DoNotAutoComputeFields);
end;

function TRestOrm.Update(aTable: TOrmClass; aID: TID;
  const aSimpleFields: array of const): boolean;
var
  Value: TOrm;
begin
  result := false; // means error
  if (self = nil) or
     (aTable = nil) or
     (aID = 0) then
    exit;
  Value := aTable.Create;
  try
    if not Value.SimplePropertiesFill(aSimpleFields) then
      exit;
    Value.IDValue := aID;
    result := Update(Value);
  finally
    Value.Free;
  end;
end;

function TRestOrm.AddOrUpdate(Value: TOrm; ForceID: boolean): TID;
begin
  if (self = nil) or
     (Value = nil) then
  begin
    result := 0;
    exit;
  end;
  WriteLock; // make this atomic
  try
    if ForceID or
       (Value.IDValue = 0) then
    begin
      result := Add(Value, true, ForceID);
      if (result <> 0) or
         (Value.IDValue = 0) then
        exit;
    end;
    if Update(Value) then
      result := Value.IDValue
    else
      result := 0;
  finally
    WriteUnlock;
  end;
end;

function TRestOrm.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; const FieldValue: array of const): boolean;
var
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := UpdateField(Table, 'RowID', [ID], FieldName, FieldValue);
  if result then
    fCache.NotifyDeletion(t, ID);
end;

function TRestOrm.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUtf8; const WhereFieldValue: array of const;
  const FieldName: RawUtf8; const FieldValue: array of const): boolean;
var
  SetValue, WhereValue: RawUtf8;
begin
  result := false;
  if (length(FieldValue) <> 1) or
     (WhereFieldName = '') or
     (length(WhereFieldValue) <> 1) then
    exit;
  VarRecToInlineValue(WhereFieldValue[0], WhereValue);
  VarRecToInlineValue(FieldValue[0], SetValue);
  result := EngineUpdateField(fModel.GetTableIndexExisting(Table),
    FieldName, SetValue, WhereFieldName, WhereValue);
  // warning: this may not update the internal cache
end;

function TRestOrm.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
var
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := UpdateField(Table, 'RowID', ID, FieldName, FieldValue);
  if result then
    fCache.NotifyDeletion(t, ID);
end;

function TRestOrm.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUtf8; const WhereFieldValue: variant;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
var
  value, where: RawUtf8;
begin
  VariantToInlineValue(WhereFieldValue, where);
  VariantToInlineValue(FieldValue, value);
  result := EngineUpdateField(fModel.GetTableIndexExisting(Table),
    FieldName, value, WhereFieldName, where);
  // warning: this may not update the internal cache
end;

function TRestOrm.UpdateFieldAt(Table: TOrmClass; const IDs: array of TID;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
var
  value, where: RawUtf8;
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  VariantToInlineValue(FieldValue, value);
  where := SelectInClause('RowID', IDs, '', INLINED_MAX);
  if length(IDs) <= INLINED_MAX then
    result := ExecuteFmt('update % set %=:(%): where %',
      [Table.SqlTableName, FieldName, value, where])
  else
    // don't cache such a statement
    result := ExecuteFmt('update % set %=% where %',
      [Table.SqlTableName, FieldName, value, where]);
  if result then
    fCache.NotifyDeletions(t, IDs);
end;

function TRestOrm.EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
  const FieldName: RawUtf8; Increment: Int64): boolean;
var
  v: Int64;
  t: TOrmClass;
begin
  if (TableModelIndex < 0) or
     (ID < 0) then
    result := false
  else if Increment = 0 then
    result := true
  else
  begin
    t := fModel.Tables[TableModelIndex];
    result := OneFieldValue(t, FieldName, 'ID=?', [], [ID], v) and
              UpdateField(t, ID, FieldName, [v + Increment]);
  end;
end;

function TRestOrm.{%H-}EngineBatchSend(Table: TOrmClass; var Data: RawUtf8;
  var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
begin
  raise EOrmException.CreateUtf8('BATCH not supported by %', [self]);
end;

function TRestOrm.UpdateFieldIncrement(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; Increment: Int64): boolean;
var
  t: integer;
begin
  if ID <> 0 then
  begin
    t := fModel.GetTableIndexExisting(Table);
    result := EngineUpdateFieldIncrement(t, ID, FieldName, Increment);
    if fCache <> nil then
      fCache.NotifyDeletion(t, ID);
  end
  else
    result := false;
end;

function TRestOrm.RecordCanBeUpdated(Table: TOrmClass; ID: TID;
  Action: TOrmEvent; ErrorMsg: PRawUtf8): boolean;
begin
  result := true; // accept by default -> override this method to customize this
end;

function TRestOrm.Delete(Table: TOrmClass; ID: TID): boolean;
var
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  if not RecordCanBeUpdated(Table, ID, oeDelete) then
    result := false
  else
  begin
    fCache.NotifyDeletion(t, ID);
    WriteLock;
    try // may be within a batch in another thread
      result := EngineDelete(t, ID);
    finally
      WriteUnLock;
    end;
  end;
end;

function TRestOrm.InternalDeleteNotifyAndGetIDs(Table: TOrmClass;
  const SqlWhere: RawUtf8; var IDs: TIDDynArray): boolean;
var
  t, i: PtrInt;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := false;
  if OneFieldValues(Table, 'RowID', SqlWhere, TInt64DynArray(IDs)) and
     (IDs <> nil) then
  begin
    for i := 0 to length(IDs) - 1 do
      if not RecordCanBeUpdated(Table, IDs[i], oeDelete) then
        exit;
    fCache.NotifyDeletions(t, IDs);
  end;
  result := true;
end;

function TRestOrm.InternalBatchStart(Encoding: TRestBatchEncoding;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false;
end;

procedure TRestOrm.InternalBatchStop;
begin
  raise EOrmBatchException.CreateUtf8('Unexpected %.InternalBatchStop', [self]);
end;

function TRestOrm.InternalBatchDirectSupport(Encoding: TRestBatchEncoding;
  RunTableIndex: integer): TRestOrmBatchDirect;
begin
  result := dirUnsupported; // by default, will use regular Add/Update
end;

function TRestOrm.{%H-}InternalBatchDirectOne(Encoding: TRestBatchEncoding;
  RunTableIndex: integer; const Fields: TFieldBits; Sent: PUtf8Char): TID;
begin
  raise EOrmBatchException.CreateUtf8(
    'Unexpected %.InternalBatchDirectOne', [self]);
end;

function TRestOrm.Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean;
var
  IDs: TIDDynArray;
begin
  if InternalDeleteNotifyAndGetIDs(Table, SqlWhere, IDs) then
  begin
    WriteLock;
    try // may be within a batch in another thread
      result := EngineDeleteWhere(fModel.GetTableIndexExisting(Table), SqlWhere, IDs);
    finally
      WriteUnLock;
    end;
  end
  else
    result := false;
end;

function TRestOrm.Delete(Table: TOrmClass; const FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const): boolean;
var
  where: RawUtf8;
begin
  where := FormatUtf8(FormatSqlWhere, [], BoundsSqlWhere);
  result := Delete(Table, where);
end;

function TRestOrm.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; out BlobData: RawBlob): boolean;
var
  blob: PRttiProp;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) then
    exit;
  blob := Table.OrmProps.BlobFieldPropFromRawUtf8(BlobFieldName);
  if blob = nil then
    exit;
  result := EngineRetrieveBlob(
    fModel.GetTableIndexExisting(Table), aID, blob, BlobData);
end;

function TRestOrm.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; out BlobStream: TCustomMemoryStream): boolean;
var
  data: RawBlob;
begin
  BlobStream := TMemoryStream.Create;
  result := RetrieveBlob(Table, aID, BlobFieldName, data);
  if not result or
     (data = '') then
    exit;
  if BlobStream.Write(pointer(data)^, length(data)) <> length(data) then
    result := false;
  BlobStream.Seek(0, soBeginning); // rewind
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; const BlobData: RawBlob): boolean;
var
  blob: PRttiProp;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) or
     not RecordCanBeUpdated(Table, aID, oeUpdate) then
    exit;
  blob := Table.OrmProps.BlobFieldPropFromRawUtf8(BlobFieldName);
  if blob = nil then
    exit;
  result := EngineUpdateBlob(
    fModel.GetTableIndexExisting(Table), aID, blob, BlobData);
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; BlobData: TStream): boolean;
var
  data: RawBlob;
  L: Int64;
begin
  result := false;
  if (self = nil) or
     (BlobData = nil) then
    exit;
  L := BlobData.Seek(0, soEnd);
  if L > maxInt then
    raise EOrmException.CreateUtf8('%.UpdateBlob: %.Size=%', [self, BlobData, L]);
  SetLength(data, L);
  BlobData.Seek(0, soBeginning);
  if BlobData.Read(pointer(data)^, L) <> L then
    exit;
  result := UpdateBlob(Table, aID, BlobFieldName, data);
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; BlobData: pointer; BlobSize: integer): boolean;
var
  tmp: RawByteString;
begin
  if (self = nil) or
     (BlobData = nil) or
     (BlobSize < 0) then
    result := false
  else
  begin
    FastSetRawByteString(tmp, BlobData, BlobSize);
    result := UpdateBlob(Table, aID, BlobFieldName, tmp);
  end;
end;

function TRestOrm.UpdateBlobFields(Value: TOrm): boolean;
var
  data: RawByteString;
  t, i: PtrInt;
begin
  result := false;
  if (Value = nil) or
     (Value.IDValue <= 0) then
    exit;
  with Value.Orm do
    if BlobFields <> nil then
    begin
      t := self.fModel.GetTableIndexExisting(POrmClass(Value)^);
      for i := 0 to length(BlobFields) - 1 do
      begin
        BlobFields[i].PropInfo.GetLongStrProp(Value, data);
        if not EngineUpdateBlob(t, Value.IDValue,
           BlobFields[i].PropInfo, data) then
          exit;
      end;
    end;
  result := true;
end;

function TRestOrm.RetrieveBlobFields(Value: TOrm): boolean;
var
  data: RawBlob;
  t, i: PtrInt;
begin
  result := false;
  if (Self = nil) or
     (Value = nil) or
     (Value.IDValue <= 0) then
    exit;
  with Value.Orm do
    if BlobFields <> nil then
    begin
      t := self.fModel.GetTableIndexExisting(POrmClass(Value)^);
      for i := 0 to length(BlobFields) - 1 do
        if EngineRetrieveBlob(t, Value.IDValue,
            BlobFields[i].PropInfo, data) then
          BlobFields[i].PropInfo.SetLongStrProp(Value, data)
        else
          exit;
    end;
  result := true;
end;

function TRestOrm.TransactionBegin(aTable: TOrmClass; SessionID: cardinal): boolean;
begin
  result := false;
  WriteLock;
  try
    if fTransactionActiveSession = 0 then
    begin
      // nested transactions are not allowed
      fTransactionActiveSession := SessionID;
      fTransactionTable := aTable;
      result := true;
    end;
  finally
    WriteUnLock;
  end;
end;

function TRestOrm.TransactionActiveSession: cardinal;
begin
  if self = nil then
    result := 0
  else
  begin
    WriteLock;
    try
      result := fTransactionActiveSession;
    finally
      WriteUnLock;
    end;
  end;
end;

procedure TRestOrm.Commit(SessionID: cardinal; RaiseException: boolean);
begin
  if self <> nil then
  begin
    WriteLock;
    try
      if (fTransactionActiveSession <> 0) and
         (fTransactionActiveSession = SessionID) then
      begin
        fTransactionActiveSession := 0; // by default, just release flag
        fTransactionTable := nil;
      end;
    finally
      WriteUnLock;
    end;
  end;
end;

procedure TRestOrm.RollBack(SessionID: cardinal);
begin
  if self <> nil then
  begin
    WriteLock;
    try
      if (fTransactionActiveSession <> 0) and
         (fTransactionActiveSession = SessionID) then
      begin
        fTransactionActiveSession := 0; // by default, just release flag
        fTransactionTable := nil;
      end;
    finally
      WriteUnLock;
    end;
  end;
end;

function TRestOrm.BatchSend(Batch: TRestBatch; var Results: TIDDynArray): integer;
var
  json: RawUtf8; // layout is '{"Table":["cmd":values,...]}'
begin
  result := HTTP_BADREQUEST;
  if (self = nil) or
     (Batch = nil) then
    // no opened BATCH sequence
    exit;
  InternalLog('BatchSend %', [Batch]);
  if Batch.PrepareForSending(json) then
    if json = '' then // i.e. Batch.Count=0
      result := HTTP_SUCCESS
    else
    try
      result := BatchSend(Batch.Table, json, Results, Batch.Count);
    except
      on Exception do
        // e.g. error during TRestServer.BatchSend()
        result := HTTP_SERVERERROR;
    end;
end;

function TRestOrm.BatchSend(Batch: TRestBatch): integer;
var
  dummyRes: TIDDynArray;
begin
  result := BatchSend(Batch, dummyRes);
end;

function TRestOrm.BatchSend(Table: TOrmClass; var Data: RawUtf8;
  var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
begin
  result := EngineBatchSend(Table, Data, Results, ExpectedResultsCount);
end;

function TRestOrm.AsyncBatchStart(Table: TOrmClass;
  SendSeconds: integer; PendingRowThreshold: integer;
  AutomaticTransactionPerRow: integer; Options: TRestBatchOptions): boolean;
begin
  if self = nil then
    result := false
  else
    result := fRest.Run.EnsureBackgroundTimerExists.AsyncBatchStart(Table,
      SendSeconds, PendingRowThreshold, AutomaticTransactionPerRow, Options);
end;

function TRestOrm.AsyncBatchStop(Table: TOrmClass): boolean;
begin
  if (self = nil) or
     (fRest.Run = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := false
  else
    result := fRest.Run.BackgroundTimer.AsyncBatchStop(Table);
end;

function TRestOrm.AsyncBatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsyncBatchAdd(Value, SendData, ForceID,
      CustomFields, DoNotAutoComputeFields);
end;

function TRestOrm.AsyncBatchRawAdd(Table: TOrmClass;
  const SentData: RawUtf8): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsyncBatchRawAdd(Table, SentData);
end;

procedure TRestOrm.AsyncBatchRawAppend(Table: TOrmClass;
  SentData: TJsonWriter);
begin
  if (self <> nil) and
     (fRest.Run.BackgroundTimer <> nil) and
     (fRest.Run.BackgroundTimer.BackgroundBatch <> nil) then
    fRest.Run.BackgroundTimer.AsyncBatchRawAppend(Table, SentData);
end;

function TRestOrm.AsyncBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsyncBatchUpdate(Value, CustomFields,
      DoNotAutoComputeFields);
end;

function TRestOrm.AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsyncBatchDelete(Table, ID);
end;

function TRestOrm.Cache: TOrmCache;
begin
  if fCache = nil then
    fCache := TOrmCache.Create(self);
  result := fCache;
end;

function TRestOrm.CacheWorthItForTable(aTableIndex: cardinal): boolean;
begin
  result := true; // always worth caching by default
end;

procedure TRestOrm.InternalLog(const Format: RawUtf8; const Args: array of const;
  Level: TSynLogInfo);
begin
  fRest.InternalLog(Format, Args, Level);
end;

function TRestOrm.GetCurrentSessionUserID: TID;
begin
  result := fRest.GetCurrentSessionUserID;
end;


{ ************ TOrmTableWritable Read/Write TOrmTable }

{ TOrmTableWritable }

function TOrmTableWritable.AddField(const FieldName: RawUtf8): integer;
var
  prev: TOrmTableJsonDataArray;
  {$ifndef NOTORMTABLELEN}
  prevlen: TIntegerDynArray;
  {$endif NOTORMTABLELEN}
  rowlen, i, n: PtrInt;
  S, D: PByte;
begin
  if (FieldName = '') or
     (FieldIndex(FieldName) >= 0) then
    raise EOrmTable.CreateUtf8('%.AddField(%) invalid fieldname', [self, FieldName]);
  // register the new field
  result := fFieldCount;
  inc(fFieldCount);
  SetLength(fFieldNames, fFieldCount);
  fFieldNames[result] := FieldName;
  QuickSortIndexedPUtf8Char(pointer(fFieldNames), fFieldCount, fFieldNameOrder);
  // prepare internal storage
  prev := fJsonData;
  fJsonData := nil;
  {$ifndef NOTORMTABLELEN}
  prevlen := fLen;
  fLen := nil; // SetResultsSafe() won't try to set fLen[]
  {$endif NOTORMTABLELEN}
  n := (fRowCount + 1) * fFieldCount;
  // adjust data rows
  SetLength(fJsonData, n);
  fData := pointer(fJsonData);
  SetResultsSafe(result, pointer(FieldName)); // set new field name in row=0
  S := pointer(prev);
  D := pointer(fJsonData);
  rowlen := result * SizeOf(fJsonData[0]);
  for i := 0 to fRowCount do
  begin
    MoveFast(S^, D^, rowlen);
    inc(S, rowlen);
    inc(D, rowlen + SizeOf(fJsonData[0])); // leave new field value as D^=nil
  end;
  {$ifndef NOTORMTABLELEN}
  // also adjust the internal fLen[] array
  SetLength(fLen, n);
  fLen[result] := length(fFieldNames[result]); // we know the new field length
  S := pointer(prevlen);
  D := pointer(fLen);
  rowlen := result shl 2;
  for i := 0 to fRowCount do
  begin
    MoveFast(S^, D^, rowlen);
    inc(S, rowlen);
    inc(D, rowlen + 4); // leave new field value as fLen[]=0
  end;
  {$endif NOTORMTABLELEN}
end;

procedure TOrmTableWritable.Update(Row: PtrInt; const FieldName, Value: RawUtf8);
begin
  Update(Row, FieldIndexExisting(FieldName), Value);
end;

procedure TOrmTableWritable.Update(Row, Field: PtrInt; const Value: RawUtf8);
var
  U: PUtf8Char;
  n: PtrInt;
  f: integer;
begin
  // update the content
  if (self = nil) or
     (fData = nil) or
     (Row <= 0) or
     (Row > fRowCount) or
     (PtrUInt(Field) >= PtrUInt(fFieldCount)) then
    exit;
  f := 1 shl integer(Field); // =0 -> too many fields -> notify all fields
  inc(Field, Row * fFieldCount);
  U := GetResults(Field);
  if StrComp(U, pointer(Value)) = 0 then
    exit; // nothing was changed
  if fUpdatedValuesInterning <> nil then
    U := pointer(fUpdatedValuesInterning.Unique(Value))
  else
  begin
    AddRawUtf8(fUpdatedValues, fUpdatedValuesCount, Value);
    U := pointer(Value);
  end;
  SetResultsSafe(Field, U);
  // track Update() modifications via UpdatedRows[]/UpdatedRowsFields[]
  if fNoUpdateTracking then
    exit;
  n := AddSortedInteger(fUpdatedRows, fUpdatedRowsCount, Row,
    @fUpdatedRowsFields); // O(log(N)) lookup
  if n < 0 then
  begin
    // update the CoValues/fUpdatedRowsFields modified field bits
    n := -(n + 1);  // returned -(foundindex+1)
    if f <> 0 then
      f := f or fUpdatedRowsFields[n];
  end;
  fUpdatedRowsFields[n] := f
end;

function TOrmTableWritable.AddField(const FieldName: RawUtf8;
  FieldType: TOrmFieldType; FieldTypeInfo: pointer; FieldSize: integer): integer;
begin
  result := AddField(FieldName);
  SetFieldType(result, FieldType, FieldTypeInfo, FieldSize);
end;

function TOrmTableWritable.AddField(const FieldName: RawUtf8;
  FieldTable: TOrmClass; const FieldTableName: RawUtf8): integer;
var
  prop: TOrmPropInfo;
  nfo: PRttiInfo;
begin
  result := AddField(FieldName);
  if FieldTable = nil then
    exit;
  with FieldTable.OrmProps.Fields do
    if FieldTableName <> '' then
      prop := ByRawUtf8Name(FieldTableName)
    else
      prop := ByRawUtf8Name(FieldName);
  if prop = nil then
    exit;
  if prop.InheritsFrom(TOrmPropInfoRtti) then
    nfo := TOrmPropInfoRtti(prop).PropType
  else
    nfo := nil;
  SetFieldType(result, prop.OrmFieldTypeStored, nfo, prop.FieldWidth,
    PtrArrayAddOnce(fQueryTables, FieldTable));
end;

procedure TOrmTableWritable.Update(Row: PtrInt; const FieldName: RawUtf8;
  const Value: variant);
begin
  Update(Row, FieldIndexExisting(FieldName), Value);
end;

procedure TOrmTableWritable.Update(Row, Field: PtrInt; const Value: variant);
var
  U: RawUtf8;
  wasString: boolean;
begin
  VariantToUtf8(Value, U, wasString);
  Update(Row, Field, U);
end;

procedure TOrmTableWritable.Join(From: TOrmTable; const FromKeyField, KeyField: RawUtf8);
var
  fk, dk, f, i, k, ndx: integer;
  n, fn: RawUtf8;
  info: POrmTableFieldType;
  new: TIntegerDynArray;
begin
  dk := FieldIndexExisting(KeyField);
  SetLength(new, FieldCount);
  fk := From.FieldIndexExisting(FromKeyField);
  From.SortFields(fk); // faster merge with O(log(n)) binary search
  for f := 0 to From.FieldCount - 1 do // add From fields (excluding FromKeyField)
    if f <> fk then
    begin
      n := From.FieldNames[f];
      fn := n;
      if FieldIndex(fn) >= 0 then // ensure unique name
        for i := 2 to 100 do
        begin
          fn := n + SmallUInt32Utf8[i];
          if FieldIndex(fn) < 0 then
            break;
        end;
      if From.FieldType(f, info) = oftUnknown then  // set TOrmTableFieldType
        i := AddField(fn)
      else if info.TableIndex >= 0 then
        i := AddField(fn, From.QueryTables[info.TableIndex], n)
      else
      begin
        i := AddField(fn);
        if i >= length(fFieldType) then
          SetLength(fFieldType, i + 1);
        fFieldType[i] := info^;
      end;
      new[f] := i;
    end;
  ndx := FieldCount;
  for i := 1 to fRowCount do
  begin
    // merge data
    k := From.SearchFieldSorted(Results[ndx + dk], fk);
    if k > 0 then
    begin
      k := k * From.FieldCount;
      for f := 0 to From.FieldCount - 1 do
        if f <> fk then // fast PUtf8Char copy
          SetResultsSafe(ndx + new[f], From.Results[k + f]);
    end;
    inc(ndx, FieldCount);
  end;
end;

function TOrmTableWritable.UpdatesToBatch(
  Batch: TRestBatch; aServerTimeStamp: TTimeLog): integer;
var
  c: TOrmClass;
  rec: TOrm;
  r, f, p: PtrInt;
  def, def32, bits: TFieldBits;
  props: TIntegerDynArray;
  upd, updlast, b: integer;
begin
  result := 0;
  c := QueryRecordType;
  if (Batch = nil) or
     (fUpdatedRowsCount = 0) or
     (c = nil) then
    exit;
  rec := c.Create;
  try
    rec.FillPrepare(self);
    rec.FillContext.ComputeSetUpdatedFieldIndexes(props);
    with rec.Orm do
      if (oftModTime in HasTypeFields) and
         not (boOnlyObjects in Batch.Options) then
      begin
        // pre-compute once any TModTime field
        if aServerTimeStamp = 0 then
          if Assigned(Batch.Rest) then
            aServerTimeStamp := Batch.Rest.GetServerTimestamp
          else
            aServerTimeStamp := TimeLogNowUtc; // e.g. from UpdatesToJson()
        def := FieldBits[oftModTime];
        rec.ComputeFieldsBeforeWrite(nil, oeUpdate, aServerTimeStamp);
      end
      else
        FillZero(def);
    def32 := rec.FillContext.TableMapFields;
    updlast := 0; // recompute bits only if changed from previous rec
    for r := 0 to fUpdatedRowsCount - 1 do
    begin
      rec.FillContext.Fill(fUpdatedRows[r]);
      if rec.IDValue = 0 then
        raise EOrmTable.CreateUtf8('%.UpdatesToBatch: no %.ID map', [self, c]);
      upd := fUpdatedRowsFields[r];
      if upd = 0 then // more than 32 fields -> include all fields to batch
        bits := def32
      else if upd <> updlast then
      begin
        updlast := upd;
        bits := def;
        b := 1;
        for f := 0 to fFieldCount - 1 do
        begin
          if upd and b <> 0 then
          begin
            p := props[f];
            if p < 0 then
              raise EOrmTable.CreateUtf8(
                '%.UpdatesToBatch: Unexpected %.%', [self, c, Results[f]]);
            FieldBitSet(bits, p);
          end;
          b := b shl 1;
        end;
      end;
      if Batch.Update(rec, bits, {donotautocompute:}true) < 0 then
        raise EOrmTable.CreateUtf8(
          '%.UpdatesToBatch: Batch.Update % failed', [self, rec]);
      inc(result);
    end;
  finally
    rec.Free;
  end;
end;

function TOrmTableWritable.UpdatesToJson(
  aOptions: TRestBatchOptions; aServerTimeStamp: TTimeLog): RawJson;
var
  b: TRestBatch;
begin
  b := TRestBatch.CreateNoRest(nil, QueryRecordType, 10000, aOptions);
  try
    UpdatesToBatch(b, aServerTimeStamp);
    b.PrepareForSending(RawUtf8(result));
  finally
    b.Free;
  end;
end;


end.

