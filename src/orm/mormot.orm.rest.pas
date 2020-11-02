/// Object-Relational-Mapping (ORM) Abstract REST Implementation
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.rest;

{
  *****************************************************************************

   IRestOrm Implementation as used by TRest
    - Some definitions Used by TRestOrm Implementation
    - TRestOrm Parent Class for abstract REST client/server
   
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
  mormot.core.secure,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.json,
  mormot.orm.core,
  mormot.rest.core,
  mormot.db.core;


{ ************ Some definitions Used by TRestOrm Implementation }

type
  /// the available HTTP methods transmitted between client and server
  // - some custom verbs are available in addition to standard REST commands
  // - most of iana verbs are available
  // see http://www.iana.org/assignments/http-methods/http-methods.xhtml
  // - for basic CRUD operations, we consider Create=mPOST, Read=mGET,
  // Update=mPUT and Delete=mDELETE - even if it is not fully RESTful
  TURIMethod = (
    mNone,
    mGET,
    mPOST,
    mPUT,
    mDELETE,
    mHEAD,
    mBEGIN,
    mEND,
    mABORT,
    mLOCK,
    mUNLOCK,
    mSTATE,
    mOPTIONS,
    mPROPFIND,
    mPROPPATCH,
    mTRACE,
    mCOPY,
    mMKCOL,
    mMOVE,
    mPURGE,
    mREPORT,
    mMKACTIVITY,
    mMKCALENDAR,
    mCHECKOUT,
    mMERGE,
    mNOTIFY,
    mPATCH,
    mSEARCH,
    mCONNECT);

  /// set of available HTTP methods transmitted between client and server
  TURIMethods = set of TURIMethod;

/// convert a string HTTP verb into its TURIMethod enumerate
function ToMethod(const method: RawUTF8): TURIMethod;

function ToText(m: TURIMethod): PShortString; overload;


{$ifdef PUREMORMOT2}

type
  TSQLURIMethod = TURIMethod;
  TSQLURIMethods = TURIMethods;

{$endif PUREMORMOT2}



{ ************ TRestOrm Parent Class for abstract REST client/server }

type
  {$M+}

  /// implements TRest.ORM process for abstract REST client/server
  TRestOrm = class(TRestOrmParent, IRestOrm)
  protected
    fRest: TRest;
    fModel: TOrmModel; // owned by the TRest associated instance
    fCache: TRestCache;
    fTransactionActiveSession: cardinal;
    fTransactionTable: TOrmClass;
    /// compute SELECT ... FROM TABLE WHERE ...
    function SQLComputeForSelect(Table: TOrmClass;
      const FieldNames, WhereClause: RawUTF8): RawUTF8;
    /// used by all overloaded Add/Delete methods
    procedure GetJSONValuesForAdd(TableIndex: integer; Value: TOrm;
      ForceID, DoNotAutoComputeFields, WithBlobs: boolean;
      CustomFields: PFieldBits; var result: RawUTF8);
    function InternalAdd(Value: TOrm; SendData: boolean;
      CustomFields: PFieldBits;
      ForceID, DoNotAutoComputeFields: boolean): TID; virtual;
    function InternalDeleteNotifyAndGetIDs(Table: TOrmClass;
      const SQLWhere: RawUTF8; var IDs: TIDDynArray): boolean;
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
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; virtual; abstract;
    /// Execute directly a SQL statement, without any result
    // - implements POST SQL on ModelRoot URI
    // - return true on success
    // - override this method for proper calling the database engine
    // - don't call this method in normal cases
    // - this method must be implemented to be thread-safe
    function EngineExecute(const aSQL: RawUTF8): boolean; virtual; abstract;
    /// get a member from its ID
    // - implements REST GET member
    // - returns the data of this object as JSON
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; virtual; abstract;
    /// create a new member
    // - implements REST POST collection
    // - SentData can contain the JSON object with field values to be added
    // - class is taken from Model.Tables[TableModelIndex]
    // - returns the TOrm ID/RowID value, 0 on error
    // - if a "RowID":.. or "ID":.. member is set in SentData, it shall force
    // this value as insertion ID
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; virtual; abstract;
    /// update a member
    // - implements REST PUT collection
    // - SentData can contain the JSON object with field values to be added
    // - returns true on success
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineUpdate(TableModelIndex: integer; ID: TID; const SentData: RawUTF8): boolean; virtual; abstract;
    /// delete a member
    // - implements REST DELETE collection
    // - returns true on success
    // - override this method for proper calling the database engine
    // - this method must be implemented in a thread-safe manner
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; virtual; abstract;
    /// delete several members, from a WHERE clause
    // - IDs[] contains the already-computed matching IDs for SQLWhere
    // - returns true on success
    // - override this method for proper calling the database engine, i.e.
    // using either IDs[] or a faster SQL statement
    // - this method must be implemented in a thread-safe manner
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
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
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; virtual; abstract;
    /// increments one integer field value
    // - this default implementation is just a wrapper around OneFieldValue +
    // UpdateField methods
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; virtual;
    /// send/execute the supplied JSON BATCH content, and return the expected array
    // - this method will be implemented for TRestClient and TRestServer only
    // - this default implementation will trigger an EOrmException
    // - warning: supplied JSON Data can be parsed in-place, so modified
    function EngineBatchSend(Table: TOrmClass; var Data: RawUTF8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; virtual;
    /// internal method called by TRestServer.Batch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    // - returns TRUE if this method is handled by the engine, or FALSE if
    // individual calls to Engine*() are expected
    // - this default implementation returns FALSE
    // - an overridden method returning TRUE shall ensure that calls to
    // EngineAdd / EngineUpdate / EngineDelete (depending of supplied Method)
    // will properly handle operations until InternalBatchStop() is called
    function InternalBatchStart(Method: TURIMethod;
      BatchOptions: TRestBatchOptions): boolean; virtual;
    /// internal method called by TRestServer.Batch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    // - this default implementation will raise an EOrmException (since
    // InternalBatchStart returns always FALSE at this TRest level)
    // - InternalBatchStart/Stop may safely use a lock for multithreading:
    // implementation in TRestServer.Batch use a try..finally block
    procedure InternalBatchStop; virtual;
  public
    // ------- TRestOrm main methods
    /// initialize the class, and associated to a TRest and its TOrmModel
    constructor Create(aRest: TRest); reintroduce; virtual;
      /// initialize the class, and associated to TOrmModel with no main TRest
    constructor CreateWithoutRest(aModel: TOrmModel); reintroduce; virtual;
    /// release internal used instances
    destructor Destroy; override;
    /// ensure the current thread will be taken into account during process
    // - this abstract method won't do anything, but overriden versions may
    procedure BeginCurrentThread(Sender: TThread); virtual;
    /// called when thread is finished to ensure
    // - this abstract method won't do anything, but overriden versions may
    procedure EndCurrentThread(Sender: TThread); virtual;
    /// low-level access to the current TOrm class holding a transaction
    // - equals nil outside of a TransactionBegin/Commit scope
    property TransactionTable: TOrmClass read fTransactionTable;
  public
    // ------- IRestOrm interface implementation methods
    // calls internaly the "SELECT Count(*) FROM TableName;" SQL statement
    function TableRowCount(Table: TOrmClass): Int64; virtual;
    // calls internaly a "SELECT RowID FROM TableName LIMIT 1" SQL statement,
    // which is much faster than testing if "SELECT count(*)" equals 0 - see
    // @http://stackoverflow.com/questions/8988915
    function TableHasRows(Table: TOrmClass): boolean; virtual;
    // executes by default "SELECT max(rowid) FROM TableName"
    function TableMaxID(Table: TOrmClass): TID; virtual;
    // try from cache, then from DB
    function MemberExists(Table: TOrmClass; ID: TID): boolean; virtual;
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8): RawUTF8; overload;
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8; Default: Int64 = 0): Int64;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): RawUTF8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const): RawUTF8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      WhereID: TID): RawUTF8; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      const WhereClause: RawUTF8): boolean; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      WhereID: TID): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; out Data: TRawUTF8DynArray): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; var Data: TInt64DynArray;
      SQL: PRawUTF8 = nil): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8 = ''; const Separator: RawUTF8 = ','): RawUTF8; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUTF8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClause: RawUTF8 = ''): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const BoundsSQLWhere: array of const): TOrmTable;
      overload; {$ifdef ISDELPHI2010} override; {$else} virtual; {$endif}
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const Args, Bounds: array of const): TOrmTable; overload;
    function FTSMatch(Table: TOrmFts3Class; const WhereClause: RawUTF8;
      var DocID: TIDDynArray): boolean; overload;
    function FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUTF8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUTF8;
    function MainFieldID(Table: TOrmClass; const Value: RawUTF8): TID;
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUTF8;
      out IDs: TIDDynArray): boolean;
    function Retrieve(const SQLWhere: RawUTF8; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload; virtual;
    function Retrieve(const WhereClauseFmt: RawUTF8;
      const Args, Bounds: array of const; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; overload; virtual;
    function Retrieve(Reference: TRecordReference;
      ForUpdate: boolean = false): TOrm; overload;
    function Retrieve(aPublishedRecord, aValue: TOrm): boolean; overload;
    function RetrieveList(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList; overload;
    function RetrieveListJSON(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''; aForceAJAX: boolean = false): RawJSON; overload;
    function RetrieveListJSON(Table: TOrmClass;
      const SQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8 = '';
      aForceAJAX: boolean = false): RawJSON; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUTF8; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const; const CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const): variant;
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const CustomFieldsCSV: RawUTF8): variant;
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean;
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const OutputFieldName: RawUTF8; W: TJSONSerializer;
      const CustomFieldsCSV: RawUTF8 = '');
    function RTreeMatch(DataTable: TOrmClass;
      const DataTableBlobFieldName: RawUTF8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUTF8): TOrmTable; virtual;
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJSON; virtual;
    function Execute(const aSQL: RawUTF8): boolean; virtual;
    function ExecuteFmt(const SQLFormat: RawUTF8;
      const Args: array of const): boolean; overload;
    function ExecuteFmt(const SQLFormat: RawUTF8;
      const Args, Bounds: array of const): boolean; overload;
    function UnLock(Table: TOrmClass; aID: TID): boolean; overload; virtual; abstract;
    function UnLock(Rec: TOrm): boolean; overload;
    function Add(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomCSVFields: RawUTF8;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomFields: TFieldBits;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function AddWithBlobs(Value: TOrm;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; virtual;
    function AddSimple(aTable: TOrmClass;
      const aSimpleFields: array of const; ForcedID: TID = 0): TID;
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload; virtual;
    function Update(Value: TOrm; const CustomCSVFields: RawUTF8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(aTable: TOrmClass; aID: TID;
      const aSimpleFields: array of const): boolean; overload;
    function AddOrUpdate(Value: TOrm; ForceID: boolean = false): TID;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUTF8;
      const WhereFieldValue: array of const; const FieldName: RawUTF8;
      const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUTF8; const WhereFieldValue: variant;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass; const IDs: array of Int64;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; Increment: Int64 = 1): boolean;
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUTF8 = nil): boolean; virtual;
    function Delete(Table: TOrmClass; ID: TID): boolean; overload; virtual;
    function Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean; overload; virtual;
    function Delete(Table: TOrmClass; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUTF8;
      out BlobData: RawBlob): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUTF8;
      out BlobStream: TCustomMemoryStream): boolean; overload; virtual;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; const BlobData: RawBlob): boolean; overload; virtual;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; BlobData: TStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; BlobData: pointer; BlobSize: integer): boolean; overload;
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
    function BatchSend(Table: TOrmClass; var Data: RawUTF8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; overload;
    function AsynchBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJSON]): boolean;
    function AsynchBatchStop(Table: TOrmClass): boolean;
    function AsynchBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsynchBatchRawAdd(Table: TOrmClass; const SentData: RawUTF8): integer;
    procedure AsynchBatchRawAppend(Table: TOrmClass; SentData: TTextWriter);
    function AsynchBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;
    function Model: TOrmModel;
      {$ifdef HASINLINE}inline;{$endif}
    function Cache: TRestCache;
    function CacheOrNil: TRestCache;
      {$ifdef HASINLINE}inline;{$endif}
    function CacheWorthItForTable(aTableIndex: cardinal): boolean; virtual;
    function LogClass: TSynLogClass;
      {$ifdef HASINLINE}inline;{$endif}
    function LogFamily: TSynLogFamily;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalLog(const Text: RawUTF8; Level: TSynLogInfo); overload;
      {$ifdef HASINLINE}inline;{$endif}
    procedure InternalLog(const Format: RawUTF8; const Args: array of const;
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


implementation


{ ************ Some definitions Used by TRestOrm Implementation }

function ToMethod(const method: RawUTF8): TURIMethod;
const
  NAME: array[mGET..high(TURIMethod)] of string[10] = (
    // sorted by occurence for in-order O(n) search
    'GET','POST','PUT','DELETE','HEAD','BEGIN','END','ABORT',
    'LOCK','UNLOCK','STATE', 'OPTIONS','PROPFIND','PROPPATCH','TRACE',
    'COPY','MKCOL','MOVE','PURGE','REPORT', 'MKACTIVITY','MKCALENDAR',
    'CHECKOUT','MERGE','NOTIFY','PATCH','SEARCH','CONNECT');
var
  L: PtrInt;
  N: PShortString;
begin
  L := Length(method);
  if L < 11 then
  begin
    N := @NAME;
    for result := low(NAME) to high(NAME) do
      if (L = ord(N^[0])) and
         IdemPropNameUSameLen(@N^[1], pointer(method), L) then
        exit
      else
        inc(PByte(N), 11);
  end;
  result := mNone;
end;

function ToText(m: TURIMethod): PShortString;
begin
  result := GetEnumName(TypeInfo(TURIMethod), ord(m));
end;


{ ************ TRestOrm Parent Class for abstract REST client/server }

{ TRestOrm }

// ------- TRestOrm main methods

constructor TRestOrm.Create(aRest: TRest);
begin
  inherited Create;
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
  FreeAndNil(fCache);
  inherited Destroy;
  if (fModel <> nil) and
     (fModel.Owner = self) then
    // make sure we are the Owner (TRestStorage has fModel<>nil e.g.)
    FreeAndNil(fModel);
end;

procedure TRestOrm.BeginCurrentThread(Sender: TThread);
begin
  // nothing do to at this level -> see e.g. TRestOrmServer.BeginCurrentThread
end;

procedure TRestOrm.EndCurrentThread(Sender: TThread);
begin
  // nothing do to at this level -> see e.g. TRestOrmServer.EndCurrentThread
end;

function TRestOrm.SQLComputeForSelect(Table: TOrmClass;
  const FieldNames, WhereClause: RawUTF8): RawUTF8;
begin
  result := '';
  if (self = nil) or
     (Table = nil) then
    exit;
  if FieldNames = '' then
    result := fModel.Props[Table].SQLFromSelectWhere('*', WhereClause)
  else
    with Table.RecordProps do
      if FieldNames = '*' then
        result := SQLFromSelect(SQLTableName, SQLTableRetrieveAllFields, WhereClause, '')
      else if (PosExChar(',', FieldNames) = 0) and
              (PosExChar('(', FieldNames) = 0) and
              not IsFieldName(FieldNames) then
        // prevent SQL error
        result := ''
      else
        result := SQLFromSelect(SQLTableName, FieldNames, WhereClause, '');
end;

procedure TRestOrm.GetJSONValuesForAdd(TableIndex: integer; Value: TOrm;
  ForceID, DoNotAutoComputeFields, WithBlobs: boolean;
  CustomFields: PFieldBits; var result: RawUTF8);
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
  props := Value.RecordProps;
  if CustomFields <> nil then
    if DoNotAutoComputeFields then
      fields := CustomFields^ * props.CopiableFieldsBits
    else
      fields := CustomFields^ * props.CopiableFieldsBits + props.ComputeBeforeAddFieldsBits
  else if WithBlobs then
    fields := props.CopiableFieldsBits
  else
    fields := props.SimpleFieldsBits[ooInsert];
  if not ForceID and IsZero(fields) then
    result := ''
  else
    result := Value.GetJSONValues(true, ForceID, fields);
end;

function TRestOrm.InternalAdd(Value: TOrm; SendData: boolean;
  CustomFields: PFieldBits; ForceID, DoNotAutoComputeFields: boolean): TID;
var
  json: RawUTF8;
  t: integer;
begin
  if Value = nil then
  begin
    result := 0;
    exit;
  end;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  if SendData then
    GetJSONValuesForAdd(t, Value, ForceID, DoNotAutoComputeFields,
      false, CustomFields, json)
  else
    json := '';
  // on success, returns the new RowID value; on error, returns 0
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
  try // may be within a batch in another thread
    result := EngineAdd(t, json); // will call static if necessary
  finally
    fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
  // on success, Value.ID is updated with the new RowID
  Value.IDValue := result;
  if SendData and
     (result <> 0) then
    fCache.Notify(POrmClass(Value)^, result, json, ooInsert);
end;


// ------- IRestOrm interface implementation methods

function TRestOrm.Model: TOrmModel;
begin
  result := fModel;
end;

function TRestOrm.CacheOrNil: TRestCache;
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

procedure TRestOrm.InternalLog(const Text: RawUTF8; Level: TSynLogInfo);
begin
  fRest.InternalLog(Text, Level);
end;

function TRestOrm.GetServerTimestamp: TTimeLog;
begin
  result := fRest.GetServerTimeStamp;
end;

procedure TRestOrm.WriteLock;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.Lock;
end;

procedure TRestOrm.WriteUnLock;
begin
  fRest.AcquireExecution[execOrmWrite].Safe.UnLock;
end;

function TRestOrm.TableRowCount(Table: TOrmClass): Int64;
var
  T: TOrmTable;
begin
  if (self = nil) or
     (Table = nil) then
    T := nil
  else
    T := ExecuteList([Table], 'SELECT Count(*) FROM ' +
      Table.RecordProps.SQLTableName);
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
      Table.RecordProps.SQLTableName + ' LIMIT 1');
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
      Table.RecordProps.SQLTableName);
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
begin
  if fCache.Retrieve(fModel.GetTableIndexExisting(Table), ID) <> '' then
    result := true
  else
    result := OneFieldValue(Table, 'RowID', ID) <> ''; // try from DB
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName,
  WhereClause: RawUTF8): RawUTF8;
var
  res: array[0..0] of RawUTF8;
begin
  if MultiFieldValue(Table, [FieldName], res, WhereClause) then
    result := res[0]
  else
    result := '';
end;

function TRestOrm.OneFieldValueInt64(Table: TOrmClass; const FieldName,
  WhereClause: RawUTF8; Default: Int64): Int64;
var
  res: array[0..0] of RawUTF8;
begin
  if not MultiFieldValue(Table, [FieldName], res, WhereClause) or
     not ToInt64(res[0], result) then
    result := Default;
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): RawUTF8;
begin
  result := OneFieldValue(Table, FieldName,
    FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere));
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const): RawUTF8;
begin
  result := OneFieldValue(Table, FieldName,
    FormatUTF8(WhereClauseFmt, Args, Bounds));
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const;
  out Data: Int64): boolean;
var
  res: array[0..0] of RawUTF8;
  err: integer;
  where: RawUTF8;
begin
  result := false;
  where := FormatUTF8(WhereClauseFmt, Args, Bounds);
  if MultiFieldValue(Table, [FieldName], res, where) then
    if res[0] <> '' then
    begin
      Data := GetInt64(pointer(res[0]), err);
      if err = 0 then
        result := true;
    end;
end;

function TRestOrm.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  WhereID: TID): RawUTF8;
var
  res: array[0..0] of RawUTF8;
begin
  if (WhereID > 0) and
     MultiFieldValue(Table, [FieldName], res,
       'RowID=:(' + Int64ToUtf8(WhereID) + '):') then
    result := res[0]
  else
    result := '';
end;

function TRestOrm.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
  const WhereClause: RawUTF8): boolean;
var
  SQL: RawUTF8;
  n, i: PtrInt;
  T: TOrmTable;
  P: PUTF8Char;
begin
  result := false;
  n := length(FieldName);
  if (self <> nil) and
     (Table <> nil) and
     (n = length(FieldValue)) then
    with Table.RecordProps do
    begin
      if (n = 1) and
         IdemPChar(pointer(FieldName[0]), 'COUNT(*)') then
        SQL := 'SELECT COUNT(*) FROM ' + SQLTableName + SQLFromWhere(WhereClause)
      else
      begin
        for i := 0 to high(FieldName) do
          if not IsFieldNameOrFunction(FieldName[i]) then
            // prevent SQL error or security breach
            exit
          else if SQL = '' then
            SQL := 'SELECT ' + FieldName[i]
          else
            SQL := SQL + ',' + FieldName[i];
        SQL := SQL + ' FROM ' + SQLTableName + SQLFromWhere(WhereClause) + ' LIMIT 1';
      end;
      T := ExecuteList([Table], SQL);
      if T <> nil then
      try
        if (T.FieldCount <> length(FieldName)) or
           (T.RowCount <= 0) then
          exit;
        // get field values from the first (and unique) row
        for i := 0 to T.FieldCount - 1 do
        begin
          P := T.Results[T.FieldCount + i];
          FastSetString(FieldValue[i], P, StrLen(P));
        end;
        result := true;
      finally
        T.Free;
      end;
    end;
end;

function TRestOrm.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
  WhereID: TID): boolean;
begin
  result := MultiFieldValue(Table, FieldName, FieldValue, 'RowID=:(' +
    Int64ToUtf8(WhereID) + '):');
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUTF8;
  out Data: TRawUTF8DynArray): boolean;
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
  const FieldName, WhereClause: RawUTF8; var Data: TInt64DynArray;
  SQL: PRawUTF8): boolean;
var
  T: TOrmTable;
  V: Int64;
  prop: RawUTF8;
  P: PUTF8Char;
begin
  Data := nil;
  // handle naive expressions like SELECT ID from Table where ID=10
  if IsRowID(pointer(FieldName)) and
     (length(WhereClause) > 2) then
  begin
    P := pointer(WhereClause);
    GetNextFieldProp(P, prop);
    if IsRowIDShort(prop) and
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
        'i', 'I':
          if P[1] in ['n', 'N'] then
          begin
            // SELECT RowID from Table where RowID in [1,2,3]
            P := GotoNextNotSpace(P + 2);
            if (P^ = '(') and
               (GotoNextNotSpace(P + 1)^ in ['0'..'9']) then
            begin
              CSVToInt64DynArray(P + 1, Data);
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
      SQL^ := T.QuerySQL;
    result := true;
  finally
    T.Free;
  end;
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause, Separator: RawUTF8): RawUTF8;
var
  i, Len, SepLen, L: PtrInt;
  Lens: TIntegerDynArray;
  T: TOrmTable;
  P: PUTF8Char;
begin
  result := '';
  T := MultiFieldValues(Table, FieldName, WhereClause);
  if T <> nil then
  try
    if (T.FieldCount <> 1) or
       (T.RowCount <= 0) then
      exit;
    // calculate row values CSV needed memory
    SetLength(Lens, T.RowCount);
    SepLen := length(Separator);
    Len := SepLen * (T.RowCount - 1);
    for i := 1 to T.RowCount do
    begin
      L := StrLen(T.Results[i]); // ignore fResults[0] i.e. field name
      inc(Len, L);
      Lens[i - 1] := L;
    end;
    SetLength(result, Len);
    // add row values as CSV
    P := pointer(result);
    i := 1;
    repeat
      L := Lens[i - 1];
      if L <> 0 then
      begin
        MoveFast(T.Results[i]^, P^, L);
        inc(P, L);
      end;
      if i = T.RowCount then
        break;
      MoveFast(pointer(Separator)^, P^, SepLen);
      inc(P, SepLen);
      inc(i);
    until false;
    //assert(P-pointer(result)=Len);
  finally
    T.Free;
  end;
end;

function TRestOrm.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUTF8; Strings: TStrings;
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
    T := ExecuteList([Table], SQLFromSelect(Table.SQLTableName,
      'ID,' + FieldName, WhereClause, ''));
    if T <> nil then
    try
      if (T.FieldCount = 2) and
         (T.RowCount > 0) then
      begin
        for Row := 1 to T.RowCount do
        begin // ignore Row 0 i.e. field names
          aID := GetInt64(T.Get(Row, 0));
          Strings.AddObject(T.GetString(Row, 1), pointer(PtrInt(aID)));
          if (IDToIndex <> nil) and
             (aID = IDToIndex^) then
          begin
            IDToIndex^ := Row - 1;
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
  const FieldNames, WhereClause: RawUTF8): TOrmTable;
var
  sql: RawUTF8;
begin
  sql := SQLComputeForSelect(Table, FieldNames, WhereClause);
  if sql = '' then
    result := nil
  else
    result := ExecuteList([Table], sql);
end;

function TRestOrm.MultiFieldValues(Table: TOrmClass;
  const FieldNames, WhereClauseFormat: RawUTF8;
  const BoundsSQLWhere: array of const): TOrmTable;
var
  where: RawUTF8;
begin
  where := FormatUTF8(WhereClauseFormat, [], BoundsSQLWhere);
  result := MultiFieldValues(Table, FieldNames, where);
end;

function TRestOrm.MultiFieldValues(Table: TOrmClass;
  const FieldNames, WhereClauseFormat: RawUTF8;
  const Args, Bounds: array of const): TOrmTable;
var
  where: RawUTF8;
begin
  where := FormatUTF8(WhereClauseFormat, Args, Bounds);
  result := MultiFieldValues(Table, FieldNames, where);
end;

function TRestOrm.FTSMatch(Table: TOrmFts3Class;
  const WhereClause: RawUTF8; var DocID: TIDDynArray): boolean;
begin
  // FTS3 tables don't have any ID, but RowID or DocID
  result := OneFieldValues(Table, 'RowID', WhereClause, TInt64DynArray(DocID));
end;

function TRestOrm.FTSMatch(Table: TOrmFts3Class;
  const MatchClause: RawUTF8; var DocID: TIDDynArray;
  const PerFieldWeight: array of double; limit, offset: integer): boolean;
var
  WhereClause: RawUTF8;
  i: PtrInt;
begin
  result := false;
  with Table.RecordProps do
    if length(PerFieldWeight) <> length(SimpleFields) then
      exit
    else
      WhereClause := FormatUTF8('% MATCH ? ORDER BY rank(matchinfo(%)',
        [SQLTableName, SQLTableName], [MatchClause]);
  for i := 0 to high(PerFieldWeight) do
    WhereClause := FormatUTF8('%,?', [WhereClause], [PerFieldWeight[i]]);
  WhereClause := WhereClause + ') DESC';
  if limit > 0 then
    WhereClause := FormatUTF8('% LIMIT % OFFSET %', [WhereClause, limit, offset]);
  result := FTSMatch(Table, WhereClause, DocID);
end;

function TRestOrm.MainFieldValue(Table: TOrmClass; ID: TID;
  ReturnFirstIfNoUnique: boolean): RawUTF8;
begin
  if (self = nil) or
     (Table = nil) or
     (ID <= 0) then
    result := ''
  else
  begin
    result := Table.RecordProps.MainFieldName(ReturnFirstIfNoUnique);
    if result <> '' then
      result := OneFieldValue(Table, result, ID);
  end;
end;

function TRestOrm.MainFieldID(Table: TOrmClass; const Value: RawUTF8): TID;
var
  main: integer;
begin
  result := 0;
  if (self <> nil) and
     (Value <> '') and
     (Table <> nil) then
    with Table.RecordProps do
    begin
      main := MainField[false];
      if main >= 0 then
        SetID(OneFieldValue(Table, 'RowID', fields.List[main].Name +
          '=:(' + QuotedStr(Value, '''') + '):'), result);
    end;
end;

function TRestOrm.MainFieldIDs(Table: TOrmClass;
  const Values: array of RawUTF8; out IDs: TIDDynArray): boolean;
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
      with Table.RecordProps do
      begin
        // request all Values[] IDs at once
        main := MainField[false];
        if main >= 0 then
          OneFieldValues(Table, 'RowID',
            SelectInClause(fields.List[main].Name, Values), TInt64DynArray(IDs));
      end;
  result := {%H-}IDs <> nil;
end;

function TRestOrm.Retrieve(const SQLWhere: RawUTF8; Value: TOrm;
  const aCustomFieldsCSV: RawUTF8): boolean;
var
  T: TOrmTable;
begin
  result := false;
  if (self = nil) or
     (Value = nil) then
    exit;
  T := MultiFieldValues(POrmClass(Value)^, aCustomFieldsCSV, SQLWhere);
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

function TRestOrm.Retrieve(const WhereClauseFmt: RawUTF8;
  const Args, Bounds: array of const; Value: TOrm;
  const aCustomFieldsCSV: RawUTF8): boolean;
var
  where: RawUTF8;
begin
  where := FormatUTF8(WhereClauseFmt, Args, Bounds);
  result := Retrieve(where, Value, aCustomFieldsCSV);
end;

function TRestOrm.Retrieve(aID: TID; Value: TOrm; ForUpdate: boolean): boolean;
var
  t: integer; // used by EngineRetrieve() for SQL statement caching
  resp: RawUTF8;
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
  // try to retrieve existing JSON from internal cache
  resp := fCache.Retrieve(t, aID);
  if resp = '' then
  begin
    // get JSON object '{...}' in resp from corresponding EngineRetrieve() method
    resp := EngineRetrieve(t, aID);
    if resp = '' then
    begin
      fCache.NotifyDeletion(t, aID);
      exit;
    end;
    fCache.Notify(t, aID, resp, ooSelect);
  end;
  Value.IDValue := aID; // resp may not contain the "RowID": field after Update
  // fill Value from JSON if was correctly retrieved
  Value.FillFrom(resp);
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
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): TObjectList;
var
  T: TOrmTable;
begin
  result := nil;
  if (self = nil) or
     (Table = nil) then
    exit;
  T := MultiFieldValues(Table, aCustomFieldsCSV, FormatSQLWhere, BoundsSQLWhere);
  if T <> nil then
  try
    result := TObjectList.Create;
    T.ToObjectList(result, Table);
  finally
    T.Free;
  end;
end;

function TRestOrm.RetrieveListJSON(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8; aForceAJAX: boolean): RawJSON;
var
  where: RawUTF8;
begin
  where := FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere);
  result := RetrieveListJSON(Table, where, aCustomFieldsCSV, aForceAJAX)
end;

function TRestOrm.RetrieveListJSON(Table: TOrmClass;
  const SQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8;
  aForceAJAX: boolean): RawJSON;
var
  sql: RawUTF8;
begin
  sql := SQLComputeForSelect(Table, aCustomFieldsCSV, SQLWhere);
  if sql = '' then
    result := ''
  else
    result := EngineList(sql, aForceAJAX);
end;

function TRestOrm.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName, CustomFieldsCSV: RawUTF8; FirstRecordID: PID;
  LastRecordID: PID): variant;
begin
  result := RetrieveDocVariantArray(Table, ObjectName, '', [], CustomFieldsCSV,
    FirstRecordID, LastRecordID);
end;

function TRestOrm.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName: RawUTF8; const FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const; const CustomFieldsCSV: RawUTF8;
  FirstRecordID: PID; LastRecordID: PID): variant;
var
  T: TOrmTable;
  v: variant;
begin
  TVarData(v).VType := varNull;
  if (self <> nil) and
     (Table <> nil) then
  begin
    T := MultiFieldValues(Table, CustomFieldsCSV, FormatSQLWhere, BoundsSQLWhere);
    if T <> nil then
    try
      T.ToDocVariant(v, {readonly=}false); // not readonly -> TDocVariant dvArray
      if FirstRecordID <> nil then
        FirstRecordID^ := T.IDColumnHiddenValue(1);
      if LastRecordID <> nil then
        LastRecordID^ := T.IDColumnHiddenValue(T.RowCount);
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
  const FieldName, FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const): variant;
var
  T: TOrmTable;
  row: integer;
  doc: TDocVariantData absolute result;
begin
  VarClear(result);
  if (self <> nil) and
     (Table <> nil) then
  begin
    T := MultiFieldValues(Table, FieldName, FormatSQLWhere, BoundsSQLWhere);
    if T <> nil then
    try
      doc.InitFast(T.RowCount, dvArray);
      doc.SetCount(T.RowCount);
      for row := 1 to T.RowCount do
        T.GetAsVariant(row, 0, doc.Values[row - 1], false, false, false,
          JSON_OPTIONS_FAST);
    finally
      T.Free;
    end;
  end;
end;

function TRestOrm.RetrieveDocVariant(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const CustomFieldsCSV: RawUTF8): variant;
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
    with Table.RecordProps do
      // handle optimized primary key direct access
      if fCache.IsCached(Table) and
         (length(BoundsSQLWhere) = 1) and
         VarRecToInt64(BoundsSQLWhere[0], Int64(ID)) and
         FieldBitsFromCSV(CustomFieldsCSV, bits) and
         (IdemPropNameU('RowID=?', FormatSQLWhere) or
          IdemPropNameU('ID=?', FormatSQLWhere)) then
      begin
        if IsZero(bits) then
          // get all simple fields if none supplied, like MultiFieldValues()
          bits := SimpleFieldsBits[ooSelect];
        if bits - SimpleFieldsBits[ooSelect] = [] then
        begin
          Rec := Table.Create(self, ID); // use the cache
          try
            Rec.GetAsDocVariant(true, bits, result, nil, {"id"=}true);
          finally
            Rec.Free;
          end;
          exit;
        end;
      end;
    T := MultiFieldValues(Table, CustomFieldsCSV, FormatSQLWhere, BoundsSQLWhere);
    if T <> nil then
    try
      T.ToDocVariant(1, result)
    finally
      T.Free;
    end;
  end;
end;

function TRestOrm.RetrieveListObjArray(var ObjArray; Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): boolean;
var
  T: TOrmTable;
begin
  result := false;
  if (self = nil) or
     (Table = nil) then
    exit;
  T := MultiFieldValues(Table, aCustomFieldsCSV, FormatSQLWhere, BoundsSQLWhere);
  if T <> nil then
  try
    result := T.ToObjArray(ObjArray, Table);
  finally
    T.Free;
  end;
end;

procedure TRestOrm.AppendListAsJsonArray(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const OutputFieldName: RawUTF8; W: TJSONSerializer; const CustomFieldsCSV: RawUTF8);
var
  Rec: TOrm;
begin
  if (self = nil) or
     (Table = nil) or
     (W = nil) then
    exit;
  Rec := Table.CreateAndFillPrepare(Self, FormatSQLWhere, BoundsSQLWhere,
    CustomFieldsCSV);
  try
    Rec.AppendFillAsJsonArray(OutputFieldName, W, Rec.FillContext.TableMapFields);
  finally
    Rec.Free;
  end;
end;

function TRestOrm.RTreeMatch(DataTable: TOrmClass;
  const DataTableBlobFieldName: RawUTF8; RTreeTable: TOrmRTreeClass;
  const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
var
  Blob: PRttiProp;
  T: TOrmTable;
  BDouble: TOrmTreeCoords;
  BInteger: TOrmTreeCoordsInteger absolute BDouble;
  Where, SQL: RawUTF8;
  Data, RTree: TOrmProperties;
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (DataTable = nil) or
     (RTreeTable = nil) or
     (DataTableBlobField = '') then
    exit;
  RTree := RTreeTable.RecordProps;
  Data := DataTable.RecordProps;
  Blob := Data.BlobFieldPropFromRawUTF8(DataTableBlobFieldName);
  if Blob = nil then
    exit;
  if RTreeTable.InheritsFrom(TOrmRTree) then
  begin
    TOrmRTree(RTreeTable).BlobToCoord(pointer(DataTableBlobField)^, BDouble);
    for i := 0 to (RTree.RTreeCoordBoundaryFields shr 1) - 1 do
      Where := FormatUTF8('%%>=:(%): and %<=:(%): and ', [{%H-}Where,
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
      Where := FormatUTF8('%%>=:(%): and %<=:(%): and ', [Where,
        RTree.Fields.List[i * 2].Name, BInteger[i].Min,
        RTree.Fields.List[i * 2 + 1].Name, BInteger[i].Max]);
  end
  else
    exit;
  FormatUTF8('select %.RowID from %,% where %.RowID=%.RowID and %%(%,:(%):);',
    [RTree.SQLTableName, Data.SQLTableName, RTree.SQLTableName,
     Data.SQLTableName, Where, RTreeTable.RTreeSQLFunctionName,
     Data.SQLTableName, BinToBase64WithMagic(DataTableBlobField)], SQL);
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
  const SQL: RawUTF8): TOrmTable;
var
  JSON: RawUTF8;
begin
  JSON := EngineList(SQL, false);
  if JSON <> '' then
    result := TOrmTableJSON.CreateFromTables(Tables, SQL, JSON)
  else
    result := nil;
end;

function TRestOrm.ExecuteJson(const Tables: array of TOrmClass;
  const SQL: RawUTF8; ForceAJAX: boolean; ReturnedRowCount: PPtrInt): RawJSON;
begin
  result := EngineList(SQL, ForceAJAX, ReturnedRowCount);
end;

function TRestOrm.Execute(const aSQL: RawUTF8): boolean;
begin
  result := EngineExecute(aSQL);
end;

function TRestOrm.ExecuteFmt(const SQLFormat: RawUTF8;
  const Args: array of const): boolean;
var
  SQL: RawUTF8;
begin
  SQL := FormatUTF8(SQLFormat, Args);
  result := EngineExecute(SQL);
end;

function TRestOrm.ExecuteFmt(const SQLFormat: RawUTF8;
  const Args, Bounds: array of const): boolean;
var
  SQL: RawUTF8;
begin
  SQL := FormatUTF8(SQLFormat, Args, Bounds);
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

function TRestOrm.Add(Value: TOrm; const CustomCSVFields: RawUTF8;
  ForceID, DoNotAutoComputeFields: boolean): TID;
var
  f: TFieldBits;
begin
  with Value.RecordProps do
    if CustomCSVFields = '*' then
      // FieldBitsFromCSV('*') will use [ooSelect]
      f := SimpleFieldsBits[ooInsert]
    else
      f := FieldBitsFromCSV(CustomCSVFields);
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
  json: RawUTF8;
begin
  if Value = nil then
  begin
    result := 0;
    exit;
  end;
  t := fModel.GetTableIndexExisting(POrmClass(Value)^);
  GetJSONValuesForAdd(t, Value, ForceID, DoNotAutoComputeFields,
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
  // here fCache.Notify is not called, since the JSONValues is verbose
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
  JSONValues: RawUTF8;
  t: integer;
  FieldBits: TFieldBits;
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
    Value.ComputeFieldsBeforeWrite(self, oeUpdate); // update oftModTime fields
  if IsZero(CustomFields) then
    if (Value.FillContext <> nil) and
       (Value.FillContext.Table <> nil) and
       (Value.FillContext.TableMapRecordManyInstances = nil) then
      // within FillPrepare/FillOne loop: update ID, TModTime and mapped fields
      FieldBits := Value.FillContext.TableMapFields +
        Value.RecordProps.FieldBits[oftModTime]
    else
      // update all simple/custom fields (also for FillPrepareMany)
      FieldBits := Value.RecordProps.SimpleFieldsBits[ooUpdate]
  else
    // CustomFields<>[] -> update specified (and TModTime fields)
    if DoNotAutoComputeFields then
      FieldBits := CustomFields
    else
      FieldBits := CustomFields + Value.RecordProps.FieldBits[oftModTime];
  if IsZero(FieldBits) then
  begin
    result := true; // a TOrm with NO simple fields (e.g. ID/blob pair)
    exit;
  end;
  fCache.Notify(Value, ooUpdate); // will serialize Value (JSONValues may not be enough)
  JSONValues := Value.GetJSONValues(true, false, FieldBits);
  WriteLock;
  try
    // may be within a batch in another thread
    result := EngineUpdate(t, Value.IDValue, JSONValues);
  finally
    WriteUnLock;
  end;
end;

function TRestOrm.Update(Value: TOrm; const CustomCSVFields: RawUTF8;
  DoNotAutoComputeFields: boolean): boolean;
begin
  if (self = nil) or
     (Value = nil) then
    result := false
  else
    result := Update(Value, Value.RecordProps.FieldBitsFromCSV(CustomCSVFields),
      DoNotAutoComputeFields);
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
end;

function TRestOrm.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUTF8; const FieldValue: array of const): boolean;
var
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := UpdateField(Table, 'RowID', [ID], FieldName, FieldValue);
  if result then
    fCache.NotifyDeletion(t, ID);
end;

function TRestOrm.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUTF8; const WhereFieldValue: array of const;
  const FieldName: RawUTF8; const FieldValue: array of const): boolean;
var
  t: integer;
  SetValue, WhereValue: RawUTF8;
begin
  result := false;
  if (length(FieldValue) <> 1) or
     (WhereFieldName = '') or
     (length(WhereFieldValue) <> 1) then
    exit;
  VarRecToInlineValue(WhereFieldValue[0], WhereValue);
  VarRecToInlineValue(FieldValue[0], SetValue);
  t := fModel.GetTableIndexExisting(Table);
  result := EngineUpdateField(t, FieldName, SetValue,
    WhereFieldName, WhereValue);
  // warning: this may not update the internal cache
end;

function TRestOrm.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
var
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := UpdateField(Table, 'RowID', ID, FieldName, FieldValue);
  if result then
    fCache.NotifyDeletion(t, ID);
end;

function TRestOrm.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUTF8; const WhereFieldValue: variant;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
var
  t: integer;
  value, where: RawUTF8;
begin
  VariantToInlineValue(WhereFieldValue, where);
  VariantToInlineValue(FieldValue, value);
  t := fModel.GetTableIndexExisting(Table);
  result := EngineUpdateField(t, FieldName, value, WhereFieldName, where);
  // warning: this may not update the internal cache
end;

function TRestOrm.UpdateField(Table: TOrmClass; const IDs: array of Int64;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
var
  value, where: RawUTF8;
  t: integer;
begin
  t := fModel.GetTableIndexExisting(Table);
  VariantToInlineValue(FieldValue, value);
  where := SelectInClause('RowID', IDs, '', INLINED_MAX);
  if length(IDs) <= INLINED_MAX then
    result := ExecuteFmt('update % set %=:(%): where %',
      [Table.SQLTableName, FieldName, value, where])
  else
    // don't cache such a statement
    result := ExecuteFmt('update % set %=% where %',
      [Table.SQLTableName, FieldName, value, where]);
  if result then
    fCache.NotifyDeletions(t, IDs);
end;

function TRestOrm.EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
  const FieldName: RawUTF8; Increment: Int64): boolean;
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

function TRestOrm.{%H-}EngineBatchSend(Table: TOrmClass; var Data: RawUTF8;
  var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
begin
  raise EOrmException.CreateUTF8('BATCH not supported by %', [self]);
end;

function TRestOrm.UpdateFieldIncrement(Table: TOrmClass; ID: TID;
  const FieldName: RawUTF8; Increment: Int64): boolean;
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
  Action: TOrmEvent; ErrorMsg: PRawUTF8): boolean;
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
  const SQLWhere: RawUTF8; var IDs: TIDDynArray): boolean;
var
  t, i: PtrInt;
begin
  t := fModel.GetTableIndexExisting(Table);
  result := false;
  if OneFieldValues(Table, 'RowID', SQLWhere, TInt64DynArray(IDs)) and
     (IDs <> nil) then
  begin
    for i := 0 to length(IDs) - 1 do
      if not RecordCanBeUpdated(Table, IDs[i], oeDelete) then
        exit;
    fCache.NotifyDeletions(t, TInt64DynArray(IDs));
  end;
  result := true;
end;

function TRestOrm.InternalBatchStart(Method: TURIMethod;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false;
end;

procedure TRestOrm.InternalBatchStop;
begin
  raise EOrmException.CreateUTF8('Unexpected %.InternalBatchStop',[self]);
end;

function TRestOrm.Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean;
var
  IDs: TIDDynArray;
begin
  if InternalDeleteNotifyAndGetIDs(Table, SQLWhere, IDs) then
  begin
    WriteLock;
    try // may be within a batch in another thread
      result := EngineDeleteWhere(fModel.GetTableIndexExisting(Table), SQLWhere, IDs);
    finally
      WriteUnLock;
    end;
  end
  else
    result := false;
end;

function TRestOrm.Delete(Table: TOrmClass; const FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const): boolean;
var
  where: RawUTF8;
begin
  where := FormatUTF8(FormatSQLWhere, [], BoundsSQLWhere);
  result := Delete(Table, where);
end;

function TRestOrm.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; out BlobData: RawBlob): boolean;
var
  blob: PRttiProp;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) then
    exit;
  blob := Table.RecordProps.BlobFieldPropFromRawUTF8(BlobFieldName);
  if blob = nil then
    exit;
  result := EngineRetrieveBlob(fModel.GetTableIndexExisting(Table), aID,
    blob, BlobData);
end;

function TRestOrm.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; out BlobStream: TCustomMemoryStream): boolean;
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
  BlobStream.Seek(0, soFromBeginning); // rewind
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; const BlobData: RawBlob): boolean;
var
  blob: PRttiProp;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) or
     not RecordCanBeUpdated(Table, aID, oeUpdate) then
    exit;
  blob := Table.RecordProps.BlobFieldPropFromRawUTF8(BlobFieldName);
  if blob = nil then
    exit;
  result := EngineUpdateBlob(fModel.GetTableIndexExisting(Table), aID, blob,
    BlobData);
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; BlobData: TStream): boolean;
var
  data: RawBlob;
  L: integer;
begin
  result := false;
  if (self = nil) or
     (BlobData = nil) then
    exit;
  L := BlobData.Seek(0, soFromEnd);
  SetLength(data, L);
  BlobData.Seek(0, soFromBeginning);
  if BlobData.Read(pointer(data)^, L) <> L then
    exit;
  result := UpdateBlob(Table, aID, BlobFieldName, data);
end;

function TRestOrm.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; BlobData: pointer; BlobSize: integer): boolean;
var
  data: RawBlob;
begin
  if (self = nil) or
     (BlobData = nil) or
     (BlobSize < 0) then
    result := false
  else
  begin
    SetString(data, PAnsiChar(BlobData), BlobSize);
    result := UpdateBlob(Table, aID, BlobFieldName, data);
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
  with Value.RecordProps do
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
  with Value.RecordProps do
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
  json: RawUTF8; // layout is '{"Table":["cmd":values,...]}'
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

function TRestOrm.{%H-}BatchSend(Table: TOrmClass; var Data: RawUTF8;
  var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
begin
  raise EOrmException.CreateUTF8('BATCH not supported by %', [self]);
end;

function TRestOrm.AsynchBatchStart(Table: TOrmClass;
  SendSeconds: integer; PendingRowThreshold: integer;
  AutomaticTransactionPerRow: integer; Options: TRestBatchOptions): boolean;
begin
  if self = nil then
    result := false
  else
    result := fRest.Run.EnsureBackgroundTimerExists.AsynchBatchStart(Table,
      SendSeconds, PendingRowThreshold, AutomaticTransactionPerRow, Options);
end;

function TRestOrm.AsynchBatchStop(Table: TOrmClass): boolean;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := false
  else
    result := fRest.Run.BackgroundTimer.AsynchBatchStop(Table);
end;

function TRestOrm.AsynchBatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsynchBatchAdd(Value, SendData, ForceID,
      CustomFields, DoNotAutoComputeFields);
end;

function TRestOrm.AsynchBatchRawAdd(Table: TOrmClass;
  const SentData: RawUTF8): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsynchBatchRawAdd(Table, SentData);
end;

procedure TRestOrm.AsynchBatchRawAppend(Table: TOrmClass;
  SentData: TTextWriter);
begin
  if (self <> nil) and
     (fRest.Run.BackgroundTimer <> nil) and
     (fRest.Run.BackgroundTimer.BackgroundBatch <> nil) then
    fRest.Run.BackgroundTimer.AsynchBatchRawAppend(Table, SentData);
end;

function TRestOrm.AsynchBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsynchBatchUpdate(Value, CustomFields,
      DoNotAutoComputeFields);
end;

function TRestOrm.AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  if (self = nil) or
     (fRest.Run.BackgroundTimer = nil) or
     (fRest.Run.BackgroundTimer.BackgroundBatch = nil) then
    result := -1
  else
    result := fRest.Run.BackgroundTimer.AsynchBatchDelete(Table, ID);
end;

function TRestOrm.Cache: TRestCache;
begin
  if fCache = nil then
    fCache := TRestCache.Create(self);
  result := fCache;
end;

function TRestOrm.CacheWorthItForTable(aTableIndex: cardinal): boolean;
begin
  result := true; // always worth caching by default
end;

procedure TRestOrm.InternalLog(const Format: RawUTF8; const Args: array of const;
  Level: TSynLogInfo);
begin
  fRest.InternalLog(Format, Args, Level);
end;

function TRestOrm.GetCurrentSessionUserID: TID;
begin
  result := fRest.GetCurrentSessionUserID;
end;


end.

