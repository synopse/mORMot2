/// ORM Types and Classes for the Client side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.client;

{
  *****************************************************************************

   Client-Side Object-Relational-Mapping (ORM) Process
    - TRestOrmClient Abstract Client
    - TRestOrmClientUri REST Client from URI

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
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core, // for TOrm and IRestOrm
  mormot.orm.rest,
  mormot.soa.core,
  mormot.db.core,
  mormot.rest.core;


{ ************ TRestOrmClient Abstract Client }

type
  /// possible call parameters for TOnTableUpdate Event
  TOnTableUpdateState = (
    tusPrepare,
    tusChanged,
    tusNoChange);

  /// used by TRestOrmClientUri.UpdateFromServer() to let the client
  // perform the rows update (for Marked[] e.g.)
  TOnTableUpdate = procedure(
    aTable: TOrmTableJson; State: TOnTableUpdateState) of object;

  /// used by TRestOrmClientUri.Update() to let the client
  // perform the record update (refresh associated report e.g.)
  TOnRecordUpdate = procedure(Value: TOrm) of object;

  /// a generic REpresentational State Transfer (REST) client
  // - is RESTful (i.e. URI) remotely implemented (TRestOrmClientUri e.g.)
  // - or for direct access to a database (TRestOrmClientDB e.g.)
  TRestOrmClient = class(TRestOrm, IRestOrmClient)
  protected
    fForceBlobTransfert: TBooleanDynArray;
    fOnTableUpdate: TOnTableUpdate;
    fOnRecordUpdate: TOnRecordUpdate;
    fBatchCurrent: TRestBatch;
    function GetForceBlobTransfert: boolean;
    procedure SetForceBlobTransfert(Value: boolean);
    function GetForceBlobTransfertTable(aTable: TOrmClass): boolean;
    procedure SetForceBlobTransfertTable(aTable: TOrmClass; aValue: boolean);
    /// get a member from its ID
    // - implements REST GET collection
    // - returns the data of this object as JSON
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function ClientRetrieve(TableModelIndex: integer; ID: TID;
      ForUpdate: boolean; var InternalState: cardinal; var Resp: RawUtf8): boolean;
       virtual; abstract;
    /// this method is called before updating any record
    // - should return FALSE to force no update
    // - can be use to update some field values just before saving to the database
    // (e.g. for digital signing purpose)
    // - this default method just return TRUE (i.e. OK to update)
    function BeforeUpdateEvent(Value: TOrm): boolean; virtual;
    /// create a new member
    // - implements REST POST collection
    // - URI is 'ModelRoot/TableName' with POST method
    // - if SendData is true, content of Value is sent to the server as JSON
    // - if ForceID is true, client sends the Value.ID field to use this ID
    // - server must return Status 201/HTTP_CREATED on success
    // - server must send on success an header entry with
    // $ Location: ModelRoot/TableName/TableID
    // - on success, returns the new RowID value; on error, returns 0
    // - on success, Value.ID is updated with the new RowID
    // - if aValue is TOrmFts3, Value.ID is stored to the virtual table
    // - this overridden method will send BLOB fields, if ForceBlobTransfert is set
    function InternalAdd(Value: TOrm; SendData: boolean;
      CustomFields: PFieldBits; ForceID: boolean;
      DoNotAutoComputeFields: boolean): TID; override;
  public
    /// release internal used instances
    destructor Destroy; override;
    /// overridden method which will call ClientRetrieve()
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    /// implements IRestOrmClient methods with an internal TRestBatch instance
    function BatchStart(aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = []): boolean; virtual;
    function BatchStartAny(AutomaticTransactionPerRow: cardinal;
      Options: TRestBatchOptions = []): boolean;
    function BatchAdd(Value: TOrm; SendData: boolean; ForceID: boolean = false;
      const CustomFields: TFieldBits = []): integer;
    function BatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer; overload;
    function BatchUpdate(Value: TOrm; const CustomFieldsCsv: RawUtf8;
      DoNotAutoComputeFields: boolean = false): integer; overload;
    function BatchDelete(ID: TID): integer; overload;
    function BatchDelete(Table: TOrmClass; ID: TID): integer; overload;
    function BatchCount: integer;
    function BatchSend(var Results: TIDDynArray): integer; overload;
    procedure BatchAbort;
  public
    /// update a member
    // - implements REST PUT collection
    // - URI is 'ModelRoot/TableName/TableID' with PUT method
    // - server must return Status 200/HTTP_SUCCESS OK on success
    // - this overridden method will call BeforeUpdateEvent and also update BLOB
    // fields, if any ForceBlobTransfert is set and CustomFields=[]
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; override;
    /// get a member from its ID
    // - implements REST GET collection
    // - URI is 'ModelRoot/TableName/TableID' with GET method
    // - server must return Status 200/HTTP_SUCCESS OK on success
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; override;
    /// get a member from its ID
    // - implements REST GET collection
    // - URI is 'ModelRoot/TableName/TableID' with GET method
    // - returns true on server returned 200/HTTP_SUCCESS OK success, false on error
    // - set Refreshed to true if the content changed
    function Refresh(aID: TID; Value: TOrm;
      var Refreshed: boolean): boolean;
    /// ask the server for its current internal state revision counter
    // - this counter is incremented every time the database is modified
    // - the returned value is 0 if the database doesn't support this feature
    // - TOrmTable does compare this value with its internal one to check if
    // its content must be updated
    // - is defined here and not in IRestOrmClient since it is very specific
    function ServerInternalState: cardinal; virtual; abstract;
    /// check if the data may have changed of the server for this objects, and
    // update it if possible
    // - only working types are TOrmTableJson and TOrm descendants
    // - make use of the InternalState function to check the data content revision
    // - return true if Data is updated successfully, or false on any error
    // during data retrieval from server (e.g. if the TOrm has been deleted)
    // - if Data contains only one TOrmTableJson, PCurrentRow can point to the
    // current selected row of this table, in order to refresh its value
    // - use this method to refresh the client UI, e.g. via a timer
    // - is defined here and not in IRestOrmClient since it is very specific
    function UpdateFromServer(const Data: array of TObject;
      out Refreshed: boolean; PCurrentRow: PInteger = nil): boolean; virtual; abstract;
    /// send a flush command to the remote Server cache
    // - this method will remotely call the Cache.Flush() methods of the server
    // instance, to force cohesion of the data
    // - ServerCacheFlush() with no parameter will flush all stored JSON content
    // - ServerCacheFlush(aTable) will flush the cache for a given table
    // - ServerCacheFlush(aTable,aID) will flush the cache for a given record
    function ServerCacheFlush(aTable: TOrmClass = nil;
      aID: TID = 0): boolean; virtual; abstract;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - default SQL statement is 'SELECT ID FROM TableName;' (i.e. retrieve
    // the list of all ID of this collection members)
    // - optional SqlSelect parameter to change the returned fields
    // as in 'SELECT SqlSelect FROM TableName;'
    // - optional SqlWhere parameter to change the search range or ORDER
    // as in 'SELECT SqlSelect FROM TableName WHERE SqlWhere;'
    // - using inlined parameters via :(...): in SqlWhere is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    function List(const Tables: array of TOrmClass;
      const SqlSelect: RawUtf8 = 'RowID';
      const SqlWhere: RawUtf8 = ''): TOrmTable; virtual; abstract;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUtf8() function, replacing all '%' chars with Args[] values
    // - using inlined parameters via :(...): in SqlWhereFormat is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8;
      const Args: array of const): TOrmTable; overload;
    /// retrieve a list of members as a TOrmTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUtf8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := ListFmt([TOrm],'Name','ID=?',[],[aID]);
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8;
      const Args, Bounds: array of const): TOrmTable; overload;
    /// begin a transaction
    // - implements REST BEGIN collection
    // - in aClient-Server environment with multiple Clients connected at the
    // same time, you should better use BATCH process, specifying a positive
    // AutomaticTransactionPerRow parameter to BatchStart()
    // - this version retries a TranslationBegin() to be successfull within
    // a supplied number of times
    // - will retry every 100 ms for "Retries" times (excluding the connection
    // time in this 100 ms time period
    // - default is to retry 10 times, i.e. within 2 second timeout
    // - in the current implementation, the aTable parameter is not used yet
    // - typical usage should be for instance:
    // !if Client.TransactionBeginRetry(TOrmPeopleObject,20) then
    // !try
    // !  // .... modify the database content, raise exceptions on error
    // !  Client.Commit;
    // !except
    // !  Client.RollBack; //  in case of error
    // !end;
    function TransactionBeginRetry(aTable: TOrmClass;
      Retries: integer = 10): boolean;

    /// if set to TRUE, all BLOB fields of all tables will be transferred
    // between the Client and the remote Server
    // - i.e. Add() Update() will use Blob-related RESTful PUT/POST request
    // - i.e. Retrieve() will use Blob-related RESTful GET request
    // - note that the Refresh method won't handle BLOB fields, even if this
    // property setting is set to TRUE
    // - by default, this property is set to FALSE, which setting will spare
    // bandwidth and CPU
    // - this property is global to all tables of the model - you can also use
    // ForceBlobTransfertTable[] to force it for a particular table
    property ForceBlobTransfert: boolean
      read GetForceBlobTransfert write SetForceBlobTransfert;
    /// if set to TRUE for a specified table of the model, all BLOB fields of
    // this tables will be transferred between the Client and the remote Server
    // - i.e. Add() Update() will use BLOB-related RESTful PUT/POST request for
    // this table
    // - i.e. Retrieve() will use BLOB-related RESTful GET request for
    // this table
    // - note that the Refresh method won't handle BLOB fields, even if this
    // property setting is set to TRUE
    // - by default, all items of this property are set to FALSE, which
    // setting will spare bandwidth and CPU
    // - this property is particular to a given tables of the model - you can
    // also use ForceBlobTransfert to force it for a all tables of this model
    property ForceBlobTransfertTable[aTable: TOrmClass]: boolean
      read GetForceBlobTransfertTable write SetForceBlobTransfertTable;
    /// this Event is called by UpdateFromServer() to let the Client adapt to
    // some rows update (for Marked[] e.g.)
    property OnTableUpdate: TOnTableUpdate
      read fOnTableUpdate write fOnTableUpdate;
    /// this Event is called by Update() to let the client
    // perform the record update (refresh associated report e.g.)
    property OnRecordUpdate: TOnRecordUpdate
      read fOnRecordUpdate write fOnRecordUpdate;
  end;



{ ************ TRestOrmClientUri REST Client from URI }

type
  /// main entry point of TRestOrmClientUri, redirecting to TRestClientUri.Uri()
  TOnRestOrmClientUri = function(const url, method: RawUtf8; Resp: PRawUtf8 = nil;
    Head: PRawUtf8 = nil; SendData: PRawUtf8 = nil; outStatus: PCardinal = nil): integer of object;

  /// URI-oriented REpresentational State Transfer (REST) client
  // - will later on be implemented over local, Windows messages, named pipe,
  // HTTP/1.1 or WebSockets
  // - works in conjunction with TRestClientUri from mormot.rest.client.pas
  TRestOrmClientUri = class(TRestOrmClient)
  protected
    // ForUpdate=true->LOCK ForUpdate=false->GET
    function URIGet(Table: TOrmClass; ID: TID; var Resp: RawUtf8;
      ForUpdate: boolean = false; outStatus: PCardinal = nil): integer;
  public
    /// will redirect any client call to TRestClientUri.Uri()
    // - is injected by TRestClientUri.SetOrmInstance
    URI: TOnRestOrmClientUri;
    // overridden methods actually calling Uri()
    function ClientRetrieve(TableModelIndex: integer; ID: TID;
      ForUpdate: boolean; var InternalState: cardinal;
      var Resp: RawUtf8): boolean; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    function EngineExecute(const SQL: RawUtf8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer;
      const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue,
      WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function EngineBatchSend(Table: TOrmClass; var Data: RawUtf8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; override;
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUtf8): TOrmTable; override;
    function List(const Tables: array of TOrmClass; const SqlSelect: RawUtf8 = 'RowID';
      const SqlWhere: RawUtf8 = ''): TOrmTable; override;
    function UnLock(Table: TOrmClass; aID: TID): boolean; override;
    function TransactionBegin(aTable: TOrmClass;
      SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED): boolean; override;
    procedure Commit(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED;
      RaiseException: boolean = false); override;
    procedure RollBack(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED); override;
    function ServerInternalState: cardinal; override;
    function UpdateFromServer(const Data: array of TObject; out Refreshed: boolean;
      PCurrentRow: PInteger = nil): boolean; override;
    function ServerCacheFlush(aTable: TOrmClass = nil;
      aID: TID = 0): boolean; override;
  end;


implementation


{ ************ TRestOrmClient Abstract Client }

{ TRestOrmClient }

function TRestOrmClient.GetForceBlobTransfert: boolean;
var
  i: PtrInt;
begin
  result := false;
  if fForceBlobTransfert = nil then
    exit;
  for i := 0 to fModel.TablesMax do
    if not fForceBlobTransfert[i] then
      exit;
  result := true; // all Tables have BLOB transfert set
end;

procedure TRestOrmClient.SetForceBlobTransfert(Value: boolean);
var
  i: PtrInt;
begin
  Finalize(fForceBlobTransfert);
  if Value then
  begin
    SetLength(fForceBlobTransfert, fModel.TablesMax + 1);
    for i := 0 to fModel.TablesMax do
      fForceBlobTransfert[i] := true;
  end;
end;

function TRestOrmClient.GetForceBlobTransfertTable(aTable: TOrmClass): boolean;
begin
  if fForceBlobTransfert = nil then
    result := false
  else
    result := fForceBlobTransfert[fModel.GetTableIndexExisting(aTable)];
end;

procedure TRestOrmClient.SetForceBlobTransfertTable(aTable: TOrmClass;
  aValue: boolean);
var
  i: PtrInt;
begin
  i := fModel.GetTableIndexExisting(aTable);
  if fForceBlobTransfert = nil then
    if aValue then
      SetLength(fForceBlobTransfert, fModel.TablesMax + 1)
    else
      exit; // nothing to set
  fForceBlobTransfert[i] := aValue;
end;

function TRestOrmClient.InternalAdd(Value: TOrm; SendData: boolean;
  CustomFields: PFieldBits; ForceID, DoNotAutoComputeFields: boolean): TID;
begin
  result := inherited InternalAdd(Value, SendData, CustomFields, ForceID,
    DoNotAutoComputeFields);
  if (result > 0) and
     (fForceBlobTransfert <> nil) and
     fForceBlobTransfert[fModel.GetTableIndexExisting(POrmClass(Value)^)] then
    UpdateBlobFields(Value);
end;

destructor TRestOrmClient.Destroy;
begin
  FreeAndNil(fBatchCurrent);
  inherited Destroy; // fCache.Free
end;

function TRestOrmClient.BatchStart(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions): boolean;
begin
  if (self = nil) or
     (fBatchCurrent <> nil) then
    result := false
  else
  begin
    fBatchCurrent := TRestBatch.Create(self, aTable,
      AutomaticTransactionPerRow, Options, 1 shl 17, {withinrest=}true);
    result := true;
  end;
end;

function TRestOrmClient.BatchStartAny(AutomaticTransactionPerRow: cardinal;
  Options: TRestBatchOptions): boolean;
begin
  result := BatchStart(nil, AutomaticTransactionPerRow, Options);
end;

function TRestOrmClient.BatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Add(Value, SendData, ForceID, CustomFields);
end;

function TRestOrmClient.BatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (Value = nil) or
     (fBatchCurrent = nil) or
     (Value.IDValue <= 0) or
     not BeforeUpdateEvent(Value) then
    result := -1
  else
    result := fBatchCurrent.Update(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRestOrmClient.BatchUpdate(Value: TOrm; const CustomFieldsCsv: RawUtf8;
  DoNotAutoComputeFields: boolean): integer;
begin
  if (self = nil) or
     (Value = nil) or
     (fBatchCurrent = nil) or
     (Value.IDValue <= 0) or
     not BeforeUpdateEvent(Value) then
    result := -1
  else
    result := fBatchCurrent.Update(Value,
      Value.Orm.FieldBitsFromCsv(CustomFieldsCsv), DoNotAutoComputeFields);
end;

function TRestOrmClient.BatchDelete(ID: TID): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Delete(ID);
end;

function TRestOrmClient.BatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Delete(Table, ID);
end;

function TRestOrmClient.BatchCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fBatchCurrent.Count;
end;

function TRestOrmClient.BatchSend(var Results: TIDDynArray): integer;
begin
  if self <> nil then
  try
    result := BatchSend(fBatchCurrent, Results);
  finally
    FreeAndNil(fBatchCurrent);
  end
  else
    result := HTTP_BADREQUEST;
end;

procedure TRestOrmClient.BatchAbort;
begin
  if self <> nil then
    FreeAndNil(fBatchCurrent);
end;

function TRestOrmClient.EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8;
var
  dummy: cardinal;
begin
  if not ClientRetrieve(TableModelIndex, ID, false, dummy, result) then
    result := '';
end;

function TRestOrmClient.Retrieve(aID: TID; Value: TOrm; ForUpdate: boolean): boolean;
var
  resp: RawUtf8;
  tableindex: integer;
  state: cardinal;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) or
     (Value = nil) then
    exit;
  tableindex := fModel.GetTableIndexExisting(POrmClass(Value)^);
  if ForUpdate then
  begin
    if not fModel.Lock(tableindex, aID) then
      exit; // error marking as locked by the client
  end
  else
  begin
    resp := fCache.Retrieve(tableindex, aID);
    if resp <> '' then
    begin
      Value.FillFrom(resp);
      Value.IDValue := aID; // JSON object may not contain the ID
      result := true;
      exit; // fast retrieved from internal Client cache (BLOBs ignored)
    end;
  end;
  try
    state := Value.InternalState;
    if ClientRetrieve(tableindex, aID, ForUpdate, state, resp) then
    begin
      Value.InternalState := state;
      if not ForUpdate then
        fCache.Notify(tableindex, aID, resp, ooSelect);
      Value.FillFrom(resp);
      Value.IDValue := aID; // JSON object may not contain the ID
      if (fForceBlobTransfert <> nil) and
         fForceBlobTransfert[tableindex] then
        result := RetrieveBlobFields(Value)
      else
        result := true;
      ForUpdate := false; // any exception shall unlock the record
    end;
  finally
    if ForUpdate then
      fModel.UnLock(tableindex, aID);
  end;
end;

function TRestOrmClient.Update(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): boolean;
begin
  result := BeforeUpdateEvent(Value) and
    inherited Update(Value, CustomFields, DoNotAutoComputeFields);
  if result then
  begin
    if (fForceBlobTransfert <> nil) and
       IsZero(CustomFields) and
       fForceBlobTransfert[fModel.GetTableIndexExisting(POrmClass(Value)^)] then
      result := UpdateBlobFields(Value);
    if result and assigned(OnRecordUpdate) then
      OnRecordUpdate(Value);
  end;
end;

function TRestOrmClient.BeforeUpdateEvent(Value: TOrm): boolean;
begin
  result := true; // by default, just allow the update to proceed
end;

function TRestOrmClient.Refresh(aID: TID; Value: TOrm;
  var Refreshed: boolean): boolean;
var
  resp, original: RawUtf8;
  state: cardinal;
begin
  result := false;
  if (aID > 0) and
     (self <> nil) and
     (Value <> nil) then
  begin
    state := Value.InternalState;
    if ClientRetrieve(fModel.GetTableIndexExisting(POrmClass(Value)^),
        aID, False, state, resp) then
    begin
      Value.InternalState := state;
      original := Value.GetJsonValues(
        IsNotAjaxJson(pointer(resp)), true, ooSelect);
      resp := TrimU(resp);
      if (resp <> '') and
         (resp[1] = '[') then // '[{....}]' -> '{...}'
        resp := copy(resp, 2, length(resp) - 2);
      if original <> resp then
      begin
        // did the content really change?
        Refreshed := true;
        Value.FillFrom(resp);
      end;
      result := true;
    end;
  end;
end;

function TRestOrmClient.ListFmt(const Tables: array of TOrmClass;
  const SqlSelect, SqlWhereFormat: RawUtf8;
  const Args: array of const): TOrmTable;
begin
  result := List(Tables, SqlSelect, FormatUtf8(SqlWhereFormat, Args));
end;

function TRestOrmClient.ListFmt(const Tables: array of TOrmClass;
  const SqlSelect, SqlWhereFormat: RawUtf8;
  const Args, Bounds: array of const): TOrmTable;
begin
  result := List(Tables, SqlSelect, FormatUtf8(SqlWhereFormat, Args, Bounds));
end;

function TRestOrmClient.TransactionBeginRetry(aTable: TOrmClass;
  Retries: integer): boolean;
begin
  if Retries > 50 then
    Retries := 50; // avoid loop for more than 10 seconds
  repeat
    result := TransactionBegin(aTable, CONST_AUTHENTICATION_NOT_USED);
    if result then
      exit;
    dec(Retries);
    if Retries <= 0 then
      break;
    SleepHiRes(100);
  until false;
end;



{ ************ TRestOrmClientUri REST Client from URI }

{ TRestOrmClientUri }

procedure TRestOrmClientUri.Commit(SessionID: cardinal; RaiseException: boolean);
begin
  inherited Commit(CONST_AUTHENTICATION_NOT_USED, RaiseException);
  // inherited Commit = reset fTransactionActiveSession flag
  // END on 'root' URI
  Uri(fModel.Root, 'END');
end;

function TRestOrmClientUri.TransactionBegin(aTable: TOrmClass;
  SessionID: cardinal): boolean;
begin
  result := inherited TransactionBegin(aTable, CONST_AUTHENTICATION_NOT_USED);
  if result then
    // fTransactionActiveSession flag was not already set
    if aTable = nil then
      // BEGIN on 'root' URI
      result := Uri(fModel.Root, 'BEGIN') in [HTTP_SUCCESS, HTTP_NOCONTENT]
    else
      // BEGIN on 'root/table' URI
      result := Uri(fModel.Uri[aTable], 'BEGIN') in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

procedure TRestOrmClientUri.RollBack(SessionID: cardinal);
begin
  inherited RollBack(CONST_AUTHENTICATION_NOT_USED);
  // inherited RollBack = reset fTransactionActiveSession flag
  // ABORT on 'root' URI
  Uri(fModel.Root, 'ABORT');
end;

function TRestOrmClientUri.UnLock(Table: TOrmClass; aID: TID): boolean;
begin
  if (self = nil) or
     not fModel.UnLock(Table, aID) then
    // was not locked by the client
    result := false
  else
    // UNLOCK on 'root/table/ID' URI
    result := Uri(fModel.GetUriID(Table, aID), 'UNLOCK') in
      [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestOrmClientUri.UriGet(Table: TOrmClass; ID: TID;
  var Resp: RawUtf8; ForUpdate: boolean; outStatus: PCardinal): integer;
const
  METHOD: array[boolean] of RawUtf8 = (
    'GET', 'LOCK');
begin
  // GET/LOCK on 'root/table/ID' URI
  result := Uri(fModel.GetUriID(Table, ID),
    METHOD[ForUpdate], @Resp, nil, nil, outStatus);
end;

function TRestOrmClientUri.ClientRetrieve(TableModelIndex: integer; ID: TID;
  ForUpdate: boolean; var InternalState: cardinal; var Resp: RawUtf8): boolean;
begin
  if cardinal(TableModelIndex) <= cardinal(fModel.TablesMax) then
    result := URIGet(fModel.Tables[TableModelIndex],
      ID, Resp, ForUpdate, @InternalState) = HTTP_SUCCESS
  else
    result := false;
end;

function TRestOrmClientUri.EngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
begin
  if (self = nil) or
     (SQL = '') or
     (ReturnedRowCount <> nil) or
     // GET on 'root' URI with SQL as body (not fully standard HTTP)
     (Uri(fModel.Root, 'GET', @result, nil, @SQL) <> HTTP_SUCCESS) then
    result := '';
end;

function TRestOrmClientUri.EngineExecute(const SQL: RawUtf8): boolean;
begin
  // POST on 'root' URI with SQL as body
  result := Uri(fModel.Root, 'POST', nil, nil, @SQL) in
    [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestOrmClientUri.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  P: PUtf8Char;
  url, head: RawUtf8;
begin
  result := 0;
  url := fModel.Uri[fModel.Tables[TableModelIndex]];
  // POST on 'root/table' URI with JSON object as body
  if Uri(url, 'POST', nil, @head, @SentData) <> HTTP_CREATED then
    // response must be '201 Created'
    exit;
  P := pointer(head); // we need to check the headers
  if P <> nil then
    repeat
      // find ID from 'Location: Member Entry URI' header entry
      if IdemPChar(P, 'LOCATION:') then
      begin
        // 'Location: root/People/11012' e.g.
        inc(P, 9);
        while P^ > #13 do
          inc(P); // go to end of line
        P^ := #0; // make line asciiz, even if ended with #13
        while P[-1] in ['0'..'9'] do
          dec(P); // get all number chars
        if P[-1] = '-' then
          dec(P);
        result := GetInt64(P); // get numerical value at the end of the URI
        exit;
      end;
      while not (P^ in [#0, #13]) do
        inc(P);
      if P^ = #0 then
        break
      else
        inc(P);
      if P^ = #10 then
        inc(P);
    until P^ = #0;
end;

function TRestOrmClientUri.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUtf8): boolean;
var
  url: RawUtf8;
begin
  // PUT on 'root/table/ID' URI
  url := fModel.GetUriID(fModel.Tables[TableModelIndex], ID);
  result := Uri(url, 'PUT', nil, nil, @SentData) in
    [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestOrmClientUri.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
var
  url: RawUtf8;
begin
  // DELETE on 'root/table/ID' URI
  url := fModel.GetUriID(fModel.Tables[TableModelIndex], ID);
  result := Uri(url, 'DELETE') in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestOrmClientUri.EngineDeleteWhere(TableModelIndex: integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  url: RawUtf8;
begin
  // DELETE on 'root/table?where=WhereClause" URI
  url := fModel.GetUri(fModel.Tables[TableModelIndex]) +
    '?where=' + UrlEncode(SqlWhere);
  result := Uri(url, 'DELETE') in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestOrmClientUri.EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
  BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  url: RawUtf8;
begin
  if (self = nil) or
     (aID <= 0) or
     (BlobField = nil) then
    result := false
  else
  begin
    // GET on 'root/table/ID/BlobFieldName' URI
    url := fModel.GetUriCallBack(BlobField^.NameUtf8,
      fModel.Tables[TableModelIndex], aID);
    result := Uri(url, 'GET', @BlobData) = HTTP_SUCCESS;
  end;
end;

function TRestOrmClientUri.EngineUpdateBlob(TableModelIndex: integer; aID: TID;
  BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  url, head: RawUtf8;
begin
  head := 'Content-Type: application/octet-stream';
  if (self = nil) or
     (aID <= 0) or
     (BlobField = nil) then
    result := false
  else
  begin
    // PUT on 'root/table/ID/BlobFieldName' URI
    url := fModel.GetUriCallBack(BlobField^.NameUtf8,
      fModel.Tables[TableModelIndex], aID);
    result := Uri(url, 'PUT', nil, @head, @BlobData) in
      [HTTP_SUCCESS, HTTP_NOCONTENT];
  end;
end;

function TRestOrmClientUri.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  url: RawUtf8;
begin
  if TableModelIndex < 0 then
    result := false
  else
  begin
    // PUT on 'root/table?setname=..&set=..&wherename=..&where=..' URI
    FormatUtf8('%?setname=%&set=%&wherename=%&where=%',
      [fModel.Uri[fModel.Tables[TableModelIndex]], SetFieldName,
       UrlEncode(SetValue), WhereFieldName, UrlEncode(WhereValue)], url);
    result := Uri(url, 'PUT') in [HTTP_SUCCESS, HTTP_NOCONTENT];
  end;
end;

function TRestOrmClientUri.EngineBatchSend(Table: TOrmClass;
  var Data: RawUtf8; var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
var
  u, resp: RawUtf8;
  R: PUtf8Char;
  i: PtrInt;
  c: PtrUInt;
  res: Int64;
begin
  // TRest.BatchSend() ensured that Batch contains some data
  try
    // PUT on 'root/Batch' or 'root/Batch/Table' URI
    u := fModel.GetUriCallBack('Batch', Table, 0);
    result := Uri(u, 'PUT', @resp, nil, @Data);
    if result <> HTTP_SUCCESS then
      exit;
    // returned resp shall be an array of integers: '[200,200,...]'
    R := pointer(resp);
    if R <> nil then
      while not (R^ in ['[', #0]) do
        inc(R);
    result := HTTP_BADREQUEST;
    if (R = nil) or
       (R^ <> '[') then
      // invalid response
      exit;
    SetLength(Results, ExpectedResultsCount);
    if IdemPChar(R, '["OK"]') then
    begin
      // to save bandwith if no adding
      for i := 0 to ExpectedResultsCount - 1 do
        Results[i] := HTTP_SUCCESS;
    end
    else
    begin
      inc(R); // jump first '['
      for i := 0 to ExpectedResultsCount - 1 do
      begin
        while (R^ <= ' ') and
              (R^ <> #0) do
          inc(R);
        res := byte(R^) - 48;
        if res <= 9 then
        begin
          inc(R);
          repeat
            c := byte(R^) - 48;
            if c > 9 then
              break;
            res := res * 10 + Int64(c);
            inc(R);
          until false;
        end;
        Results[i] := res;
        while R^ in [#1..' '] do
          inc(R);
        case R^ of
          ',':
            inc(R);
          ']':
            break;
        else
          exit;
        end;
      end;
      if R^ <> ']' then
        exit;
    end;
    result := HTTP_SUCCESS; // returns OK
  finally
    BatchAbort;
  end;
end;

function TRestOrmClientUri.ExecuteList(const Tables: array of TOrmClass;
  const SQL: RawUtf8): TOrmTable;
var
  json: RawUtf8;
  res, state: cardinal;
begin
  if self = nil then
    result := nil
  else
  begin
    // GET on 'root' URI with SQL as body (not fully HTTP compatible)
    res := Uri(fModel.Root, 'GET', @json, nil, @SQL, @state);
    if (res = HTTP_SUCCESS) and
       (json <> '') then
    begin
      result := TOrmTableJson.CreateFromTables(Tables, SQL, json,
        {ownjson=}PRefCnt(PAnsiChar(pointer(json)) - _STRREFCNT)^ = 1);
      result.InternalState := state;
    end
    else
      // no data on error
      result := nil;
  end;
end;

function TRestOrmClientUri.List(const Tables: array of TOrmClass;
  const SqlSelect: RawUtf8; const SqlWhere: RawUtf8): TOrmTable;
var
  json, sql: RawUtf8;
  u: RawUtf8;
  state: cardinal;
begin
  result := nil;
  if high(Tables) < 0 then
  exit;
  // GET Collection
  sql := Model.SqlFromSelectWhere(Tables, SqlSelect, SqlWhere);
  if high(Tables) = 0 then
  begin
    // one Table -> use REST protocol (sql as parameters)
    if not IsRowID(pointer(SqlSelect)) then
      // ID selected by default
      u := '?select=' + UrlEncode(SqlSelect)
    else
      u := '';
    if SqlWhere <> '' then
    begin
      if u <> '' then
        u := u + '&where='
      else
        u := u + '?where=';
      u := u + UrlEncode(SqlWhere);
    end;
    u := Model.Uri[Tables[0]] + u;
    if Uri(u, 'GET', @json, nil, nil, @state) <> HTTP_SUCCESS then
      exit;
  end
  // multiple tables -> send sql statement as HTTP body
  else if Uri(Model.Root,'GET', @json, nil, @sql, @state) <> HTTP_SUCCESS then
    exit;
  if json = '' then
    exit;
  result := TOrmTableJson.CreateFromTables(Tables, sql, json,
    {ownjson=}PRefCnt(PAnsiChar(pointer(json)) - _STRREFCNT)^ = 1);
  result.InternalState := state;
end;

function TRestOrmClientUri.ServerInternalState: cardinal;
begin
  if (self = nil) or
     (fModel = nil) or // avoid GPF
     (Uri(fModel.Root, 'STATE', nil, nil, nil, @result) <> HTTP_SUCCESS) then
    result := cardinal(-1);
end;

function TRestOrmClientUri.UpdateFromServer(const Data: array of TObject;
  out Refreshed: boolean; PCurrentRow: PInteger): boolean;
// notes about refresh mechanism:
// - if server doesn't implement InternalState, its value is 0 -> always refresh
// - if any TOrmTableJson or TOrm belongs to a TRestStorage,
// the Server stated fInternalState=cardinal(-1) for them -> always refresh
var
  i: PtrInt;
  s: cardinal;
  resp: RawUtf8;
  table: TOrmTableJson;
  wasrefreshed: boolean; // to check for each Table refresh
const
  _ST: array[boolean] of TOnTableUpdateState = (
    tusNoChange, tusChanged);
begin
  result := self <> nil;
  Refreshed := false;
  if not result then
    exit; // avoid GPF
  s := ServerInternalState; // get revision s from server
  for i := 0 to high(Data) do
    if Data[i] <> nil then
      if Data[i].InheritsFrom(TOrmTableJson) then
      begin
        table := TOrmTableJson(Data[i]);
        if (table.QuerySql <> '') and
           (table.InternalState <> s) then
        begin
          // refresh needed
          if Uri(fModel.Root, 'GET', @resp, nil, @table.QuerySql, @s) = HTTP_SUCCESS then
          begin
            // refresh after proper GET with SQL sent
            if Assigned(OnTableUpdate) then
              OnTableUpdate(table, tusPrepare);
            wasrefreshed := false;
            if not table.UpdateFrom(resp, wasrefreshed, PCurrentRow) then
              // mark error retrieving new content
              result := false
            else
              // successfully refreshed with new data
              table.InternalState := s;
            if wasrefreshed then
              Refreshed := true;
            if Assigned(OnTableUpdate) then
              OnTableUpdate(table, _ST[wasrefreshed]);
          end
          else
            // mark error retrieving new content
            result := false;
        end;
      end
      else if Data[i].InheritsFrom(TOrm) then
        with TOrm(Data[i]) do
          if (IDValue <> 0) and
             (InternalState <> s) then
          begin
            // refresh needed
            if not Refresh(IDValue, TOrm(Data[i]), Refreshed) then
              // mark error retrieving new content
              result := false;
          end;
end;

function TRestOrmClientUri.ServerCacheFlush(aTable: TOrmClass; aID: TID): boolean;
begin
  if (self = nil) or
     (fModel = nil) then // avoid GPF
    result := false
  else
    result := Uri(fModel.GetUriCallBack('CacheFlush', aTable, aID), 'GET')
      in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;


end.

