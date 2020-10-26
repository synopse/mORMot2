/// ORM Types and Classes for the Client side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.client;

{
  *****************************************************************************

   Client-Side Object-Relational-Mapping (ORM) Process
    - TRestORMClient Abstract Client
    - TRestORMClientURI REST Client from URI

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
  mormot.orm.core, // for TSQLRecord and IRestORM
  mormot.orm.rest,
  mormot.soa.core,
  mormot.db.core,
  mormot.rest.core;


{ ************ TRestORMClient Abstract Client }

type
  /// possible call parameters for TOnTableUpdate Event
  TOnTableUpdateState = (
    tusPrepare, tusChanged, tusNoChange);

  /// used by TRestORMClientURI.UpdateFromServer() to let the client
  // perform the rows update (for Marked[] e.g.)
  TOnTableUpdate = procedure(
    aTable: TSQLTableJSON; State: TOnTableUpdateState) of object;

  /// used by TRestORMClientURI.Update() to let the client
  // perform the record update (refresh associated report e.g.)
  TOnRecordUpdate = procedure(Value: TSQLRecord) of object;

  /// a generic REpresentational State Transfer (REST) client
  // - is RESTful (i.e. URI) remotely implemented (TRestORMClientURI e.g.)
  // - or for direct access to a database (TRestORMClientDB e.g.)
  TRestORMClient = class(TRestORM, IRestORMClient)
  protected
    fForceBlobTransfert: array of boolean;
    fOnTableUpdate: TOnTableUpdate;
    fOnRecordUpdate: TOnRecordUpdate;
    fBatchCurrent: TRestBatch;
    function GetForceBlobTransfert: boolean;
    procedure SetForceBlobTransfert(Value: boolean);
    function GetForceBlobTransfertTable(aTable: TSQLRecordClass): boolean;
    procedure SetForceBlobTransfertTable(aTable: TSQLRecordClass; aValue: boolean);
    /// get a member from its ID
    // - implements REST GET collection
    // - returns the data of this object as JSON
    // - override this method for proper data retrieval from the database engine
    // - this method must be implemented in a thread-safe manner
    function ClientRetrieve(TableModelIndex: integer; ID: TID;
      ForUpdate: boolean; var InternalState: cardinal; var Resp: RawUTF8): boolean;
       virtual; abstract;
    /// this method is called before updating any record
    // - should return FALSE to force no update
    // - can be use to update some field values just before saving to the database
    // (e.g. for digital signing purpose)
    // - this default method just return TRUE (i.e. OK to update)
    function BeforeUpdateEvent(Value: TSQLRecord): boolean; virtual;
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
    // - if aValue is TSQLRecordFTS3, Value.ID is stored to the virtual table
    // - this overridden method will send BLOB fields, if ForceBlobTransfert is set
    function InternalAdd(Value: TSQLRecord; SendData: boolean;
      CustomFields: PSQLFieldBits; ForceID: boolean;
      DoNotAutoComputeFields: boolean): TID; override;
  public
    /// release internal used instances
    destructor Destroy; override;
    /// overridden method which will call ClientRetrieve()
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    /// implements IRestORMClient methods with an internal TRestBatch instance
    function BatchStart(aTable: TSQLRecordClass;
      AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = []): boolean; virtual;
    function BatchStartAny(AutomaticTransactionPerRow: cardinal;
      Options: TRestBatchOptions = []): boolean;
    function BatchAdd(Value: TSQLRecord; SendData: boolean; ForceID: boolean = false;
      const CustomFields: TSQLFieldBits = []): integer;
    function BatchUpdate(Value: TSQLRecord; const CustomFields: TSQLFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function BatchDelete(ID: TID): integer; overload;
    function BatchDelete(Table: TSQLRecordClass; ID: TID): integer; overload;
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
    function Update(Value: TSQLRecord; const CustomFields: TSQLFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; override;
    /// get a member from its ID
    // - implements REST GET collection
    // - URI is 'ModelRoot/TableName/TableID' with GET method
    // - server must return Status 200/HTTP_SUCCESS OK on success
    // - if ForUpdate is true, the REST method is LOCK and not GET: it tries to lock
    // the corresponding record, then retrieve its content; caller has to call
    // UnLock() method after Value usage, to release the record
    function Retrieve(aID: TID; Value: TSQLRecord;
      ForUpdate: boolean = false): boolean; override;
    /// get a member from its ID
    // - implements REST GET collection
    // - URI is 'ModelRoot/TableName/TableID' with GET method
    // - returns true on server returned 200/HTTP_SUCCESS OK success, false on error
    // - set Refreshed to true if the content changed
    function Refresh(aID: TID; Value: TSQLRecord;
      var Refreshed: boolean): boolean;
    /// ask the server for its current internal state revision counter
    // - this counter is incremented every time the database is modified
    // - the returned value is 0 if the database doesn't support this feature
    // - TSQLTable does compare this value with its internal one to check if
    // its content must be updated
    // - is defined here and not in IRestORMClient since it is very specific
    function ServerInternalState: cardinal; virtual; abstract;
    /// check if the data may have changed of the server for this objects, and
    // update it if possible
    // - only working types are TSQLTableJSON and TSQLRecord descendants
    // - make use of the InternalState function to check the data content revision
    // - return true if Data is updated successfully, or false on any error
    // during data retrieval from server (e.g. if the TSQLRecord has been deleted)
    // - if Data contains only one TSQLTableJSON, PCurrentRow can point to the
    // current selected row of this table, in order to refresh its value
    // - use this method to refresh the client UI, e.g. via a timer
    // - is defined here and not in IRestORMClient since it is very specific
    function UpdateFromServer(const Data: array of TObject;
      out Refreshed: boolean; PCurrentRow: PInteger = nil): boolean; virtual; abstract;
    /// send a flush command to the remote Server cache
    // - this method will remotely call the Cache.Flush() methods of the server
    // instance, to force cohesion of the data
    // - ServerCacheFlush() with no parameter will flush all stored JSON content
    // - ServerCacheFlush(aTable) will flush the cache for a given table
    // - ServerCacheFlush(aTable,aID) will flush the cache for a given record
    function ServerCacheFlush(aTable: TSQLRecordClass = nil;
      aID: TID = 0): boolean; virtual; abstract;
    /// retrieve a list of members as a TSQLTable
    // - implements REST GET collection
    // - default SQL statement is 'SELECT ID FROM TableName;' (i.e. retrieve
    // the list of all ID of this collection members)
    // - optional SQLSelect parameter to change the returned fields
    // as in 'SELECT SQLSelect FROM TableName;'
    // - optional SQLWhere parameter to change the search range or ORDER
    // as in 'SELECT SQLSelect FROM TableName WHERE SQLWhere;'
    // - using inlined parameters via :(...): in SQLWhere is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    function List(const Tables: array of TSQLRecordClass;
      const SQLSelect: RawUTF8 = 'RowID';
      const SQLWhere: RawUTF8 = ''): TSQLTable; virtual; abstract;
    /// retrieve a list of members as a TSQLTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUTF8() function, replacing all '%' chars with Args[] values
    // - using inlined parameters via :(...): in SQLWhereFormat is always a good idea
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TSQLRecordClass;
      const SQLSelect, SQLWhereFormat: RawUTF8;
      const Args: array of const): TSQLTable; overload;
    /// retrieve a list of members as a TSQLTable
    // - implements REST GET collection
    // - in this version, the WHERE clause can be created with the same format
    // as FormatUTF8() function, replacing all '%' chars with Args[], and all '?'
    // chars with Bounds[] (inlining them with :(...): and auto-quoting strings)
    // - example of use:
    // ! Table := ListFmt([TSQLRecord],'Name','ID=?',[],[aID]);
    // - for one TClass, you should better use TRest.MultiFieldValues()
    // - will call the List virtual method internaly
    function ListFmt(const Tables: array of TSQLRecordClass;
      const SQLSelect, SQLWhereFormat: RawUTF8;
      const Args, Bounds: array of const): TSQLTable; overload;
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
    // !if Client.TransactionBeginRetry(TSQLRecordPeopleObject,20) then
    // !try
    // !  // .... modify the database content, raise exceptions on error
    // !  Client.Commit;
    // !except
    // !  Client.RollBack; //  in case of error
    // !end;
    function TransactionBeginRetry(aTable: TSQLRecordClass;
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
    property ForceBlobTransfertTable[aTable: TSQLRecordClass]: boolean
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



{ ************ TRestORMClientURI REST Client from URI }

type
  /// main entry point of TRestORMClientURI, redirecting to TRestClientURI.URI()
  TOnRestORMClientURI = function(const url, method: RawUTF8; Resp: PRawUTF8 = nil;
    Head: PRawUTF8 = nil; SendData: PRawUTF8 = nil): Int64Rec of object;

  /// URI-oriented REpresentational State Transfer (REST) client
  // - will later on be implemented over local, Windows messages, named pipe,
  // HTTP/1.1 or WebSockets
  // - works in conjunction with TRestClientURI from mormot.rest.client.pas
  TRestORMClientURI = class(TRestORMClient)
  protected
    // ForUpdate=true->LOCK ForUpdate=false->GET
    function URIGet(Table: TSQLRecordClass; ID: TID; var Resp: RawUTF8;
      ForUpdate: boolean=false): Int64Rec;
  public
    /// will redirect any client call to TRestClientURI.URI()
    // - is injected by TRestClientURI.SetORMInstance
    URI: TOnRestORMClientURI;
    // overridden methods actually calling URI()
    function ClientRetrieve(TableModelIndex: integer; ID: TID;
      ForUpdate: boolean; var InternalState: cardinal;
      var Resp: RawUTF8): boolean; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUTF8; override;
    function EngineExecute(const SQL: RawUTF8): boolean; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUTF8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer;
      const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue,
      WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineBatchSend(Table: TSQLRecordClass; var Data: RawUTF8;
       var Results: TIDDynArray; ExpectedResultsCount: integer): integer; override;
    function ExecuteList(const Tables: array of TSQLRecordClass;
      const SQL: RawUTF8): TSQLTable; override;
    function UnLock(Table: TSQLRecordClass; aID: TID): boolean; override;
    function TransactionBegin(aTable: TSQLRecordClass;
      SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED): boolean; override;
    procedure Commit(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED;
      RaiseException: boolean = false); override;
    procedure RollBack(SessionID: cardinal = CONST_AUTHENTICATION_NOT_USED); override;
    function ServerInternalState: cardinal; override;
    function UpdateFromServer(const Data: array of TObject; out Refreshed: boolean;
      PCurrentRow: PInteger = nil): boolean; override;
    function ServerCacheFlush(aTable: TSQLRecordClass = nil;
      aID: TID = 0): boolean; override;
  end;


implementation


{ ************ TRestORMClient Abstract Client }

{ TRestORMClient }

function TRestORMClient.GetForceBlobTransfert: boolean;
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

procedure TRestORMClient.SetForceBlobTransfert(Value: boolean);
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

function TRestORMClient.GetForceBlobTransfertTable(aTable: TSQLRecordClass): boolean;
begin
  if fForceBlobTransfert = nil then
    result := false
  else
    result := fForceBlobTransfert[fModel.GetTableIndexExisting(aTable)];
end;

procedure TRestORMClient.SetForceBlobTransfertTable(aTable: TSQLRecordClass;
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

function TRestORMClient.InternalAdd(Value: TSQLRecord; SendData: boolean;
  CustomFields: PSQLFieldBits; ForceID, DoNotAutoComputeFields: boolean): TID;
begin
  result := inherited InternalAdd(Value, SendData, CustomFields, ForceID,
    DoNotAutoComputeFields);
  if (result > 0) and
     (fForceBlobTransfert <> nil) and
     fForceBlobTransfert[fModel.GetTableIndexExisting(PSQLRecordClass(Value)^)] then
    UpdateBlobFields(Value);
end;

destructor TRestORMClient.Destroy;
begin
  FreeAndNil(fBatchCurrent);
  inherited Destroy; // fCache.Free
end;

function TRestORMClient.BatchStart(aTable: TSQLRecordClass;
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

function TRestORMClient.BatchStartAny(AutomaticTransactionPerRow: cardinal;
  Options: TRestBatchOptions): boolean;
begin
  result := BatchStart(nil, AutomaticTransactionPerRow, Options);
end;

function TRestORMClient.BatchAdd(Value: TSQLRecord; SendData: boolean;
  ForceID: boolean; const CustomFields: TSQLFieldBits): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Add(Value, SendData, ForceID, CustomFields);
end;

function TRestORMClient.BatchUpdate(Value: TSQLRecord;
  const CustomFields: TSQLFieldBits; DoNotAutoComputeFields: boolean): integer;
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

function TRestORMClient.BatchDelete(ID: TID): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Delete(ID);
end;

function TRestORMClient.BatchDelete(Table: TSQLRecordClass; ID: TID): integer;
begin
  if self = nil then
    result := -1
  else
    result := fBatchCurrent.Delete(Table, ID);
end;

function TRestORMClient.BatchCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := fBatchCurrent.Count;
end;

function TRestORMClient.BatchSend(var Results: TIDDynArray): integer;
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

procedure TRestORMClient.BatchAbort;
begin
  if self <> nil then
    FreeAndNil(fBatchCurrent);
end;

function TRestORMClient.EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8;
var
  dummy: cardinal;
begin
  if not ClientRetrieve(TableModelIndex, ID, false, dummy, result) then
    result := '';
end;

function TRestORMClient.Retrieve(aID: TID; Value: TSQLRecord; ForUpdate: boolean): boolean;
var
  Resp: RawUTF8;
  TableIndex: integer;
  state: cardinal;
begin
  result := false;
  if (self = nil) or
     (aID <= 0) or
     (Value = nil) then
    exit;
  TableIndex := fModel.GetTableIndexExisting(PSQLRecordClass(Value)^);
  if ForUpdate then
  begin
    if not fModel.Lock(TableIndex, aID) then
      exit; // error marking as locked by the client
  end
  else
  begin
    Resp := fCache.Retrieve(TableIndex, aID);
    if Resp <> '' then
    begin
      Value.FillFrom(Resp);
      Value.IDValue := aID; // JSON object may not contain the ID
      result := true;
      exit; // fast retrieved from internal Client cache (BLOBs ignored)
    end;
  end;
  try
    state := Value.InternalState;
    if ClientRetrieve(TableIndex, aID, ForUpdate, state, Resp) then
    begin
      Value.InternalState := state;
      if not ForUpdate then
        fCache.Notify(TableIndex, aID, Resp, soSelect);
      Value.FillFrom(Resp);
      Value.IDValue := aID; // JSON object may not contain the ID
      if (fForceBlobTransfert <> nil) and
         fForceBlobTransfert[TableIndex] then
        result := RetrieveBlobFields(Value)
      else
        result := true;
      ForUpdate := false; // any exception shall unlock the record
    end;
  finally
    if ForUpdate then
      fModel.UnLock(TableIndex, aID);
  end;
end;

function TRestORMClient.Update(Value: TSQLRecord;
  const CustomFields: TSQLFieldBits; DoNotAutoComputeFields: boolean): boolean;
begin
  result := BeforeUpdateEvent(Value) and
    inherited Update(Value, CustomFields, DoNotAutoComputeFields);
  if result then
  begin
    if (fForceBlobTransfert <> nil) and
       IsZero(CustomFields) and
       fForceBlobTransfert[fModel.GetTableIndexExisting(PSQLRecordClass(Value)^)] then
      result := UpdateBlobFields(Value);
    if result and assigned(OnRecordUpdate) then
      OnRecordUpdate(Value);
  end;
end;

function TRestORMClient.BeforeUpdateEvent(Value: TSQLRecord): boolean;
begin
  Result := true; // by default, just allow the update to proceed
end;

function TRestORMClient.Refresh(aID: TID; Value: TSQLRecord;
  var Refreshed: boolean): boolean;
var
  Resp, Original: RawUTF8;
  state: cardinal;
begin
  result := false;
  if (aID > 0) and
     (self <> nil) and
     (Value <> nil) then
  begin
    state := Value.InternalState;
    if ClientRetrieve(fModel.GetTableIndexExisting(PSQLRecordClass(Value)^),
        aID, False, state, Resp) then
    begin
      Value.InternalState := state;
      Original := Value.GetJSONValues(IsNotAjaxJSON(pointer(Resp)), true, soSelect);
      Resp := trim(Resp);
      if (Resp <> '') and
         (Resp[1] = '[') then // '[{....}]' -> '{...}'
        Resp := copy(Resp, 2, length(Resp) - 2);
      if Original <> Resp then
      begin
        // did the content really change?
        Refreshed := true;
        Value.FillFrom(Resp);
      end;
      result := true;
    end;
  end;
end;

function TRestORMClient.ListFmt(const Tables: array of TSQLRecordClass;
  const SQLSelect, SQLWhereFormat: RawUTF8; const Args: array of const): TSQLTable;
begin
  result := List(Tables, SQLSelect, FormatUTF8(SQLWhereFormat, Args));
end;

function TRestORMClient.ListFmt(const Tables: array of TSQLRecordClass;
  const SQLSelect, SQLWhereFormat: RawUTF8; const Args, Bounds: array of const): TSQLTable;
begin
  result := List(Tables, SQLSelect, FormatUTF8(SQLWhereFormat, Args, Bounds));
end;

function TRestORMClient.TransactionBeginRetry(aTable: TSQLRecordClass;
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



{ ************ TRestORMClientURI REST Client from URI }

{ TRestORMClientURI }

procedure TRestORMClientURI.Commit(SessionID: cardinal; RaiseException: boolean);
begin
  inherited Commit(CONST_AUTHENTICATION_NOT_USED, RaiseException);
  // inherited Commit = reset fTransactionActiveSession flag
  // END on 'root' URI
  URI(fModel.Root, 'END');
end;

function TRestORMClientURI.TransactionBegin(aTable: TSQLRecordClass;
  SessionID: cardinal): boolean;
begin
  result := inherited TransactionBegin(aTable, CONST_AUTHENTICATION_NOT_USED);
  if result then
    // fTransactionActiveSession flag was not already set
    if aTable = nil then
      // BEGIN on 'root' URI
      result := URI(fModel.Root, 'BEGIN').Lo in [HTTP_SUCCESS, HTTP_NOCONTENT]
    else
      // BEGIN on 'root/table' URI
      result := URI(fModel.URI[aTable], 'BEGIN').Lo in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

procedure TRestORMClientURI.RollBack(SessionID: cardinal);
begin
  inherited RollBack(CONST_AUTHENTICATION_NOT_USED);
  // inherited RollBack = reset fTransactionActiveSession flag
  // ABORT on 'root' URI
  URI(fModel.Root, 'ABORT');
end;

function TRestORMClientURI.UnLock(Table: TSQLRecordClass; aID: TID): boolean;
begin
  if (self = nil) or
     not fModel.UnLock(Table, aID) then
    // was not locked by the client
    result := false
  else
    // UNLOCK on 'root/table/ID' URI
    result := URI(fModel.GetURIID(Table, aID), 'UNLOCK').Lo in
      [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestORMClientURI.URIGet(Table: TSQLRecordClass; ID: TID;
  var Resp: RawUTF8; ForUpdate: boolean): Int64Rec;
const
  METHOD: array[boolean] of RawUTF8 = (
    'GET', 'LOCK');
begin
  // GET/LOCK on 'root/table/ID' URI
  result := URI(fModel.GetURIID(Table, ID), METHOD[ForUpdate], @Resp, nil, nil);
end;

function TRestORMClientURI.ClientRetrieve(TableModelIndex: integer; ID: TID;
  ForUpdate: boolean; var InternalState: cardinal; var Resp: RawUTF8): boolean;
begin
  if cardinal(TableModelIndex) <= cardinal(fModel.TablesMax) then
    with URIGet(fModel.Tables[TableModelIndex], ID, Resp, ForUpdate) do
      if Lo = HTTP_SUCCESS then
      begin
        InternalState := Hi;
        result := true;
      end
      else
        result := false
  else
    result := false;
end;

function TRestORMClientURI.EngineList(const SQL: RawUTF8; ForceAJAX: boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
begin
  if (self = nil) or
     (SQL = '') or
     (ReturnedRowCount <> nil) or
     // GET on 'root' URI with SQL as body (not fully standard HTTP)
     (URI(fModel.Root, 'GET', @result, nil, @SQL).Lo <> HTTP_SUCCESS) then
    result := '';
end;

function TRestORMClientURI.EngineExecute(const SQL: RawUTF8): boolean;
begin
  // POST on 'root' URI with SQL as body
  result := URI(fModel.Root, 'POST', nil, nil, @SQL).Lo in
    [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestORMClientURI.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
var
  P: PUTF8Char;
  url, Head: RawUTF8;
begin
  result := 0;
  url := fModel.URI[fModel.Tables[TableModelIndex]];
  // POST on 'root/table' URI with JSON object as body
  if URI(url, 'POST', nil, @Head, @SentData).Lo <> HTTP_CREATED then
    // response must be '201 Created'
    exit;
  P := pointer(Head); // we need to check the headers
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

function TRestORMClientURI.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
var
  url: RawUTF8;
begin
  // PUT on 'root/table/ID' URI
  url := fModel.GetURIID(fModel.Tables[TableModelIndex], ID);
  result := URI(url, 'PUT', nil, nil, @SentData).Lo in
    [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestORMClientURI.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
var
  url: RawUTF8;
begin
  // DELETE on 'root/table/ID' URI
  url := fModel.GetURIID(fModel.Tables[TableModelIndex], ID);
  result := URI(url, 'DELETE').Lo in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestORMClientURI.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
var
  url: RawUTF8;
begin
  // DELETE on 'root/table?where=WhereClause" URI
  url := fModel.GetURI(fModel.Tables[TableModelIndex]) +
    '?where=' + UrlEncode(SQLWhere);
  result := URI(url, 'DELETE').Lo in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestORMClientURI.EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
  BlobField: PRttiProp; out BlobData: TSQLRawBlob): boolean;
var
  url: RawUTF8;
begin
  if (self = nil) or
     (aID <= 0) or
     (BlobField = nil) then
    result := false
  else
  begin
    // GET on 'root/table/ID/BlobFieldName' URI
    url := fModel.GetURICallBack(BlobField^.NameUTF8,
      fModel.Tables[TableModelIndex], aID);
    result := URI(url, 'GET', @BlobData).Lo = HTTP_SUCCESS;
  end;
end;

function TRestORMClientURI.EngineUpdateBlob(TableModelIndex: integer; aID: TID;
  BlobField: PRttiProp; const BlobData: TSQLRawBlob): boolean;
var
  url, Head: RawUTF8;
begin
  Head := 'Content-Type: application/octet-stream';
  if (self = nil) or
     (aID <= 0) or
     (BlobField = nil) then
    result := false
  else
  begin
    // PUT on 'root/table/ID/BlobFieldName' URI
    url := fModel.GetURICallBack(BlobField^.NameUTF8,
      fModel.Tables[TableModelIndex], aID);
    result := URI(url, 'PUT', nil, @Head, @BlobData).Lo in
      [HTTP_SUCCESS, HTTP_NOCONTENT];
  end;
end;

function TRestORMClientURI.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var
  url: RawUTF8;
begin
  if TableModelIndex < 0 then
    result := false
  else
  begin
    // PUT on 'root/table?setname=..&set=..&wherename=..&where=..' URI
    FormatUTF8('%?setname=%&set=%&wherename=%&where=%',
      [fModel.URI[fModel.Tables[TableModelIndex]], SetFieldName,
       UrlEncode(SetValue), WhereFieldName, UrlEncode(WhereValue)], url);
    result := URI(url, 'PUT').Lo in [HTTP_SUCCESS, HTTP_NOCONTENT];
  end;
end;

function TRestORMClientURI.EngineBatchSend(Table: TSQLRecordClass;
  var Data: RawUTF8; var Results: TIDDynArray; ExpectedResultsCount: integer): integer;
var
  Resp: RawUTF8;
  R: PUTF8Char;
  i: PtrInt;
begin
  // TRest.BatchSend() ensured that Batch contains some data
  try
    // PUT on 'root/Batch' or 'root/Batch/Table' URI
    result := URI(fModel.GetURICallBack('Batch', Table, 0),
      'PUT', @Resp, nil, @Data).Lo;
    if result <> HTTP_SUCCESS then
      exit;
    // returned Resp shall be an array of integers: '[200,200,...]'
    R := pointer(Resp);
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
        Results[i] := GetNextItemQWord(R, #0);
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

function TRestORMClientURI.ExecuteList(const Tables: array of TSQLRecordClass;
  const SQL: RawUTF8): TSQLTable;
var
  resp: RawUTF8;
  res: Int64Rec;
begin
  if self = nil then
    result := nil
  else
  begin
    // GET on 'root' URI with SQL as body (not fully HTTP compatible)
    res := URI(fModel.Root, 'GET', @resp, nil, @SQL);
    if res.Lo = HTTP_SUCCESS then
    begin
      result := TSQLTableJSON.CreateFromTables(
        Tables, SQL, pointer(resp), length(resp));
      result.InternalState := res.Hi;
    end
    else
      // no data on error
      result := nil;
  end;
end;

function TRestORMClientURI.ServerInternalState: cardinal;
begin
  if (self = nil) or
     (fModel = nil) then // avoid GPF
    result := cardinal(-1)
  else
    result := URI(fModel.Root, 'STATE').Hi;
end;

function TRestORMClientURI.UpdateFromServer(const Data: array of TObject;
  out Refreshed: boolean; PCurrentRow: PInteger): boolean;
// notes about refresh mechanism:
// - if server doesn't implement InternalState, its value is 0 -> always refresh
// - if any TSQLTableJSON or TSQLRecord belongs to a TRestStorage,
// the Server stated fInternalState=cardinal(-1) for them -> always refresh
var
  i: PtrInt;
  State: cardinal;
  Resp: RawUTF8;
  T: TSQLTableJSON;
  TRefreshed: boolean; // to check for each Table refresh
const
  _ST: array[boolean] of TOnTableUpdateState = (
    tusNoChange, tusChanged);
begin
  result := self <> nil;
  Refreshed := false;
  if not result then
    exit; // avoid GPF
  State := ServerInternalState; // get revision state from server
  for i := 0 to high(Data) do
    if Data[i] <> nil then
      if Data[i].InheritsFrom(TSQLTableJSON) then
      begin
        T := TSQLTableJSON(Data[i]);
        if (T.QuerySQL <> '') and
           (T.InternalState <> State) then
        begin
          // refresh needed
          with URI(fModel.Root, 'GET', @Resp, nil, @T.QuerySQL) do
            if Lo = HTTP_SUCCESS then
            begin
              // refresh after proper GET with SQL sent
              if Assigned(OnTableUpdate) then
                OnTableUpdate(T, tusPrepare);
              TRefreshed := false;
              if not T.UpdateFrom(Resp, TRefreshed, PCurrentRow) then
                // mark error retrieving new content
                result := false
              else
                // successfully refreshed with new data
                T.InternalState := Hi;
              if TRefreshed then
                Refreshed := true;
              if Assigned(OnTableUpdate) then
                OnTableUpdate(T, _ST[TRefreshed]);
            end
            else
              // mark error retrieving new content
              result := false;
        end;
      end
      else if Data[i].InheritsFrom(TSQLRecord) then
        with TSQLRecord(Data[i]) do
          if (IDValue <> 0) and
             (InternalState <> State) then
          begin
            // refresh needed
            if not Refresh(IDValue, TSQLRecord(Data[i]), Refreshed) then
              // mark error retrieving new content
              result := false;
          end;
end;

function TRestORMClientURI.ServerCacheFlush(aTable: TSQLRecordClass; aID: TID): boolean;
begin
  if (self = nil) or
     (Model = nil) then // avoid GPF
    result := false
  else
    result := URI(fModel.GetURICallBack('CacheFlush', aTable, aID), 'GET').Lo
      in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;


end.

