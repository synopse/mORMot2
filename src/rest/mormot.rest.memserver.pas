/// REpresentation State Tranfer (REST) In-Memory Server
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.memserver;

{
  *****************************************************************************

   Standalone REST In-Memory Server Using JSON or Binary Persistence
    - TRestOrmServerFullMemory Standalone REST ORM Engine
    - TRestServerFullMemory Standalone REST Server

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
  mormot.core.crypto,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.soa.core,
  mormot.soa.server,
  mormot.rest.core,
  mormot.rest.server;


{ ************ TRestOrmServerFullMemory Standalone REST ORM Engine }


type
  /// implements TRestServerFullMemory.ORM process for REST server
  // - this server will use TRestStorageInMemory instances to handle
  // the data in memory, and optionally persist the data on disk as JSON or
  // binary files
  // - so it will not handle all SQL requests, just basic CRUD commands on
  // separated tables
  // - at least, it will compile as a TRestOrmServer without complaining for
  // pure abstract methods; it can be used to host some services if database
  // and ORM needs are basic (e.g. if only authentication and CRUD are needed),
  // without the need to link the SQLite3 engine
  TRestOrmServerFullMemory = class(TRestOrmServer)
  protected
    fFileName: TFileName;
    fBinaryFile: boolean;
    fStaticDataCount: integer;
  public
    function GetStorage(aTable: TOrmClass): TRestStorageInMemory;
    /// overridden methods which will call fStaticData[TableModelIndex] directly
    // without the TRestOrmServer overhead
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUTF8): boolean; override;
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    /// overridden methods which will return error (no main DB in our context)
    function MainEngineAdd(TableModelIndex: integer; const SentData: RawUTF8): TID; override;
    function MainEngineRetrieve(TableModelIndex: integer; ID: TID): RawUTF8; override;
    function MainEngineList(const SQL: RawUTF8; ForceAJAX: boolean;
      ReturnedRowCount: PPtrInt): RawUTF8; override;
    function MainEngineUpdate(TableModelIndex: integer; aID: TID;
      const SentData: RawUTF8): boolean; override;
    function MainEngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    function MainEngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIDDynArray): boolean; override;
    function MainEngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function MainEngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    function MainEngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    function MainEngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUTF8; Increment: Int64): boolean; override;
    // method not implemented: always return false
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
  public
    /// initialize an in-memory REST server with no database file
    constructor Create(aRest: TRest); overload; override;
    /// initialize an in-memory REST server with a database file
    // - all classes of the model will be created as TRestStorageInMemory
    // - then data persistence will be initialized using aFileName, but no
    // file will be written to disk, unless you call explicitly UpdateToFile
    // - if aFileName is left void (''), data will not be persisted on disk
    constructor Create(aRest: TRest; const aFileName: TFileName;
      aBinaryFile: boolean = false); reintroduce; overload; virtual;
    /// finalize the REST server
    // - this overridden destructor will write any modification on file (if
    // needed), and release all used memory
    destructor Destroy; override;
    /// Missing tables are created if they don't exist yet for every TOrm
    // class of the Database Model
    // - you must call explicitely this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method also create additional fields, if the TOrm definition
    // has been modified; only field adding is available, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TOrm type)
    procedure CreateMissingTables(user_version: cardinal = 0;
      Options: TOrmInitializeTableOptions = []); override;
    /// load the content from the specified file name
    // - do nothing if file name was not assigned
    procedure LoadFromFile; virtual;
    /// load the content from the supplied resource
    procedure LoadFromStream(aStream: TStream); virtual;
    /// write any modification into file
    // - do nothing if file name was not assigned
    procedure UpdateToFile; virtual;
    /// clear all internal storage content
    procedure DropDatabase; virtual;
  published
    /// the file name used for data persistence
    property FileName: TFileName
      read fFileName write fFileName;
    /// set if the file content is to be compressed binary, or standard JSON
    // - it will use TRestStorageInMemory LoadFromJSON/LoadFromBinary
    // SaveToJSON/SaveToBinary methods for optimized storage
    property BinaryFile: boolean
      read fBinaryFile write fBinaryFile;
  end;



{ ************ TRestServerFullMemory Standalone REST Server }

type
  /// a REST server using only in-memory tables
  // - this server will use TRestStorageInMemory instances to handle
  // the data in memory, and optionally persist the data on disk as JSON or
  // binary files
  // - so it will not handle all SQL requests, just basic CRUD commands and
  // most simple SELECT with a single where clause, on a single table
  // - at least, it will compile as a TRestServer without complaining for
  // pure abstract methods; it can be used to host some services if database
  // and ORM needs are basic (e.g. if only authentication and CRUD are needed),
  // without the need to link the SQLite3 engine
  TRestServerFullMemory = class(TRestServer)
  protected
    function GetBinaryFile: boolean;
    function GetFileName: TFileName;
  public
    /// initialize an in-memory REST server with no database file
    constructor Create(aModel: TOrmModel;
      aHandleUserAuthentication: boolean = false); overload; override;
    /// initialize an in-memory REST server with a database file
    // - all classes of the model will be created as TRestStorageInMemory
    // - then data persistence will be initialized using aFileName, but no
    // file will be written to disk, unless you call explicitly UpdateToFile
    // - if aFileName is left void (''), data will not be persistent
    constructor Create(aModel: TOrmModel;
      const aFileName: TFileName; aBinaryFile: boolean = false;
      aHandleUserAuthentication: boolean = false); reintroduce; overload; virtual;
  published
    /// the file name used for data persistence
    property FileName: TFileName
      read GetFileName;
    /// set if the file content is to be compressed binary, or standard JSON
    // - it will use TRestStorageInMemory LoadFromJSON/LoadFromBinary
    // SaveToJSON/SaveToBinary methods for optimized storage
    property BinaryFile: boolean
      read GetBinaryFile;
  published
    /// this method-base service will be accessible from ModelRoot/Flush URI,
    // and will write any modification into file
    // - method parameters signature matches TOnRestServerCallBack type
    // - do nothing if file name was not assigned
    // - can be used from a remote client to ensure that any Add/Update/Delete
    // will be stored to disk, via
    // ! aClient.CallBackPut('Flush','',dummy)
    procedure Flush(Ctxt: TRestServerURIContext);
  end;


/// create an external static in-memory database for a specific class
// - call it after TRestServer.Create, before IRestOrmServer.CreateMissingTables;
// warning: if you don't call this method before CreateMissingTable method
// is called, the table will be created as a regular table by the main
// database engine, and won't be static
// - can load the table content from a file if a file name is specified
// (could be either JSON or compressed Binary format on disk)
// - you can define a particular external engine by using a custom class -
// by default, it will create a TRestStorageInMemory instance
// - this data handles basic REST commands, since no complete SQL interpreter
// can be implemented by TRestStorage; to provide full SQL process,
// you should better use a Virtual Table class, inheriting e.g. from
// TOrmVirtualTableAutoID associated with TOrmVirtualTableJSON/Binary
// via a Model.VirtualTableRegister() call before TRestServer.Create
// - you can use this method to change the filename of an existing storage
// - return nil on any error, or an EModelException if the class is not in
// the aServer database model
function StaticDataCreate(aServer: TRestOrmServer; aClass: TOrmClass;
  const aFileName: TFileName = ''; aBinaryFile: boolean = false;
  aStorageClass: TRestStorageInMemoryClass = nil): TRestStorageInMemory;


/// create a new minimal TRestServer instance, to be used with
// external SQL or NoSQL storage
// - will try to instantiate an in-memory (':memory:') TRestServerDB, and if
// mormot.orm.sqlite3.pas is not linked, fallback to a TRestServerFullMemory
// - used e.g. by TRestMongoDBCreate() and TRestExternalDBCreate()
function CreateInMemoryServerForAllVirtualTables(aModel: TOrmModel;
  aHandleUserAuthentication: boolean): TRestServer;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  // should be a proper type for RegisterClassNameForDefinition
  TSQLRestServerFullMemory = TRestServerFullMemory;

{$endif PUREMORMOT2}


implementation


{ ************ TRestOrmServerFullMemory Standalone REST ORM Engine }

{ TRestOrmServerFullMemory }

constructor TRestOrmServerFullMemory.Create(aRest: TRest);
var
  t: PtrInt;
begin
  inherited Create(aRest); // calls fRest.SetOrmInstance(self)
  // pre-allocate all TRestStorageInMemory instances
  fStaticDataCount := length(fModel.Tables);
  for t := 0 to fStaticDataCount - 1 do
    StaticDataCreate(self, fModel.Tables[t]);
end;

constructor TRestOrmServerFullMemory.Create(aRest: TRest;
  const aFileName: TFileName; aBinaryFile: boolean);
begin
  fFileName := aFileName;
  fBinaryFile := aBinaryFile;
  Create(aRest);
  LoadFromFile;
  CreateMissingTables(0, []);
end;

procedure TRestOrmServerFullMemory.CreateMissingTables(user_version: cardinal;
  Options: TOrmInitializeTableOptions);
var
  t: PtrInt;
begin
  inherited;
  // create any missing static instances - e.g. for just added user/group
  if fStaticDataCount <> length(fModel.Tables) then
  begin
    for t := fStaticDataCount to high(fModel.Tables) do
      StaticDataCreate(self, fModel.Tables[t]);
    fStaticDataCount := length(fModel.Tables);
  end;
  // initialize new tables
  for t := 0 to fStaticDataCount - 1 do
    with TRestStorageInMemory(fStaticData[t]) do
      if Count = 0 then
        // emulates TSQLRestServerDB.CreateMissingTables
        StoredClass.InitializeTable(self, '', Options);
end;

destructor TRestOrmServerFullMemory.Destroy;
begin
  UpdateToFile;
  inherited;
end;

procedure TRestOrmServerFullMemory.DropDatabase;
var
  t: PtrInt;
begin
  for t := 0 to fStaticDataCount - 1 do
    TRestStorageInMemory(fStaticData[t]).DropValues;
end;

procedure TRestOrmServerFullMemory.LoadFromStream(aStream: TStream);
var
  magic, JSON: RawUTF8;
  P, TableName, Data: PUTF8Char;
  t: PtrInt;
  wasString: boolean;
begin
  if aStream = nil then
    exit;
  if fBinaryFile then
  begin
    // optimized binary content with SynLZ + variable-length record storage
    magic := ReadStringFromStream(aStream);
    if PosEx('ServerFullMemory00', magic) > 0 then
      // compatible with mORMot 1.18 'TSQLRestServerFullMemory00'
      repeat
        t := Model.GetTableIndex(ReadStringFromStream(aStream));
        if t < 0 then
          break;
      until not TRestStorageInMemory(fStaticData[t]).LoadFromBinary(aStream);
  end
  else
  begin
    // [{"AuthUser":[{....},{...}]},{"AuthGroup":[{...},{...}]}]
    JSON := StreamToRawByteString(aStream); // assume UTF-8 content
    if JSON = '' then
      exit;
    P := pointer(JSON);
    while P^ <> '[' do
      if P^ = #0 then
        exit
      else
        inc(P);
    inc(P);
    repeat
      while (P^ <> ']') and
            (P^ <> '{') do
        if P^ = #0 then
          exit
        else
          inc(P);
      if P^ = ']' then
        break;
      inc(P);
      TableName := GetJSONField(P, P, @wasString);
      if not wasString or
         (P = nil) then
        exit;
      t := Model.GetTableIndexPtr(TableName);
      if t < 0 then
        exit;
      Data := P;
      P := GotoNextJSONObjectOrArray(P);
      if P = nil then
        break;
      TRestStorageInMemory(fStaticData[t]).LoadFromJSON(Data, P - Data);
    until false;
  end;
end;

procedure TRestOrmServerFullMemory.LoadFromFile;
var
  S: THandleStream;
begin
  if (fFileName = '') or
    not FileExists(fFileName) then
    exit;
  DropDatabase;
  S := FileStreamSequentialRead(FileName);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TRestOrmServerFullMemory.UpdateToFile;
const
  CHARS: array[0..6] of AnsiChar = '[{":,}]';
                                 // 0123456
var
  S: TFileStream;
  t: PtrInt;
  Modified: boolean;
  timer: TPrecisionTimer;
begin
  if (self = nil) or
     (FileName = '') then
    exit;
  Modified := false;
  for t := 0 to fStaticDataCount - 1 do
    if TRestStorageInMemory(fStaticData[t]).Modified then
    begin
      Modified := true;
      break;
    end;
  if not Modified then
    exit;
  timer.Start;
  S := TFileStream.Create(fFileName, fmCreate);
  try
    if fBinaryFile then
    begin
      // optimized binary content with SynLZ + variable-length record storage
      WriteStringToStream(S, ToText(ClassType) + '00');
      for t := 0 to fStaticDataCount - 1 do
        with TRestStorageInMemory(fStaticData[t]) do
        begin
          // each TOrmClass is stored as SynLZ-compressed binary
          WriteStringToStream(S, StoredClassRecordProps.SQLTableName);
          SaveToBinary(S);
        end;
    end
    else
    begin
      // [{"AuthUser":[{....},{...}]},{"AuthGroup":[{...},{...}]}]
      S.WriteBuffer(CHARS[0], 1);
      for t := 0 to fStaticDataCount - 1 do
        with TRestStorageInMemory(fStaticData[t]) do
        begin
          S.WriteBuffer(CHARS[1], 2);
          with StoredClassRecordProps do
            S.WriteBuffer(pointer(SQLTableName)^, length(SQLTableName));
          S.WriteBuffer(CHARS[2], 2);
          SaveToJSON(S, true);
          S.WriteBuffer(CHARS[5], 1);
          if t < integer(fStaticDataCount - 1) then
            S.WriteBuffer(CHARS[4], 1);
        end;
      S.WriteBuffer(CHARS[6], 1);
    end;
  finally
    S.Free;
  end;
  InternalLog('UpdateToFile % done in %', [fFileName, timer.Stop], sllDB);
end;

function TRestOrmServerFullMemory.EngineExecute(const aSQL: RawUTF8): boolean;
begin
  result := false; // not implemented in this basic REST server class
end;

function TRestOrmServerFullMemory.GetStorage(
  aTable: TOrmClass): TRestStorageInMemory;
var
  i: cardinal;
begin
  i := fModel.GetTableIndex(aTable);
  if i >= cardinal(length(fStaticData)) then
    result := nil
  else
    result := TRestStorageInMemory(fStaticData[i]);
end;

// Engine*() methods will have direct access to static fStorage[])

function TRestOrmServerFullMemory.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
begin
  result := fStaticData[TableModelIndex].EngineAdd(TableModelIndex, SentData);
  inc(InternalState);
end;

function TRestOrmServerFullMemory.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
begin
  result := fStaticData[TableModelIndex].EngineRetrieve(TableModelIndex, ID);
end;

function TRestOrmServerFullMemory.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUTF8): boolean;
begin
  result := fStaticData[TableModelIndex].EngineUpdate(TableModelIndex, ID, SentData);
end;

function TRestOrmServerFullMemory.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  result := fStaticData[TableModelIndex].EngineDelete(TableModelIndex, ID);
end;

function TRestOrmServerFullMemory.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIDDynArray): boolean;
begin
  result := fStaticData[TableModelIndex].EngineDeleteWhere(TableModelIndex,
    SQLWhere, IDs);
end;

function TRestOrmServerFullMemory.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
begin
  result := fStaticData[TableModelIndex].EngineRetrieveBlob(
    TableModelIndex, aID, BlobField, BlobData);
end;

function TRestOrmServerFullMemory.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
begin
  result := fStaticData[TableModelIndex].EngineUpdateBlob(
    TableModelIndex, aID, BlobField, BlobData);
end;

function TRestOrmServerFullMemory.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
begin
  result := fStaticData[TableModelIndex].EngineUpdateField(TableModelIndex,
    SetFieldName, SetValue, WhereFieldName, WhereValue);
end;

function TRestOrmServerFullMemory.EngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUTF8;
  Increment: Int64): boolean;
begin
  result := fStaticData[TableModelIndex].EngineUpdateFieldIncrement(
    TableModelIndex, ID, FieldName, Increment);
end;

// MainEngine*() methods should return error (only access via static fStaticData[])

function TRestOrmServerFullMemory.MainEngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): TID;
begin
  result := 0;
end;

function TRestOrmServerFullMemory.MainEngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUTF8;
begin
  result := '';
end;

function TRestOrmServerFullMemory.MainEngineList(const SQL: RawUTF8;
  ForceAJAX: boolean; ReturnedRowCount: PPtrInt): RawUTF8;
begin
  result := '';
end;

function TRestOrmServerFullMemory.MainEngineUpdate(
  TableModelIndex: integer; aID: TID; const SentData: RawUTF8): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineDelete(
  TableModelIndex: integer; ID: TID): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineDeleteWhere(
  TableModelIndex: integer; const SQLWhere: RawUTF8;
  const IDs: TIDDynArray): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineRetrieveBlob(
  TableModelIndex: integer; aID: TID; BlobField: PRttiProp;
  out BlobData: RawBlob): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineUpdateBlob(
  TableModelIndex: integer; aID: TID; BlobField: PRttiProp;
  const BlobData: RawBlob): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineUpdateField(
  TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
begin
  result := false;
end;

function TRestOrmServerFullMemory.MainEngineUpdateFieldIncrement(
  TableModelIndex: integer; ID: TID; const FieldName: RawUTF8;
  Increment: Int64): boolean;
begin
  result := false;
end;


{ ************ TRestServerFullMemory Standalone REST Server }

function StaticDataCreate(aServer: TRestOrmServer; aClass: TOrmClass;
  const aFileName: TFileName; aBinaryFile: boolean;
  aStorageClass: TRestStorageInMemoryClass): TRestStorageInMemory;
var
  t: PtrInt;
begin
  if aStorageClass = nil then
    // default in-memory engine
    aStorageClass := TRestStorageInMemory;
  t := aServer.Model.GetTableIndexExisting(aClass);
  result := TRestStorageInMemory(aServer.GetStaticTableIndex(t));
  if result <> nil then
    // class already registered -> check aStorageClass, and update file name
    (result as aStorageClass).FileName := aFileName
  else
  begin
    // class not already registered -> create and register now
    result := aStorageClass.Create(aClass, aServer, aFileName, aBinaryFile);
    result.StorageLockShouldIncreaseOwnerInternalState := true;
    aServer.StaticTableSetup(t, result, sStaticDataTable);
  end;
end;

function CreateInMemoryServerForAllVirtualTables(aModel: TOrmModel;
  aHandleUserAuthentication: boolean): TRestServer;
var
  c: TRestClass;
  fake: TSynConnectionDefinition;
begin
  fake := TSynConnectionDefinition.Create;
  try
    // search SQLite3 by name, available if mormot.orm.sqlite3 is linked
    fake.Kind := 'TRestServerDB';
    c := TRest.ClassFrom(fake);
    if (c = nil) or
       not c.InheritsFrom(TRestServer) then
    begin
      fake.Kind := 'TSQLRestServerDB';
      c := TRest.ClassFrom(fake);
    end;
    if (c = nil) or
       not c.InheritsFrom(TRestServer) then
    begin
      // fallback if SQLite3 not linked
      result := TRestServerFullMemory.Create(aModel, aHandleUserAuthentication);
      exit;
    end;
    // we have the SQLite3 engine at hand
    fake.ServerName := ':memory:';
    result := TRestServerClass(c).RegisteredClassCreateFrom(
      aModel, fake, aHandleUserAuthentication);
  finally
    fake.Free;
  end;
end;


{ TRestServerFullMemory }

constructor TRestServerFullMemory.Create(aModel: TOrmModel;
  aHandleUserAuthentication: boolean);
begin
  inherited Create(aModel, aHandleUserAuthentication);
  TRestOrmServerFullMemory.Create(self); // assign the ORM in-memory engine
end;

constructor TRestServerFullMemory.Create(aModel: TOrmModel;
  const aFileName: TFileName; aBinaryFile, aHandleUserAuthentication: boolean);
begin
  inherited Create(aModel, aHandleUserAuthentication);
  TRestOrmServerFullMemory.Create(self, aFileName, aBinaryFile);
end;

function TRestServerFullMemory.GetBinaryFile: boolean;
begin
  if self = nil then
    result := false
  else
    result := (fOrmInstance as TRestOrmServerFullMemory).BinaryFile;
end;

function TRestServerFullMemory.GetFileName: TFileName;
begin
  if self = nil then
    result := ''
  else
    result := (fOrmInstance as TRestOrmServerFullMemory).FileName;
end;

procedure TRestServerFullMemory.Flush(Ctxt: TRestServerURIContext);
begin
  if Ctxt.Method = mPUT then
  begin
    (fOrmInstance as TRestOrmServerFullMemory).UpdateToFile;
    Ctxt.Success;
  end;
end;


initialization
  TRestServerFullMemory.RegisterClassNameForDefinition;
  {$ifndef PUREMORMOT2}
  TSQLRestServerFullMemory.RegisterClassNameForDefinition;
  {$endif PUREMORMOT2}

end.

