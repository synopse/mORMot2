/// ORM/ODM Types and Classes for direct MongoDB NoSQL Database Access
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.orm.mongodb;

{
  *****************************************************************************

   ORM/ODM MongoDB Database Access using mormot.db.nosql.mongodb unit
    - TRestStorageMongoDB for REST Storage Over MongoDB
    - High-Level Functions to Initialize MongoDB ORM

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
  mormot.core.search,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.orm.storage,
  mormot.rest.core,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.soa.server,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.db.nosql.mongodb;


{ *********** TRestStorageMongoDB for REST Storage Over MongoDB }

type
  /// exception class raised by this units
  EOrmMongoDB = class(EOrmException);

  /// how TRestStorageMongoDB would compute the next ID to be inserted
  // - you may choose to retrieve the last inserted ID via
  // $ {$query:{},$orderby:{_id:-1}}
  // or search for the current maximum ID in the collection via
  // $ {$group:{_id:null,max:{$max:"$_id"}}}
  // - eacLastIDOnce and eacMaxIDOnce would execute the request once when
  // the storage instance is first started, whereas eacLastIDEachTime and
  // eacMaxIDEachTime would be execute before each insertion
  // - with big amount of data, retrieving the maximum ID (eacMaxID*) performs
  // a full scan, which would be very slow: the last inserted ID (eacLastID*)
  // would definitively be faster
  // - in all cases, to ensure that a centralized MongoDB server has unique
  // ID, you should better pre-compute the ID using your own algorithm
  // depending on your nodes topology, and not rely on the ORM, e.g. using
  // SetEngineAddComputeIdentifier() method, which would allocate a
  // TSynUniqueIdentifierGenerator and associate eacSynUniqueIdentifier
  TRestStorageMongoDBEngineAddComputeID = (
    eacLastIDOnce,
    eacLastIDEachTime,
    eacMaxIDOnce,
    eacMaxIDEachTime,
    eacSynUniqueIdentifier);

  /// REST server with direct access to a MongoDB external database
  // - handle all REST commands via direct SynMongoDB call
  // - is used by TRestServer.Uri for faster RESTful direct access
  // - JOINed SQL statements are not handled yet
  TRestStorageMongoDB = class(TRestStorage)
  protected
    fCollection: TMongoCollection;
    fEngineLastID: TID;
    fEngineGenerator: TSynUniqueIdentifierGenerator;
    fEngineAddCompute: TRestStorageMongoDBEngineAddComputeID;
    fBsonProjectionSimpleFields: variant;
    fBsonProjectionBlobFields: variant;
    fBsonProjectionBlobFieldsNames: TRawUtf8DynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TUriMethod;
    fBatchWriter: TBsonWriter;
    fBatchIDs: TIDDynArray;
    fBatchIDsCount: integer;
    function EngineNextID: TID;
    function DocFromJson(const Json: RawUtf8; Occasion: TOrmOccasion;
      var Doc: TDocVariantData): TID;
    procedure JsonFromDoc(var doc: TDocVariantData; var result: RawUtf8);
    function BsonProjectionSet(var Projection: variant; WithID: boolean;
      const Fields: TFieldBits; BsonFieldNames: PRawUtf8DynArray;
      const SubFields: TRawUtf8DynArray): integer;
    function GetJsonValues(const Res: TBsonDocument;
      const extFieldNames: TRawUtf8DynArray; W: TJsonSerializer): integer;
  public
    // overridden methods calling the MongoDB external server
    function EngineRetrieve(TableModelIndex: integer; ID: TID): RawUtf8; override;
    function EngineList(const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawUtf8; override;
    function EngineAdd(TableModelIndex: integer;
      const SentData: RawUtf8): TID; override;
    function EngineUpdate(TableModelIndex: integer; ID: TID;
      const SentData: RawUtf8): boolean; override;
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue,
      WhereFieldName, WhereValue: RawUtf8): boolean; override;
    function EngineUpdateFieldIncrement(TableModelIndex: integer; ID: TID;
      const FieldName: RawUtf8; Increment: Int64): boolean; override;
    function EngineDeleteWhere(TableModelIndex: Integer;
      const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean; override;
    // BLOBs should be accessed directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; out BlobData: RawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex: integer; aID: TID;
      BlobField: PRttiProp; const BlobData: RawBlob): boolean; override;
    // method not implemented: always return false
    function EngineExecute(const aSql: RawUtf8): boolean; override;
    /// TRestServer.Uri use it for Static.EngineList to by-pass virtual table
    // - overridden method which allows return TRUE, i.e. always by-pass
    // virtual tables process
    function AdaptSqlForEngineList(var SQL: RawUtf8): boolean; override;
  public
    /// initialize the direct access to the MongoDB collection
    // - in practice, you should not have to call this constructor, but rather
    // StaticMongoDBRegister() with a TMongoDatabase instance
    constructor Create(aClass: TOrmClass; aServer: TRestOrmServer); override;
    /// release used memory
    destructor Destroy; override;

     /// overridden method for one single update call to the MongoDB server
    function UpdateBlobFields(Value: TOrm): boolean; override;
     /// overridden method for one single read call to the MongoDB server
    function RetrieveBlobFields(Value: TOrm): boolean; override;
    /// get the row count of a specified table
    // - return -1 on error
    // - return the row count of the table on success
    function TableRowCount(Table: TOrmClass): Int64; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TOrmClass): boolean; override;
    /// delete a row, calling the current MongoDB server
    // - made public since a TRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TRestServer
    function EngineDelete(TableModelIndex: integer; ID: TID): boolean; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    // - faster than OneFieldValues method, which creates a temporary JSON content
    function SearchField(const FieldName, FieldValue: RawUtf8;
      out ResultID: TIDDynArray): boolean; override;
    /// create one index for all specific FieldNames at once
    function CreateSqlMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean;
      IndexName: RawUtf8 = ''): boolean; override;
    // overridden method returning TRUE for next calls to EngineAdd/Delete
    // will properly handle operations until InternalBatchStop is called
    // BatchOptions is ignored with MongoDB (yet)
    function InternalBatchStart(Method: TUriMethod;
      BatchOptions: TRestBatchOptions): boolean; override;
    // internal method called by TRestServer.RunBatch() to process fast
    // BULK sending to remote MongoDB database
    procedure InternalBatchStop; override;

    /// drop the whole table content
    // - in practice, dropping the whole MongoDB database would be faster
    // - but you can still add items to it - whereas Collection.Drop would
    // trigger GPF issues
    procedure Drop;
    /// initialize an internal time-based unique ID generator, linked to
    // a genuine process identifier
    // - will allocate a local TSynUniqueIdentifierGenerator
    // - EngineAddCompute would be set to eacSynUniqueIdentifier
    procedure SetEngineAddComputeIdentifier(aIdentifier: word);
  published
    /// the associated MongoDB collection instance
    property Collection: TMongoCollection
      read fCollection;
    /// how the next ID would be compute at each insertion
    // - default eacLastIDOnce may be the fastest, but other options are
    // available, and may be used in some special cases
    // - consider using SetEngineAddComputeIdentifier() which is both safe
    // and fast, with a cloud of servers sharing the same MongoDB collection
    property EngineAddCompute: TRestStorageMongoDBEngineAddComputeID
      read fEngineAddCompute write fEngineAddCompute;
  end;



{ *********** High-Level Functions to Initialize MongoDB ORM }

/// creates and register a static class on the Server-side to let a given
// ORM class be stored on a remote MongoDB server
// - will associate the supplied class with a MongoDB collection for a
// specified MongoDB database
// - to be called before IRestOrmServer.CreateMissingTables
// - by default, the collection name will match TOrm.SqlTableName, but
// you can customize it with the corresponding parameter
// - the TOrm.ID (RowID) field is always mapped to MongoDB's _id field
// - will call create needed indexes
// - you can later call aServer.InitializeTables to create any missing index and
// initialize the void tables (e.g. default TSqlAuthGroup and TSqlAuthUser records)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
// (just a regular external DB as defined in mormot.orm.sql.pas unit) - it may be
// a good idea to use short field names on MongoDB side, to reduce the space
// used for storage (since they will be embedded within the document data)
// - it will return the corresponding TRestStorageMongoDB instance -
// you can access later to it and its associated collection e.g. via:
// ! (aServer.StaticDataServer[TMyOrm] as TRestStorageMongoDB)
// - you can set aMapAutoFieldsIntoSmallerLength to compute a field name
// mapping with minimal length, so that the stored BSON would be smaller:
// by definition, ID/RowID will be mapped as 'id', but other fields will
// use their first letter, and another other letter if needed (after a '_',
// or in uppercase, or the next one) e.g. FirstName -> 'f', LastName -> 'l',
// LockedAccount: 'la'...
function StaticMongoDBRegister(aClass: TOrmClass; aServer: TRestOrmServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUtf8 = '';
  aMapAutoFieldsIntoSmallerLength: boolean = false): TRestStorageMongoDB;

type
  /// all possible options for StaticMongoDBRegisterAll/TRestMongoDBCreate functions
  // - by default, TSqlAuthUser and TSqlAuthGroup tables will be handled via the
  // external DB, but you can avoid it for speed when handling session and security
  // by setting mrDoNotRegisterUserGroupTables
  // - you can set mrMapAutoFieldsIntoSmallerLength to compute a field name
  // mapping with minimal length, so that the stored BSON would be smaller:
  // by definition, ID/RowID will be mapped as 'id', but other fields will
  // use their first letter, and another other letter if needed (after a '_',
  // or in uppercase, or the next one) e.g. FirstName -> 'f', LastName -> 'l',
  // LockedAccount: 'la'... - WARNING: not yet implemented
  TStaticMongoDBRegisterOption = (
    mrDoNotRegisterUserGroupTables,
    mrMapAutoFieldsIntoSmallerLength);

  /// set of options for StaticMongoDBRegisterAll/TRestMongoDBCreate functions
  TStaticMongoDBRegisterOptions = set of TStaticMongoDBRegisterOption;

/// create and register ALL classes of a given model to access a MongoDB server
// - the collection names will follow the class names
// - this function will call aServer.InitializeTables to create any missing
// index or populate default collection content
// - if aMongoDBIdentifier is not 0, then SetEngineAddComputeIdentifier()
// would be called
function StaticMongoDBRegisterAll(aServer: TRestOrmServer;
  aMongoDatabase: TMongoDatabase;
  aOptions: TStaticMongoDBRegisterOptions = [];
  aMongoDBIdentifier: word = 0): boolean;

/// create a new TRest instance, possibly using MongoDB for its ORM process
// - if aDefinition.Kind matches a TRest registered class, one new instance
// of this kind will be created and returned
// - if aDefinition.Kind is 'MongoDB' or 'MongoDBS', it will instantiate an
// in-memory TRestServerDB or a TRestServerFullMemory instance (calling
// CreateInMemoryServerForAllVirtualTables), then StaticMongoDBRegisterAll()
// with a TMongoClient initialized from aDefinition.ServerName
// ('server' or 'server:port') - optionally with TLS enabled if Kind equals
// 'MongoDBS' - and a TMongoDatabase created from aDefinition.DatabaseName,
// using authentication if aDefinition.User/Password credentials are set
// - it will return nil if the supplied aDefinition is invalid
// - if aMongoDBIdentifier is not 0, then SetEngineAddComputeIdentifier()
// would be called for all created TRestStorageMongoDB
function TRestMongoDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aOptions: TStaticMongoDBRegisterOptions;
  aMongoDBIdentifier: word = 0): TRest; overload;


function ToText(eac: TRestStorageMongoDBEngineAddComputeID): PShortString; overload;



implementation


{ *********** TRestStorageMongoDB for REST Storage Over MongoDB }

{ TRestStorageMongoDB }

constructor TRestStorageMongoDB.Create(aClass: TOrmClass; aServer: TRestOrmServer);
begin
  inherited Create(aClass, aServer);
  // ConnectionProperties should have been set in StaticMongoDBRegister()
  fCollection := fStoredClassMapping^.ConnectionProperties as TMongoCollection;
  InternalLog('will store % using %', [aClass, Collection], sllInfo);
  BsonProjectionSet(fBsonProjectionSimpleFields, true,
    fStoredClassRecordProps.SimpleFieldsBits[ooSelect], nil, nil);
  BsonProjectionSet(fBsonProjectionBlobFields, false,
    fStoredClassRecordProps.FieldBits[oftBlob],
    @fBsonProjectionBlobFieldsNames, nil);
end;

function TRestStorageMongoDB.BsonProjectionSet(var Projection: variant;
  WithID: boolean; const Fields: TFieldBits; BsonFieldNames: PRawUtf8DynArray;
  const SubFields: TRawUtf8DynArray): integer;
var
  i, n, sf: integer;
  W: TBsonWriter;
  name: RawUtf8;
begin
  sf := length(SubFields);
  W := TBsonWriter.Create(TRawByteStringStream);
  try
    W.BsonDocumentBegin;
    if WithID then
      result := 1
    else
      result := 0;
    name := fStoredClassMapping^.RowIDFieldName;
    if sf > 0 then
      name := name + SubFields[0];
    W.BsonWrite(name, result);
    for i := 0 to fStoredClassRecordProps.Fields.Count - 1 do
      if i in Fields then
      begin
        name := fStoredClassMapping^.ExtFieldNames[i];
        if i + 1 < sf then
          name := name + SubFields[i + 1];
        W.BsonWrite(name, 1);
        inc(result);
      end;
    W.BsonDocumentEnd;
    W.ToBsonVariant(Projection);
    if BsonFieldNames <> nil then
      with fStoredClassMapping^ do
      begin
        SetLength(BsonFieldNames^, result);
        if WithID then
        begin
          BsonFieldNames^[0] := RowIDFieldName;
          n := 1;
        end
        else
          n := 0;
        for i := 0 to fStoredClassRecordProps.Fields.Count - 1 do
          if i in Fields then
          begin
            BsonFieldNames^[n] := ExtFieldNames[i];
            inc(n);
          end;
      end;
  finally
    W.Free;
  end;
end;

function TRestStorageMongoDB.CreateSqlMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean;
  IndexName: RawUtf8): boolean;
begin
  if (self = nil) or
     (fCollection = nil) or
     (Table <> fStoredClass) then
  begin
    result := false;
    exit;
  end;
  result := true;
  if (high(FieldNames) = 0) and
     IsRowID(pointer(FieldNames[0])) then
    exit; // ID primary key is always indexed by MongoDB
  try
    fCollection.EnsureIndex(FieldNames, true, Unique);
  except
    result := false;
  end;
end;

procedure TRestStorageMongoDB.Drop;
var
  DB: TMongoDatabase;
  CollName: RawUtf8;
begin
  DB := Collection.Database;
  CollName := Collection.Name;
  Collection.Drop;
  fCollection := DB.CollectionOrCreate[CollName];
  fEngineLastID := 0;
end;

destructor TRestStorageMongoDB.Destroy;
begin
  inherited;
  FreeAndNil(fBatchWriter);
  fEngineGenerator.Free;
  InternalLog('Destroy for % using %', [fStoredClass, Collection], sllInfo);
end;

function TRestStorageMongoDB.TableHasRows(Table: TOrmClass): boolean;
begin
  if (fCollection = nil) or
     (Table <> fStoredClass) then
    result := false
  else
    result := not fCollection.IsEmpty;
end;

function TRestStorageMongoDB.TableRowCount(Table: TOrmClass): Int64;
begin
  if (fCollection = nil) or
     (Table <> fStoredClass) then
    result := 0
  else
    result := fCollection.Count;
end;

procedure TRestStorageMongoDB.SetEngineAddComputeIdentifier(aIdentifier: word);
begin
  fEngineGenerator.Free;
  fEngineGenerator := TSynUniqueIdentifierGenerator.Create(aIdentifier);
  fEngineAddCompute := eacSynUniqueIdentifier;
end;

function TRestStorageMongoDB.EngineNextID: TID;

  procedure ComputeMax_ID;
  var
    res: variant;
    timer: TPrecisionTimer;
  begin
    timer.Start;
    case fEngineAddCompute of
      eacLastIDOnce, eacLastIDEachTime:
        begin
          res := fCollection.FindDoc(BsonVariant(
            '{$query:{},$orderby:{_id:-1}}'), BsonVariant(['_id', 1]));
          if not VarIsEmptyOrNull(res) then
            fEngineLastID := _Safe(res)^.I['_id'];
        end;
      eacMaxIDOnce, eacMaxIDEachTime:
        begin
          res := fCollection.AggregateDocFromJson(
            '{$group:{_id:null,max:{$max:"$_id"}}}');
          if not VarIsEmptyOrNull(res) then
            fEngineLastID := _Safe(res)^.I['max'];
        end;
    else
      raise EOrmMongoDB.CreateUtf8(
        'Unexpected %.EngineNextID with %', [self, ToText(fEngineAddCompute)^]);
    end;
    InternalLog('ComputeMax_ID=% in % using %',
      [fEngineLastID, timer.Stop, ToText(fEngineAddCompute)^], sllInfo);
  end;

begin
  if (fEngineAddCompute = eacSynUniqueIdentifier) and
     (fEngineGenerator <> nil) then
  begin
    result := fEngineGenerator.ComputeNew;
    fEngineLastID := result;
    exit;
  end;
  EnterCriticalSection(fStorageCriticalSection);
  if (fEngineLastID = 0) or
     (fEngineAddCompute in [eacLastIDEachTime, eacMaxIDEachTime]) then
    ComputeMax_ID;
  inc(fEngineLastID);
  result := fEngineLastID;
  LeaveCriticalSection(fStorageCriticalSection);
end;

function TRestStorageMongoDB.DocFromJson(const Json: RawUtf8;
  Occasion: TOrmOccasion; var Doc: TDocVariantData): TID;
var
  i, ndx: PtrInt;
  dt: TDateTime;
  blob: RawBlob;
  info: TOrmPropInfo;
  rtti: TRttiJson;
  js, RecordVersionName: RawUtf8;
  MissingID: boolean;
  V: PVarData;
begin
  Doc.InitJson(Json, [dvoValueCopiedByReference, dvoAllowDoubleValue]);
  if (Doc.Kind <> dvObject) and
     (Occasion <> ooInsert) then
    raise EOrmMongoDB.CreateUtf8('%.DocFromJson: invalid Json context', [self]);
  if not (Occasion in [ooInsert, ooUpdate]) then
    raise EOrmMongoDB.CreateUtf8(
      'Unexpected %.DocFromJson(Occasion=%)', [self, ToText(Occasion)^]);
  MissingID := true;
  for i := Doc.Count - 1 downto 0 do // downwards for doc.Delete(i) below
    if IsRowID(pointer(Doc.Names[i])) then
    begin
      Doc.Names[i] := fStoredClassMapping^.RowIDFieldName;
      VariantToInt64(Doc.Values[i], Int64(result));
      if (Occasion = ooUpdate) or
         (result = 0) then
        // update does not expect any $set:{_id:..}
        Doc.Delete(i)
      else
        // leave true if value is not an integer (=0)
        MissingID := false;
    end
    else
    begin
      ndx := fStoredClassRecordProps.Fields.IndexByName(Doc.Names[i]);
      if ndx < 0 then
        raise EOrmMongoDB.CreateUtf8(
          '%.DocFromJson: unknown field name [%]', [self, Doc.Names[i]]);
      Doc.Names[i] := fStoredClassMapping^.ExtFieldNames[ndx];
      info := fStoredClassRecordProps.Fields.List[ndx];
      V := @Doc.Values[i];
      case V^.VType of
        varInteger:
          // normalize 32-bit integer values into MongoDB boolean or date/time
          case info.OrmFieldType of
            oftBoolean:
              begin
                // normalize to boolean BSON
                if V^.VInteger <> 0 then
                  V^.VBoolean := true;
                // doc.InitJson/GetVariantFromJson store 0,1 as varInteger
                V^.VType := varBoolean;
              end;
            oftUnixTime:
              begin
                V^.VDate := UnixTimeToDateTime(V^.VInteger);
                V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
              end;
            oftUnixMSTime: // (very unlikely for actual time)
              begin
                V^.VDate := UnixMSTimeToDateTime(V^.VInteger);
                V^.VType := varDate;
              end;
          end;
        varInt64:
          // normalize some 64-bit integer values into MongoDB date/time
          case info.OrmFieldType of
            oftUnixTime:
              begin
                V^.VDate := UnixTimeToDateTime(V^.VInt64);
                V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
              end;
            oftUnixMSTime:
              begin
                V^.VDate := UnixMSTimeToDateTime(V^.VInt64);
                V^.VType := varDate;
              end;
          end;
        varString:
          // handle some TEXT values
          case info.OrmFieldType of
            oftDateTime, oftDateTimeMS:
              begin
                // ISO-8601 text as MongoDB date/time
                Iso8601ToDateTimePUtf8CharVar(
                  V^.VAny, length(RawByteString(V^.VAny)), dt);
                RawByteString(V^.VAny) := '';
                V^.VType := varDate; // direct set to avoid unexpected EInvalidOp
                V^.VDate := dt;
              end;
            oftBlob, oftBlobCustom:
              begin
                // store Base64-encoded BLOB as binary
                blob := BlobToRawBlob(RawByteString(V^.VAny));
                BsonVariantType.FromBinary(blob, bbtGeneric, Variant(V^));
              end;
            oftBlobDynArray:
              begin
                // store dynamic array as object (if has any Json)
                blob := BlobToRawBlob(RawByteString(V^.VAny));
                if blob = '' then
                  SetVariantNull(Variant(V^))
                else
                begin
                  rtti := (info as TOrmPropInfoRttiDynArray).PropRtti;
                  if rtti.ArrayRtti.Cache.RttiOrd = roUByte then
                    // TBytes or TByteDynArray stored as BSON binary
                    js := ''
                  else
                    // try to store dynamic array as BSON array (via Json)
                    js := DynArrayBlobSaveJson(rtti.Info, pointer(blob));
                  if (js <> '') and
                     (PInteger(js)^ and $00ffffff <> JSON_BASE64_MAGIC_C) then
                    BsonVariantType.FromJson(pointer(js), Variant(V^))
                  else
                    BsonVariantType.FromBinary(blob, bbtGeneric, Variant(V^));
                end;
              end;
          end;
        // oftObject,oftVariant,oftUtf8Custom were already converted to object from Json
      end;
    end;
  if Occasion = ooInsert then
    if MissingID then
    begin
      result := EngineNextID;
      Doc.AddValue(fStoredClassMapping^.RowIDFieldName, result);
    end
    else
    begin
      if fEngineAddCompute = eacSynUniqueIdentifier then
        raise EOrmMongoDB.CreateUtf8('%.DocFromJson: unexpected set ' +
          '%.ID=% with %', [self, fStoredClass, result, fEngineGenerator]);
      EnterCriticalSection(fStorageCriticalSection);
      if result > fEngineLastID then
        fEngineLastID := result;
      LeaveCriticalSection(fStorageCriticalSection);
    end;
  if fStoredClassRecordProps.RecordVersionField <> nil then
  begin
    RecordVersionName := fStoredClassMapping^.ExtFieldNames[
      fStoredClassRecordProps.RecordVersionField.PropertyIndex];
    if Doc.GetValueIndex(RecordVersionName) < 0 then
      if Owner = nil then
        raise EOrmMongoDB.CreateUtf8(
          '%.DocFromJson: unexpected Owner=nil with %.%: TRecordVersion',
          [self, fStoredClass, fStoredClassRecordProps.RecordVersionField.Name])
      else
        // compute new monotonic TRecordVersion value if not supplied by sender
        Doc.AddValue(RecordVersionName, Owner.RecordVersionCompute);
    if (Owner <> nil) and
       (Owner.Owner <> nil) and
       (Owner.Owner.Services <> nil) then
      (Owner.Owner.Services as TServiceContainerServer).
        RecordVersionNotifyAddUpdate(Occasion, fStoredClassProps.TableIndex, Doc);
  end;
  if Doc.Kind <> dvObject then
    raise EOrmMongoDB.CreateUtf8(
      '%.DocFromJson: Invalid Json context', [self]);
end;

function TRestStorageMongoDB.EngineAdd(TableModelIndex: integer;
  const SentData: RawUtf8): TID;
var
  doc: TDocVariantData;
begin
  if (fCollection = nil) or
     (TableModelIndex < 0) or
     (fModel.Tables[TableModelIndex] <> fStoredClass) then
    result := 0
  else
  try
    result := DocFromJson(SentData, ooInsert, doc);
    if fBatchMethod <> mNone then
      if (fBatchMethod <> mPOST) or
         (fBatchWriter = nil) then
        result := 0
      else
      begin
        inc(fBatchIDsCount);
        fBatchWriter.BsonWriteDoc(doc);
      end
    else
    begin
      fCollection.Insert([variant(doc)]);
      if Owner <> nil then
      begin
        Owner.InternalUpdateEvent(oeAdd, TableModelIndex, result, SentData, nil);
        Owner.FlushInternalDBCache;
      end;
    end;
  except
    result := 0;
  end;
end;

function TRestStorageMongoDB.EngineUpdate(TableModelIndex: integer; ID: TID;
  const SentData: RawUtf8): boolean;
var
  doc: TDocVariantData;
  query, update: variant; // use explicit TBsonVariant for type safety
begin
  if (fCollection = nil) or
     (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else
  try
    DocFromJson(SentData, ooUpdate, doc);
    query := BsonVariant(['_id', ID]);
    update := BsonVariant(['$set', variant(doc)]);
    fCollection.Update(query, update);
    if Owner <> nil then
    begin
      Owner.InternalUpdateEvent(oeUpdate, TableModelIndex, ID, SentData, nil);
      Owner.FlushInternalDBCache;
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUtf8): boolean;
var
  json: RawUtf8;
  query, update: variant; // use explicit TBsonVariant for type safety
  id: TBsonIterator;
begin
  if (fCollection = nil) or
     (TableModelIndex < 0) or
     (fModel.Tables[TableModelIndex] <> fStoredClass) or
     (SetFieldName = '') or
     (SetValue = '') or
     (WhereFieldName = '') or
     (WhereValue = '') then
    result := false
  else
  try
    // use {%:%} here since WhereValue/SetValue are already json encoded
    query := BsonVariant('{%:%}',
      [fStoredClassMapping^.InternalToExternal(WhereFieldName), WhereValue], []);
    update := BsonVariant('{$set:{%:%}}',
      [fStoredClassMapping^.InternalToExternal(SetFieldName), SetValue], []);
    fCollection.Update(query, update);
    if Owner <> nil then
    begin
      if Owner.InternalUpdateEventNeeded(TableModelIndex) and
         id.Init(fCollection.FindBson(query, BsonVariant(['_id', 1]))) then
      begin
        JsonEncodeNameSQLValue(SetFieldName, SetValue, json);
        while id.Next do
          Owner.InternalUpdateEvent(oeUpdate, TableModelIndex,
            id.Item.DocItemToInteger('_id'), json, nil);
      end;
      Owner.FlushInternalDBCache;
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.EngineUpdateFieldIncrement(TableModelIndex: integer;
  ID: TID; const FieldName: RawUtf8; Increment: Int64): boolean;
var
  Value: Int64;
begin
  result := false;
  if (ID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    exit;
  if (Owner <> nil) and
     Owner.InternalUpdateEventNeeded(TableModelIndex) then
    result := OneFieldValue(fStoredClass, FieldName, 'ID=?', [], [ID], Value) and
              UpdateField(fStoredClass, ID, FieldName, [Value + Increment])
  else
  try
    fCollection.Update(
      BsonVariant(['_id', ID]),
      BsonVariant('{$inc:{%:%}}',
        [fStoredClassMapping^.InternalToExternal(FieldName), Increment], []));
    if Owner <> nil then
      Owner.FlushInternalDBCache;
    result := true;
  except
    on Exception do
      result := false;
  end;
end;

function TRestStorageMongoDB.EngineUpdateBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; const BlobData: RawBlob): boolean;
var
  query, update, blob: variant; // use explicit TBsonVariant for type safety
  FieldName: RawUtf8;
  AffectedField: TFieldBits;
begin
  if (fCollection = nil) or
     (BlobField = nil) or
     (aID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else
  try
    query := BsonVariant(['_id', aID]);
    FieldName := fStoredClassMapping^.InternalToExternal(BlobField);
    BsonVariantType.FromBinary(BlobData, bbtGeneric, blob);
    update := BsonVariant(['$set', BsonVariant([FieldName, blob])]);
    fCollection.Update(query, update);
    if Owner <> nil then
    begin
      fStoredClassRecordProps.FieldBitsFromBlobField(BlobField, AffectedField);
      Owner.InternalUpdateEvent(oeUpdateBlob, TableModelIndex, aID, '',
        @AffectedField);
      Owner.FlushInternalDBCache;
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.UpdateBlobFields(Value: TOrm): boolean;
var
  query, blob: variant;
  update: TDocVariantData;
  info: TOrmPropInfo;
  blobRaw: RawByteString;
  aID: TID;
  f: PtrInt;
begin
  result := false;
  if (fCollection = nil) or
     (POrmClass(Value)^ <> fStoredClass) or
     (Value = nil) then
    exit;
  aID := Value.ID;
  if aID <= 0 then
    exit;
  query := BsonVariant(['_id', aID]);
  update.Init(JSON_FAST);
  for f := 0 to fStoredClassRecordProps.Fields.Count - 1 do
  begin
    info := fStoredClassRecordProps.Fields.List[f];
    if info.OrmFieldType = oftBlob then
    begin
      (info as TOrmPropInfoRttiRawBlob).GetBlob(Value, blobRaw);
      BsonVariantType.FromBinary(blobRaw, bbtGeneric, blob);
      update.AddValue(fStoredClassMapping^.ExtFieldNames[f], blob);
    end;
  end;
  if update.Count > 0 then
  try
    fCollection.Update(query, BsonVariant(['$set', variant(update)]));
    if Owner <> nil then
    begin
      Owner.InternalUpdateEvent(oeUpdateBlob, fStoredClassProps.TableIndex, aID,
        '', @fStoredClassRecordProps.FieldBits[oftBlob]);
      Owner.FlushInternalDBCache;
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.EngineDelete(TableModelIndex: integer;
  ID: TID): boolean;
begin
  result := false;
  if (fCollection <> nil) and
     (TableModelIndex >= 0) and
     (Model.Tables[TableModelIndex] = fStoredClass) and
     (ID > 0) then
  try
    if fBatchMethod <> mNone then
      if fBatchMethod <> mDelete then
        exit
      else
        AddID(fBatchIDs, fBatchIDsCount, ID)
    else
    begin
      if Owner <> nil then
      begin
        // notify BEFORE deletion
        Owner.InternalUpdateEvent(oeDelete, TableModelIndex, ID, '', nil);
        Owner.FlushInternalDBCache;
      end;
      fCollection.RemoveOne(ID);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.EngineDeleteWhere(TableModelIndex: Integer;
  const SqlWhere: RawUtf8; const IDs: TIDDynArray): boolean;
var
  i: PtrInt;
begin
  // here we use the pre-computed IDs[]
  result := false;
  if (fCollection <> nil) and
     (TableModelIndex >= 0) and
     (Model.Tables[TableModelIndex] = fStoredClass) and
     (IDs <> nil) then
  try
    if Owner <> nil then // notify BEFORE deletion
      for i := 0 to high(IDs) do
        Owner.InternalUpdateEvent(oeDelete, TableModelIndex, IDs[i], '', nil);
    fCollection.Remove(
      BsonVariant(['_id',
        BsonVariant(['$in', BsonVariantFromInt64s(TInt64DynArray(IDs))])]));
    if Owner <> nil then
      Owner.FlushInternalDBCache;
    result := true;
  except
    result := false;
  end;
end;

procedure TRestStorageMongoDB.JsonFromDoc(var doc: TDocVariantData;
  var result: RawUtf8);
var
  i: PtrInt;
  name: RawUtf8;
  W: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  if (doc.VarType <> DocVariantType.VarType) or
     (doc.Kind <> dvObject) or
     (doc.Count = 0) then
  begin
    result := '';
    exit;
  end;
  W := TTextWriter.CreateOwnedStream(tmp);
  try
    W.Add('{');
    for i := 0 to doc.Count - 1 do
    begin
      name := fStoredClassMapping^.ExternalToInternalOrNull(doc.Names[i]);
      if name = '' then
        raise EOrmMongoDB.CreateUtf8('%.JsonFromDoc: Unknown field [%] for %',
          [self, doc.Names[i], fStoredClass]);
      W.AddProp(pointer(name), Length(name));
      W.AddVariant(doc.Values[i], twJsonEscape);
      W.AddComma;
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TRestStorageMongoDB.EngineRetrieve(TableModelIndex: integer;
  ID: TID): RawUtf8;
var
  doc: variant;
begin
  result := '';
  if (fCollection = nil) or
     (ID <= 0) then
    exit;
  doc := fCollection.FindDoc(
    BsonVariant(['_id', ID]), fBsonProjectionSimpleFields, 1);
  JsonFromDoc(_Safe(doc)^, result);
end;

function TRestStorageMongoDB.EngineRetrieveBlob(TableModelIndex: integer;
  aID: TID; BlobField: PRttiProp; out BlobData: RawBlob): boolean;
var
  doc: variant;
  data: TVarData;
  FieldName: RawUtf8;
begin
  if (fCollection = nil) or
     (BlobField = nil) or
     (aID <= 0) or
     (TableModelIndex < 0) or
     (Model.Tables[TableModelIndex] <> fStoredClass) then
    result := false
  else
  try
    FieldName := fStoredClassMapping^.InternalToExternal(BlobField);
    doc := fCollection.FindDoc(
      BsonVariant(['_id', aID]), BsonVariant([FieldName, 1]), 1);
    if _Safe(doc)^.GetVarData(FieldName, data) then
      BsonVariantType.ToBlob(variant(data), RawByteString(BlobData));
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.RetrieveBlobFields(Value: TOrm): boolean;
var
  aID, f: Integer;
  doc: variant;
  docv: PDocVariantData;
  blob: TVarData;
  blobRaw: RawByteString;
begin
  result := false;
  if (fCollection = nil) or
     (POrmClass(Value)^ <> fStoredClass) or
     (Value = nil) then
    exit;
  aID := Value.ID;
  if aID <= 0 then
    exit;
  try
    doc := fCollection.FindDoc(
      BsonVariant(['_id', aID]), fBsonProjectionBlobFields, 1);
    docv := _Safe(doc);
    if docv^.Kind <> dvObject then
      exit; // not found
    for f := 0 to high(fStoredClassRecordProps.BlobFields) do
    begin
      if (f < docv^.Count) and // optimistic O(1) search
         IdemPropNameU(docv^.Names[f], fBsonProjectionBlobFieldsNames[f]) then
        BsonVariantType.ToBlob(docv^.Values[f], blobRaw)
      else if docv^.GetVarData(fBsonProjectionBlobFieldsNames[f], blob) then
        BsonVariantType.ToBlob(variant(blob), blobRaw)
      else
        raise EOrmMongoDB.CreateUtf8(
          '%.RetrieveBlobFields(%): field [%] not found',
          [self, Value, fBsonProjectionBlobFieldsNames[f]]);
      (fStoredClassRecordProps.BlobFields[f] as TOrmPropInfoRttiRawBlob).
        SetBlob(Value, blobRaw);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.AdaptSqlForEngineList(var SQL: RawUtf8): boolean;
begin
  result := true; // we do not have any Virtual Table yet -> always accept
end;

function TRestStorageMongoDB.SearchField(const FieldName, FieldValue: RawUtf8;
  out ResultID: TIDDynArray): boolean;
var
  query: variant;
  id: TBsonIterator;
  n: integer; // an external count is actually faster
begin
  if (fCollection = nil) or
     (FieldName = '') or
     (FieldValue = '') then
    result := false
  else
  try
    // use {%:%} here since FieldValue is already JSON encoded
    query := BsonVariant('{%:%}',
      [fStoredClassMapping^.InternalToExternal(FieldName), FieldValue], []);
    // retrieve the IDs for this query
    if id.Init(fCollection.FindBson(query, BsonVariant(['_id', 1]))) then
    begin
      n := 0;
      while id.Next do
        AddInt64(TInt64DynArray(ResultID), n, id.Item.DocItemToInteger('_id'));
      SetLength(ResultID, n);
      result := true;
    end
    else
      result := false;
  except
    result := false;
  end;
end;

function TRestStorageMongoDB.GetJsonValues(const Res: TBsonDocument;
  const extFieldNames: TRawUtf8DynArray; W: TJsonSerializer): integer;

  function itemFind(item: PBsonElement; itemcount, o1ndx: integer;
    const aName: RawUtf8): PBsonElement;
  var
    aNameLen, i: integer;
  begin
    aNameLen := length(aName);
    if aNameLen <> 0 then
    begin
      if o1ndx < itemcount then
      begin
        // O(1) optimistic search
        result := @PAnsiChar(item)[o1ndx * sizeof(item^)];
        if (result^.NameLen = aNameLen) and
           IdemPropNameUSameLenNotNull(pointer(aName), result^.name, aNameLen) then
          exit;
      end;
      result := item;
      for i := 1 to itemcount do // O(n) search if field missing or moved
        if (result^.NameLen = aNameLen) and
           IdemPropNameUSameLenNotNull(pointer(aName), result^.name, aNameLen) then
          exit
        else
          inc(result);
    end;
    result := nil;
  end;

var
  col, colCount: integer;
  row: TBsonIterator;
  item: array of TBsonElement;
  itemcount, itemsize: integer;
  itemfound: PBsonElement;
begin
  result := 0; // number of data rows in JSON output
  if W.Expand then
    W.Add('[');
  if row.Init(Res) then
  begin
    colCount := length(extFieldNames);
    if colCount <> length(W.ColNames) then
      raise EOrmMongoDB.CreateUtf8(
        '%.GetJsonValues(%): column count concern %<>%',
        [self, StoredClass, colCount, length(W.ColNames)]);
    itemsize := colCount;
    SetLength(item, itemsize);
    while row.Next do
    begin
      // retrieve all values of this BSON document into item[]
      if (row.Item.Kind <> betDoc) or
         (row.Item.Data.DocList = nil) then
        raise EOrmMongoDB.CreateUtf8('%.GetJsonValues(%): invalid row kind=%',
          [self, StoredClass, ord(row.Item.Kind)]);
      itemcount := 0;
      while row.Item.Data.DocList^ <> byte(betEOF) do
      begin
        if itemcount >= itemsize then
        begin
          inc(itemsize);
          Setlength(item, itemsize); // a field was deleted from TOrm
        end;
        if not item[itemcount].FromNext(row.Item.Data.DocList) then
          break;
        inc(itemcount);
      end;
      // convert this BSON document as JSON, following expected column order
      if W.Expand then
        W.Add('{');
      for col := 0 to colCount - 1 do
      begin
        if W.Expand then
          W.AddString(W.ColNames[col]);
        itemfound := itemFind(pointer(item), itemcount, col, extFieldNames[col]);
        if itemfound = nil then
          // this field may not exist (e.g. older schema)
          W.AddShort('null')
        else
          itemfound^.AddMongoJson(W, modNoMongo);
        W.AddComma;
      end;
      W.CancelLastComma;
      if W.Expand then
        W.Add('}', ',')
      else
        W.AddComma;
      inc(result);
    end;
  end;
  if (result = 0) and W.Expand then
  begin
    // we want the field names at least, even with no data
    W.Expand := false; //  {"fieldCount":2,"values":["col1","col2"]}
    W.CancelAll;
    fStoredClassRecordProps.SetJsonWriterColumnNames(W, 0);
  end;
  W.EndJsonObject(0, result);
end;

const
  ORDERBY_FIELD: array[boolean] of Integer = (1, -1);

function TRestStorageMongoDB.EngineList(const SQL: RawUtf8; ForceAjax: boolean;
  ReturnedRowCount: PPtrInt): RawUtf8;
var
  ResCount: PtrInt;
  Stmt: TSelectStatement;
  Query: variant;
  TextOrderByFirstField: RawUtf8;

  procedure AddWhereClause(B: TBsonWriter);
  var
    n, w: integer;
    FieldName: RawUtf8;
    joinedOR: boolean;
  begin
    n := Length(Stmt.Where);
    if (n > 1) and
       Stmt.Where[1].joinedOR then
    begin
      for w := 2 to n - 1 do
        if not Stmt.Where[w].joinedOR then
        begin
          InternalLog('%.EngineList: mixed AND/OR not supported for [%]',
            [ClassType, SQL], sllError);
          exit;
        end;
      // e.g. {$or:[{quantity:{$lt:20}},{price:10}]}
      B.BsonDocumentBegin('$or', betArray);
      joinedOR := true;
    end
    else
      joinedOR := false;
    for w := 0 to n - 1 do
    begin
      if joinedOR then
        B.BsonDocumentBegin(UInt32ToUtf8(w));
      with Stmt.Where[w] do
      begin
        FieldName := fStoredClassMapping^.FieldNameByIndex(Field - 1) + SubField;
        if not B.BsonWriteQueryOperator(
            FieldName, NotClause, Operation, ValueVariant) then
        begin
          InternalLog(
            '%.EngineList: operator % not supported for field [%] in [%]',
            [ClassType, ToText(Operation)^, FieldName, SQL], sllError);
          exit;
        end;
      end;
      if joinedOR then
        B.BsonDocumentEnd;
    end;
    if joinedOR then
      B.BsonDocumentEnd;
    B.BsonDocumentEnd;
  end;

  function ComputeQuery: boolean;
  var
    B: TBsonWriter;
    n, i: integer;
  begin
    // here we compute a BSON query, since it is the fastest
    result := false;
    if Stmt.SqlStatement = '' then
    begin
      InternalLog('%.EngineList: Invalid SQL statement [%]',
        [ClassType, SQL], sllError);
      exit;
    end;
    if (Stmt.Where = nil) and
       (Stmt.OrderByField = nil) then
    begin
      // no WHERE clause
      result := true;
      SetVariantNull(Query); // void query -> returns all rows
      exit;
    end;
    if Stmt.WhereHasParenthesis then
    begin
      InternalLog('%.EngineList: parenthesis not supported in [%]',
        [ClassType, SQL], sllError);
      exit;
    end;
    B := TBsonWriter.Create(TRawByteStringStream);
    try
      B.BsonDocumentBegin;
      if Stmt.OrderByField <> nil then
      begin
        B.BsonDocumentBegin('$query');
        AddWhereClause(B);
        n := high(Stmt.OrderByField);
        if (n = 0) and
           (Stmt.OrderByField[0] > 0) and
           (Stmt.Limit = 0) and
           (Stmt.Offset = 0) and
           (fStoredClassRecordProps.Fields.List[Stmt.OrderByField[0] - 1].
             OrmFieldType in [oftAnsiText, oftUtf8Text]) then
          // $orderby is case sensitive with MongoDB -> client-side sort
          TextOrderByFirstField := fStoredClassMapping^.FieldNameByIndex(
            Stmt.OrderByField[0] - 1)
        else if n >= 0 then
        begin
          // non textual or multiple fields -> server-side sort
          B.BsonDocumentBegin('$orderby');
          for i := 0 to n do
            B.BsonWrite(fStoredClassMapping^.FieldNameByIndex(
              Stmt.OrderByField[i] - 1),
              ORDERBY_FIELD[i in Stmt.OrderByFieldDesc]);
          B.BsonDocumentEnd;
        end;
        B.BsonDocumentEnd;
      end
      else
        AddWhereClause(B);
      B.ToBsonVariant(Query);
    finally
      B.Free;
    end;
    result := true; // indicates success
  end;

  procedure SetCount(aCount: integer);
  begin
    result := FormatUtf8('[{"Count(*)":%}]'#$A, [aCount]);
    ResCount := 1;
  end;

  procedure ComputeAggregate;
  type
    TFunc = (
      funcMax, funcMin, funcAvg, funcSum, funcCount);
  const
    FUNCT: array[TFunc] of RawUtf8 = (
      '$max', '$min', '$avg', '$sum', '$sum');
  var
    i: integer;
    func: TFunc;
    distinct: integer;
    B: TBsonWriter;
    distinctName, name, value: RawUtf8;
  begin
    distinct := -1;
    for i := 0 to high(Stmt.Select) do
      if IdemPropNameU(Stmt.Select[i].FunctionName, 'distinct') then
        if distinct >= 0 then
        begin
          InternalLog('%.EngineList: distinct() only allowed once in [%]',
            [ClassType, SQL], sllError);
          exit;
        end
        else
        begin
          distinct := Stmt.Select[i].Field;
          distinctName := fStoredClassMapping^.FieldNameByIndex(distinct - 1);
        end;
    B := TBsonWriter.Create(TRawByteStringStream);
    try
      B.BsonDocumentBegin;
      if Stmt.Where <> nil then
      begin
        B.BsonDocumentBeginInArray('$match');
        AddWhereClause(B);
      end;
      B.BsonDocumentBeginInArray('$group');
      if distinct >= 0 then
      begin
        for i := 0 to high(Stmt.GroupByField) do
          if Stmt.GroupByField[i] <> distinct then
          begin
            InternalLog('%.EngineList: Distinct(%) expected GROUP BY % in [%]',
              [ClassType, distinctName, distinctName, SQL], sllError);
            exit;
          end;
        B.BsonWrite('_id', '$' + distinctName);
      end
      else if length(Stmt.GroupByField) = 0 then
        B.BsonWrite('_id', betNull)
      else
      begin
        B.BsonDocumentBegin('_id');
        for i := 0 to high(Stmt.GroupByField) do
        begin
          name := fStoredClassMapping^.FieldNameByIndex(
            Stmt.GroupByField[i] - 1);
          B.BsonWrite(name, '$' + name);
        end;
        B.BsonDocumentEnd;
      end;
      for i := 0 to high(Stmt.Select) do
        with Stmt.Select[i] do
        begin
          if FunctionKnown = funcDistinct then
            continue;
          func := TFunc(FindPropName(
            ['max', 'min', 'avg', 'sum', 'count'], FunctionName));
          if ord(func) < 0 then
          begin
            InternalLog('%.EngineList: unexpected function %() in [%]',
              [ClassType, FunctionName, SQL], sllError);
            exit;
          end;
          B.BsonDocumentBegin('f' + UInt32ToUtf8(i));
          if func = funcCount then
            B.BsonWrite(FUNCT[func], 1)
          else
            B.BsonWrite(FUNCT[func], '$' +
              fStoredClassMapping^.FieldNameByIndex(Field - 1));
          B.BsonDocumentEnd;
        end;
      B.BsonDocumentEnd;
      if Stmt.OrderByField <> nil then
      begin
        if (length(Stmt.OrderByField) <> 1) or
           (Stmt.OrderByField[0] <> distinct) then
        begin
          InternalLog('%.EngineList: ORDER BY should match Distinct(%) in [%]',
            [ClassType, distinctName, SQL], sllError);
          exit;
        end;
        B.BsonDocumentBeginInArray('$sort');
        B.BsonWrite('_id', ORDERBY_FIELD[0 in Stmt.OrderByFieldDesc]);
        B.BsonDocumentEnd;
      end;
      B.BsonDocumentBeginInArray('$project');
      B.BsonWrite('_id', 0);
      for i := 0 to high(Stmt.Select) do
        with Stmt.Select[i] do
        begin
          if Alias <> '' then
            // name is the output ODM TOrm field
            name := Alias
          else
          begin
            if Field = 0 then
              name := ROWID_TXT
            else
              name := fStoredClassRecordProps.Fields.List[Field - 1].Name;
            if SubField <> '' then // 'field.subfield1.subfield2'
              name := name + SubField;
            if FunctionName <> '' then
              if FunctionKnown = funcDistinct then
              begin
                B.BsonWrite(name, '$_id');
                continue;
              end
              else
                name := FunctionName + '(' + name + ')';
          end;
          value := '$f' + UInt32ToUtf8(i);
          if ToBeAdded <> 0 then
          begin
            B.BsonDocumentBegin(name);
            B.BsonDocumentBegin('$add', betArray);
            B.BsonWrite('0', value);
            B.BsonWrite('1', ToBeAdded);
            B.BsonDocumentEnd(2);
          end
          else
            B.BsonWrite(name, value);
        end;
      B.BsonDocumentEnd(3);
      B.ToBsonVariant(Query, betArray);
    finally
      B.Free;
    end;
    result := fCollection.AggregateJsonFromVariant(Query);
  end;

var
  T: TOrmTableJson;
  W: TJsonSerializer;
  MS: TRawByteStringStream;
  Res: TBsonDocument;
  limit: PtrInt;
  extFieldNames, subFields: TRawUtf8DynArray;
  bits: TFieldBits;
  withID: boolean;
  Projection: variant;
begin
  // same logic as in TRestStorageInMemory.EngineList()
  result := ''; // indicates error occurred
  ResCount := 0;
  if self = nil then
    exit;
  InternalLog(SQL, sllSQL);
  StorageLock(false, 'EngineList');
  try
    if IdemPropNameU(fBasicSqlCount, SQL) then
      SetCount(TableRowCount(fStoredClass))
    else if IdemPropNameU(fBasicSqlHasRows[false], SQL) or
            IdemPropNameU(fBasicSqlHasRows[true], SQL) then
      if TableRowCount(fStoredClass) = 0 then
      begin
        // collection is void
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
      Stmt := TSelectStatement.Create(SQL,
        fStoredClassRecordProps.Fields.IndexByName,
        fStoredClassRecordProps.SimpleFieldSelect);
      try
        if (Stmt.SqlStatement = '') or // parsing failed
          not IdemPropNameU(Stmt.TableName, fStoredClassRecordProps.SqlTableName) then
          // invalid request -> return '' to mark error
          exit;
        if Stmt.SelectFunctionCount <> 0 then
          if (length(Stmt.Select) = 1) and
             (Stmt.Select[0].Alias = '') and
             IdemPropNameU(Stmt.Select[0].FunctionName, 'count') then
            if Stmt.Where = nil then
              // was "SELECT Count(*) FROM TableName;"
              SetCount(TableRowCount(fStoredClass))
            else
              // was "SELECT Count(*) FROM TableName WHERE ..."
              if ComputeQuery then
                SetCount(fCollection.FindCount(Query))
              else
                exit
          else
            // e.g. SELECT Distinct(Age),max(RowID) FROM TableName GROUP BY Age
            ComputeAggregate
        else
          // save rows as JSON from returned BSON
        if ComputeQuery then
        begin
          if Stmt.HasSelectSubFields then
            SetLength(subFields, fStoredClassRecordProps.Fields.Count + 1);
          Stmt.SelectFieldBits(bits, withID, pointer(subFields));
          BsonProjectionSet(Projection, withID, bits, @extFieldNames, subFields);
          if Stmt.Limit = 0 then
            limit := maxInt
          else
            limit := Stmt.Limit;
          Res := fCollection.FindBson(Query, Projection, limit, Stmt.Offset);
          MS := TRawByteStringStream.Create;
          try
            W := fStoredClassRecordProps.CreateJsonWriter(MS,
              ForceAjax or (Owner = nil) or not Owner.Owner.NoAjaxJson,
              withID, bits, 0);
            try
              ResCount := GetJsonValues(Res, extFieldNames, W);
              result := MS.DataString;
            finally
              W.Free;
            end;
          finally
            MS.Free;
          end;
          if TextOrderByFirstField <> '' then
          begin
            // $orderby is case sensitive with MongoDB -> client-side sort
            T := TOrmTableJson.CreateFromTables([fStoredClass], SQL,
              pointer(result), length(result));
            try
              T.SortFields(T.FieldIndex(TextOrderByFirstField),
                not(0 in Stmt.OrderByFieldDesc), nil, oftUtf8Text);
              result := T.GetJsonValues(W.Expand);
            finally
              T.Free;
            end;
          end;
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

function TRestStorageMongoDB.EngineExecute(const aSql: RawUtf8): boolean;
begin
  result := false; // it is a NO SQL engine, we said! :)
end;

function TRestStorageMongoDB.InternalBatchStart(Method: TUriMethod;
  BatchOptions: TRestBatchOptions): boolean;
begin
  result := false; // means BATCH mode not supported
  if Method in [mPOST, mDELETE] then
  begin
    StorageLock(true, 'InternalBatchStart'); // protected by try..finally in TRestServer.RunBatch
    try
      if (fBatchMethod <> mNone) or
         (fBatchWriter <> nil) then
        raise EOrmException.CreateUtf8(
          '%.InternalBatchStop should have been called', [self]);
      fBatchIDsCount := 0;
      fBatchMethod := Method;
      case Method of
        mPOST:
          // POST=ADD=INSERT -> EngineAdd() will add to fBatchWriter
          fBatchWriter := TBsonWriter.Create(TRawByteStringStream);
        //mDELETE:
          // EngineDelete() will add deleted ID to fBatchIDs[]
      end;
      result := true; // means BATCH mode is supported
    finally
      if not result then // release lock on error
        StorageUnLock;
    end;
  end;
end;

procedure TRestStorageMongoDB.InternalBatchStop;
var
  docs: TBsonDocument;
begin
  try
    case fBatchMethod of
      mPOST:
        begin
          // Add/Insert
          if fBatchWriter.TotalWritten = 0 then
            exit; // nothing to add
          fBatchWriter.ToBsonDocument(docs);
          fCollection.Insert(docs);
        end;
      mDELETE:
        begin
          SetLength(fBatchIDs, fBatchIDsCount);
          fCollection.Remove(BsonVariant(['_id', BsonVariant(['$in',
            BsonVariantFromInt64s(TInt64DynArray(fBatchIDs))])]));
        end;
    else
      raise EOrmException.CreateUtf8('%.InternalBatchStop(%) with BatchMethod=%',
        [self, StoredClass, MethodText(fBatchMethod)]);
    end;
  finally
    FreeAndNil(fBatchWriter);
    fBatchIDs := nil;
    fBatchIDsCount := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;



{ *********** High-Level Functions to Initialize MongoDB ORM }

function ToText(eac: TRestStorageMongoDBEngineAddComputeID): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestStorageMongoDBEngineAddComputeID), ord(eac));
end;


function StaticMongoDBRegister(aClass: TOrmClass; aServer: TRestOrmServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUtf8;
  aMapAutoFieldsIntoSmallerLength: boolean): TRestStorageMongoDB;
var
  Props: TOrmModelProperties;
begin
  result := nil;
  if (aServer = nil) or
     (aClass = nil) or
     (aMongoDatabase = nil) then
    exit; // avoid GPF
  if aMongoDatabase.Client.Log = nil then
    aMongoDatabase.Client.SetLog(aServer.LogClass);
  with aServer.LogClass.Enter do
  begin
    Props := aServer.Model.Props[aClass];
    if Props = nil then
      // if aClass is not part of the model
      exit;
    if aMongoCollectionName = '' then
      aMongoCollectionName := Props.Props.SqlTableName;
    Props.ExternalDB.Init(aClass, aMongoCollectionName,
      aMongoDatabase.CollectionOrCreate[aMongoCollectionName], true, []);
    Props.ExternalDB.MapField('ID', '_id');
    result := TRestStorageMongoDB.Create(aClass, aServer);
    aServer.StaticTableSetup(Props.TableIndex, result, sStaticDataTable);
  end;
end;

function StaticMongoDBRegisterAll(aServer: TRestOrmServer;
  aMongoDatabase: TMongoDatabase; aOptions: TStaticMongoDBRegisterOptions;
  aMongoDBIdentifier: word): boolean;
var
  i: PtrInt;
  storage: TRestStorageMongoDB;
begin
  if (aServer = nil) or
     (aMongoDatabase = nil) then
  begin
    result := false;
    exit; // avoid GPF
  end;
  result := true;
  with aServer.Model do
    for i := 0 to high(Tables) do
      if (mrDoNotRegisterUserGroupTables in aOptions) and
         (Tables[i].InheritsFrom(TAuthGroup) or
          Tables[i].InheritsFrom(TAuthUser)) then
        continue
      else
      begin
        storage := StaticMongoDBRegister(Tables[i], aServer, aMongoDatabase, '');
        if storage = nil then
          result := false
        else if aMongoDBIdentifier <> 0 then
          storage.SetEngineAddComputeIdentifier(aMongoDBIdentifier);
      end;
  if result then
    // ensure TOrm.InitializeTable() is called
    aServer.InitializeTables([]); // will create indexes and default data
end;

function TRestMongoDBCreate(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aHandleAuthentication: boolean;
  aOptions: TStaticMongoDBRegisterOptions; aMongoDBIdentifier: word): TRest;
var
  client: TMongoClient;
  database: TMongoDatabase;
  server, port, pwd: RawUtf8;
  tls: boolean;
  p: integer;
begin
  result := nil;
  if aDefinition = nil then
    exit;
  if IdemPChar(pointer(aDefinition.Kind), 'MONGODB') then
  begin
    Split(aDefinition.ServerName, ':', server, port);
    if (server = '') or
       (server[1] in ['?', '*']) or
       (aDefinition.DatabaseName = '') then
      // check mandatory MongoDB IP and Database
      exit;
    p := Utf8ToInteger(port, 1024, 65535, MONGODB_DEFAULTPORT);
    tls := ord(aDefinition.Kind[8]) in [ord('S'), ord('s')]; // 'MongoDBS'
    client := TMongoClient.Create(server, p, tls);
    try
      with aDefinition do
        if (User <> '') and
           (Password <> '') then
        begin
          pwd := PasswordPlain;
          database := client.OpenAuth(DatabaseName, User, pwd);
        end
        else
          database := client.Open(DatabaseName);
      result := CreateInMemoryServerForAllVirtualTables(
        aModel, aHandleAuthentication);
      StaticMongoDBRegisterAll(
        (result as TRestServer).OrmInstance as TRestOrmServer,
        database, aOptions, aMongoDBIdentifier);
      result.PrivateGarbageCollector.Add(client); // connection owned by server
    except
      FreeAndNil(result);
      client.Free; // avoid memory leak
    end;
  end
  else
    // Kind is not 'MongoDB' -> try other known TRest class
    result := TRest.CreateTryFrom(aModel, aDefinition, aHandleAuthentication);
end;


end.

