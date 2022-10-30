unit MongoDBTestCases;

{$I mormot.defines.inc}

interface

// if defined, will test with 5000 records instead of the default 100 records
{.$define ADD5000}

// if defined, will create the DB with one "toto" user, to validate authentication
{.$define TESTMONGOAUTH}

// if defined, will add some verbose log for debugging
{$define WITHLOG}

uses
  sysutils,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.log,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.test,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.nosql.bson,
  mormot.db.nosql.mongodb,
  mormot.orm.base,
  mormot.orm.core,
  {%H-}mormot.orm.rest,
  mormot.orm.mongodb,
  mormot.rest.sqlite3;

const
  //
  MONGOSERVER = 'localhost';
  //MONGOSERVER = '10.0.2.2'; // e.g. from a VirtualBox VM
  MONGOPORT = 27017;

type
  TTestDirect = class(TSynTestCase)
  protected
    fClient: TMongoClient;
    fDB: TMongoDatabase;
    fValues: TVariantDynArray;
    fExpectedCount: integer;
    procedure CleanUp; override;
  published
    procedure ConnectToLocalServer;
    procedure DropAndPrepareCollection;
    procedure FillCollection;
    procedure DropCollection;
    procedure FillCollectionBulk;
    procedure GracefulReconnect;
    procedure ReadCollection;
    procedure UpdateCollection;
    procedure DeleteSomeItems;
  end;

  TOrmOdm = class(TOrm)
  private
    fAge: integer;
    fName: RawUTF8;
    fDate: TDateTime;
    fValue: variant;
    fInts: TIntegerDynArray;
    fCreateTime: TCreateTime;
    fData: TSQLRawBlob;
    fFP: double;
  published
    property Name: RawUTF8
      read fName write fName stored AS_UNIQUE;
    property Age: integer
      read fAge write fAge;
    property Date: TDateTime
      read fDate write fDate;
    property Value: variant
      read fValue write fValue;
    property Ints: TIntegerDynArray index 1
      read fInts write fInts;
    property Data: TSQLRawBlob
      read fData write fData;
    property CreateTime: TCreateTime
      read fCreateTime write fCreateTime;
    property FP: double
      read fFP write fFP;
  end;

  TTestORM = class(TSynTestCase)
  protected
    fMongoClient: TMongoClient;
    fDB: TMongoDatabase;
    fModel: TOrmModel;
    fClient: TRestClientDB;
    fStorageMongoDB: TRestStorageMongoDB;
    fStartTimeStamp: TTimeLog;
    fUpdateOffset: integer;
    procedure TestOne(R: TOrmOdm; aID: integer);
    procedure CleanUp; override;
  published
    procedure ConnectToLocalServer;
    procedure Insert;
    procedure InsertInBatchMode;
    procedure Retrieve;
    procedure RetrieveAll;
    procedure RetrieveOneWithWhereClause;
    procedure RetrieveFromSQL;
    procedure Update;
    procedure Blobs;
    procedure Delete;
    procedure DeleteInBatchMode;
  end;

  TTestMongoDB = class(TSynTestsLogged)
  published
    procedure DirectAccess;
    procedure ORM;
  end;

  {$ifdef MONGO_OLDPROTOCOL}
  // new OP_MSG protocol has always an acknowledged protocol
  TTestDirectWithAcknowledge = class(TTestDirect);
  TTestDirectWithoutAcknowledge = class(TTestDirect);
  TTestORMWithAcknowledge = class(TTestORM);
  TTestORMWithoutAcknowledge = class(TTestORM);
  {$endif MONGO_OLDPROTOCOL}



implementation

{ TTestMongoDB }

procedure TTestMongoDB.DirectAccess;
begin
  {$ifdef WITHLOG}
  TSynLog.Family.Level := LOG_VERBOSE;
  {$endif WITHLOG}
  AddCase([
    {$ifdef MONGO_OLDPROTOCOL}
    TTestDirectWithAcknowledge,
    TTestDirectWithoutAcknowledge
    {$else}
    TTestDirect
    {$endif MONGO_OLDPROTOCOL}
    ]);
end;

procedure TTestMongoDB.ORM;
begin
  AddCase([
    {$ifdef MONGO_OLDPROTOCOL}
    TTestORMWithAcknowledge,
    TTestORMWithoutAcknowledge
    {$else}
    TTestORM
    {$endif MONGO_OLDPROTOCOL}
    ]);
end;


{ TTestDirect }

const
  COLL_NAME = 'direct';

  {$ifndef ADD5000}
  COLL_COUNT = 100;
  HASH1 = 2200155474;
  {$else}
  COLL_COUNT = 5000;
  HASH1 = 2499978132;
  {$endif ADD5000}

{$ifdef TESTMONGOAUTH}

const
  DB_NAME = 'testauth';
  USER_NAME = 'toto';
  USER_PWD = 'pass';

var
  UserCreated: boolean;

{$else}

const
  DB_NAME = 'testplain';

{$endif TESTMONGOAUTH}

procedure TTestDirect.CleanUp;
begin
  FreeAndNil(fClient);
end;

procedure TTestDirect.ConnectToLocalServer;
var
  serverTime: TDateTime;
  res: variant;
  errmsg: RawUTF8;
begin
  assert(fClient = nil);
  {$ifdef TESTMONGOAUTH}
  if not UserCreated then
  begin
    fClient := TMongoClient.Create(MONGOSERVER, MONGOPORT);
    with fClient.Database[DB_NAME] do
    begin
      DropUser(USER_NAME);
      Check(CreateUserForThisDatabase(USER_NAME, USER_PWD, true) = '');
    end;
    fClient.Free;
    UserCreated := true;
  end;
  {$endif TESTMONGOAUTH}
  fClient := TMongoClient.Create(MONGOSERVER, MONGOPORT);
  {$ifdef MONGO_OLDPROTOCOL}
  if ClassType = TTestDirectWithAcknowledge then
    fClient.WriteConcern := wcAcknowledged
  else if ClassType = TTestDirectWithoutAcknowledge then
    fClient.WriteConcern := wcUnacknowledged
  else
    assert(false);
  {$endif MONGO_OLDPROTOCOL}
  {$ifdef WITHLOG}
  fClient.SetLog(TSynLog); // define some verbose log
  {$endif WITHLOG}
  {$ifdef TESTMONGOAUTH}
  fDB := fClient.OpenAuth(DB_NAME, USER_NAME, USER_PWD); 
  {$else}
  fDB := fClient.Database[DB_NAME];
  {$endif TESTMONGOAUTH}
  Check(fDB <> nil);
  Check(fDB.Name = DB_NAME);
  errmsg := fDB.RunCommand('hostInfo', res);
  if CheckFailed(errmsg = '') or
     CheckFailed(not VarIsNull(res.system)) then
    exit;
  serverTime := res.system.currentTime; // direct conversion to TDateTime
  Check(serverTime <> 0);
  CheckSame(Now, serverTime, 0.5);
  if System.Pos('MongoDB', Owner.CustomVersions) = 0 then
    Owner.CustomVersions := format(
      '%sUsing %s'#13#10'Running on %s'#13#10'Compiled with mORMot ' +
        SYNOPSE_FRAMEWORK_VERSION, [Owner.CustomVersions,
      fClient.ServerBuildInfoText, OSVersionText]);
  fExpectedCount := COLL_COUNT;
end;

procedure TTestDirect.DropAndPrepareCollection;
var
  Coll: TMongoCollection;
  errmsg: RawUTF8;
  dat: TDateTime;
  i: integer;
begin
  assert(fDB <> nil);
  Coll := fDB.CollectionOrNil[COLL_NAME];
  if Coll <> nil then
    Check(Coll.Drop = '');
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Check(Coll.Name = COLL_NAME);
  Check(Coll.FullCollectionName = DB_NAME + '.' + COLL_NAME);
  Check(Coll.Database = fDB);
  Check(fDB.Collection[COLL_NAME] = Coll);
  Check(fDB.CollectionOrCreate[COLL_NAME] = Coll);
  errmsg := Coll.Drop;
  CheckUTF8(fClient.ServerBuildInfoNumber > 20000, errmsg);
  fValues := nil;
  SetLength(fValues, COLL_COUNT);
  for i := 0 to COLL_COUNT - 1 do
  begin
    TDocVariant.New(fValues[i]);
    fValues[i]._id := ObjectID; // null
    fValues[i].Name := 'Name ' + IntToStr(i + 1);
    fValues[i].FirstName := 'FirstName ' + IntToStr(i + COLL_COUNT);
    fValues[i].Number := i;
    dat := 1.0 * (30000 + i);
    fValues[i].Date := dat;
  end;
end;

procedure TTestDirect.FillCollection;
var
  Coll: TMongoCollection;
  oid: TBSONObjectID;
  i: integer;
  jsonArray: RawUTF8;
  bytes: Int64;
  docs: TVariantDynArray;
begin
  fDB.CollectionOrNil[COLL_NAME].Drop;
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Coll.EnsureIndex(['Name']);
  bytes := fClient.BytesTransmitted;
  for i := 0 to COLL_COUNT - 1 do
  begin
    Check(Coll.Save(fValues[i], @oid) = (i < 0));
    Check(BSONVariantType.IsOfKind(fValues[i]._id, betObjectID));
    Check(oid.Equal(fValues[i]._id), 'EnsureDocumentHasID failure');
  end;
  NotifyTestSpeed('rows inserted', COLL_COUNT, fClient.BytesTransmitted - bytes);
  CheckEqual(Coll.Count, COLL_COUNT);
  jsonArray := Coll.FindJSON(null, BSONVariant('{_id:0}'));
  CheckEqual(JSONArrayCount(@jsonArray[2]), COLL_COUNT);
  CheckEqual(Hash32(jsonArray), HASH1, 'projection over a collection');
  Coll.FindDocs('{Number:{$gt:?}}', [COLL_COUNT shr 1], docs, null);
  CheckEqual(length(docs), COLL_COUNT - (COLL_COUNT shr 1) - 1);
  for i := 0 to high(docs) do
    Check(docs[i].number > COLL_COUNT shr 1);
end;

procedure TTestDirect.DropCollection;
begin
  fDB.CollectionOrNil[COLL_NAME].Drop;
end;

procedure TTestDirect.FillCollectionBulk;
var
  Coll: TMongoCollection;
  jsonArray: RawUTF8;
  bytes: Int64;
begin
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Coll.EnsureIndex(['Name']);
  bytes := fClient.BytesTransmitted;
  Coll.Insert(fValues); // insert all values at once
  NotifyTestSpeed('rows inserted', Coll.Count, fClient.BytesTransmitted - bytes);
  jsonArray := Coll.FindJSON(null, BSONVariant('{_id:0}'));
  CheckEqual(JSONArrayCount(@jsonArray[2]), COLL_COUNT);
  CheckEqual(Hash32(jsonArray), HASH1, 'projection over a collection');
end;

procedure TTestDirect.GracefulReconnect;
var
  Coll: TMongoCollection;
  jsonArray: RawUTF8;
begin
  fClient.Connections[0].Close; // simulate server connection close
  Coll := fDB.Collection[COLL_NAME];
  CheckEqual(Coll.Count, COLL_COUNT);
  jsonArray := Coll.FindJSON(null, BSONVariant('{_id:0}'));
  CheckEqual(JSONArrayCount(@jsonArray[2]), COLL_COUNT);
  CheckEqual(Hash32(jsonArray), HASH1, 'projection over a collection');
end;

procedure TTestDirect.ReadCollection;
var
  i: integer;
  Coll: TMongoCollection;
  docs: variant;
  jsonOne, jsonArray: RawUTF8;
  bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  CheckEqual(Coll.Count, COLL_COUNT);
  bytes := fClient.BytesTransmitted;
  jsonArray := Coll.FindJSON(null, BSONVariant('{_id:0}'));
  CheckEqual(JSONArrayCount(@jsonArray[2]), COLL_COUNT);
  CheckEqual(Hash32(jsonArray), HASH1, 'projection over a collection');
  NotifyTestSpeed('rows read at once', Coll.Count, fClient.BytesTransmitted - bytes);
  for i := 0 to COLL_COUNT - 1 do
  begin
    jsonOne := VariantSaveMongoJSON(fValues[i], modMongoStrict);
    jsonArray := '[' + jsonOne + ']';
    CheckEqual(Coll.FindJSON(
      '{Name:?}', [fValues[i].Name]), jsonArray);
    CheckEqual(Coll.FindJSON(
      BSONVariant(['Name', 'Name ' + IntToStr(i + 1)]), null), jsonArray);
    CheckEqual(Coll.FindJSON(
      BSONVariant(['Name', 'Name ' + IntToStr(i + 1)]), null, 1), jsonOne);
    docs := Coll.FindDoc('{Name:?}', [fValues[i].Name]);
    CheckEqual(VariantSaveMongoJSON(docs, modMongoStrict), jsonArray);
    docs := Coll.FindDoc('{_id:?}', [fValues[i]._id]);
    CheckEqual(VariantSaveMongoJSON(docs, modMongoStrict), jsonArray);
    docs := Coll.FindDoc('{_id:?}', [fValues[i]._id], 1);
    CheckEqual(VariantSaveMongoJSON(docs, modMongoStrict), jsonOne);
  end;
end;

procedure TTestDirect.UpdateCollection;
var
  i: integer;
  Coll: TMongoCollection;
  jsonOne, jsonArray, expected: RawUTF8;
  bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count = COLL_COUNT);
  bytes := fClient.BytesTransmitted;
  for i := 0 to COLL_COUNT - 1 do
  begin
    fValues[i].Name := 'Name "' + IntToStr(i + 1);
    if i < COLL_COUNT div 3 then
      Check(not Coll.Save(fValues[i]))
    else if i < (COLL_COUNT div 3) * 2 then
      Coll.Update('{_id:?}', [fValues[i]._id], '?', [fValues[i]])
    else
      Coll.Update('{_id:?}', [fValues[i]._id], '{$set:{Name:?}}', [fValues[i].Name]);
  end;
  NotifyTestSpeed('rows updated', Coll.Count, fClient.BytesTransmitted - bytes);
  Check(Coll.Count = COLL_COUNT);
  for i := 0 to COLL_COUNT - 1 do
  begin
    jsonOne := Coll.FindJSON('{_id:?}', [fValues[i]._id], null, 1);
    expected := VariantSaveMongoJSON(fValues[i], modMongoStrict);
    Check(jsonOne = expected, 'in-place update');
  end;
  jsonArray := Coll.FindJSON(null, BSONVariant(['_id', 0, 'Date', 0, 'Number', 0]));
  Check(JSONArrayCount(@jsonArray[2]) = COLL_COUNT,
    'projection over an updated collection');
end;

procedure TTestDirect.DeleteSomeItems;
var
  beforeCount, i, j: integer;
  Coll: TMongoCollection;
  jsonOne: RawUTF8;
  bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count = fExpectedCount);
  bytes := fClient.BytesTransmitted;
  beforeCount := fExpectedCount;
  j := 0;
  for i := 0 to high(fValues) do
    if not VarIsNull(fValues[i]._id) then
    begin
      if j mod 5 = 0 then
      begin
        if j mod 10 = 0 then
          Coll.RemoveOne(BSONObjectID(fValues[i]._id))
        else if j mod 15 = 0 then
          Coll.RemoveOne(fValues[i]._id)
        else
          Coll.RemoveFmt('{_id:?}', [fValues[i]._id]);
        fValues[i]._id := null;
        dec(fExpectedCount);
      end;
      inc(j);
    end;
  NotifyTestSpeed('rows deleted', beforeCount - fExpectedCount,
   fClient.BytesTransmitted - bytes);
  Check(Coll.Count = fExpectedCount);
  for i := 0 to high(fValues) do
    if not VarIsNull(fValues[i]._id) then
    begin
      jsonOne := Coll.FindJSON('{_id:?}', [fValues[i]._id], null, 1);
      Check(jsonOne = VariantSaveMongoJSON(fValues[i], modMongoStrict), 'delete');
    end;
end;


{ TTestORM }

procedure TTestORM.ConnectToLocalServer;
begin
  fMongoClient := TMongoClient.Create(MONGOSERVER, MONGOPORT);
  {$ifdef MONGO_OLDPROTOCOL}
  if ClassType = TTestORMWithAcknowledge then
    fMongoClient.WriteConcern := wcAcknowledged
  else if ClassType = TTestORMWithoutAcknowledge then
    fMongoClient.WriteConcern := wcUnacknowledged
  else
    assert(false);
  {$endif MONGO_OLDPROTOCOL}
  {$ifdef TESTMONGOAUTH}
  fDB := fMongoClient.OpenAuth(DB_NAME, USER_NAME, USER_PWD); 
  {$else}
  fDB := fMongoClient.Database[DB_NAME];
  {$endif TESTMONGOAUTH}
  Check(fDB <> nil);
  Check(fDB.Name = DB_NAME);
  Check(fMongoClient.ServerBuildInfoNumber <> 0);
  fModel := TSQLModel.Create([TOrmOdm]);
  fClient := TSQLRestClientDB.Create(fModel, nil, ':memory:', TSQLRestServerDB);
  fStorageMongoDB := OrmMapMongoDB(TOrmOdm, fClient.Server.OrmInstance, fDB, 'mORMot');
  check(fStorageMongoDB <> nil);
  fClient.Server.CreateMissingTables;
  fStorageMongoDB.Drop;
  Check(fClient.TableRowCount(TOrmOdm) = 0);
  fStartTimeStamp := fClient.Server.Server.GetServerTimeStamp;
  Check(fStartTimeStamp > 10000);
  fClient.Server.NoAJAXJSON := true;
end;

procedure TTestORM.CleanUp;
begin
  FreeAndNil(fClient);
  FreeAndNil(fModel);
  FreeAndNil(fMongoClient);
end;

procedure TTestORM.Insert;
var
  R: TOrmOdm;
  i: integer;
  bytes: Int64;
begin
  CheckEqual(fClient.TableRowCount(TOrmOdm), 0);
  bytes := fMongoClient.BytesTransmitted;
  R := TOrmOdm.Create;
  try
    for i := 1 to COLL_COUNT do
    begin
      R.Name := 'Name ' + UInt32ToUTF8(i);
      R.Age := i and 63;
      R.Date := 1.0 * (30000 + i);
      R.Value := _ObjFast(['num', i]);
      R.Ints := nil;
      R.DynArray(1).Add(i);
      Check(fClient.Add(R, True) = i);
    end;
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows inserted', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
  CheckEqual(fClient.TableRowCount(TOrmOdm), COLL_COUNT);
  fStorageMongoDB.Drop;
end;

procedure TTestORM.InsertInBatchMode;
var
  R: TOrmOdm;
  i: integer;
  bytes: Int64;
  IDs: TIDDynArray;
begin
  Check(fClient.TableRowCount(TOrmOdm) = 0);
  bytes := fMongoClient.BytesTransmitted;
  fClient.BatchStart(TOrmOdm);
  R := TOrmOdm.Create;
  try
    for i := 1 to COLL_COUNT do
    begin
      R.Name := 'Name ' + Int32ToUTF8(i);
      R.Age := i and 63;
      R.Date := 1.0 * (30000 + i);
      R.Value := _ObjFast(['num', i]);
      R.Ints := nil;
      R.DynArray(1).Add(i);
      R.FP := i * 7.3445;
      Check(fClient.BatchAdd(R, True) >= 0);
    end;
  finally
    R.Free;
  end;
  CheckEqual(fClient.BatchSend(IDs), HTTP_SUCCESS, 'batchsend');
  CheckEqual(length(IDs), COLL_COUNT);
  NotifyTestSpeed('rows inserted', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
  CheckEqual(fClient.TableRowCount(TOrmOdm), COLL_COUNT);
end;

procedure TTestORM.TestOne(R: TOrmOdm; aID: integer);
begin
  CheckEqual(R.ID, aID);
  CheckEqual(R.Name, 'Name ' + Int32ToUTF8(aID));
  CheckEqual(R.Age, aID and 63 + fUpdateOffset);
  CheckSame(R.Date, 1.0 * (30000 + aID), 1E-5);
  Check(R.Value.num = aID + fUpdateOffset);
  CheckEqual(Length(R.Ints), 1);
  CheckEqual(R.Ints[0], aID);
  Check(R.CreateTime >= fStartTimeStamp);
  CheckSame(R.FP, aID * 7.3445);
end;

procedure TTestORM.Retrieve;
var
  R: TOrmOdm;
  i: integer;
  bytes: Int64;
begin
  CheckEqual(fClient.TableRowCount(TOrmOdm), COLL_COUNT, 'coll_count');
  bytes := fMongoClient.BytesTransmitted;
  R := TOrmOdm.Create;
  try
    for i := 1 to COLL_COUNT do
    begin
      Check(fClient.Retrieve(i, R));
      TestOne(R, i);
    end;
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows retrieved', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
end;

procedure TTestORM.RetrieveAll;
var
  n: integer;
  R: TOrmOdm;
  bytes: Int64;
begin
  bytes := fMongoClient.BytesTransmitted;
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n);
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows retrieved', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
end;

procedure TTestORM.RetrieveOneWithWhereClause;
var
  R: TOrmOdm;
  i, n: integer;
  bytes: Int64;
begin
  bytes := fMongoClient.BytesTransmitted;
  for i := 1 to COLL_COUNT do
  begin
    R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'ID=?', [i]);
    try
      n := 0;
      while R.FillOne do
      begin
        inc(n);
        TestOne(R, i);
      end;
      Check(n = 1);
    finally
      R.Free;
    end;
  end;
  NotifyTestSpeed('rows retrieved', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
end;

procedure TTestORM.RetrieveFromSQL;
var
  R: TOrmOdm;
  n, tot, tot2, total, stat: integer;
  i64: Int64;
  ages: TIntegerDynArray;
  prev: RawUTF8;
  doc: variant;
  T: TSQLTable;
  bytes: Int64;
begin
  bytes := fMongoClient.BytesTransmitted;
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Name=?', ['Name 43']);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, 43);
    end;
    Check(n = 1);
  finally
    R.Free;
  end;
  stat := n;
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age<?', [51]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check(R.Age < 51);
    end;
    Check(n > 50);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'not RowID=?', [50]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check(R.ID <> 50);
    end;
    Check(n = COLL_COUNT - 1);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'not Age<?', [52]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check(R.Age > 51);
    end;
    Check(n > 10);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age<? limit 10', [51]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n);
    end;
    Check(n = 10);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age<? limit 10 offset 10', [51]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n + 10);
    end;
    Check(n = 10);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'order by Name', []);
  try
    n := 0;
    total := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      inc(total, R.Age);
      if prev <> '' then
        Check(StrIComp(pointer(prev), pointer(R.Name)) < 0, 'order by name');
      prev := R.Name;
    end;
    Check(n = COLL_COUNT, 'client side sort of a text field');
    Check(total >= 2682);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age in (1,10,20)', []);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check(R.Age in [1, 10, 20]);
    end;
    Check(n >= 3);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age in (1,10,20) and ID=?', [10]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, 10);
    end;
    Check(n = 1);
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age in (10,20) or ID=?', [30]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check((R.Age in [10, 20]) or (R.ID = 30));
    end;
    Check(n >= 3, '{$or:[{Age:{$in:[10,20]}},{_id:30}]}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm,
    'Age in (10,20) or ID=? order by ID desc', [40]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, R.ID);
      Check((R.Age in [10, 20]) or (R.ID = 40));
    end;
    Check(n >= 3, '{$query:{$or:[{Age:{$in:[10,20]}},{_id:40}]},$orderby:{_id:-1}}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Name like ?', ['name 1%']);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      Check(IdemPChar(pointer(R.Name), 'NAME 1'));
      TestOne(R, R.ID);
    end;
    Check(n > 10, '{Name:/^name 1/i}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Name like ?', ['name 1']);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      Check(IdemPChar(pointer(R.Name), 'NAME 1'));
      TestOne(R, R.ID);
    end;
    Check(n = 1, '{Name:/^name 1$/i}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Name like ?', ['%ame 1%']);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      Check(IdemPChar(pointer(R.Name), 'NAME 1'));
      TestOne(R, R.ID);
    end;
    Check(n > 10, '{Name:/ame 1/i}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'not Name like ?', ['%ame 1%']);
  try
    tot := 0;
    while R.FillOne do
    begin
      inc(n);
      Check(not IdemPChar(pointer(R.Name), 'NAME 1'));
      TestOne(R, R.ID);
    end;
    Check(n + tot = COLL_COUNT, '{Name:/ame 1/i}');
  finally
    R.Free;
  end;
  inc(stat, n);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, 'Age in (1,10,20) and ' +
    'IntegerDynArrayContains(Ints,?)', [10]);
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, 10);
    end;
    Check(n = 1, '{Age:{$in:[1,10,20]},Ints:{$in:[10]}}');
  finally
    R.Free;
  end;
  inc(stat, n);
  check(fClient.OneFieldValue(TOrmOdm, 'count(*)', 'Data is null', [], [], i64));
  check(i64 = COLL_COUNT, '{Data:null}');
  check(fClient.OneFieldValue(TOrmOdm, 'count(*)', 'Data is not null', [], [], i64));
  check(i64 = 0, '{Data:{$ne:null}}');
  CheckEqual(fClient.RetrieveListJSON(
    TOrmOdm, '', [], 'min(RowID),max(RowID),Count(RowID)'),
    FormatUTF8('[{"min(RowID)":1,"max(RowID)":%,"Count(RowID)":%}]',
      [COLL_COUNT, COLL_COUNT]));
  doc := fClient.RetrieveDocVariant(TOrmOdm, '', [],
    'min(RowID) as a,max(RowID) as b,Count(RowID) as c');
  if checkfailed(not VarIsEmptyOrNull(doc), 'abc docvariant') then
    exit;
  check(doc.a = 1);
  check(doc.b = COLL_COUNT);
  check(doc.c = COLL_COUNT);
  doc := fClient.RetrieveDocVariant(TOrmOdm, '', [],
    'min(RowID) as a,max(RowID)+1 as b,Count(RowID) as c,sum(Age) as d');
  check(doc.a = 1);
  check(doc.b = COLL_COUNT + 1);
  check(doc.c = COLL_COUNT);
  check(doc.d = total);
  doc := fClient.RetrieveDocVariant(TOrmOdm, 'RowID=?', [50], 'max(RowID) as a');
  Check(doc.a = 50);
  doc := fClient.RetrieveDocVariant(TOrmOdm, 'RowID<?', [50], 'max(RowID) as a');
  Check(doc.a = 49);
  doc := fClient.RetrieveDocVariant(TOrmOdm, 'not RowID>=?', [50], 'max(RowID) as a');
  Check(doc.a = 49);
  doc := fClient.RetrieveDocVariant(TOrmOdm, 'not RowID=?', [50], 'count(RowID) as a');
  Check(doc.a = COLL_COUNT - 1);
  inc(stat, 9);
  T := fClient.MultiFieldValues(TOrmOdm, 'Distinct(Age),max(RowID) as first,' +
    'count(Age) as count,sum(Age) as total', 'order by age group by age');
  if not CheckFailed(T <> nil) then
  try
    n := 0;
    tot := 0;
    tot2 := 0;
    Check(T.FieldIndex('Age') = 0);
    Check(T.FieldIndex('first') = 1);
    Check(T.FieldIndex('count') = 2);
    Check(T.FieldIndex('total') = 3);
    while T.Step(false, @doc) do
    begin
      Check(AddInteger(ages, doc.Age, true));
      if n > 0 then
        Check(ages[n] > ages[n - 1]);
      Check(integer(doc.first) and 63 = doc.Age);
      inc(n);
      tot := tot + doc.Count;
      tot2 := tot2 + doc.total;
    end;
    CheckEqual(n, 64, 'agg count/sum Age');
    CheckEqual(tot, COLL_COUNT);
    CheckEqual(tot2, total);
  finally
    T.Free;
  end;
  inc(stat, n);
  NotifyTestSpeed('rows retrieved', stat, fMongoClient.BytesTransmitted - bytes);
end;

procedure TTestORM.Update;
var
  R: TOrmOdm;
  bytes: Int64;
  n: integer;
begin
  inc(fUpdateOffset);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    bytes := fMongoClient.BytesTransmitted;
    n := 0;
    while R.FillOne do
    begin
      R.Age := R.Age + fUpdateOffset;
      R.Value.num := R.Value.num + fUpdateOffset;
      fClient.Update(R);
      inc(n);
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows updated', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n);
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
end;

procedure TTestORM.Blobs;
var
  R: TOrmOdm;
  i, n: integer;
  blob, blobRead: TSQLRawBlob;
  bytes, i64: Int64;
begin
  SetLength(blob, 8);
  bytes := fMongoClient.BytesTransmitted;
  for i := 1 to COLL_COUNT do
  begin
    PIntegerArray(blob)[0] := i;
    PIntegerArray(blob)[1] := i * $01020304;
    Check(fClient.UpdateBlob(TOrmOdm, i, 'Data', blob));
  end;
  NotifyTestSpeed('rows updated', COLL_COUNT, fMongoClient.BytesTransmitted - bytes);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n);
      Check(R.Data = '');
      fClient.RetrieveBlob(TOrmOdm, n, 'Data', blobRead);
      PIntegerArray(blob)[0] := n;
      PIntegerArray(blob)[1] := n * $01020304;
      Check(blobRead = blob);
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '', 'ID,Data');
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      Check(R.ID = n);
      Check(R.Age = 0);
      PIntegerArray(blob)[0] := n;
      PIntegerArray(blob)[1] := n * $01020304;
      Check(R.Data = blob);
      PIntegerArray(blob)[0] := n * 2;
      PIntegerArray(blob)[1] := n * $02030405;
      R.Data := blob;
      fClient.Server.UpdateBlobFields(R);
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    while R.FillOne do
    begin
      inc(n);
      TestOne(R, n);
      Check(R.Data = '');
      Check(fClient.Server.RetrieveBlobFields(R));
      PIntegerArray(blob)[0] := n * 2;
      PIntegerArray(blob)[1] := n * $02030405;
      Check(R.Data = blob);
      R.Data := '';
    end;
    Check(n = COLL_COUNT);
  finally
    R.Free;
  end;
  check(fClient.OneFieldValue(TOrmOdm, 'count(*)', 'Data is null', [], [], i64));
  check(i64 = 0, '{Data:null}');
  check(fClient.OneFieldValue(TOrmOdm, 'count(*)', 'Data is not null', [], [], i64));
  check(i64 = COLL_COUNT, '{Data:{$ne:null}}');
end;

procedure TTestORM.Delete;
var
  i, n: integer;
  ExpectedCount: integer;
  bytes: Int64;
  temp: string;
  R: TOrmOdm;
begin
  Check(fClient.Delete(TOrmOdm, 'ID in (5,10,15)'), 'deletion with IN clause');
  bytes := fMongoClient.BytesTransmitted;
  ExpectedCount := COLL_COUNT - 3;
  for i := 20 to COLL_COUNT do
    if i mod 5 = 0 then
    begin
      Check(fClient.Delete(TOrmOdm, i));
      dec(ExpectedCount);
    end;
  NotifyTestSpeed('rows deleted', COLL_COUNT - ExpectedCount, fMongoClient.BytesTransmitted
    - bytes);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    i := 0;
    while R.FillOne do
    begin
      inc(i);
      if i mod 5 = 0 then
        inc(i);
      inc(n);
      TestOne(R, i);
    end;
    Check(n = ExpectedCount);
  finally
    R.Free;
  end;
  temp := fRunConsole;
  fStorageMongoDB.Drop;
  fUpdateOffset := 0;
  InsertInBatchMode;
  fRunConsole := temp;
end;

procedure TTestORM.DeleteInBatchMode;
var
  i, n: integer;
  ExpectedCount: integer;
  bytes: Int64;
  IDs: TIDDynArray;
  R: TOrmOdm;
begin
  bytes := fMongoClient.BytesTransmitted;
  ExpectedCount := COLL_COUNT;
  fClient.BatchStart(TOrmOdm);
  for i := 5 to COLL_COUNT do
    if i mod 5 = 0 then
    begin
      Check(fClient.BatchDelete(i) >= 0);
      dec(ExpectedCount);
    end;
  Check(fClient.BatchSend(IDs) = HTTP_SUCCESS);
  Check(length(IDs) = COLL_COUNT - ExpectedCount);
  NotifyTestSpeed('rows deleted', length(IDs), fMongoClient.BytesTransmitted - bytes);
  R := TOrmOdm.CreateAndFillPrepare(fClient.Orm, '');
  try
    n := 0;
    i := 0;
    while R.FillOne do
    begin
      inc(i);
      if i mod 5 = 0 then
        inc(i);
      inc(n);
      TestOne(R, i);
    end;
    Check(n = ExpectedCount);
  finally
    R.Free;
  end;
end;

end.

