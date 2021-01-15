/// regression tests for ORM process over external SQL DB engines
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.orm.extdb;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.mustache,
  mormot.core.test,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.sql.sqlite3,
  mormot.db.sql.oledb,
  mormot.db.nosql.bson,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static,
  mormot.db.proxy,
  mormot.orm.core,
  mormot.orm.storage,
  mormot.orm.sql,
  mormot.orm.rest,
  mormot.orm.client,
  mormot.orm.server,
  mormot.soa.core,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  mormot.rest.sqlite3,
  mormot.rest.http.server,
  mormot.rest.http.client,
  test.core.base,
  test.core.data,
  test.orm.sqlite3;

type
  /// a test case which will test most external DB functions of the
  // mormot.orm.sql.pas unit
  // - the external DB will be in fact a SQLite3 instance, expecting a
  // test.db3 file available in the current directory, populated with
  // some TOrmPeople rows
  // - note that SQL statement caching at SQLite3 engine level makes those test
  // 2 times faster: nice proof of performance improvement
  TTestExternalDatabase = class(TSynTestCase)
  protected
    fExternalModel: TOrmModel;
    fPeopleData: TOrmTable;
    /// called by ExternalViaREST/ExternalViaVirtualTable and
    // ExternalViaRESTWithChangeTracking tests method
    procedure Test(StaticVirtualTableDirect, TrackChanges: boolean);
  public
    /// release used instances (e.g. server) and memory
    procedure CleanUp; override;
  published
    /// test SynDB connection remote access via HTTP
    procedure _SynDBRemote;
    /// test TSqlDBConnectionProperties persistent as JSON
    procedure DBPropertiesPersistence;
    /// initialize needed RESTful client (and server) instances
    // - i.e. a RESTful direct access to an external DB
    procedure ExternalRecords;
    /// check the SQL auto-adaptation features
    procedure AutoAdaptSQL;
    /// check the per-db encryption
    // - the testpass.db3-wal file is not encrypted, but the main
    // testpass.db3 file will
    procedure CryptedDatabase;
    /// test external DB implementation via faster REST calls
    // - will mostly call directly the TRestStorageExternal instance,
    // bypassing the Virtual Table mechanism of SQLite3
    procedure ExternalViaREST;
    /// test external DB implementation via slower Virtual Table calls
    // - using the Virtual Table mechanism of SQLite3 is more than 2 times
    // slower than direct REST access
    procedure ExternalViaVirtualTable;
    /// test external DB implementation via faster REST calls and change tracking
    // - a TOrmHistory table will be used to store record history
    procedure ExternalViaRESTWithChangeTracking;
    {$ifndef CPU64}
    {$ifdef MSWINDOWS}
    /// test external DB using the JET engine
    procedure JETDatabase;
    {$endif MSWINDOWS}
    {$endif CPU64}
    {$ifdef MSWINDOWS}
    {$ifdef USEZEOS}
    /// test external Firebird embedded engine via Zeos/ZDBC (if available)
    procedure FirebirdEmbeddedViaZDBCOverHTTP;
    {$endif USEZEOS}
    {$endif MSWINDOWS}
  end;

type
  TOrmPeopleExt = class(TOrm)
  private
    fFirstName: RawUtf8;
    fLastName: RawUtf8;
    fData: RawBlob;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    fValue: TVariantDynArray;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUtf8
      index 40 read fFirstName write fFirstName;
    property LastName: RawUtf8
      index 40 read fLastName write fLastName;
    property Data: RawBlob
      read fData write fData;
    property YearOfBirth: integer
      read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word
      read fYearOfDeath write fYearOfDeath;
    property Value: TVariantDynArray
      read fValue write fValue;
    property LastChange: TModTime
      read fLastChange;
    property CreatedAt: TCreateTime
      read fCreatedAt write fCreatedAt;
  end;

  TOrmOnlyBlob = class(TOrm)
  private
    fData: RawBlob;
  published
    property Data: RawBlob
      read fData write fData;
  end;

  TOrmTestJoin = class(TOrm)
  private
    fName: RawUtf8;
    fPeople: TOrmPeopleExt;
  published
    property Name: RawUtf8
      index 30 read fName write fName;
    property People: TOrmPeopleExt
      read fPeople write fPeople;
  end;

  TOrmMyHistory = class(TOrmHistory);


implementation


{$ifdef MSWINDOWS}
{$ifdef USEZEOS}

uses
  mormot.db.sql.zeos;

{$endif USEZEOS}
{$endif MSWINDOWS}

type
  // class hooks to force DMBS property for TTestExternalDatabase.AutoAdaptSQL
  TSqlDBConnectionPropertiesHook = class(TSqlDBConnectionProperties);
  TRestStorageExternalHook = class(TRestStorageExternal);


{ TTestExternalDatabase }

procedure TTestExternalDatabase.ExternalRecords;
var
  sql: RawUtf8;
begin
  if CheckFailed(fExternalModel = nil) then
    exit; // should be called once
  fExternalModel := TOrmModel.Create([TOrmPeopleExt, TOrmOnlyBlob, TOrmTestJoin,
    TOrmASource, TOrmADest, TOrmADests, TOrmPeople, TOrmMyHistory]);
  ReplaceParamsByNames(ToUtf8(StringOfChar('?', 200)), sql);
  CheckHash(sql, $AD27D1E0, 'excludes :IF :OF');
end;

procedure TTestExternalDatabase.AutoAdaptSQL;
var
  SqlOrigin, s: RawUtf8;
  Props: TSqlDBConnectionProperties;
  Server: TRestServer;
  Ext: TRestStorageExternalHook;

  procedure Test(aDBMS: TSqlDBDefinition; AdaptShouldWork: boolean;
    const SQLExpected: RawUtf8 = '');
  var
    SQL: RawUtf8;
  begin
    SQL := SqlOrigin;
    TSqlDBConnectionPropertiesHook(Props).fDBMS := aDBMS;
    Check((Props.DBMS = aDBMS) or (aDBMS = dUnknown));
    Check(Ext.AdaptSQLForEngineList(SQL) = AdaptShouldWork);
    CheckUtf8(SameTextU(SQL, SQLExpected) or
          not AdaptShouldWork, SQLExpected + #13#10 + SQL);
  end;

  procedure Test2(const Orig, Expected: RawUtf8);
  var
    DBMS: TSqlDBDefinition;
  begin
    SqlOrigin := Orig;
    for DBMS := low(DBMS) to high(DBMS) do
      Test(DBMS, true, Expected);
  end;

begin
  check(ReplaceParamsByNumbers('', s) = 0);
  check(s = '');
  check(ReplaceParamsByNumbers('toto titi', s) = 0);
  check(s = 'toto titi');
  check(ReplaceParamsByNumbers('toto=? titi', s) = 1);
  check(s = 'toto=$1 titi');
  check(ReplaceParamsByNumbers('toto=? titi=?', s) = 2);
  check(s = 'toto=$1 titi=$2');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''''', s) = 2);
  check(s = 'toto=$1 titi=$2 and a=''''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''dd''', s) = 2);
  check(s = 'toto=$1 titi=$2 and a=''dd''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''d''''d''', s) = 2);
  check(s = 'toto=$1 titi=$2 and a=''d''''d''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''d?d''', s) = 2);
  check(s = 'toto=$1 titi=$2 and a=''d?d''');
  check(ReplaceParamsByNumbers('1?2?3?4?5?6?7?8?9?10?11?12? x', s) = 12);
  check(s = '1$12$23$34$45$56$67$78$89$910$1011$1112$12 x');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom([])), '');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom(['1'])), '{1}');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom(['''1'''])), '{"1"}');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom(['1', '2', '3'])), '{1,2,3}');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom(['''1''', '2', '''3'''])),
    '{"1",2,"3"}');
  checkequal(BoundArrayToJSONArray(TRawUtf8DynArrayFrom(['''1"1''', '2',
    '''"3\'''])), '{"1\"1",2,"\"3\\"}');

  check(TSqlDBConnectionProperties.IsSQLKeyword(dUnknown, 'SELEct'));
  check(not TSqlDBConnectionProperties.IsSQLKeyword(dUnknown, 'toto'));
  check(TSqlDBConnectionProperties.IsSQLKeyword(dOracle, 'SELEct'));
  check(not TSqlDBConnectionProperties.IsSQLKeyword(dOracle, 'toto'));
  check(TSqlDBConnectionProperties.IsSQLKeyword(dOracle, ' auDIT '));
  check(not TSqlDBConnectionProperties.IsSQLKeyword(dMySQL, ' auDIT '));
  check(TSqlDBConnectionProperties.IsSQLKeyword(dSQLite, 'SELEct'));
  check(TSqlDBConnectionProperties.IsSQLKeyword(dSQLite, 'clustER'));
  check(not TSqlDBConnectionProperties.IsSQLKeyword(dSQLite, 'value'));

  Server := TRestServerFullMemory.Create(fExternalModel);
  try
    Props := TSqlDBSQLite3ConnectionProperties.Create(
      SQLITE_MEMORY_DATABASE_NAME, '', '', '');
    try
      VirtualTableExternalMap(fExternalModel, TOrmPeopleExt, Props,
        'SampleRecord').MapField('LastChange', 'Changed');
      Ext := TRestStorageExternalHook.Create(
        TOrmPeopleExt, Server.OrmInstance as TRestOrmServer);
      try
        Test2('select rowid,firstname from PeopleExt where rowid=2',
          'select id,firstname from SampleRecord where id=2');
        Test2('select rowid,firstname from PeopleExt where rowid=?',
          'select id,firstname from SampleRecord where id=?');
        Test2('select rowid,firstname from PeopleExt where rowid>=?',
          'select id,firstname from SampleRecord where id>=?');
        Test2('select rowid,firstname from PeopleExt where rowid<?',
          'select id,firstname from SampleRecord where id<?');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and lastname=:(''toto''):',
          'select id,firstname from SampleRecord where id=2 and lastname=:(''toto''):');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and rowID=:(2): order by rowid',
          'select id,firstname from SampleRecord where id=2 and id=:(2): order by id');
        Test2('select rowid,firstname from PeopleExt where rowid=2 or lastname=:(''toto''):',
          'select id,firstname from SampleRecord where id=2 or lastname=:(''toto''):');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and not lastname like ?',
          'select id,firstname from SampleRecord where id=2 and not lastname like ?');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and not (lastname like ?)',
          'select id,firstname from SampleRecord where id=2 and not (lastname like ?)');
        Test2('select rowid,firstname from PeopleExt where (rowid=2 and lastname="toto") or lastname like ?',
          'select id,firstname from SampleRecord where (id=2 and lastname="toto") or lastname like ?');
        Test2('select rowid,firstname from PeopleExt where (rowid=2 or lastname=:("toto"):) and lastname like ?',
          'select id,firstname from SampleRecord where (id=2 or lastname=:("toto"):) and lastname like ?');
        Test2('select rowid,firstname from PeopleExt where (rowid=2) and (lastname="toto" or lastname like ?)',
          'select id,firstname from SampleRecord where (id=2) and (lastname="toto" or lastname like ?)');
        Test2('select rowid,firstname from PeopleExt where (rowid=2) and (lastname=:("toto"): or (lastname like ?))',
          'select id,firstname from SampleRecord where (id=2) and (lastname=:("toto"): or (lastname like ?))');
        Test2('select rowid,firstname from PeopleExt where rowid=2 order by RowID',
          'select id,firstname from SampleRecord where id=2 order by ID');
        Test2('select rowid,firstname from PeopleExt where rowid=2 order by RowID DeSC',
          'select id,firstname from SampleRecord where id=2 order by ID desc');
        Test2('select rowid,firstname from PeopleExt order by RowID,firstName DeSC',
          'select id,firstname from SampleRecord order by ID,firstname desc');
        Test2('select rowid, firstName from PeopleExt order by RowID, firstName',
          'select id,firstname from SampleRecord order by ID,firstname');
        Test2('select rowid, firstName from PeopleExt  order by RowID, firstName asC',
          'select id,firstname from SampleRecord order by ID,firstname');
        Test2('select rowid,firstname from PeopleExt where firstname like :(''test''): order by lastname',
          'select id,firstname from SampleRecord where firstname like :(''test''): order by lastname');
        Test2('   select    COUNT(*)  from   PeopleExt   ',
          'select count(*) from SampleRecord');
        Test2('select count(*) from PeopleExt where rowid=2',
          'select count(*) from SampleRecord where id=2');
        Test2('select Distinct(firstname) , max(lastchange)+100 from PeopleExt where rowid >= :(2):',
          'select Distinct(FirstName),max(Changed)+100 as LastChange from SampleRecord where ID>=:(2):');
        Test2('select Distinct(lastchange) , max(rowid)-100 as newid from PeopleExt where rowid >= :(2):',
          'select Distinct(Changed) as lastchange,max(id)-100 as newid from SampleRecord where ID>=:(2):');
        SqlOrigin := 'select rowid,firstname from PeopleExt where   rowid=2   limit 2';
        Test(dUnknown, false);
        Test(dDefault, false);
        Test(dOracle, true,
          'select id,firstname from SampleRecord where rownum<=2 and id=2');
        Test(dMSSQL, true, 'select top(2) id,firstname from SampleRecord where id=2');
        Test(dJet, true, 'select top 2 id,firstname from SampleRecord where id=2');
        Test(dMySQL, true, 'select id,firstname from SampleRecord where id=2 limit 2');
        Test(dSQLite, true, 'select id,firstname from SampleRecord where id=2 limit 2');
        SqlOrigin :=
          'select rowid,firstname from PeopleExt where rowid=2 order by LastName limit 2';
        Test(dUnknown, false);
        Test(dDefault, false);
        Test(dOracle, true,
          'select id,firstname from SampleRecord where rownum<=2 and id=2 order by LastName');
        Test(dMSSQL, true,
          'select top(2) id,firstname from SampleRecord where id=2 order by LastName');
        Test(dJet, true,
          'select top 2 id,firstname from SampleRecord where id=2 order by LastName');
        Test(dMySQL, true,
          'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
        Test(dSQLite, true,
          'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
        SqlOrigin :=
          'select rowid,firstname from PeopleExt where firstname=:(''test''): limit 2';
        Test(dUnknown, false);
        Test(dDefault, false);
        Test(dOracle, true,
          'select id,firstname from SampleRecord where rownum<=2 and firstname=:(''test''):');
        Test(dMSSQL, true,
          'select top(2) id,firstname from SampleRecord where firstname=:(''test''):');
        Test(dJet, true,
          'select top 2 id,firstname from SampleRecord where firstname=:(''test''):');
        Test(dMySQL, true,
          'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
        Test(dSQLite, true,
          'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
        SqlOrigin := 'select id,firstname from PeopleExt limit 2';
        Test(dUnknown, false);
        Test(dDefault, false);
        Test(dOracle, true, 'select id,firstname from SampleRecord where rownum<=2');
        Test(dMSSQL, true, 'select top(2) id,firstname from SampleRecord');
        Test(dJet, true, 'select top 2 id,firstname from SampleRecord');
        Test(dMySQL, true, 'select id,firstname from SampleRecord limit 2');
        Test(dSQLite, true, 'select id,firstname from SampleRecord limit 2');
        SqlOrigin := 'select id,firstname from PeopleExt order by firstname limit 2';
        Test(dUnknown, false);
        Test(dDefault, false);
        Test(dOracle, true,
          'select id,firstname from SampleRecord where rownum<=2 order by firstname');
        Test(dMSSQL, true,
          'select top(2) id,firstname from SampleRecord order by firstname');
        Test(dJet, true,
          'select top 2 id,firstname from SampleRecord order by firstname');
        Test(dMySQL, true,
          'select id,firstname from SampleRecord order by firstname limit 2');
        Test(dSQLite, true,
          'select id,firstname from SampleRecord order by firstname limit 2');
        SqlOrigin := 'SELECT RowID,firstname FROM PeopleExt WHERE :(3001): ' +
          'BETWEEN firstname AND RowID LIMIT 1';
        Test(dSQLite, false);
      finally
        Ext.Free;
      end;
    finally
      Props.Free;
    end;
  finally
    Server.Free;
  end;
end;

procedure TTestExternalDatabase.CleanUp;
begin
  FreeAndNil(fExternalModel);
  FreeAndNil(fPeopleData);
  inherited;
end;

procedure TTestExternalDatabase.ExternalViaREST;
begin
  Test(true, false);
end;

procedure TTestExternalDatabase.ExternalViaVirtualTable;
begin
  Test(false, false);
end;

procedure TTestExternalDatabase.ExternalViaRESTWithChangeTracking;
begin
  Test(true, true);
end;

{$ifdef MSWINDOWS}
{$ifdef USEZEOS}

procedure TTestExternalDatabase.FirebirdEmbeddedViaZDBCOverHTTP;
var
  R: TOrmPeople;
  Model: TOrmModel;
  Props: TSqlDBConnectionProperties;
  Server: TRestServerDB;
  Http: TRestHttpServer;
  Client: TRestClientURI;
  i, n: integer;
  ids: array[0..3] of TID;
  res: TIDDynArray;
begin
  if not FileExists(FIREBIRDEMBEDDEDDLL) then
    exit;
  Model := TOrmModel.Create([TOrmPeople]);
  try
    R := TOrmPeople.Create;
    try
      DeleteFile('test.fdb'); // will be re-created at first connection
      Props := TSqlDBZEOSConnectionProperties.Create(
        TSqlDBZEOSConnectionProperties.URI(
          dFirebird, '', FIREBIRDEMBEDDEDDLL, False), 'test.fdb', '', '');
      try
        VirtualTableExternalMap(Model, TOrmPeople, Props, 'peopleext').
          MapFields(['ID', 'key',
                     'YearOfBirth', 'yob']);
        Server := TRestServerDB.Create(Model, SQLITE_MEMORY_DATABASE_NAME);
        try
          Server.CreateMissingTables;
          Http := TRestHttpServer.Create(HTTP_DEFAULTPORT, Server);
          Client := TSQLHttpClient.Create('localhost', HTTP_DEFAULTPORT,
            TOrmModel.Create(Model));
          Client.Model.Owner := Client;
          try
            R.FillPrepare(fPeopleData);
            if not CheckFailed(R.fFill <> nil) then
            begin
              Client.BatchStart(TOrmPeople, 5000);
              n := 0;
              while R.FillOne do
              begin
                R.YearOfBirth := n;
                Client.BatchAdd(R, true);
                inc(n);
              end;
              Check(Client.BatchSend(res) = HTTP_SUCCESS);
              Check(length(res) = n);
              for i := 1 to 100 do
              begin
                R.ClearProperties;
                Check(Client.Retrieve(res[Random(n)], R));
                Check(R.ID <> 0);
                Check(res[R.YearOfBirth] = R.ID);
              end;
            end;
            for i := 0 to high(ids) do
            begin
              R.YearOfBirth := i;
              ids[i] := Client.Add(R, true);
            end;
            for i := 0 to high(ids) do
            begin
              Check(Client.Retrieve(ids[i], R));
              Check(R.YearOfBirth = i);
            end;
            for i := 0 to high(ids) do
            begin
              Client.BatchStart(TOrmPeople);
              Client.BatchDelete(ids[i]);
              Check(Client.BatchSend(res) = HTTP_SUCCESS);
              Check(length(res) = 1);
              Check(res[0] = HTTP_SUCCESS);
            end;
            for i := 0 to high(ids) do
              Check(not Client.Retrieve(ids[i], R));
            R.ClearProperties;
            for i := 0 to high(ids) do
            begin
              R.fID := ids[i];
              Check(Client.Update(R), 'test locking');
            end;
            for i := 0 to high(ids) do
            begin
              R.YearOfBirth := i;
              ids[i] := Client.Add(R, true);
            end;
            for i := 0 to high(ids) do
            begin
              Check(Client.Retrieve(ids[i], R));
              Check(R.YearOfBirth = i);
            end;
          finally
            Client.Free;
            Http.Free;
          end;
        finally
          Server.Free;
        end;
      finally
        Props.Free;
      end;
    finally
      R.Free;
    end;
  finally
    Model.Free;
  end;
end;

{$endif USEZEOS}
{$endif MSWINDOWS}

{$ifndef CPU64}
{$ifdef MSWINDOWS}

procedure TTestExternalDatabase.JETDatabase;
var
  R: TOrmPeople;
  Model: TOrmModel;
  Props: TSqlDBConnectionProperties;
  Client: TRestClientDB;
  i, n, ID, LastID: integer;
begin
  Model := TOrmModel.Create([TOrmPeople]);
  try
    R := TOrmPeople.Create;
    R.FillPrepare(fPeopleData);
    if not CheckFailed(R.FillContext <> nil) then
    try
      DeleteFile('test.mdb');
      Props := TSqlDBOleDBJetConnectionProperties.Create('test.mdb', '', '', '');
      try
        VirtualTableExternalRegister(Model, TOrmPeople, Props, '');
        Client := TRestClientDB.Create(
          Model, nil, SQLITE_MEMORY_DATABASE_NAME, TRestServerDB);
        try
          Client.Server.CreateMissingTables;
          Client.Orm.TransactionBegin(TOrmPeople);
          n := 0;
          while R.FillOne do
          begin
            inc(n);
            Check(Client.Orm.Add(R, true, true) =
              R.FillContext.Table.IDColumnHiddenValue(n));
            if n > 999 then
              break; // Jet is very slow e.g. within the Delphi IDE
          end;
          Client.Orm.Commit;
          R.FirstName := '';
          R.LastName := '';
          R.YearOfBirth := 100;
          R.YearOfDeath := 0;
          R.Data := '';
          LastID := Client.Orm.Add(R, true);
          for i := 1 to n do
          begin
            R.ClearProperties;
            ID := R.FillContext.Table.IDColumnHiddenValue(n);
            Check(Client.Orm.Retrieve(ID, R));
            Check(R.IDValue = ID);
            Check(R.ID = ID);
            Check(R.FirstName <> '');
            Check(R.YearOfBirth >= 1400);
            Check(R.YearOfDeath >= 1468);
          end;
          Check(Client.Orm.Retrieve(LastID, R));
          Check(R.FirstName = '');
          Check(R.LastName = '');
          Check(R.YearOfBirth = 100);
          Check(R.YearOfDeath = 0);
          Check(R.Data = '');
        finally
          Client.Free;
        end;
      finally
        Props.Free;
      end;
    finally
      R.Free;
    end;
  finally
    Model.Free;
  end;
end;

{$endif MSWINDOWS}
{$endif CPU64}

procedure TTestExternalDatabase._SynDBRemote;
var
  Props: TSqlDBConnectionProperties;

  procedure DoTest(proxy: TSqlDBConnectionProperties; msg: PUTF8Char);

    procedure DoTests;
    var
      res: ISqlDBRows;
      id, lastid, n, n1: integer;
      IDs: TIntegerDynArray;
      Row, RowDoc: variant;

      procedure DoInsert;
      var
        i: integer;
      begin
        for i := 0 to high(IDs) do
          Check(proxy.ExecuteNoResult(
            'INSERT INTO People (ID,FirstName,LastName,YearOfBirth,YearOfDeath) ' +
            'VALUES (?,?,?,?,?)', [IDs[i], 'FirstName New ' + Int32ToUtf8(i),
            'New Last', i + 1400, 1519]) = 1);
      end;

      function DoCount: integer;
      var
        res: ISqlDBRows;
      begin
        res := proxy.Execute(
          'select count(*) from People where YearOfDeath=?', [1519]);
        {%H-}Check(res.Step);
        result := res.ColumnInt(0);
      end;

    var
      log: ISynLog;
    begin
      log := TSynLogTestLog.Enter(proxy, msg);
      if proxy <> Props then
        Check(proxy.UserID = 'user');
      proxy.ExecuteNoResult('delete from people where ID>=?', [50000]);
      res := proxy.Execute('select * from People where YearOfDeath=?', [1519]);
      Check(res <> nil);
      n := 0;
      lastid := 0;
      while res.Step do
      begin
        id := res.ColumnInt('ID');
        Check(id <> lastid);
        Check(id > 0);
        lastid := id;
        Check(res.ColumnInt('YearOfDeath') = 1519);
        inc(n);
      end;
      Check(n = DoCount);
      n1 := n;
      n := 0;
      Row := res.RowData;
      if res.Step({rewind=}true) then
        repeat
          Check(Row.ID > 0);
          Check(Row.YearOfDeath = 1519);
          res.RowDocVariant(RowDoc);
          Check(RowDoc.ID = Row.ID);
          Check(_Safe(RowDoc)^.i['YearOfDeath'] = 1519);
          inc(n);
        until not res.Step;
      res.ReleaseRows;
      Check(n = n1);
      SetLength(IDs, 50);
      FillIncreasing(pointer(IDs), 50000, length(IDs));
      proxy.ThreadSafeConnection.StartTransaction;
      DoInsert;
      proxy.ThreadSafeConnection.Rollback;
      Check(DoCount = n);
      proxy.ThreadSafeConnection.StartTransaction;
      DoInsert;
      proxy.ThreadSafeConnection.Commit;
      n1 := DoCount;
      Check(n1 = n + length(IDs));
      proxy.ExecuteNoResult('delete from people where ID>=?', [50000]);
      Check(DoCount = n);
    end;

  begin
    try
      DoTests;
    finally
      if proxy <> Props then
        proxy.Free;
    end;
  end;

var
  Server: TSqlDBServerAbstract;
const
  ADDR = '127.0.0.1:' + HTTP_DEFAULTPORT;
begin
  Props := TSqlDBSQLite3ConnectionProperties.Create('test.db3', '', '', '');
  try
    DoTest(Props, 'raw Props');
    DoTest(TSqlDBRemoteConnectionPropertiesTest.Create(
      Props, 'user', 'pass', TSqlDBProxyConnectionProtocol), 'proxy test');
    DoTest(TSqlDBRemoteConnectionPropertiesTest.Create(
      Props, 'user', 'pass', TSqlDBRemoteConnectionProtocol), 'remote test');
    {$ifdef ONLYUSEHTTPSOCKET}
    Server := TSqlDBServerSockets.Create(
      Props, 'root', HTTP_DEFAULTPORT, 'user', 'pass');
    {$else}
    Server := TSqlDBServerHttpApi.Create(
      Props, 'root', HTTP_DEFAULTPORT, 'user', 'pass');
    {$endif ONLYUSEHTTPSOCKET}
    try
      DoTest(TSqlDBSocketConnectionProperties.Create(
        ADDR, 'root', 'user', 'pass'), 'socket');
      {$ifdef USEWININET}
      DoTest(TSqlDBWinHTTPConnectionProperties.Create(
        ADDR, 'root', 'user', 'pass'), 'winhttp');
      DoTest(TSqlDBWinINetConnectionProperties.Create(
        ADDR, 'root', 'user', 'pass'), 'wininet');
      {$endif USEWININET}
      {$ifdef USELIBCURL}
      DoTest(TSqlDBCurlConnectionProperties.Create(
        ADDR, 'root', 'user', 'pass'), 'libcurl');
      {$endif USELIBCURL}
    finally
      Server.Free;
    end;
  finally
    Props.Free;
  end;
end;

procedure TTestExternalDatabase.DBPropertiesPersistence;
var
  Props: TSqlDBConnectionProperties;
  json: RawUtf8;
begin
  Props := TSqlDBSQLite3ConnectionProperties.Create('server', '', '', '');
  json := Props.DefinitionToJSON(14);
  Check(json = '{"Kind":"TSqlDBSQLite3ConnectionProperties",' +
    '"ServerName":"server","DatabaseName":"","User":"","Password":""}');
  Props.Free;
  Props := TSqlDBSQLite3ConnectionProperties.Create('server', '', '', '1234');
  json := Props.DefinitionToJSON(14);
  Check(json = '{"Kind":"TSqlDBSQLite3ConnectionProperties",' +
    '"ServerName":"server","DatabaseName":"","User":"","Password":"MnVfJg=="}');
  Props.DefinitionToFile('connectionprops.json');
  Props.Free;
  Props := TSqlDBConnectionProperties.CreateFromFile('connectionprops.json');
  Check(Props.ClassType = TSqlDBSQLite3ConnectionProperties);
  Check(Props.ServerName = 'server');
  Check(Props.DatabaseName = '');
  Check(Props.UserID = '');
  Check(Props.PassWord = '1234');
  Props.Free;
  DeleteFile('connectionprops.json');
end;

procedure TTestExternalDatabase.CryptedDatabase;
var
  R, R2: TOrmPeople;
  Model: TOrmModel;
  aID: integer;
  Client, Client2: TRestClientDB;
  Res: TIDDynArray;

  procedure CheckFilledRow;
  begin
    Check(R.FillRewind);
    while R.FillOne do
      if not CheckFailed(R2.FillOne) then
      begin
        Check(R.ID <> 0);
        Check(R2.ID <> 0);
        Check(R.FirstName = R2.FirstName);
        Check(R.LastName = R2.LastName);
        Check(R.YearOfBirth = R2.YearOfBirth);
        Check(R.YearOfDeath = R2.YearOfDeath);
      end;
  end;

{$ifdef NOSQLITE3ENCRYPT}
const
  password = '';
{$else}
const
  password = 'pass';
{$endif NOSQLITE3ENCRYPT}

begin
  DeleteFile('testpass.db3');
  Model := TOrmModel.Create([TOrmPeople]);
  try
    Client := TRestClientDB.Create(Model, nil, 'test.db3', TRestServerDB, false, '');
    try
      R := TOrmPeople.Create;
      Assert(fPeopleData = nil);
      fPeopleData := Client.Client.List([TOrmPeople], '*');
      R.FillPrepare(fPeopleData);
      try
        Client2 := TRestClientDB.Create(
          Model, nil, 'testpass.db3', TRestServerDB, false, password);
        try
          Client2.Server.DB.Synchronous := smOff;
          Client2.Server.DB.LockingMode := lmExclusive;
          Client2.Server.DB.WALMode := true;
          Client2.Server.Server.CreateMissingTables;

          Check(Client2.Client.TransactionBegin(TOrmPeople));
          Check(Client2.Client.BatchStart(TOrmPeople));
          Check(Client2.Client.BatchSend(Res) = 200, 'Void batch');
          Check(Res = nil);
          Client2.Client.Commit;
          Check(Client2.Client.TransactionBegin(TOrmPeople));
          Check(Client2.Client.BatchStart(TOrmPeople));
          while R.FillOne do
          begin
            Check(R.ID <> 0);
            Check(Client2.Client.BatchAdd(R, true) >= 0);
          end;
          Check(Client2.Client.BatchSend(Res) = 200, 'INSERT batch');
          Client2.Client.Commit;
        finally
          Client2.Free;
        end;
        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3') = (password <> ''), 'encrypt1');
        // try to read then update the crypted file
        Client2 := TRestClientDB.Create(
          Model, nil, 'testpass.db3', TRestServerDB, false, password);
        try
          Client2.Server.DB.Synchronous := smOff;
          Client2.Server.DB.LockingMode := lmExclusive;

          R2 := TOrmPeople.CreateAndFillPrepare(Client2.Orm, '');
          try
            CheckFilledRow;
            R2.FirstName := 'One';
            aID := Client2.Orm.Add(R2, true);
            Check(aID <> 0);
            R2.FillPrepare(Client2.Orm, '');
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName = '');
            Check(Client2.Orm.Retrieve(aID, R2));
            Check(R2.FirstName = 'One');
          finally
            R2.Free;
          end;

        finally
          Client2.Free;
        end;

        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3') = (password <> ''), 'encrypt2');

        {$ifndef NOSQLITE3ENCRYPT}

        // now read it after uncypher
        check(ChangeSQLEncryptTablePassWord('testpass.db3', password, ''));
        Check(IsSQLite3File('testpass.db3'));
        Check(not IsSQLite3FileEncrypted('testpass.db3'), 'encrypt3');

        Client2 := TRestClientDB.Create(Model, nil, 'testpass.db3',
          TRestServerDB, false, '');
        try
          R2 := TOrmPeople.CreateAndFillPrepare(Client2.Orm, '');
          try
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName = '');
            Check(Client2.Orm.Retrieve(aID, R2));
            Check(R2.FirstName = 'One');
          finally
            R2.Free;
          end;
        finally
          Client2.Free;
        end;

        {$endif NOSQLITE3ENCRYPT}
      finally
        R.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    Model.Free;
  end;
end;

procedure TTestExternalDatabase.Test(StaticVirtualTableDirect, TrackChanges: boolean);
const
  BLOB_MAX = 1000;
var
  RInt, RInt1: TOrmPeople;
  RExt: TOrmPeopleExt;
  RBlob: TOrmOnlyBlob;
  RJoin: TOrmTestJoin;
  RHist: TOrmMyHistory;
  Tables: TRawUtf8DynArray;
  i, n, aID: integer;
  Orm: TRestOrmServer;
  ok: Boolean;
  BatchID, BatchIDUpdate, BatchIDJoined: TIDDynArray;
  ids: array[0..3] of TID;
  aExternalClient: TRestClientDB;
  fProperties: TSqlDBConnectionProperties;
  json: RawUtf8;
  Start, Updated: TTimeLog; // will work with both TModTime and TCreateTime properties

  procedure HistoryCheck(aIndex, aYOB: Integer; aEvent: TOrmHistoryEvent);
  var
    Event: TOrmHistoryEvent;
    Timestamp: TModTime;
    R: TOrmPeopleExt;
  begin
    RExt.ClearProperties;
    Check(RHist.HistoryGet(aIndex, Event, Timestamp, RExt));
    Check(Event = aEvent);
    Check(Timestamp >= Start);
    if Event = heDelete then
      exit;
    Check(RExt.ID = 400);
    Check(RExt.FirstName = 'Franz36');
    Check(RExt.YearOfBirth = aYOB);
    R := RHist.HistoryGet(aIndex) as TOrmPeopleExt;
    if CheckFailed(R <> nil) then
      exit;
    Check(R.ID = 400);
    Check(R.FirstName = 'Franz36');
    Check(R.YearOfBirth = aYOB);
    R.Free;
  end;

  procedure HistoryChecks;
  var
    i: integer;
  begin
    RHist := TOrmMyHistory.CreateHistory(aExternalClient.Orm, TOrmPeopleExt, 400);
    try
      Check(RHist.HistoryCount = 504);
      HistoryCheck(0, 1797, heAdd);
      HistoryCheck(1, 1828, heUpdate);
      HistoryCheck(2, 1515, heUpdate);
      for i := 1 to 500 do
        HistoryCheck(i + 2, i, heUpdate);
      HistoryCheck(503, 0, heDelete);
    finally
      RHist.Free;
    end;
  end;

var
  historyDB: TRestServerDB;
begin
  // run tests over an in-memory SQLite3 external database (much faster than file)
  DeleteFile('extdata.db3');
  fProperties := TSqlDBSQLite3ConnectionProperties.Create('extdata.db3', '', '', '');
  (fProperties.MainConnection as TSqlDBSQLite3Connection).Synchronous := smOff;
  (fProperties.MainConnection as TSqlDBSQLite3Connection).LockingMode := lmExclusive;
  Check(VirtualTableExternalMap(
    fExternalModel, TOrmPeopleExt, fProperties, 'PeopleExternal').
      MapField('ID', 'Key').
      MapField('YearOfDeath', 'YOD').
      MapAutoKeywordFields <> nil);
  Check(VirtualTableExternalRegister(
    fExternalModel, TOrmOnlyBlob, fProperties, 'OnlyBlobExternal'));
  Check(VirtualTableExternalRegister(
    fExternalModel, TOrmTestJoin, fProperties, 'TestJoinExternal'));
  Check(VirtualTableExternalRegister(
    fExternalModel, TOrmASource, fProperties, 'SourceExternal'));
  Check(VirtualTableExternalRegister(
    fExternalModel, TOrmADest, fProperties, 'DestExternal'));
  Check(VirtualTableExternalRegister(
    fExternalModel, TOrmADests, fProperties, 'DestsExternal'));
  DeleteFile('testExternal.db3'); // need a file for backup testing
  if TrackChanges and
     StaticVirtualTableDirect then
  begin
    DeleteFile('history.db3');
    historyDB := TRestServerDB.Create(
      TOrmModel.Create([TOrmMyHistory], 'history'), 'history.db3', false);
  end
  else
    historyDB := nil;
  aExternalClient := TRestClientDB.Create(
    fExternalModel, nil, 'testExternal.db3', TRestServerDB);
  try
    if historyDB <> nil then
    begin
      historyDB.Model.Owner := historyDB;
      historyDB.DB.Synchronous := smOff;
      historyDB.DB.LockingMode := lmExclusive;
      historyDB.Server.CreateMissingTables;
      Check((aExternalClient.Server.OrmInstance as TRestOrmServer).
        RemoteDataCreate(TOrmMyHistory, historyDB.OrmInstance) <> nil,
          'TOrmMyHistory should not be accessed from an external process');
    end;
    aExternalClient.Server.DB.Synchronous := smOff;
    aExternalClient.Server.DB.LockingMode := lmExclusive;
    aExternalClient.Server.DB.GetTableNames(Tables);
    Check(Tables = nil); // we reset the testExternal.db3 file
    Start := aExternalClient.Client.GetServerTimestamp;
    aExternalClient.Server.Server.
      SetStaticVirtualTableDirect(StaticVirtualTableDirect);
    aExternalClient.Server.Server.CreateMissingTables;
    if TrackChanges then
      aExternalClient.Server.Server.TrackChanges(
        [TOrmPeopleExt], TOrmMyHistory, 100, 10, 65536);
    Check(aExternalClient.Server.Server.
      CreateSqlMultiIndex(TOrmPeopleExt, ['FirstName', 'LastName'], false));
    InternalTestMany(self, aExternalClient.OrmInstance as TRestOrmClientUri);
    assert(fPeopleData <> nil);
    RInt := TOrmPeople.Create;
    RInt1 := TOrmPeople.Create;
    try
      RInt.FillPrepare(fPeopleData);
      Check(RInt.FillTable <> nil);
      Check(RInt.FillTable.RowCount > 0);
      Check(not aExternalClient.Orm.TableHasRows(TOrmPeopleExt));
      Check(aExternalClient.Orm.TableRowCount(TOrmPeopleExt) = 0);
      Check(not aExternalClient.Server.Orm.TableHasRows(TOrmPeopleExt));
      Check(aExternalClient.Server.Orm.TableRowCount(TOrmPeopleExt) = 0);
      RExt := TOrmPeopleExt.Create;
      try
        n := 0;
        while RInt.FillOne do
        begin
          if RInt.IDValue < 100 then // some real entries for backup testing
            aExternalClient.Orm.Add(RInt, true, true);
          RExt.Data := RInt.Data;
          RExt.FirstName := RInt.FirstName;
          RExt.LastName := RInt.LastName;
          RExt.YearOfBirth := RInt.YearOfBirth;
          RExt.YearOfDeath := RInt.YearOfDeath;
          RExt.Value := ValuesToVariantDynArray(['text', RInt.YearOfDeath]);
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          if RInt.IDValue > 100 then
          begin
            if aExternalClient.Client.BatchCount = 0 then
              aExternalClient.Client.BatchStart(TOrmPeopleExt, 5000);
            aExternalClient.Client.BatchAdd(RExt, true);
          end
          else
          begin
            aID := aExternalClient.Orm.Add(RExt, true);
            Check(aID <> 0);
            Check(RExt.LastChange >= Start);
            Check(RExt.CreatedAt >= Start);
            RExt.ClearProperties;
            Check(RExt.YearOfBirth = 0);
            Check(RExt.FirstName = '');
            Check(RExt.Value = nil);
            Check(aExternalClient.Orm.Retrieve(aID, RExt));
            Check(RExt.FirstName = RInt.FirstName);
            Check(RExt.LastName = RInt.LastName);
            Check(RExt.YearOfBirth = RInt.YearOfBirth);
            Check(RExt.YearOfDeath = RInt.YearOfDeath);
            Check(RExt.YearOfBirth <> RExt.YearOfDeath);
            json := FormatUTF8('["text",%]', [RInt.YearOfDeath]);
            Check(VariantDynArrayToJSON(RExt.Value) = json);
          end;
          inc(n);
        end;
        Check(aExternalClient.Orm.Retrieve(1, RInt1));
        Check(RInt1.IDValue = 1);
        Check(n = fPeopleData.RowCount);
        Check(aExternalClient.Client.BatchSend(BatchID) = HTTP_SUCCESS);
        Check(length(BatchID) = n - 99);
        Check(aExternalClient.Orm.TableHasRows(TOrmPeopleExt));
        Check(aExternalClient.Orm.TableMaxID(TOrmPeopleExt) = n);
        Check(aExternalClient.Orm.TableRowCount(TOrmPeopleExt) = n);
        Check(aExternalClient.Server.Orm.TableHasRows(TOrmPeopleExt));
        Check(aExternalClient.Server.Orm.TableRowCount(TOrmPeopleExt) = n);
        Check(RInt.FillRewind);
        while RInt.FillOne do
        begin
          RExt.FillPrepare(aExternalClient.Orm, 'FirstName=? and LastName=?',
            [RInt.FirstName, RInt.LastName]); // query will use index -> fast :)
          while RExt.FillOne do
          begin
            Check(RExt.FirstName = RInt.FirstName);
            Check(RExt.LastName = RInt.LastName);
            Check(RExt.YearOfBirth = RInt.YearOfBirth);
            Check(RExt.YearOfDeath = RInt.YearOfDeath);
            Check(RExt.YearOfBirth <> RExt.YearOfDeath);
            CheckEqual(VariantDynArrayToJSON(RExt.Value),
              FormatUTF8('["text",%]', [RInt.YearOfDeath]));
          end;
        end;
        Updated := aExternalClient.GetServerTimestamp;
        Check(Updated >= Start);
        for i := 1 to BatchID[high(BatchID)] do
          if i mod 100 = 0 then
          begin
            RExt.fLastChange := 0;
            RExt.CreatedAt := 0;
            RExt.Value := nil;
            Check(aExternalClient.Orm.Retrieve(i, RExt, true), 'for update');
            Check(RExt.YearOfBirth <> RExt.YearOfDeath);
            Check(RExt.CreatedAt <= Updated);
            CheckEqual(VariantDynArrayToJSON(RExt.Value),
              FormatUTF8('["text",%]', [RExt.YearOfDeath]));
            RExt.YearOfBirth := RExt.YearOfDeath; // YOB=YOD for 1/100 rows
            if i > 4000 then
            begin
              if aExternalClient.Client.BatchCount = 0 then
                aExternalClient.Client.BatchStart(TOrmPeopleExt, 10000);
              Check(aExternalClient.Client.BatchUpdate(RExt) >= 0,
                'BatchUpdate 1/100 rows');
            end
            else
            begin
              Check(aExternalClient.Client.Update(RExt), 'Update 1/100 rows');
              Check(aExternalClient.Client.UnLock(RExt));
              Check(RExt.LastChange >= Updated);
              RExt.ClearProperties;
              Check(RExt.Value = nil);
              Check(RExt.YearOfDeath = 0);
              Check(RExt.YearOfBirth = 0);
              Check(RExt.CreatedAt = 0);
              Check(aExternalClient.Client.Retrieve(i, RExt), 'after update');
              Check(RExt.YearOfBirth = RExt.YearOfDeath);
              Check(RExt.CreatedAt >= Start);
              Check(RExt.CreatedAt <= Updated);
              Check(RExt.LastChange >= Updated);
              CheckEqual(VariantDynArrayToJSON(RExt.Value),
                FormatUTF8('["text",%]', [RExt.YearOfDeath]));
            end;
          end;
        Check(aExternalClient.Client.BatchSend(BatchIDUpdate) = HTTP_SUCCESS);
        Check(length(BatchIDUpdate) = 70);
        for i := 1 to BatchID[high(BatchID)] do
          if i and 127 = 0 then
            if i > 4000 then
            begin
              if aExternalClient.Client.BatchCount = 0 then
                aExternalClient.Client.BatchStart(TOrmPeopleExt);
              Check(aExternalClient.Client.BatchDelete(i) >= 0,
                'BatchDelete 1/128 rows');
            end
            else
              Check(aExternalClient.Client.Delete(TOrmPeopleExt, i),
                'Delete 1/128 rows');
        Check(aExternalClient.Client.BatchSend(BatchIDUpdate) = HTTP_SUCCESS);
        Check(length(BatchIDUpdate) = 55);
        n := aExternalClient.Client.TableRowCount(TOrmPeople);
        Check(aExternalClient.Server.Server.
          TableRowCount(TOrmPeopleExt) = 10925);
        Orm := aExternalClient.Server.OrmInstance as TRestOrmServer;
        Check(Orm.StaticVirtualTable[TOrmPeople] = nil);
        Check(Orm.StaticVirtualTable[TOrmPeopleExt] <> nil);
        Check(Orm.StaticVirtualTable[TOrmOnlyBlob] <> nil);
        for i := 1 to BatchID[high(BatchID)] do
        begin
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          RExt.YearOfBirth := 0;
          ok := aExternalClient.Client.Retrieve(i, RExt, false);
          Check(ok = (i and 127 <> 0), 'deletion');
          if ok then
          begin
            CheckEqual(VariantDynArrayToJSON(RExt.Value),
              FormatUTF8('["text",%]', [RExt.YearOfDeath]));
            Check(RExt.CreatedAt >= Start);
            Check(RExt.CreatedAt <= Updated);
            if i mod 100 = 0 then
            begin
              Check(RExt.YearOfBirth = RExt.YearOfDeath, 'Update');
              Check(RExt.LastChange >= Updated);
            end
            else
            begin
              Check(RExt.YearOfBirth <> RExt.YearOfDeath, 'Update');
              Check(RExt.LastChange >= Start);
              Check(RExt.LastChange <= Updated);
            end;
          end;
        end;
        aExternalClient.Client.Retrieve(400, RExt);
        Check(RExt.IDValue = 400);
        Check(RExt.FirstName = 'Franz36');
        Check(RExt.YearOfBirth = 1828);
        aExternalClient.Client.UpdateField(
          TOrmPeopleExt, 400, 'YearOfBirth', [1515]);
        RInt1.ClearProperties;
        Check(aExternalClient.Client.Retrieve(1, RInt1));
        Check(RInt1.IDValue = 1);
        for i := 0 to high(ids) do
        begin
          RExt.YearOfBirth := i;
          ids[i] := aExternalClient.Orm.Add(RExt, true);
        end;
        for i := 0 to high(ids) do
        begin
          Check(aExternalClient.Orm.Retrieve(ids[i], RExt));
          Check(RExt.YearOfBirth = i);
        end;
        for i := 0 to high(ids) do
        begin
          aExternalClient.Client.BatchStart(TOrmPeopleExt);
          aExternalClient.Client.BatchDelete(ids[i]);
          Check(aExternalClient.Client.BatchSend(BatchID) = HTTP_SUCCESS);
          Check(length(BatchID) = 1);
          Check(BatchID[0] = HTTP_SUCCESS);
        end;
        for i := 0 to high(ids) do
          Check(not aExternalClient.Orm.Retrieve(ids[i], RExt));
        RExt.ClearProperties;
        for i := 0 to high(ids) do
        begin
          RExt.IDValue := ids[i];
          Check(aExternalClient.Orm.Update(RExt), 'test locking');
        end;
      finally
        RExt.Free;
      end;
      RJoin := TOrmTestJoin.Create;
      try
        aExternalClient.Client.BatchStart(TOrmTestJoin, 1000);
        for i := 1 to BLOB_MAX do
          if i and 127 <> 0 then
          begin
            RJoin.Name := Int32ToUTF8(i);
            RJoin.People := TOrmPeopleExt(i);
            aExternalClient.Client.BatchAdd(RJoin, true);
          end;
        Check(aExternalClient.Client.BatchSend(BatchIDJoined) = HTTP_SUCCESS);
        Check(length(BatchIDJoined) = 993);
        RJoin.FillPrepare(aExternalClient.Orm);
        Check(RJoin.FillTable.RowCount = 993);
        i := 1;
        while RJoin.FillOne do
        begin
          if i and 127 = 0 then
            inc(i); // deleted item
          Check(GetInteger(pointer(RJoin.Name)) = i);
          Check(RJoin.People.ID = i, 'retrieve ID from pointer');
          inc(i);
        end;
      finally
        RJoin.Free;
      end;
      for i := 0 to high(BatchIDJoined) do
      begin
        RJoin := TOrmTestJoin.CreateJoined(aExternalClient.Orm, BatchIDJoined[i]);
        try
          Check(RJoin.FillTable.FieldType(0) = oftInteger);
          Check(RJoin.FillTable.FieldType(3) = oftUTF8Text);
          Check(RJoin.ID = BatchIDJoined[i]);
          Check(PtrUInt(RJoin.People) > 1000);
          Check(GetInteger(pointer(RJoin.Name)) = RJoin.People.ID);
          Check(length(RJoin.People.Value) = 2);
          Check(RJoin.People.Value[0] = 'text');
          Check(RJoin.People.Value[1] = RJoin.People.YearOfDeath);
          RJoin.ClearProperties;
          Check(RJoin.ID = 0);
          Check(RJoin.People.ID = 0);
        finally
          RJoin.Free;
        end;
      end;
      Check(not aExternalClient.Server.Orm.TableHasRows(TOrmOnlyBlob));
      Check(aExternalClient.Server.Orm.TableRowCount(TOrmOnlyBlob) = 0);
      RBlob := TOrmOnlyBlob.Create;
      try
        aExternalClient.Client.ForceBlobTransfertTable[TOrmOnlyBlob] := true;
        aExternalClient.Orm.TransactionBegin(TOrmOnlyBlob);
        for i := 1 to BLOB_MAX do
        begin
          RBlob.Data := Int32ToUtf8(i);
          Check(aExternalClient.Orm.Add(RBlob, true) = i);
          Check(RBlob.ID = i);
        end;
        aExternalClient.Orm.Commit;
        for i := 1 to BLOB_MAX do
        begin
          Check(aExternalClient.Orm.Retrieve(i, RBlob));
          Check(GetInteger(pointer(RBlob.Data)) = i);
        end;
        aExternalClient.Orm.TransactionBegin(TOrmOnlyBlob);
        for i := BLOB_MAX downto 1 do
        begin
          RBlob.IDValue := i;
          RBlob.Data := Int32ToUtf8(i * 2);
          Check(aExternalClient.Orm.Update(RBlob));
        end;
        aExternalClient.Orm.Commit;
        for i := 1 to BLOB_MAX do
        begin
          Check(aExternalClient.Orm.Retrieve(i, RBlob));
          Check(GetInteger(pointer(RBlob.Data)) = i * 2);
        end;
        aExternalClient.Client.ForceBlobTransfertTable[TOrmOnlyBlob] := false;
        RBlob.ClearProperties;
        for i := 1 to BLOB_MAX do
        begin
          Check(aExternalClient.Orm.Retrieve(i, RBlob));
          Check(RBlob.Data = '');
        end;
      finally
        RBlob.Free;
      end;
      Check(aExternalClient.Orm.TableHasRows(TOrmOnlyBlob));
      Check(aExternalClient.Orm.TableRowCount(TOrmOnlyBlob) = 1000);
      Check(aExternalClient.Orm.TableRowCount(TOrmPeople) = n);
      RInt1.ClearProperties;
      Orm := aExternalClient.Server.OrmInstance as TRestOrmServer;
      Check(Orm.StaticVirtualTable[TOrmPeople] = nil);
      Check(Orm.StaticVirtualTable[TOrmPeopleExt] <> nil);
      Check(Orm.StaticVirtualTable[TOrmOnlyBlob] <> nil);
      Check(aExternalClient.Orm.TableHasRows(TOrmPeople));
      Check(aExternalClient.Orm.TableRowCount(TOrmPeople) = n);
      RInt1.ClearProperties;
      aExternalClient.Orm.Retrieve(1, RInt1);
      Check(RInt1.IDValue = 1);
      Check(RInt1.FirstName = 'Salvador1');
      Check(RInt1.YearOfBirth = 1904);
    finally
      RInt.Free;
      RInt1.Free;
    end;
    if TrackChanges then
    begin
      RExt := TOrmPeopleExt.Create;
      try
        RHist := TOrmMyHistory.CreateHistory(
          aExternalClient.Orm, TOrmPeopleExt, 400);
        try
          Check(RHist.HistoryCount = 3);
          HistoryCheck(0, 1797, heAdd);
          HistoryCheck(1, 1828, heUpdate);
          HistoryCheck(2, 1515, heUpdate);
        finally
          RHist.Free;
        end;
        for i := 1 to 500 do
        begin
          RExt.YearOfBirth := i;
          aExternalClient.Orm.Update(RExt, 'YearOfBirth');
        end;
        aExternalClient.Orm.Delete(TOrmPeopleExt, 400);
        HistoryChecks;
        aExternalClient.Server.Server.TrackChangesFlush(TOrmMyHistory);
        HistoryChecks;
      finally
        RExt.Free;
      end;
    end;
  finally
    aExternalClient.Free;
    fProperties.Free;
    historyDB.Free;
  end;
end;

end.

