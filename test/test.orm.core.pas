/// regression tests for basic ORM classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.orm.core;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  {$ifndef FPC}
  typinfo, // to avoid Delphi inlining problems
  {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics are buggy
  Generics.Collections,
  {$endif ISDELPHI2010}
  {$endif FPC}
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.crypt.core,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.log,
  mormot.core.mustache,
  mormot.core.test,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.core,
  mormot.soa.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.rest.memserver,
  test.core.data;

type
  /// common ancestor for tables with digitally signed RawUtf8 content
  // - content is signed according to a specific User Name and the digital
  // signature date and time
  // - internally uses the very secure SHA-256 hashing algorithm for performing
  // the digital signature
  TOrmSigned = class(TOrm)
  protected
    /// time and date of signature
    fSignatureTime: TTimeLog;
    /// hashed signature
    fSignature: RawUtf8;
    function ComputeSignature(const UserName, Content: RawByteString): RawUtf8;
  public
    /// time and date of signature
    // - if the signature is invalid, this field will contain numerical 1 value
    // - this property is defined here to allow inherited to just declared the name
    // in its published section:
    // ! property SignatureTime;
    property SignatureTime: TTimeLog
      read fSignatureTime write fSignatureTime;
    /// as the Content of this record is added to the database,
    // its value is hashed and stored as 'UserName/03A35C92....' into this property
    // - secured SHA-256 hashing is used internally
    // - digital signature is allowed only once: this property is written only once
    // - this property is defined here to allow inherited to just declared the name
    // in its published section:
    // ! property Signature;
    property Signature: RawUtf8
      read fSignature write fSignature;
  public
    /// use this procedure to sign the supplied Content of this record for a
    // specified UserName, with the current Date and Time
    // - SHA-256 hashing is used internally
    // - returns true if signed successfully (not already signed)
    function SetAndSignContent(const UserName: RawUtf8; const Content:
      RawByteString; ForcedSignatureTime: Int64 = 0): boolean;
    /// returns true if this record content is correct according to the
    // stored digital Signature
    function CheckSignature(const Content: RawByteString): boolean;
    /// retrieve the UserName who digitally signed this record
    // - returns '' if was not digitally signed
    function SignedBy: RawUtf8;
    /// reset the stored digital signature
    // - SetAndSignContent() can be called after this method
    procedure UnSign;
  end;

  /// base ORM class, which will have creation and modification timestamp fields
  TOrmTimed = class(TOrm)
  protected
    fCreated: TCreateTime;
    fModified: TModTime;
  published
    /// will be filled by the ORM when this item will be created in the database
    property Created: TCreateTime
      read fCreated write fCreated;
    /// will be filled by the ORM each time this item will be written in the database
    property Modified: TModTime
      read fModified write fModified;
  end;



type
  /// this test case will test some generic classes
  // defined and implemented in the mormot.orm.core.pas unit
  TTestOrmCore = class(TSynTestCase)
  published
    /// test the TOrm class
    // - especially SQL auto generation, or JSON export/import
    procedure _TOrm;
    /// test the digital signature of records
    procedure _TOrmSigned;
    /// test the TOrmModel class
    procedure _TOrmModel;
    /// test a full in-memory server over Windows Messages
    // - Under Linux, URIDll will be used instead due to lack of message loop
    // - without any SQLite3 engine linked
    procedure _TRestServerFullMemory;
  end;

implementation  


{ TTestOrmCore }

procedure TTestOrmCore._TOrmModel;
var
  M: TOrmModel;
  U: TRestServerUri;
begin
  M := TOrmModel.Create([TOrmTest]);
  try
    Check(M['Test'] <> nil);
    Check(M['Test2'] = nil);
    Check(M['TEST'] = TOrmTest);
  finally
    M.Free;
  end;
  Check(U.Uri = '');
  U.Uri := 'addr:port/root';
  Check(U.Address = 'addr');
  Check(U.Port = 'port');
  Check(U.Root = 'root');
  U.Uri := 'addr:port';
  Check(U.Address = 'addr');
  Check(U.Port = 'port');
  Check(U.Root = '');
  U.Uri := 'addr/root';
  Check(U.Address = 'addr');
  Check(U.Port = '');
  Check(U.Root = 'root');
  U.Uri := 'addr';
  Check(U.Address = 'addr');
  Check(U.Port = '');
  Check(U.Root = '');
end;

procedure TTestOrmCore._TRestServerFullMemory;
var
  Model: TOrmModel;
  Server: TRestServerFullMemory;
  {$ifdef MSWINDOWS2}
  Client: TRestClientUriMessage;
  {$else}
  // Under Linux, no windows message loop : URIDll will be used
  Client: TRestClientUri;
  {$endif MSWINDOWS2}
  R: TOrmTest;
  Batch: TRestBatch;
  IDs: TIDDynArray;
  i, j, n: integer;
  dummy, s: RawUtf8;

  procedure CheckVariantWith(const V: Variant; const i: Integer; const offset:
    integer = 0);
  begin
    Check(V.ID = i);
    Check(V.Int = i);
    Check(V.Test = Int32ToUtf8(i));
    Check(V.Ansi = V.Test);
    Check(V.Unicode = V.Test);
    Check(V.ValFloat = i * 2.5);
    Check(V.ValWord = i + offset);
    Check(V.ValDate = i + 30000);
    Check(V.Data = V.Test);
    Check(DocVariantType.IsOfType(V.ValVariant));
    Check(VariantSaveJson(V.ValVariant) = '{"id":' + V.Test + '}');
  end;

var
  readonly: boolean;
  docs: variant;
  T: TOrmTable;
{$ifdef ISDELPHI2010}
var
  List: TObjectList<TOrmTest>;
{$endif ISDELPHI2010}
begin
  Model := TOrmModel.Create([TOrmTest]);
  try
    DeleteFile(WorkDir + 'fullmem.data');
    Check(not FileExists(WorkDir + 'fullmem.data'));
    Server := TRestServerFullMemory.Create(Model, WorkDir + 'fullmem.data', true, true);
    try
      Server.{$ifdef PUREMORMOT2}Server.{$endif}CreateMissingTables;
      {$ifdef MSWINDOWS2}
      Check(Server.ExportServerMessage('fullmem'));
      Client := TRestClientUriMessage.Create(Model, 'fullmem', 'fullmemclient', 1000);
      {$else}
      Server.ExportServerGlobalLibraryRequest;
      Client := TRestClientLibraryRequest.Create(Model, LibraryRequest);
      {$endif MSWINDOWS2}
      try
        Client.Client.ForceBlobTransfert := true;
        Check(Client.ServerTimestampSynchronize);
        Check(Client.SetUser('User', 'synopse'));
        Client.Orm.TransactionBegin(TOrmTest);
        R := TOrmTest.Create;
        try
          for i := 1 to 99 do
          begin
            R.FillWith(i);
            Check(Client.Orm.Add(R, true) = i);
          end;
          Client.Orm.Commit;
          Check(Client.Client.BatchStart(TOrmTest, 1000));
          for i := 100 to 9999 do
          begin
            R.FillWith(i);
            Check(Client.Client.BatchAdd(R, true, false, ALL_FIELDS) = i - 100);
          end;
          CheckEqual(Client.Client.BatchSend(IDs), HTTP_SUCCESS);
          Check(Length(IDs) = 9900);
          Check(not FileExists(WorkDir + 'fullmem.data'));
          Check(Client.CallBackPut('Flush', '', dummy) = HTTP_SUCCESS);
          Check(FileExists(WorkDir + 'fullmem.data'));
          Check(Client.Orm.Retrieve(200, R));
          R.CheckWith(self, 200);
        finally
          R.Free;
        end;
      finally
        Client.Free;
      end;
    finally
      Server.Free;
    end;
    Server := TRestServerFullMemory.Create(Model, WorkDir + 'fullmem.data', true, true);
    try
      Server.Server.CreateMissingTables;
      {$ifdef MSWINDOWS2}
      Check(Server.ExportServerMessage('fullmem'));
      Client := TRestClientUriMessage.Create(Model, 'fullmem', 'fullmemclient', 1000);
      {$else}
      Server.ExportServerGlobalLibraryRequest;
      Client := TRestClientLibraryRequest.Create(Model, LibraryRequest);
      {$endif MSWINDOWS2}
      try
        Client.Client.ForceBlobTransfert := true;
        Check(Client.ServerTimestampSynchronize);
        Check(Client.SetUser('User', 'synopse'));
        R := TOrmTest.CreateAndFillPrepare(Client.Orm, '', '*');
        try
          Check((R.FillTable <> nil) and (R.FillTable.RowCount = 9999));
          i := 0;
          while R.FillOne do
          begin
            inc(i);
            R.CheckWith(self, i);
          end;
          Check(i = 9999);
          for i := 1 to 9999 do
          begin
            Check(R.FillRow(i));
            R.CheckWith(self, i);
          end;
          for i := 1 to 20000 do
          begin
            j := Random32(9999) + 1;
            Check(R.FillRow(j));
            R.CheckWith(self, j);
          end;
        finally
          R.Free;
        end;
        {$ifdef ISDELPHI2010}
        List := Client.Orm.Generics.RetrieveList<TOrmTest>('*');
        if not CheckFailed(List <> nil) then
        try
          Check(List.Count = 9999);
          for R in List do
            R.CheckWith(self, R.IDValue);
          for i := 0 to List.Count - 1 do
          begin
            R := List[i];
            R.CheckWith(self, i + 1);
          end;
        finally
          List.Free;
        end;
        {$endif ISDELPHI2010}
        for readonly := false to true do
        begin
          T := Client.Orm.MultiFieldValues(TOrmTest, '*');
          if CheckFailed(T <> nil) then
            Continue;
          Check(T.RowCount = 9999);
          T.ToDocVariant(docs, readonly);
          with DocVariantData(docs)^ do
            for j := 0 to Count - 1 do
              CheckVariantWith(Values[j], j + 1);
          T.Free;
        end;
        dummy := TSynMustache.Parse('{{#items}}'#13#10'{{Int}}={{Test}}'#13#10'{{/items}}').
          Render(Client.Orm.RetrieveDocVariantArray(TOrmTest, 'items', 'Int,Test'));
        check(IdemPChar(pointer(dummy), '1=1'#$D#$A'2=2'#$D#$A'3=3'#$D#$A'4=4'));
        CheckHash(dummy, $BC89CA72);
        Check(Client.Orm.UpdateField(TOrmTest, 100, 'ValWord', [100 + 10]),
          'update one field of a given record');
        R := TOrmTest.Create(Client.Orm, 100);
        try
          R.CheckWith(self, 100, 10);
        finally
          R.Free;
        end;
        s := Client.Orm.OneFieldValues(TOrmTest, 'Test', 'ValWord=:(110):');
        Check(s = '100,110');
        Check(Client.Orm.UpdateField(TOrmTest, 100, 'ValWord', [100]));
        R := TOrmTest.Create(Client.Orm, 100);
        try
          R.CheckWith(self, 100);
        finally
          R.Free;
        end;
        s := Client.Orm.OneFieldValues(TOrmTest, 'Test',
          FormatUtf8('ValWord=?', [], [110]));
        Check(s = '110');
        Check(Client.Orm.UpdateField(TOrmTest, 'Unicode', ['110'], 'ValWord', [120]),
          'update one field of a given record');
        R := TOrmTest.Create(Client.Orm, 110);
        try
          R.CheckWith(self, 110, 10);
          Batch := TRestBatch.Create(Server.Orm, TOrmTest, 30);
          try
            for i := 10000 to 10099 do
            begin
              R.FillWith(i);
              Check(Batch.Add(R, true, false, ALL_FIELDS) = i - 10000);
            end;
            Check(Server.Orm.BatchSend(Batch, IDs) = HTTP_SUCCESS);
          finally
            Batch.Free;
          end;
        finally
          R.Free;
        end;
        Check(Length(IDs) = 100);
        R := TOrmTest.CreateAndFillPrepare(Server.Orm, '', '*');
        try
          i := 0;
          while R.FillOne do
          begin
            inc(i);
            if i = 110 then
              R.CheckWith(self, i, 10)
            else
              R.CheckWith(self, i);
            if (i = 200) or (i = 300) then
            begin
              R.FillWith(R.ID + 10);
              Check(Client.Orm.Update(R, 'ValWord,ValDate'), 'update only 2 fields');
            end;
          end;
          Check(i = 10099);
        finally
          R.Free;
        end;
        // note: SELECT .. IN ... is implemented via a TDocVariant
        R := TOrmTest.CreateAndFillPrepare(Client.Orm, [200, 300], 'ValWord,ValDate,ID');
        try
          i := 0;
          while R.FillOne do
          begin
            inc(i);
            Check(R.ID >= 200);
            R.FillWith(R.ID + 10);
            Check(Client.Orm.Update(R, 'ValWord,ValDate'), 'update only 2 fields');
          end;
          Check(i = 2);
        finally
          R.Free;
        end;
        n := 20000;
        R := TOrmTest.create;
        try
          for i := 10100 to n do
          begin
            R.FillWith(i);
            Check(Server.Orm.AddWithBlobs(R, false) = i);
          end;
        finally
          R.Free;
        end;
        CheckEqual(Server.Orm.TableRowCount(TOrmTest), n);
        for i := 1 to n do
          if i and 511 = 0 then
          begin
            Check(Client.Orm.Delete(TOrmTest, i));
            dec(n);
          end;
        CheckEqual(Client.Orm.TableRowCount(TOrmTest), n);
        for i := 1 to n do
          Check(Server.Orm.MemberExists(TOrmTest, i) = (i and 511 <> 0));
        R := TOrmTest.CreateAndFillPrepare(Server.Orm, '', '*');
        try
          i := 0;
          while R.FillOne do
          begin
            inc(i);
            if i and 511 = 0 then
              inc(i);
            if i = 110 then
              R.CheckWith(self, i, 10)
            else if (i = 200) or (i = 300) then
            begin
              Check(R.Int = i);
              Check(R.Test = Int32ToUtf8(i));
              Check(R.ValFloat = i * 2.5);
              Check(R.ValWord = i + 10);
              Check(R.ValDate = i + 30010);
            end
            else
              R.CheckWith(self, i);
          end;
          Check(i = 20000);
        finally
          R.Free;
        end;
      finally
        Client.Free;
      end;
    finally
      Server.Free;
    end;
  finally
    Model.Free;
  end;
end;

procedure TTestOrmCore._TOrm;
var
  i: integer;
  P: PRttiProp;
  s, s1, s2: RawUtf8;
  wa: WinAnsiString;
  M: TOrmModel;
  T, T2: TOrmTest;
  s3: RawUtf8;
  bin: RawByteString;
  valid: boolean;
  obj, v: Variant;
begin
  Check(IsSelect('select * from toto'));
  Check(IsSelect(' select * from toto'));
  Check(IsSelect('vacuum'));
  Check(IsSelect(' vacuum'));
  Check(IsSelect('pragma'));
  Check(IsSelect(' pragma'));
  Check(IsSelect('with recursive cnt(x) as (values(1) union all ' +
    'select x+1 from cnt where x<1000000) select x from cnt'));
  Check(not IsSelect('update toto'));
  Check(not IsSelect(' update toto'));
  Check(not IsSelect('insert into toto'));
  Check(not IsSelect(' insert into toto'));
  Check(not IsSelect('delete from toto'));
  Check(not IsSelect(' delete from toto'));
  Check(not IsSelect('with recursive cnt(x) as (values(1) union all ' +
    'select x+1 from cnt where x<1000000) insert into toto select x from cnt'));
  Check(GetTableNameFromSqlSelect('select a,b  from  titi', false) = 'titi');
  Check(GetTableNameFromSqlSelect('select a,b  from  titi limit 10', false) = 'titi');
  Check(GetTableNameFromSqlSelect('select a,b  from  titi,tutu', false) = 'titi');
  Check(GetTableNameFromSqlSelect('select a,b  from  titi,tutu order by a',
    false) = 'titi');
  Check(GetTableNameFromSqlSelect('select a,b  from  titi,tutu', true) = '');
  Check(RawUtf8ArrayToCsv(GetTableNamesFromSqlSelect(
    'select a,b  from  titi where id=2')) = 'titi');
  Check(RawUtf8ArrayToCsv(GetTableNamesFromSqlSelect(
    'select a,b  from  titi,tutu')) = 'titi,tutu');
  Check(RawUtf8ArrayToCsv(GetTableNamesFromSqlSelect(
    'select a,b  from  titi, tutu ,  tata where a=2')) = 'titi,tutu,tata');
  T := TOrmTest.Create;
  M := TOrmModel.Create([TOrmTest]);
  for i := 0 to GetRttiProp(TOrmTest, P) - 1 do
  begin
    Check(TOrmTest.OrmProps.Fields.IndexByName(LowerCase(P^.NameUtf8)) = i);
    Check(T.OrmProps.Fields.ByRawUtf8Name(P^.NameUtf8) <> nil);
    P := P^.Next;
  end;
  Check(TOrmTest.OrmProps.Fields.IndexByName('') < 0);
  Check(TOrmTest.OrmProps.Fields.IndexByName('none') < 0);
  Check(TOrmTest.OrmProps.Fields.IndexByName('nex') < 0);
  Check(TOrmTest.OrmProps.Fields.IndexByName('next') >= 0);
  Check(TOrmTest.OrmProps.Fields.IndexByName('nexte') < 0);
  s := TOrmTest.GetSqlCreate(M);
  CheckEqual(s,
    'CREATE TABLE Test(ID INTEGER PRIMARY KEY AUTOINCREMENT, Int INTEGER, ' +
    'Test TEXT COLLATE SYSTEMNOCASE, Unicode TEXT COLLATE SYSTEMNOCASE, ' +
    'Ansi TEXT COLLATE NOCASE, ValFloat FLOAT, ValWord INTEGER, ' +
    'ValDate TEXT COLLATE ISO8601, Next INTEGER, Data BLOB, ' +
    'ValVariant TEXT COLLATE BINARY);');
  s := TOrmTest.OrmProps.SqlAddField(0);
  CheckEqual(s, 'ALTER TABLE Test ADD COLUMN Int INTEGER; ');
  s := TOrmTest.OrmProps.SqlAddField(1000);
  CheckEqual(s, '');
  T2 := TOrmTest.Create;
  try
    Check(T.OrmProps = T.Orm);
    Check(T.Orm.SqlTableName = 'Test');
    Check(T.SqlTableName = 'Test');
    Check(GetCaptionFromClass(T.RecordClass) = 'Test');
    s := T.GetSqlSet;
    CheckEqual(s,
      'Int=0, Test='''', Unicode='''', Ansi='''', ValFloat=0, ValWord=0, ' +
      'ValDate='''', Next=0, ValVariant=null');
    s := T.GetSqlValues;
    CheckEqual(s,
      'Int,Test,Unicode,Ansi,ValFloat,ValWord,ValDate,Next,ValVariant ' +
      'VALUES (0,'''','''','''',0,0,'''',0,null)');
    s := ObjectToJson(T);
    CheckEqual(s,
      '{"RowID":0,"Int":0,"Test":"","Unicode":"","Ansi":"","ValFloat":0,' +
      '"ValWord":0,"ValDate":"","Next":0,"Data":null,"ValVariant":null}');
    T.ValDate := 39882.888612; // a fixed date and time
    wa := 'abcde6ef90';
    wa[6] := #$E9; // put some accents to validate UTF-8 encoding
    wa[9] := #$E0;
    wa[10] := #$E9;
    T.Ansi := wa;
    T.Test := WinAnsiToUtf8(T.Ansi);
    T.Unicode := Utf8DecodeToRawUnicode(T.Test);
    Check(RawUnicodeToWinAnsi(T.Unicode) = T.Ansi);
    // the same string is stored with some Delphi types, but will remain
    // identical in UTF-8 SQL, as all will be converted into UTF-8
    T.Valfloat := 3.141592653;
    T.ValWord := 1203;
    T.ValVariant := 3.1416; // will be stored as TEXT, i.e. '3.1416'
    s := T.GetSqlSet;
    CheckEqual(s, 'Int=0, Test=''' + T.Test + ''', Unicode=''' + T.Test +
      ''', Ansi=''' + T.Test + ''', ValFloat=3.141592653, ValWord=1203, ' +
      'ValDate=''2009-03-10T21:19:36'', Next=0, ValVariant=''3.1416''');
    s := T.GetSqlValues;
    CheckEqual(s,
      'Int,Test,Unicode,Ansi,ValFloat,ValWord,ValDate,Next,ValVariant VALUES (0,''' +
      T.Test + ''',''' + T.Test + ''',''' + T.Test +
      ''',3.141592653,1203,''2009-03-10T21:19:36'',0,''3.1416'')');
    s := T.GetJsonValues(false, true, ooSelect);
    s1 := '{"fieldCount":10' +
      ',"values":["RowID","Int","Test","Unicode","Ansi",' +
      '"ValFloat","ValWord","ValDate","Next","ValVariant",0,0,"' +
      T.Test + '","' + T.Test + '","' + T.Test +
      '",3.141592653,1203,"2009-03-10T21:19:36",0,3.1416]}';
    CheckEqual(s, s1);
    Check(T.SameValues(T));
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJsonValues(false, true, ooSelect) = s);
    T.IDValue := 10;
    s := T.GetJsonValues(true, true, ooSelect);
    {$ifdef VERBOSE}    writeln(s); {$endif}
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJsonValues(true, true, ooSelect) = s);
    obj := T.GetSimpleFieldsAsDocVariant;
    s3 := VariantSaveJson(obj);
    Check(s3 = s);
    s := ObjectToJson(T);
    CheckEqual(s, '{"RowID":10,"Int":0,"Test":"' + T.Test + '","Unicode":"' +
      T.Test + '","Ansi":"' + T.Test + '","ValFloat":3.141592653,"ValWord":1203,' +
      '"ValDate":"2009-03-10T21:19:36","Next":0,"Data":null,"ValVariant":3.1416}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    Check(JsonToObject(T2, pointer(s), valid) <> nil);
    Check(valid);
    Check(T.SameValues(T2));
    T.Int := 1234567890123456;
    s := T.GetJsonValues(true, true, ooSelect);
    CheckEqual(s, '{"RowID":10,"Int":1234567890123456,"Test":"' + T.Test +
      '","Unicode":"' + T.Test + '","Ansi":"' + T.Test +
      '","ValFloat":3.141592653,"ValWord":1203,' +
      '"ValDate":"2009-03-10T21:19:36","Next":0,"ValVariant":3.1416}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJsonValues(true, true, ooSelect) = s);
    Check(T2.Int = 1234567890123456);
    T.ValVariant := Utf8ToSynUnicode(T.Test);
    s := T.GetJsonValues(true, true, ooSelect);
    s1 := '{"RowID":10,"Int":1234567890123456,"Test":"' + T.Test +
      '","Unicode":"' + T.Test + '","Ansi":"' + T.Test +
      '","ValFloat":3.141592653,"ValWord":1203,' +
      '"ValDate":"2009-03-10T21:19:36","Next":0';
    CheckEqual(s, s1 + ',"ValVariant":"' + T.Test + '"}');
    s := T.GetSqlSet;
    s2 := 'Int=1234567890123456, Test=''' + T.Test + ''', Unicode=''' + T.Test +
      ''', Ansi=''' + T.Test + ''', ValFloat=3.141592653, ValWord=1203, ' +
      'ValDate=''2009-03-10T21:19:36'', Next=0';
    Check(s = s2 + ', ValVariant=''' + T.Test + '''');
    T.ValVariant := _Json('{name:"John",int:1234}');
    s := T.GetSqlSet;
    Check(s = s2 + ', ValVariant=''{"name":"John","int":1234}''',
      'JSON object as text');
    s := T.GetJsonValues(true, true, ooSelect);
    Check(s = s1 + ',"ValVariant":{"name":"John","int":1234}}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    s := VariantSaveMongoJson(T2.ValVariant, modMongoStrict);
    CheckEqual(s, VariantSaveMongoJson(T.ValVariant, modMongoStrict));
    Check(T.SameValues(T2));
    s := T.GetJsonValues(true, true, ooSelect);
    Check(T2.GetJsonValues(true, true, ooSelect) = s);
    s := GetJsonObjectAsSql(s, true, false, 0, true);
    CheckEqual(s, StringReplaceAll(s2, ', ', ',') +
      ',ValVariant=''{"name":"John","int":1234}''');
    s := ObjectToJson(T);
    s := StringReplaceAll(s, 'null', '0');
    CheckEqual(s, s1 + ',"Data":0,"ValVariant":{"name":"John","int":1234}}');
    bin := T.GetBinary;
    T2.ClearProperties;
    T2.SetBinary(bin);
    Check(T.SameValues(T2));
    bin := VariantSave(T.ValVariant);
    Check(bin <> '');
    Check(VariantLoad(v, pointer(bin), @JSON_OPTIONS[true]) <> nil);
    CheckEqual(VariantSaveMongoJson(v, modMongoStrict), '{"name":"John","int":1234}');
  finally
    M.Free;
    T2.Free;
    T.Free;
  end;
end;


{ TOrmSigned }

function TOrmSigned.ComputeSignature(const UserName, Content: RawByteString): RawUtf8;
var
  SHA: TSha256;
  Digest: TSha256Digest;
begin
  SHA.Init;
  SHA.Update(TTimeLogBits(fSignatureTime).Text(false));
  SHA.Update(ToText(ClassType));
  SHA.Update(UserName);
  SHA.Update(Content);
  SHA.Final(Digest);
  result := Sha256DigestToString(Digest);
end;

function TOrmSigned.CheckSignature(const Content: RawByteString): boolean;
var
  i: integer;
  sign: RawUtf8;
begin
  result := false;
  if self = nil then
    exit;
  i := PosExChar('/', fSignature);
  if i = 0 then
    exit;
  sign := ComputeSignature(copy(fSignature, 1, i - 1), Content);
  if IdemPropNameU(sign, copy(fSignature, i + 1, SizeOf(TSha256Digest) * 2)) then
    result := true;
end;

function TOrmSigned.SetAndSignContent(const UserName: RawUtf8;
  const Content: RawByteString; ForcedSignatureTime: Int64): boolean;
begin
  result := (fSignature = '') and
            (fSignatureTime = 0);
  if not result then
    exit; // sign is allowed only once
  if ForcedSignatureTime <> 0 then
    fSignatureTime := ForcedSignatureTime
  else
    fSignatureTime := TimeLogNow;
  fSignature := UserName + '/' + ComputeSignature(UserName, Content);
end;

function TOrmSigned.SignedBy: RawUtf8;
var
  i: integer;
begin
  if self = nil then
    i := 0
  else
    i := PosExChar('/', fSignature);
  if i = 0 then
    result := ''
  else
    result := copy(fSignature, 1, i - 1);
end;

procedure TOrmSigned.UnSign;
begin
  fSignature := '';
  fSignatureTime := 0;
end;

procedure TTestOrmCore._TOrmSigned;
var
  R: TOrmSigned;
  i: integer;
  Content: RawByteString;
begin
  R := TOrmSigned.Create;
  try
    for i := 1 to 50 do
    begin
      Content := RandomString(5 * Random32(1000));
      Check(R.SetAndSignContent('User', Content));
      Check(R.SignedBy = 'User');
      Check(R.CheckSignature(Content));
      Content := Content + '?'; // invalidate content
      Check(not R.CheckSignature(Content));
      R.UnSign;
    end;
  finally
    R.Free;
  end;
end;

end.

