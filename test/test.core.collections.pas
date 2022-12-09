/// regression tests for most mormot.core.collections unit
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.collections;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.data,
  mormot.core.collections,
  mormot.core.perf,
  mormot.core.test;

{$ifdef HASGENERICS} // do-nothing unit on oldest compilers (e.g. < Delphi XE8)

type
  /// regression tests for mormot.core.collections features
  TTestCoreCollections = class(TSynTestCase)
  protected
    procedure TestOne<T>(const li: IList<T>);
  published
    procedure _IList;
    procedure _IKeyValue;
  end;

implementation


{ TTestCoreCollections }

{$I-}

{$define TESTHASH128}

{$ifndef TESTHASH128}
  {$define USEEQUALOP}
{$endif TESTHASH128}

{$ifndef FPC}
  // Delphi is sometimes just buggy: equal operator is not allowed in TestOne<T>
  {$undef USEEQUALOP}
{$endif FPC}

procedure TTestCoreCollections.TestOne<T>(const li: IList<T>);
const
  MAX = 100000;
  ONLYLOG = true; // set to FALSE for verbose benchmarking console output
var
  cop: TArray<T>;
  i, j: T;
  n: integer;
  v: PVariantArray;
  p: ^T;
  {$ifndef USEEQUALOP}
  p0, p1: ^T;
  {$endif USEEQUALOP}
  da: PDynArray;
  name: RawUtf8;
  timer, all: TPrecisionTimer;
begin
  SetLength(cop, MAX);
  da := li.Data;
  name := da^.Info.ArrayRtti.Name;
  Check(name <> '');
  for i in li do
  begin
   TestFailed('void list');
    {%H-}cop[0] := i;  // the compiler needs to use i somewhere
  end;
  timer.Start;
  for n := 0 to MAX - 1 do
    da^.ItemRandom(@cop[n]);
  NotifyTestSpeed('random % ', [name], MAX, 0, @timer, ONLYLOG);
  all.Start;
  li.Capacity := MAX;
  timer.Start;
  for n := 0 to MAX - 1 do
    CheckEqual(li.Add(cop[n]), n);
  NotifyTestSpeed('add %    ',  [name], MAX, 0, @timer, ONLYLOG);
  CheckEqual(li.Count, MAX);
  //timer.Start;
  li.Clear;
  //NotifyTestSpeed('clear %    ',  [name], MAX, 0, @timer, ONLYLOG);
  li.Count := MAX; // pre allocate + set length
  timer.Start;
  for n := 0 to MAX - 1 do // count + SetItem (faster)
    li.Items[n] := cop[n];
  NotifyTestSpeed('add items %',  [name], MAX, 0, @timer, ONLYLOG);
  li.Clear;
  li.Count := MAX; // pre allocate + set length
  timer.Start;
  p := li.First;   // use direct pointer assignment (fastest)
  for n := 0 to MAX - 1 do
  begin
    p^ := cop[n];
    inc(p);
  end;
  NotifyTestSpeed('add ptr %',  [name], MAX, 0, @timer, ONLYLOG);
  CheckEqual(li.Count, MAX);
  timer.Start;
  for n := 0 to MAX - 1 do
  begin
    {$ifdef USEEQUALOP}
    Check(li[n] = cop[n]);
    {$else}
    i := li[n];
    CheckEqual(da.ItemCompare(@i, @cop[n]), 0, 'getitem');
    {$endif USEEQUALOP}
  end;
  NotifyTestSpeed('getitem %', [name], MAX, 0, @timer, ONLYLOG);
  timer.Start;
  n := 0;
  for i in li do
  begin
    {$ifdef USEEQUALOP}
    Check(i = cop[n]);
    {$else}
    CheckEqual(da.ItemCompare(@i, @cop[n]), 0, 'in');
    {$endif USEEQUALOP}
    inc(n);
  end;
  NotifyTestSpeed('in %     ', [name], MAX, 0, @timer, ONLYLOG);
  CheckEqual(n, MAX);
  NotifyTestSpeed(' IList<%>', [name], MAX * 5, 0, @all, {onlylog=}false);
  timer.Start; // Sort is excluded of main "all" timer since is misleading
  li.Sort;
  NotifyTestSpeed('sort %   ', [name], MAX, 0, @timer, ONLYLOG);
  if TypeInfo(T) = TypeInfo(variant) then
  begin
    v := li.First;
    for n := 0 to MAX - 2 do
      Check(VariantCompare(v[n], v[n+1]) <= 0);
    exit; // Sort+Find do not match with variants due to duplicates
  end;
  {$ifdef USEEQUALOP}
  for n := 0 to MAX - 2 do
    Check(li[n] <= li[n+1]); // internal error on Delphi :(
  {$else}
  p1 := li.First;
  p0 := p1;
  inc(p1);
  for n := 0 to MAX - 2 do
  begin
    Check(da.ItemCompare(p0, p1) <= 0); // manual validate sort
    inc(p0);
    inc(p1);
  end;
  {$endif USEEQUALOP}
  timer.Start;
  for n := 0 to MAX - 1 do
  {$ifdef USEEQUALOP}
    Check(li[li.Find(cop[n])] = cop[n], 'sorted find');
  {$else}
  begin
    i := li[li.Find(cop[n])];
    CheckEqual(da.ItemCompare(@i, @cop[n]), 0, 'sorted find');
  end;
  {$endif USEEQUALOP}
  NotifyTestSpeed('find %   ', [name], MAX, 0, @timer, ONLYLOG);
  CheckEqual(li.Count, MAX);
  li.Clear;
  CheckEqual(li.Count, 0);
end;

type
  TClassWithoutRtti = class
  private
    fSoup: RawUtf8;
  public
    property Soup: RawUtf8
      read fSoup write fSoup;
  end;

procedure TTestCoreCollections._IList;
const
  MAX = 1000; // not too big since here we validate also brute force searches
var
  i: integer;
  pi: PInteger;
  li: IList<integer>;
  d: double;
  pd: PDouble;
  ld: IList<double>;
  o: TObjectWithID;
  lo: IList<TObjectWithID>;
  s: TClassWithoutRtti;
  ls: IList<TClassWithoutRtti>;
  u: RawUtf8;
  pu: PRawUtf8;
  lu: IList<RawUtf8>;
  lh: IList<THash128>;
  h: THash128;
  r: TSynTestFailed;
  lr: IList<TSynTestFailed>;
begin
  // manual IList<integer> validation
  li := Collections.NewList<integer>;
  for i in li do
    Check(i < 0);
  li.Capacity := MAX + 1;       // faster Add() thanks to pre-allocation
  CheckEqual(li.Count, 0);
  for i in li do
    Check(i < 0);
  for i in li.Range(-5) do
    Check(i < 0);
  for i := 0 to MAX do          // populate with some data
    CheckEqual(li.Add(i), i);
  for i := 0 to li.Count - 1 do // regular Items[] access
    CheckEqual(li[i], i);
  for i in li do                // use an enumerator - safe and clean
    Check(cardinal(i) <= MAX);
  for i in li.Range(-5) do      // use an enumerator for the last 5 items
    Check(i > MAX - 5);
  for i in li do
    CheckEqual(li.IndexOf(i), i);   // O(n) brute force search
  for i in li do
    CheckEqual(li.Find(i), i);      // O(n) brute force search
  pi := li.First;
  for i := 0 to li.Count - 1 do // fastest method
  begin
    CheckEqual(pi^, i);
    inc(pi);
  end;
  li := Collections.NewList<integer>([loCreateUniqueIndex]);
  li.Capacity := MAX + 1;
  for i := 0 to MAX do          // populate with some data
    CheckEqual(li.Add(i), i);
  for i in li do                // use an enumerator - safe and clean
    Check(cardinal(i) <= MAX);
  for i in li do
    CheckEqual(li.IndexOf(i), i);   // O(n) brute force search
  for i in li do
    CheckEqual(li.Find(i), i);      // O(1) hash table search
  // manual IList<double> validation
  ld := Collections.NewList<double>;
  for d in ld do
    Check(d < 0);
  ld.Capacity := MAX + 1;       // faster Add() thanks to pre-allocation
  CheckEqual(ld.Count, 0);
  for d in ld do
    Check(d < 0);
  for d in ld.Range(-5) do
    Check(d < 0);
  for i := 0 to MAX do          // populate with some data
    CheckEqual(ld.Add(i), i);
  for i := 0 to ld.Count - 1 do // regular Items[] access
    Check(ld[i] = i);
  for d in ld do                // use an enumerator - safe and clean
    Check((d >= 0) and (d <= MAX));
  for d in ld.Range(-5) do      // use an enumerator for the last 5 items
    Check(d > MAX - 5);
  for d in ld do
    Check(ld.IndexOf(d) = trunc(d));   // O(n) brute force search
  for d in ld do
    Check(ld.Find(d) = trunc(d));      // O(n) brute force search
  pd := ld.First;
  for i := 0 to ld.Count - 1 do // fastest method
  begin
    Check(pd^ = i);
    inc(pd);
  end;
  ld := Collections.NewList<double>([loCreateUniqueIndex]);
  ld.Capacity := MAX + 1;
  for i := 0 to MAX do          // populate with some data
    CheckEqual(ld.Add(i), i);
  for d in ld do                // use an enumerator - safe and clean
    Check(d <= MAX);
  for d in ld do
    CheckEqual(ld.IndexOf(d), trunc(d));   // O(n) brute force search
  for d in ld do
    CheckEqual(ld.Find(d), trunc(d));      // O(1) hash table search
  // manual IList<TObjectWithID> validation
  lo := Collections.NewList<TObjectWithID>;
  lo.Capacity := MAX + 1;
  for i := 0 to MAX do
    Check(lo.Add(TObjectWithID.CreateWithID(i)) = i);
  CheckEqual(lo.Count, MAX + 1);
  for i := 0 to lo.Count - 1 do
    CheckEqual(lo[i].IDValue, i);
  for o in lo do
    Check(o.IDValue <= MAX);
  for o in lo.Range(-5) do
    Check(o.IDValue > MAX - 5);
  i := 0;
  for o in lo do
  begin
    CheckEqual(lo.Find(o), i); // O(n) lookup by address
    inc(i);
  end;
  o := nil; // iterative Pop(o) will release the previous o<>nil instances
  for i := 0 to MAX do
    Check(lo.Pop(o, [popFromHead]));
  Check(not lo.Pop(o, [popFromHead]));
  o.Free; // but we still need to release the last o instance
  CheckEqual(lo.Count, 0);
  lo := Collections.NewList<TObjectWithID>([loCreateUniqueIndex]);
  for i := 0 to MAX do
    Check(lo.Add(TObjectWithID.CreateWithID(i)) = i);
  CheckEqual(lo.Count, MAX + 1);
  for i := 0 to MAX do
    CheckEqual(lo.Add(lo[i]), i); // duplicate found by loCreateUniqueIndex
  for i := 0 to MAX do
    CheckEqual(lo.Find(lo[i]), i); // O(1) lookup via loCreateUniqueIndex
  for i := 0 to lo.Count - 1 do
    CheckEqual(lo[i].IDValue, i);
  for o in lo do
    Check(o.IDValue <= MAX);
  for o in lo.Range(-5) do
    Check(o.IDValue > MAX - 5);
  lo := Collections.NewList<TObjectWithID>([loCreateUniqueIndex]);
  i := lo.Add(TObjectWithID.CreateWithID(100));
  Check(i = 0);
  CheckEqual(lo[i].IDValue, 100);
  // manual IList<TClassWithoutRtti> validation
  ls := Collections.NewList<TClassWithoutRtti>;
  ls.Capacity := MAX + 1;
  for i := 0 to MAX do
  begin
    s := TClassWithoutRtti.Create;
    s.Soup := UInt32ToUtf8(i);
    CheckEqual(ls.Add(s), i);
    Check(ls[i] <> nil);
   end;
   for i := 0 to ls.Count - 1 do
     Check(Utf8ToInteger(ls[i].Soup) = i);
  // manual IList<RawUtf8> validation
  lu := Collections.NewList<RawUtf8>;
  for u in lu do
    Check(u = #0);
  lu.Capacity := MAX + 1;
  Check(lu.Count = 0);
  for u in lu do
    Check(u = #0);
  for u in lu.Range(-5) do
    Check(u = #0);
  for i := 0 to MAX do          // populate with some data
    Check(lu.Add(UInt32ToUtf8(i)) = i);
  for i := 0 to lu.Count - 1 do // regular Items[] access
    Check(Utf8ToInteger(lu[i]) = i);
  i := 0;
  for u in lu do                // use an enumerator - safe and clean
  begin
    CheckEqual(Utf8ToInteger(u), i);
    inc(i);
  end;
  for u in lu.Range(-5) do      // use an enumerator for the last 5 items
    Check(Utf8ToInteger(u) > MAX - 5);
  for u in lu do
    CheckEqual(lu.IndexOf(u), Utf8ToInteger(u));   // O(n) brute force search
  for u in lu do
    CheckEqual(lu.Find(u), Utf8ToInteger(u));      // O(n) brute force search
  pu := lu.First;
  for i := 0 to lu.Count - 1 do // fastest method
  begin
    CheckEqual(Utf8ToInteger(pu^), i);
    inc(pu);
  end;
  lu := Collections.NewList<RawUtf8>([loCreateUniqueIndex]);
  lu.Capacity := MAX + 1;
  for i := 0 to MAX do          // populate with some data
    Check(lu.Add(UInt32ToUtf8(i)) = i);
  i := 0;
  for u in lu do                    // use an enumerator - safe and clean
  begin
    CheckEqual(Utf8ToInteger(u), i);
    CheckEqual(lu.IndexOf(u), i);   // O(n) brute force search
    CheckEqual(lu.Find(u), i);      // O(1) hash table search
    inc(i);
  end;
  // minimal THash128 compilation check
  {$ifdef SPECIALIZE_HASH}
  lh := Collections.NewList<THash128>;
  {$else}
  lh := Collections.NewPlainList<THash128>;
  {$endif SPECIALIZE_HASH}
  FillZero(h);
  lh.Add(h);
  lh.Sort;
  lh := nil;
  // manual IList<record> validation
  lr := Collections.NewPlainList<TSynTestFailed>;
  lr.Capacity := MAX + 1;
  r.TestName := 'n';
  for i := 0 to MAX do
  begin
    r.Error := IntToStr(i);
    CheckEqual(lr.Add(r), i);
    check(lr[i].Error = r.Error);
   end;
  for i := 0 to lr.Count - 1 do
    Check(lr[i].TestName = 'n');
  for i := 0 to lr.Count - 1 do
    CheckEqual(StrToInt(lr[i].Error), i);
  // validate and benchmark all main types using a generic sub method
  // call NewList<> here to circumvent FPC_64 internal error 2010021502 :(
  // - error appears only if T is coming through TestOne<T> generic method
  // - direct call of factory methods from here works fine (as in normal code),
  // and ensures early specifialization is triggered even on FPC x86_64/aarch64
  TestOne<byte>(Collections.NewList<byte>);
  TestOne<word>(Collections.NewList<word>);
  TestOne<integer>(Collections.NewList<integer>);
  TestOne<cardinal>(Collections.NewList<cardinal>);
  TestOne<Int64>(Collections.NewList<Int64>);
  TestOne<QWord>(Collections.NewList<QWord>);
  TestOne<Single>(Collections.NewList<single>);
  TestOne<Double>(Collections.NewList<double>);
  TestOne<TDateTime>(Collections.NewList<TDateTime>);
  TestOne<RawUtf8>(Collections.NewList<RawUtf8>);
  {$ifdef OSWINDOWS} // OleString on Windows only -> = UnicodeString on POSIX
  // disabled since SPECIALIZE_WSTRING is not set
  //TestOne<WideString>(Collections.NewList<WideString>);
  // note: WideString (BSTR API) is way slower than UnicodeString or RawUtf8
  {$endif OSWINDOWS}
  {$ifdef HASVARUSTRING}
  TestOne<UnicodeString>(Collections.NewList<UnicodeString>);
  {$endif HASVARUSTRING}
  TestOne<Variant>(Collections.NewList<Variant>);
  {$ifdef TESTHASH128}
  TestOne<THash128>({$ifdef SPECIALIZE_HASH}
                    Collections.NewList<THash128>
                    {$else}
                    Collections.NewPlainList<THash128>
                    {$endif SPECIALIZE_HASH});
  TestOne<TGuid>({$ifdef SPECIALIZE_HASH}
                 Collections.NewList<TGuid>
                 {$else}
                 Collections.NewPlainList<TGuid>
                 {$endif SPECIALIZE_HASH});
  {$endif TESTHASH128}
end;

procedure TTestCoreCollections._IKeyValue;
const
  MAX = 100000; // keys are hashed so we can have some
var
  i: integer;
  vi: Int64;
  di: IKeyValue<integer, Int64>;
  u: RawUtf8;
  pu: PRawUtf8Array;
  vu: double;
  du: IKeyValue<RawUtf8, double>;
  setcapa: boolean;
  setcapatxt: PUtf8Char;
  timer: TPrecisionTimer;
begin
  setcapatxt := nil;
  for setcapa := false to true do
  begin
    // manual IKeyValue<integer, Int64> validation
    di := Collections.NewKeyValue<integer, Int64>;
    if setcapa then
    begin
      di.Capacity := MAX + 1;
      setcapatxt := ' capa';
    end;
    timer.Start;
    for i := 0 to MAX do
      if i < 1000 then
        di.Add(i, i)
      else
        Check(di.TryAdd(i, i));
    CheckEqual(di.Count, MAX + 1);
    NotifyTestSpeed('integer,Int64 % add', [setcapatxt], MAX, 0, @timer);
    for i := 0 to MAX do
    begin
      Check(not di.TryAdd(i, i));
      Check(di.ContainsKey(i)); // key is searched with O(1) hashing
      if i < 1000 then          // value is searched as O(n)
        Check(di.ContainsValue(i));
      Check(di[i] = i);
      Check(di.TryGetValue(i, vi));
      Check(vi = i);
    end;
    timer.Start;
    for i := 0 to MAX do
      Check(di[i] = i);
    NotifyTestSpeed('integer,Int64 % get', [setcapatxt], MAX, 0, @timer);
    timer.Start;
    for i := 0 to MAX do
    begin
      Check(di.Key[i] = i);
      Check(di.Value[i] = i);
    end;
    NotifyTestSpeed('integer,Int64 % iter', [setcapatxt], MAX, 0, @timer);
    CheckEqual(di.Count, MAX + 1);
    di.Clear;
    CheckEqual(di.Count, 0);
    // manual IKeyValue<RawUtf8, double> validation
    du := Collections.NewKeyValue<RawUtf8, double>([kvoDefaultIfNotFound]);
    if setcapa then
      du.Capacity := MAX + 1;
    timer.Start;
    for i := 0 to MAX do
    begin
      UInt32ToUtf8(i, u);
      vu := i;
      if i < 1000 then
        du.Add(u, vu)
      else
        Check(du.TryAdd(u, vu));
    end;
    CheckEqual(du.Count, MAX + 1);
    NotifyTestSpeed('RawUtf8,double% add', [setcapatxt], MAX, 0, @timer);
    for i := 0 to MAX do
    begin
      UInt32ToUtf8(i, u);
      vu := i;
      Check(not du.TryAdd(u, vu));
      Check(du.ContainsKey(u));
      Check(du.GetItem(u) = vu);
      Check(du[u] = vu);
      Check(du.GetValueOrDefault(u, -1) = vu);
      if i < 1000 then
        Check(du.ContainsValue(vu));
      Check(du.TryGetValue(u, vu));
      Check(vu = i);
      UInt32ToUtf8(i + (MAX * 2), u);
      Check(not du.ContainsKey(u));
      Check(not du.TryGetValue(u, vu));
      Check(du[u] = 0); // kvoDefaultIfNotFound was set
    end;
    timer.Start;
    pu := du.Data.Keys.Value^; // fastest way to browse all keys
    for i := 0 to MAX do
      Check(du[pu[i]] = i);
    NotifyTestSpeed('RawUtf8,double% get', [setcapatxt], MAX, 0, @timer);
    timer.Start;
    for i := 0 to MAX do
    begin
      Check(du.Key[i] = pu[i]);
      Check(du.Value[i] = i);
    end;
    NotifyTestSpeed('RawUtf8,double% iter', [setcapatxt], MAX, 0, @timer);
    CheckEqual(du.Count, MAX + 1);
    du.Clear;
    Check(du.Count = 0);
  end;
end;

{$else}

type
  TTestCoreCollections = class(TSynTestCase)
  published
    procedure UnsupportedCompiler;
  end;

implementation

procedure TTestCoreCollections.UnsupportedCompiler;
begin
end;

{$endif HASGENERICS} // do-nothing unit on oldest compilers


end.

