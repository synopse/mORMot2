/// regression tests for most mormot.core.* units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.base;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  classes,
  math,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.json,
  mormot.core.variants,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.ecc,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.log,
  mormot.core.test,
  mormot.core.threads,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.db.core,
  mormot.orm.base,
  mormot.orm.core,
  mormot.rest.client;

const
  {$ifdef OSWINDOWS}
  HTTP_DEFAULTPORT = '888';
  {$else}
  HTTP_DEFAULTPORT = '8888'; // under Linux, port<1024 needs root user
  {$endif OSWINDOWS}


{$ifdef FPC_EXTRECORDRTTI}
  {$rtti explicit fields([vcPublic])} // mantadory :(
{$endif FPC_EXTRECORDRTTI}

type
  /// a test class, used by TTestServiceOrientedArchitecture
  // - also validates TPersistent objects as parameters for remote service calls
  TComplexNumber = class(TPersistent)
  private
    fReal: Double;
    fImaginary: Double;
  public
    /// create an instance to store a complex number
    constructor Create(aReal, aImaginary: double); reintroduce;
  published
    /// the real part of this complex number
    property Real: Double
      read fReal write fReal;
    /// the imaginary part of this complex number
    property Imaginary: Double
      read fImaginary write fImaginary;
  end;

  TComplexNumberObjArray = array of TComplexNumber;

  /// a record mapping TOrmPeople content
  TRecordPeople = packed record
    RowID: TID;
    FirstName: RawUtf8;
    LastName: RawUtf8;
    Data: RawBlob;
    YearOfBirth: integer;
    YearOfDeath: word;
  end;
  TRecordPeopleDynArray = array of TRecordPeople;

  // a record mapping used in the test classes of the framework
  // - this class can be used for debugging purposes, with the database
  // created by TTestFileBased from test.orm.sqlite3
  // - this class will use 'People' as a table name
  TOrmPeople = class(TOrm)
  private
    fData: RawBlob;
    fFirstName: RawUtf8;
    fLastName: RawUtf8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUtf8
      read fFirstName write fFirstName;
    property LastName: RawUtf8
      read fLastName write fLastName;
    property Data: RawBlob
      read fData write fData;
    property YearOfBirth: integer
      read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word
      read fYearOfDeath write fYearOfDeath;
  public
    /// method used to test the Client-Side
    // ModelRoot/TableName/ID/MethodName RESTful request, i.e.
    // ModelRoot/People/ID/DataAsHex in this case
    // - this method calls the supplied TRestClient to retrieve its results,
    // with the ID taken from the current TOrmPeole instance ID field
    // - parameters and result types depends on the purpose of the function
    // - TRestServerTest.DataAsHex published method implements the result
    // calculation on the Server-Side
    function DataAsHex(aClient: TRestClientUri): RawUtf8;
    /// method used to test the Client-Side
    // ModelRoot/MethodName RESTful request, i.e. ModelRoot/Sum in this case
    // - this method calls the supplied TRestClient to retrieve its results
    // - parameters and result types depends on the purpose of the function
    // - TRestServerTest.Sum published method implements the result calculation
    // on the Server-Side
    // - this method doesn't expect any ID to be supplied, therefore will be
    // called as class function - normally, it should be implement in a
    // TRestClient descendant, and not as a TOrm, since it does't depend
    // on TOrmPeople at all
    // - you could also call the same servce from the ModelRoot/People/ID/Sum URL,
    // but it won't make any difference)
    class function Sum(aClient: TRestClientUri; a, b: double; Method2: boolean): double;
  end;
  TOrmPeopleObjArray = array of TOrmPeople;
  POrmPeople = ^TOrmPeople;

  TOrmPeopleTimed = class(TOrmPeople)
  private
    fModif: TModTime;
  published
    property Modif: TModTime
      read fModif write fModif;
  end;

  /// a record used to test dynamic array serialization
  TFV = packed record
    Major, Minor, Release, Build: integer;
    Main, Detailed: string;
    BuildDateTime: TDateTime;
    BuildYear: integer;
  end;
  PFV = ^TFV;
  TFVs = array of TFV;

  TPersistentAutoCreateFieldsTest = class(TPersistentAutoCreateFields)
  private
    fText: RawUtf8;
    fValue1: TComplexNumber;
    fValue2: TComplexNumber;
  public
    constructor CreateFake;
  published
    property Text: RawUtf8 read fText write fText;
    property Value1: TComplexNumber read fValue1;
    property Value2: TComplexNumber read fValue2;
  end;
  TPersistentAutoCreateFieldsTestObjArray = array of TPersistentAutoCreateFieldsTest;

  TObjArrayTest = class(TPersistentAutoCreateFieldsTest)
  private
    fValues: TComplexNumberObjArray;
  published
    property values: TComplexNumberObjArray read fValues write fValues;
  end;

  TOrmArrayTest = class(TOrm)
  private
    fValues: TComplexNumberObjArray;
  published
    property values: TComplexNumberObjArray read fValues write fValues;
  end;

// sorting function to order by StrIComp(TOrmPeople.FirstName)
function TOrmPeopleCompareByFirstName(const A, B): integer;


type
  /// regression tests for most basic mormot.core.* features
  TTestCoreBase = class(TSynTestCase)
  protected
    a: TOrmPeopleObjArray;
    fAdd, fDel: RawUtf8;
    fQuickSelectValues: TIntegerDynArray;
    rnd: TLecuyer;
    procedure Setup; override;
    function QuickSelectGT(IndexA, IndexB: PtrInt): boolean;
    procedure intadd(const Sender; Value: integer);
    procedure intdel(const Sender; Value: integer);
    /// test the TDynArrayHashed object and methods (dictionary features)
    // - this test will create an array of 200,000 items to test speed
    procedure TDynArrayHashedSlow(Context: TObject);
    /// test the TSynDictionary class
    procedure TSynDictionarySlow(Context: TObject);
    /// test UTF-8 and Win-Ansi conversion (from or to, through RawUnicode)
    procedure Utf8Slow(Context: TObject);
    /// test the TSynTimeZone class and its cross-platform local time process
    procedure TimeZonesSlow(Context: TObject);
    /// test the TRawUtf8List class
    procedure TRawUtf8ListSlow(Context: TObject);
  published
    /// test RecordCopy(), TRttiMap and TRttiFilter
    procedure _Records;
    /// test the TSynList class
    procedure _TSynList;
    /// test the TDynArray object and methods
    procedure _TDynArray;
    /// validate the TSynQueue class
    procedure _TSynQueue;
    /// test TSynNameValue class
    procedure _TSynNameValue;
    /// test TRawUtf8Interning process
    procedure _TRawUtf8Interning;
    /// test T*ObjArray types and the ObjArray*() wrappers
    procedure _TObjArray;
    /// validate our optimized MoveFast/FillCharFast functions
    procedure CustomRTL;
    /// test StrIComp() and AnsiIComp() functions
    procedure FastStringCompare;
    /// test IdemPropName() and IdemPropNameU() functions
    procedure _IdemPropName;
    /// test our internal fast TGUID process functions
    procedure _GUID;
    /// test ParseCommandArgs() functions
    procedure _ParseCommandArgs;
    /// test TExecutableCommandLine class
    procedure _TExecutableCommandLine;
    /// test IsMatch() function
    procedure _IsMatch;
    /// test TExprParserMatch class
    procedure _TExprParserMatch;
    /// the Soundex search feature (i.e. TSynSoundex and all related
    // functions)
    procedure Soundex;
    /// low level fast Integer or Floating-Point to/from string conversion
    // - especially the RawUtf8 or PUtf8Char relative versions
    procedure NumericalConversions;
    /// test low-level integer/Int64 functions
    procedure Integers;
    /// test crc32c in both software and hardware (SSE4.2) implementations
    procedure _crc32c;
    /// test RDRAND Intel x86/x64 opcode if available, or fast gsl_rng_taus2
    procedure _Random32;
    /// test TSynBloomFilter class
    procedure BloomFilters;
    /// test DeltaCompress/DeltaExtract functions
    procedure _DeltaCompress;
    /// the new fast Currency to/from string conversion
    procedure Curr64;
    /// the camel-case / camel-uncase features, used for i18n from Delphi RTII
    procedure _CamelCase;
    /// the low-level bit management functions
    procedure Bits;
    /// the fast .ini file content direct access
    procedure IniFiles;
    /// validate Unicode / Ansi Charset conversion methods
    procedure Charsets;
    /// test UrlEncode() and UrlDecode() functions
    // - this method use some ISO-8601 encoded dates and times for the testing
    procedure UrlDecoding;
    /// test mime types recognition and multipart encoding
    procedure MimeTypes;
    /// test ASCII Baudot encoding
    procedure BaudotCode;
    /// the ISO-8601 date and time encoding
    // - test especially the conversion to/from text
    procedure Iso8601DateAndTime;
    /// test the SMBIOS decoding features
    procedure DmiSmbios;
    /// test Security IDentifier (SID) process
    procedure _SID;
    /// test the SecurityDescriptor / SDDL process
    procedure _SDDL;
    /// validates the median computation using the "Quick Select" algorithm
    procedure QuickSelect;
    /// test the TSynCache class
    procedure _TSynCache;
    /// low-level TSynFilter classes
    procedure _TSynFilter;
    /// low-level TSynValidate classes
    procedure _TSynValidate;
    /// low-level TSynLogFile class and OS detection
    procedure Debugging;
    /// client side geniune 64 bit identifiers generation
    procedure _TSynUniqueIdentifier;
    {$ifdef OSWINDOWS}
    /// some Windows-specific tests
    procedure WindowsSpecificApi;
    {$endif OSWINDOWS}
  end;



implementation


{ TOrmPeople }

function TOrmPeople.DataAsHex(aClient: TRestClientUri): RawUtf8;
begin
  result := aClient.CallBackGetResult('DataAsHex', [], RecordClass, fID);
end;

class function TOrmPeople.Sum(aClient: TRestClientUri; a, b: double;
  Method2: boolean): double;
var
  err: integer;
const
  METHOD: array[boolean] of RawUtf8 = ('sum', 'sum2');
begin
  result := GetExtended(pointer(
    aClient.CallBackGetResult(METHOD[Method2], ['a', a, 'b', b])), err);
end;


{ TComplexNumber }

constructor TComplexNumber.Create(aReal, aImaginary: double);
begin
  Real := aReal;
  Imaginary := aImaginary;
end;


{ TPersistentAutoCreateFieldsTest }

constructor TPersistentAutoCreateFieldsTest.CreateFake;
begin
  inherited Create;
  text := 'text';
  Value1.Real := 1.5;
  Value1.Imaginary := 2.5;
  Value2.Real := 1.7;
  Value2.Imaginary := 2.7;
end;


{ TTestCoreBase }

procedure TTestCoreBase.Setup;
begin
  RandomLecuyer(rnd);
end;

procedure TTestCoreBase._CamelCase;
var
  v: RawUtf8;
begin
  CheckEqual(UnCamelCase(''), '');
  v := UnCamelCase('On');
  CheckEqual(v, 'On');
  v := UnCamelCase('ON');
  CheckEqual(v, 'ON');
  v := UnCamelCase('OnLine');
  CheckEqual(v, 'On line');
  v := UnCamelCase('OnLINE');
  CheckEqual(v, 'On LINE');
  v := UnCamelCase('OnMyLINE');
  CheckEqual(v, 'On my LINE');
  v := UnCamelCase('On_MyLINE');
  CheckEqual(v, 'On - My LINE');
  v := UnCamelCase('On__MyLINE');
  CheckEqual(v, 'On: My LINE');
  v := UnCamelCase('Email1');
  CheckEqual(v, 'Email 1');
  v := UnCamelCase('Email12');
  CheckEqual(v, 'Email 12');
  v := UnCamelCase('KLMFlightNumber');
  CheckEqual(v, 'KLM flight number');
  v := UnCamelCase('GoodBBCProgram');
  CheckEqual(v, 'Good BBC program');
  CheckEqual(CamelCase(''), '');
  CheckEqual(CamelCase('a'), 'a');
  CheckEqual(CamelCase('A'), 'A');
  CheckEqual(CamelCase('abc'), 'abc');
  CheckEqual(CamelCase('Abc'), 'Abc');
  CheckEqual(CamelCase('AbcDef'), 'AbcDef');
  CheckEqual(CamelCase('Abc_Def'), 'AbcDef');
  CheckEqual(CamelCase('AbcDef_'), 'AbcDef');
  CheckEqual(CamelCase('Abc__Def'), 'AbcDef');
  CheckEqual(CamelCase('AbcDef__'), 'AbcDef');
  CheckEqual(CamelCase('Abc__Def__'), 'AbcDef');
  CheckEqual(CamelCase('variable name'), 'variableName');
  CheckEqual(CamelCase('Variable Name'), 'VariableName');
  CheckEqual(CamelCase('Variable: Name'), 'VariableName');
  CheckEqual(CamelCase('VARIABLE NAME'), 'VARIABLENAME');
  CheckEqual(CamelCase('VariableName'), 'VariableName');
  CheckEqual(LowerCamelCase(''), '');
  CheckEqual(LowerCamelCase('a'), 'a');
  CheckEqual(LowerCamelCase('A'), 'a');
  CheckEqual(LowerCamelCase('abc'), 'abc');
  CheckEqual(LowerCamelCase('Abc'), 'abc');
  CheckEqual(LowerCamelCase('AbcDef'), 'abcDef');
  CheckEqual(LowerCamelCase('Abc_Def'), 'abcDef');
  CheckEqual(LowerCamelCase('AbcDef_'), 'abcDef');
  CheckEqual(LowerCamelCase('Abc__Def'), 'abcDef');
  CheckEqual(LowerCamelCase('AbcDef__'), 'abcDef');
  CheckEqual(LowerCamelCase('Abc__Def__'), 'abcDef');
  CheckEqual(LowerCamelCase('variable name'), 'variableName');
  CheckEqual(LowerCamelCase('Variable Name'), 'variableName');
  CheckEqual(LowerCamelCase('Variable: Name'), 'variableName');
  CheckEqual(LowerCamelCase('VARIABLE NAME'), 'variablename');
  CheckEqual(LowerCamelCase('VariableName'), 'variableName');
  CheckEqual(SnakeCase(''), '');
  CheckEqual(SnakeCase('a'), 'a');
  CheckEqual(SnakeCase('A'), 'a');
  CheckEqual(SnakeCase('abc'), 'abc');
  CheckEqual(SnakeCase('Abc'), 'abc');
  CheckEqual(SnakeCase('AbcDef'), 'abc_def');
  CheckEqual(SnakeCase('Abc_Def'), 'abc_def');
  CheckEqual(SnakeCase('AbcDef_'), 'abc_def_');
  CheckEqual(SnakeCase('Abc__Def'), 'abc_def');
  CheckEqual(SnakeCase('AbcDef__'), 'abc_def_');
  CheckEqual(SnakeCase('Abc__Def__'), 'abc_def_');
  CheckEqual(SnakeCase('variable name'), 'variable_name');
  CheckEqual(SnakeCase('Variable Name'), 'variable_name');
  CheckEqual(SnakeCase('VARIABLE NAME'), 'variable_name');
  CheckEqual(SnakeCase('VariableName'), 'variable_name');
end;

function GetBitsCount64(const Bits; Count: PtrInt): PtrInt;
begin
  // reference implementation
  result := 0;
  while Count > 0 do
  begin
    dec(Count);
    if Count in TBits64(Bits) then // bt dword[rdi],edx is slow in such a loop
      inc(result);                 // ... but correct :)
  end;
end;

function GetBitsCountPurePascal(value: PtrInt): PtrInt;
begin
  result := value;
  {$ifdef CPU64}
  result := result - ((result shr 1) and $5555555555555555);
  result := (result and $3333333333333333) +
            ((result shr 2) and $3333333333333333);
  result := (result + (result shr 4)) and $0f0f0f0f0f0f0f0f;
  inc(result, result shr 8); // avoid slow multiplication
  inc(result, result shr 16);
  inc(result, result shr 32);
  result := result and $7f;
  {$else}
  result := result - ((result shr 1) and $55555555);
  result := (result and $33333333) + ((result shr 2) and $33333333);
  result := (result + (result shr 4)) and $0f0f0f0f;
  inc(result, result shr 8);
  inc(result, result shr 16);
  result := result and $3f;
  {$endif CPU64}
end;

function BSRdwordPurePascal(c: cardinal): cardinal;
const
  _debruijn32: array[0..31] of byte = (
    0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
    8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31);
begin // http://graphics.stanford.edu/~seander/bithacks.html#IntegerLogDeBruijn
  if c <> 0 then
  begin
    c := c or (c shr 1);
    c := c or (c shr 2);
    c := c or (c shr 4);
    c := c or (c shr 8);
    c := c or (c shr 16);
    c := c * $07c4acdd; // explicit step for 32-bit truncation
    result := _debruijn32[c shr 27];
  end
  else
    result := 255;
end;

procedure TTestCoreBase.Bits;
const
  N = 1000000;

  procedure TestPopCnt(const ctxt: string);
  var
    timer: TPrecisionTimer;
    i, c: integer;
    v: QWord;
  begin
    CheckEqual(GetBitsCountPtrInt(0), 0);
    CheckEqual(GetBitsCountPtrInt($f), 4);
    CheckEqual(GetBitsCountPtrInt($ff), 8);
    CheckEqual(GetBitsCountPtrInt($fff), 12);
    CheckEqual(GetBitsCountPtrInt($ffff), 16);
    CheckEqual(GetBitsCountPtrInt(-1), POINTERBITS);
    v := PtrUInt(-1);
    CheckEqual(GetBitsCount(v, 0), 0);
    CheckEqual(GetBitsCount64(v, 0), 0);
    for i := 0 to POINTERBITS - 1 do
    begin
      CheckEqual(GetBitsCountPtrInt(PtrInt(1) shl i), 1);
      if i < POINTERBITS - 1 then
      begin
        CheckEqual(GetBitsCountPtrInt(PtrInt(3) shl i), 2);
        CheckEqual(GetBitsCountPtrInt((PtrInt(1) shl (i + 1)) - 1), i + 1);
      end;
      if i < POINTERBITS - 2 then
        CheckEqual(GetBitsCountPtrInt(PtrInt(7) shl i), 3);
      if i < POINTERBITS - 3 then
        CheckEqual(GetBitsCountPtrInt(PtrInt(15) shl i), 4);
      CheckEqual(GetBitsCount64(v, i + 1), i + 1);
      CheckEqual(GetBitsCount(v, i + 1), i + 1);
    end;
    for i := 1 to 32 do
    begin
      v := ALLBITS_CARDINAL[i];
      CheckEqual(GetBitsCountPtrInt(v), i);
      CheckEqual(GetBitsCount(v, POINTERBITS), i);
      CheckEqual(GetBitsCount(v, i), i);
    end;
    for i := 1 to 1000 do
    begin
      v := i;
      c := GetBitsCount64(v, POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v), c);
      CheckEqual(GetBitsCount(v, POINTERBITS), c);
      {$ifdef FPC}
      CheckEqual(popcnt(v), c);
      {$endif FPC}
      c := BSRdwordPurePascal(v);
      CheckEqual(c, BSRdword(v));
      CheckEqual(c, BSRqword(v), 'bsrq1');
      v := v * v * 19;
      CheckEqual(BSRdwordPurePascal(v), BSRdword(v));
      c := GetBitsCount64(v, POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v), c);
      {$ifdef FPC}
      CheckEqual(popcnt(v), c);
      {$endif FPC}
      v := rnd.Next;
      c := BSRdwordPurePascal(v);
      CheckEqual(c, BSRdword(v));
      CheckEqual(c, BSRqword(v), 'bsrq2');
      {$ifdef CPU64}
      v := v or (PtrUInt(rnd.Next) shl 32);
      {$endif CPU64}
      c := GetBitsCount64(v, POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v), c);
      CheckEqual(GetBitsCount(v, POINTERBITS), c);
      {$ifdef FPC}
      CheckEqual(popcnt(v), c);
      {$endif FPC}
    end;
    CheckEqual(BSRdwordPurePascal(0), BSRdword(0));
    timer.Start;
    for i := 1 to N do
      GetBitsCountPtrInt(i);
    NotifyTestSpeed(ctxt, N, N shl POINTERSHR, @timer, {onlylog=}true);
  end;

var
  gen, ref: TLecuyer;
  Bits: TByteToByte;
  Bits64: Int64 absolute Bits;
  Si, i: integer;
  c: cardinal;
  s: ShortString;
  txt: RawUtf8;
  ip: THash128Rec;
  {$ifdef FPC}
  u: PtrUInt;
  timer: TPrecisionTimer;
  {$endif FPC}
begin
  FillZero(ip.b);
  Check(IsZero(ip.b));
  IP4Short(@ip, s);
  Check(s = '0.0.0.0');
  IP4Text(@ip, txt);
  CheckEqual(txt, '');
  IP6Short(@ip, s);
  Check(s = '::', '::');
  IP6Text(@ip, txt);
  CheckEqual(txt, '');
  ip.b[15] := 1;
  IP6Short(@ip, s);
  Check(s = '::1', '::1');
  IP6Text(@ip, txt);
  CheckEqual(txt, '127.0.0.1', 'IPv6 loopback');
  ip.b[0] := 1;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100::1');
  ip.b[15] := 0;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100::');
  ip.b[6] := $70;
  IP6Text(@ip, txt);
  CheckEqual(txt, '100:0:0:7000::');
  for i := 0 to 7 do
    ip.b[i] := i;
  IP6Text(@ip, txt);
  CheckEqual(txt, '1:203:405:607::');
  for i := 8 to 15 do
    ip.b[i] := i;
  IP6Text(@ip, txt);
  CheckEqual(txt, '1:203:405:607:809:a0b:c0d:e0f');
  for i := 0 to 15 do
    ip.b[i] := i or $70;
  IP6Text(@ip, txt);
  CheckEqual(txt, '7071:7273:7475:7677:7879:7a7b:7c7d:7e7f');
  Check(mormot.core.text.HexToBin('200100B80A0B12F00000000000000001', PByte(@ip), 16));
  IP6Text(@ip, txt);
  CheckEqual(txt, '2001:b8:a0b:12f0::1');
  {$ifdef CPUINTEL}
  GetBitsCountPtrInt := @GetBitsCountPurePascal;
  TestPopCnt('pas');
  GetBitsCountPtrInt := @GetBitsCountPas; // x86/x86_64 assembly
  TestPopCnt('asm');
  if cfPOPCNT in CpuFeatures then
  begin
    GetBitsCountPtrInt := @GetBitsCountSSE42;
    TestPopCnt('sse4.2');
  end;
  {$else}
  TestPopCnt('pas');
  {$endif CPUINTEL}
  {$ifdef FPC}
  timer.Start;
  for u := 1 to N do
    i := popcnt(u);
  NotifyTestSpeed('FPC', N, N shl POINTERSHR, @timer, {onlylog=}true);
  {$endif FPC}
  FillcharFast(Bits, SizeOf(Bits), 0);
  for i := 0 to high(Bits) * 8 + 7 do
  begin
    Check(not GetBit(Bits, i));
    Check(not GetBitPtr(@Bits, i));
  end;
  ref.SeedGenerator(0); // will reproduce the same gen.Next values
  gen := ref;
  for i := 1 to 100 do
  begin
    Si := gen.Next(SizeOf(Bits) shl 3);
    SetBit(Bits, Si);
    Check(GetBit(Bits, Si));
    Check(GetBitPtr(@Bits, Si));
  end;
  gen.SeedGenerator(0); // rewind
  for i := 1 to 100 do
    Check(GetBit(Bits, gen.Next(SizeOf(Bits) shl 3)));
  gen := ref; // rewind
  for i := 1 to 100 do
  begin
    Si := gen.Next(SizeOf(Bits) shl 3);
    UnSetBit(Bits, Si);
    Check(not GetBit(Bits, Si));
    Check(not GetBitPtr(@Bits, Si));
  end;
  Check(IsZero(@Bits, SizeOf(Bits)));
  for i := 0 to high(Bits) * 8 + 7 do
    Check(not GetBit(Bits, i));
  for i := 0 to 63 do
    Check(not GetBit64(Bits64, i));
  gen.SeedGenerator(0);
  for i := 1 to 30 do
  begin
    Si := gen.Next(64);
    SetBit64(Bits64, Si);
    Check(GetBit64(Bits64, Si));
  end;
  gen := ref;
  for i := 1 to 30 do
    Check(GetBit64(Bits64, gen.Next(64)));
  gen := ref;
  for i := 1 to 30 do
  begin
    Si := gen.Next(64);
    UnSetBit64(Bits64, Si);
    Check(not GetBit64(Bits64, Si));
  end;
  Check(IsZero(@Bits, SizeOf(Bits)));
  for i := 0 to 63 do
    Check(not GetBit64(Bits64, i));
  c := 1;
  for i := 1 to 32 do
  begin
    Check(GetAllBits($ffffffff, i));
    Check(not GetAllBits(0, i));
    Check(GetAllBits(c, i));
    Check(not GetAllBits(c and  - 2, i));
    Check(GetAllBits(ALLBITS_CARDINAL[i], i));
    c := c or (1 shl i);
  end;
end;

procedure TTestCoreBase.Curr64;
var
  tmp: string[63];
  i, err: Integer;
  V1: currency;
  V2: TSynExtended;
  i64: Int64;
  v: RawUtf8;
begin
  Check(TruncTo2Digits(1) = 1);
  Check(TruncTo2Digits(1.05) = 1.05);
  Check(TruncTo2Digits(1.051) = 1.05);
  Check(TruncTo2Digits(1.0599) = 1.05);
  Check(TruncTo2Digits(-1) = -1);
  Check(TruncTo2Digits(-1.05) = -1.05);
  Check(TruncTo2Digits(-1.051) = -1.05);
  Check(TruncTo2Digits(-1.0599) = -1.05);
  Check(SimpleRoundTo2Digits(1) = 1);
  Check(SimpleRoundTo2Digits(1.05) = 1.05);
  Check(SimpleRoundTo2Digits(1.051) = 1.05);
  Check(SimpleRoundTo2Digits(1.0549) = 1.05);
  Check(SimpleRoundTo2Digits(1.0550) = 1.05);
  Check(SimpleRoundTo2Digits(1.0551) = 1.06);
  Check(SimpleRoundTo2Digits(1.0599) = 1.06);
  Check(SimpleRoundTo2Digits(-1) = -1);
  Check(SimpleRoundTo2Digits(-1.05) = -1.05);
  Check(SimpleRoundTo2Digits(-1.051) = -1.05);
  Check(SimpleRoundTo2Digits(-1.0549) = -1.05);
  Check(SimpleRoundTo2Digits(-1.0550) = -1.05);
  Check(SimpleRoundTo2Digits(-1.0551) = -1.06);
  Check(SimpleRoundTo2Digits(-1.0599) = -1.06);
  Check(StrToCurr64('.5') = 5000);
  Check(StrToCurr64('.05') = 500);
  Check(StrToCurr64('.005') = 50);
  Check(StrToCurr64('.0005') = 5);
  Check(StrToCurr64('.00005') = 0);
  Check(StrToCurr64('0.5') = 5000);
  Check(StrToCurr64('0.05') = 500);
  Check(StrToCurr64('0.005') = 50);
  Check(StrToCurr64('0.0005') = 5);
  Check(StrToCurr64('0.00005') = 0);
  Check(StrToCurr64('1.5') = 15000);
  Check(StrToCurr64('1.05') = 10500);
  Check(StrToCurr64('1.005') = 10050);
  Check(StrToCurr64('1.0005') = 10005);
  Check(StrToCurr64('1.00005') = 10000);
  Check(StrToCurr64(pointer(Curr64ToStr(1))) = 1);
  Check(StrToCurr64(pointer(Curr64ToStr(12))) = 12);
  Check(StrToCurr64(pointer(Curr64ToStr(123))) = 123);
  Check(StrToCurr64(pointer(Curr64ToStr(1234))) = 1234);
  Check(StrToCurr64(pointer(Curr64ToStr(12345))) = 12345);
  Check(StrToCurr64(pointer(Curr64ToStr(123456))) = 123456);
  Check(StrToCurr64(pointer(Curr64ToStr(12340000))) = 12340000);
  Check(StrToCurr64(pointer(Curr64ToStr(12345000))) = 12345000);
  Check(StrToCurr64(pointer(Curr64ToStr(12345600))) = 12345600);
  Check(StrToCurr64(pointer(Curr64ToStr(12345670))) = 12345670);
  Check(StrToCurr64(pointer(Curr64ToStr(12345678))) = 12345678);
  tmp[0] := AnsiChar(Curr64ToPChar(1, @tmp[1]));
  Check(tmp = '0.0001');
  tmp[0] := AnsiChar(Curr64ToPChar(12, @tmp[1]));
  Check(tmp = '0.0012');
  tmp[0] := AnsiChar(Curr64ToPChar(123, @tmp[1]));
  Check(tmp = '0.0123');
  tmp[0] := AnsiChar(Curr64ToPChar(1234, @tmp[1]));
  Check(tmp = '0.1234');
  for i := 0 to 5000 do
  begin
    if i < 500 then
      V1 := i * 3
    else
      V1 := rnd.NextQWord shr 28;
    if rnd.Next and 3 = 0 then
      V1 := -V1;
    v := Curr64ToStr(PInt64(@V1)^);
    tmp[0] := AnsiChar(Curr64ToPChar(PInt64(@V1)^, @tmp[1]));
    CheckEqual(RawUtf8(tmp), v);
    V2 := GetExtended(pointer(v), err);
    Check(err = 0);
    CheckSame(V1, V2, 1E-4);
    i64 := StrToCurr64(pointer(v));
    CheckEqual(PInt64(@V1)^, i64);
  end;
end;

procedure TTestCoreBase.FastStringCompare;

  procedure _HasAnyChar(const text, forbidden: RawUtf8; expected: boolean = true);
  var
    any: TSynAnsicharSet;
    i: PtrInt;
  begin
    Check(ContainsChars(text, forbidden) = expected);
    any := [];
    for i := 1 to length(forbidden) do
      include(any, forbidden[i]);
    Check(HasAnyChar(text, any) = expected);
  end;

begin
  CheckEqual(CompareText('', ''), 0);
  Check(CompareText('abcd', '') > 0);
  Check(CompareText('', 'abcd') < 0);
  CheckEqual(StrIComp(nil, nil), 0);
  CheckEqual(StrIComp(PAnsiChar('abcD'), nil), 1);
  CheckEqual(StrIComp(nil, PAnsiChar('ABcd')), -1);
  CheckEqual(StrIComp(PAnsiChar('abcD'), PAnsiChar('ABcd')), 0);
  Check(StrIComp(PAnsiChar('abcD'), PAnsiChar('ABcF')) =
    StrComp(PAnsiChar('ABCD'), PAnsiChar('ABCF')));
  CheckEqual(StrComp(PAnsiChar('abcD'), nil), 1, 'abcD');
  CheckEqual(StrComp(nil, PAnsiChar('ABcd')), -1, 'nil ABCd');
  CheckEqual(StrComp(nil, nil), 0, 'nil nil');
  CheckEqual(StrComp(PAnsiChar('ABCD'), PAnsiChar('ABCD')), 0, 'ABCD');
  CheckEqual(StrComp(PAnsiChar('ABCD'), PAnsiChar('ABCE')), -1, 'ABCDE');
  CheckEqual(StrComp(PAnsiChar('ABCD'), PAnsiChar('ABCC')), 1, 'ABCC');
  CheckEqual(AnsiIComp(pointer(PAnsiChar('abcD')), pointer(PAnsiChar('ABcd'))), 0);
  Check(AnsiIComp(pointer(PAnsiChar('abcD')), pointer(PAnsiChar('ABcF'))) =
    StrComp(PAnsiChar('ABCD'), PAnsiChar('ABCF')));
  Check(StrIComp(PAnsiChar('abcD'), PAnsiChar('ABcd')) =
    AnsiIComp(PAnsiChar('abcD'), PAnsiChar('ABcd')));
  Check(StrIComp(PAnsiChar('abcD'), PAnsiChar('ABcF')) =
    AnsiIComp(PAnsiChar('ABCD'), PAnsiChar('ABCF')));
  CheckEqual(strcspn(PAnsiChar('ab'), PAnsiChar('a'#0)), 0);
  CheckEqual(strcspn(PAnsiChar('ab'), PAnsiChar('b'#0)), 1);
  CheckEqual(strcspn(PAnsiChar('1234ab'), PAnsiChar('a'#0)), 4);
  CheckEqual(strcspn(PAnsiChar('12345ab'), PAnsiChar('a'#0)), 5);
  CheckEqual(strcspn(PAnsiChar('123456ab'), PAnsiChar('a'#0)), 6);
  CheckEqual(strcspn(PAnsiChar('1234567ab'), PAnsiChar('a'#0)), 7);
  CheckEqual(strcspn(PAnsiChar('12345678ab'), PAnsiChar('a'#0)), 8);
  CheckEqual(strcspn(PAnsiChar('1234ab'), PAnsiChar('c'#0)), 6);
  CheckEqual(strcspn(PAnsiChar('12345678901234567ab'),
    PAnsiChar('cccccccccccccccccccd')), 19);
  CheckEqual(strspn(PAnsiChar('abcdef'), PAnsiChar('debca')), 5);
  CheckEqual(strspn(PAnsiChar('baabbaabcd'), PAnsiChar('ab')), 8);
  CheckEqual(strspn(PAnsiChar('abcdef'), PAnsiChar('g'#0)), 0);
  CheckEqual(strspn(PAnsiChar('abcdef'), PAnsiChar('a'#0)), 1);
  CheckEqual(strspn(PAnsiChar('bbcdef'), PAnsiChar('b'#0)), 2);
  CheckEqual(strspn(PAnsiChar('bbcdef'), PAnsiChar('bf')), 2);
  CheckEqual(strspn(PAnsiChar('bcbdef'), PAnsiChar('cb')), 3);
  CheckEqual(strspn(PAnsiChar('baabcd'), PAnsiChar('ab')), 4);
  CheckEqual(strspn(PAnsiChar('abcdef'), PAnsiChar('debca')), 5);
  CheckEqual(strspn(PAnsiChar('baabbaabcd'), PAnsiChar('ab')), 8);
  CheckEqual(strspn(PAnsiChar('baabbaabbaabcd'), PAnsiChar('ab')), 12);
  CheckEqual(strspn(PAnsiChar('baabbaabbaabbabcd'), PAnsiChar('ab')), 15);
  CheckEqual(strspn(PAnsiChar('baabbaabbaabbaabcd'), PAnsiChar('ab')), 16);
  CheckEqual(strspn(PAnsiChar('baabbaabbaababaabcd'), PAnsiChar('ab')), 17);
  _HasAnyChar('', '', false);
  _HasAnyChar('', 'a', false);
  _HasAnyChar('', 'aa', false);
  _HasAnyChar('a', '', false);
  _HasAnyChar('abcde', '', false);
  _HasAnyChar('abdef', 'cg', false);
  _HasAnyChar('a', 'c', false);
  _HasAnyChar('a', 'cd', false);
  _HasAnyChar('a', 'cdef', false);
  _HasAnyChar('a', 'cdefg', false);
  _HasAnyChar('a', 'cdefga');
  _HasAnyChar('abcde', 'a');
  _HasAnyChar('abcde', 'b');
  _HasAnyChar('abcde', 'c');
  _HasAnyChar('abcde', 'e');
  _HasAnyChar('abcde', 'ga');
  _HasAnyChar('abcde', 'gb');
  _HasAnyChar('abcde', 'gc');
  _HasAnyChar('abcde', 'ge');
  _HasAnyChar('abcde', 'ihga');
  _HasAnyChar('abcde', 'ihgb');
  _HasAnyChar('abcde', 'ihgc');
  _HasAnyChar('abcde', 'ihge');
  _HasAnyChar('abcde', 'jihga');
  _HasAnyChar('abcde', 'jihgb');
  _HasAnyChar('abcde', 'jihgc');
  _HasAnyChar('abcde', 'jihge');
  _HasAnyChar('abcde', 'jihgak');
  _HasAnyChar('abcde', 'jihgbk');
  _HasAnyChar('abcde', 'jihgck');
  _HasAnyChar('abcde', 'jihgek');
  Check(HasOnlyChar('abab', ['a' .. 'c']));
  Check(HasOnlyChar('abab', ['a' .. 'c']));
  Check(HasOnlyChar('abbab', ['a' .. 'c']));
  Check(HasOnlyChar('abab', ['a' .. 'b']));
  Check(not HasOnlyChar('abaeb', ['a' .. 'c']));
  Check(not HasOnlyChar('eabab', ['a' .. 'c']));
  Check(not HasOnlyChar('ababe', ['a' .. 'c']));
  Check(HasOnlyChar('ababe', ['a' .. 'e']));
end;

procedure TTestCoreBase.IniFiles;
var
  Content, S, N, V: RawUtf8;
  Si, Ni, Vi, i, j: integer;
  P: PUtf8Char;
begin
  Content := '';
  for i := 1 to 1000 do
  begin
    Si := rnd.Next(20);
    Ni := rnd.Next(50);
    Vi := Si * Ni + Ni;
    if Si = 0 then
      S := ''
    else
      S := 'Section' + Int32ToUtf8(Si);
    N := Int32ToUtf8(Ni);
    V := Int32ToUtf8(Vi);
    UpdateIniEntry(Content, S, N, V);
    for j := 1 to 5 do
      Check(FindIniEntry(Content, S, N) = V, 'FindIniEntry');
    Check(FindIniEntry(Content, S, 'no') = '');
    Check(FindIniEntry(Content, 'no', N) = '');
  end;
  Check(FileFromString(Content, WorkDir + 'test.ini'), 'test.ini');
  Check(AlgoSynLZ.FileCompress(WorkDir + 'test.ini',
     WorkDir + 'test.ini.synlz', $ABA51051), 'synLZ');
  if CheckFailed(AlgoSynLZ.FileUnCompress(WorkDir + 'test.ini.synlz',
     WorkDir + 'test2.ini', $ABA51051), 'unSynLZ') then
    exit;
  S := StringFromFile(WorkDir + 'test2.ini');
  Check(S = Content, WorkDir + 'test2.ini');
  Content := 'abc'#13#10'def'#10'ghijkl'#13'1234567890';
  P := pointer(Content);
  Check(GetNextLine(P, P) = 'abc');
  Check(GetNextLine(P, P) = 'def');
  Check(GetNextLine(P, P) = 'ghijkl');
  Check(GetNextLine(P, P) = '1234567890');
  Check(P = nil);
  Check(FindNameValue(pointer(Content), 'A')^ = 'b');
  Check(FindNameValue(pointer(Content), 'AB')^ = 'c');
  Check(FindNameValue(pointer(Content), 'D')^ = 'e');
  Check(FindNameValue(pointer(Content), '1')^ = '2');
  Check(FindNameValue(pointer(Content), 'GHIJK')^ = 'l');
  Check(FindNameValue(pointer(Content), 'B') = nil);
  Check(FindNameValue(pointer(Content), 'L') = nil);
  Check(FindNameValue(pointer(Content), '2') = nil);
  Check(FindNameValue(pointer(Content), 'TOTO') = nil);
  Check(FindNameValue(Content, 'AB', S));
  Check(S = 'c');
  Check(FindNameValue(Content, 'DEF', S));
  Check(S = '');
  Check(FindNameValue(Content, 'G', S));
  Check(S = 'hijkl');
  Check(FindNameValue(Content, '1234', S));
  Check(S = '567890');
  Check(not FindNameValue(Content, 'H', S));
  Check(S = '');
end;

procedure TTestCoreBase.Soundex;
var
  e: cardinal;
  PC: PAnsiChar;
  Soundex: TSynSoundEx;
  s: WinAnsiString;
begin
  Check(SoundExAnsi(PAnsiChar(' 120 ')) = 0);
  if SOUNDEX_BITS = 8 then
    {%H-}e := $2050206
  else
    e := $2526;
  Check(SoundExAnsi(PAnsiChar('bonjour')) = e);
  Check(SoundExAnsi(PAnsiChar(' 123 bonjour.  m'), @PC) = e);
  Check((PC <> nil) and
        (PC^ = '.'));
  s := ' 123 bonjourtreslongmotquidepasse  m';
  s[15] := #232;
  s[28] := #233;
  Check(SoundExAnsi(pointer(s), @PC) <> 0);
  Check((PC <> nil) and
        (PC^ = ' '));
  Check(SoundExAnsi(PAnsiChar('BOnjour')) = e);
  Check(SoundExAnsi(PAnsiChar('Bnjr')) = e);
  Check(SoundExAnsi(PAnsiChar('bonchour')) = e);
  Check(SoundExAnsi(PAnsiChar('mohammad')) =
        SoundExAnsi(PAnsiChar('mohhhammeeet')));
  if SOUNDEX_BITS = 8 then
    {%H-}e := $2050206
  else
    e := $25262;
  Check(SoundExAnsi(PAnsiChar('bonjours')) = e);
  Check(SoundExAnsi(PAnsiChar('BOnjours')) = e);
  Check(SoundExAnsi(PAnsiChar('Bnjrs')) = e);
  Check(SoundExAnsi(PAnsiChar(' 120 ')) = 0);
  if SOUNDEX_BITS = 8 then
    {%H-}e := $2050206
  else
    e := $2526;
  Check(SoundExUtf8('bonjour') = e);
  Check(SoundExUtf8(' 123 bonjour.  m', @PC) = e);
  Check((PC <> nil) and
        (PC^ = 'm'));
  Check(SoundExUtf8(Pointer(WinAnsiToUtf8(s)), @PC) <> 0);
  Check((PC <> nil) and
        (PC^ = 'm'));
  Check(SoundExUtf8('BOnjour') = e);
  Check(SoundExUtf8('Bnjr') = e);
  Check(SoundExUtf8('bonchour') = e);
  Check(SoundExUtf8('mohammad') = SoundExUtf8('mohhhammeeet'));
  if SOUNDEX_BITS = 8 then
    {%H-}e := $2050206
  else
    e := $25262;
  Check(SoundExUtf8('bonjours') = e);
  Check(SoundExUtf8('BOnjours') = e);
  Check(SoundExUtf8('Bnjrs') = e);
  Check(Soundex.Prepare(PAnsiChar('mohamad'), sndxEnglish));
  Check(Soundex.Ansi('moi rechercher mohammed ici'));
  Check(Soundex.Utf8('moi rechercher mohammed ici'));
  Check(Soundex.Ansi('moi mohammed'));
  Check(Soundex.Utf8('moi mohammed'));
  Check(not Soundex.Ansi('moi rechercher mouette ici'));
  Check(not Soundex.Utf8('moi rechercher mouette ici'));
  Check(not Soundex.Ansi('moi rechercher mouette'));
  Check(not Soundex.Utf8('moi rechercher mouette'));
end;

procedure TTestCoreBase.TRawUtf8ListSlow(Context: TObject);
const
  MAX = 20000;
var
  i, n: integer;
  L: TRawUtf8List;
  C: TComponent;
  Rec: TSynFilterOrValidate;
  s: RawUtf8;
begin
  L := TRawUtf8List.CreateEx([fObjectsOwned]);
  try // no hash table involved
    for i := 0 to MAX do
    begin
      C := TComponent.Create(nil);
      C.Tag := i;
      Check(L.AddObject(UInt32ToUtf8(i), C) = i);
    end;
    Check(L.Count = MAX + 1);
    for i := 0 to MAX do
      Check(GetInteger(Pointer(L[i])) = i);
    for i := 0 to MAX do
      Check(TComponent(L.Objects[i]).Tag = i);
    Check(L.IndexOf('') < 0);
    Check(L.IndexOf('5') = 5);
    Check(L.IndexOf('999') = 999);
    for i := MAX downto 0 do
      if i and 1 = 0 then
        L.Delete(i); // delete half the array
    Check(L.Count = MAX div 2);
    for i := 0 to L.Count - 1 do
      Check(GetInteger(Pointer(L[i])) = TComponent(L.Objects[i]).Tag);
    Check(L.IndexOf('5') = 2);
    Check(L.IndexOf('6') < 0);
    Check(L.Exists('5'));
    Check(not L.Exists('6'));
  finally
    L.Free;
  end;
  L := TRawUtf8List.CreateEx([fObjectsOwned, fNoDuplicate, fCaseSensitive]);
  try // with hash table
    for i := 1 to MAX do
    begin
      Rec := TSynFilterLowerCase.Create; // any TSynPersistent would have done
      Rec.Parameters := Int32ToUtf8(i);
      CheckEqual(L.AddObject(Rec.Parameters, Rec), i - 1);
      CheckEqual(L.IndexOf(Rec.Parameters), i - 1);
    end;
    Check(not L.Exists(''));
    Check(L.IndexOf('abcd') < 0);
    Check(L.Count = MAX);
    n := 0;
    for i := 1 to MAX do
    begin
      UInt32ToUtf8(i, s);
      CheckEqual(L.IndexOf(s), n);
      CheckEqual(TSynFilterOrValidate(L.Objects[n]).Parameters, s);
      if i and 127 = 0 then
        CheckEqual(L.Delete(s), n)
      else
        inc(n);
    end;
    CheckEqual(L.Count, n);
    for i := 1 to MAX do
    begin
      UInt32ToUtf8(i, s);
      Check((L.IndexOf(s) >= 0) = (i and 127 <> 0));
    end;
    L.SaveToFile(WorkDir + 'utf8list.txt');
    L.Clear;
    CheckEqual(L.Count, 0);
    L.LoadFromFile(WorkDir + 'utf8list.txt');
    CheckEqual(L.Count, n);
    for i := 1 to MAX do
    begin
      UInt32ToUtf8(i, s);
      Check((L.IndexOf(s) >= 0) = (i and 127 <> 0));
    end;
    DeleteFile(WorkDir + 'utf8list.txt');
  finally
    L.Free;
  end;
end;

type
  TCity = record
    Name: string;
    Country: string;
    Latitude: double;
    Longitude: double;
  end;
  TCityDynArray = array of TCity;

  TAmount = packed record
    firmID: integer;
    amount: RawUtf8;
  end;
  TAmountDynArray = array of TAmount;

  TAmountI = packed record
    firmID: integer;
    amount: integer;
  end;
  TAmountIDynArray = array of TAmountI;

procedure TTestCoreBase.TDynArrayHashedSlow(Context: TObject);
var
  ACities: TDynArrayHashed;
  Cities: TCityDynArray;
  CitiesCount: integer;
  City: TCity;
  added: boolean;
  N: string;
  i, j: integer;
  A: TAmount;
  AI: TAmountI;
  AmountCollection: TAmountDynArray;
  AmountICollection: TAmountIDynArray;
  AmountDA, AmountIDA1, AmountIDA2: TDynArrayHashed;
const
  CITIES_MAX = 200000;
begin
  // default Init() will hash and compare binary content before string, i.e. firmID
  AmountDA.Init(TypeInfo(TAmountDynArray), AmountCollection);
  Check(AmountDA.Info.Parser = ptDynArray);
  Check(AmountDA.Info.ArrayFirstField = ptInteger);
  Check(@AmountDA.HashItem = @DynArrayHashOne(ptInteger));
  for i := 1 to 100 do
  begin
    A.firmID := i;
    A.amount := UInt32ToUtf8(i);
    Check(AmountDA.Add(A) = i - 1);
  end;
  AmountDA.ForceReHash;
  for i := 1 to length(AmountCollection) do
    Check(AmountDA.FindHashed(i) = i - 1);
  // default Init() will hash and compare the WHOLE binary content, i.e. 8 bytes
  AmountIDA1.Init(TypeInfo(TAmountIDynArray), AmountICollection);
  Check(AmountIDA1.Info.Parser = ptDynArray);
  Check(AmountIDA1.Info.ArrayFirstField = ptInt64);
  Check(@AmountIDA1.HashItem = @DynArrayHashOne(ptInt64));
  for i := 1 to 100 do
  begin
    AI.firmID := i;
    AI.amount := i * 2;
    Check(AmountIDA1.Add(AI) = i - 1);
  end;
  AmountIDA1.ForceReHash;
  for i := 1 to length(AmountICollection) do
  begin
    AI.firmID := i;
    AI.amount := i * 2;
    Check(AmountIDA1.FindHashed(AI) = i - 1);
  end;
  AmountIDA1.Clear;
  // specific hash & compare of the firmID integer first field
  AmountIDA2.InitSpecific(
    TypeInfo(TAmountIDynArray), AmountICollection, ptInteger);
  Check(AmountIDA2.Info.Parser = ptDynArray);
  Check(AmountIDA2.Info.ArrayFirstField = ptInt64); // global TRttiCustom untouched
  Check(@AmountIDA2.HashItem = @DynArrayHashOne(ptInteger));
  for i := 1 to 100 do
  begin
    AI.firmID := i;
    AI.amount := i * 2;
    Check(AmountIDA2.Add(AI) = i - 1);
  end;
  AmountIDA2.ForceReHash;
  for i := 1 to length(AmountICollection) do
    Check(AmountIDA2.FindHashed(i) >= 0);
  // valide generic-like features
  // see http://docwiki.embarcadero.com/CodeExamples/en/Generics_Collections_TDictionary_(Delphi)
  ACities.Init(TypeInfo(TCityDynArray), Cities, nil, nil, nil, @CitiesCount);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  ACities.Add(City);
  City.Name := 'London';
  City.Country := 'United Kingdom';
  City.Latitude := 51.5;
  City.Longitude := -0.17;
  ACities.Add(City);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := 0;
  City.Longitude := 0;
  ACities.Add(City);
  Check(ACities.Count = 3);
  ACities.ForceReHash; // will use default hash, and search by Name = 1st field
  City.Name := 'Iasi';
  Check(ACities.FindHashedAndFill(City) = 0);
  Check(City.Name = 'Iasi');
  Check(City.Country = 'Romania');
  CheckSame(City.Latitude, 47.16);
  CheckSame(City.Longitude, 27.58);
  Check(ACities.FindHashedAndDelete(City) = 0);
  Check(City.Name = 'Iasi');
  Check(ACities.Scan(City) < 0);
  Check(ACities.FindHashed(City) < 0);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := -34.6;
  City.Longitude := -58.45;
  Check(ACities.FindHashedAndUpdate(City, {addifnotexisting=}false) >= 0);
  City.Latitude := 0;
  City.Longitude := 0;
  Check(City.Name = 'Buenos Aires');
  Check(ACities.FindHashedAndFill(City) >= 0);
  CheckSame(City.Latitude, -34.6);
  CheckSame(City.Longitude, -58.45);
  Check(ACities.FindHashedForAdding(City, added) >= 0);
  Check(not added);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  i := ACities.FindHashedForAdding(City, added);
  Check(added);
  Check(i > 0);
  if i > 0 then
  begin
    Check(Cities[i].Name = ''); // FindHashedForAdding left void content
    Cities[i] := City; // should fill Cities[i] content by hand
  end;
  CheckEqual(ACities.Count, 3);
  Check(City.Name = 'Iasi');
  Check(ACities.FindHashed(City) >= 0);
  // add CITIES_MAX items
  for i := 1 to 2000 do
  begin
    City.Name := IntToString(i);
    City.Latitude := i * 3.14;
    City.Longitude := i * 6.13;
    CheckEqual(ACities.FindHashedAndUpdate(City, true), i + 2, 'multiple ReHash');
    CheckEqual(ACities.FindHashed(City), i + 2);
  end;
  ACities.Capacity := CITIES_MAX + 30; // will trigger HASH_PO2
  for i := 2001 to CITIES_MAX do
  begin
    City.Name := IntToString(i);
    City.Latitude := i * 3.14;
    City.Longitude := i * 6.13;
    if i = 8703 then
      City.Latitude := i * 3.14;
    CheckEqual(ACities.FindHashedAndUpdate(City, true), i + 2);
    CheckEqual(ACities.FindHashed(City.Name), i + 2);
  end;
  for i := 1 to CITIES_MAX do
  begin
    N := IntToString(i);
    Check(ACities.FindHashed(N) = i + 2);
  end;
  for i := 1 to CITIES_MAX do
  begin
    N := IntToString(i);
    j := ACities.FindHashed(N);
    Check(j >= 0);
    if i and 127 = 0 then
    begin
      Check(ACities.FindHashedAndDelete(N) >= 0, 'delete');
      j := ACities.FindHashed(N);
      Check(j < 0);
    end;
  end;
  for i := 1 to CITIES_MAX do
  begin
    N := IntToString(i);
    j := ACities.FindHashed(N);
    if i and 127 = 0 then
      Check(j < 0, 'deteled')
    else if not CheckFailed(j >= 0, N) then
    begin
      Check(Cities[j].Name = N);
      CheckSame(Cities[j].Latitude, i * 3.14);
      CheckSame(Cities[j].Longitude, i * 6.13);
    end;
  end;
end;

type
  TRec = packed record
    a: integer;
    b: byte;
    c: double;
    D: Currency;
  end;
  TRecs = array of TRec;

  TProvince = record
    Name: RawUtf8;
    Comment: RawUtf8;
    Year: cardinal;
    Cities: TCityDynArray;
  end;

  TFV2 = packed record
    V1: TFV;
    value: integer;
    V2: TFV;
    Text: string;
  end;
  TFV2s = array of TFV2;

  TSynValidates = array of TSynValidate;

  TDataItem = record
    Modified: TDateTime;
    Data: string;
  end;
  TDataItems = array of TDataItem;

  TRawUtf8DynArray1 = type TRawUtf8DynArray;
  TRawUtf8DynArray2 = array of RawUtf8;

function FVSort(const A, B): integer;
begin
  // string/PChar compariosn of first "Detailed" field
  result := SysUtils.StrComp(
    PChar(pointer(TFV(A).Detailed)), PChar(pointer(TFV(B).Detailed)));
end;

procedure TTestCoreBase._TDynArray;
var
  AI, AI2: TIntegerDynArray;
  AU, AU2: TRawUtf8DynArray;
  AV1, AV2: TVariantDynArray;
  AR: TRecs;
  AF: TFVs;
  AF2: TFV2s;
  i, j, k, Len, count, AIcount: integer;
  U, U2: RawUtf8;
  P: PUtf8Char;
  PA: PAnsiChar;
  PI: PIntegerArray;
  AB: TBooleanDynArray;
  R: TRec;
  F, F1: TFV;
  F2: TFV2;
  City: TCity;
  Province: TProvince;
  AV: TSynValidates;
  V: TSynValidate;
  AIP, AI2P, AUP, ARP, AFP, ACities, AVP, dyn1, dyn2: TDynArray;
  dyniter: TDynArrayLoadFrom;
  B: boolean;
  dp: TDataItem;
  dyn1Array, dyn2Array: TDataItems;
  Test, Test2: RawByteString;
  ST: TCustomMemoryStream;
  Index: TIntegerDynArray;
  W: TJsonWriter;
  {$ifndef HASEXTRECORDRTTI}
  JSON_BASE64_MAGIC_UTF8: RawUtf8;
  {$endif HASEXTRECORDRTTI}
  tmp: TSynTempBuffer;
const
  MAGIC: array[0..1] of word = (34, $fff0);
  BUILDDATETIME: TDateTime = 36215.12; // circumvent a weird FPC/Android issue (Alf)

  procedure Fill(var F: TFV; const i: integer);
  begin
    F.Major := i;
    F.Minor := i + 1;
    F.Release := i + 2;
    F.Build := i + 3;
    F.Main := IntToString(i + 1000);
    F.Detailed := IntToString(2000 - i);
    F.BuildDateTime := BUILDDATETIME;
    F.BuildYear := i + 2011;
  end;

  procedure TestAF2;
  var
    i: integer;
    F1, F2: TFV;
  begin
    for i := 0 to AFP.Count - 1 do
    begin
      Check(AF2[i].value = i);
      Check(AF2[i].Text = IntToString(i));
      Fill(F1, i * 2);
      Fill(F2, i * 2 + 1);
      Check(RecordEquals(F1, AF2[i].V1, TypeInfo(TFV)));
      Check(RecordEquals(F2, AF2[i].V2, TypeInfo(TFV)));
    end;
  end;

  procedure Test64K;
  var
    i, E, n: integer;
    D: TDynArray;
    IA: TIntegerDynArray;
  begin
    D.Init(TypeInfo(TIntegerDynArray), IA, @n);
    D.Capacity := 16300;
    for i := 0 to 16256 do
    begin
      E := i * 5;
      Check(D.Add(E) = i);
      Check(IA[i] = i * 5);
    end;
    Check(D.Count = 16257);
    Check(D.Capacity = 16300);
    Check(length(IA) = D.Capacity);
    for i := 0 to 16256 do
      Check(IA[i] = i * 5);
    CheckHash(D.SaveTo, $55A23EC0, 'test64k');
  end;

  procedure TestCities;
  var
    i: PtrInt;
  begin
    for i := 0 to ACities.Count - 1 do
      with Province.Cities[i] do
      begin
        {$ifdef UNICODE}
        Check(StrToInt(Name) = i);
        {$else}
        Check(GetInteger(pointer(Name)) = i);
        {$endif UNICODE}
        CheckSame(Latitude, i * 3.14);
        CheckSame(Longitude, i * 6.13);
      end;
  end;

begin
  // run the slowest tests in a background thread
  Run(TDynArrayHashedSlow, self, 'TDynArrayHashed', true, false);
  Run(TSynDictionarySlow, self, 'TSynDictionary', true, false);
  Run(Utf8Slow, self, 'UTF-8', true, false);
  Run(TimeZonesSlow, self, 'TimeZones', true, false);
  Run(TRawUtf8ListSlow, self, 'TRawUtf8List', true, false);
  { TODO : implement TypeInfoToHash() if really needed }
  {
  h := TypeInfoToHash(TypeInfo(TAmount));
  Check(h=$9032161B,'TypeInfoToHash(TAmount)');
  h := TypeInfoToHash(TypeInfo(TAmountDynArray));
  Check(h=$887ED692,'TypeInfoToHash(TAmountDynArray)');
  h := TypeInfoToHash(TypeInfo(TAmountIDynArray));
  Check(h=$4051BAC,'TypeInfoToHash(TAmountIDynArray)');
  }
  Check(not IsRawUtf8DynArray(nil), 'IsRawUtf8DynArray0');
  Check(IsRawUtf8DynArray(TypeInfo(TRawUtf8DynArray)), 'IsRawUtf8DynArray1');
  Check(IsRawUtf8DynArray(TypeInfo(TRawUtf8DynArray1)), 'IsRawUtf8DynArray11');
  Check(IsRawUtf8DynArray(TypeInfo(TRawUtf8DynArray2)), 'IsRawUtf8DynArray12');
  Check(not IsRawUtf8DynArray(TypeInfo(TAmount)), 'IsRawUtf8DynArray2');
  Check(not IsRawUtf8DynArray(TypeInfo(TIntegerDynArray)), 'IsRawUtf8DynArray2');
  Check(not IsRawUtf8DynArray(TypeInfo(TPointerDynArray)), 'IsRawUtf8DynArray3');
  Check(not IsRawUtf8DynArray(TypeInfo(TAmountDynArray)), 'IsRawUtf8DynArray4');
  SetLength(AU, 2);
  AU[0] := 'true';
  AU[1] := 'false';
  SetLength(AU2, 2);
  AU2[0] := 'True';
  AU2[1] := 'False';
  Check(DynArrayEquals(TypeInfo(TRawUtf8DynArray), AU, AU2,
    nil, nil, {CaseInsensitive=}True));
  AU := nil; // for test below
  SetLength(AV1, 2);
  AV1[0] := 'true';
  AV1[1] := 'false';
  SetLength(AV2, 2);
  AV2[0] := 'True';
  AV2[1] := 'False';
  Check(DynArrayEquals(TypeInfo(TVariantDynArray), AV1, AV2,
    nil, nil, {CaseInsensitive=}True));
  AV1[0] := SynUnicode('true');
  AV1[1] := SynUnicode('false');
  SetLength(AV2, 2);
  AV2[0] := SynUnicode('True');
  AV2[1] := SynUnicode('False');
  Check(DynArrayEquals(TypeInfo(TVariantDynArray), AV1, AV2,
    nil, nil, {CaseInsensitive=}True));
  W := TJsonWriter.CreateOwnedStream;
  // validate TBooleanDynArray
  dyn1.Init(TypeInfo(TBooleanDynArray), AB);
  SetLength(AB, 4);
  for i := 0 to 3 do
    AB[i] := i and 1 = 1;
  Test := dyn1.SaveToJson;
  check(Test = '[false,true,false,true]');
  Check(AB <> nil);
  dyn1.Clear;
  Check(AB = nil);
  Check(dyn1.Count = 0);
  Check(dyn1.LoadFromJson(pointer(Test)) <> nil);
  Check(length(AB) = 4);
  Check(dyn1.Count = 4);
  for i := 0 to 3 do
    Check(AB[i] = (i and 1 = 1));
  Test := dyn1.SaveTo;
  dyn1.Clear;
  Check(AB = nil);
  Check(dyn1.LoadFrom(pointer(Test), PAnsiChar(Test) + length(Test)) <> nil);
  Check(dyn1.Count = 4);
  for i := 0 to 3 do
    Check(AB[i] = (i and 1 = 1));
  dyn1.Clear;
  Check(AB = nil);
  Check(dyn1.LoadFromBinary(Test));
  Check(dyn1.Count = 4);
  for i := 0 to 3 do
    Check(AB[i] = (i and 1 = 1));
  Check(dyniter.Init(TypeInfo(TBooleanDynArray), Test));
  Check(dyniter.Count = 4);
  for i := 0 to 3 do
  begin
    Check(dyniter.FirstField(@B));
    Check(B = (i and 1 = 1));
    B := not B;
    Check(dyniter.Step(@B));
    Check(B = (i and 1 = 1));
  end;
  Check(not dyniter.Step(@B));
  Check(not dyniter.FirstField(@B));
  // validate TIntegerDynArray
  Test64K;
  AIP.Init(TypeInfo(TIntegerDynArray), AI);
  for i := 0 to 1000 do
  begin
    Check(AIP.Count = i);
    Check(AIP.Add(i) = i);
    Check(AIP.Count = i + 1);
    Check(AI[i] = i);
  end;
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i) = i);
  for i := 0 to 1000 do
  begin
    Check(IntegerScanExists(Pointer(AI), i + 1, i));
    Check(IntegerScanExists(Pointer(AI), AIP.Count, i));
    Check(not IntegerScanExists(Pointer(AI), AIP.Count, i + 2000));
  end;
  Test := AIP.SaveTo;
  CheckHash(Test, $60DCA314, 'hash32i');
  PI := IntegerDynArrayLoadFrom(pointer(Test), AIcount);
  Check(AIcount = 1001);
  Check(PI <> nil);
  for i := 0 to 1000 do
    Check(PI[i] = i);
  W.AddDynArrayJson(AIP);
  U := W.Text;
  P := pointer(U);
  for i := 0 to 1000 do
    Check(GetNextItemCardinal(P) = cardinal(i));
  CheckHash(U, $CBDFDAFC, 'hash32a');
  for i := 0 to 1000 do
  begin
    Test2 := AIP.ItemSave(@i);
    Check(length(Test2) = 4);
    k := 0;
    AIP.ItemLoad(pointer(Test2), PAnsiChar(Test2) + length(Test2), @k);
    Check(k = i);
    Check(AIP.ItemLoadFind(pointer(Test2), PAnsiChar(Test2) + length(Test2)) = i);
  end;
  AIP.Reverse;
  for i := 0 to 1000 do
    Check(AI[i] = 1000 - i);
  AIP.Clear;
  Check(AIP.LoadFrom(pointer(Test), PAnsiChar(Test) + length(Test)) <> nil);
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i) = i);
  AIP.Clear;
  Check(AIP.LoadFromBinary(Test));
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i) = i);
  for i := 1000 downto 0 do
    if i and 3 = 0 then
      AIP.Delete(i);
  Check(AIP.Count = 750);
  for i := 0 to 1000 do
    if i and 3 = 0 then
      Check(AIP.IndexOf(i) < 0)
    else
      Check(AIP.IndexOf(i) >= 0);
  AIP.Clear;
  Check(AIP.LoadFromJson(pointer(U)) <> nil);
  for i := 0 to 1000 do
    Check(AI[i] = i);
  AIP.Init(TypeInfo(TIntegerDynArray), AI, @AIcount);
  for i := 0 to 50000 do
  begin
    Check(AIP.Count = i, 'use of AIcount should reset it to zero');
    Check(AIP.Add(i) = i);
    Check(AIP.Count = i + 1);
    Check(AI[i] = i);
  end;
  AIP.Compare := SortDynArrayInteger;
  AIP.Sort;
  Check(AIP.Count = 50001);
  for i := 0 to AIP.Count - 1 do
    Check(AIP.Find(i) = i);
  Test := AIP.SaveTo;
  CheckHash(Test, $69562803, 'hash32b');
  AIP.Reverse;
  for i := 0 to 50000 do
    Check(AI[i] = 50000 - i);
  SetLength(AI, AIcount);
  AIP.Init(TypeInfo(TIntegerDynArray), AI);
  AIP.Compare := SortDynArrayInteger;
  AIP.Sort;
  Test := AIP.SaveTo;
  CheckHash(Test, $69562803, 'hash32c');
  AIP.Reverse;
  AIP.Slice(AI2, 2000, 1000);
  Check(length(AI2) = 2000);
  for i := 0 to 1999 do
    Check(AI2[i] = 49000 - i);
  AIP.AddArray(AI2, 1000, 2000);
  Check(AIP.Count = 51001);
  for i := 0 to 50000 do
    Check(AI[i] = 50000 - i);
  for i := 0 to 999 do
    Check(AI[i + 50001] = 48000 - i);
  AIP.Count := 50001;
  AIP.AddArray(AI2);
  Check(AIP.Count = 52001);
  for i := 0 to 50000 do
    Check(AI[i] = 50000 - i);
  for i := 0 to 1999 do
    Check(AI[i + 50001] = 49000 - i);
  AIP.Clear;
  with DynArray(TypeInfo(TIntegerDynArray), AI) do
  begin
    Check(LoadFromBinary(Test));
    for i := 0 to count - 1 do
      Check(AI[i] = i);
  end;
  Check(AIP.Count = 50001);
  AI2P.Init(TypeInfo(TIntegerDynArray), AI2);
  AIP.AddDynArray(@AI2P);
  Check(AIP.Count = 52001);
  for i := 0 to 50000 do
    Check(AI[i] = i);
  for i := 0 to 1999 do
    Check(AI[i + 50001] = 49000 - i);
  // validate TSynValidates (an array of classes is an array of PtrInt)
  AVP.Init(TypeInfo(TSynValidates), AV);
  for i := 0 to 1000 do
  begin
    Check(AVP.Count = i);
    PtrInt(V) := i;
    Check(AVP.Add(V) = i);
    Check(AVP.Count = i + 1);
    Check(AV[i] = V);
  end;
  Check(length(AV) = 1001);
  Check(AVP.Count = 1001);
  for i := 0 to 1000 do
  begin
    // untyped const must be the same exact type !
    PtrInt(V) := i;
    Check(AVP.IndexOf(V) = i);
  end;
  Test := AVP.SaveTo;
  CheckHash(Test, {$ifdef CPU64} $2CB4A314 {$else} $60DCA314 {$endif}, 'AVP.SaveTo');
  // validate TRawUtf8DynArray
  AUP.Init(TypeInfo(TRawUtf8DynArray), AU);
  for i := 0 to 1000 do
  begin
    Check(AUP.Count = i);
    U := UInt32ToUtf8(i + 1000);
    Check(AUP.Add(U) = i);
    Check(AUP.Count = i + 1);
    Check(AU[i] = U);
  end;
  for i := 0 to 1000 do
  begin
    U := Int32ToUtf8(i + 1000);
    Check(AUP.IndexOf(U) = i);
  end;
  U := '3000';
  Check(AUP.IndexOf(U) < 0);
  Test := AUP.SaveTo;
  CheckEqual(Hash32(@Test[2], length(Test) - 1), $1EC51463, 'hash32e');
  // trimed Test[1]=ElemSize
  for i := 0 to 1000 do
  begin
    U := Int32ToUtf8(i + 1000);
    Check(RawUtf8DynArrayLoadFromContains(
      pointer(Test), pointer(U), length(U), false) = i);
    Check(RawUtf8DynArrayLoadFromContains(
      pointer(Test), pointer(U), length(U), true) = i);
  end;
  for i := 0 to 1000 do
  begin
    U := UInt32ToUtf8(i + 1000);
    Test2 := AUP.ItemSave(@U);
    Check(length(Test2) > 4);
    U := '';
    AUP.ItemLoad(pointer(Test2), PAnsiChar(Test2) + length(Test2), @U);
    Check(GetInteger(pointer(U)) = i + 1000);
    Check(AUP.ItemLoadFind(pointer(Test2), PAnsiChar(Test2) + length(Test2)) = i);
  end;
  W.CancelAll;
  W.AddDynArrayJson(AUP);
  W.SetText(U);
  CheckHash(U, $1D682EF8, 'hash32f');
  P := pointer(U);
  if not CheckFailed(P^ = '[') then
    inc(P);
  for i := 0 to 1000 do
  begin
    Check(P^ = '"');
    inc(P);
    Check(GetNextItemCardinal(P) = cardinal(i + 1000));
    if P = nil then
      break;
  end;
  Check(P = nil);
  AUP.Clear;
  Check(AUP.LoadFrom(pointer(Test),
    PAnsiChar(Test) + length(Test)) - pointer(Test) = length(Test));
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i])) = i + 1000);
  AUP.Clear;
  Check(AUP.LoadFromBinary(Test));
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i])) = i + 1000);
  Check(dyniter.Init(TypeInfo(TRawUtf8DynArray), Test));
  Check(dyniter.Count = 1001);
  for i := 0 to 1000 do
  begin
    Check(dyniter.FirstField(@U2));
    Check(GetInteger(pointer(U2)) = i + 1000);
    U2 := '';
    Check(dyniter.Step(@U2));
    Check(GetInteger(pointer(U2)) = i + 1000);
  end;
  Check(not dyniter.Step(@U2));
  Check(not dyniter.FirstField(@U2));
  AUP.Clear;
  Check(AUP.LoadFromJson(pointer(U)) <> nil);
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i])) = i + 1000);
  for i := 0 to 1000 do
  begin
    U := Int32ToUtf8(i + 1000);
    Check(AUP.IndexOf(U) = i);
  end;
  for i := 1000 downto 0 do
    if i and 3 = 0 then
      AUP.Delete(i);
  Check(AUP.Count = 750);
  for i := 0 to 1000 do
  begin
    U := Int32ToUtf8(i + 1000);
    if i and 3 = 0 then
      Check(AUP.IndexOf(U) < 0)
    else
      Check(AUP.IndexOf(U) >= 0);
  end;
  U := 'inserted';
  AUP.Insert(500, U);
  Check(AUP.IndexOf(U) = 500);
  j := 0;
  for i := 0 to AUP.Count - 1 do
    if i <> 500 then
    begin
      U := Int32ToUtf8(j + 1000);
      if j and 3 = 0 then
        Check(AUP.IndexOf(U) < 0)
      else
        Check(AUP.IndexOf(U) >= 0);
      inc(j);
    end;
  AUP.CreateOrderedIndex(Index, SortDynArrayAnsiString);
  Check(StrComp(pointer(AU[Index[750]]), pointer(AU[Index[749]])) > 0);
  for i := 1 to AUP.Count - 1 do
    Check(AU[Index[i]] > AU[Index[i - 1]]);
  AUP.Compare := SortDynArrayAnsiString;
  AUP.Sort;
  Check(AUP.Sorted);
  Check(AU[AUP.Count - 1] = 'inserted');
  for i := 1 to AUP.Count - 1 do
    Check(AU[i] > AU[i - 1]);
  j := 0;
  for i := 0 to AUP.Count - 1 do
    if i <> 500 then
    begin
      U := Int32ToUtf8(j + 1000);
      if j and 3 = 0 then
        Check(AUP.Find(U) < 0)
      else
        Check(AUP.Find(U) >= 0);
      inc(j);
    end;
  AUP.Sorted := false;
  j := 0;
  for i := 0 to AUP.Count - 1 do
    if i <> 500 then
    begin
      U := Int32ToUtf8(j + 1000);
      if j and 3 = 0 then
        Check(AUP.Find(U) < 0)
      else
        Check(AUP.Find(U) >= 0);
      inc(j);
    end;
  // validate packed binary record (no string inside)
  ARP.Init(TypeInfo(TRecs), AR);
  for i := 0 to 1000 do
  begin
    Check(ARP.Count = i);
    R.A := i;
    R.B := i + 1;
    R.C := i * 2.2;
    R.D := i * 3.25;
    Check(ARP.Add(R) = i);
    Check(ARP.Count = i + 1);
  end;
  for i := 0 to 1000 do
  begin
    with AR[i] do
    begin
      Check(a = i);
      Check(B = byte(i + 1));
      CheckSame(c, i * 2.2);
      CheckSame(D, i * 3.25);
    end;
    R.A := i;
    R.B := i + 1;
    R.C := i * 2.2;
    R.D := i * 3.25;
    Check(ARP.IndexOf(R) = i); // will work (packed + no ref-counted types inside)
  end;
  W.CancelAll;
  W.AddDynArrayJson(ARP);
  U := W.Text;
  {$ifndef HASEXTRECORDRTTI} // enhanced RTTI won't let binary serialization
  P := pointer(U);
  JSON_BASE64_MAGIC_UTF8 := RawUnicodeToUtf8(@MAGIC, 2);
  U2 := RawUtf8('[') + JSON_BASE64_MAGIC_UTF8 +
        RawUtf8(BinToBase64(ARP.SaveTo)) + RawUtf8('"]');
  Check(U = U2);
  {$endif HASEXTRECORDRTTI}
  ARP.Clear;
  Check(ARP.LoadFromJson(pointer(U)) <> nil);
  if not CheckFailed(ARP.Count = 1001) then
    for i := 0 to 1000 do
      with AR[i] do
      begin
        Check(a = i);
        Check(B = byte(i + 1));
        CheckSame(c, i * 2.2);
        CheckSame(D, i * 3.25);
      end;
  // validate packed record with strings inside
  AFP.Init(TypeInfo(TFVs), AF);
  for i := 0 to 1000 do
  begin
    Check(AFP.Count = i);
    Fill(F, i);
    Check(AFP.Add(F) = i);
    Check(AFP.Count = i + 1);
  end;
  Fill(F, 100);
  Check(RecordEquals(F, AF[100], TypeInfo(TFV)));
  Len := 38;
  {$ifdef UNICODE}
  inc(Len, length(F.Main) + length(F.Detailed)); // adjust wideChars binary size
  {$endif UNICODE}
  RecordSave(F, tmp, TypeInfo(TFV));
  Check(tmp.len = Len);
  Fill(F, 0); // reset
  Check(RecordLoad(F, tmp.buf, TypeInfo(TFV), nil, tmp.BufEnd) - tmp.buf = Len);
  tmp.Done;
  Check(RecordEquals(F, AF[100], TypeInfo(TFV)));
  Test := RecordSaveBase64(F, TypeInfo(TFV));
  Check(Test <> '');
  Fill(F, 0);
  Check(RecordLoadBase64(pointer(Test), length(Test), F, TypeInfo(TFV)));
  Check(RecordEquals(F, AF[100], TypeInfo(TFV)));
  Test := RecordSaveBase64(F, TypeInfo(TFV), true);
  Check(Test <> '');
  Fill(F, 0);
  Check(RecordLoadBase64(pointer(Test), length(Test), F, TypeInfo(TFV), true));
  Check(RecordEquals(F, AF[100], TypeInfo(TFV)));
  for i := 0 to 1000 do
    with AF[i] do
    begin
      Check(Major = i);
      Check(Minor = i + 1);
      Check(Release = i + 2);
      Check(Build = i + 3);
      Check(Main = IntToString(i + 1000));
      Check(Detailed = IntToString(2000 - i));
      CheckSame(BuildDateTime, BUILDDATETIME);
      Check(BuildYear = i + 2011);
    end;
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    Check(AFP.IndexOf(F) = i);
  end;
  Test := AFP.SaveTo;
  // binary follows the in-memory layout and here Main/Detailed are string
  CheckHash(Test, {$ifdef UNICODE} $080CE771 {$else} $0001179D {$endif}, 'hash32h');
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    AFP.ItemCopy(@F, @F1);
    Check(AFP.ItemEquals(@F, @F1));
    Test2 := AFP.ItemSave(@F);
    Check(length(Test2) > 4);
    AFP.ItemClear(@F);
    AFP.ItemLoad(pointer(Test2), PAnsiChar(Test2) + length(Test2), @F);
    Check(AFP.ItemEquals(@F, @F1));
    Check(AFP.ItemLoadFind(pointer(Test2), PAnsiChar(Test2) + length(Test2)) = i);
  end;
  W.CancelAll;
  W.AddDynArrayJson(AFP);
  // note: error? ensure TTestCoreBase run after TTestLowLevelTypes
  // -> otherwise custom serialization is still active with no Build* fields
  U := W.Text;
  {$ifdef HASEXTRECORDRTTI} // thanks to enhanced RTTI
  Check(IdemPChar(pointer(U), '[{"MAJOR":0,"MINOR":1,"RELEASE":2,"BUILD":3,' +
    '"MAIN":"1000","DETAILED":"2000","BUILDDATETIME":"1999-02-24T02:52:48",' +
    '"BUILDYEAR":2011},{"MAJOR":1,"MINOR":2,"RELEASE":3,"BUILD":4,'));
  CheckHash(U, $74523E0F, 'hash32i');
  {$else}
  CheckEqual(U, '[' + JSON_BASE64_MAGIC_UTF8 + BinToBase64(Test) + '"]');
  {$endif HASEXTRECORDRTTI}
  AFP.Clear;
  Check(AFP.LoadFrom(
    pointer(Test), PAnsiChar(Test) + length(Test)) - pointer(Test) = length(Test));
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    Check(AFP.IndexOf(F) = i);
  end;
  Check(dyniter.Init(TypeInfo(TFVs), Test));
  Check(dyniter.Count = 1001);
  for i := 0 to 1000 do
  begin
    Check(dyniter.Step(@F1));
    Fill(F, i);
    Check(AFP.ItemEquals(@F, @F1));
  end;
  Check(not dyniter.Step(@F1));
  ST := TMemoryStream.Create;
  AFP.SaveToStream(ST);
  AFP.Clear;
  ST.Position := 0;
  AFP.LoadFromStream(ST);
  Check(ST.Position = length(Test));
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    Check(AFP.IndexOf(F) = i);
  end;
  ST.Free;
  AFP.Clear;
  Check(AFP.LoadFromJson(pointer(U)) <> nil);
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    Check(RecordEquals(F, AF[i], AFP.Info.Cache.ItemInfoRaw));
  end;
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    F.BuildYear := 10;
    Check(AFP.IndexOf(F) < 0);
    F.BuildYear := i + 2011;
    F.Detailed := '??';
    Check(AFP.IndexOf(F) < 0);
  end;
  for i := 1000 downto 0 do
    if i and 3 = 0 then
      AFP.Delete(i);
  Check(AFP.Count = 750);
  for i := 0 to 1000 do
  begin
    Fill(F, i);
    if i and 3 = 0 then
      Check(AFP.IndexOf(F) < 0)
    else
      Check(AFP.IndexOf(F) >= 0);
  end;
  Fill(F, 5000);
  AFP.Insert(500, F);
  Check(AFP.IndexOf(F) = 500);
  j := 0;
  for i := 0 to AFP.Count - 1 do
    if i <> 500 then
    begin
      Fill(F, j);
      if j and 3 = 0 then
        Check(AFP.IndexOf(F) < 0)
      else
        Check(AFP.IndexOf(F) >= 0);
      inc(j);
    end;
  Finalize(Index);
  AFP.CreateOrderedIndex(Index, FVSort);
  for i := 1 to AUP.Count - 1 do
    Check(AF[Index[i]].Detailed > AF[Index[i - 1]].Detailed);
  AFP.Compare := FVSort;
  AFP.Sort;
  for i := 1 to AUP.Count - 1 do
    Check(AF[i].Detailed > AF[i - 1].Detailed);
  j := 0;
  for i := 0 to AFP.Count - 1 do
    if i <> 500 then
    begin
      Fill(F, j);
      if j and 3 = 0 then
        Check(AFP.Find(F) < 0)
      else
        Check(AFP.Find(F) >= 0);
      inc(j);
    end;
  W.Free;
  // validate packed record with records of strings inside
  AFP.Init(Typeinfo(TFV2s), AF2);
  for i := 0 to 1000 do
  begin
    Fill(F2.V1, i * 2);
    F2.Value := i;
    Fill(F2.V2, i * 2 + 1);
    F2.Text := IntToString(i);
    Check(AFP.Add(F2) = i);
  end;
  Check(AFP.Count = 1001);
  TestAF2;
  Test := AFP.SaveTo;
  AFP.Clear;
  Check(AFP.Count = 0);
  Check(AFP.LoadFromBinary(Test));
  Check(AFP.Count = 1001);
  TestAF2;
  // validate https://synopse.info/forum/viewtopic.php?pid=16581#p16581
  dp.Modified := Now;
  dp.Data := '1';
  dyn1.Init(TypeInfo(TDataItems), dyn1Array);
  dyn1.Add(dp);
  dp.Modified := Now;
  dp.Data := '2';
  dyn2.Init(TypeInfo(TDataItems), dyn2Array);
  check(dyn2.count = 0);
  dyn2.Add(dp);
  check(length(dyn2Array) = 1);
  check(dyn2.count = 1);
  dyn2.AddArray(dyn1Array);
  check(dyn2.count = 2);
  check(dyn2.ItemEquals(@dyn2Array[0], @dp));
  check(dyn2.ItemEquals(@dyn2Array[1], @dyn1Array[0]));
  dyn2.AddDynArray(@dyn1);
  check(dyn2.count = 3);
  check(dyn2.ItemEquals(@dyn2Array[0], @dp));
  check(dyn2.ItemEquals(@dyn2Array[1], @dyn1Array[0]));
  check(dyn2.ItemEquals(@dyn2Array[2], @dyn1Array[0]));
  // valide generic-like features
  // see http://docwiki.embarcadero.com/CodeExamples/en/Generics_Collections_TDictionary_(Delphi)
  ACities.Init(TypeInfo(TCityDynArray), Province.Cities);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  ACities.Add(City);
  City.Name := 'London';
  City.Country := 'United Kingdom';
  City.Latitude := 51.5;
  City.Longitude := -0.17;
  ACities.Add(City);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := 0;
  City.Longitude := 0;
  ACities.Add(City);
  Check(ACities.Count = 3);
  ACities.Compare := SortDynArrayString; // will search by Name = 1st field
  City.Name := 'Iasi';
  Check(ACities.FindAndFill(City) = 0);
  Check(City.Name = 'Iasi');
  Check(City.Country = 'Romania');
  CheckSame(City.Latitude, 47.16);
  CheckSame(City.Longitude, 27.58);
  Check(ACities.FindAndDelete(City) = 0);
  Check(City.Name = 'Iasi');
  Check(ACities.Find(City) < 0);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := -34.6;
  City.Longitude := -58.45;
  Check(ACities.FindAndUpdate(City) >= 0);
  City.Latitude := 0;
  City.Longitude := 0;
  Check(City.Name = 'Buenos Aires');
  Check(ACities.FindAndFill(City) >= 0);
  CheckSame(City.Latitude, -34.6);
  CheckSame(City.Longitude, -58.45);
  Check(ACities.FindAndAddIfNotExisting(City) >= 0);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  Check(ACities.FindAndAddIfNotExisting(City) < 0);
  Check(City.Name = 'Iasi');
  Check(ACities.FindAndUpdate(City) >= 0);
  ACities.Sort;
  for i := 1 to high(Province.Cities) do
    Check(Province.Cities[i].Name > Province.Cities[i - 1].Name);
  Check(ACities.Count = 3);
  // complex record test
  Province.Name := 'Test';
  Province.Comment := 'comment';
  Province.Year := 1000;
  Test := RecordSave(Province, TypeInfo(TProvince));
  FastRecordClear(@Province, TypeInfo(TProvince));
  Check(Province.Name = '');
  Check(Province.Comment = '');
  Check(length(Province.Cities) = 0);
  Check(ACities.Count = 0);
  Province.Year := 0;
  PA := RecordLoad(Province, pointer(Test), TypeInfo(TProvince), nil,
    PAnsiChar(pointer(Test)) + length(Test));
  Check((PA <> nil) and (PA^ = #0));
  Check(Province.Name = 'Test');
  Check(Province.Comment = 'comment');
  Check(Province.Year = 1000);
  Check(length(Province.Cities) = 3);
  Check(ACities.Count = 3);
  for i := 1 to high(Province.Cities) do
    Check(Province.Cities[i].Name > Province.Cities[i - 1].Name);
  Province.Cities := nil;
  Test := RecordSave(Province, TypeInfo(TProvince));
  FastRecordClear(@Province, TypeInfo(TProvince));
  Check(Province.Name = '');
  Check(Province.Comment = '');
  Check(length(Province.Cities) = 0);
  Check(ACities.Count = 0);
  Check(RecordLoad(Province, Test, TypeInfo(TProvince)));
  Check(Province.Name = 'Test');
  Check(Province.Comment = 'comment');
  Check(Province.Year = 1000);
  Check(length(Province.Cities) = 0);
  Check(ACities.Count = 0);
  // big array test
  ACities.Init(TypeInfo(TCityDynArray), Province.Cities);
  ACities.Clear;
  for i := 0 to 10000 do
  begin
    City.Name := IntToString(i);
    City.Latitude := i * 3.14;
    City.Longitude := i * 6.13;
    Check(ACities.Add(City) = i);
  end;
  Check(ACities.Count = Length(Province.Cities));
  Check(ACities.Count = 10001);
  TestCities;
  count := 7;
  ACities.Init(TypeInfo(TCityDynArray), Province.Cities, @count);
  CheckEqual(count, 0);
  for i := 0 to 100000 do
  begin
    City.Name := IntToString(i);
    City.Latitude := i * 3.14;
    City.Longitude := i * 6.13;
    Check(ACities.Add(City) = i);
  end;
  CheckEqual(count, 100001);
  CheckEqual(ACities.Count, count);
  TestCities;
end;

function BufEquals(P, n, b: PtrInt): boolean;
begin
  // slower than FillChar, faster than for loop, but fast enough for testing
  result := false;
  {$ifdef CPU32}
  b := b * $01010101;
  {$else}
  b := b * $0101010101010101;
  {$endif CPU32}
  inc(n, P - SizeOf(P));
  if n >= P then
    repeat
      if PPtrInt(P)^ <> b then
        exit;
      inc(PPtrInt(P));
    until n < P;
  inc(n, SizeOf(P));
  if P < n then
    repeat
      if PByte(P)^ <> byte(b) then
        exit;
      inc(P);
    until P >= n;
  result := true;
end;

function IsBufIncreasing(P: PByteArray; n: PtrInt; b: byte): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to n - 1 do
    if P[i] <> b then
      exit
    else
      inc(b);
  result := true;
end;

procedure TTestCoreBase.CustomRTL;
// note: mormot.core.os.posix.inc redirects FillCharFast/MoveFast to the libc
var
  buf: RawByteString;

  procedure Validate(rtl: boolean);
  var
    i, len, filled, moved: PtrInt;
    b1, b2: byte;
    timer: TPrecisionTimer;
    P: PByteArray;
    msg: string;
    {$ifdef ASMX64}
    cputxt: RawUtf8;
    {$endif ASMX64}
    elapsed: Int64;
  begin
    // first validate FillCharFast
    b1 := 0;
    len := 1;
    repeat
      b2 := (b1 + 1) and 255;
      buf[len + 1] := AnsiChar(b1);
      if rtl then
        FillChar(pointer(buf)^, len, b2)
      else
        FillCharFast(pointer(buf)^, len, b2);
      Check(BufEquals(PtrInt(buf), len, b2));
      Check(ord(buf[len + 1]) = b1);
      b1 := b2;
      if len < 16384 then
        inc(len)
      else
        inc(len, 777 + len shr 4);
    until len >= length(buf);
    // benchmark FillChar/FillCharFast
    {$ifdef ASMX64}
    cputxt := GetSetName(TypeInfo(TX64CpuFeatures), X64CpuFeatures);
    {$endif ASMX64}
    if rtl then
      msg := 'FillChar'
    else
      {$ifdef ASMX64}
      FormatString('FillCharFast [%]', [{%H-}cputxt], msg);
      {$else}
      msg := 'FillCharFast';
      {$endif ASMX64}
    // now make the same test with no Check() but with timing
    // small len makes timer.Resume/Pause unreliable -> single shot measure
    b1 := 0;
    len := 1;
    filled := 0;
    timer.Start;
    repeat
      b2 := (b1 + 1) and 255;
      if rtl then
        FillChar(pointer(buf)^, len, b2)
      else
        FillCharFast(pointer(buf)^, len, b2);
      inc(filled, len);
      b1 := b2;
      if len < 16384 then
        inc(len)
      else
        inc(len, 777 + len shr 4);
    until len >= length(buf);
    NotifyTestSpeed(msg, 1, filled, @timer);
    // validate negative count of Move/MoveFast (should not make any GPF)
    if rtl then
      move(buf[1], buf[2], -100)
    else
      moveFast(buf[1], buf[2], -100);
    // validates overlapping forward Move/MoveFast
    if rtl then
      msg := 'Move'
    else
      {$ifdef ASMX64}
      FormatString('MoveFast [%]', [{%H-}cputxt], msg);
      {$else}
      msg := 'MoveFast';
      {$endif ASMX64}
    P := pointer(buf);
    for i := 0 to length(buf) - 1 do
      P[i] := i; // fills with 0,1,2,...
    Check(IsBufIncreasing(P, length(buf), 0));
    timer.Start;
    for i := 1 to 20 do     
    begin
      len := 1;
      moved := 0;
      repeat
        if rtl then
          Move(P[moved + 1], P[moved], len)
        else
          MoveFast(P[moved + 1], P[moved], len);
        inc(moved, len);
        Check(P[moved] = P[moved - 1]);
        inc(len);
      until moved + len >= length(buf);
    end;
    NotifyTestSpeed(msg, 1, moved * 20, @timer);
    CheckHash(buf, $813F6468);
    // forward and backward overlapped moves on small buffers
    elapsed := 0;
    moved := 0;
    for len := 1 to 48 do
    begin
      timer.Start;
      if rtl then
        for i := 1 to 50000 do
        begin
          Move(P[100], P[i], len);
          Move(P[i], P[100], len);
        end
      else
        for i := 1 to 50000 do
        begin
          MoveFast(P[100], P[i], len);
          MoveFast(P[i], P[100], len);
        end;
      inc(moved, 100000 * len);
      inc(elapsed, NotifyTestSpeed('%b %', [len, msg], 1, 100000 * len, @timer,
        {onlylog=}true));
    end;
    timer.FromExternalMicroSeconds(elapsed);
    NotifyTestSpeed('small %', [msg], 1, moved, @timer);
    CheckHash(buf, $DBB1A444);
    // forward and backward non-overlapped moves on big buffers
    len := (length(buf) - 3200) shr 1;
    timer.Start;
    for i := 1 to 25 do
      if rtl then
      begin
        Move(P[len], P[i], len - i * 10);
        Move(P[i], P[len], len - i * 10);
      end
      else
      begin
        MoveFast(P[len], P[i], len - i * 10);
        MoveFast(P[i], P[len], len - i * 10);
      end;
    NotifyTestSpeed('big %', [msg], 1, 50 * len, @timer);
    CheckHash(buf, $88D61C65);
    // forward and backward overlapped moves on big buffers
    len := length(buf) - 3200;
    for i := 1 to 3 do
      if rtl then
      begin
        Move(P[3100], P[i], len - i);
        Move(P[i], P[3200], len - i);
      end
      else
      begin
        MoveFast(P[3100], P[i], len - i);
        MoveFast(P[i], P[3200], len - i);
      end;
    CheckHash(buf, $B49DB8A5);
  end;

{$ifdef ASMX64}
var
  bak, cpu: TX64CpuFeatures;
{$endif ASMX64}
begin
  Check(FileIsExecutable(Executable.ProgramFileName));
  Check(not FileIsExecutable(Executable.ProgramFilePath));
  SetLength(buf, 16 shl 20); // 16MB
  {$ifdef ASMX64} // activate and validate SSE2 + AVX branches
  bak := X64CpuFeatures;
  cpu := bak - [cpuHaswell, cpuAvx2];
  X64CpuFeatures := []; // default SSE2 128-bit process
  Validate({rtl=}false);
  {$ifdef ASMX64AVXNOCONST} // oldest Delphi doesn't support AVX asm
  if cpuAvx in cpu then
  begin
    X64CpuFeatures := [cpuAvx]; // AVX 256-bit process
    Validate(false);
  end;
  {$endif ASMX64AVXNOCONST}
  X64CpuFeatures := bak; // there is no AVX move/fillchar (still 256-bit wide)
  if (cpu <> []) and
     (cpu <> [cpuAvx]) then
    Validate(false);
  // no Validate(true): RedirectCode(@System.FillChar,@FillcharFast)
  {$else}
  Validate(true);
  Validate(false);
  {$endif ASMX64}
end;

type
  TR = record
    One: integer;
    S1: AnsiString;
    Three: byte;
    S2: WideString;
    Five: boolean;
    v: Variant;
    R: Int64Rec;
    Arr: array[0..10] of AnsiString;
    Dyn: array of integer;
    Bulk: array[0..19] of byte;
  end;

  TLicenseData = record
    CustomerNum: Integer;
    CustomerName: RawUtf8;
    CustomerAddress: RawUtf8;
    LicenceDate: TDate;
    ProductName: RawUtf8;
  end;

  TEnum = (e0, e1, e2, e3, e4);

  TPeople2 = class(TSynPersistent)
  private
    fFirstName: string;
    fLastName: RawUtf8;
    fYearOfBirth: Int64;
    fYearOfDeath: integer;
    fEnum: TEnum;
    function GetEnum: TEnum;
    procedure SetEnum(const Value: TEnum);
  published // properties are in another order
    property YearOfBirth: Int64
      read fYearOfBirth write fYearOfBirth;
    property LastName: RawUtf8
      read fLastName write fLastName;
    property FirstName: string
      read fFirstName write fFirstName;
    property YearOfDeath: integer
      read fYearOfDeath write fYearOfDeath;
    property Enum: TEnum
      read GetEnum write SetEnum;
  end;

  TPeopleR = packed record
    LastName, FirstName: RawUtf8;
    YearOfBirth, Unused: integer;
    Enum: TEnum;
  end;

function TPeople2.GetEnum: TEnum;
begin
  result := fEnum;
end;

procedure TPeople2.SetEnum(const Value: TEnum);
begin
  fEnum := Value;
end;

procedure TTestCoreBase._Records;
var
  A, B, C: TR;
  i, j: PtrInt;
  lic: TLicenseData;
  o1: TOrmPeople;
  o2: TPeople2;
  r: TPeopleR;
  p: TRecordPeople;
  m: TRttiMap;
  fo, fr: TRttiFilter;
  err, err2: string;
begin
  // FillZeroRtti()
  CheckEqual(lic.CustomerName, '');
  lic.CustomerName := 'Toto';
  FillZeroRtti(TypeInfo(TLicenseData), lic);
  CheckEqual(lic.CustomerName, '');
  lic.CustomerName := '1234';
  FillZeroRtti(TypeInfo(TLicenseData), lic);
  CheckEqual(lic.CustomerName, '');
  // validate RecordCopy()
  FillCharFast(A, SizeOf(A), 0);
  FillCharFast(B, SizeOf(B), 0);
  FillCharFast(C, SizeOf(C), 0);
  for i := 0 to High(A.Bulk) do
    A.Bulk[i] := i;
  A.S1 := 'one';
  A.S2 := 'two';
  A.Five := true;
  A.Three := $33;
  A.V := 'One Two';
  A.R.Lo := 10;
  A.R.Hi := 20;
  A.Arr[5] := 'five';
  SetLength(A.Dyn, 10);
  A.Dyn[9] := 9;
  RecordCopy(B, A, TypeInfo(TR)); // mORMot 2 doesn't overload RecordCopy()
  Check(A.One = B.One);
  Check(A.S1 = B.S1);
  Check(A.Three = B.Three);
  Check(A.S2 = B.S2);
  Check(A.Five = B.Five);
  Check(A.V = B.V);
  Check(Int64(A.R) = Int64(B.R));
  Check(A.Arr[5] = B.Arr[5]);
  Check(A.Arr[0] = B.Arr[0]);
  Check(A.Dyn[9] = B.Dyn[9]);
  Check(A.Dyn[0] = 0);
  for i := 0 to High(B.Bulk) do
    Check(B.Bulk[i] = i);
  for i := -10 to 0 do
    Check(CompareMem(@A.Bulk, @B.Bulk, i));
  for i := 0 to High(B.Bulk) do
  begin
    for j := 0 to i - 1 do
    begin
      inc(B.Bulk[j]); // validate each byte modification
      Check(not CompareMem(@A.Bulk, @B.Bulk, i));
      Check(not CompareMemSmall(@A.Bulk, @B.Bulk, i));
      Check(not CompareMemFixed(@A.Bulk, @B.Bulk, i));
      dec(B.Bulk[j]);
    end;
    Check(CompareMem(@A.Bulk, @B.Bulk, i));
    Check(CompareMemSmall(@A.Bulk, @B.Bulk, i));
    Check(CompareMemFixed(@A.Bulk, @B.Bulk, i));
  end;
  for i := 0 to High(B.Bulk) do
    Check(CompareMemSmall(@A.Bulk, @B.Bulk, i));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemFixed(@A.Bulk, @B.Bulk, i));
  FillCharFast(A.Bulk, SizeOf(A.Bulk), 255);
  for i := 0 to High(B.Bulk) do
    Check(CompareMem(@A.Bulk, @B.Bulk, i) = (i = 0));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemSmall(@A.Bulk, @B.Bulk, i) = (i = 0));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemFixed(@A.Bulk, @B.Bulk, i) = (i = 0));
  B.Three := 3;
  B.Dyn[0] := 10;
  RecordCopy(C, B, TypeInfo(TR)); // mORMot 2 doesn't overload RecordCopy()
  CheckEqual(A.One, C.One);
  Check(A.S1 = C.S1);
  CheckEqual(C.Three, 3);
  Check(A.S2 = C.S2);
  Check(A.Five = C.Five);
  Check(A.v = C.v);
  Check(Int64(A.R) = Int64(C.R));
  Check(A.Arr[5] = C.Arr[5]);
  Check(A.Arr[0] = C.Arr[0]);
  CheckEqual(A.Dyn[9], C.Dyn[9]);
  {Check(A.Dyn[0]=0) bug in original VCL?}
  CheckEqual(C.Dyn[0], 10);
  // TPeople2 <--> TOrmPeople class mapping
  o1 := TOrmPeople.Create;
  o2 := TPeople2.Create;
  try
    o1.FirstName := 'toto';
    o1.LastName := 'titi';
    o1.YearOfBirth := 1926;
    o1.YearOfDeath := 2010;
    CopyObject(o1, o2);
    CheckEqual(o1.FirstName, 'toto');
    Check(o2.FirstName = 'toto');
    CheckEqual(o1.LastName, 'titi');
    CheckEqual(o1.LastName, o2.LastName);
    CheckEqual(o1.YearOfBirth, o2.YearOfBirth);
    CheckEqual(o1.YearOfDeath, o2.YearOfDeath);
    // TRecordPeople <--> TOrmPeople record/class mapping
    p.YearOfBirth := -1;
    CheckEqual(p.YearOfBirth, -1);
    RecordZero(@p, TypeInfo(TRecordPeople));
    CheckEqual(p.FirstName, '');
    CheckEqual(p.LastName, '');
    CheckEqual(p.YearOfBirth, 0);
    CheckEqual(p.YearOfDeath, 0);
    ObjectToRecord(o2, p, TypeInfo(TRecordPeople));
    CheckEqual(p.FirstName, 'toto');
    CheckEqual(p.LastName, 'titi');
    CheckEqual(p.YearOfBirth, o2.YearOfBirth);
    CheckEqual(p.YearOfDeath, o2.YearOfDeath);
    o2.Enum := e1;
    ClearObject(o2);
    Check(o2.FirstName = '');
    CheckEqual(o2.LastName, '');
    CheckEqual(o2.YearOfBirth, 0);
    CheckEqual(o2.YearOfDeath, 0);
    Check(o2.Enum = e0);
    RecordToObject(p, o2, TypeInfo(TRecordPeople));
    Check(o2.FirstName = 'toto');
    CheckEqual(o2.LastName, 'titi');
    CheckEqual(o2.YearOfBirth, p.YearOfBirth);
    CheckEqual(o2.YearOfDeath, p.YearOfDeath);
    // TPeopleR <--> TOrmPeople record/class mapping
    o2.Enum := e4;
    {$ifndef HASEXTRECORDRTTI} // oldest Delphi or FPC
    Rtti.RegisterType(TypeInfo(TEnum));
    Rtti.RegisterFromText(TypeInfo(TPeopleR),
      'LastName,FirstName:RawUtf8 YearOfBirth,Unused:integer Enum:TEnum');
    {$endif HASEXTRECORDRTTI}
    r.YearOfBirth := -1;
    CheckEqual(r.YearOfBirth, -1);
    RecordZero(@r, TypeInfo(TPeopleR));
    CheckEqual(r.FirstName, '');
    CheckEqual(r.LastName, '');
    CheckEqual(r.YearOfBirth, 0);
    CheckEqual(r.Unused, 0);
    Check(r.Enum = e0);
    ObjectToRecord(o2, r, TypeInfo(TPeopleR));
    CheckEqual(r.FirstName, 'toto');
    CheckEqual(r.LastName, 'titi');
    CheckEqual(r.YearOfBirth, o2.YearOfBirth);
    CheckEqual(r.Unused, 0);
    Check(r.Enum = e4);
    ClearObject(o2);
    Check(o2.FirstName = '');
    CheckEqual(o2.LastName, '');
    CheckEqual(o2.YearOfBirth, 0);
    CheckEqual(o2.YearOfDeath, 0);
    Check(o2.Enum = e0);
    RecordToObject(r, o2, TypeInfo(TPeopleR));
    Check(o2.FirstName = 'toto');
    CheckEqual(o2.LastName, 'titi');
    CheckEqual(o2.YearOfBirth, r.YearOfBirth);
    CheckEqual(o2.YearOfDeath, 0);
    Check(o2.Enum = e4);
    // TPeople2 <--> TPeopleR class/record mapping with TRttiMap
    m.Init(TPeople2, TypeInfo(TPeopleR)).AutoMap;
    RecordZero(@r, TypeInfo(TPeopleR));
    CheckEqual(r.FirstName, '');
    CheckEqual(r.LastName, '');
    CheckEqual(r.YearOfBirth, 0);
    CheckEqual(r.Unused, 0);
    Check(r.Enum = e0);
    m.ToB(o2, @r); // from class to DTO
    CheckEqual(r.FirstName, 'toto');
    CheckEqual(r.LastName, 'titi');
    CheckEqual(r.YearOfBirth, o2.YearOfBirth);
    CheckEqual(r.Unused, 0);
    Check(r.Enum = e4);
    // TPeople2 <--> TPeopleR class/record custom fields mapping with TRttiMap
    m.Init(TPeople2, TypeInfo(TPeopleR)).Map([
      'firstName',   'lastname', // inverted
      'lastname',    'firstName',
      'YearOfBirth', 'Unused']); // moved
    RecordZero(@r, TypeInfo(TPeopleR));
    CheckEqual(r.FirstName, '');
    CheckEqual(r.LastName, '');
    CheckEqual(r.YearOfBirth, 0);
    CheckEqual(r.Unused, 0);
    m.ToB(o2, @r); // from class to DTO
    CheckEqual(r.LastName, 'toto');
    CheckEqual(r.FirstName, 'titi');
    CheckEqual(r.YearOfBirth, 0);
    CheckEqual(r.Unused, o2.YearOfBirth);
    Check(r.Enum = e0);
    // TOrmPeople <--> TRecordPeople class/record fields mapping with TRttiMap
    m.Init(TOrmPeople, TypeInfo(TRecordPeople)).AutoMap;
    CheckEqual(p.FirstName, 'toto');
    CheckEqual(p.LastName, 'titi');
    CheckEqual(p.YearOfBirth, o1.YearOfBirth);
    CheckEqual(p.YearOfDeath, o1.YearOfDeath);
    o1.Free;
    o1 := m.ToA(@p); // from DTO to class
    CheckEqual(o1.FirstName, 'toto');
    CheckEqual(o1.LastName, 'titi');
    CheckEqual(p.YearOfBirth, o1.YearOfBirth);
    CheckEqual(p.YearOfDeath, o1.YearOfDeath);
    // TRttiFilter validation with o1 TOrmPeople instance
    fo := TRttiFilter.Create(o1.ClassType);
    try
      CheckEqual(fo.Count, 0);
      fo.Filter(nil);
      fo.Filter(o1);
      CheckEqual(fo.Count, 0);
      Check(fo.Validate(nil) = '');
      Check(fo.Validate(o1) = '');
      fo.Add('firstname', [TSynValidateNonVoidText.Create]);
      CheckEqual(fo.Count, 1);
      err := '???';
      err := fo.Validate(nil);
      Check(err = '', err);
      CheckEqual(o1.FirstName, 'toto');
      Check(fo.Validate(o1) = '');
      o1.FirstName := '';
      Check(fo.Validate(nil) = '');
      err2 := fo.Validate(o1);
      Check(err2 = 'FirstName: Expect at least 1 character', err2);
    finally
      fo.Free;
    end;
    // TRttiMap.RandomA/B and Compare methods - useful e.g. for testing DTOs
    m.Map('id', 'rowid');
    CheckEqual(o1.IDValue, 0, 'o1id1');
    CheckEqual(p.RowID, 0, 'pRowID1');
    for i := 1 to 100 do
    begin
      o1.IDValue := 0;
      CheckEqual(o1.IDValue, 0, 'o1id');
      p.RowID := 0;
      CheckEqual(p.RowID, 0, 'pRowID2');
      m.RandomA(o1);
      if o1.IDValue = 0 then
        m.RandomA(o1); // 1 chance over 2^64 - but not twice = 2^128
      CheckNotEqual(o1.IDValue, 0, 'rndo1id');
      CheckNotEqual(m.Compare(o1, @p), 0, 'rndA');
      CheckEqual(p.RowID, 0, 'pRowID3');
      m.ToB(o1, @p);
      CheckNotEqual(p.RowID, 0, 'pRowID4');
      CheckEqual(p.RowID, o1.IDValue, 'ids1');
      CheckEqual(m.Compare(o1, @p), 0, 'cmpA');
      p.RowID := 0;
      m.RandomB(@p);
      if p.RowID = 0 then
        m.RandomB(@p); // 1 chance over 2^64 - but not twice = 2^128
      CheckNotEqual(p.RowID, 0, 'pRowID4');
      CheckNotEqual(m.Compare(o1, @p), 0, 'rndB');
      m.ToA(o1, @p);
      CheckEqual(m.Compare(o1, @p), 0, 'cmpB');
      CheckEqual(p.RowID, o1.IDValue, 'ids2');
    end;
  finally
    o1.Free;
    o2.Free;
  end;
  // TRttiFilter validation with p record
  fr := TRttiFilter.Create(TypeInfo(TRecordPeople));
  try
    CheckEqual(fr.Count, 0);
    fr.Filter(nil);
    p.FirstName := 'toto'; // reset the expected values
    p.LastName := 'titi';
    fr.Filter(@p);
    Check(fr.Validate(@p) = '');
    fr.AddClass('firstName', [TSynFilterUpperCase, TSynValidateNonVoidText]);
    CheckEqual(fr.Count, 2);
    CheckEqual(p.FirstName, 'toto');
    Check(fr.Validate(@p) = '');
    CheckEqual(p.FirstName, 'toto');
    Check(fr.Apply(@p) = '');
    CheckEqual(p.FirstName, 'TOTO');
    p.FirstName := '';
    err := fr.Validate(@p);
    Check(err = err2, err);
    err := fr.Apply(@p);
    Check(err = err2, err);
    CheckEqual(fr.Count, 2);
    fr.Clear;
    CheckEqual(fr.Count, 0);
    Check(fr.Validate(@p) = '');
    CheckEqual(p.FirstName, '');
    Check(fr.Apply(@p) = '');
    CheckEqual(p.FirstName, '');
    fr.AddClass('firstName', [TSynFilterLowerCase, TSynValidateNonVoidText]);
    CheckEqual(fr.Count, 2);
    fr.AddClass('lastNAME', [TSynValidateNonVoidText]);
    CheckEqual(fr.Count, 3);
    err := fr.Apply(@p);
    Check(err = err2, err);
    p.FirstName := 'TOTO';
    Check(fr.Validate(@p) = '');
    CheckEqual(p.FirstName, 'TOTO');
    Check(fr.Apply(@p) = '');
    CheckEqual(p.FirstName, 'toto');
    p.LastName := '';
    err := fr.Apply(@p);
    Check(err = 'LastName: Expect at least 1 character', err);
    p.FirstName := '';
    err := fr.Apply(@p);
    Check(err = err2, err);
  finally
    fr.Free;
  end;
end;

procedure TTestCoreBase._TSynList;
const
  MAX = 1000;
var
  {$ifdef FPC}
  p: TOrm; // FPC can iterate over pointers as class instances but not Delphi :(
  {$else}
  p: pointer;
  {$endif FPC}
  i: PtrInt;
  l: TSynList;
begin
  l := TSynList.Create;
  try
   CheckEqual(l.Count, 0);
   Check(not l.Exists(nil));
   Check(l.IndexOf(nil) < 0);
   for i := 0 to MAX - 1 do
     CheckEqual(l.Add(pointer(i)), i);
   CheckEqual(l.Count, MAX);
   Check(l.Exists(nil));
   for i := 0 to MAX - 1 do
     Check(l[i] = pointer(i));
   CheckEqual(l.IndexOf(nil), 0);
   p := l[MAX - 1];
   CheckEqual(l.IndexOf(p), MAX - 1);
   {$ifdef HASITERATORS}
   i := 0;
   for p in l do
   begin
     CheckEqual(PtrInt(p), i);
     inc(i);
   end;
   {$endif HASITERATORS}
  finally
    l.Free;
  end;
end;

procedure TTestCoreBase._GUID;
var
  i, j: integer;
  s, x, x2: RawUtf8;
  st, st2: string;
  g, g2: TGuid;
  h, h2: THash512Rec;
  pt: TRttiParserType;
const
  Guid: TGuid = '{c9a646d3-9c61-4cb7-bfcd-ee2522c8f633}';
begin
  CheckEqual(BitsToBytes(0), 0);
  for i := 1 to 8 do
    CheckEqual(BitsToBytes(i), 1);
  for i := 9 to 15 do
    CheckEqual(BitsToBytes(i), 2);
  s := GuidToRawUtf8(Guid);
  CheckEqual(s, '{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
  Check(s[16] = '4');
  Check(s[21] in ['8', '9', 'A', 'B']);
  Check(IsRandomGuid(@Guid));
  Check(TextToGuid(@s[2], @g2)^ = '}');
  Check(IsEqualGuid(g2, Guid));
  Check(GuidToString(Guid) = '{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
  Check(IsEqualGuid(RawUtf8ToGuid(s), Guid));
  Check(TrimGuid(s));
  CheckEqual(s, 'c9a646d39c614cb7bfcdee2522c8f633');
  FillZero(g);
  CheckEqual(GuidArrayToCsv([]), '');
  CheckEqual(GuidArrayToCsv([g]), '00000000-0000-0000-0000-000000000000');
  CheckEqual(GuidArrayToCsv([Guid, g, g2]),
    'C9A646D3-9C61-4CB7-BFCD-EE2522C8F633,00000000-0000-0000-0000-000000000000,' +
    'C9A646D3-9C61-4CB7-BFCD-EE2522C8F633');
  CheckEqual(MacTextFromHex(''), '');
  CheckEqual(MacTextFromHex('1'), '');
  CheckEqual(MacTextFromHex('12'), '12');
  CheckEqual(MacTextFromHex('123'), '');
  CheckEqual(MacTextFromHex('1234'), '12:34');
  CheckEqual(MacTextFromHex('12345'), '');
  ToHumanHex(x, @Guid, SizeOf(guid));
  CheckEqual(x, 'd3:46:a6:c9:61:9c:b7:4c:bf:cd:ee:25:22:c8:f6:33');
  CheckEqual(MacTextFromHex(mormot.core.text.BinToHex(@Guid, SizeOf(guid))), x);
  ToHumanHex(x, @Guid, SizeOf(guid), {reverse=}true);
  CheckEqual(x, '33:f6:c8:22:25:ee:cd:bf:4c:b7:9c:61:c9:a6:46:d3');
  x := 'c9:a6:46:d3:9c:61:4c:b7:bf:cd:ee:25:22:c8:f6:33';
  CheckEqual(MacTextFromHex(s), x);
  CheckEqual(MacTextFromHex(UpperCase(s)), x);
  CheckEqual(HumanHexCompare(x, x), 0);
  CheckEqual(HumanHexCompare(x, MacTextFromHex(s)), 0);
  for i := 1 to 100 do
  begin
    x2 := x;
    delete(x2, rnd.Next(length(x2)) + 1, 2);
    Check(x <> x2);
    Check(HumanHexCompare(x, x2) <> 0);
    HumanHexCompare(x, x2);
  end;
  for i := 1 to 100 do
  begin
    x2 := x;
    j := rnd.Next(length(x2)) + 1;
    delete(x2, j, 1);
    Check(x <> x2);
    Check((HumanHexCompare(x, x2) = 0) = (x[j] = ':'));
  end;
  x2 := x;
  repeat
    i := PosExChar(':', x2);
    if i = 0 then
      break;
    delete(x2, i, 1);
    CheckEqual(HumanHexCompare(x, x2), 0);
  until false;
  delete(x2, 10, 2);
  Check(HumanHexCompare(x, x2) <> 0);
  s := s + s; // validates also our patched RTL
  CheckEqual(HumanHexCompare(s, s), 0);
  repeat
    i := rnd.Next(length(s)) + 1;
    delete(s, i, 1);
    Check(TrimGuid(s) = (length(s) = 32));
  until s = '';
  s := '   ';
  Check(not TrimGuid(s));
  CheckEqual(s, '');
  Check(not TrimGuid(s));
  CheckEqual(s, '');
  s := 'C9A646D3-9C61-4CB7-BFCD-EE2522C8F633';
  Check(IsEqualGuid(RawUtf8ToGuid(s), Guid));
  Check(TrimGuid(s));
  CheckEqual(s, 'c9a646d39c614cb7bfcdee2522c8f633');
  s := 'C9A646D39C614CB7BFCDEE2522C8F633';
  Check(IsEqualGuid(RawUtf8ToGuid(s), Guid));
  Check(TrimGuid(s));
  CheckEqual(s, 'c9a646d39c614cb7bfcdee2522c8f633');
  Check(TrimGuid(s));
  CheckEqual(s, 'c9a646d39c614cb7bfcdee2522c8f633');
  s[3] := 'Z';
  Check(not TrimGuid(s));
  CheckEqual(s, 'c9Z646d39c614cb7bfcdee2522c8f633');
  s := '   1234 678 --';
  Check(not TrimGuid(s));
  CheckEqual(s, '1234678');
  for i := 1 to 1000 do
  begin
    if i and 1 = 0 then
      RandomGuid(g)
    else
      TAesPrng.Main.FillGuid(g);
    Check(IsRandomGuid(@g));
    st := GuidToString(g);
    Check(st <> st2);
    st2 := SysUtils.GuidToString(g);
    Check(st = st2);
    Check(IsEqualGuid(StringToGuid(st), g));
    s := GuidToRawUtf8(g);
    Check(s[16] = '4');
    Check(s[21] in ['8', '9', 'A', 'B']);
    Check(st = mormot.core.unicode.Utf8ToString(s));
    st[rnd.Next(38) + 1] := ' ';
    g2 := StringToGuid(st);
    Check(IsZero(@g2, SizeOf(g2)));
    Check(TextToGuid(@s[2], @g2)^ = '}');
    Check(IsEqualGuid(g2, g));
    Check(IsEqualGuid(@g2, @g));
    Check(TrimGuid(s));
    CheckEqual(length(s), 32);
    Check(IsEqualGuid(RawUtf8ToGuid(s), g));
    inc(g.D1);
    Check(not IsEqualGuid(g2, g));
    Check(not IsEqualGuid(RawUtf8ToGuid(s), g));
  end;
  // oldest Delphi can't compile TypeInfo(TGuid) -> use PT_INFO[ptGuid]
  s := RecordSaveJson(g, PT_INFO[ptGuid]);
  FillCharFast(g2, SizeOf(g2), 0);
  Check(RecordLoadJsonInPlace(g2, pointer(s), PT_INFO[ptGuid]) <> nil);
  Check(IsEqualGuid(g2, g));
  FillCharFast(h, SizeOf(h), 1);
  for pt := ptGuid to ptHash512 do
  begin
    rnd.Fill(@h, PT_SIZE[pt]);
    s := SaveJson(h, PT_INFO[pt]); // ptHash* are not record types
    CheckUtf8(TextToVariantNumberType(pointer(s)) = varString,
      '%:%', [PT_INFO[pt].RawName, s]);
    FillCharFast(h2, SizeOf(h2), 0);
    Check(LoadJsonInPlace(h2, pointer(s), PT_INFO[pt]) <> nil);
    CheckUtf8(CompareMem(@h, @h2, PT_SIZE[pt]), '%', [PT_INFO[pt].RawName]);
  end;
end;

procedure TTestCoreBase._ParseCommandArgs;

  procedure Test(const cmd: RawUtf8; const expected: array of RawUtf8;
     const flags: TParseCommands = []; posix: boolean = true);
  var
    tmp: RawUtf8;
    n, i: integer; // integer, not PtrInt
    a: TParseCommandsArgs;
    p: TRawUtf8DynArray;
  begin
    if CheckFailed(ParseCommandArgs(cmd, nil, nil, nil, posix) = flags) then
      exit;
    FillcharFast(a, SizeOf(a), 255); // ensure a[n]<>nil
    Check(ParseCommandArgs(cmd, @a, @n, @tmp, posix) = flags);
    if (flags <> []) or
       CheckFailed(n = length(expected)) then
      exit;
    for i := 0 to n - 1 do
      Check(StrComp(pointer(a[i]), pointer(expected[i])) = 0);
    Check(a[n] = nil, 'last param should be nil');
    Check(ExtractCommandArgs(cmd, p, posix) = flags);
    if not CheckFailed(n = length(p)) then
      for i := 0 to n - 1 do
        CheckEqual(p[i], expected[i]);
  end;

begin
  Test('', [], [pcInvalidCommand]);
  Test('one', ['one']);
  Test('o', ['o']);
  Test(' o', ['o']);
  Test('one two', ['one', 'two']);
  Test('    one     two    ', ['one', 'two']);
  Test('"one" two', ['one', 'two']);
  Test('one "two"', ['one', 'two']);
  Test('one     "two"', ['one', 'two']);
  Test('one " two"', ['one', ' two']);
  Test('" one" two', [' one', 'two']);
  Test(''' one'' two', [' one', 'two']);
  Test('"one one" two', ['one one', 'two']);
  Test('one "two two"', ['one', 'two two']);
  Test('"1  2"    "3    4"', ['1  2', '3    4']);
  Test('"1 '' 2"    "3    4"', ['1 '' 2', '3    4']);
  Test('''1  2''    "3    4"', ['1  2', '3    4']);
  Test('1 ( "3    4"', [], [pcHasParenthesis]);
  Test('1 "3  "  4"', [], [pcUnbalancedDoubleQuote]);
  Test(''' "3  4"', [], [pcUnbalancedSingleQuote]);
  Test('one|two', [], [pcHasRedirection]);
  Test('one\|two', ['one|two'], []);
  Test('"one|two"', ['one|two']);
  Test('one>two', [], [pcHasRedirection]);
  Test('one\>two', ['one>two'], []);
  Test('"one>two"', ['one>two']);
  Test('one&two', [], [pcHasJobControl]);
  Test('one\&two', ['one&two'], []);
  Test('"one&two"', ['one&two']);
  Test('one`two', [], [pcHasSubCommand]);
  Test('''one`two''', ['one`two']);
  Test('one$two', [], [pcHasShellVariable]);
  Test('''one$two''', ['one$two']);
  Test('one$(two)', [], [pcHasSubCommand, pcHasParenthesis]);
  Test('one\$two', ['one$two'], []);
  Test('''one$(two)''', ['one$(two)']);
  Test('one*two', [], [pcHasWildcard]);
  Test('"one*two"', ['one*two']);
  Test('one*two', [], [pcHasWildcard]);
  Test('''one*two''', ['one*two']);
  Test('one\ two', ['one two'], []);
  Test('one\\two', ['one\two'], []);
  Test('one\\\\\\two', ['one\\\two'], []);
  Test('one|two', [], [pcHasRedirection], {posix=}false);
  Test('one&two', ['one&two'], [], false);
  Test(''' one'' two', ['''', 'one''', 'two'], [], false);
  Test('"one" two', ['one', 'two'], [], false);
  Test('one "two"', ['one', 'two'], [], false);
  Test('one     "two"', ['one', 'two'], [], false);
  Test('one " two"', ['one', ' two'], [], false);
  Test('" one" two', [' one', 'two'], [], false);
  Test('"one one" two', ['one one', 'two'], [], false);
end;

procedure TTestCoreBase._TExecutableCommandLine;
var
  c: TExecutableCommandLine;
  f: RawUtf8;
  t: integer;
begin
  c := TExecutableCommandLine.Create;
  try
    c.RawParams := CsvToRawUtf8DynArray('one two three', ' ');
    c.Parse(#10, '-', '--');
    CheckEqual(length(c.Args), 3);
    CheckEqual(length(c.Options), 0);
    CheckEqual(length(c.Names), 0);
    CheckEqual(length(c.Values), length(c.Names));
    Check(c.Arg('one', 'this is 1'));
    Check(c.Arg('two', 'this is 2'));
    Check(c.Arg('three', 'this is 3'));
    CheckEqual(c.DetectUnknown, '');
    CheckHash(c.FullDescription('this is test #1 executable', 'exename'), $9147E5C5);
    c.Clear;
    c.RawParams := CsvToRawUtf8DynArray('one two three', ' ');
    c.Parse(#10, '-', '--');
    CheckEqual(length(c.Args), 3);
    CheckEqual(length(c.Options), 0);
    CheckEqual(length(c.Names), 0);
    CheckEqual(length(c.Values), length(c.Names));
    Check(c.Arg(0, 'this is the main verb'));
    Check(c.Arg(1, 'the #directory name to process'));
    Check(c.Arg(2, 'some #comment text to add'));
    CheckHash(c.FullDescription('this is test #2 executable', 'exename'), $DDDDB7D4);
    Check(not c.Option('&verbose', 'generate verbose output'));
    CheckHash(c.FullDescription('this is test #2 executable', 'exename'), $2293B264);
    Check(not c.Get('logfolder', f, 'optional log #folder to write to'));
    Check(not c.Option('log', 'log the process to a local file'));
    Check(not c.Get('&threads', t, '#number of threads to run'));
    CheckEqual(c.DetectUnknown, '');
    CheckHash(c.FullDescription('this is test #2 executable', 'exename'), $3CFE0EB3);
    c.Clear;
    c.RawParams := CsvToRawUtf8DynArray('-t=10 --dest toto -v --wrong -p=4', ' ');
    c.Parse(#10, '-', '--');
    CheckEqual(length(c.Args), 0);
    CheckEqual(length(c.Options), 2);
    CheckEqual(length(c.Names), 3);
    CheckEqual(length(c.Values), length(c.Names));
    Check(c.Option('&verbose', 'generate verbose output'));
    Check(not c.Option('log', 'log the process to a local file'));
    t := 0;
    Check(c.Get('&threads', t, '#number of threads to run', 5));
    CheckEqual(t, 10);
    f := c.Param('dest', 'destination #folder', 'c:\');
    CheckEqual(f, 'toto');
    Check(not c.Get('logdest', f, 'optional log #folder'));
    CheckEqual(f, '');
    CheckHash(c.DetectUnknown, $5EA096A5);
    CheckHash(c.FullDescription('this is test #3 executable', 'exename'), $DFE40A21);
    c.Clear;
    c.RawParams := CsvToRawUtf8DynArray('-o file.txt --y -v -t 1', ' ');
    c.Parse(#10, '-', '--');
    CheckEqual(length(c.Args), 0);
    CheckEqual(length(c.Options), 2);
    CheckEqual(length(c.Names), 2);
    CheckEqual(length(c.Values), length(c.Names));
    Check(c.Option('y'));
    Check(c.Option('v'));
    Check(c.Get('o', f));
    CheckEqual(f, 'file.txt');
    Check(c.Get('t', t));
    CheckEqual(t, 1);
  finally
    c.Free;
  end;
end;

procedure TTestCoreBase._IsMatch;
var
  i, j: integer;
  V, cont: RawUtf8;
  match: TMatch;
  reuse, isword: boolean;

  procedure Contains;
  begin
    check(match.Match('12'));
    check(match.Match('12e'));
    check(match.Match('12er'));
    check(match.Match('a12'));
    check(match.Match('a12e'));
    check(match.Match('ab12'));
    check(match.Match('ab12er'));
    check(not match.Match('1'));
    check(not match.Match('a1'));
    check(not match.Match('a1b2'));
    check(not match.Match('1a2'));
  end;

  function GL(a, b: PAnsiChar; const c: RawUtf8): boolean;
  begin
    // avoid Delphi compiler complains about PUtf8Char/PAnsiChar types
    result := GetLineContains(pointer(a), pointer(b), pointer(c));
  end;

begin
  V := '123456789ABC'#10'DEF0zxy';
  Check(GL(@V[1], nil, '1'));
  Check(GL(@V[1], nil, 'C'));
  Check(GL(@V[1], nil, '89'));
  Check(not GL(@V[1], nil, 'ZX'));
  Check(GL(@V[14], nil, 'ZXY'));
  Check(not GL(@V[1], nil, '890'));
  Check(GL(@V[1], @V[21], '89'));
  Check(GL(@V[14], @V[21], 'ZX'));
  Check(not GL(@V[1], @V[21], 'ZX'));
  Check(GL(@V[14], @V[21], 'ZXY'));
  Check(not GL(@V[1], @V[5], '89'));
  Check(not GL(@V[1], @V[15], 'ZXY'));
  Check(not GL(@V[14], @V[17], 'ZXY'));
  V := '1234567890123456'#13'1234567890123456789';
  for j := 1 to 16 do
  begin
    for i := j to 16 do
    begin
      CheckEqual(BufferLineLength(@V[j], @V[i]), i - j);
      CheckEqual(GetLineSize(@V[j], @V[i]), i - j);
    end;
    for i := 17 to 34 do
    begin
      CheckEqual(BufferLineLength(@V[j], @V[i]), 17 - j);
      CheckEqual(GetLineSize(@V[j], @V[i]), 17 - j);
    end;
    CheckEqual(GetLineSize(@V[j], nil), 17 - j);
  end;
  V := '12345678901234561234567890123456'#10'1234567890123456789';
  for j := 1 to 32 do
  begin
    for i := j to 32 do
    begin
      CheckEqual(BufferLineLength(@V[j], @V[i]), i - j);
      CheckEqual(GetLineSize(@V[j], @V[i]), i - j);
    end;
    for i := 33 to 50 do
    begin
      CheckEqual(BufferLineLength(@V[j], @V[i]), 33 - j);
      CheckEqual(GetLineSize(@V[j], @V[i]), 33 - j);
    end;
    CheckEqual(GetLineSize(@V[j], nil), 33 - j);
  end;
  Check(IsMatch('', '', true));
  Check(not IsMatch('', 'toto', true));
  Check(not IsMatch('Bidule.pas', '', true));
  Check(IsMatch('Bidule.pas', 'Bidule.pas', true));
  Check(IsMatch('Bidule.pas', 'BIDULE.pas', true));
  Check(IsMatch('Bidule.pas', 'Bidule.paS', true));
  Check(IsMatch('Bidule.pas', 'Bidule.pas', false));
  Check(not IsMatch('Bidule.pas', 'bidule.pas', false));
  Check(not IsMatch('bidule.pas', 'bidulE.pas', false));
  Check(not IsMatch('bidule.pas', 'bidule.paS', false));
  Check(not IsMatch('bidule.pas', 'bidule.pa', false));
  for i := 0 to 200 do
  begin
    V := Int32ToUtf8(i);
    Check(IsMatch(V, V, false) = IsMatch(V, V, true));
  end;
  Check(IsMatch('test*', 'test', false));
  Check(IsMatch('test*', 'test', true));
  Check(IsMatch('test*', 'teste', false));
  Check(IsMatch('test*', 'teste', true));
  Check(IsMatch('test*', 'tester', false));
  Check(IsMatch('test*', 'tester', true));
  Check(IsMatch('a*', 'anything', true));
  Check(IsMatch('a*', 'a', true));
  Check(IsMatch('*', 'anything', true));
  Check(IsMatch('*.pas', 'Bidule.pas', true));
  Check(IsMatch('*.pas', 'Bidule.pas', false));
  Check(IsMatch('*.PAS', 'Bidule.pas', true));
  Check(not IsMatch('*.PAS', 'Bidule.pas', false));
  Check(IsMatch('*.p?s', 'Bidule.pas', true));
  Check(IsMatch('*.p*S', 'Bidule.pas', true));
  Check(IsMatch('B*.PAS', 'bidule.pas', true));
  Check(IsMatch('*.p?s', 'bidule.pas', false));
  Check(IsMatch('*.p*s', 'bidule.pas', false));
  Check(IsMatch('b*.pas', 'bidule.pas', false));
  Check(not IsMatch('B*.das', 'Bidule.pas', true));
  Check(IsMatch('bidule.*', 'Bidule.pas', true));
  Check(IsMatch('ma?ch.*', 'match.exe', false));
  Check(IsMatch('ma?ch.*', 'mavch.dat', false));
  Check(IsMatch('ma?ch.*', 'march.on', false));
  Check(IsMatch('ma?ch.*', 'march.', false));
  Check(IsMatch('ab*.exyz', 'ab.exyz', true));
  Check(IsMatch('ab[ef]xyz', 'abexyz', false));
  Check(IsMatch('ab[ef]xyz', 'abexyz', true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.exyz', true));
  Check(IsMatch('ab*.[ef]xyz', 'ab.exyz', true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.exyz', true));
  Check(IsMatch('ab*.[ef]xyz', 'ab.fxyz', true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.fxyz', true));
  check(not IsMatch('ab[cd]e', 'abdde', false));
  check(not IsMatch('ab[cd]ex', 'abddex', false));
  check(not IsMatch('ab*.[cd]e', 'ab.dde', false));
  check(not IsMatch('ab*.[cd]ex', 'ab.ddex', false));
  V := 'this [e-n]s a [!zy]est';
  check(not IsMatch(V, V, false));
  Check(IsMatch(V, 'this is a test', false));
  Check(IsMatch(V, 'this is a rest', false));
  Check(not IsMatch(V, 'this is a zest', false));
  Check(not IsMatch(V, 'this as a test', false));
  Check(not IsMatch(V, 'this as a rest', false));
  Check(IsMatchs('test*', 'test', false));
  Check(IsMatchs('test*', 'test', true));
  Check(IsMatchs('test*', 'teste', false));
  Check(IsMatchs('test*', 'teste', true));
  Check(IsMatchs('test*', 'tester', false));
  Check(IsMatchs('test*', 'tester', true));
  Check(IsMatchs('a*', 'anything', true));
  Check(IsMatchs('a*', 'a', true));
  Check(IsMatchs('*', 'anything', true));
  Check(IsMatchs('*.pas', 'Bidule.pas', true));
  Check(IsMatchs('*.pas', 'Bidule.pas', false));
  Check(IsMatchs('*.PAS', 'Bidule.pas', true));
  Check(not IsMatchs('*.PAS', 'Bidule.pas', false));
  Check(IsMatchs('toto,test*', 'test', false));
  Check(IsMatchs('test*,toto', 'test', true));
  Check(IsMatchs('toto,titi,test*', 'teste', false));
  Check(IsMatchs('test*,titi,toto', 'teste', true));
  Check(IsMatchs('toto,test*,titi', 'tester', false));
  Check(IsMatchs('tata,test*', 'tester', true));
  Check(IsMatchs('a*,toto', 'anything', true));
  Check(IsMatchs('toto,a*', 'a', true));
  Check(IsMatchs('*,titi', 'anything', true));
  Check(IsMatchs('*.pas,*.txt', 'Bidule.pas', true));
  Check(IsMatchs('*.txt,*.pas', 'Bidule.pas', false));
  Check(IsMatchs('*.PAS,*.pas', 'Bidule.pas', false));
  Check(IsMatchs('*.txt,*.PAS', 'Bidule.pas', true));
  Check(not IsMatchs('*.PAS,*.pAs,*.PAs', 'Bidule.pas', false));
  for reuse := false to true do
  begin  // ensure very same behavior
    match.Prepare(V, false, reuse);
    Check(not match.Match(V));
    Check(match.Match('this is a test'));
    Check(match.Match('this is a rest'));
    Check(not match.Match('this is a zest'));
    match.Prepare('test', false, reuse);
    check(match.Match('test'));
    check(not match.Match('tes'));
    check(not match.Match('tests'));
    check(not match.Match('tesT'));
    match.Prepare('teST', true, reuse);
    check(match.Match('test'));
    check(match.Match('test'));
    match.Prepare('*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('*', true, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('**', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('****', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('*.*', false, reuse);
    check(match.Match('te.st'));
    check(match.Match('te.st.'));
    check(match.Match('test.'));
    check(match.Match('.test'));
    check(match.Match('.'));
    check(not match.Match('test'));
    match.Prepare('*.*', true, reuse);
    check(match.Match('te.st'));
    check(match.Match('te.st.'));
    check(match.Match('test.'));
    check(match.Match('.test'));
    check(not match.Match('test'));
    check(match.Match('.'));
    match.Prepare('test*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    check(match.Match('tester'));
    check(not match.Match('atest'));
    check(not match.Match('tes'));
    check(not match.Match('tEst'));
    check(not match.Match('tesT'));
    check(not match.Match('t'));
    match.Prepare('*test', false, reuse);
    check(match.Match('test'));
    check(match.Match('stest'));
    check(match.Match('attest'));
    check(not match.Match('est'));
    check(not match.Match('testa'));
    check(not match.Match('tes'));
    check(not match.Match('tEst'));
    check(not match.Match('tesT'));
    check(not match.Match('t'));
    match.Prepare('*t', false, reuse);
    check(match.Match('t'));
    check(match.Match('st'));
    check(match.Match('tt'));
    check(match.Match('att'));
    check(not match.Match('s'));
    check(not match.Match('es'));
    check(not match.Match('ts'));
    match.Prepare('**', false, reuse);
    check(match.Match('') = reuse);
    check(match.Match('test'));
    match.Prepare('*test*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(match.Match('abtest'));
    check(match.Match('abtester'));
    check(not match.Match('tes'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    check(not match.Match('tesT'));
    check(not match.Match('Teste'));
    check(not match.Match('TEster'));
    check(not match.Match('atEst'));
    check(not match.Match('ateSTe'));
    match.Prepare('*12*', false, reuse);
    Contains;
    if reuse then
    begin
      cont := '12';
      match.PrepareContains(cont, false);
      Contains;
      cont := '12';
      match.PrepareContains(cont, true);
      Contains;
    end;
    match.Prepare('*teSt*', true, reuse);
    check(match.Match('test'));
    check(match.Match('teste'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(match.Match('abtest'));
    check(match.Match('abtester'));
    check(match.Match('tesT'));
    check(match.Match('Teste'));
    check(match.Match('TEster'));
    check(match.Match('atEst'));
    check(match.Match('ateSTe'));
    check(match.Match('abteST'));
    check(match.Match('abtEster'));
    check(not match.Match('tes'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('*te?t*', true, reuse);
    check(match.Match('test'));
    check(match.Match('tezt'));
    check(match.Match('teste'));
    check(match.Match('tezte'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(not match.Match('tes'));
    check(not match.Match('tet'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('?est*', true, reuse);
    check(match.Match('test'));
    check(match.Match('test'));
    check(match.Match('teste'));
    check(match.Match('tester'));
    check(not match.Match('tezte'));
    check(not match.Match('atest'));
    check(not match.Match('est'));
    check(not match.Match('este'));
    check(not match.Match('tes'));
    check(not match.Match('tet'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('a*bx*cy*d', false, reuse);
    check(match.Match('abxcyd'));
    check(match.Match('a1bxcyd'));
    check(match.Match('a12bxcyd'));
    check(match.Match('a123bxcyd'));
    check(match.Match('abx1cyd'));
    check(match.Match('abx12cyd'));
    check(match.Match('abxcy1d'));
    check(match.Match('abxcy12d'));
    check(match.Match('abxcy123d'));
    check(not match.Match('abcyd'));
    check(not match.Match('abxcyde'));
    match.Prepare(
      '************************************************' +
      '************************************************' +
      '**************************************************.*', false, reuse);
    check(match.MatchThreadSafe('abxcyd.'));
    check(match.MatchThreadSafe('abxc.yd'));
    check(match.MatchThreadSafe('abxcy.d'));
    check(match.MatchThreadSafe('.'));
    check(match.MatchThreadSafe('.a'));
    check(match.MatchThreadSafe('.abxcyd'));
    check(not match.MatchThreadSafe('abxcyd'));
  end;
  for i := 32 to 127 do
  begin
    SetLength(V, 1);
    V[1] := AnsiChar(i);
    isword := (tcWord in TEXT_BYTES[i]);
    Check(IsMatch('[A-Za-z0-9]', V) = isword);
    Check(IsMatch('[01-456a-zA-Z789]', V) = isword);
    SetLength(V, 3);
    V[1] := AnsiChar(i);
    V[2] := AnsiChar(i);
    V[3] := AnsiChar(i);
    Check(IsMatch('[A-Za-z0-9]?[A-Za-z0-9]', V) = isword);
    Check(IsMatch('[A-Za-z0-9]*', V) = isword);
    Check(IsMatch('[a-z0-9]?[A-Z0-9]', V, true) = isword);
    Check(IsMatch('[A-Z0-9]*', V, true) = isword);
  end;
end;

procedure TTestCoreBase._TExprParserMatch;
var
  s: TExprParserMatch;

  procedure Test(const expression: RawUtf8; const ok, nok: array of RawUtf8);
  var
    i: PtrInt;
  begin
    Check(s.Parse(expression) = eprSuccess);
    for i := 0 to high(ok) do
      Check(s.Search(ok[i]));
    for i := 0 to high(nok) do
      Check(not s.Search(nok[i]));
  end;

begin
  s := TExprParserMatch.Create({casesensitive=}true);
  try // &=AND -=WITHOUT +=OR
    check(s.Parse('') = eprNoExpression);
    check(s.Parse('  ') = eprNoExpression);
    check(s.Parse('1+ ') = eprMissingFinalWord);
    Test('1', ['1', '1 2 3', '2 1'],
              ['2', '13', '2 3']);
    Test('   1   ', ['1', '1 2 3', '2 1'],
                    ['2', '13', '2 3']);
    Test('1+4', ['1', '1 2 3', '2 1', '2 4 3'],
                ['2', '13', '2 3', '41']);
    Test(' 1 + 4 ', ['1', '1 2 3', '2 1', '2 4 3'],
                    ['2', '13', '2 3', '41']);
    Test('1+4+5', ['1', '1 2 3', '2 1', '2 4 3'],
                  ['2', '13', '2 3', '41']);
    Test('1+(4+5)', ['1', '1 2 3', '2 1', '2 4 3'],
                    ['2', '13', '2 3', '41']);
    Test('1+4*+5', ['1', '1 2 3', '2 1', '2 4 3', '41'],
                   ['2', '13', '2 3']);
    Test('1+(4&555)', ['4 555 3', '555 4', '1', '1 2 3', '2 1'],
                      ['2', '13', '2 3', '41', '4 3', '3 555']);
    Test('1+(4 555)', ['4 555 3', '555 4', '1', '1 2 3', '2 1'],
                      ['2', '13', '2 3', '41', '4 3', '3 555']);
    Test('1-4', ['1', '1 2 3', '2 1', '2 1 3'],
                ['1 4', '4 2 1', '2', '13', '2 3', '41']);
    Test('1-(4&5)', ['1', '1 2 3', '2 1', '1 4', '1 5'],
                    ['2', '5 2 3 4 1', '2 3', '41', '4 3', '3 5', '1 4 5']);
    Test('1-(4&(5+6))', ['1', '1 2 3', '2 1', '1 4', '1 5', '1 6'],
        ['2', '5 2 3 4 1', '2 3', '41', '4 3', '3 5', '1 4 5', '1 4 6']);
    Test('1 - ( 4 & ( 57 + 6 ) )', ['1', '1 2 3', '2 1', '1 4', '1 57', '1 6'],
        ['2', '57 2 3 4 1', '2 3', '41', '4 3', '3 5"7', '1 4 57', '1 4 6']);
    Test('1 - ( 4 & ( 5? + 6 ) )', ['1', '1 2 3', '2 1', '1 4', '1 57', '1 6'],
        ['2', '57 2 3 4 1', '2 3', '41', '4 3', '3 5"7', '1 4 57', '1 4 6']);
    Test('1 - ( 4 & ( 5* + 6* ) )', ['1', '1 2 3', '2 1', '1 4', '1 57', '1 6'],
        ['2', '57 2 3 4 1', '2 3', '41', '4 3', '3 5"7', '1 4 57', '1 4 6']);
  finally
    s.Free;
  end;
end;

const
  REF_LECUYER_GENERATOR: TBlock512 = (
    2094674600, 1801471443, 1564436181, 3659342702,
    1831620425, 3729943674, 687904812,  2066320563,
    3494904290, 3023528103, 1358263417, 3202492728,
    1577967257, 3235083616, 712712534,  1900728807);
  REF_LECUYER_GENERATOR_TRAIL: TBlock512 = (
    2912814506, 4264204172, 1224264557, 457988427,
    3671383357, 2304790299, 1068635130, 1812365788,
    18904424,   1385490254, 3829840815, 3086100873,
    1986702847, 635322329,  2467062584, 3233345822);

procedure TTestCoreBase._Random32;
var
  i, n: PtrInt;
  q, qp: QWord;
  c: array[0..1000] of cardinal;
  timer: TPrecisionTimer;
  gen: TLecuyer;
begin
  for i := 0 to high(c) do
    c[i] := Random32;
  QuickSortInteger(@c, 0, high(c));
  n := 0;
  for i := 0 to high(c) - 1 do
    if c[i + 1] = c[i] then
      inc(n);
  Check(n < 2, 'unique Random32'); // n=1 have been seen once
  timer.Start;
  Check(Random32(0) = 0);
  Check(Random32(1) = 0);
  for i := 1 to 100000 do
    Check(Random32(i) < cardinal(i));
  for i := 0 to 100000 do
    Check(Random32(maxInt - i) < cardinal(maxInt - i));
  qp := 0;
  n := 0;
  for i := 1 to 20000 do
  begin
    q := Random64;
    Check((q = 0) or (q <> qp));
    if q and $ffffffff00000000 <> 0 then
      inc(n);
    qp := q;
  end;
  Check(n > 20000 - 20, 'Random64');
  n := 100000 * 2 + 20000 * 2;
  NotifyTestSpeed('Random32', n, n * 4, @timer);
  timer.Start;
  for i := 1 to 100 do
    RandomBytes(@c, SizeOf(c));
  NotifyTestSpeed('RandomBytes', 0, SizeOf(c) * 100, @timer);
  for i := 0 to high(REF_LECUYER_GENERATOR) do
  begin
    gen.SeedGenerator(i);
    FillCharFast(c, SizeOf(c), 0); // gen.Fill() will XOR the buffer
    gen.Fill(@c, SizeOf(c));
    CheckEqual(Hash32(@c, SizeOf(c)), REF_LECUYER_GENERATOR[i], 'lecgen');
    CheckEqual(gen.Next, REF_LECUYER_GENERATOR_TRAIL[i], 'lecgentrail');
  end;
end;

procedure TTestCoreBase._TRawUtf8Interning;
var
  int: TRawUtf8Interning;
  i, v: integer;
  tmp: RawUtf8;
  vs: TRawUtf8DynArray;
  timer: TPrecisionTimer;
  caseinsensitive: boolean;

  procedure DoOne(const u: RawUtf8);
  var
    local: RawUtf8; // will force dec(RefCnt) when leaving DoOne()
  begin
    local := int.Unique(u);
    CheckEqual(local, u);
  end;

const
  MAX = 500000;
  ONESIZE = 32; // assume each SmallUInt32Utf8[] uses 32 heap bytes
  DIRSIZE = ONESIZE * (MAX + 1);
  INTSIZE = ONESIZE * 512;
begin
  for caseinsensitive := false to true do
  begin
    int := TRawUtf8Interning.Create(1, caseinsensitive);
    try
      CheckEqual(int.Count, 0);
      DoOne('test');
      CheckEqual(int.Count, 1);
      DoOne('test');
      CheckEqual(int.Count, 1);
      CheckEqual(int.Clean, 1);
      DoOne('single');
      CheckEqual(int.Count, 1);
      CheckEqual(int.Clean, 1);
      CheckEqual(int.Count, 0);
      CheckEqual(int.Clean, 0);
      CheckEqual(int.Count, 0);
      DoOne('single1');
      CheckEqual(int.Count, 1);
      DoOne('single1');
      CheckEqual(int.Count, 1);
      DoOne('test2');
      CheckEqual(int.Count, 2);
      DoOne('test2');
      CheckEqual(int.Count, 2);
      DoOne('single2');
      CheckEqual(int.Count, 3);
      CheckEqual(int.Clean, 3);
      CheckEqual(int.Count, 0);
      int.Unique(tmp, 'kept', 4);
      CheckEqual(tmp, 'kept');
      CheckEqual(GetRefCount(tmp), 2);
      tmp := '';
      if caseinsensitive then
        int.Unique(tmp, 'KEPT', 4) // should be identified as previous 'kept'
      else
        int.Unique(tmp, 'kept', 4);
      CheckEqual(tmp, 'kept');
      CheckEqual(GetRefCount(tmp), 2);
      CheckEqual(int.Count, 1);
      CheckEqual(int.Clean, 0);
      CheckEqual(int.Count, 1);
      tmp := '';
      CheckEqual(int.Clean, 1);
      CheckEqual(int.Count, 0);
      int.Clear;
      CheckEqual(int.Count, 0);
      CheckEqual(int.Clean, 0);
      CheckEqual(int.Count, 0);
    finally
      int.Free;
    end;
    int := TRawUtf8Interning.Create(16, caseinsensitive);
    try
      for i := 0 to MAX do
      begin
        v := i and 511;
        int.Unique(tmp, SmallUInt32Utf8[v]);
        check(Utf8ToInteger(tmp) = v);
      end;
      checkEqual(int.Count, 512);
      checkEqual(int.Clean, 0);
      checkEqual(int.Count, 512);
    finally
      int.Free;
    end;
  end;
  int := TRawUtf8Interning.Create(4);
  try
    SetLength(vs, MAX + 1);
    timer.Start;
    for i := 0 to MAX do
    begin
      v := i and 511;
      int.Unique(vs[i], pointer(SmallUInt32Utf8[v]), length(SmallUInt32Utf8[v]));
    end;
    NotifyTestSpeed('interning %', [KB(INTSIZE)], MAX, DIRSIZE, @timer);
    for i := 0 to MAX do
      check(Utf8ToInteger(vs[i]) = i and 511);
    check(int.Count = 512);
    check(int.Clean = 0);
    check(int.Count = 512);
    for i := 0 to MAX do
      check(Utf8ToInteger(vs[i]) = i and 511);
    vs := nil; // fair test
    check(int.Count = 512);
    check(int.Clean = 512);
    check(int.Count = 0);
  finally
    int.Free;
  end;
  SetLength(vs, MAX + 1);
  timer.Start;
  for i := 0 to MAX do
  begin
    v := i and 511;
    FastSetString(vs[i], pointer(SmallUInt32Utf8[v]), length(SmallUInt32Utf8[v]));
  end;
  NotifyTestSpeed('direct %', [KB(DIRSIZE)], MAX, DIRSIZE, @timer);
  for i := 0 to MAX do
    check(Utf8ToInteger(vs[i]) = i and 511);
end;

function kr32reference(buf: PAnsiChar; len: cardinal): cardinal;
var
  i: integer;
begin
  result := 0;
  for i := 0 to len - 1 do
    result := result * 31 + ord(buf[i]);
end;

function fnv32reference(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
var
  i: integer;
begin
  for i := 0 to len - 1 do
    crc := (crc xor ord(buf[i])) * 16777619;
  result := crc;
end;

function crc32creference(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := not crc;
  if buf <> nil then
    while len > 0 do
    begin
      result := crc32ctab[0, ToByte(result xor ord(buf^))] xor (result shr 8);
      dec(len);
      inc(buf);
    end;
  result := not result;
end;

function Hash32Reference(Data: PCardinal; Len: integer): cardinal;
var
  s1, s2: cardinal;
  i: integer;
begin
  if Data <> nil then
  begin
    s1 := 0;
    s2 := 0;
    for i := 1 to Len shr 2 do
    begin
      // 4 bytes (DWORD) by loop
      inc(s1, Data^);
      inc(s2, s1);
      inc(Data);
    end;
    case Len and 3 of // remaining 0..3 bytes
      1:
        inc(s1, PByte(Data)^);
      2:
        inc(s1, PWord(Data)^);
      3:
        inc(s1, PWord(Data)^ or (PByteArray(Data)^[2] shl 16));
    end;
    inc(s2, s1);
    result := s1 xor (s2 shl 16);
  end
  else
    result := 0;
end;

{$ifndef FPC} // RolDWord is an intrinsic function under FPC :)
function RolDWord(value: cardinal; count: integer): cardinal;
  {$ifdef HASINLINE} inline;{$endif}
begin
  result := (value shl count) or (value shr (32 - count));
end;
{$endif FPC}

function xxHash32reference(P: PAnsiChar; len: integer; seed: cardinal = 0): cardinal;
const
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;
var
  c1, c2, c3, c4: cardinal;
  PLimit, PEnd: PAnsiChar;
begin
  PEnd := P + len;
  if len >= 16 then
  begin
    PLimit := PEnd - 16;
    c1 := seed + PRIME32_1 + PRIME32_2;
    c2 := seed + PRIME32_2;
    c3 := seed;
    c4 := seed - PRIME32_1;
    repeat
      c1 := PRIME32_1 * RolDWord(c1 + PRIME32_2 * PCardinal(P)^, 13);
      c2 := PRIME32_1 * RolDWord(c2 + PRIME32_2 * PCardinal(P + 4)^, 13);
      c3 := PRIME32_1 * RolDWord(c3 + PRIME32_2 * PCardinal(P + 8)^, 13);
      c4 := PRIME32_1 * RolDWord(c4 + PRIME32_2 * PCardinal(P + 12)^, 13);
      inc(P, 16);
    until not (P <= PLimit);
    result := RolDWord(c1, 1) + RolDWord(c2, 7) + RolDWord(c3, 12) + RolDWord(c4, 18);
  end
  else
    result := seed + PRIME32_5;
  inc(result, len);
  while P <= PEnd - 4 do
  begin
    inc(result, PCardinal(P)^ * PRIME32_3);
    result := RolDWord(result, 17) * PRIME32_4;
    inc(P, 4);
  end;
  while P < PEnd do
  begin
    inc(result, PByte(P)^ * PRIME32_5);
    result := RolDWord(result, 11) * PRIME32_1;
    inc(P);
  end;
  result := result xor (result shr 15);
  result := result * PRIME32_2;
  result := result xor (result shr 13);
  result := result * PRIME32_3;
  result := result xor (result shr 16);
end;

procedure crcblockreference(crc128, data128: PBlock128);
var
  c: cardinal;
begin
  c := crc128^[0] xor data128^[0];
  crc128^[0] := crc32ctab[3, byte(c)] xor crc32ctab[2, byte(c shr 8)] xor
                crc32ctab[1, byte(c shr 16)] xor crc32ctab[0, c shr 24];
  c := crc128^[1] xor data128^[1];
  crc128^[1] := crc32ctab[3, byte(c)] xor crc32ctab[2, byte(c shr 8)] xor
                crc32ctab[1, byte(c shr 16)] xor crc32ctab[0, c shr 24];
  c := crc128^[2] xor data128^[2];
  crc128^[2] := crc32ctab[3, byte(c)] xor crc32ctab[2, byte(c shr 8)] xor
                crc32ctab[1, byte(c shr 16)] xor crc32ctab[0, c shr 24];
  c := crc128^[3] xor data128^[3];
  crc128^[3] := crc32ctab[3, byte(c)] xor crc32ctab[2, byte(c shr 8)] xor
                crc32ctab[1, byte(c shr 16)] xor crc32ctab[0, c shr 24];
end;

procedure TTestCoreBase._crc32c;
var
  crc: array[0..10000] of record
    S: RawByteString;
    crc: cardinal;
  end;
  totallen: Cardinal;
  s2, msg: RawByteString;

  procedure Test(hash: THasher; const name: string);
  var
    i: PtrInt;
    Timer: TPrecisionTimer;
    a: string[10];
  begin
    Timer.Start;
    a := '123456789';
    Check(hash(0, @a, 0) = 0);
    Check(hash(0, @a, 1) = $2ACF889D);
    Check(hash(0, @a, 2) = $BD5FE6AF);
    Check(hash(0, @a, 3) = $7F40BC73);
    Check(hash(0, @a, 4) = $13790E51);
    Check(crc32cBy4(cardinal(not 0), PCardinal(@a)^) = cardinal(not $13790E51),
      'crc32cBy4');
    Check(hash(0, @a, 5) = $659AD21);
    Check(hash(0, @a, 6) = $85BF5A8C);
    Check(hash(0, @a, 7) = $8B0FB6FA);
    Check(hash(0, @a, 8) = $2E5336F0);
    for i := 0 to High(crc) do
      with crc[i] do
        Check(hash(0, pointer(S), length(S)) = crc);
    msg := FormatUtf8('% %:%/s', [msg, name, KBNoSpace(Timer.PerSec(totallen))]);
  end;

  procedure test16(const text: RawUtf8; expected: cardinal);
  begin
    Check(crc16(pointer(text), length(text)) = expected);
  end;

var
  i, j: integer;
  c1, c2: cardinal;
  crc1, crc2: THash128;
  crcs: THash512Rec;
  digest: THash256;
  tmp: RawByteString;
  hmac32: THmacCrc32c;
begin
  test16('', $ffff);
  test16('a', $9d77);
  test16('ab', $69f0);
  test16('toto', $e2ca);
  test16('123456789', $29b1);
  test16('123456789123456789', $a86d);
  totallen := 36;
  tmp := '123456789123456789';
  c2 := $12345678;
  c1 := HmacCrc32c(@c2, pointer(tmp), 4, length(tmp));
  check(c1 = $1C3C4B51);
  hmac32.Init(@c2, 4);
  hmac32.Update(pointer(tmp), length(tmp));
  check(hmac32.Done = c1);
  c2 := $12345678;
  HmacCrc256c(@c2, pointer(tmp), 4, length(tmp), digest);
  checkEqual(Sha256DigestToString(digest),
    '46da01fb9f4a97b5f8ba2c70512bc22aaa9b57e5030ced9f5c7c825ab5ec1715');
  FillZero(crc2);
  crcblock(@crc2, PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc2));
  check(TBlock128(crc2)[0] = 1314793854);
  check(TBlock128(crc2)[1] = 582109780);
  check(TBlock128(crc2)[2] = 1177891908);
  check(TBlock128(crc2)[3] = 4047040040);
  FillZero(crc1);
  crcblockreference(@crc1, PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc1));
  check(IsEqual(crc1, crc2));
  FillZero(crc1);
  crcblocks(@crc1, PBlock128(PAnsiChar('0123456789012345')), 1);
  check(not IsZero(crc1));
  check(IsEqual(crc1, crc2), 'crcblocks');
  FillZero(crc1);
  crcblockfast(@crc1, PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc1));
  check(IsEqual(crc1, crc2));
  for i := 0 to high(crcs.b) do
    crcs.b[i] := i;
  for j := 1 to 4 do
  begin
    FillZero(crc2);
    crcblockreference(@crc2, @crcs.h0);
    if j > 1 then
      crcblockreference(@crc2, @crcs.h1);
    if j > 2 then
      crcblockreference(@crc2, @crcs.h2);
    if j > 3 then
      crcblockreference(@crc2, @crcs.h3);
    FillZero(crc1);
    crcblocks(@crc1, @crcs.h0, j);
    check(not IsZero(crc1));
    check(IsEqual(crc1, crc2), 'crcblocks4');
    FillZero(crc1);
    crcblocksfast(@crc1, @crcs.h0, j);
    check(not IsZero(crc1));
    check(IsEqual(crc1, crc2), 'crcblocksfast4');
    CheckEqual(Hash128Index(@crcs, 4, @crcs.r[j - 1]), j - 1);
    check(Hash128Index(@crcs, j - 1, @crcs.r[j - 1]) < 0);
  end;
  CheckEqual(Hash256Index(@crcs, 2, @crcs.r[0]), 0);
  check(Hash256Index(@crcs, 2, @crcs.r[1]) < 0);
  CheckEqual(Hash256Index(@crcs, 2, @crcs.r[2]), 1);
  check(Hash256Index(@crcs, 2, @crcs.r[3]) < 0);
  for i := 0 to 50000 do
  begin
    FillZero(crc1);
    crcblock(@crc1, @digest);
    check(not IsZero(crc1));
    if @crcblock <> @crcblockfast then
    begin
      FillZero(crc2);
      crcblockreference(@crc2, @digest);
      check(not IsZero(crc2));
      check(IsEqual(crc1, crc2));
      FillZero(crc2);
      crcblockfast(@crc2, @digest);
      check(not IsZero(crc2));
      check(IsEqual(crc1, crc2));
    end;
    for j := 0 to high(digest) do
      inc(digest[j]);
  end;
  for i := 0 to High(crc) do
    with {%H-}crc[i] do
    begin
      j := i shr 3 + 1; // circumvent weird FPC code generation bug in -O2 mode
      S := RandomWinAnsi(j);
      Check(length(S) = j);
      crc := crc32creference(0, pointer(S), length(S));
      inc(totallen, length(S));
      c2 := HmacCrc32c(@c1, pointer(S), 4, length(S));
      hmac32.Init(@c1, 4);
      hmac32.Update(pointer(S), length(S));
      CheckEqual(hmac32.Done, c2, 'hmac32');
      s2 := S;
      SymmetricEncrypt(i, s2);
      check(s2 <> S);
      SymmetricEncrypt(i, s2);
      CheckEqual(s2, S, 'SymmetricEncrypt');
      s2 := S;
      LecuyerEncrypt(i, s2);
      Check(s2 <> S);
      LecuyerEncrypt(i, s2);
      CheckEqual(s2, S, 'LecuyerEncrypt');
    end;
  Check(crc32fast(0, @crc32tab, 5) = $DF4EC16C, 'crc32a');
  Check(crc32fast(0, @crc32tab, 1024) = $6FCF9E13, 'crc32b');
  Check(crc32fast(0, @crc32tab, 1024 - 5) = $70965738, 'crc32c');
  Check(crc32fast(0, pointer(PtrInt(@crc32tab) + 1), 2) = $41D912FF, 'crc32d');
  Check(crc32fast(0, pointer(PtrInt(@crc32tab) + 3), 1024 - 5) = $E5FAEC6C, 'crc32e');
  Test(crc32creference, 'pas');
  Test(crc32cinlined, 'inl');
  Test(crc32cfast, 'fast');
  {$ifdef CPUINTEL}
  {$ifndef OSDARWIN}
  // Not [yet] working on Darwin
  if cfSSE42 in CpuFeatures then
    Test(crc32csse42, 'sse42');
  {$endif OSDARWIN}
  {$ifdef CPUX64}
  if (cfSSE42 in CpuFeatures) and
     (cfAesNi in CpuFeatures) then
    Test(crc32c, 'aesni'); // use SSE4.2+pclmulqdq instructions on x64
  {$endif CPUX64}
  {$else}
  if @crc32c <> @crc32cfast then
    Test(crc32c, 'armv8');
  {$endif CPUINTEL}
  AddConsole('%', [msg]);
end;

procedure TTestCoreBase.intadd(const Sender; Value: integer);
begin
  AddToCsv(UInt32ToUtf8(Value), fAdd);
end;

procedure TTestCoreBase.intdel(const Sender; Value: integer);
begin
  AddToCsv(UInt32ToUtf8(Value), fDel);
end;

procedure TTestCoreBase.Integers;

  procedure changes(const old, new, added, deleted: RawUtf8);
  var
    o, n: TIntegerDynArray;
  begin
    CsvToIntegerDynArray(pointer(old), o);
    CsvToIntegerDynArray(pointer(new), n);
    fAdd := '';
    fDel := '';
    NotifySortedIntegerChanges(pointer(o), pointer(n), length(o), length(n),
      intadd, intdel, self);
    CheckEqual(fAdd, added, 'added');
    CheckEqual(fDel, deleted, 'deleted');
  end;

  procedure includes(const values, includes, excludes, included, excluded: RawUtf8);

    procedure includes32;
    var
      v, i, e: TIntegerDynArray;
    begin
      CsvToIntegerDynArray(Pointer(values), v);
      CsvToIntegerDynArray(Pointer(excludes), e);
      ExcludeInteger(v, e, 32); // no sort
      Check(IntegerDynArrayToCsv(v) = excluded);
      v := nil;
      e := nil;
      CsvToIntegerDynArray(Pointer(values), v);
      CsvToIntegerDynArray(Pointer(excludes), e);
      ExcludeInteger(v, e, 2); // sort
      Check(IntegerDynArrayToCsv(v) = excluded);
      v := nil;
      e := nil;
      CsvToIntegerDynArray(Pointer(values), v);
      CsvToIntegerDynArray(Pointer(includes), i);
      IncludeInteger(v, i, 32); // no sort
      Check(IntegerDynArrayToCsv(v) = included);
      v := nil;
      e := nil;
      CsvToIntegerDynArray(Pointer(values), v);
      CsvToIntegerDynArray(Pointer(includes), i);
      IncludeInteger(v, i, 2); // sort
      Check(IntegerDynArrayToCsv(v) = included);
    end;

    procedure includes64;
    var
      v, i, e: TInt64DynArray;
    begin
      CsvToInt64DynArray(Pointer(values), v);
      CsvToInt64DynArray(Pointer(excludes), e);
      ExcludeInt64(v, e, 32); // no sort
      Check(Int64DynArrayToCsv(v) = excluded);
      v := nil;
      e := nil;
      CsvToInt64DynArray(Pointer(values), v);
      CsvToInt64DynArray(Pointer(excludes), e);
      ExcludeInt64(v, e, 2); // sort
      Check(Int64DynArrayToCsv(v) = excluded);
      v := nil;
      e := nil;
      CsvToInt64DynArray(Pointer(values), v);
      CsvToInt64DynArray(Pointer(includes), i);
      IncludeInt64(v, i, 32); // no sort
      Check(Int64DynArrayToCsv(v) = included);
      v := nil;
      e := nil;
      CsvToInt64DynArray(Pointer(values), v);
      CsvToInt64DynArray(Pointer(includes), i);
      IncludeInt64(v, i, 2); // sort
      Check(Int64DynArrayToCsv(v) = included);
    end;

  begin
    Includes32;
    Includes64;
  end;

var
  i8: TByteDynArray;
  i16: TWordDynArray;
  i32, i32_: TIntegerDynArray;
  i64: TInt64DynArray;
  i, n: PtrInt;
  c: integer;
  timer: TPrecisionTimer;
begin
  CheckEqual(9007199254740991, MAX_SAFE_JS_INTEGER);
  Check(IsPowerOfTwo(0));
  Check(IsPowerOfTwo(1));
  Check(IsPowerOfTwo(2));
  Check(not IsPowerOfTwo(3));
  Check(IsPowerOfTwo(4));
  Check(not IsPowerOfTwo(5));
  Check(not IsPowerOfTwo(6));
  Check(not IsPowerOfTwo(7));
  Check(IsPowerOfTwo(8));
  for i := 9 to 15 do
    Check(not IsPowerOfTwo(i));
  Check(IsPowerOfTwo(16));
  for i := 17 to 31 do
    Check(not IsPowerOfTwo(i));
  Check(IsPowerOfTwo(32));
  CheckEqual(NextPowerOfTwo(0), 1);
  CheckEqual(NextPowerOfTwo(1), 1);
  CheckEqual(NextPowerOfTwo(2), 2);
  CheckEqual(NextPowerOfTwo(3), 4);
  CheckEqual(NextPowerOfTwo(4), 4);
  for i := 5 to 8 do
    CheckEqual(NextPowerOfTwo(i), 8);
  for i := 9 to 16 do
    CheckEqual(NextPowerOfTwo(i), 16);
  for i := 17 to 32 do
    CheckEqual(NextPowerOfTwo(i), 32);
  for i := 1025 to 2048 do
    CheckEqual(NextPowerOfTwo(i), 2048);
  for i := 65537 to 131072 do
    CheckEqual(NextPowerOfTwo(i), 131072);
  for i := 33554433 to 67108864 - 1 do // millions tests in a few ms :)
  begin
    Check(not IsPowerOfTwo(i));
    Check(NextPowerOfTwo(i) = 67108864);
  end;
  Check(IsPowerOfTwo(67108864));
  Check(NextPowerOfTwo(67108864) = 67108864);
  n := 512;
  CheckEqual(MinPtrInt(1, n), 1);
  CheckEqual(MaxPtrInt(1, n), n);
  CheckEqual(MinPtrUInt(1, n), 1);
  CheckEqual(MaxPtrUInt(1, n), n);
  SetLength(i8, n);
  for i := 0 to n - 1 do
  begin
    i8[i] := i;
    CheckEqual(MinPtrInt(i, i), i);
    CheckEqual(MaxPtrInt(i, i), i);
    CheckEqual(MinPtrUInt(i, i), i);
    CheckEqual(MaxPtrUInt(i, i), i);
    CheckEqual(MinPtrInt(i, n), i);
    CheckEqual(MaxPtrInt(i, n), n);
    CheckEqual(MinPtrUInt(i, n), i);
    CheckEqual(MaxPtrUInt(i, n), n);
    CheckEqual(MinPtrInt(i, i + 1), i);
    CheckEqual(MaxPtrInt(i, i - 1), i);
    CheckEqual(MinPtrUInt(i, i + 1), i);
    if i > 0 then
      CheckEqual(MaxPtrUInt(i, i - 1), i);
    CheckEqual(bswap16(bswap16(i)), i);
    CheckEqual(bswap32(bswap32(i)), i);
    CheckEqual(bswap64(bswap64(i)), i);
  end;
  CheckEqual(bswap16($0001), $0100, 'bswap16');
  CheckEqual(bswap32($00010203), $03020100, 'bswap32');
  CheckEqual(Int64(bswap64($0001020304050607)), $0706050403020100, 'bswap64');
  CheckEqual(ByteScanIndex(pointer(i8), 100, 100), -1);
  CheckEqual(ByteScanIndex(pointer(i8), 101, 100), 100);
  CheckEqual(ByteScanIndex(@i8[1], 100, 0), -1, 'aligned read');
  CheckEqual(ByteScanIndex(@i8[1], 100, 1), 0, 'unaligned read');
  for i := 0 to n - 1 do
    Check(ByteScanIndex(pointer(i8), n, i) = i and 255);
  SetLength(i16, n);
  for i := 0 to n - 1 do
    i16[i] := i;
  CheckEqual(WordScanIndex(pointer(i16), 100, 100), -1);
  CheckEqual(WordScanIndex(pointer(i16), 101, 100), 100);
  CheckEqual(WordScanIndex(@i16[1], 100, 0), -1, 'aligned read');
  CheckEqual(WordScanIndex(@i16[1], 100, 1), 0, 'unaligned read');
  for i := 0 to n - 1 do
    Check(WordScanIndex(pointer(i16), n, i) = i);
  SetLength(i32, n);
  for i := 0 to n - 1 do
    i32[i] := i;
  CheckEqual(IntegerScanIndex(pointer(i32), 100, 100), -1);
  CheckEqual(IntegerScanIndex(pointer(i32), 101, 100), 100);
  CheckEqual(IntegerScanIndex(@i32[1], 100, 0), -1, 'aligned read');
  CheckEqual(IntegerScanIndex(@i32[1], 100, 1), 0, 'unaligned read');
  for i := 0 to n - 1 do
    CheckEqual(IntegerScanIndex(pointer(i32), n, i), i);
  i32 := nil;
  DeduplicateInteger(i32);
  CheckEqual(i32, nil);
  SetLength(i32, 2);
  i32[0] := 1;
  QuickSortInteger(i32);
  CheckEqual(i32[0], 0);
  CheckEqual(i32[1], 1);
  DeduplicateInteger(i32);
  CheckEqual(length(i32), 2);
  CheckEqual(i32[0], 0);
  CheckEqual(i32[1], 1);
  i32[0] := 1;
  DeduplicateInteger(i32);
  CheckEqual(length(i32), 1);
  CheckEqual(i32[0], 1);
  SetLength(i32, 6);
  i32[4] := 1;
  i32[5] := 2;
  DeduplicateInteger(i32); // (1, 0, 0, 0, 1, 2)
  CheckEqual(length(i32), 3);
  CheckEqual(i32[0], 0);
  CheckEqual(i32[1], 1);
  CheckEqual(i32[2], 2);
  SetLength(i32, 6);
  i32[4] := 3;
  i32[5] := 3;
  DeduplicateInteger(i32); // (0, 1, 2, 0, 3, 3)
  CheckEqual(length(i32), 4);
  CheckEqual(i32[0], 0);
  CheckEqual(i32[1], 1);
  CheckEqual(i32[2], 2);
  CheckEqual(i32[3], 3);
  i32_ := i32;
  DeleteInteger(i32_, 2);
  CheckEqual(length(i32), 4, 'unique');
  CheckEqual(i32[0], 0);
  CheckEqual(i32[1], 1);
  CheckEqual(i32[2], 2);
  CheckEqual(i32[3], 3);
  CheckEqual(length(i32_), 3, 'copied');
  CheckEqual(i32_[0], 0);
  CheckEqual(i32_[1], 1);
  CheckEqual(i32_[2], 3);
  for n := 1 to 1000 do
  begin
    SetLength(i32, n);
    for i := 0 to n - 1 do
      i32[i] := i and 15;
    DeduplicateInteger(i32);
    if n < 16 then
      CheckEqual(Length(i32), n)
    else
      CheckEqual(Length(i32), 16);
    for i := 0 to high(i32) do
      CheckEqual(i32[i], i);
  end;
  changes('', '', '', '');
  changes('1', '1', '', '');
  changes('', '1', '1', '');
  changes('1', '', '', '1');
  changes('1,2', '1,3', '3', '2');
  changes('2', '1,3', '1,3', '2');
  changes('', '1,3', '1,3', '');
  changes('1,2,3,4', '1,2,3,4', '', '');
  changes('1,2,3,4', '1,2,3,4,5', '5', '');
  changes('1,2,3,4', '1,3,4', '', '2');
  changes('1,2,3,4', '3,4', '', '1,2');
  changes('1,2,3,4', '1,4', '', '2,3');
  changes('1,2,3,4', '', '', '1,2,3,4');
  changes('1,2,3,4', '5,6', '5,6', '1,2,3,4');
  changes('1,2,4', '1,3,5,6', '3,5,6', '2,4');
  changes('1,2,4', '3,5,6', '3,5,6', '1,2,4');
  includes('1,2,3', '2', '2', '2', '1,3');
  includes('1,2,3', '2,3', '2,3', '2,3', '1');
  includes('1,2,3', '1,2,3', '1,2,3', '1,2,3', '');
  includes('1,2,3', '3,1,2', '3,1,2', '1,2,3', '');
  check({%H-}i64 = nil);
  DeduplicateInt64(i64);
  check(i64 = nil);
  SetLength(i64, 2);
  i64[0] := 1;
  QuickSortInt64(pointer(i64), 0, 1);
  check(i64[0] = 0);
  check(i64[1] = 1);
  DeduplicateInt64(i64);
  check(length(i64) = 2);
  check(i64[0] = 0);
  check(i64[1] = 1);
  i64[0] := 1;
  DeduplicateInt64(i64);
  check(length(i64) = 1);
  check(i64[0] = 1);
  SetLength(i64, 6);
  i64[4] := 1;
  i64[5] := 2;
  DeduplicateInt64(i64); // (1, 0, 0, 0, 1, 2)
  check(length(i64) = 3);
  check(i64[0] = 0);
  check(i64[1] = 1);
  check(i64[2] = 2);
  SetLength(i64, 6);
  i64[4] := 3;
  i64[5] := 3;
  DeduplicateInt64(i64); // (0, 1, 2, 0, 3, 3)
  check(length(i64) = 4);
  check(i64[0] = 0);
  check(i64[1] = 1);
  check(i64[2] = 2);
  check(i64[3] = 3);
  c := 4;
  AddSortedInt64(i64, c, 10);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '0,1,2,3,10');
  AddSortedInt64(i64, c, 20);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '0,1,2,3,10,20');
  AddSortedInt64(i64, c, 15);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '0,1,2,3,10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, -100);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '0,1,2,3,10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, 0);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '0,1,2,3,10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, 1);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '1,2,3,10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, 2);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '2,3,10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, 9);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '10,15,20');
  RemoveSortedInt64SmallerThan(i64, c, 17);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '20');
  RemoveSortedInt64SmallerThan(i64, c, 20);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '20');
  RemoveSortedInt64SmallerThan(i64, c, 170);
  Check(i64 = nil);
  CheckEqual(c, 0);
  CheckEqual(Int64DynArrayToCsv(pointer(i64), c), '');
  for n := 1 to 1000 do
  begin
    SetLength(i64, n);
    for i := 0 to n - 1 do
      i64[i] := i and 15;
    DeduplicateInt64(i64);
    if n < 16 then
      check(Length(i64) = n)
    else
      check(Length(i64) = 16);
    for i := 0 to high(i64) do
      check(i64[i] = i);
  end;
  SetLength(i32, 100000);
  n := 10;
  repeat
    rnd.Fill(pointer(i32), n * 4);
    timer.Start;
    QuickSortInteger(pointer(i32), 0, n - 1);
    NotifyTestSpeed('QuickSortInteger', n, 0, @timer, {onlylog=}true);
    for i := 1 to n - 1 do
      Check(i32[i - 1] <= i32[i]);
    n := n * 10;
  until n > length(i32);
  SetLength(i32, 1);
  i32[0] := 1;
  CheckEqual(length(i32), 1);
  CheckEqual(i32[0], 1);
  AddSortedInteger(i32, 2);
  CheckEqual(length(i32), 2);
  CheckEqual(i32[0], 1);
  CheckEqual(i32[1], 2);
end;

function TestAddFloatStr(const str: RawUtf8): RawUtf8;
var
  tmp: TTextWriterStackBuffer;
begin
  with TJsonWriter.CreateOwnedStream(tmp) do
  try
    AddFloatStr(pointer(str));
    SetText(result);
  finally
    Free;
  end;
end;

procedure TTestCoreBase.NumericalConversions;

  procedure CheckDoubleToShort(v: double; const expected: RawUtf8);
  var
    a: ShortString;
    d: double;
    err: integer;
  begin
    ExtendedToShort(@a, v, DOUBLE_PRECISION);
    CheckEqual(ShortStringToUtf8(a), expected, 'ExtendedToShort');
    DoubleToShort(@a, v);
    CheckEqual(ShortStringToUtf8(a), expected, 'DoubleToShort');
    a[ord(a[0]) + 1] := #0;
    d := GetExtended(@a[1], err);
    CheckEqual(err, 0);
    CheckSame(v, d);
  end;

  procedure CheckDoubleToShortSame(v: double);
  var
    s: string;
    u: RawUtf8;
    err: integer;
    d: double;
  begin
    s := DoubleToString(v);
    val(s, d, err);
    CheckEqual(err, 0);
    CheckSame(d, v);
    StringToUtf8(s, u);
    d := GetExtended(pointer(u), err);
    CheckEqual(err, 0);
    CheckSame(d, v);
  end;

var
  i, j, b, err: integer;
  juint: cardinal absolute j;
  k, l: Int64;
  q: QWord;
  s, s2: RawUtf8;
  d, e: double;
  f: extended;
  sd, se: single;
  c: currency;
  ident: TRawUtf8DynArray;
  vj, vs: variant;
  a, a2: ShortString;
  u: string;
  varint: TByteToByte;
  st: TFastReader;
  PB, PC: PByte;
  P: PUtf8Char;
  crc, u32, n: cardinal;
  Timer: TPrecisionTimer;
begin
  a := '';
  AppendShortCardinal(0, a);
  check(a = '0');
  for i := 1 to 10 do
    AppendShortCardinal(i, a);
  check(a = '012345678910');
  for i := 11 to 150 do
    AppendShortCardinal(i, a);
  CheckHash(a, $6C291F09, 'AppendShortCardinal');
  Check(TwoDigits(0) = '0');
  Check(TwoDigits(1) = '1');
  Check(TwoDigits(10) = '10');
  Check(TwoDigits(100) = '100');
  Check(TwoDigits(1000) = '1000');
  Check(TwoDigits(0.1) = '0.10');
  Check(TwoDigits(0.12) = '0.12');
  Check(TwoDigits(0.123) = '0.12');
  Check(TwoDigits(0.124) = '0.12');
  Check(TwoDigits(0.125) = '0.12');
  Check(TwoDigits(0.1251) = '0.13');
  Check(TwoDigits(0.126) = '0.13');
  Check(TwoDigits(0.129) = '0.13');
  Check(TwoDigits(70.131) = '70.13');
  Check(TwoDigits(70.135) = '70.13');
  Check(TwoDigits(70.1351) = '70.14');
  Check(TwoDigits(0.01) = '0.01');
  Check(TwoDigits(0.05) = '0.05');
  Check(TwoDigits(0.051) = '0.05');
  Check(TwoDigits(0.055) = '0.05');
  Check(TwoDigits(0.0551) = '0.06');
  Check(TwoDigits(0.0015) = '0');
  Check(TwoDigits(0.0055) = '0.01');
  n := 100000;
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do
  begin
    f := d;
    {$ifdef FPC}
    j := FloatToText(PChar(@varint), f, ffGeneral, DOUBLE_PRECISION, 0);
    {$else}
    j := FloatToText(PChar(@varint), f, fvExtended, ffGeneral, DOUBLE_PRECISION, 0);
    {$endif FPC}
    PChar(@varint)[j] := #0;
    inc(crc, j);
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('FloatToText ', [PChar(@varint)], n, crc, @Timer);
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do
  begin
    str(d, a);
    inc(crc, ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('str ', [{%H-}a], n, crc, @Timer);
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do
  begin
    DoubleToShort(@a, d);
    inc(crc, ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('DoubleToShort ', [a], n, crc, @Timer);
  a[ord(a[0]) + 1] := #0;
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  {$ifdef DOUBLETOSHORT_USEGRISU}
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do
  begin
    DoubleToAscii(C_NO_MIN_WIDTH, -1, d, @a);
    inc(crc, ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('DoubleToAscii ', [a], n, crc, @Timer);
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  d := 0;
  DoubleToAscii(C_NO_MIN_WIDTH, -1, d, @a);
  Check(a = '0');
  DoubleToAscii(0, DOUBLE_PRECISION, d, @a);
  Check(a = '0');
  {$endif DOUBLETOSHORT_USEGRISU}
  CheckEqual(TestAddFloatStr(''), '0');
  CheckEqual(TestAddFloatStr(' 123'), '123');
  CheckEqual(TestAddFloatStr(' 1a23'), '1');
  CheckEqual(TestAddFloatStr(' 123z'), '123');
  CheckEqual(TestAddFloatStr(' 12.3'), '12.3');
  CheckEqual(TestAddFloatStr('12.'), '12.');
  CheckEqual(TestAddFloatStr(' +12.3'), '+12.3');
  CheckEqual(TestAddFloatStr(' -12.3'), '-12.3');
  CheckEqual(TestAddFloatStr('12.3e230'), '12.3e230');
  CheckEqual(TestAddFloatStr('12.3E230'), '12.3E230');
  CheckEqual(TestAddFloatStr('12.3e-230'), '12.3e-230');
  CheckEqual(TestAddFloatStr('12.3E-230'), '12.3E-230');
  CheckEqual(TestAddFloatStr('12.3e 230'), '12.3e');
  CheckEqual(TestAddFloatStr('12.3f230'), '12.3');
  CheckEqual(TestAddFloatStr('12.3E23.0'), '12.3E23');
  CheckEqual(TestAddFloatStr('-.01'), '-0.01'); // ODBC numeric output
  CheckEqual(TestAddFloatStr('.0002'), '0.0002'); // ODBC numeric output
  CheckEqual(OctToBin(''), '');
  CheckEqual(OctToBin('123'), '123');
  CheckEqual(OctToBin('\\123'), '\123');
  CheckEqual(OctToBin('12\\3'), '12\3');
  CheckEqual(OctToBin('123\\'), '123\');
  CheckEqual(OctToBin('123\'), '123');
  CheckEqual(OctToBin('\041'), '!');
  CheckEqual(OctToBin('a\041'), 'a!');
  CheckEqual(OctToBin('\041b'), '!b');
  CheckEqual(OctToBin('a\041b'), 'a!b');
  CheckEqual(OctToBin('a\101b\102'), 'aAbB');
  CheckEqual(OctToBin('a\101\102b'), 'aABb');
  CheckEqual(OctToBin('a\101\\\102b'), 'aA\Bb');
  CheckEqual(OctToBin('a\401b\102'), 'a');
  CheckEqual(OctToBin('a\181b\102'), 'a');
  CheckEqual(OctToBin('a\10ab\102'), 'a');
  CheckEqual(OctToBin('a\1'), 'a');
  CheckEqual(OctToBin('a\10'), 'a');
  Check(Plural('row', 0) = '0 row');
  Check(Plural('row', 1) = '1 row');
  Check(Plural('row', 2) = '2 rows');
  Check(Plural('row', 20) = '20 rows');
  Check(Plural('row', 200000) = '200000 rows');
  Check(not SameValue(386.0, 386.1));
  Check(not SameValue(386.0, 700, 2));
  Check(IntToThousandString(0) = '0');
  Check(IntToThousandString(1) = '1');
  Check(IntToThousandString(10) = '10');
  Check(IntToThousandString(100) = '100');
  Check(IntToThousandString(1000) = '1,000');
  Check(IntToThousandString(10000) = '10,000');
  Check(IntToThousandString(100000) = '100,000');
  Check(IntToThousandString(1000000) = '1,000,000');
  Check(IntToThousandString(-1) = '-1');
  Check(IntToThousandString(-10) = '-10');
  Check(IntToThousandString(-100) = '-100');
  Check(IntToThousandString(-1000) = '-1,000');
  Check(IntToThousandString(-10000) = '-10,000');
  Check(IntToThousandString(-100000) = '-100,000');
  Check(IntToThousandString(-1000000) = '-1,000,000');
  Check(UInt3DigitsToUtf8(1) = '001');
  Check(UInt3DigitsToUtf8(12) = '012');
  Check(UInt3DigitsToUtf8(123) = '123');
  Check(UInt4DigitsToUtf8(1) = '0001');
  Check(UInt4DigitsToUtf8(12) = '0012');
  Check(UInt4DigitsToUtf8(123) = '0123');
  Check(UInt4DigitsToUtf8(1234) = '1234');
  CheckEqualShort(MicroSecToString(0) , '0us');
  CheckEqualShort(MicroSecToString(QWord(-10)) , '0us');
  CheckEqualShort(MicroSecToString(10) , '10us');
  CheckEqualShort(MicroSecToString(999) , '999us');
  CheckEqualShort(MicroSecToString(1000) , '1ms');
  CheckEqualShort(MicroSecToString(1001) , '1ms');
  CheckEqualShort(MicroSecToString(1010) , '1.01ms');
  CheckEqualShort(MicroSecToString(1100) , '1.10ms');
  CheckEqualShort(MicroSecToString(999999) , '999.99ms');
  CheckEqualShort(MicroSecToString(1000000) , '1s');
  CheckEqualShort(MicroSecToString(1000001) , '1s');
  CheckEqualShort(MicroSecToString(2030001) , '2.03s');
  CheckEqualShort(MicroSecToString(200000070001) , '2d');
  Check(KbNoSpace(0)            = '0B' , 'kb0');
  Check(KbNoSpace(99)           = '99B', 'kb99');
  Check(KbNoSpace(1 shl 10 - 1) = '1KB', 'kb1');
  Check(KbNoSpace(1 shl 10)     = '1KB', 'kb2');
  Check(KbNoSpace(1 shl 10 + 1) = '1KB', 'kb3');
  Check(KbNoSpace(1 shl 20 - 1) = '1MB', 'kb4');
  Check(KbNoSpace(1 shl 20)     = '1MB', 'kb5');
  Check(KbNoSpace(1 shl 20 + 1) = '1MB', 'kb6');
  Check(KbNoSpace(1 shl 30 - 1) = '1GB', 'kb7');
  Check(KbNoSpace(1 shl 30)     = '1GB', 'kb8');
  Check(KbNoSpace(1 shl 30 + 1) = '1GB', 'kb9');
  Check(KB(-123) = '');
  Check(KB(0) = '0 B');
  Check(KB(123) = '123 B');
  Check(KB(1023) = '1 KB');
  Check(KB(1024) = '1 KB');
  Check(KB(1025) = '1 KB');
  Check(KB(16383) = '16 KB');
  Check(KB(16384) = '16 KB');
  Check(KB(16385) = '16 KB');
  Check(KB(3 * 1024 * 1024 - 800 * 1024) = '2.2 MB');
  Check(KB(3 * 1024 * 1024) = '3 MB');
  Check(KB(3 * 1024 * 1024 + 511 * 1024) = '3.5 MB');
  Check(KB(3 * 1024 * 1024 + 512 * 1024) = '3.5 MB');
  Check(KB(3 * 1024 * 1024 + 1024) = '3 MB');
  Check(KB(maxInt) = '2 GB');
  Check(KB(3294963200) = '3 GB');
  Check(KB(4294963200) = '4 GB');
  Check(Int64ToUtf8(-maxInt) = '-2147483647');
  Check(Int64ToUtf8(-1) = '-1');
  Check(Int64ToUtf8(-9223372036854775807) = '-9223372036854775807');
  Int64ToUtf8(-maxInt, s);
  Check(s = '-2147483647');
  Int64ToUtf8(-1, s);
  Check(s = '-1');
  Int64ToUtf8(100, s);
  Check(s = '100');
  Int64ToUtf8(-9223372036854775807, s);
  Check(s = '-9223372036854775807');
  {$ifdef HASINLINE} // bug with MinInt64 with older versions of Delphi
  Check(Int64ToUtf8(-9223372036854775808) = '-9223372036854775808');
  Int64ToUtf8(-9223372036854775808, s);
  Check(s = '-9223372036854775808');
  {$endif HASINLINE}
  Check(Int64ToUtf8(2119852951849248647) = '2119852951849248647');
  Check(FormatUtf8(' % ', [2119852951849248647]) = ' 2119852951849248647 ');
  s := '1234';
  d := GetExtended(pointer(s));
  CheckSame(d, 1234);
  s := '1234.1';
  d := GetExtended(pointer(s));
  CheckSame(d, 1234.1);
  s := '12345678901234567890';
  d := GetExtended(pointer(s));
  CheckSame(d, 12345678901234567890.0, 0);
  s := '1234.1234567890123456789';
  d := GetExtended(pointer(s));
  CheckSame(d, 1234.1234567890123456789);
  s := '.1234';
  d := GetExtended(pointer(s));
  CheckSame(d, 0.1234);
  s := '.1234e';
  d := GetExtended(pointer(s), err);
  Check(err <> 0);
  s := '.1234e4';
  d := GetExtended(pointer(s), err);
  Check(err = 0);
  CheckSame(d, 1234);
  u := DoubleToString(40640.5028819444);
  Check(u = '40640.5028819444', u);
  s := '40640.5028a819444';
  GetExtended(pointer(s), err);
  Check(err > 0);
  s := '40640.5028819444';
  d := GetExtended(pointer(s), err);
  Check(err = 0);
  u := DoubleToString(d);
  Check(u = '40640.5028819444', u);
  e := 40640.5028819444;
  CheckSame(d, e, 1e-11);
  s := '40640e400';
  d := GetExtended(pointer(s), err);
  CheckSame(d, 40640.0, DOUBLE_SAME, 'e400=e0');
  Check(err > 0, 'e400');
  s := 'Nan';
  d := GetExtended(pointer(s), err);
  CheckEqual(err, 0, s);
  Check(IsNan(d));
  DoubleToShort(@a, d);
  Check(IdemPropName(a, 'Nan'));
  Check(ShortToFloatNan(a) = fnNan);
  Check(FloatToJsonNan(@a)^ = JSON_NAN[fnNan]);
  s := 'INF';
  d := GetExtended(pointer(s), err);
  CheckEqual(err, 0, s);
  Check(IsInfinite(d));
  DoubleToShort(@a, d);
  Check((a = '+Inf') or (a = 'INF'));
  Check(ShortToFloatNan(a) = fnInf);
  Check(FloatToJsonNan(@a)^ = JSON_NAN[fnInf]);
  s := '-INfinity';
  d := GetExtended(pointer(s), err);
  CheckEqual(err, 0, s);
  Check(IsInfinite(d));
  DoubleToShort(@a, d);
  Check(IdemPropName(a, '-Inf'));
  Check(ShortToFloatNan(a) = fnNegInf);
  Check(FloatToJsonNan(@a)^ = JSON_NAN[fnNegInf]);
  Check(IsAnsiCompatible('t'));
  Check(IsAnsiCompatible('te'));
  Check(IsAnsiCompatible('tes'));
  Check(IsAnsiCompatible('test'));
  Check(IsAnsiCompatible('teste'));
  CheckDoubleToShort(0, '0');
  CheckDoubleToShort(1, '1');
  CheckDoubleToShort(-1, '-1');
  CheckDoubleToShort(0.1, '0.1');
  CheckDoubleToShort(0.01, '0.01');
  CheckDoubleToShort(0.001, '0.001');
  CheckDoubleToShort(0.0001, '0.0001');
  CheckDoubleToShort(-0.1, '-0.1');
  CheckDoubleToShort(-0.01, '-0.01');
  CheckDoubleToShort(-0.001, '-0.001');
  CheckDoubleToShort(-0.0001, '-0.0001');
  CheckDoubleToShort(1.1, '1.1');
  CheckDoubleToShort(1.01, '1.01');
  CheckDoubleToShort(1.001, '1.001');
  CheckDoubleToShort(1.0001, '1.0001');
  CheckDoubleToShort(1.00001, '1.00001');
  CheckDoubleToShort(-1.1, '-1.1');
  CheckDoubleToShort(-1.01, '-1.01');
  CheckDoubleToShort(-1.001, '-1.001');
  CheckDoubleToShort(-1.0001, '-1.0001');
  CheckDoubleToShort(-1.00001, '-1.00001');
  CheckDoubleToShort(7, '7');
  CheckDoubleToShort(-7, '-7');
  CheckDoubleToShort(0.7, '0.7');
  CheckDoubleToShort(0.07, '0.07');
  CheckDoubleToShort(0.007, '0.007');
  CheckDoubleToShort(0.0007, '0.0007');
  CheckDoubleToShort(-0.7, '-0.7');
  CheckDoubleToShort(-0.07, '-0.07');
  CheckDoubleToShort(-0.007, '-0.007');
  CheckDoubleToShort(-0.0007, '-0.0007');
  CheckDoubleToShort(7.7, '7.7');
  CheckDoubleToShort(7.07, '7.07');
  CheckDoubleToShort(7.007, '7.007');
  CheckDoubleToShort(7.0007, '7.0007');
  CheckDoubleToShort(7.00007, '7.00007');
  CheckDoubleToShort(-7.7, '-7.7');
  CheckDoubleToShort(-7.07, '-7.07');
  CheckDoubleToShort(-7.007, '-7.007');
  CheckDoubleToShort(-7.0007, '-7.0007');
  CheckDoubleToShort(-7.00007, '-7.00007');
  {$ifdef FPC}
  CheckDoubleToShort(0.00001, '0.00001');
  CheckDoubleToShort(-0.00001, '-0.00001');
  CheckDoubleToShort(0.00007, '0.00007');
  CheckDoubleToShort(-0.00007, '-0.00007');
  {$endif FPC}
  CheckDoubleToShort(11111.1, '11111.1');
  CheckDoubleToShort(11111.01, '11111.01');
  CheckDoubleToShort(11111.001, '11111.001');
  CheckDoubleToShort(11111.0001, '11111.0001');
  CheckDoubleToShort(11111.00001, '11111.00001');
  CheckDoubleToShort(-11111.1, '-11111.1');
  CheckDoubleToShort(-11111.01, '-11111.01');
  CheckDoubleToShort(-11111.001, '-11111.001');
  CheckDoubleToShort(-11111.0001, '-11111.0001');
  CheckDoubleToShort(-11111.00001, '-11111.00001');
  CheckDoubleToShort(0.9999999999999997, '1');
  CheckDoubleToShort(-0.9999999999999997, '-1');
  CheckDoubleToShort(9.999999999999997, '10');
  CheckDoubleToShort(-9.999999999999997, '-10');
  CheckDoubleToShort(999.9999999999997, '1000');
  CheckDoubleToShort(-999.9999999999997, '-1000');
  CheckDoubleToShort(22.99999999999997, '23');
  CheckDoubleToShort(-22.99999999999997, '-23');
  CheckDoubleToShort(999.9999999999933, '999.999999999993');
  CheckDoubleToShort(-999.9999999999933, '-999.999999999993');
  CheckDoubleToShortSame(3.3495117168);
  CheckDoubleToShortSame(-3.3495117168);
  CheckDoubleToShortSame(-3.3495117168e-1);
  CheckDoubleToShortSame(3.3495117168e-1);
  CheckDoubleToShortSame(-3.3495117168e-5);
  CheckDoubleToShortSame(3.3495117168e-5);
  CheckDoubleToShortSame(-3.3495117168e-10);
  CheckDoubleToShortSame(3.3495117168e-10);
  CheckDoubleToShortSame(-3.9999617168e-14);
  CheckDoubleToShortSame(3.9999617168e-14);
  CheckDoubleToShortSame(-3.9999617168e-15);
  CheckDoubleToShortSame(3.9999617168e-15);
  CheckDoubleToShortSame(12.345678901234);
  CheckDoubleToShortSame(123.45678901234);
  CheckDoubleToShortSame(1234.5678901234);
  Check(Int32ToUtf8(1599638299) = '1599638299');
  Check(UInt32ToUtf8(1599638299) = '1599638299');
  Check(Int32ToUtf8(-1599638299) = '-1599638299');
  Check(Int64ToUtf8(-1271083787498396012) = '-1271083787498396012');
  CheckEqual(Int64ToUtf8(242161819595454762), '242161819595454762');
  // detect 64-bit integer overflow in GetExtended()
  CheckDoubleToShort(95.0290695380, '95.029069538');
  Check(ToDouble('95.0290695380', d), '95.02');
  CheckSame(d, 95.029069538);
  Check(ToDouble('95.02906953800000000000', d), '95.x');
  CheckSame(d, 95.029069538);
  Check(ToDouble('184467440737095514', d), '184467440737095514');
  CheckSame(d, 184467440737095514);
  Check(ToDouble('1844674407370955148', d), '1844674407370955148');
  CheckSame(d, 1844674407370955148);
  //  SQLite text-to-float converter routine failed with this number
  Check(ToDouble('18446744073709551488', d), '18446744073709551488');
  CheckSame(d, 1.8446744074e+19, 1e+10);
  {$ifdef FPC} // Delphi doesn't handle consistently such huge constants
  CheckDoubleToShort(d, '1.8446744073709552E19');
  CheckDoubleToShortSame(d);
  CheckDoubleToShort(1234567890123456789, '1.2345678901234568E18');
  CheckDoubleToShortSame(1234567890123456789);
  CheckDoubleToShortSame(18446744073709551);
  CheckDoubleToShortSame(184467440737095514);
  CheckDoubleToShortSame(1844674407370955148);
  {$endif FPC}
  // validate ScanUtf8()
  Check(ScanUtf8('1 2 3', '  %', [@i, @j, @d]) = 0);
  Check(ScanUtf8('', '%d%d%f', [@i, @j, @d]) = 0);
  Check(ScanUtf8('1 2 7', '%d%d%f', [@i, @j, @d]) = 3);
  Check(i = 1);
  Check(j = 2);
  Check(d = 7);
  Check(ScanUtf8('2/3/8.1', '%d/%d/%f', [@i, @j, @d]) = 3);
  Check(i = 2);
  Check(j = 3);
  CheckSame(d, 8.1);
  Check(ScanUtf8('5 / 6/3', '%d/%d / %f', [@i, @j, @d]) = 3);
  Check(i = 5);
  Check(j = 6);
  Check(d = 3);
  Check(ScanUtf8('15 25 35', '%d%D', [@i, @k, @d]) = 2);
  Check(i = 15);
  Check(k = 25);
  Check(d = 3);
  Check(ScanUtf8('1 21 35', '%d%d%f', [@i, @j]) = 2);
  Check(i = 1);
  Check(j = 21);
  Check(d = 3);
  Check(ScanUtf8(' 10  20  abc  ', '%d%d%s', [@i, @j, @a]) = 3);
  Check(i = 10);
  Check(j = 20);
  Check(a = 'abc');
  Check(ScanUtf8('1 00000002 3.01234 ', '%dtoto %x%Ftiti', [@i, @j, @c]) = 3);
  Check(i = 1);
  Check(j = 2);
  Check(c = 3.0123);
  Check(ScanUtf8('10 0000000a 77.77 7', '%dtoto %x%Ftiti%Uboat', [@i, @j, @c, @crc],
    @ident) = 4);
  Check(i = 10);
  Check(j = 10);
  Check(c = 77.77);
  Check(crc = 7);
  Check(Length(ident) = 4);
  Check(ident[0] = 'dtoto');
  Check(ident[1] = 'x');
  Check(ident[2] = 'Ftiti');
  Check(ident[3] = 'Uboat');
  Check(xxHash32(0, 'A', 1) = 275094093);
  Check(xxHash32(0, 'ABACK', 5) = 314231639);
  Check(xxHash32(0, 'ABBREVIATIONS', 13) = 3058487595);
  Check(xxHash32(0, 'LORD', 4) = 3395586315);
  Check(xxHash32(0, 'MICROINSTRUCTION''S', 18) = 1576115228);
  a[0] := #0;
  AppendShortIntHex(0, a);
  Check(a = '00');
  AppendShortIntHex(1, a);
  Check(a = '0001');
  AppendShortIntHex(255, a);
  Check(a = '0001ff');
  AppendShortIntHex($12345, a);
  Check(a = '0001ff012345');
  a[0] := #0;
  AppendShortIntHex(Int64(-1), a);
  CheckEqual(length(a), SizeOf(Int64) * 2);
  for i := 1 to length(a) do
    Check(a[1] = 'f');
  CheckEqual(ParseHex0x('0x3f',   {no0x=}false), $3f, '3f');
  CheckEqual(ParseHex0x('0x3ff',  {no0x=}false), $3ff, '3ff');
  CheckEqual(ParseHex0x(' 0x3ff', {no0x=}false), $3ff, '3ff2');
  CheckEqual(ParseHex0x('0x3ff ', {no0x=}false), $3ff, '3ff3');
  CheckEqual(ParseHex0x('0x3ff'#13#10,  {no0x=}false), $3ff, '3ff4');
  CheckEqual(ParseHex0x(' 0x3ff'#13#10, {no0x=}false), $3ff, '3ff4');
  for i := -10000 to 10000 do
    Check(GetInteger(Pointer(Int32ToUtf8(i))) = i);
  for i := 0 to 10000 do
  begin
    j := i shr 6; // circumvent weird FPC code generation bug in -O2 mode
    rnd.FillAscii(j, s);
    CheckHash(s, Hash32Reference(pointer(s), length(s)));
    Check(kr32(0, pointer(s), length(s)) = kr32reference(pointer(s), length(s)));
    Check(fnv32(0, pointer(s), length(s)) = fnv32reference(0, pointer(s), length(s)));
    crc := crc32creference(0, pointer(s), length(s));
    Check(crc32cfast(0, pointer(s), length(s)) = crc);
    Check(crc32c(0, pointer(s), length(s)) = crc);
    if s <> '' then
      Check(xxhash32(0, pointer(s), length(s)) = xxHash32reference(pointer(s),
        length(s)));
    if i <> 0 then
      j := rnd.Next; // always validate j=0 value
    str(j, a);
    s := RawUtf8(a);
    u := string(a);
    CheckEqual(OctToBin(s), s);
    CheckEqual(TestAddFloatStr(s), s);
    Check(SysUtils.IntToStr(j) = u);
    s2 := Int32ToUtf8(j);
    CheckEqual(s2, s);
    Check(format('%d', [j]) = u);
    Check(GetInteger(pointer(s)) = j);
    CheckEqual(FormatUtf8('%', [j]), s);
    CheckEqual(FormatSql('?', [], [j]), ':(' + s + '):');
    CheckEqual(FormatUtf8('%?', [j]), s + '?');
    CheckEqual(FormatUtf8('?%', [j]), '?' + s);
    CheckEqual(FormatUtf8('?%?', [j]), '?' + s + '?');
    CheckEqual(FormatUtf8('?%%?', [j]), '?' + s + '?');
    CheckEqual(FormatUtf8('?%?%  ', [j]), '?' + s + '?  ');
    CheckEqual(FormatSql('?%', [], [j]), ':(' + s + '):');
    CheckEqual(FormatSql('%?', [j], [j]), s + ':(' + s + '):');
    CheckEqual(FormatSql('%?', [s], [s]), s + ':(''' + s + '''):');
    CheckEqual(FormatUtf8('% ', [j]), s + ' ');
    CheckEqual(FormatSql('? ', [], [j]), ':(' + s + '): ');
    CheckEqual(FormatUtf8('% %', [j]), s + ' ');
    CheckEqual(FormatUtf8(' % %', [j]), ' ' + s + ' ');
    CheckEqual(FormatSql(' ?? ', [], [j]), ' :(' + s + '): ');
    CheckEqual(FormatJson('?', [], [j]), s);
    CheckEqual(FormatJson('?%', [], [j]), s);
    CheckEqual(FormatJson('? ', [], [j]), s + ' ');
    CheckEqual(FormatJson(' ?? ', [], [j]), ' ' + s + ' ');
    CheckEqual(FormatJson('?%', [], [s]), '"' + s + '"');
    CheckEqual(FormatJson(' ?? ', [], [s]), ' "' + s + '" ');
    CheckEqual(FormatJson('? %', [s], [s]), '"' + s + '" ' + s);
    vj := variant(j);
    RawUtf8ToVariant(s, vs);
    CheckEqual(FormatJson(' ?? ', [], [vj]), ' ' + s + ' ');
    CheckEqual(FormatSql(' ?? ', [], [vj]), ' :(' + s + '): ');
    CheckEqual(FormatSql('% ?', [vj], [vj]), s + ' :(' + s + '):');
    CheckEqual(FormatSql(' ?? ', [], [vs]), ' :(''' + s + '''): ');
    CheckEqual(FormatSql('% ?', [vj], [vj]), s + ' :(' + s + '):');
    CheckEqual(FormatJson('? %', [vj], [vj]), s + ' ' + s);
    CheckEqual(FormatJson(' ?? ', [], [vs]), ' "' + s + '" ');
    CheckEqual(FormatJson('? %', [vs], [vj]), s + ' ' + s);
    k := Int64(j) * rnd.Next(MaxInt);
    b := rnd.Next(64);
    s := GetBitCsv(k, b);
    l := 0;
    P := pointer(s);
    SetBitCsv(l, b, P);
    Check(P = nil);
    while b > 0 do
    begin
      dec(b);
      Check(GetBit(l, b) = GetBit(k, b));
    end;
    str(k, a);
    s := RawUtf8(a);
    u := string(a);
    CheckEqual(TestAddFloatStr(s), s);
    Check(SysUtils.IntToStr(k) = u);
    Check(IsAnsiCompatible(s));
    Check(Int64ToUtf8(k) = s);
    Check(IntToString(k) = u);
    Check(format('%d', [k]) = u);
    Check(FormatUtf8('%', [k]) = s);
    Check(FormatSql('?', [], [k]) = ':(' + s + '):');
    err := 1;
    l := GetInt64(pointer(s), err);
    Check(l = k);
    Check(err = 0);
    dec(l);
    SetInt64(pointer(s), l);
    Check(l = k);
    if k >= 0 then
    begin
      l := GetQWord(pointer(s), err);
      Check(l = k);
      Check(err = 0);
      dec(l);
      SetQword(pointer(s), QWord(l));
      Check(l = k);
    end;
    s := s + 'z';
    l := GetInt64(pointer(s), err);
    Check(err <> 0);
    case i of // validate some explicit ToVarUInt32/64 boundaries
      8999:
        j := $0000000f;
      9990:
        j := $000000ff;
      9991:
        j := $00003fff;
      9992:
        j := $00004000;
      9993:
        j := $00004001;
      9994:
        j := $001fffff;
      9995:
        j := $00200000;
      9996:
        j := $00200001;
      9997:
        j := $0fffffff;
      9998:
        j := $10000000;
      9999:
        j := $10000001;
    end;
    str(j, a);
    Check(SysUtils.IntToStr(j) = string(a));
    Check(format('%d', [j]) = string(a));
    Check(format('%.8x', [j]) = IntToHex(j, 8));
    if j >= 0 then
    begin
      a[0] := #0;
      AppendShortIntHex(j, a); // with DisplayMinChars() trimming
      CheckUtf8(PointerToHexShort(pointer(PtrInt(j))) = a, 'p2hex %=%', [j, a]);
      CheckUtf8(IdemPropName(a, ToHexShort(@j, 4)), '2hex %=%', [j, a]);
      CheckUtf8(IdemPropNameU(Int64ToHexLower(j), @a[1], ord(a[0])), 'i2hex %', [a]);
      a[ord(a[0]) + 1] := #0; // make valid PAnsiChar
      CheckEqual(ParseHex0x(@a[1], {no0x=}true), j, '8-bit hex');
      if a[1] = '0' then
        CheckEqual(ParseHex0x(@a[2], {no0x=}true), j, '4-bit hex');
      CheckEqual(ParseHex0x(@a[1], {no0x=}false), 0, '0x');
    end;
    case i of
      9990:
        d := 1E110;
      9991:
        d := 1E-110;
      9992:
        d := 1E210;
      9993:
        d := 1E-210;
    else
      d := rnd.NextDouble * 1E-17 - rnd.NextDouble * 1E-19;
    end;
    str(d, a);
    s := RawUtf8(a);
    e := GetExtended(Pointer(s), err);
    Check(err = 0, 'GetExt1');
    Check(SameValue(e, d, 0)); // validate str()
    s := ExtendedToStr(d, DOUBLE_PRECISION);
    e := GetExtended(Pointer(s), err);
    Check(err = 0, 'GetExt2');
    Check(SameValue(e, d, 0));
    e := d;
    if (i < 9000) or
       (i > 9999) then
    begin
      a[0] := AnsiChar(ExtendedToShort(@a, d, DOUBLE_PRECISION));
      a2[0] := AnsiChar(DoubleToShort(@a2, d));
      Check(a = a2);
      a[0] := AnsiChar(ExtendedToShortNoExp(@a, d, DOUBLE_PRECISION));
      a2[0] := AnsiChar(DoubleToShortNoExp(@a2, d));
      Check(a = a2);
      CheckEqual(TestAddFloatStr(s), s);
      Check(not SameValue(e + 1, d));
      sd := d;
      Check(d = e);
      Check(SortDynArrayDouble(d, d) = 0);
      Check(SortDynArrayDouble(d, e) = 0);
      se := sd;
      Check(SortDynArraySingle(sd, sd) = 0);
      Check(SortDynArraySingle(sd, se) = 0);
    end;
    if d < 0 then
      e := e * 0.9
    else
      e := e * 1.1;
    check(d < e);
    Check(SortDynArrayDouble(d, e) = -1);
    Check(SortDynArrayDouble(e, d) = 1);
    if (i < 9000) or
       (i > 9999) then
    begin
      se := e;
      Check(SortDynArraySingle(sd{%H-}, se) = -1);
      Check(SortDynArraySingle(se, sd) = 1);
    end;
    PC := ToVarUInt32(juint, @varint);
    Check(PC <> nil);
    Check(PtrInt(PC) - PtrInt(@varint) = integer(ToVarUInt32Length(juint)));
    PB := @varint;
    Check(PtrUInt(FromVarUint32(PB)) = juint);
    Check(PB = PC);
    PC := ToVarUInt32(i, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(PtrInt(FromVarUint32(PB)) = i);
    Check(PB = PC);
    PB := FromVarUInt32Safe(@varint, PC, u32);
    Check(PtrInt(u32) = i);
    Check(PB = PC);
    PC := ToVarInt32(j, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarInt32(PB) = j);
    Check(PB = PC);
    PC := ToVarInt32(i - 1, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarInt32(PB) = i - 1);
    Check(PB = PC);
    PC := ToVarUInt64(juint, @varint);
    Check(PC <> nil);
    Check(PtrInt(PC) - PtrInt(@varint) = integer(ToVarUInt32Length(juint)));
    PB := @varint;
    Check(PtrUInt(FromVarUint64(PB)) = juint);
    Check(PB = PC);
    PB := FromVarUInt64Safe(@varint, PC, q);
    Check(q = juint);
    Check(PB = PC);
    PC := ToVarInt64(k, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarInt64(PB) = k);
    Check(PB = PC);
    Check(FromVarInt64Value(@varint) = k);
    PC := ToVarInt64(i, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarInt64(PB) = i);
    Check(PB = PC);
    if k < 0 then
      k := -k;
    PC := ToVarUInt64(k, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarUint64(PB) = k);
    Check(PB = PC);
    PC := ToVarUInt64(i, @varint);
    Check(PC <> nil);
    PB := @varint;
    Check(FromVarUint64(PB) = i);
    Check(PB = PC);
    PC := @varint;
    for n := 0 to 49 do
      PC := ToVarUInt32(juint + n, PC);
    check(PC <> nil);
    {%H-}st.Init(@varint, PAnsiChar(PC) - PAnsiChar(@varint));
    check(not st.EOF);
    for n := 0 to 48 do
      check(st.VarUInt32 = cardinal(juint + n));
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint + 49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PAnsiChar(PC) - PAnsiChar(@varint));
    check(not st.EOF);
    for n := 0 to 49 do
      check(st.VarUInt64 = cardinal(juint + n));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PAnsiChar(PC) - PAnsiChar(@varint));
    for n := 0 to 48 do
      st.VarNextInt;
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint + 49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PAnsiChar(PC) - PAnsiChar(@varint));
    st.VarNextInt(49);
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint + 49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
  end;
  exit; // code below is speed informative only, without any test
  Timer.Start;
  for i := 0 to 99999 do
    SysUtils.IntToStr(Int64(7777) * rnd.Next);
  NotifyTestSpeed('SysUtils.IntToStr', 100000, 0, @Timer);
  Timer.Start;
  for i := 0 to 99999 do
    StrInt64(@varint[31], Int64(7777) * rnd.Next);
  NotifyTestSpeed('StrInt64', 100000, 0, @Timer);
end;

function LowerCaseAscii7(const S: RawByteString): RawUtf8;
var
  Ch: AnsiChar;
  L: Integer;
  Source, Dest: PAnsiChar;
begin
  L := Length(S);
  Dest := FastSetString(result, L);
  Source := Pointer(S);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and
       (Ch <= 'Z') then
      Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

procedure TTestCoreBase.BaudotCode;
var
  u: RawUtf8;
  b: RawByteString;
  i, j, k: integer;
  P: PAnsiChar absolute u;
const
  CHR: array[0..82] of AnsiChar =
    'abcdefghijklm nopqrstuvwx yzabcdefghijklm nopqrstuvwx yz012345 6789-''3,!:(+)$?@./; ';
begin
  b := AsciiToBaudot('');
  check(b = '');
  b := AsciiToBaudot('abc');
  u := BaudotToAscii(b);
  check(u = 'abc');
  b := AsciiToBaudot('mORMot.net');
  check(BaudotToAscii(b) = 'mormot.net');
  EnsureRawUtf8(b);
  b := b + #0#0#0;
  u := BaudotToAscii(b);
  check(u = 'mormot.net');
  b := AsciiToBaudot('https://synopse.info');
  u := BaudotToAscii(b);
  check(u = 'https://synopse.info');
  b := AsciiToBaudot('abcdef 1234 5678'#13#10'ABCD;/23u'#13#10'op @toto.#com');
  check(b <> '');
  u := BaudotToAscii(b);
  check(u = 'abcdef 1234 5678'#13#10'abcd;/23u'#13#10'op @toto.com');
  for i := 1 to 200 do
  begin
    SetLength(u, i);
    for k := 1 to 50 do
    begin
      for j := 0 to i - 1 do
        P[j] := CHR[rnd.Next(83)];
      b := AsciiToBaudot(u);
      check(BaudotToAscii(b) = u);
    end;
  end;
end;

function ARawSetString: RawByteString;
var
  S: RawByteString;
begin
  S := '123456';
  SetString(result, PAnsiChar(pointer(S)), Length(S));
end;

function ARawFastSetString: RawByteString;
var
  S: RawByteString;
begin
  S := '123456';
  FastSetRawByteString(result, pointer(S), Length(S));
end;

procedure TTestCoreBase.Utf8Slow(Context: TObject);

  procedure CaseFoldingTest;
  const
    _CASEFOLDINGTESTS: array[0..23] of word =
      ($61, $41, $62, $42, $e0, $c0, $fd, $dd, $14b, $14a, $371, $370,
       $3F3, $37F, $451, $401, $435, $415, $442, $422, $4e1, $4e0, $2d00, $10a0);
  var
    i, j: PtrInt;
    up, lo, up2: array[0..10] of AnsiChar;
    src, dst: TByteToAnsiChar;
  begin
    CheckEqual('A', UpperCaseReference('a'));
    CheckEqual('ABC', UpperCaseReference('aBc'));
    CheckEqual('ABCDEF', UpperCaseReference('aBcdEf'));
    CheckEqual('ABCDEFGH', UpperCaseReference('aBcdEfgh'));
    for i := 0 to 11 do
    begin
      lo[Ucs4ToUtf8(_CASEFOLDINGTESTS[i * 2], @lo)] := #0;
      up[Ucs4ToUtf8(_CASEFOLDINGTESTS[i * 2 + 1], @up)] := #0;
      PInt64(@up2)^ := 0;
      Utf8UpperReference(@lo, @up2);
      Check(StrComp(@up, @up2) = 0, 'CaseFolding');
      PInt64(@up2)^ := 0;
      Utf8UpperReference(@lo, @up2, StrLen(@lo));
      Check(StrComp(@up, @up2) = 0, 'CaseFolding');
      CheckEqual(Utf8ICompReference(@lo, @up), 0, 'CaseFoldingComp');
      CheckEqual(Utf8ILCompReference(@lo, @up, StrLen(@lo), StrLen(@up)), 0,
        'CaseFoldingLComp');
    end;
    FillCharFast(src, SizeOf(src), ord('a'));
    for i := 0 to 200 do
    begin
      FillCharFast(dst, SizeOf(dst), 0);
      UpperCopy255Buf(@dst, @src, i)^ := #0;
      Check(StrLen(@dst) = i);
      for j := 0 to i - 1 do
        Check(dst[j] = 'A');
    end;
  end;

  procedure Test(CP: cardinal; const W: WinAnsiString);
  var
    C: TSynAnsiConvert;
    A: RawByteString;
    U: RawUtf8;
  begin
    C := TSynAnsiConvert.Engine(CP);
    CheckEqual(C.CodePage, CP, 'cpa');
    U := C.AnsiToUtf8(W);
    A := C.Utf8ToAnsi(U);
    if W = '' then
      exit;
    {$ifdef HASCODEPAGE}
    CP := StringCodePage(A);
    CheckEqual(CP, C.CodePage, 'cpb');
    CheckEqual(CP, GetCodePage(A), 'cpc');
    {$endif HASCODEPAGE}
    CheckEqual(length(W), length(A));
    CheckUtf8(EqualBuf(W, A), 'CP%', [CP]);
  end;

  procedure CheckTrimCopy(const S: RawUtf8; start, count: PtrInt);
  var
    t: RawUtf8;
  begin
    trimcopy(S, start, count, t);
    checkEqual(t, TrimU(copy(S, start, count)));
  end;

var
  i, j, k, len, len120, lenup100, CP, L, lcid: integer;
  bak, bakj: AnsiChar;
  W: WinAnsiString;
  WS: WideString;
  SU, SU2: SynUnicode;
  WU: array[0..3] of WideChar;
  str: string;
  ss: ShortString;
  up4: RawUcs4;
  U, U2, res, Up, Up2, json, json1, json2, s1, s2, s3: RawUtf8;
  arr, arr2: TRawUtf8DynArray;
  P: PUtf8Char;
  PB: PByte;
  q: RawUtf8;
  Unic: RawByteString;
  Ucs4: RawUcs4;
  WA, HasValidUtf8Avx2: Boolean;
  lng: TLanguage;
  rb1, rb2, rb3: RawByteString;
  eng: TSynAnsiConvert;
const
  ROWIDS: array[0..17] of PUtf8Char = ('id', 'ID', 'iD', 'rowid', 'ROWid',
    'ROWID', 'rowiD', 'ROWId', // ok
    'id2', 'id ', 'idd', 'i', 'rowi', 'row', 'ROWI', 'ROW', 'ROWIDD', 'ROWID ');
  IDPU: array[0..15] of PUtf8Char = ('anything', 't', '1', 'te', 'tE', 'TE',
    'tes', 'test', 'TeSt', 'teS', 'tesT', 'testE', 'T', 'T', '1', 'teste');
  IDPA: array[0..15] of PAnsiChar = (nil, 'T', '1', 'TE', 'TE', 'TE', 'TES',
    'TEST', 'TEST', 'TES', 'TEST', 'TESTE', 't', 'U', '2', 'TESTe');
  CHINESE_TEXT: array[0..8] of byte = (
    $e4, $b8, $ad, $e6, $96, $87, $61, $62, $63);
  UTF8_UCS4: array[0..11] of cardinal = ($01, $81, $801, $fff, $ffff, $10000,
    UNICODE_MAX, UNICODE_MAX + 1, $1000000, $2000000, $10000000, $7f000000);
begin
  // Trim*() functions
  CheckEqual(TrimU(''), '');
  CheckEqual(TrimU('a'), 'a');
  CheckEqual(TrimU(' a'), 'a');
  CheckEqual(TrimU('a '), 'a');
  CheckEqual(TrimU(' a '), 'a');
  CheckEqual(TrimU('ab'), 'ab');
  CheckEqual(TrimU(' ab'), 'ab');
  CheckEqual(TrimU('ab '), 'ab');
  CheckEqual(TrimU(' ab '), 'ab');
  CheckEqual(TrimU(' a b '), 'a b');
  CheckEqual(TrimLeft(''), '');
  CheckEqual(TrimLeft('a'), 'a');
  CheckEqual(TrimLeft(' a'), 'a');
  CheckEqual(TrimLeft('a '), 'a ');
  CheckEqual(TrimLeft(' a '), 'a ');
  CheckEqual(TrimLeft('ab'), 'ab');
  CheckEqual(TrimLeft(' ab'), 'ab');
  CheckEqual(TrimLeft('ab '), 'ab ');
  CheckEqual(TrimLeft(' ab '), 'ab ');
  CheckEqual(TrimLeft(' a b '), 'a b ');
  CheckEqual(TrimRight(''), '');
  CheckEqual(TrimRight('a'), 'a');
  CheckEqual(TrimRight(' a'), ' a');
  CheckEqual(TrimRight('a '), 'a');
  CheckEqual(TrimRight(' a '), ' a');
  CheckEqual(TrimRight('ab'), 'ab');
  CheckEqual(TrimRight(' ab'), ' ab');
  CheckEqual(TrimRight('ab '), 'ab');
  CheckEqual(TrimRight(' ab '), ' ab');
  CheckEqual(TrimRight(' a b '), ' a b');
  CheckEqual(TrimChar('abcd', []), 'abcd');
  CheckEqual(TrimChar('abcd', ['e']), 'abcd');
  CheckEqual(TrimChar('abcd', ['a']), 'bcd');
  CheckEqual(TrimChar('abcdabcd', ['b']), 'acdacd');
  CheckEqual(TrimChar('abcdabcd', ['a']), 'bcdbcd');
  CheckEqual(TrimChar('abcd', ['b']), 'acd');
  CheckEqual(TrimChar('abcd', ['d', 'e']), 'abc');
  CheckEqual(TrimChar('abcd', ['a', 'b']), 'cd');
  CheckEqual(TrimChar('abcd', ['a', 'b', 'c', 'd']), '');
  CheckEqual(TrimChar('aaaa', ['a']), '');
  CheckEqual(TrimChar('aaaab', ['a', 'z']), 'b');
  CheckEqual(TrimChar('baaaa', ['a', 'c']), 'b');
  CheckEqual(OnlyChar('abcd', ['a', 'b', 'c', 'd', 'e']), 'abcd');
  CheckEqual(OnlyChar('abcd', ['a', 'b', 'd', 'e']), 'abd');
  CheckEqual(OnlyChar('abcdabcd', ['a', 'b', 'd', 'e']), 'abdabd');
  CheckEqual(OnlyChar('abcd', []), '');
  CheckEqual(OnlyChar('abcd', ['e']), '');
  CheckEqual(OnlyChar('abcd', ['a']), 'a');
  CheckEqual(OnlyChar('abcd', ['b']), 'b');
  CheckEqual(OnlyChar('abcd', ['d']), 'd');
  CheckEqual(OnlyChar('abcdz', ['d', 'z']), 'dz');
  CheckEqual(OnlyChar('abzcd', ['z']), 'z');
  CheckEqual(OnlyChar('zabzcdz', ['z']), 'zzz');
  // + on RawByteString seems buggy on FPC - at least inconsistent with Delphi
  rb2 := ARawSetString;
  rb1 := rb2 + RawByteString('test');
  CheckEqual(rb1, '123456test', 'ARawSetString1');
  Append(rb2, 'test');
  CheckEqual(rb2, '123456test', 'ARawSetString2');
  rb2 := ARawFastSetString;
  rb3 := 'test';
  {$ifdef FPC} // circumvent FPC RTL oddity on Win32 :(
  SetCodePage(rb3, CP_RAWBYTESTRING, false);
  {$endif FPC}
  rb1 := rb2 + rb3;
  CheckEqual(rb1, '123456test', 'ARawFastSetString1');
  rb1 := ARawFastSetString;
  Append(rb1, 'test');
  CheckEqual(rb1, '123456test', 'ARawFastSetString2');
  Check(SafeFileName(''));
  Check(SafePathName(''));
  Check(SafeFileName('toto'));
  Check(SafeFileName('toto.jpg'));
  Check(SafeFileName('path/toto'));
  Check(SafeFileName('path\toto.jpg'));
  Check(SafeFileName('path../toto'));
  Check(SafeFileName('path..\toto.jpg'));
  Check(SafeFileName('..path/toto'));
  Check(SafeFileName('..path\toto.jpg'));
  Check(not SafeFileName('../toto'));
  Check(not SafeFileName('..\toto.jpg'));
  Check(SafePathName('one/two'));
  Check(SafePathName('one\two'));
  Check(SafePathName('one../two'));
  Check(SafePathName('one..\two'));
  Check(SafePathName('..one/two'));
  Check(SafePathName('..one\two'));
  Check(SafePathName('one/..two'));
  Check(SafePathName('one\..two'));
  Check(SafePathName('one/two..'));
  Check(SafePathName('one\two..'));
  Check(not SafePathName('one/../two'));
  Check(not SafePathName('one\..\two'));
  Check(not SafePathName('one/..'));
  Check(not SafePathName('one\..'));
  Check(not SafePathName('../two'));
  Check(not SafePathName('..\two'));
  Check(not SafePathName('/../two'));
  Check(not SafePathName('\..\two'));
  Check(SafeFileNameU(''));
  Check(SafePathNameU(''));
  Check(SafeFileNameU('toto'));
  Check(SafeFileNameU('toto.jpg'));
  Check(SafeFileNameU('path/toto'));
  Check(SafeFileNameU('path\toto.jpg'));
  Check(SafeFileNameU('path../toto'));
  Check(SafeFileNameU('path..\toto.jpg'));
  Check(SafeFileNameU('..path/toto'));
  Check(SafeFileNameU('..path\toto.jpg'));
  Check(not SafeFileNameU('../toto'));
  Check(not SafeFileNameU('..\toto.jpg'));
  Check(SafePathNameU('one/two'));
  Check(SafePathNameU('one\two'));
  Check(SafePathNameU('one../two'));
  Check(SafePathNameU('one..\two'));
  Check(SafePathNameU('..one/two'));
  Check(SafePathNameU('..one\two'));
  Check(SafePathNameU('one/..two'));
  Check(SafePathNameU('one\..two'));
  Check(SafePathNameU('one/two..'));
  Check(SafePathNameU('one\two..'));
  Check(not SafePathNameU('one/../two'));
  Check(not SafePathNameU('one\..\two'));
  Check(not SafePathNameU('one/..'));
  Check(not SafePathNameU('one\..'));
  Check(not SafePathNameU('../two'));
  Check(not SafePathNameU('..\two'));
  Check(not SafePathNameU('/../two'));
  Check(not SafePathNameU('\..\two'));
  Check(ExtractPath('/var/toto.ext') = '/var/');
  Check(ExtractPath('c:\var\toto.ext') = 'c:\var\');
  Check(ExtractPath('toto.ext') = '');
  Check(ExtractPath('/toto.ext') = '/');
  Check(ExtractPath('c:toto.ext') = 'c:');
  CheckEqual(ExtractPathU('/var/toto.ext'), '/var/');
  CheckEqual(ExtractPathU('c:\var\toto.ext'), 'c:\var\');
  CheckEqual(ExtractPathU('toto.ext'), '');
  CheckEqual(ExtractPathU('/toto.ext'), '/');
  CheckEqual(ExtractPathU('c:toto.ext'), 'c:');
  Check(ExtractName('/var/toto.ext') = 'toto.ext');
  Check(ExtractName('c:\var\toto.ext') = 'toto.ext');
  Check(ExtractName('toto.ext') = 'toto.ext');
  Check(ExtractName('toto') = 'toto');
  Check(ExtractName('/toto.ext') = 'toto.ext');
  Check(ExtractName('c:toto.ext') = 'toto.ext');
  CheckEqual(ExtractNameU('/var/toto.ext'), 'toto.ext');
  CheckEqual(ExtractNameU('c:\var\toto.ext'), 'toto.ext');
  CheckEqual(ExtractNameU('toto.ext'), 'toto.ext');
  CheckEqual(ExtractNameU('toto'), 'toto');
  CheckEqual(ExtractNameU('/toto.ext'), 'toto.ext');
  CheckEqual(ExtractNameU('c:toto.ext'), 'toto.ext');
  Check(GetFileNameWithoutExt('/var/toto.ext') = '/var/toto');
  Check(GetFileNameWithoutExt('c:\var\toto.ext') = 'c:\var\toto');
  Check(ExtractExt('toto.ext') = '.ext');
  Check(ExtractExt('toto.ext', true) = 'ext');
  Check(ExtractExt('name') = '');
  Check(ExtractExt('name.') = '.');
  Check(ExtractExt('.ext') = '');
  Check(ExtractExt('/var/toto.ext') = '.ext');
  Check(ExtractExt('/var/toto.ext', true) = 'ext');
  Check(ExtractExt('c:\var\toto.ext') = '.ext');
  Check(ExtractExt('c:\var\toto.ext', true) = 'ext');
  Check(ExtractExt('/var/toto/') = '');
  Check(ExtractExt('/var/toto') = '');
  CheckEqual(ExtractExtU('toto.ext'), '.ext');
  CheckEqual(ExtractExtU('toto.ext', true), 'ext');
  CheckEqual(ExtractExtU('name'), '');
  CheckEqual(ExtractExtU('name.'), '.');
  CheckEqual(ExtractExtU('.ext'), '');
  CheckEqual(ExtractExtU('/var/toto.ext'), '.ext');
  CheckEqual(ExtractExtU('/var/toto.ext', true), 'ext');
  CheckEqual(ExtractExtU('c:\var\toto.ext'), '.ext');
  CheckEqual(ExtractExtU('c:\var\toto.ext', true), 'ext');
  CheckEqual(ExtractExtU('/var/toto/'), '');
  CheckEqual(ExtractExtU('/var/toto'), '');
  Check(NormalizeFileName('') = '');
  Check(NormalizeFileName('toto.ext') = 'toto.ext');
  {$ifdef OSWINDOWS}
  Check(NormalizeFileName('var\toto.ext') = 'var\toto.ext');
  Check(NormalizeFileName('\var\toto.ext') = '\var\toto.ext');
  Check(NormalizeFileName('titi\var\toto.ext') = 'titi\var\toto.ext');
  Check(NormalizeFileName('\var\') = '\var\');
  Check(NormalizeFileName('var/toto.ext') = 'var\toto.ext');
  Check(NormalizeFileName('/var/toto.ext') = '\var\toto.ext');
  Check(NormalizeFileName('titi/var/toto.ext') = 'titi\var\toto.ext');
  Check(NormalizeFileName('/var/') = '\var\');
  Check(NormalizeFileName('\var/') = '\var\');
  Check(NormalizeFileName('/var\') = '\var\');
  U := '';
  NormalizeFileNameU(U);
  CheckEqual(U, '');
  U := '/var';
  NormalizeFileNameU(U);
  CheckEqual(U, '\var');
  U := '/var\';
  NormalizeFileNameU(U);
  CheckEqual(U, '\var\');
  {$else}
  Check(NormalizeFileName('var\toto.ext') = 'var/toto.ext');
  Check(NormalizeFileName('\var\toto.ext') = '/var/toto.ext');
  Check(NormalizeFileName('titi\var\toto.ext') = 'titi/var/toto.ext');
  Check(NormalizeFileName('\var\') = '/var/');
  Check(NormalizeFileName('var/toto.ext') = 'var/toto.ext');
  Check(NormalizeFileName('/var/toto.ext') = '/var/toto.ext');
  Check(NormalizeFileName('titi/var/toto.ext') = 'titi/var/toto.ext');
  Check(NormalizeFileName('/var/') = '/var/');
  Check(NormalizeFileName('\var/') = '/var/');
  Check(NormalizeFileName('/var\') = '/var/');
  U := '';
  NormalizeFileNameU(U);
  CheckEqual(U, '');
  U := '/var';
  NormalizeFileNameU(U);
  CheckEqual(U, '/var');
  U := '/var\';
  NormalizeFileNameU(U);
  CheckEqual(U, '/var/');
  {$endif OSWINDOWS}
  CaseFoldingTest;
  for i := 0 to high(ROWIDS) do
    Check(isRowID(ROWIDS[i]) = (i < 8));
  U := 'old1,old2,old3';
  Check(not RenameInCsv('old', 'new', U));
  Check(RenameInCsv('old1', 'n1', U));
  Check(U = 'n1,old2,old3');
  Check(RenameInCsv('old2', 'n2', U));
  Check(not RenameInCsv('old2', 'news2', U));
  Check(RenameInCsv('old3', 'news3', U));
  Check(U = 'n1,n2,news3');
  Check(RenameInCsv(U, '1-2-3', U, '!'));
  Check(U = '1-2-3');
  Check(RenameInCsv('2', 'bee', U, '-'));
  Check(RenameInCsv('1', 'ah', U, '-'));
  Check(RenameInCsv('3', 'see', U, '-'));
  Check(U = 'ah-bee-see');
  for i := 0 to High(IDPU) do
    Check(IdemPChar(IDPU[i], IDPA[i]) = (i < 12));
  res :=
    '{"result":[{"000001000013":{"00100000000016":[1534510257860,103100,2000,' +
    '103108,1004,104132],"00100000000026":[1534510257860,12412,2000,12420,1004,12420],' +
    '"00100000000036":[1534510257860,1378116,2000,1378112,1004,1378112],"00100000000056":' +
    '[1534510257860,479217551,2000,479217551],"00100000000076":[1534510257860,136079943,' +
    '2000,136079943,1004,136079944],"00100000000086":[1534510257860,1648800821,2000,' +
    '1648801020,1004,1648801119],"00100000000096":[1534510257860,87877677,2000,87877678,' +
    '1004,87877678],"001000000000ec":[1534510257860,1.64,2000,1.64],"001000000000fc":[' +
    '1534510257860,1.72,2000,1.72],"0010000000010c":[1534510257860,1.64,2000,1.64],"' +
    '00100000000196":[1534510257860,0,2000,0]}}]}';
  i := StrLenSafe(@res[1]);
  check(mormot.core.base.StrLen(@res[1]) = i);
  res := 'one,two,three';
  Check(IdemPCharArrayBy2(nil, 'ONTWTH') < 0);
  CheckEqual(IdemPCharArrayBy2(pointer(res), 'OFTWTH'), -1);
  CheckEqual(IdemPCharArrayBy2(pointer(res), 'ONTWTH'), 0);
  CheckEqual(IdemPCharArrayBy2(pointer(res), 'TWONTW'), 1);
  CheckEqual(IdemPCharArrayBy2(pointer(res), 'TWTHON'), 2);
  CheckEqual(IdemPCharSep('one','one|two|three'), -1);
  CheckEqual(IdemPCharSep('one','ONE|TWO|THREE|'), 0);
  CheckEqual(IdemPCharSep('one','ZERO|ONE|TWO|THREE|'), 1);
  CheckEqual(IdemPCharSep('One','ONE|'), 0);
  CheckEqual(IdemPCharSep('OnE','ONE|'), 0);
  CheckEqual(IdemPCharSep('ONE','ONE|'), 0);
  CheckEqual(IdemPCharSep('oN','ONE|ON|'), 1);
  CheckEqual(IdemPCharSep('ONE?','ONE|'), 0);
  CheckEqual(IdemPCharSep('0ne','ONE|'), -1);
  CheckEqual(IdemPCharSep('one','ZERO|ONE|'), 1);
  CheckEqual(IdemPCharSep('tWo','ZERO|ONE|TWO|THREE|'), 2);
  CheckEqual(IdemPCharSep('threE','ZERO|ONE|TWO|THREE|'), 3);
  CheckEqual(IdemPCharSep('threEf','ZERO|ONE|TWO|THREE|'), 3);
  CheckEqual(IdemPCharSep('thre0','ZERO|ONE|TWO|THREE|'), -1);
  Check(StartWith('three', 'THREE'));
  for i := 1 to length(res) do
    Check(StartWith(res, UpperCase(copy(res, 1, i))));
  Check(EndWith('three', 'THREE'));
  Check(EndWith(res, 'E'));
  Check(EndWith(res, 'THREE'));
  Check(EndWith(res, ',THREE'));
  for i := 1 to length(res) do
    Check(EndWith(res, UpperCase(copy(res, i, 100))));
  Check(not EndWith(res, ',THREe'));
  Check(not EndWith(res, res));
  Check(not EndWith('t', ',THREe'));
  Check(not EndWith('thre', ',THREe'));
  Check(EndWithArray(res, []) < 0);
  Check(EndWithArray(res, ['E', 'F']) = 0);
  Check(EndWithArray(res, ['ONE', 'THREE']) = 1);
  Check(EndWithArray(res, ['ONE', 'three', 'THREE']) = 2);
  Check(EndWithArray(res, ['ONE', '', 'THREE']) = 1);
  Check(EndWithArray(res, ['ONE', 'three', 'THREe']) < 0);
  CheckEqual(TrimControlChars(''), '');
  CheckEqual(TrimControlChars(' '), '');
  CheckEqual(TrimControlChars('    '), '');
  CheckEqual(TrimControlChars('a'), 'a');
  CheckEqual(TrimControlChars('a '), 'a');
  CheckEqual(TrimControlChars(' a '), 'a');
  CheckEqual(TrimControlChars('a '), 'a');
  CheckEqual(TrimControlChars('a  '), 'a');
  CheckEqual(TrimControlChars(' a  '), 'a');
  CheckEqual(TrimControlChars('  a  '), 'a');
  CheckEqual(TrimControlChars('a  '), 'a');
  CheckEqual(TrimControlChars('a  b'), 'ab');
  CheckEqual(TrimControlChars('synopse.info, www.synopse.info'), 'synopse.info,www.synopse.info');
  CheckEqual(Split(res, ','), 'one', 'sp1');
  CheckEqual(Split(res, ',', s1), 'two,three', 'sp2');
  CheckEqual(s1, 'one');
  CheckEqual(Split(res, ',', s1, {toupper=}true), 'TWO,THREE');
  CheckEqual(s1, 'ONE');
  Check(Split(res, '*') = res);
  Check(Split(res, ',', 5) = 'two');
  Check(Split(res, '*', 6) = 'wo,three');
  CheckEqual(Split('titi-tata-toto', ['-'], [@s1, @s2, @s3]), 3, 'split3');
  CheckEqual(s1, 'titi', 'split3a');
  CheckEqual(s2, 'tata', 'split3b');
  CheckEqual(s3, 'toto', 'split3c');
  CheckEqual(Split('--', ['-'], [@s1, @s2, @s3]), 3, 'split0');
  CheckEqual(s1, '', 'split0a');
  CheckEqual(s2, '', 'split0b');
  CheckEqual(s3, '', 'split0c');
  CheckEqual(Split('a-b-c', ['-'], [@s1, @s2, @s3]), 3, 'split1');
  CheckEqual(s1, 'a', 'split1a');
  CheckEqual(s2, 'b', 'split1b');
  CheckEqual(s3, 'c', 'split1c');
  CheckEqual(Split('-b-c', ['-'], [@s1, @s2, @s3]), 3, 'split2');
  CheckEqual(s1, '', 'split2a');
  CheckEqual(s2, 'b', 'split2b');
  CheckEqual(s3, 'c', 'split2c');
  CheckEqual(Split('a-b-', ['-'], [@s1, @s2, @s3]), 3, 'split4');
  CheckEqual(s1, 'a', 'split4a');
  CheckEqual(s2, 'b', 'split4b');
  CheckEqual(s3, '', 'split4c');
  CheckEqual(SplitRight(res, ','), 'three');
  CheckEqual(SplitRight(res, 'r'), 'ee');
  CheckEqual(SplitRights(res, ','), 'three');
  CheckEqual(SplitRights(res, '!,'), 'three');
  CheckEqual(SplitRights(res, ',!'), 'three');
  CheckEqual(SplitRights(res, '!thr'), 'ee');
  Check(mormot.core.base.StrLen(nil) = 0);
  for i := length(res) + 1 downto 1 do
    Check(mormot.core.base.StrLen(Pointer(@res[i])) = length(res) - i + 1);
  Check(StrLenSafe(nil) = 0);
  for i := length(res) + 1 downto 1 do
    Check(StrLenSafe(Pointer(@res[i])) = length(res) - i + 1);
  CsvToRawUtf8DynArray(pointer(res), arr);
  CheckEqual(length(arr), 3);
  Check(arr[0] = 'one');
  Check(arr[1] = 'two');
  Check(arr[2] = 'three');
  arr2 := arr; // pointer
  Check(RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals');
  Check(RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains');
  Check(RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame1');
  Check(RawUtf8DynArraySame(arr, arr2, {insens=}false), 'RawUtf8DynArraySame1i');
  arr2 := copy(arr);
  CheckEqual(length(arr2), 3);
  Check(RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals2');
  Check(RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains2');
  Check(RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame2');
  Check(RawUtf8DynArraySame(arr, arr2, true), 'RawUtf8DynArraySame2i');
  SetLength(arr, 2);
  Check(not RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals3');
  Check(RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains3a');
  Check(not RawUtf8DynArrayContains(arr2, arr), 'RawUtf8DynArrayContains3b');
  Check(not RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame3');
  Check(not RawUtf8DynArraySame(arr, arr2, true), 'RawUtf8DynArraySame3i');
  arr := CsvToRawUtf8DynArray('two,three,one');
  Check(not RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals4');
  Check(RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains4');
  Check(RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame4');
  Check(RawUtf8DynArraySame(arr, arr2, true), 'RawUtf8DynArraySame4i');
  arr := CsvToRawUtf8DynArray('two,ONE,three');
  Check(not RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals4');
  Check(not RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains4');
  Check(RawUtf8DynArrayContains(arr, arr2, {insens=}true), 'RawUtf8DynArrayContains4i');
  Check(not RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame4');
  Check(RawUtf8DynArraySame(arr, arr2, true), 'RawUtf8DynArraySame4i');
  arr := CsvToRawUtf8DynArray('two,one,one');
  CheckEqual(RawUtf8ArrayToCsv(arr), 'two,one,one');
  CheckEqual(RawUtf8ArrayToCsv(arr, ''), 'twooneone');
  CheckEqual(RawUtf8ArrayToCsv(arr, '/', {rev=}true), 'one/one/two');
  Check(not RawUtf8DynArrayEquals(arr, arr2), 'RawUtf8DynArrayEquals5');
  Check(RawUtf8DynArrayContains(arr, arr2), 'RawUtf8DynArrayContains5');
  Check(RawUtf8DynArrayContains(arr, arr2, {insens=}true), 'RawUtf8DynArrayContains5i');
  Check(not RawUtf8DynArraySame(arr, arr2), 'RawUtf8DynArraySame5');
  Check(not RawUtf8DynArraySame(arr, arr2, true), 'RawUtf8DynArraySame5i');
  CheckEqual(Join([]), '');
  CheckEqual(Join(['one']), 'one');
  CheckEqual(Join(['one', 'two']), 'onetwo');
  CheckEqual(Join(['', 'one', 'two']), 'onetwo');
  CheckEqual(Join(['one', 'two', ' three ']), 'onetwo three ');
  CheckEqual(Join(['one', 'two', '', 'three']), 'onetwothree');
  CheckEqual(JoinCsv('', []), '');
  CheckEqual(JoinCsv('', ['one']), 'one');
  CheckEqual(JoinCsv('', ['one', 'two']), 'onetwo');
  CheckEqual(JoinCsv('', ['', 'one', 'two']), 'onetwo');
  CheckEqual(JoinCsv('', ['one', 'two', ' three ']), 'onetwo three ');
  CheckEqual(JoinCsv('', ['one', 'two', '', 'three']), 'onetwothree');
  CheckEqual(JoinCsv(',', []), '');
  CheckEqual(JoinCsv(',', ['', '']), ',');
  CheckEqual(JoinCsv(',', ['one']), 'one');
  CheckEqual(JoinCsv(',', ['one', 'two']), 'one,two');
  CheckEqual(JoinCsv(',', ['one', 'two', ' three ']), 'one,two, three ');
  CheckEqual(JoinCsv(',', ['one', 'two', '', 'three']), 'one,two,,three');
  CheckEqual(JoinCsv('//', ['one'], true), 'one');
  CheckEqual(JoinCsv('//', ['one', 'two'], true), 'two//one');
  CheckEqual(JoinCsv('//', ['1', '2', '3'], true), '3//2//1');
  CheckEqual(JoinCsv(',', ['one', 'two'], {reverse=}true), 'two,one');
  CheckEqual(JoinCsv(',', ['one', 'two', 'three '], true), 'three ,two,one');
  CheckEqual(JoinCsv(',', ['one', 'two', ''], true), ',two,one');
  CheckEqual(JoinCsv(',', ['one'], true), 'one');
  CheckEqual(JoinCsv(',', ['', ''], true), ',');
  Finalize(arr);
  CsvToRawUtf8DynArray(res, ',', '', arr);
  CheckEqual(length(arr), 3);
  Check(arr[0] = 'one');
  Check(arr[1] = 'two');
  Check(arr[2] = 'three');
  Finalize(arr);
  CsvToRawUtf8DynArray('one=?,two=?,three=?', '=?,', '=?', arr);
  CheckEqual(length(arr), 3);
  Check(arr[0] = 'one');
  Check(arr[1] = 'two');
  Check(arr[2] = 'three');
  Finalize(arr);
  res := '-1,25,0';
  CsvToRawUtf8DynArray(pointer(res), arr);
  CheckEqual(length(arr), 3);
  Check(arr[0] = '-1');
  Check(arr[1] = '25');
  Check(arr[2] = '0');
  Finalize(arr);
  CsvToRawUtf8DynArray('AA,BB,CC,DD', ',', ',', arr);
  CheckEqual(length(arr), 4);
  Check(arr[0] = 'AA');
  Check(arr[1] = 'BB');
  Check(arr[2] = 'CC');
  Check(arr[3] = 'DD');
  Finalize(arr);
  CsvToRawUtf8DynArray('A,B,C,D', ',', ',', arr);
  CheckEqual(length(arr), 4);
  Check(arr[0]='A');
  Check(arr[1]='B');
  Check(arr[2]='C');
  Check(arr[3]='D');
  Finalize(arr);
  CsvToRawUtf8DynArray('item1   item2    item3', arr, {sep=}' ',
    {TrimItems=}true , {AddVoidItems=}false);
  CheckEqual(length(arr), 3);
  CheckEqual(arr[0], 'item1');
  CheckEqual(arr[1], 'item2');
  CheckEqual(arr[2], 'item3');
  CheckEqual(AddPrefixToCsv('One,Two,Three', 'Pre'), 'PreOne,PreTwo,PreThree');
  CheckEqual(CsvOfValue('?', 3), '?,?,?');
  CheckEqual(GetUnQuoteCsvItem('"""one,""","two "', 1, ',', '"'), 'two ');
  CheckEqual(GetUnQuoteCsvItem('''''''one,''''''', 0), '''one,''');
  CheckEqual(GetUnQuoteCsvItem('"""one,', 0, ',', '"'), '');
  Check(not CsvContains('', 'b'));
  Check(not CsvContains('a', ''));
  Check(CsvContains('a', 'a'));
  Check(CsvContains('ab', 'ab'));
  Check(not CsvContains('a', 'b'));
  Check(not CsvContains('a', 'ab'));
  Check(not CsvContains('ab', 'a'));
  Check(CsvContains('a,b,c', 'a'));
  Check(CsvContains('a,b,c', 'b'));
  Check(CsvContains('a,b,c', 'c'));
  Check(not CsvContains('a,b,c', 'A'));
  Check(not CsvContains('a,b,c', ''));
  Check(CsvContains('aa,bb,cc', 'aa'));
  Check(CsvContains('aa,bb,cc', 'bb'));
  Check(CsvContains('aa,bb,cc', 'cc'));
  Check(not CsvContains('aa,bb,cc', 'cb'));
  Check(not CsvContains('aa,bb,cc', 'a'));
  Check(not CsvContains('', 'b', ',', false));
  Check(not CsvContains('a', '', ',', false));
  Check(CsvContains('a', 'a', ',', false));
  Check(CsvContains('ab', 'ab', ',', false));
  Check(CsvContains('a', 'A', ',', false));
  Check(CsvContains('ab', 'Ab', ',', false));
  Check(CsvContains('ab', 'AB', ',', false));
  Check(CsvContains('ab', 'aB', ',', false));
  Check(not CsvContains('a', 'b', ',', false));
  Check(not CsvContains('a', 'ab', ',', false));
  Check(not CsvContains('ab', 'a', ',', false));
  Check(CsvContains('a,b,c', 'a', ',', false));
  Check(CsvContains('a,b,c', 'b', ',', false));
  Check(CsvContains('a,b,c', 'c', ',', false));
  Check(CsvContains('a,b,c', 'A', ',', false));
  Check(CsvContains('a,b,c', 'B', ',', false));
  Check(CsvContains('a,b,c', 'C', ',', false));
  Check(not CsvContains('a,b,c', '', ',', false));
  Check(CsvContains('aa,bb,cc', 'aa', ',', false));
  Check(CsvContains('aa,bb,cc', 'bb', ',', false));
  Check(CsvContains('aa,bb,cc', 'cc', ',', false));
  Check(CsvContains('aa,bb,cc', 'AA', ',', false));
  Check(CsvContains('aa,bb,cc', 'Bb', ',', false));
  Check(CsvContains('aa,bb,cc', 'cC', ',', false));
  Check(not CsvContains('aa,bb,cc', 'cb', ',', false));
  Check(not CsvContains('aa,bb,cc', 'a', ',', false));
  CheckEqual(GetFirstCsvItem(''), '');
  CheckEqual(GetFirstCsvItem('a'), 'a');
  CheckEqual(GetFirstCsvItem('ab'), 'ab');
  CheckEqual(GetFirstCsvItem('ab,'), 'ab');
  CheckEqual(GetFirstCsvItem('ab,c'), 'ab');
  CheckEqual(GetFirstCsvItem('ab,c,de,fg'), 'ab');
  CheckEqual(GetFirstCsvItem(','), '');
  CheckEqual(GetFirstCsvItem(',a'), '');
  Check(FormatSql('abcd', [U], [{%H-}WS]) = 'abcd');
  Check(MakePath([]) = '');
  Check(MakePath([], true) = '');
  Check(MakePath([1], false, '/') = '1');
  Check(MakePath([1], true, '/') = '1/');
  Check(MakePath([1, 2, '3'], false, '/') = '1/2/3');
  Check(MakePath([1, '2/', 3], false, '/') = '1/2/3');
  Check(MakePath(['1/', 2, 3], false, '/') = '1/2/3');
  Check(MakePath([1, 2, '3/'], false, '/') = '1/2/3/');
  Check(MakePath([1, '', 2, '3/'], false, '/') = '1/2/3/');
  Check(MakePath([1, 2, 3], true, '/') = '1/2/3/');
  Check(MakePath([1, 2, '3'], true, '/') = '1/2/3/');
  Check(MakePath([1, '2/', 3], true, '/') = '1/2/3/');
  Check(MakePath(['1/', 2, 3], true, '/') = '1/2/3/');
  Check(MakePath([1, 2, '3/'], true, '/') = '1/2/3/');
  Check(MakePath([1, '', 2, '3/'], true, '/') = '1/2/3/');
  Check(MakeFileName([]) = '');
  Check(MakeFileName(['toto', 'doc']) = 'toto.doc');
  {$ifdef OSWINDOWS}
  Check(MakeFileName([1, 2, 'doc'], false) = '1\2\doc');
  Check(MakeFileName([1, 2, 'doc'], true) = '1\2.doc');
  Check(MakeFileName([1, '', 2, '.doc'], true) = '1\2.doc');
  {$else}
  Check(MakeFileName([1, 2, 'doc'], false) = '1/2/doc');
  Check(MakeFileName([1, 2, 'doc'], true) = '1/2.doc');
  Check(MakeFileName([1, '', 2, '.doc'], true) = '1/2.doc');
  {$endif OSWINDOWS}
  CheckEqual(MakeCsv([]), '');
  CheckEqual(MakeCsv([], true), '');
  CheckEqual(MakeCsv([1]), '1');
  CheckEqual(MakeCsv([1], true, '+'), '1+');
  CheckEqual(MakeCsv([1, 2, 3]), '1,2,3');
  CheckEqual(MakeCsv([1, '2', 3], true), '1,2,3,');
  CheckEqual(MakeCsv([1, '2,', 3]), '1,2,3');
  CheckEqual(MakeCsv([1, '2,', 3], true), '1,2,3,');
  CheckEqual(MakeCsv([1, '2 ,', 3]), '1,2 ,3');
  CheckEqual(Make([]), '');
  CheckEqual(Make([1]), '1');
  CheckEqual(Make([1, 2, 3]), '123');
  CheckEqual(Make([1, '', 2, 3]), '123');
  CheckEqual(Make([1, '2', 3]), '123');
  CheckEqual(Make([1, '2 ,', 3]), '12 ,3');
  Check(MakeString([]) = '');
  Check(MakeString([1]) = '1');
  Check(MakeString([1, 2, 3]) = '123');
  Check(MakeString([1, '2', 3]) = '123');
  Check(MakeString([1, '2 ,', 3]) = '12 ,3');
  U := '';
  Append(U, []);
  CheckEqual(U, '');
  Append(U, [1]);
  CheckEqual(U, '1');
  Append(U, [2, '34', 5]);
  CheckEqual(U, '12345');
  Append(U, []);
  CheckEqual(U, '12345');
  Append(U, [6]);
  CheckEqual(U, '123456');
  U := '';
  Prepend(U, []);
  CheckEqual(U, '');
  Prepend(U, [1]);
  CheckEqual(U, '1');
  Prepend(U, [2, '34', 5]);
  CheckEqual(U, '23451');
  Prepend(U, []);
  CheckEqual(U, '23451');
  Prepend(U, [6]);
  CheckEqual(U, '623451');
  U := '';
  AppendLine(U, []);
  CheckEqual(U, '');
  AppendLine(U, ['a', 1]);
  CheckEqual(U, 'a1');
  AppendLine(U, [2, 3, 4, 5]);
  CheckEqual(U, 'a1'#13#10'2345');
  AppendLine(U, ['bcdef']);
  CheckEqual(U, 'a1'#13#10'2345'#13#10'bcdef');
  Append(U, #13#10);
  CheckEqual(U, 'a1'#13#10'2345'#13#10'bcdef'#13#10);
  AppendLine(U, ['ghij']);
  CheckEqual(U, 'a1'#13#10'2345'#13#10'bcdef'#13#10'ghij');
  U := QuotedStr('', '"');
  CheckEqual(U, '""');
  U := QuotedStr('abc', '"');
  CheckEqual(U, '"abc"');
  U := QuotedStr('a"c', '"');
  CheckEqual(U, '"a""c"');
  U := QuotedStr('abcd"efg', '"');
  CheckEqual(U, '"abcd""efg"');
  U := QuotedStr('abcd""efg', '"');
  CheckEqual(U, '"abcd""""efg"');
  U := QuotedStr('abcd"e"fg"', '"');
  CheckEqual(U, '"abcd""e""fg"""');
  U := QuotedStr('"abcd"efg', '"');
  CheckEqual(U, '"""abcd""efg"');
  U := QuotedStr('', '#'); // also test for custom quote
  CheckEqual(U, '##');
  U := QuotedStr('abc', '#');
  CheckEqual(U, '#abc#');
  U := QuotedStr('a#c', '#');
  CheckEqual(U, '#a##c#');
  U := QuotedStr('abcd#efg', '#');
  CheckEqual(U, '#abcd##efg#');
  U := QuotedStr('abcd##efg', '#');
  CheckEqual(U, '#abcd####efg#');
  U := QuotedStr('abcd#e#fg#', '#');
  CheckEqual(U, '#abcd##e##fg###');
  U := QuotedStr('#abcd#efg', '#');
  CheckEqual(U, '###abcd##efg#');
  for i := 0 to 1000 do
  begin
    len := i * 5;
    W := RandomAnsi7(len, CP_WINANSI);
    CheckEqual(length(W), len);
    lenup100 := len;
    if lenup100 > 100 then
      lenup100 := 100;
    str := Ansi7ToString(W); // should be fine on any code page
    if len > 0 then
    begin
      CheckEqual(length(str), len);
      CheckEqual(PosExString(str[1], str), 1);
      if str[1] <> str[2] then
      begin
        CheckEqual(PosExString(str[2], str), 2);
        if (str[1] <> str[2]) and
           (str[2] <> str[3]) and
           (str[1] <> str[3]) then
          CheckEqual(PosExString(str[3], str), 3);
      end;
      for j := 1 to lenup100 do
      begin
        CheckEqual(PosExString(#13, str, j), 0);
        CheckEqual(PosExString(str[j], str, j), j);
        if (j > 1) and
           (str[j - 1] <> str[j]) then
          CheckEqual(PosExString(str[j], str, j - 1), j);
        k := PosExString(str[j], str);
        check((k > 0) and
             (str[k] = str[j]));
      end;
    end
    else
      check(PosExString(#0, str) = 0);
    for CP := 1250 to 1258 do
      Test(CP, W);
    Test(932, W);
    Test(949, W);
    Test(874, W);
    Test(CP_UTF8, W); // note: CP_UTF16 is not a true ANSI charset for Test()
    L := Length(W);
    if L and 1 <> 0 then
      SetLength(W, L - 1); // force exact UTF-16 buffer length
    W := RandomWinAnsi(len);
    Check(length(W) = len);
    U := WinAnsiToUtf8(W);
    Check(length(U) >= len);
    check(IsValidUtf8(U), 'IsValidUtf8');
    P := UniqueRawUtf8(U);
    check(IsValidUtf8Ptr(P), 'IsValidUtf8Ptr');
    check(PosChar(P, #10) = nil);
    if len > 0 then
    begin
      CheckEqual(PosEx(U[1], U), 1);
      CheckEqual(PosExChar(U[1], U), 1);
      check(PosChar(P, P[0]) = @P[0], 'PosChar0');
      if (len > 1) and
         (U[1] <> U[2]) then
      begin
        CheckEqual(PosEx(U[2], U), 2);
        CheckEqual(PosExChar(U[2], U), 2);
        check(PosChar(P, P[1]) = @P[1], 'PosChar1');
        if (len > 2) and
           (U[1] <> U[2]) and
           (U[2] <> U[3]) and
           (U[1] <> U[3]) then
        begin
          CheckEqual(PosEx(U[3], U), 3);
          CheckEqual(PosExChar(U[3], U), 3);
          check(PosChar(P, P[2]) = @P[2], 'PosChar2');
        end;
      end;
    end;
    if length(U) > 120 then
      len120 := Utf8TruncatedLength(U, 120)
    else
      len120 := 0;
    Check(IsValidUtf8Buffer(P, len120), 'IsValidUtf8Buffer truncated');
    {$ifdef ASMX64AVXNOCONST}
    HasValidUtf8Avx2 := (cpuHaswell in X64CpuFeatures);
    if HasValidUtf8Avx2 then
    begin
      check(IsValidUtf8Small(U), 'IsValidUtf8Pas');
      Check(IsValidUtf8Pas(P, len120), 'IsValidUtf8Pas120');
    end;
    {$else}
    HasValidUtf8Avx2 := false; // IsValidUtf8Buffer = @IsValidUtf8Pas
    {$endif ASMX64AVXNOCONST}
    for j := 1 to lenup100 do
    begin
      check(PosChar(P, U[j])^ = U[j], 'PosCharj');
      // validates with offset parameter
      CheckEqual(PosEx(#13, U, j), 0);
      CheckEqual(PosEx(U[j], U, j), j);
      if (j > 1) and
         (U[j - 1] <> U[j]) then
        CheckEqual(PosEx(U[j], U, j - 1), j);
      k := PosEx(U[j], U);
      check((k > 0) and
            (U[k] = U[j]));
      CheckEqual(PosExChar(U[j], U), k);
      if len120 <> 0 then
      begin
        bak := P[len120];
        P[len120] := #0; // no need to go any further
        bakj := P[j - 1];
        P[j - 1] := AnsiChar(ord(P[j - 1]) xor 128); // invalidate the UTF-8 content
        check(not IsValidUtf8Buffer(P, len120), 'IsValidUtf8 up100');
        if HasValidUtf8Avx2 then
          check(not IsValidUtf8Pas(P, len120), 'IsValidUtf8Pas up100');
        P[j - 1] := bakj;
        check(IsValidUtf8Buffer(P, len120), 'IsValidUtf8 restored');
        if HasValidUtf8Avx2 then
          check(IsValidUtf8Pas(P, len120), 'IsValidUtf8Pas restored');
        P[j - 1] := #0;
        check(not IsValidUtf8NotVoid(P, len120), 'IsValidUtf8 0');
        if HasValidUtf8Avx2 then
          check(not IsValidUtf8Pas(P, len120), 'IsValidUtf8Pas 0');
        P[j - 1] := bakj;
        check(IsValidUtf8Buffer(P, len120), 'IsValidUtf8 final');
        P[len120] := bak;
      end;
    end;
    json := FormatJson('{"a":?,"b":%}', [i], [U]);
    Check(IsValidJson(json, {strict=}true));
    json1 := JsonReformat(json, jsonEscapeUnicode);
    Check(IsValidJson(json1, true));
    Check(IsAnsiCompatible(U) or (PosEx('\u', json1) > 0));
    json2 := JsonReformat(json1, jsonNoEscapeUnicode);
    CheckEqual(json2, json, 'jeu2');
    Unic := Utf8DecodeToUnicodeRawByteString(U);
    CheckEqual(Utf8ToWinAnsi(U), W);
    CheckEqual(WinAnsiConvert.Utf8ToAnsi(WinAnsiConvert.AnsiToUtf8(W)), W);
    CheckEqual(WinAnsiConvert.UnicodeStringToAnsi(WinAnsiConvert.AnsiToUnicodeString(W)), W);
    if CurrentAnsiConvert.InheritsFrom(TSynAnsiFixedWidth) then
    begin
      CheckEqual(CurrentAnsiConvert.Utf8ToAnsi(CurrentAnsiConvert.AnsiToUtf8(W)), W);
      CheckEqual(CurrentAnsiConvert.UnicodeStringToAnsi(CurrentAnsiConvert.AnsiToUnicodeString(W)), W);
    end;
    res := RawUnicodeToUtf8(pointer(Unic), length(Unic) shr 1);
    CheckEqual(res, U);
    WinAnsiConvert.UnicodeBufferToAnsiVar(pointer(Unic), length(Unic) shr 1, rb1);
    CheckEqual(rb1, W);
    WS := Utf8ToWideString(U);
    CheckEqual(length(WS), length(Unic) shr 1);
    if WS <> '' then
      Check(CompareMem(pointer(WS), pointer(Unic), length(WS) * SizeOf(WideChar)));
    CheckEqual(integer(Utf8ToUnicodeLength(Pointer(U))), length(WS));
    SU := Utf8ToSynUnicode(U);
    CheckEqual(length(SU), length(Unic) shr 1);
    if SU <> '' then
      Check(CompareMem(pointer(SU), pointer(Unic), length(Unic)), 'Utf8ToSU');
    WA := IsWinAnsi(pointer(Unic));
    Check(IsWinAnsi(pointer(Unic), length(Unic) shr 1) = WA);
    Check(IsWinAnsiU(pointer(U)) = WA);
    Up := mormot.core.unicode.UpperCase(U);
    Check(IsUpper(Up));
    CheckEqual(mormot.core.unicode.UpperCase(mormot.core.unicode.LowerCase(U)), Up);
    CheckEqual(Utf8IComp(pointer(U), pointer(U)), 0);
    CheckEqual(Utf8IComp(pointer(U), pointer(Up)), 0);
    CheckEqual(Utf8ILComp(pointer(U), pointer(U), length(U), length(U)), 0);
    CheckEqual(Utf8ILComp(pointer(U), pointer(Up), length(U), length(Up)), 0);
    CheckEqual(Utf8ICompReference(pointer(U), pointer(U)), 0);
    CheckEqual(Utf8ILCompReference(pointer(U), pointer(U), length(U), length(U)), 0);
    CheckEqual(Utf8CompareOS(pointer(U), pointer(U)), 0);
    CheckEqual(CompareInteger(Utf8CompareOS(pointer(U), pointer(Up)), 0),
              -CompareInteger(Utf8CompareOS(pointer(Up), pointer(U)), 0));
    CheckEqual(CompareInteger(Utf8CompareIOS(pointer(U), pointer(Up)), 0),
              -CompareInteger(Utf8CompareIOS(pointer(Up), pointer(U)), 0));
    CheckEqual(Utf8CompareIOS(pointer(U), pointer(U)), 0);
    if Unicode_CodePage = CP_WINANSI then
      CheckEqual(Utf8CompareIOS(pointer(U), pointer(Up)), 0);
    //for j := 1 to 5000 do
    try
      //W := RandomWinAnsi(len);
      //U := WinAnsiToUtf8(W);
      //check(IsValidUtf8(U), 'IsValidUtf8U');
      //Up := mormot.core.unicode.UpperCase(U);
      up4 := Utf8ToRawUcs4(U);
      CheckEqual(RawUcs4ToUtf8(up4), U);
      Up2 := UpperCaseReference(U);
      up4 := UpperCaseUcs4Reference(U);
      CheckEqual(RawUcs4ToUtf8(up4), Up2);
      CheckEqual(Ucs4Compare(Utf8ToRawUcs4(Up2), up4), 0);
      CheckEqual(StrPosIReference(pointer(U), up4), pointer(U));
      if U <> '' then
      begin
        Up2 := 'abcDE G' + U;
        CheckEqual(StrPosIReference(pointer(Up2), Up4) - pointer(Up2),  7);
        SetLength(Up2, length(Up2) - 1);
        Check(StrPosIReference(pointer(Up2), Up4) = nil);
        Up2 := 'abcDEF' + U + 'PZE';
        CheckEqual(StrPosIReference(pointer(Up2), Up4) - pointer(Up2),  6);
      end;
      if WA then
      begin
        CheckEqual(Utf8ICompReference(pointer(U), pointer(Up)), 0, 'Utf8ICompReference');
        CheckEqual(Utf8ILCompReference(pointer(U), pointer(Up), length(U), length(Up)),
          0, 'Utf8ILCompReference');
      end;
    except
      on E: Exception do
        CheckUtf8(false, '% for %[%]%', [E, length(U), EscapeToShort(U), length(up4)]);
    end;
    U2 := LowerCase(U);
    Check(IsLower(U2));
    CheckEqual(U2, LowerCaseAscii7(U));
    L := Length(U);
    SetString(Up, nil, L);
    SetString(Up2, PAnsiChar(pointer(U)), L);
    L := Utf8UpperCopy(pointer(Up), pointer(U), L) - pointer(Up);
    Check(L <= length(U));
    CheckEqual(ConvertCaseUtf8(pointer(Up2), pointer(Up2), NormToUpperByte), L);
    if Up <> '' then
      Check(EqualBuf(Up, Up2));
    if CurrentAnsiConvert.CodePage = CODEPAGE_US then
       // initial text above is WinAnsiString (CP 1252)
      CheckEqual(StringToUtf8(Utf8ToString(U)), U, '1252');
    Up := UpperCaseUnicode(U);
    CheckEqual(Up, UpperCaseUnicode(LowerCaseUnicode(U)), 'upper/lower');
    {$ifdef OSPOSIX}
    if not Icu.IsAvailable then
      // fallback when only a..z chars are translated
      CheckEqual(UpperCaseReference(LowerCaseUnicode(U)), UpperCaseReference(U), 'UCR')
    else
    {$endif OSPOSIX}
    begin
      U2 := UpperCaseReference(U);
      CheckEqual(length(Up), length(U2));
      CheckEqual(Up, U2, 'UpperCaseReference');
    end;
    CheckEqual(kr32(0, pointer(U), length(U)), kr32reference(pointer(U), length(U)), 'kr32');
    U2 := U + #10;
    check(PosChar(pointer(U2), #0) = nil);
    check(PosChar(pointer(U2), #1) = nil);
    check(PosChar(pointer(U2), #10) = @U2[length(U2)]);
    check(PosCharU(U2, #0) = nil);
    check(PosCharU(U2, #1) = nil);
    check(PosCharU(U2, #10) = @U2[length(U2)]);
    if U = '' then
      continue;
    U2 := QuotedStr(U, '"');
    Check(UnQuoteSqlStringVar(pointer(U2), res) <> nil);
    Check(res = U);
    Check(not IsZero(pointer(W), length(W)));
    FillCharFast(pointer(W)^, length(W), 0);
    Check(IsZero(pointer(W), length(W)));
    CheckEqual(FormatUtf8(U, []), U);
    res := FormatSql(U, [], []); // Delphi 5 bug with high([])>0 :(
    CheckEqual(length(res), Length(U));
    CheckEqual(res, U);
    CheckEqual(FormatUtf8('%', [U]), U);
    CheckEqual(FormatSql('%', [U], []), U);
    q := ':(' + QuotedStr(U) + '):';
    CheckEqual(FormatSql('?', [], [U]), q);
    res := 'ab' + U;
    q := 'ab' + q;
    CheckEqual(FormatUtf8('ab%', [U]), res);
    CheckEqual(FormatUtf8('%%', ['ab', U]), res);
    CheckEqual(FormatSql('ab%', [U], []), res);
    CheckEqual(FormatSql('%%', ['ab', U], []), res);
    CheckEqual(FormatSql('ab?', [], [U]), q);
    CheckEqual(FormatSql('%?', ['ab'], [U]), q);
    res := res + 'cd';
    q := q + 'cd';
    CheckEqual(FormatUtf8('ab%cd', [U]), res);
    CheckEqual(FormatSql('ab%cd', [U], []), res);
    CheckEqual(FormatUtf8('a%%cd', ['b', U]), res);
    CheckEqual(FormatSql('a%%cd', ['b', U], []), res);
    CheckEqual(FormatUtf8('%%%', ['ab', U, 'cd']), res);
    CheckEqual(FormatSql('ab?cd', [], [U]), q);
    CheckEqual(FormatSql('%?cd', ['ab'], [U]), q);
    CheckEqual(FormatSql('%?%', ['ab', 'cd'], [U]), q);
    CheckEqual(FormatSql('%?c%', ['ab', 'd'], [U]), q);
    CheckEqual(FormatSql('a%?%d', ['b', 'c'], [U]), q);
  end;
  SetLength(U, 4);
  U[1] := #$F0;
  U[2] := #$A8;
  U[3] := #$B3;
  U[4] := #$92;
  Utf8ToSynUnicode(U, SU);
  if not CheckFailed(length(SU) = 2) then
    Check(PCardinal(SU)^ = $DCD2D863);
  Check(Utf8ToUnicodeLength(Pointer(U)) = 2);
  Check(Utf8FirstLineToUtf16Length(Pointer(U)) = 2);
  PCardinal(@WU)^ := 0;
  if CheckEqual(Utf8ToWideChar(WU, pointer(U), SizeOf(WU), length(U), false), 4) then
    Check(PCardinal(@WU)^ = $DCD2D863);
  U := SynUnicodeToUtf8(SU);
  if not CheckFailed(length(U) = 4) then
    Check(PCardinal(U)^ = $92b3a8f0);
  TSynAnsiConvert.Engine(CP_UTF8).UnicodeBufferToAnsiVar(
    pointer(SU), length(SU), RawByteString(U));
  Check(length(U) = 4);
  if not CheckFailed(length(U) = 4) then
    Check(PCardinal(U)^ = $92b3a8f0);
  SetLength(res, 10);
  PB := pointer(res);
  PB := ToVarString(U, PB);
  check(PAnsiChar(PB) - pointer(res) = length(U) + 1);
  PB := pointer(res);
  U2 := FromVarString(PB);
  check(U2 = U);
  PB := pointer(res);
  FromVarString(PB, U2);
  check(U2 = U);
  for i := 0 to high(UTF8_UCS4) do
  begin
    RawUcs4ToUtf8(@UTF8_UCS4[i], 1, U);
    Check(U <> '');
    Ucs4 := Utf8ToRawUcs4(U);
    CheckEqual(length(Ucs4), 1);
    CheckEqual(Ucs4[0], UTF8_UCS4[i]);
  end;
  FastSetString(U, @CHINESE_TEXT, 9);
  CheckEqual(StrLen(pointer(U)), 9);
  SU := Utf8ToSynUnicode(U);
  eng := TSynAnsiConvert.Engine(936);
  Check(eng <> nil, 'Engine(936)');
  rb1 := eng.UnicodeStringToAnsi(SU); // GB2312
  CheckEqual(length(rb1), 7, 'cp936a');
  SU2 := eng.AnsiToUnicodeString(rb1);
  Check(SU = SU2);
  rb1 := '';
  rb1 := eng.Utf8ToAnsi(U);
  CheckEqual(length(rb1), 7);
  U2 := eng.AnsiToUtf8(rb1);
  CheckEqual(U, U2);
  eng := TSynAnsiConvert.Engine(54936);
  Check(eng <> nil, 'Engine(54936)');
  rb1 := eng.UnicodeStringToAnsi(SU); // GB18030
  if rb1 <> '' then // some Windows versions won't support this code page
  begin
    CheckEqual(length(rb1), 7, 'cp54936a');
    SU2 := eng.AnsiToUnicodeString(rb1);
    Check(SU = SU2, 'cp54936b');
    rb1 := '';
    rb1 := eng.Utf8ToAnsi(U);
    CheckEqual(length(rb1), 7, 'cp54936c');
    U2 := eng.AnsiToUtf8(rb1);
    CheckEqual(U, U2, 'cp54936d');
    {$ifdef HASCODEPAGE}
    rb2 := U;
    CheckEqual(length(rb2), 9);
    SetCodePage(rb2, 54936, {convert=}true);
    CheckEqual(length(u), 9);
    CheckEqual(length(rb1), 7);
    CheckEqual(length(rb2), 7);
    Check(rb1 = rb2, 'setcodepage');
    Check(SortDynArrayRawByteString(rb1, rb2) = 0);
    {$endif HASCODEPAGE}
    SetLength(U, 4);
    PCardinal(U)^ := $A59AAAF0; // valid in GB18030 only
    SU := Utf8ToSynUnicode(U);  // 69 D8 A5 DE , UTF16, Code Point: \uD869\uDEA5
    CheckEqual(PCardinal(SU)^, $DEA5D869);
    RB1 := eng.Utf8ToAnsi(U);
    Check((RB1 <> '') and (PCardinal(RB1)^ = $37EE3598), 'Utf8ToAnsi');
    RB2 := eng.UnicodeStringToAnsi(SU);
    Check(SortDynArrayRawByteString(rb1, rb2) = 0, 'UnicodeStringToAnsi');
    U2 := eng.AnsiToUtf8(RB1);
    CheckEqual(U2, U, 'AnsiToUtf8');
  end;
  CheckEqual(CodePageToText(CP_UTF8), 'utf8');
  CheckEqual(CodePageToText(CP_UTF16), 'utf16le');
  CheckEqual(CodePageToText(CP_WINANSI), 'ms1252');
  CheckEqual(CodePageToText(54936), 'gb18030');
  Check(LcidToLanguage(0) = lngUndefined);
  CheckEqual(LANG_LCID[lngUndefined], LANG_ENGLISH_US);
  CheckEqual(LANG_ISO[lngUndefined], '');
  CheckEqual(LANG_TXT[lngUndefined], 'Undefined');
  Check(LcidToLanguage(LANG_ENGLISH_US) = lngEnglish);
  CheckEqual(LANG_LCID[lngEnglish], LANG_ENGLISH_US);
  CheckEqual(LANG_TXT[lngEnglish], 'English');
  Check(LcidToLanguage(LANG_CHINESE_SIMPLIFIED) = lngChinese);
  CheckEqual(LANG_LCID[lngChinese], LANG_CHINESE_SIMPLIFIED);
  CheckEqual(LANG_TXT[lngChinese], 'Chinese');
  Check(LcidToLanguage(LANG_BOSNIAN_CYRILLIC) = lngBosnian);
  CheckEqual(LANG_LCID[lngBosnian], LANG_BOSNIAN_CYRILLIC);
  Check(LcidToLanguage(LANG_SERBIAN_NEUTRAL) = lngSerbian);
  CheckEqual(LANG_LCID[lngSerbian], LANG_SERBIAN_NEUTRAL);
  Check(LcidToLanguage(LANG_CROATIAN_NEUTRAL) = lngCroatian);
  CheckEqual(LANG_LCID[lngCroatian], LANG_CROATIAN_NEUTRAL);
  Check(IsoTextToLanguage('') = lngUndefined);
  Check(IsoTextToLanguage('f') = lngUndefined);
  Check(IsoTextToLanguage('fr') = lngFrench);
  Check(IsoTextToLanguage('fre') = lngUndefined);
  Check(IsoTextToLanguage(' fr') = lngUndefined);
  for lng := succ(low(lng)) to high(lng) do
  begin
    lcid := LANG_LCID[lng];
    Check(lcid <> 0);
    CheckEqual(ord(LcidToLanguage(lcid)), ord(lng), 'lcid');
    CheckEqual(ord(LcidToLanguage(lcid)), ord(lng), 'lcidcache');
    if not (lng in lngBCS) then
      CheckEqual(ord(LcidToLanguage(LANG_PRI[lng])), ord(lng), 'LANG_PRI');
    U := LcidToText(lcid);
    CheckUtf8(length(U) >= 3, U);
    CheckEqual(U, LANG_TXT[lng]);
    if lng <> high(lng) then
      CheckUtf8(SortDynArrayAnsiString(U, LANG_TXT[succ(lng)]) < 0, U);
    U := LANG_ISO[lng];
    Check(U <> '');
    Check(IdemPropNameU(U, @LANG_ISO_SHORT[lng], 2));
    CheckEqual(ord(IsoTextToLanguage(U)), ord(lng), 'iso');
    CheckEqual(ord(IsoTextToLanguage(U)), ord(lng), 'isocache');
    UpperCaseSelf(U);
    Check(IdemPropNameU(U, @LANG_ISO_SHORT[lng], 2));
    CheckEqual(ord(IsoTextToLanguage(U)), ord(lng), 'isocache2');
  end;
  Check(UnQuoteSqlStringVar('"one two"', U) <> nil);
  Check(U = 'one two');
  Check(UnQuoteSqlStringVar('one two', U) <> nil);
  Check(U = 'ne tw');
  Check(UnQuoteSqlStringVar('"one "" two"', U) <> nil);
  Check(U = 'one " two');
  Check(UnQuoteSqlStringVar('"one " two"', U) <> nil);
  Check(U = 'one ');
  Check(UnQuoteSqlStringVar('"one two', U) = nil);
  Check(UnQuoteSqlStringVar('"one "" two', U) = nil);
  Check(not IsValidEmail(''));
  Check(IsValidEmail('test@synopse.info'));
  Check(not IsValidEmail('test @synopse.info'));
  Check(not IsValidEmail('test@ synopse.info'));
  Check(not IsValidEmail('test@synopse'));
  Check(IsValidEmail('test_two@blog.synopse.info'));
  Check(IsValidEmail('test++two@blog.synopse.info'));
  Check(IsValidEmail('"test"@synopse.info'));
  Check(not IsValidEmail('"test"ed@synopse.info'));
  Check(not IsValidEmail('"test@synopse.info'));
  Check(IsValidEmail('John.Doe@synopse.info'));
  Check(not IsValidEmail('John..Doe@synopse.info'));
  Check(IsValidEmail('"John Doe"@synopse.info'));
  Check(IsValidEmail('"John  Doe"@synopse.info'));
  Check(IsValidEmail('"John.Doe"@synopse.info'));
  Check(IsValidEmail('"John..Doe"@synopse.info'));
  Check(IsValidIP4Address('192.168.1.1'));
  Check(IsValidIP4Address('192.168.001.001'));
  Check(not IsValidIP4Address('192.158.1. 1'));
  Check(not IsValidIP4Address('192.158.1.301'));
  Check(not IsValidIP4Address(' 12.158.1.01'));
  Check(not IsValidIP4Address('12.158.1.'));
  Check(not IsValidIP4Address('12.158.1'));
  {$ifdef OSWINDOWS}
  Check(FindUnicode('  ABCD DEFG', 'ABCD', 4));
  Check(FindUnicode('  ABCD DEFG', 'DEFG', 4));
  Check(FindUnicode('ABCD DEFG ', 'DEFG', 4));
  Check(FindUnicode('ABCD DEFG ', 'ABCD', 4));
  Check(FindUnicode('  abcd defg', 'ABCD', 4));
  Check(FindUnicode('  abcd defg', 'DEFG', 4));
  Check(FindUnicode('abcd defg ', 'DEFG', 4));
  Check(FindUnicode('abcd defg ', 'ABCD', 4));
  Check(FindUnicode('ABCD DEFG ', 'ABCD', 4));
  Check(FindUnicode('  abcde defg', 'ABCD', 4));
  Check(FindUnicode('  abcdf defg', 'DEFG', 4));
  Check(FindUnicode('abcdg defg ', 'DEFG', 4));
  Check(FindUnicode('abcdh defg ', 'ABCD', 4));
  Check(FindUnicode('  abcd defg', 'ABC', 3));
  Check(FindUnicode('  abcd defg', 'DEF', 3));
  Check(FindUnicode('abcd defg ', 'DEF', 3));
  Check(FindUnicode('abcd defg ', 'ABC', 3));
  Check(not FindUnicode('  abcd defg', 'ABC2', 4));
  Check(not FindUnicode('  abcd defg', 'DEF2', 4));
  Check(not FindUnicode('abcd defg ', 'DEF1', 4));
  Check(not FindUnicode('abcd defg ', 'ABC1', 4));
  Check(UpperCaseUnicode('abcdefABCD') = 'ABCDEFABCD');
  Check(LowerCaseUnicode('abcdefABCD') = 'abcdefabcd');
  {$endif OSWINDOWS}
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'toto'), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'titi'), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'ab', 'AB'), 'ABcABcABc');
  CheckEqual(StringReplaceAll('abcabcabc', 'AB', 'toto'), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'Bc', 'titi'), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', ''), 'aaa');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'B'), 'aBaBaB');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'bcd'), 'abcdabcdabcd');
  CheckEqual(StringReplaceAll('abcabcabc', 'c', 'C'), 'abCabCabC');
  CheckEqual(StringReplaceAll('abcabcabc', []), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', ['c']), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', ['c', 'C']), 'abCabCabC');
  CheckEqual(StringReplaceAll('abcabcabc', ['c', 'C', 'a']), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc',
    ['c', 'C', 'toto', 'titi', 'ab', 'AB']), 'ABCABCABC');
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'toto', false), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'titi', false), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'ab', 'AB', false), 'ABcABcABc');
  CheckEqual(StringReplaceAll('abcabcabc', 'AB', 'toto', false), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'Bc', 'titi', false), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', '', false), 'aaa');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'B', false), 'aBaBaB');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'bcd', false), 'abcdabcdabcd');
  CheckEqual(StringReplaceAll('abcabcabc', 'c', 'C', false), 'abCabCabC');
  CheckEqual(StringReplaceAll('abcabcabc', 'c', '', false), 'ababab');
  CheckEqual(StringReplaceAll('abcabcabc', 'C', '', false), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'toto', true), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'toto', 'titi', true), 'abcabcabc');
  CheckEqual(StringReplaceAll('abcabcabc', 'ab', 'AB', true), 'ABcABcABc');
  CheckEqual(StringReplaceAll('abcabcabc', 'AB', 'toto', true), 'totoctotoctotoc');
  CheckEqual(StringReplaceAll('abcabcabc', 'Bc', 't', true), 'atatat');
  CheckEqual(StringReplaceAll('abcabcabc', 'bC', '', true), 'aaa');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'B', true), 'aBaBaB');
  CheckEqual(StringReplaceAll('abcabcabc', 'bc', 'bcd', true), 'abcdabcdabcd');
  CheckEqual(StringReplaceAll('abcabcabc', 'c', 'C', true), 'abCabCabC');
  CheckEqual(StringReplaceAll('abcabcabc', 'c', '', true), 'ababab');
  CheckEqual(StringReplaceAll('abcabcabc', 'C', '', true), 'ababab');
  CheckEqual(LogEscapeFull(''), '');
  CheckEqual(LogEscapeFull(' abc'), ' abc');
  CheckEqual(LogEscapeFull('abc'), 'abc');
  u := 'abc'#10;
  Check(ContentToShort(u) = 'abc'#10);
  CheckEqual(LogEscapeFull(u), 'abc$0a');
  u2 := RawUtf8OfChar('-', 10);
  CheckEqual(u2, '----------');
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 15)^ = #0);
  CheckEqual(u2, 'abc$0a'#0'---');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 12)^ = #0);
  CheckEqual(u2, 'abc$0a'#0'---');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 7)^ = #0);
  CheckEqual(u2, 'abc$0a'#0'---');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 6)^ = #0);
  CheckEqual(u2, 'abc..'#0'----');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 5)^ = #0);
  CheckEqual(u2, 'ab..'#0'-----');
  u := 'abcd'#10;
  u[4] := #129; // not valid UTF-8
  Check(ContentToShort(u) = 'abc$81$0a');
  u := '012345678';
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 10)^ = #0);
  CheckEqual(u2, '012345678'#$00);
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 9)^ = #0);
  CheckEqual(u2, '012345..'#0'-');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 5)^ = #0);
  CheckEqual(u2, '01..'#0'-----');
  u2 := RawUtf8OfChar('-', 10);
  Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 4)^ = #0);
  CheckEqual(u2, '0..'#0'------');
  for i := 3 downto 0 do
  begin
    u2 := RawUtf8OfChar('-', 10);
    Check(EscapeBuffer(pointer(u), length(u), pointer(u2), 3)^ = #0);
    CheckEqual(u2, #0'---------');
  end;
  for i := -10 to 50 do
    for j := -10 to 50 do
    begin
      CheckTrimCopy('', i, j);
      CheckTrimCopy('1', i, j);
      CheckTrimCopy('1 ', i, j);
      CheckTrimCopy(' 1', i, j);
      CheckTrimCopy('   1', i, j);
      CheckTrimCopy('1   ', i, j);
      CheckTrimCopy('1', i, j);
      CheckTrimCopy('12', i, j);
      CheckTrimCopy('123', i, j);
      CheckTrimCopy(' 234', i, j);
      CheckTrimCopy(' 234 ', i, j);
      CheckTrimCopy(' 2 4', i, j);
      CheckTrimCopy(' 2 4 ', i, j);
      CheckTrimCopy('  3    ', i, j);
      CheckTrimCopy('  3   7  ', i, j);
      CheckTrimCopy(' 234 6', i, j);
      CheckTrimCopy('234 67 ', i, j);
      CheckTrimCopy(' 234 67 ', i, j);
      CheckTrimCopy(' 234 67 ', i, maxInt);
    end;
  u := RawUtf8OfChar('-', 300);
  for i := 250 to 260 do
  begin
    ss[0] := #0;
    ContentToShortAppend(pointer(u), i, ss);
    if i < 255 then
    begin
      CheckEqual(ord(ss[0]), i);
      for j := 1 to i do
        Check(ss[j] = '-')
    end
    else if i = 255 then
    begin
      CheckEqual(ord(ss[0]), 255);
      for j := 1 to 254 do
        Check(ss[j] = '-');
      Check(ss[255] = #0);
    end
    else
    begin
      CheckEqual(ord(ss[0]), 255);
      for j := 1 to 252 do
        Check(ss[j] = '-');
      Check(ss[253] = '.');
      Check(ss[254] = '.');
      Check(ss[255] = #0);
    end;
  end;
  P := 'toto';
  Check(GotoNextLine(P) = nil);
  P := 'to'#10'po';
  Check(GotoNextLine(P)^ = 'p');
  P := 'to'#13#10'po';
  Check(GotoNextLine(P)^ = 'p');
  P := 'to'#3#13#10'po';
  Check(GotoNextLine(P)^ = 'p');
  P := 'to'#10#10'po';
  Check(GotoNextLine(P)^ = #10);
  P := 'to'#13#10#13#10'po';
  Check(GotoNextLine(P)^ = #13);
  P := 'to'#3#1'po';
  Check(GotoNextLine(P) = nil);
  P := 'to'#3#0'po';
  Check(GotoNextLine(P) = nil);
end;

procedure TTestCoreBase.Charsets;

  procedure CheckCodePage(const name, utf, ref: RawUtf8; cp: cardinal);
  // see https://github.com/python/cpython/tree/main/Lib/test/cjkencodings
  var
    msg: string;
    su, su2: SynUnicode;
    ra, a, a2: RawByteString;
    ru, u, u2: RawUtf8;
    eng: TSynAnsiConvert;
    {$ifdef HASCODEPAGE}
    w: WideString;
    {$endif HASCODEPAGE}
  begin
    FormatString('% (cp=%)', [name, cp], msg);
    ru := Base64ToBin(utf);
    ra := Base64ToBin(ref);
    {
    u := StringFromFile('/home/ab/dev/lib2/backup/cjkencodings/' + name + '-utf8.txt');
    CheckEqual(u, ru, name);
    a := StringFromFile('/home/ab/dev/lib2/backup/cjkencodings/' + name + '.txt');
    CheckEqual(a, ra, name);
    }
    FakeCodePage(RawByteString(ru), CP_UTF8);
    FakeCodePage(ra, cp);
    if CheckFailed(ru <> '', msg) or
       CheckFailed(ra <> '', msg) then
      exit;
    Check(length(ru) > length(ra), 'utf8 oversize');
    // validate UTF-8 to/from UTF-16 conversion
    su := Utf8ToSynUnicode(ru);
    Check(su <> '', msg);
    CheckEqual(SynUnicodeToUtf8(su), ru, 'utf8');
    {$ifdef HASCODEPAGE} // old Delphi RTL does not decode UTF-16 surrogates
    w := UTF8Decode(ru);
    CheckEqual(length(w), length(su), 'rtl1');
    Check(CompareMem(pointer(w), pointer(su), length(w)), 'rtl2');
    {$endif HASCODEPAGE}
    {$ifdef OSWINDOWS}
    // skip old Windows (XP/Vista/Seven) which may miss some/most encodings
    if OSVersion < wTen then
      exit; // seems not available without a specific language pack
    {$endif OSWINDOWS}
    // validate mORMot conversion
    eng := TSynAnsiConvert.Engine(cp);
    Check(eng <> nil, 'eng1');
    CheckEqual(eng.CodePage, cp, 'eng2');
    // with ASCII-7 chars
    su2 := eng.AnsiToUnicodeString('abcd efgh');
    Check(su2 = 'abcd efgh', msg);
    a := eng.UnicodeStringToAnsi(su2);
    {$ifdef OSPOSIX}
    if cp = 50225 then // iso2022_kr
      {$ifdef OSDARWIN}
      exit;  // MacOS ICU seems to be not as expected with escape chars
      {$else}
      if not CheckFailed(a <> '', 'kr1') then
        if not CheckFailed(PCardinal(a)^ = 1126769691, 'kr2') then
          delete(a, 1, 4); // delete IEC 2022 escape char
      {$endif OSDARWIN}
    {$endif OSPOSIX}
    CheckEqual(a, 'abcd efgh');
    // don't even try on unsupported charsets
    case cp of
      951, // big5hkscs seems unstandardized on Windows: no matching code page
      50220, 50222, 51949:
        // those codepages fail on both Windows and Debian ICU
        // -> some inacurracy in Unicode_CodePageName() ?
        exit;
      // mORMot is therefore currently validated against:
      // hz (cp=52936) gb18030 (cp=54936) big5 (cp=950) cp949 (cp=949)
      // euc_jp (cp=20932) gb2312 (cp=936) gbk (cp=936) iso2022_kr (cp=50225)
      // johab (cp=1361) shift_jis (cp=932)
      // -> we would need some input from native speakers of missing charsets
    end;
    {$ifdef OSPOSIX}
    if (name = 'hz') and
       not icu.IsAvailable then // FPC RTL iconv is not enough about HZ-GB2312
      exit;
    {$endif OSPOSIX}
    // validate Unicode RTL conversion
    {$ifdef HASCODEPAGE}
    {$ifdef OSPOSIX}
    if name <> 'hz' then // HZ-GB2312 requires ICU - skip FPC RTL iconv
    {$endif OSPOSIX}
    begin
      CheckEqual(GetCodePage(ru), CP_UTF8);
      a := ru;
      SetCodePage(a, cp, {convert=}true);
      CheckEqual(a, ra, name);
    end;
    {$endif HASCODEPAGE}
    // with variable-length encoding
    a := eng.Utf8ToAnsi(ru);
    CheckEqual(a, ra, name);
    {$ifdef OSWINDOWS}
    if cp = 1361 then
      exit; // some casing issue to investigate on Windows (not with ICU)
    {$endif OSWINDOWS}
    su2 := eng.AnsiToUnicodeString(ra);
    Check(su = su2, msg);
    eng := TSynAnsiConvert.Engine(cp); // validate "last" cache
    Check(eng <> nil, 'eng3');
    CheckEqual(eng.CodePage, cp, 'eng4');
    u2 := eng.AnsiToUtf8(a);
    Check(u2 = ru, msg);
    a2 := eng.UnicodeStringToAnsi(su);
    CheckEqual(a2, a, name);
    u := eng.AnsiToUtf8(ra);
    CheckEqual(u, ru, name);
  end;

begin
  // from https://github.com/python/cpython/tree/main/Lib/test/cjkencodings
  // - base-64 encoded reference as 7-bit text to please all Delphi/FPC versions
  // - note that python code page naming may be inconsistent with ICU
  CheckCodePage('hz',
    'VGhpcyBzZW50ZW5jZSBpcyBpbiBBU0NJSS4KVGhlIG5leHQgc2VudGVuY2UgaXMg' +
    'aW4gR0Iu5bex5omA5LiN5qyy77yM5Yu/5pa95pa85Lq644CCQnllLgo=',
    'VGhpcyBzZW50ZW5jZSBpcyBpbiBBU0NJSS4KVGhlIG5leHQgc2VudGVuY2UgaXMg' +
    'aW4gR0Iufns8Okt5MjtTeyMsTnBKKWw2SEshI359QnllLgo=', 52936);
  CheckCodePage('gb18030',
    'UHl0aG9u77yI5rS+5qOu77yJ6K+t6KiA5piv5LiA56eN5Yqf6IO95by65aSn6ICM' +
    '5a6M5ZaE55qE6YCa55So5Z6L6K6h566X5py656iL5bqP6K6+6K6h6K+t6KiA77yM' +
    'CuW3sue7j+WFt+acieWNgeWkmuW5tOeahOWPkeWxleWOhuWPsu+8jOaIkOeGn+S4' +
    'lOeos+WumuOAgui/meenjeivreiogOWFt+aciemdnuW4uOeugOaNt+iAjOa4heaZ' +
    'sArnmoTor63ms5XnibnngrnvvIzpgILlkIjlrozmiJDlkITnp43pq5jlsYLku7vl' +
    'iqHvvIzlh6DkuY7lj6/ku6XlnKjmiYDmnInnmoTmk43kvZzns7vnu5/kuK0K6L+Q' +
    '6KGM44CC6L+Z56eN6K+t6KiA566A5Y2V6ICM5by65aSn77yM6YCC5ZCI5ZCE56eN' +
    '5Lq65aOr5a2m5Lmg5L2/55So44CC55uu5YmN77yM5Z+65LqO6L+ZCuenjeivreio' +
    'gOeahOebuOWFs+aKgOacr+ato+WcqOmjnumAn+eahOWPkeWxle+8jOeUqOaIt+aV' +
    'sOmHj+aApeWJp+aJqeWkp++8jOebuOWFs+eahOi1hOa6kOmdnuW4uOWkmuOAggrl' +
    'poLkvZXlnKggUHl0aG9uIOS4reS9v+eUqOaXouacieeahCBDIGxpYnJhcnk/CuOA' +
    'gOWcqOizh+ioiuenkeaKgOW/q+mAn+eZvOWxleeahOS7iuWkqSwg6ZaL55m85Y+K' +
    '5ris6Kmm6Luf6auU55qE6YCf5bqm5piv5LiN5a655b+96KaW55qECuiqsumhjC4g' +
    '54K65Yqg5b+r6ZaL55m85Y+K5ris6Kmm55qE6YCf5bqmLCDmiJHlgJHkvr/luLjl' +
    'uIzmnJvog73liKnnlKjkuIDkupvlt7Lplovnmbzlpb3nmoQKbGlicmFyeSwg5Lim' +
    '5pyJ5LiA5YCLIGZhc3QgcHJvdG90eXBpbmcg55qEIHByb2dyYW1taW5nIGxhbmd1' +
    'YWdlIOWPrwrkvpvkvb/nlKguIOebruWJjeacieioseioseWkmuWkmueahCBsaWJy' +
    'YXJ5IOaYr+S7pSBDIOWvq+aIkCwg6ICMIFB5dGhvbiDmmK/kuIDlgIsKZmFzdCBw' +
    'cm90b3R5cGluZyDnmoQgcHJvZ3JhbW1pbmcgbGFuZ3VhZ2UuIOaVheaIkeWAkeW4' +
    'jOacm+iDveWwh+aXouacieeahApDIGxpYnJhcnkg5ou/5YiwIFB5dGhvbiDnmoTn' +
    'krDlooPkuK3muKzoqablj4rmlbTlkIguIOWFtuS4reacgOS4u+imgeS5n+aYr+aI' +
    'keWAkeaJgAropoHoqI7oq5bnmoTllY/poYzlsLHmmK86Cu2MjOydtOyNrOydgCDq' +
    'sJXroKXtlZwg6riw64ql7J2EIOyngOuLjCDrspTsmqkg7Lu07ZOo7YSwIO2UhOuh' +
    'nOq3uOuemOuwjSDslrjslrTri6QuCgo=',
    'UHl0aG9uo6jFycmto6nT79HUysfSu9bWuabE3Me/tPO2+M3qyca1xM2o08PQzbzG' +
    'y+O7+rPM0PLJ6LzG0+/R1KOsCtLRvq2+39PQyq624MTqtcS3otW5wPrKt6Oss8nK' +
    '7MfSzsi2qKGj1eLW1tPv0dS+39PQt8ezo7zyvd22+MflzvoKtcTT77eozNi146Os' +
    'ysq6z83qs8m499bWuN+y48jOzvGjrLy4uvW/ydLU1NrL+dPQtcSy2df3z7XNs9bQ' +
    'CtTL0NCho9Xi1tbT79HUvPK1pbb4x7+086Osysq6z7j31tbIy8q/0afPsMq508Oh' +
    'o8S/x7CjrLv509rV4grW1tPv0dS1xM/gudi8vMr11f3U2rfJy9m1xLei1bmjrNPD' +
    'u6fK/cG/vLG+58CptPOjrM/gudi1xNfK1LS3x7OjtuChowrI57rO1NogUHl0aG9u' +
    'INbQyrnTw7zI09C1xCBDIGxpYnJhcnk/CqGh1NrZWdONv8a8vL/sy9mwbNW5tcS9' +
    '8czsLCDpX7BsvLCcedSH3Jvzd7XEy9m2yMrHsrvI3br20pW1xArVbu59LiCe6bzT' +
    'v+zpX7BsvLCcedSHtcTL2bbILCDO0oKDseOzo8+jzfvE3MD708PSu9Cp0tHpX7Bs' +
    'usO1xApsaWJyYXJ5LCCBS9PQ0ruCgCBmYXN0IHByb3RvdHlwaW5nILXEIHByb2dy' +
    'YW1taW5nIGxhbmd1YWdlIL/JCrmpyrnTwy4gxL/HsNPQ1FPUU7bgtuC1xCBsaWJy' +
    'YXJ5IMrH0tQgQyCMkbPJLCC2+CBQeXRob24gysfSu4KACmZhc3QgcHJvdG90eXBp' +
    'bmcgtcQgcHJvZ3JhbW1pbmcgbGFuZ3VhZ2UuILnKztKCg8+jzfvE3IyivMjT0LXE' +
    'CkMgbGlicmFyeSDEw7W9IFB5dGhvbiC1xK1ovrPW0Jx51Ie8sNX7us8uIMbk1tDX' +
    '7tb30qrSssrHztKCg8v5CtKq05HVk7XEhpbufb7Nysc6CoM1xzGDM5ozgzKxMYMz' +
    'lTEggjfRNoMwjDSDNoQzIII4iTWCOPs2gzOVNSCDM9UxgjmBNSCDMP05gzOGMCCD' +
    'NNwzgzX2N4M1lzUggzX5NYMwkTmCOIM5gjn8M4Mw8DQggzLrOYMy6zWCOYM5LgoK', 54936);
  CheckCodePage('big5',
    '5aaC5L2V5ZyoIFB5dGhvbiDkuK3kvb/nlKjml6LmnInnmoQgQyBsaWJyYXJ5Pwrj' +
    'gIDlnKjos4foqIrnp5HmioDlv6vpgJ/nmbzlsZXnmoTku4rlpKksIOmWi+eZvOWP' +
    'iua4rOippui7n+mrlOeahOmAn+W6puaYr+S4jeWuueW/veimlueahAroqrLpoYwu' +
    'IOeCuuWKoOW/q+mWi+eZvOWPiua4rOippueahOmAn+W6piwg5oiR5YCR5L6/5bi4' +
    '5biM5pyb6IO95Yip55So5LiA5Lqb5bey6ZaL55m85aW955qECmxpYnJhcnksIOS4' +
    'puacieS4gOWAiyBmYXN0IHByb3RvdHlwaW5nIOeahCBwcm9ncmFtbWluZyBsYW5n' +
    'dWFnZSDlj68K5L6b5L2/55SoLiDnm67liY3mnInoqLHoqLHlpJrlpJrnmoQgbGli' +
    'cmFyeSDmmK/ku6UgQyDlr6vmiJAsIOiAjCBQeXRob24g5piv5LiA5YCLCmZhc3Qg' +
    'cHJvdG90eXBpbmcg55qEIHByb2dyYW1taW5nIGxhbmd1YWdlLiDmlYXmiJHlgJHl' +
    'uIzmnJvog73lsIfml6LmnInnmoQKQyBsaWJyYXJ5IOaLv+WIsCBQeXRob24g55qE' +
    '55Kw5aKD5Lit5ris6Kmm5Y+K5pW05ZCILiDlhbbkuK3mnIDkuLvopoHkuZ/mmK/m' +
    'iJHlgJHmiYAK6KaB6KiO6KuW55qE5ZWP6aGM5bCx5pivOgoK',
    'pnCm86ZiIFB5dGhvbiCkpKjPpc6sSqazqrogQyBsaWJyYXJ5PwqhQKZiuOqwVKzs' +
    'p96n1rN0tW+uaaq6pLWk0Swgtn21b6TOtPq41bNuxemqurN0q9esT6SjrmWpv7X4' +
    'qroKvdLDRC4grLClW6fWtn21b6TOtPq41aq6s3Sr1ywgp9qtzKtLsWCnxrHmr+Cn' +
    'UaXOpECox6R3tn21b6ZuqroKbGlicmFyeSwgqMOms6RArdMgZmFzdCBwcm90b3R5' +
    'cGluZyCquiBwcm9ncmFtbWluZyBsYW5ndWFnZSClaQqo0ajPpc4uIKXYq2Wms7Nc' +
    's1ymaKZoqrogbGlicmFyeSCsT6VIIEMgvGemqCwgptMgUHl0aG9uIKxPpECt0wpm' +
    'YXN0IHByb3RvdHlwaW5nIKq6IHByb2dyYW1taW5nIGxhbmd1YWdlLiCsR6farcyn' +
    'xrHmr+CxTqxKprOqugpDIGxpYnJhcnkgrrOo7CBQeXRob24gqrrA9LnSpKS0+rjV' +
    'pM6+46ZYLiCo5KSks8ylRK1upF2sT6farcyp0gqtbrBRvdequrDdw0S0TqxPOgoK',  950);
  CheckCodePage('big5hkscs',
    '8KCEjMSa6bWu572T5rSGCsOKw4rMhMOqIMOqw6rMhAo=',
    'iEWIXIpzi9qN2AqIZohiiKcgiKeIowo=', 951);
  CheckCodePage('cp949', // with Python, this means
    '65ig67Cp6rCB7ZWYIO2OsuyLnOy9nOudvAoK44mv44mv64KpISEg5Zug5Lmd5pyI' +
    '7Yyo66+k66aU6raIIOKToeKTlu2bgMK/wr/CvyDquI3rkpkg4pOU646oIOOJry4g' +
    'Lgrkup7smIHik5TriqXtmrkgLiAuIC4gLiDshJzsmrjrpIQg646Q7ZWZ5LmZIOWu' +
    'tu2bgCAhICEgIeOFoC7jhaAK7Z2Q7Z2Q7Z2QIOOEseOEseOEseKYhuOFoF/jhaAg' +
    '7Ja066aoIO2DuOy9sOq4kCDrjozsnZEg7LmR5Lmd65Ok5LmZIOOJr+uTnOq4kArs' +
    'hKTrpowg5a627ZuAIC4gLiAuIC4g6rW07JWg7ImMIOKTlOq2iCDik6HrppjjibHq' +
    'uJAg5Zug5LuB5bed76aB5Lit6rmM7Ka8CuyZgOySgO2bgCAhICEg5Lqe7JiB4pOU' +
    'IOWutuuKpeq2iCDimIbkuIrqtIAg7JeG64ql6raI64qlIOS6nuuKpeuSiO2bgCDq' +
    'uIDslaDrk7QK4pOh66Ck65OA5LmdIOyLgO2SlOyItO2bgCDslrTrpqgg5Zug5LuB' +
    '5bed76aB5Lit7IuB4pGo65Ok7JWcISEg44mv44mv64Kp4pmhIOKMkuKMkioKCg==',
    'jGO55rCix88gvIS9w8TdtvMKCqjAqMCzsyEhIOzXzvrqxcbQkuaQcLHFIKjeqNPE' +
    'UqKvoq+iryCx4IqWIKjRtbMgqMAuIC4K5Ky/tajRtMnIwiAuIC4gLiAuILytv++3' +
    '7yC1r8fQ6+AgyqvEUiAhICEgIaTQLqTQCsjlyOXI5SCkoaShpKGh2aTQX6TQIL7u' +
    'kIogxcvE4oNPILWuwMAgr2jO+rXp6+AgqMC15YNPCryzkGogyqvEUiAuIC4gLiAu' +
    'ILG8vtaaZiCo0bHFIKjekHSowoNPIOzX7NL0ueX88emx7qOOCr/NvqzEUiAhICEg' +
    '5Ky/tajRIMqrtMmxxSCh2d++sPwgvvi0ybHFtMkg5Ky0ybXYxFIgsdu+1orbCqje' +
    't8G14M76IJrDx7S9pMRSIL7ukIog7Nfs0vS55fzx6ZrEqO+16Z3aISEgqMCowLOz' +
    'or0godKh0ioKCg==', 949);
  CheckCodePage('euc_jp',
    'UHl0aG9uIOOBrumWi+eZuuOBr+OAgTE5OTAg5bm044GU44KN44GL44KJ6ZaL5aeL' +
    '44GV44KM44Gm44GE44G+44GZ44CCCumWi+eZuuiAheOBriBHdWlkbyB2YW4gUm9z' +
    'c3VtIOOBr+aVmeiCsueUqOOBruODl+ODreOCsOODqeODn+ODs+OCsOiogOiqnuOA' +
    'jEFCQ+OAjeOBrumWi+eZuuOBq+WPguWKoOOBl+OBpuOBhOOBvuOBl+OBn+OBjOOA' +
    'gUFCQyDjga/lrp/nlKjkuIrjga7nm67nmoTjgavjga/jgYLjgb7jgorpganjgZfj' +
    'gabjgYTjgb7jgZvjgpPjgafjgZfjgZ/jgIIK44GT44Gu44Gf44KB44CBR3VpZG8g' +
    '44Gv44KI44KK5a6f55So55qE44Gq44OX44Ot44Kw44Op44Of44Oz44Kw6KiA6Kqe' +
    '44Gu6ZaL55m644KS6ZaL5aeL44GX44CB6Iux5Zu9IEJCUyDmlL7pgIHjga7jgrPj' +
    'g6Hjg4fjgqPnlarntYTjgIzjg6Ljg7Pjg4bjgqMg44OR44Kk44K944Oz44CN44Gu' +
    '44OV44Kh44Oz44Gn44GC44KLIEd1aWRvIOOBr+OBk+OBruiogOiqnuOCkuOAjFB5' +
    'dGhvbuOAjeOBqOWQjeOBpeOBkeOBvuOBl+OBn+OAggrjgZPjga7jgojjgYbjgaro' +
    'g4zmma/jgYvjgonnlJ/jgb7jgozjgZ8gUHl0aG9uIOOBruiogOiqnuioreioiOOB' +
    'r+OAgeOAjOOCt+ODs+ODl+ODq+OAjeOBp+OAjOe/kuW+l+OBjOWuueaYk+OAjeOB' +
    'qOOBhOOBhuebruaomeOBq+mHjeeCueOBjOe9ruOBi+OCjOOBpuOBhOOBvuOBmeOA' +
    'ggrlpJrjgY/jga7jgrnjgq/jg6rjg5fjg4jns7voqIDoqp7jgafjga/jg6bjg7zj' +
    'grbjga7nm67lhYjjga7liKnkvr/mgKfjgpLlhKrlhYjjgZfjgaboibLjgIXjgarm' +
    'qZ/og73jgpLoqIDoqp7opoHntKDjgajjgZfjgablj5bjgorlhaXjgozjgovloLTl' +
    'kIjjgYzlpJrjgYTjga7jgafjgZnjgYzjgIFQeXRob24g44Gn44Gv44Gd44GG44GE' +
    '44Gj44Gf5bCP57Sw5bel44GM6L+95Yqg44GV44KM44KL44GT44Go44Gv44GC44G+' +
    '44KK44GC44KK44G+44Gb44KT44CCCuiogOiqnuiHquS9k+OBruapn+iDveOBr+ac' +
    'gOWwj+mZkOOBq+aKvOOBleOBiOOAgeW/heimgeOBquapn+iDveOBr+aLoeW8teOD' +
    'ouOCuOODpeODvOODq+OBqOOBl+OBpui/veWKoOOBmeOCi+OAgeOBqOOBhOOBhuOB' +
    'ruOBjCBQeXRob24g44Gu44Od44Oq44K344O844Gn44GZ44CCCgo=',
    'UHl0aG9uIKTOs6vIr6TPoaIxOTkwIMevpLSk7aSrpOmzq7vPpLWk7KTGpKSk3qS5' +
    'oaMKs6vIr7zUpM4gR3VpZG8gdmFuIFJvc3N1bSCkz7a1sOnN0aTOpdel7aWwpeml' +
    '36XzpbC4wLjsodZBQkOh16TOs6vIr6TLu7Kyw6S3pMakpKTepLekv6SsoaJBQkMg' +
    'pM+8ws3RvuWkzszcxaqky6TPpKKk3qTqxaykt6TGpKSk3qS7pPOkx6S3pL+howqk' +
    's6TOpL+k4aGiR3VpZG8gpM+k6KTqvMLN0cWqpMql16XtpbCl6aXfpfOlsLjAuOyk' +
    'zrOryK+k8rOru8+kt6GisdG58SBCQlMgyvzB96TOpbOl4aXHpaPI1sHIodal4qXz' +
    'pcaloyCl0aWkpb2l86HXpM6l1aWhpfOkx6SipOsgR3VpZG8gpM+ks6TOuMC47KTy' +
    'odZQeXRob26h16TIzL6kxaSxpN6kt6S/oaMKpLOkzqTopKakysfYt8qkq6TpwLik' +
    '3qTspL8gUHl0aG9uIKTOuMC47MDft9ekz6Giodalt6Xzpdel66HXpMeh1r2sxsCk' +
    'rM3GsNeh16TIpKSkpszcybiky73FxcCkrMPWpKuk7KTGpKSk3qS5oaMKwr+kr6TO' +
    'pbmlr6XqpdelyLfPuMC47KTHpM+l5qG8pbakzszcwOikzs34ytjAraTyzaXA6KS3' +
    'pMa/p6G5pMq1oce9pPK4wLjszdfBx6TIpLekxrzopOrG/qTspOu+7LnnpKzCv6Sk' +
    'pM6kx6S5pKyholB5dGhvbiCkx6TPpL2kpqSkpMOkv76uutm5qaSsxMmyw6S1pOyk' +
    '66SzpMikz6SipN6k6qSipOqk3qS7pPOhowq4wLjsvKvCzqTOtaHHvaTPuse+rrjC' +
    'pMuyoaS1pKihosmszdekyrWhx72kz7PIxKWl4qW4peWhvKXrpMikt6TGxMmyw6S5' +
    'pOuhoqTIpKSkpqTOpKwgUHl0aG9uIKTOpd2l6qW3obykx6S5oaMKCg==', 20932);
  CheckCodePage('euc_jisx0213',
    'UHl0aG9uIOOBrumWi+eZuuOBr+OAgTE5OTAg5bm044GU44KN44GL44KJ6ZaL5aeL' +
    '44GV44KM44Gm44GE44G+44GZ44CCCumWi+eZuuiAheOBriBHdWlkbyB2YW4gUm9z' +
    'c3VtIOOBr+aVmeiCsueUqOOBruODl+ODreOCsOODqeODn+ODs+OCsOiogOiqnuOA' +
    'jEFCQ+OAjeOBrumWi+eZuuOBq+WPguWKoOOBl+OBpuOBhOOBvuOBl+OBn+OBjOOA' +
    'gUFCQyDjga/lrp/nlKjkuIrjga7nm67nmoTjgavjga/jgYLjgb7jgorpganjgZfj' +
    'gabjgYTjgb7jgZvjgpPjgafjgZfjgZ/jgIIK44GT44Gu44Gf44KB44CBR3VpZG8g' +
    '44Gv44KI44KK5a6f55So55qE44Gq44OX44Ot44Kw44Op44Of44Oz44Kw6KiA6Kqe' +
    '44Gu6ZaL55m644KS6ZaL5aeL44GX44CB6Iux5Zu9IEJCUyDmlL7pgIHjga7jgrPj' +
    'g6Hjg4fjgqPnlarntYTjgIzjg6Ljg7Pjg4bjgqMg44OR44Kk44K944Oz44CN44Gu' +
    '44OV44Kh44Oz44Gn44GC44KLIEd1aWRvIOOBr+OBk+OBruiogOiqnuOCkuOAjFB5' +
    'dGhvbuOAjeOBqOWQjeOBpeOBkeOBvuOBl+OBn+OAggrjgZPjga7jgojjgYbjgaro' +
    'g4zmma/jgYvjgonnlJ/jgb7jgozjgZ8gUHl0aG9uIOOBruiogOiqnuioreioiOOB' +
    'r+OAgeOAjOOCt+ODs+ODl+ODq+OAjeOBp+OAjOe/kuW+l+OBjOWuueaYk+OAjeOB' +
    'qOOBhOOBhuebruaomeOBq+mHjeeCueOBjOe9ruOBi+OCjOOBpuOBhOOBvuOBmeOA' +
    'ggrlpJrjgY/jga7jgrnjgq/jg6rjg5fjg4jns7voqIDoqp7jgafjga/jg6bjg7zj' +
    'grbjga7nm67lhYjjga7liKnkvr/mgKfjgpLlhKrlhYjjgZfjgaboibLjgIXjgarm' +
    'qZ/og73jgpLoqIDoqp7opoHntKDjgajjgZfjgablj5bjgorlhaXjgozjgovloLTl' +
    'kIjjgYzlpJrjgYTjga7jgafjgZnjgYzjgIFQeXRob24g44Gn44Gv44Gd44GG44GE' +
    '44Gj44Gf5bCP57Sw5bel44GM6L+95Yqg44GV44KM44KL44GT44Go44Gv44GC44G+' +
    '44KK44GC44KK44G+44Gb44KT44CCCuiogOiqnuiHquS9k+OBruapn+iDveOBr+ac' +
    'gOWwj+mZkOOBq+aKvOOBleOBiOOAgeW/heimgeOBquapn+iDveOBr+aLoeW8teOD' +
    'ouOCuOODpeODvOODq+OBqOOBl+OBpui/veWKoOOBmeOCi+OAgeOBqOOBhOOBhuOB' +
    'ruOBjCBQeXRob24g44Gu44Od44Oq44K344O844Gn44GZ44CCCgrjg47jgYvjgpog' +
    '44OI44KaIOODiOOCre+otu+ouSDwoZq08KqOjCDpuoDpvYHwqZuwCg==',
    'UHl0aG9uIKTOs6vIr6TPoaIxOTkwIMevpLSk7aSrpOmzq7vPpLWk7KTGpKSk3qS5' +
    'oaMKs6vIr7zUpM4gR3VpZG8gdmFuIFJvc3N1bSCkz7a1sOnN0aTOpdel7aWwpeml' +
    '36XzpbC4wLjsodZBQkOh16TOs6vIr6TLu7Kyw6S3pMakpKTepLekv6SsoaJBQkMg' +
    'pM+8ws3RvuWkzszcxaqky6TPpKKk3qTqxaykt6TGpKSk3qS7pPOkx6S3pL+howqk' +
    's6TOpL+k4aGiR3VpZG8gpM+k6KTqvMLN0cWqpMql16XtpbCl6aXfpfOlsLjAuOyk' +
    'zrOryK+k8rOru8+kt6GisdG58SBCQlMgyvzB96TOpbOl4aXHpaPI1sHIodal4qXz' +
    'pcaloyCl0aWkpb2l86HXpM6l1aWhpfOkx6SipOsgR3VpZG8gpM+ks6TOuMC47KTy' +
    'odZQeXRob26h16TIzL6kxaSxpN6kt6S/oaMKpLOkzqTopKakysfYt8qkq6TpwLik' +
    '3qTspL8gUHl0aG9uIKTOuMC47MDft9ekz6Giodalt6Xzpdel66HXpMeh1r2sxsCk' +
    'rM3GsNeh16TIpKSkpszcybiky73FxcCkrMPWpKuk7KTGpKSk3qS5oaMKwr+kr6TO' +
    'pbmlr6XqpdelyLfPuMC47KTHpM+l5qG8pbakzszcwOikzs34ytjAraTyzaXA6KS3' +
    'pMa/p6G5pMq1oce9pPK4wLjszdfBx6TIpLekxrzopOrG/qTspOu+7LnnpKzCv6Sk' +
    'pM6kx6S5pKyholB5dGhvbiCkx6TPpL2kpqSkpMOkv76uutm5qaSsxMmyw6S1pOyk' +
    '66SzpMikz6SipN6k6qSipOqk3qS7pPOhowq4wLjsvKvCzqTOtaHHvaTPuse+rrjC' +
    'pMuyoaS1pKihosmszdekyrWhx72kz7PIxKWl4qW4peWhvKXrpMikt6TGxMmyw6S5' +
    'pOuhoqTIpKSkpqTOpKwgUHl0aG9uIKTOpd2l6qW3obykx6S5oaMKCqXOpPcgpf4g' +
    'pcilra+sr9ogz+OP/tggj/7Uj/7oj/zWCg==', 50220);
  CheckCodePage('euc_kr',
    '4peOIO2MjOydtOyNrChQeXRob24p7J2AIOuwsOyasOq4sCDsib3qs6AsIOqwleug' +
    'pe2VnCDtlITroZzqt7jrnpjrsI0g7Ja47Ja07J6F64uI64ukLiDtjIzsnbTsjazs' +
    'nYAK7Zqo7Jyo7KCB7J24IOqzoOyImOykgCDrjbDsnbTthLAg6rWs7KGw7JmAIOqw' +
    'hOuLqO2VmOyngOunjCDtmqjsnKjsoIHsnbgg6rCd7LK07KeA7Zal7ZSE66Gc6re4' +
    '656Y67CN7J2ECuyngOybkO2VqeuLiOuLpC4g7YyM7J207I2s7J2YIOyasOyVhCjl' +
    'hKrpm4Up7ZWcIOusuOuyleqzvCDrj5nsoIEg7YOA7J207ZWRLCDqt7jrpqzqs6Ag' +
    '7J247YSw7ZSE66as7YyFCu2ZmOqyveydgCDtjIzsnbTsjazsnYQg7Iqk7YGs66a9' +
    '7YyF6rO8IOyXrOufrCDrtoTslbzsl5DshJzsmYAg64yA67aA67aE7J2YIO2UjOue' +
    'q+2PvOyXkOyEnOydmCDruaDrpbgK7JWg7ZSM66as7LyA7J207IWYIOqwnOuwnOyd' +
    'hCDtlaAg7IiYIOyeiOuKlCDsnbTsg4HsoIHsnbgg7Ja47Ja066GcIOunjOuTpOyW' +
    'tOykjeuLiOuLpC4KCuKYhuyyq+qwgOuBnTog64Kg7JWE6528IOyTlOyTlOyTqX4g' +
    '64uB7YG8ISDrnL3quIjsl4bsnbQg7KCE7Zml64uI64ukLiDrt4EuIOq3uOufsOqx' +
    'sCDsnY7ri6QuCg==',
    'od0gxsTAzL3jKFB5dGhvbinAuiC56L/sseIgvbGw7SwgsK23wsfRIMfBt86x17eh' +
    'udYgvvC+7sDUtM+02S4gxsTAzL3jwLoKyL/AssD7wM4gsO289sHYILWlwMzFzSCx' +
    'uMG2v80gsKO03MfPwfa4uCDIv8CywPvAziCwtMO8wfbH4sfBt86x17ehudbAuwrB' +
    '9r/4x9W0z7TZLiDGxMDMvePAxyC/7L7GKOnQ5Lopx9Egua65/bD6ILW/wPsgxbjA' +
    'zMfOLCCx17iusO0gwM7FzcfBuK7GwwrIr7DmwLogxsTAzL3jwLsgvbrFqbizxsOw' +
    '+iC/qbevILrQvt+/obytv80gtOu6zrrQwMcgx8O3p8b7v6G8rcDHILr8uKUKvtbH' +
    'w7iuxMnAzLzHILCzud/AuyDH0iC89iDA1rTCIMDMu/PA+8DOIL7wvu63ziC4uLXp' +
    'vu7B3bTPtNkuCgqh2cO5sKGzoTogs6++xrbzIKTUpLak0KTUpNSktqTQpNS+sX4g' +
    'pNSkpKTSpLfFrSEgpNSkqKTRpLex3b74wMwgwPyk1KS+pMiksrTPtNkuIKTUpLKk' +
    'zqSqLiCx17exsMUgpNSkt6TRpLS02S4K', 51949);
  CheckCodePage('gb2312',
    'UHl0aG9u77yI5rS+5qOu77yJ6K+t6KiA5piv5LiA56eN5Yqf6IO95by65aSn6ICM' +
    '5a6M5ZaE55qE6YCa55So5Z6L6K6h566X5py656iL5bqP6K6+6K6h6K+t6KiA77yM' +
    'CuW3sue7j+WFt+acieWNgeWkmuW5tOeahOWPkeWxleWOhuWPsu+8jOaIkOeGn+S4' +
    'lOeos+WumuOAgui/meenjeivreiogOWFt+aciemdnuW4uOeugOaNt+iAjOa4heaZ' +
    'sArnmoTor63ms5XnibnngrnvvIzpgILlkIjlrozmiJDlkITnp43pq5jlsYLku7vl' +
    'iqHvvIzlh6DkuY7lj6/ku6XlnKjmiYDmnInnmoTmk43kvZzns7vnu5/kuK0K6L+Q' +
    '6KGM44CC6L+Z56eN6K+t6KiA566A5Y2V6ICM5by65aSn77yM6YCC5ZCI5ZCE56eN' +
    '5Lq65aOr5a2m5Lmg5L2/55So44CC55uu5YmN77yM5Z+65LqO6L+ZCuenjeivreio' +
    'gOeahOebuOWFs+aKgOacr+ato+WcqOmjnumAn+eahOWPkeWxle+8jOeUqOaIt+aV' +
    'sOmHj+aApeWJp+aJqeWkp++8jOebuOWFs+eahOi1hOa6kOmdnuW4uOWkmuOAggoK',
    'UHl0aG9uo6jFycmto6nT79HUysfSu9bWuabE3Me/tPO2+M3qyca1xM2o08PQzbzG' +
    'y+O7+rPM0PLJ6LzG0+/R1KOsCtLRvq2+39PQyq624MTqtcS3otW5wPrKt6Oss8nK' +
    '7MfSzsi2qKGj1eLW1tPv0dS+39PQt8ezo7zyvd22+MflzvoKtcTT77eozNi146Os' +
    'ysq6z83qs8m499bWuN+y48jOzvGjrLy4uvW/ydLU1NrL+dPQtcSy2df3z7XNs9bQ' +
    'CtTL0NCho9Xi1tbT79HUvPK1pbb4x7+086Osysq6z7j31tbIy8q/0afPsMq508Oh' +
    'o8S/x7CjrLv509rV4grW1tPv0dS1xM/gudi8vMr11f3U2rfJy9m1xLei1bmjrNPD' +
    'u6fK/cG/vLG+58CptPOjrM/gudi1xNfK1LS3x7OjtuChowoK', 936);
  CheckCodePage('gbk',
    'UHl0aG9u77yI5rS+5qOu77yJ6K+t6KiA5piv5LiA56eN5Yqf6IO95by65aSn6ICM' +
    '5a6M5ZaE55qE6YCa55So5Z6L6K6h566X5py656iL5bqP6K6+6K6h6K+t6KiA77yM' +
    'CuW3sue7j+WFt+acieWNgeWkmuW5tOeahOWPkeWxleWOhuWPsu+8jOaIkOeGn+S4' +
    'lOeos+WumuOAgui/meenjeivreiogOWFt+aciemdnuW4uOeugOaNt+iAjOa4heaZ' +
    'sArnmoTor63ms5XnibnngrnvvIzpgILlkIjlrozmiJDlkITnp43pq5jlsYLku7vl' +
    'iqHvvIzlh6DkuY7lj6/ku6XlnKjmiYDmnInnmoTmk43kvZzns7vnu5/kuK0K6L+Q' +
    '6KGM44CC6L+Z56eN6K+t6KiA566A5Y2V6ICM5by65aSn77yM6YCC5ZCI5ZCE56eN' +
    '5Lq65aOr5a2m5Lmg5L2/55So44CC55uu5YmN77yM5Z+65LqO6L+ZCuenjeivreio' +
    'gOeahOebuOWFs+aKgOacr+ato+WcqOmjnumAn+eahOWPkeWxle+8jOeUqOaIt+aV' +
    'sOmHj+aApeWJp+aJqeWkp++8jOebuOWFs+eahOi1hOa6kOmdnuW4uOWkmuOAggrl' +
    'poLkvZXlnKggUHl0aG9uIOS4reS9v+eUqOaXouacieeahCBDIGxpYnJhcnk/CuOA' +
    'gOWcqOizh+ioiuenkeaKgOW/q+mAn+eZvOWxleeahOS7iuWkqSwg6ZaL55m85Y+K' +
    '5ris6Kmm6Luf6auU55qE6YCf5bqm5piv5LiN5a655b+96KaW55qECuiqsumhjC4g' +
    '54K65Yqg5b+r6ZaL55m85Y+K5ris6Kmm55qE6YCf5bqmLCDmiJHlgJHkvr/luLjl' +
    'uIzmnJvog73liKnnlKjkuIDkupvlt7Lplovnmbzlpb3nmoQKbGlicmFyeSwg5Lim' +
    '5pyJ5LiA5YCLIGZhc3QgcHJvdG90eXBpbmcg55qEIHByb2dyYW1taW5nIGxhbmd1' +
    'YWdlIOWPrwrkvpvkvb/nlKguIOebruWJjeacieioseioseWkmuWkmueahCBsaWJy' +
    'YXJ5IOaYr+S7pSBDIOWvq+aIkCwg6ICMIFB5dGhvbiDmmK/kuIDlgIsKZmFzdCBw' +
    'cm90b3R5cGluZyDnmoQgcHJvZ3JhbW1pbmcgbGFuZ3VhZ2UuIOaVheaIkeWAkeW4' +
    'jOacm+iDveWwh+aXouacieeahApDIGxpYnJhcnkg5ou/5YiwIFB5dGhvbiDnmoTn' +
    'krDlooPkuK3muKzoqablj4rmlbTlkIguIOWFtuS4reacgOS4u+imgeS5n+aYr+aI' +
    'keWAkeaJgAropoHoqI7oq5bnmoTllY/poYzlsLHmmK86Cgo=',
    'UHl0aG9uo6jFycmto6nT79HUysfSu9bWuabE3Me/tPO2+M3qyca1xM2o08PQzbzG' +
    'y+O7+rPM0PLJ6LzG0+/R1KOsCtLRvq2+39PQyq624MTqtcS3otW5wPrKt6Oss8nK' +
    '7MfSzsi2qKGj1eLW1tPv0dS+39PQt8ezo7zyvd22+MflzvoKtcTT77eozNi146Os' +
    'ysq6z83qs8m499bWuN+y48jOzvGjrLy4uvW/ydLU1NrL+dPQtcSy2df3z7XNs9bQ' +
    'CtTL0NCho9Xi1tbT79HUvPK1pbb4x7+086Osysq6z7j31tbIy8q/0afPsMq508Oh' +
    'o8S/x7CjrLv509rV4grW1tPv0dS1xM/gudi8vMr11f3U2rfJy9m1xLei1bmjrNPD' +
    'u6fK/cG/vLG+58CptPOjrM/gudi1xNfK1LS3x7OjtuChowrI57rO1NogUHl0aG9u' +
    'INbQyrnTw7zI09C1xCBDIGxpYnJhcnk/CqGh1NrZWdONv8a8vL/sy9mwbNW5tcS9' +
    '8czsLCDpX7BsvLCcedSH3Jvzd7XEy9m2yMrHsrvI3br20pW1xArVbu59LiCe6bzT' +
    'v+zpX7BsvLCcedSHtcTL2bbILCDO0oKDseOzo8+jzfvE3MD708PSu9Cp0tHpX7Bs' +
    'usO1xApsaWJyYXJ5LCCBS9PQ0ruCgCBmYXN0IHByb3RvdHlwaW5nILXEIHByb2dy' +
    'YW1taW5nIGxhbmd1YWdlIL/JCrmpyrnTwy4gxL/HsNPQ1FPUU7bgtuC1xCBsaWJy' +
    'YXJ5IMrH0tQgQyCMkbPJLCC2+CBQeXRob24gysfSu4KACmZhc3QgcHJvdG90eXBp' +
    'bmcgtcQgcHJvZ3JhbW1pbmcgbGFuZ3VhZ2UuILnKztKCg8+jzfvE3IyivMjT0LXE' +
    'CkMgbGlicmFyeSDEw7W9IFB5dGhvbiC1xK1ovrPW0Jx51Ie8sNX7us8uIMbk1tDX' +
    '7tb30qrSssrHztKCg8v5CtKq05HVk7XEhpbufb7Nysc6Cgo=', 936);
  CheckCodePage('iso2022_jp',
    'UHl0aG9uIOOBrumWi+eZuuOBr+OAgTE5OTAg5bm044GU44KN44GL44KJ6ZaL5aeL' +
    '44GV44KM44Gm44GE44G+44GZ44CCCumWi+eZuuiAheOBriBHdWlkbyB2YW4gUm9z' +
    'c3VtIOOBr+aVmeiCsueUqOOBruODl+ODreOCsOODqeODn+ODs+OCsOiogOiqnuOA' +
    'jEFCQ+OAjeOBrumWi+eZuuOBq+WPguWKoOOBl+OBpuOBhOOBvuOBl+OBn+OBjOOA' +
    'gUFCQyDjga/lrp/nlKjkuIrjga7nm67nmoTjgavjga/jgYLjgb7jgorpganjgZfj' +
    'gabjgYTjgb7jgZvjgpPjgafjgZfjgZ/jgIIK44GT44Gu44Gf44KB44CBR3VpZG8g' +
    '44Gv44KI44KK5a6f55So55qE44Gq44OX44Ot44Kw44Op44Of44Oz44Kw6KiA6Kqe' +
    '44Gu6ZaL55m644KS6ZaL5aeL44GX44CB6Iux5Zu9IEJCUyDmlL7pgIHjga7jgrPj' +
    'g6Hjg4fjgqPnlarntYTjgIzjg6Ljg7Pjg4bjgqMg44OR44Kk44K944Oz44CN44Gu' +
    '44OV44Kh44Oz44Gn44GC44KLIEd1aWRvIOOBr+OBk+OBruiogOiqnuOCkuOAjFB5' +
    'dGhvbuOAjeOBqOWQjeOBpeOBkeOBvuOBl+OBn+OAggrjgZPjga7jgojjgYbjgaro' +
    'g4zmma/jgYvjgonnlJ/jgb7jgozjgZ8gUHl0aG9uIOOBruiogOiqnuioreioiOOB' +
    'r+OAgeOAjOOCt+ODs+ODl+ODq+OAjeOBp+OAjOe/kuW+l+OBjOWuueaYk+OAjeOB' +
    'qOOBhOOBhuebruaomeOBq+mHjeeCueOBjOe9ruOBi+OCjOOBpuOBhOOBvuOBmeOA' +
    'ggrlpJrjgY/jga7jgrnjgq/jg6rjg5fjg4jns7voqIDoqp7jgafjga/jg6bjg7zj' +
    'grbjga7nm67lhYjjga7liKnkvr/mgKfjgpLlhKrlhYjjgZfjgaboibLjgIXjgarm' +
    'qZ/og73jgpLoqIDoqp7opoHntKDjgajjgZfjgablj5bjgorlhaXjgozjgovloLTl' +
    'kIjjgYzlpJrjgYTjga7jgafjgZnjgYzjgIFQeXRob24g44Gn44Gv44Gd44GG44GE' +
    '44Gj44Gf5bCP57Sw5bel44GM6L+95Yqg44GV44KM44KL44GT44Go44Gv44GC44G+' +
    '44KK44GC44KK44G+44Gb44KT44CCCuiogOiqnuiHquS9k+OBruapn+iDveOBr+ac' +
    'gOWwj+mZkOOBq+aKvOOBleOBiOOAgeW/heimgeOBquapn+iDveOBr+aLoeW8teOD' +
    'ouOCuOODpeODvOODq+OBqOOBl+OBpui/veWKoOOBmeOCi+OAgeOBqOOBhOOBhuOB' +
    'ruOBjCBQeXRob24g44Gu44Od44Oq44K344O844Gn44GZ44CCCgo=',
    'UHl0aG9uIBskQiROMytILyRPISIbKEIxOTkwIBskQkcvJDQkbSQrJGkzKztPJDUk' +
    'bCRGJCQkXiQ5ISMbKEIKGyRCMytILzxUJE4bKEIgR3VpZG8gdmFuIFJvc3N1bSAb' +
    'JEIkTzY1MGlNUSROJVclbSUwJWklXyVzJTA4QDhsIVYbKEJBQkMbJEIhVyROMytI' +
    'LyRLOzIyQyQ3JEYkJCReJDckPyQsISIbKEJBQkMgGyRCJE88Qk1RPmUkTkxcRSok' +
    'SyRPJCIkXiRqRSwkNyRGJCQkXiQ7JHMkRyQ3JD8hIxsoQgobJEIkMyROJD8kYSEi' +
    'GyhCR3VpZG8gGyRCJE8kaCRqPEJNUUUqJEolVyVtJTAlaSVfJXMlMDhAOGwkTjMr' +
    'SC8kcjMrO08kNyEiMVE5cRsoQiBCQlMgGyRCSnxBdyROJTMlYSVHJSNIVkFIIVYl' +
    'YiVzJUYlIxsoQiAbJEIlUSUkJT0lcyFXJE4lVSUhJXMkRyQiJGsbKEIgR3VpZG8g' +
    'GyRCJE8kMyROOEA4bCRyIVYbKEJQeXRob24bJEIhVyRITD4kRSQxJF4kNyQ/ISMb' +
    'KEIKGyRCJDMkTiRoJCYkSkdYN0okKyRpQDgkXiRsJD8bKEIgUHl0aG9uIBskQiRO' +
    'OEA4bEBfN1ckTyEiIVYlNyVzJVclayFXJEchVj0sRkAkLE1GMFchVyRIJCQkJkxc' +
    'STgkSz1FRUAkLENWJCskbCRGJCQkXiQ5ISMbKEIKGyRCQj8kLyROJTklLyVqJVcl' +
    'SDdPOEA4bCRHJE8lZiE8JTYkTkxcQGgkTk14SlhALSRyTSVAaCQ3JEY/JyE5JEo1' +
    'IUc9JHI4QDhsTVdBRyRIJDckRjxoJGpGfiRsJGs+bDlnJCxCPyQkJE4kRyQ5JCwh' +
    'IhsoQlB5dGhvbiAbJEIkRyRPJD0kJiQkJEMkPz4uOlk5KSQsREkyQyQ1JGwkayQz' +
    'JEgkTyQiJF4kaiQiJGokXiQ7JHMhIxsoQgobJEI4QDhsPCtCTiRONSFHPSRPOkc+' +
    'LjhCJEsyISQ1JCghIkksTVckSjUhRz0kTzNIRCUlYiU4JWUhPCVrJEgkNyRGREky' +
    'QyQ5JGshIiRIJCQkJiROJCwbKEIgUHl0aG9uIBskQiROJV0laiU3ITwkRyQ5ISMb' +
    'KEIKCg==', 50222);
  CheckCodePage('iso2022_kr',
    '4peOIO2MjOydtOyNrChQeXRob24p7J2AIOuwsOyasOq4sCDsib3qs6AsIOqwleug' +
    'pe2VnCDtlITroZzqt7jrnpjrsI0g7Ja47Ja07J6F64uI64ukLiDtjIzsnbTsjazs' +
    'nYAK7Zqo7Jyo7KCB7J24IOqzoOyImOykgCDrjbDsnbTthLAg6rWs7KGw7JmAIOqw' +
    'hOuLqO2VmOyngOunjCDtmqjsnKjsoIHsnbgg6rCd7LK07KeA7Zal7ZSE66Gc6re4' +
    '656Y67CN7J2ECuyngOybkO2VqeuLiOuLpC4g7YyM7J207I2s7J2YIOyasOyVhCjl' +
    'hKrpm4Up7ZWcIOusuOuyleqzvCDrj5nsoIEg7YOA7J207ZWRLCDqt7jrpqzqs6Ag' +
    '7J247YSw7ZSE66as7YyFCu2ZmOqyveydgCDtjIzsnbTsjazsnYQg7Iqk7YGs66a9' +
    '7YyF6rO8IOyXrOufrCDrtoTslbzsl5DshJzsmYAg64yA67aA67aE7J2YIO2UjOue' +
    'q+2PvOyXkOyEnOydmCDruaDrpbgK7JWg7ZSM66as7LyA7J207IWYIOqwnOuwnOyd' +
    'hCDtlaAg7IiYIOyeiOuKlCDsnbTsg4HsoIHsnbgg7Ja47Ja066GcIOunjOuTpOyW' +
    'tOykjeuLiOuLpC4KCuKYhuyyq+qwgOuBnTog64Kg7JWE6528IOyTqX4g7YG8ISDq' +
    'uIjsl4bsnbQg7KCE64uI64ukLiDqt7jrn7DqsbAg64ukLgo=',
    'GyQpQw4hXQ8gDkZEQEw9Yw8oUHl0aG9uKQ5AOg8gDjloP2wxYg8gDj0xMG0PLCAO' +
    'MC03QkdRDyAOR0E3TjFXNyE5Vg8gDj5wPm5AVDRPNFkPLiAORkRATD1jQDoPCg5I' +
    'P0AyQHtATg8gDjBtPHZBWA8gDjUlQExFTQ8gDjE4QTY/TQ8gDjAjNFxHT0F2ODgP' +
    'IA5IP0AyQHtATg8gDjA0QzxBdkdiR0E3TjFXNyE5VkA7DwoOQXY/eEdVNE80WQ8u' +
    'IA5GREBMPWNARw8gDj9sPkYPKA5pUGQ6DykOR1EPIA45Ljl9MHoPIA41P0B7DyAO' +
    'RThATEdODywgDjFXOC4wbQ8gDkBORU1HQTguRkMPCg5ILzBmQDoPIA5GREBMPWNA' +
    'Ow8gDj06RSk4M0ZDMHoPIA4/KTcvDyAOOlA+Xz8hPC0/TQ8gDjRrOk46UEBHDyAO' +
    'R0M3J0Z7PyE8LUBHDyAOOnw4JQ8KDj5WR0M4LkRJQEw8Rw8gDjAzOV9AOw8gDkdS' +
    'DyAOPHYPIA5AVjRCDyAOQEw7c0B7QE4PIA4+cD5uN04PIA44ODVpPm5BXTRPNFkP' +
    'LgoKDiFZQzkwITMhDzogDjMvPkY2cw8gDj4xD34gDkUtDyEgDjFdPnhATA8gDkB8' +
    'NE80WQ8uIA4xVzcxMEUPIA40WQ8uCg==', 50225);
  CheckCodePage('johab',
    '65ig67Cp6rCB7ZWYIO2OsuyLnOy9nOudvAoK44mv44mv64KpISEg5Zug5Lmd5pyI' +
    '7Yyo66+k66aU6raIIOKToeKTlu2bgMK/wr/CvyDquI3rkpkg4pOU646oIOOJry4g' +
    'Lgrkup7smIHik5TriqXtmrkgLiAuIC4gLiDshJzsmrjrpIQg646Q7ZWZ5LmZIOWu' +
    'tu2bgCAhICEgIeOFoC7jhaAK7Z2Q7Z2Q7Z2QIOOEseOEseOEseKYhuOFoF/jhaAg' +
    '7Ja066aoIO2DuOy9sOq4kCDrjozsnZEg7LmR5Lmd65Ok5LmZIOOJr+uTnOq4kArs' +
    'hKTrpowg5a627ZuAIC4gLiAuIC4g6rW07JWg7ImMIOKTlOq2iCDik6HrppjjibHq' +
    'uJAg5Zug5LuB5bed76aB5Lit6rmM7Ka8CuyZgOySgO2bgCAhICEg5Lqe7JiB4pOU' +
    'IOWutuuKpeq2iCDimIbkuIrqtIAg7JeG64ql6raI64qlIOS6nuuKpeuSiO2bgCDq' +
    'uIDslaDrk7QK4pOh66Ck65OA5LmdIOyLgO2SlOyItO2bgCDslrTrpqgg5Zug5LuB' +
    '5bed76aB5Lit7IuB4pGo65Ok7JWcISEg44mv44mv64Kp4pmhIOKMkuKMkioKCg==',
    'mbGkd4hi0GEgzVyvocWpnGEKCtzA3MCQcyEhIPFn4pzwVcyBo4mfhYqhINze3NPS' +
    'etmv2a/ZryCLd5bTINzRlYEg3MAuIC4K7Ty1d9zRk3fScyAuIC4gLiAuIKzhtome' +
    'oSCVZdBi8OAg4DvSeiAhICEgIYdBLodBCtNh02HTYSCIQYhBiEHZaYdBX4dBILTh' +
    'n5ogyKHFwYt6IJVht3cgw5finJdp8OAg3MCXYYt6Cqzpn3og4DvSeiAuIC4gLiAu' +
    'IIqJtIGuuiDc0YqhINzen4ncwot6IPFn8WL1Se388+mMYbuaCrXBsqHSeiAhICEg' +
    '7Ty1d9zRIOA7k3eKoSDZaeq+icUgtPSTd4qhk3cg7TyTd5bB0nogi2m0gZd6Ctze' +
    'nWGXQeKcIK+BzqGuodJ6ILThn5og8WfxYvVJ7fzz6a+C3O+XabR6ISEg3MDcwJBz' +
    '2b0g2WLZYioKCg==', 1361);
  CheckCodePage('shift_jisx0213',
    'UHl0aG9uIOOBrumWi+eZuuOBr+OAgTE5OTAg5bm044GU44KN44GL44KJ6ZaL5aeL' +
    '44GV44KM44Gm44GE44G+44GZ44CCCumWi+eZuuiAheOBriBHdWlkbyB2YW4gUm9z' +
    'c3VtIOOBr+aVmeiCsueUqOOBruODl+ODreOCsOODqeODn+ODs+OCsOiogOiqnuOA' +
    'jEFCQ+OAjeOBrumWi+eZuuOBq+WPguWKoOOBl+OBpuOBhOOBvuOBl+OBn+OBjOOA' +
    'gUFCQyDjga/lrp/nlKjkuIrjga7nm67nmoTjgavjga/jgYLjgb7jgorpganjgZfj' +
    'gabjgYTjgb7jgZvjgpPjgafjgZfjgZ/jgIIK44GT44Gu44Gf44KB44CBR3VpZG8g' +
    '44Gv44KI44KK5a6f55So55qE44Gq44OX44Ot44Kw44Op44Of44Oz44Kw6KiA6Kqe' +
    '44Gu6ZaL55m644KS6ZaL5aeL44GX44CB6Iux5Zu9IEJCUyDmlL7pgIHjga7jgrPj' +
    'g6Hjg4fjgqPnlarntYTjgIzjg6Ljg7Pjg4bjgqMg44OR44Kk44K944Oz44CN44Gu' +
    '44OV44Kh44Oz44Gn44GC44KLIEd1aWRvIOOBr+OBk+OBruiogOiqnuOCkuOAjFB5' +
    'dGhvbuOAjeOBqOWQjeOBpeOBkeOBvuOBl+OBn+OAggrjgZPjga7jgojjgYbjgaro' +
    'g4zmma/jgYvjgonnlJ/jgb7jgozjgZ8gUHl0aG9uIOOBruiogOiqnuioreioiOOB' +
    'r+OAgeOAjOOCt+ODs+ODl+ODq+OAjeOBp+OAjOe/kuW+l+OBjOWuueaYk+OAjeOB' +
    'qOOBhOOBhuebruaomeOBq+mHjeeCueOBjOe9ruOBi+OCjOOBpuOBhOOBvuOBmeOA' +
    'ggrlpJrjgY/jga7jgrnjgq/jg6rjg5fjg4jns7voqIDoqp7jgafjga/jg6bjg7zj' +
    'grbjga7nm67lhYjjga7liKnkvr/mgKfjgpLlhKrlhYjjgZfjgaboibLjgIXjgarm' +
    'qZ/og73jgpLoqIDoqp7opoHntKDjgajjgZfjgablj5bjgorlhaXjgozjgovloLTl' +
    'kIjjgYzlpJrjgYTjga7jgafjgZnjgYzjgIFQeXRob24g44Gn44Gv44Gd44GG44GE' +
    '44Gj44Gf5bCP57Sw5bel44GM6L+95Yqg44GV44KM44KL44GT44Go44Gv44GC44G+' +
    '44KK44GC44KK44G+44Gb44KT44CCCuiogOiqnuiHquS9k+OBruapn+iDveOBr+ac' +
    'gOWwj+mZkOOBq+aKvOOBleOBiOOAgeW/heimgeOBquapn+iDveOBr+aLoeW8teOD' +
    'ouOCuOODpeODvOODq+OBqOOBl+OBpui/veWKoOOBmeOCi+OAgeOBqOOBhOOBhuOB' +
    'ruOBjCBQeXRob24g44Gu44Od44Oq44K344O844Gn44GZ44CCCgrjg47jgYvjgpog' +
    '44OI44KaIOODiOOCre+otu+ouSDwoZq08KqOjCDpuoDpvYHwqZuwCg==',
    'UHl0aG9uIILMikqUrYLNgUExOTkwIJROgrKC64KpgueKSo5ugrOC6oLEgqKC3IK3' +
    'gUIKikqUrY7SgswgR3VpZG8gdmFuIFJvc3N1bSCCzYuziOeXcILMg3aDjYNPg4mD' +
    'foOTg0+MvozqgXVBQkOBdoLMikqUrYLJjlGJwYK1gsSCooLcgrWCvYKqgUFBQkMg' +
    'gs2OwJdwj+OCzJbak0mCyYLNgqCC3ILok0uCtYLEgqKC3IK5gvGCxYK1gr2BQgqC' +
    'sYLMgr2C34FBR3VpZG8ggs2C5oLojsCXcJNJgsiDdoONg0+DiYN+g5ODT4y+jOqC' +
    'zIpKlK2C8IpKjm6CtYFBiXCNkSBCQlMglfqRl4LMg1KDgYNmg0KU1JFngXWDgoOT' +
    'g2WDQiCDcINDg1yDk4F2gsyDdINAg5OCxYKggukgR3VpZG8ggs2CsYLMjL6M6oLw' +
    'gXVQeXRob26BdoLGlryCw4KvgtyCtYK9gUIKgrGCzILmgqSCyJR3jGmCqYLnkLaC' +
    '3ILqgr0gUHl0aG9uIILMjL6M6pDdjHaCzYFBgXWDVoOTg3aDi4F2gsWBdY9Lk76C' +
    'qpdliNWBdoLGgqKCpJbalVeCyY9kk1+CqpJ1gqmC6oLEgqKC3IK3gUIKkb2CrYLM' +
    'g1iDToOKg3aDZ4xujL6M6oLFgs2DhoFbg1WCzJbakOaCzJeYldaQq4Lwl0SQ5oK1' +
    'gsSQRoFYgsiLQJRcgvCMvozql3aRZoLGgrWCxI7mguiT/ILqgumP6o2HgqqRvYKi' +
    'gsyCxYK3gqqBQVB5dGhvbiCCxYLNgruCpIKigsGCvY+sjdeNSIKqkseJwYKzguqC' +
    '6YKxgsaCzYKggtyC6IKgguiC3IK5gvGBQgqMvozqjqmRzILMi0CUXILNjcWPrIzA' +
    'gsmJn4KzgqaBQZVLl3aCyItAlFyCzYpnkqODgoNXg4WBW4OLgsaCtYLEkseJwYK3' +
    'gumBQYLGgqKCpILMgqogUHl0aG9uIILMg3yDioNWgVuCxYK3gUIKCoNtgvUgg54g' +
    'g2eDTIhLiHkgmIP81iD80vzm+9QK', 50220);
  CheckCodePage('shift_jis',
    'UHl0aG9uIOOBrumWi+eZuuOBr+OAgTE5OTAg5bm044GU44KN44GL44KJ6ZaL5aeL' +
    '44GV44KM44Gm44GE44G+44GZ44CCCumWi+eZuuiAheOBriBHdWlkbyB2YW4gUm9z' +
    'c3VtIOOBr+aVmeiCsueUqOOBruODl+ODreOCsOODqeODn+ODs+OCsOiogOiqnuOA' +
    'jEFCQ+OAjeOBrumWi+eZuuOBq+WPguWKoOOBl+OBpuOBhOOBvuOBl+OBn+OBjOOA' +
    'gUFCQyDjga/lrp/nlKjkuIrjga7nm67nmoTjgavjga/jgYLjgb7jgorpganjgZfj' +
    'gabjgYTjgb7jgZvjgpPjgafjgZfjgZ/jgIIK44GT44Gu44Gf44KB44CBR3VpZG8g' +
    '44Gv44KI44KK5a6f55So55qE44Gq44OX44Ot44Kw44Op44Of44Oz44Kw6KiA6Kqe' +
    '44Gu6ZaL55m644KS6ZaL5aeL44GX44CB6Iux5Zu9IEJCUyDmlL7pgIHjga7jgrPj' +
    'g6Hjg4fjgqPnlarntYTjgIzjg6Ljg7Pjg4bjgqMg44OR44Kk44K944Oz44CN44Gu' +
    '44OV44Kh44Oz44Gn44GC44KLIEd1aWRvIOOBr+OBk+OBruiogOiqnuOCkuOAjFB5' +
    'dGhvbuOAjeOBqOWQjeOBpeOBkeOBvuOBl+OBn+OAggrjgZPjga7jgojjgYbjgaro' +
    'g4zmma/jgYvjgonnlJ/jgb7jgozjgZ8gUHl0aG9uIOOBruiogOiqnuioreioiOOB' +
    'r+OAgeOAjOOCt+ODs+ODl+ODq+OAjeOBp+OAjOe/kuW+l+OBjOWuueaYk+OAjeOB' +
    'qOOBhOOBhuebruaomeOBq+mHjeeCueOBjOe9ruOBi+OCjOOBpuOBhOOBvuOBmeOA' +
    'ggrlpJrjgY/jga7jgrnjgq/jg6rjg5fjg4jns7voqIDoqp7jgafjga/jg6bjg7zj' +
    'grbjga7nm67lhYjjga7liKnkvr/mgKfjgpLlhKrlhYjjgZfjgaboibLjgIXjgarm' +
    'qZ/og73jgpLoqIDoqp7opoHntKDjgajjgZfjgablj5bjgorlhaXjgozjgovloLTl' +
    'kIjjgYzlpJrjgYTjga7jgafjgZnjgYzjgIFQeXRob24g44Gn44Gv44Gd44GG44GE' +
    '44Gj44Gf5bCP57Sw5bel44GM6L+95Yqg44GV44KM44KL44GT44Go44Gv44GC44G+' +
    '44KK44GC44KK44G+44Gb44KT44CCCuiogOiqnuiHquS9k+OBruapn+iDveOBr+ac' +
    'gOWwj+mZkOOBq+aKvOOBleOBiOOAgeW/heimgeOBquapn+iDveOBr+aLoeW8teOD' +
    'ouOCuOODpeODvOODq+OBqOOBl+OBpui/veWKoOOBmeOCi+OAgeOBqOOBhOOBhuOB' +
    'ruOBjCBQeXRob24g44Gu44Od44Oq44K344O844Gn44GZ44CCCgo=',
    'UHl0aG9uIILMikqUrYLNgUExOTkwIJROgrKC64KpgueKSo5ugrOC6oLEgqKC3IK3' +
    'gUIKikqUrY7SgswgR3VpZG8gdmFuIFJvc3N1bSCCzYuziOeXcILMg3aDjYNPg4mD' +
    'foOTg0+MvozqgXVBQkOBdoLMikqUrYLJjlGJwYK1gsSCooLcgrWCvYKqgUFBQkMg' +
    'gs2OwJdwj+OCzJbak0mCyYLNgqCC3ILok0uCtYLEgqKC3IK5gvGCxYK1gr2BQgqC' +
    'sYLMgr2C34FBR3VpZG8ggs2C5oLojsCXcJNJgsiDdoONg0+DiYN+g5ODT4y+jOqC' +
    'zIpKlK2C8IpKjm6CtYFBiXCNkSBCQlMglfqRl4LMg1KDgYNmg0KU1JFngXWDgoOT' +
    'g2WDQiCDcINDg1yDk4F2gsyDdINAg5OCxYKggukgR3VpZG8ggs2CsYLMjL6M6oLw' +
    'gXVQeXRob26BdoLGlryCw4KvgtyCtYK9gUIKgrGCzILmgqSCyJR3jGmCqYLnkLaC' +
    '3ILqgr0gUHl0aG9uIILMjL6M6pDdjHaCzYFBgXWDVoOTg3aDi4F2gsWBdY9Lk76C' +
    'qpdliNWBdoLGgqKCpJbalVeCyY9kk1+CqpJ1gqmC6oLEgqKC3IK3gUIKkb2CrYLM' +
    'g1iDToOKg3aDZ4xujL6M6oLFgs2DhoFbg1WCzJbakOaCzJeYldaQq4Lwl0SQ5oK1' +
    'gsSQRoFYgsiLQJRcgvCMvozql3aRZoLGgrWCxI7mguiT/ILqgumP6o2HgqqRvYKi' +
    'gsyCxYK3gqqBQVB5dGhvbiCCxYLNgruCpIKigsGCvY+sjdeNSIKqkseJwYKzguqC' +
    '6YKxgsaCzYKggtyC6IKgguiC3IK5gvGBQgqMvozqjqmRzILMi0CUXILNjcWPrIzA' +
    'gsmJn4KzgqaBQZVLl3aCyItAlFyCzYpnkqODgoNXg4WBW4OLgsaCtYLEkseJwYK3' +
    'gumBQYLGgqKCpILMgqogUHl0aG9uIILMg3yDioNWgVuCxYK3gUIKCg==', 932);
end;

procedure TTestCoreBase.Iso8601DateAndTime;

  procedure Test(D: TDateTime; Expanded: boolean);
  var
    s, t: RawUtf8;
    E, F: TDateTime;
    I, J: TTimeLogBits;
    st, s2: TSynSystemTime;
    P: PUtf8Char;
    d1, d2: TSynDate;
  begin
    s := DateTimeToIso8601(D, Expanded);
    if Expanded then
      Check(length(s) = 19)
    else
      Check(length(s) = 15);
    if Expanded then
    begin
      Check(Iso8601CheckAndDecode(Pointer(s), length(s), E));
      Check(Abs(D - E) < (1 / SecsPerDay)); // we allow 999 ms error
    end;
    st.FromDateTime(D);
    {%H-}s2.Clear;
    DecodeDate(D, s2.Year, s2.Month, s2.Day);
    DecodeTime(D, s2.Hour, s2.Minute, s2.Second, s2.MilliSecond);
    Check(abs(st.MilliSecond - s2.MilliSecond) <= 1); // allow 1 ms rounding error
    st.MilliSecond := 0;
    s2.MilliSecond := 0;
    Check(st.IsEqual(s2)); // ensure conversion matches the RTL's
    t := st.ToText(Expanded);
    Check(Copy(t, 1, length(s)) = s);
    {%H-}d1.Clear;
    check(d1.IsZero);
    {%H-}d2.SetMax;
    check(not d2.IsZero);
    check{%H-}(not d1.IsEqual(d2));
    check(d1.Compare(d2) < 0);
    check(d2.Compare(d1) > 0);
    t := d2.ToText(false);
    check(t = '99991231');
    check(d2.ToText(true) = '9999-12-31');
    d2.Clear;
    check(d1.IsEqual(d2));
    check(d1.Compare(d2) = 0);
    check(d2.Compare(d1) = 0);
    P := pointer(s);
    check(d1.ParseFromText(P));
    check(P <> nil);
    check(not d1.IsZero);
    check(st.IsDateEqual(d1));
    t := d1.ToText(Expanded);
    check(copy(s, 1, length(t)) = t);
    d2.Clear;
    check(d2.IsZero);
    check(not d1.IsEqual(d2));
    check(d1.Compare(d2) > 0);
    check(d2.Compare(d1) < 0);
    check(d2.ToText(Expanded) = '');
    d2.SetMax;
    check(not d2.IsZero);
    check(not d1.IsEqual(d2));
    check(d1.Compare(d2) < 0);
    check(d2.Compare(d1) > 0);
    d2 := d1;
    check(d1.IsEqual(d2));
    check(d1.Compare(d2) = 0);
    check(d2.Compare(d1) = 0);
    E := Iso8601ToDateTime(s);
    Check(Abs(D - E) < (1 / SecsPerDay)); // we allow 999 ms error
    E := Iso8601ToDateTime(s + 'Z');
    Check(Abs(D - E) < (1 / SecsPerDay)); // we allow 999 ms error
    I.From(D);
    Check(Iso8601ToTimeLog(s) = I.Value);
    t := s;
    t[11] := ''''; // as in SynDB VArray[] quoted parameters
    J.From(pointer(t), 10);
    Check(I.Value and not (1 shl (6 + 6 + 5) - 1) = J.Value);
    I.From(s);
    t := I.Text(Expanded);
    if t <> s then // we allow error on time = 00:00:00 -> I.Text = just date
      Check(I.Value and (1 shl (6 + 6 + 5) - 1) = 0)
    else
      Check(true);
    J.From(E);
    Check(Int64(I) = Int64(J));
    s := TimeToIso8601(D, Expanded);
    Check(PosEx('.', s) = 0);
    Check(abs(frac(D) - Iso8601ToDateTime(s)) < 1 / SecsPerDay);
    s := TimeToIso8601(D, Expanded, 'T', true);
    Check(PosEx('.', s) > 0);
    F := Iso8601ToDateTime(s);
    Check(abs(frac(D) - F) < 1 / MSecsPerDay, 'withms1');
    s := DateToIso8601(D, Expanded);
    Check(trunc(D) = trunc(Iso8601ToDateTime(s)));
    Check(Abs(D - I.ToDateTime) < (1 / SecsPerDay));
    E := TimeLogToDateTime(I.Value);
    Check(Abs(D - E) < (1 / SecsPerDay));
    s := DateTimeToIso8601(D, Expanded, #0);
    if Expanded then
      Check(length(s) = 18)
    else
      Check(length(s) = 14);
    s := DateTimeToIso8601(D, Expanded, 'T', true);
    Check(PosEx('.', s) > 0);
    if Expanded then
      Check(length(s) = 23)
    else
      Check(length(s) = 19);
    F := Iso8601ToDateTime(s);
    Check(abs(D - F) < 1 / MSecsPerDay, 'withms2');
    if Expanded then
    begin
      F := 0;
      Check(Iso8601CheckAndDecode(pointer(s), length(s), F));
      Check(abs(D - F) < 1 / MSecsPerDay, 'withms3');
    end;
  end;

var
  i: integer;
  D: TDateTime;
  tmp: RawUtf8;
  b: TTimeLogBits;
  st, start: TSynSystemTime;
begin
  Check(st.FromText('19821031T142319'));
  start := st;
  CheckEqual(st.ToText, '1982-10-31T14:23:19.000');
  st.Second := 60;
  st.Normalize;
  CheckEqual(st.ToText, '1982-10-31T14:24:00.000', 'next minute');
  st.Minute := 60;
  st.Normalize;
  CheckEqual(st.ToText, '1982-10-31T15:00:00.000', 'next hour');
  st.Hour := 24;
  st.Normalize;
  CheckEqual(st.ToText, '1982-11-01T00:00:00.000', 'next day');
  st.Day := st.DaysInMonth + 1; // + 1 to switch to next month
  CheckEqual(st.Day, 31);
  st.Normalize;
  CheckEqual(st.ToText, '1982-12-01T00:00:00.000', 'next month');
  st.Month := 13;
  st.Normalize;
  CheckEqual(st.ToText, '1983-01-01T00:00:00.000', 'next year 1');
  st.Month := 13;
  st.Normalize;
  CheckEqual(st.ToText, '1984-01-01T00:00:00.000', 'next year 2');
  // reset each time - as THttpAnalyzer.ComputeConsolidateTime
  st := start;
  CheckEqual(st.ToText, '1982-10-31T14:23:19.000');
  st.Second := 60;
  st.Normalize;
  CheckEqual(st.ToText, '1982-10-31T14:24:00.000', 'nextminute');
  st := start;
  st.Second := 0;
  st.Minute := 60;
  st.Normalize;
  CheckEqual(st.ToText, '1982-10-31T15:00:00.000', 'nexthour');
  st := start;
  st.Second := 0;
  st.Minute := 0;
  st.Hour := 24;
  st.Normalize;
  CheckEqual(st.ToText, '1982-11-01T00:00:00.000', 'nextday');
  st := start;
  st.Second := 0;
  st.Minute := 0;
  st.Hour := 0;
  st.Day := st.DaysInMonth + 1; // + 1 to switch to next month
  CheckEqual(st.Day, 32);
  st.Normalize;
  CheckEqual(st.ToText, '1982-11-01T00:00:00.000', 'nextmonth');
  st := start;
  st.Second := 0;
  st.Minute := 0;
  st.Hour := 0;
  st.Day := 1;
  st.Month := 13;
  st.Normalize;
  CheckEqual(st.ToText, '1983-01-01T00:00:00.000', 'nextyear 1');
  st.Month := 13;
  st.Normalize;
  CheckEqual(st.ToText, '1984-01-01T00:00:00.000', 'nextyear 2');
  for i := 1700 to 2500 do
    Check(mormot.core.datetime.IsLeapYear(i) = SysUtils.IsLeapYear(i), 'IsLeapYear');
  // this will test typically from year 1905 to 2065
  D := Now / 20 + rnd.NextDouble * 20; // some starting random date/time
  for i := 1 to 2000 do
  begin
    Test(D, true);
    Test(D, false);
    D := D + rnd.NextDouble * 57; // go further a little bit: change date/time
  end;
  b.Value := Iso8601ToTimeLog('20150504');
  Check(b.Year = 2015);
  Check(b.Month = 5);
  Check(b.Day = 4);
  tmp := b.Text(false);
  CheckEqual(tmp, '20150504');
  IntervalTextToDateTimeVar('+0 06:03:20', D);
  CheckSame(D, 0.252314, 1e-5);
  D := IntervalTextToDateTime('+1 06:03:20');
  CheckSame(D, 1.252314, 1e-5);
  D := Iso8601ToDateTime('2022-05-11T23:59:56.971655858Z');
  CheckEqual(DateTimeToIso8601(D, true, 'T', true), '2022-05-11T23:59:56.971');
  CheckSame(IntervalTextToDateTime('-20 06:03:20'), -20.252314, 1e-6);
  Check(DateTimeToIso8601Text(IntervalTextToDateTime('+0 06:03:20')) = 'T06:03:20');
  tmp := DateTimeToIso8601Text(IntervalTextToDateTime('+1 06:03:20'));
  CheckEqual(tmp, '1899-12-31T06:03:20');
  tmp := DateTimeToIso8601Text(IntervalTextToDateTime('-2 06:03:20'));
  CheckEqual(tmp, '1899-12-28T06:03:20');
  CheckSame(TimeLogToDateTime(135131870949), 41578.477512, 1e-5);
  tmp := '1982-10-30T06:03:20';
  Check(Iso8601CheckAndDecode(Pointer(tmp), length(tmp), D));
  Check(DateTimeToIso8601(D, true) = tmp);
  tmp := '1982-10-30';
  Check(Iso8601CheckAndDecode(Pointer(tmp), length(tmp), D));
  Check(DateToIso8601(D, true) = tmp);
  tmp := 'T06:03:20';
  Check(Iso8601CheckAndDecode(Pointer(tmp), length(tmp), D));
  Check(TimeToIso8601(D, true) = tmp);
  tmp := '1982-10-30 06:03:20';
  Check(not Iso8601CheckAndDecode(Pointer(tmp), length(tmp), D));
  tmp := 'T06:03:2a';
  Check(not Iso8601CheckAndDecode(Pointer(tmp), length(tmp), D));
  tmp := '1435051262-45869-63626';
  check(Iso8601ToDateTime(tmp) = 0);
  check(Iso8601ToTimelog(tmp) = 0);
  tmp := UnixTimePeriodToString(0);
  CheckEqual(tmp, 'T00:00:00');
  tmp := UnixTimePeriodToString(30);
  CheckEqual(tmp, 'T00:00:30');
  tmp := UnixTimePeriodToString(SecsPerMin);
  CheckEqual(tmp, 'T00:01:00');
  tmp := UnixTimePeriodToString(SecsPerMin * MinsPerHour);
  CheckEqual(tmp, 'T01:00:00');
  tmp := UnixTimePeriodToString(SecsPerDay);
  CheckEqual(tmp, '0000-00-01');
  tmp := UnixTimePeriodToString(SecsPerDay * 15);
  CheckEqual(tmp, '0000-00-15');
  tmp := UnixTimePeriodToString(SecsPerDay * 31);
  CheckEqual(tmp, '0000-00-31');
  tmp := UnixTimePeriodToString(SecsPerDay * (31 + 4));
  CheckEqual(tmp, '0000-01-04');
  tmp := UnixTimePeriodToString(SecsPerDay * (31 + 28 + 7));
  CheckEqual(tmp, '0000-02-07');
  tmp := UnixTimePeriodToString(SecsPerDay * 365);
  CheckEqual(tmp, '0001-00-00');
  tmp := UnixTimePeriodToString(SecsPerDay * 365 + 1);
  CheckEqual(tmp, '0001-00-00');
  tmp := UnixTimePeriodToString(SecsPerDay * 366);
  CheckEqual(tmp, '0001-00-01');
  tmp := UnixTimePeriodToString(SecsPerDay * 365 * 2);
  CheckEqual(tmp, '0002-00-00');
end;

function LocalTimeToUniversal(LT: TDateTime; TZOffset: Integer): TDateTime;
begin
  result := EncodeTime(Abs(TZOffset) div 60, Abs(TZOffset) mod 60, 0, 0);
  if TZOffset > 0 then
    result := LT - result
  else if TZOffset < 0 then
    result := LT + result
  else
    result := LT;
end;

{$R ..\src\mormot.tz.res} // validate our Win10-generated resource file

procedure TTestCoreBase.TimeZonesSlow(Context: TObject);
var
  tz: TSynTimeZone;
  d: TTimeZoneData;
  i, bias: integer;
  m: word;
  hdl, reload: boolean;
  buf: RawByteString;
  dt: TDateTime;
  local: TDateTime;

  procedure testBias(year, expected: integer);
  begin
    check(tz.GetBiasForDateTime(EncodeDate(year, 10, 30), '1', bias, hdl));
    check(bias = expected);
  end;

begin
  // validate low-level HTTP date parsing functions
  bias := -10;
  Check(not ParseTimeZone('', bias));
  CheckEqual(bias, -10);
  Check(ParseTimeZone('-0000', bias));
  CheckEqual(bias, TimeZoneLocalBias);
  Check(ParseTimeZone('+0000', bias));
  CheckEqual(bias, 0);
  Check(ParseTimeZone('+0100', bias));
  CheckEqual(bias, 60);
  Check(ParseTimeZone('+1005', bias));
  CheckEqual(bias, 605);
  Check(ParseTimeZone('-1005', bias));
  CheckEqual(bias, -605);
  Check(not ParseTimeZone('+1O05', bias));
  CheckEqual(bias, -605);
  Check(not ParseTimeZone('+105', bias));
  CheckEqual(bias, -605);
  bias := -10;
  Check(not ParseTimeZone('toto', bias));
  CheckEqual(bias, -10);
  Check(ParseTimeZone('z', bias));
  CheckEqual(bias, 0);
  Check(ParseTimeZone('M', bias));
  CheckEqual(bias, 12 * 60);
  Check(ParseTimeZone('NZDT', bias));
  CheckEqual(bias, 13 * 60);
  Check(ParseTimeZone(' NZT ', bias));
  CheckEqual(bias, 12 * 60);
  Check(ParseTimeZone('utc', bias));
  CheckEqual(bias, 0);
  Check(not ParseTimeZone('uta', bias));
  CheckEqual(bias, 0);
  Check(ParseTimeZone(' east', bias));
  CheckEqual(bias, -10 * 60);
  Check(ParseTimeZone('y   ', bias));
  CheckEqual(bias, -12 * 60);
  Check(ParseTimeZone('gmT ', bias));
  CheckEqual(bias, 0);
  Check(ParseTimeZone('    IDLW    ', bias));
  CheckEqual(bias, -12 * 60);
  m := 0;
  Check(ParseMonth('Jan', m));
  CheckEqual(m, 1);
  Check(not ParseMonth('Jab', m));
  CheckEqual(m, 1);
  Check(ParseMonth(' DEC ', m));
  CheckEqual(m, 12);
  Check(ParseMonth(' apr-', m));
  CheckEqual(m, 4);
  dt := HttpDateToDateTime('Sun, 06 Nov 1994 08:49:37 GMT');
  CheckEqual(DateTimeToIso8601Text(dt), '1994-11-06T08:49:37');
  CheckEqual(DateTimeToHttpDate(dt), 'Sun, 06 Nov 1994 08:49:37 GMT');
  Check(UnixMSTimeUtcToHttpDate(DateTimeToUnixMSTime(dt)) =
    'Sun, 06 Nov 1994 08:49:37 GMT');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sunday, 06-DEC-94 08:49:37 UTC')), '1994-12-06T08:49:37');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sun Feb  6 08:49:37 1994')), '1994-02-06T08:49:37');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sun, 06 Nov 2021 08:49:37 east')), '2021-11-06T18:49:37');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sun, 06 Nov 08:49:37 east')), '');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sun, 06 Nov 2021 084937 east')), '');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Tue, 15 Nov 1994 12:45:26 Z')), '1994-11-15T12:45:26');
  CheckEqual(DateTimeToIso8601Text(HttpDateToDateTime(
    'Sunday, 06-Nov-94 08:49:37 GMT')), '1994-11-06T08:49:37');
  // validate common TSynTimeZone process
  tz := TSynTimeZone.Create;
  try
    check(tz.Zone = nil);
    FillCharFast(d, SizeOf(d), 0);
    for i := 0 to 40 do
    begin
      UInt32ToUtf8(i, RawUtf8(d.id));
      d.display := 'displayed ' + d.id;
      d.tzi.Bias := i;
      check(tz.Zones.Add(d) = i, 'add some zones');
    end;
    tz.Zones.ForceReHash;
    dt := nowutc;
    for reload := false to true do
    begin
      check(tz.Zone <> nil);
      check(tz.Zones.Count = 41);
      for i := 0 to 40 do
      begin
        UInt32ToUtf8(i, RawUtf8(d.id));
        check(tz.GetDisplay(d.id) = 'displayed ' + d.id);
        hdl := true;
        check(tz.GetBiasForDateTime(dt, d.id, bias, hdl));
        check(bias = i);
        check(not hdl);
      end;
      check(not tz.GetBiasForDateTime(dt, 'fail', bias, hdl));
      buf := tz.SaveToBuffer;
      tz.Zones.Clear;
      check(tz.Zone = nil);
      tz.LoadFromBuffer(buf);
    end;
    with tz.Zone[1] do
    begin
      SetLength(dyn, 4);
      dyn[0].year := 2000;
      dyn[0].tzi.bias := 3600;
      dyn[1].year := 2003;
      dyn[1].tzi.bias := 3601;
      dyn[2].year := 2005;
      dyn[2].tzi.bias := 3602;
      dyn[3].year := 2006;
      dyn[3].tzi.bias := 3603;
    end;
    testBias(1990, 3600);
    testBias(2000, 3600);
    testBias(2001, 3600);
    testBias(2002, 3600);
    testBias(2003, 3601);
    testBias(2004, 3601);
    testBias(2005, 3602);
    testBias(2006, 3603);
    testBias(2007, 3603);
    testBias(2008, 3603);
  finally
    tz.Free;
  end;
  // validate NowUtc / TimeZoneLocalBias
  dt := NowUtc;
  CheckSame(LocalTimeToUniversal(Now(), TimeZoneLocalBias), dt, 0.01,
    'NowUtc should not shift nor truncate time in respect to RTL Now');
  sleep(200);
  Check(not SameValue(dt, NowUtc),
    'NowUtc should not truncate time (e.g. to 5 sec resolution)');
  // validate zones taken from Windows registry or mormot.tz.res on POSIX
  tz := TSynTimeZone.Default;
  local := tz.UtcToLocal(dt, 'UTC');
  check(SameValue(local, dt));
  check(tz.GetBiasForDateTime(dt, 'UTC', bias, hdl));
  check(bias = 0);
  check(not hdl);
  local := tz.UtcToLocal(dt, 'Romance Standard Time');
  check(not SameValue(local, dt), 'Perfide Albion never matches the continent');
  check(tz.GetBiasForDateTime(dt, 'Romance Standard Time', bias, hdl));
  check(hdl);
  check(bias < 0, 'Paris is always ahead of London');
  buf := tz.SaveToBuffer;
  tz := TSynTimeZone.Create;
  try
    tz.LoadFromBuffer(buf);
    CheckSame(local, tz.UtcToLocal(dt, 'Romance Standard Time'));
  finally
    tz.Free;
  end;
  CheckSame(local, UtcToLocal(dt, 'Romance Standard Time'));
end;

const
  _REFSMB = // real export from a Lenovo T470 laptop
    'qjzHzUppv1D+CwAAAADeDgAAAZkAAxABIAIwA01lbW9yeSBJbml0IENvbXBsZQAAABB0ZQB' +
    'FbmQgb2YgRFhFIFBoYXNlAEJJT1MgQm9vaUQADggAAAAAAQAB3gAASW50ZWwoUikgU2lsaW' +
    'NvbiBWaWV3IFRlY2gAAABAbm9sb2d5AACGDQIAKAcXIAAAAAAAAAAQFwMAAwMDAQACGAAAA' +
    'P7/AgEABgARKAQAAwD+/0AAQAAAQA0AAQIagEBVCAMEBQYJAAAAIwBVCAIAsARDaGFubmVs' +
    'QS1ESU1NMABCQU5LIDAAU2FtcwAAAAB1bmcAMzU5N0Q4NEMATm9uZQBNNDcxQTJLNDNDQjE' +
    'tQyDGFwBSQyAgIAECESgFAzACAAAAAgESIwAEAAkAVeVCmgsyADk4NzY1NDMyMUAEAAAwAA' +
    'ATHwYDAP///wEwAQAAAAAAAAAAAAAAAAAAAAAAAAcTAAAAggcAAYABgACAACAAIAAABAUHT' +
    'DEgQ2FjaGUCBwgAAYEBASCCAIIAAgMiBQUFTDJIUwkAAYIBAAwADAMiBgUJTDNGUwQwCgAB' +
    'A80CAACAgOkGCAD/++u/A4pkAIwKKApBMwcACAAJAUUGAgIE/ADNAiCAAAgAAAQAVTNFMUj' +
    '+Q29ycG9yYXRpb25L/mUoVE0pIGk1LTczMAAABwAwVSBDUFUgQCAyLjYwR0h6RI+DmoOaAA' +
    'AYCwABAgDgA/+AmgAAAAAJfQAAEgADDQFGASRMRU5PVk8ATjFRRVQ5NVcgKDEuNwARAAAwI' +
    'CkAMDUvMnHBMDIyAQEbDAABAgMEzLuxmj40shGoXKeYgAAAAPAq1n4GBQaFGjIwSEVTMjNC' +
    'MFUAVGhpbmtQYWQgVDQ3MACEEI8AUEYwVkRFQjgAhBpfTVRfEktfQlVfI+xfRk2kLWdxI+x' +
    'ncQACD9IBVQEAADFFCWEACkWFABJLUxRxU0RLMEo0MDY5NyBXSU4ATDJIRjc3UzAIgIAAM0' +
    'UzQY90IEF2YWlsYWJsZU2PAAMWDgABCiE0AgICAgAAAABwgDCIAAAAAEaFg5pHVU5vIEFzc' +
    '2V0Qfxmb3JtZC+AGhcACAkPABECEhBOgWaBqqqqaAVVU0IgMQABiRAAEAYyAggRABAGMwII' +
    'EgAQBjQCCBMAEAY1AggUABAGNgIIFQAQBjcCCKoSoCQWABAGOAIIFwAQBjkCCBgCEAsfjIB' +
    'FdGhlcm5ldAIIGQIQBxyNgHh0IUhhbEiFAgAgTW+B4m9yAggaAhD/jY5IZG1pAggbABACRG' +
    'lzcGxheVBvcnQvRFYolECgSS1EAggcABAOSERNSQIIHQIQHx2MgEhlYWRwaJGDL01pY3JvY' +
    '+ggUZmABsAgYm8gSmFjaxMDHgAQISEDCREfAAEBAQMBAAABIAEATWVkaWFBU3JkEAMiDiBT' +
    'bG9BBwkRIAcRAQAAAFNpbUNhR2cMBSECAA0WIgERAgAJAAFlbi2AAAABVVMAABYaIwESAAA' +
    'DAmIJHi0E/3kHrEoFCgIARnJvbnQAUwAAgAJBTllPADAxQVY0MTkAMDMuMDEATElPTgIWJA' +
    'USxhvyKwT/CJAAAD4GiKVeUmVhcgBMR0M0MTI4OzOFBSUAAUtIT0lIR0lVQ0NIBAAQxEhJk' +
    'QSHUyYAVFAHAkJBWSBJL08gBAERQAAXAAYCAAEA/wIAAQEUFYBAAQQFAAIEAAABA/YPBPYP' +
    'BfUPAACCFCcAJEFNVBERAQGlrwLAARAAQIUEggCDQCgANQEACwEAAAHA+ABOnQIACcABCAA' +
    'LAH4QXAIAAP4A1xUDAIUhSgABACYBAHZQcm8BAAEA3RopADEQAwcDBQLwATAABQEAUmVmZX' +
    'JlbmNlIYQAwkFdZGUgLUJiAHVDbyF2VmVyc/KYVFhUIEFDTSB2JVUA3RoqCTGxAAACwAAMA' +
    'AMECwhcfhBPM01FIDExLjAATUVCeHcHkXRGaXJtd2FyZQYAAjQgBidWhmUgU0tVAADdSysA' +
    'CgEANHMD//////8EAAEAIQFQAwAGAIKODBACogAHAD4AAAIINAEAAZCxAAAACjTgCwBDA08' +
    'zU0tMIFBDSABBfS1DUgBUIGhJRCBTdGF0dXMAIedhQaNkWENPcmlnaW5x5lZhbHVlWENOIV' +
    'YEeU8QITqyUFJPTSHyUlNUIvJBSUQAdfkgSCBRpEhR+FcHePlEeCD7XUtC41RBokGtT1kgG' +
    'CB2+UxQQFoHkSRDrU8A3TYsAAcWAwBxIJtUATAzcwQF//////8G8g8CAK4AjJEH9A+B/wEA' +
    '/0AzAEEh8lN5c3RlbSBBZ2WRRk8zTVJDUXEh8lBDSXhQU0GQYAQvACgAdjiNTXY4gCMAVmL' +
    'bAAAPHy0AUgAAABAABBNA8AEAAQQCCAQKABQAFgFADIEBABgFLgAiAACEBy8AAdhhAxIXMA' +
    'EyJgCAAAAABQgVBzEABQQxAEAwoAAVBzIABwQhAIMWMwABAQALAAFUVlQtRW5ySm0ykgCIB' +
    'jQAWloAECAyYQCMEzVFhQsEAbIATVMgAQCMEzZGhQUBYwACABc3RoUGAYoTAQAFAA4ARAAC' +
    'CDgAAdsAACRNRZEE21E5ARMBRQIAlAaBEIkwAgBAyAABHwAAAADAAMkKQEQC///////////' +
    '/////////////////////MQCDAAAAKRAAAAAAAAAAAAAAAAAAAAAAQZVJMUKVMkKVMwAAhx' +
    'I6UhABAQBOCACQDwEAUgABjA87RoUHAQEC0UVIVDU0VwAwOC8xMC8yMDIxAYwrPEaFAAAAA' +
    'AgB////////////////////////////////////////AAAAAAAAfwT//gAA';

procedure TTestCoreBase.DmiSmbios;
var
  uid: TGUID;
  raw: TRawSmbiosInfo;
  os:  TSmbiosBasicInfos;
  dec: TSmbiosInfo;
  bak: byte;
  s: RawUtf8;
  b: RawByteString;

  procedure CheckAgainst(const full: TSmbiosInfo; const os: TSmbiosBasicInfos);
  begin
    CheckEqualTrim(full.Bios.VendorName,    os[sbiBiosVendor], 'vend');
    CheckEqualTrim(full.Bios.Version,       os[sbiBiosVersion], 'vers');
    CheckEqualTrim(full.Bios.Release,       os[sbiBiosRelease], 'rel');
    CheckEqualTrim(full.Bios.Firmware,      os[sbiBiosFirmware], 'firm');
    CheckEqualTrim(full.Bios.BuildDate,     os[sbiBiosDate], 'date');
    CheckEqualTrim(full.System.ProductName, os[sbiProductName], 'pname');
    CheckEqualTrim(full.System.Version,     os[sbiVersion], 'vers');
    CheckEqualTrim(full.System.Uuid,        os[sbiUuid], 'uuid');
    if full.Processor <> nil then
      CheckEqualTrim(full.Processor[0].Manufacturer, os[sbiCpuManufacturer], 'proc');
    if full.Battery <> nil then
      CheckEqualTrim(full.Battery[0].Manufacturer, os[sbiBatteryManufacturer], 'batt');
    if full.Oem <> nil then
      CheckEqualTrim(full.Oem[0], os[sbiOem], 'oem');
  end;

begin
  CheckEqual(ord(arm64DCPODP), 64);
  CheckEqual(ord(arm32AES), 32);
  CheckEqual(SizeOf(TSmbiosBiosFlags), 8);
  CheckEqual(SizeOf(TSmbiosMemory) - 7 * SizeOf(RawUtf8), 11);
  CheckEqual(SizeOf(TSmbiosMemoryArray) - 2 * SizeOf(pointer), 5);
  // validate actual retrieval from this computer
  GetComputerUuid(uid); // retrieve main SMBIOS and its UUID, or generate it
  Check(_SmbiosRetrieved);
  Check(not IsZero(THash128(uid)), 'machine UUID');
  GetSmbiosInfo; // parse using mormot.core.perf
  CheckAgainst(Smbios, _smbios);
  if Smbios.System.Uuid <> '' then
    Check(IdemPropNameU(ToUtf8(uid), Smbios.System.Uuid), 'uuid');
  // validate from reference binary export, decoding into binary or json
  raw.Data := Base64ToBin(_REFSMB);
  if CheckFailed(raw.Data <> '', '_REFSMB') or
     CheckFailed(CompressSynLZ(raw.Data, false) <> '', '_REFSMB synlz') then
    exit;
  PCardinal(@raw)^ := $010003ff;
  bak := PByte(@_SmbiosDecodeUuid)^;
  _SmbiosDecodeUuid := sduVersion; // consistent UUID decoding on all platforms
  CheckEqual(DecodeSmbios(raw, os), 3066, 'DecodeSmbios');
  Check(DecodeSmbiosInfo(raw, dec), 'DecodeSmbiosInfo');
  PByte(@_SmbiosDecodeUuid)^ := bak;
  CheckAgainst(dec, os);
  CheckHash(BinarySave(@dec.Bios,
    TypeInfo(TSmbiosBios), rkRecordTypes), $9362A439, 'Bios');
  CheckHash(BinarySave(@dec.System,
    TypeInfo(TSmbiosSystem), rkRecordTypes), $E9451367, 'System');
  CheckHash(BinarySave(@dec.Board[0],
    TypeInfo(TSmbiosBoard), rkRecordTypes), $25B6CB6C, 'Board');
  CheckHash(BinarySave(@dec.Chassis[0],
    TypeInfo(TSmbiosChassis), rkRecordTypes), $25633E53, 'Chassis');
  CheckHash(BinarySave(@dec.Processor[0],
    TypeInfo(TSmbiosProcessor), rkRecordTypes), $03E34B17, 'Proc');
  b := BinarySave(@dec.Memory[0], TypeInfo(TSmbiosMemoryArray), rkRecordTypes);
  //FileFromString(b, Executable.ProgramFilePath + CPU_ARCH_TEXT + '.dat');
  CheckEqual(length(b), 137, 'MemoryArray Len');
  CheckHash(b, $87CADDDA, 'MemoryArray');
  SaveJson(dec.Memory[0], TypeInfo(TSmbiosMemoryArray),
    [twoIgnoreDefaultInRecord], s);
  CheckEqual(s, '{"l":3,"u":3,"e":3,"c":"32 GB","n":2,"d":[{"w":64,"d":64,' +
    '"s":"16 GB","f":13,"r":2,"t":26,"e":16512,"l":"ChannelA-DIMM0","b":"B' +
    'ANK 0","m":"Samsung","n":"3597D84C","a":"None","p":"M471A2K43CB1-CRC"' +
    ',"c":2133},{"s":"0 B","f":2,"t":2,"l":"ChannelB-DIMM0","b":"BANK 2"}]}',
    'MemoryArray Json');
  CheckHash(BinarySave(@dec.Memory[0].Device[0],
    TypeInfo(TSmbiosMemory), rkRecordTypes), $895CE535, 'Memory');
  CheckHash(BinarySave(@dec.Connector[0],
    TypeInfo(TSmbiosConnector), rkRecordTypes), $129D5265, 'Conn');
  CheckHash(BinarySave(@dec.Slot[0],
    TypeInfo(TSmbiosSlot), rkRecordTypes), $0BE08E2D, 'Slot');
  CheckHash(BinarySave(@dec.Battery[0],
    TypeInfo(TSmbiosBattery), rkRecordTypes), $7FDA54A0, 'Battery');
  CheckHash(BinarySave(@dec,
    TypeInfo(TSmbiosInfo), rkRecordTypes), $C038E432, 'BinarySave1');
  SaveJson(dec, TypeInfo(TSmbiosInfo), [twoIgnoreDefaultInRecord], s);
  CheckHash(s, $87F82F78, 'BinarySave2');
end;

{$ifdef OSWINDOWS}

function CreateWellKnownSid(WellKnownSidType: byte; DomainSid: PSID;
  pSid: PSID; var cbSid: cardinal): BOOL; stdcall; external 'Advapi32.dll';
function ConvertSidToStringSidA(Sid: PSID; var StringSid: PAnsiChar): BOOL; stdcall;
  external 'Advapi32.dll';

{$endif OSWINDOWS}

procedure TTestCoreBase._SID;
var
  k: TWellKnownSid;
  s: RawUtf8;
  s1, s2: RawSid;
  ss: TShort47;
  {$ifdef OSWINDOWS}
  known: TWellKnownSids;
  sids: TRawUtf8DynArray;
  {$endif OSWINDOWS}
begin
  // validate cross-platform SID process
  CheckEqual(SizeOf(TSid), 1032, 'TSid');
  for k := low(k) to high(k) do
  begin
    s1 := KnownRawSid(k);
    Check(SidToKnown(pointer(s1)) = k);
    Check(SidCompare(pointer(s1), pointer(s1)) = 0);
    s := RawSidToText(s1);
    CheckEqual(s, RawUtf8(KnownSidToText(k)^));
    CheckUtf8(SidToKnown(s) = k, s);
    s2 := TextToRawSid(s);
    CheckEqual(s, RawSidToText(s2));
    CheckUtf8(SidCompare(pointer(s1), pointer(s2)) = 0, s);
  end;
  // some cross-platform Windows/Linux/BSD error detection
  Check(WinErrorConstant(NO_ERROR)^ = 'SUCCESS', 'weca');
  Check(WinErrorConstant(995)^ = 'OPERATION_ABORTED', 'wecb1');
  Check(WinErrorConstant(1150)^ = 'OLD_WIN_VERSION', 'wecb2');
  Check(WinErrorConstant(1450)^ = 'NO_SYSTEM_RESOURCES', 'wecb3');
  Check(WinErrorConstant(1907)^ = 'PASSWORD_MUST_CHANGE', 'wecb4');
  Check(WinErrorConstant(1200)^ = 'BAD_DEVICE', 'wecc');
  Check(WinErrorConstant(234)^ = 'MORE_DATA', 'wecd');
  Check(WinErrorConstant(5)^ = 'ACCESS_DENIED', 'wece');
  Check(WinErrorConstant(12002)^ = 'TIMEOUT', 'wecf');
  Check(WinErrorConstant($800b010a)^ = 'CERT_E_CHAINING', 'wecg');
  Check(WinErrorConstant($800b010c)^ = 'CERT_E_REVOKED', 'wecG');
  Check(WinErrorConstant($800b010d)^ = '', 'wech');
  Check(WinErrorConstant($80092002)^ = 'CRYPT_E_BAD_ENCODE', 'wecH');
  Check(WinErrorConstant(1229)^  = 'CONNECTION_INVALID', 'weci');
  Check(WinErrorConstant(122)^ = 'INSUFFICIENT_BUFFER', 'wecj');
  Check(WinErrorConstant(12152)^ = 'INVALID_SERVER_RESPONSE', 'weck');
  Check(WinErrorConstant(87)^ = 'INVALID_PARAMETER', 'wecl');
  Check(WinErrorConstant(1315)^ = 'INVALID_ACCOUNT_NAME', 'wecm');
  Check(WinErrorConstant(1331)^ = 'ACCOUNT_DISABLED', 'wecn');
  Check(WinErrorConstant(1342)^ = 'SERVER_NOT_DISABLED', 'weco');
  Check(WinErrorShort(0) = '0 ERROR_SUCCESS', 'w0');
  Check(WinErrorShort(5) = '5 ERROR_ACCESS_DENIED', 'wa');
  Check(WinErrorShort(12002) = '12002 ERROR_WINHTTP_TIMEOUT', 'w1');
  Check(WinErrorShort($800b010a) = '800b010a CERT_E_CHAINING', 'w2');
  Check(WinErrorShort($80000003) = '80000003 EXCEPTION_BREAKPOINT', 'w3');
  Check(WinErrorShort(1722) = '1722 RPC_S_SERVER_UNAVAILABLE', 'w4');
  Check(WinErrorShort(12152) = '12152 ERROR_WINHTTP_INVALID_SERVER_RESPONSE', 'w5');
  Check(WinErrorShort($c00000fd) = 'c00000fd EXCEPTION_STACK_OVERFLOW', 'w6');
  Check(WinErrorShort($80090330) = '80090330 SEC_E_DECRYPT_FAILURE', 'w7');
  Check(WinErrorShort($00090321) = '590625 SEC_I_RENEGOTIATE', 'w8');
  Check(WinErrorShort(244, {noint=}false) = '244', '244w');
  Check(WinErrorShort(245, {noint=}true) = '', '245w');
  BsdErrorShort(1, @ss);
  Check(ss = '1 EPERM', '1bsd');
  BsdErrorShort(5, @ss);
  Check(ss = '5 EIO', '5bsd');
  BsdErrorShort(40, @ss);
  Check(ss = '40 EMSGSIZE', '40bsd');
  BsdErrorShort(81, @ss);
  Check(ss = '81 ENEEDAUTH', '81bsd');
  BsdErrorShort(82, @ss);
  Check(ss = '82', '82bsd');
  LinuxErrorShort(1, @ss);
  Check(ss = '1 EPERM', '1lin');
  LinuxErrorShort(5, @ss);
  Check(ss = '5 EIO', '5lin');
  LinuxErrorShort(124, @ss);
  Check(ss = '124 EMEDIUMTYPE', '124');
  LinuxErrorShort(125, @ss);
  Check(ss = '125', '125');
  Check(OsErrorShort(244, {noint=}false) = '244', '244a');
  Check(OsErrorShort(244, {noint=}true) = '', '244b');
  // validate Windows specific SID function, especially about the current user
  {$ifdef OSWINDOWS}
  CurrentRawSid(s1, wttProcess);
  CurrentRawSid(s2, wttThread);
  Check(SidCompare(pointer(s1), pointer(s2)) = 0);
  s := RawSidToText(s1);
  CheckUtf8(IdemPChar(pointer(s), 'S-1-'), s);
  // domain users are S-1-5-21-*, but LOCAL_SYSTEM S-1-5-18 and AD user S-1-12
  sids := CurrentGroupsSid;
  Check(sids <> nil);
  known := CurrentKnownGroups;
  Check(known <> []);
  for k := low(k) to high(k) do
  begin
    Check(CurrentUserHasGroup(k) = (k in known));
    s := RawSidToText(KnownRawSid(k));
    if k in known then
    begin
      Check(FindRawUtf8(sids, s) >= 0);
      CheckUtf8(CurrentUserHasGroup(s), s);
    end
    else
      CheckUtf8(not CurrentUserHasGroup(s), s);
  end;
  CheckEqualShort(OSErrorShort(5), '5 ERROR_ACCESS_DENIED', '5ead');
  CheckEqualShort(OSErrorShort(5, true), 'ERROR_ACCESS_DENIED', '5ead2');
  {$else}
  CheckEqualShort(OSErrorShort(1), '1 EPERM', '1eperm');
  CheckEqualShort(OSErrorShort(5), '5 EIO', '5eio');
  CheckEqualShort(OSErrorShort(5, true), 'EIO', '5eio2');
  {$endif OSWINDOWS}
end;

const
  // some reference Security Descriptor self-relative buffers
  SD_B64: array[0..8] of RawUtf8 = (
    // 0 [MS-DTYP] 2.5.1.4 SDDL String to Binary Example
    'AQAUsJAAAACgAAAAFAAAADAAAAACABwAAQAAAAKAFAAAAACAAQEAAAAAAAEAAAAAAgBgAAQAAAAAAxgA' +
    'AAAAoAECAAAAAAAFIAAAACECAAAAAxgAAAAAEAECAAAAAAAFIAAAACACAAAAAxQAAAAAEAEBAAAAAAAF' +
    'EgAAAAADFAAAAAAQAQEAAAAAAAMAAAAAAQIAAAAAAAUgAAAAIAIAAAECAAAAAAAFIAAAACACAAA=',
    // https://learn.microsoft.com/en-us/windows/win32/secauthz/security-descriptor-string-format
    // 1
    'AQAUgCgBAAA4AQAAFAAAADAAAAACABwAAQAAAALAFAArAA0AAQEAAAAAAAEAAAAABAD4AAcAAAAAABQA' +
    'PwAPAAEBAAAAAAAFEgAAAAAAGAA/AA8AAQIAAAAAAAUgAAAAJAIAAAUALAADAAAAAQAAALp6lr/mDdAR' +
    'ooUAqgAwSeIBAgAAAAAABSAAAAAkAgAABQAsAAMAAAABAAAAnHqWv+YN0BGihQCqADBJ4gECAAAAAAAF' +
    'IAAAACQCAAAFACwAAwAAAAEAAAD/pKhtUg7QEaKGAKoAMEniAQIAAAAAAAUgAAAAJAIAAAUALAADAAAA' +
    'AQAAAKh6lr/mDdARooUAqgAwSeIBAgAAAAAABSAAAAAmAgAAAAAUABQAAgABAQAAAAAABQsAAAABAgAA' +
    'AAAABSAAAAAkAgAAAQEAAAAAAAUSAAAA',
    // 2
    'AQAEgDAAAABAAAAAAAAAABQAAAACABwAAQAAAAAAFAA/AA4QAQEAAAAAAAAAAAAAAQIAAAAAAAUgAAAA' +
    'JAIAAAEBAAAAAAAFEgAAAA==',
    // 3
    'AQAEgAAAAAAAAAAAAAAAABQAAAACABwAAQAAAAAAFAAAAAAQAQEAAAAAAAUHAAAA',
    // 4
    'AQAEgBQAAAAwAAAAAAAAAEwAAAABBQAAAAAABRUAAADRYBkx0xfaYIKt9RgBAgAAAQUAAAAAAAUVAAAA' +
    '0WAZMdMX2mCCrfUYAAIAAAIALAABAAAAAAAkAP8BHwABBQAAAAAABRUAAADRYBkx0xfaYIKt9RgAAgAA',
    // executable file access security descriptors, exported from several Windows VMs
    // 5 WinXP
    'AQAEgBQAAAAwAAAAAAAAAEwAAAABBQAAAAAABRUAAACCi6YoI/P2Y4qnMj/rAwAAAQUAAAAAAAUVAAAA' +
    'goumKCPz9mOKpzI/AQIAAAIAcAAEAAAAAAAYAP8BHwABAgAAAAAABSAAAAAgAgAAAAAUAP8BHwABAQAA' +
    'AAAABRIAAAAAACQA/wEfAAEFAAAAAAAFFQAAAIKLpigj8/ZjiqcyP+sDAAAAABgAqQASAAECAAAAAAAF' +
    'IAAAACECAAA=',
    // 6 Win7
    'AQAEhBQAAAAkAAAAAAAAAEAAAAABAgAAAAAABSAAAAAgAgAAAQUAAAAAAAUVAAAA0WAZMdMX2mCCrfUY' +
    'AQIAAAIAYAAEAAAAABAYAP8BHwABAgAAAAAABSAAAAAgAgAAABAUAP8BHwABAQAAAAAABRIAAAAAEBgA' +
    'qQASAAECAAAAAAAFIAAAACECAAAAEBQAvwETAAEBAAAAAAAFCwAAAA==',
    // 7 Win10
    'AQAEhBQAAAAwAAAAAAAAAEwAAAABBQAAAAAABRUAAACrWLmSPIyOxBiy0bzpAwAAAQUAAAAAAAUVAAA' +
    'Aq1i5kjyMjsQYstG8AQIAAAIAYAAEAAAAABAYAP8BHwABAgAAAAAABSAAAAAgAgAAABAUAP8BHwABAQ' +
    'AAAAAABRIAAAAAEBgAqQASAAECAAAAAAAFIAAAACECAAAAEBQAvwETAAEBAAAAAAAFCwAAAA==',
    // 8 Active Directory ntSecurityDescriptor field
    'AQAUjMQJAADgCQAAFAAAAIwAAAAEAHgAAgAAAAdaOAAgAAAAAwAAAL47Dv' +
    'Pwn9ERtgMAAPgDZ8Glepa/5g3QEaKFAKoAMEniAQEAAAAAAAEAAAAAB1o4ACAAAAADAAAAvzsO8' +
    '/Cf0RG2AwAA+ANnwaV6lr/mDdARooUAqgAwSeIBAQAAAAAAAQAAAAAEADgJMwAAAAEAJAD/AQ8A' +
    'AQUAAAAAAAUVAAAAb66a5T9f7J/5hle4VQQAAAUAOAAQAAAAAQAAAABCFkzAINARp2gAqgBuBSk' +
    'BBQAAAAAABRUAAABvrprlP1/sn/mGV7gpAgAABQA4ABAAAAABAAAAECAgX6V50BGQIADAT8LUzw' +
    'EFAAAAAAAFFQAAAG+umuU/X+yf+YZXuCkCAAAFADgAEAAAAAEAAABAwgq8qXnQEZAgAMBPwtTPA' +
    'QUAAAAAAAUVAAAAb66a5T9f7J/5hle4KQIAAAUAOAAQAAAAAQAAAPiIcAPhCtIRtCIAoMlo+TkB' +
    'BQAAAAAABRUAAABvrprlP1/sn/mGV7gpAgAABQA4ADAAAAABAAAAf3qWv+YN0BGihQCqADBJ4gE' +
    'FAAAAAAAFFQAAAG+umuU/X+yf+YZXuAUCAAAFACwAEAAAAAEAAAAdsalGrmBaQLfo/4pY1FbSAQ' +
    'IAAAAAAAUgAAAAMAIAAAUALAAwAAAAAQAAAByatm0ilNERrr0AAPgDZ8EBAgAAAAAABSAAAAAxA' +
    'gAABQAsADAAAAABAAAAYrwFWMm9KESl4oVqD0wYXgECAAAAAAAFIAAAADECAAAFACgAAAEAAAEA' +
    'AABTGnKrLx7QEZgZAKoAQFKbAQEAAAAAAAEAAAAABQAoAAABAAABAAAAUxpyqy8e0BGYGQCqAEB' +
    'SmwEBAAAAAAAFCgAAAAUAKAAAAQAAAQAAAFQacqsvHtARmBkAqgBAUpsBAQAAAAAABQoAAAAFAC' +
    'gAAAEAAAEAAABWGnKrLx7QEZgZAKoAQFKbAQEAAAAAAAUKAAAABQAoABAAAAABAAAAQi+6WaJ50' +
    'BGQIADAT8LTzwEBAAAAAAAFCwAAAAUAKAAQAAAAAQAAAFQBjeT4vNERhwIAwE+5YFABAQAAAAAA' +
    'BQsAAAAFACgAEAAAAAEAAACGuLV3SpTREa69AAD4A2fBAQEAAAAAAAULAAAABQAoABAAAAABAAA' +
    'As5VX5FWU0RGuvQAA+ANnwQEBAAAAAAAFCwAAAAUAKAAwAAAAAQAAAIa4tXdKlNERrr0AAPgDZ8' +
    'EBAQAAAAAABQoAAAAFACgAMAAAAAEAAACylVfkVZTREa69AAD4A2fBAQEAAAAAAAUKAAAABQAoA' +
    'DAAAAABAAAAs5VX5FWU0RGuvQAA+ANnwQEBAAAAAAAFCgAAAAAAJAD/AQ8AAQUAAAAAAAUVAAAA' +
    'b66a5T9f7J/5hle4AAIAAAAAGAD/AQ8AAQIAAAAAAAUgAAAAJAIAAAAAFAAAAAIAAQEAAAAAAAU' +
    'LAAAAAAAUAJQAAgABAQAAAAAABQoAAAAAABQA/wEPAAEBAAAAAAAFEgAAAAUaPAAQAAAAAwAAAA' +
    'BCFkzAINARp2gAqgBuBSkUzChINxS8RZsHrW8BXl8oAQIAAAAAAAUgAAAAKgIAAAUSPAAQAAAAA' +
    'wAAAABCFkzAINARp2gAqgBuBSm6epa/5g3QEaKFAKoAMEniAQIAAAAAAAUgAAAAKgIAAAUaPAAQ' +
    'AAAAAwAAABAgIF+ledARkCAAwE/C1M8UzChINxS8RZsHrW8BXl8oAQIAAAAAAAUgAAAAKgIAAAU' +
    'SPAAQAAAAAwAAABAgIF+ledARkCAAwE/C1M+6epa/5g3QEaKFAKoAMEniAQIAAAAAAAUgAAAAKg' +
    'IAAAUaPAAQAAAAAwAAAEDCCrypedARkCAAwE/C1M8UzChINxS8RZsHrW8BXl8oAQIAAAAAAAUgA' +
    'AAAKgIAAAUSPAAQAAAAAwAAAEDCCrypedARkCAAwE/C1M+6epa/5g3QEaKFAKoAMEniAQIAAAAA' +
    'AAUgAAAAKgIAAAUaPAAQAAAAAwAAAEIvulmiedARkCAAwE/C088UzChINxS8RZsHrW8BXl8oAQI' +
    'AAAAAAAUgAAAAKgIAAAUSPAAQAAAAAwAAAEIvulmiedARkCAAwE/C08+6epa/5g3QEaKFAKoAME' +
    'niAQIAAAAAAAUgAAAAKgIAAAUaPAAQAAAAAwAAAPiIcAPhCtIRtCIAoMlo+TkUzChINxS8RZsHr' +
    'W8BXl8oAQIAAAAAAAUgAAAAKgIAAAUSPAAQAAAAAwAAAPiIcAPhCtIRtCIAoMlo+Tm6epa/5g3Q' +
    'EaKFAKoAMEniAQIAAAAAAAUgAAAAKgIAAAUSOAAwAAAAAQAAAA/WR1uQYLJAnzcqTeiPMGMBBQA' +
    'AAAAABRUAAABvrprlP1/sn/mGV7gOAgAABRI4ADAAAAABAAAAD9ZHW5BgskCfNypN6I8wYwEFAA' +
    'AAAAAFFQAAAG+umuU/X+yf+YZXuA8CAAAFGjgACAAAAAMAAACmbQKbPA1cRovuUZnXFly6hnqWv' +
    '+YN0BGihQCqADBJ4gEBAAAAAAADAAAAAAUaOAAIAAAAAwAAAKZtAps8DVxGi+5RmdcWXLqGepa/' +
    '5g3QEaKFAKoAMEniAQEAAAAAAAUKAAAABRo4ABAAAAADAAAAbZ7Gt8cs0hGFTgCgyYP2CIZ6lr/' +
    'mDdARooUAqgAwSeIBAQAAAAAABQkAAAAFGjgAEAAAAAMAAABtnsa3xyzSEYVOAKDJg/YInHqWv+' +
    'YN0BGihQCqADBJ4gEBAAAAAAAFCQAAAAUSOAAQAAAAAwAAAG2exrfHLNIRhU4AoMmD9gi6epa/5' +
    'g3QEaKFAKoAMEniAQEAAAAAAAUJAAAABRo4ACAAAAADAAAAk3sb6khe1Ua8bE30/aeKNYZ6lr/m' +
    'DdARooUAqgAwSeIBAQAAAAAABQoAAAAFGiwAlAACAAIAAAAUzChINxS8RZsHrW8BXl8oAQIAAAA' +
    'AAAUgAAAAKgIAAAUaLACUAAIAAgAAAJx6lr/mDdARooUAqgAwSeIBAgAAAAAABSAAAAAqAgAABR' +
    'IsAJQAAgACAAAAunqWv+YN0BGihQCqADBJ4gECAAAAAAAFIAAAACoCAAAFEygAMAAAAAEAAADlw' +
    '3g/mve9RqC4nRgRbdx5AQEAAAAAAAUKAAAABRIoADABAAABAAAA3kfmkW/ZcEuVV9Y/9PPM2AEB' +
    'AAAAAAAFCgAAAAASJAD/AQ8AAQUAAAAAAAUVAAAAb66a5T9f7J/5hle4BwIAAAASGAAEAAAAAQI' +
    'AAAAAAAUgAAAAKgIAAAASGAC9AQ8AAQIAAAAAAAUgAAAAIAIAAAEFAAAAAAAFFQAAAG+umuU/X+' +
    'yf+YZXuAACAAABBQAAAAAABRUAAABvrprlP1/sn/mGV7gAAgAA'
    );
  // the expected SDDL export of those binary buffers
  SD_TXT: array[0..high(SD_B64)] of RawUtf8 = (
    // 0
    'O:BAG:BAD:P(A;OICI;GXGR;;;BU)(A;OICI;GA;;;BA)(A;OICI;GA;;;SY)(A;OICI;GA;;;CO)' +
    'S:P(AU;FA;GR;;;WD)',
    // 1
    'O:AOG:SYD:(A;;KA;;;SY)(A;;KA;;;AO)(OA;;CCDC;bf967aba-0de6-11d0-a285-00aa003049e2' +
    ';;AO)(OA;;CCDC;bf967a9c-0de6-11d0-a285-00aa003049e2;;AO)(OA;;CCDC;6da8a4ff-0e52-' +
    '11d0-a286-00aa003049e2;;AO)(OA;;CCDC;bf967aa8-0de6-11d0-a285-00aa003049e2;;PO)(A' +
    ';;LCRPRC;;;AU)S:(AU;SAFA;CCDCSWWPSDWDWO;;;WD)',
    // 2
    'O:AOG:SYD:(A;;CCDCLCSWRPWPRCWDWOGA;;;S-1-0-0)',
    // 3
    'D:(A;;GA;;;AN)',
    // 4
    'O:S-1-5-21-823746769-1624905683-418753922-513' +
    'G:S-1-5-21-823746769-1624905683-418753922-512' +
    'D:(A;;FA;;;S-1-5-21-823746769-1624905683-418753922-512)',
    // 5
    'O:S-1-5-21-682003330-1677128483-1060284298-1003' +
    'G:S-1-5-21-682003330-1677128483-1060284298-513' +
    'D:(A;;FA;;;BA)(A;;FA;;;SY)' +
      '(A;;FA;;;S-1-5-21-682003330-1677128483-1060284298-1003)(A;;0x1200a9;;;BU)',
    // 6
    'O:BA' +
    'G:S-1-5-21-823746769-1624905683-418753922-513' +
    'D:AI(A;ID;FA;;;BA)(A;ID;FA;;;SY)(A;ID;0x1200a9;;;BU)(A;ID;0x1301bf;;;AU)',
    // 7
    'O:S-1-5-21-2461620395-3297676348-3167859224-1001' +
    'G:S-1-5-21-2461620395-3297676348-3167859224-513' +
    'D:AI(A;ID;FA;;;BA)(A;ID;FA;;;SY)(A;ID;0x1200a9;;;BU)(A;ID;0x1301bf;;;AU)',
    //8
    'O:S-1-5-21-3852119663-2683068223-3092743929-512' +
    'G:S-1-5-21-3852119663-2683068223-3092743929-512' +
    'D:AI(D;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;S-1-5-21-3852119663-2683068223-3092743929-1109)' +
    '(OA;;RP;4c164200-20c0-11d0-a768-00aa006e0529;;S-1-5-21-3852119663-2683068223-3092743929-553)' +
    '(OA;;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;;S-1-5-21-3852119663-2683068223-3092743929-553)' +
    '(OA;;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;;S-1-5-21-3852119663-2683068223-3092743929-553)' +
    '(OA;;RP;037088f8-0ae1-11d2-b422-00a0c968f939;;S-1-5-21-3852119663-2683068223-3092743929-553)' +
    '(OA;;RPWP;bf967a7f-0de6-11d0-a285-00aa003049e2;;S-1-5-21-3852119663-2683068223-3092743929-517)' +
    '(OA;;RP;46a9b11d-60ae-405a-b7e8-ff8a58d456d2;;S-1-5-32-560)' +
    '(OA;;RPWP;6db69a1c-9422-11d1-aebd-0000f80367c1;;S-1-5-32-561)' +
    '(OA;;RPWP;5805bc62-bdc9-4428-a5e2-856a0f4c185e;;S-1-5-32-561)' +
    '(OA;;CR;ab721a53-1e2f-11d0-9819-00aa0040529b;;WD)' +
    '(OA;;CR;ab721a53-1e2f-11d0-9819-00aa0040529b;;PS)' +
    '(OA;;CR;ab721a54-1e2f-11d0-9819-00aa0040529b;;PS)' +
    '(OA;;CR;ab721a56-1e2f-11d0-9819-00aa0040529b;;PS)' +
    '(OA;;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;;AU)' +
    '(OA;;RP;e48d0154-bcf8-11d1-8702-00c04fb96050;;AU)' +
    '(OA;;RP;77b5b886-944a-11d1-aebd-0000f80367c1;;AU)' +
    '(OA;;RP;e45795b3-9455-11d1-aebd-0000f80367c1;;AU)' +
    '(OA;;RPWP;77b5b886-944a-11d1-aebd-0000f80367c1;;PS)' +
    '(OA;;RPWP;e45795b2-9455-11d1-aebd-0000f80367c1;;PS)' +
    '(OA;;RPWP;e45795b3-9455-11d1-aebd-0000f80367c1;;PS)' +
    '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;S-1-5-21-3852119663-2683068223-3092743929-512)' +
    '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;AO)' +
    '(A;;RC;;;AU)(A;;LCRPLORC;;;PS)' +
    '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;SY)' +
    '(OA;CIIOID;RP;4c164200-20c0-11d0-a768-00aa006e0529;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIID;RP;4c164200-20c0-11d0-a768-00aa006e0529;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIIOID;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIID;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIIOID;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIID;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIIOID;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIID;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIIOID;RP;037088f8-0ae1-11d2-b422-00a0c968f939;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIID;RP;037088f8-0ae1-11d2-b422-00a0c968f939;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIID;RPWP;5b47d60f-6090-40b2-9f37-2a4de88f3063;;S-1-5-21-3852119663-2683068223-3092743929-526)' +
    '(OA;CIID;RPWP;5b47d60f-6090-40b2-9f37-2a4de88f3063;;S-1-5-21-3852119663-2683068223-3092743929-527)' +
    '(OA;CIIOID;SW;9b026da6-0d3c-465c-8bee-5199d7165cba;bf967a86-0de6-11d0-a285-00aa003049e2;CO)' +
    '(OA;CIIOID;SW;9b026da6-0d3c-465c-8bee-5199d7165cba;bf967a86-0de6-11d0-a285-00aa003049e2;PS)' +
    '(OA;CIIOID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967a86-0de6-11d0-a285-00aa003049e2;ED)' +
    '(OA;CIIOID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967a9c-0de6-11d0-a285-00aa003049e2;ED)' +
    '(OA;CIID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967aba-0de6-11d0-a285-00aa003049e2;ED)' +
    '(OA;CIIOID;WP;ea1b7b93-5e48-46d5-bc6c-4df4fda78a35;bf967a86-0de6-11d0-a285-00aa003049e2;PS)' +
    '(OA;CIIOID;LCRPLORC;;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
    '(OA;CIIOID;LCRPLORC;;bf967a9c-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;CIID;LCRPLORC;;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
    '(OA;OICIID;RPWP;3f78c3e5-f79a-46bd-a0b8-9d18116ddc79;;PS)' +
    '(OA;CIID;RPWPCR;91e647de-d96f-4b70-9557-d63ff4f3ccd8;;PS)' +
    '(A;CIID;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;S-1-5-21-3852119663-2683068223-3092743929-519)' +
    '(A;CIID;LC;;;RU)(A;CIID;CCLCSWRPWPLOCRSDRCWDWO;;;BA)' +
    'S:AI(OU;CIIOIDSA;WP;f30e3bbe-9ff0-11d1-b603-0000f80367c1;bf967aa5-0de6-11d0-a285-00aa003049e2;WD)' +
    '(OU;CIIOIDSA;WP;f30e3bbf-9ff0-11d1-b603-0000f80367c1;bf967aa5-0de6-11d0-a285-00aa003049e2;WD)');
  // the Domain SID to be used for RID recognition
  DOM_TXT: array[4..high(SD_B64)] of RawUtf8 = (
    'S-1-5-21-823746769-1624905683-418753922',    // 4
    'S-1-5-21-682003330-1677128483-1060284298',   // 5
    'S-1-5-21-823746769-1624905683-418753922',    // 6
    'S-1-5-21-2461620395-3297676348-3167859224',  // 7
    'S-1-5-21-3852119663-2683068223-3092743929'); // 8
  // the SDDL with proper RID recognition
  RID_TXT: array[4..high(SD_B64)] of RawUtf8 = (
    'O:DUG:DAD:(A;;FA;;;DA)',
    'O:S-1-5-21-682003330-1677128483-1060284298-1003G:DUD:(A;;FA;;;BA)(A;;FA;;;SY)' +
      '(A;;FA;;;S-1-5-21-682003330-1677128483-1060284298-1003)(A;;0x1200a9;;;BU)',
    'O:BAG:DUD:AI(A;ID;FA;;;BA)(A;ID;FA;;;SY)(A;ID;0x1200a9;;;BU)(A;ID;0x1301bf;;;AU)',
    'O:S-1-5-21-2461620395-3297676348-3167859224-1001G:DUD:AI(A;ID;FA;;;BA)' +
      '(A;ID;FA;;;SY)(A;ID;0x1200a9;;;BU)(A;ID;0x1301bf;;;AU)',
    'O:DAG:DAD:AI(D;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;S-1-5-21-3852119663-2683068223-3092743929-1109)' +
      '(OA;;RP;4c164200-20c0-11d0-a768-00aa006e0529;;RS)' +
      '(OA;;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;;RS)' +
      '(OA;;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;;RS)' +
      '(OA;;RP;037088f8-0ae1-11d2-b422-00a0c968f939;;RS)' +
      '(OA;;RPWP;bf967a7f-0de6-11d0-a285-00aa003049e2;;CA)' +
      '(OA;;RP;46a9b11d-60ae-405a-b7e8-ff8a58d456d2;;S-1-5-32-560)' +
      '(OA;;RPWP;6db69a1c-9422-11d1-aebd-0000f80367c1;;S-1-5-32-561)' +
      '(OA;;RPWP;5805bc62-bdc9-4428-a5e2-856a0f4c185e;;S-1-5-32-561)' +
      '(OA;;CR;ab721a53-1e2f-11d0-9819-00aa0040529b;;WD)' +
      '(OA;;CR;ab721a53-1e2f-11d0-9819-00aa0040529b;;PS)' +
      '(OA;;CR;ab721a54-1e2f-11d0-9819-00aa0040529b;;PS)' +
      '(OA;;CR;ab721a56-1e2f-11d0-9819-00aa0040529b;;PS)' +
      '(OA;;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;;AU)' +
      '(OA;;RP;e48d0154-bcf8-11d1-8702-00c04fb96050;;AU)' +
      '(OA;;RP;77b5b886-944a-11d1-aebd-0000f80367c1;;AU)' +
      '(OA;;RP;e45795b3-9455-11d1-aebd-0000f80367c1;;AU)' +
      '(OA;;RPWP;77b5b886-944a-11d1-aebd-0000f80367c1;;PS)' +
      '(OA;;RPWP;e45795b2-9455-11d1-aebd-0000f80367c1;;PS)' +
      '(OA;;RPWP;e45795b3-9455-11d1-aebd-0000f80367c1;;PS)' +
      '(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;DA)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;AO)' +
      '(A;;RC;;;AU)(A;;LCRPLORC;;;PS)(A;;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;SY)' +
      '(OA;CIIOID;RP;4c164200-20c0-11d0-a768-00aa006e0529;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIID;RP;4c164200-20c0-11d0-a768-00aa006e0529;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIIOID;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIID;RP;5f202010-79a5-11d0-9020-00c04fc2d4cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIIOID;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIID;RP;bc0ac240-79a9-11d0-9020-00c04fc2d4cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIIOID;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIID;RP;59ba2f42-79a2-11d0-9020-00c04fc2d3cf;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIIOID;RP;037088f8-0ae1-11d2-b422-00a0c968f939;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIID;RP;037088f8-0ae1-11d2-b422-00a0c968f939;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIID;RPWP;5b47d60f-6090-40b2-9f37-2a4de88f3063;;KA)' +
      '(OA;CIID;RPWP;5b47d60f-6090-40b2-9f37-2a4de88f3063;;EK)' +
      '(OA;CIIOID;SW;9b026da6-0d3c-465c-8bee-5199d7165cba;bf967a86-0de6-11d0-a285-00aa003049e2;CO)' +
      '(OA;CIIOID;SW;9b026da6-0d3c-465c-8bee-5199d7165cba;bf967a86-0de6-11d0-a285-00aa003049e2;PS)' +
      '(OA;CIIOID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967a86-0de6-11d0-a285-00aa003049e2;ED)' +
      '(OA;CIIOID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967a9c-0de6-11d0-a285-00aa003049e2;ED)' +
      '(OA;CIID;RP;b7c69e6d-2cc7-11d2-854e-00a0c983f608;bf967aba-0de6-11d0-a285-00aa003049e2;ED)' +
      '(OA;CIIOID;WP;ea1b7b93-5e48-46d5-bc6c-4df4fda78a35;bf967a86-0de6-11d0-a285-00aa003049e2;PS)' +
      '(OA;CIIOID;LCRPLORC;;4828cc14-1437-45bc-9b07-ad6f015e5f28;RU)' +
      '(OA;CIIOID;LCRPLORC;;bf967a9c-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;CIID;LCRPLORC;;bf967aba-0de6-11d0-a285-00aa003049e2;RU)' +
      '(OA;OICIID;RPWP;3f78c3e5-f79a-46bd-a0b8-9d18116ddc79;;PS)' +
      '(OA;CIID;RPWPCR;91e647de-d96f-4b70-9557-d63ff4f3ccd8;;PS)' +
      '(A;CIID;CCDCLCSWRPWPDTLOCRSDRCWDWO;;;EA)(A;CIID;LC;;;RU)' +
      '(A;CIID;CCLCSWRPWPLOCRSDRCWDWO;;;BA)' +
      'S:AI(OU;CIIOIDSA;WP;f30e3bbe-9ff0-11d1-b603-0000f80367c1;bf967aa5-0de6-11d0-a285-00aa003049e2;WD)' +
      '(OU;CIIOIDSA;WP;f30e3bbf-9ff0-11d1-b603-0000f80367c1;bf967aa5-0de6-11d0-a285-00aa003049e2;WD)');
  // [MS-DTYP] 2.4.4.17.9 Examples: Conditional Expression Binary Representation
  ARTX_HEX: array[0..2] of RawUtf8 = (
    '61727478f80a0000005400690074006c00650010040000005600500080000000',
    '61727478f91200000073006d006100720074006300610072006400' +
    '040100000000000000030280fb0e0000006d0061006e006100670065006400' +
    '040100000000000000030280a1fa080000006400650070007400' +
    '5018000000100a000000530061006c006500730010040000004800520088a000',
    '61727478f91c00000063006c0065006100720061006e00630065004c006500760065006c00' +
    'fa220000007200650071007500690072006500640043006c0065006100720061006e0063006500' +
    '85501500000051100000000102000000000005200000002002000089a1000000');
  ARTX_TXT: array[0..high(ARTX_HEX)] of RawUtf8 = (
    '(Title=="VP")',
    '(((@User.smartcard==1) || (@Device.managed==1)) && (@Resource.dept Any_of{"Sales","HR"}))',
    '((@User.clearanceLevel>=@Resource.requiredClearance) || (Member_of{SID(BA)}))');
  // some ACE conditional expressions
  COND_TXT: array[0..2] of RawUtf8 = (
    'D:(XA;;FX;;;WD;(@User.Title=="PM" && (@User.Division=="Finance" || ' +
      '@User.Division ==" Sales")))',
    'D:(XA;;FX;;;WD;(@User.Project Any_of @Resource.Project))(A;ID;FA;;;SY)',
    'D:(XA;;FR;;;WD;(Member_of{SID(S-1-5-21-111-222-333-500),SID(BO)} && ' +
      '@Device.Bitlocker))(A;ID;FA;;;SY)');
  // our SDDL output always add parenthesis on binary expressions
  COND_EXP: array[0..2] of RawUtf8 = (
    'D:(XA;;FX;;;WD;((@User.Title=="PM") && ((@User.Division=="Finance") || ' +
      '(@User.Division==" Sales"))))',
    'D:(XA;;FX;;;WD;(@User.Project Any_of @Resource.Project))(A;ID;FA;;;SY)',
    'D:(XA;;FR;;;WD;((Member_of{SID(S-1-5-21-111-222-333-500),SID(BO)}) && '+
      '@Device.Bitlocker))(A;ID;FA;;;SY)');

procedure TTestCoreBase._SDDL;
var
  i, j: PtrInt;
  c: TSecControls;
  k, k2: TWellKnownSid;
  r, r2: TWellKnownRid;
  bin, saved: RawSecurityDescriptor;
  u, u2, dom, dom2, json: RawUtf8;
  all: TRawUtf8DynArray;
  n: integer;
  domsid: RawSid;
  sd, sd2: TSecurityDescriptor;
  bintree: TAceBinaryTree;
  sddltree: TAceTextTree;
  atp: TAceTextParse;
  a: TAdsKnownAttribute;
  p: PUtf8Char;
begin
  // validate internal structures and types
  CheckEqual(KnownSidToSddl(wksNull), '');
  CheckEqual(KnownSidToSddl(wksWorld), 'WD');
  CheckEqual(KnownSidToSddl(wksLocal), '');
  CheckEqual(KnownSidToSddl(wksCreatorOwnerRights), 'OW');
  CheckEqual(KnownSidToSddl(wksDialup), '');
  CheckEqual(KnownSidToSddl(wksNetwork), 'NU');
  CheckEqual(KnownSidToSddl(wksSelf), 'PS');
  CheckEqual(KnownSidToSddl(wksLocalSystem), 'SY');
  CheckEqual(KnownSidToSddl(wksBuiltinAdministrators), 'BA');
  CheckEqual(KnownSidToSddl(wksBuiltinNetworkConfigurationOperators), 'NO');
  CheckEqual(KnownSidToSddl(wksBuiltinPerfLoggingUsers), 'LU');
  CheckEqual(KnownSidToSddl(wksBuiltinEventLogReadersGroup), 'ER');
  CheckEqual(KnownSidToSddl(wksBuiltinAccessControlAssistanceOperators), 'AA');
  CheckEqual(KnownSidToSddl(wksBuiltinWriteRestrictedCode), 'WR');
  CheckEqual(KnownSidToSddl(wksBuiltinUserModeDriver), 'UD');
  CheckEqual(KnownSidToSddl(wksBuiltinAnyPackage), 'AC');
  CheckEqual(KnownSidToSddl(wksAuthenticationServiceAsserted), 'SS');
  CheckEqual(KnownSidToSddl(wksCapabilityInternetClient), '');
  CheckEqual(KnownSidToSddl(high(TWellKnownSid)), '');
  CheckEqual(KnownRidToSddl(wrkUserModeHwOperator), 'HO');
  n := 0;
  for k := low(k) to high(k) do
  begin
    u := KnownSidToSddl(k);
    Check((u <> '') = (k in wksWithSddl));
    Check(SddlToKnownSid(u, k2) = (u <> ''));
    if u = '' then
      continue;
    CheckUtf8(k2 = k, u);
    CheckUtf8(not SddlToKnownRid(u, r2), u);
    AddRawUtf8(all, n, FormatUtf8('% = %', [u, ToText(k)^]));
  end;
  for r := low(r) to high(r) do
  begin
    u := KnownRidToSddl(r);
    Check((u <> '') = (r in wkrWithSddl));
    Check(SddlToKnownRid(u, r2) = (u <> ''));
    if u = '' then
      continue;
    CheckUtf8(r2 = r, u);
    CheckUtf8(not SddlToKnownSid(u, k2), u);
    AddRawUtf8(all, n, FormatUtf8('% = %', [u, ToText(r)^]));
  end;
  DynArrayFakeLength(all, n);
  QuickSortRawUtf8(all, n);
  u := RawUtf8ArrayToCsv(all, #10); //ConsoleWrite(u);
  CheckHash(u, $30EF8B6D, 'sid sddl list');
  CheckEqual(SizeOf(TSid)    and 3, 0, 'TSid    DWORD-aligned');
  CheckEqual(SizeOf(TRawSD)  and 3, 0, 'TRawSD  DWORD-aligned');
  CheckEqual(SizeOf(TRawAcl) and 3, 0, 'TRawAcl DWORD-aligned');
  CheckEqual(SizeOf(TRawAce) and 3, 0, 'TRawAce DWORD-aligned');
  CheckEqual(ord(scDaclAutoInheritReq), 8);
  CheckEqual(ord(scSelfRelative), 15);
  c := [scSelfRelative];
  CheckEqual(PWord(@c)^, $8000);
  CheckEqual(ord(samGenericRead), 31, 'sam');
  dom := 'S-1-5-21-823746769-1624905683-418753922';
  CheckEqual(KnownSidToText(wkrUserAdmin, dom), dom + '-500');
  CheckEqual(KnownSidToText(wrkGroupRasServers, dom), dom + '-553');
  // validate against some reference binary material
  for i := 0 to high(SD_B64) do
  begin
    // high-level IsValidSecurityDescriptor() and SecurityDescriptorToText()
    bin := Base64ToBin(SD_B64[i]);
    Check(bin <> '', 'b64');
    Check(IsValidSecurityDescriptor(pointer(bin), length(bin)), 'bin');
    Check(SecurityDescriptorToText(bin, u), 'sdtt1');
    CheckEqual(u, SD_TXT[i]);
    {$ifdef OSWINDOWS} // validate against the OS API
    Check(CryptoApi.SecurityDescriptorToText(pointer(bin), u), 'winapi1');
    CheckEqual(u, SD_TXT[i], 'winapi2');
    {$endif OSWINDOWS}
    // TSecurityDescriptor binary load and export as SDDL or binary
    sd.Clear;
    CheckEqual(sd.ToText, '', 'clear');
    Check(sd.Flags = [scSelfRelative]);
    Check(sd.FromBinary(bin));
    Check(sd.Dacl <> nil, 'dacl');
    Check(scSelfRelative in sd.Flags);
    Check((sd.Sacl = nil) = (i in [2 .. 7]) , 'sacl');
    CheckEqual(sd.ToText, SD_TXT[i], 'ToText');
    Check(sd.Dacl[0].Opaque = '');
    Check(sd.Dacl[0].ConditionalExpression = '');
    saved := sd.ToBinary;
    Check(IsValidSecurityDescriptor(pointer(saved), length(saved)), 'saved');
    Check(SecurityDescriptorToText(saved, u), 'sdtt2');
    CheckEqual(u, SD_TXT[i]);
    {$ifdef OSWINDOWS}
    Check(CryptoApi.SecurityDescriptorToText(pointer(saved), u), 'winapi3');
    CheckEqual(u, SD_TXT[i], 'winapi4');
    {$endif OSWINDOWS}
    if i in [1, 2, 8] then
      // serialization offsets are not consistent between XP or later
      Check(saved = bin, 'ToBinary');
    // TSecurityDescriptor load from SDDL into another instance
    Check(sd2.FromText('') = atpMissingExpression);
    CheckEqual(sd2.ToText, '', 'fromnil');
    Check(not sd.IsEqual(sd2));
    Check(sd2.FromText(u) = atpSuccess, 'fromu');
    CheckEqual(sd2.ToText, u, 'fromutou');
    Check(sd.IsEqual(sd2));
    Check(sd2.IsEqual(sd));
    bin := sd2.ToBinary;
    Check(bin = saved, 'saved2');
    // TSecurityDescriptor JSON
    json := SecurityDescriptorToJson(sd);
    Check(IsValidJson(json), 'savejson');
    sd2.Clear;
    Check(not sd.IsEqual(sd2));
    Check(SecurityDescriptorFromJson(json, sd2), 'loadjson');
    Check(sd.IsEqual(sd2));
    Check(scSelfRelative in sd2.Flags);
    // TSecurityDescriptor.Add and Delete high-level methods
    Check(scDaclPresent in sd.Flags);
    if sd.Sacl <> nil then
    begin
      Check(scSaclPresent in sd.Flags);
      continue;
    end;
    Check(not (scSaclPresent in sd.Flags));
    Check(sd.Add('') = nil);
    Check(sd.IsEqual(sd2));
    Check(sd.Add('(toto;;;;)') = nil);
    Check(sd.Add('(D;;;;;SY)') = nil);
    Check(sd.Add('(A;;;;;SY)') = nil);
    Check(sd.IsEqual(sd2));
    Check(sd.Add('(A;;KA;;;SY)') <> nil);
    Check(not sd.IsEqual(sd2));
    CheckEqual(sd.ToText, u + '(A;;KA;;;SY)');
    Check(sd.Add(satCallbackAudit, 'AU', 'KR') <> nil);
    CheckEqual(sd.ToText, u + '(A;;KA;;;SY)(XU;;KR;;;AU)');
    sd.Delete(100);
    sd.Delete(length(sd.Dacl) - 2);
    CheckEqual(sd.ToText, u + '(XU;;KR;;;AU)');
    Check(not sd.IsEqual(sd2));
    sd.Delete(length(sd.Dacl) - 1);
    CheckEqual(sd.ToText, u);
    Check(sd.IsEqual(sd2));
    Check(sd.ToBinary = saved);
    Check(scDaclPresent in sd.Flags);
    for j := 1 to length(sd.Dacl) do
      sd.Delete(0);
    Check(sd.Dacl = nil);
    Check(not (scDaclPresent in sd.Flags));
    Check(sd.Add('(A;;KA;;;SY)') <> nil);
    Check(PosEx('(A;;KA;;;SY)', sd.ToText) <> 0);
    Check(scDaclPresent in sd.Flags);
  end;
  // validate parsing RID in text (e.g. DU,DA)
  atp := sd.FromText(RID_TXT[4]);
  Check(atp = atpInvalidOwner, 'dom0');
  Check(not sd.IsEqual(sd2));
  atp := sd.FromText(' O: DU G: DA D: ( A ; ; FA ; ; ; DA ) ', dom);
  Check(atp = atpSuccess, 'dom1');
  Check(not sd.IsEqual(sd2));
  u := sd.ToText;
  CheckEqual(u, FormatUtf8('O:%-513G:%-512D:(A;;FA;;;%-512)', [dom, dom, dom]));
  CheckEqual(u, SD_TXT[4], 'domasref');
  u := sd.ToText(dom);
  CheckEqual(u, RID_TXT[4], 'rid');
  saved := sd.ToBinary;
  CheckHash(saved, $F1B78A68, 'dombin');
  Check(IsValidSecurityDescriptor(pointer(saved), length(saved)), 'saveddom');
  Check(TryDomainTextToSid(dom, domsid));
  CheckEqual(sd.Dacl[0].SidText, dom + '-512');
  CheckEqual(sd.Dacl[0].SidText(pointer(domsid)), 'DA');
  CheckEqual(sd.Dacl[0].MaskText, 'FA');
  Check(sd.Dacl[0].SidParse('DU', pointer(domsid)));
  CheckEqual(sd.Dacl[0].SidText, dom + '-513');
  CheckEqual(sd.Dacl[0].SidText(pointer(domsid)), 'DU');
  dom2 := 'S-1-5-21-237846769-6124905683-148753929';
  Check(sd.FromBinary(saved));
  Check(sd2.FromBinary(saved));
  Check(sd.IsEqual(sd2));
  u := sd.ToText(dom);
  CheckEqual(u, RID_TXT[4], 'domsaved');
  Check(sd.Modified = []);
  CheckEqual(sd.ReplaceDomain(dom, dom2), 3);
  Check(sd.Modified = [sdiOwner, sdiGroup, sdiDacl]);
  sd.Modified := [];
  Check(not sd.IsEqual(sd2));
  u := sd.ToText;
  CheckNotEqual(u, SD_TXT[4], 'dom2a');
  u2 := sd.ToText(dom);
  CheckEqual(u, u2, 'dom2b');
  u := sd.ToText(dom2);
  CheckEqual(u, RID_TXT[4], 'dom2c');
  sd.Modified := [];
  CheckEqual(sd.ReplaceDomain(dom, dom2), 0);
  Check(sd.Modified = []);
  u := sd.ToText(dom);
  CheckEqual(u, u2, 'dom2d');
  u := sd.ToText(dom2);
  CheckEqual(u, RID_TXT[4], 'dom2e');
  Check(not sd.IsEqual(sd2));
  CheckEqual(sd.ReplaceDomain(dom2, dom), 3);
  Check(sd.Modified = [sdiOwner, sdiGroup, sdiDacl]);
  u := sd.ToText(dom);
  CheckNotEqual(u, SD_TXT[4], 'dom2f');
  CheckEqual(u, RID_TXT[4], 'dom2g');
  Check(sd.IsEqual(sd2), 'dom2h');
  sd.Modified := [];
  CheckEqual(sd.ReplaceDomain(dom2, dom), 0);
  Check(sd.Modified = []);
  Check(sd.IsEqual(sd2), 'dom2i');
  CheckEqual(sd.ReplaceSid(dom + '-512', dom + '-500'), 2, 'dom3a');
  Check(sd.Modified = [sdiGroup, sdiDacl]);
  u := sd.ToText(dom);
  CheckEqual(u, 'O:DUG:LAD:(A;;FA;;;LA)');
  Check(not sd.IsEqual(sd2), 'dom3c');
  sd.Modified := [];
  CheckEqual(sd.ReplaceSid(dom + '-501', dom + '-512'), 0, 'dom3d1');
  Check(sd.Modified = []);
  CheckEqual(sd.ReplaceSid(dom + '-500', dom + '-512'), 2, 'dom3d2');
  Check(sd.Modified = [sdiGroup, sdiDacl]);
  u := sd.ToText(dom);
  CheckEqual(u, RID_TXT[4]);
  Check(sd.IsEqual(sd2), 'dom3f');
  // RID reference material with several domains
  for i := low(DOM_TXT) to high(DOM_TXT) do
  begin
    atp := sd.FromText(SD_TXT[i]);
    Check(atp = atpSuccess);
    u := sd.ToText(DOM_TXT[i]);
    CheckEqual(u, RID_TXT[i]);
    Check(TryDomainTextToSid(DOM_TXT[i], domsid));
    p := pointer(u);
    atp := sd2.FromText(p, pointer(domsid));
    Check(atp = atpSuccess);
    Check(sd.IsEqual(sd2));
  end;
  // custom UUID values in SDDL text
  for a := low(a) to high(a) do
  begin
    Check(UuidToKnownAttribute(ATTR_UUID[a]) = a); // O(log(n)) binary search
    u := ATTR_TXT[a];
    CheckUtf8(TextToKnownAttribute(pointer(u), length(u)) = a, u);
  end;
  Check(sd.FromText(SD_TXT[1]) = atpSuccess, 'uuid');
  u := sd.ToText;
  CheckEqual(u, SD_TXT[1]);
  u := sd.ToText('', AppendShortKnownUuid);
  CheckEqual(u, 'O:AOG:SYD:(A;;KA;;;SY)(A;;KA;;;AO)(OA;;CCDC;User;;AO)' +
    '(OA;;CCDC;Group;;AO)(OA;;CCDC;6da8a4ff-0e52-11d0-a286-00aa003049e2;;AO)' +
    '(OA;;CCDC;Print-Queue;;PO)(A;;LCRPRC;;;AU)S:(AU;SAFA;CCDCSWWPSDWDWO;;;WD)');
  atp := sd2.FromText(u);
  Check(atp = atpInvalidUuid, 'uuid1');
  Check(not sd.IsEqual(sd2), 'uuid2');
  Check(sd2.FromText(u, '', @ShortToKnownUuid) = atpSuccess, 'uuid3');
  Check(sd.IsEqual(sd2), 'uuid4');
  // validate conditional ACEs reference binary
  for i := 0 to high(ARTX_HEX) do
  begin
    bin := mormot.core.text.HexToBin(ARTX_HEX[i]);
    Check(bin <> '');
    Check(bintree.FromBinary(bin));
    Check(bintree.Count > 0);
    u := bintree.ToText;
    CheckEqual(u, ARTX_TXT[i], 'artx1');
    atp := sddltree.FromText(u);
    Check(atp = atpSuccess);
    saved := sddltree.ToBinary;
    CheckEqual(length(saved), length(bin), '2binl');
    Check(saved = bin, '2bin');
    Check(bintree.FromBinary(saved), 'artx2');
    Check(bintree.Count > 0);
    u := bintree.ToText;
    CheckEqual(u, ARTX_TXT[i], 'artx3');
  end;
  // validate conditional ACEs
  for i := 0 to high(COND_TXT) do
  begin
    atp := sd.FromText(COND_TXT[i]);
    Check(atp = atpSuccess);
    Check(length(sd.Dacl) in [1, 2]);
    CheckEqual(length(sd.Sacl), 0);
    Check(sd.Dacl[0].AceType = satCallbackAccessAllowed);
    if not CheckFailed(sd.Dacl[0].Opaque <> '') then
    begin
      u := sd.Dacl[0].ConditionalExpression;
      Check(u <> '');
      Check(u[1] = '(');
      Check(u[length(u)] = ')');
    end;
    u := sd.ToText;
    CheckEqual(u, COND_EXP[i]);
    saved := sd.ToBinary;
    Check(saved <> '');
    Check(IsValidSecurityDescriptor(pointer(saved), length(saved)), 'savcond');
    Check(sd2.FromBinary(saved));
    Check(sd.IsEqual(sd2));
    CheckEqual(sd2.ToText, u);
  end;
  dom := 'S-1-5-21-111-222-333';
  u := sd.ToText(dom);
  CheckEqual(u, 'D:(XA;;FR;;;WD;((Member_of{SID(LA),SID(BO)}) && ' +
    '@Device.Bitlocker))(A;ID;FA;;;SY)');
  dom2 := 'S-1-5-21-1111-2222-3333';
  sd.Modified := [];
  CheckEqual(sd.ReplaceDomain(dom, dom2), 1);
  Check(sd.Modified = [sdiDacl]);
  u2 := sd.ToText;
  CheckEqual(u2,
    'D:(XA;;FR;;;WD;((Member_of{SID(S-1-5-21-1111-2222-3333-500),SID(BO)}) && '+
    '@Device.Bitlocker))(A;ID;FA;;;SY)');
  u2 := sd.ToText(dom2);
  CheckEqual(u, u2);
  sd.Modified := [];
  CheckEqual(sd.ReplaceSid('S-1-5-21-1111-2222-3333-500',
    'S-1-5-21-111-222-333-512'), 1);
  Check(sd.Modified = [sdiDacl]);
  u := sd.ToText(dom);
  CheckEqual(u, 'D:(XA;;FR;;;WD;((Member_of{SID(DA),SID(BO)}) && ' +
    '@Device.Bitlocker))(A;ID;FA;;;SY)');
  sd.Modified := [];
  CheckEqual(sd.ReplaceSid('S-1-5-21-1111-2222-3333-500',
    'S-1-5-21-111-222-333-512'), 0);
  Check(sd.Modified = []);
end;

function IPNUSL(const s1, s2: RawUtf8; len: integer): boolean;
begin
  result := IdemPropNameUSameLenNotNull(pointer(s1), pointer(s2), len);
end;

procedure TTestCoreBase._IdemPropName;
const
  abcde: PUtf8Char = 'ABcdE';
  abcdf: PUtf8Char = 'abCDF';
  zbcde: PUtf8Char = 'zBcdE';
  edf: PUtf8Char = '$a_bc[0]edfghij';
  eda: PUtf8Char = '$a_bc[0]"edfghij';
var
  WinAnsi: WinAnsiString;
  i: integer;
begin
  Check(IdemPropName('a', 'A'));
  Check(not IdemPropName('a', 'z'));
  Check(IdemPropName('ab', 'AB'));
  Check(IdemPropName('abc', 'ABc'));
  Check(IdemPropName('abcD', 'ABcd'));
  Check(not IdemPropName('abcD', 'ABcF'));
  Check(not IdemPropName('abcD', 'ABcFG'));
  Check(not IdemPropName('abcDe', 'ABcFG'));
  Check(IdemPropName('abcDe', 'ABcdE'));
  Check(not IdemPropName('abcDef', 'ABcdEe'));
  Check(IdemPropName('abcDeF', 'ABcdEF'));
  Check(IdemPropName('ABCDEF', 'ABCDEF'));
  Check(not IdemPropName('abcD', ''));
  Check(not IdemPropName('', 'ABcFG'));
  Check(IdemPropName('', ''));
  Check(IdemPropNameU('a', 'A'));
  Check(not IdemPropNameU('a', 'z'));
  Check(IdemPropNameU('ab', 'AB'));
  Check(not IdemPropNameU('abc', 'ABz'));
  Check(not IdemPropNameU('zbc', 'abc'));
  Check(IdemPropNameU('abc', 'ABc'));
  Check(IdemPropNameU('abcD', 'ABcd'));
  Check(not IdemPropNameU('abcD', 'ABcF'));
  Check(not IdemPropNameU('abcD', 'ABcFG'));
  Check(not IdemPropNameU('abcDe', 'ABcFG'));
  Check(IdemPropNameU('abcDe', 'ABcdE'));
  Check(not IdemPropNameU('abcDef', 'ABcdEe'));
  Check(IdemPropNameU('abcDeF', 'ABcdEF'));
  Check(IdemPropNameU('ABCDEF', 'ABCDEF'));
  Check(not IdemPropNameU('abcD', ''));
  Check(not IdemPropNameU('', 'ABcFG'));
  Check(not IdemPropNameU('ABcFG', ''));
  Check(IdemPropNameU('', ''));
  for i := 0 to 100 do
    Check(IdemPropNameU(RawUtf8OfChar('a', i), RawUtf8OfChar('A', i)));
  CheckEqual(UpperCaseU('abcd'), 'ABCD');
  Check(IdemPropNameU('abcDe', abcde, 5));
  Check(not IdemPropNameU('abcD', abcde, 5));
  Check(not IdemPropNameU('abcDF', abcde, 5));
  Check(IdemPropName(abcde, abcde, 4, 4));
  Check(IdemPropName(abcde, abcde, 5, 5));
  Check(not IdemPropName(abcde, abcde, 4, 5));
  Check(not IdemPropName(abcde, abcdf, 5, 5));
  Check(IdemPropName(abcde, nil, 0, 0));
  Check(not IdemPropName(abcde, nil, 1, 0));
  Check(not IPNUSL('abcD', 'ABcF', 4));
  Check(not IPNUSL('abcD', 'ABcFG', 4));
  Check(IPNUSL('abcDe', 'ABcdE', 5));
  Check(IPNUSL('ABcdE', 'abCDF', 1));
  Check(IPNUSL('ABcdE', 'abCDF', 2));
  Check(IPNUSL('ABcdE', 'abCDF', 3));
  Check(IPNUSL('ABcdE', 'abCDF', 4));
  Check(not IPNUSL('ABcdE', 'abCDF', 8));
  Check(IdemPropNameUSameLenNotNull(abcde, abcdf, 1));
  Check(IdemPropNameUSameLenNotNull(abcde, abcdf, 2));
  Check(IdemPropNameUSameLenNotNull(abcde, abcdf, 3));
  Check(IdemPropNameUSameLenNotNull(abcde, abcdf, 4));
  Check(not IdemPropNameUSameLenNotNull(abcde, abcdf, 5));
  Check(not IdemPropNameUSameLenNotNull(abcde, zbcde, 1));
  Check(not IdemPropNameUSameLenNotNull(abcde, zbcde, 2));
  Check(not IdemPropNameUSameLenNotNull(abcde, zbcde, 3));
  Check(not IdemPropNameUSameLenNotNull(abcde, zbcde, 4));
  Check(not IdemPropNameUSameLenNotNull(abcde, zbcde, 5));
  Check(FindRawUtf8(['a', 'bb', 'cc'], 'a') = 0);
  Check(FindRawUtf8(['a', 'bb', 'cc'], 'cc') = 2);
  Check(FindRawUtf8(['a', 'bb', 'cc'], 'ab') = -1);
  Check(FindRawUtf8(['a', 'bb', 'cc'], 'A') = -1);
  Check(FindRawUtf8(['a', 'bb', 'cc'], 'A', false) = 0);
  Check(FindPropName(['a', 'bb', 'cc'], 'A') = 0);
  Check(FindPropName(['a', 'bb', 'cc'], 'cC') = 2);
  Check(FindPropName(['a', 'bb', 'cc'], 'ab') = -1);
  WinAnsi := 'aecD';
  WinAnsi[2] := #$E9;
  WinAnsi[3] := #$E7;
  Check(UpperCaseU(WinAnsiToUtf8(WinAnsi)) = 'AECD');
  check(not JsonPropNameValid(nil));
  check(not JsonPropNameValid(@edf[15]));
  for i := 14 downto 0 do
    check(JsonPropNameValid(@edf[i]) <> (i in [5, 7]));
  for i := 15 downto 0 do
    check(JsonPropNameValid(@eda[i]) = (i > 8));
  Check(PosChar('ABC', 'z') = nil);
  Check(PosChar(nil, 'A') = nil);
  Check(PosChar('ABC', 'A')^ = 'A');
  Check(PosChar('ABC', 'B')^ = 'B');
  Check(PosChar('ABC', 'C')^ = 'C');
  Check(PosChar('ABC', 'a') = nil);
  Check(PosChar('ABC', #0) = nil);
  Check(PosCharAny('ABC', 'z') = nil);
  Check(PosCharAny('ABC', 'A')^ = 'A');
  Check(PosCharAny('ABC', 'B')^ = 'B');
  Check(PosCharAny('ABC', 'C')^ = 'C');
  Check(PosCharAny('ABC', 'az') = nil);
  Check(PosCharAny('ABC', 'aA')^ = 'A');
  Check(PosCharAny('ABC', 'bB')^ = 'B');
  Check(PosCharAny('ABC', 'cC')^ = 'C');
  Check(PosExChar('z', '') = 0, 'ABC');
  Check(PosExChar('z', 'A') = 0, 'ABC');
  Check(PosExChar('z', 'ABC') = 0, 'ABC');
  Check(PosExChar('A', 'A') = 1, 'ABC');
  Check(PosExChar('A', 'AB') = 1, 'ABC');
  Check(PosExChar('A', 'ABC') = 1, 'ABC');
  Check(PosExChar('B', 'ABC') = 2, 'ABC');
  Check(PosExChar('B', 'AB') = 2, 'ABC');
  Check(PosExChar('C', 'ABC') = 3, 'ABC');
end;

procedure TTestCoreBase._TSynCache;
var
  C: TSynCache;
  s, v: RawUtf8;
  i: integer;
  Tag: PtrInt;
begin
  C := TSynCache.Create;
  try
    Check(not C.Reset);
    for i := 0 to 100 do
    begin
      v := Int32ToUtf8(i);
      Tag := 0;
      s := C.Find(v, @Tag);
      Check(s = '');
      Check(Tag = 0);
      C.AddOrUpdate(v, v + v, i);
    end;
    CheckEqual(c.Count, 101);
    for i := 0 to 100 do
    begin
      v := Int32ToUtf8(i);
      Check(C.Find(v, @Tag) = v + v);
      Check(Tag = i);
    end;
    Check(C.Reset);
    CheckEqual(c.Count, 0);
  finally
    C.Free;
  end;
end;

procedure TTestCoreBase._TSynFilter;
type
  TFilterProcess = function(const Value: RawUtf8): RawUtf8;

  procedure Test(Filter: TSynFilterClass; Proc: TFilterProcess);
  var
    V, Old, New: RawUtf8;
    i: integer;
  begin
    with Filter.Create do
    try
      for i := 0 to 200 do
      begin
        V := RandomUtf8(i);
        Old := V;
        New := Proc(Old);
        Process(0, V);
        CheckEqual(V, New);
        V := Old;
        Filter.Execute('', V);
        CheckEqual(V, New);
      end;
    finally
      Free;
    end;
  end;

begin
  Test(TSynFilterTrim, TrimU);
  Test(TSynFilterLowerCase, LowerCase);
  Test(TSynFilterUpperCase, UpperCase);
  Test(TSynFilterLowerCaseU, LowerCaseU);
  Test(TSynFilterUpperCaseU, UpperCaseU);
end;

procedure TTestCoreBase._TSynValidate;

  procedure TestValidateLength(const Params: RawUtf8; aMin, aMax: cardinal);
  var
    i: cardinal;
    V: RawUtf8;
    Msg: string;
    ok: boolean;
    valid: TSynValidateText;
  begin
    valid := TSynValidateText.Create(Params);
    try
      Check(valid.MinLength = aMin);
      Check(valid.MaxLength = aMax);
      for i := 0 to 100 do
      begin
        V := RandomUtf8(i);
        CheckEqual(Utf8ToUnicodeLength(pointer(V)), i, 'Unicode glyph=Ansi char=i');
        Msg := '';
        ok := (i >= aMin) and
              (i <= aMax);
        Check(valid.Process(0, V, Msg) = ok, Msg);
        Check((Msg = '') = ok, Msg);
        Msg := TSynValidateText.Execute(Params, V);
        Check((Msg = '') = ok, Msg);
      end;
    finally
      valid.Free;
    end;
  end;

var
  Msg: string;
begin
  with TSynValidateIPAddress.Create do
  try
    Check(Process(0, '192.168.1.1', Msg));
    Check(Msg = '');
    Msg := '';
    Check(not Process(0, ' 192.168.1.1', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, '292.168.1.1', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(Process(0, '192.168.001.001', Msg));
    Check(Msg = '');
  finally
    Free;
  end;
  with TSynValidateEmail.Create do
  try
    Msg := '';
    Check(Process(0, 'test@synopse.info', Msg));
    Check(Msg = '');
    Msg := '';
    Check(not Process(0, 'test@ synopse.info', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'test@synopse.delphi', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(Process(0, 'test_two@blog.synopse.info', Msg));
    Check(Msg = '');
    Msg := '';
    Check(Process(0, 'test_two@blog.synopse.fr', Msg));
    Check(Msg = '');
  finally
    Free;
  end;
  with TSynValidateEmail.Create('{"ForbiddenDomains":"google.fr,synopse.info"}') do
  try
    Msg := '';
    Check(Process(0, 'test@blog.synopse.fr', Msg));
    Check(Process(0, 'test@blog.synopse.info', Msg));
    Check(not Process(0, 'test@synopse.info', Msg));
    Msg := '';
    Check(Process(0, 'test@blog.google.fr', Msg));
    Check(not Process(0, 'test@google.fr', Msg));
  finally
    Free;
  end;
  with TSynValidateEmail.Create('{"AllowedTLD":"com,org,net","ForbiddenTLD":"net"}') do
  try
    Msg := '';
    Check(Process(0, 'test@synopse.com', Msg));
    Check(Msg = '');
    Msg := '';
    Check(not Process(0, 'test@ synopse.com', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'test@synopse.info', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'test_two@blog.synopse.net', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'test_two@blog.synopse.fr', Msg));
    Check(Msg <> '');
  finally
    Free;
  end;
  with TSynValidatePattern.Create('this [e-n]s a [!zy]est') do
  try
    Msg := '';
    Check(Process(0, 'this is a test', Msg));
    Check(Msg = '');
    Msg := '';
    Check(Process(0, 'this is a rest', Msg));
    Check(Msg = '');
    Msg := '';
    Check(not Process(0, 'this is a zest', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'this as a test', Msg));
    Check(Msg <> '');
    Msg := '';
    Check(not Process(0, 'this as a rest', Msg));
    Check(Msg <> '');
  finally
    Free;
  end;
  TestValidateLength('', 1, maxInt);
  TestValidateLength('{"mAXlength": 10 , "MInLENgtH" : 3 }', 3, 10);
  with TSynValidateText.Create do
  try
    Msg := '';
    MaxLeftTrimCount := 0;
    Check(Process(0, 'one', Msg));
    Check(not Process(0, ' one', Msg));
    MaxRightTrimCount := 0;
    Check(Process(0, 'one', Msg));
    Check(not Process(0, ' one', Msg));
    Check(not Process(0, 'one ', Msg));
    Msg := '';
    MinAlphaCount := 3;
    Check(Process(0, 'one', Msg));
    Check(not Process(0, 'on2', Msg));
    Msg := '';
    MinDigitCount := 2;
    Check(Process(0, 'one12', Msg));
    Check(not Process(0, 'one2', Msg));
    Msg := '';
    MinPunctCount := 1;
    Check(Process(0, 'one12_', Msg));
    Check(Process(0, '_one12_', Msg));
    Check(Process(0, '_one12', Msg));
    Check(not Process(0, 'one12', Msg));
    Msg := '';
    MinLowerCount := 3;
    Check(Process(0, 'o12_ne', Msg));
    Check(not Process(0, 'o12_An', Msg));
    Msg := '';
    MinUpperCount := 3;
    Check(Process(0, 'o12_neABC', Msg));
    Check(not Process(0, 'o12_AnBc', Msg));
    Msg := '';
    MinSpaceCount := 3;
    Check(Process(0, 'o12 _ne AB C', Msg));
    Check(not Process(0, 'O1 2_A neeB', Msg));
    Msg := '';
    MaxSpaceCount := 3;
    Check(Process(0, 'o12 _ne AB C', Msg));
    Check(not Process(0, 'o12 _ ne AB C', Msg));
  finally
    Free;
  end;
  with TSynValidatePassword.Create do
  try
    Msg := '';
    Check(Process(0, 'aA3!Z', Msg));
    Check(not Process(0, 'aA3!', Msg));
    Msg := '';
    Check(not Process(0, 'aA 3!Z', Msg));
  finally
    Free;
  end;
end;

procedure TTestCoreBase.UrlDecoding;
var
  i, V: integer;
  c: cardinal;
  v64: Int64;
  s, t, d: RawUtf8;
  U: PUtf8Char;
begin
  for i := 1 to 100 do
  begin
    s := DateTimeToIso8601(Now / 20 + rnd.NextDouble * 20, true);
    t := UrlEncode(s);
    CheckEqual(UrlDecode(t), s);
    d := 'seleCT=' + t + '&where=' + Int32ToUtf8(i);
    Check(UrlDecodeNeedParameters(pointer(d), 'where,select'));
    Check(not UrlDecodeNeedParameters(pointer(d), 'foo,select'));
    Check(UrlDecodeValue(pointer(d), 'SELECT=', t, @U));
    CheckEqual(t, s, 'UrlDecodeValue');
    Check(IdemPChar(U, 'WHERE='), 'Where');
    Check(UrlDecodeInteger(U, 'WHERE=', V));
    CheckEqual(V, i);
    Check(UrlDecodeInt64(U, 'WHERE=', v64));
    CheckEqual(v64, i);
    Check(UrlDecodeCardinal(U, 'WHERE=', c));
    CheckEqual(c, i);
    Check(not UrlDecodeValue(pointer(d), 'NOTFOUND=', t, @U));
    Check(UrlDecodeInteger(U, 'WHERE=', V, @U));
    Check(U = nil);
  end;
  s := '{"b":30,"a":"toto"}'; // temp read-only var for proper overload call
  CheckEqual(UrlEncodeJsonObject('', s, []), '?b=30&a=toto');
  {$ifdef OSWINDOWS}
  Check(GetFileNameFromUrl('file:///c:/temp/toto.text') = 'c:\temp\toto.text', 't1');
  Check(GetFileNameFromUrl('file:///c:\temp\toto.text') = 'c:\temp\toto.text', 't2');
  Check(GetFileNameFromUrl(
    'file://someserver/temp/toto.text') = '\\someserver\temp\toto.text', 't3');
  {$else}
  Check(GetFileNameFromUrl('file://someserver/temp/toto.text') = '', 'posix1');
  Check(GetFileNameFromUrl('file:///temp/toto.text') = '/temp/toto.text', 'posix2');
  {$endif OSWINDOWS}
end;

procedure TTestCoreBase.MimeTypes;
const
  MIM: array[0 .. 27 * 2 - 1] of RawUtf8 = (
    'png',      'image/png',
    'PNg',      'image/png',
    'gif',      'image/gif',
    'tif',      'image/tiff',
    'tiff',     'image/tiff',
    'heic',     'image/heic',
    'jpg',      'image/jpeg',
    'JPG',      'image/jpeg',
    'jpeg',     'image/jpeg',
    'bmp',      'image/bmp',
    'doc',      'application/msword',
    'docx',     'application/msword',
    'htm',      HTML_CONTENT_TYPE,
    'html',     HTML_CONTENT_TYPE,
    'HTML',     HTML_CONTENT_TYPE,
    'css',      'text/css',
    'js',       'text/javascript',
    'ico',      'image/x-icon',
    'pdf',      'application/pdf',
    'PDF',      'application/pdf',
    'Json',     JSON_CONTENT_TYPE,
    'webp',     'image/webp',
    'manifest', 'text/cache-manifest',
    'appcache', 'text/cache-manifest',
    'h264',     'video/H264',
    'x',        'application/x-compress',
    'ogv',      'video/ogg');
  BIN: array[0 .. 2] of Cardinal = (
    $04034B50, $38464947, $fd2fb528);
  BIN_MIME: array[0 .. high(BIN)] of RawUtf8 = (
    'application/zip', 'image/gif', 'application/zstd');
  HEX: array[0 .. 5] of RawUtf8 = (
    '000000186674797069736F6D0000000069736F6D6D703432',
    '000000186674797068656963000000006D69663168656963',
    '0000001C66747970663476200000000069736F6D6D70343266347620',
    '000000206674797061766966000000006D696631617669666D696166',
    '4F67675300020000000000000000123456780000000012345678011E01766F7262697' +
      '3000000000244AC0000000000000000000000000000060F',
    '4F67675300020000000000000000ABCDEF120000000087654321012A807468656F726' +
      '10302000050004001E001200000003C00000001000000');
  HEX_MIME: array[0 .. high(HEX)] of RawUtf8 = (
    'video/mp4', 'image/heic', 'video/H264', 'image/avif',
    'audio/ogg', 'video/ogg');
var
  i, j, n: integer;
  fa: TFileAge;
  fdt: TDateTime;
  fs: Int64;
  fu: TUnixMSTime;
  fn: array[0..10] of TFileName;
  mp, mp2: TMultiPartDynArray;
  s, ct, mpc, mpct: RawUtf8;
  st: THttpMultiPartStream;
  rfc2388: boolean;

  procedure DecodeAndTest;
  var
    i: integer;
  begin
    mp2 := nil;
    Check(MultiPartFormDataDecode(mpct, mpc, mp2));
    CheckEqual(length(mp2), length(mp));
    for i := 0 to high(mp2) do
      if i <= n then
      begin
        CheckEqual(mp2[i].Name,    MIM[i * 2]);
        CheckEqual(mp2[i].Content, MIM[i * 2 + 1]);
      end
      else
      begin
        j := i - n - 1;
        CheckEqual(mp2[i].FileName, StringToUtf8(ExtractFileName(fn[j])));
        CheckEqual(mp2[i].Content,  MIM[j * 2 + 1]);
      end;
  end;

begin
  // user agent bot detection
  Check(not IsHttpUserAgentBot(
    'Mozilla/5.0 (Linux; Android 10; K) AppleWebKit/537.36 (KHTML, like ' +
    'Gecko) Chrome/120.0.6099.230 Mobile Safari/537.36'));
  Check(not IsHttpUserAgentBot(
    'Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/115.0'));
  Check(not IsHttpUserAgentBot(
    'Mozilla/5.0 (iPhone; CPU iPhone OS 17_2_1 like Mac OS X) AppleWebKit/' +
    '605.1.15 (KHTML, like Gecko) Version/17.2 Mobile/15E148 Safari/604.1'));
  Check(not IsHttpUserAgentBot(
    'Mozilla/5.0 (Windows NT 10.0; Trident/7.0; rv:11.0) like Gecko'));
  Check(not IsHttpUserAgentBot(DefaultUserAgent(self)),
    'Mozilla/5.0 (Linux x64; mORMot) TCB/3 mormot2tests');
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; ' +
    'Amazonbot/0.1; +https://developer.amazon.com/support/amazonbot) ' +
    'Chrome/119.0.6045.214 Safari/537.36'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; compatible; ' +
    'Amazonbot/0.1; +https://developer.amazon.com/support/amazonbot)'));
  Check(IsHttpUserAgentBot(
    'Amazonbot/0.1; +https://developer.amazon.com/support/amazonbot/)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)'));
  Check(IsHttpUserAgentBot(
    'Googlebot/2.1 (+http://www.google.org/bot.html)'));
  Check(IsHttpUserAgentBot(
    'Y!J-BRW/1.0 crawler (http://help.yahoo.co.jp/help/jp/search/indexing/indexing-15.html'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; adidxbot/2.0;  http://www.bing.com/bingbot.htm)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; Yahoo! Slurp; http://help.yahoo.com/help/us/ysearch/slurp)'));
  Check(IsHttpUserAgentBot(
    'adidxbot/1.1 (+http://search.msn.com/msnbot.htm)'));
  Check(IsHttpUserAgentBot(
    'Speedy Spider (http://www.entireweb.com/about/search_tech/speedy_spider/'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; Baiduspider/2.0; +http://www.baidu.com/search/spider.html)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; coccoc:1.0; +http://help.coccoc.com/searchengine)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (Linux; Android 5.0; SM-G920A) AppleWebKit (KHTML, like Gecko) ' +
    'Chrome Mobile Safari (compatible; AdsBot-Google-Mobile; +http://www.google.com/mobile/adsbot.html)'));
  Check(IsHttpUserAgentBot(
    'DuckDuckBot/1.0; (+http://duckduckgo.com/duckduckbot.html)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; Applebot/0.3; +http://www.apple.com/go/applebot'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; AhrefsBot/6.1; +http://ahrefs.com/robot/)'));
  Check(IsHttpUserAgentBot(
    'serpstatbot/1.0 (advanced backlink tracking bot; http://serpstatbot.com/;'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; TinEye-bot/1.31; +http://www.tineye.com/crawler.html)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; Yeti/1.1; +http://naver.me/bot)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; AhrefsBot/7.0; +http://ahrefs.com/robot/)'));
  Check(IsHttpUserAgentBot(
    'LinkedInBot/1.0 (compatible; Mozilla/5.0; Apache-HttpClient +http://www.linkedin.com)'));
  Check(IsHttpUserAgentBot(
    'Twitterbot/1.0 Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) '));
  Check(IsHttpUserAgentBot(
    'Wget/1.14 (linux-gnu)'));
  Check(IsHttpUserAgentBot(
    'CCBot/2.0 (https://commoncrawl.org/faq/'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; MegaIndex.ru/2.0; +http://megaindex.com/crawler)'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 (compatible; Exabot-Images/3.0; +http://www.exabot.com/go/robot'));
  Check(IsHttpUserAgentBot(
    'Mozilla/5.0 AppleWebKit/537.36 (KHTML, like Gecko; GPTBot/1.0; +https://openai.com/gptbot'));
  Check(IsHttpUserAgentBot(
    'Feedfetcher-Google; (+http://www.google.com/feedfetcher.html; 1 subscribers; feed-id=728742641706423)'));
  Check(IsHttpUserAgentBot(
    'Python-urllib/3.4'));
  // some HTTP headers processing methods
  Check(not IsInvalidHttpHeader(''));
  Check(not IsInvalidHttpHeader('a=b'));
  Check(not IsInvalidHttpHeader('a'#13#10));
  Check(not IsInvalidHttpHeader('a'#13#10'b'#13#10));
  Check(not IsInvalidHttpHeader('a'#13#10'b'));
  Check(IsInvalidHttpHeader(#13#10'a'#13#10));
  Check(IsInvalidHttpHeader(#10'a'#13#10));
  Check(IsInvalidHttpHeader(#13#10#13#10'a'#13#10));
  Check(IsInvalidHttpHeader('a'#13#10'b'#10));
  Check(IsInvalidHttpHeader('a'#10'b'#13#10));
  Check(IsInvalidHttpHeader('a'#13#10#13#10'b'#13#10));
  Check(IsInvalidHttpHeader('a'#13#10'b'#13#10#13#10));
  Check(IsInvalidHttpHeader('a'#13#10#13'b'#13#10));
  Check(IsInvalidHttpHeader('a'#13#13'b'#13#10));
  Check(IsInvalidHttpHeader('a'#13#10'b'#13#13));
  Check(IsInvalidHttpHeader('a'#13#10'b'#13));
  Check(IsInvalidHttpHeader('a'#13#10'b'#10));
  s := 'toto'#13#10;
  Check(not IsInvalidHttpHeader(s));
  CheckEqual(PurgeHeaders(''), '');
  CheckEqual(PurgeHeaders('toto'), 'toto');
  CheckEqual(PurgeHeaders(#13#10), #13#10);
  CheckEqual(PurgeHeaders('toto'#13#10), 'toto'#13#10);
  CheckEqual(PurgeHeaders(#13#10, true), '');
  CheckEqual(PurgeHeaders('toto'#13#10, true), 'toto');
  CheckEqual(PurgeHeaders('', true), '');
  CheckEqual(PurgeHeaders('toto', true), 'toto');
  CheckEqual(PurgeHeaders('content-length: 10'#13#10'toto'#13#10, true), 'toto');
  CheckEqual(PurgeHeaders('toto'#13#10'content-length: 10'#13#10, true), 'toto');
  CheckEqual(PurgeHeaders(s), s);
  CheckEqual(PurgeHeaders('content-length: 10'#13#10'toto'#13#10), s);
  CheckEqual(PurgeHeaders('toto'#13#10'content-length: 10'#13#10), s);
  CheckEqual(PurgeHeaders(
    'accept: all'#13#10'toto'#13#10'content-length: 10'#13#10), s);
  CheckEqual(PurgeHeaders(
    'accept: all'#13#10'content-length: 10'#13#10'toto'#13#10), s);
  CheckEqual(DeleteHeader(s, ''), s);
  CheckEqual(DeleteHeader('content-length: 10'#13#10'toto'#13#10, 'content-length'), s);
  CheckEqual(DeleteHeader('toto'#13#10'content-length: 10'#13#10, 'Content-Length'), s);
  CheckEqual(DeleteHeader(Join([s, s, s, 'Auth: 1']), 'auth'), Join([s, s, s]));
  CheckEqual(DeleteHeader(Join(['Auth: 0'#13#10, s, s, s]), 'auth'), Join([s, s, s]));
  CheckEqual(DeleteHeader(Join(['Auth: 0'#13#10, s, s, s, 'Auth: 1']), 'auth'), Join([s, s, s]));
  CheckEqual(DeleteHeader(Join([s, s, s, 'Auth: 2'#13#10, s]), 'auth'), Join([s, s, s, s]));
  CheckEqual(DeleteHeader(Join([s, s, s, 'Auth: 2'#13#10, 'ab']), 'auth'), Join([s, s, s, 'ab']));
  // some HTTP methods
  Check(HttpMethodWithNoBody('HEAD'));
  Check(HttpMethodWithNoBody('head'));
  Check(HttpMethodWithNoBody('HEADER'));
  Check(HttpMethodWithNoBody('OPTIONS'));
  Check(HttpMethodWithNoBody('options'));
  Check(HttpMethodWithNoBody('OPTION'));
  Check(HttpMethodWithNoBody('OPTI'));
  Check(not HttpMethodWithNoBody('toto'));
  Check(not HttpMethodWithNoBody('HE4D'));
  Check(not HttpMethodWithNoBody('GET'));
  Check(not HttpMethodWithNoBody('POST'));
  Check(not HttpMethodWithNoBody('PUT'));
  Check(not HttpMethodWithNoBody('OPT'));
  // mime content types
  CheckEqual(GetMimeContentType('', 'toto.h264'), 'video/H264');
  CheckEqual(GetMimeContentType('', 'toto', 'def1'), 'def1');
  CheckEqual(GetMimeContentType('', 'toto.', 'def2'), 'def2');
  CheckEqual(GetMimeContentType('', 'toto.a', 'def3'), 'application/a');
  CheckEqual(GetMimeContentType('', 'toto.1', 'def4'), 'def4');
  CheckEqual(GetMimeContentType('', 'toto.ab', 'def5'), 'application/ab');
  for i := 0 to high(MIM) shr 1 do
    CheckEqual(GetMimeContentType('',
      MakeString(['toto.', MIM[i * 2]])), MIM[i * 2 + 1]);
  FastSetString(s, 63);
  for i := 0 to high(BIN) do
  begin
    PCardinal(s)^ := BIN[i];
    CheckEqual(GetMimeContentType(s), BIN_MIME[i]);
    ct := '';
    Check(GetMimeContentTypeFromBuffer(s, ct) <> mtUnknown);
    CheckEqual(ct, BIN_MIME[i]);
  end;
  for i := 0 to high(HEX) do
  if not CheckFailed(length(HEX[i]) shr 1 < length(s)) then
  begin
    Check(mormot.core.text.HexToBin(pointer(HEX[i]), pointer(s), length(HEX[i]) shr 1));
    CheckEqual(GetMimeContentType(s), HEX_MIME[i]);
    ct := '';
    Check(GetMimeContentTypeFromBuffer(s, ct) <> mtUnknown);
    CheckEqual(ct, HEX_MIME[i]);
  end;
  s := '<?xml';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtXml);
  CheckUtf8(IsContentCompressed(pointer(s), length(s)) = false, s);
  s := '<html><body>';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtHtml);
  CheckUtf8(IsContentCompressed(pointer(s), length(s)) = false, s);
  s := '<!doctype html><html><body>';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtHtml);
  s := '<!doctype note>';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtXml);
  s := '<?XML';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtXml);
  s := '<HTML><body>';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtHtml);
  s := '<!DocType HTML<html><body>';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtHtml);
  s := '{"json":123}';
  CheckUtf8(IsContentCompressed(pointer(s), length(s)) = false, s);
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtJson);
  s := '["json",123]';
  CheckUtf8(IsContentCompressed(pointer(s), length(s)) = false, s);
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtJson);
  s := '["json",'#0'123]';
  Check(GetMimeContentTypeFromMemory(pointer(s), length(s)) = mtUnknown);
  Check(not IsContentTypeCompressibleU('anything'));
  Check(not IsContentTypeCompressibleU('toto/plain'));
  Check(IsContentTypeCompressibleU('text/plain'));
  Check(IsContentTypeCompressibleU('text/xml'));
  Check(IsContentTypeCompressibleU('text/css'));
  Check(not IsContentTypeCompressibleU('texto/xml'));
  Check(IsContentTypeCompressibleU('application/json'));
  Check(IsContentTypeCompressibleU('APPLICATION/JSON'));
  Check(IsContentTypeCompressibleU('application/xml'));
  Check(IsContentTypeCompressibleU('application/rtf'));
  Check(not IsContentTypeCompressibleU('application/octet-stream'));
  Check(not IsContentTypeCompressibleU('application/zrtf'));
  Check(not IsContentTypeCompressibleU('application/xm'));
  Check(IsContentTypeCompressibleU('application/javascript'));
  Check(IsContentTypeCompressibleU('application/VND.API+JSON'));
  Check(IsContentTypeCompressibleU('application/vnd.mysoft.v1+json'));
  Check(IsContentTypeCompressibleU('application/atom+XMl'));
  Check(IsContentTypeCompressibleU('application/office+RTf'));
  Check(not IsContentTypeCompressibleU('application/office+rtl'));
  Check(not IsContentTypeCompressibleU('applications/atom+xml'));
  Check(not IsContentTypeCompressibleU('application/plain'));
  Check(IsContentTypeCompressibleU('image/svg'));
  Check(IsContentTypeCompressibleU('image/X-ico'));
  Check(IsContentTypeCompressibleU('image/X-ICO'));
  Check(not IsContentTypeCompressibleU('image/png'));
  Check(IsContentTypeJsonU('ApplicatioN/JSON'));
  Check(IsContentTypeJsonU('application/json; charset=utf8'));
  Check(IsContentTypeJsonU('application/Json;CharSet=Utf-8'));
  Check(IsContentTypeJsonU('application/VND.API+JSON;CharSet=Utf-8'));
  Check(not IsContentTypeJsonU('application/vnd.mysoft.v1+jso'));
  Check(IsContentTypeJsonU('application/vnd.mysoft.v1+json'));
  Check(not IsContentTypeJsonU('application/vnd.mysoft.v1+j'));
  Check(IsContentTypeJsonU('application/vnd.mysoft.v1+json2'));
  Check(not IsContentTypeJsonU('application/vnd.mysoft.v1+'));
  Check(IsContentTypeJsonU('application/+json'));
  Check(not IsContentTypeJsonU('application/xml'));
  Check(IsContentTypeTextU('text/plain'));
  Check(IsContentTypeTextU('text/xml'));
  Check(IsContentTypeTextU('text/css'));
  Check(not IsContentTypeTextU('texto/xml'));
  Check(IsContentTypeTextU('application/json'));
  Check(IsContentTypeTextU('APPLICATION/JSON'));
  Check(IsContentTypeTextU('application/xml'));
  Check(not IsContentTypeTextU('application/octet-stream'));
  Check(IsContentTypeTextU('application/javascript'));
  Check(IsContentTypeTextU('application/VND.API+JSON'));
  Check(IsContentTypeTextU('application/vnd.mysoft.v1+json'));
  Check(IsContentTypeTextU('application/atom+xml'));
  Check(not IsContentTypeTextU('applications/atom+xml'));
  Check(not IsContentTypeTextU('application/plain'));
  Check(IsContentTypeTextU('image/svg'));
  Check(not IsContentTypeTextU('image/X-ico'));
  Check(not IsContentTypeTextU('image/X-ICO'));
  // mime multipart encoding
  for rfc2388 := false to true do
  begin
    mp := nil;
    mp2 := nil;
    n := high(MIM) shr 1;
    for i := 0 to n do
      Check(MultiPartFormDataAddField(MIM[i * 2], MIM[i * 2 + 1], mp));
    for i := 0 to high(fn) do
    begin
      fn[i] := WorkDir + 'mp' + IntToStr(i);
      s := MIM[i * 2 + 1];
      FileFromString(s, fn[i]);
      Check(MultiPartFormDataAddFile(fn[i], mp));
      fa := sysutils.FileAge(fn[i]);
      fdt := sysutils.FileDateToDateTime(fa);
      CheckSame(fdt, mormot.core.os.FileDateToDateTime(fa), DOUBLE_SAME, 'FileDateToDateTime');
      {$ifdef HASNEWFILEAGE}
      Check(FileAge(fn[i], fdt), 'FileAge');
      {$endif HASNEWFILEAGE}
      CheckSame(fdt, mormot.core.os.FileAgeToDateTime(fn[i]), 0.01, 'fdt');
      // FPC FileAge() is wrong and truncates 1-2 seconds on Windows -> 0.01
      Check(FileInfoByName(fn[i], fs, fu), 'FileInfoByName');
      CheckEqual(fs, length(s), 'FileInfoByName Size');
      CheckEqual(FileAgeToUnixTimeUtc(fn[i]), fu div 1000, 'FileAgeToUnixTimeUtc');
      // writeln('now=',DateTimeToIso8601Text(Now));
      // writeln('utc=',DateTimeToIso8601Text(NowUtc));
      // writeln('fdt=',DateTimeToIso8601Text(fdt));
      // writeln('osl=',DateTimeToIso8601Text(mormot.core.os.FileAgeToDateTime(fn[i])));
      // writeln('osu=',UnixTimeToString(FileAgeToUnixTimeUtc(fn[i])));
      //  now=2022-04-28T13:54:06
      //  utc=2022-04-28T11:54:06
      //  fdt=2022-04-28T13:54:08 -> 2 seconds error from FPC RTL FileAge()
      //  osl=2022-04-28T13:54:06
      //  osu=2022-04-28T11:54:06
    end;
    Check(MultiPartFormDataEncode(mp, mpct, mpc, rfc2388));
    DecodeAndTest;
    st := THttpMultiPartStream.Create;
    st.Rfc2388NestedFiles := rfc2388;
    for i := 0 to n do
      st.AddContent(MIM[i * 2], MIM[i * 2 + 1]);
    for i := 0 to high(fn) do
      st.AddFile('', fn[i]);
    st.Flush;
    mpct := st.MultipartContentType;
    mpc := StreamToRawByteString(st);
    DecodeAndTest;
    st.Free;
    for i := 0 to high(fn) do
      check(DeleteFile(fn[i]));
  end;
end;

function TTestCoreBase.QuickSelectGT(IndexA, IndexB: PtrInt): boolean;
begin
  result := fQuickSelectValues[IndexA] > fQuickSelectValues[IndexB];
end;

procedure TTestCoreBase.QuickSelect;

  function Median(const Csv: RawUtf8; Expected: integer): integer;
  var
    IDA: TIntegerDynArray;
  begin
    CsvToIntegerDynArray(pointer(Csv), IDA);
    result := MedianQuickSelectInteger(pointer(IDA), length(IDA));
    Check(result = Expected);
  end;

var
  n, i, med2, med1, len: integer;
  tmp: TSynTempBuffer;
  P: PIntegerArray;
begin
  Median('', 0);
  Median('2', 2);
  Median('3,5,12', 5);
  Median('12,3,5', 5);
  Median('19,10,84,11,23', 19);
  Median('1,3,3,6,7,8,9', 6);
  Median('1,2,3,4,5,6,8,9', 4);
  Median('3,5,7,12,13,14,21,23,23,23,23,29,39,40,56', 23);
  Median('3,13,7,5,21,23,39,23,40,23,14,12,56,23,29', 23);
  Median('3,5,7,12,13,14,21,23,23,23,23,29,40,56', 21);
  Median('3,13,7,5,21,23,23,40,23,14,12,56,23,29', 21);
  for n := 0 to 1000 do
  begin
    len := n * 2 + 1;
    SetLength(fQuickSelectValues, len);
    P := pointer(fQuickSelectValues);
    FillIncreasing(P, 1, len);
    med1 := MedianQuickSelect(QuickSelectGT, len, tmp);
    Check(fQuickSelectValues[med1] = n + 1);
    Check(MedianQuickSelectInteger(P, len) = n + 1);
    for i := 0 to high(fQuickSelectValues) do
      fQuickSelectValues[i] := rnd.Next31;
    med1 := fQuickSelectValues[MedianQuickSelect(QuickSelectGT, len, tmp)];
    med2 := MedianQuickSelectInteger(P, len);
    Check(med1 = med2);
    QuickSortInteger(P, 0, len - 1);
    check(med2 = fQuickSelectValues[n]);
  end;
end;

procedure TTestCoreBase.Debugging;

  procedure Test(const LOG: RawUtf8; ExpectedDate: TDateTime);
  var
    L: TSynLogFile;
    o: TLogProcSortOrder;
  begin
    L := TSynLogFile.Create(pointer(LOG), length(LOG));
    try
      Check(L.ExecutableName = 'D:\Dev\lib\SQLite3\exe\TestSQL3.exe');
      Check(L.ExecutableVersion = '1.2.3.4');
      if trunc(ExpectedDate) = 40640 then
        Check(L.InstanceName = 'D:\Dev\MyLibrary.dll')
      else
        Check(L.InstanceName = '');
      CheckSame(L.ExecutableDate, ExpectedDate, 1 / SecsPerDay);
      Check(L.ComputerHost = 'MyPC');
      Check(L.LevelUsed = [sllEnter, sllLeave, sllDebug]);
      Check(L.RunningUser = 'MySelf');
      Check(L.CPU = '2*0-15-1027');
      Check(L.OS = wXP);
      Check(L.ServicePack = 3);
      Check(not L.Wow64);
      Check(L.Freq = 0);
      CheckSame(L.StartDateTime, 40640.502882, 1 / SecsPerDay);
      if CheckFailed(L.Count = 3) then
        exit;
      Check(L.EventLevel[0] = sllEnter);
      Check(L.EventLevel[1] = sllDebug);
      CheckSame(L.EventDateTime(1), L.StartDateTime, 1 / SecsPerDay);
      Check(L.EventLevel[2] = sllLeave);
      if CheckFailed(L.LogProcCount = 1) then
        exit;
      CheckEqual(L.LogProc[0].Index, 0);
      CheckEqual(L.LogProc[0].Time, 10020006);
      for o := low(o) to high(o) do
        L.LogProcSort(o);
    finally
      L.Free;
    end;
  end;

var
  tmp: array[0..512] of AnsiChar;
  msg, n, v: RawUtf8;
  os, os2: TOperatingSystem;
  ld: TLinuxDistribution;
  islinux: boolean;
  osv: TOperatingSystemVersion;
  len: integer;
begin
  // validate UserAgentParse()
  Check(not UserAgentParse('toto (mozilla)', n, v, os));
  Check(UserAgentParse('myprogram/3.1.0.2W', n, v, os));
  Check(n = 'myprogram');
  Check(v = '3.1.0.2');
  check(os = osWindows);
  Check(UserAgentParse('mormot2tests/D', n, v, os));
  Check(n = 'mormot2tests');
  Check(v = '');
  check(os = osDebian);
  Check(UserAgentParse('myprogram/3.1.2W32', n, v, os));
  Check(n = 'myprogram');
  Check(v = '3.1.2');
  check(os = osWindows);
  // validate TOperatingSystemVersion
  osv.os := osWindows;
  osv.win := wSeven;
  osv.winbuild := 0;
  CheckEqualShort(ToText(osv), 'Windows 7');
  osv.win := wTen_64;
  CheckEqualShort(ToText(osv), 'Windows 10 64bit');
  osv.winbuild := 10240;
  CheckEqualShort(ToText(osv), 'Windows 10 64bit 1507');
  osv.winbuild := 10241;
  CheckEqualShort(ToText(osv), 'Windows 10 64bit 1507');
  osv.win := wTen;
  osv.winbuild := 19045;
  CheckEqualShort(ToText(osv), 'Windows 10 22H2');
  osv.win := wEleven;
  osv.winbuild := 22000;
  CheckEqualShort(ToText(osv), 'Windows 11 21H2');
  osv.winbuild := 22621;
  CheckEqualShort(ToText(osv), 'Windows 11 22H2');
  osv.win := wEleven_64;
  osv.winbuild := 26100;
  CheckEqualShort(ToText(osv), 'Windows 11 64bit 24H2');
  osv.winbuild := 26100;
  CheckEqualShort(ToTextOS(cardinal(osv)), 'Windows 11 64bit 24H2 26100');
  osv.winbuild := 26210;
  CheckEqualShort(ToTextOS(cardinal(osv)), 'Windows 11 64bit 25H2 26210');
  osv.win := wServer2022_64;
  osv.winbuild := 20349;
  CheckEqualShort(ToText(osv), 'Windows Server 2022 64bit 21H2');
  CheckEqual(ToTextOSU(cardinal(osv)), 'Windows Server 2022 64bit 21H2 20349');
  // validate OS definitions logic
  for os := low(os) to high(os) do
  begin
    islinux := false;
    for ld := succ(low(ld)) to high(ld) do
      if os in LINUX_DIST[ld] then
        if not CheckFailed(not islinux, 'os twice') then
          if not CheckFailed(LinuxDistribution(os) = ld, 'ld') then
            islinux := true;
    Check((os in OS_LINUX) = islinux, 'islinux');
    Check(islinux = not (os in LINUX_DIST[ldNotLinux]));
    Check(islinux = (LinuxDistribution(os) <> ldNotLinux));
    for os2 := low(os) to high(os) do
      Check((OS_INITIAL[os2] = OS_INITIAL[os]) = (os2 = os), 'OS_INITIAL');
  end;
  // validate SyslogMessage()
  FillcharFast(tmp, SizeOf(tmp), 1);
  len := SyslogMessage(sfAuth, ssCrit, 'test', '', '', tmp, SizeOf(tmp), false);
  // Check(len=65); // <-- different for every PC, due to PC name differences
  tmp[len] := #0;
  Check(IdemPChar(PUtf8Char(@tmp), PAnsiChar('<34>1 ')));
  Check(PosEx(' - - - test', tmp) = len - 10);
  msg := RawUtf8OfChar('+', 300);
  len := SyslogMessage(sfLocal4, ssNotice, msg, 'proc', 'msg', tmp, 300, false);
  Check(IdemPChar(PUtf8Char(@tmp), PAnsiChar('<165>1 ')));
  Check(PosEx(' proc msg - ++++', tmp) > 1);
  Check(len < 300, 'truncated to avoid buffer overflow');
  Check(tmp[len - 1] = '+');
  Check(tmp[len] = #1);
  // validate TSynLogFile
  Test('D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-07 11:09:06)'#13#10 +
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545 ' +
    'Instance=D:\Dev\MyLibrary.dll'#13#10 +
    'TSynLog 1.15 LVCL 2011-04-07 12:04:09'#13#10#13#10 +
    '20110407 12040903  +    SQLite3Commons.TRestServer.Uri (14163)'#13#10 +
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",' +
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TDebugFile(00ACC990)"]}'#13#10 +
    '20110407 12040915  -    SQLite3Commons.TRestServer.Uri (14163) 10.020.006',
    40640.464653);
  Test('D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-08 11:09:06)'#13#10 +
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545'#13#10 +
    'TSynLog 1.15 LVCL 2011-04-07 12:04:09'#13#10#13#10 +
    '20110407 12040903  +    SQLite3Commons.TRestServer.Uri (14163)'#13#10 +
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",' +
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TDebugFile(00ACC990)"]}'#13#10 +
    '20110407 12040915  -    SQLite3Commons.TRestServer.Uri (14163) 10.020.006',
    40641.464653);
end;

procedure TTestCoreBase._TSynNameValue;
const
  MAX = 10000;
var
  nv: TSynNameValue;
  i: integer;
  v: RawUtf8;
  tmp: TSynTempBuffer;
begin
  nv.Init(false);
  check(nv.Count = 0);
  for i := 1 to MAX do
    nv.Add(UInt32ToUtf8(i), UInt32ToUtf8(i + MAX));
  check(nv.Count = MAX);
  for i := 1 to MAX do
    checkEqual(nv.Find(UInt32ToUtf8(i)), i - 1);
  for i := MAX + 1 to MAX * 2 do
    check(nv.Find(UInt32ToUtf8(i)) < 0);
  for i := 1 to MAX do
  begin
    UInt32ToUtf8(i + MAX, v);
    checkEqual(nv.Value(UInt32ToUtf8(i)), v);
    checkEqual(nv.Str[UInt32ToUtf8(i)], v);
  end;
  nv.InitFromNamesValues(['a', 'b'], ['1', 'be']);
  checkEqual(nv.Count, 2);
  checkEqual(nv.Str['a'], '1');
  checkEqual(nv.Str['b'], 'be');
  checkEqual(nv.Str['c'], '');
  checkEqual(nv.ValueInt('a'), 1);
  checkEqual(nv.ValueInt('b'), 0);
  checkEqual(nv.ValueInt('c'), 0);
  checkEqual(nv.AsCsv('=', ';'), 'a=1;b=be;');
  checkEqual(nv.AsJson, '{"a":"1","b":"be"}');
  tmp.Init('{a:10,b:"bee"}');
  check(nv.InitFromJson(tmp.buf));
  checkEqual(nv.Count, 2);
  checkEqual(nv.Str['a'], '10');
  checkEqual(nv.Str['b'], 'bee');
  checkEqual(nv.Str['c'], '');
  checkEqual(nv.Int['a'], 10);
  checkEqual(nv.Int['b'], 0);
  checkEqual(nv.Int['c'], 0);
  checkEqual(nv.AsCsv('=', ';'), 'a=10;b=bee;');
  checkEqual(nv.AsJson, '{"a":"10","b":"bee"}');
  check(nv.Delete('b'));
  checkEqual(nv.ValueInt('a'), 10);
  checkEqual(nv.Str['b'], '');
  check(not nv.Delete('b'));
  checkEqual(nv.DeleteByValue('10'), 1);
  checkEqual(nv.ValueInt('a'), 0);
  checkEqual(nv.DeleteByValue('10'), 0);
  checkEqual(nv.Count, 0);
  checkEqual(nv.AsCsv('=', ';'), '');
  tmp.Init('{"a":20,b:"bi"]');
  check(not nv.InitFromJson(tmp.buf));
  checkEqual(nv.Count, 0);
end;

procedure TTestCoreBase._TSynUniqueIdentifier;
const
  JAN2015_UNIX = 1420070400;
var
  gen: TSynUniqueIdentifierGenerator;
  i1, i2: TSynUniqueIdentifierBits;
  js: TSynUnique53;
  i3: TSynUniqueIdentifier;
  rounds, i: integer;
  json, obfusc: RawUtf8;
  timer: TPrecisionTimer;
begin
  for rounds := 0 to 1 do
  begin
    gen := TSynUniqueIdentifierGenerator.Create(10, 'toto', rounds * 100);
    try
      for i := 1 to 50000 do
      begin
        i1.Value := 0;
        i2.Value := 0;
        gen.ComputeNew(i1);
        gen.ComputeNew(i2);
        check(i1.Value <> 0);
        check(i2.Value <> 0);
        check(i1.ProcessID = 10);
        check(i2.ProcessID = 10);
        check(i1.CreateTimeUnix > JAN2015_UNIX);
        check(i1.CreateTimeUnix <= i2.CreateTimeUnix);
        check(i1.Value < i2.Value);
        check(not i1.Equal(i2));
        i2.From(i1.Value);
        check(i1.Equal(i2));
        json := VariantSaveJson(i1.AsVariant);
        checkEqual(VariantSaveJson(i2.AsVariant), json);
        CheckEqual(json, FormatUtf8(
          '{"Created":"%","Identifier":%,"Counter":%,"Value":%,"Hex":"%"}',
          [DateTimeToIso8601Text(i1.CreateDateTime), i1.ProcessID, i1.Counter,
           i1.Value, Int64ToHex(i1.Value)]), 'asvariant');
        obfusc := gen.ToObfuscated(i1.Value);
        check(gen.FromObfuscated(obfusc, i3));
        check(i1.Value = i3);
        if rounds > 0 then
          check(Length(obfusc) = 32)
        else
          check(Length(obfusc) = 24);
        inc(obfusc[12]);
        check(not gen.FromObfuscated(obfusc, i3), 'tempered text');
        dec(obfusc[12]);
      end;
      check(gen.FromObfuscated(obfusc, i3), 'detempered');
      //writeln('LastUnixCreateTime=', gen.LastUnixCreateTime);
      //writeln('UnixTimeUtc=', UnixTimeUtc);
    finally
      gen.Free;
    end;
  end;
  gen := TSynUniqueIdentifierGenerator.Create(10, 'toto', 100);
  try
    i3 := 0;
    check(gen.FromObfuscated(obfusc, i3), 'SharedObfuscationKey');
    checkEqual(i1.Value, i3, 'FromObfuscated');
    i1.Value := 0;
    timer.Start;
    for i := 1 to 100000 do
    begin
      gen.ComputeNew(i2);
      check(i2.Value <> 0);
      Check(i1.Value <> i2.Value, 'ComputeNew');
      i1 := i2;
    end;
    NotifyTestSpeed('ComputeNew', gen.ComputedCount, 0, @timer);
    check(i1.Value <> 0);
    check(i2.Value <> 0);
    js := i1.JavaScriptID;
    check(js < MAX_SAFE_JS_INTEGER);
    check(i1.Value <> 0);
    check(i2.Value <> 0);
    CheckEqual(js, i1.JavaScriptID);
    CheckEqual(i1.Value, i2.Value);
    i2.Value := 0;
    CheckNotEqual(i1.Value, i2.Value);
    CheckNotEqual(js, i2.JavaScriptID);
    i2.JavaScriptID := js;
    CheckEqual(i1.Value, i2.Value);
  finally
    gen.Free;
  end;
end;

type
  TSDKey = record
    i: integer;
    u: TGuid;
    j: integer;
  end;
  TSDKeys = array of TSDKey;

{  TSynDictionary perf numbers on increasing integers or random guid strings
   with FPC 3.2.0 on x86_64 with our fpcx64mm - which is our main server target

 About Find(), we tested 3 scenarios based on the pre-computed r[] lookups

 A) r[i] := i;  =  unrealistic case, but best performance

 count=1,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=8KB col=5.1K curcol=1K 107%  add 6.1M/s find 14M/s
  crc32c int  setcap slots=8KB col=1K curcol=1K 108%  add 12.2M/s find 14.9M/s
  crc32c guid grow   slots=8KB col=3.2K curcol=497 49%  add 6.7M/s find 14.6M/s
  crc32c guid setcap slots=8KB col=489 curcol=489 48%  add 13.6M/s find 14.9M/s
  aesni  int  grow   slots=8KB col=3.3K curcol=466 46%  add 6.9M/s find 15.6M/s
  aesni  int  setcap slots=8KB col=466 curcol=466 46%  add 12.8M/s find 15.8M/s
  aesni  guid grow   slots=8KB col=4K curcol=587 58%  add 6.2M/s find 14M/s
  aesni  guid setcap slots=8KB col=540 curcol=540 54%  add 13.2M/s find 14.2M/s
 count=10,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=64KB col=17.9K curcol=2.3K 24%  add 9.9M/s find 19.3M/s
  crc32c int  setcap slots=128KB col=1.2K curcol=1.2K 12%  add 17.1M/s find 20M/s
  crc32c guid grow   slots=64KB col=33K curcol=7.7K 79%  add 6.5M/s find 13.9M/s
  crc32c guid setcap slots=128KB col=2.1K curcol=2.1K 22%  add 8M/s find 15.3M/s
  aesni  int  grow   slots=64KB col=33.1K curcol=7.9K 81%  add 7.2M/s find 14.1M/s
  aesni  int  setcap slots=128KB col=2.1K curcol=2.1K 22%  add 15M/s find 18M/s
  aesni  guid grow   slots=64KB col=33.2K curcol=7.1K 73%  add 6.3M/s find 14M/s
  aesni  guid setcap slots=128KB col=2.1K curcol=2.1K 21%  add 14M/s find 16.8M/s
 count=100,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=512KB col=185.9K curcol=74.6K 76%  add 6.2M/s find 11.1M/s
  crc32c int  setcap slots=1MB col=15.3K curcol=15.3K 15%  add 8.3M/s find 14.4M/s
  crc32c guid grow   slots=512KB col=358.1K curcol=156K 159%  add 3.8M/s find 6M/s
  crc32c guid setcap slots=1MB col=30.2K curcol=30.2K 30%  add 7.8M/s find 8M/s
  aesni  int  grow   slots=512KB col=366.8K curcol=155.2K 158%  add 3.7M/s find 6.6M/s
  aesni  int  setcap slots=1MB col=29.7K curcol=29.7K 30%  add 6.2M/s find 10M/s
  aesni  guid grow   slots=512KB col=360K curcol=158.5K 162%  add 3.5M/s find 5.7M/s
  aesni  guid setcap slots=1MB col=30.3K curcol=30.3K 31%  add 6.9M/s find 7.8M/s
 count=1,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=7.9MB col=2.9M curcol=503.6K 51%  add 2.7M/s find 6M/s
  crc32c int  setcap slots=7.9MB col=503.6K curcol=503.6K 51%  add 5.9M/s find 6M/s
  crc32c guid grow   slots=7.9MB col=3.6M curcol=454.9K 46%  add 1.6M/s find 4.4M/s
  crc32c guid setcap slots=7.9MB col=452.1K curcol=452.1K 46%  add 4.5M/s find 4.4M/s
  aesni  int  grow   slots=7.9MB col=3.6M curcol=453.6K 46%  add 1.6M/s find 4.6M/s
  aesni  int  setcap slots=7.9MB col=453.6K curcol=453.6K 46%  add 4.7M/s find 4.6M/s
  aesni  guid grow   slots=7.9MB col=3.6M curcol=457.8K 46%  add 1.6M/s find 4.4M/s
  aesni  guid setcap slots=7.9MB col=454.2K curcol=454.2K 46%  add 4.4M/s find 4.4M/s
 count=10,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=63.2MB col=28.6M curcol=6.4M 67%  add 2.3M/s find 5.1M/s
  crc32c int  setcap slots=79.7MB col=4.5M curcol=4.5M 47%  add 5.2M/s find 5.6M/s
  crc32c guid grow   slots=63.2MB col=32.5M curcol=7.2M 76%  add 1.4M/s find 3.3M/s
  crc32c guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 4M/s
  aesni  int  grow   slots=63.2MB col=32.4M curcol=7.2M 75%  add 1.5M/s find 3.6M/s
  aesni  int  setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 4.1M/s find 4.2M/s
  aesni  guid grow   slots=63.2MB col=32.6M curcol=7.2M 75%  add 1.4M/s find 3.3M/s
  aesni  guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 3.9M/s

 B) r[i] := Random32((Count shr 2) - 1) - maybe realistic use case (25% coverage)

 count=1,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=8KB col=5.1K curcol=1K 107%  add 6.9M/s find 18.7M/s
  crc32c int  setcap slots=8KB col=1K curcol=1K 108%  add 13.8M/s find 19M/s
  crc32c guid grow   slots=8KB col=4K curcol=540 54%  add 6.1M/s find 19.8M/s
  crc32c guid setcap slots=8KB col=395 curcol=395 39%  add 14M/s find 20.2M/s
  aesni  int  grow   slots=8KB col=3.3K curcol=478 47%  add 7.2M/s find 20.2M/s
  aesni  int  setcap slots=8KB col=478 curcol=478 47%  add 14.6M/s find 20.7M/s
  aesni  guid grow   slots=8KB col=4.3K curcol=531 53%  add 5.9M/s find 18.3M/s
  aesni  guid setcap slots=8KB col=437 curcol=437 43%  add 13.4M/s find 19M/s
 count=10,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=64KB col=17.9K curcol=2.3K 24%  add 9.8M/s find 17.8M/s
  crc32c int  setcap slots=128KB col=1.2K curcol=1.2K 12%  add 17.4M/s find 17.5M/s
  crc32c guid grow   slots=64KB col=31.2K curcol=7.5K 77%  add 6.8M/s find 17.3M/s
  crc32c guid setcap slots=128KB col=2.1K curcol=2.1K 21%  add 10.9M/s find 11.5M/s
  aesni  int  grow   slots=64KB col=31.5K curcol=7.3K 74%  add 7.4M/s find 16.1M/s
  aesni  int  setcap slots=128KB col=2K curcol=2K 21%  add 16.1M/s find 17.4M/s
  aesni  guid grow   slots=64KB col=32.2K curcol=7.3K 75%  add 6.3M/s find 16.4M/s
  aesni  guid setcap slots=128KB col=2.1K curcol=2.1K 22%  add 14.7M/s find 16.4M/s
 count=100,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=512KB col=185.9K curcol=74.6K 76%  add 5.9M/s find 10.3M/s
  crc32c int  setcap slots=1MB col=15.3K curcol=15.3K 15%  add 9.4M/s find 8.9M/s
  crc32c guid grow   slots=512KB col=366.7K curcol=153.2K 156%  add 3.9M/s find 6.8M/s
  crc32c guid setcap slots=1MB col=30.2K curcol=30.2K 31%  add 6.9M/s find 6.1M/s
  aesni  int  grow   slots=512KB col=355.4K curcol=156.2K 160%  add 3.8M/s find 7.3M/s
  aesni  int  setcap slots=1MB col=29.6K curcol=29.6K 30%  add 6.2M/s find 8.8M/s
  aesni  guid grow   slots=512KB col=362.3K curcol=158.4K 162%  add 3.7M/s find 5.9M/s
  aesni  guid setcap slots=1MB col=30K curcol=30K 30%  add 6.9M/s find 5.8M/s
 count=1,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=7.9MB col=2.9M curcol=503.6K 51%  add 2.7M/s find 2.3M/s
  crc32c int  setcap slots=7.9MB col=503.6K curcol=503.6K 51%  add 5.8M/s find 2.3M/s
  crc32c guid grow   slots=7.9MB col=3.6M curcol=455.7K 46%  add 1.6M/s find 2.4M/s
  crc32c guid setcap slots=7.9MB col=456.5K curcol=456.5K 46%  add 4.5M/s find 2.4M/s
  aesni  int  grow   slots=7.9MB col=3.6M curcol=453.1K 46%  add 1.7M/s find 2.4M/s
  aesni  int  setcap slots=7.9MB col=453.1K curcol=453.1K 46%  add 4.7M/s find 2.3M/s
  aesni  guid grow   slots=7.9MB col=3.5M curcol=452K 46%  add 1.6M/s find 2.3M/s
  aesni  guid setcap slots=7.9MB col=452.9K curcol=452.9K 46%  add 4.5M/s find 2.3M/s
 count=10,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=63.2MB col=28.6M curcol=6.4M 67%  add 2.3M/s find 2M/s
  crc32c int  setcap slots=79.7MB col=4.5M curcol=4.5M 47%  add 5.2M/s find 2M/s
  crc32c guid grow   slots=63.2MB col=32.6M curcol=7.2M 75%  add 1.4M/s find 2M/s
  crc32c guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 1.9M/s
  aesni  int  grow   slots=63.2MB col=32.5M curcol=7.2M 75%  add 1.5M/s find 2M/s
  aesni  int  setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 4.1M/s find 2M/s
  aesni  guid grow   slots=63.2MB col=32.5M curcol=7.2M 75%  add 1.4M/s find 1.9M/s
  aesni  guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 2M/s

 C) r[i] := Random32(Count - 1) - worse case with full range coverage

 count=1,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=8KB col=5.1K curcol=1K 107%  add 6.5M/s find 13.8M/s
  crc32c int  setcap slots=8KB col=1K curcol=1K 108%  add 13.6M/s find 14.6M/s
  crc32c guid grow   slots=8KB col=3.6K curcol=574 57%  add 6.1M/s find 14.6M/s
  crc32c guid setcap slots=8KB col=527 curcol=527 52%  add 13.2M/s find 15.6M/s
  aesni  int  grow   slots=8KB col=3.7K curcol=479 47%  add 7M/s find 16.4M/s
  aesni  int  setcap slots=8KB col=480 curcol=480 48%  add 12.2M/s find 16.7M/s
  aesni  guid grow   slots=8KB col=3K curcol=514 51%  add 6.5M/s find 15.1M/s
  aesni  guid setcap slots=8KB col=527 curcol=527 52%  add 13M/s find 14.6M/s
 count=10,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=64KB col=17.9K curcol=2.3K 24%  add 9.7M/s find 13.9M/s
  crc32c int  setcap slots=128KB col=1.2K curcol=1.2K 12%  add 17.3M/s find 14.4M/s
  crc32c guid grow   slots=64KB col=30.4K curcol=7.1K 73%  add 6.4M/s find 10M/s
  crc32c guid setcap slots=128KB col=2.1K curcol=2.1K 22%  add 9.7M/s find 9.3M/s
  aesni  int  grow   slots=64KB col=30.8K curcol=7.6K 77%  add 7.2M/s find 10.7M/s
  aesni  int  setcap slots=128KB col=2.1K curcol=2.1K 22%  add 14.2M/s find 12.2M/s
  aesni  guid grow   slots=64KB col=32.6K curcol=7.9K 81%  add 6M/s find 9.4M/s
  aesni  guid setcap slots=128KB col=2.1K curcol=2.1K 21%  add 13.8M/s find 7.1M/s
 count=100,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=512KB col=185.9K curcol=74.6K 76%  add 6.8M/s find 2.9M/s
  crc32c int  setcap slots=1MB col=15.3K curcol=15.3K 15%  add 6.4M/s find 3.2M/s
  crc32c guid grow   slots=512KB col=367.9K curcol=159.9K 163%  add 3.7M/s find 2.3M/s
  crc32c guid setcap slots=1MB col=30.8K curcol=30.8K 31%  add 7.1M/s find 2.5M/s
  aesni  int  grow   slots=512KB col=361.5K curcol=157.8K 161%  add 3.5M/s find 2.4M/s
  aesni  int  setcap slots=1MB col=29.5K curcol=29.5K 30%  add 5.4M/s find 2.9M/s
  aesni  guid grow   slots=512KB col=356.2K curcol=156.4K 160%  add 3.2M/s find 2.3M/s
  aesni  guid setcap slots=1MB col=30.7K curcol=30.7K 31%  add 7.4M/s find 3M/s
 count=1,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=7.9MB col=2.9M curcol=503.6K 51%  add 2.7M/s find 1.8M/s
  crc32c int  setcap slots=7.9MB col=503.6K curcol=503.6K 51%  add 5.8M/s find 1.8M/s
  crc32c guid grow   slots=7.9MB col=3.5M curcol=453K 46%  add 1.6M/s find 1.8M/s
  crc32c guid setcap slots=7.9MB col=454.2K curcol=454.2K 46%  add 4.5M/s find 1.8M/s
  aesni  int  grow   slots=7.9MB col=3.6M curcol=456.4K 46%  add 1.7M/s find 1.8M/s
  aesni  int  setcap slots=7.9MB col=456.4K curcol=456.4K 46%  add 4.7M/s find 1.8M/s
  aesni  guid grow   slots=7.9MB col=3.6M curcol=452.6K 46%  add 1.6M/s find 1.8M/s
  aesni  guid setcap slots=7.9MB col=453.3K curcol=453.3K 46%  add 4.2M/s find 1.8M/s
 count=10,000,000 DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2
  crc32c int  grow   slots=63.2MB col=28.6M curcol=6.4M 67%  add 2.3M/s find 1.5M/s
  crc32c int  setcap slots=79.7MB col=4.5M curcol=4.5M 47%  add 5.2M/s find 1.5M/s
  crc32c guid grow   slots=63.2MB col=32.5M curcol=7.2M 75%  add 1.4M/s find 1.4M/s
  crc32c guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 1.5M/s
  aesni  int  grow   slots=63.2MB col=32.5M curcol=7.2M 75%  add 1.5M/s find 1.5M/s
  aesni  int  setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 4.1M/s find 1.5M/s
  aesni  guid grow   slots=63.2MB col=32.5M curcol=7.2M 75%  add 1.4M/s find 1.4M/s
  aesni  guid setcap slots=79.7MB col=4.3M curcol=4.3M 45%  add 3.9M/s find 1.5M/s

  comments:
   1) r[i]=i is faster than r[i]=Random32() - up to 3x for count>100,000
     -> naive tests with simple loops are pointless - and we won't use a
        dictionary for a lookup from first to last item, anyway
   2) SetCapacity() makes Add() up to twice faster, but not affects Find() much
   3) Only default DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2 settings are shown,
      since we found out that this combination was a good balance,
      but you can recompile the tests to compare with other algorithms
   => CPU cache size seems to have a bigger impact than our TSynDictionary :)
}

{.$define DYNARRAYHASHCOLLISIONCOUNT}
// should also be defined in mormot.core.data.pas to have detailed information

procedure TTestCoreBase.TSynDictionarySlow(Context: TObject);
type
  tvalue = variant;
  tvalues = TVariantDynArray;
const
  MAX = 10000;
var
  dict: TSynDictionary;
  rnd: TLecuyer; // local per-thread instance

  procedure TestSpeed(Count: integer; SetCapacity, DoText: boolean;
    Hasher: THasher; const Msg: RawUtf8);
  var
    timer: TPrecisionTimer;
    i: PtrInt;
    v: integer;
    dic: TSynDictionary;
    a: TRawUtf8DynArray;
    r: TIntegerDynArray;
  begin
    if not Assigned(Hasher) then
      exit; // AesNiHash32 not available on this platform
    SetLength(a, Count);
    SetLength(r, Count);
    for i := 0 to High(a) do // pre-computed values and indexes for fairness
    begin
      v := rnd.Next(Count shr 2) shl 2; // realistic 25% coverage
      Check(v < Count, 'random32 overflow');
      r[i] := v;
      if DoText then
        rnd.FillAscii(38, a[i])
      else
        UInt32ToUtf8(i, a[i]);
    end;
    dic := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray),
      TypeInfo(TIntegerDynArray), false, 0, nil, Hasher);
    dic.ThreadUse := uNoLock; // faster without locking
    if SetCapacity then
      dic.Capacity := Length(a);
    timer.Start;
    for i := 0 to High(a) do
    begin
      v := i;
      dic.Add(a[i], v);
    end;
    NotifyTestSpeed('add  %', [Msg], Count, 0, @timer, {onlylog=}
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT} false);
    write(' ', Msg, ' slots=', KBNoSpace(dic.Keys.Hasher.HashTableSize * 4),
      ' col=', K(dic.Keys.Hasher.CountCollisions), ' curcol=',
      K(dic.Keys.Hasher.CountCollisionsCurrent),
      ' ', (dic.Keys.Hasher.CountCollisionsCurrent * 100) div Count,
      '%  add ', K(timer.PerSec(count)), '/s');
    {AddConsole(FormatString('collisions: total=% current=% %%',
      [dic.Keys.Hasher.CountCollisions, dic.Keys.Hasher.CountCollisionsCurrent,
       (dic.Keys.Hasher.CountCollisionsCurrent * 100) div Count, '%']));}
    {$else} true); {$endif DYNARRAYHASHCOLLISIONCOUNT}
    timer.Start;
    for i := 0 to High(a) do
    begin
      // FindAndCopy + random index from 25% of the content
      Check(dic.FindAndCopy(a[r[i]], v));
      Check(v = r[i]);
    end;
    NotifyTestSpeed('find %', [Msg], Count, 0, @timer, {onlylog=}
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT} false);
    writeln(' find ', K(timer.PerSec(count)), '/s');
    {$else} true); {$endif DYNARRAYHASHCOLLISIONCOUNT}
    dic.Free;
  end;

  procedure Test;
  var
    k: RawUtf8;
    v: tvalue;
    i: integer;
  begin
    check(dict.Count = MAX);
    for i := 1 to MAX do
    begin
      UInt32ToUtf8(i, k);
      v := 0;
      check(dict.Exists(k));
      check(dict.FindAndCopy(k, v));
      check(v = i);
    end;
  end;

  procedure SetDict;
  begin
    {$ifdef HASGENERICS}
    dict := TSynDictionary.New<RawUtf8, RawUtf8>(True);
    {$else}
    dict := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray), TypeInfo(TRawUtf8DynArray), True);
    {$endif HASGENERICS}
  end;

var
  v, kv: tvalue;
  s, k, key, val, u, json: RawUtf8;
  i, n: integer;
  i64: Int64;
  exists: boolean;
  b: byte;
  sdk: TSDKey;
begin
  RandomLecuyer(rnd); // local per-thread generator
  SetDict;
  try
    CheckEqual(dict.Count, 0);
    CheckEqual(dict.Capacity, 0);
  finally
    dict.Free;
  end;
  dict := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray), TypeInfo(TRawUtf8DynArray), True);
  try
    CheckEqual(dict.Count, 0);
    CheckEqual(dict.Capacity, 0);
    dict.Capacity := 64;
    CheckEqual(dict.Count, 0);
    CheckEqual(dict.Capacity, 64);
  finally
    dict.Free;
  end;
  SetDict;
  try
    CheckEqual(dict.Count, 0);
    CheckEqual(dict.Capacity, 0);
    key := 'Foobar';
    val := 'lol';
    Check(dict.AddOrUpdate(key, val) >= 0);
    CheckEqual(dict.Count, 1);
    json := dict.SaveToJson;
    CheckEqual(json, '{"Foobar":"lol"}');
    key := 'foobar';
    val := 'xxx';
    dict.AddOrUpdate(key, val);
    json := dict.SaveToJson;
    CheckEqual(json, '{"Foobar":"xxx"}');
    CheckEqual(dict.Count, 1);
    key := 'FooBar';
    dict.FindAndCopy(key, val, False);
    CheckEqual(val, 'xxx');
  finally
    dict.Free;
  end;
  {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
  n := 1000;
  for i := 1 to 5 do
  {$else}
  n := 100;
  for i := 1 to 3 do
  {$endif DYNARRAYHASHCOLLISIONCOUNT}
  begin
    {$ifdef DYNARRAYHASHCOLLISIONCOUNT}
    writeln('count=', IntToThousandString(n), ' DYNARRAYHASH_LEMIRE + DYNARRAYHASH_PO2');
    {$endif DYNARRAYHASHCOLLISIONCOUNT}
    TestSpeed(n, false, false, crc32c, 'crc32c int  grow  ');
    TestSpeed(n, true,  false, crc32c, 'crc32c int  setcap');
    TestSpeed(n, false, true,  crc32c, 'crc32c guid grow  ');
    TestSpeed(n, true,  true,  crc32c, 'crc32c guid setcap');
    TestSpeed(n, false, false, AesNiHash32, 'aesni  int  grow  ');
    TestSpeed(n, true,  false, AesNiHash32, 'aesni  int  setcap');
    TestSpeed(n, false, true,  AesNiHash32, 'aesni  guid grow  ');
    TestSpeed(n, true,  true,  AesNiHash32, 'aesni  guid setcap');
    n := n * 10;
  end;
  dict := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray), TypeInfo(tvalues));
  try
    for i := 1 to MAX do
    begin
      UInt32ToUtf8(i, k);
      v := i;
      check(dict.Add(k, v) = i - 1);
    end;
    Test;
    json := dict.SaveToJson;
    check(dict.Exists(k));
    dict.DeleteAll;
    check(dict.Count = 0);
    check(not dict.Exists(k));
    check(dict.LoadFromJson(json));
    Test;
    s := dict.SaveToBinary;
    u := '{"a":1,"b":2}';
    check(dict.LoadFromJson(u));
    CheckEqual(dict.SaveToJson, u);
    check(dict.LoadFromJson('{a:1,b:2}'), 'extended syntax');
    CheckEqual(dict.SaveToJson, u);
    check(dict.LoadFromJson('{a:1,2:{b:3,c:4}}'));
    CheckEqual(dict.SaveToJson, '{"a":1,"2":{"b":3,"c":4}}');
  finally
    dict.Free;
  end;
  dict := TSynDictionary.Create(TypeInfo(TRawUtf8DynArray), TypeInfo(tvalues));
  try
    check(dict.LoadFromBinary(s));
    Test;
    for i := MAX downto 1 do
      if i and 127 = 0 then
      begin
        UInt32ToUtf8(i, k);
        check(dict.Delete(k) = i - 1);
        check(dict.Exists(k) = false);
      end;
    for i := 1 to MAX do
    begin
      exists := (i and 127) <> 0;
      UInt32ToUtf8(i, k);
      check(dict.Exists(k) = exists);
      if exists then
      begin
        v := 0;
        check(dict.FindAndCopy(k, v));
        check(v = i);
        if i < 10000 then
        begin
          // FindKeyFromValue() brute force is slow
          k := '';
          check(dict.FindKeyFromValue(v, k));
          check(GetInteger(pointer(k)) = i);
        end;
      end;
    end;
  finally
    dict.Free;
  end;
  // keys which are not serialized as JSON string
  dict := TSynDictionary.Create(TypeInfo(TInt64DynArray), TypeInfo(tvalues));
  try
    for i := 1 to MAX do
    begin
      i64 := i;
      v := i;
      check(dict.Add(i64, v) = i - 1);
    end;
    json := dict.SaveToJson;
    Check(IsValidUtf8(json));
    Check(IsValidJson(json));
    CheckHash(json, $F67B5FA8, 'dict.savetojson');
    for i := 1 to MAX do
    begin
      i64 := i;
      check(dict.FindAndCopy(i64, v));
      check(v = i);
    end;
  finally
    dict.Free;
  end;
  dict := TSynDictionary.Create(TypeInfo(TInt64DynArray), TypeInfo(tvalues));
  try
    check(dict.LoadFromJson(json));
    CheckHash(json, $F67B5FA8, 'untouched after loadfromjson');
    checkEqual(json, dict.SaveToJson);
    for i := 1 to MAX do
    begin
      i64 := i;
      check(dict.FindAndCopy(i64, v));
      check(v = i);
    end;
  finally
    dict.Free;
  end;
  // keys with no standard RTTI: fallback to binary hash/compare
  FillCharFast(sdk, SizeOf(sdk), 0);
  v := 10;
  dict := TSynDictionary.Create(TypeInfo(TSDKeys), TypeInfo(tvalues));
  try
    CheckEqual(dict.Count, 0);
    CheckEqual(dict.Capacity, 0);
    Check(not dict.FindAndCopy(sdk, v));
    Check(dict.Add(sdk, v) = 0);
    v := 1;
    Check(v = 1);
    Check(dict.FindAndCopy(sdk, v));
    Check(v = 10);
    sdk.j := 1;
    Check(not dict.FindAndCopy(sdk, v));
    sdk.j := 0;
    CheckEqual(dict.Count, 1);
    Check(dict.Capacity > 0);
  finally
    dict.Free;
  end;
  dict := TSynDictionary.Create(TypeInfo(TSDKeys), TypeInfo(tvalues), false,
    0, nil, nil, ptInteger);
  try
    Check(not dict.FindAndCopy(sdk, v));
    Check(dict.Add(sdk, v) = 0);
    v := 1;
    Check(dict.FindAndCopy(sdk, v));
    Check(v = 10);
    v := 1;
    sdk.j := 1;
    Check(dict.FindAndCopy(sdk, v), 'ptInteger=search i only');
    Check(v = 10);
  finally
    dict.Free;
  end;
  // validate variant as keys, with proper hashing of simple or complex types
  {$ifdef HASGENERICS}
  dict := TSynDictionary.New<variant, byte>;
  {$else}
  dict := TSynDictionary.Create(TypeInfo(TVariantDynArray), TypeInfo(TByteDynArray));
  {$endif HASGENERICS}
  try
    kv := byte(1);
    CheckEqual(TVarData(kv).VType, varByte);
    b := 0;
    Check(not dict.FindAndCopy(kv, b));
    b := 255;
    CheckEqual(dict.Add(kv, b), 0);
    b := 0;
    Check(dict.FindAndCopy(kv, b));
    CheckEqual(b, 255);
    kv := integer(1);
    CheckEqual(TVarData(kv).VType, varInteger);
    b := 0;
    Check(dict.FindAndCopy(kv, b));
    CheckEqual(b, 255);
    RawUtf8ToVariant('toto', kv);
    b := 0;
    Check(not dict.FindAndCopy(kv, b));
    b := 254;
    CheckEqual(dict.Add(kv, b), 1);
    b := 0;
    Check(dict.FindAndCopy(kv, b));
    CheckEqual(b, 254);
    kv := word(1);
    CheckEqual(TVarData(kv).VType, varWord);
    Check(dict.FindAndCopy(kv, b));
    CheckEqual(b, 255);
    kv := _JsonFast('[1,2,{a:3}]');
    b := 0;
    Check(not dict.FindAndCopy(kv, b));
    b := 253;
    CheckEqual(dict.Add(kv, b), 2);
    kv := WideString('toto');
    CheckEqual(TVarData(kv).VType, varOleStr);
    Check(dict.FindAndCopy(kv, b));
    CheckEqual(b, 254);
    kv := _JsonFast('[ 1, 2, {"a":3} ]');
    Check(dict.FindAndCopy(kv, b), 'json should not matter');
    CheckEqual(b, 253);
finally
    dict.Free;
  end;
end;

procedure TTestCoreBase._TSynQueue;
var
  o, i, j, k, n: integer;
  f: TSynQueue;
  u, v: RawUtf8;
  savedint: TIntegerDynArray;
  savedu: TRawUtf8DynArray;
begin
  f := TSynQueue.Create(TypeInfo(TIntegerDynArray));
  try
    for o := 1 to 1000 do
    begin
      check(f.Count = 0);
      check(not f.Pending);
      for i := 1 to o do
        f.Push(i);
      check(f.Pending);
      check(f.Count = o);
      check(f.Capacity >= o);
      f.Save(savedint);
      check(Length(savedint) = o);
      for i := 1 to o do
      begin
        j := -1;
        check(f.Peek(j));
        check(j = i);
        j := -1;
        check(f.Pop(j));
        check(j = i);
      end;
      check(not f.Pending);
      check(f.Count = 0);
      check(f.Capacity > 0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(not f.Pending);
      check(f.Count = 0);
      check(f.Capacity = 0);
      check(Length(savedint) = o);
      for i := 1 to o do
        check(savedint[i - 1] = i);
      n := 0;
      for i := 1 to o do
        if i and 7 = 0 then
        begin
          j := -1;
          check(f.Pop(j));
          check(j and 7 <> 0);
          dec(n);
        end
        else
        begin
          f.Push(i);
          inc(n);
        end;
      check(f.Count = n);
      check(f.Pending);
      f.Save(savedint);
      check(Length(savedint) = n);
      for i := 1 to n do
        check(savedint[i - 1] and 7 <> 0);
      for i := 1 to n do
      begin
        j := -1;
        check(f.Peek(j));
        k := -1;
        check(f.Pop(k));
        check(j = k);
        check(j and 7 <> 0);
      end;
      check(f.Count = 0);
      check(f.Capacity > 0);
    end;
  finally
    f.Free;
  end;
  f := TSynQueue.Create(TypeInfo(TRawUtf8DynArray));
  try
    for o := 1 to 1000 do
    begin
      check(not f.Pending);
      check(f.Count = 0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(f.Count = 0);
      check(f.Capacity = 0);
      n := 0;
      for i := 1 to o do
        if i and 7 = 0 then
        begin
          u := '7';
          check(f.Pop(u));
          check(GetInteger(pointer(u)) and 7 <> 0);
          dec(n);
        end
        else
        begin
          u := UInt32ToUtf8(i);
          f.Push(u);
          inc(n);
        end;
      check(f.Pending);
      check(f.Count = n);
      f.Save(savedu);
      check(Length(savedu) = n);
      for i := 1 to n do
        check(GetInteger(pointer(savedu[i - 1])) and 7 <> 0);
      for i := 1 to n do
      begin
        u := '';
        check(f.Peek(u));
        v := '';
        check(f.Pop(v));
        check(u = v);
        check(GetInteger(pointer(u)) and 7 <> 0);
      end;
      check(not f.Pending);
      check(f.Count = 0);
      check(f.Capacity > 0);
    end;
    check(Length(savedu) = length(savedint));
  finally
    f.Free;
  end;
end;

procedure TTestCoreBase._DeltaCompress;
var
  o, n, d, s: RawByteString;
  i, buflen, chunk: integer;
  P: PAnsiChar;
  s1, s2: TStream;
begin
  n := RandomTextParagraph(100);
  d := DeltaCompress(n, o{%H-});
  check(DeltaExtract(d, o, s) = dsSuccess, 'delta0');
  Check(s = n);{%H-}
  d := DeltaCompress(n, s);
  check(d = '=');
  check(DeltaExtract(d, n, s) = dsSuccess, 'delta=');
  Check(s = n);
  for i := 1 to 20 do
  begin
    o := n;
    s := RandomTextParagraph(100);
    case i and 7 of
      2:
        n := n + s;
      7:
        n := s + n;
    else
      insert(s, n, i * 50);
    end;
    d := DeltaCompress(n, o);
    //ConsoleWrite('d=% s=% o=% n=%', [length(d), length(s), length(o), length(n)]);
    check(d <> '=');
    check(length(d) < length(s), 'delta should be compressed');
    check(DeltaExtract(d, o, s) = dsSuccess, 'delta+');
    Check(s = n);
  end;
  o := n;
  delete(n, 100, 100);
  d := DeltaCompress(n, o);
  check(DeltaExtract(d, o, s) = dsSuccess, 'delta-');
  Check(s = n);
  o := n;
  delete(n, 1000, 100);
  insert(RandomIdentifier(50), n, 200);
  d := DeltaCompress(n, o);
  check(DeltaExtract(d, o, s) = dsSuccess, 'delta-+');
  if CheckFailed(s = n, 'delta extract') then
    exit;
  s1 := TRawByteStringStream.Create(s);
  try
    for buflen := 8 to 32 do
      for chunk := 1 to buflen * 3 do
      begin
        s2 := TBufferedStreamReader.Create(s1, buflen);
        try
          P := pointer(n);
          FillCharFast(P^, length(n), 48);
          repeat
            i := s2.Read(P^, chunk);
            inc(P, i);
          until i = 0;
          CheckEqual(s2.Position, length(n));
          CheckEqual(s, n);
        finally
          s2.Free;
        end;
      end;
  finally
    s1.Free;
  end;
end;

procedure TTestCoreBase.BloomFilters;
const
  SIZ = 200000;
var
  b: TSynBloomFilter;
  d1, d2: TSynBloomFilterDiff;
  i, j, n: integer;
  falsepositive: double;
  sav1000, savSIZ: RawByteString;
begin
  b := TSynBloomFilter.Create(SIZ + 5000);
  try
    CheckSame(b.FalsePositivePercent, 1);
    Check(b.Size = SIZ + 5000);
    Check(b.Bits > b.Size shl 3);
    Check(b.HashFunctions = 7);
    Check(b.Inserted = 0);
    CheckLogTimeStart;
    for i := 1 to SIZ do
      Check(not b.MayExist(@i, SizeOf(i)));
    CheckLogTime(b.Inserted = 0, 'MayExists(%)=false', [SIZ]);
    for i := 1 to 1000 do
      b.Insert(@i, SizeOf(i));
    CheckLogTime(b.Inserted = 1000, 'Insert(%)', [b.Inserted]);
    sav1000 := b.SaveTo;
    CheckLogTime(sav1000 <> '', 'b.SaveTo(%) len=%', [b.Inserted, kb(sav1000)]);
    for i := 1001 to SIZ do
      b.Insert(@i, SizeOf(i));
    CheckLogTime(b.Inserted = SIZ, 'Insert(%)', [SIZ - 1000]);
    savSIZ := b.SaveTo;
    CheckLogTime(length(savSIZ) > length(sav1000), 'b.SaveTo(%) len=%',
      [SIZ, kb (savSIZ)]);
    for i := 1 to SIZ do
      Check(b.MayExist(@i, SizeOf(i)));
    CheckLogTime(b.Inserted = SIZ, 'MayExists(%)=true', [SIZ]);
    n := 0;
    for i := SIZ + 1 to SIZ + SIZ shr 5 do
      if b.MayExist(@i, SizeOf(i)) then
        inc(n);
    falsepositive := (n * 100) / (SIZ shr 5); // always 0.704 for crc32c
    // 0.9 with crc, 0.7-1.3 with AesNiHash32 depending on its seed
    CheckLogTime(falsepositive < 1.5, 'falsepositive=%', [falsepositive]);
    b.Reset;
    CheckLogTime(b.Inserted = 0, 'b.Reset', []);
    for i := 1 to SIZ do
      Check(not b.MayExist(@i, SizeOf(i)));
    CheckLogTime(b.Inserted = 0, 'MayExists(%)=false', [SIZ]);
    CheckLogTime(b.LoadFrom(sav1000), 'b.LoadFrom(%)', [1000]);
    for i := 1 to 1000 do
      Check(b.MayExist(@i, SizeOf(i)));
    CheckLogTime(b.Inserted = 1000, 'MayExists(%)=true', [1000]);
  finally
    b.Free;
  end;
  CheckLogTime(true, 'b.Free', []);
  d1 := TSynBloomFilterDiff.Create(savSIZ);
  try
    CheckLogTime(true, 'd1 := TSynBloomFilterDiff.Create(%)', [SIZ]);
    CheckSame(d1.FalsePositivePercent, 1);
    Check(d1.Size = SIZ + 5000);
    Check(d1.Bits > d1.Size shl 3);
    Check(d1.HashFunctions = 7);
    for i := 1 to SIZ do
      Check(d1.MayExist(@i, SizeOf(i)));
    CheckLogTime(d1.Inserted = SIZ, 'MayExists(%)=true', [SIZ]);
    d2 := TSynBloomFilterDiff.Create;
    try
      Check(d2.Revision = 0);
      n := SIZ;
      for j := 1 to 3 do
      begin
        savSIZ := d1.SaveToDiff(d2.Revision);
        CheckLogTime(savSIZ <> '', 'd1.SaveToDiff(%) len=%', [d2.Revision, KB(savSIZ)]);
        Check(d1.DiffKnownRevision(savSIZ) = d1.Revision);
        Check((d2.Revision = d1.Revision) = (j > 1));
        CheckLogTime(d2.LoadFromDiff(savSIZ), 'd2.LoadFromDiff(%)', [n]);
        Check(d2.Revision = d1.Revision);
        Check(d2.Size = d1.Size);
        for i := 1 to n do
          Check(d2.MayExist(@i, SizeOf(i)));
        CheckLogTime(d2.Inserted = cardinal(n), 'MayExists(%)=true', [n]);
        for i := n + 1 to n + 1000 do
          d1.Insert(@i, SizeOf(i));
        CheckLogTime(d2.Revision <> d1.Revision, 'd1.Insert(%)', [1000]);
        savSIZ := d1.SaveToDiff(d2.Revision);
        CheckLogTime(savSIZ <> '', 'd1.SaveToDiff(%) len=%', [d2.Revision, kb(savSIZ)]);
        Check(d1.DiffKnownRevision(savSIZ) = d1.Revision);
        Check(d2.Revision <> d1.Revision);
        CheckLogTime(d2.LoadFromDiff(savSIZ), 'd2.LoadFromDiff(%)', [n]);
        Check(d2.Revision = d1.Revision);
        inc(n, 1000);
        for i := 1 to n do
          Check(d2.MayExist(@i, SizeOf(i)));
        CheckLogTime(d2.Inserted = cardinal(n), 'MayExists(%)=true', [n]);
        Check(d2.Inserted = cardinal(n));
        if j = 2 then
        begin
          d1.DiffSnapshot;
          CheckLogTime(d2.Revision = d1.Revision, 'd1.DiffSnapshot', []);
        end;
      end;
    finally
      d2.Free;
      CheckLogTime(true, 'd2.Free', []);
    end;
  finally
    d1.Free;
    CheckLogTime(true, 'd1.Free', []);
  end;
end;

procedure TTestCoreBase._TObjArray;
const
  MAX = 200;
var
  i: integer;
  arr: TPersistentAutoCreateFieldsTestObjArray;
  test, test2: TObjArrayTest;
  p: TPersistentAutoCreateFieldsTest;
  r1, r2: TOrmArrayTest;
  tmp: RawUtf8;
  valid: boolean;

  procedure CheckValues(test: TComplexNumberObjArray);
  var
    i: integer;
  begin
    CheckEqual(length(test), MAX + 1);
    for i := 0 to MAX do
    begin
      CheckSame(test[i].Real, 0.5 + i);
      CheckSame(test[i].Imaginary, 0.2 + i);
    end;
  end;

begin
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(TypeInfo(TPersistentAutoCreateFieldsTestObjArray),
    TPersistentAutoCreateFieldsTest);
  {$endif HASDYNARRAYTYPE}
  try
    tmp := DynArraySaveJson(arr{%H-}, TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    check(tmp = '[]');
    p := TPersistentAutoCreateFieldsTest.CreateFake;
    ObjArrayAdd(arr, p);
    tmp := DynArraySaveJson(arr, TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    checkEqual(tmp, '[{"Text":"text","Value1":{"Real":1.5,"Imaginary":2.5},' +
      '"Value2":{"Real":1.7,"Imaginary":2.7}}]');
    for i := 1 to MAX do
    begin
      p := TPersistentAutoCreateFieldsTest.CreateFake;
      p.Value1.Real := p.Value1.Real + i * 1.0;
      Check(ObjArrayAdd(arr, p) = i);
    end;
    tmp := DynArraySaveJson(arr, TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    ObjArrayClear(arr);
    CheckEqual(length(arr), 0);
    DynArrayLoadJsonInPlace(arr, pointer(tmp), TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    CheckEqual(length(arr), MAX + 1);
    for i := 0 to MAX do
    begin
      Check(arr[i].text = 'text');
      CheckSame(arr[i].Value1.Real, 1.5 + i);
      CheckSame(arr[i].Value1.Imaginary, 2.5);
      CheckSame(arr[i].Value2.Real, 1.7);
      CheckSame(arr[i].Value2.Imaginary, 2.7);
    end;
  finally
    ObjArrayClear(arr);
  end;
  r1 := TOrmArrayTest.Create;
  r2 := TOrmArrayTest.Create;
  try
    check(r1.SameValues(r2));
  finally
    r2.Free;
    r1.Free;
  end;
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(TypeInfo(TComplexNumberObjArray), TComplexNumber);
  {$endif HASDYNARRAYTYPE}
  test := TObjArrayTest.CreateFake;
  try
    for i := 0 to max do
      ObjArrayAdd(test.fValues, TComplexNumber.Create(0.5 + i, 0.2 + i));
    CheckValues(test.Values);
    tmp := ObjectToJson(test);
  finally
    test.Free;
  end;
  r1 := TOrmArrayTest.CreateFrom(tmp);
  r2 := TOrmArrayTest.CreateFrom(tmp);
  try
    CheckEqual(r1.IDValue, 0);
    CheckEqual(r2.IDValue, 0);
    CheckValues(r1.Values);
    CheckValues(r2.Values);
    check(r1.SameValues(r2));
  finally
    r2.Free;
    r1.Free;
  end;
  test := TObjArrayTest.CreateFake;
  test2 := TObjArrayTest.CreateFake;
  try
    check(ObjectLoadJson(test, tmp));
    CheckValues(test.Values);
    JsonToObject(test2, pointer(tmp), valid);
    Check(valid);
    CheckValues(test2.Values);
    check(ObjectEquals(test, test2));
  finally
    test2.Free;
    test.Free;
  end;
end;

function TOrmPeopleCompareByFirstName(const A, B): integer;
begin
  result := StrIComp(
    pointer(TOrmPeople(A).FirstName), pointer(TOrmPeople(B).FirstName));
end;

{$ifdef OSWINDOWS}

procedure TTestCoreBase.WindowsSpecificApi;

  procedure Win32DotNetException(code: cardinal; const expected: RawUtf8);
  var
    s: ShortString;
  begin
    s[0] := #0;
    Check(Win32DotNetExceptions(code, s) = (expected <> ''));
    CheckEqual(ShortStringToAnsi7String(s), expected);
  end;

var
  nfo: TWinProcessInfo;
begin
  // validate Windows API error code recognition
  CheckEqualShort(WinApiErrorShort(122), 'ERROR_INSUFFICIENT_BUFFER');
  CheckEqualShort(WinApiErrorShort(1246), 'ERROR__CONTINUE');
  CheckEqual(WinApiErrorUtf8(122), 'ERROR_INSUFFICIENT_BUFFER');
  Check(WinApiErrorString(122) = 'ERROR_INSUFFICIENT_BUFFER');
  Check(WinApiErrorString(1246) = 'ERROR__CONTINUE');
  // validate DotNet exceptions error code recognition
  Win32DotNetException(0, '');
  Win32DotNetException(9234, '');
  Win32DotNetException($800703E9, ' [.NET/CLR unhandled StackOverflowException]');
  Win32DotNetException($80131500,
    ' [.NET/CLR unhandled Exception SUDSGeneratorException SUDSParserException]');
  // validate UAC specific functions
  Check(IsSystemFolder('c:\program files'));
  Check(IsSystemFolder('c:\program Files\toto'));
  Check(IsSystemFolder('c:\Program files (x86)'));
  Check(IsSystemFolder('d:\Program Files (X86)\toto'));
  Check(IsSystemFolder('c:\windows'));
  Check(IsSystemFolder('c:\windows\toto'));
  Check(not IsSystemFolder('c:\program file'));
  Check(not IsSystemFolder('c:\program files other\toto'));
  Check(not IsSystemFolder('c:\windowstorage'));
  if IsUacVirtualizationEnabled then
  begin
    Check(IsUacVirtualFolder('c:\program files'));
    Check(IsUacVirtualFolder('c:\program Files\toto'));
    Check(IsUacVirtualFolder('c:\Program files (x86)'));
    Check(IsUacVirtualFolder('d:\Program Files (X86)\toto'));
    Check(IsUacVirtualFolder('c:\windows'));
    Check(IsUacVirtualFolder('c:\windows\toto'));
    Check(not IsUacVirtualFolder('c:\program file'));
    Check(not IsUacVirtualFolder('c:\program files other\toto'));
    Check(not IsUacVirtualFolder('c:\windowstorage'));
  end
  else
    Check(not IsUacVirtualFolder('c:\program files'));
  // validate raw Windows process access
  FillCharFast(nfo, SizeOf(nfo), 0);
  GetProcessInfo(GetCurrentProcessId, nfo);
  Check(nfo.CommandLine <> '', 'Cmd0');
  Check(PosEx(Executable.ProgramName, SynUnicodeToUtf8(nfo.CommandLine)) > 0, 'Cmd1');
  Check(nfo.AvailableInfo = [wpaiPID..wpaiImagePath], 'AvailableInfo');
  CheckEqual(nfo.PID, GetCurrentProcessId, 'PID');
  Check(nfo.AffinityMask <> 0, 'AffinityMask');
end;

{$endif OSWINDOWS}


initialization
  {$ifndef HASDYNARRAYTYPE}
  Rtti.RegisterObjArray(TypeInfo(TOrmPeopleObjArray), TOrmPeople);
  {$endif HASDYNARRAYTYPE}
  Rtti.RegisterFromText([TypeInfo(TRecordPeopleDynArray),
    'RowID:TID FirstName,LastName:RawUtf8 Data:RawBlob YearOfBirth:integer YearOfDeath:word'
  ]);

end.

