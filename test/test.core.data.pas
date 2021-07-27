/// regression tests for high-level mormot.core.* units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.data;

interface

{$I ..\src\mormot.defines.inc}

// if defined, TTestCoreProcess.JSONBenchmark will include some other libraries

// as reference, on my Laptop, parsing the 1MB People.json array of objects:
  // TOrmTableJson   = 530 MB/s
  // TDocVariantData = 150 MB/s

{$define JSONBENCHMARK_FPJSON}
  // fpjson = 10.8 MB/s

{.$define JSONBENCHMARK_DELPHIJSON}
  // Delphi system.json < 3 MB/s on XE8

{.$define JSONBENCHMARK_JDO}
  // JsonDataObjects = 43 MB/s

{.$define JSONBENCHMARK_SO}
  // SuperObject = 16 MB/s on Delphi, 4.5 MB/s on FPC

{.$define JSONBENCHMARK_XSO}
  // X-SuperObject = 700 KB/s

{.$define JSONBENCHMARK_GRIJJY}
  // Grijjy = 24 MB/s

{.$define JSONBENCHMARK_DWS}
  // dwsJSON = 43 MB/s

{.$define JSONBENCHMARK_WSFT}
  // WinSoft JSON = 11 MB/s


uses
  sysutils,
  classes,
  variants,
  {$ifdef FPC}
  {$undef JSONBENCHMARK_DELPHIJSON} // system.json is not available on FPC
  {$ifdef JSONBENCHMARK_FPJSON}
  fpjson,
  jsonparser,
  {$endif JSONBENCHMARK_FPJSON}
  {$else}
  typinfo, // for proper Delphi inlining
  {$undef JSONBENCHMARK_FPJSON} // fpjson is not available on Delphi
  {$ifdef JSONBENCHMARK_DELPHIJSON}
  system.json,
  {$endif JSONBENCHMARK_DELPHIJSON}
  {$ifdef JSONBENCHMARK_JDO}
  JsonDataObjects,
  {$endif JSONBENCHMARK_JDO}
  {$endif FPC}
  {$ifdef JSONBENCHMARK_SO}
  SuperObject,
  SuperTypes,
  {$endif JSONBENCHMARK_SO}
  {$ifdef JSONBENCHMARK_XSO}
  XSuperObject,
  {$endif JSONBENCHMARK_XSO}
  {$ifdef JSONBENCHMARK_GRIJJY}
  Grijjy.Bson,
  {$endif JSONBENCHMARK_GRIJJY}
  {$ifdef JSONBENCHMARK_DWS}
  dwsJson,
  {$endif JSONBENCHMARK_DWS}
  {$ifdef JSONBENCHMARK_WSFT}
  WinJson,
  {$endif JSONBENCHMARK_WSFT}
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.data,
  mormot.core.datetime,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.mustache,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.zip,
  mormot.core.test,
  mormot.lib.z,
  mormot.lib.lizard,
  mormot.net.sock,
  mormot.db.core,
  mormot.db.nosql.bson,
  mormot.orm.core,
  mormot.rest.client,
  mormot.rest.server,
  mormot.net.client,
  test.core.base;

type
  /// this test case will test most high-level functions, classes and types
  // defined and implemented in the mormot.core.*.pas units
  TTestCoreProcess = class(TSynTestCase)
  protected
    procedure MustacheTranslate(var English: string);
    procedure MustacheHelper(const Value: variant; out Result: variant);
  published
    /// some low-level RTTI access
    // - especially the field type retrieval from published properties
    procedure _RTTI;
    /// some low-level Url encoding from parameters
    procedure UrlEncoding;
    /// some low-level JSON encoding/decoding
    procedure EncodeDecodeJSON;
    /// some performance numbers about JSON parsing and generating
    procedure JSONBenchmark;
    /// HTML generation from Wiki Or Markdown syntax
    procedure WikiMarkdownToHtml;
    /// some low-level variant process
    procedure Variants;
    /// test the Mustache template rendering unit
    procedure MustacheRenderer;
    /// variant-based JSON/BSON document process
    procedure _TDocVariant;
    /// low-level TDecimal128 decimal value process (as used in BSON)
    procedure _TDecimal128;
    /// BSON process (using TDocVariant)
    procedure _BSON;
    /// test SELECT statement parsing
    procedure _TSelectStatement;
    /// test advanced statistics monitoring
    procedure _TSynMonitorUsage;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented e.g. in the mormot.core.zip / mormot.lib.lizard units
  TTestCoreCompression = class(TSynTestCase)
  protected
    Data: RawByteString; // contains the first 1MB of mormot2tests executable
    DataFile: TFileName; // (may be truncated) mormot2tests executable copy
    M: TMemoryStream;
    crc0, crc1: cardinal; // crc0=plain crc1=deflated
  public
    procedure Setup; override;
    procedure CleanUp; override;
  published
    /// direct deflate/inflate functions
    procedure InMemoryCompression;
    /// .gzip archive handling
    procedure GZIPFormat;
    /// .zip archive handling
    procedure ZIPFormat;
    /// SynLZ internal format
    procedure _SynLZ;
    /// TAlgoCompress classes
    procedure _TAlgoCompress;
  end;

type
  /// a record used e.g. by IComplexCalculator.EchoRecord
  TConsultaNav = packed record
    MaxRows, Row0, RowCount: int64;
    IsSqlUpdateBack, EOF: boolean;
  end;

  TOrmTest = class(TOrm)
  private
    fTest: RawUtf8;
    fValfloat: double;
    fValWord: word;
    fNext: TOrmTest;
    fInt: int64;
    fValDate: TDateTime;
    fData: RawBlob;
    fAnsi: WinAnsiString;
    fUnicode: RawUnicode;
    fVariant: variant;
    procedure SetInt(const Value: int64);
  public
    procedure FillWith(i: Integer);
    procedure CheckWith(test: TSynTestCase; i: Integer; offset: integer = 0;
      checkblob: boolean = true);
  published
    property Int: int64
      read fInt write SetInt default 12;
    property Test: RawUtf8
      read fTest write fTest;
    property Unicode: RawUnicode
      read fUnicode write fUnicode;
    property Ansi: WinAnsiString
      read fAnsi write fAnsi;
    property ValFloat: double
      read fValfloat write fValFloat;
    property ValWord: word
      read fValWord write fValWord;
    property ValDate: tdatetime
      read fValDate write fValDate;
    property Next: TOrmTest
      read fNext write fNext;
    property Data: RawBlob
      read fData write fData;
    property ValVariant: variant
      read fVariant write fVariant;
  end;

type
  TCollTest = class(TCollectionItem)
  private
    FLength: Integer;
    FColor: Integer;
    FName: RawUtf8;
  published
    property Color: Integer
      read FColor write FColor;
    property Length: Integer
      read FLength write FLength;
    property Name: RawUtf8
      read FName write FName;
  end;

  TCollTestsI = class(TInterfacedCollection)
  public
    class function GetClass: TCollectionItemClass; override;
  end;

  TCollTests = class(TInterfacedCollection)
  private
    function GetCollItem(Index: Integer): TCollTest;
  public
    class function GetClass: TCollectionItemClass; override;
    function Add: TCollTest;
    property Item[Index: Integer]: TCollTest
      read GetCollItem; default;
  end;

  TMyCollection = class(TCollection);

  TCollTst = class(TPersistent)
  private
    fColl: TCollTests;
    fTCollTest: TCollTest;
    fStr: TStringList;
    procedure SetColl(const Value: TCollTests); // validate Setter
  public
    constructor Create;
    destructor Destroy; override;
  published
    property One: TCollTest
      read fTCollTest write fTCollTest;
    property Coll: TCollTests
      read fColl write SetColl;
    property Str: TStringList
      read fStr write fStr;
  end;

  TCollTstDynArray = class(TCollTst)
  private
    fInts: TIntegerDynArray;
    fTimeLog: TTimeLogDynArray;
    fFileVersions: TFVs;
    class procedure FVReader(var Context: TJsonParserContext; Data: pointer);
    class procedure FVWriter(W: TTextWriter; Data: pointer;
      Options: TTextWriterWriteObjectOptions);
    class procedure FVReader2(var Context: TJsonParserContext; Data: pointer);
    class procedure FVWriter2(W: TTextWriter; Data: pointer;
      Options: TTextWriterWriteObjectOptions);
    class procedure FVClassReader(var Context: TJsonParserContext;
       Value: TObject);
    class procedure FVClassWriter(W: TTextWriter; Value: TObject;
      Options: TTextWriterWriteObjectOptions);
  published
    property Ints: TIntegerDynArray
      read fInts write fInts;
    property TimeLog: TTimeLogDynArray
      read fTimeLog write fTimeLog;
    property FileVersion: TFVs
      read fFileVersions write fFileVersions;
  end;



implementation

{ TOrmTest }

procedure TOrmTest.SetInt(const Value: int64);
begin
  fInt := Value;
end;

procedure TOrmTest.FillWith(i: Integer);
begin
  Int := i;
  Test := Int32ToUtf8(i);
  Ansi := WinAnsiString(Test);
  Unicode := WinAnsiToRawUnicode(Ansi);
  ValFloat := i * 2.5;
  ValWord := i;
  ValDate := i + 30000;
  Data := Test;
{$ifndef NOVARIANTS}
  ValVariant := _ObjFast(['id', i]);
{$endif}
end;

procedure TOrmTest.CheckWith(test: TSynTestCase; i: Integer; offset: integer;
  checkblob: boolean);
begin
  test.Check(i <> 0);
  test.CheckUtf8(ID = i, 'id=%=%', [ID, i]);
  test.Check(Int = i);
  test.Check(GetInteger(pointer(self.Test)) = i);
  test.Check(Ansi = WinAnsiString(self.Test));
  test.Check(Unicode = WinAnsiToRawUnicode(Ansi));
  test.Check(ValFloat = i * 2.5);
  test.Check(ValWord = (i + offset) and $ffff);
  test.Check(ValDate = i + 30000);
  if checkblob then
    test.Check(Data = self.Test);
{$ifndef NOVARIANTS}
  test.Check(DocVariantType.IsOfType(ValVariant), 'var1');
  test.Check(VariantSaveJson(ValVariant) = '{"id":' + self.Test + '}', 'var2');
{$endif}
end;


{ TTestCoreProcess }

procedure TTestCoreProcess.Variants;
var
  v: Variant;
  vd: TVarData absolute v;
  t: pointer;
  dt: TDateTime;
  ni: TNullableInteger;
  nt: TNullableUtf8Text;
begin
  t := nil; // makes the compiler happy
  ValueVarToVariant(nil, 0, oftBoolean, vd, false, t);
  Check(not boolean(v));
  ValueVarToVariant('0', 1, oftBoolean, vd, false, t);
  Check(not boolean(v));
  ValueVarToVariant('false', 5, oftBoolean, vd, false, t);
  Check(not boolean(v));
  ValueVarToVariant('1', 1, oftBoolean, vd, false, t);
  Check(boolean(v));
  ValueVarToVariant('true', 4, oftBoolean, vd, false, t);
  Check(boolean(v));
  GetVariantFromJson('0', False, v, nil);
  Check(vd.VType = varInteger);
  Check(v = 0);
  GetVariantFromJson('123', False, v, nil);
  Check(vd.VType = varInteger);
  Check(v = 123);
  GetVariantFromJson('0123', False, v, nil);
  Check(vd.VType = varString);
  GetVariantFromJson('-123', False, v, nil);
  Check(vd.VType = varInteger);
  Check(v = -123);
  GetVariantFromJson('123456789', False, v, nil);
  Check(vd.VType = varInteger);
  Check(v = 123456789);
  GetVariantFromJson('9876543210', False, v, nil);
  Check(vd.VType = varInt64);
  Check(v = 9876543210);
  GetVariantFromJson('12345678901', False, v, nil);
  Check(vd.VType = varInt64);
  Check(v = 12345678901);
  GetVariantFromJson('12345678901234567', False, v, nil);
  Check(vd.VType = varInt64);
  GetVariantFromJson('123456789012345678', False, v, nil);
  Check(vd.VType = varInt64);
  Check(v = 123456789012345678);
  GetVariantFromJson('1234567890123456789', False, v, nil);
  Check(vd.VType = varInt64);
  Check(v = 1234567890123456789);
  GetVariantFromJson('12345678901234567890', False, v, nil, true);
  Check(vd.VType = varDouble);
  CheckSame(vd.VDouble, 12345678901234567890.0, 0);
  GetVariantFromJson('12345678901234567890', False, v, nil, false);
  Check(vd.VType = varString);
  GetVariantFromJson('-123.1', False, v, nil);
  Check(vd.VType = varCurrency);
  Check(v = -123.1);
  GetVariantFromJson('-123.12', False, v, nil);
  Check(vd.VType = varCurrency);
  Check(v = -123.12);
  GetVariantFromJson('-123.123', False, v, nil);
  Check(vd.VType = varCurrency);
  Check(v = -123.123);
  GetVariantFromJson('123.1234', False, v, nil, false);
  Check(vd.VType = varCurrency);
  Check(v = 123.1234);
  GetVariantFromJson('123.1234', False, v, nil, true);
  Check(vd.VType = varCurrency);
  Check(v = 123.1234);
  GetVariantFromJson('-123.12345', False, v, nil, true);
  Check(vd.VType = varDouble);
  CheckSame(v, -123.12345);
  GetVariantFromJson('-1.123e12', False, v, nil, true);
  Check(vd.VType = varDouble);
  CheckSame(v, -1.123e12);
  GetVariantFromJson('-123.123e-2', False, v, nil, true);
  Check(vd.VType = varDouble);
  CheckSame(v, -123.123e-2);
  GetVariantFromJson('-123.123ee2', False, v, nil, true);
  Check(vd.VType = varString);
  Check(v = '-123.123ee2');
  GetVariantFromJson('1-123.12', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '1-123.12');
  GetVariantFromJson('123.', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.');
  GetVariantFromJson('123.abc', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.abc');
  GetVariantFromJson('123.1abc', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.1abc');
  GetVariantFromJson('123.12a', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.12a');
  GetVariantFromJson('123.123a', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.123a');
  GetVariantFromJson('123.1234a', False, v, nil);
  Check(vd.VType = varString);
  Check(v = '123.1234a');
  Check(VariantToDateTime('2016', dt));
  CheckSame(dt, 42370);
  Check(VariantToDateTime(2016, dt));
  CheckSame(dt, 42370);
  Check(VariantToDateTime('1982/10/30', dt));
  CheckSame(dt, 30254);
  Check(not VariantToDateTime('201a', dt));
  ni := NullableIntegerNull;
  Check(NullableIntegerIsEmptyOrNull(ni));
  ni := NullableInteger(10);
  Check(not NullableIntegerIsEmptyOrNull(ni));
  Check(NullableIntegerToValue(ni) = 10);
  nt := NullableUtf8TextNull;
  Check(NullableUtf8TextIsEmptyOrNull(nt));
  nt := NullableUtf8Text('toto');
  Check(not NullableUtf8TextIsEmptyOrNull(nt));
  Check(NullableUtf8TextToValue(nt) = 'toto');
  {$ifndef FPC} // FPC does not allow to mix variant derivated types
  Check(ni = 10);
  Check(nt = 'toto');
  {$endif FPC}
  JsonToVariantInPlace(v, nil);
  Check(vd.VType = varEmpty);
  v := JsonToVariant('');
  Check(vd.VType = varEmpty);
  v := JsonToVariant('null');
  Check(vd.VType = varNull);
  v := JsonToVariant('false');
  Check(not boolean(v));
  v := JsonToVariant('true');
  Check(boolean(v));
  v := JsonToVariant('invalid');
  Check(vd.VType = varNull);
  v := JsonToVariant('0');
  Check(vd.VType = varInteger);
  v := JsonToVariant('123456789012345678');
  Check(vd.VType = varInt64);
  Check(v = 123456789012345678);
  v := JsonToVariant('123.1234');
  Check(vd.VType = varCurrency);
  CheckSame(v, 123.1234);
  v := JsonToVariant('-1E-300', [], true);
  Check(vd.VType = varDouble);
  CheckSame(v, -1e-300);
  v := JsonToVariant('[]');
  Check(v._kind = ord(dvArray));
  Check(v._count = 0);
  v := JsonToVariant('[ ]');
  Check(v._kind = ord(dvArray));
  Check(v._count = 0);
  v := JsonToVariant('{  }');
  Check(v._kind = ord(dvObject));
  Check(v._count = 0);
  v := JsonToVariant('[1,2,3]');
  Check(v._kind = ord(dvArray));
  Check(v._count = 3);
  v := JsonToVariant(' {"a":10,b:20}');
  Check(v._kind = ord(dvObject));
  Check(v._count = 2);
  v := JsonToVariant('{"invalid":');
  Check(vd.VType = varEmpty);
  v := JsonToVariant(' "toto\r\ntoto"');
  Check(vd.VType = varString);
  Check(v = 'toto'#$D#$A'toto');
end;

type
  TMustacheTest = packed record
    desc: string;
    template, expected: RawUtf8;
    data, partials: variant;
  end;

  TMustacheTests = packed record
    tests: array of TMustacheTest;
  end;

const
  __TMustacheTest = 'desc string template,expected RawUtf8 data,partials variant';
  __TMustacheTests = 'tests array of TMustacheTest';
  MUSTACHE_SPECS: array[0..4] of TFileName = (
    'interpolation', 'comments', 'sections', 'inverted', 'partials');

procedure TTestCoreProcess.MustacheRenderer;
var
  mustacheJson: RawByteString;
  mus: TMustacheTests;
  mustache: TSynMustache;
  mustacheJsonFileName: TFileName;
  doc: variant;
  html: RawUtf8;
  helpers: TSynMustacheHelpers;
  guid: TGUID;
  spec, i: integer;
begin
  // manual tests
  mustache := TSynMustache.Parse(
    'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
  Check(mustache.SectionMaxCount = 0);
  TDocVariant.New(doc);
  doc.name := 'Chris';
  doc.value := 10000;
  html := mustache.Render(doc);
  Check(html = 'Hello Chris'#13#10'You have just won 10000 dollars!');
  mustache := TSynMustache.Parse(
    '{{=<% %>=}}Hello <%name%><%={{ }}=%>'#13#10'You have just won {{& value }} dollars!');
  Check(mustache.SectionMaxCount = 0);
  doc := _ObjFast(['name', 'Chris', 'value', 1000]);
  html := mustache.Render(doc);
  Check(html = 'Hello Chris'#13#10'You have just won 1000 dollars!');
  mustache := TSynMustache.Parse(
    'Hello {{value.name}}'#13#10'You have just won {{value.value}} dollars!');
  Check(mustache.SectionMaxCount = 0);
  html := mustache.RenderJson(
    '{value:{name:"Chris",value:10000}}');
  Check(html = 'Hello Chris'#13#10'You have just won 10000 dollars!');
  mustache := TSynMustache.Parse(
    '* {{name}}'#13#10'* {{age}}'#13#10'* {{company}}'#13#10'* {{{company}}}');
  Check(mustache.SectionMaxCount = 0);
  html := mustache.RenderJson(
    '{name:"Chris",company:"<b>Synopse</b>"}');
  Check(html =
    '* Chris'#13#10'* '#13#10'* &lt;b&gt;Synopse&lt;/b&gt;'#13#10'* <b>Synopse</b>');
  mustache := TSynMustache.Parse(
    '* {{name}}'#13#10'* {{age}}'#13#10'* {{company}}'#13#10'* {{&company}}');
  Check(mustache.SectionMaxCount = 0);
  html := mustache.RenderJson(
    '{name:"Chris",company:"<b>Synopse</b>"}');
  Check(html =
    '* Chris'#13#10'* '#13#10'* &lt;b&gt;Synopse&lt;/b&gt;'#13#10'* <b>Synopse</b>');
  mustache := TSynMustache.Parse(
    'Shown.{{#person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:false}');
  Check(html = 'Shown.end');
  mustache := TSynMustache.Parse(
    'Shown.{{#person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:true}');
  Check(html = 'Shown.Also shown!end');
  html := mustache.RenderJson('{person:"toto"}');
  Check(html = 'Shown.Also shown!end');
  html := mustache.RenderJson('{person:false}');
  Check(html = 'Shown.end');
  mustache := TSynMustache.Parse(
    'Shown.{{#person}}As {{name}}!{{/person}}end{{name}}');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:{age:10,name:"toto"}}');
  Check(html = 'Shown.As toto!end');
  mustache := TSynMustache.Parse(
    'Shown.{{^person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:true}');
  Check(html = 'Shown.end');
  mustache := TSynMustache.Parse(
    'Shown.{{^person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:{age:10,name:"toto"}}');
  Check(html = 'Shown.end');
  mustache := TSynMustache.Parse(
    'Shown.{{^person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person:false}');
  Check(html = 'Shown.Also shown!end');
  mustache := TSynMustache.Parse(
    'Shown.{{^person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson('{person2:2}');
  Check(html = 'Shown.Also shown!end');
  Check({%H-}helpers = nil, 'compiler initialized');
  mustache.HelperAdd(helpers, 'jsonhelper', MustacheHelper);
  mustache := TSynMustache.Parse(
    '{{jsonhelper {a:"a",b:10}}}');
  html := mustache.RenderJson('', nil, helpers);
  Check(html = 'a=a,b=10');
  mustache := TSynMustache.Parse(
    '{{jsonhelper {a:"b",b:10} }}');
  html := mustache.RenderJson('', nil, helpers);
  Check(html = 'a=b,b=10');
  mustache := TSynMustache.Parse(
    '{{{jsonhelper {a:"a",b:1}}}}');
  html := mustache.RenderJson('', nil, helpers);
  check(html = 'a=a,b=1');
  mustache := TSynMustache.Parse(
    '{{jsonhelper {a:1,b:2} }},titi');
  html := mustache.RenderJson('', nil, helpers);
  Check(html = 'a=1,b=2,titi');
  mustache := TSynMustache.Parse(
    '{{jsonhelper {a:1,nested:{c:{d:[1,2]}},b:10}}}}toto');
  html := mustache.RenderJson('', nil, helpers);
  Check(html = 'a=1,b=10}toto');
  mustache := TSynMustache.Parse(
    '{{#a}}'#$A'{{one}}'#$A'{{/a}}'#$A);
  html := mustache.RenderJson('{a:{one:1}}');
  Check(html = '1'#$A);
  mustache := TSynMustache.Parse(
    '{{#a}}{{one}}{{#b}}{{one}}{{two}}{{/b}}{{/a}}');
  html := mustache.RenderJson('{a:{one:1},b:{two:2}}');
  Check(html = '112');
  mustache := TSynMustache.Parse(
    '{{>partial}}'#$A'3');
  html := mustache.RenderJson('{}', TSynMustachePartials.CreateOwned(['partial',
    '1'#$A'2']));
  Check(html = '1'#$A'23', 'external partials');
  mustache := TSynMustache.Parse(
    '{{<partial}}1'#$A'2{{name}}{{/partial}}{{>partial}}4');
  html := mustache.RenderJson('{name:3}');
  Check(html = '1'#$A'234', 'internal partials');
  mustache := TSynMustache.Parse(
    'My favorite things:'#$A'{{#things}}{{-index}}. {{.}}'#$A'{{/things}}');
  Check(mustache.SectionMaxCount = 1);
  html := mustache.RenderJson(
    '{things:["Peanut butter", "Pen spinning", "Handstands"]}');
  Check(html = 'My favorite things:'#$A'1. Peanut butter'#$A'2. Pen spinning'#$A
    + '3. Handstands'#$A, '-index pseudo variable');
  mustache := TSynMustache.Parse(
    '{{#things}}{{.}}{{/things}}');
  html := mustache.RenderJson('{things:["one", "two", "three"]}');
  check(html = 'onetwothree');
  mustache := TSynMustache.Parse(
    '{{#things}}{{#-first}}{{.}}{{/-first}}{{/things}} {{pi}}');
  html := mustache.RenderJson('{things:["one", "two", "three"],pi:3.1415}');
  check(html = 'one 3.1415');
  mustache := TSynMustache.Parse(
    '{{#things}}{{^-first}}, {{/-first}}{{.}}{{/things}}');
  html := mustache.RenderJson('{things:["one", "two", "three"]}');
  check(html = 'one, two, three');
  mustache := TSynMustache.Parse(
    '{{#things}}{{.}}{{^-last}}, {{/-last}}{{/things}}');
  html := mustache.RenderJson('{things:["one", "two", "three"]}');
  check(html = 'one, two, three');
  mustache := TSynMustache.Parse(
    '{{#things}}{{#-last}}{{.}}{{/-last}}{{/things}}');
  html := mustache.RenderJson('{things:["one", "two", "three"]}');
  check(html = 'three');
  mustache := TSynMustache.Parse(
    '{{#things}}{{#-odd}}{{.}}{{/-odd}}{{/things}}');
  html := mustache.RenderJson('{things:["one", "two", "three"]}');
  check(html = 'onethree');
  mustache := TSynMustache.Parse(
    '{{"Hello}} {{name}}'#13#10'{{"You have just won}} {{value}} {{"dollars}}!');
  Check(mustache.SectionMaxCount = 0);
  html := mustache.RenderJson('{name:?,value:?}', [], ['Chris', 10000], nil, nil,
    MustacheTranslate);
  Check(html = 'Bonjour Chris'#$D#$A'Vous venez de gagner 10000 dollars!');
  mustache := TSynMustache.Parse(
    '1+3={{tval}} - is it 4?{{#if tval=4}} yes!{{/if}}');
  html := mustache.RenderJson('{tval:4}', nil, TSynMustache.HelpersGetStandardList);
  check(html = '1+3=4 - is it 4? yes!');
  html := mustache.RenderJson('{tval:5}', nil, TSynMustache.HelpersGetStandardList);
  check(html = '1+3=5 - is it 4?');
  mustache := TSynMustache.Parse(
    '{{newguid}}');
  html := mustache.RenderJson('{}', nil, TSynMustache.HelpersGetStandardList);
  check((html <> '') and
        (TextToGuid(@html[2], @guid) <> nil));
  mustache := TSynMustache.Parse(
    '<h1>{{header}}</h1>'#$D#$A'{{#items}}'#$D#$A'{{#first}}'#$D#$A +
    '<li><strong>{{name}}</strong></li>'#$D#$A'{{/first}}'#$D#$A +
    '{{#link}}'#$D#$A'<li><a href="{{url}}">{{name}}</a></li>'#$D#$A'{{/link}}'#$D#$A +
    '{{/items}}'#$D#$A#$D#$A'{{#empty}}'#$D#$A'<p>The list is empty.</p>'#$D#$A'{{/empty}}');
  Check(mustache.SectionMaxCount = 2);
  html := mustache.RenderJson(
    '{"header":"Colors","items":[{"name":"red","first":true,"url":"#Red"},' +
    '{"name":"green","link":true,"url":"#Green"},{"name":"blue","first":true,' +
    '"link":true,"url":"#Blue"}],"empty":true}');
  Check(TrimU(html) =
    '<h1>Colors</h1>'#$D#$A'<li><strong>red</strong></li>'#$D#$A +
    '<li><a href="#Green">green</a></li>'#$D#$A'<li><strong>blue</strong></li>'#$D#$A +
    '<li><a href="#Blue">blue</a></li>'#$D#$A#$D#$A'<p>The list is empty.</p>');
  mustache := TSynMustache.Parse(
    '{{#users}}'#$D#$A'{{^Connected}}'#$D#$A +
    '- {{Name}} {{Firstname}} ({{Connected}})<BR>'#$D#$A'{{/Connected}}'#$D#$A'{{/users}}');
  Check(mustache.SectionMaxCount = 2);
  html := mustache.RenderJson('{"users":[' +
    '{"RowID":1,"Login":"safr","Firstname":"Frodon","Name":"Sacquet","Alias":"safr","Connected":true,"Resto":0},' +
    #13#10 +
    '{"RowID":2,"Login":"saga","Firstname":"Samsagace","Name":"Gamegie","Alias":"saga","Connected":false,"Resto":0},' +
    #13#10 +
    '{"RowID":3,"Login":"peto","Firstname":"Peregrin","Name":"Touque","Alias":"peto","Connected":false,"Resto":0},' +
    #13#10 +
    '{"RowID":4,"Login":"mebr","Firstname":"Meriadoc","Name":"Brandebouc","Alias":"mebr","Connected":true,"Resto":0}]}');
  check(html =
    '- Gamegie Samsagace (false)<BR>'#$D#$A'- Touque Peregrin (false)<BR>'#$D#$A);

  // run official {{mustache}} regression tests suite
  TRttiJson.RegisterFromText(TypeInfo(TMustacheTest), __TMustacheTest,
    JSONPARSER_TOLERANTOPTIONS, []);
  TRttiJson.RegisterFromText(TypeInfo(TMustacheTests), __TMustacheTests,
    JSONPARSER_TOLERANTOPTIONS, []);
  for spec := 0 to High(MUSTACHE_SPECS) do
  begin
    mustacheJsonFileName := WorkDir + MUSTACHE_SPECS[spec] + '.json';
    mustacheJson := StringFromFile(mustacheJsonFileName);
    if mustacheJson = '' then
    begin
      mustacheJson := HttpGet(
       'https://raw.githubusercontent.com/mustache/spec/' +
       'master/specs/' + StringToAnsi7(MUSTACHE_SPECS[spec]) + '.json');
      FileFromString(mustacheJson, mustacheJsonFileName);
    end;
    RecordLoadJson(mus, pointer(mustacheJson), TypeInfo(TMustacheTests));
    Check(length(mus.tests) > 5);
    for i := 0 to high(mus.tests) do
      with mus.Tests[i] do
      begin
        if desc = 'Dotted names should be resolved against former resolutions.' then
          continue; // we don't handle a":{"b":{}} context (yet)
        if PosEx(' {{>partial}}', template) > 0 then
          continue; // we don't indent each line of the expanded partials (yet)
        mustache := TSynMustache.Parse(template);
        html := mustache.Render(data, TSynMustachePartials.CreateOwned(partials));
        Check(html = expected, desc);
      end;
  end;
  Rtti.RegisterFromText(TypeInfo(TMustacheTest), '');
  Rtti.RegisterFromText(TypeInfo(TMustacheTests), '');
end;

procedure TTestCoreProcess.MustacheTranslate(var English: string);
begin
  if English = 'Hello' then
    English := 'Bonjour'
  else if English = 'You have just won' then
    English := 'Vous venez de gagner';
end;

procedure TTestCoreProcess.MustacheHelper(const Value: variant; out Result: variant);
begin
  with _Safe(Value)^ do
    RawUtf8ToVariant(FormatUtf8('a=%,b=%', [U['a'], i['b']]), Result);
end;



{ TCollTestsI }

class function TCollTestsI.GetClass: TCollectionItemClass;
begin
  result := TCollTest;
end;


{ TCollTstDynArray}

class procedure TCollTstDynArray.FVReader(
    var Context: TJsonParserContext; Data: pointer);
begin
  // '[1,2001,3001,4001,"1","1001"],[2,2002,3002,4002,"2","1002"],...'
  if Context.ParseArray then
    with PFV(Data)^ do
    begin
      Major := GetNextItemCardinal(Context.Json);
      Minor := GetNextItemCardinal(Context.Json);
      Release := GetNextItemCardinal(Context.Json);
      Build := GetNextItemCardinal(Context.Json);
      Main := Context.ParseString;
      Detailed := Context.ParseString;
      Context.ParseEndOfObject;
    end;
end;

class procedure TCollTstDynArray.FVWriter(W: TTextWriter; Data: pointer;
  Options: TTextWriterWriteObjectOptions);
begin
  with PFV(Data)^ do
    W.Add('[%,%,%,%,"%","%"]', [Major, Minor, Release, Build, Main, Detailed],
      twJsonEscape);
end;

class procedure TCollTstDynArray.FVReader2(var Context: TJsonParserContext;
  Data: pointer);
var
  Values: array[0..5] of TValuePUtf8Char;
begin
  // '{"Major":1,"Minor":2001,"Release":3001,"Build":4001,"Main":"1","Detailed":"1001"},..
  if Context.ParseObject([
     'Major', 'Minor', 'Release', 'Build', 'Main', 'Detailed'], @Values) then
    with PFV(Data)^ do
    begin
      Major   := Values[0].ToInteger;
      Minor   := Values[1].ToInteger;
      Release := Values[2].ToInteger;
      Build   := Values[3].ToInteger;
      Main    := Values[4].ToString;
      Detailed := Values[5].ToString;
    end;
end;

class procedure TCollTstDynArray.FVWriter2(W: TTextWriter; Data: pointer;
  Options: TTextWriterWriteObjectOptions);
begin
  with PFV(Data)^ do
    W.AddJsonEscape(['Major', Major,
                     'Minor', Minor,
                     'Release', Release,
                     'Build', Build,
                     'Main', Main,
                     'Detailed', Detailed]);
end;

class procedure TCollTstDynArray.FVClassReader(var Context: TJsonParserContext;
  Value: TObject);
var
  Values: array[0..5] of TValuePUtf8Char;
begin
  // '{"Major":2,"Minor":2002,"Release":3002,"Build":4002,"Main":"2","BuildDateTime":"1911-03-15"}'
  if Context.ParseObject([
     'Major', 'Minor', 'Release', 'Build', 'Main', 'BuildDateTime'], @Values) then
    with TFileVersion(Value) do
    begin
      Major   := Values[0].ToInteger;
      Minor   := Values[1].ToInteger;
      Release := Values[2].ToInteger;
      Build   := Values[3].ToInteger;
      Main    := Values[4].ToString;
      BuildDateTime := Values[5].Iso8601ToDateTime;
    end;
end;

class procedure TCollTstDynArray.FVClassWriter(W: TTextWriter; Value: TObject;
  Options: TTextWriterWriteObjectOptions);
begin
  with TFileVersion(Value) do
    W.AddJsonEscape(['Major',   Major,
                     'Minor',   Minor,
                     'Release', Release,
                     'Build',   Build,
                     'Main',    Main,
                     'BuildDateTime', DateTimeToIso8601Text(BuildDateTime)]);
end;


{ TCollTests }

function TCollTests.Add: TCollTest;
begin
  result := inherited Add as TCollTest;
end;

class function TCollTests.GetClass: TCollectionItemClass;
begin
  result := TCollTest;
end;

function TCollTests.GetCollItem(Index: Integer): TCollTest;
begin
  result := Items[Index] as TCollTest;
end;


{ TCollTst }

constructor TCollTst.Create;
begin
  inherited;
  fColl := TCollTests.Create;
  fTCollTest := TCollTest.Create(nil);
end;

destructor TCollTst.Destroy;
begin
  fColl.Free;
  fTCollTest.Free;
  fStr.Free;
  inherited;
end;

procedure TCollTst.SetColl(const Value: TCollTests);
begin
  fColl.Free;
  fColl := Value;
end;

type
  {$M+} // TPersistent has no RTTI for LVCL!
  TPersistentToJson = class(TPersistent)
  protected
    fName: RawUtf8;
    fEnum: TSynBackgroundThreadProcessStep;
    fSets: TSynBackgroundThreadProcessSteps;
  published
    property Name: RawUtf8
      read fName write fName;
    property Enum: TSynBackgroundThreadProcessStep
      read fEnum write fEnum default flagIdle;
    property Sets: TSynBackgroundThreadProcessSteps
      read fSets write fSets default[];
  end;
  {$M-}

  TRange = record
    Min, Max: Integer;
  end;

  TOffense = record
    Damage, AttackSpeed: TRange;
  end;

  TEnemy = class(TSynPersistent)
  private
    fEnabled: Boolean;
    fName: string;
    function GetOffense: RawJson;
    procedure SetOffense(Value: RawJson);
  public
    Off: TOffense;
  published
    property Enabled: Boolean
      read fEnabled write fEnabled;
    property Name: string
      read fName write fName;
    property Offense: RawJson
      read GetOffense write SetOffense;
  end;

function TEnemy.GetOffense: RawJson;
begin
  result := JsonEncode([
    'damage',
      '{', 'min', Off.Damage.Min,
           'max', Off.Damage.Max,
      '}',
    'attackspeed',
      '{', 'min', Off.AttackSpeed.Min,
           'max', Off.AttackSpeed.Max,
      '}']);
end;

procedure RangeFromJson(out Range: TRange; Json: PUtf8Char);
var
  V: array[0..1] of TValuePUtf8Char;
begin
  JsonDecode(Json, ['min', 'max'], @V);
  Range.Min := V[0].ToInteger;
  Range.Max := V[1].ToInteger;
end;

procedure TEnemy.SetOffense(Value: RawJson);
var
  V: array[0..1] of TValuePUtf8Char;
begin
  JsonDecode(Value, ['damage', 'attackspeed'], @V, true);
  RangeFromJson(Off.Damage, V[0].Value);
  RangeFromJson(Off.AttackSpeed, V[1].Value);
end;

type
  TTestCustomJsonRecord = packed record
    A, B, C: integer;
    D: RawUtf8;
    E: record
      E1, E2: double;
    end;
    F: TDateTime;
  end;

  TTestCustomJsonArray = packed record
    A, B, C: byte;
    D: RawByteString;
    E: array of record
      E1: double;
      E2: string;
    end;
    F: TDateTime;
  end;

  TTestCustomJsonArrayWithoutF = packed record
    A, B, C: byte;
    D: RawByteString;
    E: array of record
      E1: double;
      E2: string;
    end;
  end;

  TTestCustomJsonArraySimpleArray = packed record
    F: RawUtf8;
    G: array of RawUtf8;
  end;

  TTestCustomJsonArraySimple = packed record
    A, B: Int64;
    C: array of TGUID;
    D: RawUtf8;
    E: array of TTestCustomJsonArraySimpleArray;
    H: RawUtf8;
  end;

  TTestCustomJsonArrayVariant = packed record
    A, B: Int64;
    C: array of variant;
    D: RawUtf8;
  end;

  TTestCustomJsonGitHub = packed record
    name: RawUtf8;
    id: cardinal;
    description: RawUtf8;
    fork: boolean;
    owner: record
      login: RawUtf8;
      id: currency;
    end;
  end;

  TTestCustomJsonGitHubs = array of TTestCustomJsonGitHub;

  TTestCustomJson2Title = packed record
    TITYPE, TIID, TICID, TIDSC30, TIORDER, TIDEL: RawUtf8;
  end;

  TTestCustomJson2Trans = packed record
    TRTYPE: RawUtf8;
    TRDATE: TDateTime;
    TRAA: RawUtf8;
    TRCAT1, TRCAT2, TRCAT3, TRACID: TTestCustomJson2Title;
    TRRMK: RawUtf8;
  end;

  TTestCustomJson2 = packed record
    Transactions: array of TTestCustomJson2Trans;
  end;

  TTestCustomDiscogs = packed record
    pagination: record
      per_page, items, page: Integer;
    end;
    releases: array of record
      status, title, format, _label, artist: RawUtf8; // label is a keyword
      year, id: Integer;
    end;
  end;

  TSubAB = packed record
    a: RawUtf8;
    b: integer;
  end;

  TSubCD = packed record
    c: byte;
    d: RawUtf8;
  end;

  TAggregate = packed record
    abArr: array of TSubAB;
    cdArr: array of TSubCD;
  end;

  TNestedDtoObject = class(TSynAutoCreateFields)
  private
    FFieldString: RawUtf8;
    FFieldInteger: integer;
    FFieldVariant: variant;
  published
    property FieldString: RawUtf8
      read FFieldString write FFieldString;
    property FieldInteger: integer
      read FFieldInteger write FFieldInteger;
    property FieldVariant: variant
      read FFieldVariant write FFieldVariant;
  end;

  TDtoObject = class(TSynAutoCreateFields)
  private
    FFieldNestedObject: TNestedDtoObject;
    FSomeField: RawUtf8;
  published
    property NestedObject: TNestedDtoObject
      read FFieldNestedObject;
    property SomeField: RawUtf8
      read FSomeField write FSomeField;
  end;

  {$ifdef ISDELPHI2010}
  TStaticArrayOfInt = packed array[1..5] of Integer;

  TNewRtti = record
    Number: integer;
    StaticArray: array[1..2] of record
      Name: string;
      Single: Single;
      Double: Double;
    end;
    Int: TStaticArrayOfInt;
  end;

  TBookRecord = packed record
    name: string;
    author: record
      first_name: string;
      last_name: string;
    end;
  end;
  {$endif ISDELPHI2010}

const
  // convention may be to use __ or _ before the type name
  __TTestCustomJsonRecord: RawUtf8 =
      'A,B,C integer D RawUtf8 E{E1,E2 double} F TDateTime';
  __TTestCustomJsonArray: RawUtf8 =
      'A,B,C byte D RawByteString E[E1 double E2 string] F TDateTime';
  __TTestCustomJsonArraySimple =
      'A,B Int64 C array of TGUID D RawUtf8 E [F RawUtf8 G array of RawUtf8] H RawUtf8';
  __TTestCustomJsonArrayVariant =
      'A,B Int64 C array of variant D RawUtf8';
  __TTestCustomJsonGitHub =
      'name RawUtf8 id cardinal description RawUtf8 ' +
     'fork boolean owner{login RawUtf8 id currency}';
  __TTestCustomJson2Title =
      'TITYPE,TIID,TICID,TIDSC30,TIORDER,TIDEL RawUtf8';
  __TTestCustomJson2 =
    'Transactions [TRTYPE RawUtf8 TRDATE TDateTime TRAA RawUtf8 ' +
    'TRCAT1,TRCAT2,TRCAT3,TRACID TTestCustomJson2Title ' +
    'TRRMK RawUtf8]';
  __TTestCustomDiscogs =
    'pagination{per_page,items,page Integer}' +
    'releases[status,title,format,label,artist RawUtf8 year,id integer]';
  __TRestCacheEntryValue =
    'ID: Int64; Timestamp512,Tag: cardinal; Json: RawUtf8';
  __TSubAB =
    'a : RawUtf8; b : integer;';
  __TSubCD =
    'c : byte; d : RawUtf8;';
  __TAggregate =
    'abArr : array of TSubAB; cdArr : array of TSubCD;';

  zendframeworkFileName = 'zendframework.json';
  discogsFileName = 'discogs.json';


procedure TTestCoreProcess.EncodeDecodeJSON;
var
  J, U, U2: RawUtf8;
  P: PUtf8Char;
  binary, zendframeworkJson, discogsJson: RawByteString;
  V: array[0..4] of TValuePUtf8Char;
  i, a, err: integer;
  r: Double;
  Parser: TRttiCustom;
  JR, JR2: TTestCustomJsonRecord;
  JA, JA2: TTestCustomJsonArray;
  JAS: TTestCustomJsonArraySimple;
  JAV: TTestCustomJsonArrayVariant;
  GDtoObject: TDtoObject;
  Trans: TTestCustomJson2;
  Disco: TTestCustomDiscogs;
  Cache: TRestCacheEntryValue;
  peop: TOrmPeople;
  K: RawUtf8;
  Valid: boolean;
  RB: RawBlob;
  Enemy: TEnemy;
  Instance: TRttiCustom;
  Coll, C2: TCollTst;
  MyItem: TCollTest;
  Comp: TComplexNumber;
  DA: TDynArray;
  F: TFV;
  TLNow: TTimeLog;

  procedure TestMyColl(MyColl: TMyCollection);
  begin
    if CheckFailed(MyColl <> nil) then
      exit;
    MyItem := MyColl.Add as TCollTest;
    Check(MyItem.ClassType = TCollTest);
    MyItem.Length := 10;
    MyItem.Color := 20;
    MyItem.Name := 'ABC';
    U := ObjectToJson(MyColl);
    CheckEqual(U, '[{"Color":20,"Length":10,"Name":"ABC"}]');
    MyColl.Free;
  end;

  procedure TCollTstDynArrayTest;
  var
    CA: TCollTstDynArray;
    i: integer;
    tmp: RawByteString;
    pu: PUtf8Char;
  begin
    CA := TCollTstDynArray.Create;
    try
      CA.Str := TStringList.Create;
      tmp := J;
      Check(JsonToObject(CA, UniqueRawUtf8(RawUtf8(tmp)), Valid)^ = #0);
      Check(Valid);
      Check(CA.One.Color = 2);
      Check(CA.One.Name = 'test2');
      if not CheckFailed(CA.Coll.Count = 1) then
        Check(CA.Coll[0].Name = 'test');
      Check(CA.One.Length = 10);
      Check(CA.Str.Count = 10000);
      for i := 1 to CA.Str.Count do
        Check(StrToInt(CA.Str[i - 1]) = i);
      SetLength(CA.fInts, 20000);
      for i := 0 to high(CA.Ints) do
        CA.Ints[i] := i;
      U := ObjectToJson(CA);
      check(IsValidJson(U));
    finally
      CA.Free;
    end;
    CA := TCollTstDynArray.Create;
    try
      CA.Str := TStringList.Create;
      Check(JsonToObject(CA, pointer(U), Valid)^ = #0);
      Check(Valid);
      Check(CA.Str.Count = 10000);
      for i := 1 to CA.Str.Count do
        Check(StrToInt(CA.Str[i - 1]) = i);
      Check(length(CA.Ints) = 20000);
      for i := 0 to high(CA.Ints) do
        CA.Ints[i] := i;
      SetLength(CA.fTimeLog, CA.Str.Count);
      TLNow := TimeLogNow and (not 63);
      for i := 0 to high(CA.TimeLog) do
        CA.TimeLog[i] := TLNow + i and 31; // and 31 to avoid min:sec rounding
      U := ObjectToJson(CA);
      SetLength(CA.fInts, 2);
      SetLength(CA.fTimeLog, 2);
      Check(JsonToObject(CA, pointer(U), Valid)^ = #0);
      Check(Valid);
      Check(Length(CA.Ints) = 20000);
      Check(Length(CA.TimeLog) = CA.Str.Count);
      for i := 0 to high(CA.Ints) do
        Check(CA.Ints[i] = i);
      for i := 0 to high(CA.TimeLog) do
        Check(CA.TimeLog[i] = TLNow + i and 31);
      DA.Init(TypeInfo(TFVs), CA.fFileVersions);
      for i := 1 to 1000 do
      begin
        F.Major := i;
        F.Minor := i + 2000;
        F.Release := i + 3000;
        F.Build := i + 4000;
        F.Main := IntToStr(i);
        F.Detailed := IntToStr(i + 1000);
        DA.Add(F);
      end;
      U := ObjectToJson(CA);
      check(IsValidJson(U));
      DA.Clear;
      Check(Length(CA.FileVersion) = 0);
      pu := JsonToObject(CA, pointer(U), Valid);
      Check((pu <> nil) and
            (pu^ = #0));
      Check(Valid);
      Check(Length(CA.Ints) = 20000);
      Check(Length(CA.TimeLog) = CA.Str.Count);
      Check(Length(CA.FileVersion) = 1000);
      for i := 1 to 1000 do
        with CA.FileVersion[i - 1] do
        begin
          Check(Major = i);
          Check(Minor = i + 2000);
          Check(Release = i + 3000);
          Check(Build = i + 4000);
          Check(StrToInt(Main) = i);
          Check(StrToInt(Detailed) = i + 1000);
        end;
    finally
      CA.Free;
    end;
  end;

  procedure TFileVersionTest(Full: boolean);
  var
    V, F: TFileVersion;
    J: RawUtf8;
    i: integer;
    Valid: boolean;
  begin
    V := TFileVersion.Create('', 0, 0, 0, 0);
    F := TFileVersion.Create('', 0, 0, 0, 0);
    try
      for i := 1 to 1000 do
      begin
        if Full then
        begin
          V.Major := i;
          V.Minor := i + 2000;
          V.Release := i + 3000;
          V.Build := i + 4000;
          V.Main := IntToStr(i);
        end;
        V.BuildDateTime := 4090.0 + i;
        J := ObjectToJson(V);
        check(IsValidJson(J));
        JsonToObject(F, pointer(J), Valid);
        if CheckFailed(Valid) then
          continue;
        if Full then
        begin
          Check(F.Major = i);
          Check(F.Minor = V.Minor);
          Check(F.Release = V.Release);
          Check(F.Build = V.Build);
          Check(F.Main = V.Main);
        end;
        CheckSame(V.BuildDateTime, F.BuildDateTime);
      end;
    finally
      F.Free;
      V.Free;
    end;
  end;

  procedure ABCD;
  begin
    Check(Parser.Props.List[0].Name = 'A');
    Check(Parser.Props.List[0].Value.Parser = ptInteger);
    Check(Parser.Props.List[1].Name = 'B');
    Check(Parser.Props.List[1].Value.Parser = ptInteger);
    Check(Parser.Props.List[2].Name = 'C');
    Check(Parser.Props.List[2].Value.Parser = ptInteger);
    Check(Parser.Props.List[3].Name = 'D');
    Check(Parser.Props.List[3].Value.Parser = ptRawUtf8);
  end;

  procedure ABCDE(pt: TRttiParserType);
  var
    p: PRttiCustomProp;
    v: TRttiCustom;
  begin
    ABCD;
    p := @Parser.Props.List[4];
    Check(p^.Name = 'E');
    Check(p^.Value.Parser = pt);
    if pt = ptDynArray then
      v := p^.Value.ArrayRtti
    else
      v := p^.Value;
    Check(v.Props.Count = 2);
    Check(v.Props.List[0].Name = 'E1');
    Check(v.Props.List[0].Value.Parser = ptDouble);
    Check(v.Props.List[1].Name = 'E2');
    Check(v.Props.List[1].Value.Parser = ptDouble);
  end;

  procedure TestGit(ro: TJsonParserOptions; wo: TTextWriterWriteObjectOptions);
  var
    i: PtrInt;
    U: RawUtf8;
    s: RawJson;
    git, git2: TTestCustomJsonGitHubs;
    item, value: PUtf8Char;
  begin
    if zendframeworkJson = '' then
      exit; // avoid GPF e.g. on Windows XP where https is broken
    TRttiJson.RegisterFromText(TypeInfo(TTestCustomJsonGitHub),
      __TTestCustomJsonGitHub, ro, wo);
    FillCharFast(git, sizeof(git), 0);
    FillCharFast(git2, sizeof(git2), 0);
    U := zendframeworkJson; // need unique string for procedure re-entrance
    check(IsValidJson(U));
    Check(DynArrayLoadJson(
      git, UniqueRawUtf8(U), TypeInfo(TTestCustomJsonGitHubs)) <> nil);
    U := DynArraySaveJson(git, TypeInfo(TTestCustomJsonGitHubs));
    check(IsValidJson(U));
    if woHumanReadable in wo then
      FileFromString(U, WorkDir + 'zendframeworkSaved.json');
    Check(length(git) >= 30);
    Check(length(U) > 3000);
    if git[0].id = 8079771 then
    begin
      Check(git[0].name = 'Component_ZendAuthentication');
      Check(git[0].description = 'Authentication component from Zend Framework 2');
      Check(git[0].owner.login = 'zendframework');
      Check(git[0].owner.id = 296074);
    end;
    for i := 0 to high(git) do
      with git[i] do
      begin
        item := JsonArrayItem(Pointer(U), i);
        Check(item <> nil);
        value := JsonObjectItem(item, 'name');
        check(value <> nil);
        GetJsonItemAsRawJson(value, s);
        check(IsValidJson(s));
        check(TrimU(s) = '"' + name + '"');
        check(GetInteger(JsonObjectByPath(item, 'owner.id')) = owner.id);
        check(GetInteger(JsonObjectByPath(item, 'owner.i*')) = owner.id);
        check(JsonObjectByPath(item, 'owner.name') = '');
        check(JsonObjectsByPath(item, 'toto') = '');
        check(JsonObjectsByPath(item, 'toto,titi') = '');
        check(JsonObjectsByPath(item, 'toto,name') = '{"name":"' + name + '"}');
        check(JsonObjectsByPath(item, 'toto,n*') = '{"name":"' + name + '"}');
        check(JsonObjectsByPath(item, 'fork,toto,owner.id,name') =
          FormatUtf8('{"fork":%,"owner.id":%,"name":"%"}',
            [BOOL_STR[fork], owner.id, name]));
        check(JsonObjectsByPath(item, 'owner.i*') =
          FormatUtf8('{"owner.id":%}', [owner.id]));
        check(JsonObjectsByPath(item, 'owner.*') =
          FormatUtf8('{"owner.login":"%","owner.id":%}',
            [owner.login, owner.id]));
        value := JsonObjectByPath(item, 'owner');
        GetJsonItemAsRawJson(value, s);
        check(IsValidJson(s));
        check(JsonReformat(s, jsonCompact) =
          FormatUtf8('{"login":"%","id":%}', [owner.login, owner.id]));
      end;
    Check(DynArrayLoadJson(
      git2, pointer(U), TypeInfo(TTestCustomJsonGitHubs)) <> nil);
    if not CheckFailed(length(git) = Length(git2)) then
      for i := 0 to high(git) do
      begin
        Check(git[i].name = git2[i].name);
        Check(git[i].id = git2[i].id);
        Check(git[i].description = git2[i].description);
        Check(git[i].fork = git2[i].fork);
        Check(git[i].owner.login = git2[i].owner.login);
        Check(git[i].owner.id = git2[i].owner.id);
      end;
    Rtti.RegisterFromText(TypeInfo(TTestCustomJsonGitHub), '');
  end;

  procedure TestTrans;
  begin
    Check(IsValidJson(U));
    RecordZero(@Trans, TypeInfo(TTestCustomJson2));
    Check(length(Trans.Transactions) = 0);
    RecordLoadJson(Trans, UniqueRawUtf8(U), TypeInfo(TTestCustomJson2));
    Check(length(Trans.Transactions) = 1);
    Check(Trans.Transactions[0].TRTYPE = 'INCOME');
    Check(Trans.Transactions[0].TRACID.TIDEL = 'false');
    Check(Trans.Transactions[0].TRRMK = 'Remark');
  end;

  function uct(const s: RawUtf8): TOrmFieldType;
  begin
    result := Utf8ContentNumberType(pointer(s));
  end;

var
  O, O2: TPersistentToJson;
  E: TSynBackgroundThreadProcessStep;
  EndOfObject: AnsiChar;
var
  Va, Vb: Variant;
  c: currency;

  procedure TestJSONSerialization;
  var
    ab0, ab1: TSubAB;
    cd0, cd1, cd2: TSubCD;
    agg, agg2: TAggregate;
    X: RawUtf8;
    AA, AB: TRawUtf8DynArrayDynArray;
    i, a, v: PtrInt;
    {$ifdef ISDELPHI2010}
    nav, nav2: TConsultaNav;
    nrtti, nrtti2: TNewRtti;
    book: TBookRecord;
    {$endif ISDELPHI2010}
  begin
    Finalize(JR);
    Finalize(JR2);
    Finalize(JA);
    Finalize(JA2);
    FillCharFast(JR, sizeof(JR), 0);
    FillCharFast(JR2, sizeof(JR2), 0);
    FillCharFast(JA, sizeof(JA), 0);
    FillCharFast(JA2, sizeof(JA2), 0);
    U := RecordSaveJson(JR, TypeInfo(TTestCustomJsonRecord));
    CheckEqual(U, '{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0},"F":""}');
    check(IsValidJson(U));
    X := JsonToXML(U, '');
    Check(X = '<A>0</A><B>0</B><C>0</C><D></D><E><E1>0</E1><E2>0</E2></E><F></F>');
    J := JsonToXML(U, '', XMLUTF8_NAMESPACE);
    CheckEqual(J, XMLUTF8_NAMESPACE + X + '</contents>');
    J := RecordSaveJson(JA, TypeInfo(TTestCustomJsonArray));
    CheckEqual(J, '{"A":0,"B":0,"C":0,"D":null,"E":[],"F":""}');
    check(IsValidJson(J));
    X := JsonToXML(J, '');
    Check(X = '<A>0</A><B>0</B><C>0</C><D>null</D><F></F>');
    JR2.A := 10;
    JR2.D := '**';
    JR2.F := 1;
    JR := JR2;
    RecordLoadJson(JR2, pointer(U), TypeInfo(TTestCustomJsonRecord));
    Check(JR2.A = 0);
    Check(JR2.D = '');
    Check(JR2.F = 0);
    U := RecordSaveJson(JR2, TypeInfo(TTestCustomJsonRecord));
    CheckEqual(U, '{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0},"F":""}');
    check(IsValidJson(U));
    U := RecordSaveJson(JR, TypeInfo(TTestCustomJsonRecord));
    CheckEqual(U, '{"A":10,"B":0,"C":0,"D":"**","E":{"E1":0,"E2":0},"F":"1899-12-31"}');
    check(IsValidJson(U));
    JA2.A := 10;
    JA2.D := '**';
    SetLength(JA2.E, 2);
    JA2.F := 1;
    RecordLoadJson(JA2, pointer(J), TypeInfo(TTestCustomJsonArray));
    Check(JA2.A = 0);
    Check(JA2.D = '');
    check(Length(JA2.E) = 0);
    Check(JA2.F = 0);
    J := RecordSaveJson(JA2, TypeInfo(TTestCustomJsonArray));
    CheckEqual(J, '{"A":0,"B":0,"C":0,"D":null,"E":[],"F":""}');
    check(IsValidJson(J));
    JA2.A := 100;
    JA2.F := 1;
    J := RecordSaveJson(JA2, TypeInfo(TTestCustomJsonArray));
    CheckEqual(J, '{"A":100,"B":0,"C":0,"D":null,"E":[],"F":"1899-12-31"}');
    check(IsValidJson(J));
    SetLength(JA2.E, 2);
    JA2.E[0].E1 := 1;
    JA2.E[0].E2 := '2';
    JA2.E[1].E1 := 3;
    JA2.E[1].E2 := '4';
    J := RecordSaveJson(JA2, TypeInfo(TTestCustomJsonArray));
    CheckEqual(J,
      '{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}],"F":"1899-12-31"}');
    check(IsValidJson(J));
    X := JsonToXML(J, '');
    Check(X =
      '<A>100</A><B>0</B><C>0</C><D>null</D><E><E1>1</E1><E2>2</E2></E><E><E1>3</E1><E2>4</E2></E><F>1899-12-31</F>');
    RecordLoadJson(JA, pointer(J), TypeInfo(TTestCustomJsonArray));
    Check(RecordSave(JA, TypeInfo(TTestCustomJsonArray)) = RecordSave(JA2,
      TypeInfo(TTestCustomJsonArray)));
    J := '{"A":0,"B":0,"C":0,"D":null,"E":[{"E1":2,"E2":"3"}],"F":""}';
    check(IsValidJson(J));
    RecordLoadJson(JA, UniqueRawUtf8(J), TypeInfo(TTestCustomJsonArray));
    U := RecordSaveJson(JA, TypeInfo(TTestCustomJsonArray));
    Check(length(JA.E) = 1);
    CheckEqual(U, '{"A":0,"B":0,"C":0,"D":null,"E":[{"E1":2,"E2":"3"}],"F":""}');
    check(IsValidJson(U));
    X := JsonToXML(U, '');
    Check(X = '<A>0</A><B>0</B><C>0</C><D>null</D><E><E1>2</E1><E2>3</E2></E><F></F>');
    X := JsonToXML('[1,2,"three"]');
    Check(X =
      '<?xml version="1.0" encoding="UTF-8"?>'#$D#$A'<0>1</0><1>2</1><2>three</2>');

    SetLength(AA, 100);
    for i := 0 to high(AA) do
    begin
      SetLength(AA[i], Random32(100));
      for a := 0 to high(AA[i]) do
      begin
        UInt32ToUtf8(i + a, AA[i, a]);
        check(IsValidJson(AA[i, a]));
        check(IsValidJson('    ' + AA[i, a]));
        check(IsValidJson(AA[i, a] + '  '));
      end;
    end;
    binary := DynArraySave(AA, TypeInfo(TRawUtf8DynArrayDynArray));
    Check(DynArrayLoad(
      AB, pointer(binary), TypeInfo(TRawUtf8DynArrayDynArray)) <> nil);
    Check(length(AA) = length(AB));
    for i := 0 to high(AA) do
    begin
      Check(length(AA[i]) = length(AB[i]));
      for a := 0 to high(AA[i]) do
        Check(AA[i, a] = AB[i, a]);
    end;
    J := DynArraySaveJson(AA, TypeInfo(TRawUtf8DynArrayDynArray));
    check(IsValidJson(J));
    Finalize(AB);
    Check(DynArrayLoadJson(
      AB, pointer(J), TypeInfo(TRawUtf8DynArrayDynArray)) <> nil);
    Check(length(AA) = length(AB));
    for i := 0 to high(AA) do
    begin
      Check(length(AA[i]) = length(AB[i]));
      for a := 0 to high(AA[i]) do
        Check(AA[i, a] = AB[i, a]);
    end;

    ab0.a := 'AB0';
    ab0.b := 0;
    ab1.a := 'AB1';
    ab1.b := 1;
    cd0.c := 0;
    cd0.d := 'CD0';
    cd1.c := 1;
    cd1.d := 'CD1';
    cd2.c := 2;
    cd2.d := 'CD2';
    SetLength(agg.abArr, 2);
    agg.abArr[0] := ab0;
    agg.abArr[1] := ab1;
    SetLength(agg.cdArr, 3);
    agg.cdArr[0] := cd0;
    agg.cdArr[1] := cd1;
    agg.cdArr[2] := cd2;
    U :=
      '{"abArr":[{"a":"AB0","b":0},{"a":"AB1","b":1}],"cdArr":[{"c":0,"d":"CD0"},' + '{"c":1,"d":"CD1"},{"c":2,"d":"CD2"}]}';
    CheckHash(U, $E3AC9C44);
    check(IsValidJson(U));
    J := RecordSaveJson(agg, TypeInfo(TAggregate));
    CheckEqual(J, U);
    RecordLoadJson(agg2, UniqueRawUtf8(U), TypeInfo(TAggregate));
    J := RecordSaveJson(agg2, TypeInfo(TAggregate));
    CheckHash(J, $E3AC9C44);
    check(IsValidJson(J));

    Finalize(JAS);
    FillCharFast(JAS, sizeof(JAS), 0);
    U := RecordSaveJson(JAS, TypeInfo(TTestCustomJsonArraySimple));
    CheckEqual(U, '{"A":0,"B":0,"C":[],"D":"","E":[],"H":""}');
    check(IsValidJson(U));
    U := '{"a":1,"b":2,"c":["C9A646D3-9C61-4CB7-BFCD-EE2522C8F633",' +
      '"3F2504E0-4F89-11D3-9A0C-0305E82C3301"],"d":"4","e":[{"f":"f","g":["g1","g2"]}],"h":"h"}';
    J := U;
    Check(RecordLoadJson(JAS, UniqueRawUtf8(U), TypeInfo(TTestCustomJsonArraySimple)) <> nil);
    Check(JAS.A = 1);
    Check(JAS.B = 2);
    Check(length(JAS.C) = 2);
    Check(GuidToRawUtf8(JAS.C[0]) = '{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
    Check(GuidToRawUtf8(JAS.C[1]) = '{3F2504E0-4F89-11D3-9A0C-0305E82C3301}');
    Check(JAS.D = '4');
    Check(length(JAS.E) = 1);
    Check(JAS.E[0].F = 'f');
    Check(Length(JAS.E[0].G) = 2);
    Check(JAS.E[0].G[0] = 'g1');
    Check(JAS.E[0].G[1] = 'g2');
    Check(JAS.H = 'h');
    U := RecordSaveJson(JAS, TypeInfo(TTestCustomJsonArraySimple));
    Check(SameTextU(J, U));
    check(IsValidJson(U));

    Finalize(JAV);
    FillCharFast(JAV, sizeof(JAV), 0);
    U := RecordSaveJson(JAV, TypeInfo(TTestCustomJsonArrayVariant));
    CheckEqual(U, '{"A":0,"B":0,"C":[],"D":""}');
    check(IsValidJson(U));
    assert(DocVariantType <> nil);
    U := '{"a":1,"b":2,"c":["one",2,2.5,{four:[1,2,3,4]}],"d":"4"}';
    check(IsValidJson(U));
    RecordLoadJson(JAV, UniqueRawUtf8(U), TypeInfo(TTestCustomJsonArrayVariant));
    Check(JAV.A = 1);
    Check(JAV.B = 2);
    if not CheckFailed(length(JAV.C) = 4) then
    begin
      Check(JAV.C[0] = 'one');
      Check(JAV.C[1] = 2);
      CheckSame(JAV.C[2], 2.5);
      Check(JAV.C[3]._Kind = ord(dvObject));
      Check(JAV.C[3]._Count = 1);
      Check(JAV.C[3].Name(0) = 'four');
      Check(VariantSaveJson(JAV.C[3].four) = '[1,2,3,4]');
      with DocVariantData(JAV.C[3])^ do
      begin
        Check(Kind = dvObject);
        Check(Count = 1);
        Check(Names[0] = 'four');
        Check(Values[0]._Kind = ord(dvArray));
        Check(Values[0]._Count = 4);
        with DocVariantData(Values[0])^ do
        begin
          Check(Kind = dvArray);
          Check(Count = 4);
          for v := 0 to Count - 1 do
            Check(Values[v] = v + 1);
        end;
      end;
    end;
    Check(JAV.D = '4');
    GDtoObject := TDtoObject.Create;
    U := '{"SomeField":"Test"}';
    Check(ObjectLoadJson(GDtoObject, U, nil, []), 'nestedvariant1');
    J := ObjectToJson(GDtoObject, []);
    CheckEqual(J, '{"NestedObject":{"FieldString":"","FieldInteger":0,' +
      '"FieldVariant":null},"SomeField":"Test"}');
    J := ObjectToJson(GDtoObject, [woDontStoreVoid]);
    CheckEqual(J, U);
    U := '{"NestedObject":{"FieldVariant":{"a":1,"b":2}},"SomeField":"Test"}';
    Check(ObjectLoadJson(GDtoObject, U, nil, [jpoHandleCustomVariants]),
      'nestedvariant2');
    J := ObjectToJson(GDtoObject, [woDontStoreVoid]);
    CheckEqual(J, U);
    GDtoObject.Free;

    Finalize(Cache);
    FillCharFast(Cache, sizeof(Cache), 0);
    U := RecordSaveJson(Cache, TypeInfo(TRestCacheEntryValue));
    CheckEqual(U, '{"ID":0,"Timestamp512":0,"Tag":0,"Json":""}');
    check(IsValidJson(U));
    Cache.ID := 10;
    Cache.Timestamp512 := 200;
    Cache.Json := 'test';
    Cache.Tag := 12;
    U := RecordSaveJson(Cache, TypeInfo(TRestCacheEntryValue));
    CheckEqual(U, '{"ID":10,"Timestamp512":200,"Tag":12,"Json":"test"}');
    check(IsValidJson(U));
    U := '{"ID":210,"Timestamp512":2200,"Json":"test2"}';
    check(IsValidJson(U));
    RecordLoadJson(Cache, UniqueRawUtf8(U), TypeInfo(TRestCacheEntryValue));
    Check(Cache.ID = 210);
    Check(Cache.Timestamp512 = 2200);
    Check(Cache.Json = 'test2');
    Check(Cache.Tag = 12);
    U := '{ID:220,Json:"test3",Timestamp512:2300}';
    check(IsValidJson(U));
    RecordLoadJson(Cache, UniqueRawUtf8(U), TypeInfo(TRestCacheEntryValue));
    Check(Cache.ID = 220);
    Check(Cache.Timestamp512 = 2300);
    Check(Cache.Json = 'test3');
    Check(Cache.Tag = 12);

    {$ifdef ISDELPHI2010}
    FillCharFast(nav, sizeof(nav), 0);
    FillCharFast(nav2, sizeof(nav2), 1);
    Check(not CompareMem(@nav, @nav2, sizeof(nav)));
    Check(nav2.MaxRows <> 0);
    check(nav2.EOF);
    U := RecordSaveJson(nav, TypeInfo(TConsultaNav));
    J := RecordSaveJson(nav2, TypeInfo(TConsultaNav));
    Check(U <> J);
    RecordLoadJson(nav2, UniqueRawUtf8(U), TypeInfo(TConsultaNav));
    Check(nav2.MaxRows = 0);
    check(not nav2.EOF);
    J := RecordSaveJson(nav2, TypeInfo(TConsultaNav));
    CheckEqual(J, RecordSaveJson(nav, TypeInfo(TConsultaNav)));
    Check(CompareMem(@nav, @nav2, sizeof(nav)));
    Finalize(nrtti);
    FillCharFast(nrtti, sizeof(nrtti), 0);
    U := RecordSaveJson(nrtti, TypeInfo(TNewRtti));
    CheckEqual(U,
      '{"Number":0,"StaticArray":[{"Name":"","Single":0,"Double":0},' +
      '{"Name":"","Single":0,"Double":0}],"Int":[0,0,0,0,0]}');
    Finalize(nrtti2);
    FillCharFast(nrtti2, sizeof(nrtti2), 0);
    Check(RecordLoadJson(nrtti2, pointer(U), TypeInfo(TNewRtti)) <> nil);
    J := RecordSaveJson(nrtti2, TypeInfo(TNewRtti));
    CheckEqual(J, RecordSaveJson(nrtti, TypeInfo(TNewRtti)));
    nrtti.Number := 1;
    nrtti.StaticArray[1].Name := 'one';
    nrtti.StaticArray[1].Single := 1.5;
    nrtti.StaticArray[1].Double := 1.7;
    nrtti.StaticArray[2].Name := 'two';
    nrtti.StaticArray[2].Single := 2.5;
    nrtti.StaticArray[2].Double := 2.7;
    nrtti.Int[1] := 1;
    nrtti.Int[2] := 2;
    nrtti.Int[3] := 3;
    nrtti.Int[4] := 4;
    nrtti.Int[5] := 5;
    U := RecordSaveJson(nrtti, TypeInfo(TNewRtti));
    CheckEqual(U,
      '{"Number":1,"StaticArray":[{"Name":"one","Single":1.5,"Double":1.7},' +
      '{"Name":"two","Single":2.5,"Double":2.7}],"Int":[1,2,3,4,5]}');
    Finalize(nrtti2);
    FillCharFast(nrtti2, sizeof(nrtti2), 0);
    Check(RecordLoadJson(nrtti2, pointer(U), TypeInfo(TNewRtti)) <> nil);
    J := RecordSaveJson(nrtti2, TypeInfo(TNewRtti));
    CheckEqual(J, RecordSaveJson(nrtti, TypeInfo(TNewRtti)));
    U :=
      '{ "name": "Book the First", "author": { "first_name": "Bob", "last_name": "White" } }';
    RecordLoadJson(book, UniqueRawUtf8(U), TypeInfo(TBookRecord));
    check(book.name = 'Book the First');
    check(book.author.first_name = 'Bob');
    Check(book.author.last_name = 'White');
    {$endif ISDELPHI2010}
  end;

  procedure TestGetJsonField(const s, v: RawUtf8; str, error: boolean;
    eof, next: AnsiChar);
  var
    P, d: PUtf8Char;
    ws: boolean;
    e: AnsiChar;
    l: integer;
    s2: RawUtf8;
  begin
    s2 := s;
    P := UniqueRawUtf8(s2);
    P := GetJsonField(P, d, @ws, @e, @l);
    check(error = (d = nil));
    if d = nil then
      exit;
    check(str = ws);
    check(eof = e);
    check(d^ = next);
    check(l = length(v));
    check(CompareMem(P, pointer(v), length(v)));
  end;

begin
  TestGetJsonField('', '', false, true, #0, #0);
  TestGetJsonField('true,false', 'true', false, false, ',', 'f');
  TestGetJsonField('false,1', 'false', false, false, ',', '1');
  TestGetJsonField('"true",false', 'true', true, false, ',', 'f');
  TestGetJsonField('"",false', '', true, false, ',', 'f');
  TestGetJsonField('12,false', '12', false, false, ',', 'f');
  TestGetJsonField('12]', '12', false, false, ']', #0);
  TestGetJsonField('12],', '12', false, false, ']', ',');
  TestGetJsonField('1.2],', '1.2', false, false, ']', ',');
  TestGetJsonField('1.2  ],', '1.2', false, false, ']', ',');
  TestGetJsonField('"123"},false', '123', true, false, '}', ',');
  TestGetJsonField('"1\\3"},false', '1\3', true, false, '}', ',');
  TestGetJsonField('"1\r\n"},false', '1'#13#10, true, false, '}', ',');
  TestGetJsonField('"\"3"},false', '"3', true, false, '}', ',');
  TestGetJsonField('"\u00013"},false', #1'3', true, false, '}', ',');
  TestGetJsonField('"\u0020"},false', ' ', true, false, '}', ',');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"toto"'))) = '"');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"toto",'))) = '",');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"to'#0'to",')))^ = #0);
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"to\'#0'to",')))^ = '\');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"to\"to",'))) = '",');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"to\\"to",'))) = '"to",');
  Check(GotoEndOfJsonString(PUtf8Char(PAnsiChar('"to\\\\to",'))) = '",');
  Check(IsString('abc'));
  Check(IsString('NULL'));
  Check(IsString('null'));
  Check(IsString('false'));
  Check(IsString('FALSE'));
  Check(IsString('true'));
  Check(IsString('TRUE'));
  Check(not IsString('123'));
  Check(not IsString('0123'));
  Check(not IsString('0.123'));
  Check(not IsString('1E19'));
  Check(not IsString('1.23E1'));
  Check(not IsString('+0'));
  Check(IsString('1.23E'));
  Check(IsString('+'));
  Check(IsString('-'));
  Check(IsStringJson('abc'));
  Check(IsStringJson('NULL'));
  Check(not IsStringJson('null'));
  Check(not IsStringJson('false'));
  Check(IsStringJson('FALSE'));
  Check(not IsStringJson('true'));
  Check(IsStringJson('TRUE'));
  Check(not IsStringJson('123'));
  Check(IsStringJson('0123'));
  Check(not IsStringJson('0.123'));
  Check(not IsStringJson('1E19'));
  Check(not IsStringJson('1.23E1'));
  Check(not IsStringJson('0'));
  Check(not IsStringJson('0.1'));
  Check(not IsStringJson('-0'));
  Check(not IsStringJson('-0.1'));
  Check(IsStringJson('+0'));
  Check(IsStringJson('1.23E'));
  Check(IsStringJson('+'));
  Check(IsStringJson('-'));
  Check(not NeedsJsonEscape(''));
  Check(not NeedsJsonEscape('a'));
  Check(not NeedsJsonEscape('ab cd'));
  Check(not NeedsJsonEscape('13456 ds0'));
  Check(NeedsJsonEscape('"123'));
  Check(NeedsJsonEscape('123"567'));
  Check(NeedsJsonEscape('123"'));
  Check(NeedsJsonEscape('123\"'));
  Check(NeedsJsonEscape('123'#1));
  Check(NeedsJsonEscape(#10'123'));
  CheckEqual(QuotedStrJson(''), '""');
  CheckEqual(QuotedStrJson('a'), '"a"');
  CheckEqual(QuotedStrJson(#30), '"\u001E"');
  CheckEqual(QuotedStrJson('ab'), '"ab"');
  CheckEqual(QuotedStrJson(' a'), '" a"');
  CheckEqual(QuotedStrJson('a"'), '"a\""');
  CheckEqual(QuotedStrJson('a""'), '"a\"\""');
  CheckEqual(QuotedStrJson('""'), '"\"\""');
  CheckEqual(QuotedStrJson('a"b"c'), '"a\"b\"c"');
  CheckEqual(QuotedStrJson('a"b\c'), '"a\"b\\c"');
  CheckEqual(QuotedStrJson('a"b'#10'c'), '"a\"b\nc"');
  CheckEqual(QuotedStrJson('a'#13'b'#8'c'), '"a\rb\bc"');
  CheckEqual(QuotedStrJson('a'#13'b'#1'c'), '"a\rb\u0001c"');
  CheckEqual(QuotedStrJson('a'#13'b'#31'c'), '"a\rb\u001Fc"');
  CheckEqual(QuotedStrJson('a'#13'b'#31), '"a\rb\u001F"');
  Check(Utf8ContentType('null') = oftUnknown);
  Check(Utf8ContentType('0') = oftInteger);
  Check(Utf8ContentType('123') = oftInteger);
  Check(Utf8ContentType('0123') = oftUtf8Text);
  Check(Utf8ContentType('-123') = oftInteger);
  Check(Utf8ContentType('123.1') = oftCurrency);
  Check(Utf8ContentType('123.12') = oftCurrency);
  Check(Utf8ContentType('123.1234') = oftCurrency);
  Check(Utf8ContentType('123.12345678') = oftFloat);
  Check(Utf8ContentType('1.13e+12') = oftFloat);
  Check(Utf8ContentType('1.13e12') = oftFloat);
  Check(Utf8ContentType('-1.13e-12') = oftFloat);
  Check(Utf8ContentType('1.13e+120') = oftFloat);
  Check(Utf8ContentType('1.13E120') = oftFloat);
  Check(Utf8ContentType('1.13E-120') = oftFloat);
  Check(Utf8ContentType('1.13E307') = oftFloat);
  Check(Utf8ContentType('1.13E-323') = oftFloat);
  Check(Utf8ContentType('1.13e+a3') = oftUtf8Text);
  Check(Utf8ContentType('1.13e+3a') = oftUtf8Text);
  Check(Utf8ContentType('1.13e+330') = oftUtf8Text);
  Check(Utf8ContentType('1.13e330') = oftUtf8Text);
  Check(Utf8ContentType('1.13e-330') = oftUtf8Text);
  Check(Utf8ContentType('420014165100E335') = oftUtf8Text);
  Check(Utf8ContentType('123.') = oftUtf8Text);
  Check(Utf8ContentType('123.a') = oftUtf8Text);
  Check(Utf8ContentType('123.1a') = oftUtf8Text);
  Check(Utf8ContentType('123.1234a') = oftUtf8Text);
  Check(Utf8ContentType('123-2') = oftUtf8Text);
  Check(uct('null') = oftUnknown);
  Check(uct('0') = oftInteger);
  Check(uct('123') = oftInteger);
  Check(uct('0123') = oftUtf8Text);
  Check(uct('-123') = oftInteger);
  Check(uct('123.1') = oftCurrency);
  Check(uct('123.12') = oftCurrency);
  Check(uct('123.12345678') = oftFloat);
  Check(uct('1.13e+12') = oftFloat);
  Check(uct('-1.13e-12') = oftFloat);
  Check(uct('123.') = oftUtf8Text);
  Check(uct('123.a') = oftUtf8Text);
  Check(uct('123.1a') = oftUtf8Text);
  Check(uct('123.1234a') = oftUtf8Text);
  Check(uct('123-2') = oftUtf8Text);
  J := JsonEncode(['name', 'john', 'year', 1982, 'pi', 3.14159]);
  CheckEqual(J, '{"name":"john","year":1982,"pi":3.14159}');
  check(IsValidJson(J));
  JsonDecode(J, ['year', 'pi', 'john', 'name'], @V);
  Check(V[0].Value = '1982');
  Check(V[1].Value = '3.14159');
  Check(V[2].Value = nil);
  Check(V[3].Value = 'john');
  J := '{surrogate:"\uD801\uDC00"}'; // see https://en.wikipedia.org/wiki/CESU-8
  check(IsValidJson(J));
  JsonDecode(J, ['surrogate'], @V);
  Check(V[0].ValueLen = 4);
  Check(V[0].Value[0] = #$F0);
  Check(V[0].Value[1] = #$90);
  Check(V[0].Value[2] = #$90);
  Check(V[0].Value[3] = #$80);
  J := JsonEncode(['name', 'john', 'ab', '[', 'a', 'b', ']']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":["a","b"]}');
  J := JsonEncode(['name', 'john', 'ab', '[', 'a', 'b']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":["a","b"]}');
  J := JsonEncode(['name', 'john', 'ab', '[']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":[]}');
  J := JsonEncode(['name', 'john', 'ab', '{']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":{}}');
  J := JsonEncode(['name', 'john', 'ab', nil]);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":null}');
  J := JsonEncode(['name', 'john', 'ab']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john"}');
  J := JsonEncode(['name', 'john', '{']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john"}');
  J := JsonEncode(['name', 'john', '[']);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john"}');
  J := JsonEncode(['name', 'john', 'ab', '[', 'a', 'b', ']', 'pi', 3.14159]);
  check(IsValidJson(J));
  CheckEqual(J, '{"name":"john","ab":["a","b"],"pi":3.14159}');
  J := JsonEncode(['doc', '{', 'name', 'John', 'year', 1982, '}', 'id', 123]);
  check(IsValidJson(J));
  CheckEqual(J, '{"doc":{"name":"John","year":1982},"id":123}');
  J := JsonEncode(['doc', '{', 'name', 'John', 'abc', '[', 'a', 'b', 'c', ']',
    '}', 'id', 123]);
  check(IsValidJson(J));
  CheckEqual(J, '{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
  J := JsonEncode('{%:{$in:[?,?]}}', ['type'], ['food', 'snack']);
  check(IsValidJson(J));
  CheckEqual(J, '{"type":{"$in":["food","snack"]}}');
  Check(JsonEncode('{type:{$in:?}}', [], [_Arr(['food', 'snack'])]) = J);
  check(IsValidJson(J));
  J := JsonEncode('{name:"John",field:{ "$regex": "acme.*corp", $options: "i" }}',
    [], []);
  CheckEqual(J, '{"name":"John","field":{"$regex":"acme.*corp","$options":"i"}}');
  // below only works if unit mormot.db.nosql.bson is included in uses
  CheckEqual(JsonEncode('{name:?,field:/%/i}', ['acme.*corp'], ['John']), J);
  peop := TOrmPeople.Create;
  try
    peop.IDValue := 1234;
    peop.FirstName := 'FN';
    peop.LastName := 'LN';
    peop.YearOfBirth := 1000;
    peop.Data := #1#2#3#4;
    J := ObjectToJson(peop, [woRawBlobAsBase64]);
    check(IsValidJson(J));
    check(J[53] = #$EF);
    check(J[54] = #$BF);
    check(J[55] = #$B0);
    J[53] := '1';
    J[54] := '2';
    J[55] := '3';
    check(IsValidJson(J));
    CheckEqual(J, '{"ID":1234,"FirstName":"FN","LastName":"LN",' +
      '"Data":"123AQIDBA==","YearOfBirth":1000,"YearOfDeath":0}');
    J := ObjectToJson(peop);
    check(IsValidJson(J));
    CheckEqual(J, '{"ID":1234,"FirstName":"FN","LastName":"LN",' +
      '"Data":null,"YearOfBirth":1000,"YearOfDeath":0}');
    ClearObject(peop);
    J := ObjectToJson(peop);
    check(IsValidJson(J));
    CheckEqual(J, '{"ID":0,"FirstName":"","LastName":"",' +
      '"Data":null,"YearOfBirth":0,"YearOfDeath":0}');
    peop.IDValue := -1234;
    J := ObjectToJson(peop);
    check(IsValidJson(J));
    CheckEqual(J, '{"ID":-1234,"FirstName":"","LastName":"",' +
      '"Data":null,"YearOfBirth":0,"YearOfDeath":0}');
    peop.YearOfDeath := 10;
    peop.LastName := 'john';
    J := ObjectToJson(peop);
    check(IsValidJson(J));
    CheckEqual(J, '{"ID":-1234,"FirstName":"","LastName":"john","Data":null,' +
      '"YearOfBirth":0,"YearOfDeath":10}');
  finally
    peop.Free;
  end;
  for i := 1 to 100 do
  begin
    a := Random32(maxInt);
    r := Random;
    U := RandomUtf8(i);
    J := JsonEncode(['a', a, 'r', r, 'u', U]);
    check(IsValidJson(J));
    JsonDecode(J, ['U', 'R', 'A', 'FOO'], @V);
    V[0].ToUtf8(U2);
    Check(U2 = U);
    Check(SameValue(GetExtended(V[1].Value, err), r));
    Check(not IsString(V[2].Value));
    Check(not IsStringJson(V[2].Value));
    Check(V[2].ToInteger = a);
    Check(V[3].Value = nil);
    J := BinToBase64WithMagic(U);
    check(PInteger(J)^ and $00ffffff = JSON_BASE64_MAGIC_C);
    RB := BlobToRawBlob(pointer(J));
    check(length(RB) = length(U)); // RB=U is buggy under FPC :(
    check(CompareMem(pointer(RB), pointer(U), length(U)));
    Base64MagicToBlob(@J[4], K);
    RB := BlobToRawBlob(pointer(K));
    check(length(RB) = length(U));
    check(CompareMem(pointer(RB), pointer(U), length(U)));
    BlobToRawBlob(pointer(K), RB, length(K));
    check(length(RB) = length(U));
    check(CompareMem(pointer(RB), pointer(U), length(U)));
    RB := BlobToRawBlob(K);
    check(length(RB) = length(U));
    check(CompareMem(pointer(RB), pointer(U), length(U)));
{    J := TRestServer.JsonEncodeResult([r]);
    Check(SameValue(GetExtended(pointer(JsonDecode(J)),err),r)); }
    with TTextWriter.CreateOwnedStream do
    try
      AddVariant(a);
      AddComma;
      AddVariant(r);
      AddComma;
      PInt64(@c)^ := a;
      AddVariant(c);
      AddComma;
      U := Int32ToUtf8(a);
      AddVariant(U);
      J := Text;
      CheckEqual(J, U + ',' + DoubleToStr(r) + ',' + DoubleToStr(c) + ',"' + U + '"');
      P := UniqueRawUtf8(J);
      P := VariantLoadJson(Va, P);
      Check(P <> nil);
      Check(Va = a);
      P := VariantLoadJson(Va, P, nil, nil, true);
      Check(P <> nil);
      CheckSame(VariantToDoubleDef(Va), r);
      P := VariantLoadJson(Va, P);
      Check(P <> nil);
      Check(Va = c);
      P := VariantLoadJson(Va, P);
      Check((P <> nil) and
            (P^ = #0));
      Check(Va = U);
      binary := VariantSave(Va);
      Vb := VariantLoad(binary, @JSON_OPTIONS[true]);
      Check(Vb = U);
    finally
      Free;
    end;
  end;
  J := GetJsonObjectAsSql('{"ID":  1 ,"Name":"Alice","Role":"User","Last Login":null,'+
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]',
    false, true);
  U := ' (ID,Name,Role,Last Login,First Login,Department) VALUES ' +
    '(:(1):,:(''Alice''):,:(''User''):,:(null):,:(null):,:(''{"relPath":"317\\","revision":1}''):)';
  CheckEqual(J, U);
  J := GetJsonObjectAsSql('{ "Name":"Alice","Role":"User","Last Login":null,' +
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]',
    false, true, 1, true);
  CheckEqual(J, U);
  J := GetJsonObjectAsSql('{ "Name":"Alice","Role":"User","Last Login":null,' +
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]',
    false, true, 1, false);
  Insert('Row', U, 3);
  CheckEqual(J, U);
  Delete(U, 3, 3);
  J :=
    '{"ID":  1 ,"Name":"Alice","Role":"User","Last Login":null, // comment'#13#10 +
    '"First Login" : /* to be ignored */  null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]';
  check(not IsValidJson(J));
  RemoveCommentsFromJson(UniqueRawUtf8(J));
  check(not IsValidJson(J));
  check(IsValidJson('[' + J));
  J := GetJsonObjectAsSql(J, false, true);
  CheckEqual(J, U);
  J := '{'#10'"httpServer": {'#10'"host": "*",'#10'"port": "8881",'#10 +
    '"serverType": "Socket",'#10'/*"reverseProxy": {'#10'"kind": "nginx",'#10 +
    '"sendFileLocationRoot": "snake-ukrpatent-local"'#10'}*/'#10'} //eol'#10'}';
  check(not IsValidJSON(J));
  RemoveCommentsFromJson(UniqueRawUTF8(J));
  CheckUtf8(IsValidJSON(J), J);
  J := JSONReformat(J,jsonCompact);
  CheckEqual(J,'{"httpServer":{"host":"*","port":"8881","serverType":"Socket"}}');
  J :=
    '{"RowID":  210 ,"Name":"Alice","Role":"User","Last Login":null, // comment'#13#10 +
    '"First Login" : /* to be ignored */  null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]';
  check(not IsValidJson(J));
  RemoveCommentsFromJson(UniqueRawUtf8(J));
  check(not IsValidJson(J));
  check(IsValidJson('[' + J));
  J := GetJsonObjectAsSql(J, false, true, 1, True);
  CheckEqual(J, U);
  O := TPersistentToJson.Create;
  O2 := TPersistentToJson.Create;
  try
    J := ObjectToJson(O, []);
    check(IsValidJson(J));
    CheckEqual(J, '{"Name":"","Enum":0,"Sets":0}');
    J := ObjectToJson(O, [woDontStoreDefault]);
    check(IsValidJson(J));
    CheckEqual(J, '{"Name":""}');
    J := ObjectToJson(O, [woStoreClassName]);
    check(IsValidJson(J));
    CheckEqual(J, '{"ClassName":"TPersistentToJson","Name":"","Enum":0,"Sets":0}');
    J := ObjectToJson(O, [woHumanReadable]);
    check(IsValidJson(J));
    CheckEqual(J,
      #$D#$A'{'#$D#$A#9'"Name": "",'#$D#$A#9'"Enum": "flagIdle",'#$D#$A#9'"Sets": []'#$D#$A'}');
    with PRttiInfo(TypeInfo(TSynBackgroundThreadProcessStep))^.EnumBaseType^ do
      for E := low(E) to high(E) do
      begin
        O.fName := Int32ToUtf8(ord(E));
        O.fEnum := E;
        include(O.fSets, E);
        J := ObjectToJson(O, []);
        check(IsValidJson(J));
        CheckEqual(J, FormatUtf8('{"Name":"%","Enum":%,"Sets":%}',
          [ord(E), ord(E), byte(O.fSets)]));
        JsonToObject(O2, pointer(J), Valid);
        Check(Valid);
        Check(O.Name = O2.Name);
        Check(O.Enum = O2.Enum);
        Check(O.Sets = O2.Sets);
        J := ObjectToJson(O, [woHumanReadable]);
        check(IsValidJson(J));
        U := FormatUtf8(
          #13#10'{'#$D#$A#9'"NAME": "%",'#$D#$A#9'"ENUM": "%",'#$D#$A#9'"SETS": ["FLAGIDLE"',
          [ord(E), UpperCaseU(RawUtf8(GetEnumName(E)^))]);
        Check(IdemPChar(pointer(J), pointer(U)));
        JsonToObject(O2, pointer(J), Valid);
        Check(Valid);
        Check(O.Name = O2.Name);
        Check(O.Enum = O2.Enum);
        Check(O.Sets = O2.Sets);
        Check(ObjectEquals(O, O2));
      end;
    with PRttiInfo(TypeInfo(WordBool))^.EnumBaseType^ do
      Check(SizeInStorageAsEnum = 2);
    J := ObjectToJson(O, [woHumanReadable, woHumanReadableFullSetsAsStar]);
    check(IsValidJson(J));
    CheckEqual(J,
      #13#10'{'#$D#$A#9'"Name": "3",'#$D#$A#9'"Enum": "flagDestroying",' +
      #$D#$A#9'"Sets": ["*"]'#$D#$A'}');
    J := ObjectToJson(O, [woHumanReadable, woHumanReadableFullSetsAsStar,
      woHumanReadableEnumSetAsComment]);
    CheckEqual(J,
      #13#10'{'#$D#$A#9'"Name": "3",'#$D#$A#9'"Enum": "flagDestroying", ' +
      '// "flagIdle","flagStarted","flagFinished","flagDestroying"' +
      #$D#$A#9'"Sets": ["*"] // "*" or a set of "flagIdle","flagStarted",' +
      '"flagFinished","flagDestroying"'#$D#$A'}');
    O2.fName := '';
    O2.fEnum := low(E);
    O2.fSets := [];
    check(not IsValidJson(J));
    RemoveCommentsFromJson(UniqueRawUtf8(J));
    check(IsValidJson(J));
    JsonToObject(O2, pointer(J), Valid);
    Check(Valid);
    Check(O.Name = O2.Name);
    Check(O.Enum = O2.Enum);
    Check(O.Sets = O2.Sets);
    Check(ObjectEquals(O, O2));
  finally
    O2.Free;
    O.Free;
  end;
  U := '"filters":[{"name":"name1","value":"value1","comparetype":">"},' +
    '{"name":"name2","value":"value2","comparetype":"="}], "Limit":100}';
  check(not IsValidJson(U));
  check(IsValidJson('{' + U));
  P := UniqueRawUtf8(U);
  Check(GetJsonPropName(P) = 'filters');
  Check((P <> nil) and
        (P^ = '['));
  P := GotoNextJsonItem(P, 1, @EndOfObject);
  Check(EndOfObject = ',');
  Check(GetJsonPropName(P) = 'Limit');
  Check((P <> nil) and
        (P^ = '1'));
  P := GotoNextJsonItem(P, 1, @EndOfObject);
  Check(P <> nil);
  Check(EndOfObject = '}');
  check(IsValidJson('null'));
  check(IsValidJson('true'));
  check(IsValidJson('false'));
  check(IsValidJson(' null'));
  check(IsValidJson(' true'));
  check(IsValidJson(' false'));
  check(IsValidJson('null  '));
  check(IsValidJson('true  '));
  check(IsValidJson('false  '));
  check(not IsValidJson('nulle'));
  check(not IsValidJson('trye'));
  C2 := TCollTst.Create;
  Coll := TCollTst.Create;
  try
    U := ObjectToJson(Coll);
    check(IsValidJson(U));
    CheckEqual(U,
      '{"One":{"Color":0,"Length":0,"Name":""},"Coll":[],"Str":null}');
    Check(ObjectToJson(C2) = U);
    Coll.One.Name := 'test"\2';
    Coll.One.Color := 1;
    U := ObjectToJson(Coll);
    check(IsValidJson(U));
    CheckEqual(U,
      '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":[],"Str":null}');
    Check(JsonToObject(C2, pointer(U), Valid)^ = #0);
    Check(Valid);
    U := ObjectToJson(C2);
    check(IsValidJson(U));
    CheckEqual(U,
      '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":[],"Str":null}');
    Coll.Coll.Add.Color := 10;
    Coll.Coll.Add.Name := 'name';
    Check(Coll.Coll.Count = 2);
    U := ObjectToJson(Coll);
    check(IsValidJson(U));
    CheckEqual(U, '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":' +
      '[{"Color":10,"Length":0,"Name":""},{"Color":0,"Length":0,"Name":"name"}],"Str":null}');
    Check(JsonToObject(C2, pointer(U), Valid)^ = #0);
    Check(Valid);
    Check(C2.Coll.Count = 2);
    U := ObjectToJson(C2);
    check(IsValidJson(U));
    CheckEqual(U, '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":' +
      '[{"Color":10,"Length":0,"Name":""},{"Color":0,"Length":0,"Name":"name"}],"Str":null}');
    J := ObjectToJson(Coll, [woHumanReadable]);
    check(IsValidJson(U));
    CheckHash(J, $7694E4C1);
    Check(JsonReformat(J, jsonCompact) = U);
    Check(JsonReformat('{ "empty": {} }') =
      '{'#$D#$A#9'"empty": {'#$D#$A#9#9'}'#$D#$A'}');
    U := ObjectToJson(Coll, [woStoreClassName]);
    check(IsValidJson(U));
    CheckEqual(U,
      '{"ClassName":"TCollTst","One":{"ClassName":"TCollTest","Color":1,' +
      '"Length":0,"Name":"test\"\\2"},"Coll":[{"ClassName":"TCollTest","Color":10,' +
      '"Length":0,"Name":""},{"ClassName":"TCollTest","Color":0,"Length":0,"Name":"name"}],"Str":null}');
    C2.Coll.Clear;
    Check(C2.Coll.Count = 0);
    Check(JsonToObject(C2, pointer(U), Valid)^ = #0);
    Check(Valid);
    Check(C2.Coll.Count = 2);
    U := ObjectToJson(C2);
    CheckEqual(U, '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":' +
      '[{"Color":10,"Length":0,"Name":""},{"Color":0,"Length":0,"Name":"name"}],"Str":null}');
    Rtti.RegisterClasses([TComplexNumber, TCollTst]);
    J := '{"ClassName":"TComplexNumber", "Real": 10.3, "Imaginary": 7.92 }';
    P := UniqueRawUtf8(J); // make local copy of source constant
    Comp := TComplexNumber(JsonToNewObject(P, Valid));
    if not CheckFailed(Comp <> nil) then
    begin
      Check(Valid);
      Check(Comp.ClassType = TComplexNumber);
      CheckSame(Comp.Real, 10.3);
      CheckSame(Comp.Imaginary, 7.92);
      U := ObjectToJson(Comp, [woStoreClassName]);
      check(IsValidJson(U));
      CheckEqual(U, '{"ClassName":"TComplexNumber","Real":10.3,"Imaginary":7.92}');
      Comp.Free;
    end;
    Instance := Rtti.RegisterCollection(TMyCollection, TCollTest);
    TestMyColl(TMyCollection.Create(TCollTest));
    TestMyColl(TObject(Instance.ClassNewInstance) as TMyCollection);
    C2.Coll.Clear;
    U := ObjectToJson(C2);
    check(IsValidJson(U));
    CheckEqual(U,
      '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":[],"Str":null}');
    Coll.Coll.BeginUpdate;
    for i := 1 to 10000 do
      with Coll.Coll.Add do
      begin
        Color := i * 3;
        Length := i * 5;
        Name := Int32ToUtf8(i);
      end;
    Coll.Coll.EndUpdate;
    U := ObjectToJson(Coll.Coll);
    check(IsValidJson(U));
    CheckHash(U, $DB782098);
    C2.Coll.Clear;
    Check(JsonToObject(C2.fColl, pointer(U), Valid)^ = #0);
    Check(Valid);
    Check(C2.Coll.Count = Coll.Coll.Count);
    for i := 1 to C2.Coll.Count - 2 do
      with C2.Coll[i + 1] do
      begin
        Check(Color = i * 3);
        Check(Length = i * 5);
        Check(GetInteger(pointer(Name)) = i);
      end;
    U := ObjectToJson(Coll);
    check(IsValidJson(U));
    Check(length(U) = 443114);
    CheckHash(U, $B1BD5123);
    C2.One.Name := '';
    C2.Coll.Clear;
    Check(JsonToObject(C2, pointer(U), Valid)^ = #0);
    Check(Valid);
    Check(C2.Coll.Count = Coll.Coll.Count);
    U := ObjectToJson(C2);
    check(IsValidJson(U));
    Check(length(U) = 443114);
    CheckHash(U, $B1BD5123);
    for i := 1 to C2.Coll.Count - 2 do
      with C2.Coll[i + 1] do
      begin
        Check(Color = i * 3);
        Check(Length = i * 5);
        Check(GetInteger(pointer(Name)) = i);
      end;
    Coll.Coll.Clear;
    Coll.Str := TStringList.Create;
    Coll.Str.BeginUpdate;
    for i := 1 to 10000 do
      Check(Coll.Str.Add(IntToStr(i)) = i - 1);
    Coll.Str.EndUpdate;
    U := ObjectToJson(Coll);
    check(IsValidJson(U));
    CheckHash(U, $85926050);
    J := ObjectToJson(Coll, [woHumanReadable]);
    check(IsValidJson(J));
    U2 := JsonReformat(J, jsonCompact);
    check(IsValidJson(U2));
    Check(U2 = U);
    C2.Str := TStringList.Create;
    Check(JsonToObject(C2, pointer(U), Valid)^ = #0);
    Check(Valid);
    Check(C2.Str.Count = Coll.Str.Count);
    for i := 1 to C2.Str.Count do
      Check(StrToInt(C2.Str[i - 1]) = i);
    J := ObjectToJson(C2);
    check(IsValidJson(J));
    CheckHash(J, $85926050);
    C2.One.Color := 0;
    C2.One.Name := '';
    U := '{"One":{"Color":1,"Length":0,"Name":"test","Unknown":123},"Coll":[]}';
    Check(JsonToObject(C2, UniqueRawUtf8(U),
      Valid, nil, [jpoIgnoreUnknownProperty])^ = #0, 'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color = 1);
    Check(C2.One.Name = 'test');
    C2.One.Color := 0;
    C2.One.Name := '';
    U := '{"One":{"Color":1,"Length":0,"wtf":{"one":1},"Name":"test",' +
      '"Unknown":123},"dummy":null,"Coll":[]}';
    check(IsValidJson(U));
    Check(JsonToObject(C2, UniqueRawUtf8(U), Valid, nil,
      [jpoIgnoreUnknownProperty])^ = #0, 'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color = 1);
    Check(C2.One.Name = 'test');
    U := '{"One":{"Color":1,"Length":0,"Name":"test\"\\2},"Coll":[]}';
    P := JsonToObject(C2, UniqueRawUtf8(U), Valid);
    Check(IdemPChar(P, '}'), 'invalid JSON');
    Check(not Valid);
    U := '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":[]';
    P := JsonToObject(C2, UniqueRawUtf8(U), Valid);
    Check(P = nil);
    Check(not Valid);
    U := '{"One":{"Color":,"Length":0,"Name":"test\"\\2"},"Coll":[]';
    Check(JsonToObject(C2, UniqueRawUtf8(U), Valid) = nil, 'invalid JSON');
    Check(not Valid);
    U := '{"Coll":[{"Color":1,"Length":0,"Name":"test"}],' +
      '"One":{"Color":2,"Length":0,"Name":"test2"}}';
    Check(JsonToObject(C2, UniqueRawUtf8(U), Valid, nil,
      [jpoIgnoreUnknownProperty])^ = #0, 'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color = 2);
    Check(C2.One.Name = 'test2');
    Check(C2.Coll.Count = 1);
    Check(C2.Coll[0].Name = 'test');
    C2.One.Length := 10;
    J := ObjectToJson(C2);
    check(IsValidJson(J));
    CheckHash(J, $41281936);
    // (custom) dynamic array serialization
    TCollTstDynArrayTest; // first TFVs prop is serialized as binary+base64
    TRttiJson.RegisterCustomSerializer(TypeInfo(TFVs),
      TCollTstDynArray.FVReader, TCollTstDynArray.FVWriter);
    TCollTstDynArrayTest; // TFVs serialized with FVReader/FVWriter
    TRttiJson.RegisterCustomSerializer(TypeInfo(TFVs),
      TCollTstDynArray.FVReader2, TCollTstDynArray.FVWriter2);
    TCollTstDynArrayTest; // TFVs serialized with FVReader2/FVWriter2
    // (custom) class serialization
    TFileVersionTest(false);
    TRttiJson.RegisterCustomSerializerClass(TFileVersion,
      TCollTstDynArray.FVClassReader, TCollTstDynArray.FVClassWriter);
    TFileVersionTest(true);
    TRttiJson.UnRegisterCustomSerializerClass(TFileVersion);
    TFileVersionTest(false);
    MyItem := TCollTest.Create(nil);
    try
      MyItem.Length := 10;
      MyItem.Color := 20;
      MyItem.Name := 'ABC';
      J := ObjectToJson(MyItem);
      Check(IsValidJson(J));
      CheckEqual(J, '{"Color":20,"Length":10,"Name":"ABC"}');
      Rtti.ByClass[TCollTest].Props.NameChanges(['name', 'length'], ['n', 'len']);
      J := ObjectToJson(MyItem);
      Check(IsValidJson(J));
      CheckEqual(J, '{"Color":20,"len":10,"n":"ABC"}');
      J := ObjectToJson(C2);
      Check(IsValidJson(J));
      CheckHash(J, $FFBC77A, 'RegisterCustomSerializerFieldNames');
      TCollTstDynArrayTest;
      Rtti.ByClass[TCollTest].Props.NameChanges([], []);
      J := ObjectToJson(MyItem);
      CheckEqual(J, '{"Color":20,"Length":10,"Name":"ABC"}');
      J := ObjectToJson(C2);
      Check(IsValidJson(J));
      CheckHash(J, $41281936, 'unRegisterCustomSerializerFieldNames');
      TCollTstDynArrayTest;
      Rtti.ByClass[TCollTest].Props.NameChanges(['length'], ['']);
      J := ObjectToJson(MyItem);
      Check(IsValidJson(J));
      CheckEqual(J, '{"Color":20,"Name":"ABC"}', 'remove field');
      Rtti.ByClass[TCollTest].Props.NameChanges([], []);
      J := ObjectToJson(MyItem);
      Check(IsValidJson(J));
      CheckEqual(J, '{"Color":20,"Length":10,"Name":"ABC"}', 'back to default');
    finally
      MyItem.Free;
    end;
  finally
    C2.Free;
    Coll.Free;
  end;
  // test RTTI definition from text
  Parser := TRttiCustom.CreateFromText('Int: double');
  Check(Parser.Props.Count = 1);
  Check(Parser.Props.List[0].Name = 'Int');
  Check(Parser.Props.List[0].Value.Parser = ptDouble);
  Parser.Free; // should be released by caller (not registered in Rtti list)
  Parser := TRttiCustom.CreateFromText(
    'A , B,C  : integer; D: RawUtf8');
  Check(Parser.Props.Count = 4);
  ABCD;
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C: integer; D: RawUtf8; E: record E1,E2: double; end;');
  Check(Parser.Props.Count = 5);
  ABCDE(ptRecord);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B: integer; C: integer; D: RawUtf8; E: array of record E1,E2: double; end;');
  Check(Parser.Props.Count = 5);
  ABCDE(ptDynArray);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C integer D RawUtf8 E{E1,E2 double}');
  Check(Parser.Props.Count = 5);
  ABCDE(ptRecord);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C: integer, D: RawUtf8, E{E1: double, E2: double}');
  Check(Parser.Props.Count = 5);
  ABCDE(ptRecord);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C integer D:RawUtf8 E[E1,E2:double]');
  Check(Parser.Props.Count = 5);
  ABCDE(ptDynArray);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C integer D RawUtf8 E[E1,E2 double] F: string');
  Check(Parser.Props.Count = 6);
  ABCDE(ptDynArray);
  Check(Parser.Props.List[5].Name = 'F');
  Check(Parser.Props.List[5].Value.Parser = ptString);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText(
    'A,B,C integer D RawUtf8 E[E1,E2 double] F: array of string');
  Check(Parser.Props.Count = 6);
  ABCDE(ptDynArray);
  Check(Parser.Props.List[5].Name = 'F');
  Check(Parser.Props.List[5].Value.Parser = ptDynArray);
  Check(Parser.Props.List[5].Value.ArrayRtti.Props.Count = 0);
  Check(Parser.Props.List[5].Value.ArrayRtti.Parser = ptString);
  Parser.Free;
  Parser := TRttiCustom.CreateFromText('A,B,C integer D RawUtf8 ' +
    'E[E1:{E1A:integer E1B:tdatetime E1C TDatetimeMS}E2 double]');
  Check(Parser.Props.Count = 5);
  ABCD;
  with Parser.Props.List[4] do
  begin
    Check(Name = 'E');
    Check(Value.Parser = ptDynArray);
    Check(Value.ArrayRtti.Props.Count = 2);
    Check(Value.ArrayRtti.Props.List[0].Name = 'E1');
    Check(Value.ArrayRtti.Props.List[0].Value.Parser = ptRecord);
    with Value.ArrayRtti.Props.List[0].Value.Props do
    begin
      Check(Count = 3);
      Check(List[0].Name = 'E1A');
      Check(List[0].Value.Parser = ptInteger);
      Check(List[1].Name = 'E1B');
      Check(List[1].Value.Parser = ptDateTime);
      Check(List[2].Name = 'E1C');
      Check(List[2].Value.Parser = ptDateTimeMS);
    end;
    Check(Value.ArrayRtti.Props.List[1].Name = 'E2');
    Check(Value.ArrayRtti.Props.List[1].Value.Parser = ptDouble);
  end;
  Parser.Free;

  {$ifdef ISDELPHI2010}
  // test JSON serialization defined by Enhanced RTTI available since Delphi 2010
  TestJSONSerialization;
  {$endif ISDELPHI2010}
  // test TJsonRecordTextDefinition JSON serialization
  Rtti.RegisterFromText(TypeInfo(TSubAB), __TSubAB);
  Rtti.RegisterFromText(TypeInfo(TSubCD), __TSubCD);
  Rtti.RegisterFromText(TypeInfo(TAggregate), __TAggregate);
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonRecord), __TTestCustomJsonRecord);
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArray), __TTestCustomJsonArray);
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArraySimple),
    __TTestCustomJsonArraySimple);
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArrayVariant),
    __TTestCustomJsonArrayVariant);
  Rtti.RegisterFromText(TypeInfo(TRestCacheEntryValue), __TRestCacheEntryValue);
  TestJSONSerialization;
  TestJSONSerialization; // test twice for safety
  Rtti.RegisterFromText(TypeInfo(TRestCacheEntryValue), '');
  Rtti.RegisterFromText(TypeInfo(TSubAB), '');
  Rtti.RegisterFromText(TypeInfo(TSubCD), '');
  Rtti.RegisterFromText(TypeInfo(TAggregate), '');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonRecord), '');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArray), '');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArrayVariant), '');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArraySimple), '');

  {$ifdef ISDELPHI2010}
  // test JSON serialization defined by Enhanced RTTI
  TestJSONSerialization;
  {$endif ISDELPHI2010}
  // tests parsing options
  Parser := Rtti.RegisterFromText(
    TypeInfo(TTestCustomJsonRecord), __TTestCustomJsonRecord);
  U := RecordSaveJson(JR2, TypeInfo(TTestCustomJsonRecord));
  Check(IsValidJson(U));
  CheckEqual(U, '{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0},"F":""}');
  U := RecordSaveJson(JR, TypeInfo(TTestCustomJsonRecord));
  Check(IsValidJson(U));
  CheckEqual(U, '{"A":10,"B":0,"C":0,"D":"**","E":{"E1":0,"E2":0},"F":"1899-12-31"}');
  U := '{"B":0,"C":0,"A":10,"D":"**","E":{"E1":0,"E2":20}}';
  JR2.A := 100;
  JR2.F := 10;
  RecordLoadJson(JR2, UniqueRawUtf8(U), TypeInfo(TTestCustomJsonRecord));
  Check(JR2.A = 10);
  Check(JR2.D = '**');
  Check(JR2.E.E2 = 20);
  Check(JR2.F = 10);
  TRttiJson(Parser).IncludeReadOptions := JSONPARSER_TOLERANTOPTIONS;
  U := '{ "A" : 1 , "B" : 2 , "C" : 3 , "D" : "A" , "tobeignored":null,"E": '#13#10 +
       '{ "E1" : 4, "E2" : 5 } , "tbi" : { "b" : 0 } }';
  RecordLoadJson(JR2, UniqueRawUtf8(U), TypeInfo(TTestCustomJsonRecord));
  Check(JR2.A = 1);
  Check(JR2.D = 'A');
  Check(JR2.E.E1 = 4);
  Check(JR2.E.E2 = 5);
  Check(JR2.F = 10);
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonRecord), '');

  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArrayWithoutF),
    copy(__TTestCustomJsonArray, 1, PosEx(']', __TTestCustomJsonArray)));
  U := RecordSaveJson(JA2, TypeInfo(TTestCustomJsonArrayWithoutF));
  Check(IsValidJson(U));
  CheckEqual(U,
    '{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}]}');
  Finalize(JA);
  FillCharFast(JA, sizeof(JA), 0);
  RecordLoadJson(JA, pointer(U), TypeInfo(TTestCustomJsonArrayWithoutF));
  Check(JA.A = 100);
  Check(JA.D = '');
  U := RecordSaveJson(JA, TypeInfo(TTestCustomJsonArrayWithoutF));
  Check(IsValidJson(U));
  Check(length(JA.E) = 2);
  CheckEqual(U,
    '{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}]}');
  JA.D := '1234';
  U := RecordSaveJson(JA, TypeInfo(TTestCustomJsonArrayWithoutF));
  Check(IsValidJson(U));
  Check(length(JA.E) = 2);
  Finalize(JA);
  FillCharFast(JA, sizeof(JA), 0);
  RecordLoadJson(JA, pointer(U), TypeInfo(TTestCustomJsonArrayWithoutF));
  Check(length(JA.E) = 2);
  Check(JA.D = '1234');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJsonArrayWithoutF), '');

  discogsJson := StringFromFile(WorkDir + discogsFileName);
  if discogsJson = '' then
  begin
    discogsJson := HttpGet(
      'https://api.discogs.com/artists/45/releases?page=1&per_page=100');
    FileFromString(discogsJson, WorkDir + discogsFileName);
  end;
  Check(IsValidJson(discogsJson));
  zendframeworkJson := StringFromFile(WorkDir + zendframeworkFileName);
  if zendframeworkJson = '' then
  begin
    zendframeworkJson := HttpGet(
      'https://api.github.com/users/zendframework/repos');
    FileFromString(zendframeworkJson, WorkDir + zendframeworkFileName);
  end;
  Check(IsValidJson(zendframeworkJson));
  TestGit([jpoIgnoreUnknownProperty], []);
  TestGit([jpoIgnoreUnknownProperty], [woHumanReadable]);

  U := RecordSaveJson(Trans, TypeInfo(TTestCustomJson2));
  Check(IsStringJson(pointer(U)), 'bin rec1');
  TRttiJson.RegisterFromText(TypeInfo(TTestCustomJson2Title),
    __TTestCustomJson2Title, [], [woHumanReadable]);
  TRttiJson.RegisterFromText(TypeInfo(TTestCustomJson2), __TTestCustomJson2, [],
    [woHumanReadable]);
  FillCharFast(Trans, sizeof(Trans), 0);
  U := RecordSaveJson(Trans, TypeInfo(TTestCustomJson2));
  Check(IsValidJson(U));
  CheckEqual(U,  #13#10'{'#13#10#9'"Transactions": '#13#10#9'['#13#10#9']'#13#10'}');
  for i := 1 to 10 do
  begin
    U :=
      '{"transactions":[{"TRTYPE":"INCOME","TRDATE":"2013-12-09 02:30:04","TRAA":"1.23",' +
      '"TRCAT1":{"TITYPE":"C1","TIID":"1","TICID":"","TIDSC30":"description1","TIORDER":"0","TIDEL":"false"},' +
      '"TRCAT2":{"TITYPE":"C2","TIID":"2","TICID":"","TIDSC30":"description2","TIORDER":"0","TIDEL":"false"},' +
      '"TRCAT3":{"TITYPE":"C3","TIID":"3","TICID":"","TIDSC30":"description3","TIORDER":"0","TIDEL":"false"},' +
      '"TRRMK":"Remark",' +
      '"TRACID":{"TITYPE":"AC","TIID":"4","TICID":"","TIDSC30":"account1","TIORDER":"0","TIDEL":"false"}}]}';
    TestTrans;
    U := RecordSaveJson(Trans, TypeInfo(TTestCustomJson2));
    TestTrans;
  end;
  U := RecordSaveJson(Trans, TypeInfo(TTestCustomJson2));
  FileFromString(U, WorkDir + 'transactions.json');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJson2Title), '');
  Rtti.RegisterFromText(TypeInfo(TTestCustomJson2), '');
  U := RecordSaveJson(Trans, TypeInfo(TTestCustomJson2));
  Check(IsStringJson(pointer(U)), 'bin rec2');
  TestTrans;

  Parser := TRttiJson.RegisterFromText(TypeInfo(TTestCustomDiscogs),
    __TTestCustomDiscogs, [jpoIgnoreUnknownProperty], []);
  FillCharFast(Disco, sizeof(Disco), 0);
  Check(PtrUInt(@Disco.releases) - PtrUInt(@Disco) = 3 * sizeof(integer));
  Check(sizeof(Disco.releases[0]) = 5 * sizeof(Pointer) + 2 * sizeof(integer));
  Check(sizeof(Disco) = sizeof(Pointer) + 3 * sizeof(integer));
  U := RecordSaveJson(Disco, TypeInfo(TTestCustomDiscogs));
  CheckEqual(U, '{"pagination":{"per_page":0,"items":0,"page":0},"releases":[]}');
  U := JsonReformat(discogsJson, jsonCompact);
  Check(IsValidJson(U));
  Check(JsonReformat(JsonReformat(discogsJson, jsonHumanReadable), jsonCompact) = U);
  Check(JsonReformat(JsonReformat(discogsJson, jsonUnquotedPropName), jsonCompact) = U);
  Check(JsonReformat(JsonReformat(U, jsonUnquotedPropName), jsonCompact) = U);
  RecordLoadJson(Disco, pointer(discogsJson), TypeInfo(TTestCustomDiscogs));
  Check(length(Disco.releases) <= Disco.pagination.items);
  for i := 0 to high(Disco.Releases) do
    Check(Disco.Releases[i].id > 0);
  TRttiJson(Parser).IncludeWriteOptions := [woHumanReadable];
  U := RecordSaveJson(Disco, TypeInfo(TTestCustomDiscogs));
  Check(IsValidJson(U));
  FileFromString(U, WorkDir + 'discoExtract.json');
  Finalize(Disco);
  FillCharFast(Disco, sizeof(Disco), 0);
  U := '{"pagination":{"per_page":1},"releases":[{"title":"TEST","id":10}]}';
  Check(IsValidJson(U));
  RecordLoadJson(Disco, UniqueRawUtf8(U), TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page = 1);
  Check(Disco.pagination.page = 0);
  if not CheckFailed(length(Disco.releases) = 1) then
  begin
    Check(Disco.releases[0].title = 'TEST');
    Check(Disco.releases[0].id = 10);
  end;
  Finalize(Disco);
  FillCharFast(Disco, sizeof(Disco), 0);
  U := '{"pagination":{},"releases":[{"Id":10},{"TITle":"blabla"}]}';
  Check(IsValidJson(U));
  RecordLoadJson(Disco, UniqueRawUtf8(U), TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page = 0);
  Check(Disco.pagination.page = 0);
  if not CheckFailed(length(Disco.releases) = 2) then
  begin
    Check(Disco.releases[0].title = '');
    Check(Disco.releases[0].id = 10);
    Check(Disco.releases[1].title = 'blabla');
    Check(Disco.releases[1].id = 0);
  end;
  U := '{"pagination":{"page":1},"releases":[{"title":"abc","id":2}]}';
  Check(IsValidJson(U));
  RecordLoadJson(Disco, UniqueRawUtf8(U), TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page = 0);
  Check(Disco.pagination.page = 1);
  if not CheckFailed(length(Disco.releases) = 1) then
  begin
    Check(Disco.releases[0].title = 'abc');
    Check(Disco.releases[0].id = 2);
  end;
  Rtti.RegisterFromText(TypeInfo(TTestCustomDiscogs), '');
  SetString(U, PAnsiChar('true'#0'footer,'), 12);
  Check(IdemPChar(GetJsonField(pointer(U), P), 'TRUE'));
  Check((P <> nil) and
        (P^ = #0));
  CheckEqual(U, 'true'#0'footer,', '3cce80e8df');
  // validates RawJson (custom) serialization
  Enemy := TEnemy.Create;
  try
    U := ObjectToJson(Enemy);
    Check(IsValidJson(U));
    CheckEqual(U,
      '{"Enabled":false,"Name":"","Offense":{"damage":{"min":0,"max":0},' +
      '"attackspeed":{"min":0,"max":0}}}', 'RawJson');
    Enemy.Off.Damage.Min := 10;
    Enemy.Off.AttackSpeed.Max := 100;
    U := ObjectToJson(Enemy);
    Check(IsValidJson(U));
    CheckEqual(U,
      '{"Enabled":false,"Name":"","Offense":{"damage":{"min":10,"max":0},' +
      '"attackspeed":{"min":0,"max":100}}}');
    FillcharFast(Enemy.Off, sizeof(Enemy.Off), 0);
    check(Enemy.Off.Damage.Min = 0);
    check(Enemy.Off.AttackSpeed.Max = 0);
    JsonToObject(Enemy, pointer(U), Valid);
    check(Valid);
    check(Enemy.Off.Damage.Min = 10);
    check(Enemy.Off.AttackSpeed.Max = 100);
  finally
    Enemy.Free;
  end;
end;

{
  Some numbers on Delphi XE8 + Windows 7 32-bit - taken with mORMot 2 at 7/27/21

  - JSON benchmark: 100,369 assertions passed  3.99s
     StrLen() in 878us, 21.8 GB/s
     IsValidUtf8(RawUtf8) in 8.79ms, 2.1 GB/s
     IsValidUtf8(PUtf8Char) in 9.27ms, 2 GB/s
     IsValidJson(RawUtf8) in 24.19ms, 810.5 MB/s
     IsValidJson(PUtf8Char) in 23.14ms, 846.9 MB/s
     JsonArrayCount(P) in 23.29ms, 841.7 MB/s
     JsonArrayCount(P,PMax) in 21.14ms, 0.9 GB/s
     JsonObjectPropCount() in 9.80ms, 1.1 GB/s
     TDocVariant in 115.91ms, 169.1 MB/s
     TDocVariant dvoInternNames in 173.42ms, 113 MB/s
     TOrmTableJson GetJsonValues in 23.87ms, 361.2 MB/s
     TOrmTableJson expanded in 38.45ms, 509.9 MB/s
     TOrmTableJson not expanded in 25.25ms, 341.4 MB/s
     DynArrayLoadJson in 57.25ms, 150.6 MB/s
     Delphi JSON in 344.26ms, 2.5 MB/s
     JsonDataObjects in 196.54ms, 43.8 MB/s
     SuperObject in 53.69ms, 16 MB/s
     X-SuperObject in 626.83ms, 704.4 KB/s
     Grijjy in 35.76ms, 24.1 MB/s
     dwsJSON in 20.05ms, 43 MB/s
     WinSoft WinJson in 80.64ms, 10.6 MB/s

  Some numbers on FPC 3.2 + Linux x86_64:

  - JSON benchmark: 100,299 assertions passed  813.66ms
     StrLen() in 820us, 23.3 GB/s
     IsValidUtf8(RawUtf8) in 1.45ms, 13.1 GB/s
     IsValidUtf8(PUtf8Char) in 2.21ms, 8.6 GB/s
     IsValidJson(RawUtf8) in 25.47ms, 769.5 MB/s
     IsValidJson(PUtf8Char) in 24.12ms, 812.4 MB/s
     JsonArrayCount(P) in 23.82ms, 822.8 MB/s
     JsonArrayCount(P,PMax) in 24.48ms, 800.6 MB/s
     JsonObjectPropCount() in 9.41ms, 1.1 GB/s
     TDocVariant in 127.77ms, 153.4 MB/s
     TDocVariant dvoInternNames in 143.68ms, 136.4 MB/s
     TOrmTableJson GetJsonValues in 24.64ms, 349.8 MB/s
     TOrmTableJson expanded in 40.20ms, 487.5 MB/s
     TOrmTableJson not expanded in 21.10ms, 408.5 MB/s
     DynArrayLoadJson in 62.35ms, 138.2 MB/s
     fpjson in 80.36ms, 10.7 MB/s
     SuperObject in 186.77ms, 4.6 MB/s

  - IsValidUtf8() has very efficient AVX2 asm on FPC + x86_64
  - TDocVariant dvoInternNames will recognize and intern the nested object
    field names, so memory consumption is likely to be reduced and unfragmented.
  - DynArrayLoadJson() parses the JSON directly into a dynamic array of record
    using our cached RTTI, so memory consumption will be as low as possible.
  - Most libraries claim they are "fast" but actually they are just faster than
    Delphi JSON which is (dead) slow. So only JsonDataObject and dwsJSON could
    claim to be optimized. fpjson is not so bad. And mORMot 2 flies for sure.
}

procedure TTestCoreProcess.JSONBenchmark;
const
  ITER = 20;
  ONLYLOG = false;
var
  people, notexpanded: RawUtf8;
  peoples: string;
  P: PUtf8Char;
  count, len, lennexp, i, interned: integer;
  dv: TDocVariantData;
  table: TOrmTableJson;
  timer: TPrecisionTimer;
  rec: TRecordPeopleDynArray;
  {$ifdef JSONBENCHMARK_FPJSON}
  fpjson: TJSONData;
  {$endif JSONBENCHMARK_FPJSON}
  {$ifdef JSONBENCHMARK_DELPHIJSON}
  djson: system.json.TJSONValue;
  {$endif JSONBENCHMARK_DELPHIJSON}
  {$ifdef JSONBENCHMARK_JDO}
  jdo: JsonDataObjects.TJsonBaseObject;
  {$endif JSONBENCHMARK_JDO}
  {$ifdef JSONBENCHMARK_SO}
  so: superobject.ISuperObject;
  s: supertypes.SOString;
  {$endif JSONBENCHMARK_SO}
  {$ifdef JSONBENCHMARK_XSO}
  xso: xsuperobject.ISuperArray;
  {$endif JSONBENCHMARK_XSO}
  {$ifdef JSONBENCHMARK_GRIJJY}
  g: TgoBsonArray;
  {$endif JSONBENCHMARK_GRIJJY}
  {$ifdef JSONBENCHMARK_DWS}
  dws: TdwsJSONValue;
  {$endif JSONBENCHMARK_DWS}
  {$ifdef JSONBENCHMARK_WSFT}
  ws: WinJson.TJson;
  {$endif JSONBENCHMARK_WSFT}
begin
  people := StringFromFile(WorkDir + 'People.json');
  if people = '' then
    exit; // need to run at least once the ORM tests
  len := length(people);
  check(len > 800000, 'unexpected people.json');
  timer.Start;
  for i := 1 to ITER do
    Check(StrLen(pointer(people)) = len);
  len := len * ITER;
  NotifyTestSpeed('StrLen()', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
    Check(IsValidUtf8(people));
  NotifyTestSpeed('IsValidUtf8(RawUtf8)', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
    Check(IsValidUtf8(PUtf8Char(pointer(people))));
  NotifyTestSpeed('IsValidUtf8(PUtf8Char)', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
    Check(IsValidJson(people));
  NotifyTestSpeed('IsValidJson(RawUtf8)', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
    Check(IsValidJsonBuffer(pointer(people)));
  NotifyTestSpeed('IsValidJson(PUtf8Char)', 0, len, @timer, ONLYLOG);
  P := @people[2]; // point just after initial '[' for JsonArrayCount
  count := JsonArrayCount(P);
  check(count > 8200); // = 8227 in current People.json ORM tests file
  timer.Start;
  for i := 1 to ITER do
    Check(JsonArrayCount(P) = count);
  NotifyTestSpeed('JsonArrayCount(P)', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
    Check(JsonArrayCount(P, P + length(people) - 1) = count);
  NotifyTestSpeed('JsonArrayCount(P,PMax)', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER * 5000 do
    Check(JsonObjectPropCount(P + 3) = 6, 'first TOrmPeople object');
  NotifyTestSpeed('JsonObjectPropCount()', 0, ITER * 5000 * 119, @timer, ONLYLOG);
  interned := DocVariantType.InternNames.Count;
  timer.Start;
  for i := 1 to ITER do
  begin
    dv.InitJson(people, JSON_OPTIONS_FAST);
    Check(dv.count = count);
    dv.Clear; // to reuse dv
  end;
  NotifyTestSpeed('TDocVariant', 0, len, @timer, ONLYLOG);
  Check(DocVariantType.InternNames.Count = interned, 'no intern');
  DocVariantType.InternNames.Clean;
  timer.Start;
  for i := 1 to ITER do
  begin
    dv.InitJson(people, JSON_OPTIONS_FAST + [dvoInternNames]);
    Check(dv.count = count);
    dv.Clear; // to reuse dv
  end;
  NotifyTestSpeed('TDocVariant dvoInternNames', 0, len, @timer, ONLYLOG);
  Check(DocVariantType.InternNames.Count - interned = 6, 'intern');
  Check(DocVariantType.InternNames.Clean = 6, 'clean');
  Check(DocVariantType.InternNames.Count = interned, 'cleaned');
  table := TOrmTableJson.Create('', people);
  try
    Check(table.RowCount = count);
    timer.Start;
    for i := 1 to ITER do
    begin
      notexpanded := table.GetJsonValues({expand=}false, 0, 65536);
      lennexp := length(notexpanded);
      Check(lennexp < length(people), 'notexpanded');
    end;
    NotifyTestSpeed('TOrmTableJson GetJsonValues', 0, lennexp * ITER, @timer, ONLYLOG);
  finally
    table.Free;
  end;
  timer.Start;
  for i := 1 to ITER do
  begin
    table := TOrmTableJson.Create('', people);
    try
      Check(table.RowCount = count);
    finally
      table.Free;
    end;
  end;
  NotifyTestSpeed('TOrmTableJson expanded', 0, len, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
  begin
    table := TOrmTableJson.Create('', notexpanded);
    try
      Check(table.RowCount = count);
    finally
      table.Free;
    end;
  end;
  NotifyTestSpeed('TOrmTableJson not expanded', 0, lennexp * ITER, @timer, ONLYLOG);
  timer.Start;
  for i := 1 to ITER do
  begin
    Check(DynArrayLoadJson(rec, people, TypeInfo(TRecordPeopleDynArray)));
    Check(length(rec) = count);
  end;
  NotifyTestSpeed('DynArrayLoadJson', 0, lennexp * ITER, @timer, ONLYLOG);
  {$ifdef JSONBENCHMARK_FPJSON}
  timer.Start;
  for i := 1 to ITER div 10 do // div 10 since fpjson speed is 10 MB/s :(
  begin
    fpjson := GetJSON(people, {utf8=}true);
    if not CheckFailed(fpjson <> nil) then
      try
        if not CheckFailed(fpjson.JSONType = jtArray) then
          Check((fpjson as TJSONArray).Count = count);
      finally
        fpjson.Free;
      end;
  end;
  NotifyTestSpeed('fpjson', 0, lennexp * ITER div 10, @timer, ONLYLOG);
  {$endif JSONBENCHMARK_FPJSON}
  {$ifdef JSONBENCHMARK_DELPHIJSON}
  timer.Start;
  for i := 1 to ITER div 10 do // div 10 since Delphi json speed is < 3 MB/s :o
  begin
    djson := system.json.TJSONObject.ParseJSONValue(people);
    if not CheckFailed(djson <> nil) then
      try
        if not CheckFailed(djson is system.json.TJSONArray) then
          Check((djson as system.json.TJSONArray).Count = count);
      finally
        djson.Free;
      end;
  end;
  NotifyTestSpeed('Delphi JSON', 0, lennexp * (ITER div 10), @timer, ONLYLOG);
  {$endif JSONBENCHMARK_DELPHIJSON}
  {$ifdef JSONBENCHMARK_JDO}
  timer.Start;
  for i := 1 to ITER do // JsonDataObjects speed is 40 MB/s ;)
  begin
    jdo := TJsonBaseObject.ParseUtf8(people);
    if not CheckFailed(jdo <> nil) then
      try
        if not CheckFailed(jdo is JsonDataObjects.TJsonArray) then
          Check((jdo as JsonDataObjects.TJsonArray).Count = count);
      finally
        jdo.Free;
      end;
  end;
  NotifyTestSpeed('JsonDataObjects', 0, lennexp * ITER, @timer, ONLYLOG);
  {$endif JSONBENCHMARK_JDO}
  {$ifdef JSONBENCHMARK_SO}
  s := supertypes.SOString(people); // convert to UTF-8 once
  timer.Start;
  for i := 1 to ITER div 10 do
  begin
    so := superobject.SO(s);
    if not CheckFailed(so <> nil) then
      if not CheckFailed(so.IsType(stArray)) then
        Check(so.AsArray.Length = count);
  end;
  NotifyTestSpeed('SuperObject', 0, lennexp * (ITER div 10), @timer, ONLYLOG);
  {$endif JSONBENCHMARK_SO}
  Utf8ToStringVar(people, peoples); // convert to UTF-8 once
  {$ifdef JSONBENCHMARK_XSO}
  timer.Start;
  for i := 1 to 1 do // X-SuperObject is 600KB/s 8(
  begin
    xso := xsuperobject.SA(peoples);
    if not CheckFailed(xso <> nil) then
      Check(xso.Length = count);
  end;
  NotifyTestSpeed('X-SuperObject', 0, lennexp, @timer, ONLYLOG);
  {$endif JSONBENCHMARK_SO}
  {$ifdef JSONBENCHMARK_GRIJJY}
  timer.Start;
  for i := 1 to ITER div 10 do
  begin
    g := TgoBsonArray.Parse(peoples);
    Check(g.Count = count);
  end;
  NotifyTestSpeed('Grijjy', 0, lennexp * (ITER div 10), @timer, ONLYLOG);
  {$endif JSONBENCHMARK_GRIJJY}
  {$ifdef JSONBENCHMARK_DWS}
  timer.Start;
  for i := 1 to ITER div 10 do
  begin
    dws := TdwsJSONValue.ParseString(peoples);
    try
      Check((dws as TdwsJSONArray).ElementCount = count);
    finally
      dws.Free;
    end;
  end;
  NotifyTestSpeed('dwsJSON', 0, lennexp * (ITER div 10), @timer, ONLYLOG);
  {$endif JSONBENCHMARK_DWS}
  {$ifdef JSONBENCHMARK_WSFT}
  WinJson.TJsonParser.Create.Free; // run it once for the trial popup to show
  timer.Start;
  for i := 1 to ITER div 10 do
  begin
    with WinJson.TJsonParser.Create do
      try
        ws := Parse(peoples);
        try
          if not CheckFailed(ws.IsArray) then
            Check((ws as WinJson.TJsonArray).ElementCount = Count);
        finally
          ws.Free;
        end;
      finally
        Free;
      end;
  end;
  NotifyTestSpeed('WinSoft WinJson', 0, lennexp * (ITER div 10), @timer, ONLYLOG);
  {$endif JSONBENCHMARK_WSFT}
end;

procedure TTestCoreProcess.WikiMarkdownToHtml;
begin
  // wiki
  CheckEqual(HtmlEscapeWiki('test'), '<p>test</p>');
  CheckEqual(HtmlEscapeWiki('te<b>st'), '<p>te&lt;b&gt;st</p>');
  CheckEqual(HtmlEscapeWiki('t *e* st'), '<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeWiki('t*e*st'), '<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeWiki('t\*e\*st'), '<p>t*e*st</p>');
  CheckEqual(HtmlEscapeWiki('t\*e*st'), '<p>t*e<em>st</em></p>');
  CheckEqual(HtmlEscapeWiki('t +e+ st'), '<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeWiki('t+e+st'), '<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeWiki('t `e` st'), '<p>t <code>e</code> st</p>');
  CheckEqual(HtmlEscapeWiki('t`e`st'), '<p>t<code>e</code>st</p>');
  CheckEqual(HtmlEscapeWiki('https://test'),
    '<p><a href="https://test" rel="nofollow">https://test</a></p>');
  CheckEqual(HtmlEscapeWiki('test'#13#10'click on http://coucouc.net toto'),
    '<p>test</p><p>click on <a href="http://coucouc.net" rel="nofollow">http://coucouc.net</a> toto</p>');
  CheckEqual(HtmlEscapeWiki(':test: :) joy:'),
    '<p>:test: ' + EMOJI_UTF8[eSmiley] + ' joy:</p>');
  CheckEqual(HtmlEscapeWiki(':innocent: smile'),
    '<p>' + EMOJI_UTF8[eInnocent] + ' smile</p>');
  CheckEqual(HtmlEscapeWiki(':test: :) a:joy:'),
    '<p>:test: ' + EMOJI_UTF8[eSmiley] + ' a:joy:</p>');
  CheckEqual(HtmlEscapeWiki(':test: :)'),
    '<p>:test: ' + EMOJI_UTF8[eSmiley] + '</p>');
  CheckEqual(HtmlEscapeWiki(':test: (:)'), '<p>:test: (:)</p>');
  CheckEqual(HtmlEscapeWiki(':test: :))'), '<p>:test: :))</p>');
  // Markdown
  CheckEqual(HtmlEscapeMarkdown('test'), '<p>test</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'toto'), '<p>test toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10#13#10'toto'), '<p>test</p><p>toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#10#10'toto'), '<p>test</p><p>toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#10#10#10'toto'), '<p>test</p><p> toto</p>');
  CheckEqual(HtmlEscapeMarkdown('te<b>st'), '<p>te<b>st</p>');
  CheckEqual(HtmlEscapeMarkdown('te<b>st', [heHtmlEscape]), '<p>te&lt;b&gt;st</p>');
  CheckEqual(HtmlEscapeMarkdown('t *e* st'), '<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t*e*st'), '<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\*e\*st'), '<p>t*e*st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\*e*st'), '<p>t*e<em>st</em></p>');
  CheckEqual(HtmlEscapeMarkdown('t **e** st'), '<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t**e**st'), '<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t _e_ st'), '<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t_e_st'), '<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\_e\_st'), '<p>t_e_st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\_e_st'), '<p>t_e<em>st</em></p>');
  CheckEqual(HtmlEscapeMarkdown('t __e__ st'), '<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t__e__st'), '<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t `e` st'), '<p>t <code>e</code> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t`e`st'), '<p>t<code>e</code>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t***e***st'), '<p>t<strong><em>e</strong></em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'click on http://coucouc.net toto'),
    '<p>test click on <a href="http://coucouc.net" rel="nofollow">http://coucouc.net</a> toto</p>');
  CheckEqual(HtmlEscapeMarkdown('[toto](http://coucou.net) titi'),
    '<p><a href="http://coucou.net" rel="nofollow">toto</a> titi</p>');
  CheckEqual(HtmlEscapeMarkdown('blabla ![img](static/img.jpg) blibli'),
    '<p>blabla <img alt="img" src="static/img.jpg"> blibli</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'    a*=10*2'#10'    b=20'#13#10'ended'),
    '<p>test</p><pre><code>a*=10*2'#$D#$A'b=20'#$D#$A'</code></pre><p>ended</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'``` a*=10*2'#10'  b=20'#13#10'```ended'),
    '<p>test</p><pre><code> a*=10*2'#$D#$A'  b=20'#$D#$A'</code></pre><p>ended</p>');
  CheckEqual(HtmlEscapeMarkdown('*te*st'#13#10'* one'#13#10'* two'#13#10'end'),
    '<p><em>te</em>st</p><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('+test'#13#10'+ one'#13#10'- two'#13#10'end'),
    '<p>+test</p><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'2. two'#13#10'end'),
    '<p>1test</p><ol><li>one</li><li>two</li></ol><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'7. two'#13#10'3. three'#13#10'4end'),
    '<p>1test</p><ol><li>one</li><li>two</li><li>three</li></ol><p>4end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'2. two'#13#10'+ one'#13#10'- two'#13#10'end'),
    '<p>1test</p><ol><li>one</li><li>two</li></ol><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('>test'#13#10'> quote'),
    '<p>>test</p><blockquote><p>quote</p></blockquote>');
  CheckEqual(HtmlEscapeMarkdown('>test'#13#10'> quote1'#10'> quote2'#13#10'end'),
    '<p>>test</p><blockquote><p>quote1</p><p>quote2</p></blockquote><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: :) joy:'),
    '<p>:test: ' + EMOJI_UTF8[eSmiley] + ' joy:</p>');
  CheckEqual(HtmlEscapeMarkdown(':innocent: :joy'),
    '<p>' + EMOJI_UTF8[eInnocent] + ' :joy</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: :)'),
    '<p>:test: ' + EMOJI_UTF8[eSmiley] + '</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: (:)'), '<p>:test: (:)</p>');
end;

procedure TTestCoreProcess._TDecimal128;

  procedure test(const hi, lo: QWord; const expected: RawUtf8;
    special: TDecimal128SpecialValue = dsvValue);
  var
    v, v2: TDecimal128;
  begin
    v.Bits.hi := hi;
    v.Bits.lo := lo;
    CheckEqual(v.ToText, expected);
    v2.SetZero;
    Check(v2.FromText(expected) = special);
    if special <> dsvValue then
      exit;
    Check(v2.Equals(v));
    CheckEqual(v2.ToText, expected);
    v2.SetZero;
    if expected[1] <> '-' then
      Check(v2.FromText('000' + LowerCase(expected)) = dsvValue)
    else
      Check(v2.FromText(LowerCase(expected)) = dsvValue);
    Check(v2.Equals(v));
  end;

  procedure Test2(const fromvalue, expected: RawUtf8;
    h: QWord = 0; l: QWord = 0);
  var
    v: TDecimal128;
  begin
    Check(v.FromText(fromvalue) = dsvValue);
    Check(v.ToText = expected);
    if (h = 0) and
       (l = 0) then
      exit;
    Check(v.Bits.lo = l);
    Check(v.Bits.hi = h);
  end;

var
  v, v2: TDecimal128;
  s: TDecimal128SpecialValue;
  str: RawUtf8;
  i: integer;
  o: variant;
begin
  // see https://github.com/mongodb/libbson/blob/master/tests/test-decimal128.c
  Check(v.FromText('') = dsvError);
  Check(v.FromText('.') = dsvError);
  Check(v.FromText('.e') = dsvError);
  Check(v.FromText('i') = dsvError);
  Check(v.FromText('invalid') = dsvError);
  Check(v.FromText('1invalid') = dsvError);
  Check(v.FromText('E02') = dsvError);
  Check(v.FromText('E+02') = dsvError);
  Check(v.FromText('e+02') = dsvError);
  Check(v.FromText('1E02') = dsvValue);
  Check(v.FromText('1invalidE02') = dsvError);
  Check(v.FromText('..1') = dsvError);
  Check(v.FromText('0') = dsvZero);
  Check(v.ToText = '0');
  for s := dsvNan to high(s) do
  begin
    v.SetSpecial(s);
    CheckEqual(v.ToText, DECIMAL128_SPECIAL_TEXT[s], ToText(s)^);
    CheckEqual(ord(v.IsSpecial), ord(s));
    if s < dsvMin then
    begin
      v.SetZero;
      Check(v.FromText(LowerCase(DECIMAL128_SPECIAL_TEXT[s])) = s);
      Check(v.IsSpecial = s);
    end;
  end;
  v.SetZero;
  Check(v.ToText = '0');
  test(0, 0, '0', dsvZero);
  test($3040000000000000, 0, '0', dsvZero);
  test($3040000000000000, 1, '1');
  test($3040000000000000, 2, '2');
  test(QWord($b040000000000000), 2, '-2');
  test(QWord($b040000000000000), 1, '-1');
  test(QWord($b040000000000000), 0, '-0');
  test($303e000000000000, 1, '0.1');
  test($3034000000000000, $4d2, '0.001234');
  test($3040000000000000, $1cbe991a14, '123456789012');
  test($302a000000000000, $75aef40, '0.00123400000');
  test($2ffc3cde6fff9732, QWord($de825cd07e96aff2),
    '0.1234567890123456789012345678901234');
  test($3040ffffffffffff, QWord($ffffffffffffffff),
    '5192296858534827628530496329220095');
  test($5ffe314dc6448d93, $38c15b0a00000000,
    '1.000000000000000000000000000000000E+6144');
  test($000, $001, '1E-6176');
  test(QWord($8000000000000000), $001, '-1E-6176');
  test($3108000000000000, $000009184db63eb1, '9.999987654321E+112');
  test($5fffed09bead87c0, $378d8e63ffffffff,
    DECIMAL128_SPECIAL_TEXT[dsvMax]);
  test($0001ed09bead87c0, $378d8e63ffffffff,
    '9.999999999999999999999999999999999E-6143');
  test(QWord($dfffed09bead87c0), $378d8e63ffffffff,
    DECIMAL128_SPECIAL_TEXT[dsvMin]);
  test($304c000000000000, $41a, '1.050E+9');
  test($3042000000000000, $41a, '1.050E+4');
  test($3040000000000000, $069, '105');
  test($3042000000000000, $069, '1.05E+3');
  test($3046000000000000, $001, '1E+3');
  test($3298000000000000, $000, '0E+300');
  test($2b90000000000000, $000, '0E-600');
  Test2('10e0', '10');
  Test2('1e1', '1E+1');
  Test2('10e-1', '1.0');
  Test2('1000000000000000000000000000000000000000',
    '1.000000000000000000000000000000000E+39',
    $304c314dc6448d93, $38c15b0a00000000);
  Test2('10000000000000000000000000000000000',
    '1.000000000000000000000000000000000E+34',
    $3042314dc6448d93, $38c15b0a00000000);
  Test2('1000000000000000000000000000000000',
    '1000000000000000000000000000000000',
    $3040314dc6448d93, $38c15b0a00000000);
  Test2('12345678901234567e6111', '1.2345678901234567E+6127',
    $5ffe000000000000, 12345678901234567);
  Test2('-100E-10', '-1.00E-8', QWord($b02c000000000000), 100);
  v.SetZero;
  for i := 0 to 4000 do
  begin
    if i > 1000 then
      inc(v.Bits.c[0], i * 7)
    else
      v.Bits.c[0] := i;
    str := v.ToText;
    Check(GetCardinal(pointer(str)) = v.Bits.c[0]);
    if i = 0 then
      continue;
    Check(v2.FromText(str) = dsvValue);
    Check(v2.Equals(v));
  end;
  for i := -1000 to 100 do
  begin
    v.FromInt32(i);
    str := v.ToText;
    Check(GetInteger(pointer(str)) = i);
    if i = 0 then
      continue;
    Check(v2.FromText(str) = dsvValue);
    Check(v2.Equals(v));
  end;
  v.FromCurr(0);
  Check(v.ToText = '0.0000');
  Check(v.ToCurr = 0);
  v.FromCurr(3.14);
  Check(v.ToText = '3.1400');
  for i := -160 to 160 do
  begin
    v.FromFloat(i / 4);
    v.ToText(str);
    Check(GetExtended(pointer(str)) * 4 = i);
    Check(v.ToFloat * 4 = i);
    v.FromCurr(i / 16);
    v.ToText(str);
    Check(StrToCurr64(pointer(str)) = i * 625);
    Check(v.ToCurr * 16 = i);
    o := NumberDecimal(i / 8);
    Check(v.FromVariant(o));
    Check(v.ToCurr * 8 = i);
  end;
end;

procedure TTestCoreProcess._BSON;
const
  BSONAWESOME = '{"BSON":["awesome",5.05,1986]}';
  BSONAWESOMEBIN = #$31#0#0#0#4'BSON'#0#$26#0#0#0#2'0'#0#8#0#0#0'awesome'#0 +
    #1'1'#0'333333'#$14#$40#$10'2'#0#$c2#7#0#0#0#0;
  BSONID = '507F191E810C19729DE860EA';
  REGEX = '{"$regex":"acme.*corp","$options":"i"}';
  REGEX2 = '{name:"John",field:/acme.*corp/i}';

  procedure CheckRegEx(o: variant);
  var
    u, u2: RawUtf8;
  begin
    u := VariantSaveMongoJson(o, modMongoStrict);
    CheckEqual(u, '{"name":"John","field":' + REGEX + '}');
    u2 := VariantSaveMongoJson(o, modMongoStrict);
    CheckEqual(u, u2, 'call twice');
    u2 := VariantSaveJson(o);
    CheckEqual(u, u2);
    u := VariantSaveMongoJson(o, modMongoShell);
    CheckEqual(u, REGEX2);
  end;

var
  o, od, o2, value: variant;
  d, d2: TDateTime;
  oid, oid2: TBsonObjectID;
  oids: array of TBsonObjectID;
  bsonDat, temp, bin: RawByteString;
  i, j: integer;
  b: PByte;
  elem, item: TBsonElement;
  iter: TBsonIterator;
  name, u, u2, u3, json: RawUtf8;
  arr: TRawUtf8DynArray;
  st: string;
  timer: TPrecisionTimer;
  dec: TDecimal128;

  procedure CheckElemIsBsonArray;
  var
    b: PByte;
  begin
    Check(elem.Kind = betArray);
    Check(elem.Name = 'BSON');
    item.Index := -1;
    b := elem.Element;
    BsonParseLength(b, 38);
    Check(b = elem.Data.DocList);
    while item.FromNext(b) do
    begin
      case item.Index of
        0:
          Check(item.ToVariant = 'awesome');
        1:
          CheckSame(item.ToVariant, 5.05);
        2:
          Check(item.ToVariant = 1986);
      else
        Check(false);
      end;
    end;
  end;

begin
  // see http://docs.mongodb.org/manual/reference/object-id
  oid.FromText('507f191e810c19729de860ea');
  Check(oid.UnixCreateTime = bswap32($507f191e));
  u := oid.ToText;
  Check(u = BSONID);
  o := ObjectID('507f191e810c19729de860ea');
  Check(TVarData(o).VType = BsonVariantType.VarType);
  u := ToUtf8(string(o));
  Check(u = BSONID, 'variant bsonid to string');
  d2 := Iso8601ToDateTime('2012-10-17T20:46:22');
  od := d2;
  Check(TVarData(od).VType = varDate);
  {$ifdef FPC} // FPC doesn't allow direct cast from varDate to double :(
  CheckSame(TVarData(od).VDate, d2);
  d := double(o);
  {$else}
  CheckSame(od, d2);
  d := o;
  {$endif FPC}
  DateTimeToIso8601StringVar(d, 'T', st);
  CheckSame(d, d2, 1E-4, st);
  CheckSame(o, d2, 1E-4, st);
  CheckSame(TBsonVariantData(o).VObjectID.CreateDateTime, d2, 1E-4);
  o2 := o;
  Check(double(o) = double(o2));
  o := ObjectID;
  Check(Abs(NowUtc - double(o)) < 0.1);
  oid.FromText(ToUtf8(string(o)));
  Check(Abs(NowUtc - oid.CreateDateTime) < 0.1);
  oid2.ComputeNew;
  Check(oid.MachineID.b1 = oid2.MachineID.b1);
  Check(oid.MachineID.b2 = oid2.MachineID.b2);
  Check(oid.MachineID.b3 = oid2.MachineID.b3);
  Check(oid.ProcessID = oid2.ProcessID);
  o2 := ObjectID;
  {$ifdef FPC} // FPC bug: sysvartotdatetime doesn't handle custom variants :(
  Check(double(o2) >= double(o), o);
  {$else}
  Check(TDateTime(o2) >= TDateTime(o), o);
  {$endif FPC}
  oid2.ComputeNew;
  j := 100000;
  timer.Start;
  for i := 1 to j do
  begin
    oid.ComputeNew;
    Check(not oid.Equal(oid2));
    oid2 := oid;
    Check(oid.Equal(oid2));
  end;
  NotifyTestSpeed('TBsonObjectID.ComputeNew', j, 0, @timer);
  SetLength(oids, 300);
  for i := 0 to high(oids) do
  begin
    oids[i].ComputeNew;
    for j := 0 to i - 1 do
      Check(not oids[i].Equal(oids[j]), '24 bit collision');
  end;
  //Check(GetCurrentProcessId<>oid.ProcessID,'Expected overflow');
  o := _Json('{"double_params":[-12.12345678,-9.9E-15,-9.88E-15,-9E-15]}',
    [dvoReturnNullForUnknownProperty, dvoAllowDoubleValue]);
  json := TDocVariantData(o).ToJson;
  {$ifndef EXTENDEDTOSHORT_USESTR}
  check(json = '{"double_params":[-12.12345678,-9.9E-15,-9.88E-15,-9E-15]}');
  {$endif EXTENDEDTOSHORT_USESTR}
  CheckSame(double(TDocVariantData(o).A['double_params'].value[1]), -9.9E-15);
  // floats are stored as varCurrency by default in _Json()
  o := _Json('{"value":99.99}');
  d := _Safe(o)^.D['value'];
  CheckSame(d, 99.99, DOUBLE_SAME, '99.99');
  CheckEqual(DoubleToStr(d), '99.99');
  // see http://bsonspec.org/#/specification
  o := _Json('{"hello": "world"}');
  bsonDat := Bson(TDocVariantData(o));
  Check(bsonDat = #$16#0#0#0#2'hello'#0#6#0#0#0'world'#0#0);
  b := pointer(bsonDat);
  Check(BsonParseLength(b, $16) = length(bsonDat));
  Check(elem.FromNext(b));
  Check(elem.Kind = betString);
  Check(elem.Name = 'hello');
  Check(elem.Data.Text = 'world');
  Check(not elem.FromNext(b));
  Check(elem.Kind = betEof);
  u := BsonToJson(pointer(bsonDat), betDoc, length(bsonDat));
  CheckEqual(u, '{"hello":"world"}');
  elem.FromDocument(bsonDat);
  Check(elem.Kind = betDoc);
  Check(elem.DocItemToVariant('hello', value));
  check(value = 'world');
  Check(not elem.DocItemToVariant('hello2', value));
  Check(elem.DocItemToRawUtf8('hello') = 'world');
  Check(elem.DocItemToRawUtf8('hello2') = '');
  Check(elem.DocItemToInteger('hello', 1234) = 1234);
  Check(iter.Init(bsonDat));
  Check(iter.Next);
  Check(iter.Item.Kind = betString);
  Check(iter.Item.Name = 'hello');
  Check(iter.Item.Data.Text = 'world');
  Check(not iter.Next);
  b := pointer(bsonDat);
  BsonParseLength(b);
  Check(BsonParseNextElement(b, name, value));
  Check(name = 'hello');
  Check(value = 'world');
  Check(not BsonParseNextElement(b, name, value));
  o := _Json('{"BSON": ["awesome", 5.05, 1986]}');
  bsonDat := Bson(TDocVariantData(o));
  Check(length(bsonDat) = $31);
  Check(bsonDat = BSONAWESOMEBIN);
  b := pointer(bsonDat);
  Check(BsonParseLength(b, $31) = length(bsonDat));
  Check(elem.FromNext(b));
  CheckElemIsBsonArray;
  Check(not elem.FromNext(b));
  Check(elem.Kind = betEof);
  u := BsonToJson(pointer(bsonDat), betDoc, length(bsonDat));
  CheckEqual(u, BSONAWESOME);
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u, BSONAWESOME);
  u := VariantSaveJson(o);
  CheckEqual(u, BSONAWESOME);
  Check(Bson(['BSON', _Arr(['awesome', 5.05, 1986])]) = bsonDat);
  o2 := BsonVariantType[bsonDat];
  Check(VariantSaveJson(o2) = u);
  o2 := BsonVariant('{"BSON": ["awesome", 5.05, 1986]}');
  u := VariantSaveMongoJson(o2, modMongoStrict);
  CheckEqual(u, BSONAWESOME);
  o2 := BsonVariant(['BSON', _Arr(['awesome', 5.05, 1986])]);
  CheckEqual(VariantSaveMongoJson(o2, modMongoStrict), BSONAWESOME);
  o2 := BsonVariant(TDocVariantData(o));
  CheckEqual(VariantSaveMongoJson(o2, modMongoStrict), BSONAWESOME);
  o2 := BsonVariant('{%:[?,?,?]}', ['BSON'], ['awesome', 5.05, 1986]);
  CheckEqual(VariantSaveMongoJson(o2, modMongoStrict), BSONAWESOME);
  b := pointer(bsonDat);
  u := ToUtf8(string(o2));
  CheckEqual(u, '{BSON:["awesome",5.05,1986]}', 'TBsonVariant: mongoShell syntax');
  BsonParseLength(b);
  Check(BsonParseNextElement(b, name, value, asDocVariantPerReference));
  Check(name = 'BSON');
  elem.FromVariant(name, value, temp);
  CheckElemIsBsonArray;
  Check(not BsonParseNextElement(b, name, value));
  o := BsonDocumentToDoc(bsonDat);
  Check(TVarData(o).VType = DocVariantType.VarType);
  Check(DocVariantType.IsOfType(o));
  Check(o.Name(0) = 'BSON');
  Check(o._(0)._Kind = ord(dvArray));
  Check(o.bson._Kind = ord(dvArray));
  Check(o.bson._count = 3);
  Check(o.bson._(0) = 'awesome');
  CheckSame(double(o.bson._(1)), 5.05);
  Check(o.bson._(2) = 1986);
  Check(o.dummy = null);
  Check(o.Exists('bson'));
  Check(not o.Exists('dummy'));
  Check(o.NameIndex('bson') = 0);
  Check(o.NameIndex('dummy') < 0);
  DocVariantData(o.bson).ToRawUtf8DynArray(arr);
  Check(length(arr) = 3);
  Check(RawUtf8ArrayToCsv(arr) = 'awesome,5.05,1986');
  Check(DocVariantData(o.bson).ToJson = '["awesome",5.05,1986]');
  u := '{"BSON":["awesome",5.05,1986],"name":"John","one":1.2}';
  _Json(u, o);
  Check(VariantSaveJson(BsonVariant(u)) = u);
  bsonDat := Bson(TDocVariantData(o));
  b := pointer(bsonDat);
  BsonParseLength(b);
  Check(BsonParseNextElement(b, name, value));
  Check(name = 'BSON');
  elem.FromVariant(name, value, temp);
  CheckElemIsBsonArray;
  Check(BsonParseNextElement(b, name, value));
  Check(name = 'name');
  Check(value = 'John');
  elem.FromVariant(name, value, temp);
  Check(elem.name = 'name');
  Check(elem.Data.Text = 'John');
  Check(BsonParseNextElement(b, name, value));
  Check(name = 'one');
  CheckSame(value, 1.2);
  elem.FromVariant(name, value, temp);
  Check(elem.name = 'one');
  CheckSame(unaligned(PDouble(elem.Element)^), 1.2);
  Check(not BsonParseNextElement(b, name, value));
  Check(BsonToJson(pointer(bsonDat), betDoc, length(bsonDat)) = u);
  elem.FromVariant('test', o, temp);
  Check(elem.Name = 'test');
  Check(elem.Kind = betDoc);
  Check(VariantSaveMongoJson(o, modMongoStrict) = u);
  Check(VariantSaveMongoJson('test', modMongoStrict) = '"test"');
  Check(VariantSaveMongoJson(1.5, modMongoStrict) = '1.5');
  Check(VariantSaveMongoJson(_Json('{BSON:["awesome",5.05,1986]}'),
    modMongoStrict) = BSONAWESOME);
  Check(VariantSaveMongoJson(_JsonFast('{ BSON : ["awesome", 5.05, 1986] }'),
    modMongoStrict) = BSONAWESOME);
  Check(VariantSaveMongoJson(_JsonFast('{ ''BSON'' : ["awesome", 5.05, 1986] } '),
    modMongoStrict) = BSONAWESOME);
  Check(VariantSaveJson(o) = u);
  Check(VariantSaveJson('test') = '"test"');
  Check(VariantSaveJson(1.5) = '1.5');
  Check(VariantSaveJson(_Json('{BSON:["awesome",5.05,1986]}')) = BSONAWESOME);
  Check(VariantSaveJson(_JsonFast('{ BSON : ["awesome", 5.05, 1986] }')) = BSONAWESOME);
  Check(VariantSaveJson(_JsonFast('{ ''BSON'' : ["awesome", 5.05, 1986] } ')) = BSONAWESOME);
  Check(Bson('{BSON:["awesome",5.05,1986]}', [], []) = BSONAWESOMEBIN);
  Check(Bson('{ BSON : ["awesome", 5.05, 1986] }', [], []) = BSONAWESOMEBIN);
  Check(Bson('{ ''BSON'' : ["awesome", 5.05, 1986] } ', [], []) = BSONAWESOMEBIN);
  Check(Bson('{%:[?,?,?]}', ['BSON'], ['awesome', 5.05, 1986]) = BSONAWESOMEBIN);
  Check(Bson('{%:?}', ['BSON'], [_Arr(['awesome', 5.05, 1986])]) = BSONAWESOMEBIN);
  Check(Bson(['BSON', '[', 'awesome', 5.05, 1986, ']']) = BSONAWESOMEBIN);
  Check(Bson(['BSON', '[', 'awesome', 5.05, 1986]) = BSONAWESOMEBIN);
  o2 := BsonVariantType[bsonDat];
  Check(VariantSaveJson(o2) = u);
  _Json('{BSON: ["test", 5.05, 1986]}', o);
  Check(VariantSaveMongoJson(o, modMongoStrict) = '{"BSON":["test",5.05,1986]}');
  u := VariantSaveMongoJson(_Obj(['name', 'John', 'doc', _Obj(['one', 1, 'two',
    _Arr(['one', 2])])]), modMongoStrict);
  CheckEqual(u, '{"name":"John","doc":{"one":1,"two":["one",2]}}');
  Check(VariantSaveJson(BsonVariant(u)) = u);
  Check(BsonDocumentToJson(BsonFieldSelector(['a', 'b', 'c'])) = '{"a":1,"b":1,"c":1}');
  Check(BsonDocumentToJson(BsonFieldSelector('a,b,c')) = '{"a":1,"b":1,"c":1}');
  Check(VariantSaveMongoJson(BsonVariantFieldSelector(['a', 'b', 'c']),
    modMongoShell) = '{a:1,b:1,c:1}');
  Check(VariantSaveMongoJson(BsonVariantFieldSelector('a,b,c'), modMongoShell) =
    '{a:1,b:1,c:1}');
  o := _Obj(['id', ObjectID(BSONID), 'name', 'John', 'date', variant(d2)]);
  u := VariantSaveMongoJson(o, modNoMongo);
  u2 := FormatUtf8('{"id":"%","name":"John","date":"%"}', [BSONID, st]);
  CheckEqual(u, u2);
  u3 := VariantSaveJson(BsonVariant(u));
  Check(u3 = FormatUtf8('{"id":"%","name":"John","date":{"$date":"%"}}',
    [BSONID, st]));
  u3 := VariantSaveMongoJson(BsonVariant(u), modNoMongo);
  Check(u3 = u);
  u := VariantSaveMongoJson(o, modMongoShell);
  CheckEqual(u, FormatUtf8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',
    [BSONID, st]));
  u3 := VariantSaveJson(BsonVariant(u));
  u := VariantSaveJson(o);
  CheckEqual(u, FormatUtf8('{"id":{"$oid":"%"},"name":"John","date":"%"}',
    [BSONID, st]));
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u, FormatUtf8('{"id":{"$oid":"%"},"name":"John","date":{"$date":"%"}}',
    [BSONID, st]));
  Check(u3 = u);
  _Json(u, o2);
  u := VariantSaveMongoJson(o2, modMongoShell);
  CheckEqual(u, FormatUtf8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',
    [BSONID, st]));
  _Json(u, o2);
  u := VariantSaveMongoJson(o2, modNoMongo);
  CheckEqual(u, u2);
  o2 := _JsonFmt('{ id: objectID( "%" ) , name: "John", date: new date( "%" ) }',
    [BSONID, st], []);
  u := VariantSaveMongoJson(o2, modNoMongo);
  CheckEqual(u, u2);
  o2 := _JsonFmt('{id:objectID(?),name:?,date:ISODate(?)}', [],
    [BSONID, 'John', st]);
  u := VariantSaveMongoJson(o2, modNoMongo);
  CheckEqual(u, u2);
  u := VariantSaveMongoJson(o2, modMongoShell);
  CheckEqual(u, FormatUtf8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',
    [BSONID, st]));
  _Json(u, o2);
  u := VariantSaveMongoJson(o2, modNoMongo);
  CheckEqual(u, u2);
  bin := VariantSave(o2);
  u := VariantSaveMongoJson(VariantLoad(bin, @JSON_OPTIONS[true]), modNoMongo);
  CheckEqual(u, u2);
  check(VariantSaveMongoJson(VariantLoad(bin, @JSON_OPTIONS[true]), modNoMongo)
    = u2, 'twice to ensure bin is untouched');
  u := VariantSaveMongoJson(_Json('{id:ObjectId(),name:"John"}'), modNoMongo);
  Check(IdemPChar(Pointer(u), '{"ID":"'), 'ObjectId() constructor ');
  Check(PosEx('","name":"John"}', u) = 32);
  u2 := VariantSaveMongoJson(_Json('{id:ObjectId(),name:"John"}'), modNoMongo);
  Check(u2 <> u, 'should be genuine');
  o := _JsonFmt('{type:{$in:?}}', [], [_Arr(['food', 'snack'])]);
  u := VariantSaveJson(o);
  CheckEqual(u, '{"type":{"$in":["food","snack"]}}');
  u := VariantSaveMongoJson(o, modMongoShell);
  CheckEqual(u, '{type:{$in:["food","snack"]}}');
  o := _Json('{"hello": null}');
  Check(TVarData(o).VType = DocVariantVType);
  check(string(o) = '{"hello":null}');
  o := _Json('{"hello": world}');
  Check(TVarData(o).VType = varEmpty, 'invalid JSON content');
  CheckRegEx(_Json(
    '{name:"John",field:{ "$regex": "acme.*corp", $options: "i" }}'));
  CheckRegEx(_Json(REGEX2));
  CheckRegEx(_JsonFast(
    '{"name":"John",field:{ "$regex": "acme.*corp", $options: "i" }}'));
  CheckRegEx(_JsonFast(REGEX2));
  temp := Bson(REGEX2);
  b := pointer(temp);
  u := BsonToJson(b, betDoc, 0, modMongoStrict);
  CheckEqual(u, '{"name":"John","field":' + REGEX + '}');
  o2 := BsonVariant(REGEX2);
  Check(string(o2) = '{name:"John",field:/acme.*corp/i}', 'MongoShell in string cast');
  Check(VariantSaveJson(o2) = u);
  temp := Bson('{name:?,field:/%/i}', ['acme.*corp'], ['John']);
  b := pointer(temp);
  u2 := BsonToJson(b, betDoc, 0, modMongoStrict);
  CheckEqual(u, u2);
  u := VariantSaveMongoJson(_Json(
    '{name:"John",date: new date() , field: /acme.*corp/i}'), modMongoStrict);
  u2 := VariantSaveMongoJson(_Json(
    '{name:"John",date:new date(),field:/acme.*corp/i}'), modMongoStrict);
  o := _Json(u);
  o2 := _Json(u2);
  Check(o.name = o2.name);
  d := TDateTime(o.date);
  d2 := TDateTime(o2.date);
  Check(d > NowUtc - 1);
  Check(d2 - d < 0.1);
  u := VariantSaveMongoJson(o.Field, modMongoStrict);
  u2 := VariantSaveMongoJson(o2.Field, modMongoStrict);
  CheckEqual(u, u2);
  CheckEqual(u, REGEX);
  u := VariantSaveMongoJson(o.Field, modMongoShell);
  u2 := VariantSaveMongoJson(o2.Field, modMongoShell);
  CheckEqual(u, u2);
  CheckEqual(u, '/acme.*corp/i');
  u := VariantSaveMongoJson(o.Field, modMongoStrict);
  u2 := VariantSaveMongoJson(o2.Field, modMongoStrict);
  CheckEqual(u, u2);
  CheckEqual(u, REGEX);
  u := VariantSaveJson(o.Field);
  u2 := VariantSaveJson(o2.Field);
  CheckEqual(u, u2);
  CheckEqual(u, REGEX);
  o := _Json('{ tags: { $in: [ /^be/, /^st/ ] } }');
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u,
    '{"tags":{"$in":[{"$regex":"^be","$options":""},{"$regex":"^st","$options":""}]}}');
  temp := Bson(u, [], []);
  b := pointer(temp);
  u2 := VariantSaveMongoJson(o, modMongoShell);
  Check(u2 = '{tags:{$in:[/^be/,/^st/]}}');
  u := VariantSaveMongoJson(_Json(u), modMongoShell);
  CheckEqual(u, u2);
  u2 := BsonToJson(b, betDoc, 0, modMongoShell);
  CheckEqual(u, u2);
  temp := Bson('{id:ObjectId(),doc:{name:?,date:ISODate(?)}}', [],
    ['John', NowUtc]);
  b := pointer(temp);
  u := BsonToJson(b, betDoc, 0, modMongoShell);
  Check(IdemPChar(pointer(u), '{ID:OBJECTID("'));
  Check(PosEx('"),doc:{name:"John",date:ISODate("', u) > 10);
  u := BsonDocumentToJson(Bson(['doc', '{', 'name', 'John', 'year', 1982, '}',
    'id', 123]));
  CheckEqual(u, '{"doc":{"name":"John","year":1982},"id":123}');
  u := BsonDocumentToJson(Bson(['doc', '{', 'name', 'John', 'abc', '[', 'a', 'b',
    'c', ']', '}', 'id', 123]));
  CheckEqual(u, '{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
  o2 := NumberDecimal('123.5600');
  u := VariantSaveJson(o2);
  CheckEqual(u, '{"$numberDecimal":"123.5600"}');
  o := _Json('{ num: ' + u + '}');
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u, '{"num":{"$numberDecimal":"123.5600"}}');
  u := VariantSaveMongoJson(o, modMongoShell);
  CheckEqual(u, '{num:NumberDecimal("123.5600")}');
  o := BsonVariant(['num', o2]);
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u, '{"num":{"$numberDecimal":"123.5600"}}');
  u := VariantSaveMongoJson(o, modMongoShell);
  CheckEqual(u, '{num:NumberDecimal("123.5600")}');
  o := _ObjFast(['num', o2]);
  u := VariantSaveMongoJson(o, modMongoStrict);
  CheckEqual(u, '{"num":{"$numberDecimal":"123.5600"}}');
  o2 := _JsonFast(u);
  {$ifdef FPC} // TCustomVariantType.CompareOp not yet supported :(
  check(string(o) = string(o2), 'o=o2');
  {$else}
  check(o = o2, 'o=o2');
  {$endif FPC}
  u := VariantSaveMongoJson(o, modMongoShell);
  CheckEqual(u, '{num:NumberDecimal("123.5600")}');
  o2 := _JsonFast(u);
  {$ifdef FPC} // TCustomVariantType.CompareOp not yet supported :(
  check(string(o) = string(o2), 'o=o2');
  {$else}
  check(o = o2, 'o=o2');
  {$endif FPC}
  temp := Bson(u, [], []);
  b := pointer(temp);
  u2 := BsonToJson(b, betDoc, 0, modMongoShell);
  CheckEqual(u, u2);
  u2 := BsonToJson(b, betDoc, 0, modMongoStrict);
  check(u2 = '{"num":{"$numberDecimal":"123.5600"}}');
  check(dec.FromVariant(o2.num));
  check(dec.ToText = '123.5600');
  o2 := dec.ToVariant;
  u := VariantSaveJson(o2);
  CheckEqual(u, '{"$numberDecimal":"123.5600"}');
end;

procedure TTestCoreProcess._TDocVariant;

  procedure CheckDoc(var Doc: TDocVariantData; ExpectedYear: integer = 1972);
  var
    json: RawUtf8;
  begin
    if CheckFailed(Doc.VarType = DocVariantVType) then
      exit;
    Check(Doc.Kind = dvObject);
    Check(Doc.Count = 2);
    Check(Doc.Names[0] = 'name');
    Check(Doc.Values[0] = 'John');
    Check(variant(Doc)._kind = ord(dvObject));
    Check(variant(Doc).name = 'John');
    Check(variant(Doc).name = Doc.Value['name']);
    Check(variant(Doc).birthYear = ExpectedYear);
    Check(variant(Doc).birthYEAR = Doc.Value['birthYear']);
    Check(variant(Doc)._Count = 2);
    Check(variant(Doc).name(0) = 'name');
    Check(variant(Doc).name(1) = 'birthyear');
    Check(variant(Doc)._(0) = 'John');
    Check(variant(Doc)._(1) = ExpectedYear);
    Check(variant(Doc).Value(0) = 'John');
    Check(variant(Doc).Value(1) = ExpectedYear);
    json := '{"name":"John","birthyear":' + Int32ToUtf8(ExpectedYear) + '}';
    Check(Doc.ToJson = json);
    Check(variant(Doc)._Json = json);
    Check(variant(Doc)._Json__ = json, 'pseudo methods use IdemPChar');
    Check(VariantSaveMongoJson(variant(Doc), modMongoStrict) = json);
    Check(VariantToUtf8(variant(Doc)) = json);
    Check(Doc.U['name'] = 'John');
    Check(Doc.I['birthyear'] = ExpectedYear);
  end;

var
  discogs: RawUtf8;

  procedure CheckNestedDoc(aOptions: TDocVariantOptions = []);
  var
    json, json2: RawUtf8;
    Doc, Doc2: TDocVariantData;
    Doc2Doc, V, Disco: variant;
    i: Integer;
  begin
    V := _Json('["one",2,3]', aOptions);
    Check(V._Json = '["one",2,3]');
    Doc.InitObject(['name', 'John', 'birthyear', 1972],
      aOptions + [dvoReturnNullForUnknownProperty]);
    CheckDoc(Doc);
    Check(Doc.Value['toto'] = null);
    Check(variant(Doc).toto = null);
    Check(Doc.Value[10] = null);
    Doc2.InitObject(['id', 10, 'doc', _Obj(['name', 'John', 'birthyear', 1972],
      aOptions)]);
    Check(Doc2.Kind = dvObject);
    Check(variant(Doc2)._kind = ord(dvObject));
    Check(Doc2.Count = 2);
    Check(Doc2.Value['id'] = 10);
    Check(variant(Doc2).id = 10);
    Check(variant(Doc2).Doc._kind = ord(dvObject));
    Doc2Doc := variant(Doc2).Doc;
    CheckDoc(DocVariantData(Doc2Doc)^);
    CheckDoc(DocVariantData(variant(Doc2).Doc)^);
    Doc2Doc := Doc2.GetValueOrRaiseException('doc');
    json := '{"id":10,"doc":{"name":"John","birthyear":1972}}';
    Check(Doc2.ToJson = json);
    Check(Doc2.I['id'] = 10);
    Check(Doc2.O['doc'].U['name'] = 'John');
    Check(Doc2.O['doc'].I['birthyear'] = 1972);
  //Doc2Doc.birthyear := 1980;
    variant(DocVariantData(Doc2Doc)^).birthyear := 1980;
    json2 := Doc2.ToJson;
    if dvoValueCopiedByReference in aOptions then
    begin
      Check(json2 = '{"id":10,"doc":{"name":"John","birthyear":1980}}');
      Check(Doc2.O['doc'].I['birthyear'] = 1980);
    end
    else
    begin
      Check(json2 = json);
      Check(Doc2.O['doc'].I['birthyear'] = 1972);
    end;
    _Json(json, V, aOptions);
    Check(V._count = 2);
    Check(V.id = 10);
    Check(V.doc._kind = ord(dvObject));
    Check(V.doc.name = 'John');
    Check(V.doc.birthYear = 1972);
    if discogs <> '' then
    begin
      FileFromString(JsonReformat(discogs),
        WorkDir + ChangeFileExt(discogsFileName, '2.json'));
      Disco := _Json(discogs, aOptions);
      Check(Disco.releases._count <= Disco.pagination.items);
      for i := 0 to Disco.Releases._count - 1 do
      begin
        Check(Disco.Releases._(i).id > 0);
        V := Disco.Releases._(i);
        Check(V._count > 0);
        Check(V.title <> '');
      end;
//    if aOptions=[] then
//      FileFromString(TDocVariantData(Disco).ToJson,'discoVariant.json');
    end;
    _Json('[]', V, aOptions);
    Check(V._kind = ord(dvArray));
    Check(V._count = 0);
    _Json('null', V, aOptions);
    Check(V._kind = ord(dvObject));
    Check(V._count = 0);
  end;

  procedure DoChange(var oSeasons: variant);
  var
    i: integer;
    oSeason: variant;
  begin
    for i := 0 to oSeasons._Count - 1 do
    begin
      oSeason := oSeasons._(i);
      oSeason.Name := 'CHANGED !';
      oSeason.Extra := 'blabla';
    end;
  end;

  {$ifdef HASITERATORS}
  procedure DoEnumerators;
  var
    vd, v2: TDocVariantData;
    v: PVariant;
    d: PDocVariantData;
    f: TDocVariantFields;
  begin
    vd.InitArray([1, 2, 3, 4]);
    for f in vd do
    begin
      Check(f.Name = nil);
      Check(integer(f.Value^) in [1..4]);
    end;
    for v in vd.Items do
      Check(integer(v^) in [1..4]);
    vd.Clear;
    for f in vd do
      Check(f.Name = pointer(1)); // should not iterate
    for v in vd.Items do
      Check(v = nil); // should not iterate
    vd.InitJson('[{a:1,b:1}, 1, "no object", {a:2,b:2}]');
    v2.InitFast;
    for f in vd do
    begin
      Check(f.Name = nil);
      v2.AddItem(f.Value^);
    end;
    CheckEqual(vd.ToJson, v2.ToJson);
    Check(vd.Equals(v2));
    v2.Clear;
    v2.InitFast;
    for v in vd.Items do
      v2.AddItem(v^);
    CheckEqual(vd.ToJson, v2.ToJson);
    Check(vd.Equals(v2));
    v2.Clear;
    v2.InitFast;
    for d in vd.Objects do
    begin
      Check(DocVariantType.IsOfType(variant(d^)));
      v2.AddItem(variant(d^));
    end;
    CheckEqual(v2.ToJson, '[{"a":1,"b":1},{"a":2,"b":2}]');
    vd.Clear;
    vd.InitJson('{a:1,b:2,c:3}');
    v2.Clear;
    v2.InitFast;
    for f in vd do
    begin
      Check(f.Name <> nil);
      v2.AddValue(f.Name^, f.Value^);
    end;
    CheckEqual(vd.ToJson, v2.ToJson);
    Check(vd.Equals(v2));
    for v in vd.Items do
      Check(v = nil); // should not iterate
    for f in vd.Fields do
    begin
      CheckEqual(f.Name^, v2.Names[0]);
      Check(f.Value^ = v2.Values[0]);
      v2.Delete(0);
    end;
    Check(v2.Count = 0);
  end;
  {$endif HASITERATORS}

const
  MAX = 20000;
  TEST_DATA_1 = '[' +
    '{"REC_ID":1,"CHANNEL":117,"PHONE":"5004392222,12345678","RELATION_ID":10,' +
    '"TIMESTAMP_CALL":"2017-10-26T04:48:14"},{"REC_ID":2,"CHANNEL":null,"PHONE":' +
    '"1234","RELATION_ID":11,"TIMESTAMP_CALL":"2017-10-26T04:48:14"},' +
    '{"REC_ID":3,"CHANNEL":174,"PHONE":"9149556917","RELATION_ID":12,' +
    '"TIMESTAMP_CALL":"2017-10-26T04:48:14"}]';
var
  Doc, Doc2: TDocVariantData;
  vr: TTVarRecDynArray;
  i, ndx: PtrInt;
  V, V1, V2: variant;
  s, j: RawUtf8;
  vd: double;
  vs: single;
  lTable: TOrmTableJson;
  lRefreshed: Boolean;
begin
  Doc.Init;
  Check(Doc.Kind = dvUndefined);
  Check(variant(Doc)._kind = ord(dvUndefined));
  Doc.AddValue('name', 'Jonas');
  Doc.AddValue('birthyear', 1972);
  Check(Doc.Value['name'] = 'Jonas');
  Check(Doc.Value['birthyear'] = 1972);
  Check(Doc.U['name'] = 'Jonas');
  Check(Doc.I['birthyear'] = 1972);
  Doc.Value['name'] := 'John';
  Check(Doc.Value['name'] = 'John');
  CheckDoc(Doc);
  Doc.Clear;
  Doc.InitFast;
  Check(Doc.Kind = dvUndefined);
  Check(variant(Doc)._kind = ord(dvUndefined));
  Doc.AddValue('name', 'Jonas');
  Doc.AddValue('birthyear', 1972);
  Check(Doc.Value['name'] = 'Jonas');
  Check(Doc.Value['birthyear'] = 1972);
  Check(Doc.U['name'] = 'Jonas');
  Check(Doc.I['birthyear'] = 1972);
  Doc.Value['name'] := 'John';
  Check(Doc.Value['name'] = 'John');
  Check(Doc.U['name'] = 'John');
  CheckDoc(Doc);
  Doc2.InitJson(Doc.ToJson);
  Check(Doc2.Equals(Doc));
  CheckDoc(Doc2);
  Doc.Clear;
  Doc.InitArray(['one', 2, 3.0]);
  Check(variant(Doc)._kind = ord(dvArray));
  Check(variant(Doc)._count = 3);
  if not CheckFailed(Doc.Count = 3) then
  begin
    Check(Doc.Values[0] = 'one');
    Check(Doc.Values[1] = 2);
    Check(Doc.Values[2] = 3.0);
    Check(Doc.Value[0] = 'one');
    Check(Doc.Value[1] = 2);
    Check(Doc.Value[2] = 3.0);
    for i := 0 to Doc.Count - 1 do
      Check(VariantCompare(Doc.Values[i], Doc.Value[i]) = 0);
  end;
  Check(Doc.ToJson = '["one",2,3]');
  Check(Variant(Doc)._Json = '["one",2,3]');
  Doc.ToArrayOfConst(vr);
  s := FormatUtf8('[?,?,?]', [], vr, true);
  check(s = '["one",2,3]');
  s := FormatUtf8('[%,%,%]', vr, [], true);
  check(s = '[one,2,3]');
  s := FormatUtf8('[?,?,?]', [], Doc.ToArrayOfConst, true);
  check(s = '["one",2,3]');
  s := FormatUtf8('[%,%,%]', Doc.ToArrayOfConst, [], true);
  check(s = '[one,2,3]');
  V := _Json(' [ "one" ,2,3 ]   ');
  Check(V._count = 3);
  with TDocVariantData(V) do
  begin
    Check(Count = 3);
    Check(Values[0] = 'one');
    Check(Values[1] = 2);
    Check(Values[2] = 3.0);
  end;
  for i := 0 to V._count - 1 do
    Check(V._(i) = Doc.Values[i]);
  V.Add(4);
  Check(V._count = 4);
  for i := 0 to 2 do
    Check(V._(i) = Doc.Values[i]);
  Check(V._(3) = 4);
  V._ := 'a5';
  Check(V._count = 5);
  for i := 0 to 2 do
    Check(V._(i) = Doc.Values[i]);
  Check(V._(3) = 4);
  Check(V._(4) = 'a5');
  Check(V._Json = '["one",2,3,4,"a5"]');
  discogs := StringFromFile(WorkDir + discogsFileName);
  CheckNestedDoc([]);
  CheckNestedDoc([dvoValueCopiedByReference]);
  CheckNestedDoc([dvoJsonObjectParseWithinString]);
  CheckNestedDoc([dvoJsonObjectParseWithinString, dvoValueCopiedByReference]);
  V1 := _Obj(['name', 'John', 'year', 1972], [dvoValueCopiedByReference]);
  V2 := V1;             // creates a reference to the V1 instance
  V2.name := 'James';   // modifies V2.name, but also V1.name
  Check(V1.name = 'James');
  Check(V2.name = 'James');
  {$ifdef FPC}
  Check(V1._Json = '{"name":"James","year":1972}');
  {$else}
  Check(V1 = '{"name":"James","year":1972}');
  {$endif FPC}
  _Unique(V1);          // change options of V1 to be by-value
  V2 := V1;             // creates a full copy of the V1 instance
  V2.name := 'John';    // modifies V2.name, but not V1.name
  Check(V1.name = 'James');
  Check(V2.name = 'John');
  V1 := _Arr(['root', V2]); // created as by-value by default, as V2 was
  Check(V1._Count = 2);
  _UniqueFast(V1);      // change options of V1 to be by-reference
  V2 := V1;
  Check(V1._(1)._Json = '{"name":"John","year":1972}');
  {$ifdef FPC}
  TDocVariantData(V1).Values[1].name := 'Jim';
  Check(V1._Json = '["root",{"name":"Jim","year":1972}]');
  Check(V2._Json = '["root",{"name":"Jim","year":1972}]');
  {$else}
  V1._(1).name := 'Jim';
  Check(V1 = '["root",{"name":"Jim","year":1972}]');
  Check(V2 = '["root",{"name":"Jim","year":1972}]');
  {$endif FPC}
  _UniqueFast(V2); // now V1 modifications should not affect V2
  Doc.Clear;
  Doc.Init;
  for i := 0 to MAX do
  begin
    UInt32ToUtf8(i, s);
    Check(Doc.AddValue(s, s) = i);
  end;
  Check(Doc.Count = MAX + 1);
  for i := 0 to MAX do
    Check(GetInteger(Pointer(Doc.Names[i])) = i);
  for i := 0 to MAX do
    Check(Doc.Values[i] = i);
  Doc2.Clear;
  check(Doc2.Count = 0);
  s := Doc.ToJson;
  CheckHash(s, 2110959969, 'bigjson');
  Doc2.InitJson(s);
  Check(Doc2.Equals(Doc));
  check(Doc2.Count = MAX + 1);
  for i := 0 to MAX do
    Check(Doc2.Values[i] = Doc.Values[i]);
  for i := MAX downto 0 do
    if i and 1 = 0 then
      Doc.Delete(i);
  Check(Doc.Count = MAX div 2);
  check(Doc2.Count = MAX + 1);
  for i := 0 to Doc.Count - 1 do
    Check(Doc.Names[i] = Doc.Values[i]);
  s := Doc2.ToJson;
  CheckHash(s, 2110959969, 'bigjson2');
  Check(TDocVariantData(V1)._[1].U['name'] = 'Jim');
  Check(TDocVariantData(V1)._[1].I['year'] = 1972);
  {$ifdef FPC}
  _Safe(V1)^.AddItem(3.1415);
  Check(V1._Json = '["root",{"name":"Jim","year":1972},3.1415]');
  TDocVariantData(V1)._[1].Delete('year');
  Check(V1._Json = '["root",{"name":"Jim"},3.1415]');
  TDocVariantData(V1).Delete(1);
  Check(V1._Json = '["root",3.1415]');
  TDocVariantData(V2).DeleteByProp('name', 'JIM', true);
  Check(V2._Json = '["root",{"name":"Jim","year":1972}]');
  TDocVariantData(V2).DeleteByProp('name', 'JIM', false);
  Check(V2._Json = '["root"]');
  {$else}
  V1.Add(3.1415);
  Check(V1 = '["root",{"name":"Jim","year":1972},3.1415]');
  V1._(1).Delete('year');
  Check(V1 = '["root",{"name":"Jim"},3.1415]');
  V1.Delete(1);
  Check(V1 = '["root",3.1415]');
  TDocVariantData(V2).DeleteByProp('name', 'JIM', true);
  Check(V2 = '["root",{"name":"Jim","year":1972}]');
  TDocVariantData(V2).DeleteByProp('name', 'JIM', false);
  Check(V2 = '["root"]');
  {$endif FPC}
  V1 := _ObjFast(['n1', 'v1']);
  Check(V1._JSON = '{"n1":"v1"}');
  V1.Add('n2', 'v2');
  Check(V1._JSON = '{"n1":"v1","n2":"v2"}', 'FPC 3.2+ inverted order');
  s := '{"Url":"argentina","Seasons":[{"Name":"2011/2012","Url":"2011-2012",' +
    '"Competitions":[{"Name":"Ligue1","Url":"ligue-1"},{"Name":"Ligue2","Url":"ligue-2"}]},' +
    '{"Name":"2010/2011","Url":"2010-2011","Competitions":[{"Name":"Ligue1","Url":"ligue-1"},' +
    '{"Name":"Ligue2","Url":"ligue-2"}]}]}';
  CheckHash(s, $BF60E202);
  V1 := _Json(s);
  V2 := V1.seasons;
  DoChange(V2);
  j := VariantSaveJson(V1);
  Check(j <> s);
  CheckHash(j, $6998B225, 'changed');
  CheckHash(VariantSaveJson(V2), $92FEB37B);
  V1 := _Json(s);
  V2 := V1.seasons;
  _Unique(V2);
  DoChange(V2);
  Check(VariantSaveJson(V1) = s);
  CheckHash(VariantSaveJson(V2), $92FEB37B);
  V2 := TDocVariant.NewUnique(V1.Seasons);
  DoChange(V2);
  Check(VariantSaveJson(V1) = s);
  CheckHash(VariantSaveJson(V2), $92FEB37B);
  V2 := _copy(V1.Seasons);
  DoChange(V2);
  Check(VariantSaveJson(V1) = s);
  CheckHash(VariantSaveJson(V2), $92FEB37B);
  s := _Safe(V1.Seasons)^.ToNonExpandedJson;
  Check(s =
    '{"fieldCount":3,"rowCount":2,"values":["Name","Url","Competitions",' + '"2011/2012","2011-2012",[{"Name":"Ligue1","Url":"ligue-1"},{"Name":"Ligue2"' +
    ',"Url":"ligue-2"}],"2010/2011","2010-2011",[{"Name":"Ligue1","Url":"ligue-1"}' +
    ',{"Name":"Ligue2","Url":"ligue-2"}]]}');
  V := _Json('{result:{data:{"1000":"D1", "1001":"D2"}}}');
  Check(V.result._Json = '{"data":{"1000":"D1","1001":"D2"}}');
  Check(V.result.data.Exists('1000'));
  Check(V.result.data.Exists('1001'));
  Check(not V.result.data.Exists('1002'));
  Check(DocVariantData(V.result.data).Value['1000'] = 'D1');
  Check(V.result.data.Value(0) = 'D1');
  Check(V.result.data.Value('1000') = 'D1');
  Check(V.result.data.Value('1001') = 'D2');
  V := _Obj(['Z', 10, 'name', 'John', 'year', 1972, 'a', 1], []);
  j := VariantSaveJson(V);
  Check(j = '{"Z":10,"name":"John","year":1972,"a":1}');
  TDocVariantData(V).SortByName;
  j := VariantSaveJson(V);
  Check(j = '{"a":1,"name":"John","year":1972,"Z":10}');
  TDocVariantData(V).SortByName(@StrComp);
  j := VariantSaveJson(V);
  Check(j = '{"Z":10,"a":1,"name":"John","year":1972}');
  V := _JsonFast('{"Database":"\u201d\u00c9\u00c3\u00b6\u00b1\u00a2\u00a7\u00ad\u00a5\u00a4"}');
  j := VariantToUtf8(V.Database);
  Check((j <> '') and
        (j[1] = #$E2) and
        (j[2] = #$80) and
        (j[3] = #$9D), 'e2809d');
  V1 := _Arr([]);
  vs := 1.5;
  _Safe(V1)^.AddItem(vs);
  CheckEqual(VariantSaveJson(V1), '[1.5]', 'VariantSaveJson');
  vd := 1.7;
  _Safe(V1)^.AddItem(vd);
  CheckEqual(VariantSaveJson(V1), '[1.5,1.7]');
  V2 := _obj(['id', 1]);
  Check(VariantSaveJson(V2) = '{"id":1}');
  V1.Add(V2);
  Check(VariantSaveJson(V1) = '[1.5,1.7,{"id":1}]');
  s := 'abc';
  V1.Add(s);
  Check(VariantSaveJson(V1) = '[1.5,1.7,{"id":1},"abc"]');
  RawUtf8ToVariant('def', V2);
  _Safe(V1)^.AddItem(V2);
  Check(VariantSaveJson(V1) = '[1.5,1.7,{"id":1},"abc","def"]');
  Doc.Clear;
  Doc.InitObjectFromPath('name', 'toto');
  check(Doc.ToJson = '{"name":"toto"}');
  Doc.Clear;
  Doc.InitObjectFromPath('people.age', 31);
  check(Doc.ToJson = '{"people":{"age":31}}');
  check(Doc.O['people'].ToJson = '{"age":31}');
  check(Doc.O['people2'].ToJson = 'null');
  Doc.O_['people2'].AddValue('name', 'toto');
  check(Doc.ToJson = '{"people":{"age":31},"people2":{"name":"toto"}}');
  check(Doc.A['arr'].ToJson = 'null');
  Doc.A_['arr'].AddItems([1, 2.2, '3']);
  check(Doc.ToJson = '{"people":{"age":31},"people2":{"name":"toto"},"arr":[1,2.2,"3"]}');
  Doc.Clear;
  check(Doc.A['test'].ToJson = 'null');
  Doc.A_['test']^.AddItems([1, 2]);
  j := Doc.ToJson;
  check(j = '{"test":[1,2]}');
  check(Doc.A['test'].ToJson = '[1,2]');
  Doc.A_['test']^.AddItems([3, 4]);
  check(Doc.ToJson = '{"test":[1,2,3,4]}');
  check(Doc.A['test'].ToJson = '[1,2,3,4]');
  Doc.Clear;
  check(not Doc.FlattenAsNestedObject('wrong'));
  Doc.InitJson('{"p.a1":5,"p.a2":"dfasdfa"}');
  check(not Doc.FlattenAsNestedObject('wrong'));
  check(Doc.ToJson = '{"p.a1":5,"p.a2":"dfasdfa"}');
  check(Doc.FlattenAsNestedObject('p'));
  check(Doc.ToJson = '{"p":{"a1":5,"a2":"dfasdfa"}}');
  check(not Doc.FlattenAsNestedObject('p'));
  s := '[{"Val1":"blabla","Val2":"bleble"},{"Val1":"blibli","Val2":"bloblo"}]';
  V := _Json(s);
  V1 := _Copy(V._(0)); // expect a true instance for v1.Val1 := ... below
  check(V1.val1 = 'blabla');
  V2 := _Obj([]); // or TDocVariant.New(v2);
  V2.Val1 := 'blublu';
  V2.Val2 := 'blybly';
  V1.Val1 := V2.Val1;
  V1.Val2 := V2.Val2;
  check(VariantSaveJson(V1) = VariantSaveJson(V2));
  Doc.Clear;
  V := _Json('{"ID": 1,"Notation": "ABC", "Price": 10.1, "CustomNotation": "XYZ"}');
  Doc.InitCopy(V, []);
  Doc.I['ID'] := 2;
  Doc.Delete('CustomNotation');
  s := Doc.ToJson;
  check(s = '{"ID":2,"Notation":"ABC","Price":10.1}');
  s := VariantSaveJson(V);
  check(s = '{"ID":1,"Notation":"ABC","Price":10.1,"CustomNotation":"XYZ"}');
  {$ifdef HASITERATORS}
  DoEnumerators;
  {$endif HASITERATORS}
  // some tests to avoid regression about bugs reported by users on forum
  lTable := TOrmTableJson.Create('');
  try
    lTable.UpdateFrom(TEST_DATA_1, lRefreshed, nil);
    ndx := lTable.FieldIndex('RELATION_ID');
    Check(ndx = 3);
    lTable.SortFields(ndx);
    Doc.Clear;
    i := lTable.SearchFieldSorted('10', {RELATION_ID}ndx);
    lTable.ToDocVariant(i, variant(Doc));
    Doc.Delete('REC_ID');
    Doc.Clear;
    i := lTable.SearchFieldSorted('11', {RELATION_ID}ndx);
    lTable.ToDocVariant(i, variant(Doc));
    V := Doc.Value['PHONE'];
    check(V = '1234');
  finally
    lTable.Free;
  end;
end;

// wrapper used to test GetPublishedMethods()
function GetPublishedMethodAddr(aClass: TClass; const aName: RawUtf8): pointer;
var
  methods: TPublishedMethodInfoDynArray;
  m: PtrInt;
begin
  if (aClass <> nil) and
     (aName <> '') then
    for m := 0 to GetPublishedMethods(nil, methods, aClass) - 1 do
      if IdemPropNameU(methods[m].Name, aName) then
      begin
        result := methods[m].Method.Code;
        exit;
      end;
  result := nil;
end;


procedure TTestCoreProcess._RTTI;
var
  i: Integer;
  tmp: RawUtf8;
  auto: TPersistentAutoCreateFieldsTest;
  s: TSynLogInfos;
  astext: boolean;
  P: PUtf8Char;
  eoo: AnsiChar;
  e: TEmoji;
begin
  check(EMOJI_UTF8[eNone] = '');
  checkEqual(BinToHex(EMOJI_UTF8[eGrinning]), 'F09F9880');
  checkEqual(BinToHex(EMOJI_UTF8[ePray]), 'F09F998F');
  check(EmojiFromText(Pointer(EMOJI_UTF8[eGrinning]), 4) = eNone);
  check(EmojiFromText(nil, 0) = eNone);
  checkEqual(EmojiToDots('toto'), 'toto');
  for e := low(e) to high(e) do
  begin
    check(EmojiFromText(pointer(EMOJI_TEXT[e]), length(EMOJI_TEXT[e])) = e);
    if e = eNone then
      continue;
    check(length(EMOJI_UTF8[e]) = 4);
    P := Pointer(EMOJI_UTF8[e]);
    checkEqual(NextUtf8Ucs4(P), $1f5ff + ord(e));
    FormatUtf8(':smile % ok', [EMOJI_TAG[e]], tmp);
    P := pointer(tmp);
    check(EmojiParseDots(P) = eNone);
    check(IdemPChar(P, 'SMILE :'));
    inc(P, 6);
    check(P^ = ':');
    check(EmojiParseDots(P) = e);
    check(IdemPChar(P, ' OK'));
    checkEqual(EmojiToDots(EMOJI_UTF8[e]), EMOJI_TAG[e]);
    checkEqual(EmojiToDots(' ' + EMOJI_UTF8[e] + ' '), ' ' + EMOJI_TAG[e] + ' ');
    checkEqual(EmojiToDots(EmojiFromDots(tmp)), tmp);
  end;
  tmp := ':) :( :JoY: :o :|';
  P := pointer(tmp);
  check(EmojiParseDots(P) = eSmiley);
  check(P^ = ' ');
  inc(P);
  check(EmojiParseDots(P) = eFrowning);
  check(IdemPChar(P, ' :JOY:'));
  inc(P);
  check(EmojiParseDots(P) = eJoy);
  check(P^ = ' ');
  inc(P);
  check(EmojiParseDots(P) = eOpen_mouth);
  check(P^ = ' ');
  inc(P);
  check(EmojiParseDots(P) = eExpressionless);
  check(P^ = #0);
  with PRttiInfo(TypeInfo(TSynLogInfo))^.EnumBaseType^ do
    for i := 0 to integer(high(TSynLogInfo)) do
    begin
      {$ifdef VERBOSE}
      writeln(i, ' ', GetEnumName(i)^, ' ', GetEnumNameTrimed(i));
      {$endif}
      tmp := GetEnumNameTrimed(i);
      Check(GetEnumNameValue(GetEnumName(i)^) = i);
      Check(GetEnumNameTrimedValue(tmp) = i);
      Check(GetEnumNameTrimedValue(pointer(tmp)) = i);
      Check(GetEnumNameValue(tmp) = i);
      Check(GetEnumNameValue(pointer(tmp)) = i);
      Check(GetEnumNameValue(
        mormot.core.rtti.GetEnumName(TypeInfo(TSynLogInfo), i)^) = i);
      Check(mormot.core.rtti.GetEnumNameValue(
        TypeInfo(TSynLogInfo), pointer(tmp), length(tmp), true) = i);
      tmp := GetEnumName(i)^;
      Check(mormot.core.rtti.GetEnumNameValue(
        TypeInfo(TSynLogInfo), pointer(tmp), length(tmp)) = i);
    end;
  for astext := false to true do
  begin
    integer(s) := 0;
    for i := -1 to ord(high(TSynLogInfo)) do
    begin
      if i >= 0 then
        SetBit(s, i);
      tmp := SaveJson(s, TypeInfo(TSynLogInfos), astext);
      if astext then
        case i of
          -1:
            Check(tmp = '[]');
          0:
            Check(tmp = '["sllNone"]');
        else
          if i = ord(high(TSynLogInfo)) then
            Check(tmp = '["*"]');
        end
      else
        Check(GetCardinal(pointer(tmp)) = cardinal(s));
      tmp := tmp + ','; // mimics GetJsonField layout
      P := pointer(tmp);
      eoo := ' ';
      Check(mormot.core.json.GetSetNameValue(TypeInfo(TSynLogInfos), P, eoo) =
        cardinal(s));
      Check(eoo = ',');
    end;
  end;
  Check(PRttiInfo(TypeInfo(TSynLogInfos))^.SetEnumType =
    PRttiInfo(TypeInfo(TSynLogInfo))^.EnumBaseType);
  with PRttiInfo(TypeInfo(TOrmTest))^ do
  begin
    Check(InheritsFrom(TOrmTest));
    Check(InheritsFrom(TOrm));
    Check(not InheritsFrom(TOrmPeople));
  end;
  Check(GetDisplayNameFromClass(nil) = '');
  Check(GetDisplayNameFromClass(TOrm) = 'Orm');
  Check(GetDisplayNameFromClass(TOrmPeople) = 'People');
  Check(GetDisplayNameFromClass(TObject) = 'Object');
  Check(GetDisplayNameFromClass(TOrmTable) = 'Table');
  Check(GetDisplayNameFromClass(TSynValidateRest) = 'ValidateRest');
  Check(GetPublishedMethodAddr(TOrm, 'ABC') = nil);
  Check(GetPublishedMethodAddr(TRestServer, 'ABC') = nil);
  Check(GetPublishedMethodAddr(TRestServer, 'STAT') =
    TRestServer.MethodAddress('STAT'));
  Check(GetPublishedMethodAddr(TRestServer, 'timestamp') =
    TRestServer.MethodAddress('TIMEstamp'));
  auto := TPersistentAutoCreateFieldsTest.CreateFake;
  try
    Check(auto.Value1 <> nil);
    Check(auto.Value2 <> nil);
    tmp := ObjectToJson(auto);
    Check(tmp = '{"Text":"text","Value1":{"Real":1.5,"Imaginary":2.5},' +
      '"Value2":{"Real":1.7,"Imaginary":2.7}}');
  finally
    auto.Free;
  end;
end;

procedure TTestCoreProcess.UrlEncoding;
var
  i: integer;
  s, t: RawUtf8;
  d: RawUtf8;
begin
  for i := 1 to 100 do
  begin
    s := RandomUtf8(i);
    t := UrlEncode(s);
    Check(UrlDecode(t) = s);
    d := 'seleCT=' + t + '&where=' + Int32ToUtf8(i);
    Check(UrlEncode(['seleCT', s, 'where', i]) = '?' + d);
  end;
end;

procedure TTestCoreProcess._TSelectStatement;
var
  Stmt: TSelectStatement;
  Props: TOrmProperties;
  bits: TFieldBits;
  withID: boolean;

  procedure NewStmt(const SQL: RawUtf8);
  begin
    Stmt.Free;
    Stmt := TSelectStatement.Create(SQL,
      Props.Fields.IndexByName, Props.SimpleFieldSelect);
    Check(Stmt.SqlStatement = SQL, 'Statement should be valid');
  end;

  procedure CheckIdData(limit, offset: integer);
  begin
    Check(Stmt.TableName = 'tab');
    Check(Stmt.Where = nil, 'no WHERE clause');
    Check((length(Stmt.Select) = 2) and
          (Stmt.Select[0].Field = 0) and
          (Props.Fields.List[Stmt.Select[1].Field - 1].name = 'Data'));
    Check(Stmt.Limit = limit);
    Check(Stmt.Offset = offset);
  end;

  procedure CheckWhere(isOR: Boolean);
  begin
    Check(Stmt.TableName = 'tab');
    Check(length(Stmt.Where) = 2);
    Check(Stmt.Where[0].Field = 0);
    Check(Stmt.Where[0].Operation = opGreaterThanOrEqualTo);
    Check(Stmt.Where[0].ValueInteger = 10);
    Check(Stmt.Where[1].JoinedOR = isOR);
    Check(Props.Fields.List[Stmt.Where[1].Field - 1].name = 'YearOfBirth');
    Check(Stmt.Where[1].Operation = opGreaterThan);
    Check(Stmt.Where[1].ValueInteger = 1600);
    Check(Stmt.Limit = 10);
    Check(Stmt.Offset = 20);
    Check((length(Stmt.Select) = 2) and
          (Stmt.Select[1].Field = 0) and
          (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'Data'));
    Check(Stmt.OrderByField = nil);
  end;

begin
  Stmt := nil;
  Props := TOrmPeople.OrmProps;
  NewStmt('select * from atable');
  Check(Stmt.TableName = 'atable');
  Check(Stmt.Where = nil);
  Stmt.SelectFieldBits(bits, withID);
  Check(withID);
  Check(IsEqual(bits, Props.SimpleFieldsBits[ooSelect]));
  Check(Stmt.OrderByField = nil);
  NewStmt('select iD,Data from tab');
  CheckIdData(0, 0);
  Check(Stmt.OrderByField = nil);
  NewStmt('select iD,Data from tab order by firstname');
  CheckIdData(0, 0);
  Check((length(Stmt.OrderByField) = 1) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName'));
  Check(Stmt.OrderByFieldDesc = []);
  NewStmt('select iD,Data from tab order by firstname desc');
  CheckIdData(0, 0);
  Check((length(Stmt.OrderByField) = 1) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName'));
  Check(Stmt.OrderByFieldDesc = [0]);
  NewStmt('select rowid , Data from tab order by firstname , lastname desc');
  CheckIdData(0, 0);
  Check((length(Stmt.OrderByField) = 2) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName') and
        (Props.Fields.List[Stmt.OrderByField[1] - 1].name = 'LastName'));
  Check(Stmt.OrderByFieldDesc = [1]);
  NewStmt('select rowid,Data from tab order by firstname,lastname limit 10');
  CheckIdData(10, 0);
  Check((length(Stmt.OrderByField) = 2) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName') and
        (Props.Fields.List[Stmt.OrderByField[1] - 1].name = 'LastName'));
  Check(Stmt.OrderByFieldDesc = []);
  NewStmt('select rowid,Data from tab order by firstname desc,lastname limit 10');
  CheckIdData(10, 0);
  Check((length(Stmt.OrderByField) = 2) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName') and
        (Props.Fields.List[Stmt.OrderByField[1] - 1].name = 'LastName'));
  Check(Stmt.OrderByFieldDesc = [0]);
  NewStmt('select rowid,Data from tab group by firstname order by firstname,lastname');
  CheckIdData(0, 0);
  Check((length(Stmt.GroupByField) = 1) and
        (Props.Fields.List[Stmt.GroupByField[0] - 1].name = 'FirstName'));
  Check((length(Stmt.OrderByField) = 2) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName') and
        (Props.Fields.List[Stmt.OrderByField[1] - 1].name = 'LastName'));
  NewStmt('select rowid,Data from tab group by firstname,lastname limit 10');
  CheckIdData(10, 0);
  Check((length(Stmt.GroupByField) = 2) and
        (Props.Fields.List[Stmt.GroupByField[0] - 1].name = 'FirstName') and
        (Props.Fields.List[Stmt.GroupByField[1] - 1].name = 'LastName'));
  Check(Stmt.OrderByFieldDesc = []);
  NewStmt('select iD,Data from tab limit   20');
  CheckIdData(20, 0);
  Check(Stmt.OrderByField = nil);
  Check(Stmt.OrderByFieldDesc = []);
  NewStmt('select iD,Data from tab  offset   20');
  CheckIdData(0, 20);
  Check(Stmt.OrderByField = nil);
  Check(Stmt.OrderByFieldDesc = []);
  NewStmt(
    'select data,iD from tab where id >= 10 limit 10 offset 20 order by firstname desc');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 1);
  Check(Stmt.Where[0].Field = 0);
  Check(Stmt.Where[0].Operation = opGreaterThanOrEqualTo);
  Check(Stmt.Where[0].ValueInteger = 10);
  Check(Stmt.Limit = 10);
  Check(Stmt.Offset = 20);
  Check((length(Stmt.Select) = 2) and
        (Stmt.Select[1].Field = 0) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'Data'));
  Check((length(Stmt.OrderByField) = 1) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName'));
  Check(Stmt.OrderByFieldDesc = [0]);
  NewStmt('select iD,Data from tab where id in (1, 2, 3)');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 1);
  Check(Stmt.Where[0].Field = 0);
  Check(Stmt.Where[0].Operation = opIn);
  Check(Stmt.Where[0].Value = '[1,2,3]');
  Check(Stmt.OrderByField = nil);
  NewStmt('select iD,Data from tab where firstname in ( ''a'' ,  ''b'', ''3''  ) order by id desc');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 1);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'FirstName');
  Check(Stmt.Where[0].Operation = opIn);
  Check(Stmt.Where[0].Value = '["a","b","3"]');
  Check((length(Stmt.OrderByField) = 1) and
        (Stmt.OrderByField[0] = 0));
  Check(Stmt.OrderByFieldDesc = [0]);
  NewStmt('select data,iD from tab where id >= 10 and YearOfBirth > 1600 limit 10 offset 20');
  CheckWhere(false);
  NewStmt('select data,iD from tab where rowid>=10 or YearOfBirth>1600 offset 20 limit 10');
  CheckWhere(true);
  NewStmt('select data,iD from tab where id <> 100 or data is not null limit 20 offset 10');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 2);
  Check(Stmt.Where[0].Field = 0);
  Check(Stmt.Where[0].Operation = opNotEqualTo);
  Check(Stmt.Where[0].ValueInteger = 100);
  Check(Stmt.Where[1].JoinedOR);
  Check(Props.Fields.List[Stmt.Where[1].Field - 1].name = 'Data');
  Check(Stmt.Where[1].Operation = opIsNotNull);
  Check(Stmt.Limit = 20);
  Check(Stmt.Offset = 10);
  Check((length(Stmt.Select) = 2) and
        (Stmt.Select[1].Field = 0) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'Data'));
  Check(Stmt.OrderByField = nil);
  NewStmt('select data,iD from tab where firstname like "monet" or data is null limit 20 offset 10');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 2);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'FirstName');
  Check(Stmt.Where[0].Operation = opLike);
  Check(Stmt.Where[0].Value = 'monet');
  Check(Stmt.Where[1].JoinedOR);
  Check(Props.Fields.List[Stmt.Where[1].Field - 1].name = 'Data');
  Check(Stmt.Where[1].Operation = opIsNull);
  Check(Stmt.Limit = 20);
  Check(Stmt.Offset = 10);
  Check((length(Stmt.Select) = 2) and
        (Stmt.Select[1].Field = 0) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'Data'));
  Check(Stmt.OrderByField = nil);
  NewStmt('select count(*) from tab');
  Check(Stmt.TableName = 'tab');
  Check(Stmt.Where = nil);
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].Field = 0));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'count'));
  Check(Stmt.Limit = 0);
  NewStmt('select count(*) from tab limit 10');
  Check(Stmt.TableName = 'tab');
  Check(Stmt.Where = nil);
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].Field = 0));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'count'));
  Check(Stmt.Limit = 10);
  NewStmt('select count(*) from tab where yearofbirth>1000 limit 10');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 1);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'YearOfBirth');
  Check(Stmt.Where[0].Operation = opGreaterThan);
  Check(Stmt.Where[0].ValueInteger = 1000);
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].Field = 0));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'count'));
  Check(Stmt.Limit = 10);
  NewStmt('select distinct ( yearofdeath )  from  tab where yearofbirth > :(1000): limit 20');
  Check(Stmt.TableName = 'tab');
  Check(length(Stmt.Where) = 1);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'YearOfBirth');
  Check(Stmt.Where[0].Operation = opGreaterThan);
  Check(Stmt.Where[0].ValueInteger = 1000);
  Check((length(Stmt.Select) = 1) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'YearOfDeath'));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'distinct'));
  Check(Stmt.Limit = 20);
  NewStmt(
    'select id from tab where id>:(1): and integerdynarraycontains ( yearofbirth , :(10): ) ' +
    'order by firstname desc limit 20');
  Check(Stmt.TableName = 'tab');
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].Field = 0) and
        (Stmt.Select[0].Alias = ''));
  Check(length(Stmt.Where) = 2);
  Check(Stmt.Where[0].Field = 0);
  Check(Stmt.Where[0].Operation = opGreaterThan);
  Check(Stmt.Where[0].ValueInteger = 1);
  Check(Props.Fields.List[Stmt.Where[1].Field - 1].name = 'YearOfBirth');
  Check(Stmt.Where[1].FunctionName = 'INTEGERDYNARRAYCONTAINS');
  Check(Stmt.Where[1].ValueInteger = 10);
  Check(Stmt.Where[1].Operation = opContains);
  Check((length(Stmt.OrderByField) = 1) and
        (Props.Fields.List[Stmt.OrderByField[0] - 1].name = 'FirstName'));
  Check(Stmt.OrderByFieldDesc = [0]);
  Check(Stmt.Limit = 20);
  NewStmt('select max(yearofdeath) as maxYOD from tab where yearofbirth > :(1000):');
  Check(Stmt.TableName = 'tab');
  Check((length(Stmt.Select) = 1) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'YearOfDeath') and
        (Stmt.Select[0].Alias = 'maxYOD') and
        (Stmt.Select[0].ToBeAdded = 0));
  Check(length(Stmt.Where) = 1);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'YearOfBirth');
  Check(Stmt.Where[0].Operation = opGreaterThan);
  Check(Stmt.Where[0].ValueInteger = 1000);
  Check((length(Stmt.Select) = 1) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'YearOfDeath'));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'max'));
  Check(Stmt.Limit = 0);
  NewStmt('select max(yearofdeath)+115 as maxYOD from tab where yearofbirth > :(1000):');
  Check(Stmt.TableName = 'tab');
  Check((length(Stmt.Select) = 1) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'YearOfDeath') and
        (Stmt.Select[0].Alias = 'maxYOD') and
        (Stmt.Select[0].ToBeAdded = 115));
  Check(length(Stmt.Where) = 1);
  Check(Props.Fields.List[Stmt.Where[0].Field - 1].name = 'YearOfBirth');
  Check(Stmt.Where[0].Operation = opGreaterThan);
  Check(Stmt.Where[0].ValueInteger = 1000);
  Check((length(Stmt.Select) = 1) and
        (Props.Fields.List[Stmt.Select[0].Field - 1].name = 'YearOfDeath'));
  Check((length(Stmt.Select) = 1) and
        (Stmt.Select[0].FunctionName = 'max'));
  Check(Stmt.Limit = 0);
  Stmt.Free;
end;

procedure TTestCoreProcess._TSynMonitorUsage;
var
  id: TSynMonitorUsageID;
  now, id2: TTimelog;
  n: TTimeLogBits absolute now;
  i: integer;
  s, s2: RawUtf8;
begin
  id.Value := 0;
  now := TimeLogNowUtc and not pred(1 shl 12); // truncate to hour resolution
  id.FromTimeLog(now);
  s := n.Text(true);
  id2 := id.ToTimeLog;
  s2 := id.Text(true);
  Check(id2 = now);
  Check(s2 = s);
  for i := 1 to 200 do
  begin
    n.From(n.ToDateTime + Random * 50);
    now := now and not pred(1 shl 12);
    s := n.Text(true);
    id.SetTime(mugYear, n.Year);
    id.SetTime(mugMonth, n.Month);
    id.SetTime(mugDay, n.Day);
    id.SetTime(mugHour, n.Hour);
    id2 := id.ToTimeLog;
    s2 := id.Text(true);
    Check(id2 = now);
    Check(s2 = s);
    Check(id.Granularity = mugHour);
    id.From(n.Year, n.Month, n.Day);
    Check(id.Granularity = mugDay);
    id.From(n.Year, n.Month);
    Check(id.Granularity = mugMonth);
    id.From(n.Year);
    Check(id.Granularity = mugYear);
  end;
end;


{ TTestCoreCompression }

procedure TTestCoreCompression.Setup;
begin
  Data := StringFromFile(Executable.ProgramFileName);
  if length(Data) > 1 shl 20 + 1 shl 10 then
    SetLength(Data, 1 shl 20 + 1 shl 10); // no need to compress more than 1.1MB
  DataFile := WorkDir + 'exe.1mb';
  FileFromString(Data, DataFile);
end;

procedure TTestCoreCompression.CleanUp;
begin
  FreeAndNil(M);
  DeleteFile(DataFile);
end;

const
  // regression tests use a const table instead of our computed array
  crc32tab: array[byte] of cardinal = ($00000000, $77073096, $EE0E612C,
    $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63,
    $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447,
    $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3,
    $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2,
    $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
    $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2,
    $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49,
    $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541,
    $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010,
    $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998,
    $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320,
    $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27,
    $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6,
    $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A,
    $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE,
    $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785,
    $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D,
    $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD,
    $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
    $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, $BDBDF21C,
    $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B,
    $2D02EF8D);

function ReferenceCrc32(aCRC32: cardinal; inBuf: pointer; inLen: integer): cardinal;
var
  i: integer;
begin
  // slowest reference version
  result := not aCRC32;
  for i := 1 to inLen do
  begin
    result := crc32tab[(result xor pByte(inBuf)^) and $ff] xor (result shr 8);
    inc(PByte(inBuf));
  end;
  result := not result;
end;

procedure TTestCoreCompression.GZipFormat;
var
  Z: TSynZipCompressor;
  L, n: integer;
  P: PAnsiChar;
  crc2: Cardinal;
  st: TRawByteStringStream;
  s, tmp: RawByteString;
  gzr: TGZRead;
begin
  Check(crc32(0, @crc32tab, 5) = $DF4EC16C, 'crc32');
  Check(ReferenceCrc32(0, @crc32tab, 5) = $DF4EC16C, 'crc32');
  Check(crc32(0, @crc32tab, 1024) = $6FCF9E13, 'crc32');
  Check(ReferenceCrc32(0, @crc32tab, 1024) = $6FCF9E13);
  Check(crc32(0, @crc32tab, 1024 - 5) = $70965738, 'crc32');
  Check(ReferenceCrc32(0, @crc32tab, 1024 - 5) = $70965738);
  Check(crc32(0, pointer(PtrInt(@crc32tab) + 1), 2) = $41D912FF, 'crc32');
  Check(ReferenceCrc32(0, pointer(PtrInt(@crc32tab) + 1), 2) = $41D912FF);
  Check(crc32(0, pointer(PtrInt(@crc32tab) + 3), 1024 - 5) = $E5FAEC6C, 'crc32');
  Check(ReferenceCrc32(0, pointer(PtrInt(@crc32tab) + 3), 1024 - 5) = $E5FAEC6C, 'crc32');
  M := TMemoryStream.Create;
  Z := TSynZipCompressor.Create(M, 6, szcfGZ);
  L := length(Data);
  P := Pointer(Data);
  crc0 := 0;
  crc2 := 0;
  while L <> 0 do
  begin
    if L > 1000 then
      n := 1000
    else
      n := L;
    Z.Write(P^, n); // compress by little chunks to test streaming
    crc0 := crc32(crc0, P, n);
    crc2 := ReferenceCrc32(crc2, P, n);
    inc(P, n);
    dec(L, n);
  end;
  Check(crc0 = ReferenceCrc32(0, Pointer(Data), length(Data)));
  Check(crc0 = Z.CRC, 'crc32');
  Check(crc2 = crc0, 'crc32');
  Z.Free;
  Check(GZRead(M.Memory, M.Position) = Data, 'gzread');
  crc1 := crc32(0, M.Memory, M.Position);
  s := Data;
  Check(CompressGZip(s, true) = 'gzip');
  Check(CompressGZip(s, false) = 'gzip');
  Check(s = Data, 'compressGZip');
  Check(gzr.Init(M.Memory, M.Position), 'TGZRead');
  Check({%H-}gzr.uncomplen32 = Cardinal(length(Data)));
  Check(gzr.crc32 = crc0);
  Check(gzr.ToMem = Data, 'ToMem');
  st := TRawByteStringStream.Create;
  try
    Check(gzr.ToStream(st), 'ToStream');
    s := st.DataString;
    Check(s = Data, 'ToStream?');
  finally
    st.Free;
  end;
  SetLength(tmp, gzr.uncomplen32 div 5);
  Check(gzr.ZStreamStart(pointer(tmp), length(tmp)), 'ZStreamStart');
  s := '';
  repeat
    n := gzr.ZStreamNext;
    if n = 0 then
      break;
    s := s + copy(tmp, 1, n);
  until false;
  check(gzr.ZStreamDone, 'ZStreamDone');
  Check(gzr.uncomplen32 = Cardinal(length(s)));
  check(s = Data);
  s := Data;
  Check(CompressDeflate(s, true) = 'deflate');
  Check(CompressDeflate(s, false) = 'deflate');
  Check(s = Data, 'CompressDeflate');
end;

procedure TTestCoreCompression.InMemoryCompression;
var
  tmp: RawByteString;
  hash: cardinal;
  len, i: Integer;
begin
  Check(CRC32string('TestCRC32') = $2CB8CDF3);
  for i := 1 to 10 do
  begin
    tmp := RandomTextParagraph(1000 * i * i * i);
    len := length(tmp);
    hash := Hash32(tmp);
    CompressZLib(tmp, true);
    Check(len div length(tmp) > 2, 'blobloblu should be compressible');
    CompressZLib(tmp, false);
    CheckHash(tmp, hash, 'CompressZLib');
  end;
end;

procedure TTestCoreCompression.ZipFormat;
var
  FN, FN2: TFileName;
  S: TRawByteStringStream;
  zip64: boolean;

  procedure test(Z: TZipRead; aCount: integer);
  var
    i: integer;
    tmp: RawByteString;
    tmpFN: TFileName;
    local: TLocalFileHeader;
    info: TFileInfoFull;
  begin
    Check(Z.TestAll, 'testall');
    with Z do
    try
      if CheckFailed(Count = aCount, 'count') then
        exit;
      for i := 0 to Count - 1 do
        if not CheckFailed(RetrieveLocalFileHeader(i, local)) then
          Check(CompareMem(@Entry[i].dir^.fileInfo, @local.fileInfo,
            SizeOf(TFileInfo) - SizeOf(Entry[i].dir^.fileInfo.extraLen)));
      i := NameToIndex('REP1\ONE.exe');
      Check(i = 0, '0');
      FillcharFast(info, sizeof(info), 0);
      Check(RetrieveFileInfo(i, info), 'info');
      Check(integer(info.f64.zfullSize) = length(Data), 'siz');
      Check(info.f32.zcrc32 = crc0, 'crc0');
      Check(UnZip(i) = Data, 'unzip1');
      i := NameToIndex('REp2\ident.gz');
      Check(i = 1, 'unzip2');
      Check(RetrieveLocalFileHeader(i, local));
      Check(local.fileInfo.zcrc32 = crc1, 'crc1a');
      tmp := UnZip(i);
      Check(tmp <> '', 'unzip3');
      Check(crc32(0, pointer(tmp), length(tmp)) = crc1, 'crc1b');
      i := NameToIndex(ExtractFileName(DataFile));
      Check(i = 2, 'unzip4');
      Check(UnZip(i) = Data, 'unzip6');
      Check(RetrieveLocalFileHeader(i, local));
      Check(local.fileInfo.zcrc32 = info.f32.zcrc32, 'crc32');
      i := NameToIndex('REp2\ident2.gz');
      Check(i = 3, 'unzip5');
      Check(RetrieveLocalFileHeader(i, local));
      Check(local.fileInfo.zcrc32 = crc1, 'crc1c');
      tmp := UnZip(i);
      Check(tmp <> '', 'unzip7');
      Check(crc32(0, pointer(tmp), length(tmp)) = crc1, 'crc1d');
      if aCount = 4 then
        Exit;
      i := NameToIndex('REP1\twO.exe');
      Check(i = 4, 'unzip8');
      Check(UnZip(i) = Data, 'unzip6');
      tmpFN := WorkDir + 'mormot2zipformat.tmp';
      Check(UnZip('REP1\one.exe', tmpFN, true), 'unzipa');
      Check(StringFromFile(tmpFN) = Data, 'unzipb');
      Check(DeleteFile(tmpFN), 'unzipc');
    finally
      Free;
    end;
  end;

  procedure Prepare(Z: TZipWrite);
  begin
    try
      Z.ForceZip64 := zip64;
      Z.AddDeflated('rep1\one.exe', pointer(Data), length(Data));
      Check(Z.Count = 1, 'cnt1');
      Z.AddDeflated('rep2\ident.gz', M.Memory, M.Position);
      Check(Z.Count = 2, 'cnt2');
      Z.AddDeflated(DataFile);
      Check(Z.Count = 3, 'cnt3');
      Z.AddStored('rep2\ident2.gz', M.Memory, M.Position);
      Check(Z.Count = 4, 'cnt4');
      Check(Z.NeedZip64 = zip64);
    finally
      Z.Free;
    end;
  end;

var
  i, m: integer;
  mem: QWord;
  json, deleted: TStringDynArray;
begin
  for m := 1 to 2 do
  for zip64 := false to true do
  begin
    mem := 1 shl 20 * m; // test with and without TZipRead.fSource
    FN := WorkDir + 'write';
    if zip64 then
      FN := FN + '64.zip'
    else
      FN := FN + '.zip';
    Prepare(TZipWrite.Create(FN));
    test(TZipRead.Create(FN, 0, 0, mem), 4);
    S := TRawByteStringStream.Create;
    try
      Prepare(TZipWrite.Create(S));
      //FileFromString(S.DataString, FN + 'mem');
      test(TZipRead.Create(pointer(S.DataString), length(S.DataString)), 4);
    finally
      S.Free;
    end;
    with TZipWrite.CreateFrom(FN, mem) do
    try
      Check(Count = 4, 'two4');
      AddDeflated('rep1\two.exe', pointer(Data), length(Data));
      Check(Count = 5, 'two5');
    finally
      Free;
    end;
    test(TZipRead.Create(FN, 0, 0, mem), 5);
    with TZipWrite.CreateFrom(FN, ['rep1\two.exe']) do
    try
      Check(Count = 4, 'last4');
      AddDeflated('rep1\two.exe', pointer(Data), length(Data));
      Check(Count = 5, 'last5');
    finally
      Free;
    end;
    test(TZipRead.Create(FN), 5);
    FN2 := ChangeFileExt(FN, 'appended.exe');
    if m = 1 then
    begin
      FileFromString(Data, FN2);
      FileAppend(FN2, FN);
    end
    else
    begin
      FileAppend(DataFile, FN, FN2);
      mem := mem * 2;
    end;
    test(TZipRead.Create(FN2, 0, 0, mem), 5);
    DeleteFile(FN2);
    FN2 := WorkDir + 'json';
    if zip64 then
      FN2 := FN2 + '64.zip'
    else
      FN2 := FN2 + '.zip';
    json := nil;
    deleted := nil;
    with TZipWrite.Create(FN2) do
    try
      AddFolder(WorkDir, '*.json', true, 1);
      CheckUtf8(Count > 2, 'json=%', [Count]);
      for i := 0 to Count - 1 do
      begin
        Check(AddString(json, Ansi7ToString(Entry[i].intName)) = i);
        if (i and 1) = (m - 1) then
          AddString(deleted, json[i]);
        Check(SameText(ExtractFileExt(json[i]), '.json'), 'json');
      end;
    finally
      Free;
    end;
    with TZipWrite.CreateFrom(FN2, TFileNameDynArray(deleted)) do
    try
      Check(Count = length(json) - length(deleted));
    finally
      Free;
    end;
    Check(ZipTest(FN2), 'zipjson1');
    for i := 0 to high(json) do
      json[i] := WorkDir + json[i];
    ZipAppendFiles(DataFile, FN2, TFileNameDynArray(json), false, 1);
    Check(ZipTest(FN2), 'zipjson2');
    DeleteFile(FN);
    DeleteFile(FN2);
  end;
end;

function Spaces(n: integer): RawUtf8;
begin
  SetString(result, nil, n);
  FillCharFast(pointer(result)^, n, 32);
end;

function By4(pattern, n: integer): RawUtf8;
var
  i: PtrInt;
begin
  SetString(result, nil, n * 4);
  for i := 0 to n - 1 do
    PIntegerArray(result)[i] := pattern;
end;

procedure TTestCoreCompression._SynLZ;
var
  s, t, rle: RawByteString;
  i, j, complen2: integer;
  comp2, dec1: array of byte;
  {$ifdef CPUINTEL}
  comp1, dec2: array of byte;
  complen1: integer;
  {$endif CPUINTEL}
begin
  for i := 1 to 200 do
  begin
    s := AlgoSynLZ.Compress(StringOfChar(AnsiChar(i), i));
    t := AlgoSynLZ.Decompress(s);
    Check(t = StringOfChar(AnsiChar(i), i));
  end;
  rle := 'hello' + Spaces(10000) + 'hello' + Spaces(1000) + 'world';
  s := AlgoSynLZ.Compress(rle);
  t := AlgoSynLZ.Decompress(s);
  Check(t = rle);
  rle := 'hello' + by4($3031333, 10000) + 'hello' + by4($3031333, 1000) + 'world';
  s := AlgoSynLZ.Compress(rle);
  t := AlgoSynLZ.Decompress(s);
  Check(t = rle);
  for i := 0 to 1000 do
  begin
    s := StringOfChar(AnsiChar(' '), 20);
    t := RandomTextParagraph(i, '.', s);
    SetString(s, PAnsiChar(pointer(t)), length(t)); // =UniqueString
    Check(CompressSynLZ(s, true) = 'synlz');
    Check(CompressSynLZ(s, false) = 'synlz');
    Check(s = t);
    Check(AlgoSynLZ.Decompress(AlgoSynLZ.Compress(s)) = t);
    SetLength(comp2, AlgoSynLZ.Compressdestlen(length(s)));
    complen2 := SynLZCompress1pas(Pointer(s), length(s), pointer(comp2));
    Check(complen2 < length(comp2));
    {$ifdef CPUINTEL}
    // validate the i386/i86_64 asm versions against their pascal reference
    SetLength(comp1, AlgoSynLZ.Compressdestlen(length(s)));
    complen1 := SynLZCompress1(Pointer(s), length(s), pointer(comp1));
    Check(complen1 < length(comp1));
    Check(complen1 = complen2);
    Check(CompareMem(pointer(comp1), pointer(comp2), complen1));
    Check(SynLZDecompressdestlen(pointer(comp1)) = length(s));
    Check(SynLZDecompressdestlen(pointer(comp2)) = length(s));
    SetLength(dec1, Length(s));
    Check(SynLZDecompress1pas(Pointer(comp1), complen1, pointer(dec1)) = length(s));
    Check(CompareMem(pointer(dec1), pointer(s), length(s)));
    SetLength(dec2, Length(s));
    Check(SynLZDecompress1(Pointer(comp2), complen2, pointer(dec2)) = length(s));
    Check(CompareMem(pointer(dec1), pointer(s), length(s)));
    {$endif CPUINTEL}
  end;
  SetLength(dec1, length(t));
  for j := 0 to length(t) - 1 do
  begin
    FillCharFast(pointer(dec1)^, length(t), 0);
    Check(SynLZDecompress1partial(pointer(comp2), complen2, Pointer(dec1), j) = j);
    Check(CompareMem(pointer(dec1), pointer(t), j));
  end;
  s := Data;
  Check(CompressSynLZ(s, true) = 'synlz');
  Check(Length(s) < Length(Data), 'exelen');
  Check(CompressSynLZ(s, false) = 'synlz');
  Check(s = Data);
end;

procedure TTestCoreCompression._TAlgoCompress;

  procedure TestAlgo(algo: TAlgoCompress);
  var
    log, s, t, s2: RawByteString;
    i, plain, comp: integer;
    timer: TPrecisionTimer;
    timecomp, timedecomp: Int64;
  begin
    if algo = nil then
      exit;
    for i := 1 to 50 do
    begin
      t := StringOfChar(AnsiChar(i), i){%H-}+{%H-}t;
      s := StringOfChar(AnsiChar(i), i){%H-}+{%H-}s;
      Check(algo.Decompress(algo.Compress(s)) = t);
    end;
    plain := 0;
    comp := 0;
    timecomp := 0;
    timedecomp := 0;
    log := StringFromFile(WorkDir + 'bigTest.log');
    for i := 0 to 100 do
    begin
      if log = '' then
        s := copy(Data, 1, i * 800) // first 80KB from executable
      else
        s := log;
      timer.Start;
      t := algo.Compress(s);
      inc(timecomp, timer.StopInMicroSec);
      timer.Start;
      s2 := algo.Decompress(t, aclNoCrcFast);
      inc(timedecomp, timer.StopInMicroSec);
      Check(s2 = s, algo.ClassName);
      if (log <> '') and
         (s2 <> s) then
        FileFromString(s2, WorkDir + 'bigTest' + algo.ClassName + '.log');
      inc(plain, length(s));
      inc(comp, length(t));
      if log <> '' then
        break;
    end;
    AddConsole(format('%s %s->%s: comp %d:%dMB/s decomp %d:%dMB/s',
      [algo.ClassName, KB(plain), KB(comp),
       ((plain * Int64(1000 * 1000)) div timecomp) shr 20,
       ((comp * Int64(1000 * 1000)) div timecomp) shr 20,
       ((comp * Int64(1000 * 1000)) div timedecomp) shr 20,
       ((plain * Int64(1000 * 1000)) div timedecomp) shr 20]));
    s2 := algo.Decompress(algo.Compress(s), aclNoCrcFast);
    Check(s2 = s, algo.ClassName);
  end;

begin
  TestAlgo(AlgoSynLZ);
  Check(AlgoSynLZ.AlgoName = 'synlz');
  {$ifdef OSWINDOWS}
  if (Lizard = nil) and
     FileExists(Executable.ProgramFilePath + LIZARD_LIB_NAME) then
    Lizard := TSynLizardDynamic.Create;
  {$endif OSWINDOWS}
  TestAlgo(AlgoLizard);
  TestAlgo(AlgoLizardFast);
  TestAlgo(AlgoLizardHuffman);
  TestAlgo(AlgoDeflate);
  TestAlgo(AlgoDeflateFast);
  Check(AlgoDeflateFast.AlgoName = 'deflatefast');
end;

{ FPC Linux x86-64 (in VM) with static linked library for a 53MB log file:
     TAlgoSynLz 53 MB->5 MB: comp 650:62MB/s decomp 90:945MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 55:4MB/s decomp 139:1881MB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 695:89MB/s decomp 196:1522MB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 71:6MB/s decomp 48:540MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 142:18MB/s decomp 56:428MB/s
  Delphi Win64 with external lizard1-64.dll:
     TAlgoSynLz 53 MB->5 MB: comp 667:63MB/s decomp 103:1087MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 61:4MB/s decomp 169:2290MB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 690:89MB/s decomp 263:2039MB/s
     TAlgoLizardHuffman 53 MB->2 MB: comp 658:25MB/s decomp 86:2200MB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 25:2MB/s decomp 19:214MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 52:6MB/s decomp 23:176MB/s
  speed difference may come from the FPC/Delphi heap manager, and/or the Linux VM

  From realistic tests, SynLZ may focus on small buffers, or very compressible
  log files like above. But Lizard/LizardFast seem a better candidate for
  fast decompression of any kind of data, especially large JSON/binary buffers.
}


end.

