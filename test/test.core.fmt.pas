/// regression tests for advanced formats supported by mormot.core units
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.fmt;

interface

{$I ..\src\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.json,
  mormot.core.fmt,
  mormot.core.test,
  mormot.tools.jop;

type
  /// exercise the processing API without invoking a command line executable
  TTestCoreJop = class(TSynTestCase)
  protected
    fFolder: TFileName;
    fFiles: TFileNameDynArray;
    fFolders: TFileNameDynArray;
    procedure Setup; override;
    procedure CleanUp; override;
    function FileName(const Name: TFileName): TFileName;
    function Folder(const Name: TFileName; CreateIt: boolean = true): TFileName;
    procedure WriteFile(const Name: TFileName; const Content: RawUtf8);
    procedure Expect(const Source, Expected: RawUtf8; const Options: TJopOptions);
    procedure Reject(const Source: RawUtf8; const Options: TJopOptions;
      Expected: TJopError);
  published
    /// aliases, extension inference, initialized settings and throughput units
    procedure Formats;
    /// native JSON formats, scalar roots, comments and malformed documents
    procedure Json;
    /// typed literal defines, escaping, overriding and conditional expansion
    procedure Definitions;
    /// plain text and nested fallback expansion, without marker leakage
    procedure Text;
    /// YAML conversion and explicit source preprocessing
    procedure Yaml;
    /// CSV input and native relational CSV output
    procedure Csv;
    /// all native tables, shared options, remarks, Markdown and escaping
    procedure Tables;
    /// caller-owned writable buffers and native in-place decoding
    procedure Buffers;
    /// byte-preserving input and explicit UTF-8 checks at all input boundaries
    procedure Validation;
    /// include policy, insert behavior, empty failures, depth and counters
    procedure Includes;
    /// caller ownership, appending, large output and accumulating counters
    procedure Writers;
    /// BOM-aware loading, replacement, failure preservation and dry runs
    procedure Files;
    /// recursive planning, preserved paths, dry-run and collision detection
    procedure Batch;
    /// deterministic local generation, depth limits and writer ownership
    procedure Generate;
    /// native normalized comparison, property order and I/O errors
    procedure CompareFiles;
  end;


implementation


function TestJopBytes(const Hex: RawUtf8): RawUtf8;
var
  bin: RawByteString;
begin
  // build exact byte sequences on both Unicode and pre-Unicode compilers
  bin := mormot.core.text.HexToBin(Hex);
  FastSetString(result, pointer(bin), length(bin));
end;


// Literal fixtures are immutable. Copy them once at the test boundary; the
// public processing functions consume the resulting writable buffer.
function TestJopProcess(const Source: RawUtf8; const Options: TJopOptions;
  out Dest: RawUtf8; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError; overload;
var
  buffer: RawUtf8;
begin
  FastSetString(buffer, pointer(Source), length(Source));
  result := mormot.tools.jop.JopProcess(buffer, Options, Dest, Statistics, Info);
end;

function TestJopProcess(const Source: RawUtf8;
  InputFormat: TJopFormat = jfJson; OutputFormat: TJopFormat = jfJson;
  Flags: TJopFlags = []): RawUtf8; overload;
var
  buffer: RawUtf8;
begin
  FastSetString(buffer, pointer(Source), length(Source));
  result := mormot.tools.jop.JopProcess(buffer, InputFormat, OutputFormat, Flags);
end;

function TestJopProcessToWriter(const Source: RawUtf8; W: TJsonWriter;
  const Options: TJopOptions; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;
var
  buffer: RawUtf8;
begin
  FastSetString(buffer, pointer(Source), length(Source));
  result := mormot.tools.jop.JopProcessToWriter(buffer, W, Options, Statistics, Info);
end;

function TestJopProcessToStream(const Source: RawUtf8; Dest: TStream;
  const Options: TJopOptions; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;
var
  buffer: RawUtf8;
begin
  FastSetString(buffer, pointer(Source), length(Source));
  result := mormot.tools.jop.JopProcessToStream(buffer, Dest, Options, Statistics, Info);
end;

function TestJopProcessToCsv(const Source: RawUtf8; const Options: TJopOptions;
  out Tables: TTextPairs; Statistics: PJopStatistics = nil;
  Info: PRawUtf8 = nil): TJopError;
var
  buffer: RawUtf8;
begin
  FastSetString(buffer, pointer(Source), length(Source));
  result := mormot.tools.jop.JopProcessToCsv(buffer, Options, Tables, Statistics, Info);
end;


{ TTestCoreJop }

procedure TTestCoreJop.Setup;
begin
  inherited Setup;
  fFolder := EnsureDirectoryExists(TemporaryFileName(WorkDir, 'jop-tests'));
  Check(fFolder <> '');
  Check(DirectoryExists(fFolder));
end;

procedure TTestCoreJop.CleanUp;
var
  i: PtrInt;
begin
  // remove only the exact paths created or planned by this test case
  for i := high(fFiles) downto 0 do
    if FileExists(fFiles[i]) then
      DeleteFile(fFiles[i]);
  fFiles := nil;
  for i := high(fFolders) downto 0 do
    if DirectoryExists(fFolders[i]) then
      Check(RemoveDir(fFolders[i]), 'no temporary file remains');
  fFolders := nil;
  if fFolder <> '' then
  begin
    Check(RemoveDir(ExcludeTrailingPathDelimiter(fFolder)), 'test folder is empty');
    fFolder := '';
  end;
  inherited CleanUp;
end;

function TTestCoreJop.FileName(const Name: TFileName): TFileName;
var
  n: PtrInt;
begin
  result := fFolder + Name;
  n := length(fFiles);
  SetLength(fFiles, n + 1);
  fFiles[n] := result;
end;

function TTestCoreJop.Folder(const Name: TFileName; CreateIt: boolean): TFileName;
var
  n: PtrInt;
begin
  result := fFolder + Name;
  if CreateIt and
     not DirectoryExists(result) then
    Check(CreateDir(result));
  n := length(fFolders);
  SetLength(fFolders, n + 1);
  fFolders[n] := result;
  result := IncludeTrailingPathDelimiter(result);
end;

procedure TTestCoreJop.WriteFile(const Name: TFileName; const Content: RawUtf8);
begin
  Check(FileFromString(Content, FileName(Name)));
end;

procedure TTestCoreJop.Expect(const Source, Expected: RawUtf8;
  const Options: TJopOptions);
var
  output, info: RawUtf8;
  error: TJopError;
begin
  info := 'old info';
  error := TestJopProcess(Source, Options, output, nil, @info);
  CheckUtf8(error = jeSuccess, '%: %', [JOP_ERROR[error], info]);
  CheckEqual(output, Expected);
end;

procedure TTestCoreJop.Reject(const Source: RawUtf8; const Options: TJopOptions;
  Expected: TJopError);
var
  output: RawUtf8;
  error: TJopError;
begin
  output := 'old output';
  error := TestJopProcess(Source, Options, output);
  CheckEqual(ord(error), ord(Expected), JOP_ERROR[error]);
  CheckEqual(output, '', 'failure clears the string result');
end;

procedure TTestCoreJop.Formats;
var
  f, resolved: TJopFormat;
  o: TJopOptions;
  s: TJopStatistics;
begin
  o := JopOptions;
  Check(o.InputFormat = jfAuto);
  Check(o.OutputFormat = jfAuto);
  Check(o.Flags = []);
  CheckEqual(length(o.Defines), 0);
  Check(o.IncludeRoot = '', o.IncludeRoot);
  Check(o.TableOptions = TABLE_DEFAULT);
  CheckEqual(o.TableRoot, '');
  for f := low(f) to high(f) do
  begin
    Check(JopFormatFromName(JOP_FORMAT_NAMES[f], resolved));
    Check(resolved = f);
  end;
  Check(JopFormatFromName('JsOn5', resolved));
  Check(resolved = jfJson5);
  Check(JopFormatFromName('yml', resolved));
  Check(resolved = jfYaml);
  Check(JopFormatFromName('jop', resolved));
  Check(resolved = jfText);
  Check(JopFormatFromName('txt', resolved));
  Check(resolved = jfText);
  Check(JopFormatFromName('markdown', resolved));
  Check(resolved = jfMd);
  Check(JopFormatFromName('MD', resolved));
  Check(resolved = jfMd);
  Check(JopFormatFromName('md-csv', resolved));
  Check(resolved = jfMd);
  Check(JopFormatFromName('md-pipe', resolved));
  Check(resolved = jfMdPipe);
  Check(JopFormatFromFileName('table.MD', resolved));
  Check(resolved = jfMd);
  Reject('## root', JopOptions(jfMd, jfJson), jeUnsupportedInput);
  CheckNot(JopFormatFromName('not-a-format', resolved));
  Check(JopFormatFromFileName('nested/config.YML', resolved));
  Check(resolved = jfYaml);
  Check(JopFormatFromFileName('settings.ini', resolved));
  Check(resolved = jfText);
  Check(JopFormatFromFileName('settings.cnf', resolved));
  Check(resolved = jfText);
  Check(JopFormatFromFileName('settings.jop', resolved));
  Check(resolved = jfText);
  CheckNot(JopFormatFromFileName('settings.unknown', resolved));
  FillCharFast(s, SizeOf(s), 0);
  CheckSame(JopThroughput(s), 0);
  s.InputBytes := 1000000;
  s.IncludeBytes := 500000;
  s.ElapsedMicroSeconds := 500000;
  CheckSame(JopThroughput(s), 3, 1e-9, 'decimal MB per second');
end;

procedure TTestCoreJop.Json;

  procedure NativeImplicit(const Source, Expected: RawUtf8);
  var
    path: integer;
    w: TJsonWriter;
    temp: TTextWriterStackBuffer;
    settings: TPreprocSettings;
    actual: RawUtf8;
    ok: boolean;
  begin
    if Expected <> '' then
      CheckEqual(JsonReformat(Source, json5), Expected, 'legacy implicit object');
    settings.Flags := [jppJsonImplicitObject];
    settings.IncludeRoot := '';
    settings.Defines := nil;
    settings.Statistics := nil;
    for path := 0 to 1 do
    begin
      w := TJsonWriter.CreateOwnedStream(temp);
      try
        if path = 0 then
          ok := w.AddJsonReformat(pointer(Source), json5)
        else
          ok := JsonPreprocessToWriter(pointer(Source), w, json5, settings);
        Check(ok = (Expected <> ''), 'implicit object nesting');
        if ok then
        begin
          w.SetText(actual);
          CheckEqual(actual, Expected, 'implicit object member separators');
        end;
      finally
        w.Free;
      end;
    end;
  end;

const
  FLAT = '[{"name":"Alice","code":"007"},{"name":"Bob","code":""}]';
  NUMERIC_STRINGS = '["007","0x10","0o10","+12"]';
  ROOT_INPUT: array[0..5] of RawUtf8 = (
    'true // tail'#10, '{"a":1} // tail'#10,
    '[1,2] // tail'#10, '42 /* tail */',
    '42 // trailing', '42 // trailing'#10);
  ROOT_OUTPUT: array[0..5] of RawUtf8 = (
    'true', '{"a":1}', '[1,2]', '42', '42', '42');
  ROOT_COMMA: array[0..5] of RawUtf8 = (
    'true,', '},', '],', '42,', '42,', '42,');
var
  f: TJopFormat;
  i: PtrInt;
  o: TJopOptions;
  output, error, restored: RawUtf8;
  ok: boolean;
begin
  NativeImplicit('a:1'#10'b:2'#10,
    '{'#10'  a: 1,'#10'  b: 2,'#10'}');
  NativeImplicit('a:{}'#10'b:[]'#10,
    '{'#10'  a: {'#10'  },'#10'  b: ['#10'  ],'#10'}');
  NativeImplicit('a:1}', '');
  NativeImplicit('a:[1', '');
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  Expect('{a:1, b:''two'', c:[true,null]}',
    '{"a":1,"b":"two","c":[true,null]}', o);
  Expect('42', '42', o);
  Expect('true', 'true', o);
  Expect('null', 'null', o);
  Expect('"hello"', '"hello"', o);
  Expect('[]', '[]', o);
  Expect('{}', '{}', o);
  Expect(' // before'#10'{a:1} // after'#10, '{"a":1}', o);
  Reject('', o, jeInvalidJson);
  Reject('{a:"unterminated}', o, jeInvalidJson);
  Reject('{a:[1,2}', o, jeInvalidJson);
  Reject('{a:1', o, jeInvalidJson);
  Reject('[1,2', o, jeInvalidJson);
  // The default path follows the relaxed native reformatter, including
  // duplicate names. Structural validation is a separate, explicit option.
  Expect('{a:1,a:2}', '{"a":1,"a":2}', o);
  Check(TestJopProcess('true false', o, output) = jeSuccess);
  CheckNot(IsValidJson(output, true), 'no implicit validation of native output');
  include(o.Flags, jofValidateJson);
  Reject('{}{}', o, jeInvalidJson);
  Reject('true false', o, jeInvalidJson);
  exclude(o.Flags, jofValidateJson);
  for f := jfJson to jfYaml do
  begin
    o := JopOptions(jfJson, f, [jofCompact]);
    ok := (TestJopProcess(FLAT, o, output, nil, @error) = jeSuccess);
    CheckUtf8(ok, error);
    o := JopOptions(f, jfJson, [jofCompact]);
    ok := (TestJopProcess(output, o, restored, nil, @error) = jeSuccess);
    CheckUtf8(ok, error);
    CheckEqual(restored, FLAT, RawUtf8(JOP_FORMAT_NAMES[f]));
  end;
  o := JopOptions(jfJson, jfYaml);
  ok := (TestJopProcess(NUMERIC_STRINGS, o, output, nil, @error) = jeSuccess);
  CheckUtf8(ok, error);
  o := JopOptions(jfYaml, jfJson, [jofCompact]);
  ok := (TestJopProcess(output, o, restored, nil, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(restored, NUMERIC_STRINGS, 'numeric strings retain their types');
  o := JopOptions(jfHjson, jfJson, [jofCompact]);
  Expect('answer: 42'#10'name: hello'#10, '{"answer":42,"name":"hello"}', o);
  o.InputFormat := jfMorml;
  Expect('answer = 42'#10'name = hello'#10, '{"answer":42,"name":"hello"}', o);
  o := JopOptions(jfJsonC, jfJson, [jofCompact]);
  Expect('42 // trailing', '42', o);
  Expect('42 // trailing'#10, '42', o);
  for f := jfJson5 to jfHjson do
    for i := low(ROOT_INPUT) to high(ROOT_INPUT) do
    begin
      o := JopOptions(jfJsonC, f);
      ok := (TestJopProcess(ROOT_INPUT[i], o, output, nil, @error) = jeSuccess);
      CheckUtf8(ok, error);
      CheckEqual(Pos(ROOT_COMMA[i], output), 0, 'no root comma before a comment');
      o := JopOptions(f, jfJson, [jofCompact]);
      ok := (TestJopProcess(output, o, restored, nil, @error) = jeSuccess);
      CheckUtf8(ok, error);
      CheckEqual(restored, ROOT_OUTPUT[i], 'root value with trailing comment');
    end;
  o := JopOptions(jfJsonC, jfJsonC);
  Check((TestJopProcess('{ // retained'#10'a:1}', o, output, nil, @error) = jeSuccess));
  Check(PosEx('retained', output) > 0);
  include(o.Flags, jofValidateJson);
  Check(TestJopProcess('{ // retained'#10'a:1}', o, output) = jeSuccess);
  Check(PosEx('retained', output) > 0, 'validation preserves requested comments');
  include(o.Flags, jofNoComments);
  Check((TestJopProcess('{ // removed'#10'a:1}', o, output, nil, @error) = jeSuccess));
  CheckEqual(PosEx('removed', output), 0);
  o := JopOptions(jfJson, jfJson, [jofDebugComments]);
  Reject('{a:1}', o, jeInvalidOptions);
  o := JopOptions(jfJson, jfJsonC, [jofNoComments, jofDebugComments]);
  Reject('{a:1}', o, jeInvalidOptions);
  CheckEqual(TestJopProcess('{a:1}', jfJson, jfJson, [jofCompact]), '{"a":1}');
  error := 'stale';
  Check(TestJopProcess('{a:1}', JopOptions(jfJson, jfJson, [jofCompact]),
    output, nil, @error) = jeSuccess);
  CheckEqual(error, '');
end;

procedure TTestCoreJop.Definitions;
var
  o: TJopOptions;
  output, error: RawUtf8;
begin
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  SetLength(o.Defines, 6);
  o.Defines[0] := 'name=first';
  o.Defines[1] := 'name=last';
  o.Defines[2] := 'answer=42';
  o.Defines[3] := 'flag=true';
  o.Defines[4] := 'nothing=null';
  o.Defines[5] := 'path=C:\temp\"quoted"';
  Expect('{name:$(name),answer:$(answer),flag:$(flag),nothing:$(nothing)}',
    '{"name":"last","answer":42,"flag":true,"nothing":null}', o);
  Expect('{path:$(path)}', '{"path":"C:\\temp\\\"quoted\""}', o);
  Expect('$$'#10'name=source'#10'$$'#10'{name:$(name)}',
    '{"name":"source"}', o);
  Expect('{plain:"$(name)",expanded:$"hi $(name)"}',
    '{"plain":"$(name)","expanded":"hi last"}', o);
  Expect('[$if flag$ 1 $else$ 2 $endif$]', '[1]', o);
  SetLength(o.Defines, 1);
  o.Defines[0] := 'payload=line1'#10'$$'#10'insert impossible'#10'$$';
  Expect('{v:$(payload)}', '{"v":"line1\n$$\ninsert impossible\n$$"}', o);
  o.Defines[0] := 'empty=';
  Expect('{v:$(empty|default)}', '{"v":""}', o);
  o.Defines[0] := 'bad name=value';
  Reject('{}', o, jePreprocess);
  o.Defines[0] := 'missing-equals';
  Reject('{}', o, jePreprocess);
  o.Defines[0] := '=missing-name';
  Reject('{}', o, jePreprocess);
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  Check((TestJopProcess('{v:$(os:name|none)}', o, output, nil, @error) = jeSuccess));
  Check(output <> '{"v":"none"}');
  include(o.Flags, jofNoNamespaces);
  Expect('{v:$(os:name|none)}', '{"v":"none"}', o);
  include(o.Flags, jofNoExpand);
  Expect('{v:"$(name)"}', '{"v":"$(name)"}', o);
end;

procedure TTestCoreJop.Text;
var
  o: TJopOptions;
begin
  o := JopOptions(jfText, jfText);
  Expect('', '', o);
  Expect('price $25; $name$; $(unknown|fallback)',
    'price $25; $name$; fallback', o);
  Expect('$$'#10'name=world'#10'$$'#10'Hello $(name)!', 'Hello world!', o);
  SetLength(o.Defines, 2);
  o.Defines[0] := 'name=primary';
  o.Defines[1] := 'other=secondary';
  Expect('$(name|$(other))', 'primary', o);
  Expect('$(missing|$(other))', 'secondary', o);
  Expect('$(missing|$(also|fallback))', 'fallback', o);
  Expect('$$$'#10'raw $(name)'#10'$$$'#10'$(name)',
    'raw $(name)'#10'primary', o);
  include(o.Flags, jofNoExpand);
  Expect('$$'#10'include missing.txt'#10'$$'#10'$(name)',
    '$$'#10'include missing.txt'#10'$$'#10'$(name)', o);
  o.OutputFormat := jfAuto;
  Expect('unchanged', 'unchanged', o);
end;

procedure TTestCoreJop.Yaml;
var
  o: TJopOptions;
  output, error, restored: RawUtf8;
  ok: boolean;
begin
  o := JopOptions(jfYaml, jfJson, [jofCompact]);
  Expect('name: Alice'#10'count: 2'#10'enabled: true'#10,
    '{"name":"Alice","count":2,"enabled":true}', o);
  SetLength(o.Defines, 1);
  o.Defines[0] := 'name=Alice';
  Expect('name: "$(name)"'#10, '{"name":"$(name)"}', o);
  include(o.Flags, jofPreprocessInput);
  Expect('name: "$(name)"'#10, '{"name":"Alice"}', o);
  include(o.Flags, jofNoExpand);
  Expect('name: "$(name)"'#10, '{"name":"$(name)"}', o);
  o := JopOptions(jfYaml, jfJson, [jofCompact]);
  Reject('broken: [1, 2'#10, o, jeInvalidYaml);
  o := JopOptions(jfJson, jfYaml);
  Expect('{"x":"\u0000"}', 'x: "?"'#10, o);
  Expect('{"\u0000":"x"}', '"?": x'#10, o);
  ok := (TestJopProcess('{"x":"\\u0000"}', o, output, nil, @error) = jeSuccess);
  CheckUtf8(ok, error);
  o := JopOptions(jfYaml, jfJson, [jofCompact]);
  ok := (TestJopProcess(output, o, restored, nil, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(restored, '{"x":"\\u0000"}');
end;

procedure TTestCoreJop.Csv;
var
  o: TJopOptions;
  json, info: RawUtf8;
  error: TJopError;
begin
  o := JopOptions(jfCsv, jfJson, [jofCompact]);
  Expect('name,code'#13#10'Alice,007'#13#10'Bob,'#13#10,
    '[{"name":"Alice","code":"007"},{"name":"Bob","code":""}]', o);
  Expect('name,note'#10'"A, B","line 1'#13#10'line 2 ""quoted"""'#13#10,
    '[{"name":"A, B","note":"line 1\r\nline 2 \"quoted\""}]', o);
  Expect('a,b', '[]', o);
  Expect('a,b'#10',', '[{"a":"","b":""}]', o);
  Reject('', o, jeInvalidCsv);
  Reject(',b'#10'1,2', o, jeInvalidCsv);
  Reject('a,a'#10'1,2', o, jeInvalidCsv);
  Reject('a,b'#10'1', o, jeInvalidCsv);
  Reject('a,b'#10'1,2,3', o, jeInvalidCsv);
  Reject('a,b'#10'"unclosed,2', o, jeInvalidCsv);
  Reject('a,b'#10'"value"junk,2', o, jeInvalidCsv);
  Reject('a,b'#10'va"lue,2', o, jeInvalidCsv);
  o.CsvDelimiter := #9;
  Expect('a'#9'b'#10'1'#9'2', '[{"a":"1","b":"2"}]', o);
  o.CsvDelimiter := ';';
  Expect('a;b'#10'1;2', '[{"a":"1","b":"2"}]', o);
  o.CsvDelimiter := '"';
  Reject('a"b', o, jeInvalidOptions);
  o := JopOptions(jfCsv, jfJson, [jofCompact]);
  SetLength(o.Defines, 1);
  o.Defines[0] := 'name=Alice';
  Expect('name'#10'$(name)', '[{"name":"$(name)"}]', o);
  include(o.Flags, jofPreprocessInput);
  Expect('name'#10'$(name)', '[{"name":"Alice"}]', o);
  // The reusable importer reports its own enum, with no diagnostic allocation.
  Check(CsvToJson('a,b'#10'1,2', json) = ceNone);
  CheckEqual(json, '[{"a":"1","b":"2"}]');
  Check(CsvToJson('', json) = ceNoInput);
  CheckEqual(json, '');
  Check(CsvToJson('a,b'#10'1', json) = ceColumnCount);
  CheckEqual(json, '');
  Check(CsvToJson('a,a'#10'1,2', json) = ceInvalidHeader);
  Check(CsvToJson('a'#10'"broken', json) = ceInvalidQuote);
  Check(CsvToJson('a'#0'b', json) = ceEmbeddedNul);
  Check(CsvToJson('a', json, '"') = ceInvalidDelimiter);
  // Output follows the current trunk relational exporter, including IDs and LF.
  o := JopOptions(jfJson, jfCsv);
  Expect('[{"name":"Alice","code":"007"},{"code":"","name":"Bob"}]',
    '__rowid,name,code'#10'1,Alice,007'#10'2,Bob,'#10, o);
  Expect('[{"a":"A, B","b":"line 1\nline 2 \"quoted\""}]',
    '__rowid,a,b'#10'1,"A, B","line 1'#10'line 2 ""quoted"""'#10, o);
  Expect('[{"n":42,"b":true,"z":null}]',
    '__rowid,n,b,z'#10'1,42,true,'#10, o);
  Expect('[]', '__rowid,value'#10, o);
  Expect('{}', '__rowid'#10'1'#10, o);
  Expect('[{}]', '__rowid'#10'1'#10, o);
  Expect('[1,2]', '__rowid,value'#10'1,1'#10'2,2'#10, o);
  Expect('[{"a":{"b":1}}]', '__rowid,a.b'#10'1,1'#10, o);
  Expect('{"x":"\u0000"}', '__rowid,x'#10'1,?'#10, o);
  // Heterogeneous rows have unioned columns unless the native strict set is used.
  info := '';
  error := TestJopProcess('[{"a":1},{"b":2}]', o, json, nil, @info);
  Check(error = jeSuccess, 'native remarks are not a conversion error');
  Check(info <> '', 'native schema remarks remain available');
  CheckEqual(json, '__rowid,a,b'#10'1,1,'#10'2,,2'#10);
  o.TableOptions := TABLE_STRICT;
  Reject('[{"a":1},{"b":2}]', o, jeTableData);
  Reject('[{"a":1},{"a":2,"b":3}]', o, jeTableData);
  Reject('[{"a":1},{"a":"two"}]', o, jeTableData);
  o.TableOptions := TABLE_DEFAULT;
  Reject('[{"a":[1]}]', o, jeMultipleTables);
  // CsvDelimiter only configures input; the native exporter always uses comma.
  o.CsvDelimiter := ';';
  Expect('{"a":1,"b":2}', '__rowid,a,b'#10'1,1,2'#10, o);
end;

procedure TTestCoreJop.Tables;

  procedure NativeParity(const Source: RawUtf8; const Options: TJopOptions);
  var
    expected, actual: TTextPairs;
    md, output, info, root: RawUtf8;
    native: TTableResult;
    error: TJopError;
    i: PtrInt;
    f: TJopFormat;
    fmt: TTableTextFormat;
    o: TJopOptions;
  begin
    root := Options.TableRoot;
    if root = '' then
      root := 'root';
    native := JsonToCsv(Source, expected, info, Options.TableOptions, root);
    Check(native in [tbrSuccess, tbrSuccessWithRemarks]);
    error := TestJopProcessToCsv(Source, Options, actual);
    CheckUtf8(error = jeSuccess, JOP_ERROR[error]);
    if Check(length(actual) = length(expected), 'all relational tables') then
      for i := 0 to high(actual) do
      begin
        CheckEqual(actual[i].Name, expected[i].Name);
        CheckEqual(actual[i].Value, expected[i].Value);
      end;
    o := Options;
    for f := jfMd to jfMdPipe do
    begin
      o.OutputFormat := f;
      if f = jfMd then
        fmt := ttfMarkdownCsv
      else
        fmt := ttfMarkdownPipe;
      native := JsonToMd(Source, md, info, o.TableOptions, root, fmt);
      Check(native in [tbrSuccess, tbrSuccessWithRemarks]);
      error := TestJopProcess(Source, o, output);
      CheckUtf8(error = jeSuccess, JOP_ERROR[error]);
      CheckEqual(output, md, 'native Markdown exporter parity');
    end;
  end;

const
  ONE = '### root.csv'#10'```csv'#10'__rowid,name'#10 +
        '1,Alice'#10'```'#10#10;
  NESTED = '[{"name":"A","items":[1,2]},{"name":"B","items":[3]}]';
var
  o: TJopOptions;
  tables: TTextPairs;
  output, info, source: RawUtf8;
  error: TJopError;
  stats: TJopStatistics;
  i: PtrInt;
begin
  o := JopOptions(jfJson, jfMd);
  Expect('{"name":"Alice"}', ONE, o);
  o.InputFormat := jfYaml;
  Expect('name: Alice'#10, ONE, o);
  o.InputFormat := jfCsv;
  Expect('name'#10'Alice', ONE, o);
  o.InputFormat := jfJson;
  SetLength(o.Defines, 1);
  o.Defines[0] := 'name=Alice';
  Expect('{name:$(name)}', ONE, o);
  o.Defines := nil;
  NativeParity(NESTED, o);
  NativeParity('{"v":"a|b\\c\r\n<&>\t*_`!"}', o);
  NativeParity('[{"Name":1,"name":2},{"Name":3,"name":4}]', o);
  NativeParity('[1,2.5,null]', o);
  o.TableOptions := [tboNullAsText, tboKeepArrayIndex];
  NativeParity(NESTED, o);
  NativeParity('[1,2.5,null]', o);
  o.TableOptions := [tboMixedAsJson];
  NativeParity('[{"a":1},false,[2]]', o);
  o.TableOptions := TABLE_DEFAULT;
  o.TableRoot := 'orders';
  FillCharFast(stats, SizeOf(stats), 0);
  error := TestJopProcessToCsv(NESTED, o, tables, @stats);
  Check(error = jeSuccess);
  if Check(length(tables) = 2) then
  begin
    CheckEqual(tables[0].Name, 'orders');
    CheckEqual(tables[0].Value, '__rowid,name'#10'1,A'#10'2,B'#10);
    CheckEqual(tables[1].Name, 'orders.items');
    CheckEqual(tables[1].Value,
      '__rowid,__parentid,value'#10'1,1,1'#10'2,1,2'#10'3,2,3'#10);
    CheckEqual(stats.OutputBytes,
      length(tables[0].Value) + length(tables[1].Value));
  end;
  CheckEqual(stats.FilesSucceeded, 1);
  CheckEqual(stats.InputBytes, length(NESTED));
  // OutputFormat is deliberately irrelevant to the complete CSV collection API.
  o.OutputFormat := jfText;
  Check(TestJopProcessToCsv('{"n":1}', o, tables) = jeSuccess);
  if Check(length(tables) = 1) then
    CheckEqual(tables[0].Value, '__rowid,n'#10'1,1'#10);
  o.OutputFormat := jfCsv;
  Reject(NESTED, o, jeMultipleTables);
  o.TableOptions := TABLE_STRICT;
  info := 'old info';
  error := TestJopProcessToCsv('[{"a":1},{"b":2}]', o, tables, nil, @info);
  Check(error = jeTableData);
  CheckEqual(length(tables), 0, 'failure clears every CSV result');
  Check(info <> '', 'native table failure details');
  o.TableOptions := TABLE_DEFAULT;
  o.OutputFormat := jfMd;
  // The variant entry points preserve raw 8-bit data until UTF-8 is requested.
  source := '{"text":"' + TestJopBytes('ff') + '"}';
  Check(TestJopProcessToCsv(source, o, tables) = jeSuccess);
  if Check(length(tables) = 1) then
    CheckEqual(tables[0].Value, '__rowid,text'#10'1,' + TestJopBytes('ff') + #10);
  Check(TestJopProcess(source, o, output) = jeSuccess);
  Check(Pos(TestJopBytes('ff'), output) > 0);
  include(o.Flags, jofValidateUtf8);
  Reject(source, o, jeInvalidUtf8);
  Check(TestJopProcessToCsv(source, o, tables) = jeInvalidUtf8);
  CheckEqual(length(tables), 0);
  exclude(o.Flags, jofValidateUtf8);
  // Labels use the native exporters' escaping, independently of input checks.
  include(o.Flags, jofValidateUtf8);
  o.TableRoot := 'r' + TestJopBytes('ff');
  NativeParity('{}', o);
  o.TableRoot := 'root'#0'other';
  NativeParity('{}', o);
  exclude(o.Flags, jofValidateUtf8);
  o.TableRoot := '';
  // Structural validation remains opt-in for the native conversion paths.
  include(o.Flags, jofValidateJson);
  for i := ord(jfYaml) to ord(jfMdPipe) do
    if TJopFormat(i) in [jfYaml, jfCsv, jfMd, jfMdPipe] then
    begin
      o.OutputFormat := TJopFormat(i);
      Reject('{}{}', o, jeInvalidJson);
      Reject('"a" "b"', o, jeInvalidJson);
    end;
  exclude(o.Flags, jofValidateJson);
  o.OutputFormat := jfMd;
  // Depth errors come from the native exporter, not a JOP tree walk.
  source := '1';
  for i := 1 to TableMaxDepth + 1 do
    source := '{"a":' + source + '}';
  Reject(source, o, jeTableDepth);
end;

procedure TTestCoreJop.Buffers;
const
  DOC: RawUtf8 = '{"name":"Alice","note":"line\nnext",' +
    '"url":"https://example.org/a","quote":"I''m \"ready\"",' +
    '"path":"C:\\tmp\\file","dollar":"$5"}';
  ROWS: RawUtf8 = '[{"note":"one\ttwo"}]';
  SCALAR: RawUtf8 = '42';
var
  source, output, expected, info: RawUtf8;
  tables, wanted: TTextPairs;
  start: pointer;
  f: TJopFormat;
  pass: integer;
  o: TJopOptions;
  native: TTableResult;
begin
  for f := jfYaml to jfMdPipe do
    if f in [jfYaml, jfCsv, jfMd, jfMdPipe] then
    begin
      case f of
        jfYaml:
          expected := JsonToYaml(DOC);
        jfCsv:
          begin
            native := JsonToCsv(DOC, wanted, info);
            Check(native = tbrSuccess);
            expected := wanted[0].Value;
          end;
        jfMd, jfMdPipe:
          begin
            if f = jfMd then
              native := JsonToMd(DOC, expected, info,
                TABLE_DEFAULT, 'root', ttfMarkdownCsv)
            else
              native := JsonToMd(DOC, expected, info,
                TABLE_DEFAULT, 'root', ttfMarkdownPipe);
            Check(native = tbrSuccess);
          end;
      end;
      for pass := 0 to 1 do
      begin
        FastSetString(source, pointer(DOC), length(DOC));
        start := pointer(source);
        o := JopOptions(jfJson, f);
        if pass = 1 then
          include(o.Flags, jofNoExpand);
        Check(mormot.tools.jop.JopProcess(source, o, output) = jeSuccess);
        Check(pointer(source) = start, 'plain JSON is decoded in its original buffer');
        Check(source <> DOC, 'the caller-owned buffer is consumed by native decoding');
        CheckEqual(output, expected, 'escaped values retain native export behavior');
      end;
    end;
  // Exercise the direct all-table API with an array root too.
  FastSetString(source, pointer(ROWS), length(ROWS));
  start := pointer(source);
  Check(mormot.tools.jop.JopProcessToCsv(source, JopOptions(jfJson, jfCsv), tables) = jeSuccess);
  Check(pointer(source) = start);
  Check(source <> ROWS);
  native := JsonToCsv(ROWS, wanted, info);
  Check(native = tbrSuccess);
  if Check(length(tables) = 1) then
    CheckEqual(tables[0].Value, wanted[0].Value);
  // Markers outside strings still select native comment/DSL normalization.
  o := JopOptions(jfJson, jfYaml);
  Expect('// before'#10'{value:1} // after'#10, 'value: 1'#10, o);
  Expect('$$'#10'value=1'#10'$$'#10'{value:$(value)}', 'value: 1'#10, o);
  // Scalars use the native scalar decoder instead of a document cast.
  FastSetString(source, pointer(SCALAR), length(SCALAR));
  Check(mormot.tools.jop.JopProcess(source, JopOptions(jfJson, jfYaml), output) = jeSuccess);
  CheckEqual(output, '42'#10);
end;

procedure TTestCoreJop.Validation;
var
  o: TJopOptions;
  source: RawUtf8;
begin
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  source := '{"text":"' + TestJopBytes('e9') + '"}';
  Expect(source, source, o);
  include(o.Flags, jofValidateUtf8);
  Reject(source, o, jeInvalidUtf8);
  source := '{"text":"' + TestJopBytes('c3a9') + '"}';
  Expect(source, source, o);
  source := '{"text":"' + TestJopBytes('f09f9880') + '"}';
  Expect(source, source, o);
  Reject('{"text":"' + TestJopBytes('c0af') + '"}', o, jeInvalidUtf8);
  Reject('{"text":"' + TestJopBytes('eda080') + '"}', o, jeInvalidUtf8);
  Reject('{"text":"' + TestJopBytes('f4908080') + '"}', o, jeInvalidUtf8);
  o.Flags := [jofCompact];
  Reject('{}'#0'{}', o, jeEmbeddedNul);
  SetLength(o.Defines, 1);
  o.Defines[0] := 'v=' + TestJopBytes('e9');
  Expect('{v:$(v)}', '{"v":"' + TestJopBytes('e9') + '"}', o);
  include(o.Flags, jofValidateUtf8);
  Reject('{v:$(v)}', o, jePreprocess);
  o.Defines[0] := 'v=before'#0'after';
  exclude(o.Flags, jofValidateUtf8);
  Reject('{v:$(v)}', o, jePreprocess);
  o := JopOptions(jfText, jfText);
  Expect('raw ' + TestJopBytes('ff'), 'raw ' + TestJopBytes('ff'), o);
  Reject('before'#0'after', o, jeEmbeddedNul);
  include(o.Flags, jofValidateUtf8);
  Reject('raw ' + TestJopBytes('ff'), o, jeInvalidUtf8);
  o := JopOptions(jfCsv, jfJson, [jofCompact, jofValidateUtf8]);
  Reject('name'#10 + TestJopBytes('e9'), o, jeInvalidUtf8);
  o := JopOptions(jfYaml, jfJson, [jofCompact, jofValidateUtf8]);
  Reject('name: ' + TestJopBytes('e9'), o, jeInvalidUtf8);
end;

procedure TTestCoreJop.Includes;
var
  o: TJopOptions;
  s: TJopStatistics;
  output, error, source: RawUtf8;
  fn: TFileName;
  ok: boolean;
begin
  WriteFile('included.txt', 'hello $(name)'#10);
  WriteFile('empty.txt', '');
  WriteFile('jop#config.txt', 'quoted name'#10);
  o := JopOptions(jfText, jfText);
  SetLength(o.Defines, 1);
  o.Defines[0] := 'name=world';
  Reject('$$'#10'include included.txt'#10'$$'#10, o, jePreprocess);
  o.IncludeRoot := fFolder;
  source := '$$'#10'include included.txt'#10'insert included.txt'#10'$$'#10'done';
  FillCharFast(s, SizeOf(s), 0);
  ok := (TestJopProcess(source, o, output, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(output, 'hello world'#10'hello $(name)'#10'done');
  CheckEqual(s.IncludeFiles, 1);
  CheckEqual(s.InsertFiles, 1);
  CheckEqual(s.IncludeBytes, 2 * length('hello $(name)'#10));
  CheckEqual(s.InputBytes, length(source));
  CheckEqual(s.OutputBytes, length(output));
  CheckEqual(s.FilesSucceeded, 1);
  CheckEqual(s.FilesFailed, 0);
  Expect('$$'#10'include "jop#config.txt"'#10'$$'#10,
    'quoted name'#10, o);
  Reject('$$'#10'include missing.txt'#10'$$'#10'done', o, jePreprocess);
  Reject('$$'#10'include empty.txt'#10'$$'#10'done', o, jePreprocess);
  include(o.Flags, jofAllowMissingIncludes);
  Expect('$$'#10'include missing.txt'#10'$$'#10'done', 'done', o);
  Reject('$$'#10'include empty.txt'#10'$$'#10'done', o, jePreprocess);
  WriteFile('bom-only.txt', TestJopBytes('efbbbf'));
  Reject('$$'#10'include bom-only.txt'#10'$$'#10'done', o, jePreprocess);
  Reject('$$'#10'include ../outside.txt'#10'$$'#10'done', o, jePreprocess);
  fn := fFolder + 'included.txt';
  source := Make(['$$'#10'include "', fn, '"'#10'$$'#10]);
  Reject(source, o, jePreprocess);
  include(o.Flags, jofAbsolutePaths);
  Expect(source, 'hello world'#10, o);
  WriteFile('loop.txt', '$$'#10'include loop.txt'#10'$$'#10);
  Reject('$$'#10'include loop.txt'#10'$$'#10, o, jePreprocess);
  WriteFile('bytes.txt', 'bad ' + TestJopBytes('e9'));
  Expect('$$'#10'include bytes.txt'#10'$$'#10, 'bad ' + TestJopBytes('e9'), o);
  include(o.Flags, jofValidateUtf8);
  Reject('$$'#10'include bytes.txt'#10'$$'#10, o, jePreprocess);
  Reject('$$'#10'insert bytes.txt'#10'$$'#10, o, jePreprocess);
  source := 'before0after';
  CheckUtf8(source[7] = '0', source);
  source[7] := #0;
  CheckUtf8(source[7] = #0, source);
  WriteFile('zero.txt', source); // was 'nul.txt' but Windows silently skip it
  exclude(o.Flags, jofValidateUtf8);
  Reject('$$'#10'include zero.txt'#10'$$'#10, o, jePreprocess);
  Reject('$$'#10'insert zero.txt'#10'$$'#10, o, jePreprocess);
  WriteFile('fragment.json', 'answer:42');
  WriteFile('broken.json', 'answer:"unterminated');
  WriteFile('trailing.json', '{"ok":1}}');
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  o.IncludeRoot := fFolder;
  Expect('{'#10'$$'#10'include fragment.json'#10'$$'#10'}',
    '{"answer":42}', o);
  Reject('{'#10'$$'#10'include broken.json'#10'$$'#10'}', o, jePreprocess);
  Reject('{'#10'$$'#10'insert broken.json'#10'$$'#10'}', o, jePreprocess);
  Reject('$$'#10'insert trailing.json'#10'$$'#10, o, jePreprocess);
end;

procedure TTestCoreJop.Writers;
var
  o: TJopOptions;
  s: TJopStatistics;
  output, error, source: RawUtf8;
  w: TJsonWriter;
  stream: TMemoryStream;
  temp: TTextWriterStackBuffer;
  ok: boolean;
begin
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  FillCharFast(s, SizeOf(s), 0);
  w := TJsonWriter.CreateOwnedStream(temp);
  try
    w.AddShort('before:');
    ok := (TestJopProcessToWriter('{n:1}', w, o, @s, @error) = jeSuccess);
    CheckUtf8(ok, error);
    w.AddShort(':after');
    w.SetText(output);
    CheckEqual(output, 'before:{"n":1}:after');
    CheckEqual(s.OutputBytes, length('{"n":1}'));
    // Native formatting must restore caller state on success and failure.
    w.CancelAll;
    w.CustomOptions := [twoForceJsonStandard];
    w.HumanReadableLevel := 3;
    Check(TestJopProcessToWriter('{n:1}', w, JopOptions(jfJson, jfJson5)) = jeSuccess);
    Check(w.CustomOptions = [twoForceJsonStandard]);
    CheckEqual(w.HumanReadableLevel, 3);
    w.CancelAll;
    w.HumanReadableLevel := 3;
    Check(TestJopProcessToWriter('{n:[1}', w, JopOptions(jfJson, jfJson5)) = jeInvalidJson);
    Check(w.CustomOptions = [twoForceJsonStandard]);
    CheckEqual(w.HumanReadableLevel, 3);
  finally
    w.Free;
  end;
  stream := TMemoryStream.Create;
  try
    output := 'prefix:';
    stream.WriteBuffer(pointer(output)^, length(output));
    ok := (TestJopProcessToStream('{n:2}', stream, o, @s, @error) = jeSuccess);
    CheckUtf8(ok, error);
    CheckEqual(stream.Position, length('prefix:{"n":2}'));
    output := ':suffix';
    stream.WriteBuffer(pointer(output)^, length(output));
    FastSetString(output, stream.Memory, stream.Size);
    CheckEqual(output, 'prefix:{"n":2}:suffix');
    CheckEqual(s.FilesSucceeded, 2);
    CheckEqual(s.InputBytes, 2 * length('{n:1}'));
    CheckEqual(s.OutputBytes, 2 * length('{"n":1}'));
    Check(s.ElapsedMicroSeconds >= 0);
    Check(TestJopProcessToStream('{}', nil, o) = jeInvalidOptions);
    Check(TestJopProcessToWriter('{}', nil, o) = jeInvalidOptions);
    // overwrite at the requested position without truncating an existing tail
    stream.Clear;
    output := 'abcdefxxxxxxxxxxxTAIL';
    stream.WriteBuffer(pointer(output)^, length(output));
    stream.Position := 6;
    ok := (TestJopProcessToStream('{n:2}', stream, o, nil, @error) = jeSuccess);
    CheckUtf8(ok, error);
    CheckEqual(stream.Position, 13);
    FastSetString(output, stream.Memory, stream.Size);
    CheckEqual(output, 'abcdef{"n":2}xxxxTAIL');
    // exercise writer flushing beyond the stack buffer, preserving every byte
    source := RawUtf8(StringOfChar('x', 200000));
    o := JopOptions(jfText, jfText, [jofNoExpand]);
    stream.Clear;
    ok := (TestJopProcessToStream(source, stream, o, nil, @error) = jeSuccess);
    CheckUtf8(ok, error);
    FastSetString(output, stream.Memory, stream.Size);
    CheckEqual(output, source);
  finally
    stream.Free;
  end;
  o := JopOptions(jfJson, jfJson, [jofCompact]);
  CheckNot((TestJopProcess('{broken', o, output, @s, @error) = jeSuccess));
  CheckEqual(s.FilesSucceeded, 2);
  CheckEqual(s.FilesFailed, 1);
  CheckEqual(s.OutputBytes, 2 * length('{"n":1}'));
end;

procedure TTestCoreJop.Files;

  procedure EncodedFile(const Name: TFileName;
    const Bytes, Expected: RawUtf8; Flags: TJopFlags);
  var
    src, dst: TFileName;
    opt: TJopOptions;
    stats: TJopStatistics;
  begin
    src := FileName(Name);
    dst := FileName(Name + '.out');
    Check(FileFromString(Bytes, src));
    Check(FileFromString('previous output', dst));
    opt := JopOptions(jfJson, jfJson, Flags + [jofCompact]);
    FillCharFast(stats, SizeOf(stats), 0);
    Check(JopProcessFile(src, dst, opt, @stats) = jeSuccess);
    CheckEqual(stats.InputBytes, length(Expected), 'processed bytes after BOM decoding');
    CheckEqual(StringFromFile(dst), Expected, 'decoded content replaces output');
    CheckEqual(StringFromFile(src), Bytes, 'source bytes are preserved');
  end;

var
  o: TJopOptions;
  s: TJopStatistics;
  source, dest, planned: TFileName;
  error, bytes, expected: RawUtf8;
  i: integer;
  ok: boolean;
begin
  source := FileName('one.hjson');
  dest := FileName('one.json');
  Check(FileFromString('{answer:42}', source));
  Check(FileFromString('old destination', dest));
  o := JopOptions(jfAuto, jfAuto, [jofCompact]);
  FillCharFast(s, SizeOf(s), 0);
  ok := (JopProcessFile(source, dest, o, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(RawUtf8FromFile(dest), '{"answer":42}');
  CheckEqual(RawUtf8FromFile(source), '{answer:42}');
  CheckEqual(s.FilesSucceeded, 1);
  CheckEqual(s.InputBytes, length('{answer:42}'));
  CheckEqual(s.OutputBytes, length('{"answer":42}'));
  Check(FileFromString('{answer:"broken}', source));
  Check(JopProcessFile(source, dest, o, @s) = jeInvalidJson);
  CheckEqual(RawUtf8FromFile(dest), '{"answer":42}');
  CheckEqual(s.FilesFailed, 1);
  Check(JopProcessFile(source, source, o) = jeSameFile);
  CheckEqual(RawUtf8FromFile(source), '{answer:"broken}');
  Check(JopProcessFile(fFolder + 'missing.json', dest, o) = jeNoSource);
  CheckEqual(RawUtf8FromFile(dest), '{"answer":42}');
  planned := FileName('planned.json');
  include(o.Flags, jofDryRun);
  ok := (JopProcessFile(source, planned, o, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckNot(FileExists(planned));
  CheckEqual(s.FilesSkipped, 1);
  // explicit format takes precedence over an unrecognized source extension
  source := FileName('one.data');
  Check(FileFromString('name: Alice'#10, source));
  o := JopOptions(jfYaml, jfJson, [jofCompact]);
  ok := (JopProcessFile(source, dest, o, nil, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(RawUtf8FromFile(dest), '{"name":"Alice"}');
  // Output extension inference includes native Markdown and one-table CSV.
  dest := FileName('one.md');
  o.OutputFormat := jfAuto;
  FillCharFast(s, SizeOf(s), 0);
  Check(JopProcessFile(source, dest, o, @s) = jeSuccess);
  CheckEqual(s.InputBytes, length('name: Alice'#10), 'input count precedes conversion');
  CheckEqual(RawUtf8FromFile(dest),
    '### root.csv'#10'```csv'#10'__rowid,name'#10'1,Alice'#10'```'#10#10);
  dest := FileName('one-pipe.md');
  o.OutputFormat := jfMdPipe;
  Check(JopProcessFile(source, dest, o) = jeSuccess);
  CheckEqual(RawUtf8FromFile(dest),
    '## root'#10#10'| __rowid | name |'#10 +
    '| --- | --- |'#10'| 1 | Alice |'#10#10);
  o.OutputFormat := jfAuto;
  dest := FileName('one.csv');
  Check(JopProcessFile(source, dest, o) = jeSuccess);
  CheckEqual(RawUtf8FromFile(dest), '__rowid,name'#10'1,Alice'#10);
  // A multi-table result cannot silently replace a single CSV destination.
  Check(FileFromString('name: Alice'#10'items: [1,2]'#10, source));
  Check(JopProcessFile(source, dest, o) = jeMultipleTables);
  CheckEqual(RawUtf8FromFile(dest), '__rowid,name'#10'1,Alice'#10);
  Check(JopProcessFile('', dest, o) = jeInvalidPath);
  // File decoding follows RawUtf8FromFile: BOMless bytes are unchanged,
  // UTF-8 BOM is stripped, and UTF-16 BOMs select native UTF-8 conversion.
  expected := '{"text":"' + TestJopBytes('c3a9') + '"}';
  EncodedFile('utf8-bom.json', TestJopBytes('efbbbf') + expected,
    expected, [jofValidateUtf8]);
  EncodedFile('utf16-le.json',
    TestJopBytes('fffe7b002200740065007800740022003a002200e90022007d00'),
    expected, [jofValidateUtf8]);
  EncodedFile('utf16-be.json',
    TestJopBytes('feff007b002200740065007800740022003a002200e90022007d'),
    expected, [jofValidateUtf8]);
  bytes := '{"text":"' + TestJopBytes('e9') + '"}';
  EncodedFile('eight-bit.json', bytes, bytes, []);
  source := FileName('invalid-utf8.json');
  dest := FileName('invalid-utf8.out');
  Check(FileFromString(bytes, source));
  Check(FileFromString('previous output', dest));
  o := JopOptions(jfJson, jfJson, [jofCompact, jofValidateUtf8]);
  Check(JopProcessFile(source, dest, o) = jeInvalidUtf8);
  CheckEqual(StringFromFile(dest), 'previous output');
  CheckEqual(StringFromFile(source), bytes);
  // The native reader collapses empty input and read failure to an empty
  // result; the file API reports jeRead instead of replacing output with it.
  source := FileName('empty.txt');
  o := JopOptions(jfText, jfText);
  for i := 0 to 3 do
  begin
    case i of
      0: bytes := '';
      1: bytes := TestJopBytes('efbbbf');
      2: bytes := TestJopBytes('fffe');
      3: bytes := TestJopBytes('feff');
    end;
    Check(FileFromString(bytes, source));
    Check(JopProcessFile(source, dest, o) = jeRead);
    CheckEqual(StringFromFile(dest), 'previous output');
    CheckEqual(StringFromFile(source), bytes);
  end;
end;

procedure TTestCoreJop.Batch;
var
  input, output, nested, expected: TFileName;
  o: TJopOptions;
  files: TJopFileResults;
  s: TJopStatistics;
  error: RawUtf8;
  i: PtrInt;
  ok: boolean;
begin
  input := Folder('input');
  nested := Folder('input' + PathDelim + 'nested');
  output := Folder('output', {create=}false);
  WriteFile('input' + PathDelim + 'a.hjson', '{a:1}');
  WriteFile('input' + PathDelim + 'nested' + PathDelim + 'b.hjson', '{b:2}');
  WriteFile('input' + PathDelim + 'skip.txt', 'ignore');
  o := JopOptions(jfAuto, jfJson, [jofCompact, jofRecursive, jofDryRun]);
  FillCharFast(s, SizeOf(s), 0);
  ok := (JopProcessFiles(input + '*.hjson', output, o, files, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(length(files), 2);
  CheckEqual(s.FilesSkipped, 2);
  CheckEqual(s.InputBytes, 0);
  for i := 0 to high(files) do
  begin
    Check(files[i].Status = jfsSkipped);
    CheckNot(FileExists(files[i].Dest));
    Check(files[i].Error = jeSuccess);
  end;
  CheckNot(DirectoryExists(output), 'dry run does not create directories');
  expected := FileName('output' + PathDelim + 'a.json');
  Folder('output' + PathDelim + 'nested', {create=}false);
  FileName('output' + PathDelim + 'nested' + PathDelim + 'b.json');
  exclude(o.Flags, jofDryRun);
  FillCharFast(s, SizeOf(s), 0);
  ok := (JopProcessFiles(input + '*.hjson', output, o, files, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(length(files), 2);
  CheckEqual(s.FilesSucceeded, 2);
  CheckEqual(RawUtf8FromFile(expected), '{"a":1}');
  CheckEqual(RawUtf8FromFile(output + 'nested' + PathDelim + 'b.json'), '{"b":2}');
  // the whole plan must be rejected before replacing any destination
  WriteFile('input' + PathDelim + 'a.jsonc', '{a:99}');
  o.Flags := [jofCompact];
  Check(JopProcessFiles(input + 'a.*', output, o, files) = jeCollision);
  for i := 0 to high(files) do
    Check(files[i].Error = jeCollision);
  CheckEqual(RawUtf8FromFile(expected), '{"a":1}');
  Check(JopProcessFiles(input + '*.hjson', '', o, files) = jeInvalidPath);
  WriteFile('input' + PathDelim + 'exact.json', '{"same":1}');
  Check(JopProcessFiles(input + 'exact.json', input, o, files) = jeCollision);
  CheckEqual(RawUtf8FromFile(input + 'exact.json'), '{"same":1}');
  // one malformed file must not prevent other planned files from succeeding
  WriteFile('input' + PathDelim + 'bad.hjson', '{bad:"unfinished}');
  FileName('output' + PathDelim + 'bad.json');
  FillCharFast(s, SizeOf(s), 0);
  Check(JopProcessFiles(input + '*.hjson', output, o, files, @s) = jeInvalidJson);
  CheckEqual(s.FilesSucceeded, 1);
  CheckEqual(s.FilesFailed, 1);
  CheckEqual(RawUtf8FromFile(expected), '{"a":1}');
  CheckNot(FileExists(output + 'bad.json'));
  // recursive enumeration must not rediscover an output directory below input
  output := Folder('input' + PathDelim + 'generated');
  WriteFile('input' + PathDelim + 'generated' + PathDelim + 'old.hjson', '{old:1}');
  o.Flags := [jofRecursive, jofDryRun, jofCompact];
  FillCharFast(s, SizeOf(s), 0);
  ok := (JopProcessFiles(input + '*.hjson', output, o, files, @s, @error) = jeSuccess);
  CheckUtf8(ok, error);
  CheckEqual(length(files), 3);
  CheckEqual(s.FilesSkipped, 3);
  CheckEqual(s.InputBytes, 0);
  Check(JopProcessFiles(input + '*.absent', output, o, files) = jeNoFiles);
  CheckEqual(length(files), 0);
end;


procedure TTestCoreJop.Generate;
var
  w: TJsonWriter;
  temp: TTextWriterStackBuffer;
  first, second, deep, scalar: RawUtf8;
begin
  w := TJsonWriter.CreateOwnedStream(temp);
  try
    Check(JopGenerate(w, 0, 0, 1234) = jeSuccess);
    w.SetText(scalar);
    Check(IsValidJson(scalar, true));
    if Check(scalar <> '') then
      CheckNot(scalar[1] in ['[', '{'], 'depth zero is a scalar');
    w.CancelAll;
    Check(JopGenerate(w, 0, 1, 42) = jeSuccess);
    w.SetText(first);
    Check(IsValidJson(first, true), 'decimal scalar has no trailing separator');
    w.CancelAll;
    Check(JopGenerate(w, 1, 7, 1234) = jeSuccess);
    w.SetText(first);
    Check(IsValidJson(first, true));
    if Check((first <> '') and (first[1] = '[')) then
      CheckEqual(JsonArrayCount(PUtf8Char(pointer(first)) + 1), 7);
    w.CancelAll;
    Check(JopGenerate(w, 4, 3, 1234) = jeSuccess);
    w.SetText(first);
    Check(IsValidJson(first, true));
    w.CancelAll;
    Check(JopGenerate(w, 4, 3, 1234) = jeSuccess);
    w.SetText(second);
    CheckEqual(first, second, 'same parameters reproduce the same document');
    w.CancelAll;
    Check(JopGenerate(w, 4, 3, 987654321) = jeSuccess);
    w.SetText(second);
    Check(first <> second, 'seed changes generated values');
    w.CancelAll;
    Check(JopGenerate(w, 8, 3, 1234) = jeSuccess);
    w.SetText(deep);
    Check(IsValidJson(deep, true));
    Check(length(deep) < length(first) * 4, 'depth does not multiply child branches');
    w.CancelAll;
    Check(JopGenerate(w, 64, 1, 1234) = jeSuccess);
    w.SetText(deep);
    Check(IsValidJson(deep, true), 'maximum supported depth');
    w.CancelAll;
    Check(JopGenerate(w, 1, 0, 0) = jeSuccess);
    w.SetText(first);
    CheckEqual(first, '[]');
    w.CancelAll;
    w.AddShort('prefix:');
    Check(JopGenerate(w, 0, 1, 1234) = jeSuccess);
    w.AddShort(':suffix');
    w.SetText(first);
    CheckEqual(first, 'prefix:' + scalar + ':suffix', 'writer remains caller-owned');
    w.CancelAll;
    Check(JopGenerate(w, -1, 1, 1) = jeInvalidOptions);
    Check(JopGenerate(w, 65, 1, 1) = jeInvalidOptions);
    Check(JopGenerate(w, 1, -1, 1) = jeInvalidOptions);
    CheckEqual(w.TextLength, 0, 'invalid bounds append no output');
    Check(JopGenerate(nil, 1, 1, 1) = jeInvalidOptions);
  finally
    w.Free;
  end;
end;

procedure TTestCoreJop.CompareFiles;
var
  left, right: TFileName;
  a, b, info: RawUtf8;
  equal: boolean;

  procedure Compare(const A, B: RawUtf8; Expected: boolean);
  begin
    Check(FileFromString(A, left));
    Check(FileFromString(B, right));
    equal := not Expected;
    info := 'old info';
    Check(JopCompareFiles(left, right, equal, @info) = jeSuccess);
    Check(equal = Expected);
    CheckEqual(info, '');
    CheckEqual(StringFromFile(left), A);
    CheckEqual(StringFromFile(right), B);
  end;

begin
  left := FileName('compare-left.jsonc');
  right := FileName('compare-right.json');
  Compare('{a:1,b:"two"}', '{ "a" : 1, "b" : "two" } // end'#10, true);
  Compare('{"a":1,"b":2}', '{"b":2,"a":1}', false);
  Compare('{"a":1}', '{"a":2}', false);
  Compare('{"n":1}', '{"n":1.0}', false);
  Compare(TestJopBytes('efbbbf') + '{"a":1}', '{a:1}', true);
  a := '{"raw":"' + TestJopBytes('ff') + '"}';
  b := '{ raw : "' + TestJopBytes('ff') + '" }';
  Compare(a, b, true);
  equal := true;
  Check(JopCompareFiles(fFolder + 'missing.json', right, equal) = jeNoSource);
  CheckNot(equal);
  Check(FileFromString('', left));
  equal := true;
  Check(JopCompareFiles(left, right, equal) = jeRead);
  CheckNot(equal);
end;


end.
