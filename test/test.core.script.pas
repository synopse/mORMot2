/// regression tests for the framework Scripting abilities
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit test.core.script;

interface

{$I ..\src\mormot.defines.inc}

uses
  sysutils,
  variants,
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
  mormot.core.perf,
  mormot.core.test,
  mormot.lib.quickjs,
  mormot.script.core,
  mormot.script.quickjs;

type
  /// this test case will validate several low-level protocols
  TTestCoreScript = class(TSynTestCase)
  protected
    fNativeCallCount: integer;
    fNativeLastArgs: RawUtf8;
    /// native method for RegisterMethod test
    function NativeAdd(const This: variant; const Args: array of variant): variant;
    /// native method with string concatenation
    function NativeConcat(const This: variant; const Args: array of variant): variant;
  published
    /// QuickJS low-level direct API tests
    procedure QuickJSLowLevel;
    /// TQuickJSEngine high-level API tests
    procedure QuickJSEngine;
    /// TQuickJSEngine.RegisterMethod tests
    procedure QuickJSRegisterMethod;
    /// TQuickJSEngine.Global late-binding access tests
    // - Tests DoFunction for late-binding function calls like global.func()
    procedure QuickJSGlobalLateBind;
  end;

  
implementation


{ TTestCoreScript }

var
  output: RawUtf8;

function dolog(ctx: JSContext; this_val: JSValueRaw; argc: integer;
  argv: PJSValues): JSValueRaw; cdecl;
var
  i: integer;
begin
  for i := 0 to argc - 1 do
    AddToCsv(ctx.ToUtf8(argv[i]), output);
  result := JS_UNDEFINED;
end;

procedure TTestCoreScript.QuickJSLowLevel;
var
  rt: JSRuntime;
  cx: JSContext;

  function Run(const js: RawUtf8): RawUtf8;
  var
    v: JSValue;
    res: variant;
  begin
    Check(cx <> nil, 'cx');
    v := cx.Eval(js, '', JS_EVAL_TYPE_GLOBAL, result);
    if result = '' then
    begin
      Check(cx.ToVariant(v, res), 'tovar');
      cx.ToUtf8(v, result);
      if v.IsString or
         v.IsInt32 then
        CheckEqual(VariantToUtf8(res), result, 'istring/isint32')
      else if v.IsFloat then
        CheckSame(VariantToDoubleDef(res), GetExtended(pointer(result)),
          DOUBLE_SAME, 'isfloat');
    end;
    cx.Free(v);
    //writeln(js,' = ',result);
  end;

var
  v, v2: RawUtf8;
  j: JSValue;
  i: integer;
  d: double;
  i64: Int64;
  va: variant;
begin
  rt := JS_NewRuntime;
  try
    // validate numbers/text to/from JSValue conversion
    cx := rt.New;
    try
      cx.SetFunction([], 'log', @dolog, 1);
      for i := -100 to 100 do
      begin
        // 32-bit integer
        j.From32(i * 777);
        Check(j.IsNumber);
        Check(not j.IsNull);
        Check(not j.IsRefCounted);
        Check(not j.IsFloat);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsInt32);
        CheckEqual(j.Int32, i * 777);
        Check(cx.ToVariantFree(j, va));
        Check(va = i * 777);
        cx.Free(j); // do-nothing
        // float
        d := i * 777777777.77;
        j.FromNum(d);
        Check(j.IsNumber);
        Check(not j.IsRefCounted);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsFloat = (i <> 0));
        if i = 0 then
          CheckEqual(j.Int32, 0)
        else
          CheckSame(j.F64, d);
        i64 := i * Int64(77777777777);
        cx.Free(j);
        // 32-bit and 64-bit integer
        j.From64(i64);
        Check(j.IsNumber);
        Check(not j.IsRefCounted);
        Check(not j.IsString);
        Check(not j.IsNan);
        Check(j.IsInt32 = (abs(i64) < maxInt));
        if j.IsInt32 then
          CheckEqual(j.Int32, i64)
        else
          Check(j.IsFloat);
        CheckEqual(j.Int64, i64);
        cx.Free(j);
        // UTF-8 text
        if i and 7 = 0 then
          v := Int32ToUtf8(i) // Ascii
        else
          v := RandomUtf8(i + 100); // UTF-8
        j := cx.From(v);
        Check(not j.IsNumber);
        Check(not j.IsNull);
        Check(not j.IsFloat);
        Check(j.IsString);
        Check(j.IsRefCounted);
        cx.ToUtf8(j, v2);
        CheckEqual(v, v2, v);
        cx.Free(j);
        Check(j.Equals(j));
      end;
      j.From(true);
      Check(not j.IsRefCounted);
      Check(j.Bool);
      j.From(false);
      Check(not j.IsRefCounted);
      Check(not J.Bool);
      for i := 1 to 1000 do
      begin
        // basic runtime execution
        CheckEqual(Run(
          '2+2'),
          '4');
        CheckEqual(Run(
          'a=1234567; a.toString()'),
          '1234567');
        CheckEqual(Run(
          'function add(x, y) { return x + y; } add(434,343)'),
          '777');
        CheckEqual(Run(
          'add(434.732,343.045)'),
          '777.777');
        CheckEqual(Run(
          'function fn(x,y) { return (Math.log(y) / Math.log(x)).toString(); }'#10 +
          'fn(5,625)'),
          '4'); // 5 x 5 x 5 x 5 = 625
        v := Run(
          'Date.now()');
        CheckUtf8(abs(UnixMSTimeUtcFast - round(GetExtended(pointer(v)))) < 100,
          'timestamp - may fail during slow debugging [%]', [v]);
{        v := Run('new Date().toString();');
        Check(PosEx(' ' +UInt32ToUtf8(CurrentYear), v) > 0); }
        v := Run(
          'console.log("Hello World");');
        Check(PosEx('''console'' is not defined', v) > 0);
        j := cx.Call('', 'add', [777, i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = i + 777);
        j := cx.Call('', 'add', ['777', i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = '777' + UInt32ToUtf8(i));
        j := cx.Call('', 'add', ['3', i]);
        Check(cx.ToVariantFree(j, va));
        Check(va = '3' + UInt32ToUtf8(i));
        va := cx.CallVariant('', 'add', [777.777, i]);
        CheckSame(double(va), 777.777 + i);
        va := cx.CallVariant('', 'add', ['777', i]);
        Check(va = '777' + UInt32ToUtf8(i));
      end;
      CheckEqual(Run('JSON.stringify({ x: 5, y: 6 })'), '{"x":5,"y":6}');
      CheckEqual(Run('[3, 4, 5].map(x => x ** 10).forEach(x => log(x))'), 'undefined');
      CheckEqual(output, '59049,1048576,9765625');
      Check(not cx.GetValue('notexisting', j));
      CheckEqual(cx.EvalGlobal('var car = {type:"Fiat", model:"500", color:"white"};'), '');
      v := '{"type":"Fiat","model":"500","color":"white"}';
      CheckEqual(Run('JSON.stringify(car)'), v);
      Check(cx.GetValue(['car'], j));
      CheckEqual(cx.ToUtf8(j), v);
      Check(cx.ToVariantFree(j, va));
      Check(_Safe(va, dvObject)^.Count = 3);
      Check(cx.GetValue('add', j));
      Check(cx.ToVariant(j, va));
      v := cx.ToUtf8Free(j);
      CheckEqual(v, 'function add(x, y) { return x + y; }');
      Check(v = va);
    finally
      cx.Done;
    end;
  finally
    Check(rt.DoneSafe = '');
  end;
end;

procedure TTestCoreScript.QuickJSEngine;
var
  manager: TThreadSafeManager;
  engine: TQuickJSEngine;
  res: variant;
  hadError: boolean;
begin
  manager := TThreadSafeManager.Create(TQuickJSEngine, nil, 1);
  try
    engine := manager.NewEngine as TQuickJSEngine;
    // NewEngine creates engine outside pool, so we must Free it ourselves
    try
      // basic evaluation
      res := engine.Evaluate('2 + 2');
      CheckEqual(integer(res), 4, 'eval result');
      // string result
      res := engine.Evaluate('"Hello, " + "World!"');
      CheckEqual(VariantToUtf8(res), 'Hello, World!', 'string result');
      // float result
      res := engine.Evaluate('3.14159 * 2');
      CheckSame(double(res), 6.28318, 0.0001, 'float result');
      // boolean result
      res := engine.Evaluate('true && !false');
      Check(boolean(res) = true, 'bool result');
      // array result
      res := engine.Evaluate('[ 1, 2, 3 ]');
      Check(_Safe(res)^.IsArray, 'array type');
      CheckEqual(_Safe(res)^.ToJson, '[1,2,3]');
      // object result
      res := engine.Evaluate('({x:1,y:2})');
      Check(_Safe(res)^.IsObject, 'object type');
      CheckEqual(_Safe(res)^.ToJson, '{"x":1,"y":2}');
      // error handling
      if not IsDebuggerPresent then // do not pollute the IDE
      begin
        hadError := false;
        TSynLog.Family.ExceptionIgnoreCurrentThread := true;
        try
          res := engine.Evaluate('syntax error !!!');
        except
          on E: EQuickJSEngine do
          begin
            hadError := true;
            Check(E.Message <> '', 'should have error message');
          end;
        end;
        TSynLog.Family.ExceptionIgnoreCurrentThread := false;
        Check(hadError, 'should raise exception');
      end;
      // GlobalObject access - check it's initialized
      Check(not engine.GlobalObj.IsUninitialized, 'global not uninitialized');
      // define function and call it
      engine.Evaluate('function multiply(a, b) { return a * b; }');
      res := engine.Evaluate('multiply(6, 7)');
      CheckEqual(integer(res), 42, 'func result');
      // garbage collection
      engine.GarbageCollect;
    finally
      engine.Free;
    end;
  finally
    manager.Free;
  end;
end;

function TTestCoreScript.NativeAdd(const This: variant;
  const Args: array of variant): variant;
var
  v, a, i: integer;
begin
  inc(fNativeCallCount);
  v := 0;
  for i := 0 to high(Args) do
    if VariantToInteger(Args[i], a) then
      inc(v, a);
  result := v;
end;

function TTestCoreScript.NativeConcat(const This: variant;
  const Args: array of variant): variant;
var
  i: PtrInt;
begin
  inc(fNativeCallCount);
  fNativeLastArgs := '';
  for i := 0 to high(Args) do
  begin
    if i <> 0 then
      Append(fNativeLastArgs, ', ');
    Append(fNativeLastArgs, [Args[i]]);
  end;
  result := fNativeLastArgs;
end;

procedure TTestCoreScript.QuickJSRegisterMethod;
var
  manager: TThreadSafeManager;
  engine: TQuickJSEngine;
  res: variant;
begin
  manager := TThreadSafeManager.Create(TQuickJSEngine, nil, 1);
  try
    engine := manager.NewEngine as TQuickJSEngine;
    try
      fNativeCallCount := 0;
      // register native method
      Check(engine.RegisterMethod(engine.GlobalObj, 'nativeAdd', NativeAdd, 2),
        'register nativeAdd');
      Check(engine.RegisterMethod(engine.GlobalObj, 'nativeConcat', NativeConcat, 3),
        'register nativeConcat');
      // call native method from JavaScript
      res := engine.Evaluate('nativeAdd(100, 200)');
      CheckEqual(fNativeCallCount, 1, 'call count 1');
      CheckEqual(integer(res), 300, 'nativeAdd result');
      // call with expression
      res := engine.Evaluate('nativeAdd(10, 20) * 2');
      CheckEqual(fNativeCallCount, 2, 'call count 2');
      CheckEqual(integer(res), 60, 'nativeAdd in expression');
      // call nativeConcat
      res := engine.Evaluate('nativeConcat("hello", "world", "test")');
      CheckEqual(fNativeCallCount, 3, 'call count 3');
      CheckEqual(VariantToUtf8(res), 'hello, world, test', 'nativeConcat result');
      CheckEqual(fNativeLastArgs, 'hello, world, test', 'fNativeLastArgs');
      // use native function in JS function
      engine.Evaluate('function double(x) { return nativeAdd(x, x); }');
      res := engine.Evaluate('double(21)');
      CheckEqual(fNativeCallCount, 4, 'call count 4');
      CheckEqual(integer(res), 42, 'double via nativeAdd');
    finally
      engine.Free;
    end;
  finally
    manager.Free;
  end;
end;

procedure TTestCoreScript.QuickJSGlobalLateBind;
var
  manager: TThreadSafeManager;
  engine: TQuickJSEngine;
  res: variant;
  global: variant;
begin
  manager := TThreadSafeManager.Create(TQuickJSEngine, nil, 1);
  try
    engine := manager.NewEngine as TQuickJSEngine;
    try
      // define functions in JavaScript
      engine.Evaluate('function add(a, b) { return a + b; }');
      engine.Evaluate('function greet(name) { return "Hello, " + name + "!"; }');
      engine.Evaluate('function square(x) { return x * x; }');
      engine.Evaluate('var config = { version: "1.0", debug: true };');
      // test via evaluate (no late-binding)
      res := engine.Evaluate('add(10, 20)');
      CheckEqual(integer(res), 30, 'evaluate add');
      res := engine.Evaluate('greet("World")');
      CheckEqual(VariantToUtf8(res), 'Hello, World!', 'evaluate greet');
      res := engine.Evaluate('square(7)');
      CheckEqual(integer(res), 49, 'evaluate square');
      res := engine.Evaluate('config');
      Check(_Safe(res)^.IsObject, 'config is object');
      CheckEqual(_Safe(res)^.U['version'], '1.0', 'config.version');
      Check(_Safe(res)^.B['debug'] = true, 'config.debug');
      // chain calls via evaluate
      res := engine.Evaluate('add(square(3), square(4))');
      CheckEqual(integer(res), 25, 'chained: 3^2 + 4^2 = 25');
      // access Global for late-binding
      global := engine.Global;
      Check(not VarIsEmpty(global), 'global not empty');
      // test late-binding function calls (DoFunction)
      res := global.add(10, 20);
      CheckEqual(integer(res), 30, 'late-bind add');
      res := global.greet('World');
      CheckEqual(VariantToUtf8(res), 'Hello, World!', 'late-bind greet');
      res := global.square(7);
      CheckEqual(integer(res), 49, 'late-bind square');
      // test chained late-binding
      res := global.add(global.square(3), global.square(4));
      CheckEqual(integer(res), 25, 'late-bind chained');
      // clear before cleanup
      VarClear(res);
      VarClear(global);
      engine.GarbageCollect;
    finally
      engine.Free;
    end;
  finally
    manager.Free;
  end;
end;


end.

