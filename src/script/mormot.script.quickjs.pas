/// QuickJS JavaScript Engine High-Level Integration
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.script.quickjs;

{
  *****************************************************************************

   QuickJS JavaScript Interpreter High-Level Integration
    - TQuickJSEngine Thread-Safe Engine Implementation
    - TQuickJSVariant Custom Variant Type for Late-Binding

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef LIBQUICKJSSTATIC}
  {$define LIBQUICKJS}
{$endif LIBQUICKJSSTATIC}

{$ifdef LIBQUICKJS}

uses
  sysutils,
  classes,
  variants,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.rtti,
  mormot.lib.quickjs,
  mormot.script.core;


{ ******************** TQuickJSEngine Thread-Safe Engine Implementation }

type
  // forward declarations
  TQuickJSEngine = class;

  /// exception class for QuickJS engine errors
  EQuickJSEngine = class(EScriptException);

  /// callback signature for native method registration (variant-based)
  // - This: the 'this' object as variant
  // - Args: array of variant arguments
  // - returns: result as variant
  TQuickJSMethodVariant = function(const This: variant;
    const Args: array of variant): variant of object;
  PQuickJSMethodVariant = ^TQuickJSMethodVariant;
  TQuickJSMethodVariantDynArray = array of TQuickJSMethodVariant;

  /// convenient wrapper for JavaScript object operations
  {$ifdef USERECORDWITHMETHODS}
  TQuickJSObject = record
  {$else}
  TQuickJSObject = object
  {$endif USERECORDWITHMETHODS}
  private
    fEngine: TQuickJSEngine;
    fObj: JSValue;
    fDupObj: JSValue;
    fRooted: boolean;
  public
    /// initialize the wrapper
    procedure Init(aEngine: TQuickJSEngine; const aObj: JSValue);
    /// check if object has a property
    function HasProperty(const PropName: RawUtf8): boolean;
    /// check if object has own property (not inherited)
    function HasOwnProperty(const PropName: RawUtf8): boolean;
    /// get property value as variant
    function GetPropValue(const PropName: RawUtf8): variant;
    /// set property value
    procedure SetPropValue(const PropName: RawUtf8; const Value: variant);
    /// define a property with value
    procedure DefineProperty(const PropName: RawUtf8; const Value: variant);
    /// run a method on this object
    function RunMethod(const MethodName: RawUtf8;
      const Args: array of variant): variant;
    /// protect object from garbage collection
    procedure Root;
    /// release GC protection
    procedure UnRoot;
    /// access to the underlying JSValue
    property Obj: JSValue
      read fObj;
    /// access to the engine
    property Engine: TQuickJSEngine
      read fEngine;
  end;

  /// thread-safe QuickJS JavaScript engine
  // - use TThreadSafeManager.ThreadSafeEngine to retrieve the Engine instance
  TQuickJSEngine = class(TThreadSafeEngine)
  protected
    fRt: JSRuntime;
    fCx: JSContext;
    fGlobalObj: JSValue;
    fGlobalObject: TQuickJSObject;
    fLastErrorMsg: RawUtf8;
    fLastErrorFileName: RawUtf8;
    fLastErrorLine: integer;
    fErrorExist: boolean;
    fTimeoutAborted: boolean;
    fMethods: TQuickJSMethodVariantDynArray;
    fMethodCount: integer;
    fTimeoutValue: cardinal;
    fTimeoutStartTickSec: cardinal;
    fLastGCTickSec: cardinal;
    fGlobal: variant;
    function AtomNew(const Name: RawUtf8): TScriptAtom; override;
    procedure AtomFree(Atom: TScriptAtom); override;
    procedure ClearLastError;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// initialize engine after construction (outside manager lock)
    // - called within TThreadSafeManager lock
    procedure AfterCreate; override;
    /// cleanup before destruction (outside manager lock)
    procedure BeforeDestroy; override;
    /// retrieve engine from context
    class function From(aContext: TScriptContext): TThreadSafeEngine; override;
    /// evaluate JavaScript code and return result as variant
    function Evaluate(const Script: RawUtf8; const ScriptName: RawUtf8 = ''): variant;
    /// evaluate JavaScript code (string/UnicodeString version)
    function EvaluateS(const Script: string;
      const ScriptName: RawUtf8 = ''): variant;
    /// trigger garbage collection
    procedure GarbageCollect;
    /// perform GC if beneficial (at most once per second)
    procedure MaybeGarbageCollect;
    /// create a new JavaScript object
    procedure NewObject(out Obj: TQuickJSObject);
    /// register a native Delphi method callable from JavaScript
    function RegisterMethod(const Obj: JSValue; const MethodName: RawUtf8;
      const Method: TQuickJSMethodVariant; ArgCount: integer): boolean;
    /// access to the QuickJS runtime
    property rt: JSRuntime
      read fRt;
    /// access to the QuickJS context
    property cx: JSContext
      read fCx;
    /// direct access to global JSValue
    property GlobalObj: JSValue
      read fGlobalObj;
    /// access to the global object wrapper
    property GlobalObject: TQuickJSObject
      read fGlobalObject;
    /// last error message from JavaScript execution
    property LastErrorMsg: RawUtf8
      read fLastErrorMsg;
    /// last error line number
    property LastErrorLine: integer
      read fLastErrorLine;
    /// last error file name
    property LastErrorFileName: RawUtf8
      read fLastErrorFileName;
    /// TRUE if an error occurred during JavaScript execution
    property ErrorExist: boolean
      read fErrorExist;
    /// late-binding variant access to the global object
    // - allows access like: Engine.Global.myVar := 123
    property Global: variant
      read fGlobal;
    /// timeout value in seconds for script execution (0 = no timeout)
    property TimeoutValue: cardinal
      read fTimeoutValue write fTimeoutValue;
    /// TRUE if timeout was triggered during last execution
    property TimeoutAborted: boolean
      read fTimeoutAborted;
  end;


{ ******************** TQuickJSVariant Custom Variant Type for Late-Binding }

type
  /// storage for TQuickJSVariant custom variant type
  // - will store a reference to a JavaScript object for late-binding access
  TQuickJSVariantData = packed record
    /// the custom variant type registered number - match TVarData.VType
    VType: TVarType;
    VFiller: array[0..SizeOf(TVarData) - SizeOf(TVarType)
      - SizeOf(TQuickJSEngine) - SizeOf(JSValue) - 1] of byte;
    /// the associated QuickJS engine
    Engine: TQuickJSEngine;
    /// the JavaScript object value (duplicated/rooted for GC protection)
    Obj: JSValue;
  end;
  PQuickJSVariantData = ^TQuickJSVariantData;

  /// custom variant type for late-binding JavaScript object access
  // - allows Pascal code to access JS object properties using variant syntax
  // - example: jsObj.propName or jsObj.method(arg1, arg2)
  TQuickJSVariant = class(TSynInvokeableVariantType)
  public
    /// get a property value by name
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean; override;
    /// set a property value by name
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
    /// call a JavaScript function via late-binding
    // - handles obj.method(args) syntax
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): boolean; override;
  public
    /// properly release the JavaScript object reference
    procedure Clear(var V: TVarData); override;
    /// copy variant data (duplicates the JS object reference)
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// create a variant containing a JavaScript object
    class procedure New(aEngine: TQuickJSEngine; const aObj: JSValue;
      var Result: variant);
  end;

var
  /// the global TQuickJSVariant custom variant type instance
  QuickJSVariantType: TQuickJSVariant;
  /// quick access to QuickJSVariantType.VarType identifier, casted as 32-bit
  QuickJSVariantVType: cardinal;


implementation

// Interrupt handler for timeout control
function QuickJSInterruptHandler(rt: JSRuntime; opaque: pointer): integer; cdecl;
var
  engine: TQuickJSEngine absolute opaque;
begin
  result := 0;  // 0 = continue execution
  if (engine = nil) or
     (engine.fTimeoutValue = 0) or
     (GetTickSec - engine.fTimeoutStartTickSec <= engine.fTimeoutValue) then
    exit;
  engine.fTimeoutAborted := true;
  result := 1;  // non-zero = abort execution
end;

// Native function callback wrapper for RegisterMethod
function QuickJSMethodDataCallback(ctx: JSContext; this_val: JSValueConst;
  argc: integer; argv: PJSValueConstArr; magic: integer;
  func_data: PJSValue): JSValueRaw; cdecl;
var
  engine: TQuickJSEngine;
  args: array of variant;
  thisVar, res: variant;
  ndx, i: PtrInt;
  jsres: JSValue;
begin
  try
    engine := JS_GetContextOpaque(ctx);
    // Get method index from func_data[0]
    ndx := func_data^.Int32;
    if PtrUInt(ndx) >= PtrUInt(engine.fMethodCount) then
      EQuickJSEngine.RaiseU('Invalid Callback Index');
    // Convert 'this' to variant
    engine.fCx.ToVariant(JSValue(this_val), thisVar);
    // Convert arguments
    SetLength(args, argc);
    for i := 0 to argc - 1 do
      engine.fCx.ToVariant(PJSValue(@argv[i])^, args[i]);
    // Call the Delphi method
    res := engine.fMethods[ndx](thisVar, args);
    // Convert result back to JSValue
    engine.fCx.FromVariant(res, jsres);
    result := JSValueRaw(jsres);
  except
    on E: Exception do
      result := ctx^.ThrowInternalError(E);
  end;
end;


{ TQuickJSEngine }

procedure TQuickJSEngine.AfterCreate;
begin
  // Create QuickJS runtime and context
  fRt := JS_NewRuntime;
  fCx := fRt.New;
  // Store self pointer in context for From() retrieval
  JS_SetContextOpaque(fCx, self);
  // Set up interrupt handler for timeout control
  JS_SetInterruptHandler(fRt, @QuickJSInterruptHandler, self);
  // Get global object
  fGlobalObj := JSValue(JS_GetGlobalObject(fCx));
  // Initialize GlobalObject wrapper
  fGlobalObject.Init(self, fGlobalObj);
  // Initialize late-binding Global variant
  if QuickJSVariantType <> nil then
    TQuickJSVariant.New(self, fGlobalObj, fGlobal);
  // Store in parent class fields
  fRuntime := fRt;
  fContext := fCx;
end;

procedure TQuickJSEngine.BeforeDestroy;
begin
  // Clear the late-binding Global variant (releases the duplicated JSValue)
  VarClear(fGlobal);
  // Free global object
  if not fGlobalObj.IsUninitialized then
    fCx.Free(fGlobalObj);
  // Free context and runtime
  fCx.Done;
  fRt.Done;
end;

class function TQuickJSEngine.From(aContext: TScriptContext): TThreadSafeEngine;
begin
  result := JS_GetContextOpaque(aContext);
end;

procedure TQuickJSEngine.ClearLastError;
begin
  fLastErrorMsg := '';
  fLastErrorFileName := '';
  fLastErrorLine := 0;
  fErrorExist := false;
  fTimeoutAborted := false;
end;

function TQuickJSEngine.Evaluate(const Script, ScriptName: RawUtf8): variant;
var
  res: JSValue;
begin
  ClearLastError;
  // Reset timeout tracking
  if fTimeoutValue <> 0 then
    fTimeoutStartTickSec := GetTickSec;
  // Execute the supplied Script
  res := fCx.Eval(Script, Script, JS_EVAL_TYPE_GLOBAL, fLastErrorMsg);
  if res.IsException then
  begin
    fErrorExist := true;
    fLastErrorFileName := ScriptName;
    if fTimeoutAborted then
      fLastErrorMsg := 'Script execution timeout';
    fCx.Free(res);
    EQuickJSEngine.RaiseU(fLastErrorMsg);
  end;
  if not fCx.ToVariantFree(res, result) then
    VarClear(result);
end;

function TQuickJSEngine.EvaluateS(const Script: string;
  const ScriptName: RawUtf8): variant;
var
  u: RawUtf8;
begin
  StringToUtf8(Script, u);
  result := Evaluate(u, ScriptName);
end;

procedure TQuickJSEngine.GarbageCollect;
begin
  JS_RunGC(fRt);
end;

procedure TQuickJSEngine.MaybeGarbageCollect;
var
  tix: cardinal;
begin
  // QuickJS doesn't have a "maybe" GC, so run once per second
  tix := GetTickSec;
  if tix = fLastGCTickSec then
    exit;
  fLastGCTickSec := tix;
  JS_RunGC(fRt);
end;

procedure TQuickJSEngine.NewObject(out Obj: TQuickJSObject);
var
  jsobj: JSValue;
begin
  jsobj := JSValue(JS_NewObject(fCx));
  Obj.Init(self, jsobj);
end;

function TQuickJSEngine.RegisterMethod(const Obj: JSValue; const MethodName: RawUtf8;
  const Method: TQuickJSMethodVariant; ArgCount: integer): boolean;
var
  func: JSValue;
  dataVal: JSValue;
  ndx: PtrInt;
begin
  // Store method info
  ndx := fMethodCount;
  inc(fMethodCount);
  if ndx >= Length(fMethods) then
    SetLength(fMethods, NextGrow(ndx));
  fMethods[ndx] := Method;
  // Create JS function with data
  dataVal.From32(ndx); // as index, not pointer because of SetLength(fMethods)
  func := JSValue(JS_NewCFunctionData(fCx, @QuickJSMethodDataCallback,
    ArgCount, 0, 1, @dataVal));
  // Set as property on object
  result := JS_SetPropertyStr(fCx, Obj.Raw, pointer(MethodName),
    JSValueRaw(func)) >= 0;
end;

function TQuickJSEngine.AtomNew(const Name: RawUtf8): TScriptAtom;
begin
  result := {%H-}TScriptAtom(JS_NewAtom(fCx, pointer(Name)));
end;

procedure TQuickJSEngine.AtomFree(Atom: TScriptAtom);
begin
  JS_FreeAtom(fCx, {%H-}JSAtom(Atom));
end;


{ TQuickJSObject }

procedure TQuickJSObject.Init(aEngine: TQuickJSEngine; const aObj: JSValue);
begin
  fEngine := aEngine;
  fObj := aObj;
  fRooted := false;
end;

function TQuickJSObject.HasProperty(const PropName: RawUtf8): boolean;
begin
  result := JS_HasProperty(fEngine.fCx, fObj.Raw,
    {%H-}JSAtom(fEngine.AtomCacheGet(PropName))) > 0;
end;

function TQuickJSObject.HasOwnProperty(const PropName: RawUtf8): boolean;
var
  desc: JSPropertyDescriptor;
begin
  result := JS_GetOwnProperty(fEngine.fCx, @desc, fObj.Raw,
    {%H-}JSAtom(fEngine.AtomCacheGet(PropName))) > 0;
end;

function TQuickJSObject.GetPropValue(const PropName: RawUtf8): variant;
var
  val: JSValue;
begin
  val := JSValue(JS_GetPropertyStr(fEngine.fCx, fObj.Raw, pointer(PropName)));
  if not fEngine.fCx.ToVariantFree(val, result) then
    VarClear(result);
end;

procedure TQuickJSObject.SetPropValue(const PropName: RawUtf8; const Value: variant);
var
  val: JSValue;
begin
  fEngine.fCx.FromVariant(Value, val);
  JS_SetPropertyStr(fEngine.fCx, fObj.Raw, pointer(PropName), JSValueRaw(val));
end;

procedure TQuickJSObject.DefineProperty(const PropName: RawUtf8; const Value: variant);
begin
  SetPropValue(PropName, Value);
end;

function TQuickJSObject.RunMethod(const MethodName: RawUtf8;
  const Args: array of variant): variant;
begin
  result := fEngine.fCx.CallVariant(fObj, MethodName, Args);
end;

procedure TQuickJSObject.Root;
begin
  if fRooted then
    exit;
  fDupObj := fObj.Duplicate;
  fRooted := true;
end;

procedure TQuickJSObject.UnRoot;
begin
  if not fRooted then
    exit;
  fEngine.fCx.Free(fDupObj);
  fRooted := false;
end;


{ TQuickJSVariant }

procedure TQuickJSVariant.Clear(var V: TVarData);
var
  data: TQuickJSVariantData absolute V;
begin
  if (cardinal(data.VType) = cardinal(VarType)) and
     (data.Engine <> nil) and
     not data.Obj.IsUninitialized then
    data.Engine.fCx.FreeInlined(@data.Obj);
  TSynVarData(V).VType := varEmpty;
end;

procedure TQuickJSVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: boolean);
var
  src: TQuickJSVariantData absolute Source;
  dst: TQuickJSVariantData absolute Dest;
begin
  if Indirect and
     VarDataIsByRef(Source) then
    SetVariantByRef(variant(Source), variant(Dest))
  else
  begin
    VarClear(variant(Dest));
    dst.VType := VarType;
    dst.Engine := src.Engine;
    if (cardinal(src.VType) = cardinal(VarType)) and
       (src.Engine <> nil) and
       not src.Obj.IsUninitialized then
      dst.Obj := src.Obj.Duplicate // Duplicate the JSValue to protect from GC
    else
      dst.Obj := src.Obj;
  end;
end;

class procedure TQuickJSVariant.New(aEngine: TQuickJSEngine; const aObj: JSValue;
  var Result: variant);
var
  data: TQuickJSVariantData absolute Result;
begin
  VarClear(Result);
  TSynVarData(data).VType := QuickJSVariantVType;
  data.Engine := aEngine;
  data.Obj := aObj.Duplicate; // Duplicate to protect from GC
end;

function TQuickJSVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean;
var
  data: TQuickJSVariantData absolute Instance;
  propName: RawUtf8;
  val: JSValue;
  res: variant;
begin
  result := false;
  if (cardinal(data.VType) <> cardinal(VarType)) or
     (data.Engine = nil) or
     data.Obj.IsUninitialized then
    exit;
  FastSetString(propName, Name, NameLen);
  val := JSValue(JS_GetPropertyStr(data.Engine.fCx, data.Obj.Raw, pointer(propName)));
  if val.IsException then
  begin
    data.Engine.fCx.Free(val);
    exit;
  end;
  // If the result is an object, wrap it in a TQuickJSVariant
  if val.IsObject then
  begin
    TQuickJSVariant.New(data.Engine, val, res);
    data.Engine.fCx.Free(val); // New duplicates it
    TVarData(Dest) := TVarData(res);
    TSynVarData(res).VType := varEmpty; // prevent double-free
  end
  else
  begin
    if not data.Engine.fCx.ToVariantFree(val, res) then
      VarClear(res);
    TVarData(Dest) := TVarData(res);
    TSynVarData(res).VType := varEmpty;
  end;
  result := true;
end;

function TQuickJSVariant.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  data: TQuickJSVariantData absolute Instance;
  propName: RawUtf8;
  val: JSValue;
begin
  result := false;
  if (cardinal(data.VType) <> cardinal(VarType)) or
     (data.Engine = nil) or
     data.Obj.IsUninitialized then
    exit;
  FastSetString(propName, Name, NameLen); // need a local #0 ended copy
  data.Engine.fCx.FromVariant(variant(Value), val);
  result := JS_SetPropertyStr(data.Engine.fCx, data.Obj.Raw,
    pointer(propName), JSValueRaw(val)) >= 0;
end;

function TQuickJSVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): boolean;
var
  data: TQuickJSVariantData absolute V;
  funcName: RawUtf8;
  funcObj, resVal: JSValue;
  jsArgs: array of JSValue;
  i, n: PtrInt;
  res: TSynVarData;
begin
  // Ensure supplied V is a QuickJSVariant
  result := false;
  if (cardinal(data.VType) <> cardinal(VarType)) or
     (data.Engine = nil) or
     data.Obj.IsUninitialized then
    exit;
  // Get the function object
  StringToUtf8(Name, funcName);
  funcObj := JSValue(JS_GetPropertyStr(data.Engine.fCx, data.Obj.Raw,
    pointer(funcName)));
  if funcObj.IsException or
     funcObj.IsUndefined then
  begin
    data.Engine.fCx.Free(funcObj);
    exit;
  end;
  // Convert arguments to JSValues
  n := length(Arguments);
  SetLength(jsArgs, n);
  for i := 0 to n - 1 do
    data.Engine.fCx.FromVariant(variant(Arguments[i]), jsArgs[i]);
  try
    // Call the function
    resVal := JSValue(JS_Call(data.Engine.fCx, funcObj.Raw, data.Obj.Raw,
      n, pointer(jsArgs)));
    // Convert result
    if resVal.IsException then
    begin
      data.Engine.fCx.Free(resVal);
      exit;
    end;
    res.VType := varEmpty;
    if resVal.IsObject then
    begin
      TQuickJSVariant.New(data.Engine, resVal, PVariant(@res)^);
      data.Engine.fCx.Free(resVal);
    end
    else if not data.Engine.fCx.ToVariantFree(resVal, PVariant(@res)^) then
      VarClear(PVariant(@res)^);
    Dest := res.Data;
    result := true;
  finally
    // Free argument JSValues and function object
    for i := 0 to n - 1 do
      data.Engine.fCx.Free(jsArgs[i]);
    data.Engine.fCx.Free(funcObj);
  end;
end;



initialization
  QuickJSVariantType := TQuickJSVariant.Create;
  QuickJSVariantVType := QuickJSVariantType.VarType;

finalization
  FreeAndNil(QuickJSVariantType);


{$else}

implementation // compiles as a void unit if QuickJS is not supported

{$endif LIBQUICKJS}

end.
