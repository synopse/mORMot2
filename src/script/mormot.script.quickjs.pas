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

  /// callback signature for native method registration (JSON-based)
  TQuickJSMethodJSON = function(const This: TQuickJSEngine;
    const ArgsJson: RawUtf8; var ResultJson: RawUtf8): boolean of object;

  /// internal record to store registered method info
  TQuickJSMethodInfo = record
    Engine: TQuickJSEngine;
    Callback: TQuickJSMethodVariant;
    ArgCount: integer;
  end;
  PQuickJSMethodInfo = ^TQuickJSMethodInfo;
  TQuickJSMethodInfoDynArray = array of TQuickJSMethodInfo;

  /// storage for TQuickJSVariant custom variant type
  // - will store a reference to a JavaScript object for late-binding access
  TQuickJSVariantData = packed record
    /// the custom variant type registered number
    VType: TVarType;
    VFiller: array[0..SizeOf(TVarData) - SizeOf(TVarType)
      - SizeOf(TQuickJSEngine) - SizeOf(JSValue) - 1] of byte;
    /// the associated QuickJS engine
    VEngine: TQuickJSEngine;
    /// the JavaScript object value (duplicated/rooted for GC protection)
    VObj: JSValue;
  end;
  PQuickJSVariantData = ^TQuickJSVariantData;

  /// wrapper for JavaScript object operations
  {$ifdef USERECORDWITHMETHODS}
  TQuickJSObject = record
  {$else}
  TQuickJSObject = object
  {$endif USERECORDWITHMETHODS}
  private
    fEngine: TQuickJSEngine;
    fObj: JSValue;
    fRooted: boolean;
    fDupObj: JSValue;
  public
    /// initialize the wrapper
    procedure Init(aEngine: TQuickJSEngine; aObj: JSValue);
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
    /// clear last error state
    procedure ClearLastError;
  public
    /// initialize the QuickJS engine
    // - called within TThreadSafeManager lock
    constructor Create(aManager: TThreadSafeManager; aThreadData: pointer;
      aTag: PtrInt; aThreadID: TThreadID); override;
    /// finalize the QuickJS engine
    destructor Destroy; override;
    /// initialize engine after construction (outside manager lock)
    procedure AfterCreate; override;
    /// cleanup before destruction (outside manager lock)
    procedure BeforeDestroy; override;
    /// retrieve engine from context
    class function From(aContext: TScriptContext): TThreadSafeEngine; override;
    /// evaluate JavaScript code and return result as variant
    function Evaluate(const Script: RawUtf8;
      const ScriptName: RawUtf8 = ''): variant;
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
    function RegisterMethod(Obj: JSValue; const MethodName: RawUtf8;
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

  /// custom variant type for late-binding JavaScript object access
  // - allows Pascal code to access JS object properties using variant syntax
  // - example: jsObj.propName or jsObj.method(arg1, arg2)
  TQuickJSVariant = class(TSynInvokeableVariantType)
  protected
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
    /// initialize the custom variant type
    constructor Create; override;
    /// properly release the JavaScript object reference
    procedure Clear(var V: TVarData); override;
    /// copy variant data (duplicates the JS object reference)
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: boolean); override;
    /// create a variant containing a JavaScript object
    class procedure New(aEngine: TQuickJSEngine; aObj: JSValue;
      out Result: variant);
  end;

var
  /// the global TQuickJSVariant custom variant type instance
  QuickJSVariantType: TQuickJSVariant;


{$endif LIBQUICKJS}

implementation

{$ifdef LIBQUICKJS}

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
  infoPtr: PQuickJSMethodInfo;
  engine: TQuickJSEngine;
  args: array of variant;
  thisVar, res: variant;
  i: integer;
  jsres: JSValue;
begin
  engine := TQuickJSEngine(JS_GetContextOpaque(ctx));
  // Get info pointer from func_data[0]
  infoPtr := PQuickJSMethodInfo(JSValue(func_data^).Ptr);
  try
    // Convert 'this' to variant
    engine.fCx.ToVariant(JSValue(this_val), thisVar);
    // Convert arguments
    SetLength(args, argc);
    for i := 0 to argc - 1 do
      engine.fCx.ToVariant(JSValue(argv[i]), args[i]);
    // Call the Delphi method
    res := infoPtr^.Callback(thisVar, args);
    // Convert result back to JSValue
    engine.fCx.FromVariant(res, jsres);
    result := JSValueRaw(jsres);
  except
    on E: Exception do
    begin
      JS_ThrowInternalError(ctx, PAnsiChar(StringToUtf8(E.Message)));
      result := JS_EXCEPTION;
    end;
  end;
end;


{ TQuickJSEngine }

constructor TQuickJSEngine.Create(aManager: TThreadSafeManager;
  aThreadData: pointer; aTag: PtrInt; aThreadID: TThreadID);
begin
  inherited Create(aManager, aThreadData, aTag, aThreadID);
  // Runtime and Context creation deferred to AfterCreate
end;

destructor TQuickJSEngine.Destroy;
begin
  inherited Destroy;
end;

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
    fCx.FreeInlined(fGlobalObj);
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
end;

function TQuickJSEngine.Evaluate(const Script, ScriptName: RawUtf8): variant;
var
  res: JSValue;
begin
  ClearLastError;
  // Reset timeout tracking
  fTimeoutAborted := false;
  if fTimeoutValue <> 0 then
    fTimeoutStartTickSec := GetTickSec;
  // Execute the supplied Script
  res := fCx.Eval(Script, Script, JS_EVAL_TYPE_GLOBAL, fLastErrorMsg);
  if res.IsException then
  begin
    fErrorExist := true;
    if fTimeoutAborted then
      fLastErrorMsg := 'Script execution timeout';
    fCx.FreeInlined(res);
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

function TQuickJSEngine.RegisterMethod(Obj: JSValue; const MethodName: RawUtf8;
  const Method: TQuickJSMethodVariant; ArgCount: integer): boolean;
var
  func: JSValue;
  dataVal: JSValue;
  idx: integer;
begin
  // Store method info
  idx := fMethodCount;
  Inc(fMethodCount);
  if idx >= Length(fMethods) then
    SetLength(fMethods, idx + 16);
  fMethods[idx].Engine := self;
  fMethods[idx].Callback := Method;
  fMethods[idx].ArgCount := ArgCount;
  // Create data value containing pointer to our info
  dataVal.Fill(JS_TAG_INT, PtrInt(@fMethods[idx]));
  // Create JS function with data
  func := JSValue(JS_NewCFunctionData(fCx, @QuickJSMethodDataCallback,
    ArgCount, 0, 1, @dataVal));
  // Set as property on object
  result := JS_SetPropertyStr(fCx, Obj.Raw, pointer(MethodName),
    JSValueRaw(func)) >= 0;
end;


{ TQuickJSObject }

procedure TQuickJSObject.Init(aEngine: TQuickJSEngine; aObj: JSValue);
begin
  fEngine := aEngine;
  fObj := aObj;
  fRooted := false;
end;

function TQuickJSObject.HasProperty(const PropName: RawUtf8): boolean;
var
  atom: JSAtom;
begin
  atom := JS_NewAtom(fEngine.fCx, pointer(PropName));
  result := JS_HasProperty(fEngine.fCx, fObj.Raw, atom) > 0;
  JS_FreeAtom(fEngine.fCx, atom);
end;

function TQuickJSObject.HasOwnProperty(const PropName: RawUtf8): boolean;
var
  desc: JSPropertyDescriptor;
  atom: JSAtom;
begin
  atom := JS_NewAtom(fEngine.fCx, pointer(PropName));
  result := JS_GetOwnProperty(fEngine.fCx, @desc, fObj.Raw, atom) > 0;
  JS_FreeAtom(fEngine.fCx, atom);
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
  if not fRooted then
  begin
    fDupObj := fObj.Duplicate;
    fRooted := true;
  end;
end;

procedure TQuickJSObject.UnRoot;
begin
  if fRooted then
  begin
    fEngine.fCx.FreeInlined(fDupObj);
    fRooted := false;
  end;
end;


{ TQuickJSVariant }

constructor TQuickJSVariant.Create;
begin
  inherited Create;
end;

procedure TQuickJSVariant.Clear(var V: TVarData);
var
  data: PQuickJSVariantData;
begin
  data := PQuickJSVariantData(@V);
  if (data^.VEngine <> nil) and not data^.VObj.IsUninitialized then
    data^.VEngine.fCx.FreeInlined(data^.VObj);
  V.VType := varEmpty;
end;

procedure TQuickJSVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: boolean);
var
  src: PQuickJSVariantData;
  dst: PQuickJSVariantData;
begin
  if Indirect and VarDataIsByRef(Source) then
    SetVariantByRef(variant(Source), variant(Dest))
  else
  begin
    VarClear(variant(Dest));
    src := PQuickJSVariantData(@Source);
    dst := PQuickJSVariantData(@Dest);
    dst^.VType := VarType;
    dst^.VEngine := src^.VEngine;
    // Duplicate the JSValue to protect from GC
    if (src^.VEngine <> nil) and not src^.VObj.IsUninitialized then
      dst^.VObj := src^.VObj.Duplicate
    else
      dst^.VObj := src^.VObj;
  end;
end;

class procedure TQuickJSVariant.New(aEngine: TQuickJSEngine; aObj: JSValue;
  out Result: variant);
var
  data: PQuickJSVariantData;
begin
  VarClear(Result);
  data := PQuickJSVariantData(@Result);
  data^.VType := QuickJSVariantType.VarType;
  data^.VEngine := aEngine;
  // Duplicate to protect from GC
  data^.VObj := aObj.Duplicate;
end;

function TQuickJSVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt; NoException: boolean): boolean;
var
  data: PQuickJSVariantData;
  propName: RawUtf8;
  val: JSValue;
  res: variant;
begin
  data := PQuickJSVariantData(@Instance);
  if (data^.VEngine = nil) or data^.VObj.IsUninitialized then
  begin
    result := false;
    exit;
  end;
  FastSetString(propName, Name, NameLen);
  val := JSValue(JS_GetPropertyStr(data^.VEngine.fCx, data^.VObj.Raw,
    pointer(propName)));
  if val.IsException then
  begin
    data^.VEngine.fCx.FreeInlined(val);
    result := false;
    exit;
  end;
  // If the result is an object, wrap it in a TQuickJSVariant
  if val.IsObject then
  begin
    TQuickJSVariant.New(data^.VEngine, val, res);
    data^.VEngine.fCx.FreeInlined(val); // New duplicates it
    TVarData(Dest) := TVarData(res);
    TVarData(res).VType := varEmpty; // prevent double-free
  end
  else
  begin
    if not data^.VEngine.fCx.ToVariantFree(val, res) then
      VarClear(res);
    TVarData(Dest) := TVarData(res);
    TVarData(res).VType := varEmpty;
  end;
  result := true;
end;

function TQuickJSVariant.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var
  data: PQuickJSVariantData;
  propName: RawUtf8;
  val: JSValue;
begin
  data := PQuickJSVariantData(@Instance);
  if (data^.VEngine = nil) or data^.VObj.IsUninitialized then
  begin
    result := false;
    exit;
  end;
  FastSetString(propName, Name, NameLen);
  data^.VEngine.fCx.FromVariant(variant(Value), val);
  result := JS_SetPropertyStr(data^.VEngine.fCx, data^.VObj.Raw,
    pointer(propName), JSValueRaw(val)) >= 0;
end;

function TQuickJSVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): boolean;
var
  data: PQuickJSVariantData;
  funcName: RawUtf8;
  funcObj, resVal: JSValue;
  jsArgs: array of JSValue;
  i, n: PtrInt;
  res: variant;
begin
  result := false;
  data := PQuickJSVariantData(@V);
  if (data^.VEngine = nil) or data^.VObj.IsUninitialized then
    exit;
  // Get the function object
  StringToUtf8(Name, funcName);
  funcObj := JSValue(JS_GetPropertyStr(data^.VEngine.fCx, data^.VObj.Raw,
    pointer(funcName)));
  if funcObj.IsException or funcObj.IsUndefined then
  begin
    data^.VEngine.fCx.FreeInlined(funcObj);
    exit;
  end;
  // Convert arguments to JSValues
  n := length(Arguments);
  SetLength(jsArgs, n);
  for i := 0 to n - 1 do
    data^.VEngine.fCx.FromVariant(variant(Arguments[i]), jsArgs[i]);
  try
    // Call the function
    resVal := JSValue(JS_Call(data^.VEngine.fCx, funcObj.Raw, data^.VObj.Raw,
      n, pointer(jsArgs)));
    // Convert result
    if resVal.IsException then
    begin
      data^.VEngine.fCx.FreeInlined(resVal);
      exit;
    end;
    if resVal.IsObject then
    begin
      TQuickJSVariant.New(data^.VEngine, resVal, res);
      data^.VEngine.fCx.FreeInlined(resVal);
    end
    else
    begin
      if not data^.VEngine.fCx.ToVariantFree(resVal, res) then
        VarClear(res);
    end;
    TVarData(Dest) := TVarData(res);
    TVarData(res).VType := varEmpty;
    result := true;
  finally
    // Free argument JSValues and function object
    for i := 0 to n - 1 do
      data^.VEngine.fCx.FreeInlined(jsArgs[i]);
    data^.VEngine.fCx.FreeInlined(funcObj);
  end;
end;


{ TQuickJSEngine - Timeout support }

procedure TQuickJSEngine.SetTimeoutValue(const Value: double);
begin
  fTimeoutValue := Value;
end;


{$endif LIBQUICKJS}

initialization
  {$ifdef LIBQUICKJS}
  QuickJSVariantType := TQuickJSVariant.Create;
  {$endif LIBQUICKJS}

finalization
  {$ifdef LIBQUICKJS}
  FreeAndNil(QuickJSVariantType);
  {$endif LIBQUICKJS}

end.
