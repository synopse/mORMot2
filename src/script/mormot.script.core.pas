/// Scripting Integration to our Framework
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.script.core;

{
  *****************************************************************************

   Abstract Types for Generic Scripting Integration
    - Generic Execution Engine Parent class
    - 

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  math,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.lib.static;


{ ******************* }

type
  /// exception class raised from scripting fatal issues
  EScriptException = class(ESynException);

  TThreadSafeManager = class;
  TThreadSafeEngine = class;

  /// prototype of Script engines notification callback method
  // - execution is embraced with DoBeginRequest/DoEndRequest by using
  // ThreadSafeCall() - unless notified otherwise
  TEngineEvent = procedure(Engine: TThreadSafeEngine) of object;

  /// callback returning a text value
  // - used e.g. for getting engine name or web app root path
  TEngineNameEvent = function(Engine: TThreadSafeEngine): RawUtf8 of object;

  /// used internally by TThreadSafeEngine.AtomCacheFind/AtomCacheAdd methods
  // - using a pointer will do the trick for all known script API
  // - may be a SM jsid or a ChakraCore JsPropertyIdRef
  // - not needed by QuickJS
  TScriptAtom = pointer;

  /// opaque handle mapping a reference to a native (Java)Script runtime
  // - using a pointer will do the trick for all known script APIs
  TScriptRuntime = pointer;

  /// opaque handle mapping a reference to a (Java)Script execution context
  // - using a pointer will do the trick for all known script APIs
  TScriptContext = pointer;

  /// implements a remote debugger process for TThreadSafeEngine/TThreadSafeManager
  IRemoteDebugger = interface
    ['{3265C1FA-AA4B-42A8-99C6-E0F08D128684}']
    procedure StartDebugCurrentThread(Engine: TThreadSafeEngine);
    procedure StopDebugCurrentThread(Engine: TThreadSafeEngine);
    function GetNeedPauseOnFirstStep: boolean;
    procedure SetNeedPauseOnFirstStep(Value: boolean);
    procedure DoLog(const Text: RawUtf8);
  end;

  /// implements workers process for TThreadSafeEngine/TThreadSafeManager
  IWorkerManager = interface
    ['{DF03FA4D-7789-4346-A506-B302E922169D}']
    function Count: integer;
    function IsCurrentThreadWorker: boolean;
    function GetInteruptInOwnThreadhandlerForCurThread: TThreadMethod;
    procedure GetCurrentWorkerThreadName(var Name: RawUtf8);
  end;

  // note: current doc about FireFox remote protocol (and stream format) is
  // https://firefox-source-docs.mozilla.org/devtools/backend/protocol.html

  /// abstract parent class implementing a ThreadSafe (Java)Script engine
  // - use TThreadSafeManager.ThreadSafeEngine to retrieve the Engine instance
  // corresponding to the current thread, for thread-safety
  TThreadSafeEngine = class(TSynPersistent)
  protected
    fThreadID: TThreadID;
    fRuntime: TScriptRuntime;
    fContext: TScriptContext;
    fManager: TThreadSafeManager;
    fThreadData: pointer;
    fContentVersion: cardinal;
    fCreateTix: Int64;
    fTag: PtrInt;
    fPrivateDataForDebugger: pointer;
    fNeverExpire: boolean;
    fNameForDebug, fWebAppRootDir: RawUtf8;
    fAtomCache: TRawUtf8List; // hashed list of objects=TScriptAtom
    fDoInteruptInOwnThread: TThreadMethod;
    fRequestFpuBackup: array[0..3] of cardinal;
    function AtomCacheFind(const Name: RawUtf8): TScriptAtom; // nil = not found
      {$ifdef HASINLINE} inline; {$endif}
    procedure AtomCacheAdd(const Name: RawUtf8; Atom: TScriptAtom);
    { following methods should be overriden with the proper scripting API }
    // from ThreadSafeCall(): FPU mask + multi-threading (SM)
    procedure DoBeginRequest; virtual;
    procedure DoEndRequest; virtual;
  public
    /// create one thread-safe (Java) Script Engine instance
    // - inherited classes should initialize internal script Runtime, Context,
    // global objects and standard classes
    // - do not create Engine directly via this constructor, but instead call
    // TThreadSafeManager.ThreadSafeEngine
    // - this constructor should be called within the thread associated with
    // this engine instance
    // - this method is called within the TThreadSafeManager lock, so engine
    // specific initialization should be overriden in AfterCreate method
    constructor Create(aManager: TThreadSafeManager; aThreadData: pointer;
      aTag: PtrInt; aThreadID: TThreadID); reintroduce; virtual;
    /// finalize the Script engine instance
    destructor Destroy; override;
    /// should be overriden to initialize a newly created engine
    // - this method is called outside the TThreadSafeManager lock
    procedure AfterCreate; virtual; abstract;
    /// should be overriden to finialize an engine
    // - this method is called outside the TThreadSafeManager lock
    procedure BeforeDestroy; virtual; abstract;
    /// embrace Event(self) with DoBeginRequest/DoEndRequest protected methods
    procedure ThreadSafeCall(const Event: TEngineEvent); virtual;

    /// should be able to retrieve the Engine from a given execution context
    // - call e.g. JS_GetContextPrivate(aContext) on SpiderMonkey or
    // JS_GetContextOpaque() on QuickJS
    class function From(aContext: TScriptContext): TThreadSafeEngine;
      virtual; abstract;

    /// opaque handle mapping the associated native (Java)Script runtime
    // - using a pointer will do the trick for all known script APIs
    property Runtime: TScriptRuntime
      read fRuntime;
    /// opaque handle mapping the associated native (Java)Script context
    // - using a pointer will do the trick for all known script APIs
    property Context: TScriptContext
      read fContext;
    /// reference to the TThreadSafeManager instance owning this Engine
    property Manager: TThreadSafeManager
      read fManager;
    /// can hold any engine-specific data
    // - can be specified as second arg to TThreadSafeManager.ThreadSafeEngine
    property Tag: PtrInt
      read fTag write fTag;
    /// thread specific data
    // - pointer to any structure, passed into ThreadSafeEngine call
    // - used to access a thread details in the native functions e.g.
    // as TThreadSafeEngine(cx.PrivateData).ThreadData
    property ThreadData: pointer
      read fThreadData write fThreadData;
    /// the associated thread ID, as retrieved during intiialization
    // - could be accessed from other threads, e.g. for logging/debugging
    property ThreadID: TThreadID
      read fThreadID;
    /// incremental sequence number of engine scripts
    // - used in TThreadSafeManager.ThreadSafeEngine to determine if context is
    // up to date, in order to trigger on-the-fly reload of scripts without the
    // need of restarting the application/service
    // - caller must change this parameter value e.g. in case of changes in
    // the scripts folder in an HTTP server
    property ContentVersion: cardinal
      read fContentVersion;
    /// used during debugging
    property PrivateDataForDebugger: pointer
      read fPrivateDataForDebugger write fPrivateDataForDebugger;
    /// root path for the current Web Application engine context
    // - as set by TThreadSafeManager.OnGetWebAppRootPath
    property WebAppRootDir: RawUtf8
      read fWebAppRootDir write fWebAppRootDir;
    /// can be set so that this engine will never expire
    property NeverExpire: boolean
      read fNeverExpire write fNeverExpire;
    /// low-level handler for the debugger
    // - is called from debugger thread when the debugger requires it
    // - this method must wake up the engine's thread and thread must
    // execute rt.InterruptCallback(cx) for this engine
    property DoInteruptInOwnThread: TThreadMethod
      read fDoInteruptInOwnThread write fDoInteruptInOwnThread;
  published
    /// name of this engine thread
    // - as shown in the debugger to ease context identification
    // - default is 'ThreadIdHex ThreadName' unless TThreadSafeManager.OnGetName
    // or IWorkerManager. override it
    property NameForDebug: RawUtf8
      read fNameForDebug write fNameForDebug;
  end;

  /// meta-class of our thread-safe engines
  TThreadSafeEngineClass = class of TThreadSafeEngine;

  /// abstract parent class mananing a list of a per-Thread (Java)Script engines
  // - one TThreadSafeEngine will be maintained per thread
  // - never use this abstract class, but inherited implementations
  TThreadSafeManager = class(TSynPersistent)
  protected
    fContentVersion: cardinal;
    fOnNewEngine: TEngineEvent;
    fOnDebuggerInit: TEngineEvent;
    fOnGetName: TEngineNameEvent;
    fOnGetWebAppRootPath: TEngineNameEvent;
    fOnDebuggerConnected: TEngineEvent;
    fEngineClass: TThreadSafeEngineClass;
    fEngineExpireTimeOutTix: Int64;
    fEngines: TSynObjectListLightLocked; // of TThreadSafeEngine
    fEngineID: TThreadIDDynArray;
    fMaxEngines: integer;
    fDebugMainThread: boolean;
    fMainEngine: TThreadSafeEngine;
    fRemoteDebugger: IRemoteDebugger;
    fWorkerManager: IWorkerManager;
    fOnLog: TSynLogProc;
    function ThreadEngineIndex(aThreadID: TThreadID): PtrInt;
      {$ifdef HASINLINE} inline; {$endif}
    function GetPauseDebuggerOnFirstStep: boolean;
    procedure SetPauseDebuggerOnFirstStep(aPauseDebuggerOnFirstStep: boolean);
    function GetEngineExpireTimeOutMinutes: cardinal;
    procedure SetEngineExpireTimeOutMinutes(Value: cardinal);
    function NewDebugger(const port: RawUtF8): IRemoteDebugger; virtual;
    function NewWorkerManager: IWorkerManager; virtual;
  public
    /// initialize the scripting manager
    // - aMaxPerThreadEngines is the initial MaxEngines limit property
    constructor Create(aEngineClass: TThreadSafeEngineClass;
      aOnLog: TSynLogProc = nil; aMaxPerThreadEngines: integer = 128); reintroduce;
    /// finalize the scripting manager
    destructor Destroy; override;
    /// get or create one Engine associated with current running thread
    // - aThreadData is a pointer to any structure relative to this thread
    // accessible via TThreadSafeEngine.ThreadData
    function ThreadSafeEngine(ThreadData: pointer = nil;
      TagForNewEngine: PtrInt = 0): TThreadSafeEngine;
    /// retrieve the Engine associated with this Thread ID
    // - may be MainEngine or one of the previously created ThreadSafeEngine()
    // - return nil if this thread is unknown
    // - warning: call Engines.Safe.Lock/Unlock if the instance can expire
    function Engine(aThreadID: TThreadID): TThreadSafeEngine; overload;
    /// should be able to retrieve the Engine from a given context
    // - redirect to fEngineClass.From()
    function Engine(aContext: TScriptContext): TThreadSafeEngine; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// initialize a new engine to be used outside of our engine pool
    // - the returned engine won't be owned by this class, so is to be released
    // explicitly by the caller
    // - this engine won't be registered to the debugger
    function NewEngine: TThreadSafeEngine;
    /// returns how many times the NewEngine method has been called
    function NewEngineCount: integer;
    /// setup and create the main engine associated with the pools
    // - should be called once at startup from the main thread
    // - this engine won't be part of the internal ThreadSafeEngine() pool
    // - raise an Exception if called twice
    function InitializeMainEngine: TThreadSafeEngine;
    /// the main engine, as setup by InitializeMainEngine
    // - is nil if there is no such main engine but only ThreadSafeEngine()
    property MainEngine: TThreadSafeEngine
      read fMainEngine;
    /// low-level access to the per-thread TThreadSafeEngine internal pool
    property Engines: TSynObjectListLightLocked
      read fEngines;
    /// specify a maximum lifetime period after which script engines will
    // be recreated, to avoid potential JavaScript memory leak (variables in global,
    // closures circle, etc.)
    // - 0 by default - mean no expiration timeout
    // - a typical value for a production server is 4*60, i.e. 4 hours
    // - in case a specific engine must never expire, set its NeverExpire property
    property EngineExpireTimeOutMinutes: cardinal
      read GetEngineExpireTimeOutMinutes write SetEngineExpireTimeOutMinutes default 0;

    /// start the debugger to listen on a selected port
    // - expects the port to be specified as string, e.g. '1234'
    // - you can optionally specify a server address to bind to, e.g.
    // '1.2.3.4:1234'
    // - debugger create a dedicated thread where it listens to a requests
    // from a remote debug UI
    procedure StartDebugger(const Port: RawUtf8 = '6000';
      ForMainThread: boolean = false);
    /// stop the remote debugger server
    procedure StopDebugger;
    /// Write text as console log to current thread Engine's debugger (if exists)
    procedure DebuggerLog(const Text: RawUtf8);
    /// redirect to the associated debugger thread flag if the debugger
    // connected to new engine must pause on first step
    property PauseDebuggerOnFirstStep: boolean
      read GetPauseDebuggerOnFirstStep write SetPauseDebuggerOnFirstStep;
    /// delete all started worker threads
    procedure ClearWorkers; virtual;

    /// the associated (Java)Script engine class
    property EngineClass: TThreadSafeEngineClass
      read fEngineClass;
    /// incremental sequence number of engine scripts
    // - used in ThreadSafeEngine to determine if context is up to date,
    // in order to trigger on-the-fly reload of scripts without the
    // need of restarting the application/service
    // - caller must change this parameter value e.g. in case of changes in
    // the scripts folder in an HTTP server
    // - warning: this is not to be used in production, because it may trigger
    // some tricky issues, but could useful when debugging/exploring
    property ContentVersion: cardinal
      read fContentVersion write fContentVersion;
    /// hardcore limit of internal per-thread Script engine instances count
    // - is set to 128 by default, which is above any usable thread pool
    // - over this limit, a EScriptException is raised by ThreadSafeEngine(),
    // to indicate potential broken thread creation logic of your application
    property MaxEngines: integer
      read fMaxEngines write fMaxEngines;
    /// low-level access to the associated remote debugger
    property RemoteDebugger: IRemoteDebugger
      read fRemoteDebugger;
    /// low-level access to the associated workers threads
    property WorkerManager: IWorkerManager
      read fWorkerManager;
    /// may redirect to TSynLog.DoLog for logging
    property OnLog: TSynLogProc
      read fOnLog write fOnLog;
    /// event triggered every time a new TThreadSafeEngine is created
    // - event triggered before OnDebuggerInit and OnNewEngine events
    // - result of this method is Engine's name for debugging
    property OnGetName: TEngineNameEvent
      read fOnGetName write fOnGetName;
    /// event triggered every time a new TThreadSafeEngine is created
    // - event triggered before OnDebuggerInit and OnNewEngine events
    // - result of this method is Engine's web app root path used for Debugger
    property OnGetWebAppRootPath: TEngineNameEvent
      read fOnGetWebAppRootPath write fOnGetWebAppRootPath;
    /// event triggered when an internal debugger process connects to Engine
    // - event triggered in debuggers compartment
    // - here your code can change the initial state of the debugger
    property OnDebuggerInit: TEngineEvent
      read fOnDebuggerInit write fOnDebuggerInit;
    /// event triggered every time a new Engine is created
    // - here your code can change the initial state of the Engine
    property OnNewEngine: TEngineEvent
      read fOnNewEngine write fOnNewEngine;
    /// event triggered every time a new remote debugger connect to an Engine
    // - Warning: this callback is triggered in debuggers communication thread,
    // not the Engine thread, so you should NOT use Engine's Javascript in
    // this method, but only Engine's properties
    property OnDebuggerConnected: TEngineEvent
      read fOnDebuggerConnected write fOnDebuggerConnected;
  end;



implementation


{ TThreadSafeManager }

constructor TThreadSafeManager.Create(aEngineClass: TThreadSafeEngineClass;
  aOnLog: TSynLogProc; aMaxPerThreadEngines: integer);
begin
  inherited Create; // may have been overriden
  fEngineClass := aEngineClass;
  fEngines := TSynObjectListLightLocked.Create;
  fMaxEngines := aMaxPerThreadEngines;
  fOnLog := aOnLog;
  if Assigned(fOnLog) then
    fOnLog(sllInfo, 'Create(%,maxengines=%)', [fEngineClass, fMaxEngines], self);
  fWorkerManager := NewWorkerManager;
end;

destructor TThreadSafeManager.Destroy;
var
  endtix: Int64;
begin
  if Assigned(fOnLog) then
    fOnLog(sllInfo, 'Destroy: %=%', [fEngineClass, fEngines.Count], self);
  fWorkerManager := nil;
  StopDebugger;
  endtix := GetTickCount64 + 10000;
  while (fEngines.Count > 0) and
        (GetTickCount64 < endtix) do
    SleepHiRes(50);
  if fEngines.Count>0 then
    raise EScriptException.CreateUtf8(
      '%.Destroy: here are % unreleased engines', [self, fEngines.Count]);
  FreeAndNil(fMainEngine);
  inherited Destroy;
  fEngines.Free;
end;

{$ifdef THREADID32}
function TThreadSafeManager.ThreadEngineIndex(aThreadID: TThreadID): PtrInt;
begin // use SSE2 on i386/x86_64
  result := IntegerScanIndex(pointer(fEngineID), fEngines.Count, cardinal(aThreadID));
end;
{$else}
function TThreadSafeManager.ThreadEngineIndex(aThreadID: TThreadID): PtrInt;
var
  e: ^TThreadID;
begin
  // caller made fEngines.Safe.ReadLock/WriteLock
  e := pointer(fEngineID);
  for result := 0 to fEngines.Count - 1 do
    // brute force search in L1 cache is fast enough since fMaxEngines is small
    if e^ = aThreadID then
      exit
    else
      inc(e);
  result := -1;
end;
{$endif CPU32}

function TThreadSafeManager.ThreadSafeEngine(ThreadData: pointer;
  TagForNewEngine: PtrInt): TThreadSafeEngine;
var
  tid: TThreadID;
  existing, i: PtrInt;
  tobereleased: TThreadSafeEngine;
begin
  // retrieve or (re)create the engine associated with this thread
  result := nil;
  tid := GetCurrentThreadId;
  fEngines.Safe.ReadLock; // no try..finally for exception-safe code
  existing := ThreadEngineIndex(tid);
  if existing >= 0 then
  begin
    result := fEngines.List[existing];
    if ThreadData = nil then
      ThreadData := result.fThreadData  // to be used if recreated
    else
      result.fThreadData := ThreadData; // override with the given parameter
    if result.NeverExpire or
       (fEngineExpireTimeOutTix = 0) or
       (GetTickCount64 - result.fCreateTix < fEngineExpireTimeOutTix) then
      if result.fContentVersion = fContentVersion then
      begin
        // we got the right engine -> quickly return
        fEngines.Safe.ReadUnLock;
        exit;
      end;
  end;
  fEngines.Safe.ReadUnLock;
  tobereleased := nil;
  fEngines.Safe.WriteLock;
  try // some exceptions may occur from now on
    if (existing > fEngines.Count) or
       (fEngines.List[existing] <> result) then
      existing := ThreadEngineIndex(tid); // paranoid
    if existing >= 0 then
    begin
      // the engine is expired or its content changed -> remove and recreate
      tobereleased := fEngines.List[existing];
      if Assigned(fOnLog) then
        fOnLog(sllInfo, 'ThreadSafeEngine: expired %', [result], self);
      fEngines.Delete(existing, {donotfree=}true);
    end;
    // if we reached here, we need a new TThreadSafeEngine instance
    if fEngines.Count >= fMaxEngines then
      raise EScriptException.CreateUtf8(
        '%.ThreadSafeEngine reached its limit of % engines on %',
        [self, fMaxEngines, GetCurrentThreadInfo]);
    if Assigned(fOnLog) then
      fOnLog(sllTrace, 'ThreadSafeEngine: new engine needed - count=%',
        [fEngines.Count], self);
    result := fEngineClass.Create(self, ThreadData, TagForNewEngine, tid);
    fEngines.Add(result);
  finally
    // always populate the L1-cache-friendly array of all ThreadID
    if length(fEngineID) < fEngines.Count then
      SetLength(fEngineID, fMaxEngines);
    for i := 0 to fEngines.Count - 1 do
      fEngineID[i] := TThreadSafeEngine(fEngines.List[i]).ThreadID;
    // now we don't need to access fEngines anymore
    fEngines.Safe.WriteUnLock;
    // released outside the lock - garbage collection may take some time
    tobereleased.Free;
  end;
  // initialize the newly (re)created engine outside the lock
  result.AfterCreate;
  if Assigned(fRemoteDebugger) and
     not fDebugMainThread then
    fRemoteDebugger.StartDebugCurrentThread(result);
  if Assigned(fWorkerManager) and
     fWorkerManager.IsCurrentThreadWorker then
  begin
    fWorkerManager.GetCurrentWorkerThreadName(result.fNameForDebug);
    result.fDoInteruptInOwnThread :=
      fWorkerManager.GetInteruptInOwnThreadhandlerForCurThread;
  end;
  if Assigned(fOnNewEngine) then
    result.ThreadSafeCall(fOnNewEngine);
  if Assigned(fOnLog) then
    fOnLog(sllInfo, 'ThreadSafeEngine: created %', [result], self);
end;

function TThreadSafeManager.Engine(aThreadID: TThreadID): TThreadSafeEngine;
var
  i: PtrInt;
begin
  result := fMainEngine;
  if (result <> nil) and
     (result.ThreadID = aThreadID) then
    exit;
  result := nil;
  if PtrUInt(aThreadID) = 0 then
    exit;
  fEngines.Safe.ReadLock;
  try
    i := ThreadEngineIndex(aThreadID);
    if i >= 0 then
      result := fEngines.List[i];
  finally
    fEngines.Safe.ReadUnLock;
  end;
end;

function TThreadSafeManager.Engine(aContext: TScriptContext): TThreadSafeEngine;
begin
  if self = nil then
    result := nil
  else
    result := fEngineClass.From(aContext);
end;

function TThreadSafeManager.InitializeMainEngine: TThreadSafeEngine;
var
  tid: TThreadID;
begin
  tid := GetCurrentThreadId;
  result := fMainEngine;
  if result = nil then
  begin
    result := fEngineClass.Create(self, nil, 0, tid);
    fMainEngine := result;
    result.fNameForDebug := 'Main';
    result.fNeverExpire := true; // not in the pool, anyway
    result.AfterCreate;
    if Assigned(fRemoteDebugger) and
       fDebugMainThread then
      fRemoteDebugger.StartDebugCurrentThread(result);
    if Assigned(fOnNewEngine) then
      result.ThreadSafeCall(fOnNewEngine);
    if Assigned(fOnLog) then
      fOnLog(sllInfo, 'ThreadSafeEngine: created %', [result], self);
  end
  else if result.ThreadID <> tid then
    raise EScriptException.CreateUtf8('Invalid %.InitializeMainEngine', [self]);
end;

var
  NewEngineSequence: integer;

function TThreadSafeManager.NewEngine: TThreadSafeEngine;
begin
  result := fEngineClass.Create(nil, nil, 0, TThreadID(0));
  FormatUtf8('NewEngine%', [InterlockedIncrement(NewEngineSequence)],
    result.fNameForDebug);
  result.fNeverExpire := true; // not in the pool, anyway
  result.AfterCreate;
  if Assigned(fOnNewEngine) then
    result.ThreadSafeCall(fOnNewEngine);
end;

function TThreadSafeManager.NewEngineCount: integer;
begin
  result := NewEngineSequence;
end;

function TThreadSafeManager.GetEngineExpireTimeOutMinutes: cardinal;
begin
  result := fEngineExpireTimeOutTix div 60000;
end;

procedure TThreadSafeManager.SetEngineExpireTimeOutMinutes(Value: cardinal);
begin
  fEngineExpireTimeOutTix := Value * 60000;
end;

function TThreadSafeManager.NewDebugger(const port: RawUtF8): IRemoteDebugger;
begin
  result := nil;
end;

function TThreadSafeManager.NewWorkerManager: IWorkerManager;
begin
  result := nil;
end;

function TThreadSafeManager.GetPauseDebuggerOnFirstStep: boolean;
begin
  if Assigned(fRemoteDebugger) then
    result := fRemoteDebugger.GetNeedPauseOnFirstStep
  else
    result := false;
end;

procedure TThreadSafeManager.SetPauseDebuggerOnFirstStep(
  aPauseDebuggerOnFirstStep: boolean);
begin
  if Assigned(fRemoteDebugger) then
    fRemoteDebugger.SetNeedPauseOnFirstStep(aPauseDebuggerOnFirstStep);
end;

procedure TThreadSafeManager.StartDebugger(const Port: RawUtf8;
  ForMainThread: boolean);
begin
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'StartDebugger(%,%)', [Port, ForMainThread], self);
  fDebugMainThread := ForMainThread;
  fRemoteDebugger := NewDebugger(Port);
  if Assigned(fRemoteDebugger) then
    inc(fContentVersion); // force recreate engines
end;

procedure TThreadSafeManager.StopDebugger;
begin
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'StopDebugger', [], self);
  if Assigned(fRemoteDebugger) then
    fRemoteDebugger := nil;
end;

procedure TThreadSafeManager.DebuggerLog(const Text: RawUtf8);
begin
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'Log %', [Text], self);
  if Assigned(fRemoteDebugger) then
    fRemoteDebugger.DoLog(Text);
end;

procedure TThreadSafeManager.ClearWorkers;
begin
  if not Assigned(fWorkerManager) then
    exit; // nothing to clear
  if Assigned(fOnLog) then
    fOnLog(sllDebug, 'ClearWorkers count=%', [fWorkerManager.Count], self);
  fWorkerManager := nil;
  fWorkerManager := NewWorkerManager;
end;


{ TThreadSafeEngine }

constructor TThreadSafeEngine.Create(aManager: TThreadSafeManager;
  aThreadData: pointer; aTag: PtrInt; aThreadID: TThreadID);
begin
  inherited Create; // may have been overriden
  fManager := aManager;
  fCreateTix := GetTickCount64;
  fThreadID := aThreadId;
  fThreadData := aThreadData;
  fTag := aTag;
  if Assigned(fManager) then
  begin
    fContentVersion := fManager.ContentVersion;
    if Assigned(fManager.fOnGetName) then
      fNameForDebug := fManager.fOnGetName(self);
    if fNameForDebug = '' then
      FormatUtf8('% %', [ToHexShort(@fThreadId, SizeOf(fThreadId)),
        CurrentThreadName], fNameForDebug);
    if Assigned(fManager.fOnGetWebAppRootPath) then
      fWebAppRootDir := fManager.fOnGetWebAppRootPath(self)
    else
      StringToUtf8(Executable.ProgramFilePath, fWebAppRootDir);
  end;
  // TThreadSafeManager will now call AfterCreate outside of its main lock
end;

destructor TThreadSafeEngine.Destroy;
begin
  BeforeDestroy;
  if Assigned(fManager.RemoteDebugger) then
    fManager.RemoteDebugger.StopDebugCurrentThread(self);
  inherited Destroy;
  fAtomCache.Free;
end;

procedure TThreadSafeEngine.ThreadSafeCall(const Event: TEngineEvent);
begin
  if not Assigned(Event) then
    exit; // nothing to call
  DoBeginRequest;
  try
    Event(self);
  finally
    DoEndRequest;
  end;
end;

function TThreadSafeEngine.AtomCacheFind(const Name: RawUtf8): TScriptAtom;
begin
  // note: atoms are always local to a script context -> not in manager
  if fAtomCache = nil then
    result := nil
  else
    result := fAtomCache.GetObjectFrom(Name);
end;

procedure TThreadSafeEngine.AtomCacheAdd(const Name: RawUtf8; Atom: TScriptAtom);
begin
  if fAtomCache = nil then
    fAtomCache := TRawUtf8List.CreateEx([fCaseSensitive, fNoDuplicate]);
  fAtomCache.AddObject(Name, Atom);
end;

procedure TThreadSafeEngine.DoBeginRequest;
begin
  // paranoid todo: check if we need a Lock here to avoid GPF at expiration?
  if fRequestFpuBackup[0] = high(fRequestFpuBackup) then
    raise EScriptException.CreateUtf8(
      'Too Many Nested %.DoBeginRequest', [self]);
  inc(fRequestFpuBackup[0]);
  fRequestFpuBackup[fRequestFpuBackup[0]] := SetFpuFlags;
end;

procedure TThreadSafeEngine.DoEndRequest;
begin
  if fRequestFpuBackup[0] = 0 then
    exit;
  dec(fRequestFpuBackup[0]);
  ResetFpuFlags(fRequestFpuBackup[fRequestFpuBackup[0]]);
end;



end.

