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
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.lib.static;


{ ******************* }

type
  /// exception class raised from scripting fatal issues
  EScriptException = class(ESynException);

  {$M+}
  TThreadSafeManager = class;
  {$M-}

  /// abstract parent class implementing a ThreadSafe (Java)Script engine
  // - use TThreadSafeManager.ThreadSafeEngine to retrieve the Engine instance
  // corresponding to the current thread, for thread-safety
  TThreadSafeEngine = class(TSynPersistent)
  protected
    fThreadID: TThreadID;
    fThreadName: RawUtf8;
    fThreadData: pointer;
    fContentVersion: cardinal;
    fManager: TThreadSafeManager;
    fCreateTix: Int64;
    fTag: PtrInt;
    fPrivateDataForDebugger: pointer;
    fNeverExpire: boolean;
    fNameForDebug, fWebAppRootDir: RawUtf8;
    fRequestFpuBackup: TFPUExceptionMask;
    // those methods should be overriden with the proper scripting API
    procedure DoBeginRequest; virtual; // for multi-threading (e.g. for SM)
    procedure DoEndRequest; virtual;
  public
    /// create one thread-safe (Java) Script Engine instance
    // - inherited classes should initialize internal script Runtime, Context,
    // global objects and standard classes
    // - do not create Engine directly via this constructor, but instead call
    // TThreadSafeManager.ThreadSafeEngine
    // - this constructor should be called within the thread associated with
    // this engine instance
    constructor Create(aManager: TThreadSafeManager; aThreadData: pointer;
      aTag: PtrInt; aThreadID: TThreadID); reintroduce; virtual;
    /// finalize the Script engine instance
    destructor Destroy; override;

    /// reference to TThreadSafeManager owning this Engine
    property Manager: TThreadSafeManager
      read fManager;
    /// can hold any engine-specific data
    // - can be specified as second arg to TThreadSafeManager.ThreadSafeEngine
    property Tag: PtrInt
      read fTag write fTag;
    /// thread specific data
    // - pointer to any structure, passed into ThreadSafeEngine call
    // - used to access a thread-details in the native functions e.g.
    // as TThreadSafeEngine(cx.PrivateData).ThreadData
    property ThreadData: pointer
      read fThreadData write fThreadData;
    /// the associated thread ID, as retrieved during intiialization
    // - could be accessed from other threads, e.g. for logging/debugging
    property ThreadID: TThreadID
      read fThreadID;
    /// the associated thread name, as retrieved during intiialization
    // - could be accessed from other threads, e.g. for logging/debugging
    property ThreadName: RawUtf8
      read fThreadName;
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
    /// name of this engine
    // - as shown in the debugger to ease context identification
    property NameForDebug: RawUtf8
      read fNameForDebug;
    /// root path for the current Web Application engine context
    property WebAppRootDir: RawUtf8
      read fWebAppRootDir;
    /// can be set so that this engine will never expire
    property NeverExpire: boolean
      read fNeverExpire write fNeverExpire;
  end;

  /// meta-class of our thread-safe engines
  TThreadSafeEngineClass = class of TThreadSafeEngine;

  /// prototype of Script engines notification callback method
  TEngineEvent = procedure(Engine: TThreadSafeEngine) of object;

  /// callback returning a text value
  // - used e.g. for getting engine name or web app root path
  TEngineNameEvent = function(Engine: TThreadSafeEngine): RawUtf8 of object;

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
    fEngines: TSynObjectListLocked; // TThreadSafeEngine
    function ThreadEngineIndex(aThreadID: TThreadID): integer;
    function GetPauseDebuggerOnFirstStep: boolean;
    procedure SetPauseDebuggerOnFirstStep(aPauseDebuggerOnFirstStep: boolean);
    function GetEngineExpireTimeOutMinutes: cardinal;
    procedure SetEngineExpireTimeOutMinutes(Value: cardinal);
    // those methods should be overriden with the proper scripting API
    procedure DoOnNewEngine(Engine: TThreadSafeEngine); virtual;
  public
    /// initialize the scripting manager
    constructor Create(aEngineClass: TThreadSafeEngineClass); reintroduce;
    /// finalize the scripting manager
    destructor Destroy; override;
    /// get or create one Engine associated with current running thread
    // - aThreadData is a pointer to any structure relative to this thread
    // accessible via TThreadSafeEngine.ThreadData
    function ThreadSafeEngine(ThreadData: pointer;
      TagForNewEngine: PtrInt = 0): TThreadSafeEngine;
    /// specify a maximum lifetime period after which script engines will
    // be recreated, to avoid potential JavaScript memory leak (variables in global,
    // closures circle, etc.)
    // - 0 by default - mean no expire timeout
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
    procedure DebuggerLog(const Text: RawUTF8);
    /// redirect to the associated debugger thread flag if the debugger
    // connected to new engine must pause on first step
    property PauseDebuggerOnFirstStep: boolean
      read GetPauseDebuggerOnFirstStep write SetPauseDebuggerOnFirstStep;


    /// the associated (Java)Script engine class
    property EngineClass: TThreadSafeEngineClass
      read fEngineClass;
    /// incremental sequence number of engine scripts
    // - used in ThreadSafeEngine to determine if context is up to date,
    // in order to trigger on-the-fly reload of scripts without the
    // need of restarting the application/service
    // - caller must change this parameter value e.g. in case of changes in
    // the scripts folder in an HTTP server
    property ContentVersion: cardinal
      read fContentVersion write fContentVersion;
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
    /// event triggered every time a internal debugger process connected to Engine
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

// current doc about remote protocol (and stream format)
// https://firefox-source-docs.mozilla.org/devtools/backend/protocol.html

{ TThreadSafeManager }

constructor TThreadSafeManager.Create(aEngineClass: TThreadSafeEngineClass);
begin
  inherited Create;
  fEngineClass := aEngineClass;
  fEngines := TSynObjectListLocked.Create;
end;

destructor TThreadSafeManager.Destroy;
var
  endtix: Int64;
begin
  StopDebugger;
  endtix := GetTickCount64 + 10000;
  while (fEngines.Count > 0) and
        (GetTickCount64 < endtix) do
    SleepHiRes(50);
  if fEngines.Count>0 then
    raise EScriptException.CreateUtf8(
      '%.Destroy: here are % unreleased engines', [self, fEngines.Count]);
  inherited Destroy;
  fEngines.Free;
end;

function TThreadSafeManager.ThreadSafeEngine(ThreadData: pointer;
  TagForNewEngine: PtrInt): TThreadSafeEngine;
var
  tid: TThreadID;
  i: integer;
  tobereleased: TThreadSafeEngine;
begin
  tid := GetThreadID;
  tobereleased := nil;
  fEngines.Safe.Lock;
  try
    i := ThreadEngineIndex(tid);
    if i >= 0 then
    begin
      result := fEngines.List[i];
      if ThreadData = nil then
        ThreadData := result.fThreadData // to be used if recreated
      else
        result.fThreadData := ThreadData; // override with the given parameter
      if result.NeverExpire or
         (fEngineExpireTimeOutTix = 0) or
         (GetTickCount64 - result.fCreateTix < fEngineExpireTimeOutTix) then
        if result.fContentVersion = fContentVersion then
          // we got the right engine -> return quickly
          exit;
      // the engine is expired or its content changed -> recreate
      tobereleased := result;
      fEngines.Delete(i, {dontfree=}true);
    end;
    result := fEngineClass.Create(self, ThreadData, TagForNewEngine, tid);
    fEngines.Add(result);
  finally
    fEngines.Safe.UnLock;
    if tobereleased <> nil then
      // done outside the lock - garbage collection may take some time
      tobereleased.Free;
  end;
end;

function TThreadSafeManager.GetEngineExpireTimeOutMinutes: cardinal;
begin
  result := fEngineExpireTimeOutTix div 60000;
end;

procedure TThreadSafeManager.SetEngineExpireTimeOutMinutes(Value: cardinal);
begin
  fEngineExpireTimeOutTix := Value * 60000;
end;

procedure TThreadSafeManager.DoOnNewEngine(Engine: TThreadSafeEngine);
begin
  if not Assigned(fOnNewEngine) then
    exit;
  Engine.DoBeginRequest;
  try
    fOnNewEngine(Engine);
  finally
    Engine.DoEndRequest;
  end;
end;

function TThreadSafeManager.ThreadEngineIndex(aThreadID: TThreadID): integer;
var
  e: ^TThreadSafeEngine;
begin
  // caller made fEngines.Safe.Lock
  e := pointer(fEngines.List);
  for result := 0 to fEngines.Count - 1 do
    if e^.fThreadID = aThreadID then
      exit
    else
      inc(e); // brute force search is fast enough within a thread pool
  result := -1;
end;

function TThreadSafeManager.GetPauseDebuggerOnFirstStep: boolean;
begin

end;

procedure TThreadSafeManager.SetPauseDebuggerOnFirstStep(
  aPauseDebuggerOnFirstStep: boolean);
begin

end;

procedure TThreadSafeManager.StartDebugger(const Port: RawUtf8;
  ForMainThread: boolean);
begin

end;

procedure TThreadSafeManager.StopDebugger;
begin

end;

procedure TThreadSafeManager.DebuggerLog(const Text: RawUTF8);
begin

end;

{ TThreadSafeEngine }

constructor TThreadSafeEngine.Create(aManager: TThreadSafeManager;
  aThreadData: pointer; aTag: PtrInt; aThreadID: TThreadID);
begin
  inherited Create;
  fManager := aManager;
  fCreateTix := GetTickCount64;
  fContentVersion := fManager.ContentVersion;
  fThreadID := aThreadId;
  fThreadName := GetCurrentThreadName;
  fThreadData := aThreadData;
  fTag := aTag;
  if Assigned(fManager.fOnGetName) then
    fNameForDebug := fManager.fOnGetName(self);
  if Assigned(fManager.fOnGetWebAppRootPath) then
    fWebAppRootDir := fManager.fOnGetWebAppRootPath(self);
end;

destructor TThreadSafeEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TThreadSafeEngine.DoBeginRequest;
begin
  if fRequestFpuBackup <> [] then
    // typical pascal FPU mask is [exDenormalized,exUnderflow,exPrecision]
    raise EScriptException.CreateUtf8('Nested %.DoBeginRequest', [self]);
  fRequestFpuBackup := BeforeLibraryCall;
  assert(fRequestFpuBackup <> []);
end;

procedure TThreadSafeEngine.DoEndRequest;
begin
  AfterLibraryCall(fRequestFpuBackup);
  fRequestFpuBackup := [];
end;



end.

