/// Framework Core Multi-Threading Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.threads;

{
  *****************************************************************************

   High-Level Multi-Threading features shared by all framework units
    - Thread-Safe Pending Tasks List
    - Background Thread Processing
    - Parallel Execution in a Thread Pool
    - Server Process Oriented Thread Pool

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  syncobjs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.text,
  mormot.core.data,
  mormot.core.perf;


{ ************ Thread-Safe Pending Tasks List }

const
  /// defined here to avoid explicit link to syncobjs in uses clause
  wrSignaled = syncobjs.wrSignaled;

type
  /// defined here to avoid explicit link to syncobjs in uses clause
  TWaitResult = syncobjs.TWaitResult;

  /// defined here to avoid explicit link to syncobjs in uses clause
  TEvent = syncobjs.TEvent;

  /// exception class raised by this unit
  ESynThread = class(ESynException);

  /// internal item definition, used by TPendingTaskList storage
  TPendingTaskListItem = packed record
    /// the task should be executed when TPendingTaskList.GetTimestamp reaches
    // this value
    Timestamp: Int64;
    /// the associated task, stored by representation as raw binary
    Task: RawByteString;
  end;

  /// internal list definition, used by TPendingTaskList storage
  TPendingTaskListItemDynArray = array of TPendingTaskListItem;

  /// handle a list of tasks, stored as RawByteString, with a time stamp
  // - internal time stamps would be GetTickCount64 by default, so have a
  // resolution of about 16 ms under Windows
  // - you can add tasks to the internal list, to be executed after a given
  // delay, using a post/peek like algorithm
  // - execution delays are not expected to be accurate, but are best guess,
  // according to NextTask call
  // - this implementation is thread-safe, thanks to the Safe internal locker
  TPendingTaskList = class(TSynLocked)
  protected
    fCount: integer;
    fTask: TPendingTaskListItemDynArray;
    fTasks: TDynArray;
    function GetCount: integer;
    function GetTimestamp: Int64; virtual;
  public
    /// initialize the list memory and resources
    constructor Create; override;
    /// append a task, specifying a delay in milliseconds from current time
    procedure AddTask(aMilliSecondsDelayFromNow: integer;
      const aTask: RawByteString); virtual;
    /// append several tasks, specifying a delay in milliseconds between tasks
    // - first supplied delay would be computed from the current time, then
    // it would specify how much time to wait between the next supplied task
    procedure AddTasks(const aMilliSecondsDelays: array of integer;
      const aTasks: array of RawByteString);
    /// retrieve the next pending task
    // - returns '' if there is no scheduled task available at the current time
    // - returns the next stack as defined corresponding to its specified delay
    function NextPendingTask: RawByteString; virtual;
    /// flush all pending tasks
    procedure Clear; virtual;
    /// access to the internal TPendingTaskListItem.Timestamp stored value
    // - corresponding to the current time
    // - default implementation is to return GetTickCount64, with a 16 ms
    // typical resolution under Windows
    property Timestamp: Int64
      read GetTimestamp;
    /// how many pending tasks are currently defined
    property Count: integer
      read GetCount;
    /// direct low-level access to the internal task list
    // - warning: this dynamic array length is the list capacity: use Count
    // property to retrieve the exact number of stored items
    // - use Safe.Lock/TryLock with a try ... finally Safe.Unlock block for
    // thread-safe access to this array
    // - items are stored in increasing Timestamp, i.e. the first item is
    // the next one which would be returned by the NextPendingTask method
    property Task: TPendingTaskListItemDynArray
      read fTask;
  end;


{ ************ Background Thread Processing }

type
  {$M+}
  TSynBackgroundThreadAbstract = class;
  TSynBackgroundThreadEvent = class;
  {$M-}

  /// idle method called by TSynBackgroundThreadAbstract in the caller thread
  // during remote blocking process in a background thread
  // - typical use is to run Application.ProcessMessages, e.g. for
  // TRestClientUri.Uri() to provide a responsive UI even in case of slow
  // blocking remote access
  // - provide the time elapsed (in milliseconds) from the request start (can be
  // used e.g. to popup a temporary message to wait)
  // - is call once with ElapsedMS=0 at request start
  // - is call once with ElapsedMS=-1 at request ending
  // - see TLoginForm.OnIdleProcess and OnIdleProcessForm in mORMotUILogin.pas
  TOnIdleSynBackgroundThread = procedure(Sender: TSynBackgroundThreadAbstract;
    ElapsedMS: integer) of object;

  /// event prototype used e.g. by TSynBackgroundThreadAbstract and TSynThread callbacks
  TOnNotifyThread = procedure(Sender: TThread) of object;

  /// abstract TThread with its own execution content
  // - you should not use this class directly, but use either
  // TSynBackgroundThreadMethodAbstract / TSynBackgroundThreadEvent /
  // TSynBackgroundThreadMethod and provide a much more convenient callback
  TSynBackgroundThreadAbstract = class(TThread)
  protected
    fProcessEvent: TEvent;
    fOnBeforeExecute: TOnNotifyThread;
    fOnAfterExecute: TOnNotifyThread;
    fThreadName: RawUtf8;
    fExecute: (exCreated, exRun, exFinished);
    fExecuteLoopPause: boolean;
    procedure SetExecuteLoopPause(dopause: boolean);
    /// where the main process takes place
    procedure Execute; override;
    procedure ExecuteLoop; virtual; abstract;
  public
    /// initialize the thread
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TRestServer.BeginCurrentThread/EndCurrentThread, or
    // at least set OnAfterExecute to TSynLogFamily.OnThreadEnded
    constructor Create(const aThreadName: RawUtf8;
      const OnBeforeExecute: TOnNotifyThread = nil;
      const OnAfterExecute: TOnNotifyThread = nil;
      CreateSuspended: boolean = false); reintroduce;
    /// release used resources
    // - calls WaitForNotExecuting(100) for proper finalization
    destructor Destroy; override;
    {$ifndef HASTTHREADSTART}
    /// method to be called to start the thread
    // - Resume is deprecated in the newest RTL, since some OS - e.g. Linux -
    // do not implement this pause/resume feature; we define here this method
    // for older versions of Delphi
    procedure Start;
    {$endif HASTTHREADSTART}
    {$ifdef HASTTHREADTERMINATESET}
    /// properly terminate the thread
    // - called by TThread.Terminate since Delphi XE2
    procedure TerminatedSet; override;
    {$else}
    /// properly terminate the thread
    // - called by reintroduced Terminate
    procedure TerminatedSet; virtual;
    /// reintroduced to call TeminatedSet
    procedure Terminate; reintroduce;
    {$endif HASTTHREADTERMINATESET}
    /// wait for Execute/ExecuteLoop to be ended (i.e. fExecute<>exRun)
    // - call Sleep() in a loop until the timeout is reached
    // - used e.g. in Destroy to avoid any GPF and ensure clean finalization
    procedure WaitForNotExecuting(maxMS: integer = 500);
    /// safe version of Sleep() which won't break the thread process
    // - returns TRUE if the thread was Terminated
    // - returns FALSE if successfully waited up to MS milliseconds
    function SleepOrTerminated(MS: cardinal): boolean;
    /// temporary stop the execution of ExecuteLoop, until set back to false
    // - may be used e.g. by TSynBackgroundTimer to delay the process of
    // background tasks
    property Pause: boolean
      read fExecuteLoopPause write SetExecuteLoopPause;
    /// access to the low-level associated event used to notify task execution
    // to the background thread
    // - you may call ProcessEvent.SetEvent to trigger the internal process loop
    property ProcessEvent: TEvent
      read fProcessEvent;
    /// defined as public since may be used to terminate the processing methods
    property Terminated;
  end;

  /// state machine status of the TSynBackgroundThreadAbstract process
  TSynBackgroundThreadProcessStep = (
    flagIdle,
    flagStarted,
    flagFinished,
    flagDestroying);

  /// state machine statuses of the TSynBackgroundThreadAbstract process
  TSynBackgroundThreadProcessSteps = set of TSynBackgroundThreadProcessStep;

  /// abstract TThread able to run a method in its own execution content
  // - typical use is a background thread for processing data or remote access,
  // while the UI will be still responsive by running OnIdle event in loop: see
  // e.g. how TRestClientUri.OnIdle handle this in mormot.rest.client.pas unit
  // - you should not use this class directly, but inherit from it and override
  // the Process method, or use either TSynBackgroundThreadEvent /
  // TSynBackgroundThreadMethod and provide a much more convenient callback
  TSynBackgroundThreadMethodAbstract = class(TSynBackgroundThreadAbstract)
  protected
    fCallerEvent: TEvent;
    fParam: pointer;
    fCallerThreadID: TThreadID;
    fBackgroundException: Exception;
    fOnIdle: TOnIdleSynBackgroundThread;
    fOnBeforeProcess: TOnNotifyThread;
    fOnAfterProcess: TOnNotifyThread;
    fPendingProcessFlag: TSynBackgroundThreadProcessStep;
    fPendingProcessLock: TSynLocker;
    procedure ExecuteLoop; override;
    function OnIdleProcessNotify(start: Int64): integer;
    function GetOnIdleBackgroundThreadActive: boolean;
    function GetPendingProcess: TSynBackgroundThreadProcessStep;
    procedure SetPendingProcess(State: TSynBackgroundThreadProcessStep);
    // returns  flagIdle if acquired, flagDestroying if terminated
    function AcquireThread: TSynBackgroundThreadProcessStep;
    procedure WaitForFinished(start: Int64; const onmainthreadidle: TNotifyEvent);
    /// called by Execute method when fProcessParams<>nil and fEvent is notified
    procedure Process; virtual; abstract;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TRestServer.BeginCurrentThread/EndCurrentThread
    constructor Create(const aOnIdle: TOnIdleSynBackgroundThread;
      const aThreadName: RawUtf8; const OnBeforeExecute: TOnNotifyThread = nil;
      const OnAfterExecute: TOnNotifyThread = nil); reintroduce;
    /// finalize the thread
    destructor Destroy; override;
    /// launch Process abstract method asynchronously in the background thread
    // - wait until process is finished, calling OnIdle() callback in
    // the meanwhile
    // - any exception raised in background thread will be translated in the
    // caller thread
    // - returns false if self is not set, or if called from the same thread
    // as it is currently processing (to avoid race condition from OnIdle()
    // callback)
    // - returns true when the background process is finished
    // - OpaqueParam will be used to specify a thread-safe content for the
    // background process
    // - this method is thread-safe, that is it will wait for any started process
    // already launch by another thread: you may call this method from any
    // thread, even if its main purpose is to be called from the main UI thread
    function RunAndWait(OpaqueParam: pointer): boolean;
    /// set a callback event to be executed in loop during remote blocking
    // process, e.g. to refresh the UI during a somewhat long request
    // - you can assign a callback to this property, calling for instance
    // Application.ProcessMessages, to execute the remote request in a
    // background thread, but let the UI still be reactive: the
    // TLoginForm.OnIdleProcess and OnIdleProcessForm methods of
    // mORMotUILogin.pas will match this property expectations
    // - if OnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    property OnIdle: TOnIdleSynBackgroundThread
      read fOnIdle write fOnIdle;
    /// TRUE if the background thread is active, and OnIdle event is called
    // during process
    // - to be used e.g. to ensure no re-entrance from User Interface messages
    property OnIdleBackgroundThreadActive: boolean
      read GetOnIdleBackgroundThreadActive;
    /// optional callback event triggered in Execute before each Process
    property OnBeforeProcess: TOnNotifyThread
      read fOnBeforeProcess write fOnBeforeProcess;
    /// optional callback event triggered in Execute after each Process
    property OnAfterProcess: TOnNotifyThread
      read fOnAfterProcess write fOnAfterProcess;
  end;

  /// background process method called by TSynBackgroundThreadEvent
  // - will supply the OpaqueParam parameter as provided to RunAndWait()
  // method when the Process virtual method will be executed
  TOnProcessSynBackgroundThread = procedure(Sender: TSynBackgroundThreadEvent;
    ProcessOpaqueParam: pointer) of object;

  /// allow background thread process of a method callback
  TSynBackgroundThreadEvent = class(TSynBackgroundThreadMethodAbstract)
  protected
    fOnProcess: TOnProcessSynBackgroundThread;
    /// just call the OnProcess handler
    procedure Process; override;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    constructor Create(const aOnProcess: TOnProcessSynBackgroundThread;
      const aOnIdle: TOnIdleSynBackgroundThread;
      const aThreadName: RawUtf8); reintroduce;
    /// provide a method handler to be execute in the background thread
    // - triggered by RunAndWait() method - which will wait until finished
    // - the OpaqueParam as specified to RunAndWait() will be supplied here
    property OnProcess: TOnProcessSynBackgroundThread
      read fOnProcess write fOnProcess;
  end;

  /// allow background thread process of a variable TThreadMethod callback
  TSynBackgroundThreadMethod = class(TSynBackgroundThreadMethodAbstract)
  protected
    /// just call the TThreadMethod, as supplied to RunAndWait()
    procedure Process; override;
  public
    /// run once the supplied TThreadMethod callback
    // - use this method, and not the inherited RunAndWait()
    procedure RunAndWait(Method: TThreadMethod); reintroduce;
  end;

  /// background process procedure called by TSynBackgroundThreadProcedure
  // - will supply the OpaqueParam parameter as provided to RunAndWait()
  // method when the Process virtual method will be executed
  TOnProcessSynBackgroundThreadProc = procedure(ProcessOpaqueParam: pointer);

  /// allow background thread process of a procedure callback
  TSynBackgroundThreadProcedure = class(TSynBackgroundThreadMethodAbstract)
  protected
    fOnProcess: TOnProcessSynBackgroundThreadProc;
    /// just call the OnProcess handler
    procedure Process; override;
  public
    /// initialize the thread
    // - if aOnIdle is not set (i.e. equals nil), it will simply wait for
    // the background process to finish until RunAndWait() will return
    constructor Create(aOnProcess: TOnProcessSynBackgroundThreadProc;
      const aOnIdle: TOnIdleSynBackgroundThread;
      const aThreadName: RawUtf8); reintroduce;
    /// provide a procedure handler to be execute in the background thread
    // - triggered by RunAndWait() method - which will wait until finished
    // - the OpaqueParam as specified to RunAndWait() will be supplied here
    property OnProcess: TOnProcessSynBackgroundThreadProc
      read fOnProcess write fOnProcess;
  end;


type
  TSynBackgroundThreadProcess = class;

  /// event callback executed periodically by TSynBackgroundThreadProcess
  // - Event is wrTimeout after the OnProcessMS waiting period
  // - Event is wrSignaled if ProcessEvent.SetEvent has been called
  TOnSynBackgroundThreadProcess = procedure(Sender: TSynBackgroundThreadProcess;
    Event: TWaitResult) of object;

  /// TThread able to run a method at a given periodic pace
  TSynBackgroundThreadProcess = class(TSynBackgroundThreadAbstract)
  protected
    fOnProcess: TOnSynBackgroundThreadProcess;
    fOnException: TNotifyEvent;
    fOnProcessMS: cardinal;
    fStats: TSynMonitor;
    procedure ExecuteLoop; override;
  public
    /// initialize the thread for a periodic task processing
    // - aOnProcess would be called when ProcessEvent.SetEvent is called or
    // aOnProcessMS milliseconds period was elapse since last process
    // - if aOnProcessMS is 0, will wait until ProcessEvent.SetEvent is called
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TRestServer.BeginCurrentThread/EndCurrentThread
    constructor Create(const aThreadName: RawUtf8;
      const aOnProcess: TOnSynBackgroundThreadProcess;
      aOnProcessMS: cardinal; const aOnBeforeExecute: TOnNotifyThread = nil;
      const aOnAfterExecute: TOnNotifyThread = nil;
      aStats: TSynMonitorClass = nil;
      CreateSuspended: boolean = false); reintroduce; virtual;
    /// finalize the thread
    destructor Destroy; override;
    /// access to the implementation event of the periodic task
    property OnProcess: TOnSynBackgroundThreadProcess
      read fOnProcess;
    /// event callback executed when OnProcess did raise an exception
    // - supplied Sender parameter is the raised Exception instance
    property OnException: TNotifyEvent
      read fOnException write fOnException;
  published
    /// access to the delay, in milliseconds, of the periodic task processing
    property OnProcessMS: cardinal
      read fOnProcessMS write fOnProcessMS;
    /// processing statistics
    // - may be nil if aStats was nil in the class constructor
    property Stats: TSynMonitor
      read fStats;
  end;

  TSynBackgroundTimer = class;

  /// event callback executed periodically by TSynBackgroundThreadProcess
  // - Event is wrTimeout after the OnProcessMS waiting period
  // - Event is wrSignaled if ProcessEvent.SetEvent has been called
  // - Msg is '' if there is no pending message in this task FIFO
  // - Msg is set for each pending message in this task FIFO
  TOnSynBackgroundTimerProcess = procedure(Sender: TSynBackgroundTimer;
    Event: TWaitResult; const Msg: RawUtf8) of object;

  /// used by TSynBackgroundTimer internal registration list
  TSynBackgroundTimerTask = record
    OnProcess: TOnSynBackgroundTimerProcess;
    Secs: cardinal;
    NextTix: Int64;
    FIFO: TRawUtf8DynArray;
  end;

  /// stores TSynBackgroundTimer internal registration list
  TSynBackgroundTimerTaskDynArray = array of TSynBackgroundTimerTask;

  /// TThread able to run one or several tasks at a periodic pace in a
  // background thread
  // - as used e.g. by TRest.TimerEnable/TimerDisable methods, via the
  // inherited TRestBackgroundTimer
  // - each process can have its own FIFO of text messages
  // - if you expect to update some GUI, you should rather use a TTimer
  // component (with a period of e.g. 200ms), since TSynBackgroundTimer will
  // use its own separated thread
  TSynBackgroundTimer = class(TSynBackgroundThreadProcess)
  protected
    fTask: TSynBackgroundTimerTaskDynArray;
    fTasks: TDynArray;
    fTaskLock: TSynLocker;
    procedure EverySecond(Sender: TSynBackgroundThreadProcess;
      Event: TWaitResult);
    function Find(const aProcess: TMethod): integer;
    function Add(const aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsg: RawUtf8; aExecuteNow: boolean): boolean;
  public
    /// initialize the thread for a periodic task processing
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TRestServer.BeginCurrentThread/EndCurrentThread, as
    // made by TRestBackgroundTimer.Create
    constructor Create(const aThreadName: RawUtf8;
      const aOnBeforeExecute: TOnNotifyThread = nil;
      const aOnAfterExecute: TOnNotifyThread = nil;
      aStats: TSynMonitorClass = nil); reintroduce; virtual;
    /// finalize the thread
    destructor Destroy; override;
    /// define a process method for a task running on a periodic number of seconds
    // - for background process on a mORMot service, consider using TRest
    // TimerEnable/TimerDisable methods, and its associated BackgroundTimer thread
    procedure Enable(const aOnProcess: TOnSynBackgroundTimerProcess;
      aOnProcessSecs: cardinal);
    /// undefine a task running on a periodic number of seconds
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    // - for background process on a mORMot service, consider using TRestServer
    // TimerEnable/TimerDisable methods, and their TSynBackgroundTimer thread
    function Disable(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// add a message to be processed during the next execution of a task
    // - supplied message will be added to the internal FIFO list associated
    // with aOnProcess, then supplied to as aMsg parameter for each call
    // - if aExecuteNow is true, won't wait for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function EnQueue(const aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsg: RawUtf8; aExecuteNow: boolean = false): boolean; overload;
    /// add a message to be processed during the next execution of a task
    // - supplied message will be added to the internal FIFO list associated
    // with aOnProcess, then supplied to as aMsg parameter for each call
    // - if aExecuteNow is true, won't wait for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function EnQueue(const aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsgFmt: RawUtf8; const Args: array of const;
      aExecuteNow: boolean = false): boolean; overload;
    /// remove a message from the processing list
    // - supplied message will be searched in the internal FIFO list associated
    // with aOnProcess, then removed from the list if found
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied message was not registered
    function DeQueue(const aOnProcess: TOnSynBackgroundTimerProcess;
      const aMsg: RawUtf8): boolean;
    /// execute a task without waiting for the next aOnProcessSecs occurence
    // - aOnProcess should have been registered by a previous call to Enable() method
    // - returns true on success, false if the supplied task was not registered
    function ExecuteNow(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// execute a task without waiting for the next aOnProcessSecs occurence
    // - aOnProcess should not have been registered by a previous call to Enable() method
    function ExecuteOnce(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// returns true if there is currenly one task processed
    function Processing: boolean;
    /// wait until no background task is processed
    procedure WaitUntilNotProcessing(timeoutsecs: integer = 10);
    /// low-level access to the internal task list
    property Task: TSynBackgroundTimerTaskDynArray
      read fTask;
    /// low-level access to the internal task mutex
    property TaskLock: TSynLocker
      read fTaskLock;
  end;


type
  /// the current state of a TBlockingProcess instance
  TBlockingEvent = (
    evNone,
    evWaiting,
    evTimeOut,
    evRaised);

  {$M+}
  /// a semaphore used to wait for some process to be finished
  // - used e.g. by TBlockingCallback in mormot.rest.server.pas
  // - once created, process would block via a WaitFor call, which would be
  // released when NotifyFinished is called by the process background thread
  TBlockingProcess = class(TEvent)
  protected
    fTimeOutMs: integer;
    fEvent: TBlockingEvent;
    fSafe: PSynLocker;
    fOwnedSafe: boolean;
    procedure ResetInternal; virtual; // override to reset associated params
  public
    /// initialize the semaphore instance
    // - specify a time out millliseconds period after which blocking execution
    // should be handled as failure (if 0 is set, default 3000 would be used)
    // - an associated mutex shall be supplied
    constructor Create(aTimeOutMs: integer; aSafe: PSynLocker);
      reintroduce; overload; virtual;
    /// initialize the semaphore instance
    // - specify a time out millliseconds period after which blocking execution
    // should be handled as failure (if 0 is set, default 3000 would be used)
    // - an associated mutex would be created and owned by this instance
    constructor Create(aTimeOutMs: integer); reintroduce; overload; virtual;
    /// finalize the instance
    destructor Destroy; override;
    /// called to wait for NotifyFinished() to be called, or trigger timeout
    // - returns the final state of the process, i.e. evRaised or evTimeOut
    function WaitFor: TBlockingEvent; reintroduce; overload; virtual;
    /// called to wait for NotifyFinished() to be called, or trigger timeout
    // - returns the final state of the process, i.e. evRaised or evTimeOut
    function WaitFor(TimeOutMS: integer): TBlockingEvent; reintroduce; overload;
    /// should be called by the background process when it is finished
    // - the caller would then let its WaitFor method return
    // - returns TRUE on success (i.e. status was not evRaised or evTimeout)
    // - if the instance is already locked (e.g. when retrieved from
    // TBlockingProcessPool.FromCallLocked), you may set alreadyLocked=TRUE
    function NotifyFinished(alreadyLocked: boolean = false): boolean; virtual;
    /// just a wrapper to reset the internal Event state to evNone
    // - may be used to re-use the same TBlockingProcess instance, after
    // a successfull WaitFor/NotifyFinished process
    // - returns TRUE on success (i.e. status was not evWaiting), setting
    // the current state to evNone, and the Call property to 0
    // - if there is a WaitFor currently in progress, returns FALSE
    function Reset: boolean; virtual;
    /// just a wrapper around fSafe^.Lock
    procedure Lock;
    /// just a wrapper around fSafe^.Unlock
    procedure Unlock;
  published
    /// the current state of process
    // - use Reset method to re-use this instance after a WaitFor process
    property Event: TBlockingEvent
      read fEvent;
    /// the time out period, in ms, as defined at constructor level
    property TimeOutMs: integer
      read fTimeOutMS;
  end;
  {$M-}

  /// used to identify each TBlockingProcessPool call
  // - allow to match a given TBlockingProcessPoolItem semaphore
  TBlockingProcessPoolCall = type integer;

  /// a semaphore used in the TBlockingProcessPool
  // - such semaphore have a Call field to identify each execution
  TBlockingProcessPoolItem = class(TBlockingProcess)
  protected
    fCall: TBlockingProcessPoolCall;
    procedure ResetInternal; override;
  published
    /// an unique identifier, when owned by a TBlockingProcessPool
    // - Reset would restore this field to its 0 default value
    property Call: TBlockingProcessPoolCall
      read fCall;
  end;

  /// class-reference type (metaclass) of a TBlockingProcess
  TBlockingProcessPoolItemClass = class of TBlockingProcessPoolItem;

  /// manage a pool of TBlockingProcessPoolItem instances
  // - each call will be identified via a TBlockingProcessPoolCall unique value
  // - to be used to emulate e.g. blocking execution from an asynchronous
  // event-driven DDD process
  // - it would also allow to re-use TEvent system resources
  TBlockingProcessPool = class(TSynPersistent)
  protected
    fClass: TBlockingProcessPoolItemClass;
    fPool: TSynObjectListLocked;
    fCallCounter: TBlockingProcessPoolCall; // set TBlockingProcessPoolItem.Call
  public
    /// initialize the pool, for a given implementation class
    constructor Create(aClass: TBlockingProcessPoolItemClass = nil); reintroduce;
    /// finalize the pool
    // - would also force all pending WaitFor to trigger a evTimeOut
    destructor Destroy; override;
    /// book a TBlockingProcess from the internal pool
    // - returns nil on error (e.g. the instance is destroying)
    // - or returns the blocking process instance corresponding to this call;
    // its Call property would identify the call for the asynchronous callback,
    // then after WaitFor, the Reset method should be run to release the mutex
    // for the pool
    function NewProcess(aTimeOutMs: integer): TBlockingProcessPoolItem; virtual;
    /// retrieve a TBlockingProcess from its call identifier
    // - may be used e.g. from the callback of the asynchronous process
    // to set some additional parameters to the inherited TBlockingProcess,
    // then call NotifyFinished to release the caller WaitFor
    // - if leavelocked is TRUE, the returned instance would be locked: caller
    // should execute result.Unlock or NotifyFinished(true) after use
    function FromCall(call: TBlockingProcessPoolCall;
      locked: boolean = false): TBlockingProcessPoolItem; virtual;
  end;


{ ************ Parallel Execution in a Thread Pool }

type
  /// callback implementing some parallelized process for TSynParallelProcess
  // - if 0<=IndexStart<=IndexStop, it should execute some process
  TOnSynParallelProcess = procedure(IndexStart, IndexStop: integer) of object;

  /// thread executing process for TSynParallelProcess
  TSynParallelProcessThread = class(TSynBackgroundThreadMethodAbstract)
  protected
    fMethod: TOnSynParallelProcess;
    fIndexStart, fIndexStop: integer;
    procedure Start(const Method: TOnSynParallelProcess;
      IndexStart, IndexStop: integer);
    /// executes fMethod(fIndexStart,fIndexStop)
    procedure Process; override;
  public
  end;

  /// allow parallel execution of an index-based process in a thread pool
  // - will create its own thread pool, then execute any method by spliting the
  // work into each thread
  TSynParallelProcess = class(TSynPersistentLock)
  protected
    fThreadName: RawUtf8;
    fPool: array of TSynParallelProcessThread;
    fThreadPoolCount: integer;
    fParallelRunCount: integer;
  public
    /// initialize the thread pool
    // - you could define some callbacks to nest the thread execution, e.g.
    // assigned to TRestServer.BeginCurrentThread/EndCurrentThread
    // - up to MaxThreadPoolCount=32 threads could be setup (you may allow a
    // bigger value, but interrest of this thread pool is to have its process
    // saturating each CPU core)
    // - if ThreadPoolCount is 0, no thread would be created, and process
    // would take place in the current thread
    constructor Create(ThreadPoolCount: integer; const ThreadName: RawUtf8;
      const OnBeforeExecute: TOnNotifyThread = nil;
      const OnAfterExecute: TOnNotifyThread = nil;
      MaxThreadPoolCount: integer = 32); reintroduce; virtual;
    /// finalize the thread pool
    destructor Destroy; override;
    /// run a method in parallel, and wait for the execution to finish
    // - will split Method[0..MethodCount-1] execution over the threads
    // - in case of any exception during process, an ESynParallel
    // exception would be raised by this method
    // - if OnMainThreadIdle is set, the current thread (which is expected to be
    // e.g. the main UI thread) won't process anything, but call this event
    // during waiting for the background threads
    procedure ParallelRunAndWait(const Method: TOnSynParallelProcess;
      MethodCount: integer; const OnMainThreadIdle: TNotifyEvent = nil);
  published
    /// how many threads have been activated
    property ParallelRunCount: integer
      read fParallelRunCount;
    /// how many threads are currently in this instance thread pool
    property ThreadPoolCount: integer
      read fThreadPoolCount;
    /// some text identifier, used to distinguish each owned thread
    property ThreadName: RawUtf8
      read fThreadName;
  end;



{ ************ Server Process Oriented Thread Pool }

type
  {$M+}

  /// a simple TThread with a "Terminate" event run in the thread context
  // - the TThread.OnTerminate event is run within Synchronize() so did not
  // match our expectations to be able to release the resources in the thread
  // context which created them (e.g. for COM objects, or some DB drivers)
  // - used internally by THttpServerGeneric.NotifyThreadStart() - you should
  // not have to use the protected fOnThreadTerminate event handler
  // - also define a Start method for compatibility with older versions of Delphi
  TSynThread = class(TThread)
  protected
    fStartNotified: TObject;
    // we defined an fOnThreadTerminate event which would be run in the terminated
    // thread context (whereas TThread.OnTerminate is called in the main thread)
    // -> see THttpServerGeneric.OnHttpThreadTerminate event property
    fOnThreadTerminate: TOnNotifyThread;
    procedure DoTerminate; override;
  public
    /// initialize the thread instance, in non suspended state
    constructor Create(CreateSuspended: boolean); reintroduce; virtual;
    {$ifndef HASTTHREADSTART}
    /// method to be called when the thread was created as suspended
    // - Resume is deprecated in the newest RTL, since some OS - e.g. Linux -
    // do not implement this pause/resume feature
    // - we define here this method for older versions of Delphi
    procedure Start;
    {$endif HASTTHREADSTART}
    /// safe version of Sleep() which won't break the thread process
    // - returns TRUE if the thread was Terminated
    // - returns FALSE if successfully waited up to MS milliseconds
    function SleepOrTerminated(MS: cardinal): boolean;
    /// ensure fOnThreadTerminate is called only if NotifyThreadStart has been done
    property StartNotified: TObject
      read fStartNotified write fStartNotified;
    /// defined as public since may be used to terminate the processing methods
    property Terminated;
  end;

  TSynThreadPool = class;

  /// defines the work threads used by TSynThreadPool
  TSynThreadPoolWorkThread = class(TSynThread)
  protected
    fOwner: TSynThreadPool;
    fThreadNumber: integer;
    {$ifndef USE_WINIOCP}
    fProcessingContext: pointer;
    fEvent: TEvent;
    {$endif USE_WINIOCP}
    procedure NotifyThreadStart(Sender: TSynThread);
    procedure DoTask(Context: pointer); // exception-safe call of fOwner.Task()
  public
    /// initialize the thread
    constructor Create(Owner: TSynThreadPool); reintroduce;
    /// finalize the thread
    destructor Destroy; override;
    /// will loop for any pending task, and execute fOwner.Task()
    procedure Execute; override;
  end;

  TSynThreadPoolWorkThreads = array of TSynThreadPoolWorkThread;

  /// a simple Thread Pool, used e.g. for fast handling HTTP requests
  // - implemented over I/O Completion Ports under Windows, or a classical
  // Event-driven approach under Linux/POSIX
  TSynThreadPool = class
  protected
    fWorkThread: TSynThreadPoolWorkThreads;
    fWorkThreadCount: integer;
    fRunningThreads: integer;
    fExceptionsCount: integer;
    fOnThreadTerminate: TOnNotifyThread;
    fOnThreadStart: TOnNotifyThread;
    fTerminated: boolean;
    fContentionAbortCount: cardinal;
    fContentionTime: Int64;
    fContentionCount: cardinal;
    fContentionAbortDelay: integer;
    fName: RawUtf8;
    {$ifdef USE_WINIOCP}
    fRequestQueue: THandle; // IOCP has its own internal queue
    {$else}
    fQueuePendingContext: boolean;
    fPendingContext: array of pointer;
    fPendingContextCount: integer;
    fSafe: TRTLCriticalSection;
    function GetPendingContextCount: integer;
    function PopPendingContext: pointer;
    function QueueLength: integer; virtual;
    {$endif USE_WINIOCP}
    /// end thread on IO error
    function NeedStopOnIOError: boolean; virtual;
    /// process to be executed after notification
    procedure Task(aCaller: TSynThread; aContext: Pointer); virtual; abstract;
    procedure TaskAbort(aContext: Pointer); virtual;
  public
    /// initialize a thread pool with the supplied number of threads
    // - abstract Task() virtual method will be called by one of the threads
    // - up to 256 threads can be associated to a Thread Pool
    // - can optionaly accept aOverlapHandle - a handle previously
    // opened for overlapped I/O (IOCP) under Windows
    // - aQueuePendingContext=true will store the pending context into
    // an internal queue, so that Push() always returns true
    {$ifdef USE_WINIOCP}
    constructor Create(NumberOfThreads: integer = 32;
      aOverlapHandle: THandle = INVALID_HANDLE_VALUE; const aName: RawUtf8 = '');
    {$else}
    constructor Create(NumberOfThreads: integer = 32;
      aQueuePendingContext: boolean = false; const aName: RawUtf8 = '');
    {$endif USE_WINIOCP}
    /// shut down the Thread pool, releasing all associated threads
    destructor Destroy; override;
    /// let a task (specified as a pointer) be processed by the Thread Pool
    // - returns false if there is no idle thread available in the pool and
    // Create(aQueuePendingContext=false) was used (caller should retry later);
    // if aQueuePendingContext was true in Create, or IOCP is used, the supplied
    // context will be added to an internal list and handled when possible
    // - if aWaitOnContention is default false, returns immediately when the
    // queue is full; set aWaitOnContention=true to wait up to
    // ContentionAbortDelay ms and retry to queue the task
    function Push(aContext: pointer; aWaitOnContention: boolean = false): boolean;
    {$ifndef USE_WINIOCP}
    /// may be called after Push() returned false to see if queue was actually full
    // - returns false if QueuePendingContext is false
    function QueueIsFull: boolean;
    /// parameter as supplied to Create constructor
    property QueuePendingContext: boolean read fQueuePendingContext;
    {$endif USE_WINIOCP}
    /// low-level access to the threads defined in this thread pool
    property WorkThread: TSynThreadPoolWorkThreads
      read fWorkThread;
  published
    /// how many threads are available in the pool
    // - maps Create() parameter, i.e. 32 by default
    property WorkThreadCount: integer
      read fWorkThreadCount;
    /// how many threads are currently processing tasks in this thread pool
    // - is in the range 0..WorkThreadCount
    property RunningThreads: integer
      read fRunningThreads;
    /// how many tasks were rejected due to thread pool contention
    // - if this number is high, consider setting a higher number of threads,
    // or profile and tune the Task method
    property ContentionAbortCount: cardinal
      read fContentionAbortCount;
    /// milliseconds delay to reject a connection due to contention
    // - default is 5000, i.e. 5 seconds wait for some room to be available
    // in the IOCP or aQueuePendingContext internal list
    // - during this delay, no new connection is available (i.e. Accept is not
    // called), so that a load balancer could detect the contention and switch
    // to another instance in the pool, or a direct client may eventually have
    // its connection rejected, so won't start sending data
    property ContentionAbortDelay: integer
      read fContentionAbortDelay write fContentionAbortDelay;
    /// total milliseconds spent waiting for an available slot in the queue
    // - contention won't fail immediately, but will retry until ContentionAbortDelay
    // - any high number here requires code refactoring of the Task method
    property ContentionTime: Int64
      read fContentionTime;
    /// how many times the pool waited for an available slot in the queue
    // - contention won't fail immediately, but will retry until ContentionAbortDelay
    // - any high number here may better increase the threads count
    // - use this property and ContentionTime to compute the average contention time
    property ContentionCount: cardinal
      read fContentionCount;
    {$ifndef USE_WINIOCP}
    /// how many input tasks are currently waiting to be affected to threads
    property PendingContextCount: integer
      read GetPendingContextCount;
    {$endif USE_WINIOCP}
  end;

  {$M-}


const
  // allow up to 256 * 2MB = 512MB of RAM for the TSynThreadPoolWorkThread stack
  THREADPOOL_MAXTHREADS = 256;


implementation


{ ************ Thread-Safe Pending Tasks List }

{ TPendingTaskList }

constructor TPendingTaskList.Create;
begin
  inherited Create;
  fTasks.InitSpecific(TypeInfo(TPendingTaskListItemDynArray),
    fTask, ptInt64, @fCount); // sorted by Timestamp
end;

function TPendingTaskList.GetTimestamp: Int64;
begin
  result := GetTickCount64;
end;

procedure TPendingTaskList.AddTask(aMilliSecondsDelayFromNow: integer;
  const aTask: RawByteString);
var
  item: TPendingTaskListItem;
  ndx: integer;
begin
  item.Timestamp := GetTimestamp + aMilliSecondsDelayFromNow;
  item.Task := aTask;
  fSafe.Lock;
  try
    if fTasks.FastLocateSorted(item, ndx) then
      inc(ndx); // always insert just after any existing timestamp
    fTasks.FastAddSorted(ndx, item);
  finally
    fSafe.UnLock;
  end;
end;

procedure TPendingTaskList.AddTasks(const aMilliSecondsDelays: array of integer;
  const aTasks: array of RawByteString);
var
  item: TPendingTaskListItem;
  i, ndx: integer;
begin
  if length(aTasks) <> length(aMilliSecondsDelays) then
    exit;
  item.Timestamp := GetTimestamp;
  fSafe.Lock;
  try
    for i := 0 to High(aTasks) do
    begin
      inc(item.Timestamp, aMilliSecondsDelays[i]);
      item.Task := aTasks[i];
      if fTasks.FastLocateSorted(item, ndx) then
        inc(ndx); // always insert just after any existing timestamp
      fTasks.FastAddSorted(ndx, item);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TPendingTaskList.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
  begin
    fSafe.Lock;
    try
      result := fCount;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TPendingTaskList.NextPendingTask: RawByteString;
begin
  result := '';
  if (self = nil) or
     (fCount = 0) then
    exit;
  fSafe.Lock;
  try
    if fCount > 0 then
      if GetTimestamp >= fTask[0].Timestamp then
      begin
        result := fTask[0].Task;
        fTasks.FastDeleteSorted(0);
      end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TPendingTaskList.Clear;
begin
  if (self = nil) or
     (fCount = 0) then
    exit;
  fSafe.Lock;
  try
    fTasks.Clear;
  finally
    fSafe.UnLock;
  end;
end;


{ ************ Background Thread Processing }

{ TSynBackgroundThreadAbstract }

constructor TSynBackgroundThreadAbstract.Create(const aThreadName: RawUtf8;
  const OnBeforeExecute: TOnNotifyThread; const OnAfterExecute: TOnNotifyThread;
  CreateSuspended: boolean);
begin
  fProcessEvent := TEvent.Create(nil, false, false, '');
  fThreadName := aThreadName;
  fOnBeforeExecute := OnBeforeExecute;
  fOnAfterExecute := OnAfterExecute;
  inherited Create(CreateSuspended{$ifdef FPC}, 512 * 1024{$endif}); // DefaultStackSize=512KB
end;

{$ifndef HASTTHREADSTART}
procedure TSynBackgroundThreadAbstract.Start;
begin
  Resume;
end;
{$endif HASTTHREADSTART}

{$ifndef HASTTHREADTERMINATESET}
procedure TSynBackgroundThreadAbstract.Terminate;
begin
  inherited Terminate; // FTerminated := True
  TerminatedSet;
end;
{$endif HASTTHREADTERMINATESET}

procedure TSynBackgroundThreadAbstract.TerminatedSet;
begin
  fProcessEvent.SetEvent; // ExecuteLoop should handle Terminated flag
end;

procedure TSynBackgroundThreadAbstract.WaitForNotExecuting(maxMS: integer);
var
  endtix: Int64;
begin
  if fExecute = exRun then
  begin
    endtix := mormot.core.os.GetTickCount64 + maxMS;
    repeat
      SleepHiRes(1); // wait for Execute to finish
    until (fExecute <> exRun) or
          (mormot.core.os.GetTickCount64 >= endtix);
  end;
end;

function TSynBackgroundThreadAbstract.SleepOrTerminated(MS: cardinal): boolean;
var
  endtix: Int64;
begin
  result := true; // notify Terminated
  if Terminated then
    exit;
  if MS < 32 then
    // smaller than GetTickCount resolution (under Windows)
    SleepHiRes(MS)
  else
  begin
    endtix := mormot.core.os.GetTickCount64 + MS;
    repeat
      SleepHiRes(10);
      if Terminated then
        exit;
    until mormot.core.os.GetTickCount64 > endtix;
  end;
  result := Terminated; // abnormal delay expiration
end;

destructor TSynBackgroundThreadAbstract.Destroy;
begin
  if fExecute = exRun then
  begin
    Terminate;
    WaitForNotExecuting(100);
  end;
  inherited Destroy;
  FreeAndNil(fProcessEvent);
end;

procedure TSynBackgroundThreadAbstract.SetExecuteLoopPause(dopause: boolean);
begin
  if Terminated or
     (dopause = fExecuteLoopPause) or
     (fExecute = exFinished) then
    exit;
  fExecuteLoopPause := dopause;
  fProcessEvent.SetEvent; // notify Execute main loop
end;

procedure TSynBackgroundThreadAbstract.Execute;
begin
  try
    if fThreadName = '' then
      SetCurrentThreadName('%(%)', [self, pointer(self)])
    else
      SetCurrentThreadName('%', [fThreadName]);
    if Assigned(fOnBeforeExecute) then
      fOnBeforeExecute(self);
    try
      fExecute := exRun;
      while not Terminated do
        if fExecuteLoopPause then
          fProcessEvent.WaitFor(100)
        else
          ExecuteLoop;
    finally
      if Assigned(fOnAfterExecute) then
        fOnAfterExecute(self);
    end;
  finally
    fExecute := exFinished;
  end;
end;


{ TSynBackgroundThreadMethodAbstract }

constructor TSynBackgroundThreadMethodAbstract.Create(
  const aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUtf8;
  const OnBeforeExecute, OnAfterExecute: TOnNotifyThread);
begin
  fOnIdle := aOnIdle; // cross-platform may run Execute as soon as Create is called
  fCallerEvent := TEvent.Create(nil, false, false, '');
  fPendingProcessLock.Init;
  inherited Create(aThreadName, OnBeforeExecute, OnAfterExecute);
end;

destructor TSynBackgroundThreadMethodAbstract.Destroy;
begin
  SetPendingProcess(flagDestroying);
  fProcessEvent.SetEvent;  // notify terminated
  fCallerEvent.WaitFor(INFINITE); // wait for actual termination
  FreeAndNil(fCallerEvent);
  inherited Destroy;
  fPendingProcessLock.Done;
end;

function TSynBackgroundThreadMethodAbstract.GetPendingProcess: TSynBackgroundThreadProcessStep;
begin
  fPendingProcessLock.Lock;
  result := fPendingProcessFlag;
  fPendingProcessLock.UnLock;
end;

procedure TSynBackgroundThreadMethodAbstract.SetPendingProcess(
  State: TSynBackgroundThreadProcessStep);
begin
  fPendingProcessLock.Lock;
  fPendingProcessFlag := State;
  fPendingProcessLock.UnLock;
end;

procedure TSynBackgroundThreadMethodAbstract.ExecuteLoop;
var
  E: TObject;
begin
  case fProcessEvent.WaitFor(INFINITE) of
    wrSignaled:
      case GetPendingProcess of
        flagDestroying:
          begin
            fCallerEvent.SetEvent; // abort caller thread process
            Terminate; // forces Execute loop ending
            exit;
          end;
        flagStarted:
          if not Terminated then
            if fExecuteLoopPause then // pause -> try again later
              fProcessEvent.SetEvent
            else
            try
              fBackgroundException := nil;
              try
                if Assigned(fOnBeforeProcess) then
                  fOnBeforeProcess(self);
                try
                  Process;
                finally
                  if Assigned(fOnAfterProcess) then
                    fOnAfterProcess(self);
                end;
              except
                E := AcquireExceptionObject;
                if E.InheritsFrom(Exception) then
                  fBackgroundException := Exception(E);
              end;
            finally
              SetPendingProcess(flagFinished);
              fCallerEvent.SetEvent;
            end;
      end;
  end;
end;

function TSynBackgroundThreadMethodAbstract.AcquireThread: TSynBackgroundThreadProcessStep;
begin
  fPendingProcessLock.Lock;
  try
    result := fPendingProcessFlag;
    if result = flagIdle then
    begin
      // we just acquired the thread! congrats!
      fPendingProcessFlag := flagStarted; // atomic set "started" flag
      fCallerThreadID := ThreadID;
    end;
  finally
    fPendingProcessLock.UnLock;
  end;
end;

function TSynBackgroundThreadMethodAbstract.OnIdleProcessNotify(start: Int64): integer;
begin
  result := mormot.core.os.GetTickCount64 - start;
  if result < 0 then
    result := MaxInt; // should happen only under XP -> ignore
  if Assigned(fOnIdle) then
    fOnIdle(self, result);
end;

procedure TSynBackgroundThreadMethodAbstract.WaitForFinished(start: Int64;
  const onmainthreadidle: TNotifyEvent);
var
  E: Exception;
begin
  if (self = nil) or
     not (fPendingProcessFlag in [flagStarted, flagFinished]) then
    exit; // nothing to wait for
  try
    if Assigned(onmainthreadidle) then
    begin
      while fCallerEvent.WaitFor(100) = wrTimeout do
        onmainthreadidle(self);
    end
    else
    {$ifdef OSWINDOWS} // do process the OnIdle only if UI
    if Assigned(fOnIdle) then
    begin
      while fCallerEvent.WaitFor(100) = wrTimeout do
        OnIdleProcessNotify(start);
    end
    else
    {$endif OSWINDOWS}
      fCallerEvent.WaitFor(INFINITE);
    if fPendingProcessFlag <> flagFinished then
      ESynThread.CreateUtf8('%.WaitForFinished: flagFinished?', [self]);
    if fBackgroundException <> nil then
    begin
      E := fBackgroundException;
      fBackgroundException := nil;
      raise E; // raise background exception in the calling scope
    end;
  finally
    fParam := nil;
    fCallerThreadID := TThreadID(0);
    FreeAndNil(fBackgroundException);
    SetPendingProcess(flagIdle);
    if Assigned(fOnIdle) then
      fOnIdle(self, -1); // notify finished
  end;
end;

function TSynBackgroundThreadMethodAbstract.RunAndWait(OpaqueParam: pointer): boolean;
var
  start: Int64;
  ThreadID: TThreadID;
begin
  result := false;
  ThreadID := GetCurrentThreadId;
  if (self = nil) or
     (ThreadID = fCallerThreadID) then
    // avoid endless loop when waiting in same thread (e.g. UI + OnIdle)
    exit;
  // 1. wait for any previous request to be finished (should not happen often)
  if Assigned(fOnIdle) then
    fOnIdle(self, 0); // notify started
  start := mormot.core.os.GetTickCount64;
  repeat
    case AcquireThread of
      flagDestroying:
        exit;
      flagIdle:
        break; // we acquired the background thread
    end;
    case OnIdleProcessNotify(start) of // Windows.GetTickCount64 res is 10-16 ms
      0..20:
        SleepHiRes(0); // 10 microsec delay on POSIX
      21..100:
        SleepHiRes(1);
      101..900:
        SleepHiRes(5);
    else
      SleepHiRes(50);
    end;
  until false;
  // 2. process execution in the background thread
  fParam := OpaqueParam;
  fProcessEvent.SetEvent; // notify background thread for Call pending process
  WaitForFinished(start, nil); // wait for flagFinished, then set flagIdle
  result := true;
end;

function TSynBackgroundThreadMethodAbstract.GetOnIdleBackgroundThreadActive: boolean;
begin
  result := (self <> nil) and
            Assigned(fOnIdle) and
            (GetPendingProcess <> flagIdle);
end;


{ TSynBackgroundThreadEvent }

constructor TSynBackgroundThreadEvent.Create(
  const aOnProcess: TOnProcessSynBackgroundThread;
  const aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUtf8);
begin
  inherited Create(aOnIdle, aThreadName);
  fOnProcess := aOnProcess;
end;

procedure TSynBackgroundThreadEvent.Process;
begin
  if not Assigned(fOnProcess) then
    raise ESynThread.CreateUtf8('Invalid %.RunAndWait() call', [self]);
  fOnProcess(self, fParam);
end;


{ TSynBackgroundThreadMethod }

procedure TSynBackgroundThreadMethod.Process;
var
  Method: ^TThreadMethod;
begin
  if fParam = nil then
    raise ESynThread.CreateUtf8('Invalid %.RunAndWait() call', [self]);
  Method := fParam;
  Method^();
end;

procedure TSynBackgroundThreadMethod.RunAndWait(Method: TThreadMethod);
var
  Met: TMethod absolute Method;
begin
  inherited RunAndWait(@Met);
end;


{ TSynBackgroundThreadProcedure }

constructor TSynBackgroundThreadProcedure.Create(
  aOnProcess: TOnProcessSynBackgroundThreadProc;
  const aOnIdle: TOnIdleSynBackgroundThread; const aThreadName: RawUtf8);
begin
  inherited Create(aOnIdle, aThreadName);
  fOnProcess := aOnProcess;
end;

procedure TSynBackgroundThreadProcedure.Process;
begin
  if not Assigned(fOnProcess) then
    raise ESynThread.CreateUtf8('Invalid %.RunAndWait() call', [self]);
  fOnProcess(fParam);
end;


{ TSynBackgroundThreadProcess }

constructor TSynBackgroundThreadProcess.Create(const aThreadName: RawUtf8;
  const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  const aOnBeforeExecute, aOnAfterExecute: TOnNotifyThread;
  aStats: TSynMonitorClass; CreateSuspended: boolean);
begin
  if not Assigned(aOnProcess) then
    raise ESynException.CreateUtf8('%.Create(aOnProcess=nil)', [self]);
  if aStats <> nil then
    fStats := aStats.Create(aThreadName);
  fOnProcess := aOnProcess;
  fOnProcessMS := aOnProcessMS;
  if fOnProcessMS = 0 then
    fOnProcessMS := INFINITE; // wait until ProcessEvent.SetEvent or Terminated
  inherited Create(aThreadName, aOnBeforeExecute, aOnAfterExecute, CreateSuspended);
end;

destructor TSynBackgroundThreadProcess.Destroy;
begin
  if fExecute = exRun then
  begin
    Terminate;
    WaitForNotExecuting(10000); // expect the background task to be finished
  end;
  inherited Destroy;
  fStats.Free;
end;

procedure TSynBackgroundThreadProcess.ExecuteLoop;
var
  wait: TWaitResult;
begin
  wait := fProcessEvent.WaitFor(fOnProcessMS);
  if not Terminated and
     (wait in [wrSignaled, wrTimeout]) then
    if fExecuteLoopPause then // pause -> try again later
      fProcessEvent.SetEvent
    else
    try
      if fStats <> nil then
        fStats.ProcessStartTask;
      try
        fOnProcess(self, wait);
      finally
        if fStats <> nil then
          fStats.ProcessEnd;
      end;
    except
      on E: Exception do
      begin
        if fStats <> nil then
          fStats.ProcessErrorRaised(E);
        if Assigned(fOnException) then
          fOnException(E);
      end;
    end;
end;


{ TSynBackgroundTimer }

var
  ProcessSystemUse: TSystemUse;

constructor TSynBackgroundTimer.Create(const aThreadName: RawUtf8;
  const aOnBeforeExecute: TOnNotifyThread;
  const aOnAfterExecute: TOnNotifyThread; aStats: TSynMonitorClass);
begin
  fTasks.Init(TypeInfo(TSynBackgroundTimerTaskDynArray), fTask);
  fTaskLock.Init;
  fTaskLock.LockedBool[0] := false;
  inherited Create(aThreadName, EverySecond, 1000, aOnBeforeExecute, aOnAfterExecute, aStats);
end;

destructor TSynBackgroundTimer.Destroy;
begin
  if (ProcessSystemUse <> nil) and
     (ProcessSystemUse.Timer = self) then
    ProcessSystemUse.Timer := nil; // allows processing by another background timer
  inherited Destroy;
  fTaskLock.Done;
end;

const
  TIXPRECISION = 32; // GetTickCount64 resolution (for aOnProcessSecs=1)

procedure TSynBackgroundTimer.EverySecond(Sender: TSynBackgroundThreadProcess;
  Event: TWaitResult);
var
  tix: Int64;
  i, f, n: integer;
  t: ^TSynBackgroundTimerTask;
  todo: TSynBackgroundTimerTaskDynArray; // avoid lock contention
begin
  if (fTask = nil) or
     Terminated then
    exit;
  tix := mormot.core.os.GetTickCount64;
  n := 0;
  fTaskLock.Lock;
  try
    variant(fTaskLock.Padding[0]) := true; // = fTaskLock.LockedBool[0]
    try
      i := length(fTask) - 1;
      while i >= 0 do
      begin
        t := @fTask[i];
        if tix >= t^.NextTix then
        begin
          SetLength(todo, n + 1);
          todo[n] := t^;
          inc(n);
          t^.FIFO := nil; // now owned by todo[n].FIFO
          if integer(t^.Secs) = -1 then
            // from ExecuteOnce()
            fTasks.Delete(i)
          else
          begin
            // schedule for next time
            t^.NextTix := tix + ((t^.Secs * 1000) - TIXPRECISION);
            dec(i);
          end;
        end
        else
          dec(i);
      end;
    finally
      fTaskLock.UnLock;
    end;
    for i := 0 to n - 1 do
      with todo[i] do
        if FIFO <> nil then
          for f := 0 to length(FIFO) - 1 do
          try
            OnProcess(self, Event, FIFO[f]);
          except
          end
        else
        try
          OnProcess(self, Event, '');
        except
        end;
  finally
    fTaskLock.LockedBool[0] := false;
  end;
end;

function TSynBackgroundTimer.Find(const aProcess: TMethod): integer;
begin
  // caller should have made fTaskLock.Lock;
  for result := length(fTask) - 1 downto 0 do
    with TMethod(fTask[result].OnProcess) do
      if (Code = aProcess.Code) and
         (Data = aProcess.Data) then
        exit;
  result := -1;
end;

procedure TSynBackgroundTimer.Enable(
  const aOnProcess: TOnSynBackgroundTimerProcess; aOnProcessSecs: cardinal);
var
  task: TSynBackgroundTimerTask;
  found: integer;
begin
  if (self = nil) or
     Terminated or
     not Assigned(aOnProcess) then
    exit;
  if aOnProcessSecs = 0 then
  begin
    Disable(aOnProcess);
    exit;
  end;
  task.OnProcess := aOnProcess;
  task.Secs := aOnProcessSecs;
  task.NextTix := mormot.core.os.GetTickCount64 + (aOnProcessSecs * 1000 - TIXPRECISION);
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found >= 0 then
      fTask[found] := task
    else
      fTasks.Add(task);
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.Processing: boolean;
begin
  result := fTaskLock.LockedBool[0];
end;

procedure TSynBackgroundTimer.WaitUntilNotProcessing(timeoutsecs: integer);
var
  timeout: Int64;
begin
  if not Processing then
    exit;
  timeout := mormot.core.os.GetTickCount64 + timeoutsecs * 1000;
  repeat
    SleepHiRes(1);
  until not Processing or
        (mormot.core.os.GetTickcount64 > timeout);
end;

function TSynBackgroundTimer.ExecuteNow(
  const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  result := Add(aOnProcess, #0, true);
end;

function TSynBackgroundTimer.ExecuteOnce(
  const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  result := Assigned(aOnProcess) and Assigned(self);
  if not result then
    exit;
  Enable(aOnProcess, cardinal(-1));
  Add(aOnProcess, 'Once', true);
end;

function TSynBackgroundTimer.EnQueue(
  const aOnProcess: TOnSynBackgroundTimerProcess;
  const aMsg: RawUtf8; aExecuteNow: boolean): boolean;
begin
  result := Add(aOnProcess, aMsg, aExecuteNow);
end;

function TSynBackgroundTimer.EnQueue(
  const aOnProcess: TOnSynBackgroundTimerProcess; const aMsgFmt: RawUtf8;
  const Args: array of const; aExecuteNow: boolean): boolean;
var
  msg: RawUtf8;
begin
  FormatUtf8(aMsgFmt, Args, msg);
  result := Add(aOnProcess, msg, aExecuteNow);
end;

function TSynBackgroundTimer.Add(
  const aOnProcess: TOnSynBackgroundTimerProcess; const aMsg: RawUtf8;
  aExecuteNow: boolean): boolean;
var
  found: integer;
begin
  result := false;
  if (self = nil) or
     Terminated or
     not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found >= 0 then
    begin
      with fTask[found] do
      begin
        if aExecuteNow then
          NextTix := 0;
        if aMsg <> #0 then
          AddRawUtf8(FIFO, aMsg);
      end;
      if aExecuteNow then
        ProcessEvent.SetEvent;
      result := true;
    end;
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.DeQueue(
  const aOnProcess: TOnSynBackgroundTimerProcess; const aMsg: RawUtf8): boolean;
var
  found: integer;
begin
  result := false;
  if (self = nil) or
     Terminated or
     not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found >= 0 then
      with fTask[found] do
        result := DeleteRawUtf8(FIFO, FindRawUtf8(FIFO, aMsg));
  finally
    fTaskLock.UnLock;
  end;
end;

function TSynBackgroundTimer.Disable(
  const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
var
  found: integer;
begin
  result := false;
  if (self = nil) or
     Terminated or
     not Assigned(aOnProcess) then
    exit;
  fTaskLock.Lock;
  try
    found := Find(TMethod(aOnProcess));
    if found >= 0 then
    begin
      fTasks.Delete(found);
      result := true;
    end;
  finally
    fTaskLock.UnLock;
  end;
end;



{ TBlockingProcess }

constructor TBlockingProcess.Create(aTimeOutMs: integer; aSafe: PSynLocker);
begin
  inherited Create(nil, false, false, '');
  if aTimeOutMs <= 0 then
    fTimeOutMs := 3000
  else // never wait for ever
    fTimeOutMs := aTimeOutMs;
  fSafe := aSafe;
end;

constructor TBlockingProcess.Create(aTimeOutMs: integer);
begin
  fOwnedSafe := true;
  Create(aTimeOutMs, NewSynLocker);
end;

destructor TBlockingProcess.Destroy;
begin
  inherited Destroy;
  if fOwnedSafe then
    fSafe^.DoneAndFreeMem;
end;

function TBlockingProcess.WaitFor: TBlockingEvent;
begin
  fSafe^.Lock;
  try
    result := fEvent;
    if fEvent in [evRaised, evTimeOut] then
      exit;
    fEvent := evWaiting;
  finally
    fSafe^.UnLock;
  end;
  inherited WaitFor(fTimeOutMs);
  fSafe^.Lock;
  try
    if fEvent <> evRaised then
      fEvent := evTimeOut;
    result := fEvent;
  finally
    fSafe^.UnLock;
  end;
end;

function TBlockingProcess.WaitFor(TimeOutMS: integer): TBlockingEvent;
begin
  if TimeOutMS <= 0 then
    fTimeOutMs := 3000 // never wait for ever
  else
    fTimeOutMs := TimeOutMS;
  result := WaitFor;
end;

function TBlockingProcess.NotifyFinished(alreadyLocked: boolean): boolean;
begin
  result := false;
  if not alreadyLocked then
    fSafe^.Lock;
  try
    if fEvent in [evRaised, evTimeOut] then
      exit; // ignore if already notified
    fEvent := evRaised;
    SetEvent; // notify caller to unlock "WaitFor" method
    result := true;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TBlockingProcess.ResetInternal;
begin
  ResetEvent;
  fEvent := evNone;
end;

function TBlockingProcess.Reset: boolean;
begin
  fSafe^.Lock;
  try
    result := fEvent <> evWaiting;
    if result then
      ResetInternal;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TBlockingProcess.Lock;
begin
  fSafe^.Lock;
end;

procedure TBlockingProcess.Unlock;
begin
  fSafe^.Unlock;
end;


{ TBlockingProcessPoolItem }

procedure TBlockingProcessPoolItem.ResetInternal;
begin
  inherited ResetInternal; // set fEvent := evNone
  fCall := 0;
end;


{ TBlockingProcessPool }

constructor TBlockingProcessPool.Create(aClass: TBlockingProcessPoolItemClass);
begin
  inherited Create;
  if aClass = nil then
    fClass := TBlockingProcessPoolItem
  else
    fClass := aClass;
  fPool := TSynObjectListLocked.Create;
end;

const
  CALL_DESTROYING = -1;

destructor TBlockingProcessPool.Destroy;
var
  i: PtrInt;
  someWaiting: boolean;
begin
  fCallCounter := CALL_DESTROYING;
  someWaiting := false;
  for i := 0 to fPool.Count - 1 do
    with TBlockingProcessPoolItem(fPool.List[i]) do
      if Event = evWaiting then
      begin
        SetEvent; // release WaitFor (with evTimeOut)
        someWaiting := true;
      end;
  if someWaiting then
    SleepHiRes(10); // propagate the pending evTimeOut to the WaitFor threads
  fPool.Free;
  inherited;
end;

function TBlockingProcessPool.NewProcess(aTimeOutMs: integer): TBlockingProcessPoolItem;
var
  i: integer;
  p: ^TBlockingProcessPoolItem;
begin
  result := nil;
  if fCallCounter = CALL_DESTROYING then
    exit;
  if aTimeOutMs <= 0 then
    aTimeOutMs := 3000; // never wait for ever
  fPool.Safe.Lock;
  try
    p := pointer(fPool.List);
    for i := 1 to fPool.Count do
      if p^.Call = 0 then
      begin
        result := p^; // found a non-used entry
        result.fTimeOutMs := aTimeOutMs;
        break;
      end
      else
        inc(p);
    if result = nil then
    begin
      result := fClass.Create(aTimeOutMs);
      fPool.Add(result);
    end;
    inc(fCallCounter); // 1,2,3,...
    result.fCall := fCallCounter;
  finally
    fPool.Safe.UnLock;
  end;
end;

function TBlockingProcessPool.FromCall(call: TBlockingProcessPoolCall;
  locked: boolean): TBlockingProcessPoolItem;
var
  i: integer;
  p: ^TBlockingProcessPoolItem;
begin
  result := nil;
  if (fCallCounter = CALL_DESTROYING) or
     (call <= 0) then
    exit;
  fPool.Safe.Lock;
  try
    p := pointer(fPool.List);
    for i := 1 to fPool.Count do
      if p^.call = call then
      begin
        result := p^;
        if locked then
          result.Lock;
        exit;
      end
      else
        inc(p);
  finally
    fPool.Safe.UnLock;
  end;
end;


{ ************ Parallel Execution in a Thread Pool }

{ TSynParallelProcessThread }

procedure TSynParallelProcessThread.Process;
begin
  if not Assigned(fMethod) then
    exit;
  fMethod(fIndexStart, fIndexStop);
  fMethod := nil;
end;

procedure TSynParallelProcessThread.Start(
  const Method: TOnSynParallelProcess; IndexStart, IndexStop: integer);
begin
  fMethod := Method;
  fIndexStart := IndexStart;
  fIndexStop := IndexStop;
  fProcessEvent.SetEvent; // notify execution
end;


{ TSynParallelProcess }

constructor TSynParallelProcess.Create(ThreadPoolCount: integer;
  const ThreadName: RawUtf8; const OnBeforeExecute, OnAfterExecute: TOnNotifyThread;
  MaxThreadPoolCount: integer);
var
  i: PtrInt;
begin
  inherited Create;
  if ThreadPoolCount < 0 then
    raise ESynThread.CreateUtf8('%.Create(%,%)',
      [Self, ThreadPoolCount, ThreadName]);
  if ThreadPoolCount > MaxThreadPoolCount then
    ThreadPoolCount := MaxThreadPoolCount;
  fThreadPoolCount := ThreadPoolCount;
  fThreadName := ThreadName;
  SetLength(fPool, fThreadPoolCount);
  for i := 0 to fThreadPoolCount - 1 do
    fPool[i] := TSynParallelProcessThread.Create(nil,
      FormatUtf8('%#%/%', [fThreadName, i + 1, fThreadPoolCount]),
      OnBeforeExecute, OnAfterExecute);
end;

destructor TSynParallelProcess.Destroy;
begin
  ObjArrayClear(fPool);
  inherited;
end;

procedure TSynParallelProcess.ParallelRunAndWait(const Method: TOnSynParallelProcess;
  MethodCount: integer; const OnMainThreadIdle: TNotifyEvent);
var
  use, t, n, perthread: integer;
  error: RawUtf8;
begin
  if (MethodCount <= 0) or
     not Assigned(Method) then
    exit;
  if not Assigned(OnMainThreadIdle) then
    if (self = nil) or
       (MethodCount = 1) or
       (fThreadPoolCount = 0) then
    begin
      Method(0, MethodCount - 1); // no need (or impossible) to use background thread
      exit;
    end;
  use := MethodCount;
  t := fThreadPoolCount;
  if not Assigned(OnMainThreadIdle) then
    inc(t); // include current thread
  if use > t then
    use := t;
  try
    // start secondary threads
    perthread := MethodCount div use;
    if perthread = 0 then
      use := 1;
    n := 0;
    for t := 0 to use - 2 do
    begin
      repeat
        case fPool[t].AcquireThread of
          flagDestroying:
            // should not happen
            raise ESynThread.CreateUtf8(
              '%.ParallelRunAndWait [%] destroying', [self, fPool[t].fThreadName]);
          flagIdle:
            // acquired (should always be the case)
            break;
        end;
        SleepHiRes(1);
        if Assigned(OnMainThreadIdle) then
          OnMainThreadIdle(self);
      until false;
      fPool[t].start(Method, n, n + perthread - 1);
      inc(n, perthread);
      inc(fParallelRunCount);
    end;
    // run remaining items in the current/last thread
    if n < MethodCount then
    begin
      if Assigned(OnMainThreadIdle) then
      begin
        fPool[use - 1].start(Method, n, MethodCount - 1);
        inc(use); // also wait for the last thread
      end
      else
        Method(n, MethodCount - 1);
      inc(fParallelRunCount);
    end;
  finally
    // wait for the process to finish
    for t := 0 to use - 2 do
    try
      fPool[t].WaitForFinished(0, OnMainThreadIdle);
    except
      on E: Exception do
        // gather all errors from background thread
        error := FormatUtf8('% % on thread % [%]',
          [{%H-}error, E, fPool[t].fThreadName, E.Message]);
    end;
    if error <> '' then
      raise ESynThread.CreateUtf8('%.ParallelRunAndWait: %',
        [self, error]);
  end;
end;


{ ************ Server Process Oriented Thread Pool }

{ TSynThread }

constructor TSynThread.Create(CreateSuspended: boolean);
begin
  {$ifdef FPC}
  inherited Create(CreateSuspended, 512 * 1024); // DefaultSizeStack=512KB
  {$else}
  inherited Create(CreateSuspended);
  {$endif FPC}
end;

function TSynThread.SleepOrTerminated(MS: cardinal): boolean;
var
  endtix: Int64;
begin
  result := true; // notify Terminated
  if Terminated then
    exit;
  if MS < 32 then
  begin
    // smaller than GetTickCount resolution (under Windows)
    SleepHiRes(MS);
    if Terminated then
      exit;
  end
  else
  begin
    endtix := mormot.core.os.GetTickCount64 + MS;
    repeat
      SleepHiRes(10);
      if Terminated then
        exit;
    until mormot.core.os.GetTickCount64 > endtix;
  end;
  result := false; // abnormal delay expiration
end;

procedure TSynThread.DoTerminate;
begin
  if Assigned(fStartNotified) and
     Assigned(fOnThreadTerminate) then
  begin
    fOnThreadTerminate(self);
    fStartNotified := nil;
  end;
  inherited DoTerminate;
end;

{$ifndef HASTTHREADSTART}
procedure TSynThread.start;
begin
  Resume;
end;
{$endif HASTTHREADSTART}


{ TSynThreadPool }

{$ifdef USE_WINIOCP}
constructor TSynThreadPool.Create(NumberOfThreads: integer;
  aOverlapHandle: THandle; const aName: RawUtf8);
{$else}
constructor TSynThreadPool.Create(NumberOfThreads: integer;
  aQueuePendingContext: boolean; const aName: RawUtf8);
{$endif USE_WINIOCP}
var
  i: PtrInt;
begin
  if NumberOfThreads = 0 then
    NumberOfThreads := 1
  else if cardinal(NumberOfThreads) > THREADPOOL_MAXTHREADS then
    NumberOfThreads := THREADPOOL_MAXTHREADS;
  fName := aName;
  if fName = '' then
    fName := StringReplaceAll(StringReplaceAll(ToText(ClassType),
      'Pool', ''), 'Thread', '');
  // create IO completion port to queue the HTTP requests
  {$ifdef USE_WINIOCP}
  fRequestQueue := CreateIoCompletionPort(aOverlapHandle, 0, nil, NumberOfThreads);
  if fRequestQueue = INVALID_HANDLE_VALUE then
    fRequestQueue := 0;
  if fRequestQueue = 0 then
    exit;
  {$else}
  InitializeCriticalSection(fSafe);
  fQueuePendingContext := aQueuePendingContext;
  {$endif USE_WINIOCP}
  // now create the worker threads
  fWorkThreadCount := NumberOfThreads;
  SetLength(fWorkThread, fWorkThreadCount);
  for i := 0 to fWorkThreadCount - 1 do
    fWorkThread[i] := TSynThreadPoolWorkThread.Create(Self);
end;

destructor TSynThreadPool.Destroy;
var
  i: PtrInt;
  endtix: Int64;
begin
  fTerminated := true; // fWorkThread[].Execute will check this flag
  try
    {$ifdef USE_WINIOCP}
    // notify the threads we are shutting down
    for i := 0 to fWorkThreadCount - 1 do
      PostQueuedCompletionStatus(fRequestQueue, 0, nil, nil);
    {$else}
    // notify the threads we are shutting down using the event
    for i := 0 to fWorkThreadCount - 1 do
      fWorkThread[i].fEvent.SetEvent;
    // cleanup now any pending task (e.g. THttpServerSocket instance)
    for i := 0 to fPendingContextCount - 1 do
      TaskAbort(fPendingContext[i]); // not needed with WinIOCP
    {$endif USE_WINIOCP}
    // wait for threads to finish, with 30 seconds TimeOut
    endtix := GetTickCount64 + 30000;
    while (fRunningThreads > 0) and
          (GetTickCount64 < endtix) do
      SleepHiRes(5);
    for i := 0 to fWorkThreadCount - 1 do
      fWorkThread[i].Free;
  finally
    {$ifdef USE_WINIOCP}
    CloseHandle(fRequestQueue);
    {$else}
    DeleteCriticalSection(fSafe);
    {$endif USE_WINIOCP}
  end;
  inherited Destroy;
end;

function TSynThreadPool.Push(aContext: pointer; aWaitOnContention: boolean): boolean;

{$ifdef USE_WINIOCP}

  function Enqueue: boolean;
  begin
    // IOCP has its own queue
    result := PostQueuedCompletionStatus(fRequestQueue, 0, nil, aContext);
  end;

{$else}

  function Enqueue: boolean;
  var
    i, n: integer;
    found: TSynThreadPoolWorkThread;
    thread: ^TSynThreadPoolWorkThread;
  begin
    result := false; // queue is full
    found := nil;
    EnterCriticalsection(fSafe);
    try
      thread := pointer(fWorkThread);
      for i := 1 to fWorkThreadCount do
        if thread^.fProcessingContext = nil then
        begin
          found := thread^;
          found.fProcessingContext := aContext;
          result := true; // found one available thread
          exit;
        end
        else
          inc(thread);
      if not fQueuePendingContext then
        exit;
      n := fPendingContextCount;
      if n + fWorkThreadCount > QueueLength then
        exit; // too many connection limit reached (see QueueIsFull)
      if n = length(fPendingContext) then
        SetLength(fPendingContext, n + n shr 3 + 64);
      fPendingContext[n] := aContext;
      inc(fPendingContextCount);
      result := true; // added in pending queue
    finally
      LeaveCriticalsection(fSafe);
      if found <> nil then
        found.fEvent.SetEvent; // rather notify outside of the fSafe lock
    end;
  end;

{$endif USE_WINIOCP}

var
  tix, starttix, endtix: Int64;
begin
  result := false;
  if (self = nil) or
     fTerminated then
    exit;
  result := Enqueue;
  if result then
    exit;
  inc(fContentionCount);
  if (fContentionAbortDelay > 0) and
     aWaitOnContention then
  begin
    tix := GetTickCount64;
    starttix := tix;
    endtix := tix + fContentionAbortDelay; // default 5 sec
    repeat // during this delay, no new connection is ACCEPTed
      if tix - starttix < 50 then // wait for an available slot in the queue
        SleepHiRes(1)
      else
        SleepHiRes(10);
      tix := GetTickCount64;
      if fTerminated then
        exit;
      if Enqueue then
      begin
        result := true; // thread pool acquired or queued the client sock
        break;
      end;
    until fTerminated or
          (tix > endtix);
    inc(fContentionTime, tix - starttix);
  end;
  if not result then
    inc(fContentionAbortCount);
end;

{$ifndef USE_WINIOCP}

function TSynThreadPool.GetPendingContextCount: integer;
begin
  result := 0;
  if (self = nil) or
     fTerminated or
     (fPendingContext = nil) then
    exit;
  EnterCriticalsection(fSafe);
  try
    result := fPendingContextCount;
  finally
    LeaveCriticalsection(fSafe);
  end;
end;

function TSynThreadPool.QueueIsFull: boolean;
begin
  result := fQueuePendingContext and
    (GetPendingContextCount + fWorkThreadCount > QueueLength);
end;

function TSynThreadPool.PopPendingContext: pointer;
begin
  result := nil;
  if (self = nil) or
     fTerminated or
     (fPendingContext = nil) then
    exit;
  EnterCriticalsection(fSafe);
  try
    if fPendingContextCount > 0 then
    begin
      result := fPendingContext[0];
      dec(fPendingContextCount);
      MoveFast(fPendingContext[1], fPendingContext[0], fPendingContextCount *
        SizeOf(pointer));
      if fPendingContextCount = 128 then
        SetLength(fPendingContext, 128); // small queue when congestion is resolved
    end;
  finally
    LeaveCriticalsection(fSafe);
  end;
end;

function TSynThreadPool.QueueLength: integer;
begin
  result := 10000; // lazy high value
end;

{$endif USE_WINIOCP}

function TSynThreadPool.NeedStopOnIOError: boolean;
begin
  result := True;
end;

procedure TSynThreadPool.TaskAbort(aContext: Pointer);
begin
end;


{ TSynThreadPoolWorkThread }

constructor TSynThreadPoolWorkThread.Create(Owner: TSynThreadPool);
begin
  fOwner := Owner; // ensure it is set ASAP: on Linux, Execute raises immediately
  fOnThreadTerminate := Owner.fOnThreadTerminate;
  {$ifndef USE_WINIOCP}
  fEvent := TEvent.Create(nil, false, false, '');
  {$endif USE_WINIOCP}
  inherited Create({suspended=}false);
end;

destructor TSynThreadPoolWorkThread.Destroy;
begin
  inherited Destroy;
  {$ifndef USE_WINIOCP}
  fEvent.Free;
  {$endif USE_WINIOCP}
end;

procedure TSynThreadPoolWorkThread.DoTask(Context: pointer);
begin
  try
    fOwner.Task(Self, Context);
  except
    on Exception do  // intercept any exception and let the thread continue
      inc(fOwner.fExceptionsCount);
  end;
end;

procedure TSynThreadPoolWorkThread.Execute;
var
  ctxt: pointer;
  {$ifdef USE_WINIOCP}
  dum1: cardinal;
  dum2: PtrUInt;
  {$endif USE_WINIOCP}
begin
  if fOwner <> nil then
  try
    fThreadNumber := InterlockedIncrement(fOwner.fRunningThreads);
    NotifyThreadStart(self);
    repeat
      {$ifdef USE_WINIOCP}
      if (not GetQueuedCompletionStatus(fOwner.fRequestQueue,
           dum1, dum2, ctxt, INFINITE) and
          fOwner.NeedStopOnIOError) or
         fOwner.fTerminated then
        break;
      if ctxt <> nil then
        DoTask(ctxt);
      {$else}
      fEvent.WaitFor(INFINITE);
      if fOwner.fTerminated then
        break;
      EnterCriticalSection(fOwner.fSafe);
      ctxt := fProcessingContext;
      LeaveCriticalSection(fOwner.fSafe);
      if ctxt <> nil then
      begin
        repeat
          DoTask(ctxt);
          ctxt := fOwner.PopPendingContext; // unqueue any pending context
        until ctxt = nil;
        EnterCriticalSection(fOwner.fSafe);
        fProcessingContext := nil; // indicates this thread is now available
        LeaveCriticalSection(fOwner.fSafe);
      end;
     {$endif USE_WINIOCP}
    until fOwner.fTerminated or
          Terminated;
  finally
    LockedDec32(@fOwner.fRunningThreads);
  end;
end;

procedure TSynThreadPoolWorkThread.NotifyThreadStart(Sender: TSynThread);
begin
  if Sender = nil then
    raise ESynThread.CreateUtf8('%.NotifyThreadStart(nil)', [self]);
  if Assigned(fOwner.fOnThreadStart) and
     not Assigned(Sender.fStartNotified) then
  begin
    fOwner.fOnThreadStart(Sender);
    Sender.fStartNotified := self;
  end;
  if CurrentThreadName[0] = #0 then
    SetCurrentThreadName('Pool%-%', [fThreadNumber, fOwner.fName]);
end;


end.

