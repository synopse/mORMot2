/// REpresentation State Tranfer (REST) Core Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.core;

{
  *****************************************************************************

   Shared Types and Definitions for Abstract REST Process
    - Customize REST Execution
    - TRestBackgroundTimer for Multi-Thread Process
    - TRestRunThreads Multi-Threading Process of a REST instance
    - TRest Abstract Parent Class
    - RESTful Authentication Support
    - TRestUriParams REST URI Definitions
    - TRestUriContext REST Parent Process on Server Side
    - TRestThread Background Process of a REST instance
    - TOrmHistory/TOrmTableDeleted Modifications Tracked Persistence

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.jwt,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core, // for TOrm and IRestOrm
  mormot.soa.core,
  mormot.db.core;


{ ************ Customize REST Execution }

type
  /// all commands which may be executed by TRestServer.Uri() method
  // - execNone is just used as default to identify unexpected execution logic
  // - execSoaByMethod for method-based services
  // - execSoaByInterface for interface-based services
  // - execOrmGet for ORM reads i.e. Retrieve*() methods
  // - execOrmWrite for ORM writes i.e. Add Update Delete TransactionBegin
  // Commit Rollback methods
  TRestServerUriContextCommand = (
    execNone,
    execSoaByMethod,
    execSoaByInterface,
    execOrmGet,
    execOrmWrite);

  /// how a TRest class may execute read or write operations
  // - used e.g. for TRestServer.AcquireWriteMode or
  // TRestServer.AcquireExecutionMode/AcquireExecutionLockedTimeOut
  TRestServerAcquireMode = (
    amUnlocked,
    amLocked,
    amBackgroundThread,
    amBackgroundOrmSharedThread,
    amMainThread);

  /// used to store the execution parameters for a TRest instance
  TRestAcquireExecution = class(TSynLocked)
  public
    /// how read or write operations will be executed
    Mode: TRestServerAcquireMode;
    /// ms delay before failing to acquire the lock
    LockedTimeOut: cardinal;
    /// background thread instance (if any)
    Thread: TSynBackgroundThreadMethod;
    /// finalize the memory structure, and the associated background thread
    destructor Destroy; override;
  end;

  /// define how a TRest class may execute its ORM and SOA operations
  // - by definition, [execNone] if no valid index
  TRestAcquireExecutions = array[succ(low(TRestServerUriContextCommand)) ..
    high(TRestServerUriContextCommand)] of TRestAcquireExecution;

  /// a genuine identifier for a given client connection on server side
  // - see also THttpServerConnectionID as defined in mormot.net.http: may map
  // the http.sys ID, or a genuine 31-bit value from increasing sequence
  TRestConnectionID = Int64;


const
  CONTENT_TYPE_WEBFORM: PAnsiChar = 'APPLICATION/X-WWW-FORM-URLENCODED';
  CONTENT_TYPE_MULTIPARTFORM: PAnsiChar = 'MULTIPART/FORM-DATA';


{ ************ TRestBackgroundTimer for Multi-Thread Process }

type
  TRest = class;

  /// optionally called after TRest.AsyncRedirect background execution
  // - to retrieve any output result value, as JSON-encoded content
  // - as used in TRestBackgroundTimer.AsyncBackgroundExecute protected method
  TOnAsyncRedirectResult = procedure(const aMethod: TInterfaceMethod;
    const aInstance: IInvokable; const aParams, aResult: RawUtf8) of object;

  /// TThread able to run one or several tasks at a periodic pace, or do
  // asynchronous interface or batch execution, with proper TRest integration
  // - used e.g. by TRest.TimerEnable/AsyncRedirect/AsyncBatchStart methods
  // - TRest.BackgroundTimer will define one instance, but you may create
  // other dedicated instances to instantiate separated threads
  TRestBackgroundTimer = class(TSynBackgroundTimer)
  protected
    fRest: TRest;
    fBackgroundBatch: TRestBatchLockedDynArray;
    fBackgroundInterning: array of TRawUtf8Interning;
    fBackgroundInterningMaxRefCount: integer;
    fBackgroundInterningSafe: TLightLock; // paranoid lock
    procedure SystemUseBackgroundExecute(Sender: TSynBackgroundTimer;
      const Msg: RawUtf8);
    // used by AsyncRedirect/AsyncBatch/AsyncInterning
    function AsyncBatchIndex(aTable: TOrmClass): PtrInt;
    function AsyncBatchLocked(aTable: TOrmClass;
      out aBatch: TRestBatchLocked): boolean;
    procedure AsyncBatchUnLock(aBatch: TRestBatchLocked);
    procedure AsyncBatchExecute(Sender: TSynBackgroundTimer;
      const Msg: RawUtf8);
    procedure AsyncBackgroundExecute(Sender: TSynBackgroundTimer;
      const Msg: RawUtf8);
    procedure AsyncBackgroundInterning(Sender: TSynBackgroundTimer;
      const Msg: RawUtf8);
  public
    /// initialize the thread for a periodic task processing
    constructor Create(aRest: TRest; const aThreadName: RawUtf8 = '';
      aStats: TSynMonitorClass = nil); reintroduce; virtual;
    /// finalize the thread
    destructor Destroy; override;
    /// define asynchronous execution of interface methods in a background thread
    // - this method implements any interface via a fake class, which will
    // redirect all methods calls into calls of another interface, but as a FIFO
    // in a background thread, shared with TimerEnable/TimerDisable process
    // - parameters will be serialized and stored as JSON in the queue
    // - by design, only procedure methods without any output parameters are
    // allowed, since their execution will take place asynchronously
    // - of course, a slight delay is introduced in aDestinationInterface
    // methods execution, but the main process thread is not delayed any more,
    // and is free from potential race conditions
    // - the returned fake aCallbackInterface should be freed before TRest
    // is destroyed, to release the redirection resources
    // - it is an elegant resolution to the most difficult implementation
    // problem of SOA callbacks, which is to avoid race condition on reentrance,
    // e.g. if a callback is run from a thread, and then the callback code try
    // to execute something in the context of the initial thread, protected
    // by a critical section (mutex)
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    /// define asynchronous execution of interface methods in a background thread
    // - this method implements any interface via a fake class, which will
    // redirect all methods calls into calls of another interface, but as a FIFO
    // in a background thread, shared with TimerEnable/TimerDisable process
    // - parameters will be serialized and stored as JSON in the queue
    // - by design, only procedure methods without any output parameters are
    // allowed, since their execution will take place asynchronously
    // - of course, a slight delay is introduced in aDestinationInterface
    // methods execution, but the main process thread is not delayed any more,
    // and is free from potential race conditions
    // - the returned fake aCallbackInterface should be freed before TRest
    // is destroyed, to release the redirection resources
    // - it is an elegant resolution to the most difficult implementation
    // problem of SOA callbacks, which is to avoid race condition on reentrance,
    // e.g. if a callback is run from a thread, and then the callback code try
    // to execute something in the context of the initial thread, protected
    // by a critical section (mutex)
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    /// prepare an asynchronous ORM BATCH process, executed in a background thread
    // - will initialize a TRestBatch and call TimerEnable to initialize the
    // background thread, following the given processing period (in seconds),
    // or the TRestBatch.Count threshold to call BatchSend
    // - actual REST/CRUD commands will take place via AsyncBatchAdd,
    // AsyncBatchUpdate and AsyncBatchDelete methods
    // - only a single AsyncBatch() call per Table is allowed at a time, unless
    // AsyncBatchStop method is used to flush the current asynchronous BATCH
    // - using a BATCH in a dedicated thread will allow very fast background
    // asynchronous process of ORM methods, sufficient for most use cases
    function AsyncBatchStart(Table: TOrmClass;
      SendSeconds: integer; PendingRowThreshold: integer = 500;
      AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJson]): boolean;
    /// finalize asynchronous ORM BATCH process, executed in a background thread
    // - should have been preceded by a call to AsyncBatch(), or returns false
    // - Table=nil will release all existing batch instances
    function AsyncBatchStop(Table: TOrmClass): boolean;
    /// create a new ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around TRestBatch.Add() sent in the Timer thread,
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsyncBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsyncBatchAdd() with an already pre-computed
    // JSON object
    // - is a wrapper around TRestBatch.RawAdd() sent in the Timer thread,
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsyncBatchRawAdd(Table: TOrmClass; const SentData: RawUtf8): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsyncBatchAdd() with an already pre-computed
    // JSON object, as stored in a TJsonWriter instance
    // - is a wrapper around TRestBatch.RawAppend.AddNoJsonEscape(SentData)
    // in the Timer thread
    // - this method is thread-safe
    procedure AsyncBatchRawAppend(Table: TOrmClass; SentData: TJsonWriter);
    /// update an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Update() sent in the Timer thread
    // - this method is thread-safe
    function AsyncBatchUpdate(Value: TOrm;
      const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// delete an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsyncBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Delete() sent in the Timer thread
    // - this method is thread-safe
    function AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;
    /// allows background garbage collection of specified RawUtf8 interning
    // - will run Interning.Clean(2) every 5 minutes by default
    // - set InterningMaxRefCount=0 to disable process of the Interning instance
    procedure AsyncInterning(Interning: TRawUtf8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    /// direct access to the TRest instance owner
    property Rest: TRest
      read fRest;
    /// direct access to the background thread TRestBatch instances
    property BackgroundBatch: TRestBatchLockedDynArray
      read fBackgroundBatch;
  published
    /// the identifier of the thread, as logged
    property Name: RawUtf8
      read fThreadName;
  end;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

  TSqlRestServerUriContextCommand = TRestServerUriContextCommand;
  TSqlRestServerAcquireMode       = TRestServerAcquireMode;
  TSqlRestAcquireExecution        = TRestAcquireExecution;
  TSqlRestBackgroundTimer         = TRestBackgroundTimer;

{$endif PUREMORMOT2}


{ ************ TRestRunThreads Multi-Threading Process of a REST instance }

  /// access to the Multi-Threading process of a TRest instance
  TRestRunThreads = class(TObjectOSLock)
  protected
    fOwner: TRest;
    fBackgroundTimer: TRestBackgroundTimer;
    fShutdown: boolean;
  public
    /// initialize the threading process
    constructor Create(aOwner: TRest); reintroduce;
    /// notify that no new registration is allowed
    procedure Shutdown;
    /// finalize the threading process
    destructor Destroy; override;
    /// allows to safely execute a processing method in a background thread
    // - returns a TSynBackgroundThreadMethod instance, ready to execute any
    // background task via its RunAndWait() method
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewBackgroundThreadMethod(const Format: RawUtf8;
      const Args: array of const): TSynBackgroundThreadMethod;
    /// allows to safely execute a process at a given pace
    // - returns a TSynBackgroundThreadProcess instance, ready to execute the
    // supplied aOnProcess event in a loop, as aOnProcessMS periodic task
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewBackgroundThreadProcess(
      const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
      const Format: RawUtf8; const Args: array of const;
      aStats: TSynMonitorClass=nil): TSynBackgroundThreadProcess;
    /// allows to safely execute a process in parallel
    // - returns a TSynParallelProcess instance, ready to execute any task
    // in parrallel in a thread-pool given by ThreadCount
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewParallelProcess(ThreadCount: integer; const Format: RawUtf8;
      const Args: array of const): TSynParallelProcess;
    /// define a task running on a periodic number of seconds in a background thread
    // - could be used to run background maintenance or monitoring tasks on
    // this TRest instance, at a low pace (typically every few minutes)
    // - will instantiate and run a shared TSynBackgroundTimer instance for this
    // TRest, so all tasks will share the very same thread
    // - you can run BackgroundTimer.EnQueue or ExecuteNow methods to implement
    // a FIFO queue, or force immediate execution of the process
    // - will call BeginCurrentThread/EndCurrentThread as expected e.g. by logs
    function TimerEnable(const aOnProcess: TOnSynBackgroundTimerProcess;
      aOnProcessSecs: cardinal): TRestBackgroundTimer;
    /// undefine a task running on a periodic number of seconds
    // - should have been registered by a previous call to TimerEnable() method
    // - returns true on success, false if the supplied task was not registered
    function TimerDisable(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// execute once a task in the background, without waiting for it
    function Once(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    /// will gather CPU and RAM information in a background thread
    // - you can specify the update frequency, in seconds
    // - access to the information via the returned instance, which maps
    // the TSystemUse.Current class function
    // - do nothing if global TSystemUse.Current was already assigned
    function SystemUseTrack(periodSec: integer = 10): TSystemUse;
    /// low-level access with optional initialization of the associated timer
    // - this function is thread-safe
    function EnsureBackgroundTimerExists: TRestBackgroundTimer;
    /// you can call this method in TThread.Execute to ensure that
    // the thread will be taken into account during process
    // - this method will redirect TRestServer.OnBeginCurrentThread
    procedure BeginCurrentThread(Sender: TThread);
    /// you can call this method just before a thread is finished to ensure
    // e.g. that the associated external DB connection will be released
    // - this method will redirect TRestServer.OnEndCurrentThread
    procedure EndCurrentThread(Sender: TThread);
    /// define asynchronous execution of interface methods in a background thread
    // - this class allows to implements any interface via a fake class, which will
    // redirect all methods calls into calls of another interface, but as a FIFO
    // in a background thread, shared with TimerEnable/TimerDisable process
    // - it is an elegant resolution to the most difficult implementation
    // problem of SOA callbacks, which is to avoid race condition on reentrance,
    // e.g. if a callback is run from a thread, and then the callback code try
    // to execute something in the context of the initial thread, protected
    // by a critical section (mutex)
    // - is a wrapper around BackgroundTimer.AsyncRedirect()
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    /// define asynchronous execution of interface methods in a background thread
    // - this class allows to implements any interface via a fake class, which will
    // redirect all methods calls into calls of another interface, but as a FIFO
    // in a background thread, shared with TimerEnable/TimerDisable process
    // - it is an elegant resolution to the most difficult implementation
    // problem of SOA callbacks, which is to avoid race condition on reentrance,
    // e.g. if a callback is run from a thread, and then the callback code try
    // to execute something in the context of the initial thread, protected
    // by a critical section (mutex)
    // - is a wrapper around BackgroundTimer.AsyncRedirect()
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    /// allows background garbage collection of specified RawUtf8 interning
    // - will run Interning.Clean(2) every 5 minutes by default
    // - set InterningMaxRefCount=0 to disable process of the Interning instance
    // - note that InterningMaxRefCount and PeriodMinutes parameters (if not 0),
    // are common for all TRawUtf8Interning instances (the latest value wins)
    // - you may e.g. run the following to clean up TDocVariant interned RawUtf8:
    // ! aRest.Run.AsyncInterning(DocVariantType.InternNames);
    // ! aRest.Run.AsyncInterning(DocVariantType.InternValues);
    procedure AsyncInterning(Interning: TRawUtf8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    /// define redirection of interface methods calls in one or several instances
    // - this class allows to implements any interface via a fake class, which
    // will redirect all methods calls to one or several other interfaces
    // - returned aCallbackInterface will redirect all its methods (identified
    // by aGuid) into an internal list handled by IMultiCallbackRedirect.Redirect
    // - typical use is thefore:
    // ! fSharedCallback: IMyService;
    // ! fSharedCallbacks: IMultiCallbackRedirect;
    // ! ...
    // !   if fSharedCallbacks = nil then
    // !   begin
    // !     fSharedCallbacks := aRest.Run.MultiRedirect(IMyService, fSharedCallback);
    // !     aServices.SubscribeForEvents(fSharedCallback);
    // !   end;
    // !   fSharedCallbacks.Redirect(TMyCallback.Create,[]);
    // !   // now each time fSharedCallback receive one event, all callbacks
    // !   // previously registered via Redirect() will receive it
    // ! ...
    // !   fSharedCallbacks := nil; // will stop redirection
    // !                            // and unregister callbacks, if needed
    function MultiRedirect(const aGuid: TGuid; out aCallbackInterface;
      aCallBackUnRegisterNeeded: boolean = true): IMultiCallbackRedirect; overload;
    /// low-level access to the associated timer
    // - may contain nil if EnsureBackgroundTimerExists has not yet been called
    property BackgroundTimer: TRestBackgroundTimer
      read fBackgroundTimer;
  end;


{ ************ TRest Abstract Parent Class }

  /// Exception class raised on TRest issues
  ERestException = class(ESynException);

  /// class-reference type (metaclass) of a TRest kind
  TRestClass = class of TRest;

  /// a dynamic array of TRest instances
  TRestDynArray = array of TRest;

  /// a dynamic array of TRest instances, owning the instances
  TRestObjArray = array of TRest;

  /// abstract REpresentational State Transfer (REST) client/server class
  // - see Orm: IRestOrm, Services: TServiceContainer and Run: TRestRunThreads
  // main properties for its actual REST-oriented process
  // - in PUREMORMOT2 mode, all direct ORM or threading methods are hidden
  // - is a TInterfaceResolver so is able to resolve IRestOrm
  // - do NOT use this abstract class, but one of its fully implemented children
  TRest = class(TInterfaceResolver)
  protected
    fOrm: IRestOrm;
    fOrmInstance: TRestOrmParent; // is a TRestOrm from mormot.orm.rest.pas
    fModel: TOrmModel;
    fServices: TServiceContainer;
    fRun: TRestRunThreads;
    fLogClass: TSynLogClass;
    fLogFamily: TSynLogFamily;
    fLogLevel: TSynLogLevels;
    fServerTimestampCacheTix: cardinal;
    fLogResponseMaxBytes: integer;
    fAcquireExecution: TRestAcquireExecutions;
    fPrivateGarbageCollector: TSynObjectListLocked;
    fServerTimestampOffset: TDateTime;
    fServerTimestampCacheValue: TTimeLogBits;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    procedure SetLogClass(aClass: TSynLogClass); virtual;
    /// wrapper methods to access fAcquireExecution[]
    procedure CheckAcquireExecutionCommand(Cmd: TRestServerUriContextCommand;
      Context: PUtf8Char);
    function GetAcquireExecutionMode(
      Cmd: TRestServerUriContextCommand): TRestServerAcquireMode;
    procedure SetAcquireExecutionMode(
      Cmd: TRestServerUriContextCommand; Value: TRestServerAcquireMode);
    function GetAcquireExecutionLockedTimeOut(
      Cmd: TRestServerUriContextCommand): cardinal;
    procedure SetAcquireExecutionLockedTimeOut(
      Cmd: TRestServerUriContextCommand; Value: cardinal);
    /// any overriden TRest class should call it in the initialization section
    class procedure RegisterClassNameForDefinition;
    /// ensure the thread will be taken into account during process
    // - will redirect to fOrmInstance: TRestOrmParent corresponding methods
    procedure OnBeginCurrentThread(Sender: TThread); virtual;
    procedure OnEndCurrentThread(Sender: TThread); virtual;
    procedure OnRestBackgroundTimerCreate; virtual;
  public
    /// initialize the class, and associate it to a specified database Model
    constructor Create(aModel: TOrmModel); reintroduce; virtual;
    // inherited classes should unserialize the other aDefinition properties by
    // overriding this method, in a reverse logic to overriden DefinitionTo()
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); virtual;
    /// release internal used instances
    // - e.g. release associated TOrmModel and TServiceContainer
    destructor Destroy; override;
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TRestOrmParent); virtual;
    /// save the TRest properties into a persistent storage object
    // - you can then use TRest.CreateFrom() to re-instantiate it
    // - current Definition.Key value will be used for the password encryption
    // - this default implementation will set the class name in Definition.Kind:
    // inherited classes should override this method and serialize other
    // properties, then override RegisteredClassCreateFrom() protected method
    // to initiate the very same instance
    procedure DefinitionTo(Definition: TSynConnectionDefinition); virtual;
    /// save the properties into a JSON file
    // - you can then use TRest.CreateFromJson() to re-instantiate it
    // - you can specify a custom Key, if the default is not enough for you
    function DefinitionToJson(Key: cardinal = 0): RawUtf8;
    /// save the properties into a JSON file
    // - you can then use TRest.CreateFromFile() to re-instantiate it
    // - you can specify a custom Key, if the default is not enough for you
    procedure DefinitionToFile(const aJsonFile: TFileName; aKey: cardinal = 0);
    /// create a new TRest instance from its Model and stored values
    // - aDefinition.Kind will define the actual class which will be
    // instantiated: currently TRestServerFullMemory, TRestServerDB,
    // TRestClientUriNamedPipe, TRestClientUriMessage,
    // TRestHttpClientSocket, TRestHttpClientWinINet, TRestHttpClientWinHttp,
    // and TRestHttpClientCurl classes are recognized by this method
    // - then other aDefinition fields will be used to refine the instance:
    // please refer to each overriden DefinitionTo() method documentation
    // - use TRestMongoDBCreate() and/or TRestExternalDBCreate() instead
    // to create a TRest instance will all tables defined as external when
    // aDefinition.Kind is 'MongoDB' or a TSqlDBConnectionProperties class
    // - will raise an exception if the supplied definition are not valid
    class function CreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean): TRest;
    /// try to create a new TRest instance from its Model and stored values
    // - will return nil if the supplied definition are not valid
    // - if the newly created instance is a TRestServer, will force the
    // supplied aServerHandleAuthentication parameter to enable authentication
    class function CreateTryFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean): TRest;
    /// create a new TRest instance from its Model and JSON stored values
    // - aDefinition.Kind will define the actual class which will be instantiated
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromJson(aModel: TOrmModel;
      const aJsonDefinition: RawUtf8; aServerHandleAuthentication: boolean;
      aKey: cardinal = 0): TRest;
    /// create a new TRest instance from its Model and a JSON file
    // - aDefinition.Kind will define the actual class which will be instantiated
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromFile(aModel: TOrmModel;
      const aJsonFile: TFileName; aServerHandleAuthentication: boolean;
      aKey: cardinal = 0): TRest;
    /// retrieve the registered class from the aDefinition.Kind string
    class function ClassFrom(aDefinition: TSynConnectionDefinition): TRestClass;

    /// ease logging of some text in the context of the current TRest
    procedure InternalLog(const Text: RawUtf8; Level: TSynLogLevel); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// ease logging of some text in the context of the current TRest
    procedure InternalLog(Format: PUtf8Char; const Args: array of const;
      Level: TSynLogLevel = sllTrace); overload;
    /// ease logging of some response in the context of the current TRest
    procedure InternalLogResponse(const aContent: RawByteString;
      const aContext: shortstring; Level: TSynLogLevel = sllServiceReturn); overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// ease logging of some response in the context of the current TRest
    procedure InternalLogResponse(aContent: PUtf8Char; aContentLen: PtrInt;
      const aContext: shortstring; Level: TSynLogLevel = sllServiceReturn); overload;
    /// ease logging of method enter/leave in the context of the current TRest
    function Enter(TextFmt: PUtf8Char; const TextArgs: array of const;
      aInstance: TObject = nil): ISynLog;
    /// internal method to retrieve the current Session TAuthUser.ID
    function GetCurrentSessionUserID: TID; virtual; abstract;
    /// retrieve the server time stamp
    // - default implementation will use an internal Offset to compute
    // the value from PC time (i.e. NowUtc+Offset as TTimeLog)
    // - inherited classes may override this method, or set the appropriate
    // value in Offset field
    function GetServerTimestamp(tix64: Int64): TTimeLog; virtual;
    /// set the server time stamp offset from the given date/time
    // - if Value is 0, the current local UTC time will be used
    procedure SetServerTimestamp(const Value: TTimeLog);

    /// main access to the IRestOrm methods of this instance
    property Orm: IRestOrm
      read fOrm;
    /// low-level access to the associated Data Model
    property Model: TOrmModel
      read fModel;
    /// access to the interface-based services list
    // - may be nil if no service interface has been registered yet: so be
    // aware that the following line may trigger an access violation if
    // no ICalculator is defined on server side:
    // ! if fServer.Services['Calculator'].Get(Calc)) then
    // !   ...
    // - safer typical use, following the DI/IoC pattern, and which will not
    // trigger any access violation if Services=nil, could be:
    // ! if fServer.Services.Resolve(ICalculator, Calc) then
    // !   ...
    property Services: TServiceContainer
      read fServices;
    /// access or initialize the internal IoC resolver, used for interface-based
    // remote services, and more generaly any Services.Resolve() call
    // - create and initialize the internal TServiceContainer if no service
    // interface has been registered yet
    // - may be used to inject some dependencies, which are not interface-based
    // remote services, but internal IoC, without the ServiceRegister()
    // or ServiceDefine() methods - e.g.
    // ! aRest.ServiceContainer.InjectResolver([TInfraRepoUserFactory.Create(aRest)], true);
    // - overriden methods will return TServiceContainerClient or
    // TServiceContainerServer instances, on TRestClient or TRestServer
    function ServiceContainer: TServiceContainer; virtual; abstract;
    /// internal procedure called to implement TServiceContainer.Release
    procedure ServicesRelease(Caller: TServiceContainer);
    /// access to the Multi-Threading process of this instance
    property Run: TRestRunThreads
      read fRun;

    /// how this class execute its internal commands
    // - by default, TRestServer.Uri() will lock for Write ORM according to
    // AcquireWriteMode (i.e. AcquireExecutionMode[execOrmWrite]=amLocked) and
    // other operations won't be protected (for better scaling)
    // - you can tune this behavior by setting this property to the expected
    // execution mode, e.g. execute all method-based services in a dedicated
    // thread via
    // ! aServer.AcquireExecutionMode[execSoaByMethod] := amBackgroundThread;
    property AcquireExecutionMode[Cmd: TRestServerUriContextCommand]: TRestServerAcquireMode
      read GetAcquireExecutionMode write SetAcquireExecutionMode;
    /// the time (in milli seconds) to try locking internal commands of this class
    // - this value is used only for AcquireExecutionMode[*]=amLocked
    // - by default, TRestServer.Uri() will lock for Write ORM according to
    // AcquireWriteTimeOut  (i.e. AcquireExecutionLockedTimeOut[execOrmWrite])
    // and other operations won't be locked nor have any time out set
    property AcquireExecutionLockedTimeOut[Cmd: TRestServerUriContextCommand]: cardinal
      read GetAcquireExecutionLockedTimeOut write SetAcquireExecutionLockedTimeOut;
    /// how this class will handle write access to the database
    // - is a common wrapper to AcquireExecutionMode[execOrmWrite] property
    // - default amLocked mode will wait up to AcquireWriteTimeOut milli seconds
    // to have a single access to the server write ORM methods
    // - amBackgroundThread will execute the write methods in a queue, in a
    // dedicated unique thread (which can be convenient, especially for
    // external database transaction process)
    // - amBackgroundOrmSharedThread will execute all ORM methods in a queue, in
    // a dedicated unique thread, shared for both execOrmWrite and execOrmGet,
    // but still dedicated for execSoaByMethod and execSoaByInterface
    // - a slower alternative to amBackgroundThread may be amMainThread
    // - you can set amUnlocked for a concurrent write access, but be aware
    // that it may lead into multi-thread race condition issues, depending on
    // the database engine used
    property AcquireWriteMode: TRestServerAcquireMode index execOrmWrite
      read GetAcquireExecutionMode write SetAcquireExecutionMode;
    /// the time (in milli seconds) which the class will wait for acquiring a
    // write acccess to the database, when AcquireWriteMode is amLocked
    // - is a common wrapper to AcquireExecutionLockedTimeOut[execOrmWrite]
    // - in order to handle safe transactions and multi-thread safe writing, the
    // server will identify transactions using the client Session ID: this
    // property will set the time out wait period
    // - default value is 5000, i.e. TRestServer.Uri will wait up to 5 seconds
    // in order to acquire the right to write on the database before returning
    // a "408 Request Time-out" status error
    property AcquireWriteTimeOut: cardinal index execOrmWrite
      read GetAcquireExecutionLockedTimeOut write SetAcquireExecutionLockedTimeOut;
    /// low-level access to the execution mode of the ORM and SOA process
    property AcquireExecution: TRestAcquireExecutions
      read fAcquireExecution;
    /// a local "Garbage collector" list, for some classes instances which must
    // live during the whole TRestServer process
    // - is used internally by the class, but can be used for business code
    property PrivateGarbageCollector: TSynObjectListLocked
      read fPrivateGarbageCollector;
    /// access to the TSynLog class used for logging
    // - equals TSynLog by default - but you could change it to a custom class
    property LogClass: TSynLogClass
      read fLogClass write SetLogClass;
    /// access to the associate TSynLog class familly
    property LogFamily: TSynLogFamily
      read fLogFamily;
    /// access to the associate TSynLog class events
    property LogLevel: TSynLogLevels
      read fLogLevel;
    /// tune the InternalLogResponse() output maximum size
    // - equals 2048 by default - you could use e.g. MaxInt for no size limit
    // of valid UTF-8 response content
    property LogResponseMaxBytes: integer
      read fLogResponseMaxBytes write fLogResponseMaxBytes;

  {$ifndef PUREMORMOT2}
    // backward compatibility redirections to the homonymous IRestOrm methods
    // see IRestOrm documentation for the proper use information
  public
    function TableRowCount(Table: TOrmClass): Int64;
    function TableHasRows(Table: TOrmClass): boolean;
    function TableMaxID(Table: TOrmClass): TID;
    function MemberExists(Table: TOrmClass; ID: TID): boolean;
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8): RawUtf8; overload;
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUtf8; Default: Int64 = 0): Int64;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): RawUtf8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const): RawUtf8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
      WhereID: TID): RawUtf8; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      const WhereClause: RawUtf8): boolean; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
      WhereID: TID): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; out Data: TRawUtf8DynArray): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8; var Data: TInt64DynArray;
      SQL: PRawUtf8 = nil): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUtf8;
      const WhereClause: RawUtf8 = ''; const Separator: RawUtf8 = ','): RawUtf8; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUtf8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClause: RawUtf8 = ''): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8; const BoundsSqlWhere: array of const): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUtf8;
      const WhereClauseFormat: RawUtf8; const Args, Bounds: array of const): TOrmTable; overload;
    function FtsMatch(Table: TOrmFts3Class; const WhereClause: RawUtf8;
      var DocID: TIDDynArray): boolean; overload;
    function FtsMatch(Table: TOrmFts3Class; const MatchClause: RawUtf8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUtf8;
    function MainFieldID(Table: TOrmClass; const Value: RawUtf8): TID;
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUtf8;
      out IDs: TIDDynArray): boolean;
    function Retrieve(const SqlWhere: RawUtf8; Value: TOrm;
      const FieldsCsv: RawUtf8 = ''): boolean; overload;
    function Retrieve(const WhereClauseFmt: RawUtf8;
      const Args, Bounds: array of const; Value: TOrm;
      const FieldsCsv: RawUtf8 = ''): boolean; overload;
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; overload;
    function Retrieve(Reference: TRecordReference;
      ForUpdate: boolean = false): TOrm; overload;
    function Retrieve(aPublishedRecord, aValue: TOrm): boolean; overload;
    function RetrieveList(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const FieldsCsv: RawUtf8 = ''): TObjectList; overload;
    function RetrieveListJson(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const FieldsCsv: RawUtf8 = ''; aForceAjax: boolean = false): RawJson; overload;
    function RetrieveListJson(Table: TOrmClass;
      const SqlWhere: RawUtf8; const FieldsCsv: RawUtf8 = '';
      aForceAjax: boolean = false): RawJson; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, FieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUtf8; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const; const FieldsCsv: RawUtf8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const): variant;
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const FieldsCsv: RawUtf8): variant;
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const FieldsCsv: RawUtf8 = ''): boolean;
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
      const OutputFieldName: RawUtf8; W: TOrmWriter;
      const FieldsCsv: RawUtf8 = '');
    function RTreeMatch(DataTable: TOrmClass;
      const DataTableBlobFieldName: RawUtf8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUtf8): TOrmTable;
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUtf8; ForceAjax: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJson;
    function Execute(const aSql: RawUtf8): boolean;
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args: array of const): boolean; overload;
    function ExecuteFmt(const SqlFormat: RawUtf8;
      const Args, Bounds: array of const): boolean; overload;
    function UnLock(Table: TOrmClass; aID: TID): boolean; overload;
    function UnLock(Rec: TOrm): boolean; overload;
    function Add(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomCsvFields: RawUtf8;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomFields: TFieldBits;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function AddWithBlobs(Value: TOrm;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID;
    function AddSimple(aTable: TOrmClass;
      const aSimpleFields: array of const; ForcedID: TID = 0): TID;
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(Value: TOrm; const CustomCsvFields: RawUtf8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(aTable: TOrmClass; aID: TID;
      const aSimpleFields: array of const): boolean; overload;
    function AddOrUpdate(Value: TOrm; ForceID: boolean = false): TID;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUtf8;
      const WhereFieldValue: array of const; const FieldName: RawUtf8;
      const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUtf8; const WhereFieldValue: variant;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass; const IDs: array of TID;
      const FieldName: RawUtf8; const FieldValue: variant): boolean; overload;
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUtf8; Increment: Int64 = 1): boolean;
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUtf8 = nil): boolean;
    function Delete(Table: TOrmClass; ID: TID): boolean; overload;
    function Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean; overload;
    function Delete(Table: TOrmClass; const FormatSqlWhere: RawUtf8;
      const BoundsSqlWhere: array of const): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUtf8;
      out BlobData: RawBlob): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUtf8;
      out BlobStream: TCustomMemoryStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; const BlobData: RawBlob): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; BlobData: TStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUtf8; BlobData: pointer; BlobSize: integer): boolean; overload;
    function UpdateBlobFields(Value: TOrm): boolean;
    function RetrieveBlobFields(Value: TOrm): boolean;
    function TransactionBegin(aTable: TOrmClass; SessionID: cardinal): boolean;
    function TransactionActiveSession: cardinal;
    procedure Commit(SessionID: cardinal; RaiseException: boolean = false);
    procedure RollBack(SessionID: cardinal);
    procedure WriteLock;
    procedure WriteUnLock;
    function BatchSend(Batch: TRestBatch; var Results: TIDDynArray): integer; overload;
    function BatchSend(Batch: TRestBatch): integer; overload;
    function AsyncBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJson]): boolean;
    function AsyncBatchStop(Table: TOrmClass): boolean;
    function AsyncBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsyncBatchRawAdd(Table: TOrmClass; const SentData: RawUtf8): integer;
    procedure AsyncBatchRawAppend(Table: TOrmClass; SentData: TJsonWriter);
    function AsyncBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;
    function Cache: TOrmCache;
    function CacheOrNil: TOrmCache;
    function CacheWorthItForTable(aTableIndex: cardinal): boolean;
  public
    // TRestRunThreads compatibility methods
    function NewBackgroundThreadMethod(const Format: RawUtf8;
      const Args: array of const): TSynBackgroundThreadMethod;
    function NewBackgroundThreadProcess(const aOnProcess: TOnSynBackgroundThreadProcess;
      aOnProcessMS: cardinal; const Format: RawUtf8; const Args: array of const;
      aStats: TSynMonitorClass=nil): TSynBackgroundThreadProcess;
    function NewParallelProcess(ThreadCount: integer; const Format: RawUtf8;
      const Args: array of const): TSynParallelProcess;
    function TimerEnable(const aOnProcess: TOnSynBackgroundTimerProcess;
      aOnProcessSecs: cardinal): TRestBackgroundTimer;
    function TimerDisable(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    function SystemUseTrack(periodSec: integer = 10): TSystemUse;
    function EnsureBackgroundTimerExists: TRestBackgroundTimer;
    procedure BeginCurrentThread(Sender: TThread); virtual;
    procedure EndCurrentThread(Sender: TThread); virtual;
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    procedure AsyncRedirect(const aGuid: TGuid;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsyncRedirectResult = nil); overload;
    procedure AsyncInterning(Interning: TRawUtf8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    function MultiRedirect(const aGuid: TGuid; out aCallbackInterface;
      aCallBackUnRegisterNeeded: boolean = true): IMultiCallbackRedirect; overload;
    function BackgroundTimer: TRestBackgroundTimer;
      {$ifdef HASINLINE}inline;{$endif}
  {$endif PUREMORMOT2}
  end;

// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRest         = TRest;
  TSqlRestClass    = TRestClass;
  TSqlRestDynArray = TRestDynArray;

{$endif PUREMORMOT2}

function ToText(cmd: TRestServerUriContextCommand): PShortString; overload;

const
  /// custom contract value to ignore contract validation from client side
  // - you could set the aContractExpected parameter to this value for
  // TRestClientUri.ServiceDefine or TRestClientUri.ServiceRegister
  // so that the contract won't be checked with the server
  // - it will be used e.g. if the remote server is not a mORMot server,
  // but a plain REST/HTTP server - e.g. for public API notifications
  SERVICE_CONTRACT_NONE_EXPECTED = '*';


{ ************ RESTful Authentication Support }

const
  /// default hashed password set by TAuthGroup.InitializeTable for all users
  // - contains TAuthUser.ComputeHashedPassword('synopse')
  // - override AuthAdminDefaultPassword, AuthSupervisorDefaultPassword and
  // AuthUserDefaultPassword values to follow your own application expectations
  DEFAULT_HASH_SYNOPSE =
    '67aeea294e1cb515236fd7829c55ec820ef888e8e221814d24d83b3dc4d825dd';

var
  /// default timeout period set by TAuthGroup.InitializeTable for 'Admin' group
  // - you can override this value to follow your own application expectations
  AuthAdminGroupDefaultTimeout: integer = 10;
  /// default timeout period set by TAuthGroup.InitializeTable for 'Supervisor' group
  // - you can override this value to follow your own application expectations
  // - note that clients will maintain the session alive using CacheFlush/_ping_
  AuthSupervisorGroupDefaultTimeout: integer = 60;
  /// default timeout period set by TAuthGroup.InitializeTable for 'User' group
  // - you can override this value to follow your own application expectations
  // - note that clients will maintain the session alive using CacheFlush/_ping_
  AuthUserGroupDefaultTimeout: integer = 60;
  /// default timeout period set by TAuthGroup.InitializeTable for 'Guest' group
  // - you can override this value to follow your own application expectations
  // - note that clients will maintain the session alive using CacheFlush/_ping_
  AuthGuestGroupDefaultTimeout: integer = 60;

  /// default hashed password set by TAuthGroup.InitializeTable for 'Admin' user
  // - you can override this value to follow your own application expectations
  AuthAdminDefaultPassword: RawUtf8 = DEFAULT_HASH_SYNOPSE;
  /// default hashed password set by TAuthGroup.InitializeTable for 'Supervisor' user
  // - you can override this value to follow your own application expectations
  AuthSupervisorDefaultPassword: RawUtf8 = DEFAULT_HASH_SYNOPSE;
  /// default hashed password set by TAuthGroup.InitializeTable for 'User' user
  // - you can override this value to follow your own application expectations
  AuthUserDefaultPassword: RawUtf8 = DEFAULT_HASH_SYNOPSE;


type
  /// table containing the available user access rights for authentication
  // - this class should be added to the TOrmModel, together with TAuthUser,
  // to allow authentication support
  // - you can inherit from it to add your custom properties to each user info:
  // TOrmModel will search for any class inheriting from TAuthGroup to
  // manage per-group authorization data
  // - by default, it won't be accessible remotely by anyone
  TAuthGroup = class(TOrm)
  private
    fIdent: RawUtf8;
    fSessionTimeOut: integer;
    fAccessRights: RawUtf8;
    function GetOrmAccessRights: TOrmAccessRights;
    procedure SetOrmAccessRights(const Value: TOrmAccessRights);
  public
    /// called when the associated table is created in the database
    // - on a new database, if TAuthUser and TAuthGroup tables are defined
    // in the associated TOrmModel, it this will add 'Admin', 'Supervisor',
    // and 'User' rows in the AuthUser table (with 'synopse' as default password),
    // and associated 'Admin', 'Supervisor', 'User' and 'Guest' groups, with the
    // following access rights to the AuthGroup table:
    // $            POSTSQL SELECTSQL Service AuthR AuthW TablesR TablesW
    // $ Admin        Yes     Yes       Yes    Yes   Yes    Yes    Yes
    // $ Supervisor   No      Yes       Yes    Yes   No     Yes    Yes
    // $ User         No      No        Yes    No    No     Yes    Yes
    // $ Guest        No      No        No     No    No     Yes    No
    // - 'Admin' will be the only able to execute remote not SELECT SQL statements
    // for POST commands (reSQL flag in TOrmAccessRights.AllowRemoteExecute) and
    // modify the Auth tables (i.e. AuthUser and AuthGroup)
    // - 'Admin' and 'Supervisor' will allow any SELECT SQL statements to be
    // executed, even if the table can't be retrieved and checked (corresponding
    // to the reSqlSelectWithoutTable flag)
    // - 'User' won't have the reSqlSelectWithoutTable flag, nor the right
    // to retrieve the Auth tables data for other users
    // - 'Guest' won't have access to the interface-based remote JSON-RPC service
    // (no reService flag), nor perform any modification to a table: in short,
    // this is an ORM read-only limited user
    // - you MUST override the default 'synopse' password to a custom value,
    // or at least customize the global AuthAdminDefaultPassword,
    // AuthSupervisorDefaultPassword, AuthUserDefaultPassword variables
    // - of course, you can change and tune the settings of the AuthGroup and
    // AuthUser tables, but only 'Admin' group users will be able to remotely
    // modify the content of those two tables
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
    /// corresponding TOrmAccessRights for this authentication group
    // - content is converted into/from text format via AccessRight DB property
    // (so it will be not fixed e.g. by the binary TOrmTableBits layout, i.e.
    // the MAX_TABLES constant value)
    // - if you want to call Edit on the returned value, use a temp variable:
    // !var
    // !  oar: TOrmAccessRights;
    // !begin
    // !  oar := Group.OrmAccessRights;
    // !  oar.Edit(....);
    // !  oar.Edit(....);
    // !  Group.OrmAccessRights := oar;
    // !  rest.Update(Group);
    property OrmAccessRights: TOrmAccessRights
      read GetOrmAccessRights write SetOrmAccessRights;
    {$ifndef PUREMORMOT2}
    property SqlAccessRights: TOrmAccessRights
      read GetOrmAccessRights write SetOrmAccessRights;
    {$endif PUREMORMOT2}
  published
    /// the access right identifier, ready to be displayed
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" (i.e. "stored false") attribute)
    // - so you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fClient.MainFieldID(TAuthGroup,'User');
    property Ident: RawUtf8 index 50
      read fIdent write fIdent stored AS_UNIQUE;
    /// the number of minutes a session is kept alive
    property SessionTimeout: integer
      read fSessionTimeOut write fSessionTimeOut;
    /// a textual representation of a TOrmAccessRights buffer
    property AccessRights: RawUtf8 index 1600
      read fAccessRights write fAccessRights;
  end;

  /// table containing the Users registered for authentication
  // - this class should be added to the TOrmModel, together with TAuthGroup,
  // to allow authentication support
  // - you can inherit from it to add your custom properties to each user info:
  // TOrmModel will search for any class inheriting from TAuthUser to manage
  // per-user authorization data
  // - by default, it won't be accessible remotely by anyone; to enhance security,
  // you could use the TSynValidatePassWord filter to this table
  TAuthUser = class(TOrm)
  protected
    fLogonName: RawUtf8;
    fPasswordHashHexa: RawUtf8;
    fDisplayName: RawUtf8;
    fGroupRights: TAuthGroup;
    fData: RawBlob;
    // just wrap default ComputeHashedPassword() = SetPassword() plain Sha256()
    procedure SetPasswordPlain(const Value: RawUtf8);
  public
    /// set the PasswordHashHexa field using Sha256 old mORMot 1 encoding
    // - consider the much safer SetPassword(TModularCryptFormat) method
    property PasswordPlain: RawUtf8
      write SetPasswordPlain;
    /// set the PasswordHashHexa field using Pbkdf2HmacSha256 old mORMot 1 encoding
    // - consider the much safer SetPassword(TModularCryptFormat) method
    procedure SetPassword(const aPasswordPlain, aHashSalt: RawUtf8;
      aHashRound: integer = 20000); overload;
    /// set the PasswordHashHexa field using a "Modular Crypt" hash
    // - with its default parameters, and a random salt
    // - the server will send back the actual format (algo and params) expected
    // for each user during its login handshake, so you could just login with
    // TRestClientUri.SetUser() with the plain password and no other info
    // - in practice: you may still consider PasswordPlain/Sha256 or PasswordDigest
    // from a Web or JavaScript client, but rather use mcfBCryptSha256 or mcfSCrypt
    // for login from a mORMot 2 client executable; fallback to PBKDF2 variant
    // if you need to be compatible with mORMot 1 client (but you need to know
    // the number of rounds)
    procedure SetPassword(const aPasswordPlain: RawUtf8;
      aModularCrypt: TModularCryptFormat); overload;
    /// set the PasswordHashHexa field as DIGEST-HA0 from plain password content
    // - will use the current LogonName as part of the digest
    // - could be called if you want your user to authenticate from a Web client
    // using the standard "Http-Authenticate: Digest" mechanism (only over TLS)
    procedure SetPasswordDigest(const aPasswordPlain, aRealm: RawUtf8;
      aAlgo: TDigestAlgo = daSHA256);
    /// check if the user can authenticate in its current state
    // - Ctxt is a TRestServerUriContext instance (not yet defined in this unit)
    // - called by TRestServerAuthentication.GetUser() method
    // - this default implementation will return TRUE, i.e. allow the user
    // to log on
    // - override this method to disable user authentication, e.g. if the user
    // is disabled via a custom ORM field, typically marked as unsafe or expired
    function CanUserLog(Ctxt: TObject): boolean; virtual;
    /// class function called internally to compute a hashed password
    // - defined as virtual so that you may use your own hashing mechanism
    // - used by SetPassword/SetPasswordDigest to fill TAuthUser.PasswordHashHexa
    // - aHashRound = 0 uses plain Sha256(), as early mORMot 1 encoding
    // - aHashRound > 0 triggers Pbkdf2HmacSha256() via aHashSalt, and enable
    // Pbkdf2HmacSha256() to increase security on storage side (reducing brute
    // force attack via rainbow tables) - as mORMot 1 safer approach
    // - aHashRound < 0 will use standard DIGEST-HA0 hashing, compatible with
    // TDigestAuthServer, expecting aHashRound as -ord(TDigestAlgo) - to be
    // used if you want to log from a HTTP client, also from SetPasswordDigest()
    // - aLogonName is only used for aHashRound < 0 = DIGEST-HA0 hashing
    // - as a safer alternative, use ModularCryptHash() from mormot.crypt.secure
    // to fill the PasswordHashHexa field - this class method will recognize its
    // patterns in aPasswordPlain or you could use the SetPassword() overload
    class function ComputeHashedPassword(const aLogonName, aPasswordPlain: RawUtf8;
      const aHashSalt: RawUtf8 = ''; aHashRound: integer = 20000): RawUtf8; virtual;
  published
    /// the User identification Name, as entered at log-in
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" - i.e. "stored false" - attribute), and
    // therefore indexed in the database (e.g. hashed in TRestStorageInMemory)
    property LogonName: RawUtf8
      index 20 read fLogonName write fLogonName stored AS_UNIQUE;
    /// the User Name, as may be displayed or printed
    property DisplayName: RawUtf8
      index 50 read fDisplayName write fDisplayName;
    /// the encoded hash of the password
    // - use SetPassword/SetPasswordDigest methods to compute this value
    // - old default is to store the SHA-256 32 bytes as 64 hexa chars (mORMot 1
    // original algo) - or via PBKDF2 (another mORMot 1 option) or as DIGEST-HA0
    // - as a safer alternative, consider storing ModularCryptHash() hashes from
    // mormot.crypt.secure via the SetPassword(TModularCryptFormat) overload
    // - maximum size (i.e. "index" value) was 64 - but has been upgraded to 192
    // for DIGEST-HA0 with daSHA512 and the new "Modular" hashes: SHA512-Crypt
    // and SCrypt lengths are both 122 chars, but safe BCrypt is 60 chars so you
    // could still use it if you can't easily upgrade the database
    // - you can set directly your own custom "Modular Crypt" hash - e.g. forcing
    // mcfSCrypt with LogN=20, R=8, P=1 for admin/root login, burning 1.23s and
    // 1GB RAM on client side during the hashing (but not on the server side)
    property PasswordHashHexa: RawUtf8
      index 192 read fPasswordHashHexa write fPasswordHashHexa;
    /// the associated access rights of this user
    // - access rights are managed by group
    // - in TAuthSession.User instance, GroupRights property will contain a
    // REAL TAuthGroup instance for fast retrieval in TRestServer.Uri
    // - note that 'Group' field name is not allowed by SQLite
    property GroupRights: TAuthGroup
      read fGroupRights write fGroupRights;
    /// some custom data, associated to the User
    // - Server application may store here custom data
    // - TAuthSession.Create will retrieved this field from the database,
    // unless rsoGetUserRetrieveNoBlobData option is defined
    // - its content is not used by the framework but 'may' be used by yours
    property Data: RawBlob
      read fData write fData;
  end;

  /// class-reference type (metaclass) of a table containing the Users
  // registered for authentication
  // - see also TRestServer.OnAuthenticationUserRetrieve custom event
  TAuthUserClass = class of TAuthUser;

  /// class-reference type (metaclass) of the table containing the available
  // user access rights for authentication, defined as a group
  TAuthGroupClass = class of TAuthGroup;


{ ************ TRestUriParams REST URI Definitions }

type
  /// an opaque connection-specific pointer identifier with a strong type
  // - each raw connection instance maintains two abstract PtrUInt tags
  // - match THttpServerConnectionOpaque as defined in mormot.net.http
  TRestServerConnectionOpaque = record
    /// pointer-sized tag reserved to mORMot (e.g. to idenfity a REST session)
    ValueInternal: PtrUInt;
    /// pointer-sized tag free for the end-user code
    ValueExternal: PtrUInt;
  end;
  /// reference to an opaque connection-specific pointer identifier
  // - may be nil if unsupported, e.g. by the http.sys servers
  PRestServerConnectionOpaque = ^TRestServerConnectionOpaque;

  /// flags which may be set by the caller to notify low-level context
  // - llfHttps is set if the communication was made over HTTPS
  // - llfSecured if the transmission is encrypted or in-process
  // using e.g. HTTPS/TLS or our proprietary AES/ECDHE WebSockets algorithms
  // - llfWebsockets if communication was made using WebSockets
  // - llfInProcess when run from the same process, i.e. on server side
  // - llfConnectionUpgrade when "connection: upgrade" is within headers
  // - llfAuthorized when a valid "authorization:" header is set
  // - llfHttp10 if the connection is of old HTTP/1.0 level
  // - should exactly match THttpServerRequestFlag from mormot.net.http.pas
  TRestUriParamsLowLevelFlag = (
    llfHttps,
    llfSecured,
    llfWebsockets,
    llfInProcess,
    llfConnectionUpgrade,
    llfAuthorized,
    llfHttp10);

  /// some flags set by the caller to notify low-level context
  TRestUriParamsLowLevelFlags = set of TRestUriParamsLowLevelFlag;

  /// store all parameters for a Client or Server method call
  // - as used by TRestServer.Uri or TRestClientUri.InternalUri
  {$ifdef USERECORDWITHMETHODS}
  TRestUriParams = record
  {$else}
  TRestUriParams = object
  {$endif USERECORDWITHMETHODS}
  public
    /// input parameter containing the caller URI
    Url: RawUtf8;
    /// input parameter containing the caller method
    // - handle standard REST codes as GET/POST/PUT/DELETE; but also our own
    // extensions like LOCK/UNLOCK/BEGIN/END/ABORT
    Method: RawUtf8;
    /// input parameter containing the caller message headers
    // - you can use e.g. to retrieve the remote IP:
    // ! Call.Header(HEADER_REMOTEIP_UPPER)
    // ! or FindNameValue(Call.InHead,HEADER_REMOTEIP_UPPER)
    // but consider rather using TRestServerUriContext.RemoteIP
    InHead: RawUtf8;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    InBody: RawUtf8;
    /// output parameter to be set to the response message header
    // - it is the right place to set the returned message body content type,
    // e.g. TEXT_CONTENT_TYPE_HEADER or HTTP_CONTENT_TYPE_HEADER: if not set,
    // the default JSON_CONTENT_TYPE_HEADER will be returned to the client,
    // meaning that the message is JSON
    // - you can use OutBodyType() function to retrieve the stored content-type
    OutHead: RawUtf8;
    /// output parameter to be set to the response message body
    OutBody: RawUtf8;
    /// output parameter to be set to the HTTP status integer code
    // - HTTP_NOTFOUND=404 e.g. if the url doesn't start with Model.Root (caller
    // can try another TRestServer)
    OutStatus: cardinal;
    /// output parameter to be set to the database internal state
    OutInternalState: cardinal;
    /// associated RESTful access rights
    // - AccessRights must be handled by the TRestServer child, according
    // to the Application Security Policy (user logging, authentification and
    // rights management) - making access rights a parameter allows this method
    // to be handled as pure stateless, thread-safe and session-free
    RestAccessRights: POrmAccessRights;
    /// numerical reference to the connection which made this request
    // - stores mormot.net.http's THttpServerConnectionID, e.g. a http.sys
    // 64-bit ID, or an incremental rolling sequence of 31-bit integers for
    // THttpServer/TWebSocketServer, or maybe a raw PtrInt(self/THandle)
    LowLevelConnectionID: TRestConnectionID;
    /// low-level properties of the current connection
    LowLevelConnectionFlags: TRestUriParamsLowLevelFlags;
    /// efficient per-connection pointer storage at HTTP server level
    // - nil if unsupported, e.g. by the http.sys servers
    // - map to THttpAsyncServerConnection or THttpServerSocket fConnectionOpaque
    // of type THttpServerConnectionOpaque as defined in mormot.net.http
    // - could be used to avoid a lookup to a ConnectionID-indexed dictionary
    // - warning: only ValueExternal is usable by end-user code
    LowLevelConnectionOpaque: PRestServerConnectionOpaque;
    /// pre-parsed Remote IP of the current connection
    // - may equal '' if remote IP is the 127.0.0.1 loopback
    LowLevelRemoteIP: RawUtf8;
    /// pre-parsed "Bearer" HTTP header value
    LowLevelBearerToken: RawUtf8;
    /// pre-parsed "User-Agent" HTTP header value
    LowLevelUserAgent: RawUtf8;
    /// initialize the non RawUtf8 values
    procedure Init; overload;
    /// initialize the input values
    procedure Init(const aUri, aMethod, aInHead, aInBody: RawUtf8); overload;
    /// retrieve the "Content-Type" value from InHead
    // - if GuessJsonIfNoneSet is TRUE, returns JSON if none was set in headers
    procedure InBodyType(var ContentType: RawUtf8;
      GuessJsonIfNoneSet: boolean = true);
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the "Content-Type" value from OutHead
    // - if GuessJsonIfNoneSet is TRUE, returns JSON if none was set in headers
    function OutBodyType(GuessJsonIfNoneSet: boolean = true): RawUtf8;
    /// check if the "Content-Type" value from OutHead is JSON
    // - if GuessJsonIfNoneSet is TRUE, assume JSON is used
    function OutBodyTypeIsJson(GuessJsonIfNoneSet: boolean = true): boolean;
    /// just a wrapper around FindNameValue(InHead,UpperName)
    // - use e.g. as
    // ! Call.Header(HEADER_REMOTEIP_UPPER) or Call.Header(HEADER_BEARER_UPPER)
    // - consider rather using TRestServerUriContext.InHeader[] or even
    // dedicated TRestServerUriContext.RemoteIP/AuthenticationBearerToken
    function Header(UpperName: PAnsiChar): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// wrap FindNameValue(InHead,UpperName) with a cache store
    procedure HeaderOnce(var Store, Dest: RawUtf8; UpperName: PAnsiChar);
    /// retrieve the "RemoteIP" value from the incoming HTTP header
    procedure GetRemoteIP(var Dest: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// "RemoteIP" value from existing LowLevelRemoteIP but nil for '127.0.0.1'
    // - won't scan InHead content, just check current LowLevelRemoteIP value
    // - returns PUtf8Char and not RawUtf8 to avoid a try..finally e.g. on logging
    function RemoteIPNotLocal: PUtf8Char;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the "User-Agent" Dest from the incoming HTTP headers
    procedure GetUserAgent(var Dest: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the "Authorization: Bearer <token>" Dest from incoming HTTP headers
    procedure GetAuthenticationBearerToken(var Dest: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// used to map set of parameters for a Client or Server method call
  PRestUriParams = ^TRestUriParams;

  /// how a TLibraryRequest function will release its Head and Resp returned values
  TLibraryRequestFree = procedure(Data: pointer); cdecl;

  /// the function signature of the LibraryRequest() function
  // - as exported by TRestServer.ExportServerGlobalLibraryRequest
  // - and as consummed by TRestClientLibraryRequest on client side
  TLibraryRequest = function(
    Url, Method, SendData: PUtf8Char; UrlLen, MethodLen, SendDataLen: cardinal;
    out HeadRespFree: TLibraryRequestFree; var Head: PUtf8Char; var HeadLen: cardinal;
    out Resp: PUtf8Char; out RespLen, State: cardinal): cardinal; cdecl;

// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlAuthUser = TAuthUser;
  TSqlAuthGroup = TAuthGroup;
  TSqlAuthUserClass = TAuthUserClass;
  TSqlAuthGroupClass = TAuthGroupClass;
  TSqlRestUriParamsLowLevelFlag = TRestUriParamsLowLevelFlag;
  TSqlRestUriParamsLowLevelFlags = TRestUriParamsLowLevelFlags;
  TSqlRestUriParams = TRestUriParams;
  PSqlRestUriParams = PRestUriParams;

{$endif PUREMORMOT2}


{ ************ TRestUriContext REST Parent Process on Server Side }

type
  /// used by TRestUriContext.ClientKind to identify the currently
  // connected client
  TRestClientKind = (
    ckUnknown,
    ckFramework,
    ckAjax);

  /// abstract calling context for any Server-Side REST process
  // - is inherited e.g. by TRestServerUriContext for TRestServer.Uri processing
  TRestUriContext = class
  protected
    fCall: PRestUriParams;
    fMethod: TUriMethod;                                          // 8-bit
    fClientKind: TRestClientKind;                                 // 8-bit
    fCommand: TRestServerUriContextCommand;                       // 8-bit
    fInputCookiesParsed: (icpNotParsed, icpNone, icpAvailable);   // 8-bit
    fInputContentType: RawUtf8;
    fInHeaderLastName: RawUtf8;
    fInHeaderLastValue: RawUtf8;
    fJwtContent: PJwtContent;
    fTix64: Int64;
    fInputCookies: THttpCookies;
    fOutSetCookie: RawUtf8;
    function GetUserAgent: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetInHeader(const HeaderName: RawUtf8): RawUtf8;
    function InputCookiesParse: PHttpCookies;
    function InputCookies: PHttpCookies;
      {$ifdef HASINLINE} inline; {$endif}
    function GetInCookie(const CookieName: RawUtf8): RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    procedure SetOutSetCookie(const aOutSetCookie: RawUtf8); virtual;
    procedure SetOutCookie(const aName, aValue: RawUtf8);
    function StatusCodeToText(Code: cardinal): PRawUtf8; virtual;
  public
    /// access to all input/output parameters at TRestServer.Uri() level
    // - process should better call Results() or Success() methods to set the
    // appropriate answer or Error() method in case of an error
    // - use Method/RemoteIPNotLocal/UserAgent/AuthenticationBearerToken and
    // InHeader[]/InCookie[] high-level properties instead of this instance
    // - low-level access to the request parameters can be made via this pointer,
    // e.g. to access Call^.Url or Call^.LowLevelConnectionID
    property Call: PRestUriParams
      read fCall;
    /// the used Client-Server method (matching the corresponding HTTP Verb)
    // - this property will be set from incoming URI, even if RESTful
    // authentication is not enabled
    property Method: TUriMethod
      read fMethod;
    /// retrieve the "RemoteIP" value from Call^.LowLevelRemoteIP or from
    // the incoming HTTP headers
    // - may return '' or '127.0.0.1'
    procedure SetRemoteIP(var IP: RawUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    /// "RemoteIP" value from Call^.LowLevelRemoteIP but nil for '127.0.0.1'
    // - won't scan the incoming HTTP headers, but it is usually not needed
    // - returns a PUtf8Char and not a RawUtf8 to avoid a try..finally on logging
    function RemoteIPNotLocal: PUtf8Char;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve the "User-Agent" value from the incoming HTTP headers
    property UserAgent: RawUtf8
      read GetUserAgent;
    /// identify which kind of client is actually connected
    // - the "User-Agent" HTTP will be checked for 'mORMot' substring, and
    // set ckFramework on match
    // - either ckAjax for a classic (AJAX) browser, or any other kind of
    // HTTP client
    // - will be used e.g. by ClientOrmOptions to check if the
    // current remote client expects standard JSON in all cases
    function ClientKind: TRestClientKind;
    /// check if the content-type input is 'application/json' or ''
    function ContentTypeIsJson: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// decode any multipart/form-data POST request input
    // - returns TRUE and set MultiPart array as expected, on success
    function InputAsMultiPart(var MultiPart: TMultiPartDynArray): boolean;
    /// retrieve an incoming HTTP header
    // - the supplied header name is case-insensitive
    // - but rather call RemoteIP or UserAgent properties instead of
    // InHeader['RemoteIP'] or InHeader['User-Agent'] since those values may
    // have been set directly from the socket layer and not within headers
    property InHeader[const HeaderName: RawUtf8]: RawUtf8
      read GetInHeader;
    /// retrieve an incoming HTTP cookie value
    // - cookie name are case-sensitive
    // - consider faster InCookieSearch() if a transient RawUtf8 is not required
    property InCookie[const CookieName: RawUtf8]: RawUtf8
      read GetInCookie;
    /// retrieve a cookie name/value pair in the internal storage
    // - cookie name are case-sensitive
    function InCookieSearch(const CookieName: RawUtf8): PHttpCookie;
      {$ifdef HASINLINE} inline; {$endif}
    /// low-level raw cookie value as set by OutCookie[] to be sent as response
    // - if not void, TRestServer.Uri() will define a new 'set-cookie: ...'
    // header in Call^.OutHead to be stored on the HTTP client
    // - overriden TRestServerUriContext will append "Path=/" or "Secure" members
    // according to rsoCookieHttpOnlyFlagDisable and rsoCookieSecure options
    property OutSetCookie: RawUtf8
      read fOutSetCookie write SetOutSetCookie;
    /// define a new 'name=value' cookie to be returned to the client
    // - you can use COOKIE_EXPIRED as value to delete a cookie in the browser
    property OutCookie[const CookieName: RawUtf8]: RawUtf8
      write SetOutCookie;
    /// low-level HTTP header merge of the OutSetCookie value
    procedure OutHeadFromCookie; virtual;
    /// low-level wrapper method around GetTickCount64 to cache the value
    // - may avoid OS API calls on server side, during a request process
    // - warning: do not use within loops for timeout, because it won't change
    function TickCount64: Int64;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve the "Authorization: Bearer <token>" value from incoming HTTP headers
    // - typically returns a JWT for statelesss self-contained authentication,
    // as expected by TJwtAbstract.Verify method
    function AuthenticationBearerToken: RawUtf8; virtual;
    /// validate "Authorization: Bearer <JWT>" content from incoming HTTP headers
    // - returns true on success, storing the payload in JwtContent^ field
    // - set JwtContent^.result = jwtNoToken if jwt is nil
    // - on failure (i.e. returns false), will set the error context as
    // 403 HTTP_FORBIDDEN so that you may directly write:
    // ! procedure TMyDaemon.Files(Ctxt: TRestServerUriContext);
    // ! begin
    // !   if Ctxt.AuthenticationCheck(fJWT) then
    // !     Ctxt.ReturnFileFromFolder('c:\datafolder');
    // ! end;
    function AuthenticationCheck(jwt: TJwtAbstract): boolean; virtual;
    /// JWT validation information, as filled by AuthenticationCheck()
    // - equals nil if no JWT authentication was made
    property JwtContent: PJwtContent
      read fJwtContent;

    /// use this method to send back directly a result value to the caller
    // - expects Status to be either HTTP_SUCCESS, HTTP_NOTMODIFIED,
    // HTTP_CREATED, or HTTP_TEMPORARYREDIRECT, and will return as answer the
    // supplied result content with no transformation
    // - if Status is an error code, it will call Error() method
    // - CustomHeader optional parameter can be set e.g. to
    // TEXT_CONTENT_TYPE_HEADER if the default JSON_CONTENT_TYPE is not OK,
    // or calling GetMimeContentTypeHeader() on the returned binary buffer
    // - if Handle304NotModified is TRUE and Status is HTTP_SUCCESS, the result
    // content will be hashed (using crc32c) and in case of no modification
    // will return HTTP_NOTMODIFIED to the browser, without the actual result
    // content (to save bandwidth)
    // - set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age=xxx header
    procedure Returns(const result: RawUtf8; Status: integer = HTTP_SUCCESS;
      const CustomHeader: RawUtf8 = ''; Handle304NotModified: boolean = false;
      HandleErrorAsRegularResult: boolean = false; CacheControlMaxAgeSec: integer = 0;
      const ServerHash: RawUtf8 = ''); overload;
    /// use this method to send back a JSON object to the caller
    // - this method will encode the supplied values e.g. as
    // ! JsonEncode(['name','John','year',1972]) = '{"name":"John","year":1972}'
    // - implementation is just a wrapper around Returns(JsonEncode([]))
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    // - expects Status to be either HTTP_SUCCESS or HTTP_CREATED
    // - caller can set Handle304NotModified=TRUE for Status=HTTP_SUCCESS
    procedure Returns(const NameValuePairs: array of const;
      Status: integer = HTTP_SUCCESS; Handle304NotModified: boolean = false;
      HandleErrorAsRegularResult: boolean = false;
      const CustomHeader: RawUtf8 = ''); overload;
    /// use this method to send back any object as JSON document to the caller
    // - this method will call ObjectToJson() to compute the returned content
    // - you can customize OrmOptions, to force the returned JSON
    // object to have its TOrm nested fields serialized as true JSON
    // arrays or objects, or add an "ID_str" string field for JavaScript
    procedure Returns(Value: TObject; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = false;
      OrmOptions: TOrmWriterOptions = [];
      const CustomHeader: RawUtf8 = ''); overload;
    /// use this method to send back any variant as JSON to the caller
    // - this method will call VariantSaveJson() to compute the returned content
    procedure ReturnsJson(const Value: variant; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = false; Escape: TTextWriterKind = twJsonEscape;
      MakeHumanReadable: boolean = false; const CustomHeader: RawUtf8 = '';
      HandleErrorAsRegularResult: boolean = false);
    /// uses this method to send back directly any binary content to the caller
    // - the exact MIME type will be retrieved using GetMimeContentTypeHeader(),
    // from the supplied Blob binary buffer, and optional a file name
    // - by default, the HTTP_NOTMODIFIED process will take place, to minimize
    // bandwidth between the server and the client
    // - set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age=xxx header
    procedure ReturnBlob(const Blob: RawByteString; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = true; const FileName: TFileName = '';
      CacheControlMaxAgeSec: integer = 0);
    /// use this method to send back a file to the caller
    // - this method will let the HTTP server return the file content
    // - if Handle304NotModified is TRUE, will check the file age to ensure
    // that the file content will be sent back to the server only if it changed;
    // set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age=xxx header
    // - if ContentType is left to default '', method will guess the expected
    // mime-type from the file name extension
    // - if the file name does not exist, a generic 404 error page will be
    // returned, unless an explicit redirection is defined in Error404Redirect
    // - you can also specify the resulting file name, as downloaded and written
    // by the client browser, in the optional AttachmentFileName parameter, if
    // the URI does not match the expected file name
    procedure ReturnFile(const FileName: TFileName;
      Handle304NotModified: boolean = false; const ContentType: RawUtf8 = '';
      const AttachmentFileName: RawUtf8 = ''; const Error404Redirect: RawUtf8 = '';
      CacheControlMaxAgeSec: integer = 0);
    /// use this method to send back a file from a local folder to the caller
    // - this method will let the HTTP server return the file content
    // - if Handle304NotModified is TRUE, will check the file age to ensure
    // that the file content will be sent back to the server only if it changed
    // set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age=xxx header
    procedure ReturnFileFromFolder(const FolderName: TFileName;
      Handle304NotModified: boolean = true;
      const DefaultFileName: TFileName = 'index.html';
      const Error404Redirect: RawUtf8 = '';
      CacheControlMaxAgeSec: integer = 0); virtual; abstract;
    /// use this method notify the caller that the resource URI has changed
    // - returns a HTTP_TEMPORARYREDIRECT status with the specified location,
    // or HTTP_MOVEDPERMANENTLY if PermanentChange is TRUE
    procedure Redirect(const NewLocation: RawUtf8;
      PermanentChange: boolean = false);
    /// use this method to send back a JSON object with a "result" field
    // - this method will encode the supplied values as a {"result":"...}
    // JSON object, as such for one value:
    // $ {"result":"OneValue"}
    // (with one value, you can just call TRestClientUri.CallBackGetResult
    // method to call and decode this value)
    // or as a JSON object containing an array of values:
    // $ {"result":["One","two"]}
    // - expects Status to be either HTTP_SUCCESS or HTTP_CREATED
    // - caller can set Handle304NotModified=TRUE for Status=HTTP_SUCCESS and/or
    // set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age=xxx header
    procedure Results(const Values: array of const;
      Status: integer = HTTP_SUCCESS; Handle304NotModified: boolean = false;
      CacheControlMaxAgeSec: integer = 0);
    /// use this method if the caller expect no data, just a status
    // - just wrap the overloaded Returns() method with no result value
    // - if Status is an error code, it will call Error() method
    // - by default, calling this method will mark process as successful
    procedure Success(Status: integer = HTTP_SUCCESS); virtual;
    /// use this method to send back an error to the caller
    // - expects Status to not be HTTP_SUCCESS neither HTTP_CREATED,
    // and will send back a JSON error message to the caller, with the
    // supplied error text
    // - set CacheControlMaxAgeSec<>0 to include a Cache-Control: max-age = xxx header
    // - if no ErrorMessage is specified, will return a default text
    // corresponding to the Status code
    procedure Error(const ErrorMessage: RawUtf8 = '';
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAgeSec: integer = 0); overload; virtual;
    /// use this method to send back an error to the caller
    // - implementation is just a wrapper over Error(FormatUtf8(Format,Args))
    procedure Error(const Format: RawUtf8; const Args: array of const;
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAgeSec: integer = 0); overload;
    /// use this method to send back an error to the caller
    // - will serialize the supplied exception, with an optional error message
    procedure Error(E: Exception; const Format: RawUtf8;
      const Args: array of const; Status: integer = HTTP_BADREQUEST); overload; virtual;
  end;



{$ifndef PUREMORMOT2}
type
  TSqlUriMethod  = TUriMethod;
  TSqlUriMethods = TUriMethods;
{$endif PUREMORMOT2}




{ ************ TRestThread Background Process of a REST instance }

type
  /// a simple TThread for doing some process within the context of a REST instance
  // - inherited classes should override InternalExecute abstract method
  TRestThread = class(TThreadAbstract)
  protected
    fSafe: TOSLock;
    fRest: TRest;
    fOwnRest: boolean;
    fExecuting: boolean;
    fLog: TSynLog;
    fEvent: TSynEvent;
    /// allows customization in overriden Create (before Execute)
    fThreadName: RawUtf8;
    /// will call BeginCurrentThread/EndCurrentThread and catch exceptions
    procedure Execute; override;
    /// you should override this method with the proper process
    procedure InternalExecute; virtual; abstract;
  public
    /// initialize the thread
    // - if aOwnRest is TRUE, the supplied REST instance will be
    // owned by this thread
    constructor Create(aRest: TRest; aOwnRest, aCreateSuspended: boolean);
    /// properly terminate the thread, notifying WaitForNotExecuting
    procedure TerminatedSet; override;
    /// wait for Execute to be ended (i.e. fExecuting=false)
    // - will use the internal TEvent so that Terminate will stop it ASAP
    procedure WaitForNotExecuting(maxMS: integer = 500);
    /// finalize the thread
    // - and the associated REST instance if OwnRest is TRUE
    destructor Destroy; override;
    /// safe version of Sleep() which won't break the thread process
    // - returns TRUE if the thread was Terminated
    // - returns FALSE if successfully waited up to MS milliseconds
    function SleepOrTerminated(MS: integer): boolean;
    /// read-only access to the associated REST instance
    property Rest: TRest
      read fRest;
    /// TRUE if the associated REST instance will be owned by this thread
    property OwnRest: boolean
      read fOwnRest;
    /// a critical section is associated to this thread
    // - could be used to protect shared resources within the internal process
    property Safe: TOSLock
      read fSafe;
    /// read-only access to the REST TSynLog instance matching this thread
    // - can be used safely within InternalExecute code
    property Log: TSynLog
      read fLog;
    /// a event associated to this thread
    // - used mainly by Terminate/WaitForNotExecuting but could be used
    // for other notification purpose
    property Event: TSynEvent
      read fEvent;
    /// publishes the thread executing state (set when Execute leaves)
    property Executing: boolean
      read fExecuting;
  end;



{ ************ TOrmHistory/TOrmTableDeleted Modifications Tracked Persistence }

type
  /// common ancestor for tracking TOrm modifications
  // - e.g. TOrmHistory and TOrmVersion will inherit from this class
  // to track TOrm changes
  TOrmModification = class(TOrm)
  protected
    fModifiedRecord: TID;
    fTimestamp: TModTime;
  public
    /// returns the modified record table, as stored in ModifiedRecord
    function ModifiedTable(Model: TOrmModel): TOrmClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the record table index in the TOrmModel, as stored in ModifiedRecord
    function ModifiedTableIndex: integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns the modified record ID, as stored in ModifiedRecord
    function ModifiedID: TID;
      {$ifdef HASINLINE}inline;{$endif}
  published
    /// identifies the modified record
    // - ID and table index in TOrmModel is stored as one RecordRef integer
    // - you can use ModifiedTable/ModifiedID to retrieve the TOrm item
    // - in case of the record deletion, all matching TOrmHistory won't
    // be touched by TRestOrmServer.AfterDeleteForceCoherency(): so this
    // property is a plain TID/Int64, not a TRecordReference field
    property ModifiedRecord: TID
      read fModifiedRecord write fModifiedRecord;
    /// when the modification was recorded
    // - even if in most cases, this timestamp may be synchronized over TRest
    // instances (thanks to TRestClientUri.ServerTimestampSynchronize), it
    // is not safe to use this field as absolute: you should rather rely on
    // pure monotonic ID/RowID increasing values (see e.g. TOrmVersion)
    property Timestamp: TModTime
      read fTimestamp write fTimestamp;
  end;

  /// common ancestor for tracking changes on TOrm tables
  // - used by TRestServer.TrackChanges() method for simple fields history
  // - TRestServer.InternalUpdateEvent will use this table to store individual
  // row changes as SentDataJson, then will compress them in History BLOB
  // - note that any layout change of the tracked TOrm table (e.g. adding
  // a new property) will break the internal data format, so will void the table
  TOrmHistory = class(TOrmModification)
  protected
    fEvent: TOrmHistoryEvent;
    fSentData: RawUtf8;
    fHistory: RawBlob;
    // BLOB storage layout is: RTTIheader + offsets + recordsdata
    fHistoryModel: TOrmModel;
    fHistoryTable: TOrmClass;
    fHistoryTableIndex: integer;
    fHistoryUncompressedCount: integer;
    fHistoryAddCount: integer;
    fHistoryUncompressed: RawByteString;
    fHistoryUncompressedOffset: TIntegerDynArray;
    fHistoryAdd: TBufferWriter;
    fHistoryAddOffset: TIntegerDynArray;
  public
    /// load the change history of a given record
    // - then you can use HistoryGetLast, HistoryCount or HistoryGet() to access
    // all previous stored versions
    constructor CreateHistory(const aClient: IRestOrm; aTable: TOrmClass;
      aID: TID);
    /// finalize any internal memory
    destructor Destroy; override;
    /// override this to customize fields intialization
    class procedure InitializeFields(const Fields: array of const;
      var Json: RawUtf8); virtual;
    /// called when the associated table is created in the database
    // - create index on History(ModifiedRecord,Event) for process speed-up
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUtf8; Options: TOrmInitializeTableOptions); override;
  public
    /// prepare to access the History BLOB content
    // - ModifiedRecord should have been set to a proper value
    // - returns FALSE if the History BLOB is incorrect (e.g. TOrm
    // layout changed): caller shall flush all previous history
    function HistoryOpen(Model: TOrmModel): boolean;
    /// returns how many revisions are stored in the History BLOB
    // - HistoryOpen() or CreateHistory() should have been called before
    // - this method will ignore any previous HistoryAdd() call
    function HistoryCount: integer;
    /// retrieve an historical version
    // - HistoryOpen() or CreateHistory() should have been called before
    // - this method will ignore any previous HistoryAdd() call
    // - if Rec=nil, will only retrieve Event and Timestamp
    // - if Rec is set, will fill all simple properties of this TOrm
    function HistoryGet(Index: integer; out Event: TOrmHistoryEvent;
      out Timestamp: TModTime; Rec: TOrm): boolean; overload;
    /// retrieve an historical version
    // - HistoryOpen() or CreateHistory() should have been called before
    // - this method will ignore any previous HistoryAdd() call
    // - will fill all simple properties of the supplied TOrm instance
    function HistoryGet(Index: integer; Rec: TOrm): boolean; overload;
    /// retrieve an historical version
    // - HistoryOpen() or CreateHistory() should have been called before
    // - this method will ignore any previous HistoryAdd() call
    // - will return either nil, or a TOrm with all simple properties set
    function HistoryGet(Index: integer): TOrm; overload;
    /// retrieve the latest stored historical version
    // - HistoryOpen() or CreateHistory() should have been called before
    // - this method will ignore any previous HistoryAdd() call
    // - you should not have to use it, since a TRest.Retrieve() is faster
    function HistoryGetLast(Rec: TOrm): boolean; overload;
    /// retrieve the latest stored historical version
    // - HistoryOpen() or CreateHistory() should have been called before,
    // otherwise it will return nil
    // - this method will ignore any previous HistoryAdd() call
    // - you should not have to use it, since a TRest.Retrieve() is faster
    function HistoryGetLast: TOrm; overload;
    /// add a record content to the History BLOB
    // - HistoryOpen() should have been called before using this method -
    // CreateHistory() won't allow history modification
    // - use then HistorySave() to compress and replace the History field
    procedure HistoryAdd(Rec: TOrm; Hist: TOrmHistory);
    /// update the History BLOB field content
    // - HistoryOpen() should have been called before using this method -
    // CreateHistory() won't allow history modification
    // - if HistoryAdd() has not been used, returns false
    // - ID field should have been set for proper persistence on Server
    // - otherwise compress the data into History BLOB, deleting the oldest
    // versions if resulting size is biggger than expected, and returns true
    // - if Server is set, write save the History BLOB to database
    // - if Server and LastRec are set, its content will be compared with the
    // current record in DB (via a Retrieve() call) and stored: it will allow
    // to circumvent any issue about inconsistent use of tracking, e.g. if the
    // database has been modified directly, by-passing the ORM
    function HistorySave(const Server: IRestOrmServer;
      LastRec: TOrm = nil): boolean;
  published
    /// the kind of modification stored
    // - is heArchiveBlob when this record stores the compress BLOB in History
    // - otherwise, SentDataJson may contain the latest values as JSON
    property Event: TOrmHistoryEvent
      read fEvent write fEvent;
    /// for heAdd/heUpdate, the data is stored as JSON
    // - note that we defined a default maximum size of 4KB for this column,
    // to avoid using a CLOB here - perhaps it may not be enough for huge
    // records - feedback is welcome...
    property SentDataJson: RawUtf8
      index 4000 read fSentData write fSentData;
    /// after some events are written as individual SentData content, they
    // will be gathered and compressed within one BLOB field
    // - use HistoryOpen/HistoryCount/HistoryGet to access the stored data after
    // a call to CreateHistory() constructor
    // - as any BLOB field, this one won't be retrieved by default: use
    // explicitly TRest.RetrieveBlobFields(aRecordHistory) to get it if you
    // want to access it directly, and not via CreateHistory()
    property History: RawBlob
      read fHistory write fHistory;
  end;

  /// class-reference type (metaclass) to specify the storage table to be used
  // for tracking TOrm changes
  // - you can create your custom type from TOrmHistory, even for a
  // particular table, to split the tracked changes storage in several tables:
  // ! type
  // !  TOrmMyHistory = class(TOrmHistory);
  // - as expected by TRestServer.TrackChanges() method
  TOrmHistoryClass = class of TOrmHistory;

  /// ORM table used to store the deleted items of a versioned table
  // - the ID/RowID primary key of this table will be the version number
  // (i.e. value computed by TRestServer.InternalRecordVersionCompute),
  // mapped with the corresponding 'TableIndex shl 58' (so that e.g.
  // TRestServer.RecordVersionSynchronizeToBatch() could easily ask for the
  // deleted rows of a given table with a single WHERE clause on the ID/RowID)
  TOrmTableDeleted = class(TOrm)
  protected
    fDeleted: Int64;
  published
    /// this Deleted published field will track the deleted row
    // - defined as Int64 and not TID, to avoid the generation of the index on
    // this column, which is not needed here (all requests are about ID/RowID)
    property Deleted: Int64
      read fDeleted write fDeleted;
  end;

  /// class-reference type (metaclass) to specify the storage table to be used
  // for tracking TOrm deletion
  TOrmTableDeletedClass = class of TOrmTableDeleted;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRestThread = TRestThread;

{$endif PUREMORMOT2}


implementation


{ ************ Customize REST Execution }

{ TRestAcquireExecution }

destructor TRestAcquireExecution.Destroy;
begin
  inherited Destroy;
  Thread.Free;
end;


type
  TInterfacedObjectMultiDest = record
    instance: IInvokable;
    methods: TInterfaceFactoryMethodBits;
  end;

  TInterfacedObjectMultiDestDynArray = array of TInterfacedObjectMultiDest;

  TInterfacedObjectMulti = class;

  /// thread-safe implementation of IMultiCallbackRedirect
  TInterfacedObjectMultiList = class(TInterfacedPersistent,
    IMultiCallbackRedirect)
  protected
    fDest: TInterfacedObjectMultiDestDynArray;
    fDests: TDynArrayLocked;
    fFakeCallback: TInterfacedObjectMulti;
    procedure Redirect(const aCallback: IInvokable;
      const aMethodsNames: array of RawUtf8; aSubscribe: boolean); overload;
    procedure Redirect(const aCallback: TInterfacedObject;
      const aMethodsNames: array of RawUtf8; aSubscribe: boolean); overload;
    procedure CallBackUnRegister;
    function GetInstances(aMethod: integer; var aInstances: TPointerDynArray): PtrInt;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TInterfacedObjectMulti = class(TInterfacedObjectFakeCallback)
  protected
    fRest: TRest;
    fList: TInterfacedObjectMultiList;
    fCallBackUnRegisterNeeded: boolean;
    function FakeInvoke(const aMethod: TInterfaceMethod; const aParams: RawUtf8;
      aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean; override;
  public
    constructor Create(aRest: TRest; aFactory: TInterfaceFactory;
      aCallBackUnRegisterNeeded: boolean; out aCallbackInterface);
    destructor Destroy; override;
    procedure CallBackUnRegister;
  end;


{ TInterfacedObjectMultiList }

constructor TInterfacedObjectMultiList.Create;
begin
  inherited Create;
  fDests.DynArray.InitSpecific(TypeInfo(TInterfacedObjectMultiDestDynArray),
    fDest, ptInterface, @fDests.Count);
end;

procedure TInterfacedObjectMultiList.Redirect(const aCallback: IInvokable;
  const aMethodsNames: array of RawUtf8; aSubscribe: boolean);
var
  ndx: integer;
  new: TInterfacedObjectMultiDest;
const
  NAM: array[boolean] of string[11] = ('Unsubscribe', 'Subscribe');
begin
  if (self = nil) or
     (fFakeCallback = nil) then
    exit;
  fFakeCallback.fLogClass.Add.Log(sllDebug, 'Redirect: % % using %',
    [NAM[aSubscribe], fFakeCallback.Factory.InterfaceName,
     ObjectFromInterface(aCallback)], self);
  fFakeCallback.Factory.CheckMethodIndexes(aMethodsNames, true, new.methods);
  new.instance := aCallback;
  fDests.Safe.WriteLock;
  try
    ndx := fDests.DynArray.Find(aCallback);
    if aSubscribe then
      if ndx < 0 then
        fDests.DynArray.Add(new)
      else
        fDest[ndx] := new
    else
      fDests.DynArray.Delete(ndx);
  finally
    fDests.Safe.WriteUnLock;
  end;
end;

procedure TInterfacedObjectMultiList.Redirect(const aCallback: TInterfacedObject;
  const aMethodsNames: array of RawUtf8; aSubscribe: boolean);
var
  dest: IInvokable;
begin
  if (self = nil) or
     (fFakeCallback = nil) then
    exit;
  if aCallback = nil then
    EServiceException.RaiseUtf8('%.Redirect(nil)', [self]);
  if not aCallback.GetInterface(fFakeCallback.Factory.InterfaceGuid^, dest) then
    EServiceException.RaiseUtf8('%.Redirect [%]: % is not a %',
      [self, fFakeCallback.fName, aCallback, fFakeCallback.Factory.InterfaceName]);
  Redirect(dest, aMethodsNames, aSubscribe);
end;

procedure TInterfacedObjectMultiList.CallBackUnRegister;
begin
  fDests.Safe.WriteLock;
  try
    fDests.DynArray.ClearSafe;
  finally
    fDests.Safe.WriteUnLock;
  end;
  if fFakeCallback <> nil then
  begin
    fFakeCallback.CallBackUnRegister;
    fFakeCallback := nil; // disable any further Redirect()
  end;
end;

destructor TInterfacedObjectMultiList.Destroy;
begin
  CallBackUnRegister;
  inherited Destroy;
end;

function TInterfacedObjectMultiList.GetInstances(aMethod: integer;
  var aInstances: TPointerDynArray): PtrInt;
var
  i: integer;
  dest: ^TInterfacedObjectMultiDest;
begin // caller made fDests.Safe lock
  result := 0;
  dec(aMethod, RESERVED_VTABLE_SLOTS);
  if aMethod < 0 then
    exit;
  SetLength(aInstances, fDests.Count);
  dest := pointer(fDest);
  for i := 1 to fDests.Count do
  begin
    if aMethod in dest^.methods then
    begin
      aInstances[result] := pointer(dest^.instance);
      inc(result);
    end;
    inc(dest);
  end;
  if result <> fDests.Count then
    SetLength(aInstances, result);
end;


{ TInterfacedObjectMulti }

procedure TInterfacedObjectMulti.CallBackUnRegister;
begin
  if fCallBackUnRegisterNeeded then
  begin
    fLogClass.Add.Log(sllDebug, '%.Destroy -> Services.CallbackUnRegister(%)',
      [PClass(fList)^, fFactory.InterfaceName], self);
    fRest.Services.CallBackUnRegister(IInvokable(pointer(@fVTable)));
  end;
end;

constructor TInterfacedObjectMulti.Create(aRest: TRest;
  aFactory: TInterfaceFactory; aCallBackUnRegisterNeeded: boolean;
  out aCallbackInterface);
begin
  if aRest = nil then
    EServiceException.RaiseUtf8('%.Create(aRest=nil)', [self]);
  fRest := aRest;
  fLogClass := fRest.fLogClass;
  fName := fRest.Model.Root; // some context about the TRest running it
  fCallBackUnRegisterNeeded := aCallBackUnRegisterNeeded;
  fList := TInterfacedObjectMultiList.Create;
  fList.fFakeCallback := self;
  inherited Create(aFactory, nil, [ifoJsonAsExtended, ifoDontStoreVoidJson],
    FakeInvoke, nil);
  Get(aCallbackInterface);
end;

destructor TInterfacedObjectMulti.Destroy;
begin
  fList.CallBackUnRegister;
  inherited Destroy;
end;

function TInterfacedObjectMulti.FakeInvoke(const aMethod: TInterfaceMethod;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aFakeID: PInterfacedObjectFakeID; aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  i: PtrInt;
  exec: TInterfaceMethodExecute;
  instances, tobedeleted: TPointerDynArray;
begin
  result := inherited FakeInvoke(
    aMethod, aParams, aResult, aErrorMsg, aFakeID, aServiceCustomAnswer);
  if not result or
     (fList.fDests.Count = 0) then
    exit;
  fList.fDests.Safe.ReadLock; // we need to protect instances[]
  try
    if fList.GetInstances(aMethod.ExecutionMethodIndex, instances) = 0 then
      exit;
    exec := TInterfaceMethodExecute.Create(fFactory, @aMethod,
      [optIgnoreException]); // to use exec.ExecutedInstancesFailed
    try
      result := exec.ExecuteJson(instances,
        pointer(Join(['[', aParams, ']'])), nil);
      if exec.ExecutedInstancesFailed <> nil then
        for i := length(exec.ExecutedInstancesFailed) - 1 downto 0 do
          if exec.ExecutedInstancesFailed[i] <> '' then
          begin
            fRest.InternalLog('%.FakeInvoke I% failed due to % -> unsubscribe %',
              [ClassType, aMethod.InterfaceDotMethodName,
               exec.ExecutedInstancesFailed[i], instances[i]], sllDebug);
            PtrArrayAdd(tobedeleted, instances[i]);
          end;
    finally
      exec.Free;
    end;
  finally
    fList.fDests.Safe.ReadUnLock;
  end;
  if tobedeleted = nil then
    exit;
  fList.fDests.Safe.WriteLock;
  try
    for i := 0 to length(tobedeleted) - 1 do
      try
        fRest.InternalLog('%.FakeInvoke: I% delete unsafe %',
          [ClassType, aMethod.InterfaceDotMethodName, tobedeleted[i]], sllDebug);
        fList.fDests.DynArray.FindAndDelete(tobedeleted[i]);
      except // ignore any exception when releasing the (unstable) callback
      end;
  finally
    fList.fDests.Safe.WriteUnLock;
  end;
end;


{ ************ TRest Abstract Parent Class }

{ TRest }

function TRest.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
begin
  result := true;
  if aInterface = TypeInfo(IRestOrm) then
    IRestOrm(Obj) := fOrm
  else
    result := fServices.Resolve(aInterface, Obj);
end;

procedure TRest.SetLogClass(aClass: TSynLogClass);
begin
  fLogClass := aClass;
  fLogFamily := fLogClass.Family;
  fLogLevel := [];
  if fLogFamily <> nil then
    fLogLevel := fLogFamily.Level;
end;

procedure TRest.InternalLog(const Text: RawUtf8; Level: TSynLogLevel);
begin
  if (self <> nil) and
     (Level in fLogLevel) then
    fLogFamily.Add.LogText(Level, pointer(Text), self);
end;

procedure TRest.InternalLog(Format: PUtf8Char; const Args: array of const;
  Level: TSynLogLevel);
begin
  if (self <> nil) and
     (Level in fLogLevel) then
    fLogFamily.Add.Log(Level, Format, Args, self);
end;

procedure TRest.InternalLogResponse(const aContent: RawByteString;
  const aContext: shortstring; Level: TSynLogLevel);
begin // caller checked that (self <> nil) and (Level in fLogLevel)
  InternalLogResponse(pointer(aContent), length(aContent), aContext, Level);
end;

procedure TRest.InternalLogResponse(aContent: PUtf8Char; aContentLen: PtrInt;
  const aContext: shortstring; Level: TSynLogLevel);
var
  max: PtrInt;
begin // caller checked that (self <> nil) and (Level in fLogLevel)
  if (aContent = nil) or
     (aContentLen <= 0) then
    exit;
  max := fLogResponseMaxBytes;
  if max < MAX_LOGESCAPE then
    // safe ouput of the content, with proper escape if needed (e.g. binary)
    fLogFamily.Add.LogEscape(
      Level, '%', [aContext], aContent, aContentLen, self, max)
  else
    // direct huge UTF-8 or escaped content output - without aContext
    fLogFamily.Add.LogText(Level, aContent, aContentLen, self, max);
end;

function TRest.Enter(TextFmt: PUtf8Char; const TextArgs: array of const;
  aInstance: TObject): ISynLog;
begin
  result := nil;
  if (self = nil) or
     not (sllEnter in fLogLevel) then
    exit;
  if aInstance = nil then
    aInstance := self;
  fLogClass.EnterLocal(result, TextFmt, TextArgs, aInstance);
end;

function TRest.GetServerTimestamp(tix64: Int64): TTimeLog;
var
  tix: cardinal;
begin
  if tix64 = 0 then
    tix64 := GetTickCount64;
  tix := tix64 shr 9; // resolution change from 1 ms to 512 ms
  if fServerTimestampCacheTix = tix then
    result := fServerTimestampCacheValue.Value
  else
  begin
    fServerTimestampCacheTix := tix;
    fServerTimestampCacheValue.From(NowUtc + fServerTimestampOffset);
    result := fServerTimestampCacheValue.Value;
  end;
end;

procedure TRest.SetServerTimestamp(const Value: TTimeLog);
begin
  if Value = 0 then
    fServerTimestampOffset := 0
  else
    fServerTimestampOffset := PTimeLogBits(@Value)^.ToDateTime - NowUtc;
  if fServerTimestampOffset = 0 then
    fServerTimestampOffset := 0.000001; // retrieve server date/time only once
end;

procedure TRest.CheckAcquireExecutionCommand(Cmd: TRestServerUriContextCommand;
  Context: PUtf8Char);
begin
  if not (Cmd in [low(fAcquireExecution) .. high(fAcquireExecution)]) then
    ERestException.RaiseUtf8('Unexpected %.%(%)', [self, Context, ToText(Cmd)^]);
end;

function TRest.GetAcquireExecutionMode(
  Cmd: TRestServerUriContextCommand): TRestServerAcquireMode;
begin
  CheckAcquireExecutionCommand(Cmd, 'GetAcquireExecutionMode');
  result := fAcquireExecution[Cmd].Mode;
end;

procedure TRest.SetAcquireExecutionMode(
  Cmd: TRestServerUriContextCommand; Value: TRestServerAcquireMode);
begin
  CheckAcquireExecutionCommand(Cmd, 'SetAcquireExecutionMode');
  {$ifdef OSWINDOWS}
  if Assigned(ServiceSingle) and
     (Value = amMainThread) then
     ERestException.RaiseUtf8('%.SetAcquireExecutionMode(%, ' +
       'amMainThread) is not compatible with a Windows Service which has ' +
       'no main thread', [self, ToText(Cmd)^]);
  {$endif OSWINDOWS}
  fAcquireExecution[Cmd].Mode := Value;
end;

function TRest.GetAcquireExecutionLockedTimeOut(
  Cmd: TRestServerUriContextCommand): cardinal;
begin
  CheckAcquireExecutionCommand(Cmd, 'GetAcquireExecutionLockedTimeOut');
  result := fAcquireExecution[Cmd].LockedTimeOut;
end;

procedure TRest.SetAcquireExecutionLockedTimeOut(
  Cmd: TRestServerUriContextCommand; Value: cardinal);
begin
  CheckAcquireExecutionCommand(Cmd, 'SetAcquireExecutionLockedTimeOut');
  fAcquireExecution[Cmd].LockedTimeOut := Value;
end;

constructor TRest.Create(aModel: TOrmModel);
var
  cmd: TRestServerUriContextCommand;
begin
  if PClass(self)^ = TRest then
    ERestException.RaiseUtf8('Abstract %.Create', [self]);
  fPrivateGarbageCollector := TSynObjectListLocked.Create;
  fModel := aModel;
  for cmd := low(fAcquireExecution) to high(fAcquireExecution) do
    fAcquireExecution[cmd] := TRestAcquireExecution.Create;
  AcquireWriteMode := amLocked;
  AcquireWriteTimeOut := 5000;  // default 5 seconds
  fLogResponseMaxBytes := 2048; // for InternalLogResponse()
  SetLogClass(TSynLog);
  fRun := TRestRunThreads.Create(self);
end;

procedure TRest.SetOrmInstance(aORM: TRestOrmParent);
begin
  if fOrmInstance <> nil then
    ERestException.RaiseUtf8('%.SetOrmInstance twice', [self]);
  if (aORM = nil) or
     not aORM.GetInterface(IRestOrm, fOrm) then
    ERestException.RaiseUtf8('%.SetOrmInstance(%) is not an IRestOrm', [self, aORM]);
  fOrmInstance := aORM;
end;

destructor TRest.Destroy;
var
  cmd: TRestServerUriContextCommand;
begin
  InternalLog('TRest.Destroy %', [fModel.SafeRoot], sllInfo); // self->GPF
  if fOrm <> nil then
    // abort any (unlikely) pending TRestBatch
    fOrm.AsyncBatchStop(nil);
  fRun.Shutdown; // notify ASAP
  for cmd := low(fAcquireExecution) to high(fAcquireExecution) do
    FreeAndNilSafe(fAcquireExecution[cmd]); // calls fOrmInstance.OnEndThread
  FreeAndNilSafe(fServices);
  FreeAndNilSafe(fRun); // after fAcquireExecution+fServices
  if fOrmInstance <> nil then
    if (fOrm = nil) or
       (fOrmInstance.RefCount <> 1) then
      ERestException.RaiseUtf8('%.Destroy: %.RefCount=%: try to fix IRestOrm',
        [self, fOrmInstance, fOrmInstance.RefCount])
    else
      // avoid dubious GPF
      fOrmInstance := nil;
  fOrm := nil;
  if (fModel <> nil) and
     (fModel.Owner = self) then
    // make sure we are the Owner (TRestStorage has fModel<>nil e.g.)
    FreeAndNilSafe(fModel);
  // fPrivateGarbageCollector should be released in last position
  if fPrivateGarbageCollector <> nil then
  begin
    fPrivateGarbageCollector.ClearFromLast;
    FreeAndNilSafe(fPrivateGarbageCollector);
  end;
  // call TObject.Destroy
  inherited Destroy;
end;

var
  GlobalDefinitions: array of TRestClass;

class procedure TRest.RegisterClassNameForDefinition;
begin
  ObjArrayAddOnce(GlobalDefinitions, TObject(self)); // TClass stored as TObject
end;

procedure TRest.OnBeginCurrentThread(Sender: TThread);
begin
  fOrmInstance.BeginCurrentThread(Sender);
end;

procedure TRest.OnEndCurrentThread(Sender: TThread);
begin
  fOrmInstance.EndCurrentThread(Sender);
  // most will be done e.g. in TRestRunThreadsServer.EndCurrentThread
  if fLogFamily <> nil then
    fLogFamily.OnThreadEnded(Sender);
end;

procedure TRest.OnRestBackgroundTimerCreate;
begin
  // nothing to do by default
end;

procedure TRest.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition <> nil then
    Definition.Kind := ClassName;
end;

function TRest.DefinitionToJson(Key: cardinal): RawUtf8;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.Create;
  try
    Definition.Key := Key;
    DefinitionTo(Definition);
    result := Definition.SaveToJson;
  finally
    Definition.Free;
  end;
end;

procedure TRest.DefinitionToFile(const aJsonFile: TFileName; aKey: cardinal);
begin
  FileFromString(JsonReformat(DefinitionToJson(aKey)), aJsonFile);
end;

class function TRest.ClassFrom(aDefinition: TSynConnectionDefinition): TRestClass;
var
  ndx: PtrInt;
begin
  for ndx := 0 to length(GlobalDefinitions) - 1 do
    if GlobalDefinitions[ndx].ClassNameIs(aDefinition.Kind) then
    begin
      result := GlobalDefinitions[ndx];
      exit;
    end;
  result := nil;
end;

constructor TRest.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
begin
  Create(aModel);
end;

class function TRest.CreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean): TRest;
var
  C: TRestClass;
begin
  C := ClassFrom(aDefinition);
  if C = nil then
    ERestException.RaiseUtf8('%.CreateFrom: unknown % class - please ' +
      'add a reference to its implementation unit', [self, aDefinition.Kind]);
  result := C.RegisteredClassCreateFrom(aModel, aDefinition, aServerHandleAuthentication);
end;

class function TRest.CreateTryFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean): TRest;
var
  C: TRestClass;
begin
  C := ClassFrom(aDefinition);
  if C = nil then
    result := nil
  else
    result := C.RegisteredClassCreateFrom(aModel, aDefinition,
      aServerHandleAuthentication);
end;

class function TRest.CreateFromJson(aModel: TOrmModel;
  const aJsonDefinition: RawUtf8; aServerHandleAuthentication: boolean;
  aKey: cardinal): TRest;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.CreateFromJson(aJsonDefinition, aKey);
  try
    result := CreateFrom(aModel, Definition, aServerHandleAuthentication);
  finally
    Definition.Free;
  end;
end;

class function TRest.CreateFromFile(aModel: TOrmModel;
  const aJsonFile: TFileName; aServerHandleAuthentication: boolean;
  aKey: cardinal): TRest;
begin
  result := CreateFromJson(
    aModel, RawUtf8FromFile(aJsonFile), aServerHandleAuthentication, aKey);
end;

procedure TRest.ServicesRelease(Caller: TServiceContainer);
begin
  if (self <> nil) and
     (self.fServices = Caller) then
    FreeAndNil(fServices);
end;


{$ifndef PUREMORMOT2}
// backward compatibility methods redirections

{ --- redirect to TRestOrm homonymous methods }

function TRest.TableRowCount(Table: TOrmClass): Int64;
begin
  result := fOrm.TableRowCount(Table);
end;

function TRest.TableHasRows(Table: TOrmClass): boolean;
begin
  result := fOrm.TableHasRows(Table);
end;

function TRest.TableMaxID(Table: TOrmClass): TID;
begin
  result := fOrm.TableMaxID(Table);
end;

function TRest.MemberExists(Table: TOrmClass; ID: TID): boolean;
begin
  result := fOrm.MemberExists(Table, ID);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName,
  WhereClause: RawUtf8): RawUtf8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClause);
end;

function TRest.OneFieldValueInt64(Table: TOrmClass; const FieldName,
  WhereClause: RawUtf8; Default: Int64): Int64;
begin
  result := fOrm.OneFieldValueInt64(Table, FieldName, WhereClause, Default);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): RawUtf8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, FormatSqlWhere, BoundsSqlWhere);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const): RawUtf8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClauseFmt);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  const WhereClauseFmt: RawUtf8; const Args, Bounds: array of const;
  out Data: Int64): boolean;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClauseFmt, Args, Bounds, Data);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUtf8;
  WhereID: TID): RawUtf8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereID);
end;

function TRest.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
  const WhereClause: RawUtf8): boolean;
begin
  result := fOrm.MultiFieldValue(Table, FieldName, FieldValue, WhereClause);
end;

function TRest.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUtf8; var FieldValue: array of RawUtf8;
  WhereID: TID): boolean;
begin
  result := fOrm.MultiFieldValue(Table, FieldName, FieldValue, WhereID);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUtf8; const WhereClause: RawUtf8; out Data: TRawUtf8DynArray): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Data);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUtf8; const WhereClause: RawUtf8; var Data: TInt64DynArray;
  SQL: PRawUtf8): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Data, SQL);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUtf8; const WhereClause: RawUtf8; const Separator: RawUtf8): RawUtf8;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Separator);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUtf8; Strings: TStrings; IDToIndex: PID): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Strings, IDToIndex);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUtf8; const WhereClause: RawUtf8): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClause);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUtf8; const WhereClauseFormat: RawUtf8;
  const BoundsSqlWhere: array of const): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClauseFormat, BoundsSqlWhere);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUtf8; const WhereClauseFormat: RawUtf8;
  const Args, Bounds: array of const): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClauseFormat, Args, Bounds);
end;

function TRest.FtsMatch(Table: TOrmFts3Class;
  const WhereClause: RawUtf8; var DocID: TIDDynArray): boolean;
begin
  result := fOrm.FtsMatch(Table, WhereClause, DocID);
end;

function TRest.FtsMatch(Table: TOrmFts3Class;
  const MatchClause: RawUtf8; var DocID: TIDDynArray;
  const PerFieldWeight: array of double; limit, offset: integer): boolean;
begin
  result := fOrm.FtsMatch(Table, MatchClause, DocID, PerFieldWeight, limit, offset);
end;

function TRest.MainFieldValue(Table: TOrmClass; ID: TID;
  ReturnFirstIfNoUnique: boolean): RawUtf8;
begin
  result := fOrm.MainFieldValue(Table, ID, ReturnFirstIfNoUnique);
end;

function TRest.MainFieldID(Table: TOrmClass; const Value: RawUtf8): TID;
begin
  result := fOrm.MainFieldID(Table, Value);
end;

function TRest.MainFieldIDs(Table: TOrmClass;
  const Values: array of RawUtf8; out IDs: TIDDynArray): boolean;
begin
  result := fOrm.MainFieldIDs(Table, Values, IDs);
end;

function TRest.Retrieve(const SqlWhere: RawUtf8; Value: TOrm;
  const FieldsCsv: RawUtf8): boolean;
begin
  result := fOrm.Retrieve(SqlWhere, Value, FieldsCsv);
end;

function TRest.Retrieve(const WhereClauseFmt: RawUtf8;
  const Args, Bounds: array of const; Value: TOrm;
  const FieldsCsv: RawUtf8): boolean;
begin
  result := fOrm.Retrieve(WhereClauseFmt, Args, Bounds, Value, FieldsCsv);
end;

function TRest.Retrieve(aID: TID; Value: TOrm; ForUpdate: boolean): boolean;
begin
  result := fOrm.Retrieve(aID, Value, ForUpdate);
end;

function TRest.Retrieve(Reference: TRecordReference; ForUpdate: boolean): TOrm;
begin
  result := fOrm.Retrieve(Reference, ForUpdate);
end;

function TRest.Retrieve(aPublishedRecord, aValue: TOrm): boolean;
begin
  result := fOrm.Retrieve(aPublishedRecord, aValue);
end;

function TRest.RetrieveList(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const FieldsCsv: RawUtf8): TObjectList;
begin
  result := fOrm.RetrieveList(Table, FormatSqlWhere, BoundsSqlWhere, FieldsCsv);
end;

function TRest.RetrieveListJson(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const FieldsCsv: RawUtf8; aForceAjax: boolean): RawJson;
begin
  result := fOrm.RetrieveListJson(Table, FormatSqlWhere, BoundsSqlWhere,
    FieldsCsv, aForceAjax);
end;

function TRest.RetrieveListJson(Table: TOrmClass;
  const SqlWhere: RawUtf8; const FieldsCsv: RawUtf8;
  aForceAjax: boolean): RawJson;
begin
  result := fOrm.RetrieveListJson(Table, SqlWhere, FieldsCsv, aForceAjax);
end;

function TRest.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName, FieldsCsv: RawUtf8;
  FirstRecordID: PID; LastRecordID: PID): variant;
begin
  result := fOrm.RetrieveDocVariantArray(Table, ObjectName, FieldsCsv,
    FirstRecordID, LastRecordID);
end;

function TRest.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName: RawUtf8; const FormatSqlWhere: RawUtf8; const BoundsSqlWhere:
  array of const; const FieldsCsv: RawUtf8; FirstRecordID: PID;
  LastRecordID: PID): variant;
begin
  result := fOrm.RetrieveDocVariantArray(Table, ObjectName, FormatSqlWhere,
    BoundsSqlWhere, FieldsCsv, FirstRecordID, LastRecordID);
end;

function TRest.RetrieveOneFieldDocVariantArray(Table: TOrmClass;
  const FieldName, FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const): variant;
begin
  result := fOrm.RetrieveOneFieldDocVariantArray(Table, FieldName,
    FormatSqlWhere, BoundsSqlWhere);
end;

function TRest.RetrieveDocVariant(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const FieldsCsv: RawUtf8): variant;
begin
  result := fOrm.RetrieveDocVariant(Table, FormatSqlWhere, BoundsSqlWhere,
    FieldsCsv);
end;

function TRest.RetrieveListObjArray(var ObjArray; Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const FieldsCsv: RawUtf8): boolean;
begin
  result := fOrm.RetrieveListObjArray(ObjArray, Table, FormatSqlWhere,
    BoundsSqlWhere, FieldsCsv);
end;

procedure TRest.AppendListAsJsonArray(Table: TOrmClass;
  const FormatSqlWhere: RawUtf8; const BoundsSqlWhere: array of const;
  const OutputFieldName: RawUtf8; W: TOrmWriter; const FieldsCsv: RawUtf8);
begin
  fOrm.AppendListAsJsonArray(Table, FormatSqlWhere, BoundsSqlWhere,
    OutputFieldName, W, FieldsCsv);
end;

function TRest.RTreeMatch(DataTable: TOrmClass;
  const DataTableBlobFieldName: RawUtf8; RTreeTable: TOrmRTreeClass;
  const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
begin
  result := fOrm.RTreeMatch(DataTable, DataTableBlobFieldName, RTreeTable,
    DataTableBlobField, DataID);
end;

function TRest.ExecuteList(const Tables: array of TOrmClass;
  const SQL: RawUtf8): TOrmTable;
begin
  result := fOrm.ExecuteList(Tables, SQL);
end;

function TRest.ExecuteJson(const Tables: array of TOrmClass;
  const SQL: RawUtf8; ForceAjax: boolean; ReturnedRowCount: PPtrInt): RawJson;
begin
  result := fOrm.ExecuteJson(Tables, SQL, ForceAjax, ReturnedRowCount);
end;

function TRest.Execute(const aSql: RawUtf8): boolean;
begin
  result := fOrm.Execute(aSql);
end;

function TRest.ExecuteFmt(const SqlFormat: RawUtf8;
  const Args: array of const): boolean;
begin
  result := fOrm.ExecuteFmt(SqlFormat, Args);
end;

function TRest.ExecuteFmt(const SqlFormat: RawUtf8;
  const Args, Bounds: array of const): boolean;
begin
  result := fOrm.ExecuteFmt(SqlFormat, Args, Bounds);
end;

function TRest.UnLock(Table: TOrmClass; aID: TID): boolean;
begin
  result := fOrm.UnLock(Table, aID);
end;

function TRest.UnLock(Rec: TOrm): boolean;
begin
  result := fOrm.UnLock(Rec);
end;

function TRest.Add(Value: TOrm; SendData: boolean; ForceID: boolean;
  DoNotAutoComputeFields: boolean): TID;
begin
  result := fOrm.Add(Value, SendData, ForceID, DoNotAutoComputeFields);
end;

function TRest.Add(Value: TOrm; const CustomCsvFields: RawUtf8;
  ForceID: boolean; DoNotAutoComputeFields: boolean): TID;
begin
  result := fOrm.Add(Value, CustomCsvFields, ForceID, DoNotAutoComputeFields);
end;

function TRest.Add(Value: TOrm; const CustomFields: TFieldBits;
  ForceID: boolean; DoNotAutoComputeFields: boolean): TID;
begin
  result := fOrm.Add(Value, CustomFields, ForceID, DoNotAutoComputeFields);
end;

function TRest.AddWithBlobs(Value: TOrm; ForceID: boolean;
  DoNotAutoComputeFields: boolean): TID;
begin
  result := fOrm.AddWithBlobs(Value, ForceID, DoNotAutoComputeFields);
end;

function TRest.AddSimple(aTable: TOrmClass;
  const aSimpleFields: array of const; ForcedID: TID): TID;
begin
  result := fOrm.AddSimple(aTable, aSimpleFields, ForcedID);
end;

function TRest.Update(Value: TOrm; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): boolean;
begin
  result := fOrm.Update(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRest.Update(Value: TOrm; const CustomCsvFields: RawUtf8;
  DoNotAutoComputeFields: boolean): boolean;
begin
  result := fOrm.Update(Value, CustomCsvFields, DoNotAutoComputeFields);
end;

function TRest.Update(aTable: TOrmClass; aID: TID;
  const aSimpleFields: array of const): boolean;
begin
  result := fOrm.Update(aTable, aID, aSimpleFields);
end;

function TRest.AddOrUpdate(Value: TOrm; ForceID: boolean): TID;
begin
  result := fOrm.AddOrUpdate(Value, ForceID);
end;

function TRest.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; const FieldValue: array of const): boolean;
begin
  result := fOrm.UpdateField(Table, ID, FieldName, FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUtf8; const WhereFieldValue: array of const;
  const FieldName: RawUtf8; const FieldValue: array of const): boolean;
begin
  result := fOrm.UpdateField(Table, WhereFieldName, WhereFieldValue, FieldName,
    FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateField(Table, ID, FieldName, FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUtf8; const WhereFieldValue: variant;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateField(Table, WhereFieldName, WhereFieldValue, FieldName,
    FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass; const IDs: array of TID;
  const FieldName: RawUtf8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateFieldAt(Table, IDs, FieldName, FieldValue);
end;

function TRest.UpdateFieldIncrement(Table: TOrmClass; ID: TID;
  const FieldName: RawUtf8; Increment: Int64): boolean;
begin
  result := fOrm.UpdateFieldIncrement(Table, ID, FieldName, Increment);
end;

function TRest.RecordCanBeUpdated(Table: TOrmClass; ID: TID;
  Action: TOrmEvent; ErrorMsg: PRawUtf8): boolean;
begin
  result := fOrm.RecordCanBeUpdated(Table, ID, Action, ErrorMsg);
end;

function TRest.Delete(Table: TOrmClass; ID: TID): boolean;
begin
  result := fOrm.Delete(Table, ID);
end;

function TRest.Delete(Table: TOrmClass; const SqlWhere: RawUtf8): boolean;
begin
  result := fOrm.Delete(Table, SqlWhere);
end;

function TRest.Delete(Table: TOrmClass; const FormatSqlWhere: RawUtf8;
  const BoundsSqlWhere: array of const): boolean;
begin
  result := fOrm.Delete(Table, FormatSqlWhere, BoundsSqlWhere);
end;

function TRest.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; out BlobData: RawBlob): boolean;
begin
  result := fOrm.RetrieveBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; out BlobStream: TCustomMemoryStream): boolean;
begin
  result := fOrm.RetrieveBlob(Table, aID, BlobFieldName, BlobStream);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; const BlobData: RawBlob): boolean;
begin
  result := fOrm.UpdateBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; BlobData: TStream): boolean;
begin
  result := fOrm.UpdateBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUtf8; BlobData: pointer; BlobSize: integer): boolean;
begin
  result := fOrm.UpdateBlob(Table, aID, BlobFieldName, BlobData, BlobSize);
end;

function TRest.UpdateBlobFields(Value: TOrm): boolean;
begin
  result := fOrm.UpdateBlobFields(Value);
end;

function TRest.RetrieveBlobFields(Value: TOrm): boolean;
begin
  result := fOrm.RetrieveBlobFields(Value);
end;

function TRest.TransactionBegin(aTable: TOrmClass; SessionID: cardinal): boolean;
begin
  result := fOrm.TransactionBegin(aTable, SessionID);
end;

function TRest.TransactionActiveSession: cardinal;
begin
  result := fOrm.TransactionActiveSession;
end;

procedure TRest.Commit(SessionID: cardinal; RaiseException: boolean);
begin
  fOrm.Commit(SessionID, RaiseException);
end;

procedure TRest.RollBack(SessionID: cardinal);
begin
  fOrm.RollBack(SessionID);
end;

procedure TRest.WriteLock;
begin
  fOrm.WriteLock;
end;

procedure TRest.WriteUnLock;
begin
  fOrm.WriteUnLock;
end;

function TRest.BatchSend(Batch: TRestBatch; var Results: TIDDynArray): integer;
begin
  result := fOrm.BatchSend(Batch, Results);
end;

function TRest.BatchSend(Batch: TRestBatch): integer;
begin
  result := fOrm.BatchSend(Batch);
end;

function TRest.AsyncBatchStart(Table: TOrmClass;
  SendSeconds: integer; PendingRowThreshold: integer;
  AutomaticTransactionPerRow: integer; Options: TRestBatchOptions): boolean;
begin
  result := fOrm.AsyncBatchStart(Table, SendSeconds, PendingRowThreshold,
    AutomaticTransactionPerRow, Options);
end;

function TRest.AsyncBatchStop(Table: TOrmClass): boolean;
begin
  result := fOrm.AsyncBatchStop(Table);
end;

function TRest.AsyncBatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): integer;
begin
  result := fOrm.AsyncBatchAdd(Value, SendData, ForceID, CustomFields,
    DoNotAutoComputeFields);
end;

function TRest.AsyncBatchRawAdd(Table: TOrmClass;
  const SentData: RawUtf8): integer;
begin
  result := fOrm.AsyncBatchRawAdd(Table, SentData);
end;

procedure TRest.AsyncBatchRawAppend(Table: TOrmClass; SentData: TJsonWriter);
begin
  fOrm.AsyncBatchRawAppend(Table, SentData);
end;

function TRest.AsyncBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  result := fOrm.AsyncBatchUpdate(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRest.AsyncBatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  result := fOrm.AsyncBatchDelete(Table, ID);
end;

function TRest.Cache: TOrmCache;
begin
  result := fOrm.Cache;
end;

function TRest.CacheOrNil: TOrmCache;
begin
  result := fOrm.CacheOrNil;
end;

function TRest.CacheWorthItForTable(aTableIndex: cardinal): boolean;
begin
  result := fOrm.CacheWorthItForTable(aTableIndex);
end;


{ --- redirect to TRestRunThreads methods }

function TRest.EnsureBackgroundTimerExists: TRestBackgroundTimer;
begin
  if self = nil then
    result := nil
  else
    result := fRun.EnsureBackgroundTimerExists;
end;

function TRest.NewBackgroundThreadMethod(const Format: RawUtf8;
   const Args: array of const): TSynBackgroundThreadMethod;
begin
  if self = nil then
    result := nil
  else
    result := fRun.NewBackgroundThreadMethod(Format, Args);
end;

function TRest.NewParallelProcess(ThreadCount: integer; const Format: RawUtf8;
  const Args: array of const): TSynParallelProcess;
begin
  if self = nil then
    result := nil
  else
    result := fRun.NewParallelProcess(ThreadCount, Format, Args);
end;

function TRest.NewBackgroundThreadProcess(
  const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  const Format: RawUtf8; const Args: array of const;
  aStats: TSynMonitorClass): TSynBackgroundThreadProcess;
begin
  if self = nil then
    result := nil
  else
    result := fRun.NewBackgroundThreadProcess(
      aOnProcess, aOnProcessMS, Format, Args);
end;

function TRest.TimerEnable(const aOnProcess: TOnSynBackgroundTimerProcess;
  aOnProcessSecs: cardinal): TRestBackgroundTimer;
begin
  if self = nil then
    result := nil
  else
    result := fRun.TimerEnable(aOnProcess, aOnProcessSecs);
end;

function TRest.TimerDisable(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  if self = nil then
    result := false
  else
    result := fRun.TimerDisable(aOnProcess);
end;

function TRest.SystemUseTrack(periodSec: integer): TSystemUse;
begin
  if self = nil then
    result := nil
  else
    result := fRun.SystemUseTrack(periodSec);
end;

procedure TRest.BeginCurrentThread(Sender: TThread);
begin
  if self <> nil then
    fRun.BeginCurrentThread(Sender);
end;

procedure TRest.EndCurrentThread(Sender: TThread);
begin
  if self <> nil then
    fRun.EndCurrentThread(Sender);
end;

procedure TRest.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
begin
  if self <> nil then
    fRun.AsyncRedirect(
      aGuid, aDestinationInterface, aCallbackInterface, aOnResult);
end;

procedure TRest.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
begin
  if self <> nil then
    fRun.AsyncRedirect(
      aGuid, aDestinationInstance, aCallbackInterface, aOnResult);
end;

procedure TRest.AsyncInterning(Interning: TRawUtf8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if self <> nil then
    fRun.AsyncInterning(
      Interning, InterningMaxRefCount, PeriodMinutes);
end;

function TRest.MultiRedirect(const aGuid: TGuid; out aCallbackInterface;
  aCallBackUnRegisterNeeded: boolean): IMultiCallbackRedirect;
begin
  if self = nil then
    result := nil
  else
    result := fRun.MultiRedirect(aGuid, aCallbackInterface,
      aCallBackUnRegisterNeeded);
end;

function TRest.BackgroundTimer: TRestBackgroundTimer;
begin
  if (self = nil) or
     (fRun = nil) then
    result := nil
  else
    result := fRun.fBackgroundTimer;
end;

{$endif PUREMORMOT2}


function ToText(cmd: TRestServerUriContextCommand): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestServerUriContextCommand), ord(cmd));
end;


{ TInterfacedObjectAsync }

type
  TInterfacedObjectAsync = class(TInterfacedObjectFakeCallback)
  protected
    fTimer: TRestBackgroundTimer;
    fDest: IInvokable;
    fOnResult: TOnAsyncRedirectResult;
    function FakeInvoke(const aMethod: TInterfaceMethod; const aParams: RawUtf8;
      aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean; override;
  public
    constructor Create(aTimer: TRestBackgroundTimer;
      aFactory: TInterfaceFactory; const aDestinationInterface: IInvokable;
      out aCallbackInterface; const aOnResult: TOnAsyncRedirectResult);
  end;

  TInterfacedObjectAsyncCall = packed record
    Factory: TInterfaceFactory;
    Method: PInterfaceMethod;
    Instance: pointer; // weak IInvokable reference
    Params: RawUtf8;
    OnOutputParamsCopy: RawUtf8;
    OnOutput: TOnAsyncRedirectResult;
  end;

constructor TInterfacedObjectAsync.Create(aTimer: TRestBackgroundTimer;
  aFactory: TInterfaceFactory; const aDestinationInterface: IInvokable;
  out aCallbackInterface; const aOnResult: TOnAsyncRedirectResult);
begin
  fTimer := aTimer;
  fLogClass := fTimer.fRest.fLogClass;
  fName := fTimer.fThreadName;
  fDest := aDestinationInterface;
  fOnResult := aOnResult;
  inherited Create(aFactory, nil, [ifoJsonAsExtended, ifoDontStoreVoidJson],
    FakeInvoke, nil);
  Get(aCallbackInterface);
end;

function TInterfacedObjectAsync.FakeInvoke(const aMethod: TInterfaceMethod;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aFakeID: PInterfacedObjectFakeID;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  msg: RawUtf8;
  call: TInterfacedObjectAsyncCall;
begin
  result := inherited FakeInvoke(
    aMethod, aParams, aResult, aErrorMsg, aFakeID, aServiceCustomAnswer);
  if not result then
    exit;
  call.Factory := fFactory;
  call.Method := @aMethod;
  call.Instance := pointer(fDest);
  call.Params := aParams;
  if Assigned(fOnResult) then
  begin
    FastSetString(call.OnOutputParamsCopy, pointer(aParams), length(aParams));
    call.OnOutput := fOnResult;
  end
  else
    call.OnOutput := nil;
  msg := BinarySave(@call, TypeInfo(TInterfacedObjectAsyncCall), [rkRecord]);
  result := fTimer.EnQueue(fTimer.AsyncBackgroundExecute, msg, true);
end;


{ ************ TRestBackgroundTimer for Multi-Thread Process }

{ TRestBackgroundTimer }

constructor TRestBackgroundTimer.Create(aRest: TRest;
  const aThreadName: RawUtf8; aStats: TSynMonitorClass);
var
  aName: RawUtf8;
begin
  if aRest = nil then
    ERestException.RaiseUtf8('%.Create(aRest=nil,"%")', [self, aThreadName]);
  fRest := aRest;
  if aThreadName <> '' then
    aName := aThreadName
  else
    Make([fRest.Model.Root, ' ', ClassType], aName);
  inherited Create(aName,
    fRest.fRun.BeginCurrentThread, fRest.fRun.EndCurrentThread, aStats);
end;

destructor TRestBackgroundTimer.Destroy;
begin
  AsyncBatchStop(nil);
  inherited Destroy;
end;

procedure TRestBackgroundTimer.SystemUseBackgroundExecute(
  Sender: TSynBackgroundTimer; const Msg: RawUtf8);
begin
  TSystemUse.Current({createifnone=}false).OnTimerExecute(Sender);
end;

function TRestBackgroundTimer.AsyncBatchIndex(aTable: TOrmClass): PtrInt;
begin
  if (self = nil) or
     (fBackgroundBatch = nil) then
    result := -1
  else
  begin
    result := fRest.Model.GetTableIndexExisting(aTable);
    if (result >= length(fBackgroundBatch)) or
       (fBackgroundBatch[result] = nil) then
      result := -1;
  end;
end;

function TRestBackgroundTimer.AsyncBatchLocked(aTable: TOrmClass;
  out aBatch: TRestBatchLocked): boolean;
var
  b: PtrInt;
begin
  b := AsyncBatchIndex(aTable);
  if b >= 0 then
  begin
    aBatch := fBackgroundBatch[b];
    aBatch.Safe.Lock;
    result := true;
  end
  else
    result := false;
end;

procedure TRestBackgroundTimer.AsyncBatchUnLock(aBatch: TRestBatchLocked);
begin
  try
    if aBatch.Count >= aBatch.Threshold then
      ExecuteNow(AsyncBatchExecute);
  finally
    aBatch.Safe.UnLock;
  end;
end;

procedure TRestBackgroundTimer.AsyncBatchExecute(Sender: TSynBackgroundTimer;
  const Msg: RawUtf8);
var
  json, tablename: RawUtf8;
  batch: TRestBatchLocked;
  table: TOrmClass;
  b: PtrInt;
  count, status: integer;
  res: TIDDynArray;
  log: ISynLog; // for Enter auto-leave to work with FPC /Delphi 10.4+
begin
  try
    // send any pending json
    for b := 0 to length(fBackgroundBatch) - 1 do
    begin
      batch := fBackgroundBatch[b];
      if batch.Count = 0 then
        continue;
      json := '';
      batch.Safe.Lock;
      try
        // locked copy of the batch to local variables, then reset it
        table := batch.Table;
        count := batch.Count;
        if count > 0 then
        try
          if ({%H-}log = nil) and
             (fRest.fLogClass <> nil) then
            fRest.fLogClass.EnterLocal(log, 'AsyncBatchExecute % count=%',
              [table, count], self);
          batch.PrepareForSending(json);
        finally
          batch.Reset;
        end;
      finally
        batch.Safe.UnLock;
      end;
      // inlined TRest.BatchSend for lower contention
      if json <> '' then
        try
          // json layout is '{"Table":["cmd":values,...]}'
          status := fRest.Orm.BatchSend(table, json, res, count);
          // BatchSend() may take a while
          fRest.InternalLog(
            'AsyncBatchExecute % EngineBatchSend=%', [table, status]);
        except
          on E: Exception do
            fRest.InternalLog('% during AsyncBatchExecute %',
              [PClass(E)^, table], sllWarning);
        end;
    end;
  finally
    if IdemPChar(pointer(Msg), 'FREE@') then
    begin
      // from AsyncBatchStop()
      fRest.InternalLog('AsyncBatchExecute %', [Msg]);
      tablename := copy(Msg, 6, 127);
      if tablename = '' then
        // AsyncBatchStop(nil)
        ObjArrayClear(fBackgroundBatch, true)
      else
      begin
        // AsyncBatchStop(table)
        b := fRest.Model.GetTableIndex(tablename);
        if b < length(fBackgroundBatch) then
          FreeAndNil(fBackgroundBatch[b]);
      end;
    end;
  end;
end;

function TRestBackgroundTimer.AsyncBatchStart(Table: TOrmClass;
  SendSeconds, PendingRowThreshold, AutomaticTransactionPerRow: integer;
  Options: TRestBatchOptions): boolean;
var
  b: PtrInt;
begin
  result := false;
  if (self = nil) or
     (SendSeconds <= 0) then
    exit;
  b := fRest.Model.GetTableIndexExisting(Table);
  if (fBackgroundBatch <> nil) and
     (fBackgroundBatch[b] <> nil) then
    // already defined for this Table
    exit;
  fRest.InternalLog('AsyncBatchStart(%,%,%)',
    [Table, SendSeconds, PendingRowThreshold], sllDebug);
  Enable(AsyncBatchExecute, SendSeconds);
  if fBackgroundBatch = nil then
    SetLength(fBackgroundBatch, fRest.Model.TablesMax + 1);
  fBackgroundBatch[b] := TRestBatchLocked.Create(
    fRest.Orm, Table, AutomaticTransactionPerRow, Options);
  fBackgroundBatch[b].Threshold := PendingRowThreshold;
  result := true;
end;

function TRestBackgroundTimer.AsyncBatchStop(Table: TOrmClass): boolean;
var
  b: PtrInt;
  start, tix, timeout: Int64; // SleepStep() uses ms resolution
  {%H-}log: ISynLog;
begin
  result := false;
  if (self = nil) or
     (fBackgroundBatch = nil) then
    exit;
  fRest.fLogClass.EnterLocal(log, 'AsyncBatchStop(%)', [Table], self);
  start := mormot.core.os.GetTickCount64;
  timeout := start + 5000;
  if Table = nil then
  begin
    // as called from TRest.Destroy
    if not EnQueue(AsyncBatchExecute, 'free@', true) then
      exit;
    repeat
      SleepHiRes(1); // wait for all batchs to be released
    until (fBackgroundBatch = nil) or
          (mormot.core.os.GetTickCount64 > timeout);
    result := Disable(AsyncBatchExecute);
  end
  else
  begin
    // wait for regular TRestBatch process
    b := AsyncBatchIndex(Table);
    if (b < 0) or
       not EnQueue(AsyncBatchExecute, 'free@' + Table.SqlTableName, true) then
      exit;
    repeat
      tix := SleepStep(start); // wait for all pending rows to be sent
    until (fBackgroundBatch[b] = nil) or
          (tix > timeout);
    result := true;
    for b := 0 to length(fBackgroundBatch) - 1 do
      if fBackgroundBatch[b] <> nil then
        exit; // there are still some pending batchs
    result := Disable(AsyncBatchExecute);
    if result then
      ObjArrayClear(fBackgroundBatch, true); // all batches are done
  end;
end;

function TRestBackgroundTimer.AsyncBatchAdd(Value: TOrm;
  SendData, ForceID: boolean; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Value = nil) then
    exit;
  fRest.InternalLog('AsyncBatchAdd %', [Value], sllDebug);
  if AsyncBatchLocked(Value.RecordClass, b) then
  try
    result := b.Add(Value, SendData, ForceID, CustomFields, DoNotAutoComputeFields);
  finally
    AsyncBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsyncBatchRawAdd(Table: TOrmClass;
  const SentData: RawUtf8): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Table = nil) then
    exit;
  fRest.InternalLog('AsyncBatchRawAdd % %', [Table, SentData], sllDebug);
  if AsyncBatchLocked(Table, b) then
  try
    result := b.RawAdd(SentData);
  finally
    AsyncBatchUnLock(b);
  end;
end;

procedure TRestBackgroundTimer.AsyncBatchRawAppend(Table: TOrmClass;
  SentData: TJsonWriter);
var
  b: TRestBatchLocked;
begin
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Table = nil) or
     (SentData = nil) then
    exit;
  fRest.InternalLog('AsyncBatchRawAppend %', [Table], sllDebug);
  if AsyncBatchLocked(Table, b) then
  try
    b.RawAppend.AddNoJsonEscape(SentData);
  finally
    AsyncBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsyncBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Value = nil) then
    exit;
  fRest.InternalLog('AsyncBatchUpdate %', [Value], sllDebug);
  if AsyncBatchLocked(Value.RecordClass, b) then
  try
    result := b.Update(Value, CustomFields, DoNotAutoComputeFields);
  finally
    AsyncBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsyncBatchDelete(Table: TOrmClass;
  ID: TID): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) then
    exit;
  fRest.InternalLog('AsyncBatchDelete % %', [Table, ID], sllDebug);
  if AsyncBatchLocked(Table, b) then
  try
    result := b.Delete(Table, ID);
  finally
    AsyncBatchUnLock(b);
  end;
end;

procedure TRestBackgroundTimer.AsyncBackgroundExecute(
  Sender: TSynBackgroundTimer; const Msg: RawUtf8);
var
  exec: TInterfaceMethodExecute;
  call: TInterfacedObjectAsyncCall;
  o: PRawUtf8;
  output: RawUtf8;
  {%H-}log: ISynLog;
begin
  if not RecordLoad(call, Msg, TypeInfo(TInterfacedObjectAsyncCall)) then
    exit; // invalid message (e.g. periodic execution)
  fRest.fLogClass.EnterLocal(log, 'AsyncBackgroundExecute I% %',
    [call.Method^.InterfaceDotMethodName, call.Params], self);
  exec := TInterfaceMethodExecute.Create(call.Factory, call.Method, []);
  try
    if Assigned(call.OnOutput) then
      o := @output
    else
      o := nil;
    if not exec.ExecuteJsonCallback(call.Instance, call.Params, o) then
      fRest.InternalLog('%.AsyncBackgroundExecute I%: ExecuteJsonCallback failed',
        [ClassType, call.Method^.InterfaceDotMethodName], sllWarning)
    else if o <> nil then
      call.OnOutput(call.Method^,
        IInvokable(call.Instance), call.OnOutputParamsCopy, output);
  finally
    exec.Free;
  end;
end;

procedure TRestBackgroundTimer.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
var
  factory: TInterfaceFactory;
begin
  factory := TInterfaceFactory.Get(aGuid);
  if factory = nil then
    EServiceException.RaiseUtf8('%.AsyncRedirect: unknown %',
      [self, GuidToShort(aGuid)]);
  if aDestinationInterface = nil then
    EServiceException.RaiseUtf8('%.AsyncRedirect(nil)', [self]);
  fRest.InternalLog('AsyncRedirect % to % using %',
    [factory.InterfaceName, ObjectFromInterface(aDestinationInterface), self]);
  Enable(AsyncBackgroundExecute, 3600);
  TInterfacedObjectAsync.Create(self, factory, aDestinationInterface,
    aCallbackInterface, aOnResult);
end;

procedure TRestBackgroundTimer.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
var
  dest: IInvokable;
begin
  if aDestinationInstance = nil then
    EServiceException.RaiseUtf8('%.AsyncRedirect(nil)', [self]);
  if not aDestinationInstance.GetInterface(aGuid, dest) then
    EServiceException.RaiseUtf8('%.AsyncRedirect [%]: % is not a %',
      [self, fThreadName, aDestinationInstance, GuidToShort(aGuid)]);
  AsyncRedirect(aGuid, dest, aCallbackInterface, aOnResult);
end;

procedure TRestBackgroundTimer.AsyncBackgroundInterning(
  Sender: TSynBackgroundTimer; const Msg: RawUtf8);
var
  i: PtrInt;
  claimed, total: integer;
  start: Int64;
begin
  QueryPerformanceMicroSeconds(start);
  claimed := 0;
  fBackgroundInterningSafe.Lock;
  try
    for i := 0 to high(fBackgroundInterning) do
      inc(claimed, fBackgroundInterning[i].Clean(fBackgroundInterningMaxRefCount));
    if claimed = 0 then
      exit; // nothing to collect
    total := claimed;
    for i := 0 to high(fBackgroundInterning) do
      inc(total, fBackgroundInterning[i].Count);
  finally
    fBackgroundInterningSafe.UnLock;
  end;
  fRest.InternalLog(
    '%.AsyncInterning: Clean(%) claimed %/% strings from % pools in %',
    [ClassType, fBackgroundInterningMaxRefCount, claimed, total,
     length(fBackgroundInterning), MicroSecFrom(start)], sllDebug);
end;

procedure TRestBackgroundTimer.AsyncInterning(Interning: TRawUtf8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if (self = nil) or
     (Interning = nil) then
    exit;
  fBackgroundInterningSafe.Lock; // AsyncBackgroundInterning running (unlikely)
  try
    if (InterningMaxRefCount <= 0) or
       (PeriodMinutes <= 0) then
    begin
      ObjArrayDelete(fBackgroundInterning, Interning);
      exit;
    end;
    fBackgroundInterningMaxRefCount := InterningMaxRefCount;
    ObjArrayAddOnce(fBackgroundInterning, Interning);
  finally
    fBackgroundInterningSafe.UnLock;
  end;
  Enable(AsyncBackgroundInterning, PeriodMinutes * 60);
end;



{ ************ RESTful Authentication Support }

{ TAuthGroup }

function TAuthGroup.GetOrmAccessRights: TOrmAccessRights;
begin
  if self = nil then
    FillCharFast(result, SizeOf(result), 0)
  else
    result.FromString(pointer(AccessRights));
end;

class procedure TAuthGroup.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
var
  UC: TAuthUserClass;
  G: TAuthGroup;
  U: TAuthUser;
  A: TOrmAccessRights;
  AuthUserIndex, AuthGroupIndex: integer;
  AdminID, SupervisorID, UserID: PtrInt;
begin
  inherited; // will create any needed index
  if (Server <> nil) and
     (FieldName = '') and
     Server.HandleAuthentication then
  begin
    // create default Groups and Users (we are already in a Transaction)
    AuthGroupIndex := Server.Model.GetTableIndex(self);
    AuthUserIndex := Server.Model.GetTableIndexInheritsFrom(TAuthUser);
    if (AuthGroupIndex < 0) or
       (AuthUserIndex < 0) then
      EModelException.RaiseUtf8('%.InitializeTable: Model has missing % ' +
        'or TAuthUser', [self, self]);
    UC := pointer(Server.Model.Tables[AuthUserIndex]);
    if not (itoNoAutoCreateGroups in Options) then
    begin
      G := Create;
      try
        //            POSTSQL SELECTSQL Service AuthR AuthW TablesR TablesW
        // Admin        Yes     Yes       Yes    Yes   Yes    Yes    Yes
        // Supervisor   No      Yes       Yes    Yes   No     Yes    Yes
        // User         No      No        Yes    No    No     Yes    Yes
        // Guest        No      No        No     No    No     Yes    No
        A := FULL_ACCESS_RIGHTS;
        G.Ident := 'Admin';
        G.OrmAccessRights := A;
        G.SessionTimeout := AuthAdminGroupDefaultTimeout;
        AdminID := Server.Add(G, true);
        G.Ident := 'Supervisor';
        A.AllowRemoteExecute := SUPERVISOR_ACCESS_RIGHTS.AllowRemoteExecute;
        A.Edit(AuthUserIndex, [ooSelect]); // AuthUser  R/O
        A.Edit(AuthGroupIndex, [ooSelect]); // AuthGroup R/O
        G.OrmAccessRights := A;
        G.SessionTimeout := AuthSupervisorGroupDefaultTimeout;
        SupervisorID := Server.Add(G, true);
        G.Ident := 'User';
        Exclude(A.AllowRemoteExecute, reSqlSelectWithoutTable);
        Exclude(A.GET, AuthUserIndex); // no Auth R
        Exclude(A.GET, AuthGroupIndex);
        G.OrmAccessRights := A;
        G.SessionTimeout := AuthUserGroupDefaultTimeout;
        UserID := Server.Add(G, true);
        G.Ident := 'Guest';
        A.AllowRemoteExecute := [];
        FillcharFast(A.POST, SizeOf(TOrmTableBits), 0); // R/O access
        FillcharFast(A.PUT, SizeOf(TOrmTableBits), 0);
        FillcharFast(A.DELETE, SizeOf(TOrmTableBits), 0);
        G.OrmAccessRights := A;
        G.SessionTimeout := AuthGuestGroupDefaultTimeout;
        Server.Add(G, true);
      finally
        G.Free;
      end;
      if not (itoNoAutoCreateUsers in Options) and
         not Server.TableHasRows(UC) then
      begin
        U := UC.Create;
        try
          U.LogonName := 'Admin';
          U.PasswordHashHexa := AuthAdminDefaultPassword;
          U.DisplayName := U.LogonName;
          U.GroupRights := TAuthGroup(AdminID);
          Server.Add(U, true);
          U.LogonName := 'Supervisor';
          U.PasswordHashHexa := AuthSupervisorDefaultPassword;
          U.DisplayName := U.LogonName;
          U.GroupRights := TAuthGroup(SupervisorID);
          Server.Add(U, true);
          U.LogonName := 'User';
          U.PasswordHashHexa := AuthUserDefaultPassword;
          U.DisplayName := U.LogonName;
          U.GroupRights := TAuthGroup(UserID);
          Server.Add(U, true);
        finally
          U.Free;
        end;
      end;
    end;
  end;
end;

procedure TAuthGroup.SetOrmAccessRights(const Value: TOrmAccessRights);
begin
  if self <> nil then
    AccessRights := Value.ToString;
end;


{ TAuthUser }

class function TAuthUser.ComputeHashedPassword(const aLogonName, aPasswordPlain,
  aHashSalt: RawUtf8; aHashRound: integer): RawUtf8;
var
  dig: THash512Rec;
  bytes: PtrInt;
  algo: TDigestAlgo absolute aHashRound;
begin
  if (aPasswordPlain <> '') and
     (aPasswordPlain[1] = '$') and
     (ModularCryptIdentify(aPasswordPlain) in mcfValid) then
  begin
    // already in the expected new and safe "Modular Crypt" format
    result := aPasswordPlain;
    exit;
  end;
  if (aHashSalt = '') or
     (aHashRound = 0) then
  begin
    // mORMot 1 legacy format
    result := Sha256U(['salt', aPasswordPlain]);
    exit;
  end;
  if aHashRound > 0 then
  begin
    // mORMot 1 PBKDF2-HMAC-SHA256 pattern
    Pbkdf2HmacSha256(aPasswordPlain, aHashSalt, aHashRound, dig.Lo);
    bytes := SizeOf(dig.Lo);
  end
  else
  begin
    // store DIGEST-HA0 = Hash(user:realm:password) with aHashSalt = realm
    // - could be used e.g. if you need DIGEST auth from web clients
    aHashRound := -aHashRound; // aHashRound < 0 = - ord(TDigestAlgo)
    if aHashRound > ord(high(TDigestAlgo)) then
      algo := daSHA256;
    bytes := DigestHA0(algo, aLogonName, aHashSalt, aPasswordPlain, dig);
  end;
  BinToHexLower(@dig, bytes, result);
  FillCharFast(dig, SizeOf(dig), 0);
end;

procedure TAuthUser.SetPasswordPlain(const Value: RawUtf8);
begin
  if self <> nil then
    fPasswordHashHexa := ComputeHashedPassword(fLogonName, Value);
end;

procedure TAuthUser.SetPassword(const aPasswordPlain, aHashSalt: RawUtf8;
  aHashRound: integer);
begin
  if self <> nil then
    fPasswordHashHexa := ComputeHashedPassword(
      fLogonName, aPasswordPlain, aHashSalt, aHashRound);
end;

procedure TAuthUser.SetPassword(const aPasswordPlain: RawUtf8;
  aModularCrypt: TModularCryptFormat);
begin
  if (self <> nil) and
     (aModularCrypt in mcfValid) then
    fPasswordHashHexa := ModularCryptHash(aModularCrypt, aPasswordPlain);
end;

procedure TAuthUser.SetPasswordDigest(const aPasswordPlain, aRealm: RawUtf8;
  aAlgo: TDigestAlgo);
begin
  if self <> nil then
    fPasswordHashHexa := ComputeHashedPassword(
      fLogonName, aPasswordPlain, aRealm, -ord(aAlgo));
end;

function TAuthUser.CanUserLog(Ctxt: TObject): boolean;
begin
  result := true; // any existing TAuthUser is allowed by default
end;


{ ************ TRestUriParams REST URI Definitions }

{ TRestUriParams }

procedure TRestUriParams.Init;
begin
  OutStatus := 0;
  OutInternalState := 0;
  RestAccessRights := nil;
  LowLevelConnectionID := 0;
  byte(LowLevelConnectionFlags) := 0;
  LowLevelConnectionOpaque := nil;
end;

procedure TRestUriParams.Init(const aUri, aMethod, aInHead, aInBody: RawUtf8);
begin
  Init;
  Url := aUri;
  Method := aMethod;
  InHead := aInHead;
  InBody := aInBody;
end;

procedure TRestUriParams.InBodyType(var ContentType: RawUtf8;
  GuessJsonIfNoneSet: boolean);
begin
  FindNameValue(InHead, HEADER_CONTENT_TYPE_UPPER, ContentType);
  if GuessJsonIfNoneSet and
     (ContentType = '') then
    ContentType := JSON_CONTENT_TYPE_VAR;
end;

function TRestUriParams.OutBodyType(GuessJsonIfNoneSet: boolean): RawUtf8;
begin
  FindNameValue(OutHead, HEADER_CONTENT_TYPE_UPPER, result);
  if GuessJsonIfNoneSet and
     (result = '') then
    result := JSON_CONTENT_TYPE_VAR;
end;

function TRestUriParams.OutBodyTypeIsJson(GuessJsonIfNoneSet: boolean): boolean;
var
  ct: PUtf8Char;
  len: PtrInt;
begin
  ct := FindNameValuePointer(pointer(OutHead), HEADER_CONTENT_TYPE_UPPER, len);
  if ct = nil then
    result := GuessJsonIfNoneSet
  else
    result := IsContentTypeJson(ct, len);
end;

function TRestUriParams.Header(UpperName: PAnsiChar): RawUtf8;
begin
  FindNameValue(InHead, UpperName, result);
end;

procedure TRestUriParams.HeaderOnce(var Store, Dest: RawUtf8; UpperName: PAnsiChar);
begin
  if (Store = '') and
     (@self <> nil) then
  begin
    FindNameValue(InHead, UpperName, Dest);
    if Dest = '' then
      Store := NULL_STR_VAR // flag to ensure header is parsed only once
    else
      Store := Dest;
  end
  else if pointer(Store) = pointer(NULL_STR_VAR) then
    Dest := ''
  else
    Dest := Store;
end;

procedure TRestUriParams.GetRemoteIP(var Dest: RawUtf8);
begin
  HeaderOnce(LowLevelRemoteIP, Dest, HEADER_REMOTEIP_UPPER);
end;

function TRestUriParams.RemoteIPNotLocal: PUtf8Char;
begin
  result := pointer(LowLevelRemoteIP); // usually already set
  if (result <> nil) and
     IsLocalHost(result) then // '127.x.x.x' or '::1'
    result := nil;
end;

procedure TRestUriParams.GetUserAgent(var Dest: RawUtf8);
begin
  HeaderOnce(LowLevelUserAgent, Dest, 'USER-AGENT: ');
end;

procedure TRestUriParams.GetAuthenticationBearerToken(var Dest: RawUtf8);
begin
  HeaderOnce(LowLevelBearerToken, Dest, HEADER_BEARER_UPPER);
end;


{ ************ TRestUriContext REST Parent Process on Server Side }

{ TRestUriContext }

function TRestUriContext.GetUserAgent: RawUtf8;
begin
  fCall^.GetUserAgent(result);
end;

function TRestUriContext.ClientKind: TRestClientKind;
var
  agent: RawUtf8;
begin
  if fClientKind = ckUnknown then
    if llfInProcess in fCall^.LowLevelConnectionFlags then
      // e.g. from TRestClientDB.InternalUri
      fClientKind := ckFramework
    else if fCall^.InHead = '' then
      // e.g. for WebSockets remote access
      fClientKind := ckFramework
    else
    begin
      // try to recognize User-Agent header
      agent := GetUserAgent;
      if (agent = '') or
         (PosEx('mORMot', agent) > 0) then
        // 'mORMot' set e.g. from DefaultUserAgent() in mormot.net.http
        fClientKind := ckFramework
      else
        fClientKind := ckAjax;
    end;
  result := fClientKind;
end;

procedure TRestUriContext.SetRemoteIP(var IP: RawUtf8);
begin
  fCall^.GetRemoteIP(IP);
end;

function TRestUriContext.RemoteIPNotLocal: PUtf8Char;
begin
  if self <> nil then
    result := fCall^.RemoteIPNotLocal
  else
    result := nil;
end;

function TRestUriContext.AuthenticationBearerToken: RawUtf8;
begin
  fCall^.GetAuthenticationBearerToken(result);
end;

function TRestUriContext.AuthenticationCheck(jwt: TJwtAbstract): boolean;
begin
  if fJwtContent = nil then
    fJwtContent := AllocMem(SizeOf(fJwtContent^));
  if jwt = nil then
    fJwtContent^.result := jwtNoToken
  else
    jwt.Verify(AuthenticationBearerToken, fJwtContent^);
  result := JwtContent^.result = jwtValid;
  if not result then
    Error('Invalid Bearer [%]', [ToText(JwtContent^.result)^], HTTP_FORBIDDEN);
end;

function TRestUriContext.GetInHeader(const HeaderName: RawUtf8): RawUtf8;
var
  up: TByteToAnsiChar;
begin
  if self = nil then
    result := ''
  else if fInHeaderLastName = HeaderName then
    result := fInHeaderLastValue
  else
  begin
    PWord(UpperCopy255(up{%H-}, HeaderName))^ := ord(':');
    FindNameValue(fCall^.InHead, up, result); // = fCall^.Header(up)
    if result <> '' then
    begin
      fInHeaderLastName := HeaderName;
      fInHeaderLastValue := result;
    end;
  end;
end;

function TRestUriContext.InputCookiesParse: PHttpCookies;
var
  p: PUtf8Char;
begin
  result := nil;
  fInputCookiesParsed := icpNone;
  p := FindNameValue(pointer(fCall^.InHead), 'COOKIE: ');
  if p = nil then
    exit;
  fInputCookies.ParseServer(p - 8);
  if fInputCookies.Cookies = nil then
    exit;
  fInputCookiesParsed := icpAvailable;
  result := @fInputCookies;
end;

function TRestUriContext.InputCookies: PHttpCookies;
begin
  result := nil; // most common case
  if (self <> nil) and
     (fInputCookiesParsed <> icpNone) then
    if fInputCookiesParsed = icpAvailable then
      result := @fInputCookies
    else
      result := InputCookiesParse;
end;

function TRestUriContext.GetInCookie(const CookieName: RawUtf8): RawUtf8;
begin
  InputCookies^.RetrieveCookie(CookieName, result);
end;

function TRestUriContext.InCookieSearch(const CookieName: RawUtf8): PHttpCookie;
begin
  result := InputCookies^.FindCookie(CookieName);
end;

procedure TRestUriContext.SetOutSetCookie(const aOutSetCookie: RawUtf8);
var
  c: RawUtf8;
begin
  if self = nil then
    exit;
  c := TrimU(aOutSetCookie);
  if not IsValidUtf8WithoutControlChars(c) then
    ERestException.RaiseUtf8('Unsafe %.SetOutSetCookie', [self]);
  if PosExChar('=', c) < 2 then
    ERestException.RaiseUtf8(
      '"name=value" expected for %.SetOutSetCookie("%")', [self, c]);
  fOutSetCookie := c;
end;

procedure TRestUriContext.SetOutCookie(const aName, aValue: RawUtf8);
var
  n: RawUtf8;
begin
  n := StringReplaceChars(aName, '/', '_'); // TOrmModel.Root did allow '/'
  if not PropNameValid(pointer(n)) then
     ERestException.RaiseUtf8('%.SetOutCookie(%): invalid name', [self, aName]);
  SetOutSetCookie(Join([n, '=', aValue]));
end;

procedure TRestUriContext.OutHeadFromCookie;
begin
  AppendLine(fCall.OutHead, ['Set-Cookie: ', fOutSetCookie]);
end;

function TRestUriContext.ContentTypeIsJson: boolean;
var
  p: PUtf8Char;
begin
  p := pointer(fInputContentType);
  if p = nil then
    result := true
  else
    result := IsContentTypeJson(p, PStrLen(p - _STRLEN)^);
end;

function TRestUriContext.InputAsMultiPart(
  var MultiPart: TMultiPartDynArray): boolean;
begin
  result := (Method = mPOST) and
     IdemPChar(pointer(fInputContentType), CONTENT_TYPE_MULTIPARTFORM) and
     MultiPartFormDataDecode(fInputContentType, fCall^.InBody, MultiPart);
end;

function TRestUriContext.TickCount64: Int64;
begin
  if self <> nil then
  begin
    result := fTix64;
    if result <> 0 then
      exit;
  end;
  result := mormot.core.os.GetTickCount64;
  if self <> nil then
    fTix64 := result; // store in cache during the whole request flow
end;

procedure SetCacheControl(var Head: RawUtf8; CacheControlMaxAgeSec: integer);
begin
  AppendLine(Head, ['Cache-Control: max-age=', CacheControlMaxAgeSec]);
end;

procedure Process304NotModified(Call: PRestUriParams; const ServerHash: RawUtf8);
var
  server, client: RawUtf8;
begin
  FindNameValue(Call^.InHead, 'IF-NONE-MATCH: ', client);
  server := ServerHash;
  if server = '' then
    server := crc32cUtf8ToHex(Call^.OutBody);
  server := Join(['"', server, '"']);
  if client <> server then
    AppendLine(Call^.OutHead, ['ETag: ', server])
  else
  begin
    // save bandwidth by returning "304 Not Modified"
    Call^.OutBody := '';
    Call^.OutStatus := HTTP_NOTMODIFIED;
  end;
end;

procedure TRestUriContext.Returns(const Result: RawUtf8;
  Status: integer; const CustomHeader: RawUtf8;
  Handle304NotModified, HandleErrorAsRegularResult: boolean;
  CacheControlMaxAgeSec: integer; const ServerHash: RawUtf8);
begin
  if HandleErrorAsRegularResult or
     StatusCodeIsSuccess(Status) then
  begin
    fCall^.OutBody := Result;
    fCall^.OutStatus := Status;
    if CustomHeader <> '' then
      fCall^.OutHead := CustomHeader
    else if fCall^.OutHead = '' then
      fCall^.OutHead := JSON_CONTENT_TYPE_HEADER_VAR;
    if CacheControlMaxAgeSec > 0 then
      SetCacheControl(fCall^.OutHead, CacheControlMaxAgeSec);
    if Handle304NotModified and
       (Status = HTTP_SUCCESS) and
       (Length(Result) > 64) then
      Process304NotModified(fCall, ServerHash);
  end
  else
    Error(Result, Status);
end;

procedure TRestUriContext.Returns(Value: TObject; Status: integer;
  Handle304NotModified: boolean; OrmOptions: TOrmWriterOptions;
  const CustomHeader: RawUtf8);
var
  json: RawUtf8;
begin
  if Value.InheritsFrom(TOrm) then
    json := TOrm(Value).GetJsonValues(true, true, ooSelect, nil, OrmOptions)
  else
    json := ObjectToJson(Value);
  Returns(json, Status, CustomHeader, Handle304NotModified);
end;

procedure TRestUriContext.ReturnsJson(const Value: variant;
  Status: integer; Handle304NotModified: boolean; Escape: TTextWriterKind;
  MakeHumanReadable: boolean; const CustomHeader: RawUtf8;
  HandleErrorAsRegularResult: boolean);
var
  json: RawUtf8;
begin
  _VariantSaveJson(Value, Escape, json);
  if MakeHumanReadable and
     (json <> '') and
     (json[1] in ['{', '[']) then
    json := JsonReformat(json, jsonHumanReadable);
  Returns(json, Status, CustomHeader,
    Handle304NotModified, HandleErrorAsRegularResult);
end;

procedure TRestUriContext.ReturnBlob(const Blob: RawByteString;
  Status: integer; Handle304NotModified: boolean; const FileName: TFileName;
  CacheControlMaxAgeSec: integer);
begin
  if not ExistsIniName(pointer(fCall^.OutHead), HEADER_CONTENT_TYPE_UPPER) then
    AddToCsv(GetMimeContentTypeHeader(Blob, FileName), fCall^.OutHead, #13#10);
  Returns(Blob, Status, fCall^.OutHead, Handle304NotModified, false, CacheControlMaxAgeSec);
end;

procedure TRestUriContext.ReturnFile(const FileName: TFileName;
  Handle304NotModified: boolean; const ContentType: RawUtf8;
  const AttachmentFileName: RawUtf8; const Error404Redirect: RawUtf8;
  CacheControlMaxAgeSec: integer);
var
  size: Int64;
  time: TUnixMSTime;
begin
  if not FileInfoByName(FileName, size, time) or
     (size < 0) then // -1 if FileName is a folder
    if Error404Redirect <> '' then
      Redirect(Error404Redirect)
    else
      Error('', HTTP_NOTFOUND, CacheControlMaxAgeSec)
  else
  begin
    if CacheControlMaxAgeSec > 0 then
      AppendLine(fCall^.OutHead, ['Cache-Control: max-age=', CacheControlMaxAgeSec]);
    fCall^.OutStatus := HTTP_SUCCESS;
    if Handle304NotModified and
       FileHttp304NotModified(size, time, pointer(fCall^.InHead), fCall^.OutHead) then
    begin
      fCall^.OutStatus := HTTP_NOTMODIFIED;
      exit;
    end;
    // Content-Type: appears twice: 1st to notify static file, 2nd for mime type
    if not ExistsIniName(pointer(fCall^.OutHead), HEADER_CONTENT_TYPE_UPPER) then
      if ContentType <> '' then
        if IdemPChar(pointer(ContentType), HEADER_CONTENT_TYPE_UPPER) then
          AppendLine(fCall^.OutHead, [ContentType]) // already in header: format
        else
          AppendLine(fCall^.OutHead, [HEADER_CONTENT_TYPE, ContentType])
      else
        AppendLine(fCall^.OutHead, [HEADER_CONTENT_TYPE, GetMimeContentType('', FileName)]);
    Prepend(fCall^.OutHead, [STATICFILE_CONTENT_TYPE_HEADER + #13#10]);
    StringToUtf8(FileName, fCall^.OutBody); // body=filename for STATICFILE_CONTENT
    if AttachmentFileName <> '' then
      AppendLine(fCall^.OutHead,
        ['Content-Disposition: attachment; filename="', AttachmentFileName, '"']);
  end;
end;

procedure TRestUriContext.Redirect(const NewLocation: RawUtf8;
  PermanentChange: boolean);
begin
  if PermanentChange then
    fCall^.OutStatus := HTTP_MOVEDPERMANENTLY
  else
    fCall^.OutStatus := HTTP_TEMPORARYREDIRECT;
  Join(['Location: ', NewLocation], fCall^.OutHead);
end;

procedure TRestUriContext.Returns(const NameValuePairs: array of const;
  Status: integer; Handle304NotModified, HandleErrorAsRegularResult: boolean;
  const CustomHeader: RawUtf8);
begin
  Returns(JsonEncode(NameValuePairs), Status, CustomHeader,
    Handle304NotModified, HandleErrorAsRegularResult);
end;

procedure TRestUriContext.Results(const Values: array of const;
  Status: integer; Handle304NotModified: boolean; CacheControlMaxAgeSec: integer);
var
  n: PtrInt;
  json: RawUtf8;
  v: PVarRec;
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  n := length(Values);
  if n = 0 then
    json := '{"result":null}'
  else
    with TJsonWriter.CreateOwnedStream(temp) do
    try
      AddShort('{"result":');
      v := @Values[0];
      if n = 1 then
        // result is one value
        AddJsonEscapeVarRec(v)
      else
      begin
        // result is one array of values
        AddDirect('[');
        repeat
          AddJsonEscapeVarRec(v);
          AddComma;
          inc(v);
          dec(n);
        until n = 0;
        AddDirect(']');
      end;
      AddDirect('}');
      SetText(json);
    finally
      Free;
    end;
  Returns(json, Status, '', Handle304NotModified, false, CacheControlMaxAgeSec);
end;

procedure TRestUriContext.Success(Status: integer);
begin
  if StatusCodeIsSuccess(Status) then
    fCall^.OutStatus := Status
  else
    Error('', Status);
end;

procedure TRestUriContext.Error(const Format: RawUtf8;
  const Args: array of const; Status, CacheControlMaxAgeSec: integer);
var
  msg: RawUtf8;
begin
  FormatUtf8(Format, Args, msg);
  Error(msg, Status, CacheControlMaxAgeSec);
end;

procedure TRestUriContext.Error(E: Exception; const Format: RawUtf8;
  const Args: array of const; Status: integer);
var
  msg, exc: RawUtf8;
begin
  FormatUtf8(Format, Args, msg);
  if E = nil then
    Error(msg, Status)
  else
  begin
    exc := ObjectToJsonDebug(E);
    if msg = '' then
      Error('{"%":%}', [E, exc], Status)
    else
      Error(FormatJson('{"msg":?,"%":%}', [E, exc], [msg]), Status);
  end;
end;

function TRestUriContext.StatusCodeToText(Code: cardinal): PRawUtf8;
begin
  result := mormot.core.text.StatusCodeToText(Code); // standard English
end;

procedure TRestUriContext.Error(const ErrorMessage: RawUtf8;
  Status, CacheControlMaxAgeSec: integer);
var
  msg: PRawUtf8;
  temp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  fCall^.OutStatus := Status;
  if StatusCodeIsSuccess(Status) then
  begin
    // not an error
    fCall^.OutBody := ErrorMessage;
    if CacheControlMaxAgeSec <> 0 then
      // Cache-Control is ignored for errors
      FormatUtf8('Cache-Control: max-age=%', [CacheControlMaxAgeSec], fCall^.OutHead);
    exit;
  end;
  if ErrorMessage = '' then
    msg := StatusCodeToText(Status) // customizable method (also in fServer)
  else
    msg := @ErrorMessage;
  with TJsonWriter.CreateOwnedStream(temp) do
  try
    AddShort('{'#13#10'"errorCode":');
    Add(Status);
    if (msg^ <> '') and
       (msg^[1] = '{') and
       (msg^[length(msg^)] = '}') then
    begin
      // detect and append the error message as JSON object
      AddShort(','#13#10'"error":'#13#10);
      AddString(msg^);
      AddDirect(#13, #10, '}');
    end
    else
    begin
      // regular error message as JSON text
      AddShort(','#13#10'"errorText":"');
      AddJsonEscape(pointer(msg^));
      AddDirect('"', #13, #10, '}');
    end;
    SetText(fCall^.OutBody);
  finally
    Free;
  end;
end;



{ ************ TRestThread Background Process of a REST instance }

{ TRestThread }

constructor TRestThread.Create(aRest: TRest; aOwnRest, aCreateSuspended: boolean);
begin
  fSafe.Init;
  if aRest = nil then
    EOrmException.RaiseUtf8('%.Create(aRest=nil)', [self]);
  fRest := aRest;
  fOwnRest := aOwnRest;
  if fThreadName = '' then
    // if thread name has not been set by the overriden constructor
    Make([self, ' ', fRest.Model.Root], fThreadName);
  fEvent := TSynEvent.Create;
  inherited Create(aCreateSuspended);
end;

procedure TRestThread.WaitForNotExecuting(maxMS: integer);
begin
  SleepHiRes(maxMS, fExecuting, {termvalue=}false);
end;

destructor TRestThread.Destroy;
begin
  if fExecuting then
  begin
    Terminate; // will notify Execute that the process is finished
    WaitForNotExecuting;
  end;
  inherited Destroy;
  if fOwnRest and
     (fRest <> nil) then
  begin
    if GetCurrentThreadId = ThreadID then
    begin
      fRest.fLogFamily := nil; // no log after fRest.EndCurrentThread(self)
      fRest.fLogClass := nil;
    end;
    FreeAndNilSafe(fRest);
  end;
  fSafe.Done;
  fEvent.Free;
end;

function TRestThread.SleepOrTerminated(MS: integer): boolean;
var
  endtix: Int64;
begin
  result := true; // notify Terminated
  if (self = nil) or
     Terminated then
    exit;
  endtix := mormot.core.os.GetTickCount64 + MS;
  repeat
    fEvent.WaitFor(MS); // warning: can wait up to 15 ms more on Windows
    if Terminated then
      exit;
  until (MS < 32) or
        (mormot.core.os.GetTickCount64 >= endtix);
  result := false; // normal delay expiration
end;

procedure TRestThread.Execute;
begin
  fLog := fRest.fLogClass.Add; // fLog: TSynLog instance (maybe thread-specific)
  SetCurrentThreadName('%', [fThreadName]);
  fRest.fRun.BeginCurrentThread(self);
  try
    fExecuting := true;
    try
      InternalExecute;
    except
      on E: Exception do
        fLog.Log(sllError, 'Unhandled % in %.Execute -> abort',
          [E, ClassType], self);
    end;
  finally
    fRest.fRun.EndCurrentThread(self);
    fLog := nil; // no log after EndCurrentThread
    fExecuting := false;
  end;
end;

procedure TRestThread.TerminatedSet;
begin
  fEvent.SetEvent;
end;



{ ************ TRestRunThreads Multi-Threading Process of a REST instance }

{ TRestRunThreads }

constructor TRestRunThreads.Create(aOwner: TRest);
begin
  inherited Create; // initialize fSafe
  fOwner := aOwner;
end;

procedure TRestRunThreads.Shutdown;
begin
  if self <> nil then
    fShutdown := true;
end;

destructor TRestRunThreads.Destroy;
begin
  inherited Destroy;
  fShutdown := true;
  FreeAndNilSafe(fBackgroundTimer);
end;

function TRestRunThreads.EnsureBackgroundTimerExists: TRestBackgroundTimer;
begin
  if (self = nil) or
     fShutdown then
  begin
    result := nil; // paranoid check to avoid any GPF
    exit;
  end;
  fSafe.Lock;
  try
    if fBackgroundTimer = nil then
    begin
      fBackgroundTimer := TRestBackgroundTimer.Create(fOwner);
      fOwner.OnRestBackgroundTimerCreate;
    end;
    result := fBackgroundTimer;
  finally
    fSafe.UnLock;
  end;
end;

function TRestRunThreads.NewBackgroundThreadMethod(const Format: RawUtf8;
   const Args: array of const): TSynBackgroundThreadMethod;
begin
  if (self = nil) or
     fShutdown then
    result := nil
  else
    result := TSynBackgroundThreadMethod.Create(
      nil, FormatUtf8(Format, Args), BeginCurrentThread, EndCurrentThread);
end;

function TRestRunThreads.NewParallelProcess(ThreadCount: integer;
  const Format: RawUtf8; const Args: array of const): TSynParallelProcess;
begin
  if (self = nil) or
     fShutdown then
    result := nil
  else
    result := TSynParallelProcess.Create(
      ThreadCount, FormatUtf8(Format, Args), BeginCurrentThread, EndCurrentThread);
end;

function TRestRunThreads.NewBackgroundThreadProcess(
  const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  const Format: RawUtf8; const Args: array of const;
  aStats: TSynMonitorClass): TSynBackgroundThreadProcess;
var
  name: RawUtf8;
begin
  FormatUtf8(Format, Args, name);
  if (self = nil) or
     fShutdown then
    result := TSynBackgroundThreadProcess.Create(
      name, aOnProcess, aOnProcessMS, nil, nil, aStats)
  else
    result := TSynBackgroundThreadProcess.Create(
      name, aOnProcess, aOnProcessMS, BeginCurrentThread, EndCurrentThread, aStats);
end;

function TRestRunThreads.TimerEnable(
  const aOnProcess: TOnSynBackgroundTimerProcess;
  aOnProcessSecs: cardinal): TRestBackgroundTimer;
begin
  result := nil;
  if (self = nil) or
     fShutdown then
    exit;
  if aOnProcessSecs = 0 then
  begin
    TimerDisable(aOnProcess);
    exit;
  end;
  result := EnsureBackgroundTimerExists;
  result.Enable(aOnProcess, aOnProcessSecs);
end;

function TRestRunThreads.TimerDisable(
  const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  if (self = nil) or
     fShutdown or
     (fBackgroundTimer = nil) then
    result := false
  else
    result := fBackgroundTimer.Disable(aOnProcess);
end;

function TRestRunThreads.Once(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
begin
  result := EnsureBackgroundTimerExists.ExecuteOnce(aOnProcess);
end;

function TRestRunThreads.SystemUseTrack(periodSec: integer): TSystemUse;
begin
  result := nil;
  if (self = nil) or
     fShutdown then
    exit;
  result := TSystemUse.Current;
  if (result.Timer = nil) or
     ((BackgroundTimer <> nil) and
      (result.Timer = BackgroundTimer)) then
  begin
    if periodSec > 0 then
      result.Timer := EnsureBackgroundTimerExists;
    // TimerEnable() will disable if periodSec=0
    TimerEnable(BackgroundTimer.SystemUseBackgroundExecute, periodSec);
  end;
end;

procedure TRestRunThreads.BeginCurrentThread(Sender: TThread);
begin
  if self <> nil then
    fOwner.OnBeginCurrentThread(sender);
end;

procedure TRestRunThreads.EndCurrentThread(Sender: TThread);
begin
  if self <> nil then
    fOwner.OnEndCurrentThread(sender);
end;

procedure TRestRunThreads.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
begin
  if (self <> nil) and
     not fShutdown then
    EnsureBackgroundTimerExists.AsyncRedirect(
      aGuid, aDestinationInterface, aCallbackInterface, aOnResult);
end;

procedure TRestRunThreads.AsyncRedirect(const aGuid: TGuid;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsyncRedirectResult);
begin
  if (self <> nil) and
     not fShutdown then
    EnsureBackgroundTimerExists.AsyncRedirect(
      aGuid, aDestinationInstance, aCallbackInterface, aOnResult);
end;

procedure TRestRunThreads.AsyncInterning(Interning: TRawUtf8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if (self <> nil) and
     not fShutdown then
    EnsureBackgroundTimerExists.AsyncInterning(
      Interning, InterningMaxRefCount, PeriodMinutes);
end;

function TRestRunThreads.MultiRedirect(const aGuid: TGuid; out aCallbackInterface;
  aCallBackUnRegisterNeeded: boolean): IMultiCallbackRedirect;
var
  factory: TInterfaceFactory;
begin
  if (self = nil) or
     fShutdown then
    result := nil
  else
  begin
    factory := TInterfaceFactory.Get(aGuid);
    if factory = nil then
      EServiceException.RaiseUtf8('%.MultiRedirect: unknown %',
        [self, GuidToShort(aGuid)]);
     result := TInterfacedObjectMulti.Create(fOwner, factory,
       aCallBackUnRegisterNeeded, aCallbackInterface).fList;
  end;
end;



{ ************ TOrmHistory Modifications Tracked Persistence }

{ TOrmModification }

function TOrmModification.ModifiedID: TID;
begin
  if self = nil then
    result := 0
  else
    result := RecordRef(fModifiedRecord).ID;
end;

function TOrmModification.ModifiedTable(Model: TOrmModel): TOrmClass;
begin
  if (self = nil) or
     (Model = nil) then
    result := nil
  else
    result := RecordRef(fModifiedRecord).Table(Model);
end;

function TOrmModification.ModifiedTableIndex: integer;
begin
  if self = nil then
    result := 0
  else
    result := RecordRef(fModifiedRecord).TableIndex;
end;


{ TOrmHistory }

class procedure TOrmHistory.InitializeTable(const Server: IRestOrmServer;
  const FieldName: RawUtf8; Options: TOrmInitializeTableOptions);
begin
  inherited InitializeTable(Server, FieldName, Options);
  if FieldName = '' then
    Server.CreateSqlMultiIndex(self, ['ModifiedRecord', 'Event'], false);
end;

destructor TOrmHistory.Destroy;
begin
  inherited;
  fHistoryAdd.Free;
end;

constructor TOrmHistory.CreateHistory(const aClient: IRestOrm;
  aTable: TOrmClass; aID: TID);
var
  ref: RecordRef;
  rec: TOrm;
  hist: TOrmHistory;
begin
  if (aClient = nil) or
     (aID <= 0) then
    EOrmException.RaiseUtf8('Invalid %.CreateHistory(%,%,%) call',
      [self, aClient, aTable, aID]);
  // read BLOB changes
  ref.From(aClient.Model, aTable, aID);
  fModifiedRecord := ref.Value;
  fEvent := heArchiveBlob;
  Create(aClient, 'ModifiedRecord=? and Event=%',
    [ord(heArchiveBlob)], [fModifiedRecord]);
  if fID <> 0 then
    aClient.RetrieveBlobFields(self); // load former fHistory field
  if not HistoryOpen(aClient.Model) then
    EOrmException.RaiseUtf8('HistoryOpen in %.CreateHistory(%,%,%)',
      [self, aClient, aTable, aID]);
  // append JSON changes
  hist := RecordClass.CreateAndFillPrepare(aClient,
    'ModifiedRecord=? and Event<>%', [ord(heArchiveBlob)], [fModifiedRecord])
    as TOrmHistory;
  try
    if hist.FillTable.RowCount = 0 then
      // no JSON to append
      exit;
    rec := HistoryGetLast;
    try
      while hist.FillOne do
      begin
        rec.FillFrom(pointer(hist.SentDataJson));
        HistoryAdd(rec, hist);
      end;
      HistorySave(nil); // update intern fHistory field
    finally
      rec.Free;
    end;
  finally
    hist.Free;
  end;
  // prepare for HistoryCount and HistoryGet() from internal fHistory field
  HistoryOpen(aClient.Model);
end;

class procedure TOrmHistory.InitializeFields(const Fields: array of const;
  var Json: RawUtf8);
begin
  // you may use a TDocVariant to add some custom fields in your own class
  Json := JsonEncode(Fields);
end;

function TOrmHistory.HistoryOpen(Model: TOrmModel): boolean;
var
  len: cardinal;
  start, i: PtrInt;
  R: TFastReader;
  tmp: RawByteString;
begin
  result := false;
  fHistoryModel := Model;
  fHistoryUncompressed := '';
  fHistoryTable := ModifiedTable(Model);
  fHistoryUncompressedCount := 0;
  fHistoryUncompressedOffset := nil;
  if fHistoryTable = nil then
    exit; // invalid Model or ModifiedRecord
  tmp := AlgoSynLZ.Decompress(fHistory);
  len := length(tmp);
  if len > 4 then
  begin
    R.Init(pointer(tmp), len);
    if not fHistoryTable.OrmProps.CheckBinaryHeader(R) then
      // invalid content: TOrm layout may have changed
      exit;
    fHistoryUncompressedCount := R.ReadVarUInt32Array(fHistoryUncompressedOffset);
    start := R.P - PAnsiChar(pointer(tmp));
    for i := 0 to fHistoryUncompressedCount - 1 do
      inc(fHistoryUncompressedOffset[i], start);
    fHistoryUncompressed := tmp;
  end;
  result := true;
end;

function TOrmHistory.HistoryCount: integer;
begin
  if (self = nil) or
     (fHistoryUncompressed = '') then
    result := 0
  else
    result := fHistoryUncompressedCount;
end;

function TOrmHistory.HistoryGet(Index: integer;
  out Event: TOrmHistoryEvent; out Timestamp: TModTime;
  Rec: TOrm): boolean;
var
  read: TFastReader;
begin
  if cardinal(Index) >= cardinal(HistoryCount) then
  begin
    result := false;
    exit;
  end;
  read.Init(fHistoryUncompressed);
  read.Next(fHistoryUncompressedOffset[Index]);
  Event := TOrmHistoryEvent(read.NextByte);
  Timestamp := read.VarUInt64;
  if (Rec <> nil) and
     (Rec.RecordClass = fHistoryTable) then
  begin
    if Event = heDelete then
      Rec.ClearProperties
    else
      Rec.SetBinaryValuesSimpleFields(read);
    Rec.IDValue := ModifiedID;
  end;
  result := true;
end;

function TOrmHistory.HistoryGet(Index: integer; Rec: TOrm): boolean;
var
  Event: TOrmHistoryEvent;
  Timestamp: TModTime;
begin
  result := HistoryGet(Index, Event, Timestamp, Rec);
end;

function TOrmHistory.HistoryGet(Index: integer): TOrm;
var
  Event: TOrmHistoryEvent;
  Timestamp: TModTime;
begin
  if fHistoryTable = nil then
    result := nil
  else
  begin
    result := fHistoryTable.Create;
    if not HistoryGet(Index, Event, Timestamp, result) then
      FreeAndNilSafe(result);
  end;
end;

function TOrmHistory.HistoryGetLast(Rec: TOrm): boolean;
begin
  result := HistoryGet(fHistoryUncompressedCount - 1, Rec);
end;

function TOrmHistory.HistoryGetLast: TOrm;
var
  event: TOrmHistoryEvent;
  modtime: TModTime;
begin
  if fHistoryTable = nil then
    result := nil
  else
  begin
    result := fHistoryTable.Create; // always return an instance
    HistoryGet(fHistoryUncompressedCount - 1, event, modtime, result);
  end;
end;

procedure TOrmHistory.HistoryAdd(Rec: TOrm; Hist: TOrmHistory);
begin
  if (self = nil) or
     (fHistoryModel = nil) or
     (Rec.RecordClass <> fHistoryTable) then
    exit;
  if fHistoryAdd = nil then
    fHistoryAdd := TBufferWriter.Create(TRawByteStringStream);
  AddInteger(fHistoryAddOffset, fHistoryAddCount, fHistoryAdd.TotalWritten);
  fHistoryAdd.Write1(Ord(Hist.Event));
  fHistoryAdd.WriteVarUInt64(Hist.Timestamp);
  if Hist.Event <> heDelete then
    Rec.GetBinaryValuesSimpleFields(fHistoryAdd);
end;

function TOrmHistory.HistorySave(const Server: IRestOrmServer;
  LastRec: TOrm): boolean;
var
  size, i, maxSize: PtrInt;
  firstOldIndex, firstOldOffset, firstNewIndex, firstNewOffset: integer;
  newOffset: TIntegerDynArray;
  rec: TOrm;
  hist: TOrmHistory;
  W: TBufferWriter;
begin
  result := false;
  if (self = nil) or
     (fHistoryTable = nil) or
     (fModifiedRecord = 0) then
    exit; // wrong call
  try
    // ensure latest item matches "official" one, as read from DB
    if (Server <> nil) and
       (LastRec <> nil) and
       (LastRec.IDValue = ModifiedID) then
    begin
      rec := Server.Retrieve(ModifiedRecord);
      if rec <> nil then
      try // may be just deleted
        if not rec.SameRecord(LastRec) then
        begin
          hist := RecordClass.Create as TOrmHistory;
          try
            hist.fEvent := heUpdate;
            hist.fTimestamp := Server.GetServerTimestamp;
            HistoryAdd(rec, hist);
          finally
            hist.Free;
          end;
        end;
      finally
        rec.Free;
      end;
    end;
    if fHistoryAdd = nil then
      exit; // nothing new
    // ensure resulting size matches specified criteria
    firstOldIndex := 0;
    if Server = nil then
      maxSize := maxInt
    else
      maxSize := Server.MaxUncompressedBlobSize(RecordClass);
    size := fHistoryAdd.TotalWritten;
    if (size > maxSize) or
       (fHistoryUncompressedCount = 0) then
      // e.g. if fHistory.Add() is already bigger than expected
      firstOldIndex := fHistoryUncompressedCount
    else
    begin
      inc(size, Length(fHistoryUncompressed) - fHistoryUncompressedOffset[0]);
      while (firstOldIndex < fHistoryUncompressedCount - 1) and
            (size > maxSize) do
      begin
        dec(size, fHistoryUncompressedOffset[firstOldIndex + 1] -
          fHistoryUncompressedOffset[firstOldIndex]);
        inc(firstOldIndex);
      end;
    end;
    // creates and store new History BLOB
    W := TBufferWriter.Create(TRawByteStringStream);
    try
      // compute offsets
      if firstOldIndex = fHistoryUncompressedCount then
        firstOldOffset := length(fHistoryUncompressed)
      else
        firstOldOffset := fHistoryUncompressedOffset[firstOldIndex];
      SetLength(newOffset,
        fHistoryUncompressedCount - firstOldIndex + fHistoryAddCount);
      for i := firstOldIndex to fHistoryUncompressedCount - 1 do
        newOffset[i - firstOldIndex] :=
          fHistoryUncompressedOffset[i] - firstOldOffset;
      firstNewIndex := fHistoryUncompressedCount - firstOldIndex;
      firstNewOffset := Length(fHistoryUncompressed) - firstOldOffset;
      for i := 0 to fHistoryAddCount - 1 do
        newOffset[firstNewIndex + i] := fHistoryAddOffset[i] + firstNewOffset;
      // write header
      fHistoryTable.OrmProps.SaveBinaryHeader(W);
      W.WriteVarUInt32Array(newOffset, length(newOffset), wkOffsetU);
      // write data
      W.Write(@PByteArray(fHistoryUncompressed)[firstOldOffset], firstNewOffset);
      W.WriteBinary(fHistoryAdd.FlushTo);
      fHistoryUncompressed := W.FlushTo;
      fHistory := AlgoSynLZ.Compress(fHistoryUncompressed);
      if (Server <> nil) and
         (fID <> 0) then
      begin
        Server.UpdateField(RecordClass,
          'Timestamp', Int64ToUtf8(Server.GetServerTimestamp),
          ROWID_TXT,   Int64ToUtf8(fID));
        Server.UpdateBlob(RecordClass, fID, Orm.BlobFields[0].Name, fHistory);
      end;
      result := true;
    finally
      W.Free;
    end;
  finally
    fHistoryUncompressed := '';
    fHistoryUncompressedOffset := nil;
    FreeAndNilSafe(fHistoryAdd);
    fHistoryAddOffset := nil;
    fHistoryAddCount := 0;
  end;
end;

procedure InitializeUnit;
begin
  DefaultTAuthGroupClass := TAuthGroup;
end;

initialization
  InitializeUnit;

end.

