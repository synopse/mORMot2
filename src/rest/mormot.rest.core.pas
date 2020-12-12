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
    - TRestURIParams REST URI Definitions
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
  {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics are buggy
  Generics.Collections,
  {$endif ISDELPHI2010}
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
  mormot.core.crypto,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.core, // for TOrm and IRestOrm
  mormot.soa.core,
  mormot.db.core;


{ ************ Customize REST Execution }

type
  /// all commands which may be executed by TRestServer.URI() method
  // - execSOAByMethod for method-based services
  // - execSOAByInterface for interface-based services
  // - execOrmGet for ORM reads i.e. Retrieve*() methods
  // - execOrmWrite for ORM writes i.e. Add Update Delete TransactionBegin
  // Commit Rollback methods
  TRestServerURIContextCommand = (
    execNone,
    execSOAByMethod,
    execSOAByInterface,
    execOrmGet,
    execOrmWrite);

  /// how a TRest class may execute read or write operations
  // - used e.g. for TRestServer.AcquireWriteMode or
  // TRestServer.AcquireExecutionMode/AcquireExecutionLockedTimeOut
  TRestServerAcquireMode = (
    amUnlocked,
    amLocked,
    amBackgroundThread,
    amBackgroundORMSharedThread,
    amMainThread);

  /// used to store the execution parameters for a TRest instance
  TRestAcquireExecution = class(TSynPersistentLock)
  public
    /// how read or write operations will be executed
    Mode: TRestServerAcquireMode;
    /// delay before failing to acquire the lock
    LockedTimeOut: cardinal;
    /// background thread instance (if any)
    Thread: TSynBackgroundThreadMethod;
    /// finalize the memory structure, and the associated background thread
    destructor Destroy; override;
  end;

  /// define how a TRest class may execute its ORM and SOA operations
  TRestAcquireExecutions =
    array[TRestServerURIContextCommand] of TRestAcquireExecution;


const
  /// size in bytes, to log up to 2 KB of JSON response, to save space
  MAX_SIZE_RESPONSE_LOG = 2 shl 10;

  /// you can use this cookie value to delete a cookie on the browser side
  COOKIE_EXPIRED = '; Expires=Sat, 01 Jan 2010 00:00:01 GMT';


{ ************ TRestBackgroundTimer for Multi-Thread Process }

type
  {$M+}
  { we expect RTTI information for the published properties of these
    forward definitions - due to internal coupling, those classes are
    to be defined in a single "type" statement }
  TRest = class;
  {$M-}

  /// optionally called after TRest.AsynchRedirect background execution
  // - to retrieve any output result value, as JSON-encoded content
  // - as used in TRestBackgroundTimer.AsynchBackgroundExecute protected method
  TOnAsynchRedirectResult = procedure(const aMethod: TInterfaceMethod;
    const aInstance: IInvokable; const aParams, aResult: RawUTF8) of object;

  /// TThread able to run one or several tasks at a periodic pace, or do
  // asynchronous interface or batch execution, with proper TRest integration
  // - used e.g. by TRest.TimerEnable/AsynchRedirect/AsynchBatchStart methods
  // - TRest.BackgroundTimer will define one instance, but you may create
  // other dedicated instances to instantiate separated threads
  TRestBackgroundTimer = class(TSynBackgroundTimer)
  protected
    fRest: TRest;
    fBackgroundBatch: TRestBatchLockedDynArray;
    fBackgroundInterning: array of TRawUTF8Interning;
    fBackgroundInterningMaxRefCount: integer;
    procedure SystemUseBackgroundExecute(Sender: TSynBackgroundTimer;
      Event: TWaitResult; const Msg: RawUTF8);
    // used by AsynchRedirect/AsynchBatch/AsynchInterning
    function AsynchBatchIndex(aTable: TOrmClass): PtrInt;
    function AsynchBatchLocked(aTable: TOrmClass;
      out aBatch: TRestBatchLocked): boolean;
    procedure AsynchBatchUnLock(aBatch: TRestBatchLocked);
    procedure AsynchBatchExecute(Sender: TSynBackgroundTimer;
      Event: TWaitResult; const Msg: RawUTF8);
    procedure AsynchBackgroundExecute(Sender: TSynBackgroundTimer;
      Event: TWaitResult; const Msg: RawUTF8);
    procedure AsynchBackgroundInterning(Sender: TSynBackgroundTimer;
      Event: TWaitResult; const Msg: RawUTF8);
  public
    /// initialize the thread for a periodic task processing
    constructor Create(aRest: TRest; const aThreadName: RawUTF8 = '';
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
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
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
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
    /// prepare an asynchronous ORM BATCH process, executed in a background thread
    // - will initialize a TRestBatch and call TimerEnable to initialize the
    // background thread, following the given processing period (in seconds),
    // or the TRestBatch.Count threshold to call BatchSend
    // - actual REST/CRUD commands will take place via AsynchBatchAdd,
    // AsynchBatchUpdate and AsynchBatchDelete methods
    // - only a single AsynchBatch() call per Table is allowed at a time, unless
    // AsynchBatchStop method is used to flush the current asynchronous BATCH
    // - using a BATCH in a dedicated thread will allow very fast background
    // asynchronous process of ORM methods, sufficient for most use cases
    function AsynchBatchStart(Table: TOrmClass;
      SendSeconds: integer; PendingRowThreshold: integer = 500;
      AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJSON]): boolean;
    /// finalize asynchronous ORM BATCH process, executed in a background thread
    // - should have been preceded by a call to AsynchBatch(), or returns false
    // - Table=nil will release all existing batch instances
    function AsynchBatchStop(Table: TOrmClass): boolean;
    /// create a new ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around TRestBatch.Add() sent in the Timer thread,
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsynchBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsynchBatchAdd() with an already pre-computed
    // JSON object
    // - is a wrapper around TRestBatch.RawAdd() sent in the Timer thread,
    // so will return the index in the BATCH rows, not the created TID
    // - this method is thread-safe
    function AsynchBatchRawAdd(Table: TOrmClass; const SentData: RawUTF8): integer;
    /// append some JSON content in a BATCH to be writen in a background thread
    // - could be used to emulate AsynchBatchAdd() with an already pre-computed
    // JSON object, as stored in a TTextWriter instance
    // - is a wrapper around TRestBatch.RawAppend.AddNoJSONEscape(SentData)
    // in the Timer thread
    // - this method is thread-safe
    procedure AsynchBatchRawAppend(Table: TOrmClass; SentData: TTextWriter);
    /// update an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Update() sent in the Timer thread
    // - this method is thread-safe
    function AsynchBatchUpdate(Value: TOrm;
      const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    /// delete an ORM member in a BATCH to be written in a background thread
    // - should have been preceded by a call to AsynchBatchStart(), or returns -1
    // - is a wrapper around the TRestBatch.Delete() sent in the Timer thread
    // - this method is thread-safe
    function AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;
    /// allows background garbage collection of specified RawUTF8 interning
    // - will run Interning.Clean(2) every 5 minutes by default
    // - set InterningMaxRefCount=0 to disable process of the Interning instance
    procedure AsynchInterning(Interning: TRawUTF8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    /// direct access to the TRest instance owner
    property Rest: TRest
      read fRest;
    /// direct access to the background thread TRestBatch instances
    property BackgroundBatch: TRestBatchLockedDynArray
      read fBackgroundBatch;
  published
    /// the identifier of the thread, as logged
    property Name: RawUTF8
      read fThreadName;
  end;


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

  TSQLRestServerURIContextCommand = TRestServerURIContextCommand;
  TSQLRestServerAcquireMode = TRestServerAcquireMode;
  TSQLRestAcquireExecution = TRestAcquireExecution;
  TSQLRestBackgroundTimer = TRestBackgroundTimer;

{$endif PUREMORMOT2}


{ ************ TRestRunThreads Multi-Threading Process of a REST instance }

  /// access to the Multi-Threading process of a TRest instance
  TRestRunThreads = class(TSynPersistentLock)
  protected
    fOwner: TRest;
    fBackgroundTimer: TRestBackgroundTimer;
  public
    /// initialize the threading process
    constructor Create(aOwner: TRest); reintroduce;
    /// finalize the threading process
    destructor Destroy; override;
    /// allows to safely execute a processing method in a background thread
    // - returns a TSynBackgroundThreadMethod instance, ready to execute any
    // background task via its RunAndWait() method
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewBackgroundThreadMethod(const Format: RawUTF8;
      const Args: array of const): TSynBackgroundThreadMethod;
    /// allows to safely execute a process at a given pace
    // - returns a TSynBackgroundThreadProcess instance, ready to execute the
    // supplied aOnProcess event in a loop, as aOnProcessMS periodic task
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewBackgroundThreadProcess(
      const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
      const Format: RawUTF8; const Args: array of const;
      aStats: TSynMonitorClass=nil): TSynBackgroundThreadProcess;
    /// allows to safely execute a process in parallel
    // - returns a TSynParallelProcess instance, ready to execute any task
    // in parrallel in a thread-pool given by ThreadCount
    // - will properly call BeginCurrentThread/EndCurrentThread methods
    // - you should supply some runtime information to name the thread, for
    // proper debugging
    function NewParallelProcess(ThreadCount: integer; const Format: RawUTF8;
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
    // - is a wrapper around BackgroundTimer.AsynchRedirect()
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
    /// define asynchronous execution of interface methods in a background thread
    // - this class allows to implements any interface via a fake class, which will
    // redirect all methods calls into calls of another interface, but as a FIFO
    // in a background thread, shared with TimerEnable/TimerDisable process
    // - it is an elegant resolution to the most difficult implementation
    // problem of SOA callbacks, which is to avoid race condition on reentrance,
    // e.g. if a callback is run from a thread, and then the callback code try
    // to execute something in the context of the initial thread, protected
    // by a critical section (mutex)
    // - is a wrapper around BackgroundTimer.AsynchRedirect()
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
    /// allows background garbage collection of specified RawUTF8 interning
    // - will run Interning.Clean(2) every 5 minutes by default
    // - set InterningMaxRefCount=0 to disable process of the Interning instance
    // - note that InterningMaxRefCount and PeriodMinutes parameters (if not 0),
    // are common for all TRawUTF8Interning instances (the latest value wins)
    // - you may e.g. run the following to clean up TDocVariant interned RawUTF8:
    // ! aRest.Run.AsynchInterning(DocVariantType.InternNames);
    // ! aRest.Run.AsynchInterning(DocVariantType.InternValues);
    procedure AsynchInterning(Interning: TRawUTF8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    /// define redirection of interface methods calls in one or several instances
    // - this class allows to implements any interface via a fake class, which
    // will redirect all methods calls to one or several other interfaces
    // - returned aCallbackInterface will redirect all its methods (identified
    // by aGUID) into an internal list handled by IMultiCallbackRedirect.Redirect
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
    function MultiRedirect(const aGUID: TGUID; out aCallbackInterface;
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

  /// a generic REpresentational State Transfer (REST) client/server class
  // - is a TInterfaceResolver so is able to resolve IRestOrm
  TRest = class(TInterfaceResolver)
  protected
    fOrm: IRestOrm;
    fOrmInstance: TInterfacedObject; // is a TRestOrm
    fModel: TOrmModel;
    fServices: TServiceContainer;
    fRun: TRestRunThreads;
    fLogClass: TSynLogClass;
    fLogFamily: TSynLogFamily;
    fAcquireExecution: TRestAcquireExecutions;
    fPrivateGarbageCollector: TSynObjectList;
    fServerTimestamp: record
      Offset: TDateTime;
      CacheTix: cardinal;
      CacheValue: TTimeLogBits;
    end;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    procedure SetLogClass(aClass: TSynLogClass); virtual;
    function GetLogClass: TSynLogClass;
    /// compute the server time stamp offset from the given date/time
    procedure SetServerTimestamp(const Value: TTimeLog);
    /// wrapper methods to access fAcquireExecution[]
    function GetAcquireExecutionMode(
      Cmd: TRestServerURIContextCommand): TRestServerAcquireMode;
    procedure SetAcquireExecutionMode(
      Cmd: TRestServerURIContextCommand; Value: TRestServerAcquireMode);
    function GetAcquireExecutionLockedTimeOut(
      Cmd: TRestServerURIContextCommand): cardinal;
    procedure SetAcquireExecutionLockedTimeOut(
      Cmd: TRestServerURIContextCommand; Value: cardinal);
    /// any overriden TRest class should call it in the initialization section
    class procedure RegisterClassNameForDefinition;
    /// ensure the thread will be taken into account during process
    procedure OnBeginCurrentThread(Sender: TThread); virtual;
    procedure OnEndCurrentThread(Sender: TThread); virtual;
  public
    /// initialize the class, and associate it to a specified database Model
    constructor Create(aModel: TOrmModel); virtual;
    // inherited classes should unserialize the other aDefinition properties by
    // overriding this method, in a reverse logic to overriden DefinitionTo()
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); virtual;
    /// release internal used instances
    // - e.g. release associated TOrmModel and TServiceContainer
    destructor Destroy; override;
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TInterfacedObject); virtual;
    /// save the TRest properties into a persistent storage object
    // - you can then use TRest.CreateFrom() to re-instantiate it
    // - current Definition.Key value will be used for the password encryption
    // - this default implementation will set the class name in Definition.Kind:
    // inherited classes should override this method and serialize other
    // properties, then override RegisteredClassCreateFrom() protected method
    // to initiate the very same instance
    procedure DefinitionTo(Definition: TSynConnectionDefinition); virtual;
    /// save the properties into a JSON file
    // - you can then use TRest.CreateFromJSON() to re-instantiate it
    // - you can specify a custom Key, if the default is not enough for you
    function DefinitionToJSON(Key: cardinal = 0): RawUTF8;
    /// save the properties into a JSON file
    // - you can then use TRest.CreateFromFile() to re-instantiate it
    // - you can specify a custom Key, if the default is not enough for you
    procedure DefinitionToFile(const aJSONFile: TFileName; aKey: cardinal = 0);
    /// create a new TRest instance from its Model and stored values
    // - aDefinition.Kind will define the actual class which will be
    // instantiated: currently TRestServerFullMemory, TRestServerDB,
    // TRestClientURINamedPipe, TRestClientURIMessage,
    // TRestHttpClientWinSock, TRestHttpClientWinINet, TRestHttpClientWinHTTP,
    // and TRestHttpClientCurl classes are recognized by this method
    // - then other aDefinition fields will be used to refine the instance:
    // please refer to each overriden DefinitionTo() method documentation
    // - use TRestMongoDBCreate() and/or TRestExternalDBCreate() instead
    // to create a TRest instance will all tables defined as external when
    // aDefinition.Kind is 'MongoDB' or a TSQLDBConnectionProperties class
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
    class function CreateFromJSON(aModel: TOrmModel;
      const aJSONDefinition: RawUTF8; aServerHandleAuthentication: boolean;
      aKey: cardinal = 0): TRest;
    /// create a new TRest instance from its Model and a JSON file
    // - aDefinition.Kind will define the actual class which will be instantiated
    // - you can specify a custom Key, if the default is not safe enough for you
    class function CreateFromFile(aModel: TOrmModel;
      const aJSONFile: TFileName; aServerHandleAuthentication: boolean;
      aKey: cardinal = 0): TRest;
    /// retrieve the registered class from the aDefinition.Kind string
    class function ClassFrom(aDefinition: TSynConnectionDefinition): TRestClass;

    /// ease logging of some text in the context of the current TRest
    procedure InternalLog(const Text: RawUTF8; Level: TSynLogInfo); overload;
    /// ease logging of some text in the context of the current TRest
    procedure InternalLog(const Format: RawUTF8; const Args: array of const;
      Level: TSynLogInfo = sllTrace); overload;
    /// ease logging of method enter/leave in the context of the current TRest
    function Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
      aInstance: TObject = nil): ISynLog;
    /// internal method to retrieve the current Session TAuthUser.ID
    function GetCurrentSessionUserID: TID; virtual; abstract;
    /// retrieve the server time stamp
    // - default implementation will use an internal Offset to compute
    // the value from PC time (i.e. NowUTC+Offset as TTimeLog)
    // - inherited classes may override this method, or set the appropriate
    // value in Offset field
    function GetServerTimestamp: TTimeLog; virtual;

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
    // - by default, TRestServer.URI() will lock for Write ORM according to
    // AcquireWriteMode (i.e. AcquireExecutionMode[execOrmWrite]=amLocked) and
    // other operations won't be protected (for better scaling)
    // - you can tune this behavior by setting this property to the expected
    // execution mode, e.g. execute all method-based services in a dedicated
    // thread via
    // ! aServer.AcquireExecutionMode[execSOAByMethod] := amBackgroundThread;
    // - if you use external DB and a custom ConnectionTimeOutMinutes value,
    // both read and write access should be locked, so you should set:
    // ! aServer.AcquireExecutionMode[execOrmGet] := am***;
    // ! aServer.AcquireExecutionMode[execOrmWrite] := am***;
    // here, safe blocking am*** modes are any mode but amUnlocked, i.e. either
    // amLocked, amBackgroundThread, amBackgroundORMSharedThread or amMainThread
    property AcquireExecutionMode[Cmd: TRestServerURIContextCommand]: TRestServerAcquireMode
      read GetAcquireExecutionMode write SetAcquireExecutionMode;
    /// the time (in mili seconds) to try locking internal commands of this class
    // - this value is used only for AcquireExecutionMode[*]=amLocked
    // - by default, TRestServer.URI() will lock for Write ORM according to
    // AcquireWriteTimeOut  (i.e. AcquireExecutionLockedTimeOut[execOrmWrite])
    // and other operations won't be locked nor have any time out set
    property AcquireExecutionLockedTimeOut[Cmd: TRestServerURIContextCommand]: cardinal
      read GetAcquireExecutionLockedTimeOut write SetAcquireExecutionLockedTimeOut;
    /// how this class will handle write access to the database
    // - is a common wrapper to AcquireExecutionMode[execOrmWrite] property
    // - default amLocked mode will wait up to AcquireWriteTimeOut mili seconds
    // to have a single access to the server write ORM methods
    // - amBackgroundThread will execute the write methods in a queue, in a
    // dedicated unique thread (which can be convenient, especially for
    // external database transaction process)
    // - amBackgroundORMSharedThread will execute all ORM methods in a queue, in
    // a dedicated unique thread, shared for both execOrmWrite and execOrmGet,
    // but still dedicated for execSOAByMethod and execSOAByInterface
    // - a slower alternative to amBackgroundThread may be amMainThread
    // - you can set amUnlocked for a concurrent write access, but be aware
    // that it may lead into multi-thread race condition issues, depending on
    // the database engine used
    property AcquireWriteMode: TRestServerAcquireMode index execOrmWrite
      read GetAcquireExecutionMode write SetAcquireExecutionMode;
    /// the time (in mili seconds) which the class will wait for acquiring a
    // write acccess to the database, when AcquireWriteMode is amLocked
    // - is a common wrapper to AcquireExecutionLockedTimeOut[execOrmWrite]
    // - in order to handle safe transactions and multi-thread safe writing, the
    // server will identify transactions using the client Session ID: this
    // property will set the time out wait period
    // - default value is 5000, i.e. TRestServer.URI will wait up to 5 seconds
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
    property PrivateGarbageCollector: TSynObjectList
      read fPrivateGarbageCollector;
    /// access to the associate TSynLog class type
    property LogClass: TSynLogClass
      read fLogClass;
    /// access to the associate TSynLog class familly
    property LogFamily: TSynLogFamily
      read fLogFamily;

  {$ifndef PUREMORMOT2}
    // backward compatibility redirections to the homonymous IRestOrm methods
    // see IRestOrm documentation for the proper use information
  public
    function TableRowCount(Table: TOrmClass): Int64;
    function TableHasRows(Table: TOrmClass): boolean;
    function TableMaxID(Table: TOrmClass): TID;
    function MemberExists(Table: TOrmClass; ID: TID): boolean;
    function OneFieldValue(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8): RawUTF8; overload;
    function OneFieldValueInt64(Table: TOrmClass;
      const FieldName, WhereClause: RawUTF8; Default: Int64 = 0): Int64;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): RawUTF8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const): RawUTF8; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const;
      out Data: Int64): boolean; overload;
    function OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
      WhereID: TID): RawUTF8; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      const WhereClause: RawUTF8): boolean; overload;
    function MultiFieldValue(Table: TOrmClass;
      const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
      WhereID: TID): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; out Data: TRawUTF8DynArray): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8; var Data: TInt64DynArray;
      SQL: PRawUTF8 = nil): boolean; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName: RawUTF8;
      const WhereClause: RawUTF8 = ''; const Separator: RawUTF8 = ','): RawUTF8; overload;
    function OneFieldValues(Table: TOrmClass; const FieldName, WhereClause:
      RawUTF8; Strings: TStrings; IDToIndex: PID = nil): boolean; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClause: RawUTF8 = ''): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const BoundsSQLWhere: array of const): TOrmTable; overload;
    function MultiFieldValues(Table: TOrmClass; const FieldNames: RawUTF8;
      const WhereClauseFormat: RawUTF8; const Args, Bounds: array of const): TOrmTable; overload;
    function FTSMatch(Table: TOrmFts3Class; const WhereClause: RawUTF8;
      var DocID: TIDDynArray): boolean; overload;
    function FTSMatch(Table: TOrmFts3Class; const MatchClause: RawUTF8;
      var DocID: TIDDynArray; const PerFieldWeight: array of double;
      limit: integer = 0; offset: integer = 0): boolean; overload;
    function MainFieldValue(Table: TOrmClass; ID: TID;
      ReturnFirstIfNoUnique: boolean = false): RawUTF8;
    function MainFieldID(Table: TOrmClass; const Value: RawUTF8): TID;
    function MainFieldIDs(Table: TOrmClass; const Values: array of RawUTF8;
      out IDs: TIDDynArray): boolean;
    function Retrieve(const SQLWhere: RawUTF8; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    function Retrieve(const WhereClauseFmt: RawUTF8;
      const Args, Bounds: array of const; Value: TOrm;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean; overload;
    function Retrieve(aID: TID; Value: TOrm;
      ForUpdate: boolean = false): boolean; overload;
    function Retrieve(Reference: TRecordReference;
      ForUpdate: boolean = false): TOrm; overload;
    function Retrieve(aPublishedRecord, aValue: TOrm): boolean; overload;
    function RetrieveList(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList; overload;
    {$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(
    function RetrieveList<T: TOrm>(
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList<T>; overload;
       {$ifdef HASINLINE}inline;{$endif}
    function RetrieveList<T: TOrm>(const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): TObjectList<T>; overload;
    {$endif ISDELPHI2010}
    function RetrieveListJSON(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''; aForceAJAX: boolean = false): RawJSON; overload;
    function RetrieveListJSON(Table: TOrmClass;
      const SQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8 = '';
      aForceAJAX: boolean = false): RawJSON; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName, CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveDocVariantArray(Table: TOrmClass;
      const ObjectName: RawUTF8; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const; const CustomFieldsCSV: RawUTF8;
      FirstRecordID: PID = nil; LastRecordID: PID = nil): variant; overload;
    function RetrieveOneFieldDocVariantArray(Table: TOrmClass;
      const FieldName, FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const): variant;
    function RetrieveDocVariant(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const CustomFieldsCSV: RawUTF8): variant;
    function RetrieveListObjArray(var ObjArray; Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const aCustomFieldsCSV: RawUTF8 = ''): boolean;
    procedure AppendListAsJsonArray(Table: TOrmClass;
      const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
      const OutputFieldName: RawUTF8; W: TJSONSerializer;
      const CustomFieldsCSV: RawUTF8 = '');
    function RTreeMatch(DataTable: TOrmClass;
      const DataTableBlobFieldName: RawUTF8; RTreeTable: TOrmRTreeClass;
      const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
    function ExecuteList(const Tables: array of TOrmClass;
      const SQL: RawUTF8): TOrmTable;
    function ExecuteJson(const Tables: array of TOrmClass;
      const SQL: RawUTF8; ForceAJAX: boolean = false;
      ReturnedRowCount: PPtrInt = nil): RawJSON;
    function Execute(const aSQL: RawUTF8): boolean;
    function ExecuteFmt(const SQLFormat: RawUTF8;
      const Args: array of const): boolean; overload;
    function ExecuteFmt(const SQLFormat: RawUTF8;
      const Args, Bounds: array of const): boolean; overload;
    function UnLock(Table: TOrmClass; aID: TID): boolean; overload;
    function UnLock(Rec: TOrm): boolean; overload;
    function Add(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomCSVFields: RawUTF8;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function Add(Value: TOrm; const CustomFields: TFieldBits;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID; overload;
    function AddWithBlobs(Value: TOrm;
      ForceID: boolean = false; DoNotAutoComputeFields: boolean = false): TID;
    function AddSimple(aTable: TOrmClass;
      const aSimpleFields: array of const; ForcedID: TID = 0): TID;
    function Update(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(Value: TOrm; const CustomCSVFields: RawUTF8;
      DoNotAutoComputeFields: boolean = false): boolean; overload;
    function Update(aTable: TOrmClass; aID: TID;
      const aSimpleFields: array of const): boolean; overload;
    function AddOrUpdate(Value: TOrm; ForceID: boolean = false): TID;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; const WhereFieldName: RawUTF8;
      const WhereFieldValue: array of const; const FieldName: RawUTF8;
      const FieldValue: array of const): boolean; overload;
    function UpdateField(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass;
      const WhereFieldName: RawUTF8; const WhereFieldValue: variant;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateField(Table: TOrmClass; const IDs: array of Int64;
      const FieldName: RawUTF8; const FieldValue: variant): boolean; overload;
    function UpdateFieldIncrement(Table: TOrmClass; ID: TID;
      const FieldName: RawUTF8; Increment: Int64 = 1): boolean;
    function RecordCanBeUpdated(Table: TOrmClass; ID: TID;
      Action: TOrmEvent; ErrorMsg: PRawUTF8 = nil): boolean;
    function Delete(Table: TOrmClass; ID: TID): boolean; overload;
    function Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean; overload;
    function Delete(Table: TOrmClass; const FormatSQLWhere: RawUTF8;
      const BoundsSQLWhere: array of const): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUTF8;
      out BlobData: RawBlob): boolean; overload;
    function RetrieveBlob(Table: TOrmClass; aID: TID; const BlobFieldName: RawUTF8;
      out BlobStream: TCustomMemoryStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; const BlobData: RawBlob): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; BlobData: TStream): boolean; overload;
    function UpdateBlob(Table: TOrmClass; aID: TID;
      const BlobFieldName: RawUTF8; BlobData: pointer; BlobSize: integer): boolean; overload;
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
    function AsynchBatchStart(Table: TOrmClass; SendSeconds: integer;
      PendingRowThreshold: integer = 500; AutomaticTransactionPerRow: integer = 1000;
      Options: TRestBatchOptions = [boExtendedJSON]): boolean;
    function AsynchBatchStop(Table: TOrmClass): boolean;
    function AsynchBatchAdd(Value: TOrm; SendData: boolean;
      ForceID: boolean = false; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsynchBatchRawAdd(Table: TOrmClass; const SentData: RawUTF8): integer;
    procedure AsynchBatchRawAppend(Table: TOrmClass; SentData: TTextWriter);
    function AsynchBatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;
    function Cache: TRestCache;
    function CacheOrNil: TRestCache;
    function CacheWorthItForTable(aTableIndex: cardinal): boolean;
  public
    // TRestRunThreads compatibility methods
    function NewBackgroundThreadMethod(const Format: RawUTF8;
      const Args: array of const): TSynBackgroundThreadMethod;
    function NewBackgroundThreadProcess(const aOnProcess: TOnSynBackgroundThreadProcess;
      aOnProcessMS: cardinal; const Format: RawUTF8; const Args: array of const;
      aStats: TSynMonitorClass=nil): TSynBackgroundThreadProcess;
    function NewParallelProcess(ThreadCount: integer; const Format: RawUTF8;
      const Args: array of const): TSynParallelProcess;
    function TimerEnable(const aOnProcess: TOnSynBackgroundTimerProcess;
      aOnProcessSecs: cardinal): TRestBackgroundTimer;
    function TimerDisable(const aOnProcess: TOnSynBackgroundTimerProcess): boolean;
    function SystemUseTrack(periodSec: integer = 10): TSystemUse;
    function EnsureBackgroundTimerExists: TRestBackgroundTimer;
    procedure BeginCurrentThread(Sender: TThread); virtual;
    procedure EndCurrentThread(Sender: TThread); virtual;
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInterface: IInvokable; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
    procedure AsynchRedirect(const aGUID: TGUID;
      const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
      const aOnResult: TOnAsynchRedirectResult = nil); overload;
    procedure AsynchInterning(Interning: TRawUTF8Interning;
      InterningMaxRefCount: integer = 2; PeriodMinutes: integer = 5);
    function MultiRedirect(const aGUID: TGUID; out aCallbackInterface;
      aCallBackUnRegisterNeeded: boolean = true): IMultiCallbackRedirect; overload;
    function BackgroundTimer: TRestBackgroundTimer;
      {$ifdef HASINLINE}inline;{$endif}
  {$endif PUREMORMOT2}
  end;

{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLRest = TRest;
  TSQLRestClass = TRestClass;
  TSQLRestDynArray = TRestDynArray;

{$endif PUREMORMOT2}

function ToText(cmd: TRestServerURIContextCommand): PShortString; overload;

const
  /// custom contract value to ignore contract validation from client side
  // - you could set the aContractExpected parameter to this value for
  // TRestClientURI.ServiceDefine or TRestClientURI.ServiceRegister
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
  AuthAdminDefaultPassword: RawUTF8 = DEFAULT_HASH_SYNOPSE;
  /// default hashed password set by TAuthGroup.InitializeTable for 'Supervisor' user
  // - you can override this value to follow your own application expectations
  AuthSupervisorDefaultPassword: RawUTF8 = DEFAULT_HASH_SYNOPSE;
  /// default hashed password set by TAuthGroup.InitializeTable for 'User' user
  // - you can override this value to follow your own application expectations
  AuthUserDefaultPassword: RawUTF8 = DEFAULT_HASH_SYNOPSE;


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
    fIdent: RawUTF8;
    fSessionTimeOut: integer;
    fAccessRights: RawUTF8;
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
    // to the reSQLSelectWithoutTable flag)
    // - 'User' won't have the reSQLSelectWithoutTable flag, nor the right
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
      const FieldName: RawUTF8; Options: TOrmInitializeTableOptions); override;
    /// corresponding TOrmAccessRights for this authentication group
    // - content is converted into/from text format via AccessRight DB property
    // (so it will be not fixed e.g. by the binary TOrmFieldTables layout, i.e.
    // the MAX_TABLES constant value)
    property SQLAccessRights: TOrmAccessRights
      read GetOrmAccessRights write SetOrmAccessRights;
  published
    /// the access right identifier, ready to be displayed
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" (i.e. "stored false") attribute)
    // - so you can retrieve a TAuthGroup ID from its identifier, as such:
    // ! UserGroupID := fClient.MainFieldID(TAuthGroup,'User');
    property Ident: RawUTF8 index 50
      read fIdent write fIdent stored AS_UNIQUE;
    /// the number of minutes a session is kept alive
    property SessionTimeout: integer
      read fSessionTimeOut write fSessionTimeOut;
    /// a textual representation of a TOrmAccessRights buffer
    property AccessRights: RawUTF8 index 1600
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
    fLogonName: RawUTF8;
    fPasswordHashHexa: RawUTF8;
    fDisplayName: RawUTF8;
    fGroupRights: TAuthGroup;
    fData: RawBlob;
    procedure SetPasswordPlain(const Value: RawUTF8);
  public
    /// static function allowing to compute a hashed password
    // - as expected by this class
    // - defined as virtual so that you may use your own hashing class
    // - you may specify your own values in aHashSalt/aHashRound, to enable
    // PBKDF2_HMAC_SHA256() use instead of plain SHA256(): it will increase
    // security on storage side (reducing brute force attack via rainbow tables)
    class function ComputeHashedPassword(const aPasswordPlain: RawUTF8;
      const aHashSalt: RawUTF8 = ''; aHashRound: integer = 20000): RawUTF8; virtual;
    /// able to set the PasswordHashHexa field from a plain password content
    // - in fact, PasswordHashHexa := SHA256('salt'+PasswordPlain) in UTF-8
    // - use SetPassword() method if you want to customize the hash salt value
    // and use the much safer PBKDF2_HMAC_SHA256 algorithm
    property PasswordPlain: RawUTF8 write SetPasswordPlain;
    /// set the PasswordHashHexa field from a plain password content and salt
    // - use this method to specify aHashSalt/aHashRound values, enabling
    // PBKDF2_HMAC_SHA256() use instead of plain SHA256(): it will increase
    // security on storage side (reducing brute force attack via rainbow tables)
    // - you may use an application specific fixed salt, and/or append the
    // user LogonName to make the challenge unique for each TAuthUser
    // - the default aHashRound=20000 is slow but secure - since the hashing
    // process is expected to be done on client side, you may specify your
    // own higher/slower value, depending on the security level you expect
    procedure SetPassword(const aPasswordPlain, aHashSalt: RawUTF8;
      aHashRound: integer = 20000);
    /// check if the user can authenticate in its current state
    // - Ctxt is a TRestServerURIContext instance
    // - called by TRestServerAuthentication.GetUser() method
    // - this default implementation will return TRUE, i.e. allow the user
    // to log on
    // - override this method to disable user authentication, e.g. if the
    // user is disabled via a custom ORM boolean and date/time field
    function CanUserLog(Ctxt: TObject): boolean; virtual;
  published
    /// the User identification Name, as entered at log-in
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" - i.e. "stored false" - attribute), and
    // therefore indexed in the database (e.g. hashed in TRestStorageInMemory)
    property LogonName: RawUTF8
      index 20 read fLogonName write fLogonName stored AS_UNIQUE;
    /// the User Name, as may be displayed or printed
    property DisplayName: RawUTF8
      index 50 read fDisplayName write fDisplayName;
    /// the hexa encoded associated SHA-256 hash of the password
    // - see TAuthUser.ComputeHashedPassword() or SetPassword() methods
    // - store the SHA-256 32 bytes as 64 hexa chars
    property PasswordHashHexa: RawUTF8
      index 64 read fPasswordHashHexa write fPasswordHashHexa;
    /// the associated access rights of this user
    // - access rights are managed by group
    // - in TAuthSession.User instance, GroupRights property will contain a
    // REAL TAuthGroup instance for fast retrieval in TRestServer.URI
    // - note that 'Group' field name is not allowed by SQLite
    property GroupRights: TAuthGroup
      read fGroupRights write fGroupRights;
    /// some custom data, associated to the User
    // - Server application may store here custom data
    // - its content is not used by the framework but 'may' be used by your
    // application
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


{ ************ TRestURIParams REST URI Definitions }

type
  /// flags which may be set by the caller to notify low-level context
  // - llfHttps will indicates that the communication was made over HTTPS
  // - llfSecured is set if the transmission is encrypted or in-process,
  // using e.g. HTTPS/SSL/TLS or our proprietary AES/ECDHE algorithms
  // - llfWebsockets communication was made using WebSockets
  TRestURIParamsLowLevelFlag = (
    llfHttps,
    llfSecured,
    llfWebsockets);

  /// some flags set by the caller to notify low-level context
  TRestURIParamsLowLevelFlags = set of TRestURIParamsLowLevelFlag;

  /// store all parameters for a Client or Server method call
  // - as used by TRestServer.URI or TRestClientURI.InternalURI
  {$ifdef USERECORDWITHMETHODS}
  TRestURIParams = record
  {$else}
  TRestURIParams = object
  {$endif USERECORDWITHMETHODS}
  public
    /// input parameter containing the caller URI
    Url: RawUTF8;
    /// input parameter containing the caller method
    // - handle enhanced REST codes: LOCK/UNLOCK/BEGIN/END/ABORT
    Method: RawUTF8;
    /// input parameter containing the caller message headers
    // - you can use e.g. to retrieve the remote IP:
    // ! Call.Header(HEADER_REMOTEIP_UPPER)
    // ! or FindNameValue(Call.InHead,HEADER_REMOTEIP_UPPER)
    // but consider rather using TRestServerURIContext.RemoteIP
    InHead: RawUTF8;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    InBody: RawUTF8;
    /// output parameter to be set to the response message header
    // - it is the right place to set the returned message body content type,
    // e.g. TEXT_CONTENT_TYPE_HEADER or HTTP_CONTENT_TYPE_HEADER: if not set,
    // the default JSON_CONTENT_TYPE_HEADER will be returned to the client,
    // meaning that the message is JSON
    // - you can use OutBodyType() function to retrieve the stored content-type
    OutHead: RawUTF8;
    /// output parameter to be set to the response message body
    OutBody: RawUTF8;
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
    /// opaque reference to the protocol context which made this request
    // - may point e.g. to a THttpServerResp, a TWebSocketServerResp,
    // a THttpApiServer, a TRestClientURI, a TFastCGIServer or a
    // TRestServerNamedPipeResponse instance
    // - stores SynCrtSock's THttpServerConnectionID, i.e. a Int64 as expected
    // by http.sys, or an incremental rolling sequence of 31-bit integers for
    // THttpServer/TWebSocketServer, or maybe a raw PtrInt(self/THandle)
    LowLevelConnectionID: Int64;
    /// low-level properties of the current protocol context
    LowLevelFlags: TRestURIParamsLowLevelFlags;
    /// initialize the non RawUTF8 values
    procedure Init; overload;
    /// initialize the input values
    procedure Init(const aURI,aMethod,aInHead,aInBody: RawUTF8); overload;
    /// retrieve the "Content-Type" value from InHead
    // - if GuessJSONIfNoneSet is TRUE, returns JSON if none was set in headers
    function InBodyType(GuessJSONIfNoneSet: boolean = True): RawUTF8;
    /// check if the "Content-Type" value from InHead is JSON
    // - if GuessJSONIfNoneSet is TRUE, assume JSON is used
    function InBodyTypeIsJson(GuessJSONIfNoneSet: boolean = True): boolean;
    /// retrieve the "Content-Type" value from OutHead
    // - if GuessJSONIfNoneSet is TRUE, returns JSON if none was set in headers
    function OutBodyType(GuessJSONIfNoneSet: boolean = True): RawUTF8;
    /// check if the "Content-Type" value from OutHead is JSON
    // - if GuessJSONIfNoneSet is TRUE, assume JSON is used
    function OutBodyTypeIsJson(GuessJSONIfNoneSet: boolean = True): boolean;
    /// just a wrapper around FindNameValue(InHead,UpperName)
    // - use e.g. as
    // ! Call.Header(HEADER_REMOTEIP_UPPER) or Call.Header(HEADER_BEARER_UPPER)
    // - consider rather using TRestServerURIContext.InHeader[] or even
    // dedicated TRestServerURIContext.RemoteIP/AuthenticationBearerToken
    function Header(UpperName: PAnsiChar): RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    /// wrap FindNameValue(InHead,UpperName) with a cache store
    function HeaderOnce(var Store: RawUTF8; UpperName: PAnsiChar): RawUTF8;
  end;

  /// used to map set of parameters for a Client or Server method call
  PRestURIParams = ^TRestURIParams;

  /// how a TLibraryRequest function will release its Head and Resp returned values
  TLibraryRequestFree = procedure(Data: pointer); cdecl;

  /// the function signature of the LibraryRequest() function
  // - as exported by TRestServer.ExportServerGlobalLibraryRequest
  // - and as consummed by TRestClientLibraryRequest on client side
  TLibraryRequest = function(
    Url, Method, SendData: PUTF8Char; UrlLen, MethodLen, SendDataLen: cardinal;
    out HeadRespFree: TLibraryRequestFree; var Head: PUTF8Char; var HeadLen: cardinal;
    out Resp: PUTF8Char; out RespLen, State: cardinal): cardinal; cdecl;

{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLAuthUser = TAuthUser;
  TSQLAuthGroup = TAuthGroup;
  TSQLAuthUserClass = TAuthUserClass;
  TSQLAuthGroupClass = TAuthGroupClass;
  TSQLRestURIParamsLowLevelFlag = TRestURIParamsLowLevelFlag;
  TSQLRestURIParamsLowLevelFlags = TRestURIParamsLowLevelFlags;
  TSQLRestURIParams = TRestURIParams;
  PSQLRestURIParams = PRestURIParams;

{$endif PUREMORMOT2}



{ ************ TRestThread Background Process of a REST instance }

type
  {$M+}
  /// a simple TThread for doing some process within the context of a REST instance
  // - also define a Start method for compatibility with older versions of Delphi
  // - inherited classes should override InternalExecute abstract method
  TRestThread = class(TThread)
  protected
    fRest: TRest;
    fOwnRest: boolean;
    fLog: TSynLog;
    fSafe: TSynLocker;
    fEvent: TEvent;
    fExecuting: boolean;
    /// allows customization in overriden Create (before Execute)
    fThreadName: RawUTF8;
    /// will call BeginCurrentThread/EndCurrentThread and catch exceptions
    procedure Execute; override;
    /// you should override this method with the proper process
    procedure InternalExecute; virtual; abstract;
  public
    /// initialize the thread
    // - if aOwnRest is TRUE, the supplied REST instance will be
    // owned by this thread
    constructor Create(aRest: TRest; aOwnRest, aCreateSuspended: boolean);
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
    /// wait for Execute to be ended (i.e. fExecuting=false)
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
      read FRest;
    /// TRUE if the associated REST instance will be owned by this thread
    property OwnRest: boolean
      read fOwnRest;
    /// a critical section is associated to this thread
    // - could be used to protect shared resources within the internal process
    property Safe: TSynLocker
      read fSafe;
    /// read-only access to the TSynLog instance of the associated REST instance
    property Log: TSynLog
      read fLog;
    /// a event associated to this thread
    property Event: TEvent
      read fEvent;
    /// publishes the thread running state
    property Terminated;
    /// publishes the thread executing state (set when Execute leaves)
    property Executing: boolean
      read fExecuting;
  end;
  {$M-}



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
    // instances (thanks to TRestClientURI.ServerTimestampSynchronize), it
    // is not safe to use this field as absolute: you should rather rely on
    // pure monotonic ID/RowID increasing values (see e.g. TOrmVersion)
    property Timestamp: TModTime
      read fTimestamp write fTimestamp;
  end;

  /// common ancestor for tracking changes on TOrm tables
  // - used by TRestServer.TrackChanges() method for simple fields history
  // - TRestServer.InternalUpdateEvent will use this table to store individual
  // row changes as SentDataJSON, then will compress them in History BLOB
  // - note that any layout change of the tracked TOrm table (e.g. adding
  // a new property) will break the internal data format, so will void the table
  TOrmHistory = class(TOrmModification)
  protected
    fEvent: TOrmHistoryEvent;
    fSentData: RawUTF8;
    fHistory: RawBlob;
    // BLOB storage layout is: RTTIheader + offsets + recordsdata
    fHistoryModel: TOrmModel;
    fHistoryTable: TOrmClass;
    fHistoryTableIndex: integer;
    fHistoryUncompressed: RawByteString;
    fHistoryUncompressedCount: integer;
    fHistoryUncompressedOffset: TIntegerDynArray;
    fHistoryAdd: TBufferWriter;
    fHistoryAddCount: integer;
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
      var JSON: RawUTF8); virtual;
    /// called when the associated table is created in the database
    // - create index on History(ModifiedRecord,Event) for process speed-up
    class procedure InitializeTable(const Server: IRestOrmServer;
      const FieldName: RawUTF8; Options: TOrmInitializeTableOptions); override;
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
    // - otherwise, SentDataJSON may contain the latest values as JSON
    property Event: TOrmHistoryEvent
      read fEvent write fEvent;
    /// for heAdd/heUpdate, the data is stored as JSON
    // - note that we defined a default maximum size of 4KB for this column,
    // to avoid using a CLOB here - perhaps it may not be enough for huge
    // records - feedback is welcome...
    property SentDataJSON: RawUTF8
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


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLRestThread = TRestThread;

{$endif PUREMORMOT2}


implementation

uses
  mormot.orm.rest;


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
  TInterfacedObjectMultiList = class(TInterfacedObjectLocked, IMultiCallbackRedirect)
  protected
    fDest: TInterfacedObjectMultiDestDynArray;
    fDestCount: integer;
    fDests: TDynArray;
    fFakeCallback: TInterfacedObjectMulti;
    procedure Redirect(const aCallback: IInvokable;
      const aMethodsNames: array of RawUTF8; aSubscribe: boolean); overload;
    procedure Redirect(const aCallback: TInterfacedObject;
      const aMethodsNames: array of RawUTF8; aSubscribe: boolean); overload;
    procedure CallBackUnRegister;
    function GetInstances(aMethod: integer; var aInstances: TPointerDynArray): integer;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TInterfacedObjectMulti = class(TInterfacedObjectFakeCallback)
  protected
    fRest: TRest;
    fList: TInterfacedObjectMultiList;
    fCallBackUnRegisterNeeded: boolean;
    function FakeInvoke(const aMethod: TInterfaceMethod; const aParams: RawUTF8;
      aResult, aErrorMsg: PRawUTF8; aClientDrivenID: PCardinal;
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
  fDests.InitSpecific(TypeInfo(TInterfacedObjectMultiDestDynArray), fDest,
    ptInterface, @fDestCount);
end;

procedure TInterfacedObjectMultiList.Redirect(const aCallback: IInvokable;
  const aMethodsNames: array of RawUTF8; aSubscribe: boolean);
var
  ndx: integer;
  new: TInterfacedObjectMultiDest;
const
  NAM: array[boolean] of string[11] = ('Unsubscribe', 'Subscribe');
begin
  if (self = nil) or
     (fFakeCallback = nil) then
    exit;
  fFakeCallback.fLogClass.Add.Log(sllDebug, '%.Redirect: % % using %',
    [ClassType, NAM[aSubscribe], fFakeCallback.Factory.InterfaceName,
     ObjectFromInterface(aCallback)], self);
  fFakeCallback.Factory.CheckMethodIndexes(aMethodsNames, true, new.methods);
  new.instance := aCallback;
  fSafe.Lock;
  try
    ndx := fDests.Find(aCallback);
    if aSubscribe then
      if ndx < 0 then
        fDests.Add(new)
      else
        fDest[ndx] := new
    else
      fDests.Delete(ndx);
  finally
    fSafe.UnLock;
  end;
end;

procedure TInterfacedObjectMultiList.Redirect(const aCallback: TInterfacedObject;
  const aMethodsNames: array of RawUTF8; aSubscribe: boolean);
var
  dest: IInvokable;
begin
  if (self = nil) or
     (fFakeCallback = nil) then
    exit;
  if aCallback = nil then
    raise EServiceException.CreateUTF8('%.Redirect(nil)', [self]);
  if not aCallback.GetInterface(fFakeCallback.Factory.InterfaceIID, dest) then
    raise EServiceException.CreateUTF8('%.Redirect [%]: % is not a %',
      [self, fFakeCallback.fName, aCallback, fFakeCallback.Factory.InterfaceName]);
  Redirect(dest, aMethodsNames, aSubscribe);
end;

procedure TInterfacedObjectMultiList.CallBackUnRegister;
begin
  fSafe.Lock;
  try
    fDests.ClearSafe;
  finally
    fSafe.UnLock;
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
  var aInstances: TPointerDynArray): integer;
var
  i: integer;
  dest: ^TInterfacedObjectMultiDest;
begin
  result := 0;
  dec(aMethod, RESERVED_VTABLE_SLOTS);
  if aMethod < 0 then
    exit;
  SetLength(aInstances, fDestCount);
  dest := pointer(fDest);
  for i := 1 to fDestCount do
  begin
    if aMethod in dest^.methods then
    begin
      aInstances[result] := pointer(dest^.instance);
      inc(result);
    end;
    inc(dest);
  end;
  if result <> fDestCount then
    SetLength(aInstances, result);
end;


{ TInterfacedObjectMulti }

procedure TInterfacedObjectMulti.CallBackUnRegister;
begin
  if fCallBackUnRegisterNeeded then
  begin
    fLogClass.Add.Log(sllDebug, '%.Destroy -> Services.CallbackUnRegister(%)',
      [fList.ClassType, fFactory.InterfaceName], self);
    fRest.Services.CallBackUnRegister(IInvokable(pointer(@fVTable)));
  end;
end;

constructor TInterfacedObjectMulti.Create(aRest: TRest;
  aFactory: TInterfaceFactory; aCallBackUnRegisterNeeded: boolean;
  out aCallbackInterface);
begin
  if aRest = nil then
    raise EServiceException.CreateUTF8('%.Create(aRest=nil)', [self]);
  fRest := aRest;
  fLogClass := fRest.fLogClass;
  fName := fRest.Model.Root; // some context about the TRest running it
  fCallBackUnRegisterNeeded := aCallBackUnRegisterNeeded;
  fList := TInterfacedObjectMultiList.Create;
  fList.fFakeCallback := self;
  inherited Create(aFactory, nil, [ifoJsonAsExtended, ifoDontStoreVoidJSON],
    FakeInvoke, nil);
  Get(aCallbackInterface);
end;

destructor TInterfacedObjectMulti.Destroy;
begin
  fList.CallBackUnRegister;
  inherited Destroy;
end;

function TInterfacedObjectMulti.FakeInvoke(const aMethod: TInterfaceMethod;
  const aParams: RawUTF8; aResult, aErrorMsg: PRawUTF8; aClientDrivenID: PCardinal;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  i: Ptrint;
  exec: TInterfaceMethodExecute;
  instances: TPointerDynArray;
begin
  result := inherited FakeInvoke(aMethod, aParams, aResult, aErrorMsg,
    aClientDrivenID, aServiceCustomAnswer);
  if not result or
     (fList.fDestCount = 0) then
    exit;
  fList.fSafe.Lock;
  try
    if fList.GetInstances(aMethod.ExecutionMethodIndex, instances) = 0 then
      exit;
    exec := TInterfaceMethodExecute.Create(@aMethod);
    try
      exec.Options := [optIgnoreException]; // use exec.ExecutedInstancesFailed
      result := exec.ExecuteJson(instances, pointer('[' + aParams + ']'), nil);
      if exec.ExecutedInstancesFailed <> nil then
        for i := high(exec.ExecutedInstancesFailed) downto 0 do
          if exec.ExecutedInstancesFailed[i] <> '' then
          try
            fRest.InternalLog('%.FakeInvoke % failed due to % -> unsubscribe',
              [ClassType, aMethod.InterfaceDotMethodName,
               exec.ExecutedInstancesFailed[i]], sllDebug);
            fList.fDests.FindAndDelete(instances[i]);
          except // ignore any exception when releasing the (unstable?) callback
          end;
    finally
      exec.Free;
    end;
  finally
    fList.fSafe.UnLock;
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
end;

function TRest.GetLogClass: TSynLogClass;
begin
  if self = nil then
    result := TSynLog
  else
    result := fLogClass;
end;

procedure TRest.InternalLog(const Text: RawUTF8; Level: TSynLogInfo);
begin
  if (self <> nil) and
     (fLogFamily <> nil) and
     (Level in fLogFamily.Level) then
    fLogFamily.SynLog.Log(Level, Text, self);
end;

procedure TRest.InternalLog(const Format: RawUTF8; const Args: array of const;
  Level: TSynLogInfo);
begin
  if (self <> nil) and
     (fLogFamily <> nil) and
     (Level in fLogFamily.Level) then
    fLogFamily.SynLog.Log(Level, Format, Args, self);
end;

function TRest.Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
  aInstance: TObject): ISynLog;
begin
  if aInstance = nil then
    aInstance := self;
  if (self <> nil) and
     (fLogFamily <> nil) and
     (sllEnter in fLogFamily.Level) then
    result := fLogClass.Enter(TextFmt, TextArgs, aInstance)
  else
    result := nil;
end;

function TRest.GetServerTimestamp: TTimeLog;
var
  tix: cardinal;
begin
  tix := GetTickCount64 shr 9; // resolution change from 1 ms to 512 ms
  with fServerTimestamp do
    if CacheTix = tix then
      result := CacheValue.Value
    else
    begin
      CacheTix := tix;
      CacheValue.From(NowUTC + offset);
      result := CacheValue.Value;
    end;
end;

procedure TRest.SetServerTimestamp(const Value: TTimeLog);
begin
  fServerTimestamp.Offset := PTimeLogBits(@Value)^.ToDateTime - NowUTC;
  if fServerTimestamp.Offset = 0 then
    fServerTimestamp.Offset := 0.000001; // retrieve server date/time only once
end;

function TRest.GetAcquireExecutionMode(
  Cmd: TRestServerURIContextCommand): TRestServerAcquireMode;
begin
  result := fAcquireExecution[Cmd].Mode;
end;

procedure TRest.SetAcquireExecutionMode(
  Cmd: TRestServerURIContextCommand; Value: TRestServerAcquireMode);
begin
  fAcquireExecution[Cmd].Mode := Value;
end;

function TRest.GetAcquireExecutionLockedTimeOut(
  Cmd: TRestServerURIContextCommand): cardinal;
begin
  result := fAcquireExecution[Cmd].LockedTimeOut;
end;

procedure TRest.SetAcquireExecutionLockedTimeOut(
  Cmd: TRestServerURIContextCommand; Value: cardinal);
begin
  fAcquireExecution[Cmd].LockedTimeOut := Value;
end;

constructor TRest.Create(aModel: TOrmModel);
var
  cmd: TRestServerURIContextCommand;
begin
  fPrivateGarbageCollector := TSynObjectList.Create;
  fModel := aModel;
  for cmd := Low(cmd) to high(cmd) do
    fAcquireExecution[cmd] := TRestAcquireExecution.Create;
  AcquireWriteMode := amLocked;
  AcquireWriteTimeOut := 5000; // default 5 seconds
  SetLogClass(TSynLog);
  fRun := TRestRunThreads.Create(self);
end;

procedure TRest.SetOrmInstance(aORM: TInterfacedObject);
begin
  if fOrmInstance <> nil then
    raise ERestException.CreateUTF8('%.SetOrmInstance twice', [self]);
  if (aORM = nil) or
     not aORM.GetInterface(IRestOrm, fOrm) then
    raise ERestException.CreateUTF8(
      '%.SetOrmInstance(%) is not an IRestOrm', [self, aORM]);
  fOrmInstance := aORM;
  if not fOrmInstance.GetInterface(IRestOrm, fOrm) then
    raise ERestException.CreateUTF8(
      '%.Create with invalid %', [self, fOrmInstance]);
end;

destructor TRest.Destroy;
var
  cmd: TRestServerURIContextCommand;
begin
  InternalLog('TRest.Destroy %',[fModel.SafeRoot],sllInfo); // self->GPF
  if fOrm <> nil then
    fOrm.AsynchBatchStop(nil); // abort any pending TRestBatch
  FreeAndNil(fRun);
  FreeAndNil(fServices);
  if fOrmInstance <> nil then
    if (fOrm = nil) or
       (fOrmInstance.RefCount <> 1) then
      raise ERestException.CreateUTF8('%.Destroy: RefCount=%',
        [self, fOrmInstance.RefCount])
    else
      fOrmInstance := nil; // avoid dubious GPF
  fOrm := nil;
  if (fModel <> nil) and
     (fModel.Owner = self) then
    // make sure we are the Owner (TRestStorage has fModel<>nil e.g.)
    FreeAndNil(fModel);
  for cmd := Low(cmd) to high(cmd) do
    FreeAndNil(fAcquireExecution[cmd]);
  // fPrivateGarbageCollector should be released in last position
  if fPrivateGarbageCollector <> nil then
  begin
    fPrivateGarbageCollector.ClearFromLast;
    FreeAndNil(fPrivateGarbageCollector);
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
  TRestOrm(fOrmInstance).BeginCurrentThread(Sender);
end;

procedure TRest.OnEndCurrentThread(Sender: TThread);
begin
  TRestOrm(fOrmInstance).EndCurrentThread(Sender);
  // most will be done e.g. in TRestRunThreadsServer.EndCurrentThread
  if fLogFamily <> nil then
    fLogFamily.OnThreadEnded(Sender);
end;

procedure TRest.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition <> nil then
    Definition.Kind := ClassName;
end;

function TRest.DefinitionToJSON(Key: cardinal): RawUTF8;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.Create;
  try
    Definition.Key := Key;
    DefinitionTo(Definition);
    result := Definition.SaveToJSON;
  finally
    Definition.Free;
  end;
end;

procedure TRest.DefinitionToFile(const aJSONFile: TFileName; aKey: cardinal);
begin
  FileFromString(JSONReformat(DefinitionToJSON(aKey)), aJSONFile);
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
    raise ERestException.CreateUTF8('%.CreateFrom: unknown % class - please ' +
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

class function TRest.CreateFromJSON(aModel: TOrmModel;
  const aJSONDefinition: RawUTF8; aServerHandleAuthentication: boolean;
  aKey: cardinal): TRest;
var
  Definition: TSynConnectionDefinition;
begin
  Definition := TSynConnectionDefinition.CreateFromJSON(aJSONDefinition, aKey);
  try
    result := CreateFrom(aModel, Definition, aServerHandleAuthentication);
  finally
    Definition.Free;
  end;
end;

class function TRest.CreateFromFile(aModel: TOrmModel;
  const aJSONFile: TFileName; aServerHandleAuthentication: boolean;
  aKey: cardinal): TRest;
begin
  result := CreateFromJSON(aModel, AnyTextFileToRawUTF8(aJSONFile, true),
    aServerHandleAuthentication, aKey);
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
  WhereClause: RawUTF8): RawUTF8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClause);
end;

function TRest.OneFieldValueInt64(Table: TOrmClass; const FieldName,
  WhereClause: RawUTF8; Default: Int64): Int64;
begin
  result := fOrm.OneFieldValueInt64(Table, FieldName, WhereClause, Default);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): RawUTF8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, FormatSQLWhere, BoundsSQLWhere);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const): RawUTF8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClauseFmt);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  const WhereClauseFmt: RawUTF8; const Args, Bounds: array of const;
  out Data: Int64): boolean;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereClauseFmt, Args, Bounds, Data);
end;

function TRest.OneFieldValue(Table: TOrmClass; const FieldName: RawUTF8;
  WhereID: TID): RawUTF8;
begin
  result := fOrm.OneFieldValue(Table, FieldName, WhereID);
end;

function TRest.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
  const WhereClause: RawUTF8): boolean;
begin
  result := fOrm.MultiFieldValue(Table, FieldName, FieldValue, WhereClause);
end;

function TRest.MultiFieldValue(Table: TOrmClass;
  const FieldName: array of RawUTF8; var FieldValue: array of RawUTF8;
  WhereID: TID): boolean;
begin
  result := fOrm.MultiFieldValue(Table, FieldName, FieldValue, WhereID);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUTF8; const WhereClause: RawUTF8; out Data: TRawUTF8DynArray): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Data);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUTF8; const WhereClause: RawUTF8; var Data: TInt64DynArray;
  SQL: PRawUTF8): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Data, SQL);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName: RawUTF8; const WhereClause: RawUTF8; const Separator: RawUTF8): RawUTF8;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Separator);
end;

function TRest.OneFieldValues(Table: TOrmClass;
  const FieldName, WhereClause: RawUTF8; Strings: TStrings; IDToIndex: PID): boolean;
begin
  result := fOrm.OneFieldValues(Table, FieldName, WhereClause, Strings, IDToIndex);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUTF8; const WhereClause: RawUTF8): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClause);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUTF8; const WhereClauseFormat: RawUTF8;
  const BoundsSQLWhere: array of const): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClauseFormat, BoundsSQLWhere);
end;

function TRest.MultiFieldValues(Table: TOrmClass;
  const FieldNames: RawUTF8; const WhereClauseFormat: RawUTF8;
  const Args, Bounds: array of const): TOrmTable;
begin
  result := fOrm.MultiFieldValues(Table, FieldNames, WhereClauseFormat, Args, Bounds);
end;

function TRest.FTSMatch(Table: TOrmFts3Class;
  const WhereClause: RawUTF8; var DocID: TIDDynArray): boolean;
begin
  result := fOrm.FTSMatch(Table, WhereClause, DocID);
end;

function TRest.FTSMatch(Table: TOrmFts3Class;
  const MatchClause: RawUTF8; var DocID: TIDDynArray;
  const PerFieldWeight: array of double; limit, offset: integer): boolean;
begin
  result := fOrm.FTSMatch(Table, MatchClause, DocID, PerFieldWeight, limit, offset);
end;

function TRest.MainFieldValue(Table: TOrmClass; ID: TID;
  ReturnFirstIfNoUnique: boolean): RawUTF8;
begin
  result := fOrm.MainFieldValue(Table, ID, ReturnFirstIfNoUnique);
end;

function TRest.MainFieldID(Table: TOrmClass; const Value: RawUTF8): TID;
begin
  result := fOrm.MainFieldID(Table, Value);
end;

function TRest.MainFieldIDs(Table: TOrmClass;
  const Values: array of RawUTF8; out IDs: TIDDynArray): boolean;
begin
  result := fOrm.MainFieldIDs(Table, Values, IDs);
end;

function TRest.Retrieve(const SQLWhere: RawUTF8; Value: TOrm;
  const aCustomFieldsCSV: RawUTF8): boolean;
begin
  result := fOrm.Retrieve(SQLWhere, Value, aCustomFieldsCSV);
end;

function TRest.Retrieve(const WhereClauseFmt: RawUTF8;
  const Args, Bounds: array of const; Value: TOrm;
  const aCustomFieldsCSV: RawUTF8): boolean;
begin
  result := fOrm.Retrieve(WhereClauseFmt, Args, Bounds, Value, aCustomFieldsCSV);
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
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): TObjectList;
begin
  result := fOrm.RetrieveList(Table, FormatSQLWhere, BoundsSQLWhere, aCustomFieldsCSV);
end;

{$ifdef ISDELPHI2010} // Delphi 2009/2010 generics support is buggy :(

function TRest.RetrieveList<T>(const aCustomFieldsCSV: RawUTF8): TObjectList<T>;
begin
  result := fOrm.Generics.RetrieveList<T>(aCustomFieldsCSV);
end;

function TRest.RetrieveList<T>(const FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const; const aCustomFieldsCSV: RawUTF8): TObjectList<T>;
begin
  result := fOrm.Generics.RetrieveList<T>(FormatSQLWhere, BoundsSQLWhere, aCustomFieldsCSV);
end;

{$endif ISDELPHI2010}

function TRest.RetrieveListJSON(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8; aForceAJAX: boolean): RawJSON;
begin
  result := fOrm.RetrieveListJSON(Table, FormatSQLWhere, BoundsSQLWhere,
    aCustomFieldsCSV, aForceAJAX);
end;

function TRest.RetrieveListJSON(Table: TOrmClass;
  const SQLWhere: RawUTF8; const aCustomFieldsCSV: RawUTF8;
  aForceAJAX: boolean): RawJSON;
begin
  result := fOrm.RetrieveListJSON(Table, SQLWhere, aCustomFieldsCSV, aForceAJAX);
end;

function TRest.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName, CustomFieldsCSV: RawUTF8;
  FirstRecordID: PID; LastRecordID: PID): variant;
begin
  result := fOrm.RetrieveDocVariantArray(Table, ObjectName, CustomFieldsCSV,
    FirstRecordID, LastRecordID);
end;

function TRest.RetrieveDocVariantArray(Table: TOrmClass;
  const ObjectName: RawUTF8; const FormatSQLWhere: RawUTF8; const BoundsSQLWhere:
  array of const; const CustomFieldsCSV: RawUTF8; FirstRecordID: PID;
  LastRecordID: PID): variant;
begin
  result := fOrm.RetrieveDocVariantArray(Table, ObjectName, FormatSQLWhere,
    BoundsSQLWhere, CustomFieldsCSV, FirstRecordID, LastRecordID);
end;

function TRest.RetrieveOneFieldDocVariantArray(Table: TOrmClass;
  const FieldName, FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const): variant;
begin
  result := fOrm.RetrieveOneFieldDocVariantArray(Table, FieldName,
    FormatSQLWhere, BoundsSQLWhere);
end;

function TRest.RetrieveDocVariant(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const CustomFieldsCSV: RawUTF8): variant;
begin
  result := fOrm.RetrieveDocVariant(Table, FormatSQLWhere, BoundsSQLWhere,
    CustomFieldsCSV);
end;

function TRest.RetrieveListObjArray(var ObjArray; Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const aCustomFieldsCSV: RawUTF8): boolean;
begin
  result := fOrm.RetrieveListObjArray(ObjArray, Table, FormatSQLWhere,
    BoundsSQLWhere, aCustomFieldsCSV);
end;

procedure TRest.AppendListAsJsonArray(Table: TOrmClass;
  const FormatSQLWhere: RawUTF8; const BoundsSQLWhere: array of const;
  const OutputFieldName: RawUTF8; W: TJSONSerializer; const CustomFieldsCSV: RawUTF8);
begin
  fOrm.AppendListAsJsonArray(Table, FormatSQLWhere, BoundsSQLWhere,
    OutputFieldName, W, CustomFieldsCSV);
end;

function TRest.RTreeMatch(DataTable: TOrmClass;
  const DataTableBlobFieldName: RawUTF8; RTreeTable: TOrmRTreeClass;
  const DataTableBlobField: RawByteString; var DataID: TIDDynArray): boolean;
begin
  result := fOrm.RTreeMatch(DataTable, DataTableBlobFieldName, RTreeTable,
    DataTableBlobField, DataID);
end;

function TRest.ExecuteList(const Tables: array of TOrmClass;
  const SQL: RawUTF8): TOrmTable;
begin
  result := fOrm.ExecuteList(Tables, SQL);
end;

function TRest.ExecuteJson(const Tables: array of TOrmClass;
  const SQL: RawUTF8; ForceAJAX: boolean; ReturnedRowCount: PPtrInt): RawJSON;
begin
  result := fOrm.ExecuteJson(Tables, SQL, ForceAJAX, ReturnedRowCount);
end;

function TRest.Execute(const aSQL: RawUTF8): boolean;
begin
  result := fOrm.Execute(aSQL);
end;

function TRest.ExecuteFmt(const SQLFormat: RawUTF8;
  const Args: array of const): boolean;
begin
  result := fOrm.ExecuteFmt(SQLFormat, Args);
end;

function TRest.ExecuteFmt(const SQLFormat: RawUTF8;
  const Args, Bounds: array of const): boolean;
begin
  result := fOrm.ExecuteFmt(SQLFormat, Args, Bounds);
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

function TRest.Add(Value: TOrm; const CustomCSVFields: RawUTF8;
  ForceID: boolean; DoNotAutoComputeFields: boolean): TID;
begin
  result := fOrm.Add(Value, CustomCSVFields, ForceID, DoNotAutoComputeFields);
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

function TRest.Update(Value: TOrm; const CustomCSVFields: RawUTF8;
  DoNotAutoComputeFields: boolean): boolean;
begin
  result := fOrm.Update(Value, CustomCSVFields, DoNotAutoComputeFields);
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
  const FieldName: RawUTF8; const FieldValue: array of const): boolean;
begin
  result := fOrm.UpdateField(Table, ID, FieldName, FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUTF8; const WhereFieldValue: array of const;
  const FieldName: RawUTF8; const FieldValue: array of const): boolean;
begin
  result := fOrm.UpdateField(Table, WhereFieldName, WhereFieldValue, FieldName,
    FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass; ID: TID;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateField(Table, ID, FieldName, FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass;
  const WhereFieldName: RawUTF8; const WhereFieldValue: variant;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateField(Table, WhereFieldName, WhereFieldValue, FieldName,
    FieldValue);
end;

function TRest.UpdateField(Table: TOrmClass; const IDs: array of Int64;
  const FieldName: RawUTF8; const FieldValue: variant): boolean;
begin
  result := fOrm.UpdateField(Table, IDs, FieldName, FieldValue);
end;

function TRest.UpdateFieldIncrement(Table: TOrmClass; ID: TID;
  const FieldName: RawUTF8; Increment: Int64): boolean;
begin
  result := fOrm.UpdateFieldIncrement(Table, ID, FieldName, Increment);
end;

function TRest.RecordCanBeUpdated(Table: TOrmClass; ID: TID;
  Action: TOrmEvent; ErrorMsg: PRawUTF8): boolean;
begin
  result := fOrm.RecordCanBeUpdated(Table, ID, Action, ErrorMsg);
end;

function TRest.Delete(Table: TOrmClass; ID: TID): boolean;
begin
  result := fOrm.Delete(Table, ID);
end;

function TRest.Delete(Table: TOrmClass; const SQLWhere: RawUTF8): boolean;
begin
  result := fOrm.Delete(Table, SQLWhere);
end;

function TRest.Delete(Table: TOrmClass; const FormatSQLWhere: RawUTF8;
  const BoundsSQLWhere: array of const): boolean;
begin
  result := fOrm.Delete(Table, FormatSQLWhere, BoundsSQLWhere);
end;

function TRest.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; out BlobData: RawBlob): boolean;
begin
  result := fOrm.RetrieveBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.RetrieveBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; out BlobStream: TCustomMemoryStream): boolean;
begin
  result := fOrm.RetrieveBlob(Table, aID, BlobFieldName, BlobStream);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; const BlobData: RawBlob): boolean;
begin
  result := fOrm.UpdateBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; BlobData: TStream): boolean;
begin
  result := fOrm.UpdateBlob(Table, aID, BlobFieldName, BlobData);
end;

function TRest.UpdateBlob(Table: TOrmClass; aID: TID;
  const BlobFieldName: RawUTF8; BlobData: pointer; BlobSize: integer): boolean;
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

function TRest.AsynchBatchStart(Table: TOrmClass;
  SendSeconds: integer; PendingRowThreshold: integer;
  AutomaticTransactionPerRow: integer; Options: TRestBatchOptions): boolean;
begin
  result := fOrm.AsynchBatchStart(Table, SendSeconds, PendingRowThreshold,
    AutomaticTransactionPerRow, Options);
end;

function TRest.AsynchBatchStop(Table: TOrmClass): boolean;
begin
  result := fOrm.AsynchBatchStop(Table);
end;

function TRest.AsynchBatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits;
  DoNotAutoComputeFields: boolean): integer;
begin
  result := fOrm.AsynchBatchAdd(Value, SendData, ForceID, CustomFields,
    DoNotAutoComputeFields);
end;

function TRest.AsynchBatchRawAdd(Table: TOrmClass;
  const SentData: RawUTF8): integer;
begin
  result := fOrm.AsynchBatchRawAdd(Table, SentData);
end;

procedure TRest.AsynchBatchRawAppend(Table: TOrmClass; SentData: TTextWriter);
begin
  fOrm.AsynchBatchRawAppend(Table, SentData);
end;

function TRest.AsynchBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  result := fOrm.AsynchBatchUpdate(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRest.AsynchBatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  result := fOrm.AsynchBatchDelete(Table, ID);
end;

function TRest.Cache: TRestCache;
begin
  result := fOrm.Cache;
end;

function TRest.CacheOrNil: TRestCache;
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

function TRest.NewBackgroundThreadMethod(const Format: RawUTF8;
   const Args: array of const): TSynBackgroundThreadMethod;
begin
  if self = nil then
    result := nil
  else
    result := fRun.NewBackgroundThreadMethod(Format, Args);
end;

function TRest.NewParallelProcess(ThreadCount: integer; const Format: RawUTF8;
  const Args: array of const): TSynParallelProcess;
begin
  if self = nil then
    result := nil
  else
    result := fRun.NewParallelProcess(ThreadCount, Format, Args);
end;

function TRest.NewBackgroundThreadProcess(
  const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  const Format: RawUTF8; const Args: array of const;
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

procedure TRest.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
begin
  if self <> nil then
    fRun.AsynchRedirect(
      aGUID, aDestinationInterface, aCallbackInterface, aOnResult);
end;

procedure TRest.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
begin
  if self <> nil then
    fRun.AsynchRedirect(
      aGUID, aDestinationInstance, aCallbackInterface, aOnResult);
end;

procedure TRest.AsynchInterning(Interning: TRawUTF8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if self <> nil then
    fRun.AsynchInterning(
      Interning, InterningMaxRefCount, PeriodMinutes);
end;

function TRest.MultiRedirect(const aGUID: TGUID; out aCallbackInterface;
  aCallBackUnRegisterNeeded: boolean): IMultiCallbackRedirect;
begin
  if self = nil then
    result := nil
  else
    result := fRun.MultiRedirect(aGUID, aCallbackInterface,
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


function ToText(cmd: TRestServerURIContextCommand): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestServerURIContextCommand), ord(cmd));
end;


{ TInterfacedObjectAsynch }

type
  TInterfacedObjectAsynch = class(TInterfacedObjectFakeCallback)
  protected
    fTimer: TRestBackgroundTimer;
    fDest: IInvokable;
    fOnResult: TOnAsynchRedirectResult;
    function FakeInvoke(const aMethod: TInterfaceMethod; const aParams: RawUTF8;
      aResult, aErrorMsg: PRawUTF8; aClientDrivenID: PCardinal;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean; override;
  public
    constructor Create(aTimer: TRestBackgroundTimer; aFactory:
      TInterfaceFactory; const aDestinationInterface: IInvokable; out
      aCallbackInterface; const aOnResult: TOnAsynchRedirectResult);
  end;

  TInterfacedObjectAsynchCall = packed record
    Method: PInterfaceMethod;
    Instance: pointer; // weak IInvokable reference
    Params: RawUTF8;
    OnOutputParamsCopy: RawUTF8;
    OnOutput: TOnAsynchRedirectResult;
  end;

constructor TInterfacedObjectAsynch.Create(aTimer: TRestBackgroundTimer;
  aFactory: TInterfaceFactory; const aDestinationInterface: IInvokable;
  out aCallbackInterface; const aOnResult: TOnAsynchRedirectResult);
begin
  fTimer := aTimer;
  fLogClass := fTimer.fRest.fLogClass;
  fName := fTimer.fThreadName;
  fDest := aDestinationInterface;
  fOnResult := aOnResult;
  inherited Create(aFactory, nil, [ifoJsonAsExtended, ifoDontStoreVoidJSON],
    FakeInvoke, nil);
  Get(aCallbackInterface);
end;

function TInterfacedObjectAsynch.FakeInvoke(const aMethod: TInterfaceMethod;
  const aParams: RawUTF8; aResult, aErrorMsg: PRawUTF8; aClientDrivenID: PCardinal;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
var
  msg: RawUTF8;
  call: TInterfacedObjectAsynchCall;
begin
  result := inherited FakeInvoke(aMethod, aParams, aResult, aErrorMsg,
    aClientDrivenID, aServiceCustomAnswer);
  if not result then
    exit;
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
  msg := BinarySave(@call, TypeInfo(TInterfacedObjectAsynchCall), [rkRecord]);
  result := fTimer.EnQueue(fTimer.AsynchBackgroundExecute, msg, true);
end;


{ ************ TRestBackgroundTimer for Multi-Thread Process }

{ TRestBackgroundTimer }

constructor TRestBackgroundTimer.Create(aRest: TRest;
  const aThreadName: RawUTF8; aStats: TSynMonitorClass);
var
  aName: RawUTF8;
begin
  if aRest = nil then
    raise ERestException.CreateUTF8('%.Create(aRest=nil,"%")', [self, aThreadName]);
  fRest := aRest;
  if aThreadName <> '' then
    aName := aThreadName
  else
    FormatUTF8('% %', [fRest.Model.Root, ClassType], aName);
  inherited Create(aName,
    fRest.fRun.BeginCurrentThread, fRest.fRun.EndCurrentThread, aStats);
end;

destructor TRestBackgroundTimer.Destroy;
begin
  AsynchBatchStop(nil);
  inherited Destroy;
end;

procedure TRestBackgroundTimer.SystemUseBackgroundExecute(
  Sender: TSynBackgroundTimer; Event: TWaitResult; const Msg: RawUTF8);
begin
  TSystemUse.Current({createifnone=}false).OnTimerExecute(Sender);
end;

function TRestBackgroundTimer.AsynchBatchIndex(aTable: TOrmClass): PtrInt;
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

function TRestBackgroundTimer.AsynchBatchLocked(aTable: TOrmClass;
  out aBatch: TRestBatchLocked): boolean;
var
  b: PtrInt;
begin
  b := AsynchBatchIndex(aTable);
  if b >= 0 then
  begin
    aBatch := fBackgroundBatch[b];
    aBatch.Safe.Lock;
    result := true;
  end
  else
    result := false;
end;

procedure TRestBackgroundTimer.AsynchBatchUnLock(aBatch: TRestBatchLocked);
begin
  try
    if aBatch.Count >= aBatch.Threshold then
      ExecuteNow(AsynchBatchExecute);
  finally
    aBatch.Safe.UnLock;
  end;
end;

procedure TRestBackgroundTimer.AsynchBatchExecute(Sender: TSynBackgroundTimer;
  Event: TWaitResult; const Msg: RawUTF8);
var
  json, tablename: RawUTF8;
  batch: TRestBatchLocked;
  table: TOrmClass;
  b: PtrInt;
  count, status: integer;
  res: TIDDynArray;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  try
    // send any pending json
    for b := 0 to high(fBackgroundBatch) do
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
            log := fRest.fLogClass.Enter('AsynchBatchExecute % count=%',
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
        status := fRest.ORM.BatchSend(table, json, res, count); // may take a while
        fRest.InternalLog('AsynchBatchExecute % EngineBatchSend=%',
          [table, status]);
      except
        on E: Exception do
          fRest.InternalLog('% during AsynchBatchExecute %',
            [E.ClassType, table], sllWarning);
      end;
    end;
  finally
    if IdemPChar(pointer(Msg), 'FREE@') then
    begin
      // from AsynchBatchStop()
      fRest.InternalLog('AsynchBatchExecute %', [Msg]);
      tablename := copy(Msg, 6, 127);
      if tablename = '' then
        // AsynchBatchStop(nil)
        ObjArrayClear(fBackgroundBatch, true)
      else
      begin
        // AsynchBatchStop(table)
        b := fRest.Model.GetTableIndex(tablename);
        if b < length(fBackgroundBatch) then
          FreeAndNil(fBackgroundBatch[b]);
      end;
    end;
  end;
end;

function TRestBackgroundTimer.AsynchBatchStart(Table: TOrmClass;
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
  fRest.InternalLog('AsynchBatchStart(%,%,%)',
    [Table, SendSeconds, PendingRowThreshold], sllDebug);
  Enable(AsynchBatchExecute, SendSeconds);
  if fBackgroundBatch = nil then
    SetLength(fBackgroundBatch, fRest.Model.TablesMax + 1);
  fBackgroundBatch[b] := TRestBatchLocked.Create(
    fRest.ORM, Table, AutomaticTransactionPerRow, Options);
  fBackgroundBatch[b].Threshold := PendingRowThreshold;
  result := true;
end;

function TRestBackgroundTimer.AsynchBatchStop(Table: TOrmClass): boolean;
var
  b: PtrInt;
  timeout: Int64;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  result := false;
  if (self = nil) or
     (fBackgroundBatch = nil) then
    exit;
  log := fRest.fLogClass.Enter('AsynchBatchStop(%)', [Table], self);
  timeout := mormot.core.os.GetTickCount64 + 5000;
  if Table = nil then
  begin
    // as called from TRest.Destroy
    if not EnQueue(AsynchBatchExecute, 'free@', true) then
      exit;
    repeat
      SleepHiRes(1); // wait for all batchs to be released
    until (fBackgroundBatch = nil) or
          (mormot.core.os.GetTickCount64 > timeout);
    result := Disable(AsynchBatchExecute);
  end
  else
  begin
    // wait for regular TRestBatch process
    b := AsynchBatchIndex(Table);
    if (b < 0) or
       not EnQueue(AsynchBatchExecute, 'free@' + Table.SQLTableName, true) then
      exit;
    repeat
      SleepHiRes(1); // wait for all pending rows to be sent
    until (fBackgroundBatch[b] = nil) or
          (mormot.core.os.GetTickCount64 > timeout);
    if ObjArrayCount(fBackgroundBatch) > 0 then
      result := true
    else
    begin
      result := Disable(AsynchBatchExecute);
      if result then
        ObjArrayClear(fBackgroundBatch, true);
    end;
  end;
end;

function TRestBackgroundTimer.AsynchBatchAdd(Value: TOrm;
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
  fRest.InternalLog('AsynchBatchAdd %', [Value], sllDebug);
  if AsynchBatchLocked(Value.RecordClass, b) then
  try
    result := b.Add(Value, SendData, ForceID, CustomFields, DoNotAutoComputeFields);
  finally
    AsynchBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsynchBatchRawAdd(Table: TOrmClass;
  const SentData: RawUTF8): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Table = nil) then
    exit;
  fRest.InternalLog('AsynchBatchRawAdd % %', [Table, SentData], sllDebug);
  if AsynchBatchLocked(Table, b) then
  try
    result := b.RawAdd(SentData);
  finally
    AsynchBatchUnLock(b);
  end;
end;

procedure TRestBackgroundTimer.AsynchBatchRawAppend(Table: TOrmClass;
  SentData: TTextWriter);
var
  b: TRestBatchLocked;
begin
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Table = nil) or
     (SentData = nil) then
    exit;
  fRest.InternalLog('AsynchBatchRawAppend %', [Table], sllDebug);
  if AsynchBatchLocked(Table, b) then
  try
    b.RawAppend.AddNoJSONEscape(SentData);
  finally
    AsynchBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsynchBatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) or
     (Value = nil) then
    exit;
  fRest.InternalLog('AsynchBatchUpdate %', [Value], sllDebug);
  if AsynchBatchLocked(Value.RecordClass, b) then
  try
    result := b.Update(Value, CustomFields, DoNotAutoComputeFields);
  finally
    AsynchBatchUnLock(b);
  end;
end;

function TRestBackgroundTimer.AsynchBatchDelete(Table: TOrmClass;
  ID: TID): integer;
var
  b: TRestBatchLocked;
begin
  result := -1;
  if (self = nil) or
     (fBackgroundBatch = nil) then
    exit;
  fRest.InternalLog('AsynchBatchDelete % %', [Table, ID], sllDebug);
  if AsynchBatchLocked(Table, b) then
  try
    result := b.Delete(Table, ID);
  finally
    AsynchBatchUnLock(b);
  end;
end;

procedure TRestBackgroundTimer.AsynchBackgroundExecute(
  Sender: TSynBackgroundTimer; Event: TWaitResult; const Msg: RawUTF8);
var
  exec: TInterfaceMethodExecute;
  call: TInterfacedObjectAsynchCall;
  o: PRawUTF8;
  output: RawUTF8;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  if not RecordLoad(call, Msg, TypeInfo(TInterfacedObjectAsynchCall)) then
    exit; // invalid message (e.g. periodic execution)
  log := fRest.fLogClass.Enter('AsynchBackgroundExecute % %',
    [call.Method^.InterfaceDotMethodName, call.Params], self);
  exec := TInterfaceMethodExecute.Create(call.Method);
  try
    if Assigned(call.OnOutput) then
      o := @output
    else
      o := nil;
    if not exec.ExecuteJsonCallback(call.Instance, call.Params, o) then
      fRest.InternalLog('%.AsynchBackgroundExecute %: ExecuteJsonCallback failed',
        [ClassType, call.Method^.InterfaceDotMethodName], sllWarning)
    else if o <> nil then
      call.OnOutput(call.Method^,
        IInvokable(call.Instance), call.OnOutputParamsCopy, output);
  finally
    exec.Free;
  end;
end;

procedure TRestBackgroundTimer.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
var
  factory: TInterfaceFactory;
begin
  factory := TInterfaceFactory.Get(aGUID);
  if factory = nil then
    raise EServiceException.CreateUTF8('%.AsynchRedirect: unknown %',
      [self, GUIDToShort(aGUID)]);
  if aDestinationInterface = nil then
    raise EServiceException.CreateUTF8('%.AsynchRedirect(nil)', [self]);
  fRest.InternalLog('AsynchRedirect % to % using %',
    [factory.InterfaceName, ObjectFromInterface(aDestinationInterface), self]);
  Enable(AsynchBackgroundExecute, 3600);
  TInterfacedObjectAsynch.Create(self, factory, aDestinationInterface,
    aCallbackInterface, aOnResult);
end;

procedure TRestBackgroundTimer.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
var
  dest: IInvokable;
begin
  if aDestinationInstance = nil then
    raise EServiceException.CreateUTF8('%.AsynchRedirect(nil)', [self]);
  if not aDestinationInstance.GetInterface(aGUID, dest) then
    raise EServiceException.CreateUTF8('%.AsynchRedirect [%]: % is not a %',
      [self, fThreadName, aDestinationInstance, GUIDToShort(aGUID)]);
  AsynchRedirect(aGUID, dest, aCallbackInterface, aOnResult);
end;

procedure TRestBackgroundTimer.AsynchBackgroundInterning(
  Sender: TSynBackgroundTimer; Event: TWaitResult; const Msg: RawUTF8);
var
  i: PtrInt;
  claimed, total: integer;
  timer: TPrecisionTimer;
begin
  timer.Start;
  claimed := 0;
  for i := 0 to high(fBackgroundInterning) do
    inc(claimed, fBackgroundInterning[i].Clean(fBackgroundInterningMaxRefCount));
  if claimed = 0 then
    exit; // nothing to collect
  total := claimed;
  for i := 0 to high(fBackgroundInterning) do
    inc(total, fBackgroundInterning[i].Count);
  fRest.InternalLog(
    '%.AsynchInterning: Clean(%) claimed %/% strings from % pools in %',
    [ClassType, fBackgroundInterningMaxRefCount, claimed, total,
     length(fBackgroundInterning), timer.Stop], sllDebug);
end;

procedure TRestBackgroundTimer.AsynchInterning(Interning: TRawUTF8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if (self = nil) or
     (Interning = nil) then
    exit;
  fTaskLock.Lock;
  try
    if (InterningMaxRefCount <= 0) or
       (PeriodMinutes <= 0) then
      ObjArrayDelete(fBackgroundInterning, Interning)
    else
    begin
      fBackgroundInterningMaxRefCount := InterningMaxRefCount;
      ObjArrayAddOnce(fBackgroundInterning, Interning);
      Enable(AsynchBackgroundInterning, PeriodMinutes * 60);
    end;
  finally
    fTaskLock.UnLock;
  end;
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
  const FieldName: RawUTF8; Options: TOrmInitializeTableOptions);
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
      raise EModelException.CreateUTF8(
        '%.InitializeTable: Model has missing % or TAuthUser',
        [self, self]);
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
        G.SQLAccessRights := A;
        G.SessionTimeout := AuthAdminGroupDefaultTimeout;
        AdminID := Server.Add(G, true);
        G.Ident := 'Supervisor';
        A.AllowRemoteExecute := SUPERVISOR_ACCESS_RIGHTS.AllowRemoteExecute;
        A.Edit(AuthUserIndex, [ooSelect]); // AuthUser  R/O
        A.Edit(AuthGroupIndex, [ooSelect]); // AuthGroup R/O
        G.SQLAccessRights := A;
        G.SessionTimeout := AuthSupervisorGroupDefaultTimeout;
        SupervisorID := Server.Add(G, true);
        G.Ident := 'User';
        Exclude(A.AllowRemoteExecute, reSQLSelectWithoutTable);
        Exclude(A.GET, AuthUserIndex); // no Auth R
        Exclude(A.GET, AuthGroupIndex);
        G.SQLAccessRights := A;
        G.SessionTimeout := AuthUserGroupDefaultTimeout;
        UserID := Server.Add(G, true);
        G.Ident := 'Guest';
        A.AllowRemoteExecute := [];
        FillcharFast(A.POST, SizeOf(TOrmFieldTables), 0); // R/O access
        FillcharFast(A.PUT, SizeOf(TOrmFieldTables), 0);
        FillcharFast(A.DELETE, SizeOf(TOrmFieldTables), 0);
        G.SQLAccessRights := A;
        G.SessionTimeout := AuthGuestGroupDefaultTimeout;
        Server.Add(G, true);
      finally
        G.Free;
      end;
      if not (itoNoAutoCreateUsers in Options) and
         (Server.TableRowCount(UC) = 0) then
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

class function TAuthUser.ComputeHashedPassword(const aPasswordPlain,
  aHashSalt: RawUTF8; aHashRound: integer): RawUTF8;
const
  TAuthUser_SALT = 'salt';
var
  dig: TSHA256Digest;
begin
  if aHashSalt = '' then
    result := SHA256(TAuthUser_SALT + aPasswordPlain)
  else
  begin
    PBKDF2_HMAC_SHA256(aPasswordPlain, aHashSalt, aHashRound, dig);
    result := SHA256DigestToString(dig);
    FillCharFast(dig, SizeOf(dig), 0);
  end;
end;

procedure TAuthUser.SetPasswordPlain(const Value: RawUTF8);
begin
  if self <> nil then
    PasswordHashHexa := ComputeHashedPassword(Value);
end;

procedure TAuthUser.SetPassword(const aPasswordPlain, aHashSalt: RawUTF8;
  aHashRound: integer);
begin
  if self <> nil then
    PasswordHashHexa := ComputeHashedPassword(aPasswordPlain, aHashSalt, aHashRound);
end;

function TAuthUser.CanUserLog(Ctxt: TObject): boolean;
begin
  result := true; // any existing TAuthUser is allowed by default
end;


{ ************ TRestURIParams REST URI Definitions }

{ TRestURIParams }

procedure TRestURIParams.Init;
begin
  OutStatus := 0;
  OutInternalState := 0;
  RestAccessRights := nil;
  LowLevelConnectionID := 0;
  byte(LowLevelFlags) := 0;
end;

procedure TRestURIParams.Init(const aURI, aMethod, aInHead, aInBody: RawUTF8);
begin
  Init;
  Url := aURI;
  Method := aMethod;
  InHead := aInHead;
  InBody := aInBody;
end;

function TRestURIParams.InBodyType(GuessJSONIfNoneSet: boolean): RawUTF8;
begin
  FindNameValue(InHead, HEADER_CONTENT_TYPE_UPPER, result);
  if GuessJSONIfNoneSet and
     (result = '') then
    result := JSON_CONTENT_TYPE_VAR;
end;

function TRestURIParams.InBodyTypeIsJson(GuessJSONIfNoneSet: boolean): boolean;
begin
  result := IdemPChar(pointer(InBodyType(GuessJSONIfNoneSet)), JSON_CONTENT_TYPE_UPPER);
end;

function TRestURIParams.OutBodyType(GuessJSONIfNoneSet: boolean): RawUTF8;
begin
  FindNameValue(OutHead, HEADER_CONTENT_TYPE_UPPER, result);
  if GuessJSONIfNoneSet and
     (result = '') then
    result := JSON_CONTENT_TYPE_VAR;
end;

function TRestURIParams.OutBodyTypeIsJson(GuessJSONIfNoneSet: boolean): boolean;
begin
  result := IdemPChar(pointer(OutBodyType(GuessJSONIfNoneSet)), JSON_CONTENT_TYPE_UPPER);
end;

function TRestURIParams.Header(UpperName: PAnsiChar): RawUTF8;
begin
  FindNameValue(InHead, UpperName, result);
end;

function TRestURIParams.HeaderOnce(var Store: RawUTF8; UpperName: PAnsiChar): RawUTF8;
begin
  if (Store = '') and
     (@self <> nil) then
  begin
    FindNameValue(InHead, UpperName, result);
    if result = '' then
      Store := NULL_STR_VAR
    else // ensure header is parsed only once
      Store := result;
  end
  else if pointer(Store) = pointer(NULL_STR_VAR) then
    result := ''
  else
    result := Store;
end;


{ ************ TRestThread Background Process of a REST instance }

{ TRestThread }

constructor TRestThread.Create(aRest: TRest; aOwnRest, aCreateSuspended: boolean);
begin
  if aRest = nil then
    raise EOrmException.CreateUTF8('%.Create(aRest=nil)', [self]);
  fSafe.Init;
  fRest := aRest;
  fOwnRest := aOwnRest;
  if fThreadName = '' then
    // if thread name has not been set by the overriden constructor
    FormatUTF8('% %', [self, fRest.Model.Root], fThreadName);
  fEvent := TEvent.Create(nil, false, false, '');
  inherited Create(aCreateSuspended);
end;

procedure TRestThread.WaitForNotExecuting(maxMS: integer);
var
  endtix: Int64;
begin
  if fExecuting then
  begin
    endtix := mormot.core.os.GetTickCount64 + maxMS;
    repeat
      SleepHiRes(1); // wait for InternalExecute to finish
    until not fExecuting or
              (mormot.core.os.GetTickCount64 >= endtix);
  end;
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
    FreeAndNil(fRest);
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
  fLog := fRest.fLogClass.Add;
  SetCurrentThreadName('%', [fThreadName]);
  fRest.fRun.BeginCurrentThread(self);
  try
    fExecuting := true;
    try
      InternalExecute;
    except
      on E: Exception do
        fLog.Add.Log(sllError, 'Unhandled % in %.Execute -> abort',
          [E, ClassType], self);
    end;
  finally
    fRest.fRun.EndCurrentThread(self);
    fLog := nil; // no log after EndCurrentThread
    fExecuting := false;
  end;
end;

{$ifndef HASTTHREADSTART}
procedure TRestThread.Start;
begin
  Resume;
end;
{$endif HASTTHREADSTART}

{$ifndef HASTTHREADTERMINATESET}
procedure TRestThread.Terminate;
begin
  inherited Terminate; // FTerminated := True
  TerminatedSet;
end;
{$endif HASTTHREADTERMINATESET}

procedure TRestThread.TerminatedSet;
begin
  fEvent.SetEvent;
end;



{ ************ TRestRunThreads Multi-Threading Process of a REST instance }

{ TRestRunThreads }

constructor TRestRunThreads.Create(aOwner: TRest);
begin
  inherited Create;
  fOwner := aOwner;
end;

destructor TRestRunThreads.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fBackgroundTimer);
end;

function TRestRunThreads.EnsureBackgroundTimerExists: TRestBackgroundTimer;
begin
  if self = nil then
  begin
    result := nil; // paranoid check to avoid any GPF
    exit;
  end;
  fSafe.Lock;
  try
    if fBackgroundTimer = nil then
      fBackgroundTimer := TRestBackgroundTimer.Create(fOwner);
    result := fBackgroundTimer;
  finally
    fSafe.UnLock;
  end;
end;

function TRestRunThreads.NewBackgroundThreadMethod(const Format: RawUTF8;
   const Args: array of const): TSynBackgroundThreadMethod;
begin
  if self = nil then
    result := nil
  else
    result := TSynBackgroundThreadMethod.Create(nil, FormatUTF8(Format, Args),
      BeginCurrentThread, EndCurrentThread);
end;

function TRestRunThreads.NewParallelProcess(ThreadCount: integer; const Format: RawUTF8;
  const Args: array of const): TSynParallelProcess;
begin
  if self = nil then
    result := nil
  else
    result := TSynParallelProcess.Create(ThreadCount, FormatUTF8(Format, Args),
      BeginCurrentThread, EndCurrentThread);
end;

function TRestRunThreads.NewBackgroundThreadProcess(
  const aOnProcess: TOnSynBackgroundThreadProcess; aOnProcessMS: cardinal;
  const Format: RawUTF8; const Args: array of const;
  aStats: TSynMonitorClass): TSynBackgroundThreadProcess;
var
  name: RawUTF8;
begin
  FormatUTF8(Format, Args, name);
  if self = nil then
    result := TSynBackgroundThreadProcess.Create(name, aOnProcess, aOnProcessMS,
      nil, nil, aStats)
  else
    result := TSynBackgroundThreadProcess.Create(name, aOnProcess, aOnProcessMS,
      BeginCurrentThread, EndCurrentThread, aStats);
end;

function TRestRunThreads.TimerEnable(
  const aOnProcess: TOnSynBackgroundTimerProcess;
  aOnProcessSecs: cardinal): TRestBackgroundTimer;
begin
  result := nil;
  if self = nil then
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
     (fBackgroundTimer = nil) then
    result := false
  else
    result := fBackgroundTimer.Disable(aOnProcess);
end;

function TRestRunThreads.SystemUseTrack(periodSec: integer): TSystemUse;
begin
  result := nil;
  if self = nil then
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

procedure TRestRunThreads.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInterface: IInvokable; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
begin
  if self <> nil then
    EnsureBackgroundTimerExists.AsynchRedirect(
      aGUID, aDestinationInterface, aCallbackInterface, aOnResult);
end;

procedure TRestRunThreads.AsynchRedirect(const aGUID: TGUID;
  const aDestinationInstance: TInterfacedObject; out aCallbackInterface;
  const aOnResult: TOnAsynchRedirectResult);
begin
  if self <> nil then
    EnsureBackgroundTimerExists.AsynchRedirect(
      aGUID, aDestinationInstance, aCallbackInterface, aOnResult);
end;

procedure TRestRunThreads.AsynchInterning(Interning: TRawUTF8Interning;
  InterningMaxRefCount, PeriodMinutes: integer);
begin
  if self <> nil then
    EnsureBackgroundTimerExists.AsynchInterning(
      Interning, InterningMaxRefCount, PeriodMinutes);
end;

function TRestRunThreads.MultiRedirect(const aGUID: TGUID; out aCallbackInterface;
  aCallBackUnRegisterNeeded: boolean): IMultiCallbackRedirect;
var factory: TInterfaceFactory;
begin
  if self = nil then
    result := nil
  else
  begin
    factory := TInterfaceFactory.Get(aGUID);
    if factory = nil then
      raise EServiceException.CreateUTF8('%.MultiRedirect: unknown %',
        [self, GUIDToShort(aGUID)]);
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
  const FieldName: RawUTF8; Options: TOrmInitializeTableOptions);
begin
  inherited InitializeTable(Server, FieldName, Options);
  if FieldName = '' then
    Server.CreateSQLMultiIndex(self, ['ModifiedRecord', 'Event'], false);
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
    raise EOrmException.CreateUTF8('Invalid %.CreateHistory(%,%,%) call',
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
    raise EOrmException.CreateUTF8('HistoryOpen in %.CreateHistory(%,%,%)',
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
        rec.FillFrom(pointer(hist.SentDataJSON));
        HistoryAdd(rec, hist);
      end;
      HistorySave(nil); // update internal fHistory field
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
  var JSON: RawUTF8);
begin
  // you may use a TDocVariant to add some custom fields in your own class
  JSON := JSONEncode(Fields);
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
    if not fHistoryTable.RecordProps.CheckBinaryHeader(R) then
      // invalid content: TOrm layout may have changed
      exit;
    R.ReadVarUInt32Array(fHistoryUncompressedOffset);
    fHistoryUncompressedCount := length(fHistoryUncompressedOffset);
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
  P, PEnd: PAnsiChar;
begin
  result := false;
  if cardinal(Index) >= cardinal(HistoryCount) then
    exit;
  P := pointer(fHistoryUncompressed);
  PEnd := P + length(fHistoryUncompressed);
  inc(P, fHistoryUncompressedOffset[Index]);
  if P >= PEnd then
    exit;
  Event := TOrmHistoryEvent(P^);
  inc(P);
  P := pointer(FromVarUInt64Safe(pointer(P), pointer(PEnd), PQWord(@Timestamp)^));
  if P = nil then
    exit;
  if (Rec <> nil) and
     (Rec.RecordClass = fHistoryTable) then
  begin
    if Event = heDelete then
      Rec.ClearProperties
    else
      Rec.SetBinaryValuesSimpleFields(P, PEnd);
    Rec.IDValue := ModifiedID;
    if P = nil then
      exit;
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
      FreeAndNil(result);
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
        newOffset[firstNewIndex + i] :=
          fHistoryAddOffset[i] + firstNewOffset;
      // write header
      fHistoryTable.RecordProps.SaveBinaryHeader(W);
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
          'Timestamp', Int64ToUTF8(Server.GetServerTimestamp),
          'RowID', Int64ToUtf8(fID));
        Server.UpdateBlob(RecordClass, fID,
          RecordProps.BlobFields[0].Name, fHistory);
      end;
      result := true;
    finally
      W.Free;
    end;
  finally
    fHistoryUncompressed := '';
    fHistoryUncompressedOffset := nil;
    FreeAndNil(fHistoryAdd);
    fHistoryAddOffset := nil;
    fHistoryAddCount := 0;
  end;
end;



end.

