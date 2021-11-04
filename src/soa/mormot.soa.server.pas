/// Interface-based SOA Process Types and Classes for Server-Side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.soa.server;

{
  *****************************************************************************

   Server-Side Interface-based Service Oriented Architecture (SOA) Process
    - TInjectableObjectRest Service Implementation Parent Class
    - TServiceFactoryServer Service Provider
    - TServiceContainerServer Services Holder
    - Asynchronous REST Synchronisation Classes
    
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
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.db.core,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.soa.core,
  mormot.soa.client,
  mormot.rest.core,
  mormot.rest.client,
  mormot.rest.server;



{ ***************** TInjectableObjectRest Service Implementation Parent Class }

type
  TServiceFactoryServer = class;

  /// service implementation class, with direct access on the associated
  // TServiceFactoryServer/TRestServer instances
  // - allow dependency injection aka SOLID DI/IoC by the framework using
  // inherited TInjectableObject.Resolve() methods
  // - allows direct access to the underlying ORM using its Server method
  // - this class will allow Server instance access outside the scope of
  // remote SOA execution, e.g. when a DI is performed on server side: it
  // is therefore a better alternative to ServiceRunningContext.Factory,
  // ServiceRunningContext.Factory.RestServer or ServiceRunningContext.Request.Server
  TInjectableObjectRest = class(TInjectableObject)
  protected
    fFactory: TServiceFactoryServer;
    fServer: TRestServer;
  public
    /// initialize an instance, defining associated dependencies
    // - the resolver may be e.g. a TServiceContainer
    // - once the DI/IoC is defined, will call the AutoResolve() protected method
    // - as called by  TServiceFactoryServer.CreateInstance
    constructor CreateWithResolverAndRest(aResolver: TInterfaceResolver;
      aFactory: TServiceFactoryServer; aServer: TRestServer;
      aRaiseEServiceExceptionIfNotFound: boolean = true); virtual;
    /// access to the associated interface factory
    // - this property will be injected by TServiceFactoryServer.CreateInstance,
    // so may be nil if the instance was created outside the SOA context
    property Factory: TServiceFactoryServer
      read fFactory;
    /// access ot the associated REST Server, e.g. to its ORM methods
    // - slightly faster than Factory.RestServer
    // - this value will be injected by TServiceFactoryServer.CreateInstance,
    // so may be nil if the instance was created outside the SOA context
    property Server: TRestServer
      read fServer;
  end;

  /// class-reference type (metaclass) of a TInjectableObjectRest type
  TInjectableObjectRestClass = class of TInjectableObjectRest;


{ ***************** TServiceFactoryServer Service Provider }

  /// server-side service provider uses this to store one internal instance
  // - used by TServiceFactoryServer in sicClientDriven, sicPerSession,
  // sicPerUser or sicPerGroup mode
  {$ifdef USERECORDWITHMETHODS}
  TServiceFactoryServerInstance = record
  {$else}
  TServiceFactoryServerInstance = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the internal Instance ID, as remotely sent in "id":1
    // - is set to 0 when an entry in the array is free
    InstanceID: TID;
    /// GetTickCount64() timestamp corresponding to the last access of
    // this instance
    LastAccess64: Int64;
    /// the associated client session
    Session: cardinal;
    /// the implementation instance itself
    Instance: TInterfacedObject;
    /// used to release the implementation instance
    // - direct FreeAndNil(Instance) may lead to A/V if self has been assigned
    // to an interface to any sub-method on the server side -> dec(RefCount)
    procedure SafeFreeInstance(Factory: TServiceFactoryServer);
  end;

  /// server-side service provider uses this to store its internal instances
  // - used by TServiceFactoryServer in sicClientDriven, sicPerSession,
  // sicPerUser or sicPerGroup mode
  TServiceFactoryServerInstanceDynArray = array of TServiceFactoryServerInstance;

  /// callback called before any interface-method service execution to allow
  // its execution
  // - see Ctxt.Service, Ctxt.ServiceMethodIndex and Ctxt.ServiceParameters
  // to identify the executed method context
  // - Method parameter will help identify easily the corresponding method, and
  // will contain in fact PServiceMethod(Ctxt.ServiceMethod)^
  // - should return TRUE if the method can be executed
  // - should return FALSE if the method should not be executed, and set the
  // corresponding error to the supplied context e.g.
  // ! Ctxt.Error('Unauthorized method',HTTP_NOTALLOWED);
  // - i.e. called by TRestServerUriContext.InternalExecuteSoaByInterface
  TOnServiceCanExecute = function(Ctxt: TRestServerUriContext;
    const Method: TInterfaceMethod): boolean of object;

  /// callbacked used by TServiceFactoryServer.RunOnAllInstances method
  TOnServiceFactoryServerOne = function(Sender: TServiceFactoryServer;
    var Instance: TServiceFactoryServerInstance; var Opaque): integer of object;

  /// a service provider implemented on the server side
  // - each registered interface has its own TServiceFactoryServer instance,
  // available as one TServiceContainerServer item from TRest.Services property
  // - will handle the implementation class instances of a given interface
  // - by default, all methods are allowed to execution: you can call AllowAll,
  // DenyAll, Allow or Deny in order to specify your exact security policy
  TServiceFactoryServer = class(TServiceFactoryServerAbstract)
  protected
    fRestServer: TRestServer; // just a transtyped fResolver value
    fInstance: TServiceFactoryServerInstanceDynArray;
    fInstances: TDynArray;
    fInstanceCapacity: integer; // some void entries may have P^.InstanceID=0
    fInstanceCount: integer;
    fInstanceCurrentID: TID;
    fInstanceTimeOut: cardinal;
    fInstanceLock: TRTLCriticalSection;
    fStats: TSynMonitorInputOutputObjArray;
    fImplementationClass: TInterfacedClass;
    fImplementationClassKind: (
      ickBlank, ickWithCustomCreate, ickInjectable, ickInjectableRest,
      ickFromInjectedResolver, ickFake);
    fImplementationClassInterfaceEntry: PInterfaceEntry;
    fSharedInterface: IInterface;
    fBackgroundThread: TSynBackgroundThreadMethod;
    fOnMethodExecute: TOnServiceCanExecute;
    fOnExecute: array of TOnInterfaceMethodExecute;
    procedure SetServiceLogByIndex(const aMethods: TInterfaceFactoryMethodBits;
      const aLogRest: IRestOrm; aLogClass: TOrmServiceLogClass);
    procedure SetTimeoutSecInt(value: cardinal);
    function GetTimeoutSec: cardinal;
    function GetStat(const aMethod: RawUtf8): TSynMonitorInputOutput;
    /// called by ExecuteMethod to append input/output params to Sender.TempTextWriter
    procedure OnLogRestExecuteMethod(Sender: TInterfaceMethodExecuteRaw;
      Step: TInterfaceMethodExecuteEventStep);
    /// this method will create an implementation instance
    // - reference count will be set to one, in order to allow safe passing
    // of the instance into an interface, if AndIncreaseRefCount is TRUE
    // - will handle TInterfacedObjectWithCustomCreate and TInjectableObject
    // as expected, if necessary
    function CreateInstance(AndIncreaseRefCount: boolean): TInterfacedObject;
  public
    /// initialize the service provider on the server side
    // - expect an direct server-side implementation class, which may inherit
    // from plain TInterfacedClass, TInterfacedObjectWithCustomCreate if you
    // need an overridden constructor, or TInjectableObject to support DI/IoC
    // - for sicClientDriven, sicPerSession, sicPerUser or sicPerGroup modes,
    // a time out (in seconds) can be defined (default is 30 minutes) - if the
    // specified aTimeOutSec is 0, interface will be forced in sicSingle mode
    // - you should usualy have to call the TRestServer.ServiceRegister()
    // method instead of calling this constructor directly
    constructor Create(aRestServer: TRestServer; aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      aImplementationClass: TInterfacedClass; const aContractExpected: RawUtf8;
      aTimeOutSec: cardinal; aSharedInstance: TInterfacedObject); reintroduce;
    /// release all used memory
    // - e.g. any internal TServiceFactoryServerInstance instances (any shared
    // instance, and all still living instances in sicClientDriven mode)
    destructor Destroy; override;

    /// you can define here an event to allow/deny execution of any method
    // of this service, at runtime
    property OnMethodExecute: TOnServiceCanExecute
      read fOnMethodExecute write fOnMethodExecute;
    /// allow to hook the methods execution
    // - several events could be registered, and will be called directly
    // before and after method execution
    // - if optInterceptInputOutput is defined in Options, then Sender.Input/Output
    // fields will contain the execution data context when Hook is called
    // - see OnMethodExecute if you want to implement security features
    procedure AddInterceptor(const Hook: TOnInterfaceMethodExecute);

    /// retrieve an instance of this interface from the server side
    // - sicShared mode will retrieve the shared instance
    // - sicPerThread mode will retrieve the instance corresponding to the
    // current running thread
    // - all other kind of instance creation will behave the same as sicSingle
    // when accessed directly from this method, i.e. from server side: in fact,
    // on the server side, there is no notion of client, session, user nor group
    // - if ServiceRunningContext.Factory is nil (i.e. if there is no other
    // service context currently associated), this method will also update
    // ServiceRunningContext.Factory, so that the implementation method will be able
    // to access the associated TRestServer instance if needed
    function Get(out Obj): boolean; override;
    /// retrieve the published signature of this interface
    // - is always available on TServiceFactoryServer, but TServiceFactoryClient
    // will be able to retrieve it only if TServiceContainerServer.PublishSignature
    // is set to TRUE (which is not the default setting, for security reasons)
    function RetrieveSignature: RawUtf8; override;
    /// call a given method of this service provider
    // - here Ctxt.ServiceMethod points to the corresponding fInterface.Methods[]
    // (i.e. excluding _free_/_contract_/_signature_ pseudo-methods)
    // - Ctxt.ServiceMethodIndex=0=ord(imFree) will free/release
    // the corresponding aInstanceID - as called  e.g. from
    // $ {"method":"_free_", "params":[], "id":1234}
    // - Ctxt.ServiceParameters is e.g. '[1,2]' i.e. a true JSON array, which
    // will contain the incoming parameters in the same exact order than the
    // corresponding implemented interface method
    // - Ctxt.ID is an optional number, to be used in case of sicClientDriven
    // kind of Instance creation to identify the corresponding client session
    // - returns 200/HTTP_SUCCESS on success, or an HTTP error status, with an
    // optional error message in aErrorMsg
    // - on success, Ctxt.Call.OutBody shall contain a serialized JSON object
    // with one nested result property, which may be a JSON array, containing
    // all "var" or "out" parameters values, and then the method main result -
    // for instance, ExecuteMethod(..,'[1,2]') over ICalculator.Add will return:
    // $ {"result":[3],"id":0}
    // the returned "id" number is the Instance identifier to be used for any later
    // sicClientDriven remote call - or just 0 in case of sicSingle or sicShared
    procedure ExecuteMethod(Ctxt: TRestServerUriContext);
    /// call the supplied aEvent callback for all class instances implementing
    // this service
    function RunOnAllInstances(const aEvent: TOnServiceFactoryServerOne;
      var aOpaque): integer;
    /// low-level get an implementation Inst.Instance for the given Inst.InstanceID
    // - is called by ExecuteMethod() in sicClientDriven mode
    // - returns -1 on error, or aMethodIndex for successfull execution,
    // e.g. 0 after {"method":"_free_".. call
    // - otherwise, fill Inst.Instance with the matching implementation (or nil)
    function RetrieveInstance(var Inst: TServiceFactoryServerInstance;
      aMethodIndex, aSession: integer): integer;
    /// define the the instance life time-out, in seconds
    function SetTimeoutSec(value: cardinal): TServiceFactoryServerAbstract; override;
    /// log method execution information to a TOrmServiceLog table
    function SetServiceLog(const aMethod: array of RawUtf8;
      const aLogRest: IRestOrm;
      aLogClass: TOrmServiceLogClass = nil): TServiceFactoryServerAbstract; override;
    /// low-level method called from client CacheFlush/_ping_ URI
    function RenewSession(aSession: cardinal): integer;

    /// the associated TRestServer instance
    property RestServer: TRestServer
      read fRestServer;
    /// direct access to per-method detailed process statistics
    // - this Stats[] array follows Interface.Methods[] order
    // - see Stat[] property to retrieve information about a method by name
    property Stats: TSynMonitorInputOutputObjArray
      read fStats;
    /// retrieve detailed statistics about a method use
    // - will return a reference to the actual item in Stats[]: caller should
    // not free the returned instance
    property Stat[const aMethod: RawUtf8]: TSynMonitorInputOutput
      read GetStat;
  published
    /// the class type used to implement this interface
    property ImplementationClass: TInterfacedClass
      read fImplementationClass;
    /// the instance life time-out, in seconds
    // - for sicClientDriven, sicPerSession, sicPerUser or sicPerGroup modes
    // - raise an exception for other kind of execution
    // - you can also use the SetTimeOutSec() fluent function instead
    property TimeoutSec: cardinal
      read GetTimeoutSec write SetTimeoutSecInt;
  end;



{ ***************** TServiceContainerServer Services Holder }

type
  /// service definition for master/slave replication notifications subscribe
  // - implemented by TServiceRecordVersion, as used by
  // TRestServer.RecordVersionSynchronizeMasterStart(), and expected by
  // TRestServer.RecordVersionSynchronizeSlaveStart()
  IServiceRecordVersion = interface(IInvokable)
    ['{06A355CA-19EB-4CC6-9D87-7B48967D1D9F}']
    /// will register the supplied callback for the given table
    function Subscribe(const SqlTableName: RawUtf8;
      const revision: TRecordVersion;
      const callback: IServiceRecordVersionCallback): boolean;
  end;

  /// service definition with a method which will be called when a callback
  // interface instance is released on the client side
  // - may be used to implement safe publish/subscribe mechanism using
  // interface callbacks, e.g. over WebSockets
  IServiceWithCallbackReleased = interface(IInvokable)
    ['{8D518FCB-62C3-42EB-9AE7-96ED322140F7}']
    /// will be called when a callback is released on the client side
    // - this method matches the TInterfaceFactory.MethodIndexCallbackReleased
    // signature, so that it will be called with the interface instance by
    // TServiceContainerServer.FakeCallbackRelease
    // - you may use it as such - see sample Project31ChatServer.dpr:
    // ! procedure TChatService.CallbackReleased(const callback: IInvokable;
    // !   const interfaceName: RawUtf8);
    // ! begin  // unsubscribe from fConnected: array of IChatCallback
    // !   if interfaceName='IChatCallback' then
    // !     InterfaceArrayDelete(fConnected,callback);
    // ! end;
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
  end;

  /// event signature triggerred when a callback instance is released
  // - used by TServiceContainerServer.OnCallbackReleasedOnClientSide
  // and TServiceContainerServer.OnCallbackReleasedOnServerSide event properties
  // - the supplied Instance will be a TInterfacedObjectFakeServer, and the
  // Callback will be a pointer to the corresponding interface value
  // - assigned implementation should be as fast a possible, since this event
  // will be executed in a global lock for all server-side callbacks
  TOnCallbackReleased = procedure(Sender: TServiceContainer;
    Instance: TInterfacedObject; Callback: pointer) of object;

  /// how TServiceContainerServer will handle SOA callbacks
  // - by default, a callback released on the client side will log a warning
  // and continue the execution (relying e.g. on a CallbackReleased() method to
  // unsubscribe the event), but coRaiseExceptionIfReleasedByClient can be
  // defined to raise an EInterfaceFactoryException in this case
  TServiceCallbackOptions = set of (
    coRaiseExceptionIfReleasedByClient);

  /// a services provider class to be used on the server side
  // - this will maintain a list of true implementation classes
  // - inherits from TServiceContainerClientAbstract to allow remote access
  TServiceContainerServer = class(TServiceContainerClientAbstract)
  protected
    fRestServer: TRestServer; // set by Create := fOwner as TRestServer
    fPublishSignature: boolean;
    fConnectionID: TRestConnectionID;
    fFakeCallbacks: TSynObjectListLocked; // TInterfacedObjectFakeServer instances
    fOnCallbackReleasedOnClientSide: TOnCallbackReleased;
    fOnCallbackReleasedOnServerSide: TOnCallbackReleased;
    fCallbackOptions: TServiceCallbackOptions;
    fRecordVersionCallback: array of IServiceRecordVersionCallbackDynArray;
    fSessionTimeout: cardinal;
    procedure FakeCallbackAdd(aFakeInstance: TObject);
    procedure FakeCallbackRemove(aFakeInstance: TObject);
    function GetFakeCallbacksCount: integer;
    procedure RecordVersionCallbackNotify(TableIndex: integer;
      Occasion: TOrmOccasion; const DeletedID: TID;
      const DeletedRevision: TRecordVersion; const AddUpdateJson: RawUtf8);
  public
    /// initialize the list
    constructor Create(aOwner: TInterfaceResolver); override;
    /// finalize the service container
    destructor Destroy; override;
    /// method called on the server side to register a service via its
    // interface(s) and a specified implementation class or a shared
    // instance (for sicShared mode)
    // - will add a TServiceFactoryServer instance to the internal list
    // - will raise an exception on error
    // - will return the first of the registered TServiceFactoryServer created
    // on success (i.e. the one corresponding to the first item of the aInterfaces
    // array), or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    // - the same implementation class can be used to handle several interfaces
    // (just as Delphi allows to do natively)
    function AddImplementation(aImplementationClass: TInterfacedClass;
      const aInterfaces: array of PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      aSharedImplementation: TInterfacedObject;
      const aContractExpected: RawUtf8): TServiceFactoryServer;
    /// initialize and register a server-side interface callback instance
    procedure GetFakeCallback(Ctxt: TRestServerUriContext;
      ParamInterfaceInfo: PRttiInfo; FakeID: PtrInt; out Obj);
    /// low-level function called from TRestServer.CacheFlush URI method
    procedure FakeCallbackRelease(Ctxt: TRestServerUriContext);
    /// class method able to check if a given server-side callback event fake
    // instance has been released on the client side
    // - may be used to automatically purge a list of subscribed callbacks,
    // e.g. before trigerring the interface instance, and avoid an exception
    // - can optionally append the callback class instance information to
    // a local shortstring variable, e.g. for logging/debug purposes
    class function CallbackReleasedOnClientSide(const callback: IInterface;
      callbacktext: PShortString = nil): boolean; overload;
    /// replace the connection ID of callbacks after a reconnection
    // - returns the number of callbacks changed
    function FakeCallbackReplaceConnectionID(
      aConnectionIDOld, aConnectionIDNew: TRestConnectionID): integer;
    /// register a callback interface which will be called each time a write
    // operation is performed on a given TOrm with a TRecordVersion field
    // - called e.g. by TRestServer.RecordVersionSynchronizeSubscribeMaster
    function RecordVersionSynchronizeSubscribeMaster(
      TableIndex: integer; RecordVersion: TRecordVersion;
      const SlaveCallback: IServiceRecordVersionCallback): boolean;
    /// notify any TRecordVersion callback for a table Add/Update from a
    // TDocVariant content
    // - used e.g. by TRestStorageMongoDB.DocFromJson()
    procedure RecordVersionNotifyAddUpdate(Occasion: TOrmOccasion;
      TableIndex: integer; const Document: TDocVariantData); overload;
    /// notify any TRecordVersion callback for a table Add/Update from a
    // TJsonObjectDecoder content
    // - used e.g. by TRestStorageMongoDB.DocFromJson()
    procedure RecordVersionNotifyAddUpdate(Occasion: TOrmOccasion;
      TableIndex: integer; const Decoder: TJsonObjectDecoder); overload;
    /// notify any TRecordVersion callback for a table Delete
    procedure RecordVersionNotifyDelete(TableIndex: integer;
      const ID: TID; const Revision: TRecordVersion);
    /// make some garbage collection when session is finished
    procedure OnCloseSession(aSessionID: cardinal); virtual;
    /// log method execution information to a TOrmServiceLog table
    // - TServiceFactoryServer.SetServiceLog() will be called for all registered
    // interfaced-based services of this container
    // - will write to the specified aLogRest instance, and will disable
    // writing if aLogRest is nil
    // - will write to a (inherited) TOrmServiceLog table, as available in
    // TRest's model, unless a dedicated table is specified as aLogClass
    // - you could specify a CSV list of method names to be excluded from logging
    // (containing e.g. a password or a credit card number), containing either
    // the interface name (as 'ICalculator.Add'), or not (as 'Add')
    procedure SetServiceLog(const aLogRest: IRestOrm;
      aLogClass: TOrmServiceLogClass = nil;
      const aExcludedMethodNamesCsv: RawUtf8 = '');
    /// defines if the "method":"_signature_" or /root/Interface._signature
    // pseudo method is available to retrieve the whole interface signature,
    // encoded as a JSON object
    // - is set to FALSE by default, for security reasons: only "_contract_"
    // pseudo method is available - see TServiceContainer.ContractExpected
    property PublishSignature: boolean
      read fPublishSignature write fPublishSignature;
    /// the default TServiceFactoryServer.TimeoutSec value
    // - default is 30 minutes
    // - you can customize each service using its corresponding TimeoutSec property
    property SessionTimeout: cardinal
      read fSessionTimeout write fSessionTimeout;
    /// this event will be launched when a callback interface is notified as
    // relased on the Client side
    // - as an alternative, you may define the following method on the
    // registration service interface type, which will be called when a
    // callback registered via this service is released (e.g. to unsubscribe
    // the callback from an interface list, via InterfaceArrayDelete):
    // ! procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUtf8);
    property OnCallbackReleasedOnClientSide: TOnCallbackReleased
      read fOnCallbackReleasedOnClientSide;
    /// this event will be launched when a callback interface is relased on
    // the Server side
    property OnCallbackReleasedOnServerSide: TOnCallbackReleased
      read fOnCallbackReleasedOnServerSide;
    /// defines how SOA callbacks will be handled
    property CallbackOptions: TServiceCallbackOptions
      read fCallbackOptions write fCallbackOptions;
  published
    /// how many interface callbackas are currently registered
    property FakeCallbacksCount: integer
      read GetFakeCallbacksCount;
  end;


{ ***************** Asynchronous REST Synchronisation Classes }

type
  /// this class implements a service, which may be called to push notifications
  // for master/slave replication
  // - as used by TRestServer.RecordVersionSynchronizeMasterStart(), and
  // expected by TRestServer.RecordVersionSynchronizeSlaveStart()
  TServiceRecordVersion = class(TInjectableObjectRest, IServiceRecordVersion)
  public
    /// will register the supplied callback for the given table
    function Subscribe(const SqlTableName: RawUtf8;
      const revision: TRecordVersion;
      const callback: IServiceRecordVersionCallback): boolean;
   end;

  /// this class implements a callback interface, able to write all remote ORM
  // notifications to the local DB
  // - could be supplied as callback parameter, possibly via WebSockets
  // transmission, to TRestServer.RecordVersionSynchronizeSubscribeMaster()
  TServiceRecordVersionCallback = class(TInterfacedCallback,
    IServiceRecordVersionCallback)
  protected
    fTable: TOrmClass;
    fRecordVersionField: TOrmPropInfoRttiRecordVersion;
    fBatch: TRestBatch;
    fSlave: TRestServer; // fRest is master remote access
    fOnNotify: TOnBatchWrite;
    // local TOrmTableDeleted.ID follows current Model -> pre-compute offset
    fTableDeletedIDOffset: Int64;
    procedure SetCurrentRevision(const Revision: TRecordVersion;
      Event: TOrmOccasion);
  public
    /// initialize the instance able to apply callbacks for a given table on
    // a local slave REST server from a remote master REST server
    // - the optional low-level aOnNotify callback will be triggerred for each
    // incoming notification, to track the object changes in real-time
    constructor Create(aSlave: TRestServer; aMaster: TRestClientUri;
      aTable: TOrmClass; const aOnNotify: TOnBatchWrite); reintroduce;
    /// finalize this callback instance
    destructor Destroy; override;
    /// this event will be raised on any Add on a versioned record
    procedure Added(const NewContent: RawJson); virtual;
    /// this event will be raised on any Update on a versioned record
    procedure Updated(const ModifiedContent: RawJson); virtual;
    /// this event will be raised on any Delete on a versioned record
    procedure Deleted(const ID: TID; const Revision: TRecordVersion); virtual;
    /// match TInterfaceFactory.MethodIndexCurrentFrameCallback signature,
    // so that TRestHttpClientWebsockets.CallbackRequest will call it
    // - it will create a temporary TRestBatch for the whole "jumbo frame"
    procedure CurrentFrame(isLast: boolean); virtual;
    /// low-level event handler triggerred by Added/Updated/Deleted methods
    property OnNotify: TOnBatchWrite
      read fOnNotify write fOnNotify;
  end;


const
  ORMVERSION_DELETEID_SHIFT = 58;
  ORMVERSION_DELETEID_RANGE = Int64(1) shl ORMVERSION_DELETEID_SHIFT;


implementation

uses
  mormot.orm.server;


{ ***************** TInjectableObjectRest Service Implementation Parent Class }

{ TInjectableObjectRest }

constructor TInjectableObjectRest.CreateWithResolverAndRest(
  aResolver: TInterfaceResolver; aFactory: TServiceFactoryServer;
  aServer: TRestServer; aRaiseEServiceExceptionIfNotFound: boolean);
begin
  fFactory := aFactory; // may be needed by overriden Create: set before
  fServer := aServer;
  CreateWithResolver(aResolver, aRaiseEServiceExceptionIfNotFound);
end;


{ ***************** TServiceFactoryServer Service Provider }

{ TServiceFactoryServer }

constructor TServiceFactoryServer.Create(aRestServer: TRestServer;
  aInterface: PRttiInfo; aInstanceCreation: TServiceInstanceImplementation;
  aImplementationClass: TInterfacedClass; const aContractExpected: RawUtf8;
  aTimeOutSec: cardinal; aSharedInstance: TInterfacedObject);
begin
  // extract RTTI from the interface
  InitializeCriticalSection(fInstanceLock);
  fRestServer := aRestServer;
  inherited Create(aRestServer, aInterface, aInstanceCreation, aContractExpected);
  if fRestServer.MethodAddress(ShortString(InterfaceUri)) <> nil then
    raise EServiceException.CreateUtf8(
      '%.Create: I% already exposed as % published method',
      [self, InterfaceUri, fRestServer]);
  fImplementationClass := aImplementationClass;
  if fImplementationClass.InheritsFrom(TInterfacedObjectFake) then
  begin
    fImplementationClassKind := ickFake;
    if aSharedInstance = nil then
      raise EServiceException.CreateUtf8('%.Create: no Shared Instance for %/I%',
        [self, fImplementationClass, fInterfaceUri]);
    if (aSharedInstance as TInterfacedObjectFake).
        Factory.InterfaceTypeInfo <> aInterface then
      raise EServiceException.CreateUtf8(
        '%.Create: shared % instance does not implement I%',
        [self, fImplementationClass, fInterfaceUri]);
  end
  else
  begin
    if aRestServer.Services.Implements(fInterface.InterfaceTypeInfo) then
      fImplementationClassKind := ickFromInjectedResolver
    else if fImplementationClass.InheritsFrom(TInjectableObjectRest) then
      fImplementationClassKind := ickInjectableRest
    else if fImplementationClass.InheritsFrom(TInjectableObject) then
      fImplementationClassKind := ickInjectable
    else if fImplementationClass.InheritsFrom(TInterfacedObjectWithCustomCreate) then
      fImplementationClassKind := ickWithCustomCreate;
    fImplementationClassInterfaceEntry := fImplementationClass.
      GetInterfaceEntry(fInterface.InterfaceIID);
    if fImplementationClassInterfaceEntry = nil then
      raise EServiceException.CreateUtf8('%.Create: % does not implement I%',
        [self, fImplementationClass, fInterfaceUri]);
  end;
  if (fInterface.MethodIndexCallbackReleased >= 0) and
     (InstanceCreation <> sicShared) then
    raise EServiceException.CreateUtf8(
      '%.Create: I%() should be run as sicShared', [self,
      fInterface.Methods[fInterface.MethodIndexCallbackReleased].InterfaceDotMethodName]);
  // initialize the shared instance or client driven parameters
  case InstanceCreation of
    sicShared:
      begin
        if aSharedInstance = nil then
          fSharedInstance := CreateInstance(false)
        else if aSharedInstance.InheritsFrom(fImplementationClass) then
          fSharedInstance := aSharedInstance
        else
          raise EServiceException.CreateUtf8(
            '%.Create: % shared instance does not inherit from %',
            [self, aSharedInstance, fImplementationClass]);
        if fImplementationClassKind <> ickFake then
          if (fSharedInstance = nil) or
             not GetInterfaceFromEntry(fSharedInstance,
               fImplementationClassInterfaceEntry, fSharedInterface) then
            raise EServiceException.CreateUtf8(
              '%.Create: % is no implementation of I%',
              [self, fSharedInstance, fInterfaceUri]);
      end;
    sicClientDriven,
    sicPerSession,
    sicPerUser,
    sicPerGroup,
    sicPerThread:
      if (aTimeOutSec = 0) and
         (InstanceCreation <> sicPerThread) then
        fInstanceCreation := sicSingle
      else
      begin
        // only instances list is protected, since client calls shall be pipelined
        fInstances.InitSpecific(TypeInfo(TServiceFactoryServerInstanceDynArray),
          fInstance, ptQWord, @fInstanceCapacity);
        // fInstance[] are compared/sorted by InstanceID: TID
        fInstanceTimeOut := aTimeOutSec * 1000;
      end;
  end;
  SetLength(fStats, fInterface.MethodsCount);
end;

procedure TServiceFactoryServer.SetTimeoutSecInt(value: cardinal);
begin
  if (self = nil) or
     (InstanceCreation in SERVICE_IMPLEMENTATION_NOID) then
    raise EServiceException.CreateUtf8('%.SetTimeoutSecInt() with %',
      [self, ToText(InstanceCreation)^]);
  fInstanceTimeOut := value * 1000;
end;

function TServiceFactoryServer.SetTimeoutSec(
  value: cardinal): TServiceFactoryServerAbstract;
begin
  SetTimeoutSecInt(value);
  result := self;
end;

function TServiceFactoryServer.GetTimeoutSec: cardinal;
begin
  if (self = nil) or
     (InstanceCreation in SERVICE_IMPLEMENTATION_NOID) then
    result := 0
  else
    result := fInstanceTimeout div 1000;
end;

function TServiceFactoryServer.GetStat(
  const aMethod: RawUtf8): TSynMonitorInputOutput;
begin
  result := fStats[fInterface.CheckMethodIndex(aMethod)];
end;

destructor TServiceFactoryServer.Destroy;
var
  i: PtrInt;
begin
  if fInstanceCount > 0 then
    fRestServer.InternalLog('%.Destroy for I% %: fInstanceCount=%',
      [ClassType, fInterfaceUri, ToText(InstanceCreation)^, fInstanceCount], sllDebug);
  try
    EnterCriticalSection(fInstanceLock);
    try
      // release any internal instance (should have been done by client)
      try
        for i := 0 to fInstanceCapacity - 1 do
          if fInstance[i].Instance <> nil then
            fInstance[i].SafeFreeInstance(self);
      finally
        FreeAndNil(fBackgroundThread);
      end;
    except
      ; // better ignore any error in business logic code
    end;
  finally
    LeaveCriticalSection(fInstanceLock);
  end;
  DeleteCriticalSection(fInstanceLock);
  ObjArrayClear(fStats, true);
  inherited Destroy;
end;

function TServiceFactoryServer.Get(out Obj): boolean;
var
  Inst: TServiceFactoryServerInstance;
begin
  result := false;
  if self = nil then
    exit;
  case fInstanceCreation of
    sicShared:
      if fSharedInterface <> nil then
      begin
        // copy implementation interface
        IInterface(Obj) := fSharedInterface;
        result := true;
      end;
    sicPerThread:
      begin // use Length(SERVICE_PSEUDO_METHOD) to create an instance if none
        Inst.Instance := nil;
        Inst.InstanceID := PtrUInt(GetCurrentThreadId);
        if (RetrieveInstance(Inst, Length(SERVICE_PSEUDO_METHOD), 0) >= 0) and
           (Inst.Instance <> nil) then
          result := GetInterfaceFromEntry(Inst.Instance,
            fImplementationClassInterfaceEntry, Obj);
      end;
  else
    begin
      // no user/group/session on pure server-side -> always sicSingle
      Inst.Instance := CreateInstance(false);
      if Inst.Instance <> nil then
        result := GetInterfaceFromEntry(Inst.Instance,
          fImplementationClassInterfaceEntry, Obj);
    end;
  end;
  if result then
    with PServiceRunningContext(PerThreadRunningContextAddress)^ do
      if Factory = nil then
        Factory := self;
end;

function TServiceFactoryServer.RetrieveSignature: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := fContract; // just return the current value
end;

function TServiceFactoryServer.RenewSession(aSession: cardinal): integer;
var
  tix: Int64;
  i: integer;
  P: ^TServiceFactoryServerInstance;
begin
  result := 0;
  if (self = nil) or
     (fInstanceCount = 0) or
     (aSession <= CONST_AUTHENTICATION_NOT_USED) or
     not (fInstanceCreation in [sicClientDriven, sicPerSession]) then
    exit;
  tix := GetTickCount64;
  EnterCriticalSection(fInstanceLock);
  try
    P := pointer(fInstance);
    for i := 1 to fInstanceCapacity do
    begin
      if P^.Session = aSession then
      begin
        P^.LastAccess64 := tix;
        inc(result);
      end;
      inc(P);
    end;
  finally
    LeaveCriticalSection(fInstanceLock);
  end;
end;

function TServiceFactoryServer.RunOnAllInstances(
  const aEvent: TOnServiceFactoryServerOne; var aOpaque): integer;
var
  i: integer;
  P: ^TServiceFactoryServerInstance;
begin
  result := 0;
  if (self = nil) or
     not Assigned(aEvent) or
     (fInstanceCount = 0) then
    exit;
  EnterCriticalSection(fInstanceLock);
  try
    P := pointer(fInstance);
    for i := 1 to fInstanceCapacity do
    begin
      if (P^.InstanceID <> 0) and
         (P^.Instance <> nil) then
        inc(result, aEvent(self, P^, aOpaque));
      inc(P);
    end;
  finally
    LeaveCriticalSection(fInstanceLock);
  end;
end;

procedure TServiceFactoryServerInstance.SafeFreeInstance(
  Factory: TServiceFactoryServer);
var
  Obj: TInterfacedObject;
begin
  if Instance = nil then
    exit; // nothing to release
  dec(Factory.fInstanceCount);
  InstanceID := 0;
  Session := 0;
  Obj := Instance;
  Instance := nil;
  try
    if (optFreeInMainThread in Factory.fAnyOptions) and
       (GetCurrentThreadID <> MainThreadID) then
      BackgroundExecuteInstanceRelease(Obj, nil)
    else if (optFreeInPerInterfaceThread in Factory.fAnyOptions) and
            Assigned(Factory.fBackgroundThread) then
      BackgroundExecuteInstanceRelease(Obj, Factory.fBackgroundThread)
    else
      IInterface(Obj)._Release; // dec(RefCount) + Free if 0
  except
    on E: Exception do
      Factory.RestServer.Internallog('SafeFreeInstance: Ignored % exception ' +
        'during %._Release', [E.ClassType, Factory.InterfaceUri], sllDebug);
  end;
end;

function TServiceFactoryServer.RetrieveInstance(
  var Inst: TServiceFactoryServerInstance;
  aMethodIndex, aSession: integer): integer;

  procedure AddNew;
  var
    i: integer;
    P: ^TServiceFactoryServerInstance;
  begin
    Inst.Session := aSession;
    Inst.Instance := CreateInstance(true);
    if Inst.Instance = nil then
      exit;
    result := aMethodIndex; // notify caller
    inc(fInstanceCount);
    fRestServer.InternalLog(
      '%.RetrieveInstance: Adding %(%) instance (id=%) count=%',
      [ClassType, fInterfaceUri, pointer(Inst.Instance), Inst.InstanceID,
       fInstanceCount], sllDebug);
    P := pointer(fInstance);
    for i := 1 to fInstanceCapacity do
      if P^.InstanceID = 0 then
      begin
        P^ := Inst; // found an empty entry -> re-use it
        exit;
      end
      else
        inc(P);
    fInstances.Add(Inst); // append a new entry
  end;

var
  i: integer;
  P: ^TServiceFactoryServerInstance;
begin
  result := -1;
  Inst.LastAccess64 := GetTickCount64;
  EnterCriticalSection(fInstanceLock);
  try
    // first release any deprecated instances
    if (fInstanceTimeout <> 0) and
       (fInstanceCount > 0) then
    begin
      P := pointer(fInstance);
      for i := 1 to fInstanceCapacity do
      begin
        if (P^.InstanceID <> 0) and
           (Inst.LastAccess64 > P^.LastAccess64 + fInstanceTimeOut) then
        begin
          fRestServer.InternalLog('%.RetrieveInstance: Delete %(%) ' +
            'instance (id=%) after % minutes timeout (max % minutes)',
            [ClassType, fInterfaceUri, pointer(Inst.Instance), P^.InstanceID,
             (Inst.LastAccess64 - P^.LastAccess64) div 60000,
             fInstanceTimeOut div 60000], sllInfo);
          P^.SafeFreeInstance(self);
        end;
        inc(P);
      end;
    end;
    if Inst.InstanceID = 0 then
    begin
      // initialize a new sicClientDriven instance
      if (InstanceCreation <> sicClientDriven) or
         ((cardinal(aMethodIndex - Length(SERVICE_PSEUDO_METHOD)) >=
           fInterface.MethodsCount) and
          (aMethodIndex <> ord(imInstance))) then
        exit;
      inc(fInstanceCurrentID);
      Inst.InstanceID := fInstanceCurrentID;
      AddNew;
    end
    else
    begin
      // search the instance corresponding to Inst.InstanceID
      if fInstanceCount > 0 then
      begin
        P := pointer(fInstance);
        for i := 1 to fInstanceCapacity do
          if P^.InstanceID = Inst.InstanceID then
          begin
            result := aMethodIndex; // notify caller
            if aMethodIndex = ord(imFree) then
            begin
              // {"method":"_free_", "params":[], "id":1234}
              P^.SafeFreeInstance(self);
              exit;
            end;
            P^.LastAccess64 := Inst.LastAccess64;
            Inst.Instance := P^.Instance;
            exit;
          end
          else
            inc(P);
      end;
      // add any new session/user/group/thread instance if necessary
      if (InstanceCreation <> sicClientDriven) and
         (cardinal(aMethodIndex - Length(SERVICE_PSEUDO_METHOD)) <
           fInterface.MethodsCount) then
        AddNew;
    end;
  finally
    LeaveCriticalSection(fInstanceLock);
  end;
end;

function TServiceFactoryServer.CreateInstance(
  AndIncreaseRefCount: boolean): TInterfacedObject;
var
  dummyObj: pointer;
begin
  case fImplementationClassKind of
    ickWithCustomCreate:
      result := TInterfacedObjectWithCustomCreateClass(
        fImplementationClass).Create;
    ickInjectable:
      result := TInjectableObjectClass(
        fImplementationClass).CreateWithResolver(fResolver, true);
    ickInjectableRest:
      result := TInjectableObjectRestClass(fImplementationClass).
        CreateWithResolverAndRest(fResolver, self, fRestServer, true);
    ickFromInjectedResolver:
      begin
        dummyObj := nil;
        if not TServiceContainerServer(fResolver).TryResolve(
            fInterface.InterfaceTypeInfo, dummyObj) then
          raise EInterfaceFactory.CreateUtf8(
            'ickFromInjectedResolver: TryResolve(%) failed',
            [fInterface.InterfaceName]);
        result := TInterfacedObject(ObjectFromInterface(IInterface(dummyObj)));
        // RefCount=1 after TryResolve() -> adjust
        dec(TInjectableObjectRest(result).fRefCount);
      end;
  else
    result := fImplementationClass.Create;
  end;
  inc(TInjectableObjectRest(result).fRefCount); // >0 to call Support() in event
  if Assigned(fRestServer.OnServiceCreateInstance) then
    fRestServer.OnServiceCreateInstance(self, result);
  if not AndIncreaseRefCount then
    dec(TInjectableObjectRest(result).fRefCount);
end;

procedure TServiceFactoryServer.OnLogRestExecuteMethod(
  Sender: TInterfaceMethodExecuteRaw; Step: TInterfaceMethodExecuteEventStep);
var
  W: TJsonWriter;
  a: PtrInt;
  len: integer;
begin
  // append the input/output/error parameters as batch JSON
  W := (Sender as TInterfaceMethodExecute).TempTextWriter;
  with Sender.Method^ do
    case Step of
      smsBefore:
        begin
          W.CancelAll;
          W.AddShort('"POST",{Method:"');
          W.AddString(InterfaceDotMethodName);
          W.AddShort('",Input:{'); // as TOrmPropInfoRttiVariant.GetJsonValues
          if not (optNoLogInput in Sender.Options) then
          begin
            for a := ArgsInFirst to ArgsInLast do
              with Args[a] do
                if (ValueDirection <> imdOut) and
                   (ValueType <> imvInterface) and
                   not IsDefault(Sender.Values[a]) then
                begin
                  W.AddShort(ParamName^); // in JSON_FAST_EXTENDED format
                  W.Add(':');
                  if vIsSpi in ValueKindAsm then
                    W.AddShorter('"****",')
                  else
                  begin
                    AddJson(W, Sender.Values[a], SERVICELOG_WRITEOPTIONS);
                    W.AddComma;
                  end;
                end;
            W.CancelLastComma;
          end;
        end;
      smsAfter:
        begin
          W.AddShort('},Output:{');
          if not (optNoLogOutput in Sender.Options) then
            if ArgsResultIsServiceCustomAnswer then
              with PServiceCustomAnswer(Sender.Values[ArgsResultIndex])^ do
              begin
                len := length(Content);
                W.AddShorter('len:');
                W.AddU(len);
                if (Status <> 0) and
                   (Status <> HTTP_SUCCESS) then
                begin
                  W.AddShorter(',status:');
                  W.AddU(Status);
                end;
                if not fExcludeServiceLogCustomAnswer and
                   (len > 0) and
                   (len <= 1024) then
                begin
                  // write up to 1KB of result binary as base-64 text
                  W.AddShort(',result:"');
                  W.WrBase64(pointer(content), len, false);
                  W.Add('"');
                end;
              end
            else
            begin
              for a := ArgsOutFirst to ArgsOutLast do
                with Args[a] do
                  if (ValueDirection <> imdConst) and
                     not IsDefault(Sender.Values[a]) then
                  begin
                    W.AddShort(ParamName^);
                    W.Add(':');
                    if vIsSpi in ValueKindAsm then
                      W.AddShorter('"****",')
                    else
                    begin
                      AddJson(W, Sender.Values[a], SERVICELOG_WRITEOPTIONS);
                      W.AddComma;
                    end;
                  end;
              W.CancelLastComma;
            end;
        end;
      smsError:
        begin
          W.AddShort('},Output:{');
          W.AddClassName(Sender.LastException.ClassType);
          W.Add(':', '"');
          W.AddJsonEscapeString(Sender.LastException.Message);
          W.Add('"');
        end;
    end;
end;

procedure TServiceFactoryServer.ExecuteMethod(Ctxt: TRestServerUriContext);
var
  Inst: TServiceFactoryServerInstance;
  WR: TJsonWriter;
  entry: PInterfaceEntry;
  instancePtr: pointer; // weak IInvokable reference
  dolock, execres: boolean;
  opt: TInterfaceMethodOptions;
  exec: TInterfaceMethodExecute;
  timeStart, timeEnd: Int64;
  stats: TSynMonitorInputOutput;
  m: PtrInt;
  err: shortstring;
  temp: TTextWriterStackBuffer;

  function GetFullMethodName: RawUtf8;
  begin
    if Ctxt.ServiceMethod <> nil then
      result := PInterfaceMethod(Ctxt.ServiceMethod)^.InterfaceDotMethodName
    else
      result := fInterface.InterfaceName;
  end;

  procedure Error(const Msg: RawUtf8; Status: integer);
  begin
    Ctxt.Error('% % for %',
     [ToText(InstanceCreation)^, Msg, GetFullMethodName], Status);
  end;

  function StatsCreate: TSynMonitorInputOutput;
  begin
    result := TSynMonitorInputOutput.Create(GetFullMethodName);
  end;

  procedure FinalizeLogRest;
  var
    W: TJsonWriter;
  begin
    W := exec.TempTextWriter;
    if exec.CurrentStep < smsBefore then
    begin
      W.CancelAll;
      W.Add('"POST",{Method:"%",Input:{',
        [exec.Method^.InterfaceDotMethodName]);
    end;
    if exec.CurrentStep < smsAfter then
      W.AddShort('},Output:{Failed:"Probably due to wrong input"');
    W.Add('},Session:%,User:%,Time:%,MicroSec:%',
      [integer(Ctxt.Session), Ctxt.SessionUser, TimeLogNowUtc, timeEnd]);
    if Ctxt.RemoteIPIsLocalHost then
      W.Add('}', ',')
    else
      W.Add(',IP:"%"},', [Ctxt.RemoteIP]);
    with Ctxt.ServiceExecution^ do
      IRestOrm(LogRest).AsyncBatchRawAppend(LogClass, W);
  end;

begin
  if mlInterfaces in fRestServer.StatLevels then
    QueryPerformanceMicroSeconds(timeStart);
  // 1. initialize Inst.Instance and Inst.InstanceID
  Inst.InstanceID := 0;
  Inst.Instance := nil;
  case InstanceCreation of
    sicSingle:
      Inst.Instance := CreateInstance(true);
    sicShared:
      Inst.Instance := fSharedInstance;
    sicClientDriven,
    sicPerSession,
    sicPerUser,
    sicPerGroup,
    sicPerThread:
      begin
        case InstanceCreation of
          sicClientDriven:
            Inst.InstanceID := Ctxt.ServiceInstanceID;
          sicPerThread:
            Inst.InstanceID := PtrUInt(GetCurrentThreadId);
        else
          if Ctxt.Session > CONST_AUTHENTICATION_NOT_USED then
            case InstanceCreation of
              // authenticated user -> handle context
              sicPerSession:
                Inst.InstanceID := Ctxt.Session;
              sicPerUser:
                Inst.InstanceID := Ctxt.SessionUser;
              sicPerGroup:
                Inst.InstanceID := Ctxt.SessionGroup;
            end
          else
          begin
            Error('mode expects an authenticated session', HTTP_FORBIDDEN);
            exit;
          end;
        end;
        case RetrieveInstance(Inst, Ctxt.ServiceMethodIndex, Ctxt.Session) of
          ord(imFree):
            begin
              // {"method":"_free_", "params":[], "id":1234}
              Ctxt.Success;
              exit;
            end;
          ord(imInstance):
            begin
              // from TServiceFactoryClient.CreateFakeInstance
              Ctxt.Results([Inst.InstanceID], HTTP_SUCCESS);
              exit;
            end;
        end;
      end;
  end;
  if Inst.Instance = nil then
  begin
    Error('instance not found or deprecated', HTTP_FORBIDDEN);
    exit;
  end;
  Ctxt.ServiceInstanceID := Inst.InstanceID;
  // 2. call method implementation
  if (Ctxt.ServiceExecution = nil) or
     (Ctxt.ServiceMethod = nil) then
  begin
    Error('ServiceExecution=nil', HTTP_SERVERERROR);
    exit;
  end;
  stats := nil;
  if mlInterfaces in fRestServer.StatLevels then
  begin
    m := Ctxt.ServiceMethodIndex - Length(SERVICE_PSEUDO_METHOD);
    if m >= 0 then
    begin
      stats := fStats[m];
      if stats = nil then
      begin
        EnterCriticalSection(fInstanceLock);
        try
          stats := fStats[m];
          if stats = nil then
          begin
            stats := StatsCreate;
            fStats[m] := stats;
          end;
        finally
          LeaveCriticalSection(fInstanceLock);
        end;
      end;
      stats.Processing := true;
    end;
  end;
  err := '';
  exec := nil;
  try
    if fImplementationClassKind = ickFake then
      if Inst.Instance <> fSharedInstance then
        exit
      else
        TInterfacedObjectFake(Inst.Instance).GetNoAddRef(instancePtr)
    else
    begin
      if PClass(Inst.Instance)^ = fImplementationClass then
        entry := fImplementationClassInterfaceEntry
      else
      begin
        entry := Inst.Instance.GetInterfaceEntry(fInterface.InterfaceIID);
        if entry = nil then
          exit;
      end;
      instancePtr := PAnsiChar(Inst.Instance) + entry^.IOffset;
    end;
    opt := Ctxt.ServiceExecution^.Options;
    if optExecInPerInterfaceThread in opt then
      if fBackgroundThread = nil then
        fBackgroundThread := fRestServer.Run.NewBackgroundThreadMethod(
          '% %', [self, fInterface.InterfaceName]);
    WR := TJsonWriter.CreateOwnedStream(temp);
    try
      Ctxt.ThreadServer^.Factory := self;
      if not (optForceStandardJson in opt) and
         ((Ctxt.Call.InHead = '') or
          (Ctxt.ClientKind = ckFramework)) then
        // return extended/optimized pseudo-JSON, as recognized by mORMot
        WR.CustomOptions := WR.CustomOptions + [twoForceJsonExtended]
      else
        // return standard JSON, as expected e.g. by a regular AJAX client
        WR.CustomOptions := WR.CustomOptions + [twoForceJsonStandard];
      if optDontStoreVoidJson in opt then
        WR.CustomOptions := WR.CustomOptions + [twoIgnoreDefaultInRecord];
      // root/calculator {"method":"add","params":[1,2]} -> {"result":[3],"id":0}
      Ctxt.ServiceResultStart(WR);
      dolock := optExecLockedPerInterface in opt;
      if dolock then
        EnterCriticalSection(fInstanceLock);
      exec := TInterfaceMethodExecute.Create(Ctxt.ServiceMethod);
      try
        exec.Options := opt;
        exec.BackgroundExecutionThread := fBackgroundThread;
        exec.OnCallback := Ctxt.ExecuteCallback;
        if fOnExecute <> nil then
          exec.AddInterceptors(exec.OnExecute);
        if Ctxt.ServiceExecution^.LogRest <> nil then
          exec.AddInterceptor(OnLogRestExecuteMethod);
        if (fImplementationClassKind = ickFake) and
           ((Ctxt.ServiceParameters = nil) or
            (Ctxt.ServiceParameters^ = '[')) and
           not ((optExecInMainThread in exec.Options) or
                (optExecInPerInterfaceThread in exec.Options)) and
           (exec.Method^.ArgsOutputValuesCount = 0) then
          // params already as TInterfacedObjectFake expected JSON array
          execres := exec.ExecuteJsonFake(Inst.Instance, Ctxt.ServiceParameters)
        else
          // regular execution
          execres := exec.ExecuteJson([instancePtr], Ctxt.ServiceParameters,
            WR, @err, Ctxt.ForceServiceResultAsJsonObject);
        if not execres then
        begin
          // wrong request returns HTTP error 406
          if err <> '' then
            Ctxt.Error('%', [err], HTTP_NOTACCEPTABLE)
          else
            Error('execution failed (probably due to bad input parameters: ' +
               'e.g. did you initialize your input record(s)?)',
               HTTP_NOTACCEPTABLE);
          exit;
        end;
        Ctxt.Call.OutHead := exec.ServiceCustomAnswerHead;
        Ctxt.Call.OutStatus := exec.ServiceCustomAnswerStatus;
      finally
        if dolock then
          LeaveCriticalSection(fInstanceLock);
      end;
      if Ctxt.Call.OutHead = '' then
      begin
        // <>'' for TServiceCustomAnswer, where body has already been written
        Ctxt.ServiceResultEnd(WR, Inst.InstanceID);
        Ctxt.Call.OutHead := JSON_CONTENT_TYPE_HEADER_VAR;
        Ctxt.Call.OutStatus := HTTP_SUCCESS;
      end;
      WR.SetText(Ctxt.Call.OutBody);
    finally
      Ctxt.ThreadServer^.Factory := nil;
      WR.Free;
    end;
  finally
    try
      if InstanceCreation = sicSingle then
        // always release single shot instance
        Inst.SafeFreeInstance(self);
      if stats <> nil then
      begin
        QueryPerformanceMicroSeconds(timeEnd);
        dec(timeEnd, timeStart);
        Ctxt.StatsFromContext(stats, timeEnd, false);
        if Ctxt.Server.StatUsage <> nil then
          Ctxt.Server.StatUsage.Modified(stats, []);
        if (mlSessions in fRestServer.StatLevels) and
           (Ctxt.AuthSession <> nil) then
        begin
          if Ctxt.AuthSession.Interfaces = nil then
          begin
            EnterCriticalSection(fInstanceLock);
            try
              Ctxt.AuthSession.InterfacesSetLength(
                length(fRestServer.Services.InterfaceMethod));
            finally
              LeaveCriticalSection(fInstanceLock);
            end;
          end;
          m := Ctxt.ServiceListInterfaceMethodIndex;
          if m < 0 then
            m := fRestServer.Services.InterfaceMethods.FindHashed(
              PInterfaceMethod(Ctxt.ServiceMethod)^.InterfaceDotMethodName);
          if m >= 0 then
            with Ctxt.AuthSession do
            begin
              stats := Interfaces[m];
              if stats = nil then
              begin
                EnterCriticalSection(fInstanceLock);
                try
                  stats := Interfaces[m];
                  if stats = nil then
                  begin
                    stats := StatsCreate;
                    Interfaces[m] := stats;
                  end;
                finally
                  LeaveCriticalSection(fInstanceLock);
                end;
              end;
              Ctxt.StatsFromContext(stats, timeEnd, true);
              // mlSessions stats are not yet tracked per Client
            end;
        end;
      end
      else
        timeEnd := 0;
    finally
      if exec <> nil then
      begin
        if Ctxt.ServiceExecution^.LogRest <> nil then
          FinalizeLogRest;
        exec.Free;
      end;
    end;
  end;
end;

procedure TServiceFactoryServer.SetServiceLogByIndex(
  const aMethods: TInterfaceFactoryMethodBits; const aLogRest: IRestOrm;
  aLogClass: TOrmServiceLogClass);
var
  m: PtrInt;
begin
  if aLogRest = nil then
    aLogClass := nil
  else
  begin
    if aLogClass = nil then
      aLogClass := TOrmServiceLog;
    aLogRest.Model.GetTableIndexExisting(aLogClass);
  end;
  for m := 0 to fInterface.MethodsCount - 1 do
    if byte(m) in aMethods then
      with fExecution[m] do
      begin
        LogRest := pointer(aLogRest); // weak pointer to avoid reference counting
        LogClass := aLogClass;
      end;
  if aLogRest <> nil then
    // write every second or after 500 rows in background
    aLogRest.AsyncBatchStart(aLogClass, 1, 500, 1000); // do nothing if already set
end;

function TServiceFactoryServer.SetServiceLog(
  const aMethod: array of RawUtf8; const aLogRest: IRestOrm;
  aLogClass: TOrmServiceLogClass): TServiceFactoryServerAbstract;
var
  bits: TInterfaceFactoryMethodBits;
begin
  if self <> nil then
  begin
    fInterface.CheckMethodIndexes(aMethod, true, bits);
    SetServiceLogByIndex(bits, aLogRest, aLogClass);
  end;
  result := self;
end;

procedure TServiceFactoryServer.AddInterceptor(
  const Hook: TOnInterfaceMethodExecute);
begin
  MultiEventAdd(fOnExecute, TMethod(Hook));
end;


{ ***************** TServiceContainerServer Services Holder }


{ TInterfacedObjectFakeServer }

type
  TInterfacedObjectFakeServer = class(TInterfacedObjectFake)
  protected
    fServer: TRestServer;
    fLowLevelConnectionID: TRestConnectionID;
    fService: TServiceFactoryServer;
    fReleasedOnClientSide: boolean;
    fFakeInterface: Pointer;
    fRaiseExceptionOnInvokeError: boolean;
    function CallbackInvoke(const aMethod: TInterfaceMethod;
      const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
      aFakeID: PInterfacedObjectFakeID;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean; virtual;
  public
    constructor Create(aRequest: TRestServerUriContext;
      aFactory: TInterfaceFactory; aFakeID: integer);
    destructor Destroy; override;
  end;

constructor TInterfacedObjectFakeServer.Create(aRequest: TRestServerUriContext;
  aFactory: TInterfaceFactory; aFakeID: integer);
var
  opt: TInterfacedObjectFakeOptions;
begin
  if aRequest.ClientKind = ckFramework then
    opt := [ifoJsonAsExtended, ifoDontStoreVoidJson]
  else
    opt := [];
  fServer := aRequest.Server;
  fService := aRequest.Service as TServiceFactoryServer;
  fLowLevelConnectionID := aRequest.Call^.LowLevelConnectionID;
  fFakeID := aFakeID;
  inherited Create(aFactory, nil, opt, CallbackInvoke, nil);
  Get(fFakeInterface);
end;

destructor TInterfacedObjectFakeServer.Destroy;
begin
  if fServer <> nil then
  begin
    // may be called asynchronously AFTER server is down
    fServer.InternalLog('%(%:%).Destroy I%',
      [ClassType, pointer(self), fFakeID, fService.InterfaceUri]);
    if fServer.Services <> nil then
      with fServer.Services as TServiceContainerServer do
        if fFakeCallbacks <> nil then
          FakeCallbackRemove(self);
  end;
  inherited Destroy;
end;

function TInterfacedObjectFakeServer.CallbackInvoke(
  const aMethod: TInterfaceMethod; const aParams: RawUtf8;
  aResult, aErrorMsg: PRawUtf8; aFakeID: PInterfacedObjectFakeID;
  aServiceCustomAnswer: PServiceCustomAnswer): boolean;
begin
  // here aClientDrivenID^ = FakeCall ID
  if fServer = nil then
  begin
    if aErrorMsg <> nil then
      aErrorMsg^ := 'Server was already shutdown';
    result := true;
    exit;
  end;
  if not Assigned(fServer.OnNotifyCallback) then
    raise EServiceException.CreateUtf8('%(%) does not support callbacks for I%',
      [fServer, fServer.Model.Root, aMethod.InterfaceDotMethodName]);
  if fReleasedOnClientSide then
  begin
    // there is no client side to call any more
    if not IdemPropName(fFactory.InterfaceTypeInfo^.RawName, 'ISynLogCallback') then
      fServer.InternalLog('%.CallbackInvoke: % instance has been released on ' +
        'the client side, so I% callback notification was NOT sent', [self,
        fFactory.InterfaceTypeInfo^.RawName, aMethod.InterfaceDotMethodName], sllWarning);
    if fRaiseExceptionOnInvokeError or
       ((fServer.Services <> nil) and
        (coRaiseExceptionIfReleasedByClient in
         (fServer.Services as TServiceContainerServer).CallbackOptions)) then
    begin
      if aErrorMsg <> nil then
        FormatUtf8('%.CallbackInvoke(I%): instance has been released on client side',
          [self, aMethod.InterfaceDotMethodName], aErrorMsg^);
      result := false; // will raise an exception
    end
    else
      // do not raise an exception here: just log warning
      result := true;
  end
  else
  begin
    // make the (maybe asynchronous) callback call from server to client
    if aMethod.ArgsOutputValuesCount = 0 then
      // no result -> asynchronous non blocking callback
      aResult := nil;
    result := fServer.OnNotifyCallback(fServer, aMethod.InterfaceDotMethodName,
      aParams, fLowLevelConnectionID, aFakeID^, aResult, aErrorMsg);
  end;
end;


{ TServiceContainerServer }

constructor TServiceContainerServer.Create(aOwner: TInterfaceResolver);
begin
  inherited Create(aOwner);
  fRestServer := aOwner as TRestServer;
  fSessionTimeout := 30 * 60; // 30 minutes by default
end;

destructor TServiceContainerServer.Destroy;
var
  i: PtrInt;
begin
  if fFakeCallbacks <> nil then
  begin
    for i := 0 to fFakeCallbacks.Count - 1 do
      // prevent GPF in Destroy
      TInterfacedObjectFakeServer(fFakeCallbacks.List[i]).fServer := nil;
    FreeAndNil(fFakeCallbacks); // do not own objects
  end;
  fRecordVersionCallback := nil; // done after fFakeCallbacks[].fServer := nil
  inherited Destroy;
end;

function TServiceContainerServer.AddImplementation(
  aImplementationClass: TInterfacedClass; const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  aSharedImplementation: TInterfacedObject;
  const aContractExpected: RawUtf8): TServiceFactoryServer;
var
  i, j: PtrInt;
  UID, implemented: PGuidDynArray;
  F: TServiceFactoryServer;
begin
  result := nil;
  // check input parameters
  if (self = nil) or
     (aImplementationClass = nil) or
     (high(aInterfaces) < 0) then
    exit;
  if aSharedImplementation <> nil then
    if (aSharedImplementation.ClassType <> aImplementationClass) or
       (aInstanceCreation <> sicShared) then
      raise EServiceException.CreateUtf8('%.AddImplementation: invalid % class',
        [self, aSharedImplementation]);
  CheckInterface(aInterfaces);
  SetLength(UID, length(aInterfaces));
  for j := 0 to high(aInterfaces) do
    UID[j] := pointer(aInterfaces[j]^.InterfaceGuid);
  // check all interfaces available in aSharedImplementation/aImplementationClass
  if (aSharedImplementation <> nil) and
     aSharedImplementation.InheritsFrom(TInterfacedObjectFake) then
  begin
    // TInterfacedObjectFake has no RTTI
    if IsEqualGuid(UID[0],
        @TInterfacedObjectFake(aSharedImplementation).Factory.InterfaceIID) then
      UID[0] := nil; // mark TGUID implemented by this fake interface
  end
  else
  begin
    // search all implemented TGUID for this class
    implemented := GetRttiClassGuid(aImplementationClass);
    for j := 0 to high(UID) do
      for i := 0 to high(implemented) do
        if IsEqualGuid(UID[j], implemented[i]) then
        begin
          UID[j] := nil; // mark TGUID found
          break;
        end;
  end;
  for j := 0 to high(UID) do
    if UID[j] <> nil then
      raise EServiceException.CreateUtf8('%.AddImplementation: % not found in %',
        [self, aInterfaces[j]^.RawName, aImplementationClass]);
  // register this implementation class
  for j := 0 to high(aInterfaces) do
  begin
    F := TServiceFactoryServer.Create(fRestServer, aInterfaces[j],
      aInstanceCreation, aImplementationClass, aContractExpected,
      fSessionTimeout, aSharedImplementation);
    if result = nil then
    begin
      result := F; // returns the first registered interface
      if (aInstanceCreation = sicShared) and
         (aSharedImplementation = nil) then
        // re-use existing instance
        aSharedImplementation := F.fSharedInstance;
    end;
    AddServiceInternal(F);
  end;
end;

procedure TServiceContainerServer.OnCloseSession(aSessionID: cardinal);
var
  i, j: PtrInt;
  P: ^TServiceFactoryServerInstance;
  fact: TServiceFactoryServer;
  inst: TServiceFactoryServerInstance;
begin
  for i := 0 to high(fInterface) do
  begin
    fact := TServiceFactoryServer(fInterface[i].Service);
    if fact.fInstanceCount > 0 then
      case fact.InstanceCreation of
        sicPerSession:
          begin
            inst.InstanceID := aSessionID;
            fact.RetrieveInstance(inst, ord(imFree), aSessionID);
          end;
        sicClientDriven:
          begin
            // release ASAP if was not notified by client
            EnterCriticalSection(fact.fInstanceLock);
            try
              P := pointer(fact.fInstance);
              for j := 1 to fact.fInstanceCapacity do
              begin
                if P^.Session = aSessionID then
                  P^.SafeFreeInstance(fact);
                inc(P);
              end;
            finally
              LeaveCriticalSection(fact.fInstanceLock);
            end;
          end;
      end;
  end;
end;

procedure TServiceContainerServer.FakeCallbackAdd(aFakeInstance: TObject);
begin
  if self = nil then
    exit;
  if fFakeCallbacks = nil then
    fFakeCallbacks := TSynObjectListLocked.Create(false);
  fFakeCallbacks.Add(aFakeInstance);
end;

procedure TServiceContainerServer.FakeCallbackRemove(aFakeInstance: TObject);
var
  i: PtrInt;
  callbackID: TInterfacedObjectFakeID;
  connectionID: TRestConnectionID;
  fake: TInterfacedObjectFakeServer;
begin
  if (self = nil) or
     (fFakeCallbacks = nil) then
    exit;
  connectionID := 0;
  callbackID := 0;
  fFakeCallbacks.Safe.Lock;
  try
    i := fFakeCallbacks.IndexOf(aFakeInstance);
    if i >= 0 then
    begin
      fake := fFakeCallbacks.List[i];
      if not fake.fReleasedOnClientSide then
      begin
        connectionID := fake.fLowLevelConnectionID;
        callbackID := fake.FakeID;
        if Assigned(OnCallbackReleasedOnServerSide) then
          OnCallbackReleasedOnServerSide(self, fake, fake.fFakeInterface);
      end;
      fFakeCallbacks.Delete(i);
    end;
  finally
    fFakeCallbacks.Safe.UnLock;
  end;
  if connectionID <> 0 then
    if Assigned(fRestServer.OnNotifyCallback) then
      fRestServer.OnNotifyCallback(fRestServer, SERVICE_PSEUDO_METHOD[imFree],
        '', connectionID, callbackID, nil, nil);
end;

function TServiceContainerServer.GetFakeCallbacksCount: integer;
begin
  if fFakeCallbacks <> nil then
    result := fFakeCallbacks.Count
  else
    result := 0;
end;

function FakeCallbackFind(list: PPointer; n: integer; conn: TRestConnectionID;
  id: TInterfacedObjectFakeID): TInterfacedObjectFakeServer;
begin
  if n <> 0 then
    repeat
      result := list^;
      inc(list);
      if (result.fLowLevelConnectionID = conn) and
         (result.FakeID = id) then
          exit;
      dec(n);
    until n = 0;
  result := nil;
end;

procedure TServiceContainerServer.FakeCallbackRelease(
  Ctxt: TRestServerUriContext);
var
  fake: TInterfacedObjectFakeServer;
  connectionID: TRestConnectionID;
  fakeID: TInterfacedObjectFakeID;
  Values: TNameValuePUtf8CharDynArray;
  withLog: boolean; // avoid stack overflow
begin
  if (self = nil) or
    (fFakeCallbacks = nil) or
    (fFakeCallbacks.Count = 0) or
    (Ctxt = nil) then
    exit;
  connectionID := Ctxt.Call^.LowLevelConnectionID;
  JsonDecode(pointer(Ctxt.Call^.InBody), Values);
  if length(Values) <> 1 then
    exit;
  fakeID := Values[0].Value.ToCardinal;
  if (fakeID = 0) or
     (connectionID = 0) or
     (Values[0].Name.Text = nil) then
    exit;
  withLog := not Values[0].Name.Idem('ISynLogCallback');
  if withLog then
    // avoid stack overflow ;)
    fRestServer.InternalLog('%.FakeCallbackRelease(%,"%") remote call',
      [ClassType, fakeID, Values[0].Name.Text], sllDebug);
  fFakeCallbacks.Safe.Lock;
  try
    fake := FakeCallbackFind(
      pointer(fFakeCallbacks), fFakeCallbacks.Count, connectionID, fakeID);
    if fake <> nil then
    begin
      fake.fReleasedOnClientSide := true;
      if Assigned(OnCallbackReleasedOnClientSide) then
        OnCallbackReleasedOnClientSide(self, fake, fake.fFakeInterface);
      if fake.fService.fInterface.MethodIndexCallbackReleased >= 0 then
      begin
        // emulate a call to CallbackReleased(callback,'ICallbackName')
        Ctxt.ServiceMethodIndex :=
          fake.fService.fInterface.MethodIndexCallbackReleased;
        Ctxt.ServiceMethod :=
          @fake.fService.fInterface.Methods[Ctxt.ServiceMethodIndex];
        Ctxt.ServiceExecution :=
          @fake.fService.fExecution[Ctxt.ServiceMethodIndex];
        Ctxt.ServiceExecutionOptions := Ctxt.ServiceExecution.Options;
        Ctxt.Service := fake.fService;
        inc(Ctxt.ServiceMethodIndex, Length(SERVICE_PSEUDO_METHOD));
        fake._AddRef; // IInvokable=pointer in Ctxt.ExecuteCallback
        Ctxt.ServiceParameters := pointer(FormatUtf8('[%,"%"]',
          [PtrInt(PtrUInt(fake.fFakeInterface)), Values[0].Name.Text]));
        fake.fService.ExecuteMethod(Ctxt);
        if withLog then
          fRestServer.InternalLog('I%() returned %',
            [PInterfaceMethod(Ctxt.ServiceMethod)^.InterfaceDotMethodName,
             Ctxt.Call^.OutStatus], sllDebug);
      end
      else
        Ctxt.Success;
    end;
  finally
    fFakeCallbacks.Safe.UnLock;
  end;
end;

function TServiceContainerServer.RecordVersionSynchronizeSubscribeMaster(
  TableIndex: integer; RecordVersion: TRecordVersion;
  const SlaveCallback: IServiceRecordVersionCallback): boolean;
var
  instance: TObject;
begin
  result := false;
  if (self = nil) or
    (cardinal(TableIndex) > cardinal(fRestServer.Model.TablesMax)) then
    exit;
  fRestServer.AcquireExecution[execOrmWrite].Safe.Lock;
  try
    if RecordVersion <> fRestServer.RecordVersionMax then
      // there are some missing items on the client side -> synch not possible
      exit;
    if fRecordVersionCallback = nil then
      SetLength(fRecordVersionCallback, fRestServer.Model.TablesMax + 1);
    InterfaceArrayAdd(fRecordVersionCallback[TableIndex], SlaveCallback);
    instance := ObjectFromInterface(SlaveCallback);
    if (instance <> nil) and
       (instance.ClassType = TInterfacedObjectFakeServer) then
      TInterfacedObjectFakeServer(instance).fRaiseExceptionOnInvokeError := True;
  finally
    fRestServer.AcquireExecution[execOrmWrite].Safe.UnLock;
  end;
  result := true;
end;

procedure TServiceContainerServer.GetFakeCallback(Ctxt: TRestServerUriContext;
  ParamInterfaceInfo: PRttiInfo; FakeID: PtrInt; out Obj);
var
  factory: TInterfaceFactory;
  instance: TInterfacedObjectFakeServer;
begin
  factory := TInterfaceFactory.Get(ParamInterfaceInfo);
  instance := TInterfacedObjectFakeServer.Create(Ctxt, factory, FakeID);
  pointer(Obj) := instance.fFakeInterface;
  FakeCallbackAdd(instance);
end;

procedure AppendWithSpace(var dest: shortstring; const source: shortstring);
var
  d, s: PtrInt;
begin
  d := ord(dest[0]);
  s := ord(source[0]);
  if d + s < 254 then
  begin
    dest[d + 1] := ' ';
    MoveFast(source[1], dest[d + 2], s);
    inc(dest[0], s + 1);
  end;
end;

class function TServiceContainerServer.CallbackReleasedOnClientSide(
  const callback: IInterface; callbacktext: PShortString): boolean;
var
  instance: TObject;
begin
  instance := ObjectFromInterface(callback);
  if instance = nil then
    result := false
  else
  begin
    if callbacktext <> nil then
      AppendWithSpace(callbacktext^, ClassNameShort(instance)^);
    result := (instance.ClassType = TInterfacedObjectFakeServer) and
              TInterfacedObjectFakeServer(instance).fReleasedOnClientSide;
  end;
end;

function FakeCallbackReplaceID(list: PPointer; n: integer;
  old, new: TRestConnectionID): integer;
var
  fake: TInterfacedObjectFakeServer;
begin
  result := 0;
  if n <> 0 then
    repeat
      fake := list^;
      inc(list);
      if fake.fLowLevelConnectionID = old then
      begin
        fake.fLowLevelConnectionID := new;
        inc(result);
      end;
      dec(n);
    until n = 0;
end;

function TServiceContainerServer.FakeCallbackReplaceConnectionID(
  aConnectionIDOld, aConnectionIDNew: TRestConnectionID): integer;
begin
  result := 0;
  if (fFakeCallbacks = nil) or
     (aConnectionIDOld <= 0) or
     (aConnectionIDNew <= 0) or
     (aConnectionIDOld = aConnectionIDNew) then
    exit;
  fFakeCallbacks.Safe.Lock;
  try
    result := FakeCallbackReplaceID(pointer(fFakeCallbacks.List),
      fFakeCallbacks.Count, aConnectionIDOld, aConnectionIDNew);
  finally
    fFakeCallbacks.Safe.UnLock;
  end;
end;

procedure TServiceContainerServer.RecordVersionCallbackNotify(
  TableIndex: integer; Occasion: TOrmOccasion; const DeletedID: TID;
  const DeletedRevision: TRecordVersion; const AddUpdateJson: RawUtf8);
var
  i: PtrInt;
  arr: ^IServiceRecordVersionCallbackDynArray;
begin
  try
    fRestServer.AcquireExecution[execOrmWrite].Safe.Lock;
    try
      arr := @fRecordVersionCallback[TableIndex];
      for i := length(arr^) - 1 downto 0 do
        // downto: due to InterfaceArrayDelete() below
        if CallbackReleasedOnClientSide(arr^[i]) then
          // automatic removal of any released callback
          InterfaceArrayDelete(arr^, i)
        else
        try
          case Occasion of
            ooInsert:
              arr^[i].Added(AddUpdateJson);
            ooUpdate:
              arr^[i].Updated(AddUpdateJson);
            ooDelete:
              arr^[i].Deleted(DeletedID, DeletedRevision);
          end;
        except
          // on notification error -> delete this entry
          InterfaceArrayDelete(arr^, i);
        end;
    finally
      fRestServer.AcquireExecution[execOrmWrite].Safe.UnLock;
    end;
  except
    // ignore any exception here
  end;
end;

procedure TServiceContainerServer.RecordVersionNotifyAddUpdate(
  Occasion: TOrmOccasion; TableIndex: integer; const Document: TDocVariantData);
var
  json: RawUtf8;
begin
  if (Occasion in [ooInsert, ooUpdate]) and
     (fRecordVersionCallback <> nil) and
     (fRecordVersionCallback[TableIndex] <> nil) then
  begin
    json := Document.ToJson;
    RecordVersionCallbackNotify(TableIndex, Occasion, 0, 0, json);
  end;
end;

procedure TServiceContainerServer.RecordVersionNotifyAddUpdate(
  Occasion: TOrmOccasion; TableIndex: integer; const Decoder: TJsonObjectDecoder);
var
  json: RawUtf8;
begin
  if (Occasion in [ooInsert, ooUpdate]) and
     (fRecordVersionCallback <> nil) and
     (fRecordVersionCallback[TableIndex] <> nil) then
  begin
    Decoder.EncodeAsJson(json);
    RecordVersionCallbackNotify(TableIndex, Occasion, 0, 0, json);
  end;
end;

procedure TServiceContainerServer.RecordVersionNotifyDelete(
  TableIndex: integer; const ID: TID; const Revision: TRecordVersion);
begin
  if (fRecordVersionCallback <> nil) and
     (fRecordVersionCallback[TableIndex] <> nil) then
    RecordVersionCallbackNotify(TableIndex, ooDelete, ID, Revision, '');
end;

procedure TServiceContainerServer.SetServiceLog(const aLogRest: IRestOrm;
  aLogClass: TOrmServiceLogClass; const aExcludedMethodNamesCsv: RawUtf8);
var
  i, n: PtrInt;
  fact: TServiceFactory;
  excluded: TServiceContainerInterfaceMethodBits;
  methods: TInterfaceFactoryMethodBits;
  somemethods: boolean;
begin
  somemethods := aExcludedMethodNamesCsv <> '';
  if somemethods then
    SetInterfaceMethodBits(pointer(aExcludedMethodNamesCsv), true, excluded)
  else
    FillcharFast(methods, SizeOf(methods), 255);
  n := length(fInterfaceMethod);
  i := 0;
  while i < n do
  begin
    fact := fInterfaceMethod[i].InterfaceService;
    if somemethods then
    begin
      FillcharFast(methods, SizeOf(methods), 0);
      somemethods := false;
    end;
    repeat
      if (aExcludedMethodNamesCsv <> '') and
         not (byte(i) in {%H-}excluded) then
      begin
        include(methods, fInterfaceMethod[i].
          InterfaceMethodIndex - Length(SERVICE_PSEUDO_METHOD));
        somemethods := true;
      end;
      inc(i);
    until (i >= n) or
          (fInterfaceMethod[i].InterfaceService <> fact);
    if (aExcludedMethodNamesCsv = '') or
       somemethods then
      TServiceFactoryServer(fact).SetServiceLogByIndex(
        methods, aLogRest, aLogClass);
  end;
end;


{ ***************** Asynchronous REST Synchronisation Classes }

{ TServiceRecordVersion }

function TServiceRecordVersion.Subscribe(const SqlTableName: RawUtf8;
  const revision: TRecordVersion;
  const callback: IServiceRecordVersionCallback): boolean;
var
  table: TOrmClass;
begin
  result := false;
  if Server <> nil then
  begin
    table := Server.Model.Table[SqlTableName];
    if table <> nil then
      result := Server.RecordVersionSynchronizeSubscribeMaster(
        table, revision, callback);
  end;
end;


{ TServiceRecordVersionCallback }

constructor TServiceRecordVersionCallback.Create(aSlave: TRestServer;
  aMaster: TRestClientUri; aTable: TOrmClass; const aOnNotify: TOnBatchWrite);
begin
  if aSlave = nil then
    raise EServiceException.CreateUtf8('%.Create(%): Slave=nil',
      [self, aTable]);
  fSlave := aSlave;
  fRecordVersionField := aTable.OrmProps.RecordVersionField;
  if fRecordVersionField = nil then
    raise EServiceException.CreateUtf8('%.Create: % has no TRecordVersion field',
      [self, aTable]);
  fTableDeletedIDOffset := Int64(fSlave.Model.GetTableIndexExisting(aTable))
    shl ORMVERSION_DELETEID_SHIFT;
  inherited Create(aMaster, IServiceRecordVersionCallback);
  fTable := aTable;
  fOnNotify := aOnNotify;
end;

procedure TServiceRecordVersionCallback.SetCurrentRevision(
  const Revision: TRecordVersion; Event: TOrmOccasion);
var
  current: TRecordVersion;
begin
  current := fSlave.RecordVersionMax;
  if (Revision < current) or
     ((Revision = current) and
      (Event <> ooInsert)) then
    raise EServiceException.CreateUtf8(
      '%.SetCurrentRevision(%) on %: previous was %',
      [self, Revision, fTable, current]);
  fSlave.RecordVersionMax := Revision;
end;

procedure TServiceRecordVersionCallback.Added(const NewContent: RawJson);
var
  rec: TOrm;
  fields: TFieldBits;
begin
  rec := fTable.Create;
  try
    rec.FillFrom(NewContent, @fields);
    if fBatch = nil then
      fSlave.ORM.Add(rec, true, true, true)
    else
      fBatch.Add(rec, true, true, fields, true);
    SetCurrentRevision(fRecordVersionField.PropInfo.GetInt64Prop(rec), ooInsert);
    if Assigned(fOnNotify) then
      fOnNotify(fBatch, ooInsert, fTable, rec.IDValue, rec, fields);
  finally
    rec.Free;
  end;
end;

procedure TServiceRecordVersionCallback.Updated(const ModifiedContent: RawJson);
var
  rec: TOrm;
  fields: TFieldBits;
begin
  rec := fTable.Create;
  try
    rec.FillFrom(ModifiedContent, @fields);
    if fBatch = nil then
      fSlave.ORM.Update(rec, fields, true)
    else
      fBatch.Update(rec, fields, true);
    SetCurrentRevision(fRecordVersionField.PropInfo.GetInt64Prop(rec), ooUpdate);
    if Assigned(fOnNotify) then
      fOnNotify(fBatch, ooUpdate, fTable, rec.IDValue, rec, fields);
  finally
    rec.Free;
  end;
end;

procedure TServiceRecordVersionCallback.Deleted(const ID: TID;
  const Revision: TRecordVersion);
var
  del: TOrmTableDeleted;
begin
  del := TOrmTableDeleted.Create;
  try
    del.IDValue := fTableDeletedIDOffset + Revision;
    del.Deleted := ID;
    if fBatch = nil then
    try
      fSlave.AcquireExecution[execOrmWrite].Safe.Lock;
      TRestOrmServer(fSlave.OrmInstance).RecordVersionDeleteIgnore := true;
      fSlave.ORM.Add(del, true, true, true);
      fSlave.ORM.Delete(fTable, ID);
    finally
      TRestOrmServer(fSlave.OrmInstance).RecordVersionDeleteIgnore := false;
      fSlave.AcquireExecution[execOrmWrite].Safe.UnLock;
    end
    else
    begin
      fBatch.Add(del, true, true);
      fBatch.Delete(fTable, ID);
    end;
    SetCurrentRevision(Revision, ooDelete);
    if Assigned(fOnNotify) then
      fOnNotify(fBatch, ooDelete, fTable, ID, nil, []);
  finally
    del.Free;
  end;
end;

procedure TServiceRecordVersionCallback.CurrentFrame(isLast: boolean);

  procedure Error(const msg: RawUtf8);
  begin
    fRest.InternalLog('%.CurrentFrame(%) on %: %',
      [self, isLast, fTable, msg], sllError);
  end;

begin
  if isLast then
  begin
    if fBatch = nil then
      Error('unexpected last frame');
  end
  else if fBatch <> nil then
    Error('previous active BATCH -> send pending');
  if fBatch <> nil then
  try
    fSlave.AcquireExecution[execOrmWrite].Safe.Lock;
    TRestOrmServer(fSlave.OrmInstance).RecordVersionDeleteIgnore := true;
    fSlave.ORM.BatchSend(fBatch);
  finally
    TRestOrmServer(fSlave.OrmInstance).RecordVersionDeleteIgnore := false;
    fSlave.AcquireExecution[execOrmWrite].Safe.UnLock;
    FreeAndNil(fBatch);
  end;
  if not isLast then
    fBatch := TRestBatch.Create(fSlave.OrmInstance, nil, 10000);
end;

destructor TServiceRecordVersionCallback.Destroy;
var
  timeOut: Int64;
begin
  try
    if fBatch <> nil then
    begin
      timeOut := GetTickCount64 + 2000;
      repeat
        SleepHiRes(1); // allow 2 seconds to process all pending frames
        if fBatch = nil then
          exit;
      until GetTickCount64 > timeOut;
      fSlave.InternalLog('%.Destroy on %: active BATCH', [self, fTable], sllError);
      fSlave.ORM.BatchSend(fBatch);
      fBatch.Free;
    end;
  finally
    inherited Destroy;
  end;
end;


end.

