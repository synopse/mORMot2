/// Interface-based SOA Process Types and Classes for Client-Side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.soa.client;

{
  *****************************************************************************

   Client-Side Interface-based Service Oriented Architecture (SOA) Process
    - TServiceFactoryClient Service Provider
    - TServiceContainerClientAbstract Service Provider
    - TServiceContainerClient Services Holder
    
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
  mormot.core.log,
  mormot.core.data,
  mormot.core.perf,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.db.core,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.soa.core,
  mormot.rest.core;



{ ***************** TServiceContainerClientAbstract Service Provider }

type
  TServiceFactoryClient = class;

  /// class-reference type (metaclass) of a TServiceFactoryClient kind
  TServiceFactoryClientClass = class of TServiceFactoryClient;

  /// services provider class with client interfaces
  // - is the parent to both TServiceContainerClient and TServiceContainerServer
  TServiceContainerClientAbstract = class(TServiceContainer)
  protected
    fServicesFactoryClients: TServiceFactoryClientClass;
  public
    /// initialize the Services list
    // - supplied TInterfaceResolver should be able to resolve IRestOrm,
    // and is typically a TRest instance
    constructor Create(aOwner: TInterfaceResolver); override;
    /// method called on the client side to register a service via its interface(s)
    // - will add a TServiceFactoryClient instance to the internal list
    // - is called e.g. by TRestClientUri.ServiceRegister or even by
    // TRestServer.ServiceRegister(aClient: TRest...) for a remote access -
    // use TServiceContainerServer.AddImplementation() instead for normal
    // server side implementation
    // - will raise an exception on error
    // - will return true if some interfaces have been added
    // - will check for the availability of the interfaces on the server side,
    // with an optional custom contract to be used instead of methods signature
    // (only for the first interface)
    function AddInterface(const aInterfaces: array of PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      aContractExpected: RawUtf8 = ''): boolean; overload;
    /// method called on the client side to register a service via one interface
    // - overloaded method returning the corresponding service factory client,
    // or nil on error
    function AddInterface(aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryClient; overload;
    /// the services factory client classes
    // - by default, will use TServiceFactoryClient
    property ServicesFactoryClients: TServiceFactoryClientClass
      read fServicesFactoryClients write fServicesFactoryClients;
  end;


{ ***************** TServiceFactoryClient Service Provider }

    /// a service provider implemented on the client side
  // - each registered interface has its own TServiceFactoryClient instance,
  // available as one TSqlServiceContainerClient item from TRest.Services property
  // - will emulate "fake" implementation class instance of a given interface
  // and call remotely the server to process the actual implementation
  TServiceFactoryClient = class(TServiceFactory)
  protected
    fClient: TRest; // = fResolver as TRestClientUri
    fForcedUri: RawUtf8;
    fParamsAsJsonObject: boolean;
    fResultAsJsonObject: boolean;
    fDelayedInstance: boolean;
    fNonBlockWithoutAnswer: boolean;
    fSendNotificationsThread: TThread;
    fSendNotificationsRest: TRest;
    fSendNotificationsLogClass: TOrmServiceNotificationsClass;
    function CreateFakeInstance: TInterfacedObject;
    function InternalInvoke(const aMethod: RawUtf8; const aParams: RawUtf8 = '';
      aResult: PRawUtf8 = nil; aErrorMsg: PRawUtf8 = nil;
      aClientDrivenID: PCardinal = nil;
      aServiceCustomAnswer: PServiceCustomAnswer = nil;
      aClient: TRest = nil): boolean; virtual;
    // match TOnFakeInstanceInvoke callback signature
    function Invoke(const aMethod: TInterfaceMethod; const aParams: RawUtf8;
      aResult, aErrorMsg: PRawUtf8; aClientDrivenID: PCardinal;
      aServiceCustomAnswer: PServiceCustomAnswer): boolean;
    procedure NotifyInstanceDestroyed(aClientDrivenID: cardinal); virtual;
  public
    /// initialize the service provider parameters
    // - it will check and retrieve all methods of the supplied interface,
    // and prepare all internal structures for its serialized execution
    // - also set the inherited TServiceInstanceImplementation property
    // - initialize fSharedInstance if aInstanceCreation is sicShared
    // - it will also ensure that the corresponding TServiceFactory.Contract
    // matches on both client and server sides, either by comparing the default
    // signature (based on methods and arguments), either by using the supplied
    // expected contract (which may be a custom version number)
    constructor Create(aRest: TRest; aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      const aContractExpected: RawUtf8 = '');
    /// finalize the service provider used instance
    // - e.g. the shared fake implementation instance
    destructor Destroy; override;
    /// retrieve an instance of this interface from the client side
    function Get(out Obj): boolean; override;
    /// retrieve the published signature of this interface
    // - TServiceFactoryClient will be able to retrieve it only if
    // TServiceContainerServer.PublishSignature is set to TRUE (which is not the
    // default setting, for security reasons) - this function is always available
    // on TServiceFactoryServer side
    function RetrieveSignature: RawUtf8; override;
    /// convert a HTTP error from mORMot's REST/SOA into an English text message
    // - will recognize the HTTP_UNAVAILABLE, HTTP_NOTIMPLEMENTED, HTTP_NOTFOUND,
    // HTTP_NOTALLOWED, HTTP_UNAUTHORIZED or HTTP_NOTACCEPTABLE errors, as
    // generated by the TRestServer side
    // - is used by TServiceFactoryClient.InternalInvoke, but may be called
    // on client side for TServiceCustomAnswer.Status <> HTTP_SUCCESS
    class function GetErrorMessage(status: integer): RawUtf8;
    /// define execution options for a given set of methods
    // - methods names should be specified as an array (e.g. ['Add','Multiply'])
    // - if no method name is given (i.e. []), option will be set for all methods
    // - only supports optNoLogInput and optNoLogOutput on the client side,
    // by design of "fake" interface remote execution
    procedure SetOptions(const aMethod: array of RawUtf8;
      aOptions: TInterfaceMethodOptions;
      aAction: TServiceMethodOptionsAction = moaReplace);
    /// persist all service calls into a database instead of calling the client
    // - expect a REST instance, which will store all methods without any
    // results (i.e. procedure without any var/out parameters) on the
    // associated TOrmServiceNotifications class
    // - once set, regular fClient.Uri() won't be called but a new aLogClass
    // entry will be stored in aRest
    // - to disable this redirection, set aRest and aLogClass to nil
    procedure StoreNotifications(aRest: TRest;
      aLogClass: TOrmServiceNotificationsClass);
    /// allow background process of method with no results, via a temporary
    // database, to be used e.g. for safe notifications transmission
    // - will call StoreNotifications() and start background notification
    // - expect a REST instance, which will store all methods without any
    // results (i.e. procedure without any var/out parameters) on the
    // associated TOrmServiceNotifications class
    // - a background thread will be used to check for pending notifications,
    // and send them to the supplied aRemote TRestClient instance, or
    // to the main TServiceFactoryClient.fClient instance
    // - if the remote client is not reachable, will retry after the specified
    // period of time, in seconds
    // - this method is not blocking, and will write the pending calls to
    // the aRest/aLogClass table, which will be retrieved asynchronously
    // by the background thread
    procedure SendNotifications(aRest: TRest;
      aLogClass: TOrmServiceNotificationsClass; aRetryPeriodSeconds: integer = 30;
      aRemote: TRest = nil);
    /// compute how many pending notifications are waiting for background process
    // initiated by SendNotifications() method
    function SendNotificationsPending: integer;
    /// wait for all pending notifications to be sent
    // - you can supply a time out period after which no wait will take place
    procedure SendNotificationsWait(aTimeOutSeconds: integer);
    /// the TRest instance used to send pending notifications
    property SendNotificationsRest: TRest
      read fSendNotificationsRest;
  published
    /// could be used to force the remote URI to access the service
    // - by default, the URI will be Root/Calculator or Root/InterfaceMangledUri
    // but you may use this property to use another value, e.g. if you are
    // accessign a non mORMot REST server (probably with aContractExpected set
    // to SERVICE_CONTRACT_NONE_EXPECTED, and running
    // Client.ServerTimestamp := TimeLogNowUtc to avoid an unsupported
    // ServerTimestampSynchronize call)
    property ForcedUri: RawUtf8
      read fForcedUri write fForcedUri;
    /// set to TRUE to send the interface's methods parameters as JSON object
    // - by default (FALSE), any method execution will send a JSON array with
    // all CONST/VAR parameters, in order
    // - TRUE will generate a JSON object instead, with the CONST/VAR parameter
    // names as field names - may be useful e.g. when working with a non
    // mORMot server, or when the mORMot server exposes a public API
    // - defined e.g. by TRestClientUri.ServiceDefineSharedApi() method
    property ParamsAsJsonObject: boolean
      read fParamsAsJsonObject write fParamsAsJsonObject;
    /// set to TRUE if the interface's methods result is expected to be a JSON object
    // without the {"result":... } nesting
    // - by default (FALSE), any method execution will return a JSON array with
    // all VAR/OUT parameters, within a {"result":...,"id":...} layout
    // - TRUE will expect a simple JSON object instead, with the VAR/OUT parameter
    // names as field names (and "Result" for any function result) - may be
    // useful e.g. when working with JavaScript clients or any public API
    // - this value can be overridden by setting ForceServiceResultAsJsonObject
    // for a given TRestServerUriContext (e.g. for server-side JavaScript work)
    // - defined e.g. by TRestClientUri.ServiceDefineSharedApi() method
    property ResultAsJsonObjectWithoutResult: boolean
      read fResultAsJsonObject write fResultAsJsonObject;
    /// delay the sicClientDriven server-side instance to the first method call
    // - by default, CreateFakeInstance will call _instance_ server pseudo-method
    // to ensure a fClientDrivenID is safely and properly initialized
    // - if you are sure that your client's interface variables will be thread-safe,
    // you may define this property to TRUE so that the "id" field as returned
    // at first method call will be used - makes sense only if a lot of short-live
    // interface instances are expected to be generated by the client
    property DelayedInstance: boolean
      read fDelayedInstance write fDelayedInstance;
    /// if methods expecting no result (i.e. plain procedure without var/out
    // parameters) should not block the client waiting for answer
    // - may be handy e.g. when consuming an event-driven asynchronous service
    // - will call CallbackNonBlockingSetHeader, currently implemented only in
    // TRestHttpClientWebsockets, with frame gathering
    property NonBlockWithoutAnswer: boolean
      read fNonBlockWithoutAnswer write fNonBlockWithoutAnswer;
  end;


{ ***************** TServiceContainerClient Services Holder }

  /// a services provider class to be used on the client side
  // - this will maintain a list of fake implementation classes, which will
  // remotely call the server to make the actual process
  TServiceContainerClient = class(TServiceContainerClientAbstract)
  protected
    fDisableAutoRegisterAsClientDriven: boolean;
  public
    /// retrieve a service provider from its type information
    // - this overridden method will register the interface, if was not yet made
    // - in this case, the interface will be registered with sicClientDriven
    // implementation method, unless DisableAutoRegisterAsClientDriven is TRUE
    function Info(aTypeInfo: PRttiInfo): TServiceFactory; overload; override;
    /// notify the other side that the given Callback event interface is released
    // - this overriden implementation will check the private fFakeCallbacks list
    function CallBackUnRegister(const Callback: IInvokable): boolean; override;
    /// allow to disable the automatic registration as sicClientDriven in Info()
    property DisableAutoRegisterAsClientDriven: boolean
      read fDisableAutoRegisterAsClientDriven write fDisableAutoRegisterAsClientDriven;
  end;


implementation

 uses
  mormot.rest.client;

{ ***************** TServiceContainerClientAbstract Service Provider }

constructor TServiceContainerClientAbstract.Create(aOwner: TInterfaceResolver);
begin
  inherited Create(aOwner);
  fServicesFactoryClients := TServiceFactoryClient; // default client class
end;

function TServiceContainerClientAbstract.AddInterface(
  const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  aContractExpected: RawUtf8): boolean;
var
  i: PtrInt;
  F: TServiceFactoryClient;
begin
  result := false;
  if (self = nil) or
     (high(aInterfaces) < 0) then
    exit;
  CheckInterface(aInterfaces);
  for i := 0 to high(aInterfaces) do
  begin
    F := fServicesFactoryClients.Create(
      fOwner as TRest, aInterfaces[i], aInstanceCreation, aContractExpected);
    AddServiceInternal(F);
    aContractExpected := ''; // supplied contract is only for the 1st interface
  end;
  result := true;
end;

function TServiceContainerClientAbstract.AddInterface(aInterface: PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): TServiceFactoryClient;
begin
  CheckInterface([aInterface]);
  result := fServicesFactoryClients.Create(fOwner as TRest, aInterface,
    aInstanceCreation, aContractExpected);
  AddServiceInternal(result);
end;



{ ***************** TServiceFactoryClient Service Provider }

{ TServiceFactoryClientNotificationThread }

type
  TServiceFactoryClientNotificationThread = class(TRestThread)
  protected
    fClient: TServiceFactoryClient;
    fRemote: TRest;
    fRetryPeriodSeconds: integer;
    procedure InternalExecute; override;
    procedure ProcessPendingNotification;
    function GetPendingCountFromDB: Int64;
  public
    constructor Create(aClient: TServiceFactoryClient; aRemote: TRest;
      aRetryPeriodSeconds: integer); reintroduce;
  end;

constructor TServiceFactoryClientNotificationThread.Create(
  aClient: TServiceFactoryClient; aRemote: TRest; aRetryPeriodSeconds: integer);
begin
  fClient := aClient; // cross-platform may run Execute as soon as Create is called
  if (fClient = nil) or
     (fClient.fSendNotificationsRest = nil) or
     (fClient.fSendNotificationsLogClass = nil) then
    raise EServiceException.CreateUtf8(
      '%.Create(fClient.fSendNotifications=nil)', [self]);
  if aRetryPeriodSeconds <= 0 then
    fRetryPeriodSeconds := 1
  else
    fRetryPeriodSeconds := aRetryPeriodSeconds;
  if aRemote = nil then
    fRemote := fClient.Resolver as TRest
  else
    fRemote := aRemote;
  inherited Create(fClient.Resolver as TRest, false, false);
end;

function TServiceFactoryClientNotificationThread.GetPendingCountFromDB: Int64;
begin
  if not fClient.fSendNotificationsRest.ORM.OneFieldValue(
      fClient.fSendNotificationsLogClass, 'count(*)', 'Sent is null', [], [], result) then
    result := 0;
end;

procedure TServiceFactoryClientNotificationThread.ProcessPendingNotification;
var
  pending: TOrmServiceNotifications;
  params, error: RawUtf8;
  client: cardinal;
  pendings, count: integer;
  timer: TPrecisionTimer;
  output: TDocVariantData;
begin
  // one at a time, since InternalInvoke() is the bottleneck
  pending := fClient.fSendNotificationsLogClass.Create(
    fClient.SendNotificationsRest.ORM, 'Sent is null order by id limit 1');
  try
    if pending.IDValue = 0 then
    begin
      pendings := GetPendingCountFromDB;
      fSafe.LockedInt64[0] := pendings;
      if pendings = 0 then
        exit
      else
        raise EServiceException.CreateUtf8(
          '%.ProcessPendingNotification pending=% with no DB row',
          [self, pendings]);
    end;
    pendings := fSafe.LockedInt64[0];
    timer.Start;
    VariantSaveJson(pending.Input, twJsonEscape, params);
    if (params <> '') and
       (params[1] = '[') then
      // trim [..] for URI call
      params := copy(params, 2, length(params) - 2);
    client := pending.Session;
    if not fClient.InternalInvoke(
      pending.Method, params, nil, @error, @client, nil, fRemote) then
    begin
      if _Safe(pending.Output)^.GetAsInteger('errorcount', count) then
        inc(count)
      else
        count := 1;
      output.InitObject([
        'errorcount', count,
        'lasterror', error,
        'lasttime', NowUtcToString(true, 'T'),
        'lastelapsed', timer.Stop], JSON_FAST_EXTENDED);
      pending.Output := variant(output);
      fClient.fSendNotificationsRest.ORM.Update(pending, 'Output', true);
      raise EServiceException.CreateUtf8(
        '%.ProcessPendingNotification failed for %(%) [ID=%,pending=%] on %: %',
        [self, pending.Method, params, pending.IDValue, pendings, fRemote, error]);
    end;
    fClient.fClient.InternalLog(
      'ProcessPendingNotification %(%) in % [ID=%,pending=%]',
      [pending.Method, params, timer.Stop, pending.IDValue, pendings]);
    pending.Sent := TimeLogNowUtc;
    pending.MicroSec := timer.LastTimeInMicroSec;
    fClient.fSendNotificationsRest.ORM.Update(pending, 'MicroSec,Sent', true);
    fSafe.LockedInt64Increment(0, -1);
  finally
    pending.Free;
  end;
end;

procedure TServiceFactoryClientNotificationThread.InternalExecute;
var
  delay: integer;
begin
  fSafe.LockedInt64[0] := GetPendingCountFromDB;
  delay := 50;
  while not Terminated do
  begin
    while fSafe.LockedInt64[0] > 0 do
    try
      ProcessPendingNotification;
      delay := 0;
      if Terminated then
        exit;
    except
      SleepOrTerminated(fRetryPeriodSeconds * 1000); // wait before retry
    end;
    if Terminated then
      exit;
    if delay < 50 then
      inc(delay);
    SleepHiRes(delay);
  end;
end;


{ TInterfacedObjectFakeClient }

type
  TInterfacedObjectFakeClient = class(TInterfacedObjectFake)
  protected
    fClient: TServiceFactoryClient;
    procedure InterfaceWrite(W: TTextWriter; const aMethod: TInterfaceMethod;
      const aParamInfo: TInterfaceMethodArgument; aParamValue: Pointer); override;
  public
    constructor Create(aClient: TServiceFactoryClient;
      const aInvoke: TOnFakeInstanceInvoke;
      const aNotifyDestroy: TOnFakeInstanceDestroy);
    destructor Destroy; override;
  end;

constructor TInterfacedObjectFakeClient.Create(aClient: TServiceFactoryClient;
  const aInvoke: TOnFakeInstanceInvoke;
  const aNotifyDestroy: TOnFakeInstanceDestroy);
var
  opt: TInterfacedObjectFakeOptions;
begin
  fClient := aClient;
  opt := [];
  if fClient.fClient <> nil then
    with fClient.fClient as TRestClientUri do
      if (Session.ID <> 0) and
         (Session.User <> nil) then
        opt := [ifoJsonAsExtended, ifoDontStoreVoidJson];
  inherited Create(aClient.fInterface, aClient, opt, aInvoke, aNotifyDestroy);
end;

procedure TInterfacedObjectFakeClient.InterfaceWrite(W: TTextWriter;
  const aMethod: TInterfaceMethod; const aParamInfo: TInterfaceMethodArgument;
  aParamValue: Pointer);
begin
  W.Add(TRestClientUri(fClient.fClient).FakeCallbackRegister(
    fClient, aMethod, aParamInfo, aParamValue));
  W.AddComma;
end;

destructor TInterfacedObjectFakeClient.Destroy;
begin
  fClient.fClient.InternalLog('%(%).Destroy I%',
    [ClassType, pointer(self), fClient.InterfaceUri]);
  inherited Destroy;
end;


{ TServiceFactoryClient }

function TServiceFactoryClient.CreateFakeInstance: TInterfacedObject;
var
  notify: TOnFakeInstanceDestroy;
  id: RawUtf8;
begin
  if fInstanceCreation = sicClientDriven then
    notify := NotifyInstanceDestroyed
  else
    notify := nil;
  result := TInterfacedObjectFakeClient.Create(self, Invoke, notify);
  if not fDelayedInstance and
     (fInstanceCreation = sicClientDriven) and
    InternalInvoke(SERVICE_PSEUDO_METHOD[imInstance], '', @id) then
    // thread-safe initialization of the fClientDrivenID
    TInterfacedObjectFakeClient(result).fClientDrivenID := GetCardinal(pointer(id));
end;

function TServiceFactoryClient.Invoke(const aMethod: TInterfaceMethod;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aClientDrivenID: PCardinal; aServiceCustomAnswer: PServiceCustomAnswer): boolean;

  procedure SendNotificationsLog;
  var
    pending: TOrmServiceNotifications;
    input: TDocVariantData;
    json: RawUtf8;
  begin
    pending := fSendNotificationsLogClass.Create;
    try
      pending.Method := aMethod.Uri;
      json := '[' + aParams + ']';
      input.InitJsonInPlace(pointer(json), JSON_FAST_EXTENDED);
      pending.Input := variant(input);
      if (aClientDrivenID <> nil) and
         (aClientDrivenID^ <> 0) then
      begin
        pending.Session := aClientDrivenID^;
        fSendNotificationsRest.ORM.Add(pending, 'Method,Input,Session');
      end
      else
        fSendNotificationsRest.ORM.Add(pending, 'Method,Input');
    finally
      pending.Free;
    end;
  end;

begin
  if (fSendNotificationsRest <> nil) and
     (aMethod.ArgsOutputValuesCount = 0) then
  begin
    SendNotificationsLog;
    if fSendNotificationsThread <> nil then
      TServiceFactoryClientNotificationThread(fSendNotificationsThread).
        Safe.LockedInt64Increment(0, 1);
    result := true;
  end
  else
    result := InternalInvoke(aMethod.Uri, aParams, aResult, aErrorMsg,
      aClientDrivenID, aServiceCustomAnswer);
end;

class function TServiceFactoryClient.GetErrorMessage(status: integer): RawUtf8;
begin
  case status of
    HTTP_UNAVAILABLE:
      result := 'Check the communication parameters and network config';
    HTTP_NOTIMPLEMENTED:
      result := 'Server not reachable or broken connection';
    HTTP_NOTALLOWED:
      result := 'Method forbidden for this User group';
    HTTP_UNAUTHORIZED:
      result := 'No active session';
    HTTP_FORBIDDEN:
      result := 'Security error';
    HTTP_NOTACCEPTABLE:
      result := 'Invalid input parameters';
    HTTP_NOTFOUND:
      result := 'Network problem or request timeout';
  else
    result := '';
  end;
end;

function TServiceFactoryClient.InternalInvoke(const aMethod: RawUtf8;
  const aParams: RawUtf8; aResult, aErrorMsg: PRawUtf8;
  aClientDrivenID: PCardinal; aServiceCustomAnswer: PServiceCustomAnswer;
  aClient: TRest): boolean;
var
  baseuri, uri, sent, resp, clientDrivenID, head, error, ct: RawUtf8;
  Values: array[0..1] of TValuePUtf8Char;
  status, m: integer;
  service: PInterfaceMethod;
  ctxt: TRestClientSideInvoke;
  withinput: boolean;
  rcu: TRestClientUri absolute aClient;
  log: ISynLog; // for Enter auto-leave to work with FPC / Delphi 10.4+
  p: RawUtf8;

  procedure DoClientCall;
  begin
    uri := baseuri;
    rcu.ServicesRouting.ClientSideInvoke(
      uri, ctxt, aMethod, aParams, clientDrivenID, sent, head);
    if service <> nil then
    begin
      // ParamsAsJsonObject won't apply to _signature_ e.g.
      if fParamsAsJsonObject and
         (clientDrivenID = '') then
        sent := service^.ArgsArrayToObject(Pointer(sent), true);
      if fNonBlockWithoutAnswer and
         (head = '') and
         (service^.ArgsOutputValuesCount = 0) then
        rcu.CallbackNonBlockingSetHeader(head);
    end;
    status := rcu.Uri(uri, 'POST', @resp, @head, @sent);
  end;

begin
  result := false;
  if self = nil then
    exit;
  if fClient = nil then
    fClient := fResolver as TRest;
  if aClient = nil then
    aClient := fClient;
  if (aClientDrivenID <> nil) and
     (aClientDrivenID^ > 0) then
    UInt32ToUtf8(aClientDrivenID^, clientDrivenID);
  m := fInterface.FindMethodIndex(aMethod);
  if m < 0 then
    service := nil
  else
    service := @fInterface.Methods[m];
  withinput := ((service = nil) or
                ([imdConst, imdVar] * service^.HasSpiParams = [])) and
               not (optNoLogInput in fExecution[m].Options);
  if withinput then
    // include non-sensitive input in log
    p := aParams;
  log := fClient.LogClass.Enter('InternalInvoke I%.%(%) %',
    [fInterfaceUri, aMethod, {%H-}p, clientDrivenID], self);
  // call remote server according to current routing scheme
  if fForcedUri <> '' then
    baseuri := fForcedUri
  else if TRestClientUri(fClient).Services.ExpectMangledUri then
    baseuri := aClient.Model.Root + '/' + fInterfaceMangledUri
  else
    baseuri := aClient.Model.Root + '/' + fInterfaceUri;
  ctxt := [];
  if (service <> nil) and
     not ParamsAsJsonObject and
     service^.ArgsInputIsOctetStream then
    include(ctxt, csiAsOctetStream);
  status := 0;
  DoClientCall;
  if (status = HTTP_UNAUTHORIZED) and
     (clientDrivenID <> '') and
     (fInstanceCreation = sicClientDriven) and
     (aClientDrivenID <> nil) then
  begin
    if log <> nil then
      log.Log(sllClient, '% -> try to recreate ClientDrivenID', [{%H-}resp], self);
    clientDrivenID := '';
    aClientDrivenID^ := 0;
    DoClientCall;
  end;
  // decode result
  if aServiceCustomAnswer = nil then
  begin
    // handle errors at REST level
    if not StatusCodeIsSuccess(status) then
    begin
      if aErrorMsg <> nil then
      begin
        if resp = '' then
        begin
          StatusCodeToReason(status, resp);
          error := GetErrorMessage(status);
          if error <> '' then
            error := ' - ' + error;
          if not withinput then
            sent := ''; // exclude sensitive input in error text
          FormatUtf8('URI % % returned status ''%'' (%%)',
            [{%H-}uri, {%H-}sent, resp, status, error], aErrorMsg^);
        end
        else
          aErrorMsg^ := resp;
      end;
      exit; // leave result=false
    end;
    // decode JSON object
    if (log <> nil) and
       (resp <> '') and
       not (optNoLogOutput in fExecution[m].Options) and
       ((service = nil) or
        ([imdConst, imdVar] * service^.HasSpiParams = [])) then
      with fClient.LogFamily do
        if sllServiceReturn in Level then
          log.Log(sllServiceReturn, resp, self, MAX_SIZE_RESPONSE_LOG);
    if fResultAsJsonObject then
    begin
      if aResult <> nil then
        aResult^ := resp;
      if aClientDrivenID <> nil then
        aClientDrivenID^ := 0;
    end
    else if (resp <> '') and
            (aClientDrivenID = nil) and
            not IdemPChar(GotoNextNotSpace (pointer(resp)), '{"RESULT":') then
    begin
      if aResult <> nil then
        aResult^ := resp; // e.g. when client retrieves the contract
    end
    else
    begin
      if (JsonDecode(pointer(resp), ['result', // 0
                                     'id'      // 1
                                    ], @Values, true) = nil) or
         ({%H-}Values[0].Value = nil) then
      begin
        // no "result":... layout
        if aErrorMsg <> nil then
        begin
          UniqueRawUtf8ZeroToTilde(resp, 1 shl 10);
          aErrorMsg^ :=
            'Invalid returned JSON content: expects {result:...}, got ' + resp;
        end;
        exit; // leave result=false
      end;
      if aResult <> nil then
        Values[0].ToUtf8(aResult^);
      if (aClientDrivenID <> nil) and
         (Values[1].Value <> nil) then
        // keep ID if no "id":...
        aClientDrivenID^ := Values[1].ToCardinal;
    end;
  end
  else
  begin
    // custom answer returned in TServiceCustomAnswer
    if (log <> nil) and
       (resp <> '') then
      with fClient.LogFamily do
        if sllServiceReturn in Level then
        begin
          FindNameValue(head{%H-}, HEADER_CONTENT_TYPE_UPPER, ct);
          if (resp[1] in ['[', '{', '"']) and
             IdemPChar(pointer(ct), JSON_CONTENT_TYPE_UPPER) then
            log.Log(sllServiceReturn, resp, self, MAX_SIZE_RESPONSE_LOG)
          else
            log.Log(sllServiceReturn, 'TServiceCustomAnswer=% % len=% %',
              [status, ct, length(resp), EscapeToShort(resp)], self);
        end;
    aServiceCustomAnswer^.status := status;
    aServiceCustomAnswer^.Header := head;
    aServiceCustomAnswer^.Content := resp;
    // no "id" field returned, but aClientDrivenID^ should not change
  end;
  result := true;
end;

procedure TServiceFactoryClient.NotifyInstanceDestroyed(aClientDrivenID: cardinal);
begin
  if aClientDrivenID <> 0 then
    InternalInvoke(SERVICE_PSEUDO_METHOD[imFree], '', nil, nil, @aClientDrivenID);
end;

constructor TServiceFactoryClient.Create(aRest: TRest; aInterface: PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation; const aContractExpected: RawUtf8);
var
  Error, RemoteContract: RawUtf8;
begin
  // extract interface RTTI and create fake interface (and any shared instance)
  if not aRest.InheritsFrom(TRestClientUri) then
    EServiceException.CreateUtf8(
      '%.Create(): % interface needs a Client connection',
      [self, aInterface^.Name]);
  if fClient = nil then
    fClient := aRest;
  inherited Create(aRest, aInterface, aInstanceCreation, aContractExpected);
  // initialize a shared instance (if needed)
  case fInstanceCreation of
    sicShared, sicPerSession, sicPerUser, sicPerGroup, sicPerThread:
      begin
        // the instance shall remain active during the whole client session
        fSharedInstance := CreateFakeInstance;
        IInterface(fSharedInstance)._AddRef; // force stay alive
      end;
  end;
  // check if this interface is supported on the server
  if PosEx(SERVICE_CONTRACT_NONE_EXPECTED, ContractExpected) = 0 then
  begin
    if not InternalInvoke(SERVICE_PSEUDO_METHOD[imContract],
       TRestClientUri(fClient).ServicePublishOwnInterfaces, @RemoteContract, @Error) then
      raise EServiceException.CreateUtf8('%.Create(): I% interface or % routing not ' +
        'supported by server [%]', [self, fInterfaceUri,
         TRestClientUri(fClient).ServicesRouting, Error]);
    if ('[' + ContractExpected + ']' <> RemoteContract) and
       ('{"contract":' + ContractExpected + '}' <> RemoteContract) then
      raise EServiceException.CreateUtf8('%.Create(): server''s I% contract ' +
        'differs from client''s: expected [%], received % - you may need to ' +
        'upgrade your % client to match % server expectations',
        [self, fInterfaceUri, ContractExpected, RemoteContract,
         Executable.Version.DetailedOrVoid, TRestClientUri(fClient).Session.Version]);
  end;
end;

destructor TServiceFactoryClient.Destroy;
begin
  FreeAndNil(fSendNotificationsThread);
  if fSharedInstance <> nil then
    if fSharedInstance.RefCount <> 1 then
      raise EServiceException.CreateUtf8(
        '%.Destroy with RefCount=%: you must release ' +
        'I% interface (setting := nil) before Client.Free',
        [self, fSharedInstance.RefCount, fInterfaceUri])
    else
      IInterface(fSharedInstance)._Release; // bonne nuit les petits
  inherited;
end;

function TServiceFactoryClient.RetrieveSignature: RawUtf8;
begin
  result := '';
  if InternalInvoke(SERVICE_PSEUDO_METHOD[imSignature], '', @result) and
     (result <> '') then
    if result[1] = '[' then
      result := copy(result, 2, length(result) - 2)
    else if IdemPChar(pointer(result), '{"SIGNATURE":') then
      result := copy(result, 14, length(result) - 14);
end;

function TServiceFactoryClient.Get(out Obj): boolean;
var
  O: TInterfacedObjectFake;
begin
  result := false;
  if self = nil then
    exit;
  case fInstanceCreation of
    sicShared, sicPerSession, sicPerUser, sicPerGroup, sicPerThread:
      O := TInterfacedObjectFake(fSharedInstance);
    sicSingle, sicClientDriven:
      O := TInterfacedObjectFake(CreateFakeInstance);
  else
    exit;
  end;
  if O <> nil then
    O.Get(Obj);
  result := true;
end;

procedure TServiceFactoryClient.StoreNotifications(aRest: TRest;
  aLogClass: TOrmServiceNotificationsClass);
var
  c: TClass;
begin
  if (aRest = fSendNotificationsRest) and
     (aLogClass = fSendNotificationsLogClass) then
    exit;
  fSendNotificationsRest := aRest;
  fSendNotificationsLogClass := aLogClass;
  if aRest = nil then
    c := nil
  else
    c := aRest.ClassType;
  fClient.InternalLog('%.StoreNotifications(%,%) for I%',
    [ClassType, c, aLogClass, fInterfaceUri]);
end;

procedure TServiceFactoryClient.SendNotifications(aRest: TRest;
  aLogClass: TOrmServiceNotificationsClass; aRetryPeriodSeconds: integer;
  aRemote: TRest);
begin
  if (self = nil) or
     (aRest = nil) or
     (aLogClass = nil) then
    raise EServiceException.CreateUtf8(
      '%.SendNotifications invalid call', [self]);
  if fSendNotificationsThread <> nil then
    if (aRest = fSendNotificationsRest) and
       (aLogClass = fSendNotificationsLogClass) then
    begin
      fClient.InternalLog('%.SendNotifications(%,%) I% twice -> ignored',
        [ClassType, aRest.ClassType, aLogClass, fInterfaceUri], sllInfo);
      exit;
    end
    else
      raise EServiceException.CreateUtf8('%.SendNotifications twice', [self]);
  StoreNotifications(aRest, aLogClass);
  fSendNotificationsThread := TServiceFactoryClientNotificationThread.Create(
    self, aRemote, aRetryPeriodSeconds);
end;

function TServiceFactoryClient.SendNotificationsPending: integer;
begin
  if (self = nil) or
     (fSendNotificationsThread = nil) then
    result := 0
  else
    result := TServiceFactoryClientNotificationThread(fSendNotificationsThread).
      GetPendingCountFromDB;
end;

procedure TServiceFactoryClient.SendNotificationsWait(aTimeOutSeconds: integer);
var
  timeOut: Int64;
begin
  if SendNotificationsPending <> 0 then
    with fClient.LogClass.Enter do
    begin
      timeOut := GetTickCount64 + aTimeOutSeconds * 1000;
      repeat
        SleepHiRes(5);
        if SendNotificationsPending = 0 then
          exit;
      until GetTickCount64 > timeOut;
    end;
end;

procedure TServiceFactoryClient.SetOptions(const aMethod: array of RawUtf8;
  aOptions: TInterfaceMethodOptions; aAction: TServiceMethodOptionsAction);
var
  o: TInterfaceMethodOption;
begin
  for o := low(o) to high(o) do
    if (o in aOptions) and
       not (o in [optNoLogInput..optErrorOnMissingParam]) then
      raise EServiceException.CreateUtf8('%.SetOptions(%) not supported',
        [self, GetEnumName(TypeInfo(TInterfaceMethodOption), ord(o))^]);
  ExecutionAction(aMethod, aOptions, aAction);
end;



{ ***************** TServiceContainerClient Services Holder }

{ TServiceContainerClient }

function TServiceContainerClient.Info(aTypeInfo: PRttiInfo): TServiceFactory;
begin
  result := inherited Info(aTypeInfo);
  if (result = nil) and
     not fDisableAutoRegisterAsClientDriven then
    result := AddInterface(aTypeInfo, sicClientDriven);
end;

function TServiceContainerClient.CallBackUnRegister(const Callback: IInvokable): boolean;
begin
  if Assigned(Callback) then
    result := (fOwner as TRestClientUri).FakeCallbacks.UnRegister(pointer(Callback))
  else
    result := false;
end;


end.

