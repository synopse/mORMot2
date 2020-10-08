/// Interface-based SOA Process Core Types and Classes
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.soa.core;

{
  *****************************************************************************

   Shared Interface-based Service Oriented Architecture (SOA) Process
    - TSQLRecordServiceLog TSQLRecordServiceNotifications Classes
    - TServiceFactory Abstract Service Provider
    - TServiceContainer Abstract Services Holder
    - SOA Related Interfaces

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  mormot.lib.z, // for contract hashing
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.interfaces,
  mormot.orm.core; // for IRestORM


{ ************ TSQLRecordServiceLog TSQLRecordServiceNotifications Classes }

type
  /// common ancestor for storing interface-based service execution statistics
  // - each call could be logged and monitored in the database
  // - TServiceMethodExecute could store all its calls in such a table
  // - enabled on server side via either TServiceFactoryServer.SetServiceLog or
  // TServiceContainerServer.SetServiceLog method
  TSQLRecordServiceLog = class(TSQLRecordNoCaseExtended)
  protected
    fMethod: RawUTF8;
    fInput: variant;
    fOutput: variant;
    fUser: integer;
    fSession: integer;
    fTime: TModTime;
    fMicroSec: integer;
    fIP: RawUTF8;
  public
    /// overriden method creating an index on the Method/MicroSec columns
    class procedure InitializeTable(const Server: IRestORMServer;
      const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
  published
    /// the 'interface.method' identifier of this call
    // - this column will be indexed, for fast SQL queries, with the MicroSec
    // column (for performance tuning)
    property Method: RawUTF8 read fMethod write fMethod;
    /// the input parameters, as a JSON document
    // - will be stored in JSON_OPTIONS_FAST_EXTENDED format, i.e. with
    // shortened field names, for smaller TEXT storage
    // - content may be searched using JsonGet/JsonHas SQL functions on a
    // SQlite3 storage, or with direct document query under MongoDB/PostgreSQL
    property Input: variant read fInput write fInput;
    /// the output parameters, as a JSON document, including result: for a function
    // - will be stored in JSON_OPTIONS_FAST_EXTENDED format, i.e. with
    // shortened field names, for smaller TEXT storage
    // - content may be searched using JsonGet/JsonHas SQL functions on a
    // SQlite3 storage, or with direct document query under MongoDB/PostgreSQL
    property Output: variant read fOutput write fOutput;
    /// the Session ID, if there is any
    property Session: integer read fSession write fSession;
    /// the User ID, if there is an identified Session
    property User: integer read fUser write fUser;
    /// will be filled by the ORM when this record is written in the database
    property Time: TModTime read fTime write fTime;
    /// execution time of this method, in micro seconds
    property MicroSec: integer read fMicroSec write fMicroSec;
    /// if not localhost/127.0.0.1, the remote IP address
    property IP: RawUTF8 read fIP write fIP;
  end;

  /// execution statistics used for DB-based asynchronous notifications
  // - as used by TServiceFactoryClient.SendNotifications
  // - here, the Output column may contain the information about an error
  // occurred during process
  TSQLRecordServiceNotifications = class(TSQLRecordServiceLog)
  protected
    fSent: TTimeLog;
  public
    /// this overriden method will create an index on the 'Sent' column
    class procedure InitializeTable(const Server: IRestORMServer;
      const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
    /// search for pending events since a supplied ID
    // - returns FALSE if no notification was found
    // - returns TRUE ad fill a TDocVariant array of JSON Objects, including
    // "ID": field, and Method as "MethodName": field
    class function LastEventsAsObjects(const Rest: IRestORM; LastKnownID: TID;
      Limit: integer; Service: TInterfaceFactory; out Dest: TDocVariantData;
      const MethodName: RawUTF8 = 'Method'; IDAsHexa: boolean = false): boolean;
    /// allows to convert the Input array into a proper single JSON Object
    // - "ID": field will be included, and Method as "MethodName": field
    function SaveInputAsObject(Service: TInterfaceFactory;
      const MethodName: RawUTF8 = 'Method'; IDAsHexa: boolean = false): variant; virtual;
    /// run FillOne and SaveInputAsObject into a TDocVariant array of JSON Objects
    // - "ID": field will be included, and Method as "MethodName": field
    procedure SaveFillInputsAsObjects(Service: TInterfaceFactory;
      out Dest: TDocVariantData; const MethodName: RawUTF8 = 'Method';
      IDAsHexa: boolean = false);
  published
    /// when this notification has been sent
    // - equals 0 until it was actually notified
    property Sent: TTimeLog read fSent write fSent;
  end;

  /// class-reference type (metaclass) for storing interface-based service
  // execution statistics
  // - you could inherit from TSQLRecordServiceLog, and specify additional
  // fields corresponding to the execution context
  TSQLRecordServiceLogClass = class of TSQLRecordServiceLog;

  /// class-reference type (metaclass) for storing interface-based service
  // execution statistics used for DB-based asynchronous notifications
  // - as used by TServiceFactoryClient.SendNotifications
  TSQLRecordServiceNotificationsClass = class of TSQLRecordServiceNotifications;


{ ************ TServiceFactory Abstract Service Provider }

type
  /// exception dedicated to interface based service implementation
  EServiceException = class(ESynException);

  /// the possible Server-side instance implementation patterns for
  // interface-based services
  // - each interface-based service will be implemented by a corresponding
  // class instance on the server: this parameter is used to define how
  // class instances are created and managed
  // - on the Client-side, each instance will be handled depending on the
  // server side implementation (i.e. with sicClientDriven behavior if necessary)
  // - sicSingle: one object instance is created per call - this is the
  // most expensive way of implementing the service, but is safe for simple
  // workflows (like a one-type call); this is the default setting for
  // TSQLRestServer.ServiceRegister method
  // - sicShared: one object instance is used for all incoming calls and is
  // not recycled subsequent to the calls - the implementation should be
  // thread-safe on the server side
  // - sicClientDriven: one object instance will be created in synchronization
  // with the client-side lifetime of the corresponding interface: when the
  // interface will be released on client, it will be released on the server
  // side - a numerical identifier will be transmitted for all JSON requests
  // - sicPerSession, sicPerUser and sicPerGroup modes will maintain one
  // object instance per running session / user / group (only working if
  // RESTful authentication is enabled) - since it may be shared among users or
  // groups, the sicPerUser and sicPerGroup implementation should be thread-safe
  // - sicPerThread will maintain one object instance per calling thread - it
  // may be useful instead of sicShared mode if the service process expects
  // some per-heavy thread initialization, for instance
  TServiceInstanceImplementation = (
    sicSingle, sicShared, sicClientDriven,
    sicPerSession, sicPerUser, sicPerGroup, sicPerThread);

  /// set of Server-side instance implementation patterns for
  // interface-based services
  TServiceInstanceImplementations = set of TServiceInstanceImplementation;

  /// how TServiceFactoryServer.SetOptions() will set the options value
  TServiceMethodOptionsAction = (
    moaReplace, moaInclude, moaExclude);

  /// internal per-method list of execution context as hold in TServiceFactory
  TServiceFactoryExecution = record
    /// the list of denied TSQLAuthGroup ID(s)
    // - used on server side within TSQLRestServerURIContext.ExecuteSOAByInterface
    // - bit 0 for client TSQLAuthGroup.ID=1 and so on...
    // - is therefore able to store IDs up to 256
    // - void by default, i.e. no denial = all groups allowed for this method
    Denied: set of 0..255;
    /// execution options for this method (about thread safety or logging)
    Options: TInterfaceMethodOptions;
    /// where execution information should be written as TSQLRecordServiceLog
    LogRest: TObject; // TSQLRest
    /// the TSQLRecordServiceLog class to use, as defined in LogRest.Model
    LogClass: TSQLRecordServiceLogClass;
  end;

  /// points to the execution context of one method within TServiceFactory
  PServiceFactoryExecution = ^TServiceFactoryExecution;

  /// an abstract service provider, as registered in TServiceContainer
  // - each registered interface has its own TServiceFactory instance, available
  // as one TSQLServiceContainer item from TSQLRest.Services property
  // - this will be either implemented by a registered TInterfacedObject on the
  // server, or by a on-the-fly generated fake TInterfacedObject class
  // communicating via JSON on a client
  // - TSQLRestServer will have to register an interface implementation as:
  // ! Server.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
  // - TSQLRestClientURI will have to register an interface remote access as:
  // ! Client.ServiceRegister([TypeInfo(ICalculator)],sicShared));
  // note that the implementation (TServiceCalculator) remain on the server side
  // only: the client only needs the ICalculator interface
  // - then TSQLRestServer and TSQLRestClientURI will both have access to the
  // service, via their Services property, e.g. as:
  // !var I: ICalculator;
  // !...
  // ! if Services.Info(ICalculator).Get(I) then
  // !   result := I.Add(10,20);
  // which is in practice to be used with the faster wrapper method:
  // ! if Services.Resolve(ICalculator,I) then
  // !   result := I.Add(10,20);
  TServiceFactory = class(TInjectableObject)
  protected
    fInterface: TInterfaceFactory;
    fInterfaceURI: RawUTF8;
    fInterfaceMangledURI: RawUTF8;
    fInstanceCreation: TServiceInstanceImplementation;
    fORM: IRestORM;
    fSharedInstance: TInterfacedObject;
    fContract: RawUTF8;
    fContractHash: RawUTF8;
    fContractExpected: RawUTF8;
    // per-method execution rights
    fExecution: array of TServiceFactoryExecution;
    /// union of all fExecution[].Options
    fAnyOptions: TInterfaceMethodOptions;
    procedure ExecutionAction(const aMethod: array of RawUTF8; aOptions:
      TInterfaceMethodOptions; aAction: TServiceMethodOptionsAction);
    function GetInterfaceTypeInfo: PRttiInfo;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInterfaceIID: TGUID;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// initialize the service provider parameters
    // - it will check and retrieve all methods of the supplied interface,
    // and prepare all internal structures for its serialized execution
    // - supplied TInterfaceResolver should be able to resolve IRestORM,
    // and is typically a TSQLRest instance
    constructor Create(aOwner: TInterfaceResolver; aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation;
      const aContractExpected: RawUTF8); reintroduce;
    /// retrieve an instance of this interface
    // - this virtual method will be overridden to reflect the expected
    // behavior of client or server side
    // - can be used as such to resolve an I: ICalculator interface:
    // ! var I: ICalculator;
    // ! begin
    // !   if fClient.Services.Info(TypeInfo(ICalculator)).Get(I) then
    // !   ... use I
    function Get(out Obj): Boolean; virtual; abstract;
    /// retrieve the published signature of this interface
    // - is always available on TServiceFactoryServer, but TServiceFactoryClient
    // will be able to retrieve it only if TServiceContainerServer.PublishSignature
    // is set to TRUE (which is not the default setting, for security reasons)
    function RetrieveSignature: RawUTF8; virtual; abstract;
    /// access to the registered Interface RTTI information
    property InterfaceFactory: TInterfaceFactory read fInterface;
    /// the registered Interface low-level Delphi RTTI type
    // - just maps InterfaceFactory.InterfaceTypeInfo
    property InterfaceTypeInfo: PRttiInfo read GetInterfaceTypeInfo;
    /// the registered Interface GUID
    // - just maps InterfaceFactory.InterfaceIID
    property InterfaceIID: TGUID read GetInterfaceIID;
    (*/ the service contract, serialized as a JSON object
    - a "contract" is in fact the used interface signature, i.e. its
      implementation mode (InstanceCreation) and all its methods definitions
    - a possible value for a one-method interface defined as such:
    ! function ICalculator.Add(n1,n2: integer): integer;
    may be returned as the following JSON object:
    $ {"contract":"Calculator","implementation":"shared",
    $  "methods":[{"method":"Add",
    $  "arguments":[{"argument":"Self","direction":"in","type":"self"},
    $               {"argument":"n1","direction":"in","type":"integer"},
    $               {"argument":"n2","direction":"in","type":"integer"},
    $               {"argument":"Result","direction":"out","type":"integer"}
    $ ]}]} *)
    property Contract: RawUTF8 read fContract;
    /// the published service contract, as expected by both client and server
    // - by default, will contain ContractHash property value (for security)
    // - but you can override this value using plain Contract or any custom
    // value (e.g. a custom version number) - in this case, both TServiceFactoryClient
    // and TServiceFactoryServer instances must have a matching ContractExpected
    // - this value is returned by a '_contract_' pseudo-method name, with the URI:
    // $ POST /root/Interface._contract_
    // or (if TSQLRestRoutingJSON_RPC is used):
    // $ POST /root/Interface
    // $ (...)
    // $ {"method":"_contract_","params":[]}
    // (e.g. to be checked in TServiceFactoryClient.Create constructor)
    // - if set to SERVICE_CONTRACT_NONE_EXPECTED (i.e. '*'), the client won't
    // check and ask the server contract for consistency: it may be used e.g.
    // for accessing a plain REST HTTP server which is not based on mORMot,
    // so may not implement POST /root/Interface._contract_
    property ContractExpected: RawUTF8 read fContractExpected write fContractExpected;
  published
    /// access to the associated TSQLRest ORM instance
    property ORM: IRestORM read fORM;
  published
    /// the registered Interface URI
    // - in fact this is the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator
    property InterfaceURI: RawUTF8 read fInterfaceURI;
    /// the registered Interface mangled URI
    // - in fact this is encoding the GUID using BinToBase64URI(), e.g.
    // ! ['{c9a646d3-9c61-4cb7-bfcd-ee2522c8f633}'] into '00amyWGct0y_ze4lIsj2Mw'
    // - can be substituted to the clear InterfaceURI name
    property InterfaceMangledURI: RawUTF8 read fInterfaceMangledURI;
    /// how each class instance is to be created
    // - only relevant on the server side; on the client side, this class will
    // be accessed only to retrieve a remote access instance, i.e. sicSingle
    property InstanceCreation: TServiceInstanceImplementation read fInstanceCreation;
    /// a hash of the service contract, serialized as a JSON string
    // - this may be used instead of the JSON signature, to enhance security
    // (i.e. if you do not want to publish the available methods, but want
    // to check for the proper synchronization of both client and server)
    // - a possible value may be: "C351335A7406374C"
    property ContractHash: RawUTF8 read fContractHash;
  end;

function ToText(si: TServiceInstanceImplementation): PShortString; overload;


{ ************ TServiceContainer Abstract Services Holder }

type
  /// used to lookup one service in a global list of interface-based services
  TServiceContainerInterface = record
    /// one 'service' item, as set at URI, e.g. 'Calculator'
    InterfaceName: RawUTF8;
    /// the associated service provider
    Service: TServiceFactory;
  end;
  /// pointer to one  lookup in a global list of interface-based services

  PServiceContainerInterface = ^TServiceContainerInterface;
  /// used to store all s in a global list of interface-based services

  TServiceContainerInterfaces = array of TServiceContainerInterface;

  /// used to lookup one method in a global list of interface-based services
  TServiceContainerInterfaceMethod = record
    /// one 'service.method' item, as set at URI
    // - e.g.'Calculator.Add','Calculator.Multiply'...
    InterfaceDotMethodName: RawUTF8;
    /// the associated service provider
    InterfaceService: TServiceFactory;
    /// the index of the method for the given service
    // - 0..2 indicates _free_/_contract_/_signature_ pseudo-methods
    // - then points to InterfaceService.Interface.Methods[InterfaceMethodIndex-3]
    InterfaceMethodIndex: integer;
  end;
  /// pointer to one method lookup in a global list of interface-based services

  PServiceContainerInterfaceMethod = ^TServiceContainerInterfaceMethod;
  /// used to store all methods in a global list of interface-based services

  TServiceContainerInterfaceMethods = array of TServiceContainerInterfaceMethod;

  /// used in TServiceContainer to identify fListInterfaceMethod[] entries
  TServiceContainerInterfaceMethodBits = set of 0..255;

  /// a global services provider class
  // - used to maintain a list of interfaces implementation
  // - inherits from TInterfaceResolverInjected and its Resolve() methods,
  // compatible with TInjectableObject
  TServiceContainer = class(TInterfaceResolverInjected)
  protected
    fOwner: TInterfaceResolver;
    // list of services ['Calculator',...]
    fInterface: TServiceContainerInterfaces;
    fInterfaces: TDynArrayHashed;
    // list of service.method ['Calculator.Add','Calculator.Multiply',...]
    fInterfaceMethod: TServiceContainerInterfaceMethods;
    fInterfaceMethods: TDynArrayHashed;
    fExpectMangledURI: boolean;
    procedure SetExpectMangledURI(aValue: Boolean);
    procedure SetInterfaceMethodBits(MethodNamesCSV: PUTF8Char;
      IncludePseudoMethods: boolean; out bits: TServiceContainerInterfaceMethodBits);
    function GetMethodName(ListInterfaceMethodIndex: integer): RawUTF8;
    procedure CheckInterface(const aInterfaces: array of PRttiInfo);
    function AddServiceInternal(aService: TServiceFactory): integer;
    function TryResolve(aInterface: PRttiInfo; out Obj): boolean; override;
    /// retrieve a service provider from its URI
    function GetService(const aURI: RawUTF8): TServiceFactory;
  public
    /// initialize the Services list
    // - supplied TInterfaceResolver should be able to resolve IRestORM,
    // and is typically a TSQLRest instance
    constructor Create(aOwner: TInterfaceResolver); virtual;
    /// release all registered services
    destructor Destroy; override;
    /// return the number of registered service interfaces
    function Count: integer; {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a service provider from its index in the list
    // - returns nil if out of range index
    function Index(aIndex: integer): TServiceFactory; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve a service provider from its GUID / Interface type
    // - you shall have registered the interface by a previous call to
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...])
    // - on match, it will return the service the corresponding interface factory
    // - returns nil if the GUID does not match any registered interface
    // - can be used as such to resolve an I: ICalculator interface
    // ! if fClient.Services.Info(ICalculator).Get(I) then
    // !   ... use I
    {$ifdef FPC_HAS_CONSTREF}
    function Info(constref aGUID: TGUID): TServiceFactory; overload;
    {$else}
    function Info(const aGUID: TGUID): TServiceFactory; overload;
    {$endif FPC_HAS_CONSTREF}
    /// retrieve a service provider from its type information
    // - on match, it  will return the service the corresponding interface factory
    // - returns nil if the type information does not match any registered interface
    // - can be used as such to resolve an I: ICalculator interface
    // ! if fClient.Services.Info(TypeInfo(ICalculator)).Get(I) then
    // !   ... use I
    // - is defined as virtual so that e.g. TServiceContainerClient will
    // automatically register the interface, if it was not already done
    function Info(aTypeInfo: PRttiInfo): TServiceFactory; overload; virtual;
    /// notify the other side that the given Callback event interface is released
    // - this default implementation will do nothing
    function CallBackUnRegister(const Callback: IInvokable): boolean; virtual;
    /// retrieve all registered Services TGUID
    procedure SetGUIDs(out Services: TGUIDDynArray);
    /// retrieve all registered Services names
    // - i.e. all interface names without the initial 'I', e.g. 'Calculator' for
    // ICalculator
    procedure SetInterfaceNames(out Names: TRawUTF8DynArray);
    /// retrieve all registered Services contracts as a JSON array
    // - i.e. a JSON array of TServiceFactory.Contract JSON objects
    function AsJson: RawJSON;
    /// retrieve a service provider from its URI
    // - it expects the supplied URI variable  to be e.g. '00amyWGct0y_ze4lIsj2Mw'
    // or 'Calculator', depending on the ExpectMangledURI property
    // - on match, it  will return the service the corresponding interface factory
    // - returns nil if the URI does not match any registered interface
    property Services[const aURI: RawUTF8]: TServiceFactory read GetService; default;
    /// the associated TSQLRest instance, owning these services
    property Owner: TInterfaceResolver read fOwner;
    /// set if the URI is expected to be mangled from the GUID
    // - by default (FALSE), the clear service name is expected to be supplied at
    // the URI level (e.g. 'Calculator')
    // - if this property is set to TRUE, the mangled URI value will be expected
    // instead (may enhance security) - e.g. '00amyWGct0y_ze4lIsj2Mw'
    property ExpectMangledURI: boolean read fExpectMangledURI write SetExpectMangledURI;
  end;


{ ************ SOA Related Interfaces }

type
  /// prototype of a class implementing redirection of a given interface
  // - as returned e.g. by TSQLRest.MultiRedirect method
  // - can be used as a main callback, then call Redirect() to manage
  // an internal list of redirections
  // - when you release this instance, will call Rest.Service.CallbackUnregister
  // with the associated fake callback generated
  IMultiCallbackRedirect = interface
    ['{E803A30A-8C06-4BB9-94E6-EB87EACFE980}']
    /// add or remove an interface callback to the internal redirection list
    // - will register a callback if aSubscribe is true
    // - will unregister a callback if aSubscribe is false
    // - supplied aCallback shoud implement the expected interface GUID
    // - this method will be implemented as thread-safe
    // - you can specify some method names, or all methods redirection if []
    procedure Redirect(const aCallback: IInvokable;
      const aMethodsNames: array of RawUTF8; aSubscribe: boolean = true); overload;
    /// add or remove a class instance callback to the internal redirection list
    // - will register a callback if aSubscribe is true
    // - will unregister a callback if aSubscribe is false
    // - supplied aCallback instance should implement the expected interface GUID
    // - this method will be implemented as thread-safe
    // - you can specify some method names, or all methods redirection if []
    procedure Redirect(const aCallback: TInterfacedObject;
      const aMethodsNames: array of RawUTF8; aSubscribe: boolean = true); overload;
  end;


implementation


{ ************ TSQLRecordServiceLog TSQLRecordServiceNotifications Classes }

{ TSQLRecordServiceLog }

class procedure TSQLRecordServiceLog.InitializeTable(const Server: IRestORMServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
begin
  inherited;
  if FieldName = '' then
    Server.CreateSQLMultiIndex(Self, ['Method', 'MicroSec'], false);
end;


{ TSQLRecordServiceNotifications }

class procedure TSQLRecordServiceNotifications.InitializeTable(
  const Server: IRestORMServer; const FieldName: RawUTF8;
  Options: TSQLInitializeTableOptions);
begin
  inherited;
  if (FieldName = '') or (FieldName = 'Sent') then
    Server.CreateSQLMultiIndex(Self, ['Sent'], false);
end;

class function TSQLRecordServiceNotifications.LastEventsAsObjects(
  const Rest: IRestORM; LastKnownID: TID; Limit: integer; Service: TInterfaceFactory;
  out Dest: TDocVariantData; const MethodName: RawUTF8; IDAsHexa: boolean): boolean;
var
  res: TSQLRecordServiceNotifications;
begin
  res := CreateAndFillPrepare(Rest, 'ID > ? order by ID limit %',
    [Limit], [LastKnownID], 'ID,Method,Input');
  try
    if res.FillTable.RowCount > 0 then
    begin
      res.SaveFillInputsAsObjects(Service, Dest, MethodName, IDAsHexa);
      result := true;
    end
    else
      result := false;
  finally
    res.Free;
  end;
end;

function TSQLRecordServiceNotifications.SaveInputAsObject(
  Service: TInterfaceFactory; const MethodName: RawUTF8; IDAsHexa: boolean): variant;
var
  m: integer;
begin
  VarClear(result);
  with TDocVariantData(result) do
    if IDAsHexa then
      InitObject(['ID', Int64ToHex(fID), MethodName, Method], JSON_OPTIONS_FAST)
    else
      InitObject(['ID', fID, MethodName, Method], JSON_OPTIONS_FAST);
  m := Service.FindMethodIndex(Method);
  if m >= 0 then
    Service.Methods[m].ArgsAsDocVariantObject(
      _Safe(fInput)^, TDocVariantData(result), true);
end;

procedure TSQLRecordServiceNotifications.SaveFillInputsAsObjects(
  Service: TInterfaceFactory; out Dest: TDocVariantData; const MethodName: RawUTF8;
  IDAsHexa: boolean);
begin
  Dest.InitFast(FillTable.RowCount, dvArray);
  while FillOne do
    Dest.AddItem(SaveInputAsObject(Service, MethodName, IDAsHexa));
end;


{ ************ TServiceFactory Abstract Service Provider }

{ TServiceFactory }

constructor TServiceFactory.Create(aOwner: TInterfaceResolver;
  aInterface: PRttiInfo; aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8);
begin
  inherited CreateWithResolver(aOwner, {raiseIfNotFound=}true);
  fInterface := TInterfaceFactory.Get(aInterface);
  fInstanceCreation := aInstanceCreation;
  fInterfaceMangledURI := BinToBase64URI(@fInterface.InterfaceIID, SizeOf(TGUID));
  fInterfaceURI := fInterface.InterfaceURI;
  if fORM.Model.GetTableIndex(fInterfaceURI) >= 0 then
    raise EServiceException.CreateUTF8('%.Create: I% routing name is ' +
      'already used by a % SQL table name', [self, fInterfaceURI, fInterfaceURI]);
  SetLength(fExecution, fInterface.MethodsCount);
  // compute interface signature (aka "contract"), serialized as a JSON object
  FormatUTF8('{"contract":"%","implementation":"%","methods":%}', [fInterfaceURI,
    LowerCase(TrimLeftLowerCaseShort(ToText(InstanceCreation))), fInterface.Contract],
    fContract);
  fContractHash := '"' + CardinalToHex(Hash32(fContract)) +
    CardinalToHex(CRC32string(fContract)) + '"'; // 2 hashes to avoid collision
  if aContractExpected <> '' then // override default contract
    if aContractExpected[1] <> '"' then // stored as JSON string
      fContractExpected := '"' + aContractExpected + '"'
    else
      fContractExpected := aContractExpected
  else
    fContractExpected := fContractHash; // for security
end;

function TServiceFactory.GetInterfaceTypeInfo: PRttiInfo;
begin
  if (Self <> nil) and (fInterface <> nil) then
    result := fInterface.InterfaceTypeInfo
  else
    result := nil;
end;

function TServiceFactory.GetInterfaceIID: TGUID;
begin
  result := fInterface.InterfaceIID;
end;

procedure TServiceFactory.ExecutionAction(const aMethod: array of RawUTF8;
  aOptions: TInterfaceMethodOptions; aAction: TServiceMethodOptionsAction);

  procedure SetAction(var exec: TServiceFactoryExecution);
  begin
    case aAction of
      moaReplace:
        exec.Options := aOptions;
      moaInclude:
        exec.Options := exec.Options + aOptions;
      moaExclude:
        exec.Options := exec.Options - aOptions;
    end;
  end;

var
  i, m: PtrInt;
begin
  if high(aMethod) < 0 then
    for i := 0 to fInterface.MethodsCount - 1 do
      SetAction(fExecution[i])
  else
    for m := 0 to high(aMethod) do
      SetAction(fExecution[fInterface.CheckMethodIndex(aMethod[m])]);
  fAnyOptions := [];
  for i := 0 to fInterface.MethodsCount - 1 do
    fAnyOptions := fAnyOptions + fExecution[i].Options;
end;

function ToText(si: TServiceInstanceImplementation): PShortString;
begin
  result := GetEnumName(TypeInfo(TServiceInstanceImplementation), ord(si));
end;


{ ************ TServiceContainer Abstract Services Holder }

{ TServiceContainer }

constructor TServiceContainer.Create(aOwner: TInterfaceResolver);
begin
  fOwner := aOwner;
  fInterfaces.InitSpecific(TypeInfo(TServiceContainerInterfaces),
    fInterface, ptRawUTF8, nil, {caseinsensitive=}true);
  fInterfaceMethods.InitSpecific(TypeInfo(TServiceContainerInterfaceMethods),
    fInterfaceMethod, ptRawUTF8, nil, {caseinsensitive=}true);
end;

destructor TServiceContainer.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to high(fInterface) do
    fInterface[i].Service.Free;
  inherited;
end;

function TServiceContainer.Count: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fInterface);
end;

function TServiceContainer.AddServiceInternal(aService: TServiceFactory): integer;
var
  MethodIndex: integer;

  procedure AddOne(const aInterfaceDotMethodName: RawUTF8);
  var
    p: PServiceContainerInterfaceMethod;
  begin
    p := fInterfaceMethods.AddUniqueName(aInterfaceDotMethodName);
    p^.InterfaceService := aService;
    p^.InterfaceMethodIndex := MethodIndex;
    inc(MethodIndex);
  end;

var
  aURI: RawUTF8;
  internal: TServiceInternalMethod;
  m: PtrInt;
begin
  if (self = nil) or (aService = nil) then
    raise EServiceException.CreateUTF8('%.AddServiceInternal(%)', [self, aService]);
  // add TServiceFactory to the internal list
  if ExpectMangledURI then
    aURI := aService.fInterfaceMangledURI
  else
    aURI := aService.fInterfaceURI;
  PServiceContainerInterface(fInterfaces.AddUniqueName(aURI, @result))^.
    Service := aService;
  // add associated methods - first SERVICE_PSEUDO_METHOD[], then from interface
  aURI := aURI + '.';
  MethodIndex := 0;
  for internal := Low(TServiceInternalMethod) to High(TServiceInternalMethod) do
    AddOne(aURI + SERVICE_PSEUDO_METHOD[internal]);
  for m := 0 to aService.fInterface.MethodsCount - 1 do
    AddOne(aURI + aService.fInterface.Methods[m].URI);
end;

procedure TServiceContainer.CheckInterface(const aInterfaces: array of PRttiInfo);
var
  i: PtrInt;
begin
  for i := 0 to high(aInterfaces) do
    if aInterfaces[i] = nil then
      raise EServiceException.CreateUTF8('%: aInterfaces[%]=nil', [self, i])
    else
      with aInterfaces[i]^ do
        if InterfaceGUID = nil then
          raise EServiceException.CreateUTF8('%: % is not an interface', [self, Name^])
        else if not (ifHasGuid in InterfaceType^.IntfFlags) then
          raise EServiceException.CreateUTF8('%: % interface has no GUID', [self, Name^])
        else if Info(InterfaceGUID^) <> nil then
          raise EServiceException.CreateUTF8('%: % GUID already registered', [self, Name^]);
end;

procedure TServiceContainer.SetExpectMangledURI(aValue: Boolean);
var
  i: PtrInt;
  toregisteragain: TServiceContainerInterfaces;
begin
  if aValue = fExpectMangledURI then
    exit;
  fExpectMangledURI := aValue;
  toregisteragain := fInterface; // same services, but other URIs
  fInterface := nil;
  fInterfaces.InitSpecific(TypeInfo(TServiceContainerInterfaces),
    fInterface, ptRawUTF8, nil, {caseinsensitive=}not aValue);
  fInterfaceMethod := nil;
  fInterfaceMethods.InitSpecific(TypeInfo(TServiceContainerInterfaceMethods),
    fInterfaceMethod, ptRawUTF8, nil, not aValue);
  for i := 0 to high(toregisteragain) do
    AddServiceInternal(toregisteragain[i].Service);
end;

procedure TServiceContainer.SetInterfaceMethodBits(MethodNamesCSV: PUTF8Char;
  IncludePseudoMethods: boolean; out bits: TServiceContainerInterfaceMethodBits);
var
  i, n: PtrInt;
  method: RawUTF8;
begin
  FillCharFast(bits, SizeOf(bits), 0);
  n := length(fInterfaceMethod);
  if n > SizeOf(bits) shl 3 then
    raise EServiceException.CreateUTF8('%.SetInterfaceMethodBits: n=%', [self, n]);
  if IncludePseudoMethods then
    for i := 0 to n - 1 do
      if fInterfaceMethod[i].InterfaceMethodIndex < SERVICE_PSEUDO_METHOD_COUNT then
        include(bits, i);
  while MethodNamesCSV <> nil do
  begin
    GetNextItem(MethodNamesCSV, ',', method);
    if PosExChar('.', method) = 0 then
    begin
      for i := 0 to n - 1 do
        with fInterfaceMethod[i] do // O(n) search is fast enough here
          if (InterfaceMethodIndex >= SERVICE_PSEUDO_METHOD_COUNT) and
             IdemPropNameU(method, InterfaceService.fInterface.Methods[
              InterfaceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT].URI) then
            include(bits, i);
    end
    else
    begin
      i := fInterfaceMethods.FindHashed(method); // O(1) search
      if i >= 0 then
        include(bits, i);
    end;
  end;
end;

function TServiceContainer.GetMethodName(ListInterfaceMethodIndex: integer): RawUTF8;
begin
  if cardinal(ListInterfaceMethodIndex) >= cardinal(length(fInterfaceMethod)) then
    result := ''
  else
    with fInterfaceMethod[ListInterfaceMethodIndex] do
      result := InterfaceService.fInterface.GetMethodName(InterfaceMethodIndex);
end;

function TServiceContainer.GetService(const aURI: RawUTF8): TServiceFactory;
var
  i: Integer;
begin
  if (self <> nil) and (aURI <> '') then
  begin
    i := fInterfaces.FindHashed(aURI);
    if i >= 0 then
      result := fInterface[i].Service
    else
      result := nil;
  end
  else
    result := nil;
end;

function TServiceContainer.Info(aTypeInfo: PRttiInfo): TServiceFactory;
var
  n: TDALen;
  p: PServiceContainerInterface;
begin
  if self <> nil then
  begin
    p := pointer(fInterface);
    if p <> nil then
    begin
      n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
      repeat
        result := p^.Service;
        if result.fInterface.InterfaceTypeInfo = aTypeInfo then
          exit;
        inc(p);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

{$ifdef FPC_HAS_CONSTREF}
function TServiceContainer.Info(constref aGUID: TGUID): TServiceFactory;
{$else}
function TServiceContainer.Info(const aGUID: TGUID): TServiceFactory;
{$endif FPC_HAS_CONSTREF}
var
  n: TDALen;
  p: PServiceContainerInterface;
  g: THash128Rec absolute aGUID;
begin
  // very efficient generated asm on FPC
  if self <> nil then
  begin
    p := pointer(fInterface);
    if p <> nil then
    begin
      n := PDALen(PAnsiChar(p) - _DALEN)^ + _DAOFF;
      repeat
        result := p^.Service;
        with PHash128Rec(@result.fInterface.InterfaceIID)^ do
          if (g.L = L) and (g.H = H) then
            exit;
        inc(p);
        dec(n);
      until n = 0;
    end;
  end;
  result := nil;
end;

procedure TServiceContainer.SetGUIDs(out Services: TGUIDDynArray);
var
  i, n: PtrInt;
begin
  if self = nil then
    exit;
  n := length(fInterface);
  SetLength(Services, n);
  for i := 0 to n - 1 do
    Services[i] := fInterface[i].Service.fInterface.InterfaceIID;
end;

procedure TServiceContainer.SetInterfaceNames(out Names: TRawUTF8DynArray);
var
  i, n: PtrInt;
begin
  if self = nil then
    exit;
  n := length(fInterface);
  SetLength(Names, n);
  for i := 0 to n - 1 do
    Names[i] := fInterface[i].Service.fInterface.InterfaceURI;
end;

function TServiceContainer.AsJson: RawJSON;
var
  WR: TTextWriter;
  i: PtrInt;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if (self = nil) or (fInterface = nil) then
    exit;
  WR := TJSONSerializer.CreateOwnedStream(temp);
  try
    WR.Add('[');
    for i := 0 to high(fInterface) do
    begin
      WR.AddString(fInterface[i].Service.Contract);
      WR.Add(',');
    end;
    WR.CancelLastComma;
    WR.Add(']');
    WR.SetText(RawUTF8(result));
  finally
    WR.Free;
  end;
end;

function TServiceContainer.TryResolve(aInterface: PRttiInfo; out Obj): boolean;
var
  factory: TServiceFactory;
begin
  factory := Info(aInterface);
  if factory = nil then
    result := inherited TryResolve(aInterface, Obj)
  else
    result := factory.Get(Obj);
end;

function TServiceContainer.Index(aIndex: integer): TServiceFactory;
begin
  if (self = nil) or (cardinal(aIndex) > cardinal(high(fInterface))) then
    result := nil
  else
    result := fInterface[aIndex].Service;
end;

function TServiceContainer.CallBackUnRegister(const Callback: IInvokable): boolean;
begin
  result := false; // nothing to be done here
end;



initialization

finalization

end.

