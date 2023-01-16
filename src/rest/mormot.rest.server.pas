/// REpresentation State Tranfer (REST) Types and Classes on Server Side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.server;

{
  *****************************************************************************

   Server-Side REST Process
    - TRestServerUriContext Access to the Server-Side Execution
    - TRestServerRoutingRest/TRestServerRoutingJsonRpc Requests Parsing Scheme
    - TAuthSession for In-Memory User Sessions
    - TRestServerAuthentication Implementing Authentication Schemes
    - TRestServerMonitor for High-Level Statistics of a REST Server
    - TRestRouter for efficient Radix Tree based URI Multiplexing
    - TRestServer Abstract REST Server
    - TRestHttpServerDefinition Settings for a HTTP Server

  *****************************************************************************
}

  { TODO : implement TRestServer.AdministrationExecute }

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
  mormot.crypt.jwt,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.core.interfaces,
  {$ifdef DOMAINRESTAUTH}
  mormot.lib.sspi, // do-nothing units on non compliant system
  mormot.lib.gssapi,
  {$endif DOMAINRESTAUTH}
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.soa.core,
  mormot.db.core,
  mormot.rest.core,
  mormot.rest.client;


// most types are defined as a single "type" statement due to classes coupling

{ ************ TRestServerUriContext Access to the Server-Side Execution }

type
  // some forward class definition
  TAuthSession = class;
  TRestServer = class;
  TRestServerUriContext = class;

  /// exception raised in case of unexpected parsing error
  EParsingException = class(ESynException);

  /// exception raised when security authorization/authentication failed
  ESecurityException = class(ESynException);

  /// points to the currently running service on the server side
  // - your code may call the ServiceRunningContext global function
  // once in a method, since per-thread access does cost some CPU
  // !var
  // !  context: PServiceRunningContext;
  // !begin
  // !  context := ServiceRunningContext; // threadvar access once
  // !  ...
  PServiceRunningContext = ^TServiceRunningContext;

  /// will identify the currently running service on the server side
  // - is the type of the ServiceRunningContext per-thread function
  // - to access the current TRestServer instance (and e.g. its ORM/CRUD
  // or SOA methods), use Request.Server and not Factory.Server, which may not
  // be available e.g. if you run the service from the server side (so no
  // factory is involved)
  // - note that the safest (and slightly faster) access to the TRestServer
  // instance associated with a service is to inherit your implementation
  // class from TInjectableObjectRest
  // - should map TPerThreadRunningContext private version in
  // mormot.core.interfaces.pas
  TServiceRunningContext = record
    /// the currently running TServiceFactoryServer instance
    // - it can be used within server-side implementation to retrieve the
    // associated TRestServer instance
    // - note that TServiceFactoryServer.Get() won't override this value, when
    // called within another service (i.e. if Factory is not nil)
    Factory: TServiceFactory;
    /// the currently runnning context which launched the method
    // - low-level RESTful context is also available in its Call member
    // - Request.Server is the safe access point to the underlying TRestServer,
    // unless the service is implemented via TInjectableObjectRest, so the
    // TInjectableObjectRest.Server property is preferred
    // - make available e.g. current session or authentication parameters
    // (including e.g. user details via Request.Server.SessionGetUser)
    Request: TRestServerUriContext;
    /// the thread which launched the request
    // - is set by TRestServer.BeginCurrentThread from multi-thread server
    // handlers - e.g. TRestHttpServer
    RunningThread: TThread;
  end;

  /// kind of (static) database server implementation available
  // - sMainEngine will identify the default main SQlite3 engine
  // - sStaticDataTable will identify a TRestStorageInMemory - i.e.
  // TRestServer.fStaticData[] which can work without SQLite3
  // - sVirtualTable will identify virtual TRestStorage classes - i.e.
  // TRestOrmServer.fStaticVirtualTable[] which points to SQLite3 virtual tables
  // (e.g. TObjectList or external databases)
  TRestServerKind = (
    sMainEngine,
    sStaticDataTable,
    sVirtualTable);

  /// used to identify the authentication failure reason
  // - as transmitted e.g. by TRestServerUriContext.AuthenticationFailed or
  // TRestServer.OnAuthenticationFailed
  TOnAuthenticationFailedReason = (
    afInvalidSignature,
    afRemoteServiceExecutionNotAllowed,
    afUnknownUser,
    afInvalidPassword,
    afSessionAlreadyStartedForThisUser,
    afSessionCreationAborted,
    afSecureConnectionRequired,
    afJWTRequired);

  /// define the TRestTreeNode kind of REST requests
  // - as registered by TRestServer.ComputeRoutes, i.e.
  // - rnTable for ModelRoot/TableName GET POST PUT DELETE BEGIN END ABORT
  // - rnTableID for ModelRoot/TableName/<id> GET LOCK UNLOCK PUT DELETE
  // - rnTableIDBlob for ModelRoot/TableName/<id>/Blob GET PUT
  // - rnTableMethod for ModelRoot/TableName/MethodName GET POST PUT DELETE
  // - rnTableIDMethod ModelRoot/TableName/<id>/MethodName GET POST PUT DELETE
  // - rnState for ModelRoot STATE
  // - rnMethod for ModelRoot/MethodName GET POST PUT DELETE
  // - rnMethodPath for ModelRoot/MethodName/<path:fulluri> GET POST PUT DELETE
  // - rnInterface for ModelRoot/InterfaceName[/.InterfaceMethodName]
  // - rnInterfaceClientID for ModelRoot/InterfaceName/.InterfaceMethodName/<id>
  TRestNode = (
    rnNone,
    rnTable,
    rnTableID,
    rnTableIDBlob,
    rnTableMethod,
    rnTableIDMethod,
    rnState,
    rnMethod,
    rnMethodPath,
    rnInterface,
    rnInterfaceClientID);

  TRestRouter = class;

  /// pointer to a description of a method-based service
  PRestServerMethod = ^TRestServerMethod;

  /// abstract calling context for a TOnRestServerCallBack event handler
  // - having a dedicated class avoid changing the implementation methods
  // signature if the framework add some parameters or behavior to it
  // - see TOnRestServerCallBack for general code use
  // - most of the internal methods are declared as virtual, so it allows any
  // kind of custom routing or execution scheme
  // - instantiated by the TRestServer.Uri() method using its ServicesRouting
  // property
  // - see TRestServerRoutingRest and TRestServerRoutingJsonRpc for workable
  // classes - this abstract class will be rejected for TRest.ServicesRouting
  // - on client side, see TRestClientRouting reciprocal class hierarchy and
  // the ClientRouting class method - as defined in mormot.rest.client.pas
  TRestServerUriContext = class(TRestUriContext)
  protected
    fServer: TRestServer;
    fInput: TRawUtf8DynArray; // [nam1,val1, nam2,val2, ...] pairs
    fInputAllowDouble: boolean;
    fStaticKind: TRestServerKind;
    fNode: TRestNode;
    fCommand: TRestServerUriContextCommand;
    fServiceMethodIndex: integer;
    fUriSessionSignaturePos: integer;
    fMethodIndex: integer;
    fTableIndex: integer;
    fAuthSession: TAuthSession;
    fUriMethodPath: RawUtf8;
    fUriBlobField: TOrmPropInfoRttiRawBlob;
    fThreadServer: PServiceRunningContext;
    fTable: TOrmClass;
    fTableModelProps: TOrmModelProperties;
    fTableEngine: TRestOrm;
    fTableID: TID;
    fServerMethod: PRestServerMethod;
    fService: TServiceFactory;
    fServiceMethod: PInterfaceMethod;
    fServiceParameters: PUtf8Char;
    fServiceInstanceID: TID;
    fServiceExecution: PServiceFactoryExecution;
    fServiceExecutionOptions: TInterfaceMethodOptions;
    fForceServiceResultAsJsonObject: boolean;
    fForceServiceResultAsJsonObjectWithoutResult: boolean;
    fForceServiceResultAsXMLObject: boolean;
    fForceServiceResultAsXMLObjectNameSpace: RawUtf8;
    fRouterParam: PUtf8Char; // <int:clientid> or <int:tableid>
    fParameters: PUtf8Char;
    fSession: cardinal;
    fSessionOS: TOperatingSystemVersion; // 32-bit raw OS info
    fSessionGroup: TID;
    fSessionUser: TID;
    fStaticOrm: TRestOrm;
    fSessionUserName: RawUtf8;
    fCustomErrorMsg: RawUtf8;
    fTemp: RawUtf8; // used e.g. for XML or Cookie process
    fMicroSecondsStart: Int64;
    fMicroSecondsElapsed: QWord;
    fLog: TSynLog;
    fStatsInSize, fStatsOutSize: integer;
    fSessionAccessRights: TOrmAccessRights; // fSession may be deleted meanwhile
    function GetInput(const ParamName: RawUtf8): variant;
    function GetInputOrVoid(const ParamName: RawUtf8): variant;
    function GetInputNameIndex(const ParamName: RawUtf8): PtrInt;
    function GetInputExists(const ParamName: RawUtf8): boolean;
    function GetInputInt(const ParamName: RawUtf8): Int64;
    function GetInputDouble(const ParamName: RawUtf8): Double;
    procedure GetInputByName(const ParamName, InputName: RawUtf8;
      var result: RawUtf8);
    function GetInputUtf8(const ParamName: RawUtf8): RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInputString(const ParamName: RawUtf8): string;
    function GetInputIntOrVoid(const ParamName: RawUtf8): Int64;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInputHexaOrVoid(const ParamName: RawUtf8): cardinal;
    function GetInputDoubleOrVoid(const ParamName: RawUtf8): Double;
    function GetInputUtf8OrVoid(const ParamName: RawUtf8): RawUtf8;
    function GetInputStringOrVoid(const ParamName: RawUtf8): string;
    function GetResourceFileName: TFileName;
    procedure InternalSetTableFromTableIndex(Index: PtrInt); virtual;
    procedure InternalSetTableFromTableName(
      TableName: PUtf8Char; TableNameLen: PtrInt); virtual;
    procedure InternalExecuteSoaByInterface; virtual;
    procedure InternalExecuteSoaByInterfaceComputeResult;
    procedure ComputeStatsAfterCommand;
    procedure SetOutSetCookie(const aOutSetCookie: RawUtf8); override;
    function IsRemoteIPBanned: boolean; // as method to avoid temp IP string
    /// register the interface-based SOA URIs to Server.Router multiplexer
    // - abstract implementation which is to be overridden
    class procedure UriComputeRoutes(
      Router: TRestRouter; Server: TRestServer); virtual; abstract;
    /// copy TAuthSession values into the Session* members
    // - this method is not thread-safe: caller should use Sessions.Safe.ReadOnlyLock
    procedure SessionAssign(AuthSession: TAuthSession);
    /// process authentication
    // - return FALSE in case of invalid signature, TRUE if authenticated
    function Authenticate: boolean; virtual;
    /// method called in case of authentication failure
    // - the failure origin is stated by the Reason parameter
    // - this default implementation will just set OutStatus := HTTP_FORBIDDEN
    // and call TRestServer.OnAuthenticationFailed event (if any)
    procedure AuthenticationFailed(Reason: TOnAuthenticationFailedReason); virtual;
    /// direct launch of a method-based service
    // - Uri() will ensure that MethodIndex>=0 before calling it
    procedure ExecuteSoaByMethod; virtual;
    /// direct launch of an interface-based service
    // - Uri() will ensure that Service<>nil before calling it
    // - abstract implementation which is to be overridden
    procedure ExecuteSoaByInterface; virtual; abstract;
    /// handle GET/LOCK/UNLOCK/STATE verbs for ORM/CRUD process
    procedure ExecuteOrmGet; virtual;
    /// handle POST/PUT/DELETE/BEGIN/END/ABORT verbs for ORM/CRUD process
    // - execution of this method is protected by a critical section
    procedure ExecuteOrmWrite; virtual;
    /// launch the Execute* method  in the execution mode
    // set by Server.AcquireExecutionMode/AcquireExecutionLockedTimeOut
    // - this is the main process point from TRestServer.Uri()
    procedure ExecuteCommand;
  public
    /// initialize the execution context
    // - this method could have been declared as protected, since it should
    // never be called outside the TRestServer.Uri() method workflow
    // - should set Call, and Method members
    constructor Create(aServer: TRestServer;
      const aCall: TRestUriParams); reintroduce; virtual;
    /// finalize the execution context
    destructor Destroy; override;

    /// validate mPOST/mPUT/mDELETE action against current session access rights
    // - used by TRestServerUriContext.ExecuteOrmWrite and
    // TRestServer.EngineBatchSend methods for proper security checks
    function CanExecuteOrmWrite(Method: TUriMethod;
      Table: TOrmClass; TableIndex: integer; const TableID: TID;
      const Rights: TOrmAccessRights): boolean;
    /// extract the input parameters from its URI (up to 512 params)
    // - you should not have to call this method directly, but rather
    // all the InputInt/InputDouble/InputUtf8/InputExists/... properties
    // - may be useful if you want to access directly to InputPairs[] with no
    // prior knowledge of the input parameter names
    // - you can specify a title text to optionally log the input array
    procedure FillInput(const LogInputIdent: RawUtf8 = '');

    /// retrieve one input parameter from its URI name as Int64
    // - raise an EParsingException if the parameter is not found
    property InputInt[const ParamName: RawUtf8]: Int64
      read GetInputInt;
    /// retrieve one input parameter from its URI name as double
    // - raise an EParsingException if the parameter is not found
    property InputDouble[const ParamName: RawUtf8]: double
      read GetInputDouble;
    /// retrieve one input parameter from its URI name as RawUtf8
    // - raise an EParsingException if the parameter is not found
    property InputUtf8[const ParamName: RawUtf8]: RawUtf8
      read GetInputUtf8;
    /// retrieve one input parameter from its URI name as a VCL string
    // - raise an EParsingException if the parameter is not found
    // - prior to Delphi 2009, some Unicode characters may be missing in the
    // returned AnsiString value
    property InputString[const ParamName: RawUtf8]: string
      read GetInputString;
    /// retrieve one input parameter from its URI name as Int64
    // - returns 0 if the parameter is not found
    property InputIntOrVoid[const ParamName: RawUtf8]: Int64
      read GetInputIntOrVoid;
    /// retrieve one hexadecimal input parameter from its URI name as cardinal
    // - returns 0 if the parameter is not found
    property InputHexaOrVoid[const ParamName: RawUtf8]: cardinal
      read GetInputHexaOrVoid;
    /// retrieve one input parameter from its URI name as double
    // - returns 0 if the parameter is not found
    property InputDoubleOrVoid[const ParamName: RawUtf8]: double
      read GetInputDoubleOrVoid;
    /// retrieve one input parameter from its URI name as RawUtf8
    // - returns '' if the parameter is not found
    property InputUtf8OrVoid[const ParamName: RawUtf8]: RawUtf8
      read GetInputUtf8OrVoid;
    /// retrieve one input parameter from its URI name as a VCL string
    // - returns '' if the parameter is not found
    // - prior to Delphi 2009, some Unicode characters may be missing in the
    // returned AnsiString value
    property InputStringOrVoid[const ParamName: RawUtf8]: string
      read GetInputStringOrVoid;
    /// retrieve one input parameter from its URI name as RawUtf8
    // - returns FALSE and call Error(ErrorMessageForMissingParameter) - which
    // may be a resourcestring - if the parameter is not found; if
    // ErrorMessageForMissingParameter is not set, a default message is used
    // - returns TRUE and set Value if the parameter is found
    function InputUtf8OrError(const ParamName: RawUtf8; out Value: RawUtf8;
      const ErrorMessageForMissingParameter: string = ''): boolean;
    /// retrieve one input parameter from its URI name as RawUtf8
    // - returns supplied DefaultValue if the parameter is not found
    function InputUtf8OrDefault(const ParamName, DefaultValue: RawUtf8): RawUtf8;
    /// retrieve one input parameter from its URI name as an enumeration
    // - will expect the value to be specified as integer, or as the textual
    // representation of the enumerate, ignoring any optional lowercase prefix
    // as featured by TEnumType.GetEnumNameValue()
    // - returns TRUE and set ValueEnum if the parameter is found and correct
    // - returns FALSE and set ValueEnum to first item (i.e. DefaultEnumOrd) if
    // the parameter is not found, or not containing a correct value
    function InputEnum(const ParamName: RawUtf8; EnumType: PRttiInfo;
      out ValueEnum; DefaultEnumOrd: integer = 0): boolean;
    /// return TRUE if the input parameter is available at URI
    // - even if InputUtf8['param']='', there may be '..?param=&another=2'
    property InputExists[const ParamName: RawUtf8]: boolean
      read GetInputExists;
    /// retrieve one input parameter from its URI name as variant
    // - if the parameter value is text, it is stored in the variant as
    // a generic VCL string content: so before Delphi 2009, you may loose
    // some characters at decoding from UTF-8 input buffer
    // - raise an EParsingException if the parameter is not found
    property Input[const ParamName: RawUtf8]: variant
      read GetInput; default;
    /// retrieve one input parameter from its URI name as variant
    // - if the parameter value is text, it is stored in the variant as
    // a RawUtf8: so before Delphi 2009, you won't loose any Unicode character,
    // but you should convert its value to AnsiString using Utf8ToString()
    // - returns Unassigned if the parameter is not found
    property InputOrVoid[const ParamName: RawUtf8]: variant
      read GetInputOrVoid;
    /// retrieve one input parameter from its URI name as variant
    // - returns FALSE and call Error(ErrorMessageForMissingParameter) - which
    // may be a resourcestring - if the parameter is not found
    // - returns TRUE and set Value if the parameter is found
    // - if the parameter value is text, it is stored in the variant as
    // a RawUtf8: so before Delphi 2009, you won't loose any Unicode character,
    // but you should convert its value to AnsiString using Utf8ToString()
    function InputOrError(const ParamName: RawUtf8; out Value: variant;
      const ErrorMessageForMissingParameter: string): boolean;
    /// if Input[] InputOrVoid[] InputOrError() variants could be double
    property InputAllowDouble: boolean
      read fInputAllowDouble write fInputAllowDouble;
    /// retrieve all input parameters from URI as a variant JSON object
    // - returns Unassigned if no parameter was defined
    // - returns a JSON object with input parameters encoded as
    // ! {"name1":value1,"name2":value2...}
    // - optionally with a PInterfaceMethod information about the actual values types
    // - if the parameters were encoded as multipart, the JSON object
    // will be encoded with its textual values, or with nested objects, if
    // the data was supplied as binary:
    // ! {"name1":{"data":..,"filename":...,"contenttype":...},"name2":...}
    // since name1.data will be Base64 encoded, so you should better
    // use the InputAsMultiPart() method instead when working with binary
    function GetInputAsTDocVariant(const Options: TDocVariantOptions;
      InterfaceMethod: PInterfaceMethod): variant;
    /// low-level access to the input parameters, stored as pairs of UTF-8
    // - even items are parameter names, odd are values
    // - Input*[] properties should have been called previously to fill the
    // internal array, or by calling FillInput if you do not know the input
    // parameters which may appear
    property InputPairs: TRawUtf8DynArray
      read fInput;

    /// method overriden to support rsoAuthenticationUriDisable option
    // - i.e. as an alternative, a non-standard and slightly less safe way of
    // token transmission may be to encode its value as ?authenticationbearer=....
    // URI parameter (may be convenient when embedding resources in HTML DOM -
    // but note that the URI is usually part of the web server logs, so it may
    // be unsafe to use it on production - but may be handy e.g. for debugging
    // or if you can't tweak the HTTP headers - as with websockets on JavaScript)
    function AuthenticationBearerToken: RawUtf8; override;
    /// validate "Authorization: Bearer <JWT>" content from incoming HTTP headers
    // - overriden to support TRestServer.JwtForUnauthenticatedRequestWhiteIP()
    function AuthenticationCheck(jwt: TJwtAbstract): boolean; override;
    /// low-level access to the associated Session
    // - may be nil depending on the context: you should NOT use it, but the
    // safe Session, SessionGroup, SessionUser, SessionUserName fields instead
    // - is used internally
    property AuthSession: TAuthSession
      read fAuthSession;
    /// the associated routing class on the client side
    class function ClientRouting: TRestClientRoutingClass; virtual; abstract;
    /// identify if the request is about a Table containing nested objects or
    // arrays, which could be serialized as JSON objects or arrays, instead
    // of plain JSON string (as stored in the database)
    // - will idenfity ClientKind=ckAjax, or check for rsoGetAsJsonNotAsString
    // in TRestServer.Options
    function ClientOrmOptions: TOrmWriterOptions;
    /// true if called from TRestServer.AdministrationExecute
    function IsRemoteAdministrationExecute: boolean;
      {$ifdef FPC}inline;{$endif}
    /// compute the file name corresponding to the URI
    // - e.g. '/root/methodname/toto/index.html' will return 'toto\index.html'
    property ResourceFileName: TFileName
      read GetResourceFileName;
    /// implements a method-based service for live update of some settings
    // - should be called from a method-based service, e.g. Configuration()
    // - the settings are expected to be stored e.g. in a TSynAutoCreateFields
    // instance, potentially with nested objects
    // - accept the following REST methods to read and write the settings:
    // ! GET http://server:888/root/configuration
    // ! GET http://server:888/root/configuration/propname
    // ! GET http://server:888/root/configuration/propname?value=propvalue
    // - could be used e.g. as such:
    // ! procedure TMyRestServerMethods.Configuration(Ctxt: TRestServerUriContext);
    // ! begin //  http://server:888/myrestserver/configuration/name?value=newValue
    // !   Ctxt.ConfigurationRestMethod(fSettings);
    // ! end;
    procedure ConfigurationRestMethod(SettingsStorage: TObject);
    /// low-level HTTP header merge of the OutSetCookie value
    // - this overriden method will handle rsoCookieIncludeRootPath option
    procedure OutHeadFromCookie; override;
    /// low-level process of the JSON result for a service execution
    procedure ServiceResult(const Name, JsonValue: RawUtf8);
    /// low-level preparation of the JSON result for a service execution
    procedure ServiceResultStart(WR: TJsonWriter); virtual;
    /// low-level closure of the JSON result for a service execution
    procedure ServiceResultEnd(WR: TJsonWriter; ID: TID); virtual;
    /// low-level statistics merge during service execution
    procedure StatsFromContext(Stats: TSynMonitorInputOutput; MicroSec: Int64);
    /// low-level logging after service execution
    procedure LogFromContext;
    /// event raised by ExecuteMethod() for interface parameters
    // - match TInterfaceMethodInternalExecuteCallback signature
    // - redirect to TServiceContainerServer.GetFakeCallback
    procedure ExecuteCallback(var Ctxt: TJsonParserContext;
      ParamInterfaceInfo: TRttiJson; out Obj); virtual;
    /// use this method to send back a file from a local folder to the caller
    // - UriMethodPath value, as parsed from the URI, will contain the
    // expected file name in the local folder, using DefaultFileName if the
    // URI is void, and redirecting to Error404Redirect if the file is not found
    procedure ReturnFileFromFolder(const FolderName: TFileName;
      Handle304NotModified: boolean = true;
      const DefaultFileName: TFileName = 'index.html';
      const Error404Redirect: RawUtf8 = ''; CacheControlMaxAge: integer = 0); override;
    /// use this method to send back an error to the caller
    // - overriden method with additional logging
    procedure Error(const ErrorMessage: RawUtf8 = '';
      Status: integer = HTTP_BADREQUEST; CacheControlMaxAge: integer = 0); override;

    /// the associated TRestServer instance which executes its URI method
    property Server: TRestServer
      read fServer;
    /// same as Call^.Uri, but without the &session_signature=... ending
    // - will compute it from Call^.Url and UriSessionSignaturePos
    function UriWithoutSignature: RawUtf8;
    /// same as Call^.Uri, after the 'root/' prefix, including '?' params
    // - will compute it from Call^.Url and Server.Model.RootLen
    function UriWithoutRoot: RawUtf8;
    /// same as Call^.Uri, but without the ?... ending
    // - will compute it from Call^.Url and fParameters
    // - since used for logging, return a shortstring and not a RawUtf8 to
    // avoid memory allocation
    function UriWithoutInlinedParams: shortstring;
    /// the URI after the method service name, excluding the '?' parameters
    // - as set by TRestTreeNode.LookupParam from <path:fulluri> place holder
    property UriMethodPath: RawUtf8
      read fUriMethodPath;
    /// position of the &session_signature=... text in Call^.Url string
    property UriSessionSignaturePos: integer
      read fUriSessionSignaturePos;
    /// URI inlined parameters position within Call^.Url, just after trailing '?'
    // - use UrlDecodeValue*() functions to retrieve the values
    // - for mPOST requests, will also be filled from a WEB form body, i.e.
    // ! application/x-www-form-urlencoded
    // - use InputAsMultiPart() for multipart/form-data decoding
    property Parameters: PUtf8Char
      read fParameters;
    /// just a wrapper over ServiceRunningContext function result
    // - avoid a call to the threadvar resolution
    property ThreadServer: PServiceRunningContext
      read fThreadServer;
    /// the Table as specified at the URI level (if any)
    property Table: TOrmClass
      read fTable;
    /// the index in the Model of the Table specified at the URI level (if any)
    property TableIndex: integer
      read fTableIndex;
    /// the RTTI properties of the Table specified at the URI level (if any)
    property TableModelProps: TOrmModelProperties
      read fTableModelProps;
    /// the RESTful instance implementing the Table specified at the URI level (if any)
    // - equals the Server field most of the time, but may be an TRestStorage
    // for any in-memory/MongoDB/virtual instance
    property TableEngine: TRestOrm
      read fTableEngine;
    /// the associated TOrm.ID, as decoded from URI scheme
    // - this property will be set from incoming URI, even if RESTful
    // authentication is not enabled
    property TableID: TID
      read fTableID;
    /// the current execution command
    property Command: TRestServerUriContextCommand
      read fCommand;
    /// the index of the callback published method within the internal class list
    property MethodIndex: integer
      read fMethodIndex;
    /// the service identified by an interface-based URI
    // - is in fact a TServiceFactoryServer instance
    property Service: TServiceFactory
      read fService write fService;
    /// the method index for an interface-based service
    // - filled if Service is not nil
    // - as retrieved by Service.ServiceMethodIndex(), i.e. 0..3 as internal
    // _free_/_contract_/_signature_/_instance_ pseudo-methods or in
    // InterfaceFactory.Methods[ServiceMethodIndex-SERVICE_PSEUDO_METHOD_COUNT]
    property ServiceMethodIndex: integer
      read fServiceMethodIndex write fServiceMethodIndex;
    /// access to the raw information of an interface-based URI
    // - equals nil if ServiceMethodIndex is within 0..3 (pseudo-methods)
    property ServiceMethod: PInterfaceMethod
      read fServiceMethod write fServiceMethod;
    /// the JSON array of parameters for an the interface-based service
    // - Service member has already be retrieved from URI (so is not nil)
    property ServiceParameters: PUtf8Char
      read fServiceParameters write fServiceParameters;
    /// the instance ID for interface-based services instance
    // - can be e.g. the sicPerSession session ID, the sicPerThread thread ID,
    // or the sicClientDriven field as decoded by UriDecodeSoaByInterface
    property ServiceInstanceID: TID
      read fServiceInstanceID write fServiceInstanceID;
    /// the current execution context of an interface-based service
    // - maps to
    //! Service.fExecution[ServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT]
    property ServiceExecution: PServiceFactoryExecution
      read fServiceExecution write fServiceExecution;
    /// the current execution options of an interface-based service
    // - contain ServiceExecution.Options including optNoLogInput/optNoLogOutput
    // in case of TInterfaceFactory.RegisterUnsafeSpiType
    property ServiceExecutionOptions: TInterfaceMethodOptions
      read fServiceExecutionOptions write fServiceExecutionOptions;
    /// force the interface-based service methods to return a JSON object
    // - default behavior is to follow Service.ResultAsJsonObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a JSON object, even if Service.ResultAsJsonObject=false: this may be
    // handy when the method is executed from a JavaScript content
    property ForceServiceResultAsJsonObject: boolean
      read fForceServiceResultAsJsonObject;
    /// force the interface-based service methods to return a plain JSON object
    // - i.e. '{....}' instead of '{"result":{....}}'
    // - only set if ForceServiceResultAsJsonObject=TRUE and if no ID is about
    // to be returned
    // - could be used e.g. for stateless interaction with a (non mORMot)
    // stateless JSON REST Server
    property ForceServiceResultAsJsonObjectWithoutResult: boolean
      read fForceServiceResultAsJsonObjectWithoutResult;
    /// force the interface-based service methods to return a XML object
    // - default behavior is to follow Service.ResultAsJsonObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a XML object, by setting ForceServiceResultAsJsonObject then converting
    // the resulting JSON object into the corresponding XML via JsonBufferToXML()
    // - TRestServerUriContext.InternalExecuteSoaByInterface will inspect the
    // Accept HTTP header to check if the answer should be XML rather than JSON
    property ForceServiceResultAsXMLObject: boolean
      read fForceServiceResultAsXMLObject;
    /// specify a custom name space content when returning a XML object
    // - default behavior is to follow Service.ResultAsXMLObjectNameSpace
    // property (which is void by default)
    // - service may set e.g. XMLUTF8_NAMESPACE, which will append <content ...>
    // </content> around the generated XML data, to avoid validation problems
    // or set a particular XML name space, depending on the application
    property ForceServiceResultAsXMLObjectNameSpace: RawUtf8
      read fForceServiceResultAsXMLObjectNameSpace;
    /// the corresponding session TAuthSession.IDCardinal value
    // - equals 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED) if the session
    // is not started yet - i.e. if still in handshaking phase
    // - equals 1 (CONST_AUTHENTICATION_NOT_USED) if authentication mode
    // is not enabled - i.e. if TRestServer.HandleAuthentication = FALSE
    property Session: cardinal
      read fSession;
    /// the corresponding TAuthSession.User.GroupRights.ID value
    // - is undefined if Session is 0 or 1 (no authentication running)
    property SessionGroup: TID
      read fSessionGroup;
    /// the corresponding TAuthSession.User.ID value
    // - is undefined if Session is 0 or 1 (no authentication running)
    property SessionUser: TID
      read fSessionUser;
    /// the corresponding TAuthSession.User.LogonName value
    // - is undefined if Session is 0 or 1 (no authentication running)
    property SessionUserName: RawUtf8
      read fSessionUserName;
    /// the corresponding TAuthSession.RemoteOsVersion
    // - is undefined if Session is 0 or 1 (no authentication running) or if
    // the client was not using TRestClientAuthenticationDefault scheme
    property SessionOS: TOperatingSystemVersion
      read fSessionOS;
    /// the static instance corresponding to the associated Table (if any)
    property StaticOrm: TRestOrm
      read fStaticOrm;
    /// the kind of static instance corresponding to the associated Table (if any)
    property StaticKind: TRestServerKind
      read fStaticKind;
    /// optional error message which will be transmitted as JSON error (if set)
    // - contains e.g. TOnAuthenticationFailedReason text during
    // TRestServer.OnAuthenticationFailed event call, or the reason of a
    // TRestServer.RecordCanBeUpdated failure
    property CustomErrorMsg: RawUtf8
      read fCustomErrorMsg;
    /// high-resolution start of the execution command, in micro-seconds
    property MicroSecondsStart: Int64
      read fMicroSecondsStart;
    /// high-resolution timimg of the execution command, in micro-seconds
    // - only set when TRestServer.Uri finished, available e.g. for OnAfterUri
    property MicroSecondsElapsed: QWord
      read fMicroSecondsElapsed;
    /// associated logging instance for the current thread on the server
    // - you can use it to log some process on the server side
    property Log: TSynLog
      read fLog;
    {$ifndef PUREMORMOT2}
    // may be used in old method-based service callbacks
    property UriBlobFieldName: RawUtf8
      read fUriMethodPath;
    {$endif PUREMORMOT2}
  end;
  PRestServerUriContext = ^TRestServerUriContext;

  /// method prototype to be used on Server-Side for method-based services
  // - will be routed as ModelRoot/[TableName/TableID/]MethodName RESTful requests
  // - this mechanism is able to handle some custom Client/Server request, similar
  // to the DataSnap technology, but in a KISS way; it's fully integrated in the
  // Client/Server architecture of our framework
  // - just add a published method of this type to any TRestServer descendant
  // - when TRestServer.Uri receive a request for ModelRoot/MethodName
  // or ModelRoot/TableName/TableID/MethodName, it will check for a published method
  // in its self instance named MethodName which MUST be of TOnRestServerCallBack
  // type (not checked neither at compile time neither at runtime: beware!) and
  // call it to handle the request
  // - important warning: the method implementation MUST be thread-safe
  // - when TRestServer.Uri receive a request for ModelRoot/MethodName,
  // it calls the corresponding published method with aRecord set to nil
  // - when TRestServer.Uri receive a request for ModelRoot/TableName/TableID/MethodName,
  // it calls the corresponding published method with aRecord pointing to a
  // just created instance of the corresponding class,  with its field ID set;
  // note that the only set field is ID: other fields of aRecord are not set, but
  // must secificaly be retrieved on purpose
  // - for ModelRoot/TableName/TableID/MethodName, the just created instance will
  // be freed by TRestServer.Uri when the method returns
  // - Ctxt.Call is set with low-level incoming and outgoing data from client
  // (e.g. Ctxt.Call.InBody contain POST/PUT data message)
  // - Ctxt.Session* will identify to the authentication session of the remote client
  // (CONST_AUTHENTICATION_NOT_USED=1 if authentication mode is not enabled or
  // CONST_AUTHENTICATION_SESSION_NOT_STARTED=0 if the session not started yet) -
  // code may use SessionGetUser() method to retrieve the user details
  // - Ctxt.Method will indicate the used HTTP verb (e.g. GET/POST/PUT..)
  // - if process succeeded, implementation shall call Ctxt.Results([]) method to
  // set a JSON response object with one "result" field name or Ctxt.Returns([])
  // with a JSON object described in Name/Value pairs; if the returned value is
  // not JSON_CONTENT_TYPE, use Ctxt.Returns() and its optional CustomHeader
  // parameter can specify a custom header like TEXT_CONTENT_TYPE_HEADER
  // - if process succeeded, and no data is expected to be returned to the caller,
  // implementation shall call overloaded Ctxt.Success() method with the
  // expected status (i.e. just Ctxt.Success will return HTTP_SUCCESS)
  // - if process failed, implementation shall call Ctxt.Error() method to
  // set the corresponding error message and error code number
  // - a typical implementation may be:
  // ! procedure TRestServerTest.Sum(Ctxt: TRestServerUriContext);
  // ! begin
  // !   Ctxt.Results([Ctxt.InputDouble['a'] + Ctxt.InputDouble['b']]);
  // ! end;
  // - Client-Side can be implemented as you wish. By convention, it could be
  // appropriate to define in either TRestServer (if to be called as
  // ModelRoot/MethodName), either TOrm (if to be called as
  // ModelRoot/TableName[/TableID]/MethodName) a custom public or protected method,
  // calling TRestClientUri.URL with the appropriate parameters, and named
  // (by convention) as MethodName; TRestClientUri has dedicated methods
  // like CallBackGetResult, CallBackGet, CallBackPut and CallBack; see also
  // TOrmModel.getURICallBack and JsonDecode function
  // ! function TOrmPeople.Sum(aClient: TRestClientUri; a, b: double): double;
  // ! var err: integer;
  // ! begin
  // !   val(aClient.CallBackGetResult('sum', ['a', a, 'b', b]), result, err);
  // ! end;
  TOnRestServerCallBack = procedure(Ctxt: TRestServerUriContext) of object;

  /// description of a method-based service
  TRestServerMethod = record
    /// the method name
    Name: RawUtf8;
    /// the event which will be executed for this method
    CallBack: TOnRestServerCallBack;
    /// set to TRUE disable Authentication check for this method
    // - use TRestServer.ServiceMethodByPassAuthentication() method
    ByPassAuthentication: boolean;
    /// the allowed HTTP methods on this service
    Methods: TUriMethods;
    /// detailed statistics associated with this method
    Stats: TSynMonitorInputOutput;
  end;

  /// used to store all method-based services of a TRestServer instance
  TRestServerMethods = array of TRestServerMethod;


{ ************ TRestServerRoutingRest/TRestServerRoutingRest Requests Parsing Scheme }

  /// calling context for a TOnRestServerCallBack using JSON/RPC for
  // interface-based services
  // - see also TRestClientRoutingRest alternative (and default) class
  // - in this routing scheme, the URI will define the interface, then the
  // method name will be inlined with parameters, e.g.
  // $ POST /root/Calculator
  // $ (...)
  // $ {"method":"Add","params":[1,2]}
  // or, for a sicClientDriven mode service:
  // $ POST /root/ComplexNumber
  // $ (...)
  // $ {"method":"Add","params":[20,30],"id":1234}
  TRestServerRoutingJsonRpc = class(TRestServerUriContext)
  protected
    /// register the interface-based SOA URIs to Server.Router multiplexer
    // - this overridden implementation register '/Model/Interface' URI
    class procedure UriComputeRoutes(Router: TRestRouter;
      Server: TRestServer); override;
    /// direct launch of an interface-based service with URI JSON/RPC routing
    // - Uri() will ensure that Service<>nil before calling it
    // - this overridden implementation expects parameters to be sent as part
    // of a JSON object body:
    // $ {"method":"Add","params":[20,30],"id":1234}
    procedure ExecuteSoaByInterface; override;
  public
    /// the associated routing class on the client side
    // - this overriden method returns TRestClientRoutingJsonRpc
    class function ClientRouting: TRestClientRoutingClass; override;
  end;

  /// calling context for a TOnRestServerCallBack using our default simple REST
  // routing for interface-based services
  // - see also TRestClientRoutingJsonRpc alternative class
  // - this class will use RESTful routing for interface-based services:
  // method name will be identified within the URI, as
  // $ /Model/Interface.Method[/ClientDrivenID]
  // e.g. for ICalculator.Add:
  // $ POST /root/Calculator.Add
  // $ (...)
  // $ [1,2]
  // or, for a sicClientDriven mode service:
  // $ POST /root/ComplexNumber.Add/1234
  // $ (...)
  // $ [20,30]
  // in this case, the sent content will be a JSON array of [parameters...]
  // - as an alternative, input parameters may be encoded at URI level (with
  // a size limit depending on the HTTP routers, whereas there is no such
  // limitation when they are transmitted as message body)
  // - one benefit of having .../ClientDrivenID encoded at URI is that it will
  // be more secured in our RESTful authentication scheme: each method and even
  // client driven session will be signed individualy
  TRestServerRoutingRest = class(TRestServerUriContext)
  protected
    /// encode fInput[] as a JSON array for regular execution
    procedure DecodeUriParametersIntoJson;
    /// register the interface-based SOA URIs to Server.Router multiplexer
    // - this overridden implementation register URI encoded as
    // '/Model/Interface.Method[/ClientDrivenID]' for this class
    class procedure UriComputeRoutes(Router: TRestRouter;
      Server: TRestServer); override;
    /// direct run of an interface-based service with our URI RESTful routing
    // - this overridden implementation expects parameters to be sent as one JSON
    // array body (Delphi/AJAX way) or optionally with URI decoding (HTML way):
    // ! function TServiceCalculator.Add(n1, n2: integer): integer;
    // will accept such requests:
    // !  URL='root/Calculator.Add' and InBody='[ 1,2 ]'
    // !  URL='root/Calculator.Add?+%5B+1%2C2+%5D' // decoded as ' [ 1,2 ]'
    // !  URL='root/Calculator.Add?n1=1&n2=2'      // in any order, even missing
    procedure ExecuteSoaByInterface; override;
  public
    /// the associated routing class on the client side
    // - this overriden method returns TRestClientRoutingRest
    class function ClientRouting: TRestClientRoutingClass; override;
  end;

  /// class used to define the Server side expected routing
  // - match TRestClientRoutingClass reciprocal meta-class
  // - most of the internal methods are declared as virtual, so it allows any
  // kind of custom routing or execution scheme
  // - TRestServerRoutingJsonRpc and TRestServerRoutingRest classes
  // are provided in this unit, to allow RESTful and JSON/RPC protocols
  TRestServerUriContextClass = class of TRestServerUriContext;


{ ************ TAuthSession for In-Memory User Sessions }

  /// used for efficient TAuthSession.IDCardinal comparison
  TAuthSessionParent = class(TSynPersistent)
  protected
    fID: cardinal;
  end;

  /// class used to maintain in-memory sessions
  // - this is not a TOrm table so won't be remotely accessible, for
  // performance and security reasons
  // - the User field is a true instance, copy of the corresponding database
  // content (for better speed)
  // - you can inherit from this class, to add custom session process
  TAuthSession = class(TAuthSessionParent)
  protected
    fUser: TAuthUser;
    fTimeOutTix: cardinal;
    fTimeOutShr10: cardinal;
    fPrivateKey: RawUtf8;
    fPrivateSalt: RawUtf8;
    fSentHeaders: RawUtf8;
    fRemoteIP: RawUtf8;
    fConnectionID: TRestConnectionID;
    fPrivateSaltHash: cardinal;
    fLastTimestamp: cardinal; // client-side generated timestamp
    fExpectedHttpAuthentication: RawUtf8;
    fAccessRights: TOrmAccessRights;
    fMethods: TSynMonitorInputOutputObjArray;
    fInterfaces: TSynMonitorInputOutputObjArray;
    fRemoteOsVersion: TOperatingSystemVersion;
    fConnectionOpaque: PRestServerConnectionOpaque;
    function GetUserName: RawUtf8;
    function GetUserID: TID;
    function GetGroupID: TID;
    function GetRemoteOS: RawUtf8;
    procedure SaveTo(W: TBufferWriter); virtual;
    procedure ComputeProtectedValues(tix: Int64); virtual;
  public
    /// initialize a session instance with the supplied TAuthUser instance
    // - this aUser instance will be handled by the class until Destroy
    // - raise an exception on any error
    // - on success, will also retrieve the aUser.Data BLOB field content
    constructor Create(aCtxt: TRestServerUriContext;
      aUser: TAuthUser); reintroduce; virtual;
    /// initialize a session instance from some persisted buffer
    // - following the TRestServer.SessionsSaveToFile binary layout
    constructor CreateFrom(var Read: TFastReader; Server: TRestServer;
      tix: Int64); virtual;
    /// will release the User and User.GroupRights instances
    destructor Destroy; override;
    /// update the Interfaces[] statistics
    procedure NotifyInterfaces(aCtxt: TRestServerUriContext; aElapsed: Int64);
  public
    /// the associated User
    // - this is a true TAuthUser instance, and User.GroupRights will contain
    // also a true TAuthGroup instance
    property User: TAuthUser
      read fUser;
    /// set by the Access() method to the current GetTickCount64 shr 10
    // timestamp + TimeoutSecs
    property TimeOutTix: cardinal
      read fTimeOutTix;
    /// copy of the associated user access rights
    // - extracted from User.TAuthGroup.SqlAccessRights
    property AccessRights: TOrmAccessRights
      read fAccessRights;
    /// the hexadecimal private key as returned to the connected client
    // as 'SessionID+PrivateKey'
    property PrivateKey: RawUtf8
      read fPrivateKey;
    /// the transmitted HTTP headers, if any
    // - can contain e.g. 'RemoteIp: 127.0.0.1' or 'User-Agent: Mozilla/4.0'
    property SentHeaders: RawUtf8
      read fSentHeaders;
    /// per-session statistics about method-based services
    // - Methods[] follows TRestServer.fPublishedMethod[] array
    // - is initialized and maintained only if mlSessions is defined in
    // TRestServer.StatLevels property
    property Methods: TSynMonitorInputOutputObjArray
      read fMethods;
    /// per-session statistics about interface-based services
    // - Interfaces[] follows TRestServer.Services.fListInterfaceMethod[] array
    // - is initialized and maintained only if mlSessions is defined in
    // TRestServer.StatLevels property
    property Interfaces: TSynMonitorInputOutputObjArray
      read fInterfaces write fInterfaces;
    /// the Client Operating System
    // - as extracted from 'clientnonce' by TRestServerAuthenticationDefault.Auth
    property RemoteOsVersion: TOperatingSystemVersion
      read fRemoteOsVersion;
  published
    /// the session ID number, as numerical value
    // - never equals to 1 (CONST_AUTHENTICATION_NOT_USED, i.e. authentication
    // mode is not enabled), nor 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED,
    // i.e. session still in handshaking phase)
    property ID: cardinal
      read fID;
    /// the associated User Name, as in User.LogonName
    property UserName: RawUtf8
      read GetUserName;
    /// the associated User ID, as in User.ID
    property UserID: TID
      read GetUserID;
    /// the associated Group ID, as in User.GroupRights.ID
    property GroupID: TID
      read GetGroupID;
    /// the timestamp (in numbers of 1024 ms) until a session is kept alive
    // - extracted from User.TAuthGroup.SessionTimeout
    // - is used for fast comparison with GetTickCount64 shr 10
    property TimeoutShr10: cardinal
      read fTimeOutShr10;
    /// the low-level ConnectionID of the connection initiating this session
    property ConnectionID: TRestConnectionID
      read fConnectionID;
    /// the remote IP, if any
    // - is extracted from SentHeaders properties
    property RemoteIP: RawUtf8
      read fRemoteIP;
    /// the client Operating System, if sent from a mORMot 2 client
    // - is extracted from 'clientnonce' by TRestServerAuthenticationDefault.Auth
    // into RemoteOsVersion 32-bit flags
    // - returns e.g. 'Windows 11 64-bit 22000' or 'Debian 5.4.0'
    property RemoteOS: RawUtf8
      read GetRemoteOS;
  end;
  PAuthSession = ^TAuthSession;

  /// class-reference type (metaclass) used to define overridden session instances
  // - since all sessions data remain in memory, ensure they are not taking too
  // much resource (memory or process time)
  // - if you plan to use session persistence, ensure you override the
  // TAuthSession.SaveTo/CreateFrom methods in the inherited class
  TAuthSessionClass = class of TAuthSession;


{ ************ TRestServerAuthentication Implementing Authentication Schemes }

  /// optional behavior of TRestServerAuthentication class
  // - by default, saoUserByLogonOrID is set, allowing
  // TRestServerAuthentication.GetUser() to retrieve the TAuthUser by
  // logon name or by ID, if the supplied logon name is an integer
  // - if saoHandleUnknownLogonAsStar is defined, any user successfully
  // authenticated could be logged with the same ID (and authorization)
  // than TAuthUser.Logon='*' - of course, this is meaningfull only with
  // an external credential check (e.g. via SSPI or Active Directory)
  TRestServerAuthenticationOption = (
    saoUserByLogonOrID,
    saoHandleUnknownLogonAsStar);

  /// defines the optional behavior of TRestServerAuthentication class
  TRestServerAuthenticationOptions =
    set of TRestServerAuthenticationOption;

  /// abstract class used to implement server-side authentication in TRestServer
  // - inherit from this class to implement expected authentication scheme
  // - each TRestServerAuthentication class is associated with a
  // TRestClientAuthentication class from mormot.rest.client.pas
  TRestServerAuthentication = class(TSynLocked)
  protected
    fServer: TRestServer;
    fOptions: TRestServerAuthenticationOptions;
    fAlgoName: RawUtf8;
    // GET ModelRoot/auth?UserName=...&Session=... -> release session
    function AuthSessionRelease(Ctxt: TRestServerUriContext): boolean;
    /// retrieve an User instance from its logon name
    // - should return nil if not found
    // - this default implementation will retrieve it from ORM, and
    // call TAuthUser.CanUserLog() to ensure authentication is allowed
    // - if aUserName is an integer, it will try to retrieve it from ORM using
    // the supplied value as its TAuthUser.ID: it may be convenient when the
    // client is not an end-user application but a mORMot server (in a cloud
    // architecture), since it will benefit from local ORM cache
    // - you can override this method and return an on-the-fly created value
    // as a TRestServer.AuthUserClass instance (i.e. not persisted
    // in database nor retrieved by ORM), but the resulting TAuthUser
    // must have its ID and LogonName properties set with unique values (which
    // will be used to identify it for a later call and session owner
    // identification), and its GroupRights property must not yet contain a real
    // TAuthGroup instance, just a TAuthGroup(aGroupID) value (as directly
    // retrieved from the ORM) - TAuthSession.Create will retrieve the instance
    // - another possibility, orthogonal to all TRestServerAuthentication
    // classes, may be to define a TRestServer.OnAuthenticationUserRetrieve
    // custom event
    function GetUser(Ctxt: TRestServerUriContext;
      const aUserName: RawUtf8): TAuthUser; virtual;
    /// create a session on the server for a given user
    // - this default implementation will call fServer.SessionCreate() and
    // return a '{"result":"HEXASALT","logonname":"UserName"}' JSON content
    // and will always call User.Free
    // - on failure, will call TRestServerUriContext.AuthenticationFailed()
    // with afSessionAlreadyStartedForThisUser or afSessionCreationAborted reason
    procedure SessionCreate(Ctxt: TRestServerUriContext;
      var User: TAuthUser); virtual;
    /// Ctxt.Returns(['result',result,....[,'data',data]],200,header);
    procedure SessionCreateReturns(Ctxt: TRestServerUriContext;
      Session: TAuthSession; const result, data, header: RawUtf8);
  public
    /// initialize the authentication method to a specified server
    // - you can define several authentication schemes for the same server
    constructor Create(aServer: TRestServer); reintroduce; virtual;
    /// called by the Server to implement the Auth RESTful method
    // - overridden method shall return TRUE if the request has been handled
    // - returns FALSE to let the next registered TRestServerAuthentication
    // class to try implementing the content
    // - Ctxt.Parameters has been tested to contain an UserName=... value
    // - method execution is protected by TRestServer.Sessions.WriteLock
    function Auth(Ctxt: TRestServerUriContext): boolean; virtual; abstract;
    /// called by the Server to check if the execution context match a session
    // - returns a session instance corresponding to the remote request, and
    // fill Ctxt.Session* members according to in-memory session information
    // - returns nil if this remote request does not match this authentication
    // - method execution is protected by TRestServer.Sessions.Safe
    function RetrieveSession(
      Ctxt: TRestServerUriContext): TAuthSession; virtual; abstract;
    /// allow to tune the authentication process
    // - default value is [saoUserByLogonOrID]
    property Options: TRestServerAuthenticationOptions
      read fOptions write fOptions;
  end;

  /// weak authentication scheme using URL-level parameter
  TRestServerAuthenticationUri = class(TRestServerAuthentication)
  public
    /// will check URI-level signature
    // - retrieve the session ID from 'session_signature=...' parameter
    // - method execution is protected by TRestServer.Sessions.ReadOnlyLock
    function RetrieveSession(
      Ctxt: TRestServerUriContext): TAuthSession; override;
  end;

  /// secure authentication scheme using URL-level digital signature
  // - match TRestClientAuthenticationSignedUri on Client side
  // - for instance, default suaCRC32 format of session_signature is
  // !Hexa8(SessionID)+
  // !Hexa8(Timestamp)+
  // !Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
  // !            Hexa8(Timestamp)+url))
  TRestServerAuthenticationSignedUri = class(TRestServerAuthenticationUri)
  protected
    fNoTimestampCoherencyCheck: boolean;
    fTimestampCoherencySeconds: cardinal;
    fTimestampCoherencyTicks: cardinal;
    fComputeSignature: TOnRestAuthenticationSignedUriComputeSignature;
    procedure SetNoTimestampCoherencyCheck(value: boolean);
    procedure SetTimestampCoherencySeconds(value: cardinal);
    procedure SetAlgorithm(value: TRestAuthenticationSignedUriAlgo);
  public
    /// initialize the authentication method to a specified server
    constructor Create(aServer: TRestServer); override;
    /// will check URI-level signature
    // - check session_signature=... parameter to be a valid digital signature
    // - method execution is protected by TRestServer.Sessions.ReadOnlyLock
    function RetrieveSession(
      Ctxt: TRestServerUriContext): TAuthSession; override;
    /// allow any order when creating sessions
    // - by default, signed sessions are expected to be sequential, and new
    // signed session signature can't be older in time than the last one,
    // with a tolerance of TimestampCoherencySeconds
    // - but if your client is asynchronous (e.g. for AJAX requests), session
    // may be rejected due to the delay involved on the client side: you can set
    // this property to TRUE to enabled a weaker but more tolerant behavior
    // ! (aServer.AuthenticationRegister(TRestServerAuthenticationDefault) as
    // !   TRestServerAuthenticationSignedUri).NoTimestampCoherencyCheck := true;
    property NoTimestampCoherencyCheck: boolean
      read fNoTimestampCoherencyCheck write SetNoTimestampCoherencyCheck;
    /// time tolerance in seconds for the signature timestamps coherency check
    // - by default, signed sessions are expected to be sequential, and new
    // signed session signature can't be older in time than the last one,
    // with a tolerance time defined by this property
    // - default value is 5 seconds, which cover most kind of clients (AJAX or
    // WebSockets), even over a slow Internet connection
    property TimestampCoherencySeconds: cardinal
      read fTimestampCoherencySeconds write SetTimestampCoherencySeconds;
    /// customize the session_signature signing algorithm with a specific function
    // - the very same function should be set on TRestClientUri
    // - to select a known hash algorithm, you may change the Algorithm property
    property ComputeSignature: TOnRestAuthenticationSignedUriComputeSignature
      read fComputeSignature write fComputeSignature;
    /// customize the session_signature signing algorithm
    // - you need to set this value on the server side only; those known algorithms
    // will be recognized by TRestClientUri on the client side during the
    // session handshake, to select the matching ComputeSignature function
    property Algorithm: TRestAuthenticationSignedUriAlgo
      write SetAlgorithm;
  end;

  /// mORMot secure RESTful authentication scheme on Server
  // - match TRestClientAuthenticationDefault on Client side
  // - this method will use a password stored via safe SHA-256 hashing in the
  // TAuthUser ORM table
  TRestServerAuthenticationDefault = class(TRestServerAuthenticationSignedUri)
  protected
    /// check a supplied password content
    // - will match ClientComputeSessionKey() algorithm as overridden here, i.e.
    // a SHA-256 based signature with a 10 minutes activation window
    function CheckPassword(Ctxt: TRestServerUriContext;
      User: TAuthUser; const aClientNonce, aPassWord: RawUtf8): boolean; virtual;
  public
    /// will try to handle the Auth RESTful method with mORMot authentication
    // - to be called in a two pass "challenging" algorithm:
    // $ GET ModelRoot/auth?UserName=...
    // $  -> returns an hexadecimal nonce contents (valid for 5 minutes)
    // $ GET ModelRoot/auth?UserName=...&PassWord=...&ClientNonce=...
    // $ -> if password is OK, will open the corresponding session
    // $    and return 'SessionID+HexaSessionPrivateKey'
    // The Password parameter as sent for the 2nd request will be computed as
    // ! Sha256(ModelRoot+Nonce+ClientNonce+UserName+Sha256('salt'+PassWord))
    // - the returned HexaSessionPrivateKey content will identify the current
    // user logged and its corresponding session (the same user may have several
    // sessions opened at once, each with its own private key)
    // - then the private session key must be added to every query sent to
    // the server as a session_signature=???? parameter, which will be computed
    // as such:
    // $ ModelRoot/url?A=1&B=2&session_signature=012345670123456701234567
    // were the session_signature= parameter will be computed as such:
    // ! Hexa8(SessionID)+Hexa8(Timestamp)+
    // ! Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
    // !  Hexa8(Timestamp)+url))
    // ! with url='ModelRoot/url?A=1&B=2'
    // this query authentication uses crc32 for hashing instead of SHA-256 in
    // in order to lower the Server-side CPU consumption; the salted password
    // (i.e. TAuthUser.PasswordHashHexa) and client-side Timestamp are
    // inserted inside the session_signature calculation to prevent naive
    // man-in-the-middle attack (MITM)
    // - the session ID will be used to retrieve the rights associated with the
    // user which opened the session via a successful call to the Auth service
    // - when you don't need the session any more (e.g. if the TRestClientUri
    // instance is destroyed), you can call the service as such:
    // $ GET ModelRoot/auth?UserName=...&Session=...
    // - for a way of computing SHA-256 in JavaScript, see for instance
    // @http://www.webtoolkit.info/javascript-sha256.html
    function Auth(Ctxt: TRestServerUriContext): boolean; override;
  end;

  /// mORMot weak RESTful authentication scheme
  // - match TRestClientAuthenticationNone on Client side
  // - this method will authenticate with a given username, but no signature
  // - on client side, this scheme is not called by TRestClientUri.SetUser()
  // method - so you have to write:
  // ! TRestServerAuthenticationNone.ClientSetUser(Client,'User','');
  TRestServerAuthenticationNone = class(TRestServerAuthenticationUri)
  public
    /// will try to handle the Auth RESTful method with mORMot authentication
    // - to be called in a weak one pass request:
    // $ GET ModelRoot/auth?UserName=...
    // $ -> if the specified user name exists, will open the corresponding
    // $    session and return 'SessionID+HexaSessionPrivateKey'
    function Auth(Ctxt: TRestServerUriContext): boolean; override;
  end;

  /// abstract class for implementing HTTP authentication
  // - do not use this abstract class, but e.g. TRestServerAuthenticationHttpBasic
  // - this class will transmit the session_signature as HTTP cookie, not at
  // URI level, so is expected to be used only from browsers or old clients
  TRestServerAuthenticationHttpAbstract = class(TRestServerAuthentication)
  protected
    /// should be overriden according to the HTTP authentication scheme
    class function ComputeAuthenticateHeader(
      const aUserName, aPasswordClear: RawUtf8): RawUtf8; virtual; abstract;
  public
    /// will check the caller signature
    // - retrieve the session ID from "Cookie: mORMot_session_signature=..." HTTP header
    // - method execution is protected by TRestServer.Sessions.ReadOnlyLock
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
  end;

  /// authentication using HTTP Basic scheme
  // - match TRestClientAuthenticationHttpBasic on Client side
  // - this protocol send both name and password as clear (just Base64 encoded)
  // so should only be used over TLS / HTTPS, or for compatibility reasons
  // - will rely on TRestServerAuthenticationNone for authorization
  // - on client side, this scheme is not called by TRestClientUri.SetUser()
  // method - so you have to write:
  // ! TRestServerAuthenticationHttpBasic.ClientSetUser(Client,'User','password');
  // - for a remote proxy-only authentication (without creating any mORMot
  // session), you can write:
  // ! TRestServerAuthenticationHttpBasic.ClientSetUserHttpOnly(Client,'proxyUser','proxyPass');
  TRestServerAuthenticationHttpBasic = class(TRestServerAuthenticationHttpAbstract)
  protected
    /// this overriden method returns "Authorization: Basic ...." HTTP header
    class function ComputeAuthenticateHeader(
      const aUserName, aPasswordClear: RawUtf8): RawUtf8; override;
    /// decode "Authorization: Basic ...." header
    // - you could implement you own password transmission pattern, by
    // overriding both ComputeAuthenticateHeader and GetUserPassFromInHead methods
    class function GetUserPassFromInHead(Ctxt: TRestServerUriContext;
      out userPass, user, pass: RawUtf8): boolean; virtual;
    /// check a supplied password content
    // - this default implementation will use the SHA-256 hash value stored
    // within User.PasswordHashHexa
    // - you can override this method to provide your own password check
    // mechanism, for the given TAuthUser instance
    function CheckPassword(Ctxt: TRestServerUriContext;
      User: TAuthUser; const aPassWord: RawUtf8): boolean; virtual;
  public
    /// will check URI-level signature
    // - retrieve the session ID from 'session_signature=...' parameter
    // - will also check incoming "Authorization: Basic ...." HTTP header
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
    /// handle the Auth RESTful method with HTTP Basic
    // - will first return HTTP_UNAUTHORIZED (401), then expect user and password
    // to be supplied as incoming "Authorization: Basic ...." headers
    function Auth(Ctxt: TRestServerUriContext): boolean; override;
  end;

  {$ifdef DOMAINRESTAUTH}
  { will use mormot.lib.sspi/gssapi units depending on the OS }

  /// authentication of the current logged user using Windows Security Support
  // Provider Interface (SSPI) or the GSSAPI library on Linux
  // - is able to authenticate the currently logged user on the client side,
  // using either NTLM (Windows only) or Kerberos - it will allow to safely
  // authenticate on a mORMot server without prompting the user to enter its
  // password
  // - if ClientSetUser() receives aUserName as '', aPassword should be either
  // '' if you expect NTLM authentication to take place, or contain the SPN
  // registration (e.g. 'mymormotservice/myserver.mydomain.tld') for Kerberos
  // authentication
  // - if ClientSetUser() receives aUserName as 'DomainName\UserName', then
  // authentication will take place on the specified domain, with aPassword
  // as plain password value
  // - this class is not available on some targets (e.g. Android)
  TRestServerAuthenticationSspi = class(TRestServerAuthenticationSignedUri)
  protected
    /// Windows built-in authentication
    // - holds information between calls to ServerSspiAuth()
    // - access to this array is made thread-safe thanks to Safe.Lock/Unlock
    fSspiAuthContext: TSecContextDynArray;
    fSspiAuthContexts: TDynArray;
    fSspiAuthContextCount: integer;
  public
    /// initialize this SSPI/GSSAPI authentication scheme
    constructor Create(aServer: TRestServer); override;
    /// finalize internal sspi/gssapi allocated structures
    destructor Destroy; override;
    /// will try to handle the RESTful authentication via SSPI/GSSAPI
    // - to be called in a two pass algorithm, used to cypher the password
    // - the client-side logged user will be identified as valid, according
    // to a Windows SSPI API secure challenge
    function Auth(Ctxt: TRestServerUriContext): boolean; override;
  end;

  {$endif DOMAINRESTAUTH}



{ ************ TRestServerMonitor for High-Level Statistics of a REST Server }

  /// how TRestServer should maintain its statistical information
  // - used by TRestServer.StatLevels property
  TRestServerMonitorLevels = set of (
    mlUri,
    mlTables,
    mlMethods,
    mlInterfaces,
    mlSessions,
    mlSQLite3);

  /// used for high-level statistics in TRestServer.Uri()
  TRestServerMonitor = class(TSynMonitorServer)
  protected
    fServer: TRestServer;
    fStartDate: RawUtf8;
    fCurrentThreadCount: TSynMonitorOneCount;
    fSuccess: TSynMonitorCount64;
    fOutcomingFiles: TSynMonitorCount64;
    fServiceMethod: TSynMonitorCount64;
    fServiceInterface: TSynMonitorCount64;
    fCreated: TSynMonitorCount64;
    fRead: TSynMonitorCount64;
    fUpdated: TSynMonitorCount64;
    fDeleted: TSynMonitorCount64;
    // [Write: boolean] per-table statistics
    fPerTable: array[boolean] of TSynMonitorWithSizeObjArray;
    // no overriden Changed: TRestServer.Uri will do it in finally block
    function CreateNotifyOrmTable(TableIndex: PtrInt; Write: boolean): TSynMonitorWithSize;
    procedure CreateNotifyAuthSession(var Methods: TSynMonitorInputOutputObjArray);
    procedure CreateNotify(var Monitor: TSynMonitorInputOutput; const Name: RawUtf8);
  public
    /// initialize the instance
    constructor Create(aServer: TRestServer); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// should be called when a task successfully ended
    // - thread-safe method
    procedure ProcessSuccess(IsOutcomingFile: boolean);
      {$ifdef HASINLINE} inline; {$endif}
    /// update and returns the CurrentThreadCount property
    // - this method is thread-safe
    function NotifyThreadCount(delta: integer): integer;
    /// update the Created/Read/Updated/Deleted properties
    // - this method is thread-safe
    procedure NotifyOrm(aMethod: TUriMethod);
    /// update the per-table statistics
    // - this method is thread-safe
    procedure NotifyOrmTable(TableIndex, DataSize: integer; Write: boolean;
      const MicroSecondsElapsed: QWord);
  published
    /// when this monitoring instance (therefore the server) was created
    property StartDate: RawUtf8
      read fStartDate;
    /// number of valid responses
    // - i.e. which returned status code 200/HTTP_SUCCESS or 201/HTTP_CREATED
    // - any invalid request will increase the TSynMonitor.Errors property
    property Success: TSynMonitorCount64
      read fSuccess;
    /// count of the remote method-based service calls
    property ServiceMethod: TSynMonitorCount64
      read fServiceMethod;
    /// count of the remote interface-based service calls
    property ServiceInterface: TSynMonitorCount64
      read fServiceInterface;
    /// count of files transmitted directly (not part of Output size property)
    // - i.e. when the service uses STATICFILE_CONTENT_TYPE/HTTP_RESP_STATICFILE
    // as content type to let the HTTP server directly serve the file content
    property OutcomingFiles: TSynMonitorCount64
      read fOutcomingFiles;
    /// number of current declared thread counts
    // - as registered by BeginCurrentThread/EndCurrentThread
    property CurrentThreadCount: TSynMonitorOneCount
      read fCurrentThreadCount;
    /// how many Create / Add ORM operations did take place
    property Created: TSynMonitorCount64
      read fCreated;
    /// how many Read / Get ORM operations did take place
    property Read: TSynMonitorCount64
      read fRead;
    /// how many Update ORM operations did take place
    property Updated: TSynMonitorCount64
      read fUpdated;
    /// how many Delete ORM operations did take place
    property Deleted: TSynMonitorCount64
      read fDeleted;
  end;

  /// ORM table used to store TSynMonitorUsage information in TSynMonitorUsageRest
  // - the ID primary field is the TSynMonitorUsageID (accessible from UsageID
  // public property) shifted by 16-bit (by default) to include a
  // TSynUniqueIdentifierProcess value
  TOrmMonitorUsage = class(TOrmNoCaseExtended)
  protected
    fGran: TSynMonitorUsageGranularity;
    fProcess: Int64;
    fInfo: variant;
    fComment: RawUtf8;
  public
    /// compute the corresponding 23 bit TSynMonitorUsageID.Value time slice
    // - according to the stored Process field, after bit shift
    // - allows a custom aProcessIDShift if it is not set as default 16-bit
    function UsageID(aProcessIDShift: integer = 16): integer;
  published
    /// the granularity of the statistics of this entry
    property Gran: TSynMonitorUsageGranularity
      read fGran write fGran;
    /// identify which application is monitored
    // - match the lower bits of each record ID
    // - by default, is expected to be a TSynUniqueIdentifierProcess 16-bit value
    property Process: Int64
      read fProcess write fProcess;
    /// the actual statistics information, stored as a TDocVariant JSON object
    property Info: variant
      read fInfo write fInfo;
    /// a custom text, which may be used e.g. by support or developpers
    property Comment: RawUtf8
      read fComment write fComment;
  end;
  /// class-reference type (metaclass) of a TOrmMonitorUsage table
  TOrmMonitorUsageClass = class of TOrmMonitorUsage;

  /// will store TSynMonitorUsage information in TOrmMonitorUsage ORM tables
  // - TOrm.ID will be the TSynMonitorUsageID shifted by ProcessIDShift bits
  TSynMonitorUsageRest = class(TSynMonitorUsage)
  protected
    fStorage: IRestOrm;
    fProcessID: Int64;
    fProcessIDShift: integer;
    fStoredClass: TOrmMonitorUsageClass;
    fStoredCache: array[mugHour..mugYear] of TOrmMonitorUsage;
    fSaveBatch: TRestBatch;
    function SaveDB(ID: integer; const Track: variant;
      Gran: TSynMonitorUsageGranularity): boolean; override;
    function LoadDB(ID: integer; Gran: TSynMonitorUsageGranularity;
      out Track: variant): boolean; override;
  public
    /// initialize storage via ORM
    // - if a 16-bit TSynUniqueIdentifierProcess is supplied, it will be used to
    // identify the generating process by shifting TSynMonitorUsageID values
    // by aProcessIDShift bits (default 16 but you may increase it up to 40 bits)
    // - will use TOrmMonitorUsage table, unless another one is specified
    constructor Create(const aStorage: IRestOrm; aProcessID: Int64;
      aStoredClass: TOrmMonitorUsageClass = nil;
      aProcessIDShift: integer = 16); reintroduce; virtual;
    /// finalize the process, saving pending changes
    destructor Destroy; override;
    /// you can set an optional Batch instance to speed up DB writing
    // - when calling the Modified() method
    property SaveBatch: TRestBatch
      read fSaveBatch write fSaveBatch;
  published
    /// the actual ORM class used for persistence
    property StoredClass: TOrmMonitorUsageClass
      read fStoredClass;
    /// how the information could be stored for several processes
    // - e.g. when several SOA nodes gather monitoring information in a
    // shared (MongoDB) database
    // - is by default a TSynUniqueIdentifierProcess value, but may be
    // any integer up to ProcessIDShift bits as set in Create()
    property ProcessID: Int64
      read fProcessID;
    /// how process ID are stored within the mORMot TOrm.ID
    // - equals 16-bit by default, to match TSynUniqueIdentifierProcess resolution
    property ProcessIDShift: integer
      read fProcessIDShift;
  end;

  /// the flags used for TRestServer.AddStats
  TRestServerAddStat = (
    withTables,
    withMethods,
    withInterfaces,
    withSessions);

  /// some flags used for TRestServer.AddStats
  TRestServerAddStats = set of TRestServerAddStat;


{ ************ TRestRouter for efficient Radix Tree based URI Multiplexing }

  /// context information, as cloned by TRestTreeNode.Split()
  TRestTreeNodeData = record
    /// which kind of execution this node is about
    Node: TRestNode;
    /// the ORM/SOA TRestServer execution context
    Command: TRestServerUriContextCommand;
    /// the index of the associated method (16-bit is enough, -1 for none)
    // - for rn*Method*, in TRestServer.PublishedMethod[]
    // - for rnInterface*, in Service.InterfaceFactory.Methods[MethodIndex-4] or
    // 0..3 for internal _free_/_contract_/_signature_/_instance_ pseudo-methods
    MethodIndex: {$ifdef CPU32} SmallInt {$else} integer {$endif};
    /// the properties of the ORM class in the server TOrmModel for rnTable*
    Table: TOrmModelProperties;
    /// the ORM BLOB field definition for rnTableIDBlob
    Blob: TOrmPropInfoRttiRawBlob;
    /// the interface-based service for rnInterface
    Service: TServiceFactoryServerAbstract;
  end;

  /// implement a Radix Tree node to hold one URI registration
  TRestTreeNode = class(TRadixTreeNodeParams)
  protected
    function LookupParam(Ctxt: TObject; Pos: PUtf8Char; Len: integer): boolean; override;
  public
    /// all context information, as cloned by Split()
    Data: TRestTreeNodeData;
    /// overriden to support the additional Data fields
    function Split(const Text: RawUtf8): TRadixTreeNode; override;
  end;

  /// exception class raised during TRestTree URI parsing
  ERestTree = class(ERadixTree);

  /// Radix Tree to hold all registered REST URI for a given HTTP method
  TRestTree = class(TRadixTreeParams)
  protected
    procedure SetNodeClass; override; // use TRestTreeNode
  end;

  /// store per-method URI multiplexing Radix Tree in TRestRouter
  // - each HTTP method would have its dedicated TRestTree parser in TRestRouter
  TRestRouterTree = array[TUriMethod] of TRestTree;

  /// efficient server-side URI routing for TRestServer
  TRestRouter = class(TSynPersistent)
  protected
    fTree: TRestRouterTree;
    fOwner: TRestServer;
    fTreeCount: array[TUriMethod] of integer;
    fNodeCount: array[TRestNode] of integer;
  public
    /// initialize this URI routine engine
    constructor Create(aOwner: TRestServer); reintroduce;
    /// finalize this URI routing engine
    destructor Destroy; override;
    /// register a given URI to the tree, for a given HTTP method
    // - 'modelroot/' will be prefixed to the supplied aUri
    function Setup(aFrom: TUriMethod; const aUri: RawUtf8;
      aNode: TRestNode): TRestTreeNode; overload;
    /// register a given URI to the tree, for a given HTTP method
    // - 'modelroot/' will be prefixed to the supplied aUri
    procedure Setup(aFrom: TUriMethods; const aUri: RawUtf8; aNode: TRestNode;
      aTable: TOrmModelProperties; aBlob: TOrmPropInfoRttiRawBlob = nil;
      aMethodIndex: integer = -1; aService: TServiceFactory = nil); overload;
    /// quickly search for the node corresponding to Ctxt.Method and Uri
    // - should never raise an exception
    function Lookup(Ctxt: TRestServerUriContext): TRestTreeNode;
    /// used by ComputeRoutes to log the current route registration state
    function InfoText: RawUtf8;
    /// access to the internal per-method TRestTree instance
    // - some Tree[] may be nil if the HTTP method has not been registered yet
    property Tree: TRestRouterTree
      read fTree;
  end;


{ ************ TRestServer Abstract REST Server }

  /// some options for TRestServer process
  // - read-only rsoNoAjaxJson indicates that JSON data is transmitted in "not
  // expanded" format: you should NEVER change this option by including
  // this property in TRestServer.Options, but always call explicitly
  // TRestServer.NoAjaxJson := true so that the SetNoAjaxJson virtual
  // method should be called as expected (e.g. to flush TRestServerDB cache)
  // - rsoGetAsJsonNotAsString will let ORM GET return to AJAX (non Delphi)
  // clients JSON objects instead of the JSON text stored in database fields
  // - rsoGetID_str will add a "ID_str": string field to circumvent JavaScript
  // limitation of 53-bit for integers - only for AJAX (non Delphi) clients
  // - unauthenticated requests from browsers (i.e. not Delphi clients) may
  // be redirected to the TRestServer.Auth() method via rsoRedirectForbiddenToAuth
  // (e.g. for TRestServerAuthenticationHttpBasic popup)
  // - some REST/AJAX clients may expect to return status code 204 as
  // instead of 200 in case of a successful operation, but with no returned
  // body (e.g. a DELETE with SAPUI5 / OpenUI5 framework): include
  // rsoHttp200WithNoBodyReturns204 so that any HTTP_SUCCESS (200) with no
  // returned body will return a HTTP_NOCONTENT (204), as expected by
  // some clients
  // - by default, Add() or Update() will return HTTP_CREATED (201) or
  // HTTP_SUCCESS (200) with no body, unless rsoAddUpdateReturnsContent is set
  // to return as JSON the last inserted/updated record
  // - TModTime / TCreateTime fields are expected to be filled on client side,
  // unless you set rsoComputeFieldsBeforeWriteOnServerSide so that AJAX requests
  // will set the fields on the server side by calling the TOrm
  // ComputeFieldsBeforeWrite virtual method, before writing to the database
  // - rsoSecureConnectionRequired will ensure Call is flagged as llfSecured
  // (i.e. in-process, HTTPS, or encrypted WebSockets) - with the only exception
  // of the Timestamp method-based service (for monitoring purposes) - note that
  // this option doesn't make sense behind a proxy, just with a true HTTPS server
  // - by default, cookies will contain only 'Path=/Model.Root', but
  // '; Path=/' may be also added setting rsoCookieIncludeRootPath
  // - you can disable the 'HttpOnly' flag via rsoCookieHttpOnlyFlagDisable
  // - TRestServerUriContext.AuthenticationBearerToken will return the
  // ?authenticationbearer=... URI parameter value alternatively to the HTTP
  // header unless rsoAuthenticationUriDisable is set (for security reasons)
  // - you can switch off root/timestamp/info URI via rsoTimestampInfoUriDisable
  // - Uri() header output will be sanitized for any EOL injection, unless
  // rsoHttpHeaderCheckDisable is defined (to gain a few cycles)
  // - by default, TAuthUser.Data blob is retrieved from the database,
  // unless rsoGetUserRetrieveNoBlobData is defined
  // - rsoNoInternalState could be state to avoid transmitting the
  // 'Server-InternalState' header, e.g. if the clients wouldn't need it
  // - rsoNoTableURI will disable any /root/tablename URI for safety
  // - rsoMethodUnderscoreAsSlashUri will try to decode /root/method/name
  // as 'method_name' method
  // - rsoValidateUtf8Input will call IsValidUtf8() on input UTF-8 JSON/text,
  // to sanitize character encodings - with AVX2 this function is very fast so
  // this could be a good option if you don't trust your clients
  // - rsoSessionInConnectionOpaque uses LowLevelConnectionOpaque^.ValueInternal
  // to store the current TAuthSession - may be used with a lot of sessions
  TRestServerOption = (
    rsoNoAjaxJson,
    rsoGetAsJsonNotAsString,
    rsoGetID_str,
    rsoRedirectForbiddenToAuth,
    rsoHttp200WithNoBodyReturns204,
    rsoAddUpdateReturnsContent,
    rsoComputeFieldsBeforeWriteOnServerSide,
    rsoSecureConnectionRequired,
    rsoCookieIncludeRootPath,
    rsoCookieHttpOnlyFlagDisable,
    rsoAuthenticationUriDisable,
    rsoTimestampInfoUriDisable,
    rsoHttpHeaderCheckDisable,
    rsoGetUserRetrieveNoBlobData,
    rsoNoInternalState,
    rsoNoTableURI,
    rsoMethodUnderscoreAsSlashUri,
    rsoValidateUtf8Input,
    rsoSessionInConnectionOpaque);

  /// allow to customize the TRestServer process via its Options property
  TRestServerOptions = set of TRestServerOption;

  /// class-reference type (metaclass) used to define an authentication scheme
  TRestServerAuthenticationClass = class of TRestServerAuthentication;

  /// maintain a list of TRestServerAuthentication instances
  TRestServerAuthenticationDynArray = array of TRestServerAuthentication;

  /// structure used to specify custom request paging parameters for TRestServer
  // - default values are the one used for YUI component paging (i.e.
  // PAGINGPARAMETERS_YAHOO constant, as set by TRestServer.Create)
  // - warning: using paging can be VERY expensive on Server side, especially
  // when used with external databases (since all data is retrieved before
  // paging, when SQLite3 works in virtual mode)
  TRestServerUriPagingParameters = record
    /// parameter name used to specify the request sort order
    // - default value is 'SORT='
    // - note that any custom value should end with a '=' character
    Sort: RawUtf8;
    /// parameter name used to specify the request sort direction
    // - default value is 'DIR='
    // - note that any custom value should end with a '=' character
    Dir: RawUtf8;
    /// parameter name used to specify the request starting offset
    // - default value is 'STARTINDEX='
    // - note that any custom value should end with a '=' character
    StartIndex: RawUtf8;
    /// parameter name used to specify the request the page size (LIMIT clause)
    // - default value is 'RESULTS='
    // - note that any custom value should end with a '=' character
    Results: RawUtf8;
    /// parameter name used to specify the request field names
    // - default value is 'SELECT='
    // - note that any custom value should end with a '=' character
    Select: RawUtf8;
    /// parameter name used to specify the request WHERE clause
    // - default value is 'WHERE='
    // - note that any custom value should end with a '=' character
    Where: RawUtf8;
    /// returned JSON field value of optional total row counts
    // - default value is nil, i.e. no total row counts field
    // - computing total row counts can be very expensive, depending on the
    // database back-end used (especially for external databases)
    // - can be set e.g. to ',"totalRows":%' value (note that the initial "," is
    // expected by the produced JSON content, and % will be set with the value)
    SendTotalRowsCountFmt: RawUtf8;
  end;
  PRestServerUriPagingParameters = ^TRestServerUriPagingParameters;

  ///  used to define how to trigger Events on record update
  // - see TRestServer.OnUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occurred (but action must continue)
  // - to be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
  TOnOrmEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aSentData: RawUtf8): boolean of object;

  ///  used to define how to trigger Events on record field update
  // - see TRestServer.OnBlobUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occurred (but action must continue)
  // - to be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
  TOnOrmFieldEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aAffectedFields: TFieldBits): boolean of object;

  /// session-related callbacks triggered by TRestServer
  // - for OnSessionCreate, returning TRUE will abort the session creation -
  // and you can set Ctxt.Call^.OutStatus to a corresponding error code
  TOnOrmSession = function(Sender: TRestServer; Session: TAuthSession;
    Ctxt: TRestServerUriContext): boolean of object;

  /// callback allowing to customize the retrieval of an authenticated user
  // - as defined in TRestServer.OnAuthenticationUserRetrieve
  // - and executed by TRestServerAuthentication.GetUser
  // - on call, either aUserID will be <> 0, or aUserName is to be used
  // - if the function returns nil, default Server.AuthUserClass.Create()
  // methods won't be called, and the user will be reported as not found
  TOnAuthenticationUserRetrieve = function(Sender: TRestServerAuthentication;
    Ctxt: TRestServerUriContext; aUserID: TID;
    const aUserName: RawUtf8): TAuthUser of object;

  /// callback raised in case of authentication failure
  // - as used by TRestServerUriContext.AuthenticationFailed event
  TOnAuthenticationFailed = procedure(Sender: TRestServer;
    Reason: TOnAuthenticationFailedReason; Session: TAuthSession;
    Ctxt: TRestServerUriContext) of object;

  /// callback raised when TRestServer.Uri start to process a request
  // - should return HTTP_SUCCESS (200) to continue, or an error code to abort
  // - could also change the Call fields on the fly, if needed
  TOnStartUri = function(var Call: TRestUriParams): integer of object;

  /// callback raised before TRestServer.Uri ORM/SOA command execution
  // - should return TRUE to execute the command, FALSE to cancel it
  TOnBeforeUri = function(Ctxt: TRestServerUriContext): boolean of object;

  /// callback raised after TRestServer.Uri ORM/SOA command execution
  TOnAfterUri = procedure(Ctxt: TRestServerUriContext) of object;

  /// callback raised if TRestServer.Uri execution failed
  // - should return TRUE to execute Ctxt.Error(E,...), FALSE if returned
  // content has already been set as expected by the client
  TOnErrorUri = function(Ctxt: TRestServerUriContext;
    E: Exception): boolean of object;

  /// event signature used to notify a client callback
  // - implemented e.g. by TRestHttpServer.NotifyCallback
  TOnRestServerClientCallback = function(aSender: TRestServer;
    const aInterfaceDotMethodName, aParams: RawUtf8;
    aConnectionID: TRestConnectionID; aFakeCallID: integer;
    aResult, aErrorMsg: PRawUtf8): boolean of object;

  /// event signature used by TRestServer.OnServiceCreateInstance
  // - as called by TServiceFactoryServer.CreateInstance
  // - the actual Instance class can be quickly retrieved from
  // TServiceFactoryServer(Sender).ImplementationClass
  TOnServiceCreateInstance = procedure(
    Sender: TServiceFactory; Instance: TInterfacedObject) of object;

  /// callback allowing to customize the information returned by root/timestamp/info
  // - Sender is indeed a TRestServerUriContext instance
  TOnInternalInfo = procedure(Sender: TRestUriContext;
    var Info: TDocVariantData) of object;

  /// a generic REpresentational State Transfer (REST) server
  // - descendent must implement the protected EngineList() Retrieve() Add()
  // Update() Delete() methods
  // - automatic call of this methods by a generic Uri() RESTful function
  // - any published method of descendants must match TOnRestServerCallBack
  // prototype, and is expected to be thread-safe
  TRestServer = class(TRest)
  protected
    fHandleAuthentication: boolean;
    fAfterCreation: boolean;
    fShutdownRequested: boolean;
    fStatLevels: TRestServerMonitorLevels;
    fOptions: TRestServerOptions;
    fBypassOrmAuthentication: TUriMethods;
    /// the TAuthUser and TAuthGroup classes, as defined in model
    fAuthUserClass: TAuthUserClass;
    fAuthGroupClass: TAuthGroupClass;
    /// how in-memory sessions are handled
    fSessionClass: TAuthSessionClass;
    fJwtForUnauthenticatedRequest: TJwtAbstract;
    /// in-memory storage of TAuthSession instances
    fSessions: TSynObjectListSorted; // sorted by ID, with upgradable lock
    fSessionsDeprecatedTix: cardinal;
    /// used to compute genuine TAuthSession.ID cardinal value
    fSessionCounter: integer;
    fSessionCounterMin: cardinal;
    fTimestampInfoCacheTix: cardinal;
    fOnIdleLastTix: cardinal;
    fPublishedMethodTimestampIndex: byte;
    fPublishedMethodAuthIndex: byte;
    fPublishedMethodBatchIndex: byte;
    fPublishedMethodStatIndex: byte;
    fSessionAuthentication: TRestServerAuthenticationDynArray;
    fPublishedMethod: TRestServerMethods;
    fPublishedMethods: TDynArrayHashed;
    fTimestampInfoCache: RawUtf8;
    fStats: TRestServerMonitor;
    fStatUsage: TSynMonitorUsage;
    fAssociatedServices: TServicesPublishedInterfacesList;
    fRootRedirectGet: RawUtf8;
    fRootRedirectForbiddenToAuth: RawUtf8;
    fPublicUri: TRestServerUri;
    fIPBan, fIPWhiteJWT: TIPBan;
    fServicesRouting: TRestServerUriContextClass;
    fRecordVersionSlaveCallbacks: array of IServiceRecordVersionCallback;
    fServer: IRestOrmServer;
    fRouter: TRestRouter;
    fRouterSafe: TRWLightLock;
    fOnNotifyCallback: TOnRestServerClientCallback;
    procedure SetNoAjaxJson(const Value: boolean);
    function GetNoAjaxJson: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function GetRecordVersionMax: TRecordVersion;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetRecordVersionMax(Value: TRecordVersion);
    function GetAuthenticationSchemesCount: integer;
    /// ensure the thread will be taken into account during process
    procedure OnBeginCurrentThread(Sender: TThread); override;
    procedure OnEndCurrentThread(Sender: TThread); override;
    // called by Stat() and Info() method-based services
    procedure InternalStat(Ctxt: TRestServerUriContext; W: TJsonWriter); virtual;
    procedure AddStat(Flags: TRestServerAddStats; W: TJsonWriter);
    procedure InternalInfo(Ctxt: TRestServerUriContext; var Info: TDocVariantData); virtual;
    procedure SetStatUsage(usage: TSynMonitorUsage);
    function GetServiceMethodStat(const aMethod: RawUtf8): TSynMonitorInputOutput;
    procedure SetRoutingClass(aServicesRouting: TRestServerUriContextClass);
    procedure SetOptions(rso: TRestServerOptions);
    procedure SetOnNotifyCallback(const event: TOnRestServerClientCallback);
    /// add a new session to the internal session list
    // - do not use this method directly: this callback is to be used by
    // TRestServerAuthentication* classes
    // - will check that the logon name is valid
    // - on failure, will call TRestServerUriContext.AuthenticationFailed()
    // with afSessionAlreadyStartedForThisUser or afSessionCreationAborted reason
    procedure SessionCreate(var User: TAuthUser; Ctxt: TRestServerUriContext;
      out Session: TAuthSession); virtual;
    /// O(log(n)) binary search for Ctxt.Session ID in the internal list
    function LockedSessionFind(aSessionID: cardinal; aIndex: PPtrInt): TAuthSession;
    /// search for Ctxt.Session ID and fill Ctxt.Session* members if found
    // - returns nil if not found, or fill aContext.User/Group values if matchs
    // - this method will also check for outdated sessions, and delete them
    // - this method is not thread-safe: caller should use Sessions.Safe.ReadOnlyLock
    function LockedSessionAccess(Ctxt: TRestServerUriContext): TAuthSession;
    /// delete a session from its index in Sessions[]
    // - will perform any needed clean-up, and log the event
    // - this method is not thread-safe: caller should use Sessions.Safe.WriteLock
    procedure LockedSessionDelete(aSessionIndex: integer;
      Ctxt: TRestServerUriContext);
    /// SessionAccess will detect and delete outdated sessions, but you can call
    // this method to force checking for deprecated session now
    // - may be used e.g. from OnSessionCreate to limit the number of active sessions
    // - this method is thread-safe via Sessions.Safe.ReadWriteLock/WriteLock
    // - it will be called to search for outdated sessions only once per second
    // - returns how many deprecated sessions have been purge
    function SessionDeleteDeprecated(tix: cardinal): integer;
  public
    /// a method can be specified to be notified when a session is created
    // - for OnSessionCreate, returning TRUE will abort the session creation -
    // and you can set Ctxt.Call^.OutStatus to a corresponding error code
    // - it could be used e.g. to limit the number of client sessions
    OnSessionCreate: TOnOrmSession;
    /// a custom method to retrieve the TAuthUser instance for authentication
    // - will be called by TRestServerAuthentication.GetUser() instead of
    // plain AuthUserClass.Create()
    OnAuthenticationUserRetrieve: TOnAuthenticationUserRetrieve;
    /// this event handler will be executed when a session failed to initialize
    // (DenyOfService attack?) or the request is not valid (ManIntheMiddle attack?)
    // - e.g. if the URI signature is invalid, or OnSessionCreate event handler
    // aborted the session creation by returning TRUE (in this later case,
    // the Session parameter is not nil)
    // - you can access the current execution context from the Ctxt parameter,
    // e.g. to retrieve the caller's IP and ban aggressive users in Ctxt.RemoteIP
    // or the text error message corresponding to Reason in Ctxt.CustomErrorMsg
    OnAuthenticationFailed: TOnAuthenticationFailed;
    /// a method can be specified to be notified when a session is closed
    // - for OnSessionClosed, the returning boolean value is ignored
    // - Ctxt is nil if the session is closed due to a timeout
    // - Ctxt is not nil if the session is closed explicitly by the client
    OnSessionClosed: TOnOrmSession;
    /// this event will be executed by TServiceFactoryServer.CreateInstance
    // - you may set a callback to customize a server-side service instance,
    // i.e. inject class-level dependencies:
    // !procedure TMyClass.OnCreateInstance(
    // !  Sender: TServiceFactoryServer; Instance: TInterfacedObject);
    // !begin
    // !  if Sender.ImplementationClass=TLegacyStockQuery then
    // !    TLegacyStockQuery(Instance).fDbConnection := fDbConnection;
    // !end;
    // - consider using a TInjectableObjectClass implementation for pure IoC/DI
    OnServiceCreateInstance: TOnServiceCreateInstance;
    /// event trigerred when Uri start to process a request
    // - should return HTTP_SUCCESS (200) to continue, or an error code to abort
    // - could also change the Call fields on the fly, if needed
    OnStartUri: TOnStartUri;
    /// event trigerred when Uri() is about to execute an ORM/SOA command
    // - the supplied Ctxt parameter will give access to the command about to
    // be executed, e.g. Ctxt.Command=execSoaByInterface will identify a SOA
    // service execution, with the corresponding Service and ServiceMethodIndex
    // parameters as set by TRestServerUriContext.UriDecodeSoaByInterface
    // - should return TRUE if the method can be executed
    // - should return FALSE if the method should not be executed, and the
    // callback should set the corresponding error to the supplied context e.g.
    // ! Ctxt.Error('Unauthorized method',HTTP_NOTALLOWED);
    // - since this event will be executed by every TRestServer.Uri call,
    // it should better not make any slow process (like writing to a remote DB)
    OnBeforeUri: TOnBeforeUri;
    /// event trigerred when Uri() finished to process an ORM/SOA command
    // - the supplied Ctxt parameter will give access to the command which has
    // been executed, e.g. via Ctxt.Call.OutStatus or Ctxt.MicroSecondsElapsed
    // - since this event will be executed by every TRestServer.Uri call,
    // it should better not make any slow process (like writing to a remote DB)
    OnAfterUri: TOnAfterUri;
    /// event trigerred when Uri() raise an exception and failed to process a request
    // - if Ctxt.ExecuteCommand raised an exception, this callback will be
    // run with all neeed information
    // - should return TRUE to execute Ctxt.Error(E,...), FALSE if returned
    // content has already been set as expected by the client
    OnErrorUri: TOnErrorUri;
    /// event to customize the information returned by root/timestamp/info
    // - called by TRestServer.InternalInfo method
    // - you can add some application-level information for monitoring
    OnInternalInfo: TOnInternalInfo;
    /// event trigerred when Uri() is called, and at least 128 ms is elapsed
    // - could be used to execute some additional process after a period of time
    // - note that if TRestServer.Uri is not called by any client, this callback
    // won't be executed either - consider using TRestServer.Run instead
    OnIdle: TNotifyEvent;
    /// this property can be used to specify the URI parmeters to be used
    // for paged queries
    // - is set by default to PAGINGPARAMETERS_YAHOO constant by
    // TRestServer.Create() constructor
    UriPagingParameters: TRestServerUriPagingParameters;
    /// this event will be executed to push notifications from the WebSockets
    // server to a remote client, using a (fake) interface parameter
    // - is nil by default, but may point e.g. to TRestHttpServer.NotifyCallback
    // - only a single WS server can be assigned to a TRestServer instance
    property OnNotifyCallback: TOnRestServerClientCallback
      read fOnNotifyCallback write SetOnNotifyCallback;

    /// Server initialization with a specified Database Model
    // - if HandleUserAuthentication is false, will set URI access rights to
    // 'Supervisor' (i.e. all R/W access) by default
    // - if HandleUserAuthentication is true, will add TAuthUser and
    // TAuthGroup to the TOrmModel (if not already there)
    constructor Create(aModel: TOrmModel;
      aHandleUserAuthentication: boolean = false); reintroduce; overload; virtual;
    /// Server initialization with a temporary Database Model
    // - a Model will be created with supplied tables, and owned by the server
    // - if you instantiate a TRestServerFullMemory or TRestServerDB
    // with this constructor, an in-memory engine will be created, with
    // enough abilities to run regression tests, for instance
    constructor CreateWithOwnModel(const Tables: array of TOrmClass;
      aHandleUserAuthentication: boolean = false;
      const aRoot: RawUtf8 = 'root');
    /// Server initialization with a void Database Model and not authentication
    // - could be used e.g. for a interface-based services API REST server
    constructor Create(const aRoot: RawUtf8); reintroduce; overload;
    /// initialize REST server instance from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// Server finalization
    destructor Destroy; override;
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TRestOrmParent); override;

    /// implement a generic local, piped or HTTP/1.1 provider
    // - this is the main entry point of the server, from the client side
    // - default implementation calls protected methods EngineList() Retrieve()
    // Add() Update() Delete() UnLock() EngineExecute() above, which must be overridden by
    // the TRestServer child
    // - for 'GET ModelRoot/TableName', url parameters can be either "select" and
    // "where" (to specify a SQL Query, from the SqlFromSelectWhere function),
    // either "sort", "dir", "startIndex", "results", as expected by the YUI
    // DataSource Request Syntax for data pagination - see
    // http://developer.yahoo.com/yui/datatable/#data
    // - execution of this method could be monitored via OnBeforeUri and OnAfterUri
    // event handlers
    procedure Uri(var Call: TRestUriParams); virtual;

    /// you can call this method to prepare the server for shutting down
    // - it will reject any incoming request from now on, and will wait until
    // all pending requests are finished, for proper server termination
    // - you could optionally save the current server state (e.g. user sessions)
    // into a file, ready to be retrieved later on using SessionsLoadFromFile -
    // note that this will work only for ORM sessions, NOT complex SOA state
    // - this method is called by Destroy itself
    procedure Shutdown(const aStateFileName: TFileName = ''); virtual;
    /// wait for the specified number of milliseconds
    // - if Shutdown is called in-between, returns true
    // - if the thread waited the supplied time, returns false
    function SleepOrShutdown(MS: integer): boolean;
    /// can be called at startup to validate the URI routes on this server
    // - is also called from Uri() if needed (e.g. after ResetRoutes)
    procedure ComputeRoutes;
    /// called when the routes need to be re-computed
    // - e.g. is called when a service has been registered, or options changes
    procedure ResetRoutes;
    /// main access to the class implementing IRestOrm methods for this instance
    // - used internally to avoid ORM: IRestOrm reference counting and
    // enable inlining of most simple methods, if possible
    // - is a TRestOrmServer instance
    function OrmInstance: TRestOrm;
      {$ifdef HASINLINE}inline;{$endif}
    /// access TRestOrmServer.RecordVersionMax property
    // - used internally by TServiceContainerServer for client/server synchronization
    property RecordVersionMax: TRecordVersion
      read GetRecordVersionMax write SetRecordVersionMax;
    /// low-level propagation of a record content
    // - used internally by TServiceContainerServer for client/server synchronization
    procedure RecordVersionHandle(Occasion: TOrmOccasion;
      TableIndex: integer; var Decoder: TJsonObjectDecoder;
      RecordVersionField: TOrmPropInfoRttiRecordVersion); virtual;
    /// low-level access to the internal URI multiplexer
    // - to be used e.g. from overriden TRestServerUriContext.UriComputeRoutes
    property Router: TRestRouter
      read fRouter;
  public
    /// call this method to add an authentication method to the server
    // - will return the just created TRestServerAuthentication instance,
    // or the existing instance if it has already been registered
    // - you can use this method to tune the authentication, e.g. if you have
    // troubles with AJAX asynchronous callbacks:
    // ! (aServer.AuthenticationRegister(TRestServerAuthenticationDefault) as
    // !   TRestServerAuthenticationDefault).NoTimestampCoherencyCheck := true;
    // or if you want to customize the session_signature parameter algorithm:
    // ! (aServer.AuthenticationRegister(TRestServerAuthenticationDefault) as
    // !   TRestServerAuthenticationDefault).Algorithm := suaMD5;
    function AuthenticationRegister(
      aMethod: TRestServerAuthenticationClass): TRestServerAuthentication; overload;
    /// call this method to add several authentication methods to the server
    // - if TRestServer.Create() constructor is called with
    // aHandleUserAuthentication set to TRUE, it will register the two
    // following classes:
    // ! AuthenticationRegister([
    // !   TRestServerAuthenticationDefault, TRestServerAuthenticationSspi]);
    procedure AuthenticationRegister(
      const aMethods: array of TRestServerAuthenticationClass); overload;
    /// call this method to remove an authentication method to the server
    procedure AuthenticationUnregister(
      aMethod: TRestServerAuthenticationClass); overload;
    /// call this method to remove several authentication methods to the server
    procedure AuthenticationUnregister(
      const aMethods: array of TRestServerAuthenticationClass); overload;
    /// call this method to remove all authentication methods to the server
    procedure AuthenticationUnregisterAll;
    /// read-only access to the internal list of sessions
    // - ensure you protect its access using Sessions.Safe TRWLock
    property Sessions: TSynObjectListSorted
      read fSessions;
    /// read-only access to the list of registered server-side authentication
    // methods, used for session creation
    // - note that the exact number or registered services in this list is
    // stored in the AuthenticationSchemesCount property
    property AuthenticationSchemes: TRestServerAuthenticationDynArray
      read fSessionAuthentication;
    /// how many authentication methods are registered in AuthenticationSchemes
    property AuthenticationSchemesCount: integer
      read GetAuthenticationSchemesCount;
    /// define if unsecure connections (i.e. not in-process or encrypted
    // WebSockets) with no session can be authenticated via JWT
    // - once set, this instance will be owned by the TRestServer
    // - by definition, such JWT authentication won't identify any mORMot user
    // nor session (it just has to be valid), so only sicSingle, sicShared or
    // sicPerThread interface-based services execution are possible
    // - typical usage is for a public API, in conjunction with
    // ServiceDefine(...).ResultAsJsonObjectWithoutResult := true on the server
    // side and TRestClientUri.ServiceDefineSharedApi() method for the client
    // - see also JwtForUnauthenticatedRequestWhiteIP() for additional security
    property JwtForUnauthenticatedRequest: TJwtAbstract
      read fJwtForUnauthenticatedRequest write fJwtForUnauthenticatedRequest;
    /// (un)register a banned IPv4 value
    // - any connection attempt from this IP Address will be rejected by
    function BanIP(const aIP: RawUtf8; aRemoveBan: boolean = false): boolean;
    /// (un)register a an IPv4 value to the JWT white list
    // - by default, a JWT validated by JwtForUnauthenticatedRequest will be accepted
    // - to avoid MiM (Man-In-the-Middle) attacks, if a JWT white list is defined
    // using this method, any connection from a non registered IP will be rejected,
    // even with a valid JWT
    // - WebSockets connections are secure enough to bypass this list
    function JwtForUnauthenticatedRequestWhiteIP(const aIP: RawUtf8;
      aRemoveWhite: boolean = false): boolean;
    /// returns a copy of the user associated to a session ID
    // - returns nil if the session does not exist (e.g. if authentication disabled)
    // - caller MUST release the TAuthUser instance returned (if not nil)
    // - this method IS thread-safe, calling internally Sessions.Safe.ReadOnlyLock
    // (the returned TAuthUser is a private copy from Sessions[].User instance,
    // in order to be really thread-safe)
    // - the returned TAuthUser instance will have GroupRights=nil but will
    // have ID, LogonName, DisplayName, PasswordHashHexa and Data fields available
    function SessionGetUser(aSessionID: cardinal): TAuthUser;
    /// persist all in-memory sessions into a compressed binary file
    // - you should not call this method it directly, but rather use Shutdown()
    // with a StateFileName parameter - to be used e.g. for a short maintainance
    // server shutdown, without loosing the current logged user sessions
    // - this method IS thread-safe, and calls internally Sessions.Safe.ReadOnlyLock
    procedure SessionsSaveToFile(const aFileName: TFileName);
    /// re-create all in-memory sessions from a compressed binary file
    // - typical use is after a server restart, with the file supplied to the
    // Shutdown() method: it could be used e.g. for a short maintainance server
    // shutdown, without loosing the current logged user sessions
    // - WARNING: this method will restore authentication sessions for the ORM,
    // but not any complex state information used by interface-based services,
    // like sicClientDriven class instances - DO NOT use this feature with SOA
    // - this method IS thread-safe, and calls internally Sessions.Safe.WriteLock
    procedure SessionsLoadFromFile(const aFileName: TFileName;
      andDeleteExistingFileAfterRead: boolean);
    /// retrieve all current session information as a JSON array
    function SessionsAsJson: RawJson;
    /// retrieve the current session TAuthUser.ID (if any) from the
    // ServiceRunningContext threadvar
    function GetCurrentSessionUserID: TID; override;
    /// the HTTP server should call this method so that ServicesPublishedInterfaces
    // registration will be able to work
    procedure SetPublicUri(const Address, Port: RawUtf8);

    /// add all published methods of a given object instance to the method-based
    // list of services
    // - all those published method signature should match TOnRestServerCallBack
    // - will use the supplied aMethods or detect '_VERB1_[_VERB2_][..]MethodName'
    // pattern e.g. as '_GET_Info' or '_GET__DELETE_User'
    procedure ServiceMethodRegisterPublishedMethods(const aPrefix: RawUtf8;
      aInstance: TObject; aMethods: TUriMethods = [mGET, mPOST, mPUT, mDELETE]);
    /// direct registration of a method for a given low-level event handler
    // - returns the index in the fPublishedMethod[] internal array
    // - will use the supplied aMethods or detect '_VERB1_[_VERB2_][..]MethodName'
    // pattern e.g. as '_GET_Info' or '_GET__DELETE_User'
    function ServiceMethodRegister(aMethodName: RawUtf8;
      const aEvent: TOnRestServerCallBack; aByPassAuthentication: boolean = false;
      aMethods: TUriMethods = [mGET, mPOST, mPUT, mDELETE]): PtrInt;
    /// call this method to disable Authentication method check for a given
    // published method-based service name
    // - by default, only Auth and Timestamp methods do not require the RESTful
    // authentication of the URI; you may call this method to add another method
    // to the list (e.g. for returning some HTML content from a public URI)
    // - if the supplied aMethodName='', all method-based services will
    // bypass the authenticaton process
    // - returns the method index number
    function ServiceMethodByPassAuthentication(
      const aMethodName: RawUtf8): integer;
    /// access or initialize the internal IoC resolver
    // - overriden to create and initialize the internal TServiceContainerServer
    // if no service interface has been registered yet
    function ServiceContainer: TServiceContainer; override;
    /// register a Service class on the server side
    // - this methods expects a class to be supplied, and the exact list of
    // interfaces to be registered to the server (e.g. [TypeInfo(IMyInterface)])
    // and implemented by this class
    // - class can be any TInterfacedObject, but TInterfacedObjectWithCustomCreate
    // can be used if you need an overridden constructor
    // - instance implementation pattern will be set by the appropriate parameter
    // - will return the first of the registered TServiceFactoryServer created
    // on success (i.e. the one corresponding to the first item of the aInterfaces
    // array), or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    // - the same implementation class can be used to handle several interfaces
    // (just as Delphi allows to do natively)
    // - will return the first of the registered TServiceFactoryServerAbstract
    // on success (i.e. corresponding to aInterfaces[0] - not to the others),
    // or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    function ServiceRegister(aImplementationClass: TInterfacedClass;
      const aInterfaces: array of PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload; virtual;
    /// register a Service instance on the server side
    // - this methods expects a class instance to be supplied, and the exact list
    // of interfaces to be registered to the server (e.g. [TypeInfo(IMyInterface)])
    // and implemented by this shared instance
    // - as a consequence, instance implementation pattern will always be sicShared
    // - will return the first of the registered TServiceFactoryServer created
    // on success (i.e. the one corresponding to the first item of the aInterfaces
    // array), or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    // - the same implementation class can be used to handle several interfaces
    // (just as Delphi allows to do natively)
    // - will return the first of the registered TServiceFactoryServerAbstract
    // on success (i.e. corresponding to aInterfaces[0] - not to the others),
    // or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    function ServiceRegister(aSharedImplementation: TInterfacedObject;
      const aInterfaces: array of PRttiInfo;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload; virtual;
    /// register a remote Service via its interface
    // - this overloaded method will register a remote Service, accessed via the
    // supplied TRest/TRestClientUri instance: it can be available in the main
    // TRestServer.Services property, but execution will take place on a
    // remote server - may be used e.g. for dedicated hosting of services (in
    // a DMZ for instance)
    // - this methods expects a list of interfaces to be registered to the client
    // (e.g. [TypeInfo(IMyInterface)])
    // - instance implementation pattern will be set by the appropriate parameter
    // - will return true on success, false if registration failed (e.g. if any of
    // the supplied interfaces is not correct or is not available on the server)
    // - that is, server side will be called to check for the availability of
    // each interface
    // - you can specify an optional custom contract for the first interface
    function ServiceRegister(aClient: TRest; const aInterfaces: array of PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): boolean; overload; virtual;
    /// register a Service class on the server side
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - will return the first of the registered TServiceFactoryServerAbstract
    // on success (i.e. corresponding to aInterfaces[0] - not to the others),
    // or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    function ServiceDefine(aImplementationClass: TInterfacedClass;
      const aInterfaces: array of TGuid;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a Service instance on the server side
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - the supplied aSharedImplementation will be owned by this Server instance
    // - will return the first of the registered TServiceFactoryServerAbstract
    // on success (i.e. corresponding to aInterfaces[0] - not to the others),
    // or nil if registration failed (e.g. if any of the supplied interfaces
    // is not implemented by the given class)
    function ServiceDefine(aSharedImplementation: TInterfacedObject;
      const aInterfaces: array of TGuid;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a remote Service via its interface
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefine(aClient: TRest; const aInterfaces: array of TGuid;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): boolean; overload;
    /// the routing classs of the service remote request
    // - by default, will use TRestRoutingRest, i.e. an URI-based
    // layout which is secure (since will use our RESTful authentication scheme),
    // and also very fast
    // - but TRestRoutingJsonRpc can e.g. be set (on BOTH client and
    // server sides), if the client will rather use JSON/RPC alternative pattern
    // - NEVER set the abstract TRestServerUriContext class on this property
    property ServicesRouting: TRestServerUriContextClass
      read fServicesRouting write SetRoutingClass;
    /// retrieve detailed statistics about a method-based service use
    // - will return a reference to the actual alive item: caller should
    // not free the returned instance
    property ServiceMethodStat[const aMethod: RawUtf8]: TSynMonitorInputOutput
      read GetServiceMethodStat;
    /// compute a JSON description of all available services, and its public URI
    // - the JSON object matches the TServicesPublishedInterfaces record type
    // - used by TRestClientUri.ServicePublishOwnInterfaces to register all
    // the services supported by the client itself
    // - warning: the public URI should have been set via SetPublicUri()
    function ServicesPublishedInterfaces: RawUtf8;
    /// initiate asynchronous master/slave replication on a master TRest
    // - allow synchronization of a TOrm table, using its TRecordVersion
    // field, for real-time master/slave replication on the master side
    // - this method will register the IServiceRecordVersion service on the
    // server side, so that RecordVersionSynchronizeStartSlave() will be able
    // to receive push notifications of any updates
    // - this method expects the communication channel to be bidirectional,
    // e.g. a mormot.rest.http.server's TRestHttpServer in HTTP_BIDIR mode
    // (either useBidirSocket or useBidirAsync - see WEBSOCKETS_DEFAULT_MODE)
    function RecordVersionSynchronizeMasterStart(
      ByPassAuthentication: boolean = false): boolean;
    /// initiate asynchronous master/slave replication on a slave TRest
    // - start synchronization of a TOrm table, using its TRecordVersion
    // field, for real-time master/slave replication on the slave side
    // - this method will first retrieve any pending modification by regular
    // REST calls to RecordVersionSynchronizeSlave, then create and register a
    // callback instance using RecordVersionSynchronizeSubscribeMaster()
    // - this method expects the communication channel to be bidirectional, e.g.
    // a TRestHttpClientWebsockets
    // - the modifications will be pushed by the master, then applied to the
    // slave storage, until RecordVersionSynchronizeSlaveStop method is called
    // - an optional OnNotify event may be defined, which will be triggered
    // for all incoming change, supllying the updated TOrm instance
    function RecordVersionSynchronizeSlaveStart(Table: TOrmClass;
      MasterRemoteAccess: TRestClientUri;
      const OnNotify: TOnBatchWrite = nil): boolean;
    /// finalize asynchronous master/slave replication on a slave TRest
    // - stop synchronization of a TOrm table, using its TRecordVersion
    // field, for real-time master/slave replication on the slave side
    // - expect a previous call to RecordVersionSynchronizeSlaveStart
    function RecordVersionSynchronizeSlaveStop(Table: TOrmClass): boolean;
    /// low-level callback registration for asynchronous master/slave replication
    // - you should not have to use this method, but rather
    // RecordVersionSynchronizeMasterStart and RecordVersionSynchronizeSlaveStart
    // RecordVersionSynchronizeSlaveStop methods
    // - register a callback interface on the master side, which will be called
    // each time a write operation is performed on a given TOrm with a
    // TRecordVersion field
    // - the callback parameter could be a TServiceRecordVersionCallback instance,
    // which will perform all update operations as expected
    // - the callback process will be blocking for the ORM write point of view:
    // so it should be as fast as possible, or asynchronous - note that regular
    // callbacks using WebSockets, as implemented by mormot.net.ws.core.server
    // and mormot.rest.http.server's TRestHttpServer in HTTP_BIDIR mode
    // (either useBidirSocket or useBidirAsync - see WEBSOCKETS_DEFAULT_MODE)
    // useBidirSocket/useBidirAsync mode
    // - if the supplied RecordVersion is not the latest on the server side,
    // this method will return FALSE and the caller should synchronize again via
    // RecordVersionSynchronize() to avoid any missing update
    // - if the supplied RecordVersion is the latest on the server side,
    // this method will return TRUE and put the Callback notification in place
    function RecordVersionSynchronizeSubscribeMaster(Table: TOrmClass;
      RecordVersion: TRecordVersion;
      const SlaveCallback: IServiceRecordVersionCallback): boolean; overload;
    /// grant access to this database content from a dll using the global
    // LibraryRequest() function
    // - returns true if the LibraryRequest() function is implemented by this
    // TRestServer, and TRestClientLibraryRequest can access it
    // - returns false if a TRestServer was already exported
    // - call with Disable=true to remove the LibraryRequest() function binding
    function ExportServerGlobalLibraryRequest(Disable: boolean = false): boolean;

    {$ifndef PUREMORMOT2}
    /// redirect to Server: IRestOrmServer methods
    procedure CreateMissingTables(user_version: cardinal = 0;
      options: TOrmInitializeTableOptions = []);
    function CreateSqlIndex(Table: TOrmClass; const FieldName: RawUtf8;
      Unique: boolean; const IndexName: RawUtf8 = ''): boolean; overload;
    function CreateSqlIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean): boolean; overload;
    function CreateSqlMultiIndex(Table: TOrmClass;
      const FieldNames: array of RawUtf8; Unique: boolean; IndexName: RawUtf8 = ''): boolean;
    function IsInternalSQLite3Table(aTableIndex: integer): boolean;
    function MaxUncompressedBlobSize(Table: TOrmClass): integer;
    {$endif PUREMORMOT2}

    /// main access to the IRestOrmServer methods of this instance
    property Server: IRestOrmServer
      read fServer;
    /// set this property to true to transmit the JSON data in a "not expanded" format
    // - not directly compatible with Javascript object list decode: not to be
    // used in AJAX environnement (like in TSqlite3HttpServer)
    // - but transmitted JSON data is much smaller if set it's set to FALSE, and
    // if you use a Delphi Client, parsing will be also faster and memory usage
    // will be lower
    // - By default, the NoAjaxJson property is set to TRUE in
    // TRestServer.ExportServerNamedPipe: if you use named pipes for communication,
    // you probably won't use javascript because browser communicates via HTTP!
    // - But otherwise, NoAjaxJson property is set to FALSE. You could force its
    // value to TRUE and you'd save some bandwidth if you don't use javascript:
    // even the parsing of the JSON Content will be faster with Delphi client
    // if JSON content is not expanded
    // - the "expanded" or standard/AJAX layout allows you to create pure JavaScript
    // objects from the JSON content, because the field name / JavaScript object
    // property name is supplied for every value
    // - the "not expanded" layout, NoAjaxJson property is set to TRUE,
    // reflects exactly the layout of the SQL request - first line contains the
    // field names, then all next lines are the field content
    // - is in fact stored in rsoNoAjaxJson item in Options property
    property NoAjaxJson: boolean
      read GetNoAjaxJson write SetNoAjaxJson;
    /// the URI to redirect any plain GET on root URI, without any method
    // - could be used to ease access from web browsers URI
    // - consider using faster and more precise THttpServerGeneric.Route.Get()
    property RootRedirectGet: RawUtf8
      read fRootRedirectGet write fRootRedirectGet;
    /// a list of the services associated by all clients of this server instance
    // - when a client connects to this server, it will publish its own services
    // (when checking its interface contract), so that they may be identified
    property AssociatedServices: TServicesPublishedInterfacesList
      read fAssociatedServices;
    /// flag set to TRUE when the Shutdown method has been called
    property ShutdownRequested: boolean
      read fShutdownRequested;
  published
    /// allow to customize how TRestServer.Uri process the requests
    // - e.g. if HTTP_SUCCESS with no body should be translated into HTTP_NOCONTENT
    property Options: TRestServerOptions
      read fOptions write SetOptions;
    /// set to true if the server will handle per-user authentication and
    // access right management
    // - i.e. if the associated TOrmModel contains TAuthUser and
    // TAuthGroup tables (set by constructor)
    property HandleAuthentication: boolean
      read fHandleAuthentication;
    /// allow to by-pass Authentication for a given set of HTTP verbs
    // - by default, RESTful access to the ORM will follow HandleAuthentication
    /// setting: but you could define some HTTP verb to this property, which
    // will by-pass the authentication - may be used e.g. for public GET
    // of the content by an AJAX client
    property BypassOrmAuthentication: TUriMethods
      read fBypassOrmAuthentication write fBypassOrmAuthentication;
    /// read-only access to the high-level Server statistics
    // - see ServiceMethodStat[] for information about method-based services,
    // or TServiceFactoryServer.Stats / Stat[] for interface-based services
    // - statistics are available remotely as JSON from the Stat() method
    property Stats: TRestServerMonitor
      read fStats;
    /// which level of detailed information is gathered
    // - by default, contains SERVERDEFAULTMONITORLEVELS, i.e.
    // ! [mlUri, mlTables, mlMethods, mlInterfaces, mlSQLite3]
    // - you can add mlSessions to maintain per-session statistics: but it will
    // slow down the process, with higher memory consumption for each session
    property StatLevels: TRestServerMonitorLevels
      read fStatLevels write fStatLevels;
    /// could be set to track statistic from Stats information
    // - it may be e.g. a TSynMonitorUsageRest instance for REST storage
    // - warning: current Uri() implementation is inefficient (single lock)
    property StatUsage: TSynMonitorUsage
      read fStatUsage write SetStatUsage;
    /// the class inheriting from TAuthSession to handle in-memory sessions
    // - since all sessions data remain in memory, ensure they are not taking
    // too much resource (memory or process time)
    property SessionClass: TAuthSessionClass
      read fSessionClass write fSessionClass;
    /// the class inheriting from TAuthUser, as defined in the model
    // - during authentication, this class will be used for every TAuthUser
    // table access
    // - see also the OnAuthenticationUserRetrieve optional event handler
    property AuthUserClass: TAuthUserClass
      read fAuthUserClass;
    /// the class inheriting from TAuthGroup, as defined in the model
    // - during authentication, this class will be used for every TAuthGroup
    // table access
    property AuthGroupClass: TAuthGroupClass
      read fAuthGroupClass;

    { standard method-based services }
  public
    /// REST service accessible from the ModelRoot/Timestamp URI
    // - returns the server time stamp TTimeLog/Int64 value as UTF-8 text
    // - this method will not require an authenticated client
    // - hidden ModelRoot/Timestamp/info command will return basic execution
    // information, less verbose (and sensitive) than Stat(), calling virtual
    // InternalInfo() protected method
    procedure Timestamp(Ctxt: TRestServerUriContext);
    /// REST service accessible from ModelRoot/Auth URI
    // - called by the clients for authentication and session management
    // - this method won't require an authenticated client, since it is used to
    // initiate authentication
    // - this global callback method is thread-safe
    procedure Auth(Ctxt: TRestServerUriContext);
    /// REST service accessible from the ModelRoot/Batch URI
    // - will execute a set of RESTful commands, in a single step, with optional
    // automatic SQL transaction generation
    // - this method will require an authenticated client, for safety
    // - expect input as JSON commands:
    // & '{"Table":["cmd":values,...]}'
    // or for multiple tables:
    // & '["cmd@Table":values,...]'
    // with cmd in POST/PUT with {object} as value or DELETE with ID
    // - returns an array of integers: '[200,200,...]' or '["OK"]' if all
    // returned status codes are 200 (HTTP_SUCCESS)
    // - URI are either 'ModelRoot/TableName/Batch' or 'ModelRoot/Batch'
    procedure Batch(Ctxt: TRestServerUriContext);
    /// REST service accessible from the ModelRoot/CacheFlush URI
    // - it will flush the server result cache
    // - this method shall be called by the clients when the Server cache may be
    // not consistent any more (e.g. after a direct write to an external database)
    // - this method will require an authenticated client, for safety
    // - GET ModelRoot/CacheFlush URI will flush the whole Server cache,
    // for all tables
    // - GET ModelRoot/CacheFlush/TableName URI will flush the specified
    // table cache
    // - GET ModelRoot/CacheFlush/TableName/TableID URI will flush the content
    // of the specified record
    // - POST ModelRoot/CacheFlush/_callback_ URI will be called by the client
    // to notify the server that an interface callback instance has been released
    // - POST ModelRoot/CacheFlush/_ping_ URI will be called by the client after
    // every half session timeout (or at least every hour) to notify the server
    // that the connection is still alive
    procedure CacheFlush(Ctxt: TRestServerUriContext);
    /// REST service accessible from ModelRoot/Stat URI to gather detailed information
    // - returns the current execution statistics of this server, as a JSON object
    // - this method will require an authenticated client, for safety
    // - by default, will return the high-level information of this server
    // - will return human-readable JSON layout if ModelRoot/Stat/json is used, or
    // the corresponding XML content if ModelRoot/Stat/xml is used
    // - you can define withtables, withmethods, withinterfaces, withsessions or
    // withsqlite3 additional parameters to return detailed information about
    // method-based services, interface-based services, per session statistics,
    // or prepared SQLite3 SQL statement timing (for a TRestServerDB instance)
    // ! Client.CallBackGet('stat',['withtables',true,'withmethods',true,
    // !   'withinterfaces',true,'withsessions',true,'withsqlite3',true],stats);
    // - defining a 'withall' parameter will retrieve all available statistics
    // - note that TRestServer.StatLevels property will enable statistics
    // gathering for tables, methods, interfaces, sqlite3 or sessions
    // - a specific findservice=ServiceName parameter will not return any
    // statistics, but matching URIs from the server AssociatedServices list
    procedure Stat(Ctxt: TRestServerUriContext);
  end;

  /// class-reference type (metaclass) of a REST server
  TRestServerClass = class of TRestServer;


const
  /// the default URI parameters for query paging
  // - those values are the one expected by YUI components
  PAGINGPARAMETERS_YAHOO: TRestServerUriPagingParameters = (
    Sort: 'SORT=';
    Dir: 'DIR=';
    StartIndex: 'STARTINDEX=';
    Results: 'RESULTS=';
    Select: 'SELECT=';
    Where: 'WHERE=';
    SendTotalRowsCountFmt: '');

  /// default value of TRestServer.StatLevels property
  // - i.e. gather all statistics, but mlSessions which has a slowdown impact
  SERVERDEFAULTMONITORLEVELS: TRestServerMonitorLevels =
    [mlUri, mlTables, mlMethods, mlInterfaces, mlSQLite3];


function ToText(res: TOnAuthenticationFailedReason): PShortString; overload;
function ToText(n: TRestNode): PShortString; overload;


/// returns the thread-specific service context execution currently running
// on the server side
// - just an inlined transtype of the PerThreadRunningContextAddress function
// - note that in case of direct server side execution of the service, this
// information won't be filled, so the safest (and slightly faster) access
// to the TRestServer instance associated with a service is to inherit your
// implementation class from TInjectableObjectRest, and not use this threadvar
// - is set by TServiceFactoryServer.ExecuteMethod() just before calling the
// implementation method of a service, allowing to retrieve the current
// execution context - Request member is set from a client/server execution:
// Request.Server is the safe access point to the underlying TRestServer,
// in such context - also consider the CurrentServiceContextServer function to
// retrieve directly the running TRestServer (if any)
// - its content is reset to zero out of the scope of a method execution
// - when used, a local copy or a PServiceRunningContext pointer should better
// be created, since accessing a threadvar has a non negligible performance
// cost - for instance, if you want to use a "with" statement:
// ! with ServiceRunningContext do
// !   ... access TServiceRunningContext members
// or as a local variable:
// !var
// !  context: PServiceRunningContext;
// !  inContentType: RawUtf8;
// !begin
// !  context := ServiceRunningContext; // threadvar access once
// !  ...
// !  inContentType := context.Request.Call^.InBodyType;
// !end;
function ServiceRunningContext: PServiceRunningContext;
  {$ifdef HASINLINE}inline;{$endif}

/// returns a safe 256-bit hexadecimal nonce, changing every 5 minutes
// - as used e.g. by TRestServerAuthenticationDefault.Auth
// - this function is very fast, even if cryptographically-level SHA-3 secure
function CurrentNonce(Ctxt: TRestServerUriContext;
  Previous: boolean = false): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// returns a safe 256-bit nonce, changing every 5 minutes
// - can return the (may be cached) value as hexadecimal text or THash256 binary
procedure CurrentNonce(Ctxt: TRestServerUriContext; Previous: boolean;
  Nonce: PRawUtf8; Nonce256: PHash256); overload;

/// this function can be exported from a DLL to remotely access to a TRestServer
// - use TRestServer.ExportServerGlobalLibraryRequest to assign a server to this function
// - return the HTTP status, e.g. 501 HTTP_NOTIMPLEMENTED if no
// TRestServer.ExportServerGlobalLibraryRequest has been assigned yet
// - once used, memory for Resp and Head should be released with
// LibraryRequestFree() returned function
// - the Server current Internal State counter will be set to State
// - simply use TRestClientLibraryRequest to access to an exported LibraryRequest() function
// - match TLibraryRequest function signature
function LibraryRequest(
  Url, Method, SendData: PUtf8Char; UrlLen, MethodLen, SendDataLen: cardinal;
  out HeadRespFree: TLibraryRequestFree; var Head: PUtf8Char; var HeadLen: cardinal;
  out Resp: PUtf8Char; out RespLen, State: cardinal): cardinal; cdecl;


{ ************ TRestHttpServerDefinition Settings for a HTTP Server }

type
  /// supported REST authentication schemes
  // - used by the overloaded TRestHttpServer.Create(TRestHttpServerDefinition)
  // constructor in mormot.rest.http.server.pas, and also in dddInfraSettings.pas
  // - map TRestServerAuthenticationDefault, TRestServerAuthenticationHttpBasic,
  // TRestServerAuthenticationNone and TRestServerAuthenticationSspi classes
  // - asSSPI will use mormot.lib.sspi/gssapi units depending on the OS, and
  // may be not available on some targets (e.g. Android)
  TRestHttpServerRestAuthentication = (
    adDefault,
    adHttpBasic,
    adWeak,
    adSspi);

  /// customize TRestHttpServer process
  // - rsoOnlyJsonRequests will return HTTP 400 "Bad Request" if the input
  // is not of 'application/json' content type
  // - rsoOnlyValidUtf8 will return HTTP 406 "Non Acceptable" if input JSON or
  // text body is not valid UTF-8 - calling fast IsValidUtf8() function
  // - rsoRedirectServerRootUriForExactCase to search root URI case-sensitive,
  // mainly to avoid errors with HTTP cookies, which path is case-sensitive -
  // when set, such not exact case will be redirected via a HTTP 307 command
  // - rsoAllowSingleServerNoRoot will allow URI with no Model.Root prefix, i.e.
  // 'GET url' to be handled as 'GET root/url' - by design, it would work only
  // with a single registered TRestServer (to know which Model.Root to use)
  // - rsoHeadersUnFiltered maps THttpServer.HeadersUnFiltered property
  // - rsoCompressSynLZ and rsoCompressGZip enable SynLZ and GZip compression
  // on server side - it should also be enabled for the client
  // - rsoLogVerbose would include a lot of detailed information, useful only
  // to debug the low-level server process - to be enabled only when required
  // - rsoNoXPoweredHeader excludes 'X-Powered-By: mORMot 2 synopse.info' header
  // - rsoIncludeDateHeader will let all answers include a Date: ... HTTP header
  // - rsoBan40xIP will reject any IP for a few seconds after a 4xx error code
  // is returned (but 401/403) - only implemented by THttpAsyncServer for now
  TRestHttpServerOption = (
    rsoOnlyJsonRequests,
    rsoOnlyValidUtf8,
    rsoRedirectServerRootUriForExactCase,
    rsoAllowSingleServerNoRoot,
    rsoHeadersUnFiltered,
    rsoCompressSynLZ,
    rsoCompressGZip,
    rsoLogVerbose,
    rsoNoXPoweredHeader,
    rsoIncludeDateHeader,
    rsoBan40xIP);

  /// how to customize TRestHttpServer process
  TRestHttpServerOptions = set of TRestHttpServerOption;

  /// parameters supplied to publish a TRestServer via HTTP
  // - used by the overloaded TRestHttpServer.Create(TRestHttpServerDefinition)
  // constructor in mormot.rest.http.server.pas, and also in dddInfraSettings.pas
  TRestHttpServerDefinition = class(TSynPersistentWithPassword)
  protected
    fBindPort: RawByteString;
    fAuthentication: TRestHttpServerRestAuthentication;
    fDomainHostRedirect: RawUtf8;
    fRootRedirectToUri: RawUtf8;
    fEnableCors: RawUtf8;
    fThreadCount: byte;
    fHttps: boolean;
    fHttpSysQueueName: SynUnicode;
    fRemoteIPHeader: RawUtf8;
    fOptions: TRestHttpServerOptions;
    fNginxSendFileFrom: TFileName;
  public
    /// initialize with the default values
    constructor Create; override;
  published
    /// defines the port to be used for REST publishing
    // - may include an optional IP address to bind, e.g. '127.0.0.1:8888'
    property BindPort: RawByteString
      read fBindPort write fBindPort;
    /// which authentication is expected to be published
    property Authentication: TRestHttpServerRestAuthentication
      read fAuthentication write fAuthentication;
    /// register domain names to be redirected to a some Model.Root
    // - specified as CSV values of Host=Name pairs, e.g.
    // ! 'project1.com=root1,project2.com=root2,blog.project2.com=root2/blog'
    property DomainHostRedirect: RawUtf8
      read fDomainHostRedirect write fDomainHostRedirect;
    /// redirect a '/' HTTP or HTTPS request to a given URI via a 307 command
    // - follow the Https property for the redirection source
    property RootRedirectToUri: RawUtf8
      read fRootRedirectToUri write fRootRedirectToUri;
    /// allow Cross-origin resource sharing (CORS) access
    // - set this property to '*' if you want to be able to access the
    // REST methods from an HTML5 application hosted in another location,
    // or define a CSV white list of TMatch-compatible origins
    // - will set e.g. the following HTTP header:
    // ! Access-Control-Allow-Origin: *
    property EnableCors: RawUtf8
      read fEnableCors write fEnableCors;
    /// how many threads the thread pool associated with this HTTP server
    // should create
    // - if set to 0, will use default value 32
    // - this parameter may be ignored depending on the actual HTTP
    // server used, which may not have any thread pool
    property ThreadCount: byte
      read fThreadCount write fThreadCount;
    /// defines if https:// protocol should be used
    // - implemented only by http.sys server under Windows, not by socket servers
    property Https: boolean
      read fHttps write fHttps;
    /// the displayed name in the http.sys queue
    // - used only by http.sys server under Windows, not by socket-based servers
    property HttpSysQueueName: SynUnicode
      read fHttpSysQueueName write fHttpSysQueueName;
    /// the value of a custom HTTP header containing the real client IP
    // - by default, the RemoteIP information will be retrieved from the socket
    // layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the true remote client
    // IP value, mostly as 'X-Real-IP' or 'X-Forwarded-For'
    property RemoteIPHeader: RawUtf8
      read fRemoteIPHeader write fRemoteIPHeader;
    /// enable NGINX X-Accel internal redirection for STATICFILE_CONTENT_TYPE
    // - supplied value is passed as argument to THttpServer.NginxSendFileFrom()
    // - used only by the socket-based servers, not http.sys server on Windows
    property NginxSendFileFrom: TFileName
      read fNginxSendFileFrom write fNginxSendFileFrom;
    /// if defined, this HTTP server will use WebSockets, and our secure
    // encrypted binary protocol
    // - when stored in the settings JSON file, the password will be safely
    // encrypted as defined by TSynPersistentWithPassword
    // - use the inherited PlainPassword property to set or read its value
    property WebSocketPassword: SpiUtf8
      read fPassWord write fPassWord;
    /// customize the TRestHttpServer low-level process
    property Options: TRestHttpServerOptions
      read fOptions write fOptions;
  end;

const
  /// default TRestHttpServer processing options are to support SynLZ and GZip
  HTTPSERVER_DEFAULT_OPTIONS =
    [rsoCompressGZip,
     rsoCompressSynLZ];

  /// TRestHttpServer processing options which may be used during raw debugging
  HTTPSERVER_DEBUG_OPTIONS =
    [rsoCompressGZip,
     rsoCompressSynLZ,
     rsoLogVerbose];



// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRestServer = TRestServer;
  TSqlRestServerClass = TRestServerClass;
  TSqlRestServerUriContext = TRestServerUriContext;
  TSqlRestServerUriContextClass = TRestServerUriContextClass;
  TSqlRestServerAuthenticationClass = TRestServerAuthenticationClass;
  TSqlRestServerAuthenticationNone  = TRestServerAuthenticationNone;
  TSqlRestServerAuthenticationDefault = TRestServerAuthenticationDefault;
  TSqlRestServerAuthenticationHttpBasic = TRestServerAuthenticationHttpBasic;

{$endif PUREMORMOT2}


implementation

uses
  // defined here to avoid any circular reference
  mormot.soa.server,
  mormot.orm.server, // defines e.g. TRestOrmServer
  mormot.orm.storage;


{ ************ TRestServerUriContext Access to the Server-Side Execution }

function ToText(res: TOnAuthenticationFailedReason): PShortString;
begin
  result := GetEnumName(TypeInfo(TOnAuthenticationFailedReason), ord(res));
end;

function ServiceRunningContext: PServiceRunningContext;
begin
  result := PerThreadRunningContextAddress; // from mormot.core.interfaces.pas
end;

function ToText(n: TRestNode): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestNode), ord(n));
end;


{ TRestServerUriContext }

constructor TRestServerUriContext.Create(aServer: TRestServer;
  const aCall: TRestUriParams);
begin
  inherited Create(aCall);
  fServer := aServer;
  fThreadServer := PerThreadRunningContextAddress;
  fThreadServer^.Request := self;
  fMethodIndex := -1;
end;

destructor TRestServerUriContext.Destroy;
begin
  if fThreadServer <> nil then
    fThreadServer^.Request := nil;
  inherited Destroy;
end;

procedure TRestServerUriContext.SetOutSetCookie(const aOutSetCookie: RawUtf8);
const
  HTTPONLY: array[boolean] of string[15] = (
    '; HttpOnly', '');
begin
  inherited SetOutSetCookie(aOutSetCookie);
  if StrPosI('; PATH=', pointer(fOutSetCookie)) = nil then
    fOutSetCookie := FormatUtf8('%; Path=/%%', [fOutSetCookie, Server.fModel.Root,
      HTTPONLY[rsoCookieHttpOnlyFlagDisable in Server.fOptions]]);
end;

procedure TRestServerUriContext.OutHeadFromCookie;
begin
  inherited OutHeadFromCookie;
  if rsoCookieIncludeRootPath in Server.fOptions then
    // case-sensitive Path=/ModelRoot
    fCall^.OutHead := fCall^.OutHead + '; Path=/';
end;

procedure TRestServerUriContext.InternalSetTableFromTableName(
  TableName: PUtf8Char; TableNameLen: PtrInt);
begin
  fTableEngine := TRestOrm(Server.fOrmInstance);
  if rsoNoTableURI in Server.Options then
    fTableIndex := -1
  else
    InternalSetTableFromTableIndex(
      Server.fModel.GetTableIndexPtrLen(TableName, TableNameLen));
  if fTableIndex < 0 then
    exit;
  fStaticOrm := TRestOrmServer(Server.fOrmInstance).
    GetStaticTableIndex(TableIndex, fStaticKind);
  if fStaticOrm <> nil then
    fTableEngine := StaticOrm;
end;

procedure TRestServerUriContext.InternalSetTableFromTableIndex(Index: PtrInt);
begin
  fTableIndex := Index;
  if Index >= 0 then
    with Server.fModel do
    begin
      self.fTable := Tables[Index];
      self.fTableModelProps := TableProps[Index];
    end;
end;

function TRestServerUriContext.UriWithoutSignature: RawUtf8;
var
  len: PtrInt;
begin
  len := fUriSessionSignaturePos;
  if len = 0 then
    result := Call^.Url
  else
    FastSetString(result, pointer(Call^.Url), len - 1); // exclude ? or &
end;

function TRestServerUriContext.UriWithoutRoot: RawUtf8;
var
  pos: PtrInt;
begin
  pos := Server.Model.RootLen + 2;
  if Call^.Url[1] = '/' then
    inc(pos); // trim leading '/' in '/root' (may happen when called in-process)
  result := copy(Call^.Url, pos, maxInt);
end;

function TRestServerUriContext.UriWithoutInlinedParams: shortstring;
var
  urllen, len: PtrUInt;
begin
  urllen := length(Call^.Url);
  len := urllen;
  if fParameters <> nil then
  begin
    len := fParameters - pointer(Call^.Url) - 1;
    if len > urllen then // from InBody CONTENT_TYPE_WEBFORM, not from Url
      len := urllen;
  end;
  SetString(result, PAnsiChar(pointer(Call^.Url)), len);
end;

procedure TRestServerUriContext.SessionAssign(AuthSession: TAuthSession);
begin
  // touch the TAuthSession deprecation timestamp
  AuthSession.fTimeOutTix := (TickCount64 shr 10) + AuthSession.TimeoutShr10;
  // make local copy of TAuthSession information
  fSession := AuthSession.ID;
  fSessionOS := AuthSession.fRemoteOsVersion;
  fSessionUser := AuthSession.User.IDValue;
  fSessionGroup := AuthSession.User.GroupRights.IDValue;
  fSessionUserName := AuthSession.User.LogonName;
  fSessionAccessRights := AuthSession.fAccessRights;
  if (AuthSession.RemoteIP <> '') and
     (fCall^.LowLevelRemoteIP = '') then
    fCall^.LowLevelRemoteIP := AuthSession.RemoteIP;
  fCall^.RestAccessRights := @fSessionAccessRights;
  fAuthSession := AuthSession; // for TRestServer internal use only
end;

var
  // as set by TRestServer.AdministrationExecute()
  BYPASS_ACCESS_RIGHTS: TOrmAccessRights;

function TRestServerUriContext.IsRemoteAdministrationExecute: boolean;
begin
  result := (self <> nil) and
            (fCall^.RestAccessRights = @BYPASS_ACCESS_RIGHTS);
end;

function TRestServerUriContext.Authenticate: boolean;
var
  s: TAuthSession;
  a: ^TRestServerAuthentication;
  n: integer;
begin
  result := true;
  if Server.fHandleAuthentication and
     not IsRemoteAdministrationExecute then
  begin
    fSession := CONST_AUTHENTICATION_SESSION_NOT_STARTED;
    if // /auth + /timestamp are e.g. allowed methods without signature
       ((MethodIndex >= 0) and
        Server.fPublishedMethod[MethodIndex].ByPassAuthentication) or
       // you can allow a service to be called directly
       ((Service <> nil) and
        TServiceFactoryServerAbstract(Service).ByPassAuthentication) or
       // allow by-pass for a set of HTTP verbs (e.g. mGET)
       ((Table <> nil) and
        (Method in Server.BypassOrmAuthentication)) then
      // no need to check the sessions
      exit;
    if (rsoSessionInConnectionOpaque in Server.Options) and
       (Call^.LowLevelConnectionOpaque <> nil) then
    begin
      // TAuthSession instance may have been stored at connection level
      // to avoid signature parsing and session lookup
      s := pointer(Call^.LowLevelConnectionOpaque^.ValueInternal);
      if s <> nil then
        if s.InheritsFrom(Server.fSessionClass) then
        begin
          SessionAssign(s);
          exit;
        end
        else
          Call^.LowLevelConnectionOpaque^.ValueInternal := 0; // paranoid
    end;
    Server.fSessions.Safe.ReadOnlyLock; // allow concurrent authentication
    try
      a := pointer(Server.fSessionAuthentication);
      if a <> nil then
      begin
        n := PDALen(PAnsiChar(a) - _DALEN)^ + _DAOFF;
        repeat
          s := a^.RetrieveSession(self); // retrieve from URI or cookie
          if s <> nil then
          begin
            if (Log <> nil) and
               (s.RemoteIP <> '') and
               (s.RemoteIP <> '127.0.0.1') then
              Log.Log(sllUserAuth, '%/% %',
                [s.User.LogonName, s.ID, s.RemoteIP], self);
            exit;
          end;
          inc(a);
          dec(n);
        until n = 0;
      end;
    finally
      Server.fSessions.Safe.ReadOnlyUnLock;
    end;
    // if we reached here, no session has been identified
    result := false;
  end
  else
    // default unique session if authentication is not enabled
    fSession := CONST_AUTHENTICATION_NOT_USED;
end;

procedure TRestServerUriContext.AuthenticationFailed(
  Reason: TOnAuthenticationFailedReason);
var
  txt: PShortString;
begin
  txt := ToText(Reason);
  if Log <> nil then
    Log.Log(sllUserAuth, 'AuthenticationFailed(%) for % (session=%)',
      [txt^, Call^.Url, Session], self);
  // 401 HTTP_UNAUTHORIZED must include a WWW-Authenticate header, so return 403
  fCall^.OutStatus := HTTP_FORBIDDEN;
  FormatUtf8('Authentication Failed: % (%)',
    [UnCamelCase(TrimLeftLowerCaseShort(txt)), ord(Reason)], fCustomErrorMsg);
  // call the notification event
  if Assigned(Server.OnAuthenticationFailed) then
    Server.OnAuthenticationFailed(Server, Reason, nil, self);
end;

procedure TRestServerUriContext.ExecuteCommand;

  procedure TimeOut;
  begin
    Server.InternalLog('TimeOut %.Execute(%) after % ms',
      [self, ToText(Command)^,
       Server.fAcquireExecution[Command].LockedTimeOut], sllServer);
    if Call <> nil then
      Call^.OutStatus := HTTP_TIMEOUT; // 408 Request Time-out
  end;

var
  method: TThreadMethod;
  tix, endtix: Int64;
  ms, current: cardinal;
  exec: PRestAcquireExecution;
begin
  exec := @Server.fAcquireExecution[Command];
  ms := exec^.LockedTimeOut;
  if ms = 0 then
    ms := 10000; // never wait forever = 10 seconds max
  case Command of
    execSoaByMethod:
      method := ExecuteSoaByMethod;
    execSoaByInterface:
      method := ExecuteSoaByInterface;
    execOrmGet:
      method := ExecuteOrmGet;
    execOrmWrite:
      begin
        // special behavior to handle transactions at writing
        method := ExecuteOrmWrite;
        endtix := TickCount64 + ms;
        while true do
          if exec^.Safe.TryLockMS(ms, @Server.fShutdownRequested) then
            try
              current := TRestOrm(Server.fOrmInstance).TransactionActiveSession;
              if (current = 0) or
                 (current = Session) then
              begin
                // avoiding transaction mixups
                if exec^.Mode = amLocked then
                begin
                  ExecuteOrmWrite; // process within the obtained write mutex
                  exit;
                end;
                break;   // will handle Mode<>amLocked below
              end;
              // if we reached here, there is a transaction on another session
              tix := GetTickCount64; // not self.TickCount64 which is fixed
              if tix > endtix then
              begin
                TimeOut; // we were not able to acquire the transaction
                exit;
              end;
              ms := endtix - tix;
            finally
              exec^.Safe.UnLock;
            end
          else
            begin
              TimeOut;
              exit;
            end;
      end;
  else
    raise EOrmException.CreateUtf8('Unexpected Command=% in %.Execute',
      [ord(Command), self]);
  end;
  if exec^.Mode = amBackgroundOrmSharedThread then
    if (Command = execOrmWrite) and
       (Server.fAcquireExecution[execOrmGet].Mode = amBackgroundOrmSharedThread) then
      fCommand := execOrmGet; // both ORM read+write will share the read thread
  case exec^.Mode of
    amUnlocked:
      method;
    amLocked:
      if exec^.Safe.TryLockMS(ms, @Server.fShutdownRequested) then
        try
          method;
        finally
          exec^.Safe.UnLock;
        end
      else
        TimeOut;
    amMainThread:
      BackgroundExecuteThreadMethod(method, nil);
    amBackgroundThread,
    amBackgroundOrmSharedThread:
      begin
        if exec^.Thread = nil then
          exec^.Thread := Server.Run.NewBackgroundThreadMethod('% % %',
            [self, Server.fModel.Root, ToText(Command)^]);
        BackgroundExecuteThreadMethod(method, exec^.Thread);
      end;
  end;
end;

procedure TRestServerUriContext.ConfigurationRestMethod(SettingsStorage: TObject);
var
  value: TDocVariantData;
  valid: boolean;
  config: variant;
begin
  fUriMethodPath := StringReplaceChars(fUriMethodPath, '/', '.');
  if InputExists['value'] then
  begin
    if fUriMethodPath = '' then
      exit;
    value.InitObjectFromPath(fUriMethodPath, Input['value']);
    JsonToObject(SettingsStorage, pointer(value.ToJson), valid, nil,
      JSONPARSER_TOLERANTOPTIONS);
    if not valid then
    begin
      Error('Invalid input [%] - expected %', [variant(value),
        ClassFieldNamesAllPropsAsText(SettingsStorage.ClassType, true)]);
      exit;
    end;
  end;
  ObjectToVariant(SettingsStorage, config, [woDontStoreDefault]);
  if fUriMethodPath <> '' then
    config := TDocVariantData(config).GetValueByPath(fUriMethodPath);
  ReturnsJson(config, HTTP_SUCCESS, true, twJsonEscape, true);
end;

procedure TRestServerUriContext.StatsFromContext(Stats: TSynMonitorInputOutput;
  MicroSec: Int64);
begin
  if (self = nil) or
     (Stats = nil) then
    exit;
  if fStatsInSize = 0 then
  begin
    // rough estimation - but compute it once
    fStatsInSize := length(fCall^.Url) + length(fCall^.Method) +
      length(fCall^.InHead) + length(fCall^.InBody) + 12;
    fStatsOutSize := length(fCall^.OutHead) + length(fCall^.OutBody) + 16;
  end;
  // set all TSynMonitorInputOutput fields in a single TLightLock
  Stats.Notify(fStatsInSize, fStatsOutSize, MicroSec, fCall^.OutStatus);
end;

procedure TRestServerUriContext.LogFromContext;
const
  COMMANDTEXT: array[TRestServerUriContextCommand] of string[15] = (
    '?', 'Method', 'Interface', 'Read', 'Write');
begin
  if sllServer in fLog.GenericFamily.Level then
    fLog.Log(sllServer, '% % % % %=% out=% in %', [SessionUserName,
      RemoteIPNotLocal, COMMANDTEXT[fCommand], fCall.Method,
      UriWithoutInlinedParams, fCall.OutStatus, KB(fCall.OutBody),
      MicroSecToString(fMicroSecondsElapsed)]);
  if (fCall.OutBody <> '') and
     not (optNoLogOutput in fServiceExecutionOptions) and
     (sllServiceReturn in fLog.GenericFamily.Level) and
     (fCall.OutHead = '') or
      IsHtmlContentTypeTextual(pointer(fCall.OutHead)) then
    fLog.Log(sllServiceReturn, fCall.OutBody, self, MAX_SIZE_RESPONSE_LOG);
end;

procedure TRestServerUriContext.ExecuteCallback(var Ctxt: TJsonParserContext;
  ParamInterfaceInfo: TRttiJson; out Obj);
var
  fakeid: PtrInt;
begin
  if not Assigned(Server.OnNotifyCallback) then
    raise EServiceException.CreateUtf8('% does not implement callbacks for I%',
      [Server, ParamInterfaceInfo.Name]);
  // Par is the callback ID transmitted from the client side
  fakeid := Ctxt.ParseInteger;
  if Ctxt.Json = nil then
    Ctxt.Json := @NULCHAR; // allow e.g. '[12345]' (single interface parameter)
  if (fakeid = 0) or
     (ParamInterfaceInfo.Info = TypeInfo(IInvokable)) then
  begin
    pointer(Obj) := pointer(fakeid); // special call Obj = IInvokable(fakeid)
    exit;
  end;
  // let TServiceContainerServer
  (Server.Services as TServiceContainerServer).GetFakeCallback(
    self, ParamInterfaceInfo.Info, fakeid, Obj);
end;

procedure TRestServerUriContext.ComputeStatsAfterCommand;
var
  ms: Int64;
  m: PtrInt;
  stat: ^TSynMonitorInputOutput;
begin
  QueryPerformanceMicroSeconds(ms);
  dec(ms, fMicroSecondsStart); // ms = time elapsed in micro seconds
  fMicroSecondsElapsed := ms;
  if mlUri in Server.StatLevels then
    StatsFromContext(Server.fStats, ms);
  case fCommand of
    execOrmGet:
      if mlTables in Server.fStatLevels then
        Server.fStats.NotifyOrmTable(fTableIndex, length(Call.OutBody), false, ms);
    execOrmWrite:
      if mlTables in Server.fStatLevels then
        Server.fStats.NotifyOrmTable(fTableIndex, length(Call.InBody), true, ms);
    execSoaByMethod:
      if mlMethods in Server.fStatLevels then
      begin
        if fServerMethod^.Stats = nil then
          Server.fStats.CreateNotify(fServerMethod^.Stats, fServerMethod^.Name);
        StatsFromContext(fServerMethod^.Stats, ms);
        if Server.fStatUsage <> nil then
          Server.fStatUsage.Modified(fServerMethod^.Stats, []);
        if (mlSessions in Server.fStatLevels) and
           (fAuthSession <> nil) then
        begin
          if fAuthSession.fMethods = nil then
            Server.fStats.CreateNotifyAuthSession(fAuthSession.fMethods);
          stat := @fAuthSession.fMethods[MethodIndex];
          if stat^ = nil then
            Server.fStats.CreateNotify(stat^, fServerMethod^.Name);
          StatsFromContext(stat^, ms);
        end;
      end;
    execSoaByInterface:
      if mlInterfaces in Server.fStatLevels then
      begin
        m := fServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT;
        if m >= 0 then
        begin
          stat := @TServiceFactoryServer(fService).Stats[m];
          if stat^ = nil then
            Server.fStats.CreateNotify(stat^, fServiceMethod^.InterfaceDotMethodName);
          StatsFromContext(stat^, ms);
          if Server.StatUsage <> nil then
            Server.StatUsage.Modified(stat^, []);
          if (mlSessions in Server.fStatLevels) and
             (fAuthSession <> nil) then
            fAuthSession.NotifyInterfaces(self, ms);
        end;
      end;
  end;
  Server.fStats.AddCurrentRequestCount(-1);
  if Server.fStatUsage <> nil then
    Server.fStatUsage.Modified(Server.fStats, []);
end;

procedure TRestServerUriContext.ExecuteSoaByMethod;
begin
  fServerMethod^.CallBack(self);
  LockedInc64(@Server.fStats.fServiceMethod);
end;

procedure TRestServerUriContext.ServiceResultStart(WR: TJsonWriter);
const
  JSONSTART: array[boolean] of string[15] = (
    '{"result":[', '{"result":{');
begin
  // InternalExecuteSoaByInterface has set ForceServiceResultAsJsonObject
  if ForceServiceResultAsJsonObjectWithoutResult then
    WR.Add('{')
  else
    WR.AddShort(JSONSTART[ForceServiceResultAsJsonObject]);
end;

procedure TRestServerUriContext.ServiceResultEnd(WR: TJsonWriter; ID: TID);
const
  JSONSEND_WITHID: array[boolean] of string[7] = (
    '],"id":', '},"id":');
  JSONSEND_NOID: array[boolean] of AnsiChar = (
    ']', '}');
begin
  // InternalExecuteSoaByInterface has set ForceServiceResultAsJsonObject
  if ID = 0 then
    WR.Add(JSONSEND_NOID[ForceServiceResultAsJsonObject])
  else
  begin
    if ForceServiceResultAsJsonObjectWithoutResult then
      raise EServiceException.CreateUtf8('%.ServiceResultEnd(ID=%) with ' +
        'ForceServiceResultAsJsonObjectWithoutResult', [self, ID]);
    WR.AddShorter(JSONSEND_WITHID[ForceServiceResultAsJsonObject]);
    WR.Add(ID); // only used in sicClientDriven mode
  end;
  if not ForceServiceResultAsJsonObjectWithoutResult then
    WR.Add('}');
end;

procedure TRestServerUriContext.ServiceResult(const Name, JsonValue: RawUtf8);
var
  wr: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  wr := TJsonWriter.CreateOwnedStream(temp);
  try
    ServiceResultStart(wr);
    if ForceServiceResultAsJsonObject then
      wr.AddFieldName(Name);
    wr.AddString(JsonValue);
    ServiceResultEnd(wr, 0);
    Returns(wr.Text);
  finally
    wr.Free;
  end;
end;

procedure TRestServerUriContext.InternalExecuteSoaByInterfaceComputeResult;
var
  s: TServiceFactoryServer;
begin
  s := TServiceFactoryServer(Service);
  // XML needs a full JSON object as input
  fForceServiceResultAsXMLObject :=
    fForceServiceResultAsXMLObject or
    s.ResultAsXMLObject;
  fForceServiceResultAsJsonObject :=
    fForceServiceResultAsJsonObject or
    s.ResultAsJsonObject or
    s.ResultAsJsonObjectWithoutResult or
    ForceServiceResultAsXMLObject;
  fForceServiceResultAsJsonObjectWithoutResult :=
    ForceServiceResultAsJsonObject and
    (s.InstanceCreation in SERVICE_IMPLEMENTATION_NOID) and
    s.ResultAsJsonObjectWithoutResult;
  if (fForceServiceResultAsXMLObjectNameSpace = '') and
     (s.ResultAsXMLObjectNameSpace <> '') then
    fForceServiceResultAsXMLObjectNameSpace := s.ResultAsXMLObjectNameSpace;
  LockedInc64(@Server.fStats.fServiceInterface);
  case ServiceMethodIndex of // 0..3 for pseudo-methods
    ord(imFree):
      // {"method":"_free_", "params":[], "id":1234}
      if s.InstanceCreation in SERVICE_IMPLEMENTATION_NOID then
      begin
        Error('_free_ is not compatible with %', [ToText(s.InstanceCreation)^]);
        exit;
      end;
    ord(imContract):
      begin
        // "method":"_contract_" to retrieve the implementation contract
        if (Call^.InBody <> '') and
           (Call^.InBody <> '[]') then
          Server.AssociatedServices.RegisterFromClientJson(Call^.InBody);
        ServiceResult('contract', s.ContractExpected);
        exit; // "id":0 for this method -> no instance was created
      end;
    ord(imSignature):
      begin
        // "method":"_signature_" to retrieve the implementation signature
        if TServiceContainerServer(Server.Services).PublishSignature then
          ServiceResult('signature', s.Contract)
        else
          // "id":0 for this method -> no instance was created
          Error('Not allowed to publish signature');
        exit;
      end;
    ord(imInstance):
      // "method":"_instance_" from TServiceFactoryClient.CreateFakeInstance
      if s.InstanceCreation <> sicClientDriven then
      begin
        Error('_instance_ is not compatible with %',
          [ToText(s.InstanceCreation)^]);
        exit;
      end
      else if ServiceInstanceID <> 0 then
      begin
        Error('_instance_ with ServiceInstanceID=%', [ServiceInstanceID]);
        exit;
      end;
  else
    // TServiceFactoryServer.ExecuteMethod() will use ServiceMethod(Index)
    if ServiceMethod = nil then
      raise EServiceException.CreateUtf8('%.InternalExecuteSoaByInterface: ' +
        'ServiceMethodIndex=% and ServiceMethod=nil', [self, ServiceMethodIndex]);
  end;
  if (Session > CONST_AUTHENTICATION_NOT_USED) and
     (ServiceExecution <> nil) and
     ((SessionGroup <= 0) or
      (SessionGroup > 255) or
      (byte(SessionGroup - 1) in ServiceExecution.Denied)) then
  begin
    Error('Unauthorized method', HTTP_NOTALLOWED);
    exit;
  end;
  // if we reached here, we have to run the service method
  s.ExecuteMethod(self);
end;

procedure TRestServerUriContext.InternalExecuteSoaByInterface;
var
  m: PtrInt;
  spi: TInterfaceMethodValueDirections;
begin
  // expects Service, ServiceParameters, ServiceMethod(Index) to be set
  m := ServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT;
  if m >= 0 then
  begin
    if ServiceMethod = nil then
      ServiceMethod := @Service.InterfaceFactory.Methods[m];
    ServiceExecution := @Service.Execution[m];
    ServiceExecutionOptions := ServiceExecution.Options;
    // log from Ctxt.ServiceExecutionOptions
    spi := ServiceMethod^.HasSpiParams;
    if spi <> [] then
    begin
      if [imdConst, imdVar] * spi <> [] then
        include(fServiceExecutionOptions, optNoLogInput);
      if [imdVar, imdOut, imdResult] * spi <> [] then
        include(fServiceExecutionOptions, optNoLogOutput);
    end;
    // log method call and parameter values (if worth it)
    if (Log <> nil) and
       (sllServiceCall in Log.GenericFamily.Level) and
       (ServiceParameters <> nil) and
       (PWord(ServiceParameters)^ <> ord('[') + ord(']') shl 8) then
     if optNoLogInput in ServiceExecutionOptions then
       Log.Log(sllServiceCall, '%{}',
         [ServiceMethod^.InterfaceDotMethodName], Server)
     else
       Log.Log(sllServiceCall, '%%',
         [ServiceMethod^.InterfaceDotMethodName, ServiceParameters], Server);
    // OnMethodExecute() callback event
    if Assigned(TServiceFactoryServer(Service).OnMethodExecute) then
      if not TServiceFactoryServer(Service).
               OnMethodExecute(self, ServiceMethod^) then
        exit; // execution aborted by the callback
  end;
  if TServiceFactoryServer(Service).ResultAsXMLObjectIfAcceptOnlyXML and
     FindNameValue(Call^.InHead, 'ACCEPT:', fTemp) and
     (IdemPropNameU(fTemp, 'application/xml') or
      IdemPropNameU(fTemp, 'text/xml')) then
    fForceServiceResultAsXMLObject := true;
  try
    InternalExecuteSoaByInterfaceComputeResult;
  finally
    // ensure no GPF later if points to some local data
    ServiceParameters := nil;
  end;
  // optionnally convert result to XML
  if fForceServiceResultAsXMLObject and
     (fCall^.OutBody <> '') and
     (fCall^.OutHead <> '') and
     CompareMemFixed(pointer(fCall^.OutHead),
       pointer(JSON_CONTENT_TYPE_HEADER_VAR), length(JSON_CONTENT_TYPE_HEADER)) then
  begin
    // replace application/json by text/xml
    delete(fCall^.OutHead, length(HEADER_CONTENT_TYPE) + 1, length(JSON_CONTENT_TYPE));
    insert(XML_CONTENT_TYPE, fCall^.OutHead, length(HEADER_CONTENT_TYPE) + 1);
    JsonBufferToXML(pointer(fCall^.OutBody), XMLUTF8_HEADER,
      ForceServiceResultAsXMLObjectNameSpace, fTemp);
    fCall^.OutBody := fTemp;
  end;
end;

function TRestServerUriContext.CanExecuteOrmWrite(Method: TUriMethod;
  Table: TOrmClass; TableIndex: integer; const TableID: TID;
  const Rights: TOrmAccessRights): boolean;
begin
  result := true;
  case Method of
    mPOST:
      // POST=ADD=INSERT
      if Table <> nil then
        // ExecuteOrmWrite will check reSQL access right
        result := (TableIndex in Rights.POST);
    mPUT:
      // PUT=UPDATE
      result := (Table <> nil) and
        ((TableIndex in Rights.PUT) or
         ((TableID > 0) and
          (Session > CONST_AUTHENTICATION_NOT_USED) and
          (Table = Server.fAuthUserClass) and
          (TableID = SessionUser) and
          (reUserCanChangeOwnPassword in Rights.AllowRemoteExecute)));
    mDELETE:
      result := (Table <> nil) and
        (TableIndex in Rights.DELETE) and
        ((TableID > 0) or
         (reUrlEncodedDelete in Rights.AllowRemoteExecute));
  end;
end;

const
  METHOD_WRITE: array[0..3] of PUtf8Char = (
   'INSERT', // mPOST
   'UPDATE', // mPUT
   'DELETE', // mDELETE
   nil);

procedure TRestServerUriContext.ExecuteOrmGet;

  procedure ConvertOutBodyAsPlainJson(const FieldsCsv: RawUtf8;
    Options: TOrmWriterOptions);
  var
    rec: TOrm;
    W: TOrmWriter;
    bits: TFieldBits;
    withid: boolean;
    tmp: TTextWriterStackBuffer;
  begin
    // force plain standard JSON output for AJAX clients
    if (FieldsCsv = '') or
       // handle ID single field only if ID_str is needed
       (IsRowID(pointer(FieldsCsv)) and
        not (owoID_str in Options)) or
       // we won't handle min()/max() functions
       not TableModelProps.Props.FieldBitsFromCsv(FieldsCsv, bits, withid) then
      exit;
    rec := Table.CreateAndFillPrepare(fCall^.OutBody);
    try
      W := TableModelProps.Props.CreateJsonWriter(TRawByteStringStream.Create,
        true, FieldsCsv, {knownrows=}0, 0, @tmp);
      try
        W.CustomOptions := W.CustomOptions + [twoForceJsonStandard]; // regular JSON
        W.OrmOptions := Options; // SetOrmOptions() may refine ColNames[]
        rec.AppendFillAsJsonValues(W);
        W.SetText(fCall^.OutBody);
      finally
        W.Stream.Free; // associated TRawByteStringStream instance
        W.Free;
      end;
    finally
      rec.Free;
    end;
  end;

var
  sqlselect, sqlwhere, sqlwherecount, sqlsort, sqldir, sql: RawUtf8;
  sqlstartindex, sqlresults, sqltotalrowcount: integer;
  nonstandardsqlparameter, nonstandardsqlwhereparameter: boolean;
  paging: PRestServerUriPagingParameters;
  sqlisselect: boolean;
  resultlist: TOrmTable;
  tableindexes: TIntegerDynArray;
  rec: TOrm;
  opt: TOrmWriterOptions;
  P, params: PUtf8Char;
  i, j, L: PtrInt;
  cache: TOrmCache;
begin
  params := fParameters;
  case Method of
    mLOCK,
    mGET:
      begin
        if Table = nil then
        begin
          if Method <> mLOCK then
          begin
            if (fCall^.InBody = '') and
               (params <> nil) and
               (reUrlEncodedSql in fCall^.RestAccessRights^.AllowRemoteExecute) then
            begin
              // GET with a sql statement sent in URI, as sql=....
              while not UrlDecodeValue(params, 'sql=', sql, @params) do
                if params = nil then
                  break;
            end
            else
              // GET with a sql statement sent as UTF-8 body (not 100% HTTP compatible)
              sql := fCall^.InBody;
            if sql <> '' then
            begin
              sqlisselect := IsSelect(pointer(sql), @sqlselect);
              if sqlisselect or
                 (reSql in fCall^.RestAccessRights^.AllowRemoteExecute) then
              begin
                fStaticOrm := nil;
                if sqlisselect then
                begin
                  tableindexes := Server.fModel.GetTableIndexesFromSqlSelect(sql);
                  if tableindexes = nil then
                  begin
                    // check for SELECT without any known table
                    if not (reSqlSelectWithoutTable in
                        fCall^.RestAccessRights^.AllowRemoteExecute) then
                    begin
                      fCall^.OutStatus := HTTP_NOTALLOWED;
                      exit;
                    end;
                  end
                  else
                  begin
                    // check for SELECT with one (or several JOINed) tables
                    for i := 0 to length(tableindexes) - 1 do
                      if not (tableindexes[i] in fCall^.RestAccessRights^.GET) then
                      begin
                        fCall^.OutStatus := HTTP_NOTALLOWED;
                        exit;
                      end;
                    // use the first static table (poorman's JOIN)
                    fStaticOrm := TRestOrmServer(Server.fOrmInstance).
                      InternalAdaptSql(tableindexes[0], sql);
                  end;
                end;
                if StaticOrm <> nil then
                begin
                  fTableEngine := StaticOrm;
                  fCall^.OutBody := TableEngine.EngineList(sql);
                end
                else
                  fCall^.OutBody := TRestOrmServer(Server.fOrmInstance).
                    MainEngineList(sql, false, nil);
                // security note: only first statement is run by EngineList()
                if fCall^.OutBody <> '' then
                begin
                  // got JSON list '[{...}]' ?
                  if (sqlselect <> '') and
                     (length(tableindexes) = 1) then
                  begin
                    InternalSetTableFromTableIndex(tableindexes[0]);
                    opt := ClientOrmOptions;
                    if opt <> [] then
                      ConvertOutBodyAsPlainJson(sqlselect, opt);
                  end;
                  fCall^.OutStatus := HTTP_SUCCESS;  // 200 OK
                  if not sqlisselect then
                    // needed for fStats.NotifyOrm(Method) below
                    fMethod := TUriMethod(IdemPPChar(SqlBegin(pointer(sql)),
                      @METHOD_WRITE) + 2); // -1+2 -> mGET=1
                end;
              end;
            end;
          end;
        end
        else
        // here, Table<>nil and TableIndex in [0..MAX_TABLES-1]
        if not (TableIndex in fCall^.RestAccessRights^.GET) then
          // rejected from User Access
          fCall^.OutStatus := HTTP_NOTALLOWED
        else
        begin
          if TableID > 0 then
          begin
            // GET ModelRoot/TableName/TableID[/Blob] to retrieve one member,
            // with or without locking, or a specified blob field content
            if Method = mLOCK then
              // LOCK is to be followed by PUT -> check user
              if not (TableIndex in fCall^.RestAccessRights^.PUT) then
                fCall^.OutStatus := HTTP_NOTALLOWED
              else if Server.fModel.Lock(TableIndex, TableID) then
                fMethod := mGET; // mark successfully locked
            if fMethod <> mLOCK then
              if fUriBlobField <> nil then
              begin
                // GET ModelRoot/TableName/TableID/Blob: retrieve blob content
                if TableEngine.EngineRetrieveBlob(TableIndex, TableID,
                    fUriBlobField.PropInfo, RawBlob(fCall^.OutBody)) then
                begin
                  fCall^.OutHead := GetMimeContentTypeHeader(fCall^.OutBody);
                  fCall^.OutStatus := HTTP_SUCCESS; // 200 OK
                end
                else
                  fCall^.OutStatus := HTTP_NOTFOUND;
              end
              else
              begin
                // GET ModelRoot/TableName/TableID: retrieve a member content, JSON encoded
                cache := TRestOrm(Server.fOrmInstance).CacheOrNil;
                fCall^.OutBody := cache.RetrieveJson(Table, TableIndex, TableID);
                if fCall^.OutBody = '' then
                begin
                  // get JSON object '{...}'
                  if StaticOrm <> nil then
                    fCall^.OutBody := StaticOrm.EngineRetrieve(TableIndex, TableID)
                  else
                    fCall^.OutBody := TRestOrmServer(Server.fOrmInstance).
                      MainEngineRetrieve(TableIndex, TableID);
                  // cache if expected
                  if cache <> nil then
                    if fCall^.OutBody = '' then
                      cache.NotifyDeletion(TableIndex, TableID)
                    else
                      cache.NotifyJson(Table, TableIndex, TableID, fCall^.OutBody);
                end;
                if fCall^.OutBody <> '' then
                begin
                  // if something was found
                  opt := ClientOrmOptions;
                  if opt <> [] then
                  begin
                    // cached? -> make private copy, with proper opt layout
                    rec := Table.CreateFrom(fCall^.OutBody);
                    try
                      fCall^.OutBody := rec.GetJsonValues(
                        {expand=}true, {withid=}true, ooSelect, nil, opt);
                    finally
                      rec.Free;
                    end;
                  end;
                  fCall^.OutStatus := HTTP_SUCCESS; // 200 OK
                end
                else
                  fCall^.OutStatus := HTTP_NOTFOUND;
              end;
          end
          else
          // ModelRoot/TableName with 'select=..&where=' or YUI paging
          if Method <> mLOCK then
          begin
            // Safe.Lock not available here
            sqlselect := ROWID_TXT; // if no select is specified (i.e. ModelRoot/TableName)
            // all IDs of this table are returned to the client
            sqltotalrowcount := 0;
            if params <> nil then
            begin
              // '?select=...&where=...' or '?where=...'
              sqlstartindex := 0;
              sqlresults := 0;
              if params^ <> #0 then
              begin
                paging := @Server.UriPagingParameters;
                nonstandardsqlparameter :=
                  paging^.Select <> PAGINGPARAMETERS_YAHOO.Select;
                nonstandardsqlwhereparameter :=
                  paging^.Where <> PAGINGPARAMETERS_YAHOO.Where;
                repeat
                  UrlDecodeValue(params, paging^.Sort, sqlsort);
                  UrlDecodeValue(params, paging^.Dir, sqldir);
                  UrlDecodeInteger(params, paging^.StartIndex, sqlstartindex);
                  UrlDecodeInteger(params, paging^.Results, sqlresults);
                  UrlDecodeValue(params, paging^.Select, sqlselect);
                  if nonstandardsqlparameter and
                     (sqlselect = '') then
                    UrlDecodeValue(params, PAGINGPARAMETERS_YAHOO.Select, sqlselect);
                  if nonstandardsqlwhereparameter and
                     ({%H-}sqlwhere = '') then
                    UrlDecodeValue(params, PAGINGPARAMETERS_YAHOO.Where, sqlwhere);
                  UrlDecodeValue(params, paging^.Where, sqlwhere, @params);
                until params = nil;
              end;
              // let SQLite3 do the sort and the paging (will be ignored by Static)
              sqlwherecount := sqlwhere; // "select count(*)" won't expect any ORDER
              if (sqlsort <> '') and
                 (StrPosI('ORDER BY ', pointer(sqlwhere)) = nil) then
              begin
                if SameTextU(sqldir, 'DESC') then
                  // allow DESC, default is ASC
                  sqlsort := sqlsort + ' DESC';
                sqlwhere := sqlwhere + ' ORDER BY ' + sqlsort;
              end;
              TrimSelf(sqlwhere);
              if (sqlresults <> 0) and
                 (StrPosI('LIMIT ', pointer(sqlwhere)) = nil) then
              begin
                if Server.UriPagingParameters.SendTotalRowsCountFmt <> '' then
                begin
                  if sqlwhere = sqlwherecount then
                  begin
                    i := PosEx('ORDER BY ', UpperCase(sqlwherecount));
                    if i > 0 then
                      // if ORDER BY already in the sqlwhere clause
                      SetLength(sqlwherecount, i - 1);
                  end;
                  resultlist := TRestOrmServer(Server.fOrmInstance).
                    ExecuteList([Table], Server.fModel.TableProps[TableIndex].
                      SqlFromSelectWhere('Count(*)', sqlwherecount));
                  if resultlist <> nil then
                  try
                    sqltotalrowcount := resultlist.GetAsInteger(1, 0);
                  finally
                    resultlist.Free;
                  end;
                end;
                sqlwhere := FormatUtf8('% LIMIT % OFFSET %', [sqlwhere,
                  sqlresults, sqlstartindex]);
              end;
            end;
            sql := Server.fModel.TableProps[TableIndex].SqlFromSelectWhere(
              sqlselect, TrimU(sqlwhere));
            fCall^.OutBody := TRestOrmServer(Server.fOrmInstance).
              InternalListRawUtf8(TableIndex, sql);
            if fCall^.OutBody <> '' then
            begin
              // got JSON list '[{...}]' ?
              opt := ClientOrmOptions;
              if opt <> [] then
                ConvertOutBodyAsPlainJson(sqlselect, opt);
              fCall^.OutStatus := HTTP_SUCCESS;  // 200 OK
              if Server.UriPagingParameters.SendTotalRowsCountFmt <> '' then
                // insert "totalRows":% optional value to the JSON output
                if (rsoNoAjaxJson in Server.Options) or
                   (ClientKind = ckFramework) then
                begin
                  // optimized non-expanded mORMot-specific layout
                  P := pointer(fCall^.OutBody);
                  L := length(fCall^.OutBody);
                  P := NotExpandedBufferRowCountPos(P, P + L);
                  j := 0;
                  if P <> nil then
                    j := P - pointer(fCall^.OutBody) - 11
                  else
                    for i := 1 to 10 do
                      if fCall^.OutBody[L] = '}' then
                      begin
                        j := L;
                        break;
                      end
                      else
                        dec(L);
                  if j > 0 then
                    Insert(FormatUtf8(Server.UriPagingParameters.SendTotalRowsCountFmt,
                      [sqltotalrowcount]), fCall^.OutBody, j);
                end
                else
                begin
                  // expanded format -> as {"values":[...],"total":n}
                  if sqltotalrowcount = 0 then // avoid sending fields array
                    fCall^.OutBody := '[]'
                  else
                    TrimSelf(fCall^.OutBody);
                  fCall^.OutBody := '{"values":' + fCall^.OutBody +
                    FormatUtf8(Server.UriPagingParameters.SendTotalRowsCountFmt,
                     [sqltotalrowcount]) + '}';
                end;
            end
            else
              fCall^.OutStatus := HTTP_NOTFOUND;
          end;
        end;
        if fCall^.OutStatus = HTTP_SUCCESS then
          Server.fStats.NotifyOrm(Method);
      end;
    mUNLOCK:
      begin
        // ModelRoot/TableName/TableID to unlock a member
        if not (TableIndex in fCall^.RestAccessRights^.PUT) then
          fCall^.OutStatus := HTTP_NOTALLOWED
        else if (Table <> nil) and
                (TableID > 0) and
           Server.fModel.UnLock(Table, TableID) then
          fCall^.OutStatus := HTTP_SUCCESS; // 200 OK
      end;
    mSTATE:
      begin
        // STATE method for TRestClientServerInternalState
        // this method is called with Root (-> Table=nil -> Static=nil)
        // we need a specialized method in order to avoid fStats.Invalid increase
        fCall^.OutStatus := HTTP_SUCCESS;
        TRestOrmServer(Server.fOrmInstance).RefreshInternalStateFromStatic;
      end
  else
    raise EOrmException.CreateUtf8('%.ExecuteOrmGet(method=%)',
      [self, ord(Method)]);
  end;
end;

procedure TRestServerUriContext.ExecuteOrmWrite;

  procedure ComputeInBodyFields(Occasion: TOrmEvent);
  var
    Rec: TOrm;
    bits: TFieldBits;
  begin
    Rec := Table.Create;
    try
      Rec.FillFrom(pointer(fCall^.InBody), @bits);
      Rec.ComputeFieldsBeforeWrite(Server.Orm, Occasion);
      with TableModelProps.Props do
        if Occasion = oeAdd then
          bits := bits + ComputeBeforeAddFieldsBits
        else
          bits := bits + ComputeBeforeUpdateFieldsBits;
      fServer.OrmInstance.GetJsonValue(
        Rec, Rec.IDValue <> 0, bits, fCall^.InBody);
    finally
      Rec.Free;
    end;
  end;

var
  ok: boolean;
  cache: TOrmCache;
  params: PUtf8Char;
  orm: TRestOrmServer;
  sqlselect, sqlwhere, sqlsort, sqldir: RawUtf8;
begin
  if (MethodIndex >= 0) and
     (MethodIndex = Server.fPublishedMethodBatchIndex) then
  begin
    // run the BATCH process in execOrmWrite context
    ExecuteSoaByMethod;
    exit;
  end;
  if not CanExecuteOrmWrite(Method, Table, TableIndex, TableID,
      fCall^.RestAccessRights^) then
  begin
    fCall^.OutStatus := HTTP_FORBIDDEN;
    exit;
  end;
  params := fParameters;
  orm := TRestOrmServer(Server.fOrmInstance);
  case Method of
    mPOST:
      // POST=ADD=INSERT
      if Table = nil then
      begin
        // ModelRoot with free SQL statement sent as UTF-8 (only for Admin group)
        // see e.g. TRestClientUri.EngineExecute
        if reSQL in fCall^.RestAccessRights^.AllowRemoteExecute then
          if (fCall^.InBody <> '') and
             not (GotoNextNotSpace(Pointer(fCall^.InBody))^ in [#0, '[', '{']) and
             orm.EngineExecute(fCall^.InBody) then
            fCall^.OutStatus := HTTP_SUCCESS // 200 ok
          else
            fCall^.OutStatus := HTTP_FORBIDDEN;
      end
      else
      begin
        // ModelRoot/TableName with possible JSON SentData: create a new member
        // here, Table<>nil, TableID<0 and TableIndex in [0..MAX_TABLES-1]
        if rsoComputeFieldsBeforeWriteOnServerSide in Server.Options then
          ComputeInBodyFields(oeAdd);
        fTableID := TableEngine.EngineAdd(TableIndex, fCall^.InBody);
        if fTableID <> 0 then
        begin
          // 201 Created
          fCall^.OutStatus := HTTP_CREATED;
          FormatUtf8('Location: %/%',
            [fTableModelProps.Props.SqlTableName, fTableID], fCall^.OutHead);
          cache := orm.CacheOrNil;
          if rsoAddUpdateReturnsContent in Server.Options then
          begin
            fCall^.OutBody := TableEngine.EngineRetrieve(TableIndex, fTableID);
            cache.NotifyJson(Table, TableIndex, fTableID, fCall^.OutBody);
          end
          else
            cache.NotifyJson(Table, TableIndex, fTableID, fCall^.InBody);
        end;
      end;
    mPUT:
      // PUT=UPDATE
      if TableID > 0 then
      begin
        // PUT ModelRoot/TableName/TableID[/Blob] to update member/blob content
        if orm.RecordCanBeUpdated(Table, TableID, oeUpdate, @CustomErrorMsg) then
        begin
          if fUriBlobField <> nil then
          begin
            // PUT ModelRoot/TableName/TableID/Blob: update blob field content
            ok := TableEngine.EngineUpdateBlob(
              TableIndex, TableID, fUriBlobField.PropInfo, fCall^.InBody);
          end
          else
          begin
            // ModelRoot/TableName/TableID with JSON SentData: update a member
            if rsoComputeFieldsBeforeWriteOnServerSide in Server.Options then
              ComputeInBodyFields(oeUpdate);
            ok := TableEngine.EngineUpdate(TableIndex, TableID, fCall^.InBody);
            if ok then
            begin
              // flush cache after update (no CreateTime in JSON)
              orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
              if rsoAddUpdateReturnsContent in Server.Options then
                fCall^.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            end;
          end;
          if ok then
            fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
        end
        else
          fCall^.OutStatus := HTTP_FORBIDDEN;
      end
      else if params <> nil then
      begin
        // e.g. from TRestClient.EngineUpdateField
        // PUT ModelRoot/TableName?setname=..&set=..&wherename=..&where=..
        repeat
          UrlDecodeValue(params, 'SETNAME=', sqlselect);
          UrlDecodeValue(params, 'SET=', sqldir);
          UrlDecodeValue(params, 'WHERENAME=', sqlsort);
          UrlDecodeValue(params, 'WHERE=', sqlwhere, @params);
        until params = nil;
        if (sqlselect <> '') and
           (sqldir <> '') and
           (sqlsort <> '') and
           (sqlwhere <> '') then
          if TableEngine.EngineUpdateField(TableIndex, sqlselect, sqldir,
            sqlsort, sqlwhere) then
          begin
            if rsoAddUpdateReturnsContent in Server.Options then
              fCall^.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
          end;
      end;
    mDELETE:
      // DELETE
      if TableID > 0 then
        // ModelRoot/TableName/TableID to delete a member
        if not orm.RecordCanBeUpdated(Table, TableID, oeDelete,
            @CustomErrorMsg) then
          fCall^.OutStatus := HTTP_FORBIDDEN
        else
        begin
          if TableEngine.EngineDelete(TableIndex, TableID) and
             orm.AfterDeleteForceCoherency(TableIndex, TableID) then
          begin
            fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
            orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
          end;
        end
      else if params <> nil then
      begin
        // ModelRoot/TableName?where=WhereClause to delete members
        repeat
          if UrlDecodeValue(params, 'WHERE=', sqlwhere, @params) then
          begin
            TrimSelf(sqlwhere);
            if sqlwhere <> '' then
            begin
              if orm.Delete(Table, sqlwhere) then
                fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
            end;
            break;
          end;
        until params = nil;
      end;
    mBEGIN:
      begin
        // BEGIN TRANSACTION
        // TOrmVirtualTableJson/External will rely on SQLite3 module
        // and also TRestStorageInMemory, since COMMIT/ROLLBACK have Static=nil
        // mBEGIN is the opposite of mEND/mABORT: Safe.Lock main, then static
        if orm.TransactionBegin(Table, Session) then
        begin
          if (StaticOrm <> nil) and
             (StaticKind = sVirtualTable) then
            StaticOrm.TransactionBegin(Table, Session)
          else if (StaticOrm = nil) and
                  (orm.TransactionTable <> nil) then
          begin
            fStaticOrm := pointer(orm.GetVirtualStorage(orm.TransactionTable));
            if fStaticOrm <> nil then
              fStaticOrm.TransactionBegin(Table, Session);
          end;
          fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
        end;
      end;
    mEND:
      begin
        // END=COMMIT
        // this method is called with Root (-> Table=nil -> StaticOrm=nil)
        // mEND logic is the opposite of mBEGIN: release StaticOrm, then main
        if (StaticOrm <> nil) and
           (StaticKind = sVirtualTable) then
          StaticOrm.Commit(Session, false)
        else if (StaticOrm = nil) and
                (orm.TransactionTable <> nil) then
        begin
          fStaticOrm := pointer(orm.GetVirtualStorage(orm.TransactionTable));
          if fStaticOrm <> nil then
            fStaticOrm.Commit(Session, false);
        end;
        orm.Commit(Session, false);
        fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
      end;
    mABORT:
      begin
        // ABORT=ROLLBACK
        // this method is called with Root (-> Table=nil -> StaticOrm=nil)
        // mABORT logic is the opposite of mBEGIN: release StaticOrm, then main
        if (StaticOrm <> nil) and
           (StaticKind = sVirtualTable) then
          StaticOrm.RollBack(Session)
        else if (StaticOrm = nil) and
                (orm.TransactionTable <> nil) then
        begin
          fStaticOrm := pointer(orm.GetVirtualStorage(orm.TransactionTable));
          if fStaticOrm <> nil then
            fStaticOrm.RollBack(Session);
        end;
        orm.RollBack(Session);
        fCall^.OutStatus := HTTP_SUCCESS; // 200 ok
      end;
  end;
  if StatusCodeIsSuccess(fCall^.OutStatus) then
    Server.fStats.NotifyOrm(Method);
end;

const
  // MAX_METHOD_ARGS=128 may not be enough for CONTENT_TYPE_WEBFORM POST
  MAX_INPUT = 512;

procedure TRestServerUriContext.FillInput(const LogInputIdent: RawUtf8);
var
  n, max: PtrInt;
  P: PUtf8Char;
begin
  P := fParameters;
  if (fInput <> nil) or
     (P = nil) then
    // only do it once
    exit;
  n := 0;
  max := 0;
  repeat
    if n >= max then
    begin
      if n >= MAX_INPUT * 2 then
        raise EParsingException.CreateUtf8(
          'Security Policy: Accept up to % parameters for %.FillInput',
          [MAX_INPUT * 2, self]);
      inc(max, NextGrow(max));
      SetLength(fInput, max);
    end;
    if IdemPChar(P, 'SESSION_SIGNATURE=') then
    begin
      // don't include the TAuthSession signature into Input[]
      P := PosChar(P + 18, '&');
      if P = nil then
        break;
      inc(P);
    end
    else
    begin
      // regular name=value pair, to be decoded into Input[]
      P := UrlDecodeNextNameValue(P, fInput[n], fInput[n + 1]);
      if P = nil then
        break;
      inc(n, 2);
    end;
  until P^ = #0;
  if n = 0 then
  begin
    fInput := nil;
    fParameters := nil; // no need to try it again if only session_signature
  end
  else
    DynArrayFakeLength(fInput, n); // SetLength() would make a realloc()
  if (Log <> nil) and
     (LogInputIdent <> '') then
    Log.Add.Log(sllDebug, LogInputIdent, TypeInfo(TRawUtf8DynArray), fInput, self);
end;

function TRestServerUriContext.GetInputInt(const ParamName: RawUtf8): Int64;
var
  err: integer;
  v: RawUtf8;
begin
  GetInputByName(ParamName, 'Int', v);
  result := GetInt64(pointer(v), err);
  if err <> 0 then
    raise EParsingException.CreateUtf8('%.InputInt[%]: ''%'' is not an integer',
      [self, ParamName, v]);
end;

function TRestServerUriContext.GetInputDouble(const ParamName: RawUtf8): Double;
var
  err: integer;
  v: RawUtf8;
begin
  GetInputByName(ParamName, 'Double', v);
  result := GetExtended(pointer(v), err);
  if err <> 0 then
    raise EParsingException.CreateUtf8('%.InputDouble[%]: ''%'' is not a float',
      [self, ParamName, v]);
end;

function TRestServerUriContext.GetInputIntOrVoid(const ParamName: RawUtf8): Int64;
begin
  result := GetInt64(pointer(GetInputUtf8OrVoid(ParamName)));
end;

function TRestServerUriContext.GetInputHexaOrVoid(const ParamName: RawUtf8): cardinal;
var
  value: RawUtf8;
begin
  value := GetInputUtf8OrVoid(ParamName);
  if (length(value) <> 8) or
     not HexDisplayToBin(Pointer(value), @result, SizeOf(result)) then
    result := 0;
end;

function TRestServerUriContext.GetInputDoubleOrVoid(const ParamName: RawUtf8): Double;
begin
  result := GetExtended(pointer(GetInputUtf8OrVoid(ParamName)));
end;

function TRestServerUriContext.GetInputNameIndex(const ParamName: RawUtf8): PtrInt;
var
  P: PRawUtf8;
begin
  // fInput[0]='Param1',fInput[1]='Value1',fInput[2]='Param2'...
  if (fInput = nil) and
     (fParameters <> nil) then
    FillInput;
  P := pointer(fInput);
  for result := 0 to (length(fInput) shr 1) - 1 do
    if IdemPropNameU(ParamName, P^) then
      exit
    else
      inc(P, 2);
  result := -1;
end;

procedure TRestServerUriContext.GetInputByName(
  const ParamName, InputName: RawUtf8; var result: RawUtf8);
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    raise EParsingException.CreateUtf8('%: missing Input%[%]',
      [self, InputName, ParamName]);
  result := fInput[i * 2 + 1];
end;

function TRestServerUriContext.GetInputUtf8(const ParamName: RawUtf8): RawUtf8;
begin
  GetInputByName(ParamName, 'UTF8', result{%H-});
end;

function TRestServerUriContext.GetInputUtf8OrVoid(
  const ParamName: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := ''
  else
    result := fInput[i * 2 + 1];
end;

function TRestServerUriContext.InputUtf8OrDefault(
  const ParamName, DefaultValue: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := DefaultValue
  else
    result := fInput[i * 2 + 1];
end;

function TRestServerUriContext.InputUtf8OrError(const ParamName: RawUtf8;
  out Value: RawUtf8; const ErrorMessageForMissingParameter: string): boolean;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
  begin
    if ErrorMessageForMissingParameter = '' then
      Error('%: missing ''%'' parameter', [self, ParamName])
    else
      Error('%', [ErrorMessageForMissingParameter]);
    result := false;
  end
  else
  begin
    Value := fInput[i * 2 + 1];
    result := true;
  end;
end;

function TRestServerUriContext.InputEnum(const ParamName: RawUtf8;
  EnumType: PRttiInfo; out ValueEnum; DefaultEnumOrd: integer): boolean;
var
  value: RawUtf8;
  int: PtrInt;
  err: integer;
begin
  result := false;
  if (EnumType = nil) or
     (EnumType^.Kind <> rkEnumeration) then
    exit;
  value := GetInputUtf8OrVoid(ParamName);
  if value <> '' then
  begin
    int := GetInteger(Pointer(value), err);
    if err = 0 then
      result := true
    else
    begin
      int := GetEnumNameValue(EnumType, pointer(value), length(value));
      if int >= 0 then
        result := true
      else
        int := DefaultEnumOrd;
    end;
  end
  else
    int := DefaultEnumOrd;
  SetEnumFromOrdinal(EnumType, ValueEnum, int);
end;

function TRestServerUriContext.GetInputString(const ParamName: RawUtf8): string;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    raise EParsingException.CreateUtf8('%: missing InputString[%]',
      [self, ParamName]);
  Utf8ToStringVar(fInput[i * 2 + 1], result);
end;

function TRestServerUriContext.GetInputStringOrVoid(
  const ParamName: RawUtf8): string;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := ''
  else
    Utf8ToStringVar(fInput[i * 2 + 1], result);
end;

function TRestServerUriContext.GetInputExists(const ParamName: RawUtf8): boolean;
begin
  result := GetInputNameIndex(ParamName) >= 0;
end;

function TRestServerUriContext.GetInput(const ParamName: RawUtf8): variant;
var
  v: RawUtf8;
begin
  GetInputByName(ParamName, '', v);
  VariantLoadJson(result, v, nil, fInputAllowDouble);
end;

function TRestServerUriContext.GetInputOrVoid(const ParamName: RawUtf8): variant;
begin
  VariantLoadJson(result, GetInputUtf8OrVoid(ParamName), nil, fInputAllowDouble);
end;

function TRestServerUriContext.InputOrError(const ParamName: RawUtf8;
  out Value: variant; const ErrorMessageForMissingParameter: string): boolean;
var
  v: RawUtf8;
begin
  result := InputUtf8OrError(ParamName, v, ErrorMessageForMissingParameter) and
            VariantLoadJson(Value, v, nil, fInputAllowDouble);
end;

function TRestServerUriContext.GetInputAsTDocVariant(
  const Options: TDocVariantOptions; InterfaceMethod: PInterfaceMethod): variant;
var
  ndx, a: PtrInt;
  forcestring: boolean;
  v: variant;
  multipart: TMultiPartDynArray;
  name: RawUtf8;
  res: TDocVariantData absolute result;
begin
  VarClear(result{%H-});
  FillInput;
  if fInput <> nil then
  begin
    res.Init(Options, dvObject);
    for ndx := 0 to (length(fInput) shr 1) - 1 do
    begin
      name := fInput[ndx * 2];
      if InterfaceMethod <> nil then
      begin
        a := InterfaceMethod.ArgIndex(pointer(name), length(name), {input=}true);
        forcestring := (a >= 0) and
                       (rcfJsonString in InterfaceMethod.Args[a].ArgRtti.Flags);
      end
      else
        forcestring := false;
      GetVariantFromJsonField(pointer(fInput[ndx * 2 + 1]), forcestring, v,
        @Options, fInputAllowDouble, length(fInput[ndx * 2 + 1]));
      res.AddValue(name, v);
    end;
  end
  else if InputAsMultiPart(multipart) then
    MultiPartToDocVariant(multipart, res, @Options);
end;

function TRestServerUriContext.IsRemoteIPBanned: boolean;
begin
  if Server.fIPBan.Exists(fCall^.LowLevelRemoteIP) then
  begin
    Error('Banned IP %', [fCall^.LowLevelRemoteIP]);
    result := false;
  end
  else
    result := true;
end;

function TRestServerUriContext.AuthenticationBearerToken: RawUtf8;
begin
  result := inherited AuthenticationBearerToken;
  if (result = '') and
     not (rsoAuthenticationUriDisable in Server.Options) then
  begin
    result := GetInputUtf8OrVoid('authenticationbearer');
    if result <> '' then
      fCall^.LowLevelBearerToken := result;
  end;
end;

function TRestServerUriContext.AuthenticationCheck(jwt: TJwtAbstract): boolean;
begin
  result := inherited AuthenticationCheck(jwt);
  if result and
     (Server <> nil) and
     (Server.fIPWhiteJWT <> nil) and
     not Server.fIPWhiteJWT.Exists(fCall^.LowLevelRemoteIP) and
     (fCall^.LowLevelRemoteIP <> '') and
     (fCall^.LowLevelRemoteIP <> '127.0.0.1') then
  begin
    Error('Invalid IP [%]', [fCall^.LowLevelRemoteIP], HTTP_FORBIDDEN);
    result := false;
  end;
end;

function TRestServerUriContext.ClientOrmOptions: TOrmWriterOptions;
begin
  result := [];
  if (TableModelProps = nil) or
     (ClientKind <> ckAjax) then
    exit;
  if rsoGetID_str in Server.Options then
    include(result, owoID_str);
  if ([oftObject, oftBlobDynArray, oftVariant] *
      TableModelProps.Props.HasTypeFields <> []) and
     (rsoGetAsJsonNotAsString in Server.Options) then
    include(result, owoAsJsonNotAsString);
end;

function TRestServerUriContext.GetResourceFileName: TFileName;
begin
  if (fUriMethodPath = '') or
     not SafeFileNameU(fUriMethodPath) then
    // for security, disallow .. in the supplied file path
    result := ''
  else
    Utf8ToFileName(StringReplaceAll(fUriMethodPath, '/', PathDelim), result);
end;

procedure TRestServerUriContext.ReturnFileFromFolder(
  const FolderName: TFileName; Handle304NotModified: boolean;
  const DefaultFileName: TFileName; const Error404Redirect: RawUtf8;
  CacheControlMaxAge: integer);
var
  fn: RawUtf8;
  fileName: TFileName;
begin
  if fUriMethodPath = '' then
    fileName := DefaultFileName
  else
  begin
    fn := StringReplaceChars(fUriMethodPath, '/', PathDelim);
    if SafeFileNameU(fn) then
      Utf8ToFileName(fn, filename);
  end;
  inherited ReturnFileFromFolder(FolderName, Handle304NotModified,
    fileName, Error404Redirect, CacheControlMaxAge);
end;

procedure TRestServerUriContext.Error(const ErrorMessage: RawUtf8;
  Status: integer; CacheControlMaxAge: integer);
begin
  inherited Error(ErrorMessage, Status, CacheControlMaxAge);
  Server.InternalLog('%.Error: %', [ClassType, fCall^.OutBody], sllDebug);
end;



{ ************ TRestServerRoutingJsonRpc/TRestServerRoutingRest Requests Parsing Scheme }

{ TRestServerRoutingRest }

class function TRestServerRoutingRest.ClientRouting: TRestClientRoutingClass;
begin
  result := TRestClientRoutingRest;
end;

class procedure TRestServerRoutingRest.UriComputeRoutes(Router: TRestRouter;
  Server: TRestServer);
var
  services: TServiceContainerServer;
  i: PtrInt;
  ndx: integer;
  noID: boolean;
  sic: TServiceInstanceImplementation;
  rn: TRestNode;
  met: PServiceContainerInterfaceMethod;
  nam: RawUtf8;
begin
  services := Server.Services as TServiceContainerServer;
  // methods could be POST + JSON body but also GET + URI encoded parameters
  for i := 0 to high(Server.Services.InterfaceMethod) do
  begin
    met := @services.InterfaceMethod[i];
    nam := met^.InterfaceDotMethodName;
    ndx := met^.InterfaceMethodIndex; // 0..3 are im* pseudo-methods
    sic := met^.InterfaceService.InstanceCreation;
    noID := true;
    rn := rnInterface;
    case ndx of
      // pseudo-methods have a specific URI behavior
      ord(imContract):
        ; // keep noID = true
      ord(imInstance):
        if sic <> sicClientDriven then
          // imInstance is for a new sicClientDriven only
          continue;
      ord(imSignature):
        if not services.PublishSignature then
          // imSignature is disabled on this server
          continue;
      ord(imFree):
        if sic in SERVICE_IMPLEMENTATION_NOID then
          // imFree need an ID to release the instance
          continue
        else
        begin
          // imFree can make early release, e.g. from sicThread
          noID := false;
          rn := rnInterfaceClientID; // with an ID
        end
    else
      begin
        // interface methods need a /ClientDrivenID only if sicClientDriven
        noID := sic <> sicClientDriven;
        if not noID then
          rn := rnInterfaceClientID;
      end;
    end;
    // URI sent as GET/POST /Model/Interface.Method[/ClientDrivenID]
    if noID then
      Router.Setup([mGET, mPOST], nam,
        rn, nil, nil, ndx, met^.InterfaceService)
    else
      Router.Setup([mGET, mPOST], nam + '/<int:clientid>',
        rn, nil, nil, ndx, met^.InterfaceService);
    // URI sent as GET/POST /Model/Interface/Method[/ClientDrivenID]
    nam := StringReplaceChars(nam, '.', '/');
    if nam <> met^.InterfaceDotMethodName then
      if noID then
        Router.Setup([mGET, mPOST], nam,
          rn, nil, nil, ndx, met^.InterfaceService)
      else
        Router.Setup([mGET, mPOST], nam + '/<int:clientid>',
          rn, nil, nil, ndx, met^.InterfaceService);
  end;
end;

procedure TRestServerRoutingRest.DecodeUriParametersIntoJson;
var
  arg, i, ilow: PtrInt;
  WR: TJsonWriter;
  argdone: boolean;
  m: PInterfaceMethod;
  a: PInterfaceMethodArgument;
  temp: TTextWriterStackBuffer;
begin
  WR := TJsonWriter.CreateOwnedStream(temp);
  try // convert URI parameters into the expected ordered json array
    WR.Add('[');
    m := ServiceMethod;
    ilow := 0;
    a := @m^.Args[m^.ArgsInFirst];
    for arg := m^.ArgsInFirst to m^.ArgsInLast do
    begin
      if a^.ValueDirection <> imdOut then
      begin
        argdone := false;
        for i := ilow to (length(fInput) - 1) shr 1 do // search argument in URI
          if IdemPropNameU(fInput[i * 2], @a^.ParamName^[1], ord(a^.ParamName^[0])) then
          begin
            a^.AddValueJson(WR, fInput[i * 2 + 1]); // will add "" if needed
            if i = ilow then
              inc(ilow); // optimistic in-order search, but allow any order
            argdone := true;
            break;
          end;
        if not argdone then
          a^.AddDefaultJson(WR); // allow missing argument (and add ',')
      end;
      inc(a);
    end;
    WR.CancelLastComma;
    WR.Add(']');
    WR.SetText(fCall^.InBody); // input Body contains new generated input JSON
  finally
    WR.Free;
  end;
end;

procedure TRestServerRoutingRest.ExecuteSoaByInterface;
var
  par: PUtf8Char;
begin
  // here Ctxt.Service and ServiceMethod(Index) are set
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUtf8(
      '%.ExecuteSoaByInterface invalid call', [self]);
  //  URI as '/Model/Interface.Method[/ClientDrivenID]'
  if fCall^.InBody <> '' then
  begin
    // parameters sent as json array/object (the Delphi/AJAX way) or single blob
    if (ServiceMethod <> nil) and
       ServiceMethod^.ArgsInputIsOctetStream and
       not ContentTypeIsJson then
      // encode binary as Base64, as expected by InternalExecuteSoaByInterface
      // may use AVX2 on FPC x86_64 so performance is not an issue here
      fCall^.InBody := BinToBase64(fCall^.InBody, '["', '"]', false);
  end
  else
  begin
    // no body -> try URI-encoded parameters (the HTML way)
    par := fParameters;
    if par <> nil then
    begin
      while par^ = '+' do
        inc(par); // ignore trailing spaces
      if (par^ = '[') or
         IdemPChar(par, '%5B') then
        // as json array (input is e.g. '+%5B...' for ' [...')
        fCall^.InBody := UrlDecode(par)
      else
      begin
        // or as a list of parameters (input is 'Param1=Value1&Param2=Value2...')
        FillInput; // fInput[0]='Param1',fInput[1]='Value1',fInput[2]='Param2'...
        if (fInput <> nil) and
           (ServiceMethod <> nil) then
          DecodeUriParametersIntoJson;
      end;
    end;
  end;
  ServiceParameters := pointer(fCall^.InBody);
  // now Service, ServiceParameters, ServiceMethod(Index) are set
  InternalExecuteSoaByInterface;
end;


{ TRestServerRoutingJsonRpc }

class function TRestServerRoutingJsonRpc.ClientRouting: TRestClientRoutingClass;
begin
  result := TRestClientRoutingJsonRpc;
end;

class procedure TRestServerRoutingJsonRpc.UriComputeRoutes(
  Router: TRestRouter; Server: TRestServer);
var
  i: PtrInt;
  s: PServiceContainerInterface;
begin
  // URI sent as POST '/Model/Interface' for JSON-RPC service
  for i := 0 to Server.Services.Count - 1 do
  begin
    s := @Server.Services.InterfaceList[i];
    Router.Setup([mPOST], s^.InterfaceName, // as Server.Services[Uri]
      rnInterface, nil, nil, -1, s^.Service);
  end;
  // ServiceMethodIndex will be retrieved from "method": in body
end;

const
  RPC_NAMES: array[0..2] of PUtf8Char = (
    'method', // 0
    'params', // 1
    'id');    // 2

procedure TRestServerRoutingJsonRpc.ExecuteSoaByInterface;
var
  method: RawUtf8;
  values: array[0..2] of TValuePUtf8Char;
  tmp: TSynTempBuffer;
begin
  // here Ctxt.Service is set (not ServiceMethodIndex yet)
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUtf8(
      '%.ExecuteSoaByInterface invalid call', [self]);
  tmp.Init(fCall^.InBody);
  try
    JsonDecode(tmp.buf, @RPC_NAMES, length(RPC_NAMES), @values, true);
    if values[0].Text = nil then // Method name required
      exit;
    values[0].ToUtf8(method);
    ServiceParameters := values[1].Text;
    ServiceInstanceID := values[2].ToCardinal; // retrieve "id":ClientDrivenID
    ServiceMethodIndex := Service.ServiceMethodIndex(method);
    if ServiceMethodIndex < 0 then
    begin
      Error('Unknown method');
      exit;
    end;
    // now Service, ServiceParameters, ServiceMethodIndex are set
    InternalExecuteSoaByInterface;
    ServiceParameters := nil; // was pointing to transient tmp.buf
  finally
    tmp.Done; // release temp storage for values[] = Service* fields
  end;
end;



{ ************ TAuthSession for In-Memory User Sessions }

{ TAuthSession }

/// TSynObjectListSorted-TOnObjectCompare compatible callback method
function AuthSessionCompare(A, B: TObject): integer;
begin
  result := CompareCardinal(TAuthSession(A).fID, TAuthSession(B).fID);
end;

procedure TAuthSession.ComputeProtectedValues(tix: Int64);
begin
  // here User.GroupRights and fPrivateKey should have been set
  fTimeOutShr10 := (QWord(User.GroupRights.SessionTimeout) * (1000 * 60)) shr 10;
  fTimeOutTix := tix shr 10 + fTimeOutShr10;
  fAccessRights := User.GroupRights.SqlAccessRights;
  FormatUtf8('%+%', [fID, fPrivateKey], fPrivateSalt);
  fPrivateSaltHash := crc32(crc32(0, pointer(fPrivateSalt), length(fPrivateSalt)),
    pointer(User.PasswordHashHexa), length(User.PasswordHashHexa));
end;

constructor TAuthSession.Create(aCtxt: TRestServerUriContext; aUser: TAuthUser);
var
  GID: TAuthGroup;
  rnd: THash256;
  blob: RawBlob;
begin
  // inherited Create; // not mandatory - should not be overriden
  fUser := aUser;
  if (aCtxt <> nil) and
     (User <> nil) and
     (User.IDValue <> 0) then
  begin
    GID := User.GroupRights; // save pseudo TAuthGroup = ID
    User.GroupRights :=
      aCtxt.Server.fAuthGroupClass.Create(aCtxt.Server.ORM, GID);
    if User.GroupRights.IDValue <> 0 then
    begin
      // compute the next Session ID
      fID := InterlockedIncrement(aCtxt.Server.fSessionCounter);
      // set session parameters
      TAesPrng.Main.Fill(@rnd, SizeOf(rnd));
      fPrivateKey := BinToHex(@rnd, SizeOf(rnd));
      if not (rsoGetUserRetrieveNoBlobData in aCtxt.Server.Options) then
      begin
        aCtxt.Server.OrmInstance.RetrieveBlob(aCtxt.Server.fAuthUserClass,
          User.IDValue, 'Data', blob);
        User.Data := blob;
      end;
      if (aCtxt.fCall <> nil) and
         (aCtxt.fCall^.InHead <> '') then
        fSentHeaders := aCtxt.fCall^.InHead;
      ComputeProtectedValues(aCtxt.TickCount64);
      fConnectionID := aCtxt.Call^.LowLevelConnectionID;
      aCtxt.SetRemoteIP(fRemoteIP);
      fRemoteOsVersion := aCtxt.SessionOS;
      if aCtxt.Log <> nil then
        aCtxt.Log.Log(sllUserAuth,
          'New [%] session %/% created at %/% running % {%}',
          [User.GroupRights.Ident, User.LogonName, fID, fRemoteIP,
           aCtxt.Call^.LowLevelConnectionID, aCtxt.GetUserAgent,
           ToTextOS(integer(fRemoteOsVersion))], self);
      exit; // create successfull
    end;
    // on error: set GroupRights back to a pseudo TAuthGroup = ID
    User.GroupRights.Free;
    User.GroupRights := GID;
  end;
  raise ESecurityException.CreateUtf8(
    'Invalid %.Create(%,%)', [self, aCtxt, aUser]);
end;

destructor TAuthSession.Destroy;
begin
  if User <> nil then
  begin
    User.GroupRights.Free;
    fUser.Free;
  end;
  ObjArrayClear(fMethods);
  ObjArrayClear(fInterfaces);
  inherited;
end;

procedure TAuthSession.NotifyInterfaces(
  aCtxt: TRestServerUriContext; aElapsed: Int64);
var
  m: PtrInt;
begin
  if fInterfaces = nil then
  begin
    aCtxt.Server.Stats.Lock;
    SetLength(fInterfaces, length(aCtxt.Server.Services.InterfaceMethod));
    aCtxt.Server.Stats.UnLock;
  end;
  m := aCtxt.Server.Services.InterfaceMethods.FindHashed(
         aCtxt.ServiceMethod^.InterfaceDotMethodName); // mlSessions are slow
  if m < 0 then
    exit;
  if Interfaces[m] = nil then
  begin
    aCtxt.Server.Stats.Lock;
    if Interfaces[m] = nil then
      Interfaces[m] := TSynMonitorInputOutput.Create(
        aCtxt.ServiceMethod^.InterfaceDotMethodName);
    aCtxt.Server.Stats.UnLock;
  end;
  aCtxt.StatsFromContext(Interfaces[m], aElapsed);
  // mlSessions stats are not yet tracked per Client
end;

function TAuthSession.GetUserName: RawUtf8;
begin
  if User = nil then
    result := ''
  else
    result := User.LogonName;
end;

function TAuthSession.GetUserID: TID;
begin
  if User = nil then
    result := 0
  else
    result := User.IDValue;
end;

function TAuthSession.GetGroupID: TID;
begin
  if (User = nil) or
     (User.GroupRights = nil) then
    result := 0
  else
    result := User.GroupRights.IDValue;
end;

function TAuthSession.GetRemoteOS: RawUtf8;
begin
  result := ToTextOS(integer(fRemoteOsVersion));
end;

const
  TAUTHSESSION_MAGIC = 2;
  // version 2 includes fRemoteOsVersion

procedure TAuthSession.SaveTo(W: TBufferWriter);
begin
  W.Write1(TAUTHSESSION_MAGIC);
  W.WriteVarUInt32(fID);
  W.WriteVarUInt32(fUser.IDValue);
  fUser.GetBinaryValues(W); // User.fGroup is a pointer, but will be overriden
  W.WriteVarUInt32(fUser.GroupRights.IDValue);
  fUser.GroupRights.GetBinaryValues(W);
  W.Write(fPrivateKey);
  W.Write(fSentHeaders);
  W.Write4(integer(fRemoteOsVersion));
end; // TODO: persist ORM/SOA stats? -> rather integrate them before saving

constructor TAuthSession.CreateFrom(var Read: TFastReader; Server: TRestServer;
  tix: Int64);
begin
  if Read.NextByte <> TAUTHSESSION_MAGIC then
    raise ESecurityException.CreateUtf8(
      '%.CreateFrom() with invalid format on % %', [self, Server, Server.Model.Root]);
  fID := Read.VarUInt32;
  fUser := Server.AuthUserClass.Create;
  fUser.IDValue := Read.VarUInt32;
  fUser.SetBinaryValues(Read); // fUser.fGroup will be overriden by true instance
  fUser.GroupRights := Server.AuthGroupClass.Create;
  fUser.GroupRights.IDValue := Read.VarUInt32;
  fUser.GroupRights.SetBinaryValues(Read);
  Read.VarUtf8(fPrivateKey);
  Read.VarUtf8(fSentHeaders);
  integer(fRemoteOsVersion) := Read.Next4;
  ComputeProtectedValues(tix);
  FindNameValue(fSentHeaders, HEADER_REMOTEIP_UPPER, fRemoteIP);
end;



{ ************ TRestServerAuthentication Implementing Authentication Schemes }

{ TRestServerAuthentication }

constructor TRestServerAuthentication.Create(aServer: TRestServer);
begin
  inherited Create;
  fServer := aServer;
  fOptions := [saoUserByLogonOrID];
end;

function TRestServerAuthentication.AuthSessionRelease(
  Ctxt: TRestServerUriContext): boolean;
var
  uname: RawUtf8;
  sessid: cardinal;
  ndx: PtrInt;
  s: TAuthSession;
begin
  // fServer.Auth() method-based service made fServer.Sessions.Safe.WriteLock
  result := false;
  if (fServer.fSessions = nil) or
     not fServer.fHandleAuthentication then
    exit;
  uname := Ctxt.InputUtf8OrVoid['UserName'];
  if uname = '' then
    exit;
  sessid := Ctxt.InputIntOrVoid['Session'];
  if sessid = 0 then
    sessid := Ctxt.InputHexaOrVoid['SessionHex'];
  if sessid = 0 then
    exit;
  result := true; // recognized GET ModelRoot/auth?UserName=...&Session=...
  // allow only to delete its own authenticated session
  s := RetrieveSession(Ctxt); // parse signature
  if (s <> nil) and
     (sessid = s.ID) and
     (s.User.LogonName = uname) then
  begin
    Ctxt.fAuthSession := nil; // avoid GPF
    if fServer.LockedSessionFind(sessid, @ndx) = s then
    begin
      fServer.LockedSessionDelete(ndx, Ctxt);
      Ctxt.Success;
    end;
  end;
end;

function TRestServerAuthentication.GetUser(Ctxt: TRestServerUriContext;
  const aUserName: RawUtf8): TAuthUser;
var
  usrid: TID;
  err: integer;
begin
  usrid := GetInt64(pointer(aUserName), err);
  if (err <> 0) or
     (usrid <= 0) or
     not (saoUserByLogonOrID in fOptions) then
    usrid := 0;
  if Assigned(fServer.OnAuthenticationUserRetrieve) then
    result := fServer.OnAuthenticationUserRetrieve(self, Ctxt, usrid, aUserName)
  else
  begin
    if usrid <> 0 then
    begin
      // try if TAuthUser.ID was transmitted
      result := fServer.fAuthUserClass.Create(fServer.ORM, usrid); // may use ORM cache :)
      if result.IDValue = 0 then
        FreeAndNil(result);
    end
    else
      result := nil;
    if result = nil then
      result := fServer.fAuthUserClass.Create(
        fServer.ORM, 'LogonName=?', [aUserName]);
    if (result.IDValue = 0) and
       (saoHandleUnknownLogonAsStar in fOptions) then
      if fServer.OrmInstance.Retrieve('LogonName=?', [], ['*'], result) then
      begin
        result.LogonName := aUserName;
        result.DisplayName := aUserName;
      end;
  end;
  if (result = nil) or
     (result.IDValue = 0) then
  begin
    fServer.InternalLog('%.LogonName=% not found',
      [fServer.fAuthUserClass, aUserName], sllUserAuth);
    FreeAndNil(result);
  end
  else if not result.CanUserLog(Ctxt) then
  begin
    fServer.InternalLog('%.CanUserLog(%) returned FALSE -> rejected',
      [result, aUserName], sllUserAuth);
    FreeAndNil(result);
  end;
end;

procedure TRestServerAuthentication.SessionCreate(Ctxt: TRestServerUriContext;
  var User: TAuthUser);
var
  sess: TAuthSession;
begin
  // now client is authenticated -> create a sess
  if User <> nil then
  try
    fServer.SessionCreate(User, Ctxt, sess);
    // SessionCreate would call Ctxt.AuthenticationFailed on error
    if sess <> nil then
      SessionCreateReturns(Ctxt, sess, sess.fPrivateSalt, '', '');
  finally
    User.Free;
  end;
end;

procedure TRestServerAuthentication.SessionCreateReturns(
  Ctxt: TRestServerUriContext; Session: TAuthSession;
  const result, data, header: RawUtf8);
var
  body: TDocVariantData;
begin
  body.InitFast(10, dvObject);
  if result = '' then
    body.AddValue('result', Session.ID)
  else
    body.AddValue('result', RawUtf8ToVariant(result));
  if data <> '' then
    body.AddValue('data', RawUtf8ToVariant(data));
  if fAlgoName <> '' then
    // match e.g. TRestServerAuthenticationSignedUriAlgo
    body.AddValue('algo', RawUtf8ToVariant(fAlgoName));
  with Session.User do
    body.AddNameValuesToObject([
      'logonid',      IDValue,
      'logonname',    LogonName,
      'logondisplay', DisplayName,
      'logongroup',   GroupRights.IDValue,
      'timeout',      GroupRights.SessionTimeout,
      'server',       Executable.ProgramName,
      'version',      Executable.Version.DetailedOrVoid]);
  Ctxt.ReturnsJson(variant(body), HTTP_SUCCESS, false, twJsonEscape, false, header);
end;


{ TRestServerAuthenticationUri }

function TRestServerAuthenticationUri.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
begin
  result := nil;
  if (Ctxt = nil) or
     (Ctxt.UriSessionSignaturePos = 0) then
    exit;
  // expected format is 'session_signature='Hexa8(SessionID)'...
  if (Ctxt.UriSessionSignaturePos > 0) and
     (Ctxt.UriSessionSignaturePos + (18 + 8) <= length(Ctxt.Call^.Url)) and
     HexDisplayToCardinal(PAnsiChar(pointer(Ctxt.Call^.Url)) +
       Ctxt.UriSessionSignaturePos + 18, Ctxt.fSession) then
    result := fServer.LockedSessionAccess(Ctxt);
end;


{ TRestServerAuthenticationSignedUri }

// expected format is session_signature=
// Hexa8(SessionID)+
// Hexa8(Timestamp)+
// Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
//             Hexa8(Timestamp)+url))

constructor TRestServerAuthenticationSignedUri.Create(aServer: TRestServer);
begin
  inherited Create(aServer);
  SetAlgorithm(suaCRC32); // default/legacy hash algorithm
  TimestampCoherencySeconds := 5;
end;

procedure TRestServerAuthenticationSignedUri.SetNoTimestampCoherencyCheck(
  value: boolean);
begin
  if self <> nil then
    fNoTimestampCoherencyCheck := value;
end;

procedure TRestServerAuthenticationSignedUri.SetTimestampCoherencySeconds(
  value: cardinal);
begin
  if self = nil then
    exit;
  fTimestampCoherencySeconds := value;
  fTimestampCoherencyTicks := round(value * (1000 / 256)); // 256 ms resolution
end;

procedure TRestServerAuthenticationSignedUri.SetAlgorithm(
  value: TRestAuthenticationSignedUriAlgo);
begin
  fComputeSignature :=
    TRestClientAuthenticationSignedUri.GetComputeSignature(value);
  if value = suaCRC32 then
    fAlgoName := ''
  else
    fAlgoName := LowerCase(TrimLeftLowerCaseShort(ToText(value)));
end;

function TRestServerAuthenticationSignedUri.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
var
  ts, sign, minticks, expectedsign: cardinal;
  P: PAnsiChar;
  len: integer;
begin
  result := inherited RetrieveSession(Ctxt);
  if result = nil then
    // no valid session ID in session_signature
    exit;
  if Ctxt.UriSessionSignaturePos + (18 + 8 + 8 + 8) > length(Ctxt.Call^.Url) then
  begin
    result := nil;
    exit;
  end;
  len := Ctxt.UriSessionSignaturePos - 1;
  P := @Ctxt.Call^.Url[len + (20 + 8)]; // points to Hexa8(Timestamp)
  minticks := result.fLastTimestamp - fTimestampCoherencyTicks;
  if HexDisplayToBin(P, @ts, SizeOf(ts)) and
     (fNoTimestampCoherencyCheck or
      (integer(minticks) < 0) or // <0 just after login
      ({%H-}ts >= minticks)) then
  begin
    expectedsign := fComputeSignature(result.fPrivateSaltHash,
      P, pointer(Ctxt.Call^.Url), len);
    if HexDisplayToBin(P + 8, @sign, SizeOf(sign)) and
       ({%H-}sign = expectedsign) then
    begin
      if ts > result.fLastTimestamp then
        result.fLastTimestamp := ts;
      exit;
    end
    else
      Ctxt.Log.Log(sllUserAuth, 'Invalid Signature: expected %, got %',
        [CardinalToHexShort(expectedsign),
         CardinalToHexShort(sign)], self);
  end
  else
    Ctxt.Log.Log(sllUserAuth, 'Invalid Timestamp: expected >=%, got %',
      [minticks, Int64(ts)], self);
  result := nil; // indicates invalid signature
end;

var
  ServerNonceSafe: TLightLock;
  ServerNonceHasher: TSha3; // faster than THmacSha256 on small input
  ServerNonceCache: array[{previous=}boolean] of record
    tix: cardinal;
    res: RawUtf8;
    hash: THash256;
  end;

procedure CurrentServerNonceCompute(ticks: cardinal; previous: boolean;
  nonce: PRawUtf8; nonce256: PHash256);
var
  hex: RawUtf8;
  sha3: TSha3;
  tmp: THash256;
begin
  if ServerNonceHasher.Algorithm <> SHA3_256 then
  begin
    // first time used: initialize the private secret
    sha3.Init(SHA3_256);
    TAesPrng.Fill(tmp); // random seed for this process lifetime
    sha3.Update(@tmp, SizeOf(tmp));
    ServerNonceSafe.Lock;
    if ServerNonceHasher.Algorithm <> SHA3_256 then // atomic init
      ServerNonceHasher := sha3;
    ServerNonceSafe.UnLock;
  end;
  // compute and cache the new nonce for this timestamp
  sha3 := ServerNonceHasher; // thread-safe SHA-3 sponge reuse
  sha3.Update(@ticks, SizeOf(ticks));
  sha3.Final(tmp, true);
  BinToHexLower(@tmp, SizeOf(tmp), hex);
  if nonce <> nil then
    nonce^ := hex;
  if nonce256 <> nil then
    nonce256^ := tmp;
  with ServerNonceCache[previous] do
  begin
    ServerNonceSafe.Lock; // keep this global lock as short as possible
    tix := ticks;
    hash := tmp;
    res := hex;
    ServerNonceSafe.UnLock;
  end;
end;

procedure CurrentNonce(Ctxt: TRestServerUriContext; Previous: boolean;
  Nonce: PRawUtf8; Nonce256: PHash256);
var
  ticks: cardinal;
begin
  ticks := Ctxt.TickCount64 shr 18; // 4.3 minutes resolution - Ctxt may be nil
  if Previous then
    dec(ticks);
  with ServerNonceCache[Previous] do
  begin
    ServerNonceSafe.Lock;
    if (ticks = tix) and
       (res <> '') then  // check for res='' since ticks may be 0 at startup
    begin
      // fast retrieval from cache as binary or hexadecimal
      if Nonce256 <> nil then
        Nonce256^ := hash;
      if Nonce <> nil then
        Nonce^ := res;
      ServerNonceSafe.UnLock;
      exit;
    end;
    ServerNonceSafe.UnLock;
    // we need to (re)compute this value
    CurrentServerNonceCompute(ticks, Previous, Nonce, Nonce256);
  end;
end;

function CurrentNonce(Ctxt: TRestServerUriContext; Previous: boolean): RawUtf8;
begin
  CurrentNonce(Ctxt, Previous, @result, nil);
end;


{ TRestServerAuthenticationDefault }

function TRestServerAuthenticationDefault.Auth(Ctxt: TRestServerUriContext): boolean;
var
  uname, pwd, nonce: RawUtf8;
  usr: TAuthUser;
  os: TOperatingSystemVersion;
begin
  result := true;
  if AuthSessionRelease(Ctxt) then
    exit;
  uname := Ctxt.InputUtf8OrVoid['UserName'];
  nonce := Ctxt.InputUtf8OrVoid['ClientNonce'];
  if (uname <> '') and
     (length(nonce) > 32) then
  begin
    // GET ModelRoot/auth?UserName=...&PassWord=...&ClientNonce=... -> handshaking
    usr := GetUser(Ctxt, uname);
    if usr <> nil then
    try
      // decode TRestClientAuthenticationDefault.ClientComputeSessionKey nonce
      if (length(nonce) = (SizeOf(os) + SizeOf(TAesBlock)) * 2 + 1) and
         (nonce[9] = '_') and
         HexDisplayToBin(pointer(nonce), @os, SizeOf(os)) and
         (os.os <= high(os.os)) then
        Ctxt.fSessionOS := os;
      // check if match TRestClientUri.SetUser() algorithm
      pwd := Ctxt.InputUtf8OrVoid['Password'];
      if CheckPassword(Ctxt, usr, nonce, pwd) then
        // setup a new TAuthSession
        // SessionCreate would call Ctxt.AuthenticationFailed on error
        SessionCreate(Ctxt, usr)
      else
        Ctxt.AuthenticationFailed(afInvalidPassword);
    finally
      usr.Free;
    end
    else
      Ctxt.AuthenticationFailed(afUnknownUser);
  end
  else if uname <> '' then
    // only UserName=... -> return hexadecimal nonce content valid for 5 minutes
    Ctxt.Results([CurrentNonce(Ctxt)])
  else
    // parameters does not match any expected layout -> try next authentication
    result := false;
end;

function TRestServerAuthenticationDefault.CheckPassword(
  Ctxt: TRestServerUriContext; User: TAuthUser;
  const aClientNonce, aPassWord: RawUtf8): boolean;
var
  salt: RawUtf8;
begin
  salt := aClientNonce + User.LogonName + User.PasswordHashHexa;
  result := IsHex(aPassWord, SizeOf(THash256)) and
    (IdemPropNameU(aPassWord,
      Sha256(fServer.Model.Root + CurrentNonce(Ctxt, {prev=}false) + salt)) or
     // if current nonce failed, tries with previous 5 minutes' nonce
     IdemPropNameU(aPassWord,
       Sha256(fServer.Model.Root + CurrentNonce(Ctxt, {prev=}true)  + salt)));
end;


{ TRestServerAuthenticationNone }

function TRestServerAuthenticationNone.Auth(Ctxt: TRestServerUriContext): boolean;
var
  uname: RawUtf8;
  usr: TAuthUser;
begin
  uname := Ctxt.InputUtf8OrVoid['UserName'];
  if uname = '' then
  begin
    result := false; // let's try another TRestServerAuthentication class
    exit;
  end;
  result := true; // this kind of weak authentication avoid stronger ones
  if AuthSessionRelease(Ctxt) then
    exit;
  usr := GetUser(Ctxt, uname);
  if usr = nil then
    Ctxt.AuthenticationFailed(afUnknownUser)
  else
    SessionCreate(Ctxt, usr); // call Ctxt.AuthenticationFailed on error
end;


{ TRestServerAuthenticationHttpAbstract }

function TRestServerAuthenticationHttpAbstract.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
begin
  Ctxt.fTemp := Ctxt.InCookie[REST_COOKIE_SESSION];
  if (length(Ctxt.fTemp) = 8) and
     HexDisplayToCardinal(pointer(Ctxt.fTemp), Ctxt.fSession) then
    result := fServer.LockedSessionAccess(Ctxt)
  else
    result := nil;
end;



{ TRestServerAuthenticationHttpBasic }

class function TRestServerAuthenticationHttpBasic.GetUserPassFromInHead(
  Ctxt: TRestServerUriContext; out userPass, user, pass: RawUtf8): boolean;
begin
  userPass := Ctxt.InHeader['Authorization'];
  if IdemPChar(pointer(userPass), 'BASIC ') then
  begin
    delete(userPass, 1, 6);
    Split(Base64ToBin(userPass), ':', user, pass);
    result := user <> '';
  end
  else
    result := false;
end;

function TRestServerAuthenticationHttpBasic.RetrieveSession(
  Ctxt: TRestServerUriContext): TAuthSession;
var
  usrpwd, usr, pwd: RawUtf8;
begin
  result := inherited RetrieveSession(Ctxt); // retrieve cookie
  if result = nil then
    // not a valid 'Cookie: mORMot_session_signature=...' header
    exit;
  if (result.fExpectedHttpAuthentication <> '') and
     (result.fExpectedHttpAuthentication = Ctxt.InHeader['Authorization']) then
    // already previously authenticated for this session
    exit;
  if GetUserPassFromInHead(Ctxt, usrpwd, usr, pwd) then
    if usr = result.User.LogonName then
      with Ctxt.Server.AuthUserClass.Create do
      try
        PasswordPlain := pwd; // compute SHA-256 hash of the supplied password
        if PasswordHashHexa = result.User.PasswordHashHexa then
        begin
          // match -> store header in result (locked by fSessions.Safe)
          result.fExpectedHttpAuthentication := usrpwd;
          exit;
        end;
      finally
        Free;
      end;
  result := nil; // identicates authentication error
end;

class function TRestServerAuthenticationHttpBasic.ComputeAuthenticateHeader(
  const aUserName, aPasswordClear: RawUtf8): RawUtf8;
begin
  result := 'Authorization: Basic ' + BinToBase64(aUserName + ':' + aPasswordClear);
end;

function TRestServerAuthenticationHttpBasic.CheckPassword(
  Ctxt: TRestServerUriContext; User: TAuthUser;
  const aPassWord: RawUtf8): boolean;
var
  expected: RawUtf8;
begin
  expected := User.PasswordHashHexa;
  User.PasswordPlain := aPassWord; // override with SHA-256 hash from HTTP header
  result := IdemPropNameU(User.PasswordHashHexa, expected);
end;

function TRestServerAuthenticationHttpBasic.Auth(Ctxt: TRestServerUriContext): boolean;
var
  usrpwd, usr, pwd: RawUtf8;
  U: TAuthUser;
  sess: TAuthSession;
begin
  if Ctxt.InputExists['UserName'] then
  begin
    result := false; // allow other schemes to check this request
    exit;
  end;
  result := true; // this authentication method is exclusive to any other
  if GetUserPassFromInHead(Ctxt, usrpwd, usr, pwd) then
  begin
    U := GetUser(Ctxt, usr);
    if U <> nil then
    try
      if CheckPassword(Ctxt, U, pwd) then
      begin
        fServer.SessionCreate(U, Ctxt, sess);
        // SessionCreate would call Ctxt.AuthenticationFailed on error
        if sess <> nil then
        begin
          // see TRestServerAuthenticationHttpAbstract.ClientSessionSign()
          Ctxt.SetOutSetCookie((REST_COOKIE_SESSION + '=') +
            CardinalToHexLower(sess.ID));
          if (rsoRedirectForbiddenToAuth in fServer.Options) and
             (Ctxt.ClientKind = ckAjax) then
            Ctxt.Redirect(fServer.Model.Root)
          else
            SessionCreateReturns(Ctxt, sess, '', '', '');
          exit; // success
        end;
      end
      else
        Ctxt.AuthenticationFailed(afInvalidPassword);
    finally
      U.Free;
    end
    else
      Ctxt.AuthenticationFailed(afUnknownUser);
  end
  else
  begin
    Ctxt.fCall^.OutHead := 'WWW-Authenticate: Basic realm="mORMot Server"';
    Ctxt.Error('', HTTP_UNAUTHORIZED); // 401 will popup for credentials in browser
  end;
end;



{$ifdef DOMAINRESTAUTH}
{ will use mormot.lib.sspi/gssapi units depending on the OS }

const
  /// maximum number of Windows Authentication context to be handled at once
  // - 64 should be big enough
  MAXSSPIAUTHCONTEXTS = 64;


{ TRestServerAuthenticationSspi }

constructor TRestServerAuthenticationSspi.Create(aServer: TRestServer);
begin
  // setup mormot.lib.sspi/gssapi unit depending on the OS
  InitializeDomainAuth;
  // initialize this authentication scheme
  inherited Create(aServer);
  // TDynArray access to fSspiAuthContext[] by TRestConnectionID (ptInt64)
  fSspiAuthContexts.InitSpecific(TypeInfo(TSecContextDynArray),
    fSspiAuthContext, ptInt64, @fSspiAuthContextCount);
end;

destructor TRestServerAuthenticationSspi.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to fSspiAuthContextCount - 1 do
    FreeSecContext(fSspiAuthContext[i]);
  inherited Destroy;
end;

function TRestServerAuthenticationSspi.Auth(Ctxt: TRestServerUriContext): boolean;
var
  i, ndx: PtrInt;
  username, indataenc: RawUtf8;
  ticks: Int64;
  connectionID: TRestConnectionID;
  browserauth: boolean;
  outdata: RawByteString;
  user: TAuthUser;
  session: TAuthSession;
begin
  // GET ModelRoot/auth?username=...&data=... -> SSPI/GSSAPI auth
  result := AuthSessionRelease(Ctxt);
  if result or
     not Ctxt.InputExists['username'] or
     not Ctxt.InputExists['Data'] then
    exit;
  // use connectionID to find authentication session
  connectionID := Ctxt.Call^.LowLevelConnectionID;
  indataenc := Ctxt.InputUtf8['Data'];
  if indataenc = '' then
  begin
    // client is browser and used HTTP headers to send auth data
    FindNameValue(Ctxt.Call.InHead, pointer(SECPKGNAMEHTTPAUTHORIZATION), indataenc);
    if indataenc = '' then
    begin
      // no auth data sent, reply with supported auth methods
      Ctxt.Call.OutHead := SECPKGNAMEHTTPWWWAUTHENTICATE;
      Ctxt.Call.OutStatus := HTTP_UNAUTHORIZED; // (401)
      StatusCodeToReason(HTTP_UNAUTHORIZED, Ctxt.Call.OutBody);
      exit;
    end;
    browserauth := True;
  end
  else
    browserauth := False;
  // SSPI authentication
  fSafe.Lock;
  try
    // thread-safe deletion of deprecated fSspiAuthContext[] pending auths
    ticks := Ctxt.TickCount64 - 30000;
    for i := fSspiAuthContextCount - 1  downto 0 do
      if ticks > fSspiAuthContext[i].CreatedTick64 then
      begin
        FreeSecContext(fSspiAuthContext[i]);
        fSspiAuthContexts.Delete(i);
      end;
    // if no auth context specified, create a new one
    result := true;
    ndx := fSspiAuthContexts.Find(connectionID);
    if ndx < 0 then
    begin
      // 1st call: create SecCtxId
      if fSspiAuthContextCount >= MAXSSPIAUTHCONTEXTS then
      begin
        fServer.InternalLog('Too many Windows Authenticated session in  pending' +
          ' state: MAXSSPIAUTHCONTEXTS=%', [MAXSSPIAUTHCONTEXTS], sllUserAuth);
        exit;
      end;
      ndx := fSspiAuthContexts.New; // add a new entry to fSspiAuthContext[]
      InvalidateSecContext(fSspiAuthContext[ndx], connectionID);
    end;
    // call SSPI provider
    if ServerSspiAuth(fSspiAuthContext[ndx], Base64ToBin(indataenc), outdata) then
    begin
      // 1st call: send back outdata to the client
      if browserauth then
      begin
        Ctxt.Call.OutHead := (SECPKGNAMEHTTPWWWAUTHENTICATE + ' ') +
                               BinToBase64(outdata);
        Ctxt.Call.OutStatus := HTTP_UNAUTHORIZED; // (401)
        StatusCodeToReason(HTTP_UNAUTHORIZED, Ctxt.Call.OutBody);
      end
      else
        Ctxt.Returns(['result', '',
                      'data', BinToBase64(outdata)]);
      exit;
    end;
    // 2nd call: user was authenticated -> release used context
    ServerSspiAuthUser(fSspiAuthContext[ndx], username);
    if sllUserAuth in fServer.fLogFamily.Level then
      fServer.InternalLog('% Authentication success for %',
        [SecPackageName(fSspiAuthContext[ndx]), username], sllUserAuth);
    // now client is authenticated -> create a session for aUserName
    // and send back outdata
    try
      if username = '' then
        exit;
      user := GetUser(Ctxt,username);
      if user <> nil then
      try
        user.PasswordHashHexa := ''; // override with context
        fServer.SessionCreate(user, Ctxt, session);
        // SessionCreate would call Ctxt.AuthenticationFailed on error
        if session <> nil then
          with session.user do
            if browserauth then
              SessionCreateReturns(Ctxt, session, session.fPrivateSalt, '',
                (SECPKGNAMEHTTPWWWAUTHENTICATE + ' ') + BinToBase64(outdata))
            else
              SessionCreateReturns(Ctxt, session,
                BinToBase64(SecEncrypt(fSspiAuthContext[ndx], session.fPrivateSalt)),
                BinToBase64(outdata),'');
      finally
        user.Free;
      end
      else
        Ctxt.AuthenticationFailed(afUnknownUser);
    finally
      FreeSecContext(fSspiAuthContext[ndx]);
      fSspiAuthContexts.Delete(ndx);
    end;
  finally
    fSafe.UnLock; // protect fSspiAuthContext[] process
  end;
end;

{$endif DOMAINRESTAUTH}


{ ************ TRestServerMonitor for High-Level Statistics of a REST Server }

{ TRestServerMonitor }

constructor TRestServerMonitor.Create(aServer: TRestServer);
begin
  if aServer = nil then
    raise EOrmException.CreateUtf8('%.Create(nil)', [self]);
  inherited Create(aServer.Model.Root);
  fServer := aServer;
  SetLength(fPerTable[false], length(aServer.Model.Tables));
  SetLength(fPerTable[true],  length(aServer.Model.Tables));
  fStartDate := NowUtcToString;
end;

destructor TRestServerMonitor.Destroy;
begin
  ObjArrayClear(fPerTable[false]);
  ObjArrayClear(fPerTable[true]);
  inherited;
end;

procedure TRestServerMonitor.ProcessSuccess(IsOutcomingFile: boolean);
begin
  if self = nil then
    exit;
  fSafe.Lock;
  inc(fSuccess);
  if IsOutcomingFile then
    inc(fOutcomingFiles);
  fSafe.UnLock;
end;

procedure TRestServerMonitor.NotifyOrm(aMethod: TUriMethod);
var
  counter: PInt64;
begin
  if self = nil then
    exit;
  case aMethod of
    mGET,
    mLOCK:
      counter  := @fRead;
    mPOST:
      counter  := @fCreated;
    mPUT:
      counter  := @fUpdated;
    mDELETE:
      counter  := @fDeleted;
  else
    exit;
  end;
  LockedInc64(counter);
end;

function TRestServerMonitor.CreateNotifyOrmTable(
  TableIndex: PtrInt; Write: boolean): TSynMonitorWithSize;
const
  RW: array[boolean] of RawUtf8 = (
    '.read', '.write');
begin
  fSafe.Lock;
  try
    if TableIndex >= length(fPerTable[Write]) then
      // tables may have been added after Create()
      SetLength(fPerTable[Write], TableIndex + 1);
    result := fPerTable[Write, TableIndex];
    if result <> nil then
      exit;
    result := TSynMonitorWithSize.Create(
      fServer.Model.TableProps[TableIndex].Props.SqlTableName + RW[Write]);
    fPerTable[Write, TableIndex] := result;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestServerMonitor.CreateNotifyAuthSession(
  var Methods: TSynMonitorInputOutputObjArray);
begin
  fSafe.Lock;
  try
    if Methods = nil then
      SetLength(Methods, length(fServer.fPublishedMethod));
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestServerMonitor.CreateNotify(
  var Monitor: TSynMonitorInputOutput; const Name: RawUtf8);
begin
  fSafe.Lock;
  try
    if Monitor = nil then
      Monitor := TSynMonitorInputOutput.Create(Name);
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestServerMonitor.NotifyOrmTable(TableIndex, DataSize: integer;
  Write: boolean; const MicroSecondsElapsed: QWord);
var
  st: TSynMonitorWithSize;
begin
  if (self = nil) or
     (TableIndex < 0) then
    exit;
  if TableIndex >= length(fPerTable[Write]) then
    st := nil
  else
    st := fPerTable[Write, TableIndex];
  if st = nil then
    st := CreateNotifyOrmTable(TableIndex, Write);
  st.AddSize(DataSize, MicroSecondsElapsed);
  if fServer.fStatUsage <> nil then
    fServer.fStatUsage.Modified(st, []);
end;

function TRestServerMonitor.NotifyThreadCount(delta: integer): integer;
begin
  if self = nil then
    result := 0
  else
  begin
    fSafe.Lock;
    inc(fCurrentThreadCount, delta);
    result := fCurrentThreadCount;
    fSafe.UnLock;
  end;
end;


{ TOrmMonitorUsage }

function TOrmMonitorUsage.UsageID(aProcessIDShift: integer): integer;
begin
  result := fID shr aProcessIDShift;
end;


{ TSynMonitorUsageRest }

constructor TSynMonitorUsageRest.Create(const aStorage: IRestOrm;
  aProcessID: Int64; aStoredClass: TOrmMonitorUsageClass;
  aProcessIDShift: integer);
var
  g: TSynMonitorUsageGranularity;
begin
  if aStorage = nil then
    raise EOrmException.CreateUtf8('%.Create(nil)', [self]);
  if aProcessIDShift < 0 then
    aProcessIDShift := 16 { see TSynUniqueIdentifierProcess }
  else if aProcessIDShift > 40 then
    aProcessIDShift := 40;
  fProcessIDShift := aProcessIDShift;
  if aStoredClass = nil then
    fStoredClass := TOrmMonitorUsage
  else
    fStoredClass := aStoredClass;
  fStorage := aStorage;
  for g := low(fStoredCache) to high(fStoredCache) do
    fStoredCache[g] := fStoredClass.Create;
  fProcessID := aProcessID;
  fLog := fStorage.LogFamily;
  inherited Create;
end;

destructor TSynMonitorUsageRest.Destroy;
var
  g: TSynMonitorUsageGranularity;
begin
  inherited Destroy; // will save pending changes
  for g := low(fStoredCache) to high(fStoredCache) do
    fStoredCache[g].Free;
end;

function TSynMonitorUsageRest.LoadDB(ID: integer;
  Gran: TSynMonitorUsageGranularity; out Track: variant): boolean;
var
  recid: TID;
  rec: TOrmMonitorUsage;
begin
  if (ID = 0) or
     (Gran < Low(fStoredCache)) or
     (Gran > high(fStoredCache)) then
  begin
    result := false;
    exit;
  end;
  rec := fStoredCache[Gran];
  recid := (Int64(ID) shl fProcessIDShift) or Int64(fProcessID);
  if rec.IDValue = recid then
    result := true
  else if fStorage.Retrieve(recid, rec) then
  begin
    // may use REST cache
    Track := rec.Info;
    if rec.Gran = mugHour then
      fComment := rec.Comment;
    if rec.Process <> fProcessID then
      fLog.SynLog.Log(sllWarning, 'LoadDB(%,%) received Process=%, expected %',
        [ID, ToText(Gran)^, rec.Process, fProcessID], self);
    result := true;
  end
  else
  begin
    rec.ClearProperties;
    result := false;
  end;
end;

function TSynMonitorUsageRest.SaveDB(ID: integer; const Track: variant;
  Gran: TSynMonitorUsageGranularity): boolean;
var
  update: boolean;
  recid: TID;
  rec: TOrmMonitorUsage;
begin
  if (ID = 0) or
     (Gran < Low(fStoredCache)) or
     (Gran > high(fStoredCache)) then
  begin
    result := false;
    exit;
  end;
  rec := fStoredCache[Gran];
  recid := (Int64(ID) shl fProcessIDShift) or Int64(fProcessID);
  if rec.IDValue = recid then // already available
    update := true
  else
  begin
    update := fStorage.Retrieve(recid, rec); // may use REST cache
    rec.IDValue := recid;
  end;
  rec.Gran := Gran;
  rec.Process := fProcessID;
  if Gran = mugHour then
    rec.Comment := fComment;
  rec.Info := Track;
  if fSaveBatch <> nil then
    if update then
      result := fSaveBatch.Update(rec) >= 0
    else
      result := fSaveBatch.Add(rec, true, true) >= 0
  else if update then
    result := fStorage.Update(rec)
  else
    result := fStorage.Add(rec, true, true) = recid;
end;


{ ************ TRestRouter for efficient Radix Tree based URI Multiplexing }

{ TRestTreeNode }

function TRestTreeNode.LookupParam(Ctxt: TObject; Pos: PUtf8Char; Len: integer): boolean;
var
  ctx: TRestServerUriContext absolute Ctxt;
begin
  if Len < 0 then
    // Pos^ = '?par=val&par=val&...'
    ctx.fParameters := Pos + 1
  else if Data.Node = rnMethodPath then
    // ModelRoot/MethodName/<path:fulluri>
    // could be allocated, because <path:...> will always match the ending
    FastSetString(ctx.fUriMethodPath, Pos, Len)
  else
    // <int:clientid> or <int:tableid> (single placeholder by design)
    ctx.fRouterParam := Pos;
  result := true;
end;

function TRestTreeNode.Split(const Text: RawUtf8): TRadixTreeNode;
begin
  result := inherited Split(Text);
  TRestTreeNode(result).Data := Data;
  //Finalize(Data); not needed (no managed field)
  FillCharFast(Data, SizeOf(Data), 0);
end;


{ TRestTree }

procedure TRestTree.SetNodeClass;
begin
  fDefaultNodeClass := TRestTreeNode;
end;


{ TRestRouter }

constructor TRestRouter.Create(aOwner: TRestServer);
begin
  fOwner := aOwner;
  inherited Create;
end;

destructor TRestRouter.Destroy;
var
  m: TUriMethod;
begin
  inherited Destroy;
  for m := low(fTree) to high(fTree) do
    fTree[m].Free;
end;

function TRestRouter.Setup(aFrom: TUriMethod; const aUri: RawUtf8;
  aNode: TRestNode): TRestTreeNode;
var
  names: TRawUtf8DynArray;
  uri: RawUtf8;
begin
  if (aFrom < low(fTree)) or
     (aFrom > high(fTree)) then
    raise ERestTree.CreateUtf8('%.Setup(%)?', [self, MethodText(aFrom)]);
  if fTree[aFrom] = nil then
    fTree[aFrom] := TRestTree.Create([rtoCaseInsensitiveUri]);
  uri := fOwner.Model.Root;
  if aUri <> '' then
    uri := uri + '/' + aUri;
  result := fTree[aFrom].Setup(uri, names) as TRestTreeNode;
  if result = nil then
    exit;
  if result.Data.Node <> rnNone then
    raise ERestTree.CreateUtf8('%.Setup(m%,''%'',%) already exists as %',
      [self, MethodText(aFrom), aUri, ToText(aNode)^, ToText(result.Data.Node)^]);
  inc(fTreeCount[aFrom]);
  inc(fNodeCount[aNode]);
  result.Data.Node := aNode;
  case aNode of
    rnTable,
    rnTableID,
    rnTableIDBlob,
    rnState:
      if aFrom in [mGET, mLOCK, mUNLOCK, mHEAD, mSTATE] then
        result.Data.Command := execOrmGet
      else
        result.Data.Command := execOrmWrite;
    rnTableMethod,
    rnTableIDMethod,
    rnMethod,
    rnMethodPath:
      result.Data.Command := execSoaByMethod;
    rnInterface,
    rnInterfaceClientID:
      result.Data.Command := execSoaByInterface;
  else
    raise ERestTree.CreateUtf8('%.Setup(%)?', [self, ToText(aNode)^]);
  end;
end;

procedure TRestRouter.Setup(aFrom: TUriMethods; const aUri: RawUtf8;
  aNode: TRestNode; aTable: TOrmModelProperties; aBlob: TOrmPropInfoRttiRawBlob;
  aMethodIndex: integer; aService: TServiceFactory);
var
  m: TUriMethod;
  n: TRestTreeNode;
begin
  for m := low(fTree) to high(fTree) do
    if m in aFrom then
    begin
      n := Setup(m, aUri, aNode);
      n.Data.Table := aTable;
      n.Data.Blob := aBlob;
      n.Data.MethodIndex := aMethodIndex; // method or service method index
      if (aMethodIndex >= 0) and
         (n.Data.Command = execSoaByMethod) then
      begin
        if aMethodIndex >= length(fOwner.fPublishedMethod) then
          raise ERestException.CreateUtf8('%.Setup method?', [self]);
        if aMethodIndex = fOwner.fPublishedMethodBatchIndex then
          n.Data.Command := execOrmWrite; // BATCH is run as ORM write
      end;
      n.Data.Service := aService as TServiceFactoryServer;
    end;
end;

function TRestRouter.Lookup(Ctxt: TRestServerUriContext): TRestTreeNode;
var
  p: PUtf8Char;
  t: TOrmModelProperties;
  orm: TRestOrmServer;
  i: PtrInt;
begin
  // find the matching node in the URI Radix Tree
  result := nil;
  if (self = nil) or
     (Ctxt = nil) or
     (Ctxt.Call^.Url = '') or
     (Ctxt.Method < low(fTree)) or
     (Ctxt.Method > high(fTree)) then
    exit;
  p := pointer(Ctxt.Call^.Url);
  if p^ = '/' then
    inc(p);
  result := pointer((fTree[Ctxt.Method].Root as TRestTreeNode).Lookup(p, Ctxt));
  if result = nil then
    exit;
  // save the execution node information into Ctxt
  Ctxt.fNode := result.Data.Node;
  Ctxt.fCommand := result.Data.Command;
  t := result.Data.Table;
  if t <> nil then
  begin
    // remote ORM request
    Ctxt.fTable := t.Props.Table;
    Ctxt.fTableIndex := t.TableIndex;
    Ctxt.fTableModelProps := t;
    if result.Data.Node in [rnTableID, rnTableIDBlob, rnTableIDMethod] then
      SetQWord(Ctxt.fRouterParam, PQWord(@Ctxt.fTableID)^);
    orm := pointer(fOwner.fOrmInstance);
    Ctxt.fTableEngine := orm;
    Ctxt.fStaticOrm := orm.GetStaticTableIndex(t.TableIndex, Ctxt.fStaticKind);
    if Ctxt.fStaticOrm <> nil then
      Ctxt.fTableEngine := Ctxt.fStaticOrm;
  end;
  case result.Data.Node of
    rnTableIDBlob:
      // remote ORM request with BLOB field
      Ctxt.fUriBlobField := result.Data.Blob;
    rnTableMethod,
    rnTableIDMethod,
    rnMethod,
    rnMethodPath:
      begin
        // method-based service request
        i := result.Data.MethodIndex;
        Ctxt.fMethodIndex := i;
        Ctxt.fServerMethod := @fOwner.fPublishedMethod[i];
        // for rnMethodPath: Ctxt.fUriPath was set in LookupParam
      end;
    rnInterface,
    rnInterfaceClientID:
      begin
        // interface-based service request
        Ctxt.fService := result.Data.Service;
        if result.Data.Node = rnInterfaceClientID then
          SetQWord(Ctxt.fRouterParam, PQWord(@Ctxt.fServiceInstanceID)^);
        i := result.Data.MethodIndex;
        if i >= 0 then // -1 for TRestServerRoutingJsonRpc
        begin
          Ctxt.fServiceMethodIndex := i;
          dec(i, SERVICE_PSEUDO_METHOD_COUNT); // 0..3 for pseudo methods
          if i >= 0 then
            Ctxt.fServiceMethod := @Ctxt.fService.InterfaceFactory.Methods[i];
        end;
      end;
  end;
  // retrieve UriSessionSignaturePos as needed by Ctxt.Authenticate
  p := Ctxt.fParameters;
  if p <> nil then
  begin
    if fOwner.fHandleAuthentication then
    begin
      if IdemPChar(p, 'SESSION_SIGNATURE=') then
        dec(p)
      else
        p := StrPosI('&SESSION_SIGNATURE=', p);
      if p <> nil then
        Ctxt.fUriSessionSignaturePos := p - pointer(Ctxt.Call^.Url) + 1
    end;
  end
  // parameters may come from a web form, not from the URI trailer
  else if Ctxt.Method = mPost then
  begin
    p := pointer(Ctxt.fInputContentType);
    if (p <> nil) and
       IdemPChar(p, CONTENT_TYPE_WEBFORM) then
      Ctxt.fParameters := pointer(Ctxt.Call^.InBody);
  end;
end;

function TRestRouter.InfoText: RawUtf8;
var
  m: TUriMethod;
  n: TRestNode;
begin
  result := ''; // just concatenate the counters for logging
  for m := low(m) to high(m) do
    if fTreeCount[m] <> 0 then
      result := FormatUtf8('% %=%', [result, MethodText(m), fTreeCount[m]]);
  for n := low(n) to high(n) do
    if fNodeCount[n] <> 0 then
      result := FormatUtf8('% %=%', [result, ToText(n)^, fNodeCount[n]]);
end;



{ ************ TRestServer Abstract REST Server }

{ TRestServer }

constructor TRestServer.Create(aModel: TOrmModel; aHandleUserAuthentication: boolean);
begin
  if aModel = nil then
    raise EOrmException.CreateUtf8('%.Create(Model=nil)', [self]);
  fStatLevels := SERVERDEFAULTMONITORLEVELS;
  // fSessions is needed by AuthenticationRegister() below
  fSessions := TSynObjectListSorted.Create(AuthSessionCompare);
  fAuthUserClass := TAuthUser;
  fAuthGroupClass := TAuthGroup;
  fModel := aModel; // we need this property ASAP
  if fModel.TablesMax < 0 then
    fOptions := [rsoNoTableURI, rsoNoInternalState]; // no table/state to send
  fSessionClass := TAuthSession;
  if aHandleUserAuthentication then
    // default mORMot authentication schemes
    AuthenticationRegister([
      TRestServerAuthenticationDefault
      {$ifdef DOMAINRESTAUTH},
      TRestServerAuthenticationSspi
      {$endif DOMAINRESTAUTH}]);
  fAssociatedServices := TServicesPublishedInterfacesList.Create(0);
  fServicesRouting := TRestServerRoutingRest;
  inherited Create(aModel);
  fAfterCreation := true;
  fStats := TRestServerMonitor.Create(self);
  UriPagingParameters := PAGINGPARAMETERS_YAHOO;
  // + 10 to avoid CONST_AUTHENTICATION_* i.e. IDCardinal=0 or 1
  fSessionCounterMin := Random32(1 shl 20) + 10; // positive 31-bit integer
  fSessionCounter := fSessionCounterMin;
  fRootRedirectForbiddenToAuth := Model.Root + '/auth';
  fPublishedMethods.InitSpecific(
    TypeInfo(TRestServerMethods), fPublishedMethod, ptRawUtf8, nil, true);
  ServiceMethodRegisterPublishedMethods('', self);
  // manually add default method-based services
  fPublishedMethodTimestampIndex := ServiceMethodRegister(
    'timestamp', Timestamp, true, [mGET]);
  fPublishedMethodAuthIndex := ServiceMethodRegister(
    'auth', Auth, {bypassauth=}true, [mGET]);
  fPublishedMethodBatchIndex := ServiceMethodRegister(
    'batch', Batch, false, [mPUT, mPOST]);
  ServiceMethodRegister(
    'cacheflush', CacheFlush, false, [mGET, mPOST]);
  fPublishedMethodStatIndex := ServiceMethodRegister(
    'stat', Stat, false, [mGET]);
end;

var
  GlobalLibraryRequestServer: TRestServer = nil;

destructor TRestServer.Destroy;
var
  i: PtrInt;
begin
  Shutdown;
  fRecordVersionSlaveCallbacks := nil; // should be done before fServices.Free
  for i := 0 to fPublishedMethods.Count - 1 do
    fPublishedMethod[i].Stats.Free;
  ObjArrayClear(fSessionAuthentication, {continueonexception=}true);
  fServer := nil; // for proper refcnt in inherited Destroy
  inherited Destroy; // calls fServices.Free which will update fStats
  FreeAndNilSafe(fJwtForUnauthenticatedRequest);
  FreeAndNilSafe(fStats);
  fSessions.Free;
  fAssociatedServices.Free;
  if GlobalLibraryRequestServer = self then
    GlobalLibraryRequestServer := nil; // unregister
  fRouter.Free;
end;

constructor TRestServer.CreateWithOwnModel(const Tables: array of TOrmClass;
  aHandleUserAuthentication: boolean; const aRoot: RawUtf8);
var
  model: TOrmModel;
begin
  model := TOrmModel.Create(Tables, aRoot);
  Create(model, aHandleUserAuthentication);
  model.Owner := self;
end;

constructor TRestServer.Create(const aRoot: RawUtf8);
begin
  CreateWithOwnModel([], {auth=}false, aRoot);
end;

procedure TRestServer.SetOrmInstance(aORM: TRestOrmParent);
begin
  inherited SetOrmInstance(aORM);
  if not aORM.GetInterface(IRestOrmServer, fServer) then
    raise ERestException.CreateUtf8(
      '%.SetOrmInstance(%) is not an IRestOrmServer', [self, aORM]);
end;

function TRestServer.OrmInstance: TRestOrm;
begin
  result := TRestOrm(fOrmInstance);
end;

procedure TRestServer.SetNoAjaxJson(const Value: boolean);
begin
  if Value then
    include(fOptions, rsoNoAjaxJson)
  else
    exclude(fOptions, rsoNoAjaxJson);
  (fOrmInstance as TRestOrmServer).SetNoAjaxJson(Value);
end;

function TRestServer.GetNoAjaxJson: boolean;
begin
  result := (self <> nil) and
            (rsoNoAjaxJson in fOptions);
end;

constructor TRestServer.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
begin
  Create(aModel, aServerHandleAuthentication);
end;

procedure TRestServer.Shutdown(const aStateFileName: TFileName);
var
  timeout: Int64;
  {%H-}log: ISynLog; // for Enter auto-leave to work with FPC / Delphi 10.4+
begin
  if fSessions = nil then
    // avoid GPF e.g. in case of missing sqlite3-64.dll
    exit;
  if (fModel <> nil) and
     (fStats <> nil) then
    log := fLogClass.Enter('Shutdown(%) % CurrentRequestCount=%',
      [aStateFileName, fModel.Root, fStats.CurrentRequestCount], self)
  else
    log := fLogClass.Enter('Shutdown(%)', [aStateFileName], self);
  OnNotifyCallback := nil;
  fSessions.Safe.WriteLock;
  try
    if fShutdownRequested then
      // Shutdown method already called
      exit;
    fShutdownRequested := true; // will be identified by TRestServer.Uri()
  finally
    fSessions.Safe.WriteUnLock;
  end;
  if (fStats <> nil) and
     (fStats.CurrentRequestCount > 0) then
  begin
    timeout := GetTickCount64 + 30000; // never wait forever
    repeat
      SleepHiRes(5);
    until (fStats.CurrentRequestCount = 0) or
          (GetTickCount64 > timeout);
  end;
  if aStateFileName <> '' then
    SessionsSaveToFile(aStateFileName);
end;

function TRestServer.SleepOrShutdown(MS: integer): boolean;
begin
  result := not SleepHiRes(MS, fShutdownRequested);
end;

function TRestServer.GetAuthenticationSchemesCount: integer;
begin
  result := length(fSessionAuthentication);
end;

function TRestServer.GetRecordVersionMax: TRecordVersion;
begin
  result := TRestOrmServer(fOrmInstance).RecordVersionMax;
end;

procedure TRestServer.SetRecordVersionMax(Value: TRecordVersion);
begin
  TRestOrmServer(fOrmInstance).RecordVersionMax := Value;
end;

procedure TRestServer.RecordVersionHandle(Occasion: TOrmOccasion;
  TableIndex: integer; var Decoder: TJsonObjectDecoder;
  RecordVersionField: TOrmPropInfoRttiRecordVersion);
begin
  if RecordVersionField = nil then
    // no TRecordVersion field to track
    exit;
  if Decoder.FindFieldName(RecordVersionField.Name) < 0 then
    // only compute new monotonic TRecordVersion if not already supplied by sender
    Decoder.AddFieldValue(RecordVersionField.Name,
      Int64ToUtf8(TRestOrmServer(fOrmInstance).RecordVersionCompute), ftaNumber);
  if fServices <> nil then
    (fServices as TServiceContainerServer).RecordVersionNotifyAddUpdate(
      Occasion, TableIndex, Decoder);
end;

function TRestServer.AuthenticationRegister(
  aMethod: TRestServerAuthenticationClass): TRestServerAuthentication;
var
  i: PtrInt;
begin
  result := nil;
  if self = nil then
    exit;
  fSessions.Safe.WriteLock;
  try
    for i := 0 to high(fSessionAuthentication) do
      if fSessionAuthentication[i].ClassType = aMethod then
      begin
        // method already there -> return existing instance
        result := fSessionAuthentication[i];
        exit;
      end;
    // create and initialize new authentication instance
    result := aMethod.Create(self);
    ObjArrayAdd(fSessionAuthentication, result); // will be owned by this array
    fHandleAuthentication := true;
    // we need both AuthUser+AuthGroup tables for authentication -> create now
    fAuthGroupClass := Model.AddTableInherited(TAuthGroup);
    fAuthUserClass := Model.AddTableInherited(TAuthUser);
    if fAfterCreation and
       (not fOrm.TableHasRows(fAuthUserClass) or
        not fOrm.TableHasRows(fAuthGroupClass)) then
      with fOrmInstance as TRestOrmServer do
        CreateMissingTables(0, CreateMissingTablesOptions);
  finally
    fSessions.Safe.WriteUnLock;
  end;
end;

procedure TRestServer.AuthenticationRegister(
  const aMethods: array of TRestServerAuthenticationClass);
var
  i: PtrInt;
begin
  for i := 0 to high(aMethods) do
    AuthenticationRegister(aMethods[i]);
end;

procedure TRestServer.AuthenticationUnregister(
  aMethod: TRestServerAuthenticationClass);
var
  i: PtrInt;
begin
  if (self = nil) or
     (fSessionAuthentication = nil) then
    exit;
  fSessions.Safe.WriteLock;
  try
    for i := 0 to high(fSessionAuthentication) do
      if fSessionAuthentication[i].ClassType = aMethod then
      begin
        ObjArrayDelete(fSessionAuthentication, i);
        fHandleAuthentication := (fSessionAuthentication <> nil);
        break;
      end;
  finally
    fSessions.Safe.WriteUnLock;
  end;
end;

procedure TRestServer.AuthenticationUnregister(
  const aMethods: array of TRestServerAuthenticationClass);
var
  i: PtrInt;
begin
  for i := 0 to high(aMethods) do
    AuthenticationUnregister(aMethods[i]);
end;

procedure TRestServer.AuthenticationUnregisterAll;
begin
  if (self = nil) or
     (fSessionAuthentication = nil) then
    exit;
  fSessions.Safe.WriteLock;
  try
    ObjArrayClear(fSessionAuthentication);
    fHandleAuthentication := false;
  finally
    fSessions.Safe.WriteUnLock;
  end;
end;

function TRestServer.GetCurrentSessionUserID: TID;
begin
  with PServiceRunningContext(PerThreadRunningContextAddress)^ do
    if (Request <> nil) and
       (Request.Session > CONST_AUTHENTICATION_NOT_USED) then
      result := Request.SessionUser
    else
      result := 0;
end;

function TRestServer.ServicesPublishedInterfaces: RawUtf8;
var
  nfo: TServicesPublishedInterfaces;
begin
  if (self = nil) or
     (Services = nil) then
    result := ''
  else
  begin
    nfo.PublicUri := fPublicUri;
    Services.SetInterfaceNames(nfo.Names);
    result := RecordSaveJson(nfo, TypeInfo(TServicesPublishedInterfaces));
  end;
end;

procedure TRestServer.SetPublicUri(const Address, Port: RawUtf8);
begin
  fPublicUri.Address := Address;
  fPublicUri.Port := Port;
  fPublicUri.Root := Model.Root;
end;

procedure TRestServer.SetStatUsage(usage: TSynMonitorUsage);
begin
  if fStatUsage = usage then
    exit;
  if usage = nil then
  begin
    // e.g. from TTestServiceOrientedArchitecture.ClientSideRestSessionsStats
    FreeAndNil(fStatUsage);
    exit;
  end;
  if fStatUsage <> nil then
    raise EModelException.CreateUtf8('%.StatUsage should be set once', [self]);
  fStatUsage := usage;
  fStatUsage.Track(fStats, 'rest');
end;

function TRestServer.RecordVersionSynchronizeSubscribeMaster(
  Table: TOrmClass; RecordVersion: TRecordVersion;
  const SlaveCallback: IServiceRecordVersionCallback): boolean;
begin
  if self = nil then
    result := false
  else
    result := (ServiceContainer as TServiceContainerServer).
      RecordVersionSynchronizeSubscribeMaster(
        Model.GetTableIndexExisting(Table), RecordVersion, SlaveCallback);
end;

function TRestServer.RecordVersionSynchronizeMasterStart(
  ByPassAuthentication: boolean): boolean;
var
  factory: TServiceFactoryServer;
begin
  if fServices <> nil then
  begin
    factory := fServices.Info(TypeInfo(IServiceRecordVersion)) as TServiceFactoryServer;
    if factory <> nil then
    begin
      // already registered with the same authentication parameter
      result := factory.ByPassAuthentication = ByPassAuthentication;
      exit;
    end;
  end;
  factory := ServiceRegister(TServiceRecordVersion,
    [TypeInfo(IServiceRecordVersion)], sicShared) as TServiceFactoryServer;
  if factory <> nil then
  begin
    if ByPassAuthentication then
      factory.ByPassAuthentication := ByPassAuthentication;
    result := true;
  end
  else
    result := false;
end;

function TRestServer.RecordVersionSynchronizeSlaveStart(Table: TOrmClass;
  MasterRemoteAccess: TRestClientUri; const OnNotify: TOnBatchWrite): boolean;
var
  current, previous: TRecordVersion;
  tableindex: PtrInt;
  tablename: RawUtf8;
  service: IServiceRecordVersion;
  callback: IServiceRecordVersionCallback;
  retry: integer;
  {%H-}log: ISynLog;
begin
  log := fLogClass.Enter('RecordVersionSynchronizeSlaveStart % over %',
    [Table, MasterRemoteAccess], self);
  callback := nil; // weird fix for FPC/ARM
  result := false;
  if (self = nil) or
     (MasterRemoteAccess = nil) then
    exit;
  tableindex := Model.GetTableIndexExisting(Table);
  if (fRecordVersionSlaveCallbacks <> nil) and
     (fRecordVersionSlaveCallbacks[tableindex] <> nil) then
  begin
    InternalLog('RecordVersionSynchronizeSlaveStart(%): already running',
      [Table], sllWarning);
    exit;
  end;
  tablename := Model.TableProps[tableindex].Props.SqlTableName;
  if MasterRemoteAccess.Services.Info(IServiceRecordVersion) = nil then
    if not MasterRemoteAccess.ServiceRegister(
        [TypeInfo(IServiceRecordVersion)], sicShared) then
      exit;
  if not MasterRemoteAccess.Services.Resolve(IServiceRecordVersion, service) then
    exit;
  current := 0;
  retry := 0;
  repeat
    repeat
      // retrieve all pending versions (may retry up to 5 times)
      previous := current;
      current := (fOrmInstance as TRestOrmServer).RecordVersionSynchronizeSlave(
        Table, MasterRemoteAccess.Orm, 10000, OnNotify);
      if current < 0 then
      begin
        InternalLog('RecordVersionSynchronizeSlaveStart(%): REST failure',
         [Table], sllError);
        exit;
      end;
    until current = previous;
    // subscribe for any further modification
    if callback = nil then
      callback := TServiceRecordVersionCallback.Create(self,
        MasterRemoteAccess, Table, OnNotify);
    InternalLog('RecordVersionSynchronizeSlaveStart(%) current=% Subscribe(%)',
      [Table, current, pointer(callback)], sllDebug);
    if service.Subscribe(tablename, current, callback) then
    begin
      // push notifications
      if fRecordVersionSlaveCallbacks = nil then
        SetLength(fRecordVersionSlaveCallbacks, Model.TablesMax + 1);
      fRecordVersionSlaveCallbacks[tableindex] := callback;
      InternalLog('RecordVersionSynchronizeSlaveStart(%): started from revision %',
        [Table, current], sllDebug);
      result := true;
      exit;
    end;
    // some modifications since version (i.e. last RecordVersionSynchronizeSlave)
    inc(retry);
  until retry = 5; // avoid endless loop (most of the time, not needed)
  InternalLog('RecordVersionSynchronizeSlaveStart(%): retry failure', [Table], sllError);
end;

function TRestServer.RecordVersionSynchronizeSlaveStop(
  Table: TOrmClass): boolean;
var
  tableindex: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  tableindex := Model.GetTableIndexExisting(Table);
  if (fRecordVersionSlaveCallbacks = nil) or
     (fRecordVersionSlaveCallbacks[tableindex] = nil) then
  begin
    InternalLog('RecordVersionSynchronizeSlaveStop(%): not running',
      [Table], sllWarning);
    exit;
  end;
  fRecordVersionSlaveCallbacks[tableindex] := nil; // will notify the server
  result := true;
end;

procedure TRestServer.InternalInfo(Ctxt: TRestServerUriContext;
  var Info: TDocVariantData);
var
  cpu, mem, free: RawUtf8;
  now: TTimeLogBits;
  m: TSynMonitorMemory;
begin
  // called by root/Timestamp/Info REST method
  now.Value := GetServerTimestamp(Ctxt.TickCount64);
  cpu := TSystemUse.Current(false).HistoryText(0, 15, @mem);
  m := TSynMonitorMemory.Create({nospace=}true);
  try
    FormatUtf8('%/%', [m.PhysicalMemoryFree.Text, m.PhysicalMemoryTotal.Text], free);
    Info.AddNameValuesToObject([
      'nowutc',    now.Text(true, ' '),
      'timestamp', now.Value,
      'exe',       Executable.ProgramName,
      'version',   Executable.Version.DetailedOrVoid,
      'host',      Executable.Host,
      'cpu',       cpu,
      {$ifdef OSWINDOWS}
      'mem',       mem,
      {$endif OSWINDOWS}
      'memused',   KB(m.AllocatedUsed.Bytes),
      'memfree',   free,
      'diskfree',  GetDiskPartitionsText(
        {nocache=}false, {withfree=}true, {nospace=}true, {nomount=}true),
      'exception', GetLastExceptions(10)]);
  finally
    m.Free;
  end;
  Stats.Lock;
  try
    Info.AddNameValuesToObject([
      'started',    Stats.StartDate,
      'clients',    Stats.ClientsCurrent,
      'methods',    Stats.ServiceMethod,
      'interfaces', Stats.ServiceInterface,
      'total',      Stats.TaskCount,
      'time',       Stats.TotalTime.Text]);
  finally
    Stats.Unlock;
  end;
  if Assigned(OnInternalInfo) then
    OnInternalInfo(Ctxt, Info);
end;

procedure TRestServer.InternalStat(Ctxt: TRestServerUriContext; W: TJsonWriter);
var
  flags: TRestServerAddStats;
begin
  if Ctxt.InputExists['withall'] then
    flags := [low(TRestServerAddStat)..high(TRestServerAddStat)]
  else
  begin
    flags := [];
    if Ctxt.InputExists['withtables'] then
      include(flags, withtables);
    if Ctxt.InputExists['withmethods'] then
      include(flags, withmethods);
    if Ctxt.InputExists['withinterfaces'] then
      include(flags, withinterfaces);
    if Ctxt.InputExists['withsessions'] then
      include(flags, withsessions);
  end;
  AddStat(flags, W);
end;

procedure TRestServer.AddStat(Flags: TRestServerAddStats; W: TJsonWriter);
const
  READWRITE: array[boolean] of string[9] = (
    '{"read":', '{"write":');
var
  s, i: PtrInt;
  a: TAuthSession;
  rw: boolean;
begin
  Stats.ComputeDetailsTo(W);
  W.CancelLastChar('}');
  if fOrm.CacheOrNil <> nil then
  begin
    W.AddShort(',"cachedMemoryBytes":');
    W.AddU(fOrm.CacheOrNil.CachedMemory); // will also flush outdated JSON
    W.AddComma;
  end;
  if (fRun.BackgroundTimer <> nil) and
     (fRun.BackgroundTimer.Stats <> nil) then
  begin
    W.CancelLastComma;
    W.AddShort(',"backgroundTimer":');
    fRun.BackgroundTimer.Stats.ComputeDetailsTo(W);
    W.AddComma;
  end;
  if withtables in Flags then
  begin
    W.CancelLastComma;
    W.AddShort(',"tables":[');
    Stats.Lock; // thread-safe Stats.fPerTable[] access
    try
      for i := 0 to fModel.TablesMax do
      begin
        W.Add('{"%":[', [fModel.TableProps[i].Props.SqlTableName]);
        for rw := false to true do
          if (i < Length(Stats.fPerTable[rw])) and
             (Stats.fPerTable[rw, i] <> nil) and
             (Stats.fPerTable[rw, i].TaskCount <> 0) then
          begin
            W.AddShort(READWRITE[rw]);
            Stats.fPerTable[rw, i].ComputeDetailsTo(W);
            W.Add('}', ',');
          end;
        W.CancelLastComma;
        W.AddShorter(']},');
      end;
    finally
      Stats.UnLock;
    end;
    W.CancelLastComma;
    W.Add(']', ',');
  end;
  if withmethods in Flags then
  begin
    W.CancelLastComma;
    W.AddShort(',"methods":[');
    for i := 0 to high(fPublishedMethod) do
      with fPublishedMethod[i] do
        if (Stats <> nil) and
           (Stats.TaskCount <> 0) then
        begin
          W.Add('{"%":', [Name]);
          Stats.ComputeDetailsTo(W);
          W.Add('}', ',');
        end;
    W.CancelLastComma;
    W.Add(']', ',');
  end;
  if withinterfaces in Flags then
  begin
    W.CancelLastComma;
    W.AddShort(',"interfaces":[');
    for s := 0 to fServices.Count - 1 do
      with fServices.Index(s) as TServiceFactoryServer do
        for i := 0 to InterfaceFactory.MethodsCount - 1 do
          if Stats[i] <> nil then
          begin
            W.Add('{"%":', [InterfaceFactory.Methods[i].InterfaceDotMethodName]);
            Stats[i].ComputeDetailsTo(W);
            W.Add('}', ',');
          end;
    W.CancelLastComma;
    W.Add(']', ',');
  end;
  if (withsessions in Flags) and
     (fSessions <> nil) then
  begin
    W.CancelLastComma;
    W.AddShort(',"sessions":[');
    fSessions.Safe.ReadOnlyLock;
    try
      for s := 0 to fSessions.Count - 1 do
      begin
        a := fSessions.List[s];
        W.WriteObject(a);
        W.CancelLastChar('}');
        W.AddShort(',"methods":['); // append detailed statistics
        for i := 0 to high(a.fMethods) do
          if a.fMethods[i] <> nil then
          begin
            W.Add('{"%":', [fPublishedMethod[i].Name]);
            a.fMethods[i].ComputeDetailsTo(W);
            W.Add('}', ',');
          end;
        W.CancelLastComma;
        W.AddShort('],"interfaces":[');
        for i := 0 to high(a.fInterfaces) do
          if a.fInterfaces[i] <> nil then
          begin
            W.Add('{"%":', [Services.InterfaceMethod[i].InterfaceDotMethodName]);
            a.fInterfaces[i].ComputeDetailsTo(W);
            W.Add('}', ',');
          end;
        W.CancelLastComma;
        W.AddShorter(']},');
      end;
    finally
      fSessions.Safe.ReadOnlyUnLock;
    end;
    W.CancelLastComma;
    W.Add(']', ',');
  end;
  W.CancelLastComma;
  W.Add('}');
end;

function TRestServer.GetServiceMethodStat(
  const aMethod: RawUtf8): TSynMonitorInputOutput;
var
  i: PtrInt;
begin
  if self = nil then
    i := -1
  else
    i := fPublishedMethods.FindHashed(aMethod);
  if i >= 0 then
    result := fPublishedMethod[i].Stats
  else
    result := nil;
end;

procedure TRestServer.SetOnNotifyCallback(const event: TOnRestServerClientCallback);
begin
  if Assigned(fOnNotifyCallback) and
     Assigned(event) then
    raise ERestException.CreateUtf8(
      '%.OnNotifyCallback(%) set twice: only a single WS server can be assigned',
      [self, ClassNameShort(TObject(TMethod(event).Data))^]);
  fOnNotifyCallback := event;
end;

procedure TRestServer.SetRoutingClass(
  aServicesRouting: TRestServerUriContextClass);
begin
  if self <> nil then
    if aServicesRouting <> fServicesRouting then
      if (aServicesRouting = nil) or
         (aServicesRouting = TRestServerUriContext) then
        raise EServiceException.CreateUtf8(
          'Unexpected %.SetRoutingClass(%)', [self, aServicesRouting])
      else
      begin
        fServicesRouting := aServicesRouting;
        ResetRoutes; // fRouter will be re-generated when needed
      end;
end;

procedure TRestServer.SetOptions(rso: TRestServerOptions);
begin
  if rso = fOptions then
    exit;
  fOptions := rso;
  ResetRoutes;
end;

procedure TRestServer.ResetRoutes;
begin
  if fRouter = nil then
    exit;
  fRouterSafe.WriteLock;
  try
    FreeAndNil(fRouter);
  finally
    fRouterSafe.WriteUnLock;
  end;
end;

procedure TRestServer.ComputeRoutes;
var
  r: TRestRouter;
  i, j: PtrInt;
  t, n: RawUtf8;
  m: TOrmModelProperties;
  sm: PRestServerMethod;
  b: TOrmPropInfoRttiRawBlob;
  log: ISynLog;
begin
  log := fLogClass.Enter(self, 'ComputeRoutes');
  fRouterSafe.WriteLock;
  try
    if fRouter <> nil then
      exit;
    r := TRestRouter.Create(self);
    // ORM remote access via REST
    if (fModel.TablesMax >= 0) and
       not (rsoNoTableURI in fOptions) then
    begin
      // ModelRoot for
      r.Setup([mGET, mPOST, mBEGIN, mEND, mABORT], '', rnTable, nil);
      if not (rsoNoInternalState in fOptions) then
        r.Setup(mSTATE, '', rnState);
      for i := 0 to fModel.TablesMax do
      begin
        m := fModel.TableProps[i];
        t := m.Props.SqlTableName;
        // ModelRoot/TableName
        r.Setup([mGET, mPOST, mPUT, mDELETE, mBEGIN], t, rnTable, m);
        // ModelRoot/TableName/TableID
        r.Setup([mGET, mLOCK, mUNLOCK, mPUT, mDELETE],
          t + '/<int:tableid>', rnTableID, m);
        for j := 0 to high(m.Props.BlobFields) do
        begin
          b := m.Props.BlobFields[j];
          // ModelRoot/TableName/TableID/BlobField
          r.Setup([mGET, mPUT],
            t + '/<int:tableid>/' + b.Name, rnTableIDBlob, m, b);
          // ModelRoot/TableName/BlobField/TableID
          r.Setup([mGET, mPUT],
            t + '/' + b.Name + '/<int:tableid>', rnTableIDBlob, m, b);
        end;
        for j := 0 to high(fPublishedMethod) do
          if (j <> fPublishedMethodAuthIndex) and
             (j <> fPublishedMethodTimestampIndex) and
             (j <> fPublishedMethodStatIndex) then
          begin
            sm := @fPublishedMethod[j];
            // ModelRoot/TableName/MethodName
            r.Setup(sm^.Methods,
              t + '/' + sm^.Name, rnTableMethod, m, nil, j);
            if j <> fPublishedMethodBatchIndex then
              // ModelRoot/TableName/TableID/MethodName
              r.Setup(sm^.Methods,
                t + '/<int:tableid>/' + sm^.Name, rnTableIDMethod, m, nil, j);
          end;
      end;
    end;
    // method-based services
    for j := 0 to high(fPublishedMethod) do
    begin
      sm := @fPublishedMethod[j];
      n := sm^.Name;
      if n[1] = '_' then
        system.delete(n, 1, 1); // e.g. TServer._procedure -> /root/procedure
      if n = '' then
        continue;
      // ModelRoot/MethodName
      r.Setup(sm^.Methods, n, rnMethod, nil, nil, j);
      if (j <> fPublishedMethodAuthIndex) and
         (j <> fPublishedMethodStatIndex) and
         (j <> fPublishedMethodBatchIndex) then
      begin
        // ModelRoot/MethodName/<path:fulluri>
        r.Setup(sm^.Methods,
          n + '/<path:fulluri>', rnMethodPath, nil, nil, j);
        if (rsoMethodUnderscoreAsSlashUri in fOptions) and
           (PosExChar('_', n) <> 0) then
          // ModelRoot/Method/Name from Method_Name
          r.Setup(sm^.Methods,
            StringReplaceChars(n, '_', '/'), rnMethod, nil, nil, j);
      end;
    end;
    // interface-based services
    if Services <> nil then
      fServicesRouting.UriComputeRoutes(r, self); // depends on actual class
    //writeln(r.Tree[mPOST].ToText);
    fRouter := r; // set it in last position
  finally
    fRouterSafe.WriteUnLock;
  end;
  if log <> nil then
    log.Log(sllDebug, 'ComputeRoutes %: %',
      [fModel.Root, fRouter.InfoText], self);
end;

procedure TRestServer.SessionCreate(var User: TAuthUser;
  Ctxt: TRestServerUriContext; out Session: TAuthSession);
var
  i: PtrInt;
  a: TAuthSession;
  o: PRestServerConnectionOpaque;
begin
  Session := nil;
  // handle reOneSessionPerUser option
  if (reOneSessionPerUser in
      Ctxt.Call^.RestAccessRights^.AllowRemoteExecute) and
     (fSessions <> nil) then
    for i := 0 to fSessions.Count - 1 do
    begin
      a := fSessions.List[i];
      if a.User.IDValue = User.IDValue then
      begin
        InternalLog('User.LogonName=% already connected from %/%',
          [a.User.LogonName, a.RemoteIP, Ctxt.Call^.LowLevelConnectionID], sllUserAuth);
        Ctxt.AuthenticationFailed(afSessionAlreadyStartedForThisUser);
        exit; // user already connected
      end;
    end;
  // initialize the session
  Session := fSessionClass.Create(Ctxt, User);
  if Assigned(OnSessionCreate) then
    if OnSessionCreate(self, Session, Ctxt) then
    begin
      // callback returning TRUE aborts session creation
      InternalLog('Session aborted by OnSessionCreate() callback ' +
        'for User.LogonName=% (connected from %/%) - clients=%, sessions=%',
        [User.LogonName, Session.RemoteIP, Ctxt.Call^.LowLevelConnectionID,
         fStats.GetClientsCurrent, fSessions.Count], sllUserAuth);
      Ctxt.AuthenticationFailed(afSessionCreationAborted);
      User := nil;
      FreeAndNil(Session);
      exit;
    end;
  User := nil; // will be freed by TAuthSession.Destroy
  fSessions.Add(Session);
  fStats.ClientConnect;
  if rsoSessionInConnectionOpaque in fOptions then
  begin
    o := Ctxt.Call^.LowLevelConnectionOpaque;
    if o <> nil then
    begin
      // we can store the TAuthSession instance at connection level
      // to avoid signature parsing and session lookup
      o^.ValueInternal := PtrUInt(Session);
      Session.fConnectionOpaque := o; // for deprecated LockedSessionDelete()
    end;
  end;
end;

function TRestServer.LockedSessionFind(
  aSessionID: cardinal; aIndex: PPtrInt): TAuthSession;
var
  tmp: array[0..3] of PtrInt; // store a fake TAuthSessionParent instance
  i: PtrInt;
begin
  if (aSessionID < fSessionCounterMin) or
     (aSessionID > cardinal(fSessionCounter)) then
    result := nil
  else
  begin
    TAuthSessionParent(@tmp).fID := aSessionID;
    i := fSessions.IndexOf(@tmp); // use fast O(log(n)) binary search
    if aIndex <> nil then
      aIndex^ := i;
    if i < 0 then
      result := nil
    else
      result := fSessions.List[i];
  end;
end;

procedure TRestServer.LockedSessionDelete(aSessionIndex: integer;
  Ctxt: TRestServerUriContext);
var
  a: TAuthSession;
  soa: integer;

  procedure ClearConnectionOpaque(o: PRestServerConnectionOpaque);
  begin
    if o <> nil then
      try
        if o^.ValueInternal <> 0 then
          if pointer(o^.ValueInternal) = a then
            o^.ValueInternal := 0
          else
            InternalLog('Delete % session: opaque=% but % expected',
              [Ctxt, pointer(o^.ValueInternal), pointer(a)], sllWarning);
      except
        on E: Exception do
          InternalLog('Delete % session: opaque connection raised %',
            [Ctxt, ClassNameShort(E)^], sllWarning);
      end;
  end;

begin
  soa := 0;
  if (self <> nil) and
     (cardinal(aSessionIndex) < cardinal(fSessions.Count)) then
  begin
    a := fSessions.List[aSessionIndex];
    if fServices <> nil then
      soa := (fServices as TServiceContainerServer).OnCloseSession(a.ID);
    if rsoSessionInConnectionOpaque in fOptions then
      if Ctxt = nil then
        ClearConnectionOpaque(a.fConnectionOpaque)
      else
        ClearConnectionOpaque(Ctxt.Call^.LowLevelConnectionOpaque);
    if Ctxt = nil then
      InternalLog('Deleted deprecated session %:%/% soaGC=%',
        [a.User.LogonName, a.ID, fSessions.Count, soa], sllUserAuth)
    else
      InternalLog('Deleted session %:%/% from %/% soaGC=%',
        [a.User.LogonName, a.ID, fSessions.Count, a.RemoteIP,
         Ctxt.Call^.LowLevelConnectionID, soa], sllUserAuth);
    if Assigned(OnSessionClosed) then
      OnSessionClosed(self, a, Ctxt);
    fSessions.Delete(aSessionIndex);
    fStats.ClientDisconnect;
  end;
end;

function TRestServer.SessionDeleteDeprecated(tix: cardinal): integer;
var
  i: PtrInt;
begin
  // TRestServer.Uri() runs this method every second
  fSessionsDeprecatedTix := tix;
  result := 0;
  if (self = nil) or
     (fSessions = nil) or
     (fSessions.Count = 0) then
    exit;
  fSessions.Safe.ReadWriteLock; // won't block the ReadOnlyLock methods
  try
    for i := fSessions.Count - 1 downto 0 do
      if tix > TAuthSession(fSessions.List[i]).TimeOutTix then
      begin
        if result = 0 then
          fSessions.Safe.WriteLock; // upgrade the lock (seldom)
        LockedSessionDelete(i, nil);
        inc(result);
      end;
  finally
    if result <> 0 then
      fSessions.Safe.WriteUnlock;
    fSessions.Safe.ReadWriteUnLock;
  end;
end;

function TRestServer.LockedSessionAccess(Ctxt: TRestServerUriContext): TAuthSession;
begin
  // caller of RetrieveSession/SessionAccess made fSessions.Safe.ReadOnlyLock
  if (self <> nil) and
     (fSessions <> nil) and
     (Ctxt.Session > CONST_AUTHENTICATION_NOT_USED) then
  begin
    // retrieve session from its ID using O(log(n)) binary search
    result := LockedSessionFind(Ctxt.Session, nil);
    if result <> nil then
    begin
      // security check of session connection ID consistency
      if (reCheckSessionConnectionID in
          Ctxt.Call^.RestAccessRights^.AllowRemoteExecute) then
        if result.ConnectionID = 0 then // may have been retrieved from a file
          result.fConnectionID := Ctxt.Call^.LowLevelConnectionID
        else if result.ConnectionID <> Ctxt.Call^.LowLevelConnectionID then
        begin
          InternalLog('Session % from % rejected, since created from %/%',
           [Ctxt.Session, Ctxt.Call^.LowLevelConnectionID,
            result.RemoteIP, result.ConnectionID], sllUserAuth);
          exit;
        end;
      // found the session: assign it to the request Ctxt
      Ctxt.SessionAssign(result);
    end;
  end
  else
    result := nil;
end;

function TRestServer.SessionGetUser(aSessionID: cardinal): TAuthUser;
var
  s: TAuthSession;
begin
  result := nil;
  if self = nil then
    exit;
  fSessions.Safe.ReadOnlyLock;
  try
    s := LockedSessionFind(aSessionID, nil);
    if (s <> nil) and
       (s.User <> nil) then
    begin
      result := s.User.CreateCopy as fAuthUserClass;
      result.GroupRights := nil; // it is not a true instance
    end;
  finally
    fSessions.Safe.ReadOnlyUnLock;
  end;
end;

function TRestServer.SessionsAsJson: RawJson;
var
  i: PtrInt;
  W: TJsonWriter;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if (self = nil) or
     (fSessions.Count = 0) then
    exit;
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    fSessions.Safe.ReadOnlyLock;
    try
      W.Add('[');
      for i := 0 to fSessions.Count - 1 do
      begin
        W.WriteObject(fSessions.List[i]);
        W.AddComma;
      end;
      W.CancelLastComma;
      W.Add(']');
      W.SetText(RawUtf8(result));
    finally
      fSessions.Safe.ReadOnlyUnLock;
    end;
  finally
    W.Free;
  end;
end;

function TRestServer.BanIP(const aIP: RawUtf8; aRemoveBan: boolean): boolean;
begin
  result := false;
  if fIPBan = nil then
  begin
    if aRemoveBan then
      exit;
    fIPBan := TIPBan.Create;
    fPrivateGarbageCollector.Add(fIPBan);
  end;
  if aRemoveBan then
    result := fIPBan.Delete(aIP)
  else
    result := fIPBan.Add(aIP);
  InternalLog('BanIP(%,%)=%', [aIP, BOOL_STR[aRemoveBan], BOOL_STR[result]]);
end;

function TRestServer.JwtForUnauthenticatedRequestWhiteIP(const aIP: RawUtf8;
  aRemoveWhite: boolean): boolean;
begin
  result := false;
  if fJwtForUnauthenticatedRequest = nil then
    exit;
  if fIPWhiteJWT = nil then
  begin
    if aRemoveWhite then
      exit;
    fIPWhiteJWT := TIPBan.Create;
    fPrivateGarbageCollector.Add(fIPWhiteJWT);
  end;
  if aRemoveWhite then
    result := fIPWhiteJWT.Delete(aIP)
  else
    result := fIPWhiteJWT.Add(aIP);
  InternalLog('WhiteIP(%,%)=%', [aIP, BOOL_STR[aRemoveWhite], BOOL_STR[result]]);
end;

procedure TRestServer.ServiceMethodRegisterPublishedMethods(
  const aPrefix: RawUtf8; aInstance: TObject; aMethods: TUriMethods);
var
  i: PtrInt;
  methods: TPublishedMethodInfoDynArray;
begin
  if (aInstance <> nil) and
     (aMethods <> []) then
    for i := 0 to GetPublishedMethods(aInstance, methods) - 1 do
      with methods[i] do
        ServiceMethodRegister(aPrefix + Name,
          TOnRestServerCallBack(Method), false, aMethods);
end;

function TRestServer.ServiceMethodRegister(aMethodName: RawUtf8;
  const aEvent: TOnRestServerCallBack; aByPassAuthentication: boolean;
  aMethods: TUriMethods): PtrInt;
var
  m: TUriMethods;
  one: TUriMethod;
  pos: PtrInt;
  obj: TObject;
begin
  // handle '_VERB1_[_VERB2_][..]MethodName' pattern
  TrimSelf(aMethodName);
  m := [];
  while (aMethodName <> '') and
        (aMethodName[1] = '_') do
  begin
    pos := PosEx('_', aMethodName, 2); // end of '_GET_'
    if pos = 0 then
      break;
    one := ToMethod(copy(aMethodName, 2, pos - 2)); // 'GET'
    if one = mNone then
      break;
    include(m, one);
    // '_GET__POST_MethodName' -> '_POST_MethodName'
    system.delete(aMethodName, 1, pos);
  end;
  if m = [] then
    m := aMethods; // use (default) supplied methods
  if aMethodName = '' then
    raise EServiceException.CreateUtf8('%.ServiceMethodRegister('''')', [self]);
  // register the method to the internal list
  obj := TMethod(aEvent).Data;
  if not (rsoNoTableURI in fOptions) and
     (Model.GetTableIndex(aMethodName) >= 0) then
    raise EServiceException.CreateUtf8('Published method name %.% ' +
      'conflicts with a Table in the Model!', [obj, aMethodName]);
  with PRestServerMethod(fPublishedMethods.AddUniqueName(aMethodName,
    'Duplicated published method name %.%', [obj, aMethodName], @result))^ do
  begin
    Callback := aEvent;
    ByPassAuthentication := aByPassAuthentication;
    Methods := m;
  end;
  ResetRoutes;
end;

function TRestServer.ServiceMethodByPassAuthentication(
  const aMethodName: RawUtf8): integer;
var
  i: PtrInt;
begin
  result := -1;
  if self = nil then
    exit;
  if aMethodName = '' then
    // bypass auth for all methods
    for i := 0 to fPublishedMethods.Count - 1 do
      fPublishedMethod[i].ByPassAuthentication := true
  else
  begin
    result := fPublishedMethods.FindHashed(aMethodName);
    if result >= 0 then
      fPublishedMethod[result].ByPassAuthentication := true;
  end;
end;

function TRestServer.ServiceContainer: TServiceContainer;
begin
  if fServices = nil then
    fServices := TServiceContainerServer.Create(self);
  result := fServices;
end;

function TRestServer.ServiceRegister(aImplementationClass: TInterfacedClass;
  const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
begin
  if (aImplementationClass = nil) or
     (high(aInterfaces) < 0) then
    result := nil
  else
    result := (ServiceContainer as TServiceContainerServer).AddImplementation(
      aImplementationClass, aInterfaces, aInstanceCreation, nil, aContractExpected);
end;

function TRestServer.ServiceRegister(aSharedImplementation: TInterfacedObject;
  const aInterfaces: array of PRttiInfo;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
begin
  if (self = nil) or
     (aSharedImplementation = nil) or
     (high(aInterfaces) < 0) then
    result := nil
  else
    result := (ServiceContainer as TServiceContainerServer).AddImplementation(
      TInterfacedClass(aSharedImplementation.ClassType), aInterfaces, sicShared,
      aSharedImplementation, aContractExpected);
end;

function TRestServer.ServiceRegister(aClient: TRest;
  const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): boolean;
begin
  if (self = nil) or
     (high(aInterfaces) < 0) or
     (aClient = nil) then
    result := false
  else
    result := (ServiceContainer as TServiceContainerServer).AddInterface(
      aInterfaces, aInstanceCreation, aContractExpected);
end;

function TRestServer.ServiceDefine(aImplementationClass: TInterfacedClass;
  const aInterfaces: array of TGuid; aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.Guid2TypeInfo(aInterfaces);
  result := ServiceRegister(aImplementationClass, ti, aInstanceCreation,
    aContractExpected);
end;

function TRestServer.ServiceDefine(aSharedImplementation: TInterfacedObject;
  const aInterfaces: array of TGuid;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.Guid2TypeInfo(aInterfaces);
  result := ServiceRegister(aSharedImplementation, ti, aContractExpected);
end;

function TRestServer.ServiceDefine(aClient: TRest;
  const aInterfaces: array of TGuid;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): boolean;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.Guid2TypeInfo(aInterfaces);
  result := ServiceRegister(aClient, ti, aInstanceCreation, aContractExpected);
end;

const
  MAGIC_SESSION: cardinal = $A5ABA5AB;

procedure TRestServer.SessionsSaveToFile(const aFileName: TFileName);
var
  i: PtrInt;
  MS: TRawByteStringStream;
  W: TBufferWriter;
  s: RawByteString;
begin
  if self = nil then
    exit;
  DeleteFile(aFileName);
  MS := TRawByteStringStream.Create;
  try
    W := TBufferWriter.Create(MS);
    try
      fSessions.Safe.ReadOnlyLock;
      try
        W.WriteVarUInt32((fOrmInstance as TRestOrmServer).InternalState);
        AuthUserClass.OrmProps.SaveBinaryHeader(W);
        AuthGroupClass.OrmProps.SaveBinaryHeader(W);
        W.WriteVarUInt32(fSessions.Count);
        for i := 0 to fSessions.Count - 1 do
          TAuthSession(fSessions.List[i]).SaveTo(W);
        W.Write4(fSessionCounter);
        W.Write4(MAGIC_SESSION + 1);
        W.Flush;
      finally
        fSessions.Safe.ReadOnlyUnLock;
      end;
    finally
      W.Free;
    end;
    s := AlgoSynLZ.Compress(MS.DataString);
    SymmetricEncrypt(MAGIC_SESSION, s);
    FileFromString(s, aFileName, true);
  finally
    MS.Free;
  end;
end;

procedure TRestServer.SessionsLoadFromFile(const aFileName: TFileName;
  andDeleteExistingFileAfterRead: boolean);

  procedure ContentError;
  begin
    raise ESecurityException.CreateUtf8('%.SessionsLoadFromFile("%")',
      [self, aFileName]);
  end;

var
  i, n: integer;
  tix: Int64;
  s: RawByteString;
  R: TFastReader;
begin
  if self = nil then
    exit;
  s := StringFromFile(aFileName);
  SymmetricEncrypt(MAGIC_SESSION, s);
  s := AlgoSynLZ.Decompress(s);
  if s = '' then
    exit;
  tix := GetTickCount64;
  R.Init(s);
  fSessions.Safe.WriteLock;
  try
    (fOrmInstance as TRestOrmServer).InternalState := R.VarUInt32;
    if not AuthUserClass.OrmProps.CheckBinaryHeader(R) or
       not AuthGroupClass.OrmProps.CheckBinaryHeader(R) then
      ContentError;
    n := R.VarUInt32;
    fSessions.Clear;
    for i := 1 to n do
    begin
      fSessions.Add(fSessionClass.CreateFrom(R, self, tix));
      fStats.ClientConnect;
    end;
    fSessionCounter := PCardinal(R.P)^;
    inc(PCardinal(R.P));
    if PCardinal(R.P)^ <> MAGIC_SESSION + 1 then
      ContentError;
  finally
    fSessions.Safe.WriteUnLock;
  end;
  if andDeleteExistingFileAfterRead then
    DeleteFile(aFileName);
end;

procedure TRestServer.OnBeginCurrentThread(Sender: TThread);
var
  tc: integer;
  id: TThreadID;
begin
  tc := fStats.NotifyThreadCount(1);
  id := GetCurrentThreadId;
  if Sender = nil then
    raise ERestException.CreateUtf8('%.BeginCurrentThread(nil)', [self]);
  InternalLog('BeginCurrentThread(%) root=% ThreadID=% ''%'' ThreadCount=%',
    [Sender.ClassType, fModel.Root, {%H-}pointer(id), CurrentThreadName, tc]);
  if Sender.ThreadID <> id then
    raise ERestException.CreateUtf8(
      '%.BeginCurrentThread(Thread.ID=%) and CurrentThreadID=% should match',
      [self, {%H-}pointer(Sender.ThreadID), {%H-}pointer(id)]);
  with PServiceRunningContext(PerThreadRunningContextAddress)^ do
    if RunningThread <> Sender then
      // e.g. if length(TRestHttpServer.fDBServers)>1
      if RunningThread <> nil then
        raise ERestException.CreateUtf8('%.BeginCurrentThread() twice', [self])
      else
        // set the current TThread info
        RunningThread := Sender;
  // call TRestOrmServer.BeginCurrentThread
  inherited OnBeginCurrentThread(Sender);
end;

procedure TRestServer.OnEndCurrentThread(Sender: TThread);
var
  tc: integer;
  i: PtrInt;
  id: TThreadID;
  inst: TServiceFactoryServerInstance;
begin
  tc := fStats.NotifyThreadCount(-1);
  id := GetCurrentThreadId;
  if Sender = nil then
    raise ERestException.CreateUtf8('%.EndCurrentThread(nil)', [self]);
  InternalLog('EndCurrentThread(%) ThreadID=% ''%'' ThreadCount=%',
    [Sender.ClassType, {%H-}pointer(id), CurrentThreadName, tc]);
  if Sender.ThreadID <> id then
    raise ERestException.CreateUtf8(
      '%.EndCurrentThread(%.ID=%) should match CurrentThreadID=%',
      [self, Sender, {%H-}pointer(Sender.ThreadID), {%H-}pointer(id)]);
  if Services <> nil then
  begin
    inst.InstanceID := PtrUInt(id);
    for i := 0 to Services.Count - 1 do
      with TServiceFactoryServer(Services.InterfaceList[i].Service) do
        if InstanceCreation = sicPerThread then
          RetrieveInstance(nil, inst, ord(imFree), 0);
  end;
  with PServiceRunningContext(PerThreadRunningContextAddress)^ do
    if RunningThread <> nil then
      // e.g. if length(TRestHttpServer.fDBServers)>1
      if RunningThread <> Sender then
        raise ERestException.CreateUtf8(
          '%.EndCurrentThread(%) should match RunningThread=%',
          [self, Sender, RunningThread])
      else        // reset the TThread info
        RunningThread := nil;
  // call TRestOrmServer.EndCurrentThread (and TSynLogFamily.OnThreadEnded)
  inherited OnEndCurrentThread(Sender);
end;

procedure TRestServer.Uri(var Call: TRestUriParams);
var
  ctxt: TRestServerUriContext;
  node: TRestTreeNode;
  tix: Int64;
  tix32: cardinal;
  outcomingfile: boolean;
  log: ISynLog;
begin
  if fShutdownRequested then
  begin
    call.OutStatus := HTTP_UNAVAILABLE; // too late!
    exit;
  end;
  // 1. pre-request preparation
  if (fLogFamily <> nil) and
     (sllEnter in fLogFamily.Level) then
    log := fLogClass.Enter('URI % % in=%',
      [Call.Method, Call.Url, KB(Call.InBody)], self);
  if fRouter = nil then
    ComputeRoutes; // thread-safe (re)initialize once if needed
  if Assigned(OnStartUri) then
  begin
    Call.OutStatus := OnStartUri(Call);
    if Call.OutStatus <> HTTP_SUCCESS then
    begin
      if log <> nil then
        log.Log(sllServer, 'Uri: rejected by OnStartUri(% %)=%',
          [Call.Method, Call.Url, Call.OutStatus], self);
      exit;
    end;
  end;
  // 2. request initialization
  Call.OutStatus := HTTP_BADREQUEST; // default error code is 400 BAD REQUEST
  ctxt := fServicesRouting.Create(self, Call);
  try
    if (fIPBan <> nil) and
       ctxt.IsRemoteIPBanned then
    begin
      ctxt.Error('Banned IP', HTTP_TEAPOT); // not worth looking at
      exit;
    end;
    // 3. setup the statistics
    if log <> nil then
    begin
      ctxt.fLog := log.Instance;
      if StatLevels <> [] then // get start timestamp from log
        ctxt.fMicroSecondsStart := ctxt.fLog.LastQueryPerformanceMicroSeconds;
    end;
    if StatLevels <> [] then
    begin
      if ctxt.fMicroSecondsStart = 0 then
        QueryPerformanceMicroSeconds(ctxt.fMicroSecondsStart); // get from OS
      fStats.AddCurrentRequestCount(1);
    end;
    // 4. decode request URI and validate input
    fRouterSafe.ReadLock;
    node := fRouter.Lookup(ctxt);
    fRouterSafe.ReadUnLock;
    if node = nil then
      ctxt.Error('Invalid URI', HTTP_BADREQUEST) // root ok: error 400, not 404
    else if (RootRedirectGet <> '') and
            (ctxt.Method = mGet) and
            (ctxt.fNode = rnTable) and // Url = Model.Root
            (Call.InBody = '') then // unstandard GET with body = execute SELECT
      ctxt.Redirect(RootRedirectGet)
    else if (Call.InBody <> '') and
            (rsoValidateUtf8Input in fOptions) and
            ctxt.ContentTypeIsJson and
            not IsValidUtf8(Call.InBody) then
      ctxt.Error('Expects valid UTF-8 input')
    else
    // 6. handle security
    if (rsoSecureConnectionRequired in fOptions) and
       (ctxt.MethodIndex <> fPublishedMethodTimestampIndex) and
       not (llfSecured in Call.LowLevelConnectionFlags) then
      ctxt.AuthenticationFailed(afSecureConnectionRequired)
    else if not ctxt.Authenticate then
      ctxt.AuthenticationFailed(afInvalidSignature)
    else if (ctxt.Service <> nil) and
        not (reService in Call.RestAccessRights^.AllowRemoteExecute) then
      if (rsoRedirectForbiddenToAuth in Options) and
         (ctxt.ClientKind = ckAjax) then
        ctxt.Redirect(fRootRedirectForbiddenToAuth)
      else
        ctxt.AuthenticationFailed(afRemoteServiceExecutionNotAllowed)
    else if (ctxt.Session <> CONST_AUTHENTICATION_NOT_USED) or
            (fJwtForUnauthenticatedRequest = nil) or
            (ctxt.MethodIndex = fPublishedMethodTimestampIndex) or
            ((llfSecured in Call.LowLevelConnectionFlags) and
             // HTTPS does not authenticate by itself, WebSockets does
             not (llfHttps in Call.LowLevelConnectionFlags)) or
            ctxt.AuthenticationCheck(fJwtForUnauthenticatedRequest) then
    // 5. call appropriate ORM / SOA commands in fAcquireExecution[] context
    try
      if (not Assigned(OnBeforeUri)) or
         OnBeforeUri(ctxt) then
        ctxt.ExecuteCommand;
    except
      on E: Exception do
        if (not Assigned(OnErrorUri)) or
           OnErrorUri(ctxt, E) then
          if E.ClassType = EInterfaceFactory then
            ctxt.Error(E, '', [], HTTP_NOTACCEPTABLE)
          else
            ctxt.Error(E, '', [], HTTP_SERVERERROR);
    end;
    // 6. return expected result to the client
    if StatusCodeIsSuccess(Call.OutStatus) then
    begin
      outcomingfile := false;
      if Call.OutBody <> '' then
        // detect 'Content-type: !STATICFILE' as first header
        outcomingfile := (length(Call.OutHead) >= 25) and
                         (Call.OutHead[15] = '!') and
                         IdemPChar(pointer(Call.OutHead),
                           STATICFILE_CONTENT_TYPE_HEADER_UPPPER)
      else
        // handle Call.OutBody=''
        if (Call.OutStatus = HTTP_SUCCESS) and
           (rsoHttp200WithNoBodyReturns204 in fOptions) then
          Call.OutStatus := HTTP_NOCONTENT;
      if ctxt.fMicroSecondsStart <> 0 then
        fStats.ProcessSuccess(outcomingfile);
    end
    else
      // OutStatus is an error code
      if Call.OutBody = '' then
        // if no custom error message, compute it now as JSON
        ctxt.Error(ctxt.CustomErrorMsg, Call.OutStatus);
    // 7. compute returned ORM InternalState indicator
    if ((rsoNoInternalState in fOptions) or
        (rsoNoTableURI in fOptions)) and
       (ctxt.Method <> mSTATE) then
      // reduce headers verbosity
      Call.OutInternalState := 0
    else if (ctxt.StaticOrm <> nil) and
            ctxt.StaticOrm.InheritsFrom(TRestStorage) and
            TRestStorage(ctxt.StaticOrm).OutInternalStateForcedRefresh then
      // force always refresh for Static table which demands it
      Call.OutInternalState := cardinal(-1)
    else
      // database state may have changed above
      Call.OutInternalState := TRestOrmServer(fOrmInstance).InternalState;
    // set any outgoing cookie
    if ctxt.OutSetCookie <> '' then
      ctxt.OutHeadFromCookie;
    // paranoid check of the supplied output headers
    if not (rsoHttpHeaderCheckDisable in fOptions) and
       IsInvalidHttpHeader(pointer(Call.OutHead), length(Call.OutHead)) then
      ctxt.Error('Unsafe HTTP header rejected [%]',
        [EscapeToShort(Call.OutHead)], HTTP_SERVERERROR);
  finally
    // 8. gather statistics and log execution
    if ctxt.fMicroSecondsStart <> 0 then
      ctxt.ComputeStatsAfterCommand;
    if log <> nil then
      ctxt.LogFromContext;
    // 9. finalize execution context
    if Assigned(OnAfterUri) then
      try
        OnAfterUri(ctxt);
      except
      end;
    tix := ctxt.TickCount64;
    ctxt.Free;
  end;
  // 10. trigger post-request periodic process
  tix32 := tix shr 10;
  if tix32 <> fSessionsDeprecatedTix then
    // check deprecated sessions every second
    SessionDeleteDeprecated(tix32);
  if Assigned(OnIdle) then
  begin
    tix32 := tix shr 7;
    if tix32 <> fOnIdleLastTix then
    begin
      // trigger OnIdle() every 128 ms
      fOnIdleLastTix := tix32;
      OnIdle(self);
    end;
  end;
end;

procedure TRestServer.Stat(Ctxt: TRestServerUriContext);
var
  W: TJsonWriter;
  json, xml, name: RawUtf8;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonWriter.CreateOwnedStream(temp);
  try
    name := Ctxt.InputUtf8OrVoid['findservice'];
    if name = '' then
    begin
      InternalStat(Ctxt, W);
      name := 'Stats';
    end
    else
      AssociatedServices.FindServiceAll(name, W);
    W.SetText(json);
    if Ctxt.InputExists['format'] or
       IdemPropNameU(Ctxt.fUriMethodPath, 'json') then
      json := JsonReformat(json)
    else if IdemPropNameU(Ctxt.fUriMethodPath, 'xml') then
    begin
      JsonBufferToXML(pointer(json), XMLUTF8_HEADER, '<' + name + '>', xml);
      Ctxt.Returns(xml, 200, XML_CONTENT_TYPE_HEADER);
      exit;
    end;
    Ctxt.Returns(json);
  finally
    W.Free;
  end;
end;

procedure TRestServer.Auth(Ctxt: TRestServerUriContext);
var
  i: PtrInt;
begin
  if fSessionAuthentication = nil then
    exit;
  fSessions.Safe.WriteLock;
  try
    for i := 0 to length(fSessionAuthentication) - 1 do
      if fSessionAuthentication[i].Auth(Ctxt) then
        // found an authentication, which may be successfull or not
        break;
  finally
    fSessions.Safe.WriteUnLock;
  end;
end;

procedure TRestServer.Timestamp(Ctxt: TRestServerUriContext);
var
  info: TDocVariantData;
  tix: cardinal;
begin
  if IdemPropNameU(Ctxt.fUriMethodPath, 'info') and
     not (rsoTimestampInfoUriDisable in fOptions) then
  begin
    tix := Ctxt.TickCount64 shr 12; // cache refreshed every 4.096 seconds
    if tix <> fTimestampInfoCacheTix then
    begin
      fTimestampInfoCacheTix := tix;
      {%H-}info.InitFast;
      InternalInfo(Ctxt, info);
      fTimestampInfoCache := info.ToJson('', '', jsonHumanReadable);
    end;
    Ctxt.Returns(fTimestampInfoCache);
  end
  else
    Ctxt.Returns(Int64ToUtf8(GetServerTimestamp(Ctxt.TickCount64)),
      HTTP_SUCCESS, TEXT_CONTENT_TYPE_HEADER);
end;

procedure TRestServer.CacheFlush(Ctxt: TRestServerUriContext);
var
  i, count: PtrInt;
  old: TRestConnectionID;
  cache: TOrmCache;
begin
  case Ctxt.Method of
    mGET:
      begin
        // POST root/cacheflush[/table[/id]]
        cache := TRestOrmServer(fOrmInstance).CacheOrNil;
        if cache <> nil then
          if Ctxt.Table = nil then
            cache.Flush
          else if Ctxt.TableID = 0 then
            cache.Flush(Ctxt.Table)
          else
            cache.SetCache(Ctxt.Table, Ctxt.TableID);
        Ctxt.Success;
      end;
    mPOST:
      if Ctxt.fUriMethodPath = '_ping_' then
      begin
        // POST root/cacheflush/_ping_
        count := 0;
        if Ctxt.Session > CONST_AUTHENTICATION_NOT_USED then
          for i := 0 to Services.Count - 1 do
            inc(count, TServiceFactoryServer(Services.InterfaceList[i].Service).
              RenewSession(Ctxt));
        InternalLog('Renew % authenticated session % from %: count=%',
          [Model.Root, Ctxt.Session, Ctxt.RemoteIPNotLocal, count], sllUserAuth);
        Ctxt.Returns(['count', count]);
      end
      else if (fServices <> nil) and
              (llfWebsockets in Ctxt.Call^.LowLevelConnectionFlags) then
        if Ctxt.fUriMethodPath = '_callback_' then
          // POST root/cacheflush/_callback_ with {"ICallbackName":1234} body
          // as called from TRestHttpClientWebsockets.FakeCallbackUnregister
          (fServices as TServiceContainerServer).FakeCallbackRelease(Ctxt)
        else if Ctxt.fUriMethodPath = '_replaceconn_' then
        begin
          // POST root/cacheflush/_replaceconn_ (over a secured connection)
          old := GetInt64(pointer(Ctxt.Call^.InBody));
          count := (fServices as TServiceContainerServer).
            FakeCallbackReplaceConnectionID(old, Ctxt.Call^.LowLevelConnectionID);
          InternalLog('%: Connection % replaced by % from % on %',
            [Model.Root, old, Ctxt.Call^.LowLevelConnectionID,
             Ctxt.RemoteIPNotLocal, Plural('interface', count)], sllHTTP);
          Ctxt.Returns(['count', count]);
        end;
    mPUT:
  end;
end;

procedure TRestServer.Batch(Ctxt: TRestServerUriContext);
var
  res: TInt64DynArray;
  i: PtrInt;
begin
  if not (Ctxt.Method in [mPUT, mPOST]) then
  begin
    Ctxt.Error('PUT/POST only');
    exit;
  end;
  try
    TRestOrmServer(fOrmInstance).EngineBatchSend(
      Ctxt.Table, Ctxt.Call.InBody, TIDDynArray(res), 0);
  except
    on E: Exception do
    begin
      Ctxt.Error(E, 'did break % BATCH process', [Ctxt.Table], HTTP_SERVERERROR);
      exit;
    end;
  end;
  // send back operation status array
  Ctxt.Call.OutStatus := HTTP_SUCCESS;
  for i := 0 to length({%H-}res) - 1 do
    if res[i] <> HTTP_SUCCESS then
    begin
      Ctxt.Call.OutBody := Int64DynArrayToCsv(
        pointer(res), length(res), '[', ']');
      exit;
    end;
  Ctxt.Call.OutBody := '["OK"]';  // to save bandwith if no adding
end;

function TRestServer.ExportServerGlobalLibraryRequest(Disable: boolean): boolean;
begin
  result := false;
  if self <> nil then
    if Disable then
    begin
      if GlobalLibraryRequestServer = self then
      begin
        GlobalLibraryRequestServer := nil;
        result := true;
      end;
    end
    else if (GlobalLibraryRequestServer = nil) or
            (GlobalLibraryRequestServer = self) then
    begin
      GlobalLibraryRequestServer := self;
      result := true;
    end;
end;

procedure LibraryRequestFree(Data: pointer); cdecl; // match TLibraryRequestFree
begin
  if Data <> nil then
    RawUtf8(Data) := '';
end;

procedure LibraryRequestString(var s: RawUtf8; p: PUtf8Char; l: PtrInt);
begin
  if p <> nil then
  try
    with PStrRec(p - SizeOf(TStrRec))^ do
      if (refCnt > 0) and
         {$ifdef HASCODEPAGE}
         (elemSize = 1) and
         ((codepage = CP_UTF8) or
          (codepage = CP_RAWBYTESTRING)) and
         {$endif HASCODEPAGE}
         (length = l) then
      begin
        // no memory allocation if comes from pascal code -> byref assignment
        inc(refCnt);
        pointer(s) := p;
        exit;
      end
  except
  end;
  // standard allocation
  FastSetString(s, p, l);
end;

function LibraryRequest(Url, Method, SendData: PUtf8Char;
  UrlLen, MethodLen, SendDataLen: cardinal;
  out HeadRespFree: TLibraryRequestFree;
  var Head: PUtf8Char; var HeadLen: cardinal;
  out Resp: PUtf8Char; out RespLen, State: cardinal): cardinal; cdecl;
var
  call: TRestUriParams;
  h: RawUtf8;
begin
  if GlobalLibraryRequestServer = nil then
  begin
    result := HTTP_NOTIMPLEMENTED; // 501
    exit;
  end;
  HeadRespFree := @LibraryRequestFree;
  call.Init;
  LibraryRequestString(call.Url, Url, UrlLen);
  LibraryRequestString(call.Method, Method, MethodLen);
  call.LowLevelConnectionID := PtrInt(GlobalLibraryRequestServer);
  call.LowLevelConnectionFlags := [llfSecured]; // in-process call
  call.InHead := 'RemoteIP: 127.0.0.1';
  call.LowLevelRemoteIP := '127.0.0.1';
  if (Head <> nil) and
     (HeadLen <> 0) then
  begin
    LibraryRequestString(h, Head, HeadLen);
    call.InHead := h + call.InHead + #13#10;
  end;
  LibraryRequestString(call.InBody, SendData, SendDataLen);
  call.RestAccessRights := @SUPERVISOR_ACCESS_RIGHTS;
  GlobalLibraryRequestServer.Uri(call);
  result := call.OutStatus;
  State := call.OutInternalState;
  Head := pointer(call.OutHead);
  HeadLen := length(call.OutHead);
  Resp := pointer(call.OutBody);
  RespLen := length(call.OutBody);
  pointer(call.OutHead) := nil; // will be released by HeadRespFree()
  pointer(call.OutBody) := nil;
end;


{ ************ TRestHttpServerDefinition Settings for a HTTP Server }

{ TRestHttpServerDefinition }

constructor TRestHttpServerDefinition.Create;
begin
  fOptions := HTTPSERVER_DEFAULT_OPTIONS;
  inherited Create; // may have been overriden
end;



{$ifndef PUREMORMOT2}

procedure TRestServer.CreateMissingTables(user_version: cardinal;
  options: TOrmInitializeTableOptions);
begin
  fServer.CreateMissingTables(user_version, options);
end;

function TRestServer.CreateSqlIndex(Table: TOrmClass; const FieldName: RawUtf8;
  Unique: boolean; const IndexName: RawUtf8): boolean;
begin
  result := fServer.CreateSqlIndex(Table, FieldName, Unique, IndexName);
end;

function TRestServer.CreateSqlIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean): boolean;
begin
  result := fServer.CreateSqlIndex(Table, FieldNames, Unique);
end;

function TRestServer.CreateSqlMultiIndex(Table: TOrmClass;
  const FieldNames: array of RawUtf8; Unique: boolean; IndexName: RawUtf8): boolean;
begin
  result := fServer.CreateSqlMultiIndex(Table, FieldNames, Unique, IndexName);
end;

function TRestServer.IsInternalSQLite3Table(aTableIndex: integer): boolean;
begin
  result := fServer.IsInternalSQLite3Table(aTableIndex);
end;

function TRestServer.MaxUncompressedBlobSize(Table: TOrmClass): integer;
begin
  result := fServer.MaxUncompressedBlobSize(Table);
end;

{$endif PUREMORMOT2}



initialization
  // should match TPerThreadRunningContext definition in mormot.core.interfaces
  assert(SizeOf(TServiceRunningContext) =
    SizeOf(TObject) + SizeOf(TObject) + SizeOf(TThread));

end.

