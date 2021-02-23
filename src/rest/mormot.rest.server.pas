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
    - TInterfacedCallback/TBlockingCallback Classes
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
  mormot.lib.z, // zlib's crc32 for private hash
  {$ifdef DOMAINRESTAUTH}
  mormot.lib.sspi, // do-nothing units on non compliant system
  mormot.lib.gssapi,
  {$endif DOMAINRESTAUTH}
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.rtti,
  mormot.core.crypto,
  mormot.core.jwt,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.core.secure,
  mormot.core.log,
  mormot.core.interfaces,
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
  TRestServerAuthentication = class;

  /// exception raised in case of unexpected parsing error
  EParsingException = class(ESynException);

  /// exception raised when security authorization/authentication failed
  ESecurityException = class(ESynException);

  /// points to the currently running service on the server side
  // - your code may call the ServiceRunningContext global function
  // once in a method, since per-thread access does cost some CPU
  // !var context: PServiceRunningContext;
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

  /// used by TRestServerUriContext.ClientKind to identify the currently
  // connected client
  TRestServerUriContextClientKind = (
    ckUnknown,
    ckFramework,
    ckAJAX);

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
  TRestServerUriContext = class
  protected
    fInput: TRawUtf8DynArray; // even items are parameter names, odd are values
    fInputPostContentType: RawUtf8;
    fInputCookiesRetrieved: boolean;
    fInputCookies: array of record
      Name, Value: RawUtf8; // only computed if InCookie[] is used
    end;
    fInHeaderLastName: RawUtf8;
    fInHeaderLastValue: RawUtf8;
    fOutSetCookie: RawUtf8;
    fUserAgent: RawUtf8;
    fAuthenticationBearerToken: RawUtf8;
    fRemoteIP: RawUtf8;
    fAuthSession: TAuthSession;
    fClientKind: TRestServerUriContextClientKind;
    fSessionAccessRights: TOrmAccessRights; // fSession may be deleted meanwhile
    fServiceListInterfaceMethodIndex: integer;
    function GetInput(const ParamName: RawUtf8): variant;
    function GetInputOrVoid(const ParamName: RawUtf8): variant;
    function GetInputNameIndex(const ParamName: RawUtf8): PtrInt;
    function GetInputExists(const ParamName: RawUtf8): boolean;
    function GetInputInt(const ParamName: RawUtf8): Int64;
    function GetInputDouble(const ParamName: RawUtf8): Double;
    procedure GetInputByName(const ParamName,InputName: RawUtf8;
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
    function GetInHeader(const HeaderName: RawUtf8): RawUtf8;
    procedure RetrieveCookies;
    function GetInCookie(CookieName: RawUtf8): RawUtf8;
    procedure SetInCookie(CookieName, CookieValue: RawUtf8);
    function GetUserAgent: RawUtf8;
    function GetRemoteIP: RawUtf8;
    function GetRemoteIPNotLocal: RawUtf8;
    function GetRemoteIPIsLocalHost: boolean;
    function GetResourceFileName: TFileName;
    procedure SetOutSetCookie(aOutSetCookie: RawUtf8);
    procedure InternalSetTableFromTableIndex(Index: integer); virtual;
    procedure InternalSetTableFromTableName(TableName: PUtf8Char); virtual;
    procedure InternalExecuteSoaByInterface; virtual;
    function IsRemoteIPBanned: boolean; // as method to avoid temp IP string
    /// retrieve RESTful URI routing
    // - should set URI, Table,TableIndex,TableRecordProps,TableEngine,
    // ID, UriBlobFieldName and Parameters members
    // - all Table* members will be set via a InternalSetTableFromTableName() call
    // - default implementation expects an URI encoded with
    // 'ModelRoot[/TableName[/TableID][/BlobFieldName]][?param=...]' format
    // - will also set UriSessionSignaturePos and UriWithoutSignature members
    // - return FALSE in case of incorrect URI (e.g. does not match Model.Root)
    function UriDecodeRest: boolean; virtual;
    /// retrieve method-based SOA URI routing with optional RESTful mode
    // - should set MethodIndex member
    // - default RESTful implementation expects an URI encoded with
    // 'ModelRoot/MethodName' or 'ModelRoot/TableName[/TableID]/MethodName' formats
    procedure UriDecodeSoaByMethod; virtual;
    /// retrieve interface-based SOA
    // - should set Service member (and possibly InterfaceMethodIndex)
    // - abstract implementation which is to be overridden
    procedure UriDecodeSoaByInterface; virtual; abstract;
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
    /// the associated TRestServer instance which executes its URI method
    Server: TRestServer;
    /// just a wrapper over ServiceRunningContext function result
    // - avoid a call to the threadvar resolution
    ThreadServer: PServiceRunningContext;
    /// the used Client-Server method (matching the corresponding HTTP Verb)
    // - this property will be set from incoming URI, even if RESTful
    // authentication is not enabled
    Method: TUriMethod;
    /// the URI address, excluding trailing /info and ?par1=.... parameters
    // - can be either the table name (in RESTful protocol), or a service name
    Uri: RawUtf8;
    /// same as Call^.Uri, but without the &session_signature=... ending
    UriWithoutSignature: RawUtf8;
    /// points inside Call^.Uri, after the 'root/' prefix
    UriAfterRoot: PUtf8Char;
    /// the optional Blob field name as specified in URI
    // - e.g. retrieved from "ModelRoot/TableName/TableID/BlobFieldName"
    UriBlobFieldName: RawUtf8;
    /// position of the &session_signature=... text in Call^.Url string
    UriSessionSignaturePos: integer;
    /// the Table as specified at the URI level (if any)
    Table: TOrmClass;
    /// the index in the Model of the Table specified at the URI level (if any)
    TableIndex: integer;
    /// the RTTI properties of the Table specified at the URI level (if any)
    TableModelProps: TOrmModelProperties;
    /// the RESTful instance implementing the Table specified at the URI level (if any)
    // - equals the Server field most of the time, but may be an TRestStorage
    // for any in-memory/MongoDB/virtual instance
    TableEngine: TRestOrm;
    /// the associated TOrm.ID, as decoded from URI scheme
    // - this property will be set from incoming URI, even if RESTful
    // authentication is not enabled
    TableID: TID;
    /// the current execution command
    Command: TRestServerUriContextCommand;
    /// the index of the callback published method within the internal class list
    MethodIndex: integer;
    /// the service identified by an interface-based URI
    // - is an TServiceFactoryServer instance
    Service: TServiceFactory;
    /// the method index for an interface-based service
    // - Service member has already be retrieved from URI (so is not nil)
    // - 0..2 are the internal _free_/_contract_/_signature_ pseudo-methods
    ServiceMethodIndex: integer;
    /// access to the raw PInterfaceMethod information of an interface-based URI
    // - equals nil if InterfaceMethodIndex in 0..2 (pseudo-methods)
    ServiceMethod: pointer;
    /// the JSON array of parameters for an the interface-based service
    // - Service member has already be retrieved from URI (so is not nil)
    ServiceParameters: PUtf8Char;
    /// the instance ID for interface-based services instance
    // - can be e.g. the client session ID for sicPerSession or the thread ID for
    // sicPerThread
    ServiceInstanceID: TID;
    /// the current execution context of an interface-based service
    // - maps to Service.fExecution[InterfaceMethodIndex-SERVICE_PSEUDO_METHOD_COUNT]
    ServiceExecution: PServiceFactoryExecution;
    /// the current execution options of an interface-based service
    // - contain ServiceExecution.Options including optNoLogInput/optNoLogOutput
    // in case of TInterfaceFactory.RegisterUnsafeSPIType
    ServiceExecutionOptions: TInterfaceMethodOptions;
    /// low-level index of the Service in the internal methods list
    ServiceListInterfaceMethodIndex: integer;
    /// force the interface-based service methods to return a JSON object
    // - default behavior is to follow Service.ResultAsJsonObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a JSON object, even if Service.ResultAsJsonObject=false: this may be
    // handy when the method is executed from a JavaScript content
    ForceServiceResultAsJsonObject: boolean;
    /// force the interface-based service methods to return a plain JSON object
    // - i.e. '{....}' instead of '{"result":{....}}'
    // - only set if ForceServiceResultAsJsonObject=TRUE and if no ID is about
    // to be returned
    // - could be used e.g. for stateless interaction with a (non mORMot)
    // stateless JSON REST Server
    ForceServiceResultAsJsonObjectWithoutResult: boolean;
    /// force the interface-based service methods to return a XML object
    // - default behavior is to follow Service.ResultAsJsonObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a XML object, by setting ForceServiceResultAsJsonObject then converting
    // the resulting JSON object into the corresponding XML via JsonBufferToXML()
    // - TRestServerUriContext.InternalExecuteSoaByInterface will inspect the
    // Accept HTTP header to check if the answer should be XML rather than JSON
    ForceServiceResultAsXMLObject: boolean;
    /// specify a custom name space content when returning a XML object
    // - default behavior is to follow Service.ResultAsXMLObjectNameSpace
    // property (which is void by default)
    // - service may set e.g. XMLUTF8_NAMESPACE, which will append <content ...>
    // </content> around the generated XML data, to avoid validation problems
    // or set a particular XML name space, depending on the application
    ForceServiceResultAsXMLObjectNameSpace: RawUtf8;
    /// URI inlined parameters
    // - use UrlDecodeValue*() functions to retrieve the values
    // - for mPOST requests, will also be filled for following content types:
    // ! application/x-www-form-urlencoded or multipart/form-data
    Parameters: PUtf8Char;
    /// URI inlined parameters position in Call^.Url string
    // - use Parameters field to retrieve the values
    ParametersPos: integer;
    /// access to all input/output parameters at TRestServer.Uri() level
    // - process should better call Results() or Success() methods to set the
    // appropriate answer or Error() method in case of an error
    // - low-level access to the call parameters can be made via this pointer
    Call: PRestUriParams;
    /// the corresponding session TAuthSession.IDCardinal value
    // - equals 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED) if the session
    // is not started yet - i.e. if still in handshaking phase
    // - equals 1 (CONST_AUTHENTICATION_NOT_USED) if authentication mode
    // is not enabled - i.e. if TRestServer.HandleAuthentication = FALSE
    Session: cardinal;
    /// the corresponding TAuthSession.User.GroupRights.ID value
    // - is undefined if Session is 0 or 1 (no authentication running)
    SessionGroup: TID;
    /// the corresponding TAuthSession.User.ID value
    // - is undefined if Session is 0 or 1 (no authentication running)
    SessionUser: TID;
    /// the corresponding TAuthSession.User.LogonName value
    // - is undefined if Session is 0 or 1 (no authentication running)
    SessionUserName: RawUtf8;
    /// the static instance corresponding to the associated Table (if any)
    StaticOrm: TRestOrm;
    /// the kind of static instance corresponding to the associated Table (if any)
    StaticKind: TRestServerKind;
    /// optional error message which will be transmitted as JSON error (if set)
    // - contains e.g. TOnAuthenticationFailedReason text during
    // TRestServer.OnAuthenticationFailed event call, or the reason of a
    // TRestServer.RecordCanBeUpdated failure
    CustomErrorMsg: RawUtf8;
    /// high-resolution timimg of the execution command, in micro-seconds
    // - only set when TRestServer.Uri finished
    MicroSecondsElapsed: QWord;
    /// JWT validation information, as filled by AuthenticationCheck()
    JwtContent: TJwtContent;
    /// associated logging instance for the current thread on the server
    // - you can use it to log some process on the server side
    Log: TSynLog;
    /// initialize the execution context
    // - this method could have been declared as protected, since it should
    // never be called outside the TRestServer.Uri() method workflow
    // - should set Call, and Method members
    constructor Create(aServer: TRestServer;
      const aCall: TRestUriParams); virtual;
    /// finalize the execution context
    destructor Destroy; override;
    /// validate mPost/mPut/mDelete action against those access rights
    // - used by TRestServerUriContext.ExecuteOrmWrite and
    // TRestServer.EngineBatchSend methods for proper security checks
    function CanExecuteOrmWrite(Method: TUriMethod;
      Table: TOrmClass; TableIndex: integer; const TableID: TID;
      const Rights: TOrmAccessRights): boolean;
    /// extract the input parameters from its URI
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
    // may be a resourcestring - if the parameter is not found
    // - returns TRUE and set Value if the parameter is found
    function InputUtf8OrError(const ParamName: RawUtf8; out Value: RawUtf8;
      const ErrorMessageForMissingParameter: string): boolean;
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
      InterfaceMethod: pointer): variant;
    /// decode any multipart/form-data POST request input
    // - returns TRUE and set MultiPart array as expected, on success
    function InputAsMultiPart(var MultiPart: TMultiPartDynArray): boolean;
    /// low-level access to the input parameters, stored as pairs of UTF-8
    // - even items are parameter names, odd are values
    // - Input*[] properties should have been called previously to fill the
    // internal array, or by calling FillInput if you do not know the input
    // parameters which may appear
    property InputPairs: TRawUtf8DynArray
      read fInput;
    /// retrieve an incoming HTTP header
    // - the supplied header name is case-insensitive
    // - but rather call RemoteIP or UserAgent properties instead of
    // InHeader['remoteip'] or InHeader['User-Agent']
    property InHeader[const HeaderName: RawUtf8]: RawUtf8
      read GetInHeader;
    /// retrieve an incoming HTTP cookie value
    // - cookie name are case-sensitive
    property InCookie[CookieName: RawUtf8]: RawUtf8
      read GetInCookie write SetInCookie;
    /// define a new 'name=value' cookie to be returned to the client
    // - if not void, TRestServer.Uri() will define a new 'set-cookie: ...'
    // header in Call^.OutHead
    // - you can use COOKIE_EXPIRED as value to delete a cookie in the browser
    // - if no Path=/.. is included, it will append
    // $ '; Path=/'+Server.Model.Root+'; HttpOnly'
    property OutSetCookie: RawUtf8
      read fOutSetCookie write SetOutSetCookie;
    /// retrieve the "User-Agent" value from the incoming HTTP headers
    property UserAgent: RawUtf8
      read GetUserAgent;
    /// retrieve the "RemoteIP" value from the incoming HTTP headers
    property RemoteIP: RawUtf8
      read GetRemoteIP;
    /// true if the "RemoteIP" value from the incoming HTTP headers is '127.0.0.1'
    property RemoteIPIsLocalHost: boolean
      read GetRemoteIPIsLocalHost;
    /// "RemoteIP" value from the incoming HTTP headers but '' for '127.0.0.1'
    property RemoteIPNotLocal: RawUtf8
      read GetRemoteIPNotLocal;
    /// retrieve the "Authorization: Bearer <token>" value from incoming HTTP headers
    // - typically returns a JWT for statelesss self-contained authentication,
    // as expected by TJwtAbstract.Verify method
    // - as an alternative, a non-standard and slightly less safe way of
    // token transmission may be to encode its value as ?authenticationbearer=....
    // URI parameter (may be convenient when embedding resources in HTML DOM)
    function AuthenticationBearerToken: RawUtf8;
    /// validate "Authorization: Bearer <JWT>" content from incoming HTTP headers
    // - returns true on success, storing the payload in the JwtContent field
    // - set JwtContent.result = jwtNoToken if jwt is nil
    // - on failure (i.e. returns false), will set the error context as
    // 403 HTTP_FORBIDDEN so that you may directly write:
    // ! procedure TMyDaemon.Files(Ctxt: TRestServerUriContext);
    // ! begin
    // !   if Ctxt.AuthenticationCheck(fJWT) then
    // !     Ctxt.ReturnFileFromFolder('c:\datafolder');
    // ! end;
    function AuthenticationCheck(jwt: TJwtAbstract): boolean; virtual;
    /// low-level access to the associated Session
    // - may be nil depending on the context: you should NOT use it, but the
    // safe Session, SessionGroup, SessionUser, SessionUserName fields instead
    // - is used internally
    property AuthSession: TAuthSession
      read fAuthSession;
    /// identify which kind of client is actually connected
    // - the "User-Agent" HTTP will be checked for 'mORMot' substring, and
    // set ckFramework on match
    // - either ckAjax for a classic (AJAX) browser, or any other kind of
    // HTTP client
    // - will be used e.g. by ClientOrmOptions to check if the
    // current remote client expects standard JSON in all cases
    function ClientKind: TRestServerUriContextClientKind;
    /// the associated routing class on the client side
    class function ClientRouting: TRestClientRoutingClass; virtual; abstract;
    /// identify if the request is about a Table containing nested objects or
    // arrays, which could be serialized as JSON objects or arrays, instead
    // of plain JSON string (as stored in the database)
    // - will idenfity ClientKind=ckAjax, or check for rsoGetAsJsonNotAsString
    // in TRestServer.Options
    function ClientOrmOptions: TJsonSerializerOrmOptions;
    /// true if called from TRestServer.AdministrationExecute
    function IsRemoteAdministrationExecute: boolean;
      {$ifdef FPC}inline;{$endif}
    /// compute the file name corresponding to the URI
    // - e.g. '/root/methodname/toto/index.html' will return 'toto\index.html'
    property ResourceFileName: TFileName
      read GetResourceFileName;
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
    // - set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
    procedure Returns(const result: RawUtf8; Status: integer = HTTP_SUCCESS;
      const CustomHeader: RawUtf8 = ''; Handle304NotModified: boolean = false;
      HandleErrorAsRegularResult: boolean = false; CacheControlMaxAge: integer = 0;
      ServerHash: RawUtf8 = ''); overload;
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
      OrmOptions: TJsonSerializerOrmOptions = [];
      const CustomHeader: RawUtf8 = ''); overload;
    /// use this method to send back any variant as JSON to the caller
    // - this method will call VariantSaveJson() to compute the returned content
    procedure ReturnsJson(const Value: variant; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = false; Escape: TTextWriterKind = twJsonEscape;
      MakeHumanReadable: boolean = false; const CustomHeader: RawUtf8 = '');
    /// uses this method to send back directly any binary content to the caller
    // - the exact MIME type will be retrieved using GetMimeContentTypeHeader(),
    // from the supplied Blob binary buffer, and optional a file name
    // - by default, the HTTP_NOTMODIFIED process will take place, to minimize
    // bandwidth between the server and the client
    // - set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
    procedure ReturnBlob(const Blob: RawByteString; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = true; const FileName: TFileName = '';
      CacheControlMaxAge: integer = 0);
    /// use this method to send back a file to the caller
    // - this method will let the HTTP server return the file content
    // - if Handle304NotModified is TRUE, will check the file age to ensure
    // that the file content will be sent back to the server only if it changed;
    // set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
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
      CacheControlMaxAge: integer = 0);
    /// use this method to send back a file from a local folder to the caller
    // - UriBlobFieldName value, as parsed from the URI, will containn the
    // expected file name in the local folder, using DefaultFileName if the
    // URI is void, and redirecting to Error404Redirect if the file is not found
    // - this method will let the HTTP server return the file content
    // - if Handle304NotModified is TRUE, will check the file age to ensure
    // that the file content will be sent back to the server only if it changed
    // set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
    procedure ReturnFileFromFolder(const FolderName: TFileName;
      Handle304NotModified: boolean = true;
      const DefaultFileName: TFileName = 'index.html';
      const Error404Redirect: RawUtf8 = ''; CacheControlMaxAge: integer = 0);
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
    // set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
    procedure Results(const Values: array of const;
      Status: integer = HTTP_SUCCESS; Handle304NotModified: boolean = false;
      CacheControlMaxAge: integer = 0);
    /// use this method if the caller expect no data, just a status
    // - just wrap the overloaded Returns() method with no result value
    // - if Status is an error code, it will call Error() method
    // - by default, calling this method will mark process as successfull
    procedure Success(Status: integer = HTTP_SUCCESS); virtual;
    /// use this method to send back an error to the caller
    // - expects Status to not be HTTP_SUCCESS neither HTTP_CREATED,
    // and will send back a JSON error message to the caller, with the
    // supplied error text
    // - set CacheControlMaxAge<>0 to include a Cache-Control: max-age = xxx header
    // - if no ErrorMessage is specified, will return a default text
    // corresponding to the Status code
    procedure Error(const ErrorMessage: RawUtf8 = '';
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAge: integer = 0); overload; virtual;
    /// use this method to send back an error to the caller
    // - implementation is just a wrapper over Error(FormatUtf8(Format,Args))
    procedure Error(const Format: RawUtf8; const Args: array of const;
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAge: integer = 0); overload;
    /// use this method to send back an error to the caller
    // - will serialize the supplied exception, with an optional error message
    procedure Error(E: Exception; const Format: RawUtf8;
      const Args: array of const; Status: integer = HTTP_BADREQUEST); overload; virtual;
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
    /// low-level preparation of the JSON result for a service execution
    procedure ServiceResultStart(WR: TTextWriter); virtual;
    /// low-level closure of the JSON result for a service execution
    procedure ServiceResultEnd(WR: TTextWriter; ID: TID); virtual;
    /// low-level statistics merge during service execution
    procedure StatsFromContext(Stats: TSynMonitorInputOutput;
      var Diff: Int64; DiffIsMicroSecs: boolean);
    /// low-level HTTP header merge of the OutSetCookie value
    procedure OutHeadFromCookie;
    /// event raised by ExecuteMethod() for interface parameters
    // - match TInterfaceMethodInternalExecuteCallback signature
    procedure ExecuteCallback(var Par: PUtf8Char; ParamInterfaceInfo: TRttiJson;
      out Obj); virtual;
  end;

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
  // - Ctxt.Parameters values are set from incoming URI, and each parameter can be
  // retrieved with a loop like this:
  // !  if not UrlDecodeNeedParameters(Ctxt.Parameters,'SORT,COUNT') then
  // !    exit;
  // !  while Ctxt.Parameters<>nil do
  // !  begin
  // !    UrlDecodeValue(Ctxt.Parameters,'SORT=',aSortString);
  // !    UrlDecodeValueInteger(Ctxt.Parameters,'COUNT=',aCountInteger,@Ctxt.Parameters);
  // !  end;
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
  // ! var a,b: TSynExtended;
  // ! begin
  // !   if UrlDecodeNeedParameters(Ctxt.Parameters, 'A,B') then
  // !   begin
  // !     while Ctxt.Parameters <> nil do
  // !     begin
  // !       UrlDecodeExtended(Ctxt.Parameters,'A=', a);
  // !       UrlDecodeExtended(Ctxt.Parameters,'B=', b, @Ctxt.Parameters);
  // !     end;
  // !     Ctxt.Results([a + b]);
  // !     // same as: Ctxt.Returns(JsonEncode(['result', a + b]));
  // !     // same as: Ctxt.Returns(['result', a + b]);
  // !   end
  // !   else
  // !     Ctxt.Error('Missing Parameter');
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
    /// detailed statistics associated with this method
    Stats: TSynMonitorInputOutput;
  end;

  /// used to store all method-based services of a TRestServer instance
  TRestServerMethods = array of TRestServerMethod;

  /// pointer to a description of a method-based service
  PRestServerMethod = ^TRestServerMethod;


{ ************ TRestServerRoutingRest/TRestServerRoutingRest Requests Parsing Scheme }

  /// calling context for a TOnRestServerCallBack using simple REST for
  // interface-based services
  // - match TRestClientRoutingRest reciprocal class
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
  TRestServerRoutingJsonRpc = class(TRestServerUriContext)
  protected
    /// retrieve interface-based SOA with URI RESTful routing
    // - should set Service member (and possibly ServiceMethodIndex)
    // - this overridden implementation expects an URI encoded with
    // '/Model/Interface.Method[/ClientDrivenID]' for this class, and
    // will set ServiceMethodIndex for next ExecuteSoaByInterface method call
    procedure UriDecodeSoaByInterface; override;
    /// direct launch of an interface-based service with URI RESTful routing
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
    // - this overriden method returns TRestClientRoutingJsonRpc
    class function ClientRouting: TRestClientRoutingClass; override;
  end;

  /// calling context for a TOnRestServerCallBack using JSON/RPC for
  // interface-based services
  // - match TRestClientRoutingJsonRpc reciprocal class
  // - in this routing scheme, the URI will define the interface, then the
  // method name will be inlined with parameters, e.g.
  // $ POST /root/Calculator
  // $ (...)
  // $ {"method":"Add","params":[1,2]}
  // or, for a sicClientDriven mode service:
  // $ POST /root/ComplexNumber
  // $ (...)
  // $ {"method":"Add","params":[20,30],"id":1234}
  TRestServerRoutingRest = class(TRestServerUriContext)
  protected
    /// retrieve interface-based SOA with URI JSON/RPC routing
    // - this overridden implementation expects an URI encoded with
    // '/Model/Interface' as for the JSON/RPC routing scheme, and won't
    // set ServiceMethodIndex at this level (but in ExecuteSoaByInterface)
    procedure UriDecodeSoaByInterface; override;
    /// direct launch of an interface-based service with URI JSON/RPC routing
    // - Uri() will ensure that Service<>nil before calling it
    // - this overridden implementation expects parameters to be sent as part
    // of a JSON object body:
    // $ {"method":"Add","params":[20,30],"id":1234}
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

  /// class used to maintain in-memory sessions
  // - this is not a TOrm table so won't be remotely accessible, for
  // performance and security reasons
  // - the User field is a true instance, copy of the corresponding database
  // content (for better speed)
  // - you can inherit from this class, to add custom session process
  TAuthSession = class(TSynPersistent)
  protected
    fUser: TAuthUser;
    fID: RawUtf8;
    fIDCardinal: cardinal;
    fTimeOutTix: cardinal;
    fTimeOutShr10: cardinal;
    fPrivateKey: RawUtf8;
    fPrivateSalt: RawUtf8;
    fSentHeaders: RawUtf8;
    fRemoteIP: RawUtf8;
    fPrivateSaltHash: cardinal;
    fLastTimestamp: cardinal;
    fExpectedHttpAuthentication: RawUtf8;
    fAccessRights: TOrmAccessRights;
    fMethods: TSynMonitorInputOutputObjArray;
    fInterfaces: TSynMonitorInputOutputObjArray;
    function GetUserName: RawUtf8;
    function GetUserID: TID;
    function GetGroupID: TID;
    procedure SaveTo(W: TBufferWriter); virtual;
    procedure ComputeProtectedValues; virtual;
  public
    /// initialize a session instance with the supplied TAuthUser instance
    // - this aUser instance will be handled by the class until Destroy
    // - raise an exception on any error
    // - on success, will also retrieve the aUser.Data BLOB field content
    constructor Create(aCtxt: TRestServerUriContext;
      aUser: TAuthUser); reintroduce; virtual;
    /// initialize a session instance from some persisted buffer
    // - following the TRestServer.SessionsSaveToFile binary layout
    constructor CreateFrom(var Read: TFastReader; Server: TRestServer); virtual;
    /// will release the User and User.GroupRights instances
    destructor Destroy; override;
    /// initialize the Interfaces: TSynMonitorInputOutputObjArray statistics
    procedure InterfacesSetLength(MethodCount: integer);
  public
    /// the session ID number, as numerical value
    // - never equals to 1 (CONST_AUTHENTICATION_NOT_USED, i.e. authentication
    // mode is not enabled), nor 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED,
    // i.e. session still in handshaking phase)
    property IDCardinal: cardinal
      read fIDCardinal;
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
  published
    /// the session ID number, as text
    property ID: RawUtf8
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
    /// the remote IP, if any
    // - is extracted from SentHeaders properties
    property RemoteIP: RawUtf8
      read fRemoteIP;
  end;

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
    // - method execution is protected by TRestServer.fSessions.Lock
    function Auth(Ctxt: TRestServerUriContext): boolean; virtual; abstract;
    /// called by the Server to check if the execution context match a session
    // - returns a session instance corresponding to the remote request, and
    // fill Ctxt.Session* members according to in-memory session information
    // - returns nil if this remote request does not match this authentication
    // - method execution should be protected by TRestServer.fSessions.Lock
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
    // - method execution should be protected by TRestServer.fSessions.Lock
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
    // - method execution should be protected by TRestServer.fSessions.Lock
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
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(Ctxt: TRestServerUriContext): TAuthSession; override;
  end;

  /// authentication using HTTP Basic scheme
  // - match TRestClientAuthenticationHttpBasic on Client side
  // - this protocol send both name and password as clear (just base-64 encoded)
  // so should only be used over SSL / HTTPS, or for compatibility reasons
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
    fSspiAuthContextCount: integer;
    fSspiAuthContexts: TDynArray;
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
  public
    /// initialize the instance
    constructor Create(aServer: TRestServer); reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// should be called when a task successfully ended
    // - thread-safe method
    procedure ProcessSuccess(IsOutcomingFile: boolean); virtual;
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



{ ************ TInterfacedCallback/TBlockingCallback Classes }

  /// TInterfacedObject class which will notify a REST server when it is released
  // - could be used when implementing event callbacks as interfaces, so that
  // the other side instance will be notified when it is destroyed
  TInterfacedCallback = class(TInterfacedObjectLocked)
  protected
    fRest: TRest;
    fInterface: TGUID;
  public
    /// initialize the instance for a given REST and callback interface
    constructor Create(aRest: TRest; const aGuid: TGUID); reintroduce;
    /// notify the associated TRestServer that the callback is disconnnected
    // - i.e. will call TRestServer's TServiceContainer.CallBackUnRegister()
    // - this method will process the unsubscription only once
    procedure CallbackRestUnregister; virtual;
    /// finalize the instance, and notify the TRestServer that the callback
    // is now unreachable
    // - i.e. will call CallbackRestUnregister
    destructor Destroy; override;
    /// the associated TRestServer instance, which will be notified
    // when the callback is released
    property Rest: TRest
      read fRest;
    /// the interface type, implemented by this callback class
    property RestInterface: TGUID
      read fInterface write fInterface;
  end;

  /// asynchrounous callback to emulate a synchronous/blocking process
  // - once created, process will block via a WaitFor call, which will be
  // released when CallbackFinished() is called by the process background thread
  TBlockingCallback = class(TInterfacedCallback)
  protected
    fProcess: TBlockingProcess;
    function GetEvent: TBlockingEvent;
  public
    /// initialize the callback instance
    // - specify a time out millliseconds period after which blocking execution
    // should be handled as failure (if 0 is set, default 3000 will be used)
    // - you can optionally set a REST and callback interface for automatic
    // notification when this TInterfacedCallback will be released
    constructor Create(aTimeOutMs: integer;
      aRest: TRest; const aGuid: TGUID); reintroduce;
    /// finalize the callback instance
    destructor Destroy; override;
    /// called to wait for the callback to be processed, or trigger timeout
    // - will block until CallbackFinished() is called by the processing thread
    // - returns the final state of the process, i.e. beRaised or beTimeOut
    function WaitFor: TBlockingEvent; virtual;
    /// should be called by the callback when the process is finished
    // - the caller will then let its WaitFor method return
    // - if aServerUnregister is TRUE, will also call CallbackRestUnregister to
    // notify the server that the callback is no longer needed
    // - will optionally log all published properties values to the log class
    // of the supplied REST instance
    procedure CallbackFinished(aRestForLog: TRestOrm;
      aServerUnregister: boolean = false); virtual;
    /// just a wrapper to reset the internal Event state to evNone
    // - may be used to re-use the same TBlockingCallback instance, after
    // a successfull WaitFor/CallbackFinished process
    // - returns TRUE on success (i.e. status was not beWaiting)
    // - if there is a WaitFor currently in progress, returns FALSE
    function Reset: boolean; virtual;
    /// the associated blocking process instance
    property Process: TBlockingProcess
      read fProcess;
  published
    /// the current state of process
    // - just a wrapper around Process.Event
    // - use Reset method to re-use this instance after a WaitFor process
    property Event: TBlockingEvent
      read GetEvent;
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
  // rsoHttpHeaderCheckDisable is defined (to gain a few cycles?)
  // - by default, TAuthUser.Data blob is retrieved from the database,
  // unless rsoGetUserRetrieveNoBlobData is defined
  // - rsoNoInternalState could be state to avoid transmitting the
  // 'Server-InternalState' header, e.g. if the clients wouldn't need it
  // - rsoNoTableURI will disable any /root/tablename URI for safety
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
    rsoNoTableURI);

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
    Sort: RawUtf8;
    /// parameter name used to specify the request sort direction
    // - default value is 'DIR='
    Dir: RawUtf8;
    /// parameter name used to specify the request starting offset
    // - default value is 'STARTINDEX='
    StartIndex: RawUtf8;
    /// parameter name used to specify the request the page size (LIMIT clause)
    // - default value is 'RESULTS='
    Results: RawUtf8;
    /// parameter name used to specify the request field names
    // - default value is 'SELECT='
    Select: RawUtf8;
    /// parameter name used to specify the request WHERE clause
    // - default value is 'WHERE='
    Where: RawUtf8;
    /// returned JSON field value of optional total row counts
    // - default value is nil, i.e. no total row counts field
    // - computing total row counts can be very expensive, depending on the
    // database back-end used (especially for external databases)
    // - can be set e.g. to ',"totalRows":%' value (note that the initial "," is
    // expected by the produced JSON content, and % will be set with the value)
    SendTotalRowsCountFmt: RawUtf8;
  end;

  ///  used to define how to trigger Events on record update
  // - see TRestServer.OnUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occured (but action must continue)
  // - to be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientUri.UpdateFromServer)
  TOnOrmEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aSentData: RawUtf8): boolean of object;

  ///  used to define how to trigger Events on record field update
  // - see TRestServer.OnBlobUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occured (but action must continue)
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
    aConnectionID: Int64; aFakeCallID: integer;
    aResult, aErrorMsg: PRawUtf8): boolean of object;

  /// event signature used by TRestServer.OnServiceCreateInstance
  // - as called by TServiceFactoryServer.CreateInstance
  // - the actual Instance class can be quickly retrieved from
  // TServiceFactoryServer(Sender).ImplementationClass
  TOnServiceCreateInstance = procedure(
    Sender: TServiceFactory; Instance: TInterfacedObject) of object;

  /// callback allowing to customize the information returned by root/timestamp/info
  TOnInternalInfo = procedure(Sender: TRestServer;
    var info: TDocVariantData) of object;

  /// a generic REpresentational State Transfer (REST) server
  // - descendent must implement the protected EngineList() Retrieve() Add()
  // Update() Delete() methods
  // - automatic call of this methods by a generic Uri() RESTful function
  // - any published method of descendants must match TOnRestServerCallBack
  // prototype, and is expected to be thread-safe
  TRestServer = class(TRest)
  protected
    fHandleAuthentication: boolean;
    fBypassOrmAuthentication: TUriMethods;
    /// the TAuthUser and TAuthGroup classes, as defined in model
    fAuthUserClass: TAuthUserClass;
    fAuthGroupClass: TAuthGroupClass;
    fAfterCreation: boolean;
    fOptions: TRestServerOptions;
    /// how in-memory sessions are handled
    fSessionClass: TAuthSessionClass;
    fJwtForUnauthenticatedRequest: TJwtAbstract;
    /// in-memory storage of TAuthSession instances
    fSessions: TSynObjectListLocked;
    fSessionsDeprecatedTix: cardinal;
    /// used to compute genuine TAuthSession.ID cardinal value
    fSessionCounter: cardinal;
    fSessionAuthentication: TRestServerAuthenticationDynArray;
    fPublishedMethod: TRestServerMethods;
    fPublishedMethods: TDynArrayHashed;
    fPublishedMethodTimestampIndex: integer;
    fPublishedMethodAuthIndex: integer;
    fPublishedMethodBatchIndex: integer;
    fStats: TRestServerMonitor;
    fStatLevels: TRestServerMonitorLevels;
    fStatUsage: TSynMonitorUsage;
    fShutdownRequested: boolean;
    fAssociatedServices: TServicesPublishedInterfacesList;
    fRootRedirectGet: RawUtf8;
    fPublicUri: TRestServerUri;
    fIPBan, fIPWhiteJWT: TIPBan;
    fOnIdleLastTix: cardinal;
    fServicesRouting: TRestServerUriContextClass;
    fRecordVersionSlaveCallbacks: array of IServiceRecordVersionCallback;
    fServer: IRestOrmServer;
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
    procedure InternalStat(Ctxt: TRestServerUriContext; W: TTextWriter); virtual;
    procedure AddStat(Flags: TRestServerAddStats; W: TTextWriter);
    procedure InternalInfo(var info: TDocVariantData); virtual;
    procedure SetStatUsage(usage: TSynMonitorUsage);
    function GetServiceMethodStat(const aMethod: RawUtf8): TSynMonitorInputOutput;
    procedure SetRoutingClass(aServicesRouting: TRestServerUriContextClass);
    /// add a new session to the internal session list
    // - do not use this method directly: this callback is to be used by
    // TRestServerAuthentication* classes
    // - will check that the logon name is valid
    // - on failure, will call TRestServerUriContext.AuthenticationFailed()
    // with afSessionAlreadyStartedForThisUser or afSessionCreationAborted reason
    procedure SessionCreate(var User: TAuthUser; Ctxt: TRestServerUriContext;
      out Session: TAuthSession); virtual;
    /// search for Ctxt.Session ID and fill Ctxt.Session* members if found
    // - returns nil if not found, or fill aContext.User/Group values if matchs
    // - this method will also check for outdated sessions, and delete them
    // - this method is not thread-safe: caller should use Sessions.Lock/Unlock
    function SessionAccess(Ctxt: TRestServerUriContext): TAuthSession;
    /// delete a session from its index in Sessions[]
    // - will perform any needed clean-up, and log the event
    // - this method is not thread-safe: caller should use Sessions.Lock/Unlock
    procedure SessionDelete(aSessionIndex: integer; Ctxt: TRestServerUriContext);
    /// SessionAccess will detect and delete outdated sessions, but you can call
    // this method to force checking for deprecated session now
    // - may be used e.g. from OnSessionCreate to limit the number of active sessions
    // - this method is not thread-safe: caller should use Sessions.Lock/Unlock
    // - you can call it often: it will seek for outdated sessions once per second
    // - returns the current system Ticks number (at second resolution)
    function SessionDeleteDeprecated: cardinal;
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
    /// this event will be executed to push notifications from the server to
    // a remote client, using a (fake) interface parameter
    // - is nil by default, but may point e.g. to TRestHttpServer.NotifyCallback
    OnNotifyCallback: TOnRestServerClientCallback;
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
    // - note that if TRestServer.Uri is not called by any client, this
    // callback won't be executed either
    OnIdle: TNotifyEvent;
    /// this property can be used to specify the URI parmeters to be used
    // for paged queries
    // - is set by default to PAGINGPARAMETERS_YAHOO constant by
    // TRestServer.Create() constructor
    UriPagingParameters: TRestServerUriPagingParameters;

    /// Server initialization with a specified Database Model
    // - if HandleUserAuthentication is false, will set URI access rights to
    // 'Supervisor' (i.e. all R/W access) by default
    // - if HandleUserAuthentication is true, will add TAuthUser and
    // TAuthGroup to the TOrmModel (if not already there)
    constructor Create(aModel: TOrmModel;
      aHandleUserAuthentication: boolean = false); reintroduce; virtual;
    /// initialize REST server instance from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// Server finalization
    destructor Destroy; override;
    /// Server initialization with a temporary Database Model
    // - a Model will be created with supplied tables, and owned by the server
    // - if you instantiate a TRestServerFullMemory or TSqlRestServerDB
    // with this constructor, an in-memory engine will be created, with
    // enough abilities to run regression tests, for instance
    constructor CreateWithOwnModel(const Tables: array of TOrmClass;
      aHandleUserAuthentication: boolean = false;
      const aRoot: RawUtf8 = 'root');
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TInterfacedObject); override;

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
    // - ensure you protect its access calling Sessions.Lock/Sessions.Unlock
    property Sessions: TSynObjectListLocked
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
    // - once set, this instance will be owned by the TSqlRestServer
    // - by definition, such JWT authentication won't identify any mORMot user
    // nor session (it just has to be valid), so only sicSingle, sicShared or
    // sicPerThread interface-based services execution are possible
    // - typical usage is for a public API, in conjunction with
    // ServiceDefine(...).ResultAsJsonObjectWithoutResult := true on the server
    // side and TSqlRestClientUri.ServiceDefineSharedApi() method for the client
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
    // - returns nil if the session does not exist (e.g. if authentication is
    // disabled)
    // - caller MUST release the TAuthUser instance returned (if not nil)
    // - this method IS thread-safe, and calls internally Sessions.Lock
    // (the returned TAuthUser is a private copy from Sessions[].User instance,
    // in order to be really thread-safe)
    // - the returned TAuthUser instance will have GroupRights=nil but will
    // have ID, LogonName, DisplayName, PasswordHashHexa and Data fields available
    function SessionGetUser(aSessionID: cardinal): TAuthUser;
    /// persist all in-memory sessions into a compressed binary file
    // - you should not call this method it directly, but rather use Shutdown()
    // with a StateFileName parameter - to be used e.g. for a short maintainance
    // server shutdown, without loosing the current logged user sessions
    // - this method IS thread-safe, and call internally Sessions.Lock
    procedure SessionsSaveToFile(const aFileName: TFileName);
    /// re-create all in-memory sessions from a compressed binary file
    // - typical use is after a server restart, with the file supplied to the
    // Shutdown() method: it could be used e.g. for a short maintainance server
    // shutdown, without loosing the current logged user sessions
    // - WARNING: this method will restore authentication sessions for the ORM,
    // but not any complex state information used by interface-based services,
    // like sicClientDriven class instances - DO NOT use this feature with SOA
    // - this method IS thread-safe, and call internally Sessions.Lock
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
    procedure ServiceMethodRegisterPublishedMethods(const aPrefix: RawUtf8;
      aInstance: TObject);
    /// direct registration of a method for a given low-level event handler
    procedure ServiceMethodRegister(aMethodName: RawUtf8;
      const aEvent: TOnRestServerCallBack;
      aByPassAuthentication: boolean = false);
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
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
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
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
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
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aImplementationClass: TInterfacedClass;
      const aInterfaces: array of TGUID;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a Service instance on the server side
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - the supplied aSharedImplementation will be owned by this Server instance
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aSharedImplementation: TInterfacedObject;
      const aInterfaces: array of TGUID;
      const aContractExpected: RawUtf8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a remote Service via its interface
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aClient: TRest; const aInterfaces: array of TGUID;
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
    // - this method expects the communication channel to be bidirectional, e.g.
    // a mormot.rest.http.server's TRestHttpServer in useBidirSocket mode
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
    // callbacks using WebSockets, as implemented by mormot.net.ws.core.server and
    // mormot.rest.http.server's TRestHttpServer on useBidirSocket, are asynchronous
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
      read fOptions write fOptions;
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
    // ! [mlTables,mlMethods,mlInterfaces,mlSQLite3]
    // - you can add mlSessions to maintain per-session statistics: this will
    // lead into a slightly higher memory consumption, for each session
    property StatLevels: TRestServerMonitorLevels
      read fStatLevels write fStatLevels;
    /// could be set to track statistic from Stats information
    // - it may be e.g. a TSynMonitorUsageRest instance for REST storage
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
  published
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
    /// REST service accessible from ModelRoot/Auth URI
    // - called by the clients for authentication and session management
    // - this method won't require an authenticated client, since it is used to
    // initiate authentication
    // - this global callback method is thread-safe
    procedure Auth(Ctxt: TRestServerUriContext);
    /// REST service accessible from the ModelRoot/Timestamp URI
    // - returns the server time stamp TTimeLog/Int64 value as UTF-8 text
    // - this method will not require an authenticated client
    // - hidden ModelRoot/Timestamp/info command will return basic execution
    // information, less verbose (and sensitive) than Stat(), calling virtual
    // InternalInfo() protected method
    procedure Timestamp(Ctxt: TRestServerUriContext);
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
  // - i.e. gather all statistics, but mlSessions
  SERVERDEFAULTMONITORLEVELS: TRestServerMonitorLevels =
    [mlTables, mlMethods, mlInterfaces, mlSQLite3];


function ToText(res: TOnAuthenticationFailedReason): PShortString; overload;


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
// !var context: PServiceRunningContext;
// !    inContentType: RawUtf8;
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
function CurrentServerNonce(Previous: boolean = false): RawUtf8;

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
    adSSPI);

  /// customize TRestHttpServer process
  // - rsoOnlyJsonRequests to force the server to respond only to MIME type
  // APPLICATION/JSON requests
  // - rsoRedirectServerRootUriForExactCase to search root URI case-sensitive,
  // mainly to avoid errors with HTTP cookies, which path is case-sensitive -
  // when set, such not exact case will be redirected via a HTTP 307 command
  // - rsoHeadersUnFiltered maps THttpServer.HeadersUnFiltered property
  // - rsoCompressSynLZ and rsoCompressGZip enable SynLZ and GZip compression
  // on server side - it should also be enabled for the client
  TRestHttpServerOption = (
    rsoOnlyJsonRequests,
    rsoRedirectServerRootUriForExactCase,
    rsoHeadersUnFiltered,
    rsoCompressSynLZ,
    rsoCompressGZip);

  /// how to customize TRestHttpServer process
  TRestHttpServerOptions = set of TRestHttpServerOption;

  /// parameters supplied to publish a TSqlRestServer via HTTP
  // - used by the overloaded TRestHttpServer.Create(TRestHttpServerDefinition)
  // constructor in mORMotHttpServer.pas, and also in dddInfraSettings.pas
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
  /// default TRestHttpServer processing options
  HTTPSERVER_DEFAULT_OPTIONS = [rsoCompressGZip, rsoCompressSynLZ];



{$ifndef PUREMORMOT2}
// backward compatibility types redirections

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



{ TRestServerUriContext }

constructor TRestServerUriContext.Create(aServer: TRestServer;
  const aCall: TRestUriParams);
begin
  inherited Create;
  Server := aServer;
  Call := @aCall;
  Method := ToMethod(aCall.Method);
  ThreadServer := PerThreadRunningContextAddress;
  ThreadServer^.Request := self;
end;

destructor TRestServerUriContext.Destroy;
begin
  if ThreadServer <> nil then
    ThreadServer^.Request := nil;
  inherited Destroy;
end;

procedure TRestServerUriContext.InternalSetTableFromTableName(
  TableName: PUtf8Char);
begin
  TableEngine := TRestOrm(Server.fOrmInstance);
  if rsoNoTableURI in Server.Options then
    TableIndex := -1
  else
    InternalSetTableFromTableIndex(Server.fModel.GetTableIndexPtr(TableName));
  if TableIndex < 0 then
    exit;
  StaticOrm := TRestOrmServer(Server.fOrmInstance).
    GetStaticTableIndex(TableIndex, StaticKind);
  if StaticOrm <> nil then
    TableEngine := StaticOrm;
end;

procedure TRestServerUriContext.InternalSetTableFromTableIndex(Index: integer);
begin
  TableIndex := Index;
  if TableIndex >= 0 then
    with Server.fModel do
    begin
      self.Table := Tables[TableIndex];
      self.TableModelProps := TableProps[TableIndex];
    end;
end;

function TRestServerUriContext.UriDecodeRest: boolean;
var
  i, j, slash: PtrInt;
  par, P: PUtf8Char;
begin
  // expects 'ModelRoot[/TableName[/TableID][/UriBlobFieldName]][?param=...]' format
  // check root URI and Parameters
  i := 0;
  P := pointer(Call^.Url);
  if (P <> nil) and
     (P^ = '/') then
    inc(i); // URL may be '/path'
  j := length(Server.fModel.Root);
  if (i + j > length(Call^.Url)) or
     not (P[i + j] in [#0, '/', '?']) or
     not IdemPropNameUSameLenNotNull(P + i, pointer(Server.fModel.Root), j) then
  begin
    // URI do not start with Model.Root -> caller can try another TRestServer
    result := False;
    exit;
  end;
  Parameters := PosChar(P + j, '?');
  if Parameters <> nil then
  begin
    // '?select=...&where=...' or '?where=...'
    inc(Parameters);
    ParametersPos := Parameters - P;
  end;
  if Method = mPost then
  begin
    Call^.InBodyType(fInputPostContentType, {guessjsonifnone=}false);
    if (Parameters = nil) and
       IdemPChar(pointer(fInputPostContentType), 'APPLICATION/X-WWW-FORM-URLENCODED') then
      Parameters := pointer(Call^.InBody);
  end;
  // compute URI without any root nor parameter
  inc(i, j + 2);
  UriAfterRoot := P + i - 1;
  if ParametersPos = 0 then
    FastSetString(Uri, UriAfterRoot, length(Call^.Url) - i + 1)
  else
    FastSetString(Uri, UriAfterRoot, ParametersPos - i);
  // compute Table, TableID and UriBlobFieldName
  slash := PosExChar('/', Uri);
  if slash > 0 then
  begin
    Uri[slash] := #0;
    par := pointer(Uri);
    InternalSetTableFromTableName(par);
    inc(par, slash);
    if (Table <> nil) and
       (par^ in ['0'..'9']) then
      // "ModelRoot/TableName/TableID/UriBlobFieldName"
      TableID := GetNextItemInt64(par, '/')
    else
      // URI like "ModelRoot/TableName/MethodName"
      TableID := -1;
    if Table <> nil then
    begin
      P := PosChar(par, '/');
      if P <> nil then
      begin
        // handle "ModelRoot/TableName/UriBlobFieldName/ID"
        TableID := GetCardinalDef(P + 1, cardinal(-1));
        FastSetString(UriBlobFieldName, par, par - P);
      end
      else
        FastSetString(UriBlobFieldName, par, StrLen(par));
    end
    else
      FastSetString(UriBlobFieldName, par, StrLen(par));
    SetLength(Uri, slash - 1);
  end
  else
    // "ModelRoot/TableName"
    InternalSetTableFromTableName(pointer(Uri));
  // compute UriSessionSignaturePos and UriWithoutSignature
  if ParametersPos > 0 then
    if IdemPChar(Parameters, 'SESSION_SIGNATURE=') then
      UriSessionSignaturePos := ParametersPos
    else
      UriSessionSignaturePos := PosEx('&session_signature=', Call^.Url,
        ParametersPos + 1);
  if UriSessionSignaturePos = 0 then
    UriWithoutSignature := Call^.Url
  else
    FastSetString(UriWithoutSignature, pointer(Call^.Url), UriSessionSignaturePos - 1);
  result := True;
end;

procedure TRestServerUriContext.UriDecodeSoaByMethod;
begin
  if Table = nil then
    // check URI as 'ModelRoot/MethodName'
    MethodIndex := Server.fPublishedMethods.FindHashed(Uri)
  else if UriBlobFieldName <> '' then
    // check URI as 'ModelRoot/TableName[/TableID]/MethodName'
    MethodIndex := Server.fPublishedMethods.FindHashed(UriBlobFieldName)
  else
    MethodIndex := -1;
end;

var
  // as set by TRestServer.AdministrationExecute()
  BYPASS_ACCESS_RIGHTS: TOrmAccessRights;

function TRestServerUriContext.IsRemoteAdministrationExecute: boolean;
begin
  result := (self <> nil) and
            (Call.RestAccessRights = @BYPASS_ACCESS_RIGHTS);
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
    Session := CONST_AUTHENTICATION_SESSION_NOT_STARTED;
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
    Server.fSessions.Safe.Lock;
    try
      a := pointer(Server.fSessionAuthentication);
      if a <> nil then
      begin
        n := PDALen(PAnsiChar(a) - _DALEN)^ + _DAOFF;
        repeat
          s := a^.RetrieveSession(self);
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
      Server.fSessions.Safe.UnLock;
    end;
    // if we reached here, no session has been identified
    result := false;
  end
  else
    // default unique session if authentication is not enabled
    Session := CONST_AUTHENTICATION_NOT_USED;
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
  // 401 Unauthorized response MUST include a WWW-Authenticate header,
  // which is not what we used, so here we won't send 401 error code but 403
  Call.OutStatus := HTTP_FORBIDDEN;
  FormatUtf8('Authentication Failed: % (%)',
    [UnCamelCase(TrimLeftLowerCaseShort(txt)), ord(Reason)], CustomErrorMsg);
  // call the notification event
  if Assigned(Server.OnAuthenticationFailed) then
    Server.OnAuthenticationFailed(Server, Reason, nil, self);
end;

procedure TRestServerUriContext.ExecuteCommand;

  procedure TimeOut;
  begin
    Server.InternalLog('TimeOut %.Execute(%) after % ms', [self, ToText(Command)^,
      Server.fAcquireExecution[Command].LockedTimeOut], sllServer);
    if Call <> nil then
      Call^.OutStatus := HTTP_TIMEOUT; // 408 Request Time-out
  end;

var
  method: TThreadMethod;
  starttix: Int64;
  current: cardinal;
begin
  with Server.fAcquireExecution[Command] do
  begin
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
          starttix := GetTickCount64;
          repeat
            if Safe.TryLock then
            try
              current := TRestOrm(Server.fOrmInstance).TransactionActiveSession;
              if (current = 0) or
                 (current = Session) then
              begin
                // avoiding transaction mixups
                if Mode = amLocked then
                begin
                  ExecuteOrmWrite; // process within the obtained write mutex
                  exit;
                end;
                break;   // will handle Mode<>amLocked below
              end;
            finally
              Safe.UnLock;
            end;
            if (LockedTimeOut <> 0) and
               (GetTickCount64 > starttix + LockedTimeOut) then
            begin
              TimeOut; // wait up to 5 second by default
              exit;
            end;
            SleepHiRes(1); // retry every 1 ms
          until Server.fShutdownRequested;
        end;
    else
      raise EOrmException.CreateUtf8('Unexpected Command=% in %.Execute',
        [ord(Command), self]);
    end;
    if Mode = amBackgroundORMSharedThread then
      if (Command = execOrmWrite) and
         (Server.fAcquireExecution[execOrmGet].Mode = amBackgroundORMSharedThread) then
        Command := execOrmGet; // for share same thread for ORM read/write
  end;
  with Server.fAcquireExecution[Command] do
    case Mode of
      amUnlocked:
        method;
      amLocked:
        if LockedTimeOut = 0 then
        begin
          Safe.Lock;
          try
            method;
          finally
            Safe.UnLock;
          end;
        end
        else
        begin
          starttix := GetTickCount64;
          repeat
            if Safe.TryLock then
            try
              method;
            finally
              Safe.UnLock;
            end;
            if GetTickCount64 > starttix + LockedTimeOut then
              break; // wait up to 2 second by default
            SleepHiRes(1); // retry every 1 ms
          until Server.fShutdownRequested;
          TimeOut;
        end;
      amMainThread:
        BackgroundExecuteThreadMethod(method, nil);
      amBackgroundThread, amBackgroundORMSharedThread:
        begin
          if Thread = nil then
            Thread := Server.Run.NewBackgroundThreadMethod('% % %',
              [self, Server.fModel.Root, ToText(Command)^]);
          BackgroundExecuteThreadMethod(method, Thread);
        end;
    end;
end;

procedure TRestServerUriContext.ConfigurationRestMethod(SettingsStorage: TObject);
var
  value: TDocVariantData;
  valid: boolean;
  config: variant;
begin
  UriBlobFieldName := StringReplaceChars(UriBlobFieldName, '/', '.');
  if InputExists['value'] then
  begin
    if UriBlobFieldName = '' then
      exit;
    value.InitObjectFromPath(UriBlobFieldName, Input['value']);
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
  if UriBlobFieldName <> '' then
    config := TDocVariantData(config).GetValueByPath(UriBlobFieldName);
  ReturnsJson(config, HTTP_SUCCESS, true, twJsonEscape, true);
end;

procedure StatsAddSizeForCall(Stats: TSynMonitorInputOutput;
  const Call: TRestUriParams);
begin
  Stats.AddSize( // rough estimation
    length(Call.Url) + length(Call.Method) + length(Call.InHead) +
      length(Call.InBody) + 12,
    length(Call.OutHead) + length(Call.OutBody) + 16);
end;

procedure TRestServerUriContext.StatsFromContext(Stats: TSynMonitorInputOutput;
  var Diff: Int64; DiffIsMicroSecs: boolean);
begin
  StatsAddSizeForCall(Stats, Call^);
  if not StatusCodeIsSuccess(Call.OutStatus) then
    Stats.ProcessErrorNumber(Call.OutStatus);
  if DiffIsMicroSecs then
    // avoid a division
    Stats.FromExternalMicroSeconds(Diff)
  else
    // converted to us
    Diff := Stats.FromExternalQueryPerformanceCounters(Diff);
end;

procedure TRestServerUriContext.OutHeadFromCookie;
begin
  Call.OutHead := TrimU(Call.OutHead + #13#10 +
                        'Set-Cookie: ' + OutSetCookie);
  if rsoCookieIncludeRootPath in Server.fOptions then
    // case-sensitive Path=/ModelRoot
    Call.OutHead := Call.OutHead + '; Path=/';
end;

procedure TRestServerUriContext.ExecuteCallback(var Par: PUtf8Char;
  ParamInterfaceInfo: TRttiJson; out Obj);
var
  fakeid: PtrInt;
begin
  if not Assigned(Server.OnNotifyCallback) then
    raise EServiceException.CreateUtf8(
      '% does not implement callbacks for I%',
      [Server,ParamInterfaceInfo.Name]);
  fakeid := GetInteger(GetJsonField(Par, Par)); // GetInteger returns a PtrInt
  if Par = nil then
    Par := @NULCHAR; // allow e.g. '[12345]'
  if (fakeid=0) or
     (ParamInterfaceInfo=TypeInfo(IInvokable)) then
  begin
    pointer(Obj) := pointer(fakeid); // Obj = IInvokable(fakeid)
    exit;
  end;
  (Server.Services as TServiceContainerServer).GetFakeCallback(
    self, ParamInterfaceInfo.Info, fakeid, Obj);
end;

procedure TRestServerUriContext.ExecuteSoaByMethod;
var
  timstart, timstop: Int64;
  sessionstat: TSynMonitorInputOutput;
begin
  with Server.fPublishedMethod[MethodIndex] do
  begin
    if mlMethods in Server.fStatLevels then
    begin
      QueryPerformanceMicroSeconds(timstart);
      if Stats = nil then
      begin
        Server.fStats.Lock; // global lock for thread-safe initialization
        try
          if Stats = nil then
            Stats := TSynMonitorInputOutput.Create(Name);
        finally
          Server.fStats.UnLock;
        end;
      end;
      Stats.Processing := true;
    end;
    if Parameters <> nil then
      Server.InternalLog('% %', [Name, Parameters], sllServiceCall);
    CallBack(self);
    if Stats <> nil then
    begin
      QueryPerformanceMicroSeconds(timstop);
      dec(timstop, timstart);
      StatsFromContext(Stats, timstop, false);
      if Server.fStatUsage <> nil then
        Server.fStatUsage.Modified(Stats, []);
      if (mlSessions in Server.fStatLevels) and
         (fAuthSession <> nil) then
      begin
        if fAuthSession.fMethods = nil then
        begin
          Server.fStats.Lock;
          try
            if fAuthSession.fMethods = nil then
              SetLength(fAuthSession.fMethods, length(Server.fPublishedMethod));
          finally
            Server.fStats.UnLock;
          end;
        end;
        sessionstat := fAuthSession.fMethods[MethodIndex];
        if sessionstat = nil then
        begin
          Server.fStats.Lock;
          try
            sessionstat := fAuthSession.fMethods[MethodIndex];
            if sessionstat = nil then
            begin
              sessionstat := TSynMonitorInputOutput.Create(Name);
              fAuthSession.fMethods[MethodIndex] := sessionstat;
            end;
          finally
            Server.fStats.UnLock;
          end;
        end;
        StatsFromContext(sessionstat, timstop, true);
        // mlSessions stats are not yet tracked per Client
      end;
    end;
  end;
  LockedInc64(@Server.fStats.fServiceMethod);
end;

procedure TRestServerUriContext.ServiceResultStart(WR: TTextWriter);
const
  JSONSTART: array[boolean] of RawUtf8 = (
    '{"result":[', '{"result":{');
begin
  // InternalExecuteSoaByInterface has set ForceServiceResultAsJsonObject
  if ForceServiceResultAsJsonObjectWithoutResult then
    WR.Add('{')
  else
    WR.AddString(JSONSTART[ForceServiceResultAsJsonObject]);
end;

procedure TRestServerUriContext.ServiceResultEnd(WR: TTextWriter; ID: TID);
const
  JSONSEND_WITHID: array[boolean] of RawUtf8 = (
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
    WR.AddString(JSONSEND_WITHID[ForceServiceResultAsJsonObject]);
    WR.Add(ID); // only used in sicClientDriven mode
  end;
  if not ForceServiceResultAsJsonObjectWithoutResult then
    WR.Add('}');
end;

procedure TRestServerUriContext.InternalExecuteSoaByInterface;

  procedure ComputeResult;

    procedure ServiceResult(const Name, JsonValue: RawUtf8);
    var
      wr: TTextWriter;
      temp: TTextWriterStackBuffer;
    begin
      wr := TJsonSerializer.CreateOwnedStream(temp);
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

  begin
    with TServiceFactoryServer(Service) do
    begin
      // XML needs a full JSON object as input
      ForceServiceResultAsXMLObject := ForceServiceResultAsXMLObject or
                                       ResultAsXMLObject;
      ForceServiceResultAsJsonObject := ForceServiceResultAsJsonObject or
                                        ResultAsJsonObject or
                                        ResultAsJsonObjectWithoutResult or
                                        ForceServiceResultAsXMLObject;
      ForceServiceResultAsJsonObjectWithoutResult :=
        ForceServiceResultAsJsonObject and
        (InstanceCreation in SERVICE_IMPLEMENTATION_NOID) and
        ResultAsJsonObjectWithoutResult;
      if ForceServiceResultAsXMLObjectNameSpace = '' then
        ForceServiceResultAsXMLObjectNameSpace := ResultAsXMLObjectNameSpace;
    end;
    with Server.fStats do
    begin
      fSafe^.Lock;
      inc(fServiceInterface);
      Changed;
      fSafe^.UnLock;
    end;
    case ServiceMethodIndex of
      ord(imFree):
        // {"method":"_free_", "params":[], "id":1234}
        if not (Service.InstanceCreation in [sicClientDriven..sicPerThread]) then
        begin
          Error('_free_ is not compatible with %', [ToText(Service.InstanceCreation)^]);
          exit;
        end;
      ord(imContract):
        begin
          // "method":"_contract_" to retrieve the implementation contract
          if (Call^.InBody <> '') and
             (Call^.InBody <> '[]') then
            Server.AssociatedServices.RegisterFromClientJson(Call^.InBody);
          ServiceResult('contract', Service.ContractExpected);
          exit; // "id":0 for this method -> no instance was created
        end;
      ord(imSignature):
        begin
          // "method":"_signature_" to retrieve the implementation signature
          if TServiceContainerServer(Server.Services).PublishSignature then
            ServiceResult('signature', Service.Contract)
          else
            // "id":0 for this method -> no instance was created
            Error('Not allowed to publish signature');
          exit;
        end;
      ord(imInstance):
        // "method":"_instance_" from TServiceFactoryClient.CreateFakeInstance
        if not (Service.InstanceCreation in [sicClientDriven]) then
        begin
          Error('_instance_ is not compatible with %',
            [ToText(Service.InstanceCreation)^]);
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
       (SessionGroup - 1 in ServiceExecution.Denied) then
    begin
      Error('Unauthorized method', HTTP_NOTALLOWED);
      exit;
    end;
    // if we reached here, we have to run the service method
    TServiceFactoryServer(Service).ExecuteMethod(self);
  end;

var
  xml: RawUtf8;
  m: PtrInt;
begin
  // expects Service, ServiceParameters, ServiceMethod(Index) to be set
  m := ServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT;
  if m >= 0 then
  begin
    if ServiceMethod = nil then
      ServiceMethod := @Service.InterfaceFactory.Methods[m];
    ServiceExecution := @Service.Execution[m];
    ServiceExecutionOptions := ServiceExecution.Options;
    with PInterfaceMethod(ServiceMethod)^ do
    begin
      // log from Ctxt.ServiceExecutionOptions
      if [imdConst, imdVar] * HasSPIParams <> [] then
        include(ServiceExecutionOptions, optNoLogInput);
      if [imdVar, imdOut, imdResult] * HasSPIParams <> [] then
        include(ServiceExecutionOptions, optNoLogOutput);
    end;
    // log method call and parameter values (if worth it)
    if (Log <> nil) and
       (sllServiceCall in Log.GenericFamily.Level) and
       (ServiceParameters <> nil) and
       (PWord(ServiceParameters)^ <> ord('[') + ord(']') shl 8) then
      if optNoLogInput in ServiceExecutionOptions then
        Log.Log(sllServiceCall, '%{}',
          [PInterfaceMethod(ServiceMethod)^.InterfaceDotMethodName], Server)
      else
        Log.Log(sllServiceCall, '%%',
          [Service.InterfaceFactory.GetFullMethodName(ServiceMethodIndex),
           ServiceParameters], Server);
    if Assigned(TServiceFactoryServer(Service).OnMethodExecute) then
      if not TServiceFactoryServer(Service).OnMethodExecute(
         self, PInterfaceMethod(ServiceMethod)^) then
        // execution aborted during OnMethodExecute() callback event
        exit;
  end;
  if TServiceFactoryServer(Service).ResultAsXMLObjectIfAcceptOnlyXML then
  begin
    FindNameValue(Call^.InHead, 'ACCEPT:', xml);
    if (xml = 'application/xml') or
       (xml = 'text/xml') then
      ForceServiceResultAsXMLObject := true;
  end;
  try
    ComputeResult;
  finally
    // ensure no GPF later if points to some local data
    ServiceParameters := nil;
  end;
  if ForceServiceResultAsXMLObject and
     (Call.OutBody <> '') and
     (Call.OutHead <> '') and
     CompareMemFixed(pointer(Call.OutHead),
       pointer(JSON_CONTENT_TYPE_HEADER_VAR), 45) then
  begin
    delete(Call.OutHead, 15, 31);
    insert(XML_CONTENT_TYPE, Call.OutHead, 15);
    JsonBufferToXML(pointer(Call.OutBody), XMLUTF8_HEADER,
      ForceServiceResultAsXMLObjectNameSpace, xml);
    Call.OutBody := xml;
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
    mDelete:
      result := (Table <> nil) and
        (TableIndex in Rights.DELETE) and
        ((TableID > 0) or
         (reUrlEncodedDelete in Rights.AllowRemoteExecute));
  end;
end;

procedure TRestServerUriContext.ExecuteOrmGet;

  procedure ConvertOutBodyAsPlainJson(const FieldsCsv: RawUtf8;
    Options: TJsonSerializerOrmOptions);
  var
    rec: TOrm;
    W: TJsonSerializer;
    bits: TFieldBits;
    withid: boolean;
  begin
    // force plain standard JSON output for AJAX clients
    if (FieldsCsv = '') or
       // handle ID single field only if ID_str is needed
       (IsRowID(pointer(FieldsCsv)) and
        not (jwoID_str in Options)) or
       // we won't handle min()/max() functions
       not TableModelProps.Props.FieldBitsFromCsv(FieldsCsv, bits, withid) then
      exit;
    rec := Table.CreateAndFillPrepare(Call.OutBody);
    try
      W := TableModelProps.Props.CreateJsonWriter(
        TRawByteStringStream.Create, true, FieldsCsv, {knownrows=}0);
      try
        W.CustomOptions := W.CustomOptions + [twoForceJsonStandard]; // regular JSON
        W.OrmOptions := Options; // will do the magic
        rec.AppendFillAsJsonValues(W);
        W.SetText(Call.OutBody);
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
  sqlisselect: boolean;
  resultlist: TOrmTable;
  tableindexes: TIntegerDynArray;
  rec: TOrm;
  opt: TJsonSerializerOrmOptions;
  P: PUtf8Char;
  i, j, L: PtrInt;
  cache: TRestCache;
  blob: PRttiProp;
begin
  case Method of
    mLOCK, mGET:
      begin
        if Table = nil then
        begin
          if Method <> mLOCK then
          begin
            if (Call.InBody = '') and
               (Parameters <> nil) and
               (reUrlEncodedSql in Call.RestAccessRights^.AllowRemoteExecute) then
            begin
              // GET with a sql statement sent in URI, as sql=....
              while not UrlDecodeValue(Parameters, 'sql=', sql, @Parameters) do
                if Parameters = nil then
                  break;
            end
            else
              // GET with a sql statement sent as UTF-8 body (not 100% HTTP compatible)
              sql := Call.InBody;
            if sql <> '' then
            begin
              sqlisselect := IsSelect(pointer(sql), @sqlselect);
              if sqlisselect or
                 (reSql in Call.RestAccessRights^.AllowRemoteExecute) then
              begin
                StaticOrm := nil;
                if sqlisselect then
                begin
                  tableindexes := Server.fModel.GetTableIndexesFromSqlSelect(sql);
                  if tableindexes = nil then
                  begin
                    // check for SELECT without any known table
                    if not (reSqlSelectWithoutTable in
                        Call.RestAccessRights^.AllowRemoteExecute) then
                    begin
                      Call.OutStatus := HTTP_NOTALLOWED;
                      exit;
                    end;
                  end
                  else
                  begin
                    // check for SELECT with one (or several JOINed) tables
                    for i := 0 to high(tableindexes) do
                      if not (tableindexes[i] in Call.RestAccessRights^.GET) then
                      begin
                        Call.OutStatus := HTTP_NOTALLOWED;
                        exit;
                      end;
                    // use the first static table (poorman's JOIN)
                    StaticOrm := TRestOrmServer(Server.fOrmInstance).
                      InternalAdaptSql(tableindexes[0], sql);
                  end;
                end;
                if StaticOrm <> nil then
                begin
                  TableEngine := StaticOrm;
                  Call.OutBody := TableEngine.EngineList(sql);
                end
                else
                  Call.OutBody := TRestOrmServer(Server.fOrmInstance).
                    MainEngineList(sql, false, nil);
                // security note: only first statement is run by EngineList()
                if Call.OutBody <> '' then
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
                  Call.OutStatus := HTTP_SUCCESS;  // 200 OK
                  if not sqlisselect then
                   // needed for fStats.NotifyOrm(Method) below
                    Method := TUriMethod(IdemPCharArray(SqlBegin(pointer(sql)),
                      ['INSERT', 'UPDATE', 'DELETE']) + 2); // -1+2 -> mGET=1
                end;
              end;
            end;
          end;
        end
        else
        // here, Table<>nil and TableIndex in [0..MAX_TABLES-1]
        if not (TableIndex in Call.RestAccessRights^.GET) then
          // rejected from User Access
          Call.OutStatus := HTTP_NOTALLOWED
        else
        begin
          if TableID > 0 then
          begin
            // GET ModelRoot/TableName/TableID[/BlobFieldName] to retrieve one member,
            // with or w/out locking, or a specified blob field content
            if Method = mLOCK then
              // LOCK is to be followed by PUT -> check user
              if not (TableIndex in Call.RestAccessRights^.PUT) then
                Call.OutStatus := HTTP_NOTALLOWED
              else if Server.fModel.Lock(TableIndex, TableID) then
                Method := mGET; // mark successfully locked
            if Method <> mLOCK then
              if UriBlobFieldName <> '' then
              begin
                // GET ModelRoot/TableName/TableID/BlobFieldName: retrieve blob field content
                blob := Table.OrmProps.BlobFieldPropFromRawUtf8(UriBlobFieldName);
                if blob <> nil then
                begin
                  if TableEngine.EngineRetrieveBlob(TableIndex, TableID, blob,
                    RawBlob(Call.OutBody)) then
                  begin
                    Call.OutHead := GetMimeContentTypeHeader(Call.OutBody);
                    Call.OutStatus := HTTP_SUCCESS; // 200 OK
                  end
                  else
                    Call.OutStatus := HTTP_NOTFOUND;
                end;
              end
              else
              begin
                // GET ModelRoot/TableName/TableID: retrieve a member content, JSON encoded
                cache := TRestOrm(Server.fOrmInstance).CacheOrNil;
                Call.OutBody := cache.Retrieve(TableIndex, TableID);
                if Call.OutBody = '' then
                begin
                  // get JSON object '{...}'
                  if StaticOrm <> nil then
                    Call.OutBody := StaticOrm.EngineRetrieve(TableIndex, TableID)
                  else
                    Call.OutBody := TRestOrmServer(Server.fOrmInstance).
                      MainEngineRetrieve(TableIndex, TableID);
                  // cache if expected
                  if cache <> nil then
                    if Call.OutBody = '' then
                      cache.NotifyDeletion(TableIndex, TableID)
                    else
                      cache.Notify(TableIndex, TableID, Call.OutBody, ooSelect);
                end;
                if Call.OutBody <> '' then
                begin
                  // if something was found
                  opt := ClientOrmOptions;
                  if opt <> [] then
                  begin
                    // cached? -> make private
                    rec := Table.CreateFrom(Call.OutBody);
                    try
                      Call.OutBody := rec.GetJsonValues(true, true, ooSelect, nil, opt);
                    finally
                      rec.Free;
                    end;
                  end;
                  Call.OutStatus := HTTP_SUCCESS; // 200 OK
                end
                else
                  Call.OutStatus := HTTP_NOTFOUND;
              end;
          end
          else
          // ModelRoot/TableName with 'select=..&where=' or YUI paging
          if Method <> mLOCK then
          begin
            // Safe.Lock not available here
            sqlselect := 'RowID'; // if no select is specified (i.e. ModelRoot/TableName)
            // all IDs of this table are returned to the client
            sqltotalrowcount := 0;
            if Parameters <> nil then
            begin
              // '?select=...&where=...' or '?where=...'
              sqlstartindex := 0;
              sqlresults := 0;
              if Parameters^ <> #0 then
                with Server.UriPagingParameters do
                begin
                  nonstandardsqlparameter :=
                    Select <> PAGINGPARAMETERS_YAHOO.Select;
                  nonstandardsqlwhereparameter :=
                    Where <> PAGINGPARAMETERS_YAHOO.Where;
                  repeat
                    UrlDecodeValue(Parameters, Sort, sqlsort);
                    UrlDecodeValue(Parameters, Dir, sqldir);
                    UrlDecodeInteger(Parameters, StartIndex, sqlstartindex);
                    UrlDecodeInteger(Parameters, Results, sqlresults);
                    UrlDecodeValue(Parameters, Select, sqlselect);
                    if nonstandardsqlparameter and
                       (sqlselect = '') then
                      UrlDecodeValue(Parameters, PAGINGPARAMETERS_YAHOO.Select, sqlselect);
                    if nonstandardsqlwhereparameter and
                       ({%H-}sqlwhere = '') then
                      UrlDecodeValue(Parameters, PAGINGPARAMETERS_YAHOO.Where, sqlwhere);
                    UrlDecodeValue(Parameters, Server.UriPagingParameters.Where,
                      sqlwhere, @Parameters);
                  until Parameters = nil;
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
              sqlwhere := TrimU(sqlwhere);
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
            Call.OutBody := TRestOrmServer(Server.fOrmInstance).
              InternalListRawUtf8(TableIndex, sql);
            if Call.OutBody <> '' then
            begin
              // got JSON list '[{...}]' ?
              opt := ClientOrmOptions;
              if opt <> [] then
                ConvertOutBodyAsPlainJson(sqlselect, opt);
              Call.OutStatus := HTTP_SUCCESS;  // 200 OK
              if Server.UriPagingParameters.SendTotalRowsCountFmt <> '' then
                // insert "totalRows":% optional value to the JSON output
                if (rsoNoAjaxJson in Server.Options) or
                   (ClientKind = ckFramework) then
                begin
                  // optimized non-expanded mORMot-specific layout
                  P := pointer(Call.OutBody);
                  L := length(Call.OutBody);
                  P := NotExpandedBufferRowCountPos(P, P + L);
                  j := 0;
                  if P <> nil then
                    j := P - pointer(Call.OutBody) - 11
                  else
                    for i := 1 to 10 do
                      if Call.OutBody[L] = '}' then
                      begin
                        j := L;
                        break;
                      end
                      else
                        dec(L);
                  if j > 0 then
                    Insert(FormatUtf8(Server.UriPagingParameters.SendTotalRowsCountFmt,
                      [sqltotalrowcount]), Call.OutBody, j);
                end
                else
                begin
                  // expanded format -> as {"values":[...],"total":n}
                  if sqltotalrowcount = 0 then // avoid sending fields array
                    Call.OutBody := '[]'
                  else
                    Call.OutBody := TrimU(Call.OutBody);
                  Call.OutBody := '{"values":' + Call.OutBody +
                    FormatUtf8(Server.UriPagingParameters.SendTotalRowsCountFmt,
                     [sqltotalrowcount]) + '}';
                end;
            end
            else
              Call.OutStatus := HTTP_NOTFOUND;
          end;
        end;
        if Call.OutStatus = HTTP_SUCCESS then
          Server.fStats.NotifyOrm(Method);
      end;
    mUNLOCK:
      begin
        // ModelRoot/TableName/TableID to unlock a member
        if not (TableIndex in Call.RestAccessRights^.PUT) then
          Call.OutStatus := HTTP_NOTALLOWED
        else if (Table <> nil) and
                (TableID > 0) and
           Server.fModel.UnLock(Table, TableID) then
          Call.OutStatus := HTTP_SUCCESS; // 200 OK
      end;
    mSTATE:
      begin
        // STATE method for TRestClientServerInternalState
        // this method is called with Root (-> Table=nil -> Static=nil)
        // we need a specialized method in order to avoid fStats.Invalid increase
        Call.OutStatus := HTTP_SUCCESS;
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
      Rec.FillFrom(pointer(Call.InBody), @bits);
      Rec.ComputeFieldsBeforeWrite(Server.ORM, Occasion);
      with TableModelProps.Props do
        if Occasion = oeAdd then
          bits := bits + ComputeBeforeAddFieldsBits
        else
          bits := bits + ComputeBeforeUpdateFieldsBits;
      Call.Inbody := Rec.GetJsonValues(true, Rec.IDValue <> 0, bits);
    finally
      Rec.Free;
    end;
  end;

var
  ok: boolean;
  blob: PRttiProp;
  cache: TRestCache;
  orm: TRestOrmServer;
  sqlselect, sqlwhere, sqlsort, sqldir: RawUtf8;
begin
  if MethodIndex = Server.fPublishedMethodBatchIndex then
  begin
    // run the BATCH process in execOrmWrite context
    ExecuteSoaByMethod;
    exit;
  end;
  if not CanExecuteOrmWrite(Method, Table, TableIndex, TableID,
      Call.RestAccessRights^) then
  begin
    Call.OutStatus := HTTP_FORBIDDEN;
    exit;
  end;
  orm := TRestOrmServer(Server.fOrmInstance);
  case Method of
    mPOST:
      // POST=ADD=INSERT
      if Table = nil then
      begin
        // ModelRoot with free SQL statement sent as UTF-8 (only for Admin group)
        // see e.g. TRestClientUri.EngineExecute
        if reSQL in Call.RestAccessRights^.AllowRemoteExecute then
          if (Call.InBody <> '') and
             not (GotoNextNotSpace(Pointer(Call.InBody))^ in [#0, '[', '{']) and
             orm.EngineExecute(Call.InBody) then
            Call.OutStatus := HTTP_SUCCESS // 200 ok
          else
            Call.OutStatus := HTTP_FORBIDDEN;
      end
      else
      begin
        // ModelRoot/TableName with possible JSON SentData: create a new member
        // here, Table<>nil, TableID<0 and TableIndex in [0..MAX_TABLES-1]
        if rsoComputeFieldsBeforeWriteOnServerSide in Server.Options then
          ComputeInBodyFields(oeAdd);
        TableID := TableEngine.EngineAdd(TableIndex, Call.InBody);
        if TableID <> 0 then
        begin
          // 201 Created
          Call.OutStatus := HTTP_CREATED;
          FormatUtf8('Location: %/%', [Uri, TableID], Call.OutHead);
          cache := orm.CacheOrNil;
          if rsoAddUpdateReturnsContent in Server.Options then
          begin
            cache.NotifyDeletion(TableIndex, TableID);
            Call.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            cache.Notify(TableIndex, TableID, Call.OutBody, ooInsert);
          end
          else
            cache.Notify(TableIndex, TableID, Call.InBody, ooInsert);
        end;
      end;
    mPUT:
      // PUT=UPDATE
      if TableID > 0 then
      begin
        // PUT ModelRoot/TableName/TableID[/BlobFieldName] to update member/blob content
        if orm.RecordCanBeUpdated(Table, TableID, oeUpdate, @CustomErrorMsg) then
        begin
          ok := false;
          if UriBlobFieldName <> '' then
          begin
            // PUT ModelRoot/TableName/TableID/BlobFieldName: update blob field content
            blob := Table.OrmProps.BlobFieldPropFromRawUtf8(UriBlobFieldName);
            if blob <> nil then
              ok := TableEngine.EngineUpdateBlob(TableIndex, TableID, blob, Call.InBody);
          end
          else
          begin
            // ModelRoot/TableName/TableID with JSON SentData: update a member
            if rsoComputeFieldsBeforeWriteOnServerSide in Server.Options then
              ComputeInBodyFields(oeUpdate);
            ok := TableEngine.EngineUpdate(TableIndex, TableID, Call.InBody);
            if ok then
            begin
              // flush cache after update (no CreateTime in JSON)
              orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
              if rsoAddUpdateReturnsContent in Server.Options then
                Call.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            end;
          end;
          if ok then
            Call.OutStatus := HTTP_SUCCESS; // 200 ok
        end
        else
          Call.OutStatus := HTTP_FORBIDDEN;
      end
      else if Parameters <> nil then
      begin
        // e.g. from TRestClient.EngineUpdateField
        // PUT ModelRoot/TableName?setname=..&set=..&wherename=..&where=..
        repeat
          UrlDecodeValue(Parameters, 'SETNAME=', sqlselect);
          UrlDecodeValue(Parameters, 'SET=', sqldir);
          UrlDecodeValue(Parameters, 'WHERENAME=', sqlsort);
          UrlDecodeValue(Parameters, 'WHERE=', sqlwhere, @Parameters);
        until Parameters = nil;
        if (sqlselect <> '') and
           (sqldir <> '') and
           (sqlsort <> '') and
           (sqlwhere <> '') then
          if TableEngine.EngineUpdateField(TableIndex, sqlselect, sqldir,
            sqlsort, sqlwhere) then
          begin
            if rsoAddUpdateReturnsContent in Server.Options then
              Call.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            Call.OutStatus := HTTP_SUCCESS; // 200 ok
          end;
      end;
    mDELETE:
      // DELETE
      if TableID > 0 then
        // ModelRoot/TableName/TableID to delete a member
        if not orm.RecordCanBeUpdated(Table, TableID, oeDelete,
            @CustomErrorMsg) then
          Call.OutStatus := HTTP_FORBIDDEN
        else
        begin
          if TableEngine.EngineDelete(TableIndex, TableID) and
             orm.AfterDeleteForceCoherency(TableIndex, TableID) then
          begin
            Call.OutStatus := HTTP_SUCCESS; // 200 ok
            orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
          end;
        end
      else if Parameters <> nil then
      begin
        // ModelRoot/TableName?where=WhereClause to delete members
        repeat
          if UrlDecodeValue(Parameters, 'WHERE=', sqlwhere, @Parameters) then
          begin
            sqlwhere := TrimU(sqlwhere);
            if sqlwhere <> '' then
            begin
              if orm.Delete(Table, sqlwhere) then
                Call.OutStatus := HTTP_SUCCESS; // 200 ok
            end;
            break;
          end;
        until Parameters = nil;
      end;
    mBEGIN:
      begin
      // BEGIN TRANSACTION
      // TOrmVirtualTableJson/External will rely on SQLite3 module
      // and also TRestStorageInMemory, since COMMIT/ROLLBACK have Static=nil
      // mBEGIN logic is just the opposite of mEND/mABORT: Safe.Lock main, then static
        if orm.TransactionBegin(Table, Session) then
        begin
          if (StaticOrm <> nil) and
             (StaticKind = sVirtualTable) then
            StaticOrm.TransactionBegin(Table, Session)
          else if (StaticOrm = nil) and
                  (orm.TransactionTable <> nil) then
          begin
            StaticOrm := orm.StaticVirtualTable[orm.TransactionTable];
            if StaticOrm <> nil then
              StaticOrm.TransactionBegin(Table, Session);
          end;
          Call.OutStatus := HTTP_SUCCESS; // 200 ok
        end;
      end;
    mEND:
      begin
        // END=COMMIT
        // this method is called with Root (-> Table=nil -> StaticOrm=nil)
        // mEND logic is just the opposite of mBEGIN: release StaticOrm, then main
        if (StaticOrm <> nil) and
           (StaticKind = sVirtualTable) then
          StaticOrm.Commit(Session, false)
        else if (StaticOrm = nil) and
                (orm.TransactionTable <> nil) then
        begin
          StaticOrm := orm.StaticVirtualTable[orm.TransactionTable];
          if StaticOrm <> nil then
            StaticOrm.Commit(Session, false);
        end;
        orm.Commit(Session, false);
        Call.OutStatus := HTTP_SUCCESS; // 200 ok
      end;
    mABORT:
      begin
        // ABORT=ROLLBACK
        // this method is called with Root (-> Table=nil -> StaticOrm=nil)
        // mABORT logic is just the opposite of mBEGIN: release StaticOrm, then main
        if (StaticOrm <> nil) and
           (StaticKind = sVirtualTable) then
          StaticOrm.RollBack(Session)
        else if (StaticOrm = nil) and
                (orm.TransactionTable <> nil) then
        begin
          StaticOrm := orm.StaticVirtualTable[orm.TransactionTable];
          if StaticOrm <> nil then
            StaticOrm.RollBack(Session);
        end;
        orm.RollBack(Session);
        Call.OutStatus := HTTP_SUCCESS; // 200 ok
      end;
  end;
  if StatusCodeIsSuccess(Call.OutStatus) then
    Server.fStats.NotifyOrm(Method);
end;

procedure TRestServerUriContext.FillInput(const LogInputIdent: RawUtf8);
var
  n, max: PtrInt;
  P: PUtf8Char;
begin
  if (fInput <> nil) or
     (Parameters = nil) then
    // only do it once
    exit;
  P := Parameters;
  n := 0;
  max := 0;
  repeat
    if n >= max then
    begin
      if n >= 256 then
        // avoid DOS - see MAX_METHOD_ARGS for TInterfacedObjectFake
        raise EParsingException.CreateUtf8(
          'Security Policy: Accept up to 128 parameters for %.FillInput',
          [self]);
      inc(max, 16);
      SetLength(fInput, max);
    end;
    P := UrlDecodeNextNameValue(P, fInput[n], fInput[n + 1]);
    if P = nil then
      break;
    inc(n, 2);
  until P^ = #0;
  if n = 0 then
    fInput := nil
  else
    // don't call SetLength() for a temporary variable, just fake its length
    PDALen(PAnsiChar(fInput) - _DALEN)^ := n - _DAOFF;
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
     (Parameters <> nil) then
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
  int, err: integer;
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
  result := Utf8ToString(fInput[i * 2 + 1]);
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
    result := Utf8ToString(fInput[i * 2 + 1]);
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
  GetVariantFromJson(pointer(v), false, result{%H-});
end;

function TRestServerUriContext.GetInputOrVoid(const ParamName: RawUtf8): variant;
begin
  GetVariantFromJson(pointer(GetInputUtf8OrVoid(ParamName)), false, result{%H-});
end;

function TRestServerUriContext.InputOrError(const ParamName: RawUtf8;
  out Value: variant; const ErrorMessageForMissingParameter: string): boolean;
var
  v: RawUtf8;
begin
  result := InputUtf8OrError(ParamName, v, ErrorMessageForMissingParameter);
  if result then
    GetVariantFromJson(pointer(v), false, Value);
end;

function TRestServerUriContext.GetInputAsTDocVariant(
  const Options: TDocVariantOptions; InterfaceMethod: pointer): variant;
var
  ndx, a: PtrInt;
  forcestring: boolean;
  v: variant;
  multipart: TMultiPartDynArray;
  name: RawUtf8;
  met: PInterfaceMethod absolute InterfaceMethod;
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
      if met <> nil then
      begin
        a := met.ArgIndex(pointer(name), length(name), {input=}true);
        forcestring := (a >= 0) and
                       (vIsString in met.Args[a].ValueKindAsm);
      end
      else
        forcestring := false;
      GetVariantFromJson(pointer(fInput[ndx * 2 + 1]), forcestring, v, @Options);
      res.AddValue(name, v);
    end;
  end
  else if InputAsMultiPart(multipart) then
  begin
    res.Init(Options, dvObject);
    for ndx := 0 to high(multipart) do
      with multipart[ndx] do
        if ContentType = TEXT_CONTENT_TYPE then
        begin
          // append as regular "Name":"TextValue" field
          RawUtf8ToVariant(Content, v);
          res.AddValue(name, v);
        end
        else
          // append binary file as an object, with Base64-encoded data
          res.AddValue(name, _ObjFast([
            'data', BinToBase64(Content),
            'filename', FileName,
            'contenttype', ContentType]));
  end;
end;

function TRestServerUriContext.InputAsMultiPart(
  var MultiPart: TMultiPartDynArray): boolean;
begin
  result := (Method = mPOST) and
     IdemPChar(pointer(fInputPostContentType), 'MULTIPART/FORM-DATA') and
     MultiPartFormDataDecode(fInputPostContentType, Call^.InBody, MultiPart);
end;

function TRestServerUriContext.GetInHeader(const HeaderName: RawUtf8): RawUtf8;
var
  up: array[byte] of AnsiChar;
begin
  if self = nil then
    result := ''
  else if fInHeaderLastName = HeaderName then
    result := fInHeaderLastValue
  else
  begin
    PWord(UpperCopy255(up{%H-}, HeaderName))^ := ord(':');
    FindNameValue(Call.InHead, up, result);
    if result <> '' then
    begin
      fInHeaderLastName := HeaderName;
      fInHeaderLastValue := result;
    end;
  end;
end;

const
  // Deny-Of-Service (DOS) Attack detection threshold
  COOKIE_MAXCOUNT_DOSATTACK = 512;

procedure TRestServerUriContext.RetrieveCookies;
var
  n: PtrInt;
  P: PUtf8Char;
  cookie, cn, cv: RawUtf8;
begin
  fInputCookiesRetrieved := true;
  FindNameValue(Call.InHead, 'COOKIE:', cookie);
  P := pointer(cookie);
  n := 0;
  while P <> nil do
  begin
    GetNextItemTrimed(P, '=', cn);
    GetNextItemTrimed(P, ';', cv);
    if (cn = '') and
       (cv = '') then
      break;
    SetLength(fInputCookies, n + 1);
    fInputCookies[n].Name := cn;
    fInputCookies[n].Value := cv;
    inc(n);
    if n > COOKIE_MAXCOUNT_DOSATTACK then
      raise EParsingException.CreateUtf8(
        '%.RetrieveCookies overflow: DOS?', [self]);
  end;
end;

procedure TRestServerUriContext.SetInCookie(CookieName, CookieValue: RawUtf8);
var
  i, n: PtrInt;
begin
  CookieName := TrimU(CookieName);
  if (self = nil) or
     (CookieName = '') then
    exit;
  if not fInputCookiesRetrieved then
    RetrieveCookies;
  n := length(fInputCookies);
  for i := 0 to n - 1 do
    if fInputCookies[i].Name = CookieName then // cookies are case-sensitive
    begin
      fInputCookies[i].Value := CookieValue; // in-place update
      exit;
    end;
  SetLength(fInputCookies, n + 1);
  fInputCookies[n].Name := CookieName;
  fInputCookies[n].Value := CookieValue;
end;

function TRestServerUriContext.GetInCookie(CookieName: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  result := '';
  CookieName := TrimU(CookieName);
  if (self = nil) or
     (CookieName = '') then
    exit;
  if not fInputCookiesRetrieved then
    RetrieveCookies;
  for i := 0 to length(fInputCookies) - 1 do
    if fInputCookies[i].Name = CookieName then
    begin
      // cookies are case-sensitive
      result := fInputCookies[i].Value;
      exit;
    end;
end;

procedure TRestServerUriContext.SetOutSetCookie(aOutSetCookie: RawUtf8);
const
  HTTPONLY: array[boolean] of RawUtf8 = (
    '; HttpOnly', '');
begin
  if self = nil then
    exit;
  aOutSetCookie := TrimU(aOutSetCookie);
  if not IsValidUtf8WithoutControlChars(aOutSetCookie) then
    raise EParsingException.CreateUtf8('Unsafe %.SetOutSetCookie', [self]);
  if PosExChar('=', aOutSetCookie) < 2 then
    raise EParsingException.CreateUtf8(
      '"name=value" expected for %.SetOutSetCookie("%")', [self, aOutSetCookie]);
  if StrPosI('; PATH=', pointer(aOutSetCookie)) = nil then
    FormatUtf8('%; Path=/%%', [aOutSetCookie, Server.fModel.Root,
      HTTPONLY[rsoCookieHttpOnlyFlagDisable in Server.fOptions]], fOutSetCookie)
  else
    fOutSetCookie := aOutSetCookie;
end;

function TRestServerUriContext.GetUserAgent: RawUtf8;
begin
  result := Call^.HeaderOnce(fUserAgent, 'USER-AGENT: ');
end;

function TRestServerUriContext.GetRemoteIP: RawUtf8;
begin
  result := Call^.HeaderOnce(fRemoteIP, HEADER_REMOTEIP_UPPER);
end;

function TRestServerUriContext.IsRemoteIPBanned: boolean;
begin
  if Server.fIPBan.Exists(GetRemoteIP) then
  begin
    Error('Banned IP %', [fRemoteIP]);
    result := true;
  end
  else
    result := true;
end;

function TRestServerUriContext.GetRemoteIPNotLocal: RawUtf8;
begin
  result := Call^.HeaderOnce(fRemoteIP, HEADER_REMOTEIP_UPPER);
  if result = '127.0.0.1' then
    result := '';
end;

function TRestServerUriContext.GetRemoteIPIsLocalHost: boolean;
begin
  result := (GetRemoteIP = '') or
            (fRemoteIP = '127.0.0.1');
end;

function TRestServerUriContext.AuthenticationBearerToken: RawUtf8;
begin
  result := Call^.HeaderOnce(fAuthenticationBearerToken, HEADER_BEARER_UPPER);
  if (result = '') and
     not (rsoAuthenticationUriDisable in Server.Options) then
  begin
    result := GetInputUtf8OrVoid('authenticationbearer');
    if result <> '' then
      fAuthenticationBearerToken := result;
  end;
end;

function TRestServerUriContext.AuthenticationCheck(jwt: TJwtAbstract): boolean;
begin
  if jwt = nil then
    JwtContent.result := jwtNoToken
  else
    jwt.Verify(AuthenticationBearerToken, JwtContent);
  result := JwtContent.result = jwtValid;
  if not result then
    Error('Invalid Bearer [%]', [ToText(JwtContent.result)^], HTTP_FORBIDDEN)
  else if (Server.fIPWhiteJWT <> nil) and
          not Server.fIPWhiteJWT.Exists(RemoteIP) and
          (fRemoteIP <> '') and
          (fRemoteIP <> '127.0.0.1') then
  begin
    Error('Invalid IP [%]', [fRemoteIP], HTTP_FORBIDDEN);
    result := false;
  end;
end;

function TRestServerUriContext.ClientKind: TRestServerUriContextClientKind;
var
  agent: RawUtf8;
begin
  if fClientKind = ckUnknown then
    if Call.InHead = '' then
      // e.g. for WebSockets remote access
      fClientKind := ckAjax
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

function TRestServerUriContext.ClientOrmOptions: TJsonSerializerOrmOptions;
begin
  result := [];
  if (TableModelProps = nil) or
     (ClientKind <> ckAjax) then
    exit;
  if rsoGetID_str in Server.Options then
    include(result, jwoID_str);
  if ([oftObject, oftBlobDynArray, oftVariant] *
      TableModelProps.Props.HasTypeFields <> []) and
     (rsoGetAsJsonNotAsString in Server.Options) then
    include(result, jwoAsJsonNotAsString);
end;

function TRestServerUriContext.GetResourceFileName: TFileName;
begin
  if (UriBlobFieldName = '') or
     (PosEx('..', UriBlobFieldName) > 0) then
    // for security, disallow .. in the supplied file path
    result := ''
  else
    result := Utf8ToString(StringReplaceAll(UriBlobFieldName, '/', PathDelim));
end;

procedure TRestServerUriContext.Returns(const Result: RawUtf8;
  Status: integer; const CustomHeader: RawUtf8;
  Handle304NotModified, HandleErrorAsRegularResult: boolean;
  CacheControlMaxAge: integer; ServerHash: RawUtf8);
var
  clienthash: RawUtf8;
begin
  if HandleErrorAsRegularResult or StatusCodeIsSuccess(Status) then
  begin
    Call.OutStatus := Status;
    Call.OutBody := Result;
    if CustomHeader <> '' then
      Call.OutHead := CustomHeader
    else if Call.OutHead = '' then
      Call.OutHead := JSON_CONTENT_TYPE_HEADER_VAR;
    if CacheControlMaxAge > 0 then
      Call.OutHead := Call.OutHead + #13#10 +
        'Cache-Control: max-age=' + UInt32ToUtf8(CacheControlMaxAge);
    if Handle304NotModified and
       (Status = HTTP_SUCCESS) and
       (Length(Result) > 64) then
    begin
      FindNameValue(Call.InHead, 'IF-NONE-MATCH: ', clienthash);
      if ServerHash = '' then
        ServerHash := crc32cUtf8ToHex(Result);
      ServerHash := '"' + ServerHash + '"';
      if clienthash <> ServerHash then
        Call.OutHead := Call.OutHead + #13#10 +
          'ETag: ' + ServerHash
      else
      begin
        // save bandwidth by returning "304 Not Modified"
        Call.OutBody := '';
        Call.OutStatus := HTTP_NOTMODIFIED;
      end;
    end;
  end
  else
    Error(Result, Status);
end;

procedure TRestServerUriContext.Returns(Value: TObject; Status: integer;
  Handle304NotModified: boolean; OrmOptions: TJsonSerializerOrmOptions;
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

procedure TRestServerUriContext.ReturnsJson(const Value: variant;
  Status: integer; Handle304NotModified: boolean; Escape: TTextWriterKind;
  MakeHumanReadable: boolean; const CustomHeader: RawUtf8);
var
  json: RawUtf8;
  tmp: TSynTempBuffer;
begin
  VariantSaveJson(Value, Escape, json);
  if MakeHumanReadable and
     (json <> '') and
     (json[1] in ['{', '[']) then
  begin
    tmp.Init(json);
    try
      JsonBufferReformat(tmp.buf, json);
    finally
      tmp.Done;
    end;
  end;
  Returns(json, Status, CustomHeader, Handle304NotModified);
end;

procedure TRestServerUriContext.ReturnBlob(const Blob: RawByteString;
  Status: integer; Handle304NotModified: boolean; const FileName: TFileName;
  CacheControlMaxAge: integer);
begin
  if not ExistsIniName(pointer(Call.OutHead), HEADER_CONTENT_TYPE_UPPER) then
    AddToCsv(GetMimeContentTypeHeader(Blob, FileName), Call.OutHead, #13#10);
  Returns(Blob, Status, Call.OutHead, Handle304NotModified, false, CacheControlMaxAge);
end;

procedure TRestServerUriContext.ReturnFile(const FileName: TFileName;
  Handle304NotModified: boolean; const ContentType: RawUtf8;
  const AttachmentFileName: RawUtf8; const Error404Redirect: RawUtf8;
  CacheControlMaxAge: integer);
var
  filetime: TDateTime;
  clienthash, serverhash: RawUtf8;
begin
  if FileName = '' then
    filetime := 0
  else
    filetime := FileAgeToDateTime(FileName);
  if filetime = 0 then
    if Error404Redirect <> '' then
      Redirect(Error404Redirect)
    else
      Error('', HTTP_NOTFOUND, CacheControlMaxAge)
  else
  begin
    if not ExistsIniName(pointer(Call.OutHead), HEADER_CONTENT_TYPE_UPPER) then
    begin
      if Call.OutHead <> '' then
        Call.OutHead := Call.OutHead + #13#10;
      if ContentType <> '' then
        Call.OutHead := Call.OutHead + HEADER_CONTENT_TYPE + ContentType
      else
        Call.OutHead := Call.OutHead + GetMimeContentTypeHeader('', FileName);
    end;
    if CacheControlMaxAge > 0 then
      Call.OutHead := Call.OutHead + #13#10'Cache-Control: max-age=' +
        UInt32ToUtf8(CacheControlMaxAge);
    Call.OutStatus := HTTP_SUCCESS;
    if Handle304NotModified then
    begin
      FindNameValue(Call.InHead, 'IF-NONE-MATCH:', clienthash);
      serverhash := '"' + DateTimeToIso8601(filetime, false, 'T', true) + '"';
      Call.OutHead := Call.OutHead + #13#10'ETag: ' + serverhash;
      if clienthash = serverhash then
      begin
        Call.OutStatus := HTTP_NOTMODIFIED;
        exit;
      end;
    end;
    // Content-Type: appears twice: 1st to notify static file, 2nd for mime type
    Call.OutHead := STATICFILE_CONTENT_TYPE_HEADER + #13#10 + Call.OutHead;
    StringToUtf8(FileName, Call.OutBody); // body=filename for STATICFILE_CONTENT
    if AttachmentFileName <> '' then
      Call.OutHead := Call.OutHead +
        #13#10'Content-Disposition: attachment; filename="' + AttachmentFileName + '"';
  end;
end;

procedure TRestServerUriContext.ReturnFileFromFolder(
  const FolderName: TFileName; Handle304NotModified: boolean;
  const DefaultFileName: TFileName; const Error404Redirect: RawUtf8;
  CacheControlMaxAge: integer);
var
  fileName: TFileName;
begin
  if UriBlobFieldName = '' then
    fileName := DefaultFileName
  else if PosEx('..', UriBlobFieldName) > 0 then
    fileName := ''
  else
    fileName := Utf8ToString(
      StringReplaceChars(UriBlobFieldName, '/', PathDelim));
  if fileName <> '' then
    fileName := IncludeTrailingPathDelimiter(FolderName) + fileName;
  ReturnFile(fileName, Handle304NotModified, '', '', Error404Redirect,
    CacheControlMaxAge);
end;

procedure TRestServerUriContext.Redirect(const NewLocation: RawUtf8;
  PermanentChange: boolean);
begin
  if PermanentChange then
    Call.OutStatus := HTTP_MOVEDPERMANENTLY
  else
    Call.OutStatus := HTTP_TEMPORARYREDIRECT;
  Call.OutHead := 'Location: ' + NewLocation;
end;

procedure TRestServerUriContext.Returns(const NameValuePairs: array of const;
  Status: integer; Handle304NotModified, HandleErrorAsRegularResult: boolean;
  const CustomHeader: RawUtf8);
begin
  Returns(JsonEncode(NameValuePairs), Status, CustomHeader, Handle304NotModified,
    HandleErrorAsRegularResult);
end;

procedure TRestServerUriContext.Results(const Values: array of const;
  Status: integer; Handle304NotModified: boolean; CacheControlMaxAge: integer);
var
  i, h: PtrInt;
  result: RawUtf8;
  temp: TTextWriterStackBuffer;
begin
  h := high(Values);
  if h < 0 then
    result := '{"result":null}'
  else
    with TJsonSerializer.CreateOwnedStream(temp) do
    try
      AddShort('{"result":');
      if h = 0 then
        // result is one value
        AddJsonEscape(Values[0])
      else
      begin
        // result is one array of values
        Add('[');
        i := 0;
        repeat
          AddJsonEscape(Values[i]);
          if i = h then
            break;
          AddComma;
          inc(i);
        until false;
        Add(']');
      end;
      Add('}');
      SetText(result);
    finally
      Free;
    end;
  Returns(result, Status, '', Handle304NotModified, false, CacheControlMaxAge);
end;

procedure TRestServerUriContext.Success(Status: integer);
begin
  if StatusCodeIsSuccess(Status) then
    Call.OutStatus := Status
  else
    Error('', Status);
end;

procedure TRestServerUriContext.Error(const Format: RawUtf8;
  const Args: array of const; Status, CacheControlMaxAge: integer);
var
  msg: RawUtf8;
begin
  FormatUtf8(Format, Args, msg);
  Error(msg, Status, CacheControlMaxAge);
end;

procedure TRestServerUriContext.Error(E: Exception; const Format: RawUtf8;
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
      Error(FormatUtf8('{"msg":?,"%":%}', [E, exc], [msg], true), Status);
  end;
end;

procedure TRestServerUriContext.Error(const ErrorMessage: RawUtf8;
  Status, CacheControlMaxAge: integer);
var
  msg: RawUtf8;
  temp: TTextWriterStackBuffer;
begin
  Call.OutStatus := Status;
  if StatusCodeIsSuccess(Status) then
  begin
    // not an error
    Call.OutBody := ErrorMessage;
    if CacheControlMaxAge <> 0 then
      // Cache-Control is ignored for errors
      Call.OutHead := 'Cache-Control: max-age=' +
        UInt32ToUtf8(CacheControlMaxAge);
    exit;
  end;
  if ErrorMessage = '' then
    StatusCodeToReason(Status, msg)
  else
    msg := ErrorMessage;
  with TTextWriter.CreateOwnedStream(temp) do
  try
    AddShort('{'#13#10'"errorCode":');
    Add(Call.OutStatus);
    if (msg <> '') and
       (msg[1] = '{') and
       (msg[length(msg)] = '}') then
    begin
      // detect and append the error message as JSON object
      AddShort(','#13#10'"error":'#13#10);
      AddNoJsonEscape(pointer(msg), length(msg));
      AddShorter(#13#10'}');
    end
    else
    begin
      // regular error message as JSON text
      AddShort(','#13#10'"errorText":"');
      AddJsonEscape(pointer(msg));
      AddShorter('"'#13#10'}');
    end;
    SetText(Call.OutBody);
  finally
    Free;
  end;
  Server.InternalLog('%.Error: %', [ClassType, Call.OutBody], sllDebug);
end;



{ ************ TRestServerRoutingJsonRpc/TRestServerRoutingRest Requests Parsing Scheme }

{ TRestServerRoutingRest }

class function TRestServerRoutingRest.ClientRouting: TRestClientRoutingClass;
begin
  result := TRestClientRoutingRest;
end;

procedure TRestServerRoutingRest.UriDecodeSoaByInterface;
var
  i: PtrInt;
  method, clientdrivenid: RawUtf8;
begin
  if (Table = nil) and
     (MethodIndex < 0) and
     (Uri <> '') and
     (Server.Services <> nil) then
  begin
    // check URI as '/Model/Interface.Method[/ClientDrivenID]'
    i := Server.Services.InterfaceMethods.FindHashed(Uri);
    if i >= 0 then // no specific message: it may be a valid request
      with Server.Services.InterfaceMethod[i] do
      begin
        Service := TServiceFactoryServer(InterfaceService);
        ServiceMethodIndex := InterfaceMethodIndex;
        fServiceListInterfaceMethodIndex := i;
        i := ServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT;
        if i >= 0 then
          ServiceMethod := @Service.InterfaceFactory.Methods[i];
        ServiceInstanceID := GetInteger(pointer(UriBlobFieldName));
      end
    else if UriBlobFieldName <> '' then
    begin
      // check URI as '/Model/Interface/Method[/ClientDrivenID]''
      Service := Server.Services[Uri];
      if Service <> nil then
      begin
        // identified as a valid JSON-RPC service
        Split(UriBlobFieldName, '/', method, clientdrivenid);
        ServiceMethodIndex := Service.InterfaceFactory.FindMethodIndex(method);
        if ServiceMethodIndex < 0 then
          Service := nil
        else
        begin
          ServiceMethod := @Service.InterfaceFactory.Methods[ServiceMethodIndex];
          inc(ServiceMethodIndex, SERVICE_PSEUDO_METHOD_COUNT);
          fServiceListInterfaceMethodIndex := -1;
          ServiceInstanceID := GetInteger(pointer(clientdrivenid));
        end;
      end;
    end;
  end;
end;

procedure TRestServerRoutingRest.ExecuteSoaByInterface;
var
  json: RawUtf8;

  procedure DecodeUriParametersIntoJson(const input: TRawUtf8DynArray);
  var
    a, i, ilow: PtrInt;
    WR: TTextWriter;
    argdone: boolean;
    temp: TTextWriterStackBuffer;
  begin
    WR := TJsonSerializer.CreateOwnedStream(temp);
    try // convert URI parameters into the expected ordered json array
      WR.Add('[');
      with PInterfaceMethod(ServiceMethod)^ do
      begin
        ilow := 0;
        for a := ArgsInFirst to ArgsInLast do
          with Args[a] do
            if ValueDirection <> imdOut then
            begin
              argdone := false;
              for i := ilow to high(input) shr 1 do // search argument in URI
                if IdemPropNameU(input[i * 2], @ParamName^[1], ord(ParamName^[0])) then
                begin
                  AddValueJson(WR, input[i * 2 + 1]); // will add "" if needed
                  if i = ilow then
                    inc(ilow); // optimistic in-order search, but allow any order
                  argdone := true;
                  break;
                end;
              if not argdone then
                AddDefaultJson(WR); // allow missing argument (and add ',')
            end;
      end;
      WR.CancelLastComma;
      WR.Add(']');
      WR.SetText(json);
    finally
      WR.Free;
    end;
  end;

var
  par: PUtf8Char;
begin
  // here Ctxt.Service and ServiceMethod(Index) are set
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUtf8(
      '%.ExecuteSoaByInterface invalid call', [self]);
  //  URI as '/Model/Interface.Method[/ClientDrivenID]'
  if Call.InBody <> '' then
    // parameters sent as json array/object (the Delphi/AJAX way) or single blob
    if (ServiceMethod <> nil) and
       PInterfaceMethod(ServiceMethod)^.ArgsInputIsOctetStream and
       not Call.InBodyTypeIsJson then
    begin
      json := BinToBase64(Call.InBody, '["', '"]', false);
      ServiceParameters := pointer(json); // as expected by InternalExecuteSoaByInterface
    end
    else
      ServiceParameters := pointer(Call.InBody)
  else
  begin
    // no body -> try URI-encoded parameters (the HTML way)
    par := Parameters;
    if par <> nil then
    begin
      while par^ = '+' do
        inc(par); // ignore trailing spaces
      if (par^ = '[') or
         IdemPChar(par, '%5B') then
        // as json array (input is e.g. '+%5B...' for ' [...')
        json := UrlDecode(Parameters)
      else
      begin
        // or as a list of parameters (input is 'Param1=Value1&Param2=Value2...')
        FillInput; // fInput[0]='Param1',fInput[1]='Value1',fInput[2]='Param2'...
        if (fInput <> nil) and
           (ServiceMethod <> nil) then
          DecodeUriParametersIntoJson(fInput);
      end;
    end;
    ServiceParameters := pointer({%H-}json);
  end;
  // now Service, ServiceParameters, ServiceMethod(Index) are set
  InternalExecuteSoaByInterface;
end;


{ TRestServerRoutingJsonRpc }

class function TRestServerRoutingJsonRpc.ClientRouting: TRestClientRoutingClass;
begin
  result := TRestClientRoutingJsonRpc;
end;

procedure TRestServerRoutingJsonRpc.UriDecodeSoaByInterface;
begin
  if (Table = nil) and
     (MethodIndex < 0) and
     (Uri <> '') and
     (Server.Services <> nil) then
    // URI sent as '/Model/Interface' for JSON-RPC service
    Service := Server.Services[Uri];
  // ServiceMethodIndex will be retrieved from "method": in body
end;

procedure TRestServerRoutingJsonRpc.ExecuteSoaByInterface;
var
  method: RawUtf8;
  values: array[0..2] of TValuePUtf8Char;
  m: TServiceInternalMethod;
  tmp: TSynTempBuffer;
begin
  // here Ctxt.Service is set (not ServiceMethodIndex yet)
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUtf8(
      '%.ExecuteSoaByInterface invalid call', [self]);
  tmp.Init(Call.Inbody);
  try
    JsonDecode(tmp.buf, ['method', 'params', 'id'], @values, true);
    if values[0].value = nil then // Method name required
      exit;
    values[0].ToUtf8(method);
    ServiceParameters := values[1].value;
    ServiceInstanceID := values[2].ToCardinal; // retrieve "id":ClientDrivenID
    ServiceMethodIndex := Service.InterfaceFactory.FindMethodIndex(method);
    if ServiceMethodIndex >= 0 then
      inc(ServiceMethodIndex, SERVICE_PSEUDO_METHOD_COUNT)
    else
    begin
      for m := low(TServiceInternalMethod) to high(TServiceInternalMethod) do
        if IdemPropNameU(method, SERVICE_PSEUDO_METHOD[m]) then
        begin
          ServiceMethodIndex := ord(m);
          break;
        end;
      if ServiceMethodIndex < 0 then
      begin
        Error('Unknown method');
        exit;
      end;
    end;
    // now Service, ServiceParameters, ServiceMethod(Index) are set
    InternalExecuteSoaByInterface;
    ServiceParameters := nil;
  finally
    tmp.Done; // release temp storage for values[] = Service* fields
  end;
end;



{ ************ TAuthSession for In-Memory User Sessions }

{ TAuthSession }

procedure TAuthSession.ComputeProtectedValues;
begin
  // here User.GroupRights and fPrivateKey should have been set
  fTimeOutShr10 := (QWord(User.GroupRights.SessionTimeout) * (1000 * 60)) shr 10;
  fTimeOutTix := GetTickCount64 shr 10 + fTimeOutShr10;
  fAccessRights := User.GroupRights.SqlAccessRights;
  fPrivateSalt := fID + '+' + fPrivateKey;
  fPrivateSaltHash := crc32(crc32(0, pointer(fPrivateSalt), length(fPrivateSalt)),
    pointer(User.PasswordHashHexa), length(User.PasswordHashHexa));
end;

constructor TAuthSession.Create(aCtxt: TRestServerUriContext; aUser: TAuthUser);
var
  GID: TAuthGroup;
  rnd: THash256;
  blob: RawBlob;
begin
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
      with aCtxt.Server do
      begin
        if fSessionCounter >= cardinal(maxInt) then
          // avoid CONST_AUTHENTICATION_* i.e. IDCardinal=0 (77) or 1 (76)
          fSessionCounter := 100
        else
          inc(fSessionCounter);
        fIDCardinal := fSessionCounter xor 77; // simple obfuscation
        UInt32ToUtf8(fIDCardinal, fID);
      end;
      // set session parameters
      TAesPrng.Main.Fill(@rnd, SizeOf(rnd));
      fPrivateKey := BinToHex(@rnd, SizeOf(rnd));
      if not (rsoGetUserRetrieveNoBlobData in aCtxt.Server.Options) then
      begin
        aCtxt.Server.OrmInstance.RetrieveBlob(aCtxt.Server.fAuthUserClass,
          User.IDValue, 'Data', blob);
        User.Data := blob;
      end;
      if (aCtxt.Call <> nil) and
         (aCtxt.Call.InHead <> '') then
        fSentHeaders := aCtxt.Call.InHead;
      ComputeProtectedValues;
      fRemoteIP := aCtxt.RemoteIP;
      if aCtxt.Log <> nil then
        aCtxt.Log.Log(sllUserAuth, 'New [%] session %/% created at %/% running %',
          [User.GroupRights.Ident, User.LogonName, fIDCardinal, fRemoteIP,
           aCtxt.Call^.LowLevelConnectionID, aCtxt.GetUserAgent], self);
      exit; // create successfull
    end;
    // on error: set GroupRights back to a pseudo TAuthGroup = ID
    User.GroupRights.Free;
    User.GroupRights := GID;
  end;
  raise ESecurityException.CreateUtf8('Invalid %.Create(%,%)', [self, aCtxt, aUser]);
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

procedure TAuthSession.InterfacesSetLength(MethodCount: integer);
begin
  if fInterfaces = nil then
    SetLength(fInterfaces, MethodCount);
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

const
  TAUTHSESSION_MAGIC = 1;

procedure TAuthSession.SaveTo(W: TBufferWriter);
begin
  W.Write1(TAUTHSESSION_MAGIC);
  W.WriteVarUInt32(fIDCardinal);
  W.WriteVarUInt32(fUser.IDValue);
  fUser.GetBinaryValues(W); // User.fGroup is a pointer, but will be overriden
  W.WriteVarUInt32(fUser.GroupRights.IDValue);
  fUser.GroupRights.GetBinaryValues(W);
  W.Write(fPrivateKey);
  W.Write(fSentHeaders);
end; // TODO: persist ORM/SOA stats? -> rather integrate them before saving

constructor TAuthSession.CreateFrom(var Read: TFastReader; Server: TRestServer);
begin
  if Read.NextByte <> TAUTHSESSION_MAGIC then
    raise ESecurityException.CreateUtf8(
      '%.CreateFrom() with invalid format on % %', [self, Server, Server.Model.Root]);
  fIDCardinal := Read.VarUInt32;
  UInt32ToUtf8(fIDCardinal, fID);
  fUser := Server.AuthUserClass.Create;
  fUser.IDValue := Read.VarUInt32;
  fUser.SetBinaryValues(Read); // fUser.fGroup will be overriden by true instance
  fUser.GroupRights := Server.AuthGroupClass.Create;
  fUser.GroupRights.IDValue := Read.VarUInt32;
  fUser.GroupRights.SetBinaryValues(Read);
  Read.VarUtf8(fPrivateKey);
  Read.VarUtf8(fSentHeaders);
  ComputeProtectedValues;
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
  i: PtrInt;
begin
  result := false;
  if fServer.fSessions = nil then
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
  // allow only to delete its own session - ticket [7723fa7ebd]
  if sessid = Ctxt.Session then
    for i := 0 to fServer.fSessions.Count - 1 do
      with TAuthSession(fServer.fSessions.List[i]) do
        if (fIDCardinal = sessid) and
           (fUser.LogonName = uname) then
        begin
          Ctxt.fAuthSession := nil; // avoid GPF
          fServer.SessionDelete(i, Ctxt);
          Ctxt.Success;
          break;
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
    fServer.SessionCreate(User, Ctxt, sess); // call Ctxt.AuthenticationFailed on error
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
    body.AddValue('result', Session.IDCardinal)
  else
    body.AddValue('result', RawUtf8ToVariant(result));
  if data <> '' then
    body.AddValue('data', RawUtf8ToVariant(data));
  if fAlgoName <> '' then
    // match e.g. TRestServerAuthenticationSignedUriAlgo
    body.AddValue('algo', RawUtf8ToVariant(fAlgoName));
  with Session.User do
    body.AddNameValuesToObject([
      'logonid', IDValue,
      'logonname', LogonName,
      'logondisplay', DisplayName,
      'logongroup', GroupRights.IDValue,
      'timeout', GroupRights.SessionTimeout,
      'server', Executable.ProgramName,
      'version', Executable.Version.DetailedOrVoid]);
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
       Ctxt.UriSessionSignaturePos + 18, Ctxt.Session) then
    result := fServer.SessionAccess(Ctxt);
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
  fComputeSignature := TRestClientAuthenticationSignedUri.GetComputeSignature(value);
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
  begin
    Ctxt.Log.Log(sllUserAuth, 'Invalid Timestamp: expected >=%, got %',
      [minticks, Int64(ts)], self);
  end;
  result := nil; // indicates invalid signature
end;

var
  ServerNonceHash: TSha3; // faster than THMAC_SHA256 on small input
  ServerNonceCache: array[boolean] of record
    tix: cardinal;
    res: RawUtf8;
  end;

function CurrentServerNonce(Previous: boolean): RawUtf8;
var
  ticks: cardinal;
  hash: TSha3;
  res: THash256;
begin
  ticks := GetTickCount64 div (60 * 5 * 1000); // 5 minutes resolution
  if Previous then
    dec(ticks);
  with ServerNonceCache[Previous] do
    if (ticks = tix) and
       (res <> '') then  // check for res='' since ticks may be 0 at startup
    begin
      // very efficiently retrieval from cache
      result := res;
      exit;
    end;
  if ServerNonceHash.Algorithm <> SHA3_256 then
  begin
    // first time used: initialize the private secret for this process lifetime
    FillRandom(@res, SizeOf(res) shr 2); // good enough as seed
    hash.Init(SHA3_256);
    hash.Update(@res, SizeOf(res));
    GlobalLock;
    try
      if ServerNonceHash.Algorithm <> SHA3_256 then
        ServerNonceHash := hash;
    finally
      GlobalUnLock;
    end;
  end;
  hash := ServerNonceHash; // thread-safe SHA-3 sponge reuse
  hash.Update(@ticks, SizeOf(ticks));
  hash.Final(res, true);
  result := BinToHexLower(@res, SizeOf(res));
  with ServerNonceCache[Previous] do
  begin
    tix := ticks;
    res := result;
  end;
end;



{ TRestServerAuthenticationDefault }

function TRestServerAuthenticationDefault.Auth(Ctxt: TRestServerUriContext): boolean;
var
  uname, pwd, cltnonce: RawUtf8;
  usr: TAuthUser;
begin
  result := true;
  if AuthSessionRelease(Ctxt) then
    exit;
  uname := Ctxt.InputUtf8OrVoid['UserName'];
  pwd := Ctxt.InputUtf8OrVoid['Password'];
  cltnonce := Ctxt.InputUtf8OrVoid['ClientNonce'];
  if (uname <> '') and
     (length(cltnonce) > 32) then
  begin
    // GET ModelRoot/auth?UserName=...&PassWord=...&ClientNonce=... -> handshaking
    usr := GetUser(Ctxt, uname);
    if usr <> nil then
    try
      // check if match TRestClientUri.SetUser() algorithm
      if CheckPassword(Ctxt, usr, cltnonce, pwd) then
        // setup a new TAuthSession
        SessionCreate(Ctxt, usr)
      else        // will call Ctxt.AuthenticationFailed on error
        Ctxt.AuthenticationFailed(afInvalidPassword);
    finally
      usr.Free;
    end
    else
      Ctxt.AuthenticationFailed(afUnknownUser);
  end
  else if uname <> '' then
    // only UserName=... -> return hexadecimal nonce content valid for 5 minutes
    Ctxt.Results([CurrentServerNonce])
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
      Sha256(fServer.Model.Root + CurrentServerNonce(false) + salt)) or
     // if current nonce failed, tries with previous 5 minutes' nonce
     IdemPropNameU(aPassWord,
      Sha256(fServer.Model.Root + CurrentServerNonce(true)  + salt)));
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
var
  cookie: RawUtf8;
begin
  cookie := Ctxt.InCookie[REST_COOKIE_SESSION];
  if (length(cookie) = 8) and
     HexDisplayToCardinal(pointer(cookie), Ctxt.Session) then
    result := fServer.SessionAccess(Ctxt)
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
  result := inherited RetrieveSession(Ctxt);
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
          // match -> store header in result (locked by fSessions.fSafe.Lock)
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
        fServer.SessionCreate(U, Ctxt, sess); // call Ctxt.AuthenticationFailed on error
        if sess <> nil then
        begin
          // see TRestServerAuthenticationHttpAbstract.ClientSessionSign()
          Ctxt.SetOutSetCookie((REST_COOKIE_SESSION + '=') +
            CardinalToHexLower(sess.IDCardinal));
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
    Ctxt.Call.OutHead := 'WWW-Authenticate: Basic realm="mORMot Server"';
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

function TRestServerAuthenticationSspi.Auth(
  Ctxt: TRestServerUriContext): boolean;
var
  i, ndx: PtrInt;
  username, indataenc: RawUtf8;
  ticks, connectionID: Int64;
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
    FindNameValue(Ctxt.Call.InHead, SECPKGNAMEHTTPAUTHORIZATION, indataenc);
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
  // check for outdated auth context
  fSafe.Lock;
  try
    // thread-safe deletion of deprecated fSspiAuthContext[] pending auths
    ticks := GetTickCount64 - 30000;
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
      if browserauth then
      begin
        Ctxt.Call.OutHead :=
          (SECPKGNAMEHTTPWWWAUTHENTICATE + ' ') + BinToBase64(outdata);
        Ctxt.Call.OutStatus := HTTP_UNAUTHORIZED; // (401)
        StatusCodeToReason(HTTP_UNAUTHORIZED, Ctxt.Call.OutBody);
      end
      else
        Ctxt.Returns(['result', '',
                      'data', BinToBase64(outdata)]);
      exit; // 1st call: send back outdata to the client
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
        fServer.SessionCreate(
          user, Ctxt, session); // call Ctxt.AuthenticationFailed on error
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
      end else
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
  SetLength(fPerTable[true], length(aServer.Model.Tables));
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
  fSafe^.Lock;
  try
    inc(fSuccess);
    if IsOutcomingFile then
      inc(fOutcomingFiles);
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TRestServerMonitor.NotifyOrm(aMethod: TUriMethod);
begin
  fSafe^.Lock;
  try
    case aMethod of
      mGET, mLOCK:
        inc(fRead);
      mPOST:
        inc(fCreated);
      mPUT:
        inc(fUpdated);
      mDELETE:
        inc(fDeleted);
    end;
    Changed;
  finally
    fSafe^.UnLock;
  end;
end;

procedure TRestServerMonitor.NotifyOrmTable(TableIndex, DataSize: integer;
  Write: boolean; const MicroSecondsElapsed: QWord);
const
  RW: array[boolean] of RawUtf8 = (
    '.read', '.write');
var
  st: TSynMonitorWithSize;
begin
  if TableIndex < 0 then
    exit;
  fSafe^.Lock;
  try
    if TableIndex >= length(fPerTable[Write]) then
      // tables may have been added after Create()
      SetLength(fPerTable[Write], TableIndex + 1);
    if fPerTable[Write, TableIndex] = nil then
      fPerTable[Write, TableIndex] := TSynMonitorWithSize.Create(
        fServer.Model.TableProps[TableIndex].Props.SqlTableName + RW[Write]);
    st := fPerTable[Write, TableIndex];
    st.FromExternalMicroSeconds(MicroSecondsElapsed);
    st.AddSize(DataSize);
    if fServer.fStatUsage <> nil then
      fServer.fStatUsage.Modified(st, []);
  finally
    fSafe^.UnLock;
  end;
end;

function TRestServerMonitor.NotifyThreadCount(delta: integer): integer;
begin
  if self = nil then
    result := 0
  else
  begin
    fSafe^.Lock;
    try
      inc(fCurrentThreadCount, delta);
      result := fCurrentThreadCount;
      if delta <> 0 then
        Changed;
    finally
      fSafe^.UnLock;
    end;
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
      fLog.SynLog.Log(sllWarning, '%.LoadDB(%,%) received Process=%, expected %',
        [ClassType, ID, ToText(Gran)^, rec.Process, fProcessID]);
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



{ ************ TInterfacedCallback/TBlockingCallback Classes }


{ TInterfacedCallback }

constructor TInterfacedCallback.Create(aRest: TRest; const aGuid: TGUID);
begin
  inherited Create;
  fRest := aRest;
  fInterface := aGuid;
end;

procedure TInterfacedCallback.CallbackRestUnregister;
var
  obj: pointer; // not IInvokable to avoid unexpected (recursive) Destroy call
begin
  if (fRest <> nil) and
     (fRest.Services <> nil) and
     not IsNullGuid(fInterface) then
    if GetInterface(fInterface, obj) then
    begin
      fRest.Services.CallBackUnRegister(IInvokable(obj));
      dec(fRefCount); // GetInterface() did increase the refcount
      fRest := nil; // notify once
    end;
end;

destructor TInterfacedCallback.Destroy;
begin
  CallbackRestUnregister;
  inherited Destroy;
end;


{ TBlockingCallback }

constructor TBlockingCallback.Create(aTimeOutMs: integer; aRest: TRest;
  const aGuid: TGUID);
begin
  inherited Create(aRest, aGuid);
  fProcess := TBlockingProcess.Create(aTimeOutMs, fSafe);
end;

destructor TBlockingCallback.Destroy;
begin
  FreeAndNil(fProcess);
  inherited Destroy;
end;

procedure TBlockingCallback.CallbackFinished(aRestForLog: TRestOrm;
  aServerUnregister: boolean);
begin
  if fProcess.NotifyFinished then
  begin
    if aRestForLog <> nil then
      aRestForLog.LogClass.Add.Log(sllTrace, self);
    if aServerUnregister then
      CallbackRestUnregister;
  end;
end;

function TBlockingCallback.WaitFor: TBlockingEvent;
begin
  result := fProcess.WaitFor;
end;

function TBlockingCallback.Reset: boolean;
begin
  result := fProcess.Reset;
end;

function TBlockingCallback.GetEvent: TBlockingEvent;
begin
  result := fProcess.Event;
end;



{ ************ TRestServer Abstract REST Server }

{ TRestServer }

constructor TRestServer.Create(aModel: TOrmModel; aHandleUserAuthentication: boolean);
var
  tmp: RawUtf8;
begin
  if aModel = nil then
    raise EOrmException.CreateUtf8('%.Create(Model=nil)', [self]);
  fStatLevels := SERVERDEFAULTMONITORLEVELS;
  fSessions := TSynObjectListLocked.Create; // needed by AuthenticationRegister() below
  fAuthUserClass := TAuthUser;
  fAuthGroupClass := TAuthGroup;
  fModel := aModel; // we need this property ASAP
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
  // + 100 to avoid CONST_AUTHENTICATION_* i.e. IDCardinal=0 (77) or 1 (76)
  fSessionCounter := Random32(maxInt - 200) + 100; // positive 31-bit integer
  // retrieve published methods
  fPublishedMethods.InitSpecific(
    TypeInfo(TRestServerMethods), fPublishedMethod, ptRawUtf8, nil, true);
  ServiceMethodRegisterPublishedMethods('', self);
  fPublishedMethodAuthIndex := ServiceMethodByPassAuthentication('Auth');
  fPublishedMethodTimestampIndex := ServiceMethodByPassAuthentication('Timestamp');
  tmp := 'Batch';
  fPublishedMethodBatchIndex := fPublishedMethods.FindHashed(tmp);
  if (fPublishedMethodBatchIndex < 0) or
     (fPublishedMethodTimestampIndex < 0) then
    raise EOrmException.CreateUtf8('%.Create: missing method!', [self]);
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
  ObjArrayClear(fSessionAuthentication);
  fServer := nil; // for proper refcnt in inherited Destroy
  inherited Destroy; // calls fServices.Free which will update fStats
  FreeAndNil(fJwtForUnauthenticatedRequest);
  FreeAndNil(fStats);
  fSessions.Free;
  fAssociatedServices.Free;
  if GlobalLibraryRequestServer = self then
    GlobalLibraryRequestServer := nil; // unregister
end;

constructor TRestServer.CreateWithOwnModel(
  const Tables: array of TOrmClass; aHandleUserAuthentication: boolean;
  const aRoot: RawUtf8);
var
  model: TOrmModel;
begin
  model := TOrmModel.Create(Tables, aRoot);
  Create(model, aHandleUserAuthentication);
  model.Owner := self;
end;

procedure TRestServer.SetOrmInstance(aORM: TInterfacedObject);
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
  log: ISynLog; // for Enter auto-leave to work with FPC / Delphi 10.4+
begin
  if fSessions = nil then
    // avoid GPF e.g. in case of missing sqlite3-64.dll
    exit;
  log := fLogClass.Enter('Shutdown(%) % CurrentRequestCount=%',
    [aStateFileName, fModel.Root, fStats.AddCurrentRequestCount(0)], self);
  OnNotifyCallback := nil;
  fSessions.Safe.Lock;
  try
    if fShutdownRequested then
      // Shutdown method already called
      exit;
    fShutdownRequested := true; // will be identified by TRestServer.Uri()
  finally
    fSessions.Safe.UnLock;
  end;
  if fStats.CurrentRequestCount > 0 then
  begin
    timeout := GetTickCount64 + 30000; // never wait forever
    repeat
      SleepHiRes(5);
    until (fStats.AddCurrentRequestCount(0) = 0) or
          (GetTickCount64 > timeout);
  end;
  if aStateFileName <> '' then
    SessionsSaveToFile(aStateFileName);
end;

function TRestServer.SleepOrShutdown(MS: integer): boolean;
var
  timeout: Int64;
begin
  result := true;
  timeout := GetTickCount64 + MS;
  repeat
    if fShutdownRequested then
      exit;
    if MS <= 10 then
      SleepHiRes(MS)
    else
      SleepHiRes(1);
    if fShutdownRequested then
      exit;
  until (MS <= 10) or
        (GetTickCount64 >= timeout);
  result := false;
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
  fSessions.Safe.Lock;
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
    fSessions.Safe.UnLock;
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
  fSessions.Safe.Lock;
  try
    for i := 0 to high(fSessionAuthentication) do
      if fSessionAuthentication[i].ClassType = aMethod then
      begin
        ObjArrayDelete(fSessionAuthentication, i);
        fHandleAuthentication := (fSessionAuthentication <> nil);
        break;
      end;
  finally
    fSessions.Safe.UnLock;
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
  fSessions.Safe.Lock;
  try
    ObjArrayClear(fSessionAuthentication);
    fHandleAuthentication := false;
  finally
    fSessions.Safe.UnLock;
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
  tableindex: integer;
  tablename: RawUtf8;
  service: IServiceRecordVersion;
  callback: IServiceRecordVersionCallback;
  retry: integer;
  log: ISynLog;
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
  tableindex: integer;
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

procedure TRestServer.InternalInfo(var info: TDocVariantData);
var
  cpu, mem, free: RawUtf8;
  now: TTimeLogBits;
  m: TSynMonitorMemory;
begin
  // called by root/Timestamp/info REST method
  now.Value := GetServerTimestamp;
  cpu := TSystemUse.Current(false).HistoryText(0, 15, @mem);
  m := TSynMonitorMemory.Create({nospace=}true);
  try
    FormatUtf8('%/%', [m.PhysicalMemoryFree.Text, m.PhysicalMemoryTotal.Text], free);
    info.AddNameValuesToObject([
      'nowutc', now.Text(true, ' '),
      'timestamp', now.Value,
      'exe', Executable.ProgramName,
      'version', Executable.Version.DetailedOrVoid,
      'host', Executable.Host,
      'cpu', cpu,
      {$ifdef OSWINDOWS} 'mem', mem, {$endif OSWINDOWS}
      'memused', KB(m.AllocatedUsed.Bytes),
      'memfree', free,
      'diskfree', GetDiskPartitionsText({nocache=}false, {withfree=}true, {nospace=}true),
      'exception', GetLastExceptions(10)]);
  finally
    m.Free;
  end;
  Stats.Lock;
  try
    info.AddNameValuesToObject([
      'started', Stats.StartDate,
      'clients', Stats.ClientsCurrent,
      'methods', Stats.ServiceMethod,
      'interfaces', Stats.ServiceInterface,
      'total', Stats.TaskCount,
      'time', Stats.TotalTime.Text]);
  finally
    Stats.Unlock;
  end;
  if Assigned(OnInternalInfo) then
    OnInternalInfo(self, info);
end;

procedure TRestServer.InternalStat(Ctxt: TRestServerUriContext; W: TTextWriter);
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

procedure TRestServer.AddStat(Flags: TRestServerAddStats; W: TTextWriter);
const
  READWRITE: array[boolean] of string[9] = (
    '{"read":', '{"write":');
var
  s, i: PtrInt;
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
    fSessions.Safe.Lock;
    try
      for s := 0 to fSessions.Count - 1 do
      begin
        W.WriteObject(fSessions.List[s]);
        W.CancelLastChar('}');
        with TAuthSession(fSessions.List[s]) do
        begin
          W.AddShort(',"methods":[');
          for i := 0 to high(fMethods) do
            if fMethods[i] <> nil then
            begin
              W.Add('{"%":', [fPublishedMethod[i].Name]);
              fMethods[i].ComputeDetailsTo(W);
              W.Add('}', ',');
            end;
          W.CancelLastComma;
          W.AddShort('],"interfaces":[');
          for i := 0 to high(fInterfaces) do
            if fInterfaces[i] <> nil then
            begin
              W.Add('{"%":', [Services.InterfaceMethod[i].InterfaceDotMethodName]);
              fInterfaces[i].ComputeDetailsTo(W);
              W.Add('}', ',');
            end;
          W.CancelLastComma;
          W.AddShorter(']},');
        end;
      end;
    finally
      fSessions.Safe.UnLock;
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
  i: integer;
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
        fServicesRouting := aServicesRouting;
end;

procedure TRestServer.SessionCreate(var User: TAuthUser;
  Ctxt: TRestServerUriContext; out Session: TAuthSession);
var
  i: PtrInt;
begin
  Session := nil;
  if (reOneSessionPerUser in
      Ctxt.Call^.RestAccessRights^.AllowRemoteExecute) and
     (fSessions <> nil) then
    for i := 0 to fSessions.Count - 1 do
      if TAuthSession(fSessions.List[i]).User.IDValue = User.IDValue then
      begin
        with TAuthSession(fSessions.List[i]) do
          InternalLog('User.LogonName=% already connected from %/%',
            [User.LogonName, RemoteIP, Ctxt.Call^.LowLevelConnectionID], sllUserAuth);
        Ctxt.AuthenticationFailed(afSessionAlreadyStartedForThisUser);
        exit; // user already connected
      end;
  Session := fSessionClass.Create(Ctxt, User);
  if Assigned(OnSessionCreate) then
    if OnSessionCreate(self, Session, Ctxt) then
    begin
      // TRUE aborts session creation
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
end;

procedure TRestServer.SessionDelete(aSessionIndex: integer;
  Ctxt: TRestServerUriContext);
var
  sess: TAuthSession;
begin
  if (self <> nil) and
     (cardinal(aSessionIndex) < cardinal(fSessions.Count)) then
  begin
    sess := fSessions.List[aSessionIndex];
    if Services <> nil then
      (Services as TServiceContainerServer).OnCloseSession(sess.IDCardinal);
    if Ctxt = nil then
      InternalLog('Deleted session %:%/%',
        [sess.User.LogonName, sess.IDCardinal, fSessions.Count], sllUserAuth)
    else
      InternalLog('Deleted session %:%/% from %/%',
        [sess.User.LogonName, sess.IDCardinal, fSessions.Count, sess.RemoteIP,
         Ctxt.Call^.LowLevelConnectionID], sllUserAuth);
    if Assigned(OnSessionClosed) then
      OnSessionClosed(self, sess, Ctxt);
    fSessions.Delete(aSessionIndex);
    fStats.ClientDisconnect;
  end;
end;

function TRestServer.SessionDeleteDeprecated: cardinal;
var
  i: PtrInt;
begin
  // caller made fSessions.Safe.Lock
  result := GetTickCount64 shr 10;
  if (self <> nil) and
     (fSessions<>nil) and
     (result <> fSessionsDeprecatedTix) then
  begin
    // it is enough to check for outdated sessions every second
    fSessionsDeprecatedTix := result;
    for i := fSessions.Count - 1 downto 0 do
      if result > TAuthSession(fSessions.List[i]).TimeOutTix then
        SessionDelete(i, nil);
  end;
end;

function TRestServer.SessionAccess(Ctxt: TRestServerUriContext): TAuthSession;
var
  i: integer;
  tix, session: cardinal;
  sessions: ^TAuthSession;
begin
  // caller of RetrieveSession() made fSessions.Safe.Lock
  if (self <> nil) and
     (fSessions <> nil) then
  begin
    // check deprecated sessions every second
    tix := SessionDeleteDeprecated;
    // retrieve session from its ID
    sessions := pointer(fSessions.List);
    session := Ctxt.Session;
    if session > CONST_AUTHENTICATION_NOT_USED then
      for i := 1 to fSessions.Count do
        if sessions^.IDCardinal = session then
        begin
          // found the session
          result := sessions^;
          result.fTimeOutTix := tix + result.TimeoutShr10;
          Ctxt.fAuthSession := result; // for TRestServer internal use
          // make local copy of TAuthSession information
          Ctxt.SessionUser := result.User.IDValue;
          Ctxt.SessionGroup := result.User.GroupRights.IDValue;
          Ctxt.SessionUserName := result.User.LogonName;
          if (result.RemoteIP <> '') and
             (Ctxt.fRemoteIP = '') then
            Ctxt.fRemoteIP := result.RemoteIP;
          Ctxt.fSessionAccessRights := result.fAccessRights;
          Ctxt.Call^.RestAccessRights := @Ctxt.fSessionAccessRights;
          exit;
        end
        else
          inc(sessions);
  end;
  result := nil;
end;

function TRestServer.SessionGetUser(aSessionID: cardinal): TAuthUser;
var
  i: PtrInt;
begin
  result := nil;
  if self = nil then
    exit;
  fSessions.Safe.Lock;
  try
    for i := 0 to fSessions.Count - 1 do
      with TAuthSession(fSessions.List[i]) do
        if IDCardinal = aSessionID then
        begin
          if User <> nil then
          begin
            result := User.CreateCopy as fAuthUserClass;
            result.GroupRights := nil;
          end;
          Break;
        end;
  finally
    fSessions.Safe.UnLock;
  end;
end;

function TRestServer.SessionsAsJson: RawJson;
var
  i: PtrInt;
  W: TJsonSerializer;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if (self = nil) or
     (fSessions.Count = 0) then
    exit;
  W := TJsonSerializer.CreateOwnedStream(temp);
  try
    fSessions.Safe.Lock;
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
      fSessions.Safe.UnLock;
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
  const aPrefix: RawUtf8; aInstance: TObject);
var
  i: PtrInt;
  methods: TPublishedMethodInfoDynArray;
begin
  if aInstance = nil then
    exit;
  if PosExChar('/', aPrefix) > 0 then
    raise EServiceException.CreateUtf8('%.ServiceMethodRegisterPublishedMethods' +
      '("%"): prefix should not contain "/"', [self, aPrefix]);
  for i := 0 to GetPublishedMethods(aInstance, methods) - 1 do
    with methods[i] do
      ServiceMethodRegister(aPrefix + Name, TOnRestServerCallBack(Method));
end;

procedure TRestServer.ServiceMethodRegister(aMethodName: RawUtf8;
  const aEvent: TOnRestServerCallBack; aByPassAuthentication: boolean);
begin
  aMethodName := TrimU(aMethodName);
  if aMethodName = '' then
    raise EServiceException.CreateUtf8('%.ServiceMethodRegister('''')', [self]);
  if not (rsoNoTableURI in fOptions) and
     (Model.GetTableIndex(aMethodName) >= 0) then
    raise EServiceException.CreateUtf8('Published method name %.% ' +
      'conflicts with a Table in the Model!', [self, aMethodName]);
  with PRestServerMethod(fPublishedMethods.AddUniqueName(aMethodName,
    'Duplicated published method name %.%', [self, aMethodName]))^ do
  begin
    Callback := aEvent;
    ByPassAuthentication := aByPassAuthentication;
  end;
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
  const aInterfaces: array of TGUID; aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.Guid2TypeInfo(aInterfaces);
  result := ServiceRegister(aImplementationClass, ti, aInstanceCreation,
    aContractExpected);
end;

function TRestServer.ServiceDefine(aSharedImplementation: TInterfacedObject;
  const aInterfaces: array of TGUID;
  const aContractExpected: RawUtf8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.Guid2TypeInfo(aInterfaces);
  result := ServiceRegister(aSharedImplementation, ti, aContractExpected);
end;

function TRestServer.ServiceDefine(aClient: TRest;
  const aInterfaces: array of TGUID;
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
      fSessions.Safe.Lock;
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
        fSessions.Safe.UnLock;
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
  R.Init(s);
  fSessions.Safe.Lock;
  try
    (fOrmInstance as TRestOrmServer).InternalState := R.VarUInt32;
    if not AuthUserClass.OrmProps.CheckBinaryHeader(R) or
       not AuthGroupClass.OrmProps.CheckBinaryHeader(R) then
      ContentError;
    n := R.VarUInt32;
    fSessions.Clear;
    for i := 1 to n do
    begin
      fSessions.Add(fSessionClass.CreateFrom(R, self));
      fStats.ClientConnect;
    end;
    fSessionCounter := PCardinal(R.P)^;
    inc(PCardinal(R.P));
    if PCardinal(R.P)^ <> MAGIC_SESSION + 1 then
      ContentError;
  finally
    fSessions.Safe.UnLock;
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
      with TServiceFactoryServer(Services.InterfaceList[i].service) do
        if InstanceCreation = sicPerThread then
          InternalInstanceRetrieve(inst, ord(imFree), 0);
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
  // call TRestOrmServer.EndCurrentThread
  inherited OnEndCurrentThread(Sender);
end;

procedure TRestServer.Uri(var Call: TRestUriParams);
const
  COMMANDTEXT: array[TRestServerUriContextCommand] of string[9] = (
    '?', 'Method', 'Interface', 'Read', 'Write');
var
  ctxt: TRestServerUriContext;
  msstart, msstop: Int64;
  elapsed, len: cardinal;
  outcomingfile: boolean;
  log: ISynLog;
begin
  if (fLogFamily <> nil) and
     (sllEnter in fLogFamily.Level) then
    log := fLogClass.Enter('URI % % in=%',
      [Call.Method, Call.Url, KB(Call.InBody)], self);
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
  QueryPerformanceMicroSeconds(msstart);
  fStats.AddCurrentRequestCount(1);
  Call.OutStatus := HTTP_BADREQUEST; // default error code is 400 BAD REQUEST
  ctxt := ServicesRouting.Create(self, Call);
  try
    if log <> nil then
      ctxt.Log := log.Instance;
    if fShutdownRequested then
      ctxt.Error('Server is shutting down', HTTP_UNAVAILABLE)
    else if ctxt.Method = mNone then
      ctxt.Error('Unknown Verb %', [Call.Method])
    else if (fIPBan = nil) or
            not ctxt.IsRemoteIPBanned then
    // 1. decode URI
    if not ctxt.UriDecodeRest then
      ctxt.Error('Invalid Root', HTTP_NOTFOUND)
    else if (RootRedirectGet <> '') and
            (ctxt.Method = mGet) and
            (Call.Url = Model.Root) and
            (Call.InBody = '') then
      ctxt.Redirect(RootRedirectGet)
    else
    begin
      ctxt.UriDecodeSoaByMethod;
      if (ctxt.MethodIndex < 0) and
         (ctxt.Uri <> '') then
        ctxt.UriDecodeSoaByInterface;
      // 2. handle security
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
          ctxt.Redirect(Model.Root + '/auth')
        else
          ctxt.AuthenticationFailed(afRemoteServiceExecutionNotAllowed)
      else if (ctxt.Session <> CONST_AUTHENTICATION_NOT_USED) or
              (fJwtForUnauthenticatedRequest = nil) or
              (ctxt.MethodIndex = fPublishedMethodTimestampIndex) or
              ((llfSecured in Call.LowLevelConnectionFlags) and
               // HTTPS does not authenticate by itself, WebSockets does
               not (llfHttps in Call.LowLevelConnectionFlags)) or
              ctxt.AuthenticationCheck(fJwtForUnauthenticatedRequest) then
      // 3. call appropriate ORM / SOA commands in fAcquireExecution[] context
      try
        if ctxt.MethodIndex >= 0 then
          if ctxt.MethodIndex = fPublishedMethodBatchIndex then
            ctxt.Command := execOrmWrite
          else
            ctxt.Command := execSoaByMethod
        else if ctxt.Service <> nil then
          ctxt.Command := execSoaByInterface
        else if ctxt.Method in [mLOCK, mGET, mUNLOCK, mSTATE, mHEAD] then
          // read methods
          ctxt.Command := execOrmGet
        else
          // write methods (mPOST, mPUT, mDELETE...)
          ctxt.Command := execOrmWrite;
        if not Assigned(OnBeforeUri) or
           OnBeforeUri(ctxt) then
          ctxt.ExecuteCommand;
      except
        on E: Exception do
          if not Assigned(OnErrorUri) or
             OnErrorUri(ctxt, E) then
            if E.ClassType = EInterfaceFactory then
              ctxt.Error(E, '', [], HTTP_NOTACCEPTABLE)
            else
              ctxt.Error(E, '', [], HTTP_SERVERERROR);
      end;
    end;
    // 4. return expected result to the client and update Server statistics
    if StatusCodeIsSuccess(Call.OutStatus) then
    begin
      outcomingfile := false;
      if Call.OutBody <> '' then
      begin
        len := length(Call.OutHead);
        outcomingfile := (len >= 25) and
                         (Call.OutHead[15] = '!') and
          IdemPChar(pointer(Call.OutHead), STATICFILE_CONTENT_TYPE_HEADER_UPPPER);
      end
      else
        // handle Call.OutBody=''
        if (Call.OutStatus = HTTP_SUCCESS) and
           (rsoHttp200WithNoBodyReturns204 in fOptions) then
          Call.OutStatus := HTTP_NOCONTENT;
      fStats.ProcessSuccess(outcomingfile);
    end
    else
    begin
      // OutStatus is an error code
      fStats.ProcessErrorNumber(Call.OutStatus);
      if Call.OutBody = '' then
        // if no custom error message, compute it now as JSON
        ctxt.Error(ctxt.CustomErrorMsg, Call.OutStatus);
    end;
    StatsAddSizeForCall(fStats, Call);
    if (rsoNoInternalState in fOptions) and
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
    if ctxt.OutSetCookie <> '' then
      ctxt.OutHeadFromCookie;
    if not (rsoHttpHeaderCheckDisable in fOptions) and
       IsInvalidHttpHeader(pointer(Call.OutHead), length(Call.OutHead)) then
      ctxt.Error('Unsafe HTTP header rejected [%]',
        [EscapeToShort(Call.OutHead)], HTTP_SERVERERROR);
  finally
    QueryPerformanceMicroSeconds(msstop);
    ctxt.MicroSecondsElapsed :=
      fStats.FromExternalQueryPerformanceCounters(msstop - msstart);
    if log <> nil then
    begin
      if sllServer in fLogFamily.Level then
        log.Log(sllServer, '% % % %/% %=% out=% in %', [ctxt.SessionUserName,
          ctxt.RemoteIPNotLocal, Call.Method, Model.Root, ctxt.Uri,
          COMMANDTEXT[ctxt.Command], Call.OutStatus, KB(Call.OutBody),
          MicroSecToString(ctxt.MicroSecondsElapsed)]);
      if (Call.OutBody <> '') and
         (sllServiceReturn in fLogFamily.Level) then
        if not (optNoLogOutput in ctxt.ServiceExecutionOptions) then
          if IsHTMLContentTypeTextual(pointer(Call.OutHead)) then
            fLogFamily.SynLog.Log(sllServiceReturn, Call.OutBody, self,
              MAX_SIZE_RESPONSE_LOG);
    end;
    if mlTables in StatLevels then
      case ctxt.Command of
        execOrmGet:
          fStats.NotifyOrmTable(ctxt.TableIndex, length(Call.OutBody), false,
            ctxt.MicroSecondsElapsed);
        execOrmWrite:
          fStats.NotifyOrmTable(ctxt.TableIndex, length(Call.InBody), true,
            ctxt.MicroSecondsElapsed);
      end;
    fStats.AddCurrentRequestCount(-1);
    if fStatUsage <> nil then
      fStatUsage.Modified(fStats, []);
    if Assigned(OnAfterUri) then
      try
        OnAfterUri(ctxt);
      except
      end;
    ctxt.Free;
  end;
  if Assigned(OnIdle) then
  begin
    elapsed := GetTickCount64 shr 7; // trigger every 128 ms
    if elapsed <> fOnIdleLastTix then
    begin
      OnIdle(self);
      fOnIdleLastTix := elapsed;
    end;
  end;
end;

procedure TRestServer.Stat(Ctxt: TRestServerUriContext);
var
  W: TTextWriter;
  json, xml, name: RawUtf8;
  temp: TTextWriterStackBuffer;
begin
  W := TJsonSerializer.CreateOwnedStream(temp);
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
       IdemPropNameU(Ctxt.UriBlobFieldName, 'json') then
      json := JsonReformat(json)
    else if IdemPropNameU(Ctxt.UriBlobFieldName, 'xml') then
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
  fSessions.Safe.Lock;
  try
    for i := 0 to length(fSessionAuthentication) - 1 do
      if fSessionAuthentication[i].Auth(Ctxt) then
        // found an authentication, which may be successfull or not
        break;
  finally
    fSessions.Safe.UnLock;
  end;
end;

procedure TRestServer.Timestamp(Ctxt: TRestServerUriContext);

  procedure DoInfo;
  var
    info: TDocVariantData;
  begin
    info.InitFast;
    InternalInfo(info);
    Ctxt.Returns(info.ToJson('', '', jsonHumanReadable));
  end;

begin
  if IdemPropNameU(Ctxt.UriBlobFieldName, 'info') and
     not (rsoTimestampInfoUriDisable in fOptions) then
    DoInfo
  else
    Ctxt.Returns(Int64ToUtf8(GetServerTimestamp),
      HTTP_SUCCESS, TEXT_CONTENT_TYPE_HEADER);
end;

procedure TRestServer.CacheFlush(Ctxt: TRestServerUriContext);
var
  i, count: PtrInt;
  cache: TRestCache;
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
      if Ctxt.UriBlobFieldName = '_callback_' then
        // POST root/cacheflush/_callback_
        // as called from TSqlHttpClientWebsockets.FakeCallbackUnregister
        (Services as TServiceContainerServer).FakeCallbackRelease(Ctxt)
      else if Ctxt.UriBlobFieldName = '_ping_' then
      begin
        // POST root/cacheflush/_ping_
        count := 0;
        if Ctxt.Session > CONST_AUTHENTICATION_NOT_USED then
          for i := 0 to Services.Count - 1 do
            inc(count, TServiceFactoryServer(Services.InterfaceList[i].service).
              RenewSession(Ctxt.Session));
        InternalLog('Renew % authenticated session % from %: count=%',
          [Model.Root, Ctxt.Session, Ctxt.RemoteIPNotLocal, count], sllUserAuth);
        Ctxt.Returns(['count', count]);
      end;
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
  inherited Create;
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

