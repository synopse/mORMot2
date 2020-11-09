/// REpresentation State Tranfer (REST) Types and Classes on Server Side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.server;

{
  *****************************************************************************

   Server-Side REST Process
    - TRestServerURIContext Access to the Server-Side Execution
    - TRestServerRoutingREST/TRestServerRoutingJSON_RPC Requests Parsing Scheme
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

{ ************ TRestServerURIContext Access to the Server-Side Execution }

type
  // some forward class definition
  TAuthSession = class;
  TRestServer = class;
  TRestServerURIContext = class;
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
    Request: TRestServerURIContext;
    /// the thread which launched the request
    // - is set by TRestServer.BeginCurrentThread from multi-thread server
    // handlers - e.g. TSQLite3HttpServer or TRestServerNamedPipeResponse
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

  /// used by TRestServerURIContext.ClientKind to identify the currently
  // connected client
  TRestServerURIContextClientKind = (
    ckUnknown,
    ckFramework,
    ckAJAX);

  /// used to identify the authentication failure reason
  // - as transmitted e.g. by TRestServerURIContext.AuthenticationFailed or
  // TRestServer.OnAuthenticationFailed
  TOnAuthenticationFailedReason = (
    afInvalidSignature,
    afRemoteServiceExecutionNotAllowed,
    afUnknownUser,afInvalidPassword,
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
  // - instantiated by the TRestServer.URI() method using its ServicesRouting
  // property
  // - see TRestServerRoutingREST and TRestServerRoutingJSON_RPC for working inherited
  // classes - NEVER set this abstract TRestServerURIContext class to
  // TRest.ServicesRouting property !
  TRestServerURIContext = class
  protected
    fInput: TRawUTF8DynArray; // even items are parameter names, odd are values
    fInputPostContentType: RawUTF8;
    fInputCookiesRetrieved: boolean;
    fInputCookies: array of record
      Name, Value: RawUTF8; // only computed if InCookie[] is used
    end;
    fInHeaderLastName: RawUTF8;
    fInHeaderLastValue: RawUTF8;
    fOutSetCookie: RawUTF8;
    fUserAgent: RawUTF8;
    fAuthenticationBearerToken: RawUTF8;
    fRemoteIP: RawUTF8;
    fAuthSession: TAuthSession;
    fClientKind: TRestServerURIContextClientKind;
    fSessionAccessRights: TOrmAccessRights; // fSession may be deleted meanwhile
    fServiceListInterfaceMethodIndex: integer;
    function GetInput(const ParamName: RawUTF8): variant;
    function GetInputOrVoid(const ParamName: RawUTF8): variant;
    function GetInputNameIndex(const ParamName: RawUTF8): PtrInt;
    function GetInputExists(const ParamName: RawUTF8): boolean;
    function GetInputInt(const ParamName: RawUTF8): Int64;
    function GetInputDouble(const ParamName: RawUTF8): Double;
    procedure GetInputByName(const ParamName,InputName: RawUTF8;
      var result: RawUTF8);
    function GetInputUTF8(const ParamName: RawUTF8): RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInputString(const ParamName: RawUTF8): string;
    function GetInputIntOrVoid(const ParamName: RawUTF8): Int64;
      {$ifdef HASINLINE}inline;{$endif}
    function GetInputHexaOrVoid(const ParamName: RawUTF8): cardinal;
    function GetInputDoubleOrVoid(const ParamName: RawUTF8): Double;
    function GetInputUTF8OrVoid(const ParamName: RawUTF8): RawUTF8;
    function GetInputStringOrVoid(const ParamName: RawUTF8): string;
    function GetInHeader(const HeaderName: RawUTF8): RawUTF8;
    procedure RetrieveCookies;
    function GetInCookie(CookieName: RawUTF8): RawUTF8;
    procedure SetInCookie(CookieName, CookieValue: RawUTF8);
    function GetUserAgent: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetRemoteIP: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetRemoteIPNotLocal: RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}
    function GetRemoteIPIsLocalHost: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function GetResourceFileName: TFileName;
    procedure SetOutSetCookie(aOutSetCookie: RawUTF8);
    procedure InternalSetTableFromTableIndex(Index: integer); virtual;
    procedure InternalSetTableFromTableName(TableName: PUTF8Char); virtual;
    procedure InternalExecuteSOAByInterface; virtual;
    /// retrieve RESTful URI routing
    // - should set URI, Table,TableIndex,TableRecordProps,TableEngine,
    // ID, URIBlobFieldName and Parameters members
    // - all Table* members will be set via a InternalSetTableFromTableName() call
    // - default implementation expects an URI encoded with
    // 'ModelRoot[/TableName[/TableID][/BlobFieldName]][?param=...]' format
    // - will also set URISessionSignaturePos and URIWithoutSignature members
    // - return FALSE in case of incorrect URI (e.g. does not match Model.Root)
    function URIDecodeREST: boolean; virtual;
    /// retrieve method-based SOA URI routing with optional RESTful mode
    // - should set MethodIndex member
    // - default RESTful implementation expects an URI encoded with
    // 'ModelRoot/MethodName' or 'ModelRoot/TableName[/TableID]/MethodName' formats
    procedure URIDecodeSOAByMethod; virtual;
    /// retrieve interface-based SOA
    // - should set Service member (and possibly InterfaceMethodIndex)
    // - abstract implementation which is to be overridden
    procedure URIDecodeSOAByInterface; virtual; abstract;
    /// process authentication
    // - return FALSE in case of invalid signature, TRUE if authenticated
    function Authenticate: boolean; virtual;
    /// method called in case of authentication failure
    // - the failure origin is stated by the Reason parameter
    // - this default implementation will just set OutStatus := HTTP_FORBIDDEN
    // and call TRestServer.OnAuthenticationFailed event (if any)
    procedure AuthenticationFailed(Reason: TOnAuthenticationFailedReason); virtual;
    /// direct launch of a method-based service
    // - URI() will ensure that MethodIndex>=0 before calling it
    procedure ExecuteSOAByMethod; virtual;
    /// direct launch of an interface-based service
    // - URI() will ensure that Service<>nil before calling it
    // - abstract implementation which is to be overridden
    procedure ExecuteSOAByInterface; virtual; abstract;
    /// handle GET/LOCK/UNLOCK/STATE verbs for ORM/CRUD process
    procedure ExecuteORMGet; virtual;
    /// handle POST/PUT/DELETE/BEGIN/END/ABORT verbs for ORM/CRUD process
    // - execution of this method is protected by a critical section
    procedure ExecuteORMWrite; virtual;
    /// launch the Execute* method  in the execution mode
    // set by Server.AcquireExecutionMode/AcquireExecutionLockedTimeOut
    // - this is the main process point from TRestServer.URI()
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
    Method: TURIMethod;
    /// the URI address, excluding trailing /info and ?par1=.... parameters
    // - can be either the table name (in RESTful protocol), or a service name
    URI: RawUTF8;
    /// same as Call^.URI, but without the &session_signature=... ending
    URIWithoutSignature: RawUTF8;
    /// points inside Call^.URI, after the 'root/' prefix
    URIAfterRoot: PUTF8Char;
    /// the optional Blob field name as specified in URI
    // - e.g. retrieved from "ModelRoot/TableName/TableID/BlobFieldName"
    URIBlobFieldName: RawUTF8;
    /// position of the &session_signature=... text in Call^.url string
    URISessionSignaturePos: integer;
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
    Command: TRestServerURIContextCommand;
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
    ServiceParameters: PUTF8Char;
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
    // - default behavior is to follow Service.ResultAsJSONObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a JSON object, even if Service.ResultAsJSONObject=false: this may be
    // handy when the method is executed from a JavaScript content
    ForceServiceResultAsJSONObject: boolean;
    /// force the interface-based service methods to return a plain JSON object
    // - i.e. '{....}' instead of '{"result":{....}}'
    // - only set if ForceServiceResultAsJSONObject=TRUE and if no ID is about
    // to be returned
    // - could be used e.g. for stateless interaction with a (non mORMot)
    // stateless JSON REST Server
    ForceServiceResultAsJSONObjectWithoutResult: boolean;
    /// force the interface-based service methods to return a XML object
    // - default behavior is to follow Service.ResultAsJSONObject property value
    // (which own default is to return a more convenient JSON array)
    // - if set to TRUE, this execution context will FORCE the method to return
    // a XML object, by setting ForceServiceResultAsJSONObject then converting
    // the resulting JSON object into the corresponding XML via JSONBufferToXML()
    // - TRestServerURIContext.InternalExecuteSOAByInterface will inspect the
    // Accept HTTP header to check if the answer should be XML rather than JSON
    ForceServiceResultAsXMLObject: boolean;
    /// specify a custom name space content when returning a XML object
    // - default behavior is to follow Service.ResultAsXMLObjectNameSpace
    // property (which is void by default)
    // - service may set e.g. XMLUTF8_NAMESPACE, which will append <content ...>
    // </content> around the generated XML data, to avoid validation problems
    // or set a particular XML name space, depending on the application
    ForceServiceResultAsXMLObjectNameSpace: RawUTF8;
    /// URI inlined parameters
    // - use UrlDecodeValue*() functions to retrieve the values
    // - for mPOST requests, will also be filled for following content types:
    // ! application/x-www-form-urlencoded or multipart/form-data
    Parameters: PUTF8Char;
    /// URI inlined parameters position in Call^.url string
    // - use Parameters field to retrieve the values
    ParametersPos: integer;
    /// access to all input/output parameters at TRestServer.URI() level
    // - process should better call Results() or Success() methods to set the
    // appropriate answer or Error() method in case of an error
    // - low-level access to the call parameters can be made via this pointer
    Call: PRestURIParams;
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
    SessionUserName: RawUTF8;
    /// the static instance corresponding to the associated Table (if any)
    StaticOrm: TRestOrm;
    /// the kind of static instance corresponding to the associated Table (if any)
    StaticKind: TRestServerKind;
    /// optional error message which will be transmitted as JSON error (if set)
    // - contains e.g. TOnAuthenticationFailedReason text during
    // TRestServer.OnAuthenticationFailed event call, or the reason of a
    // TRestServer.RecordCanBeUpdated failure
    CustomErrorMsg: RawUTF8;
    /// high-resolution timimg of the execution command, in micro-seconds
    // - only set when TRestServer.URI finished
    MicroSecondsElapsed: QWord;
    /// JWT validation information, as filled by AuthenticationCheck()
    JWTContent: TJWTContent;
    /// associated logging instance for the current thread on the server
    // - you can use it to log some process on the server side
    Log: TSynLog;
    /// initialize the execution context
    // - this method could have been declared as protected, since it should
    // never be called outside the TRestServer.URI() method workflow
    // - should set Call, and Method members
    constructor Create(aServer: TRestServer;
      const aCall: TRestURIParams); virtual;
    /// finalize the execution context
    destructor Destroy; override;
    /// validate mPost/mPut/mDelete action against those access rights
    // - used by TRestServerURIContext.ExecuteORMWrite and
    // TRestServer.EngineBatchSend methods for proper security checks
    function CanExecuteORMWrite(Method: TURIMethod;
      Table: TOrmClass; TableIndex: integer; const TableID: TID;
      const Rights: TOrmAccessRights): boolean;
    /// extract the input parameters from its URI
    // - you should not have to call this method directly, but rather
    // all the InputInt/InputDouble/InputUTF8/InputExists/... properties
    // - may be useful if you want to access directly to InputPairs[] with no
    // prior knowledge of the input parameter names
    // - you can specify a title text to optionally log the input array
    procedure FillInput(const LogInputIdent: RawUTF8 = '');
    /// retrieve one input parameter from its URI name as Int64
    // - raise an EParsingException if the parameter is not found
    property InputInt[const ParamName: RawUTF8]: Int64
      read GetInputInt;
    /// retrieve one input parameter from its URI name as double
    // - raise an EParsingException if the parameter is not found
    property InputDouble[const ParamName: RawUTF8]: double
      read GetInputDouble;
    /// retrieve one input parameter from its URI name as RawUTF8
    // - raise an EParsingException if the parameter is not found
    property InputUTF8[const ParamName: RawUTF8]: RawUTF8
      read GetInputUTF8;
    /// retrieve one input parameter from its URI name as a VCL string
    // - raise an EParsingException if the parameter is not found
    // - prior to Delphi 2009, some Unicode characters may be missing in the
    // returned AnsiString value
    property InputString[const ParamName: RawUTF8]: string
      read GetInputString;
    /// retrieve one input parameter from its URI name as Int64
    // - returns 0 if the parameter is not found
    property InputIntOrVoid[const ParamName: RawUTF8]: Int64
      read GetInputIntOrVoid;
    /// retrieve one hexadecimal input parameter from its URI name as cardinal
    // - returns 0 if the parameter is not found
    property InputHexaOrVoid[const ParamName: RawUTF8]: cardinal
      read GetInputHexaOrVoid;
    /// retrieve one input parameter from its URI name as double
    // - returns 0 if the parameter is not found
    property InputDoubleOrVoid[const ParamName: RawUTF8]: double
      read GetInputDoubleOrVoid;
    /// retrieve one input parameter from its URI name as RawUTF8
    // - returns '' if the parameter is not found
    property InputUTF8OrVoid[const ParamName: RawUTF8]: RawUTF8
      read GetInputUTF8OrVoid;
    /// retrieve one input parameter from its URI name as a VCL string
    // - returns '' if the parameter is not found
    // - prior to Delphi 2009, some Unicode characters may be missing in the
    // returned AnsiString value
    property InputStringOrVoid[const ParamName: RawUTF8]: string
      read GetInputStringOrVoid;
    /// retrieve one input parameter from its URI name as RawUTF8
    // - returns FALSE and call Error(ErrorMessageForMissingParameter) - which
    // may be a resourcestring - if the parameter is not found
    // - returns TRUE and set Value if the parameter is found
    function InputUTF8OrError(const ParamName: RawUTF8; out Value: RawUTF8;
      const ErrorMessageForMissingParameter: string): boolean;
    /// retrieve one input parameter from its URI name as RawUTF8
    // - returns supplied DefaultValue if the parameter is not found
    function InputUTF8OrDefault(const ParamName, DefaultValue: RawUTF8): RawUTF8;
    /// retrieve one input parameter from its URI name as an enumeration
    // - will expect the value to be specified as integer, or as the textual
    // representation of the enumerate, ignoring any optional lowercase prefix
    // as featured by TEnumType.GetEnumNameValue()
    // - returns TRUE and set ValueEnum if the parameter is found and correct
    // - returns FALSE and set ValueEnum to first item (i.e. DefaultEnumOrd) if
    // the parameter is not found, or not containing a correct value
    function InputEnum(const ParamName: RawUTF8; EnumType: PRttiInfo;
      out ValueEnum; DefaultEnumOrd: integer = 0): boolean;
    /// return TRUE if the input parameter is available at URI
    // - even if InputUTF8['param']='', there may be '..?param=&another=2'
    property InputExists[const ParamName: RawUTF8]: boolean
      read GetInputExists;
    /// retrieve one input parameter from its URI name as variant
    // - if the parameter value is text, it is stored in the variant as
    // a generic VCL string content: so before Delphi 2009, you may loose
    // some characters at decoding from UTF-8 input buffer
    // - raise an EParsingException if the parameter is not found
    property Input[const ParamName: RawUTF8]: variant
      read GetInput; default;
    /// retrieve one input parameter from its URI name as variant
    // - if the parameter value is text, it is stored in the variant as
    // a RawUTF8: so before Delphi 2009, you won't loose any Unicode character,
    // but you should convert its value to AnsiString using UTF8ToString()
    // - returns Unassigned if the parameter is not found
    property InputOrVoid[const ParamName: RawUTF8]: variant
      read GetInputOrVoid;
    /// retrieve one input parameter from its URI name as variant
    // - returns FALSE and call Error(ErrorMessageForMissingParameter) - which
    // may be a resourcestring - if the parameter is not found
    // - returns TRUE and set Value if the parameter is found
    // - if the parameter value is text, it is stored in the variant as
    // a RawUTF8: so before Delphi 2009, you won't loose any Unicode character,
    // but you should convert its value to AnsiString using UTF8ToString()
    function InputOrError(const ParamName: RawUTF8; out Value: variant;
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
    property InputPairs: TRawUTF8DynArray
      read FInput;
    /// retrieve an incoming HTTP header
    // - the supplied header name is case-insensitive
    // - but rather call RemoteIP or UserAgent properties instead of
    // InHeader['remoteip'] or InHeader['User-Agent']
    property InHeader[const HeaderName: RawUTF8]: RawUTF8
      read GetInHeader;
    /// retrieve an incoming HTTP cookie value
    // - cookie name are case-sensitive
    property InCookie[CookieName: RawUTF8]: RawUTF8
      read GetInCookie write SetInCookie;
    /// define a new 'name=value' cookie to be returned to the client
    // - if not void, TRestServer.URI() will define a new 'set-cookie: ...'
    // header in Call^.OutHead
    // - you can use COOKIE_EXPIRED as value to delete a cookie in the browser
    // - if no Path=/.. is included, it will append
    // $ '; Path=/'+Server.Model.Root+'; HttpOnly'
    property OutSetCookie: RawUTF8
      read fOutSetCookie write SetOutSetCookie;
    /// retrieve the "User-Agent" value from the incoming HTTP headers
    property UserAgent: RawUTF8
      read GetUserAgent;
    /// retrieve the "RemoteIP" value from the incoming HTTP headers
    property RemoteIP: RawUTF8
      read GetRemoteIP;
    /// true if the "RemoteIP" value from the incoming HTTP headers is '127.0.0.1'
    property RemoteIPIsLocalHost: boolean
      read GetRemoteIPIsLocalHost;
    /// "RemoteIP" value from the incoming HTTP headers but '' for '127.0.0.1'
    property RemoteIPNotLocal: RawUTF8
      read GetRemoteIPNotLocal;
    /// retrieve the "Authorization: Bearer <token>" value from incoming HTTP headers
    // - typically returns a JWT for statelesss self-contained authentication,
    // as expected by TJWTAbstract.Verify method
    // - as an alternative, a non-standard and slightly less safe way of
    // token transmission may be to encode its value as ?authenticationbearer=....
    // URI parameter (may be convenient when embedding resources in HTML DOM)
    function AuthenticationBearerToken: RawUTF8;
    /// validate "Authorization: Bearer <JWT>" content from incoming HTTP headers
    // - returns true on success, storing the payload in the JWTContent field
    // - set JWTContent.result = jwtNoToken if jwt is nil
    // - on failure (i.e. returns false), will set the error context as
    // 403 HTTP_FORBIDDEN so that you may directly write:
    // ! procedure TMyDaemon.Files(Ctxt: TRestServerURIContext);
    // ! begin
    // !   if Ctxt.AuthenticationCheck(fJWT) then
    // !     Ctxt.ReturnFileFromFolder('c:\datafolder');
    // ! end;
    function AuthenticationCheck(jwt: TJWTAbstract): boolean; virtual;
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
    // - will be used e.g. by ClienTOrmOptions to check if the
    // current remote client expects standard JSON in all cases
    function ClientKind: TRestServerURIContextClientKind;
    /// identify if the request is about a Table containing nested objects or
    // arrays, which could be serialized as JSON objects or arrays, instead
    // of plain JSON string (as stored in the database)
    // - will idenfity ClientKind=ckAjax, or check for rsoGetAsJsonNotAsString
    // in TRestServer.Options
    function ClienTOrmOptions: TJSONSerializerOrmOptions;
    /// true if called from TRestServer.AdministrationExecute
    function IsRemoteAdministrationExecute: boolean;
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
    procedure Returns(const result: RawUTF8; Status: integer = HTTP_SUCCESS;
      const CustomHeader: RawUTF8 = ''; Handle304NotModified: boolean = false;
      HandleErrorAsRegularResult: boolean = false; CacheControlMaxAge: integer = 0;
      ServerHash: RawUTF8 = ''); overload;
    /// use this method to send back a JSON object to the caller
    // - this method will encode the supplied values e.g. as
    // ! JSONEncode(['name','John','year',1972]) = '{"name":"John","year":1972}'
    // - implementation is just a wrapper around Returns(JSONEncode([]))
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    // - expects Status to be either HTTP_SUCCESS or HTTP_CREATED
    // - caller can set Handle304NotModified=TRUE for Status=HTTP_SUCCESS
    procedure Returns(const NameValuePairs: array of const;
      Status: integer = HTTP_SUCCESS; Handle304NotModified: boolean = false;
      HandleErrorAsRegularResult: boolean = false;
      const CustomHeader: RawUTF8 = ''); overload;
    /// use this method to send back any object as JSON document to the caller
    // - this method will call ObjectToJson() to compute the returned content
    // - you can customize OrmOptions, to force the returned JSON
    // object to have its TOrm nested fields serialized as true JSON
    // arrays or objects, or add an "ID_str" string field for JavaScript
    procedure Returns(Value: TObject; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = false;
      OrmOptions: TJSONSerializerOrmOptions = [];
      const CustomHeader: RawUTF8 = ''); overload;
    /// use this method to send back any variant as JSON to the caller
    // - this method will call VariantSaveJSON() to compute the returned content
    procedure ReturnsJson(const Value: variant; Status: integer = HTTP_SUCCESS;
      Handle304NotModified: boolean = false; Escape: TTextWriterKind = twJSONEscape;
      MakeHumanReadable: boolean = false; const CustomHeader: RawUTF8 = '');
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
      Handle304NotModified: boolean = false; const ContentType: RawUTF8 = '';
      const AttachmentFileName: RawUTF8 = ''; const Error404Redirect: RawUTF8 = '';
      CacheControlMaxAge: integer = 0);
    /// use this method to send back a file from a local folder to the caller
    // - URIBlobFieldName value, as parsed from the URI, will containn the
    // expected file name in the local folder, using DefaultFileName if the
    // URI is void, and redirecting to Error404Redirect if the file is not found
    // - this method will let the HTTP server return the file content
    // - if Handle304NotModified is TRUE, will check the file age to ensure
    // that the file content will be sent back to the server only if it changed
    // set CacheControlMaxAge<>0 to include a Cache-Control: max-age=xxx header
    procedure ReturnFileFromFolder(const FolderName: TFileName;
      Handle304NotModified: boolean = true;
      const DefaultFileName: TFileName = 'index.html';
      const Error404Redirect: RawUTF8 = ''; CacheControlMaxAge: integer = 0);
    /// use this method notify the caller that the resource URI has changed
    // - returns a HTTP_TEMPORARYREDIRECT status with the specified location,
    // or HTTP_MOVEDPERMANENTLY if PermanentChange is TRUE
    procedure Redirect(const NewLocation: RawUTF8;
      PermanentChange: boolean = false);
    /// use this method to send back a JSON object with a "result" field
    // - this method will encode the supplied values as a {"result":"...}
    // JSON object, as such for one value:
    // $ {"result":"OneValue"}
    // (with one value, you can just call TRestClientURI.CallBackGetResult
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
    procedure Error(const ErrorMessage: RawUTF8 = '';
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAge: integer = 0); overload; virtual;
    /// use this method to send back an error to the caller
    // - implementation is just a wrapper over Error(FormatUTF8(Format,Args))
    procedure Error(const Format: RawUTF8; const Args: array of const;
      Status: integer = HTTP_BADREQUEST;
      CacheControlMaxAge: integer = 0); overload;
    /// use this method to send back an error to the caller
    // - will serialize the supplied exception, with an optional error message
    procedure Error(E: Exception; const Format: RawUTF8;
      const Args: array of const; Status: integer = HTTP_BADREQUEST); overload;
    /// implements a method-based service for live update of some settings
    // - should be called from a method-based service, e.g. Configuration()
    // - the settings are expected to be stored e.g. in a TSynAutoCreateFields
    // instance, potentially with nested objects
    // - accept the following REST methods to read and write the settings:
    // ! GET http://server:888/root/configuration
    // ! GET http://server:888/root/configuration/propname
    // ! GET http://server:888/root/configuration/propname?value=propvalue
    // - could be used e.g. as such:
    // ! procedure TMyRestServerMethods.Configuration(Ctxt: TRestServerURIContext);
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
    /// event raised by ExecuteMethod() for interface parameters
    // - match TInterfaceMethodInternalExecuteCallback signature
    procedure ExecuteCallback(var Par: PUTF8Char; ParamInterfaceInfo: TRttiJson;
      out Obj);
  end;

  /// method prototype to be used on Server-Side for method-based services
  // - will be routed as ModelRoot/[TableName/TableID/]MethodName RESTful requests
  // - this mechanism is able to handle some custom Client/Server request, similar
  // to the DataSnap technology, but in a KISS way; it's fully integrated in the
  // Client/Server architecture of our framework
  // - just add a published method of this type to any TRestServer descendant
  // - when TRestServer.URI receive a request for ModelRoot/MethodName
  // or ModelRoot/TableName/TableID/MethodName, it will check for a published method
  // in its self instance named MethodName which MUST be of TOnRestServerCallBack
  // type (not checked neither at compile time neither at runtime: beware!) and
  // call it to handle the request
  // - important warning: the method implementation MUST be thread-safe
  // - when TRestServer.URI receive a request for ModelRoot/MethodName,
  // it calls the corresponding published method with aRecord set to nil
  // - when TRestServer.URI receive a request for ModelRoot/TableName/TableID/MethodName,
  // it calls the corresponding published method with aRecord pointing to a
  // just created instance of the corresponding class,  with its field ID set;
  // note that the only set field is ID: other fields of aRecord are not set, but
  // must secificaly be retrieved on purpose
  // - for ModelRoot/TableName/TableID/MethodName, the just created instance will
  // be freed by TRestServer.URI when the method returns
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
  // ! procedure TRestServerTest.Sum(Ctxt: TRestServerURIContext);
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
  // !     // same as: Ctxt.Returns(JSONEncode(['result', a + b]));
  // !     // same as: Ctxt.Returns(['result', a + b]);
  // !   end
  // !   else
  // !     Ctxt.Error('Missing Parameter');
  // ! end;
  // - Client-Side can be implemented as you wish. By convention, it could be
  // appropriate to define in either TRestServer (if to be called as
  // ModelRoot/MethodName), either TOrm (if to be called as
  // ModelRoot/TableName[/TableID]/MethodName) a custom public or protected method,
  // calling TRestClientURI.URL with the appropriate parameters, and named
  // (by convention) as MethodName; TRestClientURI has dedicated methods
  // like CallBackGetResult, CallBackGet, CallBackPut and CallBack; see also
  // TOrmModel.getURICallBack and JSONDecode function
  // ! function TOrmPeople.Sum(aClient: TRestClientURI; a, b: double): double;
  // ! var err: integer;
  // ! begin
  // !   val(aClient.CallBackGetResult('sum', ['a', a, 'b', b]), result, err);
  // ! end;
  TOnRestServerCallBack = procedure(Ctxt: TRestServerURIContext) of object;

  /// description of a method-based service
  TRestServerMethod = record
    /// the method name
    Name: RawUTF8;
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


{ ************ TRestServerRoutingREST/TRestServerRoutingJSON_RPC Requests Parsing Scheme }

  /// calling context for a TOnRestServerCallBack using simple REST for
  // interface-based services
  // - match TRestClientRoutingREST reciprocal class
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
  TRestServerRoutingREST = class(TRestServerURIContext)
  protected
    /// retrieve interface-based SOA with URI RESTful routing
    // - should set Service member (and possibly ServiceMethodIndex)
    // - this overridden implementation expects an URI encoded with
    // '/Model/Interface.Method[/ClientDrivenID]' for this class, and
    // will set ServiceMethodIndex for next ExecuteSOAByInterface method call
    procedure URIDecodeSOAByInterface; override;
    /// direct launch of an interface-based service with URI RESTful routing
    // - this overridden implementation expects parameters to be sent as one JSON
    // array body (Delphi/AJAX way) or optionally with URI decoding (HTML way):
    // ! function TServiceCalculator.Add(n1, n2: integer): integer;
    // will accept such requests:
    // !  URL='root/Calculator.Add' and InBody='[ 1,2 ]'
    // !  URL='root/Calculator.Add?+%5B+1%2C2+%5D' // decoded as ' [ 1,2 ]'
    // !  URL='root/Calculator.Add?n1=1&n2=2'      // in any order, even missing
    procedure ExecuteSOAByInterface; override;
  end;

  /// calling context for a TOnRestServerCallBack using JSON/RPC for
  // interface-based services
  // - match TRestClientRoutingJSON_RPC reciprocal class
  // - in this routing scheme, the URI will define the interface, then the
  // method name will be inlined with parameters, e.g.
  // $ POST /root/Calculator
  // $ (...)
  // $ {"method":"Add","params":[1,2]}
  // or, for a sicClientDriven mode service:
  // $ POST /root/ComplexNumber
  // $ (...)
  // $ {"method":"Add","params":[20,30],"id":1234}
  TRestServerRoutingJSON_RPC = class(TRestServerURIContext)
  protected
    /// retrieve interface-based SOA with URI JSON/RPC routing
    // - this overridden implementation expects an URI encoded with
    // '/Model/Interface' as for the JSON/RPC routing scheme, and won't
    // set ServiceMethodIndex at this level (but in ExecuteSOAByInterface)
    procedure URIDecodeSOAByInterface; override;
    /// direct launch of an interface-based service with URI JSON/RPC routing
    // - URI() will ensure that Service<>nil before calling it
    // - this overridden implementation expects parameters to be sent as part
    // of a JSON object body:
    // $ {"method":"Add","params":[20,30],"id":1234}
    procedure ExecuteSOAByInterface; override;
  end;

  /// class used to define the Server side expected routing
  // - match TRestClientRoutingClass reciprocal meta-class
  // - most of the internal methods are declared as virtual, so it allows any
  // kind of custom routing or execution scheme
  // - TRestRoutingREST and TRestRoutingJSON_RPC classes
  // are provided in this unit, to allow RESTful and JSON/RPC protocols
  TRestServerURIContextClass = class of TRestServerURIContext;



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
    fID: RawUTF8;
    fIDCardinal: cardinal;
    fTimeOutTix: cardinal;
    fTimeOutShr10: cardinal;
    fPrivateKey: RawUTF8;
    fPrivateSalt: RawUTF8;
    fSentHeaders: RawUTF8;
    fRemoteIP: RawUTF8;
    fPrivateSaltHash: Cardinal;
    fLastTimestamp: Cardinal;
    fExpectedHttpAuthentication: RawUTF8;
    fAccessRights: TOrmAccessRights;
    fMethods: TSynMonitorInputOutputObjArray;
    fInterfaces: TSynMonitorInputOutputObjArray;
    function GetUserName: RawUTF8;
    function GetUserID: TID;
    function GetGroupID: TID;
    procedure SaveTo(W: TBufferWriter); virtual;
    procedure ComputeProtectedValues; virtual;
  public
    /// initialize a session instance with the supplied TAuthUser instance
    // - this aUser instance will be handled by the class until Destroy
    // - raise an exception on any error
    // - on success, will also retrieve the aUser.Data BLOB field content
    constructor Create(aCtxt: TRestServerURIContext;
      aUser: TAuthUser); reintroduce; virtual;
    /// initialize a session instance from some persisted buffer
    // - following the TRestServer.SessionsSaveToFile binary layout
    constructor CreateFrom(var P: PAnsiChar; PEnd: PAnsiChar;
      Server: TRestServer); virtual;
    /// will release the User and User.GroupRights instances
    destructor Destroy; override;
    /// initialize the Interfaces: TSynMonitorInputOutputObjArray statistics
    procedure InterfacesSetLength(MethodCount: integer);
  public
    /// the session ID number, as numerical value
    // - never equals to 1 (CONST_AUTHENTICATION_NOT_USED, i.e. authentication
    // mode is not enabled), nor 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED,
    // i.e. session still in handshaking phase)
    property IDCardinal: cardinal read fIDCardinal;
    /// the associated User
    // - this is a true TAuthUser instance, and User.GroupRights will contain
    // also a true TAuthGroup instance
    property User: TAuthUser read fUser;
    /// set by the Access() method to the current GetTickCount64 shr 10
    // timestamp + TimeoutSecs
    property TimeOutTix: cardinal read fTimeOutTix;
    /// copy of the associated user access rights
    // - extracted from User.TAuthGroup.SQLAccessRights
    property AccessRights: TOrmAccessRights read fAccessRights;
    /// the hexadecimal private key as returned to the connected client
    // as 'SessionID+PrivateKey'
    property PrivateKey: RawUTF8 read fPrivateKey;
    /// the transmitted HTTP headers, if any
    // - can contain e.g. 'RemoteIp: 127.0.0.1' or 'User-Agent: Mozilla/4.0'
    property SentHeaders: RawUTF8 read fSentHeaders;
    /// per-session statistics about method-based services
    // - Methods[] follows TRestServer.fPublishedMethod[] array
    // - is initialized and maintained only if mlSessions is defined in
    // TRestServer.StatLevels property
    property Methods: TSynMonitorInputOutputObjArray read fMethods;
    /// per-session statistics about interface-based services
    // - Interfaces[] follows TRestServer.Services.fListInterfaceMethod[] array
    // - is initialized and maintained only if mlSessions is defined in
    // TRestServer.StatLevels property
    property Interfaces: TSynMonitorInputOutputObjArray
      read fInterfaces write fInterfaces;
  published
    /// the session ID number, as text
    property ID: RawUTF8 read fID;
    /// the associated User Name, as in User.LogonName
    property UserName: RawUTF8 read GetUserName;
    /// the associated User ID, as in User.ID
    property UserID: TID read GetUserID;
    /// the associated Group ID, as in User.GroupRights.ID
    property GroupID: TID read GetGroupID;
    /// the timestamp (in numbers of 1024 ms) until a session is kept alive
    // - extracted from User.TAuthGroup.SessionTimeout
    // - is used for fast comparison with GetTickCount64 shr 10
    property TimeoutShr10: cardinal read fTimeOutShr10;
    /// the remote IP, if any
    // - is extracted from SentHeaders properties
    property RemoteIP: RawUTF8 read fRemoteIP;
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
  TRestServerAuthentication = class
  protected
    fServer: TRestServer;
    fOptions: TRestServerAuthenticationOptions;
    fAlgoName: RawUTF8;
    // GET ModelRoot/auth?UserName=...&Session=... -> release session
    function AuthSessionRelease(Ctxt: TRestServerURIContext): boolean;
    /// retrieve an User instance from its logon name
    // - should return nil if not found
    // - this default implementation will retrieve it from ORM, and
    // call TAuthUser.CanUserLog() to ensure authentication is allowed
    // - if aUserName is an integer, it will try to retrieve it from ORM using
    // the supplied value as its TAuthUser.ID: it may be convenient when the
    // client is not an end-user application but a mORMot server (in a cloud
    // architecture), since it will benefit from local ORM cache
    // - you can override this method and return an on-the-fly created value
    // as a TRestServer.SQLAuthUserClass instance (i.e. not persisted
    // in database nor retrieved by ORM), but the resulting TAuthUser
    // must have its ID and LogonName properties set with unique values (which
    // will be used to identify it for a later call and session owner
    // identification), and its GroupRights property must not yet contain a real
    // TAuthGroup instance, just a TAuthGroup(aGroupID) value (as directly
    // retrieved from the ORM) - TAuthSession.Create will retrieve the instance
    // - another possibility, orthogonal to all TRestServerAuthentication
    // classes, may be to define a TRestServer.OnAuthenticationUserRetrieve
    // custom event
    function GetUser(Ctxt: TRestServerURIContext;
      const aUserName: RawUTF8): TAuthUser; virtual;
    /// create a session on the server for a given user
    // - this default implementation will call fServer.SessionCreate() and
    // return a '{"result":"HEXASALT","logonname":"UserName"}' JSON content
    // and will always call User.Free
    // - on failure, will call TRestServerURIContext.AuthenticationFailed()
    // with afSessionAlreadyStartedForThisUser or afSessionCreationAborted reason
    procedure SessionCreate(Ctxt: TRestServerURIContext;
      var User: TAuthUser); virtual;
    /// Ctxt.Returns(['result',result,....[,'data',data]],200,header);
    procedure SessionCreateReturns(Ctxt: TRestServerURIContext;
      Session: TAuthSession; const result, data, header: RawUTF8);
  public
    /// initialize the authentication method to a specified server
    // - you can define several authentication schemes for the same server
    constructor Create(aServer: TRestServer); virtual;
    /// called by the Server to implement the Auth RESTful method
    // - overridden method shall return TRUE if the request has been handled
    // - returns FALSE to let the next registered TRestServerAuthentication
    // class to try implementing the content
    // - Ctxt.Parameters has been tested to contain an UserName=... value
    // - method execution is protected by TRestServer.fSessions.Lock
    function Auth(Ctxt: TRestServerURIContext): boolean; virtual; abstract;
    /// called by the Server to check if the execution context match a session
    // - returns a session instance corresponding to the remote request, and
    // fill Ctxt.Session* members according to in-memory session information
    // - returns nil if this remote request does not match this authentication
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(
      Ctxt: TRestServerURIContext): TAuthSession; virtual; abstract;
    /// allow to tune the authentication process
    // - default value is [saoUserByLogonOrID]
    property Options: TRestServerAuthenticationOptions
      read fOptions write fOptions;
  end;

  /// weak authentication scheme using URL-level parameter
  TRestServerAuthenticationURI = class(TRestServerAuthentication)
  public
    /// will check URI-level signature
    // - retrieve the session ID from 'session_signature=...' parameter
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(
      Ctxt: TRestServerURIContext): TAuthSession; override;
  end;

  /// secure authentication scheme using URL-level digital signature
  // - match TRestClientAuthenticationSignedURI on Client side
  // - for instance, default suaCRC32 format of session_signature is
  // !Hexa8(SessionID)+
  // !Hexa8(Timestamp)+
  // !Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
  // !            Hexa8(Timestamp)+url))
  TRestServerAuthenticationSignedURI = class(TRestServerAuthenticationURI)
  protected
    fNoTimestampCoherencyCheck: boolean;
    fTimestampCoherencySeconds: cardinal;
    fTimestampCoherencyTicks: cardinal;
    fComputeSignature: TOnRestAuthenticationSignedURIComputeSignature;
    procedure SetNoTimestampCoherencyCheck(value: boolean);
    procedure SetTimestampCoherencySeconds(value: cardinal);
    procedure SetAlgorithm(value: TRestAuthenticationSignedURIAlgo);
  public
    /// initialize the authentication method to a specified server
    constructor Create(aServer: TRestServer); override;
    /// will check URI-level signature
    // - check session_signature=... parameter to be a valid digital signature
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(
      Ctxt: TRestServerURIContext): TAuthSession; override;
    /// allow any order when creating sessions
    // - by default, signed sessions are expected to be sequential, and new
    // signed session signature can't be older in time than the last one,
    // with a tolerance of TimestampCoherencySeconds
    // - but if your client is asynchronous (e.g. for AJAX requests), session
    // may be rejected due to the delay involved on the client side: you can set
    // this property to TRUE to enabled a weaker but more tolerant behavior
    // ! (aServer.AuthenticationRegister(TRestServerAuthenticationDefault) as
    // !   TRestServerAuthenticationSignedURI).NoTimestampCoherencyCheck := true;
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
    // - the very same function should be set on TRestClientURI
    // - to select a known hash algorithm, you may change the Algorithm property
    property ComputeSignature: TOnRestAuthenticationSignedURIComputeSignature
      read fComputeSignature write fComputeSignature;
    /// customize the session_signature signing algorithm
    // - you need to set this value on the server side only; those known algorithms
    // will be recognized by TRestClientURI on the client side during the
    // session handshake, to select the matching ComputeSignature function
    property Algorithm: TRestAuthenticationSignedURIAlgo
      write SetAlgorithm;
  end;

  /// mORMot secure RESTful authentication scheme on Server
  // - match TRestClientAuthenticationDefault on Client side
  // - this method will use a password stored via safe SHA-256 hashing in the
  // TAuthUser ORM table
  TRestServerAuthenticationDefault = class(TRestServerAuthenticationSignedURI)
  protected
    /// check a supplied password content
    // - will match ClientComputeSessionKey() algorithm as overridden here, i.e.
    // a SHA-256 based signature with a 10 minutes activation window
    function CheckPassword(Ctxt: TRestServerURIContext;
      User: TAuthUser; const aClientNonce, aPassWord: RawUTF8): boolean; virtual;
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
    // - when you don't need the session any more (e.g. if the TRestClientURI
    // instance is destroyed), you can call the service as such:
    // $ GET ModelRoot/auth?UserName=...&Session=...
    // - for a way of computing SHA-256 in JavaScript, see for instance
    // @http://www.webtoolkit.info/javascript-sha256.html
    function Auth(Ctxt: TRestServerURIContext): boolean; override;
  end;

  /// mORMot weak RESTful authentication scheme
  // - match TRestClientAuthenticationNone on Client side
  // - this method will authenticate with a given username, but no signature
  // - on client side, this scheme is not called by TRestClientURI.SetUser()
  // method - so you have to write:
  // ! TRestServerAuthenticationNone.ClientSetUser(Client,'User','');
  TRestServerAuthenticationNone = class(TRestServerAuthenticationURI)
  public
    /// will try to handle the Auth RESTful method with mORMot authentication
    // - to be called in a weak one pass request:
    // $ GET ModelRoot/auth?UserName=...
    // $ -> if the specified user name exists, will open the corresponding
    // $    session and return 'SessionID+HexaSessionPrivateKey'
    function Auth(Ctxt: TRestServerURIContext): boolean; override;
  end;

  /// abstract class for implementing HTTP authentication
  // - do not use this abstract class, but e.g. TRestServerAuthenticationHttpBasic
  // - this class will transmit the session_signature as HTTP cookie, not at
  // URI level, so is expected to be used only from browsers or old clients
  TRestServerAuthenticationHttpAbstract = class(TRestServerAuthentication)
  protected
    /// should be overriden according to the HTTP authentication scheme
    class function ComputeAuthenticateHeader(
      const aUserName, aPasswordClear: RawUTF8): RawUTF8; virtual; abstract;
  public
    /// will check the caller signature
    // - retrieve the session ID from "Cookie: mORMot_session_signature=..." HTTP header
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(Ctxt: TRestServerURIContext): TAuthSession; override;
  end;

  /// authentication using HTTP Basic scheme
  // - match TRestClientAuthenticationHttpBasic on Client side
  // - this protocol send both name and password as clear (just base-64 encoded)
  // so should only be used over SSL / HTTPS, or for compatibility reasons
  // - will rely on TRestServerAuthenticationNone for authorization
  // - on client side, this scheme is not called by TRestClientURI.SetUser()
  // method - so you have to write:
  // ! TRestServerAuthenticationHttpBasic.ClientSetUser(Client,'User','password');
  // - for a remote proxy-only authentication (without creating any mORMot
  // session), you can write:
  // ! TRestServerAuthenticationHttpBasic.ClientSetUserHttpOnly(Client,'proxyUser','proxyPass');
  TRestServerAuthenticationHttpBasic = class(TRestServerAuthenticationHttpAbstract)
  protected
    /// this overriden method returns "Authorization: Basic ...." HTTP header
    class function ComputeAuthenticateHeader(
      const aUserName, aPasswordClear: RawUTF8): RawUTF8; override;
    /// decode "Authorization: Basic ...." header
    // - you could implement you own password transmission pattern, by
    // overriding both ComputeAuthenticateHeader and GetUserPassFromInHead methods
    class function GetUserPassFromInHead(Ctxt: TRestServerURIContext;
      out userPass, user, pass: RawUTF8): boolean; virtual;
    /// check a supplied password content
    // - this default implementation will use the SHA-256 hash value stored
    // within User.PasswordHashHexa
    // - you can override this method to provide your own password check
    // mechanism, for the given TAuthUser instance
    function CheckPassword(Ctxt: TRestServerURIContext;
      User: TAuthUser; const aPassWord: RawUTF8): boolean; virtual;
  public
    /// will check URI-level signature
    // - retrieve the session ID from 'session_signature=...' parameter
    // - will also check incoming "Authorization: Basic ...." HTTP header
    // - method execution should be protected by TRestServer.fSessions.Lock
    function RetrieveSession(Ctxt: TRestServerURIContext): TAuthSession; override;
    /// handle the Auth RESTful method with HTTP Basic
    // - will first return HTTP_UNAUTHORIZED (401), then expect user and password
    // to be supplied as incoming "Authorization: Basic ...." headers
    function Auth(Ctxt: TRestServerURIContext): boolean; override;
  end;

  {$ifdef DOMAINRESTAUTH}
  { will use mormot.lib.sspi/gssapi units depending on the OS }

  /// authentication of the current logged user using Windows Security Support
  // Provider Interface (SSPI) or GSSAPI library on Linux
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
  TRestServerAuthenticationSSPI = class(TRestServerAuthenticationSignedURI)
  protected
    /// Windows built-in authentication
    // - holds information between calls to ServerSSPIAuth
    fSSPIAuthContexts: TSecContextDynArray;
  public
    /// finalize internal sspi/gssapi allocated structures
    destructor Destroy; override;
    /// will try to handle the Auth RESTful method with Windows SSPI API
    // - to be called in a two pass algorithm, used to cypher the password
    // - the client-side logged user will be identified as valid, according
    // to a Windows SSPI API secure challenge
    function Auth(Ctxt: TRestServerURIContext): boolean; override;
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

  /// used for high-level statistics in TRestServer.URI()
  TRestServerMonitor = class(TSynMonitorServer)
  protected
    fServer: TRestServer;
    fStartDate: RawUTF8;
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
    // no overriden Changed: TRestServer.URI will do it in finally block
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
    procedure NotifyORM(aMethod: TURIMethod);
    /// update the per-table statistics
    // - this method is thread-safe
    procedure NotifyOrmTable(TableIndex, DataSize: integer; Write: boolean;
      const MicroSecondsElapsed: QWord);
  published
    /// when this monitoring instance (therefore the server) was created
    property StartDate: RawUTF8 read fStartDate;
    /// number of valid responses
    // - i.e. which returned status code 200/HTTP_SUCCESS or 201/HTTP_CREATED
    // - any invalid request will increase the TSynMonitor.Errors property
    property Success: TSynMonitorCount64 read fSuccess;
    /// count of the remote method-based service calls
    property ServiceMethod: TSynMonitorCount64 read fServiceMethod;
    /// count of the remote interface-based service calls
    property ServiceInterface: TSynMonitorCount64 read fServiceInterface;
    /// count of files transmitted directly (not part of Output size property)
    // - i.e. when the service uses STATICFILE_CONTENT_TYPE/HTTP_RESP_STATICFILE
    // as content type to let the HTTP server directly serve the file content
    property OutcomingFiles: TSynMonitorCount64 read fOutcomingFiles;
    /// number of current declared thread counts
    // - as registered by BeginCurrentThread/EndCurrentThread
    property CurrentThreadCount: TSynMonitorOneCount read fCurrentThreadCount;
    /// how many Create / Add ORM operations did take place
    property Created: TSynMonitorCount64 read fCreated;
    /// how many Read / Get ORM operations did take place
    property Read: TSynMonitorCount64 read fRead;
    /// how many Update ORM operations did take place
    property Updated: TSynMonitorCount64 read fUpdated;
    /// how many Delete ORM operations did take place
    property Deleted: TSynMonitorCount64 read fDeleted;
  end;

  /// ORM table used to store TSynMonitorUsage information in TSynMonitorUsageRest
  // - the ID primary field is the TSynMonitorUsageID (accessible from UsageID
  // public property) shifted by 16 bits (by default) to include a
  // TSynUniqueIdentifierProcess value
  TOrmMonitorUsage = class(TOrmNoCaseExtended)
  protected
    fGran: TSynMonitorUsageGranularity;
    fProcess: Int64;
    fInfo: variant;
    fComment: RawUTF8;
  public
    /// compute the corresponding 23 bit TSynMonitorUsageID.Value time slice
    // - according to the stored Process field, after bit shift
    // - allows a custom aProcessIDShift if it is not set as default 16 bits
    function UsageID(aProcessIDShift: integer = 16): integer;
  published
    /// the granularity of the statistics of this entry
    property Gran: TSynMonitorUsageGranularity read fGran write fGran;
    /// identify which application is monitored
    // - match the lower bits of each record ID
    // - by default, is expected to be a TSynUniqueIdentifierProcess 16-bit value
    property Process: Int64 read fProcess write fProcess;
    /// the actual statistics information, stored as a TDocVariant JSON object
    property Info: variant read fInfo write fInfo;
    /// a custom text, which may be used e.g. by support or developpers
    property Comment: RawUTF8 read fComment write fComment;
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
    property SaveBatch: TRestBatch read fSaveBatch write fSaveBatch;
  published
    /// the actual ORM class used for persistence
    property StoredClass: TOrmMonitorUsageClass read fStoredClass;
    /// how the information could be stored for several processes
    // - e.g. when several SOA nodes gather monitoring information in a
    // shared (MongoDB) database
    // - is by default a TSynUniqueIdentifierProcess value, but may be
    // any integer up to ProcessIDShift bits as set in Create()
    property ProcessID: Int64 read fProcessID;
    /// how process ID are stored within the mORMot TOrm.ID
    // - equals 16 bits by default, to match TSynUniqueIdentifierProcess resolution
    property ProcessIDShift: integer read fProcessIDShift;
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
    constructor Create(aRest: TRest; const aGUID: TGUID); reintroduce;
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
    property Rest: TRest read fRest;
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
      aRest: TRest; const aGUID: TGUID); reintroduce;
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
    property Process: TBlockingProcess read fProcess;
  published
    /// the current state of process
    // - just a wrapper around Process.Event
    // - use Reset method to re-use this instance after a WaitFor process
    property Event: TBlockingEvent read GetEvent;
  end;



{ ************ TRestServer Abstract REST Server }

  /// some options for TRestServer process
  // - read-only rsoNoAJAXJSON indicates that JSON data is transmitted in "not
  // expanded" format: you should NEVER change this option by including
  // this property in TRestServer.Options, but always call explicitly
  // TRestServer.NoAJAXJSON := true so that the SetNoAJAXJSON virtual
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
  // - TRestServerURIContext.AuthenticationBearerToken will return the
  // ?authenticationbearer=... URI parameter value alternatively to the HTTP
  // header unless rsoAuthenticationURIDisable is set (for security reasons)
  // - you can switch off root/timestamp/info URI via rsoTimestampInfoURIDisable
  // - URI() header output will be sanitized for any EOL injection, unless
  // rsoHttpHeaderCheckDisable is defined (to gain a few cycles?)
  // - by default, TAuthUser.Data blob is retrieved from the database,
  // unless rsoGetUserRetrieveNoBlobData is defined
  // - rsoNoInternalState could be state to avoid transmitting the
  // 'Server-InternalState' header, e.g. if the clients wouldn't need it
  TRestServerOption = (
    rsoNoAJAXJSON,
    rsoGetAsJsonNotAsString,
    rsoGetID_str,
    rsoRedirectForbiddenToAuth,
    rsoHttp200WithNoBodyReturns204,
    rsoAddUpdateReturnsContent,
    rsoComputeFieldsBeforeWriteOnServerSide,
    rsoSecureConnectionRequired,
    rsoCookieIncludeRootPath,
    rsoCookieHttpOnlyFlagDisable,
    rsoAuthenticationURIDisable,
    rsoTimestampInfoURIDisable,
    rsoHttpHeaderCheckDisable,
    rsoGetUserRetrieveNoBlobData,
    rsoNoInternalState);

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
  TRestServerURIPagingParameters = record
    /// parameter name used to specify the request sort order
    // - default value is 'SORT='
    Sort: RawUTF8;
    /// parameter name used to specify the request sort direction
    // - default value is 'DIR='
    Dir: RawUTF8;
    /// parameter name used to specify the request starting offset
    // - default value is 'STARTINDEX='
    StartIndex: RawUTF8;
    /// parameter name used to specify the request the page size (LIMIT clause)
    // - default value is 'RESULTS='
    Results: RawUTF8;
    /// parameter name used to specify the request field names
    // - default value is 'SELECT='
    Select: RawUTF8;
    /// parameter name used to specify the request WHERE clause
    // - default value is 'WHERE='
    Where: RawUTF8;
    /// returned JSON field value of optional total row counts
    // - default value is nil, i.e. no total row counts field
    // - computing total row counts can be very expensive, depending on the
    // database back-end used (especially for external databases)
    // - can be set e.g. to ',"totalRows":%' value (note that the initial "," is
    // expected by the produced JSON content, and % will be set with the value)
    SendTotalRowsCountFmt: RawUTF8;
  end;

  ///  used to define how to trigger Events on record update
  // - see TRestServer.OnUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occured (but action must continue)
  // - to be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientURI.UpdateFromServer)
  TOnOrmEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aSentData: RawUTF8): boolean of object;

  ///  used to define how to trigger Events on record field update
  // - see TRestServer.OnBlobUpdateEvent property and InternalUpdateEvent() method
  // - returns true on success, false if an error occured (but action must continue)
  // - to be used only server-side, not to synchronize some clients: the framework
  // is designed around a stateless RESTful architecture (like HTTP/1.1), in which
  // clients ask the server for refresh (see TRestClientURI.UpdateFromServer)
  TOnOrmFieldEvent = function(Sender: TRestServer; Event: TOrmEvent;
    aTable: TOrmClass; const aID: TID;
    const aAffectedFields: TFieldBits): boolean of object;

  /// session-related callbacks triggered by TRestServer
  // - for OnSessionCreate, returning TRUE will abort the session creation -
  // and you can set Ctxt.Call^.OutStatus to a corresponding error code
  TOnOrmSession = function(Sender: TRestServer; Session: TAuthSession;
    Ctxt: TRestServerURIContext): boolean of object;

  /// callback allowing to customize the retrieval of an authenticated user
  // - as defined in TRestServer.OnAuthenticationUserRetrieve
  // - and executed by TRestServerAuthentication.GetUser
  // - on call, either aUserID will be <> 0, or aUserName is to be used
  // - if the function returns nil, default Server.SQLAuthUserClass.Create()
  // methods won't be called, and the user will be reported as not found
  TOnAuthenticationUserRetrieve = function(Sender: TRestServerAuthentication;
    Ctxt: TRestServerURIContext; aUserID: TID;
    const aUserName: RawUTF8): TAuthUser of object;

  /// callback raised in case of authentication failure
  // - as used by TRestServerURIContext.AuthenticationFailed event
  TOnAuthenticationFailed = procedure(Sender: TRestServer;
    Reason: TOnAuthenticationFailedReason; Session: TAuthSession;
    Ctxt: TRestServerURIContext) of object;

  /// callback raised before TRestServer.URI execution
  // - should return TRUE to execute the command, FALSE to cancel it
  TOnBeforeURI = function(Ctxt: TRestServerURIContext): boolean of object;

  /// callback raised after TRestServer.URI execution
  TOnAfterURI = procedure(Ctxt: TRestServerURIContext) of object;

  /// callback raised if TRestServer.URI execution failed
  // - should return TRUE to execute Ctxt.Error(E,...), FALSE if returned
  // content has already been set as expected by the client
  TNotifyErrorURI = function(Ctxt: TRestServerURIContext;
    E: Exception): boolean of object;

  /// event signature used to notify a client callback
  // - implemented e.g. by TRestHttpServer.NotifyCallback
  TOnRestServerClientCallback = function(aSender: TRestServer;
    const aInterfaceDotMethodName, aParams: RawUTF8;
    aConnectionID: Int64; aFakeCallID: integer;
    aResult, aErrorMsg: PRawUTF8): boolean of object;

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
  // - automatic call of this methods by a generic URI() RESTful function
  // - any published method of descendants must match TOnRestServerCallBack
  // prototype, and is expected to be thread-safe
  TRestServer = class(TRest)
  protected
    fHandleAuthentication: boolean;
    fBypassORMAuthentication: TURIMethods;
    /// the TAuthUser and TAuthGroup classes, as defined in model
    fSQLAuthUserClass: TAuthUserClass;
    fSQLAuthGroupClass: TAuthGroupClass;
    fAfterCreation: boolean;
    fOptions: TRestServerOptions;
    /// how in-memory sessions are handled
    fSessionClass: TAuthSessionClass;
    fJWTForUnauthenticatedRequest: TJWTAbstract;
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
    fRootRedirectGet: RawUTF8;
    fPublicURI: TRestServerURI;
    fIPBan, fIPWhiteJWT: TIPBan;
    fOnIdleLastTix: cardinal;
    fServicesRouting: TRestServerURIContextClass;
    fRecordVersionSlaveCallbacks: array of IServiceRecordVersionCallback;
    procedure SetNoAJAXJSON(const Value: boolean);
    function GetNoAJAXJSON: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    function GetRecordVersionMax: TRecordVersion;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetRecordVersionMax(Value: TRecordVersion);
    function GetAuthenticationSchemesCount: integer;
    /// ensure the thread will be taken into account during process
    procedure OnBeginCurrentThread(Sender: TThread); override;
    procedure OnEndCurrentThread(Sender: TThread); override;
    // called by Stat() and Info() method-based services
    procedure InternalStat(Ctxt: TRestServerURIContext; W: TTextWriter); virtual;
    procedure AddStat(Flags: TRestServerAddStats; W: TTextWriter);
    procedure InternalInfo(var info: TDocVariantData); virtual;
    procedure SetStatUsage(usage: TSynMonitorUsage);
    function GetServiceMethodStat(const aMethod: RawUTF8): TSynMonitorInputOutput;
    procedure SetRoutingClass(aServicesRouting: TRestServerURIContextClass);
    /// add a new session to the internal session list
    // - do not use this method directly: this callback is to be used by
    // TRestServerAuthentication* classes
    // - will check that the logon name is valid
    // - on failure, will call TRestServerURIContext.AuthenticationFailed()
    // with afSessionAlreadyStartedForThisUser or afSessionCreationAborted reason
    procedure SessionCreate(var User: TAuthUser; Ctxt: TRestServerURIContext;
      out Session: TAuthSession); virtual;
    /// search for Ctxt.Session ID and fill Ctxt.Session* members if found
    // - returns nil if not found, or fill aContext.User/Group values if matchs
    // - this method will also check for outdated sessions, and delete them
    // - this method is not thread-safe: caller should use Sessions.Lock/Unlock
    function SessionAccess(Ctxt: TRestServerURIContext): TAuthSession;
    /// delete a session from its index in Sessions[]
    // - will perform any needed clean-up, and log the event
    // - this method is not thread-safe: caller should use Sessions.Lock/Unlock
    procedure SessionDelete(aSessionIndex: integer; Ctxt: TRestServerURIContext);
  public
    /// a method can be specified to be notified when a session is created
    // - for OnSessionCreate, returning TRUE will abort the session creation -
    // and you can set Ctxt.Call^.OutStatus to a corresponding error code
    // - it could be used e.g. to limit the number of client sessions
    OnSessionCreate: TOnOrmSession;
    /// a custom method to retrieve the TAuthUser instance for authentication
    // - will be called by TRestServerAuthentication.GetUser() instead of
    // plain SQLAuthUserClass.Create()
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
    /// event trigerred when URI() starts to process a request
    // - the supplied Ctxt parameter will give access to the command about to
    // be executed, e.g. Ctxt.Command=execSOAByInterface will identify a SOA
    // service execution, with the corresponding Service and ServiceMethodIndex
    // parameters as set by TRestServerURIContext.URIDecodeSOAByInterface
    // - should return TRUE if the method can be executed
    // - should return FALSE if the method should not be executed, and the
    // callback should set the corresponding error to the supplied context e.g.
    // ! Ctxt.Error('Unauthorized method',HTTP_NOTALLOWED);
    // - since this event will be executed by every TRestServer.URI call,
    // it should better not make any slow process (like writing to a remote DB)
    // - see also TRest.OnDecryptBody, which is common to the client side, so
    // may be a better place for implementing shared process (e.g. encryption)
    OnBeforeURI: TOnBeforeURI;
    /// event trigerred when URI() finished to process a request
    // - the supplied Ctxt parameter will give access to the command which has
    // been executed, e.g. via Ctxt.Call.OutStatus or Ctxt.MicroSecondsElapsed
    // - since this event will be executed by every TRestServer.URI call,
    // it should better not make any slow process (like writing to a remote DB)
    // - see also TRest.OnDecryptBody/OnEncryptBody, which is common to the
    // client side, so may be better to implement shared process (e.g. encryption)
    OnAfterURI: TOnAfterURI;
    /// event trigerred when URI() failed to process a request
    // - if Ctxt.ExecuteCommand raised an execption, this callback will be
    // run with all neeed information
    // - should return TRUE to execute Ctxt.Error(E,...), FALSE if returned
    // content has already been set as expected by the client
    OnErrorURI: TNotifyErrorURI;
    /// event to customize the information returned by root/timestamp/info
    // - called by TRestServer.InternalInfo method
    // - you can add some application-level information for monitoring
    OnInternalInfo: TOnInternalInfo;
    /// event trigerred when URI() is called, and at least 128 ms is elapsed
    // - could be used to execute some additional process after a period of time
    // - note that if TRestServer.URI is not called by any client, this
    // callback won't be executed either
    OnIdle: TNotifyEvent;
    /// this property can be used to specify the URI parmeters to be used
    // for paged queries
    // - is set by default to PAGINGPARAMETERS_YAHOO constant by
    // TRestServer.Create() constructor
    URIPagingParameters: TRestServerURIPagingParameters;

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
    // - if you instantiate a TRestServerFullMemory or TSQLRestServerDB
    // with this constructor, an in-memory engine will be created, with
    // enough abilities to run regression tests, for instance
    constructor CreateWithOwnModel(const Tables: array of TOrmClass;
      aHandleUserAuthentication: boolean = false;
      const aRoot: RawUTF8 = 'root');

    /// implement a generic local, piped or HTTP/1.1 provider
    // - this is the main entry point of the server, from the client side
    // - default implementation calls protected methods EngineList() Retrieve()
    // Add() Update() Delete() UnLock() EngineExecute() above, which must be overridden by
    // the TRestServer child
    // - for 'GET ModelRoot/TableName', url parameters can be either "select" and
    // "where" (to specify a SQL Query, from the SQLFromSelectWhere function),
    // either "sort", "dir", "startIndex", "results", as expected by the YUI
    // DataSource Request Syntax for data pagination - see
    // http://developer.yahoo.com/yui/datatable/#data
    // - execution of this method could be monitored via OnBeforeURI and OnAfterURI
    // event handlers
    procedure URI(var Call: TRestURIParams); virtual;

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
    function OrmInstance: TRestOrm;
      {$ifdef HASINLINE}inline;{$endif}
    /// access TRestOrmServer.RecordVersionMax property
    // - used internally by TServiceContainerServer for client/server synchronization
    property RecordVersionMax: TRecordVersion
      read GetRecordVersionMax write SetRecordVersionMax;
    /// low-level propagation of a record content
    // - used internally by TServiceContainerServer for client/server synchronization
    procedure RecordVersionHandle(Occasion: TOrmOccasion;
      TableIndex: integer; var Decoder: TJSONObjectDecoder;
      RecordVersionField: TOrmPropInfoRTTIRecordVersion); virtual;
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
    // !   TRestServerAuthenticationDefault, TRestServerAuthenticationSSPI]);
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
    /// (un)register a banned IPv4 value
    // - any connection attempt from this IP Address will be rejected by
    function BanIP(const aIP: RawUTF8; aRemoveBan: boolean = false): boolean;
    /// (un)register a an IPv4 value to the JWT white list
    // - by default, a JWT validated by JWTForUnauthenticatedRequest will be accepted
    // - to avoid MiM (Man-In-the-Middle) attacks, if a JWT white list is defined
    // using this method, any connection from a non registered IP will be rejected,
    // even with a valid JWT
    // - WebSockets connections are secure enough to bypass this list
    function JWTForUnauthenticatedRequestWhiteIP(const aIP: RawUTF8;
      aRemoveWhite: boolean = false): boolean;
    /// returns a copy of the user associated to a session ID
    // - returns nil if the session does not exist (e.g. if authentication is
    // disabled)
    // - caller MUST release the TAuthUser instance returned (if not nil)
    // - this method IS thread-safe, and calls internaly Sessions.Lock
    // (the returned TAuthUser is a private copy from Sessions[].User instance,
    // in order to be really thread-safe)
    // - the returned TAuthUser instance will have GroupRights=nil but will
    // have ID, LogonName, DisplayName, PasswordHashHexa and Data fields available
    function SessionGetUser(aSessionID: Cardinal): TAuthUser;
    /// persist all in-memory sessions into a compressed binary file
    // - you should not call this method it directly, but rather use Shutdown()
    // with a StateFileName parameter - to be used e.g. for a short maintainance
    // server shutdown, without loosing the current logged user sessions
    // - this method IS thread-safe, and call internaly Sessions.Lock
    procedure SessionsSaveToFile(const aFileName: TFileName);
    /// re-create all in-memory sessions from a compressed binary file
    // - typical use is after a server restart, with the file supplied to the
    // Shutdown() method: it could be used e.g. for a short maintainance server
    // shutdown, without loosing the current logged user sessions
    // - WARNING: this method will restore authentication sessions for the ORM,
    // but not any complex state information used by interface-based services,
    // like sicClientDriven class instances - DO NOT use this feature with SOA
    // - this method IS thread-safe, and call internaly Sessions.Lock
    procedure SessionsLoadFromFile(const aFileName: TFileName;
      andDeleteExistingFileAfterRead: boolean);
    /// retrieve all current session information as a JSON array
    function SessionsAsJson: RawJSON;
    /// retrieve the current session TAuthUser.ID (if any) from the
    // ServiceRunningContext threadvar
    function GetCurrentSessionUserID: TID; override;
    /// the HTTP server should call this method so that ServicesPublishedInterfaces
    // registration will be able to work
    procedure SetPublicURI(const Address, Port: RawUTF8);

    /// add all published methods of a given object instance to the method-based
    // list of services
    // - all those published method signature should match TOnRestServerCallBack
    procedure ServiceMethodRegisterPublishedMethods(const aPrefix: RawUTF8;
      aInstance: TObject);
    /// direct registration of a method for a given low-level event handler
    procedure ServiceMethodRegister(aMethodName: RawUTF8;
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
      const aMethodName: RawUTF8): integer;
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
      const aContractExpected: RawUTF8 = ''): TServiceFactoryServerAbstract; overload; virtual;
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
      const aContractExpected: RawUTF8 = ''): TServiceFactoryServerAbstract; overload; virtual;
    /// register a remote Service via its interface
    // - this overloaded method will register a remote Service, accessed via the
    // supplied TRest/TRestClientURI instance: it can be available in the main
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
      const aContractExpected: RawUTF8 = ''): boolean; overload; virtual;
    /// register a Service class on the server side
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aImplementationClass: TInterfacedClass;
      const aInterfaces: array of TGUID;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUTF8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a Service instance on the server side
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - the supplied aSharedImplementation will be owned by this Server instance
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aSharedImplementation: TInterfacedObject;
      const aInterfaces: array of TGUID;
      const aContractExpected: RawUTF8 = ''): TServiceFactoryServerAbstract; overload;
    /// register a remote Service via its interface
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - you can use the returned TServiceFactoryServerAbstract instance to set the
    // expected security parameters associated as a fluent interface
    function ServiceDefine(aClient: TRest; const aInterfaces: array of TGUID;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUTF8 = ''): boolean; overload;
    /// the routing classs of the service remote request
    // - by default, will use TRestRoutingREST, i.e. an URI-based
    // layout which is secure (since will use our RESTful authentication scheme),
    // and also very fast
    // - but TRestRoutingJSON_RPC can e.g. be set (on BOTH client and
    // server sides), if the client will rather use JSON/RPC alternative pattern
    // - NEVER set the abstract TRestServerURIContext class on this property
    property ServicesRouting: TRestServerURIContextClass
      read fServicesRouting write SetRoutingClass;
    /// retrieve detailed statistics about a method-based service use
    // - will return a reference to the actual alive item: caller should
    // not free the returned instance
    property ServiceMethodStat[const aMethod: RawUTF8]: TSynMonitorInputOutput
      read GetServiceMethodStat;
    /// compute a JSON description of all available services, and its public URI
    // - the JSON object matches the TServicesPublishedInterfaces record type
    // - used by TRestClientURI.ServicePublishOwnInterfaces to register all
    // the services supported by the client itself
    // - warning: the public URI should have been set via SetPublicURI()
    function ServicesPublishedInterfaces: RawUTF8;
    /// initiate asynchronous master/slave replication on a master TRest
    // - allow synchronization of a TOrm table, using its TRecordVersion
    // field, for real-time master/slave replication on the master side
    // - this method will register the IServiceRecordVersion service on the
    // server side, so that RecordVersionSynchronizeStartSlave() will be able
    // to receive push notifications of any updates
    // - this method expects the communication channel to be bidirectional, e.g.
    // a mORMotHTTPServer's TRestHttpServer in useBidirSocket mode
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
      MasterRemoteAccess: TRestClientURI; OnNotify: TOnBatchWrite = nil): boolean;
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
    // callbacks using WebSockets, as implemented by SynBidirSock.pas and
    // mORMotHTTPServer's TRestHttpServer in useBidirSocket mode, are asynchronous
    // - if the supplied RecordVersion is not the latest on the server side,
    // this method will return FALSE and the caller should synchronize again via
    // RecordVersionSynchronize() to avoid any missing update
    // - if the supplied RecordVersion is the latest on the server side,
    // this method will return TRUE and put the Callback notification in place
    function RecordVersionSynchronizeSubscribeMaster(Table: TOrmClass;
      RecordVersion: TRecordVersion;
      const SlaveCallback: IServiceRecordVersionCallback): boolean; overload;
    /// set this property to true to transmit the JSON data in a "not expanded" format
    // - not directly compatible with Javascript object list decode: not to be
    // used in AJAX environnement (like in TSQLite3HttpServer)
    // - but transmitted JSON data is much smaller if set it's set to FALSE, and
    // if you use a Delphi Client, parsing will be also faster and memory usage
    // will be lower
    // - By default, the NoAJAXJSON property is set to TRUE in
    // TRestServer.ExportServerNamedPipe: if you use named pipes for communication,
    // you probably won't use javascript because browser communicates via HTTP!
    // - But otherwise, NoAJAXJSON property is set to FALSE. You could force its
    // value to TRUE and you'd save some bandwidth if you don't use javascript:
    // even the parsing of the JSON Content will be faster with Delphi client
    // if JSON content is not expanded
    // - the "expanded" or standard/AJAX layout allows you to create pure JavaScript
    // objects from the JSON content, because the field name / JavaScript object
    // property name is supplied for every value
    // - the "not expanded" layout, NoAJAXJSON property is set to TRUE,
    // reflects exactly the layout of the SQL request - first line contains the
    // field names, then all next lines are the field content
    // - is in fact stored in rsoNoAJAXJSON item in Options property
    property NoAJAXJSON: boolean
      read GetNoAJAXJSON write SetNoAJAXJSON;
    /// the URI to redirect any plain GET on root URI, without any method
    // - could be used to ease access from web browsers URI
    property RootRedirectGet: RawUTF8
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
    /// allow to customize how TRestServer.URI process the requests
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
    property BypassORMAuthentication: TURIMethods
      read fBypassORMAuthentication write fBypassORMAuthentication;
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
    property SQLAuthUserClass: TAuthUserClass
      read fSQLAuthUserClass;
    /// the class inheriting from TAuthGroup, as defined in the model
    // - during authentication, this class will be used for every TAuthGroup
    // table access
    property SQLAuthGroupClass: TAuthGroupClass
      read fSQLAuthGroupClass;
  end;

  /// class-reference type (metaclass) of a REST server
  TRestServerClass = class of TRestServer;


const
  /// the default URI parameters for query paging
  // - those values are the one expected by YUI components
  PAGINGPARAMETERS_YAHOO: TRestServerURIPagingParameters = (
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
// !    inContentType: RawUTF8;
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
function CurrentServerNonce(Previous: boolean = false): RawUTF8;



{ ************ TRestHttpServerDefinition Settings for a HTTP Server }

type
  /// supported REST authentication schemes
  // - used by the overloaded TRestHttpServer.Create(TRestHttpServerDefinition)
  // constructor in mORMotHttpServer.pas, and also in dddInfraSettings.pas
  // - asSSPI won't be defined under Linux, since it is a Windows-centric feature
  TRestHttpServerRestAuthentication = (
    adDefault,
    adHttpBasic,
    adWeak,
    adSSPI);

  /// parameters supplied to publish a TSQLRestServer via HTTP
  // - used by the overloaded TRestHttpServer.Create(TRestHttpServerDefinition)
  // constructor in mORMotHttpServer.pas, and also in dddInfraSettings.pas
  TRestHttpServerDefinition = class(TSynPersistentWithPassword)
  protected
    fBindPort: RawByteString;
    fAuthentication: TRestHttpServerRestAuthentication;
    fEnableCORS: RawUTF8;
    fThreadCount: byte;
    fHttps: boolean;
    fHttpSysQueueName: SynUnicode;
    fRemoteIPHeader: RawUTF8;
  published
    /// defines the port to be used for REST publishing
    // - may include an optional IP address to bind, e.g. '127.0.0.1:8888'
    property BindPort: RawByteString
      read fBindPort write fBindPort;
    /// which authentication is expected to be published
    property Authentication: TRestHttpServerRestAuthentication
      read fAuthentication write fAuthentication;
    /// allow Cross-origin resource sharing (CORS) access
    // - set this property to '*' if you want to be able to access the
    // REST methods from an HTML5 application hosted in another location,
    // or define a CSV white list of TMatch-compatible origins
    // - will set e.g. the following HTTP header:
    // ! Access-Control-Allow-Origin: *
    property EnableCORS: RawUTF8
      read fEnableCORS write fEnableCORS;
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
    property RemoteIPHeader: RawUTF8
      read fRemoteIPHeader write fRemoteIPHeader;
    /// if defined, this HTTP server will use WebSockets, and our secure
    // encrypted binary protocol
    // - when stored in the settings JSON file, the password will be safely
    // encrypted as defined by TSynPersistentWithPassword
    // - use the inherited PlainPassword property to set or read its value
    property WebSocketPassword: SPIUTF8
      read fPassWord write fPassWord;
  end;



{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLRestServer = TRestServer;
  TSQLRestServerClass = TRestServerClass;
  TSQLRestServerURIContext = TRestServerURIContext;
  TSQLRestServerURIContextClass = TRestServerURIContextClass;
  TSQLRestServerAuthenticationClass = TRestServerAuthenticationClass;
  TSQLRestServerAuthenticationNone  = TRestServerAuthenticationNone;
  TSQLRestServerAuthenticationDefault = TRestServerAuthenticationDefault;
  TSQLRestServerAuthenticationHttpBasic = TRestServerAuthenticationHttpBasic;

{$endif PUREMORMOT2}


implementation

uses
  mormot.soa.server,
  mormot.orm.server,
  mormot.orm.storage;


{ ************ TRestServerURIContext Access to the Server-Side Execution }

function ToText(res: TOnAuthenticationFailedReason): PShortString;
begin
  result := GetEnumName(TypeInfo(TOnAuthenticationFailedReason), ord(res));
end;

function ServiceRunningContext: PServiceRunningContext;
begin
  result := PerThreadRunningContextAddress; // from mormot.core.interfaces.pas
end;



{ TRestServerURIContext }

constructor TRestServerURIContext.Create(aServer: TRestServer;
  const aCall: TRestURIParams);
begin
  inherited Create;
  Server := aServer;
  Call := @aCall;
  Method := ToMethod(aCall.Method);
  ;
  ThreadServer := PerThreadRunningContextAddress;
  ThreadServer^.Request := self;
end;

destructor TRestServerURIContext.Destroy;
begin
  if ThreadServer <> nil then
    ThreadServer^.Request := nil;
  inherited Destroy;
end;

procedure TRestServerURIContext.InternalSetTableFromTableName(
  TableName: PUTF8Char);
begin
  TableEngine := TRestOrm(Server.fOrmInstance);
  InternalSetTableFromTableIndex(Server.fModel.GetTableIndexPtr(TableName));
  if TableIndex < 0 then
    exit;
  StaticOrm := TRestOrmServer(Server.fOrmInstance).
    GetStaticTableIndex(TableIndex, StaticKind);
  if StaticOrm <> nil then
    TableEngine := StaticOrm;
end;

procedure TRestServerURIContext.InternalSetTableFromTableIndex(Index: integer);
begin
  TableIndex := Index;
  if TableIndex >= 0 then
    with Server.fModel do
    begin
      self.Table := Tables[TableIndex];
      self.TableModelProps := TableProps[TableIndex];
    end;
end;

function TRestServerURIContext.URIDecodeREST: boolean;
var
  i, j, slash: PtrInt;
  Par: PUTF8Char;
begin
  // expects 'ModelRoot[/TableName[/TableID][/URIBlobFieldName]][?param=...]' format
  // check root URI and Parameters
  i := 0;
  if (Call^.url <> '') and
     (Call^.url[1] = '/') then
    inc(i); // URL may be '/path'
  j := length(Server.fModel.Root);
  if (i + j > length(Call^.Url)) or
     (not (Call^.Url[i + j + 1] in [#0, '/', '?'])) or
     (StrCompIL(pointer(PtrInt(Call^.url) + i),
       pointer(Server.fModel.Root), j, 0) <> 0) then
  begin
    // bad ModelRoot -> caller can try another TRestServer
    result := False;
    exit;
  end;
  ParametersPos := PosExChar('?', Call^.url);
  if ParametersPos > 0 then
    // '?select=...&where=...' or '?where=...'
    Parameters := @Call^.url[ParametersPos + 1];
  if Method = mPost then
  begin
    fInputPostContentType := Call^.InBodyType(false);
    if (Parameters = nil) and
       IdemPChar(pointer(fInputPostContentType), 'APPLICATION/X-WWW-FORM-URLENCODED') then
      Parameters := pointer(Call^.InBody);
  end;
  // compute URI without any root nor parameter
  inc(i, j + 2);
  if ParametersPos = 0 then
    URI := copy(Call^.url, i, maxInt)
  else
    URI := copy(Call^.url, i, ParametersPos - i);
  URIAfterRoot := PUTF8Char(pointer(Call^.url)) + i - 1;
  // compute Table, TableID and URIBlobFieldName
  slash := PosExChar('/', URI);
  if slash > 0 then
  begin
    URI[slash] := #0;
    Par := pointer(URI);
    InternalSetTableFromTableName(Par);
    inc(Par, slash);
    if (Table <> nil) and
       (Par^ in ['0'..'9']) then
      // "ModelRoot/TableName/TableID/URIBlobFieldName"
      TableID := GetNextItemInt64(Par, '/')
    else
      // URI like "ModelRoot/TableName/MethodName"
      TableID := -1;
    URIBlobFieldName := Par;
    if Table <> nil then
    begin
      j := PosExChar('/', URIBlobFieldName);
      if j > 0 then
      begin
        // handle "ModelRoot/TableName/URIBlobFieldName/ID"
        TableID := GetCardinalDef(pointer(PtrInt(URIBlobFieldName) + j), cardinal(-1));
        SetLength(URIBlobFieldName, j - 1);
      end;
    end;
    SetLength(URI, slash - 1);
  end
  else
    // "ModelRoot/TableName"
    InternalSetTableFromTableName(pointer(URI));
  // compute URISessionSignaturePos and URIWithoutSignature
  if ParametersPos > 0 then
    if IdemPChar(Parameters, 'SESSION_SIGNATURE=') then
      URISessionSignaturePos := ParametersPos
    else
      URISessionSignaturePos := PosEx('&session_signature=', Call^.url,
        ParametersPos + 1);
  if URISessionSignaturePos = 0 then
    URIWithoutSignature := Call^.Url
  else
    URIWithoutSignature := Copy(Call^.Url, 1, URISessionSignaturePos - 1);
  result := True;
end;

procedure TRestServerURIContext.URIDecodeSOAByMethod;
begin
  if Table = nil then
    // check URI as 'ModelRoot/MethodName'
    MethodIndex := Server.fPublishedMethods.FindHashed(URI)
  else if URIBlobFieldName <> '' then
    // check URI as 'ModelRoot/TableName[/TableID]/MethodName'
    MethodIndex := Server.fPublishedMethods.FindHashed(URIBlobFieldName)
  else
    MethodIndex := -1;
end;

var // as set by TRestServer.AdministrationExecute()
  BYPASS_ACCESS_RIGHTS: TOrmAccessRights;

function TRestServerURIContext.Authenticate: boolean;
var
  aSession: TAuthSession;
  i: PtrInt;
begin
  if Server.fHandleAuthentication and not IsRemoteAdministrationExecute then
  begin
    Session := CONST_AUTHENTICATION_SESSION_NOT_STARTED;
    result := false;
    Server.fSessions.Safe.Lock;
    try
      if Server.fSessionAuthentication <> nil then
        for i := 0 to length(Server.fSessionAuthentication) - 1 do
        begin
          aSession := Server.fSessionAuthentication[i].RetrieveSession(self);
          if aSession <> nil then
          begin
            if (aSession.RemoteIP <> '') and
               (aSession.RemoteIP <> '127.0.0.1') then
              Log.Log(sllUserAuth, '%/% %', [aSession.User.LogonName,
                aSession.ID, aSession.RemoteIP], self);
            result := true;
            exit;
          end;
        end;
    finally
      Server.fSessions.Safe.UnLock;
    end;
    // if we reached here, no session was found
    if Service <> nil then
      // you can allow a service to be called directly
      //TODOresult := Service.ByPassAuthentication else
      if MethodIndex >= 0 then
        // /auth + /timestamp are e.g. allowed methods without signature
        result := Server.fPublishedMethod[MethodIndex].ByPassAuthentication
      else if (Table <> nil) and
              (Method in Server.fBypassORMAuthentication) then
        // allow by-pass for a set of HTTP verbs (e.g. mGET)
        result := true;
  end
  else
  begin
    // default unique session if authentication is not enabled
    Session := CONST_AUTHENTICATION_NOT_USED;
    result := true;
  end;
end;

procedure TRestServerURIContext.AuthenticationFailed(
  Reason: TOnAuthenticationFailedReason);
var
  txt: PShortString;
begin
  txt := ToText(Reason);
  Log.Log(sllUserAuth, 'AuthenticationFailed(%) for % (session=%)',
    [txt^, Call^.Url, Session], self);
  // 401 Unauthorized response MUST include a WWW-Authenticate header,
  // which is not what we used, so here we won't send 401 error code but 403
  Call.OutStatus := HTTP_FORBIDDEN;
  FormatUTF8('Authentication Failed: % (%)',
    [UnCamelCase(TrimLeftLowerCaseShort(txt)), ord(Reason)], CustomErrorMsg);
  // call the notification event
  if Assigned(Server.OnAuthenticationFailed) then
    Server.OnAuthenticationFailed(Server, Reason, nil, self);
end;

procedure TRestServerURIContext.ExecuteCommand;

  procedure TimeOut;
  begin
    Log.Log(sllServer, 'TimeOut %.Execute(%) after % ms', [self,
      ToText(Command)^, Server.fAcquireExecution[Command].LockedTimeOut], self);
    if Call <> nil then
      Call^.OutStatus := HTTP_TIMEOUT; // 408 Request Time-out
  end;

var
  Method: TThreadMethod;
  Start64: Int64;
  current: cardinal;
begin
  with Server.fAcquireExecution[Command] do
  begin
    case Command of
      execSOAByMethod:
        Method := ExecuteSOAByMethod;
      execSOAByInterface:
        Method := ExecuteSOAByInterface;
      execOrmGet:
        Method := ExecuteORMGet;
      execOrmWrite:
        begin
          // special behavior to handle transactions at writing
          Method := ExecuteORMWrite;
          Start64 := GetTickCount64;
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
                  ExecuteORMWrite; // process within the obtained write mutex
                  exit;
                end;
                break;   // will handle Mode<>amLocked below
              end;
            finally
              Safe.UnLock;
            end;
            if (LockedTimeOut <> 0) and
               (GetTickCount64 > Start64 + LockedTimeOut) then
            begin
              TimeOut; // wait up to 5 second by default
              exit;
            end;
            SleepHiRes(1); // retry every 1 ms
          until Server.fShutdownRequested;
        end;
    else
      raise EOrmException.CreateUTF8('Unexpected Command=% in %.Execute',
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
        Method;
      amLocked:
        if LockedTimeOut = 0 then
        begin
          Safe.Lock;
          try
            Method;
          finally
            Safe.UnLock;
          end;
        end
        else
        begin
          Start64 := GetTickCount64;
          repeat
            if Safe.TryLock then
            try
              Method;
            finally
              Safe.UnLock;
            end;
            if GetTickCount64 > Start64 + LockedTimeOut then
              break; // wait up to 2 second by default
            SleepHiRes(1); // retry every 1 ms
          until Server.fShutdownRequested;
          TimeOut;
        end;
      amMainThread:
        BackgroundExecuteThreadMethod(Method, nil);
      amBackgroundThread, amBackgroundORMSharedThread:
        begin
          if Thread = nil then
            Thread := Server.Run.NewBackgroundThreadMethod('% % %',
              [self, Server.fModel.Root, ToText(Command)^]);
          BackgroundExecuteThreadMethod(Method, Thread);
        end;
    end;
end;

procedure TRestServerURIContext.ConfigurationRestMethod(SettingsStorage: TObject);
var
  value: TDocVariantData;
  valid: boolean;
  config: variant;
begin
  URIBlobFieldName := StringReplaceChars(URIBlobFieldName, '/', '.');
  if InputExists['value'] then
  begin
    if URIBlobFieldName = '' then
      exit;
    value.InitObjectFromPath(URIBlobFieldName, Input['value']);
    JsonToObject(SettingsStorage, pointer(value.ToJSON), valid, nil,
      JSONPARSER_TOLERANTOPTIONS);
    if not valid then
    begin
      Error('Invalid input [%] - expected %', [variant(value),
        ClassFieldNamesAllPropsAsText(SettingsStorage.ClassType, true)]);
      exit;
    end;
  end;
  ObjectToVariant(SettingsStorage, config, [woDontStoreDefault]);
  if URIBlobFieldName <> '' then
    config := TDocVariantData(config).GetValueByPath(URIBlobFieldName);
  ReturnsJson(config, HTTP_SUCCESS, true, twJsonEscape, true);
end;

procedure StatsAddSizeForCall(Stats: TSynMonitorInputOutput;
  const Call: TRestURIParams);
begin
  Stats.AddSize( // rough estimation
    length(Call.Url) + length(Call.Method) + length(Call.InHead) +
    length(Call.InBody) + 12, length(Call.OutHead) + length(Call.OutBody) + 16);
end;

procedure TRestServerURIContext.StatsFromContext(Stats: TSynMonitorInputOutput;
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

procedure TRestServerURIContext.ExecuteCallback(var Par: PUTF8Char;
  ParamInterfaceInfo: TRttiJson; out Obj);
var
  FakeID: PtrInt;
begin
  if not Assigned(Server.OnNotifyCallback) then
    raise EServiceException.CreateUTF8(
      '% does not implement callbacks for I%',
      [Server,ParamInterfaceInfo.Name]);
  FakeID := GetInteger(GetJSONField(Par, Par)); // GetInteger returns a PtrInt
  if Par = nil then
    Par := @NULCHAR; // allow e.g. '[12345]'
  if (FakeID=0) or
     (ParamInterfaceInfo=TypeInfo(IInvokable)) then
  begin
    pointer(Obj) := pointer(FakeID); // Obj = IInvokable(FakeID)
    exit;
  end;
  (Server.Services as TServiceContainerServer).GetFakeCallback(
    self, ParamInterfaceInfo.Info, FakeID, Obj);
end;

procedure TRestServerURIContext.ExecuteSOAByMethod;
var
  timeStart, timeEnd: Int64;
  sessionstat: TSynMonitorInputOutput;
begin
  with Server.fPublishedMethod[MethodIndex] do
  begin
    if mlMethods in Server.fStatLevels then
    begin
      QueryPerformanceMicroSeconds(timeStart);
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
    if Parameters <> '' then
      Server.InternalLog('% %', [Name, Parameters], sllServiceCall);
    CallBack(self);
    if Stats <> nil then
    begin
      QueryPerformanceMicroSeconds(timeEnd);
      dec(timeEnd, timeStart);
      StatsFromContext(Stats, timeEnd, false);
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
        StatsFromContext(sessionstat, timeEnd, true);
        // mlSessions stats are not yet tracked per Client
      end;
    end;
  end;
  with Server.fStats do
  begin
    // try...finally not mandatory (slower)
    fSafe^.Lock;
    inc(fServiceMethod); // TRestServerMonitor.Changed method is void
    fSafe^.UnLock;
  end;
end;

procedure TRestServerURIContext.ServiceResultStart(WR: TTextWriter);
const
  JSONSTART: array[boolean] of RawUTF8 = ('{"result":[', '{"result":{');
begin
  // InternalExecuteSOAByInterface has set ForceServiceResultAsJSONObject
  if ForceServiceResultAsJSONObjectWithoutResult then
    WR.Add('{')
  else
    WR.AddString(JSONSTART[ForceServiceResultAsJSONObject]);
end;

procedure TRestServerURIContext.ServiceResultEnd(WR: TTextWriter; ID: TID);
const
  JSONSEND_WITHID: array[boolean] of RawUTF8 = (
    '],"id":', '},"id":');
  JSONSEND_NOID: array[boolean] of AnsiChar = (
    ']', '}');
begin
  // InternalExecuteSOAByInterface has set ForceServiceResultAsJSONObject
  if ID = 0 then
    WR.Add(JSONSEND_NOID[ForceServiceResultAsJSONObject])
  else
  begin
    if ForceServiceResultAsJSONObjectWithoutResult then
      raise EServiceException.CreateUTF8('%.ServiceResultEnd(ID=%) with ' +
        'ForceServiceResultAsJSONObjectWithoutResult', [self, ID]);
    WR.AddString(JSONSEND_WITHID[ForceServiceResultAsJSONObject]);
    WR.Add(ID); // only used in sicClientDriven mode
  end;
  if not ForceServiceResultAsJSONObjectWithoutResult then
    WR.Add('}');
end;

procedure TRestServerURIContext.InternalExecuteSOAByInterface;

  procedure ComputeResult;

    procedure ServiceResult(const Name, JSONValue: RawUTF8);
    var
      WR: TTextWriter;
      temp: TTextWriterStackBuffer;
    begin
      WR := TJSONSerializer.CreateOwnedStream(temp);
      try
        ServiceResultStart(WR);
        if ForceServiceResultAsJSONObject then
          WR.AddFieldName(Name);
        WR.AddString(JSONValue);
        ServiceResultEnd(WR, 0);
        Returns(WR.Text);
      finally
        WR.Free;
      end;
    end;

  begin
    with TServiceFactoryServer(Service) do
    begin
      // XML needs a full JSON object as input
      ForceServiceResultAsXMLObject := ForceServiceResultAsXMLObject or
                                       ResultAsXMLObject;
      ForceServiceResultAsJSONObject := ForceServiceResultAsJSONObject or
                                        ResultAsJSONObject or
                                        ResultAsJSONObjectWithoutResult or
                                        ForceServiceResultAsXMLObject;
      ForceServiceResultAsJSONObjectWithoutResult :=
        ForceServiceResultAsJSONObject and
        (InstanceCreation in SERVICE_IMPLEMENTATION_NOID) and
        ResultAsJSONObjectWithoutResult;
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
            Server.AssociatedServices.RegisterFromClientJSON(Call^.InBody);
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
        raise EServiceException.CreateUTF8('%.InternalExecuteSOAByInterface: ' +
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
  xml: RawUTF8;
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
    if (sllServiceCall in Log.GenericFamily.Level) and
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
    JSONBufferToXML(pointer(Call.OutBody), XMLUTF8_HEADER,
      ForceServiceResultAsXMLObjectNameSpace, xml);
    Call.OutBody := xml;
  end;
end;

function TRestServerURIContext.CanExecuteORMWrite(Method: TURIMethod;
  Table: TOrmClass; TableIndex: integer; const TableID: TID;
  const Rights: TOrmAccessRights): boolean;
begin
  result := true;
  case Method of
    mPOST:
      // POST=ADD=INSERT
      if Table <> nil then
        // ExecuteORMWrite will check reSQL access right
        result := (TableIndex in Rights.POST);
    mPUT:
      // PUT=UPDATE
      result := (Table <> nil) and
        ((TableIndex in Rights.PUT) or
         ((TableID > 0) and
          (Session > CONST_AUTHENTICATION_NOT_USED) and
          (Table = Server.fSQLAuthUserClass) and
          (TableID = SessionUser) and
          (reUserCanChangeOwnPassword in Rights.AllowRemoteExecute)));
    mDelete:
      result := (Table <> nil) and
        (TableIndex in Rights.DELETE) and
        ((TableID > 0) or
         (reUrlEncodedDelete in Rights.AllowRemoteExecute));
  end;
end;

procedure TRestServerURIContext.ExecuteORMGet;

  procedure ConvertOutBodyAsPlainJSON(const FieldsCSV: RawUTF8;
    Options: TJSONSerializerOrmOptions);
  var
    rec: TOrm;
    W: TJSONSerializer;
    bits: TFieldBits;
    withid: boolean;
  begin
    // force plain standard JSON output for AJAX clients
    if (FieldsCSV = '') or
       // handle ID single field only if ID_str is needed
       (IsRowID(pointer(FieldsCSV)) and
        not (jwoID_str in Options)) or
       // we won't handle min()/max() functions
       not TableModelProps.Props.FieldBitsFromCSV(FieldsCSV, bits, withid) then
      exit;
    rec := Table.CreateAndFillPrepare(Call.OutBody);
    try
      W := TableModelProps.Props.CreateJSONWriter(
        TRawByteStringStream.Create, true, FieldsCSV, {knownrows=}0);
      try
        W.CustomOptions := W.CustomOptions + [twoForceJSONStandard]; // regular JSON
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
  SQLSelect, SQLWhere, SQLWhereCount, SQLSort, SQLDir, SQL: RawUTF8;
  SQLStartIndex, SQLResults, SQLTotalRowsCount: integer;
  NonStandardSQLSelectParameter, NonStandardSQLWhereParameter: boolean;
  SQLisSelect: boolean;
  ResultList: TOrmTable;
  TableIndexes: TIntegerDynArray;
  rec: TOrm;
  opt: TJSONSerializerOrmOptions;
  P: PUTF8Char;
  i, j, L: PtrInt;
  cache: TRestCache;
  Blob: PRttiProp;
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
               (reUrlEncodedSQL in Call.RestAccessRights^.AllowRemoteExecute) then
            begin
              // GET with a SQL statement sent in URI, as sql=....
              while not UrlDecodeValue(Parameters, 'SQL=', SQL, @Parameters) do
                if Parameters = nil then
                  break;
            end
            else
              // GET with a SQL statement sent as UTF-8 body (not 100% HTTP compatible)
              SQL := Call.InBody;
            if SQL <> '' then
            begin
              SQLisSelect := isSelect(pointer(SQL), @SQLSelect);
              if SQLisSelect or
                 (reSQL in Call.RestAccessRights^.AllowRemoteExecute) then
              begin
                StaticOrm := nil;
                if SQLisSelect then
                begin
                  TableIndexes := Server.fModel.GetTableIndexesFromSQLSelect(SQL);
                  if TableIndexes = nil then
                  begin
                    // check for SELECT without any known table
                    if not (reSQLSelectWithoutTable in
                        Call.RestAccessRights^.AllowRemoteExecute) then
                    begin
                      Call.OutStatus := HTTP_NOTALLOWED;
                      exit;
                    end;
                  end
                  else
                  begin
                    // check for SELECT with one (or several JOINed) tables
                    for i := 0 to high(TableIndexes) do
                      if not (TableIndexes[i] in Call.RestAccessRights^.GET) then
                      begin
                        Call.OutStatus := HTTP_NOTALLOWED;
                        exit;
                      end;
                    // use the first static table (poorman's JOIN)
                    StaticOrm := TRestOrmServer(Server.fOrmInstance).
                      InternalAdaptSQL(TableIndexes[0], SQL);
                  end;
                end;
                if StaticOrm <> nil then
                begin
                  TableEngine := StaticOrm;
                  Call.OutBody := TableEngine.EngineList(SQL);
                end
                else
                  Call.OutBody := TRestOrmServer(Server.fOrmInstance).
                    MainEngineList(SQL, false, nil);
                // security note: only first statement is run by EngineList()
                if Call.OutBody <> '' then
                begin
                  // got JSON list '[{...}]' ?
                  if (SQLSelect <> '') and
                     (length(TableIndexes) = 1) then
                  begin
                    InternalSetTableFromTableIndex(TableIndexes[0]);
                    opt := ClienTOrmOptions;
                    if opt <> [] then
                      ConvertOutBodyAsPlainJSON(SQLSelect, opt);
                  end;
                  Call.OutStatus := HTTP_SUCCESS;  // 200 OK
                  if not SQLisSelect then
                   // needed for fStats.NotifyORM(Method) below
                    Method := TURIMethod(IdemPCharArray(SQLBegin(pointer(SQL)),
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
            // with or w/out locking, or a specified BLOB field content
            if Method = mLOCK then
              // LOCK is to be followed by PUT -> check user
              if not (TableIndex in Call.RestAccessRights^.PUT) then
                Call.OutStatus := HTTP_NOTALLOWED
              else if Server.fModel.Lock(TableIndex, TableID) then
                Method := mGET; // mark successfully locked
            if Method <> mLOCK then
              if URIBlobFieldName <> '' then
              begin
                // GET ModelRoot/TableName/TableID/BlobFieldName: retrieve BLOB field content
                Blob := Table.RecordProps.BlobFieldPropFromRawUTF8(URIBlobFieldName);
                if Blob <> nil then
                begin
                  if TableEngine.EngineRetrieveBlob(TableIndex, TableID, Blob,
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
                    Call.OutBody := TRestOrmServer(Server.fOrmInstance).MainEngineRetrieve
                      (TableIndex, TableID);
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
                  opt := ClienTOrmOptions;
                  if opt <> [] then
                  begin
                    // cached? -> make private
                    rec := Table.CreateFrom(Call.OutBody);
                    try
                      Call.OutBody := rec.GetJSONValues(true, true, ooSelect, nil, opt);
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
            SQLSelect := 'RowID'; // if no select is specified (i.e. ModelRoot/TableName)
            // all IDs of this table are returned to the client
            SQLTotalRowsCount := 0;
            if Parameters <> nil then
            begin
              // '?select=...&where=...' or '?where=...'
              SQLStartIndex := 0;
              SQLResults := 0;
              if Parameters^ <> #0 then
                with Server.URIPagingParameters do
                begin
                  NonStandardSQLSelectParameter :=
                    Select <> PAGINGPARAMETERS_YAHOO.Select;
                  NonStandardSQLWhereParameter :=
                    Where <> PAGINGPARAMETERS_YAHOO.Where;
                  repeat
                    UrlDecodeValue(Parameters, Sort, SQLSort);
                    UrlDecodeValue(Parameters, Dir, SQLDir);
                    UrlDecodeInteger(Parameters, StartIndex, SQLStartIndex);
                    UrlDecodeInteger(Parameters, Results, SQLResults);
                    UrlDecodeValue(Parameters, Select, SQLSelect);
                    if NonStandardSQLSelectParameter and
                       (SQLSelect = '') then
                      UrlDecodeValue(Parameters, PAGINGPARAMETERS_YAHOO.Select, SQLSelect);
                    if NonStandardSQLWhereParameter and
                       ({%H-}SQLWhere = '') then
                      UrlDecodeValue(Parameters, PAGINGPARAMETERS_YAHOO.Where, SQLWhere);
                    UrlDecodeValue(Parameters, Server.URIPagingParameters.Where,
                      SQLWhere, @Parameters);
                  until Parameters = nil;
                end;
              // let SQLite3 do the sort and the paging (will be ignored by Static)
              SQLWhereCount := SQLWhere; // "select count(*)" won't expect any ORDER
              if (SQLSort <> '') and
                 (StrPosI('ORDER BY ', pointer(SQLWhere)) = nil) then
              begin
                if SameTextU(SQLDir, 'DESC') then
                  // allow DESC, default is ASC
                  SQLSort := SQLSort + ' DESC';
                SQLWhere := SQLWhere + ' ORDER BY ' + SQLSort;
              end;
              SQLWhere := trim(SQLWhere);
              if (SQLResults <> 0) and
                 (StrPosI('LIMIT ', pointer(SQLWhere)) = nil) then
              begin
                if Server.URIPagingParameters.SendTotalRowsCountFmt <> '' then
                begin
                  if SQLWhere = SQLWhereCount then
                  begin
                    i := PosEx('ORDER BY ', UpperCase(SQLWhereCount));
                    if i > 0 then
                      // if ORDER BY already in the SQLWhere clause
                      SetLength(SQLWhereCount, i - 1);
                  end;
                  ResultList := TRestOrmServer(Server.fOrmInstance).ExecuteList([Table],
                    Server.fModel.TableProps[TableIndex].SQLFromSelectWhere('Count(*)',
                    SQLWhereCount));
                  if ResultList <> nil then
                  try
                    SQLTotalRowsCount := ResultList.GetAsInteger(1, 0);
                  finally
                    ResultList.Free;
                  end;
                end;
                SQLWhere := FormatUTF8('% LIMIT % OFFSET %', [SQLWhere,
                  SQLResults, SQLStartIndex]);
              end;
            end;
            SQL := Server.fModel.TableProps[TableIndex].SQLFromSelectWhere(
              SQLSelect, trim(SQLWhere));
            Call.OutBody := TRestOrmServer(Server.fOrmInstance).
              InternalListRawUTF8(TableIndex, SQL);
            if Call.OutBody <> '' then
            begin
              // got JSON list '[{...}]' ?
              opt := ClienTOrmOptions;
              if opt <> [] then
                ConvertOutBodyAsPlainJSON(SQLSelect, opt);
              Call.OutStatus := HTTP_SUCCESS;  // 200 OK
              if Server.URIPagingParameters.SendTotalRowsCountFmt <> '' then
                // insert "totalRows":% optional value to the JSON output
                if (rsoNoAJAXJSON in Server.Options) or
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
                    Insert(FormatUTF8(Server.URIPagingParameters.SendTotalRowsCountFmt,
                      [SQLTotalRowsCount]), Call.OutBody, j);
                end
                else
                begin
                  // expanded format -> as {"values":[...],"total":n}
                  if SQLTotalRowsCount = 0 then // avoid sending fields array
                    Call.OutBody := '[]'
                  else
                    Call.OutBody := trim(Call.OutBody);
                  Call.OutBody := '{"values":' + Call.OutBody +
                    FormatUTF8(Server.URIPagingParameters.SendTotalRowsCountFmt,
                     [SQLTotalRowsCount]) + '}';
                end;
            end
            else
              Call.OutStatus := HTTP_NOTFOUND;
          end;
        end;
        if Call.OutStatus = HTTP_SUCCESS then
          Server.fStats.NotifyORM(Method);
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
    raise EOrmException.CreateUTF8('%.ExecuteORMGet(method=%)',
      [self, ord(Method)]);
  end;
end;

procedure TRestServerURIContext.ExecuteORMWrite;

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
      Call.Inbody := Rec.GetJSONValues(true, Rec.IDValue <> 0, bits);
    finally
      Rec.Free;
    end;
  end;

var
  OK: boolean;
  Blob: PRttiProp;
  cache: TRestCache;
  orm: TRestOrmServer;
  SQLSelect, SQLWhere, SQLSort, SQLDir: RawUTF8;
begin
  if MethodIndex = Server.fPublishedMethodBatchIndex then
  begin
    // run the BATCH process in execOrmWrite context
    ExecuteSOAByMethod;
    exit;
  end;
  if not CanExecuteORMWrite(Method, Table, TableIndex, TableID,
      Call.RestAccessRights^) then
  begin
    Call.OutStatus := HTTP_FORBIDDEN;
    exit;
  end;
  orm := Server.fOrmInstance as TRestOrmServer;
  case Method of
    mPOST:
      // POST=ADD=INSERT
      if Table = nil then
      begin
        // ModelRoot with free SQL statement sent as UTF-8 (only for Admin group)
        // see e.g. TRestClientURI.EngineExecute
        if reSQL in Call.RestAccessRights^.AllowRemoteExecute then
          if (Call.InBody <> '') and
             not (GotoNextNotSpace(Pointer(Call.InBody))^ in [#0, '[', '{']) and
             orm.EngineExecute(Call.InBody) then
            Call.OutStatus := HTTP_SUCCESS // 200 OK
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
          FormatUTF8('Location: %/%', [URI, TableID], Call.OutHead);
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
        // PUT ModelRoot/TableName/TableID[/BlobFieldName] to update member/BLOB content
        if orm.RecordCanBeUpdated(Table, TableID, oeUpdate, @CustomErrorMsg) then
        begin
          OK := false;
          if URIBlobFieldName <> '' then
          begin
            // PUT ModelRoot/TableName/TableID/BlobFieldName: update BLOB field content
            Blob := Table.RecordProps.BlobFieldPropFromRawUTF8(URIBlobFieldName);
            if Blob <> nil then
              OK := TableEngine.EngineUpdateBlob(TableIndex, TableID, Blob, Call.InBody);
          end
          else
          begin
            // ModelRoot/TableName/TableID with JSON SentData: update a member
            if rsoComputeFieldsBeforeWriteOnServerSide in Server.Options then
              ComputeInBodyFields(oeUpdate);
            OK := TableEngine.EngineUpdate(TableIndex, TableID, Call.InBody);
            if OK then
            begin
              // flush cache after update (no CreateTime in JSON)
              orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
              if rsoAddUpdateReturnsContent in Server.Options then
                Call.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            end;
          end;
          if OK then
            Call.OutStatus := HTTP_SUCCESS; // 200 OK
        end
        else
          Call.OutStatus := HTTP_FORBIDDEN;
      end
      else if Parameters <> nil then
      begin
        // e.g. from TRestClient.EngineUpdateField
        // PUT ModelRoot/TableName?setname=..&set=..&wherename=..&where=..
        repeat
          UrlDecodeValue(Parameters, 'SETNAME=', SQLSelect);
          UrlDecodeValue(Parameters, 'SET=', SQLDir);
          UrlDecodeValue(Parameters, 'WHERENAME=', SQLSort);
          UrlDecodeValue(Parameters, 'WHERE=', SQLWhere, @Parameters);
        until Parameters = nil;
        if (SQLSelect <> '') and
           (SQLDir <> '') and
           (SQLSort <> '') and
           (SQLWhere <> '') then
          if TableEngine.EngineUpdateField(TableIndex, SQLSelect, SQLDir,
            SQLSort, SQLWhere) then
          begin
            if rsoAddUpdateReturnsContent in Server.Options then
              Call.OutBody := TableEngine.EngineRetrieve(TableIndex, TableID);
            Call.OutStatus := HTTP_SUCCESS; // 200 OK
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
            Call.OutStatus := HTTP_SUCCESS; // 200 OK
            orm.CacheOrNil.NotifyDeletion(TableIndex, TableID);
          end;
        end
      else if Parameters <> nil then
      begin
        // ModelRoot/TableName?where=WhereClause to delete members
        repeat
          if UrlDecodeValue(Parameters, 'WHERE=', SQLWhere, @Parameters) then
          begin
            SQLWhere := trim(SQLWhere);
            if SQLWhere <> '' then
            begin
              if orm.Delete(Table, SQLWhere) then
                Call.OutStatus := HTTP_SUCCESS; // 200 OK
            end;
            break;
          end;
        until Parameters = nil;
      end;
    mBEGIN:
      begin
      // BEGIN TRANSACTION
      // TOrmVirtualTableJSON/External will rely on SQLite3 module
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
          Call.OutStatus := HTTP_SUCCESS; // 200 OK
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
        Call.OutStatus := HTTP_SUCCESS; // 200 OK
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
        Call.OutStatus := HTTP_SUCCESS; // 200 OK
      end;
  end;
  if StatusCodeIsSuccess(Call.OutStatus) then
    Server.fStats.NotifyORM(Method);
end;

procedure TRestServerURIContext.FillInput(const LogInputIdent: RawUTF8);
var
  n, max: PtrInt;
  P: PUTF8Char;
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
        raise EParsingException.CreateUTF8(
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
  SetLength(fInput, n);
  if LogInputIdent <> '' then
    Log.Add.Log(sllDebug, LogInputIdent, TypeInfo(TRawUTF8DynArray), fInput, self);
end;

function TRestServerURIContext.GetInputInt(const ParamName: RawUTF8): Int64;
var
  err: integer;
  v: RawUTF8;
begin
  GetInputByName(ParamName, 'Int', v);
  result := GetInt64(pointer(v), err);
  if err <> 0 then
    raise EParsingException.CreateUTF8('%.InputInt[%]: ''%'' is not an integer',
      [self, ParamName, v]);
end;

function TRestServerURIContext.GetInputDouble(const ParamName: RawUTF8): Double;
var
  err: integer;
  v: RawUTF8;
begin
  GetInputByName(ParamName, 'Double', v);
  result := GetExtended(pointer(v), err);
  if err <> 0 then
    raise EParsingException.CreateUTF8('%.InputDouble[%]: ''%'' is not a float',
      [self, ParamName, v]);
end;

function TRestServerURIContext.GetInputIntOrVoid(const ParamName: RawUTF8): Int64;
begin
  result := GetInt64(pointer(GetInputUTF8OrVoid(ParamName)));
end;

function TRestServerURIContext.GetInputHexaOrVoid(const ParamName: RawUTF8): cardinal;
var
  value: RawUTF8;
begin
  value := GetInputUTF8OrVoid(ParamName);
  if (length(value) <> 8) or
     not HexDisplayToCardinal(Pointer(value), result) then
    result := 0;
end;

function TRestServerURIContext.GetInputDoubleOrVoid(const ParamName: RawUTF8): Double;
begin
  result := GetExtended(pointer(GetInputUTF8OrVoid(ParamName)));
end;

function TRestServerURIContext.GetInputNameIndex(const ParamName: RawUTF8): PtrInt;
begin
  // fInput[0]='Param1',fInput[1]='Value1',fInput[2]='Param2'...
  if (fInput = nil) and
     (Parameters <> nil) then
    FillInput;
  for result := 0 to (length(fInput) shr 1) - 1 do
    if IdemPropNameU(ParamName, fInput[result * 2]) then
      exit;
  result := -1;
end;

procedure TRestServerURIContext.GetInputByName(
  const ParamName, InputName: RawUTF8; var result: RawUTF8);
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    raise EParsingException.CreateUTF8('%: missing Input%[%]',
      [self, InputName, ParamName]);
  result := fInput[i * 2 + 1];
end;

function TRestServerURIContext.GetInputUTF8(const ParamName: RawUTF8): RawUTF8;
begin
  GetInputByName(ParamName, 'UTF8', result{%H-});
end;

function TRestServerURIContext.GetInputUTF8OrVoid(
  const ParamName: RawUTF8): RawUTF8;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := ''
  else
    result := fInput[i * 2 + 1];
end;

function TRestServerURIContext.InputUTF8OrDefault(
  const ParamName, DefaultValue: RawUTF8): RawUTF8;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := DefaultValue
  else
    result := fInput[i * 2 + 1];
end;

function TRestServerURIContext.InputUTF8OrError(const ParamName: RawUTF8;
  out Value: RawUTF8; const ErrorMessageForMissingParameter: string): boolean;
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

function TRestServerURIContext.InputEnum(const ParamName: RawUTF8;
  EnumType: PRttiInfo; out ValueEnum; DefaultEnumOrd: integer): boolean;
var
  value: RawUTF8;
  int, err: integer;
begin
  result := false;
  if (EnumType = nil) or
     (EnumType^.Kind <> rkEnumeration) then
    exit;
  value := GetInputUTF8OrVoid(ParamName);
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

function TRestServerURIContext.GetInputString(const ParamName: RawUTF8): string;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    raise EParsingException.CreateUTF8('%: missing InputString[%]',
      [self, ParamName]);
  result := UTF8ToString(fInput[i * 2 + 1]);
end;

function TRestServerURIContext.GetInputStringOrVoid(
  const ParamName: RawUTF8): string;
var
  i: PtrInt;
begin
  i := GetInputNameIndex(ParamName);
  if i < 0 then
    result := ''
  else
    result := UTF8ToString(fInput[i * 2 + 1]);
end;

function TRestServerURIContext.GetInputExists(const ParamName: RawUTF8): boolean;
begin
  result := GetInputNameIndex(ParamName) >= 0;
end;

function TRestServerURIContext.GetInput(const ParamName: RawUTF8): variant;
var
  v: RawUTF8;
begin
  GetInputByName(ParamName, '', v);
  GetVariantFromJSON(pointer(v), false, result{%H-});
end;

function TRestServerURIContext.GetInputOrVoid(const ParamName: RawUTF8): variant;
begin
  GetVariantFromJSON(pointer(GetInputUTF8OrVoid(ParamName)), false, result{%H-});
end;

function TRestServerURIContext.InputOrError(const ParamName: RawUTF8;
  out Value: variant; const ErrorMessageForMissingParameter: string): boolean;
var
  v: RawUTF8;
begin
  result := InputUTF8OrError(ParamName, v, ErrorMessageForMissingParameter);
  if result then
    GetVariantFromJSON(pointer(v), false, Value);
end;

function TRestServerURIContext.GetInputAsTDocVariant(
  const Options: TDocVariantOptions; InterfaceMethod: pointer): variant;
var
  ndx, a: PtrInt;
  forcestring: boolean;
  v: variant;
  MultiPart: TMultiPartDynArray;
  name: RawUTF8;
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
      GetVariantFromJSON(pointer(fInput[ndx * 2 + 1]), forcestring, v, @Options);
      res.AddValue(name, v);
    end;
  end
  else if InputAsMultiPart(MultiPart) then
  begin
    res.Init(Options, dvObject);
    for ndx := 0 to high(MultiPart) do
      with MultiPart[ndx] do
        if ContentType = TEXT_CONTENT_TYPE then
        begin
          // append as regular "Name":"TextValue" field
          RawUTF8ToVariant(Content, v);
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

function TRestServerURIContext.InputAsMultiPart(
  var MultiPart: TMultiPartDynArray): boolean;
begin
  result := (Method = mPOST) and
     IdemPChar(pointer(fInputPostContentType), 'MULTIPART/FORM-DATA') and
     MultiPartFormDataDecode(fInputPostContentType, Call^.InBody, MultiPart);
end;

function TRestServerURIContext.GetInHeader(const HeaderName: RawUTF8): RawUTF8;
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

procedure TRestServerURIContext.RetrieveCookies;
var
  n: PtrInt;
  P: PUTF8Char;
  cookie, cn, cv: RawUTF8;
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
    fInputCookies[n].value := cv;
    inc(n);
    if n > COOKIE_MAXCOUNT_DOSATTACK then
      raise EParsingException.CreateUTF8(
        '%.RetrieveCookies overflow: DOS?', [self]);
  end;
end;

procedure TRestServerURIContext.SetInCookie(CookieName, CookieValue: RawUTF8);
var
  i, n: PtrInt;
begin
  CookieName := trim(CookieName);
  if (self = nil) or
     (CookieName = '') then
    exit;
  if not fInputCookiesRetrieved then
    RetrieveCookies;
  n := length(fInputCookies);
  for i := 0 to n - 1 do
    if fInputCookies[i].Name = CookieName then
    begin // cookies are case-sensitive
      fInputCookies[i].value := CookieValue; // in-place update
      exit;
    end;
  SetLength(fInputCookies, n + 1);
  fInputCookies[n].Name := CookieName;
  fInputCookies[n].value := CookieValue;
end;

function TRestServerURIContext.GetInCookie(CookieName: RawUTF8): RawUTF8;
var
  i: PtrInt;
begin
  result := '';
  CookieName := trim(CookieName);
  if (self = nil) or
     (CookieName = '') then
    exit;
  if not fInputCookiesRetrieved then
    RetrieveCookies;
  for i := 0 to length(fInputCookies) - 1 do
    if fInputCookies[i].Name = CookieName then
    begin
      // cookies are case-sensitive
      result := fInputCookies[i].value;
      exit;
    end;
end;

procedure TRestServerURIContext.SetOutSetCookie(aOutSetCookie: RawUTF8);
const
  HTTPONLY: array[boolean] of RawUTF8 = (
    '; HttpOnly', '');
begin
  if self = nil then
    exit;
  aOutSetCookie := Trim(aOutSetCookie);
  if not IsValidUTF8WithoutControlChars(aOutSetCookie) then
    raise EParsingException.CreateUTF8('Unsafe %.SetOutSetCookie', [self]);
  if PosExChar('=', aOutSetCookie) < 2 then
    raise EParsingException.CreateUTF8(
      '"name=value" expected for %.SetOutSetCookie("%")', [self, aOutSetCookie]);
  if StrPosI('; PATH=', pointer(aOutSetCookie)) = nil then
    FormatUTF8('%; Path=/%%', [aOutSetCookie, Server.fModel.Root,
      HTTPONLY[rsoCookieHttpOnlyFlagDisable in Server.fOptions]], fOutSetCookie)
  else
    fOutSetCookie := aOutSetCookie;
end;

function TRestServerURIContext.GetUserAgent: RawUTF8;
begin
  result := Call^.HeaderOnce(fUserAgent, 'USER-AGENT: ');
end;

function TRestServerURIContext.GetRemoteIP: RawUTF8;
begin
  result := Call^.HeaderOnce(fRemoteIP, HEADER_REMOTEIP_UPPER);
end;

function TRestServerURIContext.GetRemoteIPNotLocal: RawUTF8;
begin
  result := Call^.HeaderOnce(fRemoteIP, HEADER_REMOTEIP_UPPER);
  if result = '127.0.0.1' then
    result := '';
end;

function TRestServerURIContext.GetRemoteIPIsLocalHost: boolean;
begin
  result := (GetRemoteIP = '') or
            (fRemoteIP = '127.0.0.1');
end;

function TRestServerURIContext.AuthenticationBearerToken: RawUTF8;
begin
  result := Call^.HeaderOnce(fAuthenticationBearerToken, HEADER_BEARER_UPPER);
  if (result = '') and
     not (rsoAuthenticationURIDisable in Server.Options) then
  begin
    result := GetInputUTF8OrVoid('authenticationbearer');
    if result <> '' then
      fAuthenticationBearerToken := result;
  end;
end;

function TRestServerURIContext.AuthenticationCheck(jwt: TJWTAbstract): boolean;
begin
  if jwt = nil then
    JWTContent.result := jwtNoToken
  else
    jwt.Verify(AuthenticationBearerToken, JWTContent);
  result := JWTContent.result = jwtValid;
  if not result then
    Error('Invalid Bearer [%]', [ToText(JWTContent.result)^], HTTP_FORBIDDEN)
  else if (Server.fIPWhiteJWT <> nil) and
          not Server.fIPWhiteJWT.Exists(RemoteIP) and
          (fRemoteIP <> '') and
          (fRemoteIP <> '127.0.0.1') then
  begin
    Error('Invalid IP [%]', [fRemoteIP], HTTP_FORBIDDEN);
    result := false;
  end;
end;

function TRestServerURIContext.ClientKind: TRestServerURIContextClientKind;
var
  agent: RawUTF8;
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
        // 'mORMot' set e.g. from XPOWEREDPROGRAM value in SynCrtSock
        fClientKind := ckFramework
      else
        fClientKind := ckAjax;
    end;
  result := fClientKind;
end;

function TRestServerURIContext.IsRemoteAdministrationExecute: boolean;
begin
  result := (self <> nil) and
            (Call.RestAccessRights = @BYPASS_ACCESS_RIGHTS);
end;

function TRestServerURIContext.ClienTOrmOptions: TJSONSerializerOrmOptions;
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

function TRestServerURIContext.GetResourceFileName: TFileName;
begin
  if (URIBlobFieldName = '') or
     (PosEx('..', URIBlobFieldName) > 0) then
    // for security, disallow .. in the supplied file path
    result := ''
  else
    result := UTF8ToString(StringReplaceAll(URIBlobFieldName, '/', PathDelim));
end;

procedure TRestServerURIContext.Returns(const result: RawUTF8;
  Status: integer; const CustomHeader: RawUTF8;
  Handle304NotModified, HandleErrorAsRegularResult: boolean;
  CacheControlMaxAge: integer; ServerHash: RawUTF8);
var
  clientHash: RawUTF8;
begin
  if HandleErrorAsRegularResult or StatusCodeIsSuccess(Status) then
  begin
    Call.OutStatus := Status;
    Call.OutBody := result;
    if CustomHeader <> '' then
      Call.OutHead := CustomHeader
    else if Call.OutHead = '' then
      Call.OutHead := JSON_CONTENT_TYPE_HEADER_VAR;
    if CacheControlMaxAge > 0 then
      Call.OutHead := Call.OutHead + #13#10'Cache-Control: max-age=' +
        UInt32ToUtf8(CacheControlMaxAge);
    if Handle304NotModified and
       (Status = HTTP_SUCCESS) and
       (Length(result) > 64) then
    begin
      FindNameValue(Call.InHead, 'IF-NONE-MATCH: ', clientHash);
      if ServerHash = '' then
        ServerHash := '"' + crc32cUTF8ToHex(result) + '"';
      ServerHash := '"' + ServerHash + '"';
      if clientHash <> ServerHash then
        Call.OutHead := Call.OutHead + #13#10'ETag: ' + ServerHash
      else
      begin
        // save bandwidth by returning "304 Not Modified"
        Call.OutBody := '';
        Call.OutStatus := HTTP_NOTMODIFIED;
      end;
    end;
  end
  else
    Error(result, Status);
end;

procedure TRestServerURIContext.Returns(Value: TObject; Status: integer;
  Handle304NotModified: boolean; OrmOptions: TJSONSerializerOrmOptions;
  const CustomHeader: RawUTF8);
var
  json: RawUTF8;
begin
  if Value.InheritsFrom(TOrm) then
    json := TOrm(Value).GetJSONValues(true, true, ooSelect, nil, OrmOptions)
  else
    json := ObjectToJSON(Value);
  Returns(json, Status, CustomHeader, Handle304NotModified);
end;

procedure TRestServerURIContext.ReturnsJson(const Value: variant;
  Status: integer; Handle304NotModified: boolean; Escape: TTextWriterKind;
  MakeHumanReadable: boolean; const CustomHeader: RawUTF8);
var
  json: RawUTF8;
  tmp: TSynTempBuffer;
begin
  VariantSaveJSON(Value, Escape, json);
  if MakeHumanReadable and
     (json <> '') and
     (json[1] in ['{', '[']) then
  begin
    tmp.Init(json);
    try
      JSONBufferReformat(tmp.buf, json);
    finally
      tmp.Done;
    end;
  end;
  Returns(json, Status, CustomHeader, Handle304NotModified);
end;

procedure TRestServerURIContext.ReturnBlob(const Blob: RawByteString;
  Status: integer; Handle304NotModified: boolean; const FileName: TFileName;
  CacheControlMaxAge: integer);
begin
  if not ExistsIniName(pointer(Call.OutHead), HEADER_CONTENT_TYPE_UPPER) then
    AddToCSV(GetMimeContentTypeHeader(Blob, FileName), Call.OutHead, #13#10);
  Returns(Blob, Status, Call.OutHead, Handle304NotModified, false, CacheControlMaxAge);
end;

procedure TRestServerURIContext.ReturnFile(const FileName: TFileName;
  Handle304NotModified: boolean; const ContentType, AttachmentFileName,
  Error404Redirect: RawUTF8; CacheControlMaxAge: integer);
var
  FileTime: TDateTime;
  clientHash, serverHash: RawUTF8;
begin
  if FileName = '' then
    FileTime := 0
  else
    FileTime := FileAgeToDateTime(FileName);
  if FileTime = 0 then
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
      FindNameValue(Call.InHead, 'IF-NONE-MATCH:', clientHash);
      serverHash := '"' + DateTimeToIso8601(FileTime, false, 'T', true) + '"';
      Call.OutHead := Call.OutHead + #13#10'ETag: ' + serverHash;
      if clientHash = serverHash then
      begin
        Call.OutStatus := HTTP_NOTMODIFIED;
        exit;
      end;
    end;
    // Content-Type: appears twice: 1st to notify static file, 2nd for mime type
    Call.OutHead := STATICFILE_CONTENT_TYPE_HEADER + #13#10 + Call.OutHead;
    StringToUTF8(FileName, Call.OutBody); // body=filename for STATICFILE_CONTENT
    if AttachmentFileName <> '' then
      Call.OutHead := Call.OutHead +
        #13#10'Content-Disposition: attachment; filename="' + AttachmentFileName + '"';
  end;
end;

procedure TRestServerURIContext.ReturnFileFromFolder(
  const FolderName: TFileName; Handle304NotModified: boolean;
  const DefaultFileName: TFileName; const Error404Redirect: RawUTF8;
  CacheControlMaxAge: integer);
var
  fileName: TFileName;
begin
  if URIBlobFieldName = '' then
    fileName := DefaultFileName
  else if PosEx('..', URIBlobFieldName) > 0 then
    fileName := ''
  else
    fileName := UTF8ToString(
      StringReplaceChars(URIBlobFieldName, '/', PathDelim));
  if fileName <> '' then
    fileName := IncludeTrailingPathDelimiter(FolderName) + fileName;
  ReturnFile(fileName, Handle304NotModified, '', '', Error404Redirect,
    CacheControlMaxAge);
end;

procedure TRestServerURIContext.Redirect(const NewLocation: RawUTF8;
  PermanentChange: boolean);
begin
  if PermanentChange then
    Call.OutStatus := HTTP_MOVEDPERMANENTLY
  else
    Call.OutStatus := HTTP_TEMPORARYREDIRECT;
  Call.OutHead := 'Location: ' + NewLocation;
end;

procedure TRestServerURIContext.Returns(const NameValuePairs: array of const;
  Status: integer; Handle304NotModified, HandleErrorAsRegularResult: boolean;
  const CustomHeader: RawUTF8);
begin
  Returns(JSONEncode(NameValuePairs), Status, CustomHeader, Handle304NotModified,
    HandleErrorAsRegularResult);
end;

procedure TRestServerURIContext.Results(const Values: array of const;
  Status: integer; Handle304NotModified: boolean; CacheControlMaxAge: integer);
var
  i, h: PtrInt;
  result: RawUTF8;
  temp: TTextWriterStackBuffer;
begin
  h := high(Values);
  if h < 0 then
    result := '{"result":null}'
  else
    with TJSONSerializer.CreateOwnedStream(temp) do
    try
      AddShort('{"result":');
      if h = 0 then
        // result is one value
        AddJSONEscape(Values[0])
      else
      begin
        // result is one array of values
        Add('[');
        i := 0;
        repeat
          AddJSONEscape(Values[i]);
          if i = h then
            break;
          Add(',');
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

procedure TRestServerURIContext.Success(Status: integer);
begin
  if StatusCodeIsSuccess(Status) then
    Call.OutStatus := Status
  else
    Error('', Status);
end;

procedure TRestServerURIContext.Error(const Format: RawUTF8;
  const Args: array of const; Status, CacheControlMaxAge: integer);
var
  msg: RawUTF8;
begin
  FormatUTF8(Format, Args, msg);
  Error(msg, Status, CacheControlMaxAge);
end;

procedure TRestServerURIContext.Error(E: Exception; const Format: RawUTF8;
  const Args: array of const; Status: integer);
var
  msg, exc: RawUTF8;
begin
  FormatUTF8(Format, Args, msg);
  if E = nil then
    Error(msg, Status)
  else
  begin
    exc := ObjectToJSONDebug(E);
    if msg = '' then
      Error('{"%":%}', [E, exc], Status)
    else
      Error(FormatUTF8('{"msg":?,"%":%}', [E, exc], [msg], true), Status);
  end;
end;

procedure TRestServerURIContext.Error(const ErrorMessage: RawUTF8;
  Status, CacheControlMaxAge: integer);
var
  ErrorMsg: RawUTF8;
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
    ErrorMsg := StatusCodeToReason(Status)
  else
    ErrorMsg := ErrorMessage;
  with TTextWriter.CreateOwnedStream(temp) do
  try
    AddShort('{'#13#10'"errorCode":');
    Add(Call.OutStatus);
    if (ErrorMsg <> '') and
       (ErrorMsg[1] = '{') and
       (ErrorMsg[length(ErrorMsg)] = '}') then
    begin
      AddShort(','#13#10'"error":'#13#10);
      AddNoJSONEscape(pointer(ErrorMsg), length(ErrorMsg));
      AddShorter(#13#10'}');
    end
    else
    begin
      AddShort(','#13#10'"errorText":"');
      AddJSONEscape(pointer(ErrorMsg));
      AddShorter('"'#13#10'}');
    end;
    SetText(Call.OutBody);
  finally
    Free;
  end;
  Server.InternalLog('%.Error: %', [ClassType, Call.OutBody], sllDebug);
end;



{ ************ TRestServerRoutingREST/TRestServerRoutingJSON_RPC Requests Parsing Scheme }

{ TRestServerRoutingJSON_RPC }

procedure TRestServerRoutingJSON_RPC.URIDecodeSOAByInterface;
var
  i: PtrInt;
  method, clientdrivenid: RawUTF8;
begin
  if (Table = nil) and
     (MethodIndex < 0) and
     (URI <> '') and
     (Server.Services <> nil) then
  begin
    // check URI as '/Model/Interface.Method[/ClientDrivenID]'
    i := Server.Services.InterfaceMethods.FindHashed(URI);
    if i >= 0 then // no specific message: it may be a valid request
      with Server.Services.InterfaceMethod[i] do
      begin
        Service := TServiceFactoryServer(InterfaceService);
        ServiceMethodIndex := InterfaceMethodIndex;
        fServiceListInterfaceMethodIndex := i;
        i := ServiceMethodIndex - SERVICE_PSEUDO_METHOD_COUNT;
        if i >= 0 then
          ServiceMethod := @Service.InterfaceFactory.Methods[i];
        ServiceInstanceID := GetInteger(pointer(URIBlobFieldName));
      end
    else if URIBlobFieldName <> '' then
    begin
      // check URI as '/Model/Interface/Method[/ClientDrivenID]''
      Service := Server.Services[URI];
      if Service <> nil then
      begin
        // identified as a valid JSON-RPC service
        Split(URIBlobFieldName, '/', method, clientdrivenid);
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

procedure TRestServerRoutingJSON_RPC.ExecuteSOAByInterface;
var
  JSON: RawUTF8;

  procedure DecodeUriParametersIntoJSON(const input: TRawUTF8DynArray);
  var
    a, i, iLow: PtrInt;
    WR: TTextWriter;
    argDone: boolean;
    temp: TTextWriterStackBuffer;
  begin
    WR := TJSONSerializer.CreateOwnedStream(temp);
    try // convert URI parameters into the expected ordered JSON array
      WR.Add('[');
      with PInterfaceMethod(ServiceMethod)^ do
      begin
        iLow := 0;
        for a := ArgsInFirst to ArgsInLast do
          with Args[a] do
            if ValueDirection <> imdOut then
            begin
              argDone := false;
              for i := iLow to high(input) shr 1 do // search argument in URI
                if IdemPropNameU(input[i * 2], @ParamName^[1], ord(ParamName^[0])) then
                begin
                  AddValueJSON(WR, input[i * 2 + 1]); // will add "" if needed
                  if i = iLow then
                    inc(iLow); // optimistic in-order search, but allow any order
                  argDone := true;
                  break;
                end;
              if not argDone then
                AddDefaultJSON(WR); // allow missing argument (and add ',')
            end;
      end;
      WR.CancelLastComma;
      WR.Add(']');
      WR.SetText(JSON);
    finally
      WR.Free;
    end;
  end;

var
  Par: PUTF8Char;
begin // here Ctxt.Service and ServiceMethod(Index) are set
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUTF8(
      '%.ExecuteSOAByInterface invalid call', [self]);
  //  URI as '/Model/Interface.Method[/ClientDrivenID]'
  if Call.InBody <> '' then
    // parameters sent as JSON array/object (the Delphi/AJAX way) or single blob
    if (ServiceMethod <> nil) and
       PInterfaceMethod(ServiceMethod)^.ArgsInputIsOctetStream and
       not Call.InBodyTypeIsJson then
    begin
      JSON := BinToBase64(Call.InBody, '["', '"]', false);
      ServiceParameters := pointer(JSON); // as expected by InternalExecuteSOAByInterface
    end
    else
      ServiceParameters := pointer(Call.InBody)
  else
  begin
    // no body -> try URI-encoded parameters (the HTML way)
    Par := Parameters;
    if Par <> nil then
    begin
      while Par^ = '+' do
        inc(Par); // ignore trailing spaces
      if (Par^ = '[') or
         IdemPChar(Par, '%5B') then
        // as JSON array (input is e.g. '+%5B...' for ' [...')
        JSON := UrlDecode(Parameters)
      else
      begin
        // or as a list of parameters (input is 'Param1=Value1&Param2=Value2...')
        FillInput; // fInput[0]='Param1',fInput[1]='Value1',fInput[2]='Param2'...
        if (fInput <> nil) and
           (ServiceMethod <> nil) then
          DecodeUriParametersIntoJSON(fInput);
      end;
    end;
    ServiceParameters := pointer({%H-}JSON);
  end;
  // now Service, ServiceParameters, ServiceMethod(Index) are set
  InternalExecuteSOAByInterface;
end;


{ TRestServerRoutingREST }

procedure TRestServerRoutingREST.URIDecodeSOAByInterface;
begin
  if (Table = nil) and
     (MethodIndex < 0) and
     (URI <> '') and
     (Server.Services <> nil) then
    // URI sent as '/Model/Interface' for JSON-RPC service
    Service := Server.Services[URI];
  // ServiceMethodIndex will be retrieved from "method": in body
end;

procedure TRestServerRoutingREST.ExecuteSOAByInterface;
var
  method: RawUTF8;
  Values: array[0..2] of TValuePUTF8Char;
  m: TServiceInternalMethod;
  tmp: TSynTempBuffer;
begin // here Ctxt.Service is set (not ServiceMethodIndex yet)
  if (Server.Services = nil) or
     (Service = nil) then
    raise EServiceException.CreateUTF8(
      '%.ExecuteSOAByInterface invalid call', [self]);
  tmp.Init(Call.Inbody);
  try
    JSONDecode(tmp.buf, ['method', 'params', 'id'], @Values, true);
    if Values[0].value = nil then // Method name required
      exit;
    Values[0].ToUTF8(method);
    ServiceParameters := Values[1].value;
    ServiceInstanceID := Values[2].ToCardinal; // retrieve "id":ClientDrivenID
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
    InternalExecuteSOAByInterface;
    ServiceParameters := nil;
  finally
    tmp.Done; // release temp storage for Values[] = Service* fields
  end;
end;



{ ************ TAuthSession for In-Memory User Sessions }

{ TAuthSession }

procedure TAuthSession.ComputeProtectedValues;
begin
  // here User.GroupRights and fPrivateKey should have been set
  fTimeOutShr10 := (QWord(User.GroupRights.SessionTimeout) * (1000 * 60)) shr 10;
  fTimeOutTix := GetTickCount64 shr 10 + fTimeOutShr10;
  fAccessRights := User.GroupRights.SQLAccessRights;
  fPrivateSalt := fID + '+' + fPrivateKey;
  fPrivateSaltHash := crc32(crc32(0, pointer(fPrivateSalt), length(fPrivateSalt)),
    pointer(User.PasswordHashHexa), length(User.PasswordHashHexa));
end;

constructor TAuthSession.Create(aCtxt: TRestServerURIContext; aUser: TAuthUser);
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
      aCtxt.Server.fSQLAuthGroupClass.Create(aCtxt.Server.ORM, GID);
    if User.GroupRights.IDValue <> 0 then
    begin
      // compute the next Session ID
      with aCtxt.Server do
      begin
        if fSessionCounter >= cardinal(maxInt) then
          fSessionCounter := 10
        else if fSessionCounter = 75 then
          // avoid IDCardinal=0 (77) or 1 (76)
          fSessionCounter := 78
        else
          inc(fSessionCounter);
        fIDCardinal := fSessionCounter xor 77;
        UInt32ToUtf8(fIDCardinal, fID);
      end;
      // set session parameters
      TAESPRNG.Main.Fill(@rnd, SizeOf(rnd));
      fPrivateKey := BinToHex(@rnd, SizeOf(rnd));
      if not (rsoGetUserRetrieveNoBlobData in aCtxt.Server.Options) then
      begin
        aCtxt.Server.OrmInstance.RetrieveBlob(aCtxt.Server.fSQLAuthUserClass,
          User.IDValue, 'Data', blob);
        User.Data := blob;
      end;
      if (aCtxt.Call <> nil) and
         (aCtxt.Call.InHead <> '') then
        fSentHeaders := aCtxt.Call.InHead;
      ComputeProtectedValues;
      fRemoteIP := aCtxt.RemoteIP;
      aCtxt.Log.Log(sllUserAuth, 'New [%] session %/% created at %/% running %',
        [User.GroupRights.Ident, User.LogonName, fIDCardinal, fRemoteIP,
         aCtxt.Call^.LowLevelConnectionID, aCtxt.GetUserAgent], self);
      exit; // create successfull
    end;
    // on error: set GroupRights back to a pseudo TAuthGroup = ID
    User.GroupRights.Free;
    User.GroupRights := GID;
  end;
  raise ESecurityException.CreateUTF8('Invalid %.Create(%,%)', [self, aCtxt, aUser]);
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

function TAuthSession.GetUserName: RawUTF8;
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
  W.WriteVarUInt32(IDCardinal);
  W.WriteVarUInt32(fUser.IDValue);
  fUser.GetBinaryValues(W); // User.fGroup is a pointer, but will be overriden
  W.WriteVarUInt32(fUser.GroupRights.IDValue);
  fUser.GroupRights.GetBinaryValues(W);
  W.Write(fPrivateKey);
  W.Write(fSentHeaders);
end; // TODO: persist ORM/SOA stats? -> rather integrate them before saving

constructor TAuthSession.CreateFrom(var P: PAnsiChar; PEnd: PAnsiChar;
  Server: TRestServer);

  procedure RaiseError;
  begin
    raise ESynException.CreateUTF8('%.CreateFrom() with invalid format', [self]);
  end;

var
  PB: PByte absolute P;
  i32: cardinal;
begin
  if PB^ = TAUTHSESSION_MAGIC then
    inc(PB)
  else
    RaiseError;
  PB := FromVarUInt32Safe(PB, pointer(PEnd), fIDCardinal);
  if PB = nil then
    RaiseError;
  UInt32ToUtf8(fIDCardinal, fID);
  fUser := Server.SQLAuthUserClass.Create;
  PB := FromVarUInt32Safe(PB, pointer(PEnd), i32);
  if PB = nil then
    RaiseError;
  fUser.IDValue := i32;
  fUser.SetBinaryValues(P, PEnd); // fUser.fGroup will be overriden by true instance
  fUser.GroupRights := Server.SQLAuthGroupClass.Create;
  PB := FromVarUInt32Safe(PB, pointer(PEnd), i32);
  if PB = nil then
    RaiseError;
  fUser.GroupRights.IDValue := i32;
  fUser.GroupRights.SetBinaryValues(P, PEnd);
  fPrivateKey := FromVarString(PB, pointer(PEnd));
  fSentHeaders := FromVarString(PB, pointer(PEnd));
  if PB = nil then
    RaiseError;
  ComputeProtectedValues;
  FindNameValue(fSentHeaders, HEADER_REMOTEIP_UPPER, fRemoteIP);
end;



{ ************ TRestServerAuthentication Implementing Authentication Schemes }

{ TRestServerAuthentication }

constructor TRestServerAuthentication.Create(aServer: TRestServer);
begin
  fServer := aServer;
  fOptions := [saoUserByLogonOrID];
end;

function TRestServerAuthentication.AuthSessionRelease(
  Ctxt: TRestServerURIContext): boolean;
var
  aUserName: RawUTF8;
  aSessionID: cardinal;
  i: PtrInt;
begin
  result := false;
  if fServer.fSessions = nil then
    exit;
  aUserName := Ctxt.InputUTF8OrVoid['UserName'];
  if aUserName = '' then
    exit;
  aSessionID := Ctxt.InputIntOrVoid['Session'];
  if aSessionID = 0 then
    aSessionID := Ctxt.InputHexaOrVoid['SessionHex'];
  if aSessionID = 0 then
    exit;
  result := true; // recognized GET ModelRoot/auth?UserName=...&Session=...
  // allow only to delete its own session - ticket [7723fa7ebd]
  if aSessionID = Ctxt.Session then
    for i := 0 to fServer.fSessions.Count - 1 do
      with TAuthSession(fServer.fSessions.List[i]) do
        if (fIDCardinal = aSessionID) and
           (fUser.LogonName = aUserName) then
        begin
          Ctxt.fAuthSession := nil; // avoid GPF
          fServer.SessionDelete(i, Ctxt);
          Ctxt.Success;
          break;
        end;
end;

function TRestServerAuthentication.GetUser(Ctxt: TRestServerURIContext;
  const aUserName: RawUTF8): TAuthUser;
var
  UserID: TID;
  err: integer;
begin
  UserID := GetInt64(pointer(aUserName), err);
  if (err <> 0) or
     (UserID <= 0) or
     not (saoUserByLogonOrID in fOptions) then
    UserID := 0;
  if Assigned(fServer.OnAuthenticationUserRetrieve) then
    result := fServer.OnAuthenticationUserRetrieve(self, Ctxt, UserID, aUserName)
  else
  begin
    if UserID <> 0 then
    begin
      // try if TAuthUser.ID was transmitted
      result := fServer.fSQLAuthUserClass.Create(fServer.ORM, UserID); // may use ORM cache :)
      if result.IDValue = 0 then
        FreeAndNil(result);
    end
    else
      result := nil;
    if result = nil then
      result := fServer.fSQLAuthUserClass.Create(
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
      [fServer.fSQLAuthUserClass, aUserName], sllUserAuth);
    FreeAndNil(result);
  end
  else if not result.CanUserLog(Ctxt) then
  begin
    fServer.InternalLog('%.CanUserLog(%) returned FALSE -> rejected',
      [result, aUserName], sllUserAuth);
    FreeAndNil(result);
  end;
end;

procedure TRestServerAuthentication.SessionCreate(Ctxt: TRestServerURIContext;
  var User: TAuthUser);
var
  Session: TAuthSession;
begin
  // now client is authenticated -> create a session
  if User <> nil then
  try
    fServer.SessionCreate(User, Ctxt, Session); // call Ctxt.AuthenticationFailed on error
    if Session <> nil then
      SessionCreateReturns(Ctxt, Session, Session.fPrivateSalt, '', '');
  finally
    User.Free;
  end;
end;

procedure TRestServerAuthentication.SessionCreateReturns(
  Ctxt: TRestServerURIContext; Session: TAuthSession;
  const result, data, header: RawUTF8);
var
  body: TDocVariantData;
begin
  body.InitFast(10, dvObject);
  if result = '' then
    body.AddValue('result', Session.IDCardinal)
  else
    body.AddValue('result', RawUTF8ToVariant(result));
  if data <> '' then
    body.AddValue('data', RawUTF8ToVariant(data));
  if fAlgoName <> '' then // match e.g. TRestServerAuthenticationSignedURIAlgo
    body.AddValue('algo', RawUTF8ToVariant(fAlgoName));
  with Session.User do
    body.AddNameValuesToObject([
      'logonid', IDValue,
      'logonname', LogonName,
      'logondisplay', DisplayName,
      'logongroup', GroupRights.IDValue,
      'timeout', GroupRights.SessionTimeout,
      'server', ExeVersion.ProgramName,
      'version', ExeVersion.Version.DetailedOrVoid]);
  Ctxt.ReturnsJson(variant(body), HTTP_SUCCESS, false, twJSONEscape, false, header);
end;


{ TRestServerAuthenticationURI }

function TRestServerAuthenticationURI.RetrieveSession(Ctxt:
  TRestServerURIContext): TAuthSession;
begin
  result := nil;
  if (Ctxt = nil) or
     (Ctxt.URISessionSignaturePos = 0) then
    exit;
  // expected format is 'session_signature='Hexa8(SessionID)'...
  if (Ctxt.URISessionSignaturePos > 0) and
     (Ctxt.URISessionSignaturePos + (18 + 8) <= length(Ctxt.Call^.Url)) and
     HexDisplayToCardinal(PAnsiChar(pointer(Ctxt.Call^.url)) +
       Ctxt.URISessionSignaturePos + 18, Ctxt.Session) then
    result := fServer.SessionAccess(Ctxt);
end;


{ TRestServerAuthenticationSignedURI }

// expected format is session_signature=
// Hexa8(SessionID)+
// Hexa8(Timestamp)+
// Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
//             Hexa8(Timestamp)+url))

constructor TRestServerAuthenticationSignedURI.Create(aServer: TRestServer);
begin
  inherited Create(aServer);
  SetAlgorithm(suaCRC32); // default/legacy hash algorithm
  TimestampCoherencySeconds := 5;
end;

procedure TRestServerAuthenticationSignedURI.SetNoTimestampCoherencyCheck(
  value: boolean);
begin
  if self <> nil then
    fNoTimestampCoherencyCheck := value;
end;

procedure TRestServerAuthenticationSignedURI.SetTimestampCoherencySeconds(
  value: cardinal);
begin
  if self = nil then
    exit;
  fTimestampCoherencySeconds := value;
  fTimestampCoherencyTicks := round(value * (1000 / 256)); // 256 ms resolution
end;

procedure TRestServerAuthenticationSignedURI.SetAlgorithm(
  value: TRestAuthenticationSignedURIAlgo);
begin
  fComputeSignature := TRestClientAuthenticationSignedURI.GetComputeSignature(value);
  if value = suaCRC32 then
    fAlgoName := ''
  else
    fAlgoName := LowerCase(TrimLeftLowerCaseShort(ToText(value)));
end;

function TRestServerAuthenticationSignedURI.RetrieveSession(
  Ctxt: TRestServerURIContext): TAuthSession;
var
  aTimestamp, aSignature, aMinimalTimestamp, aExpectedSignature: cardinal;
  PTimestamp: PAnsiChar;
  aURLlength: integer;
begin
  result := inherited RetrieveSession(Ctxt);
  if result = nil then
    // no valid session ID in session_signature
    exit;
  if Ctxt.URISessionSignaturePos + (18 + 8 + 8 + 8) > length(Ctxt.Call^.url) then
  begin
    result := nil;
    exit;
  end;
  aURLlength := Ctxt.URISessionSignaturePos - 1;
  PTimestamp := @Ctxt.Call^.url[aURLlength + (20 + 8)]; // points to Hexa8(Timestamp)
  aMinimalTimestamp := result.fLastTimestamp - fTimestampCoherencyTicks;
  if HexDisplayToCardinal(PTimestamp, aTimestamp) and
     (fNoTimestampCoherencyCheck or
      (integer(aMinimalTimestamp) < 0) or // <0 just after login
      (aTimestamp >= aMinimalTimestamp)) then
  begin
    aExpectedSignature := fComputeSignature(result.fPrivateSaltHash,
      PTimestamp, pointer(Ctxt.Call^.url), aURLlength);
    if HexDisplayToCardinal(PTimestamp + 8, aSignature) and
       (aSignature = aExpectedSignature) then
    begin
      if aTimestamp > result.fLastTimestamp then
        result.fLastTimestamp := aTimestamp;
      exit;
    end
    else
    begin
      Ctxt.Log.Log(sllUserAuth, 'Invalid Signature: expected %, got %',
        [Int64(aExpectedSignature), Int64(aSignature)], self);
    end;
  end
  else
  begin
    Ctxt.Log.Log(sllUserAuth, 'Invalid Timestamp: expected >=%, got %',
      [aMinimalTimestamp, Int64(aTimestamp)], self);
  end;
  result := nil; // indicates invalid signature
end;

var
  ServerNonceHash: TSHA3; // faster than THMAC_SHA256 on small input
  ServerNonceCache: array[boolean] of record
    tix: cardinal;
    res: RawUTF8;
  end;

function CurrentServerNonce(Previous: boolean): RawUTF8;
var
  ticks: cardinal;
  hash: TSHA3;
  res: THash256;
begin
  ticks := GetTickCount64 div (60 * 5 * 1000); // 5 minutes resolution
  if Previous then
    dec(ticks);
  with ServerNonceCache[Previous] do
    if ticks = tix then
    begin
      // very efficiently retrieved from cache
      result := res;
      exit;
    end;
  if ServerNonceHash.Algorithm <> SHA3_256 then
  begin
    // first time used: initialize the private secret for this process lifetime
    TAESPRNG.Main.Fill(@res, SizeOf(res)); // ensure unpredictable nonce
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
  hash.final(res, true);
  result := BinToHexLower(@res, SizeOf(res));
  with ServerNonceCache[Previous] do
  begin
    tix := ticks;
    res := result;
  end;
end;


{ TRestServerAuthenticationDefault }

function TRestServerAuthenticationDefault.Auth(Ctxt: TRestServerURIContext): boolean;
var
  aUserName, aPassWord, aClientNonce: RawUTF8;
  User: TAuthUser;
begin
  result := true;
  if AuthSessionRelease(Ctxt) then
    exit;
  aUserName := Ctxt.InputUTF8OrVoid['UserName'];
  aPassWord := Ctxt.InputUTF8OrVoid['Password'];
  aClientNonce := Ctxt.InputUTF8OrVoid['ClientNonce'];
  if (aUserName <> '') and
     (length(aClientNonce) > 32) then
  begin
    // GET ModelRoot/auth?UserName=...&PassWord=...&ClientNonce=... -> handshaking
    User := GetUser(Ctxt, aUserName);
    if User <> nil then
    try
      // check if match TRestClientURI.SetUser() algorithm
      if CheckPassword(Ctxt, User, aClientNonce, aPassWord) then
        // setup a new TAuthSession
        SessionCreate(Ctxt, User)
      else        // will call Ctxt.AuthenticationFailed on error
        Ctxt.AuthenticationFailed(afInvalidPassword);
    finally
      User.Free;
    end
    else
      Ctxt.AuthenticationFailed(afUnknownUser);
  end
  else if aUserName <> '' then
    // only UserName=... -> return hexadecimal nonce content valid for 5 minutes
    Ctxt.Results([CurrentServerNonce])
  else
    // parameters does not match any expected layout -> try next authentication
    result := false;
end;

function TRestServerAuthenticationDefault.CheckPassword(
  Ctxt: TRestServerURIContext; User: TAuthUser;
  const aClientNonce, aPassWord: RawUTF8): boolean;
var
  aSalt: RawUTF8;
begin
  aSalt := aClientNonce + User.LogonName + User.PasswordHashHexa;
  result := IsHex(aPassWord, SizeOf(THash256)) and
    (IdemPropNameU(aPassWord,
      SHA256(fServer.Model.Root + CurrentServerNonce(false) + aSalt)) or
     // if current nonce failed, tries with previous 5 minutes' nonce
     IdemPropNameU(aPassWord,
      SHA256(fServer.Model.Root + CurrentServerNonce(true)  + aSalt)));
end;


{ TRestServerAuthenticationNone }

function TRestServerAuthenticationNone.Auth(Ctxt: TRestServerURIContext): boolean;
var
  aUserName: RawUTF8;
  U: TAuthUser;
begin
  aUserName := Ctxt.InputUTF8OrVoid['UserName'];
  if aUserName = '' then
  begin
    result := false; // let's try another TRestServerAuthentication class
    exit;
  end;
  result := true; // this kind of weak authentication avoid stronger ones
  if AuthSessionRelease(Ctxt) then
    exit;
  U := GetUser(Ctxt, aUserName);
  if U = nil then
    Ctxt.AuthenticationFailed(afUnknownUser)
  else
    SessionCreate(Ctxt, U); // call Ctxt.AuthenticationFailed on error
end;


{ TRestServerAuthenticationHttpAbstract }

function TRestServerAuthenticationHttpAbstract.RetrieveSession(
  Ctxt: TRestServerURIContext): TAuthSession;
var
  cookie: RawUTF8;
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
  Ctxt: TRestServerURIContext; out userPass, user, pass: RawUTF8): boolean;
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
  Ctxt: TRestServerURIContext): TAuthSession;
var
  userPass, user, pass: RawUTF8;
begin
  result := inherited RetrieveSession(Ctxt);
  if result = nil then
    // not a valid 'Cookie: mORMot_session_signature=...' header
    exit;
  if (result.fExpectedHttpAuthentication <> '') and
     (result.fExpectedHttpAuthentication = Ctxt.InHeader['Authorization']) then
    // already previously authenticated for this session
    exit;
  if GetUserPassFromInHead(Ctxt, userPass, user, pass) then
    if user = result.User.LogonName then
      with Ctxt.Server.SQLAuthUserClass.Create do
      try
        PasswordPlain := pass; // compute SHA-256 hash of the supplied password
        if PasswordHashHexa = result.User.PasswordHashHexa then
        begin
          // match -> store header in result (locked by fSessions.fSafe.Lock)
          result.fExpectedHttpAuthentication := userPass;
          exit;
        end;
      finally
        Free;
      end;
  result := nil; // identicates authentication error
end;

class function TRestServerAuthenticationHttpBasic.ComputeAuthenticateHeader(
  const aUserName, aPasswordClear: RawUTF8): RawUTF8;
begin
  result := 'Authorization: Basic ' + BinToBase64(aUserName + ':' + aPasswordClear);
end;

function TRestServerAuthenticationHttpBasic.CheckPassword(
  Ctxt: TRestServerURIContext; User: TAuthUser;
  const aPassWord: RawUTF8): boolean;
var
  expectedPass: RawUTF8;
begin
  expectedPass := User.PasswordHashHexa;
  User.PasswordPlain := aPassWord; // override with SHA-256 hash from HTTP header
  result := IdemPropNameU(User.PasswordHashHexa, expectedPass);
end;

function TRestServerAuthenticationHttpBasic.Auth(Ctxt: TRestServerURIContext): boolean;
var
  userPass, user, pass: RawUTF8;
  U: TAuthUser;
  Session: TAuthSession;
begin
  if Ctxt.InputExists['UserName'] then
  begin
    result := false; // allow other schemes to check this request
    exit;
  end;
  result := true; // this authentication method is exclusive to any other
  if GetUserPassFromInHead(Ctxt, userPass, user, pass) then
  begin
    U := GetUser(Ctxt, user);
    if U <> nil then
    try
      if CheckPassword(Ctxt, U, pass) then
      begin
        fServer.SessionCreate(U, Ctxt, Session); // call Ctxt.AuthenticationFailed on error
        if Session <> nil then
        begin
          // see TRestServerAuthenticationHttpAbstract.ClientSessionSign()
          Ctxt.SetOutSetCookie((REST_COOKIE_SESSION + '=') +
            CardinalToHexLower(Session.IDCardinal));
          if (rsoRedirectForbiddenToAuth in fServer.Options) and
             (Ctxt.ClientKind = ckAjax) then
            Ctxt.Redirect(fServer.Model.Root)
          else
            SessionCreateReturns(Ctxt, Session, '', '', '');
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


{ TRestServerAuthenticationSSPI }

destructor TRestServerAuthenticationSSPI.Destroy;
var
  i: PtrInt;
begin
  for i := 0 to High(fSSPIAuthContexts) do
    FreeSecContext(fSSPIAuthContexts[i]);
  inherited Destroy;
end;

function TRestServerAuthenticationSSPI.Auth(
  Ctxt: TRestServerURIContext): boolean;
var
  i, ndx: PtrInt;
  username, indataenc: RawUTF8;
  ticks,connectionID: Int64;
  browserauth: Boolean;
  ctxarray: TDynArray;
  outdata: RawByteString;
  user: TAuthUser;
  session: TAuthSession;
begin
  result := AuthSessionRelease(Ctxt);
  if result or
     not Ctxt.InputExists['username'] or
     not Ctxt.InputExists['Data'] then
    exit;
  // use connectionID to find authentication session
  connectionID := Ctxt.Call^.LowLevelConnectionID;
  // GET ModelRoot/auth?username=&data=... -> windows SSPI auth
  indataenc := Ctxt.InputUTF8['Data'];
  if indataenc = '' then
  begin
    // client is browser and used HTTP headers to send auth data
    FindNameValue(Ctxt.Call.InHead, SECPKGNAMEHTTPAUTHORIZATION, indataenc);
    if indataenc = '' then
    begin
      // no auth data sent, reply with supported auth methods
      Ctxt.Call.OutHead := SECPKGNAMEHTTPWWWAUTHENTICATE;
      Ctxt.Call.OutStatus := HTTP_UNAUTHORIZED; // (401)
      Ctxt.Call.OutBody := StatusCodeToReason(HTTP_UNAUTHORIZED);
      exit;
    end;
    browserauth := True;
  end
  else
    browserauth := False;
  ctxarray.InitSpecific(TypeInfo(TSecContextDynArray), fSSPIAuthContexts, ptInt64);
  // check for outdated auth context
  ticks := GetTickCount64 - 30000;
  for i := High(fSSPIAuthContexts) downto 0 do
    if ticks > fSSPIAuthContexts[i].CreatedTick64 then
    begin
      FreeSecContext(fSSPIAuthContexts[i]);
      ctxarray.Delete(i);
    end;
  // if no auth context specified, create a new one
  result := true;
  ndx := ctxarray.Find(connectionID);
  if ndx < 0 then
  begin
    // 1st call: create SecCtxId
    if High(fSSPIAuthContexts) > MAXSSPIAUTHCONTEXTS then
    begin
      fServer.InternalLog('Too many Windows Authenticated session in  pending' +
        ' state: MAXSSPIAUTHCONTEXTS=%', [MAXSSPIAUTHCONTEXTS], sllUserAuth);
      exit;
    end;
    ndx := ctxarray.New; // add a new entry to fSSPIAuthContexts[]
    InvalidateSecContext(fSSPIAuthContexts[ndx], connectionID);
  end;
  // call SSPI provider
  if ServerSSPIAuth(fSSPIAuthContexts[ndx], Base64ToBin(indataenc), outdata) then
  begin
    if browserauth then
    begin
      Ctxt.Call.OutHead :=
        (SECPKGNAMEHTTPWWWAUTHENTICATE + ' ') + BinToBase64(outdata);
      Ctxt.Call.OutStatus := HTTP_UNAUTHORIZED; // (401)
      Ctxt.Call.OutBody := StatusCodeToReason(HTTP_UNAUTHORIZED);
    end
    else
      Ctxt.Returns(['result', '',
                    'data', BinToBase64(outdata)]);
    exit; // 1st call: send back outdata to the client
  end;
  // 2nd call: user was authenticated -> release used context
  ServerSSPIAuthUser(fSSPIAuthContexts[ndx], username);
  if sllUserAuth in fServer.fLogFamily.Level then
    fServer.fLogFamily.SynLog.Log(sllUserAuth, '% Authentication success for %',
      [SecPackageName(fSSPIAuthContexts[ndx]), username], self);
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
              BinToBase64(SecEncrypt(fSSPIAuthContexts[ndx], session.fPrivateSalt)),
              BinToBase64(outdata),'');
    finally
      user.Free;
    end else
      Ctxt.AuthenticationFailed(afUnknownUser);
  finally
    FreeSecContext(fSSPIAuthContexts[ndx]);
    ctxarray.Delete(ndx);
  end;
end;

{$endif DOMAINRESTAUTH}


{ ************ TRestServerMonitor for High-Level Statistics of a REST Server }

{ TRestServerMonitor }

constructor TRestServerMonitor.Create(aServer: TRestServer);
begin
  if aServer = nil then
    raise EOrmException.CreateUTF8('%.Create(nil)', [self]);
  inherited Create(aServer.Model.Root);
  fServer := aServer;
  SetLength(fPerTable[false], length(aServer.Model.Tables));
  SetLength(fPerTable[true], length(aServer.Model.Tables));
  fStartDate := NowUTCToString;
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

procedure TRestServerMonitor.NotifyORM(aMethod: TURIMethod);
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
  RW: array[boolean] of RawUTF8 = (
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
        fServer.Model.TableProps[TableIndex].Props.SQLTableName + RW[Write]);
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
    raise ESynException.CreateUTF8('%.Create(nil)', [self]);
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
  begin // may use REST cache
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

constructor TInterfacedCallback.Create(aRest: TRest; const aGUID: TGUID);
begin
  inherited Create;
  fRest := aRest;
  fInterface := aGUID;
end;

procedure TInterfacedCallback.CallbackRestUnregister;
var
  obj: pointer; // not IInvokable to avoid unexpected (recursive) Destroy call
begin
  if (fRest <> nil) and
     (fRest.Services <> nil) and
     not IsNullGUID(fInterface) then
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
  const aGUID: TGUID);
begin
  inherited Create(aRest, aGUID);
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
  tmp: RawUTF8;
begin
  if aModel = nil then
    raise EOrmException.CreateUTF8('%.Create(Model=nil)', [self]);
  fStatLevels := SERVERDEFAULTMONITORLEVELS;
  fSessions := TSynObjectListLocked.Create; // needed by AuthenticationRegister() below
  fSQLAuthUserClass := TAuthUser;
  fSQLAuthGroupClass := TAuthGroup;
  fModel := aModel; // we need this property ASAP
  fSessionClass := TAuthSession;
  if aHandleUserAuthentication then
    // default mORMot authentication schemes
    AuthenticationRegister([
      TRestServerAuthenticationDefault
      {$ifdef DOMAINRESTAUTH},
      TRestServerAuthenticationSSPI
      {$endif DOMAINRESTAUTH}]);
  fAssociatedServices := TServicesPublishedInterfacesList.Create(0);
  inherited Create(aModel);
  fAfterCreation := true;
  fStats := TRestServerMonitor.Create(self);
  URIPagingParameters := PAGINGPARAMETERS_YAHOO;
  TAESPRNG.Main.Fill(@fSessionCounter, SizeOf(fSessionCounter));
  if integer(fSessionCounter) < 0 then
    // ensure the counter is a random but positive 31-bit integer
    fSessionCounter := -fSessionCounter;
  // retrieve published methods
  fPublishedMethods.InitSpecific(TypeInfo(TRestServerMethods), fPublishedMethod,
    ptRawUTF8, nil, true);
  ServiceMethodRegisterPublishedMethods('', self);
  fPublishedMethodAuthIndex := ServiceMethodByPassAuthentication('Auth');
  fPublishedMethodTimestampIndex := ServiceMethodByPassAuthentication('Timestamp');
  tmp := 'Batch';
  fPublishedMethodBatchIndex := fPublishedMethods.FindHashed(tmp);
  if (fPublishedMethodBatchIndex < 0) or
     (fPublishedMethodTimestampIndex < 0) then
    raise EOrmException.CreateUTF8('%.Create: missing method!', [self]);
end;

destructor TRestServer.Destroy;
var
  i: PtrInt;
begin
  Shutdown;
  fOrm.AsynchBatchStop(nil); // release all existing batch instances
  FreeAndNil(fRun); // do it ASAP
  fRecordVersionSlaveCallbacks := nil; // should be done before fServices.Free
  for i := 0 to fPublishedMethods.Count - 1 do
    fPublishedMethod[i].Stats.Free;
  ObjArrayClear(fSessionAuthentication);
  inherited Destroy; // calls fServices.Free which will update fStats
  FreeAndNil(fJWTForUnauthenticatedRequest);
  FreeAndNil(fStats);
end;

constructor TRestServer.CreateWithOwnModel(
  const Tables: array of TOrmClass; aHandleUserAuthentication: boolean;
  const aRoot: RawUTF8);
var
  Model: TOrmModel;
begin
  Model := TOrmModel.Create(Tables, aRoot);
  Create(Model, aHandleUserAuthentication);
  Model.Owner := self;
end;

function TRestServer.OrmInstance: TRestOrm;
begin
  result := TRestOrm(fOrmInstance);
end;

procedure TRestServer.SetNoAJAXJSON(const Value: boolean);
begin
  if Value then
    include(fOptions, rsoNoAJAXJSON)
  else
    exclude(fOptions, rsoNoAJAXJSON);
  (fOrmInstance as TRestOrmServer).SetNoAJAXJSON(Value);
end;

function TRestServer.GetNoAJAXJSON: boolean;
begin
  result := (self <> nil) and
            (rsoNoAJAXJSON in fOptions);
end;

constructor TRestServer.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
begin
  Create(aModel, aServerHandleAuthentication);
end;

procedure TRestServer.Shutdown(const aStateFileName: TFileName);
var
  timeout: Int64;
  log: ISynLog; // for Enter auto-leave to work with FPC
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
    fShutdownRequested := true; // will be identified by TRestServer.URI()
  finally
    fSessions.Safe.UnLock;
  end;
  timeout := GetTickCount64 + 30000; // never wait forever
  repeat
    SleepHiRes(5);
  until (fStats.AddCurrentRequestCount(0) = 0) or
        (GetTickCount64 > timeout);
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
  TableIndex: integer; var Decoder: TJSONObjectDecoder;
  RecordVersionField: TOrmPropInfoRTTIRecordVersion);
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
    fSQLAuthGroupClass := Model.AddTableInherited(TAuthGroup);
    fSQLAuthUserClass := Model.AddTableInherited(TAuthUser);
    if fAfterCreation and
       (not fOrm.TableHasRows(fSQLAuthUserClass) or
        not fOrm.TableHasRows(fSQLAuthGroupClass)) then
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

function TRestServer.ServicesPublishedInterfaces: RawUTF8;
var
  nfo: TServicesPublishedInterfaces;
begin
  if (self = nil) or
     (Services = nil) then
    result := ''
  else
  begin
    nfo.PublicURI := fPublicURI;
    Services.SetInterfaceNames(nfo.Names);
    result := RecordSaveJSON(nfo, TypeInfo(TServicesPublishedInterfaces));
  end;
end;

procedure TRestServer.SetPublicURI(const Address, Port: RawUTF8);
begin
  fPublicURI.Address := Address;
  fPublicURI.Port := Port;
  fPublicURI.Root := Model.Root;
end;

procedure TRestServer.SetStatUsage(usage: TSynMonitorUsage);
begin
  if fStatUsage = usage then
    exit;
  if usage = nil then
  begin
    // e.g. from TTestServiceOrientedArchitecture.ClientSideRESTSessionsStats
    FreeAndNil(fStatUsage);
    exit;
  end;
  if fStatUsage <> nil then
    raise EModelException.CreateUTF8('%.StatUsage should be set once', [self]);
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
    result := (fServices as TServiceContainerServer).RecordVersionSynchronizeSubscribeMaster(
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
  MasterRemoteAccess: TRestClientURI; OnNotify: TOnBatchWrite): boolean;
var
  current, previous: TRecordVersion;
  tableIndex: integer;
  tableName: RawUTF8;
  service: IServiceRecordVersion;
  callback: IServiceRecordVersionCallback;
  retry: integer;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fLogClass.Enter('RecordVersionSynchronizeSlaveStart % over %',
    [Table, MasterRemoteAccess], self);
  callback := nil; // weird fix for FPC/ARM
  result := false;
  if (self = nil) or
     (MasterRemoteAccess = nil) then
    exit;
  tableIndex := Model.GetTableIndexExisting(Table);
  if (fRecordVersionSlaveCallbacks <> nil) and
     (fRecordVersionSlaveCallbacks[tableIndex] <> nil) then
  begin
    InternalLog('RecordVersionSynchronizeSlaveStart(%): already running',
      [Table], sllWarning);
    exit;
  end;
  tableName := Model.TableProps[tableIndex].Props.SQLTableName;
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
        Table, MasterRemoteAccess, 10000, OnNotify);
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
    if service.Subscribe(tableName, current, callback) then
    begin
      // push notifications
      if fRecordVersionSlaveCallbacks = nil then
        SetLength(fRecordVersionSlaveCallbacks, Model.TablesMax + 1);
      fRecordVersionSlaveCallbacks[tableIndex] := callback;
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
  tableIndex: integer;
begin
  result := false;
  if self = nil then
    exit;
  tableIndex := Model.GetTableIndexExisting(Table);
  if (fRecordVersionSlaveCallbacks = nil) or
     (fRecordVersionSlaveCallbacks[tableIndex] = nil) then
  begin
    InternalLog('RecordVersionSynchronizeSlaveStop(%): not running',
      [Table], sllWarning);
    exit;
  end;
  fRecordVersionSlaveCallbacks[tableIndex] := nil; // will notify the server
  result := true;
end;

procedure TRestServer.InternalInfo(var info: TDocVariantData);
var
  cpu, mem, free: RawUTF8;
  now: TTimeLogBits;
  m: TSynMonitorMemory;
begin
  // called by root/Timestamp/info REST method
  now.Value := GetServerTimestamp;
  cpu := TSystemUse.Current(false).HistoryText(0, 15, @mem);
  m := TSynMonitorMemory.Create({nospace=}true);
  try
    FormatUTF8('%/%', [m.PhysicalMemoryFree.Text, m.PhysicalMemoryTotal.Text], free);
    info.AddNameValuesToObject([
      'nowutc', now.Text(true, ' '),
      'timestamp', now.Value,
      'exe', ExeVersion.ProgramName,
      'version', ExeVersion.Version.DetailedOrVoid,
      'host', ExeVersion.Host,
      'cpu', cpu,
      {$ifdef MSWINDOWS} 'mem', mem, {$endif}
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

procedure TRestServer.InternalStat(Ctxt: TRestServerURIContext; W: TTextWriter);
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
    W.Add(',');
  end;
  if (fRun.BackgroundTimer <> nil) and
     (fRun.BackgroundTimer.Stats <> nil) then
  begin
    W.CancelLastComma;
    W.AddShort(',"backgroundTimer":');
    fRun.BackgroundTimer.Stats.ComputeDetailsTo(W);
    W.Add(',');
  end;
  if withtables in Flags then
  begin
    W.CancelLastComma;
    W.AddShort(',"tables":[');
    Stats.Lock; // thread-safe Stats.fPerTable[] access
    try
      for i := 0 to fModel.TablesMax do
      begin
        W.Add('{"%":[', [fModel.TableProps[i].Props.SQLTableName]);
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
  const aMethod: RawUTF8): TSynMonitorInputOutput;
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
  aServicesRouting: TRestServerURIContextClass);
begin
  if self <> nil then
    if aServicesRouting <> fServicesRouting then
      if (aServicesRouting = nil) or
         (aServicesRouting = TRestServerURIContext) then
        raise EServiceException.CreateUTF8(
          'Unexpected %.SetRoutingClass(%)', [self, aServicesRouting])
      else
        fServicesRouting := aServicesRouting;
end;

procedure TRestServer.SessionCreate(var User: TAuthUser;
  Ctxt: TRestServerURIContext; out Session: TAuthSession);
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
          Ctxt.Log.Log(sllUserAuth,
            'User.LogonName=% already connected from %/%', [User.LogonName,
            RemoteIP, Ctxt.Call^.LowLevelConnectionID], self);
        Ctxt.AuthenticationFailed(afSessionAlreadyStartedForThisUser);
        exit; // user already connected
      end;
  Session := fSessionClass.Create(Ctxt, User);
  if Assigned(OnSessionCreate) then
    if OnSessionCreate(self, Session, Ctxt) then
    begin
      // TRUE aborts session creation
      Ctxt.Log.Log(sllUserAuth, 'Session aborted by OnSessionCreate() callback ' +
        'for User.LogonName=% (connected from %/%) - clients=%, sessions=%',
        [User.LogonName, Session.RemoteIP, Ctxt.Call^.LowLevelConnectionID,
         fStats.GetClientsCurrent, fSessions.Count], self);
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
  Ctxt: TRestServerURIContext);
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

function TRestServer.SessionAccess(Ctxt: TRestServerURIContext): TAuthSession;
var
  i: integer;
  tix, session: cardinal;
  sessions: ^TAuthSession;
begin // caller of RetrieveSession() made fSessions.Safe.Lock
  if (self <> nil) and
     (fSessions <> nil) then
  begin
    tix := GetTickCount64 shr 10;
    if tix <> fSessionsDeprecatedTix then
    begin
      fSessionsDeprecatedTix := tix; // check deprecated sessions every second
      for i := fSessions.Count - 1 downto 0 do
        if tix > TAuthSession(fSessions.List[i]).TimeOutTix then
          SessionDelete(i, nil);
    end;
    // retrieve session from its ID
    sessions := pointer(fSessions.List);
    session := Ctxt.Session;
    for i := 1 to fSessions.Count do
      if sessions^.IDCardinal = session then
      begin
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

function TRestServer.SessionGetUser(aSessionID: Cardinal): TAuthUser;
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
            result := User.CreateCopy as fSQLAuthUserClass;
            result.GroupRights := nil;
          end;
          Break;
        end;
  finally
    fSessions.Safe.UnLock;
  end;
end;

function TRestServer.SessionsAsJson: RawJSON;
var
  i: PtrInt;
  W: TJSONSerializer;
  temp: TTextWriterStackBuffer;
begin
  result := '';
  if (self = nil) or
     (fSessions.Count = 0) then
    exit;
  W := TJSONSerializer.CreateOwnedStream(temp);
  try
    fSessions.Safe.Lock;
    try
      W.Add('[');
      for i := 0 to fSessions.Count - 1 do
      begin
        W.WriteObject(fSessions.List[i]);
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add(']');
      W.SetText(RawUTF8(result));
    finally
      fSessions.Safe.UnLock;
    end;
  finally
    W.Free;
  end;
end;

function TRestServer.BanIP(const aIP: RawUTF8; aRemoveBan: boolean): boolean;
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

function TRestServer.JWTForUnauthenticatedRequestWhiteIP(const aIP: RawUTF8;
  aRemoveWhite: boolean): boolean;
begin
  result := false;
  if fJWTForUnauthenticatedRequest = nil then
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
  const aPrefix: RawUTF8; aInstance: TObject);
var
  i: PtrInt;
  methods: TPublishedMethodInfoDynArray;
begin
  if aInstance = nil then
    exit;
  if PosExChar('/', aPrefix) > 0 then
    raise EServiceException.CreateUTF8('%.ServiceMethodRegisterPublishedMethods' +
      '("%"): prefix should not contain "/"', [self, aPrefix]);
  for i := 0 to GetPublishedMethods(aInstance, methods) - 1 do
    with methods[i] do
      ServiceMethodRegister(aPrefix + Name, TOnRestServerCallBack(Method));
end;

procedure TRestServer.ServiceMethodRegister(aMethodName: RawUTF8;
  const aEvent: TOnRestServerCallBack; aByPassAuthentication: boolean);
begin
  aMethodName := trim(aMethodName);
  if aMethodName = '' then
    raise EServiceException.CreateUTF8('%.ServiceMethodRegister('''')', [self]);
  if Model.GetTableIndex(aMethodName) >= 0 then
    raise EServiceException.CreateUTF8('Published method name %.% ' +
      'conflicts with a Table in the Model!', [self, aMethodName]);
  with PRestServerMethod(fPublishedMethods.AddUniqueName(aMethodName,
    'Duplicated published method name %.%', [self, aMethodName]))^ do
  begin
    callback := aEvent;
    ByPassAuthentication := aByPassAuthentication;
  end;
end;

function TRestServer.ServiceMethodByPassAuthentication(
  const aMethodName: RawUTF8): integer;
var
  i: PtrInt;
begin
  result := -1;
  if self = nil then
    exit;
  if aMethodName = '' then
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
  const aContractExpected: RawUTF8): TServiceFactoryServerAbstract;
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
  const aContractExpected: RawUTF8): TServiceFactoryServerAbstract;
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
  const aContractExpected: RawUTF8): boolean;
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
  const aContractExpected: RawUTF8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.GUID2TypeInfo(aInterfaces);
  result := ServiceRegister(aImplementationClass, ti, aInstanceCreation,
    aContractExpected);
end;

function TRestServer.ServiceDefine(aSharedImplementation: TInterfacedObject;
  const aInterfaces: array of TGUID;
  const aContractExpected: RawUTF8): TServiceFactoryServerAbstract;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.GUID2TypeInfo(aInterfaces);
  result := ServiceRegister(aSharedImplementation, ti, aContractExpected);
end;

function TRestServer.ServiceDefine(aClient: TRest;
  const aInterfaces: array of TGUID;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8): boolean;
var
  ti: PRttiInfoDynArray;
begin
  ti := TInterfaceFactory.GUID2TypeInfo(aInterfaces);
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
        SQLAuthUserClass.RecordProps.SaveBinaryHeader(W);
        SQLAuthGroupClass.RecordProps.SaveBinaryHeader(W);
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
    raise ESynException.CreateUTF8('%.SessionsLoadFromFile("%")', [self, aFileName]);
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
  R.Init(pointer(s), length(s));
  fSessions.Safe.Lock;
  try
    (fOrmInstance as TRestOrmServer).InternalState := R.VarUInt32;
    if not SQLAuthUserClass.RecordProps.CheckBinaryHeader(R) or
       not SQLAuthGroupClass.RecordProps.CheckBinaryHeader(R) then
      ContentError;
    n := R.VarUInt32;
    fSessions.Clear;
    for i := 1 to n do
    begin
      fSessions.Add(fSessionClass.CreateFrom(R.P, R.Last, self));
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
    raise ERestException.CreateUTF8('%.BeginCurrentThread(nil)', [self]);
  InternalLog('BeginCurrentThread(%) root=% ThreadID=% ThreadCount=%',
    [Sender.ClassType, fModel.Root, {%H-}pointer(id), tc]);
  if Sender.ThreadID <> id then
    raise ERestException.CreateUTF8(
      '%.BeginCurrentThread(Thread.ID=%) and CurrentThreadID=% should match',
      [self, {%H-}pointer(Sender.ThreadID), {%H-}pointer(id)]);
  with PServiceRunningContext(PerThreadRunningContextAddress)^ do
    if RunningThread <> Sender then
      // e.g. if length(TRestHttpServer.fDBServers)>1
      if RunningThread <> nil then
        raise ERestException.CreateUTF8('%.BeginCurrentThread() twice', [self])
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
  Inst: TServiceFactoryServerInstance;
begin
  tc := fStats.NotifyThreadCount(-1);
  id := GetCurrentThreadId;
  if Sender = nil then
    raise ERestException.CreateUTF8('%.EndCurrentThread(nil)', [self]);
  InternalLog('EndCurrentThread(%) ThreadID=% ThreadCount=%',
    [Sender.ClassType, {%H-}pointer(id), tc]);
  if Sender.ThreadID <> id then
    raise ERestException.CreateUTF8(
      '%.EndCurrentThread(%.ID=%) should match CurrentThreadID=%',
      [self, Sender, {%H-}pointer(Sender.ThreadID), {%H-}pointer(id)]);
  if Services <> nil then
  begin
    Inst.InstanceID := PtrUInt(id);
    for i := 0 to Services.Count - 1 do
      with TServiceFactoryServer(Services.InterfaceList[i].service) do
        if InstanceCreation = sicPerThread then
          InternalInstanceRetrieve(Inst, ord(imFree), 0);
  end;
  with PServiceRunningContext(PerThreadRunningContextAddress)^ do
    if RunningThread <> nil then
      // e.g. if length(TRestHttpServer.fDBServers)>1
      if RunningThread <> Sender then
        raise ERestException.CreateUTF8(
          '%.EndCurrentThread(%) should match RunningThread=%',
          [self, Sender, RunningThread])
      else        // reset the TThread info
        RunningThread := nil;
  // call TRestOrmServer.EndCurrentThread
  inherited OnEndCurrentThread(Sender);
end;

procedure TRestServer.URI(var Call: TRestURIParams);
const
  COMMANDTEXT: array[TRestServerURIContextCommand] of string[9] = (
    '?', 'Method', 'Interface', 'Read', 'Write');
var
  Ctxt: TRestServerURIContext;
  timeStart, timeEnd: Int64;
  elapsed, len: cardinal;
  outcomingfile: boolean;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  log := fLogClass.Enter('URI % % in=%',
    [Call.Method, Call.Url, KB(Call.InBody)], self);
  QueryPerformanceMicroSeconds(timeStart);
  fStats.AddCurrentRequestCount(1);
  Call.OutStatus := HTTP_BADREQUEST; // default error code is 400 BAD REQUEST
  Ctxt := ServicesRouting.Create(self, Call);
  try
    if log <> nil then
      Ctxt.Log := log.Instance;
    if fShutdownRequested then
      Ctxt.Error('Server is shutting down', HTTP_UNAVAILABLE)
    else if Ctxt.Method = mNone then
      Ctxt.Error('Unknown Verb %', [Call.Method])
    else if (fIPBan <> nil) and
            fIPBan.Exists(Ctxt.RemoteIP) then
      Ctxt.Error('Banned IP %', [Ctxt.fRemoteIP])
    else
    // 1. decode URI
    if not Ctxt.URIDecodeREST then
      Ctxt.Error('Invalid Root', HTTP_NOTFOUND)
    else if (RootRedirectGet <> '') and
            (Ctxt.Method = mGet) and
            (Call.Url = Model.Root) and
            (Call.InBody = '') then
      Ctxt.Redirect(RootRedirectGet)
    else
    begin
      Ctxt.URIDecodeSOAByMethod;
      if (Ctxt.MethodIndex < 0) and
         (Ctxt.URI <> '') then
        Ctxt.URIDecodeSOAByInterface;
      // 2. handle security
      if (rsoSecureConnectionRequired in fOptions) and
         (Ctxt.MethodIndex <> fPublishedMethodTimestampIndex) and
         not (llfSecured in Call.LowLevelFlags) then
        Ctxt.AuthenticationFailed(afSecureConnectionRequired)
      else if not Ctxt.Authenticate then
        Ctxt.AuthenticationFailed(afInvalidSignature)
      else if (Ctxt.Service <> nil) and
          not (reService in Call.RestAccessRights^.AllowRemoteExecute) then
        if (rsoRedirectForbiddenToAuth in Options) and
           (Ctxt.ClientKind = ckAjax) then
          Ctxt.Redirect(Model.Root + '/auth')
        else
          Ctxt.AuthenticationFailed(afRemoteServiceExecutionNotAllowed)
      else if (Ctxt.Session <> CONST_AUTHENTICATION_NOT_USED) or
              (fJWTForUnauthenticatedRequest = nil) or
              (Ctxt.MethodIndex = fPublishedMethodTimestampIndex) or
              ((llfSecured in Call.LowLevelFlags) and
               // HTTPS does not authenticate by itself
               not (llfHttps in Call.LowLevelFlags)) or
              Ctxt.AuthenticationCheck(fJWTForUnauthenticatedRequest) then
      // 3. call appropriate ORM / SOA commands in fAcquireExecution[] context
      try
        if Ctxt.MethodIndex >= 0 then
          if Ctxt.MethodIndex = fPublishedMethodBatchIndex then
            Ctxt.Command := execOrmWrite
          else
            Ctxt.Command := execSOAByMethod
        else if Ctxt.Service <> nil then
          Ctxt.Command := execSOAByInterface
        else if Ctxt.Method in [mLOCK, mGET, mUNLOCK, mSTATE] then
          // handle read methods
          Ctxt.Command := execOrmGet
        else
          // write methods (mPOST, mPUT, mDELETE...)
          Ctxt.Command := execOrmWrite;
        if not Assigned(OnBeforeURI) or
           OnBeforeURI(Ctxt) then
          Ctxt.ExecuteCommand;
      except
        on E: Exception do
          if not Assigned(OnErrorURI) or
             OnErrorURI(Ctxt, E) then
            if E.ClassType = EInterfaceFactory then
              Ctxt.Error(E, '', [], HTTP_NOTACCEPTABLE)
            else
              Ctxt.Error(E, '', [], HTTP_SERVERERROR);
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
        Ctxt.Error(Ctxt.CustomErrorMsg, Call.OutStatus);
    end;
    StatsAddSizeForCall(fStats, Call);
    if (rsoNoInternalState in fOptions) and
       (Ctxt.Method <> mSTATE) then
      // reduce headers verbosity
      Call.OutInternalState := 0
    else if (Ctxt.StaticOrm <> nil) and
            Ctxt.StaticOrm.InheritsFrom(TRestStorage) and
            TRestStorage(Ctxt.StaticOrm).OutInternalStateForcedRefresh then
      // force always refresh for Static table which demands it
      Call.OutInternalState := cardinal(-1)
    else
      // database state may have changed above
      Call.OutInternalState := TRestOrmServer(fOrmInstance).InternalState;
    if Ctxt.OutSetCookie <> '' then
    begin
      Call.OutHead := Trim(Call.OutHead + #13#10'Set-Cookie: ' + Ctxt.OutSetCookie);
      if rsoCookieIncludeRootPath in fOptions then
        // case-sensitive Path=/ModelRoot
        Call.OutHead := Call.OutHead + '; Path=/';
    end;
    if not (rsoHttpHeaderCheckDisable in fOptions) and
       IsInvalidHttpHeader(pointer(Call.OutHead), length(Call.OutHead)) then
      Ctxt.Error('Unsafe HTTP header rejected [%]',
        [EscapeToShort(Call.OutHead)], HTTP_SERVERERROR);
  finally
    QueryPerformanceMicroSeconds(timeEnd);
    Ctxt.MicroSecondsElapsed :=
      fStats.FromExternalQueryPerformanceCounters(timeEnd - timeStart);
    InternalLog('% % % %/% %=% out=% in %', [Ctxt.SessionUserName,
      Ctxt.RemoteIPNotLocal, Call.Method, Model.Root, Ctxt.URI,
      COMMANDTEXT[Ctxt.Command], Call.OutStatus, KB(Call.OutBody),
      MicroSecToString(Ctxt.MicroSecondsElapsed)], sllServer);
    if (Call.OutBody <> '') and
       (sllServiceReturn in fLogFamily.Level) then
      if not (optNoLogOutput in Ctxt.ServiceExecutionOptions) then
        if IsHTMLContentTypeTextual(pointer(Call.OutHead)) then
          fLogFamily.SynLog.Log(sllServiceReturn, Call.OutBody, self,
            MAX_SIZE_RESPONSE_LOG);
    if mlTables in StatLevels then
      case Ctxt.Command of
        execOrmGet:
          fStats.NotifyOrmTable(Ctxt.TableIndex, length(Call.OutBody), false,
            Ctxt.MicroSecondsElapsed);
        execOrmWrite:
          fStats.NotifyOrmTable(Ctxt.TableIndex, length(Call.InBody), true,
            Ctxt.MicroSecondsElapsed);
      end;
    fStats.AddCurrentRequestCount(-1);
    if fStatUsage <> nil then
      fStatUsage.Modified(fStats, []);
    if Assigned(OnAfterURI) then
    try
      OnAfterURI(Ctxt);
    except
    end;
    Ctxt.Free;
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


initialization
  // should match TPerThreadRunningContext definition in mormot.core.interfaces
  assert(SizeOf(TServiceRunningContext) =
    SizeOf(TObject) + SizeOf(TObject) + SizeOf(TThread));
  {$ifdef DOMAINRESTAUTH}
  // setup mormot.lib.sspi/gssapi unit depending on the OS
  InitializeDomainAuth;
  {$endif DOMAINRESTAUTH}

end.

