/// REpresentation State Tranfer (REST) Types and Classes on Client side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.client;

{
  *****************************************************************************

   Client-Side REST Process
    - Client Authentication and Authorization Logic
    - TRestClientRoutingRest/TRestClientRoutingJsonRpc Routing Schemes
    - TRestClientUri Base Class for Actual Clients
    - TRestClientLibraryRequest after TRestServer.ExportServerGlobalLibraryRequest
    - TInterfacedCallback/TBlockingCallback Classes

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
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
  mormot.crypt.core,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.perf,
  mormot.core.search,
  mormot.crypt.secure,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.rest,
  mormot.soa.core,
  mormot.soa.client,
  mormot.db.core,
  mormot.rest.core;


{ ************ Client Authentication and Authorization Logic }

type
  TRestClientUri = class;

  /// used by TRestClientUri.Uri() to let the client ask for an User name
  // and password, in order to retry authentication to the server
  // - should return TRUE if aUserName and aPassword both contain some entered
  // values to be sent for remote secure authentication
  // - should return FALSE if the user pressed cancel or the number of Retry
  // reached a defined limit
  // - here input/output parameters are defined as plain string, to match the
  // type expected by the client's User Interface, via VCL properties, or
  // e.g. from TLoginForm as defined in mORMotUILogin.pas unit
  TOnAuthentificationFailed = function(Retry: integer;
    var aUserName, aPassword: string; out aPasswordHashed: boolean): boolean of object;

  /// define how TRestClientAuthentication.ClientSetUser() should interpret
  // the supplied password
  // - passClear means that the password is not encrypted, e.g. as entered
  // by the user in the login screen
  // - passHashed means that the passwod is already hashed as in
  // TAuthUser.PasswordHashHexa i.e. Sha256('salt'+Value)
  // - passKerberosSpn indicates that the password is the Kerberos SPN domain
  TRestClientSetUserPassword = (
    passClear,
    passHashed,
    passKerberosSpn);

  /// algorithms known by TRestClientAuthenticationSignedUri and
  // TRestServerAuthenticationSignedUri to digitaly compute the
  // session_signature parameter value for a given URI
  // - by default, suaCRC32 will compute fast but not cryptographically secure
  // ! crc32(crc32(privatesalt, timestamp, 8), url, urllen)
  // - suaCRC32C and suaXXHASH will be faster and slightly safer
  // - but you can select other stronger alternatives, which result will be
  // reduced to 32-bit hexadecimal - suaMD5 will be the fastest cryptographic
  // hash available on all platforms, for enhanced security, by calling e.g.
  // ! (aServer.AuthenticationRegister(TRestClientAuthenticationDefault) as
  // !   TRestServerAuthenticationDefault).Algorithm := suaMD5;
  // - suaSHA1, suaSHA256, suaSHA512 and suaSHA3 will be the slowest, to provide
  // additional level of trust, depending on your requirements: note that
  // since the hash is reduced to 32-bit resolution, those may not provide
  // higher security than suaMD5
  // - note that SynCrossPlatformRest clients only implements suaCRC32 yet
  TRestAuthenticationSignedUriAlgo = (
    suaCRC32,
    suaCRC32C,
    suaXXHASH,
    suaMD5,
    suaSHA1,
    suaSHA256,
    suaSHA512,
    suaSHA3);

  /// function prototype for TRestClientAuthenticationSignedUri and
  // TRestServerAuthenticationSignedUri computation of the session_signature
  // parameter value
  TOnRestAuthenticationSignedUriComputeSignature = function(
    privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal of object;

  /// abstract class used to implement client-side authentication
  // - inherit from this class to implement expected authentication scheme
  TRestClientAuthentication = class
  protected
    /// abstract method which will be called by ClientSetUser() to process the
    // authentication step on the client side
    // - at call, a TAuthUser instance will be supplied, with LogonName set
    // with aUserName and PasswordHashHexa with a SHA-256 hash of aPassword
    // - override with the expected method, returning the session key on success
    class function ClientComputeSessionKey(Sender: TRestClientUri;
      User: TAuthUser): RawUtf8; virtual; abstract;
    /// is called by ClientComputeSessionKey() overriden method to execute the
    // root/Auth service with the supplied parameters, then retrieve and
    // decode the "result": session key and any other values (e.g. "version")
    class function ClientGetSessionKey(Sender: TRestClientUri; User: TAuthUser;
      const aNameValueParameters: array of const): RawUtf8; virtual;
  public
    /// class method to be used on client side to create a remote session
    // - call this method instead of TRestClientUri.SetUser() if you need
    // a custom authentication class
    // - if saoUserByLogonOrID is defined in the server Options, aUserName may
    // be a TAuthUser.ID and not a TAuthUser.LogonName
    // - if passClear is used, you may specify aHashSalt and aHashRound,
    // to enable Pbkdf2HmacSha256() use instead of plain Sha256(), and increase
    // security on storage side (reducing brute force attack via rainbow tables)
    // - will call the ModelRoot/Auth service, i.e. call TRestServer.Auth()
    // published method to create a session for this user
    // - returns true on success
    class function ClientSetUser(Sender: TRestClientUri;
      const aUserName, aPassword: RawUtf8;
      aPasswordKind: TRestClientSetUserPassword = passClear;
      const aHashSalt: RawUtf8 = ''; aHashRound: integer = 20000): boolean; virtual;
    /// class method to be called on client side to sign an URI
    // - used by TRestClientUri.Uri()
    // - shall match the method as expected by RetrieveSession() virtual method
    class procedure ClientSessionSign(Sender: TRestClientUri;
      var Call: TRestUriParams); virtual; abstract;
  end;

  /// class-reference type (metaclass) used to define an authentication scheme
  TRestClientAuthenticationClass = class of TRestClientAuthentication;

  /// weak authentication scheme using URL-level parameter
  TRestClientAuthenticationUri = class(TRestClientAuthentication)
  public
    /// class method to be called on client side to add the SessionID to the URI
    // - append '&session_signature=SessionID' to the url
    class procedure ClientSessionSign(Sender: TRestClientUri;
      var Call: TRestUriParams); override;
  end;

  /// secure authentication scheme using URL-level digital signature
  // - default suaCRC32 format of session_signature is
  // !Hexa8(SessionID)+
  // !Hexa8(Timestamp)+
  // !Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
  // !            Hexa8(Timestamp)+url))
  TRestClientAuthenticationSignedUri = class(TRestClientAuthenticationUri)
  protected
    // class functions implementing TRestAuthenticationSignedUriAlgo
    class function ComputeSignatureCrc32(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureCrc32c(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignaturexxHash(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureMd5(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSha1(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSha256(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSha512(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSha3(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
  public
    /// retrieve the method to compute the session_signature=.... value
    class function GetComputeSignature(
      algo: TRestAuthenticationSignedUriAlgo): TOnRestAuthenticationSignedUriComputeSignature;
    /// class method to be called on client side to sign an URI
    // - generate the digital signature as expected by overridden RetrieveSession()
    // - timestamp resolution is about 256 ms in the current implementation
    class procedure ClientSessionSign(Sender: TRestClientUri;
      var Call: TRestUriParams); override;
  end;

  /// mORMot secure RESTful authentication scheme
  // - match TRestServerAuthenticationDefault class on server side
  // - this scheme will use a password stored via safe SHA-256 hashing in the
  // TAuthUser ORM table
  TRestClientAuthenticationDefault = class(TRestClientAuthenticationSignedUri)
  protected
    /// class method used on client side to create a remote session
    // - will call the ModelRoot/Auth service, i.e. call TRestServer.Auth()
    // published method to create a session for this user: so
    // TRestServerAuthenticationDefault should be registered on server side
    // - User.LogonName and User.PasswordHashHexa will be checked
    class function ClientComputeSessionKey(Sender: TRestClientUri;
      User: TAuthUser): RawUtf8; override;
  end;

  /// mORMot weak RESTful authentication scheme
  // - this method will authenticate with a given username, but no signature
  // - match TRestServerAuthenticationNone class on server side
  // - on client side, this scheme is not called by TRestClientUri.SetUser()
  // method - so you have to write:
  // ! TRestClientAuthenticationNone.ClientSetUser(Client,'User','');
  TRestClientAuthenticationNone = class(TRestClientAuthenticationUri)
  protected
    /// class method used on client side to create a remote session
    // - will call the ModelRoot/Auth service, i.e. call TRestClient.Auth()
    // published method to create a session for this user: so
    // TRestServerAuthenticationNone should be registered on server side
    // - will check User.LogonName, but User.PasswordHashHexa will be ignored
    class function ClientComputeSessionKey(Sender: TRestClientUri;
      User: TAuthUser): RawUtf8; override;
  end;

  /// abstract class for implementing HTTP authentication
  // - do not use this abstract class, but e.g. TRestClientAuthenticationHttpBasic
  // - this class will transmit the session_signature as HTTP cookie, not at
  // URI level, so is expected to be used only from browsers or old clients
  TRestClientAuthenticationHttpAbstract = class(TRestClientAuthentication)
  protected
    /// should be overriden according to the HTTP authentication scheme
    class function ComputeAuthenticateHeader(
      const aUserName,aPasswordClear: RawUtf8): RawUtf8; virtual; abstract;
  public
    /// class method to be called on client side to sign an URI in Auth Basic
    // resolution is about 256 ms in the current implementation
    // - set "Cookie: mORMot_session_signature=..." HTTP header
    class procedure ClientSessionSign(Sender: TRestClientUri;
      var Call: TRestUriParams); override;
    /// class method to be used on client side to create a remote session
    // - call TRestClientAuthenticationHttpBasic.ClientSetUser() instead of
    // TRestClientUri.SetUser(), and never the method of this abstract class
    // - needs the plain aPassword, so aPasswordKind should be passClear
    // - returns true on success
    class function ClientSetUser(Sender: TRestClientUri;
      const aUserName, aPassword: RawUtf8;
      aPasswordKind: TRestClientSetUserPassword = passClear;
      const aHashSalt: RawUtf8 = ''; aHashRound: integer = 20000): boolean; override;
    /// class method to be used on client side to force the HTTP header for
    // the corresponding HTTP authentication, without creating any remote session
    // - call virtual protected method ComputeAuthenticateHeader()
    // - here the password should be given as clear content
    // - potential use case is to use a mORMot client through a HTTPS proxy,
    // e.g. with TRestClientAuthenticationHttpBasic authentication
    // - then you can use TRestClientAuthentication*.ClientSetUser() to
    // define any another "mORMot only" authentication
    // - this method is also called by the ClientSetUser() method of this class
    // for a full client + server authentication via HTTP
    // TRestClientAuthenticationHttp*.ClientSetUser()
    class procedure ClientSetUserHttpOnly(Sender: TRestClientUri;
      const aUserName, aPasswordClear: RawUtf8); virtual;
  end;

  /// authentication using HTTP Basic scheme
  // - this protocol send both name and password as clear (just Base64 encoded)
  // so should only be used over TLS / HTTPS, or for compatibility reasons
  // - match TRestServerAuthenticationHttpBasic class on server side
  // - will rely on TRestClientAuthenticationNone for authorization
  // - on client side, this scheme is not called by TRestClientUri.SetUser()
  // method - so you have to write:
  // ! TRestClientAuthenticationHttpBasic.ClientSetUser(Client,'User','password');
  // - for a remote proxy-only authentication (without creating any mORMot
  // session), you can write:
  // ! TRestClientAuthenticationHttpBasic.ClientSetUserHttpOnly(Client,'proxyUser','proxyPass');
  TRestClientAuthenticationHttpBasic = class(TRestClientAuthenticationHttpAbstract)
  protected
    /// this overriden method returns "Authorization: Basic ...." HTTP header
    class function ComputeAuthenticateHeader(
      const aUserName,aPasswordClear: RawUtf8): RawUtf8; override;
  end;

  {$ifdef DOMAINRESTAUTH}
  { will use mormot.lib.sspi/gssapi units depending on the OS }

  /// authentication of the current logged user using Windows Security Support
  // Provider Interface (SSPI) or GSSAPI library on Linux
  // - is able to authenticate the currently logged user on the client side,
  // using either NTLM (Windows only) or Kerberos - it will allow to safely
  // authenticate on a mORMot server without prompting the user to enter its
  // password
  // - match TRestServerAuthenticationSspi class on server side
  // - if ClientSetUser() receives aUserName as '', aPassword should be either
  // '' if you expect NTLM authentication to take place, or contain the SPN
  // registration (e.g. 'mymormotservice/myserver.mydomain.tld') for Kerberos
  // authentication
  // - if ClientSetUser() receives aUserName as 'DomainName\UserName', then
  // authentication will take place on the specified domain, with aPassword
  // as plain password value
  TRestClientAuthenticationSspi = class(TRestClientAuthenticationSignedUri)
  protected
    class function ClientComputeSessionKey(Sender: TRestClientUri;
      User: TAuthUser): RawUtf8; override;
  end;

  {$endif DOMAINRESTAUTH}

  /// store the information about the current session
  // - as set after a sucessfull TRestClientUri.SetUser() method
  TRestClientSession = record
  {$ifdef HASINLINE}
  private
  {$endif HASINLINE}
    // for internal use
    Authentication: TRestClientAuthenticationClass;
    IDHexa8: RawUtf8;
    PrivateKey: cardinal;
    Data: RawByteString;
    LastTick64: Int64;
  {$ifdef HASINLINE}
  public
  {$endif HASINLINE}
    /// the current user as set by SetUser() method
    // - contains nil if no User is currently authenticated
    // - once authenticated, a TAuthUser instance is set, with its ID,
    // LogonName, DisplayName, PasswordHashHexa and GroupRights (filled with a
    // TAuthGroup ID casted as a pointer) properties - you can retrieve any
    // optional binary data associated with this user via RetrieveBlobFields()
    User: TAuthUser;
    /// the current session ID as set after a successfull SetUser() method call
    // - equals 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED) if the session
    // is not started yet - i.e. if SetUser() call failed
    // - equals 1 (CONST_AUTHENTICATION_NOT_USED) if authentication mode
    // is not enabled - i.e. after a fresh Create() without SetUser() call
    ID: cardinal;
    /// access to the low-level HTTP header used for authentication
    // - you can force here your own header, e.g. a JWT as authentication bearer
    // or as in TRestClientAuthenticationHttpAbstract.ClientSetUserHttpOnlyUser
    // - used e.g. by TRestClientAuthenticationHttpBasic
    HttpHeader: RawUtf8;
    /// the remote server executable name, as retrieved after a SetUser() success
    Server: RawUtf8;
    /// the remote server version, as retrieved after a SetUser() success
    Version: RawUtf8;
    /// the remote server session tiemout in minutes, as retrieved after
    // a SetUser() success
    // - will be used to set SessionHeartbeatSeconds default
    ServerTimeout: integer;
    /// frequency of Callback/_ping_ calls to maintain session and services
    // - will be used to call SessionRenewEvent at the specified period, so that
    // the session and all sicClientDriven instances will be maintained on the
    // server side as long as the client connection stands
    // - equals half SessionServerTimeout or 25 minutes (if lower) by default -
    // 25 minutes matches the default service timeout of 30 minutes
    // - you may set 0 to disable this SOA-level heartbeat feature
    HeartbeatSeconds: integer;
  end;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

  TSqlRestServerAuthenticationClientSetUserPassword = TRestClientSetUserPassword;
  TSqlRestServerAuthenticationSignedUriAlgo = TRestAuthenticationSignedUriAlgo;
  TSqlRestServerAuthenticationSignedUriComputeSignature  =
    TOnRestAuthenticationSignedUriComputeSignature;
  // TRestServerAuthentication* classes have client-side only corresponding
  // types named as TRestClientAuthentication*

{$endif PUREMORMOT2}



{ ************ TRestClientRoutingRest/TRestClientRoutingJsonRpc Routing Schemes }

  //// used to customize TRestClientRouting.ClientSideInvoke process
  TRestClientSideInvoke = set of (
    csiAsOctetStream);

  /// abstract Client side service routing
  // - match TRestServerUriContext reciprocal class
  // - never use this abstract class, but rather TRestClientRoutingRest or
  // TRestClientRoutingJsonRpc classes
  TRestClientRouting = class
  public
    /// at Client Side, compute URI and BODY according to the routing scheme
    // - abstract implementation which is to be overridden
    // - as input, method should be the method name to be executed,
    // params should contain the incoming parameters as JSON CSV (without []),
    // and clientDriven ID should contain the optional Client ID value
    // - at output, should update the HTTP uri corresponding to the proper
    // routing, and should return the corresponding HTTP body/headers within
    // sent/head parameters
    class procedure ClientSideInvoke(var uri: RawUtf8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUtf8;
      out sent, head: RawUtf8); virtual; abstract;
  end;

  /// class used to define the Client side expected routing
  // - match TRestServerUriContextClass reciprocal meta-class
  // - most of the internal methods are declared as virtual, so it allows any
  // kind of custom routing or execution scheme
  // - TRestClientRoutingRest and TRestClientRoutingJsonRpc classes are provided
  // in this unit, to allow RESTful and JSON/RPC protocols on Client side
  // - you can retrieve the client class from the reciprocal server-side class
  // using TRestServerUriContext.ClientRouting class method
  TRestClientRoutingClass = class of TRestClientRouting;

  /// client calling context using simple REST for interface-based services
  // - match TRestServerRoutingRest reciprocal class
  TRestClientRoutingRest = class(TRestClientRouting)
    /// at Client Side, compute URI and BODY according to RESTful routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='1,2' and
    // clientDrivenID='1234' -> on output uri='root/Calculator.Add/1234' and
    // sent='[1,2]'
    class procedure ClientSideInvoke(var uri: RawUtf8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUtf8;
      out sent, head: RawUtf8); override;
  end;

  /// client calling context using simple REST for interface-based services
  // - match TRestServerRoutingJsonRpc reciprocal class
  TRestClientRoutingJsonRpc = class(TRestClientRouting)
     /// at Client Side, compute URI and BODY according to JSON/RPC routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='1,2' and
    // clientDrivenID='1234' -> on output uri='root/Calculator' and
    // sent={"method":"Add","params":[1,2],"id":1234}
    class procedure ClientSideInvoke(var uri: RawUtf8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUtf8;
      out sent, head: RawUtf8); override;
  end;

// backward compatibility types redirections
{$ifndef PUREMORMOT2}

  TSqlRestServerUriContextClientInvoke = TRestClientSideInvoke;

{$endif PUREMORMOT2}


{ ************ TRestClientUri Base Class for Actual Clients }

  /// called by TRestClientUri.Uri() when an error occurred
  // - so that you may have a single entry point for all client-side issues
  // - information will be available in Sender's LastErrorCode and
  // LastErrorMessage properties
  // - if the error comes from an Exception, it will be supplied as parameter
  // - the REST context (if any) will be supplied within the Call parameter,
  // and in this case Call^.OutStatus=HTTP_NOTIMPLEMENTED indicates a broken
  // connection
  TOnClientFailed = procedure(Sender: TRestClientUri; E: Exception;
    Call: PRestUriParams) of object;

  /// 31-bit positive identifier of an interface callback, as sent to the server
  TRestClientCallbackID = type integer;

  /// store information about registered interface callbacks
  TRestClientCallbackItem = record
    /// the identifier of the callback, as sent to the server side
    // - computed from TRestClientUriCallbacks.fCurrentID counter
    ID: TRestClientCallbackID;
    /// weak pointer typecast to the associated IInvokable variable
    Instance: pointer;
    //// information about the associated IInvokable
    Factory: TInterfaceFactory;
    /// set to TRUE if the instance was released from the server
    ReleasedFromServer: boolean;
  end;

  /// points to information about registered interface callbacks
  PRestClientCallbackItem = ^TRestClientCallbackItem;

  /// store the references to active interface callbacks on a REST Client
  TRestClientCallbacks = class(TSynPersistent)
  protected
    fSafe: TLightLock;
    fCurrentID: integer; // thread-safe TRestClientCallbackID sequence generator
    function UnRegisterByIndex(index: integer): boolean;
  public
    /// the associated REST instance
    Owner: TRestClientUri;
    /// how many callbacks are registered
    Count: integer;
    /// list of registered interface callbacks
    List: array of TRestClientCallbackItem;
    /// initialize the storage list
    constructor Create(aOwner: TRestClientUri); reintroduce;
    /// register a callback event interface instance from a new computed ID
    function DoRegister(aInstance: pointer;
      aFactory: TInterfaceFactory): TRestClientCallbackID; overload;
    /// register a callback event interface instance from its supplied ID
    procedure DoRegister(aID: TRestClientCallbackID; aInstance: pointer;
      aFactory: TInterfaceFactory); overload;
    /// delete all callback events from the internal list, as specified by its instance
    // - note that the same IInvokable instance may be registered for several IDs
    function UnRegister(aInstance: pointer): boolean; overload;
    /// find the index of the ID in the internal list
    // - warning: this method should be called within Safe.Lock/Safe.Unlock
    function FindIndex(aID: TRestClientCallbackID): PtrInt;
    /// find a matching callback
    // - will call FindIndex(aItem.ID) within Safe.Lock/Safe.Unlock
    // - returns TRUE if aItem.ID was found and aItem filled, FALSE otherwise
    function FindEntry(var aItem: TRestClientCallbackItem): boolean;
    /// find a matching entry
    // - will call FindIndex(aID) within Safe.Lock/Safe.Unlock
    // - returns TRUE if aID was found and aInstance/aFactory set, FALSE otherwise
    function FindAndRelease(aID: TRestClientCallbackID): boolean;
  end;

  /// signature e.g. of the TRestClientUri.OnSetUser event handler
  TOnClientNotify = procedure(Sender: TRestClientUri) of object;

  /// a generic REpresentational State Transfer (REST) client with URI
  // - URI are standard Collection/Member implemented as ModelRoot/TableName/TableID
  // - handle RESTful commands GET POST PUT DELETE LOCK UNLOCK
  // - never call this abstract class, but inherit and override the
  // InternalUri/InternalIsOpen/InternalOpen/InternalClose virtual abstract methods
  TRestClientUri = class(TRest)
  protected
    fClient: IRestOrmClient;
    fSession: TRestClientSession;
    fComputeSignature: TOnRestAuthenticationSignedUriComputeSignature;
    fOnConnected: TOnClientNotify;
    fOnConnectionFailed: TOnClientFailed;
    fOnIdle: TOnIdleSynBackgroundThread;
    fOnFailed: TOnClientFailed;
    fOnAuthentificationFailed: TOnAuthentificationFailed;
    fOnSetUser: TOnClientNotify;
    fBackgroundThread: TSynBackgroundThreadEvent;
    fConnectRetrySeconds: integer; // used by IsOpen
    fMaximumAuthentificationRetry: integer;
    fRetryOnceOnTimeout: boolean;
    fInternalState: set of (isDestroying, isInAuth, isNotImplemented);
    fLastErrorCode: integer;
    fLastErrorMessage: RawUtf8;
    fLastErrorException: ExceptClass;
    fSafe: IAutoLocker; // to make the Uri() method thread-safe
    fRemoteLogClass: TSynLog;
    fRemoteLogThread: TObject; // private TRemoteLogThread
    fRemoteLogOwnedByFamily: boolean;
    fServicesRouting: TRestClientRoutingClass;
    fServicePublishOwnInterfaces: RawUtf8;
    fFakeCallbacks: TRestClientCallbacks;
    {$ifdef OSWINDOWS}
    fServiceNotificationMethodViaMessages: record
      Wnd: HWND;
      Msg: cardinal;
    end;
    {$endif OSWINDOWS}
    procedure SetRoutingClass(aServicesRouting: TRestClientRoutingClass);
    procedure SetSessionHeartbeatSeconds(timeout: integer);
    function GetOnIdleBackgroundThreadActive: boolean;
    procedure OnBackgroundProcess(Sender: TSynBackgroundThreadEvent;
      ProcessOpaqueParam: pointer);
    procedure SetLastException(E: Exception = nil; ErrorCode: integer = HTTP_BADREQUEST;
      Call: PRestUriParams = nil);
    function InternalRemoteLogSend(const aText: RawUtf8): boolean;
    procedure InternalNotificationMethodExecute(var Ctxt: TRestUriParams); virtual;
    /// will call timestamp/info if the session has currently not been retrieved
    function GetSessionVersion: RawUtf8;
    // register the user session to the TRestClientUri instance
    function SessionCreate(aAuth: TRestClientAuthenticationClass;
      var aUser: TAuthUser; const aSessionKey: RawUtf8): boolean;
    // call each fSession.HeartbeatSeconds delay
    procedure SessionRenewEvent(Sender: TSynBackgroundTimer; const Msg: RawUtf8);
    /// abstract methods to be implemented with a local, piped or HTTP/1.1 provider
    // - you can specify some POST/PUT data in Call.OutBody (leave '' otherwise)
    // - return the execution result in Call.OutStatus
    // - for clients, RestAccessRights is never used
    procedure InternalUri(var Call: TRestUriParams); virtual; abstract;
    /// overridden protected method shall check if client is connected
    function InternalIsOpen: boolean; virtual; abstract;
    /// overridden protected method shall reopen the connectio
    procedure InternalOpen; virtual; abstract;
    /// overridden protected method shall force the connection to be closed,
    // - a next call to IsOpen method shall re-open the connection
    procedure InternalClose; virtual; abstract;
  {$ifndef PUREMORMOT2}
    // backward compatibility redirections to the homonymous IRestOrmClient methods
    // see IRestOrmClient documentation for the proper use information
  public
    function Refresh(aID: TID; Value: TOrm; var Refreshed: boolean): boolean;
    function List(const Tables: array of TOrmClass; const SqlSelect: RawUtf8 = 'RowID';
      const SqlWhere: RawUtf8 = ''): TOrmTable;
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8; const Args: array of const): TOrmTable; overload;
    function ListFmt(const Tables: array of TOrmClass;
      const SqlSelect, SqlWhereFormat: RawUtf8; const Args, Bounds: array of const): TOrmTable; overload;
    function TransactionBeginRetry(aTable: TOrmClass; Retries: integer = 10): boolean;
    function BatchStart(aTable: TOrmClass;
      AutomaticTransactionPerRow: cardinal = 0;
      Options: TRestBatchOptions = []): boolean;
    function BatchStartAny(AutomaticTransactionPerRow: cardinal;
      Options: TRestBatchOptions = []): boolean;
    function BatchAdd(Value: TOrm; SendData: boolean; ForceID: boolean = false;
      const CustomFields: TFieldBits = []): integer;
    function BatchUpdate(Value: TOrm; const CustomFields: TFieldBits = [];
      DoNotAutoComputeFields: boolean = false): integer;
    function BatchDelete(ID: TID): integer; overload;
    function BatchDelete(Table: TOrmClass; ID: TID): integer; overload;
    function BatchCount: integer;
    function BatchSend(var Results: TIDDynArray): integer; overload;
    procedure BatchAbort;
  {$endif PUREMORMOT2}
  public
    /// initialize REST client instance
    constructor Create(aModel: TOrmModel); override;
    /// initialize REST client instance from a TSynConnectionDefinition
    constructor RegisteredClassCreateFrom(aModel: TOrmModel;
      aDefinition: TSynConnectionDefinition;
      aServerHandleAuthentication: boolean); override;
    /// release memory and close client connection
    // - also unlock all still locked records by this client
    destructor Destroy; override;
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TRestOrmParent); override;
    /// save the TRestClientUri properties into a persistent storage object
    // - CreateFrom() will expect Definition.UserName/Password to store the
    // credentials which will be used by SetUser()
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;
    /// check if connected to the server, or try to (re)create the connection
    // - convenient wrapper around InternalIsOpen and InternalOpen virtual methods
    // - return TRUE on success, FALSE on any connection error
    // - follows ConnectRetrySeconds property for optional retrial
    // - calls OnConnected/OnConnectionFailed events if set
    function IsOpen: boolean; virtual;
    /// main method calling the remote Server via a RESTful command
    // - redirect to the InternalUri() abstract method, which should be
    // overridden for local, pipe, HTTP/1.1 or WebSockets actual communication
    // - this method will sign the url with the appropriate digital signature
    // according to the current SessionUser property
    // - this method will retry the connection in case of authentication failure
    // (i.e. if the session was closed by the remote server, for any reason -
    // mostly a time out) if the OnAuthentificationFailed event handler is set
    function Uri(const url, method: RawUtf8; Resp: PRawUtf8 = nil;
      Head: PRawUtf8 = nil; SendData: PRawUtf8 = nil;
      OutInternalState: PCardinal = nil): integer;
    /// wrapper to the protected URI method to call a method on the server, using
    // a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - this version will use a GET with supplied parameters (which will be
    // encoded with the URL), and append the expected signature (if needed)
    function CallBackGet(const aMethodName: RawUtf8;
      const aNameValueParameters: array of const;
      out aResponse: RawUtf8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUtf8 = nil): integer;
    /// wrapper to the protected URI method to call a method on the server, using
    // a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    // - returns the UTF-8 decoded JSON result (server must reply with one
    // "result":"value" JSON object)
    // - this version will use a GET with supplied parameters (which will be
    // encoded with the URL), and append the expected signature (if needed)
    function CallBackGetResult(const aMethodName: RawUtf8;
      const aNameValueParameters: array of const;
      aTable: TOrmClass = nil; aID: TID = 0): RawUtf8;
    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful PUT request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - this version will use a PUT with the supplied raw UTF-8 data
    function CallBackPut(const aMethodName, aSentData: RawUtf8;
      out aResponse: RawUtf8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUtf8 = nil): integer;
    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful with any kind of request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - for GET/PUT methods, you should better use CallBackGet/CallBackPut
    function CallBack(method: TUriMethod; const aMethodName,aSentData: RawUtf8;
      out aResponse: RawUtf8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUtf8 = nil): integer;
    /// to be called before CallBack() if the client could ignore the answer
    // - do nothing by default, but overriden e.g. in TRestHttpClientWebsockets
    procedure CallbackNonBlockingSetHeader(out Header: RawUtf8); virtual;

    /// access or initialize the internal IoC resolver, used for interface-based
    // remote services, and more generaly any Services.Resolve() call
    // - create and initialize the internal TServiceContainerClient if no
    // service interface has been registered yet
    // - may be used to inject some dependencies, which are not interface-based
    // remote services, but internal IoC, without the ServiceRegister()
    // or ServiceDefine() methods - e.g.
    // ! aRest.ServiceContainer.InjectResolver([TInfraRepoUserFactory.Create(aRest)],true);
    function ServiceContainer: TServiceContainer; override;
    /// register one or several Services on the client side via their interfaces
    // - this methods expects a list of interfaces to be registered to the client
    // (e.g. [TypeInfo(IMyInterface)])
    // - instance implementation pattern will be set by the appropriate parameter
    // - will return true on success, false if registration failed (e.g. if any of
    // the supplied interfaces is not correct or is not available on the server)
    // - that is, server side will be called to check for the availability of
    // each interface
    // - you can specify an optional custom contract for the first interface
    function ServiceRegister(const aInterfaces: array of PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): boolean; overload; virtual;
    /// register a Service on the client side via its interface
    // - this methods expects one interface to be registered to the client, as
    // ! Client.ServiceRegister(TypeInfo(IMyInterface),sicShared);
    // - instance implementation pattern will be set by the appropriate parameter
    // - will return the corresponding fake class factory on success, nil if
    // registration failed (e.g. if any of supplied interfaces is not correct or
    // is not available on the server)
    // - that is, server side will be called to check for the availability of
    // each interface
    // - you can specify an optional custom contract for the first interface
    function ServiceRegister(aInterface: PRttiInfo;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = '';
      aIgnoreAnyException: boolean = true): TServiceFactory; overload;
    /// register and retrieve the sicClientDriven Service instance
    // - will return TRUE on success, filling Obj output variable with the
    // corresponding interface instance
    // - will return FALSE on error
    function ServiceRegisterClientDriven(aInterface: PRttiInfo; out Obj;
      const aContractExpected: RawUtf8 = ''): boolean; overload;
    /// register one or several Services on the client side via their interfaces
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefine(const aInterfaces: array of TGuid;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = ''): boolean; overload;
    /// register a Service on the client side via its interface
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefine(const aInterface: TGuid;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUtf8 = '';
      aIgnoreAnyException: boolean = true): TServiceFactoryClient; overload;
    /// register and retrieve the sicClientDriven Service instance
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefineClientDriven(const aInterface: TGuid; out Obj;
      const aContractExpected: RawUtf8 = ''): boolean;
    /// register a sicShared Service instance communicating via JSON objects
    // - will force SERVICE_CONTRACT_NONE_EXPECTED, ParamsAsJsonObject=true and
    // ResultAsJsonObjectWithoutResult=true
    // - may be used e.g. for accessing a sessionless public REST/JSON API, i.e.
    // ! TRestServer.ServiceDefine(...).ResultAsJsonObjectWithoutResult := true
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - aIgnoreAnyException may be set to TRUE if the server is likely
    // to not propose this service, and any exception is to be catched
    function ServiceDefineSharedApi(const aInterface: TGuid;
      const aContractExpected: RawUtf8 = SERVICE_CONTRACT_NONE_EXPECTED;
      aIgnoreAnyException: boolean = false): TServiceFactoryClient;
    /// allow to notify a server the services this client may be actually capable
    // - when this client will connect to a remote server to access its services,
    // it will register its own services, supplying its TRestServer instance,
    // and its corresponding public URI, within its '_contract_' internal call
    // - it will allow automatic service discovery of Peer To Peer Servers,
    // without the need of an actual centralized SOA catalog service: any
    // client could retrieve an associated REST server for a given service,
    // via the ServiceRetrieveAssociated method
    property ServicePublishOwnInterfaces: RawUtf8
      read fServicePublishOwnInterfaces write fServicePublishOwnInterfaces;
    /// return all REST server URI associated to this client, for a given
    // service name, the latest registered in first position
    // - will lookup for the Interface name without the initial 'I', e.g.
    // 'Calculator' for ICalculator - warning: research is case-sensitive
    // - this methods is the reverse from ServicePublishOwnInterfaces: it allows
    // to guess an associated REST server which may implement a given service
    function ServiceRetrieveAssociated(const aServiceName: RawUtf8;
      out URI: TRestServerURIDynArray): boolean; overload;
    /// return all REST server URI associated to this client, for a given service
    // - here the service is specified as its TGuid, e.g. IMyInterface
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - the URI[] output array contains the matching server URIs, the latest
    // registered in first position
    // - this methods is the reverse from ServicePublishOwnInterfaces: it allows
    // to guess an associated REST server which may implement a given service
    function ServiceRetrieveAssociated(const aInterface: TGuid;
      out URI: TRestServerUriDynArray): boolean; overload;
    /// the routing class of the service remote request on client side
    // - by default, contains TRestClientRoutingRest, i.e. an URI-based
    // layout which is secure (since will use our RESTful authentication scheme),
    // and also very fast
    // - but TRestClientRoutingJsonRpc can e.g. be set (with
    // TRestServerRoutingJsonRpc on server sides), if the client will rather
    // use JSON/RPC alternative pattern
    // - NEVER set the abstract TRestClientRouting class on this property
    property ServicesRouting: TRestClientRoutingClass
      read fServicesRouting write SetRoutingClass;
    // internal methods used by mormot.soa.client
    function FakeCallbackRegister(Sender: TServiceFactory;
      const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
      ParamValue: Pointer): TRestClientCallbackID; virtual;
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: TRestClientCallbackID; Instance: pointer): boolean; virtual;
    property FakeCallbacks: TRestClientCallbacks
      read fFakeCallbacks;

    /// you can call this method to call the remote URI root/Timestamp
    // - this can be an handy way of testing the connection, since this method
    // is always available, even without authentication
    // - returns TRUE if the client time correction has been retrieved
    // - returns FALSE on any connection error - check LastErrorMessage and
    // LastErrorException to find out the exact connection error
    function ServerTimestampSynchronize: boolean;
    /// asynchronous call a 'RemoteLog' remote logging method on the server
    // - as implemented by mORMot's LogView tool in server mode
    // - to be used via ServerRemoteLogStart/ServerRemoteLogStop methods
    // - a dedicated background thread will run the transmission process without
    // blocking the main program execution, gathering log rows in chunks in case
    // of high activity
    // - map TOnTextWriterEcho signature, so that you will be able to set e.g.:
    // ! TSqlLog.Family.EchoCustom := aClient.ServerRemoteLog;
    function ServerRemoteLog(Sender: TEchoWriter; Level: TSynLogInfo;
      const Text: RawUtf8): boolean; overload; virtual;
    /// internal method able to emulate a call to TSynLog.Add.Log()
    // - will compute timestamp and event text, than call the overloaded
    // ServerRemoteLog() method
    function ServerRemoteLog(Level: TSynLogInfo; const FormatMsg: RawUtf8;
      const Args: array of const): boolean; overload;
    /// start to send all logs to the server 'RemoteLog' method-based service
    // - will associate the EchoCustom callback of the running log class to the
    // ServerRemoteLog() method
    // - if aClientOwnedByFamily is TRUE, this TRestClientUri instance
    // lifetime will be managed by TSynLogFamily - which is mostly wished
    // - if aClientOwnedByFamily is FALSE, you should manage this instance
    // life time, and may call ServerRemoteLogStop to stop remote logging
    // - warning: current implementation will disable all logging for this
    // TRestClientUri instance, to avoid any potential concern (e.g. for
    // multi-threaded process, or in case of communication error): you should
    // therefore use this TRestClientUri connection only for the remote log
    // server, e.g. via TRestHttpClientGeneric.CreateForRemoteLogging() - do
    // not call ServerRemoteLogStart() from a high-level business client!
    procedure ServerRemoteLogStart(aLogClass: TSynLogClass;
      aClientOwnedByFamily: boolean);
    /// stop sending all logs to the server 'RemoteLog' method-based service
    // - do nothing if aClientOwnedByFamily was TRUE for ServerRemoteLogStart
    procedure ServerRemoteLogStop;
    /// authenticate an User to the current connected Server
    // - will call the ModelRoot/Auth service, i.e. call TRestServer.Auth()
    // published method to create a session for this user, with our secure
    // TRestClientAuthenticationDefault authentication scheme
    // - returns true on success
    // - calling this method is optional, depending on your user right policy:
    // your Server need to handle authentication
    // - if saoUserByLogonOrID is defined in the server Options, aUserName may
    // be a TAuthUser.ID integer value and not a TAuthUser.LogonName
    // - on success, the SessionUser property map the logged user session on the
    // server side
    // - if aHashedPassword is TRUE, the aPassword parameter is expected to
    // contain the already-hashed value, just as stored in PasswordHashHexa
    // (i.e. Sha256('salt'+Value) as in TAuthUser.SetPasswordPlain method)
    // - if SSPIAUTH conditional is defined, and aUserName='', a Windows
    // authentication will be performed via TRestClientAuthenticationSspi -
    // in this case, aPassword will contain the SPN domain for Kerberos
    // (otherwise NTLM will be used), and table TAuthUser shall contain
    // an entry for the logged Windows user, with the LoginName in form
    // 'DomainName\UserName'
    // - you can directly create the class method ClientSetUser() of a given
    // TRestClientAuthentication inherited class, if neither
    // TRestClientAuthenticationDefault nor TRestClientAuthenticationSspi
    // match your need
    function SetUser(const aUserName, aPassword: RawUtf8;
      aHashedPassword: boolean = false): boolean;
    /// clear session and call the /auth service on the server to notify shutdown
    // - is called by Destroy and SetUser/ClientSetUser methods, so you should
    // not have usually to call this method directly
    procedure SessionClose;
    /// internal method to retrieve the current Session TAuthUser.ID
    function GetCurrentSessionUserID: TID; override;
    /// customize the session_signature signing algorithm with a specific function
    // - will be used by TRestServerAuthenticationSignedUri classes,
    // e.g. TRestServerAuthenticationDefault instead of the algorithm
    // specified by the server at session handshake
    property ComputeSignature: TOnRestAuthenticationSignedUriComputeSignature
      read fComputeSignature write fComputeSignature;
    /// the current session information as set by a successfull SetUser() call
    property Session: TRestClientSession
      read fSession;
    /// the current user as set by SetUser() method
    // - contains nil if no User is currently authenticated
    property SessionUser: TAuthUser
      read fSession.User;
    /// access to the low-level HTTP header used for authentication
    // - you can force here your own header, e.g. a JWT as authentication bearer
    // or as in TRestClientAuthenticationHttpAbstract.ClientSetUserHttpOnlyUser
    property SessionHttpHeader: RawUtf8
      read fSession.HttpHeader write fSession.HttpHeader;
    /// main access to the IRestOrmClient methods of this instance
    property Client: IRestOrmClient
      read fClient;
    /// main access to the class implementing IRestOrm methods for this instance
    // - used internally to avoid ORM: IRestOrm reference counting and
    // enable inlining of most simple methods, if possible
    function OrmInstance: TRestOrm;
      {$ifdef HASINLINE}inline;{$endif}

    {$ifdef OSWINDOWS}

    /// set a HWND/WM_* pair to let interface-based services notification
    // callbacks be processed safely in the main UI thread, via Windows messages
    // - by default callbacks are executed in the transmission thread, e.g.
    // the WebSockets client thread: using VCL Synchronize() method may
    // trigger some unexpected race conditions, e.g. when asynchronous
    // notifications are received during a blocking REST command - this
    // message-based mechanism will allow safe and easy notification for
    // any VCL client application
    // - the associated ServiceNotificationMethodExecute() method shall be
    // called in the client HWND TForm for the defined WM_* message
    procedure ServiceNotificationMethodViaMessages(hWnd: HWND; Msg: cardinal);
    /// event to be triggered when a WM_* message is received from
    // the internal asynchronous notification system, to run the callback
    // in the main UI thread
    // - WM_* message identifier should have been set e.g. via the associated
    // $ ServiceNotificationMethodViaMessages(Form.Handle, WM_USER)
    // - message will be sent for any interface-based service method callback
    // which expects no result (i.e. no out parameter nor function result),
    // so is safely handled as asynchronous notification
    // - is defines as a class procedure, since the underlying TRestClientUri
    // instance has no impact here: a single WM_* handler is enough for
    // several TRestClientUri instances
    class procedure ServiceNotificationMethodExecute(var Msg: TMessage);

    {$endif OSWINDOWS}
    /// called by IsOpen when the raw connection is (re)established
    property OnConnected: TOnClientNotify
      read fOnConnected write fOnConnected;
    /// called by IsOpen when it failed to connect
    property OnConnectionFailed: TOnClientFailed
      read fOnConnectionFailed write fOnConnectionFailed;
    /// set a callback event to be executed in loop during remote blocking
    // process, e.g. to refresh the UI during a somewhat long request
    // - if not set, the request will be executed in the current thread,
    // so may block the User Interface
    // - you can assign a callback to this property, calling for instance
    // Application.ProcessMessages, to execute the remote request in a
    // background thread, but let the UI still be reactive: the
    // TLoginForm.OnIdleProcess and OnIdleProcessForm methods of
    // mORMotUILogin.pas will match this property expectations
    property OnIdle: TOnIdleSynBackgroundThread
      read fOnIdle write fOnIdle;
    /// TRUE if the background thread is active, and OnIdle event is called
    // during process
    // - to be used e.g. to ensure no re-entrance from User Interface messages
    property OnIdleBackgroundThreadActive: boolean
      read GetOnIdleBackgroundThreadActive;
    /// this Event is called in case of remote authentication failure
    // - client software can ask the user to enter a password and user name
    // - if no event is specified, the Uri() method will return directly
    // an HTTP_FORBIDDEN "403 Forbidden" error code
    property OnAuthentificationFailed: TOnAuthentificationFailed
      read fOnAuthentificationFailed write fOnAuthentificationFailed;
    /// this Event is called if Uri() was not successfull
    // - the callback will have all needed information
    // - e.g. Call^.OutStatus=HTTP_NOTIMPLEMENTED indicates a broken connection
    property OnFailed: TOnClientFailed
      read fOnFailed write fOnFailed;
    /// this Event is called when a user is authenticated
    // - is called always, on each TRestClientUri.SetUser call
    // - you can check the Sender.SessionUser property pointing to the current
    // authenticated user, or nil if authentication failed
    // - could be used to refresh the User Interface layout according to
    // current authenticated user rights, or to subscribe to some services
    // via callbacks
    property OnSetUser: TOnClientNotify
      read fOnSetUser write fOnSetUser;
  published
    /// low-level error code, as returned by server
    // - check this value about HTTP_* constants
    // - HTTP_SUCCESS or HTTP_CREATED mean no error
    // - otherwise, check LastErrorMessage property for additional information
    // - this property value will record status codes returned by Uri() method
    property LastErrorCode: integer
      read fLastErrorCode;
    /// low-level error message, as returned by server
    // - this property value will record content returned by Uri() method in
    // case of an error, or '' if LastErrorCode is HTTP_SUCCESS or HTTP_CREATED
    property LastErrorMessage: RawUtf8
      read fLastErrorMessage;
    /// low-level exception class, if any
    // - will record any Exception class raised within Uri() method
    // - contains nil if Uri() execution did not raise any exception (which
    // is the most expected behavior, since server-side errors are trapped
    // into LastErrorCode/LastErrorMessage properties
    property LastErrorException: ExceptClass
      read fLastErrorException;
    /// maximum additional retry occurence
    // - defaut is 1, i.e. will retry once
    // - set OnAuthentificationFailed to nil in order to avoid any retry
    property MaximumAuthentificationRetry: integer
      read fMaximumAuthentificationRetry write fMaximumAuthentificationRetry;
    /// if the client shall retry once after "408 REQUEST TIMEOUT" server error
    // - this is about an HTTP error 408 returned by the server, e.g. when the
    // ORM lock or transaction could not be acquired in a good enough time: this
    // value does not apply to the client side timeout, e.g. at HTTP level
    property RetryOnceOnTimeout: boolean
      read fRetryOnceOnTimeout write fRetryOnceOnTimeout;
    /// how many seconds the client may try to connect after open socket failure
    // - is disabled to 0 by default, but you may set some seconds here e.g. to
    // let the server start properly, and let the client handle exceptions to
    // wait and retry until the specified timeout is reached
    property ConnectRetrySeconds: integer
      read fConnectRetrySeconds write fConnectRetrySeconds;
    /// the current session ID as set after a successfull SetUser() method call
    // - equals 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED) if the session
    // is not started yet - i.e. if SetUser() call failed
    // - equals 1 (CONST_AUTHENTICATION_NOT_USED) if authentication mode
    // is not enabled - i.e. after a fresh Create() without SetUser() call
    property SessionID: cardinal
      read fSession.ID;
    /// the remote server executable name, as retrieved after a SetUser() success
    property SessionServer: RawUtf8
      read fSession.Server;
    /// the remote server version, as retrieved after a SetUser() success
    property SessionVersion: RawUtf8
      read fSession.Version;
    /// the remote server session tiemout in minutes, as retrieved after
    // a SetUser() success
    // - will be used to set SessionHeartbeatSeconds default
    property SessionServerTimeout: integer
      read fSession.ServerTimeout;
    /// frequency of Callback/_ping_ calls to maintain session and services
    // - will be used to call SessionRenewEvent at the specified period, so that
    // the session and all sicClientDriven instances will be maintained on the
    // server side as long as the client connection stands
    // - equals half SessionServerTimeout or 25 minutes (if lower) by default -
    // 25 minutes matches the default service timeout of 30 minutes
    // - you may set 0 to disable this SOA-level heartbeat feature
    property SessionHeartbeatSeconds: integer
      read fSession.HeartbeatSeconds write SetSessionHeartbeatSeconds;
  end;

const
  REST_COOKIE_SESSION = 'mORMot_session_signature';


function ToText(a: TRestAuthenticationSignedUriAlgo): PShortString; overload;



{ ************ TRestClientLibraryRequest after TRestServer.ExportServerGlobalLibraryRequest }

type
  /// REST client with direct access to a server logic through a .dll/.so library
  // - use only one TLibraryRequest function for the whole communication
  // - the data is stored in Global system memory, and freed by GlobalFree()
  TRestClientLibraryRequest = class(TRestClientUri)
  protected
    fRequest: TLibraryRequest;
    /// used by Create(from dll) constructor
    fLibraryHandle: TLibHandle;
    /// method calling the RESTful server through a DLL or executable, using
    // direct memory
    procedure InternalUri(var Call: TRestUriParams); override;
    /// overridden protected method do nothing (direct DLL access has no connection)
    function InternalIsOpen: boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
  public
    /// connect to a server from a remote function
    constructor Create(aModel: TOrmModel; aRequest: TLibraryRequest); reintroduce; overload;
    /// connect to a server contained in a shared library
    // - this .dll/.so must contain at least a LibraryRequest entry
    // - raise an exception if the shared library is not found or invalid
    constructor Create(aModel: TOrmModel; const LibraryName: TFileName); reintroduce; overload;
    /// release memory and handles
    destructor Destroy; override;
  end;


// backward compatibility types redirections
{$ifndef PUREMORMOT2}

type
  TSqlRestClientUri = TRestClientUri;
  TSqlRestClientUriDll = TRestClientLibraryRequest;

{$endif PUREMORMOT2}


{ ************ TInterfacedCallback/TBlockingCallback Classes }

  /// TInterfacedObject class which will notify a REST server when it is released
  // - could be used when implementing event callbacks as interfaces, so that
  // the other side instance will be notified when it is destroyed
  TInterfacedCallback = class(TInterfacedObjectLocked)
  protected
    fRest: TRest;
    fInterface: TGuid;
  public
    /// initialize the instance for a given REST client and callback interface
    constructor Create(aRest: TRest; const aGuid: TGuid); reintroduce;
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
    property RestInterface: TGuid
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
      aRest: TRest; const aGuid: TGuid); reintroduce;
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


implementation

uses
  mormot.orm.client; // for injection of TRestOrmClientUri.Uri field


{ ************ Client Authentication and Authorization Logic }

function ToText(a: TRestAuthenticationSignedUriAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestAuthenticationSignedUriAlgo), ord(a));
end;


{ TRestClientAuthentication }

const
  AUTH_N: array[0..9] of PUtf8Char = (
    'result',        // 0
    'data',          // 1
    'server',        // 2
    'version',       // 3
    'logonid',       // 4
    'logonname',     // 5
    'logondisplay',  // 6
    'logongroup',    // 7
    'timeout',       // 8
    'algo');         // 9

class function TRestClientAuthentication.ClientGetSessionKey(
  Sender: TRestClientUri; User: TAuthUser;
  const aNameValueParameters: array of const): RawUtf8;
var
  resp: RawUtf8;
  values: array[0..high(AUTH_N)] of TValuePUtf8Char;
  a: integer;
  algo: TRestAuthenticationSignedUriAlgo absolute a;
begin
  if (Sender.CallBackGet('auth', aNameValueParameters, resp) <> HTTP_SUCCESS) or
     (JsonDecode(pointer({%H-}resp), @AUTH_N, length(AUTH_N), @values) = nil) then
  begin
    Sender.fSession.Data := ''; // reset temporary 'data' field
    result := ''; // error
  end
  else
  begin
    result := values[0].ToUtf8; // not ToUtf8(result) to please Delphi 2007
    Base64ToBin(PAnsiChar(values[1].Text), values[1].Len, Sender.fSession.Data);
    values[2].ToUtf8(Sender.fSession.Server);
    values[3].ToUtf8(Sender.fSession.Version);
    User.IDValue := values[4].ToInt64;
    User.LogonName := values[5].ToUtf8; // set/fix using values from server
    User.DisplayName := values[6].ToUtf8;
    User.GroupRights := pointer(values[7].ToInteger);
    Sender.fSession.ServerTimeout := values[8].ToInteger;
    if Sender.fSession.ServerTimeout <= 0 then
      Sender.fSession.ServerTimeout := 60; // default 1 hour if not suppplied
    a := GetEnumNameValueTrimmed(TypeInfo(TRestAuthenticationSignedUriAlgo),
      values[9].Text, values[9].Len);
    if a >= 0 then
      Sender.fComputeSignature :=
        TRestClientAuthenticationSignedUri.GetComputeSignature(algo);
  end;
end;

class function TRestClientAuthentication.ClientSetUser(
  Sender: TRestClientUri; const aUserName, aPassword: RawUtf8;
  aPasswordKind: TRestClientSetUserPassword; const aHashSalt: RawUtf8;
  aHashRound: integer): boolean;
var
  U: TAuthUser;
  key: RawUtf8;
begin
  result := false;
  if Sender = nil then
    exit;
  try
    Sender.SessionClose;  // ensure Sender.SessionUser=nil
    U := TAuthUser(Sender.fModel.GetTableInherited(TAuthUser).Create);
    try
      U.LogonName := TrimU(aUserName);
      U.DisplayName := U.LogonName;
      if aPasswordKind <> passClear then
        U.PasswordHashHexa := aPassword
      else if aHashSalt = '' then
        U.PasswordPlain := aPassword
      else
        // compute Sha256() or proper Pbkdf2HmacSha256()
        U.SetPassword(aPassword, aHashSalt, aHashRound);
      key := ClientComputeSessionKey(Sender, U);
      result := Sender.SessionCreate(self, U, key);
    finally
      U.Free;
    end;
  finally
    if Assigned(Sender.OnSetUser) then
      // always notify of user change, even if failed
      Sender.OnSetUser(Sender);
  end;
end;


{ TRestClientAuthenticationDefault }

class function TRestClientAuthenticationDefault.ClientComputeSessionKey(
  Sender: TRestClientUri; User: TAuthUser): RawUtf8;
var
  aServerNonce, aClientNonce: RawUtf8;
  rnd: THash128;
begin
  result := '';
  if User.LogonName = '' then
    exit;
  aServerNonce := Sender.CallBackGetResult('auth', ['username', User.LogonName]);
  if aServerNonce = '' then
    exit;
  TAesPrng.Main.FillRandom(rnd); // 128-bit random
  aClientNonce := CardinalToHexLower(OSVersionInt32) + '_' +
                  BinToHexLower(@rnd, SizeOf(rnd)); // 160-bit nonce
  result := ClientGetSessionKey(Sender, User, [
    'username',   User.LogonName,
    'password',   Sha256(Sender.fModel.Root + aServerNonce + aClientNonce +
                         User.LogonName + User.PasswordHashHexa),
    'clientnonce', aClientNonce]);
end;


{ TRestClientAuthenticationSignedUri }

{ Some Numbers - Indicative only!
  - Client side REST sign with crc32: 794,759 assertions passed  730.86ms
  - Client side REST sign with crc32c: 794,753 assertions passed  718.26ms
  - Client side REST sign with xxhash: 794,753 assertions passed  717.63ms
  - Client side REST sign with md5: 794,753 assertions passed  741.87ms
  - Client side REST sign with sha256: 794,753 assertions passed  767.58ms
  - Client side REST sign with sha512: 794,753 assertions passed  800.34ms
}

class function TRestClientAuthenticationSignedUri.ComputeSignatureCrc32(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // historical algorithm, from zlib crc32 polynom
  result := crc32(crc32(privatesalt, timestamp, 8), url, urllen);
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureCrc32c(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // faster on SSE4.2 CPU, and slightly more secure if not cascaded
  result := crc32c(privatesalt, timestamp, 8) xor crc32c(privatesalt, url, urllen);
end;

class function TRestClientAuthenticationSignedUri.ComputeSignaturexxHash(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // xxHash32 has no immediate reverse function, but is really weak
  result := xxHash32(privatesalt, timestamp, 8) xor xxHash32(privatesalt, url, urllen);
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureMd5(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash128Rec;
  MD5: TMd5;
  i: PtrInt;
begin
  MD5.Init;
  MD5.Update(privatesalt, 4);
  MD5.Update(timestamp^, 8);
  MD5.Update(url^, urllen);
  MD5.Final(digest.b);
  result := digest.c[0];
  for i := 1 to high(digest.c) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest.c[i];
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureSha1(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: array[0..(SizeOf(TSha1Digest) div 4) - 1] of cardinal;
  SHA1: TSha1;
  i: PtrInt;
begin
  SHA1.Init;
  SHA1.Update(@privatesalt, 4);
  SHA1.Update(timestamp, 8);
  SHA1.Update(url, urllen);
  SHA1.Final(TSha1Digest(digest));
  result := digest[0];
  for i := 1 to high(digest) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest[i];
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureSha256(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash256Rec;
  SHA256: TSha256;
  i: PtrInt;
begin
  SHA256.Init;
  SHA256.Update(@privatesalt, 4);
  SHA256.Update(timestamp, 8);
  SHA256.Update(url, urllen);
  SHA256.Final(digest.b);
  result := digest.c[0];
  for i := 1 to high(digest.c) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest.c[i];
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureSha512(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash512Rec;
  SHA512: TSha512;
  i: PtrInt;
begin
  SHA512.Init;
  SHA512.Update(@privatesalt, 4);
  SHA512.Update(timestamp, 8);
  SHA512.Update(url, urllen);
  SHA512.Final(digest.b);
  result := digest.c[0];
  for i := 1 to high(digest.c) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest.c[i];
end;

class function TRestClientAuthenticationSignedUri.ComputeSignatureSha3(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash256Rec;
  Sha3: TSha3;
  i: PtrInt;
begin
  Sha3.Init(SHA3_256);
  Sha3.Update(@privatesalt, 4);
  Sha3.Update(timestamp, 8);
  Sha3.Update(url, urllen);
  Sha3.Final(@digest.b);
  result := digest.c[0];
  for i := 1 to high(digest.c) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest.c[i];
end;

class function TRestClientAuthenticationSignedUri.GetComputeSignature(
  algo: TRestAuthenticationSignedUriAlgo): TOnRestAuthenticationSignedUriComputeSignature;
begin
  // FPC doesn't allow to use constants for procedure of object
  case algo of
    suaCRC32C:
      result := ComputeSignatureCrc32c;
    suaXXHASH:
      result := ComputeSignaturexxHash;
    suaMD5:
      result := ComputeSignatureMd5;
    suaSHA1:
      result := ComputeSignatureSha1;
    suaSHA256:
      result := ComputeSignatureSha256;
    suaSHA3:
      result := ComputeSignatureSha3;
    suaSHA512:
      result := ComputeSignatureSha512;
  else
    result := ComputeSignatureCrc32; // default/legacy/fallback
  end;
end;

class procedure TRestClientAuthenticationSignedUri.ClientSessionSign(
  Sender: TRestClientUri; var Call: TRestUriParams);
var
  nonce, blankURI: RawUtf8;
  sign: cardinal;
begin
  if (Sender = nil) or
     (Sender.Session.ID = 0) or
     (Sender.Session.User = nil) then
    exit;
  blankURI := Call.Url;
  if PosExChar('?', Call.url) = 0 then
    Call.url := Call.Url + '?session_signature='
  else
    Call.url := Call.Url + '&session_signature=';
  with Sender do
  begin
    fSession.LastTick64 := GetTickCount64;
    nonce := CardinalToHexLower(fSession.LastTick64 shr 8); // 256 ms resolution
    sign := Sender.fComputeSignature(fSession.PrivateKey, Pointer(nonce),
      Pointer(blankURI), length(blankURI));
    Call.url := Call.url + fSession.IDHexa8 + nonce + CardinalToHexLower(sign);
  end;
end;


{ TRestClientAuthenticationUri }

class procedure TRestClientAuthenticationUri.ClientSessionSign(
  Sender: TRestClientUri; var Call: TRestUriParams);
begin
  if (Sender <> nil) and
     (Sender.Session.ID <> 0) and
     (Sender.Session.User <> nil) then
    if PosExChar('?', Call.url) = 0 then
      Call.url := Call.url + '?session_signature=' + Sender.Session.IDHexa8
    else
      Call.url := Call.url + '&session_signature=' + Sender.Session.IDHexa8;
end;


{ TRestClientAuthenticationNone }

class function TRestClientAuthenticationNone.ClientComputeSessionKey(
  Sender: TRestClientUri; User: TAuthUser): RawUtf8;
begin
  result := ClientGetSessionKey(Sender, User, [
    'userName', User.LogonName]);
end;


{ TRestClientAuthenticationHttpAbstract }

class procedure TRestClientAuthenticationHttpAbstract.ClientSessionSign(
  Sender: TRestClientUri; var Call: TRestUriParams);
begin
  if (Sender <> nil) and
     (Sender.Session.ID <> 0) and
     (Sender.Session.User <> nil) then
    Call.InHead := TrimU(Call.InHead + // session ID transmitted as HTTP cookie
      (#13#10'Cookie: ' + REST_COOKIE_SESSION + '=') + Sender.Session.IDHexa8);
end;

class function TRestClientAuthenticationHttpAbstract.ClientSetUser(
  Sender: TRestClientUri; const aUserName, aPassword: RawUtf8;
  aPasswordKind: TRestClientSetUserPassword;
  const aHashSalt: RawUtf8; aHashRound: integer): boolean;
var
  res: RawUtf8;
  U: TAuthUser;
begin
  result := false;
  if (aUserName = '') or
     (Sender = nil) then
    exit;
  if aPasswordKind <> passClear then
    raise ERestException.CreateUtf8(
      '%.ClientSetUser(%) expects passClear', [self, Sender]);
  Sender.SessionClose; // ensure Sender.SessionUser=nil
  try
    // inherited ClientSetUser() won't fit with server's Auth() method
    ClientSetUserHttpOnly(Sender, aUserName, aPassword);
    Sender.fSession.Authentication := self; // to enable ClientSessionSign()
    U := TAuthUser(Sender.fModel.GetTableInherited(TAuthUser).Create);
    try
      U.LogonName := TrimU(aUserName);
      res := ClientGetSessionKey(Sender, U, []);
      if res <> '' then
        result := Sender.SessionCreate(self, U, res);
    finally
      U.Free;
    end;
  finally
    if not result then
    begin
      // on error, reverse all values
      Sender.fSession.Authentication := nil;
      Sender.fSession.HttpHeader := '';
    end;
    if Assigned(Sender.OnSetUser) then
      Sender.OnSetUser(Sender); // always notify of user change, even if failed
  end;
end;

class procedure TRestClientAuthenticationHttpAbstract.ClientSetUserHttpOnly(
  Sender: TRestClientUri; const aUserName, aPasswordClear: RawUtf8);
begin
  Sender.fSession.HttpHeader := ComputeAuthenticateHeader(aUserName, aPasswordClear);
end;


{ TRestClientAuthenticationHttpBasic }

class function TRestClientAuthenticationHttpBasic.ComputeAuthenticateHeader(
  const aUserName, aPasswordClear: RawUtf8): RawUtf8;
begin
  result := 'Authorization: Basic ' +
    BinToBase64(aUserName + ':' + aPasswordClear);
end;


{$ifdef DOMAINRESTAUTH}
{ will use mormot.lib.sspi/gssapi units depending on the OS }

class function TRestClientAuthenticationSspi.ClientComputeSessionKey(
  Sender: TRestClientUri; User: TAuthUser): RawUtf8;
var
  SecCtx: TSecContext;
  WithPassword: boolean;
  OutData: RawByteString;
begin
  InitializeDomainAuth; // setup mormot.lib.sspi/gssapi unit depending on the OS
  result := '';
  InvalidateSecContext(SecCtx, 0);
  WithPassword := User.LogonName <> '';
  Sender.fSession.Data := '';
  try
    repeat
      if WithPassword then
        ClientSspiAuthWithPassword(SecCtx,
          Sender.fSession.Data, User.LogonName, User.PasswordHashHexa, OutData)
      else
        ClientSspiAuth(SecCtx,
          Sender.fSession.Data, User.PasswordHashHexa, OutData);
      if OutData = '' then
        break;
      if result <> '' then
        break; // 2nd pass
      // 1st call will return data, 2nd call SessionKey
      result := ClientGetSessionKey(Sender, User,
        ['username', '',
         'data', BinToBase64(OutData)]);
    until Sender.fSession.Data = '';
    if result <> '' then
    begin
      // TRestServerAuthenticationSspi.Auth encrypted session.fPrivateSalt
      OutData := Base64ToBin(result); // need a local copy on Windows / SSPI
      result := SecDecrypt(SecCtx, OutData);
    end;
  finally
    FreeSecContext(SecCtx);
  end;
  // authenticated by Windows on the server side: use the returned
  // SessionKey + PasswordHashHexa to sign the URI, as usual
  User.PasswordHashHexa := ''; // should not appear on URI signature
end;

{$endif DOMAINRESTAUTH}


{ ************ TRestClientRoutingRest/TRestClientRoutingJsonRpc Routing Schemes }

{ TRestClientRoutingRest }

class procedure TRestClientRoutingRest.ClientSideInvoke(var uri: RawUtf8;
  ctxt: TRestClientSideInvoke; const method, params, clientDrivenID: RawUtf8;
  out sent, head: RawUtf8);
begin
  if clientDrivenID <> '' then
    uri := uri + '.' + method + '/' + clientDrivenID
  else
    uri := uri + '.' + method;
  if (csiAsOctetStream in ctxt) and
     (length(params) > 2) and
     (params[1] = '"') then
  begin
    sent := Base64ToBin(@params[2], length(params) - 2);
    if sent <> '' then
    begin
      head := BINARY_CONTENT_TYPE_HEADER;
      exit;
    end;
  end;
  sent := '[' + params + ']'; // we may also encode them within the URI
end;


{ TRestClientRoutingJsonRpc }

class procedure TRestClientRoutingJsonRpc.ClientSideInvoke(var uri: RawUtf8;
  ctxt: TRestClientSideInvoke; const method, params, clientDrivenID: RawUtf8;
  out sent, head: RawUtf8);
begin
  sent := '{"method":"' + method + '","params":[' + params;
  if clientDrivenID = '' then
    sent := sent + ']}'
  else
    sent := sent + '],"id":' + clientDrivenID + '}';
end;



{ ************ TRestClientUri Base Class for Actual Clients }

{ TRestClientCallbacks }

constructor TRestClientCallbacks.Create(aOwner: TRestClientUri);
begin
  inherited Create;
  Owner := aOwner;
end;

function TRestClientCallbacks.FindIndex(aID: TRestClientCallbackID): PtrInt;
var
  P: PRestClientCallbackItem;
begin
  if self <> nil then
  begin
    P := pointer(List);
    for result := 0 to Count - 1 do
      if P^.ID = aID then
        exit
      else
        inc(P);
  end;
  result := -1;
end;

function TRestClientCallbacks.FindEntry(
  var aItem: TRestClientCallbackItem): boolean;
var
  i: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    i := FindIndex(aItem.ID);
    if i < 0 then
      exit;
    result := true;
    aItem := List[i];
  finally
    fSafe.UnLock;
  end;
end;

function TRestClientCallbacks.FindAndRelease(aID: TRestClientCallbackID): boolean;
var
  i: PtrInt;
begin
  result := false;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    i := FindIndex(aID);
    if i < 0 then
      exit;
    List[i].ReleasedFromServer := True; // just flag it -> delay deletion
  finally
    fSafe.UnLock;
  end;
  result := true;
end;

function TRestClientCallbacks.UnRegisterByIndex(index: integer): boolean;
begin
  result := false;
  if cardinal(index) >= cardinal(Count) then
    exit;
  with List[index] do
    if not ReleasedFromServer then
    try
      if Owner.FakeCallbackUnregister(Factory, ID, Instance) then
        result := true;
    except
      // ignore errors at this point, and continue
    end;
  dec(Count);
  if index < Count then
    MoveFast(List[index + 1], List[index], (Count - index) * SizeOf(List[index]));
end;

function TRestClientCallbacks.UnRegister(aInstance: pointer): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (self = nil) or
     (Count = 0) then
    exit;
  fSafe.Lock;
  try
    for i := Count - 1 downto 0 do
      if List[i].Instance = aInstance then
        if UnRegisterByIndex(i) then
          result := true
        else
          break;
  finally
    fSafe.UnLock;
  end;
end;

procedure TRestClientCallbacks.DoRegister(aID: TRestClientCallbackID;
  aInstance: pointer; aFactory: TInterfaceFactory);
begin
  if aID <= 0 then
    exit;
  fSafe.Lock;
  try
    if length(List) >= Count then
      SetLength(List, Count + 32);
    with List[Count] do
    begin
      ID := aID;
      Instance := aInstance;
      Factory := aFactory;
    end;
    inc(Count);
  finally
    fSafe.UnLock;
  end;
end;

function TRestClientCallbacks.DoRegister(aInstance: pointer;
  aFactory: TInterfaceFactory): TRestClientCallbackID;
begin
  result := InterlockedIncrement(fCurrentID);
  DoRegister(result, aInstance, aFactory);
end;


{ TRemoteLogThread }

type
  TRemoteLogThread = class(TRestThread)
  protected
    fClient: TRestClientUri;
    fPendingRows: RawUtf8;
    procedure InternalExecute; override;
  public
    constructor Create(aClient: TRestClientUri); reintroduce;
    destructor Destroy; override;
    procedure AddRow(const aText: RawUtf8);
  end;

constructor TRemoteLogThread.Create(aClient: TRestClientUri);
begin
  fClient := aClient;
  inherited Create(aClient, false, false);
end;

destructor TRemoteLogThread.Destroy;
var
  i: integer;
begin
  if fPendingRows <> '' then
  begin
    fEvent.SetEvent;
    for i := 1 to 200 do
    begin
      SleepHiRes(10);
      if fPendingRows = '' then
        break;
    end;
  end;
  inherited Destroy;
end;

procedure TRemoteLogThread.AddRow(const aText: RawUtf8);
begin
  fSafe.Lock;
  try
    AddToCsv(aText, fPendingRows, #13#10);
  finally
    fSafe.UnLock;
  end;
  fEvent.SetEvent;
end;

procedure TRemoteLogThread.InternalExecute;
var
  aText: RawUtf8;
begin
  while not Terminated do
  begin
    fEvent.WaitForEver;
    if Terminated then
      break;
    fSafe.Lock;
    try
      aText := fPendingRows;
      fPendingRows := '';
    finally
      fSafe.UnLock;
    end;
    if (aText <> '') and
       not Terminated then
    try
      while not fClient.InternalRemoteLogSend(aText) do
        if SleepOrTerminated(2000) then // retry after 2 seconds delay
          exit;
    except
      on E: Exception do
        if (fClient <> nil) and
           not Terminated then
          fClient.InternalLog('%.Execute fatal error: %' +
            'some events were not transmitted', [ClassType, E], sllWarning);
    end;
  end;
end;


{ TRestClientUri }

procedure TRestClientUri.SetRoutingClass(aServicesRouting: TRestClientRoutingClass);
begin
  if self <> nil then
    if aServicesRouting <> fServicesRouting then
      if (aServicesRouting = nil) or
         (aServicesRouting = TRestClientRouting) then
         raise EServiceException.CreateUtf8('Unexpected %.SetRoutingClass(%)',
           [self, aServicesRouting])
      else
         fServicesRouting := aServicesRouting;
end;

procedure TRestClientUri.SetSessionHeartbeatSeconds(timeout: integer);
begin
  if (timeout < 0) or
     (timeout = fSession.HeartbeatSeconds) then
    exit;
  fSession.HeartbeatSeconds := timeout;
  fRun.TimerEnable(SessionRenewEvent, timeout);
end;

function TRestClientUri.GetOnIdleBackgroundThreadActive: boolean;
begin
  result := (self <> nil) and
            Assigned(fOnIdle) and
            fBackgroundThread.OnIdleBackgroundThreadActive;
end;

procedure TRestClientUri.OnBackgroundProcess(
  Sender: TSynBackgroundThreadEvent; ProcessOpaqueParam: pointer);
var
  Call: ^TRestUriParams absolute ProcessOpaqueParam;
begin
  if Call = nil then
    exit;
  InternalUri(Call^);
  if ((Sender = nil) or OnIdleBackgroundThreadActive) and
     not (isDestroying in fInternalState) then
  begin
    if (Call^.OutStatus = HTTP_NOTIMPLEMENTED) and
       not (isNotImplemented in fInternalState) then
    begin
      InternalClose; // force recreate connection
      Include(fInternalState, isNotImplemented);
      if (Sender = nil) or
         OnIdleBackgroundThreadActive then
        InternalUri(Call^); // try request again
    end;
    if Call^.OutStatus <> HTTP_NOTIMPLEMENTED then
      Exclude(fInternalState, isNotImplemented);
  end;
end;

procedure TRestClientUri.SetLastException(E: Exception; ErrorCode: integer;
  Call: PRestUriParams);
begin
  fLastErrorCode := ErrorCode;
  if E = nil then
  begin
    fLastErrorException := nil;
    if StatusCodeIsSuccess(ErrorCode) then
      fLastErrorMessage := ''
    else
      StatusCodeToReason(ErrorCode, fLastErrorMessage);
  end
  else
  begin
    fLastErrorException := PPointer(E)^;
    fLastErrorMessage := ObjectToJsonDebug(E);
  end;
  if Assigned(fOnFailed) then
    fOnFailed(self, E, Call);
end;

function TRestClientUri.InternalRemoteLogSend(const aText: RawUtf8): boolean;
begin
  result := Uri(fModel.GetUriCallBack('RemoteLog', nil, 0),
    'PUT', nil, nil, @aText) in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestClientUri.{%H-}FakeCallbackRegister(Sender: TServiceFactory;
  const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
  ParamValue: Pointer): TRestClientCallbackID;
begin
  raise EServiceException.CreateUtf8('% does not support interface parameters ' +
    'for %.%(%: %): consider using another kind of client',
    [self, Sender.InterfaceFactory.InterfaceName, Method.Uri,
     ParamInfo.ParamName^, ParamInfo.ArgTypeName^]);
end;

function TRestClientUri.{%H-}FakeCallbackUnregister(Factory: TInterfaceFactory;
  FakeCallbackID: TRestClientCallbackID; Instance: pointer): boolean;
begin
  raise EServiceException.CreateUtf8(
    '% does not support % callbacks: consider using another kind of client',
    [self, Factory.InterfaceName]);
end;

const
  SSPI_DEFINITION_USERNAME = '***SSPI***';

constructor TRestClientUri.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
begin
  if fModel = nil then // if not already created with a reintroduced constructor
    Create(aModel);
  if fModel <> nil then
    fOnIdle := fModel.OnClientIdle; // allow UI interactivity during SetUser()
  if aDefinition.User <> '' then
  begin
    {$ifdef DOMAINRESTAUTH}
    if aDefinition.User = SSPI_DEFINITION_USERNAME then
      SetUser('', aDefinition.PasswordPlain)
    else
    {$endif DOMAINRESTAUTH}
      SetUser(aDefinition.User, aDefinition.PasswordPlain, true);
  end;
end;

procedure TRestClientUri.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition = nil then
    exit;
  inherited DefinitionTo(Definition); // save Kind
  if (fSession.Authentication <> nil) and
     (fSession.User <> nil) then
  begin
    {$ifdef DOMAINRESTAUTH}
    if fSession.Authentication.InheritsFrom(TRestClientAuthenticationSspi) then
      Definition.User := SSPI_DEFINITION_USERNAME
    else
    {$endif DOMAINRESTAUTH}
       Definition.User := fSession.User.LogonName;
     Definition.PasswordPlain := fSession.User.PasswordHashHexa;
  end;
end;

function TRestClientUri.GetCurrentSessionUserID: TID;
begin
  if fSession.User = nil then
    result := 0
  else
    result := fSession.User.IDValue;
end;

function TRestClientUri.GetSessionVersion: RawUtf8;
var
  resp: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fSession.Version = '' then
      // no session (e.g. API public URI) -> ask
      if CallBackGet('timestamp/info', [], resp) = HTTP_SUCCESS then
        fSession.Version := JsonDecode(resp, 'version');
    result := fSession.Version;
  end;
end;

function TRestClientUri.SessionCreate(aAuth: TRestClientAuthenticationClass;
  var aUser: TAuthUser; const aSessionKey: RawUtf8): boolean;
var
  period: integer;
begin
  result := false;
  fSession.ID := GetCardinal(pointer(aSessionKey));
  if fSession.ID = 0 then
    exit;
  fSession.IDHexa8 := CardinalToHexLower(fSession.ID);
  fSession.PrivateKey := crc32(crc32(0,
    pointer(aSessionKey), length(aSessionKey)),
    pointer(aUser.PasswordHashHexa), length(aUser.PasswordHashHexa));
  fSession.User := aUser;
  fSession.Authentication := aAuth;
  aUser := nil; // now owned by this instance
  if fSession.ServerTimeout > 0 then
  begin
    // call _ping_ every half timeout period
    period := fSession.ServerTimeout * (60 div 2);
    if period > 25 * 60 then
      // default REST heartbeat at least every 25 minutes
      period := 25 * 60;
    SetSessionHeartbeatSeconds(period);
  end;
  result := true;
end;

procedure TRestClientUri.SessionRenewEvent(Sender: TSynBackgroundTimer;
  const Msg: RawUtf8);
var
  resp: RawUtf8;
  status: integer;
begin
  status := CallBack(mPOST, 'CacheFlush/_ping_', '', resp);
  InternalLog('SessionRenewEvent(%) received status=% count=% from % % (timeout=% min)',
    [fModel.Root, status, JsonDecode(resp, 'count'), fSession.Server,
     fSession.Version, fSession.ServerTimeout], sllUserAuth);
end;

function TRestClientUri.IsOpen: boolean;
var
  started, elapsed, max: Int64;
  wait, retry: integer;
  exc: ExceptionClass;
begin
  result := InternalIsOpen;
  if result or
     (isDestroying in fInternalState) then
    exit;
  fSafe.Enter;
  try
    result := InternalIsOpen; // check again within lock
    if result or
       (isDestroying in fInternalState) then
      exit;
    retry := 0;
    max := fConnectRetrySeconds * 1000;
    if max = 0 then
      started := 0
    else
      started := GetTickCount64;
    repeat
      exc := nil;
      try
        InternalOpen;
      except
        on E: Exception do
        begin
          InternalClose;
          if (started = 0) or
             (isDestroying in fInternalState) then
            exit;
          if Assigned(fOnConnectionFailed) then
            try
              fOnConnectionFailed(self, E, nil);
            except
            end;
          exc := PPointer(E)^;
        end;
      end;
      if InternalIsOpen then
        break;
      elapsed := GetTickCount64 - started;
      if elapsed >= max then
        exit;
      inc(retry);
      if elapsed < 500 then // retry in pace of 100-200ms, 1-2s, 5-10s
        wait := 100
      else if elapsed < 10000 then
        wait := 1000
      else
        wait := 5000;
      inc(wait, Random32(wait)); // randomness to reduce server load
      if elapsed + wait > max then
      begin
        wait := max - elapsed;
        if wait <= 0 then
          exit;
      end;
      fLogClass.Add.Log(sllTrace, 'IsOpen: % after % -> wait % and ' +
        'retry #% up to % seconds - %',
        [exc, MilliSecToString(elapsed), MilliSecToString(wait), retry,
         fConnectRetrySeconds, self], self);
      SleepHiRes(wait);
      if isDestroying in fInternalState then
        exit;
    until InternalIsOpen;
  finally
    fSafe.Leave;
  end;
  if Assigned(fOnConnected) then
    try
      with fLogClass.Enter(self, 'IsOpen: call OnConnected') do
        fOnConnected(self);
    except
    end;
  result := true;
end;

function TRestClientUri.OrmInstance: TRestOrm;
begin
  result := TRestOrm(fOrmInstance);
end;

constructor TRestClientUri.Create(aModel: TOrmModel);
begin
  inherited Create(aModel);
  fMaximumAuthentificationRetry := 1;
  fComputeSignature := TRestClientAuthenticationSignedUri.ComputeSignatureCrc32;
  fSession.ID := CONST_AUTHENTICATION_NOT_USED;
  fFakeCallbacks := TRestClientCallbacks.Create(self);
  fSafe := TAutoLocker.Create;
  fServicesRouting := TRestClientRoutingRest;
  TRestOrmClientUri.Create(self); // asssign the URI-based ORM kernel
end;

destructor TRestClientUri.Destroy;
var
  t, i: PtrInt;
  aID: TID;
  Table: TOrmClass;
begin
  include(fInternalState, isDestroying);
  if SynLogFileFreeing then // may be owned by a TSynLogFamily
    SetLogClass(nil);
  {$ifdef OSWINDOWS}
  fServiceNotificationMethodViaMessages.Wnd := 0; // disable notification
  {$endif OSWINDOWS}
  FreeAndNilSafe(fFakeCallbacks);
  try
    // unlock all still locked records by this client
    if fModel <> nil then
      for t := 0 to high(fModel.Locks) do
      begin
        Table := fModel.Tables[t];
        with fModel.Locks[t] do
          for i := 0 to Count - 1 do
          begin
            aID := IDs[i];
            if aID <> 0 then // 0 is empty after unlock
              fOrm.UnLock(Table, aID);
          end;
      end;
    SessionClose; // if not already notified
  finally
    fClient := nil; // for proper RefCnt in inherited Destroy
    // release memory and associated classes
    if fRemoteLogClass <> nil then
    begin
      FreeAndNilSafe(fRemoteLogThread);
      ServerRemoteLogStop;
    end;
    FreeAndNilSafe(fSession.User);
    try
      inherited Destroy; // fModel.Free if owned by this TRest instance
      FreeAndNilSafe(fBackgroundThread); // should be done after fServices.Free
      fOnIdle := nil;
    finally
      InternalClose;
    end;
  end;
end;

procedure TRestClientUri.SetOrmInstance(aORM: TRestOrmParent);
begin
  inherited SetOrmInstance(aORM); // set fOrm
  if not fOrmInstance.GetInterface(IRestOrmClient, fClient) then
    raise ERestException.CreateUtf8('%.Create with invalid %', [self, fOrmInstance]);
  // enable redirection of Uri() from IRestOrm/IRestOrmClient into this class
  (fOrmInstance as TRestOrmClientUri).Uri := URI;
end;

procedure TRestClientUri.SessionClose;
var
  tmp: RawUtf8;
begin
  if (self <> nil) and
     (fSession.User <> nil) and
     (fSession.ID <> CONST_AUTHENTICATION_SESSION_NOT_STARTED) then
  try
    fRun.TimerDisable(SessionRenewEvent);
    InternalLog('SessionClose: notify server', sllTrace);
    CallBackGet('auth', ['userName', fSession.User.LogonName,
                         'session',  fSession.ID], tmp);
  finally
    // back to no session, with default values
    fSession.ID := CONST_AUTHENTICATION_SESSION_NOT_STARTED;
    fSession.IDHexa8 := '';
    fSession.PrivateKey := 0;
    fSession.Authentication := nil;
    fSession.Server := '';
    fSession.Version := '';
    FillZero(fSession.Data);
    fSession.Data := '';
    fSession.ServerTimeout := 0;
    FreeAndNilSafe(fSession.User);
    fComputeSignature := TRestClientAuthenticationSignedUri.ComputeSignatureCrc32;
  end;
end;

{$ifdef OSWINDOWS}

type
  TRestClientUriServiceNotification = class(TInterfaceMethodExecute)
  protected
    /// parameters set by TRestClientUri.InternalNotificationMethodExecute
    fOwner: TRestClientUri;
    fInstance: pointer; // weak IInvokable reference
    fPar: RawUtf8;
  end;

procedure TRestClientUri.ServiceNotificationMethodViaMessages(
  hWnd: HWND; Msg: cardinal);
begin
  if Msg = 0 then
    hWnd := 0; // avoid half defined parameters
  fServiceNotificationMethodViaMessages.Wnd := hWnd;
  fServiceNotificationMethodViaMessages.Msg := Msg;
end;

class procedure TRestClientUri.ServiceNotificationMethodExecute(
  var Msg: TMessage);
var
  exec: TRestClientUriServiceNotification;
begin
  exec := pointer(Msg.LParam);
  if exec <> nil then
  try
    try
      if exec.InheritsFrom(TRestClientUriServiceNotification) and
         (HWND(Msg.WParam) = exec.fOwner.fServiceNotificationMethodViaMessages.Wnd) then
        // run asynchronous notification callback in the main UI thread context
        exec.ExecuteJson([exec.fInstance], pointer(exec.fPar), nil);
    finally
      exec.Free; // always release notification resources
    end;
  except
    ; // ignore any exception, e.g. in case of invalid/fuzzed Msg
  end;
end;

{$endif OSWINDOWS}

procedure TRestClientUri.InternalNotificationMethodExecute(
  var Ctxt: TRestUriParams);
var
  url, interfmethod, interf, id, method, frames: RawUtf8;
  callback: TRestClientCallbackItem;
  methodIndex: PtrInt;
  WR: TJsonWriter;
  temp: TTextWriterStackBuffer;
  ok: boolean;

  procedure Call(methodIndex: integer; const par: RawUtf8; res: TJsonWriter);
  var
    method: PInterfaceMethod;
    exec: TInterfaceMethodExecute;
    {$ifdef OSWINDOWS}
    execmsg: TRestClientUriServiceNotification absolute exec;
    {$endif OSWINDOWS}
  begin
    method := @callback.Factory.Methods[methodIndex];
    {$ifdef OSWINDOWS}
    if (fServiceNotificationMethodViaMessages.Wnd <> 0) and
       (method^.ArgsOutputValuesCount = 0) then
    begin
      // expects no result -> asynchronous non blocking notification in UI thread
      Ctxt.OutStatus := 0;
      execmsg := TRestClientUriServiceNotification.Create(
        callback.Factory, method, []);
      execmsg.fOwner := self;
      execmsg.fInstance := callback.Instance;
      execmsg.fPar := par;
      with fServiceNotificationMethodViaMessages do
        ok := PostMessage(Wnd, Msg, Wnd, PtrInt(execmsg));
      if ok then
        exit;
    end
    else
    // if PostMessage() failed, or expecting result -> blocking execution
    {$endif OSWINDOWS}
      exec := TInterfaceMethodExecute.Create(callback.Factory, method, []);
    try
      ok := exec.ExecuteJson([callback.Instance], pointer(par), res);
      Ctxt.OutHead := exec.ServiceCustomAnswerHead;
      Ctxt.OutStatus := exec.ServiceCustomAnswerStatus;
    finally
      exec.Free;
    end;
  end;

begin
  Ctxt.OutStatus := HTTP_BADREQUEST;
  // parse and validate the URI into its actual TRestClientCallbackItem
  url := Ctxt.Url;
  if (url = '') or
     (isDestroying in fInternalState) then
    exit;
  if url[1] = '/' then
    system.delete(url, 1, 1);
  // 'root/BidirCallback.AsyncEvent/1' into root/interfmethod/id
  if (fModel.UriMatch(url, false) = rmNoMatch) or
     (url[fModel.RootLen + 1] <> '/') or
     not Split(copy(url, fModel.RootLen + 2, 1024), '/', interfmethod, id) then
    exit;
  callback.ID := GetInteger(pointer(id));
  if callback.ID <= 0 then
    exit;
  if interfmethod = SERVICE_PSEUDO_METHOD[imFree] then
  begin
    // 'root/_free_/1'
    if fFakeCallbacks.FindAndRelease(callback.ID) then
      Ctxt.OutStatus := HTTP_SUCCESS;
    exit;
  end;
  if not fFakeCallbacks.FindEntry(callback) then
    exit;
  if (Ctxt.InHead <> '') and
     (callback.Factory.MethodIndexCurrentFrameCallback >= 0) then
    // to properly call CurrentFrame(isLast: boolean) hooking method
    FindNameValue(Ctxt.InHead, 'SEC-WEBSOCKET-FRAME: ', frames);
  Split(interfmethod, '.', interf, method);
  methodIndex := callback.Factory.FindMethodIndex(method);
  if (methodIndex >= 0) and
     IdemPropNameU(interfmethod,
      callback.Factory.Methods[methodIndex].InterfaceDotMethodName) then
  try
    // execute the method using JSON as data representation
    WR := TJsonWriter.CreateOwnedStream(temp);
    try
      WR.AddShort('{"result":[');
      if frames = '[0]' then
        // call CurrentFrame(isLast=false) before the first method of the jumbo frame
        Call(callback.Factory.MethodIndexCurrentFrameCallback, frames, nil);
      // call the method with input JSON body into output JSON body
      Call(methodIndex, Ctxt.InBody, WR);
      if ok then
      begin
        if Ctxt.OutHead = '' then
        begin
          // <>'' if set via TServiceCustomAnswer
          WR.Add(']', '}');
          Ctxt.OutStatus := HTTP_SUCCESS;
        end;
        Ctxt.OutBody := WR.Text;
      end
      else
        Ctxt.OutStatus := HTTP_SERVERERROR;
      if frames = '[1]' then
        // call CurrentFrame(isLast=true) after the last method of the jumbo frame
        Call(callback.Factory.MethodIndexCurrentFrameCallback, frames, nil);
    finally
      WR.Free;
    end;
  except
    on E: Exception do
    begin
      Ctxt.OutHead := '';
      Ctxt.OutBody := ObjectToJsonDebug(E);
      Ctxt.OutStatus := HTTP_SERVERERROR;
    end;
  end;
end;

function TRestClientUri.Uri(const url, method: RawUtf8; Resp: PRawUtf8;
  Head: PRawUtf8; SendData: PRawUtf8; OutInternalState: PCardinal): integer;
var
  retry: integer;
  aUserName, aPassword: string;
  StatusMsg: RawUtf8;
  Call: TRestUriParams;
  aPasswordHashed: boolean;

  procedure CallInternalUri;
  begin
    Call.Url := url; // reset to allow proper re-sign
    if fSession.Authentication <> nil then
      fSession.Authentication.ClientSessionSign(self, Call);
    Call.Method := method;
    if SendData <> nil then
      Call.InBody := SendData^;
    if Assigned(fOnIdle) then
    begin
      if fBackgroundThread = nil then
        fBackgroundThread := TSynBackgroundThreadEvent.Create(OnBackgroundProcess,
          OnIdle, FormatUtf8('% % background', [self, fModel.Root]));
      if not fBackgroundThread.RunAndWait(@Call) then
        Call.OutStatus := HTTP_UNAVAILABLE;
    end
    else
      OnBackgroundProcess({SenderThread=}nil, @Call);
    result := Call.OutStatus;
    if OutInternalState <> nil then
      OutInternalState ^ := Call.OutInternalState;
    if Head <> nil then
      Head^ := Call.OutHead;
    if Resp <> nil then
      Resp^ := Call.OutBody;
    fLastErrorCode := Call.OutStatus;
  end;

begin
  if self = nil then
  begin
    result := HTTP_UNAVAILABLE;
    SetLastException(nil, HTTP_UNAVAILABLE);
    exit;
  end;
  fLastErrorMessage := '';
  fLastErrorException := nil;
  if fServerTimestamp.Offset = 0 then
  begin
    if not ServerTimestampSynchronize then
    begin
      result := HTTP_UNAVAILABLE;
      exit; // if Timestamp is not available, server is down!
    end;
  end;
  Call.Init;
  if (Head <> nil) and
     (Head^ <> '') then
    Call.InHead := Head^;
  if fSession.HttpHeader <> '' then
    Call.InHead := TrimU(Call.InHead + #13#10 + fSession.HttpHeader);
  try
    CallInternalUri;
    if (Call.OutStatus = HTTP_TIMEOUT) and
       RetryOnceOnTimeout then
    begin
      InternalLog('% % returned "408 Request Timeout" -> RETRY', [method, url], sllError);
      CallInternalUri;
    end
    else if (Call.OutStatus = HTTP_FORBIDDEN) and
            (MaximumAuthentificationRetry > 0) and
            Assigned(OnAuthentificationFailed) and
            not (isInAuth in fInternalState) then
    try
      Include(fInternalState, isInAuth);
      retry := 1;
      while retry <= MaximumAuthentificationRetry do
      begin
        // "403 Forbidden" in case of authentication failure -> try relog
        if OnAuthentificationFailed(retry, aUserName, aPassword, aPasswordHashed) and
           SetUser(StringToUtf8(aUserName), StringToUtf8(aPassword), aPasswordHashed) then
        begin
          CallInternalUri;
          break;
        end;
        Inc(retry);
      end;
    finally
      Exclude(fInternalState, isInAuth);
    end;
    if not StatusCodeIsSuccess(Call.OutStatus) then
    begin
      StatusCodeToReason(Call.OutStatus, StatusMsg);
      if Call.OutBody = '' then
        fLastErrorMessage := StatusMsg
      else
        fLastErrorMessage := Call.OutBody;
      InternalLog('% % returned % (%) with message %',
        [method, url, Call.OutStatus, StatusMsg, fLastErrorMessage],
        LOG_TRACEERROR[Call.OutStatus <> HTTP_NOTFOUND]); // 404 is not fatal
      if Assigned(fOnFailed) then
        fOnFailed(self, nil, @Call);
    end;
  except
    on E: Exception do
    begin
      result := HTTP_NOTIMPLEMENTED; // 501
      SetLastException(E, HTTP_NOTIMPLEMENTED, @Call);
      exit;
    end;
  end;
end;

function TRestClientUri.CallBackGet(const aMethodName: RawUtf8;
  const aNameValueParameters: array of const; out aResponse: RawUtf8;
  aTable: TOrmClass; aID: TID; aResponseHead: PRawUtf8): integer;
var
  url, header: RawUtf8;
  log: ISynLog; // for Enter auto-leave to work with FPC / Delphi 10.4+
begin
  if self = nil then
    result := HTTP_UNAVAILABLE
  else
  begin
    url := fModel.GetUriCallBack(aMethodName, aTable, aID);
    if high(aNameValueParameters) > 0 then
      url := url + UrlEncode(aNameValueParameters);
    log := fLogClass.Enter('CallBackGet %', [url], self);
    result := Uri(url, 'GET', @aResponse, @header);
    if aResponseHead <> nil then
      aResponseHead^ := header;
    if (log <> nil) and
       (aResponse <> '') and
       (sllServiceReturn in fLogFamily.Level) then
      if IsHtmlContentTypeTextual(pointer(header)) then
        log.Log(sllServiceReturn, aResponse, self, MAX_SIZE_RESPONSE_LOG)
      else
        log.Log(sllServiceReturn, '% bytes [%]', [length(aResponse), header], self);
  end;
end;

function TRestClientUri.CallBackGetResult(const aMethodName: RawUtf8;
  const aNameValueParameters: array of const; aTable: TOrmClass;
  aID: TID): RawUtf8;
var
  resp: RawUtf8;
begin
  if CallBackGet(
      aMethodName, aNameValueParameters, resp, aTable, aID) = HTTP_SUCCESS then
    result := JsonDecode(resp)
  else
    result := '';
end;

function TRestClientUri.CallBackPut(const aMethodName, aSentData: RawUtf8;
  out aResponse: RawUtf8; aTable: TOrmClass; aID: TID;
  aResponseHead: PRawUtf8): integer;
begin
  result := Callback(mPUT, aMethodName, aSentData, aResponse, aTable, aID, aResponseHead);
end;

function TRestClientUri.CallBack(method: TUriMethod; const aMethodName,
  aSentData: RawUtf8; out aResponse: RawUtf8; aTable: TOrmClass;
  aID: TID; aResponseHead: PRawUtf8): integer;
var
  u, m: RawUtf8;
  {%H-}log: ISynLog;
begin
  if (self = nil) or
     (method = mNone) then
    result := HTTP_UNAVAILABLE
  else
  begin
    u := fModel.GetUriCallBack(aMethodName, aTable, aID);
    log := fLogClass.Enter('Callback %', [u], self);
    m := MethodText(method);
    result := Uri(u, m, @aResponse, aResponseHead, @aSentData);
    InternalLog('% result=% resplen=%',
      [m, result, length(aResponse)], sllServiceReturn);
  end;
end;

procedure TRestClientUri.CallbackNonBlockingSetHeader(out Header: RawUtf8);
begin
  // nothing to do by default (plain REST/HTTP works in blocking mode)
end;

function TRestClientUri.ServiceContainer: TServiceContainer;
begin
  if fServices = nil then
    fServices := TServiceContainerClient.Create(self);
  result := fServices;
end;

function TRestClientUri.ServiceRegister(const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): boolean;
begin
  result := False;
  if (self = nil) or
     (high(aInterfaces) < 0) then
    exit;
  result := (ServiceContainer as TServiceContainerClient).AddInterface(
    aInterfaces, aInstanceCreation, aContractExpected);
end;

function TRestClientUri.ServiceRegister(aInterface: PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8; aIgnoreAnyException: boolean): TServiceFactory;
begin
  result := nil;
  if (self = nil) or
     (aInterface = nil) then
  begin
    SetLastException;
    exit;
  end;
  with ServiceContainer as TServiceContainerClient do
    try
      result := AddInterface(aInterface, aInstanceCreation, aContractExpected);
    except
      on E: Exception do
        if aIgnoreAnyException then
          SetLastException(E)
        else
          raise;
    end;
end;

function TRestClientUri.ServiceRegisterClientDriven(aInterface: PRttiInfo;
  out Obj; const aContractExpected: RawUtf8): boolean;
var
  Factory: TServiceFactory;
begin
  Factory := ServiceRegister(aInterface, sicClientDriven, aContractExpected);
  if Factory <> nil then
  begin
    result := true;
    Factory.Get(Obj);
  end
  else
    result := false;
end;

function TRestClientUri.ServiceDefine(const aInterfaces: array of TGuid;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8): boolean;
begin
  if self <> nil then
    result := ServiceRegister(TInterfaceFactory.Guid2TypeInfo(aInterfaces),
      aInstanceCreation, aContractExpected)
  else
    result := false;
end;

function TRestClientUri.ServiceDefine(const aInterface: TGuid;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUtf8; aIgnoreAnyException: boolean): TServiceFactoryClient;
begin
  result := TServiceFactoryClient(
    ServiceRegister(TInterfaceFactory.Guid2TypeInfo(aInterface),
     aInstanceCreation, aContractExpected, aIgnoreAnyException));
end;

function TRestClientUri.ServiceDefineClientDriven(const aInterface: TGuid;
  out Obj; const aContractExpected: RawUtf8): boolean;
begin
  result := ServiceRegisterClientDriven(
    TInterfaceFactory.Guid2TypeInfo(aInterface), Obj, aContractExpected);
end;

function TRestClientUri.ServiceDefineSharedApi(const aInterface: TGuid;
  const aContractExpected: RawUtf8; aIgnoreAnyException: boolean): TServiceFactoryClient;
begin
  try
    result := ServiceDefine(
      aInterface, sicShared, aContractExpected, aIgnoreAnyException);
    if result <> nil then
    begin
      result.ParamsAsJsonObject := true; // no contract -> explicit parameters
      result.ResultAsJsonObjectWithoutResult := true;
    end;
  except
    if aIgnoreAnyException then
      result := nil
    else
      raise;
  end;
end;

function TRestClientUri.ServiceRetrieveAssociated(const aServiceName: RawUtf8;
  out URI: TRestServerURIDynArray): boolean;
var
  json: RawUtf8;
begin
  result := (CallBackGet('stat', ['findservice', aServiceName], json) = HTTP_SUCCESS) and
    (DynArrayLoadJson(URI, pointer({%H-}json), TypeInfo(TRestServerUriDynArray)) <> nil);
end;

function TRestClientUri.ServiceRetrieveAssociated(const aInterface: TGuid;
  out URI: TRestServerUriDynArray): boolean;
var
  fact: TInterfaceFactory;
begin
  fact := TInterfaceFactory.Get(aInterface);
  if fact = nil then
    result := false
  else
    result := ServiceRetrieveAssociated(copy(fact.InterfaceName, 2, maxInt), URI);
end;

function TRestClientUri.ServerTimestampSynchronize: boolean;
var
  status: integer;
  resp: RawUtf8;
begin
  if self = nil then
  begin
    result := false;
    exit;
  end;
  fServerTimestamp.Offset := 0.0001; // avoid endless recursive call
  status := CallBackGet('timestamp', [], resp);
  result := (status = HTTP_SUCCESS) and
            (resp <> '');
  if result then
    SetServerTimestamp(GetInt64(pointer(resp)))
  else
  begin
    InternalLog('/Timestamp call failed -> Server not available', sllWarning);
    fLastErrorMessage := 'Server not available  - ' + TrimU(fLastErrorMessage);
  end;
end;

function TRestClientUri.ServerRemoteLog(Sender: TEchoWriter;
  Level: TSynLogInfo; const Text: RawUtf8): boolean;
begin
  if fRemoteLogThread = nil then
    result := InternalRemoteLogSend(Text)
  else
  begin
    TRemoteLogThread(fRemoteLogThread).AddRow(Text);
    result := true;
  end;
end;

function TRestClientUri.ServerRemoteLog(Level: TSynLogInfo;
  const FormatMsg: RawUtf8; const Args: array of const): boolean;
begin
  result := ServerRemoteLog(nil, Level, FormatUtf8('%00%    %',
    [NowToString(false), LOG_LEVEL_TEXT[Level], FormatUtf8(FormatMsg, Args)]));
end;

procedure TRestClientUri.ServerRemoteLogStart(aLogClass: TSynLogClass;
  aClientOwnedByFamily: boolean);
begin
  if (fRemoteLogClass <> nil) or
     (aLogClass = nil) then
    exit;
  SetLogClass(TSynLog.Void); // this client won't log anything
  if not ServerRemoteLog(sllClient, 'Remote Client % Connected', [self]) then
    // first test server without threading
    raise ERestException.CreateUtf8('%.ServerRemoteLogStart: Connection ' +
      'to RemoteLog server impossible'#13#10'%', [LastErrorMessage]);
  if fRemoteLogThread <> nil then
    raise ERestException.CreateUtf8('%.ServerRemoteLogStart twice', [self]);
  fRemoteLogThread := TRemoteLogThread.Create(self);
  fRemoteLogClass := aLogClass.Add;
  aLogClass.Family.EchoRemoteStart(self, ServerRemoteLog, aClientOwnedByFamily);
  fRemoteLogOwnedByFamily := aClientOwnedByFamily;
end;

procedure TRestClientUri.ServerRemoteLogStop;
begin
  if fRemoteLogClass = nil then
    exit;
  if not fRemoteLogOwnedByFamily then
  begin
    fRemoteLogClass.Log(sllTrace, 'End Echoing to remote server');
    fRemoteLogClass.Family.EchoRemoteStop;
  end;
  fRemoteLogClass := nil;
end;

function TRestClientUri.SetUser(const aUserName, aPassword: RawUtf8;
  aHashedPassword: boolean): boolean;
const
  HASH: array[boolean] of TRestClientSetUserPassword = (
    passClear, passHashed);
begin
  if self = nil then
  begin
    result := false;
    exit;
  end;
  {$ifdef DOMAINRESTAUTH}
  // try Windows/GSSAPI authentication with the current logged user
  result := true;
  if ((TrimU(aUserName) = '') or
      (PosExChar(SSPI_USER_CHAR, aUserName) > 0)) and
     TRestClientAuthenticationSspi.ClientSetUser(
       self, aUserName, aPassword, passKerberosSpn) then
    exit;
  {$endif DOMAINRESTAUTH}
  result := TRestClientAuthenticationDefault.ClientSetUser(self, aUserName,
    aPassword, HASH[aHashedPassword]);
end;

{$ifndef PUREMORMOT2}

function TRestClientUri.Refresh(aID: TID; Value: TOrm;
  var Refreshed: boolean): boolean;
begin
  result := fClient.Refresh(aID, Value, Refreshed);
end;

function TRestClientUri.List(const Tables: array of TOrmClass;
  const SqlSelect: RawUtf8; const SqlWhere: RawUtf8): TOrmTable;
begin
  result := fClient.List(Tables, SqlSelect, SqlWhere);
end;

function TRestClientUri.ListFmt(const Tables: array of TOrmClass;
  const SqlSelect, SqlWhereFormat: RawUtf8; const Args: array of const): TOrmTable;
begin
  result := fClient.ListFmt(Tables, SqlSelect, SqlWhereFormat, Args);
end;

function TRestClientUri.ListFmt(const Tables: array of TOrmClass;
  const SqlSelect, SqlWhereFormat: RawUtf8; const Args, Bounds: array of const): TOrmTable;
begin
  result := fClient.ListFmt(Tables, SqlSelect, SqlWhereFormat, Args, Bounds);
end;

function TRestClientUri.TransactionBeginRetry(aTable: TOrmClass;
  Retries: integer): boolean;
begin
  result := fClient.TransactionBeginRetry(aTable, Retries);
end;

function TRestClientUri.BatchStart(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions): boolean;
begin
  result := fClient.BatchStart(aTable, AutomaticTransactionPerRow, Options);
end;

function TRestClientUri.BatchStartAny(AutomaticTransactionPerRow: cardinal;
  Options: TRestBatchOptions): boolean;
begin
  result := fClient.BatchStartAny(AutomaticTransactionPerRow, Options);
end;

function TRestClientUri.BatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits): integer;
begin
  result := fClient.BatchAdd(Value, SendData, ForceID, CustomFields);
end;

function TRestClientUri.BatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  result := fClient.BatchUpdate(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRestClientUri.BatchDelete(ID: TID): integer;
begin
  result := fClient.BatchDelete(ID);
end;

function TRestClientUri.BatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  result := fClient.BatchDelete(Table, ID);
end;

function TRestClientUri.BatchCount: integer;
begin
  result := fClient.BatchCount;
end;

function TRestClientUri.BatchSend(var Results: TIDDynArray): integer;
begin
  result := fClient.BatchSend(Results);
end;

procedure TRestClientUri.BatchAbort;
begin
  fClient.BatchAbort;
end;

{$endif PUREMORMOT2}


{ ************ TRestClientLibraryRequest after TRestServer.ExportServerGlobalLibraryRequest }

{ TRestClientLibraryRequest }

constructor TRestClientLibraryRequest.Create(aModel: TOrmModel;
  const LibraryName: TFileName);
var
  call: TRestUriParams;
  h: TLibHandle;
begin
  h := LibraryOpen(LibraryName);
  if h = 0 then
    raise ERestException.CreateUtf8('%.Create: LoadLibrary(%) failed',
      [self, LibraryName]);
  fRequest := LibraryResolve(h, 'LibraryRequest');
  call.Init;
  InternalUri(call);
  if call.OutStatus <> HTTP_NOTFOUND then
  begin
    @fRequest := nil;
    LibraryClose(h);
    raise ERestException.CreateUtf8(
      '%.Create: % doesn''t export a valid LibraryRequest() function (ret=%)',
      [self, LibraryName, call.OutStatus]);
  end;
  fLibraryHandle := h;
  Create(aModel, fRequest);
end;

constructor TRestClientLibraryRequest.Create(aModel: TOrmModel;
  aRequest: TLibraryRequest);
begin
  inherited Create(aModel);
  fRequest := aRequest;
end;

destructor TRestClientLibraryRequest.Destroy;
begin
  inherited Destroy;
  if fLibraryHandle<>0 then
    LibraryClose(fLibraryHandle);
end;

procedure TRestClientLibraryRequest.InternalUri(var Call: TRestUriParams);
var
  f: TLibraryRequestFree;
  h, r: PUtf8Char;
  hl, rl: cardinal;
begin
  if @fRequest = nil then
  begin
    // 501 (no valid application or library)
    Call.OutStatus := HTTP_NOTIMPLEMENTED;
    exit;
  end;
  call.LowLevelRemoteIP := '127.0.0.1';
  call.LowLevelConnectionID := PtrUInt(self);
  call.LowLevelConnectionFlags := [llfInProcess, llfSecured];
  h := pointer(call.InHead);
  hl := length(call.InHead);
  r := nil;
  rl := 0;
  Call.OutStatus := fRequest(
    pointer(Call.Url), pointer(Call.Method), pointer(call.InBody),
    length(Call.Url),  length(Call.Method),  length(call.InBody),
    f, h, hl, r, rl, Call.OutInternalState);
  FastSetString(Call.OutHead, h, hl);
  FastSetString(Call.OutBody, r, rl);
  f(h);
  f(r);
end;

function TRestClientLibraryRequest.InternalIsOpen: boolean;
begin
  result := @fRequest <> nil;
end;

procedure TRestClientLibraryRequest.InternalOpen;
begin
end;

procedure TRestClientLibraryRequest.InternalClose;
begin
end;


{ ************ TInterfacedCallback/TBlockingCallback Classes }

{ TInterfacedCallback }

constructor TInterfacedCallback.Create(aRest: TRest; const aGuid: TGuid);
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
  const aGuid: TGuid);
begin
  inherited Create(aRest, aGuid);
  fProcess := TBlockingProcess.Create(aTimeOutMs, fSafe);
end;

destructor TBlockingCallback.Destroy;
begin
  FreeAndNilSafe(fProcess);
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



end.

