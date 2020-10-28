/// REpresentation State Tranfer (REST) Types and Classes on Client side
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.rest.client;

{
  *****************************************************************************

   Client-Side REST Process
    - Client Authentication and Authorization Logic
    - TRestClientRoutingREST/TRestClientRoutingJSON_RPC Routing Schemes
    - TRestClientURI Base Class for Actual Clients

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  variants,
  contnrs,
  mormot.lib.z, // we use zlib crc32 as default URI signature
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
  mormot.soa.client,
  mormot.db.core,
  mormot.rest.core;


{ ************ Client Authentication and Authorization Logic }

type
  TRestClientURI = class;

  /// used by TRestClientURI.URI() to let the client ask for an User name
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
  // TAuthUser.PasswordHashHexa i.e. SHA256('salt'+Value)
  // - passKerberosSPN indicates that the password is the Kerberos SPN domain
  TRestClientSetUserPassword = (
    passClear,
    passHashed,
    passKerberosSPN);

  /// algorithms known by TRestClientAuthenticationSignedURI and
  // TRestServerAuthenticationSignedURI to digitaly compute the
  // session_signature parameter value for a given URI
  // - by default, suaCRC32 will compute fast but not cryptographically secure
  // ! crc32(crc32(privatesalt, timestamp, 8), url, urllen)
  // - suaCRC32C and suaXXHASH will be faster and slightly safer
  // - but you can select other stronger alternatives, which result will be
  // reduced to 32-bit hexadecimal - suaMD5 will be the fastest cryptographic
  // hash available on all platforms, for enhanced security, by calling e.g.
  // ! (aServer.AuthenticationRegister(TRestClientAuthenticationDefault) as
  // !   TRestServerAuthenticationDefault).Algorithm := suaMD5;
  // - suaSHA1, suaSHA256 and suaSHA512 will be the slowest, to provide
  // additional level of trust, depending on your requirements: note that
  // since the hash is reduced to 32-bit resolution, those may not provide
  // higher security than suaMD5
  // - note that SynCrossPlatformRest clients only implements suaCRC32 yet
  TRestAuthenticationSignedURIAlgo = (
    suaCRC32,
    suaCRC32C,
    suaXXHASH,
    suaMD5,
    suaSHA1,
    suaSHA256,
    suaSHA512);

  /// function prototype for TRestClientAuthenticationSignedURI and
  // TRestServerAuthenticationSignedURI computation of the session_signature
  // parameter value
  TOnRestAuthenticationSignedURIComputeSignature = function(
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
    class function ClientComputeSessionKey(Sender: TRestClientURI;
      User: TAuthUser): RawUTF8; virtual; abstract;
    /// is called by ClientComputeSessionKey() overriden method to execute the
    // root/Auth service with the supplied parameters, then retrieve and
    // decode the "result": session key and any other values (e.g. "version")
    class function ClientGetSessionKey(Sender: TRestClientURI;
      User: TAuthUser; const aNameValueParameters: array of const): RawUTF8; virtual;
  public
    /// class method to be used on client side to create a remote session
    // - call this method instead of TRestClientURI.SetUser() if you need
    // a custom authentication class
    // - if saoUserByLogonOrID is defined in the server Options, aUserName may
    // be a TAuthUser.ID and not a TAuthUser.LogonName
    // - if passClear is used, you may specify aHashSalt and aHashRound,
    // to enable PBKDF2_HMAC_SHA256() use instead of plain SHA256(), and increase
    // security on storage side (reducing brute force attack via rainbow tables)
    // - will call the ModelRoot/Auth service, i.e. call TRestServer.Auth()
    // published method to create a session for this user
    // - returns true on success
    class function ClientSetUser(Sender: TRestClientURI;
      const aUserName, aPassword: RawUTF8;
      aPasswordKind: TRestClientSetUserPassword = passClear;
      const aHashSalt: RawUTF8 = ''; aHashRound: integer = 20000): boolean; virtual;
    /// class method to be called on client side to sign an URI
    // - used by TRestClientURI.URI()
    // - shall match the method as expected by RetrieveSession() virtual method
    class procedure ClientSessionSign(Sender: TRestClientURI;
      var Call: TRestURIParams); virtual; abstract;
  end;

  /// class-reference type (metaclass) used to define an authentication scheme
  TRestClientAuthenticationClass = class of TRestClientAuthentication;

  /// weak authentication scheme using URL-level parameter
  TRestClientAuthenticationURI = class(TRestClientAuthentication)
  public
    /// class method to be called on client side to add the SessionID to the URI
    // - append '&session_signature=SessionID' to the url
    class procedure ClientSessionSign(Sender: TRestClientURI;
      var Call: TRestURIParams); override;
  end;

  /// secure authentication scheme using URL-level digital signature
  // - default suaCRC32 format of session_signature is
  // !Hexa8(SessionID)+
  // !Hexa8(Timestamp)+
  // !Hexa8(crc32('SessionID+HexaSessionPrivateKey'+Sha256('salt'+PassWord)+
  // !            Hexa8(Timestamp)+url))
  TRestClientAuthenticationSignedURI = class(TRestClientAuthenticationURI)
  protected
    // class functions implementing TRestAuthenticationSignedURIAlgo
    class function ComputeSignatureCrc32(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureCrc32c(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignaturexxHash(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureMD5(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSHA1(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSHA256(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
    class function ComputeSignatureSHA512(privatesalt: cardinal;
      timestamp, url: PAnsiChar; urllen: integer): cardinal;
  public
    /// retrieve the method to compute the session_signature=.... value
    class function GetComputeSignature(
      algo: TRestAuthenticationSignedURIAlgo): TOnRestAuthenticationSignedURIComputeSignature;
    /// class method to be called on client side to sign an URI
    // - generate the digital signature as expected by overridden RetrieveSession()
    // - timestamp resolution is about 256 ms in the current implementation
    class procedure ClientSessionSign(Sender: TRestClientURI;
      var Call: TRestURIParams); override;
  end;

  /// mORMot secure RESTful authentication scheme
  // - match TRestServerAuthenticationDefault class on server side
  // - this scheme will use a password stored via safe SHA-256 hashing in the
  // TAuthUser ORM table
  TRestClientAuthenticationDefault = class(TRestClientAuthenticationSignedURI)
  protected
    /// class method used on client side to create a remote session
    // - will call the ModelRoot/Auth service, i.e. call TRestServer.Auth()
    // published method to create a session for this user: so
    // TRestServerAuthenticationDefault should be registered on server side
    // - User.LogonName and User.PasswordHashHexa will be checked
    class function ClientComputeSessionKey(Sender: TRestClientURI;
      User: TAuthUser): RawUTF8; override;
  end;

  /// mORMot weak RESTful authentication scheme
  // - this method will authenticate with a given username, but no signature
  // - match TRestServerAuthenticationNone class on server side
  // - on client side, this scheme is not called by TRestClientURI.SetUser()
  // method - so you have to write:
  // ! TRestClientAuthenticationNone.ClientSetUser(Client,'User','');
  TRestClientAuthenticationNone = class(TRestClientAuthenticationURI)
  protected
    /// class method used on client side to create a remote session
    // - will call the ModelRoot/Auth service, i.e. call TRestClient.Auth()
    // published method to create a session for this user: so
    // TRestServerAuthenticationNone should be registered on server side
    // - will check User.LogonName, but User.PasswordHashHexa will be ignored
    class function ClientComputeSessionKey(Sender: TRestClientURI;
      User: TAuthUser): RawUTF8; override;
  end;

  /// abstract class for implementing HTTP authentication
  // - do not use this abstract class, but e.g. TRestClientAuthenticationHttpBasic
  // - this class will transmit the session_signature as HTTP cookie, not at
  // URI level, so is expected to be used only from browsers or old clients
  TRestClientAuthenticationHttpAbstract = class(TRestClientAuthentication)
  protected
    /// should be overriden according to the HTTP authentication scheme
    class function ComputeAuthenticateHeader(
      const aUserName,aPasswordClear: RawUTF8): RawUTF8; virtual; abstract;
  public
    /// class method to be called on client side to sign an URI in Auth Basic
    // resolution is about 256 ms in the current implementation
    // - set "Cookie: mORMot_session_signature=..." HTTP header
    class procedure ClientSessionSign(Sender: TRestClientURI;
      var Call: TRestURIParams); override;
    /// class method to be used on client side to create a remote session
    // - call TRestClientAuthenticationHttpBasic.ClientSetUser() instead of
    // TRestClientURI.SetUser(), and never the method of this abstract class
    // - needs the plain aPassword, so aPasswordKind should be passClear
    // - returns true on success
    class function ClientSetUser(Sender: TRestClientURI;
      const aUserName, aPassword: RawUTF8;
      aPasswordKind: TRestClientSetUserPassword = passClear;
      const aHashSalt: RawUTF8 = ''; aHashRound: integer = 20000): boolean; override;
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
    class procedure ClientSetUserHttpOnly(Sender: TRestClientURI;
      const aUserName, aPasswordClear: RawUTF8); virtual;
  end;

  /// authentication using HTTP Basic scheme
  // - this protocol send both name and password as clear (just base-64 encoded)
  // so should only be used over SSL / HTTPS, or for compatibility reasons
  // - match TRestServerAuthenticationHttpBasic class on server side
  // - will rely on TRestClientAuthenticationNone for authorization
  // - on client side, this scheme is not called by TRestClientURI.SetUser()
  // method - so you have to write:
  // ! TRestClientAuthenticationHttpBasic.ClientSetUser(Client,'User','password');
  // - for a remote proxy-only authentication (without creating any mORMot
  // session), you can write:
  // ! TRestClientAuthenticationHttpBasic.ClientSetUserHttpOnly(Client,'proxyUser','proxyPass');
  TRestClientAuthenticationHttpBasic = class(TRestClientAuthenticationHttpAbstract)
  protected
    /// this overriden method returns "Authorization: Basic ...." HTTP header
    class function ComputeAuthenticateHeader(
      const aUserName,aPasswordClear: RawUTF8): RawUTF8; override;
  end;

  {$ifdef DOMAINAUTH}

  /// authentication of the current logged user using Windows Security Support
  // Provider Interface (SSPI) or GSSAPI library on Linux
  // - is able to authenticate the currently logged user on the client side,
  // using either NTLM (Windows only) or Kerberos - it will allow to safely
  // authenticate on a mORMot server without prompting the user to enter its
  // password
  // - match TRestServerAuthenticationSSPI class on server side
  // - if ClientSetUser() receives aUserName as '', aPassword should be either
  // '' if you expect NTLM authentication to take place, or contain the SPN
  // registration (e.g. 'mymormotservice/myserver.mydomain.tld') for Kerberos
  // authentication
  // - if ClientSetUser() receives aUserName as 'DomainName\UserName', then
  // authentication will take place on the specified domain, with aPassword
  // as plain password value
  TRestClientAuthenticationSSPI = class(TRestClientAuthenticationSignedURI)
  protected
    class function ClientComputeSessionKey(Sender: TRestClientURI;
      User: TAuthUser): RawUTF8; override;
  end;

  {$endif DOMAINAUTH}

  /// store the information about the current session
  // - as set after a sucessfull TRestClientURI.SetUser() method
  TRestClientSession = record
    // for internal use
    Authentication: TRestClientAuthenticationClass;
    IDHexa8: RawUTF8;
    PrivateKey: cardinal;
    Data: RawByteString;
    LastTick64: Int64;
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
    HttpHeader: RawUTF8;
    /// the remote server executable name, as retrieved after a SetUser() success
    Server: RawUTF8;
    /// the remote server version, as retrieved after a SetUser() success
    Version: RawUTF8;
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

{$ifndef PUREMORMOT2}
// backward compatibility types redirections

  TSQLRestServerAuthenticationClientSetUserPassword = TRestClientSetUserPassword;
  TSQLRestServerAuthenticationSignedURIAlgo = TRestAuthenticationSignedURIAlgo;
  TSQLRestServerAuthenticationSignedURIComputeSignature  =
    TOnRestAuthenticationSignedURIComputeSignature;
  // TRestServerAuthentication* classes have client-side only corresponding
  // types named as TRestClientAuthentication*

{$endif PUREMORMOT2}



{ ************ TRestClientRoutingREST/TRestClientRoutingJSON_RPC Routing Schemes }

  //// used to customize TRestClientRouting.ClientSideInvoke process
  TRestClientSideInvoke = set of (
    csiAsOctetStream);

  /// abstract Client side service routing
  // - match TRestServerURIContext reciprocal class
  // - never use this abstract class, but rather TRestRoutingREST or
  // TRestRoutingJSON_RPC classes
  TRestClientRouting = class
  public
    /// at Client Side, compute URI and BODY according to the routing scheme
    // - abstract implementation which is to be overridden
    // - as input, method should be the method name to be executed,
    // params should contain the incoming parameters as JSON CSV (without []),
    // and clientDriven ID should contain the optional Client ID value
    // - at output, should update the HTTP uri corresponding to the proper
    // routing, and should return the corresponding HTTP body within sent
    class procedure ClientSideInvoke(var uri: RawUTF8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUTF8;
      out sent, head: RawUTF8); virtual; abstract;
  end;

  /// class used to define the Client side expected routing
  // - match TRestServerURIContextClass reciprocal meta-class
  // - most of the internal methods are declared as virtual, so it allows any
  // kind of custom routing or execution scheme
  // - TRestRoutingREST and TRestRoutingJSON_RPC classes are provided
  // in this unit, to allow RESTful and JSON/RPC protocols
  TRestClientRoutingClass = class of TRestClientRouting;

  /// client calling context using simple REST for interface-based services
  // - match TRestServerRoutingREST reciprocal class
  TRestClientRoutingREST = class(TRestClientRouting)
    /// at Client Side, compute URI and BODY according to RESTful routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='1,2' and
    // clientDrivenID='1234' -> on output uri='root/Calculator.Add/1234' and
    // sent='[1,2]'
    class procedure ClientSideInvoke(var uri: RawUTF8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUTF8;
      out sent, head: RawUTF8); override;
  end;

  /// client calling context using simple REST for interface-based services
  // - match TRestServerRoutingJSON_RPC reciprocal class
  TRestClientRoutingJSON_RPC = class(TRestClientRouting)
     /// at Client Side, compute URI and BODY according to JSON/RPC routing scheme
    // - e.g. on input uri='root/Calculator', method='Add', params='1,2' and
    // clientDrivenID='1234' -> on output uri='root/Calculator' and
    // sent={"method":"Add","params":[1,2],"id":1234}
    class procedure ClientSideInvoke(var uri: RawUTF8;
      ctxt: TRestClientSideInvoke;
      const method, params, clientDrivenID: RawUTF8;
      out sent, head: RawUTF8); override;
  end;

{$ifndef PUREMORMOT2}
// backward compatibility types redirections

  TSQLRestServerURIContextClientInvoke = TRestClientSideInvoke;

{$endif PUREMORMOT2}


{ ************ TRestClientURI Base Class for Actual Clients }

  /// called by TRestClientURI.URI() when an error occurred
  // - so that you may have a single entry point for all client-side issues
  // - information will be available in Sender's LastErrorCode and
  // LastErrorMessage properties
  // - if the error comes from an Exception, it will be supplied as parameter
  // - the REST context (if any) will be supplied within the Call parameter,
  // and in this case Call^.OutStatus=HTTP_NOTIMPLEMENTED indicates a broken
  // connection
  TOnClientFailed = procedure(Sender: TRestClientURI; E: Exception;
    Call: PRestURIParams) of object;

  /// store information about registered interface callbacks
  TRestClientCallbackItem = record
    /// the identifier of the callback, as sent to the server side
    // - computed from TRestClientURICallbacks.fCurrentID counter
    ID: integer;
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
  TRestClientCallbacks = class(TSynPersistentLock)
  protected
    fCurrentID: integer;
    function UnRegisterByIndex(index: integer): boolean;
  public
    /// the associated REST instance
    Owner: TRestClientURI;
    /// how many callbacks are registered
    Count: integer;
    /// list of registered interface callbacks
    List: array of TRestClientCallbackItem;
    /// initialize the storage list
    constructor Create(aOwner: TRestClientURI); reintroduce;
    /// register a callback event interface instance from a new computed ID
    function DoRegister(aInstance: pointer; aFactory: TInterfaceFactory): integer; overload;
    /// register a callback event interface instance from its supplied ID
    procedure DoRegister(aID: Integer; aInstance: pointer; aFactory: TInterfaceFactory); overload;
    /// delete all callback events from the internal list, as specified by its instance
    // - note that the same IInvokable instance may be registered for several IDs
    function UnRegister(aInstance: pointer): boolean; overload;
    /// find the index of the ID in the internal list
    // - warning: this method should be called within Safe.Lock/Safe.Unlock
    function FindIndex(aID: integer): integer;
    /// find a matching callback
    // - will call FindIndex(aItem.ID) within Safe.Lock/Safe.Unlock
    // - returns TRUE if aItem.ID was found and aItem filled, FALSE otherwise
    function FindEntry(var aItem: TRestClientCallbackItem): boolean;
    /// find a matching entry
    // - will call FindIndex(aID) within Safe.Lock/Safe.Unlock
    // - returns TRUE if aID was found and aInstance/aFactory set, FALSE otherwise
    function FindAndRelease(aID: integer): boolean;
  end;

  /// signature e.g. of the TRestClientURI.OnSetUser event handler
  TOnRestClientNotify = procedure(Sender: TRestClientURI) of object;

  /// a generic REpresentational State Transfer (REST) client with URI
  // - URI are standard Collection/Member implemented as ModelRoot/TableName/TableID
  // - handle RESTful commands GET POST PUT DELETE LOCK UNLOCK
  TRestClientURI = class(TRest)
  protected
    fOrmClient: IRestOrmClient;
    fSession: TRestClientSession;
    fComputeSignature: TOnRestAuthenticationSignedURIComputeSignature;
    fOnIdle: TOnIdleSynBackgroundThread;
    fOnFailed: TOnClientFailed;
    fOnAuthentificationFailed: TOnAuthentificationFailed;
    fOnSetUser: TOnRestClientNotify;
    fBackgroundThread: TSynBackgroundThreadEvent;
    fMaximumAuthentificationRetry: Integer;
    fRetryOnceOnTimeout: boolean;
    fInternalState: set of (isOpened, isDestroying, isInAuth);
    fLastErrorCode: integer;
    fLastErrorMessage: RawUTF8;
    fLastErrorException: ExceptClass;
    fSafe: IAutoLocker; // to make the URI() method thread-safe
    fRemoteLogClass: TSynLog;
    fRemoteLogThread: TObject; // private TRemoteLogThread
    fRemoteLogOwnedByFamily: boolean;
    fServicesRouting: TRestClientRoutingClass;
    fServicePublishOwnInterfaces: RawUTF8;
    fFakeCallbacks: TRestClientCallbacks;
    {$ifdef MSWINDOWS}
    fServiceNotificationMethodViaMessages: record
      Wnd: HWND;
      Msg: cardinal;
    end;
    {$endif MSWINDOWS}
    procedure SetRoutingClass(aServicesRouting: TRestClientRoutingClass);
    procedure SetSessionHeartbeatSeconds(timeout: integer);
    function GetOnIdleBackgroundThreadActive: boolean;
    procedure OnBackgroundProcess(Sender: TSynBackgroundThreadEvent;
      ProcessOpaqueParam: pointer);
    procedure SetLastException(E: Exception = nil; ErrorCode: integer = HTTP_BADREQUEST;
      Call: PRestURIParams = nil);
    function InternalRemoteLogSend(const aText: RawUTF8): boolean;
    procedure InternalNotificationMethodExecute(var Ctxt: TRestURIParams); virtual;
    /// will call timestamp/info if the session has currently not been retrieved
    function GetSessionVersion: RawUTF8;
    // register the user session to the TRestClientURI instance
    function SessionCreate(aAuth: TRestClientAuthenticationClass;
      var aUser: TAuthUser; const aSessionKey: RawUTF8): boolean;
    // call each fSession.HeartbeatSeconds delay
    procedure SessionRenewEvent(Sender: TSynBackgroundTimer; Event: TWaitResult;
      const Msg: RawUTF8);
    /// abstract methods to be implemented with a local, piped or HTTP/1.1 provider
    // - you can specify some POST/PUT data in Call.OutBody (leave '' otherwise)
    // - return the execution result in Call.OutStatus
    // - for clients, RestAccessRights is never used
    procedure InternalURI(var Call: TRestURIParams); virtual; abstract;
    /// overridden protected method shall check if not connected to reopen it
    // - shall return TRUE on success, FALSE on any connection error
    function InternalCheckOpen: boolean; virtual; abstract;
    /// overridden protected method shall force the connection to be closed,
    // - a next call to InternalCheckOpen method shall re-open the connection
    procedure InternalClose; virtual; abstract;
  {$ifndef PUREMORMOT2}
    // backward compatibility redirections to the homonymous IRestOrmClient methods
    // see IRestOrmClient documentation for the proper use information
  public
    function Refresh(aID: TID; Value: TOrm; var Refreshed: boolean): boolean;
    function List(const Tables: array of TOrmClass; const SQLSelect: RawUTF8 = 'RowID';
      const SQLWhere: RawUTF8 = ''): TOrmTable;
    function ListFmt(const Tables: array of TOrmClass;
      const SQLSelect, SQLWhereFormat: RawUTF8; const Args: array of const): TOrmTable; overload;
    function ListFmt(const Tables: array of TOrmClass;
      const SQLSelect, SQLWhereFormat: RawUTF8; const Args, Bounds: array of const): TOrmTable; overload;
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
      aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean); override;
    /// release memory and close client connection
    // - also unlock all still locked records by this client
    destructor Destroy; override;
    /// called by TRestOrm.Create overriden constructor to set fOrm from IRestOrm
    procedure SetOrmInstance(aORM: TInterfacedObject); override;
    /// main method calling the remote Server via a RESTful command
    // - redirect to the InternalURI() abstract method, which should be
    // overridden for local, pipe, HTTP/1.1 or WebSockets actual communication
    // - this method will sign the url with the appropriate digital signature
    // according to the current SessionUser property
    // - this method will retry the connection in case of authentication failure
    // (i.e. if the session was closed by the remote server, for any reason -
    // mostly a time out) if the OnAuthentificationFailed event handler is set
    function URI(const url, method: RawUTF8; Resp: PRawUTF8 = nil;
      Head: PRawUTF8 = nil; SendData: PRawUTF8 = nil): Int64Rec;
    /// wrapper to the protected URI method to call a method on the server, using
    // a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - this version will use a GET with supplied parameters (which will be encoded
    // with the URL)
    function CallBackGet(const aMethodName: RawUTF8;
      const aNameValueParameters: array of const;
      out aResponse: RawUTF8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUTF8 = nil): integer;
    /// wrapper to the protected URI method to call a method on the server, using
    // a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    // - returns the UTF-8 decoded JSON result (server must reply with one
    // "result":"value" JSON object)
    // - this version will use a GET with supplied parameters (which will be encoded
    // with the URL)
    function CallBackGetResult(const aMethodName: RawUTF8;
      const aNameValueParameters: array of const;
      aTable: TOrmClass = nil; aID: TID = 0): RawUTF8;
    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful PUT request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - this version will use a PUT with the supplied raw UTF-8 data
    function CallBackPut(const aMethodName, aSentData: RawUTF8;
      out aResponse: RawUTF8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUTF8 = nil): integer;
    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful with any kind of request
    // - returns the HTTP error code (e.g. 200/HTTP_SUCCESS on success)
    // - for GET/PUT methods, you should better use CallBackGet/CallBackPut
    function CallBack(method: TURIMethod; const aMethodName,aSentData: RawUTF8;
      out aResponse: RawUTF8; aTable: TOrmClass = nil; aID: TID = 0;
      aResponseHead: PRawUTF8 = nil): integer;
    /// to be called before CallBack() if the client could ignore the answer
    // - do nothing by default, but overriden e.g. in TSQLHttpClientWebsockets
    procedure CallbackNonBlockingSetHeader(out Header: RawUTF8); virtual;

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
      const aContractExpected: RawUTF8 = ''): boolean; overload; virtual;
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
      const aContractExpected: RawUTF8 = '';
      aIgnoreAnyException: boolean = true): TServiceFactory; overload;
    /// register and retrieve the sicClientDriven Service instance
    // - will return TRUE on success, filling Obj output variable with the
    // corresponding interface instance
    // - will return FALSE on error
    function ServiceRegisterClientDriven(aInterface: PRttiInfo; out Obj;
      const aContractExpected: RawUTF8 = ''): boolean; overload;
    /// register one or several Services on the client side via their interfaces
    // - this method expects the interface(s) to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefine(const aInterfaces: array of TGUID;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUTF8 = ''): boolean; overload;
    /// register a Service on the client side via its interface
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefine(const aInterface: TGUID;
      aInstanceCreation: TServiceInstanceImplementation = sicSingle;
      const aContractExpected: RawUTF8 = '';
      aIgnoreAnyException: boolean = true): TServiceFactoryClient; overload;
    /// register and retrieve the sicClientDriven Service instance
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    function ServiceDefineClientDriven(const aInterface: TGUID; out Obj;
      const aContractExpected: RawUTF8 = ''): boolean;
    /// register a sicShared Service instance communicating via JSON objects
    // - will force SERVICE_CONTRACT_NONE_EXPECTED, ParamsAsJSONObject=true and
    // ResultAsJSONObjectWithoutResult=true
    // - may be used e.g. for accessing a sessionless public REST/JSON API, i.e.
    // ! TRestServer.ServiceDefine(...).ResultAsJSONObjectWithoutResult := true
    // - this method expects the interface to have been registered previously:
    // ! TInterfaceFactory.RegisterInterfaces([TypeInfo(IMyInterface),...]);
    // - aIgnoreAnyException may be set to TRUE if the server is likely
    // to not propose this service, and any exception is to be catched
    function ServiceDefineSharedAPI(const aInterface: TGUID;
      const aContractExpected: RawUTF8 = SERVICE_CONTRACT_NONE_EXPECTED;
      aIgnoreAnyException: boolean = false): TServiceFactoryClient;
    /// allow to notify a server the services this client may be actually capable
    // - when this client will connect to a remote server to access its services,
    // it will register its own services, supplying its TRestServer instance,
    // and its corresponding public URI, within its '_contract_' internal call
    // - it will allow automatic service discovery of Peer To Peer Servers,
    // without the need of an actual centralized SOA catalog service: any
    // client could retrieve an associated REST server for a given service,
    // via the ServiceRetrieveAssociated method
    property ServicePublishOwnInterfaces: RawUTF8
      read fServicePublishOwnInterfaces write fServicePublishOwnInterfaces;
    /// the routing class of the service remote request on client side
    // - by default, contains TRestClientRoutingREST, i.e. an URI-based
    // layout which is secure (since will use our RESTful authentication scheme),
    // and also very fast
    // - but TRestClientRoutingJSON_RPC can e.g. be set (with
    // TRestServerRoutingJSON_RPC on server sides), if the client will rather
    // use JSON/RPC alternative pattern
    // - NEVER set the abstract TRestClientRouting class on this property
    property ServicesRouting: TRestClientRoutingClass
      read fServicesRouting write SetRoutingClass;
    // internal methods used by mormot.soa.client
    function FakeCallbackRegister(Sender: TServiceFactory;
      const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
      ParamValue: Pointer): integer; virtual;
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: integer; Instance: pointer): boolean; virtual;
    property FakeCallbacks: TRestClientCallbacks read fFakeCallbacks;

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
    // ! TSQLLog.Family.EchoCustom := aClient.ServerRemoteLog;
    function ServerRemoteLog(Sender: TBaseWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean; overload; virtual;
    /// internal method able to emulate a call to TSynLog.Add.Log()
    // - will compute timestamp and event text, than call the overloaded
    // ServerRemoteLog() method
    function ServerRemoteLog(Level: TSynLogInfo; const FormatMsg: RawUTF8;
      const Args: array of const): boolean; overload;
    /// start to send all logs to the server 'RemoteLog' method-based service
    // - will associate the EchoCustom callback of the running log class to the
    // ServerRemoteLog() method
    // - if aClientOwnedByFamily is TRUE, this TRestClientURI instance
    // lifetime will be managed by TSynLogFamily - which is mostly wished
    // - if aClientOwnedByFamily is FALSE, you should manage this instance
    // life time, and may call ServerRemoteLogStop to stop remote logging
    // - warning: current implementation will disable all logging for this
    // TRestClientURI instance, to avoid any potential concern (e.g. for
    // multi-threaded process, or in case of communication error): you should
    // therefore use this TRestClientURI connection only for the remote log
    // server, e.g. via TSQLHttpClientGeneric.CreateForRemoteLogging() - do
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
    // (i.e. SHA256('salt'+Value) as in TAuthUser.SetPasswordPlain method)
    // - if SSPIAUTH conditional is defined, and aUserName='', a Windows
    // authentication will be performed via TRestClientAuthenticationSSPI -
    // in this case, aPassword will contain the SPN domain for Kerberos
    // (otherwise NTLM will be used), and table TAuthUser shall contain
    // an entry for the logged Windows user, with the LoginName in form
    // 'DomainName\UserName'
    // - you can directly create the class method ClientSetUser() of a given
    // TRestClientAuthentication inherited class, if neither
    // TRestClientAuthenticationDefault nor TRestClientAuthenticationSSPI
    // match your need
    function SetUser(const aUserName, aPassword: RawUTF8;
      aHashedPassword: boolean = false): boolean;
    /// clear session and call the /auth service on the server to notify shutdown
    // - is called by Destroy and SetUser/ClientSetUser methods, so you should
    // not have usually to call this method directly
    procedure SessionClose;
    /// internal method to retrieve the current Session TAuthUser.ID
    function GetCurrentSessionUserID: TID; override;
    /// customize the session_signature signing algorithm with a specific function
    // - will be used by TRestServerAuthenticationSignedURI classes,
    // e.g. TRestServerAuthenticationDefault instead of the algorithm
    // specified by the server at session handshake
    property ComputeSignature: TOnRestAuthenticationSignedURIComputeSignature
      read fComputeSignature write fComputeSignature;
    /// the current session information as set by a successfull SetUser() call
    property Session: TRestClientSession read fSession;
    /// the current user as set by SetUser() method
    // - contains nil if no User is currently authenticated
    property SessionUser: TAuthUser read fSession.User;
    /// access to the low-level HTTP header used for authentication
    // - you can force here your own header, e.g. a JWT as authentication bearer
    // or as in TRestClientAuthenticationHttpAbstract.ClientSetUserHttpOnlyUser
    property SessionHttpHeader: RawUTF8
      read fSession.HttpHeader write fSession.HttpHeader;

    {$ifdef MSWINDOWS}

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
    // - is defines as a class procedure, since the underlying TRestClientURI
    // instance has no impact here: a single WM_* handler is enough for
    // several TRestClientURI instances
    class procedure ServiceNotificationMethodExecute(var Msg: TMessage);

    {$endif MSWINDOWS}

    /// set a callback event to be executed in loop during remote blocking
    // process, e.g. to refresh the UI during a somewhat long request
    // - if not set, the request will be executed in the current thread,
    // so may block the User Interface
    // - you can assign a callback to this property, calling for instance
    // Application.ProcessMessages, to execute the remote request in a
    // background thread, but let the UI still be reactive: the
    // TLoginForm.OnIdleProcess and OnIdleProcessForm methods of
    // mORMotUILogin.pas will match this property expectations
    property OnIdle: TOnIdleSynBackgroundThread read fOnIdle write fOnIdle;
    /// TRUE if the background thread is active, and OnIdle event is called
    // during process
    // - to be used e.g. to ensure no re-entrance from User Interface messages
    property OnIdleBackgroundThreadActive: boolean read GetOnIdleBackgroundThreadActive;
    /// this Event is called in case of remote authentication failure
    // - client software can ask the user to enter a password and user name
    // - if no event is specified, the URI() method will return directly
    // an HTTP_FORBIDDEN "403 Forbidden" error code
    property OnAuthentificationFailed: TOnAuthentificationFailed
      read fOnAuthentificationFailed write fOnAuthentificationFailed;
    /// this Event is called if URI() was not successfull
    // - the callback will have all needed information
    // - e.g. Call^.OutStatus=HTTP_NOTIMPLEMENTED indicates a broken connection
    property OnFailed: TOnClientFailed read fOnFailed write fOnFailed;
    /// this Event is called when a user is authenticated
    // - is called always, on each TRestClientURI.SetUser call
    // - you can check the Sender.SessionUser property pointing to the current
    // authenticated user, or nil if authentication failed
    // - could be used to refresh the User Interface layout according to
    // current authenticated user rights, or to subscribe to some services
    // via callbacks
    property OnSetUser: TOnRestClientNotify read fOnSetUser write fOnSetUser;
  published
    /// low-level error code, as returned by server
    // - check this value about HTTP_* constants
    // - HTTP_SUCCESS or HTTP_CREATED mean no error
    // - otherwise, check LastErrorMessage property for additional information
    // - this property value will record status codes returned by URI() method
    property LastErrorCode: integer read fLastErrorCode;
    /// low-level error message, as returned by server
    // - this property value will record content returned by URI() method in
    // case of an error, or '' if LastErrorCode is HTTP_SUCCESS or HTTP_CREATED
    property LastErrorMessage: RawUTF8 read fLastErrorMessage;
    /// low-level exception class, if any
    // - will record any Exception class raised within URI() method
    // - contains nil if URI() execution did not raise any exception (which
    // is the most expected behavior, since server-side errors are trapped
    // into LastErrorCode/LastErrorMessage properties
    property LastErrorException: ExceptClass read fLastErrorException;
    /// maximum additional retry occurence
    // - defaut is 1, i.e. will retry once
    // - set OnAuthentificationFailed to nil in order to avoid any retry
    property MaximumAuthentificationRetry: Integer
      read fMaximumAuthentificationRetry write fMaximumAuthentificationRetry;
    /// if the client shall retry once in case of "408 REQUEST TIMEOUT" error
    property RetryOnceOnTimeout: boolean
      read fRetryOnceOnTimeout write fRetryOnceOnTimeout;
    /// the current session ID as set after a successfull SetUser() method call
    // - equals 0 (CONST_AUTHENTICATION_SESSION_NOT_STARTED) if the session
    // is not started yet - i.e. if SetUser() call failed
    // - equals 1 (CONST_AUTHENTICATION_NOT_USED) if authentication mode
    // is not enabled - i.e. after a fresh Create() without SetUser() call
    property SessionID: cardinal read fSession.ID;
    /// the remote server executable name, as retrieved after a SetUser() success
    property SessionServer: RawUTF8 read fSession.Server;
    /// the remote server version, as retrieved after a SetUser() success
    property SessionVersion: RawUTF8 read fSession.Version;
    /// the remote server session tiemout in minutes, as retrieved after
    // a SetUser() success
    // - will be used to set SessionHeartbeatSeconds default
    property SessionServerTimeout: integer read fSession.ServerTimeout;
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


{$ifndef PUREMORMOT2}
// backward compatibility types redirections

type
  TSQLRestClientURI = TRestClientURI;

{$endif PUREMORMOT2}

function ToText(a: TRestAuthenticationSignedURIAlgo): PShortString; overload;



implementation

uses
  mormot.orm.client; // for injection of TRestOrmClientURI.URI field


{ ************ Client Authentication and Authorization Logic }

function ToText(a: TRestAuthenticationSignedURIAlgo): PShortString;
begin
  result := GetEnumName(TypeInfo(TRestAuthenticationSignedURIAlgo), ord(a));
end;


{ TRestClientAuthentication }

class function TRestClientAuthentication.ClientGetSessionKey(
  Sender: TRestClientURI; User: TAuthUser;
  const aNameValueParameters: array of const): RawUTF8;
var
  resp: RawUTF8;
  values: array[0..9] of TValuePUTF8Char;
  a: integer;
  algo: TRestAuthenticationSignedURIAlgo absolute a;
begin
  result := '';
  if (Sender.CallBackGet('Auth', aNameValueParameters, resp) <> HTTP_SUCCESS) or
     (JSONDecode(pointer({%H-}resp),
      ['result', 'data', 'server', 'version', 'logonid', 'logonname',
       'logondisplay', 'logongroup', 'timeout', 'algo'], @values) = nil) then
    Sender.fSession.Data := '' // reset temporary 'data' field
  else
  begin
    values[0].ToUTF8(result);
    Base64ToBin(PAnsiChar(values[1].Value), values[1].ValueLen, Sender.fSession.Data);
    values[2].ToUTF8(Sender.fSession.Server);
    values[3].ToUTF8(Sender.fSession.Version);
    User.IDValue := GetInt64(values[4].Value);
    User.LogonName := values[5].ToUTF8; // set/fix using values from server
    User.DisplayName := values[6].ToUTF8;
    User.GroupRights := pointer(values[7].ToInteger);
    Sender.fSession.ServerTimeout := values[8].ToInteger;
    if Sender.fSession.ServerTimeout <= 0 then
      Sender.fSession.ServerTimeout := 60; // default 1 hour if not suppplied
    a := GetEnumNameValueTrimmed(TypeInfo(TRestAuthenticationSignedURIAlgo),
      values[9].Value, values[9].ValueLen);
    if a >= 0 then
      Sender.fComputeSignature :=
        TRestClientAuthenticationSignedURI.GetComputeSignature(algo);
  end;
end;

class function TRestClientAuthentication.ClientSetUser(
  Sender: TRestClientURI; const aUserName, aPassword: RawUTF8;
  aPasswordKind: TRestClientSetUserPassword; const aHashSalt: RawUTF8;
  aHashRound: integer): boolean;
var
  U: TAuthUser;
  key: RawUTF8;
begin
  result := false;
  if Sender = nil then
    exit;
  try
    Sender.SessionClose;  // ensure Sender.SessionUser=nil
    U := TAuthUser(Sender.fModel.GetTableInherited(TAuthUser).Create);
    try
      U.LogonName := trim(aUserName);
      U.DisplayName := U.LogonName;
      if aPasswordKind <> passClear then
        U.PasswordHashHexa := aPassword
      else if aHashSalt = '' then
        U.PasswordPlain := aPassword
      else
        // compute SHA256('salt'+aPassword)
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
  Sender: TRestClientURI; User: TAuthUser): RawUTF8;
var
  aServerNonce, aClientNonce: RawUTF8;
  rnd: THash256;
begin
  result := '';
  if User.LogonName = '' then
    exit;
  aServerNonce := Sender.CallBackGetResult('Auth', ['UserName', User.LogonName]);
  if aServerNonce = '' then
    exit;
  TAESPRNG.Main.FillRandom(@rnd, SizeOf(rnd));
  aClientNonce := BinToHexLower(@rnd, SizeOf(rnd));
  result := ClientGetSessionKey(Sender, User, [
    'UserName', User.LogonName,
    'Password', Sha256(Sender.fModel.Root + aServerNonce + aClientNonce +
                       User.LogonName + User.PasswordHashHexa),
    'ClientNonce', aClientNonce]);
end;


{ TRestClientAuthenticationSignedURI }

{ Some Numbers - Indicative only!
  - Client side REST sign with crc32: 794,759 assertions passed  730.86ms
  - Client side REST sign with crc32c: 794,753 assertions passed  718.26ms
  - Client side REST sign with xxhash: 794,753 assertions passed  717.63ms
  - Client side REST sign with md5: 794,753 assertions passed  741.87ms
  - Client side REST sign with sha256: 794,753 assertions passed  767.58ms
  - Client side REST sign with sha512: 794,753 assertions passed  800.34ms
}

class function TRestClientAuthenticationSignedURI.ComputeSignatureCrc32(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // historical algorithm, from zlib crc32 polynom
  result := crc32(crc32(privatesalt, timestamp, 8), url, urllen);
end;

class function TRestClientAuthenticationSignedURI.ComputeSignatureCrc32c(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // faster on SSE4.2 CPU, and slightly more secure if not cascaded
  result := crc32c(privatesalt, timestamp, 8) xor crc32c(privatesalt, url, urllen);
end;

class function TRestClientAuthenticationSignedURI.ComputeSignaturexxHash(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
begin
  // xxHash32 has no immediate reverse function, but is really weak
  result := xxHash32(privatesalt, timestamp, 8) xor xxHash32(privatesalt, url, urllen);
end;

class function TRestClientAuthenticationSignedURI.ComputeSignatureMD5(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash128Rec;
  MD5: TMD5;
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

class function TRestClientAuthenticationSignedURI.ComputeSignatureSHA1(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: array[0..(SizeOf(TSHA1Digest) div 4) - 1] of cardinal;
  SHA1: TSHA1;
  i: PtrInt;
begin
  SHA1.Init;
  SHA1.Update(@privatesalt, 4);
  SHA1.Update(timestamp, 8);
  SHA1.Update(url, urllen);
  SHA1.Final(TSHA1Digest(digest));
  result := digest[0];
  for i := 1 to high(digest) do
    // we may have used the first 32-bit of the digest, but cascaded xor is fine
    result := result xor digest[i];
end;

class function TRestClientAuthenticationSignedURI.ComputeSignatureSHA256(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash256Rec;
  SHA256: TSHA256;
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

class function TRestClientAuthenticationSignedURI.ComputeSignatureSHA512(
  privatesalt: cardinal; timestamp, url: PAnsiChar; urllen: integer): cardinal;
var
  digest: THash512Rec;
  SHA512: TSHA512;
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

class function TRestClientAuthenticationSignedURI.GetComputeSignature(
  algo: TRestAuthenticationSignedURIAlgo): TOnRestAuthenticationSignedURIComputeSignature;
begin
  // FPC doesn't allow to use constants for procedure of object
  case algo of
    suaCRC32C:
      result := ComputeSignatureCrc32c;
    suaXXHASH:
      result := ComputeSignaturexxHash;
    suaMD5:
      result := ComputeSignatureMD5;
    suaSHA1:
      result := ComputeSignatureSHA1;
    suaSHA256:
      result := ComputeSignatureSHA256;
    suaSHA512:
      result := ComputeSignatureSHA512;
  else
    result := ComputeSignatureCrc32; // default/legacy/fallback
  end;
end;

class procedure TRestClientAuthenticationSignedURI.ClientSessionSign(
  Sender: TRestClientURI; var Call: TRestURIParams);
var
  nonce, blankURI: RawUTF8;
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
    Call.url := Call.url + fSession.IDHexa8 + nonce + CardinalToHexLower(
      Sender.fComputeSignature(fSession.PrivateKey, Pointer(nonce),
        Pointer(blankURI), length(blankURI)));
  end;
end;


{ TRestClientAuthenticationURI }

class procedure TRestClientAuthenticationURI.ClientSessionSign(
  Sender: TRestClientURI; var Call: TRestURIParams);
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
  Sender: TRestClientURI; User: TAuthUser): RawUTF8;
begin
  result := ClientGetSessionKey(Sender, User, ['UserName', User.LogonName]);
end;


{ TRestClientAuthenticationHttpAbstract }

class procedure TRestClientAuthenticationHttpAbstract.ClientSessionSign(
  Sender: TRestClientURI; var Call: TRestURIParams);
begin
  if (Sender <> nil) and
     (Sender.Session.ID <> 0) and
     (Sender.Session.User <> nil) then
    Call.InHead := Trim(Call.InHead + // session ID transmitted as HTTP cookie
      (#13#10'Cookie: ' + REST_COOKIE_SESSION + '=') + Sender.Session.IDHexa8);
end;

class function TRestClientAuthenticationHttpAbstract.ClientSetUser(
  Sender: TRestClientURI; const aUserName, aPassword: RawUTF8;
  aPasswordKind: TRestClientSetUserPassword;
  const aHashSalt: RawUTF8; aHashRound: integer): boolean;
var
  res: RawUTF8;
  U: TAuthUser;
begin
  result := false;
  if (aUserName = '') or
     (Sender = nil) then
    exit;
  if aPasswordKind <> passClear then
    raise ERestException.CreateUTF8(
      '%.ClientSetUser(%) expects passClear', [self, Sender]);
  Sender.SessionClose; // ensure Sender.SessionUser=nil
  try
    // inherited ClientSetUser() won't fit with server's Auth() method
    ClientSetUserHttpOnly(Sender, aUserName, aPassword);
    Sender.fSession.Authentication := self; // to enable ClientSessionSign()
    U := TAuthUser(Sender.fModel.GetTableInherited(TAuthUser).Create);
    try
      U.LogonName := trim(aUserName);
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
  Sender: TRestClientURI; const aUserName, aPasswordClear: RawUTF8);
begin
  Sender.fSession.HttpHeader := ComputeAuthenticateHeader(aUserName, aPasswordClear);
end;


{ TRestClientAuthenticationHttpBasic }

class function TRestClientAuthenticationHttpBasic.ComputeAuthenticateHeader(
  const aUserName, aPasswordClear: RawUTF8): RawUTF8;
begin
  result := 'Authorization: Basic ' + BinToBase64(aUserName + ':' + aPasswordClear);
end;


{$ifdef DOMAINAUTH}

class function TRestClientAuthenticationSSPI.ClientComputeSessionKey(
  Sender: TRestClientURI; User: TAuthUser): RawUTF8;
var
  SecCtx: TSecContext;
  WithPassword: boolean;
  OutData: RawByteString;
begin
  result := '';
  InvalidateSecContext(SecCtx, 0);
  WithPassword := User.LogonName <> '';
  Sender.fSessionData := '';
  try
    repeat
      if WithPassword then
        ClientSSPIAuthWithPassword(SecCtx, Sender.fSessionData, User.LogonName,
          User.PasswordHashHexa, OutData)
      else
        ClientSSPIAuth(SecCtx, Sender.fSessionData, User.PasswordHashHexa, OutData);
      if OutData = '' then
        break;
      if result <> '' then
        break; // 2nd pass
      // 1st call will return data, 2nd call SessionKey
      result := ClientGetSessionKey(Sender, User,
        ['UserName', '', 'data', BinToBase64(OutData)]);
    until Sender.fSessionData = '';
    if result <> '' then
      result := SecDecrypt(SecCtx, Base64ToBin(result));
  finally
    FreeSecContext(SecCtx);
  end;
  // authenticated by Windows on the server side: use the returned
  // SessionKey + PasswordHashHexa to sign the URI, as usual
  User.PasswordHashHexa := ''; // should not appear on URI signature
end;

{$endif DOMAINAUTH}


{ ************ TRestClientRoutingREST/TRestClientRoutingJSON_RPC Routing Schemes }

{ TRestClientRoutingREST }

class procedure TRestClientRoutingREST.ClientSideInvoke(var uri: RawUTF8;
  ctxt: TRestClientSideInvoke; const method, params, clientDrivenID: RawUTF8;
  out sent, head: RawUTF8);
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


{ TRestClientRoutingJSON_RPC }

class procedure TRestClientRoutingJSON_RPC.ClientSideInvoke(var uri: RawUTF8;
  ctxt: TRestClientSideInvoke; const method, params, clientDrivenID: RawUTF8;
  out sent, head: RawUTF8);
begin
  sent := '{"method":"' + method + '","params":[' + params;
  if clientDrivenID = '' then
    sent := sent + ']}'
  else
    sent := sent + '],"id":' + clientDrivenID + '}';
end;



{ ************ TRestClientURI Base Class for Actual Clients }

{ TRestClientCallbacks }

constructor TRestClientCallbacks.Create(aOwner: TRestClientURI);
begin
  inherited Create;
  Owner := aOwner;
end;

function TRestClientCallbacks.FindIndex(aID: integer): integer;
begin
  if self <> nil then
    for result := 0 to Count - 1 do
      if List[result].ID = aID then
        exit;
  result := -1;
end;

function TRestClientCallbacks.FindEntry(
  var aItem: TRestClientCallbackItem): boolean;
var
  i: integer;
  P: PRestClientCallbackItem;
begin
  result := false;
  if self = nil then
    exit;
  fSafe.Lock;
  try
    P := pointer(List);
    for i := 1 to Count do
      if P^.ID = aItem.ID then
      begin
        if P^.Instance <> nil then
        begin
          result := true;
          aItem := P^;
        end;
        exit;
      end
      else
        inc(P);
  finally
    Safe.UnLock;
  end;
end;

function TRestClientCallbacks.FindAndRelease(aID: integer): boolean;
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
    List[i].ReleasedFromServer := True;
  finally
    Safe.UnLock;
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
  Safe.Lock;
  try
    for i := Count - 1 downto 0 do
      if List[i].Instance = aInstance then
        if UnRegisterByIndex(i) then
          result := true
        else
          break;
  finally
    Safe.UnLock;
  end;
end;

procedure TRestClientCallbacks.DoRegister(aID: integer; aInstance: pointer;
  aFactory: TInterfaceFactory);
begin
  if aID <= 0 then
    exit;
  Safe.Lock;
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
    Safe.UnLock;
  end;
end;

function TRestClientCallbacks.DoRegister(aInstance: pointer;
  aFactory: TInterfaceFactory): integer;
begin
  result := InterlockedIncrement(fCurrentID);
  DoRegister(result, aInstance, aFactory);
end;


{ TRemoteLogThread }

type
  TRemoteLogThread = class(TRestThread)
  protected
    fClient: TRestClientURI;
    fPendingRows: RawUTF8;
    procedure InternalExecute; override;
  public
    constructor Create(aClient: TRestClientURI); reintroduce;
    destructor Destroy; override;
    procedure AddRow(const aText: RawUTF8);
  end;

constructor TRemoteLogThread.Create(aClient: TRestClientURI);
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

procedure TRemoteLogThread.AddRow(const aText: RawUTF8);
begin
  fSafe.Lock;
  try
    AddToCSV(aText, fPendingRows, #13#10);
  finally
    fSafe.UnLock;
  end;
  fEvent.SetEvent;
end;

procedure TRemoteLogThread.InternalExecute;
var
  aText: RawUTF8;
begin
  while not Terminated do
    if fEvent.WaitFor(INFINITE) = wrSignaled then
    begin
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


{ TRestClientURI }

procedure TRestClientURI.SetRoutingClass(aServicesRouting: TRestClientRoutingClass);
begin
  if self <> nil then
    if aServicesRouting <> fServicesRouting then
      if (aServicesRouting = nil) or
         (aServicesRouting = TRestClientRouting) then
         raise EServiceException.CreateUTF8('Unexpected %.SetRoutingClass(%)',
           [self, aServicesRouting])
      else
         fServicesRouting := aServicesRouting;
end;

procedure TRestClientURI.SetSessionHeartbeatSeconds(timeout: integer);
begin
  if (timeout < 0) or
     (timeout = fSession.HeartbeatSeconds) then
    exit;
  fSession.HeartbeatSeconds := timeout;
  TimerEnable(SessionRenewEvent, timeout);
end;

function TRestClientURI.GetOnIdleBackgroundThreadActive: boolean;
begin
  result := (self <> nil) and Assigned(fOnIdle) and
            fBackgroundThread.OnIdleBackgroundThreadActive;
end;

procedure TRestClientURI.OnBackgroundProcess(
  Sender: TSynBackgroundThreadEvent; ProcessOpaqueParam: pointer);
var
  Call: ^TRestURIParams absolute ProcessOpaqueParam;
begin
  if Call = nil then
    exit;
  InternalURI(Call^);
  if ((Sender = nil) or OnIdleBackgroundThreadActive) and
     not (isDestroying in fInternalState) then
  begin
    if (Call^.OutStatus = HTTP_NOTIMPLEMENTED) and
       (isOpened in fInternalState) then
    begin
      InternalClose; // force recreate connection
      Exclude(fInternalState, isOpened);
      if (Sender = nil) or
         OnIdleBackgroundThreadActive then
        InternalURI(Call^); // try request again
    end;
    if Call^.OutStatus <> HTTP_NOTIMPLEMENTED then
      Include(fInternalState, isOpened);
  end;
end;

procedure TRestClientURI.SetLastException(E: Exception; ErrorCode: integer;
  Call: PRestURIParams);
begin
  fLastErrorCode := ErrorCode;
  if E = nil then
  begin
    fLastErrorException := nil;
    if StatusCodeIsSuccess(ErrorCode) then
      fLastErrorMessage := ''
    else
      fLastErrorMessage := StatusCodeToReason(ErrorCode);
  end
  else
  begin
    fLastErrorException := PPointer(E)^;
    fLastErrorMessage := ObjectToJSONDebug(E);
  end;
  if Assigned(fOnFailed) then
    fOnFailed(self, E, Call);
end;

function TRestClientURI.InternalRemoteLogSend(const aText: RawUTF8): boolean;
begin
  result := URI(fModel.GetURICallBack('RemoteLog', nil, 0), 'PUT', nil, nil, @aText).
    Lo in [HTTP_SUCCESS, HTTP_NOCONTENT];
end;

function TRestClientURI.{%H-}FakeCallbackRegister(Sender: TServiceFactory;
  const Method: TInterfaceMethod; const ParamInfo: TInterfaceMethodArgument;
  ParamValue: Pointer): integer;
begin
  raise EServiceException.CreateUTF8('% does not support interface parameters ' +
    'for %.%(%: %): consider using another kind of client',
    [self, Sender.InterfaceFactory.InterfaceName, Method.URI,
     ParamInfo.ParamName^, ParamInfo.ArgTypeName^]);
end;

function TRestClientURI.{%H-}FakeCallbackUnregister(
  Factory: TInterfaceFactory; FakeCallbackID: integer; Instance: pointer): boolean;
begin
  raise EServiceException.CreateUTF8(
    '% does not support % callbacks: consider using another kind of client',
    [self, Factory.InterfaceName]);
end;

constructor TRestClientURI.RegisteredClassCreateFrom(aModel: TOrmModel;
  aDefinition: TSynConnectionDefinition; aServerHandleAuthentication: boolean);
begin
  if fModel = nil then // if not already created with a reintroduced constructor
    Create(aModel);
  if fModel <> nil then
    fOnIdle := fModel.OnClientIdle; // allow UI interactivity during SetUser()
  if aDefinition.User <> '' then
  begin
    {$ifdef DOMAINAUTH}
    if aDefinition.User = SSPI_DEFINITION_USERNAME then
      SetUser('', aDefinition.PasswordPlain)
    else
    {$endif DOMAINAUTH}
      SetUser(aDefinition.User, aDefinition.PasswordPlain, true);
  end;
end;

function TRestClientURI.GetCurrentSessionUserID: TID;
begin
  if fSession.User = nil then
    result := 0
  else
    result := fSession.User.IDValue;
end;

function TRestClientURI.GetSessionVersion: RawUTF8;
var
  resp: RawUTF8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fSession.Version = '' then
      // no session (e.g. API public URI) -> ask
      if CallBackGet('timestamp/info', [], resp) = HTTP_SUCCESS then
        fSession.Version := JSONDecode(resp, 'version');
    result := fSession.Version;
  end;
end;

function TRestClientURI.SessionCreate(aAuth: TRestClientAuthenticationClass;
  var aUser: TAuthUser; const aSessionKey: RawUTF8): boolean;
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

procedure TRestClientURI.SessionRenewEvent(Sender: TSynBackgroundTimer;
  Event: TWaitResult; const Msg: RawUTF8);
var
  resp: RawUTF8;
  status: integer;
begin
  status := CallBack(mPOST, 'CacheFlush/_ping_', '', resp);
  InternalLog('SessionRenewEvent(%) received status=% count=% from % % (timeout=% min)',
    [fModel.Root, status, JSONDecode(resp, 'count'), fSession.Server,
     fSession.Version, fSession.ServerTimeout], sllUserAuth);
end;

constructor TRestClientURI.Create(aModel: TOrmModel);
begin
  inherited Create(aModel);
  fMaximumAuthentificationRetry := 1;
  fComputeSignature := TRestClientAuthenticationSignedURI.ComputeSignatureCrc32;
  fSession.ID := CONST_AUTHENTICATION_NOT_USED;
  fFakeCallbacks := TRestClientCallbacks.Create(self);
  {$ifdef USELOCKERDEBUG}
  fSafe := TAutoLockerDebug.Create(fLogClass, aModel.Root); // more verbose
  {$else}
  fSafe := TAutoLocker.Create;
  {$endif USELOCKERDEBUG}
end;

destructor TRestClientURI.Destroy;
var
  t, i: PtrInt;
  aID: TID;
  Table: TOrmClass;
begin
  include(fInternalState, isDestroying);
  if SynLogFileFreeing then // may be owned by a TSynLogFamily
    SetLogClass(nil);
  {$ifdef MSWINDOWS}
  fServiceNotificationMethodViaMessages.Wnd := 0; // disable notification
  {$endif MSWINDOWS}
  FreeAndNil(fFakeCallbacks);
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
    // release memory and associated classes
    if fRemoteLogClass <> nil then
    begin
      FreeAndNil(fRemoteLogThread);
      ServerRemoteLogStop;
    end;
    FreeAndNil(fSession.User);
    try
      inherited Destroy; // fModel.Free if owned by this TRest instance
      FreeAndNil(fBackgroundThread); // should be done after fServices.Free
      fOnIdle := nil;
    finally
      InternalClose;
    end;
  end;
end;

procedure TRestClientURI.SetOrmInstance(aORM: TInterfacedObject);
begin
  inherited SetOrmInstance(aORM); // set fOrm
  if not fOrmInstance.GetInterface(IRestOrmClient, fOrmClient) then
    raise ERestException.CreateUTF8('%.Create with invalid %', [self, fOrmInstance]);
  // enable redirection of URI() from IRestOrm/IRestOrmClient into this class
  (fOrmInstance as TRestOrmClientURI).URI := URI;
end;

procedure TRestClientURI.SessionClose;
var
  tmp: RawUTF8;
begin
  if (self <> nil) and
     (fSession.User <> nil) and
     (fSession.ID <> CONST_AUTHENTICATION_SESSION_NOT_STARTED) then
  try
    TimerDisable(SessionRenewEvent);
    InternalLog('SessionClose: notify server', sllTrace);
    CallBackGet('Auth', [
      'UserName', fSession.User.LogonName,
      'Session', fSession.ID], tmp);
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
    FreeAndNil(fSession.User);
    fComputeSignature := TRestClientAuthenticationSignedURI.ComputeSignatureCrc32;
  end;
end;

{$ifdef MSWINDOWS}

type
  TRestClientURIServiceNotification = class(TInterfaceMethodExecute)
  protected
    /// parameters set by TRestClientURI.InternalNotificationMethodExecute
    fOwner: TRestClientURI;
    fInstance: pointer; // weak IInvokable reference
    fPar: RawUTF8;
  end;

procedure TRestClientURI.ServiceNotificationMethodViaMessages(
  hWnd: HWND; Msg: cardinal);
begin
  if Msg = 0 then
    hWnd := 0; // avoid half defined parameters
  fServiceNotificationMethodViaMessages.Wnd := hWnd;
  fServiceNotificationMethodViaMessages.Msg := Msg;
end;

class procedure TRestClientURI.ServiceNotificationMethodExecute(
  var Msg: TMessage);
var
  exec: TRestClientURIServiceNotification;
begin
  exec := pointer(Msg.LParam);
  if exec <> nil then
  try
    try
      if exec.InheritsFrom(TRestClientURIServiceNotification) and
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

{$endif MSWINDOWS}

procedure TRestClientURI.InternalNotificationMethodExecute(
  var Ctxt: TRestURIParams);
var
  url, root, interfmethod, interf, id, method, frames: RawUTF8;
  callback: TRestClientCallbackItem;
  methodIndex: integer;
  WR: TTextWriter;
  temp: TTextWriterStackBuffer;
  ok: boolean;

  procedure Call(methodIndex: Integer; const par: RawUTF8; res: TTextWriter);
  var
    method: PInterfaceMethod;
    exec: TInterfaceMethodExecute;
    {$ifdef MSWINDOWS}
    execmsg: TRestClientURIServiceNotification absolute exec;
    {$endif MSWINDOWS}
  begin
    method := @callback.Factory.Methods[methodIndex];
    {$ifdef MSWINDOWS}
    if (fServiceNotificationMethodViaMessages.Wnd <> 0) and
       (method^.ArgsOutputValuesCount = 0) then
    begin
      // expects no result -> asynchronous non blocking notification in UI thread
      Ctxt.OutStatus := 0;
      execmsg := TRestClientURIServiceNotification.Create(method);
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
    {$endif MSWINDOWS}
      exec := TInterfaceMethodExecute.Create(method);
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
  // 'root/BidirCallback.AsynchEvent/1' into root/interfmethod/id
  Split(Split(url, '/', root), '/', interfmethod, id);
  if not IdemPropNameU(root, fModel.Root) then
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
    WR := TTextWriter.CreateOwnedStream(temp);
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
      Ctxt.OutBody := ObjectToJSONDebug(E);
      Ctxt.OutStatus := HTTP_SERVERERROR;
    end;
  end;
end;

function TRestClientURI.URI(const url, method: RawUTF8; Resp: PRawUTF8;
  Head: PRawUTF8; SendData: PRawUTF8): Int64Rec;
var
  retry: Integer;
  aUserName, aPassword: string;
  StatusMsg: RawUTF8;
  Call: TRestURIParams;
  aPasswordHashed: boolean;

  procedure CallInternalURI;
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
          OnIdle, FormatUTF8('% % background', [self, fModel.Root]));
      if not fBackgroundThread.RunAndWait(@Call) then
        Call.OutStatus := HTTP_UNAVAILABLE;
    end
    else
      OnBackgroundProcess({SenderThread=}nil, @Call);
    result.Lo := Call.OutStatus;
    result.Hi := Call.OutInternalState;
    if Head <> nil then
      Head^ := Call.OutHead;
    if Resp <> nil then
      Resp^ := Call.OutBody;
    fLastErrorCode := Call.OutStatus;
  end;

begin
  if self = nil then
  begin
    Int64(result) := HTTP_UNAVAILABLE;
    SetLastException(nil, HTTP_UNAVAILABLE);
    exit;
  end;
  fLastErrorMessage := '';
  fLastErrorException := nil;
  if fServerTimestamp.Offset = 0 then
  begin
    if not ServerTimestampSynchronize then
    begin
      Int64(result) := HTTP_UNAVAILABLE;
      exit; // if Timestamp is not available, server is down!
    end;
  end;
  Call.Init;
  if (Head <> nil) and
     (Head^ <> '') then
    Call.InHead := Head^;
  if fSession.HttpHeader <> '' then
    Call.InHead := Trim(Call.InHead + #13#10 + fSession.HttpHeader);
  try
    CallInternalURI;
    if (Call.OutStatus = HTTP_TIMEOUT) and
       RetryOnceOnTimeout then
    begin
      InternalLog('% % returned "408 Request Timeout" -> RETRY', [method, url], sllError);
      CallInternalURI;
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
           SetUser(StringToUTF8(aUserName), StringToUTF8(aPassword), aPasswordHashed) then
        begin
          CallInternalURI;
          break;
        end;
        Inc(retry);
      end;
    finally
      Exclude(fInternalState, isInAuth);
    end;
    if not StatusCodeIsSuccess(Call.OutStatus) then
    begin
      StatusMsg := StatusCodeToReason(Call.OutStatus);
      if Call.OutBody = '' then
        fLastErrorMessage := StatusMsg
      else
        fLastErrorMessage := Call.OutBody;
      InternalLog('% % returned % (%) with message  %',
        [method, url, Call.OutStatus, StatusMsg, fLastErrorMessage], sllError);
      if Assigned(fOnFailed) then
        fOnFailed(self, nil, @Call);
    end;
  except
    on E: Exception do
    begin
      Int64(result) := HTTP_NOTIMPLEMENTED; // 501
      SetLastException(E, HTTP_NOTIMPLEMENTED, @Call);
      exit;
    end;
  end;
end;

function TRestClientURI.CallBackGet(const aMethodName: RawUTF8;
  const aNameValueParameters: array of const; out aResponse: RawUTF8;
  aTable: TOrmClass; aID: TID; aResponseHead: PRawUTF8): integer;
var
  url, header: RawUTF8;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  if self = nil then
    result := HTTP_UNAVAILABLE
  else
  begin
    url := fModel.GetURICallBack(aMethodName, aTable, aID);
    if high(aNameValueParameters) > 0 then
      url := url + UrlEncode(aNameValueParameters);
    log := fLogClass.Enter('CallBackGet %', [url], self);
    result := URI(url, 'GET', @aResponse, @header).Lo;
    if aResponseHead <> nil then
      aResponseHead^ := header;
    if (log <> nil) and
       (aResponse <> '') and
       (sllServiceReturn in fLogFamily.Level) then
      if IsHTMLContentTypeTextual(pointer(header)) then
        log.Log(sllServiceReturn, aResponse, self, MAX_SIZE_RESPONSE_LOG)
      else
        log.Log(sllServiceReturn, '% bytes [%]', [length(aResponse), header], self);
  end;
end;

function TRestClientURI.CallBackGetResult(const aMethodName: RawUTF8;
  const aNameValueParameters: array of const; aTable: TOrmClass;
  aID: TID): RawUTF8;
var
  resp: RawUTF8;
begin
  if CallBackGet(aMethodName, aNameValueParameters, resp, aTable, aID) = HTTP_SUCCESS then
    result := JSONDecode(resp)
  else
    result := '';
end;

function TRestClientURI.CallBackPut(const aMethodName, aSentData: RawUTF8;
  out aResponse: RawUTF8; aTable: TOrmClass; aID: TID;
  aResponseHead: PRawUTF8): integer;
begin
  result := callback(mPUT, aMethodName, aSentData, aResponse, aTable, aID, aResponseHead);
end;

function TRestClientURI.CallBack(method: TURIMethod; const aMethodName,
  aSentData: RawUTF8; out aResponse: RawUTF8; aTable: TOrmClass;
  aID: TID; aResponseHead: PRawUTF8): integer;
var
  u, m: RawUTF8;
  log: ISynLog; // for Enter auto-leave to work with FPC
begin
  if (self = nil) or
     (method = mNone) then
    result := HTTP_UNAVAILABLE
  else
  begin
    u := fModel.GetURICallBack(aMethodName, aTable, aID);
    log := fLogClass.Enter('Callback %', [u], self);
    m := TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TURIMethod), ord(method)));
    result := URI(u, m, @aResponse, aResponseHead, @aSentData).Lo;
    InternalLog('% result=% resplen=%',
      [m, result, length(aResponse)], sllServiceReturn);
  end;
end;

procedure TRestClientURI.CallbackNonBlockingSetHeader(out Header: RawUTF8);
begin
  // nothing to do by default (plain REST/HTTP works in blocking mode)
end;

function TRestClientURI.ServiceRegister(const aInterfaces: array of PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8): boolean;
begin
  result := False;
  if (self = nil) or
     (high(aInterfaces) < 0) then
    exit;
  result := (ServiceContainer as TServiceContainerClient).AddInterface(
    aInterfaces, aInstanceCreation, aContractExpected);
end;

function TRestClientURI.ServiceRegister(aInterface: PRttiInfo;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8; aIgnoreAnyException: boolean): TServiceFactory;
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

function TRestClientURI.ServiceRegisterClientDriven(aInterface: PRttiInfo;
  out Obj; const aContractExpected: RawUTF8): boolean;
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

function TRestClientURI.ServiceDefine(const aInterfaces: array of TGUID;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8): boolean;
begin
  if self <> nil then
    result := ServiceRegister(TInterfaceFactory.GUID2TypeInfo(aInterfaces),
      aInstanceCreation, aContractExpected)
  else
    result := false;
end;

function TRestClientURI.ServiceDefine(const aInterface: TGUID;
  aInstanceCreation: TServiceInstanceImplementation;
  const aContractExpected: RawUTF8; aIgnoreAnyException: boolean): TServiceFactoryClient;
begin
  result := TServiceFactoryClient(
    ServiceRegister(TInterfaceFactory.GUID2TypeInfo(aInterface),
     aInstanceCreation, aContractExpected, aIgnoreAnyException));
end;

function TRestClientURI.ServiceDefineClientDriven(const aInterface: TGUID;
  out Obj; const aContractExpected: RawUTF8): boolean;
begin
  result := ServiceRegisterClientDriven(
    TInterfaceFactory.GUID2TypeInfo(aInterface), Obj, aContractExpected);
end;

function TRestClientURI.ServiceDefineSharedAPI(const aInterface: TGUID;
  const aContractExpected: RawUTF8; aIgnoreAnyException: boolean): TServiceFactoryClient;
begin
  try
    result := ServiceDefine(
      aInterface, sicShared, aContractExpected, aIgnoreAnyException);
    if result <> nil then
    begin
      result.ParamsAsJSONObject := true; // no contract -> explicit parameters
      result.ResultAsJSONObjectWithoutResult := true;
    end;
  except
    if aIgnoreAnyException then
      result := nil
    else
      raise;
  end;
end;

function TRestClientURI.ServerTimestampSynchronize: boolean;
var
  status: integer;
  resp: RawUTF8;
begin
  if self = nil then
  begin
    result := false;
    exit;
  end;
  fServerTimestamp.Offset := 0.0001; // avoid endless recursive call
  status := CallBackGet('Timestamp', [], resp);
  result := (status = HTTP_SUCCESS) and
            (resp <> '');
  if result then
    SetServerTimestamp(GetInt64(pointer(resp)))
  else
  begin
    InternalLog('/Timestamp call failed -> Server not available', sllWarning);
    fLastErrorMessage := 'Server not available  - ' + Trim(fLastErrorMessage);
  end;
end;

function TRestClientURI.ServerRemoteLog(Sender: TBaseWriter;
  Level: TSynLogInfo; const Text: RawUTF8): boolean;
begin
  if fRemoteLogThread = nil then
    result := InternalRemoteLogSend(Text)
  else
  begin
    TRemoteLogThread(fRemoteLogThread).AddRow(Text);
    result := true;
  end;
end;

function TRestClientURI.ServerRemoteLog(Level: TSynLogInfo;
  const FormatMsg: RawUTF8; const Args: array of const): boolean;
begin
  result := ServerRemoteLog(nil, Level, FormatUTF8('%00%    %',
    [NowToString(false), LOG_LEVEL_TEXT[Level], FormatUTF8(FormatMsg, Args)]));
end;

procedure TRestClientURI.ServerRemoteLogStart(aLogClass: TSynLogClass;
  aClientOwnedByFamily: boolean);
begin
  if (fRemoteLogClass <> nil) or
     (aLogClass = nil) then
    exit;
  SetLogClass(TSynLog.Void); // this client won't log anything
  if not ServerRemoteLog(sllClient, 'Remote Client % Connected', [self]) then
    // first test server without threading
    raise ERestException.CreateUTF8('%.ServerRemoteLogStart: Connection ' +
      'to RemoteLog server impossible'#13#10'%', [LastErrorMessage]);
  if fRemoteLogThread <> nil then
    raise ERestException.CreateUTF8('%.ServerRemoteLogStart twice', [self]);
  fRemoteLogThread := TRemoteLogThread.Create(self);
  fRemoteLogClass := aLogClass.Add;
  aLogClass.Family.EchoRemoteStart(self, ServerRemoteLog, aClientOwnedByFamily);
  fRemoteLogOwnedByFamily := aClientOwnedByFamily;
end;

procedure TRestClientURI.ServerRemoteLogStop;
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

const
  {$ifdef GSSAPIAUTH}
  SSPI_USER_CHAR = '@';
  {$else}
  SSPI_USER_CHAR = '\';
  {$endif GSSAPIAUTH}

function TRestClientURI.SetUser(const aUserName, aPassword: RawUTF8;
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
  {$ifdef DOMAINAUTH}
  // try Windows/GSSAPI authentication with the current logged user
  result := true;
  if (IsVoid(aUserName) or
      (PosExChar(SSPI_USER_CHAR, aUserName) > 0)) and
     TRestClientAuthenticationSSPI.ClientSetUser(
       self, aUserName, aPassword, passKerberosSPN) then
    exit;
  {$endif DOMAINAUTH}
  result := TRestClientAuthentication.ClientSetUser(self, aUserName,
    aPassword, HASH[aHashedPassword]);
end;

{$ifndef PUREMORMOT2}

function TRestClientURI.Refresh(aID: TID; Value: TOrm;
  var Refreshed: boolean): boolean;
begin
  result := fOrmClient.Refresh(aID, Value, Refreshed);
end;

function TRestClientURI.List(const Tables: array of TOrmClass;
  const SQLSelect: RawUTF8; const SQLWhere: RawUTF8): TOrmTable;
begin
  result := fOrmClient.List(Tables, SQLSelect, SQLWhere);
end;

function TRestClientURI.ListFmt(const Tables: array of TOrmClass;
  const SQLSelect, SQLWhereFormat: RawUTF8; const Args: array of const): TOrmTable;
begin
  result := fOrmClient.ListFmt(Tables, SQLSelect, SQLWhereFormat, Args);
end;

function TRestClientURI.ListFmt(const Tables: array of TOrmClass;
  const SQLSelect, SQLWhereFormat: RawUTF8; const Args, Bounds: array of const): TOrmTable;
begin
  result := fOrmClient.ListFmt(Tables, SQLSelect, SQLWhereFormat, Args, Bounds);
end;

function TRestClientURI.TransactionBeginRetry(aTable: TOrmClass;
  Retries: integer): boolean;
begin
  result := fOrmClient.TransactionBeginRetry(aTable, Retries);
end;

function TRestClientURI.BatchStart(aTable: TOrmClass;
  AutomaticTransactionPerRow: cardinal; Options: TRestBatchOptions): boolean;
begin
  result := fOrmClient.BatchStart(aTable, AutomaticTransactionPerRow, Options);
end;

function TRestClientURI.BatchStartAny(AutomaticTransactionPerRow: cardinal;
  Options: TRestBatchOptions): boolean;
begin
  result := fOrmClient.BatchStartAny(AutomaticTransactionPerRow, Options);
end;

function TRestClientURI.BatchAdd(Value: TOrm; SendData: boolean;
  ForceID: boolean; const CustomFields: TFieldBits): integer;
begin
  result := fOrmClient.BatchAdd(Value, SendData, ForceID, CustomFields);
end;

function TRestClientURI.BatchUpdate(Value: TOrm;
  const CustomFields: TFieldBits; DoNotAutoComputeFields: boolean): integer;
begin
  result := fOrmClient.BatchUpdate(Value, CustomFields, DoNotAutoComputeFields);
end;

function TRestClientURI.BatchDelete(ID: TID): integer;
begin
  result := fOrmClient.BatchDelete(ID);
end;

function TRestClientURI.BatchDelete(Table: TOrmClass; ID: TID): integer;
begin
  result := fOrmClient.BatchDelete(Table, ID);
end;

function TRestClientURI.BatchCount: integer;
begin
  result := fOrmClient.BatchCount;
end;

function TRestClientURI.BatchSend(var Results: TIDDynArray): integer;
begin
  result := fOrmClient.BatchSend(Results);
end;

procedure TRestClientURI.BatchAbort;
begin
  fOrmClient.BatchAbort;
end;

{$endif PUREMORMOT2}


end.

