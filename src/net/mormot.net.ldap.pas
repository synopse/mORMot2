/// Simple Network LDAP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ldap;

{
  *****************************************************************************

   Simple LDAP Protocol Client
    - Basic ASN.1 Support
    - LDAP Protocol Definitions
    - LDAP Response Storage
    - LDAP Client Class

  *****************************************************************************
  Code below was inspired by Synapse Library code:
   The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
   Portions created by Lukas Gebauer are (c)2003-2014. All Rights Reserved.
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.data,
  mormot.lib.sspi, // do-nothing units on non compliant OS
  mormot.lib.gssapi,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.dns;


{ **************** Basic ASN.1 Support }

type
  /// we defined our own type to hold an ASN object binary
  TAsnObject = RawByteString;

const
  // base types
  ASN1_BOOL        = $01;
  ASN1_INT         = $02;
  ASN1_BITSTR      = $03;
  ASN1_OCTSTR      = $04;
  ASN1_NULL        = $05;
  ASN1_OBJID       = $06;
  ASN1_ENUM        = $0a;
  ASN1_UTF8STRING  = $0c;
  ASN1_SEQ         = $30;
  ASN1_SETOF       = $31;
  ASN1_IPADDR      = $40;
  ASN1_COUNTER     = $41;
  ASN1_GAUGE       = $42;
  ASN1_TIMETICKS   = $43;
  ASN1_OPAQUE      = $44;
  ASN1_COUNTER64   = $46;

  // class type masks
  ASN1_CL_APP   = $40;
  ASN1_CL_CTX   = $80;
  ASN1_CL_PRI   = $c0;

  //  context-specific class, tag #n
  ASN1_CTX0  = $80;
  ASN1_CTX1  = $81;
  ASN1_CTX2  = $82;
  ASN1_CTX3  = $83;
  ASN1_CTX4  = $84;
  ASN1_CTX5  = $85;
  ASN1_CTX6  = $86;
  ASN1_CTX7  = $87;
  ASN1_CTX8  = $88;
  ASN1_CTX9  = $89;

  //  context-specific class, constructed, tag #n
  ASN1_CTC0  = $a0;
  ASN1_CTC1  = $a1;
  ASN1_CTC2  = $a2;
  ASN1_CTC3  = $a3;
  ASN1_CTC4  = $a4;
  ASN1_CTC5  = $a5;
  ASN1_CTC6  = $a6;
  ASN1_CTC7  = $a7;
  ASN1_CTC8  = $a8;
  ASN1_CTC9  = $a9;

  ASN1_BOOLEAN: array[boolean] of byte = (
    $00,
    $ff);

/// encode a 64-bit signed integer value into ASN.1 binary
function AsnEncInt(Value: Int64): TAsnObject;

/// encode a 64-bit unsigned OID integer value into ASN.1 binary
function AsnEncOidItem(Value: Int64): TAsnObject;

/// encode the len of a ASN.1 binary item
function AsnEncLen(Len: cardinal; dest: PByte): PtrInt;

/// create an ASN.1 binary from the aggregation of several binaries
function Asn(AsnType: integer;
  const Content: array of TAsnObject): TAsnObject; overload;

/// create an ASN.1 binary from some raw data - as OCTSTR by default
function Asn(const Data: RawByteString; AsnType: integer = ASN1_OCTSTR): TAsnObject;
  overload; {$ifdef HASINLINE} inline; {$endif}

/// create an ASN.1 binary from 64-bit signed integer, calling AsnEncInt()
function Asn(Value: Int64; AsnType: integer = ASN1_INT): TAsnObject; overload;

/// create an ASN.1 binary from a boolean value
function Asn(Value: boolean): TAsnObject; overload;

/// create an ASN.1 SEQuence from some raw data
function AsnSeq(const Data: TAsnObject): TAsnObject;

/// raw append some binary to an ASN.1 object buffer
procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject);
  overload; {$ifdef HASINLINE} inline; {$endif}

/// encode and append some raw data as ASN.1
procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject;
  AsnType: integer); overload;

/// decode the len of a ASN.1 binary item
function AsnDecLen(var Start: integer; const Buffer: TAsnObject): cardinal;

/// decode an OID ASN.1 value into human-readable text
function OidToText(Pos, EndPos: integer; const Buffer: RawByteString): RawUtf8;

/// parse the next ASN.1 value as text
function AsnNext(var Pos: integer; const Buffer: TAsnObject;
  out ValueType: integer): RawByteString;

/// convert a Distinguished Name to a Canonical Name
// - raise an exception if the supplied DN is not a valid Distinguished Name
// - e.g. DNToCN('CN=User1,OU=Users,OU=London,DC=xyz,DC=local') =
// 'xyz.local/London/Users/User1'
function DNToCN(const DN: RawUtf8): RawUtf8;



{ **************** LDAP Protocol Definitions }

const
  // LDAP result codes
  LDAP_RES_SUCCESS                        = 0;
  LDAP_RES_OPERATIONS_ERROR               = 1;
  LDAP_RES_PROTOCOL_ERROR                 = 2;
  LDAP_RES_TIME_LIMIT_EXCEEDED            = 3;
  LDAP_RES_SIZE_LIMIT_EXCEEDED            = 4;
  LDAP_RES_COMPARE_FALSE                  = 5;
  LDAP_RES_COMPARE_TRUE                   = 6;
  LDAP_RES_AUTH_METHOD_NOT_SUPPORTED      = 7;
  LDAP_RES_STRONGER_AUTH_REQUIRED         = 8;
  LDAP_RES_REFERRAL                       = 10;
  LDAP_RES_ADMIN_LIMIT_EXCEEDED           = 11;
  LDAP_RES_UNAVAILABLE_CRITICAL_EXTENSION = 12;
  LDAP_RES_CONFIDENTIALITY_REQUIRED       = 13;
  LDAP_RES_SASL_BIND_IN_PROGRESS          = 14;
  LDAP_RES_NO_SUCH_ATTRIBUTE              = 16;
  LDAP_RES_UNDEFINED_ATTRIBUTE_TYPE       = 17;
  LDAP_RES_INAPPROPRIATE_MATCHING         = 18;
  LDAP_RES_CONSTRAINT_VIOLATION           = 19;
  LDAP_RES_ATTRIBUTE_OR_VALUE_EXISTS      = 20;
  LDAP_RES_INVALID_ATTRIBUTE_SYNTAX       = 21;
  LDAP_RES_NO_SUCH_OBJECT                 = 32;
  LDAP_RES_ALIAS_PROBLEM                  = 33;
  LDAP_RES_INVALID_DN_SYNTAX              = 34;
  LDAP_RES_IS_LEAF                        = 35;
  LDAP_RES_ALIAS_DEREFERENCING_PROBLEM    = 36;
  LDAP_RES_INAPPROPRIATE_AUTHENTICATION   = 48;
  LDAP_RES_INVALID_CREDENTIALS            = 49;
  LDAP_RES_INSUFFICIENT_ACCESS_RIGHTS     = 50;
  LDAP_RES_BUSY                           = 51;
  LDAP_RES_UNAVAILABLE                    = 52;
  LDAP_RES_UNWILLING_TO_PERFORM           = 53;
  LDAP_RES_LOOP_DETECT                    = 54;
  LDAP_RES_SORT_CONTROL_MISSING           = 60;
  LDAP_RES_OFFSET_RANGE_ERROR             = 61;
  LDAP_RES_NAMING_VIOLATION               = 64;
  LDAP_RES_OBJECT_CLASS_VIOLATION         = 65;
  LDAP_RES_NOT_ALLOWED_ON_NON_LEAF        = 66;
  LDAP_RES_NOT_ALLOWED_ON_RDN             = 67;
  LDAP_RES_ENTRY_ALREADY_EXISTS           = 68;
  LDAP_RES_OBJECT_CLASS_MODS_PROHIBITED   = 69;
  LDAP_RES_RESULTS_TOO_LARGE              = 70;
  LDAP_RES_AFFECTS_MULTIPLE_DSAS          = 71;
  LDAP_RES_CONTROL_ERROR                  = 76;
  LDAP_RES_OTHER                          = 80;
  LDAP_RES_SERVER_DOWN                    = 81;
  LDAP_RES_LOCAL_ERROR                    = 82;
  LDAP_RES_ENCODING_ERROR                 = 83;
  LDAP_RES_DECODING_ERROR                 = 84;
  LDAP_RES_TIMEOUT                        = 85;
  LDAP_RES_AUTH_UNKNOWN                   = 86;
  LDAP_RES_FILTER_ERROR                   = 87;
  LDAP_RES_USER_CANCELED                  = 88;
  LDAP_RES_PARAM_ERROR                    = 89;
  LDAP_RES_NO_MEMORY                      = 90;
  LDAP_RES_CONNECT_ERROR                  = 91;
  LDAP_RES_NOT_SUPPORTED                  = 92;
  LDAP_RES_CONTROL_NOT_FOUND              = 93;
  LDAP_RES_NO_RESULTS_RETURNED            = 94;
  LDAP_RES_MORE_RESULTS_TO_RETURN         = 95;
  LDAP_RES_CLIENT_LOOP                    = 96;
  LDAP_RES_REFERRAL_LIMIT_EXCEEDED        = 97;
  LDAP_RES_INVALID_RESPONSE               = 100;
  LDAP_RES_AMBIGUOUS_RESPONSE             = 101;
  LDAP_RES_TLS_NOT_SUPPORTED              = 112;
  LDAP_RES_INTERMEDIATE_RESPONSE          = 113;
  LDAP_RES_UNKNOWN_TYPE                   = 114;
  LDAP_RES_CANCELED                       = 118;
  LDAP_RES_NO_SUCH_OPERATION              = 119;
  LDAP_RES_TOO_LATE                       = 120;
  LDAP_RES_CANNOT_CANCEL                  = 121;
  LDAP_RES_ASSERTION_FAILED               = 122;
  LDAP_RES_AUTHORIZATION_DENIED           = 123;
  LDAP_RES_ESYNC_REFRESH_REQUIRED         = 4096;
  LDAP_RES_NO_OPERATION                   = 16654;

const
  // Well-Known LDAP Objects GUID
  GUID_COMPUTERS_CONTAINER_W                 = 'AA312825768811D1ADED00C04FD8D5CD';
  GUID_DELETED_OBJECTS_CONTAINER_W           = '18E2EA80684F11D2B9AA00C04F79F805';
  GUID_DOMAIN_CONTROLLERS_CONTAINER_W        = 'A361B2FFFFD211D1AA4B00C04FD7D83A';
  GUID_FOREIGNSECURITYPRINCIPALS_CONTAINER_W = '22B70C67D56E4EFB91E9300FCA3DC1AA';
  GUID_INFRASTRUCTURE_CONTAINER_W            = '2FBAC1870ADE11D297C400C04FD8D5CD';
  GUID_LOSTANDFOUND_CONTAINER_W              = 'AB8153B7768811D1ADED00C04FD8D5CD';
  GUID_MICROSOFT_PROGRAM_DATA_CONTAINER_W    = 'F4BE92A4C777485E878E9421D53087DB';
  GUID_NTDS_QUOTAS_CONTAINER_W               = '6227F0AF1FC2410D8E3BB10615BB5B0F';
  GUID_PROGRAM_DATA_CONTAINER_W              = '09460C08AE1E4A4EA0F64AEE7DAA1E5A';
  GUID_SYSTEMS_CONTAINER_W                   = 'AB1D30F3768811D1ADED00C04FD8D5CD';
  GUID_USERS_CONTAINER_W                     = 'A9D1CA15768811D1ADED00C04FD8D5CD';
  GUID_MANAGED_SERVICE_ACCOUNTS_CONTAINER_W  = '1EB93889E40C45DF9F0C64D23BBB6237';

const
  // LDAP ASN.1 types
  LDAP_ASN1_BIND_REQUEST      = $60;
  LDAP_ASN1_BIND_RESPONSE     = $61;
  LDAP_ASN1_UNBIND_REQUEST    = $42;
  LDAP_ASN1_SEARCH_REQUEST    = $63;
  LDAP_ASN1_SEARCH_ENTRY      = $64;
  LDAP_ASN1_SEARCH_DONE       = $65;
  LDAP_ASN1_SEARCH_REFERENCE  = $73;
  LDAP_ASN1_MODIFY_REQUEST    = $66;
  LDAP_ASN1_MODIFY_RESPONSE   = $67;
  LDAP_ASN1_ADD_REQUEST       = $68;
  LDAP_ASN1_ADD_RESPONSE      = $69;
  LDAP_ASN1_DEL_REQUEST       = $4a;
  LDAP_ASN1_DEL_RESPONSE      = $6b;
  LDAP_ASN1_MODIFYDN_REQUEST  = $6c;
  LDAP_ASN1_MODIFYDN_RESPONSE = $6d;
  LDAP_ASN1_COMPARE_REQUEST   = $6e;
  LDAP_ASN1_COMPARE_RESPONSE  = $6f;
  LDAP_ASN1_ABANDON_REQUEST   = $70;
  LDAP_ASN1_EXT_REQUEST       = $77;
  LDAP_ASN1_EXT_RESPONSE      = $78;
  LDAP_ASN1_CONTROLS          = $a0;


{ **************** LDAP Response Storage }

type
  /// store a named LDAP attribute with the list of its values
  TLdapAttribute = class
  private
    fList: TRawUtf8DynArray;
    fAttributeName: RawUtf8;
    fCount: integer;
    fIsBinary: boolean;
  public
    /// initialize the attribute(s) storage
    constructor Create(const AttrName: RawUtf8);
    /// include a new value to this list
    // - IsBinary values will be stored base-64 encoded
    procedure Add(const aValue: RawByteString);
    /// retrieve a value as human-readable text
    function GetReadable(index: PtrInt = 0): RawUtf8;
    /// retrieve a value as its inital value stored with Add()
    function GetRaw(index: PtrInt = 0): RawByteString;
    /// how many values have been added to this attribute
    property Count: integer
      read fCount;
    /// name of this LDAP attribute
    property AttributeName: RawUtf8
      read fAttributeName;
    /// true if the attribute contains binary data
    property IsBinary: boolean
      read fIsBinary;
  end;
  /// dynamic array of LDAP attribute, as stored in TLdapAttributeList
  TLdapAttributeDynArray = array of TLdapAttribute;

  /// list one or several TLdapAttribute
  TLdapAttributeList = class
  private
    fItems: TLdapAttributeDynArray;
  public
    /// finalize the list
    destructor Destroy; override;
    /// clear the list
    procedure Clear;
    /// number of TLdapAttribute objects in this list
    function Count: integer;
      {$ifdef HASINLINE} inline; {$endif}
    /// allocate and a new TLdapAttribute object and its value to the list
    function Add(const AttributeName: RawUtf8;
      const AttributeValue: RawByteString): TLdapAttribute; overload;
    /// allocate and a new TLdapAttribute object to the list
    function Add(const AttributeName: RawUtf8): TLdapAttribute; overload;
    /// remove one TLdapAttribute object from the list
    procedure Delete(const AttributeName: RawUtf8);
    /// find and return attribute index with the requested name
    // - returns -1 if not found
    function FindIndex(const AttributeName: RawUtf8): PtrInt;
    /// find and return attribute with the requested name
    // - returns nil if not found
    function Find(const AttributeName: RawUtf8): TLdapAttribute;
    /// Find and return first attribute value with requested name
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    function Get(const AttributeName: RawUtf8): RawUtf8;
    /// access to the internal list of TLdapAttribute objects
    property Items: TLdapAttributeDynArray
      read fItems;
  end;

  /// store one LDAP result, i.e. object name and attributes
  TLdapResult = class
  private
    fObjectName: RawUtf8;
    fAttributes: TLdapAttributeList;
  public
    /// initialize the instance
    constructor Create; reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// Name of this LDAP object
    property ObjectName: RawUtf8
      read fObjectName write fObjectName;
    /// Here is list of object attributes
    property Attributes: TLdapAttributeList read fAttributes;
    /// Copy the 'objectSid' attribute if present
    // - Return true on success
    function CopyObjectSid(out objectSid: RawUtf8): boolean;
    /// Copy the 'objectGUID' attribute if present
    // - Return true on success
    function CopyObjectGUID(out objectGUID: TGuid): boolean;
  end;
  TLdapResultObjArray = array of TLdapResult;

  /// maintain a list of LDAP result objects
  TLdapResultList = class(TObject)
  private
    fItems: TLdapResultObjArray;
    fCount: integer;
  public
    /// finalize the list
    destructor Destroy; override;
    /// create and add new TLdapResult object to the list
    function Add: TLdapResult;
    /// ensure Count = length(fItems) to allow proper "for res in Items do"
    // - is called e.g. by TLdapClient.Search after all its Add()
    procedure AfterAdd;
    /// clear all TLdapResult objects in list
    procedure Clear;
    /// return all Items[].ObjectName as a sorted array
    function ObjectNames(asCN: boolean = false): TRawUtf8DynArray;
    /// dump the result of a LDAP search into human readable form
    // - used for debugging
    function Dump: RawUtf8;
    /// access to the TLdapResult objects
    // - you can write e.g. "for res in Items do writeln(res.ObjectName)"
    property Items: TLdapResultObjArray
      read fItems;
    /// number of TLdapResult objects in list
    property Count: integer
      read fCount;
  end;



{ **************** LDAP Client Class }

type
  /// define possible operations for LDAP MODIFY operations
  TLdapModifyOp = (
    lmoAdd,
    lmoDelete,
    lmoReplace
  );

  /// define possible values for LDAP search scope
  TLdapSearchScope = (
    lssBaseObject,
    lssSingleLevel,
    lssWholeSubtree
  );

  /// define possible values about LDAP alias dereferencing
  TLdapSearchAliases = (
    lsaNeverDeref,
    lsaInSearching,
    lsaFindingBaseObj,
    lsaAlways
  );

  /// the resultset of TLdapClient.GetWellKnownObjects()
  TLdapKnownCommonNames = record
    Computers: RawUtf8;
    DeletedObjects: RawUtf8;
    DomainControllers: RawUtf8;
    ForeignSecurityPrincipals: RawUtf8;
    Infrastructure: RawUtf8;
    LostAndFound: RawUtf8;
    MicrosoftProgramData: RawUtf8;
    NtdsQuotas: RawUtf8;
    ProgramData: RawUtf8;
    Systems: RawUtf8;
    Users: RawUtf8;
    ManagedServiceAccounts: RawUtf8;
  end;

  /// store the authentication and connection settings of a TLdapClient instance
  TLdapClientSettings = class(TSynPersistent)
  protected
    fTargetHost: RawUtf8;
    fTargetPort: RawUtf8;
    fUserName: RawUtf8;
    fPassword: SpiUtf8;
    fKerberosDN: RawUtf8;
    fKerberosSpn: RawUtf8;
    fTimeout: integer;
    fTls: boolean;
  public
    /// initialize this instance
    constructor Create; override;
    /// finalize this instance
    destructor Destroy; override;
    /// try to setup the LDAP server information from the system
    // - use a temporary TLdapClient.Login then optionally call BindSaslKerberos
    // - any existing KerberosDN and KerberosSpn will be used during discovery
    function LoadDefaultFromSystem(TryKerberos: boolean): boolean;
  published
    /// target server IP (or symbolic name)
    // - default is '' but if not set, Login will call DnsLdapControlers() from
    // mormot.net.dns to retrieve the current value from the system
    // - after connect, will contain the actual server name
    // - typical value is 'dc-one.mycorp.com'
    property TargetHost: RawUtf8
      read fTargetHost Write fTargetHost;
    /// target server port (or symbolic name)
    // - is '389' by default but could be '636' (or '3269') on TLS
    property TargetPort: RawUtf8
      read fTargetPort Write fTargetPort;
    /// milliseconds timeout for socket operations
    // - default is 5000, ie. 5 seconds
    property Timeout: integer
      read fTimeout Write fTimeout;
    /// if connection to the LDAP server is secured via TLS
    property Tls: boolean
      read fTls Write fTls;
    /// if protocol needs user authorization, then fill here user name
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    property UserName: RawUtf8
      read fUserName Write fUserName;
    /// if protocol needs user authorization, then fill here its password
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    property Password: SpiUtf8
      read fPassword Write fPassword;
    /// Kerberos Canonical Domain Name
    // - as set by Login when TargetHost is empty
    // - can be pre-set before Login if the system is not part of the domain
    // - used by BindSaslKerberos to compute the SPN
    // - typical value is 'ad.mycorp.com'
    property KerberosDN: RawUtf8
      read fKerberosDN write fKerberosDN;
    /// Kerberos Canonical Domain Name
    // - as used or computed by BindSaslKerberos for its SPN
    // - can be pre-set before BindSaslKerberos if the SPN is not of a standard
    // 'LDAP/<targethost>@<DOMAIN>' form
    // - typical value is e.g. 'LDAP/dc-one.mycorp.com@AD.MYCORP.COM'
    property KerberosSpn: RawUtf8
      read fKerberosSpn write fKerberosSpn;
  end;

  /// implementation of LDAP client version 2 and 3
  // - will default setup a TLS connection on the OS-designed LDAP server
  // - Authentication will use Username/Password properties
  TLdapClient = class(TSynPersistent)
  protected
    fSettings: TLdapClientSettings;
    fSock: TCrtSocket;
    fSeq: integer;
    fResultCode: integer;
    fResultString: RawUtf8;
    fFullResult: TAsnObject;
    fTlsContext: TNetTlsContext;
    fResponseCode: integer;
    fResponseDN: RawUtf8;
    fReferals: TRawUtf8List;
    fVersion: integer;
    fBound: boolean;
    fSearchScope: TLdapSearchScope;
    fSearchAliases: TLdapSearchAliases;
    fSearchSizeLimit: integer;
    fSearchTimeLimit: integer;
    fSearchPageSize: integer;
    fSearchCookie: RawUtf8;
    fSearchResult: TLdapResultList;
    fExtName: RawUtf8;
    fExtValue: RawUtf8;
    fRootDN: RawUtf8;
    function BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
    function GetNetbiosDomainName: RawUtf8;
    function GetRootDN: RawUtf8;
    procedure SendPacket(const Asn1Data: TAsnObject);
    function ReceiveResponse: TAsnObject;
    function DecodeResponse(const Asn1Response: TAsnObject): TAsnObject;
    function SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
    function TranslateFilter(const Filter: RawUtf8): TAsnObject;
    function ReceiveString(Size: integer): RawByteString;
  public
    /// initialize this LDAP client instance
    constructor Create; overload; override;
    /// initialize this LDAP client instance with the given settings
    // - which may be persisted as JSON e.g. into a TSynAutoCreateFields holder
    constructor Create(aSettings: TLdapClientSettings); reintroduce; overload;
    /// finalize this LDAP client instance
    destructor Destroy; override;
    /// try to connect to LDAP server
    // - if no TargetHost/TargetPort/FullTls has been set, will try the OS
    // DnsLdapControlers() hosts (from mormot.net.dns) over TLS if possible
    // - do nothing if was already connected
    function Login: boolean;
    /// authenticate a client to the directory server with Username/Password
    // - if these are empty strings, then it does annonymous binding
    // - warning: uses plaintext transport of password - consider using TLS
    function Bind: boolean;
    /// authenticate a client to the directory server with Username/Password
    // - uses DIGEST-MD5 as password obfuscation challenge - consider using TLS
    // - you can specify a stronger algorithm if MD5 is not strong enough
    // - seems not implemented by OpenLdap
    function BindSaslDigest(Algo: THashAlgo = hfMD5): boolean;
    /// authenticate a client to the directory server using Kerberos
    // - if no UserName/Password has been set, will try current logged user
    // - uses GSSAPI and mormot.lib.gssapi/sspi to perform a safe authentication
    // - if no SPN is supplied, derivate one from Login's DnsLdapControlers()
    // - can optionally return the KerberosUser which made the authentication
    function BindSaslKerberos(const AuthIdentify: RawUtf8 = '';
      KerberosUser: PRawUtf8 = nil): boolean;
    /// close connection to the LDAP server
    function Logout: boolean;
    /// retrieve all entries that match a given set of criteria
    // - will generate as many requests/responses as needed to retrieve all
    // the information into the SearchResult property
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
    /// retrieve all entries that match a given set of criteria and return the
    // first result
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if no result is found or if the search failed
    function SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
      const Attributes: array of RawByteString): TLdapResult;
    /// retrieve the entry matching the given ObjectDN
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN: RawUtf8;
      const Attributes: array of RawByteString): TLdapResult;
    /// create a new entry in the directory
    function Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
    /// Add a new computer in the domain
    // - If password is empty, it isn't set in the attributes
    // - If DeleteIfPresent is false and there is already a computer with this
    // name in the domain, the operation fail
    // - ErrorMessage contains the failure reason (if the operation failed)
    // - Return false if the operation failed
    function AddComputer(const ComputerParentDN, ComputerName: RawUtf8;
      out ErrorMessage: RawUtf8; const Password: SpiUtf8 = '';
      DeleteIfPresent : boolean = false): boolean;
    /// make one or more changes to the set of attribute values in an entry
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      Value: TLdapAttribute): boolean;
    /// change an entry DN
    // - it can be used to rename the entry (by changing its RDN), move it to a
    // different location in the DIT (by specifying a new parent entry), or both
    function ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    ///  remove an entry from the directory server
    function Delete(const Obj: RawUtf8): boolean;
    /// determine whether a given entry has a specified attribute value
    function Compare(const Obj, AttributeValue: RawUtf8): boolean;
    /// call any LDAP v3 extended operations
    // - e.g. StartTLS, cancel, transactions
    function Extended(const Oid, Value: RawUtf8): boolean;
    /// try to discover the root DN of the AD
    // - Return an empty string if not found
    function DiscoverRootDN: RawUtf8;
    /// test whether the client is connected to the server
    // - if AndBound is set, it also checks that a successfull bind request has been made
    function Connected(AndBound: boolean = true): boolean;
    /// retrieve a well known object DN or CN from its GUID
    // - see GUID_*_W constants, e.g. GUID_COMPUTERS_CONTAINER_W
    // - search in objects identified by the RootDN property
    // - return an empty string if not found
    function GetWellKnownObject(const ObjectGUID: RawUtf8;
      AsCN: boolean = false): RawUtf8;
    /// retrieve al well known object DN or CN as a single convenient record
    function GetWellKnownObjects(AsCN: boolean = true): TLdapKnownCommonNames;
    /// Translate a result code into its string explanation
    class function GetErrorString(ErrorCode: integer): RawUtf8;
    /// binary string of the last full response from LDAP server
    // - This string is encoded by ASN.1 BER encoding
    // - You need this only for debugging
    property FullResult: TAsnObject
      read fFullResult;
    /// optional advanced options for FullTls = true
    property TlsContext: TNetTlsContext
      read fTlsContext write fTlsContext;
    /// sequence number of the last LDAP command
    // - incremented with any LDAP command
    property Seq: integer
      read fSeq;
    /// the search scope used in search command
    property SearchScope: TLdapSearchScope
      read fSearchScope Write fSearchScope;
    /// how to handle aliases in search command
    property SearchAliases: TLdapSearchAliases
      read fSearchAliases Write fSearchAliases;
    /// result size limit in search command (bytes)
    // - 0 means without size limit
    property SearchSizeLimit: integer
      read fSearchSizeLimit Write fSearchSizeLimit;
    /// search time limit in search command (seconds)
    // - 0 means without time limit
    property SearchTimeLimit: integer
      read fSearchTimeLimit Write fSearchTimeLimit;
    /// number of results to return per search request
    // - 0 means no paging
    property SearchPageSize: integer
      read fSearchPageSize Write fSearchPageSize;
    /// cookie returned by paged search results
    // - use an empty string for the first search request
    property SearchCookie: RawUtf8
      read fSearchCookie Write fSearchCookie;
    /// result of the search command
    property SearchResult: TLdapResultList
      read fSearchResult;
    /// each LDAP operation on server can return some referals URLs
    property Referals: TRawUtf8List
      read fReferals;
    /// on Extended operation, here is the result Name asreturned by server
    property ExtName: RawUtf8
      read fExtName;
    /// on Extended operation, here is the result Value as returned by server
    property ExtValue: RawUtf8
      read fExtValue;
    /// raw TCP socket used by all LDAP operations
    property Sock: TCrtSocket
      read fSock;
    /// Root DN, retrieved using DiscoverRootDN if possible
    property RootDN: RawUtf8
      read GetRootDN write fRootDN;
    /// domain NETBIOS name, Empty string if not found 
    property NetbiosDomainName: RawUtf8
      read GetNetbiosDomainName;
  published
    /// the authentication and connection settings of a this instance
    property Settings: TLdapClientSettings
      read fSettings;
    /// the version of LDAP protocol used
    // - default value is 3
    property Version: integer
      read fVersion write fVersion default 3;
    /// contains the result code of the last LDAP operation
    // - could be e.g. LDAP_RES_SUCCESS or an error code - see ResultString
    property ResultCode: integer
      read fResultCode;
    /// human readable description of the last LDAP operation
    property ResultString: RawUtf8
      read fResultString;
  end;



implementation


{****** Support procedures and functions }

function IsBinaryString(const Value: RawByteString): boolean;
var
  n: PtrInt;
begin
  result := true;
  for n := 1 to length(Value) do
    if ord(Value[n]) in [0..8, 10..31] then
      // consider null-terminated strings as non-binary
      if (n <> length(value)) or
         (Value[n] = #0) then
        exit;
  result := false;
end;

function SeparateLeft(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x: PtrInt;
begin
  x := PosExChar(Delimiter, Value);
  if x = 0 then
    result := Value
  else
    result := copy(Value, 1, x - 1);
end;

function SeparateRight(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x: PtrInt;
begin
  x := PosExChar(Delimiter, Value);
  result := copy(Value, x + 1, length(Value) - x);
end;

function SeparateRightU(const Value, Delimiter: RawUtf8): RawUtf8;
var
  x: PtrInt;
begin
  x := mormot.core.base.PosEx(Delimiter, Value);
  TrimCopy(Value, x + 1, length(Value) - x, result);
end;

function GetBetween(PairBegin, PairEnd: AnsiChar; const Value: RawUtf8): RawUtf8;
var
  n, len, x: PtrInt;
  s: RawUtf8;
begin
  n := length(Value);
  if (n = 2) and
     (Value[1] = PairBegin) and
     (Value[2] = PairEnd) then
  begin
    result := '';//nothing between
    exit;
  end;
  if n < 2 then
  begin
    result := Value;
    exit;
  end;
  s := SeparateRight(Value, PairBegin);
  if s = Value then
  begin
    result := Value;
    exit;
  end;
  n := PosExChar(PairEnd, s);
  if n = 0 then
  begin
    result := Value;
    exit;
  end;
  len := length(s);
  x := 1;
  for n := 1 to len do
  begin
    if s[n] = PairBegin then
      inc(x)
    else if s[n] = PairEnd then
    begin
      dec(x);
      if x <= 0 then
      begin
        len := n - 1;
        break;
      end;
    end;
  end;
  result := copy(s, 1, len);
end;

function DecodeTriplet(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x, l, lv: integer;
  c: AnsiChar;
  b: byte;
  bad: boolean;
begin
  lv := length(Value);
  SetLength(result, lv);
  x := 1;
  l := 1;
  while x <= lv do
  begin
    c := Value[x];
    inc(x);
    if c <> Delimiter then
    begin
      result[l] := c;
      inc(l);
    end
    else
      if x < lv then
      begin
        case Value[x] of
          #13:
            if Value[x + 1] = #10 then
              inc(x, 2)
            else
              inc(x);
          #10:
            if Value[x + 1] = #13 then
              inc(x, 2)
            else
              inc(x);
        else
          begin
            bad := false;
            case Value[x] of
              '0'..'9':
                b := (byte(Value[x]) - 48) shl 4;
              'a'..'f',
              'A'..'F':
                b := ((byte(Value[x]) and 7) + 9) shl 4;
            else
              begin
                b := 0;
                bad := true;
              end;
            end;
            case Value[x + 1] of
              '0'..'9':
                b := b or (byte(Value[x + 1]) - 48);
              'a'..'f',
              'A'..'F':
                b := b or ((byte(Value[x + 1]) and 7) + 9);
            else
              bad := true;
            end;
            if bad then
            begin
              result[l] := c;
              inc(l);
            end
            else
            begin
              inc(x, 2);
              result[l] := AnsiChar(b);
              inc(l);
            end;
          end;
        end;
      end
      else
        break;
  end;
  dec(l);
  SetLength(result, l);
end;

function TrimSPLeft(const S: RawUtf8): RawUtf8;
var
  i, l: integer;
begin
  result := '';
  if S = '' then
    exit;
  l := length(S);
  i := 1;
  while (i <= l) and
        (S[i] = ' ') do
    inc(i);
  result := copy(S, i, Maxint);
end;

function TrimSPRight(const S: RawUtf8): RawUtf8;
var
  i: integer;
begin
  result := '';
  if S = '' then
    exit;
  i := length(S);
  while (i > 0) and
        (S[i] = ' ') do
    dec(i);
  result := copy(S, 1, i);
end;

function TrimSP(const S: RawUtf8): RawUtf8;
begin
  result := TrimSPRight(TrimSPLeft(s));
end;

function FetchBin(var Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  s: RawUtf8;
begin
  result := SeparateLeft(Value, Delimiter);
  s := SeparateRight(Value, Delimiter);
  if s = Value then
    Value := ''
  else
    Value := s;
end;

function Fetch(var Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
begin
  result := TrimSP(FetchBin(Value, Delimiter));
  Value := TrimSP(Value);
end;



{ **************** Basic ASN.1 Support }

function AsnEncOidItem(Value: Int64): TAsnObject;
var
  r: PByte;
begin
  FastSetRawByteString(result, nil, 16);
  r := pointer(result);
  r^ := byte(Value) and $7f;
  inc(r);
  Value := Value shr 7;
  while Value <> 0 do
  begin
    r^ := byte(Value) or $80;
    inc(r);
    Value := Value shr 7;
  end;
  FakeLength(result, PAnsiChar(r) - pointer(result));
end;

function AsnDecOidItem(var Pos: integer; const Buffer: TAsnObject): integer;
var
  x: byte;
begin
  result := 0;
  repeat
    result := result shl 7;
    x := ord(Buffer[Pos]);
    inc(Pos);
    inc(result, x and $7F);
  until (x and $80) = 0;
end;

function AsnEncLen(Len: cardinal; dest: PByte): PtrInt;
var
  n: PtrInt;
  tmp: array[0..7] of byte;
begin
  if Len < $80 then
  begin
    dest^ := Len;
    result := 1;
    exit;
  end;
  n := 0;
  repeat
    tmp[n] := byte(Len);
    inc(n);
    Len := Len shr 8;
  until Len = 0;
  result := n + 1;
  dest^ := byte(n) or $80; // first byte is number of following bytes + $80
  repeat
    inc(dest);
    dec(n);
    dest^ := tmp[n]; // stored as big endian
  until n = 0;
end;

function AsnDecLen(var Start: integer; const Buffer: TAsnObject): cardinal;
var
  n: byte;
begin
  result := ord(Buffer[Start]);
  inc(Start);
  if result < $80 then
    exit;
  n := result and $7f;
  result := 0;
  repeat
    result := (result shl 8) + cardinal(Buffer[Start]);
    inc(Start);
    dec(n);
  until n = 0;
end;

function AsnEncInt(Value: Int64): TAsnObject;
var
  y: byte;
  neg: boolean;
  n: PtrInt;
  p: PByte;
  tmp: array[0..15] of byte;
begin
  result := '';
  neg := Value < 0;
  Value := Abs(Value);
  if neg then
    dec(Value);
  n := 0;
  repeat
    y := byte(Value);
    if neg then
      y := not y;
    tmp[n] := y;
    inc(n);
    Value := Value shr 8;
  until Value = 0;
  if neg then
  begin
    if y <= $7f then
    begin
      tmp[n] := $ff; // negative numbers start with ff or 8x
      inc(n);
    end;
  end
  else if y > $7F then
  begin
    tmp[n] := 0; // positive numbers start with a 0 or 0x..7x
    inc(n);
  end;
  FastSetRawByteString(result, nil, n);
  p := pointer(result);
  repeat
    dec(n);
    p^ := tmp[n]; // stored as big endian
    inc(p);
  until n = 0;
end;

function Asn(AsnType: integer; const Content: array of TAsnObject): TAsnObject;
var
  tmp: array[0..7] of byte;
  i, len, al: PtrInt;
  p: PByte;
begin
  len := 0;
  for i := 0 to high(Content) do
    inc(len, length(Content[i]));
  al := AsnEncLen(len, @tmp);
  SetString(result, nil, 1 + al + len);
  p := pointer(result);
  p^ := AsnType;         // type
  inc(p);
  MoveFast(tmp, p^, al); // encoded length
  inc(p, al);
  for i := 0 to high(Content) do
  begin
    len := length(Content[i]);
    MoveFast(pointer(Content[i])^, p^, len); // content
    inc(p, len);
  end;
end;

function Asn(const Data: RawByteString; AsnType: integer): TAsnObject;
begin
  result := Asn(AsnType, [Data]);
end;

function Asn(Value: Int64; AsnType: integer): TAsnObject;
begin
  result := Asn(AsnType, [AsnEncInt(Value)]);
end;

function Asn(Value: boolean): TAsnObject;
begin
  result := Asn(ASN1_BOOL, [AsnEncInt(ASN1_BOOLEAN[Value])]);
end;

function AsnSeq(const Data: TAsnObject): TAsnObject;
begin
  result := Asn(ASN1_SEQ, [Data]);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject);
begin
  Append(Data, Buffer);
end;

procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject; AsnType: integer);
begin
  Append(Data, Asn(AsnType, [Buffer]));
end;

function OidToText(Pos, EndPos: integer; const Buffer: RawByteString): RawUtf8;
var
  x, y: integer;
begin
  result := '';
  while Pos < EndPos do
  begin
    x := AsnDecOidItem(Pos, Buffer);
    if Pos = 2 then
    begin
      y := x div 40; // first byte = two first numbers modulo 40
      x := x mod 40;
      UInt32ToUtf8(y, result);
    end;
    Append(result, ['.', x]);
  end;
end;

function AsnNext(var Pos: integer; const Buffer: TAsnObject;
  out ValueType: integer): RawByteString;
var
  asntype, asnsize, n, l: integer;
  y: int64;
  x: byte;
  neg: boolean;
begin
  result := '';
  ValueType := ASN1_NULL;
  l := length(Buffer);
  if Pos > l then
    exit;
  asntype := ord(Buffer[Pos]);
  ValueType := asntype;
  inc(Pos);
  asnsize := AsnDecLen(Pos, Buffer);
  if (Pos + asnsize - 1) > l then
    exit;
  if (asntype and $20) <> 0 then
    result := copy(Buffer, Pos, asnsize)
  else
    case asntype of
      ASN1_INT,
      ASN1_ENUM,
      ASN1_BOOL:
        begin
          y := 0;
          neg := false;
          for n := 1 to asnsize do
          begin
            x := ord(Buffer[Pos]);
            if (n = 1) and
               (x > $7F) then
              neg := true;
            if neg then
              x := not x;
            y := (y shl 8) + x;
            inc(Pos);
          end;
          if neg then
            y := -(y + 1);
          result := ToUtf8(y);
        end;
      ASN1_COUNTER,
      ASN1_GAUGE,
      ASN1_TIMETICKS,
      ASN1_COUNTER64:
        begin
          y := 0;
          for n := 1 to asnsize do
          begin
            y := (y shl 8) + ord(Buffer[Pos]);
            inc(Pos);
          end;
          result := ToUtf8(y);
        end;
      ASN1_OBJID:
        begin
          result := OidToText(Pos, Pos + asnsize, Buffer);
          inc(Pos, asnsize);
        end;
      ASN1_IPADDR:
        begin
          case asnsize of
            4:
              IP4Text(pointer(@Buffer[Pos]), RawUtf8(result));
            16:
              IP6Text(pointer(@Buffer[Pos]), RawUtf8(result));
          else
            BinToHexLower(@Buffer[Pos], asnsize, RawUtf8(result));
          end;
          inc(Pos, asnsize);
        end;
      ASN1_NULL:
        inc(Pos, asnsize);
    else
      // ASN1_UTF8STRING, ASN1_OCTSTR, ASN1_OPAQUE or unknown
      begin
        result := copy(Buffer, Pos, asnsize); // return as raw binary
        inc(Pos, asnsize);
      end;
    end;
end;

function DNToCN(const DN: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
  DC, OU, CN, kind, value: RawUtf8;
begin
  p := pointer(DN);
  while p <> nil do
  begin
    GetNextItemTrimed(p, '=', kind);
    GetNextItemTrimed(p, ',', value);
    if (kind = '') or
       (value = '') then
      raise ENetSock.CreateFmt('DNToCN(%s): invalid Distinguished Name', [DN]);
    UpperCaseSelf(kind);
    if kind = 'DC' then
    begin
      if DC <> '' then
        DC := DC + '.';
      DC := DC + value;
    end
    else if kind = 'OU' then
      Prepend(OU, ['/', value])
    else if kind = 'CN' then
      Prepend(CN, ['/', value]);
  end;
  result := DC + OU + CN;
end;

{$ifdef ASNDEBUG} // not used nor fully tested

function IntMibToStr(const Value: RawByteString): RawUtf8;
var
  i, y: integer;
begin
  y := 0;
  for i := 1 to length(Value) - 1 do
    y := (y shl 8) + ord(Value[i]);
  UInt32ToUtf8(y, result);
end;

function MibToId(Mib: RawUtf8): RawByteString;
var
  x: integer;

  function WalkInt(var s: RawUtf8): integer;
  var
    x: integer;
    t: RawByteString;
  begin
    x := PosExChar('.', s);
    if x < 1 then
    begin
      t := s;
      s := '';
    end
    else
    begin
      t := copy(s, 1, x - 1);
      s := copy(s, x + 1, length(s) - x);
    end;
    result := Utf8ToInteger(t, 0);
  end;

begin
  result := '';
  x := WalkInt(Mib);
  x := x * 40 + WalkInt(Mib);
  result := AsnEncOidItem(x);
  while Mib <> '' do
  begin
    x := WalkInt(Mib);
    Append(result, AsnEncOidItem(x));
  end;
end;

function AsnEncUInt(Value: integer): RawByteString;
var
  x, y: integer;
  neg: boolean;
begin
  neg := Value < 0;
  x := Value;
  if neg then
    x := x and $7FFFFFFF;
  result := '';
  repeat
    y := x mod 256;
    x := x div 256;
    Prepend(result, [AnsiChar(y)]);
  until x = 0;
  if neg then
    result[1] := AnsiChar(ord(result[1]) or $80);
end;

function DumpExStr(const Buffer: RawByteString): RawUtf8;
var
  n: integer;
  x: byte;
begin
  result := '';
  for n := 1 to length(Buffer) do
  begin
    x := ord(Buffer[n]);
    if x in [65..90, 97..122] then
      Append(result, [' +''', AnsiChar(x), ''''])
    else
      Append(result, [' +#$', BinToHexDisplayLowerShort(@x, 1)]);
  end;
end;

function AsnDump(const Value: TAsnObject): RawUtf8;
var
  i, at, x, n: integer;
  s, indent: RawUtf8;
  il: TIntegerDynArray;
begin
  result := '';
  i := 1;
  indent := '';
  while i < length(Value) do
  begin
    for n := length(il) - 1 downto 0 do
    begin
      x := il[n];
      if x <= i then
      begin
        DeleteInteger(il, n);
        Delete(indent, 1, 2);
      end;
    end;
    s := AsnNext(i, Value, at);
    Append(result, [indent, '$', IntToHex(at, 2)]);
    if (at and $20) > 0 then
    begin
      x := length(s);
      Append(result, [' constructed: length ', x]);
      Append(indent, '  ');
      AddInteger(il, x + i - 1);
    end
    else
    begin
      case at of
        ASN1_BOOL:
          Append(result, ' BOOL: ');
        ASN1_INT:
          Append(result, ' INT: ');
        ASN1_ENUM:
          Append(result, ' ENUM: ');
        ASN1_COUNTER:
          Append(result, ' COUNTER: ');
        ASN1_GAUGE:
          Append(result, ' GAUGE: ');
        ASN1_TIMETICKS:
          Append(result, ' TIMETICKS: ');
        ASN1_OCTSTR:
          Append(result, ' OCTSTR: ');
        ASN1_OPAQUE:
          Append(result, ' OPAQUE: ');
        ASN1_OBJID:
          Append(result, ' OBJID: ');
        ASN1_IPADDR:
          Append(result, ' IPADDR: ');
        ASN1_NULL:
          Append(result, ' NULL: ');
        ASN1_COUNTER64:
          Append(result, ' COUNTER64: ');
      else // other
        Append(result, ' unknown: ');
      end;
      if IsBinaryString(s) then
        s := DumpExStr(s);
      Append(result, s);
    end;
    Append(result, #$0d);
    Append(result, #$0a);
  end;
end;

{$endif ASNDEBUG}


{ **************** LDAP Response Storage }

{ TLdapAttribute }

constructor TLdapAttribute.Create(const AttrName: RawUtf8);
begin
  inherited Create;
  fAttributeName := AttrName;
  fIsBinary := StrPosI(';BINARY', pointer(AttrName)) <> nil;
  SetLength(fList, 1); // optimized for a single value (most used case)
end;

procedure TLdapAttribute.Add(const aValue: RawByteString);
begin
  AddRawUtf8(fList, fCount, aValue);
end;

function TLdapAttribute.GetReadable(index: PtrInt): RawUtf8;
begin
  if (self = nil) or
     (index >= fCount) then
    result := ''
  else
  begin
    result := fList[index];
    if fIsBinary then
      result := BinToBase64(result)
    else if IsBinaryString(result) then
      result := LogEscapeFull(result);
    SetCodePage(RawByteString(Result), CP_UTF8, False);
  end;
end;

function TLdapAttribute.GetRaw(index: PtrInt): RawByteString;
begin
  if (self = nil) or
     (index >= fCount) then
    result := ''
  else
    result := fList[index];
end;


{ TLdapAttributeList }

destructor TLdapAttributeList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLdapAttributeList.Clear;
begin
  ObjArrayClear(fItems);
end;

function TLdapAttributeList.Count: integer;
begin
  result := length(fItems);
end;

function TLdapAttributeList.FindIndex(const AttributeName: RawUtf8): PtrInt;
begin
  if self <> nil then
    for result := 0 to length(fItems) - 1 do
      if IdemPropNameU(fItems[result].AttributeName, AttributeName) then
        exit;
  result := -1;
end;

function TLdapAttributeList.Find(const AttributeName: RawUtf8): TLdapAttribute;
var
  i: PtrInt;
begin
  i := FindIndex(AttributeName);
  if i >= 0 then
    result := fItems[i]
  else
    result := nil;
end;

function TLdapAttributeList.Get(const AttributeName: RawUtf8): RawUtf8;
begin
  result := Find(AttributeName).GetReadable(0);
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8): TLdapAttribute;
begin
  result := TLdapAttribute.Create(AttributeName);
  ObjArrayAdd(fItems, result);
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8;
  const AttributeValue: RawByteString): TLdapAttribute;
begin
  result := Add(AttributeName);
  result.Add(AttributeValue);
end;

procedure TLdapAttributeList.Delete(const AttributeName: RawUtf8);
begin
  ObjArrayDelete(fItems, FindIndex(AttributeName));
end;


{ TLdapResult }

constructor TLdapResult.Create;
begin
  inherited Create;
  fAttributes := TLdapAttributeList.Create;
end;

destructor TLdapResult.Destroy;
begin
  fAttributes.Free;
  inherited Destroy;
end;

function TLdapResult.CopyObjectSid(out objectSid: RawUtf8): boolean;
begin
  objectSid := SidToText(Attributes.Find('objectSid').GetRaw);
  result := objectSid <> '';
end;

function TLdapResult.CopyObjectGUID(out objectGUID: TGuid): boolean;
var
  GuidAttr: TLdapAttribute;
  GuidBinary: RawByteString;
begin
  result := false;
  GuidAttr := Attributes.Find('objectGUID');
  if GuidAttr = nil then
    exit;
  GuidBinary := GuidAttr.GetRaw;
  if length(GuidBinary) = SizeOf(TGuid) then
  begin
    objectGUID := PGuid(GuidBinary)^;
    result := true;
  end;
end;


{ TLdapResultList }

destructor TLdapResultList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLdapResultList.Clear;
begin
  ObjArrayClear(fItems, fCount);
  fCount := 0;
end;

function TLdapResultList.ObjectNames(asCN: boolean): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) then
    exit;
  SetLength(result, fCount);
  for i := 0 to fCount - 1 do
  begin
    result[i] := fItems[i].ObjectName;
    if asCN then
      result[i] := DNToCN(result[i]);
  end;
  QuickSortRawUtf8(result, fCount);
end;

procedure TLdapResultList.AfterAdd;
begin
  if fItems <> nil then
    DynArrayFakeLength(fItems, fCount);
end;

function TLdapResultList.Add: TLdapResult;
begin
  result := TLdapResult.Create;
  ObjArrayAddCount(fItems, result, fCount);
end;

function TLdapResultList.Dump: RawUtf8;
var
  i, j, k: PtrInt;
  res: TLdapResult;
  attr: TLdapAttribute;
begin
  result := 'results: ' + ToUtf8(Count) + CRLF + CRLF;
  for i := 0 to Count - 1 do
  begin
    result := result + ToUtf8(i) + ':' + CRLF;
    res := Items[i];
    result := result + '  Object: ' + res.ObjectName + CRLF;
    for j := 0 to res.Attributes.Count - 1 do
    begin
      attr := res.Attributes.Items[j];
      result := result + '  Attribute: ' + attr.AttributeName + CRLF;
      for k := 0 to attr.Count - 1 do
        result := result + '    ' + attr.GetReadable(k) + CRLF;
    end;
  end;
end;


{ **************** LDAP Client Class }


{ TLdapClientSettings }

constructor TLdapClientSettings.Create;
begin
  inherited Create;
  fTargetPort := '389';
  fTimeout := 5000;
end;

destructor TLdapClientSettings.Destroy;
begin
  inherited Destroy;
  FillZero(fPassword);
end;

function TLdapClientSettings.LoadDefaultFromSystem(TryKerberos: boolean): boolean;
var
  test: TLdapClient;
begin
  result := false;
  test := TLdapClient.Create;
  try
    test.Settings.KerberosDN := fKerberosDN; // allow customization
    if not test.Login then
      exit;
    if TryKerberos then
    begin
      test.Settings.KerberosSpn := fKerberosSpn;
      if not test.BindSaslKerberos then
        exit;
    end;
    CopyObject(test.Settings, self);
  finally
    test.Free;
  end;
  result := true;
end;


{ TLdapClient }

constructor TLdapClient.Create;
begin
  inherited Create;
  fSettings := TLdapClientSettings.Create;
  fReferals := TRawUtf8List.Create;
  fTlsContext.IgnoreCertificateErrors := true;
  fVersion := 3;
  fSearchScope := lssWholeSubtree;
  fSearchAliases := lsaAlways;
  fSearchResult := TLdapResultList.Create;
end;

constructor TLdapClient.Create(aSettings: TLdapClientSettings);
begin
  Create;
  CopyObject(aSettings, fSettings);
end;

destructor TLdapClient.Destroy;
begin
  fSock.Free;
  fSearchResult.Free;
  fReferals.Free;
  fSettings.Free;
  inherited Destroy;
end;

class function TLdapClient.GetErrorString(ErrorCode: integer): RawUtf8;
begin
  case ErrorCode of
    LDAP_RES_SUCCESS:
      result := 'Success';
    LDAP_RES_OPERATIONS_ERROR:
      result := 'Operations error';
    LDAP_RES_PROTOCOL_ERROR:
      result := 'Protocol error';
    LDAP_RES_TIME_LIMIT_EXCEEDED:
      result := 'Time limit Exceeded';
    LDAP_RES_SIZE_LIMIT_EXCEEDED:
      result := 'Size limit Exceeded';
    LDAP_RES_COMPARE_FALSE:
      result := 'Compare false';
    LDAP_RES_COMPARE_TRUE:
      result := 'Compare true';
    LDAP_RES_AUTH_METHOD_NOT_SUPPORTED:
      result := 'Auth method not supported';
    LDAP_RES_STRONGER_AUTH_REQUIRED:
      result := 'Strong auth required';
    LDAP_RES_REFERRAL:
      result := 'Referral';
    LDAP_RES_ADMIN_LIMIT_EXCEEDED:
      result := 'Admin limit exceeded';
    LDAP_RES_UNAVAILABLE_CRITICAL_EXTENSION:
      result := 'Unavailable critical extension';
    LDAP_RES_CONFIDENTIALITY_REQUIRED:
      result := 'Confidentality required';
    LDAP_RES_SASL_BIND_IN_PROGRESS:
      result := 'Sasl bind in progress';
    LDAP_RES_NO_SUCH_ATTRIBUTE:
      result := 'No such attribute';
    LDAP_RES_UNDEFINED_ATTRIBUTE_TYPE:
      result := 'Undefined attribute type';
    LDAP_RES_INAPPROPRIATE_MATCHING:
      result := 'Inappropriate matching';
    LDAP_RES_CONSTRAINT_VIOLATION:
      result := 'Constraint violation';
    LDAP_RES_ATTRIBUTE_OR_VALUE_EXISTS:
      result := 'Attribute or value exists';
    LDAP_RES_INVALID_ATTRIBUTE_SYNTAX:
      result := 'Invalid attribute syntax';
    LDAP_RES_NO_SUCH_OBJECT:
      result := 'No such object';
    LDAP_RES_ALIAS_PROBLEM:
      result := 'Alias problem';
    LDAP_RES_INVALID_DN_SYNTAX:
      result := 'Invalid DN syntax';
    LDAP_RES_ALIAS_DEREFERENCING_PROBLEM:
      result := 'Alias dereferencing problem';
    LDAP_RES_INAPPROPRIATE_AUTHENTICATION:
      result := 'Inappropriate authentication';
    LDAP_RES_INVALID_CREDENTIALS:
      result := 'Invalid credentials';
    LDAP_RES_INSUFFICIENT_ACCESS_RIGHTS:
      result := 'Insufficient access rights';
    LDAP_RES_BUSY:
      result := 'Busy';
    LDAP_RES_UNAVAILABLE:
      result := 'Unavailable';
    LDAP_RES_UNWILLING_TO_PERFORM:
      result := 'Unwilling to perform';
    LDAP_RES_LOOP_DETECT:
      result := 'Loop detect';
    LDAP_RES_NAMING_VIOLATION:
      result := 'Naming violation';
    LDAP_RES_OBJECT_CLASS_VIOLATION:
      result := 'Object class violation';
    LDAP_RES_NOT_ALLOWED_ON_NON_LEAF:
      result := 'Not allowed on non leaf';
    LDAP_RES_NOT_ALLOWED_ON_RDN:
      result := 'Not allowed on RDN';
    LDAP_RES_ENTRY_ALREADY_EXISTS:
      result := 'Entry already exists';
    LDAP_RES_OBJECT_CLASS_MODS_PROHIBITED:
      result := 'Object class mods prohibited';
    LDAP_RES_AFFECTS_MULTIPLE_DSAS:
      result := 'Affects multiple DSAs';
    LDAP_RES_OTHER:
      result := 'Other';
  else
    FormatUtf8('unknown #%', [ErrorCode], result);
  end;
end;

function TLdapClient.ReceiveString(Size: integer): RawByteString;
begin
  if fSock = nil then
    result := ''
  else
  begin
    FastSetRawByteString(result, nil, Size);
    fSock.SockInRead(pointer(result), Size);
  end;
end;

function TLdapClient.Login: boolean;
var
  dc: TRawUtf8DynArray;
  h, p: RawUtf8;
  i: PtrInt;
begin
  result := fSock <> nil;
  if result then
    exit; // socket was already connected
  if fSettings.TargetHost = '' then
  begin
    if ForcedDomainName = '' then
      ForcedDomainName := fSettings.KerberosDN; // may be pre-set
    dc := DnsLdapControlers('', false, @fSettings.fKerberosDN);  // from OS
  end
  else
    AddRawUtf8(dc,  // from instance properties
      fSettings.TargetHost + ':' + fSettings.TargetPort);
  fSeq := 0;
  for i := 0 to high(dc) do
    try
      Split(dc[i], ':', h, p);
      if fSettings.TargetHost = '' then // not from DnsLdapControlers
      begin
        if HasOpenSsl and // SChannel seems to have troubles with LDAP TLS
           (p = '389') and
           not fSettings.Tls then
        try
          // always first try to connect with TLS on its default port (much safer)
          fSock := TCrtSocket.Open(
            h, '636', nlTcp, fSettings.TimeOut, {tls=}true, @fTlsContext);
          p := '636';
        except
          on E: ENetSock do
            FreeAndNil(fSock); // no TLS support on this port
        end;
      end
      else
        fSettings.Tls := (p = '636') or // this port is likely to be over TLS
                         (p = '3269');
      if fSock = nil then
        // try connection to the server
        fSock := TCrtSocket.Open(
          h, p, nlTcp, fSettings.TimeOut, fSettings.Tls, @fTlsContext);
      fSock.CreateSockIn;
      result := fSock.SockConnected;
      if result then
      begin
        fSettings.TargetHost := h;
        fSettings.TargetPort := p;
        fSettings.Tls := fSock.TLS.Enabled;
        exit;
      end;
    except
      on E: ENetSock do
        FreeAndNil(fSock); // abort and try next dc[]
    end;
end;

function TLdapClient.BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
begin
  inc(fSeq);
  result := Asn(ASN1_SEQ, [
    Asn(fSeq),
    Asn1Data]);
end;

function TLdapClient.GetNetbiosDomainName: RawUtf8;
var
  NetbiosObject: TLdapResult;
begin
  NetbiosObject := SearchFirst(FormatUtf8('CN=Partitions,CN=Configuration,%', [RootDN]),
    '(nETBIOSName=*)', ['nETBIOSName']);
  if Assigned(NetbiosObject) then
    result := NetbiosObject.Attributes.Get('nETBIOSName')
  else
    result := '';
end;

function TLdapClient.GetRootDN: RawUtf8;
begin
  if (fRootDN = '') and
     fSock.SockConnected then
    fRootDN := DiscoverRootDN;
  result := fRootDN;
end;

procedure TLdapClient.SendPacket(const Asn1Data: TAsnObject);
begin
  if fSock <> nil then
    fSock.SockSendFlush(BuildPacket(Asn1Data));
end;

function TLdapClient.ReceiveResponse: TAsnObject;
var
  b: byte;
  len, pos: integer;
begin
  result := '';
  if fSock = nil then
    exit;
  fFullResult := '';
  try
    // receive ASN type
    fSock.SockInRead(pointer(@b), 1);
    if b <> ASN1_SEQ then
      exit;
    result := AnsiChar(b);
    // receive length
    fSock.SockInRead(pointer(@b), 1);
    Append(result, @b, 1);
    if b >= $80 then // $8x means x bytes of length
      AsnAdd(result, ReceiveString(b and $7f));
    // decode length of LDAP packet
    pos := 2;
    len := AsnDecLen(pos, result);
    // retrieve body of LDAP packet
    if len > 0 then
      AsnAdd(result, ReceiveString(len));
  except
    on E: ENetSock do
    begin
      result := '';
      exit;
    end;
  end;
  fFullResult := result;
end;

// see https://ldap.com/ldapv3-wire-protocol-reference-ldap-result

function TLdapClient.DecodeResponse(const Asn1Response: TAsnObject): TAsnObject;
var
  i, x, numseq: integer;
  asntype: integer;
  s, t: TAsnObject;
begin
  result := '';
  fResultCode := -1;
  fResultString := '';
  fResponseCode := -1;
  fResponseDN := '';
  fReferals.Clear;
  i := 1;
  AsnNext(i, Asn1Response, asntype); // initial ANS1_SEQ
  numseq := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), 0);
  if (asntype <> ASN1_INT) or
     (numseq <> fSeq) then
    exit;
  AsnNext(i, Asn1Response, fResponseCode);
  if fResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    fResultCode := Utf8ToInteger(AsnNext(i, Asn1Response, asntype), -1);
    fResponseDN := AsnNext(i, Asn1Response, asntype);   // matchedDN
    fResultString := AsnNext(i, Asn1Response, asntype); // diagnosticMessage
    if fResultString = '' then
      fResultString := GetErrorString(fResultCode);
    if fResultCode = LDAP_RES_REFERRAL then
    begin
      s := AsnNext(i, Asn1Response, asntype);
      if asntype = ASN1_CTC3 then
      begin
        x := 1;
        while x < length(s) do
        begin
          t := AsnNext(x, s, asntype);
          fReferals.Add(t);
        end;
      end;
    end;
  end;
  result := copy(Asn1Response, i, length(Asn1Response) - i + 1); // body
end;

function TLdapClient.SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
begin
  SendPacket(Asn1Data);
  result := DecodeResponse(ReceiveResponse);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function TLdapClient.TranslateFilter(const Filter: RawUtf8): TAsnObject;
var
  x, dn: integer;
  c: Ansichar;
  s, t, l, r, attr, rule: RawUtf8;
begin
  result := '';
  if Filter = '' then
    exit;
  s := Filter;
  if Filter[1] = '(' then
    for x := length(Filter) downto 2 do
      if Filter[x] = ')' then
      begin
        s := copy(Filter, 2, x - 2); // get value between (...)
        break;
      end;
  if s = '' then
    exit;
  case s[1] of
    '!':
      // NOT rule (recursive call)
      result := Asn(TranslateFilter(GetBetween('(', ')', s)), ASN1_CTC2);
    '&':
      // and rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRightU(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          AsnAdd(result, TranslateFilter(t));
        until s = '';
        result := Asn(result, ASN1_CTC0);
      end;
    '|':
      // or rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRightU(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          AsnAdd(result, TranslateFilter(t));
        until s = '';
        result := Asn(result, ASN1_CTC1);
      end;
    else
      begin
        l := TrimU(SeparateLeft(s, '='));
        r := TrimU(SeparateRight(s, '='));
        if l <> '' then
        begin
          c := l[length(l)];
          case c of
            ':':
              // Extensible match
              begin
                System.Delete(l, length(l), 1);
                dn := ASN1_BOOLEAN[false];
                attr := '';
                rule := '';
                if mormot.core.base.PosEx(':dn', l) > 0 then
                begin
                  dn := ASN1_BOOLEAN[true];
                  l := StringReplaceAll(l, ':dn', '');
                end;
                attr := TrimU(SeparateLeft(l, ':'));
                rule := TrimU(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  result := Asn(rule, ASN1_CTX1);
                if attr <> '' then
                  AsnAdd(result, attr, ASN1_CTX2);
                AsnAdd(result, DecodeTriplet(r, '\'), ASN1_CTX3);
                AsnAdd(result, AsnEncInt(dn), ASN1_CTX4);
                result := Asn(result, ASN1_CTC9);
              end;
            '~':
              // Approx match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC8, [
                  Asn(l),
                  Asn(DecodeTriplet(r, '\'))]);
              end;
            '>':
              // Greater or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC5, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
            '<':
              // Less or equal match
              begin
                System.Delete(l, length(l), 1);
                result := Asn(ASN1_CTC6, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
          else
            // present
            if r = '*' then
              result := Asn(l, ASN1_CTX7)
            else
              if PosExChar('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  result := Asn(DecodeTriplet(s, '\'), ASN1_CTX0);
                while r <> '' do
                begin
                  if PosExChar('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  AsnAdd(result, DecodeTriplet(s, '\'), ASN1_CTX1);
                end;
                if r <> '' then
                  AsnAdd(result, DecodeTriplet(r, '\'), ASN1_CTX2);
                result := Asn(ASN1_CTC4, [
                   Asn(l),
                   AsnSeq(result)]);
              end
              else
              begin
                // Equality match
                result := Asn(ASN1_CTC3, [
                   Asn(l),
                   Asn(DecodeTriplet(r, '\'))]);
              end;
          end;
        end;
      end;
  end;
end;

// see https://ldap.com/ldapv3-wire-protocol-reference-bind

function TLdapClient.Bind: boolean;
begin
  result := false;
  if not Login then
    exit;
  SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                   Asn(fVersion),
                   Asn(fSettings.UserName),
                   Asn(fSettings.Password, ASN1_CTX0)]));
  result := fResultCode = LDAP_RES_SUCCESS;
  fBound := result;
end;

function TLdapClient.BindSaslDigest(Algo: THashAlgo): boolean;
var
  x, xt: integer;
  dig: RawUtf8;
  s, t, digreq: TAsnObject;
begin
  result := false;
  if not Login then
    exit;
  if fSettings.Password = '' then
    result := Bind
  else
  begin
    digreq := Asn(LDAP_ASN1_BIND_REQUEST, [
                Asn(fVersion),
                Asn(''),
                Asn(ASN1_CTC3, [
                  Asn(DIGEST_ALGONAME[Algo])])]);
    t := SendAndReceive(digreq);
    if fResultCode = LDAP_RES_SASL_BIND_IN_PROGRESS then
    begin
      s := t;
      x := 1;
      t := AsnNext(x, s, xt);
      dig := DigestClient(Algo, t, 'ldap/' + LowerCaseU(fSock.Server),
        fSettings.UserName, fSettings.Password);
      SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                       Asn(fVersion),
                       Asn(''),
                       Asn(ASN1_CTC3, [
                         Asn(DIGEST_ALGONAME[Algo]),
                         Asn(dig)])]));
      if fResultCode = LDAP_RES_SASL_BIND_IN_PROGRESS then
        SendAndReceive(digreq);
      result := fResultCode = LDAP_RES_SUCCESS;
      fBound := result;
    end;
  end;
end;

function TLdapClient.BindSaslKerberos(const AuthIdentify: RawUtf8;
  KerberosUser: PRawUtf8): boolean;
var
  sc: TSecContext;
  datain, dataout: RawByteString;
  x, xt: integer;
  t, req1, req2: TAsnObject;
begin
  result := false;
  if not Login or
     not InitializeDomainAuth then
     exit;
  if (fSettings.KerberosSpn = '') and
     (fSettings.KerberosDN <> '') then
    fSettings.KerberosSpn := 'LDAP/' + fSettings.TargetHost +
                             '@' + UpperCase(fSettings.KerberosDN);
  req1 := Asn(LDAP_ASN1_BIND_REQUEST, [
            Asn(fVersion),
            Asn(''),
            Asn(ASN1_CTC3, [
              Asn('GSSAPI')])]);
  t := SendAndReceive(req1);
  if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
    exit;
  InvalidateSecContext(sc, 0);
  try
    repeat
      x := 1;
      datain := AsnNext(x, t, xt);
      try
        if fSettings.UserName <> '' then
          ClientSspiAuthWithPassword(sc, datain, fSettings.UserName,
            fSettings.Password, fSettings.KerberosSpn, dataout)
        else
          ClientSspiAuth(sc, datain, fSettings.KerberosSpn, dataout);
      except
        exit; // catch SSPI/GSSAPI errors and return false
      end;
      if dataout = '' then
      begin
        // last step of SASL handshake - see RFC 4752 section 3.1
        t := SendAndReceive(req1);
        if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
          exit;
        x := 1;
        datain := AsnNext(x, t, xt);
        datain := SecDecrypt(sc, datain);
        if (length(datain) <> 4) or
           ((datain[1] = #0) and
            (PCardinal(datain)^ <> 0)) then
          exit; // invalid token
        PCardinal(datain)^ := 0; // #0: noseclayer, #1#2#3: maxmsgsize=0
        if AuthIdentify <> '' then
          Append(datain, AuthIdentify);
        dataout := SecEncrypt(sc, datain);
      end;
      req2 := Asn(LDAP_ASN1_BIND_REQUEST, [
                Asn(fVersion),
                Asn(''),
                Asn(ASN1_CTC3, [
                  Asn('GSSAPI'),
                  Asn(dataout)])]);
      t := SendAndReceive(req2);
    until fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS;
    result := fResultCode = LDAP_RES_SUCCESS;
    if result and
       (KerberosUser <> nil) then
      ServerSspiAuthUser(sc, KerberosUser^);
    fBound := result;
  finally
    FreeSecContext(sc);
  end;
end;


// https://ldap.com/ldapv3-wire-protocol-reference-unbind

function TLdapClient.Logout: boolean;
begin
  SendPacket(Asn('', LDAP_ASN1_UNBIND_REQUEST));
  FreeAndNil(fSock);
  result := true;
  fBound := false;
  fRootDN := '';
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Value: TLdapAttribute): boolean;
var
  query: TAsnObject;
  i: integer;
begin
  for i := 0 to Value.Count -1 do
    AsnAdd(query, Asn(Value.GetRaw(i)));
  SendAndReceive(Asn(LDAP_ASN1_MODIFY_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(ASN1_SEQ, [
                       Asn(ord(Op), ASN1_ENUM),
                       Asn(ASN1_SEQ, [
                         Asn(Value.AttributeName),
                         Asn(query, ASN1_SETOF)])])])]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-add

function TLdapClient.Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
var
  query, sub: TAsnObject;
  attr: TLdapAttribute;
  i, j: PtrInt;
begin
  result := false;
  if not Connected then
    exit;
  for i := 0 to Value.Count - 1 do
  begin
    attr := Value.Items[i];
    sub := '';
    for j := 0 to attr.Count - 1 do
      AsnAdd(sub, Asn(attr.GetRaw(j)));
    Append(query,
      Asn(ASN1_SEQ, [
        Asn(attr.AttributeName),
        Asn(ASN1_SETOF, [sub])]));
  end;
  SendAndReceive(Asn(LDAP_ASN1_ADD_REQUEST, [
                   Asn(obj),
                   AsnSeq(query)]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

function TLdapClient.AddComputer(const ComputerParentDN, ComputerName: RawUtf8;
  out ErrorMessage: RawUtf8; const Password: SpiUtf8; DeleteIfPresent: boolean): boolean;
var
  PwdU8: SpiUtf8;
  ComputerDN: RawUtf8;
  PwdU16: RawByteString;
  Attributes: TLdapAttributeList;
begin
  result := false;
  if not Connected then
    exit;
  ComputerDN := 'CN=' + ComputerName + ',' + ComputerParentDN;
  // Search if computer is already present in the domain
  if not Search(ComputerDN, false, '', []) and
     (ResultCode <> LDAP_RES_NO_SUCH_OBJECT) then
  begin
    ErrorMessage := GetErrorString(ResultCode);
    exit;
  end;
  if SearchResult.Count > 0 then
    if DeleteIfPresent then
      Delete(ComputerDN)
    else
    begin
      ErrorMessage := 'Computer is already present';
      result := true;
      exit;
    end;
  Attributes := TLDAPAttributeList.Create;
  try
    Attributes.Add('objectClass', 'computer');
    Attributes.Add('cn', ComputerName);
    Attributes.Add('sAMAccountName', UpperCase(ComputerName) + '$');
    Attributes.Add('userAccountControl', '4096');
    if Password <> '' then
    begin
      PwdU8 := '"' + Password + '"';
      PwdU16 := Utf8DecodeToUnicodeRawByteString(PwdU8);
      Attributes.Add('unicodePwd', PwdU16);
    end;
    result := Add(ComputerDN, Attributes);
    if not result then
      ErrorMessage := GetErrorString(ResultCode);
  finally
    Attributes.Free;
    FillZero(PwdU8);
    FillZero(PwdU16);
  end;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-delete

function TLdapClient.Delete(const Obj: RawUtf8): boolean;
begin
  result := false;
  if not Connected then
    exit;
  SendAndReceive(Asn(obj, LDAP_ASN1_DEL_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify-dn

function TLdapClient.ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
  DeleteOldRdn: boolean): boolean;
var
  query: TAsnObject;
begin
  result := false;
  if not Connected then
    exit;
  query := Asn(obj);
  Append(query, [Asn(newRdn), Asn(DeleteOldRdn)]);
  if newSuperior <> '' then
    AsnAdd(query, Asn(newSuperior, ASN1_CTX0));
  SendAndReceive(Asn(query, LDAP_ASN1_MODIFYDN_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-compare

function TLdapClient.Compare(const Obj, AttributeValue: RawUtf8): boolean;
begin
  SendAndReceive(Asn(LDAP_ASN1_COMPARE_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(TrimU(SeparateLeft(AttributeValue, '='))),
                     Asn(TrimU(SeparateRight(AttributeValue, '=')))])]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function TLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
var
  s, filt, attr, resp: TAsnObject;
  u: RawUtf8;
  n, i, x: integer;
  r: TLdapResult;
  a: TLdapAttribute;
begin
  result := false;
  if not fSock.SockConnected then
    exit;
  // see https://ldap.com/ldapv3-wire-protocol-reference-search
  fSearchResult.Clear;
  fReferals.Clear;
  if Filter = '' then
    Filter := '(objectclass=*)';
  filt := TranslateFilter(Filter);
  if filt = '' then
    filt := Asn('', ASN1_NULL);
  for n := 0 to high(Attributes) do
    AsnAdd(attr, Asn(Attributes[n]));
  s := Asn(LDAP_ASN1_SEARCH_REQUEST, [
           Asn(BaseDN),
           Asn(ord(fSearchScope),   ASN1_ENUM),
           Asn(ord(fSearchAliases), ASN1_ENUM),
           Asn(fSearchSizeLimit),
           Asn(fSearchTimeLimit),
           Asn(TypesOnly),
           filt,
           AsnSeq(attr)]);
  if fSearchPageSize > 0 then
    Append(s, Asn(
        Asn(ASN1_SEQ, [
           Asn('1.2.840.113556.1.4.319'), // controlType: pagedresultsControl
           Asn(false), // criticality: false
           Asn(Asn(ASN1_SEQ, [
             Asn(fSearchPageSize),
             Asn(fSearchCookie)]))]), LDAP_ASN1_CONTROLS));
  SendPacket(s);
  repeat
    resp := DecodeResponse(ReceiveResponse);
    if fResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      r := fSearchResult.Add;
      n := 1;
      r.ObjectName := AsnNext(n, resp, x);
      AsnNext(n, resp, x);
      if x = ASN1_SEQ then
      begin
        while n < length(resp) do
        begin
          s := AsnNext(n, resp, x);
          if x = ASN1_SEQ then
          begin
            i := n + length(s);
            u := AsnNext(n, resp, x);
            a := r.Attributes.Add(u);
            AsnNext(n, resp, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := AsnNext(n, resp, x);
                a.Add(u);
              end;
          end;
        end;
      end;
    end;
    if fResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < length(resp) do
        fReferals.Add(AsnNext(n, resp, x));
    end;
  until fResponseCode = LDAP_ASN1_SEARCH_DONE;
  n := 1;
  AsnNext(n, resp, x);
  if x = LDAP_ASN1_CONTROLS then
  begin
    AsnNext(n, resp, x);
    if x = ASN1_SEQ then
    begin
      s := AsnNext(n, resp, x);
      if s = '1.2.840.113556.1.4.319' then
      begin
        s := AsnNext(n, resp, x); // searchControlValue
        n := 1;
        AsnNext(n, s, x);
        if x = ASN1_SEQ then
        begin
          // total number of result records, if known, otherwise 0
          AsnNext(n, s, x);
          // active search cookie, empty when done
          fSearchCookie := AsnNext(n, s, x);
        end;
      end;
    end;
  end;
  fSearchResult.AfterAdd; // allow "for res in ldap.SearchResult.Items do"
  result := fResultCode = LDAP_RES_SUCCESS;
end;

function TLdapClient.SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
  const Attributes: array of RawByteString): TLdapResult;
begin
  result := nil;
  if Search(BaseDN, false, Filter, Attributes) and
     (SearchResult.Count > 0) then
    result := SearchResult.Items[0];
end;

function TLdapClient.SearchObject(const ObjectDN: RawUtf8;
  const Attributes: array of RawByteString): TLdapResult;
var
  PreviousSearchScope: TLdapSearchScope;
begin
  PreviousSearchScope := SearchScope;
  try
    SearchScope := lssBaseObject;
    result := SearchFirst(ObjectDN, '', Attributes);
  finally
    SearchScope := PreviousSearchScope;
  end;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-extended

function TLdapClient.Extended(const Oid, Value: RawUtf8): boolean;
var
  query, decoded: TAsnObject;
  pos, xt: integer;
begin
  result := false;
  if not Connected then
    exit;
  query := Asn(Oid, ASN1_CTX0);
  if Value <> '' then
    AsnAdd(query, Asn(Value, ASN1_CTX1));
  decoded := SendAndReceive(Asn(query, LDAP_ASN1_EXT_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
  if result then
  begin
    pos := 1;
    fExtName  := AsnNext(pos, decoded, xt);
    fExtValue := AsnNext(pos, decoded, xt);
  end;
end;

function TLdapClient.DiscoverRootDN: RawUtf8;
var
  prev: TLdapSearchScope;
  root: TLdapResult;
  attr: TLdapAttribute;
begin
  result := '';
  if not fSock.SockConnected then
    exit;
  prev := SearchScope;
  try
    SearchScope := lssBaseObject;
    root := SearchFirst('', '*', ['rootDomainNamingContext']);
    if Assigned(root) then
    begin
      attr := root.Attributes.Find('rootDomainNamingContext');
      if Assigned(attr) then
        result := attr.GetReadable;
    end;
  finally
    SearchScope := prev;
  end;
end;

function TLdapClient.Connected(AndBound: boolean): boolean;
begin
  result := fSock.SockConnected;
  if result and
     AndBound then
    result := fBound;
end;

function TLdapClient.GetWellKnownObject(const ObjectGUID: RawUtf8;
  AsCN: boolean): RawUtf8;
var
  root: TLdapResult;
  attr: TLdapAttribute;
  i: integer;
  sn, v: RawUtf8;
begin
  result := '';
  if not Connected or
     (RootDN = '') then
    exit;
  root := SearchObject(RootDN, ['wellKnownObjects']);
  if not Assigned(root) then
    exit;
  attr := root.Attributes.Find('wellKnownObjects');
  if not Assigned(attr) then
    exit;
  sn := 'B:32:' + ObjectGUID;
  for i := 0 to attr.Count - 1 do
  begin
    v := attr.GetReadable(i);
    if NetStartWith(pointer(v), pointer(sn)) then
    begin
      result := Copy(v, Length(sn) + 2, MaxInt);
      if AsCN then
        result := DNToCN(result);
      break;
    end;
  end;
end;

function TLdapClient.GetWellKnownObjects(AsCN: boolean): TLdapKnownCommonNames;
var
  root: TLdapResult;
  attr: TLdapAttribute;
  tmp: TRawUtf8DynArray;
  i: PtrInt;

  function One(const guid: RawUtf8): RawUtf8;
  var
    sn: RawUtf8;
    i: PtrInt;
  begin
    sn := 'B:32:' + guid;
    for i := 0 to high(tmp) do
      if (tmp[i] <> '') and
         NetStartWith(pointer(tmp[i]), pointer(sn)) then
      begin
        result := copy(tmp[i], Length(sn) + 2, MaxInt);
        if AsCN then
          result := DNToCN(result);
        tmp[i] := ''; // no need to search this one any more
        exit;
      end;
    result := '';
  end;

begin
  Finalize(result);
  if not Connected or
     (RootDN = '') then
    exit;
  root := SearchObject(RootDN, ['wellKnownObjects']);
  if not Assigned(root) then
    exit;
  attr := root.Attributes.Find('wellKnownObjects');
  if not Assigned(attr) then
    exit;
  SetLength(tmp, attr.Count);
  for i := 0 to attr.Count - 1 do
    tmp[i] := attr.GetReadable(i);
  result.Computers                 := One(GUID_COMPUTERS_CONTAINER_W);
  result.DeletedObjects            := One(GUID_DELETED_OBJECTS_CONTAINER_W);
  result.DomainControllers         := One(GUID_DOMAIN_CONTROLLERS_CONTAINER_W);
  result.ForeignSecurityPrincipals := One(GUID_FOREIGNSECURITYPRINCIPALS_CONTAINER_W);
  result.Infrastructure            := One(GUID_INFRASTRUCTURE_CONTAINER_W);
  result.LostAndFound              := One(GUID_LOSTANDFOUND_CONTAINER_W);
  result.MicrosoftProgramData      := One(GUID_MICROSOFT_PROGRAM_DATA_CONTAINER_W);
  result.NtdsQuotas                := One(GUID_NTDS_QUOTAS_CONTAINER_W);
  result.ProgramData               := One(GUID_PROGRAM_DATA_CONTAINER_W);
  result.Systems                   := One(GUID_SYSTEMS_CONTAINER_W);
  result.Users                     := One(GUID_USERS_CONTAINER_W);
  result.ManagedServiceAccounts    := One(GUID_MANAGED_SERVICE_ACCOUNTS_CONTAINER_W);
end;


end.

