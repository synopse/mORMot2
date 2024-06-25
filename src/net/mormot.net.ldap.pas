/// Simple Network LDAP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ldap;

{
  *****************************************************************************

   Simple LDAP Protocol Client
    - LDAP Protocol Definitions
    - LDAP Response Storage
    - CLDAP Client Functions
    - LDAP Client Class
    - HTTP BASIC Authentication via LDAP or Kerberos

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
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.log,
  mormot.lib.sspi, // do-nothing units on non compliant OS
  mormot.lib.gssapi,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.dns;


{ **************** LDAP Protocol Definitions }

/// convert a Distinguished Name to a Canonical Name
// - raise an exception if the supplied DN is not a valid Distinguished Name
// - e.g. DNToCN('CN=User1,OU=Users,OU=London,DC=xyz,DC=local') =
// 'xyz.local/London/Users/User1'
function DNToCN(const DN: RawUtf8): RawUtf8;

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

  LDAP_RES_NOERROR = [
    LDAP_RES_SUCCESS,
    LDAP_RES_COMPARE_FALSE,
    LDAP_RES_COMPARE_TRUE,
    LDAP_RES_SASL_BIND_IN_PROGRESS];

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
  LDAP_ASN1_ERROR             = -1;

  LDAP_ASN1_RESPONSES = [
    LDAP_ASN1_BIND_RESPONSE,
    LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE,
    LDAP_ASN1_ADD_RESPONSE,
    LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE,
    LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE];

const
  /// the TCP/UDP port usually published by a LDAP server
  LDAP_PORT = '389';
  /// the TCP port usually published by a LDAP server over TLS
  LDAP_TLS_PORT = '636';
  /// the TCP port usually published by a LDAP server over plain or TLS
  LDAP_DEFAULT_PORT: array[{tls=}boolean] of RawUtf8 = (
    LDAP_PORT,
    LDAP_TLS_PORT);
  /// the URI scheme for a LDAP server over plain or TLS
  LDAP_DEFAULT_SCHEME: array[{tls=}boolean] of RawUtf8 = (
    'ldap://',
    'ldaps://');

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

/// translate a LDAP_RES_* integer result code into some human-readable text
function RawLdapErrorString(ErrorCode: integer): RawUtf8;

/// encode a LDAP search filter text into an ASN.1 binary
// - as used by CldapBroadcast() and TLdapClient.Search()
function RawLdapTranslateFilter(const Filter: RawUtf8): TAsnObject;

/// encode the ASN.1 binary for a LDAP_ASN1_SEARCH_REQUEST
// - as used by CldapBroadcast() and TLdapClient.Search()
function RawLdapSearch(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawUtf8;
  Scope: TLdapSearchScope = lssBaseObject; Aliases: TLdapSearchAliases = lsaAlways;
  Sizelimit: integer = 0; TimeLimit: integer = 0): TAsnObject;

/// decode the ASN.1 binary of a LDAP_ASN1_SEARCH_ENTRY
// - and lookup by name the returned attributes as RawUtf8 variables
// - as used by CldapBroadcast()
function RawLdapSearchParse(const Response: TAsnObject; MessageId: integer;
  const Attributes: array of RawUtf8; const Values: array of PRawUtf8): boolean;

/// returns true when no * ( ) \ character is part of Text
// - to avoid LDAP filter injection, e.g. from user-entered names
// - note that MS AD does escape with \## hexadecimal, whereas other servers
// are likely to use simple \ escape: it is easier to reject than escaping
function LdapSafe(const Text: RawUtf8): boolean;

const
  /// the chars to escape for LdapEscape()
  LDAP_ESC: array[{keepwildchar=}boolean] of TSynAnsicharSet = (
    [#0 .. #31, '(', ')', '&', '|', '=', '!', '>', '<', '~', '/', '\', '*'],
    [#0 .. #31, '(', ')', '&', '|', '=', '!', '>', '<', '~', '/', '\']);

  /// the chars to escape for LdapEscapeCN()
  LDAP_CN: TSynAnsicharSet = (
    ['.', '/', '\']);

/// escape the ( ) & | = ! > < ~ * / \ characters as expected by LDAP filters
// - you can let * untouched if KeepWildChar is set
function LdapEscape(const Text: RawUtf8; KeepWildChar: boolean = false): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// returns true when no * \ character is part of a non-void UTF-8 name
// - as called by LdapEscapeName() for sAMAccountName
function LdapValidName(const Text: RawUtf8): boolean;

/// calls LdapValidName() then LdapEscape() or return false
// - in the context of user or group distinguished name, i.e. will return
// false if Text is void or contains any unexpected * or \ character
function LdapEscapeName(const Text: RawUtf8; var Safe: RawUtf8): boolean; overload;

/// calls LdapValidName() then LdapEscape() or raise an exception
// - in the context of user or group distinguished name, i.e. will raise an
// ELdap exception if Text is void or contains any unexpected * or \ character
function LdapEscapeName(const Text: RawUtf8): RawUtf8; overload;

/// returns true when no * character is part of a non-void UTF-8 name
function LdapIsValidDistinguishedName(const Text: RawUtf8): boolean;

/// returns Text when no * character is part of a non-void UTF-8 name
// - used e.g. for distinguishedName values - with no escape
function LdapValidDistinguishedName(const Text: RawUtf8): RawUtf8;

/// decode \xx or \c from a LDAP string value
// - following e.g. https://www.rfc-editor.org/rfc/rfc4514#section-2.4
// specifications about distinguished names encoding
// - is also the reverse function of LdapEscape()
function LdapUnescape(const Text: RawUtf8): RawUtf8;
  {$ifdef HASINLINE} inline; {$endif}

/// escape the . / \ characters as expected by LDAP filters
function LdapEscapeCN(const Text: RawUtf8): RawUtf8;


{ **************** CLDAP Client Functions }

type
  /// the decoded TCldapDomainInfo.RawLogonType value
  TCldapDomainLogonType = (
    cltUnknown,
    cltUser,
    cltAnonymous);

  /// each decoded TCldapDomainInfo.RawFlags value
  TCldapDomainFlag = (
    cdfPDC,
    cdfObsolete,
    cdfGC,
    cdfLDAP,
    cdfDS,
    cdfKDC,
    cdfTimeServer,
    cdfClosest,
    cdfWritable,
    cdfGoodTimeServer,
    cdfNonDomainNC);

  /// the decoded TCldapDomainInfo.RawFlags content
  TCldapDomainFlags = set of TCldapDomainFlag;

  /// define the domain information returned by CldapGetDomainInfo(
  TCldapDomainInfo = record
    RawLogonType, RawFlags, NTVersion: cardinal;
    LogonType: TCldapDomainLogonType;
    Flags: TCldapDomainFlags;
    Guid: TGuid;
    Forest, Domain, HostName, NetbiosDomain, NetbiosHostname: RawUtf8;
    Unk, User, IP, ServerSite, ClientSite: RawUtf8;
  end;

/// send a CLDAP NetLogon message to a LDAP server over UDP to retrieve all
// information of the domain
function CldapGetDomainInfo(var Info: TCldapDomainInfo; TimeOutMS: integer;
  const DomainName, LdapServerAddress: RawUtf8;
  const LdapServerPort: RawUtf8 = LDAP_PORT): boolean;

/// retrieve the LDAP 'server:port' corresponding of a given AD Domain Name
// - will send CLDAP NetLogon messages to the known LDAP server(s) to retrieve
// TCldapDomainInfo.ClientSite then request the DNS for the LDAP of this site
// - this is the safest approach for a client, safer than CldapBroadcast()
// or CldapSortHosts() / DnsLdapControlersSorted()
// - as used with default lccCldap option for TLdapClient.Connect with a
// ForcedDomainName
function CldapGetLdapController(const DomainName: RawUtf8;
  const NameServer: RawUtf8 = ''; TimeOutMS: integer = 500): RawUtf8;

/// retrieve the LDAP 'server:port' corresponding to the running computer
// - will send CLDAP NetLogon messages to the known LDAP server(s) to retrieve
// TCldapDomainInfo.ClientSite then request the DNS for the LDAP of this site
// - if no NameServer is supplied, will use GetDnsAddresses - note that NameServer
// is expected to be an IPv4 address, maybe prefixed as 'tcp@1.2.3.4' to force TCP
// - this is the safest approach for a client, safer than CldapBroadcast()
// or CldapSortHosts() / DnsLdapControlersSorted()
// - as used with default lccCldap option for TLdapClient.Connect with no
// ForcedDomainName
function CldapMyLdapController(const NameServer: RawUtf8 = '';
  UsePosixEnv: boolean = false; DomainName: PRawUtf8 = nil;
  TimeOutMS: integer = 500): RawUtf8;

/// retrieve the default LDAP 'server:port'
// - if ForcedDomainName global variable is set, calls CldapGetLdapController()
// - otherwise, calls CldapMyLdapController()
// - can optionally return the associated Domain Name and Service Principal Name
// - as used by TLdapClient.Connect in lccCldap discover mode
function CldapGetDefaultLdapController(
  DN: PRawUtf8 = nil; SPN: PRawUtf8 = nil): RawUtf8;

/// pickup the preferred LDAP 'server:port' of a set of LDAP servers
// - will send CLDAP NetLogon messages to the LdapServers to retrieve
// TCldapDomainInfo.ClientSite then request the DNS for the LDAP of this site
// - as used by CldapGetLdapController() and CldapMyLdapController()
function CldapGetBestLdapController(const LdapServers: TRawUtf8DynArray;
  const DomainName, NameServer: RawUtf8; TimeOutMS: integer = 500): RawUtf8;

type
  /// define one result for a server identified by CldapBroadcast()
  TCldapServer = record
    /// after how many microseconds this response has been received
    TimeMicroSec: integer;
    /// the raw IP address where the UDP response came from
    IP: RawUtf8;
    /// the "dnsHostName" attribute returned by the server
    // - a typical value is e.g. 'dc-site3.ad.company.it'
    HostName: RawUtf8;
    /// the "ldapServiceName" attribute returned by the server
    // - a typical value is e.g. 'ad.company.it:dc-site3@AD.COMPANY.IT'
    ServiceName: RawUtf8;
    /// the "defaultNamingContext" attribute returned by the server
    // - a typical value is e.g. 'DC=ad,DC=company,DC=it'
    NamingContext: RawUtf8;
    /// the "vendorName" attribute returned by the server
    // - a typical value is e.g. 'Samba Team (https://www.samba.org)'
    VendorName: RawUtf8;
  end;
  /// the type of array results returned by CldapBroadcast()
  TCldapServers = array of TCldapServer;

/// allow to discover the local LDAP server(s) using a CLDAP UDP broadcast
// - will broadcast a CLDAP UDP search message over the local network,
// then return the results in receiving order (probably the closest first)
// - you can specify your broadcast address to check a specific network mask
// - some AD may be configured to drop and timeout so testing via TCP is
// unreliable: using UDP and CLDAP could be a safer approach
// - returns the number of results added to the Servers array[] - which will be
// sorted by time with any previous requests so you can call CldapBroadcast()
// over several address masks
// - note: cBroadcast = '255.255.255.255' does not mean "everywhere" in practice:
// e.g. on Windows, you need to run also the function on cLocalHost, and on
// POSIX it seems to require a specific broadcast per interface network mask
// otherwise only a single interface is broadcasted
// - is useful only for low-level forensic tools: to find out which LDAP
// client ot use, rather call CldapGetLdapController/CldapMyLdapController
function CldapBroadcast(var Servers: TCldapServers; TimeOutMS: integer = 100;
  const Address: RawUtf8 = cBroadcast; const Port: RawUtf8 = LDAP_PORT): integer;

/// sort some LDAP host names using CLDAP over UDP
// - expects Hosts in 'host:port' format, as returned by DnsLdapControlers,
// e.g. ['dc-one.mycorp.com:389', 'dc-two.mycorp.com:389']
// - hosts not available over UDP within MinimalUdpCount or the TimeoutMS period,
// are put at the end of the list because they may still be reachable via TCP
// - used e.g. by TLdapClient.Connect() with the lccClosest option
procedure CldapSortHosts(var Hosts: TRawUtf8DynArray;
  TimeoutMS, MinimalUdpCount: integer);

/// retrieve the LDAP controlers sorted by UDP response time
// - just a wrapper around DnsLdapControlers() and CldapSortHosts()
// - won't sort by UDP response time if UdpFirstDelayMS = 0
// - used e.g. by TLdapClient.Connect() with the lccClosest option
// - a safer approach may be to use CldapGetLdapController/CldapMyLdapController
function DnsLdapControlersSorted(UdpFirstDelayMS, MinimalUdpCount: integer;
  const NameServer: RawUtf8 = ''; UsePosixEnv: boolean = false;
  DomainName: PRawUtf8 = nil): TRawUtf8DynArray;



{ **************** LDAP Response Storage }

const
  // Well-Known LDAP Objects GUID
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-adts/
  //   5a00c890-6be5-4575-93c4-8bf8be0ca8d8
  LDAP_GUID_COMPUTERS                 = 'AA312825768811D1ADED00C04FD8D5CD';
  LDAP_GUID_DELETED_OBJECTS           = '18E2EA80684F11D2B9AA00C04F79F805';
  LDAP_GUID_DOMAIN_CONTROLLERS        = 'A361B2FFFFD211D1AA4B00C04FD7D83A';
  LDAP_GUID_FOREIGNSECURITYPRINCIPALS = '22B70C67D56E4EFB91E9300FCA3DC1AA';
  LDAP_GUID_INFRASTRUCTURE            = '2FBAC1870ADE11D297C400C04FD8D5CD';
  LDAP_GUID_LOSTANDFOUND              = 'AB8153B7768811D1ADED00C04FD8D5CD';
  LDAP_GUID_MICROSOFT_PROGRAM_DATA    = 'F4BE92A4C777485E878E9421D53087DB';
  LDAP_GUID_NTDS_QUOTAS               = '6227F0AF1FC2410D8E3BB10615BB5B0F';
  LDAP_GUID_PROGRAM_DATA              = '09460C08AE1E4A4EA0F64AEE7DAA1E5A';
  LDAP_GUID_SYSTEMS                   = 'AB1D30F3768811D1ADED00C04FD8D5CD';
  LDAP_GUID_USERS                     = 'A9D1CA15768811D1ADED00C04FD8D5CD';
  LDAP_GUID_MANAGED_SERVICE_ACCOUNTS  = '1EB93889E40C45DF9F0C64D23BBB6237';

type
  /// recognize some TLdapAttribute.AttributeName encoding
  TLdapAttributeType = (
    latUndefined,
    latObjectSid,
    latObjectGuid,
    latIsoTime,
    latFileTime);

/// recognize some non-textual attributes by name
// - as used by TLdapAttribute.Create
function AttributeNameType(const AttrName: RawUtf8): TLdapAttributeType;

/// convert a raw attribute value into human-readable text
// - as used by TLdapAttribute.GetReadable/GetAllReadable
procedure AttributeValueMakeReadable(var s: RawUtf8; lat: TLdapAttributeType);

type
  /// store a named LDAP attribute with the list of its values
  TLdapAttribute = class
  private
    fList: TRawByteStringDynArray;
    fAttributeName: RawUtf8;
    fCount: integer;
    fKnownType: TLdapAttributeType;
  public
    /// initialize the attribute(s) storage
    constructor Create(const AttrName: RawUtf8);
    /// include a new value to this list
    // - IsBinary values will be stored base-64 encoded
    procedure Add(const aValue: RawByteString);
    /// retrieve a value as human-readable text
    function GetReadable(index: PtrInt = 0): RawUtf8;
    /// retrieve all values as human-readable text
    function GetAllReadable: TRawUtf8DynArray;
    /// retrieve a value as its inital value stored with Add()
    // - return '' if the index is out of range, or the attribute is void
    function GetRaw(index: PtrInt = 0): RawByteString;
    /// how many values have been added to this attribute
    property Count: integer
      read fCount;
    /// access to the individual rwas
    // - note that length(List) = capacity - use Count property instead
    property List: TRawByteStringDynArray
      read fList;
    /// name of this LDAP attribute
    property AttributeName: RawUtf8
      read fAttributeName;
  end;
  /// dynamic array of LDAP attribute, as stored in TLdapAttributeList
  TLdapAttributeDynArray = array of TLdapAttribute;

  /// list one or several TLdapAttribute
  // - will use TLdapResultList.fInterning as hashed list of names to minimize
  // memory allocation, and makes efficient lookup
  TLdapAttributeList = class
  private
    fItems: TLdapAttributeDynArray;
    fInterning: TRawUtf8Interning; // global hash table of attribute names
    fLastFound: PtrInt;
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
    /// add an attribute value to the list, reusing any existing TLdapAttribute
    function FindAdd(const AttributeName: RawUtf8;
      const AttributeValue: RawByteString): TLdapAttribute;
    /// Find and return first attribute value with requested name
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    function Get(const AttributeName: RawUtf8): RawUtf8;
    /// access to the internal list of TLdapAttribute objects
    // - note that length(Items) = Count for this class
    property Items: TLdapAttributeDynArray
      read fItems;
  end;

  /// store one LDAP result, i.e. one object name and associated attributes
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
    property Attributes: TLdapAttributeList
      read fAttributes;
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
    fSearchTimeMicroSec: Int64;
    fCount: integer;
    fInterning: TRawUtf8Interning; // injected to TLdapResult by Add
  public
    /// initialize the result list
    constructor Create; reintroduce;
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
    /// return all Items[].Attributes.Get(AttributeName) as a sorted array
    function ObjectAttributes(const AttributeName: RawUtf8): TRawUtf8DynArray;
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
    /// the time elapsed in microseconds on client side
    // - including command sending, receiving and result parsing
    property SearchTimeMicroSec: Int64
      read fSearchTimeMicroSec;
  end;



{ **************** LDAP Client Class }

type
  ELdap = class(ESynException);

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
  PLdapKnownCommonNames = ^TLdapKnownCommonNames;
  TLdapKnownCommonNamesDual = array[boolean] of TLdapKnownCommonNames;

  /// the decoded fields of TLdapGroup.groupType
  // - https://learn.microsoft.com/en-us/windows/win32/adschema/a-grouptype
  TGroupType = (
    gtBuiltIn,
    gtGlobal,
    gtDomainLocal,
    gtUniversal,
    gtAppBasic,
    gtAppQuery,
    gtSecurity = 31);
  TGroupTypes = set of TGroupType;

  /// the decoded fields of TLdapUser.userAccountControl
  // - https://learn.microsoft.com/en-us/windows/win32/adschema/a-useraccountcontrol
  TUserAccountControl = (
    uacScript,                            //       1
    uacAccountDisable,                    //       2
    uacHomeDirRequired = 3,               //       8
    uacLockedOut,                         //      10
    uacPasswordNotRequired,               //      20
    uacPasswordCannotChange,              //      40
    uacPasswordUnencrypted,               //      80
    uacTempDuplicateAccount,              //     100
    uacNormalAccount,                     //     200
    uacInterDomainTrusted = 11,           //     800
    uacWorkstationTrusted,                //    1000
    uacServerTrusted,                     //    2000
    uacPasswordDonotExpire = 16,          //   10000
    uacLogonAccount,                      //   20000
    uacSmartcardRequired,                 //   40000
    uacKerberosTrustedForDelegation,      //   80000
    uacKerberosNotDelegated,              //  100000
    uacKerberosDesOnly,                   //  200000
    uacKerberosRequirePreAuth,            //  400000
    uacPasswordExpired,                   //  800000
    uacKerberosTrustedToDelegate,         // 1000000
    uacKerberosNoPac,                     // 2000000
    uacPartialSecretsRodc);               // 4000000
  TUserAccountControls = set of TUserAccountControl;

  /// high-level information of a User or Group object in the LDAP database
  TLdapObject = object
  public
    sAMAccountName, distinguishedName, canonicalName: RawUtf8;
    name, CN, description: RawUtf8;
    objectSid, objectGUID: RawUtf8;
    whenCreated, whenChanged: TDateTime;
    customNames: TRawUtf8DynArray;
    customValues: TRawUtf8DynArray;
    function Custom(const AttributeName: RawUtf8): RawUtf8;
    procedure Fill(Attributes: TLdapAttributeList;
      const CustomAttributes: TRawUtf8DynArray);
  end;

  /// high-level information of a Group in the LDAP database
  // - note that "member" array won't include nested groups - use rather the
  // TLdapClient.GetIsMemberOf() method or TLdapCheckMember class instead
  TLdapGroup = object(TLdapObject)
  public
    primaryGroupID: cardinal;
    groupType: TGroupTypes;
    member: TRawUtf8DynArray;
    procedure FillGroup(Attributes: TLdapAttributeList; WithMember: boolean;
      const CustomAttributes: TRawUtf8DynArray);
  end;

  /// high-level information of a User in the LDAP database
  // - inherit from TLdapGroup to share some common fields
  // - some of the fields are only populated if the User matches the logged one
  // - note that "memberof" array won't include nested groups - use rather the
  // TLdapClient.GetIsMemberOf() method or TLdapCheckMember class instead
  TLdapUser = object(TLdapObject)
  public
    userPrincipalName, displayName, mail: RawUtf8;
    pwdLastSet, lastLogon: TDateTime;
    memberof: TRawUtf8DynArray;
    userAccountControl: TUserAccountControls;
    primaryGroupID: cardinal;
    procedure FillUser(Attributes: TLdapAttributeList; WithMemberOf: boolean;
      const CustomAttributes: TRawUtf8DynArray);
  end;
  PLdapUser = ^TLdapUser;

  /// how TLdapClient.Connect try to find the LDAP server if no TargetHost is set
  // - default lccCldap will call CldapMyLdapController() to retrieve the
  // best possible LDAP server for this client
  // - lccClosest will make a round over the supplied addresses with a CLDAP
  // query over UDP, to find out the closest alive instances - also circumvent
  // if some AD were configured to drop and timeout more distant hosts
  // - default lccTlsFirst will try to connect as TLS on port 636 (if OpenSSL
  // is loaded)
  TLdapClientConnect = set of (
    lccCldap,
    lccClosest,
    lccTlsFirst);

  /// how a TLdapClient is connected to its associated LDAP Server
  TLdapClientTransmission = (
    lctNone,
    lctPlain,
    lctEncrypted);

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
    fAllowUnsafePasswordBind: boolean;
    function GetTargetUri: RawUtf8;
    procedure SetTargetUri(const uri: RawUtf8);
  public
    /// initialize this instance
    constructor Create(const aUri: RawUtf8 = ''); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// run Connect and Bind of a temporary TLdapClient over TargetHost/TargetPort
    // - don't validate the password, just TargetHost/TargetPort
    function CheckTargetHost: TLdapClientTransmission;
    /// try to setup the LDAP server information from the system
    // - use a temporary TLdapClient.Connect then optionally call BindSaslKerberos
    // - any existing KerberosDN and KerberosSpn will be used during discovery
    // - by default, will try CLDAP recognition, and favor a TLS connection
    function LoadDefaultFromSystem(TryKerberos: boolean;
      DiscoverMode: TLdapClientConnect = [lccCldap, lccTlsFirst];
      DelayMS: integer = 500): TLdapClientTransmission;
    /// raise ELdap if neither CheckTargetHost nor LoadDefaultFromSystem succeeded
    procedure ValidateTargetHostOrLoadDefault(TryKerberos: boolean = true;
      EnsureEncrypted: boolean = false);
    /// the 'ldap[s]://TargetHost:TargetPort[/KerberosDN]' human-readable URI
    // - reflecting the TargetHost, TargetPort, TLS and KerberosDN properties
    // - for safety, won't include the UserName/Password content
    // - a typical value is 'ldaps://dc-sitename.ad.company.it/ad.company.it'
    property TargetUri: RawUtf8
      read GetTargetUri write SetTargetUri;
  published
    /// target server IP (or symbolic name)
    // - default is '' but if not set, Connect will call DnsLdapControlers()
    // from mormot.net.dns to retrieve the current value from the system
    // - after connect, will contain the actual server name
    // - typical value is 'dc-one.mycorp.com'
    // - the Kerberos protocol expects the FQN to be specified, not the raw IP
    // address: you could use RegisterKnownHost() to pre-register a FQN instead
    // of adding it to the /etc/hosts file of the system
    property TargetHost: RawUtf8
      read fTargetHost write fTargetHost;
    /// target server port (or symbolic name)
    // - is '389' (LDAP_PORT) by default but could be '636' (LDAP_TLS_PORT or
    // sometimes '3269') on TLS
    property TargetPort: RawUtf8
      read fTargetPort write fTargetPort;
    /// if connection to the LDAP server is secured via TLS
    property Tls: boolean
      read fTls write fTls;
    /// by default, plain Bind with a password would require TLS
    // - you can set this property to TRUE to allow sending the password over
    // the wire, which is an unsafe pattern for sure
    // - won't affect BindSaslKerberos which hides the password during handshake
    property AllowUnsafePasswordBind: boolean
      read fAllowUnsafePasswordBind write fAllowUnsafePasswordBind;
    /// milliseconds timeout for socket operations
    // - default is 5000, ie. 5 seconds
    property Timeout: integer
      read fTimeout write fTimeout;
    /// if protocol needs user authorization, then fill here user name
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    // - with BindSaslKerberos, on Linux or Windows it should be 'username'
    // but on MacOS it should be 'username@ad.mycompany.tld'
    property UserName: RawUtf8
      read fUserName write fUserName;
    /// if protocol needs user authorization, then fill here its password
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    property Password: SpiUtf8
      read fPassword write fPassword;
    /// Kerberos Canonical Domain Name
    // - as set by Connect when TargetHost is empty
    // - can be pre-set before Connect if the system is not part of the domain
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
  // - is not thread-safe, but you can call Lock/UnLock to share the connection
  TLdapClient = class(TSynPersistentLock)
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
    fSecContextEncrypt: boolean;
    fSearchScope: TLdapSearchScope;
    fSearchAliases: TLdapSearchAliases;
    fSearchSizeLimit: integer;
    fSearchTimeLimit: integer;
    fSearchPageSize: integer;
    fSearchCookie: RawUtf8;
    fSearchResult: TLdapResultList;
    fExtName: RawUtf8;
    fExtValue: RawUtf8;
    fDefaultDN, fRootDN, fConfigDN: RawUtf8;
    fNetbiosDN: RawUtf8;
    fMechanisms: TRawUtf8DynArray;
    fSecContext: TSecContext;
    fSecContextUser: RawUtf8;
    fSockBuffer: RawByteString;
    fSockBufferPos: integer;
    fWellKnownObjects: TLdapKnownCommonNamesDual;
    fWellKnownObjectsCached: boolean;
    function BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
    procedure SendPacket(const Asn1Data: TAsnObject);
    procedure ReceivePacket(Dest: pointer; DestLen: integer); overload;
    procedure ReceivePacket(var Append: RawByteString; Len: PtrInt); overload;
    procedure ReceivePacketFillSockBuffer;
    function ReceiveResponse: TAsnObject;
    function DecodeResponse(var Pos: integer; const Asn1Response: TAsnObject): TAsnObject;
    function SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
    function RetrieveWellKnownObjects(const DN: RawUtf8;
      out Dual: TLdapKnownCommonNamesDual): boolean;
    procedure GetByAccountType(AT, Uac, unUac: integer;
      const BaseDN, CustomFilter, Match, AttributeName: RawUtf8;
      out Res: TRawUtf8DynArray);
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
    // DnsLdapControlers() hosts (from mormot.net.dns) following DiscoverMode
    // - do nothing if was already connected
    function Connect(DiscoverMode: TLdapClientConnect = [lccCldap, lccTlsFirst];
      DelayMS: integer = 500): boolean;
    /// the Root domain name of this LDAP server
    // - use an internal cache for fast retrieval
    function RootDN: RawUtf8;
    /// the Default domain name of this LDAP server
    // - is the same as RootDN when domain isn't a subdomain
    // - use an internal cache for fast retrieval
    function DefaultDN(const BaseDN: RawUtf8 = ''): RawUtf8;
    /// the Confirguration domain name of this LDAP server
    // - use an internal cache for fast retrieval
    function ConfigDN: RawUtf8;
    /// the NETBIOS domain name, empty string if not found
    // - use an internal cache for fast retrieval
    function NetbiosDN: RawUtf8;
    /// the authentication mechanisms supported on this LDAP server
    // - returns e.g. ['GSSAPI','GSS-SPNEGO','EXTERNAL','DIGEST-MD5']
    // - use an internal cache for fast retrieval
    function Mechanisms: TRawUtf8DynArray;
    /// search if the server supports a given authentication mechanism by name
    // - a typical value to search is e.g. 'GSSAPI' or 'DIGEST-MD5'
    function Supports(const MechanismName: RawUtf8): boolean;
    /// retrieve al well known object DN or CN as a single convenient record
    // - use an internal cache for fast retrieval
    function WellKnownObjects(AsCN: boolean = false): PLdapKnownCommonNames;
    /// authenticate a client to the directory server with Settings.Username/Password
    // - if these are empty strings, then it does annonymous binding
    // - warning: raise ELdap on plaintext transport of the password without a
    // safe TLS connection (unless Settings.AllowUnsafePasswordBind is set)
    function Bind: boolean;
    /// authenticate a client to the directory server with Settings.Username/Password
    // - uses DIGEST-MD5 as password obfuscation challenge - consider using TLS
    // - you can specify a stronger algorithm if DIGEST-MD5 is not strong enough
    // - seems not implemented by OpenLdap
    function BindSaslDigest(Algo: TDigestAlgo = daMD5_Sess): boolean;
    /// authenticate a client to the directory server using Kerberos
    // - if no Settings.UserName/Password has been set, will try current logged user
    // - uses GSSAPI and mormot.lib.gssapi/sspi to perform a safe authentication
    // - if no SPN is supplied, derivate one from Connect's DnsLdapControlers()
    // - can optionally return the KerberosUser which made the authentication
    function BindSaslKerberos(const AuthIdentify: RawUtf8 = '';
      KerberosUser: PRawUtf8 = nil): boolean;
    /// close the connection to the LDAP server, sending an Unbind message
    function Close: boolean;
    /// enable paging for the searches
    // - you can then loop calling Search() until it returns an empty result,
    // and eventually SearchEnd when done with this query
    // - is just a wrapper to set SearchPageSize
    procedure SearchBegin(PageSize: integer = 100);
    /// retrieve all entries that match a given set of criteria
    // - will generate as many requests/responses as needed to retrieve all
    // the information into the SearchResult property
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      const Filter: RawUtf8; const Attributes: array of RawUtf8): boolean;
    /// retrieve all entries that match a given set of criteria
    // - here the filter is generated using FormatUtf8()
    function SearchFmt(const BaseDN: RawUtf8; TypesOnly: boolean;
      const FilterFmt: RawUtf8; const FilterArgs: array of const;
      const Attributes: array of RawUtf8): boolean;
    /// finalize paging for the searches
    // - is just a wrapper to reset SearchPageSize and the SearchCookie
    procedure SearchEnd;
    /// retrieve all entries that match a given set of criteria and return the
    // first result
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if no result is found or if the search failed
    function SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
      const Attributes: array of RawUtf8): TLdapResult;
    /// retrieve the entry matching the given ObjectDN
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN, Filter: RawUtf8;
      const Attributes: array of RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapResult; overload;
    /// retrieve the entry matching the given ObjectDN and Attribute
    // - Returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN, Filter, Attribute: RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapAttribute; overload;
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
      DeleteIfPresent : boolean = false;
      UserAccount: TUserAccountControls = [uacWorkstationTrusted]): boolean;
    /// make one or more changes to the set of attribute values in an entry
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      Value: TLdapAttribute): boolean;
    /// change an entry DN
    // - it can be used to rename the entry (by changing its RDN), move it to a
    // different location in the DIT (by specifying a new parent entry), or both
    function ModifyDN(const obj, newRdn, newSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    ///  remove an entry from the directory server
    // - if the object has children and DeleteChildren is false, the deletion
    // will not work and the result will be LDAP_RES_NOT_ALLOWED_ON_NON_LEAF
    function Delete(const Obj: RawUtf8; DeleteChildren: boolean = false): boolean;
    /// determine whether a given entry has a specified attribute value
    function Compare(const Obj, AttributeValue: RawUtf8): boolean;
    /// call any LDAP v3 extended operations
    // - e.g. StartTLS, cancel, transactions
    function Extended(const Oid, Value: RawUtf8): boolean;
    /// retrieve all Group names in the LDAP Server
    // - you can refine your query via CustomFilter or TGroupTypes
    // - Match allow to search as a (AttributeName=Match) filter
    // - returns the sAMAccountName by default, but could be 'distinguishedName'
    function GetGroups(FilterUac: TGroupTypes = [];
      UnFilterUac: TGroupTypes = []; const Match: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''; const BaseDN: RawUtf8 = '';
      const AttributeName: RawUtf8 = 'sAMAccountName'): TRawUtf8DynArray;
    /// retrieve all User names in the LDAP Server
    // - you can refine your query via CustomFilter or TUserAccountControls
    // - Match allow to search as a (AttributeName=Match) filter
    // - returns the sAMAccountName by default, but could be 'distinguishedName'
    function GetUsers(FilterUac: TUserAccountControls = [];
      UnFilterUac: TUserAccountControls = [uacAccountDisable];
      const Match: RawUtf8 = ''; const CustomFilter: RawUtf8 = '';
      const BaseDN: RawUtf8 = '';
      const AttributeName: RawUtf8 = 'sAMAccountName'): TRawUtf8DynArray;
    /// retrieve the basic information of a LDAP Group
    // - could lookup by sAMAccountName or distinguishedName
    function GetGroupInfo(const AN, DN: RawUtf8; out Info: TLdapGroup;
      const BaseDN: RawUtf8 = ''; WithMember: boolean = false;
      const CustomAttributes: TRawUtf8DynArray = nil): boolean;
    /// retrieve the distinguishedName of a Group from its sAMAccountName
    function GetGroupDN(const AN: RawUtf8; const BaseDN: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''): RawUtf8;
    /// retrieve the last integer of a Group SID from its sAMAccountName
    // or distinguishedName
    // - could be used to check an user primaryGroupID attribute, which is
    // likely to not be part of the "member" array of the (e.g. domain) group
    function GetGroupPrimaryID(const AN, DN: RawUtf8; out PrimaryGroupID: cardinal;
      const BaseDN: RawUtf8 = ''; const CustomFilter: RawUtf8 = ''): boolean;
    /// retrieve the basic information of a LDAP User
    // - could lookup by sAMAccountName, distinguishedName or userPrincipalName
    function GetUserInfo(const AN, DN, UPN: RawUtf8; out Info: TLdapUser;
      const BaseDN: RawUtf8 = ''; WithMemberOf: boolean = false;
      const CustomAttributes: TRawUtf8DynArray = nil): boolean;
    /// retrieve the distinguishedName of a User from its sAMAccountName
    // or userPrincipalName
    // - can optionally return its primaryGroupID attribute, which is
    // likely to not be part of the "member" array of the (e.g. domain) group
    function GetUserDN(const AN, UPN: RawUtf8; const BaseDN: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''; PrimaryGroupID: PCardinal = nil;
      ObjectSid: PRawUtf8 = nil): RawUtf8;
    /// check if a User is registered as part of a group or its nested groups
    // - the UserDN could be retrieved from a GetUserDN() call
    // - the group is identified by sAMAccountName or distinguishedName
    // - Nested checks for the "member" field with the 1.2.840.113556.1.4.1941 flag
    // - won't check the user primaryGroupID which should be searched before,
    // as it is not part of the "member" array of the (e.g. domain) groups -
    // use rather TLdapCheckMember instead if you can
    function GetIsMemberOf(const UserDN, GroupAN, GroupDN: RawUtf8;
      const CustomFilter: RawUtf8 = ''; Nested: boolean = true;
      const BaseDN: RawUtf8 = ''): boolean; overload;
    /// check if a User is registered as part of some groups and their nested groups
    // - groups are identified by sAMAccountName or distinguishedName
    // - Nested checks for the "member" field with the 1.2.840.113556.1.4.1941 flag
    // - won't check the user primaryGroupID which should be searched before -
    // use rather TLdapCheckMember instead if you can
    // - can optionally return the sAMAccountName of matching groups of this user
    function GetIsMemberOf(const UserDN, CustomFilter: RawUtf8;
      const GroupAN, GroupDN: array of RawUtf8; Nested: boolean = true;
      const BaseDN: RawUtf8 = ''; GroupsAN: PRawUtf8DynArray = nil): boolean; overload;
    /// test whether the client is connected to the server
    // - if AndBound is set, it also checks that a successful bind request has been made
    function Connected(AndBound: boolean = true): boolean;
    /// test whether the client is connected with TLS or Kerberos Signing-Sealing
    // - it is unsafe to send e.g. a plain Password without lctEncrypted
    function Transmission: TLdapClientTransmission;
    /// true after a successful call to Bind or BindSaslKerberos
    property Bound: boolean
      read fBound;
    /// binary string of the last full response from LDAP server
    // - This string is encoded by ASN.1 BER encoding
    // - You need this only for debugging
    property FullResult: TAsnObject
      read fFullResult;
    /// optional advanced options for FullTls = true
    // - by default, IgnoreCertificateErrors is set to true by Create
    // - but you can change these default settings to validate the server
    // certificate if needed
    property TlsContext: TNetTlsContext
      read fTlsContext write fTlsContext;
    /// sequence number of the last LDAP command
    // - incremented with any LDAP command
    property Seq: integer
      read fSeq;
    /// the search scope used in search command
    property SearchScope: TLdapSearchScope
      read fSearchScope write fSearchScope;
    /// how to handle aliases in search command
    property SearchAliases: TLdapSearchAliases
      read fSearchAliases write fSearchAliases;
    /// result size limit in search command (bytes)
    // - 0 means without size limit
    property SearchSizeLimit: integer
      read fSearchSizeLimit write fSearchSizeLimit;
    /// search time limit in search command (seconds)
    // - 0 means without time limit
    property SearchTimeLimit: integer
      read fSearchTimeLimit write fSearchTimeLimit;
    /// number of results to return per search request
    // - default 0 means no paging
    // - you may rather call SearchBegin/SearchEnd wrapper functions
    // - note: if you expect a single result row, settting 1 won't necessary
    // reduce the data stream, because it would include an additional block with
    // a SearchCookie, and is likely to use more server resource for paging
    property SearchPageSize: integer
      read fSearchPageSize write fSearchPageSize;
    /// cookie returned by paged search results
    // - use an empty string for the first search request
    // - if not empty, you should call Search() again for the next page until it
    // is eventually empty
    // - you can force to an empty string to reset the pagination or for a new
    // Search()
    // - you may rather call SearchBegin/SearchEnd wrapper functions
    property SearchCookie: RawUtf8
      read fSearchCookie write fSearchCookie;
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

  /// a LDAP client instance dedicated to validate user group membership
  // - will maintain a connection to the LDAP server, e.g. to efficiently
  // implement a "search and bind" pattern for TBasicAuthServerExternal
  // - is able to automatically re-connect to the LDAP server if needed
  // - features an internal cache to avoid too recurrent LDAP server calls
  // - assume groups and users are on the same domain - i.e. connected to a LDAP
  // server on port 389, not on Global Catalog port (3268)
  // - is made thread-safe by using TLdapClient.Safe.Lock/UnLock
  // - mainly used e.g. by TBasicAuthServerExternal.CheckMember
  TLdapCheckMember = class(TLdapClient)
  protected
    fGroupAN, fGroupDN, fGroupIDAN: TRawUtf8DynArray;
    fGroupID: TIntegerDynArray;
    fUserBaseDN, fGroupBaseDN, fUserCustomFilter, fGroupCustomFilter: RawUtf8;
    fGroupNested: boolean;
    fLastConnectedTix32: cardinal;
    // naive but efficient O(n) cache of valid and invalid Users
    fCacheTimeoutSeconds: integer;
    fCacheOK, fCacheKO: TRawUtf8DynArray;
    fCacheOKGroupsAN: TRawUtf8DynArrayDynArray;
    fCacheOKCount, fCacheKOCount: integer;
    fCacheTimeoutTix: Int64;
    procedure CacheClear(tix: Int64);
  public
    /// initialize this inherited LDAP client instance
    constructor Create; overload; override;
    /// main entry point to check for a user membership
    // - will first ask the internal in-memory cache of previous requests
    // - ensure the LDAP is still connected, and re-connect if necessary
    // - call GetUserDN() to retrieve the user's distinguishedName and
    // primaryGroupID attributes from User sAMAccountName or userPrincipalName
    // - will check if primaryGroupID matches any registered groups SID, then
    // call GetIsMemberOf() to search for registered groups
    // - optionally return the matching sAMAccountName in GroupsAN[]
    function Authorize(const User: RawUtf8;
      GroupsAN: PRawUtf8DynArray = nil): boolean;
    /// main TOnAuthServer callback to check if a user is allowed to login
    // - just redirect to the Authorize() method
    function BeforeAuth(Sender: TObject; const User: RawUtf8): boolean;
    /// remove any previously allowed groups
    procedure AllowGroupClear;
    /// register the sAMAccountName of the allowed group(s)
    procedure AllowGroupAN(const GroupAN: TRawUtf8DynArray); overload;
    /// register the sAMAccountName of the allowed group(s) as CSV
    procedure AllowGroupAN(const GroupANCsv: RawUtf8); overload;
    /// register the distinguishedName of the allowed group(s)
    procedure AllowGroupDN(const GroupDN: TRawUtf8DynArray);
    /// allow to customize the BaseDN parameter used for GetUserDN()
    property UserBaseDN: RawUtf8
      read fUserBaseDN write fUserBaseDN;
    /// allow to customize the BaseDN parameter used for GetIsMemberOf()
    property GroupBaseDN: RawUtf8
      read fGroupBaseDN write fGroupBaseDN;
    /// allow to customize the custom filter parameter used for GetUserDN()
    property UserCustomFilter: RawUtf8
      read fUserCustomFilter write fUserCustomFilter;
    /// allow to customize the custom filter parameter used for GetIsMemberOf()
    property GroupCustomFilter: RawUtf8
      read fGroupCustomFilter write fGroupCustomFilter;
    /// allow to enable the 1.2.840.113556.1.4.1941 recursive search flag
    property GroupNested: boolean
      read fGroupNested write fGroupNested;
    /// after how many seconds the internal cache should be flushed
    // - default valucache timeout is 300 seconds, i.e. 5 minutes
    property CacheTimeoutSeconds: integer
      read fCacheTimeoutSeconds write fCacheTimeoutSeconds;
  end;


{ **************** HTTP BASIC Authentication via LDAP or Kerberos }

type
  /// abstract BASIC access via an external authentication service
  // - properly implemented by TBasicAuthServerKerberos and TBasicAuthServerLdap
  // - will maintain an internal in-memory cache of the latest valid credentials
  // as secured SHA3-256 hashes
  TBasicAuthServerExternal = class(TDigestAuthServerMem)
  protected
    fLog: TSynLogClass;
    fCheckMember: TLdapCheckMember;
    function ExternalServerAsk(const aUser: RawUtf8;
      const aPassword: SpiUtf8): boolean; virtual; abstract;
    procedure SetCheckMember(Value: TLdapCheckMember); virtual;
  public
    /// finalize this instance
    destructor Destroy; override;
    /// will ask the external server if some credentials are not already in cache
    // - as called from OnBasicAuth() callback
    // - you can also register manually some credentials for IDigestAuthServer
    function CheckCredential(const aUser: RawUtf8;
      const aPassword: SpiUtf8): TAuthServerResult; override;
    /// optionally implement the "search and bind" pattern
    // - supplied instance will be owned by this TBasicAuthServerExternal
    property CheckMember: TLdapCheckMember
      read fCheckMember write SetCheckMember;
    /// can add some logs to the LDAP client process
    property Log: TSynLogClass
      read fLog write fLog;
  end;

  /// BASIC access authentication engine via SSPI/GSSAPI Kerberos API
  // - allows Kerberos authentication from a HTTP client outside of the domain
  // - maintain an internal in-memory cache of the latest valid credentials
  // - could also be used outside of the HTTP BASIC authentication usecase to
  // check for user/name credential pairs over Kerberos, with a cache
  // - on Windows, it could use the Kerberos token to check for allowed groups
  // of the domain, without any TLdapCheckMember instance involved
  TBasicAuthServerKerberos = class(TBasicAuthServerExternal)
  protected
    fKerberosSpn: RawUtf8;
    {$ifdef OSWINDOWS}
    fGroupSid: RawSidDynArray;
    {$endif OSWINDOWS}
    function ExternalServerAsk(const aUser: RawUtf8;
      const aPassword: SpiUtf8): boolean; override;
  public
    /// initialize the BASIC access authentication engine via Kerberos
    // - you could use as SPN e.g. 'LDAP/ad.domain.com@DOMAIN.COM'
    // - if no SPN is supplied, will try with CldapGetDefaultLdapController()
    // - if aRealm is not set, will extract the Kerberos realm from the SPN
    // - the valid credentials will be cached by default for 5 minutes
    constructor Create(const aKerberosSpn: RawUtf8 = ''; const aRealm: RawUtf8 = '';
      aCacheTimeoutSec: integer = 5 * 60); reintroduce;
    /// low-level Kerberos authentication method via SSPI/GSSAPI (without cache)
    // - can optionally return the full Netbios user name
    // - as called by CheckCredential() if no match was found in cache
    function KerberosAsk(const aUser: RawUtf8; const aPassword: SpiUtf8;
      aFullUserName: PRawUtf8 = nil): boolean;
    {$ifdef OSWINDOWS}
    /// remove any previously allowed groups
    procedure AllowGroupClear;
    /// register the objectSid text of the allowed group(s)
    procedure AllowGroupBySid(const GroupSid: TRawUtf8DynArray); overload;
    /// register the objectSid text of the allowed group(s) as CSV
    procedure AllowGroupBySid(const GroupSidCsv: RawUtf8); overload;
    {$endif OSWINDOWS}
    /// the Kerberos Service Principal Name, as registered in domain
    // - e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
    property KerberosSpn: RawUtf8
      read fKerberosSpn;
  end;

  /// BASIC access authentication engine via a LDAP client
  // - allows for remote HTTP authentication to a server outside of the domain
  // - if the server is inside a domain, consider safer TBasicAuthServerKerberos
  // - will maintain an internal in-memory cache of the latest valid credentials
  // as secured SHA3-256 hashes
  // - IDigestAuthServer methods won't work as expected unless SetCredential()
  // or CheckCredential() have been explicitly called before, because LDAP's
  // digest fields are not compatible with HTTP's
  // - could also be used outside of HTTP BASIC authentication usecase to check
  // for user/name credential pairs on a LDAP server, with a cache
  TBasicAuthServerLdap = class(TBasicAuthServerExternal)
  protected
    fLdapSettings: TLdapClientSettings;
    function ExternalServerAsk(const aUser: RawUtf8;
      const aPassword: SpiUtf8): boolean; override;
  public
    /// initialize the BASIC access authentication engine via a LDAP client
    // - will try to connect to aLdapUri or the default LDAP server if none set
    // - if aRealm is not set, will use the Kerberos realm (e.g. 'ad.mycorp.com')
    // - the valid credentials will be cached by default for 5 minutes
    constructor Create(const aLdapUri: RawUtf8 = ''; const aRealm: RawUtf8 = '';
      aCacheTimeoutSec: integer = 5 * 60); reintroduce; overload;
    /// initialize the BASIC access authentication engine via a LDAP client
    // - will try to connect via aLdapSettings or the default LDAP server if none set
    // - supplied aLdapSettings will be owned by this instance
    constructor Create(const aRealm: RawUtf8; aLdapSettings: TLdapClientSettings;
      aCacheTimeoutSec: integer = 60 * 60); reintroduce; overload;
    /// finalize this instance
    destructor Destroy; override;
    /// access to the reference settings to access the LDAP server
    // - as set by Create from aLdapUri
    property LdapSettings: TLdapClientSettings
      read fLdapSettings;
  end;


implementation


{****** Support procedures and functions }

function SeparateRight(const Value: RawUtf8; Delimiter: AnsiChar): RawUtf8;
var
  x: PtrInt;
begin
  x := PosExChar(Delimiter, Value);
  if x = 0 then
    result := Value
  else
    result := copy(Value, x + 1, length(Value) - x);
end;

function SeparateRightU(const Value, Delimiter: RawUtf8): RawUtf8;
var
  x: PtrInt;
begin
  x := mormot.core.base.PosEx(Delimiter, Value);
  if x = 0 then
    result := Value
  else
    result := copy(Value, x + length(Delimiter), MaxInt); // no TrimCopy()
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
    result := ''; // nothing in-between
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
    ELdap.RaiseUtf8('Missing ending parenthesis in %', [Value]);
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

function TrimSPLeft(const S: RawUtf8): RawUtf8;
var
  i, l: PtrInt;
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
  i: PtrInt;
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
  result := GetFirstCsvItem(Value, Delimiter);
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


{ **************** LDAP Protocol Definitions }

function DNToCN(const DN: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
  dc, ou, cn, kind, value: RawUtf8;
begin
  result := '';
  p := pointer(DN);
  if p = nil then
    exit;
  repeat
    GetNextItemTrimed(p, '=', kind);
    GetNextItemTrimed(p, ',', value);
    if (kind = '') or
       (value = '') then
      ELdap.RaiseUtf8('DNToCN(%): invalid Distinguished Name', [DN]);
    if not PropNameValid(pointer(value)) then // simple alphanum is just fine
      value := LdapEscapeCN(LdapUnescape(value)); // may need some (un)escape
    LowerCaseSelf(kind);
    if kind = 'dc' then
    begin
      if dc <> '' then
        dc := dc + '.';
      dc := dc + value;
    end
    else if kind = 'ou' then
      Prepend(ou, ['/', value])
    else if kind = 'cn' then
      Prepend(cn, ['/', value]);
  until p = nil;
  result := dc + ou + cn;
end;

function RawLdapErrorString(ErrorCode: integer): RawUtf8;
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
    result := 'Unknown';
  end;
  result := FormatUtf8('% (#%)', [result, ErrorCode]);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function RawLdapTranslateFilter(const Filter: RawUtf8): TAsnObject;
var
  x: integer;
  c: Ansichar;
  dn: boolean;
  s, t, l, r, attr, rule: RawUtf8;
begin
  if Filter = '*' then
  begin
    result := Asn('*', ASN1_CTX7);
    exit;
  end;
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
      result := Asn(RawLdapTranslateFilter(GetBetween('(', ')', s)), ASN1_CTC2);
    '&':
      // and rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRightU(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          AsnAdd(result, RawLdapTranslateFilter(t));
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
          AsnAdd(result, RawLdapTranslateFilter(t));
        until s = '';
        result := Asn(result, ASN1_CTC1);
      end;
    else
      begin
        l := TrimU(GetFirstCsvItem(s, '='));
        r := SeparateRight(s, '=');
        if l <> '' then
        begin
          c := l[length(l)];
          case c of
            ':':
              // Extensible match
              begin
                SetLength(l, length(l) - 1);
                attr := '';
                rule := '';
                dn := false;
                if mormot.core.base.PosEx(':dn', l) > 0 then
                begin
                  dn := true;
                  l := StringReplaceAll(l, ':dn', '');
                end;
                attr := TrimU(GetFirstCsvItem(l, ':'));
                rule := TrimU(SeparateRight(l, ':'));
                if rule = l then
                  rule := '';
                if rule <> '' then
                  result := Asn(rule, ASN1_CTX1);
                if attr <> '' then
                  AsnAdd(result, attr, ASN1_CTX2);
                AsnAdd(result, UnescapeHex(r), ASN1_CTX3);
                if dn then // default is FALSE
                  AsnAdd(result, RawByteString(#$01#$ff), ASN1_CTX4);
                result := Asn(result, ASN1_CTC9);
              end;
            '~':
              // Approx match
              begin
                SetLength(l, length(l) - 1);
                result := Asn(ASN1_CTC8, [
                  Asn(l),
                  Asn(UnescapeHex(r))]);
              end;
            '>':
              // Greater or equal match
              begin
                SetLength(l, length(l) - 1);
                result := Asn(ASN1_CTC5, [
                   Asn(l),
                   Asn(UnescapeHex(r))]);
              end;
            '<':
              // Less or equal match
              begin
                SetLength(l, length(l) - 1);
                result := Asn(ASN1_CTC6, [
                   Asn(l),
                   Asn(UnescapeHex(r))]);
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
                  result := Asn(UnescapeHex(s), ASN1_CTX0);
                while r <> '' do
                begin
                  if PosExChar('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  AsnAdd(result, UnescapeHex(s), ASN1_CTX1);
                end;
                if r <> '' then
                  AsnAdd(result, UnescapeHex(r), ASN1_CTX2);
                result := Asn(ASN1_CTC4, [
                   Asn(l),
                   Asn(ASN1_SEQ, [result])]);
              end
              else
              begin
                // Equality match
                result := Asn(ASN1_CTC3, [
                   Asn(l),
                   Asn(UnescapeHex(r))]);
              end;
          end;
        end;
      end;
  end;
end;

function RawLdapSearch(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawUtf8;
  Scope: TLdapSearchScope; Aliases: TLdapSearchAliases;
  Sizelimit, TimeLimit: integer): TAsnObject;
var
  filt: RawUtf8;
begin
  if Filter = '' then
    Filter := '(objectclass=*)';
  filt := RawLdapTranslateFilter(Filter);
  if filt = '' then
    filt := Asn('', ASN1_NULL);
  result := Asn(LDAP_ASN1_SEARCH_REQUEST, [
              Asn(BaseDN),
              Asn(ord(Scope),   ASN1_ENUM),
              Asn(ord(Aliases), ASN1_ENUM),
              Asn(Sizelimit),
              Asn(TimeLimit),
              ASN1_BOOLEAN_VALUE[TypesOnly],
              filt,
              Asn(ASN1_SEQ, [AsnArr(Attributes)])]);
end;

function RawLdapSearchParse(const Response: TAsnObject; MessageId: integer;
  const Attributes: array of RawUtf8; const Values: array of PRawUtf8): boolean;
var
  i, a, asntype, seqend, setend: integer;
  name, value: RawUtf8;
begin
  result := false;
  if (length(Response) <= 5) or
     (length(Attributes) <> length(Values)) then
    exit;
  i := 1;
  if (AsnNext(i, Response) = ASN1_SEQ) and
     (AsnNextInteger(i, response, asntype) = MessageId) and
     (asntype = ASN1_INT) and
     (AsnNext(i, Response) = LDAP_ASN1_SEARCH_ENTRY) and
     (AsnNext(i, Response) = ASN1_OCTSTR) and
     (AsnNext(i, Response, nil, @seqend) = ASN1_SEQ) then
    while (i < seqend) and
          (AsnNext(i, Response) = ASN1_SEQ) and
          (AsnNext(i, Response, @name) = ASN1_OCTSTR) and
          (AsnNext(i, Response, nil, @setend) = ASN1_SETOF) do
    begin
      if AsnNext(i, Response, @value) = ASN1_OCTSTR then
      begin
        a := FindPropName(Attributes, name);
        if a >= 0 then
        begin
          if Values[a] <> nil then
            Values[a]^ := value;
          result := true; // at least one attribute = success
        end;
      end;
      i := {%H-}setend; // if several ASN1_OCTSTR are stored - but return first
    end;
end;

function LdapSafe(const Text: RawUtf8): boolean;
begin
  result := (Text = '') or
            ((StrLen(pointer(Text)) = length(Text)) and
             (PosCharAny(pointer(Text), '*()\') = nil));
end;

function LdapValidName(const Text: RawUtf8): boolean;
begin
  result := (Text <> '') and
            (PosExChar('*', Text) = 0) and
            (PosExChar('\', Text) = 0) and
            IsValidUtf8(Text);
end;

function LdapEscape(const Text: RawUtf8; KeepWildChar: boolean): RawUtf8;
begin
  if (Text = '') or
     PropNameValid(pointer(Text)) then
    result := Text // alphanum requires no escape nor memory allocation
  else
    result := EscapeHex(Text, LDAP_ESC[KeepWildChar], '\');
end;

function LdapEscapeName(const Text: RawUtf8; var Safe: RawUtf8): boolean;
begin
  result := LdapValidName(Text);
  if result then
    Safe := LdapEscape(Text, {KeepWildChar=}false);
end;

function LdapEscapeName(const Text: RawUtf8): RawUtf8;
begin
  if not LdapEscapeName(Text, result) then
    ELdap.RaiseUtf8('Invalid input name: %', [Text]);
end;

function LdapIsValidDistinguishedName(const Text: RawUtf8): boolean;
begin
  result := (Text <> '') and
            (PosExChar('*', Text) = 0) and // but allows \ within DN
            IsValidUtf8(Text);
end;

function LdapValidDistinguishedName(const Text: RawUtf8): RawUtf8;
begin
  if LdapIsValidDistinguishedName(Text) then
    result := Text // no escape of the DN value
  else
    ELdap.RaiseUtf8('Invalid distinguishedName: %', [Text]);
end;

function LdapUnescape(const Text: RawUtf8): RawUtf8;
begin
  result := UnescapeHex(Text, '\');
end;

function LdapEscapeCN(const Text: RawUtf8): RawUtf8;
begin
  if (Text = '') or
     PropNameValid(pointer(Text)) then
    result := Text // alphanum requires no escape nor memory allocation
  else
    result := EscapeChar(Text, LDAP_CN, '\');
end;


{ **************** CLDAP Client Functions }

const
  NTVER: RawByteString = #6#0#0#0; // '\00\00\00\06' does NOT work on CLDAP

function Random31: cardinal;
begin
  repeat
    result := Random32 shr 1;
  until result <> 0;
end;

function CldapGetDomainInfo(var Info: TCldapDomainInfo; TimeOutMS: integer;
  const DomainName, LdapServerAddress, LdapServerPort: RawUtf8): boolean;
var
  id, len: integer;
  i: PtrInt;
  filter, v: RawUtf8;
  req, response: RawByteString;
  addr, resp: TNetAddr;
  sock: TNetSocket;
  tmp: array[0..1999] of byte; // big enough for a UDP frame
begin
  FastRecordClear(@Info, TypeInfo(TCldapDomainInfo));
  result := false;
  if addr.SetFrom(LdapServerAddress, LdapServerPort, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
  try
    id := Random31;
    FormatUtf8('(&(DnsDomain=%)(NtVer=%))',
      [LdapEscapeName(DomainName), NTVER], filter);
    req := Asn(ASN1_SEQ, [
             Asn(id),
             RawLdapSearch('', false, filter, ['NetLogon'])
           ]);
    sock.SetReceiveTimeout(TimeOutMS);
    if sock.SendTo(pointer(req), length(req), addr) <> nrOK then
      exit;
    len := sock.RecvFrom(@tmp, SizeOf(tmp), resp);
    FastSetRawByteString(response, @tmp, len);
    if not RawLdapSearchParse(response, id, ['netlogon'], [@v]) then
      exit;
    Info.IP := addr.IPWithPort;
    Info.RawLogonType := PCardinalArray(v)[0];
    case Info.RawLogonType of
      23:
        Info.LogonType := cltAnonymous;
      25:
        Info.LogonType := cltUser;
    end;
    Info.RawFlags := PCardinalArray(v)[1];
    PWord(@Info.Flags)^ := Info.RawFlags;
    exclude(Info.Flags, cdfObsolete);
    Info.Guid := PGuid(@PCardinalArray(v)[2])^;
    i := DnsParseString(v, 24, Info.Forest);
    i := DnsParseString(v, i, Info.Domain);
    i := DnsParseString(v, i, Info.HostName);
    i := DnsParseString(v, i, Info.NetbiosDomain);
    i := DnsParseString(v, i, Info.NetbiosHostname);
    i := DnsParseString(v, i, Info.Unk);
    if Info.LogonType = cltUser then
      i := DnsParseString(v, i, Info.User);
    i := DnsParseString(v, i, Info.ServerSite);
    i := DnsParseString(v, i, Info.ClientSite);
    Info.NTVersion := PCardinal(@PByteArray(v)[i])^;
    result := (i + 4) < length(v);
  finally
    sock.Close;
  end;
end;

function CldapGetBestLdapController(const LdapServers: TRawUtf8DynArray;
  const DomainName, NameServer: RawUtf8; TimeOutMS: integer): RawUtf8;
var
  i: PtrInt;
  h, p, n: RawUtf8;
  info: TCldapDomainInfo;
  res: TRawUtf8DynArray;
begin
  for i := 0 to length(LdapServers) - 1 do
  begin
    Split(LdapServers[i], ':', h, p);
    if CldapGetDomainInfo(info, TimeOutMS, DomainName, h, p) and
       (info.ClientSite <> '') then
    begin
      FormatUtf8('_ldap._tcp.%._sites.%', [info.ClientSite, DomainName], n);
      res := DnsServices(n, NameServer);
      if res <> nil then
      begin
        result := res[0];
        exit;
      end;
    end;
  end;
  result := '';
end;

function CldapGetLdapController(const DomainName, NameServer: RawUtf8;
  TimeOutMS: integer): RawUtf8;
var
  ldap: TRawUtf8DynArray;
begin
  ldap := DnsLdapServices(DomainName, NameServer);
  result := CldapGetBestLdapController(ldap, DomainName, NameServer, TimeOutMS);
end;

function CldapMyLdapController(const NameServer: RawUtf8; UsePosixEnv: boolean;
  DomainName: PRawUtf8; TimeOutMS: integer): RawUtf8;
var
  ldap: TRawUtf8DynArray;
  dn: RawUtf8;
begin
  ldap := DnsLdapControlers(NameServer, UsePosixEnv, @dn);
  result := CldapGetBestLdapController(ldap, dn, NameServer, TimeOutMS);
  if (result <> '') and
     (DomainName <> nil) then
    DomainName^ := dn;
end;

function CldapGetDefaultLdapController(DN, Spn: PRawUtf8): RawUtF8;
var
  domain: RawUtf8;
begin
  if ForcedDomainName <> '' then
  begin
    domain := ForcedDomainName;
    result := CldapGetLdapController(ForcedDomainName, '', 500);
  end
  else
    result := CldapMyLdapController('', false, @domain);
  if result = '' then
    exit;
  if DN <> nil then
    DN^ := domain;
  if Spn <> nil then
    Spn^ := NetConcat(['LDAP/', Split(result, ':'), '@', UpperCase(domain)]);
end;

function CldapBroadcast(var Servers: TCldapServers; TimeOutMS: integer;
  const Address, Port: RawUtf8): integer;
var
  id: integer;
  req, response: RawByteString;
  addr, resp: TNetAddr;
  start, stop: Int64;
  sock: TNetSocket;
  len: PtrInt;
  v: TCldapServer;
  tmp: array[0..1999] of byte; // big enough for any UDP frame
begin
  result := 0;
  if addr.SetFrom(Address, Port, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
  try
    sock.SetBroadcast(true);
    id := Random31;
    req := Asn(ASN1_SEQ, [
             Asn(id),
             //Asn(''), // the RFC 1798 requires user, but MS AD does not :(
             RawLdapSearch('', false, '*', ['dnsHostName',
               'defaultNamingContext', 'ldapServiceName', 'vendorName'])
           ]);
    sock.SetReceiveTimeout(TimeOutMS);
    QueryPerformanceMicroSeconds(start);
    if sock.SendTo(pointer(req), length(req), addr) <> nrOK then
      exit;
    repeat
      len := sock.RecvFrom(@tmp, SizeOf(tmp), resp);
      if (len > 5) and
         (tmp[0] = ASN1_SEQ) then
      begin
        FastSetRawByteString(response, @tmp, len);
        if RawLdapSearchParse(response, id,
          ['dnsHostName', 'defaultNamingContext', 'ldapServiceName', 'vendorName'],
          [@v.HostName, @v.NamingContext, @v.ServiceName, @v.VendorName]) then
        begin
          QueryPerformanceMicroSeconds(stop);
          v.TimeMicroSec := stop - start;
          resp.IP(v.IP);
          SetLength(Servers, length(Servers) + 1);
          Servers[high(Servers)] := v;
          Finalize(v);
          inc(result);
        end;
      end;
    until len < 0; // stop at last recvfrom() timeout
  finally
    sock.Close;
  end;
  if (result <> 0) and
     (result <> length(Servers)) then
    // ensure results are sorted by TimeMicroSec: integer first field
    DynArray(TypeInfo(TCldapServers), Servers).Sort(SortDynArrayInteger);
end;

procedure CldapSortHosts(var Hosts: TRawUtf8DynArray;
  TimeoutMS, MinimalUdpCount: integer);
var
  sock: TNetSocketDynArray;
  h, p, v: RawUtf8;
  addr, resp: TNetAddr;
  req: TAsnObject;
  sorted: TRawUtf8DynArray;
  tix: Int64;
  n, i, r: PtrInt;
  len, found: integer;
  poll: TPollSocketAbstract;
  res: TPollSocketResults;
  tmp: array[0..1999] of byte; // big enough for a UDP frame
begin
  n := length(Hosts);
  if n = 0 then
    exit;
  found := 0;
  SetLength(sock, n);
  poll := PollFewSockets;
  try
    // multi-cast a simple LDAP request over UDP to all servers
    for i := 0 to n - 1 do
    begin
      Split(Hosts[i], ':', h, p);
      if p = '' then
        p := LDAP_PORT;
      if addr.SetFrom(h, p, nlUdp) <> nrOk then
        continue;
      sock[i] := addr.NewSocket(nlUdp);
      if sock[i] = nil then
        continue;
      sock[i].SetReceiveTimeout(1);
      req := Asn(ASN1_SEQ, [
               Asn(777 + i),
               RawLdapSearch('', false, '*', ['dnsHostName'])
             ]);
      if sock[i].SendTo(pointer(req), length(req), addr) = nrOk then
        poll.Subscribe(sock[i], [pseRead], i)
      else
      begin
        sock[i].Close;
        sock[i] := nil;
      end;
    end;
    // wait for the first incoming response(s)
    tix := GetTickCount64 + TimeoutMS;
    repeat
      if poll.WaitForModified(res, 10) then
        for r := 0 to res.Count - 1 do
        begin
          i := ResToTag(res.Events[r]);
          if (PtrUInt(i) >= PtrUInt(n)) or
             (sock[i] = nil) then
            continue; // paranoid
          len := sock[i].RecvFrom(@tmp, SizeOf(tmp), resp);
          if (len > 5) and
             (tmp[0] = ASN1_SEQ) then
          begin
            FastSetRawByteString(req, @tmp, len);
            if RawLdapSearchParse(req, 777 + i, ['dnsHostName'], [@v]) then
              AddRawUtf8(sorted, found, Hosts[i]); // found a true LDAP server
            poll.Unsubscribe(sock[i]); // some kind of server
            sock[i].Close;
            sock[i] := nil;
          end;
       end;
    until (found > MinimalUdpCount) or // stop as soon as we got enough host(s)
          (found = n) or               // or we got all hosts
          (GetTickCount64 > tix);      // or we timeout
  finally
    poll.Free;
    for i := 0 to n - 1 do
      if sock[i] <> nil then
      begin
        sock[i].Close;
        AddRawUtf8(sorted, found, Hosts[i]); // not via UDP, but maybe on TCP
      end;
  end;
  SetLength(sorted, found);
  if found <> n then // e.g. if sock[] creation failed
    for i := 0 to n - 1 do
      AddRawUtf8(sorted, hosts[i], {nodup=}true); // ensure eventually exist
  Hosts := sorted;
end;

function DnsLdapControlersSorted(UdpFirstDelayMS, MinimalUdpCount: integer;
  const NameServer: RawUtf8; UsePosixEnv: boolean;
  DomainName: PRawUtf8): TRawUtf8DynArray;
begin
  result := DnsLdapControlers(NameServer, UsePosixEnv, DomainName);
  if UdpFirstDelayMS > 0 then
    CldapSortHosts(result, UdpFirstDelayMS, MinimalUdpCount);
end;


{ **************** LDAP Response Storage }

const
  KNOWN_NAMES: array[0..11] of RawUtf8 = (
    'ObjectSid',                   // 0 latObjectSid
    'ObjectGUID',                  // 1 latObjectGuid
    'accountExpires',              // 2 latFileTime
    'badPasswordTime',             //      ..
    'lastLogon',                   //
    'lastLogonTimestamp',          //
    'lastLogoff',                  //
    'lockoutTime',                 //
    'pwdLastSet',                  //
    'ms-MCS-AdmPwdExpirationTime', // 9    ..
    'whenCreated',                 // 10 latIsoTime
    'whenChanged');                // 11   ..

function AttributeNameType(const AttrName: RawUtf8): TLdapAttributeType;
begin
  case FindPropName(@KNOWN_NAMES, AttrName, length(KNOWN_NAMES)) of
    0:
      result := latObjectSid;
    1:
      result := latObjectGuid;
    2 .. 9:
      result := latFileTime;
    10 .. 11:
      result := latIsoTime;
  else
    result := latUndefined;
  end;
end;

procedure AttributeValueMakeReadable(var s: RawUtf8; lat: TLdapAttributeType);
var
  qw: QWord;
  ts: TTimeLogBits;
  err: integer;
begin
  // handle the most recognized attribute names encoding
  case lat of
    latObjectSid:
      if IsValidRawSid(s) then
      begin
        s := SidToText(pointer(s));
        exit;
      end;
    latObjectGuid:
      if length(s) = SizeOf(TGuid) then
      begin
        s := ToUtf8(PGuid(s)^);
        exit;
      end;
    latFileTime:
      begin
        qw := GetQWord(pointer(s), err);
        if (err = 0) and
           (qw <> 0) then
        begin
          if qw >= $7FFFFFFFFFFFFFFF then
            s := 'Never expires'
          else
            s := UnixMSTimeToString(WindowsFileTime64ToUnixMSTime(qw));
          exit;
        end;
      end;
    latIsoTime:
      begin
        ts.From(pointer(s), length(s) - 3);
        if ts.Value <> 0 then
        begin
          s := ts.Text({expanded=}true); // normalize
          exit;
        end;
      end;
  end;
  // fallback to hexa if the input is not valid UTF-8 (as expected LDAP v3)
  if IsValidUtf8(s) then
    EnsureRawUtf8(s)
  else
    s := BinToHexLower(s);
end;


{ TLdapAttribute }

constructor TLdapAttribute.Create(const AttrName: RawUtf8);
begin
  inherited Create;
  fAttributeName := AttrName;
  fKnownType := AttributeNameType(AttrName);
  SetLength(fList, 1); // optimized for a single value (most used case)
end;

procedure TLdapAttribute.Add(const aValue: RawByteString);
begin
  AddRawUtf8(TRawUtf8DynArray(fList), fCount, aValue);
end;

function TLdapAttribute.GetReadable(index: PtrInt): RawUtf8;
begin
  if (self = nil) or
     (index >= fCount) then
    result := ''
  else
  begin
    result := fList[index];
    AttributeValueMakeReadable(result, fKnownType);
  end;
end;

function TLdapAttribute.GetAllReadable: TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) then
    exit;
  SetLength(result, fCount);
  for i := 0 to fCount - 1 do
    result[i] := GetReadable(i);
end;

function TLdapAttribute.GetRaw(index: PtrInt): RawByteString;
begin
  if (self = nil) or
     (PtrUInt(index) >= PtrUInt(fCount)) then
    result := ''
  else
    result := fList[index];
end;


{ TLdapAttributeList }

destructor TLdapAttributeList.Destroy;
begin
  ObjArrayClear(fItems);
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
var
  existing: pointer;
begin
  if (self <> nil) and
     (fItems <> nil) then
  begin
    result := length(fItems) - 1;
    if result = 0 then // very common case for single attribute lookup
    begin
      if fItems[0].AttributeName = AttributeName then
        exit;
    end
    else if (fLastFound <= result) and
            (fItems[fLastFound].AttributeName = AttributeName) then
    begin
      result := fLastFound; // match last Find()
      exit;
    end
    else
    begin
      existing := fInterning.Existing(AttributeName); // fast pointer search
      if existing <> nil then // no need to search if we know it won't be there
        for result := 0 to result do
          if pointer(fItems[result].AttributeName) = existing then
            exit;
    end;
  end;
  result := -1;
end;

function TLdapAttributeList.Find(const AttributeName: RawUtf8): TLdapAttribute;
var
  i: PtrInt;
begin
  i := FindIndex(AttributeName);
  if i >= 0 then
  begin
    fLastFound := i;
    result := fItems[i];
  end
  else
    result := nil;
end;

function TLdapAttributeList.Get(const AttributeName: RawUtf8): RawUtf8;
begin
  result := Find(AttributeName).GetReadable(0);
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8): TLdapAttribute;
begin
  result := TLdapAttribute.Create(fInterning.Unique(AttributeName));
  ObjArrayAdd(fItems, result);
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8;
  const AttributeValue: RawByteString): TLdapAttribute;
begin
  result := Add(AttributeName);
  result.Add(AttributeValue);
end;

function TLdapAttributeList.FindAdd(const AttributeName: RawUtf8;
  const AttributeValue: RawByteString): TLdapAttribute;
begin
  result := Find(AttributeName);
  if result = nil then
    result := Add(AttributeName); // first time this attribute is used
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
  objectSid := RawSidToText(Attributes.Find('objectSid').GetRaw);
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

constructor TLdapResultList.Create;
begin
  fInterning := TRawUtf8Interning.Create;
end;

destructor TLdapResultList.Destroy;
begin
  Clear;
  inherited Destroy;
  fInterning.Free;
end;

procedure TLdapResultList.Clear;
begin
  ObjArrayClear(fItems, fCount);
  fCount := 0;
  fSearchTimeMicroSec := 0;
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

function TLdapResultList.ObjectAttributes(const AttributeName: RawUtf8): TRawUtf8DynArray;
var
  i, n: PtrInt;
  a: RawUtf8;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) or
     (AttributeName = '') then
    exit;
  n := 0;
  SetLength(result, fCount);
  for i := 0 to fCount - 1 do
  begin
    a := fItems[i].Attributes.Get(AttributeName);
    if a = '' then
      continue;
    result[n] := a;
    inc(n);
  end;
  if n <> fCount then
    SetLength(result, n);
  QuickSortRawUtf8(result, n);
end;

procedure TLdapResultList.AfterAdd;
begin
  if fItems <> nil then
    DynArrayFakeLength(fItems, fCount);
end;

function TLdapResultList.Add: TLdapResult;
begin
  result := TLdapResult.Create;
  result.fAttributes.fInterning := fInterning;
  ObjArrayAddCount(fItems, result, fCount);
end;

function TLdapResultList.Dump: RawUtf8;
var
  i, j, k: PtrInt;
  res: TLdapResult;
  attr: TLdapAttribute;
  w: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  w := TTextWriter.CreateOwnedStream(tmp);
  try
    w.AddShort('results: ');
    w.Add(count);
    w.AddShorter(' in ');
    w.AddShort(MicroSecToString(fSearchTimeMicroSec));
    w.AddCR;
    for i := 0 to Count - 1 do
    begin
      res := Items[i];
      w.Add('%: %'#10, [i, DNToCN(res.ObjectName)]);
      w.Add('  objectName : %'#10, [res.ObjectName]);
      for j := 0 to res.Attributes.Count - 1 do
      begin
        attr := res.Attributes.Items[j];
        w.Add('  % : ', [attr.AttributeName]);
        if attr.Count <> 1 then
          w.AddCR;
        for k := 0 to attr.Count - 1 do
        begin
          if attr.Count <> 1 then
            w.AddShorter('    - ');
          w.AddString(attr.GetReadable(k));
          w.AddCR;
        end;
      end;
      w.AddCR;
    end;
    w.SetText(result);
  finally
    w.Free;
  end;
end;


{ **************** LDAP Client Class }

{ TLdapClientSettings }

constructor TLdapClientSettings.Create(const aUri: RawUtf8);
begin
  inherited Create;
  fTimeout := 5000;
  SetTargetUri(aUri); // initialize TargetHost/TargetPort and TLS
end;

destructor TLdapClientSettings.Destroy;
begin
  inherited Destroy;
  FillZero(fPassword);
end;

function TLdapClientSettings.CheckTargetHost: TLdapClientTransmission;
var
  test: TLdapClient;
begin
  result := lctNone;
  if (fTargetHost <> '') and
     (fTargetPort <> '') then
    try
      test := TLdapClient.Create(self);
      try
        if test.Bind then // connect and anonymous binding
        begin
          fTls := test.Sock.TLS.Enabled; // may have changed during Connect
          if fTls then
            result := lctEncrypted
          else
            result := lctPlain;
        end;
      finally
        test.Free;
      end;
    except
      result := lctNone;
    end;
end;

function TLdapClientSettings.LoadDefaultFromSystem(TryKerberos: boolean;
  DiscoverMode: TLdapClientConnect; DelayMS: integer): TLdapClientTransmission;
var
  test: TLdapClient;
begin
  result := lctNone;
  try
    test := TLdapClient.Create;
    try
      test.Settings.KerberosDN := fKerberosDN; // allow customization
      if not test.Connect(DiscoverMode, DelayMS) then
        exit;
      if TryKerberos then
      begin
        if test.Settings.KerberosSpn = '' then
          test.Settings.KerberosSpn := fKerberosSpn;
        if test.BindSaslKerberos then
          fKerberosSpn := test.Settings.KerberosSpn;
          // if Kerberos failed, continue anyway (may work with credentials)
      end;
      result := test.Transmission;
      CopyObject(test.Settings, self);
    finally
      test.Free;
    end;
  except
    result := lctNone;
  end;
end;

procedure TLdapClientSettings.ValidateTargetHostOrLoadDefault(
  TryKerberos, EnsureEncrypted: boolean);
var
  trans: TLdapClientTransmission;
begin
  if fTargetHost <> '' then
  begin
    // there are some known LDAP server parameters
    trans := CheckTargetHost;
    if trans = lctNone then
      ELdap.RaiseUtf8('%: invalid %', [self, GetTargetUri]);
  end
  else
  begin
    // guess the LDAP server from system information and CLDAP
    trans := LoadDefaultFromSystem(TryKerberos);
    if trans = lctNone then
      ELdap.RaiseUtf8('%: no default LDAP server', [self]);
  end;
  if EnsureEncrypted and
     (trans <> lctEncrypted) then
    ELdap.RaiseUtf8('%: no encryption on the wire', [self]);
end;

function TLdapClientSettings.GetTargetUri: RawUtf8;
begin
  result := '';
  if (self = nil) or
     (fTargetHost = '') then
    exit;
  result := NetConcat([LDAP_DEFAULT_SCHEME[fTls], fTargetHost]);
  if fTargetPort <> LDAP_DEFAULT_PORT[fTls] then
    result := NetConcat([result, ':', fTargetPort]);
  if fKerberosDN <> '' then
    result := NetConcat([result, '/', fKerberosDN]);
end;

procedure TLdapClientSettings.SetTargetUri(const uri: RawUtf8);
var
  u: TUri;
begin
  fTargetHost := '';
  fTargetPort := LDAP_PORT;
  fKerberosDN := '';
  fTls := false;
  if not u.From(uri, '0') then
    exit;
  if u.Scheme <> '' then
    if IdemPChar(pointer(u.Scheme), 'LDAP') then
      case u.Scheme[5] of
        #0:
          fTls := false;
        's', 'S':
          fTls := true;
      else
        exit;
      end
    else
      exit // not the ldap[s]:// scheme
  else if u.Port = LDAP_TLS_PORT then
    fTls := true; // no scheme:// means LDAP - force LDAPS on address:636
  fTargetHost := u.Server;
  if u.Port = '0' then
    u.Port := LDAP_DEFAULT_PORT[fTls];
  fTargetPort := u.Port;
  if u.Address = '' then
    exit;
  if u.Address[1] = '/' then
    delete(u.Address, 1, 1);
  fKerberosDN := u.Address;
end;



const
  AT_GROUP = $10000000; // 268435456
  AT_USER  = $30000000; // 805306368
  // https://theitbros.com/ldap-query-examples-active-directory/
  // https://social.technet.microsoft.com/wiki/contents/articles/5392.active-directory-ldap-syntax-filters.aspx

function InfoFilter(AT: cardinal; const AN, DN, UPN, CustomFilter: RawUtf8): RawUtf8;
begin
  result := '';
  if AN <> '' then
    FormatUtf8('(sAMAccountName=%)',
      [LdapEscapeName(AN)], result);
  if DN <> '' then
    result := FormatUtf8('%(distinguishedName=%)',
      [result, LdapValidDistinguishedName(DN)]); // no escape
  if UPN <> '' then
    result := FormatUtf8('%(userPrincipalName=%)',
      [result, LdapEscapeName(UPN)]);
  if result = '' then
  begin
    result := '(cn=)'; // return no answer whatsoever
    exit;
  end;
  if ord(AN <> '') + ord(DN <> '') + ord(UPN <> '') > 1 then
    result := FormatUtf8('(|%)', [result]);
  result := FormatUtf8('(&(sAMAccountType=%)%%)', [AT, result, CustomFilter]);
end;

function LdapToDate(const Text: RawUtf8): TDateTime;
begin
  if Text = 'Never expires' then
    result := 0
  else
    result := Iso8601ToDateTime(Text);
end;

{ TLdapObject }

procedure TLdapObject.Fill(Attributes: TLdapAttributeList;
  const CustomAttributes: TRawUtf8DynArray);
var
  n, c, i: PtrInt;
  a: TLdapAttribute;
begin
  sAMAccountName := Attributes.Get('sAMAccountName');
  distinguishedName := Attributes.Get('distinguishedName');
  canonicalName := DNToCN(distinguishedName);
  name := Attributes.Get('name');
  CN := Attributes.Get('cn');
  description := Attributes.Get('description');
  objectSid := Attributes.Get('objectSid');
  objectGUID := Attributes.Get('objectGUID');
  whenCreated := LdapToDate(Attributes.Get('whenCreated'));
  whenChanged := LdapToDate(Attributes.Get('whenChanged'));
  n := length(CustomAttributes);
  if n = 0 then
    exit;
  c := 0;
  for i := 0 to n - 1 do
  begin
    a := Attributes.Find(CustomAttributes[i]);
    if a = nil then
      continue;
    if c = 0 then
    begin
      dec(n, i);
      SetLength(customNames, n);
      SetLength(customValues, n);
    end;
    customNames[c] := a.AttributeName; // assign the interned name
    customValues[c] := a.GetReadable;
    inc(c);
  end;
  if c = 0 then
    exit;
  DynArrayFakeLength(customNames, c);
  DynArrayFakeLength(customValues, c);
end;

function TLdapObject.Custom(const AttributeName: RawUtf8): RawUtf8;
var
  i: PtrInt;
begin
  for i := 0 to length(customNames) - 1 do
    if customNames[i] = AttributeName then
    begin
      result := customValues[i];
      exit;
    end;
  result := '';
end;


{ TLdapGroup }

procedure TLdapGroup.FillGroup(Attributes: TLdapAttributeList; WithMember: boolean;
  const CustomAttributes: TRawUtf8DynArray);
var
  uac: integer;
begin
  Fill(Attributes, CustomAttributes);
  ToCardinal(SplitRight(objectSID, '-'), PrimaryGroupID);
  if ToInteger(Attributes.Get('groupType'), uac) then
    groupType := TGroupTypes(uac);
  if WithMember then
    member := Attributes.Find('member').GetAllReadable;
end;


{ TLdapUser }

procedure TLdapUser.FillUser(Attributes: TLdapAttributeList; WithMemberOf: boolean;
  const CustomAttributes: TRawUtf8DynArray);
var
  uac: integer;
begin
  Fill(Attributes, CustomAttributes);
  userPrincipalName := Attributes.Get('userPrincipalName');
  displayName := Attributes.Get('displayName');
  mail := Attributes.Get('mail');
  pwdLastSet := LdapToDate(Attributes.Get('pwdLastSet'));
  lastLogon := LdapToDate(Attributes.Get('lastLogon'));
  ToCardinal(Attributes.Get('primaryGroupID'), primaryGroupID);
  if WithMemberOf then
    memberOf := Attributes.Find('memberOf').GetAllReadable;
  if ToInteger(Attributes.Get('userAccountControl'), uac) then
    userAccountControl := TUserAccountControls(uac);
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
  Close;
  fSearchResult.Free;
  fReferals.Free;
  fSettings.Free;
  inherited Destroy;
end;

function TLdapClient.Connect(DiscoverMode: TLdapClientConnect;
  DelayMS: integer): boolean;
var
  dc: TRawUtf8DynArray;
  h, p: RawUtf8;
  i: PtrInt;
begin
  result := fSock <> nil;
  if result then
    exit; // socket was already connected
  fResultString := '';
  if fSettings.TargetHost = '' then
  begin
    // try all LDAP servers from OS list
    if ForcedDomainName = '' then
      ForcedDomainName := fSettings.KerberosDN; // may be pre-set
    if lccCldap in DiscoverMode then
    begin
      h := CldapGetDefaultLdapController(
        @fSettings.fKerberosDN, @fSettings.fKerberosSpn);
      if h <> '' then
        AddRawUtf8(dc, h);
    end;
    if dc = nil then
    begin
      if not (lccClosest in DiscoverMode) then
        DelayMS := 0; // disable CldapSortHosts()
      dc := DnsLdapControlersSorted(
        DelayMS, {MinimalUdpCount=}0, '', false, @fSettings.fKerberosDN);
    end;
  end
  else
    // try the LDAP server as specified in TLdapClient settings
    AddRawUtf8(dc, NetConcat([fSettings.TargetHost, ':', fSettings.TargetPort]));
  if dc = nil then
  begin
    fResultString := 'Connect: no TargetHost supplied';
    exit;
  end;
  fSeq := 0;
  for i := 0 to high(dc) do
    try
      Split(dc[i], ':', h, p);
      if fSettings.TargetHost = '' then // not from DnsLdapControlers
      begin
        if (lccTlsFirst in DiscoverMode) and
           HasOpenSsl and // SChannel seems to have troubles with LDAP TLS
           (p = LDAP_PORT) and
           not fSettings.Tls then
        try
          // first try to connect with TLS on its default port (much safer)
          fSock := TCrtSocket.Open(
            h, LDAP_TLS_PORT, nlTcp, fSettings.TimeOut, {tls=}true, @fTlsContext);
          p := LDAP_TLS_PORT;
        except
          on E: ENetSock do
            FreeAndNil(fSock); // no TLS support on this port
        end;
      end
      else
        if (p = LDAP_TLS_PORT) or // likely to be over TLS
           (p = '3269') then
          fSettings.Tls := true;
      if fSock = nil then
        // try connection to the server
        fSock := TCrtSocket.Open(
          h, p, nlTcp, fSettings.TimeOut, fSettings.Tls, @fTlsContext);
      result := fSock.SockConnected;
      if result then
      begin
        fSettings.TargetHost := h;
        fSettings.TargetPort := p;
        fSettings.Tls := fSock.TLS.Enabled;
        exit;
      end;
    except
      on E: Exception do
      begin
        FreeAndNil(fSock); // abort and try next dc[]
        FormatUtf8('Connect %: %', [E, E.Message], fResultString);
      end;
    end;
  if fResultString = '' then
    fResultString := 'Connect: failed';
end;

function TLdapClient.BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
begin
  inc(fSeq);
  result := Asn(ASN1_SEQ, [
              Asn(fSeq),
              Asn1Data
            ]);
  if not fSecContextEncrypt then
    exit;
  result := SecEncrypt(fSecContext, result);
  insert('0000', result, 1);
  PCardinal(result)^ := bswap32(length(result) - 4); // SASL Buffer Length
end;

function TLdapClient.NetbiosDN: RawUtf8;
begin
  if (fNetbiosDN = '') and
     fBound and
     fSock.SockConnected then
    fNetbiosDN := SearchObject('CN=Partitions,' + ConfigDN,
      FormatUtf8('(&(nETBIOSName=*)(nCName=%))',
        [DefaultDN]), 'nETBIOSName', lssWholeSubtree).GetReadable;
  result := fNetbiosDN;
end;

function TLdapClient.RootDN: RawUtf8;
begin
  if (fRootDN = '') and
     fSock.SockConnected then
    fRootDN := SearchObject('', '*', 'rootDomainNamingContext').GetReadable;
  result := fRootDN;
end;

function TLdapClient.DefaultDN(const BaseDN: RawUtf8): RawUtf8;
begin
  if BaseDN <> '' then
    result := BaseDN
  else
  begin
    if (fDefaultDN = '') and
       fSock.SockConnected then
      fDefaultDN := SearchObject('', '*', 'defaultNamingContext').GetReadable;
    result := fDefaultDN;
  end;
end;

function TLdapClient.ConfigDN: RawUtf8;
begin
  if (fConfigDN = '') and
     fSock.SockConnected then
    fConfigDN := SearchObject('', '*', 'configurationNamingContext').GetReadable;
  result := fConfigDN;
end;

function TLdapClient.Mechanisms: TRawUtf8DynArray;
begin
  if (fMechanisms = nil) and
     fSock.SockConnected then
    fMechanisms := SearchObject('', '*', 'supportedSASLMechanisms').GetAllReadable;
  result := fMechanisms;
end;

function TLdapClient.Supports(const MechanismName: RawUtf8): boolean;
var
  i: PtrInt;
begin
  result := Mechanisms <> nil;
  if result then
    for i := 0 to high(fMechanisms) do
      if PropNameEquals(fMechanisms[i], MechanismName) then
        exit;
  result := false;
end;

procedure TLdapClient.SendPacket(const Asn1Data: TAsnObject);
begin
  {$ifdef ASNDEBUG}
  {$I-} write('------'#10'Sending ');
  if fSecContextEncrypt then writeln('(encrypted) =') else writeln('=');
  writeln(AsnDump(Asn1Data));
  {$endif ASNDEBUG}
  if fSock <> nil then
    fSock.SockSendFlush(BuildPacket(Asn1Data));
end;

procedure TLdapClient.ReceivePacketFillSockBuffer;
var
  saslLen: integer;
  ciphered: RawByteString;
begin
  fSockBufferPos := 0;
  if fSecContextEncrypt then
  begin
    // through Kerberos encryption (sealing)
    saslLen := 0;
    fSock.SockRecv(@saslLen, 4);
    ciphered := fSock.SockRecv(bswap32(saslLen));
    fSockBuffer := SecDecrypt(fSecContext, ciphered);
  end
  else
  begin
    // get as much as possible unciphered data from socket
    fSockBuffer := fSock.SockReceiveString;
    if fSockBuffer = '' then
      ELdap.RaiseUtf8('%.ReceivePacket: no response from %:%',
        [self, fSettings.TargetHost, fSettings.TargetPort]);
  end;
  {$ifdef ASNDEBUG}
  writeln('Packet received bytes = ', length(fSockBuffer));
  {$endif ASNDEBUG}
end;

procedure TLdapClient.ReceivePacket(Dest: pointer; DestLen: integer);
var
  len: integer;
begin
  while DestLen > 0 do
  begin
    len := length(fSockBuffer) - fSockBufferPos;
    if len > 0 then
    begin
      // return what we need/can from fSockBuffer
      if len > DestLen then
        len := DestLen;
      MoveFast(PByteArray(fSockBuffer)[fSockBufferPos], Dest^, len);
      inc(fSockBufferPos, len);
      inc(PByte(Dest), len);
      dec(DestLen, len);
      if DestLen = 0 then
        exit;
    end;
    // fill fSockBuffer from fSock pending data
    ReceivePacketFillSockBuffer;
  end;
  // note: several SEQ messages may be returned
end;

procedure TLdapClient.ReceivePacket(var Append: RawByteString; Len: PtrInt);
var
  l: PtrInt;
begin
  l := length(Append);
  SetLength(Append, l + Len);
  ReceivePacket(@PByteArray(Append)[l], Len);
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
    // we need to decode the ASN.1 plain input to return a single SEQ message
    ReceivePacket(@b, 1); // ASN type
    if b <> ASN1_SEQ then
      exit;
    FastSetRawByteString(result, @b, 2);
    ReceivePacket(@b, 1); // first byte of ASN length
    PByteArray(result)[1] := b;
    if b > $7f then
      ReceivePacket(result, b and $7f); // $8x means x bytes of length
    // decode length of LDAP packet
    pos := 2;
    len := AsnDecLen(pos, result);
    // retrieve body of LDAP packet
    if len > 0 then
      ReceivePacket(result, len);
  except
    on Exception do
    begin
      result := '';
      exit;
    end;
  end;
  fFullResult := result;
  {$ifdef ASNDEBUG}
  {$I-} write('------'#10'Received ');
  if fSecContextEncrypt then writeln('(encrypted) =') else writeln('=');
  writeln(AsnDump(result));
  {$endif ASNDEBUG}
end;

// see https://ldap.com/ldapv3-wire-protocol-reference-ldap-result

function TLdapClient.DecodeResponse(
  var Pos: integer; const Asn1Response: TAsnObject): TAsnObject;
var
  x, asntype, seqend: integer;
  s, t: TAsnObject;
  errmsg: RawUtf8;
begin
  result := '';
  fResultCode := -1;
  fResultString := '';
  fResponseCode := LDAP_ASN1_ERROR;
  fResponseDN := '';
  if (AsnNext(Pos, Asn1Response) <> ASN1_SEQ) or
     (AsnNextInteger(Pos, Asn1Response, asntype) <> fSeq) or
     (asntype <> ASN1_INT) then
    exit;
  fResponseCode := AsnNext(Pos, Asn1Response, nil, @seqend);
  if fResponseCode in LDAP_ASN1_RESPONSES then
  begin
    // final response
    fResultCode := AsnNextInteger(Pos, Asn1Response, asntype);
    AsnNext(Pos, Asn1Response, @fResponseDN);   // matchedDN
    AsnNext(Pos, Asn1Response, @fResultString); // diagnosticMessage
    if not (fResultCode in LDAP_RES_NOERROR) then
    begin
      errmsg := RawLdapErrorString(fResultCode);
      if fResultString = '' then
        fResultString := errmsg
      else
        fResultString := FormatUtf8('% [%]', [errmsg, fResultString]);
    end;
    if fResultCode = LDAP_RES_REFERRAL then
      if AsnNext(Pos, Asn1Response, @s) = ASN1_CTC3 then
      begin
        x := 1;
        while x < length(s) do
        begin
          AsnNext(x, s, @t);
          fReferals.Add(t);
        end;
    end;
    result := copy(Asn1Response, Pos, length(Asn1Response) - Pos + 1); // body
    Pos := length(Asn1Response) + 1;
  end
  else
  begin
    // partial response (e.g. LDAP_ASN1_SEARCH_ENTRY)
    result := copy(Asn1Response, Pos, seqend - Pos);
    Pos := seqend;
  end;
end;

function TLdapClient.SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
var
  resp: RawByteString;
  x: integer;
begin
  fReferals.Clear;
  SendPacket(Asn1Data);
  resp := ReceiveResponse;
  x := 1;
  result := DecodeResponse(x, resp);
end;

// see https://ldap.com/ldapv3-wire-protocol-reference-bind

function TLdapClient.Bind: boolean;
begin
  result := false;
  if not Connect then
    exit;
  if (fSettings.Password <> '') and
     not fSettings.Tls and
     not fSettings.AllowUnsafePasswordBind then
    ELdap.RaiseUtf8('%.Bind with a password requires a TLS connection', [self]);
  SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                   Asn(fVersion),
                   Asn(fSettings.UserName),
                   Asn(fSettings.Password, ASN1_CTX0)]));
  result := fResultCode = LDAP_RES_SUCCESS;
  fBound := result;
end;

const
  DIGEST_ALGONAME: array[TDigestAlgo] of RawUtf8 = (
    '',                     // daUndefined
    '',                     // daMD5
    'DIGEST-MD5',           // daMD5_Sess
    '',                     // daSHA256
    'DIGEST-SHA-256',       // daSHA256_Sess
    '',                     // daSHA512_256
    'DIGEST-SHA-512-256',   // daSHA512_256_Sess
    '',                     // daSHA3_256
    '');                    // daSHA3_256_Sess (not part of Digest RFC yet)

function TLdapClient.BindSaslDigest(Algo: TDigestAlgo): boolean;
var
  x: integer;
  dig: RawUtf8;
  s, t, digreq: TAsnObject;
begin
  result := false;
  if fBound or
     not Connect then
    exit;
  if DIGEST_ALGONAME[Algo] = '' then
    ELdap.RaiseUtf8('Unsupported %.BindSaslDigest(%) algorithm',
      [self, DIGEST_NAME[Algo]]);
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
      AsnNext(x, s, @t);
      dig := DigestClient(Algo, t, '', 'ldap/' + LowerCaseU(fSock.Server),
        fSettings.UserName, fSettings.Password, 'digest-uri');
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

type
  // see https://www.rfc-editor.org/rfc/rfc4752#section-3.3
  TKerbSecLayer = set of (
    kslNone,
    kslIntegrity,
    kslConfidentiality);

const
  /// the bit-mask of the security layer to be used (if any wanted by the server)
  // - kslConfidentiality maps our SecEncrypt() wrapper scheme, i.e. conf_flag=1
  // (sign and seal), and not kslIntegrity (sign only)
  // - should match Samba expectations in its "strong auth = yes" default mode
  KLS_EXPECTED: TKerbSecLayer = [kslConfidentiality];

function TLdapClient.BindSaslKerberos(const AuthIdentify: RawUtf8;
  KerberosUser: PRawUtf8): boolean;
var
  datain, dataout: RawByteString;
  t, req1, req2: TAsnObject;
  needencrypt: boolean;
  seclayers: TKerbSecLayer;
  secmaxsize: integer;

  procedure ParseInput;
  var
    pos: integer;
  begin
    pos := 1;
    if (AsnNext(pos, t, @datain) = ASN1_CTC7) and  // CTX PRI 07 CTR
       (AsnNext(pos, t) = ASN1_CTC3) and           // CTX PRI 04 CTR
       (AsnNext(pos, t) = ASN1_OCTSTR) then
    begin
      // MS AD seems to encapsulate the binary in a non-standard shape
      AsnNext(pos, t, @datain);
      // MS AD seems to require frames encryption once bound
      needencrypt := true;
    end;
  end;

begin
  result := false;
  if fBound or
     not Connect then
    exit;
  if not InitializeDomainAuth then
  begin
    fResultString := 'Kerberos: Error initializing the library';
    exit;
  end;
  needencrypt := false;
  fSecContextUser := '';
  if (fSettings.KerberosSpn = '') and
     (fSettings.KerberosDN <> '') then
    fSettings.KerberosSpn := 'LDAP/' + fSettings.TargetHost + {noport}
                             '@' + UpperCase(fSettings.KerberosDN);
  req1 := Asn(LDAP_ASN1_BIND_REQUEST, [
            Asn(fVersion),
            Asn(''),
            Asn(ASN1_CTC3, [
              Asn('GSSAPI')])]);
  t := SendAndReceive(req1);
  if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
    exit;
  InvalidateSecContext(fSecContext, 0);
  try
    repeat
      ParseInput;
      if (datain = '') and
         (fResultCode = LDAP_RES_SUCCESS) then
        break;
      try
        if fSettings.UserName <> '' then
          ClientSspiAuthWithPassword(fSecContext, datain, fSettings.UserName,
            fSettings.Password, fSettings.KerberosSpn, dataout)
        else
          ClientSspiAuth(fSecContext, datain, fSettings.KerberosSpn, dataout);
      except
        on E: Exception do
        begin
          FormatUtf8('Kerberos %: %', [E, E.Message], fResultString);
          // keep ResultCode = LDAP_RES_SASL_BIND_IN_PROGRESS (14)
          exit; // catch SSPI/GSSAPI errors and return false
        end;
      end;
      if dataout = '' then
      begin
        // last step of SASL handshake - see RFC 4752 section 3.1
        if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
          break; // perform if only needed (e.g. not on MS AD)
        t := SendAndReceive(req1);
        if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
        begin
          if fResultCode = LDAP_RES_SUCCESS then // paranoid
            fResultString := 'Kerberos: aborted SASL handshake';
          exit;
        end;
        ParseInput;
        datain := SecDecrypt(fSecContext, datain);
        if length(datain) <> 4 then
        begin
          fResultString := 'Kerberos: Unexpected SecLayer response';
          exit; // expected format is #0=SecLayer #1#2#3=MaxMsgSizeInNetOrder
        end;
        seclayers := TKerbSecLayer(datain[1]);
        secmaxsize := bswap32(PCardinal(datain)^) and $00ffffff;
        if seclayers = [] then
          // the server requires no additional security layer
          if secmaxsize <> 0 then
          begin
            // invalid answer (as stated by RFC 4752)
            FormatUtf8('Kerberos: Unexpected secmaxsize=%',
              [secmaxsize], fResultString);
            exit;
          end
          else
          begin
            // #0: noseclayer, #1#2#3: maxmsgsize=0
            PCardinal(datain)^ := 0;
            needencrypt := false; // fSecContextEncrypt = false by default
          end
        else if seclayers * KLS_EXPECTED = [] then
        begin
          // we only support signing+sealing
          FormatUtf8('Kerberos: Unsupported [%] method(s)',
            [GetSetName(TypeInfo(TKerbSecLayer), seclayers)], fResultString);
          exit;
        end
        else
        // if we reached here, the server asked for signing+sealing
        if needencrypt or             // from MS AD
           not fSock.TLS.Enabled then // ldap_require_strong_auth on OpenLDAP
        begin
          // return the supported algorithm, with a 64KB maximum message size
          if secmaxsize > 64 shl 10 then
            secmaxsize := 64 shl 10;
            // note: calling gss_wrap_size_limit is pointless due to 16bit limit
          PCardinal(datain)^ := bswap32(secmaxsize) + byte(KLS_EXPECTED);
          needencrypt := true; // fSecContextEncrypt = true = sign and seal
        end
        else
          // needed for OpenLDAP over TLS to avoid LDAP_RES_UNWILLING_TO_PERFORM
          PCardinal(datain)^ := 0;
        if AuthIdentify <> '' then
          Append(datain, AuthIdentify);
        dataout := SecEncrypt(fSecContext, datain);
      end;
      req2 := Asn(LDAP_ASN1_BIND_REQUEST, [
                Asn(fVersion),
                Asn(''),
                Asn(ASN1_CTC3, [
                  Asn('GSSAPI'),
                  Asn(dataout)])]);
      t := SendAndReceive(req2);
    until not (fResultCode in [LDAP_RES_SUCCESS, LDAP_RES_SASL_BIND_IN_PROGRESS]);
    result := fResultCode = LDAP_RES_SUCCESS;
    if result then
    begin
      ServerSspiAuthUser(fSecContext, fSecContextUser);
      if KerberosUser <> nil then
        KerberosUser^ := fSecContextUser;
    end;
    fBound := result;
    fSecContextEncrypt := needencrypt;
  finally
    if not result then
    begin
      fSecContextEncrypt := false;
      FreeSecContext(fSecContext);
    end;
  end;
end;


// https://ldap.com/ldapv3-wire-protocol-reference-unbind

function TLdapClient.Close: boolean;
begin
  result := true;
  if fSock.SockConnected then
    try
      SendPacket(Asn('', LDAP_ASN1_UNBIND_REQUEST));
    except
      result := false;
    end;
  FreeAndNil(fSock);
  if fSecContextEncrypt then
    FreeSecContext(fSecContext);
  fSecContextEncrypt := false;
  fBound := false;
  fRootDN := '';
  fDefaultDN := '';
  fConfigDN := '';
  fWellKnownObjectsCached := false;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Value: TLdapAttribute): boolean;
var
  query: TAsnObject;
  i: integer;
begin
  for i := 0 to Value.Count -1 do
    AsnAdd(query, Asn(Value.List[i]));
  SendAndReceive(Asn(LDAP_ASN1_MODIFY_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(ASN1_SEQ, [
                       Asn(ord(Op), ASN1_ENUM),
                       Asn(ASN1_SEQ, [
                         Asn(Value.AttributeName),
                         Asn(query, ASN1_SETOF)
                       ])
                     ])
                   ])
                 ]));
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
      AsnAdd(sub, Asn(attr.List[j]));
    Append(query,
      Asn(ASN1_SEQ, [
        Asn(attr.AttributeName),
        Asn(ASN1_SETOF, [sub])
      ]));
  end;
  SendAndReceive(Asn(LDAP_ASN1_ADD_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, query)]));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

function TLdapClient.AddComputer(const ComputerParentDN, ComputerName: RawUtf8;
  out ErrorMessage: RawUtf8; const Password: SpiUtf8; DeleteIfPresent: boolean;
  UserAccount: TUserAccountControls): boolean;
var
  PwdU8: SpiUtf8;
  ComputerSafe, ComputerDN, ComputerSam: RawUtf8;
  PwdU16: RawByteString;
  Attributes: TLdapAttributeList;
  ComputerObject: TLdapResult;
begin
  result := false;
  if not Connected or
     not LdapEscapeName(ComputerName, ComputerSafe) then
    exit;
  ComputerDN := NetConcat(['CN=', ComputerSafe, ',', ComputerParentDN]);
  ComputerSam := NetConcat([UpperCase(ComputerSafe), '$']);
  // Search Computer object in the domain
  ComputerObject := SearchFirst(DefaultDN,
    FormatUtf8('(sAMAccountName=%)', [ComputerSam]), ['']);
  // If the search failed, we exit with the error message
  if ResultCode <> LDAP_RES_SUCCESS then
  begin
    ErrorMessage := FormatUtf8('Search failed: %', [RawLdapErrorString(ResultCode)]);
    exit;
  end;
  // Computer with the same sAMAccountName is already existing
  if Assigned(ComputerObject) then
  begin
    result := true;
    // We don't want to delete it
    if not DeleteIfPresent then
    begin
      ErrorMessage := 'Computer is already present';
      exit;
    end;
    result := Delete(ComputerObject.ObjectName);
    // Unable to delete the computer (probably insufficient access rights)
    if not result then
    begin
      ErrorMessage := FormatUtf8('Delete failed: %', [RawLdapErrorString(ResultCode)]);
      exit;
    end;
  end;
  // Create the new computer object
  Attributes := TLDAPAttributeList.Create;
  try
    Attributes.Add('objectClass', 'computer');
    Attributes.Add('cn', ComputerSafe);
    Attributes.Add('sAMAccountName', ComputerSam);
    Attributes.Add('userAccountControl', UInt32ToUtf8(cardinal(UserAccount)));
    if Password <> '' then
    begin
      PwdU8 := NetConcat(['"', Password, '"']);
      PwdU16 := Utf8DecodeToUnicodeRawByteString(PwdU8);
      Attributes.Add('unicodePwd', PwdU16);
      FillZero(PwdU8);
      FillZero(PwdU16);
    end;
    result := Add(ComputerDN, Attributes);
    if not result then
      ErrorMessage := FormatUtf8('Add failed: %', [RawLdapErrorString(ResultCode)]);
  finally
    Attributes.Free;
    FillZero(PwdU8);
    FillZero(PwdU16);
  end;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-delete

function TLdapClient.Delete(const Obj: RawUtf8; DeleteChildren: boolean): boolean;
var
  PreviousSearchScope: TLdapSearchScope;
  Children: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := false;
  if not Connected then
    exit;
  PreviousSearchScope := SearchScope;
  SendAndReceive(Asn(Obj, LDAP_ASN1_DEL_REQUEST));
  if (fResultCode = LDAP_RES_NOT_ALLOWED_ON_NON_LEAF) and
     DeleteChildren then
    // Obj had children and DeleteChildren is True
    try
      SearchScope := lssSingleLevel;
      Search(Obj, false, '', []);
      Children := fSearchResult.ObjectNames;
      for i := 0 to high(Children) do
        if not Delete(Children[i], {DeleteChildren=}true) then
          break; // stop on error
      if fResultCode = LDAP_RES_SUCCESS then
        // retry Obj deletion after children have been successfully removed
        SendAndReceive(Asn(Obj, LDAP_ASN1_DEL_REQUEST));
    finally
      SearchScope := PreviousSearchScope;
    end;
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
  Append(query, [Asn(newRdn), ASN1_BOOLEAN_VALUE[DeleteOldRdn]]);
  if newSuperior <> '' then
    AsnAdd(query, Asn(newSuperior, ASN1_CTX0));
  SendAndReceive(Asn(query, LDAP_ASN1_MODIFYDN_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-compare

function TLdapClient.Compare(const Obj, AttributeValue: RawUtf8): boolean;
begin
  result := false;
  if not Connected(False) then
    exit;
  SendAndReceive(Asn(LDAP_ASN1_COMPARE_REQUEST, [
                   Asn(obj),
                   Asn(ASN1_SEQ, [
                     Asn(TrimU(GetFirstCsvItem(AttributeValue, '='))),
                     Asn(TrimU(SeparateRight(AttributeValue, '=')))
                   ])
                 ]));
  result := fResultCode = LDAP_RES_COMPARE_TRUE;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

const
  ASN1_OID_PAGEDRESULTS = '1.2.840.113556.1.4.319';

function TLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  const Filter: RawUtf8; const Attributes: array of RawUtf8): boolean;
var
  s, resp, packet: TAsnObject;
  start, stop: Int64;
  u: RawUtf8;
  x, n, seqend: integer;
  r: TLdapResult;
  a: TLdapAttribute;
begin
  result := false;
  if not fSock.SockConnected then
    exit;
  QueryPerformanceMicroSeconds(start);
  fSearchResult.Clear;
  fReferals.Clear;
  s := RawLdapSearch(BaseDN, TypesOnly, Filter, Attributes, fSearchScope,
    fSearchAliases, fSearchSizeLimit, fSearchTimeLimit);
  if fSearchPageSize > 0 then // https://www.rfc-editor.org/rfc/rfc2696
    Append(s, Asn(
        Asn(ASN1_SEQ, [
           Asn(ASN1_OID_PAGEDRESULTS), // controlType: pagedresultsControl
           ASN1_BOOLEAN_VALUE[false],  // criticality: false
           Asn(Asn(ASN1_SEQ, [
                 Asn(fSearchPageSize),
                 Asn(fSearchCookie)
               ]))
        ]), LDAP_ASN1_CONTROLS));
  SendPacket(s);
  x := 1;
  repeat
    if x >= length(packet) then
    begin
      packet := ReceiveResponse;
      x := 1;
    end;
    resp := DecodeResponse(x, packet);
    case fResponseCode of
      LDAP_ASN1_SEARCH_DONE:
        break;
      LDAP_ASN1_SEARCH_ENTRY:
        begin
          r := fSearchResult.Add;
          n := 1;
          AsnNext(n, resp, @r.ObjectName);
          if AsnNext(n, resp) = ASN1_SEQ then
          begin
            while n < length(resp) do
            begin
              if AsnNext(n, resp, nil, @seqend) = ASN1_SEQ then
              begin
                AsnNext(n, resp, @u);
                a := r.Attributes.Add(u);
                if AsnNext(n, resp) = ASN1_SETOF then
                  while n < seqend do
                  begin
                    AsnNext(n, resp, @u);
                    a.Add(u);
                  end;
              end;
            end;
          end;
        end;
      LDAP_ASN1_SEARCH_REFERENCE:
        begin
          n := 1;
          while n < length(resp) do
          begin
            AsnNext(n, resp, @u);
            fReferals.Add(u);
          end;
        end;
    else
      exit; // unexpected block
    end;
  until false;
  n := 1;
  if AsnNext(n, resp) = LDAP_ASN1_CONTROLS then
    if AsnNext(n, resp) = ASN1_SEQ then
    begin
      AsnNext(n, resp, @s);
      if s = ASN1_OID_PAGEDRESULTS then
      begin
        AsnNext(n, resp, @s); // searchControlValue
        n := 1;
        if AsnNext(n, s) = ASN1_SEQ then
        begin
          // total number of result records, if known, otherwise 0
          AsnNext(n, s);
          // active search cookie, empty when done
          AsnNext(n, s, @fSearchCookie);
        end;
      end;
    end;
  fSearchResult.AfterAdd; // allow "for res in ldap.SearchResult.Items do"
  result := fResultCode = LDAP_RES_SUCCESS;
  QueryPerformanceMicroSeconds(stop);
  fSearchResult.fSearchTimeMicroSec := stop - start;
end;

function TLdapClient.SearchFmt(const BaseDN: RawUtf8; TypesOnly: boolean;
  const FilterFmt: RawUtf8; const FilterArgs: array of const;
  const Attributes: array of RawUtf8): boolean;
begin
  result := Search(BaseDN, TypesOnly, FormatUtf8(FilterFmt, FilterArgs), Attributes);
end;

procedure TLdapClient.SearchBegin(PageSize: integer);
begin
  SearchCookie := '';
  SearchPageSize := PageSize;
end;

procedure TLdapClient.SearchEnd;
begin
  SearchBegin(0);
end;

function TLdapClient.SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
  const Attributes: array of RawUtf8): TLdapResult;
begin
  result := nil;
  if Search(BaseDN, false, Filter, Attributes) and
     (SearchResult.Count > 0) then
    result := SearchResult.Items[0];
end;

function TLdapClient.SearchObject(const ObjectDN, Filter: RawUtf8;
  const Attributes: array of RawUtf8; Scope: TLdapSearchScope): TLdapResult;
var
  prev: TLdapSearchScope;
begin
  prev := SearchScope;
  try
    SearchScope := Scope;
    result := SearchFirst(ObjectDN, Filter, Attributes);
  finally
    SearchScope := prev;
  end;
end;

function TLdapClient.SearchObject(const ObjectDN, Filter, Attribute: RawUtf8;
  Scope: TLdapSearchScope): TLdapAttribute;
var
  root: TLdapResult;
begin
  result := nil;
  root := SearchObject(ObjectDN, Filter, [Attribute], Scope);
  if root <> nil then
    result := root.Attributes.Find(Attribute);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-extended
function TLdapClient.Extended(const Oid, Value: RawUtf8): boolean;
var
  query, decoded: TAsnObject;
  pos: integer;
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
    AsnNext(pos, decoded, @fExtName);
    AsnNext(pos, decoded, @fExtValue);
  end;
end;

const
  // https://learn.microsoft.com/en-us/windows/win32/adsi/search-filter-syntax
  AND_FLAG = ':1.2.840.113556.1.4.803:';
  NESTED_FLAG: array[boolean] of RawUtf8 = (
    '', ':1.2.840.113556.1.4.1941:');

procedure TLdapClient.GetByAccountType(AT, Uac, unUac: integer;
  const BaseDN, CustomFilter, Match, AttributeName: RawUtf8;
  out Res: TRawUtf8DynArray);
var
  f, filter, uacname: RawUtf8;
begin
  if AT = AT_USER then
    uacname := 'userAccountControl'
  else
    uacname := 'groupType';
  unUac := unUac and (not Uac); // FilterUac has precedence over FilterUnUac
  f := CustomFilter;
  if Uac <> 0 then
    f := FormatUtf8('(%%=%)%', [uacname, AND_FLAG, Uac, f]);
  if unUac <> 0 then
    f := FormatUtf8('(!(%%=%))%', [uacname, AND_FLAG, unUac, f]);
  if Match <> '' then // allow * wildchar in Match - but escape others
    f := FormatUtf8('%(%=%)', [f, AttributeName, LdapEscape(Match, {keep*=}true)]);
  FormatUtf8('(sAMAccountType=%)', [AT], filter);
  if f <> '' then
    filter := FormatUtf8('(&%%)', [filter, f]);
  if Search(DefaultDN(BaseDN), false, filter, [AttributeName]) and
     (SearchResult.Count > 0) then
    Res := SearchResult.ObjectAttributes(AttributeName);
end;

function TLdapClient.GetGroups(FilterUac, UnFilterUac: TGroupTypes;
  const Match, CustomFilter, BaseDN, AttributeName: RawUtf8): TRawUtf8DynArray;
begin
  GetByAccountType(AT_GROUP, integer(FilterUac), integer(UnFilterUac),
    BaseDN, CustomFilter, Match, AttributeName, result);
end;

function TLdapClient.GetUsers(FilterUac, UnFilterUac: TUserAccountControls;
  const Match, CustomFilter, BaseDN, AttributeName: RawUtf8): TRawUtf8DynArray;
begin
  GetByAccountType(AT_USER, integer(FilterUac), integer(UnFilterUac),
    BaseDN, CustomFilter, Match, AttributeName, result);
end;

const
  // TLdapObject attributes
  LDAPOBJECT_ATTR = 'sAMAccountName,distinguishedName,name,cn,description,' +
    'objectSid,objectGUID,whenCreated,whenChanged';

function TLdapClient.GetGroupInfo(const AN, DN: RawUtf8;
  out Info: TLdapGroup; const BaseDN: RawUtf8; WithMember: boolean;
  const CustomAttributes: TRawUtf8DynArray): boolean;
var
  attr: TRawUtf8DynArray;
begin
  FastRecordClear(@Info, TypeInfo(TLdapGroup));
  attr := CsvToRawUtf8DynArray(LDAPOBJECT_ATTR + ',groupType');
  if WithMember then
    AddRawUtf8(attr, 'member');
  AddRawUtf8(attr, CustomAttributes);
  result := Search(DefaultDN(BaseDN), false,
              InfoFilter(AT_GROUP, AN, DN, '', ''), attr) and
            (SearchResult.Count = 1);
  if result then
    Info.FillGroup(SearchResult.Items[0].Attributes, WithMember, CustomAttributes);
end;

function TLdapClient.GetGroupDN(const AN, BaseDN, CustomFilter: RawUtf8): RawUtf8;
begin
  if Search(DefaultDN(BaseDN), false,
       InfoFilter(AT_GROUP, AN, '', '', CustomFilter), ['distinguishedName']) and
     (SearchResult.Count = 1) then
    result := SearchResult.Items[0].Attributes.Get('distinguishedName')
  else
    result := '';
end;

function TLdapClient.GetGroupPrimaryID(const AN, DN: RawUtf8;
  out PrimaryGroupID: cardinal; const BaseDN, CustomFilter: RawUtf8): boolean;
var
  last: RawUtf8;
begin
  result := false;
  if Search(DefaultDN(BaseDN), false,
       InfoFilter(AT_GROUP, AN, DN, '', CustomFilter), ['objectSid']) and
     (SearchResult.Count = 1) then
  begin
    last := SplitRight(SearchResult.Items[0].Attributes.Get('objectSid'), '-');
    result := (last <> '') and
              ToCardinal(last, PrimaryGroupID);
  end;
end;

function TLdapClient.GetUserInfo(const AN, DN, UPN: RawUtf8;
  out Info: TLdapUser; const BaseDN: RawUtf8; WithMemberOf: boolean;
  const CustomAttributes: TRawUtf8DynArray): boolean;
var
  attr: TRawUtf8DynArray;
begin
  FastRecordClear(@Info, TypeInfo(TLdapUser));
  attr := CsvToRawUtf8DynArray(LDAPOBJECT_ATTR + ',userPrincipalName,' +
    'displayName,mail,pwdLastSet,lastLogon,userAccountControl,primaryGroupID');
  if WithMemberOf then
    AddRawUtf8(attr, 'memberOf');
  AddRawUtf8(attr, CustomAttributes);
  result := Search(DefaultDN(BaseDN), false,
              InfoFilter(AT_USER, AN, DN, UPN, ''), attr) and
            (SearchResult.Count = 1);
  if result then
    Info.FillUser(SearchResult.Items[0].Attributes, WithMemberOf, CustomAttributes);
end;

function TLdapClient.GetUserDN(const AN, UPN, BaseDN, CustomFilter: RawUtf8;
  PrimaryGroupID: PCardinal; ObjectSid: PRawUtf8): RawUtf8;
begin
  if Search(DefaultDN(BaseDN), false,
       InfoFilter(AT_USER, AN, '', UPN, CustomFilter),
       ['distinguishedName', 'primaryGroupID', 'objectSid']) and
     (SearchResult.Count = 1) then
    with SearchResult.Items[0].Attributes do
    begin
      result := Get('distinguishedName');
      if PrimaryGroupID <> nil then
        ToCardinal(Get('primaryGroupID'), PrimaryGroupID^);
      if ObjectSid <> nil then
        ObjectSid^ := Get('objectSid');
    end
    else
      result := '';
end;

function TLdapClient.GetIsMemberOf(
  const UserDN, GroupAN, GroupDN, CustomFilter: RawUtf8;
  Nested: boolean; const BaseDN: RawUtf8): boolean;
begin
  result := GetIsMemberOf(UserDN, CustomFilter, [GroupAN], [GroupDN], Nested);
end;

function TLdapClient.GetIsMemberOf(const UserDN, CustomFilter: RawUtf8;
  const GroupAN, GroupDN: array of RawUtf8; Nested: boolean;
  const BaseDN: RawUtf8; GroupsAN: PRawUtf8DynArray): boolean;
var
  user, grp, filter: RawUtf8;
  i, n: PtrInt;
begin
  result := false;
  if not LdapEscapeName(UserDN, user) then
    exit;
  n := 0;
  for i := 0 to high(GroupAN) do
    if GroupAN[i] <> '' then
      if LdapEscapeName(GroupAN[i], grp) then
      begin
        filter := FormatUtf8('%(sAMAccountName=%)', [filter, grp]);
        inc(n);
      end
      else
        exit;
  for i := 0 to high(GroupDN) do
    if GroupDN[i] <> '' then
      if LdapIsValidDistinguishedName(GroupDN[i]) then
      begin
        filter := FormatUtf8('%(distinguishedName=%)', [filter, GroupDN[i]]);
        inc(n); // no escape of the DN content
      end
      else
        exit;
  if n = 0 then
    exit; // we need at least one valid name to compare to
  if n > 1 then
    filter := FormatUtf8('(|%)', [filter]); // OR operator
  filter := FormatUtf8('(&(sAMAccountType=%)%%(member%=%))',
    [AT_GROUP, filter, CustomFilter, NESTED_FLAG[Nested], user]);
  if Search(DefaultDN(BaseDN), false, filter, ['sAMAccountName']) and
     (SearchResult.Count > 0) then
  begin
    if GroupsAN <> nil then
      GroupsAN^ := SearchResult.ObjectAttributes('sAMAccountName');
    result := true;
  end;
end;

function TLdapClient.Connected(AndBound: boolean): boolean;
begin
  result := fSock.SockConnected;
  if result and
     AndBound then
    result := fBound;
end;

function TLdapClient.Transmission: TLdapClientTransmission;
begin
  if (self = nil) or
     not fSock.SockConnected then
    result := lctNone
  else if fSettings.Tls or
          fSecContextEncrypt then
    result := lctEncrypted
  else
    result := lctPlain;
end;

function TLdapClient.RetrieveWellKnownObjects(const DN: RawUtf8;
  out Dual: TLdapKnownCommonNamesDual): boolean;
var
  tmp: TRawUtf8DynArray;
  i: PtrInt;

  function One(const guid: RawUtf8): RawUtf8;
  var
    i: PtrInt;
    p: PUtf8Char;
  begin
    for i := 0 to high(tmp) do
    begin
      p := pointer(tmp[i]);
      if (p <> nil) and
         NetStartWith(p + 5, pointer(guid)) then
        begin
          result := copy(tmp[i], Length(guid) + 7, MaxInt);
          tmp[i] := ''; // no need to search this one any more
          exit;
        end;
    end;
    result := '';
  end;

begin
  result := false;
  if not Connected or
     (DN = '') then
    exit;
  tmp := SearchObject(DN, '', 'wellKnownObjects').GetAllReadable;
  if tmp = nil then
    exit;
  result := true;
  for i := 0 to high(tmp) do
    if not NetStartWith(pointer(tmp[i]), 'B:32:') then
      tmp[i] := '';
  with Dual[{asCN=}false] do
  begin
    Computers                 := One(LDAP_GUID_COMPUTERS);
    DeletedObjects            := One(LDAP_GUID_DELETED_OBJECTS);
    DomainControllers         := One(LDAP_GUID_DOMAIN_CONTROLLERS);
    ForeignSecurityPrincipals := One(LDAP_GUID_FOREIGNSECURITYPRINCIPALS);
    Infrastructure            := One(LDAP_GUID_INFRASTRUCTURE);
    LostAndFound              := One(LDAP_GUID_LOSTANDFOUND);
    MicrosoftProgramData      := One(LDAP_GUID_MICROSOFT_PROGRAM_DATA);
    NtdsQuotas                := One(LDAP_GUID_NTDS_QUOTAS);
    ProgramData               := One(LDAP_GUID_PROGRAM_DATA);
    Systems                   := One(LDAP_GUID_SYSTEMS);
    Users                     := One(LDAP_GUID_USERS);
    ManagedServiceAccounts    := One(LDAP_GUID_MANAGED_SERVICE_ACCOUNTS);
  end;
  with Dual[{asCN=}true] do
  begin
    Computers                 := DNToCn(Dual[false].Computers);
    DeletedObjects            := DNToCn(Dual[false].DeletedObjects);
    DomainControllers         := DNToCn(Dual[false].DomainControllers);
    ForeignSecurityPrincipals := DNToCn(Dual[false].ForeignSecurityPrincipals);
    Infrastructure            := DNToCn(Dual[false].Infrastructure);
    LostAndFound              := DNToCn(Dual[false].LostAndFound);
    MicrosoftProgramData      := DNToCn(Dual[false].MicrosoftProgramData);
    NtdsQuotas                := DNToCn(Dual[false].NtdsQuotas);
    ProgramData               := DNToCn(Dual[false].ProgramData);
    Systems                   := DNToCn(Dual[false].Systems);
    Users                     := DNToCn(Dual[false].Users);
    ManagedServiceAccounts    := DNToCn(Dual[false].ManagedServiceAccounts);
  end;
end;

function TLdapClient.WellKnownObjects(AsCN: boolean): PLdapKnownCommonNames;
begin
  if not fWellKnownObjectsCached then
    if RetrieveWellKnownObjects(DefaultDN, fWellKnownObjects) then
        fWellKnownObjectsCached := true;
  result := @fWellKnownObjects[AsCN];
end;


{ TLdapCheckMember }

// checking group membership on a LDAP server is really a complex task
// - so a dedicated TLdapCheckMember class is worth the effort :)
// - we follow the pattern as implemented in the best reference code we found:
// https://www.gabescode.com/active-directory/2018/09/13/one-user-is-member-of-a-group.html
// - since we are likely to connect to a LDAP over port 389, we won't access the
// Global Catalog on port 3268, so we assume that the group and the user are
// part of the very same domain: we can skip the "Foreign Security Principals"
// part of the article, and can quickly check for primaryGroupId membership
// by comparing the last SID block/integer only

constructor TLdapCheckMember.Create;
begin
  inherited Create;
  fGroupNested := true;
  fCacheTimeoutSeconds := 300; // 5 minutes
end;

procedure TLdapCheckMember.CacheClear(tix: Int64);
begin
  fCacheOK := nil;
  fCacheOKCount := 0;
  fCacheKO := nil;
  fCacheKOCount := 0;
  fCacheTimeoutTix := tix + fCacheTimeoutSeconds * 1000;
end;

function TLdapCheckMember.BeforeAuth(Sender: TObject;
  const User: RawUtf8): boolean;
begin
  result := Authorize(User);
end;

function TLdapCheckMember.Authorize(const User: RawUtf8;
  GroupsAN: PRawUtf8DynArray): boolean;
var
  tix: Int64;
  userpid: cardinal;
  fromcachendx, primaryidndx: PtrInt;
  userdn: RawUtf8;
  groups: TRawUtf8DynArray;
begin
  result := false;
  if ((fGroupAN = nil) and
      (fGroupDN = nil) and
      (fGroupID = nil)) or
     not LdapValidName(User) then
    exit;
  tix := GetTickCount64;
  if not fBound and
     (fLastConnectedTix32 = tix shr 12) then
    exit; // too soon to retry re-connect (quick exit outside of the lock)
  try
    fSafe.Lock;
    try
      // first check from cache
      fromcachendx := -1;
      if fCacheTimeoutSeconds <> 0 then
        if (tix > fCacheTimeoutTix) or
           (fCacheOKCount > 1000) or
           (fCacheKOCount > 1000) then
          CacheClear(tix)
        else
        begin
          fromcachendx := FindPropName(pointer(fCacheOK), User, fCacheOKCount);
          if fromcachendx >= 0 then
          begin
            result := true;
            if GroupsAN = nil then
              exit;
            GroupsAN^ := fCacheOKGroupsAN[fromcachendx];
            if GroupsAN^ <> nil then
              exit; // we did get the groups in a previous call
          end
          else if FindPropName(pointer(fCacheKO), User, fCacheKOCount) >= 0 then
            exit;
        end;
      // re-create the connection to the LDAP server if needed
      if not Connected({andbound=}true) then
        if fLastConnectedTix32 <> tix shr 12 then
        begin
          fLastConnectedTix32 := tix shr 12; // retry every 4 seconds only
          if fSettings.KerberosSpn <> '' then
            BindSaslKerberos
          else if fSettings.Password = '' then
            exit // anonymous binding would fail the search for sure
          else
            Bind;
        end
        else
          exit; // too soon to retry
      // call the LDAP server to actually check user membership
      userdn := GetUserDN(User, User, fUserBaseDN, fUserCustomFilter, @userpid);
      if userdn <> '' then
      begin
        if userpid = 0 then
          primaryidndx := -1
        else
          primaryidndx := IntegerScanIndex(pointer(fGroupID), length(fGroupID), userpid);
        if primaryidndx >= 0 then
          result := true;
        if (fromcachendx >= 0) or
           (GroupsAN <> nil) or
           not result then
        begin
          if GetIsMemberOf(userdn, fGroupCustomFilter,
              fGroupAN, fGroupDN, fGroupNested, fGroupBaseDN, @groups) then
            result := true;
          if primaryidndx >= 0 then
            AddRawUtf8(groups, fGroupIDAN[primaryidndx]);
        end;
      end;
      // actualize the internal cache
      if result then
      begin
        if fromcachendx < 0 then
        begin
          fromcachendx := AddRawUtf8(fCacheOK, fCacheOKCount, User);
          if length(fCacheOKGroupsAN) <> length(fCacheOK) then
            SetLength(fCacheOKGroupsAN, length(fCacheOK)); // grow capacity
        end;
        fCacheOKGroupsAN[fromcachendx] := groups;
        if GroupsAN <> nil then
          GroupsAN^ := groups;
      end
      else
        AddRawUtf8(fCacheKO, fCacheKOCount, User)
    finally
      fSafe.UnLock;
    end;
  except
    on Exception do
      begin
        // there was an error connecting with the LDAP server
        result := false; // assume failed
        Close;  // but will try to reconnect
      end;
  end;
end;

procedure TLdapCheckMember.AllowGroupClear;
begin
  fSafe.Lock;
  try
    fGroupAN := nil;
    fGroupDN := nil;
    fGroupID := nil;
    fGroupIDAN := nil;
    CacheClear(GetTickCount64);
  finally
    fSafe.UnLock;
  end;
end;

procedure TLdapCheckMember.AllowGroupAN(const GroupAN: TRawUtf8DynArray);
var
  i: PtrInt;
  pid: cardinal;
begin
  fSafe.Lock;
  try
    for i := 0 to high(GroupAN) do
      if GetGroupPrimaryID(
           GroupAN[i], '', pid, fGroupBaseDN, fGroupCustomFilter) then
      begin
        if AddInteger(fGroupID, pid, {nodup=}true) then
          AddRawUtf8(fGroupIDAN, GroupAN[i]);
        AddRawUtf8(fGroupAN, GroupAN[i], {nodup=}true, {casesens=}false);
      end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TLdapCheckMember.AllowGroupAN(const GroupANCsv: RawUtf8);
var
  g: TRawUtf8DynArray;
begin
  CsvToRawUtf8DynArray(pointer(GroupANCsv), g, ',', {trim=}true);
  AllowGroupAN(g);
end;

procedure TLdapCheckMember.AllowGroupDN(const GroupDN: TRawUtf8DynArray);
var
  i: PtrInt;
  pid: cardinal;
begin
  fSafe.Lock;
  try
    for i := 0 to high(GroupDN) do
    begin
      if GetGroupPrimaryID(
           '', GroupDN[i], pid, fGroupBaseDN, fGroupCustomFilter) then
        AddInteger(fGroupID, pid, {nodup=}true);
      AddRawUtf8(fGroupDN, GroupDN[i], {nodup=}true, {casesens=}true);
    end;
  finally
    fSafe.UnLock;
  end;
end;


{ **************** HTTP BASIC Authentication via LDAP or Kerberos }

{ TBasicAuthServerExternal }

procedure TBasicAuthServerExternal.SetCheckMember(Value: TLdapCheckMember);
begin
  FreeAndNil(fCheckMember);
  if Value = nil then
    fOnBeforeAuth := nil
  else
    fOnBeforeAuth := Value.BeforeAuth;
  fCheckMember := Value;
end;

destructor TBasicAuthServerExternal.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fCheckMember);
end;

function TBasicAuthServerExternal.CheckCredential(const aUser: RawUtf8;
  const aPassword: SpiUtf8): TAuthServerResult;
var
  start: Int64;
begin
  result := asrUnknownUser;
  if (aUser = '') or
     (aPassword = '') then
    exit;
  // quick check from the internal in-memory cache
  fOnAfterAuthDelayed := true; // OnAfterAuth() callback is triggered below
  result := inherited CheckCredential(aUser, aPassword);
  if result <> asrUnknownUser then
    exit;
  // first time we encounter this user
  QueryPerformanceMicroSeconds(start);
  if ExternalServerAsk(aUser, aPassword) then
    if Assigned(fOnAfterAuth) and
       not fOnAfterAuth(self, aUser) then
      result := asrRejected
    else
    begin
      SetCredential(aUser, aPassword); // add valid credentials to the cache
      result := asrMatch;
    end;
  if fLog <> nil then
    fLog.Add.Log(sllTrace, 'CheckCredential(%)=% in %',
      [aUser, GetEnumName(TypeInfo(TAuthServerResult), ord(result)),
       MicroSecFrom(start)], self);
end;


{ TBasicAuthServerKerberos }

constructor TBasicAuthServerKerberos.Create(const aKerberosSpn: RawUtf8;
  const aRealm: RawUtf8; aCacheTimeoutSec: integer);
var
  r: RawUtf8;
begin
  fKerberosSpn := aKerberosSpn;
  if fKerberosSpn = '' then
    CldapGetDefaultLdapController(@r, @fKerberosSpn);
  if aRealm <> '' then
    r := aRealm
  else if r = '' then
    r := mormot.core.unicode.LowerCase(SplitRight(fKerberosSpn, '@'));
  inherited Create(r, daSHA3_256);
  if not InitializeDomainAuth then
    ELdap.RaiseUtf8('%.Create: no SSPI/GSSAPI available', [self]);
end;

function TBasicAuthServerKerberos.ExternalServerAsk(const aUser: RawUtf8;
  const aPassword: SpiUtf8): boolean;
begin
  result := KerberosAsk(aUser, aPassword, nil);
end;

function TBasicAuthServerKerberos.KerberosAsk(const aUser: RawUtf8;
  const aPassword: SpiUtf8; aFullUserName: PRawUtf8): boolean;
var
  client, server: TSecContext;
  datain, dataout: RawByteString;
begin
  result := false;
  InvalidateSecContext(client, 0);
  InvalidateSecContext(server, 0);
  try
    try
      while ClientSspiAuthWithPassword(
              client, datain, aUser, aPassword, fKerberosSpn, dataout) and
            ServerSspiAuth(server, dataout, datain) do ;
      if aFullUserName <> nil then
        ServerSspiAuthUser(server, aFullUserName^);
      {$ifdef OSWINDOWS}
      if (fGroupSid = nil) or
         ServerSspiAuthGroup(server, fGroupSid) then
      {$endif OSWINDOWS}
        result := true;
    except
      result := false;
    end;
  finally
    FreeSecContext(server);
    FreeSecContext(client);
  end;
end;

{$ifdef OSWINDOWS}

procedure TBasicAuthServerKerberos.AllowGroupClear;
begin
  fGroupSid := nil;
end;

procedure TBasicAuthServerKerberos.AllowGroupBySid(const GroupSid: TRawUtf8DynArray);
var
  i: PtrInt;
  sid: TSid;
begin
  for i := 0 to high(GroupSid) do
    if TextToSid(pointer(GroupSid[i]), sid) then
      AddRawSid(fGroupSid, @sid);
end;

procedure TBasicAuthServerKerberos.AllowGroupBySid(const GroupSidCsv: RawUtf8);
var
  g: TRawUtf8DynArray;
begin
  CsvToRawUtf8DynArray(pointer(GroupSidCsv), g, ',', {trim=}true);
  AllowGroupBySid(g);
end;

{$endif OSWINDOWS}


{ TBasicAuthServerLdap }

constructor TBasicAuthServerLdap.Create(const aRealm: RawUtf8;
  aLdapSettings: TLdapClientSettings; aCacheTimeoutSec: integer);
var
  r: RawUtf8;
begin
  // validate we have some LDAP server to safely connect to
  if aLdapSettings = nil then
    ELdap.RaiseUtf8('%.Create(nil)', [self]);
  fLdapSettings := aLdapSettings;
  fLdapSettings.ValidateTargetHostOrLoadDefault({trykerberos=}false);
  r := aRealm;
  if r = '' then
    r := fLdapSettings.KerberosDN; // e.g. 'ad.mycorp.com'
  if not fLdapSettings.Tls then
    ELdap.RaiseUtf8('%.Create(%): TLS is mandatory', [self, r]);
  // setup the internal in-memory cache
  inherited Create(r, daSHA3_256);
  fUsers.TimeOutSeconds := aCacheTimeoutSec;
end;

constructor TBasicAuthServerLdap.Create(const aLdapUri, aRealm: RawUtf8;
  aCacheTimeoutSec: integer);
begin
  Create(aRealm, TLdapClientSettings.Create(aLdapUri), aCacheTimeoutSec);
end;

destructor TBasicAuthServerLdap.Destroy;
begin
  inherited Destroy;
  fLdapSettings.Free;
end;

function TBasicAuthServerLdap.ExternalServerAsk(const aUser: RawUtf8;
  const aPassword: SpiUtf8): boolean;
var
  u: RawUtf8;
  client: TLdapClient;
begin
  // the AD expects the username in form 'user@full.kerberos.realm'
  u := aUser;
  if PosExChar('@', u) = 0 then
    if fLdapSettings.KerberosDN = '' then
      u := NetConcat([u, '@', fRealm])
    else
      u := NetConcat([u, '@', fLdapSettings.KerberosDN]);
  // try to use those credentials to bind to the LDAP server
  client := TLdapClient.Create(fLdapSettings);
  try
    client.Settings.UserName := u;
    client.Settings.Password := aPassword;
    result := client.Bind;
  finally
    client.Free;
  end;
end;


initialization
  assert((1 shl ord(uacPartialSecretsRodc)) = $04000000);

end.

