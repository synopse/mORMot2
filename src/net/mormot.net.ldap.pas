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
  mormot.core.datetime,
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

{.$define ASNDEBUG}
// enable low-level debugging of the LDAP transmitted frames on the console

const
  /// constructed class type bitmask
  ASN1_CL_CTR   = $20;
  /// application-specific class type bitmask
  ASN1_CL_APP   = $40;
  /// context-specific class type bitmask
  ASN1_CL_CTX   = $80;
  /// private class type bitmask
  ASN1_CL_PRI   = $c0;

  // base ASN.1 types
  ASN1_BOOL        = $01;
  ASN1_INT         = $02;
  ASN1_BITSTR      = $03;
  ASN1_OCTSTR      = $04;
  ASN1_NULL        = $05;
  ASN1_OBJID       = $06;
  ASN1_ENUM        = $0a;
  ASN1_UTF8STRING  = $0c;
  // base ASN1_CL_CTR types
  ASN1_SEQ         = $30;
  ASN1_SETOF       = $31;
  // common ASN1_CL_APP types
  ASN1_IPADDR      = $40;
  ASN1_COUNTER     = $41;
  ASN1_GAUGE       = $42;
  ASN1_TIMETICKS   = $43;
  ASN1_OPAQUE      = $44;
  ASN1_COUNTER64   = $46;

  ASN1_NUMBERS = [
    ASN1_INT,
    ASN1_ENUM,
    ASN1_BOOL,
    ASN1_COUNTER,
    ASN1_GAUGE,
    ASN1_TIMETICKS,
    ASN1_COUNTER64];

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

/// create an ASN.1 binary from several raw data - as OCTSTR by default
function AsnArr(const Data: array of RawByteString;
  AsnType: integer = ASN1_OCTSTR): TAsnObject;

/// create an ASN.1 binary from 64-bit signed integer, calling AsnEncInt()
function Asn(Value: Int64; AsnType: integer = ASN1_INT): TAsnObject; overload;

/// create an ASN.1 binary from a boolean value
function Asn(Value: boolean): TAsnObject; overload;

/// create an ASN.1 SEQuence from some raw data
function AsnSeq(const Data: TAsnObject): TAsnObject; overload;

/// raw append some binary to an ASN.1 object buffer
procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject);
  overload; {$ifdef HASINLINE} inline; {$endif}

/// encode and append some raw data as ASN.1
procedure AsnAdd(var Data: TAsnObject; const Buffer: TAsnObject;
  AsnType: integer); overload;

/// decode the len of a ASN.1 binary item
function AsnDecLen(var Start: integer; const Buffer: TAsnObject): cardinal;

/// decode the header of a ASN.1 binary item
function AsnDecHeader(var Pos: integer; const Buffer: TAsnObject;
  out AsnType, AsnSize: integer): boolean;

/// decode an ASN1_INT ASN1_ENUM ASN1_BOOL value
function AsnDecInt(var Start: integer; const Buffer: TAsnObject;
  AsnSize: integer): Int64;

/// decode an OID ASN.1 value into human-readable text
function AsnDecOid(Pos, EndPos: integer; const Buffer: TAsnObject): RawUtf8;

/// parse the next ASN.1 value as text
// - returns the ASN.1 value type, and optionally the ASN.1 value blob itself
function AsnNext(var Pos: integer; const Buffer: TAsnObject;
  Value: PRawByteString = nil; CtrEndPos: PInteger = nil): integer;

/// parse the next ASN1_INT ASN1_ENUM ASN1_BOOL value as integer
function AsnNextInteger(var Pos: integer; const Buffer: TAsnObject;
  out ValueType: integer): Int64;

/// human-readable display of a ASN.1 value binary
// - used e.g. by the ASNDEBUG conditional
function AsnDump(const Value: TAsnObject): RawUtf8;

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

/// translate a LDAP_RES_* result code into some human-readable text
function RawLdapErrorString(ErrorCode: integer): RawUtf8;

/// encode a LDAP search filter text into an ASN.1 binary
// - as used by CldapBroadcast() and TLdapClient.Search()
function RawLdapTranslateFilter(const Filter: RawUtf8): TAsnObject;

/// encode the ASN.1 binary for a LDAP_ASN1_SEARCH_REQUEST
// - as used by CldapBroadcast() and TLdapClient.Search()
function RawLdapSearch(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawByteString;
  Scope: TLdapSearchScope = lssBaseObject; Aliases: TLdapSearchAliases = lsaAlways;
  Sizelimit: integer = 0; TimeLimit: integer = 0): TAsnObject;

/// decode the ASN.1 binary of a LDAP_ASN1_SEARCH_ENTRY
// - and lookup by name the returned attributes as RawUtf8 variables
// - as used by CldapBroadcast()
function RawLdapSearchParse(const Response: TAsnObject; MessageId: integer;
  const Attributes: array of RawUtf8; const Values: array of PRawUtf8): boolean;

/// sort some LDAP host names using CLDAP over UDP
// - expects Hosts in 'host:port' format, as returned by DnsLdapControlers,
// e.g. ['dc-one.mycorp.com:389', 'dc-two.mycorp.com:389']
// - hosts not available over UDP within the TimeoutMS period, are put at the
// end of the list because they may still be reachable via TCP
// - used e.g. by TLdapClient.Connect() with the lccUdpFirst option
procedure CldapSortHosts(var Hosts: TRawUtf8DynArray; TimeoutMS: integer);

type
  /// define one result for a server identified by CldapBroadcast()
  TCldapServer = record
    /// after how many microseconds this response has been received
    TimeMicroSec: Integer;
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
function CldapBroadcast(var Servers: TCldapServers; TimeOutMS: integer = 100;
  const Address: RawUtf8 = cBroadcast; const Port: RawUtf8 = LDAP_PORT): integer;



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
    property List: TRawByteStringDynArray
      read fList;
    /// name of this LDAP attribute
    property AttributeName: RawUtf8
      read fAttributeName;
  end;
  /// dynamic array of LDAP attribute, as stored in TLdapAttributeList
  TLdapAttributeDynArray = array of TLdapAttribute;

  /// list one or several TLdapAttribute
  TLdapAttributeList = class
  private
    fItems: TLdapAttributeDynArray;
    fInterning: TRawUtf8Interning; // global hash table of attribute names
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
    // - use a temporary TLdapClient.Connect then optionally call BindSaslKerberos
    // - any existing KerberosDN and KerberosSpn will be used during discovery
    function LoadDefaultFromSystem(TryKerberos: boolean): boolean;
  published
    /// target server IP (or symbolic name)
    // - default is '' but if not set, Connect will call DnsLdapControlers()
    // from mormot.net.dns to retrieve the current value from the system
    // - after connect, will contain the actual server name
    // - typical value is 'dc-one.mycorp.com'
    property TargetHost: RawUtf8
      read fTargetHost Write fTargetHost;
    /// target server port (or symbolic name)
    // - is '389' (LDAP_PORT) by default but could be '636' (LDAP_TLS_PORT or
    // sometimes '3269') on TLS
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
    // - with BindSaslKerberos, on Linux or Windows it should be 'username'
    // but on MacOS it should be 'username@ad.mycompany.tld'
    property UserName: RawUtf8
      read fUserName Write fUserName;
    /// if protocol needs user authorization, then fill here its password
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    property Password: SpiUtf8
      read fPassword Write fPassword;
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

  /// how TLdapClient.Connect try to find the LDAP server if no TargetHost is set
  // - lccUdpFirst will make a first round over the supplied addresses using
  // UDP and CLDAP, to find out the closest alive instances - also circumvent
  // if some AD were configured to drop and timeout more distant hosts
  // - lccTlsFirst will try to connect as TLS on port 636 (if OpenSSL is loaded)
  TLdapClientConnect = set of (
    lccUdpFirst,
    lccTlsFirst);

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
    fRootDN, fConfigDN: RawUtf8;
    fNetbiosDN: RawUtf8;
    fMechanisms: TRawUtf8DynArray;
    fSecContext: TSecContext;
    fSecContextUser: RawUtf8;
    fWellKnownObjects: TLdapKnownCommonNamesDual;
    fWellKnownObjectsCached: boolean;
    function BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
    procedure SendPacket(const Asn1Data: TAsnObject);
    function ReceiveResponse: TAsnObject;
    function DecodeResponse(var Pos: integer; const Asn1Response: TAsnObject): TAsnObject;
    function SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
    function RetrieveWellKnownObjects(const DN: RawUtf8;
      out Dual: TLdapKnownCommonNamesDual): boolean;
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
    function Connect(DiscoverMode: TLdapClientConnect = [lccUdpFirst, lccTlsFirst]): boolean;
    /// the Root domain name of this LDAP server
    // - use an internal cache for fast retrieval
    function RootDN: RawUtf8;
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
    // - warning: uses plaintext transport of password - consider using TLS
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
    /// retrieve all entries that match a given set of criteria
    // - will generate as many requests/responses as needed to retrieve all
    // the information into the SearchResult property
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      const Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
    /// retrieve all entries that match a given set of criteria and return the
    // first result
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if no result is found or if the search failed
    function SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
      const Attributes: array of RawByteString): TLdapResult;
    /// retrieve the entry matching the given ObjectDN
    // - Will call Search method, therefore SearchResult will contains all the results
    // - Returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN, Filter: RawUtf8;
      const Attributes: array of RawByteString;
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
    /// test whether the client is connected to the server
    // - if AndBound is set, it also checks that a successfull bind request has been made
    function Connected(AndBound: boolean = true): boolean;
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

function AsnDecInt(var Start: integer; const Buffer: TAsnObject;
  AsnSize: integer): Int64;
var
  x: byte;
  neg: boolean;
begin
  result := 0;
  if (AsnSize <= 0) or
     (Start + AsnSize > length(Buffer)) then
    exit;
  neg := ord(Buffer[Start]) > $7f;
  while AsnSize > 0 do
  begin
    x := ord(Buffer[Start]);
    if neg then
      x := not x;
    result := (result shl 8) + x;
    inc(Start);
    dec(AsnSize);
  end;
  if neg then
    result := -(result + 1);
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

function AsnArr(const Data: array of RawByteString; AsnType: integer): TAsnObject;
var
  i: PtrInt;
begin
  result := '';
  for i := 0 to high(Data) do
    Append(result, Asn(AsnType, [Data[i]]));
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

function AsnDecOid(Pos, EndPos: integer; const Buffer: TAsnObject): RawUtf8;
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

function AsnDecHeader(var Pos: integer; const Buffer: TAsnObject;
  out AsnType, AsnSize: integer): boolean;
var
  l: integer;
begin
  result := false;
  l := length(Buffer);
  if Pos > l then
    exit;
  AsnType := ord(Buffer[Pos]);
  inc(Pos);
  AsnSize := AsnDecLen(Pos, Buffer);
  if (Pos + AsnSize - 1) > l then
    exit;
  result := true;
end;

function AsnNextInteger(var Pos: integer; const Buffer: TAsnObject;
  out ValueType: integer): Int64;
var
  asnsize: integer;
begin
  if AsnDecHeader(Pos, Buffer, ValueType, asnsize) and
     (ValueType in [ASN1_INT, ASN1_ENUM, ASN1_BOOL]) then
    result := AsnDecInt(Pos, Buffer, asnsize)
  else
  begin
    ValueType := ASN1_NULL;
    result := -1;
  end;
end;

function AsnNext(var Pos: integer; const Buffer: TAsnObject;
  Value: PRawByteString; CtrEndPos: PInteger): integer;
var
  asnsize: integer;
  y: int64;
begin
  if Value <> nil then
    Value^ := '';
  result := ASN1_NULL;
  if not AsnDecHeader(Pos, Buffer, result, asnsize) then
    exit;
  if CtrEndPos <> nil then
    CtrEndPos^ := Pos + asnsize;
  if Value = nil then
  begin
    // no need to allocate and return the whole Value^: just compute position
    if (result and ASN1_CL_CTR) = 0 then
      // constructed (e.g. ASN1_SEQ): keep Pos after header
      inc(Pos, asnsize);
    exit;
  end;
  // we need to return the Value^
  if (result and ASN1_CL_CTR) <> 0 then
    // constructed (e.g. ASN1_SEQ): return whole data, but keep Pos after header
    Value^ := copy(Buffer, Pos, asnsize)
  else
    case result of
      ASN1_INT,
      ASN1_ENUM,
      ASN1_BOOL:
        Value^ := ToUtf8(AsnDecInt(Pos, Buffer, asnsize));
      ASN1_COUNTER,
      ASN1_GAUGE,
      ASN1_TIMETICKS,
      ASN1_COUNTER64:
        begin
          y := 0;
          while asnsize <> 0 do
          begin
            y := (y shl 8) + ord(Buffer[Pos]);
            inc(Pos);
            dec(asnsize);
          end;
          Value^ := ToUtf8(y);
        end;
      ASN1_OBJID:
        begin
          Value^ := AsnDecOid(Pos, Pos + asnsize, Buffer);
          inc(Pos, asnsize);
        end;
      ASN1_IPADDR:
        begin
          case asnsize of
            4:
              IP4Text(pointer(@Buffer[Pos]), RawUtf8(Value^));
            16:
              IP6Text(pointer(@Buffer[Pos]), RawUtf8(Value^));
          else
            BinToHexLower(@Buffer[Pos], asnsize, RawUtf8(Value^));
          end;
          inc(Pos, asnsize);
        end;
      ASN1_NULL:
        inc(Pos, asnsize);
    else
      // ASN1_UTF8STRING, ASN1_OCTSTR, ASN1_OPAQUE or unknown
      begin
        Value^ := copy(Buffer, Pos, asnsize); // return as raw binary
        inc(Pos, asnsize);
        if (Value^ <> '') and
           IsValidUtf8(Value^) then
          FakeCodePage(Value^, CP_UTF8); // we know this is value UTF-8
      end;
    end;
end;

function DNToCN(const DN: RawUtf8): RawUtf8;
var
  p: PUtf8Char;
  DC, OU, CN, kind, value: RawUtf8;
begin
  result := '';
  p := pointer(DN);
  if P = nil then
    exit;
  repeat
    GetNextItemTrimed(p, '=', kind);
    GetNextItemTrimed(p, ',', value);
    if (kind = '') or
       (value = '') then
      raise ELdap.CreateFmt('DNToCN(%s): invalid Distinguished Name', [DN]);
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
  until P = nil;
  result := DC + OU + CN;
end;

function IsBinaryString(var Value: RawByteString): boolean;
var
  n: PtrInt;
begin
  result := true;
  for n := 1 to length(Value) do
    case ord(Value[n]) of
      0:
        if n <> length(value) then
          exit
        else // consider null-terminated strings as non-binary, but truncate
          SetLength(Value, n - 1);
      1..8, 10..31:
        exit;
    end;
  result := false;
end;

procedure DumpClass(at: integer; w: TTextWriter);
begin
  if at and ASN1_CL_APP <> 0 then
    w.AddShorter('APP ');
  if at and ASN1_CL_CTX <> 0 then
    w.AddShorter('CTX ');
  if at and ASN1_CL_PRI = ASN1_CL_PRI then
    w.AddShorter('PRI ');
  if at < ASN1_CL_APP then
    w.AddShorter('unknown')
  else
    w.AddByteToHex(at and $0f);
end;

function AsnDump(const Value: TAsnObject): RawUtf8;
var
  i, at, x, n, indent: integer;
  s: RawByteString;
  il: TIntegerDynArray;
  w: TTextWriter;
  tmp: TTextWriterStackBuffer;
begin
  w := TTextWriter.CreateOwnedStream(tmp);
  try
    i := 1;
    indent := 0;
    while i < length(Value) do
    begin
      for n := length(il) - 1 downto 0 do
      begin
        x := il[n];
        if x <= i then
        begin
          DeleteInteger(il, n);
          dec(indent, 2);
        end;
      end;
      at := AsnNext(i, Value, @s);
      w.AddChars(' ', indent);
      w.Add('$');
      w.AddByteToHex(at);
      if (at and ASN1_CL_CTR) <> 0 then
      begin
        w.Add(' ');
        case at of
          ASN1_SEQ:
            w.AddShorter('SEQ');
          ASN1_SETOF:
            w.AddShorter('SETOF');
        else
          DumpClass(at, w);
        end;
        x := length(s);
        w.Add(' CTR: length %', [x]);
        inc(indent, 2);
        AddInteger(il, x + i - 1);
      end
      else
      begin
        w.Add(' ');
        case at of
          // base ASN.1 types
          ASN1_BOOL:
            w.AddShorter('BOOL');
          ASN1_INT:
            w.AddShorter('INT');
          ASN1_BITSTR:
            w.AddShorter('BITSTR');
          ASN1_OCTSTR:
            w.AddShorter('OCTSTR');
          ASN1_NULL:
            w.AddShorter('NULL');
          ASN1_OBJID:
            w.AddShorter('OBJID');
          ASN1_ENUM:
            w.AddShorter('ENUM');
          ASN1_UTF8STRING:
            w.AddShorter('UTF8');
          // ASN1_CL_APP are application-specific
          {
          ASN1_IPADDR:
            w.AddShorter('IPADDR');
          ASN1_COUNTER:
            w.AddShorter('COUNTER');
          ASN1_GAUGE:
            w.AddShorter('GAUGE');
          ASN1_TIMETICKS:
            w.AddShorter('TIMETICK');
          ASN1_OPAQUE:
            w.AddShorter('OPAQUE');
          ASN1_COUNTER64:
            w.AddShorter('CNTR64');
          }
        else
          DumpClass(at, w);
        end;
        w.Add(':', ' ');
        if IsBinaryString(s) then
        begin
          w.Add('binary len=% ', [length(s)]);
          w.AddShort(EscapeToShort(s));
        end
        else if at in ASN1_NUMBERS then
          w.AddString(s) // not quoted value
        else if PosExChar('"', s) = 0 then
        begin
          w.Add('"');
          w.AddString(s);
          w.Add('"');
        end
        else
        begin
          w.Add('''');
          w.AddString(s); // alternate output layout for quoted text
          w.Add('''');
        end;
      end;
      w.AddCR;
    end;
    w.SetText(result);
  finally
    w.Free;
  end;
end;

{$ifdef ASNUNSTABLE}

// not used nor fully tested
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
    y := x and $ff;
    x := x shr 8;
    Prepend(result, [AnsiChar(y)]);
  until x = 0;
  if neg then
    result[1] := AnsiChar(ord(result[1]) or $80);
end;

{$endif ASNUNSTABLE}

{ **************** LDAP Protocol Definitions }

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
  x, dn: integer;
  c: Ansichar;
  s, t, l, r, attr, rule: RawUtf8;
begin
  result := '';
  if Filter = '' then
    exit;
  if Filter = '*' then
  begin
    result := Asn('*', ASN1_CTX7);
    exit;
  end;
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

function RawLdapSearch(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawByteString;
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
              Asn(TypesOnly),
              filt,
              AsnSeq(AsnArr(Attributes))]);
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

function CldapBroadcast(var Servers: TCldapServers; TimeOutMS: integer;
  const Address, Port: RawUtf8): integer;
var
  id: integer;
  req, response: RawByteString;
  addr, resp: TNetAddr;
  start, stop: Int64;
  sock: TNetSocket;
  len: PtrInt;
  res: TNetResult;
  v: TCldapServer;
  tmp: array[0..1999] of byte; // big enough for a UDP frame
begin
  result := 0;
  if addr.SetFrom(Address, Port, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
  try
    sock.SetBroadcast(true);
    repeat
      id := Random32 shr 1; // use a 31-bit random MessageID for UDP
    until id <> 0;
    req := Asn(ASN1_SEQ, [
             Asn(id),
             //Asn(''), // the RFC 1798 requires user, but MS AD does not :(
             RawLdapSearch('', false, '*', ['dnsHostName',
               'defaultNamingContext', 'ldapServiceName', 'vendorName'])]);
    sock.SetReceiveTimeout(TimeOutMS);
    QueryPerformanceMicroSeconds(start);
    res := sock.SendTo(pointer(req), length(req), addr);
    if res <> nrOK then
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

procedure CldapSortHosts(var Hosts: TRawUtf8DynArray; TimeoutMS: integer);
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
               RawLdapSearch('', false, '*', ['dnsHostName'])]);
      if sock[i].SendTo(pointer(req), length(req), addr) = nrOk then
        poll.Subscribe(sock[i], [pseRead], i)
      else
      begin
        sock[i].Close;
        sock[i] := nil;
      end;
    end;
    // wait for all incoming requests
    tix := GetTickCount64 + TimeoutMS;
    repeat
      if poll.WaitForModified(res, 10) then
        for r := 0 to res.Count - 1 do
        begin
          i := ResToTag(res.Events[r]);
          if (PtrUInt(i) >= PtrUInt(n)) or
             (sock[i] = nil) then
            break;
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
    until (found = n) or
          (GetTickCount64 > tix);
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


{ **************** LDAP Response Storage }

const
  KNOWN_NAMES: array[0..11] of RawUtf8 = (
    'ObjectSid',                   // 0
    'ObjectGUID',                  // 1
    'accountExpires',              // 2
    'badPasswordTime',             //
    'lastLogon',                   //
    'lastLogonTimestamp',          //
    'lastLogoff',                  //
    'lockoutTime',                 //
    'pwdLastSet',                  //
    'ms-MCS-AdmPwdExpirationTime', // 9
    'whenCreated',                 // 10
    'whenChanged');                // 11

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
  case lat of
    latObjectSid:
      if IsValidSid(s) then
        s := SidToText(pointer(s));
    latObjectGuid:
      if length(s) = SizeOf(TGuid) then
        s := ToUtf8(PGuid(s)^);
    latFileTime:
      begin
        qw := GetQWord(pointer(s), err);
        if (err = 0) and
           (qw <> 0) then
          if qw >= $7FFFFFFFFFFFFFFF then
            s := 'Never expires'
          else
            s := UnixMSTimeToString(WindowsFileTime64ToUnixMSTime(qw));
      end;
    latIsoTime:
      begin
        ts.From(pointer(s), length(s) - 3);
        if ts.Value <> 0 then
          s := ts.Text(true); // normalize
      end;
  end;
  if not IsValidUtf8(s) then
    s := BinToHexLower(s)
  else
    EnsureRawUtf8(s);
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
  result := TLdapAttribute.Create(fInterning.Unique(AttributeName));
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
    w.AddShorter(MicroSecToString(fSearchTimeMicroSec));
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

constructor TLdapClientSettings.Create;
begin
  inherited Create;
  fTargetPort := LDAP_PORT;
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
    if not test.Connect then
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
  Close;
  fSearchResult.Free;
  fReferals.Free;
  fSettings.Free;
  inherited Destroy;
end;

function TLdapClient.Connect(DiscoverMode: TLdapClientConnect): boolean;
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
    if lccUdpFirst in DiscoverMode then
      CldapSortHosts(dc, 100); // 100 ms should be enough to locate local ADs
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
        fSettings.Tls := (p = LDAP_TLS_PORT) or // likely to be over TLS
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
    fNetbiosDN := SearchObject('CN=Partitions,CN=Configuration,' + RootDN,
      '(nETBIOSName=*)', 'nETBIOSName', lssWholeSubtree).GetReadable;
  result := fNetbiosDN;
end;

function TLdapClient.RootDN: RawUtf8;
begin
  if (fRootDN = '') and
     fSock.SockConnected then
    fRootDN := SearchObject('', '*', 'rootDomainNamingContext').GetReadable;
  result := fRootDN;
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
  if (fRootDN = '') and
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
      if IdemPropNameU(fMechanisms[i], MechanismName) then
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
    if fSecContextEncrypt then
    begin
      // we need to use the Kerberos encryption
      len := 0;
      fSock.SockInRead(pointer(@len), 4); // SASL Buffer length
      len := bswap32(len);
      result := fSock.SockInRead(len);
      result := SecDecrypt(fSecContext, result); // decrypt
      // note: several SEQ messages may be returned
    end
    else
    begin
      // we need to decode the ASN.1 plain input to return a single SEQ message
      fSock.SockInRead(pointer(@b), 1, true); // ASN type
      if b <> ASN1_SEQ then
        exit;
      result := AnsiChar(b);
      fSock.SockInRead(pointer(@b), 1, true); // ASN length
      Append(result, @b, 1);
      if b >= $80 then // $8x means x bytes of length
        AsnAdd(result, fSock.SockInRead(b and $7f, true));
      // decode length of LDAP packet
      pos := 2;
      len := AsnDecLen(pos, result);
      // retrieve body of LDAP packet
      if len > 0 then
        AsnAdd(result, fSock.SockInRead(len, true));
    end;
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
begin
  result := '';
  fResultCode := -1;
  fResultString := '';
  fResponseCode := LDAP_ASN1_ERROR;
  fResponseDN := '';
  if (AsnNext(pos, Asn1Response) <> ASN1_SEQ) or
     (AsnNextInteger(pos, Asn1Response, asntype) <> fSeq) or
     (asntype <> ASN1_INT) then
    exit;
  fResponseCode := AsnNext(pos, Asn1Response, nil, @seqend);
  if fResponseCode in LDAP_ASN1_RESPONSES then
  begin
    // final response
    fResultCode := AsnNextInteger(pos, Asn1Response, asntype);
    AsnNext(pos, Asn1Response, @fResponseDN);   // matchedDN
    AsnNext(pos, Asn1Response, @fResultString); // diagnosticMessage
    if (fResultString = '') and
       (fResultCode <> LDAP_RES_SUCCESS) then
      fResultString := RawLdapErrorString(fResultCode);
    if fResultCode = LDAP_RES_REFERRAL then
      if AsnNext(pos, Asn1Response, @s) = ASN1_CTC3 then
      begin
        x := 1;
        while x < length(s) do
        begin
          AsnNext(x, s, @t);
          fReferals.Add(t);
        end;
    end;
    result := copy(Asn1Response, pos, length(Asn1Response) - pos + 1); // body
    pos := length(Asn1Response) + 1;
  end
  else
  begin
    // partial response (e.g. LDAP_ASN1_SEARCH_ENTRY)
    result := copy(Asn1Response, pos, seqend - pos);
    pos := seqend;
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
    'DIGEST-SHA3-256');     // daSHA3_256_Sess

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
    raise ESynCrypto.CreateUtf8('%.BindSaslDigest(%) requires a *-sess algo',
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
      dig := DigestClient(Algo, t, 'ldap/' + LowerCaseU(fSock.Server),
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

function TLdapClient.BindSaslKerberos(const AuthIdentify: RawUtf8;
  KerberosUser: PRawUtf8): boolean;
var
  datain, dataout: RawByteString;
  t, req1, req2: TAsnObject;
  needencrypt: boolean;

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
     not Connect or
     not InitializeDomainAuth then
     exit;
  needencrypt := false;
  fSecContextUser := '';
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
        exit; // catch SSPI/GSSAPI errors and return false
      end;
      if dataout = '' then
      begin
        // last step of SASL handshake - see RFC 4752 section 3.1
        if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
          break; // perform if only needed (e.g. not on MS AD)
        t := SendAndReceive(req1);
        if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
          exit;
        ParseInput;
        datain := SecDecrypt(fSecContext, datain);
        if (length(datain) <> 4) or
           ((datain[1] = #0) and
            (PCardinal(datain)^ <> 0)) then
          exit; // invalid or unsupported token
        PCardinal(datain)^ := 0; // #0: noseclayer, #1#2#3: maxmsgsize=0
        if AuthIdentify <> '' then
          Append(datain, AuthIdentify);
        dataout := SecEncrypt(fSecContext, datain);
        needencrypt := false; // noseclayer
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
      AsnAdd(sub, Asn(attr.List[j]));
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
  ComputerDN, ComputerSam: RawUtf8;
  PwdU16: RawByteString;
  Attributes: TLdapAttributeList;
  ComputerObject: TLdapResult;
begin
  result := false;
  if not Connected then
    exit;
  ComputerDN := 'CN=' + ComputerName + ',' + ComputerParentDN;
  ComputerSam := UpperCase(ComputerName) + '$';
  // Search Computer object in the domain
  ComputerObject := SearchFirst(RootDN,
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
    Attributes.Add('cn', ComputerName);
    Attributes.Add('sAMAccountName', ComputerSam);
    Attributes.Add('userAccountControl', '4096');
    if Password <> '' then
    begin
      PwdU8 := '"' + Password + '"';
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
  const Filter: RawUtf8; const Attributes: array of RawByteString): boolean;
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
  // see https://ldap.com/ldapv3-wire-protocol-reference-search
  QueryPerformanceMicroSeconds(start);
  fSearchResult.Clear;
  fReferals.Clear;
  s := RawLdapSearch(BaseDN, TypesOnly, Filter, Attributes, fSearchScope,
    fSearchAliases, fSearchSizeLimit, fSearchTimeLimit);
  if fSearchPageSize > 0 then
    Append(s, Asn(
        Asn(ASN1_SEQ, [
           Asn('1.2.840.113556.1.4.319'), // controlType: pagedresultsControl
           Asn(false), // criticality: false
           Asn(Asn(ASN1_SEQ, [
             Asn(fSearchPageSize),
             Asn(fSearchCookie)]))]), LDAP_ASN1_CONTROLS));
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
  begin
    if AsnNext(n, resp) = ASN1_SEQ then
    begin
      AsnNext(n, resp, @s);
      if s = '1.2.840.113556.1.4.319' then
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
  end;
  fSearchResult.AfterAdd; // allow "for res in ldap.SearchResult.Items do"
  result := fResultCode = LDAP_RES_SUCCESS;
  QueryPerformanceMicroSeconds(stop);
  fSearchResult.fSearchTimeMicroSec := stop - start;
end;

function TLdapClient.SearchFirst(const BaseDN: RawUtf8; Filter: RawUtf8;
  const Attributes: array of RawByteString): TLdapResult;
begin
  result := nil;
  if Search(BaseDN, false, Filter, Attributes) and
     (SearchResult.Count > 0) then
    result := SearchResult.Items[0];
end;

function TLdapClient.SearchObject(const ObjectDN, Filter: RawUtf8;
  const Attributes: array of RawByteString; Scope: TLdapSearchScope): TLdapResult;
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

function TLdapClient.Connected(AndBound: boolean): boolean;
begin
  result := fSock.SockConnected;
  if result and
     AndBound then
    result := fBound;
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
    if RetrieveWellKnownObjects(RootDN, fWellKnownObjects) then
        fWellKnownObjectsCached := true;
  result := @fWellKnownObjects[AsCN];
end;


end.

