/// Simple Network LDAP Client
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ldap;

{
  *****************************************************************************

   Simple LDAP Protocol Client
    - CLDAP Client Functions
    - LDAP Protocol Definitions
    - LDAP Attributes Definitions
    - LDIF Data Interchange Format
    - LDAP Response Storage
    - Main TLdapClient Class
    - Dedicated TLdapCheckMember Class
    - HTTP BASIC Authentication via LDAP or Kerberos

  *****************************************************************************
  Code below was inspired by Synapse Library code:
   The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
   Portions created by Lukas Gebauer are (c)2003-2014. All Rights Reserved.
}

interface

{$I ..\mormot.defines.inc}

{.$define ASNDEBUG}
// enable low-level debugging of the LDAP transmitted frames on the console

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.buffers,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.variants,
  mormot.core.data,
  mormot.core.log,
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.net.sock,
  mormot.net.dns;


{ **************** CLDAP Client Functions }

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

  /// the domain information returned by CldapGetDomainInfo(
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
  DistinguishedName: PRawUtf8 = nil; SPN: PRawUtf8 = nil): RawUtf8;

/// pickup the preferred LDAP 'server:port' of a set of LDAP servers
// - will send CLDAP NetLogon messages to the LdapServers to retrieve
// TCldapDomainInfo.ClientSite then request the DNS for the LDAP of this site
// - if no site is defined, fallback to the first known LDAP server
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


{ **************** LDAP Protocol Definitions }

/// convert a Distinguished Name to a Canonical Name
// - raise ELdap if the supplied DN is invalid - unless NoRaise was set to true
// - e.g. DNToCN('CN=User1,OU=Users,OU=London,DC=xyz,DC=local') =
// 'xyz.local/London/Users/User1'
function DNToCN(const DN: RawUtf8; NoRaise: boolean = false): RawUtf8;

/// normalize a Distinguished Name into its standard layout
// - trim spaces, and use CN= OU= DC= specifiers
function NormalizeDN(const DN: RawUtf8): RawUtf8;

/// low-level parse a Distinguished Name text into its DC= OU= CN= parts
// - on parsing error, raise ELdap or return false if NoRaise was set to true
function ParseDN(const DN: RawUtf8; out dc, ou, cn: TRawUtf8DynArray;
  ValueEscapeCN: boolean = false; NoRaise: boolean = false): boolean; overload;

type
  /// one name / value pair from ParseDN()
  TNameValueDN = record
    Name: RawUtf8;
    Value: RawUtf8;
  end;
  /// name / value pairs as ParseDN() resultset
  TNameValueDNs = array of TNameValueDN;

/// parse a Distinguished Name text into all its name=value parts
// - on parsing error, raise ELdap or return false if NoRaise was set to true
function ParseDN(const DN: RawUtf8; out pairs: TNameValueDNs;
  NoRaise: boolean = false): boolean; overload;

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

type
  /// high level LDAP result codes
  // - as returned e.g. by TLdapClient.ResultError property
  // - leUnknown is likely to be a client-side error - check the
  // TLdapClient.ResultString text content for more information
  // - use RawLdapError() and RawLdapErrorString() to decode a LDAP result code
  // or LDAP_RES_CODE[] and LDAP_ERROR_TEXT[] to their integer/text value
  // - see https://ldap.com/ldap-result-code-reference
  TLdapError = (
    leUnknown,
    leSuccess,
    leOperationsError,
    leProtocolError,
    leTimeLimitExceeded,
    leSizeLimitExceeded,
    leCompareFalse,
    leCompareTrue,
    leAuthMethodNotSupported,
    leStrongerAuthRequired,
    leReferral,
    leAdminLimitExceeded,
    leUnavailableCriticalExtension,
    leConfidentalityRequired,
    leSaslBindInProgress,
    leNoSuchAttribute,
    leUndefinedAttributeType,
    leInappropriateMatching,
    leConstraintViolation,
    leAttributeOrValueExists,
    leInvalidAttributeSyntax,
    leNoSuchObject,
    leAliasProblem,
    leInvalidDNSyntax,
    leIsLeaf,
    leAliasDereferencingProblem,
    leInappropriateAuthentication,
    leInvalidCredentials,
    leInsufficientAccessRights,
    leBusy,
    leUnavailable,
    leUnwillingToPerform,
    leLoopDetect,
    leSortControlMissing,
    leOffsetRangeError,
    leNamingViolation,
    leObjectClassViolation,
    leNotAllowedOnNonLeaf,
    leNotAllowedOnRDN,
    leEntryAlreadyExists,
    leObjectClassModsProhibited,
    leResultsTooLarge,
    leAffectsMultipleDSAs,
    leControlError,
    leOther,
    leServerDown,
    leLocalError,
    leEncodingError,
    leDecodingError,
    leTimeout,
    leAuthUnknown,
    leFilterError,
    leUserCanceled,
    leParamError,
    leNoMemory,
    leConnectError,
    leNotSupported,
    leControlNotFound,
    leNoResultsReturned,
    leMoreResultsToReturn,
    leClientLoop,
    leReferralLimitExceeded,
    leInvalidResponse,
    leAmbiguousResponse,
    leTlsNotSupported,
    leIntermediateResponse,
    leUnknownType,
    leCanceled,
    leNoSuchOperation,
    leTooLate,
    leCannotCancel,
    leAssertionFailed,
    leAuthorizationDenied,
    leEsyncRefreshRequired,
    leNoOperation);
  PLdapError = ^TLdapError;

var
  /// human friendly text of all TLdapError items
  // - are the official text of all TLdapError identifiers as listed in
  // https://ldap.com/ldap-result-code-reference
  // e.g. LDAP_ERROR_TEXT[leEsyncRefreshRequired] = 'e-syncRefreshRequired'
  // - see RawLdapErrorString() to decode a LDAP result code into a full message
  LDAP_ERROR_TEXT: array[TLdapError] of RawUtf8;

const
  /// raw LDAP result codes (in range 0..123) of all TLdapError items
  // - use RawLdapError() or RawLdapErrorString() to decode a LDAP result code
  LDAP_RES_CODE: array[leSuccess .. leAuthorizationDenied] of byte = (
    LDAP_RES_SUCCESS,                        // leSuccess
    LDAP_RES_OPERATIONS_ERROR,               // leOperationsError
    LDAP_RES_PROTOCOL_ERROR,                 // leProtocolError
    LDAP_RES_TIME_LIMIT_EXCEEDED,            // leTimeLimitExceeded
    LDAP_RES_SIZE_LIMIT_EXCEEDED,            // leSizeLimitExceeded
    LDAP_RES_COMPARE_FALSE,                  // leCompareFalse
    LDAP_RES_COMPARE_TRUE,                   // leCompareTrue
    LDAP_RES_AUTH_METHOD_NOT_SUPPORTED,      // leAuthMethodNotSupported
    LDAP_RES_STRONGER_AUTH_REQUIRED,         // leStrongerAuthRequired
    LDAP_RES_REFERRAL,                       // leReferral
    LDAP_RES_ADMIN_LIMIT_EXCEEDED,           // leAdminLimitExceeded
    LDAP_RES_UNAVAILABLE_CRITICAL_EXTENSION, // leUnavailableCriticalExtension
    LDAP_RES_CONFIDENTIALITY_REQUIRED,       // leConfidentalityRequired
    LDAP_RES_SASL_BIND_IN_PROGRESS,          // leSaslBindInProgress
    LDAP_RES_NO_SUCH_ATTRIBUTE,              // leNoSuchAttribute
    LDAP_RES_UNDEFINED_ATTRIBUTE_TYPE,       // leUndefinedAttributeType
    LDAP_RES_INAPPROPRIATE_MATCHING,         // leInappropriateMatching
    LDAP_RES_CONSTRAINT_VIOLATION,           // leConstraintViolation
    LDAP_RES_ATTRIBUTE_OR_VALUE_EXISTS,      // leAttributeOrValueExists
    LDAP_RES_INVALID_ATTRIBUTE_SYNTAX,       // leInvalidAttributeSyntax
    LDAP_RES_NO_SUCH_OBJECT,                 // leNoSuchObject
    LDAP_RES_ALIAS_PROBLEM,                  // leAliasProblem
    LDAP_RES_INVALID_DN_SYNTAX,              // leInvalidDNSyntax
    LDAP_RES_IS_LEAF,                        // leIsLeaf
    LDAP_RES_ALIAS_DEREFERENCING_PROBLEM,    // leAliasDereferencingProblem
    LDAP_RES_INAPPROPRIATE_AUTHENTICATION,   // leInappropriateAuthentication
    LDAP_RES_INVALID_CREDENTIALS,            // leInvalidCredentials
    LDAP_RES_INSUFFICIENT_ACCESS_RIGHTS,     // leInsufficientAccessRights
    LDAP_RES_BUSY,                           // leBusy
    LDAP_RES_UNAVAILABLE,                    // leUnavailable
    LDAP_RES_UNWILLING_TO_PERFORM,           // leUnwillingToPerform
    LDAP_RES_LOOP_DETECT,                    // leLoopDetect
    LDAP_RES_SORT_CONTROL_MISSING,           // leSortControlMissing
    LDAP_RES_OFFSET_RANGE_ERROR,             // leOffsetRangeError
    LDAP_RES_NAMING_VIOLATION,               // leNamingViolation
    LDAP_RES_OBJECT_CLASS_VIOLATION,         // leObjectClassViolation
    LDAP_RES_NOT_ALLOWED_ON_NON_LEAF,        // leNotAllowedOnNonLeaf
    LDAP_RES_NOT_ALLOWED_ON_RDN,             // leNotAllowedOnRDN
    LDAP_RES_ENTRY_ALREADY_EXISTS,           // leEntryAlreadyExists
    LDAP_RES_OBJECT_CLASS_MODS_PROHIBITED,   // leObjectClassModsProhibited
    LDAP_RES_RESULTS_TOO_LARGE,              // leResultsTooLarge
    LDAP_RES_AFFECTS_MULTIPLE_DSAS,          // leAffectsMultipleDSAs
    LDAP_RES_CONTROL_ERROR,                  // leControlError
    LDAP_RES_OTHER,                          // leOther
    LDAP_RES_SERVER_DOWN,                    // leServerDown
    LDAP_RES_LOCAL_ERROR,                    // leLocalError
    LDAP_RES_ENCODING_ERROR,                 // leEncodingError
    LDAP_RES_DECODING_ERROR,                 // leDecodingError
    LDAP_RES_TIMEOUT,                        // leTimeout
    LDAP_RES_AUTH_UNKNOWN,                   // leAuthUnknown
    LDAP_RES_FILTER_ERROR,                   // leFilterError
    LDAP_RES_USER_CANCELED,                  // leUserCanceled
    LDAP_RES_PARAM_ERROR,                    // leParamError
    LDAP_RES_NO_MEMORY,                      // leNoMemory
    LDAP_RES_CONNECT_ERROR,                  // leConnectError
    LDAP_RES_NOT_SUPPORTED,                  // leNotSupported
    LDAP_RES_CONTROL_NOT_FOUND,              // leControlNotFound
    LDAP_RES_NO_RESULTS_RETURNED,            // leNoResultsReturned
    LDAP_RES_MORE_RESULTS_TO_RETURN,         // leMoreResultsToReturn
    LDAP_RES_CLIENT_LOOP,                    // leClientLoop
    LDAP_RES_REFERRAL_LIMIT_EXCEEDED,        // leReferralLimitExceeded
    LDAP_RES_INVALID_RESPONSE,               // leInvalidResponse
    LDAP_RES_AMBIGUOUS_RESPONSE,             // leAmbiguousResponse
    LDAP_RES_TLS_NOT_SUPPORTED,              // leTlsNotSupported
    LDAP_RES_INTERMEDIATE_RESPONSE,          // leIntermediateResponse
    LDAP_RES_UNKNOWN_TYPE,                   // leUnknownType
    LDAP_RES_CANCELED,                       // leCanceled
    LDAP_RES_NO_SUCH_OPERATION,              // leNoSuchOperation
    LDAP_RES_TOO_LATE,                       // leTooLate
    LDAP_RES_CANNOT_CANCEL,                  // leCannotCancel
    LDAP_RES_ASSERTION_FAILED,               // leAssertionFailed
    LDAP_RES_AUTHORIZATION_DENIED);          // leAuthorizationDenied

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
  /// the OID used to specify TLdapClient.SearchPageSize
  // - https://ldapwiki.com/wiki/Wiki.jsp?page=Simple%20Paged%20Results%20Control
  LDAP_PAGED_RESULT_OID_STRING = '1.2.840.113556.1.4.319';
  /// the OID used to specify TLdapClient.SearchSDFlags
  // - https://ldapwiki.com/wiki/Wiki.jsp?page=LDAP_SERVER_SD_FLAGS_OID
  LDAP_SERVER_SD_FLAGS_OID     = '1.2.840.113556.1.4.801';

  /// OID of namingContexts attribute in the root DSE
  ASN1_OID_DSE_NAMINGCONTEXTS       = '1.3.6.1.4.1.1466.101.120.5';
  /// OID of altServer attribute in the root DSE
  ASN1_OID_DSE_ALTSERVER            = '1.3.6.1.4.1.1466.101.120.6';
  /// OID of supportedExtension attribute in the root DSE
  ASN1_OID_DSE_SUPPORTEDEXTENSION   = '1.3.6.1.4.1.1466.101.120.7';
  /// OID of supportedControl attribute in the root DSE
  ASN1_OID_DSE_SUPPORTEDCONTROL     = '1.3.6.1.4.1.1466.101.120.13';
  /// OID of supportedLDAPVersion attribute in the root DSE
  ASN1_OID_DSE_SUPPORTEDLDAPVERSION = '1.3.6.1.4.1.1466.101.120.15';

  /// OID of LDAPv3 "Password Modify" extended operation (RFC 3062)
  ASN1_OID_PASSWDMODIFY = '1.3.6.1.4.1.4203.1.11.1';
  /// OID of LDAPv3 "Who Am I" extended operation (RFC 4532)
  ASN1_OID_WHOAMI       = '1.3.6.1.4.1.4203.1.11.3';

type
  /// exception class raised by this unit
  ELdap = class(ESynException);

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

/// translate a LDAP_RES_* integer result code into our TLdapError enumerate
function RawLdapError(ErrorCode: integer): TLdapError;

/// translate a LDAP_RES_* integer result code into some human-readable text
// - searching for the ErrorCode within LDAP_RES_CODE[] values
// - use LDAP_ERROR_TEXT[RawLdapError()] if you only need the error text
function RawLdapErrorString(ErrorCode: integer; out Enum: TLdapError): RawUtf8; overload;

/// translate a LDAP_RES_* integer result code into some human-readable text
function RawLdapErrorString(ErrorCode: integer): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// encode a LDAP search filter text into its ASN.1 binary representation
// - as used by CLDAP raw functions and TLdapClient.Search()
// - in respect to the standard, this function allows no outter parenthesis,
// e.g. allows 'accountBalance<=1234' or '&(attr1=a)(attr2=b)'
// - on parsing error raise ELdap or return '' if NoRaise is true
function RawLdapTranslateFilter(const Filter: RawUtf8;
  NoRaise: boolean = false): TAsnObject;

/// encode the ASN.1 binary for a LDAP_ASN1_SEARCH_REQUEST
// - as used by CLDAP raw functions and TLdapClient.Search()
function RawLdapSearch(const BaseDN: RawUtf8; TypesOnly: boolean;
  Filter: RawUtf8; const Attributes: array of RawUtf8;
  Scope: TLdapSearchScope = lssBaseObject; Aliases: TLdapSearchAliases = lsaAlways;
  Sizelimit: integer = 0; TimeLimit: integer = 0): TAsnObject;

/// decode the ASN.1 binary of a LDAP_ASN1_SEARCH_ENTRY
// - and lookup by name the returned attributes as RawUtf8 variables
// - as used by CLDAP raw functions but not by TLdapClient.Search()
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
  LDAP_CN: TSynAnsicharSet = ['.', '/', '\'];

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

/// encode a "unicodePwd" binary value from a UTF-8 password
// - for extensive/paranoid anti-forensic measure, call FillZero() on the result
function LdapUnicodePwd(const aPassword: SpiUtf8): RawByteString;

/// decode a LDAP attribute date/time value into a pascal TDateTime
function LdapToDate(const Text: RawUtf8): TDateTime;

const
  // https://learn.microsoft.com/en-us/windows/win32/adsi/search-filter-syntax
  AND_FLAG = ':1.2.840.113556.1.4.803:';
  NESTED_FLAG: array[boolean] of RawUtf8 = (
    '', ':1.2.840.113556.1.4.1941:');

  // traditionally, computer sAMAccountName ends with $
  MACHINE_CHAR: array[boolean] of string[1] = ('', '$');


{ **************** LDAP Attributes Definitions }

type
  /// define how a TLdapAttributeType is actually stored in the LDAP raw value
  // - allow complex binary types to be decoded / made readable
  TLdapAttributeTypeStorage = (
    atsAny,
    atsRawUtf8,
    atsInteger,
    atsIntegerUserAccountControl,
    atsIntegerSystemFlags,
    atsIntegerGroupType,
    atsIntegerAccountType,
    atsFileTime,
    atsTextTime,
    atsSid,
    atsGuid,
    atsSecurityDescriptor,
    atsUnicodePwd);

  /// common Attribute Types, as stored in TLdapAttribute.AttributeName
  // - so that the most useful types could be specified as convenient enumerates
  // - allow complex binary types (like SID/GUID/FileTime) to be recognized and
  // properly decoded / made readable
  TLdapAttributeType = (
    atUndefined,
    atDistinguishedName,
    atObjectClass,
    atObjectCategory,
    atAlias,
    atName,
    atCommonName,
    atSurName,
    atGivenName,
    atDisplayName,
    atUserPrincipalName,
    atUserAccountControl,
    atSystemFlags,
    atSAMAccountName,
    atSAMAccountType,
    atAdminCount,
    atDescription,
    atGenerationQualifier,
    atInitials,
    atOrganizationName,
    atOrganizationUnitName,
    atMail,
    atMemberOf,
    atCountryName,
    atLocalityName,
    atStateName,
    atStreetAddress,
    atTelephoneNumber,
    atTitle,
    atSerialNumber,
    atMember,
    atOwner,
    atGroupType,
    atPrimaryGroupID,
    atNTSecurityDescriptor,
    atObjectSid,                   // encoded as binary RawSid
    atObjectGuid,                  // encoded as binary TGuid
    atLogonCount,
    atBadPwdCount,
    atDnsHostName,
    atAccountExpires,              // first 64-bit FileTime
    atBadPasswordTime,
    atLastLogon,
    atLastLogonTimestamp,
    atLastLogoff,
    atLockoutTime,
    atPwdLastSet,
    atMcsAdmPwdExpirationTime,     // last 64-bit FileTime
    atWhenCreated,                 // first date/time text
    atWhenChanged,                 // last date/time text
    atOperatingSystem,
    atOperatingSystemVersion,
    atServicePrincipalName,
    atUnicodePwd,
    atAccountNameHistory,
    atTokenGroups);                // virtual/constructed attribute for self

  /// set of common Attribute Types
  TLdapAttributeTypes = set of TLdapAttributeType;

var
  /// the standard "lDAPDisplayName" of our common Attribute Types
  // - these value will be interned and recognized internally as raw pointer()
  // - e.g. AttrTypeName[atOrganizationUnitName] = 'ou'
  // - by design, atUndefined would return ''
  AttrTypeName: array[TLdapAttributeType] of RawUtf8;

  /// alternate "lDAPDisplayName" of our common Attribute Types
  // - e.g. AttrTypeNameAlt[6] = 'organizationName' and
  // AttrTypeNameAlt[6] = atOrganizationUnitName
  // - defined for unit testing purpose only
  AttrTypeNameAlt: array[0 .. 7] of RawUtf8;

const
  // AttrTypeNameAlt[] types - defined for unit testing purpose only
  AttrTypeAltType: array[0 .. high(AttrTypeNameAlt)] of TLdapAttributeType = (
    atCommonName, atSurName, atCountryName, atLocalityName, atStateName,
    atStreetAddress, atOrganizationName, atOrganizationUnitName);

  /// the standard RDN of our common Attribute Types
  // - as retrieved from an actual AD instance catalog
  // - see AttrTypeName[] for the corresponding standard "lDAPDisplayName"
  AttrTypeCommonName: array[TLdapAttributeType] of RawUtf8 = (
    '',                            // atUndefined
    'Obj-Dist-Name',               // atDistinguishedName
    'Object-Class',                // atObjectClass
    'Object-Category',             // atObjectCategory
    'Alias',                       // atAlias
    'RDN',                         // atName
    'Common-Name',                 // atCommonName
    'Surname',                     // atSurName
    'Given-Name',                  // atGivenName
    'Display-Name',                // atDisplayName
    'User-Principal-Name',         // atUserPrincipalName
    'User-Account-Control',        // atUserAccountControl
    'System-Flags',                // atSystemFlags
    'SAM-Account-Name',            // atSAMAccountName
    'SAM-Account-Type',            // atSAMAccountType
    'Admin-Count',                 // atAdminCount
    'Description',                 // atDescription
    'Generation-Qualifier',        // atGenerationQualifier
    'Initials',                    // atInitials
    'Organization-Name',           // atOrganizationName
    'Organizational-Unit-Name',    // atOrganizationUnitName
    'E-mail-Addresses',            // atMail
    'Is-Member-Of-DL',             // atMemberOf
    'Country-Name',                // atCountryName
    'Locality-Name',               // atLocalityName
    'State-Or-Province-Name',      // atStateName
    'Street-Address',              // atStreetAddress
    'Telephone-Number',            // atTelephoneNumber
    'Title',                       // atTitle
    'Serial-Number',               // atSerialNumber
    'Member',                      // atMember
    'Owner',                       // atOwner
    'Group-Type',                  // atGroupType
    'Primary-Group-ID',            // atPrimaryGroupID
    'NT-Security-Descriptor',      // atNTSecurityDescriptor
    'Object-Sid',                  // atObjectSid
    'Object-Guid',                 // atObjectGuid
    'Logon-Count',                 // atLogonCount
    'Bad-Pwd-Count',               // atBadPwdCount
    'DNS-Host-Name',               // atDnsHostName
    'Account-Expires',             // atAccountExpires
    'Bad-Password-Time',           // atBadPasswordTime
    'Last-Logon',                  // atLastLogon
    'Last-Logon-Timestamp',        // atLastLogonTimestamp
    'Last-Logoff',                 // atLastLogoff
    'Lockout-Time',                // atLockoutTime
    'Pwd-Last-Set',                // atPwdLastSet
    'ms-Mcs-AdmPwdExpirationTime', // atMcsAdmPwdExpirationTime
    'When-Created',                // atWhenCreated
    'When-Changed',                // atWhenChanged
    'Operating-System',            // atOperatingSystem
    'Operating-System-Version',    // atOperatingSystemVersion
    'Service-Principal-Name',      // atServicePrincipalName
    'Unicode-Pwd',                 // atUnicodePwd
    'Account-Name-History',        // atAccountNameHistory
    'Token-Groups');               // atTokenGroups

  /// how all TLdapAttributeType are actually stored in the LDAP raw value
  AttrTypeStorage: array[TLdapAttributeType] of TLdapAttributeTypeStorage = (
    atsAny,                         // atUndefined
    atsRawUtf8,                     // atDistinguishedName
    atsRawUtf8,                     // atObjectClass
    atsRawUtf8,                     // otObjectCategory
    atsRawUtf8,                     // atAlias
    atsRawUtf8,                     // atName
    atsRawUtf8,                     // atCommonName
    atsRawUtf8,                     // atSurName
    atsRawUtf8,                     // atGivenName
    atsRawUtf8,                     // atDisplayName
    atsRawUtf8,                     // atUserPrincipalName
    atsIntegerUserAccountControl,   // atUserAccountControl
    atsIntegerSystemFlags,          // atSystemFlags
    atsRawUtf8,                     // atSAMAccountName
    atsIntegerAccountType,          // atSAMAccountType
    atsInteger,                     // atAdminCount
    atsRawUtf8,                     // atDescription
    atsRawUtf8,                     // atGenerationQualifier
    atsRawUtf8,                     // atInitials
    atsRawUtf8,                     // atOrganizationName
    atsRawUtf8,                     // atOrganizationUnitName
    atsRawUtf8,                     // atMail
    atsRawUtf8,                     // atMemberOf
    atsRawUtf8,                     // atCountryName
    atsRawUtf8,                     // atLocalityName
    atsRawUtf8,                     // atStateName
    atsRawUtf8,                     // atStreetAddress
    atsRawUtf8,                     // atTelephoneNumber
    atsRawUtf8,                     // atTitle
    atsRawUtf8,                     // atSerialNumber
    atsRawUtf8,                     // atMember
    atsRawUtf8,                     // atOwner
    atsIntegerGroupType,            // atGroupType
    atsInteger,                     // atPrimaryGroupID
    atsSecurityDescriptor,          // atNTSecurityDescriptor
    atsSid,                         // atObjectSid
    atsGuid,                        // atObjectGuid
    atsInteger,                     // atLogonCount
    atsInteger,                     // atBadPwdCount
    atsRawUtf8,                     // atDnsHostName
    atsFileTime,                    // atAccountExpires
    atsFileTime,                    // atBadPasswordTime
    atsFileTime,                    // atLastLogon
    atsFileTime,                    // atLastLogonTimestamp
    atsFileTime,                    // atLastLogoff
    atsFileTime,                    // atLockoutTime
    atsFileTime,                    // atPwdLastSet
    atsFileTime,                    // atMcsAdmPwdExpirationTime
    atsTextTime,                    // atWhenCreated
    atsTextTime,                    // atWhenChanged
    atsRawUtf8,                     // atOperatingSystem
    atsRawUtf8,                     // atOperatingSystemVersion
    atsRawUtf8,                     // atServicePrincipalName
    atsUnicodePwd,                  // atUnicodePwd
    atsRawUtf8,                     // atAccountNameHistory
    atsSid);                        // atTokenGroups

  /// the LDAP raw values stored as UTF-8, which do not require any conversion
  ATS_READABLE = [atsRawUtf8 .. atsIntegerAccountType];
  /// the LDAP raw values stored as integer
  ATS_INTEGER = [atsInteger .. atsIntegerAccountType];
  /// the LDAP raw values stored as raw binary
  // - always base-64 encoded in LDIF output
  ATS_BINARY = [atsSid, atsGuid, atsSecurityDescriptor];

  /// the LDAP attributes which are expected to have a single value
  // - i.e. have been defined with isSingleValued = TRUE in the AD Catalog
  // - atUndefined is part of this set, so that less common values will follow
  // the actual storage number for roKnownValuesAsArray in SearchAll()
  ATS_SINGLEVALUE = [
    atUndefined,
    atDistinguishedName, atObjectCategory, atName, atCommonName,
    atSurName, atDisplayName, atUserPrincipalName, atUserAccountControl,
    atSystemFlags, atSAMAccountName, atSAMAccountType, atAdminCount,
    atGenerationQualifier, atInitials, atMail, atCountryName, atLocalityName,
    atStateName, atStreetAddress, atTelephoneNumber, atTitle, atOwner,
    atGroupType, atPrimaryGroupID, atNTSecurityDescriptor, atObjectSid,
    atObjectGuid, atLogonCount, atBadPwdCount, atDnsHostName, atAccountExpires,
    atBadPasswordTime, atLastLogon, atLastLogonTimestamp, atLastLogoff,
    atLockoutTime, atPwdLastSet, atWhenCreated, atWhenChanged,
    atOperatingSystem, atOperatingSystemVersion, atUnicodePwd];

/// recognize our common Attribute Types from their standard NAME text
// - allow to use e.g. AttrTypeStorage[AttributeNameType(AttrName)]
function AttributeNameType(const AttrName: RawUtf8): TLdapAttributeType;

/// replace an attribute name with its known case-insensitive normalized value
// - i.e. all AttrTypeStorage[] known identifiers, and all previously used
// attribute names during the curent process lifetime
// - as done by the TLdapAttributeList class itself
procedure AttributeNameNormalize(var AttrName: RawUtf8);

/// convert in-place a raw attribute value into human-readable text
// - as used by TLdapAttribute.GetReadable/GetAllReadable
// - will detect SID, GUID, FileTime and text date/time known fields
// - if s is not truly UTF-8 encoded, will return its hexadecimal representation
// - returns TRUE if s was converted into hexadecimal
function AttributeValueMakeReadable(var s: RawUtf8; ats: TLdapAttributeTypeStorage;
  dom: PSid = nil; uuid: TAppendShortUuid = nil): boolean;

/// convert a set of common Attribute Types into their array text representation
// - by design, atUndefined would be excluded from the list
function ToText(Attributes: TLdapAttributeTypes): TRawUtf8DynArray; overload;

/// just a convenient redirection to AttrTypeName[Attribute]
function ToText(Attribute: TLdapAttributeType): RawUtf8; overload;
  {$ifdef HASINLINE} inline; {$endif}

type
  /// the decoded fields of TLdapGroup.GroupType
  // - https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/manage/understand-security-groups
  // - gtBuiltIn is created by the system
  // - gtGlobal has global scope
  // - gtDomainLocal has domain local scope
  // - gtUniversal has universal scope
  // - gtAppBasic specifies an APP_BASIC group for Windows Server Authorization Manager
  // - gtAppQuery specifies an APP_QUERY group for Windows Server Authorization Manager
  // - gtSecurity specifies a security group; if this flag is not set, then
  // the group is a distribution group
  TGroupType = (
    gtBuiltIn,
    gtGlobal,
    gtDomainLocal,
    gtUniversal,
    gtAppBasic,
    gtAppQuery,
    gtSecurity);

  /// define TLdapUser.GroupType decoded flags
  // - use GroupTypesFromInteger() GroupTypesFromText() and GroupTypesValue()
  // functions to encode/decode such values
  TGroupTypes = set of TGroupType;

  /// the decoded fields of TLdapUser.userAccountControl
  // - https://learn.microsoft.com/en-us/windows/win32/adschema/a-useraccountcontrol
  TUserAccountControl = (
    uacScript,                            //        1
    uacAccountDisable,                    //        2
    uacHomeDirRequired,                   //        8
    uacLockedOut,                         //       10 = 16
    uacPasswordNotRequired,               //       20 = 32
    uacPasswordCannotChange,              //       40 = 64
    uacPasswordUnencrypted,               //       80 = 128
    uacTempDuplicateAccount,              //      100 = 256
    uacNormalAccount,                     //      200 = 512
    uacInterDomainTrusted,                //      800 = 2048
    uacWorkstationTrusted,                //     1000 = 4096
    uacServerTrusted,                     //     2000 = 8192
    uacPasswordDoNotExpire,               //    10000 = 65536
    uacLogonAccount,                      //    20000 = 131072
    uacSmartcardRequired,                 //    40000 = 262144
    uacKerberosTrustedForDelegation,      //    80000 = 524288
    uacKerberosNotDelegated,              //   100000 = 1048576
    uacKerberosDesOnly,                   //   200000 = 2097152
    uacKerberosRequirePreAuth,            //   400000 = 4194304
    uacPasswordExpired,                   //   800000 = 8388608
    uacKerberosTrustedToDelegate,         //  1000000 = 16777216
    uacKerberosNoPac,                     //  2000000 = 33554432
    uacPartialSecretsRodc,                //  4000000 = 67108864
    uacUserUseAesKeys);                   // 80000000

  /// define TLdapUser.userAccountControl decoded flags
  // - use UserAccountControlsFromInteger() UserAccountControlsFromText() and
  // UserAccountControlsValue() functions to encode/decode such values
  TUserAccountControls = set of TUserAccountControl;

  /// known sAMAccountType values
  // - use SamAccountTypeFromInteger() SamAccountTypeFromText() and
  // SamAccountTypeValue() functions to encode/decode such values
  TSamAccountType = (
    satUnknown,
    satGroup,
    satNonSecurityGroup,
    satAlias,
    satNonSecurityAlias,
    satUserAccount,
    satMachineAccount,
    satTrustAccount,
    satAppBasicGroup,
    satAppQueryGroup);

  /// known systemFlags values
  TSystemFlag = (
    sfAttrNotReplicated,          //        1
    sfAttrReqPartialSetMember,    //        2
    sfAttrIsConstructed,          //        4
    sfAttrIsOperational,          //        8
    sfSchemaBaseObject,           //       10
    sfAttrIsRdn,                  //       20
    sfDomainDisallowMove,         //  4000000
    sfDomainDisallowRename,       //  8000000
    sfConfigAllowLimitedMove,     // 10000000
    sfConfigAllowMove,            // 20000000
    sfConfigAllowRename,          // 40000000
    sfConfigDisallowDelete);      // 80000000

  /// define systemFlags decoded flags
  // - use SystemFlagsFromInteger() SystemFlagsFromText() and
  // SystemFlagsValue() functions to encode/decode such values
  TSystemFlags = set of TSystemFlag;

  /// customize the TLdapAttributeList.Add(name, value) process
  // - default aoAlways will append the name/value pair to the existing content
  // - aoAlwaysFast always append the name/value, and do not resize the fList,
  // so you should call AfterAdd once done
  // - aoReplaceValue: if name already exists, replace its value
  // - aoKeepExisting: if name already exists, keep it and ignore the supplied value
  // - aoNoDuplicateValue: if value already exists as such, don't add it again
  TLdapAddOption = (
    aoAlways,
    aoAlwaysFast,
    aoReplaceValue,
    aoKeepExisting,
    aoNoDuplicateValue);

  /// customize TLdapResult.SearchAll/TLdapResultList.AppendTo output
  // - roTypesOnly will set "TypeOnly=true" for all Search() calls
  // - roSortByName will sort the TDocVariant resultset by its (nested) fields
  // - roAutoRange would detect "member;range=0-1499" paged members and populate
  // the "member" results with successive calls
  // - roNoDCAtRoot, roObjectNameAtRoot, roObjectNameWithoutDCAtRoot,
  // roCanonicalNameAtRoot and roCommonNameAtRoot will define how the object is
  // inserted and named in the output hierarchy - those options are exclusive
  // - by default, a "objectName" field is added, unless roNoObjectName is set
  // - a "canonicalName" field could be added if roWithCanonicalName is set
  // - roAllValuesAsArray will force all values to be returned as arrays, and
  // roKnownValuesAsArray detect ATS_SINGLEVALUE and store anything else as array
  // - atNTSecurityDescriptor recognizes known RID unless roNoSddlDomainRid is
  //  set; it won't recognize known ldapDisplayName unless roSddlKnownUuid is set
  // - roRawValues disable decoding of complex values (map all the following)
  // - roRawBoolean won't generate JSON true/false but keep "TRUE"/"FALSE" string
  // - roRawUac/roRawFlags/roRawGroupType/roRawAccountType disable decoding of
  // of atUserAccountControl/atSystemFlags/atGroupType/atAccountType values
  TLdapResultOptions = set of (
    roTypesOnly,
    roSortByName,
    roAutoRange,
    roNoDCAtRoot,
    roObjectNameAtRoot,
    roObjectNameWithoutDCAtRoot,
    roCanonicalNameAtRoot,
    roCommonNameAtRoot,
    roNoObjectName,
    roWithCanonicalName,
    roAllValuesAsArray,
    roKnownValuesAsArray,
    roNoSddlDomainRid,
    roSddlKnownUuid,
    roRawValues,
    roRawBoolean,
    roRawUac,
    roRawFlags,
    roRawGroupType,
    roRawAccountType);

  /// store a named LDAP attribute with the list of its values
  // - inherit from TClonable: Assign or Clone/CloneObjArray methods are usable
  TLdapAttribute = class(TClonable)
  protected
    fList: TRawByteStringDynArray;
    fAttributeName: RawUtf8;
    fCount: integer;
    fKnownType: TLdapAttributeType;
    fKnownTypeStorage: TLdapAttributeTypeStorage;
    fObjectSidIsDomain: boolean;
    procedure AssignTo(Dest: TClonable); override;
    procedure SetVariantOne(var v: TVarData; const s: RawUtf8;
      options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
    procedure SetVariantArray(var v: TDocVariantData;
      options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
    procedure SetKnownType(AttrType: TLdapAttributeType);
  public
    /// initialize the attribute(s) storage
    constructor Create(const AttrName: RawUtf8; AttrType: TLdapAttributeType); overload;
    /// finalize the attribute(s) storage
    destructor Destroy; override;
    /// clear all the internal fields
    procedure Clear;
    /// include a new value to this list
    procedure Add(const aValue: RawByteString;
      Option: TLdapAddOption = aoAlways); overload;
    /// include a new formatted text value to this list
    procedure AddFmt(const aValueFmt: RawUtf8; const aValueArgs: array of const;
      Option: TLdapAddOption = aoAlways);
    /// ensure Count = length(fItems) to allow proper "for res in Items do"
    // - is called e.g. by TLdapClient.Search after all its Add()
    procedure AfterAdd;
      {$ifdef HASINLINE} inline; {$endif}
    /// include new values to this list from another instance
    procedure AddFrom(Another: TLdapAttribute);
    /// retrieve a value as human-readable text
    // - wraps AttributeValueMakeReadable() and the known storage type
    function GetReadable(index: PtrInt = 0): RawUtf8; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve a value as human-readable text
    procedure GetReadable(index: PtrInt; var Value: RawUtf8); overload;
    /// retrieve all values as human-readable text
    function GetAllReadable: TRawUtf8DynArray;
    /// retrieve a value as its inital value stored with Add()
    // - return '' if the index is out of range, or the attribute is void
    function GetRaw(index: PtrInt = 0): RawByteString;
      {$ifdef HASINLINE} inline; {$endif}
    /// retrieve this attribute value(s) as a variant
    // - return null if there is no value (self=nil or Count=0)
    // - if there is a single value, return it as a single variant text
    // - if Count > 0, return a TDocVariant array with all texts
    function GetVariant(options: TLdapResultOptions = [];
      dom: PSid = nil; uuid: TAppendShortUuid = nil): variant;
    /// retrieve this attribute value(s) as a variant
    // - expects v to be fully zeroed (e.g. just allocated from a variant array)
    // - as called by GetVariant()
    procedure SetNewVariant(var v: variant;
      options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
    /// search for a given value within this list
    function FindIndex(const aValue: RawByteString): PtrInt;
    /// add all attributes as LDIF "dn: ###" entry, as specified by RFC 2849
    // - aHumanFriendly = TRUE will also append '# dn: <utf-8 content>' e.g. if
    // base-64 encoded was involved for human-friendly export
    procedure ExportToLdif(w: TTextWriter; aHumanFriendly: boolean;
      aMaxLineLen: PtrInt);
    /// save all attributes into a Modifier() / TLdapClient.Modify() ASN1_SEQ
    function ExportToAsnSeq: TAsnObject;
    /// how many values have been added to this attribute
    property Count: integer
      read fCount;
    /// name of this LDAP attribute
    // - this string instance has been interned to this unit, to reduce memory
    // allocation and enhance the lookup speed
    property AttributeName: RawUtf8
      read fAttributeName;
    /// the common LDAP Attribute Type corresponding to this AttributeName
    property KnownType: TLdapAttributeType
      read fKnownType write SetKnownType;
    /// the common LDAP Attribute Type Storage corresponding to this AttributeName
    property KnownTypeStorage: TLdapAttributeTypeStorage
      read fKnownTypeStorage;
    /// raw direct access to the individual values
    // - note that length(List) = capacity - use Count property instead, and
    // don't iterate on this array as "for u in attribute.List do..."
    property List: TRawByteStringDynArray
      read fList;
  end;
  /// dynamic array of LDAP attribute, as stored in TLdapAttributeList
  TLdapAttributeDynArray = array of TLdapAttribute;

  /// list one or several TLdapAttribute
  // - will use a global case-insensitive TRawUtf8InterningSlot hashed list of
  // names to minimize memory allocation, makes efficient lookup, and normalize
  // known TLdapAttributeType casing
  // - inherit from TClonable: Assign or Clone/CloneObjArray methods are usable
  TLdapAttributeList = class(TClonable)
  protected
    fItems: TLdapAttributeDynArray;
    fCount: integer;
    fKnownTypes: TLdapAttributeTypes;
    fIndexTypes: array[TLdapAttributeType] of byte; // index in fItems[] + 1
    procedure AssignTo(Dest: TClonable); override;
    function DoAdd(const aName: RawUtf8; aType: TLdapAttributeType): TLdapAttribute;
    procedure SetAttr(AttributeType: TLdapAttributeType; const Value: RawUtf8);
      {$ifdef HASINLINE} inline; {$endif}
    function GetUserAccountControl: TUserAccountControls;
    procedure SetUserAccountControl(Value: TUserAccountControls);
    procedure AfterModify;
  public
    /// initialize the attribute list with some type/value pairs
    constructor Create(const Types: array of TLdapAttributeType;
                       const Values: array of const); overload;
    /// finalize the list
    destructor Destroy; override;
    /// clear the list
    procedure Clear;
    /// search or allocate a new TLdapAttribute object to the list
    function Add(const AttributeName: RawUtf8): TLdapAttribute; overload;
    /// search or allocate a new TLdapAttribute object and its value to the list
    function Add(const AttributeName: RawUtf8; const AttributeValue: RawByteString;
      Option: TLdapAddOption = aoAlways): TLdapAttribute; overload;
    /// search or allocate TLdapAttribute object(s) from name/value pairs to the list
    procedure AddPairs(const NameValuePairs: array of RawUtf8;
      Option: TLdapAddOption = aoAlways); 
    /// search or allocate a new TLdapAttribute object to the list
    function Add(AttributeType: TLdapAttributeType): TLdapAttribute; overload;
    /// search or allocate a new TLdapAttribute object and its value to the list
    function Add(AttributeType: TLdapAttributeType; const AttributeValue: RawByteString;
      Option: TLdapAddOption = aoAlways): TLdapAttribute; overload;
    /// search or allocate TLdapAttribute object(s) from type/value to the list
    procedure Add(const Types: array of TLdapAttributeType;
      const Values: array of const; Option: TLdapAddOption = aoAlways); overload;
    /// search or allocate "unicodePwd" TLdapAttribute value to the list
    function AddUnicodePwd(const aPassword: SpiUtf8): TLdapAttribute;
    /// remove one TLdapAttribute object from the list
    procedure Delete(const AttributeName: RawUtf8); overload;
    /// find and return attribute index with the requested name
    // - returns -1 if not found
    function FindIndex(const AttributeName: RawUtf8;
      IgnoreRange: boolean = false): PtrInt; overload;
    /// find and return attribute with the requested name
    // - returns nil if not found
    function Find(const AttributeName: RawUtf8;
      IgnoreRange: boolean = false): TLdapAttribute; overload;
    /// find and return first attribute value with requested name
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    function GetByName(const AttributeName: RawUtf8): RawUtf8; 
    /// remove one TLdapAttribute object from the list
    procedure Delete(AttributeType: TLdapAttributeType); overload;
     /// remove one TLdapAttribute object from the list
    procedure Delete(Index: integer); overload;
   /// find and return attribute index with the requested attribute type
    // - returns -1 if not found
    // - faster than overloaded FindIndex(AttributeName)
    function FindIndex(AttributeType: TLdapAttributeType): PtrInt; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// find and return attribute with the requested attribute type
    // - returns nil if not found
    // - faster than overloaded Find(AttributeName)
    function Find(AttributeType: TLdapAttributeType): TLdapAttribute; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// find and return first attribute value with the requested type
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    // - faster than overloaded Get(AttributeName)
    function Get(AttributeType: TLdapAttributeType): RawUtf8; 
    /// find and return first attribute value with the requested type
    // - calls GetAllReadable on the found attribute
    function GetAll(AttributeType: TLdapAttributeType): TRawUtf8DynArray;
    /// access atSAMAccountType attribute value with proper decoding
    // - should never be set, because it is defined by the AD at object creation
    function AccountType: TSamAccountType;
    /// access atGroupType attribute value with proper decoding
    function GroupTypes: TGroupTypes;
    /// access atSystemFlags attribute value with proper decoding
    function SystemFlags: TSystemFlags;
    /// return the atObjectSid, if it is in a 'S-1-5-21-xx-xx-xx-RID' domain form
    function Domain: PSid;
    /// access atUserAccountControl attribute value with proper decoding/encoding
    property UserAccountControl: TUserAccountControls
      read GetUserAccountControl write SetUserAccountControl;
    /// access any attribute value from its known type
    // - calls GetReadable(0) to read, or Add(aoReplaceValue) to write
    // - returns empty string if not found
    // - is defined as the default property for convenience
    property Attr[AttributeType: TLdapAttributeType]: RawUtf8
      read Get write SetAttr; default;
    /// access to the internal list of TLdapAttribute objects
    // - note that length(Items) may be <> Count for this class, so you should
    // NEVER use an enumerate "for a in list.Items do" loop, but rely on Count
    property Items: TLdapAttributeDynArray
      read fItems;
    /// number of TLdapAttribute objects in this list
    property Count: integer
      read fCount;
    /// the common Attribute Types currently stored in this list
    property KnownTypes: TLdapAttributeTypes
      read fKnownTypes;
  end;

/// recognize the integer value stored in a LDAP atSAMAccountType entry as TSamAccountType
function SamAccountTypeFromText(const value: RawUtf8): TSamAccountType;
function SamAccountTypeFromInteger(value: cardinal): TSamAccountType;

/// convert a TSamAccountType as integer value stored in a LDAP atSAMAccountType entry
function SamAccountTypeValue(sat: TSamAccountType): integer;

/// recognize the text integer value stored in a LDAP atGroupType entry
function GroupTypesFromText(const value: RawUtf8): TGroupTypes;

/// recognize the integer value stored in a LDAP atGroupType entry
function GroupTypesFromInteger(value: integer): TGroupTypes;

/// compute the integer value stored in a LDAP atGroupType entry
function GroupTypesValue(gt: TGroupTypes): integer;

/// recognize the text integer value stored in a LDAP atUserAccountControl entry
function UserAccountControlsFromText(const value: RawUtf8): TUserAccountControls;

/// recognize the integer value stored in a LDAP atUserAccountControl entry
function UserAccountControlsFromInteger(value: integer): TUserAccountControls;

/// compute the integer value stored in a LDAP atUserAccountControl entry
function UserAccountControlsValue(uac: TUserAccountControls): integer;

/// recognize the text integer value stored in a LDAP atSystemFlags entry
function SystemFlagsFromText(const value: RawUtf8): TSystemFlags;

/// recognize the integer value stored in a LDAP atSystemFlags entry
function SystemFlagsFromInteger(value: integer): TSystemFlags;

/// compute the integer value stored in a LDAP atSystemFlags entry
function SystemFlagsValue(sf: TSystemFlags): integer;

function ToText(sat: TSamAccountType): PShortString; overload;
procedure ToTextTrimmed(sat: TSamAccountType; var text: RawUtf8); overload;

type
  /// some well-known reusable filters, used e.g. by ObjectFilter()
  // - depending on the kind of object, a complex combination of objectClass or
  // sAMAccountType is needed
  // - in the future, we are likely to add some new filters - feedback is welcome
  // - note that UserAccountControls/GroupType additional search criterias are
  // already handled by UacFilter() and GtFilter() so won't appear in this list
  TObjectFilter = (
    ofNone,
    ofAll,
    ofUsers,
    ofGroups,
    ofContacts,
    ofComputers,
    ofOrganizationalUnits,
    ofContainers,
    ofBuiltInContainers,
    ofDomain,
    ofTrustedDomain,
    ofGroupPolicies,
    ofServiceConnectionPoints,
    ofExpirableUsers);

const
  /// main Object LDAP query strings used by ObjectFilter()
  // - those filters are expected to be part of a &()()() block
  // - reference is hard to find, but we followed official MS documentation at
  // https://learn.microsoft.com/en-us/archive/technet-wiki/5392.active-directory-ldap-syntax-filters
  OBJECT_FILTER: array[TObjectFilter] of RawUtf8 = (
   '',                                              // ofNone
   '(objectClass=*)',                               // ofAll
   '(sAMAccountType=805306368)',                    // ofUsers
   // note: '(objectCategory=person)(objectClass=user)' is possible but slower
   '(objectCategory=group)',                        // ofGroups
   '(objectClass=contact)',                         // ofContacts
   '(objectCategory=computer)',                     // ofComputers
   '(objectCategory=organizationalUnit)',           // ofOrganizationalUnits
   '(objectCategory=container)',                    // ofContainers
   '(objectCategory=builtinDomain)',                // ofBuiltInContainers
   '(objectCategory=domain)',                       // ofDomain
   '(objectClass=trustedDomain)',                   // ofTrustedDomain
   '(objectCategory=groupPolicyContainer)',         // ofGroupPolicies
   '(objectClass=serviceConnectionPoint)',          // ofServiceConnectionPoints
   '(sAMAccountType=805306368)(accountExpires>=1)' +
     '(accountExpires<=9223372036854775806)');      // ofExpirableUsers

  /// TObjectFilter search criterias returning Users
  USER_FILTER     = [ofUsers, ofExpirableUsers];
  /// TObjectFilter search criterias returning Groups
  GROUP_FILTER    = [ofGroups];
  /// TObjectFilter search criterias returning Computers
  COMPUTER_FILTER = [ofComputers];

/// compute a TLdapClient.Search filter for a given account type
// - specify the entry by AccountName, DistinguishedName or UserPrincipalName
function ObjectFilter(Filter: TObjectFilter;
  const AccountName: RawUtf8 = ''; const DistinguishedName: RawUtf8 = '';
  const UserPrincipalName: RawUtf8 = ''; const CustomFilter: RawUtf8 = ''): RawUtf8;

function ToText(oft: TObjectFilter): PShortString; overload;
procedure ToTextTrimmed(oft: TObjectFilter; var text: RawUtf8); overload;

/// compute a custom filter according to included/excluded TUserAccountControls
// - typically used as CustomFilter parameter to ObjectFilter()
function UacFilter(Uac, unUac: TUserAccountControls): RawUtf8;

/// compute a custom filter according to included/excluded TGroupTypes
// - typically used as CustomFilter parameter to ObjectFilter()
function GtFilter(Gt, unGt: TGroupTypes): RawUtf8;


/// compute a sequence of modifications from its raw encoded attribute(s) sequence
function Modifier(Op: TLdapModifyOp; const Sequence: TAsnObject): TAsnObject; overload;

/// compute a sequence of modifications of a given attribute and its raw value
// - as used by TLdapClient.Modify(Obj, Op, AttrType, AttrValue)
// - the AttrValue should be properly encoded, as expected by the LDAP server
// - you can associate several Modifier() entries in a single TLdapClient.Modify
function Modifier(Op: TLdapModifyOp; AttrType: TLdapAttributeType;
  const AttrValue: RawByteString): TAsnObject; overload;

/// compute a sequence of modifications of a given attribute and its raw value
// - the AttrValue should be properly encoded, as expected by the LDAP server
// - you can associate several Modifier() entries in a single TLdapClient.Modify
function Modifier(Op: TLdapModifyOp; const AttrName: RawUtf8;
  const AttrValue: RawByteString): TAsnObject; overload;

/// compute a sequence of modifications of several attribute/raw value pairs
// - the Values should be properly encoded, as expected by the LDAP server
function Modifier(Op: TLdapModifyOp; const Types: array of TLdapAttributeType;
  const Values: array of const): TAsnObject; overload;

/// compute a sequence of modifications of several attribute/raw value pairs
// - here the modified attribute names are specified as text
function Modifier(Op: TLdapModifyOp;
  const NameValuePairs: array of RawUtf8): TAsnObject; overload;



{ **************** LDIF Data Interchange Format }

/// check if the supplied buffer requires base-64 encoding as RFC 2849 value
// - i.e. if p[0..l-1] contains SAFE-STRING = [SAFE-INIT-CHAR *SAFE-CHAR]
// - note that https://www.rfc-editor.org/errata/eid3646 states it applied to
// also to dn/rdn values
function IsLdifSafe(p: PUtf8Char; l: PtrInt): boolean;

const
  /// some default maximum line for our LDIF output
  MAX_LDIF_LINE = 80;

/// raw append of the supplied buffer value as specified by RFC 2849
// - humanfriendly=true will add '# attname: <utf-8 content>' comment line
// e.g. if base-64 encoding was involved, for human-friendly file export
// - as used e.g. by TLdapAttribute.ExportToLdif and TLdapResult.ExportToLdif
procedure AddLdif(w: TTextWriter; const v: RawByteString;
  forcebase64, humanfriendly: boolean; att: pointer{TLdapAttribute};
  maxlen: PtrInt);

type
  /// store a set of name/value attributes, for a given LDIF change type
  TLdifChange = class(TLdapAttributeList)
  protected
    fChangeType: TLdapModifyOp;
    procedure AssignTo(Dest: TClonable); override;
  public
    /// how the associated attributes should be applied
    // - by default, if not specified in the LDIF file, plain lmoAdd is assumed
    property ChangeType: TLdapModifyOp
      read fChangeType;
  end;
  /// dynamic array of LDIF changes, as stored in TLdifEntry
  TLdifChangeDynArray = array of TLdifChange;

  /// store a LDIF entry, associated with a given dn
  // - will reflect the exact content of a LDIF file section
  TLdifEntry = class(TClonable)
  protected
    fDn: RawUtf8;
    fItems: TLdifChangeDynArray;
    procedure AssignTo(Dest: TClonable); override;
  public
    /// finalize the list
    destructor Destroy; override;
    /// access to the internal list of TLdifChange objects
    property Items: TLdifChangeDynArray
      read fItems;
    property Dn: RawUtf8
      read fDn write fDn;
  end;
  /// dynamic array of LDIF entries, as stored in TLdifFile
  TLdifEntryDynArray = array of TLdifEntry;

  /// store all decoded LDIF content in memory
  // - will reflect the exact content of a LDIF file
  TLdifFile = class(TClonable)
  protected
    fItems: TLdifEntryDynArray;
    procedure AssignTo(Dest: TClonable); override;
  public
    /// finalize the list
    destructor Destroy; override;
    /// access to the internal list of TLdifChange objects
    property Items: TLdifEntryDynArray
      read fItems;
  end;


{ **************** LDAP Response Storage }

type
  /// store one LDAP result, i.e. one object name and associated attributes
  // - inherit from TClonable: Assign or Clone/CloneObjArray methods are usable
  TLdapResult = class(TClonable)
  protected
    fObjectName, fCanonicalName: RawUtf8;
    fAttributes: TLdapAttributeList;
    procedure AssignTo(Dest: TClonable); override;
    procedure SetObjectName(const Value: RawUtf8);
    function GetAttr(AttributeType: TLdapAttributeType): RawUtf8;
    function AppendToLocate(var Dvo: TDocVariantData;
      Options: TLdapResultOptions): PDocVariantData;
  public
    /// initialize the instance
    constructor Create; override;
    /// finalize the instance
    destructor Destroy; override;
    /// the Distinguised Name of this LDAP object
    // - will call NormalizeDN() if this property is manually set
    property ObjectName: RawUtf8
      read fObjectName write SetObjectName;
    /// return the Canonical Name of this LDAP object
    // - raise ELdap if we were not able to parse the ObjectName as DN
    function CanonicalName: RawUtf8;
      /// find and return first attribute value with the requested type
    function Get(AttributeType: TLdapAttributeType;
      out Value: RawUtf8): boolean;
    /// search an existing TLdapAttribute within the list
    function Find(const AttributeName: RawUtf8;
      IgnoreRange: boolean = false): TLdapAttribute;
    /// search an TLdapAttribute with the requested name or add one if none
    function FindOrAdd(const AttributeName: RawUtf8): TLdapAttribute;
    /// direct raw access to the internal list of attributes
    property Attributes: TLdapAttributeList
      read fAttributes;
    /// access any attribute value from its known type
    // - calls GetReadable(0) on the found attribute
    // - returns empty string if not found
    // - is defined as the default property for convenience
    property Attr[AttributeType: TLdapAttributeType]: RawUtf8
      read GetAttr; default;
    /// copy the 'objectSid' attribute if present
    // - return true on success
    function CopyObjectSid(out objectSid: RawUtf8): boolean;
    /// copy the binary 'objectGUID' attribute if present
    // - return true on success
    function CopyObjectGuid(out objectGuid: TGuid): boolean;
    /// add a "dn: ###" entry to a ldif-content buffer
    // - aHumanFriendly=true will add '# attname: <utf-8 content>' comment line
    // e.g. if base-64 encoding was involved, for human-friendly file export
    procedure ExportToLdif(w: TTextWriter; aHumanFriendly: boolean;
      aMaxLineLen: PtrInt);
  end;
  /// just a dynamic array of TLdapResult instances
  TLdapResultObjArray = array of TLdapResult;

  /// maintain a list of LDAP result objects
  // - inherit from TClonable: Assign or Clone/CloneObjArray methods are usable
  TLdapResultList = class(TClonable)
  protected
    fItems: TLdapResultObjArray;
    fCount: integer;
    fMicroSec, fIn, fOut: Int64;
    procedure AssignTo(Dest: TClonable); override;
    function GetElapsed: RawUtf8;
    procedure GetAttributes(const AttrName: RawUtf8; AttrType: TLdapAttributeType;
      ObjectNames: PRawUtf8DynArray; out Values: TRawUtf8DynArray);
    procedure ExtractPagedAttributes(Source: TLdapResultList);
  public
    /// finalize the list
    destructor Destroy; override;
    /// create and add new TLdapResult object to the list
    function Add: TLdapResult; overload;
    /// create and add new TLdapResult object with its name to the list
    function Add(const ObjectName: RawUtf8): TLdapResult; overload;
    /// search an existing TLdapResult object within the list
    function Find(const ObjectName: RawUtf8): TLdapResult;
    /// search an existing TLdapResult object within the list or add if none
    function FindOrAdd(const ObjectName: RawUtf8): TLdapResult; overload;
    /// search an existing TLdapAttribute within the list or add if none
    function FindOrAdd(const ObjectName, AttributeName: RawUtf8): TLdapAttribute; overload;
    /// ensure Count = length(fItems) to allow proper "for res in Items do"
    // - is called e.g. by TLdapClient.Search after all its Add()
    procedure AfterAdd;
      {$ifdef HASINLINE} inline; {$endif}
    /// clear all TLdapResult objects in list
    procedure Clear;
    /// return all Items[].ObjectName as a sorted array
    // - raise ELdap if any ObjectName cannot be parsed as a valid DN
    function ObjectNames(asCN: boolean = false;
      noSort: boolean = false): TRawUtf8DynArray;
    /// return all Items[].Attributes.Get(AttributeName)
    // - as a sorted array by default
    // - or as an unsorted array, but with all corresponding ObjectName values
    function ObjectAttributes(const AttributeName: RawUtf8;
      ObjectNames: PRawUtf8DynArray = nil): TRawUtf8DynArray; overload;
    /// return all Items[].Attributes.Get(AttrType)
    // - as a sorted array by default
    // - or as an unsorted array, but with all corresponding ObjectName values
    function ObjectAttributes(AttrType: TLdapAttributeType;
      ObjectNames: PRawUtf8DynArray = nil): TRawUtf8DynArray; overload;
    /// add all results as a TDocVariant object nested tree
    // - the full DN will be used as path, according to the options
    // - attributes would be included as ObjectAttributeField (e.g. '_attr')
    // fields (including the "objectName" value), unless ObjectAttributeField
    // is '', and no attribute will be set; if ObjectAttributeField is '*', no
    // sub-field will be generated, and attributes will be written directly
    // - as called by TLdapResultList.GetVariant and TLdapClient.SearchAll
    procedure AppendTo(var Dvo: TDocVariantData; Options: TLdapResultOptions;
      const ObjectAttributeField: RawUtf8; Dom: PSid = nil);
    /// export all results as a TDocVariant object variant
    function GetVariant(Options: TLdapResultOptions = [];
      const ObjectAttributeField: RawUtf8 = '*'): variant;
    /// export all results as a JSON object
    // - use a transient TDocVariant for the conversion
    function GetJson(Options: TLdapResultOptions = [];
      const ObjectAttributeField: RawUtf8 = '*';
      Format: TTextWriterJsonFormat = jsonCompact): RawUtf8;
    /// export all results as LDIF text, as specified by RFC 2849
    // - aHumanFriendly=true will add '# attname: <utf-8 content>' comment line
    // e.g. if base-64 encoding was involved, for human-friendly file export
    function ExportToLdifContent(aHumanFriendly: boolean = false;
      aMaxLineLen: PtrInt = MAX_LDIF_LINE): RawUtf8;
    /// dump the result of a LDAP search into human readable form
    // - used for debugging
    function Dump(NoTime: boolean = false): RawUtf8;
    /// access to the TLdapResult objects
    // - you can write e.g. "for res in Items do writeln(res.ObjectName)"
    property Items: TLdapResultObjArray
      read fItems;
    /// the time elapsed in microseconds on client side
    // - including command sending, receiving and result parsing
    property MicroSec: Int64
      read fMicroSec;
  published
    /// number of TLdapResult objects in list
    property Count: integer
      read fCount;
    /// the time elapsed in microseconds on client side
    // - including command sending, receiving and result parsing
    property Elapsed: RawUtf8
      read GetElapsed;
    /// how many bytes have been sent on the raw socket for these results
    property Sent: Int64
      read fOut;
    /// how many bytes have been received on the raw socket for these results
    property Recv: Int64
      read fIn;
  end;


{ **************** Main TLdapClient Class }

type
  /// well-known LDAP Objects, as defined from their GUID by Microsoft
  TLdapKnownObject = (
    lkoComputers,
    lkoDeletedObjects,
    lkoDomainControllers,
    lkoForeignSecurityPrincipals,
    lkoInfrastructure,
    lkoLostAndFound,
    lkoMicrosoftProgramData,
    lkoNtdsQuotas,
    lkoProgramData,
    lkoSystems,
    lkoUsers,
    lkoManagedServiceAccounts);

  /// the resultset of TLdapClient.GetWellKnownObject()
  TLdapKnownCommonNames = array [TLdapKnownObject] of RawUtf8;
  PLdapKnownCommonNames = ^TLdapKnownCommonNames;

  /// high-level information of a User or Group object in the LDAP database
  TLdapObject = object
  private
    procedure FillObject(Attributes: TLdapAttributeList;
      const CustomAttributes: TRawUtf8DynArray; CustomTypes: TLdapAttributeTypes);
    procedure CustomAdd(Attr: TLdapAttribute);
  public
    sAMAccountName, distinguishedName, canonicalName: RawUtf8;
    name, CN, description: RawUtf8;
    objectSid, objectGuid: RawUtf8;
    whenCreated, whenChanged: TDateTime;
    customNames: TRawUtf8DynArray;
    customValues: TRawUtf8DynArray;
    function Custom(const AttributeName: RawUtf8): RawUtf8;
  end;

  /// high-level information of a Computer in the LDAP database
  TLdapComputer = object(TLdapObject)
  public
    pwdLastSet, lastLogonTimestamp, admPwdExpirationTime: TDateTime;
    userAccountControl: TUserAccountControls;
    primaryGroupID, logonCount, badPwdCount: cardinal;
    dNSHostName, operatingSystem, operatingSystemVersion: RawUtf8;
    servicePrincipalName: TRawUtf8DynArray;
    procedure Fill(Attributes: TLdapAttributeList;
      const CustomAttributes: TRawUtf8DynArray;
      const CustomTypes: TLdapAttributeTypes);
  end;
  PLdapComputer = ^TLdapComputer;

  /// high-level information of a Group in the LDAP database
  // - note that "member" array won't include nested groups - use rather the
  // TLdapClient.GetIsMemberOf() method or TLdapCheckMember class instead
  TLdapGroup = object(TLdapObject)
  public
    primaryGroupID: cardinal;
    groupType: TGroupTypes;
    member: TRawUtf8DynArray;
    procedure Fill(Attributes: TLdapAttributeList; WithMember: boolean;
      const CustomAttributes: TRawUtf8DynArray;
      const CustomTypes: TLdapAttributeTypes);
  end;

  /// high-level information of a User in the LDAP database
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
    procedure Fill(Attributes: TLdapAttributeList; WithMemberOf: boolean;
      const CustomAttributes: TRawUtf8DynArray;
      const CustomTypes: TLdapAttributeTypes);
  end;
  PLdapUser = ^TLdapUser;

  /// how TLdapClient.Connect try to find the LDAP server if no TargetHost is set
  // - lccNoDiscovery overrides lccCldap/lccClosest and disable any DNS discovery
  // - default lccCldap will call CldapMyLdapController() to retrieve the
  // best possible LDAP server for this client
  // - lccClosest will make a round over the supplied addresses with a CLDAP
  // query over UDP, to find out the closest alive instances - also circumvent
  // if some AD were configured to drop and timeout more distant hosts
  // - default lccTlsFirst will try to connect as TLS on port 636 (if OpenSSL
  // is loaded)
  TLdapClientConnect = set of (
    lccNoDiscovery,
    lccCldap,
    lccClosest,
    lccTlsFirst);

  /// how a TLdapClient is connected to its associated LDAP Server
  TLdapClientTransmission = (
    lctNone,
    lctPlain,
    lctEncrypted);

  /// how a TLdapClient is authenticated to its associated LDAP Server
  TLdapClientBound = (
    lcbNone,
    lcbPlain,
    lcbDigest,
    lcbKerberos);

  /// define possible values for TLdapClient.SearchSDFlags
  // - LDAP_SERVER_SD_FLAGS_OID control
  TLdapSearchSDFlags = set of (
    lsfOwnerSecurityInformation,
    lsfGroupSecurityInformation,
    lsfDaclSecurityInformation,
    lsfSaclSecurityInformation);

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
    fKerberosDisableChannelBinding: boolean;
    fAutoReconnect: boolean;
    fAutoBind: TLdapClientBound;
    function GetTargetUri: RawUtf8;
    procedure SetTargetUri(const uri: RawUtf8);
  public
    /// initialize this instance
    constructor Create(const aUri: RawUtf8 = ''); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// run Connect and Bind of a temporary TLdapClient over TargetHost/TargetPort
    // - don't validate the password nor Kerberos auth, just TargetHost/TargetPort
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
    /// by default, a disconnect Server socket would raise a ELdap exception
    // - you can set this property to TRUE to try auto-reconnection with the
    // previous authentication method
    // - default value is TRUE
    property AutoReconnect: boolean
      read fAutoReconnect write fAutoReconnect;
    /// would call e.g. Bind or BindKerberos after Connect
    // - default is lcbNone for manual Bind/BindKerberos method call
    property AutoBind: TLdapClientBound
      read fAutoBind write fAutoBind;
    /// milliseconds timeout for socket operations
    // - default is 5000, ie. 5 seconds
    property Timeout: integer
      read fTimeout write fTimeout;
    /// the user identifier for non-anonymous Bind/BindSaslKerberos
    // - with Bind, should be a DN like 'CN=John,CN=Users,DC=mycompany,DC=tld',
    // as stated by the official LDAP specification - but note that some servers
    // (like Active Directory) allow displayName or even 'john@mycompany.tld'
    // - with BindSaslKerberos, on Linux or Windows it could be plain 'logonname'
    // but on MacOS it seems to be the fully qualified 'logonname@mycompany.tld'
    property UserName: RawUtf8
      read fUserName write fUserName;
    /// the user password for non-anonymous Bind/BindSaslKerberos
    // - if you can, use instead password-less Kerberos authentication, or
    // at least ensure the connection is secured via TLS
    // - as an alternative, on POSIX you can specify a keytab associated with
    // UserName as 'FILE:/full/path/to/my.keytab' into this property
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
    /// option to disable Channel Binding on Kerberos + TLS
    // - Microsoft will eventually (someday) make it mandatory for its AD servers
    // - needed also e.g. on Samba >= 4.20.3, if defined with non-default
    // $ ldap server require strong auth = allow_sasl_without_tls_channel_bindings
    property KerberosDisableChannelBinding: boolean
      read fKerberosDisableChannelBinding write fKerberosDisableChannelBinding;
  end;

  TLdapClient = class;

  /// callback signature used e.g. for TLdapClient.OnDisconnect event
  TOnLdapClientEvent = procedure(Sender: TLdapClient) of object;

  /// implementation of LDAP client version 2 and 3
  // - will default setup a TLS connection on the OS-designed LDAP server
  // - Authentication will use Username/Password properties
  // - is not thread-safe, but you can call Lock/UnLock to share the connection
  TLdapClient = class(TObjectOSLock)
  protected
    fSettings: TLdapClientSettings;
    fSock: TCrtSocket;
    fVersion: integer;
    fSeq: integer;
    fResponseCode: integer;
    fResultCode: integer;
    fResultString: RawUtf8;
    fResponseDN: RawUtf8;
    fReferals: TRawUtf8List;
    fBound: boolean;
    fBoundAs: TLdapClientBound;
    fBoundDigestAlgo: TDigestAlgo; // for Reconnect
    fFlags: set of (
      fSecContextEncrypt, fRetrieveRootDseInfo, fRetrievedDefaultDNInfo);
    fResultError: TLdapError;
    fSearchScope: TLdapSearchScope;
    fSearchAliases: TLdapSearchAliases;
    fSearchSDFlags: TLdapSearchSDFlags;
    fSearchSizeLimit: integer;
    fSearchTimeLimit: integer;
    fSearchPageSize: integer;
    fSearchPageCount: integer;
    fSearchCookie: RawUtf8;
    fSearchResult: TLdapResultList;
    fSearchRange: TLdapResultList;
    fDefaultDN, fRootDN, fConfigDN, fVendorName, fServiceName: RawUtf8;
    fNetbiosDN: RawUtf8;
    fMechanisms, fControls, fExtensions, fNamingContexts: TRawUtf8DynArray;
    fSecContext: TSecContext;
    fBoundUser: RawUtf8;
    fBoundKerberosAuthIdentify: RawUtf8; // for Reconnect
    fSockBuffer: RawByteString;
    fFullResult: TAsnObject;
    fDomainSid: RawSid;
    fTlsContext: TNetTlsContext;
    fSearchBeginBak: TIntegerDynArray; // SearchPageSize (recursive) backup
    fSearchBeginCount: integer; // usually = only 0..1
    fSockBufferPos: integer;
    fLog: TSynLogClass;
    fOnDisconnect: TOnLdapClientEvent;
    fWellKnownObjects: TLdapKnownCommonNames;
    // protocol methods
    function GetTlsContext: PNetTlsContext;
      {$ifdef HASINLINE} inline; {$endif}
    function BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
    procedure SendPacket(const Asn1Data: TAsnObject);
    procedure ReceivePacket(Dest: pointer; DestLen: PtrInt); overload;
    procedure ReceivePacket(var Append: RawByteString; Len: PtrInt); overload;
    procedure ReceivePacketFillSockBuffer;
    function ReceiveResponse: TAsnObject;
    function DecodeResponse(var Pos: integer; const Asn1Response: TAsnObject): TAsnObject;
    function SendAndReceive(const Asn1Data: TAsnObject): TAsnObject;
    // internal wrapper methods
    procedure GetAllValues(Filter: TObjectFilter; Flags, noFlags: integer;
      const BaseDN, CustomFilter, Match: RawUtf8; Attribute: TLdapAttributeType;
      out Res: TRawUtf8DynArray; ObjectNames: PRawUtf8DynArray);
    function SearchMissing(const ObjectName: RawUtf8; Attribute: TLdapAttribute): integer;
    procedure SearchMissingAttributes(var Result: TDocVariantData;
      Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8); overload;
    procedure SearchMissingAttributes; overload;
    procedure RetrieveRootDseInfo;
    procedure RetrieveDefaultDNInfo;
    procedure Reset(reconnect: boolean);
    procedure SetUnknownError(const msg: RawUtf8); overload;
    procedure SetUnknownError(const fmt: RawUtf8; const args: array of const); overload;
    function DoBind(Mode: TLdapClientBound): boolean;
    function Reconnect(const context: ShortString): boolean;
  public
    /// initialize this LDAP client instance
    constructor Create; overload; override;
    /// initialize this LDAP client instance with the given settings
    // - which may be persisted as JSON e.g. into a TSynAutoCreateFields holder
    constructor Create(aSettings: TLdapClientSettings); reintroduce; overload;
    /// finalize this LDAP client instance
    destructor Destroy; override;

    { connection methods }

    /// try to connect to LDAP server at socket level
    // - without any authentication: consider using Bind/BindSaslKerberos instead
    // - if no TargetHost/TargetPort/FullTls has been set, will try the OS
    // DnsLdapControlers() hosts (from mormot.net.dns) following DiscoverMode
    // - do nothing if was already connected
    function Connect(DiscoverMode: TLdapClientConnect = [lccCldap, lccTlsFirst];
      DelayMS: integer = 500): boolean;
    /// the published "rootDomainNamingContext" attribute in the Root DSE
    // - a typical value is e.g. 'DC=ad,DC=company,DC=it'
    // - use an internal cache for fast retrieval
    function RootDN: RawUtf8;
    /// the published "defaultNamingContext" attribute in the Root DSE
    // - is the same as RootDN when domain isn't a subdomain
    // - a typical value is e.g. 'DC=ad,DC=company,DC=it'
    // - use an internal cache for fast retrieval
    function DefaultDN(const BaseDN: RawUtf8 = ''): RawUtf8;
    /// the published "vendorName" attribute in the Root DSE
    // - a typical value is e.g. 'Samba Team (https://www.samba.org)'
    // - use an internal cache for fast retrieval
    function VendorName: RawUtf8;
    /// the published "ldapServiceName" attribute in the Root DSE
    // - a typical value is e.g. 'ad.company.it:dc-main@AD.COMPANY.IT'
    // - use an internal cache for fast retrieval
    function ServiceName: RawUtf8;
    /// the published "configurationNamingContext" attribute in the Root DSE
    // - use an internal cache for fast retrieval
    function ConfigDN: RawUtf8;
    /// the NETBIOS domain name, empty string if not found
    // - retrieved from the CN=Partitions of this server's ConfigDN
    // - use an internal cache for fast retrieval
    function NetbiosDN: RawUtf8;
    /// the published "namingContexts" attribute in the Root DSE
    // - use an internal cache for fast retrieval
    function NamingContexts: TRawUtf8DynArray;
    /// the authentication mechanisms supported on this LDAP server
    // - returns e.g. ['GSSAPI','GSS-SPNEGO','EXTERNAL','DIGEST-MD5']
    // - from the published "supportedSASLMechanisms" attribute in the Root DSE
    // - use an internal cache for fast retrieval
    function Mechanisms: TRawUtf8DynArray;
    /// the controls supported on this LDAP server
    // - the OIDs are returned sorted, and de-duplicated
    // - from the published "supportedControl" attribute in the Root DSE
    // - use an internal cache for fast retrieval
    function Controls: TRawUtf8DynArray;
    /// the LDAP v3 extensions supported on this LDAP server
    // - the OIDs are returned sorted, and de-duplicated
    // - use an internal cache for fast retrieval
    // - note that SambaAD/OpenLDAP seems to not publish anything under the
    // 'supportedExtension' attribute, as it should
    function Extensions: TRawUtf8DynArray;
    /// search if the server supports a given authentication mechanism by name
    // - a typical value to search is e.g. 'GSSAPI' or 'DIGEST-MD5'
    // - search is case-insensitive
    function SupportsMech(const MechanismName: RawUtf8): boolean;
    /// search if the server supports a given LDAP control by name
    // - a typical value to search is e.g. '1.2.840.113556.1.4.319'
    // (LDAP_PAGED_RESULT_OID_STRING) or '1.2.840.113556.1.4.801'
    // (LDAP_SERVER_SD_FLAGS_OID)
    // - use a very fast O(log(n)) binary search inside the memory cache
    function SupportsControl(const ControlName: RawUtf8): boolean;
    /// search if the server supports a given LDAP v3 extension by name
    // - a typical value to search is e.g. ASN1_OID_WHOAMI
    // - use a very fast O(log(n)) binary search inside the memory cache
    // - note that SambaAD/OpenLDAP seems to not publish anything under the
    // 'supportedExtension' attribute, as it should
    function SupportsExt(const ExtensionName: RawUtf8): boolean;
    /// retrieve a well known object DN or CN text value for the DefaultDN
    // - use an internal cache for faster retrieval
    function WellKnownObject(WellKnown: TLdapKnownObject;
      AsCN: boolean = false): RawUtf8;
    /// retrieve the raw binary domain SID for the DefaultDN
    // - use an internal cache for fast retrieval
    function DomainSid: RawSid;

    { binding methods }

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
    /// test whether the client socket is connected to the server
    function Connected: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// test whether the client is connected to the server and try re-connect
    // - follows Settings.AutoReconnect property and OnDisconnect event
    function EnsureConnected(const context: ShortString): boolean;
    /// test whether the client is connected with TLS or Kerberos Signing-Sealing
    // - it is unsafe to send e.g. a plain Password without lctEncrypted
    function Transmission: TLdapClientTransmission;
    /// close the connection to the LDAP server, sending an Unbind message
    function Close: boolean;
    /// low-level call of LDAP v3 extended operations
    // - e.g. StartTLS, cancel, transactions, user password change
    // - called e.g. by ExtModifyUserPassword()
    function Extended(const Oid: RawUtf8; const Value: TAsnObject;
      RespName: PRawUtf8; RespValue: PAsnObject): boolean;
    /// retrieves the current authorization identity for the client connection
    // - calls the LDAPv3 "Who Am I" extended operation (as defined in RFC 4532)
    // - the BoundUser property is the value supplied at connection, whereas
    // this value is an authzId returned by the server (e.g. 'u:xxyyz@EXAMPLE.NET')
    // as defined in https://www.rfc-editor.org/rfc/rfc4513#section-5.2.1.8
    // - If the client is authenticated as a Windows security principal, the
    // authzId returned in the response will contain the string 'u:' followed
    // by either (1) the NetBIOS domain name, followed by a backslash (\),
    // followed by the sAMAccountName of the security principal, or (2) the
    // SID of the security principal, in SDDL SID string format
    // - If the client is authenticated as an AD LDS security principal, the
    // returned authzId will contain the string 'dn:' followed by the DN of
    // the security principal
    // - If the client has not authenticated, the returned authzId will be
    // the empty string
    function ExtWhoAmI: RawUtf8;

    { read methods }

    /// enable paging for the searches
    // - you can then loop calling Search() until it returns an empty result,
    // and eventually SearchEnd when done with this query
    // - is just a wrapper to set SearchPageSize, with a backup of the current
    // value (allowing nested calls)
    procedure SearchBegin(PageSize: integer = 1000);
    /// finalize paging for the searches
    // - is just a wrapper to reset SearchPageSize and the SearchCookie
    procedure SearchEnd;
    /// enable "###;range=0-1499" paging attribute detecting for the searches
    // - should finally call one of the SearchRangeEnd overloaded methods
    // - since Search() may have its own paging, we need to defer paging
    // attributes retrieval after the main request, via eventual SearchRangeEnd
    // - as used e.g. by SearchAll() with the roAutoRange option
    procedure SearchRangeBegin;
    /// finalize "###;range=0-1499" paging attribute detection into self
    // - this method will ask for all remaining paged attributes, and
    // consolidate all values into the main SearchResult
    // - could be used e.g. to search all groups with pagined "member" as
    // ! SearchRangeBegin;
    // ! Search(DefaultDN, false, ObjectFilter(ofGroups), []);
    // ! SearchRangeEnd; // any paginated attributes will be retrieved here
    procedure SearchRangeEnd; overload;
    /// finalize "member;range=0-1499" paging attribute detection as variant
    // - this method will ask for all remaining paged attributes, and
    // consolidate all values into the supplied TDocVariantData
    // - as used e.g. by SearchAll() with the roAutoRange option
    procedure SearchRangeEnd(var Result: TDocVariantData;
      Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8); overload;
    /// retrieve all entries that match a given set of criteria
    // - will generate as many requests/responses as needed to retrieve all
    // the information into the SearchResult property
    // - if paging has been enabled (e.g. with SearchBegin), you should call
    // and process the SearchResult several times, until SearchCookie is ''
    // - by default, all attributes would be retrieved, unless a specific set
    // of Attributes is supplied; if you want no attribute, use ['']
    // - return true on success (ResultError = leSuccess)
    // - may return false and ResultError = leSizeLimitExceeded if too many
    // items were found: in this case, the first page is available in SearchResult
    // but you should enable pagination and use SearchBegin/SearchEnd or SearchAll
    function Search(const BaseDN: RawUtf8; TypesOnly: boolean;
      const Filter: RawUtf8; const Attributes: array of RawUtf8): boolean; overload;
    /// retrieve all entries that match a given set of criteria
    // - here the filter is generated using FormatUtf8()
    function SearchFmt(const BaseDN: RawUtf8; TypesOnly: boolean;
      const FilterFmt: RawUtf8; const FilterArgs: array of const;
      const Attributes: array of RawUtf8): boolean; overload;
    /// retrieve all entries a given set of criteria and return the first result
    // - will call Search() method, therefore SearchResult will contains all
    // matching entries
    // - returns nil if no result is found or if the search failed
    function SearchFirst(const BaseDN, Filter: RawUtf8;
      const Attributes: array of RawUtf8): TLdapResult; overload;
    /// retrieve the entry matching the given ObjectDN
    // - will call Search method, therefore SearchResult will contains all the results
    // - returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN, Filter: RawUtf8;
      const Attributes: array of RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapResult; overload;
    /// retrieve the attribute matching the given ObjectDN and Attribute
    // - returns nil if the object is not found or if the search failed
    function SearchObject(const ObjectDN, Filter, Attribute: RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapAttribute; overload;
    /// retrieve all pages of entries into a TDocVariant kind of variant
    // - won't suffer from leSizeLimitExceeded since calls SearchBegin/SearchEnd
    // - will contain the nested results as an object, generated from the
    // returned object canonical names
    // - attributes would be added as ObjectAttributeField (e.g. '_attr') fields,
    // unless ObjectAttributeField is '', and no attribute will be added, or
    // ObjectAttributeField is '*', and attributes are written as no sub-field
    // (which is the default behavior)
    function SearchAllRaw(const BaseDN, Filter: RawUtf8;
      const Attributes: array of RawUtf8; Options: TLdapResultOptions;
      const ObjectAttributeField: RawUtf8 = '*'; MaxCount: integer = 0): variant;
    /// retrieve all pages of entries into a TDocVariant instance
    // - as used by overloaded SearchAll*() functions
    // - Dest.Init will be called first, so Dest should be un-initialized or
    // caller should have properly made Dest.Clear before executing this method
    function SearchAllDocRaw(out Dest: TDocVariantData;
      const BaseDN, Filter: RawUtf8; const Attributes: array of RawUtf8;
      Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8 = '*';
      MaxCount: integer = 0; PerPage: integer = 1000): boolean;
    /// retrieve all entries that match a given set of criteria
    // - may return false and ResultError = leSizeLimitExceeded if too many
    // items were found: in this case, the first page is available in SearchResult
    // but you should enable pagination and use SearchBegin/SearchEnd or SearchAll
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function Search(const Attributes: TLdapAttributeTypes;
      const Filter: RawUtf8 = ''; const BaseDN: RawUtf8 = '';
      TypesOnly: boolean = false): boolean; overload;
    /// retrieve all entries that match a given set of criteria
    // - may return false and ResultError = leSizeLimitExceeded if too many
    // items were found: in this case, the first page is available in SearchResult
    // but you should enable pagination and use SearchBegin/SearchEnd or SearchAll
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchFmt(const Attributes: TLdapAttributeTypes;
      const FilterFmt: RawUtf8; const FilterArgs: array of const;
      const BaseDN: RawUtf8 = ''; TypesOnly: boolean = false): boolean; overload;
    /// retrieve all entries a given set of criteria and return the first result
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchFirst(const Attributes: TLdapAttributeTypes;
      const Filter: RawUtf8; const BaseDN: RawUtf8 = ''): TLdapResult; overload;
    /// retrieve all entries a given set of criteria and return the first result
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchFirstFmt(const Attributes: TLdapAttributeTypes;
      const FilterFmt: RawUtf8; const FilterArgs: array of const;
      const BaseDN: RawUtf8 = ''): TLdapResult;
    /// retrieve the entry matching the given ObjectDN
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchObject(const Attributes: TLdapAttributeTypes;
      const ObjectDN, Filter: RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapResult; overload;
    /// retrieve the attribute matching the given ObjectDN and Attribute
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchObject(Attribute: TLdapAttributeType;
      const ObjectDN, Filter: RawUtf8;
      Scope: TLdapSearchScope = lssBaseObject): TLdapAttribute; overload;
    /// retrieve all pages of entries into a TDocVariant kind of variant
    // - won't suffer from leSizeLimitExceeded since calls SearchBegin/SearchEnd
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchAll(const Attributes: TLdapAttributeTypes;
      const Filter: RawUtf8; Options: TLdapResultOptions = [];
      const ObjectAttributeField: RawUtf8 = '*';
      const BaseDN: RawUtf8 = ''; MaxCount: integer = 0): variant; 
    /// retrieve all pages of entries into a TDocVariant instance
    // - overloaded method using convenient TLdapAttributeTypes for Attributes
    function SearchAllDoc(out Dest: TDocVariantData;
      const Attributes: TLdapAttributeTypes; const Filter: RawUtf8;
      Options: TLdapResultOptions = []; const ObjectAttributeField: RawUtf8 = '*';
      const BaseDN: RawUtf8 = ''; MaxCount: integer = 0): boolean; 
    /// determine whether a given entry has a specified attribute value
    function Compare(const Obj, AttrName, AttrValue: RawUtf8): boolean;

    { write methods }

    /// create a new entry in the directory
    function Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
    /// make one or more changes to an entry
    // - the Modifications are one or several Modifier() operations
    // - is the main modification method, called by other Modify() overloads
    function Modify(const Obj: RawUtf8;
      const Modifications: array of TAsnObject): boolean; overload;
    /// make one change as specified by attribute type and raw value
    // - the AttrValue should be properly encoded, as expected by the LDAP server
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      AttrType: TLdapAttributeType; const AttrValue: RawByteString): boolean; overload;
    /// make one change as specified by attribute type and raw value
    // - the AttrValue should be properly encoded, as expected by the LDAP server
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      const AttrName: RawUtf8; const AttrValue: RawByteString): boolean; overload;
    /// make one or more changes with several attribute/raw value pairs
    // - the Values should be properly encoded, as expected by the LDAP server
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      const Types: array of TLdapAttributeType;
      const Values: array of const): boolean; overload;
    /// make one or more changes to the set of attribute values in an entry
    function Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
      Attribute: TLdapAttribute): boolean; overload;
    /// change an entry Distinguished Name
    // - it can be used to rename the entry (by changing its RDN), move it to a
    // different location in the DIT (by specifying a new parent entry), or both
    function ModifyDN(const Obj, NewRdn, NewSuperior: RawUtf8;
      DeleteOldRdn: boolean): boolean;
    ///  remove an entry from the directory server
    // - if the object has children and DeleteChildren is false, the deletion
    // will not work and the result will be LDAP_RES_NOT_ALLOWED_ON_NON_LEAF
    function Delete(const Obj: RawUtf8; DeleteChildren: boolean = false): boolean;

    { high-level computer methods }

    /// retrieve all User names in the LDAP Server
    // - you can refine your query via CustomFilter or TUserAccountControls
    // - Match allow to search as a (AttributeName=Match) filter
    // - returns the sAMAccountName values by default, optionally with the
    // associated atDistinguishedName values
    function GetComputers(FilterUac: TUserAccountControls = [];
      UnFilterUac: TUserAccountControls = [uacAccountDisable];
      const Match: RawUtf8 = ''; const CustomFilter: RawUtf8 = '';
      const BaseDN: RawUtf8 = ''; ObjectNames: PRawUtf8DynArray = nil;
      Attribute: TLdapAttributeType = atSAMAccountName): TRawUtf8DynArray;
    /// retrieve the basic information of a LDAP Computer
    // - could lookup by sAMAccountName or distinguishedName
    function GetComputerInfo(const AccountName, DistinguishedName: RawUtf8;
      out Info: TLdapComputer; const BaseDN: RawUtf8 = '';
      const CustomAttributes: TRawUtf8DynArray = nil;
      const CustomTypes: TLdapAttributeTypes = []): boolean;
    /// Add a new computer in the domain
    // - If password is empty, it isn't set in the attributes
    // - If DeleteIfPresent is false and there is already a computer with this
    // name in the domain, the operation fail
    // - ErrorMessage contains the failure reason (if the operation failed)
    // - return false if the operation failed
    function AddComputer(const ComputerParentDN, ComputerName: RawUtf8;
      out ErrorMessage: RawUtf8; const Password: SpiUtf8 = '';
      DeleteIfPresent : boolean = false;
      UserAccount: TUserAccountControls = [uacWorkstationTrusted]): boolean;

    { high-level user/group methods }

    /// retrieve all Group names in the LDAP Server
    // - you can refine your query via CustomFilter or TGroupTypes
    // - Match allow to search as a (AttributeName=Match) filter
    // - returns the sAMAccountName values by default, optionally with the
    // associated atDistinguishedName values
    function GetGroups(FilterUac: TGroupTypes = [];
      UnFilterUac: TGroupTypes = []; const Match: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''; const BaseDN: RawUtf8 = '';
      ObjectNames: PRawUtf8DynArray = nil;
      Attribute: TLdapAttributeType = atSAMAccountName): TRawUtf8DynArray;
    /// retrieve all User names in the LDAP Server
    // - you can refine your query via CustomFilter or TUserAccountControls
    // - Match allow to search as a (AttributeName=Match) filter
    // - returns the sAMAccountName values by default, optionally with the
    // associated atDistinguishedName values
    function GetUsers(FilterUac: TUserAccountControls = [];
      UnFilterUac: TUserAccountControls = [uacAccountDisable];
      const Match: RawUtf8 = ''; const CustomFilter: RawUtf8 = '';
      const BaseDN: RawUtf8 = ''; ObjectNames: PRawUtf8DynArray = nil;
      Attribute: TLdapAttributeType = atSAMAccountName): TRawUtf8DynArray;
    /// retrieve the basic information of a LDAP Group
    // - could lookup by sAMAccountName or distinguishedName
    function GetGroupInfo(const AccountName, DistinguishedName: RawUtf8;
      out Info: TLdapGroup;
      const BaseDN: RawUtf8 = ''; WithMember: boolean = false;
      const CustomAttributes: TRawUtf8DynArray = nil;
      const CustomTypes: TLdapAttributeTypes = []): boolean;
    /// retrieve the distinguishedName of a Group from its sAMAccountName
    function GetGroupDN(const AccountName: RawUtf8; const BaseDN: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''): RawUtf8;
    /// retrieve the last integer of a Group SID from its sAMAccountName
    // or distinguishedName
    // - could be used to check an user primaryGroupID attribute, which is
    // likely to not be part of the "member" array of the (e.g. domain) group
    function GetGroupPrimaryID(const AccountName, DistinguishedName: RawUtf8;
      out PrimaryGroupID: cardinal; const BaseDN: RawUtf8 = '';
      const CustomFilter: RawUtf8 = ''): boolean;
    /// retrieve the basic information of a LDAP User
    // - could lookup by sAMAccountName, distinguishedName or userPrincipalName
    function GetUserInfo(
      const AccountName, DistinguishedName, UserPrincipalName: RawUtf8;
      out Info: TLdapUser;
      const BaseDN: RawUtf8 = ''; WithMemberOf: boolean = false;
      const CustomAttributes: TRawUtf8DynArray = nil;
      const CustomTypes: TLdapAttributeTypes = []): boolean;
    /// retrieve the distinguishedName of a User from its sAMAccountName
    // or userPrincipalName
    // - can optionally return its primaryGroupID attribute, which is
    // likely to not be part of the "member" array of the (e.g. domain) group
    function GetUserDN(const AccountName, UserPrincipalName: RawUtf8;
      const BaseDN: RawUtf8 = ''; const CustomFilter: RawUtf8 = '';
      PrimaryGroupID: PCardinal = nil; ObjectSid: PRawUtf8 = nil;
      ObjectKind: TObjectFilter = ofUsers): RawUtf8;
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
    /// change a user's password using regular Modify() method
    // - raise ELdap if the connection is anonymous or not encrypted
    // - non-void UserDN is typically 'uid=jdoe,ou=People,dc=example,dc=com'
    // - OldPassword is mandatory for normal users, but admins can leave it to ''
    function ModifyUserPassword(const UserDN: RawUtf8;
      const OldPassword, NewPassword: SpiUtf8): boolean;
    /// change a user's password using RFC 3062 extension
    // - raise ELdap if the connection is anonymous or not encrypted
    // - non-void UserDN is typically 'uid=jdoe,ou=People,dc=example,dc=com'
    // - if NewPassword was specified, returns NewPassword on success
    // - if NewPassword was not specified, returns the server-generated password
    function ExtModifyUserPassword(const UserDN: RawUtf8;
      const OldPassword, NewPassword: SpiUtf8): SpiUtf8;

    /// true after a successful call to Bind or BindSaslKerberos
    property Bound: boolean
      read fBound;
    /// contains the connected user, after Bind or BindSaslKerberos
    // - an anonymous connection will return ''
    property BoundUser: RawUtf8
      read fBoundUser;
    /// binary string of the last full response from LDAP server
    // - this string is encoded by ASN.1 BER encoding
    // - only needed for debugging
    property FullResult: TAsnObject
      read fFullResult;
    /// optional advanced options for FullTls = true
    // - we define a pointer to the record and not directly a record property
    // to allow direct modification of any property of the record
    // - by default, IgnoreCertificateErrors is set to true by Create - you can
    // e.g. change this default behavior (for additional safety) as such:
    // ! TlsContext^.IgnoreCertificateErrors := false;
    property TlsContext: PNetTlsContext
      read GetTlsContext;
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
    // - you may rather call SearchBegin/SearchEnd or SearchAll wrapper functions
    // - if not [], append a LDAP_PAGED_RESULT_OID_STRING control to each Search()
    // - note: if you expect a single result row, settting 1 won't necessary
    // reduce the data stream, because it would include an additional block with
    // a SearchCookie, and is likely to use more server resource for paging
    property SearchPageSize: integer
      read fSearchPageSize write fSearchPageSize;
    /// cookie returned by paged search results, i.e. for SearchPageSize > 0
    // - use an empty string for the first search request
    // - if not empty, you should call Search() again for the next page until it
    // is eventually empty
    // - force an empty string to reset the pagination or for a new Search()
    // - you may rather call SearchBegin/SearchEnd or SearchAll wrapper functions
    property SearchCookie: RawUtf8
      read fSearchCookie write fSearchCookie;
    /// how many objects have been returned during last SearchBegin/SearchEnd
    property SearchPageCount: integer
      read fSearchPageCount;
    /// the optional security flags to include in the response
    // - default [] means no atNTSecurityDescriptor
    // - you can set e.g. lsfMain to retrieve main Security Descriptor fields
    // - if not [], append a LDAP_SERVER_SD_FLAGS_OID control to each Search(),
    // so that atNTSecurityDescriptor will contain the specified flags content
    property SearchSDFlags: TLdapSearchSDFlags
      read fSearchSDFlags write fSearchSDFlags;
    /// result of the search command
    property SearchResult: TLdapResultList
      read fSearchResult;
    /// each LDAP operation on server can return some referals URLs
    property Referals: TRawUtf8List
      read fReferals;
    /// raw TCP socket used by all LDAP operations
    property Sock: TCrtSocket
      read fSock;
    /// can add some logs to the LDAP client process
    property Log: TSynLogClass
      read fLog write fLog;
  published
    /// the authentication and connection settings of a this instance
    property Settings: TLdapClientSettings
      read fSettings;
    /// how the previous successful Bind*() method has been performed
    // - Bound follows the actual connection state, whereas this property won't
    property BoundAs: TLdapClientBound
      read fBoundAs;
    /// the version of LDAP protocol used
    // - default value is 3
    property Version: integer
      read fVersion write fVersion default 3;
    /// contains the result code of the last LDAP operation
    // - could be e.g. LDAP_RES_SUCCESS or an error code
    // - see also ResultString text and ResultError enumerate
    property ResultCode: integer
      read fResultCode;
    /// contains the high-level result enumerate of the last LDAP operation
    // - see also ResultString text, especially for leUnknown
    property ResultError: TLdapError
      read fResultError;
    /// human readable description of the last LDAP operation
    // - see also ResultCode raw integer and ResultError enumerate
    property ResultString: RawUtf8
      read fResultString;
    /// callback raised when a TLdapClient instance is disconnected at socket level
    // - if not defined, will follow Settings.AutoReconnect property
    property OnDisconnect: TOnLdapClientEvent
      read fOnDisconnect write fOnDisconnect;
  end;

const
  /// common possible value for TLdapClient.SearchSDFlags
  // - lsfSaclSecurityInformation is not included because it is likely to be not
  // available for the client, and would void the atNTSecurityDescriptor field
  lsfMain = [lsfOwnerSecurityInformation,
             lsfGroupSecurityInformation,
             lsfDaclSecurityInformation];

function ToText(mode: TLdapClientBound): PShortString; overload;
function ToText(lct: TLdapClientTransmission): PShortString; overload;


{ **************** Dedicated TLdapCheckMember Class }

type
  /// a LDAP client instance dedicated to validate user group membership
  // - properly checking group membership is a complex issue, worth its own class
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
    fLastConnectedTix32: cardinal;
    // naive but efficient O(n) cache of valid and invalid Users
    fCacheTimeoutSeconds: integer;
    fCacheOK, fCacheKO: TRawUtf8DynArray;
    fCacheOKGroupsAN: TRawUtf8DynArrayDynArray;
    fCacheOKCount, fCacheKOCount: integer;
    fCacheTimeoutTix: Int64;
    fGroupNested: boolean;
    fSearchFilter: TObjectFilter;
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
    // call GetIsMemberOf() to search for registered groups (following
    // GroupNested property to walk the chain of ancestries)
    // - optionally return the matching sAMAccountName in GroupsAN[]
    function Authorize(const User: RawUtf8;
      GroupsAN: PRawUtf8DynArray = nil): boolean;
    /// main TOnAuthServer callback to check if a user is allowed to login
    // - just redirect to the Authorize() method
    function BeforeAuth(Sender: TObject; const User: RawUtf8): boolean;
    /// remove any previously allowed groups
    procedure AllowGroupClear;
    /// register the sAMAccountName of additional allowed group(s)
    procedure AllowGroupAN(const GroupAN: TRawUtf8DynArray); overload;
    /// register the sAMAccountName of additional allowed group(s) as CSV
    procedure AllowGroupAN(const GroupANCsv: RawUtf8); overload;
    /// register the distinguishedName of additional allowed group(s)
    procedure AllowGroupDN(const GroupDN: TRawUtf8DynArray);
    /// register all allowed group(s) at once
    // - same as AllowGroupClear + AllowGroupAN() + AllowGroupDN()
    // - do nothing if the supplied groups are already the one used
    procedure AllowGroups(const GroupAN, GroupDN: TRawUtf8DynArray);
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
    // - true by default; as documented by Microsoft: "walks the chain of
    // ancestry in objects all the way to the root until it finds a match"
    property GroupNested: boolean
      read fGroupNested write fGroupNested;
    /// after how many seconds the internal cache should be flushed
    // - default valucache timeout is 300 seconds, i.e. 5 minutes
    // - you can disable the cache by setting 0 here, e.g. if this instance is
    // calling Authorize() just once
    property CacheTimeoutSeconds: integer
      read fCacheTimeoutSeconds write fCacheTimeoutSeconds;
    /// access to the AllowGroupAN() and AllowGroupDN() primaryGroupID attributes
    property GroupID: TIntegerDynArray
      read fGroupID;
    /// this class will search for ofUsers by default, but you may change it here
    property SearchFilter: TObjectFilter
      read fSearchFilter write fSearchFilter;
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

{ **************** CLDAP Client Functions }

const
  NTVER: RawUtf8 = '\06\00\00\00'; // RawLdapTranslateFilter() does UnescapeHex()

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
  RecordZero(@Info, TypeInfo(TCldapDomainInfo));
  result := false;
  if addr.SetFrom(LdapServerAddress, LdapServerPort, nlUdp) <> nrOk then
    exit;
  sock := addr.NewSocket(nlUdp);
  if sock <> nil then
  try
    id := Random31Not0;
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
    addr.IPWithPort(Info.IP);
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
        result := res[Random32(length(res))]; // return a matching site
        exit;
      end;
    end;
  end;
  if LdapServers <> nil then // if no site is defined, use one of the servers
    result := LdapServers[Random32(length(LdapServers))]
  else
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

function CldapGetDefaultLdapController(DistinguishedName, Spn: PRawUtf8): RawUtF8;
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
  if DistinguishedName <> nil then
    DistinguishedName^ := domain;
  if Spn <> nil then
    Join(['LDAP/', Split(result, ':'), '@', UpperCase(domain)], Spn^);
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
    id := Random31Not0;
    req := Asn(ASN1_SEQ, [
             Asn(id),
             //Asn(''), // the RFC 1798 requires user, but MS AD does not :(
             RawLdapSearch('', false, '*', [
               'dnsHostName',
               'defaultNamingContext',
               'ldapServiceName',
               'vendorName'])
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
          ['dnsHostName',
           'defaultNamingContext',
           'ldapServiceName',
           'vendorName'],
          [@v.HostName,
           @v.NamingContext,
           @v.ServiceName,
           @v.VendorName]) then
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


{ **************** LDAP Protocol Definitions }

function ParseDN(const DN: RawUtf8; out dc, ou, cn: TRawUtf8DynArray;
  ValueEscapeCN, NoRaise: boolean): boolean;
var
  p: PUtf8Char;
  kind, value: RawUtf8;
  dcn, oun, cnn: integer;
begin
  result := false;
  p := pointer(DN);
  if p = nil then
    exit;
  dcn := 0;
  oun := 0;
  cnn := 0;
  repeat
    GetNextItemTrimedEscaped(p, '=', '\', kind);
    GetNextItemTrimedEscaped(p, ',', '\', value);
    if (kind = '') or
       (value = '') then
      if NoRaise then
        exit
      else
        ELdap.RaiseUtf8('ParseDN(%): invalid Distinguished Name', [DN]);
    if not PropNameValid(pointer(value)) then // simple alphanum is always fine
    begin
      value := LdapUnescape(value); // may need some (un)escape
      if ValueEscapeCN then
        value := EscapeChar(value, LDAP_CN, '\'); // inlined LdapEscapeCN()
    end;
    case PCardinal(kind)^ and $ffdfdf of
      ord('D') + ord('C') shl 8:
        AddRawUtf8(dc, dcn, value);
      ord('O') + ord('U') shl 8:
        AddRawUtf8(ou, oun, value);
      ord('C') + ord('N') shl 8:
        AddRawUtf8(cn, cnn, value);
    else
      if NoRaise then
        exit
      else
        ELdap.RaiseUtf8('ParseDN(%): unexpected %= field', [DN, kind]);
    end;
  until p = nil;
  if dc <> nil then
    DynArrayFakeLength(dc, dcn);
  if ou <> nil then
    DynArrayFakeLength(ou, oun);
  if cn <> nil then
    DynArrayFakeLength(cn, cnn);
  result := true;
end;

function DNsToCN(const dc, ou, cn: TRawUtf8DynArray): RawUtf8;
var
  w: TTextWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
begin
  w := TTextWriter.CreateOwnedStream(tmp);
  try
    w.AddCsvStrings(dc, '.', -1, {reverse=}false);
    if (ou <> nil) or
       (cn <> nil) then
      w.AddDirect('/');
    if ou <> nil then
      w.AddCsvStrings(ou, '/', -1, {reverse=}true);
    if cn <> nil then
    begin
      if ou <> nil then
        w.AddDirect('/');
      w.AddCsvStrings(cn, '/', -1, {reverse=}true);
    end;
    w.SetText(result);
  finally
    w.Free;
  end;
end;

function DNToCN(const DN: RawUtf8; NoRaise: boolean): RawUtf8;
var
  dc, ou, cn: TRawUtf8DynArray;
begin
  result := '';
  if (DN <> '') and
     ParseDN(DN, dc, ou, cn, {valueEscapeCN=}true, NoRaise) then
    result := DNsToCN(dc, ou, cn);
end;

function NormalizeDN(const DN: RawUtf8): RawUtf8;
var
  dc, ou, cn: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := '';
  if (DN = '') or
     not ParseDN(DN, dc, ou, cn, {valueEscapeCN=}true, {noraise=}true) then
    exit;
  for i := 0 to length(cn) - 1 do
    Append(result, ',CN=', cn[i]);
  for i := 0 to length(ou) - 1 do
    Append(result, ',OU=', ou[i]);
  for i := 0 to length(dc) - 1 do
    Append(result, ',DC=', dc[i]);
  delete(result, 1, 1); // trim leading ','
end;

function ParseDN(const DN: RawUtf8; out pairs: TNameValueDNs; NoRaise: boolean): boolean;
var
  p: PUtf8Char;
  n, v: RawUtf8;
  c: integer;
begin
  result := false;
  p := pointer(DN);
  if p = nil then
    exit;
  c := 0;
  repeat
    GetNextItemTrimedEscaped(p, '=', '\', n);
    GetNextItemTrimedEscaped(p, ',', '\', v);
    if (n = '') or
       (v = '') then
      if NoRaise then
        exit
      else
        ELdap.RaiseUtf8('ParseDN(%): invalid Distinguished Name', [DN]);
    if PropNameValid(pointer(v)) then // simple alphanum is always fine
      v := LdapUnescape(v); // may need some (un)escape
    if c = length(pairs) then
      SetLength(pairs, NextGrow(c));
    with pairs[c] do
    begin
      Name := n;
      Value := v;
    end;
    inc(c);
  until p = nil;
  if c <> 0 then
    DynArrayFakeLength(pairs, c);
  result := true;
end;

function RawLdapError(ErrorCode: integer): TLdapError;
begin
  case ErrorCode of
    LDAP_RES_SUCCESS .. LDAP_RES_AUTHORIZATION_DENIED: // 0 .. 123
      result := TLdapError(ByteScanIndex(
        @LDAP_RES_CODE, length(LDAP_RES_CODE), ErrorCode) + 1);
    LDAP_RES_ESYNC_REFRESH_REQUIRED:                   // 4096
      result := leEsyncRefreshRequired;
    LDAP_RES_NO_OPERATION:                             // 16654
      result := leNoOperation;
  else
    result := leUnknown
  end;
end;

function RawLdapErrorString(ErrorCode: integer; out Enum: TLdapError): RawUtf8;
begin
  Enum := RawLdapError(ErrorCode);
  FormatUtf8('% (#%)', [LDAP_ERROR_TEXT[Enum], ErrorCode], result);
end;

function RawLdapErrorString(ErrorCode: integer): RawUtf8;
var
  dummy: TLdapError;
begin
  result := RawLdapErrorString(ErrorCode, dummy);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function RawLdapTranslateFilter(const Filter: RawUtf8; NoRaise: boolean): TAsnObject;
var
  text, attr, value, rule: RawUtf8;
  expr: TAsnObject;
  i, attrlen: PtrInt;
  ok, dn: boolean;

  procedure RaiseError;
  begin
    ELdap.RaiseUtf8('Invalid Expression: %', [Filter]);
  end;

  function GetNextRecursiveExpr: boolean;
  var
    p, i, len, parent: PtrInt;
  begin
    // extract the next (..(..(..)..)..) expression from text into expr
    result := false;
    parent := 1;
    len := length(text);
    p := PosExChar('(', text);
    if (p <> 0) and
       (len > 2) then
      for i := p + 1 to len do
        case text[i] of
          '(':
            inc(parent);
          ')':
            begin
              dec(parent);
              if parent <> 0 then
                continue;
              // return the whole expression and the trimmed text
              inc(p); // excluding parenthesis
              rule := copy(text, p, i - p);
              p := i;
              while (p <= len) and
                    (text[p] = ' ') do
                inc(p);
              delete(text, 1, p);
              expr := RawLdapTranslateFilter(rule, NoRaise); // recursive call
              result := expr <> '';
              break;
            end;
        end;
    if (not result) and
       (not NoRaise) then
      RaiseError;
  end;

  function TrimAttr: boolean;
  begin
    result := false;
    dec(attrlen);
    if attrlen = 0 then
      if NoRaise then
        exit
      else
        RaiseError;
    FakeLength(attr, attrlen);
    result := true;
  end;

  procedure ParseOperator(ctc: integer);
  begin
    if TrimAttr then
      result := Asn(ctc, [
        AsnOctStr(attr),
        AsnOctStr(UnescapeHex(value))]);
  end;

  procedure SubFetch(asn: integer);
  begin
    if i > 1 then
    begin
      TrimCopy(value, 1, i - 1, text);
      AsnAdd(expr, UnescapeHex(text), asn);
    end;
    delete(value, 1, i);
    i := PosExChar('*', value);
  end;

begin
  result := '';
  text := TrimU(Filter);
  if text = '' then
    exit;
  if text[1] = '(' then
  begin
    ok := false;
    for i := length(text) downto 2 do
      if text[i] = ')' then
      begin
        TrimCopy(text, 2, i - 2, text); // trim main parenthesis
        ok := text <> '';
        break;
      end;
    if not ok then
      if NoRaise then
        exit
      else
        RaiseError;
  end;
  if PWord(text)^ = ord('*') then
    // Present Filter Type
    result := AsnTyped('*', ASN1_CTX7)
  else
  case text[1] of
    '&':
      // AND Filter Type (recursive call)
      begin
        if Filter <> '(&)' then // RFC 4526 absolute true filter
          repeat
            if not GetNextRecursiveExpr then
              exit;
            AsnAdd(result, expr);
          until text = '';
        result := AsnTyped(result, ASN1_CTC0);
      end;
    '|':
      // OR Filter Type (recursive call)
      begin
        if Filter <> '(|)' then // RFC 4526 absolute false filter
          repeat
            if not GetNextRecursiveExpr then
              exit;
            AsnAdd(result, expr);
          until text = '';
        result := AsnTyped(result, ASN1_CTC1);
      end;
    '!':
      // NOT Filter Type (recursive call)
      if GetNextRecursiveExpr then
        result := AsnTyped(expr, ASN1_CTC2);
    else
      begin
        // extract the (attr=value) pair
        i := PosExChar('=', text);
        if i = 0 then
          if NoRaise then
            exit
          else
            RaiseError;
        TrimCopy(text, 1, i, attr);
        attrlen := length(attr);
        value := copy(text, i + 1, 2047); // no value trim
        if TrimAttr then
          case attr[attrlen] of
            '>':
              // (attr>=value) greaterOrEqual Filter Type
              ParseOperator(ASN1_CTC5);
            '<':
              // (attr<=value) lessOrEqual Filter Type
              ParseOperator(ASN1_CTC6);
            '~':
              // (attr~=value) approximateMatch Filter Type
              ParseOperator(ASN1_CTC8);
            ':':
              // (attr:=value) extensibleMatch Filter Type
              if TrimAttr then
              begin
                // e.g. '(uid:dn:caseIgnoreMatch:=jdoe)'
                dn := false;
                repeat
                  i := mormot.core.base.PosEx(':dn', attr);
                  if i = 0 then
                    break;
                  dn := true;
                  delete(attr, i, 3);
                until false;
                if TrimSplit(attr, attr, rule, ':') then
                  if rule <> '' then
                    expr := AsnTyped(rule, ASN1_CTX1);
                if attr <> '' then
                  AsnAdd(expr, attr, ASN1_CTX2);
                AsnAdd(expr, UnescapeHex(value), ASN1_CTX3);
                if dn then // dnAttributes flag - default is FALSE
                  AsnAdd(expr, RawByteString(#$01#$ff), ASN1_CTX4);
                result := AsnTyped(expr, ASN1_CTC9);
              end;
          else
            if value = '*' then
              // (attr=*) present Filter Type
              result := AsnTyped(attr, ASN1_CTX7)
            else
            begin
              i := PosExChar('*', value);
              if i = 0 then
                // (attr=value) equalityMatch Filter Type
                result := Asn(ASN1_CTC3, [
                   AsnOctStr(attr),
                   AsnOctStr(UnescapeHex(value))])
              else
              begin
                // (attr=value*value*) substrings Filter Type
                if i > 1 then
                  SubFetch(ASN1_CTX0);
                while i <> 0 do
                  SubFetch(ASN1_CTX1);
                if value <> '' then
                  AsnAdd(expr, UnescapeHex(value), ASN1_CTX2);
                result := Asn(ASN1_CTC4, [
                   AsnOctStr(attr),
                   AsnSeq(expr)]);
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
  encodedfilter: RawUtf8;
begin
  if Filter = '' then
    Filter := OBJECT_FILTER[ofAll];
  encodedfilter := RawLdapTranslateFilter(Filter);
  if encodedfilter = '' then
    encodedfilter := AsnTyped('', ASN1_NULL);
  result := Asn(LDAP_ASN1_SEARCH_REQUEST, [
              AsnOctStr(BaseDN),
              AsnEnum(ord(Scope)),
              AsnEnum(ord(Aliases)),
              Asn(Sizelimit),
              Asn(TimeLimit),
              ASN1_BOOLEAN_VALUE[TypesOnly],
              encodedfilter,
              AsnSeq(AsnArr(Attributes))
            ]);
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
      i := {%H-}setend; // if several ASN1_OCTSTR are stored - return only first
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
            IsValidUtf8Small(Text); // AVX2 not worth it
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
            IsValidUtf8Small(Text); // AVX2 not worth it
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

function LdapUnicodePwd(const aPassword: SpiUtf8): RawByteString;
var
  u8: SpiUtf8;
begin
  try
    u8 := Join(['"', aPassword, '"']);
    result := Utf8DecodeToUnicodeRawByteString(u8);
  finally
    FillZero(u8);
  end;
end;

function LdapToDate(const Text: RawUtf8): TDateTime;
begin
  if Text = 'Never expires' then
    result := 0
  else
    result := Iso8601ToDateTime(Text);
end;


{ **************** LDAP Attributes Definitions }

// private copy from constant to global variables because of Delphi which makes
// a new RefCnt > 0 copy when assigning a RefCnt = -1 constant to a variable :(
const
  // reference names to fill the global AttrTypeName[]
  _AttrTypeName: array[TLdapAttributeType] of RawUtf8 = (
    '',                            // atUndefined
    'distinguishedName',           // atDistinguishedName
    'objectClass',                 // atObjectClass
    'objectCategory',              // otObjectCategory
    'alias',                       // atAlias
    'name',                        // atName
    'cn',                          // atCommonName
    'sn',                          // atSurName
    'givenName',                   // atGivenName
    'displayName',                 // atDisplayName
    'userPrincipalName',           // atUserPrincipalName
    'userAccountControl',          // atUserAccountControl
    'systemFlags',                 // atSystemFlags
    'sAMAccountName',              // atSAMAccountName
    'sAMAccountType',              // atSAMAccountType
    'adminCount',                  // atAdminCount
    'description',                 // atDescription
    'generationQualifier',         // atGenerationQualifier
    'initials',                    // atInitials
    'o',                           // atOrganizationName
    'ou',                          // atOrganizationUnitName
    'mail',                        // atMail
    'memberOf',                    // atMemberOf
    'c',                           // atCountryName
    'l',                           // atLocalityName
    'st',                          // atStateName
    'street',                      // atStreetAddress
    'telephoneNumber',             // atTelephoneNumber
    'title',                       // atTitle
    'serialNumber',                // atSerialNumber
    'member',                      // atMember
    'owner',                       // atOwner
    'groupType',                   // atGroupType
    'primaryGroupID',              // atPrimaryGroupID
    'nTSecurityDescriptor',        // atNTSecurityDescriptor
    'objectSid',                   // atObjectSid
    'objectGUID',                  // atObjectGuid
    'logonCount',                  // atLogonCount
    'badPwdCount',                 // atBadPwdCount
    'dNSHostName',                 // atDnsHostName
    'accountExpires',              // atAccountExpires
    'badPasswordTime',             // atBadPasswordTime
    'lastLogon',                   // atLastLogon
    'lastLogonTimestamp',          // atLastLogonTimestamp
    'lastLogoff',                  // atLastLogoff
    'lockoutTime',                 // atLockoutTime
    'pwdLastSet',                  // atPwdLastSet
    'ms-MCS-AdmPwdExpirationTime', // atMcsAdmPwdExpirationTime
    'whenCreated',                 // atWhenCreated
    'whenChanged',                 // atWhenChanged
    'operatingSystem',             // atOperatingSystem
    'operatingSystemVersion',      // atOperatingSystemVersion
    'servicePrincipalName',        // atServicePrincipalName
    'unicodePwd',                  // atUnicodePwd
    'accountNameHistory',          // atAccountNameHistory
    'tokenGroups');                // atTokenGroups

  // reference names to fill the global AttrTypeNameAlt[]
  _AttrTypeNameAlt: array[0 .. high(AttrTypeNameAlt)] of RawUtf8 = (
    'commonName',                  // cn
    'surname',                     // sn
    'countryName',                 // c
    'localityName',                // l
    'stateOrProvinceName',         // st
    'streetAddress',               // street
    'organizationName',            // o
    'organizationalUnitName');     // ou

var
  // we intern "normalized" case-insensitive attribute names
  _LdapIntern: TRawUtf8InterningSlot;
  // allow fast linear search in L1 CPU cache of interned attribute names
  // - 32-bit is enough to identify pointers, and leverage O(n) SSE2 asm
  _LdapIntern32: array[0 .. length(_AttrTypeName) + length(_AttrTypeNameAlt) - 2] of cardinal;
  _LdapInternType: array[0 .. high(_LdapIntern32)] of TLdapAttributeType;
  sObjectName, sCanonicalName: RawUtf8;

procedure InitializeUnit;
var
  t: TLdapAttributeType;
  i, n, failed: PtrInt;
begin
  GetEnumTrimmedNames(TypeInfo(TLdapError), @LDAP_ERROR_TEXT, false, false,
    {lowcasefirst=}true);
  LDAP_ERROR_TEXT[leEsyncRefreshRequired] := 'e-syncRefreshRequired';
  // register all our common Attribute Types names for quick search as pointer()
  _LdapIntern.Init({CaseInsensitive=}true, {Capacity=}128);
  failed := -1;
  n := 0;
  for t := succ(low(t)) to high(t) do
    if _LdapIntern.Unique(AttrTypeName[t], _AttrTypeName[t]) then
    begin
      {$ifdef CPU64} // identify very unlikely low 32-bit pointer collision
      if failed < 0 then
        failed := IntegerScanIndex(@_LdapIntern32, n, PtrUInt(AttrTypeName[t]));
      {$endif CPU64}
      _LdapIntern32[n] := PtrUInt(AttrTypeName[t]); // truncated to 32-bit
      _LdapInternType[n] := t;
      inc(n);
    end
    else
      ELdap.RaiseUtf8('dup %', [_AttrTypeName[t]]); // paranoid
  for i := 0 to high(_AttrTypeNameAlt) do
    if _LdapIntern.Unique(AttrTypeNameAlt[i], _AttrTypeNameAlt[i]) then
    begin
      {$ifdef CPU64}
      if failed < 0 then
        failed := IntegerScanIndex(@_LdapIntern32, n, PtrUInt(AttrTypeNameAlt[i]));
      {$endif CPU64}
      _LdapIntern32[n] := PtrUInt(AttrTypeNameAlt[i]);
      _LdapInternType[n] := AttrTypeAltType[i];
      inc(n);
    end
    else
      ELdap.RaiseUtf8('dup alt %', [_AttrTypeNameAlt[i]]);
  if failed >= 0 then
    ELdap.RaiseUtf8('32-bit pointer collision of %', [_LdapIntern32[failed]]);
  _LdapIntern.Unique(sObjectName, 'objectName');
  _LdapIntern.Unique(sCanonicalName, 'canonicalName');
end;

// internal function: O(n) search of AttrName 32-bit-truncated interned pointer
function _AttributeNameType(AttrName: pointer): TLdapAttributeType;
var
  i: PtrInt;
begin
  result := atUndefined;
  if AttrName = nil then
    exit;
  i := IntegerScanIndex(@_LdapIntern32, length(_LdapIntern32), PtrUInt(AttrName));
  if i >= 0 then
    result := _LdapInternType[i];
end;

function AttributeNameType(const AttrName: RawUtf8): TLdapAttributeType;
begin
  if AttrName = '' then
    result := atUndefined
  else
    result := _AttributeNameType(_LdapIntern.Existing(AttrName)); // very fast
end;

procedure AttributeNameNormalize(var AttrName: RawUtf8);
var
  existing: pointer;
begin
  if AttrName = '' then
    exit;
  existing := _LdapIntern.Existing(AttrName);
  if (existing <> nil) and
     (existing <> pointer(AttrName)) then
    AttrName := RawUtf8(existing); // replace with existing interned name
end;

function AttributeValueMakeReadable(var s: RawUtf8; ats: TLdapAttributeTypeStorage;
  dom: PSid; uuid: TAppendShortUuid): boolean;
var
  ft: QWord;
  guid: TGuid;
  err: integer absolute guid;
  ts: TTimeLogBits absolute guid;
begin
  // handle the storage kind of our recognized attribute types
  result := false;
  case ats of
    atsRawUtf8, // most used - LDAP v3 requires UTF-8 encoding
    atsInteger,
    atsIntegerUserAccountControl,
    atsIntegerSystemFlags,
    atsIntegerGroupType,
    atsIntegerAccountType:
      exit; // no need to make any conversion for ATS_READABLE content
    atsSid:
      if IsValidRawSid(s) then // stored as binary SID
      begin
        SidToText(pointer(s), s);
        exit;
      end;
    atsGuid:
      if length(s) = SizeOf(TGuid) then // stored as binary GUID
      begin
        guid := PGuid(s)^; // temp copy since ToUtf8() overrides s itself
        ToUtf8(guid, s);  // e.g. '3F2504E0-4F89-11D3-9A0C-0305E82C3301'
        exit;
      end;
    atsSecurityDescriptor:
      if SecurityDescriptorToText(s, s, dom, uuid) then // into SDDL text
        exit;
    atsFileTime: // 64-bit FileTime
      begin
        ft := GetQWord(pointer(s), err);
        if (err = 0) and
           (ft <> 0) then
        begin
          if ft >= $7FFFFFFFFFFFFFFF then
            s := 'Never expires'
          else
          begin
            ts.FromUnixMSTime(WindowsFileTime64ToUnixMSTime(ft));
            ts.SetText(s, {expanded=}true); // normalize as pure ISO-8601
          end;
          exit;
        end;
      end;
    atsTextTime: // some date/time text
      begin
        ts.From(pointer(s), length(s) - 3);
        if ts.Value <> 0 then
        begin
          ts.SetText(s, {expanded=}true); // normalize as pure ISO-8601
          exit;
        end;
      end;
    atsUnicodePwd:
      begin
        s := 'xxxxxxxx'; // anti-forensic measure
        result := true;
        exit;
      end;
  end;
  // atsAny or not expected format: check valid UTF-8, or fallback to hexa
  if IsValidUtf8Small(s) then // AVX2 not worth it
    EnsureRawUtf8(s)
  else
  begin
    BinToHexLowerSelf(RawByteString(s));
    result := true;
  end;
end;

function ToText(Attributes: TLdapAttributeTypes): TRawUtf8DynArray;
var
  n: PtrInt;
  t: TLdapAttributeType;
  r: PRawUtf8;
begin
  result := nil;
  exclude(Attributes, atUndefined);
  n := GetBitsCount(Attributes, {bits=}SizeOf(Attributes) shl 3);
  if n = 0 then
    exit;
  SetLength(result, n);
  r := pointer(result);
  for t := succ(atUndefined) to high(t) do
    if t in Attributes then
    begin
      r^ := AttrTypeName[t];
      dec(n);
      if n = 0 then
        break;
      inc(r);
    end;
end;

function ToText(Attribute: TLdapAttributeType): RawUtf8;
begin
  result := AttrTypeName[Attribute];
end;

const
  // see https://ldapwiki.com/wiki/Wiki.jsp?page=SAMAccountType
  AT_VALUE: array[TSamAccountType] of cardinal = (
    $00000000,  // satUnknown
    $10000000,  // satGroup            = 268435456
    $10000001,  // satNonSecurityGroup = 268435457
    $20000000,  // satAlias
    $20000001,  // satNonSecurityAlias
    $30000000,  // satUserAccount    = 805306368
    $30000001,  // satMachineAccount = 805306369
    $30000002,  // satTrustAccount
    $40000000,  // satAppBasicGroup
    $40000001); // satAppQueryGroup

  // see https://ldapwiki.com/wiki/Wiki.jsp?page=GroupType
  GT_VALUE: array[TGroupType] of integer = (
    1,                   // gtBuiltIn
    2,                   // gtGlobal
    4,                   // gtDomainLocal
    8,                   // gtUniversal
    16,                  // gtAppBasic
    32,                  // gtAppQuery
    integer($80000000)); // gtSecurity

  // see https://ldapwiki.com/wiki/Wiki.jsp?page=User-Account-Control%20Attribute%20Values
  UAC_VALUE: array[TUserAccountControl] of integer = (
    1,                   // uacScript
    2,                   // uacAccountDisable
    8,                   // uacHomeDirRequired
    16,                  // uacLockedOut
    32,                  // uacPasswordNotRequired
    64,                  // uacPasswordCannotChange
    128,                 // uacPasswordUnencrypted
    256,                 // uacTempDuplicateAccount
    512,                 // uacNormalAccount
    2048,                // uacInterDomainTrusted
    4096,                // uacWorkstationTrusted
    8192,                // uacServerTrusted
    65536,               // uacPasswordDoNotExpire
    131072,              // uacLogonAccount
    262144,              // uacSmartcardRequired
    524288,              // uacKerberosTrustedForDelegation
    1048576,             // uacKerberosNotDelegated
    2097152,             // uacKerberosDesOnly
    4194304,             // uacKerberosRequirePreAuth
    8388608,             // uacPasswordExpired
    16777216,            // uacKerberosTrustedToDelegate
    33554432,            // uacKerberosNoPac
    67108864,            // uacPartialSecretsRodc
    integer($80000000)); // uacUserUseAesKeys

  // see https://ldapwiki.com/wiki/Wiki.jsp?page=X-SYSTEMFLAGS
  SF_VALUE: array[TSystemFlag] of integer = (
    1,                   // sfAttrNotReplicated
    2,                   // sfAttrReqPartialSetMember
    4,                   // sfAttrIsConstructed
    8,                   // sfAttrIsOperational
    16,                  // sfSchemaBaseObject
    32,                  // sfAttrIsRdn
    $04000000,           // sfDomainDisallowMove
    $08000000,           // sfDomainDisallowRename
    $10000000,           // sfConfigAllowLimitedMove
    $20000000,           // sfConfigAllowMove
    $40000000,           // sfConfigAllowRename
    integer($80000000)); // sfConfigDisallowDelete

function SamAccountTypeFromInteger(value: cardinal): TSamAccountType;
begin
  result := TSamAccountType(IntegerScanIndex(
    @AT_VALUE[succ(low(result))], length(AT_VALUE) - 1, value) + 1)
end;

function SamAccountTypeFromText(const value: RawUtf8): TSamAccountType;
var
  c: cardinal;
begin
  if ToCardinal(value, c) then
    result := SamAccountTypeFromInteger(c)
  else
    result := satUnknown;
end;

function SamAccountTypeValue(sat: TSamAccountType): integer;
begin
  result := AT_VALUE[sat];
end;

function GroupTypesFromInteger(value: integer): TGroupTypes;
var
  g: TGroupType;
  f: integer;
begin
  result := [];
  if value <> 0 then
    for g := low(g) to high(g) do
    begin
      f := GT_VALUE[g];
      if value and f = 0 then
        continue;
      include(result, g);
      dec(value, f);
      if value = 0 then
        break;
    end;
end;

function GroupTypesFromText(const value: RawUtf8): TGroupTypes;
var
  v: integer;
begin
  result := [];
  if ToInteger(value, v) then
    result := GroupTypesFromInteger(v);
end;

function GroupTypesValue(gt: TGroupTypes): integer;
var
  g: TGroupType;
begin
  result := 0;
  if gt <> [] then
    for g := low(g) to high(g) do
      if g in gt then
        result := result or GT_VALUE[g];
end;

function UserAccountControlsFromInteger(value: integer): TUserAccountControls;
var
  uac: TUserAccountControl;
  f: integer;
begin
  result := [];
  if value <> 0 then
    for uac := low(uac) to high(uac) do
    begin
      f := UAC_VALUE[uac];
      if value and f = 0 then
        continue;
      include(result, uac);
      dec(value, f);
      if value = 0 then
        break;
    end;
end;

function UserAccountControlsFromText(const value: RawUtf8): TUserAccountControls;
var
  v: integer;
begin
  result := [];
  if ToInteger(value, v) then
    result := UserAccountControlsFromInteger(v);
end;

function UserAccountControlsValue(uac: TUserAccountControls): integer;
var
  u: TUserAccountControl;
begin
  result := 0;
  if uac <> [] then
    for u := low(u) to high(u) do
      if u in uac then
        result := result or UAC_VALUE[u];
end;

function SystemFlagsFromInteger(value: integer): TSystemFlags;
var
  sf: TSystemFlag;
  f: integer;
begin
  result := [];
  if value <> 0 then
    for sf := low(sf) to high(sf) do
    begin
      f := SF_VALUE[sf];
      if value and f = 0 then
        continue;
      include(result, sf);
      dec(value, f);
      if value = 0 then
        break;
    end;
end;

function SystemFlagsFromText(const value: RawUtf8): TSystemFlags;
var
  v: integer;
begin
  result := [];
  if ToInteger(value, v) then
    result := SystemFlagsFromInteger(v);
end;

function SystemFlagsValue(sf: TSystemFlags): integer;
var
  f: TSystemFlag;
begin
  result := 0;
  if sf <> [] then
    for f := low(f) to high(f) do
      if f in sf then
        result := result or SF_VALUE[f];
end;

function ToText(sat: TSamAccountType): PShortString;
begin
  result := GetEnumName(TypeInfo(TSamAccountType), ord(sat));
end;

procedure ToTextTrimmed(sat: TSamAccountType; var text: RawUtf8);
begin
  TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TSamAccountType), ord(sat)), text);
end;

function ToText(oft: TObjectFilter): PShortString;
begin
  result := GetEnumName(TypeInfo(TObjectFilter), ord(oft));
end;

procedure ToTextTrimmed(oft: TObjectFilter; var text: RawUtf8);
begin
  TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TObjectFilter), ord(oft)), text);
end;

function ObjectFilter(Filter: TObjectFilter; const AccountName,
  DistinguishedName, UserPrincipalName, CustomFilter: RawUtf8): RawUtf8;
begin
  // put AccountName/DistinguishedName/UserPrincipalName into result
  result := '';
  if AccountName <> '' then
    FormatUtf8('(sAMAccountName=%%)',
      [LdapEscapeName(AccountName),
       MACHINE_CHAR[(Filter in COMPUTER_FILTER) and
                    (AccountName[length(AccountName)] <> '$')]], result);
  if DistinguishedName <> '' then
    result := FormatUtf8('%(distinguishedName=%)',
      [result, LdapValidDistinguishedName(DistinguishedName)]); // no escape
  if UserPrincipalName <> '' then
    result := FormatUtf8('%(userPrincipalName=%)',
      [result, LdapEscapeName(UserPrincipalName)]);
  if ord(AccountName <> '') +
     ord(DistinguishedName <> '') +
     ord(UserPrincipalName <> '') > 1 then
    result := FormatUtf8('(|%)', [result]); // "or" between identifiers
  // compute global filter text
  if (Filter <> ofNone) or
     (CustomFilter <> '') then
    result := FormatUtf8('(&%%%)', [OBJECT_FILTER[Filter], result, CustomFilter]);
end;

procedure FlagsFilterInteger(const FlagsName: RawUtf8; Flags, noFlags: integer;
  out Filter: RawUtf8);
begin
  noFlags := noFlags and (not Flags); // Flags has precedence over un-Flags
  if Flags <> 0 then
    FormatUtf8('(%%=%)', [FlagsName, AND_FLAG, Flags], Filter);
  if noFlags <> 0 then
    Filter := FormatUtf8('(!(%%=%))%', [FlagsName, AND_FLAG, noFlags, Filter]);
end;

function UacFilter(Uac, unUac: TUserAccountControls): RawUtf8;
begin
  FlagsFilterInteger('userAccountControl',
    UserAccountControlsValue(Uac), UserAccountControlsValue(unUac), result);
end;

function GtFilter(Gt, unGt: TGroupTypes): RawUtf8;
begin
  FlagsFilterInteger('groupType',
    GroupTypesValue(Gt), GroupTypesValue(unGt), result);
end;


function Modifier(Op: TLdapModifyOp; const Sequence: TAsnObject): TAsnObject;
begin
  result := Asn(ASN1_SEQ, [
              AsnEnum(ord(Op)), // modification type as enum
              Sequence]);       // attribute(s) sequence
end;

function Modifier(Op: TLdapModifyOp; AttrType: TLdapAttributeType;
  const AttrValue: RawByteString): TAsnObject;
begin
  result := Modifier(Op, AttrTypeName[AttrType], AttrValue);
end;

function Modifier(Op: TLdapModifyOp; const AttrName: RawUtf8;
  const AttrValue: RawByteString): TAsnObject;
begin
  result := Modifier(Op,
              Asn(ASN1_SEQ, [                   // attribute sequence
                AsnOctStr(AttrName),            // attribute description
                AsnSetOf(AsnOctStr(AttrValue))  // attribute value set
              ]));
end;

function Modifier(Op: TLdapModifyOp; const Types: array of TLdapAttributeType;
  const Values: array of const): TAsnObject;
var
  i, n: PtrInt;
  v: RawUtf8;
begin
  result := '';
  n := high(Types);
  if (n < 0) or
     (n <> length(Values)) then
    exit;
  for i := 0 to n - 1 do // see TLdapAttribute.ToAsnSeq
    if Types[i] <> atUndefined then
    begin
      VarRecToUtf8(@Values[i], v); // Values[] are typically RawUtf8 or integer
      Append(result,
        AsnOctStr(AttrTypeName[Types[i]]), // attribute description
        AsnSetOf(AsnOctStr(v)));           // attribute value set
    end;
  if result <> '' then
    result := Modifier(Op, AsnSeq(result));
end;

function Modifier(Op: TLdapModifyOp;
  const NameValuePairs: array of RawUtf8): TAsnObject;
var
  i, n: PtrInt;
begin
  result := '';
  n := length(NameValuePairs);
  if (n = 0) or
     (n and 1 <> 0) then
    exit;
  for i := 0 to (n shr 1) - 1 do
    Append(result,
      AsnOctStr(NameValuePairs[i * 2]),                // attribute description
      AsnSetOf(AsnOctStr(NameValuePairs[i * 2 + 1]))); // attribute value set
  result := Modifier(Op, AsnSeq(result));
end;


{ TLdapAttribute }

constructor TLdapAttribute.Create(
  const AttrName: RawUtf8; AttrType: TLdapAttributeType);
begin
  inherited Create;
  fAttributeName := AttrName;
  SetKnownType(AttrType);
  SetLength(fList, 1); // optimized for a single value (most used case)
end;

destructor TLdapAttribute.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLdapAttribute.SetKnownType(AttrType: TLdapAttributeType);
begin
  fKnownType := AttrType;
  fKnownTypeStorage := AttrTypeStorage[AttrType];
end;

procedure TLdapAttribute.Clear;
var
  i: PtrInt;
begin
  if fCount = 0 then
    exit;
  if fKnownType = atUnicodePwd then
    for i := 0 to fCount - 1 do
      FillZero(fList[i]); // anti-forensic measure
  StringClearSeveral(pointer(fList), fCount); // remove values, but keep capacity
  fCount := 0;
end;

procedure TLdapAttribute.AssignTo(Dest: TClonable);
var
  d: TLdapAttribute absolute Dest;
begin
  d.fAttributeName := fAttributeName;
  d.fCount := fCount;
  d.fKnownType := fKnownType;
  d.fKnownTypeStorage := fKnownTypeStorage;
  d.fObjectSidIsDomain := fObjectSidIsDomain;
  d.fList := copy(fList, 0, fCount);
end;

procedure TLdapAttribute.Add(const aValue: RawByteString; Option: TLdapAddOption);
begin
  // handle Add() options
  case Option of
    aoReplaceValue:
      if (fCount = 1) and
         (fList[0] = aValue) then
        exit // nothing to replace
      else if fCount <> 0 then
        Clear; // replace existing by adding aValue as single item
    aoKeepExisting:
      if fCount <> 0 then
        exit;
    aoNoDuplicateValue:
      if FindIndex(aValue) >= 0 then
        exit;
  end;
  // some type-specific process
  case fKnownType of
    atObjectSid:
      fObjectSidIsDomain := SidIsDomain(pointer(aValue));
  end;
  // append to the internal list, with optional resize
  AddRawUtf8(TRawUtf8DynArray(fList), fCount, aValue);
  if Option <> aoAlwaysFast then
    DynArrayFakeLength(fList, fCount);
end;

procedure TLdapAttribute.AddFmt(const aValueFmt: RawUtf8;
  const aValueArgs: array of const; Option: TLdapAddOption);
begin
  Add(FormatUtf8(aValueFmt, aValueArgs), Option);
end;

procedure TLdapAttribute.AfterAdd;
begin
  if (fList <> nil) and
     (fCount <> 0) then
    DynArrayFakeLength(fList, fCount);
end;

procedure TLdapAttribute.AddFrom(Another: TLdapAttribute);
var
  i, n: PtrInt;
begin
  if (self = nil) or
     (Another = nil) or
     (Another.Count = 0) then
    exit;
  n := fCount;
  inc(fCount, Another.Count);
  SetLength(fList, fCount); // like AfterAdd
  for i := 0 to Another.Count - 1 do
    fList[n + i] := Another.fList[i];
end;

function TLdapAttribute.GetReadable(index: PtrInt): RawUtf8;
begin
  GetReadable(index, result);
end;

procedure TLdapAttribute.GetReadable(index: PtrInt; var Value: RawUtf8);
begin
  if (self = nil) or
     (index >= fCount) then
  begin
    FastAssignNew(Value);
    exit;
  end;
  Value := fList[index];
  if not (fKnownTypeStorage in ATS_READABLE) then
    AttributeValueMakeReadable(Value, fKnownTypeStorage);
end;

function TLdapAttribute.GetAllReadable: TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) then
    exit;
  if fKnownTypeStorage in ATS_READABLE then
  begin
    // no need to make any conversion, nor any allocation
    DynArrayFakeLength(fList, fCount);
    result := TRawUtf8DynArray(fList);
    exit;
  end;
  SetLength(result, fCount);
  for i := 0 to fCount - 1 do
  begin
    result[i] := fList[i];
    AttributeValueMakeReadable(result[i], fKnownTypeStorage);
  end;
end;

function TLdapAttribute.GetRaw(index: PtrInt): RawByteString;
begin
  if (self = nil) or
     (PtrUInt(index) >= PtrUInt(fCount)) then
    result := ''
  else
    result := fList[index];
end;

procedure TLdapAttribute.SetVariantOne(var v: TVarData; const s: RawUtf8;
  options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
var
  i: integer;
  uac: TUserAccountControls;
  gt: TGroupTypes;
  sat: TSamAccountType;
  sf: TSystemFlags;
begin
  if not (roRawValues in options) then
    case fKnownTypeStorage of
      atsAny,
      atsInteger:
        if ToInt64(s, v.VInt64) then
        begin
          v.VType := varInt64;
          exit;
        end
        else
        begin
          v.VInt64 := 0; // avoid GPF below
          if (fKnownTypeStorage = atsAny) and
             not (roRawBoolean in options) then
            if s = 'FALSE' then
            begin
              v.VType := varBoolean;
              exit;
            end
            else if s = 'TRUE' then
            begin
              v.VType := varBoolean;
              v.VInteger := ord(true);
              exit;
            end;
        end;
      atsIntegerUserAccountControl:
        if ToInteger(s, i) then
          if roRawUac in options then
          begin
            v.VType := varInteger;
            v.VInteger := i;
            exit;
          end
          else
          begin
            uac := UserAccountControlsFromInteger(i);
            TDocVariantData(v).InitArrayFromSet(
              TypeInfo(TUserAccountControls), uac, JSON_FAST, {trimmed=}true);
            exit;
          end;
      atsIntegerSystemFlags:
        if ToInteger(s, i) then
          if roRawFlags in options then
          begin
            v.VType := varInteger;
            v.VInteger := i;
            exit;
          end
          else
          begin
            sf := SystemFlagsFromInteger(i);
            TDocVariantData(v).InitArrayFromSet(
              TypeInfo(TSystemFlags), sf, JSON_FAST, {trimmed=}true);
            exit;
          end;
      atsIntegerGroupType:
        if ToInteger(s, i) then
          if roRawGroupType in options then
          begin
            v.VType := varInteger;
            v.VInteger := i;
            exit;
          end
          else
          begin
            gt := GroupTypesFromInteger(i);
            TDocVariantData(v).InitArrayFromSet(
              TypeInfo(TGroupTypes), gt, JSON_FAST, {trimmed=}true);
            exit;
          end;
      atsIntegerAccountType:
        if ToInteger(s, i) then
        begin
          if roRawAccountType in options then
            sat := satUnknown
          else
            sat := SamAccountTypeFromInteger(i);
          if sat <> satUnknown then
          begin
            v.VType := varString;
            ToTextTrimmed(sat, RawUtf8(v.VAny));
          end
          else
          begin
            v.VType := varInteger; // store satUnknown as integer
            v.VInteger := i;
          end;
          exit;
        end;
      atsFileTime:
        if s = '0' then
          exit; // 0 = null
    end;
  v.VType := varString;
  RawUtf8(v.VAny) := s;
  if not (fKnownTypeStorage in ATS_READABLE) and
     not (roRawValues in options) then
    AttributeValueMakeReadable(RawUtf8(v.VAny), fKnownTypeStorage, dom, uuid);
end;

function KnownUuid(Options: TLdapResultOptions): TAppendShortUuid;
  {$ifdef HASINLINE} inline; {$endif}
begin
  if roSddlKnownUuid in Options then
    result := @AppendShortKnownUuid // recognize TAdsKnownAttribute
  else
    result := @AppendShortUuid;
end;

procedure TLdapAttribute.SetVariantArray(var v: TDocVariantData;
  options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
var
  i: PtrInt;
begin // avoid implit try..finally in TLdapAttribute.GetVariant
  v.InitFast(fCount, dvArray);
  v.SetCount(fCount);
  if @uuid = nil then
    uuid := KnownUuid(options); // if not pre-resolved
  for i := 0 to fCount - 1 do
    SetVariantOne(PVarData(@v.Values[i])^, fList[i], options, dom, uuid);
end;

procedure TLdapAttribute.SetNewVariant(var v: variant;
  options: TLdapResultOptions; dom: PSid; uuid: TAppendShortUuid);
begin
  if (fCount <> 1) or
     (roAllValuesAsArray in options) or
     ((roKnownValuesAsArray in options) and
      not (fKnownType in ATS_SINGLEVALUE)) then
    if fKnownTypeStorage = atsRawUtf8 then
      TDocVariantData(v).InitArrayFrom(TRawUtf8DynArray(fList), JSON_FAST, fCount)
    else
      SetVariantArray(TDocVariantData(v), options, dom, uuid)
  else
    SetVariantOne(TVarData(v), fList[0], options, dom, uuid);
end;

function TLdapAttribute.GetVariant(options: TLdapResultOptions;
  dom: PSid; uuid: TAppendShortUuid): variant;
begin
  SetVariantNull(result);
  TVarData(result).VAny := nil; // as required by SetNewVariant()
  if (self <> nil) and
     (fCount > 0) then
    SetNewVariant(result, options, dom, uuid);
end;

function TLdapAttribute.ExportToAsnSeq: TAsnObject;
var
  i: PtrInt;
begin
  result := '';
  if (self = nil) or
     (fCount = 0) then
    exit;
  for i := 0 to fCount - 1 do
    AsnAdd(result, AsnOctStr(fList[i]));
  result := Asn(ASN1_SEQ, [              // attribute(s) sequence
              AsnOctStr(fAttributeName), // attribute description
              AsnSetOf(result)           // attribute value set
            ]);
end;

function TLdapAttribute.FindIndex(const aValue: RawByteString): PtrInt;
begin
  if (self <> nil) and
     (fCount > 0) then
    result := FindRawUtf8(pointer(fList), aValue, fCount, {casesens=}true)
  else
    result := -1;
end;

procedure TLdapAttribute.ExportToLdif(w: TTextWriter;
  aHumanFriendly: boolean; aMaxLineLen: PtrInt);
var
  i: PtrInt;
begin
  for i := 0 to fCount - 1 do
  begin
    w.AddString(fAttributeName); // is either OID or plain alphanum
    w.AddDirect(':');
    AddLdif(w, fList[i], {forcebase64:} fKnownTypeStorage in ATS_BINARY,
      aHumanFriendly, self, aMaxLineLen);
    w.AddDirect(#10);
  end;
end;


{ TLdapAttributeList }

constructor TLdapAttributeList.Create(
  const Types: array of TLdapAttributeType; const Values: array of const);
begin
  inherited Create;
  Add(Types, Values);
end;

destructor TLdapAttributeList.Destroy;
begin
  ObjArrayClear(fItems, fCount);
  inherited Destroy;
end;

procedure TLdapAttributeList.Clear;
begin
  ObjArrayClear(fItems, fCount);
  fCount := 0;
  fKnownTypes := [];
  FillCharFast(fIndexTypes, SizeOf(fIndexTypes), 0); // store index+1
end;

procedure TLdapAttributeList.AssignTo(Dest: TClonable);
var
  d: TLdapAttributeList absolute Dest;
begin
  d.fKnownTypes := fKnownTypes;
  d.fIndexTypes := fIndexTypes;
  TLdapAttribute.CloneObjArray(fItems, d.fItems, @fCount, @d.fCount);
end;

function TLdapAttributeList.FindIndex(const AttributeName: RawUtf8;
  IgnoreRange: boolean): PtrInt;
var
  existing: pointer;
  a: ^TLdapAttribute;
  p: PtrInt;
begin
  if (self <> nil) and
     (AttributeName <> '') then
  begin
    result := fCount - 1;
    a := pointer(fItems);
    if a <> nil then
      if IgnoreRange then // match 'AttributeName;range=1500-2999'
      begin
        for result := 0 to result do
        begin
          p := PosExChar(';', a^.AttributeName) - 1;
          if (p = length(AttributeName)) and
             IdemPropNameUSameLenNotNull(pointer(AttributeName), pointer(a^.AttributeName), p) then
            exit;
          inc(a);
        end;
      end
      else if result <> 0 then
      begin // case-insensitive name match, using fast interned string pointer comparison
        existing := _LdapIntern.Existing(AttributeName);
        if existing <> nil then // no need to search if it won't be there
          for result := 0 to result do
            if pointer(a^.AttributeName) = existing then
              exit
            else
              inc(a);
      end
      else if IdemPropNameU(fItems[0].AttributeName, AttributeName) then
        exit; // single attribute found with fCount = 1
  end;
  result := -1;
end;

function TLdapAttributeList.Find(const AttributeName: RawUtf8;
  IgnoreRange: boolean): TLdapAttribute;
var
  i: PtrInt;
begin
  i := FindIndex(AttributeName, IgnoreRange);
  if i >= 0 then
    result := fItems[i]
  else
    result := nil;
end;

function TLdapAttributeList.GetByName(const AttributeName: RawUtf8): RawUtf8;
begin
  Find(AttributeName).GetReadable(0, result);
end;

function TLdapAttributeList.FindIndex(AttributeType: TLdapAttributeType): PtrInt;
begin
  if (self <> nil) and
     (AttributeType <> atUndefined) then
    result := fIndexTypes[AttributeType]  // O(1) lookup - stored as index + 1
  else
    result := 0;
  dec(result);
end;

function TLdapAttributeList.Find(AttributeType: TLdapAttributeType): TLdapAttribute;
var
  i: PtrInt;
begin
  i := FindIndex(AttributeType);
  if i >= 0 then
    result := fItems[i]
  else
    result := nil;
end;

function TLdapAttributeList.Get(AttributeType: TLdapAttributeType): RawUtf8;
begin
  Find(AttributeType).GetReadable(0, result);
end;

function TLdapAttributeList.GetAll(AttributeType: TLdapAttributeType): TRawUtf8DynArray;
begin
  result := Find(AttributeType).GetAllReadable;
end;

function TLdapAttributeList.DoAdd(const aName: RawUtf8;
  aType: TLdapAttributeType): TLdapAttribute;
begin
  include(fKnownTypes, aType);
  result := TLdapAttribute.Create(aName, aType);
  PtrArrayAdd(fItems, result, fCount);
  if fCount <= 255 then // paranoid
    fIndexTypes[aType] := fCount; // store index + 1
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8): TLdapAttribute;
var
  i: PtrInt;
  n: RawUtf8;
begin
  if AttributeName = '' then
    ELdap.RaiseUtf8('Unexpected %.Add('''')', [self]);
  // search for existing TLdapAttribute instance during the name interning step
  if not _LdapIntern.Unique(n, AttributeName) then // n = normalized name
    for i := 0 to fCount - 1 do // fast interned pointer search as in Find()
    begin
      result := fItems[i];
      if pointer(result.AttributeName) = pointer(n) then
        exit;
    end;
  // need to add a new TLdapAttribute with this interned attribute name
  result := DoAdd(n, _AttributeNameType(pointer(n)));
end;

function TLdapAttributeList.Add(const AttributeName: RawUtf8;
  const AttributeValue: RawByteString; Option: TLdapAddOption): TLdapAttribute;
begin
  result := Add(AttributeName);
  result.Add(AttributeValue, Option);
end;

procedure TLdapAttributeList.AddPairs(const NameValuePairs: array of RawUtf8;
  Option: TLdapAddOption);
var
  i, n: PtrInt;
begin
  n := length(NameValuePairs);
  if (n <> 0) and
     (n and 1 = 0) then
    for i := 0 to (n shr 1) - 1 do
      Add(NameValuePairs[i * 2], NameValuePairs[i * 2 + 1], Option);
end;

function TLdapAttributeList.Add(AttributeType: TLdapAttributeType): TLdapAttribute;
begin
  if AttributeType = atUndefined then
    ELdap.RaiseUtf8('%.Add(atUndefined)', [self]);
  result := Find(AttributeType);
  if result = nil then
    result := DoAdd(AttrTypeName[AttributeType], AttributeType);
end;

function TLdapAttributeList.Add(AttributeType: TLdapAttributeType;
  const AttributeValue: RawByteString; Option: TLdapAddOption): TLdapAttribute;
begin
  result := Add(AttributeType);
  result.Add(AttributeValue, Option);
end;

procedure TLdapAttributeList.Add(const Types: array of TLdapAttributeType;
  const Values: array of const; Option: TLdapAddOption);
var
  i: PtrInt;
  v: RawUtf8;
begin
  if high(Types) = high(Values) then
    for i := 0 to high(Types) do
    begin
      VarRecToUtf8(@Values[i], v); // typically RawUtf8 or integer value
      Add(Types[i], v, Option)
    end
  else
    ELdap.RaiseUtf8('Inconsistent %.Add', [self]);
end;

function TLdapAttributeList.AddUnicodePwd(const aPassword: SpiUtf8): TLdapAttribute;
var
  encoded: RawByteString;
begin
  encoded := LdapUnicodePwd(aPassword);
  result := Add(atUnicodePwd, encoded);
  FillZero(encoded);
end;

procedure TLdapAttributeList.Delete(const AttributeName: RawUtf8);
begin
  Delete(FindIndex(AttributeName));
end;

procedure TLdapAttributeList.Delete(AttributeType: TLdapAttributeType);
begin
  Delete(FindIndex(AttributeType));
end;

procedure TLdapAttributeList.Delete(Index: integer);
begin
  if cardinal(Index) >= cardinal(fCount) then
    exit;
  PtrArrayDelete(fItems, Index, @fCount, pakClass);
  AfterModify;
end;

procedure TLdapAttributeList.AfterModify;
var
  i: integer;
  a: ^TLdapAttribute;
  at: TLdapAttributeType;
begin
  fKnownTypes := [];
  FillCharFast(fIndexTypes, SizeOf(fIndexTypes), 0);
  a := pointer(fItems);
  for i := 1 to MinPtrInt(fCount, 255) do // brute force is fast enough
  begin
    at := a^.KnownType;
    include(fKnownTypes, at);
    if at <> atUndefined then
      fIndexTypes[at] := i; // store index + 1
    inc(a);
  end;
end;

procedure TLdapAttributeList.SetAttr(
  AttributeType: TLdapAttributeType; const Value: RawUtf8);
begin
  Add(AttributeType, Value, aoReplaceValue);
end;

function TLdapAttributeList.AccountType: TSamAccountType;
begin
  result := SamAccountTypeFromText(Get(atSAMAccountType));
end;

function TLdapAttributeList.GroupTypes: TGroupTypes;
begin
  result := GroupTypesFromText(Get(atGroupType));
end;

function TLdapAttributeList.SystemFlags: TSystemFlags;
begin
  result := SystemFlagsFromText(Get(atSystemFlags));
end;

function TLdapAttributeList.GetUserAccountControl: TUserAccountControls;
begin
  result := UserAccountControlsFromText(Get(atUserAccountControl));
end;

procedure TLdapAttributeList.SetUserAccountControl(Value: TUserAccountControls);
begin
  Add(atUserAccountControl, ToUtf8(UserAccountControlsValue(Value)), aoReplaceValue);
end;

function TLdapAttributeList.Domain: PSid;
var
  a: TLdapAttribute;
begin
  a := Find(atObjectSid);
  if (a = nil) or
     not a.fObjectSidIsDomain then // checked once in TLdapAttribute.Add()
    result := nil
  else
    result := pointer(a.fList[0]);
end;


{ **************** LDIF Data Interchange Format }

// we follow https://www.rfc-editor.org/rfc/rfc2849 specs

function IsLdifSafe(p: PUtf8Char; l: PtrInt): boolean; // RFC 2849
begin
  if (p <> nil) and
     (l > 0) then
  begin
    result := false;
    if p^ in [#0, #10, #13, ' ', ':', '<', #128 .. #255] then
      exit; // SAFE-INIT-CHAR: <= 127, not NUL, LF, CR, SPACE, COLON, LESS-THAN
    dec(l);
    if p[l] = ' ' then
      exit; // "should not end with a space" RFC 2849 point 8)
    if l <> 0 then
      repeat
        inc(p);
        if p^ in [#0, #10, #13, #128 .. #255] then
          exit; // SAFE-CHAR: <= 127 not NUL, LF, CR
        dec(l);
      until l = 0;
  end;
  result := true;
end;

procedure AddWrapLine(w: TTextWriter; p: PUtf8Char; len, pos, maxlen: PtrInt);
var
  perline: PtrInt;
begin // here maxlen > 0
  perline := MinPtrInt(len, maxlen - pos);
  repeat
    w.AddNoJsonEscape(p, perline);
    dec(len, perline);
    if len = 0 then
      exit;
    w.AddDirect(#10, ' ');
    inc(p, perline);
    perline := MinPtrInt(len, maxlen);
  until false;
end;

procedure AddLdif(w: TTextWriter; const v: RawByteString;
  forcebase64, humanfriendly: boolean; att: pointer; maxlen: PtrInt);
var
  a: TLdapAttribute absolute att;
  truncated: boolean;
  tmp: RawUtf8;
begin
  dec(maxlen);
  if forcebase64 or
     not IsLdifSafe(pointer(v), length(v)) then
  begin
    // UTF-8 or binary content are stored as 'attributename:: <base64>'
    w.AddDirect(':', ' ');
    if (att = nil) or
       (maxlen <= 0) then
      w.WrBase64(pointer(v), length(v), {withmagic=}false) // line feeds optional
    else
    begin
      tmp := BinToBase64(v);
      AddWrapLine(w, pointer(tmp), length(tmp), length(a.AttributeName) + 2, maxlen);
    end;
    if forcebase64 or
       (not humanfriendly) or
       (a = nil) or
       (a.fKnownTypeStorage in [atsAny, atsInteger]) then
      exit;
    humanfriendly := false; // skip AttributeValueMakeReadable() below
  end
  else
  begin
    // simple 'attributename: <us-ascii>' form
    w.AddDirect(' ');
    if (att = nil) or
       (maxlen <= 0) then
      w.AddString(v)
    else
      AddWrapLine(w, pointer(v), length(v), length(a.AttributeName) + 1, maxlen);
    if (not humanfriendly) or
       (a = nil) or
       (a.fKnownTypeStorage = atsRawUtf8) then
      exit;
  end;
  // optionally append the human-friendly value as comment
  tmp := v;
  if humanfriendly then
    if AttributeValueMakeReadable(tmp, a.fKnownTypeStorage) or
       (pointer(tmp) = pointer(v)) then
      exit; // don't put hexadecimal or identical content in comment
  w.AddDirect(#10, '#', ' ');
  w.AddString(a.AttributeName); // is either OID or plain alphanum
  w.AddDirect(':', ' ', '<');
  truncated := (maxlen > 0) and
               (length(tmp) + length(a.AttributeName) > maxlen - 6);
  if truncated then
    FakeLength(tmp, Utf8TruncatedLength(tmp, maxlen - 9));
  w.AddString(tmp);             // human-readable text as comment
  if truncated then
    w.AddDirect('.', '.', '.', '>')
  else
    w.AddDirect('>');
end;


{ TLdifChange }

procedure TLdifChange.AssignTo(Dest: TClonable);
var
  d: TLdifChange absolute Dest;
begin
  inherited AssignTo(Dest); // copy all TLdapAttribute
  d.fChangeType := fChangeType;
end;


{ TLdifEntry }

procedure TLdifEntry.AssignTo(Dest: TClonable);
var
  d: TLdifEntry absolute Dest;
begin
  d.fDn := fDn;
  TLdifChange.CloneObjArray(fItems, d.fItems);
end;

destructor TLdifEntry.Destroy;
begin
  ObjArrayClear(fItems);
  inherited Destroy;
end;


{ TLdifFile }

procedure TLdifFile.AssignTo(Dest: TClonable);
var
  d: TLdifFile absolute Dest;
begin
  TLdifEntry.CloneObjArray(fItems, d.fItems);
end;

destructor TLdifFile.Destroy;
begin
  ObjArrayClear(fItems);
  inherited Destroy;
end;



{ **************** LDAP Response Storage }

{ TLdapResult }

constructor TLdapResult.Create;
begin
  fAttributes := TLdapAttributeList.Create;
end;

destructor TLdapResult.Destroy;
begin
  fAttributes.Free;
  inherited Destroy;
end;

procedure TLdapResult.AssignTo(Dest: TClonable);
var
  d: TLdapResult absolute Dest;
begin
  d.fObjectName := fObjectName;
  d.fCanonicalName := fCanonicalName;
  fAttributes.AssignTo(d.fAttributes);
end;

procedure TLdapResult.SetObjectName(const Value: RawUtf8);
begin
  fObjectName := NormalizeDN(Value);
end;

function TLdapResult.CanonicalName: RawUtf8;
begin
  if fCanonicalName = '' then
    fCanonicalName := DNToCN(fObjectName);
  result := fCanonicalName;
end;

function TLdapResult.GetAttr(AttributeType: TLdapAttributeType): RawUtf8;
begin
  fAttributes.Find(AttributeType).GetReadable(0, result);
end;

function TLdapResult.Get(AttributeType: TLdapAttributeType;
  out value: RawUtf8): boolean;
var
  a: TLdapAttribute;
begin
  a := fAttributes.Find(AttributeType);
  if a <> nil then
    a.GetReadable(0, Value);
  result := a <> nil;
end;

function TLdapResult.Find(const AttributeName: RawUtf8;
  IgnoreRange: boolean): TLdapAttribute;
begin
  if self = nil then
    result := nil
  else
    result := Attributes.Find(AttributeName, IgnoreRange);
end;

function TLdapResult.FindOrAdd(const AttributeName: RawUtf8): TLdapAttribute;
begin
  result := nil;
  if self = nil then
    exit;
  result := Attributes.Find(AttributeName);
  if result = nil then
    result := Attributes.Add(AttributeName);
end;

function TLdapResult.CopyObjectSid(out objectSid: RawUtf8): boolean;
begin
  objectSid := RawSidToText(Attributes.Find(atObjectSid).GetRaw);
  result := objectSid <> '';
end;

function TLdapResult.CopyObjectGuid(out objectGuid: TGuid): boolean;
var
  attr: TLdapAttribute;
  bin: RawByteString;
begin
  result := false;
  attr := Attributes.Find(atObjectGuid);
  if attr = nil then
    exit;
  bin := attr.GetRaw;
  if length(bin) = SizeOf(TGuid) then
  begin
    objectGuid := PGuid(bin)^;
    result := true;
  end;
end;

function TLdapResult.AppendToLocate(var Dvo: TDocVariantData;
  Options: TLdapResultOptions): PDocVariantData;
var
  dc, ou, cn: TRawUtf8DynArray;
  j: PtrInt;
begin
  result := @Dvo;
  if (roObjectNameAtRoot in Options) or
     not ParseDN(fObjectName, dc, ou, cn, {esc=}false, {noraise=}true) then
  begin
    result := result^.O_[fObjectName];
    exit;
  end;
  if [roWithCanonicalName, roCanonicalNameAtRoot] * Options <> [] then
    if fCanonicalName = '' then
      fCanonicalName := DNsToCN(dc, ou, cn); // compute know from parsed DN
  if roObjectNameWithoutDCAtRoot in Options then
    result := result^.O_[DNsToCN(nil, ou, cn)]
  else if roCanonicalNameAtRoot in Options then
    result := result^.O_[fCanonicalName]
  else if roCommonNameAtRoot in Options then
    result := result^.O_[RawUtf8ArrayToCsv(cn, '/', {reverse=}true)]
  else
  begin
    if dc <> nil then
      if not (roNoDCAtRoot in Options) then
        result := result^.O_[RawUtf8ArrayToCsv(dc, '.')];
    for j := high(ou) downto 0 do
      result := result^.O_[ou[j]];
    for j := high(cn) downto 0 do
      result := result^.O_[cn[j]];
  end;
end;

procedure TLdapResult.ExportToLdif(w: TTextWriter; aHumanFriendly: boolean;
  aMaxLineLen: PtrInt);
var
  i: PtrInt;
begin
  w.AddDirect('#', ' ');
  w.AddString(DNToCN(fObjectName, {NoRaise=}true));
  w.AddDirect(#10, 'd', 'n', ':');
  AddLdif(w, fObjectName, false, false, nil, 0);
  w.AddDirect(#10);
  for i := 0 to fAttributes.Count - 1 do
    fAttributes.Items[i].ExportToLdif(w, aHumanFriendly, aMaxLineLen);
  w.AddDirect(#10);
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
  fMicroSec := 0;
  fIn := 0;
  fOut := 0;
end;

procedure TLdapResultList.AssignTo(Dest: TClonable);
var
  d: TLdapResultList absolute Dest;
begin
  TLdapResult.CloneObjArray(fItems, d.fItems, @fCount, @d.fCount);
  d.fMicroSec := fMicroSec;
  d.fIn := fIn;
  d.fOut := fOut;
end;

function TLdapResultList.GetElapsed: RawUtf8;
var
  tmp: TShort16;
begin
  MicroSecToString(fMicroSec, tmp);
  FastSetString(result, @tmp[1], ord(tmp[0]));
end;

function TLdapResultList.ObjectNames(asCN, noSort: boolean): TRawUtf8DynArray;
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
  if not noSort then
    QuickSortRawUtf8(result, fCount);
end;

procedure TLdapResultList.GetAttributes(
  const AttrName: RawUtf8; AttrType: TLdapAttributeType;
  ObjectNames: PRawUtf8DynArray; out Values: TRawUtf8DynArray);
var
  i, n: PtrInt;
  r: TLdapResult;
  a: RawUtf8;
begin
  if ObjectNames <> nil then
    ObjectNames^ := nil;
  if (self = nil) or
     (fCount = 0) or
     ((AttrName = '') and
      (AttrType = atUndefined)) then
    exit;
  n := 0;
  for i := 0 to fCount - 1 do
  begin
    r := fItems[i];
    if AttrType = atUndefined then
      a := r.Attributes.GetByName(AttrName)
    else
      a := r.Attributes.Get(AttrType);
    if a = '' then
      continue;
    if Values = nil then
    begin
      SetLength(Values, fCount - i);
      if ObjectNames <> nil then
        SetLength(ObjectNames^, fCount - i);
    end;
    Values[n] := a;
    if ObjectNames <> nil then
      ObjectNames^[n] := r.ObjectName;
    inc(n);
  end;
  if n <> fCount then
  begin
    DynArrayFakeLength(Values, n);
    if ObjectNames <> nil then
      DynArrayFakeLength(ObjectNames^, n);
  end;
  if ObjectNames = nil then
    QuickSortRawUtf8(Values, n);
end;

function TLdapResultList.ObjectAttributes(const AttributeName: RawUtf8;
  ObjectNames: PRawUtf8DynArray): TRawUtf8DynArray;
begin
  GetAttributes(AttributeName, atUndefined, ObjectNames, result);
end;

function TLdapResultList.ObjectAttributes(AttrType: TLdapAttributeType;
  ObjectNames: PRawUtf8DynArray): TRawUtf8DynArray;
begin
  GetAttributes('', AttrType, ObjectNames, result);
end;

procedure TLdapResultList.AfterAdd;
begin
  if (fItems <> nil) and
     (fCount <> 0) then
    DynArrayFakeLength(fItems, fCount);
end;

function TLdapResultList.Add: TLdapResult;
begin
  result := TLdapResult.Create;
  ObjArrayAddCount(fItems, result, fCount);
end;

function TLdapResultList.Add(const ObjectName: RawUtf8): TLdapResult;
begin
  result := Add;
  result.fObjectName := ObjectName; // already normalized
end;

function TLdapResultList.Find(const ObjectName: RawUtf8): TLdapResult;
var
  i: PtrInt;
begin
  if self <> nil then
    for i := 0 to fCount - 1 do
    begin
      result := fItems[i];
      if result.ObjectName = ObjectName then
        exit;
    end;
  result := nil;
end;

function TLdapResultList.FindOrAdd(const ObjectName: RawUtf8): TLdapResult;
begin
  result := Find(ObjectName);
  if result = nil then
    result := Add(ObjectName);
end;

function TLdapResultList.FindOrAdd(const ObjectName, AttributeName: RawUtf8): TLdapAttribute;
begin
  result := FindOrAdd(ObjectName).FindOrAdd(AttributeName);
end;

function TLdapResultList.ExportToLdifContent(aHumanFriendly: boolean;
  aMaxLineLen: PtrInt): RawUtf8;
var
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  w: TTextWriter;
  i: PtrInt;
begin
  w := DefaultJsonWriter.CreateOwnedStream(tmp);
  try
    W.AddShort('version: 1'#10);
    for i := 0 to Count - 1 do
      Items[i].ExportToLdif(w, aHumanFriendly, aMaxLineLen);
    W.Add('# total number of entries: %'#10, [Count]);
    w.SetText(result);
  finally
    w.Free;
  end;
end;

function TLdapResultList.Dump(NoTime: boolean): RawUtf8;
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
    if not NoTime then
    begin
      w.AddDirect(' ', 'i', 'n', ' ');
      w.AddShort(MicroSecToString(fMicroSec));
      w.AddShorter(' sent=');
      w.AddShort(KBNoSpace(fOut));
      w.AddShorter(' recv=');
      w.AddShort(KBNoSpace(fIn));
    end;
    w.AddDirectNewLine; // = #13#10 on Windows, #10 on POSIX
    for i := 0 to Count - 1 do
    begin
      res := Items[i];
      w.Add('%: %' + CRLF, [i, DNToCN(res.ObjectName, {NoRaise=}true)]);
      w.Add('  objectName : %' + CRLF, [res.ObjectName]);
      for j := 0 to res.Attributes.Count - 1 do
      begin
        attr := res.Attributes.Items[j];
        w.Add('  % : ', [attr.AttributeName]);
        if attr.Count <> 1 then
          w.AddDirectNewLine;
        for k := 0 to attr.Count - 1 do
        begin
          if attr.Count <> 1 then
            w.AddShorter('    - ');
          w.AddString(attr.GetReadable(k));
          w.AddDirectNewLine;
        end;
      end;
      w.AddDirectNewLine;
    end;
    w.SetText(result);
  finally
    w.Free;
  end;
end;

procedure TLdapResultList.AppendTo(var Dvo: TDocVariantData;
  Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8; Dom: PSid);
var
  i, j, k: PtrInt;
  res: TLdapResult;
  attr: ^TLdapAttribute;
  uuid: TAppendShortUuid;
  v: PDocVariantData;
  a: TDocVariantData;
begin
  if ord(roNoDCAtRoot in Options) +
     ord(roObjectNameAtRoot in Options) +
     ord(roObjectNameWithoutDCAtRoot in Options) +
     ord(roCanonicalNameAtRoot in Options) +
     ord(roCommonNameAtRoot in Options) > 1 then
    ELdap.RaiseUtf8('%.AppendTo: roNoDCAtRoot, roObjectNameAtRoot, ' +
      'roObjectNameWithoutDCAtRoot, roCanonicalNameAtRoot and ' +
      'roCommonNameAtRoot are exclusive', [self]);
  if (roRawValues in Options) and
     (Options * [roRawBoolean .. roRawAccountType] <> []) then
    ELdap.RaiseUtf8('%.AppendTo: roRawValues and other roRaw* options ' +
      'are exclusive', [self]);
  uuid := KnownUuid(Options);
  for i := 0 to Count - 1 do
  begin
    res := Items[i];
    if res.ObjectName = '' then
      continue; // malformed data - a primary key is required
    v := res.AppendToLocate(Dvo, Options);
    if ObjectAttributeField = '' then
      continue; // no attribute
    a.Init(mFast, dvObject);
    a.SetCount(res.Attributes.Count +
               ord(not(roNoObjectName in Options)) +
               ord(roWithCanonicalName in Options));
    a.Capacity := a.Count;
    k := 0;
    if not(roNoObjectName in Options) then
    begin
      a.Names[0] := sObjectName;
      RawUtf8ToVariant(res.ObjectName, a.Values[0]);
      inc(k);
    end;
    if roWithCanonicalName in Options then
    begin
      a.Names[k] := sCanonicalName;
      RawUtf8ToVariant(res.CanonicalName, a.Values[k]);
      inc(k);
    end;
    if roNoSddlDomainRid in Options then
      Dom := nil // won't recognize known RID in this context
    else if Dom = nil then // if not specified e.g. from TLdapClient.DomainSid
      Dom := res.Attributes.Domain; // guess RID from atObjectSid
    attr := pointer(res.Attributes.Items);
    for j := k to k + res.Attributes.Count - 1 do
    begin
      a.Names[j] := attr^.AttributeName; // use TRawUtf8InterningSlot
      attr^.SetNewVariant(a.Values[j], Options, Dom, uuid);
      inc(attr);
    end;
    if ObjectAttributeField = '*' then
      v^.AddOrUpdateFrom(a, {onlymissing=}true)
    else
      v^.AddValue(ObjectAttributeField, variant(a), {owned=}true);
    a.Clear; // mandatory to prepare the next a.Init in this loop
  end;
end;

procedure TLdapResultList.ExtractPagedAttributes(Source: TLdapResultList);
var
  r, a, p: PtrInt;
  main: RawUtf8;
  res: TLdapResult;
  att, attmain: TLdapAttribute;
begin
  // see https://evetsleep.github.io/activedirectory/2016/08/06/PagingMembers.html
  for r := 0 to Source.Count - 1 do
  begin
    res := Source.Items[r];
    for a := res.Attributes.Count - 1 downto 0 do // Delete(a) may happen below
    begin
      // check if is a '###;range=...' paged attribute
      att := res.Attributes.Items[a];
      if att.KnownType <> atUndefined then // '###;range=...' is never detected
        continue;
      p := PosExChar(';', att.AttributeName);
      if (p = 0) or
         not IdemPChar(PUtf8Char(pointer(att.AttributeName)) + p, 'RANGE=') then
        continue;
      main := copy(att.AttributeName, 1, p - 1); // trim ';range=...' into ###
      attmain := res.Attributes.Find(main);
      if (attmain = nil) or
         (attmain.Count <> 0) then
        continue; // was a regular '###;range=...' request, not a paged attribute
      // create or update any existing partial results
      FindOrAdd(res.ObjectName, main).AddFrom(att);
      res.Attributes.Delete(a); // never include '###;range=0-1499' directly
    end;
  end;
end;

function TLdapResultList.GetVariant(Options: TLdapResultOptions;
  const ObjectAttributeField: RawUtf8): variant;
begin
  VarClear(result);
  TDocVariantData(result).Init(mFast, dvObject);
  AppendTo(TDocVariantData(result), Options, ObjectAttributeField);
end;

function TLdapResultList.GetJson(Options: TLdapResultOptions;
  const ObjectAttributeField: RawUtf8; Format: TTextWriterJsonFormat): RawUtf8;
var
  v: variant;
begin
  v := GetVariant(Options, ObjectAttributeField);
  DocVariantType.ToJson(@v, result, '', '', Format);
end;


{ **************** Main TLdapClient Class }

{ TLdapClientSettings }

constructor TLdapClientSettings.Create(const aUri: RawUtf8);
begin
  inherited Create;
  fTimeout := 5000;
  fAutoReconnect := true; // sounds fair enough
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
          result := test.Transmission; // lctEncrypted or lctPlain
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
  Join([LDAP_DEFAULT_SCHEME[fTls], fTargetHost], result);
  if fTargetPort <> LDAP_DEFAULT_PORT[fTls] then
    Append(result, ':', fTargetPort);
  if fKerberosDN <> '' then
    Append(result, '/', fKerberosDN);
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


{ TLdapObject }

procedure TLdapObject.CustomAdd(Attr: TLdapAttribute);
var
  n: PtrInt;
begin
  if Attr = nil then
    exit;
  n := length(customNames);
  SetLength(customNames, n + 1);
  customNames[n] := Attr.AttributeName;
  SetLength(customValues, n + 1);
  Attr.GetReadable(0, customValues[n]);
end;

procedure TLdapObject.FillObject(Attributes: TLdapAttributeList;
  const CustomAttributes: TRawUtf8DynArray; CustomTypes: TLdapAttributeTypes);
var
  i: PtrInt;
  t: TLdapAttributeType;
begin
  sAMAccountName    := Attributes[atSAMAccountName];
  distinguishedName := Attributes[atDistinguishedName];
  canonicalName     := DNToCN(distinguishedName, {NoRaise=}true);
  name              := Attributes[atName];
  CN                := Attributes[atCommonName];
  description       := Attributes[atDescription];
  objectSid         := Attributes[atObjectSid];
  objectGUID        := Attributes[atObjectGuid];
  whenCreated       := LdapToDate(Attributes[atWhenCreated]);
  whenChanged       := LdapToDate(Attributes[atWhenChanged]);
  for i := 0 to length(CustomAttributes) - 1 do
    CustomAdd(Attributes.Find(CustomAttributes[i]));
  if CustomTypes <> [] then
    for t := succ(low(t)) to high(t) do
      if t in CustomTypes then
      begin
        CustomAdd(Attributes.Find(t));
        exclude(CustomTypes, t);
        if CustomTypes = [] then
          break;
      end;
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

procedure TLdapGroup.Fill(Attributes: TLdapAttributeList; WithMember: boolean;
  const CustomAttributes: TRawUtf8DynArray; const CustomTypes: TLdapAttributeTypes);
begin
  FillObject(Attributes, CustomAttributes, CustomTypes);
  ToCardinal(SplitRight(objectSID, '-'), PrimaryGroupID);
  groupType := Attributes.GroupTypes;
  if WithMember then
    member := Attributes.Find(atMember).GetAllReadable;
end;


{ TLdapUser }

procedure TLdapUser.Fill(Attributes: TLdapAttributeList; WithMemberOf: boolean;
  const CustomAttributes: TRawUtf8DynArray; const CustomTypes: TLdapAttributeTypes);
begin
  FillObject(Attributes, CustomAttributes, CustomTypes);
  userPrincipalName := Attributes[atUserPrincipalName];
  displayName       := Attributes[atDisplayName];
  mail              := Attributes[atMail];
  pwdLastSet        := LdapToDate(Attributes[atPwdLastSet]);
  lastLogon         := LdapToDate(Attributes[atLastLogon]);
  ToCardinal(Attributes[atPrimaryGroupID], primaryGroupID);
  if WithMemberOf then
    memberOf := Attributes.Find(atMemberOf).GetAllReadable;
  userAccountControl := Attributes.UserAccountControl;
end;


{ TLdapComputer }

procedure TLdapComputer.Fill(Attributes: TLdapAttributeList;
  const CustomAttributes: TRawUtf8DynArray; const CustomTypes: TLdapAttributeTypes);
begin
  FillObject(Attributes, CustomAttributes, CustomTypes);
  pwdLastSet           := LdapToDate(Attributes[atPwdLastSet]);
  lastLogonTimestamp   := LdapToDate(Attributes[atLastLogon]);
  admPwdExpirationTime := LdapToDate(Attributes[atMcsAdmPwdExpirationTime]);
  userAccountControl   := Attributes.UserAccountControl;
  ToCardinal(Attributes[atPrimaryGroupID], primaryGroupID);
  ToCardinal(Attributes[atLogonCount], logonCount);
  ToCardinal(Attributes[atBadPwdCount], badPwdCount);
  dNSHostName          := Attributes[atDnsHostName];
  operatingSystem      := Attributes[atOperatingSystem];
  operatingSystemVersion := Attributes[atOperatingSystemVersion];
  servicePrincipalName := Attributes.GetAll(atServicePrincipalName);
end;


{ TLdapClient }

constructor TLdapClient.Create;
begin
  inherited Create;
  fSettings := TLdapClientSettings.Create;
  fReferals := TRawUtf8List.Create;
  fTlsContext.IgnoreCertificateErrors := true;
  fBoundDigestAlgo := daMD5_Sess;
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


// **** TLdapClient connection methods

procedure TLdapClient.SetUnknownError(const msg: RawUtf8);
begin
  fResultError := leUnknown;
  fResultCode := -1;
  fResultString := msg;
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, msg, self);
end;

procedure TLdapClient.SetUnknownError(const fmt: RawUtf8; const args: array of const);
begin
  SetUnknownError(FormatUtf8(fmt, args));
end;

function TLdapClient.Connect(DiscoverMode: TLdapClientConnect;
  DelayMS: integer): boolean;
var
  dc: TRawUtf8DynArray;
  h, p: RawUtf8;
  i: PtrInt;
  log: ISynLog;
begin
  result := fSock <> nil;
  if result then
    exit; // socket was already connected
  if fLog.HasLevel([sllEnter]) then
    fLog.EnterLocal(log, self, 'Connect');
  fResultError := leUnknown;
  fResultString := '';
  if fSettings.TargetHost = '' then
    if lccNoDiscovery in DiscoverMode then
    begin
      SetUnknownError('Connect: no TargetHost supplied');
      exit;
    end
    else
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
      if dc = nil then
      begin
        SetUnknownError('Connect: no LDAP server found on this network');
        exit;
      end;
      if Assigned(log) then
        log.Log(sllTrace, 'Connect: cldap', TypeInfo(TRawUtf8DynArray), dc, self);
    end
  else
    // try the LDAP server as specified in TLdapClient settings
    AddRawUtf8(dc, Join([fSettings.TargetHost, ':', fSettings.TargetPort]));
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
      if result and
         (fSettings.AutoBind <> lcbNone) then
        result := DoBind(fSettings.AutoBind);
      if result then
      begin
        fSettings.TargetHost := h;
        fSettings.TargetPort := p;
        fSettings.Tls := fSock.TLS.Enabled;
        if Assigned(log) then
          log.Log(sllTrace, 'Connected to %', [fSettings.TargetUri], self);
        exit; // success
      end;
    except
      on E: Exception do
      begin
        FreeAndNil(fSock); // abort and try next dc[]
        SetUnknownError('Connect %: %', [E, E.Message]);
      end;
    end;
  if fResultString = '' then
    SetUnknownError('Connect: failed');
end;

function TLdapClient.GetTlsContext: PNetTlsContext;
begin
  result := @fTlsContext;
end;

function TLdapClient.NetbiosDN: RawUtf8;
begin
  if (fNetbiosDN = '') and
     EnsureConnected('NetbiosDN') and
     fBound then
    fNetbiosDN := SearchObject('CN=Partitions,' + ConfigDN,
      FormatUtf8('(&(nETBIOSName=*)(nCName=%))', [DefaultDN]),
      'nETBIOSName', lssWholeSubtree).GetReadable;
  result := fNetbiosDN;
end;

procedure TLdapClient.RetrieveRootDseInfo;
var
  root: TLdapResult;
begin
  // retrieve all needed Root DSE attributes in a single call
  if not EnsureConnected('RetrieveRootDseInfo') then
    exit;
  include(fFlags, fRetrieveRootDseInfo);
  // note: root DSE distinguished name is the zero-length string
  root := SearchObject('', '(objectClass=top)', [
    'rootDomainNamingContext',
    'defaultNamingContext',
    'namingContexts',
    'configurationNamingContext',
    'supportedSASLMechanisms',
    'supportedControl',
    'supportedExtension',
    'vendorName',
    'ldapServiceName'
    ]);
  if root = nil then
    exit; // no Root DSE (won't retry again)
  fRootDN         := root.Attributes.GetByName('rootDomainNamingContext');
  fDefaultDN      := root.Attributes.GetByName('defaultNamingContext');
  fNamingContexts := root.Attributes.Find('namingContexts').GetAllReadable;
  if length(fNamingContexts) = 1 then
  begin
    if fRootDN = '' then
      fRootDN := fNamingContexts[0];
    if fDefaultDN = '' then
      fDefaultDN := fNamingContexts[0];
  end;
  fConfigDN       := root.Attributes.GetByName('configurationNamingContext');
  fMechanisms     := root.Attributes.Find('supportedSASLMechanisms').GetAllReadable;
  fControls       := root.Attributes.Find('supportedControl').GetAllReadable;
  DeduplicateRawUtf8(fControls);
  fExtensions     := root.Attributes.Find('supportedExtension').GetAllReadable;
  DeduplicateRawUtf8(fExtensions);
  fVendorName     := root.Attributes.GetByName('vendorName');
  fServiceName    := root.Attributes.GetByName('ldapServiceName');
end;

function TLdapClient.RootDN: RawUtf8;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fRootDN;
end;

function TLdapClient.DefaultDN(const BaseDN: RawUtf8): RawUtf8;
begin
  if BaseDN <> '' then
    result := BaseDN
  else
  begin
    if not (fRetrieveRootDseInfo in fFlags) then
      RetrieveRootDseInfo;
    result := fDefaultDN;
  end;
end;

function TLdapClient.NamingContexts: TRawUtf8DynArray;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fNamingContexts;
end;

function TLdapClient.ConfigDN: RawUtf8;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fConfigDN;
end;

function TLdapClient.VendorName: RawUtf8;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fVendorName;
end;

function TLdapClient.ServiceName: RawUtf8;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fServiceName;
end;

function TLdapClient.Mechanisms: TRawUtf8DynArray;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fMechanisms;
end;

function TLdapClient.Controls: TRawUtf8DynArray;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fControls;
end;

function TLdapClient.Extensions: TRawUtf8DynArray;
begin
  if not (fRetrieveRootDseInfo in fFlags) then
    RetrieveRootDseInfo;
  result := fExtensions;
end;

function TLdapClient.SupportsMech(const MechanismName: RawUtf8): boolean;
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

function TLdapClient.SupportsControl(const ControlName: RawUtf8): boolean;
begin
  result := (Controls <> nil) and
    (ControlName <> '') and
    (FastFindPUtf8CharSorted(
      pointer(fControls), high(fControls), pointer(ControlName)) >= 0);
end;

function TLdapClient.SupportsExt(const ExtensionName: RawUtf8): boolean;
begin
  result := (Extensions <> nil) and
    (ExtensionName <> '') and
    (FastFindPUtf8CharSorted(
      pointer(fExtensions), high(fExtensions), pointer(ExtensionName)) >= 0);
end;

const
  // Well-Known LDAP Objects GUID
  // https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-adts/
  //   5a00c890-6be5-4575-93c4-8bf8be0ca8d8
  LDAP_GUID: array [TLdapKnownObject] of RawUtf8 = (
    'AA312825768811D1ADED00C04FD8D5CD',  // lkoComputers
    '18E2EA80684F11D2B9AA00C04F79F805',  // lkoDeletedObjects
    'A361B2FFFFD211D1AA4B00C04FD7D83A',  // lkoDomainControllers
    '22B70C67D56E4EFB91E9300FCA3DC1AA',  // lkoForeignSecurityPrincipals
    '2FBAC1870ADE11D297C400C04FD8D5CD',  // lkoInfrastructure
    'AB8153B7768811D1ADED00C04FD8D5CD',  // lkoLostAndFound
    'F4BE92A4C777485E878E9421D53087DB',  // lkoMicrosoftProgramData
    '6227F0AF1FC2410D8E3BB10615BB5B0F',  // lkoNtdsQuotas
    '09460C08AE1E4A4EA0F64AEE7DAA1E5A',  // lkoProgramData
    'AB1D30F3768811D1ADED00C04FD8D5CD',  // lkoSystems
    'A9D1CA15768811D1ADED00C04FD8D5CD',  // lkoUsers
    '1EB93889E40C45DF9F0C64D23BBB6237'); // lkoManagedServiceAccounts

procedure TLdapClient.RetrieveDefaultDNInfo;
var
  tmp: TRawUtf8DynArray;
  o: TLdapKnownObject;
  i: PtrInt;
  p: PUtf8Char;
  res: TLdapResult;
begin
  if not EnsureConnected('RetrieveDefaultDNInfo') then
    exit;
  include(fFlags, fRetrievedDefaultDNInfo);
  res := SearchObject(DefaultDN, '',
    ['wellKnownObjects', 'otherWellKnownObjects', 'objectSid']);
  if res = nil then
    exit;
  fDomainSid := res.Attributes.Find(atObjectSid).GetRaw;
  if not IsValidRawSid(fDomainSid) or
     not SidIsDomain(pointer(fDomainSid)) then
    fDomainSid := '';
  tmp := res.Find('wellKnownObjects').GetAllReadable;
  AddRawUtf8(tmp, res.Find('otherWellKnownObjects').GetAllReadable);
  if tmp = nil then
    exit;
  for i := 0 to high(tmp) do
    if not NetStartWith(pointer(tmp[i]), 'B:32:') then
      tmp[i] := '';
  for o := low(o) to high(o) do
    for i := 0 to high(tmp) do
    begin
      p := pointer(tmp[i]);
      if (p <> nil) and
         NetStartWith(p + 5, pointer(LDAP_GUID[o])) then
        begin
          system.delete(tmp[i], 1, 38);
          fWellKnownObjects[o] := tmp[i];
          tmp[i] := ''; // no need to search this one any more
          break;
        end;
    end;
end;

function TLdapClient.WellKnownObject(WellKnown: TLdapKnownObject;
  AsCN: boolean): RawUtf8;
begin
  if not (fRetrievedDefaultDNInfo in fFlags) then
    RetrieveDefaultDNInfo;
  result := fWellKnownObjects[WellKnown];
  if AsCN then
    result := DNToCn(result, {NoRaise=}true);
end;

function TLdapClient.DomainSid: RawSid;
begin
  if not (fRetrievedDefaultDNInfo in fFlags) then
    RetrieveDefaultDNInfo;
  result := fDomainSid;
end;


// **** TLdapClient protocol methods

function TLdapClient.BuildPacket(const Asn1Data: TAsnObject): TAsnObject;
begin
  inc(fSeq);
  result := Asn(ASN1_SEQ, [
              Asn(fSeq),
              Asn1Data
            ]);
  if not (fSecContextEncrypt in fFlags) then
    exit;
  result := SecEncrypt(fSecContext, result);
  insert('0000', result, 1); // SASL Buffer Length prefix
  PCardinal(result)^ := bswap32(length(result) - 4);
end;

procedure TLdapClient.SendPacket(const Asn1Data: TAsnObject);
var
  packet: RawByteString;
  res: TNetResult;
  raw: integer;
begin
  {$ifdef ASNDEBUG}
  ConsoleWrite('------%Sending=%', [CRLF, AsnDump(Asn1Data)]);
  {$endif ASNDEBUG}
  packet := BuildPacket(Asn1Data);
  if fSock.TrySndLow(pointer(packet), length(packet), @res, @raw) then
    exit; // success
  case res of
    nrNoSocket:
      ELdap.RaiseUtf8('%.SendPacket: not connected', [self]);
    nrClosed: // it is usual for most LDAP servers to close any idle socket
      begin
        if Assigned(fOnDisconnect) then
          fOnDisconnect(self)
        else if fSettings.AutoReconnect then // is TRUE by default
          if not Reconnect('SendPacket') then
            ELdap.RaiseUtf8('%.SendPacket: AutoReconnect failed as %',
              [self, fResultString]);
        if fSock.SockConnected then
        begin
          packet := BuildPacket(Asn1Data); // rebuild with new fSeq + encryption
          if fSock.TrySndLow(pointer(packet), length(packet), @res, @raw) then
            exit; // success after re-connection
        end;
      end;
  end;
  // a non-recoverable socket error occured at sending the request
  raise ENetSock.Create('SendPacket(%s) failed', self, [fSock.Server], res, @raw);
end;

procedure TLdapClient.ReceivePacketFillSockBuffer;
var
  saslLen, len, err: integer;
  ciphered: RawByteString;
  res: TNetResult;
begin
  fSockBufferPos := 0;
  fSockBuffer := '';
  if fSecContextEncrypt in fFlags then
  begin
    // through Kerberos encryption (sealing)
    saslLen := 0;
    len := SizeOf(saslLen);
    if fSock.TrySockRecv(@saslLen, len, {stopbeforelen=}false, @res, @err) then
    begin
      saslLen := bswap32(saslLen); // SASL Buffer Length prefix
      if saslLen > 16 shl 20 then  // 16MB chunk seems big enough: usually 64KB
        res := nrTooManyConnections
      else if fSock.TrySockRecv(FastNewRawByteString(ciphered, saslLen),
                saslLen, {stopbeforelen=}false, @res, @err) then
        fSockBuffer := SecDecrypt(fSecContext, ciphered);
    end;
  end
  else
    // get as much as possible unciphered data from socket
    fSockBuffer := fSock.SockReceiveString(@res, @err);

  case res of
    nrNoSocket:
      ELdap.RaiseUtf8('%.ReceivePacketFillSockBuffer: not connected', [self]);
    nrClosed: // it is usual for most LDAP servers to close any idle socket
      begin
        if Assigned(fOnDisconnect) then
          fOnDisconnect(self)
        else if fSettings.AutoReconnect then // is TRUE by default
          if not Reconnect('ReceivePacketFillSockBuffer') then
            ELdap.RaiseUtf8('%.ReceivePacketFillSockBuffer: AutoReconnect failed as %',
              [self, fResultString]);
        if fSock.SockConnected then
        begin // Not sure about what to do in here

        end;
      end;
  end;
  if fSockBuffer = '' then
    ELdap.RaiseUtf8('%.ReceivePacket: error #% % from %:%',
      [self, err, _NR[res], fSettings.TargetHost, fSettings.TargetPort]);
  {$ifdef ASNDEBUG}
  ConsoleWrite('Packet received bytes = %', [length(fSockBuffer)]);
  {$endif ASNDEBUG}
end;

procedure TLdapClient.ReceivePacket(Dest: pointer; DestLen: PtrInt);
var
  len: PtrInt;
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
  ConsoleWrite('------%Received=%', [CRLF, AsnDump(result)]);
  {$endif ASNDEBUG}
end;

// see https://ldap.com/ldapv3-wire-protocol-reference-ldap-result

function TLdapClient.DecodeResponse(
  var Pos: integer; const Asn1Response: TAsnObject): TAsnObject;
var
  x, asntype, seqend, winerr: integer;
  s, t: TAsnObject;
  errmsg: RawUtf8;
  hex: PAnsiChar;
begin
  result := '';
  fResultCode := -1;
  fResultError := leUnknown;
  fResultString := '';
  fResponseCode := LDAP_ASN1_ERROR;
  fResponseDN := '';
  if AsnNext(Pos, Asn1Response) <> ASN1_SEQ then
  begin
    SetUnknownError('Malformated response: missing ASN.1 SEQ');
    exit;
  end;
  seqend := AsnNextInteger(Pos, Asn1Response, asntype);
  if (seqend <> fSeq) or
     (asntype <> ASN1_INT) then
   begin
     SetUnknownError('Unexpected SEQ=% expected=%', [seqend, fSeq]);
     exit;
   end;
  fResponseCode := AsnNext(Pos, Asn1Response, nil, @seqend);
  if fResponseCode in LDAP_ASN1_RESPONSES then
  begin
    // final response
    fResultCode := AsnNextInteger(Pos, Asn1Response, asntype);
    AsnNext(Pos, Asn1Response, @fResponseDN);   // matchedDN
    AsnNext(Pos, Asn1Response, @fResultString); // diagnosticMessage
    if not (fResultCode in LDAP_RES_NOERROR) then
    begin
      errmsg := RawLdapErrorString(fResultCode, fResultError);
      if fResultCode = LDAP_RES_INVALID_CREDENTIALS then
      begin
// https://ldapwiki.com/wiki/Wiki.jsp?page=Common%20Active%20Directory%20Bind%20Errors
        hex := pointer(StrPosI(', DATA ', pointer(fResultString)));
        if hex <> nil then
        begin
          winerr := ParseHex0x(hex + 7, {no0x=}true);
          if winerr <> 0 then
            Append(errmsg, [' ', WinErrorShort(winerr)]);
        end;
      end;
      if fResultString = '' then
        fResultString := errmsg
      else
        fResultString := FormatUtf8('% [%]', [errmsg, fResultString]);
      if Assigned(fLog) then
        fLog.Add.Log(sllTrace, 'DecodeResponse: %', [fResultString], self);
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
  SendPacket(Asn1Data); // raise exception on failure
  resp := ReceiveResponse;
  x := 1;
  result := DecodeResponse(x, resp);
end;


// **** TLdapClient binding methods

// see https://ldap.com/ldapv3-wire-protocol-reference-bind

function TLdapClient.Bind: boolean;
var
  log: ISynLog;
begin
  result := false;
  if fBound or
     not Connect then
    exit;
  if (fSettings.Password <> '') and
     not fSettings.Tls and
     not fSettings.AllowUnsafePasswordBind then
    ELdap.RaiseUtf8('%.Bind with a password requires a TLS connection', [self]);
  fLog.EnterLocal(log, 'Bind as %', [fSettings.UserName], self);
  try
    SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                     Asn(fVersion),
                     AsnOctStr(fSettings.UserName),
                     AsnTyped(fSettings.Password, ASN1_CTX0)]));
    if fResultCode <> LDAP_RES_SUCCESS then
      exit; // binding error
    fBound := true;
    fBoundAs := lcbPlain;
    fBoundUser := fSettings.UserName;
    result := true;
  finally
    if Assigned(log) then
      log.Log(LOG_DEBUGERROR[not result], 'Bind=% % %',
        [BOOL_STR[result], fResultCode, fResultString], self);
  end;
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
  log: ISynLog;
begin
  result := false;
  if fBound or
     not Connect then
    exit;
  fLog.EnterLocal(log, 'BindSalsDigest(%) as %',
    [DIGEST_NAME[Algo], fSettings.UserName], self);
  if DIGEST_ALGONAME[Algo] = '' then
    ELdap.RaiseUtf8('Unsupported %.BindSaslDigest(%) algorithm',
      [self, DIGEST_NAME[Algo]]);
  if fSettings.Password = '' then
    result := Bind
  else
  try
    digreq := Asn(LDAP_ASN1_BIND_REQUEST, [
                Asn(fVersion),
                AsnOctStr(''),
                Asn(ASN1_CTC3, [
                  AsnOctStr(DIGEST_ALGONAME[Algo])])]);
    s := SendAndReceive(digreq);
    if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
      exit;
    x := 1;
    AsnNext(x, s, @t);
    dig := DigestClient(Algo, t, '', 'ldap/' + LowerCaseU(fSock.Server),
      fSettings.UserName, fSettings.Password, 'digest-uri');
    SendAndReceive(Asn(LDAP_ASN1_BIND_REQUEST, [
                     Asn(fVersion),
                     AsnOctStr(''),
                     Asn(ASN1_CTC3, [
                       AsnOctStr(DIGEST_ALGONAME[Algo]),
                       AsnOctStr(dig)])]));
    if fResultCode = LDAP_RES_SASL_BIND_IN_PROGRESS then
      SendAndReceive(digreq);
    if fResultCode <> LDAP_RES_SUCCESS then
      exit; // binding error
    fBound := true;
    fBoundAs := lcbDigest;
    fBoundDigestAlgo := Algo;
    fBoundUser := fSettings.UserName;
    result := true;
  finally
    if Assigned(log) then
      log.Log(LOG_DEBUGERROR[not result], 'BindSaslDigest=% % %',
        [BOOL_STR[result], fResultCode, fResultString], self);
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
  datain, dataout, cert: RawByteString;
  certhashname: RawUtf8;
  channelbindinghash: THash512Rec;
  t, req1, req2: TAsnObject;
  needencrypt: boolean;
  seclayers: TKerbSecLayer;
  secmaxsize: integer;
  log: ISynLog;

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
  // initiate GSSAPI bind request
  if not InitializeDomainAuth then
  begin
    SetUnknownError('Kerberos: Error initializing the library');
    exit;
  end;
  if (fSettings.KerberosSpn = '') and
     (fSettings.KerberosDN <> '') then
    fSettings.KerberosSpn := 'LDAP/' + fSettings.TargetHost + {noport}
                             '@' + UpperCase(fSettings.KerberosDN);
  fLog.EnterLocal(log, 'BindSaslKerberos(%) on %',
    [fSettings.UserName, fSettings.KerberosSpn], self);
  needencrypt := false;
  try
    req1 := Asn(LDAP_ASN1_BIND_REQUEST, [
              Asn(fVersion),
              AsnOctStr(''),
              Asn(ASN1_CTC3, [
                AsnOctStr('GSSAPI')])]);
    t := SendAndReceive(req1);
    if fResultCode <> LDAP_RES_SASL_BIND_IN_PROGRESS then
      exit;
    // setup GSSAPI / Kerberos context
    InvalidateSecContext(fSecContext);
    if fSock.TLS.Enabled and
       Assigned(fSock.Secure) and
       not fSettings.KerberosDisableChannelBinding then
    begin
      // Kerberos + TLS now requires tls-server-end-point channel binding
      cert := fSock.Secure.GetRawCert(@certhashname);
      if cert <> '' then
      begin
        fSecContext.ChannelBindingsHashLen :=
          HashForChannelBinding(cert, certhashname, channelbindinghash);
        if fSecContext.ChannelBindingsHashLen <> 0 then
          fSecContext.ChannelBindingsHash := @channelbindinghash;
      end;
    end;
    // main GSSAPI / Kerberos loop
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
            SetUnknownError('Kerberos %: %', [E, E.Message]);
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
              SetUnknownError('Kerberos: aborted SASL handshake');
            exit;
          end;
          ParseInput;
          datain := SecDecrypt(fSecContext, datain);
          if length(datain) <> 4 then
          begin
            SetUnknownError('Kerberos: Unexpected SecLayer response');
            exit; // expected format is #0=SecLayer #1#2#3=MaxMsgSizeInNetOrder
          end;
          seclayers := TKerbSecLayer(datain[1]);
          secmaxsize := bswap32(PCardinal(datain)^) and $00ffffff;
          if seclayers = [] then
            // the server requires no additional security layer
            if secmaxsize <> 0 then
            begin
              // invalid answer (as stated by RFC 4752)
              SetUnknownError('Kerberos: Unexpected secmaxsize=%', [secmaxsize]);
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
            SetUnknownError('Kerberos: Unsupported [%] method(s)',
              [GetSetName(TypeInfo(TKerbSecLayer), seclayers)]);
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
                  AsnOctStr(''),
                  Asn(ASN1_CTC3, [
                    AsnOctStr('GSSAPI'),
                    AsnOctStr(dataout)])]);
        t := SendAndReceive(req2);
      until not (fResultCode in [LDAP_RES_SUCCESS, LDAP_RES_SASL_BIND_IN_PROGRESS]);
      if fResultCode <> LDAP_RES_SUCCESS then
        exit; // error
      // we are successfully authenticated (and probably encrypted)
      ServerSspiAuthUser(fSecContext, fBoundUser);
      if KerberosUser <> nil then
        KerberosUser^ := fBoundUser;
      fBound := true;
      fBoundAs := lcbKerberos;
      fBoundKerberosAuthIdentify := AuthIdentify;
      if needencrypt then
        include(fFlags, fSecContextEncrypt);
      result := true;
    finally
      if not result then
        FreeSecContext(fSecContext);
    end;
  finally
    if Assigned(log) then
      log.Log(LOG_DEBUGERROR[not result],
        'BindSaslKerberos=% % % signseal=% as %', [BOOL_STR[result], fResultCode,
        ToText(Transmission)^, BOOL_STR[needencrypt], fBoundUser], self);
  end;
end;

function TLdapClient.Connected: boolean;
begin
  result := fSock.SockConnected;
end;

function TLdapClient.EnsureConnected(const context: ShortString): boolean;
begin
  result := (self <> nil) and
            (fSock.SockConnected or
             ((fBoundAs <> lcbNone) and
              Reconnect(context))); // try re-connect and re-bind if possible
end;

function TLdapClient.Transmission: TLdapClientTransmission;
begin
  if (self = nil) or
     not fSock.SockConnected then
    result := lctNone
  else if fSettings.Tls or
          (fSecContextEncrypt in fFlags) then
    result := lctEncrypted
  else
    result := lctPlain;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-unbind

function TLdapClient.Close: boolean;
begin
  result := true;
  fLog.Add.Log(sllTrace, 'Close', self);
  if fSock.SockConnected then
    try
      SendPacket(AsnTyped('', LDAP_ASN1_UNBIND_REQUEST)); // may raise exception
    except
      result := false;
    end;
  FreeAndNil(fSock);
  Reset({reconnect=}false);
  fFlags := [];
end;

procedure TLdapClient.Reset(reconnect: boolean);
begin
  fLog.Add.Log(sllTrace, 'Reset', self);
  if fSecContextEncrypt in fFlags then
    FreeSecContext(fSecContext);
  fSeq := 0;
  fBound := false; // fBoundAs should be kept as it is
  fBoundUser := '';
  if reconnect then
    fFlags := fFlags * [fRetrieveRootDseInfo, fRetrievedDefaultDNInfo]
  else
  begin
    fFlags := []; // from Close: full reset
    fRootDN := '';
    fDefaultDN := '';
    fConfigDN := '';
  end;
end;

function TLdapClient.DoBind(Mode: TLdapClientBound): boolean;
begin
  case Mode of
    lcbPlain:
      result := Bind;
    lcbDigest:
      result := BindSaslDigest(fBoundDigestAlgo);
    lcbKerberos:
      result := BindSaslKerberos(fBoundKerberosAuthIdentify);
  else
    result := false;
  end;
end;

function TLdapClient.Reconnect(const context: ShortString): boolean;
var
  log: ISynLog;
  step: PUtf8Char;
begin
  result := false;
  if (self = nil) or
     (fBoundAs = lcbNone) or
     (fSock = nil) then
    exit; // no server to reconnect
  fLog.EnterLocal(log, 'Reconnect from %', [context], self);
  // reset the client state and close any current socket
  Reset({reconnect=}true);
  fSock.Close;
  // re-create the client socket with previous valid TCP parameters
  if Assigned(log) then
    log.Log(sllTrace, 'Reconnect: OpenBind(%)', [fSettings.TargetUri], self);
  step := 'OpenBind';
  try
    fSock.OpenBind(fSettings.TargetHost, fSettings.TargetPort,
                   {bind=}false, fSettings.Tls);
    // re-bound with the previous mean of authentication
    step := 'DoBind';
    result := DoBind(fBoundAs);
  except
    on E: Exception do
      SetUnknownError('Reconnect: % raised %', [step, E]);
  end;
end;

// https://ldap.com/ldapv3-wire-protocol-reference-extended
function TLdapClient.Extended(const Oid: RawUtf8; const Value: TAsnObject;
  RespName: PRawUtf8; RespValue: PAsnObject): boolean;
var
  query, decoded, v: TAsnObject;
  pos: integer;
begin
  result := false;
  if EnsureConnected('Extended') then
  try
    query := AsnTyped(Oid, ASN1_CTX0);
    if Value <> '' then
      AsnAdd(query, AsnTyped(Value, ASN1_CTX1));
    decoded := SendAndReceive(AsnTyped(query, LDAP_ASN1_EXT_REQUEST));
    result := fResultCode = LDAP_RES_SUCCESS;
    if not result then
      exit;
    // https://www.rfc-editor.org/rfc/rfc2251#section-4.12
    pos := 1;
    while pos < length(decoded) do
      case AsnNext(pos, decoded, @v) of
        ASN1_CTX10:
          if RespName <> nil then
            RespName^ := v;
        ASN1_CTX11:
          if RespValue <> nil then
            RespValue^ := v;
      end;
  finally
    fLog.Add.Log(LOG_TRACEERROR[not result], 'Extended(%)=% % % %',
      [Oid, BOOL_STR[result], fResultCode, fResultString, v], self);
  end;
end;

function TLdapClient.ExtWhoAmI: RawUtf8;
begin
  if not Extended(ASN1_OID_WHOAMI, '', nil, @result) then
    result := '';
end;


// **** TLdapClient read methods

procedure TLdapClient.SearchBegin(PageSize: integer);
begin
  fSearchCookie := '';
  AddInteger(fSearchBeginBak, fSearchBeginCount, fSearchPageSize);
  fSearchPageSize := PageSize;
  fSearchPageCount := 0;
end;

procedure TLdapClient.SearchEnd;
begin
  if fSearchBeginCount = 0 then
    ELdap.RaiseUtf8('Unexpected %.SearchEnd with no SearchBegin', [self]);
  fSearchCookie := '';
  dec(fSearchBeginCount);
  fSearchPageSize := fSearchBeginBak[fSearchBeginCount]; // restore
end;

// https://ldap.com/ldapv3-wire-protocol-reference-search

function TLdapClient.Search(const BaseDN: RawUtf8; TypesOnly: boolean;
  const Filter: RawUtf8; const Attributes: array of RawUtf8): boolean;
var
  s, resp, packet, controls: TAsnObject;
  start, stop, bytesIn, bytesOut: Int64;
  u: RawUtf8;
  x, n, seqend: integer;
  r: TLdapResult;
  a: TLdapAttribute;
begin
  result := false;
  if not EnsureConnected('Search') then
    exit;
  // compute the main request
  QueryPerformanceMicroSeconds(start);
  fSearchResult.Clear;
  fReferals.Clear;
  s := RawLdapSearch(BaseDN, TypesOnly, Filter, Attributes, fSearchScope,
    fSearchAliases, fSearchSizeLimit, fSearchTimeLimit);
  // append optional control extensions
  if fSearchPageSize > 0 then // https://www.rfc-editor.org/rfc/rfc2696
    controls :=
      Asn(ASN1_SEQ, [
         AsnOctStr(LDAP_PAGED_RESULT_OID_STRING), // controlType: pagedresultsControl
         ASN1_BOOLEAN_VALUE[false],               // criticality: false
         AsnOctStr(Asn(ASN1_SEQ, [
               Asn(fSearchPageSize),
               AsnOctStr(fSearchCookie)
             ]))
      ]);
  if fSearchSDFlags <> [] then
    Append(controls,
      Asn(ASN1_SEQ, [
         AsnOctStr(LDAP_SERVER_SD_FLAGS_OID), // controlType: SDFlagsRequestValue
         ASN1_BOOLEAN_VALUE[false],           // criticality: false
         AsnOctStr(Asn(ASN1_SEQ, [
               Asn(byte(fSearchSDFlags))
             ]))
      ]));
  if controls <> '' then
    Append(s, AsnTyped(controls, LDAP_ASN1_CONTROLS));
  try
    // actually send the request
    bytesIn := fSock.BytesIn;
    bytesOut := fSock.BytesOut;
    SendPacket(s); // raise exception on failure
    // receive and parse the response
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
            AsnNext(n, resp, @r.fObjectName);
            if AsnNext(n, resp) = ASN1_SEQ then
            begin
              while n < length(resp) do
              begin
                if AsnNext(n, resp, nil, @seqend) = ASN1_SEQ then
                begin
                  AsnNext(n, resp, @u);
                  a := r.Attributes.Add(u);
                  if AsnNext(n, resp) = ASN1_SETOF then
                  begin
                    while n < seqend do
                    begin
                      AsnNext(n, resp, @u);
                      a.Add(u, aoAlwaysFast); // with eventual AfterAdd
                    end;
                    a.AfterAdd; // allow "for a in attr.List do"
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
        if s = LDAP_PAGED_RESULT_OID_STRING then
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
    if result and
       (fSearchRange <> nil) then // within SearchRangeBegin .. SearchRangeEnd
      fSearchRange.ExtractPagedAttributes(fSearchResult);
    if fSearchBeginCount <> 0 then
      inc(fSearchPageCount, fSearchResult.Count);
    QueryPerformanceMicroSeconds(stop);
    fSearchResult.fMicroSec := stop - start;
    fSearchResult.fIn := fSock.BytesIn - bytesIn;
    fSearchResult.fOut := fSock.BytesOut - bytesOut;
  finally
    if Assigned(fLog) then
      if result then
        fLog.Add.Log(sllTrace, 'Search dn="%" filter="%" %',
          [BaseDN, Filter, fSearchResult], self)
      else
        fLog.Add.Log(sllError, 'Search dn="%" filter="%" failed as %',
          [BaseDN, Filter, fResultString], self);
  end;
end;

function TLdapClient.SearchFmt(const BaseDN: RawUtf8; TypesOnly: boolean;
  const FilterFmt: RawUtf8; const FilterArgs: array of const;
  const Attributes: array of RawUtf8): boolean;
begin
  result := Search(BaseDN, TypesOnly,
    FormatUtf8(FilterFmt, FilterArgs), Attributes);
end;

function TLdapClient.SearchFirst(const BaseDN, Filter: RawUtf8;
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
  bak: TLdapSearchScope;
begin
  bak := SearchScope;
  try
    SearchScope := Scope;
    result := SearchFirst(ObjectDN, Filter, Attributes);
  finally
    SearchScope := bak;
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

function TLdapClient.SearchObject(Attribute: TLdapAttributeType;
  const ObjectDN, Filter: RawUtf8; Scope: TLdapSearchScope): TLdapAttribute;
begin
  if Attribute = atUndefined then
    result := nil
  else
    result := SearchObject(ObjectDN, Filter, AttrTypeName[Attribute], Scope);
end;

function TLdapClient.SearchMissing(const ObjectName: RawUtf8;
  Attribute: TLdapAttribute): integer;
var
  atts: RawUtf8;
  new: TLdapAttribute;
begin
  result := Attribute.Count; // returns the page size
  if result = 0 then
    exit;
  repeat
    // request '###;range=...' paged attribute values
    FormatUtf8('%;range=%-%', [Attribute.AttributeName,
      Attribute.Count, Attribute.Count + result - 1], atts);
    if not Search(ObjectName, false, '', atts) then
      break;
    inc(fSearchRange.fMicroSec, fSearchResult.fMicroSec);
    inc(fSearchRange.fIn, fSearchResult.fIn);
    inc(fSearchRange.fOut, fSearchResult.fOut);
    new := fSearchResult.Find(ObjectName).
                         Find(Attribute.AttributeName, {ignorerange=}true);
    if new = nil then
      break;
    Attribute.AddFrom(new);
    if new.AttributeName[length(new.AttributeName)] = '*' then
      break; // don't put in "until" to circumvent Delphi compiler bug
  until false;
end;

procedure TLdapClient.SearchMissingAttributes(var Result: TDocVariantData;
  Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8);
var
  r, a: PtrInt;
  res: TLdapResult;
  att: TLdapAttribute;
  bak: TLdapSearchScope;
  v: PDocVariantData;
begin
  // consolidate into TDocVariantData
  if (ObjectAttributeField = '') or
     (fSearchRange = nil) or
     (fSearchRange.Count = 0) then
    exit;
  fSearchRange.fMicroSec := fSearchResult.fMicroSec;
  fSearchRange.fIn  := fSearchResult.fIn;
  fSearchRange.fOut := fSearchResult.fOut;
  bak := fSearchScope;
  try
    fSearchScope := lssBaseObject;
    for r := 0 to fSearchRange.Count - 1 do
    begin
      res := fSearchRange.Items[r];
      for a := 0 to res.Attributes.Count - 1 do
      begin
        // request '###;range=...' paged attribute values
        att := res.Attributes.Items[a];
        if SearchMissing(res.ObjectName, att) = 0 then
          continue;
        // merge with existing TDocVariant resultset
        v := res.AppendToLocate(Result, Options);
        if ObjectAttributeField <> '*' then
          v := v^.O_[ObjectAttributeField];
        v := v^.A_[att.AttributeName];
        v^.Clear;
        att.SetNewVariant(PVariant(v)^, Options, nil, nil);
      end;
    end;
  finally
    fSearchScope := bak;
    fSearchResult.fMicroSec := fSearchRange.fMicroSec;
    fSearchResult.fIn := fSearchRange.fIn;
    fSearchResult.fOut := fSearchRange.fOut;
    if Assigned(fLog) then
      fLog.Add.Log(sllTrace, 'SearchMissingAttributes: %', [fSearchResult], self);
  end;
end;

procedure TLdapClient.SearchMissingAttributes;
var
  r, a: PtrInt;
  res: TLdapResult;
  att: TLdapAttribute;
  bak: TLdapSearchScope;
  all: TLdapResultList;
begin
  if (fSearchRange = nil) or
     (fSearchRange.Count = 0) then
    exit;
  all := fSearchResult; // consolidate into self
  fSearchRange.fMicroSec := all.fMicroSec;
  fSearchRange.fIn := all.fIn;
  fSearchRange.fOut := all.fOut;
  fSearchResult := TLdapResultList.Create; // use a new temporary instance
  bak := fSearchScope;
  try
    fSearchScope := lssBaseObject;
    for r := 0 to fSearchRange.Count - 1 do
    begin
      res := fSearchRange.Items[r];
      for a := 0 to res.Attributes.Count - 1 do
      begin
        att := res.Attributes.Items[a];
        if SearchMissing(res.ObjectName, att) <> 0 then
          all.FindOrAdd(res.ObjectName, att.AttributeName).AddFrom(att);
      end;
    end;
  finally
    fSearchScope := bak;
    all.fMicroSec := fSearchRange.fMicroSec;
    all.fIn := fSearchRange.fIn;
    all.fOut := fSearchRange.fOut;
    fSearchResult.Free;
    fSearchResult := all; // back to consolidated results
  end;
end;

procedure TLdapClient.SearchRangeBegin;
begin
  if fSearchRange <> nil then
    ELdap.RaiseUtf8('%.SearchRangeBegin: missing SearchRangeEnd', [self]);
  fSearchRange := TLdapResultList.Create;
end;

procedure TLdapClient.SearchRangeEnd;
begin
  if fSearchRange = nil then
    ELdap.RaiseUtf8('%.SearchRangeEnd: missing SearchRangeBegin', [self]);
  try
    SearchMissingAttributes; // consolidate into self
  finally
    FreeAndNil(fSearchRange);
  end;
end;

procedure TLdapClient.SearchRangeEnd(var Result: TDocVariantData;
  Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8);
begin
  if fSearchRange = nil then
    ELdap.RaiseUtf8('%.SearchRangeEnd: missing SearchRangeBegin', [self]);
  try
    SearchMissingAttributes(Result, Options, ObjectAttributeField);
  finally
    FreeAndNil(fSearchRange);
  end;
end;

function TLdapClient.SearchAllDocRaw(out Dest: TDocVariantData;
  const BaseDN, Filter: RawUtf8; const Attributes: array of RawUtf8;
  Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8;
  MaxCount, PerPage: integer): boolean;
var
  n, recv: integer;
  dom: PSid;
  ilog: ISynLog;
begin
  // setup context and resultset
  if Assigned(fLog) then
    fLog.EnterLocal(ilog, 'SearchAllDocRaw max=% perpage=%',
      [MaxCount, PerPage], self);
  dom := nil;
  if not (roNoSddlDomainRid in Options) then
    dom := pointer(DomainSid); // RID resolution from cached Domain SID
  Dest.Init(mFast, dvObject);
  n := 0;
  recv := 0;
  // check for "paging attributes" auto-range results
  if (roAutoRange in Options) and
     (ObjectAttributeField <> '') and
     not (roTypesOnly in Options) then
    SearchRangeBegin;
  if PerPage <= 100 then
    PerPage := 100;
  SearchBegin(PerPage); // force pagination for the loop below
  try
    // retrieve all result pages
    repeat
      if not Search(BaseDN, roTypesOnly in Options, Filter, Attributes) then
        break;
      fSearchResult.AppendTo(Dest, Options, ObjectAttributeField, dom);
      inc(n, fSearchResult.Count);
      inc(recv, fSearchResult.Recv);
    until (SearchCookie = '') or
          ((MaxCount > 0) and
           (n > MaxCount));
    result := fResultCode = LDAP_RES_SUCCESS;
  finally
    SearchEnd;
    // additional requests to fill any "paging attributes" auto-range results
    if fSearchRange <> nil then
      SearchRangeEnd(Dest, Options, ObjectAttributeField); // as TDocVariant
  end;
  if Assigned(ilog) then
    ilog.Log(sllDebug, 'SearchAllDocRaw=% count=% recv=%',
      [BOOL_STR[result], n, KBNoSpace(recv)], self);
  // eventually sort by field names (if specified)
  if roSortByName in Options then
    Dest.SortByName(nil, {reverse=}false, {nested=}true);
end;

function TLdapClient.SearchAllRaw(const BaseDN: RawUtf8;
  const Filter: RawUtf8; const Attributes: array of RawUtf8;
  Options: TLdapResultOptions; const ObjectAttributeField: RawUtf8;
  MaxCount: integer): variant;
begin
  VarClear(result);
  SearchAllDocRaw(TDocVariantData(result), DefaultDN(BaseDN), Filter,
    Attributes, Options, ObjectAttributeField, MaxCount);
end;

function TLdapClient.Search(const Attributes: TLdapAttributeTypes;
  const Filter, BaseDN: RawUtf8; TypesOnly: boolean): boolean;
begin
  result := Search(DefaultDN(BaseDN), TypesOnly, Filter, ToText(Attributes));
end;

function TLdapClient.SearchFmt(const Attributes: TLdapAttributeTypes;
  const FilterFmt: RawUtf8; const FilterArgs: array of const;
  const BaseDN: RawUtf8; TypesOnly: boolean): boolean;
begin
  result := Search(Attributes, FormatUtf8(FilterFmt, FilterArgs), BaseDN, TypesOnly);
end;

function TLdapClient.SearchFirst(const Attributes: TLdapAttributeTypes;
  const Filter, BaseDN: RawUtf8): TLdapResult;
begin
  result := SearchFirst(DefaultDN(BaseDN), Filter, ToText(Attributes));
end;

function TLdapClient.SearchFirstFmt(const Attributes: TLdapAttributeTypes;
  const FilterFmt: RawUtf8; const FilterArgs: array of const;
  const BaseDN: RawUtf8): TLdapResult;
begin
  result := SearchFirst(Attributes, FormatUtf8(FilterFmt, FilterArgs), BaseDN);
end;

function TLdapClient.SearchObject(const Attributes: TLdapAttributeTypes;
  const ObjectDN, Filter: RawUtf8; Scope: TLdapSearchScope): TLdapResult;
begin
  result := SearchObject(ObjectDN, Filter, ToText(Attributes), Scope);
end;

function TLdapClient.SearchAll(const Attributes: TLdapAttributeTypes;
  const Filter: RawUtf8; Options: TLdapResultOptions;
  const ObjectAttributeField, BaseDN: RawUtf8;
  MaxCount: integer): variant;
begin
  VarClear(result);
  SearchAllDoc(TDocVariantData(result), Attributes, Filter, Options,
    ObjectAttributeField, BaseDN, MaxCount);
end;

function TLdapClient.SearchAllDoc(out Dest: TDocVariantData;
  const Attributes: TLdapAttributeTypes; const Filter: RawUtf8;
  Options: TLdapResultOptions; const ObjectAttributeField, BaseDN: RawUtf8;
  MaxCount: integer): boolean;
begin
  result := SearchAllDocRaw(Dest, DefaultDN(BaseDN), Filter, ToText(Attributes),
              Options, ObjectAttributeField, MaxCount);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-compare

function TLdapClient.Compare(const Obj, AttrName, AttrValue: RawUtf8): boolean;
begin
  result := false;
  if not EnsureConnected('Compare') then
    exit;
  SendAndReceive(Asn(LDAP_ASN1_COMPARE_REQUEST, [
                   AsnOctStr(obj),
                   Asn(ASN1_SEQ, [
                     AsnOctStr(AttrName),
                     AsnOctStr(AttrValue)
                   ])
                 ]));
  result := fResultCode = LDAP_RES_COMPARE_TRUE;
  if Assigned(fLog) then
    fLog.Add.Log(LOG_TRACEERROR[not (fResultCode in LDAP_RES_NOERROR)],
      'Compare("%",%,%)=% % %', [Obj, AttrName, AttrValue,
      BOOL_STR[result], fResultCode, fResultString], self);
end;


// **** TLdapClient write methods

// https://ldap.com/ldapv3-wire-protocol-reference-add

function TLdapClient.Add(const Obj: RawUtf8; Value: TLdapAttributeList): boolean;
var
  query: TAsnObject;
  i: PtrInt;
begin
  result := false;
  if (Value = nil) or
     not EnsureConnected('Add') then
    exit;
  for i := 0 to Value.Count - 1 do
    Append(query, Value.Items[i].ExportToAsnSeq);
  SendAndReceive(Asn(LDAP_ASN1_ADD_REQUEST, [
                   AsnOctStr(Obj),
                   AsnSeq(query)]));
  result := fResultCode = LDAP_RES_SUCCESS;
  if Assigned(fLog) then
    fLog.Add.Log(LOG_DEBUGERROR[not result], 'Add(%)=% % %',
      [Obj, BOOL_STR[result], fResultCode, fResultString], self);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-modify

function TLdapClient.Modify(const Obj: RawUtf8;
  const Modifications: array of TAsnObject): boolean;
begin
  result := false;
  if (high(Modifications) < 0) or
     ((high(Modifications) = 0) and
      (Modifications[0] = '')) or
     not EnsureConnected('Modify') then
    exit;
  SendAndReceive(Asn(LDAP_ASN1_MODIFY_REQUEST, [
                   AsnOctStr(Obj),       // the DN of the entry to modify
                   AsnSeq(Modifications) // sequence of modifications
                 ]));
  result := fResultCode = LDAP_RES_SUCCESS;
  if Assigned(fLog) then
    if result then
      fLog.Add.Log(sllDebug, 'Modify(%) n=%', [Obj, length(Modifications)], self)
    else
      fLog.Add.Log(sllError, 'Modify(%)=% % %',
        [Obj, BOOL_STR[result], fResultCode, fResultString], self);
end;

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  AttrType: TLdapAttributeType; const AttrValue: RawByteString): boolean;
begin
  result := (AttrType <> atUndefined) and
            Modify(Obj, [Modifier(Op, AttrTypeName[AttrType], AttrValue)]);
end;

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  const AttrName: RawUtf8; const AttrValue: RawByteString): boolean;
begin
  result := (AttrName <> '') and
            Modify(Obj, [Modifier(Op, AttrName, AttrValue)]);
end;

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  const Types: array of TLdapAttributeType; const Values: array of const): boolean;
begin
  result := (length(Types) <> 0) and
            (length(Types) = length(Values)) and
            Modify(Obj, [Modifier(Op, Types, Values)]);
end;

function TLdapClient.Modify(const Obj: RawUtf8; Op: TLdapModifyOp;
  Attribute: TLdapAttribute): boolean;
begin
  result := Modify(Obj, [Modifier(Op, Attribute.ExportToAsnSeq)]);
end;


// https://ldap.com/ldapv3-wire-protocol-reference-modify-dn

function TLdapClient.ModifyDN(const Obj, NewRdn, NewSuperior: RawUtf8;
  DeleteOldRdn: boolean): boolean;
var
  query: TAsnObject;
begin
  result := false;
  if not EnsureConnected('ModifyDN') then
    exit;
  query := AsnOctStr(Obj);
  Append(query, AsnOctStr(NewRdn), ASN1_BOOLEAN_VALUE[DeleteOldRdn]);
  if NewSuperior <> '' then
    AsnAdd(query, AsnTyped(NewSuperior, ASN1_CTX0));
  SendAndReceive(AsnTyped(query, LDAP_ASN1_MODIFYDN_REQUEST));
  result := fResultCode = LDAP_RES_SUCCESS;
  if Assigned(fLog) then
    fLog.Add.Log(LOG_DEBUGERROR[not result], 'ModifyDN(%)=% % %',
      [Obj, BOOL_STR[result], fResultCode, fResultString], self);
end;

// https://ldap.com/ldapv3-wire-protocol-reference-delete

function TLdapClient.Delete(const Obj: RawUtf8; DeleteChildren: boolean): boolean;
var
  bak: TLdapSearchScope;
  children: TRawUtf8DynArray;
  i: PtrInt;
begin
  result := false;
  if not EnsureConnected('Delete') then
    exit;
  bak := SearchScope;
  SendAndReceive(AsnTyped(Obj, LDAP_ASN1_DEL_REQUEST));
  if (fResultCode = LDAP_RES_NOT_ALLOWED_ON_NON_LEAF) and
     DeleteChildren then
    // Obj had children and DeleteChildren is True
    try
      SearchScope := lssSingleLevel;
      Search([], {filter=}'', {dn=}Obj);
      children := fSearchResult.ObjectNames;
      for i := 0 to high(children) do
        if not Delete(children[i], {DeleteChildren=}true) then
          break; // stop on error
      if fResultCode = LDAP_RES_SUCCESS then
        // retry Obj deletion after children have been successfully removed
        SendAndReceive(AsnTyped(Obj, LDAP_ASN1_DEL_REQUEST));
    finally
      SearchScope := bak;
    end;
  result := fResultCode = LDAP_RES_SUCCESS;
  if Assigned(fLog) then
    fLog.Add.Log(LOG_DEBUGERROR[not result], 'Delete(%)=% % %',
      [Obj, BOOL_STR[result], fResultCode, fResultString], self);
end;


// **** TLdapClient high-level Computer methods

const
  // TLdapObject attributes, common to TLdapComputer, TLdapGroup and TLdapUser
  LDAPOBJECT_ATTR = [
    atSAMAccountName,
    atDistinguishedName,
    atName,
    atCommonName,
    atDescription,
    atObjectSid,
    atObjectGuid,
    atWhenCreated,
    atWhenChanged];
  // TLdapComputer attributes
  LDAPMACHINE_ATTR = LDAPOBJECT_ATTR + [
    atPwdLastSet,
    atLastLogonTimestamp,
    atMcsAdmPwdExpirationTime,
    atUserAccountControl,
    atPrimaryGroupID,
    atLogonCount,
    atBadPwdCount,
    atDnsHostName,
    atOperatingSystem,
    atOperatingSystemVersion,
    atServicePrincipalName];
  // TLdapGroup attributes
  LDAPGROUP_ATTR = LDAPOBJECT_ATTR + [
    atGroupType];
  // TLdapUser attributes
  LDAPUSER_ATTR  = LDAPOBJECT_ATTR + [
    atUserPrincipalName,
    atDisplayName,
    atMail,
    atPwdLastSet,
    atLastLogon,
    atUserAccountControl,
    atPrimaryGroupID];

procedure TLdapClient.GetAllValues(Filter: TObjectFilter; Flags, noFlags: integer;
  const BaseDN, CustomFilter, Match: RawUtf8; Attribute: TLdapAttributeType;
  out Res: TRawUtf8DynArray; ObjectNames: PRawUtf8DynArray);
var
  f, flagsName: RawUtf8;
begin
  if Filter in GROUP_FILTER then
    flagsName := 'groupType'
  else
    flagsName := 'userAccountControl'; // users and computers
  FlagsFilterInteger(flagsName, Flags, noFlags, f);
  if Match <> '' then // allow * wildchar in Match - but escape others
    f := FormatUtf8('%(%=%)',
      [f, AttrTypeName[Attribute], LdapEscape(Match, {keep*=}true)]);
  f := FormatUtf8('(&%%%)', [OBJECT_FILTER[Filter], f, CustomFilter]);
  SearchBegin(1000); // force pagination for the loop below
  try
    repeat
      if Search([Attribute], f, BaseDN) and
         (SearchResult.Count > 0) then
        AddRawUtf8(Res, SearchResult.ObjectAttributes(Attribute, ObjectNames));
    until SearchCookie = '';
  finally
    SearchEnd;
  end;
end;

function TLdapClient.GetComputers(FilterUac, UnFilterUac: TUserAccountControls;
  const Match, CustomFilter, BaseDN: RawUtf8; ObjectNames: PRawUtf8DynArray;
  Attribute: TLdapAttributeType): TRawUtf8DynArray;
begin
  GetAllValues(ofComputers,
    UserAccountControlsValue(FilterUac), UserAccountControlsValue(UnFilterUac),
    BaseDN, CustomFilter, Match, Attribute, result, ObjectNames);
end;

function TLdapClient.GetComputerInfo(
  const AccountName, DistinguishedName: RawUtf8; out Info: TLdapComputer;
  const BaseDN: RawUtf8; const CustomAttributes: TRawUtf8DynArray;
  const CustomTypes: TLdapAttributeTypes): boolean;
var
  attr: TRawUtf8DynArray;
begin
  RecordZero(@Info, TypeInfo(TLdapComputer));
  attr := ToText(LDAPMACHINE_ATTR + CustomTypes);
  AddRawUtf8(attr, CustomAttributes);
  result := ((AccountName <> '') or
             (DistinguishedName <> '')) and
            Search(DefaultDN(BaseDN), false, ObjectFilter(
              ofComputers, AccountName, DistinguishedName), attr) and
            (SearchResult.Count = 1);
  if result then
    Info.Fill(SearchResult.Items[0].Attributes,
      CustomAttributes, CustomTypes);
end;

function TLdapClient.AddComputer(const ComputerParentDN, ComputerName: RawUtf8;
  out ErrorMessage: RawUtf8; const Password: SpiUtf8; DeleteIfPresent: boolean;
  UserAccount: TUserAccountControls): boolean;
var
  cSafe, cDn, cSam: RawUtf8;
  attrs: TLdapAttributeList;
  cExisting: TLdapResult;
begin
  result := false;
  if not EnsureConnected('AddComputer') or
     not LdapEscapeName(ComputerName, cSafe) then
    exit;
  cDn := NormalizeDN(Join(['CN=', cSafe, ',', ComputerParentDN]));
  cSam := Join([UpperCase(cSafe), '$']); // traditional upper with ending $
  // Search Computer object in the domain
  cExisting := SearchFirstFmt([atSAMAccountName], '(sAMAccountName=%)', [cSam]);
  // If the search failed, we exit with the error message
  if ResultCode <> LDAP_RES_SUCCESS then
  begin
    FormatUtf8('AddComputer.Search failed: %', [fResultString], ErrorMessage);
    exit;
  end;
  // Computer with the same sAMAccountName is already existing
  if Assigned(cExisting) then
  begin
    // We don't want to delete it
    if not DeleteIfPresent then
    begin
      ErrorMessage := 'Computer is already present';
      exit;
    end;
    result := Delete(cExisting.ObjectName);
    // Unable to delete the computer (probably insufficient access rights)
    if not result then
    begin
      FormatUtf8('AddComputer.Delete failed: %', [fResultString], ErrorMessage);
      exit;
    end;
  end;
  // Create the new computer entry
  attrs := TLdapAttributeList.Create(
    [atObjectClass, atCommonName, atName, atSAMAccountName],
    ['computer',    cSafe,        cSafe,  cSam]);
  try
    // attrs.AccountType should not be set, because it is defined by the AD
    attrs.UserAccountControl := UserAccount;
    if Password <> '' then
      attrs.AddUnicodePwd(Password);
    result := Add(cDn, attrs);
    if not result then
      FormatUtf8('AddComputer.Add failed: %', [fResultString], ErrorMessage);
  finally
    attrs.Free;
  end;
end;


// **** TLdapClient high-level User/Group methods

function TLdapClient.GetGroups(FilterUac, UnFilterUac: TGroupTypes;
  const Match, CustomFilter, BaseDN: RawUtf8; ObjectNames: PRawUtf8DynArray;
  Attribute: TLdapAttributeType): TRawUtf8DynArray;
begin
  GetAllValues(ofGroups,
    GroupTypesValue(FilterUac), GroupTypesValue(UnFilterUac),
    BaseDN, CustomFilter, Match, Attribute, result, ObjectNames);
end;

function TLdapClient.GetUsers(FilterUac, UnFilterUac: TUserAccountControls;
  const Match, CustomFilter, BaseDN: RawUtf8; ObjectNames: PRawUtf8DynArray;
  Attribute: TLdapAttributeType): TRawUtf8DynArray;
begin
  GetAllValues(ofUsers,
    UserAccountControlsValue(FilterUac), UserAccountControlsValue(UnFilterUac),
    BaseDN, CustomFilter, Match, Attribute, result, ObjectNames);
end;

function TLdapClient.GetGroupInfo(
  const AccountName, DistinguishedName: RawUtf8; out Info: TLdapGroup;
  const BaseDN: RawUtf8; WithMember: boolean;
  const CustomAttributes: TRawUtf8DynArray;
  const CustomTypes: TLdapAttributeTypes): boolean;
var
  attr: TRawUtf8DynArray;
  attrs: TLdapAttributeTypes;
begin
  RecordZero(@Info, TypeInfo(TLdapGroup));
  result := false;
  if (AccountName = '') and
     (DistinguishedName = '') then
    exit;
  attrs := LDAPGROUP_ATTR + CustomTypes;
  if WithMember then
    include(attrs, atMember);
  attr := ToText(attrs);
  AddRawUtf8(attr, CustomAttributes);
  if WithMember then
    SearchRangeBegin; // support 'member;range=0..1499' pagined attributes
  try
    result := Search(DefaultDN(BaseDN), false, ObjectFilter(
                ofGroups, AccountName, DistinguishedName), attr) and
              (SearchResult.Count = 1);
  finally
    if WithMember then
      SearchRangeEnd;
  end;
  if result then
    Info.Fill(SearchResult.Items[0].Attributes, WithMember,
      CustomAttributes, CustomTypes);
end;

function TLdapClient.GetGroupDN(
  const AccountName, BaseDN, CustomFilter: RawUtf8): RawUtf8;
begin
  if (AccountName <> '') and
     Search([atDistinguishedName], ObjectFilter(
       ofGroups, AccountName, '', '', CustomFilter), BaseDN) and
     (SearchResult.Count = 1) then
    result := SearchResult.Items[0][atDistinguishedName]
  else
    result := '';
end;

function TLdapClient.GetGroupPrimaryID(
  const AccountName, DistinguishedName: RawUtf8;
  out PrimaryGroupID: cardinal; const BaseDN, CustomFilter: RawUtf8): boolean;
var
  last: RawUtf8;
begin
  result := false;
  if ((AccountName <> '') or
      (DistinguishedName <> '')) and
     Search([atObjectSid], ObjectFilter(
       ofGroups, AccountName, DistinguishedName, '', CustomFilter), BaseDN) and
     (SearchResult.Count = 1) then
  begin
    last := SplitRight(SearchResult.Items[0][atObjectSid], '-');
    result := (last <> '') and
              ToCardinal(last, PrimaryGroupID);
  end;
end;

function TLdapClient.GetUserInfo(
  const AccountName, DistinguishedName, UserPrincipalName: RawUtf8;
  out Info: TLdapUser; const BaseDN: RawUtf8; WithMemberOf: boolean;
  const CustomAttributes: TRawUtf8DynArray;
  const CustomTypes: TLdapAttributeTypes): boolean;
var
  attr: TRawUtf8DynArray;
  attrs: TLdapAttributeTypes;
begin
  RecordZero(@Info, TypeInfo(TLdapUser));
  attrs := LDAPUSER_ATTR + CustomTypes;
  if WithMemberOf then
    include(attrs, atMemberOf);
  attr := ToText(attrs);
  AddRawUtf8(attr, CustomAttributes);
  result := ((AccountName <> '') or
             (DistinguishedName <> '') or
             (UserPrincipalName <> '')) and
            Search(DefaultDN(BaseDN), false, ObjectFilter(ofUsers,
              AccountName, DistinguishedName, UserPrincipalName), attr) and
            (SearchResult.Count = 1);
  if result then
    Info.Fill(SearchResult.Items[0].Attributes,
      WithMemberOf, CustomAttributes, CustomTypes);
end;

function TLdapClient.GetUserDN(
  const AccountName, UserPrincipalName, BaseDN, CustomFilter: RawUtf8;
  PrimaryGroupID: PCardinal; ObjectSid: PRawUtf8; ObjectKind: TObjectFilter): RawUtf8;
begin
  if ((AccountName <> '') or
      (UserPrincipalName <> '')) and
     Search([atDistinguishedName, atPrimaryGroupID, atObjectSid], ObjectFilter(
       ObjectKind, AccountName, '', UserPrincipalName, CustomFilter), BaseDN) and
     (SearchResult.Count = 1) then
    with SearchResult.Items[0].Attributes do
    begin
      result := Get(atDistinguishedName);
      if PrimaryGroupID <> nil then
        ToCardinal(Get(atPrimaryGroupID), PrimaryGroupID^);
      if ObjectSid <> nil then
        ObjectSid^ := Get(atObjectSid);
    end
    else
      result := '';
end;

function TLdapClient.GetIsMemberOf(
  const UserDN, GroupAN, GroupDN, CustomFilter: RawUtf8;
  Nested: boolean; const BaseDN: RawUtf8): boolean;
begin
  result := GetIsMemberOf(UserDN, CustomFilter, [GroupAN], [GroupDN], Nested, BaseDN);
end;

function TLdapClient.GetIsMemberOf(const UserDN, CustomFilter: RawUtf8;
  const GroupAN, GroupDN: array of RawUtf8; Nested: boolean;
  const BaseDN: RawUtf8; GroupsAN: PRawUtf8DynArray): boolean;
var
  user, grp, filter: RawUtf8;
  i, n: PtrInt;
begin
  result := false;
  if not LdapIsValidDistinguishedName(UserDN) then
    exit;
  user := EscapeHex(UserDN, '\'); // RawLdapTranslateFilter() does UnEscapeHex()
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
  filter := FormatUtf8('(&%%%(member%=%))',
    [OBJECT_FILTER[ofGroups], filter, CustomFilter, NESTED_FLAG[Nested], user]);
  if Search([atSAMAccountName], filter, BaseDN) and
     (SearchResult.Count > 0) then
  begin
    if GroupsAN <> nil then
      GroupsAN^ := SearchResult.ObjectAttributes(atSAMAccountName);
    result := true;
  end;
end;

function TLdapClient.ModifyUserPassword(const UserDN: RawUtf8;
  const OldPassword, NewPassword: SpiUtf8): boolean;
var
  old, new: SpiUtf8;
begin
  result := false;
  if UserDN = '' then
    exit;
  if Transmission <> lctEncrypted then
    ELdap.RaiseUtf8('%.ModifyUserPassword requires encryption', [self]);
  try
    new := LdapUnicodePwd(NewPassword);
    if OldPassword <> '' then
    begin
      // normal users must specify old and new passwords
      old := LdapUnicodePwd(OldPassword);
      result := Modify(UserDN, [
                  Modifier(lmoDelete, atUnicodePwd, old),
                  Modifier(lmoAdd,    atUnicodePwd, new)]);
    end
    else
      // admin users can reset password without sending the old one
      result := Modify(UserDN, [
                  Modifier(lmoReplace, atUnicodePwd, new)]);
  finally
    FillZero(old); // anti-forensic
    FillZero(new);
  end;
end;

function TLdapClient.ExtModifyUserPassword(const UserDN: RawUtf8;
  const OldPassword, NewPassword: SpiUtf8): SpiUtf8;
var
  req, v: TAsnObject;
  pos: integer;
begin
  // the RFC states that ASN1_OID_PASSWDMODIFY supportedExtension SHOULD be
  // verified in server root DSE - but OpenLDAP does not have this list, nor seem
  // to actually implement this extension, and this OID is not listed by MSAD :(
  result := '';
  if UserDN = '' then
    exit;
  if Transmission <> lctEncrypted then
    ELdap.RaiseUtf8('%.ExtModifyUserPassword requires encryption', [self]);
  if BoundUser = '' then
    ELdap.RaiseUtf8('%.ExtModifyUserPassword cannot be anonymous', [self]);
  req := AsnTyped(UserDN, ASN1_CTX0);
  if OldPassword <> '' then
    Append(req, AsnTyped(OldPassword, ASN1_CTX1));
  if NewPassword <> '' then
    Append(req, AsnTyped(NewPassword, ASN1_CTX2));
  pos := 1;
  if Extended(ASN1_OID_PASSWDMODIFY, AsnSeq(req), nil, @v) then
    if NewPassword <> '' then
      result := NewPassword     // password supplied by the client
    else if AsnNext(pos, v) = ASN1_SEQ then
      AsnNext(pos, v, @result); // password generated by the server
end;


function ToText(mode: TLdapClientBound): PShortString;
begin
  result := GetEnumName(TypeInfo(TLdapClientBound), ord(mode));
end;

function ToText(lct: TLdapClientTransmission): PShortString;
begin
  result := GetEnumName(TypeInfo(TLdapClientTransmission), ord(lct));
end;



{ **************** Dedicated TLdapCheckMember Class }

{ TLdapCheckMember }

// proper group membership on a LDAP server is really a complex task
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
  fSearchFilter := ofUsers;
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
  pid: cardinal;
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
  if not fBound and
     (fLastConnectedTix32 = GetTickCount64 shr 12) then // retry every 4 seconds
    exit; // too soon to re-connect (quick exit outside of the lock)
  try
    fSafe.Lock;
    try
      // first check from cache
      tix := GetTickCount64; // lock may have taken some time
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
      if not (Connected and Bound) then
        if fLastConnectedTix32 <> tix shr 12 then
        begin
          fLastConnectedTix32 := tix shr 12; // retry every 4 seconds only
          if not Reconnect('Authorize') then
            exit;
        end
        else
          exit; // too soon to retry
      // call the LDAP server to actually check user membership
      pid := 0;
      userdn := GetUserDN(
        User, User, fUserBaseDN, fUserCustomFilter, @pid, nil, fSearchFilter);
      if userdn <> '' then
      begin
        if pid = 0 then
          primaryidndx := -1
        else
          primaryidndx := IntegerScanIndex(pointer(fGroupID), length(fGroupID), pid);
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
      if fCacheTimeoutSeconds <> 0 then
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
      fSock.Close; // close the socket, but will try to reconnect
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
  if GroupAN = nil then
    exit;
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
  if GroupDN = nil then
    exit;
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

procedure TLdapCheckMember.AllowGroups(const GroupAN, GroupDN: TRawUtf8DynArray);
begin
  fSafe.Lock;
  try
    if RawUtf8DynArraySame(GroupAN, fGroupAN, {caseinsens=}true) and
       RawUtf8DynArraySame(GroupDN, fGroupDN) then // nothing to change
      exit;
    // need to register the new groups
    AllowGroupClear;
    AllowGroupAN(GroupAN);
    AllowGroupDN(GroupDN);
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
      [aUser, ToText(result)^, MicroSecFrom(start)], self);
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
  if StartWithExact(aPassword, 'FILE:') then
    exit; // don't cheat with this server credentials :)
  InvalidateSecContext(client);
  try
    try
      if (aFullUserName = nil)
         {$ifdef OSWINDOWS} and (fGroupSid = nil) {$endif} then
        // simple aUser/aPassword credential check needs no server side
        // - see as reference mag_auth_basic() in NGINX's mod_auth_gssapi.c
        result := ClientSspiAuthWithPassword(client, 'onlypass',
                    aUser, aPassword, fKerberosSpn, dataout)
      else
      begin
        // more user information currently need a ServerSspiAuth() context
        InvalidateSecContext(server);
        try
          while ClientSspiAuthWithPassword(client, datain,
                  aUser, aPassword, fKerberosSpn, dataout) and
                ServerSspiAuth(server, dataout, datain) do ;
          if aFullUserName <> nil then
            ServerSspiAuthUser(server, aFullUserName^);
          {$ifdef OSWINDOWS}
          // on Windows, ensure this user is part of AllowGroupBySid()
          if (fGroupSid = nil) or
             ServerSspiAuthGroup(server, fGroupSid) then
          {$endif OSWINDOWS}
            result := true;
        finally
          FreeSecContext(server);
        end;
      end;
    finally
      FreeSecContext(client);
    end;
  except
    result := false;
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
  p: PUtf8Char;
begin
  for i := 0 to high(GroupSid) do
  begin
    p := pointer(GroupSid[i]);
    if TextToSid(p, sid) and
       (p^ = #0) then
      AddRawSid(fGroupSid, @sid);
  end;
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
      Append(u, '@', fRealm)
    else
      Append(u, '@', fLdapSettings.KerberosDN);
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
  InitializeUnit;

end.

