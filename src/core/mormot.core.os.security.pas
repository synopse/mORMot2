/// Framework Core Definitions of Operating System Security
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os.security;

{
  *****************************************************************************

  Cross-Platform Operating System Security Definitions
  - Security IDentifier (SID) Definitions
  - Security Descriptor Self-Relative Binary Structures
  - Access Control List (DACL/SACL) Definitions
  - Conditional ACE Expressions SDDL and Binary Support
  - Active Directory Definitions
  - Security Descriptor Definition Language (SDDL)
  - TSecurityDescriptor Wrapper Object
  - Windows API Specific Security Types and Functions

  Even if most of those security definitions comes from the Windows/AD world,
    our framework (re)implemented them in a cross-platform way.
  Most definitions below refers to the official Windows Open Specifications
    document, named [MS-DTYP] in comments below.
  This low-level unit only refers to mormot.core.base and mormot.core.os.

  *****************************************************************************

  MS-DTYP: https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp
  TODO: resources attributes - see [MS-DTYP] 2.4.10.1
}

interface

{$I ..\mormot.defines.inc}

uses
  {$ifdef OSWINDOWS}
  Windows, // for Windows API Specific Security Types and Functions below
  {$endif OSWINDOWS}
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os;


{ ****************** Security IDentifier (SID) Definitions }

type
  /// exception class raised by this unit
  EOSSecurity = class(ExceptionWithProps);

  /// custom binary buffer type used as convenient Windows SID storage
  // - mormot.crypt.secure will recognize this type and serialize its standard
  // text as a JSON string
  RawSid = type RawByteString;
  PRawSid = ^RawSid;

  /// a dynamic array of binary SID storage buffers
  RawSidDynArray = array of RawSid;

  /// Security IDentifier (SID) Authority, encoded as 48-bit binary
  // - see [MS-DTYP] 2.4.1 SID_IDENTIFIER_AUTHORITY
  TSidAuth = array[0..5] of byte;
  PSidAuth = ^TSidAuth;

  /// Security IDentifier (SID) binary format, as retrieved e.g. by Windows API
  // - this definition is not detailed on oldest Delphi, and not available on
  // POSIX, whereas it makes sense to also have it, e.g. for server process
  // - its maximum used length is 1032 bytes
  // - see [MS-DTYP] 2.4.2 SID
  TSid = packed record
    Revision: byte;
    SubAuthorityCount: byte;
    IdentifierAuthority: TSidAuth;
    SubAuthority: array[byte] of cardinal;
  end;
  PSid = ^TSid;
  PSids = array of PSid;

const
  // some internal constants used for proper inlining (as required by Delphi)
  SID_MINLEN = SizeOf(TSidAuth) + 2; // = 8
  SID_RIDLEN = SID_MINLEN + 5 * SizeOf(cardinal); // sid.SubAuthority[4] = RID
  SID_DOMAINLEN = SID_RIDLEN - SizeOf(cardinal);  // exclude RID
  SID_REV32 = ord('S') + ord('-') shl 8 + ord('1') shl 16 + ord('-') shl 24;
  SID_DOM_MASKSID = $00000401; // Revision = 1 and SubAuthorityCount = 4
  SID_DOM_MASKRID = $00000501; // Revision = 1 and SubAuthorityCount = 5
  SID_DOM_MASKAUT = $05000000; // IdentifierAuthority = S-1-5-xxx

/// a wrapper around MemCmp() on two Security IDentifier binary buffers
// - will first compare by length, then by content
function SidCompare(a, b: PSid): integer;

/// compute the actual binary length of a Security IDentifier buffer, in bytes
function SidLength(sid: PSid): PtrInt;
  {$ifdef HASINLINE} inline; {$endif}

/// allocate a RawSid instance from a PSid raw handler
procedure ToRawSid(sid: PSid; out result: RawSid);

/// check if a RawSid binary buffer has the expected length of a valid SID
function IsValidRawSid(const sid: RawSid): boolean;

/// search within SID dynamic array for a given SID
function HasSid(const sids: PSids; sid: PSid): boolean;

/// search within SID dynamic array for a given dynamic array of SID buffers
function HasAnySid(const sids: PSids; const sid: RawSidDynArray): boolean;

/// append a SID buffer pointer to a dynamic array of SID buffers
procedure AddRawSid(var sids: RawSidDynArray; sid: PSid);

/// append a SID as text, following the standard representation
procedure SidAppendShort(sid: PSid; var s: ShortString);

/// convert a Security IDentifier as text, following the standard representation
procedure SidToTextShort(sid: PSid; var result: ShortString);
  {$ifdef FPC}inline;{$endif} // Delphi doesn't suppport inlining shortstring :(

/// convert a Security IDentifier as text, following the standard representation
function SidToText(sid: PSid): RawUtf8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a Security IDentifier as text, following the standard representation
// - this function is able to convert into itself, i.e. allows sid=pointer(text)
procedure SidToText(sid: PSid; var text: RawUtf8); overload;

/// convert several Security IDentifier as text dynamic array
function SidsToText(sids: PSids): TRawUtf8DynArray;

/// convert a Security IDentifier as text, following the standard representation
function RawSidToText(const sid: RawSid): RawUtf8;

/// parse a Security IDentifier text, following the standard representation
// - won't support hexadecimal 48-bit IdentifierAuthority, i.e. S-1-0x######-..
// - will parse the input text buffer in-place, and return the next position
function TextToSid(var P: PUtf8Char; out sid: TSid): boolean;

/// parse a Security IDentifier text, following the standard representation
function TextToRawSid(const text: RawUtf8): RawSid; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// parse a Security IDentifier text, following the standard representation
function TextToRawSid(const text: RawUtf8; out sid: RawSid): boolean; overload;

/// parse several Security IDentifier text, following the standard representation
function TextToRawSidArray(const text: array of RawUtf8; out sid: RawSidDynArray): boolean;

/// quickly check if a SID is in 'S-1-5-21-xx-xx-xx-RID' domain form
function SidIsDomain(s: PSid): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// decode a domain SID text into a generic binary RID value
// - returns true if Domain is '', or is in its 'S-1-5-21-xx-xx-xx' domain form
// - will also accepts any 'S-1-5-21-xx-xx-xx-yyy' form, e.g. the current user SID
// - if a domain SID, Dom binary buffer will contain a S-1-5-21-xx-xx-xx-0 value,
// ready to be used with KnownRidSid(), SidSameDomain(), SddlAppendSid(),
// SddlNextSid() or TSecurityDescriptor.AppendAsText functions
function TryDomainTextToSid(const Domain: RawUtf8; out Dom: RawSid): boolean;

/// quickly check if two binary SID buffers domain do overlap
// - sid should be a RID in S-1-5-21-xx-xx-xx-RID layout
// - dom should be a domain SID in S-1-5-21-xx-xx-xx[-RID] layout, typically
// from TryDomainTextToSid() output
function SidSameDomain(sid, dom: PSid): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// check if a RID is part of OldDomain, then change it into NewDomain
// - Sid should be in S-1-5-21-xx-xx-xx-RID layout
// - OldDomain/NewDomain should be in S-1-5-21-xx-xx-xx[-RID] layout
// - returns 0 if SID was not modified, or 1 if its domain part has been adjusted
function SidReplaceDomain(OldDomain, NewDomain: PSid; maxRid: cardinal;
  var Sid: RawSid): integer;

/// replace a any occurence of OldSid[] in Sid value by NewSid[]
// - length(OldSid) should equal length(NewSid)
function SidReplaceAny(const OldSid, NewSid: RawSidDynArray;
  var Sid: RawSid): integer; overload;

/// replace a any occurence of OldSid[] in Sid value by NewSid[]
// - each length(OldSid[]) should equal length(NewSid[]) and also equal SidLen
function SidReplaceAny(const OldSid, NewSid: RawSidDynArray;
  var Sid: TSid; SidLen: PtrInt): integer; overload;


type
  /// define a list of well-known Security IDentifier (SID) groups
  // - for instance, wksBuiltinAdministrators is set for local administrators
  // - warning: does not exactly match winnt.h WELL_KNOWN_SID_TYPE enumeration
  // - see [MS-DTYP] 2.4.2.4 Well-Known SID Structures
  TWellKnownSid = (
    wksNull,                                      // S-1-0-0
    wksWorld,                                     // S-1-1-0       WD
    wksLocal,                                     // S-1-2-0
    wksConsoleLogon,                              // S-1-2-1
    wksCreatorOwner,                              // S-1-3-0       CO
    wksCreatorGroup,                              // S-1-3-1       CG
    wksCreatorOwnerServer,                        // S-1-3-2
    wksCreatorGroupServer,                        // S-1-3-3
    wksCreatorOwnerRights,                        // S-1-3-4       OW
    wksIntegrityUntrusted,                        // S-1-16-0
    wksIntegrityLow,                              // S-1-16-4096   LW
    wksIntegrityMedium,                           // S-1-16-8192   ME
    wksIntegrityMediumPlus,                       // S-1-16-8448   MP
    wksIntegrityHigh,                             // S-1-16-12288  HI
    wksIntegritySystem,                           // S-1-16-16384  SI
    wksIntegrityProtectedProcess,                 // S-1-16-20480
    wksIntegritySecureProcess,                    // S-1-16-28672
    wksAuthenticationAuthorityAsserted,           // S-1-18-1
    wksAuthenticationServiceAsserted,             // S-1-18-2      SS
    wksAuthenticationFreshKeyAuth,                // S-1-18-3
    wksAuthenticationKeyTrust,                    // S-1-18-4
    wksAuthenticationKeyPropertyMfa,              // S-1-18-5
    wksAuthenticationKeyPropertyAttestation,      // S-1-18-6
    wksNtAuthority,                               // S-1-5
    wksDialup,                                    // S-1-5-1
    wksNetwork,                                   // S-1-5-2       NU
    wksBatch,                                     // S-1-5-3
    wksInteractive,                               // S-1-5-4       IU
    wksService,                                   // S-1-5-6       SU
    wksAnonymous,                                 // S-1-5-7       AN
    wksProxy,                                     // S-1-5-8
    wksEnterpriseControllers,                     // S-1-5-9       ED
    wksSelf,                                      // S-1-5-10      PS
    wksAuthenticatedUser,                         // S-1-5-11      AU
    wksRestrictedCode,                            // S-1-5-12      RC
    wksTerminalServer,                            // S-1-5-13
    wksRemoteLogonId,                             // S-1-5-14
    wksThisOrganisation,                          // S-1-5-15
    wksIisUser,                                   // S-1-5-17
    wksLocalSystem,                               // S-1-5-18      SY
    wksLocalService,                              // S-1-5-19      LS
    wksNetworkService,                            // S-1-5-20      NS
    wksLocalAccount,                              // S-1-5-113
    wksLocalAccountAndAdministrator,              // S-1-5-114
    wksBuiltinDomain,                             // S-1-5-32
    wksBuiltinAdministrators,                     // S-1-5-32-544  BA
    wksBuiltinUsers,                              // S-1-5-32-545  BU
    wksBuiltinGuests,                             // S-1-5-32-546  BG
    wksBuiltinPowerUsers,                         // S-1-5-32-547  PU
    wksBuiltinAccountOperators,                   // S-1-5-32-548  AO
    wksBuiltinSystemOperators,                    // S-1-5-32-549  SO
    wksBuiltinPrintOperators,                     // S-1-5-32-550  PO
    wksBuiltinBackupOperators,                    // S-1-5-32-551  BO
    wksBuiltinReplicator,                         // S-1-5-32-552  RE
    wksBuiltinRasServers,                         // S-1-5-32-553
    wksBuiltinPreWindows2000CompatibleAccess,     // S-1-5-32-554  RU
    wksBuiltinRemoteDesktopUsers,                 // S-1-5-32-555  RD
    wksBuiltinNetworkConfigurationOperators,      // S-1-5-32-556  NO
    wksBuiltinIncomingForestTrustBuilders,        // S-1-5-32-557
    wksBuiltinPerfMonitoringUsers,                // S-1-5-32-558  MU
    wksBuiltinPerfLoggingUsers,                   // S-1-5-32-559  LU
    wksBuiltinAuthorizationAccess,                // S-1-5-32-560
    wksBuiltinTerminalServerLicenseServers,       // S-1-5-32-561
    wksBuiltinDcomUsers,                          // S-1-5-32-562
    wksBuiltinIUsers,                             // S-1-5-32-568  IS
    wksBuiltinCryptoOperators,                    // S-1-5-32-569  CY
    wksBuiltinUnknown,                            // S-1-5-32-570
    wksBuiltinCacheablePrincipalsGroups,          // S-1-5-32-571
    wksBuiltinNonCacheablePrincipalsGroups,       // S-1-5-32-572
    wksBuiltinEventLogReadersGroup,               // S-1-5-32-573  ER
    wksBuiltinCertSvcDComAccessGroup,             // S-1-5-32-574  CD
    wksBuiltinRdsRemoteAccessServers,             // S-1-5-32-575  RA
    wksBuiltinRdsEndpointServers,                 // S-1-5-32-576  ES
    wksBuiltinRdsManagementServers,               // S-1-5-32-577  MS
    wksBuiltinHyperVAdmins,                       // S-1-5-32-578  HA
    wksBuiltinAccessControlAssistanceOperators,   // S-1-5-32-579  AA
    wksBuiltinRemoteManagementUsers,              // S-1-5-32-580  RM
    wksBuiltinDefaultSystemManagedGroup,          // S-1-5-32-581
    wksBuiltinStorageReplicaAdmins,               // S-1-5-32-582
    wksBuiltinDeviceOwners,                       // S-1-5-32-583
    wksBuiltinWriteRestrictedCode,                // S-1-5-33      WR
    wksBuiltinUserModeDriver,                     // S-1-5-84-0-0-0-0-0 UD
    wksCapabilityInternetClient,                  // S-1-15-3-1
    wksCapabilityInternetClientServer,            // S-1-15-3-2
    wksCapabilityPrivateNetworkClientServer,      // S-1-15-3-3
    wksCapabilityPicturesLibrary,                 // S-1-15-3-4
    wksCapabilityVideosLibrary,                   // S-1-15-3-5
    wksCapabilityMusicLibrary,                    // S-1-15-3-6
    wksCapabilityDocumentsLibrary,                // S-1-15-3-7
    wksCapabilityEnterpriseAuthentication,        // S-1-15-3-8
    wksCapabilitySharedUserCertificates,          // S-1-15-3-9
    wksCapabilityRemovableStorage,                // S-1-15-3-10
    wksCapabilityAppointments,                    // S-1-15-3-11
    wksCapabilityContacts,                        // S-1-15-3-12
    wksBuiltinAnyPackage,                         // S-1-15-2-1    AC
    wksBuiltinAnyRestrictedPackage,               // S-1-15-2-2
    wksNtlmAuthentication,                        // S-1-5-64-10
    wksSChannelAuthentication,                    // S-1-5-64-14
    wksDigestAuthentication);                     // S-1-5-64-21

  /// define a set of well-known SID
  TWellKnownSids = set of TWellKnownSid;

  /// define a list of well-known domain relative sub-authority RID values
  // - see [MS-DTYP] 2.4.2.4 Well-Known SID Structures
  TWellKnownRid = (
    wkrGroupReadOnly,                  // DOMAIN_GROUP_RID_ENTERPRISE_READONLY_DOMAIN_CONTROLLERS RO
    wkrUserAdmin,                      // DOMAIN_USER_RID_ADMIN  LA
    wkrUserGuest,                      // DOMAIN_USER_RID_GUEST  LG
    wkrServiceKrbtgt,                  // DOMAIN_USER_RID_KRBTGT
    wkrGroupAdmins,                    // DOMAIN_GROUP_RID_ADMINS DA
    wkrGroupUsers,                     // DOMAIN_GROUP_RID_USERS DU
    wkrGroupGuests,                    // DOMAIN_GROUP_RID_GUESTS DG
    wkrGroupComputers,                 // DOMAIN_GROUP_RID_COMPUTERS DC
    wkrGroupControllers,               // DOMAIN_GROUP_RID_CONTROLLERS DD
    wkrGroupCertAdmins,                // DOMAIN_GROUP_RID_CERT_ADMINS     CA
    wkrGroupSchemaAdmins,              // DOMAIN_GROUP_RID_SCHEMA_ADMINS   SA
    wkrGroupEntrepriseAdmins,          // DOMAIN_GROUP_RID_ENTERPRISE_ADMINS EA
    wkrGroupPolicyAdmins,              // DOMAIN_GROUP_RID_POLICY_ADMINS  PA
    wkrGroupReadOnlyControllers,       // DOMAIN_GROUP_RID_READONLY_CONTROLLERS
    wkrGroupCloneableControllers,      // DOMAIN_GROUP_RID_CLONEABLE_CONTROLLERS CN
    wkrGroupProtectedUsers,            // DOMAIN_GROUP_RID_PROTECTED_USERS AP
    wkrGroupKeyAdmins,                 // DOMAIN_GROUP_RID_KEY_ADMINS  KA
    wkrGroupEntrepriseKeyAdmins,       // DOMAIN_GROUP_RID_ENTERPRISE_KEY_ADMINS EK
    wrkGroupRasServers,                // DOMAIN_ALIAS_RID_RAS_SERVERS RS
    wrkAllowedRodcPasswordReplication, // ALLOWED_RODC_PASSWORD_REPLICATION_GROUP
    wrkDeniedRodcPasswordReplication,  // DENIED_RODC_PASSWORD_REPLICATION_GROUP
    wrkUserModeHwOperator);            // DOMAIN_ALIAS_RID_USER_MODE_HARDWARE_OPERATORS HO

  /// define a set of well-known RID
  TWellKnownRids = set of TWellKnownRid;

/// returns a Security IDentifier of a well-known SID as binary
// - is using an internal cache for the returned RawSid instances
function KnownRawSid(wks: TWellKnownSid): RawSid; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// returns a Security IDentifier of a well-known SID as binary
procedure KnownRawSid(wks: TWellKnownSid; var sid: RawSid); overload;

/// returns a Security IDentifier of a well-known SID as static binary buffer
procedure KnownRawSid(wks: TWellKnownSid; var sid: TSid); overload;

/// returns a Security IDentifier of a well-known SID as standard text
// - e.g. wksBuiltinAdministrators as 'S-1-5-32-544'
function KnownSidToText(wks: TWellKnownSid): PShortString; overload;

/// recognize most well-known SID from a Security IDentifier binary buffer
// - returns wksNull if the supplied buffer was not recognized
function SidToKnown(sid: PSid): TWellKnownSid; overload;

/// recognize most well-known SID from a Security IDentifier standard text
// - returns wksNull if the supplied text was not recognized
function SidToKnown(const text: RawUtf8): TWellKnownSid; overload;

/// recognize some well-known SIDs from the supplied SID dynamic array
function SidToKnownGroups(const sids: PSids): TWellKnownSids;

/// returns a Security IDentifier of a well-known RID as binary
// - the Domain is expected to be in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' layout
function KnownRawSid(wkr: TWellKnownRid; const Domain: RawUtf8): RawSid; overload;

/// returns a Security IDentifier of a well-known RID as standard text
// - the Domain is expected to be in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' layout
// - e.g. wkrUserAdmin as 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx-500'
function KnownSidToText(wkr: TWellKnownRid; const Domain: RawUtf8): RawUtf8; overload;

/// compute a binary SID from a decoded binary Domain and a well-known RID
// - more efficient than KnownRawSid() overload and KnownSidToText()
procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: RawSid); overload;

/// compute a static binary SID from a decoded binary Domain and a well-known RID
procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: TSid); overload;

const
  /// the S-1-5-21-xx-xx-xx-RID trailer value of each known RID
  // - see [MS-DTYP] 2.4.2.4 Well-Known SID Structures
  WKR_RID: array[TWellKnownRid] of word = (
    498,    // $1f2  RO wkrGroupReadOnly
    500,    // $1f4  LA wkrUserAdmin
    501,    // $1f5  LG wkrUserGuest
    502,    // $1f6     wkrServiceKrbtgt
    512,    // $200  DA wkrGroupAdmins
    513,    // $201  DU wkrGroupUsers
    514,    // $202  DG wkrGroupGuests
    515,    // $203  DC wkrGroupComputers
    516,    // $204  DD wkrGroupControllers
    517,    // $205  CA wkrGroupCertAdmins
    518,    // $206  SA wkrGroupSchemaAdmins
    519,    // $207  EA wkrGroupEntrepriseAdmins
    520,    // $208  PA wkrGroupPolicyAdmins
    521,    // $209     wkrGroupReadOnlyControllers
    522,    // $20a  CN wkrGroupCloneableControllers
    525,    // $20d  AP wkrGroupProtectedUsers
    526,    // $20e  KA wkrGroupKeyAdmins
    527,    // $20f  EK wkrGroupEntrepriseKeyAdmins
    553,    // $229  RS wrkGroupRasServers
    571,    // $23b     wrkAllowedRodcPasswordReplication
    572,    // $23c     wrkDeniedRodcPasswordReplication
    584);   // $248  HO wrkUserModeHwOperator

  /// the maximum known RID value of S-1-5-21-xx-xx-xx-RID patterns
  WKR_RID_MAX = 584;


{ ****************** Security Descriptor Self-Relative Binary Structures }

type
  /// flags to specify control access to TSecurityDescriptor
  // - see e.g. [MS-DTYP] 2.4.6 SECURITY_DESCRIPTOR
  TSecControl = (
    scOwnerDefaulted,
    scGroupDefaulted,
    scDaclPresent,
    scDaclDefaulted,
    scSaclPresent,
    scSaclDefaulted,
    scDaclTrusted,
    scServerSecurity,
    scDaclAutoInheritReq,
    scSaclAutoInheritReq,
    scDaclAutoInherit,
    scSaclAutoInherit,
    scDaclProtected,
    scSaclProtected,
    scRmControlValid,
    scSelfRelative);
  /// TSecurityDescriptor.Controls to specify control access
  TSecControls = set of TSecControl;

  // map the _SECURITY_DESCRIPTOR struct in self-relative state
  // - see [MS-DTYP] 2.4.6 SECURITY_DESCRIPTOR
  TRawSD = packed record
    Revision: byte;
    Sbz1: byte;      // always DWORD-aligned
    Control: TSecControls;
    Owner: cardinal; // not pointers, but self-relative position
    Group: cardinal;
    Sacl: cardinal;
    Dacl: cardinal;
  end;
  PRawSD = ^TRawSD;

  // map the Access Control List header
  // - see [MS-DTYP] 2.4.5.1 ACL--RPC Representation
  TRawAcl = packed record
    AclRevision: byte;
    Sbz1: byte;
    AclSize: word; // including TRawAcl header + all ACEs
    AceCount: word;
    Sbz2: word;    // always DWORD-aligned
  end;
  PRawAcl = ^TRawAcl;

  /// high-level supported ACE flags in TSecAce.Flags
  // - see [MS-DTYP] 2.4.4.1 ACE_HEADER
  // - safObjectInherit: non-container child objects inherit the ACE as an
  // effective ACE
  // - safContainerInherit: child objects that are containers, such as directories,
  // inherit the ACE as an effective ACE
  // - safNoPropagateInherit: If the ACE is inherited by a child object, the
  // system clears the safObjectInherit and safContainerInherit flags in the
  // inherited ACE. This prevents the ACE from being inherited by subsequent
  // generations of objects.
  // - safInheritOnly indicates an inherit-only ACE, which does not control
  // access to the object to which it is attached. If this flag is not set,
  // the ACE is an effective ACE that controls access to the object to which
  // it is attached
  // - safInherited is used to indicate that the ACE was inherited
  // - safSuccessfulAccess is used with system-audit ACEs in a SACL to generate
  // audit messages for successful access attempts
  // - safFailedAccess is used with system-audit ACEs in a system access
  // control list (SACL) to generate audit messages for failed access attempts
  TSecAceFlag = (
    safObjectInherit,        // OI
    safContainerInherit,     // CI
    safNoPropagateInherit,   // NP
    safInheritOnly,          // IO
    safInherited,            // ID
    saf5,
    safSuccessfulAccess,     // SA
    safFailedAccess);        // FA

  /// high-level supported ACE flags in TSecAce.Flags
  TSecAceFlags = set of TSecAceFlag;

  /// standard and generic access rights in TSecAce.Mask
  // - see [MS-DTYP] 2.4.3 ACCESS_MASK
  // - well defined set of files or keys flags are defined as TSecAccessRight
  // - other sets of rights will be identified by those individual values
  TSecAccess = (
    samCreateChild,          // CC
    samDeleteChild,          // DC
    samListChildren,         // LC
    samSelfWrite,            // SW
    samReadProp,             // RP
    samWriteProp,            // WP
    samDeleteTree,           // DT
    samListObject,           // LO
    samControlAccess,        // CR
    sam9,
    sam10,
    sam11,
    sam12,
    sam13,
    sam14,
    sam15,
    samDelete,               // SD
    samReadControl,          // RC
    samWriteDac,             // WD
    samWriteOwner,           // WO
    samSynchronize,
    sam21,
    sam22,
    sam23,
    samAccessSystemSecurity,
    samMaximumAllowed,
    sam26,
    sam27,
    samGenericAll,           // GA
    samGenericExecute,       // GX
    samGenericWrite,         // GW
    samGenericRead);         // GR

  /// 32-bit standard and generic access rights in TRawAce/TSecAce.Mask
  // - see [MS-DTYP] 2.4.3 ACCESS_MASK
  // - TSecAccessRight defines specific sets for files or registry keys
  TSecAccessMask = set of TSecAccess;
  PSecAccessMask = ^TSecAccessMask;

  // map one Access Control Entry
  // - see [MS-DTYP] 2.4.4.1 ACE_HEADER
  TRawAce = packed record
    // ACE header
    AceType: byte;  // typically equals ord(TSecAceType) - 1
    AceFlags: TSecAceFlags;
    AceSize: word; // include ACE header + whole body
    // ACE body
    Mask: TSecAccessMask;
    case integer of
      0: (CommonSid: cardinal);
      1: (ObjectFlags: cardinal;
          ObjectStart: cardinal);
  end;
  PRawAce = ^TRawAce;

  /// TRawAce/TSecAce.Mask constant values with their own SDDL identifier
  // - i.e. TSecAccessMask sets which are commonly used togethers
  // - sarFile* for files, sarKey* for registry keys, and also services
  // - note that sarKeyExecute and sarKeyRead have the actual same 32-bit value
  TSecAccessRight = (
    sarFileAll,             // FA
    sarFileRead,            // FR
    sarFileWrite,           // FW
    sarFileExecute,         // FX
    sarKeyAll,              // KA
    sarKeyRead,             // KR
    sarKeyWrite,            // KW
    sarKeyExecute);         // KE

const
  /// 'artx' prefix to identify a conditional ACE binary buffer
  // - see [MS-DTYP] 2.4.4.17.4 Conditional ACE Binary Formats
  ACE_CONDITION_SIGNATURE = $78747261;

  ACE_OBJECT_TYPE_PRESENT           = 1;
  ACE_INHERITED_OBJECT_TYPE_PRESENT = 2;

  {  cross-platform definitions of TSecAccessRight 32-bit Windows access rights }

  SAR_FILE_ALL_ACCESS      = $001f01ff;
  SAR_FILE_GENERIC_READ    = $00120089;
  SAR_FILE_GENERIC_WRITE   = $00120116;
  SAR_FILE_GENERIC_EXECUTE = $001200a0;

  SAR_KEY_ALL_ACCESS       = $000f003f;
  SAR_KEY_READ             = $00020019;
  SAR_KEY_WRITE            = $00020006;
  SAR_KEY_EXECUTE          = $00020019; // note that KEY_EXECUTE = KEY_READ

  /// match the TSecAce.Mask constant values with their own SDDL identifier
  SAR_MASK: array[TSecAccessRight] of cardinal = (
    SAR_FILE_ALL_ACCESS,       // sarFileAll
    SAR_FILE_GENERIC_READ,     // sarFileRead
    SAR_FILE_GENERIC_WRITE,    // sarFileWrite
    SAR_FILE_GENERIC_EXECUTE,  // sarFileExecute
    SAR_KEY_ALL_ACCESS,        // sarKeyAll
    SAR_KEY_READ,              // sarKeyRead
    SAR_KEY_WRITE,             // sarKeyWrite
    SAR_KEY_EXECUTE);          // sarKeyExecute = sarKeyRead

  /// specifies the user rights allowed by satObject ACE
  // - see e.g. [MS-DTYP] 2.4.4.3 ACCESS_ALLOWED_OBJECT_ACE
  samObject = [
    samCreateChild,
    samDeleteChild,
    samSelfWrite,
    samReadProp,
    samWriteProp,
    samControlAccess];

  /// alias for satMandatoryLabel so that a principal with a lower mandatory
  // level than the object cannot write to the object
  samMandatoryLabelNoWriteUp   = samCreateChild;
  /// alias for satMandatoryLabel so that a principal with a lower mandatory
  // level than the object cannot read the object
  samMandatoryLabelNoReadUp    = samDeleteChild;
  /// alias for satMandatoryLabel so that a principal with a lower mandatory
  // level than the object cannot execute the object
  samMandatoryLabelNoExecuteUp = samListChildren;


{ ****************** Access Control List (DACL/SACL) Definitions }

type
  /// high-level supported ACE kinds in TSecAce.AceType
  // - see [MS-DTYP] 2.4.4.1 ACE_HEADER
  // - satAccessAllowed/satAccessDenied allows or denies access to an object
  // for a specific trustee identified by a security identifier (SID)
  // - satAudit causes an audit message to be logged when a specified trustee
  // (identified by a SID) attempts to gain access to an object
  // - satObjectAccessAllowed/satObjectAccessDenied define an ACE that controls
  // allowed/denied access to an object, a property set, or property: in addition
  // to the access rights and SID, it includes object GUIDs and inheritance flags
  // - satCallbackAccessAllowed/satCallbackAccessDenied allows or denies access
  // to an object for a specific trustee identified by a SID, with optional
  // application data, typically a conditional ACE expression
  // - satMandatoryLabel defines an ACE for the SACL that specifies the
  // mandatory access level and policy for a securable object - masks values
  // are samMandatoryLabelNoWriteUp, samMandatoryLabelNoReadUp and
  // samMandatoryLabelNoExecuteUp
  // - satResourceAttribute defines an ACE for the specification of a resource
  // attribute associated with an object, i.e. is is used in conditional ACEs in
  // specifying access or audit policy for the resource - ending with a [MS-DTYP]
  // 2.4.10.1 CLAIM_SECURITY_ATTRIBUTE_RELATIVE_V1 resource attribute structure
  // - satScoppedPolicy defines an ACE for the purpose of applying a central
  // access policy to the resource: Mask is 0, and ends with a [MS-GPCAP] 3.2.1.1
  // CentralAccessPoliciesList structure
  TSecAceType = (
    satUnknown,
    satAccessAllowed,                 // A  0  ACCESS_ALLOWED_ACE_TYPE
    satAccessDenied,                  // D  1  ACCESS_DENIED_ACE_TYPE
    satAudit,                         // AU 2  SYSTEM_AUDIT_ACE_TYPE
    satAlarm,                         // AL 3  SYSTEM_ALARM_ACE_TYPE
    satCompoundAllowed,               //    4  ACCESS_ALLOWED_COMPOUND_ACE_TYPE
    satObjectAccessAllowed,           // OA 5  ACCESS_ALLOWED_OBJECT_ACE_TYPE
    satObjectAccessDenied,            // OD 6  ACCESS_DENIED_OBJECT_ACE_TYPE
    satObjectAudit,                   // OU 7  SYSTEM_AUDIT_OBJECT_ACE_TYPE
    satObjectAlarm,                   // OL 8  SYSTEM_ALARM_OBJECT_ACE_TYPE
    satCallbackAccessAllowed,         // XA 9  ACCESS_ALLOWED_CALLBACK_ACE_TYPE
    satCallbackAccessDenied,          // XD 10 ACCESS_DENIED_CALLBACK_ACE_TYPE
    satCallbackObjectAccessAllowed,   // ZA 11 ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE
    satCallbackObjectAccessDenied,    //    12 ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE
    satCallbackAudit,                 // XU 13 SYSTEM_AUDIT_CALLBACK_ACE_TYPE
    satCallbackAlarm,                 //    14 SYSTEM_ALARM_CALLBACK_ACE_TYPE
    satCallbackObjectAudit,           //    15 SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE
    satCallbackObjectAlarm,           //    16 SYSTEM_ALARM_CALLBACK_OBJECT_ACE_TYPE
    satMandatoryLabel,                // ML 17 SYSTEM_MANDATORY_LABEL_ACE_TYPE
    satResourceAttribute,             // RA 18 SYSTEM_RESOURCE_ATTRIBUTE_ACE_TYPE
    satScoppedPolicy,                 // SP 19 SYSTEM_SCOPED_POLICY_ID_ACE_TYPE
    satProcessTrustLabel,             // TL 20 SYSTEM_PROCESS_TRUST_LABEL_ACE_TYPE
    satAccessFilter);                 // FL 21 SYSTEM_ACCESS_FILTER_ACE_TYPE

  /// define one TSecurityDescriptor Dacl[] or Sacl[] access control list (ACL)
  TSecAceScope = (
    sasDacl,
    sasSacl);

  /// result of TSecurityDescriptor.FromText and other SDDL parsing methods
  TAceTextParse = (
    atpSuccess,
    atpInvalidOwner,
    atpInvalidGroup,
    atpInvalidType,
    atpInvalidFlags,
    atpInvalidMask,
    atpInvalidUuid,
    atpNoExpression,
    atpTooManyExpressions,
    atpTooManyParenthesis,
    atpMissingParenthesis,
    atpMissingFinal,
    atpTooBigExpression,
    atpMissingExpression,
    atpUnexpectedToken,
    atpInvalidExpression,
    atpInvalidComposite,
    atpInvalidUnicode,
    atpInvalidSid,
    atpInvalidOctet,
    atpInvalidContent);

  {$A-} // both TSecAce and TSecurityDescriptor should be packed for JSON serialization

  /// define one discretionary/system access entry (ACE)
  {$ifdef USERECORDWITHMETHODS}
  TSecAce = record
  {$else}
  TSecAce = object
  {$endif USERECORDWITHMETHODS}
  public
    /// high-level supported ACE kinds
    AceType: TSecAceType;
    // the ACE flags
    Flags: TSecAceFlags;
    /// the raw ACE identifier - typically = ord(AceType) - 1
    // - if you force this type, ensure you set AceType=satUnknown before saving
    // - defined as word for proper alignment
    RawType: word;
    /// contains the associated 32-bit access mask
    Mask: TSecAccessMask;
    /// contains an associated SID, in its raw binary content
    Sid: RawSid;
    /// some optional opaque callback/resource binary data, stored after the Sid
    // - e.g. is a conditional expression binary for satConditional ACEs like
    // '(@User.Project Any_of @Resource.Project)', or for satResourceAttribute
    // is a CLAIM_SECURITY_ATTRIBUTE_RELATIVE_V1 struct
    // - may end with up to 3 zero bytes, for binary DWORD alignment
    Opaque: RawByteString;
    /// the associated Object Type GUID (satObject only)
    ObjectType: TGuid;
    /// the inherited Object Type GUID  (satObject only)
    InheritedObjectType: TGuid;
    /// remove any previous content
    procedure Clear;
    /// compare the fields of this instance with another
    function IsEqual(const ace: TSecAce): boolean;
    /// append this entry as SDDL text into an existing buffer
    // - could also generate SDDL RID placeholders, if dom binary is supplied,
    // e.g. S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
    // - could also customize UUID values, e.g. with uuid = @AppendShortKnownUuid
    procedure AppendAsText(var s: ShortString; var sddl: TSynTempBuffer;
      dom: PSid; uuid: TAppendShortUuid);
    /// decode a SDDL ACE textual representation into this (cleared) entry
    function FromText(var p: PUtf8Char; dom: PSid; uuid: TShortToUuid): TAceTextParse;
    /// encode this entry into a self-relative binary buffer
    // - returns the output length in bytes
    // - if dest is nil, will compute the length but won't write anything
    function ToBinary(dest: PAnsiChar): PtrInt;
    /// decode a self-relative binary buffer into this (cleared) entry
    function FromBinary(p, max: PByte): boolean;
    /// user-friendly set the properties of this ACE
    // - SID and Mask are supplied in their regular / SDDL text form
    // - returns true on success - i.e. if all input params were correct
    function Fill(sat: TSecAceType; const sidSddl, maskSddl: RawUtf8;
      dom: PSid = nil; const condExp: RawUtf8 = ''; saf: TSecAceFlags = []): boolean;
    /// get the associated SID, as SDDL text, optionally with a RID domain
    function SidText(dom: PSid = nil): RawUtf8; overload;
    /// set the associated SID, as SDDL text, optionally with a RID domain
    function SidText(const sidSddl: RawUtf8; dom: PSid = nil): boolean; overload;
    /// get the associated access mask, as SDDL text format
    function MaskText: RawUtf8; overload;
    /// set the associated access mask, as SDDL text format
    function MaskText(const maskSddl: RawUtf8): boolean; overload;
    /// get the ACE conditional expression, as stored in Opaque binary
    function ConditionalExpression(dom: PSid = nil): RawUtf8; overload;
    /// parse a ACE conditional expression, and assign it to the Opaque binary
    function ConditionalExpression(const condExp: RawUtf8;
      dom: PSid = nil): TAceTextParse; overload;
    /// replace all nested RID from one domain to another
    // - also within any ACE conditional expression
    function ReplaceDomainRaw(old, new: PSid; maxRid: cardinal): integer;
    /// replace all nested SID from one set of values to another
    // - also within any ACE conditional expression
    function ReplaceAnySid(const OldSid, NewSid: RawSidDynArray): integer;
  end;

  /// pointer to one ACE of the TSecurityDescriptor ACL
  PSecAce = ^TSecAce;

  /// define a discretionary/system access control list (DACL)
  // - as stored in TSecurityDescriptor Dacl[] Sacl[] access control lists (ACL)
  TSecAcl = array of TSecAce;
  PSecAcl = ^TSecAcl;

  {$A+}

const
  /// the ACE which have just a Mask and SID in their definition
  satCommon = [
    satAccessAllowed,
    satAccessDenied,
    satAudit,
    satAlarm,
    satCallbackAccessAllowed,
    satCallbackAccessDenied,
    satCallbackAudit,
    satCallbackAlarm,
    satMandatoryLabel,
    satResourceAttribute,
    satScoppedPolicy,
    satAccessFilter];

  /// the ACE which have a samObject Mask, SID and Object UUIDs in their definition
  satObject = [
    satObjectAccessAllowed,
    satObjectAccessDenied,
    satObjectAudit,
    satObjectAlarm,
    satCallbackObjectAccessAllowed,
    satCallbackObjectAccessDenied,
    satCallbackObjectAudit,
    satCallbackObjectAlarm];

  /// the ACE which have a conditional expression as TSecAce.Opaque member
  // - see [MS-DTYP] 2.4.4.17
  // - the associated conditional expression is encoded in a binary format
  // in the TSecAce.Opaque
  satConditional = [
    satCallbackAccessAllowed,
    satCallbackAccessDenied,
    satCallbackAudit,
    satCallbackObjectAccessAllowed,
    satCallbackObjectAccessDenied,
    satCallbackObjectAudit];

  /// defined in TSecAce.Flags for an ACE which has inheritance
  safInheritanceFlags = [
    safObjectInherit,
    safContainerInherit,
    safNoPropagateInherit,
    safInheritOnly];

  /// defined in TSecAce.Flags for an ACE which refers to audit
  safAuditFlags = [
    safSuccessfulAccess,
    safFailedAccess];

/// compute a self-relative binary of a given ACL array
// - as stored within a TSecurityDescriptor instance, and accepted by
// low-level WinAPI SetSystemSecurityDescriptor() function
function SecAclToBinary(const acl: TSecAcl): RawByteString;


{ ******************* Conditional ACE Expressions SDDL and Binary Support }

type
  /// token types for the conditional ACE binary storage
  // - the conditional ACE is stored as a binary tree of value/operator nodes
  // - mainly as sctLiteral, sctAttribute or sctOperator tokens
  // - TSecConditionalToken ordinal value is the stored ACE binary token byte
  // - tokens >= sctInternalFinal are never serialized, but used internally
  // during SddlNextOperand() SDDL text parsing
  // - see [MS-DTYP] 2.4.4.17.4 and following
  TSecConditionalToken = (
    sctPadding              = $00,
    sctInt8                 = $01,
    sctInt16                = $02,
    sctInt32                = $03,
    sctInt64                = $04,
    sctUnicode              = $10,
    sctOctetString          = $18,
    sctComposite            = $50,
    sctSid                  = $51,
    sctEqual                = $80,
    sctNotEqual             = $81,
    sctLessThan             = $82,
    sctLessThanOrEqual      = $83,
    sctGreaterThan          = $84,
    sctGreaterThanOrEqual   = $85,
    sctContains             = $86,
    sctExists               = $87,
    sctAnyOf                = $88,
    sctMemberOf             = $89,
    sctDeviceMemberOf       = $8a,
    sctMemberOfAny          = $8b,
    sctDeviceMemberOfAny    = $8c,
    sctNotExists            = $8d,
    sctNotContains          = $8e,
    sctNotAnyOf             = $8f,
    sctNotMemberOf          = $90,
    sctNotDeviceMemberOf    = $91,
    sctNotMemberOfAny       = $92,
    sctNotDeviceMemberOfAny = $93,
    sctAnd                  = $a0,
    sctOr                   = $a1,
    sctNot                  = $a2,
    sctLocalAttribute       = $f8,
    sctUserAttribute        = $f9,
    sctResourceAttribute    = $fa,
    sctDeviceAttribute      = $fb,
    // internal tokens - never serialized into binary
    sctInternalFinal        = $fc,
    sctInternalParenthOpen  = $fe,
    sctInternalParenthClose = $ff);
  PSecConditionalToken = ^TSecConditionalToken;

  /// binary conditional ACE integer base indicator for SDDL/display purposes
  // - see [MS-DTYP] 2.4.4.17.5 Literal Tokens
  TSecConditionalBase = (
    scbUndefined   = 0,
    scbOctal       = 1,
    scbDecimal     = 2,
    scbHexadecimal = 3);

  /// binary conditional ACE integer sign indicator for SDDL/display purposes
  // - see [MS-DTYP] 2.4.4.17.5 Literal Tokens
  TSecConditionalSign = (
    scsUndefined = 0,
    scsPositive  = 1,
    scsNegative  = 2,
    scsNone      = 3);

const
  /// literal integer ACE token types
  // - see [MS-DTYP] 2.4.4.17.5 Literal Tokens
  sctInt = [
    sctInt8 .. sctInt64];

  /// literal ACE token types
  // - see [MS-DTYP] 2.4.4.17.5 Literal Tokens
  sctLiteral =
    sctInt + [
      sctUnicode,
      sctOctetString,
      sctComposite,
      sctSid];

  /// unary relational operator ACE token types
  // - operand type must be either a SID literal, or a composite, each of
  // whose elements is a SID literal
  // - see [MS-DTYP] 2.4.4.17.6 Relational Operator Tokens
  sctUnaryOperator = [
    sctMemberOf .. sctDeviceMemberOfAny,
    sctNotMemberOf .. sctNotDeviceMemberOfAny];

  /// binary relational operator ACE token types
  // - expects two operands, left-hand-side (LHS - in simple or @Prefixed form)
  // and right-hand-side (RHS - in @Prefixed form or literal)
  // - see [MS-DTYP] 2.4.4.17.6 Relational Operator Tokens
  sctBinaryOperator = [
    sctEqual .. sctContains,
    sctAnyOf,
    sctNotContains,
    sctNotAnyOf];

  /// relational operator ACE token types
  // - see [MS-DTYP] 2.4.4.17.6 Relational Operator Tokens
  sctRelationalOperator = sctUnaryOperator + sctBinaryOperator;

  /// unary logical operator ACE token types
  // - see [MS-DTYP] 2.4.4.17.7 Logical Operator Tokens
  sctUnaryLogicalOperator = [
    sctExists,
    sctNotExists,
    sctNot];

  /// binary logical operator ACE token types
  // - see [MS-DTYP] 2.4.4.17.7 Logical Operator Tokens
  sctBinaryLogicalOperator = [
    sctAnd,
    sctOr];

  /// logical operator ACE token types
  // - operand type must be conditional expressions and/or expression terms:
  // a sctLiteral operand would return an error
  // - see [MS-DTYP] 2.4.4.17.7 Logical Operator Tokens
  sctLogicalOperator = sctUnaryLogicalOperator + sctBinaryLogicalOperator;

  /// attribute ACE token types
  // - attributes can be associated with local environments, users, resources,
  // or devices
  // - see [MS-DTYP] 2.4.4.17.8 Attribute Tokens
  sctAttribute = [
    sctLocalAttribute,
    sctUserAttribute,
    sctResourceAttribute,
    sctDeviceAttribute];

  /// operands ACE token types
  sctOperand = sctLiteral + sctAttribute;

  /// single-operand ACE token types
  sctUnary = sctUnaryOperator + sctUnaryLogicalOperator;

  /// dual-operand ACE token types
  sctBinary = sctBinaryOperator + sctBinaryLogicalOperator;

  /// operator ACE tokens
  sctOperator = sctUnary + sctBinary;

  /// maximum depth of the TAceBinaryTree/TAceTextTree resolution
  // - should be <= 255 to match the nodes index fields as byte
  // - 192 nodes seems fair enough for realistic ACE conditional expressions
  MAX_TREE_NODE = 191;

  /// maximum length, in bytes, of a TAceBinaryTree/TAceTextTree binary/text input
  // - absolute maximum is below 64KB (in TRawAce.AceSize)
  // - 8KB of conditional expression is consistent with the MAX_TREE_NODE limit
  MAX_TREE_BYTES = 8 shl 10;

type
  /// internal type used for TRawAceOperand.Int
  TRawAceOperandInt = packed record
    /// the actual value is always stored as 64-bit little-endian
    Value: Int64;
    /// can customize the sign when serialized as SDDL text
    Sign: TSecConditionalSign;
    /// can customize the base when serialized as SDDL text
    Base: TSecConditionalBase;
  end;

  /// token binary serialization for satConditional ACE literals and attributes
  // - the TSecAce.Opaque binary field starts with 'artx' ACE_CONDITION_SIGNATURE
  // and a series of entries, describing an expression tree in reverse Polish order
  // - only sctOperand do have nested fields here, because sctOperator are stored
  // as a single TSecConditionalToken byte on the binary stack
  // - see [MS-DTYP] 2.4.4.17.4 Conditional ACE Binary Formats
  TRawAceOperand = packed record
    case Token: TSecConditionalToken of
      // sctLiteral tokens
      sctInt8,
      sctInt16,
      sctInt32,
      sctInt64: (
        Int: TRawAceOperandInt);
      sctOctetString: (
        OctetBytes: cardinal;
        Octet: array[byte] of byte);
      sctComposite: (
        CompositeBytes: cardinal;
        Composite: array[byte] of byte);
      sctSid: (
        SidBytes: cardinal;
        Sid: TSid);
      sctUnicode,
      // sctAttribute tokens
      sctLocalAttribute,
      sctUserAttribute,
      sctResourceAttribute,
      sctDeviceAttribute: (
        UnicodeBytes: cardinal;
        Unicode: array[byte] of WideChar);
  end;
  PRawAceOperand = ^TRawAceOperand;

  /// define one node in the TAceBinaryTree.Nodes
  // - here pointers are just 8-bit indexes to the main TAceBinaryTree.Nodes[]
  TAceBinaryTreeNode = packed record
    /// position of this node in the input binary or text
    Position: word;
    /// index of the left or single operand
    Left: byte;
    /// index of the right operand
    Right: byte;
  end;
  /// points to one node in the TAceBinaryTree.Nodes
  PAceBinaryTreeNode = ^TAceBinaryTreeNode;

  /// store a processing tree of a conditional ACE in binary format
  // - used for conversion from binary to SDDL text format, or execution
  // - can easily be allocated on stack for efficient process
  {$ifdef USERECORDWITHMETHODS}
  TAceBinaryTree = record
  {$else}
  TAceBinaryTree = object
  {$endif USERECORDWITHMETHODS}
  private
    ToTextDom: PSid;
    function AddNode(position, left, right: cardinal): boolean;
    procedure GetNodeText(index: byte; var u: RawUtf8);
  public
    /// the associated input storage, typically mapping an TSecAce.Opaque buffer
    Storage: PUtf8Char;
    /// number of bytes in Storage
    StorageSize: cardinal;
    /// number of TAceTreeNode stored in Tokens[] and Stack[]
    Count: byte;
    /// stores the actual tree of this conditional expression
    Nodes: array[0 .. MAX_TREE_NODE] of TAceBinaryTreeNode;
    /// fill Nodes[] tree from a binary conditional ACE
    // - this process is very fast, and requires no memory allocation
    function FromBinary(const Input: RawByteString): boolean;
    /// save the internal Stack[] nodes into a binary conditional ACE
    // - just return the Storage/StorageSize content
    function ToBinary: RawByteString;
    /// compute the SDDL conditional ACE text of the stored Stack[]
    // - use a simple recursive algorithm, with temporary RawUtf8 allocations
    function ToText(dom: PSid = nil): RawUtf8;
  end;

  /// define one node in the TAceTextTree.Nodes
  // - here pointers are just 8-bit indexes to the main TAceTextTree.Nodes[]
  TAceTextTreeNode = packed record
    /// position of this node in the input binary or text
    Position: word;
    /// actual recognized token
    Token: TSecConditionalToken;
    /// index of the left or single operand
    Left: byte;
    /// index of the right operand
    Right: byte;
    /// number of UTF-8 bytes parsed at Position
    Length: byte;
  end;

  /// store a processing tree of a conditional ACE in binary format
  // - used for conversion from SDDL text to binary format
  // - can easily be allocated on stack for efficient process
  {$ifdef USERECORDWITHMETHODS}
  TAceTextTree = record
  {$else}
  TAceTextTree = object
  {$endif USERECORDWITHMETHODS}
  private
    TextCurrent: PUtf8Char;
    TokenCurrent: TSecConditionalToken;
    Root: byte;
    ParentCount: byte;
    ToBinaryDom: PSid;
    function AddNode(position, len: cardinal): integer;
    function ParseNextToken: integer;
    function ParseExpr: integer;
    function AppendBinary(var bin: TSynTempBuffer; node: integer): boolean;
    function RawAppendBinary(var bin: TSynTempBuffer;
      const node: TAceTextTreeNode): boolean;
  public
    /// the associated SDDL text input storage
    Storage: PUtf8Char;
    /// number of bytes in Storage
    StorageSize: cardinal;
    /// number of TAceTreeNode stored in Tokens[] and Stack[]
    Count: byte;
    /// actual status after FromText() or ToBinary processing
    Error: TAceTextParse;
    /// stores the actual tree of this conditional expression
    Nodes: array[0 .. MAX_TREE_NODE] of TAceTextTreeNode;
    /// fill Nodes[] tree from a SDDL text conditional ACE
    // - not all input will be parsed during this initial step: only the
    // global parsing is done, not detailed Unicode or composite parsing
    function FromText(const Input: RawUtf8): TAceTextParse; overload;
    /// fill Nodes[] tree from a SDDL text conditional ACE
    // - Storage/StorageSize should have been set before calling
    function FromText: TAceTextParse; overload;
    /// save the internal Stack[] nodes into a SDDL text conditional ACE
    // - just return the Storage/StorageSize content
    function ToText: RawUtf8;
    /// compute the binary conditional ACE of the stored Stack[]
    // - will also parse the nested Unicode or composite expressions from the
    // SDDL input, so this method could also set Error and return ''
    // - no temporary memory allocation is done during this process
    function ToBinary(dom: PSid = nil): RawByteString;
  end;

/// compute the length in bytes of an ACE token binary entry
function AceTokenLength(v: PRawAceOperand): PtrUInt;

/// check if any Sid within a conditional ACE binary is part of OldDomain,
// then change it into NewDomain
// - instead of using a temp TAceBinaryTree, we implemented a O(1) direct process
// within the RawByteString binary buffer itself
function AceReplaceDomain(olddom, newdom: PSid; maxRid: cardinal;
  var opaque: RawByteString): integer;

/// replace any occurence of old[] SID into new[] within a conditional ACE binary
// - instead of using a temp TAceBinaryTree, we implemented a O(1) direct process
// within the RawByteString binary buffer itself
// - expects each length(old[]) = length(new[]) to be replaced in-place
function AceReplaceAnySid(const old, new: RawSidDynArray;
  var opaque: RawByteString): integer;


{ ****************** Active Directory Definitions }
type
  /// some known Active Directory schema attributes
  // - we embed a small list of attributes with their ObjectID for conveniency
  // - this enumeration is sorted by ATTR_UUID[] binary order
  // - [MS-ADA3] publishes a huge list of content, which is outside the scope
  // of this unit - feel free to contribute, via some pull requests
  TAdsKnownAttribute = (
    kaNull,
    kaUserAccountRestrictions,
    kaMsDsKeyCredentialLink,
    kaUserLogon,
    kaInetOrgPerson,
    kaTerminalServer,
    kaTokenGroupsGlobalAndUniversal,
    kaDsSetOwner,
    kaMemberShip,
    kaGeneralInformation,
    kaDnsHostName,
    kaDescription,
    kaDomainAdministerServer,
    kaUserChangePassword,
    kaDisplayName,
    kaPublicInformation,
    kaSendAs,
    kaSendTo,
    kaReceiveAs,
    kaDomainPassword,
    kaTerminalServerLicenseServer,
    kaCertificateEnrollment,
    kaTokenGroups,
    kaUserForceChangePassword,
    kaAllowedToAuthenticate,
    kaUnexpirePassword,
    kaX509Cert,
    kaComputer,
    kaPersonalInformation,
    kaServicePrincipalName,
    kaApplyGroupPolicy,
    kaMsTpmInformationForComputer,
    kaGroup,
    kaDsValidatedWriteComputer,
    kaPrintQueue,
    kaAddGuid,
    kaEmailInformation,
    kaWebInformation,
    kaUser,
    kaSamAccountName,
    kaPrivateInformation,
    kaMsDsAllowedToActOnBehalfOfOtherIdentity,
    kaRasInformation);

const
  /// the ObjectID of known Active Directory schema attributes
  // - this list is sorted at byte level for faster O(log(n)) binary search
  // - note that byte-level order does not follow the GUID hexa text order
  ATTR_UUID: array[TAdsKnownAttribute] of TGuid = (
    '{00000000-0000-0000-0000-000000000000}',  // kaNull
    '{4c164200-20c0-11d0-a768-00aa006e0529}',  // kaUserAccountRestrictions
    '{5b47d60f-6090-40b2-9f37-2a4de88f3063}',  // kaMsDsKeyCredentialLink
    '{5f202010-79a5-11d0-9020-00c04fc2d4cf}',  // kaUserLogon
    '{4828cc14-1437-45bc-9b07-ad6f015e5f28}',  // kaInetOrgPerson
    '{6db69a1c-9422-11d1-aebd-0000f80367c1}',  // kaTerminalServer
    '{46a9b11d-60ae-405a-b7e8-ff8a58d456d2}',  // kaTokenGroupsGlobalAndUniversal
    '{4125c71f-7fac-4ff0-bcb7-f09a41325286}',  // kaDsSetOwner
    '{bc0ac240-79a9-11d0-9020-00c04fc2d4cf}',  // kaMemberShip
    '{59ba2f42-79a2-11d0-9020-00c04fc2d3cf}',  // kaGeneralInformation
    '{72e39547-7b18-11d1-adef-00c04fd8d5cd}',  // kaDnsHostName
    '{bf967950-0de6-11d0-a285-00aa003049e2}',  // kaDescription
    '{ab721a52-1e2f-11d0-9819-00aa0040529b}',  // kaDomainAdministerServer
    '{ab721a53-1e2f-11d0-9819-00aa0040529b}',  // kaUserChangePassword
    '{bf967953-0de6-11d0-a285-00aa003049e2}',  // kaDisplayName
    '{e48d0154-bcf8-11d1-8702-00c04fb96050}',  // kaPublicInformation
    '{ab721a54-1e2f-11d0-9819-00aa0040529b}',  // kaSendAs
    '{ab721a55-1e2f-11d0-9819-00aa0040529b}',  // kaSendTo
    '{ab721a56-1e2f-11d0-9819-00aa0040529b}',  // kaReceiveAs
    '{c7407360-20bf-11d0-a768-00aa006e0529}',  // kaDomainPassword
    '{5805bc62-bdc9-4428-a5e2-856a0f4c185e}',  // kaTerminalServerLicenseServer
    '{0e10c968-78fb-11d2-90d4-00c04f79dc55}',  // kaCertificateEnrollment
    '{b7c69e6d-2cc7-11d2-854e-00a0c983f608}',  // kaTokenGroups
    '{00299570-246d-11d0-a768-00aa006e0529}',  // kaUserForceChangePassword
    '{68b1d179-0d15-4d4f-ab71-46152e79a7bc}',  // kaAllowedToAuthenticate
    '{ccc2dc7d-a6ad-4a7a-8846-c04e3cc53501}',  // kaUnexpirePassword
    '{bf967a7f-0de6-11d0-a285-00aa003049e2}',  // kaX509Cert
    '{bf967a86-0de6-11d0-a285-00aa003049e2}',  // kaComputer
    '{77b5b886-944a-11d1-aebd-0000f80367c1}',  // kaPersonalInformation
    '{f3a64788-5306-11d1-a9c5-0000f80367c1}',  // kaServicePrincipalName
    '{edacfd8f-ffb3-11d1-b41d-00a0c968f939}',  // kaApplyGroupPolicy
    '{ea1b7b93-5e48-46d5-bc6c-4df4fda78a35}',  // kaMsTpmInformationForComputer
    '{bf967a9c-0de6-11d0-a285-00aa003049e2}',  // kaGroup
    '{9b026da6-0d3c-465c-8bee-5199d7165cba}',  // kaDsValidatedWriteComputer
    '{bf967aa8-0de6-11d0-a285-00aa003049e2}',  // kaPrintQueue
    '{440820ad-65b4-11d1-a3da-0000f875ae0d}',  // kaAddGuid
    '{e45795b2-9455-11d1-aebd-0000f80367c1}',  // kaEmailInformation
    '{e45795b3-9455-11d1-aebd-0000f80367c1}',  // kaWebInformation
    '{bf967aba-0de6-11d0-a285-00aa003049e2}',  // kaUser
    '{3e0abfd0-126a-11d0-a060-00aa006c33ed}',  // kaSamAccountName
    '{91e647de-d96f-4b70-9557-d63ff4f3ccd8}',  // kaPrivateInformation
    '{3f78c3e5-f79a-46bd-a0b8-9d18116ddc79}',  // kaMsDsAllowedToActOnBehalfOfOtherIdentity
    '{037088f8-0ae1-11d2-b422-00a0c968f939}'); // kaRasInformation

  /// the official ldapDisplayName of our known Active Directory schema attributes
  ATTR_TXT: array[TAdsKnownAttribute] of RawUtf8 = (
    '',                                        // kaNull
    'User-Account-Restrictions',               // kaUserAccountRestrictions
    'ms-DS-Key-Credential-Link',               // kaMsDsKeyCredentialLink
    'User-Login',                              // kaUserLogon
    'inetOrgPerson',                           // kaInetOrgPerson
    'Terminal-Server',                         // kaTerminalServer
    'Token-Groups-Global-And-Universal',       // kaTokenGroupsGlobalAndUniversal
    'DS-Set-Owner',                            // kaDsSetOwner
    'MemberShip',                              // kaMemberShip
    'General-Information',                     // kaGeneralInformation
    'DNS-Host-Name',                           // kaDnsHostName
    'Description',                             // kaDescription
    'Domain-Administer-Server',                // kaDomainAdministerServer
    'User-Change-Password',                    // kaUserChangePassword
    'Display-Name',                            // kaDisplayName
    'Public-Information',                      // kaPublicInformation
    'Send-As',                                 // kaSendAs
    'Send-To',                                 // kaSendTo
    'Receive-As',                              // kaReceiveAs
    'Domain-Password',                         // kaDomainPassword
    'Terminal-Server-License-Server',          // kaTerminalServerLicenseServer
    'Certificate-Enrollment',                  // kaCertificateEnrollment
    'Token-Groups',                            // kaTokenGroups
    'User-Force-Change-Password',              // kaUserForceChangePassword
    'Allowed-To-Authenticate',                 // kaAllowedToAuthenticate
    'Unexpire-Password',                       // kaUnexpirePassword
    'X509-Cert',                               // kaX509Cert
    'Computer',                                // kaComputer
    'Personal-Information',                    // kaPersonalInformation
    'Service-Principal-Name',                  // kaServicePrincipalName
    'Apply-Group-Policy',                      // kaApplyGroupPolicy
    'ms-TPM-Tpm-Information-For-Computer',     // kaMsTpmInformationForComputer
    'Group',                                   // kaGroup
    'DS-Validated-Write-Computer',             // kaDsValidatedWriteComputer
    'Print-Queue',                             // kaPrintQueue
    'Add-GUID',                                // kaAddGuid
    'Email-Information',                       // kaEmailInformation
    'Web-Information',                         // kaWebInformation
    'User',                                    // kaUser
    'SAM-Account-Name',                        // kaSamAccountName
    'Private-Information',                     // kaPrivateInformation
    'ms-DS-Allowed-To-Act-On-Behalf-Of-Other-Identity',  // kaMsDsAllowedToActOnBehalfOfOtherIdentity
    'RAS-Information');                        // kaRasInformation

/// search a known AD schema attribute from its ObjectID
// - is implemented via O(log(n)) binary search within ordered ATTR_UUID[]
// - returns kaNull if the supplied TGuid was not found
function UuidToKnownAttribute(const u: TGuid): TAdsKnownAttribute;

/// recognize the ldapDisplayName of our TAdsKnownAttribute selection
// - use FindNonVoidRawUtf8I(ATTR_TXT[]) O(n) case-insensitive brute force search
// - returns kaNull if the supplied text does not match any known ldapDisplayName
function TextToKnownAttribute(p: PUtf8Char; len: TStrLen): TAdsKnownAttribute;

/// append an ObjectID as TAdsKnownAttribute's ldapDisplayName, or as UUID hexa
// - if u is a TAdsKnownAttribute, append its ATTR_TXT[] text
// - otherwise, append regular '3f2504e0-4f89-11d3-9a0c-0305e82c3301' text
// - can be used as TAppendShortUuid optional parameter for SDDL generation
// functions, e.g. SecurityDescriptorToText()
// - you can also define your own TAppendShortUuid function
// - use fast O(log(n)) binary search in CPU L1 cache over ATTR_UUID[] items
procedure AppendShortKnownUuid(const u: TGuid; var s: ShortString);

/// parse an ObjectID, recognizing TAdsKnownAttribute's ldapDisplayName or UUID hexa
// - can be used as TShortToUuid optional parameter for SDDL parsing
// - you can also define your own TShortToUuidfunction
// - use O(n) case-insensitive brute force search over ATTR_TXT[] values
function ShortToKnownUuid(const text: ShortString; out uuid: TGuid): boolean;


{ ****************** Security Descriptor Definition Language (SDDL) }

/// parse a SID from its SDDL text form into its binary buffer
// - recognize TWellKnownSid SDDL identifiers, e.g. 'WD' into S-1-1-0 (wksWorld)
// - with optional TWellKnownRid recognition if dom binary is supplied, e.g.
// 'DA' identifier into S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins)
function SddlNextSid(var p: PUtf8Char; var sid: RawSid; dom: PSid): boolean; overload;

/// parse a SID from its SDDL text form into its static binary buffer
function SddlNextSid(var p: PUtf8Char; out sid: TSid; dom: PSid): boolean; overload;

/// append a binary SID as SDDL text form
// - recognize TWellKnownSid into SDDL text, e.g. S-1-1-0 (wksWorld) into 'WD'
// - with optional TWellKnownRid recognition if dom binary is supplied, e.g.
// S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
procedure SddlAppendSid(var s: ShortString; sid, dom: PSid);

/// parse a TSecAce.Mask 32-bit value from its SDDL text
function SddlNextMask(var p: PUtf8Char; var mask: TSecAccessMask): boolean;

/// append a TSecAce.Mask 32-bit value as SDDL text form
procedure SddlAppendMask(var s: ShortString; mask: TSecAccessMask);

/// parse a TSecAce.Opaque value from its SDDL text
// - use a TAceTextTree for the SDDL parsing
function SddlNextOpaque(var p: PUtf8Char; var ace: TSecAce): TAceTextParse;

/// append a binary ACE literal or attribute as SDDL text form
// - see [MS-DTYP] 2.5.1.2.2 @Prefixed Attribute Name Form and
// [MS-DTYP] 2.5.1.1 SDDL Syntax
// - returns true on success, false if the input is not (yet) supported
function SddlAppendOperand(var s: ShortString; v: PRawAceOperand; dom: PSid): boolean;

/// return the SDDL text form of a binary ACE literal or attribute
// - just a wrapper around SddlAppendLiteral()
procedure SddlOperandToText(v: PRawAceOperand; dom: PSid; out u: RawUtf8);

/// return the SDDL text form of a binary ACE unary operator
// - and free the l supplied variable
procedure SddlUnaryToText(tok: TSecConditionalToken; var l, u: RawUtf8);

/// return the SDDL text form of a binary ACE binary operator
// - and free the l and r supplied variables
procedure SddlBinaryToText(tok: TSecConditionalToken; var l, r, u: RawUtf8);

/// append a TSecAce.Opaque value as SDDL text form
// - use a TAceBinaryTree for the binary to SDDL processing
procedure SddlAppendOpaque(var s: TSynTempBuffer; const ace: TSecAce; dom: PSid);

/// parse the next conditional ACE token from its SDDL text
// - see [MS-DTYP] 2.5.1.1 SDDL Syntax
// - identify some internal "fake" tokens >= sctInternalFinal
function SddlNextOperand(var p: PUtf8Char): TSecConditionalToken;


// defined for unit tests only
function KnownSidToSddl(wks: TWellKnownSid): RawUtf8;
function KnownRidToSddl(wkr: TWellKnownRid): RawUtf8;
function SddlToKnownSid(const sddl: RawUtf8; out wks: TWellKnownSid): boolean;
function SddlToKnownRid(const sddl: RawUtf8; out wkr: TWellKnownRid): boolean;

const
  { SDDL standard identifiers using string[3] for efficient 32-bit alignment }

  /// define how ACE kinds in TSecAce.AceType are stored as SDDL
  SAT_SDDL: array[TSecAceType] of string[3] = (
    '',    // satUnknown
    'A',   // satAccessAllowed
    'D',   // satAccessDenied
    'AU',  // satAudit
    'AL',  // satAlarm
    '',    // satCompoundAllowed
    'OA',  // satObjectAccessAllowed
    'OD',  // satObjectAccessDenied
    'OU',  // satObjectAudit
    'OL',  // satObjectAlarm
    'XA',  // satCallbackAccessAllowed
    'XD',  // satCallbackAccessDenied
    'ZA',  // satCallbackObjectAccessAllowed
    '',    // satCallbackObjectAccessDenied
    'XU',  // satCallbackAudit
    '',    // satCallbackAlarm
    '',    // satCallbackObjectAudit
    '',    // satCallbackObjectAlarm
    'ML',  // satMandatoryLabel
    'RA',  // satResourceAttribute
    'SP',  // satScoppedPolicy
    'TL',  // satProcessTrustLabel
    'FL'); // satAccessFilter

  /// define how ACE flags in TSecAce.Flags are stored as SDDL
  SAF_SDDL: array[TSecAceFlag] of string[3] = (
    'OI',  // safObjectInherit
    'CI',  // safContainerInherit
    'NP',  // safNoPropagateInherit
    'IO',  // safInheritOnly
    'ID',  // safInherited
    '',    // saf5
    'SA',  // safSuccessfulAccess
    'FA'); // safFailedAccess

  /// define how ACE access rights bits in TSecAce.Mask are stored as SDDL
  SAM_SDDL: array[TSecAccess] of string[3] = (
    'CC',  // samCreateChild
    'DC',  // samDeleteChild
    'LC',  // samListChildren
    'SW',  // samSelfWrite
    'RP',  // samReadProp
    'WP',  // samWriteProp
    'DT',  // samDeleteTree
    'LO',  // samListObject
    'CR',  // samControlAccess
    '',    // sam9
    '',    // sam10
    '',    // sam11
    '',    // sam12
    '',    // sam13
    '',    // sam14
    '',    // sam15
    'SD',  // samDelete
    'RC',  // samReadControl
    'WD',  // samWriteDac
    'WO',  // samWriteOwner
    '',    // samSynchronize
    '',    // sam21
    '',    // sam22
    '',    // sam23
    '',    // samAccessSystemSecurity
    '',    // samMaximumAllowed
    '',    // sam26
    '',    // sam27
    'GA',  // samGenericAll
    'GX',  // samGenericExecute
    'GW',  // samGenericWrite
    'GR'); // samGenericRead

  /// define how full 32-bit ACE access rights in TSecAce.Mask are stored as SDDL
  SAR_SDDL: array[TSecAccessRight] of string[3] = (
    'FA',  //  sarFileAll
    'FR',  //  sarFileRead
    'FW',  //  sarFileWrite
    'FX',  //  sarFileExecute
    'KA',  //  sarKeyAll
    'KR',  //  sarKeyRead
    'KW',  //  sarKeyWrite
    'KX'); //  sarKeyExecute

  /// define how a sctAttribute is stored as SDDL
  ATTR_SDDL: array[sctLocalAttribute .. sctDeviceAttribute] of string[10] = (
   '',            // sctLocalAttribute
   '@User.',      // sctUserAttribute
   '@Resource.',  // sctResourceAttribute
   '@Device.');   // sctDeviceAttribute

  /// lookup of sctOperator tokens, for SDDL_OPER_TXT[] and SDDL_OPER_INDEX[]
  SDDL_OPER: array[1 .. 23] of TSecConditionalToken = (
    sctEqual,
    sctNotEqual,
    sctLessThan,
    sctLessThanOrEqual,
    sctGreaterThan,
    sctGreaterThanOrEqual,
    sctContains,
    sctExists,
    sctAnyOf,
    sctMemberOf,
    sctDeviceMemberOf,
    sctMemberOfAny,
    sctDeviceMemberOfAny,
    sctNotExists,
    sctNotContains,
    sctNotAnyOf,
    sctNotMemberOf,
    sctNotDeviceMemberOf,
    sctNotMemberOfAny,
    sctNotDeviceMemberOfAny,
    sctAnd,
    sctOr,
    sctNot);

  /// define how a sctOperator is stored as SDDL
  // - used e.g. as SDDL_OPER_TXT[SDDL_OPER_INDEX[v^.Token]]
  // - see [MS-DTYPE] 2.4.4.17.6 and 2.4.4.17.7
  SDDL_OPER_TXT: array[0 .. high(SDDL_OPER)] of RawUtf8 = (
    '',
    '==',                       // sctEqual
    '!=',                       // sctNotEqual
    '<',                        // sctLessThan
    '<=',                       // sctLessThanOrEqual
    '>',                        // sctGreaterThan
    '>=',                       // sctGreaterThanOrEqual
    'Contains',                 // sctContains = 7
    'Exists',                   // sctExists
    'Any_of',                   // sctAnyOf
    'Member_of',                // sctMemberOf
    'Device_Member_of',         // sctDeviceMemberOf
    'Member_of_Any',            // sctMemberOfAny
    'Device_Member_of_Any',     // sctDeviceMemberOfAny
    'Not_Exists',               // sctNotExists
    'Not_Contains',             // sctNotContains
    'Not_Any_of',               // sctNotAnyOf
    'Not_Member_of',            // sctNotMemberOf
    'Not_Device_Member_of',     // sctNotDeviceMemberOf
    'Not_Member_of_Any',        // sctNotMemberOfAny
    'Not_Device_Member_of_Any', // sctNotDeviceMemberOfAny = 20
    ' && ',                     // sctAnd
    ' || ',                     // sctOr
    '!');                       // sctNot

  /// used during SDDL parsing to stop identifier names
  SDDL_END_IDENT = [#0 .. ' ', '=', '!', '<', '>', '{', '(', ')'];

  /// lookup table of SDDL two-chars identifiers over TWellKnownSid
  SDDL_WKS: array[1..48] of TWellKnownSid = (
    wksWorld,                                     // S-1-1-0       WD
    wksCreatorOwner,                              // S-1-3-0       CO
    wksCreatorGroup,                              // S-1-3-1       CG
    wksCreatorOwnerRights,                        // S-1-3-4       OW
    wksIntegrityLow,                              // S-1-16-4096   LW
    wksIntegrityMedium,                           // S-1-16-8192   ME
    wksIntegrityMediumPlus,                       // S-1-16-8448   MP
    wksIntegrityHigh,                             // S-1-16-12288  HI
    wksIntegritySystem,                           // S-1-16-16384  SI
    wksAuthenticationServiceAsserted,             // S-1-18-2      SS
    wksNetwork,                                   // S-1-5-2       NU
    wksInteractive,                               // S-1-5-4       IU
    wksService,                                   // S-1-5-6       SU
    wksAnonymous,                                 // S-1-5-7       AN
    wksEnterpriseControllers,                     // S-1-5-9       ED
    wksSelf,                                      // S-1-5-10      PS
    wksAuthenticatedUser,                         // S-1-5-11      AU
    wksRestrictedCode,                            // S-1-5-12      RC
    wksLocalSystem,                               // S-1-5-18      SY
    wksLocalService,                              // S-1-5-19      LS
    wksNetworkService,                            // S-1-5-20      NS
    wksBuiltinAdministrators,                     // S-1-5-32-544  BA
    wksBuiltinUsers,                              // S-1-5-32-545  BU
    wksBuiltinGuests,                             // S-1-5-32-546  BG
    wksBuiltinPowerUsers,                         // S-1-5-32-547  PU
    wksBuiltinAccountOperators,                   // S-1-5-32-548  AO
    wksBuiltinSystemOperators,                    // S-1-5-32-549  SO
    wksBuiltinPrintOperators,                     // S-1-5-32-550  PO
    wksBuiltinBackupOperators,                    // S-1-5-32-551  BO
    wksBuiltinReplicator,                         // S-1-5-32-552  RE
    wksBuiltinPreWindows2000CompatibleAccess,     // S-1-5-32-554  RU
    wksBuiltinRemoteDesktopUsers,                 // S-1-5-32-555  RD
    wksBuiltinNetworkConfigurationOperators,      // S-1-5-32-556  NO
    wksBuiltinPerfMonitoringUsers,                // S-1-5-32-558  MU
    wksBuiltinPerfLoggingUsers,                   // S-1-5-32-559  LU
    wksBuiltinIUsers,                             // S-1-5-32-568  IS
    wksBuiltinCryptoOperators,                    // S-1-5-32-569  CY
    wksBuiltinEventLogReadersGroup,               // S-1-5-32-573  ER
    wksBuiltinCertSvcDComAccessGroup,             // S-1-5-32-574  CD
    wksBuiltinRdsRemoteAccessServers,             // S-1-5-32-575  RA
    wksBuiltinRdsEndpointServers,                 // S-1-5-32-576  ES
    wksBuiltinRdsManagementServers,               // S-1-5-32-577  MS
    wksBuiltinHyperVAdmins,                       // S-1-5-32-578  HA
    wksBuiltinAccessControlAssistanceOperators,   // S-1-5-32-579  AA
    wksBuiltinRemoteManagementUsers,              // S-1-5-32-580  RM
    wksBuiltinWriteRestrictedCode,                // S-1-5-33      WR
    wksBuiltinUserModeDriver,                     // S-1-5-84-0-0-0-0-0 UD
    wksBuiltinAnyPackage);                        // S-1-15-2-1    AC

  /// lookup table of SDDL two-chars identifiers over TWellKnownRid
  SDDL_WKR: array[1..18] of TWellKnownRid = (
    wkrGroupReadOnly,                // RO
    wkrUserAdmin,                    // LA
    wkrUserGuest,                    // LG
    wkrGroupAdmins,                  // DA
    wkrGroupUsers,                   // DU
    wkrGroupGuests,                  // DG
    wkrGroupComputers,               // DC
    wkrGroupControllers,             // DD
    wkrGroupCertAdmins,              // CA
    wkrGroupSchemaAdmins,            // SA
    wkrGroupEntrepriseAdmins,        // EA
    wkrGroupPolicyAdmins,            // PA
    wkrGroupCloneableControllers,    // CN
    wkrGroupProtectedUsers,          // AP
    wkrGroupKeyAdmins,               // KA
    wkrGroupEntrepriseKeyAdmins,     // EK
    wrkGroupRasServers,              // RS
    wrkUserModeHwOperator);          // HO

{
 Here is the current list of all SID known identifiers, matching the reference
   from https://learn.microsoft.com/en-us/windows/win32/secauthz/sid-strings
 (table generated by uncommenting a ConsoleWrite() in TTestCoreBase._SDDL tests)

   AA = wksBuiltinAccessControlAssistanceOperators
   AC = wksBuiltinAnyPackage
   AN = wksAnonymous
   AO = wksBuiltinAccountOperators
   AP = wkrGroupProtectedUsers
   AU = wksAuthenticatedUser
   BA = wksBuiltinAdministrators
   BG = wksBuiltinGuests
   BO = wksBuiltinBackupOperators
   BU = wksBuiltinUsers
   CA = wkrGroupCertAdmins
   CD = wksBuiltinCertSvcDComAccessGroup
   CG = wksCreatorGroup
   CN = wkrGroupCloneableControllers
   CO = wksCreatorOwner
   CY = wksBuiltinCryptoOperators
   DA = wkrGroupAdmins
   DC = wkrGroupComputers
   DD = wkrGroupControllers
   DG = wkrGroupGuests
   DU = wkrGroupUsers
   EA = wkrGroupEntrepriseAdmins
   ED = wksEnterpriseControllers
   EK = wkrGroupEntrepriseKeyAdmins
   ER = wksBuiltinEventLogReadersGroup
   ES = wksBuiltinRdsEndpointServers
   HA = wksBuiltinHyperVAdmins
   HI = wksIntegrityHigh
   HO = wrkUserModeHwOperator
   IS = wksBuiltinIUsers
   IU = wksInteractive
   KA = wkrGroupKeyAdmins
   LA = wkrUserAdmin
   LG = wkrUserGuest
   LS = wksLocalService
   LU = wksBuiltinPerfLoggingUsers
   LW = wksIntegrityLow
   ME = wksIntegrityMedium
   MP = wksIntegrityMediumPlus
   MS = wksBuiltinRdsManagementServers
   MU = wksBuiltinPerfMonitoringUsers
   NO = wksBuiltinNetworkConfigurationOperators
   NS = wksNetworkService
   NU = wksNetwork
   OW = wksCreatorOwnerRights
   PA = wkrGroupPolicyAdmins
   PO = wksBuiltinPrintOperators
   PS = wksSelf
   PU = wksBuiltinPowerUsers
   RA = wksBuiltinRdsRemoteAccessServers
   RC = wksRestrictedCode
   RD = wksBuiltinRemoteDesktopUsers
   RE = wksBuiltinReplicator
   RM = wksBuiltinRemoteManagementUsers
   RO = wkrGroupReadOnly
   RS = wrkGroupRasServers
   RU = wksBuiltinPreWindows2000CompatibleAccess
   SA = wkrGroupSchemaAdmins
   SI = wksIntegritySystem
   SO = wksBuiltinSystemOperators
   SS = wksAuthenticationServiceAsserted
   SU = wksService
   SY = wksLocalSystem
   UD = wksBuiltinUserModeDriver
   WD = wksWorld
   WR = wksBuiltinWriteRestrictedCode
}

var
  { globals filled during unit initialization from actual code constants }

  /// allow to quickly check if a TWellKnownSid has a SDDL identifier
  wksWithSddl: TWellKnownSids;

  /// allow to quickly check if a TWellKnownRid has a SDDL identifier
  wkrWithSddl: TWellKnownRids;

  /// allow to quickly check if a TSecAccess has a SDDL identifier
  samWithSddl: TSecAccessMask;

  /// O(1) lookup table used e.g. as SDDL_OPER_TXT[SDDL_OPER_INDEX[v^.Token]]
  SDDL_OPER_INDEX: array[TSecConditionalToken] of byte;


{ ****************** TSecurityDescriptor Wrapper Object }

type
  /// custom binary buffer type used as Windows self-relative Security Descriptor
  // - mormot.crypt.secure will recognize this type and serialize its SDDL
  // text as a JSON string
  RawSecurityDescriptor = type RawByteString;
  PRawSecurityDescriptor = ^RawSecurityDescriptor;

  /// define the information stored in a TSecurityDescriptor
  // - maps the TSecurityDescriptor Owner, Group, Dacl[] and Sacl[] properties
  // - also matches OWNER_SECURITY_INFORMATION, GROUP_SECURITY_INFORMATION,
  // DACL_SECURITY_INFORMATION and SACL_SECURITY_INFORMATION WinAPI flags
  TSecurityDescriptorInfo = (
   sdiOwner,
   sdiGroup,
   sdiDacl,
   sdiSacl);

  /// define the extend of information stored in a TSecurityDescriptor
  // - used e.g. for the TSecurityDescriptor.Modified flags
  TSecurityDescriptorInfos = set of TSecurityDescriptorInfo;

  {$A-} // both TSecAce and TSecurityDescriptor should be packed for JSON serialization

  /// high-level cross-platform support of one Windows Security Descriptor
  // - can be loaded and exported as self-relative binary or SDDL text
  // - JSON is supported via SecurityDescriptorToJson() and
  // SecurityDescriptorFromJson() from mormot.crypt.secure
  // - high level wrapper of [MS-DTYP] 2.4.6 SECURITY_DESCRIPTOR
  {$ifdef USERECORDWITHMETHODS}
  TSecurityDescriptor = record
  {$else}
  TSecurityDescriptor = object
  {$endif USERECORDWITHMETHODS}
  private
    function NextAclFromText(var p: PUtf8Char; dom: PSid; uuid: TShortToUuid;
      scope: TSecAceScope): TAceTextParse;
    procedure AclToText(var sddl: TSynTempBuffer; dom: PSid; uuid: TAppendShortUuid;
      scope: TSecAceScope);
    function InternalAdd(scope: TSecAceScope; out acl: PSecAcl): PSecAce;
    function InternalAdded(scope: TSecAceScope; ace: PSecAce; acl: PSecAcl;
      success: boolean): PSecAce;
  public
    /// the owner security identifier (SID)
    Owner: RawSid;
    /// the primary group SID
    Group: RawSid;
    /// discretionary access control list
    Dacl: TSecAcl;
    /// system access control list
    Sacl: TSecAcl;
    /// control flags of this Security Descriptor
    Flags: TSecControls;
    /// which fields have been modified by Add() or other write methods
    // - can then be applied, e.g. on Windows, via SetSystemSecurityDescriptor()
    Modified: TSecurityDescriptorInfos;
    /// remove any previous content
    procedure Clear;
    /// compare the fields of this instance with another
    function IsEqual(const sd: TSecurityDescriptor): boolean;
    /// decode a self-relative binary Security Descriptor buffer
    function FromBinary(p: PByteArray; len: cardinal): boolean; overload;
    /// decode a self-relative binary Security Descriptor buffer
    function FromBinary(const Bin: RawSecurityDescriptor): boolean; overload;
    /// decode a self-relative binary Security Descriptor buffer
    // - this method will parse the binary but won't first validate its input
    // because it has no len to supply to IsValidSecurityDescriptor()
    function FromBinary(p: PByteArray): boolean; overload;
    /// encode this Security Descriptor into a self-relative binary buffer
    function ToBinary: RawSecurityDescriptor;
    /// decode a Security Descriptor from its SDDL textual representation
    // - could also recognize SDDL RID placeholders, with the specified
    // RidDomain in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' text form
    // - recognize ldapDisplayName of TAdsKnownAttribute if uuid=@ShortToKnownUuid
    function FromText(const SddlText: RawUtf8;
      const RidDomain: RawUtf8 = ''; uuid: TShortToUuid = nil): TAceTextParse; overload;
    /// decode a Security Descriptor from its SDDL textual representation
    function FromText(var p: PUtf8Char; dom: PSid = nil; uuid: TShortToUuid = nil;
      endchar: AnsiChar = #0): TAceTextParse; overload;
    /// encode this Security Descriptor into its SDDL textual representation
    // - could also generate SDDL RID placeholders, from the specified
    // RidDomain in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' text form
    // - could also customize UUID values, e.g. with uuid = @AppendShortKnownUuid
    function ToText(const RidDomain: RawUtf8 = '';
      uuid: TAppendShortUuid = nil): RawUtf8;
    /// append this Security Descriptor as SDDL text into an existing buffer
    // - could also generate SDDL RID placeholders, if dom binary is supplied,
    // e.g. S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
    // - could also customize UUID values, e.g. with uuid = @AppendShortKnownUuid
    procedure AppendAsText(var sddl: TSynTempBuffer; dom: PSid = nil;
      uuid: TAppendShortUuid = nil);
    /// add one new ACE to the DACL (or SACL)
    // - SID and Mask are supplied in their regular / SDDL text form, with
    // dom optionally able to recognize SDDL RID placeholders
    // - add to Dacl[] unless scope is sasSacl so it is added to Sacl[]
    // - return nil on sidText/maskSddl parsing error, or the newly added entry
    function Add(sat: TSecAceType; const sidText, maskSddl: RawUtf8;
      dom: PSid = nil; const condExp: RawUtf8 = ''; scope: TSecAceScope = sasDacl;
      saf: TSecAceFlags = []): PSecAce; overload;
    /// add one new ACE to the DACL (or SACL) from SDDL text
    // - dom <> nil would enable SDDL RID placeholders recognition
    // - recognize ldapDisplayName of TAdsKnownAttribute if uuid=@ShortToKnownUuid
    // - add to Dacl[] unless scope is sasSacl so it is added to Sacl[]
    // - return nil on sddl input text parsing error, or the newly added entry
    function Add(const sddl: RawUtf8; dom: PSid = nil; uuid: TShortToUuid = nil;
      scope: TSecAceScope = sasDacl): PSecAce; overload;
    /// delete one ACE from the DACL (or SACL)
    procedure Delete(index: PtrUInt; scope: TSecAceScope = sasDacl);
    /// change all well-known RIDs for a given domain to another
    // - replace only well-known RID < WKR_RID_MAX, not any machine/user ID
    // - just wrap TryDomainTextToSid() and the ReplaceDomainRaw() functions
    // - returns the number of entries changed in the input, including both
    // Owner/Group and nested ACE (and conditionals)
    // - returns -1 if OldDomain/NewDomain are no valid S-1-5-21-xx-xx-xx[-rid]
    function ReplaceDomain(const OldDomain, NewDomain: RawUtf8;
      maxRid: cardinal = WKR_RID_MAX): integer;
    /// change all well-known RIDs for a given domain to another, from raw binary
    function ReplaceDomainRaw(OldDomain, NewDomain: PSid;
      maxRid: cardinal): integer;
    /// change all occurrences of a given SID value
    // - returns the number of entries changed in the input, including both
    // Owner/Group and nested ACE (and conditionals)
    // - returns -1 if OldSid/NewSid are no valid SID text, or with unequal length
    function ReplaceSid(const OldSid, NewSid: RawUtf8): integer; overload;
    /// change all occurrences of one or several given SID value(s)
    // - returns the number of entries changed in the input, or -1 on invalid SID
    // - each and every old/new SID lengths should match for in-place replacement
    // of the Opaque binary content
    function ReplaceSid(const OldSid, NewSid: array of RawUtf8): integer; overload;
    /// change all occurrences of one or several given SID value(s)
    function ReplaceSidRaw(const OldSid, NewSid: RawSidDynArray): integer; overload;
  end;

  {$A+}

/// check the conformity of a self-relative binary Security Descriptor buffer
// - only check the TSecurityDescriptor main fields consistency
function IsValidSecurityDescriptor(p: PByteArray; len: cardinal): boolean;

/// convert a self-relative Security Descriptor buffer as text (SDDL or hexa)
// - will wrap our TSecurityDescriptor binary decoder / SDDL encoder on all platforms
// - returns true if the conversion succeeded
// - returns false, and don't change the text value on rendering error
// - function is able to convert the value itself, i.e. allows @sd = @text
// - could also generate SDDL RID placeholders, if dom binary is supplied,
// e.g. S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
// - you can customize the UUID ACE output e.g. with uuid = @AppendShortKnownUuid
// - on Windows, see also the native CryptoApi.SecurityDescriptorToText()
function SecurityDescriptorToText(const sd: RawSecurityDescriptor;
  var text: RawUtf8; dom: PSid = nil; uuid: TAppendShortUuid = nil): boolean;


{ ****************** Windows API Specific Security Types and Functions }

{$ifdef OSWINDOWS}

type
  /// the SID types, as recognized by LookupSid()
  TSidType = (
    stUndefined,
    stTypeUser,
    stTypeGroup,
    stTypeDomain,
    stTypeAlias,
    stTypeWellKnownGroup,
    stTypeDeletedAccount,
    stTypeInvalid,
    stTypeUnknown,
    stTypeComputer,
    stTypeLabel,
    stTypeLogonSession);

/// return the SID of a given token, nil if none found
// - the returned PSid is located within buf temporary buffer
// - so caller should call buf.Done once this PSid value is not needed any more
function RawTokenSid(tok: THandle; var buf: TSynTempBuffer): PSid;

/// return the group SIDs of a given token, nil if none found
// - the returned PSid is located within buf temporary buffer
// - so caller should call buf.Done once this PSid value is not needed any more
function RawTokenGroups(tok: THandle; var buf: TSynTempBuffer): PSids;

/// return the group SIDs of a given token as text dynamic array
function TokenGroupsText(tok: THandle): TRawUtf8DynArray;

/// check if a group SID is part of a given token
function TokenHasGroup(tok: THandle; sid: PSid): boolean;

/// check if any group SID is part of a given token
function TokenHasAnyGroup(tok: THandle; const sid: RawSidDynArray): boolean;

/// return the SID of the current user, from process or thread, as text
// - e.g. 'S-1-5-21-823746769-1624905683-418753922-1000'
// - optionally returning the name and domain via LookupSid()
function CurrentSid(wtt: TWinTokenType = wttProcess;
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil): RawUtf8;

/// return the SID of the current user, from process or thread, as raw binary
procedure CurrentRawSid(out sid: RawSid; wtt: TWinTokenType = wttProcess;
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil);

/// return the SID of the current user groups, from process or thread, as text
function CurrentGroupsSid(wtt: TWinTokenType = wttProcess): TRawUtf8DynArray;

/// recognize the well-known SIDs from the current user, from process or thread
// - for instance, for an user with administrator rights on Windows, returns
// $ [wksWorld, wksLocal, wksConsoleLogon, wksIntegrityHigh, wksInteractive,
// $  wksAuthenticatedUser, wksThisOrganisation, wksBuiltinAdministrators,
// $  wksBuiltinUsers, wksNtlmAuthentication]
function CurrentKnownGroups(wtt: TWinTokenType = wttProcess): TWellKnownSids;

/// fast check if the current user, from process or thread, has a well-known group SID
// - e.g. CurrentUserHasGroup(wksLocalSystem) returns true for LOCAL_SYSTEM user
function CurrentUserHasGroup(wks: TWellKnownSid;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has a given group SID
function CurrentUserHasGroup(const sid: RawUtf8;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has a given group SID
function CurrentUserHasGroup(sid: PSid;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// fast check if the current user, from process or thread, has any given group SID
function CurrentUserHasAnyGroup(const sid: RawSidDynArray;
  wtt: TWinTokenType = wttProcess): boolean;

/// fast check if the current user, from process or thread, match a group by name
// - calls LookupSid() on each group SID of this user, and filter with name/domain
function CurrentUserHasGroup(const name, domain, server: RawUtf8;
  wtt: TWinTokenType = wttProcess): boolean; overload;

/// just a wrapper around CurrentUserHasGroup(wksBuiltinAdministrators)
function CurrentUserIsAdmin: boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// retrieve the name and domain of a given SID
// - returns stUndefined if the SID could not be resolved by LookupAccountSid()
function LookupSid(sid: PSid; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): TSidType; overload;

/// retrieve the name and domain of a given SID, encoded from text
// - returns stUndefined if the SID could not be resolved by LookupAccountSid()
function LookupSid(const sid: RawUtf8; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): TSidType; overload;

/// retrieve the name and domain of a given Token
function LookupToken(tok: THandle; out name, domain: RawUtf8;
  const server: RawUtf8 = ''): boolean; overload;

/// retrieve the 'domain\name' combined value of a given Token
function LookupToken(tok: THandle; const server: RawUtf8 = ''): RawUtf8; overload;


type
  /// define the kind of resource access by GetFileSecurityDescriptor()
  // - match the SE_OBJECT_TYPE low-level Windows definition
  // - nrtFile is a relative, absolute or UNC file name e.g. 'c:\toto\titi.txt'
  // - nrtService expects a local 'ServiceName' or '\\ComputerName\ServiceName'
  // - nrtPrinter expects a local 'PrinterName' or '\\ComputerName\PrinterName'
  // - nrtRegistryKey expects local 'CURRENT_USER\SomePath', 'MACHINE\SomePath',
  // 'USERS\SomePath' or remote '\\ComputerName\CLASSES_ROOT\SomePath'
  // - nrtNetworkShare is e.g. 'ShareName' or '\\ComputerName\ShareName'
  // - nrtKernelObject are e.g. named semaphore, event, mutex or file mapping
  // - nrtWindowObject is not applicable
  // - nrtDsObject indicates a directory entry, in X.500 form, e.g.
  // 'CN=SomeObject,OU=ou2,OU=ou1,DC=DomainName,DC=CompanyName,DC=com,O=internet'
  // - nrtDsObjectAll indicates a directory entry and all its properties
  // - other fields are less used and somewhat undocumented
  TNamedResourceType = (
    nrtUnknown,
    nrtFile,
    nrtService,
    nrtPrinter,
    nrtRegistryKey,
    nrtNetworkShare,
    nrtKernelObject,
    nrtWindowObject,
    nrtDsObject,
    nrtDsObjectAll,
    nrtProviderDefinedObjet,
    nrtWmiGuidObject,
    nrtWow64RegistryKey32,
    nrtWow64RegistryKey64);

/// retrieve the security descriptor information from a given file or named resource
// - will also set the wspSecurity priviledge, unless privileges is [], e.g. to
// reuse a single TSynWindowsPrivileges.Enable() instance between several calls
function GetSystemSecurityDescriptor(const fn: TFileName;
  out dest: TSecurityDescriptor;
  info: TSecurityDescriptorInfos = [sdiOwner, sdiGroup, sdiDacl];
  kind: TNamedResourceType = nrtFile;
  privileges: TWinSystemPrivileges = [wspSecurity]): boolean;

/// change the security descriptor information of a given file or named resource
// - info covers the extent of dest fields to be updated in the system - its
// default [] value will use dest.Modified flags
// - may require ownership to the resource, or the wspTakeOwnership privilege
// - wspSecurity privilege may also be needed, e.g. for DACL
function SetSystemSecurityDescriptor(const fn: TFileName;
  const dest: TSecurityDescriptor; info: TSecurityDescriptorInfos = [];
  kind: TNamedResourceType = nrtFile;
  privileges: TWinSystemPrivileges = [wspSecurity]): boolean;

{$endif OSWINDOWS}

implementation


{ ****************** Security IDentifier (SID) Definitions }

function SidLength(sid: PSid): PtrInt;
begin
  if sid = nil then
    result := 0
  else
    result := PtrInt(sid^.SubAuthorityCount) shl 2 + SID_MINLEN;
end;

function SidCompare(a, b: PSid): integer;
var
  l: PtrInt;
begin
  l := SidLength(a);
  result := l - SidLength(b);
  if result = 0 then
    result := MemCmp(pointer(a), pointer(b), l);
end;

procedure ToRawSid(sid: PSid; out result: RawSid);
begin
  if sid <> nil then
    FastSetRawByteString(RawByteString(result), sid, SidLength(sid));
end;

procedure SidAppendShort(sid: PSid; var s: ShortString);
var
  a: PSidAuth;
  i: PtrInt;
begin // faster than ConvertSidToStringSidA(), and cross-platform
  if (sid = nil ) or
     (sid^.Revision <> 1) then // invalid SID
    exit;
  a := @sid^.IdentifierAuthority;
  if (a^[0] <> 0) or
     (a^[1] <> 0) then
  begin
    AppendShort('S-1-0x', s);
    for i := 0 to 5 do // hexa 48-bit IdentifierAuthority, i.e. S-1-0x######-..
      AppendShortByteHex(a^[i], s)
  end
  else
  begin
    AppendShort('S-1-', s);
    AppendShortCardinal(bswap32(PCardinal(@a^[2])^), s);
  end;
  for i := 0 to PtrInt(sid^.SubAuthorityCount) - 1 do
  begin
    AppendShortChar('-', @s);
    AppendShortCardinal(sid^.SubAuthority[i], s);
  end;
end;

procedure SidToTextShort(sid: PSid; var result: ShortString);
begin
  result[0] := #0;
  SidAppendShort(sid, result);
end;

function SidToText(sid: PSid): RawUtf8;
begin
  SidToText(sid, result);
end;

procedure SidToText(sid: PSid; var text: RawUtf8);
var
  tmp: ShortString;
begin
  tmp[0] := #0;
  SidAppendShort(sid, tmp);
  FastSetString(text, @tmp[1], ord(tmp[0]));
end;

function SidsToText(sids: PSids): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  SetLength(result, length(sids));
  for i := 0 to length(sids) - 1 do
    result[i] := SidToText(sids[i]);
end;

function IsValidRawSid(const sid: RawSid): boolean;
var
  l: PtrInt;
begin
  l := length(sid) - SID_MINLEN;
  result := (l >= 0) and
            (PtrInt(PSid(sid)^.SubAuthorityCount) shl 2 = l); // SidLength()
end;

function HasSid(const sids: PSids; sid: PSid): boolean;
var
  i: PtrInt;
begin
  result := true;
  if sid <> nil then
    for i := 0 to length(sids) - 1 do
      if SidCompare(sid, sids[i]) = 0 then
        exit;
  result := false;
end;

function HasAnySid(const sids: PSids; const sid: RawSidDynArray): boolean;
var
  i: PtrInt;
begin
  result := true;
  for i := 0 to length(sid) - 1 do
    if HasSid(sids, pointer(sid[i])) then
      exit;
  result := false;
end;

procedure AddRawSid(var sids: RawSidDynArray; sid: PSid);
var
  n: PtrInt;
begin
  if sid = nil then
    exit;
  n := length(sids);
  SetLength(sids, n + 1);
  ToRawSid(sid, sids[n]);
end;

function RawSidToText(const sid: RawSid): RawUtf8;
begin
  if IsValidRawSid(sid) then
    result := SidToText(pointer(sid))
  else
    result := '';
end;

function GetNextUInt32(var P: PUtf8Char): cardinal;
var
  c: cardinal;
begin
  result := 0;
  if P = nil then
    exit;
  repeat
    c := ord(P^) - 48;
    if c > 9 then
      break
    else
      result := result * 10 + c;
    inc(P);
  until false;
  while P^ in ['.', '-', ' '] do
    inc(P);
end;

function TextToSid(var P: PUtf8Char; out sid: TSid): boolean;
begin
  result := false;
  if (P = nil) or
     (PCardinal(P)^ <> SID_REV32) then
    exit;
  inc(P, 4);
  if not (P^ in ['0'..'9']) then
    exit;
  PInt64(@sid)^ := 1; // initialize TSid structure
  PCardinal(@sid.IdentifierAuthority[2])^ := bswap32(GetNextUInt32(P));
  while P^ in ['0'..'9'] do
  begin
    sid.SubAuthority[sid.SubAuthorityCount] := GetNextUInt32(P);
    inc(sid.SubAuthorityCount);
    if sid.SubAuthorityCount = 0 then
      exit; // avoid any overflow
  end;
  result := true; // P^ is likely to be #0
end;

function TextToRawSid(const text: RawUtf8): RawSid;
begin
  TextToRawSid(text, result);
end;

function TextToRawSid(const text: RawUtf8; out sid: RawSid): boolean;
var
  tmp: TSid; // maximum size possible on stack (1032 bytes)
  p: PUtf8Char;
begin
  p := pointer(text);
  result := TextToSid(p, tmp) and (p^ = #0);
  if result then
    ToRawSid(@tmp, sid);
end;

function TextToRawSidArray(const text: array of RawUtf8; out sid: RawSidDynArray): boolean;
var
  i, n: PtrInt;
begin
  n := length(Text);
  SetLength(sid, n);
  for i := 0 to n - 1 do
    if not TextToRawSid(text[i], sid[i]) then
    begin
      sid := nil;
      result := false;
      exit;
    end;
  result := true;
end;

function SidIsDomain(s: PSid): boolean;
begin
  result := (s <> nil) and
            ((PCardinal(s)^ = SID_DOM_MASKRID) or   // SubAuthorityCount in [4,5]
             (PCardinal(s)^ = SID_DOM_MASKSID)) and // to allow domain or rid
            (s^.SubAuthority[0] = 21) and
            (PCardinalArray(s)[1] = SID_DOM_MASKAUT);
end;

function TryDomainTextToSid(const Domain: RawUtf8; out Dom: RawSid): boolean;
var
  tmp: TSid;
  p: PUtf8Char;
begin
  result := true;
  if Domain = '' then
    exit; // no Domain involved: continue
  p := pointer(Domain);
  result := TextToSid(p, tmp) and // expects S-1-5-21-xx-xx-xx[-rid]
            (p^ = #0) and
            SidIsDomain(@tmp);
  if not result then
    exit; // this Domain text is no valid domain SID
  tmp.SubAuthorityCount := 5; // reserve place for WKR_RID[wkr] trailer
  tmp.SubAuthority[4] := 0;
  ToRawSid(@tmp, Dom);        // output Dom as S-1-5-21-xx-xx-xx-RID
end;

function SidSameDomain(sid, dom: PSid): boolean;
begin
  result := (PCardinal(sid)^ = SID_DOM_MASKRID) and         // rid
            (PCardinalArray(sid)[1] = SID_DOM_MASKAUT) and  // S-1-5
            (PInt64Array(sid)[1] = PInt64Array(dom)[1]) and // auth[0..1]
            (PInt64Array(sid)[2] = PInt64Array(dom)[2]);    // auth[2..3]
            // so sid.SubAuthority[4] will contain any RID
end;

function SidReplaceDomain(OldDomain, NewDomain: PSid; maxRid: cardinal;
  var Sid: RawSid): integer;
begin
  if (length(Sid) <> SID_RIDLEN) or
     not SidSameDomain(pointer(Sid), OldDomain) or
     (PSid(Sid)^.SubAuthority[4] > maxRid) then
  begin
    result := 0;
    exit;
  end;
  if GetRefCount(Sid) <> 1 then
    UniqueString(AnsiString(Sid)); // paranoid
  MoveFast(pointer(NewDomain)^, pointer(Sid)^, SID_DOMAINLEN); // overwrite
  result := 1;
end;

function SidReplaceAny(const OldSid, NewSid: RawSidDynArray; var Sid: RawSid): integer;
var
  i: PtrInt;
begin
  for i := 0 to length(OldSid) - 1 do
    if OldSid[i] = Sid then
    begin
      Sid := NewSid[i];
      result := 1;
      exit;
    end;
  result := 0;
end;

function SidReplaceAny(const OldSid, NewSid: RawSidDynArray;
  var Sid: TSid; SidLen: PtrInt): integer;
var
  i: PtrInt;
begin
  for i := 0 to length(OldSid) - 1 do
    {$ifdef CPUX64}
    if MemCmp(pointer(OldSid[i]), @Sid, SidLen) = 0 then // use SSE2 asm
    {$else}
    if CompareMem(pointer(OldSid[i]), @Sid, SidLen) then
    {$endif CPUX64}
    begin
      MoveFast(pointer(NewSid[i])^, Sid, SidLen); // overwrite
      result := 1;
      exit;
    end;
  result := 0;
end;

var
  KNOWN_SID_SAFE: TLightLock; // lighter than GlobalLock/GlobalUnLock
  KNOWN_SID: array[TWellKnownSid] of RawSid;
  KNOWN_SID_TEXT: array[TWellKnownSid] of string[15];

const
  INTEGRITY_SID: array[0..7] of word = ( // S-1-16-x known values
    0, 4096, 8192, 8448, 12288, 16384, 20480, 28672);

procedure ComputeKnownSid(wks: TWellKnownSid);
var
  sid: TSid;
begin
  PInt64(@sid)^ := $0101; // sid.Revision=1, sid.SubAuthorityCount=1
  if wks <= wksLocal then
  begin // S-1-1-0
    sid.IdentifierAuthority[5] := ord(wks);
    sid.SubAuthority[0] := 0;
  end
  else if wks = wksConsoleLogon then
  begin // S-1-2-1
    sid.IdentifierAuthority[5] := 2;
    sid.SubAuthority[0] := 1;
  end
  else if wks <= wksCreatorOwnerRights then
  begin // S-1-3-0
    sid.IdentifierAuthority[5] := 3;
    sid.SubAuthority[0] := ord(wks) - ord(wksCreatorOwner);
  end
  else if wks <= wksIntegritySecureProcess then
  begin
    sid.IdentifierAuthority[5] := 16; // S-1-16-x
    sid.SubAuthority[0] := INTEGRITY_SID[ord(wks) - ord(wksIntegrityUntrusted)];
  end
  else if wks <= wksAuthenticationKeyPropertyAttestation then
  begin // S-1-18-1
    sid.IdentifierAuthority[5] := 18;
    sid.SubAuthority[0] := ord(wks) - (ord(wksAuthenticationAuthorityAsserted) - 1)
  end
  else
  begin // S-1-5-x
    sid.IdentifierAuthority[5] := 5;
    if wks = wksNtAuthority then
      sid.SubAuthorityCount := 0
    else if wks <= wksInteractive then
      sid.SubAuthority[0] := ord(wks) - ord(wksNtAuthority)
    else if wks <= wksThisOrganisation then
      sid.SubAuthority[0] := ord(wks) - (ord(wksNtAuthority) - 1)
    else if wks <= wksNetworkService then
      sid.SubAuthority[0] := ord(wks) - (ord(wksNtAuthority) - 2)
    else if wks <= wksLocalAccountAndAdministrator then //  S-1-5-113
      sid.SubAuthority[0] := ord(wks) - (ord(wksLocalAccount) - 113)
    else if wks = wksBuiltinWriteRestrictedCode then
      sid.SubAuthority[0] := 33
    else if wks = wksBuiltinUserModeDriver then // S-1-5-84-0-0-0-0-0
    begin
      sid.SubAuthorityCount := 6;
      sid.SubAuthority[0] := 84;
      sid.SubAuthority[1] := 0;
      sid.SubAuthority[2] := 0;
      sid.SubAuthority[3] := 0;
      sid.SubAuthority[4] := 0;
      sid.SubAuthority[5] := 0;
    end
    else
    begin
      sid.SubAuthority[0] := 32;
      if wks <> wksBuiltinDomain then
      begin
        sid.SubAuthorityCount := 2;
        if wks <= wksBuiltinDcomUsers then
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinAdministrators) - 544)
        else if wks <= wksBuiltinDeviceOwners then // S-1-5-32-583
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinIUsers) - 568)
        else if wks <= wksCapabilityContacts then
        begin // S-1-15-3-1
          sid.IdentifierAuthority[5] := 15;
          sid.SubAuthority[0] := 3;
          sid.SubAuthority[1] := ord(wks) - (ord(wksCapabilityInternetClient) - 1)
        end
        else if wks <= wksBuiltinAnyRestrictedPackage then
        begin // S-1-15-2-1
          sid.IdentifierAuthority[5] := 15;
          sid.SubAuthority[0] := 2;
          sid.SubAuthority[1] := ord(wks) - (ord(wksBuiltinAnyPackage) - 1)
        end
        else if wks <= wksDigestAuthentication then
        begin
          sid.SubAuthority[0] := 64;
          case wks of
            wksNtlmAuthentication:
              sid.SubAuthority[1] := 10; // S-1-5-64-10
            wksSChannelAuthentication:
              sid.SubAuthority[1] := 14;
            wksDigestAuthentication:
              sid.SubAuthority[1] := 21;
          end;
        end;
      end;
    end;
  end;
  KNOWN_SID_SAFE.Lock;
  if KNOWN_SID[wks] = '' then
  begin
    SidToTextShort(@sid, KNOWN_SID_TEXT[wks]);
    ToRawSid(@sid, KNOWN_SID[wks]); // to be set last
  end;
  KNOWN_SID_SAFE.UnLock;
end;

function KnownRawSid(wks: TWellKnownSid): RawSid;
begin
  KnownRawSid(wks, result);
end;

procedure KnownRawSid(wks: TWellKnownSid; var sid: RawSid);
begin
  if (wks <> wksNull) and
     (KNOWN_SID[wks] = '') then
    ComputeKnownSid(wks);
  sid := KNOWN_SID[wks];
end;

procedure KnownRawSid(wks: TWellKnownSid; var sid: TSid);
var
  s: PSid;
begin
  if (wks <> wksNull) and
     (KNOWN_SID[wks] = '') then
    ComputeKnownSid(wks);
  s := pointer(KNOWN_SID[wks]);
  MoveFast(s^, sid, SidLength(s));
end;

function KnownSidToText(wks: TWellKnownSid): PShortString;
begin
  if (wks <> wksNull) and
     (KNOWN_SID[wks] = '') then
    ComputeKnownSid(wks);
  result := @KNOWN_SID_TEXT[wks];
end;

// https://learn.microsoft.com/en-us/windows/win32/secauthz/well-known-sids
// https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/81d92bba-d22b-4a8c-908a-554ab29148ab

function SidToKnown(sid: PSid): TWellKnownSid;
var
  c: integer;
begin
  result := wksNull; // not recognized
  if (sid = nil) or
     (sid.Revision <> 1) or
     (PCardinal(@sid.IdentifierAuthority)^ <> 0) or
     (sid.IdentifierAuthority[4] <> 0) then
    exit;
  case sid.SubAuthorityCount of // very fast O(1) SID binary recognition
    0:
      if sid.IdentifierAuthority[5] = 5 then
        result := wksNtAuthority; // S-1-5
    1:
      begin
        c := sid.SubAuthority[0];
        case sid.IdentifierAuthority[5] of
          1:
            if c = 0 then
              result := wksWorld; // S-1-1-0
          2:
            if c in [0 .. 1] then // S-1-2-x
              result := TWellKnownSid(ord(wksLocal) + c);
          3:
            if c in [0 .. 4] then // S-1-3-x
              result := TWellKnownSid(ord(wksCreatorOwner) + c);
          5:
            case c of // S-1-5-x
              1 .. 4:
                result := TWellKnownSid((ord(wksDialup) - 1) + c);
              6 .. 15:
                result := TWellKnownSid((ord(wksService) - 6) + c);
              17 .. 20:
                result := TWellKnownSid((ord(wksIisUser) - 17) + c);
              32:
                result := wksBuiltinDomain;
              33:
                result := wksBuiltinWriteRestrictedCode;
              113 .. 114:
                result := TWellKnownSid(integer(ord(wksLocalAccount) - 113) + c);
            end;
          16:
            begin // S-1-16-x
              c := WordScanIndex(@INTEGRITY_SID, length(INTEGRITY_SID), c);
              if c >= 0 then
                result := TWellKnownSid(ord(wksIntegrityUntrusted) + c);
            end;
          18:
            if c in [1 .. 6] then // S-1-18-x
              result :=
                TWellKnownSid((ord(wksAuthenticationAuthorityAsserted) - 1) + c);
        end;
      end;
    2:
      begin
        c := sid.SubAuthority[1];
        case sid.IdentifierAuthority[5] of
          5:
            case sid.SubAuthority[0] of
              32: // S-1-5-32-544
                case c of
                  544 .. 562:
                    result := TWellKnownSid(ord(wksBuiltinAdministrators) + c - 544);
                  568 .. 583:
                    result := TWellKnownSid(ord(wksBuiltinIUsers) + c - 568);
                end;
              64: // S-1-5-64-10
                case c of
                  10:
                    result := wksNtlmAuthentication;
                  14:
                    result := wksSChannelAuthentication;
                  21:
                    result := wksDigestAuthentication;
                end;
            end;
          15:
            case sid.SubAuthority[0] of
              2:
                if c in [1 .. 2] then // S-1-15-2-x
                  result := TWellKnownSid(ord(pred(wksBuiltinAnyPackage)) + c);
              3:
                if c in [1 .. 12] then // S-1-15-3-x
                  result := TWellKnownSid(ord(pred(wksCapabilityInternetClient)) + c);
            end;
        end;
      end;
    6:
      case sid.SubAuthority[0] of
        84:
          if (sid.SubAuthority[1] = 0) and
             (sid.SubAuthority[2] = 0) and
             (sid.SubAuthority[3] = 0) and
             (sid.SubAuthority[4] = 0) and
             (sid.SubAuthority[5] = 0) then
           result := wksBuiltinUserModeDriver; // S-1-5-84-0-0-0-0-0
      end;
  end;
end;

function SidToKnown(const text: RawUtf8): TWellKnownSid;
var
  sid: TSid;
  p: PUtf8Char;
begin
  p := pointer(text);
  if TextToSid(p, sid) and (p^ = #0) then
    result := SidToKnown(@sid)
  else
    result := wksNull;
end;

function SidToKnownGroups(const sids: PSids): TWellKnownSids;
var
  k: TWellKnownSid;
  i: PtrInt;
begin
  result := [];
  for i := 0 to length(sids) - 1 do
  begin
    k := SidToKnown(sids[i]);
    if k <> wksNull then
      include(result, k);
  end;
end;

function KnownSidToText(wkr: TWellKnownRid; const Domain: RawUtf8): RawUtf8;
begin
  SidToText(pointer(KnownRawSid(wkr, Domain)), result);
end;

function KnownRawSid(wkr: TWellKnownRid; const Domain: RawUtf8): RawSid;
begin
  if TryDomainTextToSid(Domain, result) then
    PSid(result)^.SubAuthority[4] := WKR_RID[wkr];
end;

procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: TSid);
begin
  MoveFast(dom^, result, SID_MINLEN + 4 * 4); // copy domain fields
  result.SubAuthorityCount := 5; // if dom was a pure domain SID, not a RID
  result.SubAuthority[4] := WKR_RID[wkr]; // append the RID
end;

procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: RawSid);
begin
  FastNewRawByteString(RawByteString(result), SID_MINLEN + 5 * 4);
  KnownRidSid(wkr, dom, PSid(result)^);
end;


{ ****************** Security Descriptor Self-Relative Binary Structures }

function BinToSecAcl(p: PByteArray; offset: cardinal; var res: TSecAcl): boolean;
var
  hdr: PRawAcl;
  ace: PRawAce;
  max: pointer;
  a: ^TSecAce;
  i: integer;
begin
  result := offset = 0;
  if result then
    exit; // no DACL/SACL
  hdr := @p[offset];
  if (hdr^.Sbz1 <> 0) or
     not (hdr^.AclRevision in [2, 4]) then
    exit;
  if hdr^.AceCount <> 0 then
  begin
    max := @p[offset + hdr^.AclSize];
    SetLength(res, hdr^.AceCount);
    a := pointer(res);
    ace := @p[offset + SizeOf(hdr^)];
    for i := 1 to hdr^.AceCount do
    begin
      if not a^.FromBinary(pointer(ace), max) then
        exit;
      inc(PByte(ace), ace^.AceSize);
      inc(a);
    end;
  end;
  result := true;
end;

function SecAclToBin(p: PAnsiChar; const acl: TSecAcl): PtrInt;
var
  hdr: PRawAcl;
  a: ^TSecAce;
  i, len: PtrInt;
begin
  result := 0;
  if acl = nil then
    exit;
  hdr := pointer(p);
  result := SizeOf(hdr^);
  if hdr <> nil then // need to write ACL header
    inc(p, result);
  a := pointer(acl);
  for i := 1 to length(acl) do
  begin
    len := a^.ToBinary(p);
    inc(result, len);
    if hdr <> nil then
      inc(p, len);
    inc(a);
  end;
  if hdr = nil then
    exit;
  hdr^.AclRevision := 2;
  hdr^.Sbz1 := 0;
  hdr^.AceCount := length(acl);
  hdr^.Sbz2 := 0;
  hdr^.AclSize := result;
end;

function SecAclToBinary(const acl: TSecAcl): RawByteString;
begin
  FastNewRawByteString(result, SecAclToBin({dest=}nil, acl)); // allocate
  SecAclToBin(pointer(result), acl); // fill
end;

function AclReplaceDomainRaw(old, new: PSid; maxRid: cardinal;
  var acl: TSecAcl): integer;
var
  a: ^TSecAce;
  i: PtrInt;
begin
  result := 0;
  if acl = nil then
    exit;
  a := pointer(acl);
  for i := 1 to length(acl) do
  begin
    inc(result, a^.ReplaceDomainRaw(old, new, maxRid));
    inc(a);
  end;
end;

function AclReplaceAny(const OldSid, NewSid: RawSidDynArray;
  var acl: TSecAcl): integer;
var
  a: ^TSecAce;
  i: PtrInt;
begin
  result := 0;
  if acl = nil then
    exit;
  a := pointer(acl);
  for i := 1 to length(acl) do
  begin
    inc(result, a^.ReplaceAnySid(OldSid, NewSid));
    inc(a);
  end;
end;


{ ****************** Active Directory Definitions }

function UuidToKnownAttribute(const u: TGuid): TAdsKnownAttribute;
begin
  result := TAdsKnownAttribute(FastFindBinarySorted( // branchless O(log(n))
    @ATTR_UUID[succ(kaNull)], @u, SizeOf(u), length(ATTR_UUID) - 2) + 1);
end;

procedure AppendShortKnownUuid(const u: TGuid; var s: ShortString);
var
  a: TAdsKnownAttribute;
begin
  a := UuidToKnownAttribute(u); // fast branchless O(log(n)) binary search
  if a = kaNull then
    AppendShortUuid(u, s) // append as regular UUID hexadecimal text
  else
    AppendShortAnsi7String(ATTR_TXT[a], s); // append the ldapDisplayName
end;

function TextToKnownAttribute(p: PUtf8Char; len: TStrLen): TAdsKnownAttribute;
begin
  if len < 4 then
    result := kaNull
  else
    result := TAdsKnownAttribute(FindNonVoidRawUtf8I( // brute force search
      @ATTR_TXT[succ(kaNull)], p, len, length(ATTR_TXT) - 1) + 1);
end;

function ShortToKnownUuid(const text: ShortString; out uuid: TGuid): boolean;
var
  a: TAdsKnownAttribute;
begin
  result := false;
  if text[0] = #36 then
    // decode '3F2504E0-4F89-11D3-9A0C-0305E82C3301' standard encoding
    result := ShortToUuid(text, uuid) // using the RTL or mormot.core.text
  else
  begin
    // case-insensitive ATTR_TXT[] text search
    a := TextToKnownAttribute(@text[1], ord(text[0]));
    if a = kaNull then
      exit;
    uuid := ATTR_UUID[a];
    result := true;
  end;
end;


{ ****************** Security Descriptor Definition Language (SDDL) }

const
  // defined as a packed array of chars for fast SSE2 brute force search
  SID_SDDL: array[1 .. (high(SDDL_WKS) + high(SDDL_WKR)) * 2] of AnsiChar =
    // TWellKnownSid in SDDL_WKS[] order
    'WDCOCGOWLWMEMPHISISSNUIUSUANEDPSAURCSYLSNSBABUBGPUAOSOPOBORERURDNOMULU'+
    'ISCYERCDRAESMSHAAARMWRUDAC'+
    // TWellKnownRid in SDDL_WKR[] order
    'ROLALGDADUDGDCDDCASAEAPACNAPKAEKRSHO';
var
  SID_SDDLW: packed array[byte] of word absolute SID_SDDL; // for fast lookup
  SDDL_WKS_INDEX: array[TWellKnownSid] of byte; // into 1..48
  SDDL_WKR_INDEX: array[TWellKnownRid] of byte; // into 49..66

function KnownSidToSddl(wks: TWellKnownSid): RawUtf8;
var
  i: PtrInt;
begin
  FastAssignNew(result);
  i := SDDL_WKS_INDEX[wks];
  if i <> 0 then
    FastSetString(result, @SID_SDDLW[i - 1], 2);
end;

function KnownRidToSddl(wkr: TWellKnownRid): RawUtf8;
var
  i: PtrInt;
begin
  FastAssignNew(result);
  i := SDDL_WKR_INDEX[wkr];
  if i <> 0 then
    FastSetString(result, @SID_SDDLW[i - 1], 2);
end;

function SddlToKnownSid(const sddl: RawUtf8; out wks: TWellKnownSid): boolean;
var
  i: PtrInt;
begin
  result := false;
  if length(sddl) <> 2 then
    exit;
  i := WordScanIndex(@SID_SDDLW, high(SDDL_WKS), PWord(sddl)^);
  if i < 0 then
    exit;
  wks := SDDL_WKS[i + 1];
  result := true;
end;

function SddlToKnownRid(const sddl: RawUtf8; out wkr: TWellKnownRid): boolean;
var
  i: PtrInt;
begin
  result := false;
  if length(sddl) <> 2 then
    exit;
  i := WordScanIndex(@SID_SDDLW[high(SDDL_WKS)], high(SDDL_WKR), PWord(sddl)^);
  if i < 0 then
    exit;
  wkr := SDDL_WKR[i + 1];
  result := true;
end;

function SddlNextSid(var p: PUtf8Char; var sid: RawSid; dom: PSid): boolean;
var
  tmp: TSid;
begin
  result := SddlNextSid(p, tmp, dom);
  if result then
    ToRawSid(@tmp, sid);
end;

function SddlNextSid(var p: PUtf8Char; out sid: TSid; dom: PSid): boolean;
var
  i: PtrInt;
begin
  while p^ = ' ' do
    inc(p);
  if PCardinal(p)^ = SID_REV32 then
  begin
    result := TextToSid(p, sid); // parse e.g. S-1-5-32-544
    while p^ = ' ' do
      inc(p);
    exit;
  end;
  result := false;
  if not (p^ in ['A' .. 'Z']) then
    exit;
  i := WordScanIndex(@SID_SDDL, SizeOf(SID_SDDL) shr 1, PWord(p)^);
  if i < 0 then
    exit
  else if i < high(SDDL_WKS) then
    KnownRawSid(SDDL_WKS[i + 1], sid)
  else if dom = nil then
    exit // no RID support
  else
    KnownRidSid(SDDL_WKR[i - pred(high(SDDL_WKS))], dom, sid);
  inc(p, 2);
  while p^ = ' ' do
    inc(p);
  result := true;
end;

procedure SddlAppendSid(var s: ShortString; sid, dom: PSid);
var
  k: TWellKnownSid;
  i: PtrInt;
begin
  if sid = nil then
    exit;
  k := SidToKnown(sid);
  i := SDDL_WKS_INDEX[k];
  if i <> 0 then
  begin
    AppendShortTwoChars(@SID_SDDLW[i - 1], @s); // e.g. WD SY
    exit;
  end
  else if (k = wksNull) and
          (dom <> nil) and
          SidSameDomain(sid, dom) and
          (sid^.SubAuthority[4] <= WKR_RID_MAX) then
  begin
    i := WordScanIndex(@WKR_RID, length(WKR_RID), sid^.SubAuthority[4]);
    if i >= 0 then
    begin
      i := SDDL_WKR_INDEX[TWellKnownRid(i)];
      if i <> 0 then
      begin
        AppendShortTwoChars(@SID_SDDLW[i - 1], @s); // e.g. DA DU
        exit;
      end;
    end
  end;
  SidAppendShort(sid, s); // if not known, append as 'S-1-x....' standard text
end;

function SddlNextPart(var p: PUtf8Char; out u: ShortString): boolean;
var
  s: PUtf8Char;
begin
  s := p;
  while s^ = ' ' do
    inc(s);
  p := s;
  while not (s^ in [#0, ' ', ';', ')']) do
    inc(s);
  SetString(u, p, s - p);
  while s^ = ' ' do
    inc(s);
  result := s^ = ';';
  p := s;
end;

function SddlNextTwo(var p: PUtf8Char; out u: ShortString): boolean;
var
  s: PUtf8Char;
begin
  result := false;
  s := p;
  while s^ = ' ' do
    inc(s);
  if not (s[0] in ['A' .. 'Z']) or
     not (s[1] in ['A' .. 'Z']) then
    exit;
  u[0] := #2;
  PWord(@u[1])^ := PWord(s)^;
  inc(s, 2);
  while s^ = ' ' do
    inc(s);
  p := s;
  result := true;
end;

function SddlNextUuid(var p: PUtf8Char; read: TShortToUuid; out uuid: TGuid): boolean;
var
  u: ShortString;
begin
  result := SddlNextPart(p, u) and
            ((u[0] = #0) or
             read(u, uuid)); // use RTL or mormot.core.text
  repeat
    inc(p);
  until p^ <> ' ';
end;

function SddlNext0xInteger(var p: PUtf8Char; out c: integer): boolean;
begin
  c := ParseHex0x(PAnsiChar(p)); // p^ is '0x####'
  repeat
    inc(p);
  until p^ in [#0, ';'];
  result := c <> 0;
end;

function SddlNextMask(var p: PUtf8Char; var mask: TSecAccessMask): boolean;
var
  r: TSecAccessRight;
  a: TSecAccess;
  m: integer absolute mask;
  one: integer;
  u: string[2];
begin
  result := false;
  m := 0;
  if p = nil then
    exit;
  while p^ = ' ' do
    inc(p);
  while not (p^ in [#0, ';']) do
  begin
    if PWord(p)^ = ord('0') + ord('x') shl 8 then
      if SddlNext0xInteger(p, m) then
        break // we got the mask as a 32-bit hexadecimal value
      else
        exit;
    if not SddlNextTwo(p, u) then
      exit;
    one := 0;
    for r := low(r) to high(r) do
      if SAR_SDDL[r] = u then
      begin
        one := SAR_MASK[r];
        break;
      end;
    if one = 0 then
      for a := low(a) to high(a) do
        if SAM_SDDL[a] = u then
        begin
          one := 1 shl ord(a);
          break;
        end;
    if one = 0 then
      exit; // unrecognized
    m := m or one;
  end;
  result := m <> 0;
end;

procedure SddlAppendMask(var s: ShortString; mask: TSecAccessMask);
var
  a: TSecAccess;
  i: PtrInt;
begin
  if cardinal(mask) = 0 then
    exit;
  i := IntegerScanIndex(@SAR_MASK, length(SAR_MASK), cardinal(mask));
  if i >= 0 then
    AppendShortTwoChars(@SAR_SDDL[TSecAccessRight(i)][1], @s)
  else if mask - samWithSddl <> [] then
  begin
    AppendShortTwoChars('0x', @s);        // we don't have all tokens it needs
    AppendShortIntHex(cardinal(mask), s); // store as @x##### hexadecimal
  end
  else
    for a := low(a) to high(a) do
      if a in mask then
        AppendShortTwoChars(@SAM_SDDL[a][1], @s); // store as SDDL pairs
end;

function SddlNextOpaque(var p: PUtf8Char; var ace: TSecAce): TAceTextParse;
var
  s: PUtf8Char;
  parent: integer;
  tree: TAceTextTree;
begin
  ace.Opaque := '';
  if p <> nil then
    while p^ = ' ' do
      inc(p);
  s := p;
  if s <> nil then
  begin
    parent := 0;
    if (ace.AceType in satConditional) and
       (s^ = '(') then // conditional ACE expression
    begin
      result := atpMissingFinal;
      repeat
        case s^ of
          #0:
            exit; // premature end
          '(':
            inc(parent); // count nested parenthesis
          '"':
            repeat
              inc(s);
              if s^ = #0 then
                exit;
            until s^ = '"'; // ignore any ( ) within "unicode" blocks
          ')':
            if parent = 0 then
              break // ending ACE parenthesis
            else
              dec(parent);
        end;
        inc(s);
      until false;
      tree.Storage := p;
      tree.StorageSize := s - p;
      if tree.FromText = atpSuccess then
        ace.Opaque := tree.ToBinary;
      if ace.Opaque = '' then
      begin
        result := tree.Error; // return the detailed parsing error
        exit;
      end;
    end
    else // fallback to hexadecimal output
    begin
      while not (s^ in [#0, ')']) do // retrieve everthing until ending ')'
        inc(s);
      FastNewRawByteString(ace.Opaque, (s - p) shr 1);
      result := atpInvalidContent;
      if ParseHex(PAnsiChar(p),
           pointer(ace.Opaque), length(ace.Opaque)) <> PAnsiChar(s) then
        exit;
    end;
  end;
  p := s;
  result := atpSuccess;
end;

procedure SddlAppendOpaque(var s: TSynTempBuffer; const ace: TSecAce; dom: PSid);
var
  tree: TAceBinaryTree;
  tmp: shortstring absolute tree;
begin
  if ace.Opaque <> '' then
    if (ace.AceType in satConditional) and
       tree.FromBinary(ace.Opaque) then
      // append conditional ACE expression using recursive SDDL generation
      s.Add(tree.ToText(dom))
    else
    begin
      // fallback to hexadecimal (not standard, but good enough for display)
      tmp[0] := #0;
      AppendShortHex(pointer(ace.Opaque), length(ace.Opaque), tmp);
      s.AddShort(tmp);
    end;
end;

procedure SddlOperandToText(v: PRawAceOperand; dom: PSid; out u: RawUtf8);
var
  s: ShortString;
begin
  s[0] := #0;
  if SddlAppendOperand(s, v, dom) then
    FastSetString(u, @s[1], ord(s[0]));
end;

procedure SddlUnaryToText(tok: TSecConditionalToken; var l, u: RawUtf8);
var
  op: PRawUtf8;
begin
  op := @SDDL_OPER_TXT[SDDL_OPER_INDEX[tok]];
  if tok = sctNot then
    // inner parenth for '!(..)'
    u := op^ + '(' + l + ')'
  else
    // e.g. '(Member_of{SID(BA)})'
    u := '(' + op^ + l + ')';
  FastAssignNew(l); // release param
end;

procedure SddlBinaryToText(tok: TSecConditionalToken; var l, r, u: RawUtf8);
var
  op: PRawUtf8;
begin
  op := @SDDL_OPER_TXT[SDDL_OPER_INDEX[tok]];
  if tok in [sctContains, sctAnyOf, sctNotContains, sctNotAnyOf] then
    if (r <> '') and
       (r[1] <> '{') then
      // e.g. '(@User.Project Any_of @Resource.Project)'
      u := '(' + l + ' ' + op^ + ' ' + r + ')'
    else
      // e.g. '(@Resource.dept Any_of{"Sales","HR"})'
      u := '(' + l + ' ' + op^ + r + ')'
  else
    // e.g. '(Title=="VP")'
    u := '(' + l + op^ + r + ')';
  FastAssignNew(l); // release params
  FastAssignNew(r);
end;

function SddlAppendOperand(var s: ShortString; v: PRawAceOperand; dom: PSid): boolean;
var
  utf8: shortstring;
  c: PRawAceOperand;
  singleComposite: boolean;
  i, comp, clen: PtrUInt;
begin
  result := true;
  case v^.Token of
    sctInt8,
    sctInt16,
    sctInt32,
    sctInt64:
      if v^.Int.Base <> scbDecimal then // scbOctal does fallback to hexa
      begin
        AppendShortTwoChars('0x', @s);
        AppendShortIntHex(v^.Int.Value, s);
      end
      else if v^.Int.Sign = scsNegative then
        AppendShortInt64(v^.Int.Value, s)
      else
        AppendShortQWord(v^.Int.Value, s);
    sctUnicode:
      begin
        AppendShortChar('"', @s);
        Unicode_WideToShort(@v^.Unicode, v^.UnicodeBytes shr 1, CP_UTF8, utf8);
        if ord(s[0]) + ord(utf8[0]) > 250 then
          result := false // we don't like to be truncated
        else
          AppendShort(utf8, s);
        AppendShortChar('"', @s);
      end;
    sctLocalAttribute,
    sctUserAttribute,
    sctResourceAttribute,
    sctDeviceAttribute:
      begin
        AppendShort(ATTR_SDDL[v^.Token], s);
        Unicode_WideToShort(@v^.Unicode, v^.UnicodeBytes shr 1, CP_UTF8, utf8);
        for i := 1 to ord(utf8[0]) do
          if ord(s[0]) > 250 then
            result := false
          else if PosExChar(utf8[i], '!"&()<=>|%') <> 0 then  // see attr-char2
          begin
            AppendShort('%00', s); // needs escape
            AppendShortByteHex(ord(utf8[i]), s);
          end
          else
          begin
            inc(s[0]);
            s[ord(s[0])] := utf8[i];
          end;
      end;
    sctOctetString:
      begin
        AppendShortChar('#', @s);
        if ord(s[0]) + v^.OctetBytes shl 1 > 250 then
          result := false // we don't like to be truncated
        else
          AppendShortHex(@v^.Octet, v^.OctetBytes, s);
      end;
    sctComposite: // e.g. '{"Sales","HR"}'
      begin
        result := false;
        comp := v^.CompositeBytes;
        if comp = 0 then
          exit; // should not be void
        c := @v.Composite;
        singleComposite := comp = AceTokenLength(c);
        if singleComposite then
          if c^.Token = sctSid then
            // e.g. '(Member_of{SID(BA)}))'
            singleComposite := false
          else
            // e.g. '(@User.Project Any_of 1)'
            AppendShortChar(' ', @s);
        if not singleComposite then
          AppendShortChar('{', @s);
        repeat
          clen := AceTokenLength(c);
          if clen > comp then
            exit; // avoid buffer overflow
          if (c^.Token in sctAttribute) or
             not SddlAppendOperand(s, c, dom) then
            exit; // unsupported or truncated/overflow content
          dec(comp, clen);
          if comp = 0 then
            break;
          inc(PByte(c), clen);
          AppendShortChar(',', @s);
        until false;
        if not singleComposite then
          AppendShortChar('}', @s);
        result := true;
      end;
    sctSid:
      if PtrInt(v^.SidBytes) >= SidLength(@v^.Sid) then
      begin
        AppendShort('SID(', s);
        SddlAppendSid(s, @v^.Sid, dom);
        AppendShortChar(')', @s);
      end
      else
        exit; // should not be void
  else
    result := false; // unsupported content
  end;
end;

function SddlNextOperand(var p: PUtf8Char): TSecConditionalToken;
var
  s: PUtf8Char;
  i: PtrInt;
  sct: TSecConditionalToken;
begin
  result := sctPadding; // unknown
  s := p;
  case s^ of
    #0:
      result := sctInternalFinal;
    '-', '0' .. '9':
      begin
        repeat
          inc(s);
        until not (s^ in ['0' .. '9']);
        result := sctInt64;
      end;
    '"':
      begin
        repeat
          inc(s);
          if s^ = #0 then
            exit;
        until s^ = '"'; // nested double quotes support is not needed
        inc(s);
        result := sctUnicode;
      end;
    '#':
      begin
        repeat
          inc(s);
        until not (s^ in ['#', '0' .. '9', 'A' .. 'Z', 'a' .. 'z']);
        result := sctOctetString;
      end;
    '{':
       begin
         repeat
           inc(s);
           if s^ = #0 then
             exit;
         until s^ = '}';
         inc(s);
         result := sctComposite;
       end;
    '=':
      begin
        if s[1] <> '=' then
          exit;
        inc(s, 2);
        result := sctEqual;
      end;
    '!':
      if s[1] = '=' then
      begin
        inc(s, 2);
        result := sctNotEqual;
      end
      else
      begin
        inc(s);
        result := sctNot;
      end;
    '<':
      if s[1] = '=' then
      begin
        inc(s, 2);
        result := sctLessThanOrEqual;
      end
      else
      begin
        inc(s);
        result := sctLessThan;
      end;
    '>':
      if s[1] = '=' then
      begin
        inc(s, 2);
        result := sctGreaterThanOrEqual;
      end
      else
      begin
        inc(s);
        result := sctGreaterThan;
      end;
    'A' .. 'Z',
    'a' .. 'z',
    #$80 .. #$ff:
      begin
        result := sctLocalAttribute;
        repeat
          inc(s);
        until s^ in SDDL_END_IDENT;
        case p^ of
          'A', 'C', 'D', 'E', 'M', 'N', 'a', 'c', 'd', 'e', 'm', 'n':
            for i := SDDL_OPER_INDEX[sctContains] to
                     SDDL_OPER_INDEX[sctNotDeviceMemberOfAny] do
              if PropNameEquals(SDDL_OPER_TXT[i], PAnsiChar(p), s - p) then
              begin
                result := SDDL_OPER[i];
                break;
              end;
          'S', 's': // e.g. 'SID(BA)'
            if PCardinal(p)^ and $ffdfdfdf =
                 ord('S') + ord('I') shl 8 + ord('D') shl 16 + ord('(') shl 24 then
            begin
              repeat
                inc(s);
                if s^ = #0 then
                  exit;
              until s^ = ')';
              inc(s);
              result := sctSid;
            end;
        end;
      end;
    '&':
      begin
        if s[1] <> '&' then
          exit;
        inc(s, 2);
        result := sctAnd;
      end;
    '|':
      begin
        if s[1] <> '|' then
          exit;
        inc(s, 2);
        result := sctOr;
      end;
    '@':
      begin
        repeat
          inc(s);
          if s^ = #0 then
            exit;
        until s^ = '.';
        inc(s);
        for sct := sctUserAttribute to sctDeviceAttribute do
          if PropNameEquals(@ATTR_SDDL[sct], PAnsiChar(p), s - p) then
          begin
            result := sct;
            break;
          end;
        if result = sctPadding then
          exit; // unknown @Reference.
        p := s;
        while not (s^ in SDDL_END_IDENT) do
          inc(s);
        if s = p then
          exit; // no attribute name
      end;
    // for internal SDDL text parsing use only
    '(':
      begin
        inc(s);
        result := sctInternalParenthOpen;
      end;
    ')':
      begin
        inc(s);
        result := sctInternalParenthClose;
      end;
  end;
  p := s;
end;


{ ****************** Access Control List (DACL/SACL) Definitions }

{ TSecAce }

procedure TSecAce.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

function TSecAce.IsEqual(const ace: TSecAce): boolean;
begin
  result := (AceType = ace.AceType) and
            (RawType = ace.RawType) and
            (Flags = ace.Flags) and
            (Mask = ace.Mask) and
            (Sid = ace.Sid) and
            (Opaque = ace.Opaque) and
            IsEqualGuid(ObjectType, ace.ObjectType) and
            IsEqualGuid(InheritedObjectType, ace.InheritedObjectType);
end;

function TSecAce.SidText(dom: PSid): RawUtf8;
var
  s: ShortString;
begin
  s[0] := #0;
  SddlAppendSid(s, pointer(sid), dom);
  ShortStringToAnsi7String(s, result);
end;

function TSecAce.SidText(const sidSddl: RawUtf8; dom: PSid): boolean;
var
  p: PUtf8Char;
begin
  p := pointer(sidSddl);
  result := SddlNextSid(p, Sid, dom);
end;

function TSecAce.MaskText: RawUtf8;
var
  s: ShortString;
begin
  s[0] := #0;
  SddlAppendMask(s, Mask);
  ShortStringToAnsi7String(s, result);
end;

function TSecAce.MaskText(const maskSddl: RawUtf8): boolean;
var
  p: PUtf8Char;
begin
  p := pointer(maskSddl);
  result := SddlNextMask(p, Mask);
end;

function TSecAce.ConditionalExpression(dom: PSid): RawUtf8;
var
  tmp: TSynTempBuffer;
begin
  tmp.InitOnStack;
  SddlAppendOpaque(tmp, self, dom);
  tmp.Done(result, CP_UTF8);
end;

function TSecAce.ConditionalExpression(
  const condExp: RawUtf8; dom: PSid): TAceTextParse;
var
  p: PUtf8Char;
begin
  p := pointer(condExp);
  result := SddlNextOpaque(p, self);
end;

function TSecAce.ReplaceDomainRaw(old, new: PSid; maxRid: cardinal): integer;
begin
  result := SidReplaceDomain(old, new, maxRid, Sid);
  if (Opaque <> '') and
     (PCardinal(Opaque)^ = ACE_CONDITION_SIGNATURE) then
    inc(result, AceReplaceDomain(old, new, maxRid, Opaque));
end;

function TSecAce.ReplaceAnySid(const OldSid, NewSid: RawSidDynArray): integer;
begin
  result := SidReplaceAny(OldSid, NewSid, Sid);
  if (Opaque <> '') and
     (PCardinal(Opaque)^ = ACE_CONDITION_SIGNATURE) then
    inc(result, AceReplaceAnySid(OldSid, NewSid, Opaque));
end;

function TSecAce.Fill(sat: TSecAceType; const sidSddl, maskSddl: RawUtf8;
  dom: PSid; const condExp: RawUtf8; saf: TSecAceFlags): boolean;
begin
  Clear;
  result := false;
  if (sat = satUnknown) or
     not SidText(sidSddl, dom) or
     not MaskText(maskSddl) or
     (ConditionalExpression(condExp) <> atpSuccess) then
    exit;
  AceType := sat;
  RawType := ord(sat) + 1;
  Flags := saf;
  result := true;
end;

procedure TSecAce.AppendAsText(var s: ShortString; var sddl: TSynTempBuffer;
  dom: PSid; uuid: TAppendShortUuid);
var
  f: TSecAceFlag;
begin
  AppendShortChar('(', @s);
  if SAT_SDDL[AceType][0] <> #0 then
    AppendShort(SAT_SDDL[AceType], s)
  else
  begin
    AppendShortTwoChars('0x', @s);
    AppendShortIntHex(RawType, s); // fallback to lower hex - paranoid
  end;
  AppendShortChar(';', @s);
  if Flags <> [] then
    for f := low(f) to high(f) do
      if f in Flags then
        AppendShort(SAF_SDDL[f], s);
  AppendShortChar(';', @s);
  SddlAppendMask(s, Mask);
  if AceType in satObject then
  begin
    AppendShortChar(';', @s);
    if not IsNullGuid(ObjectType) then
      uuid(ObjectType, s); // RTL or mormot.core.text
    AppendShortChar(';', @s);
    if not IsNullGuid(InheritedObjectType) then
      uuid(InheritedObjectType, s);
    AppendShortChar(';', @s);
  end
  else
    AppendShort(';;;', s);
  SddlAppendSid(s, pointer(Sid), dom);
  if Opaque <> '' then
  begin
    AppendShortChar(';', @s);
    sddl.AddShort(s);
    s[0] := #0;
    SddlAppendOpaque(sddl, self, dom); // direct expression write in sddl
  end;
  AppendShortChar(')', @s);
  sddl.AddShort(s);
  s[0] := #0;
end;

function TSecAce.FromText(var p: PUtf8Char; dom: PSid; uuid: TShortToUuid): TAceTextParse;
var
  u: ShortString;
  t: TSecAceType;
  f: TSecAceFlag;
  i: integer;
begin
  result := atpInvalidType;
  while p^ = ' ' do
    inc(p);
  if PWord(p)^ = ord('0') + ord('x') shl 8 then // our own fallback format
    if SddlNext0xInteger(p, i) then
      RawType := i
    else
      exit
  else
  begin
    if not SddlNextPart(p, u) or
       not (u[0] in [#1, #2]) then
      exit;
    for t := succ(low(t)) to high(t) do
      if SAT_SDDL[t] = u then
      begin
        AceType := t;
        break;
      end;
    if AceType = satUnknown then
      exit;
    RawType := ord(AceType) - 1;
  end;
  if p^ <> ';' then
    exit;
  repeat
    inc(p);
  until p^ <> ' ';
  result := atpInvalidFlags;
  while p^ <> ';' do
  begin
    if not SddlNextTwo(p, u) then
      exit;
    for f := low(f) to high(f) do
      if SAF_SDDL[f] = u then
      begin
        include(Flags, f);
        break;
      end;
  end;
  inc(p);
  result := atpInvalidMask;
  if not SddlNextMask(p, Mask) or
     (p^ <> ';') then
    exit;
  repeat
    inc(p);
  until p^ <> ' ';
  result := atpInvalidUuid;
  if not SddlNextUuid(p, uuid, ObjectType) or
     not SddlNextUuid(p, uuid, InheritedObjectType) then // satObject or nothing
    exit;
  result := atpInvalidSid;
  if not SddlNextSid(p, Sid, dom) then // entries always end with a SID/RID
    exit;
  if p^ = ';' then // optional additional/opaque parameter
  begin
    inc(p);
    result := SddlNextOpaque(p, self);
    if result <> atpSuccess then
      exit;
  end;
  result := atpMissingParenthesis;
  if p^ = ')' then // ACE should end with a parenthesis
    result := atpSuccess;
end;

function TSecAce.ToBinary(dest: PAnsiChar): PtrInt;
var
  hdr: PRawAce; // ACE binary header
  f: cardinal;
  pf: PCardinal;
begin
  hdr := pointer(dest);
  inc(dest, 8); // ACE header + Mask
  if AceType in satObject then
  begin
    pf := pointer(dest);
    inc(PCardinal(dest));
    f := 0;
    if not IsNullGuid(ObjectType) then
    begin
      f := ACE_OBJECT_TYPE_PRESENT;
      if hdr <> nil then
        PGuid(dest)^ := ObjectType;
      inc(PGuid(dest));
    end;
    if not IsNullGuid(InheritedObjectType) then
    begin
      f := f or ACE_INHERITED_OBJECT_TYPE_PRESENT;
      if hdr <> nil then
        PGuid(dest)^ := InheritedObjectType;
      inc(PGuid(dest));
    end;
    if hdr <> nil then
      pf^ := f;
  end;
  if hdr <> nil then
    MoveFast(pointer(Sid)^, dest^, length(Sid));
  inc(dest, length(Sid));
  if hdr <> nil then
    MoveFast(pointer(Opaque)^, dest^, length(Opaque));
  inc(dest, length(Opaque));
  result := dest - pointer(hdr);
  if hdr = nil then
    exit; // nothing to write
  if AceType <> satUnknown then
    hdr^.AceType := ord(AceType) - 1
  else
    hdr^.AceType := RawType;
  hdr^.AceFlags := Flags;
  hdr^.AceSize := dest - pointer(hdr);
  hdr^.Mask := Mask;
end;

function TSecAce.FromBinary(p, max: PByte): boolean;
var
  ace: PRawAce;
  opaquelen, sidlen: integer;
  psid: pointer;
begin
  result := false;
  ace := pointer(p);
  if (max <> nil) and
     (PtrUInt(ace) + ace^.AceSize > PtrUInt(max)) then
    exit; // avoid buffer overflow
  RawType := ace^.AceType;
  AceType := TSecAceType(ace^.AceType + 1); // range is checked below
  Flags := ace^.AceFlags;
  Mask := ace^.Mask;
  psid := nil;
  if AceType in satCommon then
    psid := @ace^.CommonSid
  else if AceType in satObject then
  begin
    psid := @ace^.ObjectStart;
    if ace^.ObjectFlags and ACE_OBJECT_TYPE_PRESENT <> 0 then
    begin
      ObjectType := PGuid(psid)^;
      inc(PGuid(psid));
    end;
    if ace^.ObjectFlags and ACE_INHERITED_OBJECT_TYPE_PRESENT <> 0 then
    begin
      InheritedObjectType := PGuid(psid)^;
      inc(PGuid(psid));
    end;
  end
  else if AceType > high(TSecAceType) then
    AceType := satUnknown; // unsupported or out of range RawType
  if psid = nil then
    exit;
  sidlen := SidLength(psid) + (PAnsiChar(psid) - pointer(ace));
  opaquelen := integer(ace^.AceSize) - sidlen;
  if opaquelen < 0 then
    exit; // invalid SidLength()
  ToRawSid(psid, Sid);
  result := true;
  if opaquelen = 0 then
    exit;
  inc(PByte(psid), sidlen - 8);
  FastSetRawByteString(Opaque, psid, opaquelen);
end;


{ ******************* Conditional ACE Expressions SDDL and Binary Support }

function AceTokenLength(v: PRawAceOperand): PtrUInt;
begin
  case v^.Token of // literal or attribute
    sctInt8,
    sctInt16,
    sctInt32,
    sctInt64:
      result := SizeOf(v^.Token) + SizeOf(v^.Int);
    sctUnicode,     // need to append UnicodeBytes
    sctLocalAttribute,
    sctUserAttribute,
    sctResourceAttribute,
    sctDeviceAttribute,
    sctOctetString, // OctetBytes     = UnicodeBytes
    sctComposite,   // CompositeBytes = UnicodeBytes
    sctSid:         // SidBytes       = UnicodeBytes
      result := v^.UnicodeBytes + (SizeOf(v^.Token) + SizeOf(v^.UnicodeBytes));
  else
    result := SizeOf(v^.Token); // operation stored as a single token byte
  end;
end;

function AceReplaceDomain(olddom, newdom: PSid; maxRid: cardinal;
  var opaque: RawByteString): integer;
var
  v, c: PRawAceOperand;
  opaquesize, size, len, comp, clen: PtrUInt;
begin
  result := 0;
  opaquesize := length(opaque);
  size := opaquesize;
  if (size = 0) or
     (size and 3 <> 0) or
     (PCardinal(opaque)^ <> ACE_CONDITION_SIGNATURE) then
    exit;
  v := @PCardinalArray(opaque)[1];
  repeat
    len := AceTokenLength(v);
    if len > size then
      exit; // avoid buffer overflow
    case v^.Token of
      sctSid:
        if SidSameDomain(@v^.Sid, olddom) and
           (v^.Sid.SubAuthority[4] <= maxRid) then
        begin
          MoveFast(newdom^, v^.Sid, SID_DOMAINLEN); // in-place overwrite
          inc(result);
        end;
      sctComposite: // check for any nested SID
        begin
          c := @v^.Composite;
          comp := v^.CompositeBytes;
          if comp <> 0 then
            repeat
              clen := AceTokenLength(c);
              if clen > comp then
                exit;
              if (c^.Token = sctSid) and
                 SidSameDomain(@c^.Sid, olddom) and
                 (c^.Sid.SubAuthority[4] <= maxRid) then
              begin
                MoveFast(newdom^, c^.Sid, SID_DOMAINLEN); // in-place overwrite
                inc(result);
              end;
              inc(PByte(c), clen);
              dec(comp, clen);
            until comp = 0;
        end;
    end;
    inc(PByte(v), len);
    dec(size, len);
  until size = 0;
end;

function AceReplaceAnySid(const old, new: RawSidDynArray;
  var opaque: RawByteString): integer;
var
  v, c: PRawAceOperand;
  opaquesize, size, oldlen, len, comp, clen: PtrUInt;
begin
  result := 0;
  opaquesize := length(opaque);
  size := opaquesize;
  if (old = nil) or
     (size = 0) or
     (size and 3 <> 0) or
     (PCardinal(opaque)^ <> ACE_CONDITION_SIGNATURE) then
    exit;
  oldlen := length(old[0]); // all old[] should have the same length
  v := @PCardinalArray(opaque)[1];
  repeat
    len := AceTokenLength(v);
    if len > size then
      exit; // avoid buffer overflow
    case v^.Token of
      sctSid:
        if v^.SidBytes = oldlen then
          inc(result, SidReplaceAny(old, new, v^.Sid, oldlen));
      sctComposite: // check for any nested SID
        begin
          c := @v^.Composite;
          comp := v^.CompositeBytes;
          if comp <> 0 then
            repeat
              clen := AceTokenLength(c);
              if clen > comp then
                exit;
              if (c^.Token = sctSid) and
                 (c^.SidBytes = oldlen) then
                inc(result, SidReplaceAny(old, new, c^.Sid, oldlen));
              inc(PByte(c), clen);
              dec(comp, clen);
            until comp = 0;
        end;
    end;
    inc(PByte(v), len);
    dec(size, len);
  until size = 0;
end;


{ TAceBinaryTree }

function TAceBinaryTree.FromBinary(const Input: RawByteString): boolean;
var
  v: PRawAceOperand;
  position, len, stCount: PtrUInt;
  st: array[0 .. 31] of byte; // local stack of last few operands
begin
  result := false;
  Storage := pointer(Input);
  StorageSize := length(Input);
  Count := 0;
  if (Storage = nil) or
     (StorageSize and 3 <> 0) or // should be DWORD-aligned
     (StorageSize >= MAX_TREE_BYTES) or
     (PCardinal(Storage)^ <> ACE_CONDITION_SIGNATURE) then
    exit;
  stCount := 0;
  position := 4; // tokens start after 'artx' header
  repeat
    v := @Storage[position];
    len := AceTokenLength(v);
    if (position + len > StorageSize) then // avoid buffer overflow
      exit;
    if v^.Token <> sctPadding then
    begin
      // parse the next non-void operand or operation
      if v^.Token in sctOperand then
      begin
        if not AddNode(position, 255, 255) then
          exit;
      end
      else if v^.Token in sctUnary then
      begin
        if stCount < 1 then
          exit;
        dec(stCount); // unstack one operand
        if not AddNode(position, st[stCount], 255) then
          exit;
      end
      else if v^.Token in sctBinary then
      begin
        if stCount < 2 then
          exit;
        dec(stCount, 2); // unstack two operands
        if not AddNode(position, st[stCount], st[stCount + 1]) then
          exit;
      end
      else
        exit; // invalid token
      // stack this operand
      if stCount > high(st) then
        exit; // paranoid
      st[stCount] := Count - 1;
      inc(stCount);
    end;
    inc(position, len);
  until position = StorageSize;
  result := (Count > 0) and
            (stCount = 1); // should end with no pending operand
end;

function TAceBinaryTree.AddNode(position, left, right: cardinal): boolean;
var
  n: PAceBinaryTreeNode;
begin
  result := (Count < high(Nodes)) and
            (position < StorageSize);
  if not result then
    exit;
  n := @Nodes[Count];
  n^.Position := position;
  n^.Left := left;
  n^.Right := right;
  inc(Count);
end;

procedure TAceBinaryTree.GetNodeText(index: byte; var u: RawUtf8);
var
  n: PAceBinaryTreeNode;
  v: PRawAceOperand;
  l, r: pointer; // store actual RawUtf8 variables
begin
  if index >= Count then
  begin
    FastAssignNew(u);
    exit;
  end;
  n := @Nodes[index];
  l := nil;
  r := nil;
  if n^.Left < Count then
    GetNodeText(n^.Left, RawUtf8(l)); // recursive resolution
  if n^.Right < Count then
    GetNodeText(n^.Right, RawUtf8(r));
  v := @Storage[n^.Position];
  if l <> nil then
    if r <> nil then
      SddlBinaryToText(v^.Token, RawUtf8(l), RawUtf8(r), u) // and release l+r
    else
      SddlUnaryToText(v^.Token, RawUtf8(l), u) // and release l
  else
    SddlOperandToText(v, ToTextDom, u);
end;

function TAceBinaryTree.ToBinary: RawByteString;
begin
  FastSetRawByteString(result, Storage, StorageSize); // FromBinary() value
end;

function TAceBinaryTree.ToText(dom: PSid): RawUtf8;
begin
  ToTextDom := dom;
  GetNodeText(Count - 1, result); // simple recursive generation
  ToTextDom := nil;
end;


{ TAceTextTree }

function TAceTextTree.AddNode(position, len: cardinal): integer;
var
  n: ^TAceTextTreeNode;
begin
  n := @Nodes[Count];
  n^.Token := TokenCurrent;
  n^.Position := position;
  n^.Length := len;
  n^.Left := 255;
  n^.Right := 255;
  result := Count;
  inc(Count);
end;

function TAceTextTree.ParseNextToken: integer;
var
  start: PUtf8Char;
  len: PtrUInt;
begin
  result := -1;
  if Count = MAX_TREE_NODE then
  begin
    Error := atpTooManyExpressions;
    exit;
  end;
  start := TextCurrent;
  while start^ = ' ' do
    inc(start);
  TextCurrent := start;
  if PtrUInt(start - Storage) >= StorageSize then // reached of input text
  begin
    TokenCurrent := sctInternalFinal; // SDDL may be within another expression
    exit;
  end;
  TokenCurrent := SddlNextOperand(TextCurrent);
  len := TextCurrent - start;
  if len > 255 then
    Error := atpTooBigExpression
  else
    case TokenCurrent of
      sctPadding:  // error parsing
        Error := atpInvalidExpression;
      sctInternalFinal .. high(TSecConditionalToken):
        ; // internal tokens are never serialized nor added to Nodes[]
    else
      result := AddNode(start - Storage, len);
    end;
end;

function TAceTextTree.ParseExpr: integer;
var
  t, l, r: integer;
begin
  result := -1;
  t := ParseNextToken;
  while Error = atpSuccess do
  begin
    case TokenCurrent of
      sctInternalFinal:
        exit;
      sctInternalParenthOpen:
        if ParentCount = 255 then
          Error := atpTooManyParenthesis
        else
        begin
          inc(ParentCount);
          result := ParseExpr;
        end;
      sctInternalParenthClose:
        if ParentCount = 0 then
          Error := atpMissingParenthesis
        else
        begin
          dec(ParentCount);
          exit;
        end;
      sctNot: // !( .... ) token has always parenthesis
        if ParentCount = 255 then
          Error := atpTooManyParenthesis
        else
        begin
          ParseNextToken;
          if TokenCurrent <> sctInternalParenthOpen then
            Error := atpMissingParenthesis
          else
          begin
            result := t;
            inc(ParentCount);
            Nodes[t].Left := ParseExpr;
          end;
        end;
    else
      if TokenCurrent in sctUnary then
      begin
        result := t;
        l := ParseNextToken; // sctAttribute, sctSid or sctComposite
        if l < 0 then
          Error := atpInvalidExpression
        else
          Nodes[t].Left := l;
      end
      else if TokenCurrent in sctLiteral + sctAttribute then
      begin
        l := t;
        result := ParseNextToken;
        r := -1;
        if TokenCurrent in sctBinaryOperator then
          r := ParseNextToken // sctAttribute, sctSid or sctComposite
        else if TokenCurrent in sctBinaryLogicalOperator then
          r := ParseExpr     // could be another expression
        else if TokenCurrent in [sctInternalFinal, sctInternalParenthClose] then
        begin
          result := t;       // e.g. '&& @Device.Bitlocker'
          continue;
        end
        else
          Error := atpInvalidExpression;
        if (Error = atpSuccess) and
           (r >= 0) then
          with Nodes[result] do
          begin
            Left := l;
            Right := r;
          end;
      end else if TokenCurrent in sctBinaryLogicalOperator then
        if result < 0 then
          Error := atpInvalidExpression
        else
        begin
          l := result;
          r := ParseExpr;
          result := t;
          if (Error = atpSuccess) and
             (r >= 0) then
            with Nodes[result] do
            begin
              Left := l;
              Right := r;
            end;
        end
      else
        Error := atpInvalidExpression;
    end;
    if (TokenCurrent = sctInternalFinal) or
       (Error <> atpSuccess) then
      break;
    t := ParseNextToken;
  end;
end;

function TAceTextTree.FromText(const Input: RawUtf8): TAceTextParse;
begin
  Storage := pointer(Input);
  StorageSize := length(Input);
  result := FromText;
end;

function TAceTextTree.FromText: TAceTextParse;
var
  r: integer;
begin
  result := atpNoExpression;
  Root := 255;
  Count := 0;
  ParentCount := 0;
  if (Storage = nil) or
     (Storage[0] <> '(') or
     (Storage[StorageSize - 1] <> ')') or
     (StorageSize >= MAX_TREE_BYTES) or
     (cardinal(StrLen(Storage)) < StorageSize) then
    exit; // not a valid SDDL text input for sure
  TextCurrent := Storage;
  TokenCurrent := sctPadding;
  Error := atpSuccess;
  r := ParseExpr;
  Root := r;
  if Error = atpSuccess then
    if r < 0 then
      Error := atpInvalidExpression
    else if ParentCount <> 0 then
      Error := atpMissingParenthesis;
  result := Error;
end;

function TAceTextTree.RawAppendBinary(var bin: TSynTempBuffer;
  const node: TAceTextTreeNode): boolean;

  procedure DoBytes(b: pointer; blen: PtrInt);
  var
    v: PRawAceOperand;
  begin
    if (blen = 0) and
       (node.Token <> sctUnicode) then
    begin
      Error := atpMissingExpression; // those tokens could not have length = 0
      exit;
    end;
    v := bin.Add(5 + blen);
    v^.Token := node.Token;
    v^.UnicodeBytes := blen;        // also v^.CompositeBytes/v^.SidBytes
    MoveFast(b^, v^.Unicode, blen); // also v^.Composite/v^.Sid
    result := true; // success
  end;

  // sub-functions to minimize stack usage on recursive RawAppendBinary() calls

  procedure DoComposite(p: PUtf8Char);
  var
    s: PUtf8Char;
    one: TAceTextTreeNode;
    items: TSynTempBuffer;
  begin
    items.InitOnStack;
    s := p + 1; // ignore trailing '{'
    repeat
      while s^ = ' ' do
        inc(s);
      one.Position := s - Storage;
      p := s;
      one.Token := SddlNextOperand(p);
      one.Length := p - s;
      if not (one.Token in sctLiteral) then
        Error := atpInvalidComposite
      else if not RawAppendBinary(items, one) then // allow recursive {..,{..}}
        break;
      s := p;
      while s^ = ' ' do
        inc(s);
      if s^ = '}' then
        break // end of sctComposite text block
      else if s^ <> ',' then
        Error := atpInvalidComposite
      else
        inc(s);
    until Error <> atpSuccess;
    if Error = atpSuccess then
      DoBytes(items.buf, items.added);
    items.Done;
  end;

  procedure DoSid(p: PUtf8Char);
  var
    sid: TSid;
  begin
    inc(p, 4); // ignore trailing 'SID(' chars
    if SddlNextSid(p, sid, ToBinaryDom) then
      DoBytes(@sid, SidLength(@sid))
    else
      Error := atpInvalidSid;
  end;

  procedure DoUnicode(p: PUtf8Char);
  var
    tmpw: TSynTempBuffer;
    l: PtrInt;
    w: PWideChar;
  begin
    l := node.Length;
    case node.Token of
      sctUnicode:
        begin
          inc(p);
          dec(l, 2); // trim "...." double quotes
        end;
     sctUserAttribute,
     sctResourceAttribute,
     sctDeviceAttribute:
       begin
         l := ord(ATTR_SDDL[node.Token][0]); // ignore e.g. trailing '@User.'
         inc(p, l); // no double quotes on identifiers
         l := node.Length - l;
       end;
    end;
    w := Unicode_ToUtf8(p, l, tmpw);
    if w = nil then
      Error := atpInvalidUnicode // invalid UTF-8 input
    else
      DoBytes(w, tmpw.len * 2);  // length in bytes
    tmpw.Done; // paranoid, since node.Length <= 255
  end;

var
  v: PRawAceOperand;
  p: pointer;
  b: PtrUInt;
begin
  result := false;
  p := Storage + node.Position;
  case node.Token of
    // sctLiteral tokens
    sctInt64:
      begin
        v := bin.Add(SizeOf(v^.Token) + SizeOf(v^.Int));
        v^.Token := node.Token;
        v^.Int.Value := GetInt64(p);
        v^.Int.Sign := scsNone;
        if v^.Int.Value < 0 then
          v^.Int.Sign := scsNegative;
        v^.Int.Base := scbDecimal;
        result := true;
      end;
    sctOctetString:
      begin
        b := node.Length shr 1;
        v := bin.Add((SizeOf(v^.Token) + SizeOf(v^.OctetBytes)) + b);
        v^.Token := node.Token;
        inc(PAnsiChar(p)); // ignore trailing '#'
        v^.OctetBytes := ParseHex(p, @v^.Octet, b) - p;
        if v^.OctetBytes = b then
          result := true
        else
          Error := atpInvalidOctet;
      end;
    sctComposite:  // e.g. '{"Sales","HR"}'
      DoComposite(p);
    sctSid:        // e.g. 'SID(BA)'
      DoSid(p);
    sctUnicode,
    // sctAttribute tokens
    sctLocalAttribute,
    sctUserAttribute,
    sctResourceAttribute,
    sctDeviceAttribute:
      DoUnicode(p);
  else
    if node.Token in sctOperator then // expect only single-byte token here
    begin
      PSecConditionalToken(bin.Add(1))^ := node.Token;
      result := true;
    end
    else
      Error := atpUnexpectedToken;
  end;
end;

function TAceTextTree.AppendBinary(var bin: TSynTempBuffer; node: integer): boolean;
var
  n: ^TAceTextTreeNode;
begin
  result := false;
  if cardinal(node) > cardinal(Count) then
    exit;
  n := @Nodes[node];
  if n^.Token >= sctInternalFinal then
    exit; // paranoid
  if n^.Left <> 255 then
    if not AppendBinary(bin, n^.Left) then
      exit;
  if n^.Right <> 255 then
    if not AppendBinary(bin, n^.Right) then
      exit;
  result := RawAppendBinary(bin, n^); // stored in reverse Polish order
end;

function TAceTextTree.ToBinary(dom: PSid): RawByteString;
var
  pad: PtrUInt;
  bin: TSynTempBuffer; // no temporary allocation needed
begin
  result := '';
  if (Count = 0) or
     (Error <> atpSuccess) or
     (Root >= Count) then
    exit;
  ToBinaryDom := dom;
  bin.InitOnStack;
  PCardinal(bin.Add(4))^ := ACE_CONDITION_SIGNATURE;
  if AppendBinary(bin, Root) then // recursive generation
  begin
    pad := bin.added and 3;
    if pad <> 0 then
      PCardinal(bin.Add(4 - pad))^ := 0; // should be DWORD-padded with zeros
    FastSetRawByteString(result, bin.buf, bin.added);
  end
  else if Error = atpSuccess then
    Error := atpInvalidContent; // if no Error was specified
  bin.Done;
  ToBinaryDom := nil;
end;

function TAceTextTree.ToText: RawUtf8;
begin
  FastSetString(result, Storage, StorageSize); // FromText() value
end;


{ ****************** TSecurityDescriptor Wrapper Object }

function IsValidSecurityDescriptor(p: PByteArray; len: cardinal): boolean;
begin
  result :=
    // main buffer coherency
    (p <> nil) and
    (len > SizeOf(TRawSD)) and
    (len and 3 = 0) and // should be DWORD-aligned
    // header
    (PRawSD(p)^.Revision = 1) and
    (PRawSD(p)^.Sbz1 = 0) and
    (scSelfRelative in PRawSD(p)^.Control) and
    // owner SID consistency
    (PRawSD(p)^.Owner < len) and
    (cardinal(SidLength(@p[PRawSD(p)^.Owner])) + PRawSD(p)^.Owner <= len) and
    // group SID consistency
    (PRawSD(p)^.Group < len) and
    (cardinal(SidLength(@p[PRawSD(p)^.Group])) + PRawSD(p)^.Group  <= len) and
    // DACL consistency
    (PRawSD(p)^.Dacl < len) and
    ((PRawSD(p)^.Dacl <> 0) = (scDaclPresent in PRawSD(p)^.Control)) and
    ((PRawSD(p)^.Dacl = 0) or
     (PRawAcl(@p[PRawSD(p)^.Dacl])^.AclSize + PRawSD(p)^.Dacl <= len)) and
    // SACL consistency
    (PRawSD(p)^.Sacl < len) and
    ((PRawSD(p)^.Sacl <> 0) = (scSaclPresent in PRawSD(p)^.Control)) and
    ((PRawSD(p)^.Sacl = 0) or
     (PRawAcl(@p[PRawSD(p)^.Sacl])^.AclSize + PRawSD(p)^.Sacl <= len));
end;

function SecurityDescriptorToText(const sd: RawSecurityDescriptor;
  var text: RawUtf8; dom: PSid; uuid: TAppendShortUuid): boolean;
var
  tmp: TSecurityDescriptor;
  buf: TSynTempBuffer;
begin
  result := tmp.FromBinary(sd);
  if not result then
    exit; // returns false, and don't change the text value on rendering error
  buf.InitOnStack;
  tmp.AppendAsText(buf, dom, uuid);
  buf.Done(text, CP_UTF8);
end;


{ TSecurityDescriptor }

procedure TSecurityDescriptor.Clear;
begin
  Finalize(self);
  Flags := [scSelfRelative];
  Modified := [];
end;

function TSecurityDescriptor.IsEqual(const sd: TSecurityDescriptor): boolean;
var
  i: PtrInt;
begin
  result := false;
  if (Owner <> sd.Owner) or
     (Group <> sd.Group) or
     (Flags <> sd.Flags) or
     (length(Dacl) <> length(sd.Dacl)) or
     (length(Sacl) <> length(sd.Sacl)) then
    exit;
  for i := 0 to high(Dacl) do
    if not Dacl[i].IsEqual(sd.Dacl[i]) then
      exit;
  for i := 0 to high(Sacl) do
    if not Sacl[i].IsEqual(sd.Sacl[i]) then
      exit;
  result := true;
end;

function TSecurityDescriptor.FromBinary(const Bin: RawSecurityDescriptor): boolean;
begin
  result := FromBinary(pointer(Bin), length(Bin));
end;

function TSecurityDescriptor.FromBinary(p: PByteArray; len: cardinal): boolean;
begin
  Clear;
  result := IsValidSecurityDescriptor(p, len) and
            FromBinary(p);
end;

function TSecurityDescriptor.FromBinary(p: PByteArray): boolean;
begin
  Clear;
  result := false;
  if p = nil then
    exit;
  if PRawSD(p)^.Owner <> 0 then
    ToRawSid(@p[PRawSD(p)^.Owner], Owner);
  if PRawSD(p)^.Group <> 0 then
    ToRawSid(@p[PRawSD(p)^.Group], Group);
  Flags := PRawSD(p)^.Control;
  result := BinToSecAcl(p, PRawSD(p)^.Sacl, Sacl) and
            BinToSecAcl(p, PRawSD(p)^.Dacl, Dacl);
end;

function TSecurityDescriptor.ToBinary: RawSecurityDescriptor;
var
  p: PAnsiChar;
  hdr: PRawSD;
begin
  FastNewRawByteString(RawByteString(result),
    SizeOf(hdr^) + length(Owner) + length(Group) +
    SecAclToBin(nil, Sacl) + SecAclToBin(nil, Dacl));
  p := pointer(result);
  hdr := pointer(p);
  FillCharFast(hdr^, SizeOf(hdr^), 0);
  hdr^.Revision := 1;
  hdr^.Control := Flags + [scSelfRelative];
  inc(PRawSD(p));
  if Owner <> '' then
  begin
    hdr^.Owner := p - pointer(result);
    MoveFast(pointer(Owner)^, p^, length(Owner));
    inc(p, length(Owner));
  end;
  if Group <> '' then
  begin
    hdr^.Group := p - pointer(result);
    MoveFast(pointer(Group)^, p^, length(Group));
    inc(p, length(Group));
  end;
  if Sacl <> nil then
  begin
    include(hdr^.Control, scSaclPresent);
    hdr^.Sacl := p - pointer(result);
    inc(p, SecAclToBin(p, Sacl));
  end;
  if Dacl <> nil then
  begin
    include(hdr^.Control, scDaclPresent);
    hdr^.Dacl := p - pointer(result);
    inc(p, SecAclToBin(p, Dacl));
  end;
  if p - pointer(result) <> length(result) then
    raise EOSSecurity.Create('TSecurityDescriptor.ToBinary'); // paranoid
end;

const
  SCOPE_SDDL: array[TSecAceScope] of string[3] = (
    'D:', 'S:');
  SCOPE_P: array[TSecAceScope] of TSecControl = (
    scDaclProtected, scSaclProtected);
  SCOPE_AR: array[TSecAceScope] of TSecControl = (
    scDaclAutoInheritReq, scSaclAutoInheritReq);
  SCOPE_AI: array[TSecAceScope] of TSecControl = (
    scDaclAutoInherit, scSaclAutoInherit);
  SCOPE_FLAG: array[TSecAceScope] of TSecControl = (
    scDaclPresent, scSaclPresent);

function TSecurityDescriptor.NextAclFromText(var p: PUtf8Char; dom: PSid;
  uuid: TShortToUuid; scope: TSecAceScope): TAceTextParse;
var
  acl: PSecAcl;
begin
  while p^ = ' ' do
    inc(p);
  if p^ = 'P' then
  begin
    include(Flags, SCOPE_P[scope]);
    inc(p);
  end;
  while p^ = ' ' do
    inc(p);
  if PWord(p)^ = ord('A') + ord('R') shl 8 then
  begin
    include(Flags, SCOPE_AR[scope]);
    inc(p, 2);
  end;
  while p^ = ' ' do
    inc(p);
  if PWord(p)^ = ord('A') + ord('I') shl 8 then
  begin
    include(Flags, SCOPE_AI[scope]);
    inc(p, 2);
  end;
  while p^ = ' ' do
    inc(p);
  result := atpMissingParenthesis;
  if p^ <> '(' then
    exit;
  repeat
    inc(p);
    result := InternalAdd(scope, acl).FromText(p, dom, uuid);
    if result <> atpSuccess then
      exit;
    inc(p); // p^ = ')'
  until p^ <> '(';
end;

procedure TSecurityDescriptor.AclToText(var sddl: TSynTempBuffer;
  dom: PSid; uuid: TAppendShortUuid; scope: TSecAceScope);
var
  tmp: ShortString;
  acl: PSecAcl;
  i: Ptrint;
begin
  PCardinal(@tmp)^ := PCardinal(@SCOPE_SDDL[scope])^;
  if SCOPE_P[scope] in Flags then
    AppendShortChar('P', @tmp);
  if SCOPE_AR[scope] in Flags then
    AppendShortTwoChars('AR', @tmp);
  if SCOPE_AI[scope] in Flags then
    AppendShortTwoChars('AI', @tmp);
  acl := @Dacl;
  if scope = sasSacl then
    acl := @Sacl;
  for i := 0 to length(acl^) - 1 do
    acl^[i].AppendAsText(tmp, sddl, dom, uuid);
end;

function TSecurityDescriptor.FromText(var p: PUtf8Char;
  dom: PSid; uuid: TShortToUuid; endchar: AnsiChar): TAceTextParse;
begin
  Clear;
  result := atpMissingExpression;
  if p = nil then
    exit;
  if not Assigned(@uuid) then
    uuid := @ShortToUuid; // default UUID standard text parsing
  repeat
    while p^ = ' ' do
      inc(p);
    result := atpMissingExpression;
    if p[1] <> ':' then
      exit;
    inc(p, 2);
    result := atpSuccess;
    case p[-2] of
      'O':
        if not SddlNextSid(p, Owner, dom) then
          result := atpInvalidOwner;
      'G':
        if not SddlNextSid(p, Group, dom) then
          result := atpInvalidGroup;
      'D':
        result := NextAclFromText(p, dom, uuid, sasDacl);
      'S':
        result := NextAclFromText(p, dom, uuid, sasSacl);
    else
      result := atpInvalidContent;
    end;
    if result <> atpSuccess then
      exit;
    while p^ = ' ' do
      inc(p);
  until (p^ = #0) or (p^ = endchar);
  if Dacl <> nil then
    include(Flags, scDaclPresent);
  if Sacl <> nil then
    include(Flags, scSaclPresent);
end;

function TSecurityDescriptor.FromText(
  const SddlText, RidDomain: RawUtf8; uuid: TShortToUuid): TAceTextParse;
var
  p: PUtf8Char;
  dom: RawSid;
begin
  p := pointer(SddlText);
  result := atpInvalidSid;
  if TryDomainTextToSid(RidDomain, dom) then
    result := FromText(p, pointer(dom), uuid);
end;

function TSecurityDescriptor.ToText(const RidDomain: RawUtf8;
  uuid: TAppendShortUuid): RawUtf8;
var
  dom: RawSid;
  tmp: TSynTempBuffer;
begin
  result := '';
  if not TryDomainTextToSid(RidDomain, dom) then
    exit;
  tmp.InitOnStack;
  AppendAsText(tmp, pointer(dom), uuid);
  tmp.Done(result, CP_UTF8);
end;

procedure TSecurityDescriptor.AppendAsText(var sddl: TSynTempBuffer;
  dom: PSid; uuid: TAppendShortUuid);
var
  tmp: ShortString;
begin
  tmp[0] := #0;
  if Owner <> '' then
  begin
    tmp := 'O:';
    SddlAppendSid(tmp, pointer(Owner), dom);
  end;
  if Group <> '' then
  begin
    AppendShortTwoChars('G:', @tmp);
    SddlAppendSid(tmp, pointer(Group), dom);
  end;
  sddl.AddShort(tmp);
  if not Assigned(@uuid) then
    uuid := @AppendShortUuid; // default append as UUID standard text
  AclToText(sddl, dom, uuid, sasDacl);
  AclToText(sddl, dom, uuid, sasSacl);
end;

function TSecurityDescriptor.InternalAdd(scope: TSecAceScope;
  out acl: PSecAcl): PSecAce;
var
  n: PtrInt;
begin
  acl := @Dacl;
  if scope = sasSacl then
    acl := @Sacl;
  n := length(acl^);
  SetLength(acl^, n + 1); // append
  result := @acl^[n];
end;

function TSecurityDescriptor.InternalAdded(scope: TSecAceScope; ace: PSecAce;
  acl: PSecAcl; success: boolean): PSecAce;
begin
  if success then
  begin
    include(Flags, SCOPE_FLAG[scope]);
    if scope = sasSacl then
      include(Modified, sdiSacl)
    else
      include(Modified, sdiDacl);
    result := ace;
  end
  else
  begin
    SetLength(acl^, length(acl^) - 1); // abort
    if acl^ = nil then
      exclude(Flags, SCOPE_FLAG[scope]);
    result := nil;
  end;
end;

function TSecurityDescriptor.Add(sat: TSecAceType;
  const sidText, maskSddl: RawUtf8; dom: PSid; const condExp: RawUtf8;
  scope: TSecAceScope; saf: TSecAceFlags): PSecAce;
var
  acl: PSecAcl;
begin
  result := InternalAdd(scope, acl);
  result := InternalAdded(scope, result, acl,
    result^.Fill(sat, sidText, maskSddl, dom, condExp, saf));
end;

function TSecurityDescriptor.Add(const sddl: RawUtf8; dom: PSid;
  uuid: TShortToUuid; scope: TSecAceScope): PSecAce;
var
  p: PUtf8Char;
  acl: PSecAcl;
begin
  result := nil;
  p := pointer(sddl);
  if (p = nil) or
     (p^ <> '(') then
    exit;
  inc(p);
  result := InternalAdd(scope, acl);
  result := InternalAdded(scope, result, acl,
    result^.FromText(p, dom, uuid) = atpSuccess);
end;

procedure TSecurityDescriptor.Delete(index: PtrUInt; scope: TSecAceScope);
var
  dest: PSecAcl;
  n: PtrUInt;
begin
  dest := @Dacl;
  if scope = sasSacl then
    dest := @Sacl;
  n := length(dest^);
  if index >= n then
    exit;
  Finalize(dest^[index]);
  dec(n);
  if n = 0 then
  begin
    dest^ := nil;
    exclude(Flags, SCOPE_FLAG[scope]);
  end
  else
    DynArrayFakeDelete(dest^, index, n, SizeOf(dest^[0]));
  if scope = sasSacl then
    include(Modified, sdiSacl)
  else
    include(Modified, sdiDacl);
end;

function TSecurityDescriptor.ReplaceDomain(const OldDomain, NewDomain: RawUtf8;
  maxRid: cardinal): integer;
var
  old, new: RawSid;
begin
  if TryDomainTextToSid(OldDomain, old) and
     TryDomainTextToSid(NewDomain, new) then
     if OldDomain = NewDomain then
       result := 0
     else
       result := ReplaceDomainRaw(pointer(old), pointer(new), maxRid)
  else
    result := -1;
end;

procedure Apply(var modified: TSecurityDescriptorInfos;
  sdi: TSecurityDescriptorInfo; var result: integer; new: integer);
  {$ifdef HASINLINE} inline; {$endif}
begin
  if new = 0 then
    exit;
  inc(result, new);
  include(modified, sdi);
end;

function TSecurityDescriptor.ReplaceDomainRaw(OldDomain, NewDomain: PSid;
  maxRid: cardinal): integer;
begin
  result := -1;
  if not SidIsDomain(OldDomain) or
     not SidIsDomain(NewDomain) then
    exit;
  result := 0;
  Apply(Modified, sdiOwner, result,
    SidReplaceDomain(OldDomain, NewDomain, maxRid, Owner));
  Apply(Modified, sdiGroup, result,
    SidReplaceDomain(OldDomain, NewDomain, maxRid, Group));
  Apply(Modified, sdiDacl, result,
    AclReplaceDomainRaw(OldDomain, NewDomain, maxRid, Dacl));
  Apply(Modified, sdiSacl, result,
    AclReplaceDomainRaw(OldDomain, NewDomain, maxRid, Sacl));
end;

function TSecurityDescriptor.ReplaceSid(const OldSid, NewSid: RawUtf8): integer;
begin
  result := ReplaceSid([OldSid], [NewSid]);
end;

function TSecurityDescriptor.ReplaceSid(const OldSid, NewSid: array of RawUtf8): integer;
var
  old, new: RawSidDynArray;
begin
  result := -1;
  if (length(OldSid) <> length(NewSid)) or
     not TextToRawSidArray(OldSid, old) or
     not TextToRawSidArray(NewSid, new) then
    exit;
  result := ReplaceSidRaw(old, new)
end;

function TSecurityDescriptor.ReplaceSidRaw(const OldSid, NewSid: RawSidDynArray): integer;
var
  i: PtrInt;
begin
  result := -1;
  if length(OldSid) <> length(NewSid) then
    exit;
  for i := 0 to length(OldSid) - 1 do
    if length(OldSid[i]) <> length(NewSid[i]) then
      exit; // old/new SID lengths should match for in-place Opaque replacement
  result := 0;
  Apply(Modified, sdiOwner, result, SidReplaceAny(OldSid, NewSid, Owner));
  Apply(Modified, sdiGroup, result, SidReplaceAny(OldSid, NewSid, Group));
  Apply(Modified, sdiDacl,  result, AclReplaceAny(OldSid, NewSid, Dacl));
  Apply(Modified, sdiSacl,  result, AclReplaceAny(OldSid, NewSid, Sacl));
end;


{ ****************** Windows API Specific Security Types and Functions }

{$ifdef OSWINDOWS}

function LookupAccountSidW(lpSystemName: PWideChar; Sid: PSID; Name: PWideChar;
  var cchName: DWord; ReferencedDomainName: PAnsiChar;
  var cchReferencedDomainName: DWord; var peUse: DWord): BOOL;
    stdcall; external advapi32;

type
  TOKEN_USER = record
    User: SID_AND_ATTRIBUTES;
  end;
  PTOKEN_USER = ^TOKEN_USER;

function RawTokenSid(tok: THandle; var buf: TSynTempBuffer): PSid;
begin
  if RawTokenGetInfo(tok, TokenUser, buf) >= SizeOf(TOKEN_USER) then
    result := PSid(PTOKEN_USER(buf.buf)^.User.Sid) // within buf.buf/len
  else
    result := nil;
end;

function CurrentSid(wtt: TWinTokenType; name, domain: PRawUtf8): RawUtf8;
var
  sid: RawSid;
begin
  CurrentRawSid(sid, wtt, name, domain);
  result := RawSidToText(sid);
end;

procedure CurrentRawSid(out sid: RawSid; wtt: TWinTokenType;
  name, domain: PRawUtf8);
var
  h: THandle;
  p: PSid;
  n, d: RawUtf8;
  tmp: TSynTempBuffer;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  p := RawTokenSid(h, tmp);
  if p <> nil then
  begin
    ToRawSid(p, sid);
    if (name <> nil) or
       (domain <> nil) then
    begin
      LookupSid(p, n, d);
      if name <> nil then
        name^ := n;
      if domain <> nil then
        domain^ := d;
    end;
  end;
  tmp.Done;
  CloseHandle(h);
end;

function RawTokenGroups(tok: THandle; var buf: TSynTempBuffer): PSids;
var
  nfo: PTokenGroups;
  i: PtrInt;
begin
  result := nil;
  if RawTokenGetInfo(tok, TokenGroups, buf) < SizeOf({%H-}nfo^) then
    exit;
  nfo := buf.buf;
  if nfo.GroupCount = 0 then
    exit;
  SetLength(result, nfo.GroupCount);
  for i := 0 to nfo.GroupCount - 1 do
    result[i] := pointer(nfo.Groups[i].Sid); // within buf.buf/len
end;

function TokenGroupsText(tok: THandle): TRawUtf8DynArray;
var
  tmp: TSynTempBuffer;
begin
  result := SidsToText(RawTokenGroups(tok, tmp));
  tmp.Done;
end;

function TokenHasGroup(tok: THandle; sid: PSid): boolean;
var
  tmp: TSynTempBuffer;
  i: PtrInt;
begin
  result := false;
  if (sid <> nil) and
     (RawTokenGetInfo(tok, TokenGroups, tmp) <> 0) then
    with PTokenGroups(tmp.buf)^ do
      for i := 0 to GroupCount - 1 do
        if SidCompare(pointer(Groups[i].Sid), sid) = 0 then
        begin
          result := true;
          break;
        end;
  tmp.Done;
end;

function TokenHasAnyGroup(tok: THandle; const sid: RawSidDynArray): boolean;
var
  tmp: TSynTempBuffer;
begin
  result := HasAnySid(RawTokenGroups(tok, tmp), sid);
  tmp.Done;
end;

function CurrentGroups(wtt: TWinTokenType; var tmp: TSynTempBuffer): PSids;
var
  h: THandle;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  result := RawTokenGroups(h, tmp);
  CloseHandle(h);
end;

function CurrentGroupsSid(wtt: TWinTokenType): TRawUtf8DynArray;
var
  tmp: TSynTempBuffer;
begin
  result := SidsToText(CurrentGroups(wtt, tmp));
  tmp.Done;
end;

function CurrentKnownGroups(wtt: TWinTokenType): TWellKnownSids;
var
  tmp: TSynTempBuffer;
begin
  result := SidToKnownGroups(CurrentGroups(wtt, tmp));
  tmp.Done;
end;

function CurrentUserHasGroup(sid: PSid; wtt: TWinTokenType): boolean;
var
  h: THandle;
begin
  h := RawTokenOpen(wtt, TOKEN_QUERY);
  result := TokenHasGroup(h, sid);
  CloseHandle(h);
end;

function CurrentUserHasGroup(wks: TWellKnownSid; wtt: TWinTokenType): boolean;
begin
  result := (wks <> wksNull) and
            CurrentUserHasGroup(pointer(KnownRawSid(wks)), wtt);
end;

function CurrentUserHasGroup(const sid: RawUtf8; wtt: TWinTokenType): boolean;
var
  s: TSid;
  p: PUtf8Char;
begin
  p := pointer(sid);
  result := TextToSid(p, s) and
            (p^ = #0) and
            CurrentUserHasGroup(@s, wtt);
end;

function CurrentUserHasAnyGroup(const sid: RawSidDynArray; wtt: TWinTokenType): boolean;
var
  tmp: TSynTempBuffer;
begin
  result := HasAnySid(CurrentGroups(wtt, tmp), sid);
  tmp.Done;
end;

function CurrentUserHasGroup(const name, domain, server: RawUtf8;
  wtt: TWinTokenType): boolean;
var
  i: PtrInt;
  sids: PSids;
  n, d: RawUtf8;
  tmp: TSynTempBuffer;
begin
  result := false;
  sids := CurrentGroups(wtt, tmp);
  for i := 0 to length(sids) - 1 do
    if (SidToKnown(sids[i]) = wksNull) and
       (LookupSid(sids[i], n, d, server) = stTypeGroup) then
      if PropNameEquals(n, name) and
         ((domain = '') or
          PropNameEquals(d, domain)) then
      begin
        result := true;
        break;
      end;
  tmp.Done;
end;

function CurrentUserIsAdmin: boolean;
begin
  result := CurrentUserHasGroup(wksBuiltinAdministrators);
end;

function LookupSid(sid: PSid; out name, domain: RawUtf8;
  const server: RawUtf8): TSidType;
var
  n, d: array[byte] of WideChar;
  s: TSynTempBuffer;
  nl, dl, use: cardinal;
begin
  result := stUndefined;
  if sid = nil then
    exit;
  nl := SizeOf(n);
  dl := SizeOf(d);
  if LookupAccountSidW(
       Utf8ToWin32PWideChar(server, s), sid, @n, nl, @d, dl, use) then
  begin
    Win32PWideCharToUtf8(@n, name);
    Win32PWideCharToUtf8(@d, domain);
    if use <= byte(high(TSidType)) then
      result := TSidType(use);
  end;
  s.Done;
end;

function LookupSid(const sid: RawUtf8; out name, domain: RawUtf8;
  const server: RawUtf8): TSidType;
var
  s: TSid;
  p: PUtf8Char;
begin
  p := pointer(sid);
  if TextToSid(p, s) and
     (p^ = #0) then
    result := LookupSid(@s, name, domain, server)
  else
    result := stUndefined;
end;

function LookupToken(tok: THandle; out name, domain: RawUtf8;
  const server: RawUtf8): boolean;
var
  tmp: TSynTempBuffer;
  sid: PSid;
begin
  sid := RawTokenSid(tok, tmp);
  result := LookupSid(sid, name, domain, server) <> stUndefined;
  tmp.Done;
end;

function LookupToken(tok: THandle; const server: RawUtf8): RawUtf8;
var
  name, domain: RawUtf8;
begin
  if LookupToken(tok, name, domain, server) then
    result := domain + '\' + name
  else
    result := '';
end;


function GetSystemSecurityDescriptor(const fn: TFileName;
  out dest: TSecurityDescriptor; info: TSecurityDescriptorInfos;
  kind: TNamedResourceType; privileges: TWinSystemPrivileges): boolean;
var
  tmp: TW32Temp;
  priv: TSynWindowsPrivileges;
  sd: PSECURITY_DESCRIPTOR;
  bak: integer;
begin
  dest.Clear;
  result := false;
  if (info = []) or
     (fn = '') then
    exit;
  if privileges <> [] then
  begin
    priv.Init;
    priv.Enable(privileges);
  end;
  sd := nil;
  result := GetNamedSecurityInfoW(
    W32(fn, tmp), ord(kind), byte(info), nil, nil, nil, nil, sd) = 0;
  bak := GetLastError;
  if privileges <> [] then
    priv.Done; // restore initial privileges ASAP
  if result then
    result := dest.FromBinary(pointer(sd)); // assume OS input is safe
  if sd <> nil then
    LocalFree(HLOCAL(sd));
  if not result then
    SetLastError(bak); // so that WinLastError / RaiseLastError would work
end;

function SetSystemSecurityDescriptor(const fn: TFileName;
  const dest: TSecurityDescriptor; info: TSecurityDescriptorInfos;
  kind: TNamedResourceType; privileges: TWinSystemPrivileges): boolean;
var
  tmp: TW32Temp;
  priv: TSynWindowsPrivileges;
  o, g, d, s: pointer;
  bak: integer;
begin
  result := false;
  if info = [] then
    info := dest.Modified;
  if (info = []) or
     (fn = '') then
    exit;
  if privileges <> [] then
  begin
    priv.Init;
    priv.Enable(privileges);
  end;
  o := nil;
  g := nil;
  d := nil;
  s := nil;
  if sdiOwner in info then
    o := pointer(dest.Owner);
  if sdiGroup in info then
    g := pointer(dest.Group);
  if sdiDacl in info then
    d := pointer(SecAclToBinary(dest.Dacl));
  if sdiSacl in info then
    s := pointer(SecAclToBinary(dest.Sacl));
  result := SetNamedSecurityInfoW(
    W32(fn, tmp), ord(kind), byte(info), o, g, d, s) = 0;
  bak := GetLastError;
  if privileges <> [] then
    priv.Done; // restore initial privileges ASAP
  if not result then
    SetLastError(bak); // so that WinLastError / RaiseLastError would work
end;

{$endif OSWINDOWS}


procedure InitializeUnit;
var
  wks: TWellKnownSid;
  wkr: TWellKnownRid;
  sam: TSecAccess;
  i: PtrInt;
begin
  for i := low(SDDL_WKS) to high(SDDL_WKS) do
  begin
    wks := SDDL_WKS[i];
    include(wksWithSddl, wks);
    SDDL_WKS_INDEX[wks] := i;
  end;
  for i := low(SDDL_WKR) to high(SDDL_WKR) do
  begin
    wkr := SDDL_WKR[i];
    include(wkrWithSddl, wkr);
    SDDL_WKR_INDEX[wkr] := i + high(SDDL_WKS);
  end;
  for i := low(SDDL_OPER) to high(SDDL_OPER) do
    SDDL_OPER_INDEX[SDDL_OPER[i]] := i;
  for sam := low(sam) to high(sam) do
    if SAM_SDDL[sam][0] <> #0  then
      include(samWithSddl, sam);
end;

initialization
  InitializeUnit;

end.

