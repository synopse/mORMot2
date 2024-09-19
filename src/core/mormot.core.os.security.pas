/// Framework Core Definitions of Operating System Security
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os.security;

{
  *****************************************************************************

  Cross-Platform Operating System Security Definitions
  - Security IDentifier (SID) Definitions
  - Security Descriptor Definition Language (SDDL) Definitions
  - Discretionary Access Control List (DACL) Definitions
  - Windows API Specific Security Types and Functions

  Even if most of those security definitions comes from the Windows/AD world,
    our framework (re)implemented them in a cross-platform way.
  This low-level unit only refers to mormot.core.base and mormot.core.os.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os;


{ ****************** Security IDentifier (SID) Definitions }

type
  /// Security IDentifier (SID) Authority, encoded as 48-bit binary
  TSidAuth = array[0..5] of byte;
  PSidAuth = ^TSidAuth;

  /// Security IDentifier (SID) binary format, as retrieved e.g. by Windows API
  // - this definition is not detailed on oldest Delphi, and not available on
  // POSIX, whereas it makes sense to also have it, e.g. for server process
  TSid = packed record
    Revision: byte;
    SubAuthorityCount: byte;
    IdentifierAuthority: TSidAuth;
    SubAuthority: array[byte] of cardinal;
  end;
  PSid = ^TSid;
  PSids = array of PSid;

  /// define a list of well-known Security IDentifier (SID) groups
  // - for instance, wksBuiltinAdministrators is set for local administrators
  // - warning: does not exactly match winnt.h WELL_KNOWN_SID_TYPE enumeration
  TWellKnownSid = (
    wksNull,                                      // S-1-0-0
    wksWorld,                                     // S-1-1-0       WD
    wksLocal,                                     // S-1-2-0
    wksConsoleLogon,                              // S-1-2-1
    wksCreatorOwner,                              // S-1-3-0       CO
    wksCreatorGroup,                              // S-1-3-1       CG
    wksCreatorOwnerServer,                        // S-1-3-2
    wksCreatorGroupServer,                        // S-1-3-3
    wksIntegrityUntrusted,                        // S-1-16-0
    wksIntegrityLow,                              // S-1-16-4096
    wksIntegrityMedium,                           // S-1-16-8192
    wksIntegrityMediumPlus,                       // S-1-16-8448
    wksIntegrityHigh,                             // S-1-16-12288
    wksIntegritySystem,                           // S-1-16-16384
    wksIntegrityProtectedProcess,                 // S-1-16-20480
    wksIntegritySecureProcess,                    // S-1-16-28672
    wksAuthenticationAuthorityAsserted,           // S-1-18-1
    wksAuthenticationServiceAsserted,             // S-1-18-2
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
    wksEnterpriseControllers,                     // S-1-5-9
    wksSelf,                                      // S-1-5-10      PS
    wksAuthenticatedUser,                         // S-1-5-11      AU
    wksRestrictedCode,                            // S-1-5-12      RC
    wksTerminalServer,                            // S-1-5-13
    wksRemoteLogonId,                             // S-1-5-14
    wksThisOrganisation,                          // S-1-5-15
    wksIisUser,                                   // S-1-5-17
    wksLocalSystem,                               // S-1-5-18      SY
    wksLocalService,                              // S-1-5-19
    wksNetworkService,                            // S-1-5-20
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
    wksBuiltinRasServers,                         // S-1-5-32-553  RS
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
    wksBuiltinRdsManagementServers,               // S-1-5-32-577
    wksBuiltinHyperVAdmins,                       // S-1-5-32-578  HA
    wksBuiltinAccessControlAssistanceOperators,   // S-1-5-32-579  AA
    wksBuiltinRemoteManagementUsers,              // S-1-5-32-580
    wksBuiltinDefaultSystemManagedGroup,          // S-1-5-32-581
    wksBuiltinStorageReplicaAdmins,               // S-1-5-32-582
    wksBuiltinDeviceOwners,                       // S-1-5-32-583
    wksBuiltinWriteRestrictedCode,                // S-1-5-33      WR
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
    wksBuiltinAnyPackage,                         // S-1-15-2-1
    wksBuiltinAnyRestrictedPackage,               // S-1-15-2-2
    wksNtlmAuthentication,                        // S-1-5-64-10
    wksSChannelAuthentication,                    // S-1-5-64-14
    wksDigestAuthentication);                     // S-1-5-64-21

  /// define a set of well-known SID
  TWellKnownSids = set of TWellKnownSid;

  /// define a list of well-known domain relative sub-authority RID values
  TWellKnownRid = (
    wkrGroupReadOnly,                // DOMAIN_GROUP_RID_ENTERPRISE_READONLY_DOMAIN_CONTROLLERS RO
    wkrUserAdmin,                    // DOMAIN_USER_RID_ADMIN  LA
    wkrUserGuest,                    // DOMAIN_USER_RID_GUEST  LG
    wkrServiceKrbtgt,                // DOMAIN_USER_RID_KRBTGT
    wkrGroupAdmins,                  // DOMAIN_GROUP_RID_ADMINS DA
    wkrGroupUsers,                   // DOMAIN_GROUP_RID_USERS DU
    wkrGroupGuests,                  // DOMAIN_GROUP_RID_GUESTS DG
    wkrGroupComputers,               // DOMAIN_GROUP_RID_COMPUTERS DC
    wkrGroupControllers,             // DOMAIN_GROUP_RID_CONTROLLERS DD
    wkrGroupCertAdmins,              // DOMAIN_GROUP_RID_CERT_ADMINS     CA
    wkrGroupSchemaAdmins,            // DOMAIN_GROUP_RID_SCHEMA_ADMINS   SA
    wkrGroupEntrepriseAdmins,        // DOMAIN_GROUP_RID_ENTERPRISE_ADMINS EA
    wkrGroupPolicyAdmins,            // DOMAIN_GROUP_RID_POLICY_ADMINS  PA
    wkrGroupReadOnlyControllers,     // DOMAIN_GROUP_RID_READONLY_CONTROLLERS
    wkrGroupCloneableControllers,    // DOMAIN_GROUP_RID_CLONEABLE_CONTROLLERS CN
    wkrGroupProtectedUsers,          // DOMAIN_GROUP_RID_PROTECTED_USERS AP
    wkrGroupKeyAdmins,               // DOMAIN_GROUP_RID_KEY_ADMINS  KA
    wkrGroupEntrepriseKeyAdmins,     // DOMAIN_GROUP_RID_ENTERPRISE_KEY_ADMINS EK
    wkrSecurityMandatoryLow,         // SECURITY_MANDATORY_LOW_RID    LW
    wkrSecurityMandatoryMedium,      // SECURITY_MANDATORY_MEDIUM_RID ME
    wkrSecurityMandatoryMediumPlus,  // SECURITY_MANDATORY_MEDIUM_PLUS_RID MP
    wkrSecurityMandatoryHigh,        // SECURITY_MANDATORY_HIGH_RID   HI
    wkrSecurityMandatorySystem);     // SECURITY_MANDATORY_SYSTEM_RID SI

  /// define a set of well-known RID
  TWellKnownRids = set of TWellKnownRid;

  /// custom binary buffer type used as convenient Windows SID storage
  RawSid = type RawByteString;

  /// a dynamic array of binary SID storage buffers
  RawSidDynArray = array of RawSid;


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

/// convert a Security IDentifier as text, following the standard representation
procedure SidToTextShort(sid: PSid; var result: shortstring);

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

/// returns a Security IDentifier of a well-known SID as binary
// - is using an internal cache for the returned RawSid instances
function KnownRawSid(wks: TWellKnownSid): RawSid; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// returns a Security IDentifier of a well-known SID as binary
procedure KnownRawSid(wks: TWellKnownSid; var sid: RawSid); overload;

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

/// decode a domain SID text into a generic binary RID value
// - returns true if Domain is '', or is in its 'S-1-5-21-xx-xx-xx' domain form
// - will also accepts any 'S-1-5-21-xx-xx-xx-yyy' form, e.g. the current user SID
// - if a domain SID, Dom binary buffer will contain a S-1-5-21-xx-xx-xx-0 value,
// ready to be used with KnownRidSid(), SameDomain(), SddlAppendSid(),
// SddlNextSid() or TSecDesc.AppendAsText functions
function TryDomainTextToSid(const Domain: RawUtf8; out Dom: RawSid): boolean;

/// compute a binary SID from a decoded binary Domain and a well-known RID
// - more efficient than KnownRawSid() overload and KnownSidToText()
procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: RawSid);

/// quickly check if two binary SID buffers domain do overlap
// - one the values should be a domain SID in S-1-5-21-xx-xx-xx-RID layout
function SameDomain(a, b: PSid): boolean;
  {$ifdef HASINLINE} inline; {$endif}


{ ****************** Security Descriptor Definition Language (SDDL) Definitions }

/// append a binary SID in its SDDL text form
// - recognize TWellKnownSid into SDDL text, e.g. S-1-1-0 (wksWorld) into 'WD'
// - with optional TWellKnownRid recognition if dom binary is supplied, e.g.
// S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
procedure SddlAppendSid(var s: shortstring; sid, dom: PSid);

/// parse a SID from its SDDL text form into its binary buffer
// - recognize TWellKnownSid SDDL identifiers, e.g. 'WD' into S-1-1-0 (wksWorld)
// - with optional TWellKnownRid recognition if dom binary is supplied, e.g.
// 'DA' identifier into S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins)
function SddlNextSid(var p: PUtf8Char; var sid: RawSid; dom: PSid): boolean;

function KnownSidToSddl(wks: TWellKnownSid): RawUtf8; // for unit tests only



{ ****************** Discretionary Access Control List (DACL) Definitions }

type
  /// standard and generic access rights in TSecAce.Mask
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
    sam9, sam10, sam11, sam12, sam13, sam14, sam15,
    samDelete,               // SD
    samReadControl,          // RC
    samWriteDac,             // WD
    samWriteOwner,           // WO
    samSynchronize,
    sam21, sam22, sam23,
    samAccessSystemSecurity,
    samMaximumAllowed,
    sam26, sam27,
    samGenericAll,           // GA
    samGenericExecute,       // GX
    samGenericWrite,         // GW
    samGenericRead);         // GR
  /// 32-bit standard and generic access rights in TSecAce.Mask
  TSecAccessMask = set of TSecAccess;

  /// custom binary buffer type used as Windows self-relative Security Descriptor
  RawSecurityDescriptor = type RawByteString;

  TSecControl = (
    scOwnerDefaulted,
    scGroupDefaulted,
    scDaclPresent,
    scDaclDefaulted,
    scSaclPresent,
    scSaclDefaulted,
    sc6, sc7,
    scDaclAutoInheritReq,
    scSaclAutoInheritReq,
    scDaclAutoInherit,
    scSaclAutoInherit,
    scDaclProtected,
    scSaclProtected,
    scRmControlValid,
    scSelfRelative);
  TSecControls = set of TSecControl;

  /// high-level supported ACE kinds in TSecAce.AceType
  TSecAceType = (
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
    satAccessFilter,                  // FL 21 SYSTEM_ACCESS_FILTER_ACE_TYPE
    satUnknown);                      //    22 unknown = for internal use

  /// high-level supported ACE flags in TSecAce.Flags
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

  /// define one discretionary/system access control, as stored in TSecDesc.Dacl[]
  TSecAce = packed record
    /// high-level supported ACE kinds
    AceType: TSecAceType;
    // the ACE flags
    Flags: TSecAceFlags;
    /// the raw ACE identifier - typically = ord(AceType)
    // - if you force this type, ensure you set AceType=satUnknown before saving
    // - defined as word for proper alignment
    RawType: word;
    /// contains the associated 32-bit access mask
    Mask: TSecAccessMask;
    /// contains an associated SID
    Sid: RawSid;
    /// some optional opaque callback/resource data, stored after the Sid
    // - e.g. is a conditional expression string for satConditional ACEs like
    // '(@User.Project Any_of @Resource.Project)'
    Opaque: RawByteString;
    /// the associated Object Type GUID (satObject only)
    ObjectType: TGuid;
    /// the inherited Object Type GUID  (satObject only)
    InheritedObjectType: TGuid;
  end;
  /// define a discretionary access control list (DACL)
  TSecAces = array of TSecAce;

  /// high-level cross-platform support of Windows Security Descriptors
  // - can load and export Windows SD as self-relative binary or SDDL text
  {$ifdef USERECORDWITHMETHODS}
  TSecDesc = record
  {$else}
  TSecDesc = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the owner security identifier (SID)
    Owner: RawSid;
    /// the primary group SID
    Group: RawSid;
    /// discretionary access control list
    Dacl: TSecAces;
    /// system access control list
    Sacl: TSecAces;
    /// control flags of this Security Descriptor
    Flags: TSecControls;
    /// remove any previous content
    procedure Clear;
    /// compare the fields of this instance with another
    function IsEqual(const sd: TSecDesc): boolean;
    /// decode a self-relative binary Security Descriptor buffer
    function FromBinary(p: PByteArray; len: cardinal): boolean; overload;
    /// decode a self-relative binary Security Descriptor buffer
    function FromBinary(const Bin: RawSecurityDescriptor): boolean; overload;
    /// encode this Security Descriptor into a self-relative binary buffer
    function ToBinary: RawSecurityDescriptor;
    /// decode a Security Descriptor from its SDDL textual representation
    // - could also recognize SDDL RID placeholders, with the specified
    // RidDomain in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' text form
    function FromText(const SddlText: RawUtf8;
      const RidDomain: RawUtf8 = ''): boolean; overload;
    /// decode a Security Descriptor from its SDDL textual representation
    function FromText(var p: PUtf8Char; dom: PSid = nil;
      endchar: AnsiChar = #0): boolean; overload;
    /// encode this Security Descriptor into its SDDL textual representation
    // - could also generate SDDL RID placeholders, from the specified
    // RidDomain in its 'S-1-5-21-xxxxxx-xxxxxxx-xxxxxx' text form
    function ToText(const RidDomain: RawUtf8 = ''): RawUtf8;
    /// append this Security Descriptor as SDDL text into an existing buffer
    // - could also generate SDDL RID placeholders, if dom binary is supplied,
    // e.g. S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
    procedure AppendAsText(var result: RawUtf8; dom: PSid = nil);
  end;

const
  satCommon = [satAccessAllowed, satAccessDenied, satAudit,  satAlarm,
               satCallbackAccessAllowed, satCallbackAccessDenied,
               satCallbackAudit,  satCallbackAlarm,
               satMandatoryLabel, satResourceAttribute,
               satScoppedPolicy,  satAccessFilter];
  satObject = [satObjectAccessAllowed, satObjectAccessDenied,
               satObjectAudit, satObjectAlarm,
               satCallbackObjectAccessAllowed, satCallbackObjectAccessDenied,
               satCallbackObjectAudit, satCallbackObjectAlarm];
  satConditional = [satCallbackAccessAllowed, satCallbackAccessDenied,
               satCallbackObjectAccessAllowed, satCallbackObjectAccessDenied];
  safInheritanceFlags = [safObjectInherit, safContainerInherit,
                         safNoPropagateInherit, safInheritOnly];
  safAuditFlags = [safSuccessfulAccess, safFailedAccess];

/// check the conformity of a self-relative binary Security Descriptor buffer
// - only check the TSecDesc main fields consistency
function IsValidSecurityDescriptor(p: PByteArray; len: cardinal): boolean;

/// convert a self-relative Security Descriptor buffer as text (SDDL or hexa)
// - will wrap our TSecDesc binary decoder / SDDL encoder on all platforms
// - returns true if the conversion succeeded
// - function is able to convert itself, i.e. allows pointer(sd)=pointer(text)
// - could also generate SDDL RID placeholders, if dom binary is supplied,
// e.g. S-1-5-21-xx-xx-xx-512 (wkrGroupAdmins) into 'DA'
// - on Windows, you can call native CryptoApi.SecurityDescriptorToText()
function SecurityDescriptorToText(const sd: RawSecurityDescriptor;
  var text: RawUtf8; dom: PSid = nil): boolean;


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
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil): RawUtf8; overload;

/// return the SID of the current user, from process or thread, as raw binary
procedure CurrentRawSid(out sid: RawSid; wtt: TWinTokenType = wttProcess;
  name: PRawUtf8 = nil; domain: PRawUtf8 = nil); overload;

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


{$endif OSWINDOWS}

implementation

{$ifdef OSWINDOWS}
uses
  Windows; // for Windows API Specific Security Types and Functions below
{$endif OSWINDOWS}


{ some general wrapper functions, to avoid dependencies to mormot.core.text }

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

{$ifdef ISDELPHI} // missing convenient RTL function in Delphi
function TryStringToGUID(const s: string; var uuid: TGuid): boolean;
begin
  try
    uuid := StringToGUID(s);
    result := true;
  except
    result := false;
  end;
end;
{$endif ISDELPHI}

function TextToUuid(const text: shortstring; out uuid: TGuid): boolean;
begin // sub-function to avoid temp string on the stack
  result := TryStringToGUID('{' + string(text) + '}', uuid); // RTL fast enough
end;

procedure AppendShortUuid(const u: TGuid; var s: shortstring);
begin // sub-function to avoid temp string on the stack
  AppendShortAnsi7String(AnsiString(LowerCase(copy(GUIDToString(u), 2, 36))), s);
end;


{ ****************** Security IDentifier (SID) Definitions }

const
  SID_MINLEN = SizeOf(TSidAuth) + 2; // = 8
  SID_REV32 = ord('S') + ord('-') shl 8 + ord('1') shl 16 + ord('-') shl 24;

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

procedure SidToTextShort(sid: PSid; var result: shortstring);
var
  a: PSidAuth;
  i: PtrInt;
begin // faster than ConvertSidToStringSidA(), and cross-platform
  if (sid = nil ) or
     (sid^.Revision <> 1) then
  begin
    result[0] := #0; // invalid SID
    exit;
  end;
  a := @sid^.IdentifierAuthority;
  if (a^[0] <> 0) or
     (a^[1] <> 0) then
  begin
    result := 'S-1-0x'; // hexa 48-bit IdentifierAuthority, i.e. S-1-0x######-..
    for i := 0 to 5 do
      AppendShortByteHex(a^[i], result)
  end
  else
  begin
    result := 'S-1-';
    AppendShortCardinal(bswap32(PCardinal(@a^[2])^), result);
  end;
  for i := 0 to PtrInt(sid^.SubAuthorityCount) - 1 do
  begin
    AppendShortChar('-', @result);
    AppendShortCardinal(sid^.SubAuthority[i], result);
  end;
end;

function SidToText(sid: PSid): RawUtf8;
begin
  SidToText(sid, result);
end;

procedure SidToText(sid: PSid; var text: RawUtf8);
var
  tmp: shortstring;
begin
  SidToTextShort(sid, tmp);
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
  else if wks <= wksCreatorGroupServer then
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
            if c in [0 .. 3] then // S-1-3-x
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
            (tmp.SubAuthorityCount in [4, 5]) and // allow domain or rid
            (tmp.SubAuthority[0] = 21) and
            (PWord(@tmp.IdentifierAuthority)^ = 0) and
            (PCardinal(@tmp.IdentifierAuthority[2])^ = $05000000);
  if not result then
    exit; // this Domain text is no valid domain SID
  tmp.SubAuthorityCount := 5; // reserve place for WKR_RID[wkr] trailer
  tmp.SubAuthority[4] := 0;
  ToRawSid(@tmp, Dom);        // output Dom as S-1-5-21-xx-xx-xx-RID
end;

const
  WKR_RID: array[TWellKnownRid] of word = (
    $1f2, $1f4, $1f5, $1f6, $200, $201, $202, $203, $204, $205, $206, $207,
    $208, $209, $20a, $20d, $20e, $20f, $1000, $2000, $2100, $3000, $4000);

function KnownSidToText(wkr: TWellKnownRid; const Domain: RawUtf8): RawUtf8;
begin
  SidToText(pointer(KnownRawSid(wkr, Domain)), result);
end;

function KnownRawSid(wkr: TWellKnownRid; const Domain: RawUtf8): RawSid;
begin
  if TryDomainTextToSid(Domain, result) then
    PSid(result)^.SubAuthority[4] := WKR_RID[wkr];
end;

procedure KnownRidSid(wkr: TWellKnownRid; dom: PSid; var result: RawSid);
begin
  FastSetRawByteString(RawByteString(result), pointer(dom), SID_MINLEN + 5 * 4);
  PSid(result)^.SubAuthority[4] := WKR_RID[wkr];
end;

function SameDomain(a, b: PSid): boolean;
begin // both values are exepceted to be in a S-1-5-21-xx-xx-xx-RID layout
  result := (PInt64Array(a)[0] = PInt64Array(b)[0]) and // rev+count+auth
            (PInt64Array(a)[1] = PInt64Array(b)[1]) and // auth[0..1]
            (PInt64Array(a)[2] = PInt64Array(b)[2]);    // auth[2..3]
end;


{ ****************** Security Descriptor Definition Language (SDDL) Definitions }

const
  wksLastSddl = wksBuiltinWriteRestrictedCode;
  wksWithSddl = [wksWorld .. wksLastSddl];
  SID_SDDL: array[1 .. (ord(wksLastSddl) + ord(high(TWellKnownRid)) + 2) * 2] of AnsiChar =
    '  WD    COCG                                    NU  ' +
    'IUSUAN    PSAURC        SY          BABUBGPUAOSOPOBORERSRURDNO  MULU' +
    '      ISCY      ERCDRAES  HAAA        WR' +      // TWellKnownSid
    'ROLALG  DADUDGDCDDCASAEAPA  CNAPKAEKLWMEMPHISI'; // TWellKnownRid

function SddlNextSid(var p: PUtf8Char; var sid: RawSid; dom: PSid): boolean;
var
  i: PtrInt;
  tmp: TSid;
begin
  while p^ = ' ' do
    inc(p);
  if PCardinal(p)^ = SID_REV32 then
  begin
    result := TextToSid(p, tmp); // parse e.g. S-1-5-32-544
    if result then
      FastSetRawByteString(RawByteString(sid), @tmp, SidLength(@tmp));
    while p^ = ' ' do
      inc(p);
    exit;
  end;
  result := false;
  if not (p^ in ['A' .. 'Z']) then
    exit;
  i := WordScanIndex(@SID_SDDL, SizeOf(SID_SDDL) shr 1, PWord(p)^);
  if i <= 0 then
    exit
  else if i <= ord(wksLastSddl) then
    KnownRawSid(TWellKnownSid(i), sid)
  else if dom = nil then
    exit // no RID support
  else
    KnownRidSid(TWellKnownRid(i - (ord(wksLastSddl) + 1)), dom, sid);
  inc(p, 2);
  while p^ = ' ' do
    inc(p);
  result := true;
end;

function KnownSidToSddl(wks: TWellKnownSid): RawUtf8;
begin
  FastAssignNew(result); // no need to optimize: only used for regression tests
  if (wks in wksWithSddl) and
     (PWordArray(@SID_SDDL)^[ord(wks)] <> $2020) then
    FastSetString(result, @PWordArray(@SID_SDDL)^[ord(wks)], 2);
end;

procedure SddlAppendSid(var s: shortstring; sid, dom: PSid);
var
  k: TWellKnownSid;
  c: cardinal;
  i: PtrInt;
  tmp: shortstring;
begin
  if sid = nil then
    exit;
  k := SidToKnown(sid);
  c := $2020;
  if k in wksWithSddl then
    c := PWordArray(@SID_SDDL)^[ord(k)]
  else if (k = wksNull) and
          (dom <> nil) and
          SameDomain(sid, dom) and
          (sid^.SubAuthority[4] <= $4000{WKR_RID[high(WKR_RID)]}) then
  begin
    i := WordScanIndex(@WKR_RID, length(WKR_RID), sid^.SubAuthority[4]);
    if i >= 0 then // found a TWellKnownRid
      c := PWordArray(@SID_SDDL)^[i + (ord(wksLastSddl) + 1)];
  end;
  if c <> $2020 then
    AppendShortTwoChars(@c, @s)
  else
  begin
    SidToTextShort(sid, tmp);
    AppendShort(tmp, s);
  end;
end;

function SddlNextPart(var p: PUtf8Char; out u: shortstring): boolean;
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

function SddlNextTwo(var p: PUtf8Char; out u: shortstring): boolean;
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

function SddlNextGuid(var p: PUtf8Char; out uuid: TGuid): boolean;
var
  u: shortstring;
begin
  result := SddlNextPart(p, u) and
            ((u[0] = #0) or
             TextToUuid(u, uuid)); // use RTL
  repeat
    inc(p);
  until p^ <> ' ';
end;

function SddlNextInteger(var p: PUtf8Char; out c: integer): boolean;
var
  u: shortstring;
  err: integer;
begin
  result := false;
  if not SddlNextPart(p, u) then
    exit;
  val({$ifdef UNICODE}string{$endif}(u), c, err); // RTL handles 0x###
  result := err = 0;
end;

type
  TSecAccessRight = (
    sarFileAll, sarFileRead, sarFileWrite, sarFileExecute,
    sarKeyAll,  sarKeyRead,  sarKeyWrite,  sarKeyExecute);

const
  // SDDL standard identifiers - use string[3] for 32-bit efficient alignment
  SAT_SDDL: array[TSecAceType] of string[3] = (
    'A', 'D', 'AU', 'AL', '', 'OA', 'OD', 'OU', 'OL', 'XA', 'XD', 'ZA', '',
    'XU', '', '', '', 'ML', 'RA', 'SP', 'TL', 'FL', '');
  SAF_SDDL: array[TSecAceFlag] of string[3] = (
    'OI', 'CI', 'NP', 'IO', 'ID', '', 'SA', 'FA');
  samWithSddl = [samCreateChild .. samControlAccess,
                 samDelete .. samWriteOwner, samGenericAll .. samGenericRead];
  SAM_SDDL: array[TSecAccess] of string[3] = (
    'CC', 'DC', 'LC', 'SW', 'RP', 'WP', 'DT', 'LO', 'CR', '', '', '', '', '', '', '',
    'SD', 'RC', 'WD', 'WO', '', '', '', '', '', '', '', '', 'GA', 'GX', 'GW', 'GR');
  SAR_SDDL: array[TSecAccessRight] of string[3] = (
    'FA', 'FR', 'FW', 'FX', 'KA', 'KR', 'KW', 'KX');

  // some cross-platform definitions of main 32-bit Windows access rights
  FILE_ALL_ACCESS      = $001f01ff;
  FILE_GENERIC_READ    = $00120089;
  FILE_GENERIC_WRITE   = $00120116;
  FILE_GENERIC_EXECUTE = $001200a0;
  KEY_ALL_ACCESS       = $000f003f;
  KEY_READ             = $00020019;
  KEY_WRITE            = $00020006;
  KEY_EXECUTE          = $00020019; // note that KEY_EXECUTE = KEY_READ
  SAR_MASK: array[TSecAccessRight] of cardinal = (
    FILE_ALL_ACCESS, FILE_GENERIC_READ, FILE_GENERIC_WRITE, FILE_GENERIC_EXECUTE,
    KEY_ALL_ACCESS, KEY_READ, KEY_WRITE, KEY_EXECUTE);

function SddlNextAce(var p: PUtf8Char; var ace: TSecAce; dom: PSid): boolean;
var
  u: shortstring;
  t: TSecAceType;
  f: TSecAceFlag;
  a: TSecAccess;
  r: TSecAccessRight;
  s: PUtf8Char;
  mask, parent: integer;
begin
  result := false;
  ace.AceType := satUnknown;
  while p^ = ' ' do
    inc(p);
  if PWord(p)^ = ord('0') + ord('x') shl 8 then // our own fallback format
    if SddlNextInteger(p, mask) then
      ace.RawType := mask
    else
      exit
  else
  begin
    if not SddlNextPart(p, u) or
       not (u[0] in [#1, #2]) then
      exit;
    for t := low(t) to high(t) do
      if SAT_SDDL[t] = u then
      begin
        ace.AceType := t;
        break;
      end;
    if ace.AceType = satUnknown then
      exit;
    ace.RawType := ord(ace.AceType);
  end;
  if p^ <> ';' then
    exit;
  repeat
    inc(p);
  until p^ <> ' ';
  while p^ <> ';' do
  begin
    if not SddlNextTwo(p, u) then
      exit;
    for f := low(f) to high(f) do
      if SAF_SDDL[f] = u then
      begin
        include(ace.Flags, f);
        break;
      end;
  end;
  repeat
    inc(p);
  until p^ <> ' ';
  while p^ <> ';' do
  begin
    if PWord(p)^ = ord('0') + ord('x') shl 8 then
      if SddlNextInteger(p, PInteger(@ace.Mask)^) then
        break // we got the mask as a 32-bit hexadecimal value
      else
        exit;
    if not SddlNextTwo(p, u) then
      exit;
    mask := 0;
    for r := low(r) to high(r) do
      if SAR_SDDL[r] = u then
      begin
        mask := SAR_MASK[r];
        break;
      end;
    if mask = 0 then
      for a := low(a) to high(a) do
        if SAM_SDDL[a] = u then
        begin
          mask := 1 shl ord(a);
          break;
        end;
    if mask = 0 then
      exit; // unrecognized
    PInteger(@ace.Mask)^ := PInteger(@ace.Mask)^ or mask;
  end;
  repeat
    inc(p);
  until p^ <> ' ';
  if not SddlNextGuid(p, ace.ObjectType) or
     not SddlNextGuid(p, ace.InheritedObjectType) or // satObject or nothing
     not SddlNextSid(p, ace.Sid, dom) then // entries always end with a SID/RID
    exit;
  if p^ = ';' then // optional additional/opaque parameter
  begin
    repeat
      inc(p);
    until p^ <> ' ';
    parent := 0;
    s := p;
    if (ace.AceType in satConditional) and
       (s^ = '(') then // conditional ACE expression
      repeat
        case s^ of
          #0:
            exit; // premature end
          '(':
            inc(parent); // count nested parenthesis
          ')':
            if parent = 0 then
              break // ending ACE parenthesis
            else
              dec(parent);
        end;
        inc(s);
      until false
    else
      while not (s^ in [#0, ')']) do // retrieve everthing until ending ')'
        inc(s);
    FastSetString(RawUtf8(ace.Opaque), p, s - p);
    p := s;
  end;
  result := p^ = ')'; // ACE should end with a parenthesis
end;

procedure SddlAppendAce(var s: shortstring; const ace: TSecAce; dom: PSid);
var
  f: TSecAceFlag;
  a: TSecAccess;
  c: cardinal;
  i: PtrInt;
begin
  if SAT_SDDL[ace.AceType][0] <> #0 then
    AppendShort(SAT_SDDL[ace.AceType], s)
  else
  begin
    AppendShortTwoChars('0x', @s);
    AppendShortIntHex(ace.RawType, s); // fallback to lower hex - paranoid
  end;
  AppendShortChar(';', @s);
  for f := low(f) to high(f) do
    if f in ace.Flags then
      AppendShort(SAF_SDDL[f], s);
  AppendShortChar(';', @s);
  c := cardinal(ace.Mask);
  if c <> 0 then
  begin
    i := IntegerScanIndex(@SAR_MASK, length(SAR_MASK), c);
    if i >= 0 then
      AppendShortTwoChars(@SAR_SDDL[TSecAccessRight(i)][1], @s)
    else if ace.Mask - samWithSddl <> [] then
    begin
      AppendShortTwoChars('0x', @s); // we don't have the tokens it needs
      AppendShortIntHex(c, s);       // store as @x##### hexadecimal
    end
    else
      for a := low(a) to high(a) do
        if a in ace.Mask then
          AppendShortTwoChars(@SAM_SDDL[a][1], @s)
  end;
  if ace.AceType in satObject then
  begin
    AppendShortChar(';', @s);
    if not IsNullGuid(ace.ObjectType) then
      AppendShortUuid(ace.ObjectType, s);
    AppendShortChar(';', @s);
    if not IsNullGuid(ace.InheritedObjectType) then
      AppendShortUuid(ace.InheritedObjectType, s);
    AppendShortChar(';', @s);
  end
  else
    AppendShort(';;;', s);
  SddlAppendSid(s, pointer(ace.Sid), dom);
  if ace.Opaque = '' then
    exit;
  AppendShortChar(';', @s);
  if StrLen(pointer(ace.Opaque)) = length(ace.Opaque) then // true text
    AppendShortAnsi7String(ace.Opaque, s) // e.g. conditional ACE expression
  else
    AppendShortHex(pointer(ace.Opaque), length(ace.Opaque), s); // paranoid
end;


{ ****************** Discretionary Access Control List (DACL) Definitions }

{ low-level self-relative Security Descriptor buffer data structures }

type
  // map the _SECURITY_DESCRIPTOR struct in self-relative state
  TSD = packed record
    Revision: byte;
    Sbz1: byte;
    Control: TSecControls;
    Owner: cardinal; // not pointers, but self-relative position
    Group: cardinal;
    Sacl: cardinal;
    Dacl: cardinal;
  end;
  PSD = ^TSD;
  // map the Access Control List header
  TACL = packed record
    AclRevision: byte;
    Sbz1: byte;
    AclSize: word; // including TACL header + all ACEs
    AceCount: word;
    Sbz2: word;
  end;
  PACL = ^TACL;
  // map one Access Control Entry
  TACE = packed record
    // ACE header
    AceType: byte;
    AceFlags: TSecAceFlags;
    AceSize: word; // include ACE header + whole body
    // ACE body
    Mask: TSecAccessMask;
    case integer of
      0: (CommonSid: cardinal);
      1: (ObjectFlags: cardinal;
          ObjectStart: cardinal);
  end;
  PACE = ^TACE;

const
  ACE_OBJECT_TYPE_PRESENT = 1;
  ACE_INHERITED_OBJECT_TYPE_PRESENT = 2;

function IsValidSecurityDescriptor(p: PByteArray; len: cardinal): boolean;
begin
  result := (p <> nil) and
            (len > SizeOf(TSD)) and
            (PSD(p)^.Revision = 1) and
            (PSD(p)^.Sbz1 = 0) and
            (scSelfRelative in PSD(p)^.Control) and
            (PSD(p)^.Owner < len) and
            (cardinal(SidLength(@p[PSD(p)^.Owner])) + PSD(p)^.Owner <= len) and
            (PSD(p)^.Group < len) and
            (cardinal(SidLength(@p[PSD(p)^.Group])) + PSD(p)^.Group  <= len) and
            (PSD(p)^.Sacl < len) and
            (PSD(p)^.Dacl < len) and
            ((PSD(p)^.Dacl <> 0) = (scDaclPresent in PSD(p)^.Control)) and
            ((PSD(p)^.Dacl = 0) or
             (PACL(@p[PSD(p)^.Dacl])^.AclSize + PSD(p)^.Dacl <= len)) and
            ((PSD(p)^.Sacl <> 0) = (scSaclPresent in PSD(p)^.Control)) and
            ((PSD(p)^.Sacl = 0) or
             (PACL(@p[PSD(p)^.Sacl])^.AclSize + PSD(p)^.Sacl <= len));
end;

function SecurityDescriptorToText(const sd: RawSecurityDescriptor;
  var text: RawUtf8; dom: PSid): boolean;
var
  tmp: TSecDesc;
begin
  result := tmp.FromBinary(sd);
  if not result then
    exit;
  FastAssignNew(text);
  tmp.AppendAsText(text, dom);
end;


{ TSecAces }

function BinToAcl(p: PByteArray; offset: cardinal; var res: TSecAces): boolean;
var
  hdr: PACL;
  ace: PACE;
  i, opaquelen, sidlen: integer;
  max: PtrUInt;
  sid: pointer;
  a: ^TSecAce;
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
    max := PtrUInt(@p[offset + hdr^.AclSize]);
    SetLength(res, hdr^.AceCount);
    a := pointer(res);
    ace := @p[offset + SizeOf(hdr^)];
    for i := 1 to hdr^.AceCount do
    begin
      // unserialize each entry
      if PtrUInt(ace) + ace^.AceSize > max then
        exit; // avoid buffer overflow
      a^.RawType := ace^.AceType;
      a^.AceType := TSecAceType(ace^.AceType);
      a^.Flags := ace^.AceFlags;
      a^.Mask := ace^.Mask;
      sid := nil;
      if a^.AceType in satCommon then
        sid := @ace^.CommonSid
      else if a^.AceType in satObject then
      begin
        sid := @ace^.ObjectStart;
        if ace^.ObjectFlags and ACE_OBJECT_TYPE_PRESENT <> 0 then
        begin
          a^.ObjectType := PGuid(sid)^;
          inc(PGuid(sid));
        end;
        if ace^.ObjectFlags and ACE_INHERITED_OBJECT_TYPE_PRESENT <> 0 then
        begin
          a^.InheritedObjectType := PGuid(sid)^;
          inc(PGuid(sid));
        end;
      end
      else if a^.AceType >= satUnknown then
        a^.AceType := satUnknown; // unsupported
      if sid <> nil then
      begin
        sidlen := SidLength(sid) + (PAnsiChar(sid) - pointer(ace));
        opaquelen := integer(ace^.AceSize) - sidlen;
        if opaquelen < 0 then
          exit;
        ToRawSid(sid, a^.Sid);
        if opaquelen <> 0 then
        begin
          inc(PByte(sid), sidlen - 8);
          FastSetRawByteString(a^.Opaque, sid, opaquelen);
        end;
      end;
      inc(PByte(ace), ace^.AceSize);
      inc(a);
    end;
  end;
  result := true;
end;

function AclToBin(p: PAnsiChar; const ace: TSecAces): PtrInt;
var
  hdr: PACL;
  e: PACE;
  a: ^TSecAce;
  f: cardinal;
  pf: PCardinal;
  i: PtrInt;
begin
  hdr := pointer(p); // hdr=nil below if we just want the length
  if ace <> nil then
  begin
    inc(PACL(p));
    a := pointer(ace);
    for i := 0 to length(ace) - 1 do
    begin
      e := pointer(p);
      inc(p, 8); // ACE header + Mask
      if a^.AceType in satObject then
      begin
        pf := pointer(p);
        inc(PCardinal(p));
        f := 0;
        if not IsNullGuid(a^.ObjectType) then
        begin
          f := ACE_OBJECT_TYPE_PRESENT;
          if hdr <> nil then
            PGuid(p)^ := a^.ObjectType;
          inc(PGuid(p));
        end;
        if not IsNullGuid(a^.InheritedObjectType) then
        begin
          f := f or ACE_INHERITED_OBJECT_TYPE_PRESENT;
          if hdr <> nil then
            PGuid(p)^ := a^.InheritedObjectType;
          inc(PGuid(p));
        end;
        if hdr <> nil then
          pf^ := f;
      end;
      if hdr <> nil then
        MoveFast(pointer(a^.Sid)^, p^, length(a^.Sid));
      inc(p, length(a^.Sid));
      if hdr <> nil then
        MoveFast(pointer(a^.Opaque)^, p^, length(a^.Opaque));
      inc(p, length(a^.Opaque));
      if hdr <> nil then
      begin
        if a^.AceType < satUnknown then
          e^.AceType := ord(a^.AceType)
        else
          e^.AceType := a^.RawType;
        e^.AceFlags := a^.Flags;
        e^.AceSize := p - pointer(e);
        e^.Mask := a^.Mask;
      end;
      inc(a);
    end;
    if hdr <> nil then
    begin
      hdr^.AclRevision := 2;
      hdr^.Sbz1 := 0;
      hdr^.AceCount := length(ace);
      hdr^.Sbz2 := 0;
      hdr^.AclSize := p - pointer(hdr);
    end;
  end;
  result := p - pointer(hdr);
end;


{ TSecAce }

function AceCompare(const a, b: TSecAce): boolean;
begin
  result := (a.AceType = b.AceType) and
            (a.RawType = b.RawType) and
            (a.Flags = b.Flags) and
            (a.Mask = b.Mask) and
            (a.Sid = b.Sid) and
            (a.Opaque = b.Opaque) and
            IsEqualGuid(a.ObjectType, b.ObjectType) and
            IsEqualGuid(a.InheritedObjectType, b.InheritedObjectType);
end;


{ TSecDesc }

procedure TSecDesc.Clear;
begin
  Finalize(self);
  Flags := [scSelfRelative];
end;

function TSecDesc.IsEqual(const sd: TSecDesc): boolean;
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
    if not AceCompare(Dacl[i], sd.Dacl[i]) then
      exit;
  for i := 0 to high(Sacl) do
    if not AceCompare(Sacl[i], sd.Sacl[i]) then
      exit;
  result := true;
end;

function TSecDesc.FromBinary(const Bin: RawSecurityDescriptor): boolean;
begin
  result := FromBinary(pointer(Bin), length(Bin));
end;

function TSecDesc.FromBinary(p: PByteArray; len: cardinal): boolean;
begin
  Clear;
  result := false;
  if not IsValidSecurityDescriptor(p, len) then
    exit;
  if PSD(p)^.Owner <> 0 then
    ToRawSid(@p[PSD(p)^.Owner], Owner);
  if PSD(p)^.Group <> 0 then
    ToRawSid(@p[PSD(p)^.Group], Group);
  Flags := PSD(p)^.Control;
  result := BinToAcl(p, PSD(p)^.Dacl, Dacl) and
            BinToAcl(p, PSD(p)^.Sacl, Sacl);
end;

function TSecDesc.ToBinary: RawSecurityDescriptor;
var
  p: PAnsiChar;
  hdr: PSD;
begin
  FastSetRawByteString(RawByteString(result), nil,
    SizeOf(hdr^) + length(Owner) + length(Group) +
    AclToBin(nil, Sacl) + AclToBin(nil, Dacl));
  p := pointer(result);
  hdr := pointer(p);
  FillCharFast(hdr^, SizeOf(hdr^), 0);
  hdr^.Revision := 1;
  hdr^.Control := Flags + [scSelfRelative];
  inc(PSD(p));
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
    inc(p, AclToBin(p, Sacl));
  end;
  if Dacl <> nil then
  begin
    include(hdr^.Control, scDaclPresent);
    hdr^.Dacl := p - pointer(result);
    inc(p, AclToBin(p, Dacl));
  end;
  if p - pointer(result) <> length(result) then
    raise EOSException.Create('TSecDesc.ToBinary'); // paranoid
end;

function TSecDesc.FromText(var p: PUtf8Char; dom: PSid; endchar: AnsiChar): boolean;

  function NextAces(var aces: TSecAces; pr, ar, ai: TSecControl): boolean;
  begin
    result := false;
    while p^ = ' ' do
      inc(p);
    if p^ = 'P' then
    begin
      include(Flags, pr);
      inc(p);
    end;
    while p^ = ' ' do
      inc(p);
    if PWord(p)^ = ord('A') + ord('R') shl 8 then
    begin
      include(Flags, ar);
      inc(p, 2);
    end;
    while p^ = ' ' do
      inc(p);
    if PWord(p)^ = ord('A') + ord('I') shl 8 then
    begin
      include(Flags, ai);
      inc(p, 2);
    end;
    while p^ = ' ' do
      inc(p);
    if p^ <> '(' then
      exit;
    repeat
      inc(p);
      SetLength(aces, length(aces) + 1);
      if not SddlNextAce(p, aces[high(aces)], dom) then
        exit;
      inc(p); // p^ = ')'
    until p^ <> '(';
    result := true;
  end;

begin
  Clear;
  result := false;
  if p = nil then
    exit;
  repeat
    while p^ = ' ' do
      inc(p);
    if p[1] <> ':' then
      exit;
    inc(p, 2);
    case p[-2] of
      'O':
        if not SddlNextSid(p, Owner, dom) then
          exit;
      'G':
        if not SddlNextSid(p, Group, dom) then
          exit;
      'D':
        if not NextAces(Dacl,
                scDaclProtected, scDaclAutoInheritReq, scDaclAutoInherit) then
          exit;
      'S':
        if not NextAces(Sacl,
                 scSaclProtected, scSaclAutoInheritReq, scSaclAutoInherit) then
          exit;
    else
      exit;
    end;
    while p^ = ' ' do
      inc(p);
  until (p^ = #0) or (p^ = endchar);
  if Dacl <> nil then
    include(Flags, scDaclPresent);
  if Sacl <> nil then
    include(Flags, scSaclPresent);
  result := true;
end;

function TSecDesc.FromText(const SddlText, RidDomain: RawUtf8): boolean;
var
  p: PUtf8Char;
  dom: RawSid;
begin
  p := pointer(SddlText);
  result := TryDomainTextToSid(RidDomain, dom) and
            FromText(p, pointer(dom));
end;

function TSecDesc.ToText(const RidDomain: RawUtf8): RawUtf8;
var
  dom: RawSid;
begin
  result := '';
  if TryDomainTextToSid(RidDomain, dom) then
    AppendAsText(result, pointer(dom));
end;

procedure TSecDesc.AppendAsText(var result: RawUtf8; dom: PSid);
var
  tmp: shortstring;

  procedure AppendTmp;
  var
    n: PtrInt;
  begin
    n := length(result);
    SetLength(result, n + ord(tmp[0]));
    MoveFast(tmp[1], PByteArray(result)[n], ord(tmp[0]));
  end;

  procedure AppendAces(const aces: TSecAces; p, ar, ai: TSecControl);
  var
    i: Ptrint;
  begin
    if aces = nil then
      exit;
    if p in Flags then
      AppendShortChar('P', @tmp);
    if ar in Flags then
      AppendShortTwoChars('AR', @tmp);
    if ai in Flags then
      AppendShortTwoChars('AI', @tmp);
    for i := 0 to length(aces) - 1 do
    begin
      AppendShortChar('(', @tmp);
      SddlAppendAce(tmp, aces[i], dom);
      AppendShortChar(')', @tmp);
      AppendTmp;
      tmp[0] := #0;
    end;
  end;

begin
  if Owner <> '' then
  begin
    tmp := 'O:';
    SddlAppendSid(tmp, pointer(Owner), dom);
  end;
  if Group <> '' then
  begin
    AppendShortTwoChars('G:', @tmp);
    SddlAppendSid(tmp, pointer(Group), dom);
    AppendTmp;
  end;
  tmp := 'D:';
  AppendAces(Dacl, scDaclProtected, scDaclAutoInheritReq, scDaclAutoInherit);
  tmp := 'S:';
  AppendAces(Sacl, scSaclProtected, scSaclAutoInheritReq, scSaclAutoInherit);
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

{$endif OSWINDOWS}


end.

