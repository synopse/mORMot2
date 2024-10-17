/// low-level access to the GssApi on Linux/POSIX
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.gssapi;


{
  *****************************************************************************

   Generic Security Service API on POSIX/Linux
   - Low-Level libgssapi_krb5/libgssapi.so Library Access
   - Middle-Level GSSAPI Wrappers
   - High-Level Client and Server Authentication using GSSAPI

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSWINDOWS}

// do-nothing-unit on non POSIX system

implementation

{$else}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;


{ ****************** Low-Level libgssapi_krb5/libgssapi.so Library Access }

type
  gss_name_t = pointer;
  gss_name_t_ptr = ^gss_name_t;
  gss_cred_id_t = pointer;
  gss_ctx_id_t = pointer;

  // we need to circumvent non-standard definitions of MacOS
  {$ifdef OSDARWIN}
  gss_length_t = cardinal; // no OM_STRING/xom.h on MacOS - OM_uint32 in gssapi.hin
  {$ifdef CPUINTEL}
  // #if defined(__APPLE__) && (defined(__ppc__) || defined(__ppc64__) || defined(__i386__) || defined(__x86_64__))
  {$A2} // #pragma pack(push,2)
  {$endif CPUINTEL}
  {$else}
  gss_length_t = PtrUInt;
  {$endif OSDARWIN}

  gss_OID_desc = record
    length: gss_length_t;
    elements: pointer;
  end;
  gss_OID = ^gss_OID_desc;
  gss_OID_ptr = ^gss_OID;

  gss_OID_array = array[word] of gss_OID_desc;
  gss_OID_descs = ^gss_OID_array;

  gss_OID_set_desc = record
    count: PtrUInt; // size_t in all platforms
    elements: gss_OID_descs;
  end;
  gss_OID_set = ^gss_OID_set_desc;
  gss_OID_set_ptr = ^gss_OID_set;

  gss_buffer_desc = record
    length: PtrUInt; // size_t in all platforms
    value: pointer;
  end;
  gss_buffer_t = ^gss_buffer_desc;

  {$A+} // back to usual class/record alignment


const
  GSS_C_NO_NAME = nil;
  GSS_C_NO_OID  = nil;

  GSS_C_GSS_CODE  = 1;
  GSS_C_MECH_CODE = 2;

  // Expiration time of 2^32-1 seconds means infinite lifetime
  GSS_C_INDEFINITE = $ffffffff;

  GSS_C_BOTH      = 0;
  GSS_C_INITIATE  = 1;
  GSS_C_ACCEPT    = 2;

  // Request that remote peer authenticate itself
  GSS_C_MUTUAL_FLAG   = 2;
  // Enable replay detection for messages protected with gss_wrap or gss_get_mic
  GSS_C_REPLAY_FLAG   = 4;
  // Enable detection of out-of-sequence protected messages
  GSS_C_SEQUENCE_FLAG = 8;
  // Request that confidentiality service be made available (via gss_wrap).
  GSS_C_CONF_FLAG     = 16;
  // Request that integrity service be made available (via gss_wrap or gss_get_mic)
  GSS_C_INTEG_FLAG    = 32;
  // Do not reveal the initiator's identity to the acceptor
  GSS_C_ANON_FLAG     = 64;

  GSS_C_CALLING_ERROR_OFFSET = 24;
  GSS_C_ROUTINE_ERROR_OFFSET = 16;
  GSS_C_SUPPLEMENTARY_OFFSET =  0;

  GSS_C_CALLING_ERROR_MASK = $ff;
  GSS_C_ROUTINE_ERROR_MASK = $ff;
  GSS_C_SUPPLEMENTARY_MASK = $ffff;

  GSS_S_CONTINUE_NEEDED = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 0);
  GSS_S_DUPLICATE_TOKEN = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 1);
  GSS_S_OLD_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 2);
  GSS_S_UNSEQ_TOKEN     = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 3);
  GSS_S_GAP_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 4);

  // https://github.com/krb5/krb5/blob/master/src/lib/gssapi/generic/gssapi_generic.c#L35

  // raw 1.2.840.113554.1.2.1.1 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2)
  //  generic(1) user-name(1)}
  gss_nt_user_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 1);
  gss_nt_user_name_desc: gss_OID_desc = (
    length: SizeOf(gss_nt_user_name);
    elements: @gss_nt_user_name);
  GSS_C_NT_USER_NAME: gss_OID = @gss_nt_user_name_desc;

  // raw 1.2.840.113554.1.2.1.2 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2)
  //  generic(1) machine_uid_name(2)}
  gss_nt_machine_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 2);
  gss_nt_machine_name_desc: gss_OID_desc = (
    length: SizeOf(gss_nt_machine_name);
    elements: @gss_nt_machine_name);
  GSS_C_NT_MACHINE_UID_NAME: gss_OID = @gss_nt_machine_name_desc;

  // raw 1.2.840.113554.1.2.1.3 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2)
  //  generic(1) string_uid_name(3)}
  gss_nt_stringuidname_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 3);
  gss_nt_stringuidname_name_desc: gss_OID_desc = (
    length: SizeOf(gss_nt_stringuidname_name);
    elements: @gss_nt_stringuidname_name);
  GSS_C_NT_STRING_UID_NAME: gss_OID = @gss_nt_stringuidname_name_desc;

  // raw 1.2.840.113554.1.2.1.2 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2)
  //  generic(1) service_name(4)}
  gss_nt_hostbased_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 4);
  gss_nt_hostbased_name_desc: gss_OID_desc = (
    length: SizeOf(gss_nt_hostbased_name);
    elements: @gss_nt_hostbased_name);
  GSS_C_NT_HOSTBASED_SERVICE: gss_OID = @gss_nt_hostbased_name_desc;

  // raw 1.2.840.113554.1.2.2.1 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2)
  //  krb5(2) krb5-name(1)}
  gss_nt_krb5_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 2, 1);
  gss_nt_krb5_name_desc: gss_OID_desc = (
    length: SizeOf(gss_nt_krb5_name);
    elements: @gss_nt_krb5_name);
  GSS_KRB5_NT_PRINCIPAL_NAME: gss_OID = @gss_nt_krb5_name_desc;

  // raw 1.2.840.113554.1.2.2 OID
  // {iso(1) member-body(2) us(840) mit(113554) infosys(1) gssapi(2) krb5(2)}
  gss_mech_krb5: array [0..8] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 2);
  gss_mech_krb5_desc: gss_OID_desc = (
    length: SizeOf(gss_mech_krb5);
    elements: @gss_mech_krb5);
  GSS_C_MECH_KRB5: gss_OID = @gss_mech_krb5_desc;

  // raw 1.3.6.1.5.5.2 OID for SPNEGO Simple and Protected Negotiation Mechanism
  // {iso(1) org(3) dod(6) internet(1) security(5) mechanisms(5) snego(2)}
  gss_mech_spnego: array [0..5] of byte = (
    43, 6, 1, 5, 5, 2);
  gss_mech_spnego_desc: gss_OID_desc = (
    length: SizeOf(gss_mech_spnego);
    elements: @gss_mech_spnego);
  GSS_C_MECH_SPNEGO: gss_OID = @gss_mech_spnego_desc;

  // raw 1.3.6.1.4.1.311.2.2.10 OID for GS2-NTLM (NTLM) Mechanism
  // {iso(1) identified-organization(3) dod(6) internet(1) private(4)
  //  enterprise(1) 311 2 2 10}
  gss_mech_ntlm: array [0..9] of byte = (
    $2b, $06, $01, $04, $01, $82, $37, $02, $02, $0a);
  gss_mech_ntlm_desc: gss_OID_desc = (
    length: SizeOf(gss_mech_ntlm);
    elements: @gss_mech_ntlm);
  GSS_C_MECH_NTLM: gss_OID = @gss_mech_ntlm_desc;


type
  TGssApi = class(TSynLibrary)
  public
    gss_import_name: function (
      out minor_status: cardinal;
      input_name_buffer: gss_buffer_t;
      input_name_type: gss_OID;
      out output_name: gss_name_t): cardinal; cdecl;
    gss_display_name: function (
      out minor_status: cardinal;
      input_name: gss_name_t;
      output_name_buffer: gss_buffer_t;
      output_name_type: gss_OID_ptr): cardinal; cdecl;
    gss_release_name: function (
      out minor_status: cardinal;
      var name: gss_name_t): cardinal; cdecl;
    gss_acquire_cred: function (
      out minor_status: cardinal;
      desired_name: gss_name_t;
      time_req: cardinal;
      desired_mechs: gss_OID_set;
      cred_usage: integer;
      out output_cred_handle: gss_cred_id_t;
      actual_mechs: gss_OID_set_ptr;
      time_rec: PCardinal): cardinal; cdecl;
    gss_acquire_cred_with_password: function (
      out minor_status: cardinal;
      desired_name: gss_name_t;
      password: gss_buffer_t;
      time_req: cardinal;
      desired_mechs: gss_OID_set;
      cred_usage: integer;
      out output_cred_handle: gss_cred_id_t;
      actual_mechs: gss_OID_set_ptr;
      time_rec: PCardinal): cardinal; cdecl;
    gss_release_cred: function (
      out minor_status: cardinal;
      var cred_handle: gss_cred_id_t): cardinal; cdecl;
    gss_init_sec_context: function (
      out minor_status: cardinal;
      initiator_cred_handle: gss_cred_id_t;
      var context_handle: gss_ctx_id_t;
      target_name: gss_name_t;
      mech_type: gss_OID;
      req_flags: cardinal;
      time_req: cardinal;
      input_chan_bindings: pointer;
      input_token: gss_buffer_t;
      actual_mech_type: gss_OID_ptr;
      output_token: gss_buffer_t;
      ret_flags: PCardinal;
      time_rec: PCardinal): cardinal; cdecl;
    gss_accept_sec_context: function (
      out minor_status: cardinal;
      var context_handle: pointer;
      acceptor_cred_handle: pointer;
      input_token_buffer: gss_buffer_t;
      input_chan_bindings: pointer;
      src_name: gss_name_t;
      mech_type: gss_OID_ptr;
      output_token: gss_buffer_t;
      ret_flags: PCardinal;
      time_rec: PCardinal;
      delegated_cred_handle: PPointer): cardinal; cdecl;
    gss_inquire_context: function (
      out minor_status: cardinal;
      context_handle: gss_ctx_id_t;
      src_name: gss_name_t_ptr;
      targ_name: gss_name_t_ptr;
      lifetime_rec: PCardinal;
      mech_type: gss_OID_ptr;
      ctx_flags: PCardinal;
      locally_initiated: PInteger;
      open: PInteger): cardinal; cdecl;
    gss_delete_sec_context: function (
      out minor_status: cardinal;
      var gss_context: gss_ctx_id_t;
      buffer: gss_buffer_t): cardinal; cdecl;
    gss_inquire_saslname_for_mech: function (
      out minor_status: cardinal;
      desired_mech: gss_OID;
      sasl_mech_name: gss_buffer_t;
      mech_name: gss_buffer_t;
      mech_description: gss_buffer_t): cardinal; cdecl;
    gss_release_buffer: function (
      out minor_status: cardinal;
      var buffer: gss_buffer_desc): cardinal; cdecl;
    gss_wrap: function (
      out minor_status: cardinal;
      context_handle: gss_ctx_id_t;
      conf_req_flag: integer;
      qop_req: cardinal;
      input_message_buffer: gss_buffer_t;
      conf_state: PInteger;
      output_message_buffer: gss_buffer_t): cardinal; cdecl;
    gss_unwrap: function (
      out minor_status: cardinal;
      context_handle: gss_ctx_id_t;
      input_message_buffer: gss_buffer_t;
      output_message_buffer: gss_buffer_t;
      conf_state: PInteger;
      qop_state: PCardinal): cardinal; cdecl;
    gss_indicate_mechs: function (
      out minor_status: cardinal;
      out mech_set: gss_OID_set): cardinal; cdecl;
    gss_release_oid_set: function (
      out minor_status: cardinal;
      out mech_set: gss_OID_set): cardinal; cdecl;
    gss_display_status: function (
      out minor_status: cardinal;
      status: cardinal;
      status_type: integer;
      mech_type: gss_OID;
      out message_context: cardinal;
      out status_string: gss_buffer_desc): cardinal; cdecl;
    krb5_gss_register_acceptor_identity: function (
      path: PAnsiChar): cardinal; cdecl;
  end;

  /// Exception raised during gssapi library process
  EGssApi = class(ExceptionWithProps)
  private
    fMajorStatus: cardinal;
    fMinorStatus: cardinal;
  public
    /// initialize an gssapi library exception with the proper error message
    constructor Create(aMajorStatus, aMinorStatus: cardinal;
      const aPrefix: RawUtf8);
  published
    /// associated GSS_C_GSS_CODE state value
    property MajorStatus: cardinal
      read fMajorStatus;
    /// associated GSS_C_MECH_CODE state value
    property MinorStatus: cardinal
      read fMinorStatus;
  end;


const
  {$ifdef OSDARWIN}
  GssMitDef     = 'libgssapi_krb5.dylib';
  GssHeimdalDef = 'libgssapi.dylib';
  GssOSDef      = '/System/Library/Frameworks/GSS.framework/GSS';
  {$else}
  GssMitDef     = 'libgssapi_krb5.so.2';
  GssHeimdalDef = 'libgssapi.so.3';
  GssOSDef      = '';
  {$endif OSDARWIN}


var
  /// access to the low-level libgssapi
  GssApi: TGssApi;

  /// custom library name for GSSAPI
  // - tried before OS/MIT/Heimdal standard alternatives
  // - you can overwrite with a custom value, make FreeAndNil(GssApi) and call
  // LoadGssApi again
  // - may be used on MacOS e.g. with '/full/path/to/libgssapi_krb5.dylib' for
  // proper user/password credential without any previous kinit or logged user
  GssLib_Custom: TFileName = '';

  /// library name of the MIT implementation of GSSAPI
  // - you can overwrite with a custom value, make FreeAndNil(GssApi) and call
  // LoadGssApi again
  GssLib_MIT: TFileName = GssMitDef;

  /// library name of the Heimdal implementation of GSSAPI
  // - you can overwrite with a custom value, make FreeAndNil(GssApi) and call
  // LoadGssApi again
  GssLib_Heimdal: TFileName = GssHeimdalDef;

  /// library name of the system implementation of GSSAPI
  // - only used on MacOS by default (GSS is available since 10.7 Lion in 2011)
  // - you can overwrite with a custom value, make FreeAndNil(GssApi) and call
  // LoadGssApi again
  GssLib_OS: TFileName = GssOSDef;


/// dynamically load GSSAPI library
// - do nothing if the library is already loaded
// - will try LibraryName, GssLib_Custom, GssLib_MIT, GssLib_Heimdal and
// GssLib_OS in this specific order, and maybe from the executable folder
procedure LoadGssApi(const LibraryName: TFileName = '');

/// check whether GSSAPI library is loaded or not
function GssApiLoaded: boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// check whether GSSAPI library was loaded and raise exception if not
procedure RequireGssApi;


// some macros for GSSAPI functions process
function GSS_CALLING_ERROR(x: cardinal): cardinal; inline;
function GSS_ROUTINE_ERROR(x: cardinal): cardinal; inline;
function GSS_SUPPLEMENTARY_INFO(x: cardinal): cardinal; inline;
function GSS_ERROR(x: cardinal): cardinal; inline;

function gss_compare_oid(oid1, oid2: gss_OID): boolean;


{ ****************** Middle-Level GSSAPI Wrappers }

type
  /// GSSAPI Auth context
  // - first field should be an Int64 ID - typically a THttpServerConnectionID
  TSecContext = record
    ID: Int64;
    CredHandle: pointer;
    CtxHandle: pointer;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of Auth contexts
  // - used to hold information between calls to ServerSspiAuth
  TSecContextDynArray = array of TSecContext;


/// Sets aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext;
  aConnectionID: Int64);

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

/// Encrypts a message using 'sign and seal' (i.e. integrity and encryption)
// - aSecContext must be set e.g. from previous success call to ServerSspiAuth
// or ClientSspiAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext;
  const aPlain: RawByteString): RawByteString;

/// Decrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSspiAuth
// or ClientSspiAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext;
  const aEncrypted: RawByteString): RawByteString;

/// Checks the return value of GSSAPI call and raises ESynGSSAPI exception
// when it indicates failure
procedure GccCheck(aMajorStatus, aMinorStatus: cardinal;
  const aPrefix: RawUtf8 = '');

/// Lists supported security mechanisms in form
// sasl:name:description
// - not all mechanisms provide human readable name and description
// - optionally return the corresponding raw OID values, encoded as gss_OID
function GssEnlistMechsSupported(oid: PBytesDynArray = nil): TRawUtf8DynArray;



{ ****************** High-Level Client and Server Authentication using GSSAPI }

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSpn is the Service Principal Name,
// registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
// - aOutData contains data that must be sent to server
// - you can specify an optional Mechanism OID - default is SPNEGO
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString; aMech: gss_OID = nil): boolean;

/// Client-side authentication procedure with clear text password
// - This function must be used when application need to use different
//  user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of 'username' or
// 'username@MYDOMAIN.TLD' if aSecKerberosSpn is not set or if
// ClientForceSpn() has not been called ahead
// - aPassword is the user clear text password - you may set '' if you have a
// previous kinit for aUserName on the system, and want to recover this token
// - aOutData contains data that must be sent to server
// - you can specify an optional Mechanism OID - default is SPNEGO
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
// - warning: on MacOS, the system GSSAPI library seems to create a session-wide
// token (as if a kinit was made), whereas it should only create a transient
// token in memory, so it is pretty unsafe to use; a workaround is to provide
// your own libgssapi_krb5.dylib in GssLib_Custom
function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8;
  const aPassword: SpiUtf8; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString; aMech: gss_OID = nil): boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again with data, returned from client
function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previous successful call to ServerSspiAuth
// - aUserName contains authenticated user name
procedure ServerSspiAuthUser(var aSecContext: TSecContext;
  out aUserName: RawUtf8);

/// Returns name of the security package that has been used with the
// negotiation process
// - aSecContext must be received from previous success call to ServerSspiAuth
// or ClientSspiAuth
function SecPackageName(var aSecContext: TSecContext): RawUtf8;

/// force using a Kerberos SPN for server identification
// - aSecKerberosSpn is the Service Principal Name, as registered in domain,
// e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
procedure ClientForceSpn(const aSecKerberosSpn: RawUtf8);

/// Force loading server credentials from specified keytab file
// - by default, clients may authenticate to any service principal
// in the default keytab (/etc/krb5.keytab or the value of the KRB5_KTNAME
// environment variable)
procedure ServerForceKeytab(const aKeytab: RawUtf8);

const
  /// the API available on this system to implement Kerberos
  SECPKGNAMEAPI = 'GSSAPI';

  /// HTTP Challenge name
  // - GSS API only supports Negotiate/Kerberos - NTLM is unsafe and deprecated
  SECPKGNAMEHTTP: RawUtf8 = 'Negotiate';

  /// HTTP Challenge name, converted into uppercase for IdemPChar() pattern
  SECPKGNAMEHTTP_UPPER: RawUtf8 = 'NEGOTIATE';

  /// HTTP header to be set for authentication
  // - GSS API only supports Negotiate/Kerberos - NTLM is unsafe and deprecated
  SECPKGNAMEHTTPWWWAUTHENTICATE: RawUtf8 = 'WWW-Authenticate: Negotiate';

  /// HTTP header pattern received for authentication
  SECPKGNAMEHTTPAUTHORIZATION: RawUtf8 = 'AUTHORIZATION: NEGOTIATE ';

  /// character used as marker in user name to indicates the associated domain
  SSPI_USER_CHAR = '@';


/// help converting fully qualified domain names to NT4-style NetBIOS names
// - to use the same value for TAuthUser.LogonName on all platforms, user name
// should be changed from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
// - when converting a fully qualified domain name to NT4-style NetBIOS name,
// ServerDomainMapRegister() list is first checked. If domain name is not found,
// then it's truncated on first dot, e.g. 'user1@CORP.ABC.COM' into 'CORP\user1'
// - you can change domain name conversion by registering names at server startup,
// e.g. ServerDomainMapRegister('CORP.ABC.COM', 'ABCCORP') change conversion for
// previous example to 'ABCCORP\user1'
// - used only if automatic conversion (truncate on first dot) does it wrong
// - this method is thread-safe
procedure ServerDomainMapRegister(const aOld, aNew: RawUtf8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegister(const aOld, aNew: RawUtf8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegisterAll;


/// high-level cross-platform initialization function
// - as called e.g. by mormot.rest.client/server.pas
// - in this unit, will just call LoadGssApi('')
function InitializeDomainAuth: boolean;


implementation


{ ****************** Low-Level libgssapi_krb5/libgssapi.so Library Access }

function gss_compare_oid(oid1, oid2: gss_OID): boolean;
begin
  if (oid1 <> nil) and
     (oid2 <> nil) then
  begin
    result := (oid1^.length = oid2^.length) and
              CompareMemSmall(oid1^.elements, oid2^.elements, oid1^.length);
  end
  else
    result := false;
end;

function GSS_CALLING_ERROR(x: cardinal): cardinal;
begin
  result := x and
            (GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET);
end;

function GSS_ROUTINE_ERROR(x: cardinal): cardinal;
begin
  result := x and
            (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET);
end;

function GSS_SUPPLEMENTARY_INFO(x: cardinal): cardinal;
begin
  result := x and
            (GSS_C_SUPPLEMENTARY_MASK shl GSS_C_SUPPLEMENTARY_OFFSET);
end;

function GSS_ERROR(x: cardinal): cardinal;
begin
  result := x and
            ((GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET) or
             (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET));
end;

procedure GccCheck(AMajorStatus, AMinorStatus: cardinal; const APrefix: RawUtf8);
begin
  if GSS_ERROR(AMajorStatus) <> 0 then
    raise EGssApi.Create(AMajorStatus, AMinorStatus, APrefix);
end;

const
  GSS_NAMES: array[0 .. 17] of RawUtf8 = (
    'gss_import_name',
    'gss_display_name',
    'gss_release_name',
    'gss_acquire_cred',
    'gss_acquire_cred_with_password',
    'gss_release_cred',
    'gss_init_sec_context',
    'gss_accept_sec_context',
    'gss_inquire_context',
    'gss_delete_sec_context',
    'gss_inquire_saslname_for_mech',
    'gss_release_buffer',
    'gss_wrap',
    'gss_unwrap',
    'gss_indicate_mechs',
    'gss_release_oid_set',
    'gss_display_status',
    'krb5_gss_register_acceptor_identity');

var
  GssApiTried: TFileName;

procedure LoadGssApi(const LibraryName: TFileName);
var
  i: PtrInt;
  P: PPointerArray;
  api: TGssApi; // local instance for thread-safe load attempt
  tried: TFileName;
begin
  if GssApi <> nil then
    // already loaded
    exit;
  tried := LibraryName + GssLib_Custom + GssLib_MIT + GssLib_Heimdal + GssLib_OS;
  if GssApiTried = tried then
    // try LoadLibrary() only if any of the .so names changed
    exit;
  GssApiTried := tried;
  api := TGssApi.Create;
  api.TryFromExecutableFolder := true; // good idea to check local first
  if api.TryLoadLibrary(
      [LibraryName, GssLib_Custom, GssLib_MIT, GssLib_Heimdal, GssLib_OS], nil) then
  begin
    P := @@api.gss_import_name;
    for i := 0 to high(GSS_NAMES) do
      api.Resolve('', GSS_NAMES[i], @P^[i]); // no exception, set nil on failure
    if not Assigned(api.krb5_gss_register_acceptor_identity) then
      // try alternate function name
      api.Resolve('', 'gsskrb5_register_acceptor_identity',
        @api.krb5_gss_register_acceptor_identity);
    if Assigned(api.gss_acquire_cred) and
       Assigned(api.gss_accept_sec_context) and
       Assigned(api.gss_release_buffer) and
       Assigned(api.gss_inquire_context) and
       Assigned(api.gss_display_name) and
       Assigned(api.gss_release_name) then
    begin
      // minimal API to work on server side -> thread safe setup into GSSAPI
      GlobalLock;
      try
        if GssApi = nil then
        begin
          GssApi := api;
          exit;
        end;
      finally
        GlobalUnlock;
      end;
    end;
  end;
  // always release on setup failure
  api.Free;
end;

function GssApiLoaded: boolean;
begin
  result := GssApi <> nil
end;

procedure RequireGssApi;
begin
  if GssApi = nil then
    raise ENotSupportedException.Create('No GSSAPI library found - please ' +
      'install either MIT or Heimdal GSSAPI implementation and do not ' +
      'forget to call InitializeDomainAuth once');
end;


{ EGssApi }

procedure GetDisplayStatus(var Msg: RawUtf8; aErrorStatus: cardinal;
  StatusType: integer);
var
  Str: RawUtf8;
  MsgCtx: cardinal;
  MsgBuf: gss_buffer_desc;
  MajSt, MinSt: cardinal;
begin
  if GssApi = nil then
    exit;
  MsgCtx := 0;
  repeat
    MajSt := GssApi.gss_display_status(
      MinSt, aErrorStatus, StatusType, nil, MsgCtx, MsgBuf);
    FastSetString(Str, MsgBuf.value, MsgBuf.length);
    GssApi.gss_release_buffer(MinSt, MsgBuf);
    if Msg <> '' then
      Msg := Msg + ' - ' + Str
    else
      Msg := Str;
  until (GSS_ERROR(MajSt) <> 0) or
        (MsgCtx = 0);
end;

constructor EGssApi.Create(aMajorStatus, aMinorStatus: cardinal;
  const aPrefix: RawUtf8);
var
  Msg: RawUtf8;
begin
  Msg := aPrefix;
  GetDisplayStatus(Msg, aMajorStatus, GSS_C_GSS_CODE);
  if (aMinorStatus <> 0) and
     (aMinorStatus <> 100001) then
    GetDisplayStatus(Msg, aMinorStatus, GSS_C_MECH_CODE);
//writeln('ERROR: ', Msg);
  inherited Create(Utf8ToString(Msg));
  fMajorStatus := AMajorStatus;
  fMinorStatus := AMinorStatus;
end;


{ ****************** Middle-Level GSSAPI Wrappers }

procedure InvalidateSecContext(var aSecContext: TSecContext;
  aConnectionID: Int64);
begin
  aSecContext.ID := aConnectionID;
  aSecContext.CredHandle := nil;
  aSecContext.CtxHandle := nil;
  aSecContext.CreatedTick64 := 0;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
var
  MinStatus: cardinal;
begin
  if aSecContext.CtxHandle <> nil then
    GssApi.gss_delete_sec_context(MinStatus, aSecContext.CtxHandle, nil);
  if aSecContext.CredHandle <> nil then
    GssApi.gss_release_cred(MinStatus, aSecContext.CredHandle);
end;

// see https://learn.microsoft.com/en-us/windows/win32/secauthn/sspi-kerberos-interoperability-with-gssapi

function SecEncrypt(var aSecContext: TSecContext;
  const aPlain: RawByteString): RawByteString;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
begin
  InBuf.length := Length(aPlain);
  InBuf.value := pointer(aPlain);
  MajStatus := GssApi.gss_wrap(
    MinStatus, aSecContext.CtxHandle, 1, 0, @InBuf, nil, @OutBuf);
  GccCheck(MajStatus, MinStatus, 'Failed to encrypt message');
  FastSetRawByteString(result, OutBuf.value, OutBuf.length);
  GssApi.gss_release_buffer(MinStatus, OutBuf);
end;

function SecDecrypt(var aSecContext: TSecContext;
  const aEncrypted: RawByteString): RawByteString;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
begin
  InBuf.length := Length(aEncrypted);
  InBuf.value := pointer(aEncrypted);
  MajStatus := GssApi.gss_unwrap(
    MinStatus, aSecContext.CtxHandle, @InBuf, @OutBuf, nil, nil);
  GccCheck(MajStatus, MinStatus, 'Failed to decrypt message');
  FastSetRawByteString(result, OutBuf.value, OutBuf.length);
  GssApi.gss_release_buffer(MinStatus, OutBuf);
end;

function GssEnlistMechsSupported(oid: PBytesDynArray): TRawUtf8DynArray;
var
  i: PtrInt;
  o: gss_OID;
  MinSt: cardinal;
  Mechs: gss_OID_set;
  Buf_sasl, Buf_name, Buf_desc: gss_buffer_desc;
  Sasl, Name, Desc: RawUtf8;
begin
  RequireGssApi;
  GssApi.gss_indicate_mechs(MinSt, Mechs);
  SetLength(result, Mechs^.count);
  if oid <> nil then
    SetLength(oid^, Mechs^.count);
  for i := 0 to Mechs^.count - 1 do
  begin
    if oid <> nil then
      with Mechs^.elements^[i] do
      begin // store as gss_OID() binary buffer
        SetLength(oid^[i], length + SizeOf(gss_OID_desc));
        o := pointer(oid^[i]);
        o^.length := length;
        o^.elements := PAnsiChar(o) + SizeOf(o^); // points just after oid_desc
        MoveFast(elements^, o^.elements^, length);
      end;
    GssApi.gss_inquire_saslname_for_mech(
      MinSt, @Mechs^.elements^[i], @Buf_sasl, @Buf_name, @Buf_desc);
    FastSetString(Sasl, Buf_sasl.value, Buf_sasl.length);
    FastSetString(Name, Buf_name.value, Buf_name.length);
    FastSetString(Desc, Buf_desc.value, Buf_desc.length);
    result[i] := Sasl + ':' + Name + ':' + Desc;
    GssApi.gss_release_buffer(MinSt, Buf_sasl);
    GssApi.gss_release_buffer(MinSt, Buf_name);
    GssApi.gss_release_buffer(MinSt, Buf_desc);
  end;
  GssApi.gss_release_oid_set(MinSt, Mechs);
end;


{ ****************** High-Level Client and Server Authentication using GSSAPI }

var
  ForceSecKerberosSpn: RawUtf8;

function ClientSspiAuthWorker(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString; aMech: gss_OID): boolean;
var
  TargetName: gss_name_t;
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
  CtxReqAttr: cardinal;
  CtxAttr: cardinal;
begin
  TargetName := nil;
  if aSecKerberosSpn <> '' then
  begin
    InBuf.length := Length(aSecKerberosSpn);
    InBuf.value := pointer(aSecKerberosSpn);
    MajStatus := GssApi.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, TargetName);
    GccCheck(MajStatus, MinStatus,
      'ClientSspiAuthWorker: Failed to import server SPN');
  end;
  try
    CtxReqAttr := GSS_C_INTEG_FLAG or GSS_C_CONF_FLAG;
    if TargetName <> nil then
      CtxReqAttr := CtxReqAttr or GSS_C_MUTUAL_FLAG;
    InBuf.length := Length(aInData);
    InBuf.value := pointer(aInData);
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := GssApi.gss_init_sec_context(MinStatus, aSecContext.CredHandle,
      aSecContext.CtxHandle, TargetName, aMech,
      CtxReqAttr, GSS_C_INDEFINITE, nil, @InBuf, nil, @OutBuf, @CtxAttr, nil);
    GccCheck(MajStatus, MinStatus,
      'ClientSspiAuthWorker: Failed to initialize security context');
    result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;
    FastSetRawByteString(aOutData, OutBuf.value, OutBuf.length);
    GssApi.gss_release_buffer(MinStatus, OutBuf);
  finally
    if TargetName <> nil then
      GssApi.gss_release_name(MinStatus, TargetName);
  end;
end;

function SetCredMech(var mech: gss_OID; var mechs: gss_OID_set_desc): gss_OID_set;
begin
  RequireGssApi;
  if mech = nil then
    mech := GSS_C_MECH_SPNEGO;
  {$ifdef OSDARWIN}
  if copy(GssApi.LibraryPath, 1, 8) = '/System/' then
  begin
    result := nil; // circumvent weird MacOS system library limitation
    exit;
  end;
  {$endif OSDARWIN}
  mechs.count := 1;
  mechs.elements := pointer(mech);
  result := @mechs;
end;

function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString; aMech: gss_OID): boolean;
var
  MajStatus, MinStatus: cardinal;
  SecKerberosSpn: RawUtf8;
  m: gss_OID_set;
  mechs: gss_OID_set_desc;
begin
  m := SetCredMech(aMech, mechs);
  if aSecContext.CredHandle = nil then
  begin
    // first call: create the needed context for the current user
    aSecContext.CreatedTick64 := GetTickCount64();
    MajStatus := GssApi.gss_acquire_cred(MinStatus, nil, GSS_C_INDEFINITE,
      m, GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GccCheck(MajStatus, MinStatus,
      'ClientSspiAuth: Failed to acquire credentials for current user');
  end;
  if aSecKerberosSpn <> '' then
    SecKerberosSpn := aSecKerberosSpn
  else
    SecKerberosSpn := ForceSecKerberosSpn;
  result := ClientSspiAuthWorker(
    aSecContext, aInData, SecKerberosSpn, aOutData, aMech);
end;

function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8; const aPassword: SpiUtf8;
  const aSecKerberosSpn: RawUtf8; out aOutData: RawByteString;
  aMech: gss_OID): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  UserName: gss_name_t;
  SecKerberosSpn, n , p, u: RawUtf8;
  m: gss_OID_set;
  mechs: gss_OID_set_desc;
begin
  m := SetCredMech(aMech, mechs);
  if aSecKerberosSpn <> '' then
    SecKerberosSpn := aSecKerberosSpn
  else
    SecKerberosSpn := ForceSecKerberosSpn;
  if aSecContext.CredHandle = nil then
  begin
    // first call: create the needed context for those credentials
    UserName := nil;
    aSecContext.CreatedTick64 := GetTickCount64;
    u := aUserName;
    Split(u, '@', n, p);
    if p = '' then
      p := SplitRight(SecKerberosSpn, '@'); // try to extract the SPN
    if p <> '' then
      u := n + '@' + UpperCase(p); // force upcase to avoid enduser confusion
    InBuf.length := Length(u);
    InBuf.value := pointer(u);
    MajStatus := GssApi.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, UserName);
    GccCheck(MajStatus, MinStatus, 'Failed to import UserName');
    if aPassword = '' then
      // recover an existing session with the supplied UserName
      MajStatus := GssApi.gss_acquire_cred(MinStatus, UserName,
        GSS_C_INDEFINITE, m, GSS_C_INITIATE, aSecContext.CredHandle, nil, nil)
    else
    begin
      // create a new transient in-memory token with the supplied credentials
      // WARNING: on MacOS, the default system GSSAPI stack seems to create a
      //  session-wide token (like kinit), not a transient token in memory - you
      //  may prefer to load a proper libgssapi_krb5.dylib instead
      InBuf.length := Length(aPassword);
      InBuf.value := pointer(aPassword);
      MajStatus := GssApi.gss_acquire_cred_with_password(
        MinStatus, UserName, @InBuf, GSS_C_INDEFINITE, m,
        GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    end;
    if UserName <> nil then
      GssApi.gss_release_name(MinStatus, UserName);
    GccCheck(MajStatus, MinStatus,
      'Failed to acquire credentials for specified user');
  end;
  result := ClientSspiAuthWorker(
    aSecContext, aInData, SecKerberosSpn, aOutData, aMech);
end;

function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
  CtxAttr: cardinal;
begin
  RequireGssApi;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if IdemPChar(pointer(aInData), 'NTLMSSP') then
      raise EGssApi.CreateFmt(
        'NTLM authentication not supported by GSSAPI library', []);
    MajStatus := GssApi.gss_acquire_cred(
      MinStatus, nil, GSS_C_INDEFINITE, nil, GSS_C_ACCEPT,
      aSecContext.CredHandle, nil, nil);
    GccCheck(MajStatus, MinStatus, 'Failed to aquire credentials for service');
  end;
  InBuf.length := Length(aInData);
  InBuf.value := PByte(aInData);
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GssApi.gss_accept_sec_context(MinStatus, aSecContext.CtxHandle,
    aSecContext.CredHandle, @InBuf, nil, nil, nil, @OutBuf, @CtxAttr, nil, nil);
  GccCheck(MajStatus, MinStatus, 'Failed to accept client credentials');
  result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;
  FastSetRawByteString(aOutData, OutBuf.value, OutBuf.length);
  GssApi.gss_release_buffer(MinStatus, OutBuf);
end;

var
  ServerDomainMapSafe: TRWLightLock;
  ServerDomainMap: array of record
    Old, New: RawUtf8;
  end;

function ServerDomainFind(const aOld: RawUtf8): PtrInt;
begin
  // caller made ServerDomainMapSafe.WriteLock
  for result := 0 to length(ServerDomainMap) - 1 do
    with ServerDomainMap[result] do
      if PropNameEquals(Old, aOld) then
        exit;
  result := -1;
end;

procedure ServerDomainMapRegister(const aOld, aNew: RawUtf8);
var
  i: PtrInt;
begin
  ServerDomainMapSafe.WriteLock;
  try
    i := ServerDomainFind(aOld);
    if i < 0 then
    begin
      i := length(ServerDomainMap);
      SetLength(ServerDomainMap, i + 1);
      ServerDomainMap[i].Old := aOld;
    end;
    ServerDomainMap[i].New := aNew;
  finally
    ServerDomainMapSafe.WriteUnLock;
  end;
end;

procedure ServerDomainMapUnRegister(const aOld, aNew: RawUtf8);
var
  i: PtrInt;
begin
  ServerDomainMapSafe.WriteLock;
  try
    i := ServerDomainFind(aOld);
    if i >= 0 then
      // for our purpose, it is enough to void the slot
      ServerDomainMap[i].Old := '';
  finally
    ServerDomainMapSafe.WriteUnLock;
  end;
end;

procedure ServerDomainMapUnRegisterAll;
begin
  ServerDomainMapSafe.WriteLock;
  try
    ServerDomainMap := nil;
  finally
    ServerDomainMapSafe.WriteUnLock;
  end;
end;

procedure ConvertUserName(P: PUtf8Char; Len: PtrUInt; out aUserName: RawUtf8);
var
  DomainStart, DomainEnd, DomainNext: PUtf8Char;
  DomainLen, i: PtrInt;
  Domain, User: RawUtf8;
begin
  // Ensure GSSAPI buffer is null-terminated
  if P[Len] <> #0 then
    exit;
  // Change user name from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
  DomainStart := PosChar(P, '@');
  if DomainStart <> nil then
  begin
    DomainStart^ := #0;
    Inc(DomainStart);
    if ServerDomainMap <> nil then
    begin
      DomainLen := StrLen(DomainStart);
      ServerDomainMapSafe.ReadLock;
      try
        for i := 0 to high(ServerDomainMap) do
          with ServerDomainMap[i] do
            if IdemPropNameU(Old, DomainStart, DomainLen) then
            begin
              Domain := New;
              break;
            end;
      finally
        ServerDomainMapSafe.ReadUnLock;
      end;
    end;
    if {%H-}Domain = '' then
    begin
      DomainEnd := PosChar(DomainStart, '.');
      if DomainEnd <> nil then
        repeat // e.g. 'user@AD.MYCOMP.TLD' -> 'MYCOMP'
          DomainNext := PosChar(DomainEnd + 1, '.');
          if DomainNext = nil then
            break; // we found the last '.TLD'
          DomainStart := DomainEnd + 1;
          DomainEnd := DomainNext;
        until false;
      if DomainEnd <> nil then
        DomainEnd^ := #0;
      Domain := DomainStart;
    end;
    User := P;
    aUserName := Domain + '\' + User;
  end
  else
    // Unknown user name format, leave as is
    FastSetString(aUserName, P, Len);
end;

procedure ServerSspiAuthUser(var aSecContext: TSecContext;
  out aUserName: RawUtf8);
var
  MajStatus, MinStatus: cardinal;
  SrcName: gss_name_t;
  OutBuf: gss_buffer_desc;
  NameType: gss_OID;
begin
  RequireGssApi;
  MajStatus := GssApi.gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    @SrcName, nil, nil, nil, nil, nil, nil);
  GccCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (src_name)');
  try
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := GssApi.gss_display_name(
      MinStatus, SrcName, @OutBuf, @NameType);
    GccCheck(MajStatus, MinStatus,
      'Failed to obtain name for authenticated user');
    if gss_compare_oid(NameType, GSS_KRB5_NT_PRINCIPAL_NAME) then
      ConvertUserName(PUtf8Char(OutBuf.value), OutBuf.length, aUserName);
    GssApi.gss_release_buffer(MinStatus, OutBuf);
  finally
    GssApi.gss_release_name(MinStatus, SrcName);
  end;
end;

function SecPackageName(var aSecContext: TSecContext): RawUtf8;
var
  MajStatus, MinStatus: cardinal;
  MechType: gss_OID;
  OutBuf: gss_buffer_desc;
begin
  RequireGssApi;
  MajStatus := GssApi.gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    nil, nil, nil, @MechType, nil, nil, nil);
  GccCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (mech_type)');
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GssApi.gss_inquire_saslname_for_mech(
    MinStatus, MechType, nil, @OutBuf, nil);
  GccCheck(MajStatus, MinStatus, 'Failed to obtain name for mech');
  FastSetString(result, OutBuf.value, OutBuf.length);
  GssApi.gss_release_buffer(MinStatus, OutBuf);
end;

procedure ClientForceSpn(const aSecKerberosSpn: RawUtf8);
begin
  ForceSecKerberosSpn := aSecKerberosSpn;
end;

procedure ServerForceKeytab(const aKeytab: RawUtf8);
begin
  if Assigned(GssApi.krb5_gss_register_acceptor_identity) then
    GssApi.krb5_gss_register_acceptor_identity(pointer(aKeytab));
end;

function InitializeDomainAuth: boolean;
begin
  if GssApi = nil then
    LoadGssApi('');
  result := GssApi <> nil;
end;

initialization

finalization
  FreeAndNil(GssApi);

{$endif OSWINDOWS}

end.

