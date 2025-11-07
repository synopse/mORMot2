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
  mormot.core.os.security, // for FileIsKeyTab()
  mormot.core.unicode,     // e.g. for Split/SplitRight/IdemPChar
  mormot.core.buffers;     // for base-64 encoding


{ ****************** Low-Level libgssapi_krb5/libgssapi.so Library Access }

type
  gss_name_t     = pointer;
  gss_name_t_ptr = ^gss_name_t;
  gss_cred_id_t  = pointer;
  gss_ctx_id_t   = pointer;

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

  gss_channel_bindings_struct = record
    initiator_addrtype: cardinal;
    initiator_address: gss_buffer_desc;
    acceptor_addrtype: cardinal;
    acceptor_address: gss_buffer_desc;
    application_data: gss_buffer_desc;
  end;
  gss_channel_bindings_t = ^gss_channel_bindings_struct;

  {$A+} // back to usual class/record alignment


const
  GSS_C_NO_NAME = nil;
  GSS_C_NO_OID  = nil;
  GSS_C_NO_CHANNEL_BINDINGS = nil;

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
  /// direct access to the libgssapi functions
  TGssApi = class(TSynLibrary)
  public
    /// convert a contiguous string name to internal form
    // - returned output_name must be freed by the application after use
    // with a call to gss_release_name()
    gss_import_name: function (
      out minor_status: cardinal;
      input_name_buffer: gss_buffer_t;
      input_name_type: gss_OID;
      out output_name: gss_name_t): cardinal; cdecl;
    /// convert an internal form name into its text string
    // - output_name_buffer must be freed by the application after use
    // with a call to gss_release_buffer()
    gss_display_name: function (
      out minor_status: cardinal;
      input_name: gss_name_t;
      output_name_buffer: gss_buffer_t;
      output_name_type: gss_OID_ptr): cardinal; cdecl;
    /// free an an internal form name storage allocated by the API
    gss_release_name: function (
      out minor_status: cardinal;
      var name: gss_name_t): cardinal; cdecl;
    /// obtain a credential handle for pre-existing credentials
    gss_acquire_cred: function (
      out minor_status: cardinal;
      desired_name: gss_name_t;
      time_req: cardinal;
      desired_mechs: gss_OID_set;
      cred_usage: integer;
      out output_cred_handle: gss_cred_id_t;
      actual_mechs: gss_OID_set_ptr;
      time_rec: PCardinal): cardinal; cdecl;
    /// obtain a credential handle for a given username and password pair
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
    /// free a credential handle
    gss_release_cred: function (
      out minor_status: cardinal;
      var cred_handle: gss_cred_id_t): cardinal; cdecl;
    /// initiate a client security context with a peer application
    gss_init_sec_context: function (
      out minor_status: cardinal;
      initiator_cred_handle: gss_cred_id_t;
      var context_handle: gss_ctx_id_t;
      target_name: gss_name_t;
      mech_type: gss_OID;
      req_flags: cardinal;
      time_req: cardinal;
      input_chan_bindings: gss_channel_bindings_t;
      input_token: gss_buffer_t;
      actual_mech_type: gss_OID_ptr;
      output_token: gss_buffer_t;
      ret_flags: PCardinal;
      time_rec: PCardinal): cardinal; cdecl;
    /// accept a server security context initiated by a peer application
    gss_accept_sec_context: function (
      out minor_status: cardinal;
      var context_handle: pointer;
      acceptor_cred_handle: pointer;
      input_token_buffer: gss_buffer_t;
      input_chan_bindings: gss_channel_bindings_t;
      src_name: gss_name_t;
      mech_type: gss_OID_ptr;
      output_token: gss_buffer_t;
      ret_flags: PCardinal;
      time_rec: PCardinal;
      delegated_cred_handle: PPointer): cardinal; cdecl;
    /// obtain information about a security context
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
    /// free a security context
    gss_delete_sec_context: function (
      out minor_status: cardinal;
      var gss_context: gss_ctx_id_t;
      buffer: gss_buffer_t): cardinal; cdecl;
    /// return the SASL name types supported by the specified mechanism
    gss_inquire_saslname_for_mech: function (
      out minor_status: cardinal;
      desired_mech: gss_OID;
      sasl_mech_name: gss_buffer_t;
      mech_name: gss_buffer_t;
      mech_description: gss_buffer_t): cardinal; cdecl;
    /// free a libgssapi-allocated buffer
    gss_release_buffer: function (
      out minor_status: cardinal;
      var buffer: gss_buffer_desc): cardinal; cdecl;
    /// identify and encrypt a message
    gss_wrap: function (
      out minor_status: cardinal;
      context_handle: gss_ctx_id_t;
      conf_req_flag: integer;
      qop_req: cardinal;
      input_message_buffer: gss_buffer_t;
      conf_state: PInteger;
      output_message_buffer: gss_buffer_t): cardinal; cdecl;
    /// verify and decrypt a message
    gss_unwrap: function (
      out minor_status: cardinal;
      context_handle: gss_ctx_id_t;
      input_message_buffer: gss_buffer_t;
      output_message_buffer: gss_buffer_t;
      conf_state: PInteger;
      qop_state: PCardinal): cardinal; cdecl;
    /// return available underlying authentication mechanisms
    // - returned mech_set should be freed after use with gss_release_oid_set()
    gss_indicate_mechs: function (
      out minor_status: cardinal;
      out mech_set: gss_OID_set): cardinal; cdecl;
    /// free a set of object identifiers
    gss_release_oid_set: function (
      out minor_status: cardinal;
      out mech_set: gss_OID_set): cardinal; cdecl;
    /// convert a libgssapi integer status code to text
    gss_display_status: function (
      out minor_status: cardinal;
      status: cardinal;
      status_type: integer;
      mech_type: gss_OID;
      out message_context: cardinal;
      out status_string: gss_buffer_desc): cardinal; cdecl;
    /// set the default credentials cache name for use by Kerberos
    // - returned old_name must not be freed, but passed back upon a next call
    // to this function
    gss_krb5_ccache_name: function(
      out minor_status: cardinal;
      new_name: PUtf8Char;
      old_name: PPUtf8Char): cardinal; cdecl;
    /// thread-specific change of the Kerberos keytab file name to use
    // - gss_krb5_import_cred() could be preferred but it is more complex, and
    // the usual spnego-http-auth-nginx-module
    krb5_gss_register_acceptor_identity: function (
      path: PAnsiChar): cardinal; cdecl;
  end;

  /// Exception raised during libgssapi process
  EGssApi = class(ExceptionWithProps)
  private
    fMajorStatus: cardinal;
    fMinorStatus: cardinal;
  public
    /// initialize a libgssapi exception with the proper error message
    constructor Create(aMajor, aMinor: cardinal; const aPrefix: RawUtf8);
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
  /// direct access to the low-level libgssapi functions
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


// some macros for libgssapi functions process
function GSS_CALLING_ERROR(x: cardinal): boolean; inline;
function GSS_ROUTINE_ERROR(x: cardinal): boolean; inline;
function GSS_SUPPLEMENTARY_INFO(x: cardinal): boolean; inline;
function GSS_ERROR(x: cardinal): boolean; inline;

function gss_compare_oid(oid1, oid2: gss_OID): boolean;


{ ****************** Middle-Level GSSAPI Wrappers }

type
  /// GSSAPI high-level  Auth context
  TSecContext = record
    CredHandle: pointer;
    CtxHandle: pointer;
    ChannelBindingsHash: pointer;
    ChannelBindingsHashLen: cardinal;
  end;
  PSecContext = ^TSecContext;

/// set aSecHandle fields to empty state for a new handshake
procedure InvalidateSecContext(var aSecContext: TSecContext);

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
procedure GssCheck(aMajorStatus, aMinorStatus: cardinal;
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
// - aPassword is the user clear text password - you may set '' if you did a
// previous kinit for aUserName on the system and want to recover this token,
// or force a local keytab file e.g. as aPassword = 'FILE:/full/path/to/my.keytab'
// - aOutData contains data that must be sent back to the server
// - you can specify an optional Mechanism OID - default is SPNEGO / Kerberos
// - if the function returns True, client must send aOutData to server
// and re-call this function again with the data returned from server
// - see also ClientSspiAuthWithPasswordNoMemCcache global to disable the
// default transient memory ccache used during the authentication
function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8;
  const aPassword: SpiUtf8; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString; aMech: gss_OID = nil): boolean;

/// check if a binary request packet from a client is using NTLM
function ServerSspiDataNtlm(const aInData: RawByteString): boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from client
// - aOutData contains data that must be sent to client
// - will raise an EGssApi if authentication failed (e.g. invalid credentials)
// - server must send aOutData to the client (if any), and if True was returned,
// call this function again with any new data receive from the client
function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previous successful call to ServerSspiAuth
// - aUserName contains authenticated user name, as 'NETBIOSNAME\username' pattern,
// following ServerDomainMapRegister() mapping, or 'REALM.TLD\username' if
// global ServerDomainMapUseRealm was forced to true
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

type
  /// allow to track keytab files and their changes at runtime
  // - calling ServerForceKeytab() on each thread, only when needed
  TServerSspiKeyTab = class(TSynPersistent)
  protected
    fSafe: TLightLock;
    fKeyTab: TFileName;
    fKeyTabSize: Int64;
    fKeyTabTime: TUnixMSTime;
    fKeyTabSequence: integer; // stored in a threadvar
    fLastRefresh: cardinal;
    procedure _SetKeyTab(const aKeyTab: TFileName);
  public
    /// each thread should call this method before ServerSspiAuth()
    // - will do nothing if the thread is already prepared for the keytab
    procedure PrepareKeyTab;
    /// propagate a keytab file to all server threads
    // - returns true if the keytab was identified as changed
    function SetKeyTab(const aKeyTab: TFileName): boolean;
    /// can be called at Idle every few seconds to check if a keytab file changed
    // - it will allow hot reload of the keytab, only if needed
    // - returns true if the keytab was identified as changed
    function TryRefresh(Tix32: cardinal): boolean;
    /// parse HTTP input headers and perform Negotiate/Kerberos authentication
    // - will identify 'Authorization: Negotiate <base64 encoding>' HTTP header
    // - returns '' on error, or the 'WWW-Authenticate:' header on success
    // - can optionally return the authenticated user name
    // - will automatically call TryRefresh to check the file every 2 seconds
    // - is a cut-down version of THttpServerSocketGeneric.Authorization(),
    // assuming a simple two-way Negotiate/Kerberos handshake
    function ComputeServerHeader(const InputHeaders: RawUtf8;
      AuthUser: PRawUtf8 = nil): RawUtf8;
  published
    /// the keytab file name propagated to all server threads
    property KeyTab: TFileName
      read fKeyTab write _SetKeyTab;
    /// the current number of assigned KeyTab since the start of this instance
    property KeyTabSequence: integer
      read fKeyTabSequence;
  end;

/// force loading server credentials from specified keytab file
// - by default, clients may authenticate to any service principal
// in the default keytab (/etc/krb5.keytab or the value of the global
// KRB5_KTNAME environment variable)
// - this function is thread-specific and should be done on all threads, e.g.
// via TServerSspiKeyTab.PrepareKeyTab
function ServerForceKeytab(const aKeytab: TFileName): boolean;

const
  /// the API available on this system to implement Kerberos
  SECPKGNAMEAPI = 'GSSAPI';

  /// HTTP Challenge name
  // - GSS API only supports Negotiate/Kerberos - NTLM is unsafe and deprecated
  SECPKGNAMEHTTP = 'Negotiate';

  /// HTTP Challenge name, converted into uppercase for IdemPChar() pattern
  SECPKGNAMEHTTP_UPPER = 'NEGOTIATE';

  /// HTTP header to be set for authentication
  // - GSS API only supports Negotiate/Kerberos - NTLM is unsafe and deprecated
  SECPKGNAMEHTTPWWWAUTHENTICATE = 'WWW-Authenticate: Negotiate ';

  /// HTTP header pattern received for authentication
  SECPKGNAMEHTTPAUTHORIZATION = 'AUTHORIZATION: NEGOTIATE ';

  /// character used as marker in user name to indicates the associated domain
  SSPI_USER_CHAR = '@';

var
  /// ServerSspiAuthUser() won't return NT4-style NetBIOS name but the realm
  // - the GSS API only returns the realm (mydomain.tld) whereas Windows SSPI
  // returns the NetBIOS name (e.g. MYDOMAIN)
  // - default false will try to guess the NetBIOS name, or use
  // ServerDomainRegister()
  // - forcing this flag to true will let ServerSspiAuthUser() return the realm,
  // i.e. 'MYDOMAIN.TLD\username'
  ServerDomainMapUseRealm: boolean = false;

  /// ClientSspiAuthWithPassword() won't try gss_krb5_ccache_name('MEMORY:...')
  // - by default, a transient memory ccache will be used to not mess with the
  // current ccache environment when acquiring a token from the server: on Mac,
  // we have seen the transient token been added to the main klist :(
  // - you may set this global to true to disable this feature (as with the
  // initial behavior of this unit) if it seems to trigger some problems
  // - on any GSS_ERROR on this memory ccache, this unit will force this global
  // flag to true to avoid any further issue
  ClientSspiAuthWithPasswordNoMemCcache: boolean = false;

/// help converting fully qualified domain names to NT4-style NetBIOS names
// - to use the same value for TAuthUser.LogonName on all platforms, user name
// should be changed from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
// - when converting a fully qualified domain name to NT4-style NetBIOS name,
// ServerDomainMapRegister() list is first checked. If domain name is not found,
// then it's truncated on first dot, e.g. 'user1@CORP.ABC.COM' into 'CORP\user1'
// - you can change domain name conversion by registering names at server startup,
// e.g. ServerDomainMapRegister('CORP.ABC.COM', 'ABCCORP') change conversion for
// previous example to 'ABCCORP\user1'
// - used only if automatic conversion (truncate on first dot) does it wrong,
// and if ServerDomainMapUseRealm flag has not been forced to true
// - this method is thread-safe
procedure ServerDomainMapRegister(const aOld, aNew: RawUtf8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegister(const aOld, aNew: RawUtf8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegisterAll;


/// high-level cross-platform initialization function
// - as called e.g. by mormot.rest.client/server.pas
// - in this unit, will just call LoadGssApi('')
// - you can set GssLib_Custom global variable to load a specific .so library
function InitializeDomainAuth: boolean;


implementation


{ ****************** Low-Level libgssapi_krb5/libgssapi.so Library Access }

function gss_compare_oid(oid1, oid2: gss_OID): boolean;
begin
  result := (oid1 <> nil) and
            (oid2 <> nil) and
            (oid1^.length = oid2^.length) and
            CompareMemSmall(oid1^.elements, oid2^.elements, oid1^.length);
end;

function GSS_CALLING_ERROR(x: cardinal): boolean;
begin
  result := (x and
             (GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET)) <> 0;
end;

function GSS_ROUTINE_ERROR(x: cardinal): boolean;
begin
  result := (x and
             (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET)) <> 0;
end;

function GSS_SUPPLEMENTARY_INFO(x: cardinal): boolean;
begin
  result := (x and
             (GSS_C_SUPPLEMENTARY_MASK shl GSS_C_SUPPLEMENTARY_OFFSET)) <> 0;
end;

function GSS_ERROR(x: cardinal): boolean;
begin
  result := GSS_CALLING_ERROR(x) or
            GSS_ROUTINE_ERROR(x);
end;

procedure GssCheck(AMajorStatus, AMinorStatus: cardinal; const APrefix: RawUtf8);
begin
  if GSS_ERROR(AMajorStatus) then
    raise EGssApi.Create(AMajorStatus, AMinorStatus, APrefix);
end;

const
  GSS_ENTRIES: array[0 .. 19] of PAnsiChar = (
    // GSSAPI entries
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
    // Kerberos specific entries - potentially with Heimdal alternative name
    'gss_krb5_ccache_name',
    'krb5_gss_register_acceptor_identity gsskrb5_register_acceptor_identity',
    nil);

var
  GssApiTried: TFileName;

procedure LoadGssApi(const LibraryName: TFileName);
var
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
  if api.TryLoadResolve(
      [LibraryName, GssLib_Custom, GssLib_MIT, GssLib_Heimdal, GssLib_OS],
      '', @GSS_ENTRIES, @@api.gss_import_name) then
  begin
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
  if (GssApi = nil) or
     not Assigned(GssApi.gss_display_status) then
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
  until GSS_ERROR(MajSt) or
        (MsgCtx = 0);
end;

constructor EGssApi.Create(aMajor, aMinor: cardinal; const aPrefix: RawUtf8);
var
  Msg: RawUtf8;
begin
  Msg := aPrefix;
  GetDisplayStatus(Msg, aMajor, GSS_C_GSS_CODE);
  if (aMinor <> 0) and
     (aMinor <> 100001) then
    GetDisplayStatus(Msg, aMinor, GSS_C_MECH_CODE);
  inherited Create(Utf8ToString(Msg));
  fMajorStatus := aMajor;
  fMinorStatus := aMinor;
end;


{ ****************** Middle-Level GSSAPI Wrappers }

procedure InvalidateSecContext(var aSecContext: TSecContext);
begin
  FillCharFast(aSecContext, SizeOf(aSecContext), 0);
end;

procedure FreeSecContext(var aSecContext: TSecContext);
var
  MinStatus: cardinal;
begin
  if aSecContext.CtxHandle <> nil then
    GssApi.gss_delete_sec_context(MinStatus, aSecContext.CtxHandle, nil);
  if aSecContext.CredHandle <> nil then
    GssApi.gss_release_cred(MinStatus, aSecContext.CredHandle);
  InvalidateSecContext(aSecContext);
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
  GssCheck(MajStatus, MinStatus, 'Failed to encrypt message');
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
  GssCheck(MajStatus, MinStatus, 'Failed to decrypt message');
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
  result := nil;
  RequireGssApi;
  if not Assigned(GssApi.gss_indicate_mechs) or
     not Assigned(GssApi.gss_inquire_saslname_for_mech) then
    exit;
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

type
  TGssBindIdent = array[0..20] of AnsiChar;
  TGssBind = packed record
    head: gss_channel_bindings_struct;
    ident: TGssBindIdent;
    hash: THash512;
  end;

const
  GSS_SERVERENDPOINT: TGssBindIdent = 'tls-server-end-point:';

function SetBind(const SC: TSecContext; var Bind: TGssBind): gss_channel_bindings_t;
begin
  result := nil;
  if SC.ChannelBindingsHash = nil then
    exit;
  if SC.ChannelBindingsHashLen > SizeOf(Bind.hash) then
    raise EGssApi.CreateFmt('ClientSspi: ChannelBindingsHashLen=%d>%d',
      [SC.ChannelBindingsHashLen, SizeOf(Bind.hash)]);
  FillCharFast(Bind.head, SizeOf(Bind.head), 0);
  Bind.ident := GSS_SERVERENDPOINT;
  MoveFast(SC.ChannelBindingsHash^, Bind.hash, SC.ChannelBindingsHashLen);
  Bind.head.application_data.value := @Bind.ident;
  Bind.head.application_data.length := SC.ChannelBindingsHashLen + SizeOf(Bind.ident);
  result := @Bind;
end;

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
  Bind: TGssBind;
begin
  TargetName := nil;
  if aSecKerberosSpn <> '' then
  begin
    InBuf.length := Length(aSecKerberosSpn);
    InBuf.value := pointer(aSecKerberosSpn);
    MajStatus := GssApi.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, TargetName);
    GssCheck(MajStatus, MinStatus,
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
      aSecContext.CtxHandle, TargetName, aMech, CtxReqAttr, GSS_C_INDEFINITE,
        SetBind(aSecContext, Bind), @InBuf, nil, @OutBuf, @CtxAttr, nil);
    GssCheck(MajStatus, MinStatus,
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
  maj, min: cardinal;
  spn: RawUtf8;
  m: gss_OID_set;
  tmp: gss_OID_set_desc;
begin
  m := SetCredMech(aMech, tmp);
  if aSecContext.CredHandle = nil then
  begin
    // first call: create the needed context from a current system session
    maj := GssApi.gss_acquire_cred(min, nil, GSS_C_INDEFINITE,
      m, GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GssCheck(maj, min,
      'ClientSspiAuth: Failed to acquire credentials for current user');
  end;
  spn := aSecKerberosSpn;
  if spn = '' then
    spn := ForceSecKerberosSpn;
  // compute the first/next client-server roundtrip
  result := ClientSspiAuthWorker(aSecContext, aInData, spn, aOutData, aMech);
end;

procedure ClientSspiCreateCredHandle(var aSecContext: TSecContext;
  const aUserName, aPassword, aSecKerberosSpn: RawUtf8; aMech: gss_OID_set);
var
  maj, min, min2: cardinal;
  buf: gss_buffer_desc;
  user: gss_name_t;
  n , p, u: RawUtf8;
  orig: PAnsiChar;
  memCcache: TShort31;
begin
  // 1) retrieve the user information in the proper gss_name_t format
  user := nil;
  u := aUserName;
  Split(u, '@', n, p);
  if p = '' then
    p := SplitRight(aSecKerberosSpn, '@'); // try to extract the SPN
  if p <> '' then
    u := n + '@' + UpperCase(p); // force upcase to avoid enduser confusion
  buf.length := Length(u);
  buf.value := pointer(u);
  maj := GssApi.gss_import_name(
    min, @buf, GSS_KRB5_NT_PRINCIPAL_NAME, user);
  GssCheck(maj, min, 'Failed to import UserName');
  // 2) acquire this credential
  orig := nil;
  p := aPassword;
  if StartWithExact(p, 'FILE:') and // e.g. 'FILE:/full/path/to/my.keytab'
     FileExists(TFileName(copy(p, 6, 1023))) then // no file: keep as password
  begin
    if not Assigned(GssApi.gss_krb5_ccache_name) then
      raise EGssApi.CreateFmt(
        'ClientSspiAuthWithPassword(%s): missing gss_krb5_ccache_name', [p]);
    // use the explicit 'FILE:/tmp/krb5cc_custom' param for this authentication
    maj := GssApi.gss_krb5_ccache_name(min, pointer(p), @orig);
    if GSS_ERROR(maj) then
      orig := nil;
    p := ''; // we now should use the plain gss_acquire_cred()
  end;
  if p = '' then
    // recover an existing session with the supplied user
    maj := GssApi.gss_acquire_cred(min, user,
      GSS_C_INDEFINITE, aMech, GSS_C_INITIATE, aSecContext.CredHandle, nil, nil)
  else
  begin
    if not Assigned(GssApi.gss_acquire_cred_with_password) then
        raise EGssApi.CreateFmt(
          'ClientSspiAuthWithPassword(%s): missing in GSSAPI', [aUserName]);
    if Assigned(GssApi.gss_krb5_ccache_name) and
       not ClientSspiAuthWithPasswordNoMemCcache then
    begin
      // setup a thread-specific temporary memory ccache for this credential
      // as done by mag_auth_basic() in NGINX's mod_auth_gssapi.c
      memCcache := 'MEMORY:tmp_'; // threadid seems cleaner than random here
      AppendShortIntHex(PtrUInt(GetCurrentThreadId), memCcache);
      memCcache[ord(memCcache[0]) + 1] := #0; // make ASCIIZ
      // note: this memCcache is released at final FreeSecContext(aSecContext)
      maj := GssApi.gss_krb5_ccache_name(min, @memCcache[1], @orig);
      // note: old Heimdal implementation may not properly return orig
      //       see https://github.com/heimdal/heimdal/commit/fc9f9b322a88
      if GSS_ERROR(maj) then
      begin
        // transient memCcache failed -> cross fingers, and continue
        orig := nil; // nothing to release
        ClientSspiAuthWithPasswordNoMemCcache := true; // won't try again
        // Warning: in this case, MacOS system lib may create a session-wide
        //          token (like kinit) - consider using a stand-alone MIT
        //          libgssapi_krb5.dylib in GssLib_Custom
      end;
    end;
    // actually authenticate with the supplied credentials
    buf.length := Length(p);
    buf.value := pointer(p);
    maj := GssApi.gss_acquire_cred_with_password(
      min, user, @buf, GSS_C_INDEFINITE, aMech,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
  end;
  // release gss_name_t resource
  if user <> nil then
    GssApi.gss_release_name(min, user);
  // restore the previous memCcache for this thread (before GssCheck)
  if orig <> nil then // orig = e.g. 'FILE:/tmp/krb5cc_1000'
    GssApi.gss_krb5_ccache_name(min2, orig, nil);  // ignore any error
    // note: IBM doc states that krb5_free_string(orig) should be done
    //       but Debian doc and mod_auth_gssapi.c don't: so we won't either
  // 3) eventually raise EGssApi on authentication error
  GssCheck(maj, min,
    'Failed to acquire credentials for specified user');
end;

function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8; const aPassword: SpiUtf8;
  const aSecKerberosSpn: RawUtf8; out aOutData: RawByteString;
  aMech: gss_OID): boolean;
var
  m: gss_OID_set;
  tmp: gss_OID_set_desc;
  spn: RawUtf8;
begin
  m := SetCredMech(aMech, tmp);
  spn := aSecKerberosSpn;
  if spn = '' then
    spn := ForceSecKerberosSpn;
  if aSecContext.CredHandle = nil then
    // first call: create the needed context for those credentials
    ClientSspiCreateCredHandle(aSecContext, aUserName, aPassword, spn, m);
  // compute the first/next client-server roundtrip
  result := (aInData = 'onlypass') or // magic from TBasicAuthServerKerberos
            ClientSspiAuthWorker(aSecContext, aInData, spn, aOutData, aMech);
end;

function ServerSspiDataNtlm(const aInData: RawByteString): boolean;
begin
  result := (aInData <> '') and
            (PCardinal(aInData)^ or $20202020 =
               ord('n') + ord('t') shl 8 + ord('l') shl 16 + ord('m') shl 24);
end;

function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
  CtxAttr: cardinal;
  Bind: TGssBind;
begin
  RequireGssApi;
  if aSecContext.CredHandle = nil then // initial call
  begin
    if ServerSspiDataNtlm(aInData) then
      raise ENotSupportedException.Create(
        'NTLM authentication not supported by the GSSAPI library');
    MajStatus := GssApi.gss_acquire_cred(
      MinStatus, nil, GSS_C_INDEFINITE, nil, GSS_C_ACCEPT,
      aSecContext.CredHandle, nil, nil);
    GssCheck(MajStatus, MinStatus, 'Failed to acquire credentials for service');
  end;
  InBuf.length := Length(aInData);
  InBuf.value := PByte(aInData);
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GssApi.gss_accept_sec_context(MinStatus, aSecContext.CtxHandle,
    aSecContext.CredHandle, @InBuf, SetBind(aSecContext, Bind), nil, nil,
    @OutBuf, @CtxAttr, nil, nil);
  GssCheck(MajStatus, MinStatus, 'Failed to accept client credentials');
  result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0; // need more client input
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
    inc(DomainStart);
    if ServerDomainMap <> nil then
    begin
      DomainLen := StrLen(DomainStart);
      ServerDomainMapSafe.ReadLock;
      try
        for i := 0 to high(ServerDomainMap) do
          with ServerDomainMap[i] do
            if IdemPropNameU(Old, DomainStart, DomainLen) then
            begin
              Domain := New; // = '' after ServerDomainMapUnRegister()
              break;
            end;
      finally
        ServerDomainMapSafe.ReadUnLock;
      end;
    end;
    if {%H-}Domain = '' then
    begin
      if not ServerDomainMapUseRealm then // keep 'AD.MYDOMAIN.TLD' if true
      begin
        DomainEnd := PosChar(DomainStart, '.');
        if DomainEnd <> nil then
          repeat // e.g. 'user@AD.MYDOMAIN.TLD' -> 'MYDOMAIN'
            DomainNext := PosChar(DomainEnd + 1, '.');
            if DomainNext = nil then
              break; // we found the last '.TLD'
            DomainStart := DomainEnd + 1;
            DomainEnd := DomainNext;
          until false;
        if DomainEnd <> nil then
          DomainEnd^ := #0; // truncate to 'MYDOMAIN'
      end;
      Domain := DomainStart; // 'MYDOMAIN' or 'AD.MYDOMAIN.TLD'
    end;
    User := P; // 'username'
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
  GssCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (src_name)');
  try
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := GssApi.gss_display_name(
      MinStatus, SrcName, @OutBuf, @NameType);
    GssCheck(MajStatus, MinStatus,
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
  result := '';
  RequireGssApi;
  MajStatus := GssApi.gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    nil, nil, nil, @MechType, nil, nil, nil);
  GssCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (mech_type)');
  if not Assigned(GssApi.gss_inquire_saslname_for_mech) then
    exit; // returns '' if the needed API is missing
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GssApi.gss_inquire_saslname_for_mech(
    MinStatus, MechType, nil, @OutBuf, nil);
  GssCheck(MajStatus, MinStatus, 'Failed to obtain name for mech');
  FastSetString(result, OutBuf.value, OutBuf.length);
  GssApi.gss_release_buffer(MinStatus, OutBuf);
end;

procedure ClientForceSpn(const aSecKerberosSpn: RawUtf8);
begin
  ForceSecKerberosSpn := aSecKerberosSpn;
end;

function ServerForceKeytab(const aKeytab: TFileName): boolean;
begin
  result := Assigned(GssApi.krb5_gss_register_acceptor_identity) and
    not GSS_ERROR(GssApi.krb5_gss_register_acceptor_identity(pointer(aKeytab)));
end;         // = gsskrb5_register_acceptor_identity()


{ TServerSspiKeyTab }

threadvar // efficient API call once per thread, with proper hot reload
  ServerSspiKeyTabSequence: integer;

procedure TServerSspiKeyTab.PrepareKeyTab;
var
  seq: PInteger;
begin
  if (self = nil) or
     (fKeytabSequence = 0) then
    exit; // no SetKeyTab() call yet
  seq := @ServerSspiKeyTabSequence;
  if seq^ = fKeytabSequence then
    exit; // we can reuse existing keytab already set for this particular thread
  seq^ := fKeytabSequence;
  ServerForceKeytab(fKeyTab); // per-thread GSSAPI call
end;

function TServerSspiKeyTab.SetKeyTab(const aKeyTab: TFileName): boolean;
var
  fs: Int64;
  ft: TUnixMSTime;
begin
  result := false;
  if (self <> nil) and
     (aKeyTab <> '') and
     Assigned(GssApi.krb5_gss_register_acceptor_identity) and
     FileInfoByName(aKeyTab, fs, ft) and // aKeyTab exists
     (fs > 0) then                       // not a folder
    if (ft <> fKeyTabTime) or            // ensure don't reload if not changed
       (fs <> fKeyTabSize) or
       (fKeyTab <> aKeyTab) then
      if FileIsKeyTab(aKeyTab) then // ensure this new file is a valid keytab
      begin
        fSafe.Lock;
        fKeyTab := aKeyTab;
        fKeyTabSize := fs;
        fKeyTabTime := ft;
        inc(fKeytabSequence); // should be the last to notify PrepareKeyTab
        fSafe.UnLock;
        result := true;
      end;
end;

procedure TServerSspiKeyTab._SetKeyTab(const aKeyTab: TFileName);
begin
  SetKeyTab(aKeyTab); // encapsulate the function to be used as a setter method
end;

function TServerSspiKeyTab.TryRefresh(Tix32: cardinal): boolean;
begin
  result := false;
  Tix32 := Tix32 shr 1;
  if (self = nil) or
     (fKeyTab = '') or
     (Tix32 = fLastRefresh) then // try at most every two seconds
    exit;
  fLastRefresh := Tix32;
  result := SetKeyTab(fKeyTab);
end;

function TServerSspiKeyTab.ComputeServerHeader(const InputHeaders: RawUtf8;
  AuthUser: PRawUtf8): RawUtf8;
var
  auth, authend: PUtf8Char;
  bin, bout: RawByteString;
  ctx: TSecContext;
begin
  result := '';
  if AuthUser <> nil then
    AuthUser^ := '';
  auth := FindNameValue(pointer(InputHeaders), SECPKGNAMEHTTPAUTHORIZATION);
  if (auth = nil) or
     not InitializeDomainAuth then // late initialization of the GSS library
    exit;
  authend := PosChar(auth, #13); // parse 'Authorization: Negotiate <base64 encoding>'
  if (authend = nil) or
     not Base64ToBin(PAnsiChar(auth), authend - auth, bin) or
     ServerSspiDataNtlm(bin) then // two-way Kerberos only
    exit;
  if (self <> nil) and
     (fKeyTab <> '') then
  begin
    TryRefresh(GetTickSec); // check the local keytab file every two seconds
    PrepareKeyTab;          // thread specific setup for ServerSspiAuth()
  end;
  InvalidateSecContext(ctx);
  try
    // code below raise ESynSspi/EGssApi on authentication error
    if ServerSspiAuth(ctx, bin, bout) then
      // CONTINUE flag = need more input from the client: unsupported yet
      exit;
    // now client is authenticated in a single roundtrip: identify the user
    if AuthUser <> nil then
      ServerSspiAuthUser(ctx, AuthUser^);
    result := BinToBase64(bout, SECPKGNAMEHTTPWWWAUTHENTICATE, '', false);
  finally
    FreeSecContext(ctx);
  end;
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

