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

  gss_OID_desc = record
    length: PtrUInt;
    elements: pointer;
  end;
  gss_OID = ^gss_OID_desc;
  gss_OID_ptr = ^gss_OID;
  gss_OID_array = array [0..0] of gss_OID_desc;
  gss_OID_descs = ^gss_OID_array;

  gss_OID_set_desc = record
    count: PtrUInt;
    elements: gss_OID_descs;
  end;
  gss_OID_set = ^gss_OID_set_desc;
  gss_OID_set_ptr = ^gss_OID_set;

  gss_buffer_desc = record
    length: PtrUInt;
    value: pointer;
  end;
  gss_buffer_t = ^gss_buffer_desc;


const
  GSS_C_NO_NAME = nil;

  GSS_C_GSS_CODE  = 1;
  GSS_C_MECH_CODE = 2;

  GSS_C_INDEFINITE = $FFFFFFFF;

  GSS_C_BOTH      = 0;
  GSS_C_INITIATE  = 1;
  GSS_C_ACCEPT    = 2;

  GSS_C_MUTUAL_FLAG = 2;
  GSS_C_CONF_FLAG   = 16;
  GSS_C_INTEG_FLAG  = 32;

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

  gss_mech_spnego: array [0..5] of byte = (
    43, 6, 1, 5, 5, 2);
  gss_mech_spnego_desc: gss_OID_desc = (
    length: Length(gss_mech_spnego);
    elements: @gss_mech_spnego);
  GSS_C_MECH_SPNEGO: gss_OID = @gss_mech_spnego_desc;

  gss_nt_krb5_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 2, 1);
  gss_nt_krb5_name_desc: gss_OID_desc = (
    length: Length(gss_nt_krb5_name);
    elements: @gss_nt_krb5_name);
  GSS_KRB5_NT_PRINCIPAL_NAME: gss_OID = @gss_nt_krb5_name_desc;

  gss_nt_user_name: array [0..9] of byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 1);
  gss_nt_user_name_desc: gss_OID_desc = (
    length: Length(gss_nt_user_name);
    elements: @gss_nt_user_name);
  GSS_C_NT_USER_NAME: gss_OID = @gss_nt_user_name_desc;


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
      const aPrefix: string);
  published
    /// associated GSS_C_GSS_CODE state value
    property MajorStatus: cardinal
      read fMajorStatus;
    /// associated GSS_C_MECH_CODE state value
    property MinorStatus: cardinal
      read fMinorStatus;
  end;


var
  /// access to the low-level libgssapi
  GssApi: TGssApi;

  /// library name of the MIT implementation of GSSAPI
  GssLib_MIT: TFileName = 'libgssapi_krb5.so.2';

  /// library name of the Heimdal implementation of GSSAPI
  GssLib_Heimdal: TFileName = 'libgssapi.so.3';


/// dynamically load GSSAPI library
// - do nothing if the library is already loaded
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

/// Encrypts a message
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
  const aPrefix: string = '');

/// Lists supported security mechanisms in form
// sasl:name:description
// - not all mechanisms provide human readable name and description
procedure GssEnlistMechsSupported(MechList: TStringList);



{ ****************** High-Level Client and Server Authentication using GSSAPI }

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSpn is the Service Principal Name,
// registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString): boolean;

/// Client-side authentication procedure with clear text password.
// - This function must be used when application need to use different
//  user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of
// 'username@MYDOMAIN.TLD'. Note that domain name requires to be in upper case.
// - aPassword is the user clear text password
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
// - you must use ClientForceSpn to specify server SPN before call
function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8;
  const aPassword: RawUtf8; out aOutData: RawByteString): boolean;

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
procedure ServerSspiAuthUser(var aSecContext: TSecContext; out aUserName: RawUtf8);

/// Returns name of the security package that has been used with the negotiation process
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
// - to use same value for TAuthUser.LogonName on all platforms user name
// changed from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
// - when converting fully qualified domain name to NT4-style NetBIOS name
// ServerDomainMap first checked. If domain name not found, then it's truncated on first dot,
// e.g. 'user1@CORP.ABC.COM' changed to 'CORP\user1'
// - you can change domain name conversion by registering names at server startup, e.g.
// ServerDomainMap.Add('CORP.ABC.COM', 'ABCCORP') change conversion for previuos
// example to 'ABCCORP\user1'
// - use only if automatic conversion (truncate on first dot) do it wrong
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
    result := False;
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

procedure GccCheck(AMajorStatus, AMinorStatus: cardinal; const APrefix: string);
begin
  if GSS_ERROR(AMajorStatus) <> 0 then
    raise EGssApi.Create(AMajorStatus, AMinorStatus, APrefix);
end;

const
  GSS_NAMES: array[0 .. 17] of PAnsiChar = (
    'gss_import_name', 'gss_display_name', 'gss_release_name',
    'gss_acquire_cred', 'gss_acquire_cred_with_password', 'gss_release_cred',
    'gss_init_sec_context', 'gss_accept_sec_context', 'gss_inquire_context',
    'gss_delete_sec_context', 'gss_inquire_saslname_for_mech',
    'gss_release_buffer', 'gss_wrap', 'gss_unwrap', 'gss_indicate_mechs',
    'gss_release_oid_set', 'gss_display_status',
    'krb5_gss_register_acceptor_identity');

var
  GssApiTried: TFileName;

procedure LoadGssApi(const LibraryName: TFileName);
var
  i: PtrInt;
  P: PPointerArray;
  api: TGssApi; // local instance for thread-safe load attempt
  tried: RawUtf8;
begin
  if GssApi <> nil then
    // already loaded
    exit;
  tried := LibraryName + GssLib_MIT + GssLib_Heimdal;
  if GssApiTried = tried then
    // try LoadLibrary() only if any of the .so names changed
    exit;
  GssApiTried := tried;
  api := TGssApi.Create;
  if api.TryLoadLibrary([LibraryName, GssLib_MIT, GssLib_Heimdal], nil) then
  begin
    P := @@api.gss_import_name;
    for i := 0 to high(GSS_NAMES) do
      api.Resolve(GSS_NAMES[i], @P^[i]); // no exception, set nil on failure
    if not Assigned(api.krb5_gss_register_acceptor_identity) then
      // try alternate function name
      api.Resolve('gsskrb5_register_acceptor_identity',
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
      'install either MIT or Heimdal GSSAPI implementation');
end;


{ EGssApi }

procedure GetDisplayStatus(var Msg: string; aErrorStatus: cardinal;
  StatusType: integer);
var
  Str: string;
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
    SetString(Str, MsgBuf.value, MsgBuf.length);
    GssApi.gss_release_buffer(MinSt, MsgBuf);
    if Msg <> '' then
      Msg := Msg + ' - ' + Str
    else
      Msg := Str;
  until (GSS_ERROR(MajSt) <> 0) or
        (MsgCtx = 0);
end;

constructor EGssApi.Create(aMajorStatus, aMinorStatus: cardinal;
  const aPrefix: string);
var
  Msg: string;
begin
  Msg := aPrefix;
  GetDisplayStatus(Msg, aMajorStatus, GSS_C_GSS_CODE);
  if aMinorStatus <> 0 then
    GetDisplayStatus(Msg, aMinorStatus, GSS_C_MECH_CODE);
  inherited Create(Msg);
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

procedure GssEnlistMechsSupported(MechList: TStringList);
var
  i: PtrInt;
  MinSt: cardinal;
  Mechs: gss_OID_set;
  Buf_sasl, Buf_name, Buf_desc: gss_buffer_desc;
  Sasl, Name, Desc: string;
begin
  if MechList <> nil then
  begin
    RequireGssApi;
    GssApi.gss_indicate_mechs(MinSt, Mechs);
    for i := 0 to Pred(Mechs^.count) do
    begin
      GssApi.gss_inquire_saslname_for_mech(
        MinSt, @Mechs^.elements[i], @Buf_sasl, @Buf_name, @Buf_desc);
      SetString(Sasl, Buf_sasl.value, Buf_sasl.length);
      SetString(Name, Buf_name.value, Buf_name.length);
      SetString(Desc, Buf_desc.value, Buf_desc.length);
      MechList.Add(Format('%s:%s:%s', [Sasl, Name, Desc]));
      GssApi.gss_release_buffer(MinSt, Buf_sasl);
      GssApi.gss_release_buffer(MinSt, Buf_name);
      GssApi.gss_release_buffer(MinSt, Buf_desc);
    end;
    GssApi.gss_release_oid_set(MinSt, Mechs);
  end;
end;


{ ****************** High-Level Client and Server Authentication using GSSAPI }

var
  ForceSecKerberosSpn: RawUtf8;

function ClientSspiAuthWorker(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString): boolean;
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
      aSecContext.CtxHandle, TargetName, GSS_C_MECH_SPNEGO,
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

function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  SecKerberosSpn: RawUtf8;
begin
  RequireGssApi;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64();
    MajStatus := GssApi.gss_acquire_cred(MinStatus, nil, GSS_C_INDEFINITE, nil,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GccCheck(MajStatus, MinStatus,
      'ClientSspiAuth: Failed to acquire credentials for current user');
  end;
  if aSecKerberosSpn <> '' then
    SecKerberosSpn := aSecKerberosSpn
  else
    SecKerberosSpn := ForceSecKerberosSpn;
  result := ClientSspiAuthWorker(
    aSecContext, aInData, SecKerberosSpn, aOutData);
end;

function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName, aPassword: RawUtf8;
  out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  UserName: gss_name_t;
begin
  RequireGssApi;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    InBuf.length := Length(aUserName);
    InBuf.value := pointer(aUserName);
    MajStatus := GssApi.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, UserName);
    GccCheck(MajStatus, MinStatus, 'Failed to import UserName');
    InBuf.length := Length(aPassword);
    InBuf.value := pointer(aPassword);
    MajStatus := GssApi.gss_acquire_cred_with_password(
      MinStatus, UserName, @InBuf, GSS_C_INDEFINITE, nil,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GccCheck(MajStatus, MinStatus,
      'Failed to acquire credentials for specified user');
  end;
  result := ClientSspiAuthWorker(
    aSecContext, aInData, ForceSecKerberosSpn, aOutData);
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
        'NTLM authentication not supported by GSSAPI library',[]);
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
  ServerDomainMap: array of record
    Old, New: RawUtf8;
  end;

function ServerDomainFind(const aOld: RawUtf8): PtrInt;
begin
  for result := 0 to length(ServerDomainMap) - 1 do
    with ServerDomainMap[result] do
      if IdemPropNameU(Old, aOld) then
        exit;
  result := -1;
end;

procedure ServerDomainMapRegister(const aOld, aNew: RawUtf8);
var
  i: PtrInt;
begin
  i := ServerDomainFind(aOld);
  if i < 0 then
  begin
    i := length(ServerDomainMap);
    SetLength(ServerDomainMap, i + 1);
    ServerDomainMap[i].Old := aOld;
  end;
  ServerDomainMap[i].New := aNew;
end;

procedure ServerDomainMapUnRegister(const aOld, aNew: RawUtf8);
var
  i: PtrInt;
begin
  i := ServerDomainFind(aOld);
  if i >= 0 then
    // for our purpose, it is enough
    ServerDomainMap[i].Old := '';
end;

procedure ServerDomainMapUnRegisterAll;
begin
  ServerDomainMap := nil;
end;

procedure ConvertUserName(P: PUtf8Char; Len: PtrUInt; out aUserName: RawUtf8);
var
  DomainStart, DomainEnd: PUtf8Char;
  DomainLen, i: PtrInt;
  Domain, User: RawUtf8;
begin
  // Ensure GSSAPI buffer is null-terminated
  Assert(P[Len] = #0);
  // Change user name from 'username@MYDOMAIN.TLD' to 'MYDOMAIN\username'
  DomainStart := PosChar(P, '@');
  if DomainStart <> nil then
  begin
    DomainStart^ := #0;
    Inc(DomainStart);
    DomainLen := StrLen(DomainStart);
    for i := 0 to high(ServerDomainMap) do
      with ServerDomainMap[i] do
        if IdemPropNameU(Old, DomainStart, DomainLen) then
        begin
          Domain := New;
          break;
        end;
    if {%H-}Domain = '' then
    begin
      DomainEnd := PosChar(DomainStart, '.');
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

