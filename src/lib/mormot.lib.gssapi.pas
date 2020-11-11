/// low-level access to the GSSAPI on Linux/POSIX
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

{$ifdef MSWINDOWS}

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

  gss_mech_spnego: array [0..5] of Byte = (
    43, 6, 1, 5, 5, 2);
  gss_mech_spnego_desc: gss_OID_desc = (
    length: Length(gss_mech_spnego);
    elements: @gss_mech_spnego);
  GSS_C_MECH_SPNEGO: gss_OID = @gss_mech_spnego_desc;

  gss_nt_krb5_name: array [0..9] of Byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 2, 1);
  gss_nt_krb5_name_desc: gss_OID_desc = (
    length: Length(gss_nt_krb5_name);
    elements: @gss_nt_krb5_name);
  GSS_KRB5_NT_PRINCIPAL_NAME: gss_OID = @gss_nt_krb5_name_desc;

  gss_nt_user_name: array [0..9] of Byte = (
    42, 134, 72, 134, 247, 18, 1, 2, 1, 1);
  gss_nt_user_name_desc: gss_OID_desc = (
    length: Length(gss_nt_user_name);
    elements: @gss_nt_user_name);
  GSS_C_NT_USER_NAME: gss_OID = @gss_nt_user_name_desc;


type
  TGSSAPI = class(TSynLibrary)
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
  EGSSAPI = class(Exception)
  private
    fMajorStatus: cardinal;
    fMinorStatus: cardinal;
  public
    /// initialize an gssapi library exception
    constructor Create(aMajorStatus, aMinorStatus: cardinal;
      const aPrefix: string);
    /// associated GSS_C_GSS_CODE state value
    property MajorStatus: cardinal
      read fMajorStatus;
    /// associated GSS_C_MECH_CODE state value
    property MinorStatus: cardinal
      read fMinorStatus;
  end;


var
  /// access to the low-level libgssapi
  GSSAPI: TGSSAPI;

  /// library name of the MIT implementation of GSSAPI
  GSSLib_MIT: TFileName = 'libgssapi_krb5.so.2';

  /// library name of the Heimdal implementation of GSSAPI
  GSSLib_Heimdal: TFileName = 'libgssapi.so.3';


/// Dynamically load GSSAPI library
// - in multithreaded server application you must call LoadGSSAPI
// at startup to avoid race condition (if you do not use mormot.rest.server.pas)
// - do nothing if the library is already loaded
procedure LoadGSSAPI(const LibraryName: TFileName = '');

/// Call this function to check whether GSSAPI library loaded or not
function GSSAPILoaded: boolean;

/// Call this function to check whether GSSAPI library loaded
// and raise exception if not
procedure RequireGSSAPI;


// some macros for GSSAPI functions process
function GSS_CALLING_ERROR(x: cardinal): cardinal; inline;
function GSS_ROUTINE_ERROR(x: cardinal): cardinal; inline;
function GSS_SUPPLEMENTARY_INFO(x: cardinal): cardinal; inline;
function GSS_ERROR(x: cardinal): cardinal; inline;

function gss_compare_oid(oid1, oid2: gss_OID): boolean;


{ ****************** Middle-Level GSSAPI Wrappers }

type
  /// GSSAPI Auth context
  // - first field should be an Int64 ID
  TSecContext = record
    ID: Int64;
    CredHandle: pointer;
    CtxHandle: pointer;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of Auth contexts
  // - used to hold information between calls to ServerSSPIAuth
  TSecContextDynArray = array of TSecContext;


/// Sets aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext;
  aConnectionID: Int64);

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

/// Encrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext;
  const aPlain: RawByteString): RawByteString;

/// Decrypts a message
// - aSecContext must be set e.g. from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext;
  const aEncrypted: RawByteString): RawByteString;

/// Checks the return value of GSSAPI call and raises ESynGSSAPI exception
// when it indicates failure
procedure GSSCheck(aMajorStatus, aMinorStatus: cardinal;
  const aPrefix: string = '');

/// Lists supported security mechanisms in form
// sasl:name:description
// - not all mechanisms provide human readable name and description
procedure GSSEnlistMechsSupported(MechList: TStringList);



{ ****************** High-Level Client and Server Authentication using GSSAPI }

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSPN is the Service Principal Name,
// registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
function ClientSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
  out aOutData: RawByteString): boolean;

/// Client-side authentication procedure with clear text password.
//  This function must be used when application need to use different
//  user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of
// 'username@MYDOMAIN.TLD'. Note that domain name requires to be in upper case.
// - aPassword is the user clear text password
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again with data, returned from server
// - you must use ClientForceSPN to specify server SPN before call
function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUTF8;
  const aPassword: RawUTF8; out aOutData: RawByteString): boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again with data, returned from client
function ServerSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previous successful call to ServerSSPIAuth
// - aUserName contains authenticated user name
procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);

/// Returns name of the security package that has been used with the negotiation process
// - aSecContext must be received from previous success call to ServerSSPIAuth
// or ClientSSPIAuth
function SecPackageName(var aSecContext: TSecContext): RawUTF8;

/// Force using aSecKerberosSPN for server identification
// - aSecKerberosSPN is the Service Principal Name, registered in domain, e.g.
// 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);

/// Force loading server credentials from specified keytab file
// - by default, clients may authenticate to any service principal
// in the default keytab (/etc/krb5.keytab or the value of the KRB5_KTNAME
// environment variable)
procedure ServerForceKeytab(const aKeytab: RawUTF8);

const
  /// HTTP header to be set for authentication
  SECPKGNAMEHTTPWWWAUTHENTICATE = 'WWW-Authenticate: Negotiate';

  /// HTTP header pattern received for authentication
  SECPKGNAMEHTTPAUTHORIZATION = 'AUTHORIZATION: NEGOTIATE ';

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
procedure ServerDomainMapRegister(const aOld, aNew: RawUTF8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegister(const aOld, aNew: RawUTF8);

/// help converting fully qualified domain names to NT4-style NetBIOS names
procedure ServerDomainMapUnRegisterAll;


/// high-level cross-platform initialization function
// - as called e.g. by mormot.rest.client/server.pas
// - in this unit, will just call LoadGSSAPI('')
procedure InitializeDomainAuth;


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

procedure GSSCheck(AMajorStatus, AMinorStatus: cardinal; const APrefix: string);
begin
  if GSS_ERROR(AMajorStatus) <> 0 then
    raise EGSSAPI.Create(AMajorStatus, AMinorStatus, APrefix);
end;

const
  GSS_NAMES: array[0 .. 17] of PChar = (
    'gss_import_name', 'gss_display_name', 'gss_release_name',
    'gss_acquire_cred', 'gss_acquire_cred_with_password', 'gss_release_cred',
    'gss_init_sec_context', 'gss_accept_sec_context', 'gss_inquire_context',
    'gss_delete_sec_context', 'gss_inquire_saslname_for_mech',
    'gss_release_buffer', 'gss_wrap', 'gss_unwrap', 'gss_indicate_mechs',
    'gss_release_oid_set', 'gss_display_status',
    'krb5_gss_register_acceptor_identity');

procedure LoadGSSAPI(const LibraryName: TFileName);
var
  i: PtrInt;
  P: PPointerArray;
  api: TGSSAPI;
begin
  if GSSAPI <> nil then
    // already loaded
    exit;
  api := TGSSAPI.Create;
  if api.TryLoadLibrary([LibraryName, GSSLib_MIT, GSSLib_Heimdal], nil) then
  begin
    P := @@api.gss_import_name;
    for i := 0 to high(GSS_NAMES) do
      api.GetProc(GSS_NAMES[i], @P^[i]);
    if not Assigned(api.krb5_gss_register_acceptor_identity) then
      // try alternate function name
      api.GetProc('gsskrb5_register_acceptor_identity',
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
        if GSSAPI = nil then
        begin
          GSSAPI := api;
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

function GSSAPILoaded: boolean;
begin
  result := GSSAPI <> nil
end;

procedure RequireGSSAPI;
begin
  if GSSAPI = nil then
    raise ENotSupportedException.Create('No GSSAPI library found - please ' +
      'install either MIT or Heimdal GSSAPI implementation');
end;


{ EGSSAPI }

procedure GetDisplayStatus(var Msg: string; aErrorStatus: cardinal;
  StatusType: integer);
var
  Str: string;
  MsgCtx: cardinal;
  MsgBuf: gss_buffer_desc;
  MajSt, MinSt: cardinal;
begin
  if GSSAPI = nil then
    exit;
  MsgCtx := 0;
  repeat
    MajSt := GSSAPI.gss_display_status(
      MinSt, aErrorStatus, StatusType, nil, MsgCtx, MsgBuf);
    SetString(Str, MsgBuf.value, MsgBuf.length);
    GSSAPI.gss_release_buffer(MinSt, MsgBuf);
    if Msg <> '' then
      Msg := Msg + ': ' + Str
    else
      Msg := Str;
  until (GSS_ERROR(MajSt) <> 0) or
        (MsgCtx = 0);
end;

constructor EGSSAPI.Create(aMajorStatus, aMinorStatus: cardinal;
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
    GSSAPI.gss_delete_sec_context(MinStatus, aSecContext.CtxHandle, nil);
  if aSecContext.CredHandle <> nil then
    GSSAPI.gss_release_cred(MinStatus, aSecContext.CredHandle);
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
  MajStatus := GSSAPI.gss_wrap(
    MinStatus, aSecContext.CtxHandle, 1, 0, @InBuf, nil, @OutBuf);
  GSSCheck(MajStatus, MinStatus, 'Failed to encrypt message');
  SetString(result, PAnsiChar(OutBuf.value), OutBuf.length);
  GSSAPI.gss_release_buffer(MinStatus, OutBuf);
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
  MajStatus := GSSAPI.gss_unwrap(
    MinStatus, aSecContext.CtxHandle, @InBuf, @OutBuf, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to decrypt message');
  SetString(result, PAnsiChar(OutBuf.value), OutBuf.length);
  GSSAPI.gss_release_buffer(MinStatus, OutBuf);
end;

procedure GSSEnlistMechsSupported(MechList: TStringList);
var
  i: PtrInt;
  MajSt, MinSt: cardinal;
  Mechs: gss_OID_set;
  Buf_sasl, Buf_name, Buf_desc: gss_buffer_desc;
  Sasl, Name, Desc: string;
begin
  if MechList <> nil then
  begin
    RequireGSSAPI;
    MajSt := GSSAPI.gss_indicate_mechs(MinSt, Mechs);
    for i := 0 to Pred(Mechs^.count) do
    begin
      MajSt := GSSAPI.gss_inquire_saslname_for_mech(
        MinSt, @Mechs^.elements[i], @Buf_sasl, @Buf_name, @Buf_desc);
      SetString(Sasl, Buf_sasl.value, Buf_sasl.length);
      SetString(Name, Buf_name.value, Buf_name.length);
      SetString(Desc, Buf_desc.value, Buf_desc.length);
      MechList.Add(Format('%s:%s:%s', [Sasl, Name, Desc]));
      GSSAPI.gss_release_buffer(MinSt, Buf_sasl);
      GSSAPI.gss_release_buffer(MinSt, Buf_name);
      GSSAPI.gss_release_buffer(MinSt, Buf_desc);
    end;
    MajSt := GSSAPI.gss_release_oid_set(MinSt, Mechs);
  end;
end;


{ ****************** High-Level Client and Server Authentication using GSSAPI }

var
  ForceSecKerberosSPN: RawUTF8;

function ClientSSPIAuthWorker(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
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
  if aSecKerberosSPN <> '' then
  begin
    InBuf.length := Length(aSecKerberosSPN);
    InBuf.value := pointer(aSecKerberosSPN);
    MajStatus := GSSAPI.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, TargetName);
    GSSCheck(MajStatus, MinStatus, 'Failed to import server SPN');
  end;
  try
    CtxReqAttr := GSS_C_INTEG_FLAG or GSS_C_CONF_FLAG;
    if TargetName <> nil then
      CtxReqAttr := CtxReqAttr or GSS_C_MUTUAL_FLAG;
    InBuf.length := Length(aInData);
    InBuf.value := pointer(aInData);
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := GSSAPI.gss_init_sec_context(MinStatus, aSecContext.CredHandle,
      aSecContext.CtxHandle, TargetName, GSS_C_MECH_SPNEGO,
      CtxReqAttr, GSS_C_INDEFINITE, nil, @InBuf, nil, @OutBuf, @CtxAttr, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to initialize security context');
    result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;
    SetString(aOutData, PAnsiChar(OutBuf.value), OutBuf.length);
    GSSAPI.gss_release_buffer(MinStatus, OutBuf);
  finally
    if TargetName <> nil then
      GSSAPI.gss_release_name(MinStatus, TargetName);
  end;
end;

function ClientSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
  out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  SecKerberosSPN: RawUTF8;
begin
  RequireGSSAPI;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64();
    MajStatus := GSSAPI.gss_acquire_cred(MinStatus, nil, GSS_C_INDEFINITE, nil,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus,
      'Failed to acquire credentials for current user');
  end;
  if aSecKerberosSPN <> '' then
    SecKerberosSPN := aSecKerberosSPN
  else
    SecKerberosSPN := ForceSecKerberosSPN;
  result := ClientSSPIAuthWorker(
    aSecContext, aInData, SecKerberosSPN, aOutData);
end;

function ClientSSPIAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName, aPassword: RawUTF8;
  out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  UserName: gss_name_t;
begin
  RequireGSSAPI;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    InBuf.length := Length(aUserName);
    InBuf.value := pointer(aUserName);
    MajStatus := GSSAPI.gss_import_name(
      MinStatus, @InBuf, GSS_KRB5_NT_PRINCIPAL_NAME, UserName);
    GSSCheck(MajStatus, MinStatus, 'Failed to import UserName');
    InBuf.length := Length(aPassword);
    InBuf.value := pointer(aPassword);
    MajStatus := GSSAPI.gss_acquire_cred_with_password(
      MinStatus, UserName, @InBuf, GSS_C_INDEFINITE, nil,
      GSS_C_INITIATE, aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus,
      'Failed to acquire credentials for specified user');
  end;
  result := ClientSSPIAuthWorker(
    aSecContext, aInData, ForceSecKerberosSPN, aOutData);
end;

function ServerSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;
var
  MajStatus, MinStatus: cardinal;
  InBuf: gss_buffer_desc;
  OutBuf: gss_buffer_desc;
  CtxAttr: cardinal;
begin
  RequireGSSAPI;
  if aSecContext.CredHandle = nil then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if IdemPChar(pointer(aInData), 'NTLMSSP') then
      raise EGSSAPI.CreateFmt(
        'NTLM authentication not supported by GSSAPI library',[]);
    MajStatus := GSSAPI.gss_acquire_cred(
      MinStatus, nil, GSS_C_INDEFINITE, nil, GSS_C_ACCEPT,
      aSecContext.CredHandle, nil, nil);
    GSSCheck(MajStatus, MinStatus, 'Failed to aquire credentials for service');
  end;
  InBuf.length := Length(aInData);
  InBuf.value := PByte(aInData);
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GSSAPI.gss_accept_sec_context(MinStatus, aSecContext.CtxHandle,
    aSecContext.CredHandle, @InBuf, nil, nil, nil, @OutBuf, @CtxAttr, nil, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to accept client credentials');
  result := (MajStatus and GSS_S_CONTINUE_NEEDED) <> 0;
  SetString(aOutData, PAnsiChar(OutBuf.value), OutBuf.length);
  GSSAPI.gss_release_buffer(MinStatus, OutBuf);
end;

var
  ServerDomainMap: array of record
    Old, New: RawUTF8;
  end;

function ServerDomainFind(const aOld: RawUTF8): PtrInt;
begin
  for result := 0 to length(ServerDomainMap) - 1 do
    with ServerDomainMap[result] do
      if (length(Old) = length(aOld)) and
         IdemPChar(pointer(aOld), pointer(Old)) then
        exit;
  result := -1;
end;

procedure ServerDomainMapRegister(const aOld, aNew: RawUTF8);
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

procedure ServerDomainMapUnRegister(const aOld, aNew: RawUTF8);
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

procedure ConvertUserName(P: PUTF8Char; Len: PtrUInt; out aUserName: RawUTF8);
var
  DomainStart, DomainEnd: PUTF8Char;
  DomainLen, i: PtrInt;
  Domain, User: RawUTF8;
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
        if (length(Old) = DomainLen) and
           IdemPChar(DomainStart, pointer(Old)) then
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
    SetString(aUserName, P, Len);
end;

procedure ServerSSPIAuthUser(var aSecContext: TSecContext;
  out aUserName: RawUTF8);
var
  MajStatus, MinStatus: cardinal;
  SrcName: gss_name_t;
  OutBuf: gss_buffer_desc;
  NameType: gss_OID;
begin
  RequireGSSAPI;
  MajStatus := GSSAPI.gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    @SrcName, nil, nil, nil, nil, nil, nil);
  GSSCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (src_name)');
  try
    OutBuf.length := 0;
    OutBuf.value := nil;
    MajStatus := GSSAPI.gss_display_name(
      MinStatus, SrcName, @OutBuf, @NameType);
    GSSCheck(MajStatus, MinStatus,
      'Failed to obtain name for authenticated user');
    if gss_compare_oid(NameType, GSS_KRB5_NT_PRINCIPAL_NAME) then
      ConvertUserName(PUTF8Char(OutBuf.value), OutBuf.length, aUserName);
    GSSAPI.gss_release_buffer(MinStatus, OutBuf);
  finally
    GSSAPI.gss_release_name(MinStatus, SrcName);
  end;
end;

function SecPackageName(var aSecContext: TSecContext): RawUTF8;
var
  MajStatus, MinStatus: cardinal;
  MechType: gss_OID;
  OutBuf: gss_buffer_desc;
begin
  RequireGSSAPI;
  MajStatus := GSSAPI.gss_inquire_context(MinStatus, aSecContext.CtxHandle,
    nil, nil, nil, @MechType, nil, nil, nil);
  GSSCheck(MajStatus, MinStatus,
    'Failed to inquire security context information (mech_type)');
  OutBuf.length := 0;
  OutBuf.value := nil;
  MajStatus := GSSAPI.gss_inquire_saslname_for_mech(
    MinStatus, MechType, nil, @OutBuf, nil);
  GSSCheck(MajStatus, MinStatus, 'Failed to obtain name for mech');
  SetString(result, PAnsiChar(OutBuf.value), OutBuf.length);
  GSSAPI.gss_release_buffer(MinStatus, OutBuf);
end;

procedure ClientForceSPN(const aSecKerberosSPN: RawUTF8);
begin
  ForceSecKerberosSPN := aSecKerberosSPN;
end;

procedure ServerForceKeytab(const aKeytab: RawUTF8);
begin
  if Assigned(GSSAPI.krb5_gss_register_acceptor_identity) then
    GSSAPI.krb5_gss_register_acceptor_identity(pointer(aKeytab));
end;

procedure InitializeDomainAuth;
begin
  LoadGSSAPI('');
end;

finalization
  GSSAPI.Free;

{$endif MSWINDOWS}

end.

