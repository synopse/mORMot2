/// low-level access to the SSPI/SChannel API for Win32/Win64
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.sspi;


{
  *****************************************************************************

   Security Support Provider Interface (SSPI) Support on Windows
   - Low-Level SSPI/SChannel Functions
   - Middle-Level SSPI Wrappers
   - High-Level Client and Server Authentication using SSPI

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSPOSIX}

// do-nothing-unit on non Windows system

implementation

{$else}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode;


{ ****************** Low-Level SSPI/SChannel Functions }

type
  LONG_PTR = PtrInt;

  /// SSPI context handle
  TSecHandle = record
    dwLower: LONG_PTR;
    dwUpper: LONG_PTR;
  end;
  PSecHandle = ^TSecHandle;

  /// SSPI context
  TSecContext = record
    ID: Int64;
    CredHandle: TSecHandle;
    CtxHandle: TSecHandle;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of SSPI contexts
  // - used to hold information between calls to ServerSspiAuth
  TSecContextDynArray = array of TSecContext;

  /// defines a SSPI buffer
  {$ifdef USERECORDWITHMETHODS}
  TSecBuffer = record
  {$else}
  TSecBuffer = object
  {$endif USERECORDWITHMETHODS}
  public
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
    procedure Init(aType: cardinal; aData: pointer; aSize: cardinal);
  end;
  PSecBuffer = ^TSecBuffer;

  /// describes a SSPI buffer
  {$ifdef USERECORDWITHMETHODS}
  TSecBufferDesc = record
  {$else}
  TSecBufferDesc = object
  {$endif USERECORDWITHMETHODS}
  public
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
    procedure Init(aVersion: cardinal;
      aBuffers: PSecBuffer; aBuffersCount: cardinal);
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  /// store the name associated with the context
  SecPkgContext_NamesW = record
    sUserName: PWideChar;
  end;

  /// store information about a SSPI package
  TSecPkgInfoW = record
    fCapabilities: cardinal;
    wVersion: Word;
    wRPCID: Word;
    cbMaxToken: cardinal;
    Name: PWideChar;
    Comment: PWideChar;
  end;
  /// pointer to information about a SSPI package
  PSecPkgInfoW = ^TSecPkgInfoW;

  /// store negotation information about a SSPI package
  TSecPkgContext_NegotiationInfo = record
    PackageInfo: PSecPkgInfoW;
    NegotiationState: cardinal;
  end;

  /// store various working buffer sizes of a SSPI command
  TSecPkgContext_Sizes = record
    cbMaxToken: cardinal;
    cbMaxSignature: cardinal;
    cbBlockSize: cardinal;
    cbSecurityTrailer: cardinal;
  end;

  /// store various working buffer sizes of a SSPI stream
  TSecPkgContext_StreamSizes = record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;

  /// information about SSPI supported algorithm
  TSecPkgCred_SupportedAlgs = record
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: pointer;
  end;
  /// pointer to SSPI supported algorithm
  PSecPkgCred_SupportedAlgs = ^TSecPkgCred_SupportedAlgs;

  /// information about SSPI Authority Identify
  TSecWinntAuthIdentityW = record
    User: PWideChar;
    UserLength: cardinal;
    Domain: PWideChar;
    DomainLength: cardinal;
    Password: PWideChar;
    PasswordLength: cardinal;
    Flags: cardinal
  end;
  /// pointer to SSPI Authority Identify
  PSecWinntAuthIdentityW = ^TSecWinntAuthIdentityW;

const
  SECBUFFER_VERSION = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECBUFFER_PADDING = 9;
  SECBUFFER_STREAM = 10;
  SECPKG_CRED_INBOUND  = $00000001;
  SECPKG_CRED_OUTBOUND = $00000002;
  SECPKG_ATTR_SIZES = 0;
  SECPKG_ATTR_NAMES = 1;
  SECPKG_ATTR_STREAM_SIZES = 4;
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  SECURITY_NETWORK_DREP = 0;
  SECURITY_NATIVE_DREP = $10;
  ISC_REQ_MUTUAL_AUTH = $00000002;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ASC_REQ_CONFIDENTIALITY = $00000010;
  ASC_REQ_ALLOCATE_MEMORY = $00000100;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_COMPLETE_NEEDED = $00090313;
  SEC_I_COMPLETE_AND_CONTINUE = $00090314;
  SEC_WINNT_AUTH_IDENTITY_UNICODE = $02;

function QuerySecurityPackageInfoW(pszPackageName: PWideChar;
  var ppPackageInfo: PSecPkgInfoW): integer; stdcall;

function AcquireCredentialsHandleW(pszPrincipal, pszPackage: PWideChar;
  fCredentialUse: cardinal; pvLogonId: pointer; pAuthData: PSecWinntAuthIdentityW;
  pGetKeyFn: pointer; pvGetKeyArgument: pointer; phCredential: PSecHandle;
  var ptsExpiry: LARGE_INTEGER): integer; stdcall;

function InitializeSecurityContextW(phCredential: PSecHandle; phContext: PSecHandle;
  pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: cardinal;
  pInput: PSecBufferDesc; Reserved2: cardinal; phNewContext: PSecHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: cardinal;
  var ptsExpiry: LARGE_INTEGER): integer; stdcall;

function AcceptSecurityContext(phCredential: PSecHandle; phContext: PSecHandle;
  pInput: PSecBufferDesc; fContextReq, TargetDataRep: cardinal;
  phNewContext: PSecHandle; pOutput: PSecBufferDesc; var pfContextAttr: cardinal;
  var ptsExpiry: LARGE_INTEGER): integer; stdcall;

function CompleteAuthToken(phContext: PSecHandle;
  pToken: PSecBufferDesc): integer; stdcall;

function QueryContextAttributesW(phContext: PSecHandle; ulAttribute: cardinal;
  pBuffer: pointer): integer; stdcall;

function QuerySecurityContextToken(phContext: PSecHandle;
  var Token: THandle): integer; stdcall;

function EncryptMessage(phContext: PSecHandle; fQOP: cardinal;
  pToken: PSecBufferDesc; MessageSeqNo: cardinal): integer; stdcall;

function DecryptMessage(phContext: PSecHandle; pToken: PSecBufferDesc;
  MessageSeqNo: cardinal; var fQOP: cardinal): integer; stdcall;

function FreeContextBuffer(pvContextBuffer: pointer): integer; stdcall;

function DeleteSecurityContext(phContext: PSecHandle): integer; stdcall;

function FreeCredentialsHandle(phCredential: PSecHandle): integer; stdcall;


type
  HCRYPTPROV = pointer;
  HCERTSTORE = pointer;
  PCCERT_CONTEXT = pointer;
  ALG_ID = pointer;
  _HMAPPER = pointer;

  /// SChannel credential information
  TSChannel_Cred = record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: PCCERT_CONTEXT;
    hRootStore: HCERTSTORE;
    cMappers: cardinal;
    aphMappers: _HMAPPER;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: ALG_ID;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  /// pointer to SChannel credential information
  PSChannel_Cred = ^TSChannel_Cred;


const
  UNISP_NAME = 'Microsoft Unified Security Protocol Provider';

  SP_PROT_TLS1_0_SERVER = $00000040;
  SP_PROT_TLS1_0_CLIENT = $00000080;
  SP_PROT_TLS1_0        = SP_PROT_TLS1_0_SERVER + SP_PROT_TLS1_0_CLIENT;

  SP_PROT_TLS1_1_SERVER = $00000100;
  SP_PROT_TLS1_1_CLIENT = $00000200;
  SP_PROT_TLS1_1        = SP_PROT_TLS1_1_SERVER + SP_PROT_TLS1_1_CLIENT;

  // warning: TLS 1.2 should be the preferred safe default
  SP_PROT_TLS1_2_SERVER = $00000400;
  SP_PROT_TLS1_2_CLIENT = $00000800;

  SP_PROT_TLS1_2  = SP_PROT_TLS1_2_SERVER + SP_PROT_TLS1_2_CLIENT;

  SP_PROT_TLS1_X_SERVER =
    SP_PROT_TLS1_0_SERVER + SP_PROT_TLS1_1_SERVER + SP_PROT_TLS1_2_SERVER;

  SP_PROT_TLS1_X_CLIENT =
    SP_PROT_TLS1_0_CLIENT + SP_PROT_TLS1_1_CLIENT + SP_PROT_TLS1_2_CLIENT;

  SP_PROT_TLS1_X =
    SP_PROT_TLS1_X_SERVER + SP_PROT_TLS1_X_CLIENT;


function CertOpenStore(lpszStoreProvider: PAnsiChar; dwEncodingType: cardinal;
  hCryptProv: HCRYPTPROV; dwFlags: cardinal; pvPara: pointer): HCERTSTORE; stdcall;

function CertOpenSystemStoreW(hProv: HCRYPTPROV;
  szSubsystemProtocol: PWideChar): HCERTSTORE; stdcall;

function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: cardinal): BOOL; stdcall;

function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: cardinal; pvFindPara: pointer;
  pPrevCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;



{ ****************** Middle-Level SSPI Wrappers }


type
  /// exception class raised during SSPI process
  ESynSspi = class(Exception)
  public
    constructor CreateLastOSError(const aContext: TSecContext);
  end;


/// set aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext;
  aConnectionID: Int64);

/// free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

/// encrypt a message
// - aSecContext must be set e.g. from previous success call to ServerSspiAuth
// or ClientSspiAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext;
  const aPlain: RawByteString): RawByteString;

/// decrypt a message
// - aSecContext must be set e.g. from previous success call to ServerSspiAuth
// or ClientSspiAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext;
  const aEncrypted: RawByteString): RawByteString;



{ ****************** High-Level Client and Server Authentication using SSPI }

/// client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSpn is the optional SPN domain name, e.g.
// 'mymormotservice/myserver.mydomain.tld'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from servsr
function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString): boolean;

/// client-side authentication procedure with clear text password
//  - this function must be used when application need to use different
// user credentials (not credentials of logged in user)
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aUserName is the domain and user name, in form of
// 'DomainName\UserName'
// - aPassword is the user clear text password
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from server
function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8;
  const aPassword: RawUtf8; out aOutData: RawByteString): boolean;

/// server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data recieved from client
// - aOutData contains data that must be sent to client
// - if this function returns True, server must send aOutData to client
// and call function again width data, returned from client
function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from a previous successful call to
// ServerSspiAuth()
// - aUserName contains authenticated user name
procedure ServerSspiAuthUser(var aSecContext: TSecContext;
  out aUserName: RawUtf8);

/// return the name of the security package that has been used
// during the negotiation process
// - aSecContext must be received from previous successful call to
// ServerSspiAuth() or ClientSspiAuth()
function SecPackageName(var aSecContext: TSecContext): RawUtf8;

/// force using a Kerberos SPN for server identification
// - aSecKerberosSpn is the Service Principal Name, as registered in domain,
// e.g. 'mymormotservice/myserver.mydomain.tld@MYDOMAIN.TLD'
procedure ClientForceSpn(const aSecKerberosSpn: RawUtf8);

/// high-level cross-platform initialization function
// - as called e.g. by mormot.rest.client/server.pas
// - in this unit, will just call ServerForceNTLM(false)
function InitializeDomainAuth: boolean;


const
  /// character used as marker in user name to indicates the associated domain
  SSPI_USER_CHAR = '\';

  // SSPI package names. Client always use Negotiate
  // Server detect Negotiate or NTLM requests and use appropriate package
  SECPKGNAMENTLM = 'NTLM';
  SECPKGNAMENEGOTIATE = 'Negotiate';

var
  /// HTTP Challenge name for SSPI authentication
  // - call ServerForceNTLM() to specialize this value to 'NTLM' or 'Negotiate'
  SECPKGNAMEHTTP: RawUtf8;

  /// HTTP Challenge name, converted into uppercase for IdemPChar() pattern
  SECPKGNAMEHTTP_UPPER: RawUtf8;

  /// HTTP header to be set for SSPI authentication
  // - call ServerForceNTLM() to specialize this value to either
  // 'WWW-Authenticate: NTLM' or 'WWW-Authenticate: Negotiate';
  SECPKGNAMEHTTPWWWAUTHENTICATE: RawUtf8;

  /// HTTP header pattern received for SSPI authentication
  // - call ServerForceNTLM() to specialize this value to either
  // 'AUTHORIZATION: NTLM ' or 'AUTHORIZATION: NEGOTIATE '
  SECPKGNAMEHTTPAUTHORIZATION: RawUtf8;

  /// by default, this unit will use Negotiate/Kerberos for client authentication
  // - can be set to TRUE to use the deprecated and unsafe NTLM protocol instead
  // - use case: SPNs not configured properly in domain
  // - see for details https://synopse.info/forum/viewtopic.php?id=931&p=3
  SspiForceNtlmClient: boolean = false;



implementation


{ ****************** Low-Level SSPI/SChannel Functions }

const
  secur32 = 'secur32.dll';

function QuerySecurityPackageInfoW;  external secur32;
function AcquireCredentialsHandleW;  external secur32;
function InitializeSecurityContextW; external secur32;
function AcceptSecurityContext;      external secur32;
function CompleteAuthToken;          external secur32;
function QueryContextAttributesW;    external secur32;
function QuerySecurityContextToken;  external secur32;
function EncryptMessage;             external secur32;
function DecryptMessage;             external secur32;
function FreeContextBuffer;          external secur32;
function DeleteSecurityContext;      external secur32;
function FreeCredentialsHandle;      external secur32;

const
  crypt32 = 'crypt32.dll';

function CertOpenStore;              external crypt32;
function CertOpenSystemStoreW;       external crypt32;
function CertCloseStore;             external crypt32;
function CertFindCertificateInStore; external crypt32;


{ TSecBuffer }

procedure TSecBuffer.Init(aType: cardinal; aData: pointer;
  aSize: cardinal);
begin
  BufferType := aType;
  pvBuffer := aData;
  cbBuffer := aSize;
end;


{ TSecBufferDesc }

procedure TSecBufferDesc.Init(aVersion: cardinal; aBuffers: PSecBuffer;
  aBuffersCount: cardinal);
begin
  ulVersion := aVersion;
  pBuffers := aBuffers;
  cBuffers := aBuffersCount;
end;



{ ****************** Middle-Level SSPI Wrappers }


{ ESynSspi }

constructor ESynSspi.CreateLastOSError(const aContext: TSecContext);
var
  error: integer;
begin
  error := GetLastError;
  CreateFmt('SSPI API Error %x [%s] for ConnectionID=%d',
    [error, SysErrorMessage(error), aContext.ID]);
end;


procedure InvalidateSecContext(var aSecContext: TSecContext;
  aConnectionID: Int64);
begin
  aSecContext.ID := aConnectionID;
  aSecContext.CredHandle.dwLower := -1;
  aSecContext.CredHandle.dwUpper := -1;
  aSecContext.CtxHandle.dwLower := -1;
  aSecContext.CtxHandle.dwUpper := -1;
  aSecContext.CreatedTick64 := 0;
end;

procedure FreeSecurityContext(var handle: TSecHandle);
begin
  if (handle.dwLower <> -1) or
     (handle.dwUpper <> -1) then
  begin
    DeleteSecurityContext(@handle);
    handle.dwLower := -1;
    handle.dwUpper := -1;
  end;
end;

procedure FreeCredentialsContext(var handle: TSecHandle);
begin
  if (handle.dwLower <> -1) or
     (handle.dwUpper <> -1) then
  begin
    FreeCredentialsHandle(@handle);
    handle.dwLower := -1;
    handle.dwUpper := -1;
  end;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
begin
  FreeSecurityContext(aSecContext.CtxHandle);
  FreeCredentialsContext(aSecContext.CredHandle);
end;

function SecEncrypt(var aSecContext: TSecContext;
  const aPlain: RawByteString): RawByteString;
var
  Sizes: TSecPkgContext_Sizes;
  SrcLen, EncLen: cardinal;
  Token: array [0..127] of byte; // Usually 60 bytes
  Padding: array [0..63] of byte; // Usually 1 byte
  InBuf: array[0..2] of TSecBuffer;
  InDesc: TSecBufferDesc;
  EncBuffer: RawByteString;
  Status: integer;
  BufPtr: PByte;
begin
  // Sizes.cbSecurityTrailer is size of the trailer (signature + padding) block
  if QueryContextAttributesW(
       @aSecContext.CtxHandle, SECPKG_ATTR_SIZES, @Sizes) <> 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  // Encrypted data buffer structure:
  //
  // SSPI/Kerberos Interoperability with GSSAPI
  // https://msdn.microsoft.com/library/windows/desktop/aa380496.aspx
  //
  // GSS-API wrapper for Microsoft's Kerberos SSPI in Windows 2000
  // http://www.kerberos.org/software/samples/gsskrb5/gsskrb5/krb5/krb5msg.c
  //
  //   cbSecurityTrailer bytes   SrcLen bytes     cbBlockSize bytes or less
  //   (60 bytes)                                 (0 bytes, not used)
  // +-------------------------+----------------+--------------------------+
  // | Trailer                 | Data           | Padding                  |
  // +-------------------------+----------------+--------------------------+
  Assert(Sizes.cbSecurityTrailer <= High(Token)+1);
  {%H-}InBuf[0].Init(SECBUFFER_TOKEN, @Token[0], Sizes.cbSecurityTrailer);
  // Encoding done in-place, so we copy the data
  SrcLen := Length(aPlain);
  SetString(EncBuffer, PAnsiChar(pointer(aPlain)), SrcLen);
  InBuf[1].Init(SECBUFFER_DATA, pointer(EncBuffer), SrcLen);
  Assert(Sizes.cbBlockSize <= High(Padding)+1);
  InBuf[2].Init(SECBUFFER_PADDING, @Padding[0], Sizes.cbBlockSize);
  InDesc.Init(SECBUFFER_VERSION, @InBuf, 3);
  Status := EncryptMessage(@aSecContext.CtxHandle, 0, @InDesc, 0);
  if Status < 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  EncLen := InBuf[0].cbBuffer + InBuf[1].cbBuffer + InBuf[2].cbBuffer;
  SetLength(result, EncLen);
  BufPtr := PByte(result);
  MoveFast(PByte(InBuf[0].pvBuffer)^, BufPtr^, InBuf[0].cbBuffer);
  Inc(BufPtr, InBuf[0].cbBuffer);
  MoveFast(PByte(InBuf[1].pvBuffer)^, BufPtr^, InBuf[1].cbBuffer);
  Inc(BufPtr, InBuf[1].cbBuffer);
  MoveFast(PByte(InBuf[2].pvBuffer)^, BufPtr^, InBuf[2].cbBuffer);
end;

function SecDecrypt(var aSecContext: TSecContext;
  const aEncrypted: RawByteString): RawByteString;
var
  EncLen, SigLen: cardinal;
  BufPtr: PByte;
  InBuf: array [0..1] of TSecBuffer;
  InDesc: TSecBufferDesc;
  Status: integer;
  QOP: cardinal;
begin
  EncLen := Length(aEncrypted);
  BufPtr := PByte(aEncrypted);
  if EncLen < SizeOf(cardinal) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    raise ESynSspi.CreateLastOSError(aSecContext);
  end;
  // Hack for compatibility with previous versions.
  // Should be removed in future.
  // Old version buffer format - first 4 bytes is Trailer length, skip it.
  // 16 bytes for NTLM and 60 bytes for Kerberos
  SigLen := PCardinal(BufPtr)^;
  if (SigLen = 16) or
     (SigLen = 60) then
  begin
    Inc(BufPtr, SizeOf(cardinal));
    Dec(EncLen, SizeOf(cardinal));
  end;
  {%H-}InBuf[0].Init(SECBUFFER_STREAM, BufPtr, EncLen);
  InBuf[1].Init(SECBUFFER_DATA, nil, 0);
  InDesc.Init(SECBUFFER_VERSION, @InBuf, 2);
  Status := DecryptMessage(@aSecContext.CtxHandle, @InDesc, 0, QOP);
  if Status < 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  SetString(result, PAnsiChar(InBuf[1].pvBuffer), InBuf[1].cbBuffer);
  FreeContextBuffer(InBuf[1].pvBuffer);
end;



{ ****************** High-Level Client and Server Authentication using SSPI }

var
  ForceSecKerberosSpn: SynUnicode;

function ClientSspiAuthWorker(var aSecContext: TSecContext;
  const aInData: RawByteString; pszTargetName: PWideChar;
  pAuthData: PSecWinntAuthIdentityW;
  out aOutData: RawByteString): boolean;
var
  InBuf: TSecBuffer;
  InDesc: TSecBufferDesc;
  InDescPtr: PSecBufferDesc;
  SecPkgInfo: PSecPkgInfoW;
  Expiry: LARGE_INTEGER;
  LInCtxPtr: PSecHandle;
  OutBuf: TSecBuffer;
  OutDesc: TSecBufferDesc;
  CtxReqAttr: cardinal;
  CtxAttr: cardinal;
  Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);
  if (aSecContext.CredHandle.dwLower = -1) and
     (aSecContext.CredHandle.dwUpper = -1) then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if QuerySecurityPackageInfoW(SECPKGNAMENEGOTIATE, SecPkgInfo) <> 0 then
      raise ESynSspi.CreateLastOSError(aSecContext);
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo^.Name, SECPKG_CRED_OUTBOUND,
          nil, pAuthData, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
        raise ESynSspi.CreateLastOSError(aSecContext);
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    InDescPtr := nil;
    LInCtxPtr := nil;
  end
  else
  begin
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers := 1;
    InDesc.pBuffers := @InBuf;
    InDescPtr := @InDesc;
    LInCtxPtr := @aSecContext.CtxHandle;
  end;
  CtxReqAttr := ISC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY;
  if pszTargetName <> nil then
    CtxReqAttr := CtxReqAttr or ISC_REQ_MUTUAL_AUTH;
  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;
  Status := InitializeSecurityContextW(@aSecContext.CredHandle, LInCtxPtr,
    pszTargetName, CtxReqAttr, 0, SECURITY_NATIVE_DREP, InDescPtr, 0,
    @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);
  result := (Status = SEC_I_CONTINUE_NEEDED) or
            (Status = SEC_I_COMPLETE_AND_CONTINUE);
  if (Status = SEC_I_COMPLETE_NEEDED) or
     (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

function ClientSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; const aSecKerberosSpn: RawUtf8;
  out aOutData: RawByteString): boolean;
var
  TargetName: PWideChar;
begin
  if aSecKerberosSpn <> '' then
    TargetName := pointer(Utf8ToSynUnicode(aSecKerberosSpn))
  else if ForceSecKerberosSpn <> '' then
    TargetName := pointer(ForceSecKerberosSpn)
  else
    TargetName := nil;
  result :=  ClientSspiAuthWorker(
    aSecContext, aInData, TargetName, nil, aOutData);
end;

function ClientSspiAuthWithPassword(var aSecContext: TSecContext;
  const aInData: RawByteString; const aUserName: RawUtf8;
  const aPassword: RawUtf8; out aOutData: RawByteString): boolean;
var
  UserPos: Integer;
  Domain, User, Password: SynUnicode;
  AuthIdentity: TSecWinntAuthIdentityW;
  TargetName: PWideChar;
begin
  UserPos := PosExChar('\', aUserName);
  if UserPos = 0 then
  begin
    Domain := '';
    Utf8ToSynUnicode(aUserName, User);
  end
  else
  begin
    Utf8ToSynUnicode(Copy(aUserName, 1, UserPos - 1), Domain);
    Utf8ToSynUnicode(Copy(aUserName, UserPos + 1, MaxInt), User);
  end;
  Utf8ToSynUnicode(aPassword, PassWord);
  AuthIdentity.Domain := pointer(Domain);
  AuthIdentity.DomainLength := Length(Domain);
  AuthIdentity.User := pointer(User);
  AuthIdentity.UserLength := Length(User);
  AuthIdentity.Password := pointer(Password);
  AuthIdentity.PasswordLength := Length(Password);
  AuthIdentity.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;
  if ForceSecKerberosSpn <> '' then
    TargetName := pointer(ForceSecKerberosSpn)
  else
    TargetName := nil;
  result :=  ClientSspiAuthWorker(
    aSecContext, aInData, TargetName, @AuthIdentity, aOutData);
end;

function ServerSspiAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): boolean;
var
  InBuf: TSecBuffer;
  InDesc: TSecBufferDesc;
  SecPkgInfo: PSecPkgInfoW;
  Expiry: LARGE_INTEGER;
  LInCtxPtr: PSecHandle;
  OutBuf: TSecBuffer;
  OutDesc: TSecBufferDesc;
  CtxAttr: cardinal;
  Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);
  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers := 1;
  InDesc.pBuffers := @InBuf;
  if (aSecContext.CredHandle.dwLower = -1) and
     (aSecContext.CredHandle.dwUpper = -1) then
  begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if IdemPChar(Pointer(aInData), 'NTLMSSP') then
    begin
      if QuerySecurityPackageInfoW(SECPKGNAMENTLM, SecPkgInfo) <> 0 then
        raise ESynSspi.CreateLastOSError(aSecContext);
    end
    else
      if QuerySecurityPackageInfoW(SECPKGNAMENEGOTIATE, SecPkgInfo) <> 0 then
        raise ESynSspi.CreateLastOSError(aSecContext);
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo^.Name, SECPKG_CRED_INBOUND,
          nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
        raise ESynSspi.CreateLastOSError(aSecContext);
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    LInCtxPtr := nil;
  end
  else
    LInCtxPtr := @aSecContext.CtxHandle;
  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;
  Status := AcceptSecurityContext(@aSecContext.CredHandle, LInCtxPtr, @InDesc,
      ASC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY,
      SECURITY_NATIVE_DREP, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);
  result := (Status = SEC_I_CONTINUE_NEEDED) or
            (Status = SEC_I_COMPLETE_AND_CONTINUE);
  if (Status = SEC_I_COMPLETE_NEEDED) or
     (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
      raise ESynSspi.CreateLastOSError(aSecContext);
  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

procedure ServerSspiAuthUser(var aSecContext: TSecContext;
  out aUserName: RawUtf8);
var
  Names: SecPkgContext_NamesW;
begin
  if QueryContextAttributesW(@aSecContext.CtxHandle,
       SECPKG_ATTR_NAMES, @Names) <> 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  aUserName := RawUnicodeToUtf8(Names.sUserName, StrLenW(Names.sUserName));
  FreeContextBuffer(Names.sUserName);
end;

function SecPackageName(var aSecContext: TSecContext): RawUtf8;
var
  NegotiationInfo: TSecPkgContext_NegotiationInfo;
begin
  if QueryContextAttributesW(@aSecContext.CtxHandle,
       SECPKG_ATTR_NEGOTIATION_INFO, @NegotiationInfo) <> 0 then
    raise ESynSspi.CreateLastOSError(aSecContext);
  result := RawUnicodeToUtf8(NegotiationInfo.PackageInfo^.Name,
              StrLenW(NegotiationInfo.PackageInfo^.Name));
  FreeContextBuffer(NegotiationInfo.PackageInfo);
end;

procedure ClientForceSpn(const aSecKerberosSpn: RawUtf8);
begin
  Utf8ToSynUnicode(aSecKerberosSpn, ForceSecKerberosSpn);
end;

var
  DomainAuthMode: (damUndefined, damNtlm, damNegotiate);

procedure SetDomainAuthMode;
begin
  if SspiForceNtlmClient then
  begin
    SECPKGNAMEHTTP := 'NTLM';
    DomainAuthMode := damNtlm;
  end
  else
  begin
    SECPKGNAMEHTTP := 'Negotiate';
    DomainAuthMode := damNegotiate;
  end;
  SECPKGNAMEHTTP_UPPER := UpperCase(SECPKGNAMEHTTP);
  SECPKGNAMEHTTPWWWAUTHENTICATE := 'WWW-Authenticate: ' + SECPKGNAMEHTTP;
  SECPKGNAMEHTTPAUTHORIZATION := 'AUTHORIZATION: ' + SECPKGNAMEHTTP_UPPER + ' ';
end;

function InitializeDomainAuth: boolean;
begin
  if (DomainAuthMode = damUndefined) or
     (SspiForceNtlmClient <> (DomainAuthMode = damNtlm)) then
    SetDomainAuthMode;
  result := true;
end;

{$endif OSPOSIX}

end.

