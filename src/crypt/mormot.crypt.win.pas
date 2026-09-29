/// Framework Core Cryptography using Windows API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.win;

{
  *****************************************************************************

    Direct Cryptography using Windows API
    - AES cypher/uncypher using PROV_RSA_AES CryptoApi
    - High-Level Windows Certificate Store Integration
    - Middle-Level Windows CNG Private Key Integration

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSWINDOWS} // do-nothing unit outside of Windows

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security, // low-level Windows Security API
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti,
  mormot.core.log,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.x509,
  mormot.lib.sspi;



{ ***************** AES cypher/uncypher using PROV_RSA_AES CryptoApi }

{$ifdef USE_PROV_RSA_AES}

type
  /// handle AES cypher/uncypher using Windows CryptoApi and the
  // official Microsoft AES Cryptographic Provider (PROV_RSA_AES)
  // - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa386979
  // - timing of our optimized asm versions, for small (<=8KB) block processing
  // (similar to standard web pages or most typical JSON/XML content),
  // benchmarked on a Core i7 notebook and compiled as Win32 platform:
  // ! AES128 - ECB:79.33ms CBC:83.37ms CFB:80.75ms OFB:78.98ms CTR:80.45ms
  // ! AES192 - ECB:91.16ms CBC:96.06ms CFB:96.45ms OFB:92.12ms CTR:93.38ms
  // ! AES256 - ECB:103.22ms CBC:119.14ms CFB:111.59ms OFB:107.00ms CTR:110.13ms
  // - timing of the same process, using CryptoApi official PROV_RSA_AES provider:
  // ! AES128 - ECB_API:102.88ms CBC_API:124.91ms
  // ! AES192 - ECB_API:115.75ms CBC_API:129.95ms
  // ! AES256 - ECB_API:139.50ms CBC_API:154.02ms
  // - but the CryptoApi does not supports AES-NI, whereas our classes handle it,
  // with a huge speed benefit
  // - under Win64, the official CryptoApi is slower our x86_64 asm version,
  // and the Win32 version of CryptoApi itself, but slower than our AES-NI code
  // ! AES128 - ECB:107.95ms CBC:112.65ms CFB:109.62ms OFB:107.23ms CTR:109.42ms
  // ! AES192 - ECB:130.30ms CBC:133.04ms CFB:128.78ms OFB:127.25ms CTR:130.22ms
  // ! AES256 - ECB:145.33ms CBC:147.01ms CFB:148.36ms OFB:145.96ms CTR:149.67ms
  // ! AES128 - ECB_API:89.64ms CBC_API:100.84ms
  // ! AES192 - ECB_API:99.05ms CBC_API:105.85ms
  // ! AES256 - ECB_API:107.11ms CBC_API:118.04ms
  // - in practice, you could forget about using the CryptoApi, unless you are
  // required to do so, for legal/corporate reasons
  TAesAbstractApi = class(TAesAbstract)
  protected
    fKeyHeader: packed record
      bType: byte;
      bVersion: byte;
      reserved: word;
      aiKeyAlg: cardinal;
      dwKeyLength: cardinal;
    end;
    fKeyHeaderKey: TAesKey; // should be just after fKeyHeader record
    fKeyCryptoApi: pointer;
    fInternalMode: cardinal;
    procedure AfterCreate; override;
    procedure InternalSetMode; virtual; abstract;
    procedure EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
      DoEncrypt: boolean);
  public
    /// release the AES execution context
    destructor Destroy; override;
    /// perform the AES cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAesAbstractSyn classes: you should better use PKC7 padding instead
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAesAbstractSyn classes: you should better use PKC7 padding instead
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher without chaining (ECB) using Windows CryptoApi
  TAesEcbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_ECB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher-block chaining (CBC) using Windows CryptoApi
  TAesCbcApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_CBC
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher feedback (CFB) using Windows CryptoApi
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not return
  // expected values for CFB
  TAesCfbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_CFB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Output feedback (OFB) using Windows CryptoApi
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not implement
  // this mode, and returns a NTE_BAD_ALGID error
  TAesOfbApi = class(TAesAbstractApi)
  protected
    /// will set fInternalMode := CRYPT_MODE_OFB
    procedure InternalSetMode; override;
  end;

{$endif USE_PROV_RSA_AES}


{ ***************** High-Level Windows Certificate Store Integration }

type
  ECryptCertCng = class(ECryptCert);

  /// Certificate interface with specific Windows CNG information
  // - inherits all the regular ICryptCert X.509 methods
  // - the certificate is represented internally by TX509
  // - its private key remains in the Windows CNG Key Storage Provider
  ICryptCertCng = interface(ICryptCert)
    /// change the asymmetric signing algorithm used with this certificate
    // - regular RSA keys may use RSxxx or PSxxx
    // - RSA-PSS restricted keys only accept PSxxx
    // - ECC keys only accept their matching curve
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    /// allow or forbid user interface from the underlying CNG provider
    // - false by default, allowing e.g. a SmartCard PIN dialog
    // - set true for services and other non-interactive processes
    procedure SetSilent(Value: boolean);
    /// return true if KSP user interface has been disabled
    function Silent: boolean;
    /// the Windows system certificate store containing this certificate
    function SystemStore: TSystemCertificateStore;
    /// the Windows certificate store location containing this certificate
    function StoreLocation: TWinCertStoreLocation;
    /// the CNG Key Storage Provider name associated with this certificate
    function KeyProvider: RawUtf8;
    /// the CNG private key/container name associated with this certificate
    function KeyContainer: RawUtf8;
    /// access the retained Windows certificate context
    // - caller should never free this context
    // - it remains valid while this ICryptCertCng instance is alive
    function WinContext: PCCERT_CONTEXT;
  end;

  /// store several Windows CNG Certificate interface instances
  ICryptCertCngs = array of ICryptCertCng;

  TCryptCertCng = class;

  /// enumerate CNG-backed certificates from Windows certificate stores
  // - defaults to CurrentUser\MY and LocalMachine\MY
  // - only certificates associated with a CNG KSP are exposed
  // - use Cert(), Find() or FindOne() to retrieve ICryptCertCng instances
  TCryptCertAlgoCng = class(TCryptCertAlgo)
  protected
    fLog: TSynLogClass;
    fCertStore: TSystemCertificateStore;
    fLocations: TWinCertStoreLocations;
    fCert: ICryptCertCngs;
    procedure LoadStore(Location: TWinCertStoreLocation);
  public
    /// enumerate certificates from the supplied Windows system store
    constructor Create(aCertStore: TSystemCertificateStore = scsMY;
      aLocations: TWinCertStoreLocations = [wcslCurrentUser, wcslLocalMachine];
      aLog: TSynLogClass = nil); reintroduce;
    /// refresh the list of available CNG-backed certificates
    // - existing ICryptCertCng references remain valid because each instance
    // owns a duplicated PCCERT_CONTEXT
    procedure Refresh;
    /// search the internal certificate list for a given attribute
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCertCngs;
    /// return the first certificate matching a given attribute
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber): ICryptCertCng;
    /// access all currently recognized Windows CNG certificates
    function Cert: ICryptCertCngs;
      {$ifdef HASINLINE} inline; {$endif}
    /// logging class used by this provider
    property Log: TSynLogClass
      read fLog;
  published
    /// logical Windows system certificate store
    property CertificateStore: TSystemCertificateStore
      read fCertStore;
    /// Windows certificate store locations being enumerated
    property Locations: TWinCertStoreLocations
      read fLocations;
  end;

  /// ICryptCert implementation backed by a Windows CNG private key
  // - TCryptCertX509Abstract parent class will manage the X.509 certificates
  // - only the private key is managed by this class using the CNG API
  TCryptCertCng = class(TCryptCertX509Abstract, ICryptCertCng)
  protected
    fContext: PCCERT_CONTEXT;
    fCertStore: TSystemCertificateStore;
    fStoreLocation: TWinCertStoreLocation;
    fKeyOptions: TWinCertCngKeyOptions;
    fCaa: TCryptAsymAlgo;
    fKeyProvider: RawUtf8;
    fKeyContainer: RawUtf8;
    procedure RaiseError(const Msg: ShortString); overload; override;
  public
    /// create a certificate from a context currently enumerated in a store
    // - duplicates aContext so this instance owns its certificate context
    constructor Create(aOwner: TCryptCertAlgoCng;
      aContext: PCCERT_CONTEXT; aLocation: TWinCertStoreLocation;
      const aInfo: TWinCertKeyProviderInfo); reintroduce;
    /// clear both the TX509 representation and retained Windows context
    procedure Clear; override;
    /// return the logging class from the associated CNG catalog
    function Log: TSynLogClass;
      {$ifdef HASINLINE} inline; {$endif}
    // ICryptCert methods
    function AsymAlgo: TCryptAsymAlgo; override;
    function CertAlgo: TCryptCertAlgo; override;
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; override;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; override;
    function Save(Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; override;
    function SetPrivateKey(const saved: RawByteString): boolean; override;
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage): RawByteString; override;
    procedure Sign(const Authority: ICryptCert); override;
    // ICryptCertCng methods
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    procedure SetSilent(Value: boolean);
    function Silent: boolean;
    function SystemStore: TSystemCertificateStore;
    function StoreLocation: TWinCertStoreLocation;
    function KeyProvider: RawUtf8;
    function KeyContainer: RawUtf8;
    function WinContext: PCCERT_CONTEXT;
  end;

const
  /// text to identify the location of a Windows system certificate store
  // - used mainly for logging purpose
  WINCNG_LOCATION_TEXT: array[TWinCertStoreLocation] of TShort15 = (
    'CurrentUser',
    'LocalMachine');


{ ***************** Windows CNG Private Key Integration }

type
  /// non-exportable private key redirecting operations to Windows CNG
  TCryptPrivateKeyCng = class(TCryptPrivateKey)
  protected
    fCert: TCryptCertCng;
    function FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
      pub: TCryptPublicKey): boolean; override;
    function SignDigest(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TCryptAsymAlgo): RawByteString; override;
  public
    /// initialize this adapter for its owning certificate
    constructor Create(aCert: TCryptCertCng); reintroduce;
    /// unsupported: key generation belongs to Windows CNG
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString; override;
    /// returns '' because the private key is not exportable through this class
    function ToDer: RawByteString; override;
    /// return the public key stored in the associated X.509 certificate
    function ToSubjectPublicKey: RawByteString; override;
    /// private key decryption is not implemented yet
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
  end;

/// small internal conversion function between algorithms types enumerates
function CngSignParams(Algo: TCryptAsymAlgo;
  out Hash: TNcryptHashAlgo; out Mode: TNcryptSignMode): boolean;



implementation


{ ***************** AES cypher/uncypher using PROV_RSA_AES CryptoApi }

{$ifdef USE_PROV_RSA_AES}

var
  CryptoApiAesProvider: HCRYPTPROV = HCRYPTPROV_NOTTESTED;

procedure EnsureCryptoApiAesProviderAvailable;
begin
  if CryptoApiAesProvider = nil then
    ESynCrypto.RaiseU('PROV_RSA_AES provider not installed')
  else if CryptoApiAesProvider = HCRYPTPROV_NOTTESTED then
  begin
    CryptoApiAesProvider := nil;
    if CryptoApi.Available then
    begin
      if not CryptoApi.AcquireContextA(CryptoApiAesProvider, nil, nil,
              PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
        if (HRESULT(GetLastError) <> NTE_BAD_KEYSET) or
           not CryptoApi.AcquireContextA(CryptoApiAesProvider, nil, nil,
             PROV_RSA_AES, CRYPT_NEWKEYSET) then
          ESynCrypto.RaiseLastOSError('in AcquireContext', []);
    end;
  end;
end;

procedure XorMemoryTrailer(Dest, Source1, Source2: PByteArray; Size: PtrUInt);
  {$ifdef HASINLINE}inline;{$endif}
begin // just XOR 0..15 of bytes
  while Size <> 0 do
  begin
    dec(Size);
    Dest[Size] := Source1[Size] xor Source2[Size];
  end;
end;


{ TAesAbstractApi }

procedure TAesAbstractApi.AfterCreate;
begin
  EnsureCryptoApiAesProviderAvailable;
  InternalSetMode;
  fKeyHeader.bType := PLAINTEXTKEYBLOB;
  fKeyHeader.bVersion := CUR_BLOB_VERSION;
  case fKeySize of
    128:
      fKeyHeader.aiKeyAlg := CALG_AES_128;
    192:
      fKeyHeader.aiKeyAlg := CALG_AES_192;
    256:
      fKeyHeader.aiKeyAlg := CALG_AES_256;
  end;
  fKeyHeader.dwKeyLength := fKeySizeBytes;
  fKeyHeaderKey := fKey;
end;

destructor TAesAbstractApi.Destroy;
begin
  if fKeyCryptoApi <> nil then
    CryptoApi.DestroyKey(fKeyCryptoApi);
  FillCharFast(fKeyHeaderKey, SizeOf(fKeyHeaderKey), 0);
  inherited;
end;

procedure TAesAbstractApi.EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
  DoEncrypt: boolean);
var
  n: cardinal;
begin
  if Count = 0 then
    exit; // nothing to do
  if fKeyCryptoApi <> nil then
  begin
    CryptoApi.DestroyKey(fKeyCryptoApi);
    fKeyCryptoApi := nil;
  end;
  if not CryptoApi.ImportKey(CryptoApiAesProvider, @fKeyHeader,
     SizeOf(fKeyHeader) + fKeySizeBytes, nil, 0, fKeyCryptoApi) then
    ESynCrypto.RaiseLastOSError('in CryptImportKey for %', [self]);
  if not CryptoApi.SetKeyParam(fKeyCryptoApi, KP_IV, @fIV, 0) then
    ESynCrypto.RaiseLastOSError('in CryptSetKeyParam(KP_IV) for %', [self]);
  if not CryptoApi.SetKeyParam(fKeyCryptoApi, KP_MODE, @fInternalMode, 0) then
    ESynCrypto.RaiseLastOSError('in CryptSetKeyParam(KP_MODE,%) for %',
       [fInternalMode, self]);
  if BufOut <> BufIn then
    MoveFast(BufIn^, BufOut^, Count);
  n := Count and not AesBlockMod;
  if DoEncrypt then
  begin
    if not CryptoApi.Encrypt(fKeyCryptoApi, nil, false, 0, BufOut, n, Count) then
      ESynCrypto.RaiseLastOSError('in Encrypt() for %', [self]);
  end
  else if not CryptoApi.Decrypt(fKeyCryptoApi, nil, false, 0, BufOut, n) then
    ESynCrypto.RaiseLastOSError('in Decrypt() for %', [self]);
  dec(Count, n);
  if Count > 0 then // remaining bytes will be XORed with the supplied IV
    XorMemoryTrailer(@PByteArray(BufOut)[n], @PByteArray(BufIn)[n], @fIV, Count);
end;

procedure TAesAbstractApi.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn, BufOut, Count, true);
end;

procedure TAesAbstractApi.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn, BufOut, Count, false);
end;


{ TAesEcbApi }

procedure TAesEcbApi.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_ECB;
  fAlgoMode := mEcb;
end;


{ TAesCbcApi }

procedure TAesCbcApi.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_CBC;
  fAlgoMode := mCbc;
end;


{ TAesCfbApi }

procedure TAesCfbApi.InternalSetMode;
begin
  ESynCrypto.RaiseUtf8('%: CRYPT_MODE_CFB is not compliant', [self]);
  fInternalMode := CRYPT_MODE_CFB;
  fAlgoMode := mCfb;
end;


{ TAesOfbApi }

procedure TAesOfbApi.InternalSetMode;
begin
  ESynCrypto.RaiseUtf8('%: CRYPT_MODE_OFB not implemented by PROV_RSA_AES', [self]);
  fInternalMode := CRYPT_MODE_OFB;
  fAlgoMode := mOfb;
end;

{$endif USE_PROV_RSA_AES}


{ ***************** Windows CNG Private Key Integration }

function CngSignParams(Algo: TCryptAsymAlgo;
  out Hash: TNcryptHashAlgo; out Mode: TNcryptSignMode): boolean;
begin // seldom called: function is easier than array[TCryptAsymAlgo] constants
  result := false;
  case CAA_HF[Algo] of
    hfSHA256:
      Hash := nhaSha256;
    hfSHA384:
      Hash := nhaSha384;
    hfSHA512:
      Hash := nhaSha512;
  else
    exit;
  end;
  case Algo of
    caaRS256 .. caaRS512:
      Mode := nsmRsaPkcs1;
    caaPS256 .. caaPS512:
      Mode := nsmRsaPss;
    caaES256,
    caaES384,
    caaES512:
      Mode := nsmEcdsa;
  else
    exit;
  end;
  result := true;
end;


{ TCryptPrivateKeyCng }

constructor TCryptPrivateKeyCng.Create(aCert: TCryptCertCng);
begin
  inherited Create;
  fCert := aCert;
  if (aCert <> nil) and
     (aCert.fX509 <> nil) then
    fKeyAlgo := XKA_TO_CKA[aCert.fX509.Signed.SubjectPublicKeyAlgorithm];
end;

function TCryptPrivateKeyCng.FromDer(algo: TCryptKeyAlgo;
  const der: RawByteString; pub: TCryptPublicKey): boolean;
begin
  result := false; // private key stays in the Windows CNG provider
end;

function TCryptPrivateKeyCng.SignDigest(const Dig: THash512Rec;
  DigLen: integer; DigAlgo: TCryptAsymAlgo): RawByteString;
var
  hash: TNcryptHashAlgo;
  mode: TNcryptSignMode;
  err: cardinal;
  key: TWinCertCngKey; // safe short-lived CNG private key access
  log: ISynLog;
begin
  FastAssignNew(result);
  if (fCert = nil) or
     (fCert.fX509 = nil) or
     (DigAlgo <> fCert.fCaa) or
     (HASH_SIZE[CAA_HF[DigAlgo]] <> DigLen) or
     not CngSignParams(DigAlgo, hash, mode) then
    exit;
  fCert.Log.EnterLocal(log,
    'SignDigest % %', [ToText(DigAlgo)^, fCert], self);
  err := key.Init(fCert.fContext, fCert.fKeyOptions);
  if err <> NO_ERROR then
    log.Log(sllTrace,
      'SignDigest: CryptAcquireCertificatePrivateKey failed %',
      [OsErrorShort(err)], self)
  else
  try
    try
      // TNCrypt.KeySign() avoids an extra size-query signing operation,
      // which is important for smartcards and interactive hardware KSPs
      result := NCrypt.KeySign(key.Handle, @Dig.b, DigLen,
        hash, mode, {PssSaltLen=}0, wckSilent in fCert.fKeyOptions);
      if (mode = nsmEcdsa) and
         (result <> '') then
        // CNG returns fixed-width r || s whereas ICryptCert expects DER
        result := SetSignatureSecurityRaw(DigAlgo, RawUtf8(result));
      log.Log(sllTrace,
        'SignDigest: returns len=%', [length(result)], self);
    except
      on E: Exception do
      begin
        log.Log(sllTrace, 'SignDigest failed due to %', [E], self);
        FastAssignNew(result);
      end;
    end;
  finally
    key.Done; // release the private key handle ASAP for safety
  end;
end;

function TCryptPrivateKeyCng.Generate(
  Algorithm: TCryptAsymAlgo): RawByteString;
begin
  FastAssignNew(result); // key generation belongs to Windows CNG
end;

function TCryptPrivateKeyCng.ToDer: RawByteString;
begin
  FastAssignNew(result); // private key stays in the Windows CNG provider
end;

function TCryptPrivateKeyCng.ToSubjectPublicKey: RawByteString;
begin
  if (fCert = nil) or
     (fCert.fX509 = nil) then
    FastAssignNew(result)
  else
    result := fCert.fX509.Signed.SubjectPublicKey;
end;

function TCryptPrivateKeyCng.Open(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  FastAssignNew(result); // NCryptDecrypt support will be added later
end;


{ ***************** High-Level Windows Certificate Store Integration }

{ TCryptCertAlgoCng }

constructor TCryptCertAlgoCng.Create(aCertStore: TSystemCertificateStore;
  aLocations: TWinCertStoreLocations; aLog: TSynLogClass);
begin
  if aLog = nil then
    aLog := TSynLog;
  fLog := aLog;
  fCertStore := aCertStore;
  fLocations := aLocations;
  Refresh;
end;

procedure TCryptCertAlgoCng.LoadStore(Location: TWinCertStoreLocation);
var
  store: TWinCertStore;
  cert: ICryptCertCng;
  n: PtrInt;
  info: TWinCertKeyProviderInfo;
  log: ISynLog;
begin
  fLog.EnterLocal(log, 'LoadStore %', [WINCNG_LOCATION_TEXT[Location]], self);
  store := TWinCertStore.Create(fCertStore, Location);
  try
    if store.Handle = nil then
      log.Log(sllLastError, 'LoadStore: CertOpenStore failed', self)
    else
      while store.Next do
      begin
        if not WinCertCtxtKeyProvider(store.Context, info) or
           (info.ProviderType <> 0) then
          continue; // no private key or a legacy CryptoAPI CSP
        try
          cert := TCryptCertCng.Create(self, store.Context, Location, info);
          n := length(fCert);
          SetLength(fCert, n + 1);
          fCert[n] := cert;
        except
          on E: Exception do
            log.Log(sllTrace,
              'LoadStore: ignored certificate due to %', [E], self);
        end;
      end;
  finally
    store.Free;
  end;
end;

procedure TCryptCertAlgoCng.Refresh;
var
  wcsl: TWinCertStoreLocation;
begin
  fCert := nil; // clear any previous certificates
  if not NCrypt.Exists then
  begin
    fLog.Add.Log(sllWarning,
      'Refresh: Windows CNG is not available on %', [OSVersionShort], self);
    exit;
  end;
  for wcsl := low(wcsl) to high(wcsl) do
    if wcsl in fLocations then
      LoadStore(wcsl);
  fLog.Add.Log(sllDebug,
    'Refresh: loaded % CNG certificate(s)', [length(fCert)], self);
end;

function TCryptCertAlgoCng.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCertCngs;
begin
  result := nil;
  if fCert <> nil then
    TCryptCertCng.InternalFind(pointer(fCert), Value, Method, length(fCert),
      MaxCount, ICryptCerts(result));
end;

function TCryptCertAlgoCng.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCertCng;
var
  found: ICryptCertCngs;
begin
  found := Find(Value, Method, 1);
  if found = nil then
    result := nil
  else
    result := found[0];
end;

function TCryptCertAlgoCng.Cert: ICryptCertCngs;
begin
  result := fCert;
end;


{ TCryptCertCng }

constructor TCryptCertCng.Create(aOwner: TCryptCertAlgoCng;
  aContext: PCCERT_CONTEXT; aLocation: TWinCertStoreLocation;
  const aInfo: TWinCertKeyProviderInfo);
var
  der: RawByteString;
  xka: TXPublicKeyAlgorithm;
begin
  if (aOwner = nil) or
     (aContext = nil) then
    ECryptCertCng.RaiseU('TCryptCertCng.Create: invalid owner/context');
  fKeyOptions := [wckCompareKey];
  inherited Create;
  try
    fContext := CertDuplicateCertificateContext(aContext);
    if fContext = nil then
      RaiseError('Create: CertDuplicateCertificateContext failed');
    FastSetRawByteString(der, fContext^.pbCertEncoded, fContext^.cbCertEncoded);
    fX509 := TX509.Create;
    if not fX509.LoadFromDer(der) then
      RaiseError('Create: invalid X.509 certificate');
    xka := fX509.Signed.SubjectPublicKeyAlgorithm;
    if not (xka in [xkaRsa, xkaRsaPss, xkaEcc256, xkaEcc384, xkaEcc512]) then
      RaiseError('Create: unsupported public key algorithm %',
        [ToText(xka)^]);
    if aInfo.ProviderType <> 0 then
      RaiseError('Create: certificate is not backed by a CNG KSP');
    fCryptAlgo := aOwner;
    fCertStore := aOwner.fCertStore;
    fStoreLocation := aLocation;
    fKeyProvider := aInfo.Provider;
    fKeyContainer := aInfo.Container;
    // XKA_TO_CAA defaults RSA/RSA-PSS to SHA-256, as with PKCS#11
    fCaa := XKA_TO_CAA[xka];
    // safe access of its own private key using the Windows CNG API
    fPrivateKey := TCryptPrivateKeyCng.Create(self);
  except
    Clear;
    raise;
  end;
end;

procedure TCryptCertCng.Clear;
begin
  if fContext <> nil then
  begin
    CertFreeCertificateContext(fContext);
    fContext := nil;
  end;
  inherited Clear; // release fPrivateKey and fX509 instances
end;

procedure TCryptCertCng.RaiseError(const Msg: ShortString);
begin
  ECryptCertCng.RaiseUtf8('% (provider=% key=%) %',
    [self, fKeyProvider, fKeyContainer, Msg]);
end;

function TCryptCertCng.Log: TSynLogClass;
begin
  if fCryptAlgo = nil then
    result := TSynLog
  else
    result := TCryptCertAlgoCng(fCryptAlgo).fLog;
end;

// ICryptCert methods

function TCryptCertCng.AsymAlgo: TCryptAsymAlgo;
begin
  result := fCaa;
end;

function TCryptCertCng.CertAlgo: TCryptCertAlgo;
begin
  // certificate parsing/verification still uses the regular TX509 engine
  result := CryptCertX509[fCaa];
end;

function TCryptCertCng.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
begin
  result := nil; // certificate/key generation belongs to Windows
end;

function TCryptCertCng.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
begin
  result := false; // identities are discovered from Windows stores
end;

function TCryptCertCng.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
begin
  FastAssignNew(result);
  if not (Format in [ccfBinary, ccfPem]) then
    // hexa/base64 variants are handled by TCryptCert
    result := inherited Save(Content, PrivatePassword, Format)
  else
    case Content of
      cccCertOnly:
        if fX509 <> nil then
        begin
          result := fX509.SaveToDer;
          if Format = ccfPem then
            result := DerToPem(result, pemCertificate);
        end;
    else
      RaiseError(
        'Save: only cccCertOnly is supported for a Windows CNG key');
    end;
end;

function TCryptCertCng.SetPrivateKey(
  const saved: RawByteString): boolean;
begin
  result := false; // private material stays in Windows CNG
end;

function TCryptCertCng.Sign(Data: pointer; Len: integer;
  Usage: TCryptCertUsage): RawByteString;
begin
  if HasPrivateSecret and
     (fX509 <> nil) and
     (Usage in fX509.Usages) then
    result := fPrivateKey.Sign(fCaa, Data, Len)
  else
    FastAssignNew(result);
end;

procedure TCryptCertCng.Sign(const Authority: ICryptCert);
begin
  RaiseError(
    'Sign(Authority) is not supported - use TCryptCertX509 instead');
end;

// ICryptCertCng methods

procedure TCryptCertCng.SetAsymAlgo(caa: TCryptAsymAlgo);
var
  xka: TXPublicKeyAlgorithm;
begin
  if caa = fCaa then
    exit;
  if fX509 = nil then
    RaiseError('SetAsymAlgo: no X.509 certificate');
  xka := fX509.Signed.SubjectPublicKeyAlgorithm;
  case xka of
    xkaRsa:
      // an unrestricted RSA CNG key may sign with PKCS#1 or PSS
      if caa in CAA_RSA then
      begin
        fCaa := caa;
        exit;
      end;
    xkaRsaPss:
      // an RSA-PSS SubjectPublicKeyInfo remains PSS-restricted
      if caa in [caaPS256 .. caaPS512] then
      begin
        fCaa := caa;
        exit;
      end;
  else
    // ECC curves should remain exactly compatible with their key algorithm
    if CAA_CKA[fCaa] = CAA_CKA[caa] then
    begin
      fCaa := caa;
      exit;
    end;
  end;
  RaiseError('SetAsymAlgo(%): incompatible with the % public key',
    [ToText(caa)^, ToText(xka)^]);
end;

procedure TCryptCertCng.SetSilent(Value: boolean);
begin
  if Value then
    include(fKeyOptions, wckSilent)
  else
    exclude(fKeyOptions, wckSilent);
end;

function TCryptCertCng.Silent: boolean;
begin
  result := wckSilent in fKeyOptions;
end;

function TCryptCertCng.SystemStore: TSystemCertificateStore;
begin
  result := fCertStore;
end;

function TCryptCertCng.StoreLocation: TWinCertStoreLocation;
begin
  result := fStoreLocation;
end;

function TCryptCertCng.KeyProvider: RawUtf8;
begin
  result := fKeyProvider;
end;

function TCryptCertCng.KeyContainer: RawUtf8;
begin
  result := fKeyContainer;
end;

function TCryptCertCng.WinContext: PCCERT_CONTEXT;
begin
  result := fContext;
end;



procedure InitializeUnit;
begin
end;

procedure FinalizeUnit;
begin
  {$ifdef USE_PROV_RSA_AES}
  if (CryptoApiAesProvider <> nil) and
     (CryptoApiAesProvider <> HCRYPTPROV_NOTTESTED) then
    CryptoApi.ReleaseContext(CryptoApiAesProvider, 0);
  {$endif USE_PROV_RSA_AES}
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

{$else}
implementation // do-nothing unit on POSIX
{$endif OSWINDOWS}

end.
