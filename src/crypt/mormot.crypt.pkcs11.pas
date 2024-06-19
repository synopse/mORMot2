/// Framework Core High-Level HSM access via PKCS#11
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.pkcs11;

{
  *****************************************************************************

   Access Hardware Security Modules (HSM) via PKCS#11  
    - High-Level PKCS#11 Integration with the Framework Types
    - Registration of the PKCS#11 Engine to the TCryptAsym/TCryptCert Factories

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.ecc256r1,
  mormot.crypt.ecc,
  mormot.crypt.rsa,
  mormot.crypt.x509,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.threads,
  mormot.lib.pkcs11;



{ ***************** High-Level PKCS#11 Integration with the Framework Types }

/// fill a PKCS#11 Mechanism structure with the parameters for a given algorithm
procedure Pkcs11SetMechanism(Algo: TCryptAsymAlgo; out Mech: CK_MECHANISM);

/// guess the TX509.SubjectPublicKeyAlgorithm of a given PKCS#11 Object
// - supports only CKO_PUBLIC_KEY and CKO_PRIVATE_KEY kind of objects
// - CKO_CERTIFICATE should be parsed and inspected directly
function Pkcs11KeyAlgorithm(const obj: TPkcs11Object): TXPublicKeyAlgorithm;

/// guess the TCryptCert usages from raw PKCS#11 Object storage flags
function Pkcs11FlagsToCertUsages(pos: TPkcs11ObjectStorages): TCryptCertUsages;

/// guess the raw PKCS#11 Object storage flags from TCryptCert usages
function CertUsagesToPkcs11Flags(cu: TCryptCertUsages;
  forpubkey: boolean): TPkcs11ObjectStorages;


{ ***************** Registration of the PKCS#11 Engine to our Factories }

type
  /// Certificate interface with specific PKCS#11 information
  // - inherit from ICryptCert so has all the main Certificates methods
  // - as implemented by TCryptCertPkcs11 instances
  // - values are retrieved in the constructor - may have changed on HW
  ICryptCertPkcs11 = interface(ICryptCert)
    /// change the high-level asymmetric algorithm used for this certificate
    // - by default, it is guessed with 256-bit RSA hash via XKA_TO_CAA[] lookup
    // - you can refine the expected hash algorithm used for Verify/Sign
    // - by design, any key algorithm switch (e.g. from RSA to ECC) will fail
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    /// set the PIN code to be used to access the CKO_PRIVATE_KEY object
    // - same as Load('', cccPrivateKeyOnly, PinCode)
    // - return true if the PIN code is correct
    // - warning: a wrong PinCode may lock your HW device for any further use
    function SetPin(const PinCode: SpiUtf8): boolean;
    /// true if was filled from a true CKO_CERTIFICATE object
    // - false if is a "fake" X.509 certificate, created from a CKO_PUBLIC_KEY
    function IsX509: boolean;
    /// the associated PKCS#11 library instance
    function Engine: TPkcs11;
    /// the associated Slot ID in the PKCS#11 instance Engine
    function SlotID: TPkcs11SlotID;
    /// the hexadecimal CKA_ID of the associated token in the PKCS#11 instance
    // - a single TPkcs11Object.StorageID usually appear in the objects list
    // to identify a public key (or full X.509 certificate), but is used by
    // TCryptCertPkcs11.OpenPrivateKey to unlock the associated CKO_PRIVATE_KEY
    function StorageID: TPkcs11ObjectID;
    /// the associated Slot ID and hexa CKA_ID in the PKCS#11 instance Engine
    // - i.e. 'SlotID-StorageID' text
    function Storage: RawUtf8;
    /// the associated label, as shared by all objects of this StorageID
    function StorageLabel: RawUtf8;
    /// the associated Token Name in the PKCS#11 instance Engine
    function TokenName: RawUtf8;
    /// the associated slot in the PKCS#11 instance Engine
    function Slot: TPkcs11Slot;
    /// the associated token in the PKCS#11 instance Engine
    function Token: TPkcs11Token;
  end;

  /// store several Certificate interfaces with specific PKCS#11 information
  ICryptCertPkcs11s = array of ICryptCertPkcs11;

  /// class loading a PKCS#11 library in the context of our high-level
  // cryptographic catalog
  // - it is the main factory for ICryptCert support of a PKCS#11 library
  TCryptCertAlgoPkcs11 = class(TCryptCertAlgo)
  protected
    fEngine: TPkcs11;
    fLog: TSynLogClass;
    fConfigRetrieved: boolean;
    fCert: ICryptCertPkcs11s;
    fLibraryName: TFileName;
    fLoadingError: string;
    procedure BackgroundLoad(Sender: TObject);
    procedure EnsureRetrieveConfig;
    procedure CryptCertToPkcs11PrivKeyAttributes(const Cert: ICryptCert;
     const StoreLabel: RawUtf8; const PrivKeyDer, BinaryID: RawByteString;
     var Attr: CK_ATTRIBUTES);
  public
    /// load a PKCS#11 library and asynchronously retrieve its configuration
    // - Engine.Load() and RetrieveConfig() will happen in a background thread
    // - Cert method will wait if needed for the configuration to be loaded
    // - see LoadingError property for any error during the background process
    constructor Create(const aLibraryName: TFileName;
      aLog: TSynLogClass = nil); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// search the internal ICryptCertPkcs11 list for a given attribute
    // - return all the certificates matching a given value
    // - will use brute-force O(n) search algorithm with lockfree multi-read
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCertPkcs11s;
    /// search the internal list for a given attribute
    // - return the first certificate matching a given value
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber): ICryptCertPkcs11;
    /// search the internal list per ICryptCertPkcs11.StorageLabel value
    function FindByLabel(const Value: RawUtf8): ICryptCertPkcs11;
    /// search the internal list per ICryptCertPkcs11.StorageID value
    // - i.e. known public certificate matching hexadecimal CKA_ID
    function FindByID(const Value: TPkcs11ObjectID): ICryptCertPkcs11;
    /// store a ICryptCert instance with its private key into the token
    // - call CreateObject() with the certificate and its associated private key
    function Import(const CertWithPrivKey: ICryptCert; Slot: TPkcs11SlotID;
      const ID: RawUtf8; const SoPinCode: SpiUtf8): ICryptCertPkcs11;
    // TCryptCertAlgo methods are mostly unsupported
    function New: ICryptCert; override;
    function FromHandle(Handle: pointer): ICryptCert; override;
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8; override;
    /// access to the high-level certificates recognized in this PKCS#11 instance
    // - will wait if background loading of information is not finished
    function Cert: ICryptCertPkcs11s;
      {$ifdef HASINLINE} inline; {$endif}
    /// the associated PKCS#11 library instance
    property Engine: TPkcs11
      read fEngine;
    /// internal flag used to notify the end of background loading
    property LoadingConfigRetrieved: boolean
      read fConfigRetrieved;
    /// the exception message if Create() failed to retrieve the PKCS#11 info
    property LoadingError: string
      read fLoadingError;
  end;

  /// class implementing ICryptCert using PCKS#11
  // - CKO_CERTIFICATES will be directly retrieved from their X.509 DER binary
  // - CKO_PUBLIC_KEY will create fake X.509 certificate from their raw binary
  // - CKO_PRIVATE_KEY will be used when needed, using a PIN code specified as
  // PrivatePassword in SetPin or Load(cccPrivateKeyOnly) - safely stored
  // encrypted in memory for Engine.Open() delayed call
  TCryptCertPkcs11 = class(TCryptCertX509Abstract, ICryptCertPkcs11)
  protected
    fEngine: TPkcs11;
    fStorageID: TPkcs11ObjectID;  // match TPkcs11Object.StorageID
    fStorageLabel: RawUtf8;       // match TPkcs11Object.StorageLabel
    fLocation: RawUtf8;
    fSecret, fPin: RawByteString; // anti-forensic PIN storage
    fSlotID: TPkcs11SlotID;
    fIsX509: boolean;
    fCaa: TCryptAsymAlgo;
    fSlot: TPkcs11Slot;
    fToken: TPkcs11Token;
    procedure RaiseError(const Msg: shortstring); overload; override;
    // if true then caller should make try ... finally fEngine.Close
    function OpenPrivateKey: CK_OBJECT_HANDLE;
  public
    /// create a X.509 from the supplied information
    // - should supply all aObjects[] and aValues[] on this SlotID and
    // a given CKA_ID to filter
    // - if no session is currently opened, no CKO_PRIVATE_KEY may be available:
    // call later SetPin() or Load('', cccPrivateKeyOnly, PIN)
    constructor Create(aOwner: TCryptCertAlgoPkcs11; aSlotID: TPkcs11SlotID;
      const aObjects: TPkcs11ObjectDynArray; const aValues: TRawByteStringDynArray;
      const aStorageID: TPkcs11ObjectID); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
    /// return the TSynLogClass from the associated TCryptCertAlgoPkcs11
    function Log: TSynLogClass;
      {$ifdef HASINLINE} inline; {$endif}
    // ICryptCert methods
    function AsymAlgo: TCryptAsymAlgo; override;
    function CertAlgo: TCryptCertAlgo; override;
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; override;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; override; // PIN cccPrivateKeyOnly
    function Save(Content: TCryptCertContent; const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; override;
    function SetPrivateKey(const saved: RawByteString): boolean; override;
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage): RawByteString; override;
    procedure Sign(const Authority: ICryptCert); override;
    // ICryptCertPkcs11 methods
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    function SetPin(const PinCode: SpiUtf8): boolean;
    function IsX509: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function Engine: TPkcs11;
      {$ifdef HASINLINE} inline; {$endif}
    function StorageID: TPkcs11ObjectID;
      {$ifdef HASINLINE} inline; {$endif}
    function StorageLabel: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function Storage: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function SlotID: TPkcs11SlotID;
      {$ifdef HASINLINE} inline; {$endif}
    function TokenName: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function Slot: TPkcs11Slot;
    function Token: TPkcs11Token;
  published
    /// redirect to ICryptCertPkcs11.Storage value, i.e. 'SlotID-StorageID'
    property Location: RawUtf8
      read fLocation;
    /// redirect to ICryptCertPkcs11.StorageLabel value
    property StoredLabel: RawUtf8
      read fStorageLabel;
  end;


implementation

{ ***************** High-Level PKCS#11 Integration with the Framework Types }

const
  /// the CK_MECHANISM for each high-level Framework algorithm
  // - the hash is made on SW side, before calling the PKCS#11 library
  CAA_TO_CKM: array[TCryptAsymAlgo] of CK_MECHANISM_TYPE = (
    CKM_ECDSA,        // caaES256
    CKM_ECDSA,        // caaES384
    CKM_ECDSA,        // caaES512
    CKM_ECDSA,        // caaES256K
    CKM_RSA_PKCS,     // caaRS256
    CKM_RSA_PKCS,     // caaRS384
    CKM_RSA_PKCS,     // caaRS512
    CKM_RSA_PKCS_PSS, // caaPS256
    CKM_RSA_PKCS_PSS, // caaPS384
    CKM_RSA_PKCS_PSS, // caaPS512
    CKM_EDDSA);       // caaEdDSA

  /// the DER encoding of an ANSI X9.62 OID to be supplied as CKA_EC_PARAMS
  // - see e.g. openssl ecparam -name prime256v1 -outform DER | hexdump -C
  // and CKA_OID[] from mormot.crypt.secure; see also as reference
  // https://github.com/OpenSC/OpenSC/blob/master/src/tools/pkcs11-tool.c#L108
  CAA_TO_ECPARAMS: array[TCryptAsymAlgo] of RawByteString = (
    RawByteString(#$06#$08#$2a#$86#$48#$ce#$3d#$03#$01#$07),
      // caaES256  '1.2.840.10045.3.1.7'
    RawByteString(#$06#$05#$2b#$81#$04#$00#$22), // caaES384  '1.3.132.0.34'
    RawByteString(#$06#$05#$2b#$81#$04#$00#$23), // caaES512  '1.3.132.0.35'
    RawByteString(#$06#$05#$2b#$81#$04#$00#$0a), // caaES256K '1.3.132.0.10'
    '',                        // caaRS256
    '',                        // caaRS384
    '',                        // caaRS512
    '',                        // caaPS256
    '',                        // caaPS384
    '',                        // caaPS512
    RawByteString(#$13#$0c#$65#$64#$77#$61#$72#$64#$73#$32#$35#$35#$31#$39));
      // caaEdDSA '1.3.6.1.4.1.11591.15.1' - but optional

  /// the CK_KEY_TYPE for each high-level Framework algorithm
  CAA_TO_CKK: array[TCryptAsymAlgo] of CK_KEY_TYPE = (
    CKK_EC,          // caaES256
    CKK_EC,          // caaES384
    CKK_EC,          // caaES512
    CKK_EC,          // caaES256K
    CKK_RSA,         // caaRS256
    CKK_RSA,         // caaRS384
    CKK_RSA,         // caaRS512
    CKK_RSA,         // caaPS256
    CKK_RSA,         // caaPS384
    CKK_RSA,         // caaPS512
    CKK_EC_EDWARDS); // caaEdDSA

procedure Pkcs11SetMechanism(Algo: TCryptAsymAlgo; out Mech: CK_MECHANISM);
begin
  Mech.mechanism := ToULONG(CAA_TO_CKM[Algo]);
  Mech.pParameter := nil;
  Mech.ulParameterLen := 0;
  // RSA details are set as CKA_MODULUS_BITS/CKA_PUBLIC_EXPONENT attributes
  // EC type is set as CKA_EC_PARAMS attribute
end;

function Pkcs11KeyAlgorithm(const obj: TPkcs11Object): TXPublicKeyAlgorithm;
begin
  result := xkaNone;
  if obj.ObjClass in [CKO_PUBLIC_KEY, CKO_PRIVATE_KEY] then
    case obj.KeyType of
      CKK_RSA:
        result := xkaRsa;
      CKK_EC:
        case obj.KeyBits of // wild guess for the most common curves
          256:
            result := xkaEcc256;
          384:
            result := xkaEcc384;
          512:
            result := xkaEcc512;
        end;
      CKK_EC_EDWARDS:
        result := xkaEdDSA;
    end;
end;

function Pkcs11FlagsToCertUsages(pos: TPkcs11ObjectStorages): TCryptCertUsages;
begin
  result := [];
  // compute the X.509 usage flags corresponding to the cryptoki attributes
  if [posEncrypt, posDecrypt] * pos <> [] then
    include(result, cuDataEncipherment);
  if posDerive in pos then
    include(result, cuKeyAgreement);
  if [posWrap, posUnWrap] * pos <> [] then
    include(result, cuKeyEncipherment);
  if posEncrypt in pos then
    include(result, cuEncipherOnly);
  if posDecrypt in pos then
    include(result, cuDecipherOnly);
  if [posSign, posVerify] * pos <> [] then
    result := result + [cuCrlSign, cuKeyCertSign, cuDigitalSignature,
                        cuNonRepudiation];
  // cuCodeSign, cuTlsServer and cuTlsClient are not included because they
  // require a full X.509 certificate with its issuer/authority fields for
  // proper PKI trust chain verification
end;

function CertUsagesToPkcs11Flags(cu: TCryptCertUsages;
  forpubkey: boolean): TPkcs11ObjectStorages;
begin
  result := [];
  if cuDataEncipherment in cu then
    if forpubkey then
      include(result, posEncrypt)
    else
      include(result, posDecrypt);
  if cuKeyAgreement in cu then
    if forpubkey then
      include(result, posWrap)
    else
      include(result, posUnWrap);
  if (cuEncipherOnly in cu) and
     forpubkey then
    include(result, posEncrypt);
  if (cuDecipherOnly in cu) and
     not forpubkey then
    include(result, posDecrypt);
  if cu * [cuCrlSign, cuKeyCertSign, cuDigitalSignature, cuNonRepudiation,
           cuCodeSign, cuTlsServer, cuTlsClient] <> [] then
    if forpubkey then
      include(result, posVerify)
    else
      include(result, posSign);
end;


{ ***************** Registration of the PKCS#11 Engine to our Factories }

type
  ECryptCertPkcs11 = class(ECryptCert);

  /// access a PKCS#11 private key in ICryptPrivateKey format
  TCryptPrivateKeyPkcs11 = class(TCryptPrivateKey)
  protected
    fCert: TCryptCertPkcs11;
    function FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
      pub: TCryptPublicKey): boolean; override;
    function SignDigest(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TCryptAsymAlgo): RawByteString; override;
  public
    /// initialize this instance
    constructor Create(aCert: TCryptCertPkcs11); reintroduce;
    /// create a new private / public key pair
    // - should return the associated public key binary in SubjectPublicKey format
    // - currently not implemented
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString; override;
    /// returns '' by definition since the private key stays in the device
    function ToDer: RawByteString; override;
    /// return the associated public key as stored in the associated TX509
    function ToSubjectPublicKey: RawByteString; override;
    /// decryption with this private key
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    /// compute the shared-secret with another public key
    function SharedSecret(const PeerKey: ICryptPublicKey): RawByteString;
      override;
  end;


{ TCryptPrivateKeyPkcs11 }

constructor TCryptPrivateKeyPkcs11.Create(aCert: TCryptCertPkcs11);
begin
  inherited Create;
  fCert := aCert;
end;

function TCryptPrivateKeyPkcs11.FromDer(algo: TCryptKeyAlgo;
  const der: RawByteString; pub: TCryptPublicKey): boolean;
begin
  result := false; // unsupported by definition (key stays in the device)
end;

function TCryptPrivateKeyPkcs11.SignDigest(const Dig: THash512Rec;
  DigLen: integer; DigAlgo: TCryptAsymAlgo): RawByteString;
var
  obj: CK_OBJECT_HANDLE;
  mech: CK_MECHANISM;
  hf: THashAlgo;
  seq: TAsnObject;
  log: ISynLog; // seldom called, and better be traced (and profiled)
begin
  log := fCert.Log.Enter('SignDigest % %', [ToText(DigAlgo)^, fCert], self);
  result := '';
  hf := CAA_HF[DigAlgo];
  if HASH_SIZE[hf] <> DigLen then
    exit; // paranoid
  obj := fCert.OpenPrivateKey;
  if obj <> CK_INVALID_HANDLE then
    try
      // see https://crypto.stackexchange.com/a/10103/40200
      Pkcs11SetMechanism(DigAlgo, mech);
      if fCert.fCaa in CAA_RSA then // CKM_RSA_PKCS or CKM_RSA_PKCS_PSS
        seq := RsaSignHashToDer(@Dig.b, hf)
      else
        FastSetRawByteString(seq, @Dig, DigLen); // CKM_ECDSA (to be validated)
      result := fCert.fEngine.Sign(pointer(seq), length(seq), obj, mech);
      case fCert.fCaa of
        caaES256:
          if length(result) = SizeOf(TEccSignature) then
            result := EccToDer(PEccSignature(result)^);
      end;
      log.Log(sllTrace, 'SignDigest: returns len=%', [length(result)], self);
    finally
      fCert.fEngine.Close;
    end;
end;

function TCryptPrivateKeyPkcs11.Generate(Algorithm: TCryptAsymAlgo): RawByteString;
begin
  result := ''; // to be implemented later on
end;

function TCryptPrivateKeyPkcs11.ToDer: RawByteString;
begin
  result := ''; // unsupported by definition (key stays in the device)
end;

function TCryptPrivateKeyPkcs11.ToSubjectPublicKey: RawByteString;
begin
  result := fCert.fX509.Signed.SubjectPublicKey; // from TX509 (fake) instance
end;

function TCryptPrivateKeyPkcs11.Open(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  result := ''; // to be implemented later on
end;

function TCryptPrivateKeyPkcs11.SharedSecret(
  const PeerKey: ICryptPublicKey): RawByteString;
begin
  result := ''; // to be implemented later on?
end;


{ TCryptCertAlgoPkcs11 }

constructor TCryptCertAlgoPkcs11.Create(const aLibraryName: TFileName;
  aLog: TSynLogClass);
begin
  if aLog = nil then
    aLog := TSynLog;
  fLog := aLog;
  with fLog.Enter('Create %', [aLibraryName], self) do
  begin
    fLibraryName := aLibraryName;
    fEngine := TPkcs11.Create; // the dll/so is loaded in BackgroundLoad
    TLoggedWorkThread.Create(fLog, 'BackgroundLoad', self, BackgroundLoad);
  end;
end;

destructor TCryptCertAlgoPkcs11.Destroy;
begin
  fEngine.Free;
  inherited Destroy;
end;

function CertStorageCompare(const A, B): integer;
var
  sa, sb: RawUtf8;
begin
  sa := ICryptCertPkcs11(A).Storage;
  sb := ICryptCertPkcs11(B).Storage;
  result := StrComp(pointer(sa), pointer(sb));
end;

procedure TCryptCertAlgoPkcs11.BackgroundLoad(Sender: TObject);
var
  i, j: PtrInt;
  val: TRawByteStringDynArray;
  obj: TPkcs11ObjectDynArray;
  ids: TPkcs11ObjectIDs;
  c: ICryptCertPkcs11;
begin
  try
    // this operation could take 10 seconds
    fEngine.Load(fLibraryName);
    fEngine.RetrieveConfig({includevoid=}false, {includmechs=}false);
    fLog.Add.Log(sllDebug, 'BackgroundLoad %', [fEngine], self);
    // generate all ICryptCertPkcs11 certificates from the retrieved information
    for i := 0 to high(fEngine.SlotIDs) do
    begin
      fEngine.Open(fEngine.SlotIDs[i]); // anynymous session for certs and pubkey
      try
        obj := fEngine.GetObjects(nil, nil, @val); // all objects
        ids := nil;
        for j := 0 to high(obj) do
          if obj[j].ObjClass in [CKO_CERTIFICATE, CKO_PUBLIC_KEY] then
            AddRawUtf8(ids, obj[j].StorageID, {nodup=}true);
        for j := 0 to high(ids) do
        begin
          c := TCryptCertPkcs11.Create(self, fEngine.SlotIDs[i], obj, val, ids[j]);
          InterfaceArrayAdd(fCert, c);
        end;
      finally
        fEngine.Close; // close session
      end;
    end;
    ObjArraySort(fCert, CertStorageCompare);
  except
    on E: Exception do
    begin
      fLog.Add.Log(sllTrace, 'BackgroundLoad: aborted due to %', [E], self);
      fLoadingError := E.Message;
    end;
  end;
  fConfigRetrieved := true;
end;

procedure TCryptCertAlgoPkcs11.EnsureRetrieveConfig;
var
  endtix: Int64;
begin // caller did "if not fConfigRetrieved then EnsureRetrieveConfig"
  endtix := GetTickCount64 + MilliSecsPerMin; // never wait forever
  repeat
    SleepHiRes(100);
    if fConfigRetrieved then
      exit;
  until GetTickCount64 > endtix;
  ECryptCertPkcs11.RaiseUtf8('%.EnsureRetrieveConfig timeout', [self]);
end;

function TCryptCertAlgoPkcs11.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCertPkcs11s;
begin
  result := nil;
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundLoad has finished
  if fCert <> nil then
    TCryptCertPkcs11.InternalFind(pointer(fCert), Value, Method, length(fCert),
                                    MaxCount, ICryptCerts(result));
end;

function TCryptCertAlgoPkcs11.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCertPkcs11;
var
  res: ICryptCertPkcs11s;
begin
  res := Find(Value, Method, 1);
  if res = nil then
    result := nil
  else
    result := res[0];
end;

function TCryptCertAlgoPkcs11.FindByLabel(const Value: RawUtf8): ICryptCertPkcs11;
var
  i: PtrInt;
begin
  result := nil;
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundLoad has finished
  for i := 0 to high(fCert) do
    if IdemPropNameU(fCert[i].StorageLabel, Value) then
    begin
      result := fCert[i];
      break;
    end;
end;

function TCryptCertAlgoPkcs11.FindByID(const Value: TPkcs11ObjectID): ICryptCertPkcs11;
var
  i: PtrInt;
begin
  result := nil;
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundLoad has finished
  for i := 0 to high(fCert) do
    if IdemPropNameU(fCert[i].StorageID, Value) then
    begin
      result := fCert[i];
      break;
    end;
end;

procedure TCryptCertAlgoPkcs11.CryptCertToPkcs11PrivKeyAttributes(
  const Cert: ICryptCert; const StoreLabel: RawUtf8;
  const PrivKeyDer, BinaryID: RawByteString; var Attr: CK_ATTRIBUTES);
var
  caa: TCryptAsymAlgo;
  rsa: TRsaPrivateKey; // safe decoding of RSA privake key values
  ecp, ecv: RawByteString;
begin
  Attr.New(CKO_PRIVATE_KEY, StoreLabel, BinaryID);
  AddToAttributes(Attr, [posToken,
                         posPrivate,
                         posSensitive,
                         posSign]);
  caa := Cert.AsymAlgo;
  Attr.Add(CKA_KEY_TYPE, ToULONG(CAA_TO_CKK[caa]));
  if caa in CAA_RSA then
  begin
    if not rsa.FromDer(PrivKeyDer) then
      ECryptCertPkcs11.RaiseUtf8('%.Import: no RSA Key', [self]);
    Attr.Add(CKA_MODULUS, rsa.Modulus);
    Attr.Add(CKA_PUBLIC_EXPONENT, rsa.PublicExponent);
    Attr.Add(CKA_PRIME_1, rsa.Prime1);
    Attr.Add(CKA_PRIME_2, rsa.Prime2);
    Attr.Add(CKA_PRIVATE_EXPONENT, rsa.PrivateExponent);
    Attr.Add(CKA_EXPONENT_1, rsa.Exponent1);
    Attr.Add(CKA_EXPONENT_2, rsa.Exponent2);
    Attr.Add(CKA_COEFFICIENT, rsa.Coefficient);
    exit;
    // NO rsa.Done: anti-forensic measure would flush all Attr values
  end
  else
  begin
    ecp := CAA_TO_ECPARAMS[caa];
    if ecp = '' then
      ECryptCertPkcs11.RaiseUtf8(
        '%.Import: unsupported %', [self, Cert.CertAlgo.JwtName]);
    Attr.Add(CKA_EC_PARAMS, ecp);
    ecv := SeqToEccPrivKey(CAA_CKA[caa], PrivKeyDer);
    writeln(length(ecv));
    if ecv = '' then
      ECryptCertPkcs11.RaiseUtf8(
        '%.Import: incorrect % PrivKeyDer', [self, Cert.CertAlgo.JwtName]);
    Attr.Add(CKA_VALUE, ecv) // stored as raw big number value
  end;
end;

function TCryptCertAlgoPkcs11.Import(const CertWithPrivKey: ICryptCert;
  Slot: TPkcs11SlotID; const ID: RawUtf8; const SoPinCode: SpiUtf8): ICryptCertPkcs11;
var
  der, key, binid: RawByteString;
  cert, priv: CK_OBJECT_HANDLE;
  lab: RawUtf8;
  a: CK_ATTRIBUTES;
begin
  result := nil;
  if not Assigned(CertWithPrivKey) or
     not CertWithPrivKey.HasPrivateSecret or
     (SoPinCode = '') or
     (ID = '') then
    exit;
  binid := HexToBin(ID);
  if binid = '' then
    exit;
  der := CertWithPrivKey.Save; // to be stored as CKO_CERTIFICATE
  if der = '' then
    exit;
  lab := CertWithPrivKey.GetSubject; // subject CN
  cert := CK_INVALID_HANDLE;
  priv := CK_INVALID_HANDLE;
  fEngine.Open(Slot, SoPinCode, {rw=}true, {so=}true);
  try
    // import the X.509 certificate
    cert := fEngine.AddSessionCertificate(der, CertWithPrivKey.GetSubject('DER'),
      binid, CertUsagesToPkcs11Flags(CertWithPrivKey.GetUsage, {pub=}true), lab);
    // import the associated private key
    key := CertWithPrivKey.GetPrivateKey; // raw PKCS#8 DER into CKO_PRIVATE_KEY
    if key = '' then
      exit;
    CryptCertToPkcs11PrivKeyAttributes(CertWithPrivKey, lab, key, binid, a);
    priv := fEngine.SessionCreateObject(a);
  finally
    FillZero(key);
    if result = nil then
    begin
      // on failure, delete any transient stored objects
      fEngine.SessionDestroyObject(cert);
      fEngine.SessionDestroyObject(priv);
    end;
    fEngine.Close;
  end;
end;

function TCryptCertAlgoPkcs11.Cert: ICryptCertPkcs11s;
begin
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundLoad has finished
  result := fCert;
end;

// TCryptCertAlgo methods are mostly unsupported

function TCryptCertAlgoPkcs11.New: ICryptCert;
begin
  result := nil; // unsupported
end;

function TCryptCertAlgoPkcs11.FromHandle(Handle: pointer): ICryptCert;
begin
  result := nil; // unsupported
end;

function TCryptCertAlgoPkcs11.CreateSelfSignedCsr(const Subjects: RawUtf8;
  const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;
begin
  result := ''; // unsupported
end;


{ TCryptCertPkcs11 }

procedure TCryptCertPkcs11.RaiseError(const Msg: shortstring);
begin
  ECryptCertPkcs11.RaiseUtf8('% (slot=#%, CKA_ID=%) %',
    [self, fSlotID, fStorageID, Msg]);
end;

function TCryptCertPkcs11.OpenPrivateKey: CK_OBJECT_HANDLE;
var
  pin: RawUtf8;
begin
  result := CK_INVALID_HANDLE;
  if (self = nil) or
     (fEngine = nil) or
     (fX509 = nil) or
     (fStorageID = '') or
     (fPin = '') then
    exit;
  pin := CryptDataForCurrentUser(fPin, fSecret, {encrypt=}false);
  try
    fEngine.Open(fSlotID, pin);
    try
      result := fEngine.GetObject(CKO_PRIVATE_KEY, '', HexToBin(fStorageID));
    finally
      if result = CK_INVALID_HANDLE then
        fEngine.Close;
    end;
  except
    on E: Exception do
      Log.Add.Log(sllTrace, 'OpenPrivateKey failed due to %', [E], self);
  end;
  FillZero(pin); // anti-forensic
end;

constructor TCryptCertPkcs11.Create(aOwner: TCryptCertAlgoPkcs11;
  aSlotID: TPkcs11SlotID; const aObjects: TPkcs11ObjectDynArray;
  const aValues: TRawByteStringDynArray; const aStorageID: TPkcs11ObjectID);
var
  slt: PPkcs11Slot;
  tok: PPkcs11Token;
  o: ^TPkcs11Object;
  n, i, pub: PtrInt;
  xka, xkaKey: TXPublicKeyAlgorithm;

  function ValuePubToSub: RawByteString;
  begin
    result := aValues[pub];
    if xka in [xkaEcc256 .. xkaEdDSA] then
      result := AsnDecOctStr(result); // ECC are encoded as ASN1_OCTSTR
  end;

begin
  fStorageID := aStorageID; // set both first for RaiseError()
  fSlotID := aSlotID;
  if (aOwner = nil) or
     (aOwner.Engine = nil) then
    RaiseError('Create: aOwner=nil');
  inherited Create;
  slt := aOwner.Engine.SlotByID(aSlotID);
  if slt = nil then
    RaiseError('Create: invalid slot');
  tok := aOwner.Engine.TokenByID(aSlotID);
  if tok = nil then
    RaiseError('Create: no token');
  n := length(aObjects);
  if n <> length(aValues) then
    RaiseError('Create: aObjects/aValues mismatch');
  try
    pub := -1;
    xka := xkaNone;
    o := pointer(aObjects);
    for i := 0 to high(aObjects) do
    begin
      if o^.StorageID = aStorageID then
      begin
        fStorageLabel := o^.StorageLabel;
        case o^.ObjClass of
          CKO_PUBLIC_KEY:
            begin
              if pub >= 0 then
                RaiseError('Create: duplicated public key');
              xkaKey := Pkcs11KeyAlgorithm(o^); // wild guess
              if xka = xkaNone then
                if xkaKey = xkaNone then
                  RaiseError('Create: unsupported %-% type',
                    [ToText(o^.KeyType)^, o^.KeyBits])
                else
                  xka := xkaKey; // no cert yet
              pub := i;
            end;
          CKO_CERTIFICATE:
            if fX509 <> nil then
              RaiseError('Create: duplicated certificates')
            else
            begin
              fX509 := TX509.Create;
              if not fX509.LoadFromDer(aValues[i]) then
                RaiseError('Create: invalid CKO_CERTIFICATE content');
              fIsX509 := true;
              xka := fX509.Signed.SubjectPublicKeyAlgorithm // more precise
            end
          // just ignore unneeded objects - e.g. CKO_PRIVATE_KEY in this context
        end;
      end;
      inc(o);
    end;
    if xka = xkaNone then
      RaiseError('Create: no matching object');
    if pub >= 0 then
      if fX509 = nil then
      begin
        // no associated CKO_CERTIFICATE: create a fake X.509 certificate
        o := @aObjects[pub]; // from the CKO_PUBLIC_KEY information
        fX509 := TX509.Create;
        fX509.Signed.Version := 3;
        fX509.Signed.SubjectPublicKeyAlgorithm := xka;
        fX509.Signed.SubjectPublicKey := ValuePubToSub; // from aValues[pub]
        fX509.Signed.SubjectPublicKeyBits := o^.KeyBits;
        fX509.Signed.SerialNumberHex := Sha1(aValues[pub]); // fake serial
        fX509.Signed.SerialNumber := HexToBin(fX509.Signed.SerialNumberHex);
        fX509.Signed.CertUsages := Pkcs11FlagsToCertUsages(o^.StorageFlags);
        FormatUtf8('%-%', [aSlotID, o^.StorageID], fX509.Signed.Issuer.Name[xaDC]);
        fX509.Signed.Issuer.Name[xaCN] := o^.StorageLabel;
        fX509.Signed.Issuer.Name[xaSER] := tok^.Serial;
        fX509.Signed.Issuer.Name[xaO] := tok^.Manufacturer;
        fX509.Signed.Issuer.Name[xaOU] := tok^.Model;
        fX509.Signed.Issuer.Name[xaN] := tok^.Name;
        fX509.Signed.Subject := fX509.Signed.Issuer; // emulate self-signed
      end
      else if fX509.Signed.SubjectPublicKey <> ValuePubToSub then
        RaiseError('Create: CKO_PUBLIC_KEY and CKO_CERTIFICATE do not match');
  except
    FreeAndNil(fX509);
    raise;
  end;
  // we have a new ICryptCert instance with a valid TX509: store parameters
  fCryptAlgo := aOwner;
  fEngine := aOwner.Engine;
  fCaa := XKA_TO_CAA[xka]; // approximate guess with 256-bit RSA hash
  fSlot := slt^;
  fToken := tok^;
  fSecret := ToUtf8(RandomGuid); // anti-forensic temp salt
  FormatUtf8('%-%', [aSlotID, aStorageID], fLocation);
end;

destructor TCryptCertPkcs11.Destroy;
begin
  FillZero(fSecret);
  FillZero(fPin); // paranoid
  inherited Destroy;
end;

function TCryptCertPkcs11.Log: TSynLogClass;
begin
  result := (fCryptAlgo as TCryptCertAlgoPkcs11).fLog;
end;

// ICryptCert methods

function TCryptCertPkcs11.AsymAlgo: TCryptAsymAlgo;
begin
  // don't return fCryptAlgo.AsymAlgo which is not relevant here
  result := fCaa;
end;

function TCryptCertPkcs11.CertAlgo: TCryptCertAlgo;
begin
  // don't return fCryptAlgo which is not relevant here
  result := CryptCertX509[fCaa];
end;

function TCryptCertPkcs11.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
begin
  result := nil; // unsupported yet
end;

function TCryptCertPkcs11.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
begin
  result := (Saved = '') and
            (Content = cccPrivateKeyOnly) and
            SetPin(PrivatePassword);
end;

function TCryptCertPkcs11.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
begin
  result := '';
  if not (Format in [ccfBinary, ccfPem]) then
    // hexa or base64 encoding of the binary output is handled by TCryptCert
    result := inherited Save(Content, PrivatePassword, Format)
  else
    // we implement ccfPem and ccfBinary here for cccCertOnly only
    case Content of
      cccCertOnly:
        if fX509 <> nil then
        begin
          result := fX509.SaveToDer;
          if Format = ccfPem then
            result := DerToPem(result, pemCertificate);
        end;
    else
      RaiseError('Save: only cccCertOnly is supported on a HW token');
    end;
end;

function TCryptCertPkcs11.SetPrivateKey(const saved: RawByteString): boolean;
begin
  result := false; // unsupported by definition (privkey stays in the device)
end;

function TCryptCertPkcs11.Sign(Data: pointer; Len: integer;
  Usage: TCryptCertUsage): RawByteString;
begin
  if HasPrivateSecret and
     (fX509 <> nil) and
     (Usage in fX509.Usages) then
    result := fPrivateKey.Sign(fCaa, Data, Len)
  else
    result := '';
end;

procedure TCryptCertPkcs11.Sign(const Authority: ICryptCert);
begin
  RaiseError('Sign(Authority) is not supported - use TCryptCertX509 instead');
end;

// ICryptCertPkcs11 methods

procedure TCryptCertPkcs11.SetAsymAlgo(caa: TCryptAsymAlgo);
begin
  if caa = fCaa then
    exit; // nothing to change
  if CAA_CKA[fCaa] <> CAA_CKA[caa] then
    RaiseError('SetAsymAlgo(%): incompatible with the % public key',
      [ToText(caa)^, ToText(CAA_CKA[fCaa])^]);
  fCaa := caa;
end;

function TCryptCertPkcs11.SetPin(const PinCode: SpiUtf8): boolean;
begin
  result := false;
  // store the PIN code in memory with proper obfuscation
  fPin := CryptDataForCurrentUser(PinCode, fSecret, {encrypt=}true);
  // validate the supplied PIN code
  if OpenPrivateKey = CK_INVALID_HANDLE then
  begin
    fPin := ''; // incorrect PIN
    exit;
  end;
  fEngine.Close; // no need to use this session yet
  result := true;
  if fPrivateKey = nil then
    // notify we could try to access the private key from now on
    fPrivateKey := TCryptPrivateKeyPkcs11.Create(self);
end;

function TCryptCertPkcs11.IsX509: boolean;
begin
  result := fIsX509;
end;

function TCryptCertPkcs11.Engine: TPkcs11;
begin
  result := fEngine;
end;

function TCryptCertPkcs11.SlotID: TPkcs11SlotID;
begin
  result := fSlot.Slot;
end;

function TCryptCertPkcs11.TokenName: RawUtf8;
begin
  result := fToken.Name;
end;

function TCryptCertPkcs11.StorageID: TPkcs11ObjectID;
begin
  result := fStorageID;
end;

function TCryptCertPkcs11.StorageLabel: RawUtf8;
begin
  result := fStorageLabel;
end;

function TCryptCertPkcs11.Storage: RawUtf8;
begin
  result := fLocation;
end;

function TCryptCertPkcs11.Slot: TPkcs11Slot;
begin
  result := fSlot;
end;

function TCryptCertPkcs11.Token: TPkcs11Token;
begin
  result := fToken;
end;




procedure InitializeUnit;
begin
end;

initialization
  InitializeUnit;

end.


