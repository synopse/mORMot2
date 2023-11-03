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


{ ***************** Registration of the PKCS#11 Engine to our Factories }

type
  /// Certificate interface with specific PKCS#11 information
  // - inherit from ICryptCert so has all the main Certificates methods
  // - as implemented by TCryptCertPkcs11 instances
  // - values are retrieved in the constructor - may have changed on HW in-between
  ICryptCertPkcs11 = interface(ICryptCert)
    /// change the high-level asymmetric algorithm used for this certificate
    // - by default, it is guessed with 256-bit RSA hash via XKA_TO_CAA[] lookup
    // - you can refine the expected hash algorithm used for Verify/Sign
    // - any key algorithm switch (e.g. from RSA to ECC) would fail
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    /// true if was filled from a true CKO_CERTIFICATE instance
    function IsX509: boolean;
    /// the associated PKCS#11 library instance
    function Engine: TPkcs11;
    /// the associated hexadecimal CKA_ID in the token of this PKCS#11 instance
    // - all involved TPkcs11Object.StorageID do match in the objects list
    function StorageID: TPkcs11ObjectID;
    /// the associated label, as shared by all objects of this StorageID
    function StorageLabel: RawUtf8;
    /// the associated Slot ID in the PKCS#11 instance Engine
    function SlotID: TPkcs11SlotID;
    /// the associated Token Name in the PKCS#11 instance Engine
    function TokenName: RawUtf8;
    /// the associated slot in the PKCS#11 instance Engine
    function Slot: TPkcs11Slot;
    /// the associated token in the PKCS#11 instance Engine
    function Token: TPkcs11Token;
  end;

  /// store several Certificate interfaces with specific PKCS#11 information
  ICryptCertPkcs11s = array of ICryptCertPkcs11;

  /// class loading a PKCS#11 library in the context of ICryptCert
  // - it is the main factory of ICryptCert support of a PKCS#11 library
  TCryptCertAlgoPkcs11 = class(TCryptCertAlgo)
  protected
    fEngine: TPkcs11;
    fLog: TSynLogClass;
    fConfigRetrieved: boolean;
    fCert: ICryptCertPkcs11s;
    procedure BackgroundRetrieveConfig(Sender: TObject);
    procedure EnsureRetrieveConfig;
  public
    /// load a PKCS#11 library and asynchronously retrieve its configuration
    // - raise EPkcs11 if the library can't be loaded into the Engine property
    // - slow Engine.RetrieveConfig() will then take place in a background thread
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
    // TCryptCertAlgo methods are mostly unsupported
    function New: ICryptCert; override;
    function FromHandle(Handle: pointer): ICryptCert; override;
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8; override;
    /// the associated PKCS#11 library instance
    property Engine: TPkcs11
      read fEngine;
    /// access to the high-level certificates recognized in this PKCS#11 instance
    // - will wait if background loading of information is not finished
    function Cert: ICryptCertPkcs11s;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// class implementing ICryptCert using PCKS#11
  // - CKO_CERTIFICATES will be directly retrieved from their X.509 DER binary
  // - CKO_PUBLIC_KEY will create fake X.509 certificate from their raw binary
  // - CKO_PRIVATE_KEY will be used when needed, using a PIN code specified as
  // PrivatePassword in Load(cccPrivateKeyOnly) - stored encrypted in memory
  TCryptCertPkcs11 = class(TCryptCertX509Only, ICryptCertPkcs11)
  protected
    fEngine: TPkcs11;
    fStorageID: TPkcs11ObjectID;  // match TPkcs11Object.StorageID
    fStorageLabel: RawUtf8;       // match TPkcs11Object.StorageLabel
    fSecret, fPin: RawByteString; // anti-forensic PIN storage
    fSlotID: TPkcs11SlotID;
    fIsX509: boolean;
    fCaa: TCryptAsymAlgo;
    fSlot: TPkcs11Slot;
    fToken: TPkcs11Token;
    procedure RaiseError(const Msg: shortstring); overload; override;
  public
    /// create a X.509 from the supplied information
    // - should supply all aObjects[] and aValues[] on this SlotID and
    // a given CKA_ID to filter
    // - if no session is currently opened, no CKO_PRIVATE_KEY may be available:
    // call later Load('', cccPrivateKeyOnly) and PIN as PrivatePassword
    constructor Create(aOwner: TCryptCertAlgoPkcs11; aSlotID: TPkcs11SlotID;
      const aObjects: TPkcs11ObjectDynArray; const aValues: TRawByteStringDynArray;
      const aStorageID: TPkcs11ObjectID); reintroduce;
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
    function HasPrivateSecret: boolean; override;
    function GetPrivateKey: RawByteString; override;
    function SetPrivateKey(const saved: RawByteString): boolean; override;
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage): RawByteString; override;
    procedure Sign(const Authority: ICryptCert); override;
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    // ICryptCertPkcs11 methods
    procedure SetAsymAlgo(caa: TCryptAsymAlgo);
    function IsX509: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function Engine: TPkcs11;
      {$ifdef HASINLINE} inline; {$endif}
    function StorageID: TPkcs11ObjectID;
      {$ifdef HASINLINE} inline; {$endif}
    function StorageLabel: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function SlotID: TPkcs11SlotID;
      {$ifdef HASINLINE} inline; {$endif}
    function TokenName: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function Slot: TPkcs11Slot;
    function Token: TPkcs11Token;
  end;


implementation

{ ***************** High-Level PKCS#11 Integration with the Framework Types }

const
  /// the CK_MECHANISM for each high-level Framework algorithm
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


  /// the DER hexadecimal OID for ECDSA high-level Framework curves
  // - see e.g. openssl ecparam -name prime256v1 -outform DER | hexdump -C
  // and CKA_OID[] from mormot.crypt.secure
  CAA_TO_DER: array[TCryptAsymAlgo] of RawByteString = (
    #$06#$08#$2a#$86#$48#$ce#$3d#$03#$01#$07, // caaES256  '1.2.840.10045.3.1.7'
    #$06#$05#$2b#$81#$04#$00#$22,             // caaES384  '1.3.132.0.34'
    #$06#$05#$2b#$81#$04#$00#$23,             // caaES512  '1.3.132.0.35'
    #$06#$05#$2b#$81#$04#$00#$0a,             // caaES256K '1.3.132.0.10'
    '',                        // caaRS256
    '',                        // caaRS384
    '',                        // caaRS512
    '',                        // caaPS256
    '',                        // caaPS384
    '',                        // caaPS512
    #$06#$09#$2B#$06#$01#$04#$01#$DA#$47#$0F#$01);
    // caaEdDSA '1.3.6.1.4.1.11591.15.1' - but optional


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
  // cuCodeSign, cuTlsServer and cuTlsClient require a full X.509 certificate
  // with its issuer/authority fields for proper PKI trust chain verification
end;


{ ***************** Registration of the PKCS#11 Engine to our Factories }

type
  ECryptCertPkcs11 = class(ECryptCert);


{ TCryptCertAlgoPkcs11 }

constructor TCryptCertAlgoPkcs11.Create(const aLibraryName: TFileName;
  aLog: TSynLogClass);
begin
  if aLog = nil then
    aLog := TSynLog;
  fLog := aLog;
  with fLog.Enter('Create %', [aLibraryName], self) do
  begin
    fEngine := TPkcs11.Create(aLibraryName); // load the dll/so library
    TLoggedWorkThread.Create(fLog, 'RetrieveConfig', self, BackgroundRetrieveConfig);
  end;
end;

destructor TCryptCertAlgoPkcs11.Destroy;
begin
  fEngine.Free;
  inherited Destroy;
end;

function TCryptCertAlgoPkcs11.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCertPkcs11s;
begin
  result := nil;
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundRetrieveConfig has finished
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

procedure TCryptCertAlgoPkcs11.BackgroundRetrieveConfig(Sender: TObject);
var
  i, j: PtrInt;
  val: TRawByteStringDynArray;
  obj: TPkcs11ObjectDynArray;
  ids: TPkcs11ObjectIDs;
begin
  // this operation could take 10 seconds
  fEngine.RetrieveConfig({includevoid=}false, {includmechs=}false);
  fLog.Add.Log(sllDebug, 'BackgroundRetrieveConfig %', [fEngine], self);
  // generate all ICryptCertPkcs11 certificates from the retrieved information
  for i := 0 to high(fEngine.SlotIDs) do
  try
    fEngine.Open(fEngine.SlotIDs[i]); // anynymous session
    try
      obj := fEngine.GetObjects(nil, @val); // all objects
      ids := nil;
      for j := 0 to high(obj) do
        AddRawUtf8(ids, obj[j].StorageID, {nodup=}true);
      for j := 0 to high(ids) do
        InterfaceArrayAdd(fCert,
          TCryptCertPkcs11.Create(self, fEngine.SlotIDs[i], obj, val, ids[j]));
    finally
      fEngine.Close; // close session
    end;
  except
    on E: Exception do
      fLog.Add.Log(sllTrace, 'BackgroundRetrieveConfig: slot % raised %',
        [fEngine.SlotIDs[i], E], self); // log and continue
  end;
  fConfigRetrieved := true;
end;

procedure TCryptCertAlgoPkcs11.EnsureRetrieveConfig;
var
  endtix: Int64;
begin
  endtix := GetTickCount64 + 60000; // never wait forever
  repeat
    SleepHiRes(100);
    if fConfigRetrieved then
      exit;
  until GetTickCount64 > endtix;
  raise ECryptCertPkcs11.CreateUtf8('%.EnsureRetrieveConfig timeout', [self]);
end;

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

function TCryptCertAlgoPkcs11.Cert: ICryptCertPkcs11s;
begin
  if not fConfigRetrieved then
    EnsureRetrieveConfig; // wait until BackgroundRetrieveConfig has finished
  result := fCert;
end;


{ TCryptCertPkcs11 }

procedure TCryptCertPkcs11.RaiseError(const Msg: shortstring);
begin
  raise ECryptCertPkcs11.CreateUtf8('% (slot=#%, CKA_ID=%) %',
    [self, fSlotID, fStorageID, Msg]);
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
        fX509.Signed.SubjectPublicKey := aValues[pub];
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
        fX509.LoadFromDer(fX509.SaveToDer);
      end
      else if fX509.Signed.SubjectPublicKey <> aValues[pub] then
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
  result := nil; // unsupported
end;

function TCryptCertPkcs11.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
begin
  result := false;
  if (Saved <> '') or
     (Content <> cccPrivateKeyOnly) then
    exit;
  fPin := CryptDataForCurrentUser(PrivatePassword, fSecret, true);
  result := true;
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
      RaiseError('Save: only cccCertOnly is supported');
    end;
end;

function TCryptCertPkcs11.HasPrivateSecret: boolean;
begin
  result := fPin <> '';
end;

function TCryptCertPkcs11.GetPrivateKey: RawByteString;
begin
  result := ''; // unsupported
end;

function TCryptCertPkcs11.SetPrivateKey(const saved: RawByteString): boolean;
begin
  result := false; // unsupported
end;

function TCryptCertPkcs11.Sign(Data: pointer; Len: integer;
  Usage: TCryptCertUsage): RawByteString;
begin

end;

procedure TCryptCertPkcs11.Sign(const Authority: ICryptCert);
begin
  RaiseError('Sign(Authority) is unsupported');
end;

function TCryptCertPkcs11.Decrypt(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin

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


