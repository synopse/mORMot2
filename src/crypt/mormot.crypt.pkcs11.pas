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
  mormot.core.json,
  mormot.core.rtti,
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
  /// class implementing ICryptCert using PCKS#11
  // - CKO_CERTIFICATES will be directly retrieved from their X.509 DER binary
  // - CKO_PUBLIC_KEY will create fake X.509 certificate from their raw binary
  // - CKO_PRIVATE_KEY will be used when needed, using a safely stored PIN
  TCryptCertPkcs11 = class(TCryptCertX509Only)
  protected
    fEngine: TPkcs11;
    fSlot: TPkcs11Slot;
    fToken: TPkcs11Token;
    fStorageID: TPkcs11ObjectID; // match TPkcs11Object.StorageID
    fSecret, fPin: RawByteString;
    fCaa: TCryptAsymAlgo;
  public
    /// create a X.509 from the supplied information
    // - should supply all aObjects[] and aValues[] on this SlotID and
    // a given CKA_ID to filter
    // - if no session is currently opened, no CKO_PRIVATE_KEY may be available:
    // call later Load('', cccPrivateKeyOnly) and PIN as PrivatePassword
    constructor Create(aEngine: TPkcs11; aSlotID: TPkcs11SlotID;
      const aObjects: TPkcs11ObjectDynArray; const aValues: TRawByteStringDynArray;
      const aStorageID: TPkcs11ObjectID); reintroduce;
    /// the associated PKCS#11 instance
    property Engine: TPkcs11
      read fEngine;
    /// the associated slot in the PKCS#11 instance Engine
    // - retrieved in the constructor - may have changed on HW in-between
    property Slot: TPkcs11Slot
      read fSlot;
    /// the associated slot in the PKCS#11 instance Engine
    // - retrieved in the constructor - may have changed on HW in-between
    property Token: TPkcs11Token
      read fToken;
    /// the associated hexadecimal CKA_ID in the PKCS#11 instance Engine
    // - all involved TPkcs11Object.StorageID do match in the objects list
    property StorageID: TPkcs11ObjectID
      read fStorageID;
    // ICryptCert methods
    function AsymAlgo: TCryptAsymAlgo; override;
    function CertAlgo: TCryptCertAlgo; override;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; override; // PIN cccPrivateKeyOnly
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


{ TCryptCertPkcs11 }

constructor TCryptCertPkcs11.Create(aEngine: TPkcs11; aSlotID: TPkcs11SlotID;
  const aObjects: TPkcs11ObjectDynArray; const aValues: TRawByteStringDynArray;
  const aStorageID: TPkcs11ObjectID);
var
  slt: PPkcs11Slot;
  tok: PPkcs11Token;
  o: ^TPkcs11Object;
  n, i, pub: PtrInt;
  xka, xkaKey: TXPublicKeyAlgorithm;

  procedure RaiseError(const Context: RawUtf8);
  begin
    raise ECryptCertPkcs11.CreateUtf8('%.Create(slot=#%, CKA_ID=%): %',
      [self, aSlotID, aStorageID, Context]);
  end;

begin
  if aEngine = nil then
    RaiseError('aEngine=nil');
  inherited Create;
  slt := aEngine.SlotByID(aSlotID);
  if slt = nil then
    RaiseError('invalid slot');
  tok := aEngine.TokenByID(aSlotID);
  if tok = nil then
    RaiseError('no token');
  n := length(aObjects);
  if n <> length(aValues) then
    RaiseError('aObjects/aValues mismatch');
  try
    pub := -1;
    xka := xkaNone;
    o := pointer(aObjects);
    for i := 0 to high(aObjects) do
    begin
      if o^.StorageID = aStorageID then
      begin
        case o^.ObjClass of
          CKO_PUBLIC_KEY:
            begin
              if pub >= 0 then
                RaiseError('duplicated public key');
              xkaKey := Pkcs11KeyAlgorithm(o^); // wild guess
              if xka = xkaNone then
                if xkaKey = xkaNone then
                  RaiseError(FormatUtf8('unsupported %-% type',
                    [ToText(o^.KeyType)^, o^.KeyBits]))
                else
                  xka := xkaKey; // no cert yet
              pub := i;
            end;
          CKO_CERTIFICATE:
            if fX509 <> nil then
              RaiseError('duplicated certificates')
            else
            begin
              fX509 := TX509.Create;
              if fX509.LoadFromDer(aValues[i]) then
                xka := fX509.Signed.SubjectPublicKeyAlgorithm // more precise
              else
                RaiseError('invalid CKO_CERTIFICATE content')
            end
          // just ignore unneeded objects - e.g. CKO_PRIVATE_KEY in this context
        end;
      end;
      inc(o);
    end;
    if xka = xkaNone then
      RaiseError('no matching object');
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
        fX509.Signed.Issuer.Name[xaOU] := tok^.Name;
        fX509.Signed.Issuer.Name[xaO] := tok^.Manufacturer;
        fX509.Signed.Issuer.Name[xaN] := tok^.Model;
        fX509.Signed.Issuer.Name[xaSER] := tok^.Serial;
        fX509.Signed.Subject := fX509.Signed.Issuer; // emulate self-signed
        fX509.LoadFromDer(fX509.SaveToDer);
      end
      else if fX509.Signed.SubjectPublicKey <> aValues[pub] then
        RaiseError('CKO_PUBLIC_KEY and CKO_CERTIFICATE do not match');
  except
    FreeAndNil(fX509);
    raise;
  end;
  // we have a new ICryptCert instance with a valid TX509: store parameters
  fEngine := aEngine;
  fSlot := slt^;
  fToken := tok^;
  fStorageID := aStorageID;
  fCaa := XKA_TO_CAA[xka]; // approximate guess with 256-bit RSA hash
  fSecret := ToUtf8(RandomGuid); // anti-forensic temp salt
end;

function TCryptCertPkcs11.AsymAlgo: TCryptAsymAlgo;
begin
  result := fCaa;
end;

function TCryptCertPkcs11.CertAlgo: TCryptCertAlgo;
begin
  result := CryptCertX509[fCaa];
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




procedure InitializeUnit;
begin
end;

initialization
  InitializeUnit;

end.


