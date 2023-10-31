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


{ ***************** Registration of the PKCS#11 Engine to our Factories }



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
    CKM_ECDSA);       // caaEdDSA

  /// the DER hexadecimal OID for ECDSA high-level Framework curves
  // - see e.g. openssl ecparam -name prime256v1 -outform DER | hexdump -C
  // and CKA_OID[] from mormot.crypt.secure
  CAA_TO_DER: array[TCryptAsymAlgo] of RawUtf8 = (
    '06082a8648ce3d030107',    // caaES256  '1.2.840.10045.3.1.7'
    '06052b81040022',          // caaES384  '1.3.132.0.34'
    '06052b81040023',          // caaES512  '1.3.132.0.35'
    '06052b8104000a',          // caaES256K '1.3.132.0.10'
    '',                        // caaRS256
    '',                        // caaRS384
    '',                        // caaRS512
    '',                        // caaPS256
    '',                        // caaPS384
    '',                        // caaPS512
    '06092B06010401DA470F01'); // caaEdDSA


procedure Pkcs11SetMechanism(Algo: TCryptAsymAlgo; out Mech: CK_MECHANISM);
begin

end;





{ ***************** Registration of the PKCS#11 Engine to our Factories }

type
  ECryptCertPkcs11 = class(ECryptCert);

  /// class implementing ICryptCert using PCKS#11 certificate
  TCryptCertPkcs11 = class(TCryptCert)
  protected
    fEngine: TPkcs11;
    fSlot: TPkcs11Slot;
    fToken: TPkcs11Token;
    fContent: TPkcs11ObjectDynArray;
  public
    // caller use TPkcs11.SlotByID/SlotByTokenName to retrieve the slot
    constructor Create(aEngine: TPkcs11; aSlotID: TPkcs11SlotID); reintroduce;
    // ICryptCert methods
    function GetSerial: RawUtf8; override;
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

    property Content: TPkcs11ObjectDynArray
      read fContent;
  end;


{ TCryptCertPkcs11 }

constructor TCryptCertPkcs11.Create(aEngine: TPkcs11; aSlotID: TPkcs11SlotID);
var
  slt: PPkcs11Slot;
  tok: PPkcs11Token;
begin
  if aEngine = nil then
    raise ECryptCertPkcs11.CreateUtf8('%.Create: aEngine=nil', [self]);
  inherited Create;
  slt := aEngine.SlotByID(aSlotID); // some paranoid checks
  tok := aEngine.TokenByID(aSlotID);
  if slt = nil then
    raise ECryptCertPkcs11.CreateUtf8('%.Create: no slot #%', [self, aSlotID]);
  if tok = nil then
    raise ECryptCertPkcs11.CreateUtf8('%.Create: void slot #%', [self, aSlotID]);
  fEngine := aEngine;
  fSlot := slt^;
  fToken := tok^;
end;

function TCryptCertPkcs11.GetSerial: RawUtf8;
begin
  ToHumanHex(result, pointer(fToken.Serial), Length(fToken.Serial));
end;




procedure InitializeUnit;
begin
end;

initialization
  InitializeUnit;

end.


