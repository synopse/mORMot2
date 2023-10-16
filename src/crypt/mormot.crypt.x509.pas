/// Framework Core X.509 Certificates Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.x509;

{
  *****************************************************************************

   X.509 Certificates Implementation - see RFC 5280
    - X.509 Fields Logic
    - RSA and ECC Public/Private Key support for X.509
    - X.509 Certificates and Certificate Signing Request (CSR)
    - Registration of our X.509 Engine to the TCryptCert Factory

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
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.json,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.ecc256r1,
  mormot.crypt.ecc,
  mormot.crypt.rsa;


{ **************** X.509 Fields Logic }

// note: all X.509 type names in this unit start with TX

type
  /// exception raised by this unit
  EX509 = class(ESynException);

  /// known X.501 Type Names, as stored in X.509 Certificates attributes
  // - as defined in RFC 5280 appendix A, and available via TXName.Names[]
  // - corresponding Relative Distinguished Name (RDN) text is extracted via
  // RTTI, e.g. 'CN' for xaCN or 'OU' for xaOU - as in TextToXa()
  TXAttr = (
    xaNone,
    xaDC,   // domainComponent
    xeUID,  // userID
    xaCN,   // commonName (3)
    xaSER,  // serialNumber (5)
    xaC,    // countryName (6)
    xaL,    // localityName (7)
    xaST,   // stateOrProvinceName (8)
    xaO,    // organizationName (10)
    xaOU,   // organizationalUnitName (11)
    xaT,    // title (12)
    xaTN,   // telephoneNumber (20)
    xaN,    // name (41)
    xaSN,   // surname (4)
    xaGN,   // givenName (42)
    xaI,    // initials (43)
    xaGQ,   // generationQualifier (44)
    xaQ,    // distinguishedNameQualifier (46)
    xaP,    // pseudonym (65)
    xaE);   // email

  /// known X.509 v3 Certificate extensions
  // - standard extensions as defined in RFC 5280 4.2.1
  TXExtension = (
    xeNone,
    xeSubjectDirAttributes,    // 9
    xeSubjectKeyIdentifier,    // 14
    xeKeyUsage,                // 15
    xeSubjectAlternativeName,  // 17
    xeIssuerAlternativeName,   // 18
    xeBasicConstraints,        // 19
    xeNameConstraints,         // 30
    xeCrlDistributionPoints,   // 31
    xeCertificatePolicies,     // 32
    xePolicyMappings,          // 33
    xeAuthorityKeyIdentifier,  // 35
    xePolicyConstraints,       // 36
    xeExtendedKeyUsage,        // 37
    xeAuthorityInformationAccess,
    xeGoogleSignedCertificateTimestamp,
    xeNetscapeComment);

  /// X.509 Certificate Key Usage - see RFC 5280 Section 4.2.1.3
  // - bit order is inverted to the RFC due to BITSTRING encoding
  TXKeyUsage = (
    xuEncipherOnly,
    xuCrlSign,
    xuKeyCertSign,
    xuKeyAgreement,
    xuDataEncipherment,
    xuKeyEncipherment,
    xuNonRepudiation,
    xuDigitalsignature,
    xuDecipherOnly);

  /// set of X.509 Certificate Key Usages - see RFC 5280 Section 4.2.1.3
  TXKeyUsages = set of TXKeyUsage;

  /// X.509 Certificate Extended Key Usage - see RFC 5280 Section 4.2.1.12
  TXExtendedKeyUsage = (
    xkuNone,
    xkuServerAuth,         // 1
    xkuClientAuth,         // 2
    xkuCodeSigning,        // 3
    xkuEmailProtection,    // 4
    xkuTimeStamping,       // 8
    xkuOcspSigning);       // 9

  /// set of X.509 Certificate Extended Key Usage - see RFC 5280 Section 4.2.1.12
  TXExtendedKeyUsages = set of TXExtendedKeyUsage;

  /// supported X.509 Signature Algorithms
  // - implement RSA, RSA-PSS and ECC-256 asymmetric algorithms using our units
  // - for safety, any unsafe algorithms (e.g. MD5 or SHA-1) are not defined
  TXSignatureAlgorithm = (
    xsaNone,
    xsaSha256Rsa,
    xsaSha384Rsa,
    xsaSha512Rsa,
    xsaSha256RsaPss,
    xsaSha384RsaPss,
    xsaSha512RsaPss,
    xsaSha256Ecc256);

  /// supported TX509.SubjectPublicKeyAlgorithm values
  TXPublicKeyAlgorithm = (
    xkaNone,
    xkaRsa,
    xkaRsaPss,
    xkaEcc256);

  /// used to store one unknown/unsupported attribute or extension
  // - in TXname.Other or TXTbsCertificate.ExtensionOther
  TXOther = record
    /// the OID of this value, in raw binary form
    Oid: RawByteString;
    /// the associated value
    // - as RawUtf8 for TXname.Other[]
    // - as ASN1_OCTSTR raw content for TXTbsCertificate.ExtensionOther[]
    Value: RawByteString;
  end;
  PXOther = ^TXOther;

  /// used to store the unknown attributes or extensions
  // - in TXname.Other or TXTbsCertificate.ExtensionOther
  TXOthers = array of TXOther;

  /// store the CSV of the values of each kind of known attributes
  // - as used in TXName.Name and TX509.Issuer
  TXAttrNames = array[TXAttr] of RawUtf8;

  /// decoded extensions as defined for X.509 v3 certificates
  // - as used in TXTbsCertificate.Extension[] and TX509.Extension
  TXExtensions = array[TXExtension] of RawUtf8;

  /// a X.501 Type Name as used in X.509 Certificate Issuer and Subject fields
  {$ifdef USERECORDWITHMETHODS}
  TXName = record
  {$else}
  TXName = object
  {$endif USERECORDWITHMETHODS}
  private
    fSafe: TLightLock;
    fCachedAsn: RawByteString;
    fCachedText: RawUtf8;
    procedure ComputeAsn;
    procedure ComputeText;
  public
    /// CSV of the values of each kind of known attributes
    Name: TXAttrNames;
    /// values which are not part of the known attributes
    Other: TXOthers;
    /// convert Name[a] from CSV to an array of RawUtf8
    function NameArray(a: TXAttr): TRawUtf8DynArray;
    /// the raw ASN1_SEQ encoded value of this name
    // - is cached internally for efficiency
    function ToBinary: RawByteString;
    /// return the values as a single line Distinguished Name text
    // - e.g. 'CN=R3, C=US, O=Let''s Encrypt'
    // - is cached internally for efficiency
    function AsDNText: RawUtf8;
    /// unserialize the X.501 Type Name from raw ASN1_SEQ binary
    function FromAsn(const seq: TAsnObject): boolean;
    /// unserialize the X.501 Type Name from the next raw ASN1_SEQ binary
    function FromAsnNext(var pos: integer; const der: TAsnObject): boolean;
    /// fill Name[] attributes with TCryptCertFields information
    procedure FromFields(const fields: TCryptCertFields);
    /// return Name[] by RDN, or ToDigest() by hash name, or by FindOid()
    function Get(const Rdn: RawUtf8): RawUtf8;
    /// return the hash of the normalized Binary of this field
    function ToDigest(algo: THashAlgo = hfSha1): RawUtf8;
    /// return the UTF-8 text value of a given text OID
    // - search in Other[] then Name[]
    function FindOid(const oid: RawUtf8): RawUtf8;
    /// to be called once any field has been changed to refresh internal caches
    procedure AfterModified;
  end;

/// append a new entry to a dynamic array of TXOther
procedure AddOther(var others: TXOthers; const o, v: RawByteString);

/// efficient search of a TXOther.Value from a 'x.x.x.x.x' text OID
function FindOther(const Other: TXOthers; const OidText: RawUtf8): RawByteString;

/// low-level search of a TXOther.Value from a binary OID
function FindOtherAsn(o: PXOther; n: integer; const OidBinary: TAsnObject): RawByteString;

function ToText(a: TXAttr): PShortString; overload;
function ToText(e: TXExtension): PShortString; overload;
function ToText(u: TXKeyUsage): PShortString; overload;
function ToText(x: TXExtendedKeyUsage): PShortString; overload;
function ToText(a: TXSignatureAlgorithm): PShortString; overload;
function ToText(a: TXPublicKeyAlgorithm): PShortString; overload;

/// identifies the known RDN text e.g. 'CN' as xaCN or 'OU' as xaOU
function TextToXa(const Rdn: RawUtf8; out Xa: TXAttr): boolean;

const
  /// set of the Public Key Algorithms using RSA cryptography
  xkaRsas = [xkaRsa, xkaRsaPss];

  /// internal lookup table from X.509 Signature to Public Key Algorithms
  XSA_TO_XKA: array[TXSignatureAlgorithm] of TXPublicKeyAlgorithm = (
    xkaNone,     // xsaNone
    xkaRsa,      // xsaSha256Rsa
    xkaRsa,      // xsaSha384Rsa
    xkaRsa,      // xsaSha512Rsa
    xkaRsaPss,   // xsaSha256RsaPss
    xkaRsaPss,   // xsaSha384RsaPss
    xkaRsaPss,   // xsaSha512RsaPss
    xkaEcc256);  // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature to Hash Algorithms
  XSA_TO_HF: array[TXSignatureAlgorithm] of THashAlgo = (
    hfSha256,    // xsaNone
    hfSha256,    // xsaSha256Rsa
    hfSha384,    // xsaSha384Rsa
    hfSha512,    // xsaSha512Rsa
    hfSha256,    // xsaSha256RsaPss
    hfSha384,    // xsaSha384RsaPss
    hfSha512,    // xsaSha512RsaPss
    hfSha256);   // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature to ICryptCert Algorithms
  XSA_TO_AA: array[TXSignatureAlgorithm] of TCryptAsymAlgo = (
    caaES256K,   // xsaNone
    caaRS256,    // xsaSha256Rsa
    caaRS384,    // xsaSha384Rsa
    caaRS512,    // xsaSha512Rsa
    caaPS256,    // xsaSha256RsaPss
    caaPS384,    // xsaSha384RsaPss
    caaPS512,    // xsaSha512RsaPss
    caaES256);   // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature Algorithm as text
  XSA_TXT: array[TXSignatureAlgorithm] of RawUtf8 = (
    '',                               // xsaNone
    'SHA256 with RSA encryption',     // xsaSha256Rsa
    'SHA384 with RSA encryption',     // xsaSha384Rsa
    'SHA512 with RSA encryption',     // xsaSha512Rsa
    'SHA256 with RSA-PSS encryption', // xsaSha256RsaPss
    'SHA384 with RSA-PSS encryption', // xsaSha384RsaPss
    'SHA512 with RSA-PSS encryption', // xsaSha512RsaPss
    'SHA256 with prime256v1 ECDSA');  // xsaSha256Ecc256

  /// internal lookup table from X.509 Public Key Algorithm as text
  XKA_TXT: array[TXPublicKeyAlgorithm] of RawUtf8 = (
    '',                    // xkaNone
    'RSA encryption',      // xkaRsa
    'RSA-PSS encryption',  // xkaRsaPss
    'prime256v1 ECDSA');   // xkaEcc256

  /// the OID of all known TX509Name attributes, as defined in RFC 5280 A.1
  XA_OID: array[TXAttr] of PUtf8Char = (
    '',                           // xaNone
    '0.9.2342.19200300.100.1.25', // xaDC  domainComponent
    '0.9.2342.19200300.100.1.1',  // xeUID userID
    '2.5.4.3',                    // xaCN  commonName (3)
    '2.5.4.5',                    // xaSER serialNumber (5)
    '2.5.4.6',                    // xaC   countryName (6)
    '2.5.4.7',                    // xaL   localityName (7)
    '2.5.4.8',                    // xaST  stateOrProvinceName (8)
    '2.5.4.10',                   // xaO   organizationName (10)
    '2.5.4.11',                   // xaOU  organizationalUnitName (11)
    '2.5.4.12',                   // xaT   title (12)
    '2.5.4.20',                   // xaTN  telephoneNumber (20)
    '2.5.4.41',                   // xaN   name (41)
    '2.5.4.4',                    // xaSN  surname (4)
    '2.5.4.42',                   // xaGN  givenName (42)
    '2.5.4.43',                   // xaI   initials (43)
    '2.5.4.44',                   // xaGQ  generationQualifier (44)
    '2.5.4.46',                   // xaQ   distinguishedNameQualifier (46)
    '2.5.4.65',                   // xaP   pseudonym (65)
    '1.2.840.113549.1.9.1');      // xaE   email

  /// the OID of all known X.509 v3 Certificate extensions, as in RFC 5280 4.2.1
  // - with some additional common values
  XE_OID: array[TXExtension] of PUtf8Char = (
    '',                          // xeNone
    '2.5.29.9',                  // xeSubjectDirAttributes
    '2.5.29.14',                 // xeSubjectKeyIdentifier
    '2.5.29.15',                 // xeKeyUsage
    '2.5.29.17',                 // xeSubjectAlternativeName
    '2.5.29.18',                 // xeIssuerAlternativeName
    '2.5.29.19',                 // xeBasicConstraints
    '2.5.29.30',                 // xeNameConstraints
    '2.5.29.31',                 // xeCrlDistributionPoints
    '2.5.29.32',                 // xeCertificatePolicies
    '2.5.29.33',                 // xePolicyMappings
    '2.5.29.35',                 // xeAuthorityKeyIdentifier
    '2.5.29.36',                 // xePolicyConstraints
    '2.5.29.37',                 // xeExtendedKeyUsage
    '1.3.6.1.5.5.7.1.1',         // xeAuthorityInformationAccess
    '1.3.6.1.4.1.11129.2.4.2',   // xeGoogleSignedCertificateTimestamp
    '2.16.840.1.113730.1.13');   // xeNetscapeComment

  /// the OID of all known X.509 Certificate Extended Key Usage
  XKU_OID: array[TXExtendedKeyUsage] of PUtf8Char = (
    '',                    // xkuNone
    '1.3.6.1.5.5.7.3.1',   // xkuServerAuth
    '1.3.6.1.5.5.7.3.2',   // xkuClientAuth
    '1.3.6.1.5.5.7.3.3',   // xkuCodeSigning
    '1.3.6.1.5.5.7.3.4',   // xkuEmailProtection
    '1.3.6.1.5.5.7.3.8',   // xkuTimeStamping
    '1.3.6.1.5.5.7.3.9');  // xkuOcspSigning

  /// the OID of all known X.509 Signature Algorithms
  // - RSA-PSS store ASN1_OID_PKCS1_RSA_PSS with the THashAlgo as parameters
  ASN1_OID_SIGNATURE: array[TXSignatureAlgorithm] of RawUtf8 = (
     '',
     '1.2.840.113549.1.1.11',  // xsaSha256Rsa
     '1.2.840.113549.1.1.12',  // xsaSha384Rsa
     '1.2.840.113549.1.1.13',  // xsaSha512Rsa
     '2.16.840.1.101.3.4.2.1', // xsaSha256RsaPss = ASN1_OID_HASH[hfSHA256]
     '2.16.840.1.101.3.4.2.2', // xsaSha384RsaPss = ASN1_OID_HASH[hfSHA384]
     '2.16.840.1.101.3.4.2.3', // xsaSha512RsaPss = ASN1_OID_HASH[hfSHA256]
     '1.2.840.10045.4.3.2');   // xsaSha256Ecc256

  ASN1_OID_PKCS1_RSA       = '1.2.840.113549.1.1.1';
  ASN1_OID_PKCS1_MGF       = '1.2.840.113549.1.1.8';
  ASN1_OID_PKCS1_RSA_PSS   = '1.2.840.113549.1.1.10';
  ASN1_OID_PKCS9_EXTREQ    = '1.2.840.113549.1.9.14';
  ASN1_OID_X962_PUBLICKEY  = '1.2.840.10045.2.1';
  ASN1_OID_X962_ECDSA_P256 = '1.2.840.10045.3.1.7';

function XsaToSeq(xsa: TXSignatureAlgorithm): TAsnObject;
function XkaToSeq(xka: TXPublicKeyAlgorithm): RawByteString;
function OidToXsa(const oid: RawUtf8; out xsa: TXSignatureAlgorithm): boolean;
function OidToXka(const oid, oid2: RawUtf8; out xka: TXPublicKeyAlgorithm): boolean;
function OidToXa(const oid: RawByteString): TXAttr;
function OidToXe(const oid: RawByteString): TXExtension;
function OidToXku(const oid: RawByteString): TXExtendedKeyUsage;
function XkuToOids(usages: TXExtendedKeyUsages): RawByteString;


{ **************** RSA and ECC Public/Private Key support for X.509 }

type
  /// store a RSA or ECC public key for TX509
  TXPublicKey = class
  protected
    fRsa: TRsa;
    fEcc: TEcc256r1VerifyAbstract;
    fEccPub: TEccPublicKey;
    fSubjectPublicKey: RawByteString;
    fAlgo: TXPublicKeyAlgorithm;
  public
    /// unserialized the public key from raw binary stored in a X.509 certificate
    function Load(Algorithm: TXPublicKeyAlgorithm;
      const SubjectPublicKey: RawByteString): boolean;
    /// finalize this instance
    destructor Destroy; override;
    /// verify the RSA or ECC signature of a given hash
    // - also checking the store OID for RSA so that it do match the Hash algorithm
    function Verify(Sig: pointer; Dig: THash512Rec; SigLen, DigLen: integer;
      Hash: THashAlgo): boolean; overload;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(DigAlgo: TXSignatureAlgorithm;
      Data, Sig: pointer; DataLen, SigLen: integer): boolean; overload;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(DigAlgo: TXSignatureAlgorithm;
      const Data, Sig: RawByteString): boolean; overload;
    /// as used by TCryptCertX509.GetPrivateKeyParams
    function GetParams(out x, y: RawByteString): boolean;
    /// use EciesSeal or RSA sealing, i.e. encryption with this public key
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
  end;

  /// store a RSA or ECC private key for TX509
  TXPrivateKey = class
  protected
    fRsa: TRsa;
    fEcc: TEccPrivateKey;
    fAlgo: TXPublicKeyAlgorithm;
  public
    /// unserialized the private key from DER binary or PEM text
    // - will also ensure the private key do match the associated public key
    function Load(Algorithm: TXPublicKeyAlgorithm; AssociatedKey: TXPublicKey;
      const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean;
    /// create a new private / public key pair
    // - returns the associated public key binary in SubjectPublicKey format
    function Generate(Algorithm: TXPublicKeyAlgorithm): RawByteString;
    /// compute a self-signed Certificate Signing Request as PEM
    // - generate a new private / public key pair if none is already available
    function ComputeSelfSignedCsr(Algorithm: TXSignatureAlgorithm;
      const Subjects: RawUtf8; Usages: TCryptCertUsages;
      Fields: PCryptCertFields): RawUtf8;
    /// finalize this instance
    destructor Destroy; override;
    /// return the private key as raw binary
    function ToDer: RawByteString;
    /// return the associated public key as stored in a X509 certificate
    function ToSubjectPublicKey: RawByteString;
    /// return the private key in the TCryptCertX509.Save expected format
    function Save(Format: TCryptCertFormat; const Password: SpiUtf8): RawByteString;
    /// sign a memory buffer digest with RSA or ECC using the stored private key
    // - storing the DigAlgo Hash algorithm OID for RSA
    function Sign(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TXSignatureAlgorithm): RawByteString; overload;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(DigAlgo: TXSignatureAlgorithm;
      Data: pointer; DataLen: integer): RawByteString; overload;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(DigAlgo: TXSignatureAlgorithm;
      const Data: RawByteString): RawByteString; overload;
    /// use EciesSeal or RSA un-sealing, i.e. decryption with this private key
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
    /// compute the shared-secret with another public key
    // - by design, ECDHE is only available for ECC
    function SharedSecret(pub: TXPublicKey): RawByteString;
  end;


{ **************** X.509 Certificates and Certificate Signing Request (CSR) }

type
  /// X.509 Certificate fields, as defined in RFC 5280 #4.1.2 and in TX509.Signed
  // - contains information associated with the subject of the certificate
  // and the CA that issued it
  {$ifdef USERECORDWITHMETHODS}
  TXTbsCertificate = record
  {$else}
  TXTbsCertificate = object
  {$endif USERECORDWITHMETHODS}
  private
    fSafe: TLightLock;
    fCachedDer: RawByteString; // for ToDer
    procedure ComputeCachedDer;
    procedure ComputeCertUsages;
    procedure AddNextExtensions(pos: integer; const der: TAsnObject);
    function ComputeExtensions: TAsnObject;
    procedure Generate(ExpireDays, ValidDays: integer);
  public
    /// describes the version of the encoded X.509 Certificate
    // - equals usually 3, once extensions are used
    Version: integer;
    /// raw binary of a positive integer assigned by the CA to each certificate
    // - maps PBigInt.Save binary serialization
    // - use SerialNumberHex/SerialNumberText functions for human readable text
    SerialNumber: RawByteString;
    /// the cryptographic algorithm used by the CA over the TX509.Signed field
    // - match TX509.SignatureAlgorithm field
    Signature: TXSignatureAlgorithm;
    /// identifies the entity that has signed and issued the certificate
    Issuer: TXName;
    /// date on which the certificate validity period begins
    NotBefore: TDateTime;
    /// date on which the certificate validity period ends
    // - may equal 0 if '99991231235959Z' was stored as "unspecified end date"
    // (see RFC 5280 #4.1.2.5)
    NotAfter: TDateTime;
    /// identifies the entity associated with the public key stored in the
    // subject public key field of this certificate
    Subject: TXName;
    /// decoded AlgorithmIdentifier structure of the stored public key
    SubjectPublicKeyAlgorithm: TXPublicKeyAlgorithm;
    /// decoded number of bits of the stored public key
    // - typically 2048 for RSA, or 256 for ECC
    SubjectPublicKeyBits: integer;
    /// public key raw binary
    SubjectPublicKey: RawByteString;
    /// decoded extensions as defined for X.509 v3 certificates
    // - will contain the ready-to-use UTF-8 text of the value
    // - some types are not fully decoded, so you may need to use ExtensionRaw[]
    Extension: TXExtensions;
    /// if a decoded extension was marked as Critical
    ExtensionCritical: array[TXExtension] of boolean;
    /// raw ASN1_OCTSTR of decoded Extension[] after FromDer()
    ExtensionRaw: array[TXExtension] of RawByteString;
    /// unsupported extensions as defined for X.509 v3 certificates
    ExtensionOther: TXOthers;
    /// declared X.509 v3 certificate Key Usages from extensions
    KeyUsages: TXKeyUsages;
    /// declared X.509 v3 certificate Extended Key Usages from extensions
    ExtendedKeyUsages: TXExtendedKeyUsages;
    /// declared X.509 v3 certificate Usages in mormot.crypt.secure form
    // - aggregate KeyUsages and ExtendedKeyUsages X.509 fields with
    // cuCA from Extension[xeBasicConstraints]
    CertUsages: TCryptCertUsages;
    /// hexadecimal of a positive integer assigned by the CA to each certificate
    // - e.g. '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c'
    function SerialNumberHex: RawUtf8;
    /// decimal text of a positive integer assigned by the CA to each certificate
    // - e.g. '330929475774275458452528262248458246563660'
    function SerialNumberText: RawUtf8;
    /// convert Extension[x] from CSV to an array of RawUtf8
    function ExtensionArray(x: TXExtension): TRawUtf8DynArray;
    /// reset all internal context
    procedure Clear;
    /// serialize those fields into ASN.1 DER binary
    function ToDer: TAsnObject;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.2 encoding
    function FromDer(const der: TCertDer): boolean;
    /// to be called once any field has been changed to refresh ToDer cache
    // - following RFC 5280 #4.1.2 encoding
    procedure AfterModified;
  end;

  /// pointer to a X.509 Certificate fields, as stored in TX509.Signed
  PXTbsCertificate = ^TXTbsCertificate;

  /// a X.509 signed Certificate, as defined in RFC 5280
  TX509 = class(TSynPersistent)
  protected
    fSafe: TLightLock;
    fCachedDer: RawByteString;
    fCachedSha1: RawUtf8;
    fCachedPeerInfo: RawUtf8;
    fSignatureValue: RawByteString;
    fSignatureAlgorithm: TXSignatureAlgorithm;
    fPublicKey: TXPublicKey;
    procedure ComputeCachedDer;
    procedure ComputeCachedPeerInfo;
    function GetSerialNumber: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetIssuerDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectPublicKeyAlgorithm: RawUtf8;
  public
    /// actual to-be-signed Certificate content
    Signed: TXTbsCertificate;
    /// raw binary digital signature computed upon Signed.ToDer
    property SignatureValue: RawByteString
      read fSignatureValue;
  public
    /// finalize this instance
    destructor Destroy; override;
    /// reset all internal context
    procedure Clear;
    /// verify the digital signature of this Certificate using a X.509 Authority
    // - some errors can be ignored, e.g. cvWrongUsage or cvDeprecatedAuthority
    // - certificate expiration date can be specified instead of current time
    // - this method is thread-safe
    function Verify(Authority: TX509 = nil; IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify the digital signature of a memory buffer using the SubjectPublicKey
    // of this Certificate
    // - some errors can be ignored, e.g. cvWrongUsage or cvDeprecatedAuthority
    // - certificate expiration date can be specified instead of current time
    // - this method is thread-safe
    function Verify(Sig, Data: pointer; SigLen, DataLen: integer;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// to be called once any field has been changed to refresh the Binary cache
    procedure AfterModified;
    /// serialize those fields into ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    // - value is cached internally after LoadFromDer() or re-computation
    function SaveToDer: TCertDer;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function LoadFromDer(const der: TCertDer): boolean;
    /// unserialize those fields from a PEM content
    // - will fallback and try as ASN.1 DER binary if content is not PEM
    function LoadFromPem(const pem: TCertPem): boolean;
    /// fill those fields from a Certificate Signing Request PEM or DER content
    function LoadFromCsr(const Csr: RawByteString): boolean;
    /// the lowercase hexa hash of the normalized Binary of this Certificate
    // - default hfSha1 value is cached internally so is efficient for lookup
    function FingerPrint(algo: THashAlgo = hfSha1): RawUtf8;
    /// check if the Certificate Issuer is also its Subject
    function IsSelfSigned: boolean;
    /// return the associated Public Key instance
    // - initialize it from stored Signed.SubjectPublicKey, if needed
    function PublicKey: TXPublicKey;
    /// compute the number of security bits of the digital signature
    // - e.g. 112 for RSA-2048, 128 for ECC-256
    function SignatureSecurityBits: integer;
    /// an array of (DNS) Subject names covered by this Certificate
    // - convert the Extension[xeSubjectAlternativeName] CSV as a RawUtf8 array
    function SubjectAlternativeNames: TRawUtf8DynArray;
    /// return some multi-line text of the main information of this Certificate
    // - in a layout similar to X509_print() OpenSSL usual formatting
    // - is cached internally for efficiency
    function PeerInfo: RawUtf8;
    /// return the main information of this Certificate into
    procedure ToParsedInfo(out Info: TX509Parsed);
    /// main properties of the entity associated with the public key stored
    // in this certificate
    // - e.g. for an Internet certificate, Subject[xaCN] is 'synopse.info'
    property Subject: TXAttrNames
      read Signed.Subject.Name;
    /// main properties of the entity that has signed and issued the certificate
    // - e.g. for an Internet certificate, Issuer[xaO] may be 'Let''s Encrypt'
    property Issuer: TXAttrNames
      read Signed.Issuer.Name;
    /// main extensions as defined for X.509 v3 certificates
    // - will contain the ready-to-use UTF-8 CSV text of each value
    // - Extension[xeSubjectKeyIdentifier] and Extension[xeAuthorityKeyIdentifier]
    // are also useful to validate a full PKI certification paths and trust
    // - see also the SubjectAlternativeName property
    property Extension: TXExtensions
      read Signed.Extension;
  published
    /// hexadecimal of a positive integer assigned by the CA to each certificate
    // - e.g. '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c'
    property SerialNumber: RawUtf8
      read GetSerialNumber;
    /// issuer entity of this Certificate as Distinguished Name text
    // - e.g. 'CN=R3, C=US, O=Let''s Encrypt'
    property IssuerDN: RawUtf8
      read GetIssuerDN;
    /// date on which the certificate validity period begins
    property NotBefore: TDateTime
      read Signed.NotBefore;
    /// date on which the certificate validity period ends
    // - may equal 0 for an "unspecified end date" as per RFC 5280
    property NotAfter: TDateTime
      read Signed.NotAfter;
    /// declared X.509 v3 certificate Usages in mormot.crypt.secure form
    // - aggregate KeyUsages and ExtendedKeyUsages X.509 fields
    // - e.g. for an Internet certificate, is typically
    // ! [cuDigitalSignature, cuKeyEncipherment, cuTlsServer, cuTlsClient]
    property Usages: TCryptCertUsages
      read Signed.CertUsages;
    /// subject entity of this Certificate as Distinguished Name text
    // - e.g. 'CN=synopse.info'
    property SubjectDN: RawUtf8
      read GetSubjectDN;
    /// Subject names covered by this Certificate, as CSV
    // - e.g. for an Internet certificate is 'synopse.info,www.synopse.info'
    property SubjectAlternativeName: RawUtf8
      read Signed.Extension[xeSubjectAlternativeName];
    /// the cryptographic algorithm used by the stored public key
    property SubjectPublicKeyAlgorithm: RawUtf8
      read GetSubjectPublicKeyAlgorithm;
    /// the cryptographic algorithm used by the CA over the Signed field
    // - match Signed.Signature internal field
    property SignatureAlgorithm: TXSignatureAlgorithm
      read fSignatureAlgorithm;
  end;


{ **************** Registration of our X.509 Engine to the TCryptCert Factory }

/// high-level function to decode X.509 certificate main properties using TX509
// - assigned to mormot.core.secure X509Parse() redirection by this unit
function TX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;


implementation


{ **************** X.509 Fields Logic}

procedure AddOther(var others: TXOthers; const o, v: RawByteString);
var
  n: PtrInt;
begin
  n := length(others);
  SetLength(others, n + 1);
  with others[n] do
  begin
    Oid := o;
    Value := v;
  end;
end;

function FindOther(const Other: TXOthers; const OidText: RawUtf8): RawByteString;
begin
  result := FindOtherAsn(pointer(Other), length(Other), AsnEncOid(pointer(OidText)));
end;

function FindOtherAsn(o: PXOther; n: integer; const OidBinary: TAsnObject): RawByteString;
begin
  result := '';
  if (o <> nil) and
     (n > 0) and
     (OidBinary <> '') then
    repeat
      if CompareBuf(o^.Oid, OidBinary) then // efficient O(n) search
      begin
        result := o^.Value;
        exit;
      end;
      inc(o);
      dec(n);
    until n = 0;
end;

function ToText(a: TXAttr): PShortString;
begin
  result := GetEnumName(TypeInfo(TXAttr), ord(a));
end;

function ToText(e: TXExtension): PShortString;
begin
  result := GetEnumName(TypeInfo(TXExtension), ord(e));
end;

function ToText(u: TXKeyUsage): PShortString;
begin
  result := GetEnumName(TypeInfo(TXKeyUsage), ord(u));
end;

function ToText(x: TXExtendedKeyUsage): PShortString;
begin
  result := GetEnumName(TypeInfo(TXExtendedKeyUsage), ord(x));
end;

function ToText(a: TXSignatureAlgorithm): PShortString;
begin
  result := GetEnumName(TypeInfo(TXSignatureAlgorithm), ord(a));
end;

function ToText(a: TXPublicKeyAlgorithm): PShortString;
begin
  result := GetEnumName(TypeInfo(TXPublicKeyAlgorithm), ord(a));
end;

function TextToXa(const Rdn: RawUtf8; out Xa: TXAttr): boolean;
var
  i: integer;
begin
  i := GetEnumNameValueTrimmed(TypeInfo(TXAttr), pointer(Rdn), length(Rdn));
  if i <= 0 then
    result := false
  else
  begin
    result := true;
    Xa := TXAttr(i);
  end;
end;


function XsaToSeq(xsa: TXSignatureAlgorithm): TAsnObject;
begin
  case xsa of
    xsaSha256Rsa .. xsaSha512Rsa:
      result := AsnSeq([
                  AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                  ASN1_NULL_VALUE // optional parameters
                ]);
    xsaSha256RsaPss .. xsaSha512RsaPss:
      // ASN1_OID_SIGNATURE[xsa] is the hash algorithm for RSA-PSS
      result :=
        AsnSeq([
          AsnOid(ASN1_OID_PKCS1_RSA_PSS),
          AsnSeq([ // RSASSA-PSS-params - see RFC 8017 A.2.3
            Asn(ASN1_CTC0, [AsnSeq([ // HashAlgorithm
                              AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                              ASN1_NULL_VALUE
                              ])
                           ]),
            Asn(ASN1_CTC1, [AsnSeq([ // MaskGenAlgorithm
                              AsnOid(ASN1_OID_PKCS1_MGF),
                              AsnSeq([
                                AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                                ASN1_NULL_VALUE
                              ])
                            ])
                           ]),
            Asn(ASN1_CTC2, [Asn(HASH_SIZE[XSA_TO_HF[xsa]])]) // saltLength
          ])
        ]);
    xsaSha256Ecc256:
      result := AsnSeq([
                  AsnOid(pointer(ASN1_OID_SIGNATURE[xsa]))
                ]);
  else
    raise EX509.CreateUtf8('Unexpected XsaToSeq(%)', [ord(xsa)]);
  end;
end;

function XkaToSeq(xka: TXPublicKeyAlgorithm): RawByteString;
begin
  case xka of
    xkaRsa:
      result := AsnSeq([
                  AsnOid(ASN1_OID_PKCS1_RSA),
                  ASN1_NULL_VALUE // optional
                ]);
    xkaRsaPss:
      result := AsnSeq([
                  AsnOid(ASN1_OID_PKCS1_RSA_PSS)
                ]);
    xkaEcc256:
      result := AsnSeq([
                  AsnOid(ASN1_OID_X962_PUBLICKEY),
                  AsnOid(ASN1_OID_X962_ECDSA_P256)
                ]);
  else
    raise EX509.CreateUtf8('Unexpected XkaToSeq(%)', [ord(xka)]);
  end;
end;

function AsnNextAlgoOid(var pos: integer; const der: TAsnObject;
  out oid, oid2: RawByteString): boolean;
var
  seq: RawByteString;
  p: integer;
begin
  p := 1;
  result := (AsnNextRaw(pos, der, seq) = ASN1_SEQ) and
            (AsnNext(p, seq, @oid) = ASN1_OBJID) and // decode OID as text
            ({%H-}oid <> '');
  if result then
    case AsnNext(p, seq, @oid2) of
      ASN1_OBJID:
        ; // e.g. xkaEcc256 or xsaSha256Ecc256 will check oid2 = ECDSA_P256
      ASN1_SEQ:
        // e.g. for xsaSha256RsaPss
        if (AsnNext(p, seq) <> ASN1_CTC0) or
           (AsnNext(p, seq) <> ASN1_SEQ) or
           (AsnNext(p, seq, @oid2) <> ASN1_OBJID) then
          oid2 := ''
        else
          // ASN1_OID_SIGNATURE[xsa] is the hash algorithm for RSA-PSS
          oid := oid2;
      // TODO: support non-standard saltLength as generated by OpenSSL :(
      // OpenSSL puts e.g. saltLength=222 for Sha256 but FIPS 185-4 §5.5 states
      // it should not be bigger than SizeOf(Sha256)=32 -> keep it standard
    else
      oid2 := ''; // e.g. ASN1_NULL for xkaRsa/xkaRsaPss or xsaSha256Rsa
    end;
end;

function OidToXsa(const oid: RawUtf8; out xsa: TXSignatureAlgorithm): boolean;
var
  x: TXSignatureAlgorithm;
begin
  for x := succ(low(x)) to high(x) do
    if CompareBuf(oid, ASN1_OID_SIGNATURE[x]) then
    begin
      xsa := x;
      result := true;
      exit;
    end;
  result := false;
end;

function OidToXka(const oid, oid2: RawUtf8; out xka: TXPublicKeyAlgorithm): boolean;
begin
  result := true;
  if oid = ASN1_OID_PKCS1_RSA then
    xka := xkaRsa
  else if oid = ASN1_OID_PKCS1_RSA_PSS then
    xka := xkaRsaPss
  else if (oid = ASN1_OID_X962_PUBLICKEY) and
          (oid2 = ASN1_OID_X962_ECDSA_P256) then
    xka := xkaEcc256
  else
    result := false;
end;

var
  // fast OID binary comparison search - initialized at unit startup
  XA_OID_ASN: array[TXAttr] of TAsnObject;
  XE_OID_ASN: array[TXExtension] of TAsnObject;
  XKU_OID_ASN: array[TXExtendedKeyUsage] of TAsnObject;

function OidToXa(const oid: RawByteString): TXAttr;
begin
  for result := succ(low(result)) to high(result) do
    if CompareBuf(oid, XA_OID_ASN[result]) then
      exit;
  result := xaNone;
end;

function OidToXe(const oid: RawByteString): TXExtension;
begin
  for result := succ(low(result)) to high(result) do
    if CompareBuf(oid, XE_OID_ASN[result]) then
      exit;
  result := xeNone;
end;

function OidToXku(const oid: RawByteString): TXExtendedKeyUsage;
begin
  for result := succ(low(result)) to high(result) do
    if CompareBuf(oid, XKU_OID_ASN[result]) then
      exit;
  result := xkuNone;
end;

function XkuToOids(usages: TXExtendedKeyUsages): RawByteString;
var
  xku: TXExtendedKeyUsage;
begin
  result := '';
  for xku := succ(low(xku)) to high(xku) do
    if xku in usages then
      Append(result, Asn(ASN1_OBJID, [XKU_OID_ASN[xku]]));
end;

function KuToBitStr(usages: TXKeyUsages): RawByteString;
begin
  // return expected ASN1_BITSTR value for xeKeyUsage
  FastSetRawByteString(result, @usages, 2);
  if xuDecipherOnly in usages then
    result[2] := #$80
  else
    FakeLength(result, 1);
end;


{ TX509Name }

procedure TXName.ComputeAsn;
var
  a: TXAttr;
  p: PUtf8Char;
  o: PtrInt;
  v: RawUtf8;
  tmp, one: RawByteString;
begin
  fSafe.Lock;
  try
    if fCachedAsn = '' then
    begin
      for a := succ(low(a)) to high(a) do
      begin
        p := pointer(Name[a]);
        if p <> nil then
        begin
          one := '';
          repeat
            GetNextItemTrimed(p, ',', v);
            Append(one, AsnSeq([
                          Asn(ASN1_OBJID, [XA_OID_ASN[a]]),
                          AsnText(v)
                        ]));
          until p = nil;
          Append(tmp, Asn(ASN1_SETOF, [one]));
        end;
      end;
      for o := 0 to high(Other) do
        with Other[o] do
          Append(tmp, Asn(ASN1_SETOF, [
                        AsnSeq([
                          Asn(ASN1_OBJID, [Oid]),
                          AsnText(Value)
                        ])
                      ]));
      fCachedAsn := AsnSeq(tmp);
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TXName.ToBinary: RawByteString;
begin
  if fCachedAsn = '' then
    ComputeAsn;
  result := fCachedAsn;
end;

procedure TXName.ComputeText;
var
  tmp: TTextWriterStackBuffer;
  a: TXAttr;
  first: boolean;
  p: PUtf8Char;
  n, v: shortstring;
begin
  fSafe.Lock;
  try
    if fCachedText = '' then
      with TTextWriter.CreateOwnedStream(tmp) do
      try
        first := true;
        for a := succ(low(a)) to high(a) do
        begin
          p := pointer(Name[a]);
          if p <> nil then
          begin
            TrimLeftLowerCaseToShort(ToText(a), n);
            repeat
              GetNextItemShortString(p, @v);
              if v[0] <> #0 then
              begin
                if first then
                  first := false
                else
                  Add(',', ' ');
                AddShort(n);
                Add('=');
                AddShort(v);
              end;
            until p = nil;
          end;
        end;
        SetText(fCachedText);
      finally
        Free;
      end;
  finally
    fSafe.UnLock;
  end;
end;

function TXName.NameArray(a: TXAttr): TRawUtf8DynArray;
begin
  result := nil;
  CsvToRawUtf8DynArray(pointer(Name[a]), result);
end;

function TXName.AsDNText: RawUtf8;
begin
  if fCachedText = '' then
    ComputeText;
  result := fCachedText;
end;

function TXName.FromAsn(const seq: TAsnObject): boolean;
var
  posseq, posone: integer;
  xa: TXAttr;
  one, oid, v: RawByteString;
begin
  result := false;
  fCachedAsn := seq; // store exact binary since used for comparison
  fCachedText := '';
  posseq := 1;
  if AsnNext(posseq, seq) <> ASN1_SEQ then
    exit;
  while AsnNextRaw(posseq, seq, one) = ASN1_SETOF do
  begin
    posone := 1;
    while AsnNext(posone, one) = ASN1_SEQ do
      if (AsnNextRaw(posone, one, oid) <> ASN1_OBJID) or
         (oid = '') or
         not (AsnNext(posone, one, @v) in ASN1_TEXT) or
         not IsValidUtf8(v) then
        exit
      else
      begin
        xa := OidToXa(oid);
        if xa = xaNone then
          // unsupported OID
          AddOther(Other, oid, v)
        else
          // known attribute
          AddToCsv(v, Name[xa]);
      end;
  end;
  result := true;
end;

function TXName.FromAsnNext(var pos: integer; const der: TAsnObject): boolean;
var
  seq: RawByteString;
begin
  result := (AsnNextRaw(pos, der, seq, {includeheader=}true) = ASN1_SEQ) and
            FromAsn(seq);
end;

procedure TXName.FromFields(const fields: TCryptCertFields);
begin
  Name[xaC]  := TrimU(fields.Country);
  Name[xaST] := TrimU(fields.State);
  Name[xaL]  := TrimU(fields.Locality);
  Name[xaO]  := TrimU(fields.Organization);
  Name[xaOU] := TrimU(fields.OrgUnit);
  Name[xaCN] := TrimU(fields.CommonName);
  Name[xaE]  := TrimU(fields.EmailAddress);
  Name[xaSN] := TrimU(fields.SurName);
  Name[xaGN] := TrimU(fields.GivenName);
end;

function TXName.Get(const Rdn: RawUtf8): RawUtf8;
var
  xa: TXAttr;
  h: THashAlgo;
begin
  if TextToXa(Rdn, xa) then
    result := Name[xa]
  else if TextToHashAlgo(Rdn, h) then
    result := ToDigest(h)
  else
    result := FindOid(Rdn);
end;

procedure TXName.AfterModified;
begin
  fSafe.Lock;
  try
    fCachedAsn := '';
    fCachedText := '';
  finally
    fSafe.UnLock;
  end;
end;

function TXName.ToDigest(algo: THashAlgo): RawUtf8;
begin
  result := HashFull(algo, ToBinary);
end;

function TXName.FindOid(const oid: RawUtf8): RawUtf8;
var
  o: TAsnObject;
  xa: TXAttr;
begin
  result := '';
  o := AsnEncOid(pointer(Oid));
  if o = '' then
    exit;
  result := FindOtherAsn(pointer(Other), length(Other), o);
  if result <> '' then
    exit;
  xa := OidToXa(o);
  if xa <> xaNone then
    result := Name[xa];
end;


// some internal functions shared with X.509 Certificate and CSR

procedure CertInfoPrepare(var Subject: TXName; var Extension: TXExtensions;
  const Subjects: RawUtf8; Fields: PCryptCertFields);
var
  subs: TRawUtf8DynArray;
  sub: RawUtf8;
begin
  CsvToRawUtf8DynArray(pointer(Subjects), subs, ',', {trim=}true);
  if (subs = nil) and
     (Fields <> nil) then
    sub := TrimU(Fields^.CommonName) // like TCryptCertOpenSsl.Generate()
  else
    sub := RawUtf8ArrayToCsv(subs); // normalized
  Extension[xeSubjectAlternativeName] := sub;
  Subject.Name[xaCN] := GetCsvItem(pointer(sub), 0);
  if Fields <> nil then
  begin
    Subject.FromFields(Fields^);
    Extension[xeNetscapeComment] := Fields^.Comment;
  end;
end;

function HumanRandomID: RawUtf8;
var
  rnd: THash256;
begin
  RandomBytes(@rnd, SizeOf(rnd)); // Lecuyer is enough for public random
  rnd[0] := rnd[0] and $7f;     // ensure > 0
  ToHumanHex(result, @rnd, 20); // 20 bytes = 160-bit as a common size
end;

function CsvToDns(p: PUtf8Char): RawByteString;
begin
  result := '';
  while p <> nil do
    Append(result, Asn(ASN1_CTX2, [TrimU(GetNextItem(p))]));
end;

procedure AddExt(var result: TAsnObject; xe: TXExtension;
  const value: RawByteString; critical: boolean = false);
begin
  Append(result, AsnSeq([
                   Asn(ASN1_OBJID, [XE_OID_ASN[xe]]),
                   ASN1_BOOLEAN_NONE[critical],
                   Asn(ASN1_OCTSTR, [value])
                 ]));
end;

const
  KU: array[cuEncipherOnly .. cuDecipherOnly] of TXKeyUsage = (
    xuEncipherOnly,
    xuCrlSign,
    xuKeyCertSign,
    xuKeyAgreement,
    xuDataEncipherment,
    xuKeyEncipherment,
    xuNonRepudiation,
    xuDigitalsignature,
    xuDecipherOnly);

  XU: array[cuTlsServer .. cuTimestamp] of TXExtendedKeyUsage = (
    xkuServerAuth,
    xkuClientAuth,
    xkuEmailProtection,
    xkuCodeSigning,
    xkuOcspSigning,
    xkuTimeStamping);

function CertInfoCompute(usages: TCryptCertUsages; const ext: TXExtensions;
  out xku: TXKeyUsages; out xeku: TXExtendedKeyUsages): TAsnObject;
var
  r: TCryptCertUsage;
begin
  result := '';
  // high-level usages values are converted into usages and xku
  xku := [];
  for r := low(KU) to high(KU) do
    if r in usages then
      include(xku, KU[r]);
  xeku := [];
  for r := low(XU) to high(XU) do
    if r in usages then
      include(xeku, XU[r]);
  // RFC 5280 #4.2.1.9
  AddExt(result, xeBasicConstraints,
    AsnSeq(ASN1_BOOLEAN_NONE[cuCA in usages]), {critical=}true);
  // RFC 5280 #4.2.1.3
  if xku <> [] then
    AddExt(result, xeKeyUsage,
      Asn(ASN1_BITSTR, [KuToBitStr(xku)]), {critical=}true);
  // RFC 5280 #4.2.1.12
  if xeku <> [] then
    AddExt(result, xeExtendedKeyUsage,
      AsnSeq(XkuToOids(xeku)));
  // ext[] RawUtf8 are used as source
  // - ExtensionOther[] and ExtensionRaw[] are ignored
  // RFC 5280 #4.2.1.2
  if ext[xeSubjectKeyIdentifier] <> '' then
    AddExt(result, xeSubjectKeyIdentifier,
      Asn(ASN1_OCTSTR, [HumanHexToBin(ext[xeSubjectKeyIdentifier])]));
  // RFC 5280 #4.2.1.1
  if ext[xeAuthorityKeyIdentifier] <> '' then
    AddExt(result, xeAuthorityKeyIdentifier,
      AsnSeq(Asn(ASN1_CTX0, [HumanHexToBin(ext[xeAuthorityKeyIdentifier])])));
  // RFC 5280 #4.2.1.6
  if ext[xeSubjectAlternativeName] <> '' then
    AddExt(result, xeSubjectAlternativeName,
      AsnSeq(CsvToDns(pointer(ext[xeSubjectAlternativeName]))));
  // RFC 5280 #4.2.1.7
  if ext[xeIssuerAlternativeName] <> '' then
    AddExt(result, xeIssuerAlternativeName,
      AsnSeq(CsvToDns(pointer(ext[xeIssuerAlternativeName]))));
  // non-standard ext - but defined as TCryptCertFields.Comment
  if ext[xeNetscapeComment] <> '' then
    AddExt(result, xeNetscapeComment,
      Asn(ASN1_IA5STRING, [ext[xeNetscapeComment]]));
  // xeAuthorityInformationAccess and xeCertificatePolicies not yet persisted
end;


{ **************** RSA and ECC Public/Private Key support for X.509 }

{ TXPublicKey }

const
  XKA_RSA: array[TXPublicKeyAlgorithm] of TRsaClass = (
    nil,      // xkaNone
    TRsa,     // xkaRsa
    TRsaPss,  // xkaRsaPss
    nil  );   // xkaEcc256

function TXPublicKey.Load(Algorithm: TXPublicKeyAlgorithm;
  const SubjectPublicKey: RawByteString): boolean;
begin
  result := false;
  if (fAlgo <> xkaNone) or
     (SubjectPublicKey = '') then
    exit;
  fAlgo := Algorithm;
  case Algorithm of
    xkaRsa,
    xkaRsaPss:
      begin
        fRsa := XKA_RSA[Algorithm].Create;
        if fRsa.LoadFromPublicKeyDer(SubjectPublicKey) then
          result := true
        else
          FreeAndNil(fRsa);
      end;
    xkaEcc256:
      if Ecc256r1CompressAsn1(SubjectPublicKey, fEccPub) then
      begin
        fEcc := TEcc256r1Verify.Create(fEccPub); // OpenSSL or mormot.crypt
        fSubjectPublicKey := SubjectPublicKey;
        result := true;
      end;
  else
    raise EX509.CreateUtf8('%.Create: unsupported %', [self, ToText(fAlgo)^]);
  end;
end;

destructor TXPublicKey.Destroy;
begin
  inherited Destroy;
  fRsa.Free;
  fEcc.Free;
end;

function TXPublicKey.Verify(Sig: pointer; Dig: THash512Rec;
  SigLen, DigLen: integer; Hash: THashAlgo): boolean;
var
  eccsig: TEccSignature;
begin
  result := false;
  if (self <> nil) and
     (DigLen <> 0) then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        // RSA digital signature verification (thread-safe but blocking)
        result := fRsa.Verify(@Dig, Sig, Hash, SigLen);
      xkaEcc256:
        if DerToEcc(Sig, SigLen, eccsig) then
          // secp256r1 digital signature verification
          result := fEcc.Verify(Dig.Lo, eccsig); // thread-safe
    end;
end;

function TXPublicKey.Verify(DigAlgo: TXSignatureAlgorithm; Data, Sig: pointer;
  DataLen, SigLen: integer): boolean;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(XSA_TO_HF[DigAlgo], Data, DataLen, dig);
  result := (diglen <> 0) and
            Verify(Sig, dig, SigLen, diglen, XSA_TO_HF[DigAlgo]);
end;

function TXPublicKey.Verify(DigAlgo: TXSignatureAlgorithm;
  const Data, Sig: RawByteString): boolean;
begin
  result := Verify(DigAlgo, pointer(Data), pointer(Sig), length(Data), length(Sig));
end;

function TXPublicKey.GetParams(out x, y: RawByteString): boolean;
var
  k: TEccPublicKeyUncompressed;
begin
  result := false;
  if self <> nil then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        begin
          // for RSA, x is set to the Exponent (e), and y to the Modulus (n)
          x := fRsa.E^.Save;
          y := fRsa.M^.Save;
          result := (x <> '') and
                    (y <> '');
        end;
      xkaEcc256:
        // for ECC, returns the x,y uncompressed coordinates from stored ASN.1
        if Ecc256r1ExtractAsn1(fSubjectPublicKey, k) then
        begin
          FastSetRawByteString(x, nil, ECC_BYTES);;
          FastSetRawByteString(y, nil, ECC_BYTES);;
          bswap256(@PHash512Rec(@k)^.Lo, pointer(x));
          bswap256(@PHash512Rec(@k)^.Hi, pointer(y));
          result := true;
        end;
    end;
end;

function TXPublicKey.Seal(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  result  := '';
  if self <> nil then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        result := fRsa.Seal(Cipher, Message);
      xkaEcc256:
        result := EciesSeal(Cipher, fEcc.PublicKey, Message);
    end;
end;


{ TXPrivateKey }

const
  // those values match EccPrivateKeyEncrypt/EccPrivateKeyDecrypt for xkaEcc256
  // xkaRsa and xkaRsaPss share the same public/private key files by definition
  XKA_SALT: array[TXPublicKeyAlgorithm] of RawUtf8 = (
    '', 'synrsa', 'synrsa', 'synecc');
  XKA_ROUNDS: array[TXPublicKeyAlgorithm] of byte = (
    0, 3, 3, 31);

function TXPrivateKey.Load(Algorithm: TXPublicKeyAlgorithm; AssociatedKey: TXPublicKey;
  const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean;
var
  saved, der: RawByteString;
begin
  result := false;
  if (self = nil) or
     (fAlgo <> xkaNone) or
     (Algorithm = xkaNone) or
     (PrivateKeySaved = '') then
    exit;
  try
    saved := PrivateKeySaved;
    if Password <> '' then
    begin
      // use mormot.core.secure encryption, not standard PKCS#8
      der := PemToDer(saved); // see also TCryptCertX509.Load
      saved := PrivateKeyDecrypt(
        der, XKA_SALT[Algorithm], Password, XKA_ROUNDS[Algorithm]);
      if saved = '' then
        exit;
    end;
    fAlgo := Algorithm;
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        begin
          fRsa := XKA_RSA[fAlgo].Create;
          if fRsa.LoadFromPrivateKeyPem(saved) and
             ((AssociatedKey = nil) or
              fRsa.MatchKey(AssociatedKey.fRsa)) then
            result := true
          else
            FreeAndNil(fRsa);
        end;
      xkaEcc256:
        if PemDerRawToEcc(saved, fEcc) and
           ((AssociatedKey = nil) or
            Ecc256r1MatchKeys(fEcc, AssociatedKey.fEcc.PublicKey)) then
          result := true
        else
          FillZero(fEcc);
    end;
  finally
    FillZero(saved);
    FillZero(der);
  end;
end;

function TXPrivateKey.Generate(Algorithm: TXPublicKeyAlgorithm): RawByteString;
var
  eccpub: TEccPublicKey;
begin
  result := '';
  if (self = nil) or
     (fAlgo <> xkaNone) then
    exit;
  fAlgo := Algorithm;
  case fAlgo of
    xkaRsa,
    xkaRsaPss:
      if fRsa = nil then
      begin
        fRsa := XKA_RSA[fAlgo].Create;
        if fRsa.Generate(RSA_DEFAULT_GENERATION_BITS) then
          result := fRsa.SavePublicKey.ToSubjectPublicKey;
      end;
    xkaEcc256:
      if IsZero(fEcc) and
         Ecc256r1MakeKey(eccpub, fEcc) then
        result := Ecc256r1UncompressAsn1(eccpub);
  end;
end;

function TXPrivateKey.ComputeSelfSignedCsr(
  Algorithm: TXSignatureAlgorithm; const Subjects: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;
var
  pub, extreq, der: RawByteString;
  sub: TXName;
  ext: TXExtensions;
  xu: TXKeyUsages;
  xku: TXExtendedKeyUsages;
begin
  result := '';
  if self = nil then
    exit;
  // create a new key pair if needed
  if fAlgo <> xkaNone then
    pub := ToSubjectPublicKey
  else
    pub := Generate(XSA_TO_XKA[Algorithm]);
  if pub = '' then
    exit;
  // setup the CSR fields
  FillCharFast(sub, SizeOf(sub), 0);
  CertInfoPrepare(sub, ext, Subjects, Fields);
  extreq := CertInfoCompute(Usages, ext, xu, xku);
  if extreq <> '' then
    // extensionRequest (PKCS #9 via CRMF)
    extreq := Asn(ASN1_CTC0, [
                AsnSeq([
                  AsnOid(ASN1_OID_PKCS9_EXTREQ),
                  Asn(ASN1_SETOF, [
                    AsnSeq(extreq)
                  ])
                ])
              ]);
  // compute the main CSR body
  der := AsnSeq([
           Asn(0), // version
           sub.ToBinary,
           AsnSeq([
             XkaToSeq(fAlgo),
             Asn(ASN1_BITSTR, [pub])
           ]),
           extreq
         ]);
  // sign and return the whole CSR as PEM
  result := DerToPem(AsnSeq([
                      der,
                      XsaToSeq(Algorithm),
                      Asn(ASN1_BITSTR, [Sign(Algorithm, der)])
                    ]), pemCertificateRequest);
end;

destructor TXPrivateKey.Destroy;
begin
  inherited Destroy;
  FillZero(fEcc);
  fRsa.Free;
end;

function TXPrivateKey.ToDer: RawByteString;
begin
  if self = nil then
    result := ''
  else if fRsa <> nil then
    result := fRsa.SavePrivateKeyDer
  else
    result := EccToDer(fEcc); // does IsZero()
end;

function TXPrivateKey.ToSubjectPublicKey: RawByteString;
var
  eccpub: TEccPublicKey;
begin
  result := '';
  if self <> nil then
    if fRsa <> nil then
      result := fRsa.SavePublicKey.ToSubjectPublicKey
    else if not IsZero(fEcc) then
    begin
      Ecc256r1PublicFromPrivate(fEcc, eccpub);
      result := Ecc256r1UncompressAsn1(eccpub);
    end;
end;

function TXPrivateKey.Save(Format: TCryptCertFormat;
  const Password: SpiUtf8): RawByteString;
var
  der, bin: RawByteString;
  k: TPemKind;
begin
  if self = nil then
    result := ''
  else
  try
    der := ToDer;
    if Password = '' then
      // save as plain unencrypted PEM/DER
      if Format = ccfPem then
        if fAlgo in xkaRsas then
          k := pemRsaPrivateKey
        else
          k := pemEcPrivateKey
      else
        k := pemUnspecified // save as ccfBinary
    else
    begin
      // use mormot.core.secure encryption, not standard PKCS#8
      bin := der; // for FillZero()
      der := PrivateKeyEncrypt(bin, XKA_SALT[fAlgo], Password, XKA_ROUNDS[fAlgo]);
      if Format = ccfPem then
        if fAlgo in xkaRsas then
          k := pemSynopseRsaEncryptedPrivateKey
        else
          k := pemSynopseEccEncryptedPrivateKey
        else
          k := pemUnspecified;
    end;
    if k = pemUnspecified then
      result := der
    else
      result := DerToPem(der, k);
  finally
    FillZero(der);
    FillZero(bin);
  end;
end;

function TXPrivateKey.Sign(const Dig: THash512Rec; DigLen: integer;
  DigAlgo: TXSignatureAlgorithm): RawByteString;
var
  eccsig: TEccSignature;
begin
  result := '';
  if (self <> nil) and
     (XSA_TO_XKA[DigAlgo] = fAlgo) and
     (HASH_SIZE[XSA_TO_HF[DigAlgo]] = DigLen) then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        if fRsa <> nil then
          result := fRsa.Sign(@Dig.b, XSA_TO_HF[DigAlgo]); // thread-safe
      xkaEcc256:
        if Ecc256r1Sign(fEcc, Dig.Lo, eccsig) then // thread-safe
          result := EccToDer(eccsig);
    end;
end;

function TXPrivateKey.Sign(DigAlgo: TXSignatureAlgorithm; Data: pointer;
  DataLen: integer): RawByteString;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(XSA_TO_HF[DigAlgo], Data, DataLen, dig);
  result := Sign(dig, diglen, DigAlgo);
end;

function TXPrivateKey.Sign(DigAlgo: TXSignatureAlgorithm;
  const Data: RawByteString): RawByteString;
begin
  result := Sign(DigAlgo, pointer(Data), length(Data));
end;

function TXPrivateKey.Open(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  result := '';
  if self <> nil then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        result := fRsa.Open(Cipher, Message);
      xkaEcc256:
        result := EciesOpen(Cipher, fEcc, Message);
    end;
end;

function TXPrivateKey.SharedSecret(pub: TXPublicKey): RawByteString;
var
  sec: TEccSecretKey;
begin
  result := '';
  if (self <> nil) and
     (pub <> nil) and
     (pub.fAlgo = fAlgo) then
    case fAlgo of
      xkaRsa,
      xkaRsaPss:
        ; // not possible by definition
      xkaEcc256:
        try
          if Ecc256r1SharedSecret(pub.fEccPub, fEcc, sec) then
            FastSetRawByteString(result{%H-}, @sec, SizeOf(sec));
        finally
          FillZero(sec);
        end;
    end;
end;


{ **************** X.509 Certificates and CSR }

{ TXTbsCertificate }

function TXTbsCertificate.ComputeExtensions: TAsnObject;
begin
  result := CertInfoCompute(CertUsages, Extension, KeyUsages, ExtendedKeyUsages);
end;

procedure TXTbsCertificate.ComputeCachedDer;
var
  ext: RawByteString;
begin
  fSafe.Lock;
  try
    if fCachedDer = '' then
    begin
      if Version >= 3 then
        // compute the X.509 v3 extensions block
        ext := Asn(ASN1_CTC3, [
                 AsnSeq(ComputeExtensions)
               ]);
      fCachedDer := AsnSeq([
                      Asn(ASN1_CTC0, [{%H-}Asn(Version - 1)]),
                      Asn(ASN1_INT, [SerialNumber]),
                      XsaToSeq(Signature),
                      Issuer.ToBinary,
                      AsnSeq([
                        AsnTime(NotBefore),
                        AsnTime(NotAfter)
                      ]),
                      Subject.ToBinary,
                      AsnSeq([
                        XkaToSeq(SubjectPublicKeyAlgorithm),
                        Asn(ASN1_BITSTR, [SubjectPublicKey])
                      ]),
                      ext
                    ]);
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TXTbsCertificate.AddNextExtensions(pos: integer; const der: TAsnObject);
var
  ext, oid, seq, v: RawByteString;
  decoded: RawUtf8;
  vt, extpos, seqpos: integer;
  xe: TXExtension;
  xku: TXExtendedKeyUsage;
  critical: boolean;
  w: word;
begin
  while (AsnNext(pos, der) = ASN1_SEQ) and
        (AsnNextRaw(pos, der, oid) = ASN1_OBJID) do
  begin
    // loop for each X.509 v3 extension
    critical := false;
    vt := AsnNextRaw(pos, der, ext);
    if vt = ASN1_BOOL then // optional Critical flag
    begin
      critical := ext = #$ff;
      vt := AsnNextRaw(pos, der, ext);
    end;
    if vt <> ASN1_OCTSTR then // extnValue
      exit;
    xe := OidToXe(oid);
    if xe = xeNone then
      // unsupported OID are stored as raw binary values
      AddOther(ExtensionOther, oid, ext)
    else
    begin
      // decode most common extensions as RawUtf8
      ExtensionCritical[xe] := critical;
      ExtensionRaw[xe] := ext;
      decoded := '';
      extpos := 1;
      case xe of
        xeAuthorityKeyIdentifier:    // RFC 5280 #4.2.1.1
          if (AsnNext(extpos, ext) = ASN1_SEQ) and
             (AsnNextRaw(extpos, ext, v) <> ASN1_NULL) then
            ToHumanHex(decoded, pointer(v), length(v));
        xeSubjectKeyIdentifier:       // RFC 5280 #4.2.1.2
          if AsnNextRaw(extpos, ext, v) = ASN1_OCTSTR then
            ToHumanHex(decoded, pointer(v), length(v));
        xeSubjectAlternativeName,    // RFC 5280 #4.2.1.6
        xeIssuerAlternativeName:     // RFC 5280 #4.2.1.7
          if AsnNext(extpos, ext) = ASN1_SEQ then
            repeat
              case AsnNextRaw(extpos, ext, v) of
                ASN1_NULL:
                  break;
                ASN1_CTX1, // rfc8722Name
                ASN1_CTX2, // dnsName
                ASN1_CTX6: // uri
                  EnsureRawUtf8(v); // was stored as IA5String
                ASN1_CTX7: // ip
                  v := AsnDecIp(pointer(v), length(v));
                ASN1_CTX8: // registeredID
                  v := AsnDecOid(1, 1 + length(v), v);
              else
                continue;  // unsupported value type
              end;
              if v <> '' then
                AddToCsv(v, decoded);
            until false;
        xeBasicConstraints:          // RFC 5280 #4.2.1.9
          if (AsnNext(extpos, ext) = ASN1_SEQ) and
             (AsnNextRaw(extpos, ext, v) = ASN1_BOOL) and
             (v = #$ff) then
            decoded := 'CA'; // as expected by cuCA usage flag
        xeKeyUsage:                  // RFC 5280 #4.2.1.3
          if (AsnNextRaw(extpos, ext, v) = ASN1_BITSTR) and
             (v <> '') and
             (length(v) <= 2) then
          begin
            w := PWord(v)^; // length=1 ends with a #0
            KeyUsages := TXKeyUsages(w and $ff);
            if w and $8000 <> 0 then
              include(KeyUsages, xuDecipherOnly);
          end;
        xeExtendedKeyUsage:          // RFC 5280 #4.2.1.12
          if AsnNext(extpos, ext) = ASN1_SEQ then
            while AsnNextRaw(extpos, ext, oid) = ASN1_OBJID do
            begin
              xku := OidToXku(oid);
              if xku <> xkuNone then
                include(ExtendedKeyUsages, xku);
            end;
        xeAuthorityInformationAccess: // RFC 5280 #4.2.2.1
          // e.g. 'ocsp=http://r3.o.lencr.org,caIssuers=http://r3.i.lencr.org/'
          if AsnNext(extpos, ext) = ASN1_SEQ then
            while (AsnNext(extpos, ext) = ASN1_SEQ) and
                  (AsnNext(extpos, ext, @oid) = ASN1_OBJID) and
                  (AsnNext(extpos, ext, @v) = ASN1_CTX6) do
            begin
              if oid = '1.3.6.1.5.5.7.48.1' then
                Prepend(v, 'ocsp=')
              else if oid = '1.3.6.1.5.5.7.48.2' then
                Prepend(v, 'caIssuers=')
              else
                continue;
              EnsureRawUtf8(v);
              AddToCsv(v, decoded);
            end;
        xeCertificatePolicies:      // RFC 5280 #4.2.1.4
          if AsnNext(extpos, ext) = ASN1_SEQ then
          begin
            while AsnNextRaw(extpos, ext, seq) = ASN1_SEQ do
            begin
              seqpos := 1;
              if AsnNext(seqpos, seq, @oid) = ASN1_OBJID then
                AddToCsv(oid, decoded);
            end;
          end;
        xeNetscapeComment:
          if AsnNext(extpos, ext, @v) in ASN1_TEXT then // typically IA5String
            decoded := v;
      end;
      if decoded <> '' then
        Extension[xe] := decoded;
    end;
  end;
  ComputeCertUsages;
end;

procedure TXTbsCertificate.ComputeCertUsages;
var
  c: TCryptCertUsages;
  r: TCryptCertUsage;
  u: TXKeyUsages;
  x: TXExtendedKeyUsages;
begin
  c := [];
  if Extension[xeBasicConstraints] = 'CA' then
    include(c, cuCA);
  u := KeyUsages;
  if u <> [] then
    for r := low(KU) to high(KU) do
      if KU[r] in u then
        include(c, r);
  x := ExtendedKeyUsages;
  if x <> [] then
    for r := low(XU) to high(XU) do
      if XU[r] in x then
        include(c, r);
  CertUsages := c;
end;

procedure TXTbsCertificate.Generate(ExpireDays, ValidDays: integer);
var
  start: TDateTime;
begin
  // fill the main certificate fields
  Version := 3;
  SerialNumber := HumanHexToBin(HumanRandomID);
  Extension[xeSubjectKeyIdentifier] := HumanRandomID;
  // ValidDays and ExpireDays are relative to the current time
  start := NowUtc;
  NotBefore := start + ValidDays;
  NotAfter := start + ExpireDays;
end;

function TXTbsCertificate.SerialNumberHex: RawUtf8;
begin
  ToHumanHex(result, pointer(SerialNumber), length(SerialNumber));
end;

function TXTbsCertificate.SerialNumberText: RawUtf8;
begin
  result := BigIntToText(SerialNumber);
end;

function TXTbsCertificate.ExtensionArray(x: TXExtension): TRawUtf8DynArray;
begin
  result := nil;
  CsvToRawUtf8DynArray(pointer(Extension[x]), result);
end;

function TXTbsCertificate.ToDer: TAsnObject;
begin
  if fCachedDer = '' then
    ComputeCachedDer;
  result := fCachedDer;
end;

function TXTbsCertificate.FromDer(const der: TCertDer): boolean;
var
  pos, vt: integer;
  oid, oid2: RawByteString;
begin
  result := false;
  Clear;
  fCachedDer := der;
  // read main X.509 tbsCertificate fields
  pos := 1;
  if (AsnNext(pos, der) <> ASN1_SEQ) or
     (AsnNext(pos, der) <> ASN1_CTC0) then
    exit;
  Version := AsnNextInteger(pos, der, vt) + 1;
  if (vt <> ASN1_INT) or
     not (Version in [2..3]) or
     (AsnNextRaw(pos, der, SerialNumber) <> ASN1_INT) or
     not AsnNextAlgoOid(pos, der, oid, oid2) or
     not OidToXsa(oid, Signature) or
     not Issuer.FromAsnNext(pos, der) or
     (AsnNext(pos, der) <> ASN1_SEQ) or // validity
     not AsnNextTime(pos, der, NotBefore) or
     not AsnNextTime(pos, der, NotAfter) or
     not Subject.FromAsnNext(pos, der) or
     (AsnNext(pos, der) <> ASN1_SEQ) or // subjectPublicKeyInfo
     not AsnNextAlgoOid(pos, der, oid, oid2) or
     not OidToXka(oid, oid2, SubjectPublicKeyAlgorithm) or
     (AsnNextRaw(pos, der, SubjectPublicKey) <> ASN1_BITSTR) then
    exit;
  SubjectPublicKeyBits := X509PubKeyBits(SubjectPublicKey);
  // handle X.509 v3 extensions
  if (Version = 3) and
     (AsnNext(pos, der) = ASN1_CTC3) and
     (AsnNext(pos, der) = ASN1_SEQ) then
    AddNextExtensions(pos, der);
  result := true;
end;

procedure TXTbsCertificate.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

procedure TXTbsCertificate.AfterModified;
begin
  fSafe.Lock;
  try
    fCachedDer := ''; // reset the cache
  finally
    fSafe.UnLock;
  end;
  Subject.AfterModified;
  Issuer.AfterModified;
end;


{ TX509 }

destructor TX509.Destroy;
begin
  inherited Destroy;
  fPublicKey.Free;
end;

procedure TX509.Clear;
begin
  fCachedDer := '';
  fCachedSha1 := '';
  fCachedPeerInfo := '';
  Signed.Clear;
  fSignatureAlgorithm := xsaNone;
  fSignatureValue := '';
  FreeAndNil(fPublicKey);
end;

const
  DEPRECATION_THRESHOLD = 0.5; // allow a half day margin

function CanVerify(auth: TX509; usage: TCryptCertUsage; selfsigned: boolean;
  ignored: TCryptCertValidities; timeutc: TDateTime): TCryptCertValidity;
var
  na, nb: TDateTime;
begin
  if auth = nil then
    result := cvUnknownAuthority
  else if (not (cvWrongUsage in ignored)) and
          (not (selfsigned or (usage in auth.Signed.CertUsages))) then
    result := cvWrongUsage
  else
  begin
    result := cvValidSigned;
    if cvDeprecatedAuthority in ignored then
      exit;
    if timeutc = 0 then
      timeutc := NowUtc;
    na := auth.Signed.NotAfter; // 0 if was not specified in X.509 cert
    nb := auth.Signed.NotBefore;
    if ((na <> 0) and
        (timeutc > na + DEPRECATION_THRESHOLD)) or
       ((nb <> 0) and
        (timeutc < nb - DEPRECATION_THRESHOLD)) then
      result := cvDeprecatedAuthority;
  end;
end;

function TX509.Verify(Authority: TX509; IgnoreError: TCryptCertValidities;
  TimeUtc: TDateTime): TCryptCertValidity;
begin
   result := cvBadParameter;
   if self = nil then
     exit;
   if IsSelfSigned then
     Authority := self
   else if Authority <> nil then
   begin
     result := cvInvalidSignature;
     if (SignatureAlgorithm = xsaNone) or
        (Authority.Signed.SubjectPublicKeyAlgorithm <>
           XSA_TO_XKA[SignatureAlgorithm]) then
       exit;
     result := cvUnknownAuthority;
     if (Authority.Signed.Extension[xeSubjectKeyIdentifier] <>
          Signed.Extension[xeAuthorityKeyIdentifier]) or
        (Authority.Signed.SubjectPublicKey = '') then
       exit;
   end;
   result := CanVerify(
     Authority, cuKeyCertSign, Authority = self, IgnoreError, TimeUtc);
   if result = cvValidSigned then
     if not Authority.PublicKey.Verify(
              SignatureAlgorithm, Signed.ToDer, SignatureValue) then
       result := cvInvalidSignature
     else if Authority = self then
       result := cvValidSelfSigned;
end;

function TX509.Verify(Sig, Data: pointer; SigLen, DataLen: integer;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := cvBadParameter;
  if (self = nil) or
     (SigLen <= 0) or
     (DataLen <= 0) or
     (PublicKey = nil) then
    exit;
  result := CanVerify(self, cuDigitalSignature, false, IgnoreError, TimeUtc);
  if result = cvValidSigned then
    if fPublicKey.Verify(SignatureAlgorithm, Data, Sig, DataLen, SigLen) then
      if IsSelfSigned then
        result := cvValidSelfSigned
      else
        result := cvValidSigned
    else
      result := cvInvalidSignature;
end;

function TX509.PublicKey: TXPublicKey;
begin
  result := nil;
  if self = nil then
    exit;
  result := fPublicKey;
  if result <> nil then
    exit;
  if (Signed.SubjectPublicKey = '') or
     (Signed.SubjectPublicKeyAlgorithm = xkaNone) then
    exit;
  fSafe.Lock;
  try
    if fPublicKey = nil then
    begin
      fPublicKey := TXPublicKey.Create;
      if fPublicKey.Load(Signed.SubjectPublicKeyAlgorithm,
          Signed.SubjectPublicKey) then
        result := fPublicKey
      else
        FreeAndNil(fPublicKey);
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TX509.ToParsedInfo(out Info: TX509Parsed);
begin
  Info.Serial := SerialNumber;
  Info.SubjectDN := SubjectDN;
  Info.IssuerDN := IssuerDN;
  Info.SubjectID := Extension[xeSubjectKeyIdentifier];
  Info.IssuerID := Extension[xeAuthorityKeyIdentifier];
  Info.SubjectAltNames := StringReplaceAll(
    Extension[xeSubjectAlternativeName], ',', ', '); // more human friendly
  Info.SigAlg := XSA_TXT[SignatureAlgorithm];
  Info.PubAlg := GetSubjectPublicKeyAlgorithm;
  Info.Usage := Usages;
  Info.NotBefore := NotBefore;
  Info.NotAfter := NotAfter;
  Info.PubKey := Signed.SubjectPublicKey;
  Info.PeerInfo := ParsedToText(Info); // should be the last
end;

function TX509.SaveToDer: TCertDer;
begin
  if fCachedDer = '' then
    ComputeCachedDer;
  result := fCachedDer;
end;

function TX509.GetSerialNumber: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := Signed.SerialNumberHex;
end;

function TX509.GetIssuerDN: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := Signed.Issuer.AsDNText;
end;

function TX509.GetSubjectDN: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := Signed.Subject.AsDNText;
end;

function TX509.GetSubjectPublicKeyAlgorithm: RawUtf8;
begin
  FormatUtf8('%-bit %', [Signed.SubjectPublicKeyBits,
    XKA_TXT[Signed.SubjectPublicKeyAlgorithm]], result);
end;

procedure TX509.ComputeCachedDer;
begin
  if (SignatureAlgorithm = xsaNone) or
     (SignatureValue = '') then
    raise EX509.Create('TX509.ToDer with no previous Sign() call');
  fSafe.Lock;
  try
    if fCachedDer = '' then
    begin
      fCachedSha1 := '';
      fCachedPeerInfo := '';
      fCachedDer := AsnSeq([
                      Signed.ToDer,
                      XsaToSeq(SignatureAlgorithm),
                      Asn(ASN1_BITSTR, [SignatureValue])
                    ]);
    end;
  finally
    fSafe.UnLock;
  end;
end;

procedure TX509.ComputeCachedPeerInfo;
var
  info: TX509Parsed;
begin
  ToParsedInfo(info);
  fCachedPeerInfo := info.PeerInfo;
end;

function TX509.LoadFromDer(const der: TCertDer): boolean;
var
  pos: integer;
  tbs, oid, oid2: RawByteString;
begin
  Clear;
  fCachedDer := der;
  pos := 1;
  result := (der <> '') and
            (AsnNext(pos, der) = ASN1_SEQ) and
            (AsnNextRaw(pos, der, tbs, {includeheader=}true) = ASN1_SEQ) and
            Signed.FromDer(tbs) and
            AsnNextAlgoOid(pos, der, oid, oid2) and
            OidToXsa(oid, fSignatureAlgorithm) and
            (AsnNextRaw(pos, der, fSignatureValue) = ASN1_BITSTR) and
            (fSignatureAlgorithm = Signed.Signature);
end;

function TX509.LoadFromPem(const pem: TCertPem): boolean;
begin
  result := LoadFromDer(PemToDer(pem));
end;

function TX509.LoadFromCsr(const Csr: RawByteString): boolean;
var
  pos, posnfo: integer;
  xsa: TXSignatureAlgorithm;
  der, nfo, version, oid, oid2, sig: RawByteString;
begin
  result := false;
  if (self = nil) or
     (Csr = '') then
    exit;
  der := PemToDer(Csr);
  // parse supplied Certificate Signing Request content
  pos := 1;
  posnfo := 1;
  if (AsnNext(pos, der) = ASN1_SEQ) and
     (AsnNextRaw(pos, der, nfo, {includeheader=}true) = ASN1_SEQ) and
     (AsnNext(posnfo, nfo) = ASN1_SEQ) and
     (AsnNext(posnfo, nfo, @version) = ASN1_INT) and
     (version = '0') and
     Signed.Subject.FromAsnNext(posnfo, nfo) and
     (AsnNext(posnfo, nfo) = ASN1_SEQ) and // subjectPublicKeyInfo
     AsnNextAlgoOid(posnfo, nfo, oid, oid2) and
     OidToXka(oid, oid2, Signed.SubjectPublicKeyAlgorithm) and
     (AsnNextRaw(posnfo, nfo, Signed.SubjectPublicKey) = ASN1_BITSTR) and
     AsnNextAlgoOid(pos, der, oid, oid2) and
     OidToXsa(oid, xsa) and
     (AsnNextRaw(pos, der, sig) = ASN1_BITSTR) and
     PublicKey.Verify(xsa, nfo, sig) then // check self-signature
  begin
    Signed.SubjectPublicKeyBits := X509PubKeyBits(Signed.SubjectPublicKey);
    // load any extensionRequest (PKCS #9 via CRMF)
    if (AsnNext(posnfo, nfo) = ASN1_CTC0) and // optional attributes sequence
       (AsnNext(posnfo, nfo) = ASN1_SEQ) and
       (AsnNext(posnfo, nfo, @oid) = ASN1_OBJID) and
       (oid = ASN1_OID_PKCS9_EXTREQ) and
       (AsnNext(posnfo, nfo) = ASN1_SETOF) and
       (AsnNext(posnfo, nfo) = ASN1_SEQ) then
      Signed.AddNextExtensions(posnfo, nfo);
    result := true;
  end;
end;

function TX509.FingerPrint(algo: THashAlgo): RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if algo = hfSHA1 then
    begin
      result := fCachedSha1;
      if result <> '' then
        exit;
    end;
    result := HashFull(algo, SaveToDer);
    if algo = hfSHA1 then
    begin
      fSafe.Lock;
      try
        fCachedSha1 := result;
      finally
        fSafe.UnLock;
      end;
    end;
  end;
end;

function TX509.IsSelfSigned: boolean;
begin
  result := (self <> nil) and
            (Signed.Issuer.ToBinary = Signed.Subject.ToBinary);
end;

function TX509.SignatureSecurityBits: integer;
begin
  if (self <> nil) and
     (SignatureAlgorithm <> xsaNone) then
    result := GetSignatureSecurityBits(
      XSA_TO_AA[SignatureAlgorithm], length(SignatureValue))
  else
    result := 0;
end;

function TX509.SubjectAlternativeNames: TRawUtf8DynArray;
begin
  if self = nil then
    result := nil
  else
    result := Signed.ExtensionArray(xeSubjectAlternativeName);
end;

function TX509.PeerInfo: RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    if fCachedPeerInfo = '' then
      ComputeCachedPeerInfo;
    result := fCachedPeerInfo;
  end;
end;

procedure TX509.AfterModified;
begin
  fSafe.Lock;
  try
    fCachedDer := '';
    fCachedSha1 := '';
    fCachedPeerInfo := '';
    fSignatureValue := '';
  finally
    fSafe.UnLock;
  end;
  Signed.AfterModified;
end;



{ **************** Registration of our X.509 Engine to the TCryptCert Factory }

function TX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;
var
  x: TX509;
begin
  x := TX509.Create;
  try
    result := x.LoadFromPem(Cert); // support PEM or DER input
    if result then
      x.ToParsedInfo(Info);
  finally
    x.Free;
  end;
end;

type
  ECryptCertX509 = class(ECryptCert);

  /// ICryptCert factory using our TX509 class
  TCryptCertAlgoX509 = class(TCryptCertAlgo)
  protected
    fXsa: TXSignatureAlgorithm;
    fXka: TXPublicKeyAlgorithm;
  public
    constructor Create(xsa: TXSignatureAlgorithm;
      const suffix: RawUtf8); reintroduce; overload;
    function New: ICryptCert; override; // = TCryptCertX509.Create(self)
    function FromHandle(Handle: pointer): ICryptCert; override;
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8; override;
  end;

  /// class implementing ICryptCert using our TX509 class
  // - will store a certificate as TX509 and/or a TXPrivateKey private key
  TCryptCertX509 = class(TCryptCert)
  protected
    fX509: TX509;
    fPrivateKey: TXPrivateKey;
    function Xsa: TXSignatureAlgorithm;
      {$ifdef HASINLINE} inline; {$endif}
    function Xka: TXPublicKeyAlgorithm;
      {$ifdef HASINLINE} inline; {$endif}
    function VerifyAuthority(const Authority: ICryptCert): TCryptCertX509;
    procedure GeneratePrivateKey;
  public
    destructor Destroy; override;
    procedure Clear;
    // ICryptCert methods
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; override;
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert; override;
    function GetSerial: RawUtf8; override;
    function GetSubjectName: RawUtf8; override;
    function GetSubject(const Rdn: RawUtf8): RawUtf8; override;
    function GetSubjects: TRawUtf8DynArray; override;
    function GetIssuerName: RawUtf8; override;
    function GetIssuer(const Rdn: RawUtf8): RawUtf8; override;
    function GetSubjectKey: RawUtf8; override;
    function GetAuthorityKey: RawUtf8; override;
    function IsSelfSigned: boolean; override;
    function GetNotBefore: TDateTime; override;
    function GetNotAfter: TDateTime; override;
    function GetUsage: TCryptCertUsages; override;
    function GetPeerInfo: RawUtf8; override;
    function GetSignatureInfo: RawUtf8; override;
    function GetDigest(Algo: THashAlgo): RawUtf8; override;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; override;
    function Save(Content: TCryptCertContent; const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; override;
    function HasPrivateSecret: boolean; override;
    function GetPublicKey: RawByteString; override;
    function GetPrivateKey: RawByteString; override;
    function SetPrivateKey(const saved: RawByteString): boolean; override;
    function Sign(Data: pointer; Len: integer): RawByteString; override;
    procedure Sign(const Authority: ICryptCert); override;
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity; override;
    function Verify(const Authority: ICryptCert; IgnoreError: TCryptCertValidities;
      TimeUtc: TDateTime): TCryptCertValidity; override;
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    function SharedSecret(const pub: ICryptCert): RawByteString; override;
    function Handle: pointer; override; // a TX509 instance
    function PrivateKeyHandle: pointer; override;
    function GetPrivateKeyParams(out x, y: RawByteString): boolean; override;
    property X509: TX509
      read fX509;
  end;


{ TCryptCertAlgoX509 }

constructor TCryptCertAlgoX509.Create(xsa: TXSignatureAlgorithm;
  const suffix: RawUtf8);
begin
  if xsa = xsaNone then
    raise ECryptCertX509.CreateUtf8('Unexpected %.Create(%)', [self, ToText(xsa)^]);
  fXsa := xsa;
  fXka := XSA_TO_XKA[xsa];
  fOsa := XSA_TO_AA[xsa];
  inherited Create('x509-' + LowerCase(CAA_JWT[fOsa]) + suffix);
end;

function TCryptCertAlgoX509.New: ICryptCert;
begin
  result := TCryptCertX509.Create(self);
end;

function TCryptCertAlgoX509.FromHandle(Handle: pointer): ICryptCert;
var
  instance: TCryptCertX509;
begin
  if (Handle = nil) or
     not TObject(Handle).InheritsFrom(TX509) then
    instance := nil
  else
  begin
    instance := TCryptCertX509.Create(self);
    instance.fX509 := Handle;
  end;
  result := instance;
end;

function TCryptCertAlgoX509.CreateSelfSignedCsr(const Subjects: RawUtf8;
  const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;

  procedure RaiseError(const msg: shortstring);
  begin
    raise ECryptCertX509.CreateUtf8(
      '%.CreateSelfSignedCsr %: % error', [self, JwtName, msg]);
  end;

var
  key: TXPrivateKey;
begin
  if Subjects = '' then
    RaiseError('no Subjects');
  key := TXPrivateKey.Create;
  try
    // load or generate a public/private key pair
    if PrivateKeyPem <> '' then
      if not key.Load(fXka, nil, PrivateKeyPem, PrivateKeyPassword) then
        RaiseError('PrivateKeyPem');
    // setup the CSR fields, self-sign the CSR and return it as PEM
    result := key.ComputeSelfSignedCsr(fXsa, Subjects, Usages, Fields);
    // save the generated private key (if was not previously loaded)
    if (result <> '') and
       (PrivateKeyPem = '') then
      PrivateKeyPem := key.Save(ccfPem, PrivateKeyPassword);
  finally
    key.Free;
  end;
end;


{ TCryptCertX509 }

destructor TCryptCertX509.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TCryptCertX509.Clear;
begin
  FreeAndNil(fX509);
  FreeAndnil(fPrivateKey);
end;

function TCryptCertX509.Xsa: TXSignatureAlgorithm;
begin
  result := TCryptCertAlgoX509(fCryptAlgo).fXsa;
end;

function TCryptCertX509.Xka: TXPublicKeyAlgorithm;
begin
  result := TCryptCertAlgoX509(fCryptAlgo).fXka;
end;

function TCryptCertX509.VerifyAuthority(const Authority: ICryptCert): TCryptCertX509;
begin
  if (fX509 <> nil) or
     HasPrivateSecret then
    RaiseErrorGenerate('duplicated call');
  result := self; // self-signed
  if Authority <> nil then
    if Authority.HasPrivateSecret then
      if Authority.Instance.InheritsFrom(TCryptCertX509) then
        result := TCryptCertX509(Authority.Instance)
      else
        RaiseErrorGenerate('Authority is not a TCryptCertX509')
    else
      RaiseErrorGenerate('Authority has no private key to sign');
end;

procedure TCryptCertX509.GeneratePrivateKey;
begin
  if HasPrivateSecret then
    RaiseErrorGenerate('duplicated GeneratePrivateKey');
  fPrivateKey := TXPrivateKey.Create;
  fX509.Signed.SubjectPublicKey := fPrivateKey.Generate(xka);
  if fX509.Signed.SubjectPublicKey = '' then
    RaiseErrorGenerate('GeneratePrivateKey failed');
  fX509.Signed.SubjectPublicKeyAlgorithm := Xka;
  fX509.Signed.SubjectPublicKeyBits :=
    X509PubKeyBits(fX509.Signed.SubjectPublicKey);
end;

function TCryptCertX509.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
var
  auth: TCryptCertX509;
begin
  auth := VerifyAuthority(Authority);
  fX509 := TX509.Create;
  try
    // fill the supplied certificate fields
    fX509.Signed.CertUsages := Usages; // used by SetFromCertUsages
    CertInfoPrepare(fX509.Signed.Subject, fX509.Signed.Extension,
      Subjects, Fields);
    fX509.Signed.Generate(ExpireDays, ValidDays);
    // generate a new public/private key pair to store with this instance
    GeneratePrivateKey;
    // (self-)sign this certificate
    Sign(auth);
    result := self;
  except
    Clear;
  end;
end;

function TCryptCertX509.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
var
  auth: TCryptCertX509;
begin
  auth := VerifyAuthority(Authority);
  fX509 := TX509.Create;
  try
    // fill the certificate fields from the supplied CSR information
    if not fX509.LoadFromCsr(Csr) then
      RaiseErrorGenerate('LoadFromCsr');
    fX509.Signed.Generate(ExpireDays, ValidDays);
    // (self-)sign this certificate
    if auth = self then
      // the CSR has only a public key: generate a new key pair
      GeneratePrivateKey;
    Sign(auth);
    result := self;
  except
    Clear;
  end;
end;

function TCryptCertX509.GetSerial: RawUtf8;
begin
  result := fX509.GetSerialNumber;
end;

function TCryptCertX509.GetSubjectName: RawUtf8;
begin
  result := fX509.GetSubjectDN;
end;

function TCryptCertX509.GetSubject(const Rdn: RawUtf8): RawUtf8;
var
  subs: TRawUtf8DynArray;
begin
  result := '';
  if fX509 <> nil then
    result := fX509.Signed.Subject.Get(Rdn); // RDN or hash
  if result <> '' then
    exit;
  subs := fX509.SubjectAlternativeNames;
  if subs <> nil then
    result := subs[0];  // return the first DNS: as with mormot.crypt.ecc
end;

function TCryptCertX509.GetSubjects: TRawUtf8DynArray;
begin
  result := fX509.SubjectAlternativeNames;
end;

function TCryptCertX509.GetIssuerName: RawUtf8;
begin
  result := fX509.GetIssuerDN;
end;

function TCryptCertX509.GetIssuer(const Rdn: RawUtf8): RawUtf8;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Signed.Issuer.Get(Rdn); // RDN or hash
end;

function TCryptCertX509.GetSubjectKey: RawUtf8;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Extension[xeSubjectKeyIdentifier];
end;

function TCryptCertX509.GetAuthorityKey: RawUtf8;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Extension[xeAuthorityKeyIdentifier];
end;

function TCryptCertX509.IsSelfSigned: boolean;
begin
  result := fX509.IsSelfSigned;
end;

function TCryptCertX509.GetNotBefore: TDateTime;
begin
  if fX509 = nil then
    result := 0
  else
    result := fX509.NotBefore;
end;

function TCryptCertX509.GetNotAfter: TDateTime;
begin
  if fX509 = nil then
    result := 0
  else
    result := fX509.NotAfter;
end;

function TCryptCertX509.GetUsage: TCryptCertUsages;
begin
  if fX509 = nil then
    result := []
  else
    result := fX509.Usages;
end;

function TCryptCertX509.GetPeerInfo: RawUtf8;
begin
  result := fX509.PeerInfo;
end;

function TCryptCertX509.GetSignatureInfo: RawUtf8;
var
  bits: integer;
begin
  bits := fX509.SignatureSecurityBits;
  if bits = 0 then
    result := ''
  else
    FormatUtf8('% %', [bits, XSA_TXT[fX509.SignatureAlgorithm]], result);
end;

function TCryptCertX509.GetDigest(Algo: THashAlgo): RawUtf8;
begin
  result := fX509.FingerPrint(Algo);
end;

function TCryptCertX509.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
var
  bin, der: RawByteString;
begin
  result := false;
  if Saved <> '' then
  try
    case Content of
      cccPrivateKeyOnly:
        begin
          // use mormot.core.secure encryption, not standard PKCS#8
          der := PemToDer(Saved); // see also TXPrivateKey.Load
          bin := PrivateKeyDecrypt(
            der, XKA_SALT[Xka], PrivatePassword, XKA_ROUNDS[Xka]);
          result := SetPrivateKey(bin);
        end;
      cccCertOnly:
        begin
          Clear;
          fX509 := TX509.Create;
          //FileFromString(Saved, '/home/ab/Downloads/generated.der');
          if fX509.LoadFromPem(Saved) then
            result := true
          else
            FreeAndNil(fX509);
        end;
      cccCertWithPrivateKey:
        // concatenate certificate PEM and private key PEM - no PKCS#12 yet
        result := PemToCertAndPrivKey(Saved, der, bin) and
                  Load(der, cccCertOnly, '') and
                  Load(bin, cccPrivateKeyOnly, PrivatePassword)
    end;
  finally
    FillZero(bin);
    FillZero(der);
  end;
end;

function TCryptCertX509.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
var
  pem: RawUtf8;
begin
  result := '';
  if not (Format in [ccfBinary, ccfPem]) then
    // hexa or base64 encoding of the binary output is handled by TCryptCert
    result := inherited Save(Content, PrivatePassword, Format)
  else
    // we implement ccfPem and ccfBinary here
    case Content of
      cccCertOnly:
        if fX509 <> nil then
        begin
          result := fX509.SaveToDer;
          if Format = ccfPem then
            result := DerToPem(result, pemCertificate);
        end;
      cccCertWithPrivateKey:
        if fX509 <> nil then
          if HasPrivateSecret then
          try
            // save as concatenated PEM, even if ccfBinary was requested
            // (no PKCS#12 support yet)
            pem := Save(cccPrivateKeyOnly, PrivatePassword, ccfPem);
            result := Save(cccCertOnly, '', ccfPem) + RawUtf8(#13#10) + pem;
          finally
            FillZero(pem);
          end
          else
            RaiseError('Save(cccCertWithPrivateKey) with no Private Key');
      cccPrivateKeyOnly:
        if HasPrivateSecret then
          result := fPrivateKey.Save(Format, PrivatePassword)
        else
          RaiseError('Save(cccPrivateKeyOnly) with no Private Key');
    end;
end;

function TCryptCertX509.HasPrivateSecret: boolean;
begin
  result := fPrivateKey <> nil;
end;

function TCryptCertX509.GetPublicKey: RawByteString;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Signed.SubjectPublicKey;
end;

function TCryptCertX509.GetPrivateKey: RawByteString;
begin
  result := fPrivateKey.ToDer;
end;

function TCryptCertX509.SetPrivateKey(const saved: RawByteString): boolean;
var
  pub: TXPublicKey;
begin
  result := false;
  FreeAndNil(fPrivateKey); // always release - SetPrivateKey('') is "wipe out"
  if saved <> '' then
  begin
    pub := nil; // SetPrivateKey() may be called without a public key yet
    if fX509 <> nil then
      pub := fX509.PublicKey;
    fPrivateKey := TXPrivateKey.Create;
    if fPrivateKey.Load(Xka, pub, saved, '') then
      result := true
    else
      FreeAndNil(fPrivateKey);
  end;
end;

function TCryptCertX509.Sign(Data: pointer; Len: integer): RawByteString;
begin
  if HasPrivateSecret and
     (fX509 <> nil) and
     (cuDigitalSignature in fX509.Usages) then
    result := fPrivateKey.Sign(Xsa, Data, Len)
  else
    result := '';
end;

procedure TCryptCertX509.Sign(const Authority: ICryptCert);
var
  auth: TCryptCertX509;
begin
  if Assigned(Authority) and
    Authority.HasPrivateSecret then
  begin
    // validate usage
    auth := Authority.Instance as TCryptCertX509;
    if auth.fX509 = nil then
      RaiseError('Sign: no public key');
    if (auth <> self) and
       not (cuKeyCertSign in auth.fX509.Usages) then
      RaiseError('Sign: no cuKeyCertSign');
    // assign the Issuer information
    fX509.Signed.Issuer := auth.fX509.Signed.Subject; // may be self
    fX509.Signed.Extension[xeAuthorityKeyIdentifier] :=
       auth.fX509.Signed.Extension[xeSubjectKeyIdentifier];
    // compute the digital signature
    fX509.AfterModified;
    fX509.Signed.Signature := Xsa;
    fX509.fSignatureValue := auth.fPrivateKey.Sign(Xsa, fX509.Signed.ToDer);
    fX509.fSignatureAlgorithm := Xsa;
  end
  else
    RaiseError('Sign: not a CA');
end;

function TCryptCertX509.Verify(Sign, Data: pointer; SignLen, DataLen: integer;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := fX509.Verify(Sign, Data, SignLen, DataLen, IgnoreError, TimeUtc);
end;

function TCryptCertX509.Verify(const Authority: ICryptCert;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
var
  auth: TX509;
begin
  result := cvBadParameter;
  if fX509 = nil then
    exit;
  auth := nil;
  if Authority <> nil then
    if Authority.Instance.InheritsFrom(TCryptCertX509) then
      auth := Authority.Handle
    else
      exit;
  result := fX509.Verify(auth, IgnoreError, TimeUtc);
end;

function TCryptCertX509.Encrypt(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  if (fX509 <> nil) and
     (fX509.Usages * [cuDataEncipherment, cuEncipherOnly] <> []) then
    result := fX509.PublicKey.Seal(Message, Cipher)
  else
    result := '';
end;

function TCryptCertX509.Decrypt(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  if (fX509 <> nil) and
     (fPrivateKey <> nil) and
     (fX509.Usages * [cuDataEncipherment, cuEncipherOnly] <> []) then
    result := fPrivateKey.Open(Message, Cipher)
  else
    result := '';
end;

function TCryptCertX509.SharedSecret(const pub: ICryptCert): RawByteString;
begin
  if (fX509 <> nil) and
     (fPrivateKey <> nil) and
     (cuKeyAgreement in fX509.Usages) and
     Assigned(pub) and
     pub.Instance.InheritsFrom(TCryptCertX509) and
     (pub.Handle <> nil) and
     (cuKeyAgreement in TX509(pub.Handle).Usages) then
    result := fPrivateKey.SharedSecret(TX509(pub.Handle).PublicKey)
  else
    result := '';
end;

function TCryptCertX509.Handle: pointer;
begin
  result := fX509;
end;

function TCryptCertX509.PrivateKeyHandle: pointer;
begin
  result := fPrivateKey;
end;

function TCryptCertX509.GetPrivateKeyParams(out x, y: RawByteString): boolean;
begin
  result := fX509.PublicKey.GetParams(x, y);
end;


procedure InitializeUnit;
var
  a: TXAttr;
  o: TXExtension;
  k: TXExtendedKeyUsage;
  xsa: TXSignatureAlgorithm;
begin
  for a := succ(low(a)) to high(a) do
    XA_OID_ASN[a] := AsnEncOid(XA_OID[a]);
  for o := succ(low(o)) to high(o) do
    XE_OID_ASN[o] := AsnEncOid(XE_OID[o]);
  for k := succ(low(k)) to high(k) do
    XKU_OID_ASN[k] := AsnEncOid(XKU_OID[k]);
  // register TX509 to our high-level cryptographic catalog
  // - 'x509-rs256-int' 'x509-ps256-int' and 'x509-es256-int' match this unit
  // - 'x509-rs/ps384/512-int' methods seem superfluous
  TCryptCertAlgoX509.Create(xsaSha256Rsa,    {suffix=}'-int');
  TCryptCertAlgoX509.Create(xsaSha256RsaPss, {suffix=}'-int');
  TCryptCertAlgoX509.Create(xsaSha256Ecc256, {suffix=}'-int');
  // register 'x509-rs256' 'x509-rs384' 'x509-rs512' 'x509-ps256' 'x509-ps384'
  // 'x509-ps512' and 'x509-es256' certificates
  // - may be overriden by the faster mormot.crypt.openssl if included
  for xsa := succ(low(xsa)) to high(xsa) do
    TCryptCertAlgoX509.Create(xsa, {suffix=}'');
  // use our class for X.509 parsing - unless mormot.crypt.openssl is included
  X509Parse := @TX509Parse;
end;



initialization
  InitializeUnit;
  
end.
