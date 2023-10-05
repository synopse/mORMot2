/// Framework Core ASN.1 / X.509 Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.x509;

{
  *****************************************************************************

   ASN.1 Encoding and X.509 Certificates Implementation 
    - X.509 Encoding Decoding
    - X.509 Certificates
    - Registration of our X.509 Engine to the TCryptCert Factory

  *****************************************************************************
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


{ **************** X.509 Encoding Decoding }

// note: all X.509 type names in this unit start with TX

type
  /// exception raised by this unit
  EX509 = class(ESynException);

  /// known X.501 Type Names, as stored in X.509 Certificates attributes
  // - as defined in RFC 5280 appendix A, and available via TX501Name.Names[]
  TXAttr = (
    xaNone,
    xaDC,   // domainComponent
    xaCN,   // commonName (3)
    xaSN,   // serialNumber (5)
    xaC,    // countryName (6)
    xaL,    // localityName (7)
    xaST,   // stateOrProvinceName (8)
    xaO,    // organizationName (10)
    xaOU,   // organizationalUnitName (11)
    xaT,    // title (12)
    xaTN,   // telephoneNumber (20)
    xaN,    // name (41)
    xaS,    // surname (4)
    xaG,    // givenName (42)
    xaI,    // initials (43)
    xaGQ,   // generationQualifier (44)
    xaQ,    // qualifier (46)
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
  // - implement RSA and ECC-256 asymmetric algorithms using our own units
  // - for safety, any unsafe algorithms (e.g. MD5 or SHA-1) are not defined
  TXSignatureAlgorithm = (
    xsaNone,
    xsaSha256Rsa,
    xsaSha384Rsa,
    xsaSha512Rsa,
    xsaSha256Ecc256);

  /// supported TX509.SubjectPublicKeyAlgorithm values
  TXPublicKeyAlgorithm = (
    xkaNone,
    xkaRsa,
    xkaEcc256);

  /// used to store one unknown/unsupported attribute or extension
  // - in TXname.Other or TXTbsCertificate.ExtensionOther
  TXOther = record
    /// the OID of this value, in raw binary form
    Oid: RawByteString;
    /// the associated value
    // - as RawUtf8 for TXname.Others[]
    // - as ASN1_OCTSTR raw content for TXTbsCertificate.ExtensionOther[]
    Value: RawByteString;
  end;

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
    /// return the hash of the normalized Binary of this field
    function ToDigest(algo: THashAlgo = hfSha1): RawUtf8;
    /// to be called once any field has been changed to refresh internal caches
    procedure AfterModified;
  end;

/// efficient search of a TXOther.Value from a 'x.x.x.x.x' text OID
function FindOther(const Other: TXOthers; const OidText: RawUtf8): RawByteString;

function ToText(a: TXAttr): PShortString; overload;
function ToText(e: TXExtension): PShortString; overload;
function ToText(u: TXKeyUsage): PShortString; overload;
function ToText(x: TXExtendedKeyUsage): PShortString; overload;
function ToText(a: TXSignatureAlgorithm): PShortString; overload;
function ToText(a: TXPublicKeyAlgorithm): PShortString; overload;


const
  /// internal lookup table from X.509 Signature to Public Key Algorithms
  XSA_TO_XKA: array[TXSignatureAlgorithm] of TXPublicKeyAlgorithm = (
    xkaNone,     // xsaNone
    xkaRsa,      // xsaSha256Rsa
    xkaRsa,      // xsaSha384Rsa
    xkaRsa,      // xsaSha512Rsa
    xkaEcc256);  // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature to Hash Algorithms
  XSA_TO_HF: array[TXSignatureAlgorithm] of THashAlgo = (
    hfSha256,    // xsaNone
    hfSha256,    // xsaSha256Rsa
    hfSha384,    // xsaSha384Rsa
    hfSha512,    // xsaSha512Rsa
    hfSha256);   // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature to ICryptCert Algorithms
  XSA_TO_AA: array[TXSignatureAlgorithm] of TCryptAsymAlgo = (
    caaES256K,   // xsaNone
    caaRS256,    // xsaSha256Rsa
    caaRS384,    // xsaSha384Rsa
    caaRS512,    // xsaSha512Rsa
    caaES256);   // xsaSha256Ecc256

  /// internal lookup table from X.509 Signature Algorithm as text
  XSA_TXT: array[TXSignatureAlgorithm] of RawUtf8 = (
    '',                              // xsaNone
    'SHA256 with RSA encryption',    // xsaSha256Rsa
    'SHA384 with RSA encryption',    // xsaSha384Rsa
    'SHA512 with RSA encryption',    // xsaSha512Rsa
    'SHA256 with prime256v1 ECDSA'); // xsaSha256Ecc256

  /// internal lookup table from X.509 Public Key Algorithm as text
  XKA_TXT: array[TXPublicKeyAlgorithm] of RawUtf8 = (
    '',                   // xkaNone
    'RSA encryption',     // xkaRsa
    'prime256v1 ECDSA');  // xkaEcc256

  /// the OID of all known TX509Name attributes, as defined in RFC 5280 A.1
  XA_OID: array[TXAttr] of PUtf8Char = (
    '',                           // xaNone
    '0.9.2342.19200300.100.1.25', // xaDC  domainComponent
    '2.5.4.3',                    // xaCN  commonName (3)
    '2.5.4.5',                    // xaSN  serialNumber (5)
    '2.5.4.6',                    // xaC   countryName (6)
    '2.5.4.7',                    // xaL   localityName (7)
    '2.5.4.8',                    // xaST  stateOrProvinceName (8)
    '2.5.4.10',                   // xaO   organizationName (10)
    '2.5.4.11',                   // xaOU  organizationalUnitName (11)
    '2.5.4.12',                   // xaT   title (12)
    '2.5.4.20',                   // xaTN  telephoneNumber (20)
    '2.5.4.41',                   // xaN   name (41)
    '2.5.4.4',                    // xaS   surname (4)
    '2.5.4.42',                   // xaG   givenName (42)
    '2.5.4.43',                   // xaI   initials (43)
    '2.5.4.44',                   // xaGQ  generationQualifier (44)
    '2.5.4.46',                   // xaQ   qualifier (46)
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
  ASN1_OID_SIGNATURE: array[TXSignatureAlgorithm] of RawUtf8 = (
     '',
     '1.2.840.113549.1.1.11', // xsaSha256Rsa
     '1.2.840.113549.1.1.12', // xsaSha384Rsa
     '1.2.840.113549.1.1.13', // xsaSha512Rsa
     '1.2.840.10045.4.3.2');  // xsaSha256Ecc256

  ASN1_OID_PKCS1_RSA       = '1.2.840.113549.1.1.1';
  ASN1_OID_X962_PUBLICKEY  = '1.2.840.10045.2.1';
  ASN1_OID_X962_ECDSA_P256 = '1.2.840.10045.3.1.7';


{ **************** X.509 Certificates }

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
    fCachedDer: RawByteString; // for ToDer
    procedure ComputeCachedDer;
    procedure ComputeCertUsages;
    procedure AddNextExtensions(pos: integer; const der: TAsnObject);
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
    fRsa: TRsa;
    fEcc: TEcc256r1VerifyAbstract;
    procedure ComputeCachedDer;
    procedure ComputeCachedPeerInfo;
    function GetSerialNumber: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetIssuerDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectPublicKeyAlgorithm: RawUtf8;
    function ComputeDigest(Algo: TXSignatureAlgorithm): TSha256Digest;
    procedure SignRsa(RsaAuthority: TRsa);
    procedure SignEcc(const EccKey: TEccPrivateKey);
    /// verify some buffer with the stored Signed.SubjectPublicKey
    // - will maintain an internal RSA or ECC256 public key instance
    function RawSubjectPublicKeyVerify(const Data, Signature: RawByteString;
      Hash: THashAlgo): boolean;
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
    // - depending on the engine, some errors can be ignored, e.g.
    // cvWrongUsage or cvDeprecatedAuthority
    // - certificate expiration date can be specified instead of current time
    // - this method is thread-safe
    function Verify(Authority: TX509 = nil; IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity;
    /// to be called once any field has been changed to refresh the Binary cache
    procedure AfterModified;
    /// serialize those fields into ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function SaveToDer: TCertDer;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function LoadFromDer(const der: TCertDer): boolean;
    /// unserialize those fields from a PEM content
    // - will fallback and try as ASN.1 DER binary if content is not PEM
    function LoadFromPem(const pem: TCertPem): boolean;
    /// the lowercase hexa hash of the normalized Binary of this Certificate
    // - default hfSha1 value is cached internally so is efficient for lookup
    function FingerPrint(algo: THashAlgo = hfSha1): RawUtf8;
    /// check if the Certificate Issuer is also its Subject
    function IsSelfSigned: boolean;
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


implementation



{ **************** X.509 Encoding Decoding}

function XsaToSeq(xsa: TXSignatureAlgorithm): TAsnObject;
begin
  case xsa of
    xsaSha256Rsa .. xsaSha512Rsa:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                  ASN1_NULL_VALUE // optional
                ]);
    xsaSha256Ecc256:
      result := Asn(ASN1_SEQ, [
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
      result := Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_PKCS1_RSA),
                  ASN1_NULL_VALUE // optional
                ]);
    xkaEcc256:
      result := Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_X962_PUBLICKEY),
                  AsnOid(ASN1_OID_X962_ECDSA_P256)
                ]);
  else
    raise EX509.CreateUtf8('Unexpected XkaToSeq(%)', [ord(xka)]);
  end;
end;

function AsnNextAlgoOid(var pos: integer; const der: TAsnObject;
  out oid: RawByteString; oid2: PRawByteString): boolean;
var
  seq: RawByteString;
  p: integer;
begin
  p := 1;
  result := (AsnNextRaw(pos, der, seq) = ASN1_SEQ) and
            (AsnNext(p, seq, @oid) = ASN1_OBJID) and // decode OID as text
            ({%H-}oid <> '');
  if result and
     (oid2 <> nil) then
    AsnNext(p, seq, oid2);
end;

function OidToXsa(const oid: RawUtf8; out xsa: TXSignatureAlgorithm): boolean;
var
  x: TXSignatureAlgorithm;
begin
  for x := succ(low(x)) to high(x) do
    if oid = ASN1_OID_SIGNATURE[x] then
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
    if oid = XA_OID_ASN[result] then
      exit;
  result := xaNone;
end;

function OidToXe(const oid: RawByteString): TXExtension;
begin
  for result := succ(low(result)) to high(result) do
    if oid = XE_OID_ASN[result] then
      exit;
  result := xeNone;
end;

function OidToXku(const oid: RawByteString): TXExtendedKeyUsage;
begin
  for result := succ(low(result)) to high(result) do
    if oid = XKU_OID_ASN[result] then
      exit;
  result := xkuNone;
end;

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



{ TX509Name }

procedure TXName.ComputeAsn;
var
  a: TXAttr;
  p: PUtf8Char;
  o: PtrInt;
  v: RawUtf8;
  tmp, one: RawByteString;
begin
  for a := succ(low(a)) to high(a) do
  begin
    p := pointer(Name[a]);
    if p <> nil then
    begin
      one := '';
      repeat
        GetNextItemTrimed(p, ',', v);
        Append(one, Asn(ASN1_SEQ, [
                      XA_OID_ASN[a],
                      AsnText(v)
                    ]));
      until p = nil;
      Append(tmp, Asn(ASN1_SETOF, [one]));
    end;
  end;
  for o := 0 to high(Other) do
    with Other[o] do
      Append(tmp, Asn(ASN1_SETOF, [
                    Asn(ASN1_SEQ, [
                      Asn(ASN1_OBJID, [Oid]),
                      AsnText(Value)
                    ])
                  ]));
  fCachedAsn := Asn(ASN1_SEQ, [tmp]);
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
  result := (AsnNextRaw(pos, der, seq) = ASN1_SEQ) and
            FromAsn(seq);
end;

procedure TXName.AfterModified;
begin
  fCachedAsn := '';
  fCachedText := '';
end;

function TXName.ToDigest(algo: THashAlgo): RawUtf8;
begin
  result := HashFull(algo, ToBinary);
end;


function FindOther(const Other: TXOthers; const OidText: RawUtf8): RawByteString;
var
  n: integer;
  o: ^TXOther;
  oid: RawByteString;
begin
  result := '';
  if Other = nil then
    exit;
  oid := AsnEncOid(pointer(OidText));
  if oid = '' then
    exit;
  o := pointer(Other);
  n := length(Other);
  repeat
    if o^.Oid = oid then // efficient search
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



{ **************** X.509 Certificates }

{ TXTbsCertificate }

procedure TXTbsCertificate.ComputeCachedDer;
var
  ext: RawByteString;
begin
  if Version >= 3 then
  begin

    ext := Asn(ASN1_CTC3, [ext]);
  end;
  fCachedDer := Asn(ASN1_SEQ, [
                  {%H-}Asn(Version - 1),
                  Asn(ASN1_INT, SerialNumber),
                  XsaToSeq(Signature),
                  Issuer.ToBinary,
                  Asn(ASN1_SEQ, [
                    AsnTime(NotBefore),
                    AsnTime(NotAfter)
                  ]),
                  Subject.ToBinary,
                  Asn(ASN1_SEQ, [
                    XkaToSeq(SubjectPublicKeyAlgorithm),
                    SubjectPublicKey
                  ]),
                  ext
                ]);
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
     not AsnNextAlgoOid(pos, der, oid, nil) or
     not OidToXsa(oid, Signature) or
     not Issuer.FromAsnNext(pos, der) or
     (AsnNext(pos, der) <> ASN1_SEQ) or // validity
     not AsnNextTime(pos, der, NotBefore) or
     not AsnNextTime(pos, der, NotAfter) or
     not Subject.FromAsnNext(pos, der) or
     (AsnNext(pos, der) <> ASN1_SEQ) or // subjectPublicKeyInfo
     not AsnNextAlgoOid(pos, der, oid, @oid2) or
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
  fCachedDer := ''; // reset the cache
  Subject.AfterModified;
  Issuer.AfterModified;
end;


{ TX509 }

destructor TX509.Destroy;
begin
  inherited Destroy;
  fRsa.Free;
  fEcc.Free;
end;

procedure TX509.Clear;
begin
  fCachedDer := '';
  fCachedSha1 := '';
  fCachedPeerInfo := '';
  Signed.Clear;
  fSignatureAlgorithm := xsaNone;
  fSignatureValue := '';
  FreeAndNil(fRsa);
  FreeAndNil(fEcc);
end;

procedure TX509.SignRsa(RsaAuthority: TRsa);
var
  dig: THash256;
begin
  if not RsaAuthority.HasPrivateKey then
    raise EX509.Create('TX509.Sign with no RsaAuthority');
  dig := ComputeDigest(xsaSha256Rsa);
  fSignatureValue := RsaAuthority.Sign(@dig, hfSHA256);
end;

procedure TX509.SignEcc(const EccKey: TEccPrivateKey);
var
  sig: TEccSignature;
begin
  if Ecc256r1Sign(EccKey, ComputeDigest(xsaSha256Ecc256), sig) then
    fSignatureValue := EccToDer(sig)
  else
    raise EX509.Create('TX509.SignEcc failed');
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
     if not Authority.RawSubjectPublicKeyVerify(
              Signed.ToDer, SignatureValue, XSA_TO_HF[SignatureAlgorithm]) then
       result := cvInvalidSignature
     else if Authority = self then
       result := cvValidSelfSigned;
end;

function TX509.RawSubjectPublicKeyVerify(const Data, Signature: RawByteString;
  Hash: THashAlgo): boolean;
var
  hasher: TSynHasher;
  eccpub: TEccPublicKey absolute hasher;
  eccsig: TEccSignature;
  dig: THash512Rec;
  diglen: PtrInt;
  oid: RawUtf8;
  bin: RawByteString;
begin
  result := false;
  diglen := hasher.Full(Hash, pointer(Data), length(Data), dig);
  if (diglen = 0) or
     (Signed.SubjectPublicKey = '') then
    exit;
  case Signed.SubjectPublicKeyAlgorithm of
    xkaRsa:
      begin
        if fRsa = nil then
        begin
          fSafe.Lock;
          try
            if fRsa = nil then
            begin
              // load the public key into a local fRsa reusable instance
              fRsa := TRsa.Create;
              if not fRsa.LoadFromPublicKeyDer(Signed.SubjectPublicKey) then
              begin
                FreeAndNil(fRsa);
                exit;
              end;
            end;
          finally
            fSafe.UnLock;
          end;
        end;
        // RSA digital signature verification
        bin := fRsa.Verify(Signature, @oid);
        result := (length(bin) = diglen) and
                  CompareMem(pointer(bin), @dig, diglen) and
                  (oid = ASN1_OID_HASH[Hash]);
      end;
    xkaEcc256:
      if DerToEcc(pointer(Signature), length(Signature), eccsig) then
      begin
        if fEcc = nil then
        begin
          fSafe.Lock;
          try
            // load the public key into a local fEcc reusable instance
            if fEcc = nil then
            begin
              if not Ecc256r1CompressAsn1(Signed.SubjectPublicKey, eccpub) then
                exit;
              fEcc := TEcc256r1Verify.Create(eccpub);
            end;
          finally
            fSafe.UnLock;
          end;
        end;
        // secp256r1 digital signature verification
        result := fEcc.Verify(dig.Lo, eccsig);
      end;
  end;
end;

procedure TX509.ToParsedInfo(out Info: TX509Parsed);
begin
  Info.Serial := Signed.SerialNumberHex;
  Info.SubjectDN := SubjectDN;
  Info.IssuerDN := IssuerDN;
  Info.SubjectID := Extension[xeSubjectKeyIdentifier];
  Info.IssuerID := Extension[xeAuthorityKeyIdentifier];
  Info.SubjectAltNames := StringReplaceAll(
    Extension[xeSubjectAlternativeName], ',', ', ');
  Info.SigAlg := XSA_TXT[SignatureAlgorithm];
  Info.PubAlg := GetSubjectPublicKeyAlgorithm;
  Info.Usage := Usages;
  Info.NotBefore := NotBefore;
  Info.NotAfter := NotAfter;
  Info.PubKey := Signed.SubjectPublicKey;
  Info.PeerInfo := ToText(Info); // should be the last
end;

function TX509.SaveToDer: TCertDer;
begin
  if fCachedDer = '' then
    ComputeCachedDer;
  result := fCachedDer;
end;

function TX509.ComputeDigest(Algo: TXSignatureAlgorithm): TSha256Digest;
begin
  // handle any Algorithm change
  if fSignatureAlgorithm <> Algo then
  begin
    fSignatureAlgorithm := Algo;
    fCachedDer := '';
    if Signed.Signature <> Algo then
    begin
      Signed.Signature := Algo;
      Signed.fCachedDer := ''; // force recompute cache
    end;
  end;
  // we only use SHA-256 for newly signed certificates
  result := Sha256Digest(Signed.ToDer);
end;

function TX509.GetSerialNumber: RawUtf8;
begin
  result := Signed.SerialNumberHex;
end;

function TX509.GetIssuerDN: RawUtf8;
begin
  result := Signed.Issuer.AsDNText;
end;

function TX509.GetSubjectDN: RawUtf8;
begin
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
  fCachedSha1 := '';
  fCachedPeerInfo := '';
  fCachedDer := Asn(ASN1_SEQ, [
                  Signed.ToDer,
                  XsaToSeq(SignatureAlgorithm),
                  Asn(ASN1_BITSTR, SignatureValue)
                ]);
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
  tbs, oid: RawByteString;
begin
  Clear;
  fCachedDer := der;
  pos := 1;
  result := (der <> '') and
            (AsnNext(pos, der) = ASN1_SEQ) and
            (AsnNextRaw(pos, der, tbs, {includeheader=}true) = ASN1_SEQ) and
            Signed.FromDer(tbs) and
            AsnNextAlgoOid(pos, der, oid, nil) and
            OidToXsa(oid, fSignatureAlgorithm) and
            (AsnNextRaw(pos, der, fSignatureValue) = ASN1_BITSTR) and
            (fSignatureAlgorithm = Signed.Signature);
end;

function TX509.LoadFromPem(const pem: TCertPem): boolean;
begin
  result := LoadFromDer(PemToDer(pem));
end;

function TX509.FingerPrint(algo: THashAlgo): RawUtf8;
begin
  if algo = hfSHA1 then
  begin
    result := fCachedSha1;
    if result <> '' then
      exit;
  end;
  result := HashFull(algo, SaveToDer);
  if algo = hfSHA1 then
    fCachedSha1 := result;
end;

function TX509.IsSelfSigned: boolean;
begin
  result := Signed.Issuer.ToBinary = Signed.Subject.ToBinary;
end;

function TX509.SubjectAlternativeNames: TRawUtf8DynArray;
begin
  result := Signed.ExtensionArray(xeSubjectAlternativeName);
end;

function TX509.PeerInfo: RawUtf8;
begin
  if fCachedPeerInfo = '' then
    ComputeCachedPeerInfo;
  result := fCachedPeerInfo;
end;

procedure TX509.AfterModified;
begin
  fCachedDer := '';
  fCachedSha1 := '';
  fCachedPeerInfo := '';
  fSignatureValue := '';
  Signed.AfterModified;
end;


function _X509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;
var
  x: TX509;
begin
  x := TX509.Create;
  try
    result := x.LoadFromPem(Cert);
    if result then
      x.ToParsedInfo(Info);
  finally
    x.Free;
  end;
end;


procedure InitializeUnit;
var
  a: TXAttr;
  o: TXExtension;
  k: TXExtendedKeyUsage;
begin
  for a := succ(low(a)) to high(a) do
    XA_OID_ASN[a] := AsnEncOid(XA_OID[a]);
  for o := succ(low(o)) to high(o) do
    XE_OID_ASN[o] := AsnEncOid(XE_OID[o]);
  for k := succ(low(k)) to high(k) do
    XKU_OID_ASN[k] := AsnEncOid(XKU_OID[k]);
  // use our class for X.509 parsing
  X509Parse := @_X509Parse;
end;


initialization
  InitializeUnit;
  
end.
