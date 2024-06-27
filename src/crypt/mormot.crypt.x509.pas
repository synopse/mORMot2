/// Framework Core X.509 Certificates Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.x509;

{
  *****************************************************************************

   X.509 Certificates Implementation - see RFC 5280
    - X.509 Fields Logic
    - X.509 Certificates and Certificate Signing Request (CSR)
    - X.509 Certificate Revocation List (CRL)
    - X.509 Private Key Infrastructure (PKI)
    - Registration of our X.509 Engine to the TCryptCert/TCryptStore Factories

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
    xkuServerAuth,         // 1.3.6.1.5.5.7.3.1
    xkuClientAuth,         // 1.3.6.1.5.5.7.3.2
    xkuCodeSigning,        // 1.3.6.1.5.5.7.3.3
    xkuEmailProtection,    // 1.3.6.1.5.5.7.3.4
    xkuTimeStamping,       // 1.3.6.1.5.5.7.3.8
    xkuOcspSigning,        // 1.3.6.1.5.5.7.3.9
    xkuMsPublisher);       // 1.3.6.1.4.1.311.76.8.1

  /// set of X.509 Certificate Extended Key Usage - see RFC 5280 Section 4.2.1.12
  TXExtendedKeyUsages = set of TXExtendedKeyUsage;

  /// supported X.509 Signature Algorithms
  // - implement RSA, RSA-PSS and ECC-256 asymmetric algorithms using our units
  // - ECC-384, ECC-512 and EdDSA are available if mormot.crypt.openssl is set
  // - for safety, any unsafe algorithms (e.g. MD5 or SHA-1) are not defined
  TXSignatureAlgorithm = (
    xsaNone,
    xsaSha256Rsa,
    xsaSha384Rsa,
    xsaSha512Rsa,
    xsaSha256RsaPss,
    xsaSha384RsaPss,
    xsaSha512RsaPss,
    xsaSha256Ecc256,
    xsaSha384Ecc384,
    xsaSha512Ecc512,
    xsaSha512EdDSA);

  /// supported TX509.SubjectPublicKeyAlgorithm values
  TXPublicKeyAlgorithm = (
    xkaNone,
    xkaRsa,
    xkaRsaPss,
    xkaEcc256,
    xkaEcc384,
    xkaEcc512,
    xkaEdDSA);

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
    /// fill the whole record with zeros
    procedure Clear;
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
    /// compare the ToBinary content of two X.501 names
    function Compare(var Another: TXName): integer;
    /// return the UTF-8 text value of a given text OID
    // - search in Other[] then Name[]
    function FindOid(const oid: RawUtf8): RawUtf8;
    /// to be called once any field has been changed to refresh internal caches
    procedure AfterModified;
  end;

/// convert a X.501 name ASN.1 binary as a single line Distinguished Name text
function XNameAsDN(const Der: TAsnObject): RawUtf8;

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
  /// internal lookup table from X.509 Signature to Public Key Algorithms
  XSA_TO_XKA: array[TXSignatureAlgorithm] of TXPublicKeyAlgorithm = (
    xkaNone,     // xsaNone
    xkaRsa,      // xsaSha256Rsa
    xkaRsa,      // xsaSha384Rsa
    xkaRsa,      // xsaSha512Rsa
    xkaRsaPss,   // xsaSha256RsaPss
    xkaRsaPss,   // xsaSha384RsaPss
    xkaRsaPss,   // xsaSha512RsaPss
    xkaEcc256,   // xsaSha256Ecc256
    xkaEcc384,   // xsaSha384Ecc384
    xkaEcc512,   // xsaSha512Ecc512
    xkaEdDSA);   // xsaSha512EdDSA

  /// internal lookup table from X.509 Signature to Hash Algorithms
  XSA_TO_HF: array[TXSignatureAlgorithm] of THashAlgo = (
    hfSha256,    // xsaNone
    hfSha256,    // xsaSha256Rsa
    hfSha384,    // xsaSha384Rsa
    hfSha512,    // xsaSha512Rsa
    hfSha256,    // xsaSha256RsaPss
    hfSha384,    // xsaSha384RsaPss
    hfSha512,    // xsaSha512RsaPss
    hfSha256,    // xsaSha256Ecc256
    hfSha384,    // xsaSha384Ecc384
    hfSha512,    // xsaSha512Ecc512
    hfSha512);   // xsaSha512EdDSA - the hash is embedded with OpenSSL

  /// internal lookup table from X.509 Signature to ICryptCert Algorithms
  XSA_TO_CAA: array[TXSignatureAlgorithm] of TCryptAsymAlgo = (
    caaES256K,   // xsaNone
    caaRS256,    // xsaSha256Rsa
    caaRS384,    // xsaSha384Rsa
    caaRS512,    // xsaSha512Rsa
    caaPS256,    // xsaSha256RsaPss
    caaPS384,    // xsaSha384RsaPss
    caaPS512,    // xsaSha512RsaPss
    caaES256,    // xsaSha256Ecc256
    caaES384,    // xsaSha256Ecc384
    caaES512,    // xsaSha256Ecc512
    caaEdDSA);   // xsaSha512EdDSA

  /// internal lookup table from ICryptCert Algorithms to X.509 Signature
  CAA_TO_XSA: array[TCryptAsymAlgo] of TXSignatureAlgorithm = (
    xsaSha256Ecc256,  // caaES256
    xsaSha384Ecc384,  // caaES384
    xsaSha512Ecc512,  // caaES512
    xsaNone,          // caaES256K
    xsaSha256Rsa,     // caaRS256
    xsaSha384Rsa,     // caaRS384
    xsaSha512Rsa,     // caaRS512
    xsaSha256RsaPss,  // caaPS256
    xsaSha384RsaPss,  // caaPS384
    xsaSha512RsaPss,  // caaPS512
    xsaNone);         // caaEdDSA

  /// internal lookup table from X.509 Signature Algorithm as text
  XSA_TXT: array[TXSignatureAlgorithm] of RawUtf8 = (
    'none',                           // xsaNone
    'SHA256 with RSA encryption',     // xsaSha256Rsa
    'SHA384 with RSA encryption',     // xsaSha384Rsa
    'SHA512 with RSA encryption',     // xsaSha512Rsa
    'SHA256 with RSA-PSS encryption', // xsaSha256RsaPss
    'SHA384 with RSA-PSS encryption', // xsaSha384RsaPss
    'SHA512 with RSA-PSS encryption', // xsaSha512RsaPss
    'SHA256 with prime256v1 ECDSA',   // xsaSha256Ecc256
    'SHA384 with secp384r1 ECDSA',    // xsaSha384Ecc384
    'SHA512 with secp521r1 ECDSA' ,   // xsaSha512Ecc512
    'SHA512 with Ed25519 ECDSA');     // xsaSha512EdDSA


  /// internal lookup table from X.509 Public Key Algorithm as text
  XKA_TXT: array[TXPublicKeyAlgorithm] of RawUtf8 = (
    '',                    // xkaNone
    'RSA encryption',      // xkaRsa
    'RSA-PSS encryption',  // xkaRsaPss
    'prime256v1 ECDSA',    // xkaEcc256
    'secp384r1 ECDSA',     // xkaEcc384
    'secp521r1 ECDSA',     // xkaEcc512
    'Ed25519 ECDSA');      // xkaEdDSA

  /// internal lookup table from X.509 Public Key Algorithm to our key algorithm
  XKA_TO_CKA: array[TXPublicKeyAlgorithm] of TCryptKeyAlgo = (
    ckaNone,      // xkaNone
    ckaRsa,       // xkaRsa
    ckaRsaPss,    // xkaRsaPss
    ckaEcc256,    // xkaEcc256
    ckaEcc384,    // xkaEcc384
    ckaEcc512,    // xkaEcc512
    ckaEdDSA);    // xkaEdDSA

  /// internal lookup table from our key algorithm to X.509 Public Key Algorithm
  // - this unit does not support all key types yet
  CKA_TO_XKA: array[TCryptKeyAlgo] of TXPublicKeyAlgorithm = (
    xkaNone,      // ckaNone
    xkaRsa,       // ckaRsa
    xkaRsaPss,    // ckaRsaPss
    xkaEcc256,    // ckaEcc256
    xkaEcc384,    // ckaEcc384
    xkaEcc512,    // ckaEcc512
    xkaNone,      // ckaEcc256k
    xkaEdDSA);    // ckaEdDSA

  /// internal lookup table from X.509 Public Key Algorithm to our key algorithm
  // - this unit does not support all key types yet
  // - default to 256-bit hash for RSA and RSA-PSS
  XKA_TO_CAA: array[TXPublicKeyAlgorithm] of TCryptAsymAlgo = (
    caaES256,  // xkaNone
    caaRS256,  // xkaRsa
    caaPS256,  // xkaRsaPss
    caaES256,  // xkaEcc256
    caaES384,  // xkaEcc384
    caaES512,  // xkaEcc512
    caaEdDSA); // xkaEdDSA

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
    '',                        // xkuNone
    '1.3.6.1.5.5.7.3.1',       // xkuServerAuth
    '1.3.6.1.5.5.7.3.2',       // xkuClientAuth
    '1.3.6.1.5.5.7.3.3',       // xkuCodeSigning
    '1.3.6.1.5.5.7.3.4',       // xkuEmailProtection
    '1.3.6.1.5.5.7.3.8',       // xkuTimeStamping
    '1.3.6.1.5.5.7.3.9',       // xkuOcspSigning
    '1.3.6.1.4.1.311.76.8.1'); // xkuMsPublisher

  /// the OID of all known X.509 Signature Algorithms
  // - RSA-PSS store CKA_OID[ckaRsaPss] with the THashAlgo as parameters
  ASN1_OID_SIGNATURE: array[TXSignatureAlgorithm] of RawUtf8 = (
     '',
     '1.2.840.113549.1.1.11',  // xsaSha256Rsa
     '1.2.840.113549.1.1.12',  // xsaSha384Rsa
     '1.2.840.113549.1.1.13',  // xsaSha512Rsa
     '2.16.840.1.101.3.4.2.1', // xsaSha256RsaPss = ASN1_OID_HASH[hfSHA256]
     '2.16.840.1.101.3.4.2.2', // xsaSha384RsaPss = ASN1_OID_HASH[hfSHA384]
     '2.16.840.1.101.3.4.2.3', // xsaSha512RsaPss = ASN1_OID_HASH[hfSHA256]
     '1.2.840.10045.4.3.2',    // xsaSha256Ecc256 = sha256ECDSA
     '1.2.840.10045.4.3.3',    // xsaSha384Ecc384 = sha384ECDSA
     '1.2.840.10045.4.3.4',    // xsaSha512Ecc512 = sha512ECDSA
     '1.3.101.110');           // xsaSha512EdDSA


  ASN1_OID_PKCS1_MGF       = '1.2.840.113549.1.1.8';
  ASN1_OID_PKCS9_EXTREQ    = '1.2.840.113549.1.9.14';

  ASN1_OID_X509_CRL_REASON  = '2.5.29.21';
  ASN1_OID_X509_CRL_INVDATE = '2.5.29.24';
  ASN1_OID_X509_CRL_ISSUER  = '2.5.29.29';

function XsaToSeq(xsa: TXSignatureAlgorithm): TAsnObject;
function OidToXsa(const oid: RawUtf8; out xsa: TXSignatureAlgorithm): boolean;
function OidToXka(const oid, oid2: RawUtf8; out xka: TXPublicKeyAlgorithm): boolean;
function OidToXa(const oid: RawByteString): TXAttr;
function OidToXe(const oid: RawByteString): TXExtension;
function OidToXku(const oid: RawByteString): TXExtendedKeyUsage;
function XkuToOids(usages: TXExtendedKeyUsages): RawByteString;


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
    /// hexadecimal text of the integer assigned by the CA to each certificate
    // - e.g. '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c'
    SerialNumberHex: RawUtf8;
    /// the cryptographic algorithm used by the CA over the TX509.Signed field
    // - match TX509.SignatureAlgorithm field
    Signature: TXSignatureAlgorithm;
    /// decoded AlgorithmIdentifier structure of the stored public key
    SubjectPublicKeyAlgorithm: TXPublicKeyAlgorithm;
    /// date on which the certificate validity period begins
    NotBefore: TDateTime;
    /// date on which the certificate validity period ends
    // - may equal 0 if '99991231235959Z' was stored as "unspecified end date"
    // (see RFC 5280 #4.1.2.5)
    NotAfter: TDateTime;
    /// identifies the entity associated with the public key stored in the
    // subject public key field of this certificate
    Subject: TXName;
    /// identifies the entity that has signed and issued the certificate
    Issuer: TXName;
    /// decoded number of bits of the stored public key
    // - typically 2048 for RSA, or 256 for ECC
    SubjectPublicKeyBits: integer;
    /// public key raw binary
    SubjectPublicKey: RawByteString;
    /// decoded extensions as defined for X.509 v3 certificates
    // - will contain the ready-to-use UTF-8 text of the value
    // - some types are not yet decoded, so you may need to use ExtensionRaw[]
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
    /// decimal text of a positive integer assigned by the CA to each certificate
    // - e.g. '330929475774275458452528262248458246563660'
    function SerialNumberText: RawUtf8;
    /// convert Extension[x] from CSV to an array of RawUtf8
    function ExtensionArray(x: TXExtension): TRawUtf8DynArray;
    /// check a date/time coherency with NotBefore/NotAfter
    function IsValidDate(timeutc: TDateTime = 0): boolean;
      {$ifdef HASINLINE} inline; {$endif}
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
  public
    /// actual to-be-signed Certificate content
    Signed: TXTbsCertificate;
  protected
    fSafe: TLightLock;
    fSignatureValue: RawByteString;
    fSignatureAlgorithm: TXSignatureAlgorithm;
    fPublicKey: ICryptPublicKey;
    fCachedDer: RawByteString;
    fCachedHash: array[THashAlgo] of RawUtf8;
    fCachedPeerInfo: RawUtf8;
    fLastVerifyAuthPublicKey: RawByteString;
    fRawSubjectKeyIdentifier: RawByteString;
    fRawAuthorityKeyIdentifier: TRawByteStringDynArray;
    fIsSelfSigned: boolean;
    fIsRevokedTag: integer; // <0 if revoked, or should = TCryptStoreX509 tag
    procedure AfterLoaded;
    procedure ComputeCachedDer;
    procedure ComputeCachedPeerInfo;
    procedure ComputeCachedHash(algo: THashAlgo);
    function GetIssuerDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetSubjectPublicKeyAlgorithm: RawUtf8;
  public
    /// reset all internal context
    procedure Clear;
    /// verify the digital signature of this Certificate using a X.509 Authority
    // - some errors can be ignored, e.g. cvWrongUsage or cvDeprecatedAuthority
    // - certificate expiration date can be specified instead of current time
    // - use a cache so the next calls with the same Authority will be immediate
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
    /// serialize those fields into ASN.1 PEM text
    function SaveToPem: TCertPem;
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
    /// check if a FingerPrint() hexa hash match a supplied value
    // - will inline the hash computation to avoid temporary string allocation
    function FingerPrintCompare(const Value: RawUtf8;
      Algo: THashAlgo = hfSha1): integer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if a FingerPrint() hexa hash match the hash of another certificate
    // - will inline the hash computation to avoid temporary string allocation
    function FingerPrintCompare(Another: TX509;
      Algo: THashAlgo = hfSha1): integer; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if this certificate has been issued by the specified certificate
    // - ensure Authority xeSubjectKeyIdentifier is in xeAuthorityKeyIdentifier
    function IsAuthorizedBy(Authority: TX509): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// compare two certificates
    function Compare(Another: TX509; Method: TCryptCertComparer = ccmBinary): integer;
    /// return the associated Public Key instance
    // - initialize it from stored Signed.SubjectPublicKey, if needed
    // - may be a faster TCryptPublicKeyOpenSsl, if available
    function PublicKey: ICryptPublicKey;
    /// compute the number of security bits of the digital signature
    // - e.g. 112 for RSA-2048, 128 for ECC-256
    function SignatureSecurityBits: integer;
    /// an array of (DNS) Subject names covered by this Certificate
    // - convert the Extension[xeSubjectAlternativeName] CSV as a RawUtf8 array
    function SubjectAlternativeNames: TRawUtf8DynArray;
    /// an array of (DNS) Subject names covered by the Issuer of this Certificate
    // - convert the Extension[xeIssuerAlternativeName] CSV as a RawUtf8 array
    function IssuerAlternativeNames: TRawUtf8DynArray;
    /// return some multi-line text of the main information of this Certificate
    // - in a layout similar to X509_print() OpenSSL usual formatting
    // - is cached internally for efficiency
    function PeerInfo: RawUtf8;
    /// return the main information of this Certificate into
    procedure ToParsedInfo(out Info: TX509Parsed);
    /// true if the Certificate Issuer is also its Subject
    property IsSelfSigned: boolean
      read fIsSelfSigned;
    /// main properties of the entity associated with the public key stored
    // in this certificate
    // - e.g. for an Internet certificate, Subject[xaCN] is 'synopse.info'
    // - see SubjectDN to retrieve the full Distinguished Name of the Subject
    property Subject: TXAttrNames
      read Signed.Subject.Name;
    /// main properties of the entity that has signed and issued the certificate
    // - e.g. for an Internet certificate, Issuer[xaO] may be 'Let''s Encrypt'
    // - see IssuerDN to retrieve the full Distinguished Name of the Issuer
    property Issuer: TXAttrNames
      read Signed.Issuer.Name;
    /// main extensions as defined for X.509 v3 certificates
    // - will contain the ready-to-use UTF-8 CSV text of each value
    // - Extension[xeSubjectKeyIdentifier] and Extension[xeAuthorityKeyIdentifier]
    // are also useful to validate a full PKI certification paths and trust
    // (as used by function IsAuthorizedBy() method)
    // - see also the SubjectAlternativeName property
    property Extension: TXExtensions
      read Signed.Extension;
    /// raw binary digital signature computed upon Signed.ToDer
    property SignatureValue: RawByteString
      read fSignatureValue;
  published
    /// hexadecimal of a positive integer assigned by the CA to each certificate
    // - e.g. '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c'
    property SerialNumber: RawUtf8
      read Signed.SerialNumberHex;
    /// issuer entity of this Certificate as Distinguished Name text
    // - e.g. 'CN=R3, C=US, O=Let''s Encrypt'
    // - see Issuer[] property to retrieve one specific field of the DN
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
    // - see Subject[] property to retrieve one specific field of the DN
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


{ **************** X.509 Certificate Revocation List (CRL) }

type
  /// known X.509 CRL v2 extensions
  // - standard extensions as defined in RFC 5280 5.2
  // - currently only the mandatory xceAuthorityKeyIdentifier,
  // xceIssuerAlternativeName and xceCrlNumber are handled from and to DER/PEM
  TXCrlExtension = (
    xceNone,
    xceAuthorityKeyIdentifier,
    xceIssuerAlternativeName,
    xceCrlNumber,
    xceDeltaCrlIndicator,
    xceIssuingDistributionPoints,
    xceDeltaCrlDistributionPoints,
    xceAuthorityInformationAccess);

  /// decoded extensions as defined for X.509 v2 CRL
  // - as used in TXTbsCertList.Extension[] and TX509Crt.Extension
  TXCrlExtensions = array[TXCrlExtension] of RawUtf8;

  /// store one revoked certificate as defined in RFC 5280 5.1.2.6 and 5.3
  // and used in TXTbsCertList.Revoked[]
  {$ifdef USERECORDWITHMETHODS}
  TXCrlRevokedCert = record
  {$else}
  TXCrlRevokedCert = object
  {$endif USERECORDWITHMETHODS}
  public
    /// revoked certificates are listed by their raw binary Serial Number
    SerialNumber: RawByteString;
    /// date on which the revocation occurred
    RevocationDate: TDateTime;
    /// non-critical CRL entry extension that identifies the reason for
    // the certificate revocation
    ReasonCode: TCryptCertRevocationReason;
    /// non-critical CRL entry extension that provides the date on which it is
    // known or suspected that the private key was compromised or that the
    // certificate otherwise became invalid
    InvalidityDate: TDateTime;
    /// optional distinguished name (DN) from the issuer field of the
    // certificate that corresponds to this CRL entry
    // - present only if the certificate issuer was not the CRL issuer
    // - should be marked as critital
    CertificateIssuerDN: RawUtf8;
    /// serialize those fields into ASN.1 DER binary
    function ToDer: TAsnObject;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 encoding
    function FromDer(const der: TCertDer): boolean;
  end;

  // a pointer to one revoked certificate as stored in TXTbsCertList.Revoked[]
  PXCrlRevokedCert = ^TXCrlRevokedCert;

  /// the revoked certificates as stored in TXTbsCertList.Revoked[]
  TXCrlRevokedCerts = array of TXCrlRevokedCert;

  /// the X.509 CRL fields, as defined in RFC 5280 and in TX509Crl.Signed
  // - contains information associated with the subject of the certificate
  // and the CA that issued it
  {$ifdef USERECORDWITHMETHODS}
  TXTbsCertList = record
  {$else}
  TXTbsCertList = object
  {$endif USERECORDWITHMETHODS}
  private
  public
    /// describes the version of the encoded X.509 CRL
    // - equals usually 2, once extensions are used
    Version: integer;
    /// the cryptographic algorithm used by the CA over the TX509Crl.Signed field
    // - match TX509Crl.SignatureAlgorithm field
    Signature: TXSignatureAlgorithm;
    /// date on which the CRL validity period begins
    ThisUpdate: TDateTime;
    /// date on which the CRL validity period ends
    // - may equal 0 if the "nextUpdate" Time optional field was not present
    NextUpdate: TDateTime;
    /// identifies the entity that has signed and issued this CRL
    Issuer: TXName;
    /// list of revoked certificates
    Revoked: TXCrlRevokedCerts;
    /// decoded known X.509 CRL v2 extensions as defined in RFC 5280 5.2
    // - will contain the ready-to-use UTF-8 text of the value
    // - only xceAuthorityKeyIdentifier, xceIssuerAlternativeName and xceCrlNumber
    // are decoded yet, so you may need to use ExtensionRaw[] for other values
    Extension: TXCrlExtensions;
    /// raw ASN1_OCTSTR of decoded Extension[] after FromDer()
    ExtensionRaw: array[TXCrlExtension] of RawByteString;
    /// reset all internal context
    procedure Clear;
    /// return the entry in Revoked[] from the supplied binary Serial Number
    // - returns nil if the serial is not found in the internal list
    function FindRevoked(const RawSerialNumber: RawByteString): PXCrlRevokedCert;
    /// serialize those fields into ASN.1 DER binary
    function ToDer: TAsnObject;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #5.1.2 encoding
    function FromDer(const der: TCertDer): boolean;
  end;

  /// a X.509 signed Certificate Revocation List (CRL), as defined in RFC 5280
  TX509Crl = class(TSynPersistent)
  protected
    fCachedDer: RawByteString;
    fSignatureValue: RawByteString;
    fSignatureAlgorithm: TXSignatureAlgorithm;
    fCrlNumber: QWord;
    fRawAuthorityKeyIdentifier: RawByteString; // for TX509CrlList search
    function GetIssuerDN: RawUtf8;
      {$ifdef HASINLINE} inline; {$endif}
    function GetCrlNumber: QWord;
      {$ifdef HASINLINE} inline; {$endif}
    procedure SetCrlNumber(Value: QWord);
  public
    /// actual to-be-signed revoked Certificate List content
    Signed: TXTbsCertList;
    /// raw binary digital signature computed upon Signed.ToDer
    property SignatureValue: RawByteString
      read fSignatureValue;
  public
    /// reset all internal context
    procedure Clear;
    /// append a revoked certificate to the internal list
    // - will initialize the internal fields, if possible
    // - caller should eventually call Sign() with the corresponding CA
    function AddRevocation(const Serial: RawUtf8;
      Reason: TCryptCertRevocationReason; ValidDays: integer = 0;
      Date: TDateTime = 0; CertIssuerDN: RawUtf8 = ''): boolean;
    /// serialize those fields into ASN.1 DER binary
    // - following RFC 5280 #5.1.1 encoding
    // - value is not cached internally
    function SaveToDer: TCertDer;
    /// serialize those fields into ASN.1 PEM text
    function SaveToPem: TCertPem;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #5.1.1 encoding
    function LoadFromDer(const der: TCertDer): boolean;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #5.1.1 encoding
    function LoadFromPem(const pem: TCertPem): boolean;
    /// to be called once any field has been changed to refresh internal caches
    procedure AfterModified;
    /// quickly check if a date is compatible with ThisUpdate/NextUpdate values
    function IsValidDate(TimeUtc: TDateTime): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// check if an hexadecimal Serial Number is part of Signed.Revoked[]
    // - returns crrNotRevoked if this Serial Number was not found, or
    // the notified revocation reason
    function IsRevoked(const SerialNumber: RawUtf8): TCryptCertRevocationReason;
    /// return Signed.Revoked[].SerialNumber values as hexadecimal
    function Revoked: TRawUtf8DynArray;
    /// verify the digital signature of this CRL using a X.509 Authority
    // - the supplied authority should have the cuCrlSign usage
    // - some errors can be ignored, e.g. cvWrongUsage or cvDeprecatedAuthority
    // - detection / revocation date can be specified instead of the current time
    // - this method is thread-safe
    function Verify(Authority: TX509 = nil; IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity;
    /// let a X.509 authority verify the digital signature of this CRL
    // - accept TCryptCertX509 or TCryptCertOpenSsl kind of authorities
    // - the supplied authority should have the cuCrlSign usage
    // - will use internally a TCryptCertX509 instance for the computation
    function VerifyCryptCert(const Authority: ICryptCert;
      IgnoreError: TCryptCertValidities = []; TimeUtc: TDateTime = 0): TCryptCertValidity;
    /// let a X.509 authority compute the digital signature of this CRL
    // - accept TCryptCertX509 or TCryptCertOpenSsl kind of authorities
    // - the supplied authority should have the cuCrlSign usage
    // - will use internally a TCryptCertX509 instance for the computation
    procedure SignCryptCert(const Authority: ICryptCert;
      AuthorityCrlNumber: QWord);
    /// main properties of the entity that has signed and issued the CRL
    // - e.g. for an Internet certificate, Issuer[xaO] may be 'Cloudflare'
    property Issuer: TXAttrNames
      read Signed.Issuer.Name;
  published
    /// the cryptographic algorithm used by the CA over the Signed field
    // - match Signed.Signature internal field
    property SignatureAlgorithm: TXSignatureAlgorithm
      read fSignatureAlgorithm;
    /// issuer entity of this Certificate as Distinguished Name text
    // - e.g. 'CN=Cloudflare Inc ECC CA-3, C=US, O=Cloudflare, O=Inc.'
    // - see Issuer[] property to retrieve one specific field of the DN
    property IssuerDN: RawUtf8
      read GetIssuerDN;
    /// date on which the CRL validity period begins
    property ThisUpdate: TDateTime
      read Signed.ThisUpdate;
    /// date on which the CRL validity period ends
    // - may equal 0 if the "nextUpdate" Time optional field was not present
    property NextUpdate: TDateTime
      read Signed.NextUpdate;
    /// the KeyID of the public key corresponding to the private key used to
    // sign this CRL
    // - this value may be more precise than IssuerDN
    property AuthorityKeyIdentifier: RawUtf8
      read Signed.Extension[xceAuthorityKeyIdentifier];
    /// monotonically increasing sequence number for a given CRL scope and
    // CRL issuer as defined in RFC 5280 5.2.3
    // - any number > 63-bit should use Signed.ExtensionRaw[xceCrlNumber]
    property CrlNumber: QWord
      read GetCrlNumber write SetCrlNumber;
  end;

  /// a dynamic array of TX509Crl instances
  TX509CrlObjArray = array of TX509Crl;

  /// store several TX509Crl instances
  TX509CrlList = class(TSynPersistent)
  protected
    fSafe: TRWLightLock;
    fList: TX509CrlObjArray;
    fCount: integer;
    fDA: TDynArray;
    function GetRevoked: integer;
  public
    /// initialize this instance
    constructor Create; override;
    /// finalize this list
    destructor Destroy; override;
    /// include a X.509 CRL instance to the internal list
    // - from now on, it will be owned by this TX509CrlList class
    // - will replace any existing CRL with its AKID, if older
    procedure Add(Crl: TX509Crl);
    /// include a X.509 CRL from its DER binary representation to the list
    function AddFromDer(const Der: TCertDer): boolean;
    /// append a revoked certificate to the corresponding CRL in the list
    // - will add a new TX509Crl item, if needed
    // - caller should eventually call Sign() with the corresponding CA
    function AddRevocation(const AuthorityKeyIdentifier, Serial: RawUtf8;
      Reason: TCryptCertRevocationReason; ValidDays: integer = 0;
      Date: TDateTime = 0; CertIssuerDN: RawUtf8 = ''): boolean;
    /// search the most recent X.509 CRL of a given authority from its own AKID
    function FindByKeyIssuer(const AuthorityKeyIdentifier: RawUtf8): TX509Crl;
    /// search the most recent X.509 CRL of a given authority from its own AKID
    function FindByKeyIssuerRaw(const AuthorityKeyIdentifier: RawByteString): TX509Crl;
    /// search the most recent X.509 CRL of a given authority from a DNS name
    // - the DNS name value search is case-insensitive
    function FindByAlternativeName(const DnsName: RawUtf8;
      TimeUtc: TDateTime = 0): TX509CrlObjArray;
    /// quickly check if a given certificate was part of one known CRL
    // - the proper CRL(s) will be first checked with AuthorityKeyIdentifiers,
    // then the method will search if the Serial Number is part of it
    // - returns crrNotRevoked is the serial is not known as part of the CRL
    // - returns the reason why this certificate has been revoked otherwise
    function IsRevoked(const AuthorityKeyIdentifiers,
      SerialNumber: RawUtf8): TCryptCertRevocationReason;
    /// quickly check if a given certificate was part of one known CRL
    // - internal method directly working on binary buffers
    function IsRevokedRaw(akid: PRawByteString; n: integer;
      const sn: RawByteString): TCryptCertRevocationReason;
    /// return a copy of the internal list items
    // - the list is sorted by AuthorityKeyIdentifier and CrlNumber
    // - caller should NOT free the returned items
    function List: TX509CrlObjArray;
    /// persist all stored CRL in PEM format
    procedure SaveToPem(W: TTextWriter; WithExplanatoryText: boolean = false);
  published
    /// how many CRL are currently in the list
    property Count: integer
      read fCount;
    /// how many certificates are currently revoked
    property Revoked: integer
      read GetRevoked;
  end;


const
  /// the OID of all known X.509 CRL v2 extensions, as in RFC 5280 5.2
  XCE_OID: array[TXCrlExtension] of PUtf8Char = (
    '',                     // xceNone
    '2.5.29.35',            // xceAuthorityKeyIdentifier
    '2.5.29.18',            // xceIssuerAlternativeName
    '2.5.29.20',            // xceCrlNumber
    '2.5.29.27',            // xceDeltaCrlIndicator
    '2.5.29.28',            // xceIssuingDistributionPoints
    '2.5.29.46',            // xceDeltaCrlDistributionPoints
    '1.3.6.1.5.5.7.1.1');   // xceAuthorityInformationAccess

function OidToXce(const oid: RawByteString): TXCrlExtension;


{ **************** X.509 Private Key Infrastructure (PKI) }

type
  /// 'x509-pki' ICryptStore algorithm using TX509 and TX509Crl for its process
  TCryptStoreAlgoX509 = class(TCryptStoreAlgo)
  public
    function New: ICryptStore; override; // = TCryptStoreX509.Create(self)
  end;

  /// 'x509-pki' ICryptStore using TX509 and TX509Crl as a full featured PKI
  // - will maintain a cache of ICryptCert instances, a list of trusted
  // certificates, a list of signed CRL (received from a CA) and a list
  // of unsigned CRL (manually registered via the Revoke method)
  // - Certification Path Validation follows RFC 5280 section 6 requirements
  // - published here to make it expandable if needed (by inheritance)
  TCryptStoreX509 = class(TCryptStore)
  protected
    fTrust: TCryptCertList;
    fCA: TCryptCertList;
    fSignedCrl: TX509CrlList;   // from a CA
    fUnsignedCrl: TX509CrlList; // from manual Revoke()
    fValidDepth: integer;
    fIsRevokedTag: integer; // always >= 0 - to store in TX509.fIsRevokedTag
    function GetRevoked: integer;
    function GetCacheCount: integer;
    function GetCACount: integer;
    function ComputeIsRevoked(cert: TX509): TCryptCertRevocationReason;
  public
    /// initialize a 'x509-pki' ICryptStore
    constructor Create(algo: TCryptAlgo); override;
    /// finalize this ICryptStore instance
    destructor Destroy; override;
    /// check both signed CRL list (for CA) and unsigned CRL list (manual Revoke)
    // - use TX509.fIsRevokedTag instance cache if possible
    function IsRevokedX509(cert: TX509): TCryptCertRevocationReason;
      {$ifdef HASINLINE} inline; {$endif}
    // ICryptStore methods
    procedure Clear; override;
    function Save: RawByteString; override;
    function GetBySerial(const Serial: RawUtf8): ICryptCert; override;
    function GetBySubjectKey(const Key: RawUtf8): ICryptCert; override;
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer): ICryptCert; override;
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; override;
    function Add(const cert: ICryptCert): boolean; override;
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray; override;
    function Revoke(const Cert: ICryptCert; Reason: TCryptCertRevocationReason;
      RevocationDate: TDateTime): boolean; override;
    function IsValid(const cert: ICryptCert;
      date: TDateTime): TCryptCertValidity; override;
    function Verify(const Signature: RawByteString; Data: pointer; Len: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity; override;
    function Count: integer; override;
    function CrlCount: integer; override;
    function DefaultCertAlgo: TCryptCertAlgo; override;
  public
    /// how many levels IsValid() should iterate over the trusted certificates
    // before finding a self-signed "root anchor"
    // - equals 32 by default which is more than enough for most PKI, and don't
    // affect performance because most context is cached during the process
    // - 0 would mean that IsValid() checks for a single issuer in the known
    // Trust[] list, and consider it successful even if the "root anchor" was
    // not reached - but may be unsafe if the "root anchor" has been revoked
    property ValidDepth: integer
      read fValidDepth write fValidDepth;
    /// access to the internal trusted ICryptCert list of this PKI
    // - i.e. all trusted certificates, some being self-signed "root anchors",
    // others being intermediate certificates
    // - you usually should not need to access this list, but call ICryptStore
    // IsValid() and IsRevoked() convenient methods
    property Trust: TCryptCertList
      read fTrust;
    /// access to the internal list of CRL signed by some root CA
    // - never change directly, but in the context of its CA owner
    property SignedCrl: TX509CrlList
      read fSignedCrl;
    /// access to the internal list of unsigned CRL, set by Revoke() calls
    // - will be persisted as PEM with no signature, which is mORMot-specific
    // - never change directly, e.g. don't call UnSignedCrl.AddRevocation()
    property UnsignedCrl: TX509CrlList
      read fUnsignedCrl;
  published
    /// how many trusted certificates are actually stored
    property Trusted: integer
      read Count;
    /// how many certificates are currently revoked
    // - in both SignedCrl and UnsignedCrl lists
    property Revoked: integer
      read GetRevoked;
    /// how many ICryptCert instances are actually cached
    property Cached: integer
      read GetCacheCount;
  end;

  /// maintain a cache of X.509 ICryptCert instances, from their DER/binary
  // - to speed up typical PKI process, no DER parsing would be necessary
  // and ECC/RSA digital signature verifications and certificate revocation
  // state will be cached at TX509 level, in this bounded context
  // - this class is thread-safe and will flush its oldest entries automatically
  TCryptCertCacheX509 = class(TCryptCertCache)
  protected
    // properly overidden to call X509Load() and return a TCryptCertX509
    function InternalLoad(const Cert: RawByteString): ICryptCert; override;
  end;


{ ******** Registration of our X.509 Engine to the TCryptCert/TCryptStore Factories }

/// high-level function to decode X.509 certificate main properties using TX509
// - assigned to mormot.core.secure X509Parse() redirection by this unit
function TX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;

/// compute a new ICryptCert instance from DER or PEM input
// - returns nil if the input is not correct or not supported
// - or returns a TCryptCertX509 instance from function TX509.LoadFromDer()
// - will guess the proper TCryptCertAlgoX509 to use for the ICryptCert
// - called e.g. by TCryptCertCacheX509 which is the preferred factory
function X509Load(const Cert: RawByteString): ICryptCert;

/// could be called once additional CryptPublicKey[] / CryptPrivateKey[]
// factories have been defined
// - is called once in this unit initialization section to register the already
// known algorithms, i.e. ckaRsa, ckaRsaPss and ckaEcc256
// - can be called after RegisterOpenSsl from mormot.crypt.openssl to register
// TCryptCertAlgoX509 with the ckaEcc384, ckaEcc512 and ckaEdDSA algorithms
procedure RegisterX509;

{
  NOTICE:
  - the algorithms of this unit are available as 'x509-es256' and 'x509-rs256'
    to 'x509-ps256', and 'x509-pki'
  - mormot.crypt.secure also exposes CryptCertX509[] and CryptStoreX509 globals
  - if OpenSSL is loaded, the 'x509-*' algorithms will be overriden but you can
    still have access to 'x509-rs256-int' 'x509-ps256-int' and 'x509-es256-int'
  - if OpenSSL is loaded and RegisterX509 is called, all algorithms would be
    available from this unit
  - if OpenSSL is loaded, this unit will use it RSA and ECC-256 key work, so it
    is likely to be faster and more complete than OpenSSL certs
  - they are fully compatible with X.509 certificates, and the 'x509-pki' store
    from TCryptStoreX509 is the only fully compliant with RFC recommendations
}

type
  /// abstract parent class implementing ICryptCert using our TX509 class
  // - will store a certificate as TX509 and an abstract ICryptPrivateKey
  // - is the parent of both TCryptCertX509 in this unit and TCryptCertPkcs11
  // in mormot.crypt.pkcs11.pas
  TCryptCertX509Abstract = class(TCryptCert)
  protected
    fX509: TX509;
    // may be nil or a TCryptPrivateKeyRsa, TCryptPrivateKeyEcc,
    // TCryptPrivateKeyOpenSsl or a TCryptPrivateKeyPkcs11
    fPrivateKey: ICryptPrivateKey;
    // overriden to use a faster search with no temporary memory allocation
    class procedure InternalFind(Cert: PICryptCert; const Value: RawByteString;
      Method: TCryptCertComparer; Count, MaxCount: integer;
      out Chain: ICryptCerts); override;
  public
    destructor Destroy; override;
    procedure Clear; virtual;
    // ICryptCert methods - redirected to the internal TX509 instance
    function GetSerial: RawUtf8; override;
    function GetSubjectName: RawUtf8; override;
    function GetSubject(const Rdn: RawUtf8): RawUtf8; override;
    function GetSubjects: TRawUtf8DynArray; override;
    function GetIssuerName: RawUtf8; override;
    function GetIssuer(const Rdn: RawUtf8): RawUtf8; override;
    function GetIssuers: TRawUtf8DynArray; override;
    function GetSubjectKey: RawUtf8; override;
    function GetAuthorityKey: RawUtf8; override;
    function IsSelfSigned: boolean; override;
    function IsAuthorizedBy(const Authority: ICryptCert): boolean; override;
    function Compare(const Another: ICryptCert; Method: TCryptCertComparer): integer; override;
    function GetNotBefore: TDateTime; override;
    function GetNotAfter: TDateTime; override;
    function IsValidDate(date: TDateTime): boolean; override;
    function GetUsage: TCryptCertUsages; override;
    function GetPeerInfo: RawUtf8; override;
    function GetSignatureInfo: RawUtf8; override;
    function GetDigest(Algo: THashAlgo): RawUtf8; override;
    function GetPublicKey: RawByteString; override;
    function HasPrivateSecret: boolean; override;
    function GetPrivateKey: RawByteString; override;
    function Handle: pointer; override; // a TX509 instance
    function GetKeyParams(out x, y: RawByteString): boolean; override;
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity; override;
    function Verify(const Authority: ICryptCert; IgnoreError: TCryptCertValidities;
      TimeUtc: TDateTime): TCryptCertValidity; override;
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; override;
    function SharedSecret(const pub: ICryptCert): RawByteString; override;
    function PrivateKeyHandle: pointer; override; // a ICryptPrivateKey weak ref
    property X509: TX509
      read fX509;
  end;


implementation


{ **************** X.509 Fields Logic}

function XNameAsDN(const Der: TAsnObject): RawUtf8;
var
  x: TXName;
begin
  x.Clear;
  if x.FromAsn(Der) then
    result := x.AsDNText
  else
    result := '';
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
      if SortDynArrayRawByteString(o^.Oid, OidBinary) = 0 then // O(n) search
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
    xsaNone:
      result := Asn(ASN1_SEQ, [
                      Asn(ASN1_OBJID, [])]); // e.g. for fUnsignedCrl
    xsaSha256Rsa .. xsaSha512Rsa:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                  ASN1_NULL_VALUE // optional parameters
                ]);
    xsaSha256RsaPss .. xsaSha512RsaPss:
      // ASN1_OID_SIGNATURE[xsa] is the hash algorithm for RSA-PSS
      result :=
        Asn(ASN1_SEQ, [
          AsnOid(pointer(CKA_OID[ckaRsaPss])),
          Asn(ASN1_SEQ, [ // RSASSA-PSS-params - see RFC 8017 A.2.3
            Asn(ASN1_CTC0, [Asn(ASN1_SEQ, [ // HashAlgorithm
                              AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                              ASN1_NULL_VALUE
                              ])
                           ]),
            Asn(ASN1_CTC1, [Asn(ASN1_SEQ, [ // MaskGenAlgorithm
                              AsnOid(ASN1_OID_PKCS1_MGF),
                              Asn(ASN1_SEQ, [
                                AsnOid(pointer(ASN1_OID_SIGNATURE[xsa])),
                                ASN1_NULL_VALUE
                              ])
                            ])
                           ]),
            Asn(ASN1_CTC2, [Asn(HASH_SIZE[XSA_TO_HF[xsa]])]) // saltLength
          ])
        ]);
    xsaSha256Ecc256 .. xsaSha512EdDSA:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(ASN1_OID_SIGNATURE[xsa]))
                ]);
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
            (AsnNext(p, seq, @oid) = ASN1_OBJID); // decode OID as text
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
      // TODO: support non-standard saltLength as generated by OpenSSL? :(
      // OpenSSL puts e.g. saltLength=222 for Sha256 but FIPS 185-4 5.5 states
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
    if SortDynArrayRawByteString(oid, ASN1_OID_SIGNATURE[x]) = 0 then
    begin
      xsa := x;
      result := true;
      exit;
    end;
  result := false;
end;

function OidToXka(const oid, oid2: RawUtf8; out xka: TXPublicKeyAlgorithm): boolean;
begin
  xka := CKA_TO_XKA[OidToCka(oid, oid2)];
  result := xka <> xkaNone;
end;

var
  // fast OID binary comparison search - initialized at unit startup
  XA_OID_ASN: array[TXAttr] of TAsnObject;
  XE_OID_ASN: array[TXExtension] of TAsnObject;
  XCE_OID_ASN: array[TXCrlExtension] of TAsnObject;
  XKU_OID_ASN: array[TXExtendedKeyUsage] of TAsnObject;

function OidToXa(const oid: RawByteString): TXAttr;
begin
  for result := succ(low(result)) to high(result) do
    if SortDynArrayRawByteString(oid, XA_OID_ASN[result]) = 0 then
      exit;
  result := xaNone;
end;

function OidToXe(const oid: RawByteString): TXExtension;
begin
  for result := succ(low(result)) to high(result) do
    if SortDynArrayRawByteString(oid, XE_OID_ASN[result]) = 0 then
      exit;
  result := xeNone;
end;

function OidToXce(const oid: RawByteString): TXCrlExtension;
begin
  for result := succ(low(result)) to high(result) do
    if SortDynArrayRawByteString(oid, XCE_OID_ASN[result]) = 0 then
      exit;
  result := xceNone;
end;

function OidToXku(const oid: RawByteString): TXExtendedKeyUsage;
begin
  for result := succ(low(result)) to high(result) do
    if SortDynArrayRawByteString(oid, XKU_OID_ASN[result]) = 0 then
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
            Append(one, Asn(ASN1_SEQ, [
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
                        Asn(ASN1_SEQ, [
                          Asn(ASN1_OBJID, [Oid]),
                          AsnText(Value)
                        ])
                      ]));
      fCachedAsn := Asn(ASN1_SEQ, [tmp]);
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

function TXName.Compare(var Another: TXName): integer;
begin
  // update cache manually to avoid temporary strings with ToBinary calls
  if fCachedAsn = '' then
    ComputeAsn;
  if Another.fCachedAsn = '' then
    Another.ComputeAsn;
  result := SortDynArrayRawByteString(fCachedAsn, Another.fCachedAsn);
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

procedure TXName.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
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
  if Rdn = '' then
    result := ''
  else if TextToXa(Rdn, xa) then
    result := Name[xa]
  else if IsDer(Rdn) then
    result := ToBinary
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
  Subject.Name[xaCN] := GetFirstCsvItem(sub);
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
  Append(result, Asn(ASN1_SEQ, [
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
    Asn(ASN1_SEQ, ASN1_BOOLEAN_NONE[cuCA in usages]), {critical=}true);
  // RFC 5280 #4.2.1.3
  if xku <> [] then
    AddExt(result, xeKeyUsage,
      Asn(ASN1_BITSTR, [KuToBitStr(xku)]), {critical=}true);
  // RFC 5280 #4.2.1.12
  if xeku <> [] then
    AddExt(result, xeExtendedKeyUsage,
      Asn(ASN1_SEQ, [XkuToOids(xeku)]));
  // ext[] RawUtf8 are used as source
  // - ExtensionOther[] and ExtensionRaw[] are ignored
  // RFC 5280 #4.2.1.2
  if ext[xeSubjectKeyIdentifier] <> '' then
    AddExt(result, xeSubjectKeyIdentifier,
      Asn(ASN1_OCTSTR, [HumanHexToBin(ext[xeSubjectKeyIdentifier])]));
  // RFC 5280 #4.2.1.1
  if ext[xeAuthorityKeyIdentifier] <> '' then
    AddExt(result, xeAuthorityKeyIdentifier,
      Asn(ASN1_SEQ, [
        Asn(ASN1_CTX0, [HumanHexToBin(ext[xeAuthorityKeyIdentifier])])]));
  // RFC 5280 #4.2.1.6
  if ext[xeSubjectAlternativeName] <> '' then
    AddExt(result, xeSubjectAlternativeName,
      Asn(ASN1_SEQ, [
        CsvToDns(pointer(ext[xeSubjectAlternativeName]))]));
  // RFC 5280 #4.2.1.7
  if ext[xeIssuerAlternativeName] <> '' then
    AddExt(result, xeIssuerAlternativeName,
      Asn(ASN1_SEQ, [
        CsvToDns(pointer(ext[xeIssuerAlternativeName]))]));
  // non-standard ext - but defined as TCryptCertFields.Comment
  if ext[xeNetscapeComment] <> '' then
    AddExt(result, xeNetscapeComment,
      Asn(ASN1_IA5STRING, [ext[xeNetscapeComment]]));
  // xeAuthorityInformationAccess and xeCertificatePolicies not yet persisted
end;


{ **************** X.509 Certificates and Certificate Signing Request (CSR) }

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
                 Asn(ASN1_SEQ, [ComputeExtensions])
               ]);
      fCachedDer := Asn(ASN1_SEQ, [
                      Asn(ASN1_CTC0, [{%H-}Asn(Version - 1)]),
                      Asn(ASN1_INT, [SerialNumber]),
                      XsaToSeq(Signature),
                      Issuer.ToBinary,
                      Asn(ASN1_SEQ, [
                        AsnTime(NotBefore),
                        AsnTime(NotAfter)
                      ]),
                      Subject.ToBinary,
                      X509PubKeyToDer(XKA_TO_CKA[SubjectPublicKeyAlgorithm],
                        SubjectPublicKey),
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
  SerialNumberHex := HumanRandomID;
  SerialNumber := HumanHexToBin(SerialNumberHex);
  Extension[xeSubjectKeyIdentifier] := HumanRandomID;
  // ValidDays and ExpireDays are relative to the current time
  start := NowUtc;
  NotBefore := start + ValidDays;
  NotAfter := start + ExpireDays;
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

function TXTbsCertificate.IsValidDate(timeutc: TDateTime): boolean;
begin
  if timeutc = 0 then
    timeutc := NowUtc;
  result := ((NotAfter = 0) or
             (timeutc < NotAfter + CERT_DEPRECATION_THRESHOLD)) and
            ((NotBefore = 0) or
             (timeutc + CERT_DEPRECATION_THRESHOLD > NotBefore));
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

procedure TX509.Clear;
begin
  Signed.Clear;
  fCachedDer := '';
  Finalize(fCachedHash);
  fCachedPeerInfo := '';
  fLastVerifyAuthPublicKey := '';
  fRawSubjectKeyIdentifier := '';
  fRawAuthorityKeyIdentifier := nil;
  fSignatureAlgorithm := xsaNone;
  fSignatureValue := '';
  fPublicKey := nil;
end;

function CanVerify(auth: TX509; usage: TCryptCertUsage; selfsigned: boolean;
  ignored: TCryptCertValidities; timeutc: TDateTime): TCryptCertValidity;
begin
  if auth = nil then
    result := cvUnknownAuthority
  else if (not (cvWrongUsage in ignored)) and
          (not (selfsigned or (usage in auth.Signed.CertUsages))) then
    result := cvWrongUsage
  else if (cvDeprecatedAuthority in ignored) or
     auth.Signed.IsValidDate(timeutc) then
    result := cvValidSigned
  else
    result := cvDeprecatedAuthority;
end;

function TX509.Verify(Authority: TX509; IgnoreError: TCryptCertValidities;
  TimeUtc: TDateTime): TCryptCertValidity;
begin
   result := cvBadParameter;
   if self = nil then
     exit;
   // check the supplied Authority
   if IsSelfSigned then
     Authority := self
   else if Authority <> nil then
   begin
     result := cvInvalidSignature;
     if (SignatureAlgorithm = xsaNone) or
        (Authority.Signed.SubjectPublicKey = '') or
        (Authority.Signed.SubjectPublicKeyAlgorithm <>
          XSA_TO_XKA[SignatureAlgorithm]) then
       exit;
     result := cvUnknownAuthority;
     if not IsAuthorizedBy(Authority) then
       exit; // Auth xeSubjectKeyIdentifier is not in xeAuthorityKeyIdentifier
   end;
   // check the verification context (e.g. date, usage)
   result := CanVerify(
     Authority, cuKeyCertSign, Authority = self, IgnoreError, TimeUtc);
   if result <> cvValidSigned then
     exit;
   // verify the digital signature
   result := cvInvalidSignature;
   if (fLastVerifyAuthPublicKey = '') or
      (SortDynArrayRawByteString(fLastVerifyAuthPublicKey,
        Authority.Signed.SubjectPublicKey) <> 0) then
   begin
     // check signature with asymmetric RSA or ECC cryptography
     if Signed.fCachedDer = '' then
       Signed.ComputeCachedDer;
     if (Authority.PublicKey = nil) or
        not Authority.fPublicKey.Verify(XSA_TO_CAA[SignatureAlgorithm],
              Signed.fCachedDer, SignatureValue) then
        exit;
     // don't call slow PublicKey.Verify() the next time with this authority
     fLastVerifyAuthPublicKey := Authority.Signed.SubjectPublicKey;
   end;
   // if we reached here, this certificate content has been verified
   if Authority = self then
     result := cvValidSelfSigned
   else
     result := cvValidSigned;
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
    if fPublicKey.Verify(
         XSA_TO_CAA[SignatureAlgorithm], Data, Sig, DataLen, SigLen) then
      if IsSelfSigned then
        result := cvValidSelfSigned
      else
        result := cvValidSigned
    else
      result := cvInvalidSignature;
end;

function TX509.PublicKey: ICryptPublicKey;
var
  cka: TCryptKeyAlgo;
begin
  result := nil;
  if self = nil then
    exit;
  result := fPublicKey;
  if (result <> nil) or
     (Signed.SubjectPublicKey = '') or
     (Signed.SubjectPublicKeyAlgorithm = xkaNone) then
    exit;
  fSafe.Lock;
  try
    if fPublicKey = nil then
    begin
      cka := XKA_TO_CKA[Signed.SubjectPublicKeyAlgorithm];
      fPublicKey := CryptPublicKey[cka].Create;
      if fPublicKey.Load(cka, Signed.SubjectPublicKey) then
        result := fPublicKey
      else
        fPublicKey := nil;
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

function TX509.SaveToPem: TCertPem;
begin
  result := DerToPem(SaveToDer, pemCertificate);
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

procedure TX509.AfterLoaded;
var
  akid: TRawUtf8DynArray;
  i: PtrInt;
begin
  ToHumanHex(Signed.SerialNumberHex, pointer(Signed.SerialNumber), length(Signed.SerialNumber));
  Signed.SubjectPublicKeyBits := X509PubKeyBits(Signed.SubjectPublicKey);
  if (fCachedDer = '') and
     (SignatureValue <> '') then // not possible yet (e.g. after LoadFromCsr)
    ComputeCachedDer;
  if Signed.Issuer.fCachedAsn = '' then
    Signed.Issuer.ComputeAsn;
  if Signed.Subject.fCachedAsn = '' then
    Signed.Subject.ComputeAsn;
  fIsSelfSigned := Signed.Issuer.fCachedAsn = Signed.Subject.fCachedAsn;
  HumanHexToBin(Signed.Extension[xeSubjectKeyIdentifier], fRawSubjectKeyIdentifier);
  CsvToRawUtf8DynArray(pointer(Signed.Extension[xeAuthorityKeyIdentifier]), akid);
  SetLength(fRawAuthorityKeyIdentifier, length(akid));
  for i := 0 to length(akid) - 1 do
    HumanHexToBin(akid[i], fRawAuthorityKeyIdentifier[i]);
  fIsRevokedTag := 0;
  fLastVerifyAuthPublicKey := '';
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
      Finalize(fCachedHash);
      fCachedPeerInfo := '';
      fCachedDer := Asn(ASN1_SEQ, [
                      Signed.ToDer,
                      XsaToSeq(SignatureAlgorithm),
                      Asn(ASN1_BITSTR, [SignatureValue])
                    ]);
    end;
    AfterLoaded;
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

procedure TX509.ComputeCachedHash(algo: THashAlgo);
var
  tmp: RawUtf8;
begin
  tmp := HashFull(algo, SaveToDer); // SaveToDer should be done outside the lock
  fSafe.Lock;
  try
    fCachedHash[algo] := tmp;
  finally
    fSafe.UnLock;
  end;
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
  AfterLoaded;
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
     PublicKey.Verify(XSA_TO_CAA[xsa], nfo, sig) then // check self-signature
  begin
    // load any extensionRequest (PKCS #9 via CRMF)
    if (AsnNext(posnfo, nfo) = ASN1_CTC0) and // optional attributes sequence
       (AsnNext(posnfo, nfo) = ASN1_SEQ) and
       (AsnNext(posnfo, nfo, @oid) = ASN1_OBJID) and
       (oid = ASN1_OID_PKCS9_EXTREQ) and
       (AsnNext(posnfo, nfo) = ASN1_SETOF) and
       (AsnNext(posnfo, nfo) = ASN1_SEQ) then
      Signed.AddNextExtensions(posnfo, nfo);
    AfterLoaded;
    result := true;
  end;
end;

function TX509.FingerPrint(algo: THashAlgo): RawUtf8;
begin
  if self = nil then
    result := ''
  else
  begin
    result := fCachedHash[algo];
    if result <> '' then
      exit;
    ComputeCachedHash(algo);
    result := fCachedHash[algo];
  end;
end;

function TX509.FingerPrintCompare(const Value: RawUtf8; Algo: THashAlgo): integer;
begin
  if fCachedHash[Algo] = '' then
    ComputeCachedHash(Algo);
  result := SortDynArrayAnsiString(fCachedHash[Algo], Value);
end;

function TX509.FingerPrintCompare(Another: TX509; Algo: THashAlgo): integer;
begin
  if Another <> nil then
  begin
    if Another.fCachedHash[Algo] = '' then
      Another.ComputeCachedHash(Algo);
    result := FingerPrintCompare(Another.fCachedHash[algo], Algo);
  end
  else
    result := 1;
end;

function TX509.IsAuthorizedBy(Authority: TX509): boolean;
var
  n: integer;
  s, a: PRawByteString;
begin
  if (self <> nil) and
     (Authority <> nil) and
     (Authority.Signed.SubjectPublicKey <> '') then
  begin
    // fast search with no memory allocation
    result := true;
    s := @Authority.fRawSubjectKeyIdentifier;
    a := pointer(fRawAuthorityKeyIdentifier);
    if a <> nil then
    begin
      n := PDALen(PAnsiChar(pointer(a)) - _DALEN)^ + _DAOFF;
      repeat
        if SortDynArrayRawByteString(a^, s^) = 0 then
        begin
          if PPointer(a)^ <> PPointer(s)^ then
            a^ := s^; // for a faster pointer comparison next time
          exit;
        end;
        dec(n);
        if n = 0 then
          break;
        inc(a);
      until false;
    end;
  end;
  result := false;
end;

function TX509.Compare(Another: TX509; Method: TCryptCertComparer): integer;
begin
  // no memory allocation occurs during this comparison
  if self <> nil then
    if Another <> nil then
      case Method of
        ccmSerialNumber:
          result := SortDynArrayRawByteString(
                      Signed.SerialNumber, Another.Signed.SerialNumber);
        ccmSubjectName:
          result := Signed.Subject.Compare(Another.Signed.Subject);
        ccmIssuerName:
          result := Signed.Issuer.Compare(Another.Signed.Issuer);
        ccmSubjectCN:
          result := SortDynArrayAnsiString(
                      Signed.Subject.Name[xaCN], Another.Signed.Subject.Name[xaCN]);
        ccmIssuerCN:
          result := SortDynArrayAnsiString(
                      Signed.Issuer.Name[xaCN], Another.Signed.Issuer.Name[xaCN]);
        ccmSubjectKey:
          result := SortDynArrayRawByteString(
                      Signed.ExtensionRaw[xeSubjectKeyIdentifier],
                      Another.Signed.ExtensionRaw[xeSubjectKeyIdentifier]);
        ccmAuthorityKey:
          result := SortDynArrayRawByteString(
                      Signed.ExtensionRaw[xeAuthorityKeyIdentifier],
                      Another.Signed.ExtensionRaw[xeAuthorityKeyIdentifier]);
        ccmSubjectAltName:
          result := SortDynArrayAnsiString(
            Signed.Extension[xeSubjectAlternativeName],
            Another.Signed.Extension[xeSubjectAlternativeName]);
        ccmIssuerAltName:
          result := SortDynArrayAnsiString(
            Signed.Extension[xeIssuerAlternativeName],
            Another.Signed.Extension[xeIssuerAlternativeName]);
        ccmUsage:
          result := word(Signed.CertUsages) - word(Another.Signed.CertUsages);
        ccmBinary: // fCachedDer should have been set by AfterLoaded
          result := SortDynArrayRawByteString(fCachedDer, Another.fCachedDer);
        ccmSha1:
          result := FingerPrintCompare(Another, hfSHA1);
        ccmSha256:
          result := FingerPrintCompare(Another, hfSHA256);
      else
        result := ComparePointer(self, Another); // e.g. ccmInstance
      end
    else
      result := 1
  else if Another = nil then
    result := 0
  else
    result := -1;
end;

function TX509.SignatureSecurityBits: integer;
begin
  if (self <> nil) and
     (SignatureAlgorithm <> xsaNone) then
    result := GetSignatureSecurityBits(
      XSA_TO_CAA[SignatureAlgorithm], length(SignatureValue))
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

function TX509.IssuerAlternativeNames: TRawUtf8DynArray;
begin
  if self = nil then
    result := nil
  else
    result := Signed.ExtensionArray(xeIssuerAlternativeName);
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
    Finalize(fCachedHash);
    fCachedPeerInfo := '';
    fLastVerifyAuthPublicKey := '';
    fSignatureValue := '';
  finally
    fSafe.UnLock;
  end;
  Signed.AfterModified;
end;


{ **************** X.509 Certificate Revocation List (CRL) }

{ TXCrlRevokedCert }

function TXCrlRevokedCert.ToDer: TAsnObject;
var
  ext: RawByteString;
begin
  if not (ReasonCode in [crrUnspecified, crrNotRevoked]) then
    ext := Asn(ASN1_SEQ, [
             AsnOid(ASN1_OID_X509_CRL_REASON),
             Asn(ASN1_OCTSTR, [Asn(ord(ReasonCode), ASN1_ENUM)])
           ]);
  if InvalidityDate <> 0 then
    Append(ext, Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_X509_CRL_INVDATE),
                  Asn(ASN1_OCTSTR, [AsnTime(InvalidityDate)])
                ]));
  if CertificateIssuerDN <> '' then
    Append(ext, Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_X509_CRL_ISSUER),
                  Asn(ASN1_OCTSTR, [Asn(ASN1_CTX2, [CertificateIssuerDN])])
                ]));
  if ext <> '' then
    ext := Asn(ASN1_SEQ, [ext]);
  result := Asn(ASN1_SEQ, [
              Asn(ASN1_INT, [SerialNumber]),
              AsnTime(RevocationDate),
              ext
            ]);
end;

function TXCrlRevokedCert.FromDer(const der: TCertDer): boolean;
var
  pos, extpos, vpos, vint: integer;
  ext, v, dn: RawByteString;
  oid: RawUtf8;
begin
  result := false;
  pos := 1;
  if (AsnNextRaw(pos, der, SerialNumber) <> ASN1_INT) or
     not AsnNextTime(pos, der, RevocationDate) then
    exit;
  extpos := 1;
  if AsnNextRaw(pos, der, ext) = ASN1_SEQ then
    while (AsnNext(extpos, ext) = ASN1_SEQ) and
          (AsnNext(extpos, ext, @oid) = ASN1_OBJID) and
          (AsnNextRaw(extpos, ext, v) = ASN1_OCTSTR) do
    begin
      vpos := 1;
      if oid = ASN1_OID_X509_CRL_REASON then
      begin
        if AsnNextInt32(vpos, v, vint) = ASN1_ENUM then
        begin
          ReasonCode := TCryptCertRevocationReason(vint);
          if (ReasonCode > high(TCryptCertRevocationReason)) or
             (ReasonCode = crrNotRevoked) then
            ReasonCode := crrUnspecified;
        end;
      end
      else if oid = ASN1_OID_X509_CRL_INVDATE then
        AsnNextTime(vpos, v, InvalidityDate)
      else if oid = ASN1_OID_X509_CRL_ISSUER then
        if AsnNextRaw(vpos, v, dn) = ASN1_CTX2 then
        begin
          EnsureRawUtf8(dn);
          CertificateIssuerDN := dn;
        end;
    end;
  result := true;
end;


{ TXTbsCertList }

procedure TXTbsCertList.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

function TXTbsCertList.FindRevoked(const RawSerialNumber: RawByteString): PXCrlRevokedCert;
var
  n: integer;
begin
  result := pointer(Revoked);
  if result = nil then
    exit;
  n := PDALen(PAnsiChar(result) - _DALEN)^ + _DAOFF;
  repeat
    if SortDynArrayRawByteString(result^.SerialNumber, RawSerialNumber) = 0 then
      exit;
    dec(n);
    if n = 0 then
      break;
    inc(result);
  until false;
  result := nil;
end;

procedure AddCrlExt(var result: TAsnObject; xce: TXCrlExtension;
  const value: RawByteString);
begin
  Append(result, Asn(ASN1_SEQ, [
                   Asn(ASN1_OBJID, [XCE_OID_ASN[xce]]),
                   Asn(ASN1_OCTSTR, [value])
                 ]));
end;

function TXTbsCertList.ToDer: TAsnObject;
var
  nextup, rev, ext: RawByteString;
  i: PtrInt;
begin
  // optional nextUpdate time
  if NextUpdate <> 0 then
    nextup := AsnTime(NextUpdate);
  // compute the revoked certificate(s) sequence content
  for i := 0 to length(Revoked) - 1 do
    Append(rev, Revoked[i].ToDer);
  // export known extensions - no ExtensionRaw[] support yet
  if Extension[xceAuthorityKeyIdentifier] <> '' then
    AddCrlExt(ext, xceAuthorityKeyIdentifier,
      Asn(ASN1_SEQ, [
        Asn(ASN1_CTX0, [HumanHexToBin(Extension[xceAuthorityKeyIdentifier])
      ])]));
  if Extension[xceIssuerAlternativeName] <> '' then
    AddCrlExt(ext, xceIssuerAlternativeName,
      Asn(ASN1_SEQ, [
        CsvToDns(pointer(Extension[xceIssuerAlternativeName]))]));
  if Extension[xceCrlNumber] <> '' then
    AddCrlExt(ext, xceCrlNumber,
      Asn(GetInt64(pointer(Extension[xceCrlNumber])))); // 63-bit resolution
  if ext <> '' then
    ext := Asn(ASN1_CTC0, [Asn(ASN1_SEQ, [ext])]);
  // generate the whole CRL DER content
  result := Asn(ASN1_SEQ, [
              Asn(1),  // write X.509 CRL version 2, including extensions
              XsaToSeq(Signature),
              Issuer.ToBinary,
              AsnTime(ThisUpdate),
              nextup,
              Asn(ASN1_SEQ, [
                rev
              ]),
              ext
            ]);
end;

function TXTbsCertList.FromDer(const der: TCertDer): boolean;
var
  pos, posv, vt, nrev: integer;
  v64: QWord;
  oid, oid2, v, rev, ext: RawByteString;
  xce: TXCrlExtension;
begin
  result := false;
  // decode main CRL fields
  pos := 1;
  if AsnNext(pos, der) <> ASN1_SEQ then
    exit;
  Version := AsnNextInteger(pos, der, vt) + 1;
  if (vt <> ASN1_INT) or
     not (Version in [1..2]) or
     not AsnNextAlgoOid(pos, der, oid, oid2) or
     ((oid <> '') and // allow fUnsignedCrl
      not OidToXsa(oid, Signature)) or
     not Issuer.FromAsnNext(pos, der) or
     not AsnNextTime(pos, der, ThisUpdate) or
     (pos > length(der)) or
     ((ord(der[pos]) in [ASN1_UTCTIME, ASN1_GENTIME]) and // optional
       not AsnNextTime(pos, der, NextUpdate)) or
     (AsnNextRaw(pos, der, v) <> ASN1_SEQ) then
    exit;
  if v <> '' then
  begin
    // retrieve the revoked certificates list
    SetLength(Revoked, 8);
    nrev := 0;
    posv := 1;
    while (AsnNextRaw(posv, v, rev) = ASN1_SEQ) and
          Revoked[nrev].FromDer(rev) do
    begin
      inc(nrev);
      if nrev = length(Revoked) then
        SetLength(Revoked, NextGrow(nrev));
    end;
    if nrev = 0 then
      Revoked := nil
    else
      DynArrayFakeLength(Revoked, nrev);
  end;
  // parse X.509 CRL version 2 extensions
  if (Version >= 2) and
     (AsnNext(pos, der) = ASN1_CTC0) then
    if AsnNext(pos, der) <> ASN1_SEQ then
      exit
    else
      while (AsnNext(pos, der) = ASN1_SEQ) and
            (AsnNextRaw(pos, der, oid) = ASN1_OBJID) and
            (AsnNextRaw(pos, der, v) = ASN1_OCTSTR) do
      begin
        xce := OidToXce(oid);
        if xce = xceNone then
          continue;
        ExtensionRaw[xce] := v;
        posv := 1;
        case xce of
          xceAuthorityKeyIdentifier:
            if (AsnNext(posv, v) = ASN1_SEQ) and
               (AsnNextRaw(posv, v, ext) <> ASN1_NULL) then
              ToHumanHex(Extension[xceAuthorityKeyIdentifier],
                pointer(ext), length(ext));
          xceIssuerAlternativeName:
            if AsnNext(posv, v) = ASN1_SEQ then
              repeat
                case AsnNextRaw(posv, v, ext) of
                  ASN1_NULL:
                    break;
                  ASN1_CTX1, // rfc8722Name
                  ASN1_CTX2, // dnsName
                  ASN1_CTX6: // uri
                    EnsureRawUtf8(v); // was stored as IA5String
                end;
                if v <> '' then
                  AddToCsv(v, Extension[xceIssuerAlternativeName]);
              until false;
          xceCrlNumber:
            begin
              v64 := AsnNextInteger(posv, v, vt);
              if vt = ASN1_INT then
                UInt64ToUtf8(v64, Extension[xceCrlNumber]);
              // any number > 63-bit should use ExtensionRaw[xceCrlNumber]
            end;
        end;
      end;
  result := true;
end;


{ TX509Crl }

function TX509Crl.GetIssuerDN: RawUtf8;
begin
  if self = nil then
    result := ''
  else
    result := Signed.Issuer.AsDNText;
end;

function TX509Crl.GetCrlNumber: QWord;
begin
  if fCrlNumber = 0 then // simple cache
    SetQWord(pointer(Signed.Extension[xceCrlNumber]), fCrlNumber);
  result := fCrlNumber;
end;

procedure TX509Crl.SetCrlNumber(Value: QWord);
begin
  if (self = nil) and
     (Value <> fCrlNumber) then
     exit;
  fCrlNumber := Value;
  UInt64ToUtf8(Value, Signed.Extension[xceCrlNumber]);
end;

procedure TX509Crl.Clear;
begin
  Signed.Clear;
  fCachedDer := '';
  fSignatureAlgorithm := xsaNone;
  fSignatureValue := '';
  fCrlNumber := 0;
end;

function TX509Crl.AddRevocation(const Serial: RawUtf8;
  Reason: TCryptCertRevocationReason; ValidDays: integer;
  Date: TDateTime; CertIssuerDN: RawUtf8): boolean;
var
  n: PtrInt;
  next: TDateTime;
  sn: RawByteString;
begin
  result := false;
  if (self = nil) or
     (Reason = crrNotRevoked) or
     not HumanHexToBin(Serial, sn) then
    exit;
  AfterModified;
  if Date = 0 then
    Date := NowUtc;
  Signed.Version := 2;
  if Signed.ThisUpdate = 0 then
    Signed.ThisUpdate := Date;
  if ValidDays > 0 then
  begin
    next := Date + ValidDays;
    if (Signed.NextUpdate = 0) or
       (Signed.NextUpdate > next) then
      Signed.NextUpdate := next;
  end;
  n := length(Signed.Revoked);
  SetLength(Signed.Revoked, n + 1);
  with Signed.Revoked[n] do
  begin
    SerialNumber := sn;
    RevocationDate := Date;
    ReasonCode := Reason;
    InvalidityDate := Date;
    CertificateIssuerDN := CertIssuerDN;
  end;
  result := true;
end;

function TX509Crl.SaveToDer: TCertDer;
begin
  if fCachedDer = '' then
    fCachedDer := Asn(ASN1_SEQ, [
                    Signed.ToDer,
                    XsaToSeq(SignatureAlgorithm),
                    Asn(ASN1_BITSTR, [SignatureValue])
                  ]);
  result := fCachedDer;
end;

function TX509Crl.SaveToPem: TCertPem;
begin
  result := DerToPem(SaveToDer, pemCrl);
end;

function TX509Crl.LoadFromDer(const der: TCertDer): boolean;
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
            ((oid = '') or // allow fUnsignedCrl
             OidToXsa(oid, fSignatureAlgorithm)) and
            (AsnNextRaw(pos, der, fSignatureValue) = ASN1_BITSTR) and
            (fSignatureAlgorithm = Signed.Signature);
end;

function TX509Crl.LoadFromPem(const pem: TCertPem): boolean;
begin
  result := LoadFromDer(PemToDer(pem));
end;

procedure TX509Crl.AfterModified;
begin
  fCachedDer := '';
end;

function TX509Crl.IsValidDate(TimeUtc: TDateTime): boolean;
begin
  result := (TimeUtc + CERT_DEPRECATION_THRESHOLD > Signed.ThisUpdate) and
            ((Signed.NextUpdate = 0) or
             (Signed.NextUpdate + CERT_DEPRECATION_THRESHOLD < TimeUtc));
end;

function TX509Crl.IsRevoked(const SerialNumber: RawUtf8): TCryptCertRevocationReason;
var
  bin: RawByteString;
  res: PXCrlRevokedCert;
begin
  result := crrNotRevoked;
  if (self = nil) or
     (Signed.Revoked = nil) or
     not HumanHexToBin(SerialNumber, bin) then
    exit;
  res := Signed.FindRevoked(bin);
  if res <> nil then
    result := res^.ReasonCode;
end;

function TX509Crl.Revoked: TRawUtf8DynArray;
var
  i, n: PtrInt;
begin
  if self = nil then
    n := 0
  else
    n := length(Signed.Revoked);
  SetLength(result, n);
  for i := 0 to n - 1 do
    with Signed.Revoked[i] do
      ToHumanHex(result[i], pointer(SerialNumber), length(SerialNumber));
end;

function TX509Crl.Verify(Authority: TX509; IgnoreError: TCryptCertValidities;
  TimeUtc: TDateTime): TCryptCertValidity;
begin
   result := cvBadParameter;
   if (self = nil) or
      (Authority = nil) or
      (PClass(Authority)^ <> TX509) then
     exit;
   result := cvInvalidSignature;
   if (SignatureAlgorithm = xsaNone) or
      (Authority.Signed.SubjectPublicKeyAlgorithm <>
         XSA_TO_XKA[SignatureAlgorithm]) then
     exit;
   result := cvUnknownAuthority;
   if (Authority.Signed.Extension[xeSubjectKeyIdentifier] <>
        Signed.Extension[xceAuthorityKeyIdentifier]) or // no CsvContains() need
      (Authority.Signed.SubjectPublicKey = '') then
     exit;
   result := CanVerify(
     Authority, cuCrlSign, {selfsigned=}false, IgnoreError, TimeUtc);
   if result = cvValidSigned then
     if not Authority.PublicKey.Verify(XSA_TO_CAA[SignatureAlgorithm],
              Signed.ToDer, SignatureValue) then
       result := cvInvalidSignature;
end;


{ TX509CrlList }

function TX509CrlCompareWithAkid(const A, B): integer;
begin
  // FastLocateSorted() calls fCompare(Item, P[n * fInfo.Cache.ItemSize])
  result := SortDynArrayAnsiString(A, TX509Crl(B).fRawAuthorityKeyIdentifier);
end;

constructor TX509CrlList.Create;
begin
  inherited Create;
  fDA.Init(TypeInfo(TX509CrlObjArray), fList, @fCount);
  fDA.Compare := TX509CrlCompareWithAkid; // FastLocateSorted() search by AKID
end;

destructor TX509CrlList.Destroy;
begin
  inherited Destroy;
  ObjArrayClear(fList, fCount);
end;

procedure TX509CrlList.Add(Crl: TX509Crl);
var
  i: integer;
  akid: RawByteString;
begin
  if (Crl = nil) or
     not HumanHexToBin(Crl.Signed.Extension[xceAuthorityKeyIdentifier], akid) then
  begin
    Crl.Free; // avoid memory leak
    exit;
  end;
  Crl.fRawAuthorityKeyIdentifier := akid; // as expected by FastLocateSorted()
  fSafe.WriteLock;
  try
    // use fast O(log(n)) binary search of this AKID
    if fDA.FastLocateSorted(akid, i) then
      // there is already a CRL with this AKID
      if fList[i].CrlNumber < Crl.CrlNumber then
      begin
        fList[i].Free; // replace existing CRL
        fList[i] := Crl;
      end
      else
        Crl.Free // this supplied CRL is older than the existing -> ignore
    else if i >= 0 then
      // add this CRL with its unknown SKID at the expected sorted position
      fDA.FastAddSorted(i, Crl)
    else
      EX509.RaiseUtf8('Inconsistent %.Add order', [self]); // paranoid
  finally
    fSafe.WriteUnLock;
  end;
end;

function TX509CrlList.AddFromDer(const Der: TCertDer): boolean;
var
  crl: TX509Crl;
begin
  result := false;
  crl := TX509Crl.Create;
  try
    if not crl.LoadFromDer(Der) then
      exit;
    Add(crl);
    crl := nil; // will be owned by fList from now on
  finally
    crl.Free;
  end;
  result := true;
end;

function TX509CrlList.AddRevocation(const AuthorityKeyIdentifier,
  Serial: RawUtf8; Reason: TCryptCertRevocationReason; ValidDays: integer;
  Date: TDateTime; CertIssuerDN: RawUtf8): boolean;
var
  i: integer;
  crl: TX509Crl;
  akid: RawByteString;
begin
  result := false;
  if (self = nil) or
     (Serial = '') or
     (Reason = crrNotRevoked) or
     not HumanHexToBin(AuthorityKeyIdentifier, akid) then
    exit;
  fSafe.WriteLock;
  try
    if fDA.FastLocateSorted(akid, i) then
      crl := fList[i]
    else if i >= 0 then
    begin
      crl := TX509Crl.Create;
      crl.Signed.Issuer.Name[xaCN] := Executable.Host;
      crl.Signed.Extension[xceAuthorityKeyIdentifier] := AuthorityKeyIdentifier;
      crl.fRawAuthorityKeyIdentifier := akid; // for internal search
      fDA.FastAddSorted(i, crl);
    end
    else
      EX509.RaiseUtf8('Inconsistent % order', [self]); // paranoid
    result := crl.AddRevocation(Serial, Reason, ValidDays, Date, CertIssuerDN);
    if result then
      crl.CrlNumber := crl.CrlNumber + 1; // ensure in increasing sequence
  finally
    fSafe.WriteUnLock;
  end;
end;

function TX509CrlList.GetRevoked: integer;
var
  i: integer;
begin
  result := 0;
  if (self = nil) or
     (fCount = 0) then
    exit;
  fSafe.ReadLock;
  try
    for i := 0 to fCount - 1 do
      inc(result, length(fList[i].Signed.Revoked));
  finally
    fSafe.ReadUnLock;
  end;
end;

function TX509CrlList.FindByKeyIssuer(const AuthorityKeyIdentifier: RawUtf8): TX509Crl;
var
  akid: RawByteString;
begin
  if HumanHexToBin(AuthorityKeyIdentifier, akid) then
    result := FindByKeyIssuerRaw(akid)
  else
    result := nil;
end;

function TX509CrlList.FindByKeyIssuerRaw(
  const AuthorityKeyIdentifier: RawByteString): TX509Crl;
var
  i: integer;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) then
    exit;
  fSafe.ReadLock;
  try
    // efficient O(log(n)) binary search
    if fDA.FastLocateSorted(AuthorityKeyIdentifier, i) then
      result := fList[i]; // Add() should have made this unique per AKID
  finally
    fSafe.ReadUnLock;
  end;
end;

function TX509CrlList.FindByAlternativeName(const DnsName: RawUtf8;
  TimeUtc: TDateTime): TX509CrlObjArray;
var
  p: ^TX509Crl;
  i: integer;
begin
  result := nil;
  if (self = nil) or
     (fCount = 0) or
     (DnsName = '') then
    exit;
  if TimeUtc = 0 then
    TimeUtc := NowUtc;
  fSafe.ReadLock;
  try
    // brute force O(n) linear search
    p := pointer(fList);
    for i := 1 to fCount do
    begin
      if CsvContains(p^.Signed.Extension[xceIssuerAlternativeName],
           DnsName, ',', {casesensitive=}false) and
         p^.IsValidDate(TimeUtc) then
        ObjArrayAdd(result, p^);
      inc(p);
    end;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TX509CrlList.IsRevoked(const AuthorityKeyIdentifiers,
  SerialNumber: RawUtf8): TCryptCertRevocationReason;
var
  akid: RawUtf8;
  sn: RawByteString;
  p: PUtf8Char;
  crl: TX509Crl;
  res: PXCrlRevokedCert;
begin
  result := crrNotRevoked;
  if (self = nil) or
     (fCount = 0) or
     (AuthorityKeyIdentifiers = '') or
     (SerialNumber = '') then
    exit;
  p := nil;
  akid := AuthorityKeyIdentifiers;
  if PosExChar(',', akid) <> 0 then
    p := pointer(akid); // needs specific CSV process
  fSafe.ReadLock; // reentrant multi-read lock
  try
    repeat
      if p <> nil then
        GetNextItem(p, ',', akid);
      crl := FindByKeyIssuer(akid); // O(log(n)) binary search
      if (crl = nil) or
         not HumanHexToBin(SerialNumber, sn)  then
        continue;
      res := crl.Signed.FindRevoked(sn); // few items O(n) search
      if res = nil then
        continue;
      result := res^.ReasonCode;
      break;
    until p = nil;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TX509CrlList.IsRevokedRaw(akid: PRawByteString; n: integer;
  const sn: RawByteString): TCryptCertRevocationReason;
var
  i: integer;
  res: PXCrlRevokedCert;
begin
  result := crrNotRevoked;
  if (self = nil) or
     (fCount = 0) or
     (akid = nil) or
     (n = 0) or
     (sn = '') then
    exit;
  fSafe.ReadLock; // reentrant multi-read lock
  try
    repeat
      if fDA.FastLocateSorted(akid^, i) then // inlined FindByKeyIssuerRaw()
        with fList[i].Signed do
        begin
          res := FindRevoked(sn); // few items O(n) search
          if res <> nil then
          begin
            result := res^.ReasonCode;
            exit;
          end;
        end;
      dec(n);
      if n = 0 then
        break;
      inc(akid);
    until false;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TX509CrlList.List: TX509CrlObjArray;
begin
  fSafe.ReadLock;
  try
    SetLength(result, fCount);
    MoveFast(pointer(fList)^, pointer(result)^, fCount * SizeOf(TX509Crl));
  finally
    fSafe.ReadUnLock;
  end;
end;

procedure TX509CrlList.SaveToPem(W: TTextWriter; WithExplanatoryText: boolean);
var
  i: PtrInt;
  c: TX509Crl;
begin
  fSafe.ReadLock;
  try
    for i := 0 to fCount - 1 do
    begin
      c := fList[i];
      if WithExplanatoryText then
        // see https://datatracker.ietf.org/doc/html/rfc7468#section-5.2
        W.Add('Issuer: %'#13#10'Validity: from % to %'#13#10'Signature: %'#13#10,
         [c.IssuerDN, DateTimeToIso8601Short(c.ThisUpdate),
          DateTimeToIso8601Short(c.NextUpdate), XSA_TXT[c.SignatureAlgorithm]]);
      W.AddString(c.SaveToPem);
      W.AddCR;
    end;
  finally
    fSafe.ReadUnLock;
  end;
end;



{ **************** X.509 Private Key Infrastructure (PKI) }

function ComputeSelfSignedCsr(const PrivateKey: ICryptPrivateKey;
  Algorithm: TXSignatureAlgorithm; const Subjects: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;
var
  pub, extreq, der: RawByteString;
  caa: TCryptAsymAlgo;
  sub: TXName;
  ext: TXExtensions;
  xu: TXKeyUsages;
  xku: TXExtendedKeyUsages;
begin
  result := '';
  if PrivateKey = nil then
    exit;
  // create a new key pair if not supplied
  caa := XSA_TO_CAA[Algorithm];
  if PrivateKey.KeyAlgo <> ckaNone then
    pub := PrivateKey.ToSubjectPublicKey
  else
    pub := PrivateKey.Generate(caa);
  if pub = '' then
    exit;
  // setup the CSR fields
  FillCharFast(sub, SizeOf(sub), 0);
  CertInfoPrepare(sub, ext, Subjects, Fields);
  extreq := CertInfoCompute(Usages, ext, xu, xku);
  if extreq <> '' then
    // extensionRequest (PKCS #9 via CRMF)
    extreq := Asn(ASN1_CTC0, [
                Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_PKCS9_EXTREQ),
                  Asn(ASN1_SETOF, [
                    Asn(ASN1_SEQ, [extreq])
                  ])
                ])
              ]);
  // compute the main CSR body
  der := Asn(ASN1_SEQ, [
           Asn(0), // version
           sub.ToBinary,
           X509PubKeyToDer(PrivateKey.KeyAlgo, pub),
           extreq
         ]);
  // sign and return the whole CSR as PEM
  result := DerToPem(Asn(ASN1_SEQ, [
                      der,
                      XsaToSeq(Algorithm),
                      Asn(ASN1_BITSTR, [PrivateKey.Sign(caa, der)])
                    ]), pemCertificateRequest);
end;


{ TCryptCertX509Abstract }

destructor TCryptCertX509Abstract.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TCryptCertX509Abstract.Clear;
begin
  FreeAndNil(fX509);
  fPrivateKey := nil;
end;

class procedure TCryptCertX509Abstract.InternalFind(Cert: PICryptCert;
  const Value: RawByteString; Method: TCryptCertComparer;
  Count, MaxCount: integer; out Chain: ICryptCerts);
var
  found: boolean;
  res: integer;
  bin: RawByteString;
begin
  // prepare the search
  case Method of
    ccmSerialNumber,
    ccmSubjectKey:
      if not HumanHexToBin(Value, bin) then
        bin := Value; // allow Value to be in hexadecimal or raw binary
  end;
  if MaxCount <= 0 then
    MaxCount := MaxInt;
  // O(n) efficient search loop with no temporary memory allocation
  res := 0;
  while Count <> 0 do
  begin
    with TX509(Cert^.Handle) do // retrieve the TX509 in a single method call
      case Method of
        ccmSerialNumber:
          found := SortDynArrayRawByteString(Signed.SerialNumber, bin) = 0;
        ccmSubjectName:
          with Signed.Subject do
          begin
            if fCachedText = '' then
              ComputeText;
            found := SortDynArrayAnsiString(fCachedText, Value) = 0;
          end;
        ccmIssuerName:
          with Signed.Issuer do
          begin
            if fCachedText = '' then
              ComputeText;
            found := SortDynArrayAnsiString(fCachedText, Value) = 0;
          end;
        ccmSubjectCN:
          found := IdemPropNameU(Signed.Subject.Name[xaCN], Value);
        ccmIssuerCN:
          found := IdemPropNameU(Signed.Issuer.Name[xaCN], Value);
        ccmSubjectKey:
          found := SortDynArrayRawByteString(
               Signed.ExtensionRaw[xeSubjectKeyIdentifier], bin) = 0;
        ccmAuthorityKey:
          found := CsvContains(Signed.Extension[xeAuthorityKeyIdentifier], Value);
        ccmSubjectAltName:
          found := CsvContains(Signed.Extension[xeSubjectAlternativeName],
                     Value, ',', {casesensitive=}false);
        ccmIssuerAltName:
          found := CsvContains(Signed.Extension[xeIssuerAlternativeName],
                     Value, ',', {casesensitive=}false);
        ccmBinary: // fCachedDer has been set by AfterLoaded
          found := SortDynArrayRawByteString(fCachedDer, Value) = 0;
        ccmSha1:
          found := FingerPrintCompare(Value, hfSHA1) = 0;
        ccmSha256:
          found := FingerPrintCompare(Value, hfSHA256) = 0;
      else
        found := false; // unsupported search method (e.g. ccmUsage)
      end;
    if found then
    begin
      InterfaceArrayAddCount(Chain, res, Cert^);
      dec(MaxCount);
      if MaxCount = 0 then
        break;
    end;
    inc(Cert);
    dec(Count);
  end;
  if res <> length({%H-}Chain) then
    DynArrayFakeLength(Chain, res);
end;

function TCryptCertX509Abstract.GetSerial: RawUtf8;
begin
  if fX509 <> nil then
    result := fX509.Signed.SerialNumberHex
  else
    result := '';
end;

function TCryptCertX509Abstract.GetSubjectName: RawUtf8;
begin
  result := fX509.GetSubjectDN;
end;

function TCryptCertX509Abstract.GetSubject(const Rdn: RawUtf8): RawUtf8;
begin
  result := '';
  if (Rdn = '') or
     (fX509 = nil) then
    exit;
  result := fX509.Signed.Subject.Get(Rdn); // RDN or hash or OID
  if (result = '') and
     IsCN(Rdn) then
    // CN fallback to first DNS: as with mormot.crypt.ecc and mormot.crypt.openssl
    result := GetFirstCsvItem(fX509.Extension[xeSubjectAlternativeName]);
end;

function TCryptCertX509Abstract.GetSubjects: TRawUtf8DynArray;
begin
  result := fX509.SubjectAlternativeNames;
end;

function TCryptCertX509Abstract.GetIssuerName: RawUtf8;
begin
  result := fX509.GetIssuerDN;
end;

function TCryptCertX509Abstract.GetIssuer(const Rdn: RawUtf8): RawUtf8;
begin
  if (Rdn = '') or
     (fX509 = nil) then
    result := ''
  else
    result := fX509.Signed.Issuer.Get(Rdn); // RDN or hash or OID
end;

function TCryptCertX509Abstract.GetIssuers: TRawUtf8DynArray;
begin
  result := fX509.IssuerAlternativeNames;
end;

function TCryptCertX509Abstract.GetSubjectKey: RawUtf8;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Signed.Extension[xeSubjectKeyIdentifier];
end;

function TCryptCertX509Abstract.GetAuthorityKey: RawUtf8;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Signed.Extension[xeAuthorityKeyIdentifier];
end;

function TCryptCertX509Abstract.IsSelfSigned: boolean;
begin
  result := (fX509 <> nil) and
            fX509.IsSelfSigned;
end;

function TCryptCertX509Abstract.IsAuthorizedBy(const Authority: ICryptCert): boolean;
var
  a: TCryptCertX509Abstract;
begin
  if Assigned(Authority) then
  begin
    a := pointer(Authority.Instance);
    result := a.InheritsFrom(TCryptCertX509Abstract) and
      // ensure Authority xeSubjectKeyIdentifier is in xeAuthorityKeyIdentifier
      fX509.IsAuthorizedBy(a.fX509);
  end
  else
    result := false;
end;

function TCryptCertX509Abstract.Compare(const Another: ICryptCert;
  Method: TCryptCertComparer): integer;
var
  a: TCryptCertX509Abstract;
begin
  if Assigned(Another) then
  begin
    a := pointer(Another.Instance);
    if a.InheritsFrom(TCryptCertX509Abstract) then
      result := fX509.Compare(a.fX509, Method)
    else
      result := 1
  end
  else
    result := 1;
end;

function TCryptCertX509Abstract.GetNotBefore: TDateTime;
begin
  if fX509 = nil then
    result := 0
  else
    result := fX509.Signed.NotBefore;
end;

function TCryptCertX509Abstract.GetNotAfter: TDateTime;
begin
  if fX509 = nil then
    result := 0
  else
    result := fX509.Signed.NotAfter;
end;

function TCryptCertX509Abstract.IsValidDate(date: TDateTime): boolean;
begin
  result := (fX509 <> nil) and
            fX509.Signed.IsValidDate(date);
end;

function TCryptCertX509Abstract.GetUsage: TCryptCertUsages;
begin
  if fX509 = nil then
    result := []
  else
    result := fX509.Signed.CertUsages;
end;

function TCryptCertX509Abstract.GetPeerInfo: RawUtf8;
begin
  result := fX509.PeerInfo;
end;

function TCryptCertX509Abstract.GetSignatureInfo: RawUtf8;
var
  bits: integer;
begin
  bits := fX509.SignatureSecurityBits;
  if bits = 0 then
    result := ''
  else
    FormatUtf8('% %', [bits, XSA_TXT[fX509.SignatureAlgorithm]], result);
end;

function TCryptCertX509Abstract.GetDigest(Algo: THashAlgo): RawUtf8;
begin
  result := fX509.FingerPrint(Algo);
end;

function TCryptCertX509Abstract.GetPublicKey: RawByteString;
begin
  if fX509 = nil then
    result := ''
  else
    result := fX509.Signed.SubjectPublicKey;
end;

function TCryptCertX509Abstract.HasPrivateSecret: boolean;
begin
  result := fPrivateKey <> nil;
end;

function TCryptCertX509Abstract.GetPrivateKey: RawByteString;
begin
  if fPrivateKey = nil then
    result := ''
  else
    result := fPrivateKey.ToDer;
end;

function TCryptCertX509Abstract.Verify(Sign, Data: pointer; SignLen, DataLen: integer;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := fX509.Verify(Sign, Data, SignLen, DataLen, IgnoreError, TimeUtc);
end;

function TCryptCertX509Abstract.Verify(const Authority: ICryptCert;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
var
  auth, tempauth: TX509;
begin
  result := cvBadParameter;
  if fX509 = nil then
    exit;
  auth := nil;
  tempauth := nil;
  if Authority <> nil then
    if Authority.Instance.InheritsFrom(TCryptCertX509Abstract) then
      auth := Authority.Handle
    else
    begin
      // create a temp TX509 for the verification (slow, but working)
      tempauth := TX509.Create;
      if tempauth.LoadFromDer(Authority.Save) then
        auth := tempauth;
    end;
  // TX509 has a cache so the next calls with the same auth will be immediate
  result := fX509.Verify(auth, IgnoreError, TimeUtc);
  tempauth.Free;
end;

function TCryptCertX509Abstract.Encrypt(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  if (fX509 <> nil) and
     (fX509.Usages * [cuDataEncipherment, cuEncipherOnly] <> []) then
    result := fX509.PublicKey.Seal(Message, Cipher)
  else
    result := '';
end;

function TCryptCertX509Abstract.Decrypt(const Message: RawByteString;
  const Cipher: RawUtf8): RawByteString;
begin
  if (fX509 <> nil) and
     (fPrivateKey <> nil) and
     (fX509.Usages * [cuDataEncipherment, cuEncipherOnly] <> []) then
    result := fPrivateKey.Open(Message, Cipher)
  else
    result := '';
end;

function TCryptCertX509Abstract.SharedSecret(const pub: ICryptCert): RawByteString;
begin
  if (fX509 <> nil) and
     (fPrivateKey <> nil) and
     (cuKeyAgreement in fX509.Usages) and
     Assigned(pub) and
     pub.Instance.InheritsFrom(TCryptCertX509Abstract) and
     (pub.Handle <> nil) and
     (cuKeyAgreement in TX509(pub.Handle).Usages) then
    result := fPrivateKey.SharedSecret(TX509(pub.Handle).PublicKey)
  else
    result := '';
end;

function TCryptCertX509Abstract.PrivateKeyHandle: pointer;
begin
  result := pointer(fPrivateKey);
end;

function TCryptCertX509Abstract.Handle: pointer;
begin
  result := fX509;
end;

function TCryptCertX509Abstract.GetKeyParams(out x, y: RawByteString): boolean;
begin
  result := fX509.PublicKey.GetParams(x, y);
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
    // TCryptCertAlgo methods
    function New: ICryptCert; override; // = TCryptCertX509.Create(self)
    function FromHandle(Handle: pointer): ICryptCert; override;
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8; override;
  end;

  /// class implementing ICryptCert using our TX509 class
  // - will store a certificate as TX509 and/or a ICryptPrivateKey instance
  TCryptCertX509 = class(TCryptCertX509Abstract)
  protected
    function AlgoXsa: TXSignatureAlgorithm; // from TCryptCertAlgoX509
      {$ifdef HASINLINE} inline; {$endif}
    function AlgoXka: TXPublicKeyAlgorithm; // from TCryptCertAlgoX509
      {$ifdef HASINLINE} inline; {$endif}
    function VerifyAuthority(const Authority: ICryptCert): TCryptCert;
    procedure GeneratePrivateKey;
  public
    // ICryptCert methods - relative to fPrivateKey process
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; override;
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert; override;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; override;
    function Save(Content: TCryptCertContent; const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; override;
    function SetPrivateKey(const saved: RawByteString): boolean; override;
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage): RawByteString; override;
    procedure Sign(const Authority: ICryptCert); override;
  end;


{ TCryptCertAlgoX509 }

constructor TCryptCertAlgoX509.Create(xsa: TXSignatureAlgorithm;
  const suffix: RawUtf8);
begin
  if xsa = xsaNone then
    ECryptCertX509.RaiseUtf8('Unexpected %.Create(%)', [self, ToText(xsa)^]);
  fXsa := xsa;
  fXka := XSA_TO_XKA[xsa];
  fCaa := XSA_TO_CAA[xsa];
  inherited Create('x509-' + LowerCase(CAA_JWT[fCaa]) + suffix);
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
  cka: TCryptKeyAlgo;
  key: ICryptPrivateKey;
begin
  if Subjects = '' then
    RaiseError('no Subjects');
  cka := XKA_TO_CKA[fXka];
  key := CryptPrivateKey[cka].Create;
  // load or generate a public/private key pair
  if PrivateKeyPem <> '' then
    if not key.Load(cka, nil, PrivateKeyPem, PrivateKeyPassword) then
      RaiseError('PrivateKeyPem');
  // setup the CSR fields, self-sign the CSR and return it as PEM
  result := ComputeSelfSignedCsr(key, fXsa, Subjects, Usages, Fields);
  // save the generated private key (if was not previously loaded)
  if (result <> '') and
     (PrivateKeyPem = '') then
    PrivateKeyPem := key.Save({aspem=}true, PrivateKeyPassword);
end;


{ TCryptCertX509 }

function TCryptCertX509.AlgoXsa: TXSignatureAlgorithm;
begin
  result := TCryptCertAlgoX509(fCryptAlgo).fXsa;
end;

function TCryptCertX509.AlgoXka: TXPublicKeyAlgorithm;
begin
  result := TCryptCertAlgoX509(fCryptAlgo).fXka;
end;

function TCryptCertX509.VerifyAuthority(const Authority: ICryptCert): TCryptCert;
begin
  if (fX509 <> nil) or
     HasPrivateSecret then
    RaiseErrorGenerate('duplicated call');
  result := self; // self-signed if no Authority supplied
  if Authority <> nil then
    if Authority.HasPrivateSecret then
      result := Authority.Instance // any TCryptCert class would do
    else
      RaiseErrorGenerate('Authority has no private key to sign');
end;

procedure TCryptCertX509.GeneratePrivateKey;
begin
  if HasPrivateSecret then
    RaiseErrorGenerate('duplicated GeneratePrivateKey');
  fPrivateKey := CryptPrivateKey[XKA_TO_CKA[AlgoXka]].Create;
  fX509.Signed.SubjectPublicKey := fPrivateKey.Generate(XKA_TO_CAA[AlgoXka]);
  if fX509.Signed.SubjectPublicKey = '' then
    RaiseErrorGenerate('GeneratePrivateKey failed');
  fX509.Signed.SubjectPublicKeyAlgorithm := AlgoXka;
  fX509.Signed.SubjectPublicKeyBits :=
    X509PubKeyBits(fX509.Signed.SubjectPublicKey);
end;

function TCryptCertX509.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
var
  auth: TCryptCert; // may be self
begin
  if fX509 <> nil then
    RaiseErrorGenerate('duplicated call - not a void instance');
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
    // ensure all TX509 DER/raw binary fields are properly set
    fX509.LoadFromDer(fX509.SaveToDer);
    if fPrivateKey.ToSubjectPublicKey <> fX509.Signed.SubjectPublicKey then
      RaiseErrorGenerate('inconsistent DER generation');
    // we successully generated a new signed X.509 certificate
    result := self;
  except
    Clear;
  end;
end;

function TCryptCertX509.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
var
  auth: TCryptCert; // may be self
begin
  result := nil;
  if fX509 <> nil then
    RaiseErrorGenerate('(FromCsr) duplicated call');
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
    // ensure all TX509 DER/raw binary fields are properly set
    fX509.LoadFromDer(fX509.SaveToDer);
    // we successully generated a new signed X.509 certificate from this CSR
    result := self;
  except
    Clear;
  end;
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
          // may use mormot.core.secure encryption, not standard PKCS#8
          der := PemToDer(Saved); // see also TCryptPrivateKey.Load
          fPrivateKey := CryptPrivateKey[XKA_TO_CKA[AlgoXka]].Create; // replace
          if fPrivateKey.Load(
               XKA_TO_CKA[AlgoXka], fX509.PublicKey, der, PrivatePassword) then
            result := true
          else
            fPrivateKey := nil;
        end;
      cccCertOnly:
        begin
          EnsureCanWrite('Load');
          Clear;
          fX509 := TX509.Create;
          if fX509.LoadFromPem(Saved) then
            result := true
          else
            FreeAndNil(fX509);
        end;
      cccCertWithPrivateKey:
        // unconcatenate certificate PEM and private key PEM - no PKCS#12 yet
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
          result := fPrivateKey.Save(Format = ccfPem, PrivatePassword)
        else
          RaiseError('Save(cccPrivateKeyOnly) with no Private Key');
    end;
end;

function TCryptCertX509.SetPrivateKey(const saved: RawByteString): boolean;
begin
  result := false;
  fPrivateKey := nil; // always release - SetPrivateKey('') is "wipe out"
  if saved <> '' then
  begin
    // note: SetPrivateKey() may be called without a public key yet
    fPrivateKey := CryptPrivateKey[XKA_TO_CKA[AlgoXka]].Create;
    if fPrivateKey.Load(XKA_TO_CKA[AlgoXka], fX509.PublicKey, saved, '') then
      result := true
    else
      fPrivateKey := nil;
  end;
end;

function TCryptCertX509.Sign(Data: pointer; Len: integer;
  Usage: TCryptCertUsage): RawByteString;
begin
  if HasPrivateSecret and
     (fX509 <> nil) and
     (Usage in fX509.Usages) then
    result := fPrivateKey.Sign(XSA_TO_CAA[AlgoXsa], Data, Len)
  else
    result := '';
end;

procedure TCryptCertX509.Sign(const Authority: ICryptCert);
var
  a: TCryptCert;
  u: TCryptCertUsage;
begin
  EnsureCanWrite('Sign');
  if Assigned(Authority) and
    Authority.HasPrivateSecret then
  begin
    // validate usage
    a := Authority.Instance; // may be self
    u := cuKeyCertSign;
    if a = self then
      u := GetFirstUsage(GetUsage) // any usage to let Sign() pass below
    else if not (cuKeyCertSign in a.GetUsage) then
      RaiseError('Sign: % Authority has no cuKeyCertSign', [a]);
    // assign the Issuer information (from any TCryptCert kind of class)
    if not fX509.Signed.Issuer.FromAsn(a.GetSubject('DER')) then
      RaiseError('Sign: invalid % Authority DER', [a]);
    if a <> self then // same as OpenSSL: no AKID for for self-signed certs
      fX509.Signed.Extension[xeAuthorityKeyIdentifier] := a.GetSubjectKey;
    // compute the digital signature
    fX509.AfterModified;
    fX509.Signed.Signature := AlgoXsa;
    fX509.fSignatureValue := a.Sign(fX509.Signed.ToDer, u);
    if fX509.fSignatureValue = '' then
      RaiseError('Sign: % Authority failed its digital signature', [a]);
    fX509.fSignatureAlgorithm := AlgoXsa;
    fX509.ComputeCachedDer;
  end
  else
    RaiseError('Sign: Authority is not a CA');
end;


{ those methods are defined here for proper TCryptCertX509 knowledge }

// retrieve an authority as TCryptCertX509 instance
function AuthToCryptCertX509(const Authority: ICryptCert;
  Content: TCryptCertContent; var TempCryptCert: ICryptCert): TCryptCertX509;
var
  xsa: TXSignatureAlgorithm;
  auth: TCryptCertX509;
  pem: RawUtf8; // TCryptCertX509.Load(cccCertWithPrivateKey) only supports PEM
begin
  result := nil;
  xsa := CAA_TO_XSA[Authority.AsymAlgo];
  if xsa = xsaNone then
    exit;
  auth := pointer(Authority.Instance);
  if auth.InheritsFrom(TCryptCertX509) then
    // quickly return the ICryptCert Authority of the expected type
    result := auth
  else
  try
    // create a new temporary TCryptCertX509 instance from this Authority PEM
    pem := Authority.Save(Content, '', ccfPem); // e.g. a TCryptCertAlgoOpenSsl
    if pem = '' then
      exit;
    TempCryptCert := CryptCertX509[Authority.AsymAlgo].New;
    if TempCryptCert.Load(pem, Content, '') then
      result := TempCryptCert.Instance as TCryptCertX509;
  finally
    FillZero(pem);
  end;
end;

procedure TX509Crl.SignCryptCert(const Authority: ICryptCert;
  AuthorityCrlNumber: QWord);
var
  auth: TCryptCertX509;
  temp: ICryptCert;
begin
  if (self <> nil) and
     Assigned(Authority) and
     Authority.HasPrivateSecret then
  begin
    // retrieve a compatible authority instance
    auth := AuthToCryptCertX509(Authority, cccCertWithPrivateKey, temp);
    if auth = nil then
      EX509.RaiseUtf8('%.Sign: unsupported Authority % %',
        [self, Authority.Instance, ToText(Authority.AsymAlgo)^]);
    if auth.fX509 = nil then
      EX509.RaiseUtf8('%.Sign: authority has no public key', [self]);
    // validate usage
    if not (cuCrlSign in auth.fX509.Usages) then
      EX509.RaiseUtf8('%.Sign: authority has no cuCrlSign', [self]);
    // assign the Issuer information
    Signed.Issuer := auth.fX509.Signed.Subject;
    Signed.Extension[xceAuthorityKeyIdentifier] :=
      auth.fX509.Signed.Extension[xeSubjectKeyIdentifier];
    if AuthorityCrlNumber = 0 then
      // we need some increasing value for conformity
      AuthorityCrlNumber := UnixTimeMinimalUtc; // increase every second
    SetCrlNumber(AuthorityCrlNumber);
    // compute the digital signature
    AfterModified;
    Signed.Signature := auth.AlgoXsa;
    fSignatureValue := auth.fPrivateKey.Sign(
                         XSA_TO_CAA[auth.AlgoXsa], Signed.ToDer);
    fSignatureAlgorithm := auth.AlgoXsa;
  end
  else
    EX509.RaiseUtf8('%.Sign: not a CA', [self]);
end;

function TX509Crl.VerifyCryptCert(const Authority: ICryptCert;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
var
  auth: TCryptCertX509;
  temp: ICryptCert;
begin
  result := cvBadParameter;
  if (self <> nil) and
     Assigned(Authority) and
     (Authority.Handle <> nil) then
  begin
    // use a compatible authority instance for digital signature verification
    auth := AuthToCryptCertX509(Authority, cccCertOnly, temp);
    if auth <> nil then
      result := Verify(auth.fX509, IgnoreError, TimeUtc);
  end;
end;


{ TCryptCertCacheX509 }

function TCryptCertCacheX509.InternalLoad(const Cert: RawByteString): ICryptCert;
begin
  result := X509Load(Cert);
end;


{ TCryptStoreAlgoX509 }

function TCryptStoreAlgoX509.New: ICryptStore;
begin
  result := TCryptStoreX509.Create(self);
end;


{ TCryptStoreX509 }

function TCryptStoreX509.GetCacheCount: integer;
begin
  result := fCache.Count;
end;

function TCryptStoreX509.GetRevoked: integer;
begin
  result := fSignedCrl.Revoked + fUnsignedCrl.Revoked;
end;

function TCryptStoreX509.GetCACount: integer;
begin
  result := fCA.Count;
end;

constructor TCryptStoreX509.Create(algo: TCryptAlgo);
begin
  inherited Create(algo);
  fIsRevokedTag := Random32 shr 10; // to force ComputeIsRevoked between stores
  fCache := TCryptCertCacheX509.Create;
  TCryptCertCacheX509(fCache).SetCryptCertClass(TCryptCertX509);
  fValidDepth := 32;
  fTrust := fCache.NewList;
  fCA := fCache.NewList;
  fSignedCrl := TX509CrlList.Create;
  fUnsignedCrl := TX509CrlList.Create;
end;

destructor TCryptStoreX509.Destroy;
begin
  inherited Destroy;
  fUnsignedCrl.Free;
  fSignedCrl.Free;
  fCA.Free;
  fTrust.Free;
end;

procedure TCryptStoreX509.Clear;
begin
  // keep fCache intact, just re-create all nested storage classes
  fUnsignedCrl.Free;
  fSignedCrl.Free;
  fCA.Free;
  fTrust.Free;
  fTrust := fCache.NewList;
  fCA := fCache.NewList;
  fSignedCrl := TX509CrlList.Create;
  fUnsignedCrl := TX509CrlList.Create;
end;

function TCryptStoreX509.Save: RawByteString;
var
  tmp: TTextWriterStackBuffer;
  w: TTextWriter;
begin
  w := TTextWriter.CreateOwnedStream(tmp);
  try
    fTrust.SaveToPem(W, {WithExplanatoryText=}true);
    w.AddCR;
    fSignedCrl.SaveToPem(W, {WithExplanatoryText=}true);
    w.AddCR;
    fUnsignedCrl.SaveToPem(W, {WithExplanatoryText=}true);
    w.SetText(RawUtf8(result));
  finally
    w.Free;
  end;
end;

function TCryptStoreX509.GetBySerial(const Serial: RawUtf8): ICryptCert;
begin
  result := fTrust.FindOne(Serial, ccmSerialNumber);
end;

function TCryptStoreX509.GetBySubjectKey(const Key: RawUtf8): ICryptCert;
begin
  result := fTrust.FindBySubjectKey(Key);
end;

function TCryptStoreX509.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCert;
begin
  result := fTrust.FindOne(Value, Method);
end;

function TCryptStoreX509.ComputeIsRevoked(cert: TX509): TCryptCertRevocationReason;
var
  id: PRawByteString;
  idcount, ownertag: integer;
begin
  // multi-thread safety: get tag sequence number before searches
  ownertag := fIsRevokedTag;
  // retrieve the AKID of this certificate (maybe SKID if self-signed)
  id := pointer(cert.fRawAuthorityKeyIdentifier);
  if id = nil then
  begin
    id := @cert.fRawSubjectKeyIdentifier; // self-signed
    idcount := 1;
  end
  else
    idcount := PDALen(PAnsiChar(pointer(id)) - _DALEN)^ + _DAOFF;
  // ask the fSignedCrl and fUnsignedCrl lists
  result := fSignedCrl.IsRevokedRaw(id, idcount, cert.Signed.SerialNumber);
  if result = crrNotRevoked then
    result := fUnsignedCrl.IsRevokedRaw(id, idcount, cert.Signed.SerialNumber);
  // cache the result into cert.fIsRevokedTag
  if result = crrNotRevoked then
    cert.fIsRevokedTag := ownertag // no need to test until next revocation
  else
    cert.fIsRevokedTag := -(integer(result) + 1); // -1..-11 to mark as revoked
    // as a nice side effect: once revoked, always revoked
end;

function TCryptStoreX509.IsRevokedX509(cert: TX509): TCryptCertRevocationReason;
var
  flags: integer;
begin
  // very quick resolution using the per-TX509 instance cache tag
  result := crrNotRevoked; // most common case is "known as not revoked"
  if cert = nil then
    exit;
  flags := cert.fIsRevokedTag;
  if flags <> fIsRevokedTag then // are we in sync with the store?
    if flags < 0 then
      result := TCryptCertRevocationReason(-(flags + 1)) // revoked
    else
      result := ComputeIsRevoked(cert); // ask once both TX509CrlList
end;

function TCryptStoreX509.IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason;
var
  x: TCryptCert;
begin
  result := crrNotRevoked;
  if not Assigned(cert) then
    exit;
  x := cert.Instance;
  if x.InheritsFrom(TCryptCertX509Abstract) then
    result := IsRevokedX509(TCryptCertX509Abstract(x).fX509);
end;

function TCryptStoreX509.Add(const cert: ICryptCert): boolean;
begin
  result := (cert <> nil) and
            cert.Instance.InheritsFrom(TCryptCertX509Abstract) and
            fTrust.Add(cert);
end;

function TCryptStoreX509.AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray;
var
  k: TPemKind;
  p: PUtf8Char;
  der: TCertDer;
  crl: TX509Crl;
  cert: ICryptCert;
  new: ICryptCerts;
  i: PtrInt;
begin
  result := nil;
  if IsPem(Content) then
  begin
    // expect certificate(s) and/or CRL(s) stored as (concatenated) PEM text
    p := pointer(Content);
    repeat
      der := NextPemToDer(p, @k);
      if der = '' then
        break;
      case k of
        pemUnspecified,
        pemCertificate:
          begin
            cert := fCache.Load(der);
            if cert <> nil then
              ChainAdd(new, cert);
          end;
        pemCrl:
          begin
            crl := TX509Crl.Create;
            try
              if crl.LoadFromDer(der) then
              begin
                if crl.SignatureValue = '' then
                  fUnsignedCrl.Add(crl)   // unsigned: from AddRevocation()
                else
                  fSignedCrl.Add(crl);    // signed by a CA
                inc(fIsRevokedTag);
                crl := nil; // owned by one of the two lists
              end;
            finally
              crl.Free;
            end;
          end;
      end;
    until false;
  end
  else
    // a single DER file should be a certificate
    new := fCache.Load([Content]);
  for i := 0 to high(new) do
    if Add(new[i]) then
      AddRawUtf8(result, new[i].GetSerial);
  if fIsRevokedTag < 0 then
    fIsRevokedTag := 0; // paranoid 31-bit overflow
end;

function TCryptStoreX509.Revoke(const Cert: ICryptCert;
  Reason: TCryptCertRevocationReason; RevocationDate: TDateTime): boolean;
var
  akid: RawUtf8;
begin
  result := false;
  if Cert = nil then
    exit;
  akid := Cert.GetAuthorityKey;
  if akid = '' then
    akid := Cert.GetSubjectKey; // self-signed certificate
  result := fUnsignedCrl.AddRevocation(
              akid, Cert.GetSerial, Reason, 0, RevocationDate);
  if not result then
    exit;
  inc(fIsRevokedTag);
  if fIsRevokedTag < 0 then
    fIsRevokedTag := 0; // paranoid 31-bit overflow
end;

function TCryptStoreX509.IsValid(const cert: ICryptCert;
  date: TDateTime): TCryptCertValidity;
var
  c: TCryptCert;
  x, xa: TX509;
  a, f: ICryptCert;
  skid, akid: PRawByteString;
  level: integer;
begin
  // validate this certificate context
  result := cvUnknownAuthority;
  if not Assigned(cert) then
    exit;
  c := cert.Instance;
  if (c = nil) or
     not c.InheritsFrom(TCryptCertX509Abstract) then
    exit;
  x := TCryptCertX509Abstract(c).fX509;
  if x = nil then
    exit;
  result := cvInvalidDate;
  if not x.Signed.IsValidDate(date) then
    exit;
  result := cvRevoked;
  if IsRevokedX509(x) <> crrNotRevoked then // has a TX509 cache
    exit;
  // search within our database of known certificates
  result := cvCorrupted;
  skid := @x.fRawSubjectKeyIdentifier;
  if skid^ = '' then
    exit;
  f := fTrust.FindBySubjectKeyRaw(skid^);
  if (f <> nil) and
     (x.Compare(f.Handle, ccmBinary) <> 0) then
    exit; // this certificate was forged
  result := cvUnknownAuthority;
  if fTrust.Count = 0 then
    exit;
  if x.IsSelfSigned then
  begin
    if f <> nil then // self-signed certs should be known
      // verify the self signature of this trusted cert
      result := x.Verify(x, [], x.NotBefore);
    exit;
  end;
  // check all known issuers until we reach ValidDepth or a root anchor
  for level := 0 to ValidDepth do
  begin
    result := cvCorrupted;
    akid := pointer(x.fRawAuthorityKeyIdentifier); // check only first auth
    if akid = nil then
      if x.IsSelfSigned then
        akid := skid // typical on X.509
      else
        exit; // missing field
    result := cvUnknownAuthority;
    a := fTrust.FindBySubjectKeyRaw(akid^);
    if a = nil then
      exit;
    // verify the cert digital signature with the issuer public key
    xa := a.Handle;
    result := x.Verify(xa, [], x.NotBefore); // has a TX509 cache
    if result = cvValidSelfSigned then
    begin
      // we reached a root anchor: success
      if not cert.IsSelfSigned then
        result := cvValidSigned;
      exit;
    end else if result <> cvValidSigned then
      exit;
    // continue to the next level
    skid := akid;
    x := xa;
    result := cvRevoked;
    if IsRevokedX509(x) <> crrNotRevoked then
      exit;
  end;
  // if we reached as many level as requested, consider it done
  if cert.IsSelfSigned then
    result := cvValidSelfSigned
  else
    result := cvValidSigned;
end;

function TCryptStoreX509.Verify(const Signature: RawByteString;
  Data: pointer; Len: integer; IgnoreError: TCryptCertValidities;
  TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := cvNotSupported; // we don't know which signing authority to use
end;

function TCryptStoreX509.Count: integer;
begin
  result := fTrust.Count;
end;

function TCryptStoreX509.CrlCount: integer;
begin
  result := fSignedCrl.Count + fUnsignedCrl.Count;
end;

function TCryptStoreX509.DefaultCertAlgo: TCryptCertAlgo;
begin
  result := CryptCertX509[CryptAlgoDefault];
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

function X509Load(const Cert: RawByteString): ICryptCert;
var
  x: TX509;
  der: RawByteString;
begin
  result := nil;
  der := PemToDer(Cert);
  if not AsnDecChunk(der) then // basic input validation
    exit;
  x := TX509.Create;
  try
    if x.LoadFromDer(der) and
       (x.SignatureAlgorithm <> xsaNone) then // support PEM or DER input
    begin
      result := CryptCertX509[XSA_TO_CAA[x.SignatureAlgorithm]].FromHandle(x);
      if result <> nil then
        x := nil;
    end;
  finally
    x.Free;
  end;
end;

procedure RegisterX509;
var
  xsa: TXSignatureAlgorithm;
  caa: TCryptAsymAlgo;
begin
  for xsa := succ(low(xsa)) to high(xsa) do
    if (CryptCertX509[XSA_TO_CAA[xsa]] = nil) and
       (CryptPublicKey[XKA_TO_CKA[XSA_TO_XKA[xsa]]] <> nil) then
    begin
      caa := XSA_TO_CAA[xsa];
      CryptCertX509[caa] := TCryptCertAlgoX509.Create(xsa, '-int');
      if CryptCert[caa] = nil then
        CryptCert[caa] := CryptCertX509[caa];
    end;
end;

procedure InitializeUnit;
var
  a: TXAttr;
  o: TXExtension;
  c: TXCrlExtension;
  k: TXExtendedKeyUsage;
  xsa: TXSignatureAlgorithm;
  caa: TCryptAsymAlgo;
begin
  for a := succ(low(a)) to high(a) do
    XA_OID_ASN[a] := AsnEncOid(XA_OID[a]);
  for o := succ(low(o)) to high(o) do
    XE_OID_ASN[o] := AsnEncOid(XE_OID[o]);
  for c := succ(low(c)) to high(c) do
    XCE_OID_ASN[c] := AsnEncOid(XCE_OID[c]);
  for k := succ(low(k)) to high(k) do
    XKU_OID_ASN[k] := AsnEncOid(XKU_OID[k]);
  // register this unit to our high-level cryptographic catalog
  // 'x509-rs256-int' 'x509-ps256-int' and 'x509-es256-int' match this unit
  // ('x509-rs/ps384/512-int' methods seem superfluous so are not defined)
  TCryptCertAlgoX509.Create(xsaSha256Rsa,    {suffix=}'-int');
  TCryptCertAlgoX509.Create(xsaSha256RsaPss, {suffix=}'-int');
  TCryptCertAlgoX509.Create(xsaSha256Ecc256, {suffix=}'-int');
  // register 'x509-rs256' 'x509-rs384' 'x509-rs512' 'x509-ps256' 'x509-ps384'
  // 'x509-ps512' and 'x509-es256' certificates
  // - may be overriden by the faster mormot.crypt.openssl if included
  // - but still accessible from CryptCertX509[] global factories
  for xsa := succ(low(xsa)) to high(xsa) do
    if CryptPublicKey[XKA_TO_CKA[XSA_TO_XKA[xsa]]] <> nil then
    begin
      caa := XSA_TO_CAA[xsa];
      CryptCertX509[caa] := TCryptCertAlgoX509.Create(xsa, '');
      if CryptCert[caa] = nil then
        CryptCert[caa] := CryptCertX509[caa];
    end;
  // register 'x509-pki' store to our catalog
  CryptStoreX509 := TCryptStoreAlgoX509.Create('x509-pki');
  // use our class for X.509 parsing - unless mormot.crypt.openssl is included
  X509Parse := @TX509Parse;
end;


initialization
  InitializeUnit;

end.
