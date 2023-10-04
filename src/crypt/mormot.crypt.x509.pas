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

// note: all X.509 types start with TX

type
  /// exception raised by this unit
  EX509 = class(ESynException);

  /// known X.501 Type Names, as stored in X509 Certificates attributes
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

  /// known X509 v3 Certificate extensions
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
    xeGoogleSignedCertificateTimestamp,
    xeNetscapeComment);

  /// X509 Certificate Key Usage - see RFC 5280 Section 4.2.1.3
  TXKeyUsage = (
    kuDigitalsignature,
    kuNonRepudiation,
    kuKeyEncipherment,
    kuDataEncipherment,
    kuKeyAgreement,
    kuKeyCertSign,
    kuCrlSign,
    kuEncipherOnly,
    kuDecipherOnly);
  /// set of X509 Certificate Key Usages - see RFC 5280 Section 4.2.1.3
  TXKeyUsages = set of TXKeyUsage;

  /// X509 Certificate Extended Key Usage - see RFC 5280 Section 4.2.1.12
  TXExtendedKeyUsage = (
    xkuNone,
    xkuServerAuth,         // 1
    xkuClientAuth,         // 2
    xkuCodeSigning,        // 3
    xkuEmailProtection,    // 4
    xkuTimeStamping,       // 8
    xkuOcspSigning);       // 9
  /// set of X509 Certificate Extended Key Usage - see RFC 5280 Section 4.2.1.12
  TXExtendedKeyUsages = set of TXExtendedKeyUsage;

  /// supported TX509.SignatureAlgorithm values
  // - we support RSA and ECC-256 asymmetric algorithms using our own units
  // - for safety, any unsafe algorithms (e.g. MD5 or SHA-1) are not defined
  TX509SignatureAlgorithm = (
    xsaNone,
    xsaSha256Rsa,
    xsaSha384Rsa,
    xsaSha512Rsa,
    xsaSha256Ecc256);

  /// supported TX509.SubjectPublicKeyAlgorithm values
  TX509PublicKeyAlgorithm = (
    xkaNone,
    xkaRsa,
    xkaEcc256);

  /// used to store one unknown attribute or extension
  TX509Other = record
    Oid: RawByteString;
    Value: RawByteString;
  end;
  /// used to store several attributes or extensions
  TX509Others = array of TX509Other;

  /// a X.501 Type Name
  {$ifdef USERECORDWITHMETHODS}
  TX501Name = record
  {$else}
  TX501Name = object
  {$endif USERECORDWITHMETHODS}
  private
    fCachedAsn: RawByteString;
    procedure ComputeAsn;
  public
    /// CSV of the values of each kind of known attributes
    Names: array[TXAttr] of RawUtf8;
    /// values which are not part of the known attributes
    Others: TX509Others;
    /// the raw ASN1_SEQ encoded value of this name
    function ToBinary: RawByteString;
    /// unserialize the X.501 Type Name from raw ASN1_SEQ binary
    function FromAsn(const seq: TAsnObject): boolean;
    /// unserialize the X.501 Type Name from the next raw ASN1_SEQ binary
    function FromAsnNext(var pos: integer; const der: TAsnObject): boolean;
    /// to be called once any field has been changed to refresh the Binary cache
    procedure AfterModified;
    /// return the hash of the normalized Binary of this field
    function ToDigest(algo: THashAlgo = hfSha1): RawUtf8;
  end;

const
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

  /// the OID of all known X509 Certificate extensions, as defined in RFC 5280 4.2.1
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
    '1.3.6.1.4.1.11129.2.4.2',   // xeGoogleSignedCertificateTimestamp
    '2.16.840.1.113730.1.13');   // xeNetscapeComment

  /// the OID of all known X509 Certificate Extended Key Usage
  XKU_OID: array[TXExtendedKeyUsage] of PUtf8Char = (
    '',                    // xkuNone
    '1.3.6.1.5.5.7.3.1',   // xkuServerAuth
    '1.3.6.1.5.5.7.3.2',   // xkuClientAuth
    '1.3.6.1.5.5.7.3.3',   // xkuCodeSigning
    '1.3.6.1.5.5.7.3.4',   // xkuEmailProtection
    '1.3.6.1.5.5.7.3.8',   // xkuTimeStamping
    '1.3.6.1.5.5.7.3.9');  // xkuOcspSigning

const
  OID_ID_PE                       = '1.3.6.1.5.5.7.1';
  OID_ID_PE_AUTHORITY_INFO_ACCESS = '1.3.6.1.5.5.7.1.1';
  OID_ID_KP_SERVER_AUTH           = '1.3.6.1.5.5.7.3.1';
  OID_ID_KP_CLIENT_AUTH           = '1.3.6.1.5.5.7.3.2';
  OID_ID_KP_OCSP_SIGNING          = '1.3.6.1.5.5.7.3.9';

  OID_HASH_SHA1        = '1.3.14.3.2.26';
  OID_HASH_SHA1_RSA    = '1.3.14.3.2.29';
  OID_HASH_ALGORITHMS  = '2.16.840.1.101.3.4.2';
  OID_HASH_SHA256      = '2.16.840.1.101.3.4.2.1';
  OID_HASH_SHA384      = '2.16.840.1.101.3.4.2.2';
  OID_HASH_SHA512      = '2.16.840.1.101.3.4.2.3';
  OID_HASH_SHA224      = '2.16.840.1.101.3.4.2.4';

  OID_PKCS1_ALGORITHMS = '1.2.840.113549.1.1';
  OID_PKCS1_RSA        = '1.2.840.113549.1.1.1'; // as generated by OpenSSL
  OID_PKCS1_MD5_RSA    = '1.2.840.113549.1.1.4';
  OID_PKCS1_SHA1_RSA   = '1.2.840.113549.1.1.5';
  OID_PKCS1_RSA_PSS    = '1.2.840.113549.1.1.10';
  OID_PKCS1_SHA256_RSA = '1.2.840.113549.1.1.11';
  OID_PKCS1_SHA384_RSA = '1.2.840.113549.1.1.12';
  OID_PKCS1_SHA512_RSA = '1.2.840.113549.1.1.13';
  OIDPKCS1__SHA224_RSA = '1.2.840.113549.1.1.14';

  OID_PKCS2_ALGORITHMS = '1.2.840.113549.2';
  OID_PKCS2_MD5        = '1.2.840.113549.2.5';

  OID_X962_PUBLICKEY   = '1.2.840.10045.2.1';
  OID_X962_PRIME256V1  = '1.2.840.10045.3.1.1.7';

  OID_THAWTE_ED25519   = '1.3.101.112';


{ **************** X.509 Certificates }

type
  /// a X509 Certificate, as defined in RFC 5280 #4.1.2 and stored in TX509.Signed
  // - contains information associated with the subject of the certificate
  // and the CA that issued it
  {$ifdef USERECORDWITHMETHODS}
  TX509Content = record
  {$else}
  TX509Content = object
  {$endif USERECORDWITHMETHODS}
  private
    fCachedDer: RawByteString; // for ToDer
    procedure ComputeCachedDer;
  public
    /// describes the version of the encoded certificate
    // - equals usually 3, once extensions are used
    Version: integer;
    /// raw binary of a positive integer assigned by the CA to each certificate
    // - maps PBigInt.Save binary serialization
    SerialNumber: RawByteString;
    /// the cryptographic algorithm used by the CA over the TX509.Signed field
    // - match TX509.SignatureAlgorithm field
    Signature: TX509SignatureAlgorithm;
    /// identifies the entity that has signed and issued the certificate
    Issuer: TX501Name;
    /// date on which the certificate validity period begins
    NotBefore: TDateTime;
    /// date on which the certificate validity period ends
    // - may equal 0 if '99991231235959Z' was stored as "unspecified end date"
    // (see RFC 5280 #4.1.2.5)
    NotAfter: TDateTime;
    /// identifies the entity associated with the public key stored in the
    // subject public key field of this certificate
    Subject: TX501Name;
    /// decoded AlgorithmIdentifier structure of the stored public key
    SubjectPublicKeyAlgorithm: TX509PublicKeyAlgorithm;
    /// public key raw binary
    SubjectPublicKey: RawByteString;
    /// decoded extensions as defined for X.509 v3 certificates
    // - will contain the ready-to-use UTF-8 text of the value
    // - some types are not fully decoded, so you may need to use ExtensionRaw[]
    Extension: array[TXExtension] of RawUtf8;
    /// if a decoded extension was marked as Critical
    ExtensionCritical: array[TXExtension] of boolean;
    /// raw ASN1_OCTSTR of decoded Extension[] after FromDer()
    ExtensionRaw: array[TXExtension] of RawByteString;
    /// unsupported extensions as defined for X.509 v3 certificates
    ExtensionOther: TX509Others;
    /// declared X.509 v3 certificate Key Usages from extensions
    KeyUsages: TXKeyUsages;
    /// declared X.509 v3 certificate Extended Key Usages from extensions
    ExtendedKeyUsages: TXExtendedKeyUsages;
    /// check Extension[xeBasicConstraints]
    function IsCertificateAuthority: boolean;
    /// hexadecimal of a positive integer assigned by the CA to each certificate
    // - e.g. '03:cc:83:aa:af:f9:c1:e2:1c:fa:fa:80:af:e6:67:6e:27:4c'
    function SerialNumberHex: RawUtf8;
    /// decimal text of a positive integer assigned by the CA to each certificate
    // - e.g. '330929475774275458452528262248458246563660'
    function SerialNumberText: RawUtf8;
    /// check if the Issuer is also the Subject
    function IsSelfSigned: boolean;
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

  /// pointer to a X509 Certificate, as stored in TX509.Signed
  PX509Content = ^TX509Content;

  /// a X509 signed Certificate, as defined in RFC 5280
  TX509 = class(TSynPersistent)
  protected
    fCachedDer: RawByteString;
    fSignatureValue: RawByteString;
    fSignatureAlgorithm: TX509SignatureAlgorithm;
    procedure ComputeAsn;
    function ComputeDigest(Algo: TX509SignatureAlgorithm): TSha256Digest;
  public
    /// actual to-be-signed Certificate content
    Signed: TX509Content;
    /// the cryptographic algorithm used by the CA over the Signed field
    // - match TX509Context.Signature field
    property SignatureAlgorithm: TX509SignatureAlgorithm
      read fSignatureAlgorithm;
    /// raw binary digital signature computed upon Signed.ToDer
    property SignatureValue: RawByteString
      read fSignatureValue;
    /// reset all internal context
    procedure Clear;
    /// generate the SignatureAlgorithm/SignatureValue using a RSA private key
    procedure SignRsa(RsaAuthority: TRsa);
    /// generate the SignatureAlgorithm/SignatureValue using a ECC256 private key
    procedure SignEcc(const EccKey: TEccPrivateKey);
    /// generate the SignatureAlgorithm/SignatureValue using a RSA public key
    function VerifyRsa(RsaAuthority: TRsa): boolean;
    /// generate the SignatureAlgorithm/SignatureValue using a ECC256 public key
    function VerifyEcc(const EccKey: TEccPublicKey): boolean;
    /// serialize those fields into ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function ToDer: TCertDer;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function FromDer(const der: TCertDer): boolean;
    /// unserialize those fields from a PEM content
    // - will fallback and try as ASN.1 DER binary if content is not PEM
    function FromPem(const pem: TCertPem): boolean;
    /// the lowercase hexa hash of the normalized Binary of this Certificate
    function FingerPrint(algo: THashAlgo = hfSha1): RawUtf8;
    /// to be called once any field has been changed to refresh the Binary cache
    procedure AfterModified;
  end;

const
  ASN1_OID_SIGNATURE: array[TX509SignatureAlgorithm] of RawUtf8 = (
     '',
     '1.2.840.113549.1.1.11', // xsaSha256Rsa
     '1.2.840.113549.1.1.12', // xsaSha384Rsa
     '1.2.840.113549.1.1.13', // xsaSha512Rsa
     '1.2.840.10045.4.3.2');  // xsaSha256Ecc256

  ASN1_OID_PKCS1_RSA       = '1.2.840.113549.1.1.1';
  ASN1_OID_X962_PUBLICKEY  = '1.2.840.10045.2.1';
  ASN1_OID_X962_ECDSA_P256 = '1.2.840.10045.3.1.7';


implementation



{ **************** X.509 Encoding Decoding}

function XsaToSeq(xsa: TX509SignatureAlgorithm): TAsnObject;
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

function XkaToSeq(xka: TX509PublicKeyAlgorithm): RawByteString;
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

function OidToXsa(const oid: RawUtf8; out xsa: TX509SignatureAlgorithm): boolean;
var
  x: TX509SignatureAlgorithm;
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

function OidToXka(const oid, oid2: RawUtf8; out xka: TX509PublicKeyAlgorithm): boolean;
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


{ TX509Name }

procedure TX501Name.ComputeAsn;
var
  a: TXAttr;
  p: PUtf8Char;
  o: PtrInt;
  v: RawUtf8;
  tmp, one: RawByteString;
begin
  for a := succ(low(a)) to high(a) do
  begin
    p := pointer(Names[a]);
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
  for o := 0 to high(Others) do
    with Others[o] do
      Append(tmp, Asn(ASN1_SETOF, [
                    Asn(ASN1_SEQ, [
                      AsnOid(pointer(Oid)),
                      AsnText(Value)
                    ])
                  ]));
  fCachedAsn := Asn(ASN1_SEQ, [tmp]);
end;

function TX501Name.ToBinary: RawByteString;
begin
  if fCachedAsn = '' then
    ComputeAsn;
  result := fCachedAsn;
end;

procedure AddOther(var others: TX509Others; const o, v: RawByteString);
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

function TX501Name.FromAsn(const seq: TAsnObject): boolean;
var
  posseq, posone: integer;
  xa: TXAttr;
  one, oid, v: RawByteString;
begin
  result := false;
  fCachedAsn := seq; // store exact binary since used for comparison
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
          AddOther(Others, oid, v)
        else
          // known attribute
          AddToCsv(v, Names[xa]);
      end;
  end;
  result := true;
end;

function TX501Name.FromAsnNext(var pos: integer; const der: TAsnObject): boolean;
var
  seq: RawByteString;
begin
  result := (AsnNextRaw(pos, der, seq) = ASN1_SEQ) and
            FromAsn(seq);
end;

procedure TX501Name.AfterModified;
begin
  fCachedAsn := '';
end;

function TX501Name.ToDigest(algo: THashAlgo): RawUtf8;
begin
  result := HashFull(algo, ToBinary);
end;


{ **************** X.509 Certificates }

{ TX509Content }

procedure TX509Content.ComputeCachedDer;
var
  ext: RawByteString;
begin
  if Version >= 3 then
  begin

    ext := Asn(ASN1_CTC3, [ext]);
  end;
  fCachedDer := Asn(ASN1_SEQ, [
                  Asn(Version - 1),
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

function TX509Content.IsCertificateAuthority: boolean;
begin
  result := Extension[xeBasicConstraints] = 'CA';
end;

function TX509Content.SerialNumberHex: RawUtf8;
begin
  ToHumanHex(result, pointer(SerialNumber), length(SerialNumber));
end;

function TX509Content.SerialNumberText: RawUtf8;
begin
  result := BigIntToText(SerialNumber);
end;

function TX509Content.IsSelfSigned: boolean;
begin
  result := Issuer.ToBinary = Subject.ToBinary;
end;

function TX509Content.ToDer: TAsnObject;
begin
  if fCachedDer = '' then
    ComputeCachedDer;
  result := fCachedDer;
end;

function TX509Content.FromDer(const der: TCertDer): boolean;
var
  pos, vt, extpos: integer;
  oid, oid2, ext, v: RawByteString;
  decoded: RawUtf8;
  xe: TXExtension;
  xku: TXExtendedKeyUsage;
  w: word;
  critical: boolean;
begin
  result := false;
  Clear;
  fCachedDer := der;
  // read main X509 tbsCertificate fields
  pos := 1;
  if AsnNext(pos, der) <> ASN1_CTC0 then
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
  if (Version = 3) and
     (AsnNext(pos, der) = ASN1_CTC3) and
     (AsnNext(pos, der) = ASN1_SEQ) then
    // read extensions
    while (AsnNext(pos, der) = ASN1_SEQ) and
          (AsnNextRaw(pos, der, oid) = ASN1_OBJID) do // extnID
    begin
      critical := false;
      vt := AsnNextRaw(pos, der, ext);
      if vt = ASN1_BOOL then // optional Critical flag
      begin
        critical := ext = #$ff;
        vt := AsnNextRaw(pos, der, ext);
      end;
      if vt <> ASN1_OCTSTR then // extnValue
        continue;
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
                  v := '';
                end;
                if v <> '' then
                  AddToCsv(v, decoded);
              until false;
          xeBasicConstraints:          // RFC 5280 #4.2.1.9
            if (AsnNext(extpos, ext) = ASN1_SEQ) and
               (AsnNextRaw(extpos, ext, v) = ASN1_BOOL) and
               (v = #$ff) then
              decoded := 'CA'; // as expected by IsCertificateAuthority
          xeKeyUsage:                  // RFC 5280 #4.2.1.3
            if AsnNextRaw(extpos, ext, v) = ASN1_BITSTR then
            begin
              w := 0;
              case length(v) of
                1:
                  w := PByte(v)^ shl 8;
                2:
                  w := PWord(v)^
              end;
              PWord(@KeyUsages)^ := swap(w);
            end;
          xeExtendedKeyUsage:          // RFC 5280 #4.2.1.12
            if AsnNext(extpos, ext) = ASN1_SEQ then
              while AsnNextRaw(extpos, ext, oid) = ASN1_OBJID do
              begin
                xku := OidToXku(oid);
                if xku <> xkuNone then
                  include(ExtendedKeyUsages, xku);
              end;
        end;
        if decoded <> '' then
          Extension[xe] := decoded;
      end;
    end;
  result := true;
end;

procedure TX509Content.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

procedure TX509Content.AfterModified;
begin
  fCachedDer := ''; // just reset the cache
  Subject.AfterModified;
  Issuer.AfterModified;
end;


{ TX509 }

procedure TX509.Clear;
begin
  fCachedDer := '';
  Signed.Clear;
  fSignatureAlgorithm := xsaNone;
  fSignatureValue := '';
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
  XSA_TO_HF: array[xsaSha256Rsa .. xsaSha512Rsa] of THashAlgo = (
    hfSha256,
    hfSha384,
    hfSha512);

function TX509.VerifyRsa(RsaAuthority: TRsa): boolean;
var
  hash: THashAlgo;
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
  oid: RawUtf8;
  bin: RawByteString;
begin
  result := (SignatureAlgorithm in [low(XSA_TO_HF) .. high(XSA_TO_HF)]) and
            (SignatureValue <> '') and
            RsaAuthority.HasPublicKey;
  if not result then
    exit;
  bin := Signed.ToDer;
  hash := XSA_TO_HF[SignatureAlgorithm];
  diglen := hasher.Full(hash, pointer(bin), length(bin), dig);
  bin := RsaAuthority.Verify(SignatureValue, @oid);
  result := (oid = ASN1_OID_HASH[hash]) and
            (length(bin) = diglen) and
            CompareMem(pointer(bin), @dig, diglen);
end;

function TX509.VerifyEcc(const EccKey: TEccPublicKey): boolean;
var
  sig: TEccSignature;
begin
  result := (SignatureAlgorithm = xsaSha256Ecc256) and
            not IsZero(EccKey) and
            DerToEcc(pointer(SignatureValue), length(SignatureValue), sig) and
            Ecc256r1Verify(EccKey, Sha256Digest(Signed.ToDer), sig);
end;

function TX509.ToDer: TCertDer;
begin
  if fCachedDer = '' then
    ComputeAsn;
  result := fCachedDer;
end;

function TX509.ComputeDigest(Algo: TX509SignatureAlgorithm): TSha256Digest;
begin
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
  result := Sha256Digest(Signed.ToDer);
end;

procedure TX509.ComputeAsn;
begin
  if (SignatureAlgorithm = xsaNone) or
     (SignatureValue = '') then
    raise EX509.Create('TX509.ToDer with no previous Sign() call');
  fCachedDer := Asn(ASN1_SEQ, [
                  Signed.ToDer,
                  XsaToSeq(SignatureAlgorithm),
                  Asn(ASN1_BITSTR, SignatureValue)
                ]);
end;

function TX509.FromDer(const der: TCertDer): boolean;
var
  pos: integer;
  tbs, oid: RawByteString;
begin
  Clear;
  fCachedDer := der;
  pos := 1;
  result := (der <> '') and
            (AsnNext(pos, der) = ASN1_SEQ) and
            (AsnNextRaw(pos, der, tbs) = ASN1_SEQ) and
            Signed.FromDer(tbs) and
            AsnNextAlgoOid(pos, der, oid, nil) and
            OidToXsa(oid, fSignatureAlgorithm) and
            (AsnNextRaw(pos, der, fSignatureValue) = ASN1_BITSTR) and
            (fSignatureAlgorithm = Signed.Signature);
end;

function TX509.FromPem(const pem: TCertPem): boolean;
begin
  result := FromDer(PemToDer(pem));
end;

function TX509.FingerPrint(algo: THashAlgo): RawUtf8;
begin
  result := HashFull(algo, ToDer);
end;

procedure TX509.AfterModified;
begin
  fCachedDer := '';
  fSignatureValue := '';
  Signed.AfterModified;
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
end;


initialization
  InitializeUnit;
  
end.
