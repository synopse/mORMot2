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

  /// known X509 Certificates attributes
  // - as available via TX501Name.Names[]
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
    xaN,    // name (41)
    xaS,    // surname (4)
    xaG,    // givenName (42)
    xaI,    // initials (43)
    xaQ,    // qualifier (46)
    xaP,    // pseudonym (65)
    xaE);   // email

  /// Possible certificate validation results
  /// Certificate extensions
  TXExtensions = set of (
    xeBasicConstraints,   // X509_EXT_BASIC_CONSTRAINTS
    xePathLenConstraint,  // X509_EXT_PATH_LEN_CONSTRAINT
    xeKeyUsage,           // X509_EXT_KEY_USAGE
    xeSubjectAltName,     // X509_EXT_SUBJECT_ALT_NAME
    xeIssuerAltName,      // X509_EXT_ISSUER_ALT_NAME
    xeExtKeyUsage);       // X509_EXT_EXT_KEY_USAGE

  /// Certificate Key Usage - see RFC 5280 Section 4.2.1.3
  TXKeyUsage = set of (
    kuDigitalsignature, // X509_KEY_USAGE_DIGITAL_SIGNATURE
    kuNonRepudiation,   // X509_KEY_USAGE_NON_REPUDIATION
    kuKeyEncipherment,  // X509_KEY_USAGE_KEY_ENCIPHERMENT
    kuDataEncipherment, // X509_KEY_USAGE_DATA_ENCIPHERMENT
    kuKeyAgreement,     // X509_KEY_USAGE_KEY_AGREEMENT
    kuKeyCertSign,      // X509_KEY_USAGE_KEY_CERT_SIGN
    kuCrlSign,          // X509_KEY_USAGE_CRL_SIGN
    kuEncipherOnly,     // X509_KEY_USAGE_ENCIPHER_ONLY
    kuDecipherOnly);    // X509_KEY_USAGE_DECIPHER_ONLY

  /// Certificate Extended Key Usage
  TXExtendedKeyUsage = set of (
    xkuAny,        // X509_EXT_KEY_USAGE_ANY
    xkuServerAuth, // X509_EXT_KEY_USAGE_SERVER_AUTH
    xkuClientAuth, // X509_EXT_KEY_USAGE_CLIENT_AUTH
    xkuOCSP);      // X509_EXT_KEY_USAGE_OCSP

  /// supported TX509.SignatureAlgorithm values
  // - we support RSA and ECC-256 asymmetric algorithms using our own units
  // - for safety, any unsafe algorithms (e.g. MD5 or SHA-1) are not defined
  TX509SignatureAlgorithm = (
    xsaNone,
    xsaSha256Rsa,
    xsaSha384Rsa,
    xsaSha512Rsa,
    xsaSha256Ecc256);

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
    Others: array of record
      RawOid: RawByteString;
      Value: RawUtf8;
    end;
    /// the raw ASN1_SEQ encoded value of this name
    function Binary: RawByteString;
    /// unserialize the X.501 Type Name from raw ASN1_SEQ binary
    function FromAsn(const seq: TAsnObject): boolean;
    /// unserialize the X.501 Type Name from the next raw ASN1_SEQ binary
    function FromAsnNext(var pos: integer; const der: TAsnObject): boolean;
    /// to be called once any field has been changed to refresh the Binary cache
    procedure AfterUpdate;
    /// return the hash of the normalized Binary of this field
    function ToDigest(algo: THashAlgo = hfSha1): RawUtf8;
  end;

const
  /// the OID of all known attributes, as defined in RFC 3780 Appendix A.1
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
    '2.5.4.41',                   // xaN   name (41)
    '2.5.4.4',                    // xaS   surname (4)
    '2.5.4.42',                   // xaG   givenName (42)
    '2.5.4.43',                   // xaI   initials (43)
    '2.5.4.46',                   // xaQ   qualifier (46)
    '2.5.4.65',                   // xaP   pseudonym (65)
    '1.2.840.113549.1.9.1');      // xaE   email

const
  OID_ID_CE                          = '2.5.29';
  OID_ID_CE_SUBJECT_KEY_IDENTIFIER   = '2.5.29.14';
  OID_ID_CE_KEY_USAGE                = '2.5.29.15';
  OID_ID_CE_SUBJECT_ALT_NAME         = '2.5.29.17';
  OID_ID_CE_ISSUER_ALT_NAME          = '2.5.29.18';
  OID_ID_CE_BASIC_CONTRAINTS         = '2.5.29.19'; // 2.5.29.10 is obsolete
  OID_ID_CE_CRL_DISTRIBUTION_POINTS  = '2.5.29.31';
  OID_ID_CE_CERTIFICATE_POLICIES     = '2.5.29.32';
  OID_ID_CE_CERTIFICATE_POLICIES_ANY = '2.5.29.32.0';
  OID_ID_CE_POLICY_MAPPINGS          = '2.5.29.33';
  OID_ID_CE_AUTH_KEY_IDENTIFIER      = '2.5.29.35'; // 2.5.29.1 is obsolete
  OID_ID_CE_EXT_KEY_USAGE            = '2.5.29.37';
  OID_ID_CE_EXT_KEY_USAGE_ANY        = '2.5.29.37.0';

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
  OID_X962_ECDSA_P256  = '1.2.840.10045.3.1.7'; // as generated by OpenSSL

  OID_THAWTE_ED25519   = '1.3.101.112';


{ **************** X.509 Certificates }

type
  /// a X509 Certificate, as defined in RFC 5280 #4.1.2
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
    Signature: TX509SignatureAlgorithm;
    /// identifies the entity that has signed and issued the certificate
    Issuer: TX501Name;
    ///
    NotBefore: TUnixTime;
    /// hexadecimal of a positive integer assigned by the CA to each certificate
    // - e.g. '04:f9:25:39:39:f8:ce:79:1a:a4:0e:b3:fa:72:e3:bc:9e:d6'
    function SerialNumberHex: RawUtf8;
    /// decimal text of a positive integer assigned by the CA to each certificate
    // - e.g. '330929475774275458452528262248458246563660'
    function SerialNumberText: RawUtf8;
    /// serialize those fields into ASN.1 DER binary
    function ToDer: TAsnObject;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.2 encoding
    function FromDer(const der: TCertDer): boolean;
    /// to be called once any field has been changed to refresh ToDer cache
    // - following RFC 5280 #4.1.2 encoding
    procedure AfterUpdate;
  end;

  /// a X509 signed Certificate, as defined in RFC 5280
  {$ifdef USERECORDWITHMETHODS}
  TX509 = record
  {$else}
  TX509 = object
  {$endif USERECORDWITHMETHODS}
  public
    /// actual to-be-signed Certificate content
    Signed: TX509Content;
    /// the cryptographic algorithm used by the CA over the Signed field
    SignatureAlgorithm: TX509SignatureAlgorithm;
    /// raw binary digital signature computed upon Signed.ToDer
    SignatureValue: RawByteString;
    /// rest all internal context
    procedure Clear;
    /// generate the SignatureAlgorithm/SignatureValue using a RSA private key
    procedure Sign(RsaAuthority: TRsa); overload;
    /// generate the SignatureAlgorithm/SignatureValue using a ECC256 private key
    procedure Sign(const EccKey: TEccPrivateKey); overload;
    /// generate the SignatureAlgorithm/SignatureValue using a RSA public key
    function Verify(RsaAuthority: TRsa): boolean; overload;
    /// generate the SignatureAlgorithm/SignatureValue using a ECC256 public key
    function Verify(const EccKey: TEccPublicKey): boolean; overload;
    /// serialize those fields into ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function ToDer: TCertDer;
    /// unserialize those fields from ASN.1 DER binary
    // - following RFC 5280 #4.1.1 encoding
    function FromDer(const der: TCertDer): boolean;
  end;

const
  ASN1_OID_SIGNATURE: array[TX509SignatureAlgorithm] of RawUtf8 = (
     '',
     '1.2.840.113549.1.1.11', // xsaSha256Rsa
     '1.2.840.113549.1.1.12', // xsaSha384Rsa
     '1.2.840.113549.1.1.13', // xsaSha512Rsa
     '1.2.840.10045.4.3.2');  // xsaSha256Ecc256

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
    raise EX509.CreateUtf8('Unexpected Asn1OidSignatureSeq(%)', [ord(xsa)]);
  end;
end;

function AsnNextSeqToXsa(var pos: integer; const der: TAsnObject;
  out xsa: TX509SignatureAlgorithm): boolean;
var
  seq, oid: RawByteString;
  p: integer;
  x: TX509SignatureAlgorithm;
begin
  p := 1;
  if (AsnNextRaw(pos, der, seq) = ASN1_SEQ) and
     (AsnNext(p, seq, @oid) = ASN1_OBJID) and
     (oid <> '') then
    for x := succ(low(x)) to high(x) do
      if IdemPropNameU(ASN1_OID_SIGNATURE[x], oid) then
      begin
        xsa := x;
        result := true;
        exit;
      end;
  result := false;
end;


{ TX509Name }

var
  XA_OID_ASN: array[TXAttr] of TAsnObject; // filled from XA_OID[]

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
                      AsnOid(pointer(RawOid)),
                      AsnText(Value)
                    ])
                  ]));
  fCachedAsn := Asn(ASN1_SEQ, [tmp]);
end;

function TX501Name.Binary: RawByteString;
begin
  if fCachedAsn = '' then
    ComputeAsn;
  result := fCachedAsn;
end;

function TX501Name.FromAsn(const seq: TAsnObject): boolean;
var
  posseq, posone, o: integer;
  a, xa: TXAttr;
  one, oid, v: RawByteString;
begin
  result := false;
  AfterUpdate;
  o := 0;
  posseq := 1;
  while AsnNextRaw(posseq, seq, one) = ASN1_SETOF do
  begin
    posone := 1;
    while AsnNext(posone, one) = ASN1_SEQ do
      if (AsnNextRaw(posone, one, oid) <> ASN1_OBJID) or
         not (AsnNext(posone, one, @v) in ASN1_TEXT) then
        exit
      else
      begin
        xa := xaNone;
        for a := succ(low(a)) to high(a) do
          if oid = XA_OID_ASN[a] then
          begin
            xa := a;
            break;
          end;
        if xa = xaNone then
        begin
          // unsupported OID
          SetLength(Others, o + 1);
          with Others[o] do
          begin
            RawOid := oid;
            Value := v;
          end;
        end
        else if IsValidUtf8(v) then
          // known attribute
          AddToCsv(v, Names[xa])
        else
          exit; // incorrect encoding or corrupted input
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

procedure TX501Name.AfterUpdate;
begin
  fCachedAsn := '';
end;

function TX501Name.ToDigest(algo: THashAlgo): RawUtf8;
begin
  result := HashFull(algo, Binary);
end;


{ **************** X.509 Certificates }

{ TX509Content }

procedure TX509Content.ComputeCachedDer;
begin
  fCachedDer := Asn(ASN1_SEQ, [
                  Asn(Version),
                  Asn(ASN1_INT, SerialNumber),
                  XsaToSeq(Signature)
                ]);
end;

function TX509Content.SerialNumberHex: RawUtf8;
begin
  ToHumanHex(result, pointer(SerialNumber), length(SerialNumber));
end;

function TX509Content.SerialNumberText: RawUtf8;
begin
  result := BigIntToText(SerialNumber);
end;

function TX509Content.ToDer: TAsnObject;
begin
  if fCachedDer = '' then
    ComputeCachedDer;
  result := fCachedDer;
end;

function TX509Content.FromDer(const der: TCertDer): boolean;
var
  pos, vt: integer;
begin
  result := false;
  fCachedDer := der;
  pos := 1;
  Version := AsnNextInteger(pos, der, vt);
  if (vt <> ASN1_INT) or
     not (vt in [2..3]) or
     (AsnNextRaw(pos, der, SerialNumber) <> ASN1_INT) or
     not AsnNextSeqToXsa(pos, der, Signature) or
     not Issuer.FromAsnNext(pos, der) then
    exit;

  result := true;
end;

procedure TX509Content.AfterUpdate;
begin
  fCachedDer := ''; // just reset the cache
end;


{ TX509 }

procedure TX509.Clear;
begin
  Finalize(self);
  FillCharFast(self, SizeOf(self), 0);
end;

procedure TX509.Sign(RsaAuthority: TRsa);
var
  dig: THash256;
begin
  if not RsaAuthority.HasPrivateKey then
    raise EX509.Create('TX509.Sign with no RsaAuthority');
  SignatureAlgorithm := xsaSha256Rsa;
  dig := Sha256Digest(Signed.ToDer);
  SignatureValue := RsaAuthority.Sign(@dig, hfSHA256);
end;

procedure TX509.Sign(const EccKey: TEccPrivateKey);
var
  sig: TEccSignature;
begin
  SignatureAlgorithm := xsaSha256Ecc256;
  if Ecc256r1Sign(EccKey, Sha256Digest(Signed.ToDer), sig) then
    SignatureValue := EccToDer(sig)
  else
    raise EX509.Create('TX509.SignEcc failed');
end;

const
  XSA_TO_HF: array[xsaSha256Rsa .. xsaSha512Rsa] of THashAlgo = (
    hfSha256,
    hfSha384,
    hfSha512);

function TX509.Verify(RsaAuthority: TRsa): boolean;
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

function TX509.Verify(const EccKey: TEccPublicKey): boolean;
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
  if (SignatureAlgorithm = xsaNone) or
     (SignatureValue = '') then
    raise EX509.Create('TX509.ToDer with no previous Sign() call');
  result := Asn(ASN1_SEQ, [
              Signed.ToDer,
              XsaToSeq(SignatureAlgorithm),
              Asn(ASN1_BITSTR, SignatureValue)
            ]);
end;

function TX509.FromDer(const der: TCertDer): boolean;
var
  p: integer;
  tbs: RawByteString;
begin
  Clear;
  p := 1;
  result := (der <> '') and
            (AsnNext(p, der) = ASN1_SEQ) and
            (AsnNextRaw(p, der, tbs) = ASN1_SEQ) and
            Signed.FromDer(tbs) and
            AsnNextSeqToXsa(p, der, SignatureAlgorithm) and
            (AsnNextRaw(p, der, SignatureValue) = ASN1_BITSTR);
end;


procedure InitializeUnit;
var
  a: TXAttr;
begin
  for a := succ(low(a)) to high(a) do
    XA_OID_ASN[a] := AsnOid(XA_OID[a]);
end;

initialization
  InitializeUnit;
  
end.
