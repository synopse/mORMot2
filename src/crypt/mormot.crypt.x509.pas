/// Framework Core ASN.1 / X.509 Support
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.x509;

{
  *****************************************************************************

   ASN.1 Encoding and X.509 Certificates Implementation 
    - ASN.1 Encoding Decoding
    - X.509 Encoding Decoding
    - X.509 Certificates

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
  mormot.crypt.secure;


{ **************** ASN.1 Encoding Decoding }

// note: all ASN.1 types start with TAsn

type
  /// exception which may be raised during ASN.1 processing
  EAsn = class(ESynException);

  /// the recognized ASN.1 TAG NUMBER
  // - only most simple kind of tags are actually supported by this unit
  TAsnTagKind = (
    atEoc,              // ASN1_TAG_EOC
    atBoolean,          // ASN1_TAG_BOOLEAN
    atInteger,          // ASN1_TAG_INTEGER
    atBitString,        // ASN1_TAG_BITSTRING
    atOctetString,      // ASN1_TAG_OCTETSTRING
    atNull,             // ASN1_TAG_NULL
    atOid,              // ASN1_TAG_OID
    atObjectDesc,       // ASN1_TAG_OBJECT_DESCRIPTOR
    atExternal,         // ASN1_TAG_EXTERNAL
    atReal,             // ASN1_TAG_REAL
    atEnumerated,       // ASN1_TAG_ENUMERATED
    atEmbeddedPdv,      // ASN1_TAG_EMBEDDED_PDV
    atUtf8String,       // ASN1_TAG_UTF8STRING
    atRelativeOid,      // ASN1_TAG_RELATIVE_OID
    atUnusedE,
    atUnusedF,
    atSequence,         // ASN1_TAG_SEQUENCE
    atSet,              // ASN1_TAG_SET
    atNumericString,    // ASN1_TAG_NUMERICSTRING
    atPrintableString,  // ASN1_TAG_PRINTABLESTRING
    atTG1String,        // ASN1_TAG_TG1STRING
    atVideoTextString,  // ASN1_TAG_VIDEOTEXSTRING
    atIA5String,        // ASN1_TAG_IA5STRING
    atUtcTime,          // ASN1_TAG_UTCTIME
    atGenerizedTime,    // ASN1_TAG_GENERALIZEDTIME
    atGraphicString,    // ASN1_TAG_GRAPHICSTRING
    atVisibleString,    // ASN1_TAG_VISIBLESTRING
    atGeneralString,    // ASN1_TAG_GENERALSTRING
    atUniversalString,  // ASN1_TAG_UNIVERSALSTRING
    atCharacterString,  // ASN1_TAG_CHARACTERSTRING
    atBmpString,        // ASN1_TAG_BMPSTRING
    atLongForm,         // ASN1_TAG_LONGFORM
    atUnknown);

  /// defines the ASN.1 TAG CLASS
  TAsnTagClass = (
    acUniversal,        // ASN1_CLASS_UNIVERSAL
    acApplication,      // ASN1_CLASS_APPLICATION
    acContextSpecific,  // ASN1_CLASS_CONTEXT_SPECIFIC
    acPrivate);         // ASN1_CLASS_PRIVATE

  TAsnFlags = set of (
    afConstructed);

  /// used to store an ASN.1 OID content as integers
  TAsnOid = type TCardinalDynArray;

  /// used to store an ASN.1 OID content as text
  TAsnOidText = type RawUtf8;

  /// used to store an ASN.1 atInteger / atBitString content
  // - note that Delphi doesn't allow "type TBytes" so TByteDynArray alias
  TAsnInt = type TByteDynArray;

  /// used to store a binary buffer
  TAsnBinary = type RawByteString;

  /// used to store an ASN.1 TAG content
  TAsnTag = object
    /// mostly acUniversal kind of TAG
    TagClass: TAsnTagClass;
    /// defines this TAG content
    TagKind: TAsnTagKind;
    /// additional information about this tag
    Flags: TAsnFlags;
    /// low-level TAG-specific information
    // - either the TagKind ordinal value, or the 32-bit custom identifier for
    // atLongForm kind of TAG
    TagNumber: cardinal;
    /// the actual data of this TAG
    Data: PAnsiChar;
    /// how many bytes are stored in this TAG
    Len: PtrUInt;
    /// low-level unserialization from DER encoded binary
    procedure Parse(var reader: TFastReader);
    /// convert an atOid tag into an array of unsigned integers
    function ToOid: TAsnOid; overload;
    /// convert an atOid tag into a text array of unsigned integers
    procedure ToOid(out text: shortstring); overload;
    /// convert an atOid tag into a text array of unsigned integers
    procedure ToOid(out text: TAsnOidText); overload;
    /// convert an atBoolean tag into its true/false value
    function ToBoolean: boolean;
    /// convert an atInteger/atBitString tag into its 32-bit unsigned value
    // - atBitString is converted from big-endian to a little-endian cardinal
    function ToUInt32: cardinal;
    /// retrieve an atInteger/atBitString tag in its native binary form
    // - endianess is not changed, so atBitString is still big-endian encoded
    function ToInteger: TAsnInt;
    /// convert a string tag into its UTF-8 text
    // - UTF-16 atBmpString is decoded into UTF-8 as expected
    function ToUtf8: RawUtf8;
    /// retrieve a tag in its native binary form
    // - the leading 0 of unused bits in atBitString will be trimmed
    // - other kind of tag will return Data/Len as raw binary
    function ToBuffer: TAsnBinary;
  end;

  PAsnTag = ^TAsnTag;

  /// stores an array of ASN.1 TAGs
  TAsnTags = array of TAsnTag;

  /// decode some ASN.1 encoded binary buffer
  TAsnParser = object
  public
    /// safe and efficient binary parsing of the input buffer
    Reader: TFastReader;
    /// internal ASN.1 Tag value used during parsing
    Tag: TAsnTag;
    /// initialize the parser
    procedure Init(buf: pointer; buflen: PtrInt); overload;
    /// initialize the parser and decode the first tags
    function Init(const buf: RawByteString;
      const kinds: array of TAsnTagKind): boolean; overload;
    /// quickly check the upcoming Tag type
    // - return atUnknown if TAsnTagClass does not match isclass
    function PeekTag(isclass: TAsnTagClass = acUniversal): TAsnTagKind;
    /// fill the Tag member with the next tag and check its type
    function NextTag(iskind: TAsnTagKind; isclass: TAsnTagClass = acUniversal): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// expect the next tag to be an atOid, and returns its decoded value
    procedure NextNull;
    /// expect the next tag to be an atOid, and returns its decoded value
    function NextOid: TAsnOid;
    /// expect the next tag to be an atBoolean, and returns its decoded value
    function NextBoolean: boolean;
    /// expect the next tag to be a 32-bit atInteger/atBitString
    function NextUInt32: cardinal;
    /// expect the next tag to be a atInteger/atBitString of any size
    function NextInteger: TAsnInt; overload;
    /// expect the next tag to be a atInteger of any size
    function NextInteger(out value: TAsnInt): boolean; overload;
    /// expect the next tag to be a atBitString of any size
    function NextBigInt(out value: TAsnInt): boolean;
    /// expect the next tag to be a string, and returns its UTF-8 content
    function NextUtf8: RawUtf8;
    /// return most used tag contentin its raw binary format
    function NextBuffer: RawByteString;
   end;

const
  ASN1_MAX_OID_LEN = 20;

  ASN1_TAG: array[TAsnTagKind] of PUtf8Char = (
    'EOC',               // atEoc
    'BOOLEAN',           // atBoolean
    'INTEGER',           // atInteger
    'BITSTRING',         // atBitString
    'OCTETSTRING',       // atOctetString
    'NULL',              // atNull
    'OID',               // atOid
    'OBJECT_DESCRIPTOR', // atObjectDesc
    'EXTERNAL',          // atExternal
    'REAL',              // atReal
    'ENUMERATED',        // atEnumerated
    'EMBEDDED_PDV',      // atEmbeddedPdv
    'UTF8STRING',        // atUtf8String
    'RELATIVE_OID',      // atRelativeOid
    '',
    '',
    'SEQUENCE',          // atSequence
    'SET',               // atSet
    'NUMERICSTRING',     // atNumericString
    'PRINTABLESTRING',   // atPrintableString
    'TG1STRING',         // atTG1String
    'VIDEOTEXSTRING',    // atVideoTextString
    'IA5STRING',         // atIA5String
    'UTCTIME',           // atUtcTime
    'GENERALIZEDTIME',   // atGenerizedTime
    'GRAPHICSTRING',     // atGraphicString
    'VISIBLESTRING',     // atVisibleString
    'GENERALSTRING',     // atGeneralString
    'UNIVERSALSTRING',   // atUniversalString
    'CHARACTERSTRING',   // atCharacterString
    'BMPSTRING',         // atBmpString
    'LONGFORM',          // atLongForm
    '');

  ASN1_TOKEN: array[TAsnTagKind] of AnsiChar = (
    'x', // atEoc
    'b', // atBoolean
    'i', // atInteger
    'B', // atBitString
    'O', // atOctetString with potential (...)
    'n', // atNull
    'I', // atOid
    'D', // atObjectDesc
    'X', // atExternal
    'R', // atReal
    'e', // atEnumerated always with (...)
    'p', // atEmbeddedPdv
    'u', // atUtf8String
    'r', // atRelativeOid
    #0,
    #0,
    'S', // atSequence   always with (...)
    'E', // atSet        always with (...)
    'N', // atNumericString
    'P', // atPrintableString
    'T', // atTG1String
    'V', // atVideoTextString
    'A', // atIA5String
    't', // atUtcTime
    'z', // atGenerizedTime
    'G', // atGraphicString
    'V', // atVisibleString
    'L', // atGeneralString
    'U', // atUniversalString
    'C', // atCharacterString
    'M', // atBmpString
    'l', // atLongForm
    #0);

  ASN1_CLASS: array[TAsnTagClass] of PUtf8Char = (
    'UNIVERSAL',         // acUniversal
    'APPLICATION',       // acApplication
    'CONTEXT_SPECIFIC',  // acContextSpecific
    'PRIVATE');          // acPrivate


function ToText(k: TAsnTagKind): PShortString; overload; // see ASN1_TAG[]
function ToText(c: TAsnTagClass): PShortString; overload;

function IsEqual(const A, B: TAsnOid): boolean; overload;
function StartWith(const A, Prefix: TAsnOid): boolean; overload;
function StartWith(const A: TAsnOid; Prefix: PUtf8Char): boolean; overload;
function ToText(const O: TAsnOid): RawUtf8; overload;

/// create a list of ASN.1 nodes
// - all TAGs will be unserialized in-order into the returned array
function AsnParse(buf: pointer; buflen: PtrInt): TAsnTags;


{ **************** X.509 Encoding Decoding}

// note: all X.509 types start with TX

type

  EX509 = class(ESynException);

  /// X509 Certificates attributes
  TXAttr = (
    xaNone, // X509_NAME_ATTR_NONE
    xaDC,   // X509_NAME_ATTR_DC  = domainComponent
    xaCN,   // X509_NAME_ATTR_CN  = commonName
    xaC,    // X509_NAME_ATTR_C   = countryName
    xaL,    // X509_NAME_ATTR_L   = localityName
    xaST,   // X509_NAME_ATTR_ST  = stateOrProvinceName
    xaO,    // X509_NAME_ATTR_O   = organizationName
    xaOU);  // X509_NAME_ATTR_OU  = organizationalUnitName

  /// Possible certificate validation results
  TXValidate = (
    xvOk,                     // X509_VALIDATE_OK
    xvBadCertificate,         // X509_VALIDATE_BAD_CERTIFICATE
    xvUnsupportedCertificate, // X509_VALIDATE_UNSUPPORTED_CERTIFICATE
    xvCertificateRevoked,     // X509_VALIDATE_CERTIFICATE_REVOKED
    xvCertificateExpired,     // X509_VALIDATE_CERTIFICATE_EXPIRED
    xvCertificateUnknown,     // X509_VALIDATE_CERTIFICATE_UNKNOWN
    xvUnknownCA);             // X509_VALIDATE_UNKNOWN_CA

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

  /// Certificate File Types
  TXFileType = (
    ftPem,
    fTAsn);

  /// X.509 Binary Serial Number - up to X509_MAX_SERIAL_NUM_LEN bytes
  TXSerialNumber = RawByteString;

  TXNameAttribute = record
    Attr: TXAttr;
    Value: RawUtf8;
  end;
  TXNameAttributes = array of TXNameAttribute;

  TXAlgorithmIdentifier = TAsnOidText;

  TXValue = record
    Algo: TXAlgorithmIdentifier;
    Value: TAsnBinary;
  end;

  TXPublicKey = type TXValue;
  TXPrivateKey = type TXValue;
  TXSignature = type TXValue;

  TXRsaPublicKey = record
    PubMod, PubExp: TAsnInt;
  end;
  TXRsaPrivateKey = record
    PubMod, PubExp, D, P, Q, DP, DQ, U: TAsnInt;
  end;

  TXDsaPublicKey = record
    P, Q, G: TAsnInt;
  end;
  TXDsaPrivateKey = record
    P, Q, G, X: TAsnInt;
  end;

  TXEccPublicKey = record
    P: TAsnInt; // match TEccPublicKey compressed form (33 bytes)
  end;
  TXEccPrivateKey = record
    Q: TAsnInt; // match TEccPrivateKey compressed form (32 bytes)
    P: TAsnInt; // match TEccPublicKey compressed form (33 bytes)
  end;

  /// the record types as recognized by XRegister()
  TXKeyKind = (
    xkRsaPublicKey,
    xkRsaPrivateKey,
    xkDsaPublicKey,
    xkDsaPrivateKey,
    xkEccPublicKey,
    xkEccPrivateKey);

/// register a packed record type into the X.509 parsing
// - follow ASN1_TOKEN[] markup, with (...) nested structures for E e S O
// - constant (dotted) numbers wil be processed as expected OID/integers
procedure XRegister(Kind: TXKeyKind; RecordInfo: PRttiInfo;
  const Def: array of RawUtf8);


const
  X509_MAX_NAME_ATTRIBUTES = 20;
  X509_MAX_SERIAL_NUM_LEN = 20;

  X509_NAME_ATTR: array[TXAttr] of string[2] = (
    '--',   // xaNone
    'DC',   // xaDC
    'CN',   // xaCN
    'C',    // xaC
    'L',    // xaL
    'ST',   // xaST
    'O',    // xaO
    'OU');  // xaOU


// constants to be used with StartWith(TAsnOid,PUtf8Char) or ToOid(shortstring)

const
  OID_ATTRIBUTE_TYPES   = '2.5.4';
  OID_ATTRIBUTE_TYPE_CN = '2.5.4.3';
  OID_ATTRIBUTE_TYPE_C  = '2.5.4.6';
  OID_ATTRIBUTE_TYPE_L  = '2.5.4.7';
  OID_ATTRIBUTE_TYPE_ST = '2.5.4.8';
  OID_ATTRIBUTE_TYPE_O  = '2.5.4.10';
  OID_ATTRIBUTE_TYPE_OU = '2.5.4.11';

  OID_EMAIL_ADDRESS     = '1.2.840.113549.1.9.1';
  OID_ATTRIBUTE_TYPE_DC = '0.9.2342.19200300.100.1.25';

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
  TXName = class(TSynPersistent)
  published

  end;

  TXCertificate = class(TSynPersistentStoreJson)
  private
    fIssuer: TXName;
  protected
    fVersion: integer;
    fSerial: TXSerialNumber;
    fSignatureAlgorithm: TXAlgorithmIdentifier;
  public

  published
    property Version: integer
      read fVersion;
    property Serial: TXSerialNumber
      read fSerial;
    property SignatureAlgorithm: TXAlgorithmIdentifier
      read fSignatureAlgorithm;
    property Issuer: TXName
      read fIssuer;
  end;


implementation


{ **************** ASN.1 Encoding Decoding }

function ToText(k: TAsnTagKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TAsnTagKind), ord(k));
end;

function ToText(c: TAsnTagClass): PShortString;
begin
  result := GetEnumName(TypeInfo(TAsnTagClass), ord(c));
end;

function IsEqual(const A, B: TAsnOid): boolean;
begin
  result := (length(A) = length(B)) and
            CompareMem(pointer(A), pointer(B), length(A) * SizeOf(A[0]));
end;

function StartWith(const A, Prefix: TAsnOid): boolean;
begin
  result := (length(A) >= length(Prefix)) and
            CompareMem(pointer(A), pointer(Prefix), length(Prefix) * SizeOf(A[0]));
end;

function StartWith(const A: TAsnOid; Prefix: PUtf8Char): boolean;
var
  i: PtrInt;
begin
  result := false;
  for i := 0 to length(A) - 1 do
    if Prefix = nil then
      break
    else if GetNextItemCardinal(Prefix) <> A[i] then
      exit;
  result := true;
end;

function ToText(const O: TAsnOid): RawUtf8;
begin
  result := IntegerDynArrayToCsv(pointer(O), length(O), '', '', false, '.');
end;


{ TAsnTag }

procedure TAsnTag.Parse(var reader: TFastReader);
var
  v: cardinal;
begin
  FillCharFast(self, SizeOf(self), 0);
  if reader.EOF then
    reader.ErrorOverflow;
  v := reader.NextByte;
  TagClass := TAsnTagClass(v shr 6); // 2 highest bits are the class
  if v and (1 shl 5) <> 0 then
    include(Flags, afConstructed);
  TagKind := TAsnTagKind(v and $1f);
  if TagKind = atLongForm then
    TagNumber := reader.VarUInt32
  else
    TagNumber := ord(TagKind);
  v := reader.NextByte;
  if v and $80 <> 0 then
  begin
    v := v and $7f;
    if v > 4 then
      raise EAsn.CreateUtf8('Unexpected Len=%', [v]);
    reader.Copy(@Len, v);
  end
  else
    Len := v;
  Data := reader.Next(Len);
end;

function TAsnTag.ToOid: TAsnOid;
var
  v, f: cardinal;
  r: TFastReader;
  n: PtrInt;
begin
  if (Len = 0) or
     (TagClass <> acUniversal) or
     (TagKind <> atOid) then
    raise EAsn.CreateUtf8('ToOID with len=% class=% kind=%',
      [Len, ToText(TagClass)^, ASN1_TAG[TagKind]]);
  SetLength(result, ASN1_MAX_OID_LEN);
  r.Init(Data, Len);
  v := r.VarUInt32;
  f := v div 40; // first 2 nodes are encoded in the first integer
  if f > 2 then
    f := 2;
  result[0] := f;
  result[1] := v - f * 40;
  n := 2;
  while not r.EOF do // process the remaining integers
  begin
    if n = ASN1_MAX_OID_LEN then
      r.ErrorOverflow;
    result[n] := r.VarUInt32;
    inc(n);
  end;
  DynArrayFakeLength(result, n);
end;

procedure TAsnTag.ToOid(out text: shortstring);
var
  v, f: cardinal;
  p, pmax: PByte;
begin
  p := pointer(Data);
  pmax := pointer(Data + Len);
  p := FromVarUInt32Safe(p, pmax, v);
  f := v div 40; // first 2 nodes are encoded in the first integer
  if f > 2 then
    f := 2;
  text[0] := #2;
  text[1] := AnsiChar(f + ord('0')); // first integer is 0,1,2
  text[2] := '.';
  AppendShortInteger(v - f * 40, text);
  inc(text[0]);
  text[ord(text[0])] := '.';
  while p <> nil do // process the remaining integers
  begin
    p := FromVarUInt32Safe(p, pmax, v);
    AppendShortInteger(v, text);
    inc(text[0]);
    text[ord(text[0])] := '.';
  end;
  dec(text[0]);
end;

procedure TAsnTag.ToOid(out text: TAsnOidText);
var
  tmp: ShortString;
begin
  ToOid(tmp);
  FastSetString(RawUtf8(text), @tmp[1], ord(tmp[0]));
end;

function TAsnTag.ToBoolean: boolean;
begin
  if (TagClass <> acUniversal) or
     (TagKind <> atBoolean) then
    raise EAsn.Create('ToBoolean');
  result := Data[0] <> #0;
end;

function TAsnTag.ToUInt32: cardinal;
begin
  if (TagClass <> acUniversal) or
     (Len = 0) or
     (Len > SizeOf(integer)) then
    raise EAsn.Create('ToUInt32');
  result := 0;
  case TagKind of
    atInteger:
      begin
        if Data[0] = #0 then
        begin
          inc(Data); // a leading zero indicates a positive value -> ignore
          dec(Len);
          if Len = 0 then
            exit; // was a plain and simple 0 value
        end;
        MoveSmall(Data, @result, Len); // was stored as little-endian
      end;
    atBitString:
      begin
        if (Len <= 1) or
           (Data[0] <> #0) then
          raise EAsn.CreateUtf8('ToUInt32 BitString unused=%', [ord(Data[0])]);
        inc(Data); // just ignore the unused number of bits = 0 (as in BER)
        dec(Len);
        MoveSmall(Data, PAnsiChar(@result) + 4 - Len, Len);
        result := bswap32(result); // was stored as big-endian
      end
  else
    raise EAsn.CreateUtf8('ToUInt32 with %', [ASN1_TAG[TagKind]]);
  end;
end;

function TAsnTag.ToInteger: TAsnInt;
begin
  if (TagClass <> acUniversal) or
     not(TagKind in [atInteger, atBitString]) or
     (Len = 0) then
    raise EAsn.Create('ToInteger');
  result := nil;
  if (Data[0] = #0) and
     (Len <> 0) then
  begin
    inc(Data); // a leading zero indicates a positive value for atInteger
    dec(Len);                           // or 0 unused bits for atBitString
  end;
  SetLength(result, Len);
  MoveFast(Data^, pointer(result)^, Len);
end;

function TAsnTag.ToUtf8: RawUtf8;
begin
  if TagClass <> acUniversal then
    raise EAsn.Create('ToUtf8');
  case TagKind of
    atUtf8String,
    atIA5String, // ASCII is a sub-part of UTF-8
    atPrintableString:
      FastSetString(result, Data, Len);
    atBmpString:
      RawUnicodeToUtf8(pointer(Data), Len shr 1, result);
  else
    raise EAsn.CreateUtf8('ToUtf8 with %', [ASN1_TAG[TagKind]]);
  end;
end;

function TAsnTag.ToBuffer: TAsnBinary;
begin
  if TagClass <> acUniversal then
    raise EAsn.Create('ToBuffer');
  case TagKind of
    atBitString:
      begin
        if (Len = 0) or
           (Data[0] <> #0) then
          raise EAsn.CreateUtf8('ToBuffer BitString unused=%', [ord(Data[0])]);
        inc(Data); // just ignore the unused number of bits = 0 (as in BER)
        dec(Len);
      end;
    atInteger,
    atOctetString,
    atUtf8String,
    atIA5String,
    atPrintableString,
    atBmpString:
      ; // return the raw binary buffer
  else
    raise EAsn.CreateUtf8('ToBuffer with %', [ASN1_TAG[TagKind]]);
  end;
  SetString(result, Data, Len);
end;


{ TAsnParser }

procedure TAsnParser.Init(buf: pointer; buflen: PtrInt);
begin
  Reader.Init(buf, buflen);
  if Reader.EOF then
    Reader.ErrorOverflow;
end;

function TAsnParser.NextTag(iskind: TAsnTagKind; isclass: TAsnTagClass): boolean;
begin
  result := false;
  if Reader.Eof then
    exit;
  Tag.Parse(Reader);
  result := (Tag.TagClass = isclass) and
            (Tag.TagKind = iskind);
end;

function TAsnParser.Init(const buf: RawByteString;
  const kinds: array of TAsnTagKind): boolean;
var
  k: PtrInt;
begin
  result := false;
  if buf = '' then
    exit;
  Reader.Init(pointer(buf), length(buf));
  for k := 0 to high(kinds) do
    if not NextTag(kinds[k]) then
      exit;
  result := true;
end;

function TAsnParser.PeekTag(isclass: TAsnTagClass): TAsnTagKind;
var
  v: cardinal;
begin
  if Reader.EOF then
    Reader.ErrorOverflow;
  v := ord(Reader.P^);
  if TAsnTagClass(v shr 6) = isClass then
    result := TAsnTagKind(v and $1f)
  else
    result := atUnknown;
end;

function TAsnParser.NextOid: TAsnOid;
begin
  Tag.Parse(Reader);
  result := Tag.ToOid;
end;

procedure TAsnParser.NextNull;
begin
  Tag.Parse(Reader);
  if (Tag.TagClass <> acUniversal) or
     (Tag.TagKind <> atNull) then
    raise EAsn.Create('NextNull');
end;

function TAsnParser.NextBoolean: boolean;
begin
  Tag.Parse(Reader);
  result := Tag.ToBoolean;
end;

function TAsnParser.NextUInt32: cardinal;
begin
  Tag.Parse(Reader);
  result := Tag.ToUInt32;
end;

function TAsnParser.NextInteger: TAsnInt;
begin
  Tag.Parse(Reader);
  result := Tag.ToInteger;
end;

function TAsnParser.NextInteger(out value: TAsnInt): boolean;
begin
  result := NextTag(atInteger);
  if result then
    value := Tag.ToInteger;
end;

function TAsnParser.NextBigInt(out value: TAsnInt): boolean;
begin
  result := NextTag(atBitString);
  if result then
    value := Tag.ToInteger;
end;

function TAsnParser.NextUtf8: RawUtf8;
begin
  Tag.Parse(Reader);
  result := Tag.ToUtf8;
end;

function TAsnParser.NextBuffer: RawByteString;
begin
  Tag.Parse(Reader);
  result := Tag.ToBuffer;
end;


function AsnParse(buf: pointer; buflen: PtrInt): TAsnTags;
var
  reader: TFastReader;
  n: PtrInt;
begin
  result := nil;
  reader.Init(buf, buflen);
  SetLength(result, 16); // initial capacity should be enough in most cases
  n := 0;
  repeat
    if n = length(result) then
      SetLength(result, NextGrow(n));
    result[n].Parse(reader);
    inc(n);
  until reader.EOF;
  DynArrayFakeLength(result, n);
end;


{ **************** X.509 Encoding Decoding}

type
  TXRegisteredDef = record
    Definition: RawUtf8;
    Start: RawUtf8;
  end;

  TXRegistered = record
    RecordInfo: PRttiInfo;
    Definition: array of TXRegisteredDef;
  end;

var
  XRegistered: array[TXKeyKind] of TXRegistered;
  ASN1_KIND: array[AnsiChar] of TAsnTagKind;

procedure XRegister(Kind: TXKeyKind; RecordInfo: PRttiInfo; const Def: array of RawUtf8);
var
  reg: ^TXRegistered;
  i, n: PtrInt;
begin
  reg := @XRegistered[Kind];
  reg.RecordInfo := RecordInfo;
  SetLength(reg.Definition, length(Def));
  for i := 0 to high(Def) do
  begin
    reg.Definition[i].Definition := Def[i];
    n := PosExChar('%', Def[i]);
    if (n = 0) or
       (n > 255) then
      raise EX509.CreateUtf8('XRegister: invalid %', [Def[i]]);
    FastSetString(reg.Definition[i].Start, pointer(Def[i]), n - 1);

  end;
end;


{ **************** X.509 Certificates }


procedure InitializeUnit;
var
  k: TAsnTagKind;
begin
  assert(ord(atLongForm) = $1f);
  for k := low(k) to high(k) do
    ASN1_KIND[ASN1_TOKEN[k]] := k;
  XRegister(xkEccPublicKey, TypeInfo(TXEccPublicKey),
     ['S(S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_ECDSA_P256 + ' )%B )',
      'S(S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_PRIME256V1 + ' )%B )']);
  XRegister(xkEccPrivateKey, TypeInfo(TXEccPrivateKey),
     ['S(0 S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_ECDSA_P256 + ')O(S(1 %O e(%B ))))',
      'S(0 S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_ECDSA_P256 + ')O(S(0 %O )))',
      'S(0 S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_PRIME256V1 + ')O(S(1 %O e(%B ))))',
      'S(0 S(' + OID_X962_PUBLICKEY + ' ' + OID_X962_PRIME256V1 + ')O(S(0 %O )))']);
  XRegister(xkRsaPublicKey, TypeInfo(TXRsaPublicKey),
     ['S(S(' + OID_PKCS1_RSA + ' N )B(S(%I %I )))',
      'S(S(' + OID_PKCS1_RSA + ' N )O(S(0 %I %I )))',
      'S(S(' + OID_PKCS1_RSA + ' N )B(S(%I %I )))']);
  XRegister(xkRsaPrivateKey, TypeInfo(TXRsaPrivateKey),
     ['S(0 S(' + OID_PKCS1_RSA + ' N )O(S(0 %I %I %I %I %I %I %I %I )))']);


end;


initialization
  InitializeUnit;
  
end.
