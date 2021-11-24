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

type
  /// exception which may be raised during ASN.1 processing
  EAsn1 = class(ESynException);

  /// the recognized ASN.1 TAG NUMBER
  // - only most simple kind of tags are actually supported by this unit
  TAsn1TagKind = (
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
  TAsn1TagClass = (
    acUniversal,        // ASN1_CLASS_UNIVERSAL
    acApplication,      // ASN1_CLASS_APPLICATION
    acContextSpecific,  // ASN1_CLASS_CONTEXT_SPECIFIC
    acPrivate);         // ASN1_CLASS_PRIVATE

  TAsn1Flags = set of (
    afConstructed);

  /// used to store an ASN.1 OID content as integers
  TAsn1Oid = type TCardinalDynArray;

  /// used to store an ASN.1 OID content as text
  TAsn1OidText = type RawUtf8;

  /// used to store a binary buffer
  TAsn1Binary = type RawByteString;

  /// used to store an ASN.1 TAG content
  TAsn1Tag = object
    /// mostly acUniversal kind of TAG
    TagClass: TAsn1TagClass;
    /// defines this TAG content
    TagKind: TAsn1TagKind;
    /// additional information about this tag
    Flags: TAsn1Flags;
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
    function ToOid: TAsn1Oid; overload;
    /// convert an atOid tag into a text array of unsigned integers
    procedure ToOid(out text: shortstring); overload;
    /// convert an atOid tag into a text array of unsigned integers
    procedure ToOid(out text: TAsn1OidText); overload;
    /// convert an atBoolean tag into its true/false value
    function ToBoolean: boolean;
    /// convert an atInteger/atBitString tag into its 32-bit unsigned value
    // - atBitString is converted from big-endian to a little-endian cardinal
    function ToUInt32: cardinal;
    /// retrieve an atInteger/atBitString tag in its native binary form
    // - endianess is not changed, so atBitString is still big-endian encoded
    function ToInteger: TBytes;
    /// convert a string tag into its UTF-8 text
    // - UTF-16 atBmpString is decoded into UTF-8 as expected
    function ToUtf8: RawUtf8;
    /// retrieve a tag in its native binary form
    // - the leading 0 of unused bits in atBitString will be trimmed
    // - other kind of tag will return Data/Len as raw binary
    function ToBuffer: TAsn1Binary;
  end;

  PAsn1Tag = ^TAsn1Tag;

  /// stores an array of ASN.1 TAGs
  TAsn1Tags = array of TAsn1Tag;

  /// decode some ASN.1 encoded binary buffer
  TAsn1Parser = object
  public
    /// safe and efficient binary parsing of the input buffer
    Reader: TFastReader;
    /// internal ASN.1 Tag value used during parsing
    Tag: TAsn1Tag;
    /// initialize the parser
    procedure Init(buf: pointer; buflen: PtrInt); overload;
    /// initialize the parser and decode the first tags
    function Init(const buf: RawByteString;
      const kinds: array of TAsn1TagKind): boolean; overload;
    /// quickly check the upcoming Tag type
    // - return atUnknown if TAsn1TagClass does not match isclass
    function PeekTag(isclass: TAsn1TagClass = acUniversal): TAsn1TagKind;
    /// fill the Tag member with the next tag and check its type
    function NextTag(iskind: TAsn1TagKind; isclass: TAsn1TagClass = acUniversal): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// expect the next tag to be an atOid, and returns its decoded value
    procedure NextNull;
    /// expect the next tag to be an atOid, and returns its decoded value
    function NextOid: TAsn1Oid;
    /// expect the next tag to be an atBoolean, and returns its decoded value
    function NextBoolean: boolean;
    /// expect the next tag to be a 32-bit atInteger/atBitString
    function NextUInt32: cardinal;
    /// expect the next tag to be a atInteger/atBitString of any size
    function NextInteger: TBytes; overload;
    /// expect the next tag to be a atInteger of any size
    function NextInteger(out value: TBytes): boolean; overload;
    /// expect the next tag to be a atBitString of any size
    function NextBigInt(out value: TBytes): boolean;
    /// expect the next tag to be a string, and returns its UTF-8 content
    function NextUtf8: RawUtf8;
    /// return most used tag contentin its raw binary format
    function NextBuffer: RawByteString;
   end;

const
  ASN1_MAX_OID_LEN = 20;

  ASN1_TAG: array[TAsn1TagKind] of PUtf8Char = (
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

  ASN1_CLASS: array[TAsn1TagClass] of PUtf8Char = (
    'UNIVERSAL',         // acUniversal
    'APPLICATION',       // acApplication
    'CONTEXT_SPECIFIC',  // acContextSpecific
    'PRIVATE');          // acPrivate


function ToText(k: TAsn1TagKind): PShortString; overload; // see ASN1_TAG[]
function ToText(c: TAsn1TagClass): PShortString; overload;

function IsEqual(const A, B: TAsn1Oid): boolean; overload;
function StartWith(const A, Prefix: TAsn1Oid): boolean; overload;
function StartWith(const A: TAsn1Oid; Prefix: PUtf8Char): boolean; overload;
function ToText(const O: TAsn1Oid): RawUtf8; overload;

/// create a list of ASN.1 nodes
// - all TAGs will be unserialized in-order into the returned array
function Asn1Parse(buf: pointer; buflen: PtrInt): TAsn1Tags;


{ **************** X.509 Encoding Decoding}

type
  /// X509 Certificates attributes
  TX509Attr = (
    xaNone, // X509_NAME_ATTR_NONE
    xaDC,   // X509_NAME_ATTR_DC  = domainComponent
    xaCN,   // X509_NAME_ATTR_CN  = commonName
    xaC,    // X509_NAME_ATTR_C   = countryName
    xaL,    // X509_NAME_ATTR_L   = localityName
    xaST,   // X509_NAME_ATTR_ST  = stateOrProvinceName
    xaO,    // X509_NAME_ATTR_O   = organizationName
    xaOU);  // X509_NAME_ATTR_OU  = organizationalUnitName

  /// Possible certificate validation results
  TX509Validate = (
    xvOk,                     // X509_VALIDATE_OK
    xvBadCertificate,         // X509_VALIDATE_BAD_CERTIFICATE
    xvUnsupportedCertificate, // X509_VALIDATE_UNSUPPORTED_CERTIFICATE
    xvCertificateRevoked,     // X509_VALIDATE_CERTIFICATE_REVOKED
    xvCertificateExpired,     // X509_VALIDATE_CERTIFICATE_EXPIRED
    xvCertificateUnknown,     // X509_VALIDATE_CERTIFICATE_UNKNOWN
    xvUnknownCA);             // X509_VALIDATE_UNKNOWN_CA

  /// Certificate extensions
  TX509Extensions = set of (
    xeBasicConstraints,   // X509_EXT_BASIC_CONSTRAINTS
    xePathLenConstraint,  // X509_EXT_PATH_LEN_CONSTRAINT
    xeKeyUsage,           // X509_EXT_KEY_USAGE
    xeSubjectAltName,     // X509_EXT_SUBJECT_ALT_NAME
    xeIssuerAltName,      // X509_EXT_ISSUER_ALT_NAME
    xeExtKeyUsage);       // X509_EXT_EXT_KEY_USAGE

  /// Certificate Key Usage - see RFC 5280 Section 4.2.1.3
  TX509KeyUsage = set of (
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
  TX509ExtendedKeyUsage = set of (
    xkuAny,        // X509_EXT_KEY_USAGE_ANY
    xkuServerAuth, // X509_EXT_KEY_USAGE_SERVER_AUTH
    xkuClientAuth, // X509_EXT_KEY_USAGE_CLIENT_AUTH
    xkuOCSP);      // X509_EXT_KEY_USAGE_OCSP

  /// Certificate File Types
  TX509FileType = (
    ftPem,
    ftAsn1);

  /// X.509 Binary Serial Number - up to X509_MAX_SERIAL_NUM_LEN bytes
  TX509SerialNumber = RawByteString;

  TX509NameAttribute = record
    Attr: TX509Attr;
    Value: RawUtf8;
  end;
  TX509NameAttributes = array of TX509NameAttribute;

  TX509AlgorithmIdentifier = TAsn1OidText;

  TX509Value = record
    Algo: TX509AlgorithmIdentifier;
    Value: TAsn1Binary;
  end;

  TX509PublicKey = type TX509Value;
  TX509PrivateKey = type TX509Value;
  TX509Signature = type TX509Value;

  TX509RsaPublicKey = record
    Modulus: TBytes;
    PublicExponent: TBytes;
  end;

  TX509RsaPrivateKey = record
    Version: integer;
    Modulus: TBytes;
    PublicExponent: TBytes;
    PrivateExponent: TBytes;
    Prime1: TBytes;
    Prime2: TBytes;
    Exponent1: TBytes;
    Exponent2: TBytes;
    Coefficient: TBytes;
  end;

procedure FillZero(var Priv: TX509PrivateKey); overload;
  {$ifdef HASINLINE} inline; {$endif}

procedure FillZero(var Priv: TX509RsaPrivateKey); overload;
  {$ifdef HASINLINE} inline; {$endif}

function DerImport(var Parser: TAsn1Parser;
  out Pub: TX509RsaPublicKey): boolean; overload;

function DerImport(var Parser: TAsn1Parser;
  out Priv: TX509RsaPrivateKey): boolean; overload;

const
  X509_MAX_NAME_ATTRIBUTES = 20;
  X509_MAX_SERIAL_NUM_LEN = 20;

  X509_NAME_ATTR: array[TX509Attr] of string[2] = (
    '--',   // xaNone
    'DC',   // xaDC
    'CN',   // xaCN
    'C',    // xaC
    'L',    // xaL
    'ST',   // xaST
    'O',    // xaO
    'OU');  // xaOU


// constants to be used with StartWith(TAsn1Oid,PUtf8Char) or ToOid(shortstring)

const
  OID_ATTRIBUTE_TYPES   = '2,5,4';
  OID_ATTRIBUTE_TYPE_CN = '2.5.4.3';
  OID_ATTRIBUTE_TYPE_C  = '2.5.4.6';
  OID_ATTRIBUTE_TYPE_L  = '2.5.4.7';
  OID_ATTRIBUTE_TYPE_ST = '2.5.4.8';
  OID_ATTRIBUTE_TYPE_O  = '2.5.4.10';
  OID_ATTRIBUTE_TYPE_OU = '2.5.4.11';

  OID_EMAIL_ADDRESS     = '1.2.840.113549.1.9.1';
  OID_ATTRIBUTE_TYPE_DC = '0.9.2342.19200300.100.1.25';

  OID_ID_CE                         = '2.5.29';
  OID_ID_CE_KEY_USAGE               = '2.5.29.15';
  OID_ID_CE_SUBJECT_ALT_NAME        = '2.5.29.17';
  OID_ID_CE_ISSUER_ALT_NAME         = '2.5.29.18';
  OID_ID_CE_BASIC_CONTRAINTS        = '2.5.29.19';
  OID_ID_CE_CRL_DISTRIBUTION_POINTS = '2.5.29.31';
  OID_ID_CE_CERTIFICATE_POLICIES    = '2.5.29.32';
  OID_ID_CE_EXT_KEY_USAGE           = '2.5.29.37';
  OID_ID_CE_EXT_KEY_USAGE_ANY       = '2.5.29.37.0';

  OID_ID_PE                       = '1.3.6.1.5.5.7.1';
  OID_ID_PE_AUTHORITY_INFO_ACCESS = '1.3.6.1.5.5.7.1.1';
  OID_ID_KP_SERVER_AUTH           = '1.3.6.1.5.5.7.3.1';
  OID_ID_KP_CLIENT_AUTH           = '1.3.6.1.5.5.7.3.2';
  OID_ID_KP_OCSP_SIGNING          = '1.3.6.1.5.5.7.3.9';

  OID_HASH_ALGORITHMS  = '2.16.840.1.101.3.4.2';
  OID_HASH_SHA256      = '2.16.840.1.101.3.4.2.1';
  OID_HASH_SHA384      = '2.16.840.1.101.3.4.2.2';
  OID_HASH_SHA512      = '2.16.840.1.101.3.4.2.3';

  OID_HASH_SHA1        = '1.3.14.3.2.26';
  OID_HASH_SHA1_RSA    = '1.3.14.3.2.29';

  OID_PKCS1_ALGORITHMS = '1.2.840.113549.1.1';
  OID_PKCS1_RSA        = '1.2.840.113549.1.1.1';
  OID_PKCS1_MD5_RSA    = '1.2.840.113549.1.1.4';
  OID_PKCS1_SHA1_RSA   = '1.2.840.113549.1.1.5';
  OID_PKCS1_SHA256_RSA = '1.2.840.113549.1.1.11';
  OID_PKCS1_SHA384_RSA = '1.2.840.113549.1.1.12';
  OID_PKCS1_SHA512_RSA = '1.2.840.113549.1.1.13';

  OID_PKCS2_ALGORITHMS = '1.2.840.113549.2';
  OID_PKCS2_MD5        = '1.2.840.113549.2.5';

  OID_X962_PUBLICKEY   = '1.2.840.10045.2.1';
  OID_X962_PRIME256V1  = '1.2.840.10045.3.1.7';
  OID_THAWTE_ED25519   = '1.3.101.112';


{ **************** X.509 Certificates }

type
  TX509Name = class(TSynPersistent)
  published

  end;

  TX509Certificate = class(TSynPersistentStoreJson)
  private
    fIssuer: TX509Name;
  protected
    fVersion: integer;
    fSerial: TX509SerialNumber;
    fSignatureAlgorithm: TX509AlgorithmIdentifier;
  public

  published
    property Version: integer
      read fVersion;
    property Serial: TX509SerialNumber
      read fSerial;
    property SignatureAlgorithm: TX509AlgorithmIdentifier
      read fSignatureAlgorithm;
    property Issuer: TX509Name
      read fIssuer;
  end;


implementation


{ **************** ASN.1 Encoding Decoding }

function ToText(k: TAsn1TagKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TAsn1TagKind), ord(k));
end;

function ToText(c: TAsn1TagClass): PShortString;
begin
  result := GetEnumName(TypeInfo(TAsn1TagClass), ord(c));
end;

function IsEqual(const A, B: TAsn1Oid): boolean;
begin
  result := (length(A) = length(B)) and
            CompareMem(pointer(A), pointer(B), length(A) * SizeOf(A[0]));
end;

function StartWith(const A, Prefix: TAsn1Oid): boolean;
begin
  result := (length(A) >= length(Prefix)) and
            CompareMem(pointer(A), pointer(Prefix), length(Prefix) * SizeOf(A[0]));
end;

function StartWith(const A: TAsn1Oid; Prefix: PUtf8Char): boolean;
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

function ToText(const O: TAsn1Oid): RawUtf8;
begin
  result := IntegerDynArrayToCsv(pointer(O), length(O), '', '', false, '.');
end;


{ TAsn1Tag }

procedure TAsn1Tag.Parse(var reader: TFastReader);
var
  v: cardinal;
begin
  FillCharFast(self, SizeOf(self), 0);
  if reader.EOF then
    reader.ErrorOverflow;
  v := reader.NextByte;
  TagClass := TAsn1TagClass(v shr 6); // 2 highest bits are the class
  if v and (1 shl 5) <> 0 then
    include(Flags, afConstructed);
  TagKind := TAsn1TagKind(v and ord(atLongForm));
  if TagKind = atLongForm then
    TagNumber := reader.VarUInt32
  else
    TagNumber := ord(TagKind);
  v := reader.NextByte;
  if v and $80 <> 0 then
  begin
    v := v and $7f;
    if v > 4 then
      raise EAsn1.CreateUtf8('Unexpected Len=%', [v]);
    reader.Copy(@Len, v);
  end
  else
    Len := v;
  Data := reader.Next(Len);
end;

function TAsn1Tag.ToOid: TAsn1Oid;
var
  v, f: cardinal;
  r: TFastReader;
  n: PtrInt;
begin
  if (Len = 0) or
     (TagClass <> acUniversal) or
     (TagKind <> atOid) then
    raise EAsn1.CreateUtf8('ToOID with len=% class=% kind=%',
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

procedure TAsn1Tag.ToOid(out text: shortstring);
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

procedure TAsn1Tag.ToOid(out text: TAsn1OidText);
var
  tmp: ShortString;
begin
  ToOid(tmp);
  FastSetString(text, @tmp[1], ord(tmp[0]));
end;

function TAsn1Tag.ToBoolean: boolean;
begin
  if (TagClass <> acUniversal) or
     (TagKind <> atBoolean) then
    raise EAsn1.Create('ToBoolean');
  result := Data[0] <> #0;
end;

function TAsn1Tag.ToUInt32: cardinal;
begin
  if (TagClass <> acUniversal) or
     (Len = 0) or
     (Len > SizeOf(integer)) then
    raise EAsn1.Create('ToUInt32');
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
          raise EAsn1.CreateUtf8('ToUInt32 BitString unused=%', [ord(Data[0])]);
        inc(Data); // just ignore the unused number of bits = 0 (as in BER)
        dec(Len);
        MoveSmall(Data, PAnsiChar(@result) + 4 - Len, Len);
        result := bswap32(result); // was stored as big-endian
      end
  else
    raise EAsn1.CreateUtf8('ToUInt32 with %', [ASN1_TAG[TagKind]]);
  end;
end;

function TAsn1Tag.ToInteger: TBytes;
begin
  if (TagClass <> acUniversal) or
     not(TagKind in [atInteger, atBitString]) or
     (Len = 0) then
    raise EAsn1.Create('ToInteger');
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

function TAsn1Tag.ToUtf8: RawUtf8;
begin
  if TagClass <> acUniversal then
    raise EAsn1.Create('ToUtf8');
  case TagKind of
    atUtf8String,
    atIA5String, // ASCII is a sub-part of UTF-8
    atPrintableString:
      FastSetString(result, Data, Len);
    atBmpString:
      RawUnicodeToUtf8(pointer(Data), Len shr 1, result);
  else
    raise EAsn1.CreateUtf8('ToUtf8 with %', [ASN1_TAG[TagKind]]);
  end;
end;

function TAsn1Tag.ToBuffer: TAsn1Binary;
begin
  if TagClass <> acUniversal then
    raise EAsn1.Create('ToBuffer');
  case TagKind of
    atBitString:
      begin
        if (Len = 0) or
           (Data[0] <> #0) then
          raise EAsn1.CreateUtf8('ToBuffer BitString unused=%', [ord(Data[0])]);
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
    raise EAsn1.CreateUtf8('ToBuffer with %', [ASN1_TAG[TagKind]]);
  end;
  SetString(result, Data, Len);
end;


{ TAsn1Parser }

procedure TAsn1Parser.Init(buf: pointer; buflen: PtrInt);
begin
  Reader.Init(buf, buflen);
  if Reader.EOF then
    Reader.ErrorOverflow;
end;

function TAsn1Parser.NextTag(iskind: TAsn1TagKind; isclass: TAsn1TagClass): boolean;
begin
  result := false;
  if Reader.Eof then
    exit;
  Tag.Parse(Reader);
  result := (Tag.TagClass = isclass) and
            (Tag.TagKind = iskind);
end;

function TAsn1Parser.Init(const buf: RawByteString;
  const kinds: array of TAsn1TagKind): boolean;
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

function TAsn1Parser.PeekTag(isclass: TAsn1TagClass): TAsn1TagKind;
var
  v: cardinal;
begin
  if Reader.EOF then
    Reader.ErrorOverflow;
  v := ord(Reader.P^);
  if TAsn1TagClass(v shr 6) = isClass then
    result := TAsn1TagKind(v and ord(atLongForm))
  else
    result := atUnknown;
end;

function TAsn1Parser.NextOid: TAsn1Oid;
begin
  Tag.Parse(Reader);
  result := Tag.ToOid;
end;

procedure TAsn1Parser.NextNull;
begin
  Tag.Parse(Reader);
  if (Tag.TagClass <> acUniversal) or
     (Tag.TagKind <> atNull) then
    raise EAsn1.Create('NextNull');
end;

function TAsn1Parser.NextBoolean: boolean;
begin
  Tag.Parse(Reader);
  result := Tag.ToBoolean;
end;

function TAsn1Parser.NextUInt32: cardinal;
begin
  Tag.Parse(Reader);
  result := Tag.ToUInt32;
end;

function TAsn1Parser.NextInteger: TBytes;
begin
  Tag.Parse(Reader);
  result := Tag.ToInteger;
end;

function TAsn1Parser.NextInteger(out value: TBytes): boolean;
begin
  result := NextTag(atInteger);
  if result then
    value := Tag.ToInteger;
end;

function TAsn1Parser.NextBigInt(out value: TBytes): boolean;
begin
  result := NextTag(atBitString);
  if result then
    value := Tag.ToInteger;
end;

function TAsn1Parser.NextUtf8: RawUtf8;
begin
  Tag.Parse(Reader);
  result := Tag.ToUtf8;
end;

function TAsn1Parser.NextBuffer: RawByteString;
begin
  Tag.Parse(Reader);
  result := Tag.ToBuffer;
end;


function Asn1Parse(buf: pointer; buflen: PtrInt): TAsn1Tags;
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

procedure FillZero(var Priv: TX509PrivateKey);
begin
  FillZero(Priv.Value);
end;

procedure FillZero(var Priv: TX509RsaPrivateKey);
begin
  FillZero(Priv.PrivateExponent);
  FillZero(Priv.Prime1);
  FillZero(Priv.Prime2);
  FillZero(Priv.Exponent1);
  FillZero(Priv.Exponent2);
  FillZero(Priv.Coefficient);
end;

function DerImport(var Parser: TAsn1Parser;
  out Pub: TX509RsaPublicKey): boolean;
begin
  result := false;
  if not Parser.NextTag(atSequence) or
     not Parser.NextTag(atInteger) then
    exit;
  Pub.Modulus := Parser.Tag.ToInteger;
  result := Parser.NextInteger(Pub.PublicExponent);
end;

function DerImport(var Parser: TAsn1Parser;
  out Priv: TX509RsaPrivateKey): boolean;
begin
  result := false;
  if not Parser.NextTag(atSequence) or
     not Parser.NextTag(atInteger) then
    exit;
  Priv.Version := Parser.Tag.ToUInt32;
  result := (Priv.Version = 0) and
            Parser.NextInteger(Priv.Modulus) and
            Parser.NextInteger(Priv.PublicExponent) and
            Parser.NextInteger(Priv.PrivateExponent) and
            Parser.NextInteger(Priv.Prime1) and
            Parser.NextInteger(Priv.Prime2) and
            Parser.NextInteger(Priv.Exponent1) and
            Parser.NextInteger(Priv.Exponent2) and
            Parser.NextInteger(Priv.Coefficient);
end;


{ **************** X.509 Certificates }


procedure InitializeUnit;
begin
  assert(ord(atLongForm) = $1f);
end;


initialization
  InitializeUnit;
  
end.
