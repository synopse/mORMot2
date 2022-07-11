/// Framework Core Cryptographic Process using OpenSSL
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.openssl;

{
  *****************************************************************************

   High-Performance Cryptographic Features using OpenSSL 1.1.1
    - OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG)
    - AES Cypher/Uncypher in various Modes
    - Hashers and Signers OpenSSL Wrappers
    - OpenSSL Asymmetric Cryptography
    - JWT Implementation using any OpenSSL Algorithm
    - Register OpenSSL to our General Cryptography Catalog

  *****************************************************************************

  TL;DR: On x86_64, our mormot.crypt.core.pas asm is stand-alone and faster
         than OpenSSL for most algorithms, and only 20% slower for AES-GCM.
         For ECC, our mormot.crypt.ecc256r1 is noticeably slower than OpenSSL.

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

interface

{$I ..\mormot.defines.inc}

{$ifdef USE_OPENSSL}

// compile as a void unit if USE_OPENSSL is not defined

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.crypt.core,
  mormot.crypt.ecc256r1,
  mormot.crypt.ecc,
  mormot.crypt.secure,
  mormot.crypt.jwt,
  mormot.lib.openssl11;


{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

type
  /// exception class raised by the AES classes of this unit
  EOpenSslCrypto = class(EOpenSsl);

  /// TAesPrng-compatible class using OpenSSL 1.1.1
  // - we abbreviate OpenSsl as Osl in class names for brevity
  // - may be used instead of TAesPrng if a "proven" generator is required -
  // you could override MainAesPrng global variable
  // - but mormot.crypt.core TAesPrng is faster, especially for small output,
  // and use a similar and proven 256-bit AES-CTR source of randomness:
  // $  OpenSSL Random32 in 288.71ms i.e. 346,363/s, aver. 2us, 1.3 MB/s
  // $  mORMot Random32 in 3.95ms i.e. 25,303,643/s, aver. 0us, 96.5 MB/s
  // $  OpenSSL FillRandom in 240us, 397.3 MB/s
  // $  mORMot FillRandom in 46us, 2 GB/s
  TAesPrngOsl = class(TAesPrngAbstract)
  public
    /// initialize the CSPRNG using OpenSSL 1.1.1
    // - if the library is not available, will raise an Exception
    constructor Create; override;
    /// wrapper around function OpenSslIsAvailable
    class function IsAvailable: boolean; override;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    // - is just a wrapper around RAND_bytes() API call
    procedure FillRandom(Buffer: pointer; Len: PtrInt); override;
    /// returns the single system-wide instance of TAesPrngOsl
    // - if you need to generate some random content, just call the
    // TAesPrngOsl.Main.FillRandom() overloaded methods, or directly
    // TAesPrngOsl.Fill() class methods
    class function Main: TAesPrngAbstract; override;
  end;


{ ************** AES Cypher/Uncypher in various Modes }

type
  /// reusable wrapper around OpenSSL Cipher process
  TAesOsl = object
  public
    Owner: TAesAbstract;
    Cipher: PEVP_CIPHER; // computed from TAesAbstractOsl.AlgoName
    Ctx: array[boolean] of PEVP_CIPHER_CTX; // set and reused in CallEvp()
    procedure Init(aOwner: TAesAbstract; aCipherName: PUtf8Char);
    procedure Done;
    procedure SetEvp(DoEncrypt: boolean; const method: shortstring);
    procedure UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
    procedure Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
  end;

  /// handle AES cypher/uncypher with chaining with OpenSSL 1.1
  // - we abbreviate OpenSsl as Osl in class names for brevity
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAesEcbOsl, TAesCbcOsl, TAesCfbOsl, TAesOfbOsl and TAesCtrOsl
  // classes to handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7-like padding)
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAesAbstractOsl = class(TAesAbstract)
  protected
    fAes: TAesOsl;
    procedure AfterCreate; override; // circumvent Delphi bug about const aKey
  public
    /// creates a new instance with the very same values
    // - directly copy the existing OpenSSL context for efficiency
    function Clone: TAesAbstract; override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    function CloneEncryptDecrypt: TAesAbstract; override;
    /// release the used instance memory and resources
    destructor Destroy; override;
    /// wrapper around function OpenSslIsAvailable
    // - actual cipher won't be checked until Create() since we need the keysize
    class function IsAvailable: boolean; override;
    /// perform the AES cypher in the corresponding mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the corresponding mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// OpenSSL AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  TAesEcbOsl = class(TAesAbstractOsl)
  protected
    procedure AfterCreate; override;
  end;

  /// OpenSSL AES cypher/uncypher with Cipher-block chaining (CBC)
  TAesCbcOsl = class(TAesAbstractOsl)
  protected
    procedure AfterCreate; override;
  end;

  /// OpenSSL AES cypher/uncypher with Cipher feedback (CFB)
  // - our TAesCfb class is faster than OpenSSL on x86_64:
  // $  mormot aes-128-cfb in 6.95ms i.e. 359247/s or 764.5 MB/s
  // $  mormot aes-256-cfb in 9.40ms i.e. 265816/s or 565.7 MB/s
  // $  openssl aes-128-cfb in 10.53ms i.e. 237326/s or 505 MB/s
  // $  openssl aes-256-cfb in 13.18ms i.e. 189652/s or 403.6 MB/s
  TAesCfbOsl = class(TAesAbstractOsl)
  protected
    procedure AfterCreate; override;
  end;

  /// OpenSSL AES cypher/uncypher with Output feedback (OFB)
  // - our TAesOfb class is faster than OpenSSL on x86_64:
  // $  mormot aes-128-ofb in 6.88ms i.e. 363002/s or 772.5 MB/s
  // $  mormot aes-256-ofb in 9.37ms i.e. 266808/s or 567.8 MB/s
  // $  openssl aes-128-ofb in 7.82ms i.e. 319693/s or 680.3 MB/s
  // $  openssl aes-256-ofb in 10.39ms i.e. 240523/s or 511.8 MB/s
  TAesOfbOsl = class(TAesAbstractOsl)
  protected
    procedure AfterCreate; override;
  end;

  /// OpenSSL AES cypher/uncypher with 128-bit Counter mode (CTR)
  // - similar to TAesCtrNist, not our proprietary TAesCtrAny with a 64-bit CTR
  // - our TAesCtrNist class is faster than OpenSSL on x86_64:
  // $  mormot aes-128-ctr in 1.99ms i.e. 1254390/s or 2.6 GB/s
  // $  mormot aes-256-ctr in 2.64ms i.e. 945179/s or 1.9 GB/s
  // $  openssl aes-128-ctr in 2.23ms i.e. 1121076/s or 2.3 GB/s
  // $  openssl aes-256-ctr in 2.80ms i.e. 891901/s or 1.8 GB/s
  TAesCtrOsl = class(TAesAbstractOsl)
  protected
    procedure AfterCreate; override;
  end;

  /// OpenSSL AES-GCM cypher/uncypher
  // - implements AEAD (authenticated-encryption with associated-data) process
  // via MacSetNonce/MacEncrypt or AesGcmAad/AesGcmFinal methods
  // - OpenSSL is faster than our TAesGcm class, but not so much:
  // $  openssl aes-128-gcm in 2.86ms i.e. 874125/s or 1.8 GB/s
  // $  openssl aes-256-gcm in 3.43ms i.e. 727590/s or 1.5 GB/s
  // $  mormot aes-128-gcm in 3.45ms i.e. 722752/s or 1.5 GB/s
  // $  mormot aes-256-gcm in 4.11ms i.e. 607385/s or 1.2 GB/s
  // - WARNING: AEAD associated information is currently unsupported by
  // TAesGcmOsl, due to some obscure OpenSSL padding issue in our code - GMAC
  // will be properly handled by TAesGcmOsl, but you should use plain TAesGcm
  // instead if you really need to authenticate some associated information
  TAesGcmOsl = class(TAesGcmAbstract)
  protected
    fAes: TAesOsl;
    function AesGcmInit: boolean; override; // from fKey/fKeySize
    procedure AesGcmDone; override;
    procedure AesGcmReset; override; // from fIV/CTR_POS
    function AesGcmProcess(BufIn, BufOut: pointer; Count: cardinal): boolean; override;
  public
    /// creates a new instance with the very same values
    // - by design, our classes will use TAesGcmEngine stateless context, so
    // this method will just copy the current fields to a new instance,
    // by-passing the key creation step
    function Clone: TAesAbstract; override;
    /// compute a class instance similar to this one, for performing the
    // reverse encryption/decryption process
    // - will return self to avoid creating two instances
    function CloneEncryptDecrypt: TAesAbstract; override;
    /// AES-GCM pure alternative to MacSetNonce() - unsupported
    // - set the IV as usual (only the first 12 bytes will be used for GCM),
    // then optionally append any AEAD data with this method; warning: you need
    // to call Encrypt() once before - perhaps as Encrypt(nil, nil, 0)
    // - WARNING: AEAD associated information is currently unsupported by
    // TAesGcmOsl, due to some obscure OpenSSL padding issue in our code - GMAC
    // will be properly handled by TAesGcmOsl, but you should use plain TAesGcm
    // instead if you really need to authenticate some associated information
    procedure AesGcmAad(Buf: pointer; Len: integer); override;
    /// AES-GCM pure alternative to MacEncryptGetTag/MacDecryptCheckTag
    // - after Encrypt, fill tag with the GCM value of the data and return true
    // - after Decrypt, return true only if the GCM value of the data match tag
    function AesGcmFinal(var tag: TAesBlock): boolean; override;
  end;


{ ************** Hashers and Signers OpenSSL Wrappers }

type
  /// exception class raised by the hashing classes of this unit
  EOpenSslHash = class(EOpenSsl);

  /// abstract parent multi-algorithm hashing/HMAC wrapper for OpenSSL
  TOpenSslDigestAbstract = class
  private
    fDigestSize: cardinal;
    fDigestValue: THash512Rec;
  public
    /// wrapper around function OpenSslIsAvailable
    class function IsAvailable: boolean;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Digest to retrieve the HMAC
    procedure Update(Data: pointer; DataLength: integer); overload; virtual; abstract;
    /// call this method for each continuous message block
    procedure Update(const Data: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the hash of all supplied messages
    // - store it internally the DigestValue buffer, or copy it into Dest buffer
    // which is typically a THash256 or THash512 variable
    // - returns actual DigestValue/Dest buffer length, i.e. DigestSize
    function Digest(Dest: pointer = nil): cardinal; virtual; abstract;
    /// computes the Hash/HMAC of all supplied message as lowercase hexa chars
    function DigestHex: RawUtf8;
    /// raw access to the final Message Digest, after a call to the Digest method
    // - if HashSize as supplied to Create() is bigger than 64, first bytes will
    // be truncated in this field - you should specify a buffer to Digest() call
    property DigestValue: THash512Rec
      read fDigestValue;
    /// how many bytes have been stored in DigestValues, after a call to Digest
    property DigestSize: cardinal
      read fDigestSize;
  end;

  /// convenient multi-algorithm wrapper for OpenSSL Hash algorithms
  TOpenSslHash = class(TOpenSslDigestAbstract)
  private
    fCtx: PEVP_MD_CTX;
    fXof: boolean;
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - Algorithm is one of `openssl list -digest-algorithms`
    // - if Algorithm is not specified, EVP_sha256 will be used
    // - for XOF hash functions such as 'shake256', the hashSize option
    // can be used to specify the desired output length in bytes
    // - raise an EOpenSslHash exception on unknown/unsupported algorithm
    constructor Create(const Algorithm: RawUtf8; HashSize: cardinal = 0);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Digest to retrieve the Hash
    procedure Update(Data: pointer; DataLength: integer); override;
    /// computes the hash of all supplied messages
    // - store it internally the DigestValue buffer, or copy it into Dest buffer
    // which is typically a THash256 or THash512 variable
    // - returns actual DigestValue/Dest buffer length, i.e. DigestSize
    function Digest(Dest: pointer = nil): cardinal; override;
    /// compute the message authentication code using `Algorithm` as hash function
    class function Hash(const Algorithm: RawUtf8; const Data: RawByteString;
      HashSize: cardinal = 0): RawUtf8;
    /// release the digest context
    destructor Destroy; override;
  end;

  /// convenient multi-algorithm wrapper for OpenSSL HMAC algorithms
  TOpenSslHmac = class(TOpenSslDigestAbstract)
  private
    fCtx: PHMAC_CTX;
  public
    /// initialize the internal HMAC structure for a specific Algorithm and Key
    // - Algorithm is one of `openssl list -digest-algorithms`
    // - if Algorithm is not specified, EVP_sha256 will be used
    // - Key/KeyLen define the HMAC associated salt
    // - raise an EOpenSslHash exception on unknown/unsupported algorithm
    constructor Create(const Algorithm: RawUtf8;
      Key: pointer; KeyLength: cardinal); overload;
    /// initialize the internal HMAC structure for a specific Algorithm and Key
    constructor Create(const Algorithm: RawUtf8;
      const Key: RawByteString); overload;
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call Digest to retrieve the Hash
    procedure Update(Data: pointer; DataLength: integer); override;
    /// computes the HMAC of all supplied messages
    // - store it internally the DigestValue buffer, or copy it into Dest buffer
    // which is typically a THash256 or THash512 variable
    // - returns actual DigestValue/Dest buffer length, i.e. DigestSize
    function Digest(Dest: pointer = nil): cardinal; override;
    /// compute the HMAC using `algorithm` as hash function
    class function Hmac(const Algorithm: RawUtf8; const Data: RawByteString;
      Key: pointer; KeyLength: cardinal): RawUtf8; overload;
    /// compute the HMAC using `algorithm` as hash function
    class function Hmac(const Algorithm: RawUtf8;
      const Data, Key: RawByteString): RawUtf8; overload;
    /// release the digest context
    destructor Destroy; override;
  end;

/// retrieve a low-level PEVP_MD digest from its algorithm name
// - raise an EOpenSslHash if this algorithm is not found
function OpenSslGetMd(const Algorithm: RawUtf8; const Caller: shortstring): PEVP_MD; overload;

/// retrieve a low-level PEVP_MD digest from mORMot THashAlgo algorithm enum
// - returns nil if not found, e.g. if OpenSsl is not available
function OpenSslGetMd(Algorithm: THashAlgo): PEVP_MD; overload;



{ ************** OpenSSL Asymmetric Cryptography }

type
  /// exception class raised by the sign/verify classes of this unit
  EOpenSslAsymmetric = class(EOpenSsl);

/// asymmetric digital signature of some Message using a given PrivateKey
// - if Algorithm is '', EVP_sha256 will be used as message Digest
// - if Algorithm is 'null', no Digest is done before signature - which is
// mandatory e.g. for ed25519 which uses internally SHA-512
// - returns 0 on error, or the result Signature size in bytes
function OpenSslSign(const Algorithm: RawUtf8;
  Message, PrivateKey: pointer; MessageLen, PrivateKeyLen: integer;
  out Signature: RawByteString; const PrivateKeyPassword: RawUtf8 = '';
  const Engine: RawUtf8 = ''): cardinal;

/// asymmetric digital verification of some Message using a given PublicKey
// - if Algorithm is '', EVP_sha256 will be used as message Digest
// - if Algorithm is 'null', no Digest is done before signature - which is
// mandatory e.g. for ed25519 which uses internally SHA-512
// - returns false on error, or true if the Message has been authenticated
function OpenSslVerify(const Algorithm, PublicKeyPassword: RawUtf8;
  Message, PublicKey, Signature: pointer;
  MessageLen, PublicKeyLen, SignatureLen: integer;
  const Engine: RawUtf8 = ''): boolean;

/// generate a public/private pair of keys in raw OpenSSL format
// - if EvpType is EVP_PKEY_DSA, EVP_PKEY_DH or EVP_PKEY_RSA or EVP_PKEY_RSA_PSS,
// BitsOrCurve is the number of bits of the key
// - if EvpType is EVP_PKEY_EC, BitsOrCurve is the curve NID (e.g.
// NID_X9_62_prime256v1)
// - if EvpType is EVP_PKEY_ED25519, BitsOrCurve is ignored
// - caller should result.Free once done with the private key returned instance
function OpenSslGenerateKeys(EvpType, BitsOrCurve: integer): PEVP_PKEY; overload;

/// generate a public/private pair of keys in PEM text format
// - if EvpType is EVP_PKEY_DSA, EVP_PKEY_DH or EVP_PKEY_RSA or EVP_PKEY_RSA_PSS,
// BitsOrCurve is the number of bits of the key
// - if EvpType is EVP_PKEY_EC, BitsOrCurve is the Elliptic curve NID (e.g.
// NID_X9_62_prime256v1)
// - if EvpType is EVP_PKEY_ED25519, BitsOrCurve is ignored
procedure OpenSslGenerateKeys(EvpType, BitsOrCurve: integer;
  out PrivateKey, PublicKey: RawUtf8); overload;

/// generate a public/private pair of keys in raw DER binary format
// - if EvpType is EVP_PKEY_DSA, EVP_PKEY_DH or EVP_PKEY_RSA or EVP_PKEY_RSA_PSS,
// BitsOrCurve is the number of bits of the key
// - if EvpType is EVP_PKEY_EC, BitsOrCurve is the Elliptic curve NID (e.g.
// NID_X9_62_prime256v1)
// - if EvpType is EVP_PKEY_ED25519, BitsOrCurve is ignored
procedure OpenSslGenerateBinaryKeys(EvpType, BitsOrCurve: integer;
  out PrivateKey, PublicKey: RawByteString);

{
/// compute the (e.g. ECDH) shared secret from a public/private keys inverted pair
function OpenSslSharedSecret(EvpType, BitsOrCurve: integer;
  const PublicKey, PrivateKey: RawUtf8; const PrivateKeyPassword: RawUtf8 = ''): RawByteString;
}

/// mormot.crypt.ecc256r1 compatible function for asymmetric key generation
// - this OpenSSL-powered function will replace our slower mormot.crypt.ecc256r1
// $ OpenSSL: 300 Ecc256r1MakeKey in 7.75ms i.e. 38,664/s, aver. 25us
// $ mORMot:  300 Ecc256r1MakeKey in 255ms i.e. 1,176/s, aver. 850us
// - directly access OpenSSL prime256v1, so faster than OpenSslGenerateKeys()
function ecc_make_key_osl(out PublicKey: TEccPublicKey;
  out PrivateKey: TEccPrivateKey): boolean;

/// mormot.crypt.ecc256r1 compatible function for asymmetric key signature
// - this OpenSSL-powered function will replace our slower pascal/c code
// $ OpenSSL: 300 Ecc256r1Sign in 11.66ms i.e. 25,711/s, aver. 38us
// $ mORMot:  300 Ecc256r1Sign in 262.72ms i.e. 1,141/s, aver. 875us
// - directly access OpenSSL prime256v1, so faster than OpenSslSign()
function ecdsa_sign_osl(const PrivateKey: TEccPrivateKey; const Hash: TEccHash;
  out Signature: TEccSignature): boolean;

/// mormot.crypt.ecc256r1 compatible function for asymmetric key verification
// - this OpenSSL-powered function will replace our slower pascal/c code
// $ OpenSSL: 300 Ecc256r1Verify in 41.32ms i.e. 7,260/s, aver. 137us
// $ mORMot:  300 Ecc256r1Verify in 319.32ms i.e. 939/s, aver. 1.06ms
// - directly access OpenSSL prime256v1, so faster than OpenSslVerify()
function ecdsa_verify_osl(const PublicKey: TEccPublicKey; const Hash: TEccHash;
  const Signature: TEccSignature): boolean;

/// mormot.crypt.ecc256r1 compatible function for ECDH shared secret computation
// - this OpenSSL-powered function will replace our slower pascal/c code
// $ OpenSSL: 598 Ecc256r1SharedSecret in 67.98ms i.e. 8,796/s, aver. 113us
// $ mORMot:  598 Ecc256r1SharedSecret in 537.95ms i.e. 1,111/s, aver. 899us
function ecdh_shared_secret_osl(const PublicKey: TEccPublicKey;
  const PrivateKey: TEccPrivateKey; out Secret: TEccSecretKey): boolean;

type
  /// OpenSSL verification of a ECDSA signature using ECC secp256r1 cryptography
  // - assigned to TEcc256r1Verify global meta-class at startup
  TEcc256r1VerifyOsl = class(TEcc256r1VerifyAbstract)
  protected
    fKey: PEC_KEY;
    fPoint: PEC_POINT;
  public
    /// initialize the verifier with a given ECC compressed public key
    constructor Create(const pub: TEccPublicKey); override;
    /// finalize this instance
    destructor Destroy; override;
    /// validate a signature against a hash using ECC
    function Verify(const hash: TEccHash; const sign: TEccSignature): boolean;
      override;
  end;

const
  CAA_HASH: array[TCryptAsymAlgo] of RawUtf8 = (
    '',       // caaES256 will recognize '' as SHA-256 hash
    'SHA384', // caaES384
    'SHA512', // caaES512
    '',       // caaES256K
    '',       // caaRS256
    'SHA384', // caaRS384
    'SHA512', // caaRS512
    '',       // caaPS256
    'SHA384', // caaPS384
    'SHA512', // caaPS512
    'null');  // caaEdDSA Ed25519 includes its own SHA-512

  CAA_EVPTYPE: array[TCryptAsymAlgo] of integer = (
    EVP_PKEY_EC,          // caaES256
    EVP_PKEY_EC,          // caaES384
    EVP_PKEY_EC,          // caaES512
    EVP_PKEY_EC,          // caaES256K
    EVP_PKEY_RSA,         // caaRS256
    EVP_PKEY_RSA,         // caaRS384
    EVP_PKEY_RSA,         // caaRS512
    EVP_PKEY_RSA_PSS,     // caaPS256
    EVP_PKEY_RSA_PSS,     // caaPS384
    EVP_PKEY_RSA_PSS,     // caaPS512
    EVP_PKEY_ED25519);    // caaEdDSA

  CAA_BITSORCURVE: array[TCryptAsymAlgo] of integer = (
    NID_X9_62_prime256v1, // caaES256
    NID_secp384r1,        // caaES384
    NID_secp521r1,        // caaES512
    NID_secp256k1,        // caaES256K
    2048,                 // caaRS256
    2048,                 // caaRS384
    2048,                 // caaRS512
    2048,                 // caaPS256
    2048,                 // caaPS384
    2048,                 // caaPS512
    0);                   // caaEdDSA


{ ************** JWT Implementation using any OpenSSL Algorithm }

type
  /// implements JSON Web Tokens using OpenSSL Algorithms
  TJwtOpenSsl = class(TJwtAbstract)
  protected
    fPrivateKey, fPublicKey: RawByteString;
    fPrivateKeyPassword, fPublicKeyPassword: RawUtf8;
    fHashAlgorithm: RawUtf8;
    fGenEvpType: integer;
    fGenBitsOrCurve: integer;
    fAlgoMd: PEVP_MD;
    fPrivKey, fPubKey: PEVP_PKEY;
    function ComputeSignature(const headpayload: RawUtf8): RawUtf8; override;
    procedure CheckSignature(const headpayload: RawUtf8; const signature: RawByteString;
      var JWT: TJwtContent); override;
  public
    /// initialize the JWT processing instance using any supported OpenSSL algorithm
    constructor Create(const aJwtAlgorithm, aHashAlgorithm: RawUtf8;
      aGenEvpType, aGenBitsOrCurve: integer;
      const aPrivateKey, aPublicKey: RawByteString;
      const aPrivateKeyPassword, aPublicKeyPassword: RawUtf8;
      aClaims: TJwtClaims; const aAudience: array of RawUtf8;
      aExpirationMinutes: integer = 0; aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUtf8 = ''; aIDObfuscationKeyNewKdf: integer = 0);
      reintroduce;
    /// finalize the instance
    destructor Destroy; override;
    /// wrapper around function OpenSslIsAvailable
    class function IsAvailable: boolean;
    /// the OpenSSL hash algorithm, as supplied to the constructor
    property HashAlgorithm: RawUtf8
      read fHashAlgorithm;
  end;

  /// abstract parent for OpenSSL JWT algorithms - never use this plain class!
  // - inherited classes implement all official algorithms from https://jwt.io
  // - we abbreviate OpenSsl as Osl in class names for brevity
  // - some numbers from our regresssion tests on Linux x86_64 for JWT validation:
  // $ 100 RS256 in 2.03ms i.e. 47.8K/s, aver. 20us
  // $ 100 RS384 in 1.99ms i.e. 48.9K/s, aver. 19us
  // $ 100 RS512 in 1.99ms i.e. 48.9K/s, aver. 19us
  // $ 100 PS256 in 2.30ms i.e. 42.3K/s, aver. 23us
  // $ 100 PS384 in 2.26ms i.e. 43.1K/s, aver. 22us
  // $ 100 PS512 in 2.75ms i.e. 35.4K/s, aver. 27us
  // $ 100 ES256 in 8.64ms i.e. 11.3K/s, aver. 86us
  // $ 100 ES384 in 81.43ms i.e. 1.1K/s, aver. 814us
  // $ 100 ES512 in 59.81ms i.e. 1.6K/s, aver. 598us
  // $ 100 ES256K in 40.43ms i.e. 2.4K/s, aver. 404us
  // $ 100 EdDSA in 11.55ms i.e. 8.4K/s, aver. 115us
  TJwtAbstractOsl = class(TJwtOpenSsl)
  protected
    fAsym: TCryptAsymAlgo;
    procedure SetAlgorithms; virtual; // set fHashAlgo+fHashAlgorithm
    procedure SetAlgorithm; virtual; abstract; // set fAsym
  public
    /// initialize the JWT processing instance calling SetAlgorithms abstract method
    // - the supplied key(s) could be in PEM or raw DER binary format
    // - the supplied set of claims are expected to be defined in the JWT payload
    // - aAudience are the allowed values for the jrcAudience claim
    // - aExpirationMinutes is the deprecation time for the jrcExpirationTime claim
    // - aIDIdentifier and aIDObfuscationKey/aIDObfuscationKeyNewKdf are passed
    // to a TSynUniqueIdentifierGenerator instance used for jrcJwtID claim
    constructor Create(
      const aPrivateKey, aPublicKey: RawByteString;
      const aPrivateKeyPassword, aPublicKeyPassword: RawUtf8;
      aClaims: TJwtClaims; const aAudience: array of RawUtf8;
      aExpirationMinutes: integer = 0; aIDIdentifier: TSynUniqueIdentifierProcess = 0;
      aIDObfuscationKey: RawUtf8 = ''; aIDObfuscationKeyNewKdf: integer = 0);
      reintroduce;
    /// generate a private/public keys pair for this algorithm in PEM text format
    class procedure GenerateKeys(out PrivateKey, PublicKey: RawUtf8);
    /// generate a private/public keys pair for this algorithm in raw DER format
    class procedure GenerateBinaryKeys(out PrivateKey, PublicKey: RawByteString);
  end;

  /// meta-class of all OpenSSL JWT algorithms
  TJwtAbstractOslClass = class of TJwtAbstractOsl;

  /// implements 'ES256' secp256r1 ECC algorithm over SHA-256 using OpenSSL
  // - note that our TJwtES256 class is slightly faster on Linux x86_64:
  // $ TJwtES256 pascal:   100 ES256 in 33.57ms i.e. 2.9K/s, aver. 335us
  // $ TJwtES256 OpenSSL:  100 ES256 in 6.90ms i.e. 14.1K/s, aver. 69us
  // $ TJwtES256Osl:       100 ES256 in 8.64ms i.e. 11.3K/s, aver. 86us
  TJwtES256Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'ES384' secp384r1 ECC algorithm over SHA-384 using OpenSSL
  TJwtES384Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'ES512' ecp521r1 ECC algorithm over SHA-512 using OpenSSL
  TJwtES512Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'ES256K' secp256k1 ECC algorithm using OpenSSL
  TJwtES256KOsl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'RS256' RSA 2048-bit algorithm over SHA-256 using OpenSSL
  TJwtRS256Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'RS384' RSA 2048-bit algorithm over SHA-384 using OpenSSL
  TJwtRS384Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'RS512' RSA 2048-bit algorithm over SHA-512 using OpenSSL
  TJwtRS512Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'PS256' RSA-PSS 2048-bit algorithm over SHA-256 using OpenSSL
  TJwtPS256Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'PS384' RSA-PSS 2048-bit algorithm over SHA-384 using OpenSSL
  TJwtPS384Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'PS512' RSA-PSS 2048-bit algorithm over SHA-512 using OpenSSL
  TJwtPS512Osl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;

  /// implements 'EdDSA' Ed25519 algorithm using OpenSSL
  TJwtEdDSAOsl = class(TJwtAbstractOsl)
  protected
    procedure SetAlgorithm; override;
  end;


{ ************** Register OpenSSL to our General Cryptography Catalog }

/// call once at program startup to use OpenSSL when its performance matters
// - redirects TAesGcmFast (and TAesCtrFast on i386) globals to OpenSSL
// - redirects raw mormot.crypt.ecc256r1 functions to use OpenSSL which is much
// faster than our stand-alone C/pascal version
// - register OpenSSL for our Asym() high-level factory (via an hidden
// TCryptAsymOsl class)
procedure RegisterOpenSsl;


implementation


{ TAesOsl }

procedure TAesOsl.Init(aOwner: TAesAbstract; aCipherName: PUtf8Char);
begin
  Owner := aOwner;
  EOpenSslCrypto.CheckAvailable(PClass(Owner)^, 'Create');
  Cipher := EVP_get_cipherbyname(aCipherName);
  if Cipher = nil then
    raise EOpenSslCrypto.CreateFmt('%s.Create: unknown ''%s'' cipher',
      [ClassNameShort(Owner)^, aCipherName]);
end;

procedure TAesOsl.Done;
begin
  if Ctx[false] <> nil then
    EVP_CIPHER_CTX_free(Ctx[false]);
  if Ctx[true] <> nil then
    EVP_CIPHER_CTX_free(Ctx[true]);
end;

procedure TAesOsl.SetEvp(DoEncrypt: boolean; const method: shortstring);
var
  c: PEVP_CIPHER_CTX;
begin
  c := Ctx[DoEncrypt];
  if (c <> nil) and
     (Owner.AlgoMode = mGcm) and
     (OpenSslVersion >= $30000000) then
    // on OpenSSL 3, GCM requires a full reinitialization of the context :(
    EVP_CIPHER_CTX_reset(c)
  else if c = nil then
  begin
    // allocate new encrypt/decrypt context
    c := EVP_CIPHER_CTX_new;
    Ctx[DoEncrypt] := c;
  end
  else
  begin
    // OpenSSL allows to reuse the previous Ctxt[], just setting the (new) IV
    // -> this makes a huge performance benefit
    // note: the latest API (i.e. EVP_CipherInit_ex on 1.1.1, EVP_CipherInit_ex2
    // on 3.0) should be called to be able to reuse the context
    EOpenSslCrypto.Check(Owner, method, EVP_CipherInit_ex2(
      c, nil, nil, @Owner.IV, ord(DoEncrypt), nil));
    exit;
  end;
  // full initialization of the context, with the proper key/IV and no padding
  EOpenSslCrypto.Check(Owner, method,
    EVP_CipherInit_ex2(
      c, Cipher, @TAesAbstractOsl(Owner).fKey, @Owner.IV, ord(DoEncrypt), nil));
  EOpenSslCrypto.Check(Owner, method,
    EVP_CIPHER_CTX_set_padding(c, 0));
end;

procedure TAesOsl.UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
var
  outl: integer;
var
  c: PEVP_CIPHER_CTX;
begin
  if (BufOut <> nil) and
     (Count and AesBlockMod <> 0) then
    raise ESynCrypto.CreateUtf8('%.%: Count=% is not a multiple of 16',
      [Owner, 'UpdEvp', Count]);
  outl := 0;
  c := Ctx[DoEncrypt];
  EOpenSslCrypto.Check(Owner, 'UpdEvp',
    EVP_CipherUpdate(c, BufOut, @outl, BufIn, Count));
  Owner.IV := PAesBlock(EVP_CIPHER_CTX_iv(c))^; // for fIVUpdated := true
  // no need to call EVP_CipherFinal_ex() since we expect no padding
end;

procedure TAesOsl.Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
var
  enc: boolean;
  s, d: TAesAbstractOsl;
begin
  s := TAesAbstractOsl(Owner);
  d := TAesAbstractOsl(ToOwner);
  d.fKeySize := s.fKeySize;
  d.fKeySizeBytes := s.fKeySizeBytes;
  d.fAlgoMode := s.fAlgoMode;
  d.fKey := s.fKey;
  ToAesOsl.Owner := ToOwner;
  ToAesOsl.Cipher := Cipher;
  for enc := false to true do
    if Ctx[enc] <> nil then
    begin
      // efficient Ctx[] copy
      ToAesOsl.Ctx[enc] := EVP_CIPHER_CTX_new;
      EVP_CIPHER_CTX_copy(ToAesOsl.Ctx[enc], Ctx[enc]);
    end;
end;



{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

{ TAesPrngOsl }

class function TAesPrngOsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

constructor TAesPrngOsl.Create;
begin
  inherited Create;
  EOpenSslCrypto.CheckAvailable(PClass(self)^, 'Create');
end;

procedure TAesPrngOsl.FillRandom(Buffer: pointer; Len: PtrInt);
begin
  inc(fTotalBytes, Len);
  RAND_bytes(Buffer, Len);
end;

var
  MainAesPrngOsl: TAesPrngOsl;

class function TAesPrngOsl.Main: TAesPrngAbstract;
begin
  result := MainAesPrngOsl;
  if result <> nil then
    exit;
  EOpenSslCrypto.CheckAvailable(self, 'Main');
  GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
  try
    if MainAesPrngOsl = nil then
      MainAesPrngOsl := RegisterGlobalShutdownRelease(TAesPrngOsl.Create);
  finally
    GlobalUnLock;
  end;
  result := MainAesPrngOsl;
end;


{ ************** AES Cypher/Uncypher in various Modes }

{ TAesAbstractOsl }

procedure TAesAbstractOsl.AfterCreate;
var
  nam: TShort16;
begin
  AlgoName(nam); // always #0 terminated
  fAes.Init(self, pointer(@nam[1]));
  fIVUpdated := true; // fAes.UpdEvp() calls EVP_CIPHER_CTX_iv() to set IV
end;

destructor TAesAbstractOsl.Destroy;
begin
  fAes.Done;
  inherited Destroy;
end;

class function TAesAbstractOsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

procedure TAesAbstractOsl.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fAes.SetEvp({doencrypt=}true, 'Encrypt');
  fAes.UpdEvp({doencrypt=}true, BufIn, BufOut, Count);
end;

procedure TAesAbstractOsl.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fAes.SetEvp({doencrypt=}false, 'Decrypt');
  fAes.UpdEvp({doencrypt=}false, BufIn, BufOut, Count);
end;

function TAesAbstractOsl.Clone: TAesAbstract;
begin
  result := TAesAbstractOsl(NewInstance);
  fAes.Clone(result, TAesAbstractOsl(result).fAes); // efficient Ctx[] copy
end;

function TAesAbstractOsl.CloneEncryptDecrypt: TAesAbstract;
begin
  result := self; // there is one Ctx[] for each direction
end;


{ TAesEcbOsl }

procedure TAesEcbOsl.AfterCreate;
begin
  fAlgoMode := mEcb;
  inherited AfterCreate;
end;

{ TAesCbcOsl }

procedure TAesCbcOsl.AfterCreate;
begin
  fAlgoMode := mCbc;
  inherited AfterCreate;
end;

{ TAesCfbOsl }

procedure TAesCfbOsl.AfterCreate;
begin
  fAlgoMode := mCfb;
  inherited AfterCreate;
end;


{ TAesOfbOsl }

procedure TAesOfbOsl.AfterCreate;
begin
  fAlgoMode := mOfb;
  inherited AfterCreate;
end;

{ TAesCtrOsl }

procedure TAesCtrOsl.AfterCreate;
begin
  fAlgoMode := mCtr;
  inherited AfterCreate;
end;

{ TAesGcmOsl }

function TAesGcmOsl.AesGcmInit: boolean;
var
  nam: TShort16;
begin
  AlgoName(nam); // always #0 terminated
  fAes.Init(self, pointer(@nam[1]));
  result := nam[0] <> #0;
end;

procedure TAesGcmOsl.AesGcmDone;
begin
  fAes.Done;
end;

procedure TAesGcmOsl.AesGcmReset;
begin
  fAes.SetEvp(fStarted = stEnc, 'AesGcmProcess');
end;

function TAesGcmOsl.AesGcmProcess(BufIn, BufOut: pointer; Count: cardinal): boolean;
begin
  fAes.UpdEvp(fStarted = stEnc, BufIn, BufOut, Count);
  result := true;
end;

procedure TAesGcmOsl.AesGcmAad(Buf: pointer; Len: integer);
begin
  fAes.UpdEvp({encrypt=}true, Buf, nil, Len);
end;

function TAesGcmOsl.AesGcmFinal(var tag: TAesBlock): boolean;
var
  outl: integer;
  dummy: TAesBlock;
begin
  case fStarted of
    stEnc:
      begin
        EOpenSslCrypto.Check(self, 'AesGcmFinal enc',
          EVP_CipherFinal_ex(fAes.Ctx[true], @dummy, @outl));
        EOpenSslCrypto.Check(self, 'AesGcmFinal enctag',
          EVP_CIPHER_CTX_ctrl(fAes.Ctx[true], EVP_CTRL_GCM_GET_TAG, 16, @tag));
        result := true;
      end;
    stDec:
      begin
        EOpenSslCrypto.Check(self, 'AesGcmFinal dectag',
          EVP_CIPHER_CTX_ctrl(fAes.Ctx[false], EVP_CTRL_GCM_SET_TAG, 16, @tag));
        outl := 16;
        result := (EVP_CipherFinal_ex(fAes.Ctx[false], @dummy, @outl) > 0) and
                  (outl = 0);
      end
  else
    result := false;
  end;
  fStarted := stNone; // allow reuse of this fAes instance
end;

function TAesGcmOsl.Clone: TAesAbstract;
begin
  result := TAesGcmOsl(NewInstance);
  fAes.Clone(result, TAesGcmOsl(result).fAes); // efficient Ctx[] copy
end;

function TAesGcmOsl.CloneEncryptDecrypt: TAesAbstract;
begin
  result := self;
end;


{ ************** Hashers and Signers OpenSSL Wrappers }

{ TOpenSslDigestAbstract }

class function TOpenSslDigestAbstract.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

procedure TOpenSslDigestAbstract.Update(const Data: RawByteString);
begin
  Update(pointer(Data), length(Data));
end;

function TOpenSslDigestAbstract.DigestHex: RawUtf8;
begin
  BinToHexLower(@fDigestValue, Digest(nil), result{%H-});
end;


{ TOpenSslHash }

constructor TOpenSslHash.Create(const Algorithm: RawUtf8; HashSize: cardinal);
var
  md: PEVP_MD;
begin
  EOpenSslHash.CheckAvailable(PClass(self)^, 'Create');
  if Algorithm = '' then
    md := EVP_sha256
  else
    md := EVP_get_digestbyname(pointer(Algorithm));
  if md = nil then
    raise EOpenSslHash.CreateFmt(
      'TOpenSslHash.Create(''%s''): Unknown algorithm', [Algorithm]);
  fCtx := EVP_MD_CTX_new;
  EOpenSslHash.Check(self, 'Create',
    EVP_DigestInit_ex(fCtx, md, nil));
  fDigestSize := EVP_MD_size(md);
  fXof := EVP_MD_flags(md) and EVP_MD_FLAG_XOF <> 0;
  if (hashSize <> 0)  and
     (fDigestSize <> HashSize) then
    if fXof then
      // custom size in XOF mode
      fDigestSize := hashSize
    else
      raise EOpenSslHash.CreateFmt('TOpenSslHash.Create: Incorrect HashSize=' +
        '%d to a non-XOF hash function "%s"', [HashSize, Algorithm]);
end;

procedure TOpenSslHash.Update(Data: pointer; DataLength: integer);
begin
  EOpenSslHash.Check(self, 'Update',
    EVP_DigestUpdate(fCtx, Data, DataLength));
end;

function TOpenSslHash.Digest(Dest: pointer): cardinal;
begin
  result := EVP_MD_CTX_size(fCtx);
  if Dest = nil then
    if result > SizeOf(fDigestValue) then
      raise EOpenSslHash.CreateFmt(
        'TOpenSslHash.Digest(nil): size=%d overflow', [result])
    else
      Dest := @fDigestValue;
  if fXof then
    EOpenSslHash.Check(self, 'DigestXof',
      EVP_DigestFinalXOF(fCtx, Dest, fDigestSize))
  else
    EOpenSslHash.Check(self, 'Digest',
      EVP_DigestFinal_ex(fCtx, Dest, @fDigestSize));
  if Dest <> @fDigestValue then
  begin
    if fDigestSize > SizeOf(fDigestValue) then
      result := SizeOf(fDigestValue) // truncate to local copy in fXof mode
    else
      result := fDigestSize;
    MoveFast(Dest^, fDigestValue, result);
  end;
  result := fDigestSize;
end;

destructor TOpenSslHash.Destroy;
begin
   if fCtx <> nil then
     EVP_MD_CTX_free(fCtx);
  inherited Destroy;
end;

class function TOpenSslHash.Hash(const Algorithm: RawUtf8;
  const Data: RawByteString; HashSize: cardinal): RawUtf8;
begin
  with Create(Algorithm, HashSize) do
    try
      Update(Data);
      result := DigestHex;
    finally
      Free;
    end;
end;


{ TOpenSslHmac }

constructor TOpenSslHmac.Create(const Algorithm: RawUtf8;
  Key: pointer; KeyLength: cardinal);
var
  md: PEVP_MD;
begin
  EOpenSslHash.CheckAvailable(PClass(self)^, 'Create');
   if Algorithm = '' then
     md := EVP_sha256
   else
     md := EVP_get_digestbyname(pointer(Algorithm));
  if md = nil then
    raise EOpenSslHash.CreateFmt(
      'TOpenSslHmac.Create(''%s''): Unknown algorithm', [Algorithm]);
  fDigestSize := EVP_MD_size(md);
  fCtx := HMAC_CTX_new;
  EOpenSslHash.Check(self, 'Create',
    HMAC_Init_ex(fCtx, Key, KeyLength, md, nil));
end;

constructor TOpenSslHmac.Create(const Algorithm: RawUtf8;
  const Key: RawByteString);
begin
  Create(Algorithm, pointer(Key), length(Key));
end;

procedure TOpenSslHmac.Update(Data: pointer; DataLength: integer);
begin
  EOpenSslHash.Check(self, 'Update',
    HMAC_Update(fCtx, Data, DataLength));
end;

function TOpenSslHmac.Digest(Dest: pointer): cardinal;
begin
  EOpenSslHash.Check(self, 'Digest',
    HMAC_Final(fCtx, @fDigestValue, @fDigestSize));
  if Dest <> nil then
    MoveFast(fDigestValue, Dest^, fDigestSize);
  result := fDigestSize;
end;

class function TOpenSslHmac.Hmac(const Algorithm: RawUtf8;
  const Data: RawByteString; Key: pointer; KeyLength: cardinal): RawUtf8;
begin
  with Create(Algorithm, Key, KeyLength) do
    try
      Update(Data);
      result := DigestHex;
    finally
      Free;
    end;
end;

class function TOpenSslHmac.Hmac(const Algorithm: RawUtf8;
  const Data, Key: RawByteString): RawUtf8;
begin
  result := HMac(Algorithm, Data, pointer(Key), length(Key));
end;

destructor TOpenSslHmac.Destroy;
begin
  if fCtx <> nil then
    HMAC_CTX_free(fCtx);
  inherited Destroy;
end;


function OpenSslGetMd(const Algorithm: RawUtf8; const Caller: shortstring): PEVP_MD;
begin
  EOpenSslHash.CheckAvailable(nil, Caller);
  if Algorithm = 'null' then
    result := nil // e.g. for ed25519
  else
    begin
      if Algorithm = '' then
        result := EVP_sha256
      else
        result := EVP_get_digestbyname(pointer(Algorithm));
      if result = nil then
        raise EOpenSslHash.CreateFmt(
          '%s: unknown [%s] algorithm', [Caller, Algorithm]);
    end;
end;

var
  _HashAlgoMd: array[THashAlgo] of PEVP_MD;

const
  _HASHALGONAME: array[THashAlgo] of PUtf8Char = (
    'MD5', 'SHA1', 'SHA256', 'SHA384', 'SHA512', 'SHA3-256', 'SHA3-512');

function OpenSslGetMd(Algorithm: THashAlgo): PEVP_MD;
var
  h: THashAlgo;
begin
  if (_HashAlgoMd[hfSHA256] = nil) and
     OpenSslIsAvailable then
    for h := low(h) to high(h) do
      _HashAlgoMd[h] := EVP_get_digestbyname(_HASHALGONAME[h]);
  result := _HashAlgoMd[Algorithm];
end;


{ ************** OpenSSL Asymmetric Cryptography }

function OpenSslSign(const Algorithm: RawUtf8;
  Message, PrivateKey: pointer; MessageLen, PrivateKeyLen: integer;
  out Signature: RawByteString; const PrivateKeyPassword, Engine: RawUtf8): cardinal;
var
  pkey: PEVP_PKEY;
begin
  pkey := LoadPrivateKey(PrivateKey, PrivateKeyLen, PrivateKeyPassword);
  try
    Signature := pkey^.Sign(
      OpenSslGetMd(Algorithm, 'OpenSslSign'), Message, MessageLen);
    result := length(Signature);
  finally
    if pkey <> nil then
      pkey.Free;
  end;
end;

function OpenSslVerify(const Algorithm, PublicKeyPassword: RawUtf8;
  Message, PublicKey, Signature: pointer;
  MessageLen, PublicKeyLen, SignatureLen: integer; const Engine: RawUtf8): boolean;
var
  md: PEVP_MD;
  pkey: PEVP_PKEY;
begin
  md := OpenSslGetMd(Algorithm, 'OpenSslVerify');
  pkey := LoadPublicKey(PublicKey, PublicKeyLen, PublicKeyPassword);
  if (pkey = nil) or
     (SignatureLen <= 0)  then
    result := false
  else
    try
      result := pkey^.Verify(md, Signature, Message, SignatureLen, MessageLen);
    finally
      pkey.Free;
    end;
end;

function OpenSslGenerateKeys(EvpType, BitsOrCurve: integer): PEVP_PKEY;
var
  ctx, kctx: PEVP_PKEY_CTX;
  par: PEVP_PKEY;
  ctrl: integer;
begin
  result := nil;
  EOpenSslAsymmetric.CheckAvailable(nil, 'OpenSslGenerateKeys');
  ctx := EVP_PKEY_CTX_new_id(EvpType, nil);
  if ctx <> nil then
  try
    // see https://wiki.openssl.org/index.php/EVP_Key_and_Parameter_Generation
    case EvpType of
      EVP_PKEY_EC,
      EVP_PKEY_DSA,
      EVP_PKEY_DH:
        begin
          EOpenSsl.Check(EVP_PKEY_paramgen_init(ctx));
          case EvpType of
            EVP_PKEY_EC:
              ctrl := EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID;
            EVP_PKEY_DSA,
            EVP_PKEY_DH:
              ctrl := EVP_PKEY_CTRL_DSA_PARAMGEN_BITS;
          else
            exit; // paranoid
          end;
          EOpenSsl.Check(EVP_PKEY_CTX_ctrl(
            ctx, EvpType, EVP_PKEY_OP_PARAMGEN, ctrl, BitsOrCurve, nil));
          par := nil;
          EOpenSsl.Check(EVP_PKEY_paramgen(ctx, @par));
          kctx := EVP_PKEY_CTX_new(par, nil);
          if kctx = nil then
            EOpenSsl.Check(-1);
          try
            EOpenSsl.Check(EVP_PKEY_keygen_init(kctx));
            EOpenSsl.Check(EVP_PKEY_keygen(kctx, @result));
          finally
            EVP_PKEY_CTX_free(kctx);
          end;
        end;
      EVP_PKEY_RSA,
      EVP_PKEY_RSA_PSS,
      EVP_PKEY_ED25519:
        begin
          EOpenSsl.Check(EVP_PKEY_keygen_init(ctx));
          case EvpType of
            EVP_PKEY_RSA,
            EVP_PKEY_RSA_PSS:
              EOpenSsl.Check(EVP_PKEY_CTX_ctrl(ctx, EvpType, EVP_PKEY_OP_KEYGEN,
                EVP_PKEY_CTRL_RSA_KEYGEN_BITS, BitsOrCurve, nil));
          end;
          EOpenSsl.Check(EVP_PKEY_keygen(ctx, @result));
        end
      else
        exit; // unsupported type
    end;
  finally
    EVP_PKEY_CTX_free(ctx);
  end;
end;

procedure OpenSslGenerateKeys(EvpType, BitsOrCurve: integer;
  out PrivateKey, PublicKey: RawUtf8);
var
  keys: PEVP_PKEY;
begin
  keys := OpenSslGenerateKeys(EvpType, BitsOrCurve);
  if keys = nil then
    raise EOpenSslHash.CreateFmt(
      'OpenSslGenerateKeys(%d,%d) failed', [EvpType, BitsOrCurve]);
  keys.ToPem(PrivateKey, PublicKey);
  keys.Free;
end;

procedure OpenSslGenerateBinaryKeys(EvpType, BitsOrCurve: integer;
  out PrivateKey, PublicKey: RawByteString);
var
  keys: PEVP_PKEY;
begin
  keys := OpenSslGenerateKeys(EvpType, BitsOrCurve);
  if keys = nil then
    raise EOpenSslHash.CreateFmt(
      'OpenSslGenerateBinaryKeys(%d,%d) failed', [EvpType, BitsOrCurve]);
  PrivateKey := keys.PrivateToBinary;
  PublicKey := keys.PublicToBinary;
  keys.Free;
end;


function OpenSslSharedSecret(EvpType, BitsOrCurve: integer;
  const PublicKey, PrivateKey, PrivateKeyPassword: RawUtf8): RawByteString;
begin
  result := '';
  EOpenSslAsymmetric.CheckAvailable(nil, 'OpenSslSharedSecret');
  { TODO: implement as https://wiki.openssl.org/index.php/Elliptic_Curve_Diffie_Hellman }
end;


var
  prime256v1grp: PEC_GROUP;

const
  PEC_GROUP_PRIME256V1_NOTAVAILABLE = pointer(1);

function NewPrime256v1Key(out key: PEC_KEY): boolean;
begin
  result := false;
  if prime256v1grp = nil then
    if OpenSslIsAvailable then
    begin
      GlobalLock;
      try
        if prime256v1grp = nil then
        begin
          prime256v1grp := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
          if prime256v1grp = nil then
            prime256v1grp := PEC_GROUP_PRIME256V1_NOTAVAILABLE;
        end;
      finally
        GlobalUnLock;
      end;
    end
    else
      prime256v1grp := PEC_GROUP_PRIME256V1_NOTAVAILABLE;
  if prime256v1grp <> PEC_GROUP_PRIME256V1_NOTAVAILABLE then
  begin
    key := EC_KEY_new;
    if EC_KEY_set_group(key, prime256v1grp) = OPENSSLSUCCESS then
      result := true
    else
    begin
      EC_GROUP_free(prime256v1grp);
      prime256v1grp := PEC_GROUP_PRIME256V1_NOTAVAILABLE;
      EC_KEY_free(key);
    end;
  end;
end;

function ecc_make_key_osl(out PublicKey: TEccPublicKey;
                          out PrivateKey: TEccPrivateKey): boolean;
var
  key: PEC_KEY;
  pub: PByte;
  priv: PBIGNUM;
  publen, privlen: integer;
begin
  result := false;
  if not NewPrime256v1Key(key) then
    exit;
  if EC_KEY_generate_key(key) = OPENSSLSUCCESS then
  begin
    priv := EC_KEY_get0_private_key(key);
    privlen := BN_num_bytes(priv);
    pub := nil;
    publen := EC_POINT_point2buf(prime256v1grp,
      EC_KEY_get0_public_key(key), POINT_CONVERSION_COMPRESSED, @pub, nil);
    if (publen = SizeOf(PublicKey)) and
       (privlen <= SizeOf(PrivateKey)) then
    begin
      FillZero(PrivateKey); // may be padded with zeros
      BN_bn2bin(priv, @PrivateKey[SizeOf(PrivateKey) - privlen]);
      MoveFast(pub^, PublicKey, publen);
      result := true;
    end;
    OPENSSL_free(pub);
  end;
  EC_KEY_free(key);
end;

function ecdsa_sign_osl(const PrivateKey: TEccPrivateKey; const Hash: TEccHash;
  out Signature: TEccSignature): boolean;
var
  key: PEC_KEY;
  bn: PBIGNUM;
  derlen: cardinal;
  der: array[0..(ECC_BYTES * 2) + 7] of byte;
begin
  result := false;
  if not NewPrime256v1Key(key) then
    exit;
  bn := BN_bin2bn(@PrivateKey, SizeOf(PrivateKey), nil);
  derlen := 0;
  if (ECDSA_size(key) <= SizeOf(der)) and
     (EC_KEY_set_private_key(key, bn) = OPENSSLSUCCESS) and
     (ECDSA_Sign(0, @Hash, SizeOf(Hash), @der, @derlen, key) = OPENSSLSUCCESS) then
    result := DerToEcc(@der, derlen, Signature);
  bn.Free;
  EC_KEY_free(key);
end;

function PublicKeyToPoint(const PublicKey: TEccPublicKey; out pt: PEC_POINT): boolean;
begin
  pt := EC_POINT_new(prime256v1grp);
  result := EC_POINT_oct2point(prime256v1grp,
    pt, @PublicKey, SizeOf(PublicKey), nil) = OPENSSLSUCCESS;
end;

function ecdsa_verify_osl(const PublicKey: TEccPublicKey; const Hash: TEccHash;
  const Signature: TEccSignature): boolean;
var
  key: PEC_KEY;
  pt: PEC_POINT;
  der: RawByteString;
begin
  result := false;
  if not NewPrime256v1Key(key) then
    exit;
  if PublicKeyToPoint(PublicKey, pt) and
     (EC_KEY_set_public_key(key, pt) = OPENSSLSUCCESS) then
  begin
    der := EccToDer(Signature);
    result := ECDSA_verify(
      0, @Hash, SizeOf(Hash), pointer(der), length(der), key) = OPENSSLSUCCESS;
  end;
  EC_POINT_free(pt);
  EC_KEY_free(key);
end;

function ecdh_shared_secret_osl(const PublicKey: TEccPublicKey;
  const PrivateKey: TEccPrivateKey; out Secret: TEccSecretKey): boolean;
var
  key: PEC_KEY;
  pub: PEC_POINT;
  priv: PBIGNUM;
begin
  FillZero(Secret);
  result := false;
  if not NewPrime256v1Key(key) then
    exit;
  priv := BN_bin2bn(@PrivateKey, SizeOf(PrivateKey), nil);
  if PublicKeyToPoint(PublicKey, pub) and
     (EC_KEY_set_private_key(key, priv) = OPENSSLSUCCESS) and
     (ECDH_compute_key(@Secret, SizeOf(Secret), pub, key, nil) = SizeOf(Secret)) then
    result := true;
  priv.Free;
  EC_POINT_free(pub);
  EC_KEY_free(key);
end;


{ TEcc256r1VerifyOsl }

constructor TEcc256r1VerifyOsl.Create(const pub: TEccPublicKey);
begin
  EOpenSslAsymmetric.CheckAvailable(PClass(self)^, 'Create');
  if not NewPrime256v1Key(fKey) or
     not PublicKeyToPoint(pub, fPoint) or
     (EC_KEY_set_public_key(fKey, fPoint) <> OPENSSLSUCCESS) then
    raise EOpenSslAsymmetric.CreateFmt('%s.Create failed', [ClassNameShort(self)^]);
end;

destructor TEcc256r1VerifyOsl.Destroy;
begin
  EC_POINT_free(fPoint);
  EC_KEY_free(fKey);
  inherited Destroy;
end;

function TEcc256r1VerifyOsl.Verify(const hash: TEccHash;
  const sign: TEccSignature): boolean;
var
  der: RawByteString;
begin
  der := EccToDer(sign);
  result := ECDSA_verify(
    0, @hash, SizeOf(hash), pointer(der), length(der), fKey) = OPENSSLSUCCESS;
end;


{ ************** JWT Implementation using any OpenSSL Algorithm }

{ TJwtOpenSsl }

constructor TJwtOpenSsl.Create(const aJwtAlgorithm, aHashAlgorithm: RawUtf8;
  aGenEvpType, aGenBitsOrCurve: integer;
  const aPrivateKey, aPublicKey: RawByteString;
  const aPrivateKeyPassword, aPublicKeyPassword: RawUtf8;
  aClaims: TJwtClaims; const aAudience: array of RawUtf8;
  aExpirationMinutes: integer; aIDIdentifier: TSynUniqueIdentifierProcess;
  aIDObfuscationKey: RawUtf8; aIDObfuscationKeyNewKdf: integer);
begin
  EOpenSsl.CheckAvailable(PClass(self)^, 'Create');
  fAlgoMd := OpenSslGetMd(aHashAlgorithm, 'TJwtOpenSsl.Create');
  fHashAlgorithm := aHashAlgorithm;
  fGenEvpType := aGenEvpType;
  fGenBitsOrCurve := aGenBitsOrCurve;
  fPrivateKey := aPrivateKey;
  fPrivateKeyPassword := aPrivateKeyPassword;
  fPublicKey := aPublicKey;
  fPublicKeyPassword := aPublicKeyPassword;
  inherited Create(aJwtAlgorithm, aClaims, aAudience, aExpirationMinutes,
    aIDIdentifier, aIDObfuscationKey, aIDObfuscationKeyNewKdf);
end;

destructor TJwtOpenSsl.Destroy;
begin
  FillZero(fPrivateKey);
  FillZero(fPrivateKeyPassword);
  FillZero(fPublicKey);
  FillZero(fPublicKeyPassword);
  fPrivKey.Free;
  fPubKey.Free;
  inherited Destroy;
end;

class function TJwtOpenSsl.IsAvailable: boolean;
begin
  result := OpenSslIsAvailable;
end;

function TJwtOpenSsl.ComputeSignature(const headpayload: RawUtf8): RawUtf8;
var
  sign: RawByteString;
begin
  if fPrivKey = nil then
    fPrivKey := LoadPrivateKey(
      pointer(fPrivateKey), length(fPrivateKey), fPrivateKeyPassword);
  sign := fPrivKey^.Sign(fAlgoMd, pointer(headpayload), length(headpayload));
  if sign = '' then
    raise EJwtException.CreateUtf8('%.ComputeSignature: OpenSslSign failed [%]',
      [self, SSL_error_short(ERR_get_error)]);
  result := BinToBase64Uri(sign);
end;

procedure TJwtOpenSsl.CheckSignature(const headpayload: RawUtf8;
  const signature: RawByteString; var JWT: TJwtContent);
begin
  if fPubKey = nil then
    fPubKey := LoadPublicKey(
      pointer(fPublicKey), length(fPublicKey), fPublicKeyPassword);
  if fPubKey^.Verify(fAlgoMd, pointer(signature), pointer(headpayload),
      length(signature), length(headpayload)) then
    JWT.result := jwtValid
  else
    JWT.result := jwtInvalidSignature;
end;


{ TJwtAbstractOsl }

procedure TJwtAbstractOsl.SetAlgorithms;
begin
  SetAlgorithm;
  fAlgorithm := CAA_JWT[fAsym];
  fHashAlgorithm := CAA_HASH[fAsym];
  fGenEvpType := CAA_EVPTYPE[fAsym];
  fGenBitsOrCurve := CAA_BITSORCURVE[fAsym];
end;

constructor TJwtAbstractOsl.Create(const aPrivateKey, aPublicKey: RawByteString;
  const aPrivateKeyPassword, aPublicKeyPassword: RawUtf8; aClaims: TJwtClaims;
  const aAudience: array of RawUtf8; aExpirationMinutes: integer;
  aIDIdentifier: TSynUniqueIdentifierProcess; aIDObfuscationKey: RawUtf8;
  aIDObfuscationKeyNewKdf: integer);
begin
  SetAlgorithms;
  inherited Create(fAlgorithm, fHashAlgorithm, fGenEvpType, fGenBitsOrCurve,
    aPrivateKey, aPublicKey, aPrivateKeyPassword, aPublicKeyPassword,
    aClaims, aAudience, aExpirationMinutes, aIDIdentifier,
    aIDObfuscationKey, aIDObfuscationKeyNewKdf);
end;

class procedure TJwtAbstractOsl.GenerateKeys(
  out PrivateKey, PublicKey: RawUtf8);
begin
  with TJwtAbstractOsl(NewInstance) do // no need to call Create
    try
      SetAlgorithms;
      OpenSslGenerateKeys(fGenEvpType, fGenBitsOrCurve, PrivateKey, PublicKey);
    finally
      Free;
    end;
end;

class procedure TJwtAbstractOsl.GenerateBinaryKeys(
  out PrivateKey, PublicKey: RawByteString);
begin
  with TJwtAbstractOsl(NewInstance) do // no need to call Create
    try
      SetAlgorithms;
      OpenSslGenerateBinaryKeys(fGenEvpType, fGenBitsOrCurve, PrivateKey, PublicKey);
    finally
      Free;
    end;
end;



{ TJwtES256Osl }

procedure TJwtES256Osl.SetAlgorithm;
begin
  fAsym := caaES256;
end;

{ TJwtES384Osl }

procedure TJwtES384Osl.SetAlgorithm;
begin
  fAsym := caaES384;
end;

{ TJwtES512Osl }

procedure TJwtES512Osl.SetAlgorithm;
begin
  fAsym := caaES512;
end;

{ TJwtES256KOsl }

procedure TJwtES256KOsl.SetAlgorithm;
begin
  fAsym := caaES256K;
end;

{ TJwtRS256Osl }

procedure TJwtRS256Osl.SetAlgorithm;
begin
  fAsym := caaRS256;
end;

{ TJwtRS384Osl }

procedure TJwtRS384Osl.SetAlgorithm;
begin
  fAsym := caaRS384;
end;

{ TJwtRS512Osl }

procedure TJwtRS512Osl.SetAlgorithm;
begin
  fAsym := caaRS512;
end;

{ TJwtPS256Osl }

procedure TJwtPS256Osl.SetAlgorithm;
begin
  fAsym := caaPS256;
end;

{ TJwtPS384Osl }

procedure TJwtPS384Osl.SetAlgorithm;
begin
  fAsym := caaPS384;
end;

{ TJwtPS512Osl }

procedure TJwtPS512Osl.SetAlgorithm;
begin
  fAsym := caaPS512;
end;

{ TJwtEdDSAOsl }

procedure TJwtEdDSAOsl.SetAlgorithm;
begin
  fAsym := caaEdDSA;
end;


{ ************** Register OpenSSL to our General Cryptography Catalog }

type
  TCryptAsymOsl = class(TCryptAsym)
  protected
    fOsa: TCryptAsymAlgo;
    fDefaultHashAlgorithm: RawUtf8;
    fEvpType: integer;
    fBitsOrCurve: integer;
    function Algo(hasher: TCryptHasher): RawUtf8;
  public
    constructor Create(const name: RawUtf8); overload; override;
    constructor Create(osa: TCryptAsymAlgo); reintroduce; overload;
    procedure GeneratePem(out pub, priv: RawUtf8; const privpwd: RawUtf8); override;
    function Sign(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const priv: RawByteString; out sig: RawByteString;
      const privpwd: RawUtf8 = ''): boolean; override;
    function Verify(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const pub, sig: RawByteString): boolean; override;
    function SharedSecret(const pub, priv: RawByteString): RawByteString; override;
  end;


{ TCryptAsymOsl }

function TCryptAsymOsl.Algo(hasher: TCryptHasher): RawUtf8;
begin
  if hasher = nil then
    result := fDefaultHashAlgorithm
  else
    result := hasher.AlgoName; // let OpenSSL resolve the algorithm by name
end;

constructor TCryptAsymOsl.Create(const name: RawUtf8);
begin
  fDefaultHashAlgorithm := CAA_HASH[fOsa];
  fEvpType := CAA_EVPTYPE[fOsa];
  fBitsOrCurve := CAA_BITSORCURVE[fOsa];
  inherited Create(name); // also register it to GlobalCryptAlgo main list
end;

constructor TCryptAsymOsl.Create(osa: TCryptAsymAlgo);
begin
  fOsa := osa;
  Create(CAA_JWT[osa]);
end;

procedure TCryptAsymOsl.GeneratePem(out pub, priv: RawUtf8;
  const privpwd: RawUtf8);
begin
  if privpwd <> '' then
    raise ECrypt.CreateUtf8('%.GeneratePem(%): unsupported privpwd', [self, fName]);
  OpenSslGenerateKeys(fEvpType, fBitsOrCurve, priv, pub);
end;

function TCryptAsymOsl.Sign(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
  const priv: RawByteString; out sig: RawByteString; const privpwd: RawUtf8): boolean;
begin
  result := OpenSslSign(Algo(hasher),
    msg, pointer(priv), msglen, length(priv), sig, privpwd) <> 0;
end;

function TCryptAsymOsl.Verify(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
  const pub, sig: RawByteString): boolean;
begin
  result := OpenSslVerify(Algo(hasher),
    '', msg, pointer(pub), pointer(sig), msglen, length(pub), length(sig));
end;

function TCryptAsymOsl.SharedSecret(const pub, priv: RawByteString): RawByteString;
begin
  result := OpenSslSharedSecret(fEvpType, fBitsOrCurve, pub, priv, '');
end;


type
  EOpenSslCert = class(EOpenSsl);

  /// ICryptCert factory using OpenSSL X509
  TCryptCertAlgoOpenSsl = class(TCryptCertAlgo)
  protected
    fHash: PEVP_MD;
    fEvpType: integer;
    fBitsOrCurve: integer;
  public
    constructor Create(osa: TCryptAsymAlgo); reintroduce; overload;
    function NewPrivateKey: PEVP_PKEY;
    function New: ICryptCert; override; // = TCryptCertOpenSsl.Create(self)
    function FromHandle(Handle: pointer): ICryptCert; override;
  end;

  /// class implementing ICryptCert using OpenSSL X509
  TCryptCertOpenSsl = class(TCryptCert)
  protected
    fX509: PX509;
    fPrivKey: PEVP_PKEY;
    function GetMD: PEVP_MD;
  public
    destructor Destroy; override;
    procedure Clear;
    // ICryptCert methods
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; override;
    function GetSerial: RawUtf8; override;
    function GetSubject: RawUtf8; override;
    function GetSubjects: TRawUtf8DynArray; override;
    function GetIssuerName: RawUtf8; override;
    function GetSubjectKey: RawUtf8; override;
    function GetAuthorityKey: RawUtf8; override;
    function GetNotBefore: TDateTime; override;
    function GetNotAfter: TDateTime; override;
    function GetUsage: TCryptCertUsages; override;
    function GetPeerInfo: RawUtf8; override;
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
    function Verify(Sign, Data: pointer;
      SignLen, DataLen: integer): TCryptCertValidity; override;
    function Verify(const Authority: ICryptCert): TCryptCertValidity; override;
    function Handle: pointer; override;
  end;

  /// 'x509-store' ICryptStore algorithm
  TCryptStoreAlgoOpenSsl = class(TCryptStoreAlgo)
  public
    function New: ICryptStore; override; // = TCryptStoreOpenSsl.Create(self)
  end;

  /// class implementing ICryptStore using OpenSSL
  TCryptStoreOpenSsl = class(TCryptStore)
  protected
    fStore: PX509_STORE;
  public
    constructor Create(algo: TCryptAlgo); override;
    destructor Destroy; override;
    // ICryptStore methods
    function FromBinary(const Binary: RawByteString): boolean; override;
    function ToBinary: RawByteString; override;
    function GetBySerial(const Serial: RawUtf8): ICryptCert; override;
    function IsRevoked(const Serial: RawUtf8): TCryptCertRevocationReason; override;
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; override;
    function Add(const cert: ICryptCert): boolean; override;
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray; override;
    function Revoke(const Cert: ICryptCert; RevocationDate: TDateTime;
      Reason: TCryptCertRevocationReason): boolean; override;
    function IsValid(const cert: ICryptCert): TCryptCertValidity; override;
    function Verify(const Signature: RawByteString;
      Data: pointer; Len: integer): TCryptCertValidity; override;
    function Count: integer; override;
    function CrlCount: integer; override;
    // return our favorite caaES256 algorithm for signing Certificates
    function CertAlgo: TCryptCertAlgo; override;
  end;


{ TCryptCertAlgoOpenSsl }

constructor TCryptCertAlgoOpenSsl.Create(osa: TCryptAsymAlgo);
begin
  fHash := OpenSslGetMd(CAA_HASH[osa], 'TCryptCertAlgoOpenSsl.Create');
  fEvpType := CAA_EVPTYPE[osa];
  fBitsOrCurve := CAA_BITSORCURVE[osa];
  Create('x509-' + LowerCase(CAA_JWT[osa]));
end;

function TCryptCertAlgoOpenSsl.NewPrivateKey: PEVP_PKEY;
begin
  result := OpenSslGenerateKeys(fEvpType, fBitsOrCurve);
end;

function TCryptCertAlgoOpenSsl.New: ICryptCert;
begin
  result := TCryptCertOpenSsl.Create(self);
end;

function TCryptCertAlgoOpenSsl.FromHandle(Handle: pointer): ICryptCert;
var
  instance: TCryptCertOpenSsl;
begin
  if Handle = nil then
    instance := nil
  else
  begin
    instance := TCryptCertOpenSsl.Create(self);
    instance.fX509 := Handle;
  end;
  result := instance;
end;


{ TCryptCertOpenSsl }

destructor TCryptCertOpenSsl.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TCryptCertOpenSsl.Clear;
begin
  fX509.Free;
  fPrivKey.Free;
  fX509 := nil;
  fPrivKey := nil;
end;

function TCryptCertOpenSsl.GetMD: PEVP_MD;
begin
  result := (fCryptAlgo as TCryptCertAlgoOpenSsl).fHash;
end;

function TCryptCertOpenSsl.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert;
  ExpireDays, ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
var
  cn: RawUtf8;
  dns: TRawUtf8DynArray;
  name: PX509_NAME;
  x: PX509;
  key: PEVP_PKEY;
  i: PtrInt;
begin
  if fX509 <> nil then
    RaiseErrorGenerate('duplicated call');
  // prepare a new X509 OpenSSL certificate instance
  if fCryptAlgo = nil then
    RaiseErrorGenerate('after CreateFrom');
  x := NewCertificate;
  if x = nil then
    RaiseErrorGenerate('NewCertificate');
  key := nil;
  try
    key := (fCryptAlgo as TCryptCertAlgoOpenSsl).NewPrivateKey;
    if key = nil then
      RaiseErrorGenerate('NewPrivateKey');
    CsvToRawUtf8DynArray(pointer(Subjects), dns, ',', {trim=}true);
    if dns <> nil then
      cn := dns[0]
    else if (Fields = nil) or
            (Fields^.CommonName = '') then
      RaiseErrorGenerate('no Subject/CommonName');
    for i := 0 to length(dns) - 1 do
      if PosExChar(':', dns[i]) = 0 then
        dns[i] := 'DNS:' + dns[i]; // e.g. DNS: email: IP: URI:
    if not x.SetBasic(cuCA in Usages, RawUtf8ArrayToCsv(dns)) then
      RaiseErrorGenerate('SetBasic');
    if not x.SetUsage(TX509Usages(Usages - [cuCA])) then
      RaiseErrorGenerate('SetUsage');
    if not x.SetValidity(ValidDays, ExpireDays) then
      RaiseErrorGenerate('SetValidity');
    name := X509_get_subject_name(x);
    if Fields <> nil then
      with Fields^ do
      begin
        if CommonName <> '' then
          cn := CommonName;
        name.AddEntries(
          Country, State, Locality, Organization, OrgUnit, cn);
      end
      else
        name.AddEntry('CN', cn);
    EOpenSslCert.Check(X509_set_pubkey(x, key));
    if not x.SetExtension(NID_subject_key_identifier, 'hash') then
      RaiseErrorGenerate('SKID');
    if Authority = nil then
    begin
      // self-signed certificate - no AKID
      EOpenSslCert.Check(X509_set_issuer_name(x, name)); // issuer = subject
      if x.Sign(key, GetMD) = 0 then
        RaiseErrorGenerate('Self Sign');
    end
    else
      try
        // certificate signed by a provided CA
        fX509 := x; // as expected by next line
        Sign(Authority);
      except
        fX509 := nil; // on erorr, rollback (and call x.Free)
      end;
    //writeln('IsSelfSigned=',x.IsSelfSigned);
    // the certificate was generated so can be stored within this instance
    fX509 := x;
    fPrivKey := key;
    key := nil;
    x := nil;
  finally
    x.Free;
    key.Free;
  end;
  result := self;
end;

function TCryptCertOpenSsl.GetSerial: RawUtf8;
begin
  result := fX509.SerialNumber;
end;

function TCryptCertOpenSsl.GetSubject: RawUtf8;
var
  subs: TRawUtf8DynArray;
begin
  result := fX509.GetSubject('CN');
  if result <> '' then
    exit;
  subs := fX509.SubjectAlternativeNames;
  if subs <> nil then
    result := subs[0]; // return the first DNS: as with mormot.crypt.ecc
end;

function TCryptCertOpenSsl.GetSubjects: TRawUtf8DynArray;
begin
  result := fX509.SubjectAlternativeNames;
end;

function TCryptCertOpenSsl.GetIssuerName: RawUtf8;
begin
  result := fX509.IssuerName;
end;

function TCryptCertOpenSsl.GetSubjectKey: RawUtf8;
begin
  result := fX509.SubjectKeyIdentifier;
end;

function TCryptCertOpenSsl.GetAuthorityKey: RawUtf8;
begin
  result := fX509.AuthorityKeyIdentifier;
end;

function TCryptCertOpenSsl.GetNotBefore: TDateTime;
begin
  result := fX509.NotBefore;
end;

function TCryptCertOpenSsl.GetNotAfter: TDateTime;
begin
  result := fX509.NotAfter;
end;

function TCryptCertOpenSsl.GetUsage: TCryptCertUsages;
begin
  result := TCryptCertUsages(word(fX509.GetUsage));
end;

function TCryptCertOpenSsl.GetPeerInfo: RawUtf8;
begin
  result := fX509.PeerInfo;
end;

function TCryptCertOpenSsl.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
begin
  result := '';
  if fX509 = nil then
    exit;
  if not (Format in [ccfBinary, ccfPem]) then
    // hexa or base64 encoding of the binary output is handled by TCryptCert
    result := inherited Save(Content, PrivatePassword, Format)
  else case Content of
    cccCertOnly:
      begin
        // include the X509 certificate (but not any private key) as DER or PEM
        result := fX509.ToBinary;
        if Format = ccfPem then
          result := DerToPem(result, pemCertificate);
      end;
    cccCertWithPrivateKey:
      if fPrivKey = nil then
        RaiseError('Save(cccCertWithPrivateKey) with no Private Key')
      else if Format = ccfPem then
        // concatenate the certificate and its private key as PEM
        result := DerToPem(fX509.ToBinary, pemCertificate) + #13#10 +
                  fPrivKey.PrivateKeyToPem(PrivatePassword)
      else
        // ccfBinary will use the PKCS12 binary encoding
        result := fX509.ToPkcs12(fPrivKey, PrivatePassword);
    cccPrivateKeyOnly:
      if fPrivKey = nil then
        RaiseError('Save(cccPrivateKeyOnly) with no Private Key')
      else if Format = ccfPem then
        result := fPrivKey.PrivateKeyToPem(PrivatePassword)
      else
      begin
        result := fPrivKey.PrivateToBinary;
        if Format = ccfPem then
          result := DerToPem(result, pemPrivateKey);
      end;
  end;
end;

function TCryptCertOpenSsl.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
var
  P: PUtf8Char;
  k: TPemKind;
  pem, cert, priv: RawByteString;
  pkcs12: PPKCS12;
begin
  result := false;
  if Content = cccPrivateKeyOnly then
  begin
    // input only include the private key as DER or PEM
    fPrivKey.Free;
    fPrivKey := LoadPrivateKey(pointer(saved), length(saved), PrivatePassword);
    if fPrivKey <> nil then
      if fX509.MatchPrivateKey(fPrivKey) then
        result := true
      else
      begin
        fPrivKey.Free;
        fPrivKey := nil;
      end;
    exit; // don't clear the main X509 certificate
  end;
  Clear;
  if Saved = '' then
    exit;
  case Content of
    cccCertOnly:
      // input only include the X509 certificate as DER or PEM
      if IsPem(Saved) then
        fX509 := LoadCertificate(PemToDer(Saved))
      else
        fX509 := LoadCertificate(Saved);
    cccCertWithPrivateKey:
      begin
        // input include the X509 certificate and its associated private key
        if IsPem(Saved) then
        begin
          // PEM certificate and PEM private key were concatenated
          P := pointer(Saved);
          repeat
            pem := NextPem(P, @k);
            if pem = '' then
              break;
            if k = pemCertificate then
              if cert <> '' then
                exit // should contain a single Certificate
              else
                cert := PemToDer(pem)
            else
              priv := pem; // private key may be with several TPemKind markers
          until false;
          if (cert = '') or
             (priv = '') then
            exit;
          fX509 := LoadCertificate(cert);
          if fX509 = nil then
            exit;
          fPrivKey := LoadPrivateKey(pointer(priv), length(priv), PrivatePassword);
        end
        else
        begin
          // input should be some PKCS12 binary with certificate and private key
          pkcs12 := LoadPkcs12(Saved);
          if not pkcs12.Extract(PrivatePassword, @fPrivKey, @fX509, nil) then
            Clear;
          pkcs12.Free;
        end;
        if not fX509.MatchPrivateKey(fPrivKey) then
          Clear;
      end;
  end;
  result := fX509 <> nil;
end;

function TCryptCertOpenSsl.HasPrivateSecret: boolean;
begin
  result := fPrivKey <> nil;
end;

function TCryptCertOpenSsl.GetPublicKey: RawByteString;
begin
  result := fX509.GetPublicKey.PublicToBinary;
end;

function TCryptCertOpenSsl.GetPrivateKey: RawByteString;
begin
  if HasPrivateSecret then
    result := fPrivKey.PrivateToBinary
  else
    result := '';
end;

function TCryptCertOpenSsl.SetPrivateKey(const saved: RawByteString): boolean;
begin
  result := false;
  fPrivKey.Free;
  fPrivKey := nil;
  if saved = '' then
    exit;
  fPrivKey := LoadPrivateKey(saved);
  if fX509.MatchPrivateKey(fPrivKey) then
    result := true
  else
  begin
    fPrivKey.Free;
    fPrivKey := nil;
  end;
end;

function TCryptCertOpenSsl.Sign(Data: pointer; Len: integer): RawByteString;
begin
  if HasPrivateSecret and
     fX509.HasUsage(kuDigitalSignature) then
    result := fPrivKey.Sign(GetMD, Data, Len)
  else
    result := '';
end;

procedure TCryptCertOpenSsl.Sign(const Authority: ICryptCert);
var
  a: PX509;
  auth: TCryptCertOpenSsl;
begin
  if Assigned(Authority) and
     Authority.HasPrivateSecret then
  begin
    auth := Authority.Instance as TCryptCertOpenSsl;
    a := auth.fX509;
    if not a.HasUsage(kuKeyCertSign) then
      RaiseError('Sign: no kuKeyCertSign');
    EOpenSslCert.Check(X509_set_issuer_name(fX509, X509_get_subject_name(a)));
    if not fX509.SetExtension(NID_authority_key_identifier, 'keyid:always', a) then
      RaiseError('Sign: AKID'); // see RFC 3280
    if fX509.Sign(auth.fPrivKey, auth.GetMD) = 0 then
      RaiseError('Sign: CA Sign');
  end
  else
    RaiseError('Sign: not a CA');
end;

function CanVerify(auth: PX509; usage: TX509Usage;
  selfsigned: boolean): TCryptCertValidity;
var
  now: TDateTime;
begin
  now := NowUtc;
  if auth = nil then
    result := cvUnknownAuthority
  else if not (selfsigned or auth.HasUsage(usage)) then
    result := cvWrongUsage
  else if (now >= auth.NotAfter) or
          (now < auth.NotBefore) then
    result := cvDeprecatedAuthority
  else
    result := cvValidSigned;
end;

function TCryptCertOpenSsl.Verify(Sign, Data: pointer;
  SignLen, DataLen: integer): TCryptCertValidity;
begin
  if (SignLen <= 0) or
     (DataLen <= 0) then
    result := cvBadParameter
  else
    result := CanVerify(fX509, kuDigitalSignature, false);
  if result = cvValidSigned then
    if fX509.GetPublicKey.Verify(GetMD, Sign, Data, SignLen, DataLen) then
      if fX509.IsSelfSigned then
        result := cvValidSelfSigned
      else
        result := cvValidSigned
    else
      result := cvInvalidSignature;
end;

function TCryptCertOpenSsl.Verify(const Authority: ICryptCert): TCryptCertValidity;
var
  auth: PX509;
begin
  result := cvBadParameter;
  if fX509 = nil then
    exit;
  if fX509.IsSelfSigned then
    auth := fX509
  else if Assigned(Authority) then
    if Authority.Instance.InheritsFrom(TCryptCertOpenSsl) then
    begin
      auth := Authority.Handle;
      if auth.SubjectKeyIdentifier <> fX509.AuthorityKeyIdentifier then
        auth := nil;
    end
    else
      exit
  else
    auth := nil;
  result := CanVerify(auth, kuKeyCertSign, auth = fX509);
  if result = cvValidSigned then
    if X509_verify(fX509, auth.GetPublicKey) <> 1 then
      result := cvInvalidSignature
    else if auth = fX509 then
      result := cvValidSelfSigned
end;

function TCryptCertOpenSsl.Handle: pointer;
begin
  result := fX509; // a PX509 instance
end;


{ TCryptStoreAlgoOpenSsl }

function TCryptStoreAlgoOpenSsl.New: ICryptStore;
begin
  result := TCryptStoreOpenSsl.Create(self);
end;


{ TCryptStoreOpenSsl }

constructor TCryptStoreOpenSsl.Create(algo: TCryptAlgo);
begin
  inherited Create(algo);
  fStore := NewCertificateStore;
end;

destructor TCryptStoreOpenSsl.Destroy;
begin
  inherited Destroy;
  fStore.Free;
end;

function TCryptStoreOpenSsl.ToBinary: RawByteString;
var
  x: PX509DynArray;
  c: PX509_CRLDynArray;
  i: PtrInt;
begin
  // since DER has no simple binary array format, use PEM serialization
  result := '';
  x := fStore.Certificates;
  for i := 0 to length(x) - 1 do
    result := x[i].ToPem + CRLF;
  c := fStore.Crls;
  for i := 0 to length(c) - 1 do
    result := c[i].ToPem + CRLF;
end;

function TCryptStoreOpenSsl.FromBinary(const Binary: RawByteString): boolean;
begin
  fStore.Free;
  fStore := NewCertificateStore;          // clear (with proper ref counting)
  result := AddFromBuffer(Binary) <> nil; // most probably ToBinary PEM format
end;

function TCryptStoreOpenSsl.GetBySerial(const Serial: RawUtf8): ICryptCert;
var
  x: PX509;
begin
  x := fStore.BySerial(Serial); // makes x.Acquire
  result := CryptCertAlgoOpenSsl[caaRS256].FromHandle(x);
end;

function ToReason(r: integer): TCryptCertRevocationReason;
begin
  case r of // both types follow RFC5280 specification
    CRL_REASON_UNSPECIFIED:
      result := crrUnspecified;
    CRL_REASON_KEY_COMPROMISE:
      result := crrCompromised;
    CRL_REASON_CA_COMPROMISE:
      result := crrAuthorityCompromised;
    CRL_REASON_AFFILIATION_CHANGED:
      result := crrUnAffiliated;
    CRL_REASON_SUPERSEDED:
      result := crrSuperseded;
    CRL_REASON_CESSATION_OF_OPERATION:
      result := crrReplaced;
    CRL_REASON_CERTIFICATE_HOLD:
      result := crrTempHold;
    CRL_REASON_REMOVE_FROM_CRL:
      result := crrRemoved;
    CRL_REASON_PRIVILEGE_WITHDRAWN:
      result := crrWithdrawn;
    CRL_REASON_AA_COMPROMISE:
      result := crrServerCompromised;
  else
    result := crrNotRevoked; // e.g. CRL_REASON_NONE
  end;
end;

function FromReason(r: TCryptCertRevocationReason): integer;
begin
  case r of
    crrUnspecified:
      result := CRL_REASON_UNSPECIFIED;
    crrCompromised:
      result := CRL_REASON_KEY_COMPROMISE;
    crrAuthorityCompromised:
      result := CRL_REASON_CA_COMPROMISE;
    crrUnAffiliated:
      result := CRL_REASON_AFFILIATION_CHANGED;
    crrSuperseded:
      result := CRL_REASON_SUPERSEDED;
    crrReplaced:
      result := CRL_REASON_CESSATION_OF_OPERATION;
    crrTempHold:
      result := CRL_REASON_CERTIFICATE_HOLD;
    crrRemoved:
      result := CRL_REASON_REMOVE_FROM_CRL;
    crrWithdrawn:
      result := CRL_REASON_PRIVILEGE_WITHDRAWN;
    crrServerCompromised:
      result := CRL_REASON_AA_COMPROMISE;
  else
    result := CRL_REASON_NONE;
  end;
end;

function TCryptStoreOpenSsl.IsRevoked(const Serial: RawUtf8): TCryptCertRevocationReason;
begin
  result := ToReason(fStore.IsRevoked(Serial));
end;

function TCryptStoreOpenSsl.IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason;
begin
  if cert = nil then
    result := crrNotRevoked
  else
    result := ToReason(
      fStore.IsRevoked((cert.Instance as TCryptCertOpenSsl).fX509.GetSerial));
end;

function TCryptStoreOpenSsl.Add(const cert: ICryptCert): boolean;
begin
  result := (cert <> nil) and
            fStore.AddCertificate((cert.Instance as TCryptCertOpenSsl).fX509);
end;

function TCryptStoreOpenSsl.AddFromBuffer(
  const Content: RawByteString): TRawUtf8DynArray;
var
  P: PUtf8Char;
  pem, serial: RawUtf8;
  k: TPemKind;
begin
  result := nil;
  if IsPem(Content) then
  begin
    P := pointer(Content);
    repeat
      // parse each incoming PEM entry into X509 certificates or CRLs
      pem  := NextPem(P, @k);
      if pem = '' then
        break;
      if k <> pemCrl then
        if fStore.AddCertificateFromPem(pem, @result) <> 0 then
          continue;
      if k <> pemCertificate then
        if fStore.AddCrlFromPem(Pem) <> 0 then
          continue;
    until false;
  end
  else
  begin
    // try binary DER serialization of X509 certificate or CRL
    serial := fStore.AddFromBinary(Content);
    if serial <> '' then
      AddRawUtf8(result, serial);
  end;
end;

function TCryptStoreOpenSsl.Count: integer;
begin
  result := fStore.CertificateCount;
end;

function TCryptStoreOpenSsl.CrlCount: integer;
begin
  result := fStore.CrlCount;
end;

function TCryptStoreOpenSsl.Revoke(const Cert: ICryptCert;
  RevocationDate: TDateTime; Reason: TCryptCertRevocationReason): boolean;
var
  r, days: integer;
begin
  result := false;
  if Cert = nil then
    exit;
  r := FromReason(Reason);
  if r = CRL_REASON_NONE then
    raise EOpenSslCert.CreateFmt(
      'TCryptStoreOpenSsl.Revoke: unsupported Reason=%s', [ToText(Reason)^]);
  if RevocationDate = 0 then
    days := 0 // revoke now
  else
  begin
    days :=  trunc(RevocationDate - Now);
    if days < 0 then
      days := 0;
  end;
  result := fStore.MainCrl.AddRevokedCertificate(
    (cert.Instance as TCryptCertOpenSsl).fX509, nil, r, days);
end;

function ToValidity(err: integer): TCryptCertValidity;
begin
  case err of
    X509_V_OK:
      result := cvValidSigned; // caller would know about cvValidSelfSigned
    X509_V_ERR_UNSPECIFIED:
      result := cvUnknown;
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT,
    X509_V_ERR_UNABLE_TO_GET_CRL,
    X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE,
    X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE,
    X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY,
    X509_V_ERR_CERT_SIGNATURE_FAILURE,
    X509_V_ERR_CRL_SIGNATURE_FAILURE:
      result := cvCorrupted;
    X509_V_ERR_CERT_NOT_YET_VALID,
    X509_V_ERR_CERT_HAS_EXPIRED:
      result := cvDeprecatedAuthority;
    X509_V_ERR_CRL_NOT_YET_VALID,
    X509_V_ERR_CRL_HAS_EXPIRED,
    X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD,
    X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD,
    X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD,
    X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
      result := cvInvalidDate;
    X509_V_ERR_CERT_REVOKED:
      result := cvRevoked;
    X509_V_ERR_INVALID_CA,
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY,
    X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT, // self-signed cert not in chain
    X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:   // root not in chain
      result := cvUnknownAuthority;
  else
    // X509_V_ERR_OUT_OF_MEM,
    // X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE,
    // X509_V_ERR_CERT_CHAIN_TOO_LONG,
    result := cvWrongUsage;
  end;
end;

function TCryptStoreOpenSsl.IsValid(const cert: ICryptCert): TCryptCertValidity;
var
  x: PX509;
  res: integer;
begin
  result := cvBadParameter;
  if cert = nil then
    exit;
  x := (cert.Instance as TCryptCertOpenSsl).fX509;
  if x = nil then
    exit;
  res := fStore.Verify(x);
  if res = X509_V_OK then
    if x.IsSelfSigned then
      result := cvValidSelfSigned
    else
      result := cvValidSigned
  else
    result := ToValidity(res);
end;

function TCryptStoreOpenSsl.Verify(const Signature: RawByteString;
  Data: pointer; Len: integer): TCryptCertValidity;
begin
  result := cvNotSupported;
end;

function TCryptStoreOpenSsl.CertAlgo: TCryptCertAlgo;
begin
  result := CryptCertAlgoOpenSsl[caaES256];
end;



procedure RegisterOpenSsl;
var
  osa: TCryptAsymAlgo;
begin
  if (TAesFast[mGcm] = TAesGcmOsl) or
     not OpenSslIsAvailable then
    exit;
  // set the fastest AES implementation classes
  TAesFast[mGcm] := TAesGcmOsl;
  {$ifdef HASAESNI}
    // mormot.crypt.core x86_64 asm is faster than OpenSSL - but GCM
    {$ifndef CPUX64}
    // our AES-CTR x86_64 asm is faster than OpenSSL's
    TAesFast[mCtr] := TAesCtrOsl;
    {$endif CPUX64}
  {$else}
  // ARM/Aarch64 would rather use OpenSSL than our purepascal code
  TAesFast[mEcb] := TAesEcbOsl;
  TAesFast[mCbc] := TAesCbcOsl;
  TAesFast[mCfb] := TAesCfbOsl;
  TAesFast[mOfb] := TAesOfbOsl;
  TAesFast[mCtr] := TAesCtrOsl;
  {$endif HASAESNI}
  // redirects raw mormot.crypt.ecc256r1 functions to the much faster OpenSSL
  @Ecc256r1MakeKey := @ecc_make_key_osl;
  @Ecc256r1Sign := @ecdsa_sign_osl;
  @Ecc256r1Verify := @ecdsa_verify_osl;
  @Ecc256r1SharedSecret := @ecdh_shared_secret_osl;
  TEcc256r1Verify := TEcc256r1VerifyOsl;
  TCryptAsymOsl.Implements('secp256r1,NISTP-256,prime256v1'); // with caaES256
  for osa := low(osa) to high(osa) do
  begin
    CryptAsymOpenSsl[osa] := TCryptAsymOsl.Create(osa);
    CryptCertAlgoOpenSsl[osa] := TCryptCertAlgoOpenSsl.Create(osa);
  end;
  // not stable enough yet
  //CryptStoreAlgoOpenSsl := TCryptStoreAlgoOpenSsl.Implements(['x509-store']);
end;

procedure FinalizeUnit;
begin
  if (prime256v1grp <> nil) and
     (prime256v1grp <> PEC_GROUP_PRIME256V1_NOTAVAILABLE) then
    EC_GROUP_free(prime256v1grp);
end;



initialization

finalization
  FinalizeUnit;

{$else}

implementation

{$endif USE_OPENSSL}

end.
