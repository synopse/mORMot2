/// Framework Core High-Level Cryptographic Algorithms Factories
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.crypt.factory;

{
  *****************************************************************************

   High-Level Cryptographic Algorithms Factories shared by all framework units
    - Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories
    - Key and Certificate Aware PEM/DER Helpers

   Uses optimized mormot.crypt.core.pas and mormot.crypt.secure.pas for the
   actual cryptographic process; the algorithm providers (mormot.crypt.ecc,
   mormot.crypt.rsa, mormot.crypt.x509, mormot.crypt.openssl...) register
   themselves into the catalog defined here.

  *****************************************************************************

   Legal Notice: as stated by our LICENSE.md terms, make sure that you comply
   to any restriction about the use of cryptographic software in your country.
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.data,
  mormot.core.variants,
  mormot.core.json,
  mormot.core.search,
  mormot.lib.sspi,
  mormot.crypt.core,
  mormot.crypt.secure;


{ ************* Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories }

type
  /// exception raised by the Rnd/Hash/Sign/Cipher/Asym/Cert/Store factories
  // - inherits from ESynCrypto, so that a single handler can catch the whole
  // cryptographic stack, from mormot.crypt.core up to the factories
  ECrypt = class(ESynCrypto);

  TCryptAlgo = class;
  TCryptAlgos = array of TCryptAlgo;

  /// abstract class implemented e.g. by TCryptRandom/TCryptHasher/TCryptAsym
  // - we define a class and not a meta-class since it allows to resolve and
  // store some engine-specific context ahead of time, for faster process
  // - inherited classes would dedicated New() factory methods; this parent
  // features the internal registration feature of the known algorithms
  TCryptAlgo = class(TSynPersistent)
  protected
    fName: RawUtf8;
    // case-insensitive quick lookup of the algorithms into a TCryptAlgo instance
    class function InternalFind(const name: RawUtf8; var Last: TCryptAlgo): pointer;
    class function InternalResolve(const name: RawUtf8; CSV: PUtf8Char): integer;
  public
    /// inherited classes should properly initialize this kind of process
    constructor Create(const name: RawUtf8); reintroduce; virtual;
    /// register this class to override one or several identifiers implementation
    // - returns the last instance created for name[]
    class function Implements(const name: array of RawUtf8): pointer; overload;
    /// register this class to override one or several identifiers implementation
    class procedure Implements(csv: PUtf8Char; const suffix: RawUtf8 = ''); overload;
    /// return all the TCryptAlog instances matching this class type
    // - could be used e.g. as TCryptRandom.Instances
    class function Instances: TCryptAlgos;
    /// return all the TCryptAlog instances matching this class type
    // - could be used e.g. as TCryptRandom.Names
    class function Names: TRawUtf8DynArray;
  published
    /// process-wide case-insensitive identifier for quick lookup of the algorithms
    // - typical values may follow OpenSSL naming, e.g. 'MD5', 'AES-128-GCM' or
    // 'prime256v1'
    property AlgoName: RawUtf8
      read fName;
  end;

  /// interface as implemented e.g. by TCryptHash and resolved by Hash() factory
  ICryptHash = interface
    /// iterative process of a memory buffer
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; overload;
    /// iterative process of a memory buffer
    function Update(const buf: RawByteString): ICryptHash; overload;
    /// iterative process of a file content
    procedure UpdateFile(const filename: TFileName);
    /// iterative process of a stream content
    // - return the number of bytes hashed from the input TStream
    function UpdateStream(stream: TStream): Int64;
    /// compute the digest, and return it in a memory buffer
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    /// compute the digest, and return it as UTF-8 hexadecimal text
    function Final: RawUtf8; overload;
  end;

  {$M+}
  /// abstract class implemented e.g. by TCryptHash/TCryptCipher/TCryptKey
  TCryptInstance = class(TInterfacedObject)
  protected
    fCryptAlgo: TCryptAlgo;
  public
    /// initialize the instance
    constructor Create(algo: TCryptAlgo); overload; virtual;
    /// resolve the name via TCryptAlgo.InternalFind() and initialize the instance
    constructor Create(const name: RawUtf8); overload;
    /// access to the associated algorithm
    property CryptAlgo: TCryptAlgo
      read fCryptAlgo;
  end;
  {$M-}

  /// randomness generator parent class, as resolved by Rnd() factory function
  TCryptRandom = class(TCryptAlgo)
  public
    /// retrieve some random bytes into a buffer
    procedure Get(dst: pointer; dstlen: PtrInt); overload; virtual; abstract;
    /// retrieve some random bytes into a RawByteString
    // - defined as procedure to ensure RefCnt=1 so that FillZero(dst) works
    procedure Get(var dst: RawByteString; len: PtrInt); overload; virtual;
    /// retrieve some random bytes into a TBytes
    function GetBytes(len: PtrInt): TBytes;
    /// retrieve a random 32-bit value
    function Get32: cardinal; overload; virtual;
    /// retrieve a random 32-bit value
    function Get32(max: cardinal): cardinal; overload;
    /// returns a 64-bit unsigned random number
    function Get64: QWord; virtual;
    /// retrieve a random floating point value in the [0..1) range calling Get32
    function GetDouble: double;
    /// computes a random ASCII password following proper common safety rules
    // - will contain uppercase/lower letters, digits and $.:()?%!-+*/@#
    // excluding ;,= to allow direct use in CSV content
    // - won't return the letters O and I to avoid confusion with digits 0 and 1
    function GetPassword(len: PtrInt): SpiUtf8;
  end;

  /// an abstract TCryptRandom class which will call Get32 as its random source
  TCryptRandom32 = class(TCryptRandom)
  public
    /// will call the Get32 virtual method to fill
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

  /// hashing/signing parent class, as returned by Hash/Sign() factories
  TCryptHash = class(TCryptInstance, ICryptHash)
  protected
    function InternalFinal(out dig: THash512Rec): PtrInt; virtual; abstract;
  public
    // ICryptHash methods
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; overload; virtual; abstract;
    function Update(const buf: RawByteString): ICryptHash; overload;
    procedure UpdateFile(const filename: TFileName);
    function UpdateStream(stream: TStream): Int64;
    function Final(digest: pointer; digestlen: PtrInt): PtrInt; overload;
    function Final: RawUtf8; overload;
  end;

  /// hashing parent class, as resolved by Hasher() factory function
  // - this class is to ensure a content as not been tempered: use Signer()
  // to compute a digital signature from a given secret
  TCryptHasher = class(TCryptAlgo)
  public
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(buf: pointer; buflen: PtrInt): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(const buf: RawByteString): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an binary digest
    // - returns the number of bytes stored in digest
    function Full(buf: pointer; buflen: PtrInt; out digest: THash512Rec): PtrInt; overload;
    /// one-step process of a whole file content, into an hexadecimal digest
    function FullFile(const filename: TFileName): RawUtf8;
    /// main factory to create a new hasher instance with this algorithm
    function New: ICryptHash; virtual; abstract;
    /// return the THashAlgo equivalency of this hasher
    // - this default implementation returns false meaning that it is unknown
    function HashAlgo(out hasher: THashAlgo): boolean; virtual;
  end;

  /// signing parent class, as resolved by Signer() factory function
  // - in respect to TCryptHasher, requires a secret key to be supplied
  // for safe HMAC content signature
  TCryptSigner = class(TCryptAlgo)
  public
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(key, buf: pointer; keylen, buflen: PtrInt): RawUtf8; overload;
    /// one-step process of a whole memory buffer, into an hexadecimal digest
    function Full(const key, buf: RawByteString): RawUtf8; overload;
    /// one-step process of a whole file content, into an hexadecimal digest
    function FullFile(key: pointer; keylen: PtrInt; const filename: TFileName): RawUtf8;
    /// strong PBKDF2 derivation of a secret (and salt) using this algorithm
    // - returns the number of bytes computed in returned key memory
    function Pbkdf2(const secret, salt: RawUtf8; rounds: integer;
      out key: THash512Rec): integer; overload; virtual; abstract;
    /// main factory to create a new signer instance with this algorithm
    function New(key: pointer; keylen: PtrInt): ICryptHash; virtual; abstract;
    /// main factory to create a new signer instance from PBKDF2 derivation
    function NewPbkdf2(const secret, salt: RawUtf8; rounds: integer): ICryptHash;
  end;

  /// interface as implemented e.g. by TCryptCipher from TCryptCipherAlgo.New
  // - as resolved by Cipher() Encrypt() or Decrypt() factory functions
  ICryptCipher = interface
    /// quickly generate a cipher with the same algorithm, direction and key
    function Clone: ICryptCipher;
    /// general encryption/decryption method using RawByteString buffers
    // - an IV is generated at the start of this if none was specified at New()
    // - will do proper PKCS7 padding on the src input buffer
    // - return TRUE on success, FALSE if input padding or AEAD MAC is incorrect
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString = ''): boolean; overload;
    /// general encryption/decryption method using TBytes buffers
    // - use TByteDynArray for aeadinfo because TBytes raises a Delphi XE
    // compiler bug - it should be assignment compatible with any TBytes value
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray = nil): boolean; overload;
    /// low-level encryption/decryption on memory buffers
    // - srclen/dsstlen should match the size block of the algorithm,
    // e.g. 16 bytes for AES or 1 byte for SHAKE (i.e. SHA-3 in XOF cipher mode)
    // - on AES-GCM algorithm, if dst=nil then AAD is set from src/srclen
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt);
    /// low-level GMAC computation for AES-GCM
    // - after Encrypt, fill gmac with the tag value of the data and return true
    // - after Decrypt, return true only if the tag value of the data match gmac
    // - always return false if not AES-GCM is used as algorithm
    function RawFinal(var gmac: TAesBlock): boolean;
  end;

  /// symmetric encryption class, as resolved by CipherAlgo() factory function
  TCryptCipherAlgo = class(TCryptAlgo)
  public
    /// check if this algorithm is of AEAD kind, i.e. can cipher and authenticate
    // - note that currently our OpenSSL AES-GCM wrapper has troubles with
    // AEAD associated authentication so returns false: it will compute and check
    // the GMAC of the content as expected, but only our internal 'AES-###-GCM-INT'
    // actually supports aeadinfo <> '' in ICryptCipher.Process
    function IsAead: boolean; virtual; abstract;
    /// main factory to create a new instance with this algorithm
    // - the supplied key should match the size expected by the algorithm
    function New(key: pointer; encrypt: boolean; iv: pointer = nil): ICryptCipher;
      overload; virtual; abstract;
    /// main factory to create a new instance from PBKDF2 key derivation
    function New(const hash, secret, salt: RawUtf8; rounds: integer;
      encrypt: boolean): ICryptCipher; overload;
    /// main factory to create a new encryption instance with this algorithm
    function Encrypt(key: pointer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new decryption instance with this algorithm
    function Decrypt(key: pointer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new encryption instance from PBKDF2 key derivation
    function Encrypt(const sign, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// main factory to create a new decryption instance from PBKDF2 key derivation
    function Decrypt(const sign, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
      {$ifdef HASINLINE} inline; {$endif}
  end;

  /// symmetric encryption parent class, as returned by Cipher() factory
  TCryptCipher = class(TCryptInstance, ICryptCipher)
  public
    // ICryptCipher methods
    function Clone: ICryptCipher; virtual; abstract;
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString): boolean; overload; virtual; abstract;
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray): boolean; overload; virtual; abstract;
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt); virtual; abstract;
    function RawFinal(var gmac: TAesBlock): boolean; virtual; abstract;
  end;

  /// the algorithms supported by a ICryptPublicKey/ICryptPrivateKey
  // - does not match TCryptAsymAlgo because ckaRsa/ckaRsaPss do not define the
  // hash algorithm needed, so dedicated caaRSxxx/caaPSxxx items are needed
  TCryptKeyAlgo = (
    ckaNone,
    ckaRsa,
    ckaRsaPss,
    ckaEcc256,
    ckaEcc384,
    ckaEcc512,
    ckaEcc256k,
    ckaEdDSA);

  /// asymmetric public-key cryptography parent class, as returned by Asym()
  TCryptAsym = class(TCryptAlgo)
  protected
    fPemPublic, fPemPrivate: byte; // TPemKind as defined below
  public
    /// the high-level asymmetric algorithm used for this certificate
    function KeyAlgo: TCryptKeyAlgo; virtual; abstract;
    /// generate a public/private pair of keys in the PEM text format
    procedure GeneratePem(out pub, priv: RawUtf8; const privpwd: RawUtf8); virtual;
    /// generate a public/private pair of keys in the DER binary format
    procedure GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8); virtual;
    /// digital signature of some message using a private key
    // - the message is first hashed with the supplied TCryptHasher
    // - the signature is returned in binary DER format
    function Sign(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const priv: RawByteString; out sig: RawByteString;
      const privpwd: RawUtf8 = ''): boolean; overload; virtual; abstract;
    /// digital signature of some message using a private key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Sign(const msg, priv: RawByteString; out sig: RawByteString;
      const hashername: RawUtf8 = ''; const privpwd: RawUtf8 = ''): boolean; overload;
    /// digital signature of some message using a private key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Sign(const msg, priv: TBytes; out sig: TBytes;
      const hashername: RawUtf8 = ''; const privpwd: RawUtf8 = ''): boolean; overload;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the supplied TCryptHasher
    function Verify(hasher: TCryptHasher; msg: pointer; msglen: PtrInt;
      const pub, sig: RawByteString): boolean; overload; virtual; abstract;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Verify(const msg, pub, sig: RawByteString;
      const hashername: RawUtf8 = ''): boolean; overload;
    /// digital signature verification of some message using a public key
    // - the message is first hashed with the default hasher of this
    // algorithm, or the specific hashername
    function Verify(const msg, pub, sig: TBytes;
      const hashername: RawUtf8 = ''): boolean; overload;
  end;

  /// the supported asymmetric algorithms, following the JWT high-level naming
  // - caaES256, caaES384, caaES512 and caaES256K match OpenSSL EVP_PKEY_EC with
  // prime256v1, NID_secp384r1, NID_secp521r1 and NID_secp256k1 curves
  // - caaRS256, caaRS384 and caaRS512 match OpenSSL EVP_PKEY_RSA with
  // SHA-256, SHA-384 and SHA-512 digest method
  // - caaPS256, caaPS384 and caaPS512 match OpenSSL EVP_PKEY_RSA_PSS with
  // SHA-256, SHA-384 and SHA-512 digest method
  // - caaEdDSA match OpenSSL EVP_PKEY_ED25519 curve
  // - mormot.crypt.ecc unit implements caaES256 with native pascal
  // - mormot.crypt.rsa unit implements all caaRS256 .. caaPS512 algorithms
  // - mormot.crypt.openssl unit implements all those algorithms
  // - our RSA wrappers generate with RSA_DEFAULT_GENERATION_BITS = 2048-bit,
  // but our units can read and manage any other size of existing certificates
  // generated e.g. by OpenSSL or our mormot.crypt.rsa unit
  TCryptAsymAlgo = (
    caaES256,
    caaES384,
    caaES512,
    caaES256K,
    caaRS256,
    caaRS384,
    caaRS512,
    caaPS256,
    caaPS384,
    caaPS512,
    caaEdDSA);

  /// set of supported asymmetric algorithms
  TCryptAsymAlgos = set of TCryptAsymAlgo;

  TCryptAbstractKey = class;

  /// abstract interface to a Public Key, as returned by CryptPublicKey[] factory
  ICryptPublicKey = interface
    /// unserialize a public key content
    // - this instance should be void, i.e. just created with no prior Load
    // - will first try the main PEM or DER usual serialization formats, then
    // from X.509 SubjectPublicKey raw binary, then as JWK format (RSA/EC)
    function Load(Algorithm: TCryptKeyAlgo;
      const PublicKeySaved: RawByteString): boolean;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(Algorithm: TCryptAsymAlgo;
      Data, Sig: pointer; DataLen, SigLen: integer): boolean; overload;
    /// verify the RSA or ECC signature of a memory buffer
    function Verify(Algorithm: TCryptAsymAlgo;
      const Data, Sig: RawByteString): boolean; overload;
    /// return raw key information as used by TCryptCert.GetKeyParams
    // - for ECC, returns the x,y coordinates
    // - for RSA, x is set to the Exponent (e), and y to the Modulus (n)
    // - return false if there is no compliant key information in the provider
    function GetParams(out x, y: RawByteString): boolean;
    /// use EciesSeal or RSA sealing, i.e. encryption with this public key
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
    /// the high-level asymmetric algorithm used for this public key
    function KeyAlgo: TCryptKeyAlgo;
    /// direct access to the class instance implementing this interface
    function Instance: TCryptAbstractKey;
  end;

  /// abstract interface to a Private Key, as returned by CryptPrivateKey[] factory
  ICryptPrivateKey = interface
    /// unserialized the private key from DER binary or PEM text
    // - this instance should be void, i.e. just created with no prior Load
    // - also ensure the private key do match an associated public key (if not nil)
    // - is able to decode and potentially decrypt a serialized key, with a
    // PKCS#8 Password for OpenSSL, and our proprietary PrivateKeyDecrypt()
    function Load(Algorithm: TCryptKeyAlgo; const AssociatedKey: ICryptPublicKey;
      const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean;
    /// create a new private / public key pair
    // - this instance should be void, i.e. just created with no prior Load
    // - returns the associated public key binary in X.509 SubjectPublicKey format
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString;
    /// return the private key as raw binary
    // - follow PKCS#8 PrivateKeyInfo encoding for RSA and prime256v1
    function ToDer: RawByteString;
    /// return the associated public key as stored in a X509 certificate
    function ToSubjectPublicKey: RawByteString;
    /// return the private key in the TCryptCertX509.Save expected format
    // - is able to encode and potentially encrypt a serialized key, with a
    // standard PKCS#8 Password (see PrivateKeySavePkcs8), or PrivateKeyEncrypt()
    function Save(AsPem: boolean; const Password: SpiUtf8): RawByteString;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(Algorithm: TCryptAsymAlgo;
      Data: pointer; DataLen: integer): RawByteString; overload;
    /// sign a memory buffer with RSA or ECC using the stored private key
    function Sign(Algorithm: TCryptAsymAlgo;
      const Data: RawByteString): RawByteString; overload;
    /// use EciesSeal or RSA un-sealing, i.e. decryption with this private key
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString;
    /// compute the shared-secret with another public key
    // - by design, ECDHE is only available for ECC
    function SharedSecret(const PeerKey: ICryptPublicKey): RawByteString;
    /// the high-level asymmetric algorithm used for this private key
    function KeyAlgo: TCryptKeyAlgo;
    /// direct access to the class instance implementing this interface
    function Instance: TCryptAbstractKey;
  end;

  /// abstract parent class to TCryptPublicKey and TCryptPrivateKey
  TCryptAbstractKey = class(TInterfacedPersistent)
  protected
    fKeyAlgo: TCryptKeyAlgo;
  public
    // ICryptPublicKey methods
    function KeyAlgo: TCryptKeyAlgo;
    function Instance: TCryptAbstractKey;
  end;

  /// abstract public key parent class, as returned by CryptPublicKey[] factory
  TCryptPublicKey = class(TCryptAbstractKey, ICryptPublicKey)
  protected
    /// verify the signature of a given hash using this public key
    function VerifyDigest(Sig: pointer; Dig: THash512Rec; SigLen, DigLen: integer;
      Hash: THashAlgo): boolean; virtual;
  public
    // ICryptPublicKey methods
    function Load(Algorithm: TCryptKeyAlgo;
      const PublicKeySaved: RawByteString): boolean; virtual; abstract;
    function Verify(Algorithm: TCryptAsymAlgo; Data, Sig: pointer;
      DataLen, SigLen: integer): boolean; overload; virtual;
    function Verify(Algorithm: TCryptAsymAlgo;
      const Data, Sig: RawByteString): boolean; overload;
    function GetParams(out x, y: RawByteString): boolean; virtual; abstract;
    function Seal(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
  end;

  /// abstract public key metaclass, as stored by the CryptPublicKey[] factory
  TCryptPublicKeyClass = class of TCryptPublicKey;

  /// abstract private key parent class, as returned by the CryptPrivateKey[] factory
  TCryptPrivateKey = class(TCryptAbstractKey, ICryptPrivateKey)
  protected
    /// default Load() will call PrivateKeyDecrypt() then FromDer()
    function FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
      pub: TCryptPublicKey): boolean; virtual;
    /// sign a memory buffer digest with the stored private key
    function SignDigest(const Dig: THash512Rec; DigLen: integer;
      DigAlgo: TCryptAsymAlgo): RawByteString; virtual;
  public
    // ICryptPrivateKey methods
    function Load(Algorithm: TCryptKeyAlgo; const AssociatedKey: ICryptPublicKey;
      const PrivateKeySaved: RawByteString; const Password: SpiUtf8): boolean; virtual;
    function Generate(Algorithm: TCryptAsymAlgo): RawByteString; virtual; abstract;
    function ToDer: RawByteString; virtual; abstract;
    function ToSubjectPublicKey: RawByteString; virtual; abstract;
    function Save(AsPem: boolean; const Password: SpiUtf8): RawByteString; virtual;
    function Sign(Algorithm: TCryptAsymAlgo;
      Data: pointer; DataLen: integer): RawByteString; overload; virtual;
    function Sign(Algorithm: TCryptAsymAlgo;
      const Data: RawByteString): RawByteString; overload;
    function Open(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function SharedSecret(
      const PeerKey: ICryptPublicKey): RawByteString; virtual;
  end;

  /// abstract public key metaclass class, as stored by the CryptPrivateKey factory
  TCryptPrivateKeyClass = class of TCryptPrivateKey;

  /// exception class raised by our High-Level Certificates Process
  ECryptCert = class(ESynException);

  /// the known Key Usages for a given Certificate
  // - is an exact match of TX509Usage enumerate in mormot.lib.openssl11.pas
  // and TWinCertUsage in mormot.lib.sspi
  // - usually stored as a 16-bit set
  TCryptCertUsage = (
    cuCA,
    cuEncipherOnly,
    cuCrlSign,
    cuKeyCertSign,
    cuKeyAgreement,
    cuDataEncipherment,
    cuKeyEncipherment,
    cuNonRepudiation,
    cuDigitalSignature,
    cuDecipherOnly,
    cuTlsServer,
    cuTlsClient,
    cuEmail,
    cuCodeSign,
    cuOcspSign,
    cuTimestamp);

  /// set of Key Usages for a given Certificate - stored as a 16-bit word
  // - stored as its own 16-bit value, with CU_ALL = 65535
  TCryptCertUsages = set of TCryptCertUsage;

  /// the RFC5280-compatible reasons why a Certificate could be revoked
  // - used for each item in a Certificate Revocation List (CRL)
  // - crrNotRevoked (item 7) is not used in the RFC, and used internally here
  TCryptCertRevocationReason = (
    crrUnspecified,            // (0)
    crrCompromised,            // (1)
    crrAuthorityCompromised,   // (2)
    crrUnAffiliated,           // (3)
    crrSuperseded,             // (4)
    crrReplaced,               // (5)
    crrTempHold,               // (6)
    crrNotRevoked,             // (7) - not used in the RFC, but used internally
    crrRemoved,                // (8)
    crrWithdrawn,              // (9)
    crrServerCompromised);     // (10)

  /// the Digital Signature results for a given Certificate
  // - is an exact match of TEccValidity enumerate in mormot.crypt.ecc256r1.pas
  // - see CV_VALIDSIGN constant for verification success
  TCryptCertValidity = (
    cvUnknown,                // (0)
    cvValidSigned,            // (1)
    cvValidSelfSigned,        // (2)
    cvNotSupported,           // (3)
    cvBadParameter,           // (4)
    cvCorrupted,              // (5)
    cvInvalidDate,            // (6)
    cvUnknownAuthority,       // (7)
    cvDeprecatedAuthority,    // (8)
    cvInvalidSignature,       // (9)
    cvRevoked,                // (10)
    cvWrongUsage,             // (11)
    cvExhaustedPathLen);      // (12)

  /// a set of Digital Signature results
  TCryptCertValidities = set of TCryptCertValidity;

  /// ICryptCert.Save possible output formats
  // - 'syn-es256' from mormot.crypt.ecc certificate will use its own proprietary
  // format, i.e. SaveToBinary/SaveToSecureBinary for ccfBinary, or non-standard
  // '-----BEGIN/END SYNECC CERTIFICATE-----' headers for ccfPem
  // - 'x509-rs256'..'x509-es256' from mormot.crypt.openssl will use the standard
  // x509 format, as DER (or PKCS12 if PrivatePassword is set) for ccfBinary,
  // or PEM for ccfPEM (concatenating the private key if PrivatePassword is set)
  // - ccfHexa, ccfBase64 and ccfBase64Uri will use the ccfBinary output, then
  // encode it as Hexadecimal or Base-64 (URI)
  TCryptCertFormat = (
    ccfBinary,
    ccfPem,
    ccfHexa,
    ccfBase64,
    ccfBase64Uri);

  /// used to store any kind of X.509 attributes or extensions as OID and Value
  // - e.g. in TCryptCertFields.CustomExts, TXTbsCertificate.ExtensionOther[],
  // TXname.Other[] or ICryptCert.GetExtensions
  TCryptCustomExt = record
    /// the OID of this value, in raw binary form - e.g. from AsnEncOid('1.2.3')
    Oid: RawByteString;
    /// the value associated with this OID
    // - as ASN1_OCTSTR raw content for TCryptCertFields.CustomExts,
    // TXTbsCertificate.ExtensionOther[] or ICryptCert.GetExtensions
    // - as RawUtf8 for TXname.Other[]
    Value: RawByteString;
    /// true if this extension is defined as critical
    Critical: boolean;
  end;
  PCryptCustomExt = ^TCryptCustomExt;

  /// used to store any kind of X.509 attributes or extensions as OID and Value
  // - e.g. in TCryptCertFields.CustomExts, TXTbsCertificate.ExtensionOther[],
  // TXname.Other[] or ICryptCert.GetExtensions
  TCryptCustomExts = array of TCryptCustomExt;

  /// convenient wrapper of X.509 Certificate subject name X.501 fields
  // - not always implemented - mainly our 'syn-es256' certificate won't
  // - as defined in RFC 5280 Appendix A.1
  TCryptCertFields = record
    /// countryName field (OID 2.5.4.6)
    Country: RawUtf8;
    /// stateOrProvinceName field (OID 2.5.4.8)
    State: RawUtf8;
    /// localityName field (OID 2.5.4.7)
    Locality: RawUtf8;
    /// organizationName field (OID 2.5.4.10)
    Organization: RawUtf8;
    /// organizationalUnitName field (OID 2.5.4.11)
    OrgUnit: RawUtf8;
    /// commonName field (OID 2.5.4.3)
    CommonName: RawUtf8;
    /// email field (OID 1.2.840.113549.1.9.1)
    EmailAddress: RawUtf8;
    /// surname field (OID 2.5.4.4)
    SurName: RawUtf8;
    /// givenName field (OID 2.5.4.42)
    GivenName: RawUtf8;
    /// serialNumber field (OID 2.5.4.5)
    // - note that is not the main X.509 certificate serial, but e.g. a Tax Number
    SerialNumber: RawUtf8;
    /// netscapeComment extension (not a name field - OID 2.16.840.1.113730.1.13)
    Comment: RawUtf8;
    /// set all subject name X.501 fields as RFC 4514 Distinguished Name (DN) text
    // - e.g. 'CN=John Doe+UID=123,O=Example\, Inc.,C=US' supporting multiple RDN
    // - other fields like Country/State/...SerialNumber would be ignored
    // - only currently supported by TXName.FromDNText from mormot.crypt.x509.pas
    DistinguishedName: RawUtf8;
    /// raw custom extensions (not name fields)
    // - use AddCustomExts() to fill it, or e.g. FindAia() to resolve OCSP/Issuers
    CustomExts: TCryptCustomExts;
  end;
  PCryptCertFields = ^TCryptCertFields;

  /// the ICryptCert.Load/Save content
  // - cccCertOnly will store the certificate as PEM or DER with its public key
  // - cccCertWithPrivateKey will include the private key to the output,
  // possibly protected by a password
  // - cccPrivateKeyOnly will export the raw private key with no password
  TCryptCertContent = (
    cccCertOnly,
    cccCertWithPrivateKey,
    cccPrivateKeyOnly);

  /// how ICryptCert.Compare() should compare two certificates
  // - ccmSubjectCN/ccmIssuerCN/ccmSubjectAltName/ccmIssuerAltName lookup is
  // case-insensitive
  TCryptCertComparer = (
    ccmInstance,
    ccmSerialNumber,
    ccmSubjectName,
    ccmIssuerName,
    ccmSubjectCN,
    ccmIssuerCN,
    ccmSubjectKey,
    ccmAuthorityKey,
    ccmSubjectAltName,
    ccmIssuerAltName,
    ccmUsage,
    ccmBinary,
    ccmSha1,
    ccmSha256,
    ccmIssuedBy);

const
  /// our units generate RSA keypairs with 2048-bit by default
  // - anything lower than 2048-bit is unsafe and should not be used
  // - 2048-bit is today's norm, creating 112-bit of security
  // - 3072-bit is supposed to be supported up to 2030, with 128-bit of security
  // - 4096-bit has no security advantage, just slower process
  // - 7680-bit or 8192-bit are highly impractical (e.g. generation can be more
  // than 30 secs) and offers only 192-bit of security
  // - see also OpenSslDefaultRsaBits() and RSA_INTERNAL_DEFAULT_GENERATION_BITS
  RSA_DEFAULT_GENERATION_BITS = 2048;

  /// the JWT algorithm names according to our known asymmetric algorithms
  // - as implemented e.g. by mormot.crypt.jwt
  CAA_JWT: array[TCryptAsymAlgo] of RawUtf8 = (
    'ES256',      // caaES256
    'ES384',      // caaES384
    'ES512',      // caaES512
    'ES256K',     // caaES256K
    'RS256',      // caaRS256
    'RS384',      // caaRS384
    'RS512',      // caaRS512
    'PS256',      // caaPS256
    'PS384',      // caaPS384
    'PS512',      // caaPS512
    'EdDSA');     // caaEdDSA

  /// the JWS ECC curve names according to our known asymmetric algorithms
  // - see https://www.iana.org/assignments/jose/jose.xhtml#web-key-elliptic-curve
  CAA_CRV: array[TCryptAsymAlgo] of RawUtf8 = (
    'P-256',     // caaES256
    'P-384',     // caaES384
    'P-521',     // caaES512, note that P-521 is not a typo ;)
    'secp256k1', // caaES256K
    '',          // caaRS256
    '',          // caaRS384
    '',          // caaRS512
    '',          // caaPS256
    '',          // caaPS384
    '',          // caaPS512
    'Ed25519');  // caaEdDSA

  /// the THashAlgo according to our known asymmetric algorithms
  CAA_HF: array[TCryptAsymAlgo] of THashAlgo = (
    hfSHA256,     // caaES256
    hfSHA384,     // caaES384
    hfSHA512,     // caaES512
    hfSHA256,     // caaES256K
    hfSHA256,     // caaRS256
    hfSHA384,     // caaRS384
    hfSHA512,     // caaRS512
    hfSHA256,     // caaPS256
    hfSHA384,     // caaPS384
    hfSHA512,     // caaPS512
    hfSHA512);    // caaEdDSA - SHA-512 is included in the algorithm

  /// the TCryptKeyAlgo according to our known asymmetric algorithms
  CAA_CKA: array[TCryptAsymAlgo] of TCryptKeyAlgo = (
    ckaEcc256,    // caaES256
    ckaEcc384,    // caaES384
    ckaEcc512,    // caaES512
    ckaEcc256K,   // caaES256K
    ckaRsa,       // caaRS256
    ckaRsa,       // caaRS384
    ckaRsa,       // caaRS512
    ckaRsaPss,    // caaPS256
    ckaRsaPss,    // caaPS384
    ckaRsaPss,    // caaPS512
    ckaEdDSA);    // caaEdDSA

  /// the known asymmetric algorithms which implement ECC cryptography
  CAA_ECC = [caaES256, caaES384, caaES512, caaES256K, caaEdDSA];

  /// the known asymmetric algorithms which implement RSA cryptography
  CAA_RSA = [caaRS256, caaRS384, caaRS512, caaPS256, caaPS384, caaPS512];

  /// the known asymmetric algorithms which implement RSA-PSS cryptography
  CAA_PSS = [caaPS256 .. caaPS512];

  /// the known asymmetric algorithms which expects no ASN1_SEQ in JWT/JWS
  CAA_RAWSIGNATURE = CAA_RSA + [caaEdDSA];

  /// the known key algorithms which implement ECC cryptography
  CKA_ECC = [ckaEcc256, ckaEcc384, ckaEcc512, ckaEcc256k, ckaEdDSA];

  /// the known key algorithms which implement RSA cryptography
  CKA_RSA = [ckaRsa, ckaRsaPss];

  /// such a Certificate could be used for anything
  CU_ALL = [low(TCryptCertUsage) .. high(TCryptCertUsage)];

  /// such a Certificate could be used for a TLS server authentication
  CU_TLS_SERVER = [cuTlsServer, cuKeyAgreement, cuKeyEncipherment];

  /// such a Certificate could be used for a TLS client authentication
  CU_TLS_CLIENT = [cuTlsClient, cuKeyAgreement, cuKeyEncipherment];

  /// KeyUsage bits mapped in TCryptCertUsages
  CU_KEY_USAGE = [cuEncipherOnly .. cuDecipherOnly];

  /// Extended KeyUsage bits mapped in TCryptCertUsages
  CU_EXT_KEY_USAGE = [cuTlsServer .. cuTimestamp];

  /// TCryptCertValidity results indicating a valid digital signature
  CV_VALIDSIGN = [cvValidSigned, cvValidSelfSigned];

  /// a two-char identifier of Certificate usage
  // - as used by ToText(u: TCryptCertUsages, from_cu_text=true)
  CU_TEXT: array[TCryptCertUsage, 0..1] of AnsiChar = (
    'ca',  //  cuCA
    'eo',  //  cuEncipherOnly
    'rs',  //  cuCrlSign
    'ks',  //  cuKeyCertSign
    'ka',  //  cuKeyAgreement
    'de',  //  cuDataEncipherment
    'ke',  //  cuKeyEncipherment
    'nr',  //  cuNonRepudiation
    'ds',  //  cuDigitalSignature
    'do',  //  cuDecipherOnly
    'ts',  //  cuTlsServer
    'tc',  //  cuTlsClient
    'em',  //  cuEmail
    'cs',  //  cuCodeSign
    'os',  //  cuOcspSign
    'tm'); //  cuTimestamp

  /// standard long identifier of Certificate usage
  // - i.e. match OpenSSL PX509.ExtendedKeyUsage/KeyUsage text
  CU_FULLTEXT: array[TCryptCertUsage] of RawUtf8 = (
    'CA',                            // cuCA
    'Encipher Only',                 // cuEncipherOnly
    'CRL Sign',                      // cuCrlSign
    'Certificate Sign',              // cuKeyCertSign
    'Key Agreement',                 // cuKeyAgreement
    'Data Encipherment',             // cuDataEncipherment
    'Key Encipherment',              // cuKeyEncipherment
    'Non Repudiation',               // cuNonRepudiation
    'Digital Signature',             // cuDigitalSignature
    'Decipher Only',                 // cuDecipherOnly
    'TLS Web Server Authentication', // cuTlsServer
    'TLS Web Client Authentication', // cuTlsClient
    'E-mail Protection',             // cuEmail
    'Code Signing',                  // cuCodeSign
    'OCSP Signing',                  // cuOcspSign
    'Time Stamping');                // cuTimestamp

function ToText(a: TCryptAsymAlgo): PShortString; overload;
function ToText(a: TCryptKeyAlgo): PShortString; overload;
function ToText(r: TCryptCertRevocationReason): PShortString; overload;
function ToText(u: TCryptCertUsage): PShortString; overload;
function ToText(u: TCryptCertUsages; from_cu_text: boolean = false): ShortString; overload;
function ToText(v: TCryptCertValidity): PShortString; overload;

/// return the first usage set, or cuKeyCertSign if [] was supplied
function GetFirstUsage(u: TCryptCertUsages): TCryptCertUsage;

/// check for one KeyUsage bit presence - if any is set (see RFC 5280 6.1.4)
// - one is typically cuKeyCertSign, cuCrlSign or cuDigitalSignature
function HasCertUsage(const one: TCryptCertUsage; const usages: TCryptCertUsages): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// check both cA=TRUE and keyCertSign - RFC 5280 6.1.4 compliant
function IsCertAuthority(usages: TCryptCertUsages): boolean;

type
  TCryptCert = class;
  TCryptCertAlgo = class;

  /// abstract interface to a Certificate, as returned by Cert() factory
  // - may be X.509 or not, OpenSSL implemented or not, e.g. for syn-es256
  // - note: features and serialization are not fully compatible between engines,
  // but those high-level methods work as expected within each TCryptCertAlgo
  ICryptCert = interface
    /// create a new Certificate instance with its genuine private key
    // - Subjects is given as a CSV text, e.g. 'synopse.info,www.synopse.info'
    // - if Authority is nil, will generate a self-signed certificate, otherwise
    // will use this Authority private key to sign the certificate
    // - ValidDays and ExpireDays are relative to the current time - ValidDays
    // is -1 by default to avoid most clock synch issues
    // - additional information can be passed into Fields (e.g. common name)
    // - return self to be used as a fluent interface, e.g. calling
    // Save(cccPrivateKeyOnly) to persist the newly created private key
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8 = '';
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil): ICryptCert;
    /// create a new Certificate instance from a supplied CSR
    // - will first unserialize and verify a self-signed CSR (PEM) content, e.g.
    // as generated by a former TCryptCertAlgo.CreateSelfSignedCsr() call
    // - retrieve the Subjects and Usages as previously set to the CSR
    // - expect either a self-signed certificate as CSR, or a PKCS#10 CSR
    // by the X.509 engines (OpenSSL or mormot.crypt.x509)
    // - if Authority is set, it will sign the certificate, and keep the public
    // key stored in the CSR so that the requester could keep its private key
    // - if Authority is not set, a new key pair is generated to self-sign
    // the certificate, and the public key is stored in the certificate, and
    // the generated keys (public and private) can be retrieved using Save
    // - return self to be used as a fluent interface, or nil if unsupported
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1): ICryptCert;
    /// the Certificate Genuine Serial Number
    // - e.g. '04:f9:25:39:39:f8:ce:79:1a:a4:0e:b3:fa:72:e3:bc:9e:d6'
    function GetSerial: RawUtf8;
    /// the High-Level Certificate Subject
    // - actual text output depend on the provider used; for instance
    // '/C=US/O=Let''s Encrypt/CN=R3' with OpenSSL, 'CN=R3, C=US, O=Let''s Encrypt'
    // with mormot.crypt.x509 or some Baudot-encoded text with mormot.crypt.ecc
    function GetSubjectName: RawUtf8;
    /// the Low-Level Certificate Main Subject
    // - returns by default the CommonName, e.g. 'synopse.info' from a X.509
    // CN= subject field
    // - can search another Relative Distinguished Name (RDN) e.g. 'O' or 'OU'
    // - if Rdn is a hash, e.g. 'SHA1'/'SHA256', will return the subject digest
    // - if Rdn is 'DER', will return the raw DER issuer value of this subject
    function GetSubject(const Rdn: RawUtf8 = 'CN'): RawUtf8;
    /// an array of all Subject names covered by this Certificate
    // - e.g. ['synopse.info', 'www.synopse.info']
    // - e.g. read from X.509 v3 Subject Alternative Names extension
    function GetSubjects: TRawUtf8DynArray;
    /// the High-Level Certificate Issuer
    // - actual text output depend on the provider used; for instance
    // '/C=US/O=Let''s Encrypt/CN=R3' with OpenSSL, 'CN=R3, C=US, O=Let''s Encrypt'
    // with mormot.crypt.x509 or some Baudot-encoded text with mormot.crypt.ecc
    function GetIssuerName: RawUtf8;
    /// the Low-Level Certificate Main Issuer
    // - returns by default the CommonName, e.g. 'R3' from a X.509 CN= subject field
    // - can search another Relative Distinguished Name (RDN) e.g. 'O' or 'OU'
    // - if Rdn is a hash, e.g. 'SHA1'/'SHA256', will return the issuer digest
    // - if Rdn is 'DER', will return the raw DER issuer value of this certificate
    function GetIssuer(const Rdn: RawUtf8 = 'CN'): RawUtf8;
    /// an array of all Subject names covered by the issuer of this Certificate
    // - e.g. read from X.509 v3 Issuer Alternative Names extension
    function GetIssuers: TRawUtf8DynArray;
    /// the Subject Key Identifier (SKID) of this Certificate
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    // - match the SKID on X.509, or the serial number for syn-es256
    function GetSubjectKey: RawUtf8;
    /// the signing Authority Key Identifier (AKID) of this Certificate
    // - match the AKID on X.509 (so may be '' for a self-signed certificate),
    // or the authority serial number for syn-es256 (so equals GetSubjectKey
    // for a self-signed certificate)
    function GetAuthorityKey: RawUtf8;
    /// fill TCryptCertFields information with internal certificates attributes
    // - returns false if those fields are not supported e.g. for syn-es256
    function GetFields(var fields: TCryptCertFields; withexts: boolean = false): boolean;
    /// check if this certificate has been self-signed
    function IsSelfSigned: boolean;
    /// check if this certificate has been issued by the specified certificate
    // - e.g. on X.509 will efficiently check the certificate AKID with the
    // Authority SKID
    function IsAuthorizedBy(const Authority: ICryptCert): boolean;
    /// compare one Certificate instance with another
    function Compare(const Another: ICryptCert; Method: TCryptCertComparer): integer;
    /// compare two Certificates, which should share the same algorithm
    // - will compare the internal properties and the public key, not the
    // private key: you could e.g. use it to verify that a ICryptCert with
    // HasPrivateSecret=false matches another with HasPrivateSecret=true
    function IsEqual(const Another: ICryptCert): boolean;
    /// the minimum Validity timestamp of this Certificate
    function GetNotBefore: TDateTime;
    /// the maximum Validity timestamp of this Certificate
    function GetNotAfter: TDateTime;
    /// check GetNotBefore/GetNotAfter validity
    // - validate against current UTC date/time if none is specified
    // - a grace period of CERT_DEPRECATION_THRESHOLD (half a day) is applied
    function IsValidDate(date: TDateTime = 0): boolean;
    /// returns true e.g. after TCryptCertAlgo.New but before Generate()
    function IsVoid: boolean;
    /// the Key Usages of this Certificate
    // - could optionally return the pathLenConstraint limit as 32-bit integer
    function GetUsage(PathLen: PInteger = nil): TCryptCertUsages;
    /// verbose Certificate information, returned as huge text/JSON blob
    function GetPeerInfo: RawUtf8;
    /// the signature algorithm as engine-specific plain text
    // - the first value is the effective security bits of this algorithm
    // - e.g. '128 ecdsa-with-SHA256', '128 RSA-SHA256' or '128 ED25519' on
    // OpenSSL, or '128 syn-es256' for our cryptography
    function GetSignatureInfo: RawUtf8;
    /// compute the hexadecimal fingerprint of this Certificate
    // - is usually the hash of its binary (e.g. DER) serialization
    function GetDigest(Algo: THashAlgo = hfSHA256): RawUtf8;
    /// load a Certificate from a Save() content
    // - use Content to specify the extent of the loaded value
    // - PrivatePassword is used for cccCertWithPrivateKey and cccPrivateKeyOnly
    // - warning: don't forget FillZero() once done with any sensitive input
    function Load(const Saved: RawByteString;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): boolean;
    /// load a Certificate from a SaveToFile() content
    // - just a wrapper around the Load() method, reading a file from disk
    // and setting the GetFileName method result value
    function LoadFromFile(const Source: TFileName;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): boolean;
    /// the last Source file name of LoadFromFile()
    function GetFileName: TFileName;
    /// serialize the Certificate as reusable content
    // - use Content to specify the extent of the returned value; e.g. after
    // Generate, this ICryptCert instance will contain both the public and
    // private key, so cccCertWithPrivateKey and cccPrivateKeyOnly content could
    // be used, with an optional PrivatePassword, to save the private key
    // - will use binary by default, but you can set e.g. ccfPem if needed
    // - note that OpenSSL ccfBinary will use PKCS12 encoding, which default
    // encoding did change with OpenSSL 3.X: you may start the PrivatePassword
    // with PKCS12_3DES_PREFIX '3des=' prefix (which will be trimmed) to force
    // legacy PBE-SHA1-3DES as compatible with Windows Server 2012 or MacOS/iOS,
    // or start with PKCS12_AES_PREFIX 'aes=' prefix to force the newer (and
    // safer) AES-256-CBC algorithm on OpenSSL 1.x
    // - warning: don't forget FillZero() once done with any sensitive result
    function Save(Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = '';
      Format: TCryptCertFormat = ccfBinary): RawByteString;
    /// serialize the Certificate as reusable file content
    // - just a wrapper to store the Save() method result as a file
    // - if Dest is '' then GetFileName value from last LoadFromFile() is used
    procedure SaveToFile(const Dest: TFileName = '';
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = '';
      Format: TCryptCertFormat = ccfBinary);
    /// compute a digital signature of some digital content
    // - memory buffer will be hashed then signed using the private secret key
    // of this certificate instance
    // - you could later verify this text signature according to the public key
    // of this certificate, using ICryptCert.Verify() or ICryptStore.Verify()
    // - this certificate should have the cuDigitalSignature usage
    // - returns '' on failure, e.g. if this Certificate has no private key
    // - returns the binary signature of the Data buffer on success
    function Sign(Data: pointer; Len: integer;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString; overload;
    /// compute a digital signature of some digital content
    // - will use the private key of this certificate
    // - just a wrapper around the overloaded Sign() function
    function Sign(const Data: RawByteString;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString; overload;
    /// sign this certificate with the private key of one CA
    // - Authority certificate should have the cuKeyCertSign usage
    procedure Sign(const Authority: ICryptCert); overload;
    /// verify a digital signature of some digital content
    // - will use the public key of this certificate
    // - this certificate should have the cuDigitalSignature usage
    // - see ICryptStore.Verify() for a complete CA chain validation
    // - depending on the engine, some errors can be ignored, e.g.
    // cvWrongUsage and cvDeprecatedAuthority with X.509 certificates
    // - certificate expiration date can be specified instead of current time
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify a digital signature of some digital content
    // - just a wrapper around the overloaded Verify() function
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify another certificate signature with this certificate public key
    // (if self-signed), or a supplied Authority reference
    // - Authority certificate should have the cuKeyCertSign usage
    // - mormot.crypt.x509 will cache the last valid Authority for fast process
    function Verify(const Authority: ICryptCert;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// compute a new JWT for a given payload using this certificate private key
    // - will use the private key and Sign() to compute the signature
    // - this certificate should have the cuDigitalSignature usage
    // - same signature than the reusable TJwtAbstract.Compute() method
    // - returns '' on error, e.g. if HasPrivateSecret is false
    function JwtCompute(const DataNameValue: array of const;
      const Issuer: RawUtf8 = ''; const Subject: RawUtf8 = '';
      const Audience: RawUtf8 = ''; NotBefore: TDateTime = 0;
      ExpirationMinutes: integer = 0; Signature: PRawByteString = nil): RawUtf8;
    /// verify a JWT signature from the public key of this certificate
    // - this certificate should have the cuDigitalSignature usage
    // - can optionally return the payload fields and/or the signature
    function JwtVerify(const Jwt: RawUtf8; Issuer, Subject, Audience: PRawUtf8;
      Payload: PDocVariantData = nil; Signature: PRawByteString = nil;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity;
    /// returns the JSON Web Key (JWT) corresponding to the public key of this
    // certificate
    // - the returned JWK is computed with no whitespace or line breaks before
    // or after any syntaxic elements, and the required members are ordered
    // lexicographically, as expected for a direct thumbprint
    // - typical pattern is '{"crv":..,"kty":"EC","x":..,"y":.. }' for ECC
    // or '{"e":..,"kty":"RSA","n":..}' for RSA
    // - is implemented by default as a wrapper to GetKeyParams() results
    function JwkCompute: RawUtf8;
    /// encrypt a message using the public key of this certificate
    // - only RSA and ES256 algorithms do support this method by now
    // - 'x509-rs*' and 'x509-ps*' RSA algorithms use an OpenSSL Envelope key
    // transport then our EVP_PKEY.RsaSeal encoding (or TRsa.Seal)
    // - both 'x509-es256' and 'syn-es256' use our EciesSeal() ES256 encoding
    // - returns '' if this feature is not supported
    // - certificate should have cuDataEncipherment or cuEncipherOnly usage
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString;
    /// decrypt a message using the private key of this certificate
    // - not all algorithms support key transport, only RSA and ES256 by now
    // - 'x509-rs*' and 'x509-ps*' RSA algorithms use an OpenSSL Envelope key
    // transport then our EVP_PKEY.RsaOpen decoding (or TRsa.Open)
    // - both 'x509-es256' and 'syn-es256' use our EciesOpen() ES256 decoding
    // - returns '' if this feature is not supported, or Message is incorrect
    // - certificate should have cuDataEncipherment or cuDecipherOnly usage
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8 = 'aes-128-ctr'): RawByteString;
    /// compute a shared secret from the private key of this certificate and the
    // public key of another certificate
    // - used e.g. to initialize network encryption with no key transmission
    // - returns '' if this algorithm doesn't support this feature (e.g. RSA)
    // - both current and pub certificates should have cuKeyAgreement usage
    // - the caller should always apply a cryptographic hash over the result
    function SharedSecret(const pub: ICryptCert): RawByteString;
    /// returns true if this ICryptCert instance holds a private key secret
    function HasPrivateSecret: boolean;
    /// retrieve the public key as raw binary
    // - actual format depend on the TCryptCert class and algorithm involved,
    // but is usually using a DER format
    function GetPublicKey: RawByteString;
    /// retrieve the private key as raw binary, or '' if none
    // - actual format depend on the TCryptCert class and algorithm involved,
    // but is usually using a DER format
    // - warning: don't forget FillZero() once done with this sensitive result
    function GetPrivateKey: RawByteString;
    /// include the raw private key as saved by GetPrivateKey
    // - the private key should match with the public key of the Certificate
    // - any previously stored private key will first be erased, therefore
    // SetPrivateKey('') will wipe any private key currently stored in memory
    // - warning: don't forget FillZero() once done with this sensitive input
    function SetPrivateKey(const saved: RawByteString): boolean;
    /// the high-level asymmetric algorithm used for this certificate
    function AsymAlgo: TCryptAsymAlgo;
    /// the high-level asymmetric algorithm class used for this certificate
    // - i.e. the factory associated with this ICryptCert instance
    function CertAlgo: TCryptCertAlgo;
    /// access to the low-level implementation class instance
    // - used internally to quickly retrieve the TCryptCert from an ICryptCert
    function Instance: TCryptCert;
    /// access to the low-level implementation handle of the certificate
    // - e.g. a PX509 for OpenSsl, a TEccCertificate class for mormot.crypt.ecc,
    // or a TX509 class for mormot.crypt.x509
    // - equals nil if there is no associated certificate yet, e.g. after New
    // - can be assigned e.g. to TNetTlsContext.CertificateRaw for OpenSSL
    function Handle: pointer;
    /// access to the low-level implementation handle of the stored private key
    // - e.g. a PEVP_PKEY for OpenSsl, a PEccPrivateKey for mormot.crypt.ecc,
    // or a ICryptPrivateKey weak reference for mormot.crypt.x509
    // - equals nil if there is no associated private key
    // - can be assigned e.g. to TNetTlsContext.PrivateKeyRaw for OpenSSL
    function PrivateKeyHandle: pointer;
    /// return the public BigInt values associated to the stored private key
    // - as BigInt binaries, ready e.g. for JWS / JSON Web Key responses
    // - for ECC, returns the x,y coordinates
    // - for RSA, x is set to the Exponent (e), and y to the Modulus (n)
    // - return false if there is no compliant key information in the provider
    function GetKeyParams(out x, y: RawByteString): boolean;
    /// returns the raw X.509 extensions as binary OID and Value pairs
    function GetExtensions: TCryptCustomExts;
  end;

  /// a dynamic array of Certificate interface instances
  ICryptCerts = array of ICryptCert;
  /// holds a Certificate chain, the first being the main certificate
  ICryptCertChain = ICryptCerts;
  /// a pointer to a Certificate interface instance
  PICryptCert = ^ICryptCert;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  // - you should never use this class, but the ICryptCert instances
  // - type is only defined here to be inherited with the actual provider units
  TCryptCert = class(TCryptInstance, ICryptCert)
  protected
    fLastLoadFromFileName: TFileName;
    fIndexer: TObject; // a TCryptCertAbstractList owner for EnsureCanWrite
    procedure RaiseError(const Msg: ShortString); overload; virtual;
    procedure RaiseError(const Fmt: RawUtf8; const Args: array of const); overload;
    procedure RaiseErrorGenerate(const api: ShortString);
    procedure EnsureCanWrite(const Context: ShortString); virtual;
    // used by TCryptCertList.Find and TCryptCertCache.Find
    class procedure InternalFind(Cert: PICryptCert; const Value: RawByteString;
      Method: TCryptCertComparer; Count, MaxCount: integer;
      out Chain: ICryptCerts); virtual;
  public
    // ICryptCert methods
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer;
      Fields: PCryptCertFields): ICryptCert; virtual; abstract;
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert; virtual;
    function GetSerial: RawUtf8; virtual; abstract;
    function GetSubjectName: RawUtf8; virtual; abstract;
    function GetSubject(const Rdn: RawUtf8): RawUtf8; virtual; abstract;
    function GetSubjects: TRawUtf8DynArray; virtual; abstract;
    function GetIssuerName: RawUtf8; virtual; abstract;
    function GetIssuer(const Rdn: RawUtf8): RawUtf8; virtual; abstract;
    function GetIssuers: TRawUtf8DynArray; virtual; abstract;
    function GetSubjectKey: RawUtf8; virtual; abstract;
    function GetAuthorityKey: RawUtf8; virtual; abstract;
    function GetFields(var fields: TCryptCertFields; withexts: boolean): boolean; virtual;
    function IsSelfSigned: boolean; virtual; abstract;
    function IsAuthorizedBy(const Authority: ICryptCert): boolean; virtual;
    function Compare(const Another: ICryptCert; Method: TCryptCertComparer): integer; virtual;
    function IsEqual(const Another: ICryptCert): boolean; virtual;
    function GetNotBefore: TDateTime; virtual; abstract;
    function GetNotAfter: TDateTime; virtual; abstract;
    function IsValidDate(date: TDateTime): boolean; virtual;
    function IsVoid: boolean; virtual;
    function GetUsage(PathLen: PInteger = nil): TCryptCertUsages; virtual; abstract;
    function GetPeerInfo: RawUtf8; virtual; abstract;
    function GetSignatureInfo: RawUtf8; virtual; abstract;
    function GetDigest(Algo: THashAlgo): RawUtf8; virtual;
    function Load(const Saved: RawByteString; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; virtual; abstract;
    function LoadFromFile(const Source: TFileName; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8): boolean; virtual;
    function GetFileName: TFileName; virtual;
    function Save(Content: TCryptCertContent; const PrivatePassword: SpiUtf8;
      Format: TCryptCertFormat): RawByteString; virtual;
    procedure SaveToFile(const Dest: TFileName; Content: TCryptCertContent;
      const PrivatePassword: SpiUtf8; Format: TCryptCertFormat);
    function HasPrivateSecret: boolean; virtual; abstract;
    function GetPublicKey: RawByteString; virtual; abstract;
    function GetPrivateKey: RawByteString; virtual; abstract;
    function SetPrivateKey(const saved: RawByteString): boolean; virtual; abstract;
    function Sign(Data: pointer; Len: integer; Usage: TCryptCertUsage): RawByteString;
      overload; virtual; abstract;
    function Sign(const Data: RawByteString;
      Usage: TCryptCertUsage = cuDigitalSignature): RawByteString;
      overload; virtual;
    procedure Sign(const Authority: ICryptCert); overload; virtual; abstract;
    function Verify(Sign, Data: pointer; SignLen, DataLen: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual; abstract;
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual;
    function Verify(const Authority: ICryptCert;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
      overload; virtual; abstract;
    function JwtCompute(const DataNameValue: array of const;
      const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
      ExpirationMinutes: integer; Signature: PRawByteString): RawUtf8; virtual;
    function JwtVerify(const Jwt: RawUtf8; Issuer, Subject, Audience: PRawUtf8;
      Payload: PDocVariantData; Signature: PRawByteString;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity; virtual;
    function JwkCompute: RawUtf8; virtual;
    function Encrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function Decrypt(const Message: RawByteString;
      const Cipher: RawUtf8): RawByteString; virtual; abstract;
    function SharedSecret(const pub: ICryptCert): RawByteString; virtual;
    function AsymAlgo: TCryptAsymAlgo; virtual;
    function CertAlgo: TCryptCertAlgo; virtual;
    function Instance: TCryptCert;
    function Handle: pointer; virtual; abstract;
    function PrivateKeyHandle: pointer; virtual;
    function GetKeyParams(out x, y: RawByteString): boolean; virtual;
    function GetExtensions: TCryptCustomExts; virtual;
  end;

  /// meta-class of the abstract parent to implement ICryptCert interface
  TCryptCertClass = class of TCryptCert;

  /// abstract parent class for ICryptCert factories
  TCryptCertAlgo = class(TCryptAlgo)
  protected
    fCaa: TCryptAsymAlgo; // should be set by the overriden constructor
  public
    /// main factory to create a new Certificate instance with this algorithm
    // - return a new void instance, ready to call e.g. ICryptCert.Load
    // - return nil by design e.g. for TCryptCertAlgoCng or TCryptCertAlgoPkcs11
    function New: ICryptCert; virtual;
    /// low-level factory directly from the raw implementation handle
    // - e.g. a PX509 for OpenSsl, or TEccCertificate for mormot.crypt.ecc
    // - warning: ensure Handle is of the expected type, otherwise it will GPF
    // - includes the private key for TEccCertificate, or not for OpenSsl
    // - return nil by design e.g. for TCryptCertAlgoCng or TCryptCertAlgoPkcs11
    // - note that this Handle will be owned (and eventually released) by the
    // new ICryptCert instance, so you should not make FromHandle(another.Handle)
    function FromHandle(Handle: pointer): ICryptCert; virtual;
    /// factory to load a Certificate from a ICryptCert.Save() content
    // - PrivatePassword is needed if the input contains a private key
    // - will only recognize and support the ccfBinary and ccfPem formats
    // - return nil if Saved content was not in the expected format/algorithm
    function Load(const Saved: RawByteString;
      Content: TCryptCertContent = cccCertOnly;
      const PrivatePassword: SpiUtf8 = ''): ICryptCert;
    /// factory to generate a new Certificate instance
    // - just a wrapper around New and ICryptCert.Generate()
    // - Subjects is a mandatory field as with X.509
    function Generate(Usages: TCryptCertUsages; const Subjects: RawUtf8;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1; Fields: PCryptCertFields = nil): ICryptCert;
    /// factory for a new Certificate Signing Request over a set of (DNS) names
    // - if PrivateKeyPem is void, will generate a new public/private key pair,
    // then forge a request with this new public key
    // - if PrivateKeyPem is supplied, will use it as public key
    // - you can optionally specify the expected usages and information fields
    // - returns both the private key and the self-signed CSR as PEM
    // - by default, this class returns a self-signed certificate as CSR, but
    // will be overriden by our X.509 engines (OpenSSL and mormot.crypt.x509) to
    // return a proper PKCS#10 standard CSR, in a Let's Encrypt compatible way
    // - return '' by design e.g. for TCryptCertAlgoCng or TCryptCertAlgoPkcs11
    function CreateSelfSignedCsr(const Subjects: RawUtf8;
      const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
      Usages: TCryptCertUsages = [];
      Fields: PCryptCertFields = nil): RawUtf8; virtual;
    /// factory to generate a new Certificate instance from a supplied CSR
    // - will first unserialize and verify a self-self CSR PEM content, as
    // generated by a former CreateSelfSignedCsr() call
    // - retrieve the Subjects and Usages as previously set to the CSR, enabling
    // only UsagesFilter items if set
    // - this default implementation expects the CSR to be a self-signed certificate
    function GenerateFromCsr(const Csr: RawByteString;
      const Authority: ICryptCert = nil; ExpireDays: integer = 365;
      ValidDays: integer = -1): ICryptCert; virtual;
    /// return the corresponding JWT algorithm name, computed from AsymAlgo
    // - e.g. 'ES256' for 'x509-es256' or 'syn-es256-v1'
    function JwtName: RawUtf8;
  published
    /// the asymmetric algorithm used for these certificates
    property AsymAlgo: TCryptAsymAlgo
      read fCaa;
  end;

  /// define the available source of trust for ICryptStore.IsValid/IsValidChain
  // - cstOsRoot to lookup trusted roots from the OS
  // - cstOsIntermediate to lookup OS intermediate CA cache/store
  // - cstAiaIntermediate retrieve missing intermediates via AIA web requests
  TCertStoreTrust = (
    cstOsRoot,
    cstOsIntermediate,
    cstAiaIntermediate
  );
  /// define the source of trust for ICryptStore.IsValid/IsValidChain
  // - [] would only search for certificates within the ICryptStore internal list
  // - you could allow certificate lookup from OS or remote AIA sources
  TCertStoreTrusts = set of TCertStoreTrust;

  TCryptCertCache = class;

  /// abstract interface to a Certificates Store, as returned by Store() factory
  // - may be X.509 or not, OpenSSL implemented or not
  ICryptStore = interface
    procedure SetTrust(const Value: TCertStoreTrusts);
    function GetTrust: TCertStoreTrusts;
    /// delete all stored Certificates or CRL information
    procedure Clear;
    /// load a Certificates Store from a ICryptStore.Save memory buffer content
    function Load(const Saved: RawByteString): boolean;
    /// serialize the Certificates Store into a memory buffer
    // - may be our TEccCertificateChain proprietary binary, or a chain of
    // X.509 Certificates and CRLs in PEM text format
    function Save: RawByteString;
    /// get the associated ICryptCert instances cache
    // - use Cache.Load() to retrieve a ICryptCert from its DER/PEM content
    function Cache: TCryptCertCache;
    /// search for a trusted certificate from its (hexadecimal) identifier
    // - note that in the X.509 context, serial may be duplicated, so
    // it is safer to use GetBySubjectKey()
    function GetBySerial(const Serial: RawUtf8): ICryptCert;
    /// search for a trusted certificate from its (hexadecimal) Subject Key Identifier
    // - e.g. '14:2E:B3:17:B7:58:56:CB:AE:50:09:40:E6:1F:AF:9D:8B:14:C2:C6'
    // - search the SKID on X.509, or the serial number for syn-es256
    function GetBySubjectKey(const Key: RawUtf8): ICryptCert;
    /// search for a trusted certificate from a given attribute
    // - return the first certificate matching a given value
    // - warning: some engines don't support this advanced search feature
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer): ICryptCert;
    /// quickly check if a given certificate is part of the internal CRL
    // - returns crrNotRevoked is the serial is not known as part of the CRL
    // - returns the reason why this certificate has been revoked otherwise
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason;
    /// register a certificate in the internal certificate chain
    // - returns false e.g. if the certificate was not valid, or its serial was
    // already part of the internal list, or not of a compatible class
    // - self-signed certificates could be included - but add them with caution
    // because they will become root CA, or "trust anchors" in X.509 terminology
    // - the Certificate should have cuCA or cuKeyCertSign typical usages
    function Add(const cert: ICryptCert): boolean; overload;
    /// register several certificates in the internal certificate chain
    // - returns the serials of added certificate(s)
    function Add(const cert: array of ICryptCert): TRawUtf8DynArray; overload;
    /// load and register a certificate or certificate chain from a memory buffer
    // - returns the serials of added certificate(s)
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray;
    /// load and register a certificate file or file chain
    // - returns the serials of added certificate(s)
    // - the Certificate(s) should have cuCA or cuKeyCertSign typical usages
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray;
    /// search and register all certificate files from a given folder
    // - returns the serials of added certificate(s)
    // - the Certificate(s) should have cuCA or cuKeyCertSign typical usages
    // - if there are any valid CRL, they will also be loaded to the store
    function AddFromFolder(const Folder: TFileName;
      const Mask: TFileName = FILES_ALL; Recursive: boolean = false): TRawUtf8DynArray;
    /// add a Certificate information to the global Certificate Revocation List
    // - on some engines (our internal ECC, but not OpenSSL), Reason=crrNotRevoked
    // could be used to unregister a certificate revocation
    function Revoke(const Cert: ICryptCert; Reason: TCryptCertRevocationReason;
      RevocationDate: TDateTime = 0): boolean;
    /// check if the certificate is valid, against known certificates chain
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored digital signature according to the public key of
    // the associated signing authority, as found within the store, for as
    // many level as needed until a self-signed "root anchor" is reached
    function IsValid(const cert: ICryptCert;
      date: TDateTime = 0): TCryptCertValidity;
    /// check a certificate against its supplied chain and known certificates
    // - with a large PKI as on the Internet, a certificates chain is usually
    // supplied for authentication with only some "trust anchors" certificates
    // - this overloaded method accept a chain as input, so that the first
    // item is to be validated against the other members of the chain as
    // intermediates (not part of the store), then eventually validating the
    // last items of the chain with the store trusted certificates
    function IsValidChain(const chain: ICryptCertChain;
      date: TDateTime = 0; ignore: TCryptCertValidities = []): TCryptCertValidity;
    /// verify the digital signature of a given memory buffer
    // - this signature should have come from a previous ICryptCert.Sign() call
    // - will check internal properties of the certificate (e.g. validity dates),
    // and validate the stored signature according to the public key of
    // the associated signing authority (which should be in this Store)
    // - warning: only supported by our 'syn-store' algorithm: OpenSSL Store
    // has no way to lookup the X.509 certificate which actually signed the buffer
    function Verify(const Signature: RawByteString; Data: pointer; Len: integer;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// verify the digital signature of a given memory string
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    /// how many trusted certificates are currently stored
    function Count: integer;
    /// how many CRLs are currently stored
    function CrlCount: integer;
    /// return the preferred algo to be used with this store
    // - call e.g. CertAlgo.New to prepare a new ICryptCert to add to this store
    function DefaultCertAlgo: TCryptCertAlgo;
    /// customize the source of trust for IsValid/IsValidChain of this instance
    // - if filled with global DefaultCryptStoreTrust variable by default
    // - only supported by TCryptStore
    property Trust: TCertStoreTrusts
      read GetTrust write SetTrust;
  end;

  /// abstract parent class to implement ICryptCert, as returned by Cert() factory
  TCryptStore = class(TCryptInstance, ICryptStore)
  protected
    fCache: TCryptCertCache;
    fTrust: TCertStoreTrusts;
    procedure SetTrust(const Value: TCertStoreTrusts); virtual;
    function GetTrust: TCertStoreTrusts;
  public
    constructor Create(algo: TCryptAlgo); override;
    destructor Destroy; override;
    // ICryptStore methods
    procedure Clear; virtual; abstract;
    function Load(const Saved: RawByteString): boolean; virtual;
    function Save: RawByteString; virtual; abstract;
    function Cache: TCryptCertCache; virtual;
    function GetBySerial(const Serial: RawUtf8): ICryptCert; virtual; abstract;
    function GetBySubjectKey(const Key: RawUtf8): ICryptCert; virtual; abstract;
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer): ICryptCert; virtual;
    function IsRevoked(const cert: ICryptCert): TCryptCertRevocationReason; virtual; abstract;
    function Add(const cert: ICryptCert): boolean; overload; virtual; abstract;
    function Add(const cert: array of ICryptCert): TRawUtf8DynArray; overload; virtual;
    function AddFromBuffer(const Content: RawByteString): TRawUtf8DynArray; virtual; abstract;
    function AddFromFile(const FileName: TFileName): TRawUtf8DynArray; virtual;
    function AddFromFolder(const Folder, Mask: TFileName;
       Recursive: boolean): TRawUtf8DynArray; virtual;
    function Revoke(const Cert: ICryptCert; Reason: TCryptCertRevocationReason;
       RevocationDate: TDateTime): boolean; virtual; abstract;
    function IsValid(const cert: ICryptCert;
      date: TDateTime): TCryptCertValidity; virtual; abstract;
    function IsValidChain(const chain: ICryptCertChain; date: TDateTime;
      ignore: TCryptCertValidities): TCryptCertValidity; virtual;
    function Verify(const Signature: RawByteString; Data: pointer; Len: integer;
      IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
        overload; virtual; abstract;
    function Verify(const Signature, Data: RawByteString;
      IgnoreError: TCryptCertValidities = [];
      TimeUtc: TDateTime = 0): TCryptCertValidity; overload;
    function Count: integer; virtual; abstract;
    function CrlCount: integer; virtual; abstract;
    function DefaultCertAlgo: TCryptCertAlgo; virtual;
  end;

  /// meta-class of the abstract parent to implement ICryptStore interface
  TCryptStoreClass = class of TCryptStore;

  /// abstract parent class for ICryptStore factories
  TCryptStoreAlgo = class(TCryptAlgo)
  public
    /// main factory to create a new Store instance with this engine
    function New: ICryptStore; virtual; abstract;
    /// main factory to create a new Store instance from saved Binary
    function NewFrom(const Binary: RawByteString): ICryptStore; virtual;
    /// return the preferred algo to be used with this store
    // - should be the same class as ICryptStore.DefaultCertAlgo
    function DefaultCertAlgo: TCryptCertAlgo; virtual; abstract;
  end;

  /// abstract parent of TCryptCertList and TCryptCertCache storage classes
  TCryptCertAbstractList = class(TSynPersistent)
  protected
    fList: TSynDictionary; // thread-safe RawByteString(SKID/DER)/ICryptCert
    fCryptCertClass: TCryptCertClass;
    procedure SetCryptCertClass(c: TCryptCertClass); // from TCryptStore.Create
    function GetCount: integer;
      {$ifdef HASINLINE} inline; {$endif}
  public
    // finalize the ICryptCert storage
    destructor Destroy; override;
    /// search the internal list for a given attribute
    // - return all the certificates matching a given value
    // - will use brute-force O(n) search algorithm with lockfree multi-read
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; virtual;
    /// search the internal list for a given attribute
    // - return the first certificate matching a given value
    function FindOne(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber): ICryptCert; virtual;
    /// return a copy of the internal list items
    function List: ICryptCerts;
    /// persist all stored Certificates in PEM format
    procedure SaveToPem(W: TTextWriter; WithExplanatoryText: boolean = false);
    /// direct low-level to the internal raw dictionary
    // - store a hash table of ICryptCert values
    // - for TCryptCertCache, RawByteString keys are DER certificates content
    // - for TCryptCertList, RawByteString keys are SKID/GetSubjectKey binary
    // - use rather the List function if you just want to access the stored values
    property RawList: TSynDictionary
      read fList;
  published
    /// the class of TCryptCert currently stored in this list
    // - is either set in the overriden constructor, or retrieved at runtime
    property CryptCertClass: TCryptCertClass
      read fCryptCertClass;
    /// how many instances are currently stored in this instance
    property Count: integer
      read GetCount;
  end;

  /// store several ICryptCert instances
  // - those instances are likely to come from a TCryptCertCache holder
  // - maintain a hashed index of ICryptCert.GetSubjectKey values for
  // fast certification path validation e.g. during ICryptStore.IsValid
  TCryptCertList = class(TCryptCertAbstractList)
  public
    /// initialize the ICryptCert storage
    constructor Create; override;
    /// include once a X.509 Certificate instance to the internal list
    // - return false if its GetSubjectKey was already present
    function Add(const Cert: ICryptCert): boolean; overload;
    /// include once several X.509 Certificate instances to the internal list
    procedure Add(const Cert: array of ICryptCert); overload;
    /// search the list for a ICryptCert.GetSubjectKey using a hashed index
    // - i.e. the Subject Key Identifier (SKID) of a X.509 Certificate or
    // the serial number for syn-ecc
    function FindBySubjectKey(const Key: RawUtf8): ICryptCert;
    /// search the list for binary ICryptCert.GetSubjectKey using a hashed index
    // - could be used instead of FindBySubjectKey() if the key is already
    // decoded into its HumanHexToBin() raw binary format (as stored internally)
    function FindBySubjectKeyRaw(const Key: RawByteString): ICryptCert;
      {$ifdef HASINLINE} inline; {$endif}
    /// search the internal list for a given attribute
    // - overriden to let ccmSubjectKey search use the hashed index
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; override;
    /// remove a ICryptCert from the list using its indexed GetSubjectKey
    function DeleteBySubjectKey(const Key: RawUtf8): boolean;
  end;

  /// abstract class to cache ICryptCert instances, from their DER/binary
  // - should be overriden to let its InternalLoad() method be implemented
  // - to speed up typical PKI process, no DER parsing would be necessary
  // - this class is thread-safe and will flush its oldest entries automatically
  // - use TCryptCertCacheX509 or TCryptCertCacheOpenSsl, not this abstract class
  TCryptCertCache = class(TCryptCertAbstractList)
  protected
    function OnDelete(const aKey, aValue; aIndex: integer): boolean;
    // this abstract method should be properly overriden to load a DER buffer
    function InternalLoad(const Cert: RawByteString): ICryptCert; virtual; abstract;
  public
    /// instantiate a ICryptCert instances cache
    // - you can have several TCryptCertCache, dedicated to each bounded context
    // - by default, internal cache will clean up instances with RefCnt = 1
    // after 10 min of inactivity
    constructor Create(TimeOutSeconds: integer = 10 * 6); reintroduce;
    /// retrieve a potentially shared ICryptCert instance from DER or PEM input
    // - returns nil if the input is not correct or not supported
    // - will guess the proper TCryptCertAlgoX509 to use for the ICryptCert
    function Load(const Cert: RawByteString): ICryptCert; overload;
    /// retrieve a chain of ICryptCert instances from an array of DER input
    // - any invalid Cert[] will just be ignored and not part of the result
    function Load(const Cert: array of RawByteString): ICryptCerts; overload;
    /// retrieve a chain of ICryptCert instances from a PEM input
    // - any invalid chunk in the PEM will be ignored and not part of the result
    function LoadPem(const Pem: RawUtf8): ICryptCerts;
    /// search the internal list for a given attribute
    // - overriden to let ccmBinary search use the hashed index
    function Find(const Value: RawByteString;
      Method: TCryptCertComparer = ccmSerialNumber;
      MaxCount: integer = 0): ICryptCerts; override;
    /// allocate a new TCryptCertList instance in the context of this cache
    function NewList: TCryptCertList; overload; virtual;
    /// allocate a new TCryptCertList instance in the context of this cache
    // - filling the list with certificates from some PEM input
    function NewList(const Pem: RawUtf8): TCryptCertList; overload;
  end;

var
  /// the default source of trust used for new ICryptStore instances
  // - equals [cstOsRoot] on POSIX, as most libraries (e.g. OpenSSL) do
  // - on Windows, contains [cstOsRoot, cstOsIntermediate, cstAiaIntermediate]
  // unless IsAiaDisabledInWindowsRegistry result is true at startup
  DefaultCryptStoreTrust: TCertStoreTrusts = [cstOsRoot]
    {$ifdef OSWINDOWS} + [cstOsIntermediate, cstAiaIntermediate] {$endif};

/// append a ICryptCert to a certificates chain
procedure ChainAdd(var chain: ICryptCertChain; const cert: ICryptCert);
  {$ifdef HASINLINE} inline; {$endif}

/// search for a ICryptCert to a certificates chain
// - will search for the ICryptCert instance itself, or by TCryptCertComparer
function ChainFind(var chain: ICryptCertChain; const cert: ICryptCert;
  comparer: TCryptCertComparer = ccmInstance): PtrInt;

/// sort a certificate chain by mutual authentication
// - returns the certificates in IsAuthorizedBy() order
function ChainConsolidate(const chain: ICryptCertChain): ICryptCertChain;

/// append a new entry as binary pair to a dynamic array of TCryptCustomExt
// - use AsnEncOid() to compute the o binary from 'x.x.x.x.x' text OID
procedure AddCustomExts(var exts: TCryptCustomExts; const o, v: RawByteString;
  crit: boolean = false); overload;

/// append a new entry as binary pair to a dynamic array of TCryptCustomExt
// - use AsnEncOid() to compute the o binary from 'x.x.x.x.x' text OID
procedure AddCustomExts(var exts: TCryptCustomExts; const o: TAsnBuffer;
  const v: RawByteString; crit: boolean = false); overload;

/// efficient search of a TCryptCustomExt.Value from a 'x.x.x.x.x' text OID
function FindCustomExts(const Other: TCryptCustomExts; OidText: PUtf8Char): RawByteString;

/// low-level search of a TCryptCustomExt.Value from a binary OID
function FindCustomExtsAsn(o: PCryptCustomExt; n: integer; const b: TAsnObject): RawByteString;

/// search and decode Authority Information Access (1.3.6.1.5.5.7.1.1) extension content
function FindAia(const ext: TCryptCustomExts; var ocsp, issuers: TRawUtf8DynArray): boolean;

/// search and decode Crl Distribution Points (2.5.29.31) extension content
function FindCdp(const ext: TCryptCustomExts): TRawUtf8DynArray;

type
  /// maintains a list of ICryptCert, easily reachable per TCryptCertUsage
  // - could be seen as a basic certificates store or "PKI of the poor" (tm)
  // - per usage lookup is in O(1) so faster than iterative ICryptCert.GetUsage
  // - also features simple PEM / binary serialization methods
  // - no CRL nor complex intermediate certificates lookup are available
  // - should be initialized by Clear at startup, or zeroed as a class field
  {$ifdef USERECORDWITHMETHODS}
  TCryptCertPerUsage = record
  {$else}
  TCryptCertPerUsage = object
  {$endif USERECORDWITHMETHODS}
  public
    /// the stored ICryptCert Instances
    List: ICryptCerts;
    /// all usages currently stored in this list
    Usages: TCryptCertUsages;
    /// lookup table used by GetUsage()/PerUsage()
    // - 0 means no certificate, or store the index in List[] + 1
    Index: array[TCryptCertUsage] of byte;
    /// reset all storage and indexes
    procedure Clear;
    /// quickly check if there is no stored certificate
    function IsVoid: boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// register the certificate to the internal list
    // - returns the duplicated usages
    // - if no usage(s) was already set as in the added one, returns []
    // - if another certificate has already an usage, it is overwritten and
    // the duplicated usage(s) are returned
    // - so the typical pattern is to add the certificate in inverse order of
    // authority, i.e. first the CA as root cuKeyCertSign, then the less
    // specialized cuKeyCertSign certificates - so that the weaker certificate
    // is returned by PerUsage/GetUsage for the actual process
    function Add(const cert: ICryptCert): TCryptCertUsages;
    /// fast lookup of a certificate per its usage
    // - i.e. returns the last/weakest certificate having the supplied usage
    function GetUsage(u: TCryptCertUsage; var cert: ICryptCert): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    /// fast lookup of a certificate per its usage
    // - i.e. returns the last/weakest certificate having the supplied usage
    function PerUsage(u: TCryptCertUsage): ICryptCert;
      {$ifdef HASINLINE} inline; {$endif}
    /// save all items as a cccCertOnly CRLF separated list of PEM certificates
    function AsPem: RawUtf8;
    /// clear and load a CRLF separated list of PEM certificates
    // - returns the duplicated usages found during adding certificates
    function FromPem(algo: TCryptCertAlgo; const pem: RawUtf8): TCryptCertUsages;
    /// save all items as a cccCertOnly binary blob of certificates
    // - binary layout is TBufferWriter.WriteVar() of all DER serialization
    function AsBinary: RawByteString;
    /// clear and load a binary blob of certificates saved by AsBinary
    // - returns the duplicated usages found during adding certificates
    function FromBinary(algo: TCryptCertAlgo; const bin: RawByteString): TCryptCertUsages;
  end;


/// fast case-insensitive check of the 'CN' Relative Distinguished Name identifier
function IsCN(const Rdn: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// fast case-insensitive check of the 'DER' fake RDNidentifier
function IsDer(const Rdn: RawUtf8): boolean;
  {$ifdef HASINLINE} inline; {$endif}

/// compute the minimum JWK output from raw binary parameters
function SaveAsJwk(algo: TCryptAsymAlgo; const x, y: RawByteString): RawUtf8;

/// main resolver of the randomness generators
// - a shared TCryptRandom instance is returned: caller should NOT free it
// - e.g. Rnd.GetBytes(100) to get 100 random bytes from 'rnd-default' engine,
// which redirects in fact to our secure TAesPrng.Main generator
// - alternative generators are 'rnd-aes' (same as 'rnd-default'), 'rnd-lecuyer'
// (fast on small content), 'rnd-system'/'rnd-systemblocking' (mapping
// FillSystemRandom, which may be slow and even blocking), 'rnd-delphi' which
// follows the weak but known Delphi RTL Random(), and 'rnd-rdrand' which calls
// the homonymous CPU HW opcode (if cfRAND in CpuFeatures)
// - call Rnd('rnd-entropy').Get() to gather OS entropy (which may be slow),
// optionally as 'rnd-entropy-full', 'rnd-entropy-sys', 'rnd-entropy-sysblocking'
// and 'rnd-entropy-user'
function Rnd(const name: RawUtf8 = 'rnd-default'): TCryptRandom;

/// main resolver of the registered hashers
// - hashers ensure a content as not been tempered: use Signer()
// to compute a digital signature from a given secret
// - the shared TCryptHasher of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'md5', 'sha1', 'sha256', 'sha384', 'sha512', 'sha3_256',
// 'sha3_512' and 32-bit non-cryptographic 'crc32', 'crc32c', 'xxhash32',
// 'adler32', 'fnv32', 'aesni32', 'md5-32' and 'sha1-32'
function Hasher(const name: RawUtf8): TCryptHasher;

/// main factory of the hashers instances as returned by Hasher()
// - if not nil, caller should call Update then Final
function Hash(const name: RawUtf8): ICryptHash;

/// main resolver of the registered signers
// - in respect to Hasher(), will require a secret for safe digital signature
// - the shared TCryptSigner of this algorithm is returned: caller should NOT free it
// - if not nil, you could call New or Full/FullFile methods
// - this unit supports 'hmac-sha1','hmac-sha256','hmac-sha384','hmac-sha512',
// and 'sha3-224','sha3-256','sha3-384','sha3-512','sha3-s128','sha3-s256'
function Signer(const name: RawUtf8): TCryptSigner;

/// main factory of the signer instances as returned by Signer()
// - in respect to Hash(), expects a key/salt to be supplied for the HMAC signing
// - if not nil, caller should call Update then Final
function Sign(key: pointer; keylen: PtrInt; const name: RawUtf8): ICryptHash; overload;

/// main factory of the signer instances, with PBKDF2 secret derivation
// - if not nil, caller should call Update then Final
function Sign(const name, secret, salt: RawUtf8; rounds: integer): ICryptHash; overload;

/// main factory of a signer instance from strong PBKDF2 derivation
// - paramsjson contains a TSynSignerParams serialized as JSON object
// - returns the number of bytes computed in returned key memory
function Sign(const paramsjson: RawUtf8;
  const defaultsalt: RawUtf8 = SIGNER_DEFAULT_SALT): ICryptHash; overload;

/// main resolver for symmetric encryption/decryption algorithms
// - names are e.g. 'aes-128-cfb' or 'aes-256-gcm' standard combinations
// - recognize some non-standard algorithms with trailing 'c64', 'cfc', 'ofc' and
// 'ctc' mode names e.g. as 'aes-256-cfc' - with AED 256-bit support (but c64)
// - will return the fastest engine, which may be OpenSSL, unless 'aes-nnn-xxx-int'
// is used to force using our mormot.crypt.core.pas own implementation
// - the shared TCryptCipherAlgo of this algorithm is returned: caller should
// NOT free it
function CipherAlgo(const name: RawUtf8): TCryptCipherAlgo;

/// main factory for symmetric encryption/decryption process
// - supplied key is expected to match the algorithm size
function Cipher(const name: RawUtf8; key: pointer; encrypt: boolean): ICryptCipher; overload;

/// main factory for symmetric encryption/decryption with PBKDF2 key derivation
function Cipher(const name, hash, secret, salt: RawUtf8; rounds: integer;
  encrypt: boolean): ICryptCipher; overload;

/// main factory for symmetric encryption process
function Encrypt(const name: RawUtf8; key: pointer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric decryption process
function Decrypt(const name: RawUtf8; key: pointer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric encryption process with PBKDF2 key derivation
function Encrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main factory for symmetric decryption process with PBKDF2 key derivation
function Decrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher; overload;
  {$ifdef HASINLINE} inline; {$endif}

/// main resolver for asymmetric public key algorithms
// - mormot.crypt.ecc.pas defines 'secp256r1','NISTP-256' and 'prime256v1'
// which are synonymous of our secp256r1 ECDSA and ECDHE pascal code
// - mormot.crypt.openssl.pas will define 'es256' .. 'EdDSA' including
// 'rs256' for the well known 2048-bit RSA + SHA256 digital signature
// - the shared TCryptAsym of this algorithm is returned: caller should NOT free it
function Asym(const name: RawUtf8): TCryptAsym;

/// main resolver for Certificates algorithms
// - mormot.crypt.ecc.pas defines 'syn-es256' for our TEccCertificate proprietary
// format (safe and efficient), with 'syn-es256-v1' for the V1 revision with
// limited Usage and Subjects support
// - mormot.crypt.openssl.pas will define 'x509-es256' .. 'x509-eddsa' including
// 'x509-rs256' for the well established 2048-bit RSA + SHA256 certificates
// - the shared TCryptCertAlgo of this algorithm is returned: caller should
// NOT free it
function CertAlgo(const name: RawUtf8): TCryptCertAlgo;

/// main factory of the Certificates instances as returned by CertAlgo()
// - name is typically 'syn-es256' or 'x509-es256'
function Cert(const name: RawUtf8): ICryptCert;

/// main resolver for Certificates Store engines
// - mormot.crypt.ecc.pas defines 'syn-store' or 'syn-store-nocache' for our
// TEccCertificateChain proprietary format (safe and efficient)
// - mormot.crypt.openssl.pas will define 'x509-store'
// - the shared TCryptStoreAlgo of this algorithm is returned: caller should
// NOT free it
function StoreAlgo(const name: RawUtf8): TCryptStoreAlgo;

/// main factory of Certificates Store engines as returned by StoreAlgo()
function Store(const name: RawUtf8): ICryptStore;

var
  /// the preferred/default algorithm to be used with X.509 certificates
  // - caaES256 (aka prime256v1 or NISTP-256) seems the new default (faster
  // and with 128-bit of security), even if RSA-2048 (i.e. caaRS256) may still
  // be used for compatiblity with legacy systems (but much slower signing and
  // generation, with only 112-bit of security)
  // - used e.g. by 'x509-store' or 'x509-pki' for its DefaultCertAlgo method
  CryptAlgoDefault: TCryptAsymAlgo = caaES256;


  (* TCryptAsym factories *)

  /// direct access to the internal TCryptAsym factories
  // - may be nil if mormot.crypt.ecc.pas or mormot.crypt.rsa.pas units
  // were not included
  // - you may use rather CryptAsymOpenSsl[] if OpenSSL is available
  CryptAsym: array[TCryptAsymAlgo] of TCryptAsym;

  /// direct access to the mormot.crypt.openssl.pas TCryptAsym factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptAsymOpenSsl: array[TCryptAsymAlgo] of TCryptAsym;


  (* ICryptPublicKey / ICryptPrivateKey factories *)

  /// RSA/ECC public key factory
  // - implemented e.g. by mormot.crypt.ecc with TCryptPublicKeyEcc,
  // mormot.crypt.rsa with TCryptPublicKeyRsa or mormot.crypt.opensssl with
  // with TCryptPublicKeyOpenSsl
  // - use as such:
  // ! var key: ICryptPublicKey;
  // ! ...
  // !   key := CryptPublicKey[ckaEcc].Create;
  // !   if key.Load(...) then ...
  CryptPublicKey: array[TCryptKeyAlgo] of TCryptPublicKeyClass;

  /// RSA/ECC private key factory
  // - implemented e.g. by mormot.crypt.ecc with TCryptPrivateKeyEcc,
  // mormot.crypt.rsa with TCryptPrivateKeyRsa or mormot.crypt.opensssl with
  // with TCryptPrivateKeyOpenSsl
  // - use as such:
  // ! var key: ICryptPrivateKey;
  // ! ...
  // !   key := CryptPrivateKey[ckaEcc].Create;
  // !   if key.Load(...) then ...
  CryptPrivateKey: array[TCryptKeyAlgo] of TCryptPrivateKeyClass;


  (* ICryptCert factories *)

  /// direct access to the mormot.crypt.ecc.pas 'syn-ecc' algorithm
  // - may be nil if this unit was not included
  CryptCertSyn: TCryptCertAlgo;

  /// direct access to the mormot.crypt.x509.pas ICryptCert factories
  // - may be nil if this unit was not included
  // - to get a void ICryptCert instance for RSA 2048 key over SHA-256, use e.g.
  // ! CryptCertX509[caaRS256].New
  CryptCertX509: array[TCryptAsymAlgo] of TCryptCertAlgo;

  /// direct access to the mormot.crypt.openssl.pas ICryptCert factories
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - to return a ICryptCert instance using OpenSSL RSA 2048 key, use e.g.
  // ! CryptCertOpenSsl[caaRS256].New
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptCertOpenSsl: array[TCryptAsymAlgo] of TCryptCertAlgo;

  /// direct access to best known X.509 ICryptCert factories
  // - may map to CryptCertX509[] if only mormot.crypt.x509.pas is included
  // - but more standard CryptCertOpenSsl[] will be stored here if available
  CryptCert: array[TCryptAsymAlgo] of TCryptCertAlgo;

  /// global shared instances as set by InitNetTlsContextSelfSignedServer()
  // - defined here to be properly released before the OpenSSL library unloading
  CryptCertOpenSslSelfSigned: array[TCryptAsymAlgo] of ICryptCert;


  (* ICryptStore factories *)

  /// direct access to the mormot.crypt.ecc.pas 'syn-store' algorithm
  // - may be nil if this unit was not included
  CryptStoreSyn: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.ecc.pas 'syn-store-nocache' algorithm
  // - may be nil if this unit was not included
  CryptStoreSynNoCache: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.x509.pas ICryptStore factory
  CryptStoreX509: TCryptStoreAlgo;

  /// direct access to the mormot.crypt.openssl.pas 'x509-store' algorithm
  // - may be nil if this unit was not included or if OpenSSL is not available
  // - is currently nil because TCryptStoreOpenSsl is not stable yet
  // - call RegisterOpenSsl once to initialize this lookup table
  CryptStoreOpenSsl: TCryptStoreAlgo;


{ ******************************* Key and Certificate Aware PEM/DER Helpers }

const
  /// the OID of all supported ICryptPublicKey/ICryptPrivateKey algorithms
  CKA_OID: array[TCryptKeyAlgo] of RawUtf8 = (
    '',                       // ckaNone
    '1.2.840.113549.1.1.1',   // ckaRsa
    '1.2.840.113549.1.1.10',  // ckaRsaPss
    '1.2.840.10045.3.1.7',    // ckaEcc256  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.34',           // ckaEcc384  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.35',           // ckaEcc512  (with ASN1_OID_X962_PUBLICKEY)
    '1.3.132.0.10',           // ckaEcc256k (with ASN1_OID_X962_PUBLICKEY)
    '1.3.101.112');           // ckaEdDSA

/// cipher a PrivateKeyInfo DER binary into standard PKCS#8 encryptedPrivateKeyInfo
function PrivateKeyToPkcs8(Algorithm: TCryptKeyAlgo;
  const Der: RawByteString): RawByteString;

/// compute the number of security bits of a digital signature
// - RSA security depends on the signature size, not the hash size
// - ECC security size is half of its X,Y coordinates storage size
// - e.g. 112 for RSA-2048, 128 for ECC-256
function GetSignatureSecurityBits(a: TCryptAsymAlgo; len: integer): integer;

/// compute the base-64 encoding of the raw binary of a ASN.1/DER digital signature
// - input comes e.g. from ICryptCert.Sign or ICryptPrivateKey.Sign content
// - base-64 encoded output can be used e.g. for JSON Web Token/Signature (JWT/JWS)
// - RSA and EdDSA signatures are not encoded, so are returnde directly
// - ECC are decoded from their ASN1_SEQ into their raw xy coordinates concatenation
function GetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const signature: RawByteString): RawUtf8;

/// encode a raw digital signature into ASN.1/DER
// - incoming comes e.g. from base-64 decoded JSON Web Token/Signature (JWT/JWS)
// - output is compatible e.g. with ICryptCert.Verify or ICryptPublicKey.Verify
// - ECC are encoded from their raw x/y coordinates concatenation into ASN1_SEQ
function SetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const rawsignature: RawUtf8): RawByteString;

/// raw function to recognize the OID(s) of a public key ASN1_SEQ definition
function OidToCka(const oid, oid2: RawUtf8): TCryptKeyAlgo;

/// raw function to generate a public key ASN1_SEQ definition with its OID(s)
function CkaToSeq(cka: TCryptKeyAlgo): RawByteString;

/// raw function to encode a PKCS#8 PrivateKeyInfo from its raw binary number
// - e.g. as generated by OpenSSL
function EccPrivKeyToSeq(cka: TCryptKeyAlgo; const rawecc: RawByteString): RawByteString;

/// raw function to decode a PKCS#8 PrivateKeyInfo into its raw binary
// - e.g. as generated by OpenSSL
// - is also able to decode RFC 5915 Elliptic Curve key pair alternate format,
// and its optional public key field
function SeqToEccPrivKey(cka: TCryptKeyAlgo; const seq: RawByteString;
  rfcpub: PRawByteString = nil): RawByteString;

/// raw function to decode a specific PKCS#8 PublicKeyInfo into its raw binary
// - e.g. as generated by OpenSSL or X509PubKeyToDer()
// - behaves like X509PubKeyFromDer() but checking for one expected ECC algorithm
function SeqToEccPubKey(cka: TCryptKeyAlgo; const seq: RawByteString): RawByteString;

/// compute the public key ASN.1 from a raw binary as stored in X.509 certificate
// - see e.g. "A.1.1. RSA Public Key Syntax" of RFC 8017
function X509PubKeyToDer(Algorithm: TCryptKeyAlgo;
  const SubjectPublicKey: RawByteString): RawByteString;

type
  /// output of the X509Parse() function
  // - contains X.509 certificate main properties and binary public key
  TX509Parsed = record
    /// the certificate Serial Number, stored as 'xx:xx:xx:xx...' hexa text
    Serial: RawUtf8;
    /// the certificate Subject, decoded as RFC 1779 text, with X500 key names
    SubjectDN: RawUtf8;
    /// the certificate Issuer, decoded as RFC 1779 text, with X500 key names
    IssuerDN: RawUtf8;
    /// the certificate Subject ID, stored as 'xx:xx:xx:xx...' hexa text
    SubjectID: RawUtf8;
    /// the certificate Issuer ID, stored as 'xx:xx:xx:xx...' hexa text
    IssuerID: RawUtf8;
    /// the algorithm name of this certificate's digital signature
    SigAlg: RawUtf8;
    /// the algorithm name of this certificate's public key
    PubAlg: RawUtf8;
    /// the certificate Alternate Subject names as a CSV array
    SubjectAltNames: RawUtf8;
    /// human-friendly multi-line text of this certificate main fields
    PeerInfo: RawUtf8;
    /// the main key usages of this certificate
    Usage: TCryptCertUsages;
    /// the certificate validity start date
    NotBefore: TDateTime;
    /// the certificate validity end date
    NotAfter: TDateTime;
    /// the certificate public key ASN1 raw binary as stored in the certificate
    PubKey: RawByteString;
  end;

/// return some multi-line text of the main TX509Parsed fields
// - in a layout similar to X509_print() OpenSSL formatting
// - is used by its own TX509Parsed.PeerInfo field
function ParsedToText(const c: TX509Parsed): RawUtf8;

var
/// high-level function to decode X.509 Certificate main properties
// - properly implemented by mormot.crypt.openssl or mormot.crypt.x509, but
// this unit will register our mormot.lib.sspi parser as failover on Windows
  X509Parse: function(const Cert: RawByteString; out Info: TX509Parsed): boolean;

{$ifdef OSWINDOWS}

/// the raw mormot.lib.sspi parser - published for testing
function WinX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;

/// internal conversion fuction, as used e.g. by WinX509Parse()
procedure WinInfoToParse(const c: TWinCertInfo; out Info: TX509Parsed);

{$endif OSWINDOWS}


implementation

{ ************* Rnd/Hash/Sign/Cipher/Asym/Cert/Store High-Level Algorithms Factories }

var
  GlobalCryptAlgo: TRawUtf8List; // Objects[] are TCryptAlgo instances

procedure GlobalCryptAlgoInit; forward;


{ TCryptAlgo }

constructor TCryptAlgo.Create(const name: RawUtf8);
begin
  if name = '' then
    ECrypt.RaiseUtf8('Unexpected %.Create('''')', [self]);
  fName := LowerCase(name);
  RegisterGlobalShutdownRelease(self);
  GlobalCryptAlgo.AddOrReplaceObject(fName, self);
end;

class function TCryptAlgo.InternalFind(
  const name: RawUtf8; var Last: TCryptAlgo): pointer;
begin
  if name = '' then
  begin
    result := nil;
    exit;
  end;
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := Last; // simple but efficient cache
  if (result <> nil) and
     PropNameEquals(TCryptAlgo(result).fName, name) then
    exit;
  result := GlobalCryptAlgo.GetObjectFrom(name); // thread-safe lookup
  if result <> nil then
    if TCryptAlgo(result).InheritsFrom(self) then
      Last := result
    else
      result := nil;
end;

class function TCryptAlgo.InternalResolve(
  const name: RawUtf8; CSV: PUtf8Char): integer;
begin
  result := FindCsvIndex(CSV, name, ',', {casesens=}false);
  if result < 0 then
    ECrypt.RaiseUtf8('%.Create(''%''): unknown algorithm in %', [self, name, CSV]);
end;

class function TCryptAlgo.Implements(const name: array of RawUtf8): pointer;
var
  i: PtrInt;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  for i := 0 to high(name) do
    if name[i] <> '' then
      result := Create(name[i]);
end;

class procedure TCryptAlgo.Implements(csv: PUtf8Char; const suffix: RawUtf8);
var
  name: RawUtf8;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  while csv <> nil do
  begin
    GetNextItem(csv, ',', name);
    if name <> '' then
      Create(name + suffix);
  end;
end;

class function TCryptAlgo.Instances: TCryptAlgos;
var
  i: PtrInt;
  n: integer;
  o: PObjectArray;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  n := 0;
  GlobalCryptAlgo.Safe.ReadOnlyLock;
  try
    o := pointer(GlobalCryptAlgo.ObjectPtr);
    for i := 0 to GlobalCryptAlgo.Count - 1 do
      if o[i].InheritsFrom(self) then
        ObjArrayAddCount(result, o[i], n);
  finally
    GlobalCryptAlgo.Safe.ReadOnlyUnLock;
  end;
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;

class function TCryptAlgo.Names: TRawUtf8DynArray;
var
  i: PtrInt;
  n: integer;
  o: PObjectArray;
begin
  if GlobalCryptAlgo = nil then
    GlobalCryptAlgoInit;
  result := nil;
  n := 0;
  GlobalCryptAlgo.Safe.ReadOnlyLock;
  try
    o := pointer(GlobalCryptAlgo.ObjectPtr);
    for i := 0 to GlobalCryptAlgo.Count - 1 do
      if o[i].InheritsFrom(self) then
        AddRawUtf8(result, n, TCryptAlgo(o[i]).fName);
  finally
    GlobalCryptAlgo.Safe.ReadOnlyUnLock;
  end;
  if n <> 0 then
    DynArrayFakeLength(result, n);
end;


{ TCryptInstance }

constructor TCryptInstance.Create(algo: TCryptAlgo);
begin
  if algo = nil then
    ECrypt.RaiseUtf8('Unexpected %.Create(nil)', [self]);
  fCryptAlgo := algo;
end;

var
  LastAlgoInstance: TCryptAlgo;

constructor TCryptInstance.Create(const name: RawUtf8);
var
  algo: TCryptAlgo;
begin
  algo := TCryptAlgo.InternalFind(name, LastAlgoInstance);
  if algo = nil then
    ECrypt.RaiseUtf8('Unexpected %.Create(''%'')', [self, name]);
  Create(algo);
end;


{ TCryptRandom }

procedure TCryptRandom.Get(var dst: RawByteString; len: PtrInt);
begin
  FillZero(dst);
  Get(FastNewRawByteString(dst, len), len);
end;

function TCryptRandom.GetBytes(len: PtrInt): TBytes;
begin
  result := nil;
  SetLength(result, len);
  Get(pointer(result), len);
end;

function TCryptRandom.Get32: cardinal;
var
  n: cardinal; // safer with a local variable
begin
  Get(@n, SizeOf(n));
  result := n;
end;

function TCryptRandom.Get32(max: cardinal): cardinal;
begin
  result := (QWord(Get32) * max) shr 32;
end;

function TCryptRandom.Get64: QWord;
var
  n: QWord; // safer with a local variable
begin
  Get(@n, SizeOf(n));
  result := n;
end;

function TCryptRandom.GetDouble: double;
const
  COEFF32: double = 1.0 / (Int64(1) shl 32);
begin
  result := Get32 * COEFF32; // 32-bit resolution is enough for our purpose
end;

function TCryptRandom.GetPassword(len: PtrInt): SpiUtf8;
var
  c: integer;
begin
  repeat
    Get(RawByteString(result), Len);
  until MakeStrongPassword(result, @c);
end;


{ TCryptRandomAesPrng }

type
  TCryptRandomAesPrng = class(TCryptRandom) // 'rnd-default,rnd-aes'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
    function Get32: cardinal; override;
    function Get64: QWord; override;
  end;

procedure TCryptRandomAesPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  TAesPrng.Main.FillRandom(dst, dstlen);
end;

function TCryptRandomAesPrng.Get32: cardinal;
begin
  result := TAesPrng.Main.Random32; // a CSPRNG is pointless for 32-bit anyway
end;

function TCryptRandomAesPrng.Get64: QWord;
begin
  result := TAesPrng.Main.Random64;
end;


{ TCryptRandomEntropy }

const
  // CSV text of TAesPrngGetEntropySource items as used for Rnd() factory naming
  RndAlgosText: PUtf8Char = 'rnd-entropy,rnd-entropy-full,rnd-entropy-sys,' +
    'rnd-entropy-sysblocking,rnd-entropy-user';

type
  TCryptRandomEntropy = class(TCryptRandom)
  protected
    fSource: TAesPrngGetEntropySource;
    fNonce: RawByteString;
  public
    constructor Create(const name: RawUtf8); override;
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

constructor TCryptRandomEntropy.Create(const name: RawUtf8);
begin
  fSource := TAesPrngGetEntropySource(InternalResolve(name, RndAlgosText));
  RandomBytes(FastNewRawByteString(fNonce, 16), 16); // good enough
  inherited Create(name); // should be done after InternalResolve()
end;

procedure TCryptRandomEntropy.Get(dst: pointer; dstlen: PtrInt);
begin // warning: may be slow for a few bytes
  TAesPrng.GetEntropy(dst, dstlen, fSource, fNonce);
end;


{ TCryptRandomSysPrng }

type
  TCryptRandomSysPrng = class(TCryptRandom) // 'rnd-system,rnd-systemblocking'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
  end;

procedure TCryptRandomSysPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  if (dstlen > 0) and
     not FillSystemRandom(dst, dstlen, {allowblocking=}length(fName) > 10) then
    ECrypt.RaiseUtf8('%.Get(%): OS API failure', [self, dstlen]);
end;


{ TCryptRandomLecuyerPrng }

type
  TCryptRandomLecuyerPrng = class(TCryptRandom) // 'rnd-lecuyer'
  public
    procedure Get(dst: pointer; dstlen: PtrInt); override;
    function Get32: cardinal; override;
  end;

procedure TCryptRandomLecuyerPrng.Get(dst: pointer; dstlen: PtrInt);
begin
  ThreadRandom.Fill(dst, dstlen); // per-thread TLecuyer gsl_rng_taus2 generator
end;

function TCryptRandomLecuyerPrng.Get32: cardinal;
begin
  result := ThreadRandom.Next; // dedicated fast 32-bit generator
end;

{ TCryptRandom32 }

procedure TCryptRandom32.Get(dst: pointer; dstlen: PtrInt);
var
  c: cardinal;
begin
  repeat
    if dstlen < 4 then
      break;
    PCardinal(dst)^ := PCardinal(dst)^ xor Get32; // e.g. Get32 = RdRand32
    inc(PCardinal(dst));
    dec(dstlen, 4);
  until false;
  if dstlen <= 0 then
    exit;
  c := Get32;
  repeat
    PByte(dst)^ := PByte(dst)^ xor c;
    inc(PByte(dst));
    c := c shr 8;
    dec(dstlen);
  until dstlen = 0;
end;

{ TCryptRandomDelphi }

type
  TCryptRandomDelphi = class(TCryptRandom32) // 'rnd-delphi'
  protected
    fSafe: TLightLock;
    fSeed: cardinal;
  public
    function Get32: cardinal; override;
  end;

function TCryptRandomDelphi.Get32: cardinal;
begin
  fSafe.Lock; // we make this generator thread-safe - whereas Delphi's is not
  if fSeed = 0 then
    fSeed := Random32; // good enough - better than Delphi/FPC Randomize anyway
  // the Delphi RTL uses such a weak deterministic linear congruential generator
  result := fSeed * 134775813 + 1;
  fSeed := result;
  fSafe.UnLock;
end;

// note: the FPC RTL has a better Mersenne Twister algorithm, but its 32-bit
// core is not published outside of the system unit, it consumes 2KB from a weak
// 32-bit seed from GetTickCount/fptime, and is not thread-safe either

{$ifdef ASMINTEL}

{ TCryptRandomRdRand }

type
  TCryptRandomRdRand = class(TCryptRandom32) // 'rnd-rdrand'
  public
    function Get32: cardinal; override;
  end;

function TCryptRandomRdRand.Get32: cardinal;
begin
  result := RdRand32; // class is only registered if cfRAND in CpuFeatures
end;

{$endif ASMINTEL}

{ TCryptHash }

function TCryptHash.Final(digest: pointer; digestlen: PtrInt): PtrInt;
var
  dig: THash512Rec;
begin
  result := InternalFinal(dig);
  if result <= digestlen then
    MoveFast(dig, digest^, result)
  else
    result := 0;
  FillZero(dig.b);
end;

function TCryptHash.Final: RawUtf8;
var
  dig: THash512Rec;
begin
  BinToHexLower(@dig, InternalFinal(dig), result);
  FillZero(dig.b);
end;

function TCryptHash.Update(const buf: RawByteString): ICryptHash;
begin
  result := Update(pointer(buf), length(buf));
end;

procedure TCryptHash.UpdateFile(const filename: TFileName);
var
  temp: RawByteString;
  F: THandle;
  size, tempsize: Int64;
  read: integer;
begin
  if filename = '' then
    exit;
  F := FileOpenSequentialRead(filename);
  if ValidHandle(F) then
  try
    size := FileSize(F);
    tempsize := 1 shl 20; // 1MB temporary buffer for reading
    if tempsize > size then
      tempsize := size;
    SetLength(temp, tempsize);
    while size > 0 do
    begin
      read := FileRead(F, pointer(temp)^, tempsize);
      if read <= 0 then
        exit;
      Update(pointer(temp), read);
      dec(size, read);
    end;
  finally
    FileClose(F);
  end;
end;

function TCryptHash.UpdateStream(stream: TStream): Int64;
var
  temp: TBuffer128K;
  read: integer;
begin
  result := 0;
  if stream <> nil then
    // we don't use stream.Size since some TStream classes don't support it
    repeat
      read := stream.Read(temp, SizeOf(temp)); // read until the end
      if read <= 0 then
        break;
      Update(@temp, read);
      inc(result, read);
    until false;
end;


{ TCryptHasher }

function TCryptHasher.Full(buf: pointer; buflen: PtrInt): RawUtf8;
var
  h: ICryptHash;
begin
  h := New;
  h.Update(buf, buflen);
  result := h.Final;
end;

function TCryptHasher.Full(const buf: RawByteString): RawUtf8;
begin
  result := Full(pointer(buf), length(buf));
end;

function TCryptHasher.Full(buf: pointer; buflen: PtrInt;
  out digest: THash512Rec): PtrInt;
var
  h: ICryptHash;
begin
  h := New;
  h.Update(buf, buflen);
  result := h.Final(@digest, SizeOf(digest));
end;

function TCryptHasher.FullFile(const filename: TFileName): RawUtf8;
var
  h: ICryptHash;
begin
  h := New;
  h.UpdateFile(filename);
  result := h.Final;
end;

function TCryptHasher.HashAlgo(out hasher: THashAlgo): boolean;
begin
  result := false; // unspecified
end;

type
  TCryptCrc32Internal = class(TCryptHasher)
  protected
    fAlgo: TCrc32Algo;
  public
    constructor Create(const name: RawUtf8); override;
    function New: ICryptHash; override;
  end;

  TCryptCrcInternal = class(TCryptHash)
  protected
    fCrc: cardinal;
    fFunc: THasher;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    constructor Create(algo: TCryptCrc32Internal); reintroduce;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;


{ TCryptCrc32Internal }

const
  // CSV text of TCrc32Algo items as used for Hasher/Hash factories naming
  CrcAlgosText: PUtf8Char =
    'crc32,crc32c,xxhash32,adler32,fnv32,default32,md5-32,sha1-32,sha256-32';

constructor TCryptCrc32Internal.Create(const name: RawUtf8);
begin
  fAlgo := TCrc32Algo(InternalResolve(name, CrcAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptCrc32Internal.New: ICryptHash;
begin
  result := TCryptCrcInternal.Create(self);
end;


{ TCryptCrcInternal }

constructor TCryptCrcInternal.Create(algo: TCryptCrc32Internal);
begin
  // resolve fFunc in New/Create, since crc32/adler functions may be set later
  fFunc := CryptCrc32(algo.fAlgo);
  if not Assigned(fFunc) then
    ECrypt.RaiseUtf8('%.New: unavailable ''%'' function', [self, algo.fName]);
  inherited Create(algo);
end;

function TCryptCrcInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fCrc := fFunc(fCrc, buf, buflen);
  result := self;
end;

function TCryptCrcInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  dig.c[0] := fCrc;
  result := SizeOf(fCrc);
end;


{ TCryptHasherInternal }

type
  TCryptHasherInternal = class(TCryptHasher)
  protected
    fAlgo: THashAlgo;
  public
    constructor Create(const name: RawUtf8); override;
    function New: ICryptHash; override;
    function HashAlgo(out hasher: THashAlgo): boolean; override;
  end;

  TCryptHashInternal = class(TCryptHash)
  protected
    fAlgo: TSynHasher;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    destructor Destroy; override;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;

const
  // CSV text of THashAlgo items, as recognized by Hasher/Hash factories naming
  HashAlgosText: PUtf8Char =
    'md5,sha1,sha256,sha384,sha512,sha3_256,sha3_512,sha224';

constructor TCryptHasherInternal.Create(const name: RawUtf8);
begin
  fAlgo := THashAlgo(InternalResolve(name, HashAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptHasherInternal.New: ICryptHash;
var
  h: TCryptHashInternal;
begin
  h := TCryptHashInternal.Create(self);
  h.fAlgo.Init(fAlgo);
  result := h;
end;

function TCryptHasherInternal.HashAlgo(out hasher: THashAlgo): boolean;
begin
  hasher := fAlgo;
  result := true;
end;


{ TCryptHashInternal }

destructor TCryptHashInternal.Destroy;
begin
  inherited Destroy;
  FillCharFast(fAlgo, SizeOf(fAlgo), 0); // override memory
end;

function TCryptHashInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fAlgo.Update(buf, buflen);
  result := self;
end;

function TCryptHashInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  fAlgo.Final(dig);
  result := fAlgo.HashSize;
end;


{ TCryptSigner }

function TCryptSigner.Full(key, buf: pointer; keylen, buflen: PtrInt): RawUtf8;
var
  h: ICryptHash;
begin
  h := New(key, keylen);
  h.Update(buf, buflen);
  result := h.Final;
end;

function TCryptSigner.Full(const key, buf: RawByteString): RawUtf8;
begin
  result := Full(pointer(key), pointer(buf), length(key), length(buf));
end;

function TCryptSigner.FullFile(key: pointer; keylen: PtrInt;
  const filename: TFileName): RawUtf8;
var
  h: ICryptHash;
begin
  h := New(key, keylen);
  h.UpdateFile(filename);
  result := h.Final;
end;

function TCryptSigner.NewPbkdf2(const secret, salt: RawUtf8; rounds: integer): ICryptHash;
var
  key: THash512Rec;
begin
  result := New(@key, Pbkdf2(secret, salt, rounds, key));
end;


{ TCryptSignerInternal }

type
  TCryptSignerInternal = class(TCryptSigner)
  protected
    fAlgo: TSignAlgo;
  public
    constructor Create(const name: RawUtf8); override;
    function New(key: pointer; keylen: PtrInt): ICryptHash; override;
    function Pbkdf2(const secret, salt: RawUtf8; rounds: integer;
      out key: THash512Rec): integer; override;
  end;

  TCryptSignInternal = class(TCryptHash)
  protected
    fAlgo: TSynSigner;
    function InternalFinal(out dig: THash512Rec): PtrInt; override;
  public
    constructor Create(const signer: TSynSigner; const key: THash512Rec); overload;
    function Update(buf: pointer; buflen: PtrInt): ICryptHash; override;
  end;

const
  // CSV text of TSignAlgo items, as used for Signer/Sign factories naming
  SignAlgosText: PUtf8Char = 'hmac-sha1,hmac-sha256,hmac-sha384,hmac-sha512,' +
    'sha3-224,sha3-256,sha3-384,sha3-512,sha3-s128,sha3-s256';

constructor TCryptSignerInternal.Create(const name: RawUtf8);
begin
  fAlgo := TSignAlgo(InternalResolve(name, SignAlgosText));
  inherited Create(name); // should be done after InternalResolve()
end;

function TCryptSignerInternal.New(key: pointer; keylen: PtrInt): ICryptHash;
var
  s: TCryptSignInternal;
begin
  s := TCryptSignInternal.Create(self);
  s.fAlgo.Init(fAlgo, key, keylen);
  result := s;
end;

function TCryptSignerInternal.Pbkdf2(const secret, salt: RawUtf8;
  rounds: integer; out key: THash512Rec): integer;
var
  s: TSynSigner;
begin
  s.Pbkdf2(fAlgo, secret, salt, rounds, @key);
  result := s.SignatureSize;
end;


{ TCryptSignInternal }

constructor TCryptSignInternal.Create(const signer: TSynSigner;
  const key: THash512Rec);
begin
  // directly called by Sign(paramsjson) factory - fCryptAlgo is ignored here
  fAlgo.Init(signer.Algo, @key, signer.SignatureSize);
end;

function TCryptSignInternal.Update(buf: pointer; buflen: PtrInt): ICryptHash;
begin
  fAlgo.Update(buf, buflen);
  result := self;
end;

function TCryptSignInternal.InternalFinal(out dig: THash512Rec): PtrInt;
begin
  fAlgo.Final(@dig, {noinit=}false);
  result := fAlgo.SignatureSize;
end;


{ TCryptCipherAlgo }

function TCryptCipherAlgo.Encrypt(key: pointer): ICryptCipher;
begin
  result := New(key, {encrypt=}true);
end;

function TCryptCipherAlgo.Decrypt(key: pointer): ICryptCipher;
begin
  result := New(key, {encrypt=}false);
end;

function TCryptCipherAlgo.New(const hash, secret, salt: RawUtf8;
  rounds: integer; encrypt: boolean): ICryptCipher;
var
  s: TCryptSigner;
  key: THash512Rec;
begin
  s := Signer(hash);
  if s = nil then
    ECrypt.RaiseUtf8('%.New: unknown ''%'' hash', [self, hash]);
  FillZero(key.b); // s.Pbkdf2 may generate less bits than the cipher consumes
  s.Pbkdf2(secret, salt, rounds, key);
  result := New(@key, encrypt);
end;

function TCryptCipherAlgo.Encrypt(const sign, secret, salt: RawUtf8;
  rounds: integer): ICryptCipher;
begin
  result := New(sign, secret, salt, rounds, {encrypt=}true);
end;

function TCryptCipherAlgo.Decrypt(const sign, secret, salt: RawUtf8;
  rounds: integer): ICryptCipher;
begin
  result := New(sign, secret, salt, rounds, {encrypt=}false);
end;


{ TCryptAesInternal }

type
  TCryptAesInternal = class(TCryptCipherAlgo)
  protected
    fMode: TAesMode;
    fBits: integer;
    fEngines: TAesAbstractClasses;
  public
    constructor Create(const name: RawUtf8; mode: TAesMode; bits: integer;
      const engines: TAesAbstractClasses); reintroduce;
    function New(key: pointer; encrypt: boolean; iv: pointer): ICryptCipher; override;
    function IsAead: boolean; override;
  end;

  TCryptAesCipher = class(TCryptCipher)
  protected
    fAes: TAesAbstract;
    fFlags: set of (fEncrypt, fIVAtBeg, fAesGcm, fAesAead);
  public
    constructor Create(algo: TCryptAesInternal; key, iv: pointer;
      encrypt: boolean; const engines: TAesAbstractClasses); overload;
    destructor Destroy; override;
    function Clone: ICryptCipher; override;
    function Process(const src: RawByteString; out dst: RawByteString;
      const aeadinfo: RawByteString): boolean; overload; override;
    function Process(const src: TBytes; out dst: TBytes;
      const aeadinfo: TByteDynArray): boolean; overload; override;
    procedure RawProcess(src, dst: pointer; srclen, dstlen: PtrInt); override;
    function RawFinal(var gmac: TAesBlock): boolean; override;
  end;

constructor TCryptAesInternal.Create(const name: RawUtf8; mode: TAesMode;
  bits: integer; const engines: TAesAbstractClasses);
begin
  inherited Create(name);
  fMode := mode;
  fBits := bits;
  fEngines := engines;
end;

function TCryptAesInternal.New(key: pointer; encrypt: boolean; iv: pointer): ICryptCipher;
begin
  result := TCryptAesCipher.Create(self, key, iv, encrypt, fEngines);
end;

function TCryptAesInternal.IsAead: boolean;
begin
  result := fMode in AES_AEAD; // mCfc, mOfc, mCtc, mGcm
  if (fMode = mGcm) and
     (fEngines[mGcm] <> TAesInternal[mGcm]) then
    result := false; //
end;


{ TCryptAesCipher }

constructor TCryptAesCipher.Create(algo: TCryptAesInternal; key, iv: pointer;
  encrypt: boolean; const engines: TAesAbstractClasses);
begin
  inherited Create(algo);
  fAes := engines[algo.fMode].Create(key^, algo.fBits);
  if iv <> nil then
    fAes.IV := PAesBlock(iv)^
  else
    include(fFlags, fIVAtBeg);
  if encrypt then
    include(fFlags, fEncrypt);
  if algo.fMode = mGcm then
    include(fFlags, fAesGcm);
  if algo.fMode in AES_AEAD then
    include(fFlags, fAesAead); // mCfc,mOfc,mCtc,mGcm
end;

destructor TCryptAesCipher.Destroy;
begin
  inherited Destroy;
  fAes.Free;
end;

function TCryptAesCipher.Clone: ICryptCipher;
var
  c: TCryptAesCipher;
begin
  c := TCryptAesCipher.Create(fCryptAlgo);
  c.fAes := fAes.Clone;
  c.fFlags := fFlags;
  if not (fIVAtBeg in fFlags) then
    c.fAes.IV := fAes.IV;
  result := c;
end;

function TCryptAesCipher.Process(const src: RawByteString;
  out dst: RawByteString; const aeadinfo: RawByteString): boolean;
begin
  result := false;
  if src = '' then
    exit;
  if fAesAead in fFlags then
    // mCfc/mOfc/mCtc/mGcm AEAD algorithms using 128-bit GMAC or 256-bit crc32c
    dst := fAes.MacAndCrypt(src, fEncrypt in fFlags, fIVAtBeg in fFlags, aeadinfo)
  // standard encryption with no AEAD/checksum
  else if fEncrypt in fFlags then
    dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags)
  else
    dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags);
  result := dst <> '';
end;

function TCryptAesCipher.Process(const src: TBytes; out dst: TBytes;
  const aeadinfo: TByteDynArray): boolean;
begin
  result := false;
  if src = nil then
    exit;
  if fAesGcm in fFlags then
  begin
    // standard GCM algorithm with trailing 128-bit GMAC
    if aeadinfo <> nil then
      TAesGcmAbstract(fAes).AesGcmAad(pointer(aeadinfo), length(aeadinfo));
    if fEncrypt in fFlags then
    begin
      dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags, GMAC_SIZE);
      if dst <> nil then
        result := TAesGcmAbstract(fAes).AesGcmFinal( // append GMAC to dst
          PAesBlock(@PByteArray(dst)[length(dst) - GMAC_SIZE])^);
    end
    else
    begin
      dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags, false, GMAC_SIZE);
      if dst <> nil then
        result := TAesGcmAbstract(fAes).AesGcmFinal( // validate GMAC from src
          PAesBlock(@PByteArray(src)[length(src) - GMAC_SIZE])^);
    end;
    exit;
  end
  else if fAesAead in fFlags then
    ECrypt.RaiseUtf8('%.Process(TBytes) is unsupported for %',
      [self, fCryptAlgo.AlgoName]) // MacAndCrypt() requires RawByteString
  // standard encryption with no AEAD/checksum
  else if fEncrypt in fFlags then
    dst := fAes.EncryptPkcs7(src, fIVAtBeg in fFlags)
  else
    dst := fAes.DecryptPkcs7(src, fIVAtBeg in fFlags);
  result := {%H-}dst <> nil;
end;

procedure TCryptAesCipher.RawProcess(src, dst: pointer; srclen, dstlen: PtrInt);
begin
  if (dst = nil) and
     (fAesGcm in fFlags) then
    TAesGcmAbstract(fAes).AesGcmAad(src, srclen)
  else if fEncrypt in fFlags then
    fAes.Encrypt(src, dst, srclen)
  else
    fAes.Decrypt(src, dst, srclen)
end;

function TCryptAesCipher.RawFinal(var gmac: TAesBlock): boolean;
begin
  result := (fAesGcm in fFlags) and
            TAesGcmAbstract(fAes).AesGcmFinal(gmac);
end;


{ TCryptAsym }

procedure TCryptAsym.GeneratePem(out pub, priv: RawUtf8;
  const privpwd: RawUtf8);
var
  derpub, derpriv: RawByteString;
  pempub, pempriv: TPemKind;
begin // inherited classes should override at least one of those Generate*()
  GenerateDer(derpub, derpriv, privpwd);
  pempub := TPemKind(fPemPublic);
  if pempub = pemUnspecified then
    pempub := pemPublicKey;
  pempriv := TPemKind(fPemPrivate);
  if pempriv = pemUnspecified then
    pempriv := pemPrivateKey;
  pub := DerToPem(pointer(derpub), length(derpub), pempub);
  priv := DerToPem(pointer(derpriv), length(derpriv), pempriv);
  FillZero(derpriv); // anti-forensic
end;

procedure TCryptAsym.GenerateDer(out pub, priv: RawByteString; const privpwd: RawUtf8);
var
  pempub, pempriv: RawUtf8;
begin // inherited classes should override at least one of those Generate*()
  GeneratePem(pempub, pempriv, privpwd);
  pub := PemToDer(pempub);
  priv := PemToDer(pempriv);
  FillZero(pempriv); // anti-forensic
end;

function TCryptAsym.Sign(const msg, priv: RawByteString; out sig: RawByteString;
  const hashername, privpwd: RawUtf8): boolean;
begin
  result := Sign(Hasher(hashername), pointer(msg), length(msg), priv, sig, privpwd);
end;

function TCryptAsym.Sign(const msg, priv: TBytes; out sig: TBytes;
  const hashername, privpwd: RawUtf8): boolean;
var
  p, s: RawByteString;
begin
  BytesToRawByteString(priv, p);
  result := Sign(Hasher(hashername), pointer(msg), length(msg), p, s, privpwd);
  if result then
    RawByteStringToBytes(s, sig);
end;

function TCryptAsym.Verify(const msg, pub, sig: RawByteString;
  const hashername: RawUtf8): boolean;
begin
  result := Verify(Hasher(hashername), pointer(msg), length(msg), pub, sig);
end;

function TCryptAsym.Verify(const msg, pub, sig: TBytes;
  const hashername: RawUtf8): boolean;
var
  p, s: RawByteString;
begin
  BytesToRawByteString(pub, p);
  BytesToRawByteString(sig, s);
  result := Verify(Hasher(hashername), pointer(msg), length(msg), p, s);
end;


const
  /// per algorithm PrivateKeyEncrypt/PrivateKeyDecrypt salt
  // - ckaRsa/ckaRsaPss share the same public/private key files by definition
  // - ckaEcc256 matches EccPrivateKeyEncrypt/EccPrivateKeyDecrypt encoding
  CKA_SALT: array[TCryptKeyAlgo] of RawUtf8 = (
    '',           // ckaNone
    'synrsa',     // ckaRsa
    'synrsa',     // ckaRsaPss
    'synecc',     // ckaEcc256
    'syne384',    // ckaEcc384
    'syne512',    // ckaEcc512
    'synecck',    // ckaEcc256k
    'syneddsa');  // ckaEdDSA

  /// per algorithm PrivateKeyEncrypt/PrivateKeyDecrypt AF-32 rounds
  // - ckaRsa/ckaRsaPss share the same public/private key files by definition
  // - ckaEcc256 matches EccPrivateKeyEncrypt/EccPrivateKeyDecrypt encoding
  CKA_ROUNDS: array[TCryptKeyAlgo] of byte = (
    0,    // ckaNone
    3,    // ckaRsa
    3,    // ckaRsaPss
    31,    // ckaEcc256
    23,    // ckaEcc384
    15,    // ckaEcc512
    31,    // ckaEcc256k
    31);   // ckaEdDSA

{ TCryptAbstractKey }

function TCryptAbstractKey.KeyAlgo: TCryptKeyAlgo;
begin
  result := fKeyAlgo;
end;

function TCryptAbstractKey.Instance: TCryptAbstractKey;
begin
  result := self;
end;


{ TCryptPublicKey }

function TCryptPublicKey.VerifyDigest(Sig: pointer; Dig: THash512Rec;
  SigLen, DigLen: integer; Hash: THashAlgo): boolean;
begin
  result := false; // to be overriden if needed (not for OpenSSL)
end;

function TCryptPublicKey.Verify(Algorithm: TCryptAsymAlgo; Data, Sig: pointer;
  DataLen, SigLen: integer): boolean;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(CAA_HF[Algorithm], Data, DataLen, dig);
  result := (diglen <> 0) and
            VerifyDigest(Sig, dig, SigLen, diglen, CAA_HF[Algorithm]);
end;

function TCryptPublicKey.Verify(Algorithm: TCryptAsymAlgo;
  const Data, Sig: RawByteString): boolean;
begin
  result := Verify(Algorithm,
    pointer(Data), pointer(Sig), length(Data), length(Sig));
end;


{ TCryptPrivateKey }

function TCryptPrivateKey.Save(AsPem: boolean;
  const Password: SpiUtf8): RawByteString;
var
  der, bin: RawByteString;
  k: TPemKind;
begin
  // standard PKCS#8 encryption by default, or our proprietary format
  // - overriden in mormot.crypt.openssl to use PEVP_PKEY standard serialization
  if self = nil then
    FastAssignNew(result)
  else
  try
    // call overriden TCryptPrivateKeyEcc.ToDer and TCryptPrivateKeyRsa.ToDer
    der := ToDer;
    // persist in the expected (may be encrypted) format
    if Password = '' then
      // save as plain unencrypted PEM/DER
      if AsPem then
        if fKeyAlgo in CKA_RSA then
          k := pemRsaPrivateKey
        else
          k := pemEcPrivateKey
      else
        k := pemUnspecified // save as ccfBinary
    else if PrivateKeySavePkcs8 then
    begin
      // standard PKCS#8 encryptedPrivateKeyInfo, as OpenSSL -aes256
      bin := PrivateKeyToPkcs8(fKeyAlgo, der);
      if pointer(bin) <> pointer(der) then
        FillZero(der);
      der := Pkcs8EncryptPrivateKey(bin, Password);
      if AsPem then
        k := pemEncryptedPrivateKey
      else
        k := pemUnspecified;
    end
    else
    begin
      // our proprietary format, as recognized by older mORMot 2 versions
      bin := der; // for FillZero()
      der := PrivateKeyEncrypt(
               bin, CKA_SALT[fKeyAlgo], Password, CKA_ROUNDS[fKeyAlgo]);
      if AsPem then
        if fKeyAlgo in CKA_RSA then
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

function TCryptPrivateKey.FromDer(algo: TCryptKeyAlgo; const der: RawByteString;
  pub: TCryptPublicKey): boolean;
begin
  result := false; // to be overriden if needed (not for OpenSSL)
end;

function TCryptPrivateKey.SignDigest(const Dig: THash512Rec; DigLen: integer;
  DigAlgo: TCryptAsymAlgo): RawByteString;
begin
  FastAssignNew(result); // to be overriden if needed (not for OpenSSL)
end;

function TCryptPrivateKey.Load(Algorithm: TCryptKeyAlgo;
  const AssociatedKey: ICryptPublicKey; const PrivateKeySaved: RawByteString;
  const Password: SpiUtf8): boolean;
var
  saved, der: RawByteString;
  pub: TCryptPublicKey;
begin
  // support standard PKCS#8 PBES2 encryption, and our proprietary format
  // - overriden in mormot.crypt.openssl to use PEVP_PKEY standard serialization
  result := false;
  if (self = nil) or
     (fKeyAlgo <> ckaNone) or
     (Algorithm = ckaNone) or
     (PrivateKeySaved = '') then
    exit;
  try
    // compute the raw DER content (may be decrypt)
    saved := PrivateKeySaved;
    if Password <> '' then
    begin
      der := PemToDer(saved); // see also TCryptCertX509.Load
      // standard PKCS#8 encryptedPrivateKeyInfo, e.g. from OpenSSL -aes256
      saved := Pkcs8DecryptPrivateKey(der, Password);
      if saved = '' then
        // fallbac to mORMot proprietary PrivateKeyEncrypt() format
        saved := PrivateKeyDecrypt(
          der, CKA_SALT[Algorithm], Password, CKA_ROUNDS[Algorithm]);
      if saved = '' then
        exit;
    end;
    if Assigned(AssociatedKey) then
      pub := AssociatedKey.Instance as TCryptPublicKey
    else
      pub := nil;
    // call overriden TCryptPrivateKeyEcc.FromDer and TCryptPrivateKeyRsa.FromDer
    if not FromDer(Algorithm, saved, pub) then
      exit;
    fKeyAlgo := Algorithm;
    result := true;
  finally
    FillZero(saved);
    FillZero(der);
  end;
end;

function TCryptPrivateKey.Sign(Algorithm: TCryptAsymAlgo;
  Data: pointer; DataLen: integer): RawByteString;
var
  hasher: TSynHasher;
  dig: THash512Rec;
  diglen: PtrInt;
begin
  diglen := hasher.Full(CAA_HF[Algorithm], Data, DataLen, dig);
  result := SignDigest(dig, diglen, Algorithm);
end;

function TCryptPrivateKey.Sign(Algorithm: TCryptAsymAlgo;
  const Data: RawByteString): RawByteString;
begin
  result := Sign(Algorithm, pointer(Data), length(Data));
end;

function TCryptPrivateKey.SharedSecret(
  const PeerKey: ICryptPublicKey): RawByteString;
begin
  FastAssignNew(result); // unsupported by this algorithm (only ECC by now)
end;


{ TCryptCertAlgo }

function TCryptCertAlgo.New: ICryptCert;
begin
  result := nil; // unsupported
end;

function TCryptCertAlgo.FromHandle(Handle: pointer): ICryptCert;
begin
  result := nil; // unsupported
end;

function TCryptCertAlgo.Load(const Saved: RawByteString;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): ICryptCert;
begin
  result := New;
  if Assigned(result) then // some classes may return nil
    if not result.Load(Saved, Content, PrivatePassword) then
      result := nil;
end;

function TCryptCertAlgo.Generate(Usages: TCryptCertUsages;
  const Subjects: RawUtf8; const Authority: ICryptCert; ExpireDays: integer;
  ValidDays: integer; Fields: PCryptCertFields): ICryptCert;
begin
  result := New;
  if Assigned(result) then // some classes may return nil
    result.Generate(Usages, Subjects, Authority, ExpireDays, ValidDays, Fields);
end;

function TCryptCertAlgo.CreateSelfSignedCsr(const Subjects: RawUtf8;
  const PrivateKeyPassword: SpiUtf8; var PrivateKeyPem: RawUtf8;
  Usages: TCryptCertUsages; Fields: PCryptCertFields): RawUtf8;
var
  csr: ICryptCert; // temporary certificate instance
begin
  FastAssignNew(result);
  if PrivateKeyPem <> '' then
    ECryptCert.RaiseUtf8('%.CreateSelfSignedCsr % does not support ' +
      'a custom private key', [self, AlgoName]);
  // by default, just generate a self-signed certificate as CSR
  csr := New;
  if not Assigned(csr) then
    exit; // e.g. from TCryptCertAlgoCng or TCryptCertAlgoPkcs11
  csr.Generate(Usages, Subjects, nil, 365, -1, Fields);
  PrivateKeyPem := csr.Save(cccPrivateKeyOnly, PrivateKeyPassword);
  result := csr.Save(cccCertOnly, '', ccfPem);
  // fields are ignored with our syn-ecc encoding anyway
end;

function TCryptCertAlgo.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
begin
  result := New;
  if Assigned(result) then // some classes may return nil
    result.GenerateFromCsr(Csr, Authority, ExpireDays, ValidDays);
end;

function TCryptCertAlgo.JwtName: RawUtf8;
begin
  result := CAA_JWT[fCaa];
end;


{ TCryptCert }

procedure TCryptCert.RaiseError(const Msg: ShortString);
begin
  raise ECryptCert.CreateUtf8('%.%', [self, Msg]);
end;

procedure TCryptCert.RaiseError(const Fmt: RawUtf8;
  const Args: array of const);
var
  msg: ShortString;
begin
  FormatShort(Fmt, Args, msg);
  RaiseError(msg);
end;

procedure TCryptCert.RaiseErrorGenerate(const api: ShortString);
begin
  RaiseError('Generate: % error', [api]); // raise ECryptCert
end;

procedure TCryptCert.EnsureCanWrite(const Context: ShortString);
begin
  if (fIndexer <> nil) and
     not IsVoid then
    RaiseError('% not allowed: currently indexed by a %', [Context, fIndexer]);
end;

class procedure TCryptCert.InternalFind(Cert: PICryptCert;
  const Value: RawByteString; Method: TCryptCertComparer;
  Count, MaxCount: integer; out Chain: ICryptCerts);
var
  found: boolean;
  res: integer;
begin
  // O(n) efficient search loop with some temporary memory allocation
  res := 0;
  while Count <> 0 do
  begin
    case Method of
      ccmSerialNumber:
        found := HumanHexCompare(Cert^.GetSerial, Value) = 0;
      ccmSubjectName:
        found := EqualBuf(Cert^.GetSubjectName, Value);
      ccmIssuerName:
        found := EqualBuf(Cert^.GetIssuerName, Value);
      ccmSubjectCN:
        found := PropNameEquals(Cert^.GetSubject('CN'), Value);
      ccmIssuerCN:
        found := PropNameEquals(Cert^.GetIssuer('CN'), Value);
      ccmSubjectKey:
        found := HumanHexCompare(Cert^.GetSubjectKey, Value) = 0;
      ccmAuthorityKey,
      ccmIssuedBy:
        found := CsvContains(Cert^.GetAuthorityKey, Value);
      ccmSubjectAltName:
        found := FindRawUtf8(Cert^.GetSubjects, Value, {casesens=}false) >= 0;
      ccmIssuerAltName:
        found := FindRawUtf8(Cert^.GetIssuers, Value, {casesens=}false) >= 0;
      ccmBinary:
        found := EqualBuf(Cert^.Save, Value);
      ccmSha1:
        found := HumanHexCompare(Cert^.GetDigest(hfSha1), Value) = 0;
      ccmSha256:
        found := HumanHexCompare(Cert^.GetDigest(hfSha256), Value) = 0;
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

function TCryptCert.GenerateFromCsr(const Csr: RawByteString;
  const Authority: ICryptCert; ExpireDays, ValidDays: integer): ICryptCert;
var
  x: ICryptCert;
begin
  EnsureCanWrite('GenerateFromCsr');
  // by default, CreateSelfSignedCsr generates a self-signed certificate as CSR
  // - mormot.crypt.openssl and mormot.crypt.x509 will generate a proper CSR
  result := nil;
  if (Csr = '') or
     (fCryptAlgo = nil) then
    exit;
  x := CertAlgo.Load(Csr);
  if (x <> nil) and
     (x.Verify(x) = cvValidSelfSigned) then
    result := Generate(x.GetUsage, RawUtf8ArrayToCsv(x.GetSubjects),
      Authority, ExpireDays, ValidDays, {Fields=}nil);
    // note: Fields=nil since TCryptCertInternal does not support them
end;

function TCryptCert.GetFields(var fields: TCryptCertFields; withexts: boolean): boolean;
begin
  result := false; // not supported by default (e.g. for syn-es256)
end;

function TCryptCert.IsAuthorizedBy(const Authority: ICryptCert): boolean;
var
  akid: RawUtf8;
begin
  result := false;
  if (Authority = nil) or
     not PropNameEquals(GetIssuerName, Authority.GetSubjectName) then
    exit; // IssuerDN should match Authority.SubjectDN
  akid := GetAuthorityKey;
  result := (akid = '') or // self-signed
            PropNameEquals(akid, Authority.GetSubjectKey); // AKI = SKI
end;

function TCryptCert.Compare(const Another: ICryptCert;
  Method: TCryptCertComparer): integer;
begin
  if Assigned(Another) and
     (Another.Handle <> nil) then
    case Method of
      ccmSerialNumber:
        result := HumanHexCompare(GetSerial, Another.GetSerial);
      ccmSubjectName:
        result := CompareBuf(GetSubjectName, Another.GetSubjectName);
      ccmIssuerName:
        result := CompareBuf(GetIssuerName, Another.GetIssuerName);
      ccmSubjectCN:
        result := CompareBuf(GetSubject('CN'), Another.GetSubject('CN'));
      ccmIssuerCN:
        result := CompareBuf(GetIssuer('CN'), Another.GetIssuer('CN'));
      ccmSubjectKey:
        result := HumanHexCompare(GetSubjectKey, Another.GetSubjectKey);
      ccmAuthorityKey:
        result := HumanHexCompare(GetAuthorityKey, Another.GetAuthorityKey);
      ccmSubjectAltName:
        result := CompareBuf(RawUtf8ArrayToCsv(GetSubjects),
                    RawUtf8ArrayToCsv(Another.GetSubjects));
      ccmIssuerAltName:
        result := CompareBuf(RawUtf8ArrayToCsv(GetIssuers),
                    RawUtf8ArrayToCsv(Another.GetIssuers));
      ccmUsage:
        result := word(GetUsage) - word(Another.GetUsage);
      ccmBinary:
        result := CompareBuf(Save(cccCertOnly, '', ccfBinary),
                             Another.Save(cccCertOnly, '', ccfBinary));
      ccmSha1:
        result := HumanHexCompare(GetDigest(hfSHA1), Another.GetDigest(hfSHA1));
      ccmSha256:
        result := HumanHexCompare(GetDigest(hfSHA256), Another.GetDigest(hfSHA256));
      ccmIssuedBy:
        result := CompareBuf(GetAuthorityKey, Another.GetSubjectKey);
    else // e.g. ccmInstance
      result := ComparePointer(pointer(self), pointer(Another));
    end
  else
    result := 1;
end;

function TCryptCert.IsEqual(const Another: ICryptCert): boolean;
begin
  result := Compare(Another, ccmBinary) = 0;
end;

function TCryptCert.IsValidDate(date: TDateTime): boolean;
begin
  result := (Handle <> nil) and
            IsCertValidDate(date, GetNotAfter, GetNotBefore);
end;

function TCryptCert.IsVoid: boolean;
begin
  result := Handle = nil;
end;

function TCryptCert.GetDigest(Algo: THashAlgo): RawUtf8;
begin
  result := HashFull(Algo, Save(cccCertOnly, '', ccfBinary));
end;

function TCryptCert.LoadFromFile(const Source: TFileName;
  Content: TCryptCertContent; const PrivatePassword: SpiUtf8): boolean;
var
  s: RawByteString;
begin
  EnsureCanWrite('LoadFromFile');
  fLastLoadFromFileName := Source;
  s := StringFromFile(Source);
  result := Load(s, Content, PrivatePassword);
  FillZero(s); // may be a private key with no password :(
end;

function TCryptCert.GetFileName: TFileName;
begin
  result := fLastLoadFromFileName;
end;

function TCryptCert.Save(Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat): RawByteString;
begin
  // overriden call only for ccfHexa, ccfBase64 and ccfBase64Uri encoding
  result := Save(Content, PrivatePassword, ccfBinary);
  case Format of
    ccfHexa:
      result := BinToHex(result);
    ccfBase64:
      result := BinToBase64(result);
    ccfBase64Uri:
      result := BinToBase64uri(result);
  else
    ECryptCert.RaiseUtf8('Unexpected %.Save', [self]); // paranoid
  end;
end;

procedure TCryptCert.SaveToFile(const Dest: TFileName; Content: TCryptCertContent;
  const PrivatePassword: SpiUtf8; Format: TCryptCertFormat);
var
  s: RawByteString;
  fn: TFileName;
begin
  fn := Dest;
  if fn = '' then
    fn := fLastLoadFromFileName;
  s := Save(Content, PrivatePassword, Format);
  FileFromString(s, fn);
  FillZero(s); // may be a private key with no password :(
end;

function TCryptCert.Sign(const Data: RawByteString;
  Usage: TCryptCertUsage): RawByteString;
begin
  result := Sign(pointer(Data), length(Data), Usage);
end;

function TCryptCert.Verify(const Signature, Data: RawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := Verify(pointer(Signature), pointer(Data),
                   length(Signature), length(Data), IgnoreError, TimeUtc);
end;

function TCryptCert.JwtCompute(const DataNameValue: array of const;
  const Issuer, Subject, Audience: RawUtf8; NotBefore: TDateTime;
  ExpirationMinutes: integer; Signature: PRawByteString): RawUtf8;
var
  payload: TDocVariantData;
  headpayload: RawUtf8;
  sig: RawByteString;
begin
  FastAssignNew(result);
  if not HasPrivateSecret then
    exit;
  // cut-down version of TJwtAbstract.PayloadToJson
  payload.InitObject(DataNameValue, JSON_FAST);
  if Issuer <> '' then
    payload.AddValueText('iss', Issuer);
  if Subject <> '' then
    payload.AddValueText('sub', Subject);
  if Audience <> '' then
    payload.AddValueText('aud', Audience);
  if NotBefore > 0 then
    payload.AddValue('nbf', DateTimeToUnixTime(NotBefore));
  if ExpirationMinutes > 0 then
    payload.AddValue('exp', UnixTimeUtc + ExpirationMinutes * 60);
  if payload.Count = 0 then
    exit; // we need something to sign
  // see TJwtAbstract.Compute
  headpayload := BinToBase64Uri(FormatUtf8('{"alg":"%"}', [CertAlgo.JwtName])) +
           '.' + BinToBase64Uri(payload.ToJson);
  sig := self.Sign(headpayload);
  if sig = '' then
    exit;
  if Signature <> nil then
    Signature^ := sig;
  result := headpayload + '.' + BinToBase64Uri(sig);
end;

function TCryptCert.JwtVerify(const Jwt: RawUtf8;
  Issuer, Subject, Audience: PRawUtf8;
  Payload: PDocVariantData; Signature: PRawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
var
  P, S: PUtf8Char;
  pl: TDocVariantData;
  ms: Int64;
  head, payl, sig: RawUtf8;
begin
  // same logic than TJwtAbstract.Verify, but (slower and) with no cache
  // -> TJwtAbstract is to be preferred if the ICryptCert is reused
  result := cvWrongUsage;
  P := PosCharU(Jwt, '.'); // use fast SSE2 asm on x86_64
  if P = nil then
    exit;
  S := PosChar(P + 1, '.');
  if S = nil then
    exit;
  head := Base64UriToBin(pointer(Jwt), P - pointer(Jwt));
  if JsonDecode(head, 'alg') <> CertAlgo.JwtName then
    exit;
  inc(P);
  payl := Base64UriToBin(pointer(P), S - P);
  inc(S);
  sig := Base64UriToBin(PAnsiChar(S), StrLen(S));
  if (payl = '') or
     (sig = '') then
    exit;
  if Signature <> nil then
    Signature^ := sig;
  if pl.InitJsonInPlace(pointer(payl), JSON_FAST) = nil then
    exit;
  result := cvInvalidDate;
  if pl.GetAsInt64('exp', ms) and
     (UnixTimeUtc > ms) then
    exit;
  if pl.GetAsInt64('nbf', ms) and
     (UnixTimeUtc + 15 < ms) then
    exit;
  result := Verify(pointer(sig), pointer(Jwt),
              length(sig), S - 1 - pointer(Jwt), IgnoreError, TimeUtc);
  if not (result in CV_VALIDSIGN) then
    exit;
  if Issuer <> nil then
    Issuer^ := pl.U['iss'];
  if Subject <> nil then
    Subject^ := pl.U['sub'];
  if Audience <> nil then
    Audience^ := pl.U['aud'];
  if Payload <> nil then
    Payload^ := pl;
end;

function TCryptCert.JwkCompute: RawUtf8;
var
  x, y: RawByteString;
begin
  // retrieve raw public key parameters and export as JWT
  FastAssignNew(result);
  if GetKeyParams(x, y) then
    result := SaveAsJwk(AsymAlgo, x, y);
end;

function TCryptCert.SharedSecret(const pub: ICryptCert): RawByteString;
begin
  FastAssignNew(result); // unsupported by default
end;

function TCryptCert.AsymAlgo: TCryptAsymAlgo;
begin
  result := (fCryptAlgo as TCryptCertAlgo).AsymAlgo;
end;

function TCryptCert.CertAlgo: TCryptCertAlgo;
begin
  if fCryptAlgo = nil then
    result := nil
  else
    result := fCryptAlgo as TCryptCertAlgo;
end;

function TCryptCert.Instance: TCryptCert;
begin
  result := self;
end;

function TCryptCert.PrivateKeyHandle: pointer;
begin
  result := nil; // unsupported
end;

function TCryptCert.GetKeyParams(out x, y: RawByteString): boolean;
begin
  result := false; // unsupported
end;

function TCryptCert.GetExtensions: TCryptCustomExts;
var
  fields: TCryptCertFields; // fallback e.g. for OpenSSL
begin
  if GetFields(fields, {withexts=}true) then
    result := fields.CustomExts
  else
    result := nil;
end;


{ TCryptStore }

constructor TCryptStore.Create(algo: TCryptAlgo);
begin
  fTrust := DefaultCryptStoreTrust;
  inherited Create(algo);
end;

destructor TCryptStore.Destroy;
begin
  inherited Destroy;
  fCache.Free;
end;

function TCryptStore.Load(const Saved: RawByteString): boolean;
begin
  Clear;
  result := AddFromBuffer(Saved) <> nil; // expect chain of PEM Cert + CRLs
end;

procedure TCryptStore.SetTrust(const Value: TCertStoreTrusts);
begin
  fTrust := Value;
end;

function TCryptStore.GetTrust: TCertStoreTrusts;
begin
  result := fTrust;
end;

function TCryptStore.Cache: TCryptCertCache;
begin
  result := fCache;
end;

function TCryptStore.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCert;
begin
  case Method of
    ccmSerialNumber:
      result := GetBySerial(Value);
    ccmSubjectKey:
      result := GetBySubjectKey(Value);
  else
    result := nil; // other methods are unsupported by default
  end;
end;

function TCryptStore.Add(const cert: array of ICryptCert): TRawUtf8DynArray;
var
  i: PtrInt;
begin
  result := nil;
  for i := 0 to high(cert) do
    if Add(cert[i]) then
      AddRawUtf8(result, cert[i].GetSerial);
end;

function TCryptStore.AddFromFile(const FileName: TFileName): TRawUtf8DynArray;
var
  tmp: RawByteString;
begin
  tmp := StringFromFile(FileName);
  if tmp <> '' then
    result := AddFromBuffer(tmp)
  else
    result := nil;
end;

type
  TDirectoryCryptStore = class(TDirectoryBrowser) // support MAX_PATH on Windows
  protected
    fStore: TCryptStore;
    fSerials: TRawUtf8DynArray;
    fCount: integer;
    function OnFile(const FileInfo: TSearchRec;
      const FullFileName: TFileName): boolean; override;
  end;

function TDirectoryCryptStore.OnFile(const FileInfo: TSearchRec;
  const FullFileName: TFileName): boolean;
begin
  if FileInfo.Size < 65535 then // certificate files are expected to be < 64KB
     AddRawUtf8(fSerials, fCount, fStore.AddFromFile(FullFileName));
  result := true; // continue;
end;

function TCryptStore.AddFromFolder(const Folder, Mask: TFileName;
  Recursive: boolean): TRawUtf8DynArray;
var
  browser: TDirectoryCryptStore;
begin
  browser := TDirectoryCryptStore.Create(Folder, [Mask], Recursive);
  try
    browser.fStore := self;
    browser.Run;
    if browser.fCount <> 0 then
      DynArrayFakeLength(browser.fSerials, browser.fCount);
    result := browser.fSerials;
  finally
    browser.Free;
  end;
end;

function TCryptStore.IsValidChain(const chain: ICryptCertChain; date: TDateTime;
  ignore: TCryptCertValidities): TCryptCertValidity;
var
  i, n: PtrInt;
  c: ICryptCertChain;
  u: array of TCryptCertUsages;
  p: TIntegerDynArray;
  plen: integer;
begin
  // we need something to validate
  result := cvBadParameter;
  if (chain = nil) or
     (chain[0] = nil) then
    exit;
  // ensure main certificate is not deprecated
  result := cvInvalidDate;
  if not (result in ignore) and
     not chain[0].IsValidDate(date) then
    exit;
  // compute the exact authority sequence (if not supplied in proper order)
  result := cvUnknownAuthority;
  c := ChainConsolidate(chain);
  n := length(c);
  if (n = 0) or
     ((n = 1) and
      not c[0].IsSelfSigned) then
    exit;
  // cache some parameters of the chain
  SetLength(u, n);
  SetLength(p, n);
  for i := 0 to n - 1 do
    u[i] := c[i].GetUsage(@p[i]);
  // check the usages of all intermediate certificates
  result := cvWrongUsage;
  if not (result in ignore) then
    for i := 1 to n - 2 do // skip u[0] leaf and u[n-1]trust anchor
      if not IsCertAuthority(u[i]) then
        exit; // check cA=TRUE and keyCertSign
  // check pathLenConstraint according to RFC 5280
  result := cvExhaustedPathLen;
  plen := 0;
  if not (result in ignore) then
    // apply each CA's constraint (if any) against the path already seen
    for i := 1 to n - 1 do            // skip u[0] leaf (but check trust anchor)
      if cuCA in u[i] then            // only count cA certificates
      begin
        if (p[i] >= 0) and            // has pathLenConstraint
           (plen > p[i]) then         // cvExhaustedPathLen
          exit;
        if (i < n - 1) and            // still an intermediate (no trust anchor)
           not c[i].IsSelfSigned then // self-issued are exempt
          inc(plen);
      end;
  // ensure no certificate in the sequence has been explicitly revoked
  result := cvRevoked;
  if not (result in ignore) then
    for i := 0 to n - 1 do
      if IsRevoked(c[i]) <> crrNotRevoked then
        exit;
  // check the cascaded dates (before any digital signature verification)
  result := cvDeprecatedAuthority;
  if not (result in ignore) then
    for i := 1 to n - 1 do
      if not c[i].IsValidDate(c[i - 1].GetNotBefore) then
        exit;
  // check the cascaded digital signatures
  for i := 0 to n - 2 do
  begin
    result := c[i].Verify(c[i + 1], [cvWrongUsage, cvDeprecatedAuthority]);
    // note: TCryptCertX509.Verify has a per-authority cache so is very fast
    if result <> cvValidSigned then
      exit;
  end;
  // eventually check the trusted anchor of the chain
  if n > 1 then
    date := c[n - 2].GetNotBefore; // anchor is not main: adjust date
  result := IsValid(c[n - 1], date);
end;

function TCryptStore.Verify(const Signature, Data: RawByteString;
  IgnoreError: TCryptCertValidities; TimeUtc: TDateTime): TCryptCertValidity;
begin
  result := Verify(Signature, pointer(Data), length(Data), IgnoreError, TimeUtc);
end;

function TCryptStore.DefaultCertAlgo: TCryptCertAlgo;
begin
  result := (fCryptAlgo as TCryptStoreAlgo).DefaultCertAlgo;
end;


{ TCryptStoreAlgo }

function TCryptStoreAlgo.NewFrom(const Binary: RawByteString): ICryptStore;
begin
  result := New;
  if not result.Load(Binary) then
    result := nil;
end;


{ TCryptCertAbstractList }

procedure TCryptCertAbstractList.SetCryptCertClass(c: TCryptCertClass);
begin
  fCryptCertClass := c;
end;

function TCryptCertAbstractList.GetCount: integer;
begin
  result := fList.Count;
end;

destructor TCryptCertAbstractList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TCryptCertAbstractList.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  result := nil;
  if (self = nil) or
     (fList.Count = 0) or
     (Value = '') then
    exit;
  fList.Safe.ReadLock;
  try
    // non-blocking O(n) search - overriden in TCryptCertX509 for performance
    if fCryptCertClass <> nil then
      fCryptCertClass.InternalFind(
        fList.Values.Value^, Value, Method, fList.Count, MaxCount, result);
  finally
    fList.Safe.ReadUnLock;
  end;
end;

function TCryptCertAbstractList.FindOne(const Value: RawByteString;
  Method: TCryptCertComparer): ICryptCert;
var
  res: ICryptCerts;
begin
  res := Find(Value, Method, 1);
  if res = nil then
    result := nil
  else
    result := res[0];
end;

function TCryptCertAbstractList.List: ICryptCerts;
begin
  fList.CopyValues(result);
end;

procedure TCryptCertAbstractList.SaveToPem(W: TTextWriter;
  WithExplanatoryText: boolean);
var
  i: PtrInt;
  c: ^ICryptCert;
begin
  fList.Safe.ReadLock;
  try
    c := fList.Values.Value^;
    for i := 1 to fList.Count do
    begin
      if WithExplanatoryText then
        // see https://datatracker.ietf.org/doc/html/rfc7468#section-5.2
        W.Add('Subject: %'#13#10'Issuer: %'#13#10'Validity: from %Z to %Z'#13#10,
         [c^.GetSubjectName, c^.GetIssuerName, DateTimeToIso8601Short(
            c^.GetNotBefore), DateTimeToIso8601Short(c^.GetNotAfter)]);
      W.AddString(c^.Save(cccCertOnly, '', ccfPem));
      W.AddCR;
      inc(c);
    end;
  finally
    fList.Safe.ReadUnLock;
  end;
end;


{ TCryptCertCache }

function TCryptCertCache.OnDelete(const aKey, aValue; aIndex: integer): boolean;
begin
  // return true to delete the deprecated item - only if not currently in use
  result := ICryptCert(aValue).Instance.RefCount = 1;
end;

constructor TCryptCertCache.Create(TimeOutSeconds: integer);
begin
  fList := TSynDictionary.Create(TypeInfo(TRawByteStringDynArray), // DER
    TypeInfo(ICryptCerts), {caseins=}false, TimeOutSeconds);
  fList.OnCanDeleteDeprecated := OnDelete;
  fList.ThreadUse := uRWLock; // non-blocking Load() and Find()
end;

function TCryptCertCache.Load(const Cert: RawByteString): ICryptCert;
var
  der: RawByteString;
  inst: TCryptCert;
begin
  result := nil;
  // normalize and validate input
  if AsnDecChunk(Cert) then
    der := Cert
  else
  begin
    der := PemToDer(Cert);
    if not AsnDecChunk(der) then
      exit;
  end;
  // try to retrieve and share an existing instance
  if fList.FindAndCopy(der, result) then
    exit;
  // we need to create a new TCryptCert instance
  result := InternalLoad(der);
  if result = nil then
    exit;
  // ensure it has a coherent implementation class
  inst := result.Instance;
  if pointer(fCryptCertClass) <> PPointer(inst)^ then
    if fCryptCertClass <> nil then
      exit // return the instance, but don't cache it
    else
      fCryptCertClass := PPointer(inst)^;
  inst.fIndexer := self; // don't touch once indexed
  // add this new instance to the internal cache
  if fList.Count > 128 then
    fList.DeleteDeprecated; // make some room (once a second and if RefCount=1)
  fList.Add(der, result);   // der key will be shared with TX509.fCachedDer
end;

function TCryptCertCache.Load(const Cert: array of RawByteString): ICryptCerts;
var
  i, n: PtrInt;
begin
  result := nil;
  SetLength(result, length(Cert));
  n := 0;
  for i := 0 to high(Cert) do
  begin
    result[n] := Load(Cert[i]);
    if Assigned(result[n]) then
      inc(n);
  end;
  SetLength(result, n);
end;

function TCryptCertCache.LoadPem(const Pem: RawUtf8): ICryptCerts;
var
  p: PUtf8Char;
  k: TPemKind;
  der: TCertDer;
  c: ICryptCert;
begin
  result := nil;
  p := pointer(Pem);
  if p <> nil then
    repeat
      der := NextPemToDer(p, @k);
      if der = '' then
        break;
      if not (k in [pemUnspecified, pemCertificate, pemSynopseCertificate]) then
        continue; // no need to try loading something which is not a certificate
      c := Load(der);
      if c <> nil then
        ChainAdd(result, c);
    until false;
end;

function TCryptCertCache.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  if Method = ccmBinary then
  begin
    SetLength(result, 1);
    if not fList.FindAndCopy(Value, result) then
      result := nil;
  end
  else
    result := inherited Find(Value, Method, MaxCount);
end;

function TCryptCertCache.NewList: TCryptCertList;
begin
  result := TCryptCertList.Create;
  result.fCryptCertClass := fCryptCertClass; // propagate the class
end;

function TCryptCertCache.NewList(const Pem: RawUtf8): TCryptCertList;
begin
  result := NewList;
  result.Add(LoadPem(Pem));
end;


{ TCryptCertList }

constructor TCryptCertList.Create;
begin
  inherited Create;
  fList := TSynDictionary.Create(TypeInfo(TRawByteStringDynArray), // key=SKID
    TypeInfo(ICryptCerts));
  fList.ThreadUse := uRWLock; // non-blocking Find()
end;

function TCryptCertList.Add(const Cert: ICryptCert): boolean;
var
  bin: RawByteString;
  inst: TCryptCert;
begin
  result := false;
  if not Assigned(Cert) then
    exit;
  inst := Cert.Instance;
  if pointer(fCryptCertClass) <> PPointer(inst)^ then
    if fCryptCertClass = nil then
      fCryptCertClass := PPointer(inst)^
    else
      ECryptCert.RaiseUtf8('%.Add(%) but we already store %',
        [self, inst, fCryptCertClass]);
  inst.fIndexer := self; // don't touch once indexed
  result := HumanHexToBin(inst.GetSubjectKey, bin) and
            (fList.Add(bin, Cert) >= 0);
end;

procedure TCryptCertList.Add(const Cert: array of ICryptCert);
var
  i: PtrInt;
begin
  for i := 0 to high(Cert) do
    Add(Cert[i]);
end;

function TCryptCertList.FindBySubjectKey(const Key: RawUtf8): ICryptCert;
var
  bin: RawByteString;
begin
  result := nil;
  if HumanHexToBin(Key, bin) then
    fList.FindAndCopy(bin, result);
end;

function TCryptCertList.FindBySubjectKeyRaw(const Key: RawByteString): ICryptCert;
begin
  fList.FindAndCopy(Key, result);
end;

function TCryptCertList.Find(const Value: RawByteString;
  Method: TCryptCertComparer; MaxCount: integer): ICryptCerts;
begin
  if Method = ccmSubjectKey then
  begin
    SetLength(result, 1);
    result[0] := FindBySubjectKey(Value); // use the TSynDictionary hash table
    if result[0] = nil then
      result := nil;
  end
  else
    result := inherited Find(Value, Method, MaxCount);
end;

function TCryptCertList.DeleteBySubjectKey(const Key: RawUtf8): boolean;
var
  bin: RawByteString;
begin
  result := HumanHexToBin(Key, bin) and
            (fList.Delete(Key) >= 0);
end;


procedure ChainAdd(var chain: ICryptCertChain; const cert: ICryptCert);
begin
  InterfaceArrayAdd(chain, cert);
end;

function ChainFind(var chain: ICryptCertChain; const cert: ICryptCert;
  comparer: TCryptCertComparer): PtrInt;
begin
  if comparer = ccmInstance then
    result := InterfaceArrayFind(chain, cert)
  else
  begin
    for result := 0 to length(chain) - 1 do
      if cert.Compare(chain[result], comparer) = 0 then
        exit;
    result := -1;
  end;
end;

function ChainConsolidate(const chain: ICryptCertChain): ICryptCertChain;
var
  ref: array[0..127] of pointer; // weak references of ICryptCert
  count, n: PtrInt;

  procedure RecursiveCompute(var one: pointer);
  var
    i: PtrInt;
    r: PPointer;
  begin
    result[n] := ICryptCert(one);
    inc(n);
    if n = count then
      exit; // paranoid
    r := @ref[1];
    for i := 1 to count - 1 do
      if (r^ <> nil) and
         ICryptCert(one).IsAuthorizedBy(ICryptCert(r^)) then
      begin
        one := nil; // faster and avoid endless loop on circular references
        RecursiveCompute(r^);
        break;
      end
      else
        inc(r);
  end;

begin
  result := nil;
  count := length(chain);
  if (chain = nil) or
     (chain[0] = nil) or
     (count >= high(ref)) then // a typical chain has 2 or 3 certificates
    exit;
  n := 0;
  SetLength(result, count);
  MoveFast(pointer(chain)^, ref[0], count * SizeOf(ref[0]));
  RecursiveCompute(ref[0]); // fill result[0..n-1] in auth order
  DynArrayFakeLength(result, n);
end;

procedure AddCustomExts(var exts: TCryptCustomExts; const o, v: RawByteString; crit: boolean);
var
  n: PtrInt;
begin
  n := length(exts);
  SetLength(exts, n + 1);
  with exts[n] do
  begin
    Oid := o;
    Value := v;
    Critical := crit;
  end;
end;

procedure AddCustomExts(var exts: TCryptCustomExts; const o: TAsnBuffer;
  const v: RawByteString; crit: boolean);
var
  oid: RawByteString;
begin
  FastSetRawByteString(oid, o.Data, o.Len); // seldom called
  AddCustomExts(exts, oid, v, crit);
end;

function FindCustomExts(const Other: TCryptCustomExts; OidText: PUtf8Char): RawByteString;
begin
  result := FindCustomExtsAsn(pointer(Other), length(Other), AsnEncOid(OidText));
end;

function FindCustomExtsAsn(o: PCryptCustomExt; n: integer; const b: TAsnObject): RawByteString;
begin
  FastAssignNew(result);
  if (o <> nil) and
     (n > 0) and
     (b <> '') then
    repeat
      if SortDynArrayRawByteString(o^.Oid, b) = 0 then // O(n) search
      begin
        result := o^.Value;
        exit;
      end;
      inc(o);
      dec(n);
    until n = 0;
end;

function FindAia(const ext: TCryptCustomExts; var ocsp, issuers: TRawUtf8DynArray): boolean;
var
  aia: TAsnObject;
begin
  aia := FindCustomExts(ext, ASN1_OID_AIA);
  result := (aia <> '') and
            AsnDecAia(aia, ocsp, issuers);
end;

function FindCdp(const ext: TCryptCustomExts): TRawUtf8DynArray;
begin
  result := AsnDecCdp(FindCustomExts(ext, ASN1_OID_CDP));
end;


{ TCryptCertPerUsage }

procedure TCryptCertPerUsage.Clear;
begin
  List := nil;
  FillCharFast(self, SizeOf(self), 0);
end;

function TCryptCertPerUsage.IsVoid: boolean;
begin
  result := List = nil;
end;

function TCryptCertPerUsage.Add(const cert: ICryptCert): TCryptCertUsages;
var
  u: TCryptCertUsage;
  n: PtrInt;
begin
  result := [];
  if cert = nil then
    exit;
  result := cert.GetUsage;
  if result = [] then
    exit;
  n := length(List);
  if n >= 255 then
    ECryptCert.RaiseU('TCryptCertPerUsage.Add overflow'); // paranoid
  SetLength(List, n + 1);
  List[n] := cert;
  inc(n); // CertPerUsage[u] stores index + 1, i.e. in 1..255 range
  for u := low(u) to high(u) do
    if u in result then
    begin
      include(Usages, u);
      if Index[u] = 0 then
        exclude(result, u);
      Index[u] := n; // replace any existing certificate
    end;
end;

function TCryptCertPerUsage.GetUsage(u: TCryptCertUsage;
  var cert: ICryptCert): boolean;
var
  i: PtrInt;
begin
  if List = nil then
    i := 0
  else
    i := Index[u]; // contains index + 1
  if i = 0 then
  begin
    cert := nil; // circumvent FPC inlining bug for "out cert"
    result := false;
  end
  else
  begin
    cert := List[i - 1];
    result := true;
  end;
end;

function TCryptCertPerUsage.PerUsage(u: TCryptCertUsage): ICryptCert;
begin
  GetUsage(u, result);
end;

function TCryptCertPerUsage.AsPem: RawUtf8;
var
  i: PtrInt;
begin
  FastAssignNew(result);
  for i := 0 to length(List) - 1 do
    result  := result + List[i].Save(cccCertOnly, '', ccfPem) + (CRLF + CRLF);
end;

function TCryptCertPerUsage.FromPem(
  algo: TCryptCertAlgo; const pem: RawUtf8): TCryptCertUsages;
var
  P: PUtf8Char;
  one: RawUtf8;
  c: ICryptCert;
begin
  Clear;
  result := [];
  if algo = nil then
    exit;
  P := pointer(pem);
  while P <> nil do
  begin
    one := NextPem(P);
    if one = '' then
      break;
    c := algo.Load(one);
    if c <> nil then
      result := result + Add(c);
  end;
end;

function TCryptCertPerUsage.AsBinary: RawByteString;
var
  i: PtrInt;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  s: TBufferWriter;
begin
  s := TBufferWriter.Create(tmp{%H-});
  try
    for i := 0 to length(List) - 1 do
      s.Write(List[i].Save(cccCertOnly, '', ccfBinary));
    result := s.FlushTo;
  finally
    s.Free;
  end;
end;

function TCryptCertPerUsage.FromBinary(algo: TCryptCertAlgo;
  const bin: RawByteString): TCryptCertUsages;
var
  s: TFastReader;
  c: ICryptCert;
begin
  Clear;
  result := [];
  if (algo = nil) or
     (bin = '') then
    exit;
  s.Init(bin);
  while not s.EOF do
  begin
    c := algo.Load(s.VarString);
    if c <> nil then
      result := result + Add(c);
  end;
end;


function ToText(a: TCryptAsymAlgo): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TCryptAsymAlgo), ord(a));
end;

function ToText(a: TCryptKeyAlgo): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TCryptKeyAlgo), ord(a));
end;

function ToText(r: TCryptCertRevocationReason): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertRevocationReason), ord(r));
end;

function ToText(u: TCryptCertUsage): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertUsage), ord(u));
end;

function ToText(u: TCryptCertUsages; from_cu_text: boolean): ShortString;
var
  cu: TCryptCertUsage;
begin
  if from_cu_text then
  begin
    result[0] := #0;
    for cu := low(cu) to high(cu) do
      if cu in u then
        AppendShortTwoCharsSafe(PWord(@CU_TEXT[cu])^, result);
  end
  else
    GetSetNameShort(TypeInfo(TCryptCertUsages), u, result, {trim=}true);
end;

function ToText(v: TCryptCertValidity): PShortString;
begin
  result := GetEnumName(TypeInfo(TCryptCertValidity), ord(v));
end;

function GetFirstUsage(u: TCryptCertUsages): TCryptCertUsage;
begin
  for result := low(result) to high(result) do
    if result in u then
      exit;
  result := cuKeyCertSign;
end;

function HasCertUsage(const one: TCryptCertUsage; const usages: TCryptCertUsages): boolean;
begin // RFC 5280 6.1.4: if KeyUsage is present, the bit must be set
  result := true;
  if one in usages then
    exit;
  if (one in CU_KEY_USAGE) and
     (usages * CU_KEY_USAGE = []) then // no KeyUsage means OK
    exit;
  result := false;
end;

function IsCertAuthority(usages: TCryptCertUsages): boolean;
begin
  result := (cuCA in usages) and                 // cA=TRUE should be present
            HasCertUsage(cuKeyCertSign, usages); // could emit signatures
end;

function IsCN(const Rdn: RawUtf8): boolean;
begin
  result := (length(Rdn) = 2) and
            (PWord(Rdn)^ and $dfdf = ord('C') + ord('N') shl 8);
end;

function IsDer(const Rdn: RawUtf8): boolean;
begin
  result := (length(Rdn) = 3) and
            (PCardinal(Rdn)^ and $dfdfdf =
               ord('D') + ord('E') shl 8 + ord('R') shl 16);
end;

function SaveAsJwk(algo: TCryptAsymAlgo; const x, y: RawByteString): RawUtf8;
var
  bx, by: RawUtf8;
begin
  FastAssignNew(result);
  if (x = '') or
     (y = '') or
     (algo in [caaEdDSA]) then // EDSA not yet supported (single "x" parameter)
    exit;
  bx := BinToBase64uri(x);
  by := BinToBase64uri(y);
  // parameters are ordered lexicographically, as expected for thumbprints
  if algo in CAA_ECC then
    // for ECC, GetKeyParams() returned the x,y coordinates
    FormatUtf8('{"crv":"%","kty":"EC","x":"%","y":"%"}',
                  [CAA_CRV[algo], bx, by], result)
  else
    // for RSA, x was set to the Exponent (e), and y to the Modulus (n)
    FormatUtf8('{"e":"%","kty":"RSA","n":"%"}', [bx, by], result);
end;


{ Register mormot.crypt.core and mormot.crypt.secure Algorithms }

procedure GlobalCryptAlgoInit;
var
  m: TAesMode;
  b, bits: integer;
  n: RawUtf8;
begin
  // don't call TAesPrng.Main to initialize MainAesPrng yet, because
  // OpenSslRandBytes() may not be already set and gathering OS entropy
  // may not be needed at all
  GlobalLock; // RegisterGlobalShutdownRelease() will use it anyway
  try
    if GlobalCryptAlgo <> nil then
      exit;
    GlobalCryptAlgo := RegisterGlobalShutdownRelease(
      TRawUtf8List.CreateEx([fNoDuplicate, fThreadSafe])); // no fObjectsOwned
    // register mormot.crypt.core engines into our factories
    TCryptRandomAesPrng.Implements('rnd-default,rnd-aes');
    TCryptRandomLecuyerPrng.Implements('rnd-lecuyer');
    TCryptRandomDelphi.Implements('rnd-delphi');
    {$ifdef ASMINTEL}
    if cfRAND in CpuFeatures then
      TCryptRandomRdRand.Implements('rnd-rdrand');
    {$endif ASMINTEL}
    TCryptRandomEntropy.Implements(RndAlgosText);
    TCryptRandomSysPrng.Implements('rnd-system,rnd-systemblocking');
    TCryptHasherInternal.Implements(HashAlgosText);
    TCryptCrc32Internal.Implements(CrcAlgosText);
    TCryptSignerInternal.Implements(SignAlgosText);
    for m := low(m) to high(m) do // register all known modes and sizes
      for b := 0 to 2 do
      begin
        bits := 128 + b * 64;
        n := AesAlgoNameEncode(m, bits);
        TCryptAesInternal.Create(n, m, bits, TAesFast); // fastest
        if not (m in AES_INTERNAL) then // if exists as OpenSSL standard
          TCryptAesInternal.Create(n + '-int', m, bits, TAesInternal);
      end;
  finally
   GlobalUnlock;
 end;
end;


{ Actual High-Level Factory Functions }

var // cache the last used algorithm for each factory function
  LastRnd, LastHasher, LastSigner, LastCipher,
  LastAsym, LastCert, LastStore: TCryptAlgo;

function Rnd(const name: RawUtf8): TCryptRandom;
begin
  result := TCryptRandom.InternalFind(name, LastRnd);
end;

function Hasher(const name: RawUtf8): TCryptHasher;
begin
  result := TCryptHasher.InternalFind(name, LastHasher);
end;

function Hash(const name: RawUtf8): ICryptHash;
var
  h: TCryptHasher;
begin
  h := Hasher(name);
  if h = nil then
    result := nil
  else
    result := h.New;
end;

function Signer(const name: RawUtf8): TCryptSigner;
begin
  result := TCryptSigner.InternalFind(name, LastSigner);
end;

function Sign(key: pointer; keylen: PtrInt; const name: RawUtf8): ICryptHash;
var
  s: TCryptSigner;
begin
  s := Signer(name);
  if s = nil then
    result := nil
  else
    result := s.New(key, keylen);
end;

function Sign(const name, secret, salt: RawUtf8; rounds: integer): ICryptHash;
var
  s: TCryptSigner;
begin
  s := Signer(name);
  if s = nil then
    result := nil
  else
    result := s.NewPbkdf2(secret, salt, rounds);
end;

function Sign(const paramsjson, defaultsalt: RawUtf8): ICryptHash;
var
  s: TSynSigner;
  key: THash512Rec;
begin
  s.Pbkdf2(paramsjson, key, defaultsalt, saSha256);
  result := TCryptSignInternal.Create(s, key);
end;

function CipherAlgo(const name: RawUtf8): TCryptCipherAlgo;
begin
  result := TCryptCipherAlgo.InternalFind(name, LastCipher);
end;

function Cipher(const name: RawUtf8; key: pointer; encrypt: boolean): ICryptCipher;
var
  c: TCryptCipherAlgo;
begin
  c := CipherAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New(key, encrypt);
end;

function Encrypt(const name: RawUtf8; key: pointer): ICryptCipher;
begin
  result := Cipher(name, key, {encrypt=}true);
end;

function Decrypt(const name: RawUtf8; key: pointer): ICryptCipher;
begin
  result := Cipher(name, key, {encrypt=}false);
end;

function Cipher(const name, hash, secret, salt: RawUtf8; rounds: integer;
  encrypt: boolean): ICryptCipher;
var
  c: TCryptCipherAlgo;
begin
  c := CipherAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New(hash, secret, salt, rounds, encrypt);
end;

function Encrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher;
begin
  result := Cipher(name, hash, secret, salt, rounds, {encrypt=}true);
end;

function Decrypt(const name, hash, secret, salt: RawUtf8; rounds: integer): ICryptCipher;
begin
  result := Cipher(name, hash, secret, salt, rounds, {encrypt=}false);
end;

function Asym(const name: RawUtf8): TCryptAsym;
begin
  result := TCryptAsym.InternalFind(name, LastAsym);
end;

function CertAlgo(const name: RawUtf8): TCryptCertAlgo;
begin
  result := TCryptCertAlgo.InternalFind(name, LastCert);
end;

function Cert(const name: RawUtf8): ICryptCert;
var
  c: TCryptCertAlgo;
begin
  c := CertAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New;
end;

function StoreAlgo(const name: RawUtf8): TCryptStoreAlgo;
begin
  result := TCryptStoreAlgo.InternalFind(name, LastStore);
end;

function Store(const name: RawUtf8): ICryptStore;
var
  c: TCryptStoreAlgo;
begin
  c := StoreAlgo(name);
  if c = nil then
    result := nil
  else
    result := c.New;
end;


{ ******************************* Key and Certificate Aware PEM/DER Helpers }

function PrivateKeyToPkcs8(Algorithm: TCryptKeyAlgo;
  const Der: RawByteString): RawByteString;
var
  pos, vers: integer;
  oct: RawByteString;
begin
  // PKCS#8 PrivateKeyInfo ::= SEQ { INT version, SEQ algorithm, OCTSTR key }
  // PKCS#1 RSAPrivateKey  ::= SEQ { INT version, INT modulus, ... }
  pos := 1;
  if (Algorithm in CKA_RSA) and
     (AsnNext(pos, Der) = ASN1_SEQ) and
     (AsnNextInt32(pos, Der, vers) = ASN1_INT) and
     (pos <= length(Der)) and
     (ord(Der[pos]) = ASN1_INT) then
  begin
    // wrap TCryptPrivateKeyRsa.ToDer PKCS#1 into a PKCS#8 PrivateKeyInfo
    oct := AsnOctStr(Der);
    result := Asn(ASN1_SEQ, [
                Asn(0), // version
                CkaToSeq(Algorithm),
                oct
              ]);
    FillZero(oct);
  end
  else
    result := Der; // ECC/EdDSA ToDer is already a PKCS#8 PrivateKeyInfo
end;

const
  CAA_SIZE: array[TCryptAsymAlgo] of byte = (
    32,  // caaES256
    48,  // caaES384
    66,  // caaES512
    32,  // caaES256K
    0,   // caaRS256
    0,   // caaRS384
    0,   // caaRS512
    0,   // caaPS256
    0,   // caaPS384
    0,   // caaPS512
    32); // caaEdDSA
function GetSignatureSecurityBits(a: TCryptAsymAlgo; len: integer): integer;
begin
  result := 0;
  len := len shl 3; // into bits
  if len < 128 then
    exit;
  result := CAA_SIZE[a];
  if result <> 0 then
    // ECC security size is half of its X,Y coordinates storage size
    result := result shl 2
  else
    // RSA security depends on the signature size, not the hash size
    if len < 1024 then
      result := 30           // 512-bit
    else if len < 2048 then
      result := 80           // 1024-bit
    else if len < 3072 then
      result := 112          // 2048-bit
    else if len < 7680 then
      result := 128          // 3072-bit
    else if len < 15360 then
      result := 192          // 7680-bit: very unlikely since very slow
    else
      result := 256; // the lower RS256 hash has 256-bit of security anyway
end;

function GetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const signature: RawByteString): RawUtf8;
var
  derlen: cardinal;
  der: PByteArray;
  eccbytes, len: PtrUInt;
  buf: array[0..131] of AnsiChar;
begin
  if algo in CAA_RAWSIGNATURE then
  begin
    // no need to be decoded, since RSA and EdDSA have no SEQ
    result := BinToBase64uri(pointer(signature), length(signature));
    exit;
  end;
  FastAssignNew(result);
  derlen := length(signature);
  der := pointer(signature);
  if (derlen < 50) or
     (der[0] <> ASN1_SEQ) or
     (der[1] > derlen - 2) then
    exit;
  eccbytes := CAA_SIZE[algo];
  if der[1] and $80 <> 0 then
  begin
    if (der[1] and $7f) <> 1 then // 2-byte length (e.g. ES512)
      exit;
    len := der[2]; // length in 2nd byte
    if DerParse(DerParse(@der[3], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 3]) then
      exit;
  end
  else
  begin
    len := der[1]; // 1-byte length
    if DerParse(DerParse(@der[2], @buf[0], eccbytes),
        @buf[eccbytes], eccbytes) <> PAnsiChar(@der[len + 2]) then
      exit;
  end;
  result := BinToBase64uri(@buf[0], eccbytes * 2);
end;

function SetSignatureSecurityRaw(algo: TCryptAsymAlgo;
  const rawsignature: RawUtf8): RawByteString;
var
  eccbytes: PtrInt;
begin
  result := rawsignature;
  if (result = '') or
     (algo in CAA_RAWSIGNATURE) then
    exit; // no need to be encoded, since RSA and EdDSA have no SEQ
  eccbytes := CAA_SIZE[algo];
  if length(result) = eccbytes * 2 then
    result := Asn(ASN1_SEQ, [
      AsnEncInt(@PByteArray(result)[0], eccbytes),
      AsnEncInt(@PByteArray(result)[eccbytes], eccbytes)
      ]);
end;

function OidToCka(const oid, oid2: RawUtf8): TCryptKeyAlgo;
begin
  if oid = CKA_OID[ckaRsa] then
    result := ckaRsa
  else if oid = CKA_OID[ckaRsaPss] then
    result := ckaRsaPss
  else if oid = ASN1_OID_X962_PUBLICKEY then
  begin
    for result := ckaEcc256 to ckaEcc256k do
      if oid2 = CKA_OID[result] then
        exit;
    result := ckaNone;
  end
  else if oid = CKA_OID[ckaEdDSA] then
    result := ckaEdDSA
  else
    result := ckaNone;
end;

function CkaToSeq(cka: TCryptKeyAlgo): RawByteString;
begin
  case cka of
    ckaRsa:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(CKA_OID[ckaRsa])),
                  ASN1_NULL_VALUE // optional
                ]);
    ckaRsaPss,
    ckaEdDSA:
      result := Asn(ASN1_SEQ, [
                  AsnOid(pointer(CKA_OID[cka]))
                ]);
    ckaEcc256 .. ckaEcc256k:
      result := Asn(ASN1_SEQ, [
                  AsnOid(ASN1_OID_X962_PUBLICKEY),
                  AsnOid(pointer(CKA_OID[cka]))
                ]);
  else
    begin
      FastAssignNew(result); // make compiler happy
      ECrypt.RaiseUtf8('Unexpected CkaToSeq(%)', [ToText(cka)^]);
    end;
  end;
end;

function EccPrivKeyToSeq(
  cka: TCryptKeyAlgo; const rawecc: RawByteString): RawByteString;
var
  oct: RawByteString;
begin
  // see PemDerRawToEcc() secp256r1/prime256v1 PKCS#8 PrivateKeyInfo
  oct := AsnSafeOct([Asn(1),
                     Asn(ASN1_OCTSTR, [rawecc])]);
  result := Asn(ASN1_SEQ, [
              Asn(0), // version
              CkaToSeq(cka),
              oct
            ]);
  FillZero(oct);
end;

function SeqToEccPrivKey(cka: TCryptKeyAlgo; const seq: RawByteString;
  rfcpub: PRawByteString): RawByteString;
var
  oid, key: TAsnObject;
  pos, vt, vers: integer;
  oct: TAsnBuffer;
begin
  FastAssignNew(result);
  if rfcpub <> nil then
    rfcpub^ := '';
  // initial sequence decoding
  pos := 1;
  if AsnNext(pos, seq) <> ASN1_SEQ then
    exit;
  vers := AsnNextInteger(pos, seq, vt);
  if vt = ASN1_INT then
    case vers of
      0: // PKCS#8 PrivateKeyInfo
        if (AsnNext(pos, seq) = ASN1_SEQ) and // privateKeyAlgorithm
           (AsnNext(pos, seq, @oid) = ASN1_OBJID) then
          case cka of
            ckaEcc256 .. ckaEcc256k: // RFC 5480 id-ecPublicKey + namedCurve OID
              if (oid = ASN1_OID_X962_PUBLICKEY) and
                 (AsnNext(pos, seq, @oid) = ASN1_OBJID) and
                 (oid = CKA_OID[cka]) and
                 // RFC 5915 ECPrivateKey inside PKCS#8 privateKey OCTET STRING:
                 (AsnNextBuffer(pos, seq, oct) = ASN1_OCTSTR) and
                 (AsnNextBuffer(oct) = ASN1_SEQ) and             // SEQ
                 (AsnNextBuffer(oct) = ASN1_INT) and             // version
                 (AsnNextBuffer(oct, @key) = ASN1_OCTSTR) then   // privateKey
                result := key;
            ckaEdDSA: // RFC 8410 algorithm identifier is directly id-Ed25519
              if (oid = CKA_OID[cka]) and
                 (AsnNextBuffer(pos, seq, oct) = ASN1_OCTSTR) and
                 (AsnNextBuffer(oct, @key) = ASN1_OCTSTR) then
                result := key;
          else
            exit; // this function is dedicated to ECC
          end;
      1: // RFC 5915 EC key pair alternate format
        if (cka in CKA_ECC) and
           (AsnNextRaw(pos, seq, key) = ASN1_OCTSTR) then
        begin
          vt := AsnNext(pos, seq);
          if vt = ASN1_NULL then
            result := key
          else if (vt = ASN1_CTC0) and // [0] ECparameters (optional)
                  (AsnNext(pos, seq, @oid) = ASN1_OBJID) and
                  {%H-}(oid = CKA_OID[cka]) then
          begin
            result := key;
            if (rfcpub <> nil) and     // [1] publicKey (optional)
               (AsnNext(pos, seq) = ASN1_CTC1) and
               (AsnNextRaw(pos, seq, key) = ASN1_BITSTR) then
              rfcpub^ := key;
          end;
        end;
    end;
  FillZero(key);
end;

function SeqToEccPubKey(cka: TCryptKeyAlgo; const seq: RawByteString): RawByteString;
var
  oid: RawByteString;
  pos: integer;
begin
  FastAssignNew(result);
  // PKCS#8 sequence decoding
  pos := 1;
  if (AsnNext(pos, seq) <> ASN1_SEQ) or
     (AsnNext(pos, seq) <> ASN1_SEQ) or // algorithm
     (AsnNext(pos, seq, @oid) <> ASN1_OBJID) then
    exit;
  // CkaToSeq() decoding and validating
  case cka of
    ckaEcc256 .. ckaEcc256k:
      if (oid <> ASN1_OID_X962_PUBLICKEY) or
         (AsnNext(pos, seq, @oid) <> ASN1_OBJID) then
        exit;
    ckaEdDSA:
      ;
  else
    exit; // this function is dedicated to ECC: use X509PubKeyFromDer() instead
  end;
  if oid = CKA_OID[cka] then
    // public key raw binary extraction
    if AsnNextRaw(pos, seq, result) <> ASN1_BITSTR then
      FastAssignNew(result);
end;

function X509PubKeyToDer(Algorithm: TCryptKeyAlgo;
  const SubjectPublicKey: RawByteString): RawByteString;
begin
  result := Asn(ASN1_SEQ, [
              CkaToSeq(Algorithm),
              Asn(ASN1_BITSTR, [
                SubjectPublicKey
              ])
            ]);
end;

function ParsedToText(const c: TX509Parsed): RawUtf8;

  procedure KeyUsage(l, h: TCryptCertUsage; const ext: RawUtf8);
  var
    cu: TCryptCertUsage;
    usage: RawUtf8;
  begin
    for cu := l to h do
      if cu in c.Usage then
        AddToCsv(CU_FULLTEXT[cu], usage{%H-}, ', ');
    if usage <> '' then
      result := Join([result, '    X509v3 ', ext, #13#10 +
                              '      ', usage, #13#10]);
  end;

var
  bits: RawUtf8;
  version: integer;
begin
  // somewhat follows X509_print() OpenSSL formatting
  if (c.Usage <> []) or
     (c.SubjectID <> '') or
     (c.IssuerID <> '') then
    version := 2   // X.509 v3
  else
    version := 1;
  X509PubKeyBits(c.PubKey, @bits);
  Join(['Certificate:'#13#10 +
        '  Version: ', SmallUInt32Utf8[version + 1],
        ' (0x', SmallUInt32Utf8[version] ,')'#13#10 +
        '  Serial Number:'#13#10 +
        '    ', c.Serial, #13#10 +
        '  Signature Algorithm: ', c.SigAlg, #13#10 +
        '  Issuer: ', c.IssuerDN, #13#10 +
        '  Validity'#13#10 +
        '    Not Before: ', DateTimeToHttpDate(c.NotBefore), #13#10 +
        '    Not After : ', DateTimeToHttpDate(c.NotAfter), #13#10 +
        '  Subject: ', c.SubjectDN, #13#10 +
        '  Subject Public Key Info:'#13#10 +
        '    Public Key Algorithm: ', c.PubAlg, #13#10,
        bits], result);
  if version = 1 then
    exit;
  // append the X.509 v3 known extensions
  Append(result, '  X509v3 extensions:'#13#10);
  KeyUsage(cuCrlSign,   cuDigitalSignature, 'Key Usage: critical');
  KeyUsage(cuTlsServer, cuTimestamp,        'Extended Key Usage:');
  if cuCA in c.Usage then
    bits := 'TRUE'
  else
    bits := 'FALSE';
  result := Join([result, '    X509v3 Basic Constraints: critical'#13#10 +
                          '      CA:', bits, #13#10]);
  if c.SubjectID <> '' then
    result := Join([result, '    X509v3 Subject Key Identifier:'#13#10 +
                            '      ', c.SubjectID, #13#10]);
  if c.IssuerID <> '' then
    result := Join([result, '    X509v3 Authority Key Identifier:'#13#10 +
                            '      ', c.IssuerID, #13#10]);
  if c.SubjectAltNames <> '' then
    result := Join([result, '    X509v3 Subject Alternative Name:'#13#10 +
                            '      ', c.SubjectAltNames, #13#10]);
end;

{$ifdef OSWINDOWS}

procedure WinInfoToParse(const c: TWinCertInfo; out Info: TX509Parsed);
begin
  Info.Serial          := c.Serial;
  Info.SubjectDN       := c.SubjectName;
  Info.IssuerDN        := c.IssuerName;
  Info.SubjectAltNames := c.SubjectAltNames;
  Info.SubjectID       := c.SubjectID;
  Info.IssuerID        := c.IssuerID;
  Info.SigAlg          := c.AlgorithmName;
  Info.PubAlg          := c.PublicKeyAlgorithmName;
  Info.Usage           := TCryptCertUsages(c.Usage); // match TWinCertUsages
  Info.NotBefore       := c.NotBefore;
  Info.NotAfter        := c.NotAfter;
  Info.PubKey          := c.PublicKeyContent;
  Info.PeerInfo        := ParsedToText(Info); // should be the last
end;

function WinX509Parse(const Cert: RawByteString; out Info: TX509Parsed): boolean;
var
  c: TWinCertInfo;
begin
  result := WinCertDecode(PemToDer(Cert), c);
  if result then
    WinInfoToParse(c, Info);
end;

function Win2Text(const c: TWinCertInfo): RawUtf8;
var
  nfo: TX509Parsed;
begin
  WinInfoToParse(c, nfo);
  result := nfo.PeerInfo;
end;

{$endif OSWINDOWS}


procedure InitializeUnit;
begin
  // Rnd/Sign/Hash/Cipher/Asym/Cert/Store are registered in GlobalCryptAlgoInit
  {$ifdef OSWINDOWS}
  X509Parse         := @WinX509Parse; // use mormot.lib.sspi.pas WinCertDecode()
  WinCertInfoToText := @Win2Text;
  if IsAiaDisabledInWindowsRegistry then
    exclude(DefaultCryptStoreTrust, cstAiaIntermediate);
  {$endif OSWINDOWS}
end;


initialization
  InitializeUnit;

end.
