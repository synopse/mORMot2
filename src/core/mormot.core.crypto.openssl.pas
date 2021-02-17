/// Framework Core Cryptographic Process using OpenSSL
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.crypto.openssl;

{
  *****************************************************************************

   High-Performance Cryptographic Features using OpenSSL 1.1.1
    - OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG)
    - AES Cypher/Uncypher in various Modes
    - Hashers and Signers OpenSSL Wrappers
    - Register OpenSSL to our General Cryptography Catalog

  *****************************************************************************

  TL;DR: on x86_64, our mormot.core.crypto.pas asm is stand-alone and faster
  than OpenSSL for most algorithms, and only 20% slower for AES-GCM.
  For asymetric cryptography, OpenSSL is much faster than mormot.core.ecc256r1.

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
  mormot.core.crypto,
  mormot.core.ecc256r1,
  mormot.lib.openssl11;


{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

type
  /// exception class raised by the AES classes of this unit
  EOpenSslCrypto = class(EOpenSsl);

  /// TAesPrng-compatible class using OpenSSL 1.1.1
  // - we abbreviate OpenSsl as Osl for class names for brevity
  // - may be used instead of TAesPrng if a "proven" generator is required -
  // you could override MainAesPrng global variable
  // - but mormot.core.crypto TAesPrng is faster, especially for small output,
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
    procedure SetEvp(DoEncrypt: boolean; const method: string);
    procedure UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
    procedure Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
  end;

  /// handle AES cypher/uncypher with chaining with OpenSSL 1.1
  // - we abbreviate OpenSsl as Osl for class names for brevity
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
  TAesCtrNistOsl = class(TAesAbstractOsl)
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
    /// AES-GCM pure alternative to MacSetNonce()
    // - set the IV as usual (only the first 12 bytes will be used for GCM),
    // then optionally append any AEAD data with this method; warning: you need
    // to call Encrypt() once before - perhaps as Encrypt(nil, nil, 0)
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

/// asymetric digital signature of some Message using a given PrivateKey
// - if Algorithm is not specified, EVP_sha256 will be used
// - returns 0 on error, or the result Signature size in bytes
function OpenSslSign(const Algorithm: RawUtf8;
  Message, PrivateKey: pointer; MessageLen, PrivateKeyLen: integer;
  const PrivateKeyPassword: RawUtf8; out Signature: THash512Rec): cardinal;


/// asymetric digital verification of some Message using a given PublicKey
// - if Algorithm is not specified, EVP_sha256 will be used
// - returns 0 on error, or the result Signature size in bytes
function OpenSslVerify(const Algorithm, PublicKeyPassword: RawUtf8;
  Message, PublicKey, Signature: pointer;
  MessageLen, PublicKeyLen, SignatureLen: integer): boolean;

/// mormot.core.ecc256r1 compatible function for asymetric digital verification
function ecdsa_verify_osl(const PublicKey: TEccPublicKey; const Hash: TEccHash;
  const Signature: TEccSignature): boolean;


{ ************** Register OpenSSL to our General Cryptography Catalog }

/// call once at program startup to use OpenSSL when its performance matters
// - redirects TAesGcmFast (and TAesCtrFast on i386) globals to OpenSSL
// - redirects raw mormot.core.ecc256r1 functions to use OpenSSL which is much
// faster even than the easy-gcc C version
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

procedure TAesOsl.SetEvp(DoEncrypt: boolean; const method: string);
var
  c: PEVP_CIPHER_CTX;
begin
  c := Ctx[DoEncrypt];
  if c = nil then
  begin
    // setup encrypt/decrypt context, with the proper key and no padding
    c := EVP_CIPHER_CTX_new;
    EOpenSslCrypto.Check(Owner, method,
      EVP_CipherInit_ex(c, Cipher, nil,
        @TAesAbstractOsl(Owner).fKey, nil, ord(DoEncrypt)));
    EOpenSslCrypto.Check(Owner, method,
      EVP_CIPHER_CTX_set_padding(c, 0));
    Ctx[DoEncrypt] := c;
  end;
  // OpenSSL allows to reuse the previous Ctxt[], just setting the (new) IV
  EOpenSslCrypto.Check(Owner, method,
    EVP_CipherInit_ex(c, nil, nil, nil, @Owner.IV, ord(DoEncrypt)));
end;

procedure TAesOsl.UpdEvp(DoEncrypt: boolean; BufIn, BufOut: pointer; Count: cardinal);
var
  outl: integer;
begin
  if (BufOut <> nil) and
     (Count and AesBlockMod <> 0) then
    raise ESynCrypto.CreateUtf8('%.%: Count=% is not a multiple of 16',
      [Owner, 'UpdEvp', Count]);
  EOpenSslCrypto.Check(Owner, 'UpdEvp',
    EVP_CipherUpdate(Ctx[DoEncrypt], BufOut, @outl, BufIn, Count));
  // no need to call EVP_CipherFinal_ex() since we expect no padding
end;

procedure TAesOsl.Clone(ToOwner: TAesAbstract; out ToAesOsl: TAesOsl);
var
  enc: boolean;
begin
  TAesAbstractOsl(ToOwner).fKeySize := TAesAbstractOsl(Owner).fKeySize;
  TAesAbstractOsl(ToOwner).fKeySizeBytes := TAesAbstractOsl(Owner).fKeySizeBytes;
  TAesAbstractOsl(ToOwner).fKey := TAesAbstractOsl(Owner).fKey;
  TAesAbstractOsl(ToOwner).fAlgoMode := TAesAbstractOsl(Owner).fAlgoMode;
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
  if result = nil then
  begin
    EOpenSslCrypto.CheckAvailable(self, 'Main');
    GlobalLock;
    try
      if MainAesPrngOsl = nil then
        MainAesPrngOsl := TAesPrngOsl.Create;
    finally
      GlobalUnLock;
    end;
    result := MainAesPrngOsl;
  end;
end;


{ ************** AES Cypher/Uncypher in various Modes }

{ TAesAbstractOsl }

procedure TAesAbstractOsl.AfterCreate;
var
  nam: TShort16;
begin
  AlgoName(nam); // always #0 terminated
  fAes.Init(self, pointer(@nam[1]));
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

{ TAesCtrNistOsl }

procedure TAesCtrNistOsl.AfterCreate;
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
  BinToHexLower(@fDigestValue, Digest(nil), result);
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
      raise EOpenSslHash.CreateFmt('TOpenSslHash.Create: Incorrect hashSize ' +
        'option passed to a non-XOF hash function "%s"', [Algorithm]);
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


function OpenSslSign(const Algorithm: RawUtf8;
  Message, PrivateKey: pointer; MessageLen, PrivateKeyLen: integer;
  const PrivateKeyPassword: RawUtf8; out Signature: THash512Rec): cardinal;
var
  md: PEVP_MD;
  priv: PBIO;
  pkey: PEVP_PKEY;
  ctx: PEVP_MD_CTX;
  size: PtrUInt;
begin
  result := 0;
  if not OpenSslIsAvailable then
    exit;
  if Algorithm = '' then
    md := EVP_sha256
  else
    md := EVP_get_digestbyname(pointer(Algorithm));
  if md = nil then
    exit;
  if (PrivateKey = nil) or
     (PrivateKeyLen = 0) then begin
    priv := nil;
    pkey := nil;
  end
  else
  begin
    priv := BIO_new_mem_buf(PrivateKey, PrivateKeyLen);
    pkey := PEM_read_bio_PrivateKey(priv, nil, nil, pointer(PrivateKeyPassword));
  end;
  ctx := EVP_MD_CTX_new;
  try
    if (EVP_DigestSignInit(ctx, nil, md, nil, pkey) = OPENSSLSUCCESS) and
       (EVP_DigestUpdate(ctx, Message, MessageLen) = OPENSSLSUCCESS) and
       (EVP_DigestSignFinal(ctx, nil, size) = OPENSSLSUCCESS) and
       (size = SizeOf(Signature)) and
       (EVP_DigestSignFinal(ctx, @Signature, size) = OPENSSLSUCCESS) then
      result := size; // success
  finally
    EVP_MD_CTX_free(ctx);
    if pkey <> nil then
      EVP_PKEY_free(pkey);
    if priv <> nil then
      BIO_free(priv);
  end;
end;

function OpenSslVerify(const Algorithm, PublicKeyPassword: RawUtf8;
  Message, PublicKey, Signature: pointer;
  MessageLen, PublicKeyLen, SignatureLen: integer): boolean;
var
  md: PEVP_MD;
  pub: PBIO;
  pkey: PEVP_PKEY;
  ctx: PEVP_MD_CTX;
begin
  result := false;
  if not OpenSslIsAvailable then
    exit;
  if Algorithm = '' then
    md := EVP_sha256
  else
    md := EVP_get_digestbyname(pointer(Algorithm));
  if (md = nil) or
     (PublicKey = nil) or
     (PublicKeyLen <= 0) or
     (SignatureLen <= 0)  then
    exit;
  pub := BIO_new_mem_buf(PublicKey, PublicKeyLen);
  pkey := PEM_read_bio_PUBKEY(pub, nil, nil, pointer(PublicKeyPassword));
  ctx := EVP_MD_CTX_new;
  try
    if (EVP_DigestVerifyInit(ctx, nil, md, nil, pkey) = OPENSSLSUCCESS) and
       (EVP_DigestUpdate(ctx, Message, MessageLen) = OPENSSLSUCCESS) and
       (EVP_DigestVerifyFinal(ctx, Signature, SignatureLen) = OPENSSLSUCCESS) then
      result := true;
  finally
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);
    BIO_free(pub);
  end;
end;

//writeln(SSL_error_short(ERR_get_error));

function ecdsa_verify_osl(const PublicKey: TEccPublicKey; const Hash: TEccHash;
  const Signature: TEccSignature): boolean;
var
  grp: PEC_GROUP;
  key: PEC_KEY;
  bn: PBIGNUM;
  pt: PEC_POINT;
  res, derlen: integer;
  der: TEccSignatureDer;
begin
  result := false;
  if not OpenSslIsAvailable then
    exit;
  grp := EC_GROUP_new_by_curve_name(NID_X9_62_prime256v1);
  if grp = nil then
    exit;
  key := EC_KEY_new;
  if EC_KEY_set_group(key, grp) = OPENSSLSUCCESS then
  begin
    bn := BN_bin2bn(@PublicKey, SizeOf(PublicKey), nil);
    pt := EC_POINT_bn2point(grp, bn, nil, nil);
    if (pt <> nil) and
       (EC_KEY_set_public_key(key, pt) = OPENSSLSUCCESS) then
    begin
      derlen := EccSignToDer(Signature, der);
      res := ECDSA_verify(0, @Hash, SizeOf(Hash), @der, derlen, key);
      result := res = OPENSSLSUCCESS;
    end;
    EC_POINT_free(pt);
    BN_free(bn);
  end;
  EC_KEY_free(key);
  EC_GROUP_free(grp);
end;


{ ************** Register OpenSSL to our General Cryptography Catalog }

procedure RegisterOpenSsl;
begin
  if not OpenSslIsAvailable then
    exit;
  // set the fastest AES implementation classes
  TAesFast[mGcm] := TAesGcmOsl;
  {$ifdef HASAESNI}
    // all mormot.core.crypto x86_64 asm is faster than OpenSSL - but GCM
    {$ifndef CPUX64}
    // our AES-CTR x86_64 asm is faster than OpenSSL's
    TAesFast[mCtr] := TAesCtrNistOsl;
    {$endif CPUX64}
  {$else}
  // ARM/Aarch64 would rather use OpenSSL than our purepascal code
  TAesFast[mEcb] := TAesEcbOsl;
  TAesFast[mCbc] := TAesCbcOsl;
  TAesFast[mCfb] := TAesCfbOsl;
  TAesFast[mOfb] := TAesOfbOsl;
  TAesFast[mCtr] := TAesCtrNistOsl;
  {$endif HASAESNI}
  // redirects raw mormot.core.ecc256r1 functions to use the much faster OpenSSL
  @Ecc256r1Verify := @ecdsa_verify_osl;
end;




initialization

finalization
  FreeAndNil(MainAesPrngOsl);

{$else}

implementation

{$endif USE_OPENSSL}

end.
